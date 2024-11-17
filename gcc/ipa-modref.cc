/* Search for references that a functions loads or stores.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
   accesses across function calls.

   This file contains a tree pass and an IPA pass.  Both performs the same
   analysis however tree pass is executed during early and late optimization
   passes to propagate info downwards in the compilation order.  IPA pass
   propagates across the callgraph and is able to handle recursion and works on
   whole program during link-time analysis.

   LTO mode differs from the local mode by not recording alias sets but types
   that are translated to alias sets later.  This is necessary in order stream
   the information because the alias sets are rebuild at stream-in time and may
   not correspond to ones seen during analysis.  For this reason part of
   analysis is duplicated.

   The following information is computed
     1) load/store access tree described in ipa-modref-tree.h
	This is used by tree-ssa-alias to disambiguate load/stores
     2) EAF flags used by points-to analysis (in tree-ssa-structalias).
	and defined in tree-core.h.
   and stored to optimization_summaries.

   There are multiple summaries computed and used during the propagation:
     - summaries holds summaries from analysis to IPA propagation
       time.
     - summaries_lto is same as summaries but holds them in a format
       that can be streamed (as described above).
     - fnspec_summary holds fnspec strings for call.  This is
       necessary because gimple_call_fnspec performs additional
       analysis except for looking callee fndecl.
     - escape_summary holds escape points for given call edge.
       That is a vector recording what function parameters
       may escape to a function call (and with what parameter index).  */

#define INCLUDE_MEMORY
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
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "attr-fnspec.h"
#include "symtab-clones.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-ssa-operands.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "attribs.h"
#include "tree-cfg.h"
#include "tree-eh.h"


namespace {

/* We record fnspec specifiers for call edges since they depends on actual
   gimple statements.  */

class fnspec_summary
{
public:
  char *fnspec;

  fnspec_summary ()
  : fnspec (NULL)
  {
  }

  ~fnspec_summary ()
  {
    free (fnspec);
  }
};

/* Summary holding fnspec string for a given call.  */

class fnspec_summaries_t : public call_summary <fnspec_summary *>
{
public:
  fnspec_summaries_t (symbol_table *symtab)
      : call_summary <fnspec_summary *> (symtab) {}
  /* Hook that is called by summary when an edge is duplicated.  */
  void duplicate (cgraph_edge *,
		  cgraph_edge *,
		  fnspec_summary *src,
		  fnspec_summary *dst) final override
  {
    dst->fnspec = xstrdup (src->fnspec);
  }
};

static fnspec_summaries_t *fnspec_summaries = NULL;

/* Escape summary holds a vector of param indexes that escape to
   a given call.  */
struct escape_entry
{
  /* Parameter that escapes at a given call.  */
  int parm_index;
  /* Argument it escapes to.  */
  unsigned int arg;
  /* Minimal flags known about the argument.  */
  eaf_flags_t min_flags;
  /* Does it escape directly or indirectly?  */
  bool direct;
};

/* Dump EAF flags.  */

static void
dump_eaf_flags (FILE *out, int flags, bool newline = true)
{
  if (flags & EAF_UNUSED)
    fprintf (out, " unused");
  if (flags & EAF_NO_DIRECT_CLOBBER)
    fprintf (out, " no_direct_clobber");
  if (flags & EAF_NO_INDIRECT_CLOBBER)
    fprintf (out, " no_indirect_clobber");
  if (flags & EAF_NO_DIRECT_ESCAPE)
    fprintf (out, " no_direct_escape");
  if (flags & EAF_NO_INDIRECT_ESCAPE)
    fprintf (out, " no_indirect_escape");
  if (flags & EAF_NOT_RETURNED_DIRECTLY)
    fprintf (out, " not_returned_directly");
  if (flags & EAF_NOT_RETURNED_INDIRECTLY)
    fprintf (out, " not_returned_indirectly");
  if (flags & EAF_NO_DIRECT_READ)
    fprintf (out, " no_direct_read");
  if (flags & EAF_NO_INDIRECT_READ)
    fprintf (out, " no_indirect_read");
  if (newline)
  fprintf (out, "\n");
}

struct escape_summary
{
  auto_vec <escape_entry> esc;
  void dump (FILE *out)
  {
    for (unsigned int i = 0; i < esc.length (); i++)
      {
	fprintf (out, "   parm %i arg %i %s min:",
		 esc[i].parm_index,
		 esc[i].arg,
		 esc[i].direct ? "(direct)" : "(indirect)");
	dump_eaf_flags (out, esc[i].min_flags, false);
      }
    fprintf (out, "\n");
  }
};

class escape_summaries_t : public call_summary <escape_summary *>
{
public:
  escape_summaries_t (symbol_table *symtab)
      : call_summary <escape_summary *> (symtab) {}
  /* Hook that is called by summary when an edge is duplicated.  */
  void duplicate (cgraph_edge *,
		  cgraph_edge *,
		  escape_summary *src,
		  escape_summary *dst) final override
  {
    dst->esc = src->esc.copy ();
  }
};

static escape_summaries_t *escape_summaries = NULL;

}  /* ANON namespace: GTY annotated summaries can not be anonymous.  */


/* Class (from which there is one global instance) that holds modref summaries
   for all analyzed functions.  */

class GTY((user)) modref_summaries
  : public fast_function_summary <modref_summary *, va_gc>
{
public:
  modref_summaries (symbol_table *symtab)
      : fast_function_summary <modref_summary *, va_gc> (symtab) {}
  void insert (cgraph_node *, modref_summary *state) final override;
  void duplicate (cgraph_node *src_node,
		  cgraph_node *dst_node,
		  modref_summary *src_data,
		  modref_summary *dst_data) final override;
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
  void insert (cgraph_node *, modref_summary_lto *state) final override;
  void duplicate (cgraph_node *src_node,
		  cgraph_node *dst_node,
		  modref_summary_lto *src_data,
		  modref_summary_lto *dst_data) final override;
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

/* Global variable holding all modref optimization summaries
   (from IPA propagation time or used by local optimization pass).  */

static GTY(()) fast_function_summary <modref_summary *, va_gc>
	 *optimization_summaries;

/* LTO summaries hold info from analysis to LTO streaming or from LTO
   stream-in through propagation to LTO stream-out.  */

static GTY(()) fast_function_summary <modref_summary_lto *, va_gc>
	 *summaries_lto;

/* Summary for a single function which this pass produces.  */

modref_summary::modref_summary ()
  : loads (NULL), stores (NULL), retslot_flags (0), static_chain_flags (0),
    writes_errno (false), side_effects (false), nondeterministic (false),
    calls_interposable (false), global_memory_read (false),
    global_memory_written (false), try_dse (false)
{
}

modref_summary::~modref_summary ()
{
  if (loads)
    ggc_delete (loads);
  if (stores)
    ggc_delete (stores);
}

/* Remove all flags from EAF_FLAGS that are implied by ECF_FLAGS and not
   useful to track.  If returns_void is true moreover clear
   EAF_NOT_RETURNED.  */
static int
remove_useless_eaf_flags (int eaf_flags, int ecf_flags, bool returns_void)
{
  if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
    eaf_flags &= ~implicit_const_eaf_flags;
  else if (ecf_flags & ECF_PURE)
    eaf_flags &= ~implicit_pure_eaf_flags;
  else if ((ecf_flags & ECF_NORETURN) || returns_void)
    eaf_flags &= ~(EAF_NOT_RETURNED_DIRECTLY | EAF_NOT_RETURNED_INDIRECTLY);
  return eaf_flags;
}

/* Return true if FLAGS holds some useful information.  */

static bool
eaf_flags_useful_p (vec <eaf_flags_t> &flags, int ecf_flags)
{
  for (unsigned i = 0; i < flags.length (); i++)
    if (remove_useless_eaf_flags (flags[i], ecf_flags, false))
      return true;
  return false;
}

/* Return true if summary is potentially useful for optimization.
   If CHECK_FLAGS is false assume that arg_flags are useful.  */

bool
modref_summary::useful_p (int ecf_flags, bool check_flags)
{
  if (arg_flags.length () && !check_flags)
    return true;
  if (check_flags && eaf_flags_useful_p (arg_flags, ecf_flags))
    return true;
  arg_flags.release ();
  if (check_flags && remove_useless_eaf_flags (retslot_flags, ecf_flags, false))
    return true;
  if (check_flags
      && remove_useless_eaf_flags (static_chain_flags, ecf_flags, false))
    return true;
  if (ecf_flags & ECF_CONST)
    return (!side_effects && (ecf_flags & ECF_LOOPING_CONST_OR_PURE));
  if (loads && !loads->every_base)
    return true;
  else
    kills.release ();
  if (ecf_flags & ECF_PURE)
    return (!side_effects && (ecf_flags & ECF_LOOPING_CONST_OR_PURE));
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
  auto_vec<modref_access_node> GTY((skip)) kills;
  auto_vec<eaf_flags_t> GTY((skip)) arg_flags;
  eaf_flags_t retslot_flags;
  eaf_flags_t static_chain_flags;
  unsigned writes_errno : 1;
  unsigned side_effects : 1;
  unsigned nondeterministic : 1;
  unsigned calls_interposable : 1;

  modref_summary_lto ();
  ~modref_summary_lto ();
  void dump (FILE *);
  bool useful_p (int ecf_flags, bool check_flags = true);
};

/* Summary for a single function which this pass produces.  */

modref_summary_lto::modref_summary_lto ()
  : loads (NULL), stores (NULL), retslot_flags (0), static_chain_flags (0),
    writes_errno (false), side_effects (false), nondeterministic (false),
    calls_interposable (false)
{
}

modref_summary_lto::~modref_summary_lto ()
{
  if (loads)
    ggc_delete (loads);
  if (stores)
    ggc_delete (stores);
}


/* Return true if lto summary is potentially useful for optimization.
   If CHECK_FLAGS is false assume that arg_flags are useful.  */

bool
modref_summary_lto::useful_p (int ecf_flags, bool check_flags)
{
  if (arg_flags.length () && !check_flags)
    return true;
  if (check_flags && eaf_flags_useful_p (arg_flags, ecf_flags))
    return true;
  arg_flags.release ();
  if (check_flags && remove_useless_eaf_flags (retslot_flags, ecf_flags, false))
    return true;
  if (check_flags
      && remove_useless_eaf_flags (static_chain_flags, ecf_flags, false))
    return true;
  if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
    return (!side_effects && (ecf_flags & ECF_LOOPING_CONST_OR_PURE));
  if (loads && !loads->every_base)
    return true;
  else
    kills.release ();
  if (ecf_flags & ECF_PURE)
    return (!side_effects && (ecf_flags & ECF_LOOPING_CONST_OR_PURE));
  return stores && !stores->every_base;
}

/* Dump records TT to OUT.  */

static void
dump_records (modref_records *tt, FILE *out)
{
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
	    {
	      fprintf (out, "          access:");
	      a->dump (out);
	    }
	}
    }
}

/* Dump records TT to OUT.  */

static void
dump_lto_records (modref_records_lto *tt, FILE *out)
{
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
      print_generic_expr (out, n->base);
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
	  print_generic_expr (out, r->ref);
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
	    {
	      fprintf (out, "          access:");
	      a->dump (out);
	    }
	}
    }
}

/* Dump all escape points of NODE to OUT.  */

static void
dump_modref_edge_summaries (FILE *out, cgraph_node *node, int depth)
{
  int i = 0;
  if (!escape_summaries)
    return;
  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
    {
      class escape_summary *sum = escape_summaries->get (e);
      if (sum)
	{
	  fprintf (out, "%*sIndirect call %i in %s escapes:",
		   depth, "", i, node->dump_name ());
	  sum->dump (out);
	}
      i++;
    }
  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	dump_modref_edge_summaries (out, e->callee, depth + 1);
      class escape_summary *sum = escape_summaries->get (e);
      if (sum)
	{
	  fprintf (out, "%*sCall %s->%s escapes:", depth, "",
		   node->dump_name (), e->callee->dump_name ());
	  sum->dump (out);
	}
      class fnspec_summary *fsum = fnspec_summaries->get (e);
      if (fsum)
	{
	  fprintf (out, "%*sCall %s->%s fnspec: %s\n", depth, "",
		   node->dump_name (), e->callee->dump_name (),
		   fsum->fnspec);
	}
    }
}

/* Remove all call edge summaries associated with NODE.  */

static void
remove_modref_edge_summaries (cgraph_node *node)
{
  if (!escape_summaries)
    return;
  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
    escape_summaries->remove (e);
  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	remove_modref_edge_summaries (e->callee);
      escape_summaries->remove (e);
      fnspec_summaries->remove (e);
    }
}

/* Dump summary.  */

void
modref_summary::dump (FILE *out) const
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
  if (kills.length ())
    {
      fprintf (out, "  kills:\n");
      for (auto kill : kills)
	{
	  fprintf (out, "    ");
	  kill.dump (out);
	}
    }
  if (writes_errno)
    fprintf (out, "  Writes errno\n");
  if (side_effects)
    fprintf (out, "  Side effects\n");
  if (nondeterministic)
    fprintf (out, "  Nondeterministic\n");
  if (calls_interposable)
    fprintf (out, "  Calls interposable\n");
  if (global_memory_read)
    fprintf (out, "  Global memory read\n");
  if (global_memory_written)
    fprintf (out, "  Global memory written\n");
  if (try_dse)
    fprintf (out, "  Try dse\n");
  if (arg_flags.length ())
    {
      for (unsigned int i = 0; i < arg_flags.length (); i++)
	if (arg_flags[i])
	  {
	    fprintf (out, "  parm %i flags:", i);
	    dump_eaf_flags (out, arg_flags[i]);
	  }
    }
  if (retslot_flags)
    {
      fprintf (out, "  Retslot flags:");
      dump_eaf_flags (out, retslot_flags);
    }
  if (static_chain_flags)
    {
      fprintf (out, "  Static chain flags:");
      dump_eaf_flags (out, static_chain_flags);
    }
}

/* Dump summary.  */

void
modref_summary_lto::dump (FILE *out)
{
  fprintf (out, "  loads:\n");
  dump_lto_records (loads, out);
  fprintf (out, "  stores:\n");
  dump_lto_records (stores, out);
  if (kills.length ())
    {
      fprintf (out, "  kills:\n");
      for (auto kill : kills)
	{
	  fprintf (out, "    ");
	  kill.dump (out);
	}
    }
  if (writes_errno)
    fprintf (out, "  Writes errno\n");
  if (side_effects)
    fprintf (out, "  Side effects\n");
  if (nondeterministic)
    fprintf (out, "  Nondeterministic\n");
  if (calls_interposable)
    fprintf (out, "  Calls interposable\n");
  if (arg_flags.length ())
    {
      for (unsigned int i = 0; i < arg_flags.length (); i++)
	if (arg_flags[i])
	  {
	    fprintf (out, "  parm %i flags:", i);
	    dump_eaf_flags (out, arg_flags[i]);
	  }
    }
  if (retslot_flags)
    {
      fprintf (out, "  Retslot flags:");
      dump_eaf_flags (out, retslot_flags);
    }
  if (static_chain_flags)
    {
      fprintf (out, "  Static chain flags:");
      dump_eaf_flags (out, static_chain_flags);
    }
}

/* Called after summary is produced and before it is used by local analysis.
   Can be called multiple times in case summary needs to update signature.
   FUN is decl of function summary is attached to.  */
void
modref_summary::finalize (tree fun)
{
  global_memory_read = !loads || loads->global_access_p ();
  global_memory_written = !stores || stores->global_access_p ();

  /* We can do DSE if we know function has no side effects and
     we can analyze all stores.  Disable dse if there are too many
     stores to try.  */
  if (side_effects || global_memory_written || writes_errno)
    try_dse = false;
  else
    {
      try_dse = true;
      size_t i, j, k;
      int num_tests = 0, max_tests
	= opt_for_fn (fun, param_modref_max_tests);
      modref_base_node <alias_set_type> *base_node;
      modref_ref_node <alias_set_type> *ref_node;
      modref_access_node *access_node;
      FOR_EACH_VEC_SAFE_ELT (stores->bases, i, base_node)
	{
	  if (base_node->every_ref)
	    {
	      try_dse = false;
	      break;
	    }
	  FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	    {
	      if (base_node->every_ref)
		{
		  try_dse = false;
		  break;
		}
	      FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
		if (num_tests++ > max_tests
		    || !access_node->parm_offset_known)
		  {
		    try_dse = false;
		    break;
		  }
	      if (!try_dse)
		break;
	    }
	  if (!try_dse)
	    break;
	}
    }
  if (loads->every_base)
    load_accesses = 1;
  else
    {
      load_accesses = 0;
      for (auto base_node : loads->bases)
	{
	  if (base_node->every_ref)
	    load_accesses++;
	  else
	    for (auto ref_node : base_node->refs)
	      if (ref_node->every_access)
		load_accesses++;
	      else
		load_accesses += ref_node->accesses->length ();
	}
    }
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
  func = func->ultimate_alias_target
		 (&avail, current_function_decl ?
			  cgraph_node::get (current_function_decl) : NULL);
  if (avail <= AVAIL_INTERPOSABLE)
    return NULL;

  modref_summary *r = optimization_summaries->get (func);
  return r;
}

/* Get function summary for CALL if it exists, return NULL otherwise.
   If non-null set interposed to indicate whether function may not
   bind to current def.  In this case sometimes loads from function
   needs to be ignored.  */

modref_summary *
get_modref_function_summary (gcall *call, bool *interposed)
{
  tree callee = gimple_call_fndecl (call);
  if (!callee)
    return NULL;
  struct cgraph_node *node = cgraph_node::get (callee);
  if (!node)
    return NULL;
  modref_summary *r = get_modref_function_summary (node);
  if (interposed && r)
    *interposed = r->calls_interposable
		  || !node->binds_to_current_def_p ();
  return r;
}


namespace {

/* Return true if ECF flags says that nondeterminism can be ignored.  */

static bool
ignore_nondeterminism_p (tree caller, int flags, tree callee_fntype)
{
  int caller_flags = flags_from_decl_or_type (caller);
  if ((flags | caller_flags) & (ECF_CONST | ECF_PURE))
    return true;
  if ((flags & (ECF_NORETURN | ECF_NOTHROW)) == (ECF_NORETURN | ECF_NOTHROW)
      || (!opt_for_fn (caller, flag_exceptions) && (flags & ECF_NORETURN)))
    return true;
  /* C language defines unsequenced and reproducible functions
     to be deterministic.  */
  if (lookup_attribute ("unsequenced", TYPE_ATTRIBUTES (TREE_TYPE (caller)))
      || lookup_attribute ("reproducible",
			   TYPE_ATTRIBUTES (TREE_TYPE (caller))))
    return true;
  if (callee_fntype
      && (lookup_attribute ("unsequenced", TYPE_ATTRIBUTES (callee_fntype))
	  || lookup_attribute ("reproducible",
			       TYPE_ATTRIBUTES (callee_fntype))))
    return true;
  return false;
}

/* Return true if ECF flags says that return value can be ignored.  */

static bool
ignore_retval_p (tree caller, int flags)
{
  if ((flags & (ECF_NORETURN | ECF_NOTHROW)) == (ECF_NORETURN | ECF_NOTHROW)
      || (!opt_for_fn (caller, flag_exceptions) && (flags & ECF_NORETURN)))
    return true;
  return false;
}

/* Return true if ECF flags says that stores can be ignored.  */

static bool
ignore_stores_p (tree caller, int flags)
{
  if (flags & (ECF_PURE | ECF_CONST | ECF_NOVOPS))
    return true;
  if ((flags & (ECF_NORETURN | ECF_NOTHROW)) == (ECF_NORETURN | ECF_NOTHROW)
      || (!opt_for_fn (caller, flag_exceptions) && (flags & ECF_NORETURN)))
    return true;
  return false;
}

/* Determine parm_map for PTR which is supposed to be a pointer.  */

modref_parm_map
parm_map_for_ptr (tree op)
{
  bool offset_known;
  poly_int64 offset;
  struct modref_parm_map parm_map;
  gcall *call;

  parm_map.parm_offset_known = false;
  parm_map.parm_offset = 0;

  offset_known = unadjusted_ptr_and_unit_offset (op, &op, &offset);
  if (TREE_CODE (op) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (op)
      && TREE_CODE (SSA_NAME_VAR (op)) == PARM_DECL)
    {
      int index = 0;

      if (cfun->static_chain_decl
	  && op == ssa_default_def (cfun, cfun->static_chain_decl))
	index = MODREF_STATIC_CHAIN_PARM;
      else
	for (tree t = DECL_ARGUMENTS (current_function_decl);
	     t != SSA_NAME_VAR (op); t = DECL_CHAIN (t))
	  index++;
      parm_map.parm_index = index;
      parm_map.parm_offset_known = offset_known;
      parm_map.parm_offset = offset;
    }
  else if (points_to_local_or_readonly_memory_p (op))
    parm_map.parm_index = MODREF_LOCAL_MEMORY_PARM;
  /* Memory allocated in the function is not visible to caller before the
     call and thus we do not need to record it as load/stores/kills.  */
  else if (TREE_CODE (op) == SSA_NAME
	   && (call = dyn_cast<gcall *>(SSA_NAME_DEF_STMT (op))) != NULL
	   && gimple_call_flags (call) & ECF_MALLOC)
    parm_map.parm_index = MODREF_LOCAL_MEMORY_PARM;
  else
    parm_map.parm_index = MODREF_UNKNOWN_PARM;
  return parm_map;
}

/* Return true if ARG with EAF flags FLAGS can not make any caller's parameter
   used (if LOAD is true we check loads, otherwise stores).  */

static bool
verify_arg (tree arg, int flags, bool load)
{
  if (flags & EAF_UNUSED)
    return true;
  if (load && (flags & EAF_NO_DIRECT_READ))
    return true;
  if (!load
      && (flags & (EAF_NO_DIRECT_CLOBBER | EAF_NO_INDIRECT_CLOBBER))
	  == (EAF_NO_DIRECT_CLOBBER | EAF_NO_INDIRECT_CLOBBER))
    return true;
  if (is_gimple_constant (arg))
    return true;
  if (DECL_P (arg) && TREE_READONLY (arg))
    return true;
  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      tree t = get_base_address (TREE_OPERAND (arg, 0));
      if (is_gimple_constant (t))
	return true;
      if (DECL_P (t)
	  && (TREE_READONLY (t) || TREE_CODE (t) == FUNCTION_DECL))
	return true;
    }
  return false;
}

/* Return true if STMT may access memory that is pointed to by parameters
   of caller and which is not seen as an escape by PTA.
   CALLEE_ECF_FLAGS are ECF flags of callee.  If LOAD is true then by access
   we mean load, otherwise we mean store.  */

static bool
may_access_nonescaping_parm_p (gcall *call, int callee_ecf_flags, bool load)
{
  int implicit_flags = 0;

  if (ignore_stores_p (current_function_decl, callee_ecf_flags))
    implicit_flags |= ignore_stores_eaf_flags;
  if (callee_ecf_flags & ECF_PURE)
    implicit_flags |= implicit_pure_eaf_flags;
  if (callee_ecf_flags & (ECF_CONST | ECF_NOVOPS))
    implicit_flags |= implicit_const_eaf_flags;
  if (gimple_call_chain (call)
      && !verify_arg (gimple_call_chain (call),
		      gimple_call_static_chain_flags (call) | implicit_flags,
		      load))
    return true;
  for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
    if (!verify_arg (gimple_call_arg (call, i),
		     gimple_call_arg_flags (call, i) | implicit_flags,
		     load))
      return true;
  return false;
}


/* Analyze memory accesses (loads, stores and kills) performed
   by the function.  Set also side_effects, calls_interposable
   and nondeterminism flags.  */

class modref_access_analysis
{
public:
  modref_access_analysis (bool ipa, modref_summary *summary,
			  modref_summary_lto *summary_lto)
  : m_summary (summary), m_summary_lto (summary_lto), m_ipa (ipa)
  {
  }
  void analyze ();
private:
  bool set_side_effects ();
  bool set_nondeterministic ();
  static modref_access_node get_access (ao_ref *ref);
  static void record_access (modref_records *, ao_ref *, modref_access_node &);
  static void record_access_lto (modref_records_lto *, ao_ref *,
				 modref_access_node &a);
  bool record_access_p (tree);
  bool record_unknown_load ();
  bool record_unknown_store ();
  bool record_global_memory_load ();
  bool record_global_memory_store ();
  bool merge_call_side_effects (gimple *, modref_summary *,
				cgraph_node *, bool);
  modref_access_node get_access_for_fnspec (gcall *, attr_fnspec &,
					    unsigned int, modref_parm_map &);
  void process_fnspec (gcall *);
  void analyze_call (gcall *);
  static bool analyze_load (gimple *, tree, tree, void *);
  static bool analyze_store (gimple *, tree, tree, void *);
  void analyze_stmt (gimple *, bool);
  void propagate ();

  /* Summary being computed.
     We work either with m_summary or m_summary_lto.  Never on both.  */
  modref_summary *m_summary;
  modref_summary_lto *m_summary_lto;
  /* Recursive calls needs simplistic dataflow after analysis finished.
     Collect all calls into this vector during analysis and later process
     them in propagate.  */
  auto_vec <gimple *, 32> m_recursive_calls;
  /* ECF flags of function being analyzed.  */
  int m_ecf_flags;
  /* True if IPA propagation will be done later.  */
  bool m_ipa;
  /* Set true if statement currently analyze is known to be
     executed each time function is called.  */
  bool m_always_executed;
};

/* Set side_effects flag and return if something changed.  */

bool
modref_access_analysis::set_side_effects ()
{
  bool changed = false;

  if (m_summary && !m_summary->side_effects)
    {
      m_summary->side_effects = true;
      changed = true;
    }
  if (m_summary_lto && !m_summary_lto->side_effects)
    {
      m_summary_lto->side_effects = true;
      changed = true;
    }
  return changed;
}

/* Set nondeterministic flag and return if something changed.  */

bool
modref_access_analysis::set_nondeterministic ()
{
  bool changed = false;

  if (m_summary && !m_summary->nondeterministic)
    {
      m_summary->side_effects = m_summary->nondeterministic = true;
      changed = true;
    }
  if (m_summary_lto && !m_summary_lto->nondeterministic)
    {
      m_summary_lto->side_effects = m_summary_lto->nondeterministic = true;
      changed = true;
    }
  return changed;
}

/* Construct modref_access_node from REF.  */

modref_access_node
modref_access_analysis::get_access (ao_ref *ref)
{
  tree base;

  base = ao_ref_base (ref);
  modref_access_node a = {ref->offset, ref->size, ref->max_size,
			  0, MODREF_UNKNOWN_PARM, false, 0};
  if (TREE_CODE (base) == MEM_REF || TREE_CODE (base) == TARGET_MEM_REF)
    {
      tree memref = base;
      modref_parm_map m = parm_map_for_ptr (TREE_OPERAND (base, 0));

      a.parm_index = m.parm_index;
      if (a.parm_index != MODREF_UNKNOWN_PARM && TREE_CODE (memref) == MEM_REF)
	{
	  a.parm_offset_known
	     = wi::to_poly_wide (TREE_OPERAND
				     (memref, 1)).to_shwi (&a.parm_offset);
	  if (a.parm_offset_known && m.parm_offset_known)
	    a.parm_offset += m.parm_offset;
	  else
	    a.parm_offset_known = false;
	}
    }
  else
    a.parm_index = MODREF_UNKNOWN_PARM;
  return a;
}

/* Record access into the modref_records data structure.  */

void
modref_access_analysis::record_access (modref_records *tt,
				       ao_ref *ref,
				       modref_access_node &a)
{
  alias_set_type base_set = !flag_strict_aliasing
			    || !flag_ipa_strict_aliasing ? 0
			    : ao_ref_base_alias_set (ref);
  alias_set_type ref_set = !flag_strict_aliasing
			   || !flag_ipa_strict_aliasing ? 0
			    : (ao_ref_alias_set (ref));
  if (dump_file)
    {
       fprintf (dump_file, "   - Recording base_set=%i ref_set=%i ",
		base_set, ref_set);
       a.dump (dump_file);
    }
  tt->insert (current_function_decl, base_set, ref_set, a, false);
}

/* IPA version of record_access_tree.  */

void
modref_access_analysis::record_access_lto (modref_records_lto *tt, ao_ref *ref,
					   modref_access_node &a)
{
  /* get_alias_set sometimes use different type to compute the alias set
     than TREE_TYPE (base).  Do same adjustments.  */
  tree base_type = NULL_TREE, ref_type = NULL_TREE;
  if (flag_strict_aliasing && flag_ipa_strict_aliasing)
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
      fprintf (dump_file, " (alias set %i) ",
	       ref_type ? get_alias_set (ref_type) : 0);
       a.dump (dump_file);
    }

  tt->insert (current_function_decl, base_type, ref_type, a, false);
}

/* Returns true if and only if we should store the access to EXPR.
   Some accesses, e.g. loads from automatic variables, are not interesting.  */

bool
modref_access_analysis::record_access_p (tree expr)
{
  if (TREE_THIS_VOLATILE (expr)
      && !ignore_nondeterminism_p (current_function_decl, 0, NULL))
    {
      if (dump_file)
	fprintf (dump_file, " (volatile; marking nondeterministic) ");
      set_nondeterministic ();
    }
  if (cfun->can_throw_non_call_exceptions
      && tree_could_throw_p (expr))
    {
      if (dump_file)
	fprintf (dump_file, " (can throw; marking side effects) ");
      set_side_effects ();
    }

  if (refs_local_or_readonly_memory_p (expr))
    {
      if (dump_file)
	fprintf (dump_file, "   - Read-only or local, ignoring.\n");
      return false;
    }
  return true;
}

/* Collapse loads and return true if something changed.  */

bool
modref_access_analysis::record_unknown_load ()
{
  bool changed = false;

  if (m_summary && !m_summary->loads->every_base)
    {
      m_summary->loads->collapse ();
      changed = true;
    }
  if (m_summary_lto && !m_summary_lto->loads->every_base)
    {
      m_summary_lto->loads->collapse ();
      changed = true;
    }
  return changed;
}

/* Collapse loads and return true if something changed.  */

bool
modref_access_analysis::record_unknown_store ()
{
  bool changed = false;

  if (m_summary && !m_summary->stores->every_base)
    {
      m_summary->stores->collapse ();
      changed = true;
    }
  if (m_summary_lto && !m_summary_lto->stores->every_base)
    {
      m_summary_lto->stores->collapse ();
      changed = true;
    }
  return changed;
}

/* Record unknown load from global memory.  */

bool
modref_access_analysis::record_global_memory_load ()
{
  bool changed = false;
  modref_access_node a = {0, -1, -1,
			  0, MODREF_GLOBAL_MEMORY_PARM, false, 0};

  if (m_summary && !m_summary->loads->every_base)
    changed |= m_summary->loads->insert (current_function_decl, 0, 0, a, false);
  if (m_summary_lto && !m_summary_lto->loads->every_base)
    changed |= m_summary_lto->loads->insert (current_function_decl,
					     0, 0, a, false);
  return changed;
}

/* Record unknown store from global memory.  */

bool
modref_access_analysis::record_global_memory_store ()
{
  bool changed = false;
  modref_access_node a = {0, -1, -1,
			  0, MODREF_GLOBAL_MEMORY_PARM, false, 0};

  if (m_summary && !m_summary->stores->every_base)
    changed |= m_summary->stores->insert (current_function_decl,
					  0, 0, a, false);
  if (m_summary_lto && !m_summary_lto->stores->every_base)
    changed |= m_summary_lto->stores->insert (current_function_decl,
					     0, 0, a, false);
  return changed;
}

/* Merge side effects of call STMT to function with CALLEE_SUMMARY.
   Return true if something changed.
   If IGNORE_STORES is true, do not merge stores.
   If RECORD_ADJUSTMENTS is true cap number of adjustments to
   a given access to make dataflow finite.  */

bool
modref_access_analysis::merge_call_side_effects
	 (gimple *stmt, modref_summary *callee_summary,
	  cgraph_node *callee_node, bool record_adjustments)
{
  gcall *call = as_a <gcall *> (stmt);
  int flags = gimple_call_flags (call);

  /* Nothing to do for non-looping cont functions.  */
  if ((flags & ECF_CONST)
      && !(flags & ECF_LOOPING_CONST_OR_PURE))
    return false;

  bool changed = false;

  if (dump_file)
    fprintf (dump_file, " - Merging side effects of %s\n",
	     callee_node->dump_name ());

  /* Merge side effects and non-determinism.
     PURE/CONST flags makes functions deterministic and if there is
     no LOOPING_CONST_OR_PURE they also have no side effects.  */
  if (!(flags & (ECF_CONST | ECF_PURE))
      || (flags & ECF_LOOPING_CONST_OR_PURE))
    {
      if (!m_summary->side_effects && callee_summary->side_effects)
	{
	  if (dump_file)
	    fprintf (dump_file, " - merging side effects.\n");
	  m_summary->side_effects = true;
	  changed = true;
	}
      if (!m_summary->nondeterministic && callee_summary->nondeterministic
	  && !ignore_nondeterminism_p (current_function_decl, flags,
				       gimple_call_fntype (call)))
	{
	  if (dump_file)
	    fprintf (dump_file, " - merging nondeterministic.\n");
	  m_summary->nondeterministic = true;
	  changed = true;
	}
     }

  /* For const functions we are done.  */
  if (flags & (ECF_CONST | ECF_NOVOPS))
    return changed;

  /* Merge calls_interposable flags.  */
  if (!m_summary->calls_interposable && callee_summary->calls_interposable)
    {
      if (dump_file)
	fprintf (dump_file, " - merging calls interposable.\n");
      m_summary->calls_interposable = true;
      changed = true;
    }

  if (!callee_node->binds_to_current_def_p () && !m_summary->calls_interposable)
    {
      if (dump_file)
	fprintf (dump_file, " - May be interposed.\n");
      m_summary->calls_interposable = true;
      changed = true;
    }

  /* Now merge the actual load, store and kill vectors.  For this we need
     to compute map translating new parameters to old.  */
  if (dump_file)
    fprintf (dump_file, "   Parm map:");

  auto_vec <modref_parm_map, 32> parm_map;
  parm_map.safe_grow_cleared (gimple_call_num_args (call), true);
  for (unsigned i = 0; i < gimple_call_num_args (call); i++)
    {
      parm_map[i] = parm_map_for_ptr (gimple_call_arg (call, i));
      if (dump_file)
	{
	  fprintf (dump_file, " %i", parm_map[i].parm_index);
	  if (parm_map[i].parm_offset_known)
	    {
	      fprintf (dump_file, " offset:");
	      print_dec ((poly_int64)parm_map[i].parm_offset,
			 dump_file, SIGNED);
	    }
	}
    }

  modref_parm_map chain_map;
  if (gimple_call_chain (call))
    {
      chain_map = parm_map_for_ptr (gimple_call_chain (call));
      if (dump_file)
	{
	  fprintf (dump_file, "static chain %i", chain_map.parm_index);
	  if (chain_map.parm_offset_known)
	    {
	      fprintf (dump_file, " offset:");
	      print_dec ((poly_int64)chain_map.parm_offset,
			 dump_file, SIGNED);
	    }
	}
    }
  if (dump_file)
    fprintf (dump_file, "\n");

  /* Kills can me merged in only if we know the function is going to be
     always executed.  */
  if (m_always_executed
      && callee_summary->kills.length ()
      && (!cfun->can_throw_non_call_exceptions
	  || !stmt_could_throw_p (cfun, call)))
    {
      /* Watch for self recursive updates.  */
      auto_vec<modref_access_node, 32> saved_kills;

      saved_kills.reserve_exact (callee_summary->kills.length ());
      saved_kills.splice (callee_summary->kills);
      for (auto kill : saved_kills)
	{
	  if (kill.parm_index >= (int)parm_map.length ())
	    continue;
	  modref_parm_map &m
		  = kill.parm_index == MODREF_STATIC_CHAIN_PARM
		    ? chain_map
		    : parm_map[kill.parm_index];
	  if (m.parm_index == MODREF_LOCAL_MEMORY_PARM
	      || m.parm_index == MODREF_UNKNOWN_PARM
	      || m.parm_index == MODREF_RETSLOT_PARM
	      || !m.parm_offset_known)
	    continue;
	  modref_access_node n = kill;
	  n.parm_index = m.parm_index;
	  n.parm_offset += m.parm_offset;
	  if (modref_access_node::insert_kill (m_summary->kills, n,
					       record_adjustments))
	    changed = true;
	}
    }

  /* Merge in loads.  */
  changed |= m_summary->loads->merge (current_function_decl,
				      callee_summary->loads,
				      &parm_map, &chain_map,
				      record_adjustments,
				      !may_access_nonescaping_parm_p
					 (call, flags, true));
  /* Merge in stores.  */
  if (!ignore_stores_p (current_function_decl, flags))
    {
      changed |= m_summary->stores->merge (current_function_decl,
					   callee_summary->stores,
					   &parm_map, &chain_map,
					   record_adjustments,
					   !may_access_nonescaping_parm_p
					       (call, flags, false));
      if (!m_summary->writes_errno
	  && callee_summary->writes_errno)
	{
	  m_summary->writes_errno = true;
	  changed = true;
	}
    }
  return changed;
}

/* Return access mode for argument I of call STMT with FNSPEC.  */

modref_access_node
modref_access_analysis::get_access_for_fnspec (gcall *call, attr_fnspec &fnspec,
					       unsigned int i,
					       modref_parm_map &map)
{
  tree size = NULL_TREE;
  unsigned int size_arg;

  if (!fnspec.arg_specified_p (i))
    ;
  else if (fnspec.arg_max_access_size_given_by_arg_p (i, &size_arg))
    size = gimple_call_arg (call, size_arg);
  else if (fnspec.arg_access_size_given_by_type_p (i))
    {
      tree callee = gimple_call_fndecl (call);
      tree t = TYPE_ARG_TYPES (TREE_TYPE (callee));

      for (unsigned int p = 0; p < i; p++)
	t = TREE_CHAIN (t);
      size = TYPE_SIZE_UNIT (TREE_TYPE (TREE_VALUE (t)));
    }
  modref_access_node a = {0, -1, -1,
			  map.parm_offset, map.parm_index,
			  map.parm_offset_known, 0};
  poly_int64 size_hwi;
  if (size
      && poly_int_tree_p (size, &size_hwi)
      && coeffs_in_range_p (size_hwi, 0,
			    HOST_WIDE_INT_MAX / BITS_PER_UNIT))
    {
      a.size = -1;
      a.max_size = size_hwi << LOG2_BITS_PER_UNIT;
    }
  return a;
}

/* Apply side effects of call STMT to CUR_SUMMARY using FNSPEC.
   If IGNORE_STORES is true ignore them.
   Return false if no useful summary can be produced.   */

void
modref_access_analysis::process_fnspec (gcall *call)
{
  int flags = gimple_call_flags (call);

  /* PURE/CONST flags makes functions deterministic and if there is
     no LOOPING_CONST_OR_PURE they also have no side effects.  */
  if (!(flags & (ECF_CONST | ECF_PURE))
      || (flags & ECF_LOOPING_CONST_OR_PURE)
      || (cfun->can_throw_non_call_exceptions
	  && stmt_could_throw_p (cfun, call)))
    {
      set_side_effects ();
      if (!ignore_nondeterminism_p (current_function_decl, flags,
				    gimple_call_fntype (call)))
	set_nondeterministic ();
    }

  /* For const functions we are done.  */
  if (flags & (ECF_CONST | ECF_NOVOPS))
    return;

  attr_fnspec fnspec = gimple_call_fnspec (call);
  /* If there is no fnpec we know nothing about loads & stores.  */
  if (!fnspec.known_p ())
    {
      if (dump_file && gimple_call_builtin_p (call, BUILT_IN_NORMAL))
	fprintf (dump_file, "      Builtin with no fnspec: %s\n",
		 IDENTIFIER_POINTER (DECL_NAME (gimple_call_fndecl (call))));
      if (!ignore_stores_p (current_function_decl, flags))
	{
	  if (!may_access_nonescaping_parm_p (call, flags, false))
	    record_global_memory_store ();
	  else
	    record_unknown_store ();
	  if (!may_access_nonescaping_parm_p (call, flags, true))
	    record_global_memory_load ();
	  else
	    record_unknown_load ();
	}
      else
	{
	  if (!may_access_nonescaping_parm_p (call, flags, true))
	    record_global_memory_load ();
	  else
	    record_unknown_load ();
	}
      return;
    }
  /* Process fnspec.  */
  if (fnspec.global_memory_read_p ())
    {
      if (may_access_nonescaping_parm_p (call, flags, true))
	record_unknown_load ();
      else
	record_global_memory_load ();
    }
  else
    {
      for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
	if (!POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, i))))
	  ;
	else if (!fnspec.arg_specified_p (i)
		 || fnspec.arg_maybe_read_p (i))
	  {
	    modref_parm_map map = parm_map_for_ptr
					(gimple_call_arg (call, i));

	    if (map.parm_index == MODREF_LOCAL_MEMORY_PARM)
	      continue;
	    if (map.parm_index == MODREF_UNKNOWN_PARM)
	      {
		record_unknown_load ();
		break;
	      }
	    modref_access_node a = get_access_for_fnspec (call, fnspec, i, map);
	    if (a.parm_index == MODREF_LOCAL_MEMORY_PARM)
	      continue;
	    if (m_summary)
	      m_summary->loads->insert (current_function_decl, 0, 0, a, false);
	    if (m_summary_lto)
	      m_summary_lto->loads->insert (current_function_decl, 0, 0, a,
					    false);
	  }
    }
  if (ignore_stores_p (current_function_decl, flags))
    return;
  if (fnspec.global_memory_written_p ())
    {
      if (may_access_nonescaping_parm_p (call, flags, false))
	record_unknown_store ();
      else
	record_global_memory_store ();
    }
  else
    {
      for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
	if (!POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, i))))
	  ;
	else if (!fnspec.arg_specified_p (i)
		 || fnspec.arg_maybe_written_p (i))
	  {
	    modref_parm_map map = parm_map_for_ptr
					 (gimple_call_arg (call, i));

	    if (map.parm_index == MODREF_LOCAL_MEMORY_PARM)
	      continue;
	    if (map.parm_index == MODREF_UNKNOWN_PARM)
	      {
		record_unknown_store ();
		break;
	      }
	    modref_access_node a = get_access_for_fnspec (call, fnspec, i, map);
	    if (a.parm_index == MODREF_LOCAL_MEMORY_PARM)
	      continue;
	    if (m_summary)
	      m_summary->stores->insert (current_function_decl, 0, 0, a, false);
	    if (m_summary_lto)
	      m_summary_lto->stores->insert (current_function_decl,
					     0, 0, a, false);
	  }
      if (fnspec.errno_maybe_written_p () && flag_errno_math)
	{
	  if (m_summary)
	    m_summary->writes_errno = true;
	  if (m_summary_lto)
	    m_summary_lto->writes_errno = true;
	}
    }
}

/* Analyze function call STMT in function F.
   Remember recursive calls in RECURSIVE_CALLS.  */

void
modref_access_analysis::analyze_call (gcall *stmt)
{
  /* Check flags on the function call.  In certain cases, analysis can be
     simplified.  */
  int flags = gimple_call_flags (stmt);

  if (dump_file)
    {
      fprintf (dump_file, " - Analyzing call:");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  if ((flags & ECF_CONST)
      && !(flags & ECF_LOOPING_CONST_OR_PURE))
    {
      if (dump_file)
	fprintf (dump_file,
		 " - ECF_CONST, ignoring all stores and all loads "
		 "except for args.\n");
      return;
    }

  /* Next, we try to get the callee's function declaration.  The goal is to
     merge their summary with ours.  */
  tree callee = gimple_call_fndecl (stmt);

  /* Check if this is an indirect call.  */
  if (!callee)
    {
      if (dump_file)
	fprintf (dump_file, gimple_call_internal_p (stmt)
		 ? " - Internal call" : " - Indirect call.\n");
      process_fnspec (stmt);
      return;
    }
  /* We only need to handle internal calls in IPA mode.  */
  gcc_checking_assert (!m_summary_lto && !m_ipa);

  struct cgraph_node *callee_node = cgraph_node::get_create (callee);

  /* If this is a recursive call, the target summary is the same as ours, so
     there's nothing to do.  */
  if (recursive_call_p (current_function_decl, callee))
    {
      m_recursive_calls.safe_push (stmt);
      set_side_effects ();
      if (dump_file)
	fprintf (dump_file, " - Skipping recursive call.\n");
      return;
    }

  gcc_assert (callee_node != NULL);

  /* Get the function symbol and its availability.  */
  enum availability avail;
  callee_node = callee_node->function_symbol (&avail);
  bool looping;
  if (builtin_safe_for_const_function_p (&looping, callee))
    {
      if (looping)
	set_side_effects ();
      if (dump_file)
	fprintf (dump_file, " - Builtin is safe for const.\n");
      return;
    }
  if (avail <= AVAIL_INTERPOSABLE)
    {
      if (dump_file)
	fprintf (dump_file,
		 " - Function availability <= AVAIL_INTERPOSABLE.\n");
      process_fnspec (stmt);
      return;
    }

  /* Get callee's modref summary.  As above, if there's no summary, we either
     have to give up or, if stores are ignored, we can just purge loads.  */
  modref_summary *callee_summary = optimization_summaries->get (callee_node);
  if (!callee_summary)
    {
      if (dump_file)
	fprintf (dump_file, " - No modref summary available for callee.\n");
      process_fnspec (stmt);
      return;
    }

  merge_call_side_effects (stmt, callee_summary, callee_node, false);

  return;
}

/* Helper for analyze_stmt.  */

bool
modref_access_analysis::analyze_load (gimple *, tree, tree op, void *data)
{
  modref_access_analysis *t = (modref_access_analysis *)data;

  if (dump_file)
    {
      fprintf (dump_file, " - Analyzing load: ");
      print_generic_expr (dump_file, op);
      fprintf (dump_file, "\n");
    }

  if (!t->record_access_p (op))
    return false;

  ao_ref r;
  ao_ref_init (&r, op);
  modref_access_node a = get_access (&r);
  if (a.parm_index == MODREF_LOCAL_MEMORY_PARM)
    return false;

  if (t->m_summary)
    t->record_access (t->m_summary->loads, &r, a);
  if (t->m_summary_lto)
    t->record_access_lto (t->m_summary_lto->loads, &r, a);
  return false;
}

/* Helper for analyze_stmt.  */

bool
modref_access_analysis::analyze_store (gimple *stmt, tree, tree op, void *data)
{
  modref_access_analysis *t = (modref_access_analysis *)data;

  if (dump_file)
    {
      fprintf (dump_file, " - Analyzing store: ");
      print_generic_expr (dump_file, op);
      fprintf (dump_file, "\n");
    }

  if (!t->record_access_p (op))
    return false;

  ao_ref r;
  ao_ref_init (&r, op);
  modref_access_node a = get_access (&r);
  if (a.parm_index == MODREF_LOCAL_MEMORY_PARM)
    return false;

  if (t->m_summary)
    t->record_access (t->m_summary->stores, &r, a);
  if (t->m_summary_lto)
    t->record_access_lto (t->m_summary_lto->stores, &r, a);
  if (t->m_always_executed
      && a.useful_for_kill_p ()
      && (!cfun->can_throw_non_call_exceptions
	  || !stmt_could_throw_p (cfun, stmt)))
    {
      if (dump_file)
	fprintf (dump_file, "   - Recording kill\n");
      if (t->m_summary)
	modref_access_node::insert_kill (t->m_summary->kills, a, false);
      if (t->m_summary_lto)
	modref_access_node::insert_kill (t->m_summary_lto->kills, a, false);
    }
  return false;
}

/* Analyze statement STMT of function F.
   If IPA is true do not merge in side effects of calls.  */

void
modref_access_analysis::analyze_stmt (gimple *stmt, bool always_executed)
{
  m_always_executed = always_executed;
  /* In general we can not ignore clobbers because they are barriers for code
     motion, however after inlining it is safe to do because local optimization
     passes do not consider clobbers from other functions.
     Similar logic is in ipa-pure-const.cc.  */
  if ((m_ipa || cfun->after_inlining) && gimple_clobber_p (stmt))
    {
      if (always_executed && record_access_p (gimple_assign_lhs (stmt)))
	{
	  ao_ref r;
	  ao_ref_init (&r, gimple_assign_lhs (stmt));
	  modref_access_node a = get_access (&r);
	  if (a.useful_for_kill_p ())
	    {
	      if (dump_file)
		fprintf (dump_file, "   - Recording kill\n");
	      if (m_summary)
		modref_access_node::insert_kill (m_summary->kills, a, false);
	      if (m_summary_lto)
		modref_access_node::insert_kill (m_summary_lto->kills,
						 a, false);
	    }
	}
      return;
    }

  /* Analyze all loads and stores in STMT.  */
  walk_stmt_load_store_ops (stmt, this,
			    analyze_load, analyze_store);

  switch (gimple_code (stmt))
   {
   case GIMPLE_ASM:
      if (gimple_asm_volatile_p (as_a <gasm *> (stmt))
	  && !ignore_nondeterminism_p (current_function_decl, 0, NULL))
	set_nondeterministic ();
      if (cfun->can_throw_non_call_exceptions
	  && stmt_could_throw_p (cfun, stmt))
	set_side_effects ();
     /* If the ASM statement does not read nor write memory, there's nothing
	to do.  Otherwise just give up.  */
     if (!gimple_asm_clobbers_memory_p (as_a <gasm *> (stmt)))
       return;
     if (dump_file)
       fprintf (dump_file, " - Function contains GIMPLE_ASM statement "
	       "which clobbers memory.\n");
     record_unknown_load ();
     record_unknown_store ();
     return;
   case GIMPLE_CALL:
     if (!m_ipa || gimple_call_internal_p (stmt))
       analyze_call (as_a <gcall *> (stmt));
     else
       {
	 attr_fnspec fnspec = gimple_call_fnspec (as_a <gcall *>(stmt));

	 if (fnspec.known_p ()
	     && (!fnspec.global_memory_read_p ()
		 || !fnspec.global_memory_written_p ()))
	   {
	     cgraph_edge *e = cgraph_node::get
				  (current_function_decl)->get_edge (stmt);
	     if (e->callee)
	       {
		 fnspec_summaries->get_create (e)->fnspec
			  = xstrdup (fnspec.get_str ());
		 if (dump_file)
		   fprintf (dump_file, "  Recorded fnspec %s\n",
			    fnspec.get_str ());
	       }
	   }
       }
     return;
   default:
     if (cfun->can_throw_non_call_exceptions
	 && stmt_could_throw_p (cfun, stmt))
	set_side_effects ();
     return;
   }
}

/* Propagate load/stores across recursive calls.  */

void
modref_access_analysis::propagate ()
{
  if (m_ipa && m_summary)
    return;

  bool changed = true;
  bool first = true;
  cgraph_node *fnode = cgraph_node::get (current_function_decl);

  m_always_executed = false;
  while (changed && m_summary->useful_p (m_ecf_flags, false))
    {
      changed = false;
      for (unsigned i = 0; i < m_recursive_calls.length (); i++)
	{
	  changed |= merge_call_side_effects (m_recursive_calls[i], m_summary,
					      fnode, !first);
	}
      first = false;
    }
}

/* Analyze function.  */

void
modref_access_analysis::analyze ()
{
  m_ecf_flags = flags_from_decl_or_type (current_function_decl);
  bool summary_useful = true;

  /* Analyze each statement in each basic block of the function.  If the
     statement cannot be analyzed (for any reason), the entire function cannot
     be analyzed by modref.  */
  basic_block bb;
  bitmap always_executed_bbs = find_always_executed_bbs (cfun, true);
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator si;
      bool always_executed = bitmap_bit_p (always_executed_bbs, bb->index);

      for (si = gsi_start_nondebug_after_labels_bb (bb);
	   !gsi_end_p (si); gsi_next_nondebug (&si))
	{
	  /* NULL memory accesses terminates BB.  These accesses are known
	     to trip undefined behavior.  gimple-ssa-isolate-paths turns them
	     to volatile accesses and adds builtin_trap call which would
	     confuse us otherwise.  */
	  if (infer_nonnull_range_by_dereference (gsi_stmt (si),
						  null_pointer_node))
	    {
	      if (dump_file)
		fprintf (dump_file, " - NULL memory access; terminating BB\n");
	      if (flag_non_call_exceptions)
		set_side_effects ();
	      break;
	    }
	  analyze_stmt (gsi_stmt (si), always_executed);

	  /* Avoid doing useless work.  */
	  if ((!m_summary || !m_summary->useful_p (m_ecf_flags, false))
	      && (!m_summary_lto
		  || !m_summary_lto->useful_p (m_ecf_flags, false)))
	    {
	      summary_useful = false;
	      break;
	    }
	  if (always_executed
	      && stmt_can_throw_external (cfun, gsi_stmt (si)))
	    always_executed = false;
	}
      if (!summary_useful)
	break;
    }
  /* In non-IPA mode we need to perform iterative dataflow on recursive calls.
     This needs to be done after all other side effects are computed.  */
  if (summary_useful)
    {
      if (!m_ipa)
	propagate ();
      if (m_summary && !m_summary->side_effects && !finite_function_p ())
	m_summary->side_effects = true;
      if (m_summary_lto && !m_summary_lto->side_effects
	  && !finite_function_p ())
	m_summary_lto->side_effects = true;
    }
  BITMAP_FREE (always_executed_bbs);
}

/* Return true if OP accesses memory pointed to by SSA_NAME.  */

bool
memory_access_to (tree op, tree ssa_name)
{
  tree base = get_base_address (op);
  if (!base)
    return false;
  if (TREE_CODE (base) != MEM_REF && TREE_CODE (base) != TARGET_MEM_REF)
    return false;
  return TREE_OPERAND (base, 0) == ssa_name;
}

/* Consider statement val = *arg.
   return EAF flags of ARG that can be determined from EAF flags of VAL
   (which are known to be FLAGS).  If IGNORE_STORES is true we can ignore
   all stores to VAL, i.e. when handling noreturn function.  */

static int
deref_flags (int flags, bool ignore_stores)
{
  /* Dereference is also a direct read but dereferenced value does not
     yield any other direct use.  */
  int ret = EAF_NO_DIRECT_CLOBBER | EAF_NO_DIRECT_ESCAPE
	    | EAF_NOT_RETURNED_DIRECTLY;
  /* If argument is unused just account for
     the read involved in dereference.  */
  if (flags & EAF_UNUSED)
    ret |= EAF_NO_INDIRECT_READ | EAF_NO_INDIRECT_CLOBBER
	   | EAF_NO_INDIRECT_ESCAPE;
  else
    {
      /* Direct or indirect accesses leads to indirect accesses.  */
      if (((flags & EAF_NO_DIRECT_CLOBBER)
	   && (flags & EAF_NO_INDIRECT_CLOBBER))
	  || ignore_stores)
	ret |= EAF_NO_INDIRECT_CLOBBER;
      if (((flags & EAF_NO_DIRECT_ESCAPE)
	   && (flags & EAF_NO_INDIRECT_ESCAPE))
	  || ignore_stores)
	ret |= EAF_NO_INDIRECT_ESCAPE;
      if ((flags & EAF_NO_DIRECT_READ)
	   && (flags & EAF_NO_INDIRECT_READ))
	ret |= EAF_NO_INDIRECT_READ;
      if ((flags & EAF_NOT_RETURNED_DIRECTLY)
	  && (flags & EAF_NOT_RETURNED_INDIRECTLY))
	ret |= EAF_NOT_RETURNED_INDIRECTLY;
    }
  return ret;
}


/* Description of an escape point: a call which affects flags of a given
   SSA name.  */

struct escape_point
{
  /* Value escapes to this call.  */
  gcall *call;
  /* Argument it escapes to.  */
  unsigned int arg;
  /* Flags already known about the argument (this can save us from recording
     escape points if local analysis did good job already).  */
  eaf_flags_t min_flags;
  /* Does value escape directly or indirectly?  */
  bool direct;
};

/* Lattice used during the eaf flags analysis dataflow.  For a given SSA name
   we aim to compute its flags and escape points.  We also use the lattice
   to dynamically build dataflow graph to propagate on.  */

class modref_lattice
{
public:
  /* EAF flags of the SSA name.  */
  eaf_flags_t flags;
  /* Used during DFS walk to mark names where final value was determined
     without need for dataflow.  */
  bool known;
  /* Used during DFS walk to mark open vertices (for cycle detection).  */
  bool open;
  /* Set during DFS walk for names that needs dataflow propagation.  */
  bool do_dataflow;
  /* Used during the iterative dataflow.  */
  bool changed;

  /* When doing IPA analysis we can not merge in callee escape points;
     Only remember them and do the merging at IPA propagation time.  */
  vec <escape_point, va_heap, vl_ptr> escape_points;

  /* Representation of a graph for dataflow.  This graph is built on-demand
     using modref_eaf_analysis::analyze_ssa and later solved by
     modref_eaf_analysis::propagate.
     Each edge represents the fact that flags of current lattice should be
     propagated to lattice of SSA_NAME.  */
  struct propagate_edge
  {
    int ssa_name;
    bool deref;
  };
  vec <propagate_edge, va_heap, vl_ptr> propagate_to;

  void init ();
  void release ();
  bool merge (const modref_lattice &with);
  bool merge (int flags);
  bool merge_deref (const modref_lattice &with, bool ignore_stores);
  bool merge_direct_load ();
  bool merge_direct_store ();
  bool add_escape_point (gcall *call, unsigned int arg,
			 eaf_flags_t min_flags, bool direct);
  void dump (FILE *out, int indent = 0) const;
};

/* Lattices are saved to vectors, so keep them PODs.  */
void
modref_lattice::init ()
{
  /* All flags we track.  */
  int f = EAF_NO_DIRECT_CLOBBER | EAF_NO_INDIRECT_CLOBBER
	  | EAF_NO_DIRECT_ESCAPE | EAF_NO_INDIRECT_ESCAPE
	  | EAF_NO_DIRECT_READ | EAF_NO_INDIRECT_READ
	  | EAF_NOT_RETURNED_DIRECTLY | EAF_NOT_RETURNED_INDIRECTLY
	  | EAF_UNUSED;
  flags = f;
  /* Check that eaf_flags_t is wide enough to hold all flags.  */
  gcc_checking_assert (f == flags);
  open = true;
  known = false;
}

/* Release memory.  */
void
modref_lattice::release ()
{
  escape_points.release ();
  propagate_to.release ();
}

/* Dump lattice to OUT; indent with INDENT spaces.  */

void
modref_lattice::dump (FILE *out, int indent) const
{
  dump_eaf_flags (out, flags);
  if (escape_points.length ())
    {
      fprintf (out, "%*sEscapes:\n", indent, "");
      for (unsigned int i = 0; i < escape_points.length (); i++)
	{
	  fprintf (out, "%*s  Arg %i (%s) min flags", indent, "",
		   escape_points[i].arg,
		   escape_points[i].direct ? "direct" : "indirect");
	  dump_eaf_flags (out, escape_points[i].min_flags, false);
	  fprintf (out, " in call ");
	  print_gimple_stmt (out, escape_points[i].call, 0);
	}
    }
}

/* Add escape point CALL, ARG, MIN_FLAGS, DIRECT.  Return false if such escape
   point exists.  */

bool
modref_lattice::add_escape_point (gcall *call, unsigned arg,
				  eaf_flags_t min_flags, bool direct)
{
  escape_point *ep;
  unsigned int i;

  /* If we already determined flags to be bad enough,
     we do not need to record.  */
  if ((flags & min_flags) == flags || (min_flags & EAF_UNUSED))
    return false;

  FOR_EACH_VEC_ELT (escape_points, i, ep)
    if (ep->call == call && ep->arg == arg && ep->direct == direct)
      {
	if ((ep->min_flags & min_flags) == min_flags)
	  return false;
	ep->min_flags &= min_flags;
	return true;
      }
  /* Give up if max escape points is met.  */
  if ((int)escape_points.length () > param_modref_max_escape_points)
    {
      if (dump_file)
	fprintf (dump_file, "--param modref-max-escape-points limit reached\n");
      merge (0);
      return true;
    }
  escape_point new_ep = {call, arg, min_flags, direct};
  escape_points.safe_push (new_ep);
  return true;
}

/* Merge in flags from F.  */
bool
modref_lattice::merge (int f)
{
  if (f & EAF_UNUSED)
    return false;
  /* Check that flags seems sane: if function does not read the parameter
     it can not access it indirectly.  */
  gcc_checking_assert (!(f & EAF_NO_DIRECT_READ)
		       || ((f & EAF_NO_INDIRECT_READ)
			   && (f & EAF_NO_INDIRECT_CLOBBER)
			   && (f & EAF_NO_INDIRECT_ESCAPE)
			   && (f & EAF_NOT_RETURNED_INDIRECTLY)));
  if ((flags & f) != flags)
    {
      flags &= f;
      /* Prune obviously useless flags;
	 We do not have ECF_FLAGS handy which is not big problem since
	 we will do final flags cleanup before producing summary.
	 Merging should be fast so it can work well with dataflow.  */
      flags = remove_useless_eaf_flags (flags, 0, false);
      if (!flags)
	escape_points.release ();
      return true;
    }
  return false;
}

/* Merge in WITH.  Return true if anything changed.  */

bool
modref_lattice::merge (const modref_lattice &with)
{
  if (!with.known)
    do_dataflow = true;

  bool changed = merge (with.flags);

  if (!flags)
    return changed;
  for (unsigned int i = 0; i < with.escape_points.length (); i++)
    changed |= add_escape_point (with.escape_points[i].call,
				 with.escape_points[i].arg,
				 with.escape_points[i].min_flags,
				 with.escape_points[i].direct);
  return changed;
}

/* Merge in deref of WITH.  If IGNORE_STORES is true do not consider
   stores.  Return true if anything changed.  */

bool
modref_lattice::merge_deref (const modref_lattice &with, bool ignore_stores)
{
  if (!with.known)
    do_dataflow = true;

  bool changed = merge (deref_flags (with.flags, ignore_stores));

  if (!flags)
    return changed;
  for (unsigned int i = 0; i < with.escape_points.length (); i++)
    {
      int min_flags = with.escape_points[i].min_flags;

      if (with.escape_points[i].direct)
	min_flags = deref_flags (min_flags, ignore_stores);
      else if (ignore_stores)
	min_flags |= ignore_stores_eaf_flags;
      changed |= add_escape_point (with.escape_points[i].call,
				   with.escape_points[i].arg,
				   min_flags,
				   false);
    }
  return changed;
}

/* Merge in flags for direct load.  */

bool
modref_lattice::merge_direct_load ()
{
  return merge (~(EAF_UNUSED | EAF_NO_DIRECT_READ));
}

/* Merge in flags for direct store.  */

bool
modref_lattice::merge_direct_store ()
{
  return merge (~(EAF_UNUSED | EAF_NO_DIRECT_CLOBBER));
}

/* Analyzer of EAF flags.
   This is generally dataflow problem over the SSA graph, however we only
   care about flags of few selected ssa names (arguments, return slot and
   static chain).  So we first call analyze_ssa_name on all relevant names
   and perform a DFS walk to discover SSA names where flags needs to be
   determined.  For acyclic graphs we try to determine final flags during
   this walk.  Once cycles or recursion depth is met we enlist SSA names
   for dataflow which is done by propagate call.

   After propagation the flags can be obtained using get_ssa_name_flags.  */

class modref_eaf_analysis
{
public:
  /* Mark NAME as relevant for analysis.  */
  void analyze_ssa_name (tree name, bool deferred = false);
  /* Dataflow solver.  */
  void propagate ();
  /* Return flags computed earlier for NAME.  */
  int get_ssa_name_flags (tree name)
  {
    int version = SSA_NAME_VERSION (name);
    gcc_checking_assert (m_lattice[version].known);
    return m_lattice[version].flags;
  }
  /* In IPA mode this will record all escape points
     determined for NAME to PARM_IDNEX.  Flags are minimal
     flags known.  */
  void record_escape_points (tree name, int parm_index, int flags);
  modref_eaf_analysis (bool ipa)
  {
    m_ipa = ipa;
    m_depth = 0;
    m_lattice.safe_grow_cleared (num_ssa_names, true);
  }
  ~modref_eaf_analysis ()
  {
    gcc_checking_assert (!m_depth);
    if (m_ipa || m_names_to_propagate.length ())
      for (unsigned int i = 0; i < num_ssa_names; i++)
	m_lattice[i].release ();
  }
private:
  /* If true, we produce analysis for IPA mode.  In this case escape points are
     collected.  */
  bool m_ipa;
  /* Depth of recursion of analyze_ssa_name.  */
  int m_depth;
  /* Propagation lattice for individual ssa names.  */
  auto_vec<modref_lattice> m_lattice;
  auto_vec<tree> m_deferred_names;
  auto_vec<int> m_names_to_propagate;

  void merge_with_ssa_name (tree dest, tree src, bool deref);
  void merge_call_lhs_flags (gcall *call, int arg, tree name, bool direct,
			     bool deref);
};


/* Call statements may return their parameters.  Consider argument number
   ARG of USE_STMT and determine flags that can needs to be cleared
   in case pointer possibly indirectly references from ARG I is returned.
   If DIRECT is true consider direct returns and if INDIRECT consider
   indirect returns.
   LATTICE, DEPTH and ipa are same as in analyze_ssa_name.
   ARG is set to -1 for static chain.  */

void
modref_eaf_analysis::merge_call_lhs_flags (gcall *call, int arg,
					   tree name, bool direct,
					   bool indirect)
{
  int index = SSA_NAME_VERSION (name);
  bool returned_directly = false;

  /* If there is no return value, no flags are affected.  */
  if (!gimple_call_lhs (call))
    return;

  /* If we know that function returns given argument and it is not ARG
     we can still be happy.  */
  if (arg >= 0)
    {
      int flags = gimple_call_return_flags (call);
      if (flags & ERF_RETURNS_ARG)
	{
	  if ((flags & ERF_RETURN_ARG_MASK) == arg)
	    returned_directly = true;
	  else
	   return;
	}
    }
  /* Make ERF_RETURNS_ARG overwrite EAF_UNUSED.  */
  if (returned_directly)
    {
      direct = true;
      indirect = false;
    }
  /* If value is not returned at all, do nothing.  */
  else if (!direct && !indirect)
    return;

  /* If return value is SSA name determine its flags.  */
  if (TREE_CODE (gimple_call_lhs (call)) == SSA_NAME)
    {
      tree lhs = gimple_call_lhs (call);
      if (direct)
	merge_with_ssa_name (name, lhs, false);
      if (indirect)
	merge_with_ssa_name (name, lhs, true);
    }
  /* In the case of memory store we can do nothing.  */
  else if (!direct)
    m_lattice[index].merge (deref_flags (0, false));
  else
    m_lattice[index].merge (0);
}

/* CALL_FLAGS are EAF_FLAGS of the argument.  Turn them
   into flags for caller, update LATTICE of corresponding
   argument if needed.  */

static int
callee_to_caller_flags (int call_flags, bool ignore_stores,
			modref_lattice &lattice)
{
  /* call_flags is about callee returning a value
     that is not the same as caller returning it.  */
  call_flags |= EAF_NOT_RETURNED_DIRECTLY
		| EAF_NOT_RETURNED_INDIRECTLY;
  if (!ignore_stores && !(call_flags & EAF_UNUSED))
    {
      /* If value escapes we are no longer able to track what happens
	 with it because we can read it from the escaped location
	 anytime.  */
      if (!(call_flags & EAF_NO_DIRECT_ESCAPE))
	lattice.merge (0);
      else if (!(call_flags & EAF_NO_INDIRECT_ESCAPE))
	lattice.merge (~(EAF_NOT_RETURNED_INDIRECTLY
			 | EAF_NO_DIRECT_READ
			 | EAF_NO_INDIRECT_READ
			 | EAF_NO_INDIRECT_CLOBBER
			 | EAF_UNUSED));
    }
  else
    call_flags |= ignore_stores_eaf_flags;
  return call_flags;
}

/* Analyze EAF flags for SSA name NAME and store result to LATTICE.
   LATTICE is an array of modref_lattices.
   DEPTH is a recursion depth used to make debug output prettier.
   If IPA is true we analyze for IPA propagation (and thus call escape points
   are processed later)  */

void
modref_eaf_analysis::analyze_ssa_name (tree name, bool deferred)
{
  imm_use_iterator ui;
  gimple *use_stmt;
  int index = SSA_NAME_VERSION (name);

  if (!deferred)
    {
      /* See if value is already computed.  */
      if (m_lattice[index].known || m_lattice[index].do_dataflow)
       return;
      if (m_lattice[index].open)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "%*sCycle in SSA graph\n",
		     m_depth * 4, "");
	  return;
	}
      /* Recursion guard.  */
      m_lattice[index].init ();
      if (m_depth == param_modref_max_depth)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "%*sMax recursion depth reached; postponing\n",
		     m_depth * 4, "");
	  m_deferred_names.safe_push (name);
	  return;
	}
    }

  if (dump_file)
    {
      fprintf (dump_file,
	       "%*sAnalyzing flags of ssa name: ", m_depth * 4, "");
      print_generic_expr (dump_file, name);
      fprintf (dump_file, "\n");
    }

  FOR_EACH_IMM_USE_STMT (use_stmt, ui, name)
    {
      if (m_lattice[index].flags == 0)
	break;
      if (is_gimple_debug (use_stmt))
	continue;
      if (dump_file)
	{
	  fprintf (dump_file, "%*s  Analyzing stmt: ", m_depth * 4, "");
	  print_gimple_stmt (dump_file, use_stmt, 0);
	}
      /* If we see a direct non-debug use, clear unused bit.
	 All dereferences should be accounted below using deref_flags.  */
      m_lattice[index].merge (~EAF_UNUSED);

      /* Gimple return may load the return value.
	 Returning name counts as an use by tree-ssa-structalias.cc  */
      if (greturn *ret = dyn_cast <greturn *> (use_stmt))
	{
	  /* Returning through return slot is seen as memory write earlier.  */
	  if (DECL_RESULT (current_function_decl)
	      && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
	    ;
	  else if (gimple_return_retval (ret) == name)
	    m_lattice[index].merge (~(EAF_UNUSED | EAF_NOT_RETURNED_DIRECTLY
				      | EAF_NOT_RETURNED_DIRECTLY));
	  else if (memory_access_to (gimple_return_retval (ret), name))
	    {
	      m_lattice[index].merge_direct_load ();
	      m_lattice[index].merge (~(EAF_UNUSED
					| EAF_NOT_RETURNED_INDIRECTLY));
	    }
	}
      /* Account for LHS store, arg loads and flags from callee function.  */
      else if (gcall *call = dyn_cast <gcall *> (use_stmt))
	{
	  tree callee = gimple_call_fndecl (call);

	  /* IPA PTA internally it treats calling a function as "writing" to
	     the argument space of all functions the function pointer points to
	     (PR101949).  We can not drop EAF_NOCLOBBER only when ipa-pta
	     is on since that would allow propagation of this from -fno-ipa-pta
	     to -fipa-pta functions.  */
	  if (gimple_call_fn (use_stmt) == name)
	    m_lattice[index].merge (~(EAF_NO_DIRECT_CLOBBER | EAF_UNUSED));

	  /* Recursion would require bit of propagation; give up for now.  */
	  if (callee && !m_ipa && recursive_call_p (current_function_decl,
						  callee))
	    m_lattice[index].merge (0);
	  else
	    {
	      int ecf_flags = gimple_call_flags (call);
	      bool ignore_stores = ignore_stores_p (current_function_decl,
						    ecf_flags);
	      bool ignore_retval = ignore_retval_p (current_function_decl,
						    ecf_flags);

	      /* Handle *name = func (...).  */
	      if (gimple_call_lhs (call)
		  && memory_access_to (gimple_call_lhs (call), name))
		{
		  m_lattice[index].merge_direct_store ();
		  /* Return slot optimization passes address of
		     LHS to callee via hidden parameter and this
		     may make LHS to escape.  See PR 98499.  */
		  if (gimple_call_return_slot_opt_p (call)
		      && TREE_ADDRESSABLE (TREE_TYPE (gimple_call_lhs (call))))
		    {
		      int call_flags = gimple_call_retslot_flags (call);
		      bool isretslot = false;

		      if (DECL_RESULT (current_function_decl)
			  && DECL_BY_REFERENCE
				(DECL_RESULT (current_function_decl)))
			isretslot = ssa_default_def
					 (cfun,
					  DECL_RESULT (current_function_decl))
					 == name;

		      /* Passing returnslot to return slot is special because
			 not_returned and escape has same meaning.
			 However passing arg to return slot is different.  If
			 the callee's return slot is returned it means that
			 arg is written to itself which is an escape.
			 Since we do not track the memory it is written to we
			 need to give up on analyzing it.  */
		      if (!isretslot)
			{
			  if (!(call_flags & (EAF_NOT_RETURNED_DIRECTLY
					      | EAF_UNUSED)))
			    m_lattice[index].merge (0);
			  else gcc_checking_assert
				(call_flags & (EAF_NOT_RETURNED_INDIRECTLY
					       | EAF_UNUSED));
			  call_flags = callee_to_caller_flags
					   (call_flags, false,
					    m_lattice[index]);
			}
		      m_lattice[index].merge (call_flags);
		    }
		}

	      if (gimple_call_chain (call)
		  && (gimple_call_chain (call) == name))
		{
		  int call_flags = gimple_call_static_chain_flags (call);
		  if (!ignore_retval && !(call_flags & EAF_UNUSED))
		    merge_call_lhs_flags
			 (call, -1, name,
			  !(call_flags & EAF_NOT_RETURNED_DIRECTLY),
			  !(call_flags & EAF_NOT_RETURNED_INDIRECTLY));
		  call_flags = callee_to_caller_flags
				   (call_flags, ignore_stores,
				    m_lattice[index]);
		  if (!(ecf_flags & (ECF_CONST | ECF_NOVOPS)))
		    m_lattice[index].merge (call_flags);
		}

	      /* Process internal functions and right away.  */
	      bool record_ipa = m_ipa && !gimple_call_internal_p (call);

	      /* Handle all function parameters.  */
	      for (unsigned i = 0;
		   i < gimple_call_num_args (call)
		   && m_lattice[index].flags; i++)
		/* Name is directly passed to the callee.  */
		if (gimple_call_arg (call, i) == name)
		  {
		    int call_flags = gimple_call_arg_flags (call, i);
		    if (!ignore_retval)
		      merge_call_lhs_flags
			      (call, i, name,
			       !(call_flags & (EAF_NOT_RETURNED_DIRECTLY
					       | EAF_UNUSED)),
			       !(call_flags & (EAF_NOT_RETURNED_INDIRECTLY
					       | EAF_UNUSED)));
		    if (!(ecf_flags & (ECF_CONST | ECF_NOVOPS)))
		      {
			call_flags = callee_to_caller_flags
					 (call_flags, ignore_stores,
					  m_lattice[index]);
			if (!record_ipa)
			  m_lattice[index].merge (call_flags);
			else
			  m_lattice[index].add_escape_point (call, i,
							   call_flags, true);
		      }
		  }
		/* Name is dereferenced and passed to a callee.  */
		else if (memory_access_to (gimple_call_arg (call, i), name))
		  {
		    int call_flags = deref_flags
			    (gimple_call_arg_flags (call, i), ignore_stores);
		    if (!ignore_retval && !(call_flags & EAF_UNUSED)
			&& (call_flags & (EAF_NOT_RETURNED_DIRECTLY
				       	  | EAF_NOT_RETURNED_INDIRECTLY))
			    != (EAF_NOT_RETURNED_DIRECTLY
				| EAF_NOT_RETURNED_INDIRECTLY))
		      merge_call_lhs_flags (call, i, name, false, true);
		    if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
		      m_lattice[index].merge_direct_load ();
		    else
		      {
			call_flags = callee_to_caller_flags
					 (call_flags, ignore_stores,
					  m_lattice[index]);
			if (!record_ipa)
			  m_lattice[index].merge (call_flags);
			else
			  m_lattice[index].add_escape_point (call, i,
							     call_flags, false);
		      }
		  }
	    }
	}
      else if (gimple_assign_load_p (use_stmt))
	{
	  gassign *assign = as_a <gassign *> (use_stmt);
	  /* Memory to memory copy.  */
	  if (gimple_store_p (assign))
	    {
	      /* Handle *lhs = *name.

		 We do not track memory locations, so assume that value
		 is used arbitrarily.  */
	      if (memory_access_to (gimple_assign_rhs1 (assign), name))
		m_lattice[index].merge (deref_flags (0, false));

	      /* Handle *name = *exp.  */
	      if (memory_access_to (gimple_assign_lhs (assign), name))
		m_lattice[index].merge_direct_store ();
	    }
	  /* Handle lhs = *name.  */
	  else if (memory_access_to (gimple_assign_rhs1 (assign), name))
	    {
	      tree lhs = gimple_assign_lhs (assign);
	      merge_with_ssa_name (name, lhs, true);
	    }
	}
      else if (gimple_store_p (use_stmt))
	{
	  gassign *assign = dyn_cast <gassign *> (use_stmt);

	  /* Handle *lhs = name.  */
	  if (assign && gimple_assign_rhs1 (assign) == name)
	    {
	      if (dump_file)
		fprintf (dump_file, "%*s  ssa name saved to memory\n",
			 m_depth * 4, "");
	      m_lattice[index].merge (0);
	    }
	  /* Handle *name = exp.  */
	  else if (assign
		   && memory_access_to (gimple_assign_lhs (assign), name))
	    {
	      /* In general we can not ignore clobbers because they are
		 barriers for code motion, however after inlining it is safe to
		 do because local optimization passes do not consider clobbers
		 from other functions.
		 Similar logic is in ipa-pure-const.cc.  */
	      if (!cfun->after_inlining || !gimple_clobber_p (assign))
		m_lattice[index].merge_direct_store ();
	    }
	  /* ASM statements etc.  */
	  else if (!assign)
	    {
	      if (dump_file)
		fprintf (dump_file, "%*s  Unhandled store\n", m_depth * 4, "");
	      m_lattice[index].merge (0);
	    }
	}
      else if (gassign *assign = dyn_cast <gassign *> (use_stmt))
	{
	  enum tree_code code = gimple_assign_rhs_code (assign);

	  /* See if operation is a merge as considered by
	     tree-ssa-structalias.cc:find_func_aliases.  */
	  if (!truth_value_p (code)
	      && code != POINTER_DIFF_EXPR
	      && (code != POINTER_PLUS_EXPR
		  || gimple_assign_rhs1 (assign) == name))
	    {
	      tree lhs = gimple_assign_lhs (assign);
	      merge_with_ssa_name (name, lhs, false);
	    }
	}
      else if (gphi *phi = dyn_cast <gphi *> (use_stmt))
	{
	  tree result = gimple_phi_result (phi);
	  merge_with_ssa_name (name, result, false);
	}
      /* Conditions are not considered escape points
	 by tree-ssa-structalias.  */
      else if (gimple_code (use_stmt) == GIMPLE_COND)
	;
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "%*s  Unhandled stmt\n", m_depth * 4, "");
	  m_lattice[index].merge (0);
	}

      if (dump_file)
	{
	  fprintf (dump_file, "%*s  current flags of ", m_depth * 4, "");
	  print_generic_expr (dump_file, name);
	  m_lattice[index].dump (dump_file, m_depth * 4 + 4);
	}
    }
  if (dump_file)
    {
      fprintf (dump_file, "%*sflags of ssa name ", m_depth * 4, "");
      print_generic_expr (dump_file, name);
      m_lattice[index].dump (dump_file, m_depth * 4 + 2);
    }
  m_lattice[index].open = false;
  if (!m_lattice[index].do_dataflow)
    m_lattice[index].known = true;
}

/* Propagate info from SRC to DEST.  If DEREF it true, assume that SRC
   is dereferenced.  */

void
modref_eaf_analysis::merge_with_ssa_name (tree dest, tree src, bool deref)
{
  int index = SSA_NAME_VERSION (dest);
  int src_index = SSA_NAME_VERSION (src);

  /* Merging lattice with itself is a no-op.  */
  if (!deref && src == dest)
    return;

  m_depth++;
  analyze_ssa_name (src);
  m_depth--;
  if (deref)
    m_lattice[index].merge_deref (m_lattice[src_index], false);
  else
    m_lattice[index].merge (m_lattice[src_index]);

  /* If we failed to produce final solution add an edge to the dataflow
     graph.  */
  if (!m_lattice[src_index].known)
    {
      modref_lattice::propagate_edge e = {index, deref};

      if (!m_lattice[src_index].propagate_to.length ())
	m_names_to_propagate.safe_push (src_index);
      m_lattice[src_index].propagate_to.safe_push (e);
      m_lattice[src_index].changed = true;
      m_lattice[src_index].do_dataflow = true;
      if (dump_file)
	fprintf (dump_file,
		 "%*sWill propgate from ssa_name %i to %i%s\n",
		 m_depth * 4 + 4,
		 "", src_index, index, deref ? " (deref)" : "");
    }
}

/* In the case we deferred some SSA names, reprocess them.  In the case some
   dataflow edges were introduced, do the actual iterative dataflow.  */

void
modref_eaf_analysis::propagate ()
{
  int iterations = 0;
  size_t i;
  int index;
  bool changed = true;

  while (m_deferred_names.length ())
    {
      tree name = m_deferred_names.pop ();
      if (dump_file)
	fprintf (dump_file, "Analyzing deferred SSA name\n");
      analyze_ssa_name (name, true);
    }

  if (!m_names_to_propagate.length ())
    return;
  if (dump_file)
    fprintf (dump_file, "Propagating EAF flags\n");

  /* Compute reverse postorder.  */
  auto_vec <int> rpo;
  struct stack_entry
  {
    int name;
    unsigned pos;
  };
  auto_vec <struct stack_entry> stack;
  int pos = m_names_to_propagate.length () - 1;

  rpo.safe_grow (m_names_to_propagate.length (), true);
  stack.reserve_exact (m_names_to_propagate.length ());

  /* We reuse known flag for RPO DFS walk bookkeeping.  */
  if (flag_checking)
    FOR_EACH_VEC_ELT (m_names_to_propagate, i, index)
      gcc_assert (!m_lattice[index].known && m_lattice[index].changed);

  FOR_EACH_VEC_ELT (m_names_to_propagate, i, index)
    {
      if (!m_lattice[index].known)
	{
	  stack_entry e = {index, 0};

	  stack.quick_push (e);
	  m_lattice[index].known = true;
	}
      while (stack.length ())
	{
	  bool found = false;
	  int index1 = stack.last ().name;

	  while (stack.last ().pos < m_lattice[index1].propagate_to.length ())
	    {
	      int index2 = m_lattice[index1]
		      .propagate_to[stack.last ().pos].ssa_name;

	      stack.last ().pos++;
	      if (!m_lattice[index2].known
		  && m_lattice[index2].propagate_to.length ())
		{
		  stack_entry e = {index2, 0};

		  stack.quick_push (e);
		  m_lattice[index2].known = true;
		  found = true;
		  break;
		}
	    }
	  if (!found
	      && stack.last ().pos == m_lattice[index1].propagate_to.length ())
	    {
	      rpo[pos--] = index1;
	      stack.pop ();
	    }
	}
    }

  /* Perform iterative dataflow.  */
  while (changed)
    {
      changed = false;
      iterations++;
      if (dump_file)
	fprintf (dump_file, " iteration %i\n", iterations);
      FOR_EACH_VEC_ELT (rpo, i, index)
	{
	  if (m_lattice[index].changed)
	    {
	      size_t j;

	      m_lattice[index].changed = false;
	      if (dump_file)
		fprintf (dump_file, "  Visiting ssa name %i\n", index);
	      for (j = 0; j < m_lattice[index].propagate_to.length (); j++)
		{
		  bool ch;
		  int target = m_lattice[index].propagate_to[j].ssa_name;
		  bool deref = m_lattice[index].propagate_to[j].deref;

		  if (dump_file)
		    fprintf (dump_file, "   Propagating flags of ssa name"
			     " %i to %i%s\n",
			     index, target, deref ? " (deref)" : "");
		  m_lattice[target].known = true;
		  if (!m_lattice[index].propagate_to[j].deref)
		    ch = m_lattice[target].merge (m_lattice[index]);
		  else
		    ch = m_lattice[target].merge_deref (m_lattice[index],
							false);
		  if (!ch)
		    continue;
		  if (dump_file)
		    {
		      fprintf (dump_file, "   New lattice: ");
		      m_lattice[target].dump (dump_file);
		    }
		  changed = true;
		  m_lattice[target].changed = true;
		}
	    }
	}
    }
  if (dump_file)
    fprintf (dump_file, "EAF flags propagated in %i iterations\n", iterations);
}

/* Record escape points of PARM_INDEX according to LATTICE.  */

void
modref_eaf_analysis::record_escape_points (tree name, int parm_index, int flags)
{
  modref_lattice &lattice = m_lattice[SSA_NAME_VERSION (name)];

  if (lattice.escape_points.length ())
    {
      escape_point *ep;
      unsigned int ip;
      cgraph_node *node = cgraph_node::get (current_function_decl);

      gcc_assert (m_ipa);
      FOR_EACH_VEC_ELT (lattice.escape_points, ip, ep)
	if ((ep->min_flags & flags) != flags)
	  {
	    cgraph_edge *e = node->get_edge (ep->call);
	    struct escape_entry ee = {parm_index, ep->arg,
				      ep->min_flags, ep->direct};

	    escape_summaries->get_create (e)->esc.safe_push (ee);
	  }
    }
}

/* Determine EAF flags for function parameters
   and fill in SUMMARY/SUMMARY_LTO.  If IPA is true work in IPA mode
   where we also collect escape points.
   PAST_FLAGS, PAST_RETSLOT_FLAGS, PAST_STATIC_CHAIN_FLAGS can be
   used to preserve flags from previous (IPA) run for cases where
   late optimizations changed code in a way we can no longer analyze
   it easily.  */

static void
analyze_parms (modref_summary *summary, modref_summary_lto *summary_lto,
	       bool ipa, vec<eaf_flags_t> &past_flags,
	       int past_retslot_flags, int past_static_chain_flags)
{
  unsigned int parm_index = 0;
  unsigned int count = 0;
  int ecf_flags = flags_from_decl_or_type (current_function_decl);
  tree retslot = NULL;
  tree static_chain = NULL;

  /* If there is return slot, look up its SSA name.  */
  if (DECL_RESULT (current_function_decl)
      && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
    retslot = ssa_default_def (cfun, DECL_RESULT (current_function_decl));
  if (cfun->static_chain_decl)
    static_chain = ssa_default_def (cfun, cfun->static_chain_decl);

  for (tree parm = DECL_ARGUMENTS (current_function_decl); parm;
       parm = TREE_CHAIN (parm))
    count++;

  if (!count && !retslot && !static_chain)
    return;

  modref_eaf_analysis eaf_analysis (ipa);

  /* Determine all SSA names we need to know flags for.  */
  for (tree parm = DECL_ARGUMENTS (current_function_decl); parm;
       parm = TREE_CHAIN (parm))
    {
      tree name = ssa_default_def (cfun, parm);
      if (name)
	eaf_analysis.analyze_ssa_name (name);
    }
  if (retslot)
    eaf_analysis.analyze_ssa_name (retslot);
  if (static_chain)
    eaf_analysis.analyze_ssa_name (static_chain);

  /* Do the dataflow.  */
  eaf_analysis.propagate ();

  tree attr = lookup_attribute ("fn spec",
				TYPE_ATTRIBUTES
				  (TREE_TYPE (current_function_decl)));
  attr_fnspec fnspec (attr
		      ? TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr)))
		      : "");


  /* Store results to summaries.  */
  for (tree parm = DECL_ARGUMENTS (current_function_decl); parm; parm_index++,
       parm = TREE_CHAIN (parm))
    {
      tree name = ssa_default_def (cfun, parm);
      if (!name || has_zero_uses (name))
	{
	  /* We do not track non-SSA parameters,
	     but we want to track unused gimple_regs.  */
	  if (!is_gimple_reg (parm))
	    continue;
	  if (summary)
	    {
	      if (parm_index >= summary->arg_flags.length ())
		summary->arg_flags.safe_grow_cleared (count, true);
	      summary->arg_flags[parm_index] = EAF_UNUSED;
	    }
	  if (summary_lto)
	    {
	      if (parm_index >= summary_lto->arg_flags.length ())
		summary_lto->arg_flags.safe_grow_cleared (count, true);
	      summary_lto->arg_flags[parm_index] = EAF_UNUSED;
	    }
	  continue;
	}
      int flags = eaf_analysis.get_ssa_name_flags (name);
      int attr_flags = fnspec.arg_eaf_flags (parm_index);

      if (dump_file && (flags | attr_flags) != flags && !(flags & EAF_UNUSED))
	{
	  fprintf (dump_file,
		   "  Flags for param %i combined with fnspec flags:",
		   (int)parm_index);
	  dump_eaf_flags (dump_file, attr_flags, false);
	  fprintf (dump_file, " determined: ");
	  dump_eaf_flags (dump_file, flags, true);
	}
      flags |= attr_flags;

      /* Eliminate useless flags so we do not end up storing unnecessary
	 summaries.  */

      flags = remove_useless_eaf_flags
		 (flags, ecf_flags,
		  VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
      if (past_flags.length () > parm_index)
	{
	  int past = past_flags[parm_index];
	  past = remove_useless_eaf_flags
		     (past, ecf_flags,
		      VOID_TYPE_P (TREE_TYPE
			  (TREE_TYPE (current_function_decl))));
	  /* Store merging can produce reads when combining together multiple
	     bitfields.  See PR111613.  */
	  past &= ~(EAF_NO_DIRECT_READ | EAF_NO_INDIRECT_READ);
	  if (dump_file && (flags | past) != flags && !(flags & EAF_UNUSED))
	    {
	      fprintf (dump_file,
		       "  Flags for param %i combined with IPA pass:",
		       (int)parm_index);
	      dump_eaf_flags (dump_file, past, false);
	      fprintf (dump_file, " determined: ");
	      dump_eaf_flags (dump_file, flags, true);
	    }
	  if (!(flags & EAF_UNUSED))
	    flags |= past;
	}

      if (flags)
	{
	  if (summary)
	    {
	      if (parm_index >= summary->arg_flags.length ())
		summary->arg_flags.safe_grow_cleared (count, true);
	      summary->arg_flags[parm_index] = flags;
	    }
	  if (summary_lto)
	    {
	      if (parm_index >= summary_lto->arg_flags.length ())
		summary_lto->arg_flags.safe_grow_cleared (count, true);
	      summary_lto->arg_flags[parm_index] = flags;
	    }
	  eaf_analysis.record_escape_points (name, parm_index, flags);
	}
    }
  if (retslot)
    {
      int flags = eaf_analysis.get_ssa_name_flags (retslot);
      int past = past_retslot_flags;

      flags = remove_useless_eaf_flags (flags, ecf_flags, false);
      past = remove_useless_eaf_flags
		 (past, ecf_flags,
		  VOID_TYPE_P (TREE_TYPE
		      (TREE_TYPE (current_function_decl))));
      if (dump_file && (flags | past) != flags && !(flags & EAF_UNUSED))
	{
	  fprintf (dump_file,
		   "  Retslot flags combined with IPA pass:");
	  dump_eaf_flags (dump_file, past, false);
	  fprintf (dump_file, " determined: ");
	  dump_eaf_flags (dump_file, flags, true);
	}
      if (!(flags & EAF_UNUSED))
	flags |= past;
      if (flags)
	{
	  if (summary)
	    summary->retslot_flags = flags;
	  if (summary_lto)
	    summary_lto->retslot_flags = flags;
	  eaf_analysis.record_escape_points (retslot,
					     MODREF_RETSLOT_PARM, flags);
	}
    }
  if (static_chain)
    {
      int flags = eaf_analysis.get_ssa_name_flags (static_chain);
      int past = past_static_chain_flags;

      flags = remove_useless_eaf_flags (flags, ecf_flags, false);
      past = remove_useless_eaf_flags
		 (past, ecf_flags,
		  VOID_TYPE_P (TREE_TYPE
		      (TREE_TYPE (current_function_decl))));
      if (dump_file && (flags | past) != flags && !(flags & EAF_UNUSED))
	{
	  fprintf (dump_file,
		   "  Static chain flags combined with IPA pass:");
	  dump_eaf_flags (dump_file, past, false);
	  fprintf (dump_file, " determined: ");
	  dump_eaf_flags (dump_file, flags, true);
	}
      if (!(flags & EAF_UNUSED))
	flags |= past;
      if (flags)
	{
	  if (summary)
	    summary->static_chain_flags = flags;
	  if (summary_lto)
	    summary_lto->static_chain_flags = flags;
	  eaf_analysis.record_escape_points (static_chain,
					     MODREF_STATIC_CHAIN_PARM,
					     flags);
	}
    }
}

/* Analyze function.  IPA indicates whether we're running in local mode
   (false) or the IPA mode (true).
   Return true if fixup cfg is needed after the pass.  */

static bool
analyze_function (bool ipa)
{
  bool fixup_cfg = false;
  if (dump_file)
    fprintf (dump_file, "\n\nmodref analyzing '%s' (ipa=%i)%s%s\n",
	     cgraph_node::get (current_function_decl)->dump_name (), ipa,
	     TREE_READONLY (current_function_decl) ? " (const)" : "",
	     DECL_PURE_P (current_function_decl) ? " (pure)" : "");

  /* Don't analyze this function if it's compiled with -fno-strict-aliasing.  */
  if (!flag_ipa_modref
      || lookup_attribute ("noipa", DECL_ATTRIBUTES (current_function_decl)))
    return false;

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

  bool past_flags_known = false;
  auto_vec <eaf_flags_t> past_flags;
  int past_retslot_flags = 0;
  int past_static_chain_flags = 0;

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
	  summary = optimization_summaries->get (fnode);
	  if (summary != NULL
	      && summary->loads)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Past summary:\n");
		  optimization_summaries->get (fnode)->dump (dump_file);
		}
	      past_flags.reserve_exact (summary->arg_flags.length ());
	      past_flags.splice (summary->arg_flags);
	      past_retslot_flags = summary->retslot_flags;
	      past_static_chain_flags = summary->static_chain_flags;
	      past_flags_known = true;
	    }
	  optimization_summaries->remove (fnode);
	}
      summary = optimization_summaries->get_create (fnode);
      gcc_checking_assert (nolto && !lto);
    }
  /* In IPA mode we analyze every function precisely once.  Assert that.  */
  else
    {
      if (nolto)
	{
	  if (!summaries)
	    summaries = modref_summaries::create_ggc (symtab);
	  else
	    summaries->remove (fnode);
	  summary = summaries->get_create (fnode);
	}
      if (lto)
	{
	  if (!summaries_lto)
	    summaries_lto = modref_summaries_lto::create_ggc (symtab);
	  else
	    summaries_lto->remove (fnode);
	  summary_lto = summaries_lto->get_create (fnode);
	}
      if (!fnspec_summaries)
	fnspec_summaries = new fnspec_summaries_t (symtab);
      if (!escape_summaries)
	escape_summaries = new escape_summaries_t (symtab);
     }


  /* Create and initialize summary for F.
     Note that summaries may be already allocated from previous
     run of the pass.  */
  if (nolto)
    {
      gcc_assert (!summary->loads);
      summary->loads = modref_records::create_ggc ();
      gcc_assert (!summary->stores);
      summary->stores = modref_records::create_ggc ();
      summary->writes_errno = false;
      summary->side_effects = false;
      summary->nondeterministic = false;
      summary->calls_interposable = false;
    }
  if (lto)
    {
      gcc_assert (!summary_lto->loads);
      summary_lto->loads = modref_records_lto::create_ggc ();
      gcc_assert (!summary_lto->stores);
      summary_lto->stores = modref_records_lto::create_ggc ();
      summary_lto->writes_errno = false;
      summary_lto->side_effects = false;
      summary_lto->nondeterministic = false;
      summary_lto->calls_interposable = false;
    }

  analyze_parms (summary, summary_lto, ipa,
		 past_flags, past_retslot_flags, past_static_chain_flags);

  {
    modref_access_analysis analyzer (ipa, summary, summary_lto);
    analyzer.analyze ();
  }

  if (!ipa && flag_ipa_pure_const)
    {
      if (!summary->stores->every_base && !summary->stores->bases
	  && !summary->nondeterministic)
	{
	  if (!summary->loads->every_base && !summary->loads->bases
	      && !summary->calls_interposable)
	    fixup_cfg = ipa_make_function_const (fnode,
						 summary->side_effects, true);
	  else
	    fixup_cfg = ipa_make_function_pure (fnode,
						summary->side_effects, true);
	}
    }
  int ecf_flags = flags_from_decl_or_type (current_function_decl);
  if (summary && !summary->useful_p (ecf_flags))
    {
      if (!ipa)
	optimization_summaries->remove (fnode);
      else
	summaries->remove (fnode);
      summary = NULL;
    }
  if (summary)
    summary->finalize (current_function_decl);
  if (summary_lto && !summary_lto->useful_p (ecf_flags))
    {
      summaries_lto->remove (fnode);
      summary_lto = NULL;
    }

  if (ipa && !summary && !summary_lto)
    remove_modref_edge_summaries (fnode);

  if (dump_file)
    {
      fprintf (dump_file, " - modref done with result: tracked.\n");
      if (summary)
	summary->dump (dump_file);
      if (summary_lto)
	summary_lto->dump (dump_file);
      dump_modref_edge_summaries (dump_file, fnode, 2);
      /* To simplify debugging, compare IPA and local solutions.  */
      if (past_flags_known && summary)
	{
	  size_t len = summary->arg_flags.length ();

	  if (past_flags.length () > len)
	    len = past_flags.length ();
	  for (size_t i = 0; i < len; i++)
	    {
	      int old_flags = i < past_flags.length () ? past_flags[i] : 0;
	      int new_flags = i < summary->arg_flags.length ()
			      ? summary->arg_flags[i] : 0;
	      old_flags = remove_useless_eaf_flags
		(old_flags, flags_from_decl_or_type (current_function_decl),
		 VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
	      if (old_flags != new_flags)
		{
		  if ((old_flags & ~new_flags) == 0
		      || (new_flags & EAF_UNUSED))
		    fprintf (dump_file, "  Flags for param %i improved:",
			     (int)i);
		  else
		    fprintf (dump_file, "  Flags for param %i changed:",
			     (int)i);
		  dump_eaf_flags (dump_file, old_flags, false);
		  fprintf (dump_file, " -> ");
		  dump_eaf_flags (dump_file, new_flags, true);
		}
	    }
	  past_retslot_flags = remove_useless_eaf_flags
		(past_retslot_flags,
		 flags_from_decl_or_type (current_function_decl),
		 VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
	  if (past_retslot_flags != summary->retslot_flags)
	    {
	      if ((past_retslot_flags & ~summary->retslot_flags) == 0
		  || (summary->retslot_flags & EAF_UNUSED))
		fprintf (dump_file, "  Flags for retslot improved:");
	      else
		fprintf (dump_file, "  Flags for retslot changed:");
	      dump_eaf_flags (dump_file, past_retslot_flags, false);
	      fprintf (dump_file, " -> ");
	      dump_eaf_flags (dump_file, summary->retslot_flags, true);
	    }
	  past_static_chain_flags = remove_useless_eaf_flags
		(past_static_chain_flags,
		 flags_from_decl_or_type (current_function_decl),
		 VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
	  if (past_static_chain_flags != summary->static_chain_flags)
	    {
	      if ((past_static_chain_flags & ~summary->static_chain_flags) == 0
		  || (summary->static_chain_flags & EAF_UNUSED))
		fprintf (dump_file, "  Flags for static chain improved:");
	      else
		fprintf (dump_file, "  Flags for static chain changed:");
	      dump_eaf_flags (dump_file, past_static_chain_flags, false);
	      fprintf (dump_file, " -> ");
	      dump_eaf_flags (dump_file, summary->static_chain_flags, true);
	    }
	}
      else if (past_flags_known && !summary)
	{
	  for (size_t i = 0; i < past_flags.length (); i++)
	    {
	      int old_flags = past_flags[i];
	      old_flags = remove_useless_eaf_flags
		(old_flags, flags_from_decl_or_type (current_function_decl),
		 VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
	      if (old_flags)
		{
		  fprintf (dump_file, "  Flags for param %i worsened:",
			   (int)i);
		  dump_eaf_flags (dump_file, old_flags, false);
		  fprintf (dump_file, " -> \n");
		}
	    }
	  past_retslot_flags = remove_useless_eaf_flags
		(past_retslot_flags,
		 flags_from_decl_or_type (current_function_decl),
		 VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
	  if (past_retslot_flags)
	    {
	      fprintf (dump_file, "  Flags for retslot worsened:");
	      dump_eaf_flags (dump_file, past_retslot_flags, false);
	      fprintf (dump_file, " ->\n");
	    }
	  past_static_chain_flags = remove_useless_eaf_flags
		(past_static_chain_flags,
		 flags_from_decl_or_type (current_function_decl),
		 VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));
	  if (past_static_chain_flags)
	    {
	      fprintf (dump_file, "  Flags for static chain worsened:");
	      dump_eaf_flags (dump_file, past_static_chain_flags, false);
	      fprintf (dump_file, " ->\n");
	    }
	}
    }
  return fixup_cfg;
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
      analyze_function (true);
      pop_cfun ();
    }
}

}  /* ANON namespace.  */

/* Debugging helper.  */

void
debug_eaf_flags (int flags)
{
   dump_eaf_flags (stderr, flags, true);
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
  if (!DECL_STRUCT_FUNCTION (node->decl)
      || !opt_for_fn (node->decl, flag_ipa_modref))
    {
      summaries->remove (node);
      return;
    }
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  analyze_function (true);
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
      || !opt_for_fn (node->decl, flag_ipa_modref)
      || propagated)
    {
      summaries_lto->remove (node);
      return;
    }
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  analyze_function (true);
  pop_cfun ();
}

/* Called when new clone is inserted to callgraph late.  */

void
modref_summaries::duplicate (cgraph_node *, cgraph_node *dst,
			     modref_summary *src_data,
			     modref_summary *dst_data)
{
  /* Do not duplicate optimization summaries; we do not handle parameter
     transforms on them.  */
  if (this == optimization_summaries)
    {
      optimization_summaries->remove (dst);
      return;
    }
  dst_data->stores = modref_records::create_ggc ();
  dst_data->stores->copy_from (src_data->stores);
  dst_data->loads = modref_records::create_ggc ();
  dst_data->loads->copy_from (src_data->loads);
  dst_data->kills.reserve_exact (src_data->kills.length ());
  dst_data->kills.splice (src_data->kills);
  dst_data->writes_errno = src_data->writes_errno;
  dst_data->side_effects = src_data->side_effects;
  dst_data->nondeterministic = src_data->nondeterministic;
  dst_data->calls_interposable = src_data->calls_interposable;
  if (src_data->arg_flags.length ())
    dst_data->arg_flags = src_data->arg_flags.copy ();
  dst_data->retslot_flags = src_data->retslot_flags;
  dst_data->static_chain_flags = src_data->static_chain_flags;
}

/* Called when new clone is inserted to callgraph late.  */

void
modref_summaries_lto::duplicate (cgraph_node *, cgraph_node *,
				 modref_summary_lto *src_data,
				 modref_summary_lto *dst_data)
{
  /* Be sure that no further cloning happens after ipa-modref.  If it does
     we will need to update signatures for possible param changes.  */
  gcc_checking_assert (!((modref_summaries_lto *)summaries_lto)->propagated);
  dst_data->stores = modref_records_lto::create_ggc ();
  dst_data->stores->copy_from (src_data->stores);
  dst_data->loads = modref_records_lto::create_ggc ();
  dst_data->loads->copy_from (src_data->loads);
  dst_data->kills.reserve_exact (src_data->kills.length ());
  dst_data->kills.splice (src_data->kills);
  dst_data->writes_errno = src_data->writes_errno;
  dst_data->side_effects = src_data->side_effects;
  dst_data->nondeterministic = src_data->nondeterministic;
  dst_data->calls_interposable = src_data->calls_interposable;
  if (src_data->arg_flags.length ())
    dst_data->arg_flags = src_data->arg_flags.copy ();
  dst_data->retslot_flags = src_data->retslot_flags;
  dst_data->static_chain_flags = src_data->static_chain_flags;
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
    opt_pass *clone () final override
    {
      return new pass_modref (m_ctxt);
    }
    bool gate (function *) final override
    {
      return flag_ipa_modref;
    }
    unsigned int execute (function *) final override;
};

/* Encode TT to the output block OB using the summary streaming API.  */

static void
write_modref_records (modref_records_lto *tt, struct output_block *ob)
{
  streamer_write_uhwi (ob, tt->every_base);
  streamer_write_uhwi (ob, vec_safe_length (tt->bases));
  for (auto base_node : tt->bases)
    {
      stream_write_tree (ob, base_node->base, true);

      streamer_write_uhwi (ob, base_node->every_ref);
      streamer_write_uhwi (ob, vec_safe_length (base_node->refs));

      for (auto ref_node : base_node->refs)
	{
	  stream_write_tree (ob, ref_node->ref, true);
	  streamer_write_uhwi (ob, ref_node->every_access);
	  streamer_write_uhwi (ob, vec_safe_length (ref_node->accesses));

	  for (auto access_node : ref_node->accesses)
	    access_node.stream_out (ob);
	}
    }
}

/* Read a modref_tree from the input block IB using the data from DATA_IN.
   This assumes that the tree was encoded using write_modref_tree.
   Either nolto_ret or lto_ret is initialized by the tree depending whether
   LTO streaming is expected or not.  */

static void
read_modref_records (tree decl,
		     lto_input_block *ib, struct data_in *data_in,
		     modref_records **nolto_ret,
		     modref_records_lto **lto_ret)
{
  size_t max_bases = opt_for_fn (decl, param_modref_max_bases);
  size_t max_refs = opt_for_fn (decl, param_modref_max_refs);
  size_t max_accesses = opt_for_fn (decl, param_modref_max_accesses);

  if (lto_ret)
    *lto_ret = modref_records_lto::create_ggc ();
  if (nolto_ret)
    *nolto_ret = modref_records::create_ggc ();
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
						     : 0, 0, INT_MAX);
      if (lto_ret)
	lto_base_node = (*lto_ret)->insert_base (base_tree, 0, max_bases);
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

	  if (nolto_ref_node && every_access)
	    nolto_ref_node->collapse ();
	  if (lto_ref_node && every_access)
	    lto_ref_node->collapse ();

	  for (size_t k = 0; k < naccesses; k++)
	    {
	      modref_access_node a = modref_access_node::stream_in (ib);
	      if (nolto_ref_node)
		nolto_ref_node->insert_access (a, max_accesses, false);
	      if (lto_ref_node)
		lto_ref_node->insert_access (a, max_accesses, false);
	    }
	}
    }
  if (lto_ret)
    (*lto_ret)->cleanup ();
  if (nolto_ret)
    (*nolto_ret)->cleanup ();
}

/* Write ESUM to BP.  */

static void
modref_write_escape_summary (struct bitpack_d *bp, escape_summary *esum)
{
  if (!esum)
    {
      bp_pack_var_len_unsigned (bp, 0);
      return;
    }
  bp_pack_var_len_unsigned (bp, esum->esc.length ());
  unsigned int i;
  escape_entry *ee;
  FOR_EACH_VEC_ELT (esum->esc, i, ee)
    {
      bp_pack_var_len_int (bp, ee->parm_index);
      bp_pack_var_len_unsigned (bp, ee->arg);
      bp_pack_var_len_unsigned (bp, ee->min_flags);
      bp_pack_value (bp, ee->direct, 1);
    }
}

/* Read escape summary for E from BP.  */

static void
modref_read_escape_summary (struct bitpack_d *bp, cgraph_edge *e)
{
  unsigned int n = bp_unpack_var_len_unsigned (bp);
  if (!n)
    return;
  escape_summary *esum = escape_summaries->get_create (e);
  esum->esc.reserve_exact (n);
  for (unsigned int i = 0; i < n; i++)
    {
      escape_entry ee;
      ee.parm_index = bp_unpack_var_len_int (bp);
      ee.arg = bp_unpack_var_len_unsigned (bp);
      ee.min_flags = bp_unpack_var_len_unsigned (bp);
      ee.direct = bp_unpack_value (bp, 1);
      esum->esc.quick_push (ee);
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

	  streamer_write_uhwi (ob, r->arg_flags.length ());
	  for (unsigned int i = 0; i < r->arg_flags.length (); i++)
	    streamer_write_uhwi (ob, r->arg_flags[i]);
	  streamer_write_uhwi (ob, r->retslot_flags);
	  streamer_write_uhwi (ob, r->static_chain_flags);

	  write_modref_records (r->loads, ob);
	  write_modref_records (r->stores, ob);
	  streamer_write_uhwi (ob, r->kills.length ());
	  for (auto kill : r->kills)
	    kill.stream_out (ob);

	  struct bitpack_d bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, r->writes_errno, 1);
	  bp_pack_value (&bp, r->side_effects, 1);
	  bp_pack_value (&bp, r->nondeterministic, 1);
	  bp_pack_value (&bp, r->calls_interposable, 1);
	  if (!flag_wpa)
	    {
	      for (cgraph_edge *e = cnode->indirect_calls;
		   e; e = e->next_callee)
		{
		  class fnspec_summary *sum = fnspec_summaries->get (e);
		  bp_pack_value (&bp, sum != NULL, 1);
		  if (sum)
		    bp_pack_string (ob, &bp, sum->fnspec, true);
		  class escape_summary *esum = escape_summaries->get (e);
		  modref_write_escape_summary (&bp,esum);
		}
	      for (cgraph_edge *e = cnode->callees; e; e = e->next_callee)
		{
		  class fnspec_summary *sum = fnspec_summaries->get (e);
		  bp_pack_value (&bp, sum != NULL, 1);
		  if (sum)
		    bp_pack_string (ob, &bp, sum->fnspec, true);
		  class escape_summary *esum = escape_summaries->get (e);
		  modref_write_escape_summary (&bp,esum);
		}
	    }
	  streamer_write_bitpack (&bp);
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
		      file_data);

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

      if (modref_sum)
	{
	  modref_sum->writes_errno = false;
	  modref_sum->side_effects = false;
	  modref_sum->nondeterministic = false;
	  modref_sum->calls_interposable = false;
	}
      if (modref_sum_lto)
	{
	  modref_sum_lto->writes_errno = false;
	  modref_sum_lto->side_effects = false;
	  modref_sum_lto->nondeterministic = false;
	  modref_sum_lto->calls_interposable = false;
	}

      gcc_assert (!modref_sum || (!modref_sum->loads
				  && !modref_sum->stores));
      gcc_assert (!modref_sum_lto || (!modref_sum_lto->loads
				      && !modref_sum_lto->stores));
      unsigned int args = streamer_read_uhwi (&ib);
      if (args && modref_sum)
	modref_sum->arg_flags.reserve_exact (args);
      if (args && modref_sum_lto)
	modref_sum_lto->arg_flags.reserve_exact (args);
      for (unsigned int i = 0; i < args; i++)
	{
	  eaf_flags_t flags = streamer_read_uhwi (&ib);
	  if (modref_sum)
	    modref_sum->arg_flags.quick_push (flags);
	  if (modref_sum_lto)
	    modref_sum_lto->arg_flags.quick_push (flags);
	}
      eaf_flags_t flags = streamer_read_uhwi (&ib);
      if (modref_sum)
	modref_sum->retslot_flags = flags;
      if (modref_sum_lto)
	modref_sum_lto->retslot_flags = flags;

      flags = streamer_read_uhwi (&ib);
      if (modref_sum)
	modref_sum->static_chain_flags = flags;
      if (modref_sum_lto)
	modref_sum_lto->static_chain_flags = flags;

      read_modref_records (node->decl, &ib, data_in,
			   modref_sum ? &modref_sum->loads : NULL,
			   modref_sum_lto ? &modref_sum_lto->loads : NULL);
      read_modref_records (node->decl, &ib, data_in,
			   modref_sum ? &modref_sum->stores : NULL,
			   modref_sum_lto ? &modref_sum_lto->stores : NULL);
      int j = streamer_read_uhwi (&ib);
      if (j && modref_sum)
	modref_sum->kills.reserve_exact (j);
      if (j && modref_sum_lto)
	modref_sum_lto->kills.reserve_exact (j);
      for (int k = 0; k < j; k++)
	{
	  modref_access_node a = modref_access_node::stream_in (&ib);

	  if (modref_sum)
	    modref_sum->kills.quick_push (a);
	  if (modref_sum_lto)
	    modref_sum_lto->kills.quick_push (a);
	}
      struct bitpack_d bp = streamer_read_bitpack (&ib);
      if (bp_unpack_value (&bp, 1))
	{
	  if (modref_sum)
	    modref_sum->writes_errno = true;
	  if (modref_sum_lto)
	    modref_sum_lto->writes_errno = true;
	}
      if (bp_unpack_value (&bp, 1))
	{
	  if (modref_sum)
	    modref_sum->side_effects = true;
	  if (modref_sum_lto)
	    modref_sum_lto->side_effects = true;
	}
      if (bp_unpack_value (&bp, 1))
	{
	  if (modref_sum)
	    modref_sum->nondeterministic = true;
	  if (modref_sum_lto)
	    modref_sum_lto->nondeterministic = true;
	}
      if (bp_unpack_value (&bp, 1))
	{
	  if (modref_sum)
	    modref_sum->calls_interposable = true;
	  if (modref_sum_lto)
	    modref_sum_lto->calls_interposable = true;
	}
      if (!flag_ltrans)
	{
	  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
	    {
	      if (bp_unpack_value (&bp, 1))
		{
		  class fnspec_summary *sum = fnspec_summaries->get_create (e);
		  sum->fnspec = xstrdup (bp_unpack_string (data_in, &bp));
		}
	      modref_read_escape_summary (&bp, e);
	    }
	  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
	    {
	      if (bp_unpack_value (&bp, 1))
		{
		  class fnspec_summary *sum = fnspec_summaries->get_create (e);
		  sum->fnspec = xstrdup (bp_unpack_string (data_in, &bp));
		}
	      modref_read_escape_summary (&bp, e);
	    }
	}
      if (flag_ltrans)
	modref_sum->finalize (node->decl);
      if (dump_file)
	{
	  fprintf (dump_file, "Read modref for %s\n",
		   node->dump_name ());
	  if (modref_sum)
	    modref_sum->dump (dump_file);
	  if (modref_sum_lto)
	    modref_sum_lto->dump (dump_file);
	  dump_modref_edge_summaries (dump_file, node, 4);
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
      if (!fnspec_summaries)
	fnspec_summaries = new fnspec_summaries_t (symtab);
      if (!escape_summaries)
	escape_summaries = new escape_summaries_t (symtab);
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

/* Recompute arg_flags for param adjustments in INFO.  */

static void
remap_arg_flags (auto_vec <eaf_flags_t> &arg_flags, clone_info *info)
{
  auto_vec<eaf_flags_t> old = arg_flags.copy ();
  int max = -1;
  size_t i;
  ipa_adjusted_param *p;

  arg_flags.release ();

  FOR_EACH_VEC_SAFE_ELT (info->param_adjustments->m_adj_params, i, p)
    {
      int o = info->param_adjustments->get_original_index (i);
      if (o >= 0 && (int)old.length () > o && old[o])
	max = i;
    }
  if (max >= 0)
    arg_flags.safe_grow_cleared (max + 1, true);
  FOR_EACH_VEC_SAFE_ELT (info->param_adjustments->m_adj_params, i, p)
    {
      int o = info->param_adjustments->get_original_index (i);
      if (o >= 0 && (int)old.length () > o && old[o])
	arg_flags[i] = old[o];
    }
}

/* Update kills according to the parm map MAP.  */

static void
remap_kills (vec <modref_access_node> &kills, const vec <int> &map)
{
  for (size_t i = 0; i < kills.length ();)
    if (kills[i].parm_index >= 0)
      {
	if (kills[i].parm_index < (int)map.length ()
	    && map[kills[i].parm_index] != MODREF_UNKNOWN_PARM)
	  {
	    kills[i].parm_index = map[kills[i].parm_index];
	    i++;
	  }
	else
	  kills.unordered_remove (i);
      }
    else
      i++;
}

/* Return true if the V can overlap with KILL.  */

static bool
ipcp_argagg_and_kill_overlap_p (const ipa_argagg_value &v,
				const modref_access_node &kill)
{
  if (kill.parm_index == v.index)
    {
      gcc_assert (kill.parm_offset_known);
      gcc_assert (known_eq (kill.max_size, kill.size));
      poly_int64 repl_size;
      bool ok = poly_int_tree_p (TYPE_SIZE (TREE_TYPE (v.value)),
				 &repl_size);
      gcc_assert (ok);
      poly_int64 repl_offset (v.unit_offset);
      repl_offset <<= LOG2_BITS_PER_UNIT;
      poly_int64 combined_offset
	= (kill.parm_offset << LOG2_BITS_PER_UNIT) + kill.offset;
      if (ranges_maybe_overlap_p (repl_offset, repl_size,
				  combined_offset, kill.size))
	return true;
    }
  return false;
}

/* If signature changed, update the summary.  */

static void
update_signature (struct cgraph_node *node)
{
  modref_summary *r = optimization_summaries
		      ? optimization_summaries->get (node) : NULL;
  modref_summary_lto *r_lto = summaries_lto
			      ? summaries_lto->get (node) : NULL;
  if (!r && !r_lto)
    return;

  /* Propagating constants in killed memory can lead to eliminated stores in
     both callees (because they are considered redundant) and callers, leading
     to missing them altogether.  */
  ipcp_transformation *ipcp_ts = ipcp_get_transformation_summary (node);
  if (ipcp_ts)
    {
    for (auto &v : ipcp_ts->m_agg_values)
      {
	if (!v.by_ref)
	  continue;
	if (r)
	  for (const modref_access_node &kill : r->kills)
	    if (ipcp_argagg_and_kill_overlap_p (v, kill))
	      {
		v.killed = true;
		break;
	      }
	if (!v.killed && r_lto)
	  for (const modref_access_node &kill : r_lto->kills)
	    if (ipcp_argagg_and_kill_overlap_p (v, kill))
	      {
		v.killed = true;
		break;
	      }
      }
    }

  clone_info *info = clone_info::get (node);
  if (!info || !info->param_adjustments)
    return;

  if (dump_file)
    {
      fprintf (dump_file, "Updating summary for %s from:\n",
	       node->dump_name ());
      if (r)
	r->dump (dump_file);
      if (r_lto)
	r_lto->dump (dump_file);
    }

  size_t i, max = 0;
  ipa_adjusted_param *p;

  FOR_EACH_VEC_SAFE_ELT (info->param_adjustments->m_adj_params, i, p)
    {
      int idx = info->param_adjustments->get_original_index (i);
      if (idx > (int)max)
	max = idx;
    }

  auto_vec <int, 32> map;

  map.reserve (max + 1);
  for (i = 0; i <= max; i++)
    map.quick_push (MODREF_UNKNOWN_PARM);
  FOR_EACH_VEC_SAFE_ELT (info->param_adjustments->m_adj_params, i, p)
    {
      int idx = info->param_adjustments->get_original_index (i);
      if (idx >= 0)
	map[idx] = i;
    }
  if (r)
    {
      r->loads->remap_params (&map);
      r->stores->remap_params (&map);
      remap_kills (r->kills, map);
      if (r->arg_flags.length ())
	remap_arg_flags (r->arg_flags, info);
    }
  if (r_lto)
    {
      r_lto->loads->remap_params (&map);
      r_lto->stores->remap_params (&map);
      remap_kills (r_lto->kills, map);
      if (r_lto->arg_flags.length ())
	remap_arg_flags (r_lto->arg_flags, info);
    }
  if (dump_file)
    {
      fprintf (dump_file, "to:\n");
      if (r)
	r->dump (dump_file);
      if (r_lto)
	r_lto->dump (dump_file);
    }
  if (r)
    r->finalize (node->decl);
  return;
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
		      NULL,	       /* function_transform */
		      NULL)            /* variable_transform */
  {}

  /* opt_pass methods: */
  opt_pass *clone () final override { return new pass_ipa_modref (m_ctxt); }
  bool gate (function *) final override
  {
    return true;
  }
  unsigned int execute (function *) final override;

};

}

unsigned int pass_modref::execute (function *)
{
  if (analyze_function (false))
    return execute_fixup_cfg ();
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

namespace {

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
  cgraph_node *callee = e->callee->ultimate_alias_target
			  (&avail, e->caller);

  return (avail <= AVAIL_INTERPOSABLE
	  || ((!optimization_summaries || !optimization_summaries->get (callee))
	      && (!summaries_lto || !summaries_lto->get (callee))));
}

/* Compute parm_map for CALLEE_EDGE.  */

static bool
compute_parm_map (cgraph_edge *callee_edge, vec<modref_parm_map> *parm_map)
{
  class ipa_edge_args *args;
  if (ipa_node_params_sum
      && !callee_edge->call_stmt_cannot_inline_p
      && (args = ipa_edge_args_sum->get (callee_edge)) != NULL)
    {
      int i, count = ipa_get_cs_argument_count (args);
      class ipa_node_params *caller_parms_info, *callee_pi;
      class ipa_call_summary *es
	     = ipa_call_summaries->get (callee_edge);
      cgraph_node *callee
	 = callee_edge->callee->ultimate_alias_target
			      (NULL, callee_edge->caller);

      caller_parms_info
	= ipa_node_params_sum->get (callee_edge->caller->inlined_to
				    ? callee_edge->caller->inlined_to
				    : callee_edge->caller);
      callee_pi = ipa_node_params_sum->get (callee);

      (*parm_map).safe_grow_cleared (count, true);

      for (i = 0; i < count; i++)
	{
	  if (es && es->param[i].points_to_local_or_readonly_memory)
	    {
	      (*parm_map)[i].parm_index = MODREF_LOCAL_MEMORY_PARM;
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
		  (*parm_map)[i].parm_index = MODREF_LOCAL_MEMORY_PARM;
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
      return true;
    }
  return false;
}

/* Map used to translate escape infos.  */

struct escape_map
{
  int parm_index;
  bool direct;
};

/* Update escape map for E.  */

static void
update_escape_summary_1 (cgraph_edge *e,
			 vec <vec <escape_map>> &map,
			 bool ignore_stores)
{
  escape_summary *sum = escape_summaries->get (e);
  if (!sum)
    return;
  auto_vec <escape_entry> old = sum->esc.copy ();
  sum->esc.release ();

  unsigned int i;
  escape_entry *ee;
  FOR_EACH_VEC_ELT (old, i, ee)
    {
      unsigned int j;
      struct escape_map *em;
      /* TODO: We do not have jump functions for return slots, so we
	 never propagate them to outer function.  */
      if (ee->parm_index >= (int)map.length ()
	  || ee->parm_index < 0)
	continue;
      FOR_EACH_VEC_ELT (map[ee->parm_index], j, em)
	{
	  eaf_flags_t min_flags = ee->min_flags;
	  if (ee->direct && !em->direct)
	    min_flags = deref_flags (min_flags, ignore_stores);
	  struct escape_entry entry = {em->parm_index, ee->arg,
				       min_flags,
				       ee->direct && em->direct};
	  sum->esc.safe_push (entry);
	}
    }
  if (!sum->esc.length ())
    escape_summaries->remove (e);
}

/* Update escape map for NODE.  */

static void
update_escape_summary (cgraph_node *node,
		       vec <vec <escape_map>> &map,
		       bool ignore_stores)
{
  if (!escape_summaries)
    return;
  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
    update_escape_summary_1 (e, map, ignore_stores);
  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	update_escape_summary (e->callee, map, ignore_stores);
      else
	update_escape_summary_1 (e, map, ignore_stores);
    }
}

/* Get parameter type from DECL.  This is only safe for special cases
   like builtins we create fnspec for because the type match is checked
   at fnspec creation time.  */

static tree
get_parm_type (tree decl, unsigned int i)
{
  tree t = TYPE_ARG_TYPES (TREE_TYPE (decl));

  for (unsigned int p = 0; p < i; p++)
    t = TREE_CHAIN (t);
  return TREE_VALUE (t);
}

/* Return access mode for argument I of call E with FNSPEC.  */

static modref_access_node
get_access_for_fnspec (cgraph_edge *e, attr_fnspec &fnspec,
		       unsigned int i, modref_parm_map &map)
{
  tree size = NULL_TREE;
  unsigned int size_arg;

  if (!fnspec.arg_specified_p (i))
    ;
  else if (fnspec.arg_max_access_size_given_by_arg_p (i, &size_arg))
    {
      cgraph_node *node = e->caller->inlined_to
			  ? e->caller->inlined_to : e->caller;
      ipa_node_params *caller_parms_info = ipa_node_params_sum->get (node);
      ipa_edge_args *args = ipa_edge_args_sum->get (e);
      struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, size_arg);

      if (jf)
	size = ipa_value_from_jfunc (caller_parms_info, jf,
				     get_parm_type (e->callee->decl, size_arg));
    }
  else if (fnspec.arg_access_size_given_by_type_p (i))
    size = TYPE_SIZE_UNIT (get_parm_type (e->callee->decl, i));
  modref_access_node a = {0, -1, -1,
			  map.parm_offset, map.parm_index,
			  map.parm_offset_known, 0};
  poly_int64 size_hwi;
  if (size
      && poly_int_tree_p (size, &size_hwi)
      && coeffs_in_range_p (size_hwi, 0,
			    HOST_WIDE_INT_MAX / BITS_PER_UNIT))
    {
      a.size = -1;
      a.max_size = size_hwi << LOG2_BITS_PER_UNIT;
    }
  return a;
}

 /* Collapse loads and return true if something changed.  */
static bool
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

/* Collapse loads and return true if something changed.  */

static bool
collapse_stores (modref_summary *cur_summary,
		modref_summary_lto *cur_summary_lto)
{
  bool changed = false;

  if (cur_summary && !cur_summary->stores->every_base)
    {
      cur_summary->stores->collapse ();
      changed = true;
    }
  if (cur_summary_lto
      && !cur_summary_lto->stores->every_base)
    {
      cur_summary_lto->stores->collapse ();
      changed = true;
    }
  return changed;
}

/* Call E in NODE with ECF_FLAGS has no summary; update MODREF_SUMMARY and
   CUR_SUMMARY_LTO accordingly.  Return true if something changed.  */

static bool
propagate_unknown_call (cgraph_node *node,
			cgraph_edge *e, int ecf_flags,
			modref_summary *cur_summary,
			modref_summary_lto *cur_summary_lto,
			bool nontrivial_scc)
{
  bool changed = false;
  class fnspec_summary *fnspec_sum = fnspec_summaries->get (e);
  auto_vec <modref_parm_map, 32> parm_map;
  bool looping;

  if (e->callee
      && builtin_safe_for_const_function_p (&looping, e->callee->decl))
    {
      if (looping && cur_summary && !cur_summary->side_effects)
	{
	  cur_summary->side_effects = true;
	  changed = true;
	}
      if (looping && cur_summary_lto && !cur_summary_lto->side_effects)
	{
	  cur_summary_lto->side_effects = true;
	  changed = true;
	}
      return changed;
    }

  if (!(ecf_flags & (ECF_CONST | ECF_PURE))
      || (ecf_flags & ECF_LOOPING_CONST_OR_PURE)
      || nontrivial_scc)
    {
      if (cur_summary && !cur_summary->side_effects)
	{
	  cur_summary->side_effects = true;
	  changed = true;
	}
      if (cur_summary_lto && !cur_summary_lto->side_effects)
	{
	  cur_summary_lto->side_effects = true;
	  changed = true;
	}
      if (!ignore_nondeterminism_p (node->decl, ecf_flags,
				    e->callee ? TREE_TYPE (e->callee->decl)
					      : NULL_TREE))
	{
	  if (cur_summary && !cur_summary->nondeterministic)
	    {
	      cur_summary->nondeterministic = true;
	      changed = true;
	    }
	  if (cur_summary_lto && !cur_summary_lto->nondeterministic)
	    {
	      cur_summary_lto->nondeterministic = true;
	      changed = true;
	    }
	}
    }
  if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
    return changed;

  if (fnspec_sum
      && compute_parm_map (e, &parm_map))
    {
      attr_fnspec fnspec (fnspec_sum->fnspec);

      gcc_checking_assert (fnspec.known_p ());
      if (fnspec.global_memory_read_p ())
	collapse_loads (cur_summary, cur_summary_lto);
      else
	{
	  tree t = TYPE_ARG_TYPES (TREE_TYPE (e->callee->decl));
	  for (unsigned i = 0; i < parm_map.length () && t;
	       i++, t = TREE_CHAIN (t))
	    if (!POINTER_TYPE_P (TREE_VALUE (t)))
	      ;
	  else if (!fnspec.arg_specified_p (i)
		   || fnspec.arg_maybe_read_p (i))
	    {
	      modref_parm_map map = parm_map[i];
	      if (map.parm_index == MODREF_LOCAL_MEMORY_PARM)
		continue;
	      if (map.parm_index == MODREF_UNKNOWN_PARM)
		{
		  collapse_loads (cur_summary, cur_summary_lto);
		  break;
		}
	      if (cur_summary)
		changed |= cur_summary->loads->insert
		  (node->decl, 0, 0,
		   get_access_for_fnspec (e, fnspec, i, map), false);
	      if (cur_summary_lto)
		changed |= cur_summary_lto->loads->insert
		  (node->decl, 0, 0,
		   get_access_for_fnspec (e, fnspec, i, map), false);
	    }
	}
      if (ignore_stores_p (node->decl, ecf_flags))
	;
      else if (fnspec.global_memory_written_p ())
	collapse_stores (cur_summary, cur_summary_lto);
      else
	{
	  tree t = TYPE_ARG_TYPES (TREE_TYPE (e->callee->decl));
	  for (unsigned i = 0; i < parm_map.length () && t;
	       i++, t = TREE_CHAIN (t))
	    if (!POINTER_TYPE_P (TREE_VALUE (t)))
	      ;
	  else if (!fnspec.arg_specified_p (i)
		   || fnspec.arg_maybe_written_p (i))
	    {
	      modref_parm_map map = parm_map[i];
	      if (map.parm_index == MODREF_LOCAL_MEMORY_PARM)
		continue;
	      if (map.parm_index == MODREF_UNKNOWN_PARM)
		{
		  collapse_stores (cur_summary, cur_summary_lto);
		  break;
		}
	      if (cur_summary)
		changed |= cur_summary->stores->insert
		  (node->decl, 0, 0,
		   get_access_for_fnspec (e, fnspec, i, map), false);
	      if (cur_summary_lto)
		changed |= cur_summary_lto->stores->insert
		  (node->decl, 0, 0,
		   get_access_for_fnspec (e, fnspec, i, map), false);
	    }
	}
      if (fnspec.errno_maybe_written_p () && flag_errno_math)
	{
	  if (cur_summary && !cur_summary->writes_errno)
	    {
	      cur_summary->writes_errno = true;
	      changed = true;
	    }
	  if (cur_summary_lto && !cur_summary_lto->writes_errno)
	    {
	      cur_summary_lto->writes_errno = true;
	      changed = true;
	    }
	}
      return changed;
    }
  if (dump_file)
    fprintf (dump_file, "      collapsing loads\n");
  changed |= collapse_loads (cur_summary, cur_summary_lto);
  if (!ignore_stores_p (node->decl, ecf_flags))
    {
      if (dump_file)
	fprintf (dump_file, "      collapsing stores\n");
      changed |= collapse_stores (cur_summary, cur_summary_lto);
    }
  return changed;
}

/* Maybe remove summaries of NODE pointed to by CUR_SUMMARY_PTR
   and CUR_SUMMARY_LTO_PTR if they are useless according to ECF_FLAGS.  */

static void
remove_useless_summaries (cgraph_node *node,
			  modref_summary **cur_summary_ptr,
			  modref_summary_lto **cur_summary_lto_ptr,
			  int ecf_flags)
{
  if (*cur_summary_ptr && !(*cur_summary_ptr)->useful_p (ecf_flags, false))
    {
      optimization_summaries->remove (node);
      *cur_summary_ptr = NULL;
    }
  if (*cur_summary_lto_ptr
      && !(*cur_summary_lto_ptr)->useful_p (ecf_flags, false))
    {
      summaries_lto->remove (node);
      *cur_summary_lto_ptr = NULL;
    }
}

/* Perform iterative dataflow on SCC component starting in COMPONENT_NODE
   and propagate loads/stores.  */

static bool
modref_propagate_in_scc (cgraph_node *component_node)
{
  bool changed = true;
  bool first = true;
  int iteration = 0;

  while (changed)
    {
      bool nontrivial_scc
		 = ((struct ipa_dfs_info *) component_node->aux)->next_cycle;
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

	  int cur_ecf_flags = flags_from_decl_or_type (node->decl);

	  if (dump_file)
	    fprintf (dump_file, "  Processing %s%s%s\n",
		     cur->dump_name (),
		     TREE_READONLY (cur->decl) ? " (const)" : "",
		     DECL_PURE_P (cur->decl) ? " (pure)" : "");

	  for (cgraph_edge *e = cur->indirect_calls; e; e = e->next_callee)
	    {
	      if (dump_file)
		fprintf (dump_file, "    Indirect call\n");
	      if (propagate_unknown_call
			   (node, e, e->indirect_info->ecf_flags,
			    cur_summary, cur_summary_lto,
			    nontrivial_scc))
		{
		  changed = true;
		  remove_useless_summaries (node, &cur_summary,
					    &cur_summary_lto,
					    cur_ecf_flags);
		  if (!cur_summary && !cur_summary_lto)
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

	      if (!callee_edge->inline_failed
		 || ((flags & ECF_CONST)
		     && !(flags & ECF_LOOPING_CONST_OR_PURE)))
		continue;

	      /* Get the callee and its summary.  */
	      enum availability avail;
	      callee = callee_edge->callee->ultimate_alias_target
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
		  if (dump_file)
		    fprintf (dump_file, "      Call target interposable"
			     " or not available\n");
		  changed |= propagate_unknown_call
			       (node, callee_edge, flags,
				cur_summary, cur_summary_lto,
				nontrivial_scc);
		  if (!cur_summary && !cur_summary_lto)
		    break;
		  continue;
		}

	      /* We don't know anything about CALLEE, hence we cannot tell
		 anything about the entire component.  */

	      if (cur_summary
		  && !(callee_summary = optimization_summaries->get (callee)))
		{
		  if (dump_file)
		    fprintf (dump_file, "      No call target summary\n");
		  changed |= propagate_unknown_call
			       (node, callee_edge, flags,
				cur_summary, NULL,
				nontrivial_scc);
		}
	      if (cur_summary_lto
		  && !(callee_summary_lto = summaries_lto->get (callee)))
		{
		  if (dump_file)
		    fprintf (dump_file, "      No call target summary\n");
		  changed |= propagate_unknown_call
			       (node, callee_edge, flags,
				NULL, cur_summary_lto,
				nontrivial_scc);
		}

	      if (callee_summary && !cur_summary->side_effects
		  && (callee_summary->side_effects
		      || callee_edge->recursive_p ()))
		{
		  cur_summary->side_effects = true;
		  changed = true;
		}
	      if (callee_summary_lto && !cur_summary_lto->side_effects
		  && (callee_summary_lto->side_effects
		      || callee_edge->recursive_p ()))
		{
		  cur_summary_lto->side_effects = true;
		  changed = true;
		}
	      if (callee_summary && !cur_summary->nondeterministic
		  && callee_summary->nondeterministic
		  && !ignore_nondeterminism_p
			  (cur->decl, flags,
			   TREE_TYPE (callee_edge->callee->decl)))
		{
		  cur_summary->nondeterministic = true;
		  changed = true;
		}
	      if (callee_summary_lto && !cur_summary_lto->nondeterministic
		  && callee_summary_lto->nondeterministic
		  && !ignore_nondeterminism_p
			  (cur->decl, flags,
			   TREE_TYPE (callee_edge->callee->decl)))
		{
		  cur_summary_lto->nondeterministic = true;
		  changed = true;
		}
	      if (flags & (ECF_CONST | ECF_NOVOPS))
		continue;

	      /* We can not safely optimize based on summary of callee if it
		 does not always bind to current def: it is possible that
		 memory load was optimized out earlier which may not happen in
		 the interposed variant.  */
	      if (!callee_edge->binds_to_current_def_p ())
		{
		  if (cur_summary && !cur_summary->calls_interposable)
		    {
		      cur_summary->calls_interposable = true;
		      changed = true;
		    }
		  if (cur_summary_lto && !cur_summary_lto->calls_interposable)
		    {
		      cur_summary_lto->calls_interposable = true;
		      changed = true;
		    }
		  if (dump_file)
		    fprintf (dump_file, "      May not bind local;"
			     " collapsing loads\n");
		}


	      auto_vec <modref_parm_map, 32> parm_map;
	      modref_parm_map chain_map;
	      /* TODO: Once we get jump functions for static chains we could
		 compute this.  */
	      chain_map.parm_index = MODREF_UNKNOWN_PARM;

	      compute_parm_map (callee_edge, &parm_map);

	      /* Merge in callee's information.  */
	      if (callee_summary)
		{
		  changed |= cur_summary->loads->merge
				  (node->decl, callee_summary->loads,
				   &parm_map, &chain_map, !first);
		  if (!ignore_stores)
		    {
		      changed |= cur_summary->stores->merge
				      (node->decl, callee_summary->stores,
				       &parm_map, &chain_map, !first);
		      if (!cur_summary->writes_errno
			  && callee_summary->writes_errno)
			{
			  cur_summary->writes_errno = true;
			  changed = true;
			}
		    }
		}
	      if (callee_summary_lto)
		{
		  changed |= cur_summary_lto->loads->merge
				  (node->decl, callee_summary_lto->loads,
				   &parm_map, &chain_map, !first);
		  if (!ignore_stores)
		    {
		      changed |= cur_summary_lto->stores->merge
				      (node->decl, callee_summary_lto->stores,
				       &parm_map, &chain_map, !first);
		      if (!cur_summary_lto->writes_errno
			  && callee_summary_lto->writes_errno)
			{
			  cur_summary_lto->writes_errno = true;
			  changed = true;
			}
		    }
		}
	      if (changed)
		remove_useless_summaries (node, &cur_summary,
					  &cur_summary_lto,
					  cur_ecf_flags);
	      if (!cur_summary && !cur_summary_lto)
		break;
	      if (dump_file && changed)
		{
		  if (cur_summary)
		    cur_summary->dump (dump_file);
		  if (cur_summary_lto)
		    cur_summary_lto->dump (dump_file);
		  dump_modref_edge_summaries (dump_file, node, 4);
		}
	    }
	}
      iteration++;
      first = false;
    }
  if (dump_file)
    fprintf (dump_file,
	     "Propagation finished in %i iterations\n", iteration);
  bool pureconst = false;
  for (struct cgraph_node *cur = component_node; cur;
       cur = ((struct ipa_dfs_info *) cur->aux)->next_cycle)
    if (!cur->inlined_to && opt_for_fn (cur->decl, flag_ipa_pure_const))
      {
	modref_summary *summary = optimization_summaries
				  ? optimization_summaries->get (cur)
				  : NULL;
	modref_summary_lto *summary_lto = summaries_lto
					  ? summaries_lto->get (cur)
					  : NULL;
	if (summary && !summary->stores->every_base && !summary->stores->bases
	    && !summary->nondeterministic)
	  {
	    if (!summary->loads->every_base && !summary->loads->bases
		&& !summary->calls_interposable)
	      pureconst |= ipa_make_function_const
		     (cur, summary->side_effects, false);
	    else
	      pureconst |= ipa_make_function_pure
		     (cur, summary->side_effects, false);
	  }
	if (summary_lto && !summary_lto->stores->every_base
	    && !summary_lto->stores->bases && !summary_lto->nondeterministic)
	  {
	    if (!summary_lto->loads->every_base && !summary_lto->loads->bases
		&& !summary_lto->calls_interposable)
	      pureconst |= ipa_make_function_const
		     (cur, summary_lto->side_effects, false);
	    else
	      pureconst |= ipa_make_function_pure
		     (cur, summary_lto->side_effects, false);
	  }
     }
  return pureconst;
}

/* Dump results of propagation in SCC rooted in COMPONENT_NODE.  */

static void
modref_propagate_dump_scc (cgraph_node *component_node)
{
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

/* Determine EAF flags know for call E with CALLEE_ECF_FLAGS and ARG.  */

int
implicit_eaf_flags_for_edge_and_arg (cgraph_edge *e, int callee_ecf_flags,
				     bool ignore_stores, int arg)
{
  /* Returning the value is already accounted to at local propagation.  */
  int implicit_flags = EAF_NOT_RETURNED_DIRECTLY
		       | EAF_NOT_RETURNED_INDIRECTLY;
  if (ignore_stores)
     implicit_flags |= ignore_stores_eaf_flags;
  if (callee_ecf_flags & ECF_PURE)
    implicit_flags |= implicit_pure_eaf_flags;
  if (callee_ecf_flags & (ECF_CONST | ECF_NOVOPS))
    implicit_flags |= implicit_const_eaf_flags;
  class fnspec_summary *fnspec_sum = fnspec_summaries->get (e);
  if (fnspec_sum)
    {
      attr_fnspec fnspec (fnspec_sum->fnspec);
      implicit_flags |= fnspec.arg_eaf_flags (arg);
    }
  return implicit_flags;
}

/* Process escapes in SUM and merge SUMMARY to CUR_SUMMARY
   and SUMMARY_LTO to CUR_SUMMARY_LTO.
   Return true if something changed.  */

static bool
modref_merge_call_site_flags (escape_summary *sum,
			      modref_summary *cur_summary,
			      modref_summary_lto *cur_summary_lto,
			      modref_summary *summary,
			      modref_summary_lto *summary_lto,
			      tree caller,
			      cgraph_edge *e,
			      int caller_ecf_flags,
			      int callee_ecf_flags,
			      bool binds_to_current_def)
{
  escape_entry *ee;
  unsigned int i;
  bool changed = false;
  bool ignore_stores = ignore_stores_p (caller, callee_ecf_flags);

  /* Return early if we have no useful info to propagate.  */
  if ((!cur_summary
       || (!cur_summary->arg_flags.length ()
	   && !cur_summary->static_chain_flags
	   && !cur_summary->retslot_flags))
      && (!cur_summary_lto
	  || (!cur_summary_lto->arg_flags.length ()
	      && !cur_summary_lto->static_chain_flags
	      && !cur_summary_lto->retslot_flags)))
    return false;

  FOR_EACH_VEC_ELT (sum->esc, i, ee)
    {
      int flags = 0;
      int flags_lto = 0;
      int implicit_flags = implicit_eaf_flags_for_edge_and_arg
				(e, callee_ecf_flags, ignore_stores, ee->arg);

      if (summary && ee->arg < summary->arg_flags.length ())
	flags = summary->arg_flags[ee->arg];
      if (summary_lto
	  && ee->arg < summary_lto->arg_flags.length ())
	flags_lto = summary_lto->arg_flags[ee->arg];
      if (!ee->direct)
	{
	  flags = deref_flags (flags, ignore_stores);
	  flags_lto = deref_flags (flags_lto, ignore_stores);
	}
      if (ignore_stores)
	 implicit_flags |= ignore_stores_eaf_flags;
      if (callee_ecf_flags & ECF_PURE)
	implicit_flags |= implicit_pure_eaf_flags;
      if (callee_ecf_flags & (ECF_CONST | ECF_NOVOPS))
	implicit_flags |= implicit_const_eaf_flags;
      class fnspec_summary *fnspec_sum = fnspec_summaries->get (e);
      if (fnspec_sum)
	{
	  attr_fnspec fnspec (fnspec_sum->fnspec);
	  implicit_flags |= fnspec.arg_eaf_flags (ee->arg);
	}
      if (!ee->direct)
	implicit_flags = deref_flags (implicit_flags, ignore_stores);
      flags |= implicit_flags;
      flags_lto |= implicit_flags;
      if (!binds_to_current_def && (flags || flags_lto))
	{
	  flags = interposable_eaf_flags (flags, implicit_flags);
	  flags_lto = interposable_eaf_flags (flags_lto, implicit_flags);
	}
      if (!(flags & EAF_UNUSED)
	  && cur_summary && ee->parm_index < (int)cur_summary->arg_flags.length ())
	{
	  eaf_flags_t &f = ee->parm_index == MODREF_RETSLOT_PARM
			   ? cur_summary->retslot_flags
			   : ee->parm_index == MODREF_STATIC_CHAIN_PARM
			   ? cur_summary->static_chain_flags
			   : cur_summary->arg_flags[ee->parm_index];
	  if ((f & flags) != f)
	    {
	      f = remove_useless_eaf_flags
			 (f & flags, caller_ecf_flags,
			  VOID_TYPE_P (TREE_TYPE (TREE_TYPE (caller))));
	      changed = true;
	    }
	}
      if (!(flags_lto & EAF_UNUSED)
	  && cur_summary_lto
	  && ee->parm_index < (int)cur_summary_lto->arg_flags.length ())
	{
	  eaf_flags_t &f = ee->parm_index == MODREF_RETSLOT_PARM
			   ? cur_summary_lto->retslot_flags
			   : ee->parm_index == MODREF_STATIC_CHAIN_PARM
			   ? cur_summary_lto->static_chain_flags
			   : cur_summary_lto->arg_flags[ee->parm_index];
	  if ((f & flags_lto) != f)
	    {
	      f = remove_useless_eaf_flags
			 (f & flags_lto, caller_ecf_flags,
			  VOID_TYPE_P (TREE_TYPE (TREE_TYPE (caller))));
	      changed = true;
	    }
	}
    }
  return changed;
}

/* Perform iterative dataflow on SCC component starting in COMPONENT_NODE
   and propagate arg flags.  */

static void
modref_propagate_flags_in_scc (cgraph_node *component_node)
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
	  int caller_ecf_flags = flags_from_decl_or_type (cur->decl);

	  if (dump_file)
	    fprintf (dump_file, "  Processing %s%s%s\n",
		     cur->dump_name (),
		     TREE_READONLY (cur->decl) ? " (const)" : "",
		     DECL_PURE_P (cur->decl) ? " (pure)" : "");

	  for (cgraph_edge *e = cur->indirect_calls; e; e = e->next_callee)
	    {
	      escape_summary *sum = escape_summaries->get (e);

	      if (!sum || ((e->indirect_info->ecf_flags & ECF_CONST)
		  && !(e->indirect_info->ecf_flags & ECF_LOOPING_CONST_OR_PURE)))
		continue;

	      changed |= modref_merge_call_site_flags
				(sum, cur_summary, cur_summary_lto,
				 NULL, NULL,
				 node->decl,
				 e,
				 caller_ecf_flags,
				 e->indirect_info->ecf_flags,
				 false);
	    }

	  if (!cur_summary && !cur_summary_lto)
	    continue;

	  for (cgraph_edge *callee_edge = cur->callees; callee_edge;
	       callee_edge = callee_edge->next_callee)
	    {
	      int ecf_flags = flags_from_decl_or_type
				 (callee_edge->callee->decl);
	      modref_summary *callee_summary = NULL;
	      modref_summary_lto *callee_summary_lto = NULL;
	      struct cgraph_node *callee;

	      if ((ecf_flags & ECF_CONST)
		  && !(ecf_flags & ECF_LOOPING_CONST_OR_PURE))
		continue;

	      /* Get the callee and its summary.  */
	      enum availability avail;
	      callee = callee_edge->callee->ultimate_alias_target
			 (&avail, cur);

	      /* It is not necessary to re-process calls outside of the
		 SCC component.  */
	      if (iteration > 0
		  && (!callee->aux
		      || ((struct ipa_dfs_info *)cur->aux)->scc_no
			  != ((struct ipa_dfs_info *)callee->aux)->scc_no))
		continue;

	      escape_summary *sum = escape_summaries->get (callee_edge);
	      if (!sum)
		continue;

	      if (dump_file)
		fprintf (dump_file, "    Call to %s\n",
			 callee_edge->callee->dump_name ());

	      if (avail <= AVAIL_INTERPOSABLE
		  || callee_edge->call_stmt_cannot_inline_p)
		;
	      else
		{
		  if (cur_summary)
		    callee_summary = optimization_summaries->get (callee);
		  if (cur_summary_lto)
		    callee_summary_lto = summaries_lto->get (callee);
		}
	      changed |= modref_merge_call_site_flags
				(sum, cur_summary, cur_summary_lto,
				 callee_summary, callee_summary_lto,
				 node->decl,
				 callee_edge,
				 caller_ecf_flags,
				 ecf_flags,
				 callee->binds_to_current_def_p ());
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
    fprintf (dump_file,
	     "Propagation of flags finished in %i iterations\n", iteration);
}

}  /* ANON namespace.  */

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
      remove_modref_edge_summaries (edge->callee);
      return;
    }

  class modref_summary *callee_info = summaries ? summaries->get (edge->callee)
				      : NULL;
  class modref_summary_lto *callee_info_lto
		 = summaries_lto ? summaries_lto->get (edge->callee) : NULL;
  int flags = flags_from_decl_or_type (edge->callee->decl);
  /* Combine in outer flags.  */
  cgraph_node *n;
  for (n = edge->caller; n->inlined_to; n = n->callers->caller)
    flags |= flags_from_decl_or_type (n->decl);
  flags |= flags_from_decl_or_type (n->decl);
  bool ignore_stores = ignore_stores_p (edge->caller->decl, flags);

  if (!callee_info && to_info)
    {
      if (!(flags & (ECF_CONST | ECF_PURE | ECF_NOVOPS)))
	to_info->loads->collapse ();
      if (!ignore_stores)
	to_info->stores->collapse ();
    }
  if (!callee_info_lto && to_info_lto)
    {
      if (!(flags & (ECF_CONST | ECF_NOVOPS)))
	to_info_lto->loads->collapse ();
      if (!ignore_stores)
	to_info_lto->stores->collapse ();
    }
  /* Merge side effects and non-determinism.
     PURE/CONST flags makes functions deterministic and if there is
     no LOOPING_CONST_OR_PURE they also have no side effects.  */
  if (!(flags & (ECF_CONST | ECF_PURE))
      || (flags & ECF_LOOPING_CONST_OR_PURE))
    {
      bool set_nondeterministic
	      = !ignore_nondeterminism_p
		      (edge->caller->decl, flags,
		       TREE_TYPE (edge->callee->decl));
      if (to_info)
	{
	  if (!callee_info || callee_info->side_effects)
	    to_info->side_effects = true;
	  if (set_nondeterministic)
	    to_info->nondeterministic = true;
	}
      if (to_info_lto)
	{
	  if (!callee_info_lto || callee_info_lto->side_effects)
	    to_info_lto->side_effects = true;
	  if (set_nondeterministic)
	    to_info_lto->nondeterministic = true;
	}
     }
  if (callee_info || callee_info_lto)
    {
      auto_vec <modref_parm_map, 32> parm_map;
      modref_parm_map chain_map;
      /* TODO: Once we get jump functions for static chains we could
	 compute parm_index.  */

      compute_parm_map (edge, &parm_map);

      if (!ignore_stores)
	{
	  if (to_info && callee_info)
	    to_info->stores->merge (to->decl, callee_info->stores, &parm_map,
				    &chain_map, false);
	  if (to_info_lto && callee_info_lto)
	    to_info_lto->stores->merge (to->decl, callee_info_lto->stores,
					&parm_map, &chain_map, false);
	}
      if (!(flags & (ECF_CONST | ECF_NOVOPS)))
	{
	  if (to_info && callee_info)
	    to_info->loads->merge (to->decl, callee_info->loads, &parm_map,
				   &chain_map, false);
	  if (to_info_lto && callee_info_lto)
	    to_info_lto->loads->merge (to->decl, callee_info_lto->loads,
				       &parm_map, &chain_map, false);
	}
    }

  /* Now merge escape summaries.
     For every escape to the callee we need to merge callee flags
     and remap callee's escapes.  */
  class escape_summary *sum = escape_summaries->get (edge);
  int max_escape = -1;
  escape_entry *ee;
  unsigned int i;

  if (sum && !(flags & (ECF_CONST | ECF_NOVOPS)))
    FOR_EACH_VEC_ELT (sum->esc, i, ee)
      if ((int)ee->arg > max_escape)
	max_escape = ee->arg;

  auto_vec <vec <struct escape_map>, 32> emap (max_escape + 1);
  emap.safe_grow (max_escape + 1, true);
  for (i = 0; (int)i < max_escape + 1; i++)
    emap[i] = vNULL;

  if (sum && !(flags & (ECF_CONST | ECF_NOVOPS)))
    FOR_EACH_VEC_ELT (sum->esc, i, ee)
      {
	bool needed = false;
	int implicit_flags = implicit_eaf_flags_for_edge_and_arg
				(edge, flags, ignore_stores,
				 ee->arg);
	if (!ee->direct)
	  implicit_flags = deref_flags (implicit_flags, ignore_stores);
	if (to_info && (int)to_info->arg_flags.length () > ee->parm_index)
	  {
	    int flags = callee_info
			&& callee_info->arg_flags.length () > ee->arg
			? callee_info->arg_flags[ee->arg] : 0;
	    if (!ee->direct)
	      flags = deref_flags (flags, ignore_stores);
	    flags |= ee->min_flags | implicit_flags;
	    eaf_flags_t &f = ee->parm_index == MODREF_RETSLOT_PARM
			     ? to_info->retslot_flags
			     : ee->parm_index == MODREF_STATIC_CHAIN_PARM
			     ? to_info->static_chain_flags
			     : to_info->arg_flags[ee->parm_index];
	    f &= flags;
	    if (f)
	      needed = true;
	  }
	if (to_info_lto
	    && (int)to_info_lto->arg_flags.length () > ee->parm_index)
	  {
	    int flags = callee_info_lto
			&& callee_info_lto->arg_flags.length () > ee->arg
			? callee_info_lto->arg_flags[ee->arg] : 0;
	    if (!ee->direct)
	      flags = deref_flags (flags, ignore_stores);
	    flags |= ee->min_flags | implicit_flags;
	    eaf_flags_t &f = ee->parm_index == MODREF_RETSLOT_PARM
			     ? to_info_lto->retslot_flags
			     : ee->parm_index == MODREF_STATIC_CHAIN_PARM
			     ? to_info_lto->static_chain_flags
			     : to_info_lto->arg_flags[ee->parm_index];
	    f &= flags;
	    if (f)
	      needed = true;
	  }
	struct escape_map entry = {ee->parm_index, ee->direct};
	if (needed)
	  emap[ee->arg].safe_push (entry);
      }
  update_escape_summary (edge->callee, emap, ignore_stores);
  for (i = 0; (int)i < max_escape + 1; i++)
    emap[i].release ();
  if (sum)
    escape_summaries->remove (edge);

  if (summaries)
    {
      if (to_info && !to_info->useful_p (flags))
	{
	  if (dump_file)
	    fprintf (dump_file, "Removed mod-ref summary for %s\n",
		     to->dump_name ());
	  summaries->remove (to);
	  to_info = NULL;
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
	  to_info_lto = NULL;
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
  if (!to_info && !to_info_lto)
    remove_modref_edge_summaries (to);
  return;
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
  bool pureconst = false;

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

      pureconst |= modref_propagate_in_scc (component_node);
      modref_propagate_flags_in_scc (component_node);
      if (optimization_summaries)
	for (struct cgraph_node *cur = component_node; cur;
	     cur = ((struct ipa_dfs_info *) cur->aux)->next_cycle)
	  if (modref_summary *sum = optimization_summaries->get (cur))
	    sum->finalize (cur->decl);
      if (dump_file)
	modref_propagate_dump_scc (component_node);
    }
  cgraph_node *node;
  FOR_EACH_FUNCTION (node)
    update_signature (node);
  if (summaries_lto)
    ((modref_summaries_lto *)summaries_lto)->propagated = true;
  ipa_free_postorder_info ();
  free (order);
  delete fnspec_summaries;
  fnspec_summaries = NULL;
  delete escape_summaries;
  escape_summaries = NULL;

  /* If we possibly made constructors const/pure we may need to remove
     them.  */
  return pureconst ? TODO_remove_functions : 0;
}

/* Summaries must stay alive until end of compilation.  */

void
ipa_modref_cc_finalize ()
{
  if (optimization_summaries)
    ggc_delete (optimization_summaries);
  optimization_summaries = NULL;
  if (summaries_lto)
    ggc_delete (summaries_lto);
  summaries_lto = NULL;
  if (fnspec_summaries)
    delete fnspec_summaries;
  fnspec_summaries = NULL;
  if (escape_summaries)
    delete escape_summaries;
  escape_summaries = NULL;
}

/* Return true if call is known to perform no memory reads.  */

bool
ipa_modref_callee_reads_no_memory_p (gcall *call)
{
  if (gimple_call_flags (call) & ECF_CONST)
    return true;
  attr_fnspec fnspec = gimple_call_fnspec (call);
  if (fnspec.known_p ()
      && !fnspec.global_memory_read_p ())
    {
      bool found = false;
      for (unsigned int i = 0; i < gimple_call_num_args (call) && !found; i++)
	if (!POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, i))))
	  ;
      else if (!fnspec.arg_specified_p (i)
	       || fnspec.arg_maybe_read_p (i))
	  found = true;
      if (!found)
	return true;
    }

  /* For interposed calls we can not be sure that the other, semantically
     equivalent body, will not perform some redundant load from memory
     that may become undefined if we optimize out some stores.  */
  bool interposed;
  modref_summary *sum = get_modref_function_summary (call, &interposed);
  if (sum && !interposed && !sum->global_memory_read && !sum->loads)
    return true;
  return false;
}

#include "gt-ipa-modref.h"
