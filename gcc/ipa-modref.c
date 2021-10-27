/* Search for references that a functions loads or stores.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
     2) EAF flags used by points-to analysis (in tree-ssa-structlias).
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
       That is a vector recording what function parmaeters
       may escape to a function call (and with what parameter index).  */

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
#include "attr-fnspec.h"
#include "symtab-clones.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-ssa-operands.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "attribs.h"


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
  virtual void duplicate (cgraph_edge *,
			  cgraph_edge *,
			  fnspec_summary *src,
			  fnspec_summary *dst)
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
  unsigned int parm_index;
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
  if (flags & EAF_DIRECT)
    fprintf (out, " direct");
  if (flags & EAF_NOCLOBBER)
    fprintf (out, " noclobber");
  if (flags & EAF_NOESCAPE)
    fprintf (out, " noescape");
  if (flags & EAF_NODIRECTESCAPE)
    fprintf (out, " nodirectescape");
  if (flags & EAF_UNUSED)
    fprintf (out, " unused");
  if (flags & EAF_NOT_RETURNED)
    fprintf (out, " not_returned");
  if (flags & EAF_NOREAD)
    fprintf (out, " noread");
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
  virtual void duplicate (cgraph_edge *,
			  cgraph_edge *,
			  escape_summary *src,
			  escape_summary *dst)
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
  : loads (NULL), stores (NULL), writes_errno (NULL)
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
  if (ecf_flags & ECF_NOVOPS)
    return 0;
  if (ecf_flags & ECF_CONST)
    eaf_flags &= ~implicit_const_eaf_flags;
  else if (ecf_flags & ECF_PURE)
    eaf_flags &= ~implicit_pure_eaf_flags;
  else if ((ecf_flags & ECF_NORETURN) || returns_void)
    eaf_flags &= ~EAF_NOT_RETURNED;
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
  if (ecf_flags & ECF_NOVOPS)
    return false;
  if (arg_flags.length () && !check_flags)
    return true;
  if (check_flags && eaf_flags_useful_p (arg_flags, ecf_flags))
    return true;
  arg_flags.release ();
  if (ecf_flags & ECF_CONST)
    return false;
  if (loads && !loads->every_base)
    return true;
  if (ecf_flags & ECF_PURE)
    return false;
  return stores && !stores->every_base;
}

/* Return true if global memory is read
   (that is loads summary contains global memory access).  */
bool
modref_summary::global_memory_read_p ()
{
  if (!loads)
    return true;
  return loads->global_access_p ();
}

/* Return true if global memory is written.  */
bool
modref_summary::global_memory_written_p ()
{
  if (!stores)
    return true;
  return stores->global_access_p ();
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
  auto_vec<eaf_flags_t> GTY((skip)) arg_flags;
  bool writes_errno;

  modref_summary_lto ();
  ~modref_summary_lto ();
  void dump (FILE *);
  bool useful_p (int ecf_flags, bool check_flags = true);
};

/* Summary for a single function which this pass produces.  */

modref_summary_lto::modref_summary_lto ()
  : loads (NULL), stores (NULL), writes_errno (NULL)
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
  if (ecf_flags & ECF_NOVOPS)
    return false;
  if (arg_flags.length () && !check_flags)
    return true;
  if (check_flags && eaf_flags_useful_p (arg_flags, ecf_flags))
    return true;
  arg_flags.release ();
  if (ecf_flags & ECF_CONST)
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
      if (a->adjustments)
	fprintf (out, " adjusted %i times", a->adjustments);
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
  if (writes_errno)
    fprintf (out, "  Writes errno\n");
  if (arg_flags.length ())
    {
      for (unsigned int i = 0; i < arg_flags.length (); i++)
	if (arg_flags[i])
	  {
	    fprintf (out, "  parm %i flags:", i);
	    dump_eaf_flags (out, arg_flags[i]);
	  }
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
  if (writes_errno)
    fprintf (out, "  Writes errno\n");
  if (arg_flags.length ())
    {
      for (unsigned int i = 0; i < arg_flags.length (); i++)
	if (arg_flags[i])
	  {
	    fprintf (out, "  parm %i flags:", i);
	    dump_eaf_flags (out, arg_flags[i]);
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
  func = func->function_or_virtual_thunk_symbol
		 (&avail, current_function_decl ?
			  cgraph_node::get (current_function_decl) : NULL);
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
			  0, -1, false, 0};
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
  tt->insert (base_set, ref_set, a, false);
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

  tt->insert (base_type, ref_type, a, false);
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

/* Determine parm_map for argument I of STMT.  */

modref_parm_map
parm_map_for_arg (gimple *stmt, int i)
{
  tree op = gimple_call_arg (stmt, i);
  bool offset_known;
  poly_int64 offset;
  struct modref_parm_map parm_map;

  parm_map.parm_offset_known = false;
  parm_map.parm_offset = 0;

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
      parm_map.parm_index = index;
      parm_map.parm_offset_known = offset_known;
      parm_map.parm_offset = offset;
    }
  else if (points_to_local_or_readonly_memory_p (op))
    parm_map.parm_index = -2;
  else
    parm_map.parm_index = -1;
  return parm_map;
}

/* Merge side effects of call STMT to function with CALLEE_SUMMARY
   int CUR_SUMMARY.  Return true if something changed.
   If IGNORE_STORES is true, do not merge stores.
   If RECORD_ADJUSTMENTS is true cap number of adjustments to
   a given access to make dataflow finite.  */

bool
merge_call_side_effects (modref_summary *cur_summary,
			 gimple *stmt, modref_summary *callee_summary,
			 bool ignore_stores, cgraph_node *callee_node,
			 bool record_adjustments)
{
  auto_vec <modref_parm_map, 32> parm_map;
  bool changed = false;

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

  if (dump_file)
    fprintf (dump_file, " - Merging side effects of %s with parm map:",
	     callee_node->dump_name ());

  parm_map.safe_grow_cleared (gimple_call_num_args (stmt), true);
  for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
    {
      parm_map[i] = parm_map_for_arg (stmt, i);
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
  changed |= cur_summary->loads->merge (callee_summary->loads, &parm_map,
					record_adjustments);
  if (!ignore_stores)
    {
      changed |= cur_summary->stores->merge (callee_summary->stores,
					     &parm_map,
					     record_adjustments);
      if (!cur_summary->writes_errno
	  && callee_summary->writes_errno)
	{
	  cur_summary->writes_errno = true;
	  changed = true;
	}
    }
  return changed;
}

/* Return access mode for argument I of call STMT with FNSPEC.  */

static modref_access_node
get_access_for_fnspec (gcall *call, attr_fnspec &fnspec,
		       unsigned int i, modref_parm_map &map)
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


/* Apply side effects of call STMT to CUR_SUMMARY using FNSPEC.
   If IGNORE_STORES is true ignore them.
   Return false if no useful summary can be produced.   */

static bool
process_fnspec (modref_summary *cur_summary,
		modref_summary_lto *cur_summary_lto,
		gcall *call, bool ignore_stores)
{
  attr_fnspec fnspec = gimple_call_fnspec (call);
  if (!fnspec.known_p ())
    {
      if (dump_file && gimple_call_builtin_p (call, BUILT_IN_NORMAL))
	fprintf (dump_file, "      Builtin with no fnspec: %s\n",
		 IDENTIFIER_POINTER (DECL_NAME (gimple_call_fndecl (call))));
      if (ignore_stores)
	{
	  collapse_loads (cur_summary, cur_summary_lto);
	  return true;
	}
      return false;
    }
  if (fnspec.global_memory_read_p ())
    collapse_loads (cur_summary, cur_summary_lto);
  else
    {
      for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
	if (!POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, i))))
	  ;
	else if (!fnspec.arg_specified_p (i)
		 || fnspec.arg_maybe_read_p (i))
	  {
	    modref_parm_map map = parm_map_for_arg (call, i);

	    if (map.parm_index == -2)
	      continue;
	    if (map.parm_index == -1)
	      {
		collapse_loads (cur_summary, cur_summary_lto);
		break;
	      }
	    if (cur_summary)
	      cur_summary->loads->insert (0, 0,
					  get_access_for_fnspec (call,
								 fnspec, i,
								 map),
					  false);
	    if (cur_summary_lto)
	      cur_summary_lto->loads->insert (0, 0,
					      get_access_for_fnspec (call,
								     fnspec, i,
								     map),
					      false);
	  }
    }
  if (ignore_stores)
    return true;
  if (fnspec.global_memory_written_p ())
    collapse_stores (cur_summary, cur_summary_lto);
  else
    {
      for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
	if (!POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, i))))
	  ;
	else if (!fnspec.arg_specified_p (i)
		 || fnspec.arg_maybe_written_p (i))
	  {
	    modref_parm_map map = parm_map_for_arg (call, i);

	    if (map.parm_index == -2)
	      continue;
	    if (map.parm_index == -1)
	      {
		collapse_stores (cur_summary, cur_summary_lto);
		break;
	      }
	    if (cur_summary)
	      cur_summary->stores->insert (0, 0,
					   get_access_for_fnspec (call,
								  fnspec, i,
								  map),
					   false);
	    if (cur_summary_lto)
	      cur_summary_lto->stores->insert (0, 0,
					       get_access_for_fnspec (call,
								      fnspec, i,
								      map),
					       false);
	  }
      if (fnspec.errno_maybe_written_p () && flag_errno_math)
	{
	  if (cur_summary)
	    cur_summary->writes_errno = true;
	  if (cur_summary_lto)
	    cur_summary_lto->writes_errno = true;
	}
    }
  return true;
}

/* Analyze function call STMT in function F.
   Remember recursive calls in RECURSIVE_CALLS.  */

static bool
analyze_call (modref_summary *cur_summary, modref_summary_lto *cur_summary_lto,
	      gcall *stmt, vec <gimple *> *recursive_calls)
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
      if (dump_file)
	fprintf (dump_file, gimple_call_internal_p (stmt)
		 ? " - Internal call" : " - Indirect call.\n");
      return process_fnspec (cur_summary, cur_summary_lto, stmt, ignore_stores);
    }
  /* We only need to handle internal calls in IPA mode.  */
  gcc_checking_assert (!cur_summary_lto);

  struct cgraph_node *callee_node = cgraph_node::get_create (callee);

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
      if (dump_file)
	fprintf (dump_file, " - Function availability <= AVAIL_INTERPOSABLE.\n");
      return process_fnspec (cur_summary, cur_summary_lto, stmt, ignore_stores);
    }

  /* Get callee's modref summary.  As above, if there's no summary, we either
     have to give up or, if stores are ignored, we can just purge loads.  */
  modref_summary *callee_summary = optimization_summaries->get (callee_node);
  if (!callee_summary)
    {
      if (dump_file)
	fprintf (dump_file, " - No modref summary available for callee.\n");
      return process_fnspec (cur_summary, cur_summary_lto, stmt, ignore_stores);
    }

  merge_call_side_effects (cur_summary, stmt, callee_summary, ignore_stores,
			   callee_node, false);

  return true;
}

/* Support analysis in non-lto and lto mode in parallel.  */

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
  /* In general we can not ignore clobbers because they are barriers for code
     motion, however after inlining it is safe to do because local optimization
     passes do not consider clobbers from other functions.
     Similar logic is in ipa-pure-const.c.  */
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
     if (!ipa || gimple_call_internal_p (stmt))
       return analyze_call (summary, summary_lto,
			    as_a <gcall *> (stmt), recursive_calls);
     else
      {
	attr_fnspec fnspec = gimple_call_fnspec (as_a <gcall *>(stmt));

	if (fnspec.known_p ()
	    && (!fnspec.global_memory_read_p ()
		|| !fnspec.global_memory_written_p ()))
	  {
	    cgraph_edge *e = cgraph_node::get (current_function_decl)->get_edge (stmt);
	    if (e->callee)
	      {
		fnspec_summaries->get_create (e)->fnspec = xstrdup (fnspec.get_str ());
		if (dump_file)
		  fprintf (dump_file, "  Recorded fnspec %s\n", fnspec.get_str ());
	      }
	  }
      }
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
      remove_modref_edge_summaries (fnode);
    }
  if (dump_file)
    fprintf (dump_file,
	     " - modref done with result: not tracked.\n");
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
  int ret = EAF_NODIRECTESCAPE;
  /* If argument is unused just account for
     the read involved in dereference.  */
  if (flags & EAF_UNUSED)
    ret |= EAF_DIRECT | EAF_NOCLOBBER | EAF_NOESCAPE | EAF_NOT_RETURNED;
  else
    {
      if ((flags & EAF_NOCLOBBER) || ignore_stores)
	ret |= EAF_NOCLOBBER;
      if ((flags & EAF_NOESCAPE) || ignore_stores)
	ret |= EAF_NOESCAPE;
      /* If the value dereferenced is not used for another load or store
	 we can still consider ARG as used only directly.

	 Consider

	 int
	 test (int *a)
	   {
	     return *a!=0;
	   }

	*/
      if ((flags & (EAF_NOREAD | EAF_NOT_RETURNED | EAF_NOESCAPE | EAF_DIRECT))
	  == (EAF_NOREAD | EAF_NOT_RETURNED | EAF_NOESCAPE | EAF_DIRECT)
	  && ((flags & EAF_NOCLOBBER) || ignore_stores))
	ret |= EAF_DIRECT;
      if (flags & EAF_NOT_RETURNED)
	ret |= EAF_NOT_RETURNED;
    }
  return ret;
}

namespace {

/* Description of an escape point.  */

struct escape_point
{
  /* Value escapes to this call.  */
  gcall *call;
  /* Argument it escapes to.  */
  int arg;
  /* Flags already known about the argument (this can save us from recording
     esape points if local analysis did good job already).  */
  eaf_flags_t min_flags;
  /* Does value escape directly or indiretly?  */
  bool direct;
};

class modref_lattice
{
public:
  /* EAF flags of the SSA name.  */
  eaf_flags_t flags;
  /* DFS bookkkeeping: we don't do real dataflow yet.  */
  bool known;
  bool open;

  /* When doing IPA analysis we can not merge in callee escape points;
     Only remember them and do the merging at IPA propagation time.  */
  vec <escape_point, va_heap, vl_ptr> escape_points;

  void init ();
  void release ();
  bool merge (const modref_lattice &with);
  bool merge (int flags);
  bool merge_deref (const modref_lattice &with, bool ignore_stores);
  bool merge_direct_load ();
  bool merge_direct_store ();
  bool add_escape_point (gcall *call, int arg, int min_flags, bool diret);
  void dump (FILE *out, int indent = 0) const;
};

/* Lattices are saved to vectors, so keep them PODs.  */
void
modref_lattice::init ()
{
  /* All flags we track.  */
  int f = EAF_DIRECT | EAF_NOCLOBBER | EAF_NOESCAPE | EAF_UNUSED
	  | EAF_NODIRECTESCAPE | EAF_NOT_RETURNED | EAF_NOREAD;
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
modref_lattice::add_escape_point (gcall *call, int arg, int min_flags,
				  bool direct)
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
  /* Noescape implies that value also does not escape directly.
     Fnspec machinery does set both so compensate for this.  */
  if (f & EAF_NOESCAPE)
    f |= EAF_NODIRECTESCAPE;
  if ((flags & f) != flags)
    {
      flags &= f;
      /* Prune obvoiusly useless flags;
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

/* Merge in WITH.  Return true if anyting changed.  */

bool
modref_lattice::merge (const modref_lattice &with)
{
  if (!with.known)
    return merge (0);

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
   stores.  Return true if anyting changed.  */

bool
modref_lattice::merge_deref (const modref_lattice &with, bool ignore_stores)
{
  if (!with.known)
    return merge (0);

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
  return merge (~(EAF_UNUSED | EAF_NOREAD));
}

/* Merge in flags for direct store.  */

bool
modref_lattice::merge_direct_store ()
{
  return merge (~(EAF_UNUSED | EAF_NOCLOBBER));
}

}  /* ANON namespace.  */

static void analyze_ssa_name_flags (tree name,
				    vec<modref_lattice> &lattice,
				    int depth, bool ipa);

/* Call statements may return their parameters.  Consider argument number
   ARG of USE_STMT and determine flags that can needs to be cleared
   in case pointer possibly indirectly references from ARG I is returned.
   LATTICE, DEPTH and ipa are same as in analyze_ssa_name_flags.  */

static void
merge_call_lhs_flags (gcall *call, int arg, int index, bool deref,
		      vec<modref_lattice> &lattice,
		      int depth, bool ipa)
{
  /* If there is no return value, no flags are affected.  */
  if (!gimple_call_lhs (call))
    return;

  /* If we know that function returns given argument and it is not ARG
     we can still be happy.  */
  int flags = gimple_call_return_flags (call);
  if ((flags & ERF_RETURNS_ARG)
      && (flags & ERF_RETURN_ARG_MASK) != arg)
    return;

  if (gimple_call_arg_flags (call, arg) & (EAF_NOT_RETURNED | EAF_UNUSED))
    return;

  /* If return value is SSA name determine its flags.  */
  if (TREE_CODE (gimple_call_lhs (call)) == SSA_NAME)
    {
      tree lhs = gimple_call_lhs (call);
      analyze_ssa_name_flags (lhs, lattice, depth + 1, ipa);
      if (deref)
	lattice[index].merge_deref (lattice[SSA_NAME_VERSION (lhs)], false);
      else
	lattice[index].merge (lattice[SSA_NAME_VERSION (lhs)]);
    }
  /* In the case of memory store we can do nothing.  */
  else
    lattice[index].merge (0);
}

/* Analyze EAF flags for SSA name NAME and store result to LATTICE.
   LATTICE is an array of modref_lattices.
   DEPTH is a recursion depth used to make debug output prettier.
   If IPA is true we analyze for IPA propagation (and thus call escape points
   are processed later)  */

static void
analyze_ssa_name_flags (tree name, vec<modref_lattice> &lattice, int depth,
			bool ipa)
{
  imm_use_iterator ui;
  gimple *use_stmt;
  int index = SSA_NAME_VERSION (name);

  /* See if value is already computed.  */
  if (lattice[index].known)
   return;
  if (lattice[index].open)
    {
      if (dump_file)
	fprintf (dump_file,
		 "%*sGiving up on a cycle in SSA graph\n", depth * 4, "");
      return;
    }
  if (depth == param_modref_max_depth)
    {
      if (dump_file)
	fprintf (dump_file,
		 "%*sGiving up on max depth\n", depth * 4, "");
      return;
    }
  /* Recursion guard.  */
  lattice[index].init ();

  if (dump_file)
    {
      fprintf (dump_file,
	       "%*sAnalyzing flags of ssa name: ", depth * 4, "");
      print_generic_expr (dump_file, name);
      fprintf (dump_file, "\n");
    }

  FOR_EACH_IMM_USE_STMT (use_stmt, ui, name)
    {
      if (lattice[index].flags == 0)
	break;
      if (is_gimple_debug (use_stmt))
	continue;
      if (dump_file)
	{
	  fprintf (dump_file, "%*s  Analyzing stmt: ", depth * 4, "");
	  print_gimple_stmt (dump_file, use_stmt, 0);
	}
      /* If we see a direct non-debug use, clear unused bit.
	 All dereferneces should be accounted below using deref_flags.  */
      lattice[index].merge (~EAF_UNUSED);

      /* Gimple return may load the return value.
	 Returning name counts as an use by tree-ssa-structalias.c  */
      if (greturn *ret = dyn_cast <greturn *> (use_stmt))
	{
	  if (gimple_return_retval (ret) == name)
	    lattice[index].merge (~(EAF_UNUSED | EAF_NOT_RETURNED));
	  else if (memory_access_to (gimple_return_retval (ret), name))
	    {
	      lattice[index].merge_direct_load ();
	      lattice[index].merge (~(EAF_UNUSED | EAF_NOT_RETURNED));
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
	    lattice[index].merge (~(EAF_NOCLOBBER | EAF_UNUSED));

	  /* Recursion would require bit of propagation; give up for now.  */
	  if (callee && !ipa && recursive_call_p (current_function_decl,
						  callee))
	    lattice[index].merge (0);
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
		  lattice[index].merge_direct_store ();
		  /* Return slot optimization passes address of
		     LHS to callee via hidden parameter and this
		     may make LHS to escape.  See PR 98499.  */
		  if (gimple_call_return_slot_opt_p (call)
		      && TREE_ADDRESSABLE (TREE_TYPE (gimple_call_lhs (call))))
		    lattice[index].merge (EAF_NOREAD | EAF_DIRECT);
		}

	      /* We do not track accesses to the static chain (we could)
		 so give up.  */
	      if (gimple_call_chain (call)
		  && (gimple_call_chain (call) == name))
		lattice[index].merge (0);

	      /* Process internal functions and right away.  */
	      bool record_ipa = ipa && !gimple_call_internal_p (call);

	      /* Handle all function parameters.  */
	      for (unsigned i = 0;
		   i < gimple_call_num_args (call) && lattice[index].flags; i++)
		/* Name is directly passed to the callee.  */
		if (gimple_call_arg (call, i) == name)
		  {
		    if (!(ecf_flags & (ECF_CONST | ECF_NOVOPS)))
		      {
			int call_flags = gimple_call_arg_flags (call, i)
					 | EAF_NOT_RETURNED;
			if (ignore_stores)
			  call_flags |= ignore_stores_eaf_flags;

			if (!record_ipa)
			  lattice[index].merge (call_flags);
			else
			  lattice[index].add_escape_point (call, i,
			     				   call_flags, true);
		      }
		    if (!ignore_retval)
		      merge_call_lhs_flags (call, i, index, false,
					    lattice, depth, ipa);
		  }
		/* Name is dereferenced and passed to a callee.  */
		else if (memory_access_to (gimple_call_arg (call, i), name))
		  {
		    if (ecf_flags & (ECF_CONST | ECF_NOVOPS))
		      lattice[index].merge_direct_load ();
		    else
		      {
			int call_flags = deref_flags
			   (gimple_call_arg_flags (call, i)
			    | EAF_NOT_RETURNED, ignore_stores);
			if (!record_ipa)
			  lattice[index].merge (call_flags);
			else
			  lattice[index].add_escape_point (call, i,
							   call_flags, false);
		      }
		    if (!ignore_retval)
		      merge_call_lhs_flags (call, i, index, true,
					    lattice, depth, ipa);
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
		lattice[index].merge (0);
	      /* Handle *name = *exp.  */
	      else if (memory_access_to (gimple_assign_lhs (assign), name))
		lattice[index].merge_direct_store ();
	    }
	  /* Handle lhs = *name.  */
	  else if (memory_access_to (gimple_assign_rhs1 (assign), name))
	    {
	      tree lhs = gimple_assign_lhs (assign);
	      analyze_ssa_name_flags (lhs, lattice, depth + 1, ipa);
	      lattice[index].merge_deref (lattice[SSA_NAME_VERSION (lhs)],
					  false);
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
			 depth * 4, "");
	      lattice[index].merge (0);
	    }
	  /* Handle *name = exp.  */
	  else if (assign
		   && memory_access_to (gimple_assign_lhs (assign), name))
	    {
	      /* In general we can not ignore clobbers because they are
		 barriers for code motion, however after inlining it is safe to
		 do because local optimization passes do not consider clobbers
		 from other functions.  Similar logic is in ipa-pure-const.c.  */
	      if (!cfun->after_inlining || !gimple_clobber_p (assign))
		lattice[index].merge_direct_store ();
	    }
	  /* ASM statements etc.  */
	  else if (!assign)
	    {
	      if (dump_file)
		fprintf (dump_file, "%*s  Unhandled store\n",
			 depth * 4, "");
	      lattice[index].merge (0);
	    }
	}
      else if (gassign *assign = dyn_cast <gassign *> (use_stmt))
	{
	  enum tree_code code = gimple_assign_rhs_code (assign);

	  /* See if operation is a merge as considered by
	     tree-ssa-structalias.c:find_func_aliases.  */
	  if (!truth_value_p (code)
	      && code != POINTER_DIFF_EXPR
	      && (code != POINTER_PLUS_EXPR
		  || gimple_assign_rhs1 (assign) == name))
	    {
	      tree lhs = gimple_assign_lhs (assign);
	      analyze_ssa_name_flags (lhs, lattice, depth + 1, ipa);
	      lattice[index].merge (lattice[SSA_NAME_VERSION (lhs)]);
	    }
	}
      else if (gphi *phi = dyn_cast <gphi *> (use_stmt))
	{
	  tree result = gimple_phi_result (phi);
	  analyze_ssa_name_flags (result, lattice, depth + 1, ipa);
	  lattice[index].merge (lattice[SSA_NAME_VERSION (result)]);
	}
      /* Conditions are not considered escape points
	 by tree-ssa-structalias.  */
      else if (gimple_code (use_stmt) == GIMPLE_COND)
	;
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "%*s  Unhandled stmt\n", depth * 4, "");
	  lattice[index].merge (0);
	}

      if (dump_file)
	{
	  fprintf (dump_file, "%*s  current flags of ", depth * 4, "");
	  print_generic_expr (dump_file, name);
	  lattice[index].dump (dump_file, depth * 4 + 4);
	}
    }
  if (dump_file)
    {
      fprintf (dump_file, "%*sflags of ssa name ", depth * 4, "");
      print_generic_expr (dump_file, name);
      lattice[index].dump (dump_file, depth * 4 + 2);
    }
  lattice[index].open = false;
  lattice[index].known = true;
}

/* Determine EAF flags for function parameters.  */

static void
analyze_parms (modref_summary *summary, modref_summary_lto *summary_lto,
	       bool ipa)
{
  unsigned int parm_index = 0;
  unsigned int count = 0;
  int ecf_flags = flags_from_decl_or_type (current_function_decl);

  /* For novops functions we have nothing to gain by EAF flags.  */
  if (ecf_flags & ECF_NOVOPS)
    return;

  for (tree parm = DECL_ARGUMENTS (current_function_decl); parm;
       parm = TREE_CHAIN (parm))
    count++;

  if (!count)
    return;

  auto_vec<modref_lattice> lattice;
  lattice.safe_grow_cleared (num_ssa_names, true);

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
	  else if (summary_lto)
	    {
	      if (parm_index >= summary_lto->arg_flags.length ())
		summary_lto->arg_flags.safe_grow_cleared (count, true);
	      summary_lto->arg_flags[parm_index] = EAF_UNUSED;
	    }
	  continue;
	}
      analyze_ssa_name_flags (name, lattice, 0, ipa);
      int flags = lattice[SSA_NAME_VERSION (name)].flags;

      /* Eliminate useless flags so we do not end up storing unnecessary
	 summaries.  */

      flags = remove_useless_eaf_flags
		 (flags, ecf_flags,
		  VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))));

      if (flags)
	{
	  if (summary)
	    {
	      if (parm_index >= summary->arg_flags.length ())
		summary->arg_flags.safe_grow_cleared (count, true);
	      summary->arg_flags[parm_index] = flags;
	    }
	  else if (summary_lto)
	    {
	      if (parm_index >= summary_lto->arg_flags.length ())
		summary_lto->arg_flags.safe_grow_cleared (count, true);
	      summary_lto->arg_flags[parm_index] = flags;
	    }
	  if (lattice[SSA_NAME_VERSION (name)].escape_points.length ())
	    {
	      escape_point *ep;
	      unsigned int ip;
	      cgraph_node *node = cgraph_node::get (current_function_decl);

	      gcc_checking_assert (ipa);
	      FOR_EACH_VEC_ELT
		  (lattice[SSA_NAME_VERSION (name)].escape_points, ip, ep)
		if ((ep->min_flags & flags) != flags)
		  {
		    cgraph_edge *e = node->get_edge (ep->call);
		    struct escape_entry ee = {parm_index, ep->arg,
		     			      ep->min_flags, ep->direct};

		    escape_summaries->get_create (e)->esc.safe_push (ee);
		  }
	    }
	}
    }
  if (ipa)
    for (unsigned int i = 0; i < num_ssa_names; i++)
      lattice[i].release ();
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
  if (!flag_ipa_modref
      || lookup_attribute ("noipa", DECL_ATTRIBUTES (current_function_decl)))
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
  /* In IPA mode we analyze every function precisely once.  Assert that.  */
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
      summary->loads = modref_records::create_ggc (param_modref_max_bases,
						   param_modref_max_refs,
						   param_modref_max_accesses);
      gcc_assert (!summary->stores);
      summary->stores = modref_records::create_ggc (param_modref_max_bases,
						    param_modref_max_refs,
						    param_modref_max_accesses);
      summary->writes_errno = false;
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
      summary_lto->writes_errno = false;
    }

  analyze_parms (summary, summary_lto, ipa);

  int ecf_flags = flags_from_decl_or_type (current_function_decl);
  auto_vec <gimple *, 32> recursive_calls;

  /* Analyze each statement in each basic block of the function.  If the
     statement cannot be analyzed (for any reason), the entire function cannot
     be analyzed by modref.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, f)
    {
      gimple_stmt_iterator si;
      for (si = gsi_start_nondebug_after_labels_bb (bb);
	   !gsi_end_p (si); gsi_next_nondebug (&si))
	{
	  if (!analyze_stmt (summary, summary_lto,
			     gsi_stmt (si), ipa, &recursive_calls)
	      || ((!summary || !summary->useful_p (ecf_flags, false))
		  && (!summary_lto
		      || !summary_lto->useful_p (ecf_flags, false))))
	    {
	      collapse_loads (summary, summary_lto);
	      collapse_stores (summary, summary_lto);
	      break;
	    }
	}
    }

  /* In non-IPA mode we need to perform iterative datafow on recursive calls.
     This needs to be done after all other side effects are computed.  */
  if (!ipa)
    {
      bool changed = true;
      bool first = true;
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
			   fnode, !first);
	      if (!summary->useful_p (ecf_flags, false))
		{
		  remove_summary (lto, nolto, ipa);
		  return;
		}
	    }
	  first = false;
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
  if (!DECL_STRUCT_FUNCTION (node->decl)
      || !opt_for_fn (node->decl, flag_ipa_modref))
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
      || !opt_for_fn (node->decl, flag_ipa_modref)
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
  /* Do not duplicate optimization summaries; we do not handle parameter
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
  dst_data->writes_errno = src_data->writes_errno;
  if (src_data->arg_flags.length ())
    dst_data->arg_flags = src_data->arg_flags.copy ();
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
  dst_data->writes_errno = src_data->writes_errno;
  if (src_data->arg_flags.length ())
    dst_data->arg_flags = src_data->arg_flags.copy ();
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
						     : 0, 0);
      if (lto_ret)
	lto_base_node = (*lto_ret)->insert_base (base_tree, 0);
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
				      parm_index, parm_offset_known, false};
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
      bp_pack_var_len_unsigned (bp, ee->parm_index);
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
      ee.parm_index = bp_unpack_var_len_unsigned (bp);
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

	  write_modref_records (r->loads, ob);
	  write_modref_records (r->stores, ob);

	  struct bitpack_d bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, r->writes_errno, 1);
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

      if (modref_sum)
	modref_sum->writes_errno = false;
      if (modref_sum_lto)
	modref_sum_lto->writes_errno = false;

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
      read_modref_records (&ib, data_in,
			   modref_sum ? &modref_sum->loads : NULL,
			   modref_sum_lto ? &modref_sum_lto->loads : NULL);
      read_modref_records (&ib, data_in,
			   modref_sum ? &modref_sum->stores : NULL,
			   modref_sum_lto ? &modref_sum_lto->stores : NULL);
      struct bitpack_d bp = streamer_read_bitpack (&ib);
      if (bp_unpack_value (&bp, 1))
	{
	  if (modref_sum)
	    modref_sum->writes_errno = true;
	  if (modref_sum_lto)
	    modref_sum_lto->writes_errno = true;
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

/* If signature changed, update the summary.  */

static void
update_signature (struct cgraph_node *node)
{
  clone_info *info = clone_info::get (node);
  if (!info || !info->param_adjustments)
    return;

  modref_summary *r = optimization_summaries
		      ? optimization_summaries->get (node) : NULL;
  modref_summary_lto *r_lto = summaries_lto
			      ? summaries_lto->get (node) : NULL;
  if (!r && !r_lto)
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
    map.quick_push (-1);
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
      if (r->arg_flags.length ())
	remap_arg_flags (r->arg_flags, info);
    }
  if (r_lto)
    {
      r_lto->loads->remap_params (&map);
      r_lto->stores->remap_params (&map);
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
	 = callee_edge->callee->function_or_virtual_thunk_symbol
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

/* Update escape map fo E.  */

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
      if (ee->parm_index >= map.length ())
	continue;
      FOR_EACH_VEC_ELT (map[ee->parm_index], j, em)
	{
	  int min_flags = ee->min_flags;
	  if (ee->direct && !em->direct)
	    min_flags = deref_flags (min_flags, ignore_stores);
	  struct escape_entry entry = {em->parm_index, ee->arg,
	    			       ee->min_flags,
				       ee->direct & em->direct};
	  sum->esc.safe_push (entry);
	}
    }
  if (!sum->esc.length ())
    escape_summaries->remove (e);
}

/* Update escape map fo NODE.  */

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
  bool ignore_stores = ignore_stores_p (edge->caller->decl, flags);

  if (!callee_info && to_info)
    {
      if (!(flags & (ECF_CONST | ECF_NOVOPS)))
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
  if (callee_info || callee_info_lto)
    {
      auto_vec <modref_parm_map, 32> parm_map;

      compute_parm_map (edge, &parm_map);

      if (!ignore_stores)
	{
	  if (to_info && callee_info)
	    to_info->stores->merge (callee_info->stores, &parm_map, false);
	  if (to_info_lto && callee_info_lto)
	    to_info_lto->stores->merge (callee_info_lto->stores, &parm_map,
					false);
	}
      if (!(flags & (ECF_CONST | ECF_NOVOPS)))
	{
	  if (to_info && callee_info)
	    to_info->loads->merge (callee_info->loads, &parm_map, false);
	  if (to_info_lto && callee_info_lto)
	    to_info_lto->loads->merge (callee_info_lto->loads, &parm_map,
				       false);
	}
    }

  /* Now merge escape summaries.
     For every escape to the callee we need to merge calle flags
     and remap calees escapes.  */
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
	if (to_info && to_info->arg_flags.length () > ee->parm_index)
	  {
	    int flags = callee_info
			&& callee_info->arg_flags.length () > ee->arg
			? callee_info->arg_flags[ee->arg] : 0;
	    if (!ee->direct)
	      flags = deref_flags (flags, ignore_stores);
	    else if (ignore_stores)
	      flags |= ignore_stores_eaf_flags;
	    flags |= ee->min_flags;
	    to_info->arg_flags[ee->parm_index] &= flags;
	    if (to_info->arg_flags[ee->parm_index])
	      needed = true;
	  }
	if (to_info_lto && to_info_lto->arg_flags.length () > ee->parm_index)
	  {
	    int flags = callee_info_lto
			&& callee_info_lto->arg_flags.length () > ee->arg
			? callee_info_lto->arg_flags[ee->arg] : 0;
	    if (!ee->direct)
	      flags = deref_flags (flags, ignore_stores);
	    else if (ignore_stores)
	      flags |= ignore_stores_eaf_flags;
	    flags |= ee->min_flags;
	    to_info_lto->arg_flags[ee->parm_index] &= flags;
	    if (to_info_lto->arg_flags[ee->parm_index])
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
	}
      else if (to_info_lto && dump_file)
	{
	  if (dump_file)
	    fprintf (dump_file, "Updated mod-ref summary for %s\n",
		     to->dump_name ());
	  to_info_lto->dump (dump_file);
	  to_info_lto = NULL;
	}
      if (callee_info_lto)
	summaries_lto->remove (edge->callee);
    }
  if (!to_info && !to_info_lto)
    remove_modref_edge_summaries (to);
  return;
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

/* Call E in NODE with ECF_FLAGS has no summary; update MODREF_SUMMARY and
   CUR_SUMMARY_LTO accordingly.  Return true if something changed.  */

static bool
propagate_unknown_call (cgraph_node *node,
			cgraph_edge *e, int ecf_flags,
			modref_summary *cur_summary,
			modref_summary_lto *cur_summary_lto)
{
  bool changed = false;
  class fnspec_summary *fnspec_sum = fnspec_summaries->get (e);
  auto_vec <modref_parm_map, 32> parm_map;
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
	      if (map.parm_index == -2)
		continue;
	      if (map.parm_index == -1)
		{
		  collapse_loads (cur_summary, cur_summary_lto);
		  break;
		}
	      if (cur_summary)
		changed |= cur_summary->loads->insert
		  (0, 0, get_access_for_fnspec (e, fnspec, i, map), false);
	      if (cur_summary_lto)
		changed |= cur_summary_lto->loads->insert
		  (0, 0, get_access_for_fnspec (e, fnspec, i, map), false);
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
	      if (map.parm_index == -2)
		continue;
	      if (map.parm_index == -1)
		{
		  collapse_stores (cur_summary, cur_summary_lto);
		  break;
		}
	      if (cur_summary)
		changed |= cur_summary->stores->insert
		  (0, 0, get_access_for_fnspec (e, fnspec, i, map), false);
	      if (cur_summary_lto)
		changed |= cur_summary_lto->stores->insert
		  (0, 0, get_access_for_fnspec (e, fnspec, i, map), false);
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

/* Maybe remove summaies of NODE pointed to by CUR_SUMMARY_PTR
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

static void
modref_propagate_in_scc (cgraph_node *component_node)
{
  bool changed = true;
  bool first = true;
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

	  int cur_ecf_flags = flags_from_decl_or_type (node->decl);

	  if (dump_file)
	    fprintf (dump_file, "  Processing %s%s%s\n",
		     cur->dump_name (),
		     TREE_READONLY (cur->decl) ? " (const)" : "",
		     DECL_PURE_P (cur->decl) ? " (pure)" : "");

	  for (cgraph_edge *e = cur->indirect_calls; e; e = e->next_callee)
	    {
	      if (e->indirect_info->ecf_flags & (ECF_CONST | ECF_NOVOPS))
		continue;
	      if (dump_file)
		fprintf (dump_file, "    Indirect call"
			 "collapsing loads\n");
	      if (propagate_unknown_call
			   (node, e, e->indirect_info->ecf_flags,
			    cur_summary, cur_summary_lto))
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
		  if (dump_file)
		    fprintf (dump_file, "      Call target interposable"
			     " or not available\n");
		  changed |= propagate_unknown_call
			       (node, callee_edge, flags,
				cur_summary, cur_summary_lto);
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
				cur_summary, NULL);
		}
	      if (cur_summary_lto
		  && !(callee_summary_lto = summaries_lto->get (callee)))
		{
		  if (dump_file)
		    fprintf (dump_file, "      No call target summary\n");
		  changed |= propagate_unknown_call
			       (node, callee_edge, flags,
				NULL, cur_summary_lto);
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
				  (callee_summary->loads, &parm_map, !first);
		  if (!ignore_stores)
		    {
		      changed |= cur_summary->stores->merge
				      (callee_summary->stores, &parm_map,
				       !first);
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
				  (callee_summary_lto->loads, &parm_map,
				   !first);
		  if (!ignore_stores)
		    {
		      changed |= cur_summary_lto->stores->merge
				      (callee_summary_lto->stores, &parm_map,
				       !first);
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
			      int ecf_flags)
{
  escape_entry *ee;
  unsigned int i;
  bool changed = false;
  bool ignore_stores = ignore_stores_p (caller, ecf_flags);

  /* If we have no useful info to propagate.  */
  if ((!cur_summary || !cur_summary->arg_flags.length ())
      && (!cur_summary_lto || !cur_summary_lto->arg_flags.length ()))
    return false;

  FOR_EACH_VEC_ELT (sum->esc, i, ee)
    {
      int flags = 0;
      int flags_lto = 0;

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
      else if (ignore_stores)
	{
	  flags |= ignore_stores_eaf_flags;
	  flags_lto |= ignore_stores_eaf_flags;
	}
      /* Returning the value is already accounted to at local propagation.  */
      flags |= ee->min_flags | EAF_NOT_RETURNED;
      flags_lto |= ee->min_flags | EAF_NOT_RETURNED;
      /* Noescape implies that value also does not escape directly.
	 Fnspec machinery does set both so compensate for this.  */
      if (flags & EAF_NOESCAPE)
	flags |= EAF_NODIRECTESCAPE;
      if (flags_lto & EAF_NOESCAPE)
	flags_lto |= EAF_NODIRECTESCAPE;
      if (!(flags & EAF_UNUSED)
	  && cur_summary && ee->parm_index < cur_summary->arg_flags.length ())
	{
	  int f = cur_summary->arg_flags[ee->parm_index];
	  if ((f & flags) != f)
	    {
	      f = remove_useless_eaf_flags
			 (f & flags, ecf_flags,
			  VOID_TYPE_P (TREE_TYPE (TREE_TYPE (caller))));
	      cur_summary->arg_flags[ee->parm_index] = f;
	      changed = true;
	    }
	}
      if (!(flags_lto & EAF_UNUSED)
	  && cur_summary_lto
	  && ee->parm_index < cur_summary_lto->arg_flags.length ())
	{
	  int f = cur_summary_lto->arg_flags[ee->parm_index];
	  if ((f & flags_lto) != f)
	    {
	      f = remove_useless_eaf_flags
			 (f & flags_lto, ecf_flags,
			  VOID_TYPE_P (TREE_TYPE (TREE_TYPE (caller))));
	      cur_summary_lto->arg_flags[ee->parm_index] = f;
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

	  if (dump_file)
	    fprintf (dump_file, "  Processing %s%s%s\n",
		     cur->dump_name (),
		     TREE_READONLY (cur->decl) ? " (const)" : "",
		     DECL_PURE_P (cur->decl) ? " (pure)" : "");

	  for (cgraph_edge *e = cur->indirect_calls; e; e = e->next_callee)
	    {
	      escape_summary *sum = escape_summaries->get (e);

	      if (!sum || (e->indirect_info->ecf_flags
			   & (ECF_CONST | ECF_NOVOPS)))
		continue;

	      changed |= modref_merge_call_site_flags
				(sum, cur_summary, cur_summary_lto,
				 NULL, NULL,
				 node->decl, e->indirect_info->ecf_flags);
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

	      if (ecf_flags & (ECF_CONST | ECF_NOVOPS)
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
				 node->decl, ecf_flags);
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
      modref_propagate_flags_in_scc (component_node);
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
  return 0;
}

/* Summaries must stay alive until end of compilation.  */

void
ipa_modref_c_finalize ()
{
  if (optimization_summaries)
    ggc_delete (optimization_summaries);
  optimization_summaries = NULL;
  gcc_checking_assert (!summaries
		       || flag_incremental_link == INCREMENTAL_LINK_LTO);
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

#include "gt-ipa-modref.h"
