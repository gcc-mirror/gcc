/* Interprocedural scalar replacement of aggregates
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
   Contributed by Martin Jambor <mjambor@suse.cz>

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

/* IPA-SRA is an interprocedural pass that removes unused function return
   values (turning functions returning a value which is never used into void
   functions) and removes unused function parameters.  It can also replace an
   aggregate parameter by a set of other parameters representing part of the
   original, turning those passed by reference into new ones which pass the
   value directly.

   The pass is a true IPA one, which means that it works in three stages in
   order to be able to take advantage of LTO.  First, summaries about functions
   and each calls are generated.  Function summaries (often called call graph
   node summaries) contain mainly information about which parameters are
   potential transformation candidates and which bits of candidates are
   accessed.  We differentiate between accesses done as a part of a call
   statement (which might be not necessary if the callee is also transformed)
   and others (which are mandatory).  Call summaries (often called call graph
   edge summaries) contain information about which function formal parameters
   feed into which actual call arguments so that if two parameters are only
   used in a sum which is then passed to another function which then however
   does not use this parameter, all three parameters of the two functions can
   be eliminated.  Edge summaries also have flags whether the return value is
   used or if it is only returned in the caller too.  In LTO mode these
   summaries are then streamed to the object file in the compilation phase and
   streamed back in in the WPA analysis stage.

   The interprocedural analysis phase traverses the graph in topological order
   in two sweeps, one in each direction.  First, from callees to callers for
   parameter removal and splitting.  Each strongly-connected component is
   processed iteratively until the situation in it stabilizes.  The pass from
   callers to callees is then carried out to remove unused return values in a
   very similar fashion.

   Because parameter manipulation has big implications for call redirection
   which is done only after all call graph nodes materialize, the
   transformation phase is not part of this patch but is carried out by the
   clone materialization and edge redirection itself, see comments in
   ipa-param-manipulation.h for more details.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-dfa.h"
#include "tree-sra.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "dbgcnt.h"
#include "tree-inline.h"
#include "ipa-utils.h"
#include "builtins.h"
#include "cfganal.h"
#include "tree-streamer.h"
#include "internal-fn.h"
#include "symtab-clones.h"
#include "attribs.h"
#include "ipa-prop.h"

static void ipa_sra_summarize_function (cgraph_node *);

/* Bits used to track size of an aggregate in bytes interprocedurally.  */
#define ISRA_ARG_SIZE_LIMIT_BITS 16
#define ISRA_ARG_SIZE_LIMIT (1 << ISRA_ARG_SIZE_LIMIT_BITS)
/* How many parameters can feed into a call actual argument and still be
   tracked.  */
#define IPA_SRA_MAX_PARAM_FLOW_LEN 7

/* Structure describing accesses to a specific portion of an aggregate
   parameter, as given by the offset and size.  Any smaller accesses that occur
   within a function that fall within another access form a tree.  The pass
   cannot analyze parameters with only partially overlapping accesses.  */

struct GTY(()) param_access
{
  /* Type that a potential replacement should have.  This field only has
     meaning in the summary building and transformation phases, when it is
     reconstructed from the body.  Must not be touched in IPA analysis
     stage.  */
  tree type;

  /* Alias reference type to be used in MEM_REFs when adjusting caller
     arguments.  */
  tree alias_ptr_type;

  /* Values returned by get_ref_base_and_extent but converted to bytes and
     stored as unsigned ints.  */
  unsigned unit_offset;
  unsigned unit_size : ISRA_ARG_SIZE_LIMIT_BITS;

  /* Set once we are sure that the access will really end up in a potentially
     transformed function - initially not set for portions of formal parameters
     that are only used as actual function arguments passed to callees.  */
  unsigned certain : 1;
  /* Set if the access has reverse scalar storage order.  */
  unsigned reverse : 1;
};

/* This structure has the same purpose as the one above and additionally it
   contains some fields that are only necessary in the summary generation
   phase.  */

struct gensum_param_access
{
  /* Values returned by get_ref_base_and_extent.  */
  HOST_WIDE_INT offset;
  HOST_WIDE_INT size;

  /* if this access has any children (in terms of the definition above), this
     points to the first one.  */
  struct gensum_param_access *first_child;
  /* In intraprocedural SRA, pointer to the next sibling in the access tree as
     described above.  */
  struct gensum_param_access *next_sibling;

  /* Type that a potential replacement should have.  This field only has
     meaning in the summary building and transformation phases, when it is
     reconstructed from the body.  Must not be touched in IPA analysis
     stage.  */
  tree type;
  /* Alias reference type to be used in MEM_REFs when adjusting caller
     arguments.  */
  tree alias_ptr_type;

  /* Cumulative count of all loads. */
  profile_count load_count;
  /* Have there been writes to or reads from this exact location except for as
     arguments to a function call that can be tracked.  */
  bool nonarg;

  /* Set if the access has reverse scalar storage order.  */
  bool reverse;
};

/* Summary describing a parameter in the IPA stages.  */

struct GTY(()) isra_param_desc
{
  /* List of access representatives to the parameters, sorted according to
     their offset.  */
  vec <param_access *, va_gc> *accesses;

  /* Unit size limit of total size of all replacements.  */
  unsigned param_size_limit : ISRA_ARG_SIZE_LIMIT_BITS;
  /* Sum of unit sizes of all certain replacements.  */
  unsigned size_reached : ISRA_ARG_SIZE_LIMIT_BITS;
  /* Minimum offset that is known to be safe to dereference because of callers
     pass pointers to DECLs of at least this size or because of dereferences in
     callers.  */
  unsigned safe_size : ISRA_ARG_SIZE_LIMIT_BITS;

  /* A parameter that is used only in call arguments and can be removed if all
     concerned actual arguments are removed.  */
  unsigned locally_unused : 1;
  /* An aggregate that is a candidate for breaking up or complete removal.  */
  unsigned split_candidate : 1;
  /* Is this a parameter passing stuff by reference?  */
  unsigned by_ref : 1;
  /* Parameter hint set during IPA analysis when there is a caller which does
     not construct the argument just to pass it to calls.  Only meaningful for
     by_ref parameters.  */
  unsigned not_specially_constructed : 1;
  /* Only meaningful for by_ref parameters.  If set, this parameter can only be
     a split candidate if all callers pass pointers that are known to point to
     a chunk of memory large enough to contain all accesses.  */
  unsigned conditionally_dereferenceable : 1;
  /* Set when safe_size has been updated from at least one caller.  */
  unsigned safe_size_set : 1;
};

/* Structure used when generating summaries that describes a parameter.  */

struct gensum_param_desc
{
  /* Roots of param_accesses.  */
  gensum_param_access *accesses;
  /* Number of accesses in the access tree rooted in field accesses.  */
  unsigned access_count;

  /* If the below is non-zero, this is the number of uses as actual arguments.  */
  int call_uses;
  /* Number of times this parameter has been directly passed to.  */
  unsigned ptr_pt_count;

  /* Size limit of total size of all replacements.  */
  unsigned param_size_limit;
  /* Sum of sizes of nonarg accesses.  */
  unsigned nonarg_acc_size;

  /* A parameter that is used only in call arguments and can be removed if all
     concerned actual arguments are removed.  */
  bool locally_unused;
  /* An aggregate that is a candidate for breaking up or a pointer passing data
     by reference that is a candidate for being converted to a set of
     parameters passing those data by value.  */
  bool split_candidate;
  /* Is this a parameter passing stuff by reference (either a pointer or a
     source language reference type)?  */
  bool by_ref;
  /* If this parameter passes stuff by reference, can it be safely dereferenced
     without performing further checks (for example because it is a
     REFERENCE_TYPE)?  */
  bool safe_ref;
  /* Only meaningful for by_ref parameters.  If set, this parameter can only be
     a split candidate if all callers pass pointers that are known to point to
     a chunk of memory large enough to contain all accesses.  */
  bool conditionally_dereferenceable;

  /* The number of this parameter as they are ordered in function decl.  */
  int param_number;
  /* For parameters passing data by reference, this is parameter index to
     compute indices to bb_dereferences.  */
  int deref_index;
};

/* Properly deallocate accesses of DESC.  TODO: Since this data structure is
   allocated in GC memory, this is not necessary and we can consider removing
   the function.  */

static void
free_param_decl_accesses (isra_param_desc *desc)
{
  unsigned len = vec_safe_length (desc->accesses);
  for (unsigned i = 0; i < len; ++i)
    ggc_free ((*desc->accesses)[i]);
  vec_free (desc->accesses);
}

/* Class used to convey information about functions from the
   intra-procedural analysis stage to inter-procedural one.  */

class GTY((for_user)) isra_func_summary
{
public:
  /* initialize the object.  */

  isra_func_summary ()
    : m_parameters (NULL), m_candidate (false), m_returns_value (false),
    m_return_ignored (false), m_queued (false)
  {}

  /* Destroy m_parameters.  */

  ~isra_func_summary ();

  /* Mark the function as not a candidate for any IPA-SRA transformation.
     Return true if it was a candidate until now.  */

  bool zap ();

  /* Vector of parameter descriptors corresponding to the function being
     analyzed.  */
  vec<isra_param_desc, va_gc> *m_parameters;

  /* Whether the node is even a candidate for any IPA-SRA transformation at
     all.  */
  unsigned m_candidate : 1;

  /* Whether the original function returns any value.  */
  unsigned m_returns_value : 1;

  /* Set to true if all call statements do not actually use the returned
     value.  */

  unsigned m_return_ignored : 1;

  /* Whether the node is already queued in IPA SRA stack during processing of
     call graphs SCCs.  */

  unsigned m_queued : 1;
};

/* Deallocate the memory pointed to by isra_func_summary.  TODO: Since this
   data structure is allocated in GC memory, this is not necessary and we can
   consider removing the destructor.  */

isra_func_summary::~isra_func_summary ()
{
  unsigned len = vec_safe_length (m_parameters);
  for (unsigned i = 0; i < len; ++i)
    free_param_decl_accesses (&(*m_parameters)[i]);
  vec_free (m_parameters);
}

/* Mark the function as not a candidate for any IPA-SRA transformation.  Return
   true if it was a candidate until now.  */

bool
isra_func_summary::zap ()
{
  bool ret = m_candidate;
  m_candidate = false;

  /* TODO: see the destructor above.  */
  unsigned len = vec_safe_length (m_parameters);
  for (unsigned i = 0; i < len; ++i)
    free_param_decl_accesses (&(*m_parameters)[i]);
  vec_free (m_parameters);

  return ret;
}

/* Structure to describe which formal parameters feed into a particular actual
   argument.  */

struct isra_param_flow
{
  /* Number of elements in array inputs that contain valid data.  */
  char length;
  /* Indices of formal parameters that feed into the described actual argument.
     If aggregate_pass_through or pointer_pass_through below are true, it must
     contain exactly one element which is passed through from a formal
     parameter if the given number.  Otherwise, the array contains indices of
     callee's formal parameters which are used to calculate value of this
     actual argument. */
  unsigned char inputs[IPA_SRA_MAX_PARAM_FLOW_LEN];

  /* Offset within the formal parameter.  */
  unsigned unit_offset;
  /* When aggregate_pass_through is set, this is the size of the portion of an
     aggregate formal parameter that is being passed.  Otherwise, this is size
     of pointed to memory that is known to be valid be dereferenced.  */
  unsigned unit_size : ISRA_ARG_SIZE_LIMIT_BITS;

  /* True when the value of this actual argument is a portion of a formal
     parameter.  */
  unsigned aggregate_pass_through : 1;
  /* True when the value of this actual copy is a verbatim pass through of an
     obtained pointer.  */
  unsigned pointer_pass_through : 1;
  /* True when it is safe to copy access candidates here from the callee, which
     would mean introducing dereferences into callers of the caller.  */
  unsigned safe_to_import_accesses : 1;
  /* True when the passed value is an address of a structure that has been
     constructed in the caller just to be passed by reference to functions
     (i.e. is never read).  */
  unsigned constructed_for_calls : 1;
};

/* Structure used to convey information about calls from the intra-procedural
   analysis stage to inter-procedural one.  */

class isra_call_summary
{
public:
  isra_call_summary ()
    : m_arg_flow (), m_return_ignored (false), m_return_returned (false),
      m_bit_aligned_arg (false), m_before_any_store (false)
  {}

  void init_inputs (unsigned arg_count);
  void dump (FILE *f);

  /* Information about what formal parameters of the caller are used to compute
     individual actual arguments of this call.  */
  auto_vec <isra_param_flow> m_arg_flow;

  /* Set to true if the call statement does not have a LHS.  */
  unsigned m_return_ignored : 1;

  /* Set to true if the LHS of call statement is only used to construct the
     return value of the caller.  */
  unsigned m_return_returned : 1;

  /* Set when any of the call arguments are not byte-aligned.  */
  unsigned m_bit_aligned_arg : 1;

  /* Set to true if the call happend before any (other) store to memory in the
     caller.  */
  unsigned m_before_any_store : 1;
};

/* Class to manage function summaries.  */

class GTY((user)) ipa_sra_function_summaries
  : public function_summary <isra_func_summary *>
{
public:
  ipa_sra_function_summaries (symbol_table *table, bool ggc):
    function_summary<isra_func_summary *> (table, ggc) { }

  void duplicate (cgraph_node *, cgraph_node *,
		  isra_func_summary *old_sum,
		  isra_func_summary *new_sum) final override;
  void insert (cgraph_node *, isra_func_summary *) final override;
};

/* Hook that is called by summary when a node is duplicated.  */

void
ipa_sra_function_summaries::duplicate (cgraph_node *, cgraph_node *,
				       isra_func_summary *old_sum,
				       isra_func_summary *new_sum)
{
  /* TODO: Somehow stop copying when ISRA is doing the cloning, it is
     useless.  */
  new_sum->m_candidate  = old_sum->m_candidate;
  new_sum->m_returns_value = old_sum->m_returns_value;
  new_sum->m_return_ignored = old_sum->m_return_ignored;
  gcc_assert (!old_sum->m_queued);
  new_sum->m_queued = false;

  unsigned param_count = vec_safe_length (old_sum->m_parameters);
  if (!param_count)
    return;
  vec_safe_reserve_exact (new_sum->m_parameters, param_count);
  new_sum->m_parameters->quick_grow_cleared (param_count);
  for (unsigned i = 0; i < param_count; i++)
    {
      isra_param_desc *s = &(*old_sum->m_parameters)[i];
      isra_param_desc *d = &(*new_sum->m_parameters)[i];

      d->param_size_limit = s->param_size_limit;
      d->size_reached = s->size_reached;
      d->safe_size = s->safe_size;
      d->locally_unused = s->locally_unused;
      d->split_candidate = s->split_candidate;
      d->by_ref = s->by_ref;
      d->not_specially_constructed = s->not_specially_constructed;
      d->conditionally_dereferenceable = s->conditionally_dereferenceable;
      d->safe_size_set = s->safe_size_set;

      unsigned acc_count = vec_safe_length (s->accesses);
      vec_safe_reserve_exact (d->accesses, acc_count);
      for (unsigned j = 0; j < acc_count; j++)
	{
	  param_access *from = (*s->accesses)[j];
	  param_access *to = ggc_cleared_alloc<param_access> ();
	  to->type = from->type;
	  to->alias_ptr_type = from->alias_ptr_type;
	  to->unit_offset = from->unit_offset;
	  to->unit_size = from->unit_size;
	  to->certain = from->certain;
	  to->reverse = from->reverse;
	  d->accesses->quick_push (to);
	}
    }
}

/* Pointer to the pass function summary holder.  */

static GTY(()) ipa_sra_function_summaries *func_sums;

/* Hook that is called by summary when new node appears.  */

void
ipa_sra_function_summaries::insert (cgraph_node *node, isra_func_summary *)
{
  if (opt_for_fn (node->decl, flag_ipa_sra))
    {
      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
      ipa_sra_summarize_function (node);
      pop_cfun ();
    }
  else
    func_sums->remove (node);
}

/* Class to manage call summaries.  */

class ipa_sra_call_summaries: public call_summary <isra_call_summary *>
{
public:
  ipa_sra_call_summaries (symbol_table *table):
    call_summary<isra_call_summary *> (table) { }

  /* Duplicate info when an edge is cloned.  */
  void duplicate (cgraph_edge *, cgraph_edge *,
		  isra_call_summary *old_sum,
		  isra_call_summary *new_sum) final override;
};

static ipa_sra_call_summaries *call_sums;


/* Initialize m_arg_flow of a particular instance of isra_call_summary.
   ARG_COUNT is the number of actual arguments passed.  */

void
isra_call_summary::init_inputs (unsigned arg_count)
{
  if (arg_count == 0)
    {
      gcc_checking_assert (m_arg_flow.length () == 0);
      return;
    }
  if (m_arg_flow.length () == 0)
    {
      m_arg_flow.reserve_exact (arg_count);
      m_arg_flow.quick_grow_cleared (arg_count);
    }
  else
    gcc_checking_assert (arg_count == m_arg_flow.length ());
}

/* Dump all information in call summary to F.  */

void
isra_call_summary::dump (FILE *f)
{
  if (m_return_ignored)
    fprintf (f, "    return value ignored\n");
  if (m_return_returned)
    fprintf (f, "    return value used only to compute caller return value\n");
  if (m_before_any_store)
    fprintf (f, "    happens before any store to memory\n");
  for (unsigned i = 0; i < m_arg_flow.length (); i++)
    {
      fprintf (f, "    Parameter %u:\n", i);
      isra_param_flow *ipf = &m_arg_flow[i];

      if (ipf->length)
	{
	  bool first = true;
	  fprintf (f, "      Scalar param sources: ");
	  for (int j = 0; j < ipf->length; j++)
	    {
	      if (!first)
		fprintf (f, ", ");
	      else
		first = false;
	      fprintf (f, "%i", (int) ipf->inputs[j]);
	    }
	  fprintf (f, "\n");
	}
      if (ipf->aggregate_pass_through)
	fprintf (f, "      Aggregate pass through from the param given above, "
		 "unit offset: %u , unit size: %u\n",
		 ipf->unit_offset, ipf->unit_size);
      else if (ipf->unit_size > 0)
	fprintf (f, "      Known dereferenceable size: %u\n", ipf->unit_size);
      if (ipf->pointer_pass_through)
	fprintf (f, "      Pointer pass through from the param given above, "
		 "safe_to_import_accesses: %u\n", ipf->safe_to_import_accesses);
      if (ipf->constructed_for_calls)
	fprintf (f, "      Variable constructed just to be passed to "
		 "calls.\n");
    }
}

/* Duplicate edge summary when an edge is cloned.  */

void
ipa_sra_call_summaries::duplicate (cgraph_edge *, cgraph_edge *,
				   isra_call_summary *old_sum,
				   isra_call_summary *new_sum)
{
  unsigned arg_count = old_sum->m_arg_flow.length ();
  new_sum->init_inputs (arg_count);
  for (unsigned i = 0; i < arg_count; i++)
    new_sum->m_arg_flow[i] = old_sum->m_arg_flow[i];

  new_sum->m_return_ignored = old_sum->m_return_ignored;
  new_sum->m_return_returned = old_sum->m_return_returned;
  new_sum->m_bit_aligned_arg = old_sum->m_bit_aligned_arg;
  new_sum->m_before_any_store = old_sum->m_before_any_store;
}


/* With all GTY stuff done, we can move to anonymous namespace.  */
namespace {
/* Quick mapping from a decl to its param descriptor.  */

hash_map<tree, gensum_param_desc *> *decl2desc;

/* All local DECLs ever loaded from of and of those that have their address
   assigned to a variable.  */

hash_set <tree> *loaded_decls;

/* Countdown of allowed Alias Analysis steps during summary building.  */

int aa_walking_limit;

/* This is a table in which for each basic block and parameter there is a
   distance (offset + size) in that parameter which is dereferenced and
   accessed in that BB.  */
HOST_WIDE_INT *bb_dereferences = NULL;
/* How many by-reference parameters there are in the current function.  */
int unsafe_by_ref_count;

/* Bitmap of BBs that can cause the function to "stop" progressing by
   returning, throwing externally, looping infinitely or calling a function
   which might abort etc.. */
bitmap final_bbs;

/* Obstack to allocate various small structures required only when generating
   summary for a function.  */
struct obstack gensum_obstack;

/* Return false the function is apparently unsuitable for IPA-SRA based on it's
   attributes, return true otherwise.  NODE is the cgraph node of the current
   function.  */

static bool
ipa_sra_preliminary_function_checks (cgraph_node *node)
{
  if (!node->can_change_signature)
    {
      if (dump_file)
	fprintf (dump_file, "Function cannot change signature.\n");
      return false;
    }

  if (!tree_versionable_function_p (node->decl))
    {
      if (dump_file)
	fprintf (dump_file, "Function is not versionable.\n");
      return false;
    }

  if (!opt_for_fn (node->decl, optimize)
      || !opt_for_fn (node->decl, flag_ipa_sra))
    {
      if (dump_file)
	fprintf (dump_file, "Not optimizing or IPA-SRA turned off for this "
		 "function.\n");
      return false;
    }

  if (DECL_VIRTUAL_P (node->decl))
    {
      if (dump_file)
	fprintf (dump_file, "Function is a virtual method.\n");
      return false;
    }

  struct function *fun = DECL_STRUCT_FUNCTION (node->decl);
  if (fun->stdarg)
    {
      if (dump_file)
	fprintf (dump_file, "Function uses stdarg. \n");
      return false;
    }

  if (DECL_DISREGARD_INLINE_LIMITS (node->decl))
    {
      if (dump_file)
	fprintf (dump_file, "Always inline function will be inlined "
		 "anyway. \n");
      return false;
    }

  return true;
}

/* Print access tree starting at ACCESS to F.  */

static void
dump_gensum_access (FILE *f, gensum_param_access *access, unsigned indent)
{
  fprintf (f, "  ");
  for (unsigned i = 0; i < indent; i++)
    fprintf (f, " ");
  fprintf (f, "    * Access to offset: " HOST_WIDE_INT_PRINT_DEC,
	   access->offset);
  fprintf (f, ", size: " HOST_WIDE_INT_PRINT_DEC, access->size);
  fprintf (f, ", type: ");
  print_generic_expr (f, access->type);
  fprintf (f, ", alias_ptr_type: ");
  print_generic_expr (f, access->alias_ptr_type);
  fprintf (f, ", load_count: ");
  access->load_count.dump (f);
  fprintf (f, ", nonarg: %u, reverse: %u\n", access->nonarg, access->reverse);
  for (gensum_param_access *ch = access->first_child;
       ch;
       ch = ch->next_sibling)
    dump_gensum_access (f, ch, indent + 2);
}


/* Print access tree starting at ACCESS to F.  */

static void
dump_isra_access (FILE *f, param_access *access)
{
  fprintf (f, "    * Access to unit offset: %u", access->unit_offset);
  fprintf (f, ", unit size: %u", access->unit_size);
  fprintf (f, ", type: ");
  print_generic_expr (f, access->type);
  fprintf (f, ", alias_ptr_type: ");
  print_generic_expr (f, access->alias_ptr_type);
  if (access->certain)
    fprintf (f, ", certain");
  else
    fprintf (f, ", not certain");
  if (access->reverse)
    fprintf (f, ", reverse");
  fprintf (f, "\n");
}

/* Dump access tree starting at ACCESS to stderr.  */

DEBUG_FUNCTION void
debug_isra_access (param_access *access)
{
  dump_isra_access (stderr, access);
}

/* Dump DESC to F.  */

static void
dump_gensum_param_descriptor (FILE *f, gensum_param_desc *desc)
{
  if (desc->locally_unused)
    fprintf (f, "    unused with %i call_uses\n", desc->call_uses);
  if (!desc->split_candidate)
    {
      fprintf (f, "    not a candidate\n");
      return;
    }
  if (desc->by_ref)
    fprintf (f, "    %s%s by_ref with %u pass throughs\n",
	     desc->safe_ref ? "safe" : "unsafe",
	     desc->conditionally_dereferenceable
	     ? " conditionally_dereferenceable" : " ok",
	     desc->ptr_pt_count);

  for (gensum_param_access *acc = desc->accesses; acc; acc = acc->next_sibling)
    dump_gensum_access (f, acc, 2);
}

/* Dump all parameter descriptors in IFS, assuming it describes FNDECL, to
   F.  */

static void
dump_gensum_param_descriptors (FILE *f, tree fndecl,
			       vec<gensum_param_desc> *param_descriptions)
{
  tree parm = DECL_ARGUMENTS (fndecl);
  for (unsigned i = 0;
       i < param_descriptions->length ();
       ++i, parm = DECL_CHAIN (parm))
    {
      fprintf (f, "  Descriptor for parameter %i ", i);
      print_generic_expr (f, parm, TDF_UID);
      fprintf (f, "\n");
      dump_gensum_param_descriptor (f, &(*param_descriptions)[i]);
    }
}


/* Dump DESC to F.  If HINTS is true, also dump IPA-analysis computed
   hints.  */

static void
dump_isra_param_descriptor (FILE *f, isra_param_desc *desc, bool hints)
{
  if (desc->locally_unused)
    {
      fprintf (f, "    (locally) unused\n");
    }
  if (!desc->split_candidate)
    {
      fprintf (f, "    not a candidate for splitting");
      if (hints && desc->by_ref && desc->safe_size_set)
	fprintf (f, ", safe_size: %u", (unsigned) desc->safe_size);
      fprintf (f, "\n");
      return;
    }
  fprintf (f, "    param_size_limit: %u, size_reached: %u%s",
	   desc->param_size_limit, desc->size_reached,
	   desc->by_ref ? ", by_ref" : "");
  if (desc->by_ref && desc->conditionally_dereferenceable)
    fprintf (f, ", conditionally_dereferenceable");
  if (hints)
    {
      if (desc->by_ref && !desc->not_specially_constructed)
	fprintf (f, ", args_specially_constructed");
      if (desc->by_ref && desc->safe_size_set)
	fprintf (f, ", safe_size: %u", (unsigned) desc->safe_size);
    }
  fprintf (f, "\n");

  for (unsigned i = 0; i < vec_safe_length (desc->accesses); ++i)
    {
      param_access *access = (*desc->accesses)[i];
      dump_isra_access (f, access);
    }
}

/* Dump all parameter descriptors in IFS, assuming it describes FNDECL, to F.
   If HINTS is true, also dump IPA-analysis computed hints.  */

static void
dump_isra_param_descriptors (FILE *f, tree fndecl, isra_func_summary *ifs,
			     bool hints)
{
  tree parm = DECL_ARGUMENTS (fndecl);
  if (!ifs->m_parameters)
    {
      fprintf (f, "  parameter descriptors not available\n");
      return;
    }

  for (unsigned i = 0;
       i < ifs->m_parameters->length ();
       ++i, parm = DECL_CHAIN (parm))
    {
      fprintf (f, "  Descriptor for parameter %i ", i);
      print_generic_expr (f, parm, TDF_UID);
      fprintf (f, "\n");
      dump_isra_param_descriptor (f, &(*ifs->m_parameters)[i], hints);
    }
}

/* Add SRC to inputs of PARAM_FLOW, unless it would exceed storage.  If the
   function fails return false, otherwise return true.  SRC must fit into an
   unsigned char.  Used for purposes of transitive unused parameter
   removal.  */

static bool
add_src_to_param_flow (isra_param_flow *param_flow, int src)
{
  gcc_checking_assert (src >= 0 && src <= UCHAR_MAX);
  if (param_flow->length == IPA_SRA_MAX_PARAM_FLOW_LEN)
    return false;

  param_flow->inputs[(int) param_flow->length] = src;
  param_flow->length++;
  return true;
}

/* Add a SRC to the inputs of PARAM_FLOW unless it is already there and assert
   it is the only input.  Used for purposes of transitive parameter
   splitting.  */

static void
set_single_param_flow_source (isra_param_flow *param_flow, int src)
{
  gcc_checking_assert (src >= 0 && src <= UCHAR_MAX);
  if (param_flow->length == 0)
    {
      param_flow->inputs[0] = src;
      param_flow->length = 1;
    }
  else if (param_flow->length == 1)
    gcc_assert (param_flow->inputs[0] == src);
  else
    gcc_unreachable ();
}

/* Assert that there is only a single value in PARAM_FLOW's inputs and return
   it.  */

static unsigned
get_single_param_flow_source (const isra_param_flow *param_flow)
{
  gcc_assert (param_flow->length == 1);
  return param_flow->inputs[0];
}

/* Inspect all uses of NAME and simple arithmetic calculations involving NAME
   in FUN represented with NODE and return a negative number if any of them is
   used for something else than either an actual call argument, simple
   arithmetic operation or debug statement.  If there are no such uses, return
   the number of actual arguments that this parameter eventually feeds to (or
   zero if there is none).  For any such parameter, mark PARM_NUM as one of its
   sources.  ANALYZED is a bitmap that tracks which SSA names we have already
   started investigating.  */

static int
isra_track_scalar_value_uses (function *fun, cgraph_node *node, tree name,
			      int parm_num, bitmap analyzed)
{
  int res = 0;
  imm_use_iterator imm_iter;
  gimple *stmt;

  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, name)
    {
      if (is_gimple_debug (stmt))
	continue;

      /* TODO: We could handle at least const builtin functions like arithmetic
	 operations below.  */
      if (is_gimple_call (stmt))
	{
	  int all_uses = 0;
	  use_operand_p use_p;
	  FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	    all_uses++;

	  gcall *call = as_a <gcall *> (stmt);
	  unsigned arg_count;
	  if (gimple_call_internal_p (call)
	      || (arg_count = gimple_call_num_args (call)) == 0)
	    {
	      res = -1;
	      break;
	    }

	  cgraph_edge *cs = node->get_edge (stmt);
	  gcc_checking_assert (cs);
	  isra_call_summary *csum = call_sums->get_create (cs);
	  csum->init_inputs (arg_count);

	  int simple_uses = 0;
	  for (unsigned i = 0; i < arg_count; i++)
	    if (gimple_call_arg (call, i) == name)
	      {
		if (!add_src_to_param_flow (&csum->m_arg_flow[i], parm_num))
		  {
		    simple_uses = -1;
		    break;
		  }
		simple_uses++;
	      }

	  if (simple_uses < 0
	      || all_uses != simple_uses)
	    {
	      res = -1;
	      break;
	    }
	  res += all_uses;
	}
      else if (!stmt_unremovable_because_of_non_call_eh_p (fun, stmt)
	       && ((is_gimple_assign (stmt) && !gimple_has_volatile_ops (stmt))
		   || gimple_code (stmt) == GIMPLE_PHI))
	{
	  tree lhs;
	  if (gimple_code (stmt) == GIMPLE_PHI)
	    lhs = gimple_phi_result (stmt);
	  else
	    lhs = gimple_assign_lhs (stmt);

	  if (TREE_CODE (lhs) != SSA_NAME)
	    {
	      res = -1;
	      break;
	    }
	  gcc_assert (!gimple_vdef (stmt));
	  if (bitmap_set_bit (analyzed, SSA_NAME_VERSION (lhs)))
	    {
	      int tmp = isra_track_scalar_value_uses (fun, node, lhs, parm_num,
						      analyzed);
	      if (tmp < 0)
		{
		  res = tmp;
		  break;
		}
	      res += tmp;
	    }
	}
      else
	{
	  res = -1;
	  break;
	}
    }
  return res;
}

/* Inspect all uses of PARM, which must be a gimple register, in FUN (which is
   also described by NODE) and simple arithmetic calculations involving PARM
   and return false if any of them is used for something else than either an
   actual call argument, simple arithmetic operation or debug statement.  If
   there are no such uses, return true and store the number of actual arguments
   that this parameter eventually feeds to (or zero if there is none) to
   *CALL_USES_P.  For any such parameter, mark PARM_NUM as one of its
   sources.

   This function is similar to ptr_parm_has_nonarg_uses but its results are
   meant for unused parameter removal, as opposed to splitting of parameters
   passed by reference or converting them to passed by value.  */

static bool
isra_track_scalar_param_local_uses (function *fun, cgraph_node *node, tree parm,
				    int parm_num, int *call_uses_p)
{
  gcc_checking_assert (is_gimple_reg (parm));

  tree name = ssa_default_def (fun, parm);
  if (!name || has_zero_uses (name))
    {
      *call_uses_p = 0;
      return false;
    }

  /* Edge summaries can only handle callers with fewer than 256 parameters.  */
  if (parm_num > UCHAR_MAX)
    return true;

  bitmap analyzed = BITMAP_ALLOC (NULL);
  int call_uses = isra_track_scalar_value_uses (fun, node, name, parm_num,
						analyzed);
  BITMAP_FREE (analyzed);
  if (call_uses < 0)
    return true;
  *call_uses_p = call_uses;
  return false;
}

/* Scan immediate uses of a default definition SSA name of a parameter PARM and
   examine whether there are any nonarg uses that are not actual arguments or
   otherwise infeasible uses.  If so, return true, otherwise return false.
   Create pass-through IPA flow records for any direct uses as argument calls
   and if returning false, store their number into *PT_COUNT_P.  NODE and FUN
   must represent the function that is currently analyzed, PARM_NUM must be the
   index of the analyzed parameter.

   This function is similar to isra_track_scalar_param_local_uses but its
   results are meant for splitting of parameters passed by reference or turning
   them into bits passed by value, as opposed to generic unused parameter
   removal.  */

static bool
ptr_parm_has_nonarg_uses (cgraph_node *node, function *fun, tree parm,
			  int parm_num, unsigned *pt_count_p)
{
  imm_use_iterator ui;
  gimple *stmt;
  tree name = ssa_default_def (fun, parm);
  bool ret = false;
  unsigned pt_count = 0;

  if (!name || has_zero_uses (name))
    return false;

  /* Edge summaries can only handle callers with fewer than 256 parameters.  */
  if (parm_num > UCHAR_MAX)
    return true;

  FOR_EACH_IMM_USE_STMT (stmt, ui, name)
    {
      unsigned uses_ok = 0;
      use_operand_p use_p;

      if (is_gimple_debug (stmt))
	continue;

      if (gimple_assign_single_p (stmt))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  if (!TREE_THIS_VOLATILE (rhs))
	    {
	      while (handled_component_p (rhs))
		rhs = TREE_OPERAND (rhs, 0);
	      if (TREE_CODE (rhs) == MEM_REF
		  && TREE_OPERAND (rhs, 0) == name
		  && integer_zerop (TREE_OPERAND (rhs, 1))
		  && types_compatible_p (TREE_TYPE (rhs),
					 TREE_TYPE (TREE_TYPE (name))))
		uses_ok++;
	    }
	}
      else if (is_gimple_call (stmt))
	{
	  gcall *call = as_a <gcall *> (stmt);
	  unsigned arg_count;
	  if (gimple_call_internal_p (call)
	      || (arg_count = gimple_call_num_args (call)) == 0)
	    {
	      ret = true;
	      break;
	    }

	  cgraph_edge *cs = node->get_edge (stmt);
	  gcc_checking_assert (cs);
	  isra_call_summary *csum = call_sums->get_create (cs);
	  csum->init_inputs (arg_count);

	  for (unsigned i = 0; i < arg_count; ++i)
	    {
	      tree arg = gimple_call_arg (stmt, i);

	      if (arg == name)
		{
		  /* TODO: Allow &MEM_REF[name + offset] here,
		     ipa_param_body_adjustments::modify_call_stmt has to be
		     adjusted too.  */
		  csum->m_arg_flow[i].pointer_pass_through = true;
		  set_single_param_flow_source (&csum->m_arg_flow[i], parm_num);
		  pt_count++;
		  uses_ok++;
		  continue;
		}

	      if (!TREE_THIS_VOLATILE (arg))
		{
		  while (handled_component_p (arg))
		    arg = TREE_OPERAND (arg, 0);
		  if (TREE_CODE (arg) == MEM_REF
		      && TREE_OPERAND (arg, 0) == name
		      && integer_zerop (TREE_OPERAND (arg, 1))
		      && types_compatible_p (TREE_TYPE (arg),
					     TREE_TYPE (TREE_TYPE (name))))
		    uses_ok++;
		}
	    }
	}

      /* If the number of valid uses does not match the number of
         uses in this stmt there is an unhandled use.  */
      unsigned all_uses = 0;
      FOR_EACH_IMM_USE_ON_STMT (use_p, ui)
	all_uses++;

      gcc_checking_assert (uses_ok <= all_uses);
      if (uses_ok != all_uses)
	{
	  ret = true;
	  break;
	}
    }

  *pt_count_p = pt_count;
  return ret;
}

/* Initialize vector of parameter descriptors of NODE.  Return true if there
   are any candidates for splitting or unused aggregate parameter removal (the
   function may return false if there are candidates for removal of register
   parameters).  */

static bool
create_parameter_descriptors (cgraph_node *node,
			      vec<gensum_param_desc> *param_descriptions)
{
  function *fun = DECL_STRUCT_FUNCTION (node->decl);
  bool ret = false;

  int num = 0;
  for (tree parm = DECL_ARGUMENTS (node->decl);
       parm;
       parm = DECL_CHAIN (parm), num++)
    {
      const char *msg;
      gensum_param_desc *desc = &(*param_descriptions)[num];
      /* param_descriptions vector is grown cleared in the caller.  */
      desc->param_number = num;
      decl2desc->put (parm, desc);

      if (dump_file && (dump_flags & TDF_DETAILS))
	print_generic_expr (dump_file, parm, TDF_UID);

      int scalar_call_uses;
      tree type = TREE_TYPE (parm);
      if (TREE_THIS_VOLATILE (parm))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, is volatile\n");
	  continue;
	}
      if (!is_gimple_reg_type (type) && is_va_list_type (type))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, is a va_list type\n");
	  continue;
	}
      if (TREE_ADDRESSABLE (parm))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, is addressable\n");
	  continue;
	}
      if (TREE_ADDRESSABLE (type))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, type cannot be split\n");
	  continue;
	}

      if (is_gimple_reg (parm)
	  && !isra_track_scalar_param_local_uses (fun, node, parm, num,
						  &scalar_call_uses))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " is a scalar with only %i call uses\n",
		     scalar_call_uses);

	  desc->locally_unused = true;
	  desc->call_uses = scalar_call_uses;
	}

      if (POINTER_TYPE_P (type))
	{
	  desc->by_ref = true;
	  if (TREE_CODE (type) == REFERENCE_TYPE
	      || (num == 0
		  && TREE_CODE (TREE_TYPE (node->decl)) == METHOD_TYPE))
	    desc->safe_ref = true;
	  else
	    desc->safe_ref = false;
	  type = TREE_TYPE (type);

	  if (TREE_CODE (type) == FUNCTION_TYPE
	      || TREE_CODE (type) == METHOD_TYPE)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " not a candidate, reference to "
			 "a function\n");
	      continue;
	    }
	  if (TYPE_VOLATILE (type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " not a candidate, reference to "
			 "a volatile type\n");
	      continue;
	    }
	  if (TREE_CODE (type) == ARRAY_TYPE
	      && TYPE_NONALIASED_COMPONENT (type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " not a candidate, reference to "
			 "a nonaliased component array\n");
	      continue;
	    }
	  if (!is_gimple_reg (parm))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " not a candidate, a reference which is "
			 "not a gimple register (probably addressable)\n");
	      continue;
	    }
	  if (is_va_list_type (type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " not a candidate, reference to "
			 "a va list\n");
	      continue;
	    }
	  if (ptr_parm_has_nonarg_uses (node, fun, parm, num,
					&desc->ptr_pt_count))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " not a candidate, reference has "
			 "nonarg uses\n");
	      continue;
	    }
	}
      else if (!AGGREGATE_TYPE_P (type))
	{
	  /* This is in an else branch because scalars passed by reference are
	     still candidates to be passed by value.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, not an aggregate\n");
	  continue;
	}

      if (!COMPLETE_TYPE_P (type))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, not a complete type\n");
	  continue;
	}
      if (!tree_fits_uhwi_p (TYPE_SIZE (type)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, size not representable\n");
	  continue;
	}
      unsigned HOST_WIDE_INT type_size
	= tree_to_uhwi (TYPE_SIZE (type)) / BITS_PER_UNIT;
      if (type_size == 0
	  || type_size >= ISRA_ARG_SIZE_LIMIT)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " not a candidate, has zero or huge size\n");
	  continue;
	}
      if (type_internals_preclude_sra_p (type, &msg))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, " not a candidate, %s\n", msg);
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " is a candidate\n");

      ret = true;
      desc->split_candidate = true;
      if (desc->by_ref && !desc->safe_ref)
	desc->deref_index = unsafe_by_ref_count++;
    }
  return ret;
}

/* Return pointer to descriptor of parameter DECL or NULL if it cannot be
   found, which happens if DECL is for a static chain.  */

static gensum_param_desc *
get_gensum_param_desc (tree decl)
{
  if (!decl2desc)
    return NULL;
  gcc_checking_assert (TREE_CODE (decl) == PARM_DECL);
  gensum_param_desc **slot = decl2desc->get (decl);
  if (!slot)
    /* This can happen for static chains which we cannot handle so far.  */
    return NULL;
  gcc_checking_assert (*slot);
  return *slot;
}


/* Remove parameter described by DESC from candidates for IPA-SRA splitting and
   write REASON to the dump file if there is one.  */

static void
disqualify_split_candidate (gensum_param_desc *desc, const char *reason)
{
  if (!desc->split_candidate)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "! Disqualifying parameter number %i - %s\n",
	     desc->param_number, reason);

  desc->split_candidate = false;
}

/* Remove DECL from candidates for IPA-SRA and write REASON to the dump file if
   there is one.  */

static void
disqualify_split_candidate (tree decl, const char *reason)
{
  gensum_param_desc *desc = get_gensum_param_desc (decl);
  if (desc)
    disqualify_split_candidate (desc, reason);
}

/* Allocate a new access to DESC and fill it in with OFFSET and SIZE.  But
   first, check that there are not too many of them already.  If so, do not
   allocate anything and return NULL.  */

static gensum_param_access *
allocate_access (gensum_param_desc *desc,
		 HOST_WIDE_INT offset, HOST_WIDE_INT size)
{
  if (desc->access_count
      == (unsigned) param_ipa_sra_max_replacements)
    {
      disqualify_split_candidate (desc, "Too many replacement candidates");
      return NULL;
    }

  gensum_param_access *access
    = (gensum_param_access *) obstack_alloc (&gensum_obstack,
					     sizeof (gensum_param_access));
  memset (access, 0, sizeof (*access));
  access->offset = offset;
  access->size = size;
  access->load_count = profile_count::zero ();
  return access;
}

/* In what context scan_expr_access has been called, whether it deals with a
   load, a function argument, or a store.  Please note that in rare
   circumstances when it is not clear if the access is a load or store,
   ISRA_CTX_STORE is used too.  */

enum isra_scan_context {ISRA_CTX_LOAD, ISRA_CTX_ARG, ISRA_CTX_STORE};

/* Return an access describing memory access to the variable described by DESC
   at OFFSET with SIZE in context CTX, starting at pointer to the linked list
   at a certain tree level FIRST.  Attempt to create it and put into the
   appropriate place in the access tree if does not exist, but fail and return
   NULL if there are already too many accesses, if it would create a partially
   overlapping access or if an access would end up within a pre-existing
   non-call access.  */

static gensum_param_access *
get_access_1 (gensum_param_desc *desc, gensum_param_access **first,
	      HOST_WIDE_INT offset, HOST_WIDE_INT size, isra_scan_context ctx)
{
  gensum_param_access *access = *first, **ptr = first;

  if (!access)
    {
      /* No pre-existing access at this level, just create it.  */
      gensum_param_access *a = allocate_access (desc, offset, size);
      if (!a)
	return NULL;
      *first = a;
      return *first;
    }

  if (access->offset >= offset + size)
    {
      /* We want to squeeze it in front of the very first access, just do
	 it.  */
      gensum_param_access *r = allocate_access (desc, offset, size);
      if (!r)
	return NULL;
      r->next_sibling = access;
      *first = r;
      return r;
    }

  /* Skip all accesses that have to come before us until the next sibling is
     already too far.  */
  while (offset >= access->offset + access->size
	 && access->next_sibling
	 && access->next_sibling->offset < offset + size)
    {
      ptr = &access->next_sibling;
      access = access->next_sibling;
    }

  /* At this point we know we do not belong before access.  */
  gcc_assert (access->offset < offset + size);

  if (access->offset == offset && access->size == size)
    /* We found what we were looking for.  */
    return access;

  if (access->offset <= offset
      && access->offset + access->size >= offset + size)
    {
    /* We fit into access which is larger than us.  We need to find/create
       something below access.  But we only allow nesting in call
       arguments.  */
      if (access->nonarg)
	return NULL;

      return get_access_1 (desc, &access->first_child, offset, size, ctx);
    }

  if (offset <= access->offset
      && offset + size  >= access->offset + access->size)
    /* We are actually bigger than access, which fully fits into us, take its
       place and make all accesses fitting into it its children.  */
    {
      /* But first, we only allow nesting in call arguments so check if that is
	 what we are trying to represent.  */
      if (ctx != ISRA_CTX_ARG)
	return NULL;

      gensum_param_access *r = allocate_access (desc, offset, size);
      if (!r)
	return NULL;
      r->first_child = access;

      while (access->next_sibling
	     && access->next_sibling->offset < offset + size)
	access = access->next_sibling;
      if (access->offset + access->size > offset + size)
	{
	  /* This must be a different access, which are sorted, so the
	     following must be true and this signals a partial overlap.  */
	  gcc_assert (access->offset > offset);
	  return NULL;
	}

      r->next_sibling = access->next_sibling;
      access->next_sibling = NULL;
      *ptr = r;
      return r;
    }

  if (offset >= access->offset + access->size)
    {
      /* We belong after access.  */
      gensum_param_access *r = allocate_access (desc, offset, size);
      if (!r)
	return NULL;
      r->next_sibling = access->next_sibling;
      access->next_sibling = r;
      return r;
    }

  if (offset < access->offset)
    {
      /* We know the following, otherwise we would have created a
	 super-access. */
      gcc_checking_assert (offset + size < access->offset + access->size);
      return NULL;
    }

  if (offset + size > access->offset + access->size)
    {
      /* Likewise.  */
      gcc_checking_assert (offset > access->offset);
      return NULL;
    }

  gcc_unreachable ();
}

/* Return an access describing memory access to the variable described by DESC
   at OFFSET with SIZE in context CTX, mark it as used in context CTX.  Attempt
   to create if it does not exist, but fail and return NULL if there are
   already too many accesses, if it would create a partially overlapping access
   or if an access would end up in a non-call access.  */

static gensum_param_access *
get_access (gensum_param_desc *desc, HOST_WIDE_INT offset, HOST_WIDE_INT size,
	    isra_scan_context ctx)
{
  gcc_checking_assert (desc->split_candidate);

  gensum_param_access *access = get_access_1 (desc, &desc->accesses, offset,
					      size, ctx);
  if (!access)
    {
      disqualify_split_candidate (desc,
				  "Bad access overlap or too many accesses");
      return NULL;
    }

  switch (ctx)
    {
    case ISRA_CTX_STORE:
      gcc_assert (!desc->by_ref);
      /* Fall-through */
    case ISRA_CTX_LOAD:
      access->nonarg = true;
      break;
    case ISRA_CTX_ARG:
      break;
    }

  return access;
}

/* Verify that parameter access tree starting with ACCESS is in good shape.
   PARENT_OFFSET and PARENT_SIZE are the corresponding fields of parent of
   ACCESS or zero if there is none.  */

static bool
verify_access_tree_1 (gensum_param_access *access, HOST_WIDE_INT parent_offset,
		      HOST_WIDE_INT parent_size)
{
  while (access)
    {
      gcc_assert (access->offset >= 0 && access->size >= 0);

      if (parent_size != 0)
	{
	  if (access->offset < parent_offset)
	    {
	      error ("Access offset before parent offset");
	      return true;
	    }
	  if (access->size >= parent_size)
	    {
	      error ("Access size greater or equal to its parent size");
	      return true;
	    }
	  if (access->offset + access->size > parent_offset + parent_size)
	    {
	      error ("Access terminates outside of its parent");
	      return true;
	    }
	}

      if (verify_access_tree_1 (access->first_child, access->offset,
				access->size))
	return true;

      if (access->next_sibling
	  && (access->next_sibling->offset < access->offset + access->size))
	{
	  error ("Access overlaps with its sibling");
	  return true;
	}

      access = access->next_sibling;
    }
  return false;
}

/* Verify that parameter access tree starting with ACCESS is in good shape,
   halt compilation and dump the tree to stderr if not.  */

DEBUG_FUNCTION void
isra_verify_access_tree (gensum_param_access *access)
{
  if (verify_access_tree_1 (access, 0, 0))
    {
      for (; access; access = access->next_sibling)
        dump_gensum_access (stderr, access, 2);
      internal_error ("IPA-SRA access verification failed");
    }
}


/* Callback of walk_stmt_load_store_addr_ops visit_addr used to determine
   GIMPLE_ASM operands with memory constrains which cannot be scalarized.  */

static bool
asm_visit_addr (gimple *, tree op, tree, void *)
{
  op = get_base_address (op);
  if (op
      && TREE_CODE (op) == PARM_DECL)
    disqualify_split_candidate (op, "Non-scalarizable GIMPLE_ASM operand.");

  return false;
}

/* Mark a dereference of parameter identified by DESC of distance DIST in a
   basic block BB, unless the BB has already been marked as a potentially
   final.  */

static void
mark_param_dereference (gensum_param_desc *desc, HOST_WIDE_INT dist,
			basic_block bb)
{
  gcc_assert (desc->by_ref);
  gcc_checking_assert (desc->split_candidate);

  if (desc->safe_ref
      || bitmap_bit_p (final_bbs, bb->index))
    return;

  int idx = bb->index * unsafe_by_ref_count + desc->deref_index;
  if (bb_dereferences[idx] < dist)
    bb_dereferences[idx] = dist;
}

/* Return true, if any potential replacements should use NEW_TYPE as opposed to
   previously recorded OLD_TYPE.  */

static bool
type_prevails_p (tree old_type, tree new_type)
{
  if (old_type == new_type)
    return false;

  /* Non-aggregates are always better.  */
  if (!is_gimple_reg_type (old_type)
      && is_gimple_reg_type (new_type))
    return true;
  if (is_gimple_reg_type (old_type)
      && !is_gimple_reg_type (new_type))
    return false;

  /* Prefer any complex or vector type over any other scalar type.  */
  if (TREE_CODE (old_type) != COMPLEX_TYPE
      && TREE_CODE (old_type) != VECTOR_TYPE
      && (TREE_CODE (new_type) == COMPLEX_TYPE
	  || TREE_CODE (new_type) == VECTOR_TYPE))
    return true;
  if ((TREE_CODE (old_type) == COMPLEX_TYPE
       || TREE_CODE (old_type) == VECTOR_TYPE)
      && TREE_CODE (new_type) != COMPLEX_TYPE
      && TREE_CODE (new_type) != VECTOR_TYPE)
    return false;

  /* Use the integral type with the bigger precision.  */
  if (INTEGRAL_TYPE_P (old_type)
      && INTEGRAL_TYPE_P (new_type))
    return (TYPE_PRECISION (new_type) > TYPE_PRECISION (old_type));

  /* Attempt to disregard any integral type with non-full precision.  */
  if (INTEGRAL_TYPE_P (old_type)
      && (TREE_INT_CST_LOW (TYPE_SIZE (old_type))
	  != TYPE_PRECISION (old_type)))
    return true;
  if (INTEGRAL_TYPE_P (new_type)
      && (TREE_INT_CST_LOW (TYPE_SIZE (new_type))
	  != TYPE_PRECISION (new_type)))
    return false;
  /* Stabilize the selection.  */
  return TYPE_UID (old_type) < TYPE_UID (new_type);
}

/* When scanning an expression which is a call argument, this structure
   specifies the call and the position of the argument.  */

struct scan_call_info
{
  /* Call graph edge representing the call. */
  cgraph_edge *cs;
  /* Total number of arguments in the call.  */
  unsigned argument_count;
  /* Number of the actual argument being scanned.  */
  unsigned arg_idx;
};

/* Record use of ACCESS which belongs to a parameter described by DESC in a
   call argument described by CALL_INFO.  */

static void
record_nonregister_call_use (gensum_param_desc *desc,
			     scan_call_info *call_info,
			     unsigned unit_offset, unsigned unit_size)
{
  isra_call_summary *csum = call_sums->get_create (call_info->cs);
  csum->init_inputs (call_info->argument_count);

  isra_param_flow *param_flow = &csum->m_arg_flow[call_info->arg_idx];
  param_flow->aggregate_pass_through = true;
  set_single_param_flow_source (param_flow, desc->param_number);
  param_flow->unit_offset = unit_offset;
  param_flow->unit_size = unit_size;
  desc->call_uses++;
}

/* Callback of walk_aliased_vdefs, just mark that there was a possible
   modification.  */

static bool
mark_maybe_modified (ao_ref *, tree, void *data)
{
  bool *maybe_modified = (bool *) data;
  *maybe_modified = true;
  return true;
}

/* Analyze expression EXPR from GIMPLE for accesses to parameters. CTX
   specifies whether EXPR is used in a load, store or as an argument call. BB
   must be the basic block in which expr resides.  If CTX specifies call
   argument context, CALL_INFO must describe that call and argument position,
   otherwise it is ignored.  */

static void
scan_expr_access (tree expr, gimple *stmt, isra_scan_context ctx,
		  basic_block bb, scan_call_info *call_info = NULL)
{
  poly_int64 poffset, psize, pmax_size;
  HOST_WIDE_INT offset, size, max_size;
  tree base;
  bool deref = false;
  bool reverse;

  if (TREE_CODE (expr) == ADDR_EXPR)
    {
      if (ctx == ISRA_CTX_ARG)
	return;
      tree t = get_base_address (TREE_OPERAND (expr, 0));
      if (TREE_CODE (t) == VAR_DECL && !TREE_STATIC (t))
	loaded_decls->add (t);
      return;
    }
  if (TREE_CODE (expr) == SSA_NAME
      || CONSTANT_CLASS_P (expr))
    return;

  if (TREE_CODE (expr) == BIT_FIELD_REF
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == REALPART_EXPR)
    expr = TREE_OPERAND (expr, 0);

  base = get_ref_base_and_extent (expr, &poffset, &psize, &pmax_size, &reverse);

  if (TREE_CODE (base) == MEM_REF)
    {
      tree op = TREE_OPERAND (base, 0);
      if (TREE_CODE (op) != SSA_NAME
	  || !SSA_NAME_IS_DEFAULT_DEF (op))
	return;
      base = SSA_NAME_VAR (op);
      if (!base)
	return;
      deref = true;
    }
  else if (TREE_CODE (base) == VAR_DECL
	   && !TREE_STATIC (base)
	   && (ctx == ISRA_CTX_ARG
	       || ctx == ISRA_CTX_LOAD))
    loaded_decls->add (base);

  if (TREE_CODE (base) != PARM_DECL)
    return;

  gensum_param_desc *desc = get_gensum_param_desc (base);
  if (!desc || !desc->split_candidate)
    return;

  if (!poffset.is_constant (&offset)
      || !psize.is_constant (&size)
      || !pmax_size.is_constant (&max_size))
    {
      disqualify_split_candidate (desc, "Encountered a polynomial-sized "
				  "access.");
      return;
    }
  if (size < 0 || size != max_size)
    {
      disqualify_split_candidate (desc, "Encountered a variable sized access.");
      return;
    }
  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
    {
      disqualify_split_candidate (desc, "Encountered a bit-field access.");
      return;
    }
  if (offset < 0)
    {
      disqualify_split_candidate (desc, "Encountered an access at a "
				  "negative offset.");
      return;
    }
  gcc_assert ((offset % BITS_PER_UNIT) == 0);
  gcc_assert ((size % BITS_PER_UNIT) == 0);
  if ((offset / BITS_PER_UNIT) >= (UINT_MAX - ISRA_ARG_SIZE_LIMIT)
      || (size / BITS_PER_UNIT) >= ISRA_ARG_SIZE_LIMIT)
    {
      disqualify_split_candidate (desc, "Encountered an access with too big "
				  "offset or size");
      return;
    }

  tree type = TREE_TYPE (expr);
  unsigned int exp_align = get_object_alignment (expr);

  if (exp_align < TYPE_ALIGN (type))
    {
      disqualify_split_candidate (desc, "Underaligned access.");
      return;
    }

  if (deref)
    {
      if (!desc->by_ref)
	{
	  disqualify_split_candidate (desc, "Dereferencing a non-reference.");
	  return;
	}
      else if (ctx == ISRA_CTX_STORE)
	{
	  disqualify_split_candidate (desc, "Storing to data passed by "
				      "reference.");
	  return;
	}

      if (!aa_walking_limit)
	{
	  disqualify_split_candidate (desc, "Out of alias analysis step "
				      "limit.");
	  return;
	}

      gcc_checking_assert (gimple_vuse (stmt));
      bool maybe_modified = false;
      ao_ref ar;

      ao_ref_init (&ar, expr);
      bitmap visited = BITMAP_ALLOC (NULL);
      int walked = walk_aliased_vdefs (&ar, gimple_vuse (stmt),
				       mark_maybe_modified, &maybe_modified,
				       &visited, NULL, aa_walking_limit);
      BITMAP_FREE (visited);
      if (walked > 0)
	{
	  gcc_assert (aa_walking_limit > walked);
	  aa_walking_limit = aa_walking_limit - walked;
	}
      if (walked < 0)
	aa_walking_limit = 0;
      if (maybe_modified || walked < 0)
	{
	  disqualify_split_candidate (desc, "Data passed by reference possibly "
				      "modified through an alias.");
	  return;
	}
      else
	mark_param_dereference (desc, offset + size, bb);
    }
  else
    /* Pointer parameters with direct uses should have been ruled out by
       analyzing SSA default def when looking at the parameters.  */
    gcc_assert (!desc->by_ref);

  gensum_param_access *access = get_access (desc, offset, size, ctx);
  if (!access)
    return;

  if (ctx == ISRA_CTX_ARG)
    {
      gcc_checking_assert (call_info);

      if (!deref)
	record_nonregister_call_use (desc, call_info, offset / BITS_PER_UNIT,
				     size / BITS_PER_UNIT);
      else
	/* This is not a pass-through of a pointer, this is a use like any
	   other.  */
	access->nonarg = true;
    }
  else if (ctx == ISRA_CTX_LOAD && bb->count.initialized_p ())
    access->load_count += bb->count;

  if (!access->type)
    {
      access->type = type;
      access->alias_ptr_type = reference_alias_ptr_type (expr);
      access->reverse = reverse;
    }
  else
    {
      if (exp_align < TYPE_ALIGN (access->type))
	{
	  disqualify_split_candidate (desc, "Reference has lower alignment "
				      "than a previous one.");
	  return;
	}
      if (access->alias_ptr_type != reference_alias_ptr_type (expr))
	{
	  disqualify_split_candidate (desc, "Multiple alias pointer types.");
	  return;
	}
      if (access->reverse != reverse)
	{
	  disqualify_split_candidate (desc, "Both normal and reverse "
				      "scalar storage order.");
	  return;
	}
      if (!deref
	  && (AGGREGATE_TYPE_P (type) || AGGREGATE_TYPE_P (access->type))
	  && (TYPE_MAIN_VARIANT (access->type) != TYPE_MAIN_VARIANT (type)))
	{
	  /* We need the same aggregate type on all accesses to be able to
	     distinguish transformation spots from pass-through arguments in
	     the transformation phase.  */
	  disqualify_split_candidate (desc, "We do not support aggregate "
				      "type punning.");
	  return;
	}

      if (type_prevails_p (access->type, type))
	 access->type = type;
    }
}

/* Scan body function described by NODE and FUN and create access trees for
   parameters.  */

static void
scan_function (cgraph_node *node, struct function *fun)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (final_bbs && stmt_can_throw_external (fun, stmt))
	    bitmap_set_bit (final_bbs, bb->index);
	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      {
		tree t = gimple_return_retval (as_a <greturn *> (stmt));
		if (t != NULL_TREE)
		  scan_expr_access (t, stmt, ISRA_CTX_LOAD, bb);
		if (final_bbs)
		  bitmap_set_bit (final_bbs, bb->index);
	      }
	      break;

	    case GIMPLE_ASSIGN:
	      if (gimple_assign_single_p (stmt)
		  && !gimple_clobber_p (stmt))
		{
		  tree rhs = gimple_assign_rhs1 (stmt);
		  scan_expr_access (rhs, stmt, ISRA_CTX_LOAD, bb);
		  tree lhs = gimple_assign_lhs (stmt);
		  scan_expr_access (lhs, stmt, ISRA_CTX_STORE, bb);
		}
	      break;

	    case GIMPLE_CALL:
	      {
		unsigned argument_count = gimple_call_num_args (stmt);
		isra_scan_context ctx = ISRA_CTX_ARG;
		scan_call_info call_info, *call_info_p = &call_info;
		if (gimple_call_internal_p (stmt))
		  {
		    call_info_p = NULL;
		    ctx = ISRA_CTX_LOAD;
		    internal_fn ifn = gimple_call_internal_fn (stmt);
		    if (internal_store_fn_p (ifn))
		      ctx = ISRA_CTX_STORE;
		  }
		else
		  {
		    call_info.cs = node->get_edge (stmt);
		    call_info.argument_count = argument_count;
		  }

		for (unsigned i = 0; i < argument_count; i++)
		  {
		    call_info.arg_idx = i;
		    scan_expr_access (gimple_call_arg (stmt, i), stmt,
				      ctx, bb, call_info_p);
		  }

		tree lhs = gimple_call_lhs (stmt);
		if (lhs)
		  scan_expr_access (lhs, stmt, ISRA_CTX_STORE, bb);
		int flags = gimple_call_flags (stmt);
		if (final_bbs
		    && (((flags & (ECF_CONST | ECF_PURE)) == 0)
			|| (flags & ECF_LOOPING_CONST_OR_PURE)))
		  bitmap_set_bit (final_bbs, bb->index);
	      }
	      break;

	    case GIMPLE_ASM:
	      {
		gasm *asm_stmt = as_a <gasm *> (stmt);
		walk_stmt_load_store_addr_ops (asm_stmt, NULL, NULL, NULL,
					       asm_visit_addr);
		if (final_bbs)
		  bitmap_set_bit (final_bbs, bb->index);

		for (unsigned i = 0; i < gimple_asm_ninputs (asm_stmt); i++)
		  {
		    tree t = TREE_VALUE (gimple_asm_input_op (asm_stmt, i));
		    scan_expr_access (t, stmt, ISRA_CTX_LOAD, bb);
		  }
		for (unsigned i = 0; i < gimple_asm_noutputs (asm_stmt); i++)
		  {
		    tree t = TREE_VALUE (gimple_asm_output_op (asm_stmt, i));
		    scan_expr_access (t, stmt, ISRA_CTX_STORE, bb);
		  }
	      }
	      break;

	    default:
	      break;
	    }
	}
    }
}

/* Return true if SSA_NAME NAME of function described by FUN is only used in
   return statements, or if results of any operations it is involved in are
   only used in return statements.  ANALYZED is a bitmap that tracks which SSA
   names we have already started investigating.  */

static bool
ssa_name_only_returned_p (function *fun, tree name, bitmap analyzed)
{
  bool res = true;
  imm_use_iterator imm_iter;
  gimple *stmt;

  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, name)
    {
      if (is_gimple_debug (stmt))
	continue;

      if (gimple_code (stmt) == GIMPLE_RETURN)
	{
	  tree t = gimple_return_retval (as_a <greturn *> (stmt));
	  if (t != name)
	    {
	      res = false;
	      break;
	    }
	}
      else if (!stmt_unremovable_because_of_non_call_eh_p (fun, stmt)
	       && ((is_gimple_assign (stmt) && !gimple_has_volatile_ops (stmt))
		   || gimple_code (stmt) == GIMPLE_PHI))
	{
	  /* TODO: And perhaps for const function calls too?  */
	  tree lhs;
	  if (gimple_code (stmt) == GIMPLE_PHI)
	    lhs = gimple_phi_result (stmt);
	  else
	    lhs = gimple_assign_lhs (stmt);

	  if (TREE_CODE (lhs) != SSA_NAME)
	    {
	      res = false;
	      break;
	    }
	  gcc_assert (!gimple_vdef (stmt));
	  if (bitmap_set_bit (analyzed, SSA_NAME_VERSION (lhs))
	      && !ssa_name_only_returned_p (fun, lhs, analyzed))
	    {
	      res = false;
	      break;
	    }
	}
      else
	{
	  res = false;
	  break;
	}
    }
  return res;
}

/* Inspect the uses of the return value of the call associated with CS, and if
   it is not used or if it is only used to construct the return value of the
   caller, mark it as such in call or caller summary.  Also check for
   misaligned arguments.  */

static void
isra_analyze_call (cgraph_edge *cs)
{
  gcall *call_stmt = cs->call_stmt;
  unsigned count = gimple_call_num_args (call_stmt);
  isra_call_summary *csum = call_sums->get_create (cs);

  for (unsigned i = 0; i < count; i++)
    {
      tree arg = gimple_call_arg (call_stmt, i);
      if (TREE_CODE (arg) == ADDR_EXPR)
	{
	  poly_int64 poffset, psize, pmax_size;
	  bool reverse;
	  tree base = get_ref_base_and_extent (TREE_OPERAND (arg, 0), &poffset,
					       &psize, &pmax_size, &reverse);
	  HOST_WIDE_INT offset;
	  unsigned HOST_WIDE_INT ds;
	  if (DECL_P (base)
	      && (poffset.is_constant (&offset))
	      && tree_fits_uhwi_p (DECL_SIZE (base))
	      && ((ds = tree_to_uhwi (DECL_SIZE (base)) - offset)
		  < ISRA_ARG_SIZE_LIMIT * BITS_PER_UNIT))
	    {
	      csum->init_inputs (count);
	      gcc_assert (!csum->m_arg_flow[i].aggregate_pass_through);
	      csum->m_arg_flow[i].unit_size = ds / BITS_PER_UNIT;
	    }

	  if (TREE_CODE (base) == VAR_DECL
	      && !TREE_STATIC (base)
	      && !loaded_decls->contains (base))
	    {
	      csum->init_inputs (count);
	      csum->m_arg_flow[i].constructed_for_calls = true;
	    }
	}

      if (is_gimple_reg (arg))
	continue;

      tree offset;
      poly_int64 bitsize, bitpos;
      machine_mode mode;
      int unsignedp, reversep, volatilep = 0;
      get_inner_reference (arg, &bitsize, &bitpos, &offset, &mode,
			   &unsignedp, &reversep, &volatilep);
      if (!multiple_p (bitpos, BITS_PER_UNIT))
	{
	  csum->m_bit_aligned_arg = true;
	  break;
	}
    }

  tree lhs = gimple_call_lhs (call_stmt);
  if (lhs)
    {
      /* TODO: Also detect aggregates on a LHS of a call that are only returned
	 from this function (without being read anywhere).  */
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  bitmap analyzed = BITMAP_ALLOC (NULL);
	  if (ssa_name_only_returned_p (DECL_STRUCT_FUNCTION (cs->caller->decl),
					lhs, analyzed))
	    csum->m_return_returned = true;
	  BITMAP_FREE (analyzed);
	}
    }
  else
    csum->m_return_ignored = true;
}

/* Look at all calls going out of NODE, described also by IFS and perform all
   analyses necessary for IPA-SRA that are not done at body scan time or done
   even when body is not scanned because the function is not a candidate.  */

static void
isra_analyze_all_outgoing_calls (cgraph_node *node)
{
  for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
    isra_analyze_call (cs);
  for (cgraph_edge *cs = node->indirect_calls; cs; cs = cs->next_callee)
    isra_analyze_call (cs);
}

/* Dump a dereferences table with heading STR to file F.  */

static void
dump_dereferences_table (FILE *f, struct function *fun, const char *str)
{
  basic_block bb;

  fprintf (dump_file, "%s", str);
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (fun),
		  EXIT_BLOCK_PTR_FOR_FN (fun), next_bb)
    {
      fprintf (f, "%4i  %i   ", bb->index, bitmap_bit_p (final_bbs, bb->index));
      if (bb != EXIT_BLOCK_PTR_FOR_FN (fun))
	{
	  int i;
	  for (i = 0; i < unsafe_by_ref_count; i++)
	    {
	      int idx = bb->index * unsafe_by_ref_count + i;
	      fprintf (f, " %4" HOST_WIDE_INT_PRINT "d", bb_dereferences[idx]);
	    }
	}
      fprintf (f, "\n");
    }
  fprintf (dump_file, "\n");
}

/* Propagate distances in bb_dereferences in the opposite direction than the
   control flow edges, in each step storing the maximum of the current value
   and the minimum of all successors.  These steps are repeated until the table
   stabilizes.  Note that BBs which might terminate the functions (according to
   final_bbs bitmap) never updated in this way.  */

static void
propagate_dereference_distances (struct function *fun)
{
  basic_block bb;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_dereferences_table (dump_file, fun,
			     "Dereference table before propagation:\n");

  auto_vec<basic_block> queue (last_basic_block_for_fn (fun));
  queue.quick_push (ENTRY_BLOCK_PTR_FOR_FN (fun));
  FOR_EACH_BB_FN (bb, fun)
    {
      queue.quick_push (bb);
      bb->aux = bb;
    }

  while (!queue.is_empty ())
    {
      edge_iterator ei;
      edge e;
      bool change = false;
      int i;

      bb = queue.pop ();
      bb->aux = NULL;

      if (bitmap_bit_p (final_bbs, bb->index))
	continue;

      for (i = 0; i < unsafe_by_ref_count; i++)
	{
	  int idx = bb->index * unsafe_by_ref_count + i;
	  bool first = true;
	  HOST_WIDE_INT inh = 0;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    int succ_idx = e->dest->index * unsafe_by_ref_count + i;

	    if (e->dest == EXIT_BLOCK_PTR_FOR_FN (fun))
	      continue;

	    if (first)
	      {
		first = false;
		inh = bb_dereferences [succ_idx];
	      }
	    else if (bb_dereferences [succ_idx] < inh)
	      inh = bb_dereferences [succ_idx];
	  }

	  if (!first && bb_dereferences[idx] < inh)
	    {
	      bb_dereferences[idx] = inh;
	      change = true;
	    }
	}

      if (change)
	FOR_EACH_EDGE (e, ei, bb->preds)
	  {
	    if (e->src->aux)
	      continue;

	    e->src->aux = e->src;
	    queue.quick_push (e->src);
	  }
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_dereferences_table (dump_file, fun,
			     "Dereference table after propagation:\n");
}

/* Return true if the ACCESS loads happen frequently enough in FUN to risk
   moving them to the caller and only pass the result.  */

static bool
dereference_probable_p (struct function *fun, gensum_param_access *access)
{
  int threshold = opt_for_fn (fun->decl, param_ipa_sra_deref_prob_threshold);
  return access->load_count
    >= ENTRY_BLOCK_PTR_FOR_FN (fun)->count.apply_scale (threshold, 100);
}

/* Perform basic checks on ACCESS to PARM (of FUN) described by DESC and all
   its children, return true if the parameter cannot be split, otherwise return
   false and update *NONARG_ACC_SIZE and *ONLY_CALLS.  ENTRY_BB_INDEX must be
   the index of the entry BB in the function of PARM.  */

static bool
check_gensum_access (struct function *fun, tree parm, gensum_param_desc *desc,
		     gensum_param_access *access,
		     HOST_WIDE_INT *nonarg_acc_size, bool *only_calls,
		     int entry_bb_index)
{
  if (access->nonarg)
    {
      *only_calls = false;
      *nonarg_acc_size += access->size;

      if (access->first_child)
	{
	  disqualify_split_candidate (desc, "Overlapping non-call uses.");
	  return true;
	}
    }
  /* Do not decompose a non-BLKmode param in a way that would create
     BLKmode params.  Especially for by-reference passing (thus,
     pointer-type param) this is hardly worthwhile.  */
  if (DECL_MODE (parm) != BLKmode
      && TYPE_MODE (access->type) == BLKmode)
    {
      disqualify_split_candidate (desc, "Would convert a non-BLK to a BLK.");
      return true;
    }

  if (desc->by_ref)
    {
      if (desc->safe_ref)
	{
	  if (!dereference_probable_p (fun, access))
	    {
	      disqualify_split_candidate (desc, "Dereferences in callers "
					  "would happen much more frequently.");
	      return true;
	    }
	}
      else
	{
	  int idx = (entry_bb_index * unsafe_by_ref_count + desc->deref_index);
	  if ((access->offset + access->size) > bb_dereferences[idx])
	    {
	      if (!dereference_probable_p (fun, access))
		{
		  disqualify_split_candidate (desc, "Would create a possibly "
					      "illegal dereference in a "
					      "caller.");
		  return true;
		}
	      desc->conditionally_dereferenceable = true;
	    }
	}
    }

  for (gensum_param_access *ch = access->first_child;
       ch;
       ch = ch->next_sibling)
    if (check_gensum_access (fun, parm, desc, ch, nonarg_acc_size, only_calls,
			     entry_bb_index))
      return true;

  return false;
}

/* Copy data from FROM and all of its children to a vector of accesses in IPA
   descriptor DESC.  */

static void
copy_accesses_to_ipa_desc (gensum_param_access *from, isra_param_desc *desc)
{
  param_access *to = ggc_cleared_alloc<param_access> ();
  gcc_checking_assert ((from->offset % BITS_PER_UNIT) == 0);
  gcc_checking_assert ((from->size % BITS_PER_UNIT) == 0);
  to->unit_offset = from->offset / BITS_PER_UNIT;
  to->unit_size = from->size / BITS_PER_UNIT;
  to->type = from->type;
  to->alias_ptr_type = from->alias_ptr_type;
  to->certain = from->nonarg;
  to->reverse = from->reverse;
  vec_safe_push (desc->accesses, to);

  for (gensum_param_access *ch = from->first_child;
       ch;
       ch = ch->next_sibling)
    copy_accesses_to_ipa_desc (ch, desc);
}

/* Analyze function body scan results stored in param_accesses and
   param_accesses, detect possible transformations and store information of
   those in function summary.  NODE, FUN and IFS are all various structures
   describing the currently analyzed function.  */

static void
process_scan_results (cgraph_node *node, struct function *fun,
		      isra_func_summary *ifs,
		      vec<gensum_param_desc> *param_descriptions)
{
  bool check_pass_throughs = false;
  bool dereferences_propagated = false;
  tree parm = DECL_ARGUMENTS (node->decl);
  unsigned param_count = param_descriptions->length();

  for (unsigned desc_index = 0;
       desc_index < param_count;
       desc_index++, parm = DECL_CHAIN (parm))
    {
      gensum_param_desc *desc = &(*param_descriptions)[desc_index];
      if (!desc->split_candidate)
	continue;

      if (flag_checking)
	isra_verify_access_tree (desc->accesses);

      if (!dereferences_propagated
	  && desc->by_ref
	  && !desc->safe_ref
	  && desc->accesses)
	{
	  propagate_dereference_distances (fun);
	  dereferences_propagated = true;
	}

      HOST_WIDE_INT nonarg_acc_size = 0;
      bool only_calls = true;
      bool check_failed = false;

      int entry_bb_index = ENTRY_BLOCK_PTR_FOR_FN (fun)->index;
      for (gensum_param_access *acc = desc->accesses;
	   acc;
	   acc = acc->next_sibling)
	if (check_gensum_access (fun, parm, desc, acc, &nonarg_acc_size,
				 &only_calls, entry_bb_index))
	  {
	    check_failed = true;
	    break;
	  }
      if (check_failed)
	continue;

      if (only_calls)
	desc->locally_unused = true;

      HOST_WIDE_INT cur_param_size
	= tree_to_uhwi (TYPE_SIZE (TREE_TYPE (parm)));
      HOST_WIDE_INT param_size_limit, optimistic_limit;
      if (!desc->by_ref || optimize_function_for_size_p (fun))
	{
	  param_size_limit = cur_param_size;
	  optimistic_limit = cur_param_size;
	}
      else
	{
	  param_size_limit
	    = opt_for_fn (node->decl,
			  param_ipa_sra_ptr_growth_factor) * cur_param_size;
	  optimistic_limit
	    = (opt_for_fn (node->decl, param_ipa_sra_ptrwrap_growth_factor)
	       * param_size_limit);
	}

      if (nonarg_acc_size > optimistic_limit
	  || (!desc->by_ref && nonarg_acc_size == param_size_limit))
	{
	  disqualify_split_candidate (desc, "Would result into a too big set "
				      "of replacements even in best "
				      "scenarios.");
	}
      else
	{
	  /* create_parameter_descriptors makes sure unit sizes of all
	     candidate parameters fit unsigned integers restricted to
	     ISRA_ARG_SIZE_LIMIT.  */
	  desc->param_size_limit = param_size_limit / BITS_PER_UNIT;
	  desc->nonarg_acc_size = nonarg_acc_size / BITS_PER_UNIT;
	  if (desc->split_candidate && desc->ptr_pt_count)
	    {
	      gcc_assert (desc->by_ref);
	      check_pass_throughs = true;
	    }
	}
    }

  /* When a pointer parameter is passed-through to a callee, in which it is
     only used to read only one or a few items, we can attempt to transform it
     to obtaining and passing through the items instead of the pointer.  But we
     must take extra care that 1) we do not introduce any segfault by moving
     dereferences above control flow and that 2) the data is not modified
     through an alias in this function.  The IPA analysis must not introduce
     any accesses candidates unless it can prove both.

     The current solution is very crude as it consists of ensuring that the
     call postdominates entry BB and that the definition of VUSE of the call is
     default definition.  TODO: For non-recursive callees in the same
     compilation unit we could do better by doing analysis in topological order
     an looking into access candidates of callees, using their alias_ptr_types
     to attempt real AA.  We could also use the maximum known dereferenced
     offset in this function at IPA level.

     TODO: Measure the overhead and the effect of just being pessimistic.
     Maybe this is only -O3 material?  */

  hash_map<gimple *, bool> analyzed_stmts;
  bitmap always_executed_bbs = NULL;
  if (check_pass_throughs)
    for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
      {
	gcall *call_stmt = cs->call_stmt;
	tree vuse = gimple_vuse (call_stmt);

	/* If the callee is a const function, we don't get a VUSE.  In such
	   case there will be no memory accesses in the called function (or the
	   const attribute is wrong) and then we just don't care.  */
	bool uses_memory_as_obtained = vuse && SSA_NAME_IS_DEFAULT_DEF (vuse);

	unsigned count = gimple_call_num_args (call_stmt);
	isra_call_summary *csum = call_sums->get_create (cs);
	csum->init_inputs (count);
	csum->m_before_any_store = uses_memory_as_obtained;
	for (unsigned argidx = 0; argidx < count; argidx++)
	  {
	    if (!csum->m_arg_flow[argidx].pointer_pass_through)
	      continue;
	    unsigned pidx
	      = get_single_param_flow_source (&csum->m_arg_flow[argidx]);
	    gensum_param_desc *desc = &(*param_descriptions)[pidx];
	    if (!desc->split_candidate)
	      {
		csum->m_arg_flow[argidx].pointer_pass_through = false;
		continue;
	      }
	    if (!uses_memory_as_obtained)
	      continue;

	    if (desc->safe_ref)
	      {
		csum->m_arg_flow[argidx].safe_to_import_accesses = true;
		continue;
	      }

	    /* Walk basic block and see if its execution can terminate earlier.
	       Keep the info for later re-use to avoid quadratic behavoiur here.  */
	    gimple_stmt_iterator gsi = gsi_for_stmt (call_stmt);
	    bool safe = true;
	    int n = 0;
	    for (gsi_prev (&gsi); !gsi_end_p (gsi); gsi_prev (&gsi))
	      {
		bool *b = analyzed_stmts.get (gsi_stmt (gsi));
		if (b)
		  {
		    safe = *b;
		    gsi_next (&gsi);
		    break;
		  }
		n++;
		if (stmt_may_terminate_function_p (fun, gsi_stmt (gsi), false))
		  {
		    safe = false;
		    break;
		  }
	      }
	    if (n)
	      {
		if (gsi_end_p (gsi))
		  gsi = gsi_start_bb (gimple_bb (call_stmt));
		for (; gsi_stmt (gsi) != call_stmt; gsi_next (&gsi))
		  analyzed_stmts.get_or_insert (gsi_stmt (gsi)) = safe;
	      }

	    if (safe && !always_executed_bbs)
	      {
		mark_dfs_back_edges ();
		always_executed_bbs = find_always_executed_bbs (fun, false);
	      }
	    if (safe && bitmap_bit_p (always_executed_bbs, gimple_bb (call_stmt)->index))
	      csum->m_arg_flow[argidx].safe_to_import_accesses = true;
	  }

      }
  BITMAP_FREE (always_executed_bbs);

  /* TODO: Add early exit if we disqualified everything.  This also requires
     that we either relax the restriction that
     ipa_param_adjustments.m_always_copy_start must be the number of PARM_DECLs
     or store the number of parameters to IPA-SRA function summary and use that
     when just removing params.  */

  vec_safe_reserve_exact (ifs->m_parameters, param_count);
  ifs->m_parameters->quick_grow_cleared (param_count);
  for (unsigned desc_index = 0; desc_index < param_count; desc_index++)
    {
      gensum_param_desc *s = &(*param_descriptions)[desc_index];
      isra_param_desc *d = &(*ifs->m_parameters)[desc_index];

      d->param_size_limit = s->param_size_limit;
      d->size_reached = s->nonarg_acc_size;
      d->locally_unused = s->locally_unused;
      d->split_candidate = s->split_candidate;
      d->by_ref = s->by_ref;
      d->conditionally_dereferenceable = s->conditionally_dereferenceable;

      for (gensum_param_access *acc = s->accesses;
	   acc;
	   acc = acc->next_sibling)
	copy_accesses_to_ipa_desc (acc, d);
    }

  if (dump_file)
    dump_isra_param_descriptors (dump_file, node->decl, ifs, false);
}

/* Return true if there are any overlaps among certain accesses of DESC.  If
   non-NULL, set *CERTAIN_ACCESS_PRESENT_P upon encountering a certain access
   too.  DESC is assumed to be a split candidate that is not locally
   unused.  */

static bool
overlapping_certain_accesses_p (isra_param_desc *desc,
				bool *certain_access_present_p)
{
  unsigned pclen = vec_safe_length (desc->accesses);
  for (unsigned i = 0; i < pclen; i++)
    {
      param_access *a1 = (*desc->accesses)[i];

      if (!a1->certain)
	continue;
      if (certain_access_present_p)
	*certain_access_present_p = true;
      for (unsigned j = i + 1; j < pclen; j++)
	{
	  param_access *a2 = (*desc->accesses)[j];
	  if (a2->certain
	      && a1->unit_offset < a2->unit_offset + a2->unit_size
	      && a1->unit_offset + a1->unit_size > a2->unit_offset)
	    return true;
	}
    }
  return false;
}

/* Check for any overlaps of certain param accesses among splitting candidates
   and signal an ICE if there are any.  If CERTAIN_MUST_EXIST is set, also
   check that used splitting candidates have at least one certain access.  */

static void
verify_splitting_accesses (cgraph_node *node, bool certain_must_exist)
{
  isra_func_summary *ifs = func_sums->get (node);
  if (!ifs || !ifs->m_candidate)
    return;
  unsigned param_count = vec_safe_length (ifs->m_parameters);
  for (unsigned pidx = 0; pidx < param_count; pidx++)
    {
      isra_param_desc *desc = &(*ifs->m_parameters)[pidx];
      if (!desc->split_candidate || desc->locally_unused)
	continue;

      bool certain_access_present = !certain_must_exist;
      if (overlapping_certain_accesses_p (desc, &certain_access_present))
	internal_error ("function %qs, parameter %u, has IPA-SRA accesses "
			"which overlap", node->dump_name (), pidx);
      if (!certain_access_present)
	internal_error ("function %qs, parameter %u, is used but does not "
			"have any certain IPA-SRA access",
			node->dump_name (), pidx);
    }
}

/* Intraprocedural part of IPA-SRA analysis.  Scan bodies of all functions in
   this compilation unit and create summary structures describing IPA-SRA
   opportunities and constraints in them.  */

static void
ipa_sra_generate_summary (void)
{
  struct cgraph_node *node;

  gcc_checking_assert (!func_sums);
  gcc_checking_assert (!call_sums);
  func_sums
    = (new (ggc_alloc_no_dtor <ipa_sra_function_summaries> ())
       ipa_sra_function_summaries (symtab, true));
  call_sums = new ipa_sra_call_summaries (symtab);

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    ipa_sra_summarize_function (node);
  return;
}

/* Write intraprocedural analysis information about E and all of its outgoing
   edges into a stream for LTO WPA.  */

static void
isra_write_edge_summary (output_block *ob, cgraph_edge *e)
{
  isra_call_summary *csum = call_sums->get (e);
  unsigned input_count = csum->m_arg_flow.length ();
  streamer_write_uhwi (ob, input_count);
  for (unsigned i = 0; i < input_count; i++)
    {
      isra_param_flow *ipf = &csum->m_arg_flow[i];
      streamer_write_hwi (ob, ipf->length);
      bitpack_d bp = bitpack_create (ob->main_stream);
      for (int j = 0; j < ipf->length; j++)
	bp_pack_value (&bp, ipf->inputs[j], 8);
      bp_pack_value (&bp, ipf->aggregate_pass_through, 1);
      bp_pack_value (&bp, ipf->pointer_pass_through, 1);
      bp_pack_value (&bp, ipf->safe_to_import_accesses, 1);
      bp_pack_value (&bp, ipf->constructed_for_calls, 1);
      streamer_write_bitpack (&bp);
      streamer_write_uhwi (ob, ipf->unit_offset);
      streamer_write_uhwi (ob, ipf->unit_size);
    }
  bitpack_d bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, csum->m_return_ignored, 1);
  bp_pack_value (&bp, csum->m_return_returned, 1);
  bp_pack_value (&bp, csum->m_bit_aligned_arg, 1);
  bp_pack_value (&bp, csum->m_before_any_store, 1);
  streamer_write_bitpack (&bp);
}

/* Write intraprocedural analysis information about NODE and all of its outgoing
   edges into a stream for LTO WPA.  */

static void
isra_write_node_summary (output_block *ob, cgraph_node *node)
{
  isra_func_summary *ifs = func_sums->get (node);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  int node_ref = lto_symtab_encoder_encode (encoder, node);
  streamer_write_uhwi (ob, node_ref);

  unsigned param_desc_count = vec_safe_length (ifs->m_parameters);
  streamer_write_uhwi (ob, param_desc_count);
  for (unsigned i = 0; i < param_desc_count; i++)
    {
      isra_param_desc *desc = &(*ifs->m_parameters)[i];
      unsigned access_count = vec_safe_length (desc->accesses);
      streamer_write_uhwi (ob, access_count);
      for (unsigned j = 0; j < access_count; j++)
	{
	  param_access *acc = (*desc->accesses)[j];
	  stream_write_tree (ob, acc->type, true);
	  stream_write_tree (ob, acc->alias_ptr_type, true);
	  streamer_write_uhwi (ob, acc->unit_offset);
	  streamer_write_uhwi (ob, acc->unit_size);
	  bitpack_d bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, acc->certain, 1);
	  bp_pack_value (&bp, acc->reverse, 1);
	  streamer_write_bitpack (&bp);
	}
      streamer_write_uhwi (ob, desc->param_size_limit);
      streamer_write_uhwi (ob, desc->size_reached);
      gcc_assert (desc->safe_size == 0);
      bitpack_d bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, desc->locally_unused, 1);
      bp_pack_value (&bp, desc->split_candidate, 1);
      bp_pack_value (&bp, desc->by_ref, 1);
      gcc_assert (!desc->not_specially_constructed);
      bp_pack_value (&bp, desc->conditionally_dereferenceable, 1);
      gcc_assert (!desc->safe_size_set);
      streamer_write_bitpack (&bp);
    }
  bitpack_d bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, ifs->m_candidate, 1);
  bp_pack_value (&bp, ifs->m_returns_value, 1);
  bp_pack_value (&bp, ifs->m_return_ignored, 1);
  gcc_assert (!ifs->m_queued);
  streamer_write_bitpack (&bp);

  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    isra_write_edge_summary (ob, e);
  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
    isra_write_edge_summary (ob, e);
}

/* Write intraprocedural analysis information into a stream for LTO WPA.  */

static void
ipa_sra_write_summary (void)
{
  if (!func_sums || !call_sums)
    return;

  struct output_block *ob = create_output_block (LTO_section_ipa_sra);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  ob->symbol = NULL;

  unsigned int count = 0;
  lto_symtab_encoder_iterator lsei;
  for (lsei = lsei_start_function_in_partition (encoder);
       !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      cgraph_node *node = lsei_cgraph_node (lsei);
      if (node->has_gimple_body_p ()
	  && func_sums->get (node) != NULL)
	count++;
    }
  streamer_write_uhwi (ob, count);

  /* Process all of the functions.  */
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      cgraph_node *node = lsei_cgraph_node (lsei);
      if (node->has_gimple_body_p ()
	  && func_sums->get (node) != NULL)
        isra_write_node_summary (ob, node);
    }
  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);
}

/* Read intraprocedural analysis information about E and all of its outgoing
   edges into a stream for LTO WPA.  */

static void
isra_read_edge_summary (struct lto_input_block *ib, cgraph_edge *cs)
{
  isra_call_summary *csum = call_sums->get_create (cs);
  unsigned input_count = streamer_read_uhwi (ib);
  csum->init_inputs (input_count);
  for (unsigned i = 0; i < input_count; i++)
    {
      isra_param_flow *ipf = &csum->m_arg_flow[i];
      ipf->length = streamer_read_hwi (ib);
      bitpack_d bp = streamer_read_bitpack (ib);
      for (int j = 0; j < ipf->length; j++)
	ipf->inputs[j] = bp_unpack_value (&bp, 8);
      ipf->aggregate_pass_through = bp_unpack_value (&bp, 1);
      ipf->pointer_pass_through = bp_unpack_value (&bp, 1);
      ipf->safe_to_import_accesses = bp_unpack_value (&bp, 1);
      ipf->constructed_for_calls = bp_unpack_value (&bp, 1);
      ipf->unit_offset = streamer_read_uhwi (ib);
      ipf->unit_size = streamer_read_uhwi (ib);
    }
  bitpack_d bp = streamer_read_bitpack (ib);
  csum->m_return_ignored = bp_unpack_value (&bp, 1);
  csum->m_return_returned = bp_unpack_value (&bp, 1);
  csum->m_bit_aligned_arg = bp_unpack_value (&bp, 1);
  csum->m_before_any_store = bp_unpack_value (&bp, 1);
}

/* Read intraprocedural analysis information about NODE and all of its outgoing
   edges into a stream for LTO WPA.  */

static void
isra_read_node_info (struct lto_input_block *ib, cgraph_node *node,
		     struct data_in *data_in)
{
  isra_func_summary *ifs = func_sums->get_create (node);
  unsigned param_desc_count = streamer_read_uhwi (ib);
  if (param_desc_count > 0)
    {
      vec_safe_reserve_exact (ifs->m_parameters, param_desc_count);
      ifs->m_parameters->quick_grow_cleared (param_desc_count);
    }
  for (unsigned i = 0; i < param_desc_count; i++)
    {
      isra_param_desc *desc = &(*ifs->m_parameters)[i];
      unsigned access_count = streamer_read_uhwi (ib);
      for (unsigned j = 0; j < access_count; j++)
	{
	  param_access *acc = ggc_cleared_alloc<param_access> ();
	  acc->type = stream_read_tree (ib, data_in);
	  acc->alias_ptr_type = stream_read_tree (ib, data_in);
	  acc->unit_offset = streamer_read_uhwi (ib);
	  acc->unit_size = streamer_read_uhwi (ib);
	  bitpack_d bp = streamer_read_bitpack (ib);
	  acc->certain = bp_unpack_value (&bp, 1);
	  acc->reverse = bp_unpack_value (&bp, 1);
	  vec_safe_push (desc->accesses, acc);
	}
      desc->param_size_limit = streamer_read_uhwi (ib);
      desc->size_reached = streamer_read_uhwi (ib);
      desc->safe_size = 0;
      bitpack_d bp = streamer_read_bitpack (ib);
      desc->locally_unused = bp_unpack_value (&bp, 1);
      desc->split_candidate = bp_unpack_value (&bp, 1);
      desc->by_ref = bp_unpack_value (&bp, 1);
      desc->not_specially_constructed = 0;
      desc->conditionally_dereferenceable = bp_unpack_value (&bp, 1);
      desc->safe_size_set = 0;
    }
  bitpack_d bp = streamer_read_bitpack (ib);
  ifs->m_candidate = bp_unpack_value (&bp, 1);
  ifs->m_returns_value = bp_unpack_value (&bp, 1);
  ifs->m_return_ignored = bp_unpack_value (&bp, 1);
  ifs->m_queued = 0;

  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    isra_read_edge_summary (ib, e);
  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
    isra_read_edge_summary (ib, e);
}

/* Read IPA-SRA summaries from a section in file FILE_DATA of length LEN with
   data DATA.  TODO: This function was copied almost verbatim from ipa-prop.cc,
   it should be possible to unify them somehow.  */

static void
isra_read_summary_section (struct lto_file_decl_data *file_data,
			   const char *data, size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  unsigned int i;
  unsigned int count;

  lto_input_block ib_main ((const char *) data + main_offset,
			   header->main_size, file_data->mode_table);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, vNULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_symtab_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->symtab_node_encoder;
      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
								index));
      gcc_assert (node->definition);
      isra_read_node_info (&ib_main, node, data_in);
    }
  lto_free_section_data (file_data, LTO_section_ipa_sra, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Read intraprocedural analysis information into a stream for LTO WPA.  */

static void
ipa_sra_read_summary (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  gcc_checking_assert (!func_sums);
  gcc_checking_assert (!call_sums);
  func_sums
    = (new (ggc_alloc_no_dtor <ipa_sra_function_summaries> ())
       ipa_sra_function_summaries (symtab, true));
  call_sums = new ipa_sra_call_summaries (symtab);

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data
	= lto_get_summary_section_data (file_data, LTO_section_ipa_sra, &len);
      if (data)
        isra_read_summary_section (file_data, data, len);
    }
}

/* Dump all IPA-SRA summary data for all cgraph nodes and edges to file F.  If
   HINTS is true, also dump IPA-analysis computed hints.  */

static void
ipa_sra_dump_all_summaries (FILE *f, bool hints)
{
  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      fprintf (f, "\nSummary for node %s:\n", node->dump_name ());

      isra_func_summary *ifs = func_sums->get (node);
      if (!ifs)
	fprintf (f, "  Function does not have any associated IPA-SRA "
		 "summary\n");
      else if (!ifs->m_candidate)
	fprintf (f, "  Not a candidate function\n");
      else
	{
	  if (ifs->m_returns_value)
	    fprintf (f, "  Returns value\n");
	  if (vec_safe_is_empty (ifs->m_parameters))
	    fprintf (f, "  No parameter information. \n");
	  else
	    for (unsigned i = 0; i < ifs->m_parameters->length (); ++i)
	      {
		fprintf (f, "  Descriptor for parameter %i:\n", i);
		dump_isra_param_descriptor (f, &(*ifs->m_parameters)[i], hints);
	      }
	  fprintf (f, "\n");
	}

      struct cgraph_edge *cs;
      for (cs = node->callees; cs; cs = cs->next_callee)
	{
	  fprintf (f, "  Summary for edge %s->%s:\n", cs->caller->dump_name (),
		   cs->callee->dump_name ());
	  isra_call_summary *csum = call_sums->get (cs);
	  if (csum)
	    csum->dump (f);
	  else
	    fprintf (f, "    Call summary is MISSING!\n");
	}

    }
  fprintf (f, "\n\n");
}

/* Perform function-scope viability tests that can be only made at IPA level
   and return false if the function is deemed unsuitable for IPA-SRA.  */

static bool
ipa_sra_ipa_function_checks (cgraph_node *node)
{
  if (!node->can_be_local_p ())
    {
      if (dump_file)
	fprintf (dump_file, "Function %s disqualified because it cannot be "
		 "made local.\n", node->dump_name ());
      return false;
    }
  if (!node->can_change_signature)
    {
      if (dump_file)
	fprintf (dump_file, "Function can not change signature.\n");
      return false;
    }

  return true;
}

/* Issues found out by check_callers_for_issues.  */

struct caller_issues
{
  /* The candidate being considered.  */
  cgraph_node *candidate;
  /* There is a thunk among callers.  */
  bool thunk;
  /* Set if there is at least one caller that is OK.  */
  bool there_is_one;
  /* Call site with no available information.  */
  bool unknown_callsite;
  /* Call from outside the candidate's comdat group.  */
  bool call_from_outside_comdat;
  /* There is a bit-aligned load into one of non-gimple-typed arguments. */
  bool bit_aligned_aggregate_argument;
};

/* Worker for call_for_symbol_and_aliases, set any flags of passed caller_issues
   that apply.  */

static bool
check_for_caller_issues (struct cgraph_node *node, void *data)
{
  struct caller_issues *issues = (struct caller_issues *) data;

  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    {
      if (cs->caller->thunk)
	{
	  issues->thunk = true;
	  /* TODO: We should be able to process at least some types of
	     thunks.  */
	  return true;
	}
      if (issues->candidate->calls_comdat_local
	  && issues->candidate->same_comdat_group
	  && !issues->candidate->in_same_comdat_group_p (cs->caller))
	{
	  issues->call_from_outside_comdat = true;
	  return true;
	}

      isra_call_summary *csum = call_sums->get (cs);
      if (!csum)
	{
	  issues->unknown_callsite = true;
	  return true;
	}

      if (csum->m_bit_aligned_arg)
	issues->bit_aligned_aggregate_argument = true;

      issues->there_is_one = true;
    }
  return false;
}

/* Look at all incoming edges to NODE, including aliases and thunks and look
   for problems.  Return true if NODE type should not be modified at all.  */

static bool
check_all_callers_for_issues (cgraph_node *node)
{
  struct caller_issues issues;
  memset (&issues, 0, sizeof (issues));
  issues.candidate = node;

  node->call_for_symbol_and_aliases (check_for_caller_issues, &issues, true);
  if (issues.unknown_callsite)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "A call of %s has not been analyzed.  Disabling "
		 "all modifications.\n", node->dump_name ());
      return true;
    }
  /* TODO: We should be able to process at least some types of thunks.  */
  if (issues.thunk)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "A call of %s is through thunk, which are not"
		 " handled yet.  Disabling all modifications.\n",
		 node->dump_name ());
      return true;
    }
  if (issues.call_from_outside_comdat)
    {
      if (dump_file)
	fprintf (dump_file, "Function would become private comdat called "
		 "outside of its comdat group.\n");
      return true;
    }

  if (issues.bit_aligned_aggregate_argument)
    {
      /* Let's only remove parameters/return values from such functions.
	 TODO: We could only prevent splitting the problematic parameters if
	 anybody thinks it is worth it.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "A call of %s has bit-aligned aggregate argument,"
		 " disabling parameter splitting.\n", node->dump_name ());

      isra_func_summary *ifs = func_sums->get (node);
      gcc_checking_assert (ifs);
      unsigned param_count = vec_safe_length (ifs->m_parameters);
      for (unsigned i = 0; i < param_count; i++)
	(*ifs->m_parameters)[i].split_candidate = false;
    }
  if (!issues.there_is_one)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "There is no call to %s that we can modify.  "
		 "Disabling all modifications.\n", node->dump_name ());
      return true;
    }
  return false;
}

/* Find the access with corresponding OFFSET and SIZE among accesses in
   PARAM_DESC and return it or NULL if such an access is not there.  */

static param_access *
find_param_access (isra_param_desc *param_desc, unsigned offset, unsigned size)
{
  unsigned pclen = vec_safe_length (param_desc->accesses);

  /* The search is linear but the number of stored accesses is bound by
     PARAM_IPA_SRA_MAX_REPLACEMENTS, so most probably 8.  */

  for (unsigned i = 0; i < pclen; i++)
    if ((*param_desc->accesses)[i]->unit_offset == offset
	&& (*param_desc->accesses)[i]->unit_size == size)
      return (*param_desc->accesses)[i];

  return NULL;
}

/* Return iff the total size of definite replacement SIZE would violate the
   limit set for it in PARAM.  */

static bool
size_would_violate_limit_p (isra_param_desc *desc, unsigned size)
{
  unsigned limit = desc->param_size_limit;
  if (size > limit
      || (!desc->by_ref && size == limit))
    return true;
  return false;
}

/* Increase reached size of DESC by SIZE or disqualify it if it would violate
   the set limit.  IDX is the parameter number which is dumped when
   disqualifying.  */

static void
bump_reached_size (isra_param_desc *desc, unsigned size, unsigned idx)
{
  unsigned after = desc->size_reached + size;
  if (size_would_violate_limit_p (desc, after))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "    ...size limit reached, disqualifying "
		 "candidate parameter %u\n", idx);
      desc->split_candidate = false;
      return;
    }
  desc->size_reached = after;
}

/* Take all actions required to deal with an edge CS that represents a call to
   an unknown or un-analyzed function, for both parameter removal and
   splitting.  */

static void
process_edge_to_unknown_caller (cgraph_edge *cs)
{
  isra_func_summary *from_ifs = func_sums->get (cs->caller);
  gcc_checking_assert (from_ifs);
  isra_call_summary *csum = call_sums->get (cs);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing an edge to an unknown caller from %s:\n",
	     cs->caller->dump_name ());

  unsigned args_count = csum->m_arg_flow.length ();
  for (unsigned i = 0; i < args_count; i++)
    {
      isra_param_flow *ipf = &csum->m_arg_flow[i];

     if (ipf->pointer_pass_through)
       {
         isra_param_desc *param_desc
           = &(*from_ifs->m_parameters)[get_single_param_flow_source (ipf)];
         param_desc->locally_unused = false;
         param_desc->split_candidate = false;
        continue;
       }
      if (ipf->aggregate_pass_through)
	{
	  unsigned idx = get_single_param_flow_source (ipf);
	  isra_param_desc *param_desc = &(*from_ifs->m_parameters)[idx];

	  param_desc->locally_unused = false;
	  if (!param_desc->split_candidate)
	    continue;
	  gcc_assert (!param_desc->by_ref);
	  param_access *pacc = find_param_access (param_desc, ipf->unit_offset,
						  ipf->unit_size);
	  gcc_checking_assert (pacc);
	  pacc->certain = true;
	  if (overlapping_certain_accesses_p (param_desc, NULL))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "    ...leading to overlap, "
			 " disqualifying candidate parameter %u\n",
			 idx);
	      param_desc->split_candidate = false;
	    }
	  else
	    bump_reached_size (param_desc, pacc->unit_size, idx);
	  ipf->aggregate_pass_through = false;
	  continue;
	}

      for (int j = 0; j < ipf->length; j++)
	{
	  int input_idx = ipf->inputs[j];
	  (*from_ifs->m_parameters)[input_idx].locally_unused = false;
	}
    }
}

/* Propagate parameter removal information through cross-SCC edge CS,
   i.e. decrease the use count in the caller parameter descriptor for each use
   in this call.  */

static void
param_removal_cross_scc_edge (cgraph_edge *cs)
{
  enum availability availability;
  cgraph_node *callee = cs->callee->function_symbol (&availability);
  isra_func_summary *to_ifs = func_sums->get (callee);
  if (!to_ifs || !to_ifs->m_candidate
      || (availability < AVAIL_AVAILABLE)
      || vec_safe_is_empty (to_ifs->m_parameters))
    {
      process_edge_to_unknown_caller (cs);
      return;
    }
  isra_func_summary *from_ifs = func_sums->get (cs->caller);
  gcc_checking_assert (from_ifs);

  isra_call_summary *csum = call_sums->get (cs);
  unsigned args_count = csum->m_arg_flow.length ();
  unsigned param_count = vec_safe_length (to_ifs->m_parameters);

  for (unsigned i = 0; i < args_count; i++)
    {
      bool unused_in_callee;
      if (i < param_count)
	unused_in_callee = (*to_ifs->m_parameters)[i].locally_unused;
      else
	unused_in_callee = false;

      if (!unused_in_callee)
	{
	  isra_param_flow *ipf = &csum->m_arg_flow[i];
	  for (int j = 0; j < ipf->length; j++)
	    {
	      int input_idx = ipf->inputs[j];
	      (*from_ifs->m_parameters)[input_idx].locally_unused = false;
	    }
	}
    }
}

/* Unless it is already there, push NODE which is also described by IFS to
   STACK.  */

static void
isra_push_node_to_stack (cgraph_node *node, isra_func_summary *ifs,
		    vec<cgraph_node *> *stack)
{
  if (!ifs->m_queued)
    {
      ifs->m_queued = true;
      stack->safe_push (node);
    }
}

/* If parameter with index INPUT_IDX is marked as locally unused, mark it as
   used and push CALLER on STACK.  */

static void
isra_mark_caller_param_used (isra_func_summary *from_ifs, int input_idx,
			     cgraph_node *caller, vec<cgraph_node *> *stack)
{
  if ((*from_ifs->m_parameters)[input_idx].locally_unused)
    {
      (*from_ifs->m_parameters)[input_idx].locally_unused = false;
      isra_push_node_to_stack (caller, from_ifs, stack);
    }
}

/* Combine safe_size of DESC with SIZE and return true if it has changed.  */

static bool
update_safe_size (isra_param_desc *desc, unsigned size)
{
  if (!desc->safe_size_set)
    {
      desc->safe_size_set = 1;
      desc->safe_size = size;
      return true;
    }
  if (desc->safe_size <= size)
    return false;
  desc->safe_size = size;
  return true;
}

/* Set all param hints in DESC to the pessimistic values.  Return true if any
   hints that need to potentially trigger further propagation have changed.  */

static bool
flip_all_hints_pessimistic (isra_param_desc *desc)
{
  desc->not_specially_constructed = true;
  return update_safe_size (desc, 0);
}

/* Because we have not analyzed or otherwise problematic caller, go over all
   parameter int flags of IFS describing a call graph node of a calllee and
   turn them pessimistic.  Return true if any hints that need to potentially
   trigger further propagation have changed.  */

static bool
flip_all_param_hints_pessimistic (isra_func_summary *ifs)
{
  if (!ifs || !ifs->m_candidate)
    return false;

  bool ret = false;
  unsigned param_count = vec_safe_length (ifs->m_parameters);

  for (unsigned i = 0; i < param_count; i++)
    ret |= flip_all_hints_pessimistic (&(*ifs->m_parameters)[i]);

  return ret;
}

/* Propagate hints accross edge CS which ultimately leads to a node described
   by TO_IFS.  Return true if any hints of the callee which should potentially
   trigger further propagation have changed.  */

static bool
propagate_param_hints_accross_call (cgraph_edge *cs, isra_func_summary *to_ifs)
{
  if (!to_ifs || !to_ifs->m_candidate)
    return false;

  isra_call_summary *csum = call_sums->get (cs);
  bool ret = false;
  unsigned args_count = csum->m_arg_flow.length ();
  unsigned param_count = vec_safe_length (to_ifs->m_parameters);

  for (unsigned i = 0; i < param_count; i++)
    {
      isra_param_desc *desc = &(*to_ifs->m_parameters)[i];
      if (i >= args_count)
	{
	  ret |= flip_all_hints_pessimistic (desc);
	  continue;
	}

      if (desc->by_ref)
	{
	  isra_param_flow *ipf = &csum->m_arg_flow[i];

	  if (!ipf->constructed_for_calls)
	    desc->not_specially_constructed = true;

	  if (ipf->pointer_pass_through)
	    {
	      isra_func_summary *from_ifs = func_sums->get (cs->caller);
	      int srcidx = get_single_param_flow_source (ipf);
	      if (vec_safe_length (from_ifs->m_parameters) > (unsigned) srcidx)
		{
		  isra_param_desc *src_d = &(*from_ifs->m_parameters)[srcidx];
		  if (src_d->safe_size_set)
		    ret |= update_safe_size (desc, src_d->safe_size);
		}
	      else
		ret |= update_safe_size (desc, 0);
	    }
	  else if (!ipf->aggregate_pass_through)
	    ret |= update_safe_size (desc, ipf->unit_size);
	  else
	    /* LTOing type-mismatched programs can end up here.  */
	    ret |= update_safe_size (desc, 0);
	}
    }
  return ret;
}

/* Propagate hints from NODE described by FROM_IFS to all its (dorect) callees,
   push those that may need re-visiting onto STACK.  */

static void
propagate_hints_to_all_callees (cgraph_node *node, isra_func_summary *from_ifs,
				vec<cgraph_node *> *stack)
{
  for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
    {
      enum availability availability;
      cgraph_node *callee = cs->callee->function_symbol (&availability);
      isra_func_summary *to_ifs = func_sums->get (callee);
      if (!from_ifs)
	{
	  if (flip_all_param_hints_pessimistic (to_ifs)
	      && ipa_edge_within_scc (cs))
	    isra_push_node_to_stack (callee, to_ifs, stack);
	}
      else if (propagate_param_hints_accross_call (cs, to_ifs)
	       && ipa_edge_within_scc (cs))
	isra_push_node_to_stack (callee, to_ifs, stack);
    }
}

/* Propagate information that any parameter is not used only locally within a
   SCC across CS to the caller, which must be in the same SCC as the
   callee.  Push any callers that need to be re-processed to STACK.  */

static void
propagate_used_across_scc_edge (cgraph_edge *cs, vec<cgraph_node *> *stack)
{
  isra_func_summary *from_ifs = func_sums->get (cs->caller);
  if (!from_ifs || vec_safe_is_empty (from_ifs->m_parameters))
    return;

  isra_call_summary *csum = call_sums->get (cs);
  gcc_checking_assert (csum);
  unsigned args_count = csum->m_arg_flow.length ();
  enum availability availability;
  cgraph_node *callee = cs->callee->function_symbol (&availability);
  isra_func_summary *to_ifs = func_sums->get (callee);

  unsigned param_count
    = (to_ifs && (availability >= AVAIL_AVAILABLE))
    ? vec_safe_length (to_ifs->m_parameters) : 0;
  for (unsigned i = 0; i < args_count; i++)
    {
      if (i < param_count
	  && (*to_ifs->m_parameters)[i].locally_unused)
	    continue;

      /* The argument is needed in the callee it, we must mark the parameter as
	 used also in the caller and its callers within this SCC.  */
      isra_param_flow *ipf = &csum->m_arg_flow[i];
      for (int j = 0; j < ipf->length; j++)
	{
	  int input_idx = ipf->inputs[j];
	  isra_mark_caller_param_used (from_ifs, input_idx, cs->caller, stack);
	}
    }
}

/* Propagate information that any parameter is not used only locally within a
   SCC (i.e. is used also elsewhere) to all callers of NODE that are in the
   same SCC.  Push any callers that need to be re-processed to STACK.  */

static bool
propagate_used_to_scc_callers (cgraph_node *node, void *data)
{
  vec<cgraph_node *> *stack = (vec<cgraph_node *> *) data;
  cgraph_edge *cs;
  for (cs = node->callers; cs; cs = cs->next_caller)
    if (ipa_edge_within_scc (cs))
      propagate_used_across_scc_edge (cs, stack);
  return false;
}

/* Return true iff all certain accesses in ARG_DESC are also present as
   certain accesses in PARAM_DESC.  */

static bool
all_callee_accesses_present_p (isra_param_desc *param_desc,
			       isra_param_desc *arg_desc)
{
  unsigned aclen = vec_safe_length (arg_desc->accesses);
  for (unsigned j = 0; j < aclen; j++)
    {
      param_access *argacc = (*arg_desc->accesses)[j];
      if (!argacc->certain)
	continue;
      param_access *pacc = find_param_access (param_desc, argacc->unit_offset,
					      argacc->unit_size);
      if (!pacc
	  || !pacc->certain
	  || !types_compatible_p (argacc->type, pacc->type))
	return false;
    }
  return true;
}

/* Type internal to function pull_accesses_from_callee.  Unfortunately gcc 4.8
   does not allow instantiating an auto_vec with a type defined within a
   function so it is a global type.   */
enum acc_prop_kind {ACC_PROP_DONT, ACC_PROP_COPY, ACC_PROP_CERTAIN};


/* Attempt to propagate all definite accesses from ARG_DESC to PARAM_DESC,
   (which belongs to CALLER) if they would not violate some constraint there.
   If successful, return NULL, otherwise return the string reason for failure
   (which can be written to the dump file).  DELTA_OFFSET is the known offset
   of the actual argument withing the formal parameter (so of ARG_DESCS within
   PARAM_DESCS), ARG_SIZE is the size of the actual argument or zero, if not
   known. In case of success, set *CHANGE_P to true if propagation actually
   changed anything.  */

static const char *
pull_accesses_from_callee (cgraph_node *caller, isra_param_desc *param_desc,
			   isra_param_desc *arg_desc,
			   unsigned delta_offset, unsigned arg_size,
			   bool *change_p)
{
  unsigned pclen = vec_safe_length (param_desc->accesses);
  unsigned aclen = vec_safe_length (arg_desc->accesses);
  unsigned prop_count = 0;
  unsigned prop_size = 0;
  bool change = false;

  auto_vec <enum acc_prop_kind, 8> prop_kinds (aclen);
  for (unsigned j = 0; j < aclen; j++)
    {
      param_access *argacc = (*arg_desc->accesses)[j];
      prop_kinds.safe_push (ACC_PROP_DONT);

      if (arg_size > 0
	  && argacc->unit_offset + argacc->unit_size > arg_size)
	return "callee access outsize size boundary";

      if (!argacc->certain)
	continue;

      unsigned offset = argacc->unit_offset + delta_offset;
      /* Given that accesses are initially stored according to increasing
	 offset and decreasing size in case of equal offsets, the following
	 searches could be written more efficiently if we kept the ordering
	 when copying. But the number of accesses is capped at
	 PARAM_IPA_SRA_MAX_REPLACEMENTS (so most likely 8) and the code gets
	 messy quickly, so let's improve on that only if necessary.  */

      bool exact_match = false;
      for (unsigned i = 0; i < pclen; i++)
	{
	  /* Check for overlaps.  */
	  param_access *pacc = (*param_desc->accesses)[i];
	  if (pacc->unit_offset == offset
	      && pacc->unit_size == argacc->unit_size)
	    {
	      if (argacc->alias_ptr_type != pacc->alias_ptr_type
		  || !types_compatible_p (argacc->type, pacc->type)
		  || argacc->reverse != pacc->reverse)
		return "propagated access types would not match existing ones";

	      exact_match = true;
	      if (!pacc->certain)
		{
		  prop_kinds[j] = ACC_PROP_CERTAIN;
		  prop_size += argacc->unit_size;
		  change = true;
		}
	      continue;
	    }

	  if (offset < pacc->unit_offset + pacc->unit_size
	      && offset + argacc->unit_size > pacc->unit_offset)
	    {
	      /* None permissible with load accesses, possible to fit into
		 argument ones.  */
	      if (pacc->certain
		  || offset < pacc->unit_offset
		  || (offset + argacc->unit_size
		      > pacc->unit_offset + pacc->unit_size))
		return "a propagated access would conflict in caller";
	    }
	}

      if (!exact_match)
	{
	  prop_kinds[j] = ACC_PROP_COPY;
	  prop_count++;
	  prop_size += argacc->unit_size;
	  change = true;
	}
    }

    if (!change)
      return NULL;

    if ((prop_count + pclen
	 > (unsigned) opt_for_fn (caller->decl, param_ipa_sra_max_replacements))
	|| size_would_violate_limit_p (param_desc,
				       param_desc->size_reached + prop_size))
      return "propagating accesses would violate the count or size limit";

  *change_p = true;
  for (unsigned j = 0; j < aclen; j++)
    {
      if (prop_kinds[j] == ACC_PROP_COPY)
	{
	  param_access *argacc = (*arg_desc->accesses)[j];

	  param_access *copy = ggc_cleared_alloc<param_access> ();
	  copy->unit_offset = argacc->unit_offset + delta_offset;
	  copy->unit_size = argacc->unit_size;
	  copy->type = argacc->type;
	  copy->alias_ptr_type = argacc->alias_ptr_type;
	  copy->certain = true;
	  copy->reverse = argacc->reverse;
	  vec_safe_push (param_desc->accesses, copy);
	}
      else if (prop_kinds[j] == ACC_PROP_CERTAIN)
	{
	  param_access *argacc = (*arg_desc->accesses)[j];
	  param_access *csp
	    = find_param_access (param_desc, argacc->unit_offset + delta_offset,
				 argacc->unit_size);
	  csp->certain = true;
	}
    }

  param_desc->size_reached += prop_size;

  return NULL;
}

/* Propagate parameter splitting information through call graph edge CS.
   Return true if any changes that might need to be propagated within SCCs have
   been made.  The function also clears the aggregate_pass_through and
   pointer_pass_through in call summaries which do not need to be processed
   again if this CS is revisited when iterating while changes are propagated
   within an SCC.  */

static bool
param_splitting_across_edge (cgraph_edge *cs)
{
  bool res = false;
  bool cross_scc = !ipa_edge_within_scc (cs);
  enum availability availability;
  cgraph_node *callee = cs->callee->function_symbol (&availability);
  isra_func_summary *from_ifs = func_sums->get (cs->caller);
  gcc_checking_assert (from_ifs && from_ifs->m_parameters);

  isra_call_summary *csum = call_sums->get (cs);
  gcc_checking_assert (csum);
  unsigned args_count = csum->m_arg_flow.length ();
  isra_func_summary *to_ifs = func_sums->get (callee);
  unsigned param_count
    = ((to_ifs && to_ifs->m_candidate && (availability >= AVAIL_AVAILABLE))
       ? vec_safe_length (to_ifs->m_parameters)
       : 0);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Splitting across %s->%s:\n",
	     cs->caller->dump_name (), callee->dump_name ());

  unsigned i;
  for (i = 0; (i < args_count) && (i < param_count); i++)
    {
      isra_param_desc *arg_desc = &(*to_ifs->m_parameters)[i];
      isra_param_flow *ipf = &csum->m_arg_flow[i];

      if (arg_desc->locally_unused)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "    ->%u: unused in callee\n", i);
	  ipf->pointer_pass_through = false;
	  continue;
	}

      if (ipf->pointer_pass_through)
	{
	  int idx = get_single_param_flow_source (ipf);
	  isra_param_desc *param_desc = &(*from_ifs->m_parameters)[idx];
	  if (!param_desc->split_candidate)
	    continue;
	  gcc_assert (param_desc->by_ref);

	  if (!arg_desc->split_candidate || !arg_desc->by_ref)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  %u->%u: not candidate or not by "
			 "reference in callee\n", idx, i);
	      param_desc->split_candidate = false;
	      ipf->pointer_pass_through = false;
	      res = true;
	    }
	  else if (!ipf->safe_to_import_accesses)
	    {
	      if (!csum->m_before_any_store
		  || !all_callee_accesses_present_p (param_desc, arg_desc))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  %u->%u: cannot import accesses.\n",
			     idx, i);
		  param_desc->split_candidate = false;
		  ipf->pointer_pass_through = false;
		  res = true;

		}
	      else
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  %u->%u: verified callee accesses "
			     "present.\n", idx, i);
		  if (cross_scc)
		    ipf->pointer_pass_through = false;
		}
	    }
	  else
	    {
	      const char *pull_failure
		= pull_accesses_from_callee (cs->caller, param_desc, arg_desc,
					     0, 0, &res);
	      if (pull_failure)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  %u->%u: by_ref access pull "
			     "failed: %s.\n", idx, i, pull_failure);
		  param_desc->split_candidate = false;
		  ipf->pointer_pass_through = false;
		  res = true;
		}
	      else
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  %u->%u: by_ref access pull "
			     "succeeded.\n", idx, i);
		  if (cross_scc)
		    ipf->pointer_pass_through = false;
		}
	    }
	}
      else if (ipf->aggregate_pass_through)
	{
	  int idx = get_single_param_flow_source (ipf);
	  isra_param_desc *param_desc = &(*from_ifs->m_parameters)[idx];
	  if (!param_desc->split_candidate)
	    continue;
	  gcc_assert (!param_desc->by_ref);
	  param_access *pacc = find_param_access (param_desc, ipf->unit_offset,
						  ipf->unit_size);
	  gcc_checking_assert (pacc);

	  if (pacc->certain)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  %u->%u: already certain\n", idx, i);
	      ipf->aggregate_pass_through = false;
	    }
	  else if (!arg_desc->split_candidate || arg_desc->by_ref)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  %u->%u: not candidate or by "
			 "reference in callee\n", idx, i);

	      pacc->certain = true;
	      if (overlapping_certain_accesses_p (param_desc, NULL))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "    ...leading to overlap, "
			     " disqualifying candidate parameter %u\n",
			     idx);
		  param_desc->split_candidate = false;
		}
	      else
		bump_reached_size (param_desc, pacc->unit_size, idx);

	      ipf->aggregate_pass_through = false;
	      res = true;
	    }
	  else
	    {
	      const char *pull_failure
		= pull_accesses_from_callee (cs->caller, param_desc, arg_desc,
					     ipf->unit_offset,
					     ipf->unit_size, &res);
	      if (pull_failure)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  %u->%u: arg access pull "
			     "failed: %s.\n", idx, i, pull_failure);

		  ipf->aggregate_pass_through = false;
		  pacc->certain = true;

		  if (overlapping_certain_accesses_p (param_desc, NULL))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "    ...leading to overlap, "
				 " disqualifying candidate parameter %u\n",
				 idx);
		      param_desc->split_candidate = false;
		    }
		  else
		    bump_reached_size (param_desc, pacc->unit_size, idx);

		  res = true;
		}
	      else
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  %u->%u: arg access pull "
			     "succeeded.\n", idx, i);
		  if (cross_scc)
		    ipf->aggregate_pass_through = false;
		}
	    }
	}
    }

  /* Handle argument-parameter count mismatches. */
  for (; (i < args_count); i++)
    {
      isra_param_flow *ipf = &csum->m_arg_flow[i];

      if (ipf->pointer_pass_through || ipf->aggregate_pass_through)
	{
	  int idx = get_single_param_flow_source (ipf);
	  ipf->pointer_pass_through = false;
	  ipf->aggregate_pass_through = false;
	  isra_param_desc *param_desc = &(*from_ifs->m_parameters)[idx];
	  if (!param_desc->split_candidate)
	    continue;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  %u->%u: no corresponding formal parameter\n",
		     idx, i);
	  param_desc->split_candidate = false;
	  res = true;
	}
    }
  return res;
}

/* Worker for call_for_symbol_and_aliases, look at all callers and if all their
   callers ignore the return value, or come from the same SCC and use the
   return value only to compute their return value, return false, otherwise
   return true.  */

static bool
retval_used_p (cgraph_node *node, void *)
{
  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    {
      isra_call_summary *csum = call_sums->get (cs);
      gcc_checking_assert (csum);
      if (csum->m_return_ignored)
	continue;
      if (!csum->m_return_returned)
	return true;

      isra_func_summary *from_ifs = func_sums->get (cs->caller);
      if (!from_ifs || !from_ifs->m_candidate)
	return true;

      if (!ipa_edge_within_scc (cs)
	  && !from_ifs->m_return_ignored)
	    return true;
    }

  return false;
}

/* Push into NEW_PARAMS all required parameter adjustment entries to copy or
   modify parameter which originally had index BASE_INDEX, in the adjustment
   vector of parent clone (if any) had PREV_CLONE_INDEX and was described by
   PREV_ADJUSTMENT.  If IPA-CP has created a transformation summary for the
   original node, it needs to be passed in IPCP_TS, otherwise it should be
   NULL.  If the parent clone is the original function, PREV_ADJUSTMENT is NULL
   and PREV_CLONE_INDEX is equal to BASE_INDEX.  */

static void
push_param_adjustments_for_index (isra_func_summary *ifs, unsigned base_index,
				  unsigned prev_clone_index,
				  ipa_adjusted_param *prev_adjustment,
				  ipcp_transformation *ipcp_ts,
				  vec<ipa_adjusted_param, va_gc> **new_params)
{
  isra_param_desc *desc = &(*ifs->m_parameters)[base_index];
  if (desc->locally_unused)
    {
      if (dump_file)
	fprintf (dump_file, "  Will remove parameter %u\n", base_index);
      return;
    }

  if (!desc->split_candidate)
    {
      ipa_adjusted_param adj;
      if (prev_adjustment)
	{
	  adj = *prev_adjustment;
	  adj.prev_clone_adjustment = true;
	  adj.prev_clone_index = prev_clone_index;
	}
      else
	{
	  memset (&adj, 0, sizeof (adj));
	  adj.op = IPA_PARAM_OP_COPY;
	  adj.base_index = base_index;
	  adj.prev_clone_index = prev_clone_index;
	}
      vec_safe_push ((*new_params), adj);
      return;
    }

  if (dump_file)
    fprintf (dump_file, "  Will split parameter %u\n", base_index);

  gcc_assert (!prev_adjustment || prev_adjustment->op == IPA_PARAM_OP_COPY);
  unsigned aclen = vec_safe_length (desc->accesses);
  for (unsigned j = 0; j < aclen; j++)
    {
      param_access *pa = (*desc->accesses)[j];
      if (!pa->certain)
	continue;

      if (ipcp_ts)
	{
	  ipa_argagg_value_list avl (ipcp_ts);
	  tree value = avl.get_value (base_index, pa->unit_offset);
	  if (value && !AGGREGATE_TYPE_P (pa->type))
	    {
	      if (dump_file)
		fprintf (dump_file, "    - omitting component at byte "
			 "offset %u which is known to have a constant value\n ",
			 pa->unit_offset);
	      continue;
	    }
	}

      if (dump_file)
	fprintf (dump_file, "    - component at byte offset %u, "
		 "size %u\n", pa->unit_offset, pa->unit_size);

      ipa_adjusted_param adj;
      memset (&adj, 0, sizeof (adj));
      adj.op = IPA_PARAM_OP_SPLIT;
      adj.base_index = base_index;
      adj.prev_clone_index = prev_clone_index;
      adj.param_prefix_index = IPA_PARAM_PREFIX_ISRA;
      adj.reverse = pa->reverse;
      adj.type = pa->type;
      adj.alias_ptr_type = pa->alias_ptr_type;
      adj.unit_offset = pa->unit_offset;
      vec_safe_push ((*new_params), adj);
    }
}

/* Worker for all call_for_symbol_thunks_and_aliases.  Set calls_comdat_local
   flag of all callers of NODE.  */

static bool
mark_callers_calls_comdat_local (struct cgraph_node *node, void *)
{
  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    cs->caller->calls_comdat_local = true;
  return false;
}

/* Remove any IPA-CP results stored in TS that are associated with removed
   parameters as marked in IFS. */

static void
zap_useless_ipcp_results (const isra_func_summary *ifs, ipcp_transformation *ts)
{
  unsigned ts_len = vec_safe_length (ts->m_agg_values);

  if (ts_len == 0)
    return;

  bool removed_item = false;
  unsigned dst_index = 0;

  for (unsigned i = 0; i < ts_len; i++)
    {
      ipa_argagg_value *v = &(*ts->m_agg_values)[i];
      const isra_param_desc *desc = &(*ifs->m_parameters)[v->index];

      if (!desc->locally_unused)
	{
	  if (removed_item)
	    (*ts->m_agg_values)[dst_index] = *v;
	  dst_index++;
	}
      else
	removed_item = true;
    }
  if (dst_index == 0)
    {
      ggc_free (ts->m_agg_values);
      ts->m_agg_values = NULL;
    }
  else if (removed_item)
    ts->m_agg_values->truncate (dst_index);

  bool useful_bits = false;
  unsigned count = vec_safe_length (ts->bits);
  for (unsigned i = 0; i < count; i++)
    if ((*ts->bits)[i])
    {
      const isra_param_desc *desc = &(*ifs->m_parameters)[i];
      if (desc->locally_unused)
	(*ts->bits)[i] = NULL;
      else
	useful_bits = true;
    }
  if (!useful_bits)
    ts->bits = NULL;

  bool useful_vr = false;
  count = vec_safe_length (ts->m_vr);
  for (unsigned i = 0; i < count; i++)
    if ((*ts->m_vr)[i].known)
      {
	const isra_param_desc *desc = &(*ifs->m_parameters)[i];
	if (desc->locally_unused)
	  (*ts->m_vr)[i].known = false;
	else
	  useful_vr = true;
      }
  if (!useful_vr)
    ts->m_vr = NULL;
}

/* Do final processing of results of IPA propagation regarding NODE, clone it
   if appropriate.  */

static void
process_isra_node_results (cgraph_node *node,
			   hash_map<const char *, unsigned> *clone_num_suffixes)
{
  isra_func_summary *ifs = func_sums->get (node);
  if (!ifs || !ifs->m_candidate)
    return;

  auto_vec<bool, 16> surviving_params;
  bool check_surviving = false;
  clone_info *cinfo = clone_info::get (node);
  if (cinfo && cinfo->param_adjustments)
    {
      check_surviving = true;
      cinfo->param_adjustments->get_surviving_params (&surviving_params);
    }

  unsigned param_count = vec_safe_length (ifs->m_parameters);
  bool will_change_function = false;
  if (ifs->m_returns_value && ifs->m_return_ignored)
    will_change_function = true;
  else
    for (unsigned i = 0; i < param_count; i++)
      {
	isra_param_desc *desc = &(*ifs->m_parameters)[i];
	if ((desc->locally_unused || desc->split_candidate)
	    /* Make sure we do not clone just to attempt to remove an already
	       removed unused argument.  */
	    && (!check_surviving
		|| (i < surviving_params.length ()
		    && surviving_params[i])))
	  {
	    will_change_function = true;
	    break;
	  }
      }
  if (!will_change_function)
    return;

  if (dump_file)
    {
      fprintf (dump_file, "\nEvaluating analysis results for %s\n",
	       node->dump_name ());
      if (ifs->m_returns_value && ifs->m_return_ignored)
	fprintf (dump_file, "  Will remove return value.\n");
    }

  ipcp_transformation *ipcp_ts = ipcp_get_transformation_summary (node);
  if (ipcp_ts)
    zap_useless_ipcp_results (ifs, ipcp_ts);
  vec<ipa_adjusted_param, va_gc> *new_params = NULL;
  if (ipa_param_adjustments *old_adjustments
	 = cinfo ? cinfo->param_adjustments : NULL)
    {
      unsigned old_adj_len = vec_safe_length (old_adjustments->m_adj_params);
      for (unsigned i = 0; i < old_adj_len; i++)
	{
	  ipa_adjusted_param *old_adj = &(*old_adjustments->m_adj_params)[i];
	  push_param_adjustments_for_index (ifs, old_adj->base_index, i,
					    old_adj, ipcp_ts, &new_params);
	}
    }
  else
    for (unsigned i = 0; i < param_count; i++)
      push_param_adjustments_for_index (ifs, i, i, NULL, ipcp_ts, &new_params);

  ipa_param_adjustments *new_adjustments
    = (new (ggc_alloc <ipa_param_adjustments> ())
       ipa_param_adjustments (new_params, param_count,
			      ifs->m_returns_value && ifs->m_return_ignored));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n  Created adjustments:\n");
      new_adjustments->dump (dump_file);
    }

  unsigned &suffix_counter = clone_num_suffixes->get_or_insert (
			       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (
				 node->decl)));
  auto_vec<cgraph_edge *> callers = node->collect_callers ();
  cgraph_node *new_node
    = node->create_virtual_clone (callers, NULL, new_adjustments, "isra",
				  suffix_counter);
  suffix_counter++;
  if (node->calls_comdat_local && node->same_comdat_group)
    {
      new_node->add_to_same_comdat_group (node);
      new_node->call_for_symbol_and_aliases (mark_callers_calls_comdat_local,
					     NULL, true);
    }
  new_node->calls_comdat_local = node->calls_comdat_local;

  if (dump_file)
    fprintf (dump_file, "  Created new node %s\n", new_node->dump_name ());
  callers.release ();
}

/* If INDICES is not empty, dump a combination of NODE's dump_name and MSG
   followed by the list of numbers in INDICES.  */

static void
dump_list_of_param_indices (const cgraph_node *node, const char* msg,
			    const vec<unsigned> &indices)
{
  if (indices.is_empty ())
    return;
  fprintf (dump_file, "The following parameters of %s %s:", node->dump_name (),
	   msg);
  for (unsigned i : indices)
    fprintf (dump_file, " %u", i);
  fprintf (dump_file, "\n");
}

/* Check which parameters of NODE described by IFS have survived until IPA-SRA
   and disable transformations for those which have not or which should not
   transformed because the associated debug counter reached its limit.  Return
   true if none survived or if there were no candidates to begin with.
   Additionally, also adjust parameter descriptions based on debug counters and
   hints propagated earlier.  */

static bool
adjust_parameter_descriptions (cgraph_node *node, isra_func_summary *ifs)
{
  bool ret = true;
  unsigned len = vec_safe_length (ifs->m_parameters);
  if (!len)
    return true;

  auto_vec<bool, 16> surviving_params;
  bool check_surviving = false;
  clone_info *cinfo = clone_info::get (node);
  if (cinfo && cinfo->param_adjustments)
    {
      check_surviving = true;
      cinfo->param_adjustments->get_surviving_params (&surviving_params);
    }
  ipcp_transformation *ipcp_ts = ipcp_get_transformation_summary (node);
  auto_vec <unsigned> dump_dead_indices;
  auto_vec <unsigned> dump_bad_cond_indices;
  for (unsigned i = 0; i < len; i++)
    {
      isra_param_desc *desc = &(*ifs->m_parameters)[i];
      if (!dbg_cnt (ipa_sra_params))
	{
	  desc->locally_unused = false;
	  desc->split_candidate = false;
	}
      else if (check_surviving
	       && (i >= surviving_params.length ()
		   || !surviving_params[i]))
	{
	  /* Even if the parameter was removed by a previous IPA pass, we do
	     not clear locally_unused because if it really is unused, this
	     information might be useful in callers.  */
	  desc->split_candidate = false;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    dump_dead_indices.safe_push (i);
	}
      else
	{
	  if (desc->split_candidate && desc->conditionally_dereferenceable)
	    {
	      gcc_assert (desc->safe_size_set);
	      for (param_access *pa : *desc->accesses)
		if ((pa->unit_offset + pa->unit_size) > desc->safe_size)
		  {
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      dump_bad_cond_indices.safe_push (i);
		    desc->split_candidate = false;
		    break;
		  }
	    }

	  if (desc->split_candidate)
	    {
	      if (desc->by_ref && !desc->not_specially_constructed)
		{
		  int extra_factor
		    = opt_for_fn (node->decl,
				  param_ipa_sra_ptrwrap_growth_factor);
		  desc->param_size_limit = extra_factor * desc->param_size_limit;
		}
	      if (size_would_violate_limit_p (desc, desc->size_reached))
		desc->split_candidate = false;
	    }

	  /* Avoid ICEs on size-mismatched VIEW_CONVERT_EXPRs when callers and
	     callees don't agree on types in aggregates and we try to do both
	     IPA-CP and IPA-SRA.  */
	  if (ipcp_ts && desc->split_candidate)
	    {
	      ipa_argagg_value_list avl (ipcp_ts);
	      for (const param_access *pa : desc->accesses)
		{
		  if (!pa->certain)
		    continue;
		  tree value = avl.get_value (i, pa->unit_offset);
		  if (value
		      && ((tree_to_uhwi (TYPE_SIZE (TREE_TYPE (value)))
			   / BITS_PER_UNIT)
			  != pa->unit_size))
		    {
		      desc->split_candidate = false;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			dump_dead_indices.safe_push (i);
		      break;
		    }
		}
	    }

	  if (desc->locally_unused || desc->split_candidate)
	    ret = false;
	}
    }

  dump_list_of_param_indices (node, "are dead on arrival or have a type "
			      "mismatch with IPA-CP", dump_dead_indices);
  dump_list_of_param_indices (node, "are not safe to dereference in all "
			      "callers", dump_bad_cond_indices);

  return ret;
}


/* Run the interprocedural part of IPA-SRA. */

static unsigned int
ipa_sra_analysis (void)
{
  if (dump_file)
    {
      fprintf (dump_file, "\n========== IPA-SRA IPA stage ==========\n");
      ipa_sra_dump_all_summaries (dump_file, false);
    }

  gcc_checking_assert (func_sums);
  gcc_checking_assert (call_sums);
  cgraph_node **order = XCNEWVEC (cgraph_node *, symtab->cgraph_count);
  auto_vec <cgraph_node *, 16> stack;
  int node_scc_count = ipa_reduced_postorder (order, true, NULL);

  /* One sweep from callers to callees for return value removal.  */
  for (int i = node_scc_count - 1; i >= 0 ; i--)
    {
      cgraph_node *scc_rep = order[i];
      vec<cgraph_node *> cycle_nodes = ipa_get_nodes_in_cycle (scc_rep);

      /* Preliminary IPA function level checks.  */
      for (cgraph_node *v : cycle_nodes)
	{
	  isra_func_summary *ifs = func_sums->get (v);
	  if (!ifs || !ifs->m_candidate)
	    continue;
	  if (!ipa_sra_ipa_function_checks (v)
	      || check_all_callers_for_issues (v))
	    ifs->zap ();
	}

      for (cgraph_node *v : cycle_nodes)
	{
	  isra_func_summary *ifs = func_sums->get (v);
	  if (!ifs || !ifs->m_candidate)
	    continue;
	  bool return_needed
	    = (ifs->m_returns_value
	       && (!dbg_cnt (ipa_sra_retvalues)
		   || v->call_for_symbol_and_aliases (retval_used_p,
						      NULL, true)));
	  ifs->m_return_ignored = !return_needed;
	  if (return_needed)
	    isra_push_node_to_stack (v, ifs, &stack);
	}

      while (!stack.is_empty ())
	{
	  cgraph_node *node = stack.pop ();
	  isra_func_summary *ifs = func_sums->get (node);
	  gcc_checking_assert (ifs && ifs->m_queued);
	  ifs->m_queued = false;

	  for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
	    if (ipa_edge_within_scc (cs)
		&& call_sums->get (cs)->m_return_returned)
	      {
		enum availability av;
		cgraph_node *callee = cs->callee->function_symbol (&av);
		isra_func_summary *to_ifs = func_sums->get (callee);
		if (to_ifs && to_ifs->m_return_ignored)
		  {
		    to_ifs->m_return_ignored = false;
		    isra_push_node_to_stack (callee, to_ifs, &stack);
		  }
	      }
	}

      /* Parameter hint propagation.  */
      for (cgraph_node *v : cycle_nodes)
	{
	  isra_func_summary *ifs = func_sums->get (v);
	  propagate_hints_to_all_callees (v, ifs, &stack);
	}

      while (!stack.is_empty ())
	{
	  cgraph_node *node = stack.pop ();
	  isra_func_summary *ifs = func_sums->get (node);
	  gcc_checking_assert (ifs && ifs->m_queued);
	  ifs->m_queued = false;
	  propagate_hints_to_all_callees (node, ifs, &stack);
	}

      cycle_nodes.release ();
    }

  /* One sweep from callees to callers for parameter removal and splitting.  */
  for (int i = 0; i < node_scc_count; i++)
    {
      cgraph_node *scc_rep = order[i];
      vec<cgraph_node *> cycle_nodes = ipa_get_nodes_in_cycle (scc_rep);

      /* First step of parameter removal.  */
      for (cgraph_node *v : cycle_nodes)
	{
	  isra_func_summary *ifs = func_sums->get (v);
	  if (!ifs || !ifs->m_candidate)
	    continue;
	  if (adjust_parameter_descriptions (v, ifs))
	    continue;
	  for (cgraph_edge *cs = v->indirect_calls; cs; cs = cs->next_callee)
	    process_edge_to_unknown_caller (cs);
	  for (cgraph_edge *cs = v->callees; cs; cs = cs->next_callee)
	    if (!ipa_edge_within_scc (cs))
	      param_removal_cross_scc_edge (cs);
	}

      /* Look at edges within the current SCC and propagate used-ness across
	 them, pushing onto the stack all notes which might need to be
	 revisited.  */
      for (cgraph_node *v : cycle_nodes)
	v->call_for_symbol_thunks_and_aliases (propagate_used_to_scc_callers,
					       &stack, true);

      /* Keep revisiting and pushing until nothing changes.  */
      while (!stack.is_empty ())
	{
	  cgraph_node *v = stack.pop ();
	  isra_func_summary *ifs = func_sums->get (v);
	  gcc_checking_assert (ifs && ifs->m_queued);
	  ifs->m_queued = false;

	  v->call_for_symbol_thunks_and_aliases (propagate_used_to_scc_callers,
						 &stack, true);
	}

      /* Parameter splitting.  */
      bool repeat_scc_access_propagation;
      do
	{
	  repeat_scc_access_propagation = false;
	  for (cgraph_node *v : cycle_nodes)
	    {
	      isra_func_summary *ifs = func_sums->get (v);
	      if (!ifs
		  || !ifs->m_candidate
		  || vec_safe_is_empty (ifs->m_parameters))
		continue;
	      for (cgraph_edge *cs = v->callees; cs; cs = cs->next_callee)
		if (param_splitting_across_edge (cs))
		  repeat_scc_access_propagation = true;
	    }
	}
      while (repeat_scc_access_propagation);

      if (flag_checking)
	for (cgraph_node *v : cycle_nodes)
	  verify_splitting_accesses (v, true);

      cycle_nodes.release ();
    }

  ipa_free_postorder_info ();
  free (order);

  if (dump_file)
    {
      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "\n========== IPA-SRA propagation final state "
		   " ==========\n");
	  ipa_sra_dump_all_summaries (dump_file, true);
	}
      fprintf (dump_file, "\n========== IPA-SRA decisions ==========\n");
    }

  hash_map<const char *, unsigned> *clone_num_suffixes
    = new hash_map<const char *, unsigned>;

  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    process_isra_node_results (node, clone_num_suffixes);

  delete clone_num_suffixes;
  ggc_delete (func_sums);
  func_sums = NULL;
  delete call_sums;
  call_sums = NULL;

  if (dump_file)
    fprintf (dump_file, "\n========== IPA SRA IPA analysis done "
	     "==========\n\n");
  return 0;
}


const pass_data pass_data_ipa_sra =
{
  IPA_PASS, /* type */
  "sra", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_SRA, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_dump_symtab | TODO_remove_functions ), /* todo_flags_finish */
};

class pass_ipa_sra : public ipa_opt_pass_d
{
public:
  pass_ipa_sra (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_sra, ctxt,
		      ipa_sra_generate_summary, /* generate_summary */
		      ipa_sra_write_summary, /* write_summary */
		      ipa_sra_read_summary, /* read_summary */
		      NULL , /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      /* TODO: We should remove the optimize check after we ensure we never run
	 IPA passes when not optimizing.  */
      return (flag_ipa_sra && optimize);
    }

  unsigned int execute (function *)  final override
  {
    return ipa_sra_analysis ();
  }

}; // class pass_ipa_sra

} // anon namespace

/* Intraprocedural part of IPA-SRA analysis.  Scan function body of NODE and
   create a summary structure describing IPA-SRA opportunities and constraints
   in it.  */

static void
ipa_sra_summarize_function (cgraph_node *node)
{
  if (dump_file)
    fprintf (dump_file, "Creating summary for %s/%i:\n", node->name (),
	     node->order);
  gcc_obstack_init (&gensum_obstack);
  loaded_decls = new hash_set<tree>;

  isra_func_summary *ifs = NULL;
  unsigned count = 0;
  if (ipa_sra_preliminary_function_checks (node))
    {
      ifs = func_sums->get_create (node);
      ifs->m_candidate = true;
      tree ret = TREE_TYPE (TREE_TYPE (node->decl));
      ifs->m_returns_value = (TREE_CODE (ret) != VOID_TYPE);
      for (tree parm = DECL_ARGUMENTS (node->decl);
	   parm;
	   parm = DECL_CHAIN (parm))
	count++;
    }
  auto_vec<gensum_param_desc, 16> param_descriptions (count);

  struct function *fun = DECL_STRUCT_FUNCTION (node->decl);
  bool cfun_pushed = false;
  if (count > 0)
    {
      decl2desc = new hash_map<tree, gensum_param_desc *>;
      param_descriptions.reserve_exact (count);
      param_descriptions.quick_grow_cleared (count);

      if (create_parameter_descriptors (node, &param_descriptions))
	{
	  push_cfun (fun);
	  cfun_pushed = true;
	  final_bbs = BITMAP_ALLOC (NULL);
	  bb_dereferences = XCNEWVEC (HOST_WIDE_INT,
				      unsafe_by_ref_count
				      * last_basic_block_for_fn (fun));
	  aa_walking_limit = opt_for_fn (node->decl, param_ipa_max_aa_steps);
	}
    }
  /* Scan function is run even when there are no removal or splitting
     candidates so that we can calculate hints on call edges which can be
     useful in callees. */
  scan_function (node, fun);

  if (count > 0)
    {
      if (dump_file)
	{
	  dump_gensum_param_descriptors (dump_file, node->decl,
					 &param_descriptions);
	  fprintf (dump_file, "----------------------------------------\n");
	}

      process_scan_results (node, fun, ifs, &param_descriptions);

      if (cfun_pushed)
	pop_cfun ();
      if (bb_dereferences)
	{
	  free (bb_dereferences);
	  bb_dereferences = NULL;
	  BITMAP_FREE (final_bbs);
	  final_bbs = NULL;
	}
    }
  isra_analyze_all_outgoing_calls (node);

  delete loaded_decls;
  loaded_decls = NULL;
  if (decl2desc)
    {
      delete decl2desc;
      decl2desc = NULL;
    }
  obstack_free (&gensum_obstack, NULL);
  if (dump_file)
    fprintf (dump_file, "\n\n");
  if (flag_checking)
    verify_splitting_accesses (node, false);
  return;
}

ipa_opt_pass_d *
make_pass_ipa_sra (gcc::context *ctxt)
{
  return new pass_ipa_sra (ctxt);
}


#include "gt-ipa-sra.h"
