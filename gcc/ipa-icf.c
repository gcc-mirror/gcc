/* Interprocedural Identical Code Folding pass
   Copyright (C) 2014-2021 Free Software Foundation, Inc.

   Contributed by Jan Hubicka <hubicka@ucw.cz> and Martin Liska <mliska@suse.cz>

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

/* Interprocedural Identical Code Folding for functions and
   read-only variables.

   The goal of this transformation is to discover functions and read-only
   variables which do have exactly the same semantics.

   In case of functions,
   we could either create a virtual clone or do a simple function wrapper
   that will call equivalent function. If the function is just locally visible,
   all function calls can be redirected. For read-only variables, we create
   aliases if possible.

   Optimization pass arranges as follows:
   1) All functions and read-only variables are visited and internal
      data structure, either sem_function or sem_variables is created.
   2) For every symbol from the previous step, VAR_DECL and FUNCTION_DECL are
      saved and matched to corresponding sem_items.
   3) These declaration are ignored for equality check and are solved
      by Value Numbering algorithm published by Alpert, Zadeck in 1992.
   4) We compute hash value for each symbol.
   5) Congruence classes are created based on hash value. If hash value are
      equal, equals function is called and symbols are deeply compared.
      We must prove that all SSA names, declarations and other items
      correspond.
   6) Value Numbering is executed for these classes. At the end of the process
      all symbol members in remaining classes can be merged.
   7) Merge operation creates alias in case of read-only variables. For
      callgraph node, we must decide if we can redirect local calls,
      create an alias or a thunk.

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "coverage.h"
#include "gimple-pretty-print.h"
#include "data-streamer.h"
#include "tree-streamer.h"
#include "fold-const.h"
#include "calls.h"
#include "varasm.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "except.h"
#include "attribs.h"
#include "print-tree.h"
#include "ipa-utils.h"
#include "tree-ssa-alias-compare.h"
#include "ipa-icf-gimple.h"
#include "fibonacci_heap.h"
#include "ipa-icf.h"
#include "stor-layout.h"
#include "dbgcnt.h"
#include "tree-vector-builder.h"
#include "symtab-thunks.h"
#include "alias.h"
#include "asan.h"

using namespace ipa_icf_gimple;

namespace ipa_icf {

/* Initialization and computation of symtab node hash, there data
   are propagated later on.  */

static sem_item_optimizer *optimizer = NULL;

/* Constructor.  */

symbol_compare_collection::symbol_compare_collection (symtab_node *node)
{
  m_references.create (0);
  m_interposables.create (0);

  ipa_ref *ref;

  if (is_a <varpool_node *> (node) && DECL_VIRTUAL_P (node->decl))
    return;

  for (unsigned i = 0; node->iterate_reference (i, ref); i++)
    {
      if (ref->address_matters_p ())
	m_references.safe_push (ref->referred);

      if (ref->referred->get_availability () <= AVAIL_INTERPOSABLE)
        {
	  if (ref->address_matters_p ())
	    m_references.safe_push (ref->referred);
	  else
	    m_interposables.safe_push (ref->referred);
	}
    }

  if (is_a <cgraph_node *> (node))
    {
      cgraph_node *cnode = dyn_cast <cgraph_node *> (node);

      for (cgraph_edge *e = cnode->callees; e; e = e->next_callee)
	if (e->callee->get_availability () <= AVAIL_INTERPOSABLE)
	  m_interposables.safe_push (e->callee);
    }
}

/* Constructor for key value pair, where _ITEM is key and _INDEX is a target.  */

sem_usage_pair::sem_usage_pair (sem_item *_item, unsigned int _index)
: item (_item), index (_index)
{
}

sem_item::sem_item (sem_item_type _type, bitmap_obstack *stack)
: type (_type), referenced_by_count (0), m_hash (-1), m_hash_set (false)
{
  setup (stack);
}

sem_item::sem_item (sem_item_type _type, symtab_node *_node,
		    bitmap_obstack *stack)
: type (_type), node (_node), referenced_by_count (0), m_hash (-1),
  m_hash_set (false)
{
  decl = node->decl;
  setup (stack);
}

/* Add reference to a semantic TARGET.  */

void
sem_item::add_reference (ref_map *refs,
			 sem_item *target)
{
  unsigned index = reference_count++;
  bool existed;

  sem_usage_pair *pair = new sem_usage_pair (target, index);
  vec<sem_item *> &v = refs->get_or_insert (pair, &existed);
  if (existed)
    delete pair;

  v.safe_push (this);
  bitmap_set_bit (target->usage_index_bitmap, index);
  refs_set.add (target->node);
  ++target->referenced_by_count;
}

/* Initialize internal data structures. Bitmap STACK is used for
   bitmap memory allocation process.  */

void
sem_item::setup (bitmap_obstack *stack)
{
  gcc_checking_assert (node);

  reference_count = 0;
  tree_refs.create (0);
  usage_index_bitmap = BITMAP_ALLOC (stack);
}

sem_item::~sem_item ()
{
  tree_refs.release ();

  BITMAP_FREE (usage_index_bitmap);
}

/* Dump function for debugging purpose.  */

DEBUG_FUNCTION void
sem_item::dump (void)
{
  if (dump_file)
    {
      fprintf (dump_file, "[%s] %s (tree:%p)\n", type == FUNC ? "func" : "var",
	       node->dump_name (), (void *) node->decl);
      fprintf (dump_file, "  hash: %u\n", get_hash ());
    }
}

/* Return true if target supports alias symbols.  */

bool
sem_item::target_supports_symbol_aliases_p (void)
{
#if !defined (ASM_OUTPUT_DEF) || (!defined(ASM_OUTPUT_WEAK_ALIAS) && !defined (ASM_WEAKEN_DECL))
  return false;
#else
  return true;
#endif
}

void sem_item::set_hash (hashval_t hash)
{
  m_hash = hash;
  m_hash_set = true;
}

hash_map<const_tree, hashval_t> sem_item::m_type_hash_cache;

/* Semantic function constructor that uses STACK as bitmap memory stack.  */

sem_function::sem_function (bitmap_obstack *stack)
  : sem_item (FUNC, stack), memory_access_types (), m_alias_sets_hash (0),
    m_checker (NULL), m_compared_func (NULL)
{
  bb_sizes.create (0);
  bb_sorted.create (0);
}

sem_function::sem_function (cgraph_node *node, bitmap_obstack *stack)
  : sem_item (FUNC, node, stack), memory_access_types (),
    m_alias_sets_hash (0), m_checker (NULL), m_compared_func (NULL)
{
  bb_sizes.create (0);
  bb_sorted.create (0);
}

sem_function::~sem_function ()
{
  for (unsigned i = 0; i < bb_sorted.length (); i++)
    delete (bb_sorted[i]);

  bb_sizes.release ();
  bb_sorted.release ();
}

/* Calculates hash value based on a BASIC_BLOCK.  */

hashval_t
sem_function::get_bb_hash (const sem_bb *basic_block)
{
  inchash::hash hstate;

  hstate.add_int (basic_block->nondbg_stmt_count);
  hstate.add_int (basic_block->edge_count);

  return hstate.end ();
}

/* References independent hash function.  */

hashval_t
sem_function::get_hash (void)
{
  if (!m_hash_set)
    {
      inchash::hash hstate;
      hstate.add_int (177454); /* Random number for function type.  */

      hstate.add_int (arg_count);
      hstate.add_int (cfg_checksum);
      hstate.add_int (gcode_hash);

      for (unsigned i = 0; i < bb_sorted.length (); i++)
	hstate.merge_hash (get_bb_hash (bb_sorted[i]));

      for (unsigned i = 0; i < bb_sizes.length (); i++)
	hstate.add_int (bb_sizes[i]);

      /* Add common features of declaration itself.  */
      if (DECL_FUNCTION_SPECIFIC_TARGET (decl))
        hstate.add_hwi
	 (cl_target_option_hash
	   (TREE_TARGET_OPTION (DECL_FUNCTION_SPECIFIC_TARGET (decl))));
      if (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl))
	hstate.add_hwi
	 (cl_optimization_hash
	   (TREE_OPTIMIZATION (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl))));
      hstate.add_flag (DECL_CXX_CONSTRUCTOR_P (decl));
      hstate.add_flag (DECL_CXX_DESTRUCTOR_P (decl));

      set_hash (hstate.end ());
    }

  return m_hash;
}

/* Compare properties of symbols N1 and N2 that does not affect semantics of
   symbol itself but affects semantics of its references from USED_BY (which
   may be NULL if it is unknown).  If comparison is false, symbols
   can still be merged but any symbols referring them can't.

   If ADDRESS is true, do extra checking needed for IPA_REF_ADDR.

   TODO: We can also split attributes to those that determine codegen of
   a function body/variable constructor itself and those that are used when
   referring to it.  */

bool
sem_item::compare_referenced_symbol_properties (symtab_node *used_by,
						symtab_node *n1,
						symtab_node *n2,
						bool address)
{
  if (is_a <cgraph_node *> (n1))
    {
      /* Inline properties matters: we do now want to merge uses of inline
	 function to uses of normal function because inline hint would be lost.
	 We however can merge inline function to noinline because the alias
	 will keep its DECL_DECLARED_INLINE flag.

	 Also ignore inline flag when optimizing for size or when function
	 is known to not be inlinable.

	 TODO: the optimize_size checks can also be assumed to be true if
	 unit has no !optimize_size functions. */

      if ((!used_by || address || !is_a <cgraph_node *> (used_by)
	   || !opt_for_fn (used_by->decl, optimize_size))
	  && !opt_for_fn (n1->decl, optimize_size)
	  && n1->get_availability () > AVAIL_INTERPOSABLE
	  && (!DECL_UNINLINABLE (n1->decl) || !DECL_UNINLINABLE (n2->decl)))
	{
	  if (DECL_DISREGARD_INLINE_LIMITS (n1->decl)
	      != DECL_DISREGARD_INLINE_LIMITS (n2->decl))
	    return return_false_with_msg
		     ("DECL_DISREGARD_INLINE_LIMITS are different");

	  if (DECL_DECLARED_INLINE_P (n1->decl)
	      != DECL_DECLARED_INLINE_P (n2->decl))
	    return return_false_with_msg ("inline attributes are different");
	}

      if (DECL_IS_OPERATOR_NEW_P (n1->decl)
	  != DECL_IS_OPERATOR_NEW_P (n2->decl))
	return return_false_with_msg ("operator new flags are different");

      if (DECL_IS_REPLACEABLE_OPERATOR (n1->decl)
	  != DECL_IS_REPLACEABLE_OPERATOR (n2->decl))
	return return_false_with_msg ("replaceable operator flags are different");
    }

  /* Merging two definitions with a reference to equivalent vtables, but
     belonging to a different type may result in ipa-polymorphic-call analysis
     giving a wrong answer about the dynamic type of instance.  */
  if (is_a <varpool_node *> (n1))
    {
      if ((DECL_VIRTUAL_P (n1->decl) || DECL_VIRTUAL_P (n2->decl))
	  && (DECL_VIRTUAL_P (n1->decl) != DECL_VIRTUAL_P (n2->decl)
	      || !types_must_be_same_for_odr (DECL_CONTEXT (n1->decl),
					      DECL_CONTEXT (n2->decl)))
	  && (!used_by || !is_a <cgraph_node *> (used_by) || address
	      || opt_for_fn (used_by->decl, flag_devirtualize)))
	return return_false_with_msg
		 ("references to virtual tables cannot be merged");

      if (address && DECL_ALIGN (n1->decl) != DECL_ALIGN (n2->decl))
	return return_false_with_msg ("alignment mismatch");

      /* For functions we compare attributes in equals_wpa, because we do
	 not know what attributes may cause codegen differences, but for
	 variables just compare attributes for references - the codegen
	 for constructors is affected only by those attributes that we lower
	 to explicit representation (such as DECL_ALIGN or DECL_SECTION).  */
      if (!attribute_list_equal (DECL_ATTRIBUTES (n1->decl),
				 DECL_ATTRIBUTES (n2->decl)))
	return return_false_with_msg ("different var decl attributes");
      if (comp_type_attributes (TREE_TYPE (n1->decl),
				TREE_TYPE (n2->decl)) != 1)
	return return_false_with_msg ("different var type attributes");
    }

  /* When matching virtual tables, be sure to also match information
     relevant for polymorphic call analysis.  */
  if (used_by && is_a <varpool_node *> (used_by)
      && DECL_VIRTUAL_P (used_by->decl))
    {
      if (DECL_VIRTUAL_P (n1->decl) != DECL_VIRTUAL_P (n2->decl))
	return return_false_with_msg ("virtual flag mismatch");
      if (DECL_VIRTUAL_P (n1->decl) && is_a <cgraph_node *> (n1)
	  && (DECL_FINAL_P (n1->decl) != DECL_FINAL_P (n2->decl)))
	return return_false_with_msg ("final flag mismatch");
    }
  return true;
}

/* Hash properties that are compared by compare_referenced_symbol_properties. */

void
sem_item::hash_referenced_symbol_properties (symtab_node *ref,
					     inchash::hash &hstate,
					     bool address)
{
  if (is_a <cgraph_node *> (ref))
    {
      if ((type != FUNC || address || !opt_for_fn (decl, optimize_size))
	  && !opt_for_fn (ref->decl, optimize_size)
	  && !DECL_UNINLINABLE (ref->decl))
	{
	  hstate.add_flag (DECL_DISREGARD_INLINE_LIMITS (ref->decl));
	  hstate.add_flag (DECL_DECLARED_INLINE_P (ref->decl));
	}
      hstate.add_flag (DECL_IS_OPERATOR_NEW_P (ref->decl));
    }
  else if (is_a <varpool_node *> (ref))
    {
      hstate.add_flag (DECL_VIRTUAL_P (ref->decl));
      if (address)
	hstate.add_int (DECL_ALIGN (ref->decl));
    }
}


/* For a given symbol table nodes N1 and N2, we check that FUNCTION_DECLs
   point to a same function. Comparison can be skipped if IGNORED_NODES
   contains these nodes.  ADDRESS indicate if address is taken.  */

bool
sem_item::compare_symbol_references (
    hash_map <symtab_node *, sem_item *> &ignored_nodes,
    symtab_node *n1, symtab_node *n2, bool address)
{
  enum availability avail1, avail2;

  if (n1 == n2)
    return true;

  /* Never match variable and function.  */
  if (is_a <varpool_node *> (n1) != is_a <varpool_node *> (n2))
    return false;

  if (!compare_referenced_symbol_properties (node, n1, n2, address))
    return false;
  if (address && n1->equal_address_to (n2) == 1)
    return true;
  if (!address && n1->semantically_equivalent_p (n2))
    return true;

  n1 = n1->ultimate_alias_target (&avail1);
  n2 = n2->ultimate_alias_target (&avail2);

  if (avail1 > AVAIL_INTERPOSABLE && ignored_nodes.get (n1)
      && avail2 > AVAIL_INTERPOSABLE && ignored_nodes.get (n2))
    return true;

  return return_false_with_msg ("different references");
}

/* If cgraph edges E1 and E2 are indirect calls, verify that
   ECF flags are the same.  */

bool sem_function::compare_edge_flags (cgraph_edge *e1, cgraph_edge *e2)
{
  if (e1->indirect_info && e2->indirect_info)
    {
      int e1_flags = e1->indirect_info->ecf_flags;
      int e2_flags = e2->indirect_info->ecf_flags;

      if (e1_flags != e2_flags)
	return return_false_with_msg ("ICF flags are different");
    }
  else if (e1->indirect_info || e2->indirect_info)
    return false;

  return true;
}

/* Return true if parameter I may be used.  */

bool
sem_function::param_used_p (unsigned int i)
{
  if (ipa_node_params_sum == NULL)
    return true;

  ipa_node_params *parms_info = ipa_node_params_sum->get (get_node ());

  if (!parms_info || vec_safe_length (parms_info->descriptors) <= i)
    return true;

  return ipa_is_param_used (parms_info, i);
}

/* Perform additional check needed to match types function parameters that are
   used.  Unlike for normal decls it matters if type is TYPE_RESTRICT and we
   make an assumption that REFERENCE_TYPE parameters are always non-NULL.  */

bool
sem_function::compatible_parm_types_p (tree parm1, tree parm2)
{
  /* Be sure that parameters are TBAA compatible.  */
  if (!func_checker::compatible_types_p (parm1, parm2))
    return return_false_with_msg ("parameter type is not compatible");

  if (POINTER_TYPE_P (parm1)
      && (TYPE_RESTRICT (parm1) != TYPE_RESTRICT (parm2)))
    return return_false_with_msg ("argument restrict flag mismatch");

  /* nonnull_arg_p implies non-zero range to REFERENCE types.  */
  if (POINTER_TYPE_P (parm1)
      && TREE_CODE (parm1) != TREE_CODE (parm2)
      && opt_for_fn (decl, flag_delete_null_pointer_checks))
    return return_false_with_msg ("pointer wrt reference mismatch");

  return true;
}

/* Fast equality function based on knowledge known in WPA.  */

bool
sem_function::equals_wpa (sem_item *item,
			  hash_map <symtab_node *, sem_item *> &ignored_nodes)
{
  gcc_assert (item->type == FUNC);
  cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
  cgraph_node *cnode2 = dyn_cast <cgraph_node *> (item->node);

  m_compared_func = static_cast<sem_function *> (item);

  if (cnode->thunk != cnode2->thunk)
    return return_false_with_msg ("thunk mismatch");
  if (cnode->former_thunk_p () != cnode2->former_thunk_p ())
    return return_false_with_msg ("former_thunk_p mismatch");

  if ((cnode->thunk || cnode->former_thunk_p ())
      && thunk_info::get (cnode) != thunk_info::get (cnode2))
    return return_false_with_msg ("thunk_info mismatch");

  /* Compare special function DECL attributes.  */
  if (DECL_FUNCTION_PERSONALITY (decl)
      != DECL_FUNCTION_PERSONALITY (item->decl))
    return return_false_with_msg ("function personalities are different");

  if (DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl)
       != DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (item->decl))
    return return_false_with_msg ("instrument function entry exit "
				  "attributes are different");

  if (DECL_NO_LIMIT_STACK (decl) != DECL_NO_LIMIT_STACK (item->decl))
    return return_false_with_msg ("no stack limit attributes are different");

  if (DECL_CXX_CONSTRUCTOR_P (decl) != DECL_CXX_CONSTRUCTOR_P (item->decl))
    return return_false_with_msg ("DECL_CXX_CONSTRUCTOR mismatch");

  if (DECL_CXX_DESTRUCTOR_P (decl) != DECL_CXX_DESTRUCTOR_P (item->decl))
    return return_false_with_msg ("DECL_CXX_DESTRUCTOR mismatch");

  /* TODO: pure/const flags mostly matters only for references, except for
     the fact that codegen takes LOOPING flag as a hint that loops are
     finite.  We may arrange the code to always pick leader that has least
     specified flags and then this can go into comparing symbol properties.  */
  if (flags_from_decl_or_type (decl) != flags_from_decl_or_type (item->decl))
    return return_false_with_msg ("decl_or_type flags are different");

  /* Do not match polymorphic constructors of different types.  They calls
     type memory location for ipa-polymorphic-call and we do not want
     it to get confused by wrong type.  */
  if (DECL_CXX_CONSTRUCTOR_P (decl)
      && opt_for_fn (decl, flag_devirtualize)
      && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (item->decl)) != METHOD_TYPE)
        return return_false_with_msg ("DECL_CXX_CONSTRUCTOR type mismatch");
      else if (!func_checker::compatible_polymorphic_types_p
		 (TYPE_METHOD_BASETYPE (TREE_TYPE (decl)),
		  TYPE_METHOD_BASETYPE (TREE_TYPE (item->decl)), false))
        return return_false_with_msg ("ctor polymorphic type mismatch");
    }

  /* Checking function TARGET and OPTIMIZATION flags.  */
  cl_target_option *tar1 = target_opts_for_fn (decl);
  cl_target_option *tar2 = target_opts_for_fn (item->decl);

  if (tar1 != tar2 && !cl_target_option_eq (tar1, tar2))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "target flags difference");
	  cl_target_option_print_diff (dump_file, 2, tar1, tar2);
	}

      return return_false_with_msg ("Target flags are different");
    }

  cl_optimization *opt1 = opts_for_fn (decl);
  cl_optimization *opt2 = opts_for_fn (item->decl);

  if (opt1 != opt2 && !cl_optimization_option_eq (opt1, opt2))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "optimization flags difference");
	  cl_optimization_print_diff (dump_file, 2, opt1, opt2);
	}

      return return_false_with_msg ("optimization flags are different");
    }

  /* Result type checking.  */
  if (!func_checker::compatible_types_p
	 (TREE_TYPE (TREE_TYPE (decl)),
	  TREE_TYPE (TREE_TYPE (m_compared_func->decl))))
    return return_false_with_msg ("result types are different");

  /* Checking types of arguments.  */
  tree list1 = TYPE_ARG_TYPES (TREE_TYPE (decl)),
       list2 = TYPE_ARG_TYPES (TREE_TYPE (m_compared_func->decl));
  for (unsigned i = 0; list1 && list2;
       list1 = TREE_CHAIN (list1), list2 = TREE_CHAIN (list2), i++)
    {
      tree parm1 = TREE_VALUE (list1);
      tree parm2 = TREE_VALUE (list2);

      /* This guard is here for function pointer with attributes (pr59927.c).  */
      if (!parm1 || !parm2)
	return return_false_with_msg ("NULL argument type");

      /* Verify that types are compatible to ensure that both functions
	 have same calling conventions.  */
      if (!types_compatible_p (parm1, parm2))
	return return_false_with_msg ("parameter types are not compatible");

      if (!param_used_p (i))
	continue;

      /* Perform additional checks for used parameters.  */
      if (!compatible_parm_types_p (parm1, parm2))
	return false;
    }

  if (list1 || list2)
    return return_false_with_msg ("Mismatched number of parameters");

  if (node->num_references () != item->node->num_references ())
    return return_false_with_msg ("different number of references");

  /* Checking function attributes.
     This is quadratic in number of attributes  */
  if (comp_type_attributes (TREE_TYPE (decl),
			    TREE_TYPE (item->decl)) != 1)
    return return_false_with_msg ("different type attributes");
  if (!attribute_list_equal (DECL_ATTRIBUTES (decl),
			     DECL_ATTRIBUTES (item->decl)))
    return return_false_with_msg ("different decl attributes");

  /* The type of THIS pointer type memory location for
     ipa-polymorphic-call-analysis.  */
  if (opt_for_fn (decl, flag_devirtualize)
      && (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE
          || TREE_CODE (TREE_TYPE (item->decl)) == METHOD_TYPE)
      && param_used_p (0)
      && compare_polymorphic_p ())
    {
      if (TREE_CODE (TREE_TYPE (decl)) != TREE_CODE (TREE_TYPE (item->decl)))
	return return_false_with_msg ("METHOD_TYPE and FUNCTION_TYPE mismatch");
      if (!func_checker::compatible_polymorphic_types_p
	   (TYPE_METHOD_BASETYPE (TREE_TYPE (decl)),
	    TYPE_METHOD_BASETYPE (TREE_TYPE (item->decl)), false))
	return return_false_with_msg ("THIS pointer ODR type mismatch");
    }

  ipa_ref *ref = NULL, *ref2 = NULL;
  for (unsigned i = 0; node->iterate_reference (i, ref); i++)
    {
      item->node->iterate_reference (i, ref2);

      if (ref->use != ref2->use)
	return return_false_with_msg ("reference use mismatch");

      if (!compare_symbol_references (ignored_nodes, ref->referred,
				      ref2->referred,
				      ref->address_matters_p ()))
	return false;
    }

  cgraph_edge *e1 = dyn_cast <cgraph_node *> (node)->callees;
  cgraph_edge *e2 = dyn_cast <cgraph_node *> (item->node)->callees;

  while (e1 && e2)
    {
      if (!compare_symbol_references (ignored_nodes, e1->callee,
				      e2->callee, false))
	return false;
      if (!compare_edge_flags (e1, e2))
	return false;

      e1 = e1->next_callee;
      e2 = e2->next_callee;
    }

  if (e1 || e2)
    return return_false_with_msg ("different number of calls");

  e1 = dyn_cast <cgraph_node *> (node)->indirect_calls;
  e2 = dyn_cast <cgraph_node *> (item->node)->indirect_calls;

  while (e1 && e2)
    {
      if (!compare_edge_flags (e1, e2))
	return false;

      e1 = e1->next_callee;
      e2 = e2->next_callee;
    }

  if (e1 || e2)
    return return_false_with_msg ("different number of indirect calls");

  return true;
}

/* Update hash by address sensitive references. We iterate over all
   sensitive references (address_matters_p) and we hash ultimate alias
   target of these nodes, which can improve a semantic item hash.

   Also hash in referenced symbols properties.  This can be done at any time
   (as the properties should not change), but it is convenient to do it here
   while we walk the references anyway.  */

void
sem_item::update_hash_by_addr_refs (hash_map <symtab_node *,
				    sem_item *> &m_symtab_node_map)
{
  ipa_ref* ref;
  inchash::hash hstate (get_hash ());

  for (unsigned i = 0; node->iterate_reference (i, ref); i++)
    {
      hstate.add_int (ref->use);
      hash_referenced_symbol_properties (ref->referred, hstate,
					 ref->use == IPA_REF_ADDR);
      if (ref->address_matters_p () || !m_symtab_node_map.get (ref->referred))
	hstate.add_int (ref->referred->ultimate_alias_target ()->order);
    }

  if (is_a <cgraph_node *> (node))
    {
      for (cgraph_edge *e = dyn_cast <cgraph_node *> (node)->callers; e;
	   e = e->next_caller)
	{
	  sem_item **result = m_symtab_node_map.get (e->callee);
	  hash_referenced_symbol_properties (e->callee, hstate, false);
	  if (!result)
	    hstate.add_int (e->callee->ultimate_alias_target ()->order);
	}
    }

  set_hash (hstate.end ());
}

/* Update hash by computed local hash values taken from different
   semantic items.
   TODO: stronger SCC based hashing would be desirable here.  */

void
sem_item::update_hash_by_local_refs (hash_map <symtab_node *,
				     sem_item *> &m_symtab_node_map)
{
  ipa_ref* ref;
  inchash::hash state (get_hash ());

  for (unsigned j = 0; node->iterate_reference (j, ref); j++)
    {
      sem_item **result = m_symtab_node_map.get (ref->referring);
      if (result)
	state.merge_hash ((*result)->get_hash ());
    }

  if (type == FUNC)
    {
      for (cgraph_edge *e = dyn_cast <cgraph_node *> (node)->callees; e;
	   e = e->next_callee)
	{
	  sem_item **result = m_symtab_node_map.get (e->caller);
	  if (result)
	    state.merge_hash ((*result)->get_hash ());
	}
    }

  global_hash = state.end ();
}

/* Returns true if the item equals to ITEM given as argument.  */

bool
sem_function::equals (sem_item *item,
		      hash_map <symtab_node *, sem_item *> &)
{
  gcc_assert (item->type == FUNC);
  bool eq = equals_private (item);

  if (m_checker != NULL)
    {
      delete m_checker;
      m_checker = NULL;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "Equals called for: %s:%s with result: %s\n\n",
	     node->dump_name (),
	     item->node->dump_name (),
	     eq ? "true" : "false");

  return eq;
}

/* Processes function equality comparison.  */

bool
sem_function::equals_private (sem_item *item)
{
  if (item->type != FUNC)
    return false;

  basic_block bb1, bb2;
  edge e1, e2;
  edge_iterator ei1, ei2;
  bool result = true;
  tree arg1, arg2;

  m_compared_func = static_cast<sem_function *> (item);

  gcc_assert (decl != item->decl);

  if (bb_sorted.length () != m_compared_func->bb_sorted.length ()
      || edge_count != m_compared_func->edge_count
      || cfg_checksum != m_compared_func->cfg_checksum)
    return return_false ();

  m_checker = new func_checker (decl, m_compared_func->decl,
				false,
				opt_for_fn (m_compared_func->decl,
					    flag_strict_aliasing),
				&refs_set,
				&m_compared_func->refs_set);
  arg1 = DECL_ARGUMENTS (decl);
  arg2 = DECL_ARGUMENTS (m_compared_func->decl);
  for (unsigned i = 0;
       arg1 && arg2; arg1 = DECL_CHAIN (arg1), arg2 = DECL_CHAIN (arg2), i++)
    {
      if (!types_compatible_p (TREE_TYPE (arg1), TREE_TYPE (arg2)))
	return return_false_with_msg ("argument types are not compatible");
      if (!param_used_p (i))
	continue;
      /* Perform additional checks for used parameters.  */
      if (!compatible_parm_types_p (TREE_TYPE (arg1), TREE_TYPE (arg2)))
	return false;
      if (!m_checker->compare_decl (arg1, arg2))
        return return_false ();
    }
  if (arg1 || arg2)
    return return_false_with_msg ("Mismatched number of arguments");

  if (!dyn_cast <cgraph_node *> (node)->has_gimple_body_p ())
    return true;

  /* Fill-up label dictionary.  */
  for (unsigned i = 0; i < bb_sorted.length (); ++i)
    {
      m_checker->parse_labels (bb_sorted[i]);
      m_checker->parse_labels (m_compared_func->bb_sorted[i]);
    }

  /* Checking all basic blocks.  */
  for (unsigned i = 0; i < bb_sorted.length (); ++i)
    if(!m_checker->compare_bb (bb_sorted[i], m_compared_func->bb_sorted[i]))
      return return_false ();

  auto_vec <int> bb_dict;

  /* Basic block edges check.  */
  for (unsigned i = 0; i < bb_sorted.length (); ++i)
    {
      bb1 = bb_sorted[i]->bb;
      bb2 = m_compared_func->bb_sorted[i]->bb;

      ei2 = ei_start (bb2->preds);

      for (ei1 = ei_start (bb1->preds); ei_cond (ei1, &e1); ei_next (&ei1))
	{
	  ei_cond (ei2, &e2);

	  if (e1->flags != e2->flags)
	    return return_false_with_msg ("flags comparison returns false");

	  if (!bb_dict_test (&bb_dict, e1->src->index, e2->src->index))
	    return return_false_with_msg ("edge comparison returns false");

	  if (!bb_dict_test (&bb_dict, e1->dest->index, e2->dest->index))
	    return return_false_with_msg ("BB comparison returns false");

	  if (!m_checker->compare_edge (e1, e2))
	    return return_false_with_msg ("edge comparison returns false");

	  ei_next (&ei2);
	}
    }

  /* Basic block PHI nodes comparison.  */
  for (unsigned i = 0; i < bb_sorted.length (); i++)
    if (!compare_phi_node (bb_sorted[i]->bb, m_compared_func->bb_sorted[i]->bb))
      return return_false_with_msg ("PHI node comparison returns false");

  return result;
}

/* Set LOCAL_P of NODE to true if DATA is non-NULL.
   Helper for call_for_symbol_thunks_and_aliases.  */

static bool
set_local (cgraph_node *node, void *data)
{
  node->local = data != NULL;
  return false;
}

/* TREE_ADDRESSABLE of NODE to true.
   Helper for call_for_symbol_thunks_and_aliases.  */

static bool
set_addressable (varpool_node *node, void *)
{
  TREE_ADDRESSABLE (node->decl) = 1;
  return false;
}

/* Clear DECL_RTL of NODE. 
   Helper for call_for_symbol_thunks_and_aliases.  */

static bool
clear_decl_rtl (symtab_node *node, void *)
{
  SET_DECL_RTL (node->decl, NULL);
  return false;
}

/* Redirect all callers of N and its aliases to TO.  Remove aliases if
   possible.  Return number of redirections made.  */

static int
redirect_all_callers (cgraph_node *n, cgraph_node *to)
{
  int nredirected = 0;
  ipa_ref *ref;
  cgraph_edge *e = n->callers;

  while (e)
    {
      /* Redirecting thunks to interposable symbols or symbols in other sections
	 may not be supported by target output code.  Play safe for now and
	 punt on redirection.  */
      if (!e->caller->thunk)
	{
	  struct cgraph_edge *nexte = e->next_caller;
          e->redirect_callee (to);
	  e = nexte;
          nredirected++;
	}
      else
	e = e->next_callee;
    }
  for (unsigned i = 0; n->iterate_direct_aliases (i, ref);)
    {
      bool removed = false;
      cgraph_node *n_alias = dyn_cast <cgraph_node *> (ref->referring);

      if ((DECL_COMDAT_GROUP (n->decl)
	   && (DECL_COMDAT_GROUP (n->decl)
	       == DECL_COMDAT_GROUP (n_alias->decl)))
	  || (n_alias->get_availability () > AVAIL_INTERPOSABLE
	      && n->get_availability () > AVAIL_INTERPOSABLE))
	{
	  nredirected += redirect_all_callers (n_alias, to);
	  if (n_alias->can_remove_if_no_direct_calls_p ()
	      && !n_alias->call_for_symbol_and_aliases (cgraph_node::has_thunk_p,
							NULL, true)
	      && !n_alias->has_aliases_p ())
	    n_alias->remove ();
	}
      if (!removed)
	i++;
    }
  return nredirected;
}

/* Merges instance with an ALIAS_ITEM, where alias, thunk or redirection can
   be applied.  */

bool
sem_function::merge (sem_item *alias_item)
{
  gcc_assert (alias_item->type == FUNC);

  sem_function *alias_func = static_cast<sem_function *> (alias_item);

  cgraph_node *original = get_node ();
  cgraph_node *local_original = NULL;
  cgraph_node *alias = alias_func->get_node ();

  bool create_wrapper = false;
  bool create_alias = false;
  bool redirect_callers = false;
  bool remove = false;

  bool original_discardable = false;
  bool original_discarded = false;

  bool original_address_matters = original->address_matters_p ();
  bool alias_address_matters = alias->address_matters_p ();

  AUTO_DUMP_SCOPE ("merge",
		   dump_user_location_t::from_function_decl (decl));

  if (DECL_EXTERNAL (alias->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; alias is external.\n");
      return false;
    }

  if (DECL_NO_INLINE_WARNING_P (original->decl)
      != DECL_NO_INLINE_WARNING_P (alias->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; DECL_NO_INLINE_WARNING mismatch.\n");
      return false;
    }

  /* Do not attempt to mix functions from different user sections;
     we do not know what user intends with those.  */
  if (((DECL_SECTION_NAME (original->decl) && !original->implicit_section)
       || (DECL_SECTION_NAME (alias->decl) && !alias->implicit_section))
      && DECL_SECTION_NAME (original->decl) != DECL_SECTION_NAME (alias->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; "
		     "original and alias are in different sections.\n");
      return false;
    }

  if (!original->in_same_comdat_group_p (alias)
      || original->comdat_local_p ())
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; alias nor wrapper cannot be created; "
		     "across comdat group boundary\n");
      return false;
    }

  /* See if original is in a section that can be discarded if the main
     symbol is not used.  */

  if (original->can_be_discarded_p ())
    original_discardable = true;
  /* Also consider case where we have resolution info and we know that
     original's definition is not going to be used.  In this case we cannot
     create alias to original.  */
  if (node->resolution != LDPR_UNKNOWN
      && !decl_binds_to_current_def_p (node->decl))
    original_discardable = original_discarded = true;

  /* Creating a symtab alias is the optimal way to merge.
     It however cannot be used in the following cases:

     1) if ORIGINAL and ALIAS may be possibly compared for address equality.
     2) if ORIGINAL is in a section that may be discarded by linker or if
	it is an external functions where we cannot create an alias
	(ORIGINAL_DISCARDABLE)
     3) if target do not support symbol aliases.
     4) original and alias lie in different comdat groups.

     If we cannot produce alias, we will turn ALIAS into WRAPPER of ORIGINAL
     and/or redirect all callers from ALIAS to ORIGINAL.  */
  if ((original_address_matters && alias_address_matters)
      || (original_discardable
	  && (!DECL_COMDAT_GROUP (alias->decl)
	      || (DECL_COMDAT_GROUP (alias->decl)
		  != DECL_COMDAT_GROUP (original->decl))))
      || original_discarded
      || !sem_item::target_supports_symbol_aliases_p ()
      || DECL_COMDAT_GROUP (alias->decl) != DECL_COMDAT_GROUP (original->decl))
    {
      /* First see if we can produce wrapper.  */

      /* Symbol properties that matter for references must be preserved.
	 TODO: We can produce wrapper, but we need to produce alias of ORIGINAL
	 with proper properties.  */
      if (!sem_item::compare_referenced_symbol_properties (NULL, original, alias,
							   alias->address_taken))
        {
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Wrapper cannot be created because referenced symbol "
			 "properties mismatch\n");
        }
      /* Do not turn function in one comdat group into wrapper to another
	 comdat group. Other compiler producing the body of the
	 another comdat group may make opposite decision and with unfortunate
	 linker choices this may close a loop.  */
      else if (DECL_COMDAT_GROUP (original->decl)
	       && DECL_COMDAT_GROUP (alias->decl)
	       && (DECL_COMDAT_GROUP (alias->decl)
		   != DECL_COMDAT_GROUP (original->decl)))
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Wrapper cannot be created because of COMDAT\n");
	}
      else if (DECL_STATIC_CHAIN (alias->decl)
	       || DECL_STATIC_CHAIN (original->decl))
        {
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Cannot create wrapper of nested function.\n");
        }
      /* TODO: We can also deal with variadic functions never calling
	 VA_START.  */
      else if (stdarg_p (TREE_TYPE (alias->decl)))
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "cannot create wrapper of stdarg function.\n");
	}
      else if (ipa_fn_summaries
	       && ipa_size_summaries->get (alias) != NULL
	       && ipa_size_summaries->get (alias)->self_size <= 2)
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION, "Wrapper creation is not "
			 "profitable (function is too small).\n");
	}
      /* If user paid attention to mark function noinline, assume it is
	 somewhat special and do not try to turn it into a wrapper that
	 cannot be undone by inliner.  */
      else if (lookup_attribute ("noinline", DECL_ATTRIBUTES (alias->decl)))
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Wrappers are not created for noinline.\n");
	}
      else
        create_wrapper = true;

      /* We can redirect local calls in the case both alias and original
	 are not interposable.  */
      redirect_callers
	= alias->get_availability () > AVAIL_INTERPOSABLE
	  && original->get_availability () > AVAIL_INTERPOSABLE;
      /* TODO: We can redirect, but we need to produce alias of ORIGINAL
	 with proper properties.  */
      if (!sem_item::compare_referenced_symbol_properties (NULL, original, alias,
							   alias->address_taken))
	redirect_callers = false;

      if (!redirect_callers && !create_wrapper)
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Not unifying; cannot redirect callers nor "
			 "produce wrapper\n");
	  return false;
	}

      /* Work out the symbol the wrapper should call.
	 If ORIGINAL is interposable, we need to call a local alias.
	 Also produce local alias (if possible) as an optimization.

	 Local aliases cannot be created inside comdat groups because that
	 prevents inlining.  */
      if (!original_discardable && !original->get_comdat_group ())
	{
	  local_original
	    = dyn_cast <cgraph_node *> (original->noninterposable_alias ());
	  if (!local_original
	      && original->get_availability () > AVAIL_INTERPOSABLE)
	    local_original = original;
	}
      /* If we cannot use local alias, fallback to the original
	 when possible.  */
      else if (original->get_availability () > AVAIL_INTERPOSABLE)
	local_original = original;

      /* If original is COMDAT local, we cannot really redirect calls outside
	 of its comdat group to it.  */
      if (original->comdat_local_p ())
        redirect_callers = false;
      if (!local_original)
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Not unifying; cannot produce local alias.\n");
	  return false;
	}

      if (!redirect_callers && !create_wrapper)
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Not unifying; "
			 "cannot redirect callers nor produce a wrapper\n");
	  return false;
	}
      if (!create_wrapper
	  && !alias->call_for_symbol_and_aliases (cgraph_node::has_thunk_p,
						  NULL, true)
	  && !alias->can_remove_if_no_direct_calls_p ())
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "Not unifying; cannot make wrapper and "
			 "function has other uses than direct calls\n");
	  return false;
	}
    }
  else
    create_alias = true;

  if (redirect_callers)
    {
      int nredirected = redirect_all_callers (alias, local_original);

      if (nredirected)
	{
	  alias->icf_merged = true;
	  local_original->icf_merged = true;

	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE,
			 "%i local calls have been "
			 "redirected.\n", nredirected);
	}

      /* If all callers was redirected, do not produce wrapper.  */
      if (alias->can_remove_if_no_direct_calls_p ()
	  && !DECL_VIRTUAL_P (alias->decl)
	  && !alias->has_aliases_p ())
	{
	  create_wrapper = false;
	  remove = true;
	}
      gcc_assert (!create_alias);
    }
  else if (create_alias)
    {
      alias->icf_merged = true;

      /* Remove the function's body.  */
      ipa_merge_profiles (original, alias);
      symtab->call_cgraph_removal_hooks (alias);
      alias->release_body (true);
      alias->reset ();
      /* Notice global symbol possibly produced RTL.  */
      ((symtab_node *)alias)->call_for_symbol_and_aliases (clear_decl_rtl,
							   NULL, true);

      /* Create the alias.  */
      cgraph_node::create_alias (alias_func->decl, decl);
      alias->resolve_alias (original);

      original->call_for_symbol_thunks_and_aliases
	 (set_local, (void *)(size_t) original->local_p (), true);

      if (dump_enabled_p ())
	dump_printf (MSG_OPTIMIZED_LOCATIONS,
		     "Unified; Function alias has been created.\n");
    }
  if (create_wrapper)
    {
      gcc_assert (!create_alias);
      alias->icf_merged = true;
      symtab->call_cgraph_removal_hooks (alias);
      local_original->icf_merged = true;

      /* FIXME update local_original counts.  */
      ipa_merge_profiles (original, alias, true);
      alias->create_wrapper (local_original);
      symtab->call_cgraph_insertion_hooks (alias);

      if (dump_enabled_p ())
	dump_printf (MSG_OPTIMIZED_LOCATIONS,
		     "Unified; Wrapper has been created.\n");
    }

  /* It's possible that redirection can hit thunks that block
     redirection opportunities.  */
  gcc_assert (alias->icf_merged || remove || redirect_callers);
  original->icf_merged = true;

  /* We use merged flag to track cases where COMDAT function is known to be
     compatible its callers.  If we merged in non-COMDAT, we need to give up
     on this optimization.  */
  if (original->merged_comdat && !alias->merged_comdat)
    {
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "Dropping merged_comdat flag.\n");
      if (local_original)
        local_original->merged_comdat = false;
      original->merged_comdat = false;
    }

  if (remove)
    {
      ipa_merge_profiles (original, alias);
      alias->release_body ();
      alias->reset ();
      alias->body_removed = true;
      alias->icf_merged = true;
      if (dump_enabled_p ())
	dump_printf (MSG_OPTIMIZED_LOCATIONS,
		     "Unified; Function body was removed.\n");
    }

  return true;
}

/* Semantic item initialization function.  */

void
sem_function::init (ipa_icf_gimple::func_checker *checker)
{
  m_checker = checker;
  if (in_lto_p)
    get_node ()->get_untransformed_body ();

  tree fndecl = node->decl;
  function *func = DECL_STRUCT_FUNCTION (fndecl);

  gcc_assert (func);
  gcc_assert (SSANAMES (func));

  ssa_names_size = SSANAMES (func)->length ();
  node = node;

  decl = fndecl;
  region_tree = func->eh->region_tree;

  /* iterating all function arguments.  */
  arg_count = count_formal_params (fndecl);

  edge_count = n_edges_for_fn (func);
  cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
  if (!cnode->thunk)
    {
      cfg_checksum = coverage_compute_cfg_checksum (func);

      inchash::hash hstate;

      basic_block bb;
      FOR_EACH_BB_FN (bb, func)
      {
	unsigned nondbg_stmt_count = 0;

	edge e;
	for (edge_iterator ei = ei_start (bb->preds); ei_cond (ei, &e);
	     ei_next (&ei))
	  cfg_checksum = iterative_hash_host_wide_int (e->flags,
			 cfg_checksum);

	for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  {
	    gimple *stmt = gsi_stmt (gsi);

	    if (gimple_code (stmt) != GIMPLE_DEBUG
		&& gimple_code (stmt) != GIMPLE_PREDICT)
	      {
		hash_stmt (stmt, hstate);
		nondbg_stmt_count++;
	      }
	  }

	hstate.commit_flag ();
	gcode_hash = hstate.end ();
	bb_sizes.safe_push (nondbg_stmt_count);

	/* Inserting basic block to hash table.  */
	sem_bb *semantic_bb = new sem_bb (bb, nondbg_stmt_count,
					  EDGE_COUNT (bb->preds)
					  + EDGE_COUNT (bb->succs));

	bb_sorted.safe_push (semantic_bb);
      }
    }
  else
    {
      cfg_checksum = 0;
      gcode_hash = thunk_info::get (cnode)->hash ();
    }

  m_checker = NULL;
}

/* Improve accumulated hash for HSTATE based on a gimple statement STMT.  */

void
sem_function::hash_stmt (gimple *stmt, inchash::hash &hstate)
{
  enum gimple_code code = gimple_code (stmt);

  hstate.add_int (code);

  switch (code)
    {
    case GIMPLE_SWITCH:
      m_checker->hash_operand (gimple_switch_index (as_a <gswitch *> (stmt)),
			       hstate, 0, func_checker::OP_NORMAL);
      break;
    case GIMPLE_ASSIGN:
      hstate.add_int (gimple_assign_rhs_code (stmt));
      /* fall through */
    case GIMPLE_CALL:
    case GIMPLE_ASM:
    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_RETURN:
      {
	func_checker::operand_access_type_map map (5);
	func_checker::classify_operands (stmt, &map);

	/* All these statements are equivalent if their operands are.  */
	for (unsigned i = 0; i < gimple_num_ops (stmt); ++i)
	  {
	    func_checker::operand_access_type
		access_type = func_checker::get_operand_access_type
					  (&map, gimple_op (stmt, i));
	    m_checker->hash_operand (gimple_op (stmt, i), hstate, 0,
				     access_type);
	    /* For memory accesses when hasing for LTO stremaing record
	       base and ref alias ptr types so we can compare them at WPA
	       time without having to read actual function body.  */
	    if (access_type == func_checker::OP_MEMORY
		&& lto_streaming_expected_p ()
		&& flag_strict_aliasing)
	      {
		ao_ref ref;

		ao_ref_init (&ref, gimple_op (stmt, i));
		tree t = ao_ref_alias_ptr_type (&ref);
		if (!variably_modified_type_p (t, NULL_TREE))
		  memory_access_types.safe_push (t);
		t = ao_ref_base_alias_ptr_type (&ref);
		if (!variably_modified_type_p (t, NULL_TREE))
		  memory_access_types.safe_push (t);
	      }
	  }
	/* Consider nocf_check attribute in hash as it affects code
	   generation.  */
	if (code == GIMPLE_CALL
	    && flag_cf_protection & CF_BRANCH)
	  hstate.add_flag (gimple_call_nocf_check_p (as_a <gcall *> (stmt)));
      }
      break;
    default:
      break;
    }
}


/* Return true if polymorphic comparison must be processed.  */

bool
sem_function::compare_polymorphic_p (void)
{
  struct cgraph_edge *e;

  if (!opt_for_fn (get_node ()->decl, flag_devirtualize))
    return false;
  if (get_node ()->indirect_calls != NULL)
    return true;
  /* TODO: We can do simple propagation determining what calls may lead to
     a polymorphic call.  */
  for (e = get_node ()->callees; e; e = e->next_callee)
    if (e->callee->definition
	&& opt_for_fn (e->callee->decl, flag_devirtualize))
      return true;
  return false;
}

/* For a given call graph NODE, the function constructs new
   semantic function item.  */

sem_function *
sem_function::parse (cgraph_node *node, bitmap_obstack *stack,
		     func_checker *checker)
{
  tree fndecl = node->decl;
  function *func = DECL_STRUCT_FUNCTION (fndecl);

  if (!func || (!node->has_gimple_body_p () && !node->thunk))
    return NULL;

  if (lookup_attribute_by_prefix ("omp ", DECL_ATTRIBUTES (node->decl)) != NULL)
    return NULL;

  if (lookup_attribute_by_prefix ("oacc ",
				  DECL_ATTRIBUTES (node->decl)) != NULL)
    return NULL;

  /* PR ipa/70306.  */
  if (DECL_STATIC_CONSTRUCTOR (node->decl)
      || DECL_STATIC_DESTRUCTOR (node->decl))
    return NULL;

  sem_function *f = new sem_function (node, stack);
  f->init (checker);

  return f;
}

/* For given basic blocks BB1 and BB2 (from functions FUNC1 and FUNC),
   return true if phi nodes are semantically equivalent in these blocks .  */

bool
sem_function::compare_phi_node (basic_block bb1, basic_block bb2)
{
  gphi_iterator si1, si2;
  gphi *phi1, *phi2;
  unsigned size1, size2, i;
  tree t1, t2;
  edge e1, e2;

  gcc_assert (bb1 != NULL);
  gcc_assert (bb2 != NULL);

  si2 = gsi_start_nonvirtual_phis (bb2);
  for (si1 = gsi_start_nonvirtual_phis (bb1); !gsi_end_p (si1);
       gsi_next_nonvirtual_phi (&si1))
    {
      if (gsi_end_p (si1) && gsi_end_p (si2))
	break;

      if (gsi_end_p (si1) || gsi_end_p (si2))
	return return_false();

      phi1 = si1.phi ();
      phi2 = si2.phi ();

      tree phi_result1 = gimple_phi_result (phi1);
      tree phi_result2 = gimple_phi_result (phi2);

      if (!m_checker->compare_operand (phi_result1, phi_result2,
				       func_checker::OP_NORMAL))
	return return_false_with_msg ("PHI results are different");

      size1 = gimple_phi_num_args (phi1);
      size2 = gimple_phi_num_args (phi2);

      if (size1 != size2)
	return return_false ();

      for (i = 0; i < size1; ++i)
	{
	  t1 = gimple_phi_arg (phi1, i)->def;
	  t2 = gimple_phi_arg (phi2, i)->def;

	  if (!m_checker->compare_operand (t1, t2, func_checker::OP_NORMAL))
	    return return_false ();

	  e1 = gimple_phi_arg_edge (phi1, i);
	  e2 = gimple_phi_arg_edge (phi2, i);

	  if (!m_checker->compare_edge (e1, e2))
	    return return_false ();
	}

      gsi_next_nonvirtual_phi (&si2);
    }

  return true;
}

/* Basic blocks dictionary BB_DICT returns true if SOURCE index BB
   corresponds to TARGET.  */

bool
sem_function::bb_dict_test (vec<int> *bb_dict, int source, int target)
{
  source++;
  target++;

  if (bb_dict->length () <= (unsigned)source)
    bb_dict->safe_grow_cleared (source + 1, true);

  if ((*bb_dict)[source] == 0)
    {
      (*bb_dict)[source] = target;
      return true;
    }
  else
    return (*bb_dict)[source] == target;
}

sem_variable::sem_variable (bitmap_obstack *stack): sem_item (VAR, stack)
{
}

sem_variable::sem_variable (varpool_node *node, bitmap_obstack *stack)
: sem_item (VAR, node, stack)
{
  gcc_checking_assert (node);
  gcc_checking_assert (get_node ());
}

/* Fast equality function based on knowledge known in WPA.  */

bool
sem_variable::equals_wpa (sem_item *item,
			  hash_map <symtab_node *, sem_item *> &ignored_nodes)
{
  gcc_assert (item->type == VAR);

  if (node->num_references () != item->node->num_references ())
    return return_false_with_msg ("different number of references");

  if (DECL_TLS_MODEL (decl) || DECL_TLS_MODEL (item->decl))
    return return_false_with_msg ("TLS model");

  /* DECL_ALIGN is safe to merge, because we will always chose the largest
     alignment out of all aliases.  */

  if (DECL_VIRTUAL_P (decl) != DECL_VIRTUAL_P (item->decl))
    return return_false_with_msg ("Virtual flag mismatch");

  if (DECL_SIZE (decl) != DECL_SIZE (item->decl)
      && ((!DECL_SIZE (decl) || !DECL_SIZE (item->decl))
	  || !operand_equal_p (DECL_SIZE (decl),
			       DECL_SIZE (item->decl), OEP_ONLY_CONST)))
    return return_false_with_msg ("size mismatch");

  /* Do not attempt to mix data from different user sections;
     we do not know what user intends with those.  */
  if (((DECL_SECTION_NAME (decl) && !node->implicit_section)
       || (DECL_SECTION_NAME (item->decl) && !item->node->implicit_section))
      && DECL_SECTION_NAME (decl) != DECL_SECTION_NAME (item->decl))
    return return_false_with_msg ("user section mismatch");

  if (DECL_IN_TEXT_SECTION (decl) != DECL_IN_TEXT_SECTION (item->decl))
    return return_false_with_msg ("text section");

  ipa_ref *ref = NULL, *ref2 = NULL;
  for (unsigned i = 0; node->iterate_reference (i, ref); i++)
    {
      item->node->iterate_reference (i, ref2);

      if (ref->use != ref2->use)
	return return_false_with_msg ("reference use mismatch");

      if (!compare_symbol_references (ignored_nodes,
				      ref->referred, ref2->referred,
				      ref->address_matters_p ()))
	return false;
    }

  return true;
}

/* Returns true if the item equals to ITEM given as argument.  */

bool
sem_variable::equals (sem_item *item,
		      hash_map <symtab_node *, sem_item *> &)
{
  gcc_assert (item->type == VAR);
  bool ret;

  if (DECL_INITIAL (decl) == error_mark_node && in_lto_p)
    dyn_cast <varpool_node *>(node)->get_constructor ();
  if (DECL_INITIAL (item->decl) == error_mark_node && in_lto_p)
    dyn_cast <varpool_node *>(item->node)->get_constructor ();

  /* As seen in PR ipa/65303 we have to compare variables types.  */
  if (!func_checker::compatible_types_p (TREE_TYPE (decl),
					 TREE_TYPE (item->decl)))
    return return_false_with_msg ("variables types are different");

  ret = sem_variable::equals (DECL_INITIAL (decl),
			      DECL_INITIAL (item->node->decl));
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "Equals called for vars: %s:%s with result: %s\n\n",
	     node->dump_name (), item->node->dump_name (),
	     ret ? "true" : "false");

  return ret;
}

/* Compares trees T1 and T2 for semantic equality.  */

bool
sem_variable::equals (tree t1, tree t2)
{
  if (!t1 || !t2)
    return return_with_debug (t1 == t2);
  if (t1 == t2)
    return true;
  tree_code tc1 = TREE_CODE (t1);
  tree_code tc2 = TREE_CODE (t2);

  if (tc1 != tc2)
    return return_false_with_msg ("TREE_CODE mismatch");

  switch (tc1)
    {
    case CONSTRUCTOR:
      {
	vec<constructor_elt, va_gc> *v1, *v2;
	unsigned HOST_WIDE_INT idx;

	enum tree_code typecode = TREE_CODE (TREE_TYPE (t1));
	if (typecode != TREE_CODE (TREE_TYPE (t2)))
	  return return_false_with_msg ("constructor type mismatch");

	if (typecode == ARRAY_TYPE)
	  {
	    HOST_WIDE_INT size_1 = int_size_in_bytes (TREE_TYPE (t1));
	    /* For arrays, check that the sizes all match.  */
	    if (TYPE_MODE (TREE_TYPE (t1)) != TYPE_MODE (TREE_TYPE (t2))
		|| size_1 == -1
		|| size_1 != int_size_in_bytes (TREE_TYPE (t2)))
	      return return_false_with_msg ("constructor array size mismatch");
	  }
	else if (!func_checker::compatible_types_p (TREE_TYPE (t1),
						    TREE_TYPE (t2)))
	  return return_false_with_msg ("constructor type incompatible");

	v1 = CONSTRUCTOR_ELTS (t1);
	v2 = CONSTRUCTOR_ELTS (t2);
	if (vec_safe_length (v1) != vec_safe_length (v2))
	  return return_false_with_msg ("constructor number of elts mismatch");

	for (idx = 0; idx < vec_safe_length (v1); ++idx)
	  {
	    constructor_elt *c1 = &(*v1)[idx];
	    constructor_elt *c2 = &(*v2)[idx];

	    /* Check that each value is the same...  */
	    if (!sem_variable::equals (c1->value, c2->value))
	      return false;
	    /* ... and that they apply to the same fields!  */
	    if (!sem_variable::equals (c1->index, c2->index))
	      return false;
	  }
	return true;
      }
    case MEM_REF:
      {
	tree x1 = TREE_OPERAND (t1, 0);
	tree x2 = TREE_OPERAND (t2, 0);
	tree y1 = TREE_OPERAND (t1, 1);
	tree y2 = TREE_OPERAND (t2, 1);

	if (!func_checker::compatible_types_p (TREE_TYPE (x1), TREE_TYPE (x2)))
	  return return_false ();

	/* Type of the offset on MEM_REF does not matter.  */
	return return_with_debug (sem_variable::equals (x1, x2)
			          && known_eq (wi::to_poly_offset (y1),
					       wi::to_poly_offset (y2)));
      }
    case ADDR_EXPR:
    case FDESC_EXPR:
      {
	tree op1 = TREE_OPERAND (t1, 0);
	tree op2 = TREE_OPERAND (t2, 0);
	return sem_variable::equals (op1, op2);
      }
    /* References to other vars/decls are compared using ipa-ref.  */
    case FUNCTION_DECL:
    case VAR_DECL:
      if (decl_in_symtab_p (t1) && decl_in_symtab_p (t2))
	return true;
      return return_false_with_msg ("Declaration mismatch");
    case CONST_DECL:
      /* TODO: We can check CONST_DECL by its DECL_INITIAL, but for that we
	 need to process its VAR/FUNCTION references without relying on ipa-ref
	 compare.  */
    case FIELD_DECL:
    case LABEL_DECL:
      return return_false_with_msg ("Declaration mismatch");
    case INTEGER_CST:
      /* Integer constants are the same only if the same width of type.  */
      if (TYPE_PRECISION (TREE_TYPE (t1)) != TYPE_PRECISION (TREE_TYPE (t2)))
        return return_false_with_msg ("INTEGER_CST precision mismatch");
      if (TYPE_MODE (TREE_TYPE (t1)) != TYPE_MODE (TREE_TYPE (t2)))
        return return_false_with_msg ("INTEGER_CST mode mismatch");
      return return_with_debug (tree_int_cst_equal (t1, t2));
    case STRING_CST:
      if (TYPE_MODE (TREE_TYPE (t1)) != TYPE_MODE (TREE_TYPE (t2)))
        return return_false_with_msg ("STRING_CST mode mismatch");
      if (TREE_STRING_LENGTH (t1) != TREE_STRING_LENGTH (t2))
	return return_false_with_msg ("STRING_CST length mismatch");
      if (memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
		    TREE_STRING_LENGTH (t1)))
	return return_false_with_msg ("STRING_CST mismatch");
      return true;
    case FIXED_CST:
      /* Fixed constants are the same only if the same width of type.  */
      if (TYPE_PRECISION (TREE_TYPE (t1)) != TYPE_PRECISION (TREE_TYPE (t2)))
        return return_false_with_msg ("FIXED_CST precision mismatch");

      return return_with_debug (FIXED_VALUES_IDENTICAL (TREE_FIXED_CST (t1),
							TREE_FIXED_CST (t2)));
    case COMPLEX_CST:
      return (sem_variable::equals (TREE_REALPART (t1), TREE_REALPART (t2))
	      && sem_variable::equals (TREE_IMAGPART (t1), TREE_IMAGPART (t2)));
    case REAL_CST:
      /* Real constants are the same only if the same width of type.  */
      if (TYPE_PRECISION (TREE_TYPE (t1)) != TYPE_PRECISION (TREE_TYPE (t2)))
        return return_false_with_msg ("REAL_CST precision mismatch");
      return return_with_debug (real_identical (&TREE_REAL_CST (t1),
						&TREE_REAL_CST (t2)));
    case VECTOR_CST:
      {
	if (maybe_ne (VECTOR_CST_NELTS (t1), VECTOR_CST_NELTS (t2)))
	  return return_false_with_msg ("VECTOR_CST nelts mismatch");

	unsigned int count
	  = tree_vector_builder::binary_encoded_nelts (t1, t2);
	for (unsigned int i = 0; i < count; ++i)
	  if (!sem_variable::equals (VECTOR_CST_ENCODED_ELT (t1, i),
				     VECTOR_CST_ENCODED_ELT (t2, i)))
	    return false;

	return true;
      }
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      {
	tree x1 = TREE_OPERAND (t1, 0);
	tree x2 = TREE_OPERAND (t2, 0);
	tree y1 = TREE_OPERAND (t1, 1);
	tree y2 = TREE_OPERAND (t2, 1);

	if (!sem_variable::equals (x1, x2) || !sem_variable::equals (y1, y2))
	  return false;
	if (!sem_variable::equals (array_ref_low_bound (t1),
				   array_ref_low_bound (t2)))
	  return false;
        if (!sem_variable::equals (array_ref_element_size (t1),
			           array_ref_element_size (t2)))
	  return false;
	return true;
      }
     
    case COMPONENT_REF:
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case RANGE_EXPR:
      {
	tree x1 = TREE_OPERAND (t1, 0);
	tree x2 = TREE_OPERAND (t2, 0);
	tree y1 = TREE_OPERAND (t1, 1);
	tree y2 = TREE_OPERAND (t2, 1);

	return sem_variable::equals (x1, x2) && sem_variable::equals (y1, y2);
      }

    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      if (!func_checker::compatible_types_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	  return return_false ();
      return sem_variable::equals (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
    case ERROR_MARK:
      return return_false_with_msg ("ERROR_MARK");
    default:
      return return_false_with_msg ("Unknown TREE code reached");
    }
}

/* Parser function that visits a varpool NODE.  */

sem_variable *
sem_variable::parse (varpool_node *node, bitmap_obstack *stack,
		     func_checker *checker)
{
  if (TREE_THIS_VOLATILE (node->decl) || DECL_HARD_REGISTER (node->decl)
      || node->alias)
    return NULL;

  sem_variable *v = new sem_variable (node, stack);
  v->init (checker);

  return v;
}

/* Semantic variable initialization function.  */

void
sem_variable::init (ipa_icf_gimple::func_checker *checker)
{
  decl = get_node ()->decl;

  /* All WPA streamed in symbols should have their hashes computed at compile
     time.  At this point, the constructor may not be in memory at all.
     DECL_INITIAL (decl) would be error_mark_node in that case.  */
  if (!m_hash_set)
    {
      gcc_assert (!node->lto_file_data);
      inchash::hash hstate;
      hstate.add_int (456346417);
      checker->hash_operand (DECL_INITIAL (decl), hstate, 0);
      set_hash (hstate.end ());
    }
}

/* References independent hash function.  */

hashval_t
sem_variable::get_hash (void)
{
  gcc_checking_assert (m_hash_set);
  return m_hash;
}

/* Merges instance with an ALIAS_ITEM, where alias, thunk or redirection can
   be applied.  */

bool
sem_variable::merge (sem_item *alias_item)
{
  gcc_assert (alias_item->type == VAR);

  AUTO_DUMP_SCOPE ("merge",
		   dump_user_location_t::from_function_decl (decl));
  if (!sem_item::target_supports_symbol_aliases_p ())
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION, "Not unifying; "
		     "Symbol aliases are not supported by target\n");
      return false;
    }

  if (DECL_EXTERNAL (alias_item->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; alias is external.\n");
      return false;
    }

  sem_variable *alias_var = static_cast<sem_variable *> (alias_item);

  varpool_node *original = get_node ();
  varpool_node *alias = alias_var->get_node ();
  bool original_discardable = false;

  bool alias_address_matters = alias->address_matters_p ();

  /* See if original is in a section that can be discarded if the main
     symbol is not used.
     Also consider case where we have resolution info and we know that
     original's definition is not going to be used.  In this case we cannot
     create alias to original.  */
  if (original->can_be_discarded_p ()
      || (node->resolution != LDPR_UNKNOWN
	  && !decl_binds_to_current_def_p (node->decl)))
    original_discardable = true;

  gcc_assert (!TREE_ASM_WRITTEN (alias->decl));

  /* Constant pool machinery is not quite ready for aliases.
     TODO: varasm code contains logic for merging DECL_IN_CONSTANT_POOL.
     For LTO merging does not happen that is an important missing feature.
     We can enable merging with LTO if the DECL_IN_CONSTANT_POOL
     flag is dropped and non-local symbol name is assigned.  */
  if (DECL_IN_CONSTANT_POOL (alias->decl)
      || DECL_IN_CONSTANT_POOL (original->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; constant pool variables.\n");
      return false;
    }

  /* Do not attempt to mix functions from different user sections;
     we do not know what user intends with those.  */
  if (((DECL_SECTION_NAME (original->decl) && !original->implicit_section)
       || (DECL_SECTION_NAME (alias->decl) && !alias->implicit_section))
      && DECL_SECTION_NAME (original->decl) != DECL_SECTION_NAME (alias->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; "
		     "original and alias are in different sections.\n");
      return false;
    }

  /* We cannot merge if address comparison matters.  */
  if (alias_address_matters && flag_merge_constants < 2)
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; address of original may be compared.\n");
      return false;
    }

  if (DECL_ALIGN (original->decl) != DECL_ALIGN (alias->decl)
      && (sanitize_flags_p (SANITIZE_ADDRESS, original->decl)
	  || sanitize_flags_p (SANITIZE_ADDRESS, alias->decl)))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; "
		     "ASAN requires equal alignments for original and alias\n");

      return false;
    }

  if (DECL_ALIGN (original->decl) < DECL_ALIGN (alias->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; "
		     "original and alias have incompatible alignments\n");

      return false;
    }

  if (DECL_COMDAT_GROUP (original->decl) != DECL_COMDAT_GROUP (alias->decl))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; alias cannot be created; "
		     "across comdat group boundary\n");

      return false;
    }

  if (original_discardable)
    {
      if (dump_enabled_p ())
	dump_printf (MSG_MISSED_OPTIMIZATION,
		     "Not unifying; alias cannot be created; "
		     "target is discardable\n");

      return false;
    }
  else
    {
      gcc_assert (!original->alias);
      gcc_assert (!alias->alias);

      alias->analyzed = false;

      DECL_INITIAL (alias->decl) = NULL;
      ((symtab_node *)alias)->call_for_symbol_and_aliases (clear_decl_rtl,
							   NULL, true);
      alias->remove_all_references ();
      if (TREE_ADDRESSABLE (alias->decl))
        original->call_for_symbol_and_aliases (set_addressable, NULL, true);

      varpool_node::create_alias (alias_var->decl, decl);
      alias->resolve_alias (original);

      if (dump_enabled_p ())
	dump_printf (MSG_OPTIMIZED_LOCATIONS,
		     "Unified; Variable alias has been created.\n");

      return true;
    }
}

/* Dump symbol to FILE.  */

void
sem_variable::dump_to_file (FILE *file)
{
  gcc_assert (file);

  print_node (file, "", decl, 0);
  fprintf (file, "\n\n");
}

unsigned int sem_item_optimizer::class_id = 0;

sem_item_optimizer::sem_item_optimizer ()
: worklist (0), m_classes (0), m_classes_count (0), m_cgraph_node_hooks (NULL),
  m_varpool_node_hooks (NULL), m_merged_variables (), m_references ()
{
  m_items.create (0);
  bitmap_obstack_initialize (&m_bmstack);
}

sem_item_optimizer::~sem_item_optimizer ()
{
  for (unsigned int i = 0; i < m_items.length (); i++)
    delete m_items[i];


  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    {
      for (unsigned int i = 0; i < (*it)->classes.length (); i++)
	delete (*it)->classes[i];

      (*it)->classes.release ();
      free (*it);
    }

  m_items.release ();

  bitmap_obstack_release (&m_bmstack);
  m_merged_variables.release ();
}

/* Write IPA ICF summary for symbols.  */

void
sem_item_optimizer::write_summary (void)
{
  unsigned int count = 0;

  output_block *ob = create_output_block (LTO_section_ipa_icf);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  ob->symbol = NULL;

  /* Calculate number of symbols to be serialized.  */
  for (lto_symtab_encoder_iterator lsei = lsei_start_in_partition (encoder);
       !lsei_end_p (lsei);
       lsei_next_in_partition (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (m_symtab_node_map.get (node))
	count++;
    }

  streamer_write_uhwi (ob, count);

  /* Process all of the symbols.  */
  for (lto_symtab_encoder_iterator lsei = lsei_start_in_partition (encoder);
       !lsei_end_p (lsei);
       lsei_next_in_partition (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      sem_item **item = m_symtab_node_map.get (node);

      if (item && *item)
	{
	  int node_ref = lto_symtab_encoder_encode (encoder, node);
	  streamer_write_uhwi_stream (ob->main_stream, node_ref);

	  streamer_write_uhwi (ob, (*item)->get_hash ());

	  if ((*item)->type == FUNC)
	    {
	      sem_function *fn = static_cast<sem_function *> (*item);
	      streamer_write_uhwi (ob, fn->memory_access_types.length ());
	      for (unsigned i = 0; i < fn->memory_access_types.length (); i++)
		stream_write_tree (ob, fn->memory_access_types[i], true);
	    }
	}
    }

  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);
}

/* Reads a section from LTO stream file FILE_DATA. Input block for DATA
   contains LEN bytes.  */

void
sem_item_optimizer::read_section (lto_file_decl_data *file_data,
				  const char *data, size_t len)
{
  const lto_function_header *header
    = (const lto_function_header *) data;
  const int cfg_offset = sizeof (lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  data_in *data_in;
  unsigned int i;
  unsigned int count;

  lto_input_block ib_main ((const char *) data + main_offset, 0,
			   header->main_size, file_data->mode_table);

  data_in
    = lto_data_in_create (file_data, (const char *) data + string_offset,
			  header->string_size, vNULL);

  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      symtab_node *node;
      lto_symtab_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->symtab_node_encoder;
      node = lto_symtab_encoder_deref (encoder, index);

      hashval_t hash = streamer_read_uhwi (&ib_main);
      gcc_assert (node->definition);

      if (is_a<cgraph_node *> (node))
	{
	  cgraph_node *cnode = dyn_cast <cgraph_node *> (node);

	  sem_function *fn = new sem_function (cnode, &m_bmstack);
	  unsigned count = streamer_read_uhwi (&ib_main);
	  inchash::hash hstate (0);
	  if (flag_incremental_link == INCREMENTAL_LINK_LTO)
	    fn->memory_access_types.reserve_exact (count);
	  for (unsigned i = 0; i < count; i++)
	    {
	      tree type = stream_read_tree (&ib_main, data_in);
	      hstate.add_int (get_deref_alias_set (type));
	      if (flag_incremental_link == INCREMENTAL_LINK_LTO)
		fn->memory_access_types.quick_push (type);
	    }
	  fn->m_alias_sets_hash = hstate.end ();
	  fn->set_hash (hash);
	  m_items.safe_push (fn);
	}
      else
	{
	  varpool_node *vnode = dyn_cast <varpool_node *> (node);

	  sem_variable *var = new sem_variable (vnode, &m_bmstack);
	  var->set_hash (hash);
	  m_items.safe_push (var);
	}
    }

  lto_free_section_data (file_data, LTO_section_ipa_icf, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Read IPA ICF summary for symbols.  */

void
sem_item_optimizer::read_summary (void)
{
  lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  lto_file_decl_data *file_data;
  unsigned int j = 0;

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data
	= lto_get_summary_section_data (file_data, LTO_section_ipa_icf, &len);
      if (data)
	read_section (file_data, data, len);
    }
}

/* Register callgraph and varpool hooks.  */

void
sem_item_optimizer::register_hooks (void)
{
  if (!m_cgraph_node_hooks)
    m_cgraph_node_hooks = symtab->add_cgraph_removal_hook
			  (&sem_item_optimizer::cgraph_removal_hook, this);

  if (!m_varpool_node_hooks)
    m_varpool_node_hooks = symtab->add_varpool_removal_hook
			   (&sem_item_optimizer::varpool_removal_hook, this);
}

/* Unregister callgraph and varpool hooks.  */

void
sem_item_optimizer::unregister_hooks (void)
{
  if (m_cgraph_node_hooks)
    symtab->remove_cgraph_removal_hook (m_cgraph_node_hooks);

  if (m_varpool_node_hooks)
    symtab->remove_varpool_removal_hook (m_varpool_node_hooks);
}

/* Adds a CLS to hashtable associated by hash value.  */

void
sem_item_optimizer::add_class (congruence_class *cls)
{
  gcc_assert (cls->members.length ());

  congruence_class_group *group
    = get_group_by_hash (cls->members[0]->get_hash (),
			 cls->members[0]->type);
  group->classes.safe_push (cls);
}

/* Gets a congruence class group based on given HASH value and TYPE.  */

congruence_class_group *
sem_item_optimizer::get_group_by_hash (hashval_t hash, sem_item_type type)
{
  congruence_class_group *item = XNEW (congruence_class_group);
  item->hash = hash;
  item->type = type;

  congruence_class_group **slot = m_classes.find_slot (item, INSERT);

  if (*slot)
    free (item);
  else
    {
      item->classes.create (1);
      *slot = item;
    }

  return *slot;
}

/* Callgraph removal hook called for a NODE with a custom DATA.  */

void
sem_item_optimizer::cgraph_removal_hook (cgraph_node *node, void *data)
{
  sem_item_optimizer *optimizer = (sem_item_optimizer *) data;
  optimizer->remove_symtab_node (node);
}

/* Varpool removal hook called for a NODE with a custom DATA.  */

void
sem_item_optimizer::varpool_removal_hook (varpool_node *node, void *data)
{
  sem_item_optimizer *optimizer = (sem_item_optimizer *) data;
  optimizer->remove_symtab_node (node);
}

/* Remove symtab NODE triggered by symtab removal hooks.  */

void
sem_item_optimizer::remove_symtab_node (symtab_node *node)
{
  gcc_assert (m_classes.is_empty ());

  m_removed_items_set.add (node);
}

void
sem_item_optimizer::remove_item (sem_item *item)
{
  if (m_symtab_node_map.get (item->node))
    m_symtab_node_map.remove (item->node);
  delete item;
}

/* Removes all callgraph and varpool nodes that are marked by symtab
   as deleted.  */

void
sem_item_optimizer::filter_removed_items (void)
{
  auto_vec <sem_item *> filtered;

  for (unsigned int i = 0; i < m_items.length(); i++)
    {
      sem_item *item = m_items[i];

      if (m_removed_items_set.contains (item->node))
        {
	  remove_item (item);
	  continue;
        }

      if (item->type == FUNC)
        {
	  cgraph_node *cnode = static_cast <sem_function *>(item)->get_node ();

	  if (in_lto_p && (cnode->alias || cnode->body_removed))
	    remove_item (item);
	  else
	    filtered.safe_push (item);
        }
      else /* VAR.  */
        {
	  if (!flag_ipa_icf_variables)
	    remove_item (item);
	  else
	    {
	      /* Filter out non-readonly variables.  */
	      tree decl = item->decl;
	      if (TREE_READONLY (decl))
		filtered.safe_push (item);
	      else
		remove_item (item);
	    }
        }
    }

  /* Clean-up of released semantic items.  */

  m_items.release ();
  for (unsigned int i = 0; i < filtered.length(); i++)
    m_items.safe_push (filtered[i]);
}

/* Optimizer entry point which returns true in case it processes
   a merge operation. True is returned if there's a merge operation
   processed.  */

bool
sem_item_optimizer::execute (void)
{
  filter_removed_items ();
  unregister_hooks ();

  build_graph ();
  update_hash_by_addr_refs ();
  update_hash_by_memory_access_type ();
  build_hash_based_classes ();

  if (dump_file)
    fprintf (dump_file, "Dump after hash based groups\n");
  dump_cong_classes ();

  subdivide_classes_by_equality (true);

  if (dump_file)
    fprintf (dump_file, "Dump after WPA based types groups\n");

  dump_cong_classes ();

  process_cong_reduction ();
  checking_verify_classes ();

  if (dump_file)
    fprintf (dump_file, "Dump after callgraph-based congruence reduction\n");

  dump_cong_classes ();

  unsigned int loaded_symbols = parse_nonsingleton_classes ();
  subdivide_classes_by_equality ();

  if (dump_file)
    fprintf (dump_file, "Dump after full equality comparison of groups\n");

  dump_cong_classes ();

  unsigned int prev_class_count = m_classes_count;

  process_cong_reduction ();
  dump_cong_classes ();
  checking_verify_classes ();
  bool merged_p = merge_classes (prev_class_count, loaded_symbols);

  if (dump_file && (dump_flags & TDF_DETAILS))
    symtab->dump (dump_file);

  return merged_p;
}

/* Function responsible for visiting all potential functions and
   read-only variables that can be merged.  */

void
sem_item_optimizer::parse_funcs_and_vars (void)
{
  cgraph_node *cnode;

  /* Create dummy func_checker for hashing purpose.  */
  func_checker checker;

  if (flag_ipa_icf_functions)
    FOR_EACH_DEFINED_FUNCTION (cnode)
    {
      sem_function *f = sem_function::parse (cnode, &m_bmstack, &checker);
      if (f)
	{
	  m_items.safe_push (f);
	  m_symtab_node_map.put (cnode, f);
	}
    }

  varpool_node *vnode;

  if (flag_ipa_icf_variables)
    FOR_EACH_DEFINED_VARIABLE (vnode)
    {
      sem_variable *v = sem_variable::parse (vnode, &m_bmstack, &checker);

      if (v)
	{
	  m_items.safe_push (v);
	  m_symtab_node_map.put (vnode, v);
	}
    }
}

/* Makes pairing between a congruence class CLS and semantic ITEM.  */

void
sem_item_optimizer::add_item_to_class (congruence_class *cls, sem_item *item)
{
  item->index_in_class = cls->members.length ();
  cls->members.safe_push (item);
  cls->referenced_by_count += item->referenced_by_count;
  item->cls = cls;
}

/* For each semantic item, append hash values of references.  */

void
sem_item_optimizer::update_hash_by_addr_refs ()
{
  /* First, append to hash sensitive references and class type if it need to
     be matched for ODR.  */
  for (unsigned i = 0; i < m_items.length (); i++)
    {
      m_items[i]->update_hash_by_addr_refs (m_symtab_node_map);
      if (m_items[i]->type == FUNC)
	{
	  if (TREE_CODE (TREE_TYPE (m_items[i]->decl)) == METHOD_TYPE
	      && contains_polymorphic_type_p
		   (TYPE_METHOD_BASETYPE (TREE_TYPE (m_items[i]->decl)))
	      && (DECL_CXX_CONSTRUCTOR_P (m_items[i]->decl)
		  || (static_cast<sem_function *> (m_items[i])->param_used_p (0)
		      && static_cast<sem_function *> (m_items[i])
			   ->compare_polymorphic_p ())))
	     {
	        tree class_type
		  = TYPE_METHOD_BASETYPE (TREE_TYPE (m_items[i]->decl));
		inchash::hash hstate (m_items[i]->get_hash ());

		/* Hash ODR types by mangled name if it is defined.
		   If not we know that type is anonymous of free_lang_data
		   was not run and in that case type main variants are
		   unique.  */
		if (TYPE_NAME (class_type)
		     && DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (class_type))
		     && !type_in_anonymous_namespace_p
				 (class_type))
		  hstate.add_hwi
		    (IDENTIFIER_HASH_VALUE
		       (DECL_ASSEMBLER_NAME (TYPE_NAME (class_type))));
		else
		  {
		    gcc_checking_assert
			 (!in_lto_p
			  || type_in_anonymous_namespace_p (class_type));
		    hstate.add_hwi (TYPE_UID (TYPE_MAIN_VARIANT (class_type)));
		  }

		m_items[i]->set_hash (hstate.end ());
	     }
	}
    }

  /* Once all symbols have enhanced hash value, we can append
     hash values of symbols that are seen by IPA ICF and are
     references by a semantic item. Newly computed values
     are saved to global_hash member variable.  */
  for (unsigned i = 0; i < m_items.length (); i++)
    m_items[i]->update_hash_by_local_refs (m_symtab_node_map);

  /* Global hash value replace current hash values.  */
  for (unsigned i = 0; i < m_items.length (); i++)
    m_items[i]->set_hash (m_items[i]->global_hash);
}

void
sem_item_optimizer::update_hash_by_memory_access_type ()
{
  for (unsigned i = 0; i < m_items.length (); i++)
    {
      if (m_items[i]->type == FUNC)
	{
	  sem_function *fn = static_cast<sem_function *> (m_items[i]);
	  inchash::hash hstate (fn->get_hash ());
	  hstate.add_int (fn->m_alias_sets_hash);
	  fn->set_hash (hstate.end ());
	}
    }
}

/* Congruence classes are built by hash value.  */

void
sem_item_optimizer::build_hash_based_classes (void)
{
  for (unsigned i = 0; i < m_items.length (); i++)
    {
      sem_item *item = m_items[i];

      congruence_class_group *group
	= get_group_by_hash (item->get_hash (), item->type);

      if (!group->classes.length ())
	{
	  m_classes_count++;
	  group->classes.safe_push (new congruence_class (class_id++));
	}

      add_item_to_class (group->classes[0], item);
    }
}

/* Build references according to call graph.  */

void
sem_item_optimizer::build_graph (void)
{
  for (unsigned i = 0; i < m_items.length (); i++)
    {
      sem_item *item = m_items[i];
      m_symtab_node_map.put (item->node, item);

      /* Initialize hash values if we are not in LTO mode.  */
      if (!in_lto_p)
	item->get_hash ();
    }

  for (unsigned i = 0; i < m_items.length (); i++)
    {
      sem_item *item = m_items[i];

      if (item->type == FUNC)
	{
	  cgraph_node *cnode = dyn_cast <cgraph_node *> (item->node);

	  cgraph_edge *e = cnode->callees;
	  while (e)
	    {
	      sem_item **slot = m_symtab_node_map.get
		(e->callee->ultimate_alias_target ());
	      if (slot)
		item->add_reference (&m_references, *slot);

	      e = e->next_callee;
	    }
	}

      ipa_ref *ref = NULL;
      for (unsigned i = 0; item->node->iterate_reference (i, ref); i++)
	{
	  sem_item **slot = m_symtab_node_map.get
	    (ref->referred->ultimate_alias_target ());
	  if (slot)
	    item->add_reference (&m_references, *slot);
	}
    }
}

/* Semantic items in classes having more than one element and initialized.
   In case of WPA, we load function body.  */

unsigned int
sem_item_optimizer::parse_nonsingleton_classes (void)
{
  unsigned int counter = 0;

  /* Create dummy func_checker for hashing purpose.  */
  func_checker checker;

  for (unsigned i = 0; i < m_items.length (); i++)
    if (m_items[i]->cls->members.length () > 1)
      {
	m_items[i]->init (&checker);
	++counter;
      }

  if (dump_file)
    {
      float f = m_items.length () ? 100.0f * counter / m_items.length () : 0.0f;
      fprintf (dump_file, "Init called for %u items (%.2f%%).\n", counter, f);
    }

  return counter;
}

/* Equality function for semantic items is used to subdivide existing
   classes. If IN_WPA, fast equality function is invoked.  */

void
sem_item_optimizer::subdivide_classes_by_equality (bool in_wpa)
{
  for (hash_table <congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    {
      unsigned int class_count = (*it)->classes.length ();

      for (unsigned i = 0; i < class_count; i++)
	{
	  congruence_class *c = (*it)->classes[i];

	  if (c->members.length() > 1)
	    {
	      auto_vec <sem_item *> new_vector;

	      sem_item *first = c->members[0];
	      new_vector.safe_push (first);

	      unsigned class_split_first = (*it)->classes.length ();

	      for (unsigned j = 1; j < c->members.length (); j++)
		{
		  sem_item *item = c->members[j];

		  bool equals
		    = in_wpa ? first->equals_wpa (item, m_symtab_node_map)
			     : first->equals (item, m_symtab_node_map);

		  if (equals)
		    new_vector.safe_push (item);
		  else
		    {
		      bool integrated = false;

		      for (unsigned k = class_split_first;
			   k < (*it)->classes.length (); k++)
			{
			  sem_item *x = (*it)->classes[k]->members[0];
			  bool equals
			    = in_wpa ? x->equals_wpa (item, m_symtab_node_map)
				     : x->equals (item, m_symtab_node_map);

			  if (equals)
			    {
			      integrated = true;
			      add_item_to_class ((*it)->classes[k], item);

			      break;
			    }
			}

		      if (!integrated)
			{
			  congruence_class *c
			    = new congruence_class (class_id++);
			  m_classes_count++;
			  add_item_to_class (c, item);

			  (*it)->classes.safe_push (c);
			}
		    }
		}

	      // We replace newly created new_vector for the class we've just
	      // splitted.
	      c->members.release ();
	      c->members.create (new_vector.length ());

	      for (unsigned int j = 0; j < new_vector.length (); j++)
		add_item_to_class (c, new_vector[j]);
	    }
	}
    }

  checking_verify_classes ();
}

/* Subdivide classes by address references that members of the class
   reference. Example can be a pair of functions that have an address
   taken from a function. If these addresses are different the class
   is split.  */

unsigned
sem_item_optimizer::subdivide_classes_by_sensitive_refs ()
{
  typedef hash_map <symbol_compare_hash, vec <sem_item *> > subdivide_hash_map;

  unsigned newly_created_classes = 0;

  for (hash_table <congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    {
      unsigned int class_count = (*it)->classes.length ();
      auto_vec<congruence_class *> new_classes;

      for (unsigned i = 0; i < class_count; i++)
	{
	  congruence_class *c = (*it)->classes[i];

	  if (c->members.length() > 1)
	    {
	      subdivide_hash_map split_map;

	      for (unsigned j = 0; j < c->members.length (); j++)
	        {
		  sem_item *source_node = c->members[j];

		  symbol_compare_collection *collection
		    = new symbol_compare_collection (source_node->node);

		  bool existed;
		  vec <sem_item *> *slot
		    = &split_map.get_or_insert (collection, &existed);
		  gcc_checking_assert (slot);

		  slot->safe_push (source_node);

		  if (existed)
		    delete collection;
	        }

	       /* If the map contains more than one key, we have to split
		  the map appropriately.  */
	      if (split_map.elements () != 1)
	        {
		  bool first_class = true;

		  for (subdivide_hash_map::iterator it2 = split_map.begin ();
		       it2 != split_map.end (); ++it2)
		    {
		      congruence_class *new_cls;
		      new_cls = new congruence_class (class_id++);

		      for (unsigned k = 0; k < (*it2).second.length (); k++)
			add_item_to_class (new_cls, (*it2).second[k]);

		      worklist_push (new_cls);
		      newly_created_classes++;

		      if (first_class)
		        {
			  (*it)->classes[i] = new_cls;
			  first_class = false;
			}
		      else
		        {
		          new_classes.safe_push (new_cls);
			  m_classes_count++;
		        }
		    }
		}

	      /* Release memory.  */
	      for (subdivide_hash_map::iterator it2 = split_map.begin ();
		   it2 != split_map.end (); ++it2)
		{
		  delete (*it2).first;
		  (*it2).second.release ();
		}
	    }
	  }

	for (unsigned i = 0; i < new_classes.length (); i++)
	  (*it)->classes.safe_push (new_classes[i]);
    }

  return newly_created_classes;
}

/* Verify congruence classes, if checking is enabled.  */

void
sem_item_optimizer::checking_verify_classes (void)
{
  if (flag_checking)
    verify_classes ();
}

/* Verify congruence classes.  */

void
sem_item_optimizer::verify_classes (void)
{
  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    {
      for (unsigned int i = 0; i < (*it)->classes.length (); i++)
	{
	  congruence_class *cls = (*it)->classes[i];

	  gcc_assert (cls);
	  gcc_assert (cls->members.length () > 0);

	  for (unsigned int j = 0; j < cls->members.length (); j++)
	    {
	      sem_item *item = cls->members[j];

	      gcc_assert (item);
	      gcc_assert (item->cls == cls);
	    }
	}
    }
}

/* Disposes split map traverse function. CLS_PTR is pointer to congruence
   class, BSLOT is bitmap slot we want to release. DATA is mandatory,
   but unused argument.  */

bool
sem_item_optimizer::release_split_map (congruence_class * const &,
				       bitmap const &b, traverse_split_pair *)
{
  bitmap bmp = b;

  BITMAP_FREE (bmp);

  return true;
}

/* Process split operation for a class given as pointer CLS_PTR,
   where bitmap B splits congruence class members. DATA is used
   as argument of split pair.  */

bool
sem_item_optimizer::traverse_congruence_split (congruence_class * const &cls,
					       bitmap const &b,
					       traverse_split_pair *pair)
{
  sem_item_optimizer *optimizer = pair->optimizer;
  const congruence_class *splitter_cls = pair->cls;

  /* If counted bits are greater than zero and less than the number of members
     a group will be splitted.  */
  unsigned popcount = bitmap_count_bits (b);

  if (popcount > 0 && popcount < cls->members.length ())
    {
      auto_vec <congruence_class *, 2> newclasses;
      newclasses.quick_push (new congruence_class (class_id++));
      newclasses.quick_push (new congruence_class (class_id++));

      for (unsigned int i = 0; i < cls->members.length (); i++)
	{
	  int target = bitmap_bit_p (b, i);
	  congruence_class *tc = newclasses[target];

	  add_item_to_class (tc, cls->members[i]);
	}

      if (flag_checking)
	{
	  for (unsigned int i = 0; i < 2; i++)
	    gcc_assert (newclasses[i]->members.length ());
	}

      if (splitter_cls == cls)
	optimizer->splitter_class_removed = true;

      /* Remove old class from worklist if presented.  */
      bool in_worklist = cls->in_worklist;

      if (in_worklist)
	cls->in_worklist = false;

      congruence_class_group g;
      g.hash = cls->members[0]->get_hash ();
      g.type = cls->members[0]->type;

      congruence_class_group *slot = optimizer->m_classes.find (&g);

      for (unsigned int i = 0; i < slot->classes.length (); i++)
	if (slot->classes[i] == cls)
	  {
	    slot->classes.ordered_remove (i);
	    break;
	  }

      /* New class will be inserted and integrated to work list.  */
      for (unsigned int i = 0; i < 2; i++)
	optimizer->add_class (newclasses[i]);

      /* Two classes replace one, so that increment just by one.  */
      optimizer->m_classes_count++;

      /* If OLD class was presented in the worklist, we remove the class
         and replace it will both newly created classes.  */
      if (in_worklist)
	for (unsigned int i = 0; i < 2; i++)
	  optimizer->worklist_push (newclasses[i]);
      else /* Just smaller class is inserted.  */
	{
	  unsigned int smaller_index
	    = (newclasses[0]->members.length ()
	       < newclasses[1]->members.length ()
	       ? 0 : 1);
	  optimizer->worklist_push (newclasses[smaller_index]);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  congruence class splitted:\n");
	  cls->dump (dump_file, 4);

	  fprintf (dump_file, "  newly created groups:\n");
	  for (unsigned int i = 0; i < 2; i++)
	    newclasses[i]->dump (dump_file, 4);
	}

      /* Release class if not presented in work list.  */
      if (!in_worklist)
	delete cls;

      return true;
    }

  return false;
}

/* Compare function for sorting pairs in do_congruence_step_f.  */

int
sem_item_optimizer::sort_congruence_split (const void *a_, const void *b_)
{
  const std::pair<congruence_class *, bitmap> *a
    = (const std::pair<congruence_class *, bitmap> *)a_;
  const std::pair<congruence_class *, bitmap> *b
    = (const std::pair<congruence_class *, bitmap> *)b_;
  if (a->first->id < b->first->id)
    return -1;
  else if (a->first->id > b->first->id)
    return 1;
  return 0;
}

/* Tests if a class CLS used as INDEXth splits any congruence classes.
   Bitmap stack BMSTACK is used for bitmap allocation.  */

bool
sem_item_optimizer::do_congruence_step_for_index (congruence_class *cls,
						  unsigned int index)
{
  hash_map <congruence_class *, bitmap> split_map;

  for (unsigned int i = 0; i < cls->members.length (); i++)
    {
      sem_item *item = cls->members[i];
      sem_usage_pair needle (item, index);
      vec<sem_item *> *callers = m_references.get (&needle);
      if (callers == NULL)
	continue;

      for (unsigned int j = 0; j < callers->length (); j++)
	{
	  sem_item *caller = (*callers)[j];
	  if (caller->cls->members.length () < 2)
	    continue;
	  bitmap *slot = split_map.get (caller->cls);
	  bitmap b;

	  if(!slot)
	    {
	      b = BITMAP_ALLOC (&m_bmstack);
	      split_map.put (caller->cls, b);
	    }
	  else
	    b = *slot;

	  gcc_checking_assert (caller->cls);
	  gcc_checking_assert (caller->index_in_class
			       < caller->cls->members.length ());

	  bitmap_set_bit (b, caller->index_in_class);
	}
    }

  auto_vec<std::pair<congruence_class *, bitmap> > to_split;
  to_split.reserve_exact (split_map.elements ());
  for (hash_map <congruence_class *, bitmap>::iterator i = split_map.begin ();
       i != split_map.end (); ++i)
    to_split.safe_push (*i);
  to_split.qsort (sort_congruence_split);

  traverse_split_pair pair;
  pair.optimizer = this;
  pair.cls = cls;

  splitter_class_removed = false;
  bool r = false;
  for (unsigned i = 0; i < to_split.length (); ++i)
    r |= traverse_congruence_split (to_split[i].first, to_split[i].second,
				    &pair);

  /* Bitmap clean-up.  */
  split_map.traverse <traverse_split_pair *,
		      sem_item_optimizer::release_split_map> (NULL);

  return r;
}

/* Every usage of a congruence class CLS is a candidate that can split the
   collection of classes. Bitmap stack BMSTACK is used for bitmap
   allocation.  */

void
sem_item_optimizer::do_congruence_step (congruence_class *cls)
{
  bitmap_iterator bi;
  unsigned int i;

  bitmap usage = BITMAP_ALLOC (&m_bmstack);

  for (unsigned int i = 0; i < cls->members.length (); i++)
    bitmap_ior_into (usage, cls->members[i]->usage_index_bitmap);

  EXECUTE_IF_SET_IN_BITMAP (usage, 0, i, bi)
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      fprintf (dump_file, "  processing congruence step for class: %u "
	       "(%u items, %u references), index: %u\n", cls->id,
	       cls->referenced_by_count, cls->members.length (), i);
    do_congruence_step_for_index (cls, i);

    if (splitter_class_removed)
      break;
  }

  BITMAP_FREE (usage);
}

/* Adds a newly created congruence class CLS to worklist.  */

void
sem_item_optimizer::worklist_push (congruence_class *cls)
{
  /* Return if the class CLS is already presented in work list.  */
  if (cls->in_worklist)
    return;

  cls->in_worklist = true;
  worklist.insert (cls->referenced_by_count, cls);
}

/* Pops a class from worklist. */

congruence_class *
sem_item_optimizer::worklist_pop (void)
{
  congruence_class *cls;

  while (!worklist.empty ())
    {
      cls = worklist.extract_min ();
      if (cls->in_worklist)
	{
	  cls->in_worklist = false;

	  return cls;
	}
      else
	{
	  /* Work list item was already intended to be removed.
	     The only reason for doing it is to split a class.
	     Thus, the class CLS is deleted.  */
	  delete cls;
	}
    }

  return NULL;
}

/* Iterative congruence reduction function.  */

void
sem_item_optimizer::process_cong_reduction (void)
{
  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    for (unsigned i = 0; i < (*it)->classes.length (); i++)
      if ((*it)->classes[i]->is_class_used ())
	worklist_push ((*it)->classes[i]);

  if (dump_file)
    fprintf (dump_file, "Worklist has been filled with: %lu\n",
	     (unsigned long) worklist.nodes ());

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Congruence class reduction\n");

  congruence_class *cls;

  /* Process complete congruence reduction.  */
  while ((cls = worklist_pop ()) != NULL)
    do_congruence_step (cls);

  /* Subdivide newly created classes according to references.  */
  unsigned new_classes = subdivide_classes_by_sensitive_refs ();

  if (dump_file)
    fprintf (dump_file, "Address reference subdivision created: %u "
	     "new classes.\n", new_classes);
}

/* Debug function prints all informations about congruence classes.  */

void
sem_item_optimizer::dump_cong_classes (void)
{
  if (!dump_file)
    return;

  /* Histogram calculation.  */
  unsigned int max_index = 0;
  unsigned int single_element_classes = 0;
  unsigned int* histogram = XCNEWVEC (unsigned int, m_items.length () + 1);

  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    for (unsigned i = 0; i < (*it)->classes.length (); i++)
      {
	unsigned int c = (*it)->classes[i]->members.length ();
	histogram[c]++;

	if (c > max_index)
	  max_index = c;

	if (c == 1)
	  ++single_element_classes;
      }

  fprintf (dump_file,
	   "Congruence classes: %lu with total: %u items (in a non-singular "
	   "class: %u)\n", (unsigned long) m_classes.elements (),
	   m_items.length (), m_items.length () - single_element_classes);
  fprintf (dump_file,
	   "Class size histogram [number of members]: number of classes\n");
  for (unsigned int i = 0; i <= max_index; i++)
    if (histogram[i])
      fprintf (dump_file, "%6u: %6u\n", i, histogram[i]);

  if (dump_flags & TDF_DETAILS)
    for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
	 it != m_classes.end (); ++it)
      {
	fprintf (dump_file, "  group: with %u classes:\n",
		 (*it)->classes.length ());

	for (unsigned i = 0; i < (*it)->classes.length (); i++)
	  {
	    (*it)->classes[i]->dump (dump_file, 4);

	    if (i < (*it)->classes.length () - 1)
	      fprintf (dump_file, " ");
	  }
      }

  free (histogram);
}

/* Sort pair of sem_items A and B by DECL_UID.  */

static int
sort_sem_items_by_decl_uid (const void *a, const void *b)
{
  const sem_item *i1 = *(const sem_item * const *)a;
  const sem_item *i2 = *(const sem_item * const *)b;

  int uid1 = DECL_UID (i1->decl);
  int uid2 = DECL_UID (i2->decl);
  return uid1 - uid2;
}

/* Sort pair of congruence_classes A and B by DECL_UID of the first member.  */

static int
sort_congruence_classes_by_decl_uid (const void *a, const void *b)
{
  const congruence_class *c1 = *(const congruence_class * const *)a;
  const congruence_class *c2 = *(const congruence_class * const *)b;

  int uid1 = DECL_UID (c1->members[0]->decl);
  int uid2 = DECL_UID (c2->members[0]->decl);
  return uid1 - uid2;
}

/* Sort pair of congruence_class_groups A and B by
   DECL_UID of the first member of a first group.  */

static int
sort_congruence_class_groups_by_decl_uid (const void *a, const void *b)
{
  const std::pair<congruence_class_group *, int> *g1
    = (const std::pair<congruence_class_group *, int> *) a;
  const std::pair<congruence_class_group *, int> *g2
    = (const std::pair<congruence_class_group *, int> *) b;
  return g1->second - g2->second;
}

/* After reduction is done, we can declare all items in a group
   to be equal. PREV_CLASS_COUNT is start number of classes
   before reduction. True is returned if there's a merge operation
   processed.  LOADED_SYMBOLS is number of symbols that were loaded
   in WPA.  */

bool
sem_item_optimizer::merge_classes (unsigned int prev_class_count,
				   unsigned int loaded_symbols)
{
  unsigned int item_count = m_items.length ();
  unsigned int class_count = m_classes_count;
  unsigned int equal_items = item_count - class_count;

  unsigned int non_singular_classes_count = 0;
  unsigned int non_singular_classes_sum = 0;

  bool merged_p = false;

  /* PR lto/78211
     Sort functions in congruence classes by DECL_UID and do the same
     for the classes to not to break -fcompare-debug.  */

  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    {
      for (unsigned int i = 0; i < (*it)->classes.length (); i++)
	{
	  congruence_class *c = (*it)->classes[i];
	  c->members.qsort (sort_sem_items_by_decl_uid);
	}

      (*it)->classes.qsort (sort_congruence_classes_by_decl_uid);
    }

  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    for (unsigned int i = 0; i < (*it)->classes.length (); i++)
      {
	congruence_class *c = (*it)->classes[i];
	if (c->members.length () > 1)
	  {
	    non_singular_classes_count++;
	    non_singular_classes_sum += c->members.length ();
	  }
      }

  auto_vec<std::pair<congruence_class_group *, int> > classes (
    m_classes.elements ());
  for (hash_table<congruence_class_hash>::iterator it = m_classes.begin ();
       it != m_classes.end (); ++it)
    {
      int uid = DECL_UID ((*it)->classes[0]->members[0]->decl);
      classes.quick_push (std::pair<congruence_class_group *, int> (*it, uid));
    }

  classes.qsort (sort_congruence_class_groups_by_decl_uid);

  if (dump_file)
    {
      fprintf (dump_file, "\nItem count: %u\n", item_count);
      fprintf (dump_file, "Congruent classes before: %u, after: %u\n",
	       prev_class_count, class_count);
      fprintf (dump_file, "Average class size before: %.2f, after: %.2f\n",
	       prev_class_count ? 1.0f * item_count / prev_class_count : 0.0f,
	       class_count ? 1.0f * item_count / class_count : 0.0f);
      fprintf (dump_file, "Average non-singular class size: %.2f, count: %u\n",
	       non_singular_classes_count ? 1.0f * non_singular_classes_sum /
	       non_singular_classes_count : 0.0f,
	       non_singular_classes_count);
      fprintf (dump_file, "Equal symbols: %u\n", equal_items);
      unsigned total = equal_items + non_singular_classes_count;
      fprintf (dump_file, "Totally needed symbols: %u"
	       ", fraction of loaded symbols: %.2f%%\n\n", total,
	       loaded_symbols ? 100.0f * total / loaded_symbols: 0.0f);
    }

  unsigned int l;
  std::pair<congruence_class_group *, int> *it;
  FOR_EACH_VEC_ELT (classes, l, it)
    for (unsigned int i = 0; i < it->first->classes.length (); i++)
      {
	congruence_class *c = it->first->classes[i];

	if (c->members.length () == 1)
	  continue;

	sem_item *source = c->members[0];

	if (DECL_NAME (source->decl)
	    && MAIN_NAME_P (DECL_NAME (source->decl)))
	  /* If merge via wrappers, picking main as the target can be
	     problematic.  */
	  source = c->members[1];

	for (unsigned int j = 0; j < c->members.length (); j++)
	  {
	    sem_item *alias = c->members[j];

	    if (alias == source)
	      continue;

	    dump_user_location_t loc
	      = dump_user_location_t::from_function_decl (source->decl);
	    if (dump_enabled_p ())
	      {
		dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
				 "Semantic equality hit:%s->%s\n",
				 source->node->dump_name (),
				 alias->node->dump_name ());
		dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
				 "Assembler symbol names:%s->%s\n",
				 source->node->dump_asm_name (),
				 alias->node->dump_asm_name ());
	      }

	    if (lookup_attribute ("no_icf", DECL_ATTRIBUTES (alias->decl)))
	      {
		if (dump_enabled_p ())
		  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
				   "Merge operation is skipped due to no_icf "
				   "attribute.\n");
		continue;
	      }

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		source->dump_to_file (dump_file);
		alias->dump_to_file (dump_file);
	      }

	    if (dbg_cnt (merged_ipa_icf))
	      {
		bool merged = source->merge (alias);
		merged_p |= merged;

		if (merged && alias->type == VAR)
		  {
		    symtab_pair p = symtab_pair (source->node, alias->node);
		    m_merged_variables.safe_push (p);
		  }
	      }
	  }
      }

  if (!m_merged_variables.is_empty ())
    fixup_points_to_sets ();

  return merged_p;
}

/* Fixup points to set PT.  */

void
sem_item_optimizer::fixup_pt_set (struct pt_solution *pt)
{
  if (pt->vars == NULL)
    return;

  unsigned i;
  symtab_pair *item;
  FOR_EACH_VEC_ELT (m_merged_variables, i, item)
    if (bitmap_bit_p (pt->vars, DECL_UID (item->second->decl)))
      bitmap_set_bit (pt->vars, DECL_UID (item->first->decl));
}

/* Set all points-to UIDs of aliases pointing to node N as UID.  */

static void
set_alias_uids (symtab_node *n, int uid)
{
  ipa_ref *ref;
  FOR_EACH_ALIAS (n, ref)
    {
      if (dump_file)
	fprintf (dump_file, "  Setting points-to UID of [%s] as %d\n",
		 ref->referring->dump_asm_name (), uid);

      SET_DECL_PT_UID (ref->referring->decl, uid);
      set_alias_uids (ref->referring, uid);
    }
}

/* Fixup points to analysis info.  */

void
sem_item_optimizer::fixup_points_to_sets (void)
{
  /* TODO: remove in GCC 9 and trigger PTA re-creation after IPA passes.  */
  cgraph_node *cnode;

  FOR_EACH_DEFINED_FUNCTION (cnode)
    {
      tree name;
      unsigned i;
      function *fn = DECL_STRUCT_FUNCTION (cnode->decl);
      if (!gimple_in_ssa_p (fn))
	continue;

      FOR_EACH_SSA_NAME (i, name, fn)
	if (POINTER_TYPE_P (TREE_TYPE (name))
	    && SSA_NAME_PTR_INFO (name))
	  fixup_pt_set (&SSA_NAME_PTR_INFO (name)->pt);
      fixup_pt_set (&fn->gimple_df->escaped);

       /* The above gets us to 99% I guess, at least catching the
	  address compares.  Below also gets us aliasing correct
	  but as said we're giving leeway to the situation with
	  readonly vars anyway, so ... */
       basic_block bb;
       FOR_EACH_BB_FN (bb, fn)
	for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  {
	    gcall *call = dyn_cast<gcall *> (gsi_stmt (gsi));
	    if (call)
	      {
		fixup_pt_set (gimple_call_use_set (call));
		fixup_pt_set (gimple_call_clobber_set (call));
	      }
	  }
    }

  unsigned i;
  symtab_pair *item;
  FOR_EACH_VEC_ELT (m_merged_variables, i, item)
    set_alias_uids (item->first, DECL_UID (item->first->decl));
}

/* Dump function prints all class members to a FILE with an INDENT.  */

void
congruence_class::dump (FILE *file, unsigned int indent) const
{
  FPRINTF_SPACES (file, indent, "class with id: %u, hash: %u, items: %u\n",
		  id, members[0]->get_hash (), members.length ());

  FPUTS_SPACES (file, indent + 2, "");
  for (unsigned i = 0; i < members.length (); i++)
    fprintf (file, "%s ", members[i]->node->dump_asm_name ());

  fprintf (file, "\n");
}

/* Returns true if there's a member that is used from another group.  */

bool
congruence_class::is_class_used (void)
{
  for (unsigned int i = 0; i < members.length (); i++)
    if (members[i]->referenced_by_count)
      return true;

  return false;
}

/* Generate pass summary for IPA ICF pass.  */

static void
ipa_icf_generate_summary (void)
{
  if (!optimizer)
    optimizer = new sem_item_optimizer ();

  optimizer->register_hooks ();
  optimizer->parse_funcs_and_vars ();
}

/* Write pass summary for IPA ICF pass.  */

static void
ipa_icf_write_summary (void)
{
  gcc_assert (optimizer);

  optimizer->write_summary ();
}

/* Read pass summary for IPA ICF pass.  */

static void
ipa_icf_read_summary (void)
{
  if (!optimizer)
    optimizer = new sem_item_optimizer ();

  optimizer->read_summary ();
  optimizer->register_hooks ();
}

/* Semantic equality execution function.  */

static unsigned int
ipa_icf_driver (void)
{
  gcc_assert (optimizer);

  bool merged_p = optimizer->execute ();

  delete optimizer;
  optimizer = NULL;

  return merged_p ? TODO_remove_functions : 0;
}

const pass_data pass_data_ipa_icf =
{
  IPA_PASS,		    /* type */
  "icf",		    /* name */
  OPTGROUP_IPA,             /* optinfo_flags */
  TV_IPA_ICF,		    /* tv_id */
  0,                        /* properties_required */
  0,                        /* properties_provided */
  0,                        /* properties_destroyed */
  0,                        /* todo_flags_start */
  0,                        /* todo_flags_finish */
};

class pass_ipa_icf : public ipa_opt_pass_d
{
public:
  pass_ipa_icf (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_icf, ctxt,
		      ipa_icf_generate_summary, /* generate_summary */
		      ipa_icf_write_summary, /* write_summary */
		      ipa_icf_read_summary, /* read_summary */
		      NULL, /*
		      write_optimization_summary */
		      NULL, /*
		      read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return in_lto_p || flag_ipa_icf_variables || flag_ipa_icf_functions;
  }

  virtual unsigned int execute (function *)
  {
    return ipa_icf_driver();
  }
}; // class pass_ipa_icf

} // ipa_icf namespace

ipa_opt_pass_d *
make_pass_ipa_icf (gcc::context *ctxt)
{
  return new ipa_icf::pass_ipa_icf (ctxt);
}
