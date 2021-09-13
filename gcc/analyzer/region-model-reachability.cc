/* Finding reachable regions and values.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "options.h"
#include "cgraph.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "convert.h"
#include "target.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "json.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/region-model-reachability.h"

#if ENABLE_ANALYZER

namespace ana {

reachable_regions::reachable_regions (region_model *model)
: m_model (model), m_store (model->get_store ()),
  m_reachable_base_regs (), m_mutable_base_regs ()
{
}

/* Callback called for each cluster when initializing this object.  */

void
reachable_regions::init_cluster_cb (const region *base_reg,
				    reachable_regions *this_ptr)
{
  this_ptr->init_cluster (base_reg);
}

/* Called for each cluster when initializing this object.  */
void
reachable_regions::init_cluster (const region *base_reg)
{
  /* Mark any globals as mutable (and traverse what they point to).  */
  const region *parent = base_reg->get_parent_region ();
  gcc_assert (parent);
  if (parent->get_kind () == RK_GLOBALS)
    add (base_reg, true);

  /* Mark any clusters that already escaped in previous unknown calls
     as mutable (and traverse what they currently point to).  */
  if (m_store->escaped_p (base_reg))
    add (base_reg, true);

  if (const symbolic_region *sym_reg = base_reg->dyn_cast_symbolic_region ())
    {
      const svalue *ptr = sym_reg->get_pointer ();
      if (ptr->implicitly_live_p (NULL, m_model))
	add (base_reg, true);
      switch (ptr->get_kind ())
	{
	default:
	  break;
	case SK_INITIAL:
	  {
	    /* If BASE_REG is *INIT_VAL(REG) for some other REG, see if REG is
	       unbound and untouched.  If so, then add BASE_REG as a root.  */
	    const initial_svalue *init_sval
	      = as_a <const initial_svalue *> (ptr);
	    const region *init_sval_reg = init_sval->get_region ();
	    const region *other_base_reg = init_sval_reg->get_base_region ();
	    const binding_cluster *other_cluster
	      = m_store->get_cluster (other_base_reg);
	    if (other_cluster == NULL
		|| !other_cluster->touched_p ())
	      add (base_reg, true);
	  }
	  break;

	case SK_UNKNOWN:
	case SK_CONJURED:
	  {
	    /* If this cluster is due to dereferencing an unknown/conjured
	       pointer, any values written through the pointer could still
	       be live.  */
	    add (base_reg, true);
	  }
	  break;
	}
    }
}

  /* Lazily mark the cluster containing REG as being reachable, recursively
     adding clusters reachable from REG's cluster.  */
void
reachable_regions::add (const region *reg, bool is_mutable)
{
  gcc_assert (reg);

  const region *base_reg = const_cast <region *> (reg->get_base_region ());
  gcc_assert (base_reg);

  /* Bail out if this cluster is already in the sets at the IS_MUTABLE
     level of mutability.  */
  if (!is_mutable && m_reachable_base_regs.contains (base_reg))
    return;
  m_reachable_base_regs.add (base_reg);

  if (is_mutable)
    {
      if (m_mutable_base_regs.contains (base_reg))
	return;
      else
	m_mutable_base_regs.add (base_reg);
    }

  /* Add values within the cluster.  If any are pointers, add the pointee.  */
  if (binding_cluster *bind_cluster = m_store->get_cluster (base_reg))
    bind_cluster->for_each_value (handle_sval_cb, this);
  else
    handle_sval (m_model->get_store_value (reg, NULL));
}

void
reachable_regions::handle_sval_cb (const svalue *sval,
				   reachable_regions *this_ptr)
{
  this_ptr->handle_sval (sval);
}

/* Add SVAL.  If it is a pointer, add the pointed-to region.  */

void
reachable_regions::handle_sval (const svalue *sval)
{
  m_reachable_svals.add (sval);
  m_mutable_svals.add (sval);
  if (const region_svalue *ptr = sval->dyn_cast_region_svalue ())
    {
      const region *pointee = ptr->get_pointee ();
      /* Use const-ness of pointer type to affect mutability.  */
      bool ptr_is_mutable = true;
      if (ptr->get_type ()
	  && TREE_CODE (ptr->get_type ()) == POINTER_TYPE
	  && TYPE_READONLY (TREE_TYPE (ptr->get_type ())))
	{
	  ptr_is_mutable = false;
	}
      else
	{
	  m_mutable_svals.add (sval);
	}
      add (pointee, ptr_is_mutable);
    }
  /* Treat all svalues within a compound_svalue as reachable.  */
  if (const compound_svalue *compound_sval
      = sval->dyn_cast_compound_svalue ())
    {
      for (compound_svalue::iterator_t iter = compound_sval->begin ();
	   iter != compound_sval->end (); ++iter)
	{
	  const svalue *iter_sval = (*iter).second;
	  handle_sval (iter_sval);
	}
    }
  if (const svalue *cast = sval->maybe_undo_cast ())
    handle_sval (cast);

  /* If SVAL is the result of a reversible operation, then the operands
     are reachable.  */
  switch (sval->get_kind ())
    {
    default:
      break;
    case SK_UNARYOP:
      {
	const unaryop_svalue *unaryop_sval = (const unaryop_svalue *)sval;
	switch (unaryop_sval->get_op ())
	  {
	  default:
	    break;
	  case NEGATE_EXPR:
	    handle_sval (unaryop_sval->get_arg ());
	    break;
	  }
      }
      break;
    case SK_BINOP:
      {
	const binop_svalue *binop_sval = (const binop_svalue *)sval;
	switch (binop_sval->get_op ())
	  {
	  default:
	    break;
	  case POINTER_PLUS_EXPR:
	    handle_sval (binop_sval->get_arg0 ());
	    handle_sval (binop_sval->get_arg1 ());
	    break;
	  }
      }
    }
}

/* Add SVAL.  If it is a pointer, add the pointed-to region.
   Use PARAM_TYPE for determining mutability.  */

void
reachable_regions::handle_parm (const svalue *sval, tree param_type)
{
  bool is_mutable = true;
  if (param_type
      && TREE_CODE (param_type) == POINTER_TYPE
      &&  TYPE_READONLY (TREE_TYPE (param_type)))
    is_mutable = false;
  if (is_mutable)
    m_mutable_svals.add (sval);
  else
    m_reachable_svals.add (sval);
  if (const region_svalue *parm_ptr
      = sval->dyn_cast_region_svalue ())
    {
      const region *pointee_reg = parm_ptr->get_pointee ();
      add (pointee_reg, is_mutable);
    }
}

/* Update the store to mark the clusters that were found to be mutable
   as having escaped.
   Notify CTXT about escaping function_decls.  */

void
reachable_regions::mark_escaped_clusters (region_model_context *ctxt)
{
  auto_vec<const function_region *> escaped_fn_regs
    (m_mutable_base_regs.elements ());
  for (hash_set<const region *>::iterator iter = m_mutable_base_regs.begin ();
       iter != m_mutable_base_regs.end (); ++iter)
    {
      const region *base_reg = *iter;
      m_store->mark_as_escaped (base_reg);

      /* If we have a function that's escaped, potentially add
	 it to the worklist.  */
      if (const function_region *fn_reg = base_reg->dyn_cast_function_region ())
	escaped_fn_regs.quick_push (fn_reg);
    }
  if (ctxt)
    {
      /* Sort to ensure deterministic results.  */
      escaped_fn_regs.qsort (region::cmp_ptr_ptr);
      unsigned i;
      const function_region *fn_reg;
      FOR_EACH_VEC_ELT (escaped_fn_regs, i, fn_reg)
	ctxt->on_escaped_function (fn_reg->get_fndecl ());
    }
}

/* Dump SET to PP, sorting it to avoid churn when comparing dumps.  */

template <typename T>
static void
dump_set (const hash_set<const T *> &set, pretty_printer *pp)
{
  auto_vec<const T *> elements (set.elements ());
  for (typename hash_set<const T *>::iterator iter = set.begin ();
       iter != set.end (); ++iter)
    elements.quick_push (*iter);

  elements.qsort (T::cmp_ptr_ptr);

  unsigned i;
  const T *element;
  FOR_EACH_VEC_ELT (elements, i, element)
    {
      pp_string (pp, "  ");
      element->dump_to_pp (pp, true);
      pp_newline (pp);
    }
}

/* Dump a multiline representation of this object to PP.  */

void
reachable_regions::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "reachable clusters: ");
  pp_newline (pp);
  dump_set (m_reachable_base_regs, pp);

  pp_string (pp, "mutable clusters: ");
  pp_newline (pp);
  dump_set (m_mutable_base_regs, pp);

  pp_string (pp, "reachable svals: ");
  pp_newline (pp);
  dump_set (m_reachable_svals, pp);

  pp_string (pp, "mutable svals: ");
  pp_newline (pp);
  dump_set (m_mutable_svals, pp);
}

/* Dump a multiline representation of this object to stderr.  */

DEBUG_FUNCTION void
reachable_regions::dump () const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
