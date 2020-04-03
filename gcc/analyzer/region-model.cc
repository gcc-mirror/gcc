/* Classes for modeling the state of memory.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
#include "diagnostic-color.h"
#include "diagnostic-metadata.h"
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
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "diagnostic-event-id.h"
#include "analyzer/sm.h"
#include "diagnostic-event-id.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/analyzer-selftests.h"
#include "stor-layout.h"

#if ENABLE_ANALYZER

namespace ana {

/* Dump T to PP in language-independent form, for debugging/logging/dumping
   purposes.  */

static void
dump_tree (pretty_printer *pp, tree t)
{
  dump_generic_node (pp, t, 0, TDF_SLIM, 0);
}

/* Dump T to PP in language-independent form in quotes, for
   debugging/logging/dumping purposes.  */

void
dump_quoted_tree (pretty_printer *pp, tree t)
{
  pp_begin_quote (pp, pp_show_color (pp));
  dump_tree (pp, t);
  pp_end_quote (pp, pp_show_color (pp));
}

/* Equivalent to pp_printf (pp, "%qT", t), to avoid nesting pp_printf
   calls within other pp_printf calls.

   default_tree_printer handles 'T' and some other codes by calling
     dump_generic_node (pp, t, 0, TDF_SLIM, 0);
   dump_generic_node calls pp_printf in various places, leading to
   garbled output.

   Ideally pp_printf could be made to be reentrant, but in the meantime
   this function provides a workaround.  */

static void
print_quoted_type (pretty_printer *pp, tree t)
{
  pp_begin_quote (pp, pp_show_color (pp));
  dump_generic_node (pp, t, 0, TDF_SLIM, 0);
  pp_end_quote (pp, pp_show_color (pp));
}

/* Dump this path_var to PP (which must support %E for trees).

   Express the stack depth using an "@DEPTH" suffix, so e.g. given
     void foo (int j);
     void bar (int i)
     {
       foo (i);
     }
   then:
   - the "i" in "bar" would be "(i @ 0)"
   - the "j" in "foo" would be "(j @ 1)".  */

void
path_var::dump (pretty_printer *pp) const
{
  if (m_tree == NULL_TREE)
    pp_string (pp, "NULL");
  if (CONSTANT_CLASS_P (m_tree))
    pp_printf (pp, "%qE", m_tree);
  else
    pp_printf (pp, "(%qE @ %i)", m_tree, m_stack_depth);
}

/* For use in printing a comma-separated list.  */

static void
dump_separator (pretty_printer *pp, bool *is_first)
{
  if (!*is_first)
    pp_string (pp, ", ");
  *is_first = false;
}

/* Concrete subclass of constraint_manager that wires it up to a region_model
   (whilst allowing the constraint_manager and region_model to be somewhat
   at arms length).
   TODO: revisit this; maybe put the region_model * into the constraint_manager
   base class.  */

class impl_constraint_manager : public constraint_manager
{
 public:
  impl_constraint_manager (region_model *model)
  : constraint_manager (),
    m_model (model)
  {}

  impl_constraint_manager (const impl_constraint_manager &other,
			   region_model *model)
  : constraint_manager (other),
    m_model (model)
  {}

  constraint_manager *clone (region_model *model) const
  {
    return new impl_constraint_manager (*this, model);
  }

  tree maybe_get_constant (svalue_id sid) const FINAL OVERRIDE
  {
    svalue *svalue = m_model->get_svalue (sid);
    return svalue->maybe_get_constant ();
  }

  svalue_id get_sid_for_constant (tree cst) const FINAL OVERRIDE
  {
    gcc_assert (CONSTANT_CLASS_P (cst));
    return m_model->get_rvalue (cst, NULL);
  }

  int get_num_svalues () const FINAL OVERRIDE
  {
    return m_model->get_num_svalues ();
  }

 private:
  region_model *m_model;
};

/* class svalue_id.  */

/* Print this svalue_id to PP.  */

void
svalue_id::print (pretty_printer *pp) const
{
  if (null_p ())
    pp_printf (pp, "null");
  else
    pp_printf (pp, "sv%i", m_idx);
}

/* Print this svalue_id in .dot format to PP.  */

void
svalue_id::dump_node_name_to_pp (pretty_printer *pp) const
{
  gcc_assert (!null_p ());
  pp_printf (pp, "svalue_%i", m_idx);
}

/* Assert that this object is valid (w.r.t. MODEL).  */

void
svalue_id::validate (const region_model &model) const
{
  gcc_assert (null_p () || m_idx < (int)model.get_num_svalues ());
}

/* class region_id.  */

/* Print this region_id to PP.  */

void
region_id::print (pretty_printer *pp) const
{
  if (null_p ())
    pp_printf (pp, "null");
  else
    pp_printf (pp, "r%i", m_idx);
}

/* Print this region_id in .dot format to PP.  */

void
region_id::dump_node_name_to_pp (pretty_printer *pp) const
{
  gcc_assert (!null_p ());
  pp_printf (pp, "region_%i", m_idx);
}

/* Assert that this object is valid (w.r.t. MODEL).  */

void
region_id::validate (const region_model &model) const
{
  gcc_assert (null_p () || m_idx < (int)model.get_num_regions ());
}

/* class region_id_set.  */

region_id_set::region_id_set (const region_model *model)
: m_bitmap (model->get_num_regions ())
{
  bitmap_clear (m_bitmap);
}

/* class svalue_id_set.  */

svalue_id_set::svalue_id_set ()
: m_bitmap (NULL)
{
  bitmap_clear (m_bitmap);
}

/* class svalue and its various subclasses.  */

/* class svalue.  */

/* svalue's equality operator.  Most of the work is done by the
   a "compare_fields" implementation on each subclass.  */

bool
svalue::operator== (const svalue &other) const
{
  enum svalue_kind this_kind = get_kind ();
  enum svalue_kind other_kind = other.get_kind ();
  if (this_kind != other_kind)
    return false;

  if (m_type != other.m_type)
    return false;

  switch (this_kind)
    {
    default:
      gcc_unreachable ();
    case SK_REGION:
      {
	const region_svalue &this_sub
	  = (const region_svalue &)*this;
	const region_svalue &other_sub
	  = (const region_svalue &)other;
	return this_sub.compare_fields (other_sub);
      }
      break;
    case SK_CONSTANT:
      {
	const constant_svalue &this_sub
	  = (const constant_svalue &)*this;
	const constant_svalue &other_sub
	  = (const constant_svalue &)other;
	return this_sub.compare_fields (other_sub);
      }
      break;
    case SK_UNKNOWN:
      {
	const unknown_svalue &this_sub
	  = (const unknown_svalue &)*this;
	const unknown_svalue &other_sub
	  = (const unknown_svalue &)other;
	return this_sub.compare_fields (other_sub);
      }
      break;
    case SK_POISONED:
      {
	const poisoned_svalue &this_sub
	  = (const poisoned_svalue &)*this;
	const poisoned_svalue &other_sub
	  = (const poisoned_svalue &)other;
	return this_sub.compare_fields (other_sub);
      }
      break;
    case SK_SETJMP:
      {
	const setjmp_svalue &this_sub
	  = (const setjmp_svalue &)*this;
	const setjmp_svalue &other_sub
	  = (const setjmp_svalue &)other;
	return this_sub.compare_fields (other_sub);
      }
      break;
    }
}

/* Generate a hash value for this svalue.  Most of the work is done by the
   add_to_hash vfunc.  */

hashval_t
svalue::hash () const
{
  inchash::hash hstate;
  if (m_type)
    hstate.add_int (TYPE_UID (m_type));
  add_to_hash (hstate);
  return hstate.end ();
}

/* Print this svalue and its ID to PP.  */

void
svalue::print (const region_model &model,
	       svalue_id this_sid,
	       pretty_printer *pp) const
{
  this_sid.print (pp);
  pp_string (pp, ": {");

  if (m_type)
    {
      gcc_assert (TYPE_P (m_type));
      pp_string (pp, "type: ");
      print_quoted_type (pp, m_type);
      pp_string (pp, ", ");
    }

  /* vfunc.  */
  print_details (model, this_sid, pp);

  pp_string (pp, "}");
}

/* Dump this svalue in the form of a .dot record to PP.  */

void
svalue::dump_dot_to_pp (const region_model &model,
			svalue_id this_sid,
			pretty_printer *pp) const
{
  this_sid.dump_node_name_to_pp (pp);
  pp_printf (pp, " [label=\"");
  pp_write_text_to_stream (pp);
  this_sid.print (pp);
  pp_string (pp, ": {");
  print (model, this_sid, pp);
  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);
  pp_string (pp, "}\"];");
  pp_newline (pp);
}

/* Base implementation of svalue::remap_region_ids vfunc.  */

void
svalue::remap_region_ids (const region_id_map &)
{
  /* Empty.  */
}

/* Base implementation of svalue::walk_for_canonicalization vfunc.  */

void
svalue::walk_for_canonicalization (canonicalization *) const
{
  /* Empty.  */
}

/* Base implementation of svalue::get_child_sid vfunc.  */

svalue_id
svalue::get_child_sid (region *parent ATTRIBUTE_UNUSED,
		       region *child,
		       region_model &model,
		       region_model_context *ctxt ATTRIBUTE_UNUSED)
{
  svalue *new_child_value = clone ();
  if (child->get_type ())
    new_child_value->m_type = child->get_type ();
  svalue_id new_child_sid = model.add_svalue (new_child_value);
  return new_child_sid;
}

/* If this svalue is a constant_svalue, return the underlying tree constant.
   Otherwise return NULL_TREE.  */

tree
svalue::maybe_get_constant () const
{
  if (const constant_svalue *cst_sval = dyn_cast_constant_svalue ())
    return cst_sval->get_constant ();
  else
    return NULL_TREE;
}

/* class region_svalue : public svalue.  */

/* Compare the fields of this region_svalue with OTHER, returning true
   if they are equal.
   For use by svalue::operator==.  */

bool
region_svalue::compare_fields (const region_svalue &other) const
{
  return m_rid == other.m_rid;
}

/* Implementation of svalue::add_to_hash vfunc for region_svalue.  */

void
region_svalue::add_to_hash (inchash::hash &hstate) const
{
  inchash::add (m_rid, hstate);
}

/* Implementation of svalue::print_details vfunc for region_svalue.  */

void
region_svalue::print_details (const region_model &model ATTRIBUTE_UNUSED,
			      svalue_id this_sid ATTRIBUTE_UNUSED,
			      pretty_printer *pp) const
{
  if (m_rid.null_p ())
    pp_string (pp, "NULL");
  else
    {
      pp_string (pp, "&");
      m_rid.print (pp);
    }
}

/* Implementation of svalue::dump_dot_to_pp for region_svalue.  */

void
region_svalue::dump_dot_to_pp (const region_model &model,
			       svalue_id this_sid,
			       pretty_printer *pp) const
{
  svalue::dump_dot_to_pp (model, this_sid, pp);

  /* If non-NULL, add an edge to the pointed-to region.  */
  if (!m_rid.null_p ())
    {
      this_sid.dump_node_name_to_pp (pp);
      pp_string (pp, " -> ");
      m_rid.dump_node_name_to_pp (pp);
      pp_string (pp, ";");
      pp_newline (pp);
  }
}

/* Implementation of svalue::remap_region_ids vfunc for region_svalue.  */

void
region_svalue::remap_region_ids (const region_id_map &map)
{
  map.update (&m_rid);
}

/* Merge REGION_SVAL_A and REGION_SVAL_B using MERGER, writing the result
   into *MERGED_SID.  */

void
region_svalue::merge_values (const region_svalue &region_sval_a,
			     const region_svalue &region_sval_b,
			     svalue_id *merged_sid,
			     tree type,
			     model_merger *merger)
{
  region_id a_rid = region_sval_a.get_pointee ();
  region_id b_rid = region_sval_b.get_pointee ();

  /* Both are non-NULL. */
  gcc_assert (!a_rid.null_p () && !b_rid.null_p ());

  /* Have these ptr-values already been merged?  */

  region_id a_rid_in_m
    = merger->m_map_regions_from_a_to_m.get_dst_for_src (a_rid);
  region_id b_rid_in_m
    = merger->m_map_regions_from_b_to_m.get_dst_for_src (b_rid);

  /* "null_p" here means "we haven't seen this ptr-value before".
     If we've seen one but not the other, or we have different
     regions, then the merged ptr has to be "unknown".  */
  if (a_rid_in_m != b_rid_in_m)
    {
      svalue *merged_sval = new unknown_svalue (type);
      *merged_sid = merger->m_merged_model->add_svalue (merged_sval);
      return;
    }

  /* Have we seen this yet?  If so, reuse the value.  */
  if (!a_rid_in_m.null_p ())
    {
      *merged_sid
	= merger->m_merged_model->get_or_create_ptr_svalue (type, a_rid_in_m);
      return;
    }

  /* Otherwise we have A/B regions that haven't been referenced yet.  */

  /* Are the regions the "same", when seen from the tree point-of-view.
     If so, create a merged pointer to it.  */
  path_var pv_a = merger->m_model_a->get_representative_path_var (a_rid);
  path_var pv_b = merger->m_model_b->get_representative_path_var (b_rid);
  if (pv_a.m_tree
      && pv_a == pv_b)
    {
      region_id merged_pointee_rid
	= merger->m_merged_model->get_lvalue (pv_a, NULL);
      *merged_sid
	= merger->m_merged_model->get_or_create_ptr_svalue (type,
							    merged_pointee_rid);
      merger->record_regions (a_rid, b_rid, merged_pointee_rid);
      return;
    }

  /* Handle an A/B pair of ptrs that both point at heap regions.
     If they both have a heap region in the merger model, merge them.  */
  region *region_a = merger->m_model_a->get_region (a_rid);
  region *region_b = merger->m_model_b->get_region (b_rid);
  region_id a_parent_rid = region_a->get_parent ();
  region_id b_parent_rid = region_b->get_parent ();
  region *parent_region_a = merger->m_model_a->get_region (a_parent_rid);
  region *parent_region_b = merger->m_model_b->get_region (b_parent_rid);
  if (parent_region_a
      && parent_region_b
      && parent_region_a->get_kind () == RK_HEAP
      && parent_region_b->get_kind () == RK_HEAP)
    {
	/* We have an A/B pair of ptrs that both point at heap regions.  */
	/* presumably we want to see if each A/B heap region already
	   has a merged region, and, if so, is it the same one.  */
	// This check is above

	region_id merged_pointee_rid
	  = merger->m_merged_model->add_new_malloc_region ();
	*merged_sid
	  = merger->m_merged_model->get_or_create_ptr_svalue
	     (type, merged_pointee_rid);
	merger->record_regions (a_rid, b_rid, merged_pointee_rid);
	return;
    }

  /* Two different non-NULL pointers?  Merge to unknown.  */
  svalue *merged_sval = new unknown_svalue (type);
  *merged_sid = merger->m_merged_model->add_svalue (merged_sval);
  return;
}

/* Implementation of svalue::walk_for_canonicalization vfunc for
   region_svalue.  */

void
region_svalue::walk_for_canonicalization (canonicalization *c) const
{
  c->walk_rid (m_rid);
}

/* Evaluate the condition LHS OP RHS.
   Subroutine of region_model::eval_condition for when we have a pair of
   pointers.  */

tristate
region_svalue::eval_condition (region_svalue *lhs,
			       enum tree_code op,
			       region_svalue *rhs)
{
  /* See if they point to the same region.  */
  /* TODO: what about child regions where the child is the first child
     (or descendent)?  */
  region_id lhs_rid = lhs->get_pointee ();
  region_id rhs_rid = rhs->get_pointee ();
  switch (op)
    {
    default:
      gcc_unreachable ();

    case EQ_EXPR:
      if (lhs_rid == rhs_rid)
	return tristate::TS_TRUE;
      else
	return tristate::TS_FALSE;
      break;

    case NE_EXPR:
      if (lhs_rid != rhs_rid)
	return tristate::TS_TRUE;
      else
	return tristate::TS_FALSE;
      break;

    case GE_EXPR:
    case LE_EXPR:
      if (lhs_rid == rhs_rid)
	return tristate::TS_TRUE;
      break;

    case GT_EXPR:
    case LT_EXPR:
      if (lhs_rid == rhs_rid)
	return tristate::TS_FALSE;
      break;
    }

  return tristate::TS_UNKNOWN;
}

/* class constant_svalue : public svalue.  */

/* Compare the fields of this constant_svalue with OTHER, returning true
   if they are equal.
   For use by svalue::operator==.  */

bool
constant_svalue::compare_fields (const constant_svalue &other) const
{
  return m_cst_expr == other.m_cst_expr;
}

/* Implementation of svalue::add_to_hash vfunc for constant_svalue.  */

void
constant_svalue::add_to_hash (inchash::hash &hstate) const
{
  inchash::add_expr (m_cst_expr, hstate);
}

/* Merge the CST_SVAL_A and CST_SVAL_B using MERGER, writing the id of
   the resulting svalue into *MERGED_SID.  */

void
constant_svalue::merge_values (const constant_svalue &cst_sval_a,
			       const constant_svalue &cst_sval_b,
			       svalue_id *merged_sid,
			       model_merger *merger)
{
  tree cst_a = cst_sval_a.get_constant ();
  tree cst_b = cst_sval_b.get_constant ();
  svalue *merged_sval;
  if (cst_a == cst_b)
    {
      /* If they are the same constant, merge as that constant value.  */
      merged_sval = new constant_svalue (cst_a);
    }
  else
    {
      /* Otherwise, we have two different constant values.
	 Merge as an unknown value.
	 TODO: impose constraints on the value?
	 (maybe just based on A, to avoid infinite chains)  */
      merged_sval = new unknown_svalue (TREE_TYPE (cst_a));
    }
  *merged_sid = merger->m_merged_model->add_svalue (merged_sval);
}

/* Evaluate the condition LHS OP RHS.
   Subroutine of region_model::eval_condition for when we have a pair of
   constants.  */

tristate
constant_svalue::eval_condition (constant_svalue *lhs,
				 enum tree_code op,
				 constant_svalue *rhs)
{
  tree lhs_const = lhs->get_constant ();
  tree rhs_const = rhs->get_constant ();

  gcc_assert (CONSTANT_CLASS_P (lhs_const));
  gcc_assert (CONSTANT_CLASS_P (rhs_const));

  /* Check for comparable types.  */
  if (types_compatible_p (TREE_TYPE (lhs_const), TREE_TYPE (rhs_const)))
    {
      tree comparison
	= fold_binary (op, boolean_type_node, lhs_const, rhs_const);
      if (comparison == boolean_true_node)
	return tristate (tristate::TS_TRUE);
      if (comparison == boolean_false_node)
	return tristate (tristate::TS_FALSE);
    }
  return tristate::TS_UNKNOWN;
}

/* Implementation of svalue::print_details vfunc for constant_svalue.  */

void
constant_svalue::print_details (const region_model &model ATTRIBUTE_UNUSED,
				svalue_id this_sid ATTRIBUTE_UNUSED,
				pretty_printer *pp) const
{
  pp_printf (pp, "%qE", m_cst_expr);
}

/* Implementation of svalue::get_child_sid vfunc for constant_svalue.  */

svalue_id
constant_svalue::get_child_sid (region *parent ATTRIBUTE_UNUSED,
				region *child,
				region_model &model,
				region_model_context *ctxt ATTRIBUTE_UNUSED)
{
  /* TODO: handle the all-zeroes case by returning an all-zeroes of the
     child type.  */

  /* Otherwise, we don't have a good way to get a child value out of a
     constant.

     Handle this case by using an unknown value.  */
  svalue *unknown_sval = new unknown_svalue (child->get_type ());
  return model.add_svalue (unknown_sval);
}

/* class unknown_svalue : public svalue.  */

/* Compare the fields of this unknown_svalue with OTHER, returning true
   if they are equal.
   For use by svalue::operator==.  */

bool
unknown_svalue::compare_fields (const unknown_svalue &) const
{
  /* I *think* we want to return true here, in that when comparing
     two region models, we want two peer unknown_svalue instances
     to be the "same".  */
  return true;
}

/* Implementation of svalue::add_to_hash vfunc for unknown_svalue.  */

void
unknown_svalue::add_to_hash (inchash::hash &) const
{
  /* Empty.  */
}

/* Implementation of svalue::print_details vfunc for unknown_svalue.  */

void
unknown_svalue::print_details (const region_model &model ATTRIBUTE_UNUSED,
			       svalue_id this_sid ATTRIBUTE_UNUSED,
			       pretty_printer *pp) const
{
  pp_string (pp, "unknown");
}

/* Get a string for KIND for use in debug dumps.  */

const char *
poison_kind_to_str (enum poison_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case POISON_KIND_UNINIT:
      return "uninit";
    case POISON_KIND_FREED:
      return "freed";
    case POISON_KIND_POPPED_STACK:
      return "popped stack";
    }
}

/* class poisoned_svalue : public svalue.  */

/* Compare the fields of this poisoned_svalue with OTHER, returning true
   if they are equal.
   For use by svalue::operator==.  */

bool
poisoned_svalue::compare_fields (const poisoned_svalue &other) const
{
  return m_kind == other.m_kind;
}

/* Implementation of svalue::add_to_hash vfunc for poisoned_svalue.  */

void
poisoned_svalue::add_to_hash (inchash::hash &hstate) const
{
  hstate.add_int (m_kind);
}

/* Implementation of svalue::print_details vfunc for poisoned_svalue.  */

void
poisoned_svalue::print_details (const region_model &model ATTRIBUTE_UNUSED,
				svalue_id this_sid ATTRIBUTE_UNUSED,
				pretty_printer *pp) const
{
  pp_printf (pp, "poisoned: %s", poison_kind_to_str (m_kind));
}

/* class setjmp_svalue's implementation is in engine.cc, so that it can use
   the declaration of exploded_node.  */

/* class region and its various subclasses.  */

/* Get a string for KIND for use in debug dumps.  */

const char *
region_kind_to_str (enum region_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case RK_PRIMITIVE:
      return "primitive";
    case RK_STRUCT:
      return "struct";
    case RK_UNION:
      return "union";
    case RK_ARRAY:
      return "array";
    case RK_FRAME:
      return "frame";
    case RK_GLOBALS:
      return "globals";
    case RK_CODE:
      return "code";
    case RK_FUNCTION:
      return "function";
    case RK_STACK:
      return "stack";
    case RK_HEAP:
      return "heap";
    case RK_ROOT:
      return "root";
    case RK_SYMBOLIC:
      return "symbolic";
    }
}

/* class region.  */

/* Equality operator for region.
   After comparing base class fields and kind, the rest of the
   comparison is handled off to a "compare_fields" member function
   specific to the appropriate subclass.  */

bool
region::operator== (const region &other) const
{
  if (m_parent_rid != other.m_parent_rid)
    return false;
  if (m_sval_id != other.m_sval_id)
    return false;
  if (m_type != other.m_type)
    return false;

  enum region_kind this_kind = get_kind ();
  enum region_kind other_kind = other.get_kind ();
  if (this_kind != other_kind)
    return false;

  /* Compare views.  */
  if (m_view_rids.length () != other.m_view_rids.length ())
    return false;
  int i;
  region_id *rid;
  FOR_EACH_VEC_ELT (m_view_rids, i, rid)
    if (! (*rid == other.m_view_rids[i]))
      return false;

  switch (this_kind)
    {
    default:
      gcc_unreachable ();
    case RK_PRIMITIVE:
      {
#if 1
	return true;
#else
	const primitive_region &this_sub
	  = (const primitive_region &)*this;
	const primitive_region &other_sub
	  = (const primitive_region &)other;
	return this_sub.compare_fields (other_sub);
#endif
      }
    case RK_STRUCT:
      {
	const struct_region &this_sub
	  = (const struct_region &)*this;
	const struct_region &other_sub
	  = (const struct_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_UNION:
      {
	const union_region &this_sub
	  = (const union_region &)*this;
	const union_region &other_sub
	  = (const union_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_ARRAY:
      {
	const array_region &this_sub
	  = (const array_region &)*this;
	const array_region &other_sub
	  = (const array_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_FRAME:
      {
	const frame_region &this_sub
	  = (const frame_region &)*this;
	const frame_region &other_sub
	  = (const frame_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_GLOBALS:
      {
	const globals_region &this_sub
	  = (const globals_region &)*this;
	const globals_region &other_sub
	  = (const globals_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_CODE:
      {
	const code_region &this_sub
	  = (const code_region &)*this;
	const code_region &other_sub
	  = (const code_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_FUNCTION:
      {
	const function_region &this_sub
	  = (const function_region &)*this;
	const function_region &other_sub
	  = (const function_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_STACK:
      {
	const stack_region &this_sub
	  = (const stack_region &)*this;
	const stack_region &other_sub
	  = (const stack_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_ROOT:
      {
	const root_region &this_sub
	  = (const root_region &)*this;
	const root_region &other_sub
	  = (const root_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_SYMBOLIC:
      {
	const symbolic_region &this_sub
	  = (const symbolic_region &)*this;
	const symbolic_region &other_sub
	  = (const symbolic_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    case RK_HEAP:
      {
	const heap_region &this_sub
	  = (const heap_region &)*this;
	const heap_region &other_sub
	  = (const heap_region &)other;
	return this_sub.compare_fields (other_sub);
      }
    }
}

/* Get the parent region of this region.  */

region *
region::get_parent_region (const region_model &model) const
{
  return model.get_region (m_parent_rid);
}

/* Set this region's value to RHS_SID (or potentially a variant of it,
   for some kinds of casts).  */

void
region::set_value (region_model &model, region_id this_rid, svalue_id rhs_sid,
		   region_model_context *ctxt)
{
  /* Handle some kinds of casting.  */
  if (m_type)
    {
      svalue *sval = model.get_svalue (rhs_sid);
      if (sval->get_type ())
	rhs_sid = model.maybe_cast (m_type, rhs_sid, ctxt);

      sval = model.get_svalue (rhs_sid);
      if (sval->get_type ())
	gcc_assert (m_type == sval->get_type ());
    }

  m_sval_id = rhs_sid;

  /* Update views.
     If this is a view, it becomes its parent's active view.
     If there was already an active views, invalidate its value; otherwise
     if the parent itself had a value, invalidate it.
     If it's not a view, then deactivate any view that is active on this
     region.  */
  {
    if (m_is_view)
      become_active_view (model, this_rid);
    else
      {
	deactivate_any_active_view (model);
	gcc_assert (m_active_view_rid.null_p ());
      }
  }
}

/* Make this region (with id THIS_RID) the "active" view of its parent.
   Any other active view has its value set to "unknown" and descendent values
   cleared.
   If there wasn't an active view, then set the parent's value to unknown, and
   clear its descendent values (apart from this view).  */

void
region::become_active_view (region_model &model, region_id this_rid)
{
  gcc_assert (m_is_view);

  region *parent_reg = model.get_region (m_parent_rid);
  gcc_assert (parent_reg);

  region_id old_active_view_rid = parent_reg->m_active_view_rid;

  if (old_active_view_rid == this_rid)
    {
      /* Already the active view: do nothing.  */
      return;
    }

  /* We have a change of active view.  */
  parent_reg->m_active_view_rid = this_rid;

  if (old_active_view_rid.null_p ())
    {
      /* No previous active view, but the parent and its other children
	 might have values.
	 If so, invalidate those values - but not that of the new view.  */
      region_id_set below_region (&model);
      model.get_descendents (m_parent_rid, &below_region, this_rid);
      for (unsigned i = 0; i < model.get_num_regions (); i++)
	{
	  region_id rid (region_id::from_int (i));
	  if (below_region.region_p (rid))
	    {
	      region *other_reg = model.get_region (rid);
	      other_reg->m_sval_id = svalue_id::null ();
	    }
	}
      region *parent = model.get_region (m_parent_rid);
      parent->m_sval_id
	= model.add_svalue (new unknown_svalue (parent->get_type ()));
    }
  else
    {
      /* If there was an active view, invalidate it.  */
      region *old_active_view = model.get_region (old_active_view_rid);
      old_active_view->deactivate_view (model, old_active_view_rid);
    }
}

/* If this region (with id THIS_RID) has an active view, deactivate it,
   clearing m_active_view_rid.  */

void
region::deactivate_any_active_view (region_model &model)
{
  if (m_active_view_rid.null_p ())
    return;
  region *view = model.get_region (m_active_view_rid);
  view->deactivate_view (model, m_active_view_rid);
  m_active_view_rid = region_id::null ();
}

/* Clear any values for regions below THIS_RID.
   Set the view's value to unknown.  */

void
region::deactivate_view (region_model &model, region_id this_view_rid)
{
  gcc_assert (is_view_p ());

  /* Purge values from old_active_this_view_rid and all its
     descendents.  Potentially we could use a poison value
     for this, but let's use unknown for now.  */
  region_id_set below_view (&model);
  model.get_descendents (this_view_rid, &below_view, region_id::null ());

  for (unsigned i = 0; i < model.get_num_regions (); i++)
    {
      region_id rid (region_id::from_int (i));
      if (below_view.region_p (rid))
	{
	  region *other_reg = model.get_region (rid);
	  other_reg->m_sval_id = svalue_id::null ();
	}
    }

  m_sval_id = model.add_svalue (new unknown_svalue (get_type ()));
}

/* Get a value for this region, either its value if it has one,
   or, failing that, "inherit" a value from first ancestor with a
   non-null value.

   For example, when getting the value for a local variable within
   a stack frame that doesn't have one, the frame doesn't have a value
   either, but the stack as a whole will have an "uninitialized" poison
   value, so inherit that.  */

svalue_id
region::get_value (region_model &model, bool non_null,
		   region_model_context *ctxt)
{
  /* If this region has a value, use it. */
  if (!m_sval_id.null_p ())
    return m_sval_id;

  /* Otherwise, "inherit" value from first ancestor with a
     non-null value. */

  region *parent = model.get_region (m_parent_rid);
  if (parent)
    {
      svalue_id inherited_sid
	= parent->get_inherited_child_sid (this, model, ctxt);
      if (!inherited_sid.null_p ())
	return inherited_sid;
    }

  /* If a non-null value has been requested, then generate
     a new unknown value.  Store it, so that repeated reads from this
     region will yield the same unknown value.  */
  if (non_null)
    {
      svalue_id unknown_sid = model.add_svalue (new unknown_svalue (m_type));
      m_sval_id = unknown_sid;
      return unknown_sid;
    }

  return svalue_id::null ();
}

/* Get a value for CHILD, inheriting from this region.

   Recurse, so this region will inherit a value if it doesn't already
   have one.  */

svalue_id
region::get_inherited_child_sid (region *child,
				 region_model &model,
				 region_model_context *ctxt)
{
  if (m_sval_id.null_p ())
    {
      /* Recurse.  */
      if (!m_parent_rid.null_p ())
	{
	  region *parent = model.get_region (m_parent_rid);
	  m_sval_id = parent->get_inherited_child_sid (this, model, ctxt);
	}
    }

  if (!m_sval_id.null_p ())
    {
      /* Clone the parent's value, so that attempts to update it
	 (e.g giving a specific value to an inherited "uninitialized"
	 value) touch the child, and not the parent.  */
      svalue *this_value = model.get_svalue (m_sval_id);
      svalue_id new_child_sid
	= this_value->get_child_sid (this, child, model, ctxt);
      if (ctxt)
	ctxt->on_inherited_svalue (m_sval_id, new_child_sid);
      child->m_sval_id = new_child_sid;
      return new_child_sid;
    }

  return svalue_id::null ();
}

/* Copy from SRC_RID to DST_RID, using CTXT for any issues that occur.
   Copy across any value for the region, and handle structs, unions
   and arrays recursively.  */

void
region_model::copy_region (region_id dst_rid, region_id src_rid,
			   region_model_context *ctxt)
{
  gcc_assert (!dst_rid.null_p ());
  gcc_assert (!src_rid.null_p ());
  if (dst_rid == src_rid)
    return;
  region *dst_reg = get_region (dst_rid);
  region *src_reg = get_region (src_rid);

  /* Copy across any value for the src region itself.  */
  svalue_id sid = src_reg->get_value (*this, true, ctxt);
  set_value (dst_rid, sid, ctxt);

  if (dst_reg->get_kind () != src_reg->get_kind ())
    return;

  /* Copy across child regions for structs, unions, and arrays.  */
  switch (dst_reg->get_kind ())
    {
    case RK_PRIMITIVE:
      return;
    case RK_STRUCT:
      {
	struct_region *dst_sub = as_a <struct_region *> (dst_reg);
	struct_region *src_sub = as_a <struct_region *> (src_reg);
	copy_struct_region (dst_rid, dst_sub, src_sub, ctxt);
      }
      return;
    case RK_UNION:
      {
	union_region *src_sub = as_a <union_region *> (src_reg);
	copy_union_region (dst_rid, src_sub, ctxt);
      }
      return;
    case RK_FRAME:
    case RK_GLOBALS:
    case RK_CODE:
    case RK_FUNCTION:
      return;
    case RK_ARRAY:
      {
	array_region *dst_sub = as_a <array_region *> (dst_reg);
	array_region *src_sub = as_a <array_region *> (src_reg);
	copy_array_region (dst_rid, dst_sub, src_sub, ctxt);
      }
      return;
    case RK_STACK:
    case RK_HEAP:
    case RK_ROOT:
    case RK_SYMBOLIC:
      return;
    }
}

/* Subroutine of region_model::copy_region for copying the child
   regions for a struct.  */

void
region_model::copy_struct_region (region_id dst_rid,
				  struct_region *dst_reg,
				  struct_region *src_reg,
				  region_model_context *ctxt)
{
  for (map_region::iterator_t iter = src_reg->begin ();
       iter != src_reg->end (); ++iter)
    {
      tree src_key = (*iter).first;
      region_id src_field_rid = (*iter).second;
      region *src_field_reg = get_region (src_field_rid);
      region_id dst_field_rid
	= dst_reg->get_or_create (this, dst_rid, src_key,
				  src_field_reg->get_type (), ctxt);
      copy_region (dst_field_rid, src_field_rid, ctxt);
    }
}

/* Subroutine of region_model::copy_region for copying the active
   child region for a union.  */

void
region_model::copy_union_region (region_id dst_rid,
				 union_region *src_reg,
				 region_model_context *ctxt)
{
  region_id src_active_view_rid = src_reg->get_active_view ();
  if (src_active_view_rid.null_p ())
    return;
  region *src_active_view = get_region (src_active_view_rid);
  tree type = src_active_view->get_type ();
  region_id dst_active_view_rid = get_or_create_view (dst_rid, type, ctxt);
  copy_region (dst_active_view_rid, src_active_view_rid, ctxt);
}

/* Subroutine of region_model::copy_region for copying the child
   regions for an array.  */

void
region_model::copy_array_region (region_id dst_rid,
				 array_region *dst_reg,
				 array_region *src_reg,
				 region_model_context *ctxt)
{
  for (array_region::iterator_t iter = src_reg->begin ();
       iter != src_reg->end (); ++iter)
    {
      array_region::key_t src_key = (*iter).first;
      region_id src_field_rid = (*iter).second;
      region *src_field_reg = get_region (src_field_rid);
      region_id dst_field_rid
	= dst_reg->get_or_create (this, dst_rid, src_key,
				  src_field_reg->get_type (), ctxt);
      copy_region (dst_field_rid, src_field_rid, ctxt);
    }
}

/* Generate a hash value for this region.  The work is done by the
   add_to_hash vfunc.  */

hashval_t
region::hash () const
{
  inchash::hash hstate;
  add_to_hash (hstate);
  return hstate.end ();
}

/* Print a one-liner representation of this region to PP, assuming
   that this region is within MODEL and its id is THIS_RID.  */

void
region::print (const region_model &model,
	       region_id this_rid,
	       pretty_printer *pp) const
{
  this_rid.print (pp);
  pp_string (pp, ": {");

  /* vfunc.  */
  print_fields (model, this_rid, pp);

  pp_string (pp, "}");
}

/* Base class implementation of region::dump_dot_to_pp vfunc.  */

void
region::dump_dot_to_pp (const region_model &model,
			region_id this_rid,
			pretty_printer *pp) const
{
  this_rid.dump_node_name_to_pp (pp);
  pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=\"",
	     "lightgrey");
  pp_write_text_to_stream (pp);
  print (model, this_rid, pp);
  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);
  pp_string (pp, "\"];");
  pp_newline (pp);

  /* Add edge to svalue.  */
  if (!m_sval_id.null_p ())
    {
      this_rid.dump_node_name_to_pp (pp);
      pp_string (pp, " -> ");
      m_sval_id.dump_node_name_to_pp (pp);
      pp_string (pp, ";");
      pp_newline (pp);
    }

  /* Add edge to parent.  */
  if (!m_parent_rid.null_p ())
    {
      this_rid.dump_node_name_to_pp (pp);
      pp_string (pp, " -> ");
      m_parent_rid.dump_node_name_to_pp (pp);
      pp_string (pp, ";");
      pp_newline (pp);
    }
}

/* Dump a tree-like ASCII-art representation of this region to PP.  */

void
region::dump_to_pp (const region_model &model,
		    region_id this_rid,
		    pretty_printer *pp,
		    const char *prefix,
		    bool is_last_child) const
{
  print (model, this_rid, pp);
  pp_newline (pp);

  const char *new_prefix;
  if (!m_parent_rid.null_p ())
    new_prefix = ACONCAT ((prefix, is_last_child ? "  " : "|  ", NULL));
  else
    new_prefix = prefix;

  const char *begin_color = colorize_start (pp_show_color (pp), "note");
  const char *end_color = colorize_stop (pp_show_color (pp));
  char *field_prefix
    = ACONCAT ((begin_color, new_prefix, "|:", end_color, NULL));

  if (!m_sval_id.null_p ())
    {
      pp_printf (pp, "%s sval: ", field_prefix);
      model.get_svalue (m_sval_id)->print (model, m_sval_id, pp);
      pp_newline (pp);
    }
  if (m_type)
    {
      pp_printf (pp, "%s type: ", field_prefix);
      print_quoted_type (pp, m_type);
      pp_newline (pp);
    }

  /* Find the children.  */

  auto_vec<region_id> child_rids;
  unsigned i;
  for (unsigned i = 0; i < model.get_num_regions (); ++i)
    {
      region_id rid = region_id::from_int (i);
      region *child = model.get_region (rid);
      if (child->m_parent_rid == this_rid)
	child_rids.safe_push (rid);
    }

  /* Print the children, using dump_child_label to label them.  */

  region_id *child_rid;
  FOR_EACH_VEC_ELT (child_rids, i, child_rid)
    {
      is_last_child = (i == child_rids.length () - 1);
      if (!this_rid.null_p ())
	{
	  const char *tail = is_last_child ? "`-" : "|-";
	  pp_printf (pp, "%r%s%s%R", "note", new_prefix, tail);
	}
      dump_child_label (model, this_rid, *child_rid, pp);
      model.get_region (*child_rid)->dump_to_pp (model, *child_rid, pp,
						 new_prefix,
						 is_last_child);
    }
}

/* Base implementation of region::dump_child_label vfunc.  */

void
region::dump_child_label (const region_model &model,
			  region_id this_rid ATTRIBUTE_UNUSED,
			  region_id child_rid,
			  pretty_printer *pp) const
{
  region *child = model.get_region (child_rid);
  if (child->m_is_view)
    {
      gcc_assert (TYPE_P (child->get_type ()));
      if (m_active_view_rid == child_rid)
	pp_string (pp, "active ");
      else
	pp_string (pp, "inactive ");
      pp_string (pp, "view as ");
      print_quoted_type (pp, child->get_type ());
      pp_string (pp, ": ");
    }
}

/* Base implementation of region::validate vfunc.
   Assert that the fields of "region" are valid; subclasses should
   chain up their implementation to this one.  */

void
region::validate (const region_model &model) const
{
  m_parent_rid.validate (model);
  m_sval_id.validate (model);
  unsigned i;
  region_id *view_rid;
  FOR_EACH_VEC_ELT (m_view_rids, i, view_rid)
    {
      gcc_assert (!view_rid->null_p ());
      view_rid->validate (model);
    }
  m_active_view_rid.validate (model);
}

/* Apply MAP to svalue_ids to this region.  This updates the value
   for the region (if any).  */

void
region::remap_svalue_ids (const svalue_id_map &map)
{
  map.update (&m_sval_id);
}

/* Base implementation of region::remap_region_ids vfunc; subclasses should
   chain up to this, updating any region_id data.  */

void
region::remap_region_ids (const region_id_map &map)
{
  map.update (&m_parent_rid);
  unsigned i;
  region_id *view_rid;
  FOR_EACH_VEC_ELT (m_view_rids, i, view_rid)
    map.update (view_rid);
  map.update (&m_active_view_rid);
}

/* Add a new region with id VIEW_RID as a view of this region.  */

void
region::add_view (region_id view_rid, region_model *model)
{
  gcc_assert (!view_rid.null_p ());
  region *new_view = model->get_region (view_rid);
  new_view->m_is_view = true;
  gcc_assert (!new_view->m_parent_rid.null_p ());
  gcc_assert (new_view->m_sval_id.null_p ());

  //gcc_assert (new_view->get_type () != NULL_TREE);
  // TODO: this can sometimes be NULL, when viewing through a (void *)

  // TODO: the type ought to not be present yet

  m_view_rids.safe_push (view_rid);
}

/* Look for a view of type TYPE of this region, returning its id if found,
   or null otherwise.  */

region_id
region::get_view (tree type, region_model *model) const
{
  unsigned i;
  region_id *view_rid;
  FOR_EACH_VEC_ELT (m_view_rids, i, view_rid)
    {
      region *view = model->get_region (*view_rid);
      gcc_assert (view->m_is_view);
      if (view->get_type () == type)
	return *view_rid;
    }
  return region_id::null ();
}

/* region's ctor.  */

region::region (region_id parent_rid, svalue_id sval_id, tree type)
: m_parent_rid (parent_rid), m_sval_id (sval_id), m_type (type),
  m_view_rids (), m_is_view (false), m_active_view_rid (region_id::null ())
{
  gcc_assert (type == NULL_TREE || TYPE_P (type));
}

/* region's copy ctor.  */

region::region (const region &other)
: m_parent_rid (other.m_parent_rid), m_sval_id (other.m_sval_id),
  m_type (other.m_type), m_view_rids (other.m_view_rids.length ()),
  m_is_view (other.m_is_view), m_active_view_rid (other.m_active_view_rid)
{
  int i;
  region_id *rid;
  FOR_EACH_VEC_ELT (other.m_view_rids, i, rid)
    m_view_rids.quick_push (*rid);
}

/* Base implementation of region::add_to_hash vfunc; subclasses should
   chain up to this.  */

void
region::add_to_hash (inchash::hash &hstate) const
{
  inchash::add (m_parent_rid, hstate);
  inchash::add (m_sval_id, hstate);
  hstate.add_ptr (m_type);
  // TODO: views
}

/* Base implementation of region::print_fields vfunc.  */

void
region::print_fields (const region_model &model ATTRIBUTE_UNUSED,
		      region_id this_rid ATTRIBUTE_UNUSED,
		      pretty_printer *pp) const
{
  pp_printf (pp, "kind: %qs", region_kind_to_str (get_kind ()));

  pp_string (pp, ", parent: ");
  m_parent_rid.print (pp);

  pp_printf (pp, ", sval: ");
  m_sval_id.print (pp);

  if (m_type)
    {
      pp_printf (pp, ", type: ");
      print_quoted_type (pp, m_type);
    }
}

/* Determine if a pointer to this region must be non-NULL.

   Generally, pointers to regions must be non-NULL, but pointers
   to symbolic_regions might, in fact, be NULL.

   This allows us to simulate functions like malloc and calloc with:
   - only one "outcome" from each statement,
   - the idea that the pointer is on the heap if non-NULL
   - the possibility that the pointer could be NULL
   - the idea that successive values returned from malloc are non-equal
   - to be able to zero-fill for calloc.  */

bool
region::non_null_p (const region_model &model) const
{
  /* Look through views to get at the underlying region.  */
  if (is_view_p ())
    return model.get_region (m_parent_rid)->non_null_p (model);

  /* Are we within a symbolic_region?  If so, it could be NULL.  */
  if (const symbolic_region *sym_reg = dyn_cast_symbolic_region ())
    {
      if (sym_reg->m_possibly_null)
	return false;
    }

  return true;
}

/* class primitive_region : public region.  */

/* Implementation of region::clone vfunc for primitive_region.  */

region *
primitive_region::clone () const
{
  return new primitive_region (*this);
}

/* Implementation of region::walk_for_canonicalization vfunc for
   primitive_region.  */

void
primitive_region::walk_for_canonicalization (canonicalization *) const
{
  /* Empty.  */
}

/* class map_region : public region.  */

/* map_region's copy ctor.  */

map_region::map_region (const map_region &other)
: region (other),
  m_map (other.m_map)
{
}

/* Compare the fields of this map_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
map_region::compare_fields (const map_region &other) const
{
  if (m_map.elements () != other.m_map.elements ())
    return false;

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      tree key = (*iter).first;
      region_id e = (*iter).second;
      region_id *other_slot = const_cast <map_t &> (other.m_map).get (key);
      if (other_slot == NULL)
	return false;
      if (e != *other_slot)
	return false;
    }
  return true;
}

/* Implementation of region::print_fields vfunc for map_region.  */

void
map_region::print_fields (const region_model &model,
			  region_id this_rid,
			  pretty_printer *pp) const
{
  region::print_fields (model, this_rid, pp);
  pp_string (pp, ", map: {");
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      if (iter != m_map.begin ())
	pp_string (pp, ", ");
      tree expr = (*iter).first;
      region_id child_rid = (*iter).second;
      dump_quoted_tree (pp, expr);
      pp_string (pp, ": ");
      child_rid.print (pp);
    }
  pp_string (pp, "}");
}

/* Implementation of region::validate vfunc for map_region.  */

void
map_region::validate (const region_model &model) const
{
  region::validate (model);
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      region_id child_rid = (*iter).second;
      child_rid.validate (model);
    }
}

/* Implementation of region::dump_dot_to_pp vfunc for map_region.  */

void
map_region::dump_dot_to_pp (const region_model &model,
			    region_id this_rid,
			    pretty_printer *pp) const
{
  region::dump_dot_to_pp (model, this_rid, pp);
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      // TODO: add nodes/edges to label things

      tree expr = (*iter).first;
      region_id child_rid = (*iter).second;

      pp_printf (pp, "rid_label_%i [label=\"", child_rid.as_int ());
      pp_write_text_to_stream (pp);
      pp_printf (pp, "%qE", expr);
      pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);
      pp_string (pp, "\"];");
      pp_newline (pp);

      pp_printf (pp, "rid_label_%i", child_rid.as_int ());
      pp_string (pp, " -> ");
      child_rid.dump_node_name_to_pp (pp);
      pp_string (pp, ";");
      pp_newline (pp);
    }
}

/* Implementation of region::dump_child_label vfunc for map_region.  */

void
map_region::dump_child_label (const region_model &model,
			      region_id this_rid,
			      region_id child_rid,
			      pretty_printer *pp) const
{
  region::dump_child_label (model, this_rid, child_rid, pp);

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      if (child_rid == (*iter).second)
	{
	  tree key = (*iter).first;
	  dump_quoted_tree (pp, key);
	  pp_string (pp, ": ");
	}
    }
}

/* Look for a child region for KEY within this map_region.
   If it doesn't already exist, create a child map_region, using TYPE for
   its type.
   Return the region_id of the child (whether pre-existing, or
   newly-created).
   Notify CTXT if we don't know how to handle TYPE.  */

region_id
map_region::get_or_create (region_model *model,
			   region_id this_rid,
			   tree key,
			   tree type,
			   region_model_context *ctxt)
{
  gcc_assert (key);
  gcc_assert (valid_key_p (key));
  region_id *slot = m_map.get (key);
  if (slot)
    return *slot;
  region_id child_rid = model->add_region_for_type (this_rid, type, ctxt);
  m_map.put (key, child_rid);
  return child_rid;
}

/* Get the region_id for the child region for KEY within this
   MAP_REGION, or NULL if there is no such child region.  */

region_id *
map_region::get (tree key)
{
  gcc_assert (key);
  gcc_assert (valid_key_p (key));
  region_id *slot = m_map.get (key);
  return slot;
}

/* Implementation of region::add_to_hash vfunc for map_region.  */

void
map_region::add_to_hash (inchash::hash &hstate) const
{
  region::add_to_hash (hstate);
  // TODO
}

/* Implementation of region::remap_region_ids vfunc for map_region.  */

void
map_region::remap_region_ids (const region_id_map &map)
{
  region::remap_region_ids (map);

  /* Remap the region ids within the map entries.  */
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    map.update (&(*iter).second);
}

/* Remove the binding of KEY to its child region (but not the
   child region itself).
   For use when purging unneeded SSA names.  */

void
map_region::unbind (tree key)
{
  gcc_assert (key);
  gcc_assert (valid_key_p (key));
  m_map.remove (key);
}

/* Look for a child region with id CHILD_RID within this map_region.
   If one is found, return its tree key, otherwise return NULL_TREE.  */

tree
map_region::get_tree_for_child_region (region_id child_rid) const
{
  // TODO: do we want to store an inverse map?
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      tree key = (*iter).first;
      region_id r = (*iter).second;
      if (r == child_rid)
	return key;
    }

  return NULL_TREE;
}

/* Look for a child region CHILD within this map_region.
   If one is found, return its tree key, otherwise return NULL_TREE.  */

tree
map_region::get_tree_for_child_region (region *child,
				       const region_model &model) const
{
  // TODO: do we want to store an inverse map?
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      tree key = (*iter).first;
      region_id r = (*iter).second;
      if (model.get_region (r) == child)
	return key;
    }

  return NULL_TREE;
}

/* Comparator for trees to impose a deterministic ordering on
   T1 and T2.  */

static int
tree_cmp (const_tree t1, const_tree t2)
{
  gcc_assert (t1);
  gcc_assert (t2);

  /* Test tree codes first.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return TREE_CODE (t1) - TREE_CODE (t2);

  /* From this point on, we know T1 and T2 have the same tree code.  */

  if (DECL_P (t1))
    {
      if (DECL_NAME (t1) && DECL_NAME (t2))
	return strcmp (IDENTIFIER_POINTER (DECL_NAME (t1)),
		       IDENTIFIER_POINTER (DECL_NAME (t2)));
      else
	{
	  if (DECL_NAME (t1))
	    return -1;
	  else if (DECL_NAME (t2))
	    return 1;
	  else
	    return DECL_UID (t1) - DECL_UID (t2);
	}
    }

  switch (TREE_CODE (t1))
    {
    case SSA_NAME:
      {
	if (SSA_NAME_VAR (t1) && SSA_NAME_VAR (t2))
	  {
	    int var_cmp = tree_cmp (SSA_NAME_VAR (t1), SSA_NAME_VAR (t2));
	    if (var_cmp)
	      return var_cmp;
	    return SSA_NAME_VERSION (t1) - SSA_NAME_VERSION (t2);
	  }
	else
	  {
	    if (SSA_NAME_VAR (t1))
	      return -1;
	    else if (SSA_NAME_VAR (t2))
	      return 1;
	    else
	      return SSA_NAME_VERSION (t1) - SSA_NAME_VERSION (t2);
	  }
      }
      break;

    case INTEGER_CST:
      return tree_int_cst_compare (t1, t2);

    case REAL_CST:
      {
	const real_value *rv1 = TREE_REAL_CST_PTR (t1);
	const real_value *rv2 = TREE_REAL_CST_PTR (t2);
	if (real_compare (UNORDERED_EXPR, rv1, rv2))
	  {
	    /* Impose an arbitrary order on NaNs relative to other NaNs
	       and to non-NaNs.  */
	    if (int cmp_isnan = real_isnan (rv1) - real_isnan (rv2))
	      return cmp_isnan;
	    if (int cmp_issignaling_nan
		  = real_issignaling_nan (rv1) - real_issignaling_nan (rv2))
	      return cmp_issignaling_nan;
	    return real_isneg (rv1) - real_isneg (rv2);
	  }
	if (real_compare (LT_EXPR, rv1, rv2))
	  return -1;
	if (real_compare (GT_EXPR, rv1, rv2))
	  return 1;
	return 0;
      }

    case STRING_CST:
      return strcmp (TREE_STRING_POINTER (t1),
		     TREE_STRING_POINTER (t2));

    default:
      gcc_unreachable ();
      break;
    }

  gcc_unreachable ();

  return 0;
}

/* qsort comparator for trees to impose a deterministic ordering on
   P1 and P2.  */

static int
tree_cmp (const void *p1, const void *p2)
{
  const_tree t1 = *(const_tree const *)p1;
  const_tree t2 = *(const_tree const *)p2;

  return tree_cmp (t1, t2);
}

/* Attempt to merge MAP_REGION_A and MAP_REGION_B into MERGED_MAP_REGION,
   which has region_id MERGED_RID, using MERGER.
   Return true if the merger is possible, false otherwise.  */

bool
map_region::can_merge_p (const map_region *map_region_a,
			 const map_region *map_region_b,
			 map_region *merged_map_region,
			 region_id merged_rid,
			 model_merger *merger)
{
  for (map_t::iterator iter = map_region_a->m_map.begin ();
       iter != map_region_a->m_map.end ();
       ++iter)
    {
      tree key_a = (*iter).first;
      region_id rid_a = (*iter).second;

      if (const region_id *slot_b
	  = const_cast<map_region *>(map_region_b)->m_map.get (key_a))
	{
	  region_id rid_b = *slot_b;

	  region *child_region_a = merger->get_region_a <region> (rid_a);
	  region *child_region_b = merger->get_region_b <region> (rid_b);

	  gcc_assert (child_region_a->get_type ()
		      == child_region_b->get_type ());

	  gcc_assert (child_region_a->get_kind ()
		      == child_region_b->get_kind ());

	  region_id child_merged_rid
	    = merged_map_region->get_or_create (merger->m_merged_model,
						merged_rid,
						key_a,
						child_region_a->get_type (),
						NULL);

	  region *child_merged_region
	    = merger->m_merged_model->get_region (child_merged_rid);

	  /* Consider values.  */
	  svalue_id child_a_sid = child_region_a->get_value_direct ();
	  svalue_id child_b_sid = child_region_b->get_value_direct ();
	  svalue_id child_merged_sid;
	  if (!merger->can_merge_values_p (child_a_sid, child_b_sid,
					   &child_merged_sid))
	    return false;
	  if (!child_merged_sid.null_p ())
	    child_merged_region->set_value (*merger->m_merged_model,
					    child_merged_rid,
					    child_merged_sid,
					    NULL);

	  if (map_region *map_region_a = child_region_a->dyn_cast_map_region ())
	    {
	      /* Recurse.  */
	      if (!can_merge_p (map_region_a,
				as_a <map_region *> (child_region_b),
				as_a <map_region *> (child_merged_region),
				child_merged_rid,
				merger))
		return false;
	    }

	}
      else
	{
	  /* TODO: region is present in A, but absent in B.  */
	}
    }

  /* TODO: check for keys in B that aren't in A.  */

  return true;
}


/* Implementation of region::walk_for_canonicalization vfunc for
   map_region.  */

void
map_region::walk_for_canonicalization (canonicalization *c) const
{
  auto_vec<tree> keys (m_map.elements ());
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      tree key_a = (*iter).first;
      keys.quick_push (key_a);
    }
  keys.qsort (tree_cmp);

  unsigned i;
  tree key;
  FOR_EACH_VEC_ELT (keys, i, key)
    {
      region_id rid = *const_cast<map_region *>(this)->m_map.get (key);
      c->walk_rid (rid);
    }
}

/* For debugging purposes: look for a child region for a decl named
   IDENTIFIER (or an SSA_NAME for such a decl), returning its value,
   or svalue_id::null if none are found.  */

svalue_id
map_region::get_value_by_name (tree identifier,
			       const region_model &model) const
{
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      tree key = (*iter).first;
      if (TREE_CODE (key) == SSA_NAME)
	if (SSA_NAME_VAR (key))
	  key = SSA_NAME_VAR (key);
      if (DECL_P (key))
	if (DECL_NAME (key) == identifier)
	  {
	    region_id rid = (*iter).second;
	    region *region = model.get_region (rid);
	    return region->get_value (const_cast<region_model &>(model),
				      false, NULL);
	  }
    }
  return svalue_id::null ();
}

/* class struct_or_union_region : public map_region.  */

/* Implementation of map_region::valid_key_p vfunc for
   struct_or_union_region.  */

bool
struct_or_union_region::valid_key_p (tree key) const
{
  return TREE_CODE (key) == FIELD_DECL;
}

/* Compare the fields of this struct_or_union_region with OTHER, returning
   true if they are equal.
   For use by region::operator==.  */

bool
struct_or_union_region::compare_fields (const struct_or_union_region &other)
  const
{
  return map_region::compare_fields (other);
}

/* class struct_region : public struct_or_union_region.  */

/* Implementation of region::clone vfunc for struct_region.  */

region *
struct_region::clone () const
{
  return new struct_region (*this);
}

/* Compare the fields of this struct_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
struct_region::compare_fields (const struct_region &other) const
{
  return struct_or_union_region::compare_fields (other);
}

/* class union_region : public struct_or_union_region.  */

/* Implementation of region::clone vfunc for union_region.  */

region *
union_region::clone () const
{
  return new union_region (*this);
}

/* Compare the fields of this union_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
union_region::compare_fields (const union_region &other) const
{
  return struct_or_union_region::compare_fields (other);
}

/* class frame_region : public map_region.  */

/* Compare the fields of this frame_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
frame_region::compare_fields (const frame_region &other) const
{
  if (!map_region::compare_fields (other))
    return false;
  if (m_fun != other.m_fun)
    return false;
  if (m_depth != other.m_depth)
    return false;
  return true;
}

/* Implementation of region::clone vfunc for frame_region.  */

region *
frame_region::clone () const
{
  return new frame_region (*this);
}

/* Implementation of map_region::valid_key_p vfunc for frame_region.  */

bool
frame_region::valid_key_p (tree key) const
{
  // TODO: could also check that VAR_DECLs are locals
  return (TREE_CODE (key) == PARM_DECL
	  || TREE_CODE (key) == VAR_DECL
	  || TREE_CODE (key) == SSA_NAME
	  || TREE_CODE (key) == RESULT_DECL);
}

/* Implementation of region::print_fields vfunc for frame_region.  */

void
frame_region::print_fields (const region_model &model,
			    region_id this_rid,
			    pretty_printer *pp) const
{
  map_region::print_fields (model, this_rid, pp);
  pp_printf (pp, ", function: %qs, depth: %i", function_name (m_fun), m_depth);
}

/* Implementation of region::add_to_hash vfunc for frame_region.  */

void
frame_region::add_to_hash (inchash::hash &hstate) const
{
  map_region::add_to_hash (hstate);
  hstate.add_ptr (m_fun);
  hstate.add_int (m_depth);
}

/* class globals_region : public scope_region.  */

/* Compare the fields of this globals_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
globals_region::compare_fields (const globals_region &other) const
{
  return map_region::compare_fields (other);
}

/* Implementation of region::clone vfunc for globals_region.  */

region *
globals_region::clone () const
{
  return new globals_region (*this);
}

/* Implementation of map_region::valid_key_p vfunc for globals_region.  */

bool
globals_region::valid_key_p (tree key) const
{
  return TREE_CODE (key) == VAR_DECL;
}

/* class code_region : public map_region.  */

/* Compare the fields of this code_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
code_region::compare_fields (const code_region &other) const
{
  return map_region::compare_fields (other);
}

/* Implementation of region::clone vfunc for code_region.  */

region *
code_region::clone () const
{
  return new code_region (*this);
}

/* Implementation of map_region::valid_key_p vfunc for code_region.  */

bool
code_region::valid_key_p (tree key) const
{
  return TREE_CODE (key) == FUNCTION_DECL;
}

/* class array_region : public region.  */

/* array_region's copy ctor.  */

array_region::array_region (const array_region &other)
: region (other),
  m_map (other.m_map)
{
}

/* Get a child region for the element with index INDEX_SID.  */

region_id
array_region::get_element (region_model *model,
			   region_id this_rid,
			   svalue_id index_sid,
			   region_model_context *ctxt)
{
  tree element_type = TREE_TYPE (get_type ());
  svalue *index_sval = model->get_svalue (index_sid);
  if (tree cst_index = index_sval->maybe_get_constant ())
    {
      key_t key = key_from_constant (cst_index);
      region_id element_rid
	= get_or_create (model, this_rid, key, element_type, ctxt);
      return element_rid;
   }

  return model->get_or_create_view (this_rid, element_type, ctxt);
}

/* Implementation of region::clone vfunc for array_region.  */

region *
array_region::clone () const
{
  return new array_region (*this);
}

/* Compare the fields of this array_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
array_region::compare_fields (const array_region &other) const
{
  if (m_map.elements () != other.m_map.elements ())
    return false;

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      int key = (*iter).first;
      region_id e = (*iter).second;
      region_id *other_slot = const_cast <map_t &> (other.m_map).get (key);
      if (other_slot == NULL)
	return false;
      if (e != *other_slot)
	return false;
    }
  return true;
}

/* Implementation of region::print_fields vfunc for array_region.  */

void
array_region::print_fields (const region_model &model,
			    region_id this_rid,
			    pretty_printer *pp) const
{
  region::print_fields (model, this_rid, pp);
  pp_string (pp, ", array: {");
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      if (iter != m_map.begin ())
	pp_string (pp, ", ");
      int key = (*iter).first;
      region_id child_rid = (*iter).second;
      pp_printf (pp, "[%i]: ", key);
      child_rid.print (pp);
    }
  pp_string (pp, "}");
}

/* Implementation of region::validate vfunc for array_region.  */

void
array_region::validate (const region_model &model) const
{
  region::validate (model);
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      region_id child_rid = (*iter).second;
      child_rid.validate (model);
    }
}

/* Implementation of region::dump_dot_to_pp vfunc for array_region.  */

void
array_region::dump_dot_to_pp (const region_model &model,
			      region_id this_rid,
			      pretty_printer *pp) const
{
  region::dump_dot_to_pp (model, this_rid, pp);
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      // TODO: add nodes/edges to label things

      int key = (*iter).first;
      region_id child_rid = (*iter).second;

      pp_printf (pp, "rid_label_%i [label=\"", child_rid.as_int ());
      pp_write_text_to_stream (pp);
      pp_printf (pp, "%qi", key);
      pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);
      pp_string (pp, "\"];");
      pp_newline (pp);

      pp_printf (pp, "rid_label_%i", child_rid.as_int ());
      pp_string (pp, " -> ");
      child_rid.dump_node_name_to_pp (pp);
      pp_string (pp, ";");
      pp_newline (pp);
    }
}

/* Implementation of region::dump_child_label vfunc for array_region.  */

void
array_region::dump_child_label (const region_model &model,
			      region_id this_rid,
			      region_id child_rid,
			      pretty_printer *pp) const
{
  region::dump_child_label (model, this_rid, child_rid, pp);

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      if (child_rid == (*iter).second)
	{
	  int key = (*iter).first;
	  pp_printf (pp, "[%i]: ", key);
	}
    }
}

/* Look for a child region for KEY within this array_region.
   If it doesn't already exist, create a child array_region, using TYPE for
   its type.
   Return the region_id of the child (whether pre-existing, or
   newly-created).
   Notify CTXT if we don't know how to handle TYPE.  */

region_id
array_region::get_or_create (region_model *model,
			     region_id this_rid,
			     key_t key,
			     tree type,
			     region_model_context *ctxt)
{
  region_id *slot = m_map.get (key);
  if (slot)
    return *slot;
  region_id child_rid = model->add_region_for_type (this_rid, type, ctxt);
  m_map.put (key, child_rid);
  return child_rid;
}

/* Get the region_id for the child region for KEY within this
   ARRAY_REGION, or NULL if there is no such child region.  */

region_id *
array_region::get (key_t key)
{
  region_id *slot = m_map.get (key);
  return slot;
}

/* Implementation of region::add_to_hash vfunc for array_region.  */

void
array_region::add_to_hash (inchash::hash &hstate) const
{
  region::add_to_hash (hstate);
  // TODO
}

/* Implementation of region::remap_region_ids vfunc for array_region.  */

void
array_region::remap_region_ids (const region_id_map &map)
{
  region::remap_region_ids (map);

  /* Remap the region ids within the map entries.  */
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    map.update (&(*iter).second);
}

/* Look for a child region with id CHILD_RID within this array_region.
   If one is found, write its key to *OUT and return true,
   otherwise return false.  */

bool
array_region::get_key_for_child_region (region_id child_rid, key_t *out) const
{
  // TODO: do we want to store an inverse map?
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      key_t key = (*iter).first;
      region_id r = (*iter).second;
      if (r == child_rid)
	{
	  *out = key;
	  return true;
	}
    }

  return false;
}

/* qsort comparator for array_region's keys.  */

int
array_region::key_cmp (const void *p1, const void *p2)
{
  key_t i1 = *(const key_t *)p1;
  key_t i2 = *(const key_t *)p2;

  if (i1 > i2)
    return 1;
  else if (i1 < i2)
    return -1;
  else
    return 0;
}

/* Implementation of region::walk_for_canonicalization vfunc for
   array_region.  */

void
array_region::walk_for_canonicalization (canonicalization *c) const
{
  auto_vec<int> keys (m_map.elements ());
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      int key_a = (*iter).first;
      keys.quick_push (key_a);
    }
  keys.qsort (key_cmp);

  unsigned i;
  int key;
  FOR_EACH_VEC_ELT (keys, i, key)
    {
      region_id rid = *const_cast<array_region *>(this)->m_map.get (key);
      c->walk_rid (rid);
    }
}

/* Convert constant CST into an array_region::key_t.  */

array_region::key_t
array_region::key_from_constant (tree cst)
{
  gcc_assert (CONSTANT_CLASS_P (cst));
  wide_int w = wi::to_wide (cst);
  key_t result = w.to_shwi ();
  return result;
}

/* Convert array_region::key_t KEY into a tree constant.  */

tree
array_region::constant_from_key (key_t key)
{
  tree array_type = get_type ();
  tree index_type = TYPE_DOMAIN (array_type);
  return build_int_cst (index_type, key);
}

/* class function_region : public map_region.  */

/* Compare the fields of this function_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
function_region::compare_fields (const function_region &other) const
{
  return map_region::compare_fields (other);
}

/* Implementation of region::clone vfunc for function_region.  */

region *
function_region::clone () const
{
  return new function_region (*this);
}

/* Implementation of map_region::valid_key_p vfunc for function_region.  */

bool
function_region::valid_key_p (tree key) const
{
  return TREE_CODE (key) == LABEL_DECL;
}

/* class stack_region : public region.  */

/* stack_region's copy ctor.  */

stack_region::stack_region (const stack_region &other)
: region (other),
  m_frame_rids (other.m_frame_rids.length ())
{
  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (other.m_frame_rids, i, frame_rid)
    m_frame_rids.quick_push (*frame_rid);
}

/* Compare the fields of this stack_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
stack_region::compare_fields (const stack_region &other) const
{
  if (m_frame_rids.length () != other.m_frame_rids.length ())
    return false;

  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (m_frame_rids, i, frame_rid)
    if (m_frame_rids[i] != other.m_frame_rids[i])
      return false;

  return true;
}

/* Implementation of region::clone vfunc for stack_region.  */

region *
stack_region::clone () const
{
  return new stack_region (*this);
}

/* Implementation of region::print_fields vfunc for stack_region.  */

void
stack_region::print_fields (const region_model &model,
			    region_id this_rid,
			    pretty_printer *pp) const
{
  region::print_fields (model, this_rid, pp);
  // TODO
}

/* Implementation of region::dump_child_label vfunc for stack_region.  */

void
stack_region::dump_child_label (const region_model &model,
				region_id this_rid ATTRIBUTE_UNUSED,
				region_id child_rid,
				pretty_printer *pp) const
{
  function *fun = model.get_region<frame_region> (child_rid)->get_function ();
  pp_printf (pp, "frame for %qs: ", function_name (fun));
}

/* Implementation of region::validate vfunc for stack_region.  */

void
stack_region::validate (const region_model &model) const
{
  region::validate (model);
  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (m_frame_rids, i, frame_rid)
    m_frame_rids[i].validate (model);
}

/* Push FRAME_RID (for a frame_region) onto this stack.  */

void
stack_region::push_frame (region_id frame_rid)
{
  m_frame_rids.safe_push (frame_rid);
}

/* Get the region_id of the top-most frame in this stack, if any.  */

region_id
stack_region::get_current_frame_id () const
{
  if (m_frame_rids.length () > 0)
    return m_frame_rids[m_frame_rids.length () - 1];
  else
    return region_id::null ();
}

/* Pop the topmost frame_region from this stack.

   If RESULT_DST_RID is non-null, copy any return value from the frame
   into RESULT_DST_RID's region.

   Purge the frame region and all its descendent regions.
   Convert any pointers that point into such regions into
   POISON_KIND_POPPED_STACK svalues.

   If PURGE, then purge all unused svalues, with the exception of any
   returned values.

   Accumulate stats on purged entities into STATS.  */

void
stack_region::pop_frame (region_model *model, region_id result_dst_rid,
			 bool purge, purge_stats *stats,
			 region_model_context *ctxt)
{
  gcc_assert (m_frame_rids.length () > 0);

  region_id frame_rid = get_current_frame_id ();
  frame_region *frame = model->get_region<frame_region> (frame_rid);

  /* Evaluate the result, within the callee frame.  */
  svalue_id_set returned_sids;
  tree fndecl = frame->get_function ()->decl;
  tree result = DECL_RESULT (fndecl);
  if (result && TREE_TYPE (result) != void_type_node)
    {
      if (!result_dst_rid.null_p ())
	{
	  /* Copy the result to RESULT_DST_RID.  */
	  model->copy_region (result_dst_rid, model->get_lvalue (result, ctxt),
			      ctxt);
	}
      if (purge)
	{
	  /* Populate returned_sids, to avoid purging them.  */
	  region_id return_rid = model->get_lvalue (result, NULL);
	  region_id_set returned_rids (model);
	  model->get_descendents (return_rid, &returned_rids,
				  region_id::null ());
	  for (unsigned i = 0; i < model->get_num_regions (); i++)
	    {
	      region_id rid = region_id::from_int (i);
	      if (returned_rids.region_p (rid))
		{
		  svalue_id sid = model->get_region (rid)->get_value_direct ();
		  returned_sids.add_svalue (sid);
		}
	    }
	}
    }

  /* Pop the frame RID.  */
  m_frame_rids.pop ();

  model->delete_region_and_descendents (frame_rid,
					POISON_KIND_POPPED_STACK,
					stats,
					ctxt ? ctxt->get_logger () : NULL);

  /* Delete unused svalues, but don't delete the return value(s).  */
  if (purge)
    model->purge_unused_svalues (stats, ctxt, &returned_sids);

  model->validate ();
}

/* Implementation of region::add_to_hash vfunc for stack_region.  */

void
stack_region::add_to_hash (inchash::hash &hstate) const
{
  region::add_to_hash (hstate);

  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (m_frame_rids, i, frame_rid)
    inchash::add (*frame_rid, hstate);
}

/* Implementation of region::remap_region_ids vfunc for stack_region.  */

void
stack_region::remap_region_ids (const region_id_map &map)
{
  region::remap_region_ids (map);
  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (m_frame_rids, i, frame_rid)
    map.update (&m_frame_rids[i]);
}

/* Attempt to merge STACK_REGION_A and STACK_REGION_B using MERGER.
   Return true if the merger is possible, false otherwise.  */

bool
stack_region::can_merge_p (const stack_region *stack_region_a,
			   const stack_region *stack_region_b,
			   model_merger *merger)
{
  if (stack_region_a->get_num_frames ()
      != stack_region_b->get_num_frames ())
    return false;

  region_model *merged_model = merger->m_merged_model;

  region_id rid_merged_stack
    = merged_model->get_root_region ()->ensure_stack_region (merged_model);

  stack_region *merged_stack
    = merged_model->get_region <stack_region> (rid_merged_stack);

  /* First, create all frames in the merged model, without populating them.
     The merging code assumes that all frames in the merged model already exist,
     so we have to do this first to handle the case in which a local in an
     older frame points at a local in a more recent frame.  */
    for (unsigned i = 0; i < stack_region_a->get_num_frames (); i++)
      {
	region_id rid_a = stack_region_a->get_frame_rid (i);
	frame_region *frame_a = merger->get_region_a <frame_region> (rid_a);

	region_id rid_b = stack_region_b->get_frame_rid (i);
	frame_region *frame_b = merger->get_region_b <frame_region> (rid_b);

	if (frame_a->get_function () != frame_b->get_function ())
	  return false;

	frame_region *merged_frame = new frame_region (rid_merged_stack,
						       frame_a->get_function (),
						       frame_a->get_depth ());
	region_id rid_merged_frame = merged_model->add_region (merged_frame);
	merged_stack->push_frame (rid_merged_frame);
      }

    /* Now populate the frames we created.  */
    for (unsigned i = 0; i < stack_region_a->get_num_frames (); i++)
      {
	region_id rid_a = stack_region_a->get_frame_rid (i);
	frame_region *frame_a = merger->get_region_a <frame_region> (rid_a);

	region_id rid_b = stack_region_b->get_frame_rid (i);
	frame_region *frame_b = merger->get_region_b <frame_region> (rid_b);

	region_id rid_merged_frame = merged_stack->get_frame_rid (i);
	frame_region *merged_frame
	  = merged_model->get_region <frame_region> (rid_merged_frame);
	if (!map_region::can_merge_p (frame_a, frame_b,
				      merged_frame, rid_merged_frame,
				      merger))
	  return false;
      }

  return true;
}

/* Implementation of region::walk_for_canonicalization vfunc for
   stack_region.  */

void
stack_region::walk_for_canonicalization (canonicalization *c) const
{
  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (m_frame_rids, i, frame_rid)
    c->walk_rid (*frame_rid);
}

/* For debugging purposes: look for a grandchild region within one of
   the child frame regions, where the grandchild is for a decl named
   IDENTIFIER (or an SSA_NAME for such a decl):

     stack_region
     `-frame_region
       `-region for decl named IDENTIFIER

   returning its value, or svalue_id::null if none are found.  */

svalue_id
stack_region::get_value_by_name (tree identifier,
				 const region_model &model) const
{
  int i;
  region_id *frame_rid;
  FOR_EACH_VEC_ELT (m_frame_rids, i, frame_rid)
    {
      frame_region *frame = model.get_region<frame_region> (*frame_rid);
      svalue_id sid = frame->get_value_by_name (identifier, model);
      if (!sid.null_p ())
	return sid;
    }

  return svalue_id::null ();
}

/* class heap_region : public region.  */

/* heap_region's copy ctor.  */

heap_region::heap_region (const heap_region &other)
: region (other)
{
}

/* Compare the fields of this heap_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
heap_region::compare_fields (const heap_region &) const
{
  /* Empty.  */
  return true;
}

/* Implementation of region::clone vfunc for heap_region.  */

region *
heap_region::clone () const
{
  return new heap_region (*this);
}

/* Implementation of region::walk_for_canonicalization vfunc for
   heap_region.  */

void
heap_region::walk_for_canonicalization (canonicalization *) const
{
  /* Empty.  */
}

/* class root_region : public region.  */

/* root_region's default ctor.  */

root_region::root_region ()
: region (region_id::null (),
	  svalue_id::null (),
	  NULL_TREE)
{
}

/* root_region's copy ctor.  */

root_region::root_region (const root_region &other)
: region (other),
  m_stack_rid (other.m_stack_rid),
  m_globals_rid (other.m_globals_rid),
  m_code_rid (other.m_code_rid),
  m_heap_rid (other.m_heap_rid)
{
}

/* Compare the fields of this root_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
root_region::compare_fields (const root_region &other) const
{
  if (m_stack_rid != other.m_stack_rid)
    return false;
  if (m_globals_rid != other.m_globals_rid)
    return false;
  if (m_code_rid != other.m_code_rid)
    return false;
  if (m_heap_rid != other.m_heap_rid)
    return false;
  return true;
}

/* Implementation of region::clone vfunc for root_region.  */

region *
root_region::clone () const
{
  return new root_region (*this);
}

/* Implementation of region::print_fields vfunc for root_region.  */

void
root_region::print_fields (const region_model &model,
			   region_id this_rid,
			   pretty_printer *pp) const
{
  region::print_fields (model, this_rid, pp);
  // TODO
}

/* Implementation of region::validate vfunc for root_region.  */

void
root_region::validate (const region_model &model) const
{
  region::validate (model);
  m_stack_rid.validate (model);
  m_globals_rid.validate (model);
  m_code_rid.validate (model);
  m_heap_rid.validate (model);
}

/* Implementation of region::dump_child_label vfunc for root_region.  */

void
root_region::dump_child_label (const region_model &model ATTRIBUTE_UNUSED,
			       region_id this_rid ATTRIBUTE_UNUSED,
			       region_id child_rid,
			       pretty_printer *pp) const
{
  if (child_rid == m_stack_rid)
    pp_printf (pp, "stack: ");
  else if (child_rid == m_globals_rid)
    pp_printf (pp, "globals: ");
  else if (child_rid == m_code_rid)
    pp_printf (pp, "code: ");
  else if (child_rid == m_heap_rid)
    pp_printf (pp, "heap: ");
}

/* Create a new frame_region for a call to FUN and push it onto
   the stack.

   If ARG_SIDS is non-NULL, use it to populate the parameters
   in the new frame.
   Otherwise, populate them with unknown values.

   Return the region_id of the new frame.  */

region_id
root_region::push_frame (region_model *model, function *fun,
			 vec<svalue_id> *arg_sids,
			 region_model_context *ctxt)
{
  gcc_assert (fun);
  /* arg_sids can be NULL.  */

  ensure_stack_region (model);
  stack_region *stack = model->get_region <stack_region> (m_stack_rid);

  frame_region *region = new frame_region (m_stack_rid, fun,
					   stack->get_num_frames ());
  region_id frame_rid = model->add_region (region);

  // TODO: unify these cases by building a vec of unknown?

  if (arg_sids)
    {
      /* Arguments supplied from a caller frame.  */

      tree fndecl = fun->decl;
      unsigned idx = 0;
      for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
	   iter_parm = DECL_CHAIN (iter_parm), ++idx)
	{
	  /* If there's a mismatching declaration, the call stmt might
	     not have enough args.  Handle this case by leaving the
	     rest of the params as uninitialized.  */
	  if (idx >= arg_sids->length ())
	    break;
	  svalue_id arg_sid = (*arg_sids)[idx];
	  region_id parm_rid
	    = region->get_or_create (model, frame_rid, iter_parm,
				     TREE_TYPE (iter_parm), ctxt);
	  model->set_value (parm_rid, arg_sid, ctxt);

	  /* Also do it for default SSA name (sharing the same unknown
	     value).  */
	  tree parm_default_ssa = ssa_default_def (fun, iter_parm);
	  if (parm_default_ssa)
	    {
	      region_id defssa_rid
		= region->get_or_create (model, frame_rid, parm_default_ssa,
					 TREE_TYPE (iter_parm), ctxt);
	      model->set_value (defssa_rid, arg_sid, ctxt);
	    }
	}
    }
  else
    {
      /* No known arguments (a top-level call within the analysis).  */

      /* Params have a defined, unknown value; they should not inherit
	 from the poisoned uninit value.  */
      tree fndecl = fun->decl;
      for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
	   iter_parm = DECL_CHAIN (iter_parm))
	{
	  region_id parm_rid
	    = region->get_or_create (model, frame_rid, iter_parm,
				     TREE_TYPE (iter_parm), ctxt);
	  svalue_id parm_sid
	    = model->set_to_new_unknown_value (parm_rid, TREE_TYPE (iter_parm),
					       ctxt);

	  /* Also do it for default SSA name (sharing the same unknown
	     value).  */
	  tree parm_default_ssa = ssa_default_def (fun, iter_parm);
	  if (parm_default_ssa)
	    {
	      region_id defssa_rid
		= region->get_or_create (model, frame_rid, parm_default_ssa,
					 TREE_TYPE (iter_parm), ctxt);
	      model->get_region (defssa_rid)->set_value (*model, defssa_rid,
							 parm_sid, ctxt);
	    }
	}
    }

  stack->push_frame (frame_rid);

  return frame_rid;
}

/* Get the region_id of the top-most frame in this root_region's stack,
   if any.  */

region_id
root_region::get_current_frame_id (const region_model &model) const
{
  stack_region *stack = model.get_region <stack_region> (m_stack_rid);
  if (stack)
    return stack->get_current_frame_id ();
  else
    return region_id::null ();
}

/* Pop the topmost frame_region from this root_region's stack;
   see the comment for stack_region::pop_frame.  */

void
root_region::pop_frame (region_model *model, region_id result_dst_rid,
			bool purge, purge_stats *out,
			region_model_context *ctxt)
{
  stack_region *stack = model->get_region <stack_region> (m_stack_rid);
  stack->pop_frame (model, result_dst_rid, purge, out, ctxt);
}

/* Return the region_id of the stack region, creating it if doesn't
   already exist.  */

region_id
root_region::ensure_stack_region (region_model *model)
{
  if (m_stack_rid.null_p ())
    {
      svalue_id uninit_sid
	= model->add_svalue (new poisoned_svalue (POISON_KIND_UNINIT,
						  NULL_TREE));
      m_stack_rid
	= model->add_region (new stack_region (model->get_root_rid (),
					       uninit_sid));
    }
  return m_stack_rid;
}

/* Return the stack region (which could be NULL).  */

stack_region *
root_region::get_stack_region (const region_model *model) const
{
  return model->get_region <stack_region> (m_stack_rid);
}

/* Return the region_id of the globals region, creating it if doesn't
   already exist.  */

region_id
root_region::ensure_globals_region (region_model *model)
{
  if (m_globals_rid.null_p ())
    m_globals_rid
      = model->add_region (new globals_region (model->get_root_rid ()));
  return m_globals_rid;
}

/* Return the code region (which could be NULL).  */

code_region *
root_region::get_code_region (const region_model *model) const
{
  return model->get_region <code_region> (m_code_rid);
}

/* Return the region_id of the code region, creating it if doesn't
   already exist.  */

region_id
root_region::ensure_code_region (region_model *model)
{
  if (m_code_rid.null_p ())
    m_code_rid
      = model->add_region (new code_region (model->get_root_rid ()));
  return m_code_rid;
}

/* Return the globals region (which could be NULL).  */

globals_region *
root_region::get_globals_region (const region_model *model) const
{
  return model->get_region <globals_region> (m_globals_rid);
}

/* Return the region_id of the heap region, creating it if doesn't
   already exist.  */

region_id
root_region::ensure_heap_region (region_model *model)
{
  if (m_heap_rid.null_p ())
    {
      svalue_id uninit_sid
	= model->add_svalue (new poisoned_svalue (POISON_KIND_UNINIT,
						  NULL_TREE));
      m_heap_rid
	= model->add_region (new heap_region (model->get_root_rid (),
					      uninit_sid));
    }
  return m_heap_rid;
}

/* Return the heap region (which could be NULL).  */

heap_region *
root_region::get_heap_region (const region_model *model) const
{
  return model->get_region <heap_region> (m_heap_rid);
}

/* Implementation of region::remap_region_ids vfunc for root_region.  */

void
root_region::remap_region_ids (const region_id_map &map)
{
  map.update (&m_stack_rid);
  map.update (&m_globals_rid);
  map.update (&m_code_rid);
  map.update (&m_heap_rid);
}

/* Attempt to merge ROOT_REGION_A and ROOT_REGION_B into
   MERGED_ROOT_REGION using MERGER.
   Return true if the merger is possible, false otherwise.  */

bool
root_region::can_merge_p (const root_region *root_region_a,
			  const root_region *root_region_b,
			  root_region *merged_root_region,
			  model_merger *merger)
{
  /* We can only merge if the stacks are sufficiently similar.  */
  stack_region *stack_a = root_region_a->get_stack_region (merger->m_model_a);
  stack_region *stack_b = root_region_b->get_stack_region (merger->m_model_b);
  if (stack_a && stack_b)
    {
      /* If the two models both have a stack, attempt to merge them.  */
      merged_root_region->ensure_stack_region (merger->m_merged_model);
      if (!stack_region::can_merge_p (stack_a, stack_b, merger))
	return false;
    }
  else if (stack_a || stack_b)
    /* Don't attempt to merge if one model has a stack and the other
       doesn't.  */
    return false;

  map_region *globals_a = root_region_a->get_globals_region (merger->m_model_a);
  map_region *globals_b = root_region_b->get_globals_region (merger->m_model_b);
  if (globals_a && globals_b)
    {
      /* If both models have globals regions, attempt to merge them.  */
      region_id merged_globals_rid
	= merged_root_region->ensure_globals_region (merger->m_merged_model);
      map_region *merged_globals
	= merged_root_region->get_globals_region (merger->m_merged_model);
      if (!map_region::can_merge_p (globals_a, globals_b,
				    merged_globals, merged_globals_rid,
				    merger))
	return false;
    }
  /* otherwise, merge as "no globals".  */

  map_region *code_a = root_region_a->get_code_region (merger->m_model_a);
  map_region *code_b = root_region_b->get_code_region (merger->m_model_b);
  if (code_a && code_b)
    {
      /* If both models have code regions, attempt to merge them.  */
      region_id merged_code_rid
	= merged_root_region->ensure_code_region (merger->m_merged_model);
      map_region *merged_code
	= merged_root_region->get_code_region (merger->m_merged_model);
      if (!map_region::can_merge_p (code_a, code_b,
				    merged_code, merged_code_rid,
				    merger))
	return false;
    }
  /* otherwise, merge as "no code".  */

  heap_region *heap_a = root_region_a->get_heap_region (merger->m_model_a);
  heap_region *heap_b = root_region_b->get_heap_region (merger->m_model_b);
  if (heap_a && heap_b)
    {
      /* If both have a heap, create a "merged" heap.
	 Actually merging the heap contents happens via the region_svalue
	 instances, as needed, when seeing pairs of region_svalue instances.  */
      merged_root_region->ensure_heap_region (merger->m_merged_model);
    }
  /* otherwise, merge as "no heap".  */

  return true;
}

/* Implementation of region::add_to_hash vfunc for root_region.  */

void
root_region::add_to_hash (inchash::hash &hstate) const
{
  region::add_to_hash (hstate);
  inchash::add (m_stack_rid, hstate);
  inchash::add (m_globals_rid, hstate);
  inchash::add (m_code_rid, hstate);
  inchash::add (m_heap_rid, hstate);
}

/* Implementation of region::walk_for_canonicalization vfunc for
   root_region.  */

void
root_region::walk_for_canonicalization (canonicalization *c) const
{
  c->walk_rid (m_stack_rid);
  c->walk_rid (m_globals_rid);
  c->walk_rid (m_code_rid);
  c->walk_rid (m_heap_rid);
}

/* For debugging purposes: look for a descendant region for a local
   or global decl named IDENTIFIER (or an SSA_NAME for such a decl),
   returning its value, or svalue_id::null if none are found.  */

svalue_id
root_region::get_value_by_name (tree identifier,
				const region_model &model) const
{
  if (stack_region *stack = get_stack_region (&model))
    {
      svalue_id sid = stack->get_value_by_name (identifier, model);
      if (!sid.null_p ())
	return sid;
    }
  if (map_region *globals = get_globals_region (&model))
    {
      svalue_id sid = globals->get_value_by_name (identifier, model);
      if (!sid.null_p ())
	return sid;
    }
  return svalue_id::null ();
}

/* class symbolic_region : public map_region.  */

/* symbolic_region's copy ctor.  */

symbolic_region::symbolic_region (const symbolic_region &other)
: region (other),
  m_possibly_null (other.m_possibly_null)
{
}

/* Compare the fields of this symbolic_region with OTHER, returning true
   if they are equal.
   For use by region::operator==.  */

bool
symbolic_region::compare_fields (const symbolic_region &other) const
{
  return m_possibly_null == other.m_possibly_null;
}

/* Implementation of region::clone vfunc for symbolic_region.  */

region *
symbolic_region::clone () const
{
  return new symbolic_region (*this);
}

/* Implementation of region::walk_for_canonicalization vfunc for
   symbolic_region.  */

void
symbolic_region::walk_for_canonicalization (canonicalization *) const
{
  /* Empty.  */
}

/* Implementation of region::print_fields vfunc for symbolic_region.  */

void
symbolic_region::print_fields (const region_model &model,
			       region_id this_rid,
			       pretty_printer *pp) const
{
  region::print_fields (model, this_rid, pp);
  pp_printf (pp, ", possibly_null: %s", m_possibly_null ? "true" : "false");
}

/* class region_model.  */

/* region_model's default ctor.  */

region_model::region_model ()
{
  m_root_rid = add_region (new root_region ());
  m_constraints = new impl_constraint_manager (this);
  // TODO
}

/* region_model's copy ctor.  */

region_model::region_model (const region_model &other)
: m_svalues (other.m_svalues.length ()),
  m_regions (other.m_regions.length ()),
  m_root_rid (other.m_root_rid)
{
  /* Clone the svalues and regions.  */
  int i;

  svalue *svalue;
  FOR_EACH_VEC_ELT (other.m_svalues, i, svalue)
    m_svalues.quick_push (svalue->clone ());

  region *region;
  FOR_EACH_VEC_ELT (other.m_regions, i, region)
    m_regions.quick_push (region->clone ());

  m_constraints = other.m_constraints->clone (this);
}

/* region_model's dtor.  */

region_model::~region_model ()
{
  delete m_constraints;
}

/* region_model's assignment operator.  */

region_model &
region_model::operator= (const region_model &other)
{
  unsigned i;
  svalue *svalue;
  region *region;

  /* Delete existing content.  */
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    delete svalue;
  m_svalues.truncate (0);

  FOR_EACH_VEC_ELT (m_regions, i, region)
    delete region;
  m_regions.truncate (0);

  delete m_constraints;

  /* Clone the svalues and regions.  */
  m_svalues.reserve (other.m_svalues.length (), true);
  FOR_EACH_VEC_ELT (other.m_svalues, i, svalue)
    m_svalues.quick_push (svalue->clone ());

  m_regions.reserve (other.m_regions.length (), true);
  FOR_EACH_VEC_ELT (other.m_regions, i, region)
    m_regions.quick_push (region->clone ());

  m_root_rid = other.m_root_rid;

  m_constraints = other.m_constraints->clone (this);

  return *this;
}

/* Equality operator for region_model.

   Amongst other things this directly compares the svalue and region
   vectors and so for this to be meaningful both this and OTHER should
   have been canonicalized.  */

bool
region_model::operator== (const region_model &other) const
{
  if (m_root_rid != other.m_root_rid)
    return false;

  if (m_svalues.length () != other.m_svalues.length ())
    return false;

  if (m_regions.length () != other.m_regions.length ())
    return false;

  if (*m_constraints != *other.m_constraints)
    return false;

  unsigned i;
  svalue *svalue;
  FOR_EACH_VEC_ELT (other.m_svalues, i, svalue)
    if (!(*m_svalues[i] == *other.m_svalues[i]))
      return false;

  region *region;
  FOR_EACH_VEC_ELT (other.m_regions, i, region)
    if (!(*m_regions[i] == *other.m_regions[i]))
      return false;

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Generate a hash value for this region_model.  */

hashval_t
region_model::hash () const
{
  hashval_t result = 0;
  int i;

  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    result ^= svalue->hash ();

  region *region;
  FOR_EACH_VEC_ELT (m_regions, i, region)
    result ^= region->hash ();

  result ^= m_constraints->hash ();

  return result;
}

/* Print an all-on-one-line representation of this region_model to PP,
   which must support %E for trees.  */

void
region_model::print (pretty_printer *pp) const
{
  int i;

  pp_string (pp, "svalues: [");
  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    {
      if (i > 0)
	pp_string (pp, ", ");
      print_svalue (svalue_id::from_int (i), pp);
    }

  pp_string (pp, "], regions: [");

  region *region;
  FOR_EACH_VEC_ELT (m_regions, i, region)
    {
      if (i > 0)
	pp_string (pp, ", ");
      region->print (*this, region_id::from_int (i), pp);
    }

  pp_string (pp, "], constraints: ");

  m_constraints->print (pp);
}

/* Print the svalue with id SID to PP.  */

void
region_model::print_svalue (svalue_id sid, pretty_printer *pp) const
{
  get_svalue (sid)->print (*this, sid, pp);
}

/* Dump a .dot representation of this region_model to PP, showing
   the values and the hierarchy of regions.  */

void
region_model::dump_dot_to_pp (pretty_printer *pp) const
{
  graphviz_out gv (pp);

  pp_string (pp, "digraph \"");
  pp_write_text_to_stream (pp);
  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);
  pp_string (pp, "\" {\n");

  gv.indent ();

  pp_string (pp, "overlap=false;\n");
  pp_string (pp, "compound=true;\n");

  int i;

  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    svalue->dump_dot_to_pp (*this, svalue_id::from_int (i), pp);

  region *region;
  FOR_EACH_VEC_ELT (m_regions, i, region)
    region->dump_dot_to_pp (*this, region_id::from_int (i), pp);

  /* TODO: constraints.  */

  /* Terminate "digraph" */
  gv.outdent ();
  pp_string (pp, "}");
  pp_newline (pp);
}

/* Dump a .dot representation of this region_model to FP.  */

void
region_model::dump_dot_to_file (FILE *fp) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp.buffer->stream = fp;
  dump_dot_to_pp (&pp);
  pp_flush (&pp);
}

/* Dump a .dot representation of this region_model to PATH.  */

void
region_model::dump_dot (const char *path) const
{
  FILE *fp = fopen (path, "w");
  dump_dot_to_file (fp);
  fclose (fp);
}

/* Dump a multiline representation of this model to PP, showing the
   region hierarchy, the svalues, and any constraints.

   If SUMMARIZE is true, show only the most pertinent information,
   in a form that attempts to be less verbose.
   Otherwise, show all information.  */

void
region_model::dump_to_pp (pretty_printer *pp, bool summarize) const
{
  if (summarize)
    {
      auto_vec<path_var> rep_path_vars;

      unsigned i;
      region *reg;
      FOR_EACH_VEC_ELT (m_regions, i, reg)
	{
	  region_id rid = region_id::from_int (i);
	  path_var pv = get_representative_path_var (rid);
	  if (pv.m_tree)
	    rep_path_vars.safe_push (pv);
	}
      bool is_first = true;

      /* Work with a copy in case the get_lvalue calls change anything
	 (they shouldn't).  */
      region_model copy (*this);
      copy.dump_summary_of_rep_path_vars (pp, &rep_path_vars, &is_first);

      equiv_class *ec;
      FOR_EACH_VEC_ELT (m_constraints->m_equiv_classes, i, ec)
	{
	  for (unsigned j = 0; j < ec->m_vars.length (); j++)
	    {
	      svalue_id lhs_sid = ec->m_vars[j];
	      tree lhs_tree = get_representative_tree (lhs_sid);
	      if (lhs_tree == NULL_TREE)
		continue;
	      for (unsigned k = j + 1; k < ec->m_vars.length (); k++)
		{
		  svalue_id rhs_sid = ec->m_vars[k];
		  tree rhs_tree = get_representative_tree (rhs_sid);
		  if (rhs_tree
		      && !(CONSTANT_CLASS_P (lhs_tree)
			   && CONSTANT_CLASS_P (rhs_tree)))
		    {
		      dump_separator (pp, &is_first);
		      dump_tree (pp, lhs_tree);
		      pp_string (pp, " == ");
		      dump_tree (pp, rhs_tree);
		    }
		}
	    }
	}

      constraint *c;
      FOR_EACH_VEC_ELT (m_constraints->m_constraints, i, c)
	{
	  const equiv_class &lhs = c->m_lhs.get_obj (*m_constraints);
	  const equiv_class &rhs = c->m_rhs.get_obj (*m_constraints);
	  svalue_id lhs_sid = lhs.get_representative ();
	  svalue_id rhs_sid = rhs.get_representative ();
	  tree lhs_tree = get_representative_tree (lhs_sid);
	  tree rhs_tree = get_representative_tree (rhs_sid);
	  if (lhs_tree && rhs_tree
	      && !(CONSTANT_CLASS_P (lhs_tree) && CONSTANT_CLASS_P (rhs_tree)))
	    {
	      dump_separator (pp, &is_first);
	      dump_tree (pp, lhs_tree);
	      pp_printf (pp, " %s ", constraint_op_code (c->m_op));
	      dump_tree (pp, rhs_tree);
	   }
	}

      return;
    }

  get_region (m_root_rid)->dump_to_pp (*this, m_root_rid, pp, "", true);

  pp_string (pp, "svalues:");
  pp_newline (pp);
  int i;
  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    {
      pp_string (pp, "  ");
      svalue_id sid = svalue_id::from_int (i);
      print_svalue (sid, pp);
      pp_newline (pp);
    }

  pp_string (pp, "constraint manager:");
  pp_newline (pp);
  m_constraints->dump_to_pp (pp);
}

/* Dump a multiline representation of this model to FILE.  */

void
region_model::dump (FILE *fp, bool summarize) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp, summarize);
  pp_flush (&pp);
}

/* Dump a multiline representation of this model to stderr.  */

DEBUG_FUNCTION void
region_model::dump (bool summarize) const
{
  dump (stderr, summarize);
}

/* Dump RMODEL fully to stderr (i.e. without summarization).  */

DEBUG_FUNCTION void
region_model::debug () const
{
  dump (false);
}

/* Dump VEC to PP, in the form "{VEC elements}: LABEL".  */

static void
dump_vec_of_tree (pretty_printer *pp,
		  bool *is_first,
		  const auto_vec<tree> &vec,
		  const char *label)
{
  if (vec.length () == 0)
    return;

  dump_separator (pp, is_first);
  pp_printf (pp, "{");
  unsigned i;
  tree key;
  FOR_EACH_VEC_ELT (vec, i, key)
    {
      if (i > 0)
	pp_string (pp, ", ");
      dump_tree (pp, key);
    }
  pp_printf (pp, "}: %s", label);
}

/* Dump all *REP_PATH_VARS to PP in compact form, updating *IS_FIRST.
   Subroutine of region_model::dump_to_pp.  */

void
region_model::dump_summary_of_rep_path_vars (pretty_printer *pp,
					     auto_vec<path_var> *rep_path_vars,
					     bool *is_first)
{
  /* Print pointers, constants, and poisoned values that aren't "uninit";
     gather keys for unknown and uninit values.  */
  unsigned i;
  path_var *pv;
  auto_vec<tree> unknown_trees;
  auto_vec<tree> uninit_trees;
  FOR_EACH_VEC_ELT (*rep_path_vars, i, pv)
    {
      if (TREE_CODE (pv->m_tree) == STRING_CST)
	continue;
      tentative_region_model_context ctxt;
      region_id child_rid = get_lvalue (*pv, &ctxt);
      if (ctxt.had_errors_p ())
	continue;
      region *child_region = get_region (child_rid);
      if (!child_region)
	continue;
      svalue_id sid = child_region->get_value_direct ();
      if (sid.null_p ())
	continue;
      svalue *sval = get_svalue (sid);
      switch (sval->get_kind ())
	{
	default:
	  gcc_unreachable ();
	case SK_REGION:
	  {
	    region_svalue *region_sval = as_a <region_svalue *> (sval);
	    region_id pointee_rid = region_sval->get_pointee ();
	    gcc_assert (!pointee_rid.null_p ());
	    tree pointee = get_representative_path_var (pointee_rid).m_tree;
	    dump_separator (pp, is_first);
	    dump_tree (pp, pv->m_tree);
	    pp_string (pp, ": ");
	    pp_character (pp, '&');
	    if (pointee)
	      dump_tree (pp, pointee);
	    else
	      pointee_rid.print (pp);
	  }
	  break;
	case SK_CONSTANT:
	  dump_separator (pp, is_first);
	  dump_tree (pp, pv->m_tree);
	  pp_string (pp, ": ");
	  dump_tree (pp, sval->dyn_cast_constant_svalue ()->get_constant ());
	  break;
	case SK_UNKNOWN:
	  unknown_trees.safe_push (pv->m_tree);
	  break;
	case SK_POISONED:
	  {
	    poisoned_svalue *poisoned_sval = as_a <poisoned_svalue *> (sval);
	    enum poison_kind pkind = poisoned_sval->get_poison_kind ();
	    if (pkind == POISON_KIND_UNINIT)
	      uninit_trees.safe_push (pv->m_tree);
	    else
	      {
		dump_separator (pp, is_first);
		dump_tree (pp, pv->m_tree);
		pp_printf (pp, ": %s", poison_kind_to_str (pkind));
	      }
	  }
	  break;
	case SK_SETJMP:
	  dump_separator (pp, is_first);
	  pp_printf (pp, "setjmp: EN: %i",
		     sval->dyn_cast_setjmp_svalue ()->get_enode_index ());
	  break;
	}
    }

  /* Print unknown and uninitialized values in consolidated form.  */
  dump_vec_of_tree (pp, is_first, unknown_trees, "unknown");
  dump_vec_of_tree (pp, is_first, uninit_trees, "uninit");
}

/* Assert that this object is valid.  */

void
region_model::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  m_constraints->validate ();

  unsigned i;
  region *r;
  FOR_EACH_VEC_ELT (m_regions, i, r)
    r->validate (*this);

  // TODO: anything else?

  /* Verify that the stack region (if any) has an "uninitialized" value.  */
  region *stack_region = get_root_region ()->get_stack_region (this);
  if (stack_region)
    {
      svalue_id stack_value_sid = stack_region->get_value_direct ();
      svalue *stack_value = get_svalue (stack_value_sid);
      gcc_assert (stack_value->get_kind () == SK_POISONED);
      poisoned_svalue *subclass = stack_value->dyn_cast_poisoned_svalue ();
      gcc_assert (subclass);
      gcc_assert (subclass->get_poison_kind () == POISON_KIND_UNINIT);
    }
}

/* Global data for use by svalue_id_cmp_by_constant_svalue.  */

static region_model *svalue_id_cmp_by_constant_svalue_model = NULL;

/* Comparator for use by region_model::canonicalize.  */

static int
svalue_id_cmp_by_constant_svalue (const void *p1, const void *p2)
{
  const svalue_id *sid1 = (const svalue_id *)p1;
  const svalue_id *sid2 = (const svalue_id *)p2;
  gcc_assert (!sid1->null_p ());
  gcc_assert (!sid2->null_p ());
  gcc_assert (svalue_id_cmp_by_constant_svalue_model);
  const svalue &sval1
    = *svalue_id_cmp_by_constant_svalue_model->get_svalue (*sid1);
  const svalue &sval2
    = *svalue_id_cmp_by_constant_svalue_model->get_svalue (*sid2);
  gcc_assert (sval1.get_kind () == SK_CONSTANT);
  gcc_assert (sval2.get_kind () == SK_CONSTANT);

  tree cst1 = ((const constant_svalue &)sval1).get_constant ();
  tree cst2 = ((const constant_svalue &)sval2).get_constant ();
  return tree_cmp (cst1, cst2);
}

/* Reorder the regions and svalues into a deterministic "canonical" order,
   to maximize the chance of equality.
   If non-NULL, notify CTXT about the svalue id remapping.  */

void
region_model::canonicalize (region_model_context *ctxt)
{
  /* Walk all regions and values in a deterministic order, visiting
     rids and sids, generating a rid and sid map.  */
  canonicalization c (*this);

  /* (1): Walk all svalues, putting constants first, sorting the constants
     (thus imposing an ordering on any constants that are purely referenced
     by constraints).
     Ignore other svalues for now.  */
  {
    unsigned i;
    auto_vec<svalue_id> sids;
    svalue *sval;
    FOR_EACH_VEC_ELT (m_svalues, i, sval)
      {
	if (sval->get_kind () == SK_CONSTANT)
	  sids.safe_push (svalue_id::from_int (i));
      }
    svalue_id_cmp_by_constant_svalue_model = this;
    sids.qsort (svalue_id_cmp_by_constant_svalue);
    svalue_id_cmp_by_constant_svalue_model = NULL;
    svalue_id *sid;
    FOR_EACH_VEC_ELT (sids, i, sid)
      c.walk_sid (*sid);
  }

  /* (2): Walk all regions (and thus their values) in a deterministic
     order.  */
  c.walk_rid (m_root_rid);

  /* (3): Ensure we've visited everything, as we don't want to purge
     at this stage.  Anything we visit for the first time here has
     arbitrary order.  */
  {
    unsigned i;
    region *region;
    FOR_EACH_VEC_ELT (m_regions, i, region)
      c.walk_rid (region_id::from_int (i));
    svalue *sval;
    FOR_EACH_VEC_ELT (m_svalues, i, sval)
      c.walk_sid (svalue_id::from_int (i));
  }

  /* (4): We now have a reordering of the regions and values.
     Apply it.  */
  remap_svalue_ids (c.m_sid_map);
  remap_region_ids (c.m_rid_map);
  if (ctxt)
    ctxt->remap_svalue_ids (c.m_sid_map);

  /* (5): Canonicalize the constraint_manager (it has already had its
     svalue_ids remapped above).  This makes use of the new svalue_id
     values, and so must happen last.  */
  m_constraints->canonicalize (get_num_svalues ());

  validate ();
}

/* Return true if this region_model is in canonical form.  */

bool
region_model::canonicalized_p () const
{
  region_model copy (*this);
  copy.canonicalize (NULL);
  return *this == copy;
}

/* A subclass of pending_diagnostic for complaining about uses of
   poisoned values.  */

class poisoned_value_diagnostic
: public pending_diagnostic_subclass<poisoned_value_diagnostic>
{
public:
  poisoned_value_diagnostic (tree expr, enum poison_kind pkind)
  : m_expr (expr), m_pkind (pkind)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "poisoned_value_diagnostic"; }

  bool operator== (const poisoned_value_diagnostic &other) const
  {
    return m_expr == other.m_expr;
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_UNINIT:
	{
	  diagnostic_metadata m;
	  m.add_cwe (457); /* "CWE-457: Use of Uninitialized Variable".  */
	  return warning_meta (rich_loc, m,
			       OPT_Wanalyzer_use_of_uninitialized_value,
			       "use of uninitialized value %qE",
			       m_expr);
	}
	break;
      case POISON_KIND_FREED:
	{
	  diagnostic_metadata m;
	  m.add_cwe (416); /* "CWE-416: Use After Free".  */
	  return warning_meta (rich_loc, m,
			       OPT_Wanalyzer_use_after_free,
			       "use after %<free%> of %qE",
			       m_expr);
	}
	break;
      case POISON_KIND_POPPED_STACK:
	{
	  /* TODO: which CWE?  */
	  return warning_at (rich_loc,
			     OPT_Wanalyzer_use_of_pointer_in_stale_stack_frame,
			     "use of pointer %qE within stale stack frame",
			     m_expr);
	}
	break;
      }
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    switch (m_pkind)
      {
      default:
	gcc_unreachable ();
      case POISON_KIND_UNINIT:
	return ev.formatted_print ("use of uninitialized value %qE here",
				   m_expr);
      case POISON_KIND_FREED:
	return ev.formatted_print ("use after %<free%> of %qE here",
				   m_expr);
      case POISON_KIND_POPPED_STACK:
	return ev.formatted_print
	  ("use of pointer %qE within stale stack frame here",
	   m_expr);
      }
  }

private:
  tree m_expr;
  enum poison_kind m_pkind;
};

/* Determine if EXPR is poisoned, and if so, queue a diagnostic to CTXT.  */

void
region_model::check_for_poison (tree expr, region_model_context *ctxt)
{
  if (!ctxt)
    return;

  // TODO: this is disabled for now (too many false positives)
  return;

  svalue_id expr_sid = get_rvalue (expr, ctxt);
  gcc_assert (!expr_sid.null_p ());
  svalue *expr_svalue = get_svalue (expr_sid);
  gcc_assert (expr_svalue);
  if (const poisoned_svalue *poisoned_sval
	= expr_svalue->dyn_cast_poisoned_svalue ())
    {
      enum poison_kind pkind = poisoned_sval->get_poison_kind ();
      ctxt->warn (new poisoned_value_diagnostic (expr, pkind));
    }
}

/* Update this model for the ASSIGN stmt, using CTXT to report any
   diagnostics.  */

void
region_model::on_assignment (const gassign *assign, region_model_context *ctxt)
{
  tree lhs = gimple_assign_lhs (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);

  region_id lhs_rid = get_lvalue (lhs, ctxt);

  /* Check for uses of poisoned values.  */
  switch (get_gimple_rhs_class (gimple_expr_code (assign)))
    {
    case GIMPLE_INVALID_RHS:
      gcc_unreachable ();
      break;
    case GIMPLE_TERNARY_RHS:
      check_for_poison (gimple_assign_rhs3 (assign), ctxt);
      /* Fallthru */
    case GIMPLE_BINARY_RHS:
      check_for_poison (gimple_assign_rhs2 (assign), ctxt);
      /* Fallthru */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
      check_for_poison (gimple_assign_rhs1 (assign), ctxt);
    }

  if (lhs_rid.null_p ())
    return;
  // TODO: issue a warning for this case

  enum tree_code op = gimple_assign_rhs_code (assign);
  switch (op)
    {
    default:
      {
	if (0)
	  sorry_at (assign->location, "unhandled assignment op: %qs",
		    get_tree_code_name (op));
	set_to_new_unknown_value (lhs_rid, TREE_TYPE (lhs), ctxt);
      }
      break;

    case BIT_FIELD_REF:
      {
	// TODO
      }
      break;

    case CONSTRUCTOR:
      {
	/* e.g. "x ={v} {CLOBBER};"  */
	// TODO
      }
      break;

    case POINTER_PLUS_EXPR:
      {
	/* e.g. "_1 = a_10(D) + 12;" */
	tree ptr = rhs1;
	tree offset = gimple_assign_rhs2 (assign);

	svalue_id ptr_sid = get_rvalue (ptr, ctxt);
	svalue_id offset_sid = get_rvalue (offset, ctxt);
	region_id element_rid
	  = get_or_create_pointer_plus_expr (TREE_TYPE (TREE_TYPE (ptr)),
					     ptr_sid, offset_sid,
					     ctxt);
	svalue_id element_ptr_sid
	  = get_or_create_ptr_svalue (TREE_TYPE (ptr), element_rid);
	set_value (lhs_rid, element_ptr_sid, ctxt);
      }
      break;

    case POINTER_DIFF_EXPR:
      {
	/* e.g. "_1 = p_2(D) - q_3(D);".  */

	/* TODO.  */

	set_to_new_unknown_value (lhs_rid, TREE_TYPE (lhs), ctxt);
      }
      break;

    case ADDR_EXPR:
      {
	/* LHS = &RHS;  */
	svalue_id ptr_sid = get_rvalue (rhs1, ctxt);
	set_value (lhs_rid, ptr_sid, ctxt);
      }
      break;

    case MEM_REF:
      {
	region_id rhs_rid = get_lvalue (rhs1, ctxt);
	svalue_id rhs_sid
	  = get_region (rhs_rid)->get_value (*this, true, ctxt);
	set_value (lhs_rid, rhs_sid, ctxt);
      }
      break;

    case REAL_CST:
    case INTEGER_CST:
    case ARRAY_REF:
      {
	/* LHS = RHS;  */
	svalue_id cst_sid = get_rvalue (rhs1, ctxt);
	set_value (lhs_rid, cst_sid, ctxt);
      }
      break;

    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NOP_EXPR:
      // cast: TODO
      // fall though for now
    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
      {
	/* LHS = VAR;  */
	region_id rhs_rid = get_lvalue (rhs1, ctxt);
	copy_region (lhs_rid, rhs_rid, ctxt);
      }
      break;

    case EQ_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case NE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
      {
	tree rhs2 = gimple_assign_rhs2 (assign);

	// TODO: constraints between svalues
	svalue_id rhs1_sid = get_rvalue (rhs1, ctxt);
	svalue_id rhs2_sid = get_rvalue (rhs2, ctxt);

	tristate t = eval_condition (rhs1_sid, op, rhs2_sid);
	if (t.is_known ())
	  set_value (lhs_rid,
		     get_rvalue (t.is_true ()
				 ? boolean_true_node
				 : boolean_false_node,
				 ctxt),
		     ctxt);
	else
	  set_to_new_unknown_value (lhs_rid, TREE_TYPE (lhs), ctxt);
      }
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      {
	// TODO: unary ops

	// TODO: constant?

	set_to_new_unknown_value (lhs_rid, TREE_TYPE (lhs), ctxt);
      }
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      {
	/* Binary ops.  */
	tree rhs2 = gimple_assign_rhs2 (assign);

	svalue_id rhs1_sid = get_rvalue (rhs1, ctxt);
	svalue_id rhs2_sid = get_rvalue (rhs2, ctxt);

	if (tree rhs1_cst = maybe_get_constant (rhs1_sid))
	  if (tree rhs2_cst = maybe_get_constant (rhs2_sid))
	    {
	      tree result = fold_binary (op, TREE_TYPE (lhs),
					 rhs1_cst, rhs2_cst);
	      if (result && CONSTANT_CLASS_P (result))
		{
		  svalue_id result_sid
		    = get_or_create_constant_svalue (result);
		  set_value (lhs_rid, result_sid, ctxt);
		  return;
		}
	    }
	set_to_new_unknown_value (lhs_rid, TREE_TYPE (lhs), ctxt);
      }
      break;

    case COMPONENT_REF:
      {
	/* LHS = op0.op1;  */
	region_id child_rid = get_lvalue (rhs1, ctxt);
	svalue_id child_sid
	  = get_region (child_rid)->get_value (*this, true, ctxt);
	set_value (lhs_rid, child_sid, ctxt);
      }
      break;
    }
}

/* Update this model for the CALL stmt, using CTXT to report any
   diagnostics - the first half.

   Updates to the region_model that should be made *before* sm-states
   are updated are done here; other updates to the region_model are done
   in region_model::on_call_post.

   Return true if the function call has unknown side effects (it wasn't
   recognized and we don't have a body for it, or are unable to tell which
   fndecl it is).  */

bool
region_model::on_call_pre (const gcall *call, region_model_context *ctxt)
{
  region_id lhs_rid;
  tree lhs_type = NULL_TREE;
  if (tree lhs = gimple_call_lhs (call))
    {
      lhs_rid = get_lvalue (lhs, ctxt);
      lhs_type = TREE_TYPE (lhs);
    }

  /* Check for uses of poisoned values.
     For now, special-case "free", to avoid warning about "use-after-free"
     when "double free" would be more precise.  */
  if (!is_special_named_call_p (call, "free", 1))
    for (unsigned i = 0; i < gimple_call_num_args (call); i++)
      check_for_poison (gimple_call_arg (call, i), ctxt);

  bool unknown_side_effects = false;

  if (tree callee_fndecl = get_fndecl_for_call (call, ctxt))
    {
      if (is_named_call_p (callee_fndecl, "malloc", call, 1))
	{
	  // TODO: capture size as a svalue?
	  region_id new_rid = add_new_malloc_region ();
	  if (!lhs_rid.null_p ())
	    {
	      svalue_id ptr_sid
		= get_or_create_ptr_svalue (lhs_type, new_rid);
	      set_value (lhs_rid, ptr_sid, ctxt);
	    }
	  return false;
	}
      else if (is_named_call_p (callee_fndecl, "__builtin_alloca", call, 1))
	{
	  region_id frame_rid = get_current_frame_id ();
	  region_id new_rid
	    = add_region (new symbolic_region (frame_rid, NULL_TREE, false));
	  if (!lhs_rid.null_p ())
	    {
	      svalue_id ptr_sid
		= get_or_create_ptr_svalue (lhs_type, new_rid);
	      set_value (lhs_rid, ptr_sid, ctxt);
	    }
	  return false;
	}
      else if (gimple_call_builtin_p (call, BUILT_IN_EXPECT)
	       || gimple_call_builtin_p (call, BUILT_IN_EXPECT_WITH_PROBABILITY)
	       || gimple_call_internal_p (call, IFN_BUILTIN_EXPECT))
	{
	  /* __builtin_expect's return value is its initial argument.  */
	  if (!lhs_rid.null_p ())
	    {
	      tree initial_arg = gimple_call_arg (call, 0);
	      svalue_id sid = get_rvalue (initial_arg, ctxt);
	      set_value (lhs_rid, sid, ctxt);
	    }
	  return false;
	}
      else if (is_named_call_p (callee_fndecl, "strlen", call, 1))
	{
	  region_id buf_rid = deref_rvalue (gimple_call_arg (call, 0), ctxt);
	  svalue_id buf_sid
	    = get_region (buf_rid)->get_value (*this, true, ctxt);
	  if (tree cst_expr = maybe_get_constant (buf_sid))
	    {
	      if (TREE_CODE (cst_expr) == STRING_CST
		  && !lhs_rid.null_p ())
		{
		  /* TREE_STRING_LENGTH is sizeof, not strlen.  */
		  int sizeof_cst = TREE_STRING_LENGTH (cst_expr);
		  int strlen_cst = sizeof_cst - 1;
		  tree t_cst = build_int_cst (lhs_type, strlen_cst);
		  svalue_id result_sid
		    = get_or_create_constant_svalue (t_cst);
		  set_value (lhs_rid, result_sid, ctxt);
		  return false;
		}
	    }
	  /* Otherwise an unknown value.  */
	}
      else if (is_named_call_p (callee_fndecl,
				"__analyzer_dump_num_heap_regions", call, 0))
	{
	  /* Handle the builtin "__analyzer_dump_num_heap_regions" by emitting
	     a warning (for use in DejaGnu tests).  */
	  int num_heap_regions = 0;
	  region_id heap_rid = get_root_region ()->ensure_heap_region (this);
	  unsigned i;
	  region *region;
	  FOR_EACH_VEC_ELT (m_regions, i, region)
	    if (region->get_parent () == heap_rid)
	      num_heap_regions++;
	  /* Use quotes to ensure the output isn't truncated.  */
	  warning_at (call->location, 0,
		      "num heap regions: %qi", num_heap_regions);
	  return false;
	}
      else if (!fndecl_has_gimple_body_p (callee_fndecl)
	       && !DECL_PURE_P (callee_fndecl))
	unknown_side_effects = true;
    }
  else
    unknown_side_effects = true;

  /* Unknown return value.  */
  if (!lhs_rid.null_p ())
    set_to_new_unknown_value (lhs_rid, lhs_type, ctxt);

  return unknown_side_effects;
}

/* Update this model for the CALL stmt, using CTXT to report any
   diagnostics - the second half.

   Updates to the region_model that should be made *after* sm-states
   are updated are done here; other updates to the region_model are done
   in region_model::on_call_pre.

   If UNKNOWN_SIDE_EFFECTS is true, also call handle_unrecognized_call
   to purge state.  */

void
region_model::on_call_post (const gcall *call,
			    bool unknown_side_effects,
			    region_model_context *ctxt)
{
  /* Update for "free" here, after sm-handling.

     If the ptr points to an underlying heap region, delete the region,
     poisoning pointers to it and regions within it.

     We delay this until after sm-state has been updated so that the
     sm-handling can transition all of the various casts of the pointer
     to a "freed" state *before* we delete the related region here.

     This has to be done here so that the sm-handling can use the fact
     that they point to the same region to establish that they are equal
     (in region_model::eval_condition_without_cm), and thus transition
     all pointers to the region to the "freed" state together, regardless
     of casts.  */
  if (tree callee_fndecl = get_fndecl_for_call (call, ctxt))
    if (is_named_call_p (callee_fndecl, "free", call, 1))
      {
	tree ptr = gimple_call_arg (call, 0);
	svalue_id ptr_sid = get_rvalue (ptr, ctxt);
	svalue *ptr_sval = get_svalue (ptr_sid);
	if (region_svalue *ptr_to_region_sval
	    = ptr_sval->dyn_cast_region_svalue ())
	  {
	    /* If the ptr points to an underlying heap region, delete it,
	       poisoning pointers.  */
	    region_id pointee_rid = ptr_to_region_sval->get_pointee ();
	    region_id heap_rid = get_root_region ()->ensure_heap_region (this);
	    if (!pointee_rid.null_p ()
		&& get_region (pointee_rid)->get_parent () == heap_rid)
	      {
		purge_stats stats;
		delete_region_and_descendents (pointee_rid,
					       POISON_KIND_FREED,
					       &stats, ctxt->get_logger ());
		purge_unused_svalues (&stats, ctxt);
		validate ();
		// TODO: do anything with stats?
	      }
	  }
	return;
      }

  if (unknown_side_effects)
    handle_unrecognized_call (call, ctxt);
}

/* Helper class for region_model::handle_unrecognized_call, for keeping
   track of all regions that are reachable, and, of those, which are
   mutable.  */

class reachable_regions
{
public:
  reachable_regions (region_model *model)
  : m_model (model), m_reachable_rids (), m_mutable_rids ()
  {}

  /* Lazily mark RID as being reachable, recursively adding regions
     reachable from RID.  */
  void add (region_id rid, bool is_mutable)
  {
    gcc_assert (!rid.null_p ());

    unsigned idx = rid.as_int ();
    /* Bail out if this region is already in the sets at the IS_MUTABLE
       level of mutability.  */
    if (!is_mutable && bitmap_bit_p (m_reachable_rids, idx))
      return;
    bitmap_set_bit (m_reachable_rids, idx);

    if (is_mutable)
      {
	if (bitmap_bit_p (m_mutable_rids, idx))
	  return;
	else
	  bitmap_set_bit (m_mutable_rids, idx);
      }

    /* If this region's value is a pointer, add the pointee.  */
    region *reg = m_model->get_region (rid);
    svalue_id sid = reg->get_value_direct ();
    svalue *sval = m_model->get_svalue (sid);
    if (sval)
      if (region_svalue *ptr = sval->dyn_cast_region_svalue ())
	{
	  region_id pointee_rid = ptr->get_pointee ();
	  /* Use const-ness of pointer type to affect mutability.  */
	  bool ptr_is_mutable = true;
	  if (ptr->get_type ()
	      && TREE_CODE (ptr->get_type ()) == POINTER_TYPE
	      && TYPE_READONLY (TREE_TYPE (ptr->get_type ())))
	    ptr_is_mutable = false;
	  add (pointee_rid, ptr_is_mutable);
	}

    /* Add descendents of this region.  */
    region_id_set descendents (m_model);
    m_model->get_descendents (rid, &descendents, region_id::null ());
    for (unsigned i = 0; i < m_model->get_num_regions (); i++)
      {
	region_id iter_rid = region_id::from_int (i);
	if (descendents.region_p (iter_rid))
	  add (iter_rid, is_mutable);
      }
  }

  bool mutable_p (region_id rid)
  {
    gcc_assert (!rid.null_p ());
    return bitmap_bit_p (m_mutable_rids, rid.as_int ());
  }

private:
  region_model *m_model;

  /* The region ids already seen.  This has to be an auto_bitmap rather than
     an auto_sbitmap as new regions can be created within the model during
     the traversal.  */
  auto_bitmap m_reachable_rids;

  /* The region_ids that can be changed (accessed via non-const pointers).  */
  auto_bitmap m_mutable_rids;
};

/* Handle a call CALL to a function with unknown behavior.

   Traverse the regions in this model, determining what regions are
   reachable from pointer arguments to CALL and from global variables,
   recursively.

   Set all reachable regions to new unknown values and purge sm-state
   from their values, and from values that point to them.  */

void
region_model::handle_unrecognized_call (const gcall *call,
					region_model_context *ctxt)
{
  tree fndecl = get_fndecl_for_call (call, ctxt);

  reachable_regions reachable_regions (this);

  /* Determine the reachable regions and their mutability.  */
  {
    /* Globals.  */
    region_id globals_rid = get_globals_region_id ();
    if (!globals_rid.null_p ())
      reachable_regions.add (globals_rid, true);

    /* Params that are pointers.  */
    tree iter_param_types = NULL_TREE;
    if (fndecl)
      iter_param_types = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
    for (unsigned arg_idx = 0; arg_idx < gimple_call_num_args (call); arg_idx++)
      {
	/* Track expected param type, where available.  */
	tree param_type = NULL_TREE;
	if (iter_param_types)
	  {
	    param_type = TREE_VALUE (iter_param_types);
	    gcc_assert (param_type);
	    iter_param_types = TREE_CHAIN (iter_param_types);
	  }

	tree parm = gimple_call_arg (call, arg_idx);
	svalue_id parm_sid = get_rvalue (parm, ctxt);
	svalue *parm_sval = get_svalue (parm_sid);
	if (parm_sval)
	  if (region_svalue *parm_ptr = parm_sval->dyn_cast_region_svalue ())
	    {
	      region_id pointee_rid = parm_ptr->get_pointee ();
	      bool is_mutable = true;
	      if (param_type
		  && TREE_CODE (param_type) == POINTER_TYPE
		  &&  TYPE_READONLY (TREE_TYPE (param_type)))
		is_mutable = false;
	      reachable_regions.add (pointee_rid, is_mutable);
	    }
	// FIXME: what about compound parms that contain ptrs?
      }
  }

  /* OK: we now have all reachable regions.
     Set them all to new unknown values.  */
  for (unsigned i = 0; i < get_num_regions (); i++)
    {
      region_id iter_rid = region_id::from_int (i);
      if (reachable_regions.mutable_p (iter_rid))
	{
	  region *reg = get_region (iter_rid);

	  /* Purge any sm-state for any underlying svalue.  */
	  svalue_id curr_sid = reg->get_value_direct ();
	  if (!curr_sid.null_p ())
	    ctxt->on_unknown_change (curr_sid);

	  set_to_new_unknown_value (iter_rid,
				    reg->get_type (),
				    ctxt);
	}
    }

  /* Purge sm-state for any remaining svalues that point to regions that
     were reachable.  This helps suppress leak false-positives.

     For example, if we had a malloc call that was cast to a "foo *" type,
     we could have a temporary void * for the result of malloc which has its
     own svalue, not reachable from the function call, but for which the
     "foo *" svalue was reachable.  If we don't purge it, the temporary will
     be reported as a leak.  */
  int i;
  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    if (region_svalue *ptr = svalue->dyn_cast_region_svalue ())
      {
	region_id pointee_rid = ptr->get_pointee ();
	if (reachable_regions.mutable_p (pointee_rid))
	  ctxt->on_unknown_change (svalue_id::from_int (i));
      }

  validate ();
}

/* Update this model for the RETURN_STMT, using CTXT to report any
   diagnostics.  */

void
region_model::on_return (const greturn *return_stmt, region_model_context *ctxt)
{
  tree callee = get_current_function ()->decl;
  tree lhs = DECL_RESULT (callee);
  tree rhs = gimple_return_retval (return_stmt);

  if (lhs && rhs)
    copy_region (get_lvalue (lhs, ctxt), get_lvalue (rhs, ctxt), ctxt);
}

/* Update this model for a call and return of setjmp/sigsetjmp at CALL within
   ENODE, using CTXT to report any diagnostics.

   This is for the initial direct invocation of setjmp/sigsetjmp (which returns
   0), as opposed to any second return due to longjmp/sigsetjmp.  */

void
region_model::on_setjmp (const gcall *call, const exploded_node *enode,
			 region_model_context *ctxt)
{
  region_id buf_rid = deref_rvalue (gimple_call_arg (call, 0), ctxt);
  region *buf = get_region (buf_rid);

  /* Create a setjmp_svalue for this call and store it in BUF_RID's region.  */
  if (buf)
    {
      setjmp_record r (enode, call);
      svalue *sval = new setjmp_svalue (r, buf->get_type ());
      svalue_id new_sid = add_svalue (sval);
      set_value (buf_rid, new_sid, ctxt);
    }

  /* Direct calls to setjmp return 0.  */
  if (tree lhs = gimple_call_lhs (call))
    {
      tree zero = build_int_cst (TREE_TYPE (lhs), 0);
      svalue_id new_sid = get_or_create_constant_svalue (zero);
      region_id lhs_rid = get_lvalue (lhs, ctxt);
      set_value (lhs_rid, new_sid, ctxt);
    }
}

/* Update this region_model for rewinding from a "longjmp" at LONGJMP_CALL
   to a "setjmp" at SETJMP_CALL where the final stack depth should be
   SETJMP_STACK_DEPTH.  Purge any stack frames, potentially reporting on
   leaks to CTXT.  */

void
region_model::on_longjmp (const gcall *longjmp_call, const gcall *setjmp_call,
			  int setjmp_stack_depth,
			  region_model_context *ctxt)
{
  /* Evaluate the val, using the frame of the "longjmp".  */
  tree fake_retval = gimple_call_arg (longjmp_call, 1);
  svalue_id fake_retval_sid = get_rvalue (fake_retval, ctxt);

  /* Pop any frames until we reach the stack depth of the function where
     setjmp was called.  */
  gcc_assert (get_stack_depth () >= setjmp_stack_depth);
  while (get_stack_depth () > setjmp_stack_depth)
    {
      /* Don't purge unused svalues yet, as we're using fake_retval_sid.  */
      pop_frame (region_id::null (), false, NULL, ctxt);
    }

  gcc_assert (get_stack_depth () == setjmp_stack_depth);

  /* Assign to LHS of "setjmp" in new_state.  */
  if (tree lhs = gimple_call_lhs (setjmp_call))
    {
      /* Passing 0 as the val to longjmp leads to setjmp returning 1.  */
      tree t_zero = build_int_cst (TREE_TYPE (fake_retval), 0);
      svalue_id zero_sid = get_or_create_constant_svalue (t_zero);
      tristate eq_zero = eval_condition (fake_retval_sid, EQ_EXPR, zero_sid);
      /* If we have 0, use 1.  */
      if (eq_zero.is_true ())
	{
	  tree t_one = build_int_cst (TREE_TYPE (fake_retval), 1);
	  svalue_id one_sid = get_or_create_constant_svalue (t_one);
	  fake_retval_sid = one_sid;
	}
      else
	{
	  /* Otherwise note that the value is nonzero.  */
	  m_constraints->add_constraint (fake_retval_sid, NE_EXPR, zero_sid);
	}

      region_id lhs_rid = get_lvalue (lhs, ctxt);
      set_value (lhs_rid, fake_retval_sid, ctxt);
    }

  /* Now that we've assigned the fake_retval, we can purge the unused
     svalues, which could detect leaks.  */
  purge_unused_svalues (NULL, ctxt, NULL);
  validate ();
}

/* Update this region_model for a phi stmt of the form
     LHS = PHI <...RHS...>.
   where RHS is for the appropriate edge.  */

void
region_model::handle_phi (const gphi *phi,
			  tree lhs, tree rhs, bool is_back_edge,
			  region_model_context *ctxt)
{
  /* For now, don't bother tracking the .MEM SSA names.  */
  if (tree var = SSA_NAME_VAR (lhs))
    if (TREE_CODE (var) == VAR_DECL)
      if (VAR_DECL_IS_VIRTUAL_OPERAND (var))
	return;

  svalue_id rhs_sid = get_rvalue (rhs, ctxt);

  if (is_back_edge && get_svalue (rhs_sid)->get_kind () != SK_UNKNOWN)
    {
      /* If we have a back edge, we probably have a loop.
	 Use an unknown value, to avoid effectively unrolling the
	 loop.
	 To terminate, we need to avoid generating a series of
	 models with an unbounded monotonically increasing number of
	 redundant unknown values; hence we need to purge svalues
	 before inserting the state into the exploded graph, to
	 collect unused svalues.  */
      set_to_new_unknown_value (get_lvalue (lhs, ctxt), TREE_TYPE (lhs), ctxt);
    }
  else
    set_value (get_lvalue (lhs, ctxt), rhs_sid, ctxt);

  if (ctxt)
    ctxt->on_phi (phi, rhs);
}

/* Implementation of region_model::get_lvalue; the latter adds type-checking.

   Get the id of the region for PV within this region_model,
   emitting any diagnostics to CTXT.  */

region_id
region_model::get_lvalue_1 (path_var pv, region_model_context *ctxt)
{
  tree expr = pv.m_tree;

  gcc_assert (expr);

  switch (TREE_CODE (expr))
    {
    default:
      return make_region_for_unexpected_tree_code (ctxt, expr,
						   dump_location_t ());

    case ARRAY_REF:
      {
	tree array = TREE_OPERAND (expr, 0);
	tree index = TREE_OPERAND (expr, 1);
#if 0
	// TODO: operands 2 and 3, if present:
	gcc_assert (TREE_OPERAND (expr, 2) == NULL_TREE);
	gcc_assert (TREE_OPERAND (expr, 3) == NULL_TREE);
#endif

	region_id array_rid = get_lvalue (array, ctxt);
	svalue_id index_sid = get_rvalue (index, ctxt);
	region *base_array_reg = get_region (array_rid);
	array_region *array_reg  = base_array_reg->dyn_cast_array_region ();
	if (!array_reg)
	  {
	    /* Normally, array_rid ought to refer to an array_region, since
	       array's type will be ARRAY_TYPE.  However, if we have an
	       unexpected tree code for array, we could have a
	       symbolic_region here.  If so, we're in error-handling. */
	    gcc_assert (base_array_reg->get_type () == NULL_TREE);
	    return make_region_for_unexpected_tree_code (ctxt, expr,
							 dump_location_t ());
	  }
	return array_reg->get_element (this, array_rid, index_sid, ctxt);
      }
      break;

    case BIT_FIELD_REF:
      {
	/* For now, create a view, as if a cast, ignoring the bit positions.  */
	tree obj = TREE_OPERAND (expr, 0);
	return get_or_create_view (get_lvalue (obj, ctxt), TREE_TYPE (expr),
				   ctxt);
      };
      break;

    case MEM_REF:
      {
	tree ptr = TREE_OPERAND (expr, 0);
	tree offset = TREE_OPERAND (expr, 1);
	svalue_id ptr_sid = get_rvalue (ptr, ctxt);
	svalue_id offset_sid = get_rvalue (offset, ctxt);
	return get_or_create_mem_ref (TREE_TYPE (expr), ptr_sid,
				      offset_sid, ctxt);
      }
      break;

    case VAR_DECL:
      /* Handle globals.  */
      if (is_global_var (expr))
	{
	  region_id globals_rid
	    = get_root_region ()->ensure_globals_region (this);
	  map_region *globals = get_region<map_region> (globals_rid);
	  region_id var_rid = globals->get_or_create (this, globals_rid, expr,
						      TREE_TYPE (expr), ctxt);
	  return var_rid;
	}

      /* Fall through.  */

    case SSA_NAME:
    case PARM_DECL:
    case RESULT_DECL:
      {
	gcc_assert (TREE_CODE (expr) == SSA_NAME
		    || TREE_CODE (expr) == PARM_DECL
		    || TREE_CODE (expr) == VAR_DECL
		    || TREE_CODE (expr) == RESULT_DECL);

	int stack_depth = pv.m_stack_depth;
	stack_region *stack = get_root_region ()->get_stack_region (this);
	gcc_assert (stack);
	region_id frame_rid = stack->get_frame_rid (stack_depth);
	frame_region *frame = get_region <frame_region> (frame_rid);
	gcc_assert (frame);
	region_id child_rid = frame->get_or_create (this, frame_rid, expr,
						    TREE_TYPE (expr), ctxt);
	return child_rid;
      }

    case COMPONENT_REF:
      {
	/* obj.field  */
	tree obj = TREE_OPERAND (expr, 0);
	tree field = TREE_OPERAND (expr, 1);
	tree obj_type = TREE_TYPE (obj);
	if (TREE_CODE (obj_type) != RECORD_TYPE
	    && TREE_CODE (obj_type) != UNION_TYPE)
	  return make_region_for_unexpected_tree_code (ctxt, obj_type,
						       dump_location_t ());
	region_id obj_rid = get_lvalue (obj, ctxt);
	region_id struct_or_union_rid
	  = get_or_create_view (obj_rid, TREE_TYPE (obj), ctxt);
	return get_field_region (struct_or_union_rid, field, ctxt);
      }
      break;

    case CONST_DECL:
      {
	tree cst_type = TREE_TYPE (expr);
	region_id cst_rid = add_region_for_type (m_root_rid, cst_type, ctxt);
	if (tree value = DECL_INITIAL (expr))
	  {
	    svalue_id sid = get_rvalue (value, ctxt);
	    get_region (cst_rid)->set_value (*this, cst_rid, sid, ctxt);
	  }
	return cst_rid;
      }
      break;

    case STRING_CST:
      {
	tree cst_type = TREE_TYPE (expr);
	array_region *cst_region = new array_region (m_root_rid, cst_type);
	region_id cst_rid = add_region (cst_region);
	svalue_id cst_sid = get_or_create_constant_svalue (expr);
	cst_region->set_value (*this, cst_rid, cst_sid, ctxt);
	return cst_rid;
      }
      break;

    case NOP_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	tree obj = TREE_OPERAND (expr, 0);
	return get_or_create_view (get_lvalue (obj, ctxt), TREE_TYPE (expr),
				   ctxt);
      };
      break;
    }
}

/* If we see a tree code we don't know how to handle, rather than
   ICE or generate bogus results, create a dummy region, and notify
   CTXT so that it can mark the new state as being not properly
   modelled.  The exploded graph can then stop exploring that path,
   since any diagnostics we might issue will have questionable
   validity.  */

region_id
region_model::make_region_for_unexpected_tree_code (region_model_context *ctxt,
						    tree t,
						    const dump_location_t &loc)
{
  gcc_assert (ctxt);
  region_id new_rid
    = add_region (new symbolic_region (m_root_rid, NULL_TREE, false));
  ctxt->on_unexpected_tree_code (t, loc);
  return new_rid;
}

/* Assert that SRC_TYPE can be converted to DST_TYPE as a no-op.  */

static void
assert_compat_types (tree src_type, tree dst_type)
{
  if (src_type && dst_type && !VOID_TYPE_P (dst_type))
    gcc_checking_assert (useless_type_conversion_p (src_type, dst_type));
}

/* Get the id of the region for PV within this region_model,
   emitting any diagnostics to CTXT.  */

region_id
region_model::get_lvalue (path_var pv, region_model_context *ctxt)
{
  if (pv.m_tree == NULL_TREE)
    return region_id::null ();

  region_id result_rid = get_lvalue_1 (pv, ctxt);
  assert_compat_types (get_region (result_rid)->get_type (),
		       TREE_TYPE (pv.m_tree));
  return result_rid;
}

/* Get the region_id for EXPR within this region_model (assuming the most
   recent stack frame if it's a local).  */

region_id
region_model::get_lvalue (tree expr, region_model_context *ctxt)
{
  return get_lvalue (path_var (expr, get_stack_depth () - 1), ctxt);
}

/* Implementation of region_model::get_rvalue; the latter adds type-checking.

   Get the value of PV within this region_model,
   emitting any diagnostics to CTXT.  */

svalue_id
region_model::get_rvalue_1 (path_var pv, region_model_context *ctxt)
{
  gcc_assert (pv.m_tree);

  switch (TREE_CODE (pv.m_tree))
    {
    default:
      {
	svalue *unknown_sval = new unknown_svalue (TREE_TYPE (pv.m_tree));
	return add_svalue (unknown_sval);
      }
      break;

    case ADDR_EXPR:
      {
	/* "&EXPR".  */
	tree expr = pv.m_tree;
	tree op0 = TREE_OPERAND (expr, 0);
	if (TREE_CODE (op0) == FUNCTION_DECL)
	  return get_svalue_for_fndecl (TREE_TYPE (expr), op0, ctxt);
	else if (TREE_CODE (op0) == LABEL_DECL)
	  return get_svalue_for_label (TREE_TYPE (expr), op0, ctxt);
	region_id expr_rid = get_lvalue (op0, ctxt);
	return get_or_create_ptr_svalue (TREE_TYPE (expr), expr_rid);
      }
      break;

    case ARRAY_REF:
      {
	region_id element_rid = get_lvalue (pv, ctxt);
	return get_region (element_rid)->get_value (*this, true, ctxt);
      }

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return get_or_create_constant_svalue (pv.m_tree);

    case COMPONENT_REF:
    case MEM_REF:
    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      {
	region_id var_rid = get_lvalue (pv, ctxt);
	return get_region (var_rid)->get_value (*this, true, ctxt);
      }
    }
}

/* Get the value of PV within this region_model,
   emitting any diagnostics to CTXT.  */

svalue_id
region_model::get_rvalue (path_var pv, region_model_context *ctxt)
{
  if (pv.m_tree == NULL_TREE)
    return svalue_id::null ();
  svalue_id result_sid = get_rvalue_1 (pv, ctxt);

  assert_compat_types (get_svalue (result_sid)->get_type (),
		       TREE_TYPE (pv.m_tree));

  return result_sid;
}

/* Get the value of EXPR within this region_model (assuming the most
   recent stack frame if it's a local).  */

svalue_id
region_model::get_rvalue (tree expr, region_model_context *ctxt)
{
  return get_rvalue (path_var (expr, get_stack_depth () - 1), ctxt);
}

/* Return an svalue_id for a pointer to RID of type PTR_TYPE, reusing
   existing pointer values if one is available.  */

svalue_id
region_model::get_or_create_ptr_svalue (tree ptr_type, region_id rid)
{
  /* Reuse existing region_svalue, if one of the right type is
     available.  */
  /* In theory we could stash a svalue_id in "region", but differing
     pointer types muddles things.
     For now, just do a linear search through all existing svalues.  */
  int i;
  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    if (region_svalue *ptr_svalue = svalue->dyn_cast_region_svalue ())
      if (ptr_svalue->get_pointee () == rid
	  && ptr_svalue->get_type () == ptr_type)
	return svalue_id::from_int (i);

  return add_svalue (new region_svalue (ptr_type, rid));
}

/* Return an svalue_id for a constant_svalue for CST_EXPR,
   creating the constant_svalue if necessary.
   The constant_svalue instances are reused, based on pointer equality
   of trees  */

svalue_id
region_model::get_or_create_constant_svalue (tree cst_expr)
{
  gcc_assert (cst_expr);

  /* Reuse one if it already exists.  */
  // TODO: maybe store a map, rather than do linear search?
  int i;
  svalue *svalue;
  FOR_EACH_VEC_ELT (m_svalues, i, svalue)
    if (svalue->maybe_get_constant () == cst_expr)
      return svalue_id::from_int (i);

  svalue_id cst_sid = add_svalue (new constant_svalue (cst_expr));
  return cst_sid;
}

/* Return an svalue_id for a region_svalue for FNDECL,
   creating the function_region if necessary.  */

svalue_id
region_model::get_svalue_for_fndecl (tree ptr_type, tree fndecl,
				     region_model_context *ctxt)
{
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);
  region_id function_rid = get_region_for_fndecl (fndecl, ctxt);
  return get_or_create_ptr_svalue (ptr_type, function_rid);
}

/* Return a region_id for a function_region for FNDECL,
   creating it if necessary.  */

region_id
region_model::get_region_for_fndecl (tree fndecl,
				     region_model_context *ctxt)
{
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);

  region_id code_rid = get_root_region ()->ensure_code_region (this);
  code_region *code = get_root_region ()->get_code_region (this);

  return code->get_or_create (this, code_rid, fndecl, TREE_TYPE (fndecl),
			      ctxt);
}

/* Return an svalue_id for a region_svalue for LABEL,
   creating the label_region if necessary.  */

svalue_id
region_model::get_svalue_for_label (tree ptr_type, tree label,
				    region_model_context *ctxt)
{
  gcc_assert (TREE_CODE (label) == LABEL_DECL);
  region_id label_rid = get_region_for_label (label, ctxt);
  return get_or_create_ptr_svalue (ptr_type, label_rid);
}

/* Return a region_id for a label_region for LABEL,
   creating it if necessary.  */

region_id
region_model::get_region_for_label (tree label,
				    region_model_context *ctxt)
{
  gcc_assert (TREE_CODE (label) == LABEL_DECL);

  tree fndecl = DECL_CONTEXT (label);
  gcc_assert (fndecl && TREE_CODE (fndecl) == FUNCTION_DECL);

  region_id func_rid = get_region_for_fndecl (fndecl, ctxt);
  function_region *func_reg = get_region <function_region> (func_rid);
  return func_reg->get_or_create (this, func_rid, label, TREE_TYPE (label),
				  ctxt);
}

/* Build a cast of SRC_EXPR to DST_TYPE, or return NULL_TREE.

   Adapted from gcc::jit::playback::context::build_cast, which in turn is
   adapted from
     - c/c-typeck.c:build_c_cast
     - c/c-convert.c: convert
     - convert.h
   Only some kinds of cast are currently supported here.  */

static tree
build_cast (tree dst_type, tree src_expr)
{
  tree result = targetm.convert_to_type (dst_type, src_expr);
  if (result)
    return result;
  enum tree_code dst_code = TREE_CODE (dst_type);
  switch (dst_code)
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      result = convert_to_integer (dst_type, src_expr);
      goto maybe_fold;

    case BOOLEAN_TYPE:
      /* Compare with c_objc_common_truthvalue_conversion and
	 c_common_truthvalue_conversion. */
      /* For now, convert to: (src_expr != 0)  */
      result = build2 (NE_EXPR, dst_type,
		       src_expr,
		       build_int_cst (TREE_TYPE (src_expr), 0));
      goto maybe_fold;

    case REAL_TYPE:
      result = convert_to_real (dst_type, src_expr);
      goto maybe_fold;

    case POINTER_TYPE:
      result = build1 (NOP_EXPR, dst_type, src_expr);
      goto maybe_fold;

    default:
      return NULL_TREE;

    maybe_fold:
      if (TREE_CODE (result) != C_MAYBE_CONST_EXPR)
	result = fold (result);
      return result;
    }
}

/* If the type of SID's underlying value is DST_TYPE, return SID.
   Otherwise, attempt to create (or reuse) an svalue representing an access
   of SID as a DST_TYPE and return that value's svalue_id.  */

svalue_id
region_model::maybe_cast_1 (tree dst_type, svalue_id sid)
{
  svalue *sval = get_svalue (sid);
  tree src_type = sval->get_type ();
  if (src_type == dst_type)
    return sid;

  if (POINTER_TYPE_P (dst_type)
      || POINTER_TYPE_P (src_type))
    {
      /* Pointer to region.  */
      if (region_svalue *ptr_sval = sval->dyn_cast_region_svalue ())
	return get_or_create_ptr_svalue (dst_type, ptr_sval->get_pointee ());

      /* Unknown pointer?   Get or create a new unknown pointer of the
	 correct type, preserving the equality between the pointers.  */
      if (sval->dyn_cast_unknown_svalue ())
	{
	  equiv_class &ec = m_constraints->get_equiv_class (sid);

	  /* Look for an existing pointer of the correct type within the EC.  */
	  int i;
	  svalue_id *equiv_sid;
	  FOR_EACH_VEC_ELT (ec.m_vars, i, equiv_sid)
	    {
	      svalue *equiv_val = get_svalue (*equiv_sid);
	      if (equiv_val->get_type () == dst_type)
		return *equiv_sid;
	    }

	  /* Otherwise, create a new unknown pointer of the correct type.  */
	  svalue *unknown_sval = new unknown_svalue (dst_type);
	  svalue_id new_ptr_sid = add_svalue (unknown_sval);
	  m_constraints->add_constraint (sid, EQ_EXPR, new_ptr_sid);
	  return new_ptr_sid;
	}
    }

  /* Attempt to cast constants.  */
  if (tree src_cst = sval->maybe_get_constant ())
    {
      if (tree dst = build_cast (dst_type, src_cst))
	if (CONSTANT_CLASS_P (dst))
	  return get_or_create_constant_svalue (dst);
    }

  /* Otherwise, return a new unknown value.  */
  svalue *unknown_sval = new unknown_svalue (dst_type);
  return add_svalue (unknown_sval);
}

/* If the type of SID's underlying value is DST_TYPE, return SID.
   Otherwise, attempt to create (or reuse) an svalue representing an access
   of SID as a DST_TYPE and return that value's svalue_id.

   If the result != SID, then call CTXT's on_cast vfunc (if CTXT is non-NULL),
   so that sm-state can be propagated from SID to the result.  */

svalue_id
region_model::maybe_cast (tree dst_type, svalue_id sid,
			  region_model_context *ctxt)
{
  svalue_id result = maybe_cast_1 (dst_type, sid);
  if (result != sid)
    if (ctxt)
      {
	/* Notify ctxt about a cast, so any sm-state can be copied.  */
	ctxt->on_cast (sid, result);
      }
  return result;
}

/* Ensure that the region for OBJ_RID has a child region for FIELD;
   return the child region's region_id.  */

region_id
region_model::get_field_region (region_id struct_or_union_rid, tree field,
				region_model_context *ctxt)
{
  struct_or_union_region *sou_reg
    = get_region<struct_or_union_region> (struct_or_union_rid);

  /* Inherit constness from parent type.  */
  const int qual_mask = TYPE_QUAL_CONST;
  int sou_quals = TYPE_QUALS (sou_reg->get_type ()) & qual_mask;
  tree field_type = TREE_TYPE (field);
  tree field_type_with_quals = build_qualified_type (field_type, sou_quals);

  // TODO: maybe convert to a vfunc?
  if (sou_reg->get_kind () == RK_UNION)
    {
      /* Union.
	 Get a view of the union as a whole, with the type of the field.  */
      region_id view_rid
	= get_or_create_view (struct_or_union_rid, field_type_with_quals, ctxt);
      return view_rid;
    }
  else
    {
      /* Struct.  */
      region_id child_rid
	= sou_reg->get_or_create (this, struct_or_union_rid, field,
				  field_type_with_quals, ctxt);
      return child_rid;
    }
}

/* Get a region_id for referencing PTR_SID, creating a region if need be, and
   potentially generating warnings via CTXT.  */

region_id
region_model::deref_rvalue (svalue_id ptr_sid, region_model_context *ctxt)
{
  gcc_assert (!ptr_sid.null_p ());
  svalue *ptr_svalue = get_svalue (ptr_sid);
  gcc_assert (ptr_svalue);

  switch (ptr_svalue->get_kind ())
    {
    case SK_REGION:
      {
	region_svalue *region_sval = as_a <region_svalue *> (ptr_svalue);
	return region_sval->get_pointee ();
      }

    case SK_CONSTANT:
      goto create_symbolic_region;

    case SK_POISONED:
      {
	if (ctxt)
	  if (tree ptr = get_representative_tree (ptr_sid))
	    {
	      poisoned_svalue *poisoned_sval
		= as_a <poisoned_svalue *> (ptr_svalue);
	      enum poison_kind pkind = poisoned_sval->get_poison_kind ();
	      ctxt->warn (new poisoned_value_diagnostic (ptr, pkind));
	    }
	goto create_symbolic_region;
      }

    case SK_UNKNOWN:
      {
      create_symbolic_region:
	/* We need a symbolic_region to represent this unknown region.
	   We don't know if it on the heap, stack, or a global,
	   so use the root region as parent.  */
	region_id new_rid
	  = add_region (new symbolic_region (m_root_rid, NULL_TREE, false));

	/* We need to write the region back into the pointer,
	   or we'll get a new, different region each time.
	   We do this by changing the meaning of ptr_sid, replacing
	   the unknown value with the ptr to the new region.
	   We replace the meaning of the ID rather than simply writing
	   to PTR's lvalue since there could be several places sharing
	   the same unknown ptr value.  */
	svalue *ptr_val
	  = new region_svalue (ptr_svalue->get_type (), new_rid);
	replace_svalue (ptr_sid, ptr_val);

	return new_rid;
      }

    case SK_SETJMP:
      goto create_symbolic_region;
    }

  gcc_unreachable ();
}

/* Get a region_id for referencing PTR, creating a region if need be, and
   potentially generating warnings via CTXT.  */

region_id
region_model::deref_rvalue (tree ptr, region_model_context *ctxt)
{
  svalue_id ptr_sid = get_rvalue (ptr, ctxt);
  return deref_rvalue (ptr_sid, ctxt);
}

/* Set the value of the region given by LHS_RID to the value given
   by RHS_SID.  */

void
region_model::set_value (region_id lhs_rid, svalue_id rhs_sid,
			 region_model_context *ctxt)
{
  gcc_assert (!lhs_rid.null_p ());
  gcc_assert (!rhs_sid.null_p ());
  get_region (lhs_rid)->set_value (*this, lhs_rid, rhs_sid, ctxt);
}

/* Set the value of the region given by LHS to the value given
   by RHS.  */

void
region_model::set_value (tree lhs, tree rhs, region_model_context *ctxt)
{
  region_id lhs_rid = get_lvalue (lhs, ctxt);
  svalue_id rhs_sid = get_rvalue (rhs, ctxt);
  gcc_assert (!lhs_rid.null_p ());
  gcc_assert (!rhs_sid.null_p ());
  set_value (lhs_rid, rhs_sid, ctxt);
}

/* Determine what is known about the condition "LHS_SID OP RHS_SID" within
   this model.  */

tristate
region_model::eval_condition (svalue_id lhs_sid,
			      enum tree_code op,
			      svalue_id rhs_sid) const
{
  svalue *lhs = get_svalue (lhs_sid);
  svalue *rhs = get_svalue (rhs_sid);

  /* For now, make no attempt to capture constraints on floating-point
     values.  */
  if ((lhs->get_type () && FLOAT_TYPE_P (lhs->get_type ()))
      || (rhs->get_type () && FLOAT_TYPE_P (rhs->get_type ())))
    return tristate::unknown ();

  tristate ts = eval_condition_without_cm (lhs_sid, op, rhs_sid);

  if (ts.is_known ())
    return ts;

  /* Otherwise, try constraints.  */
  return m_constraints->eval_condition (lhs_sid, op, rhs_sid);
}

/* Determine what is known about the condition "LHS_SID OP RHS_SID" within
   this model, without resorting to the constraint_manager.

   This is exposed so that impl_region_model_context::on_state_leak can
   check for equality part-way through region_model::purge_unused_svalues
   without risking creating new ECs.  */

tristate
region_model::eval_condition_without_cm (svalue_id lhs_sid,
					 enum tree_code op,
					 svalue_id rhs_sid) const
{
  svalue *lhs = get_svalue (lhs_sid);
  svalue *rhs = get_svalue (rhs_sid);
  gcc_assert (lhs);
  gcc_assert (rhs);

  /* See what we know based on the values.  */
  if (lhs && rhs)
    {
      /* For now, make no attempt to capture constraints on floating-point
	 values.  */
      if ((lhs->get_type () && FLOAT_TYPE_P (lhs->get_type ()))
	  || (rhs->get_type () && FLOAT_TYPE_P (rhs->get_type ())))
	return tristate::unknown ();

      if (lhs == rhs)
	{
	  /* If we have the same svalue, then we have equality
	     (apart from NaN-handling).
	     TODO: should this definitely be the case for poisoned values?  */
	  switch (op)
	    {
	    case EQ_EXPR:
	    case GE_EXPR:
	    case LE_EXPR:
	      return tristate::TS_TRUE;

	    case NE_EXPR:
	    case GT_EXPR:
	    case LT_EXPR:
	      return tristate::TS_FALSE;

	    default:
	      /* For other ops, use the logic below.  */
	      break;
	    }
	}

      /* If we have a pair of region_svalues, compare them.  */
      if (region_svalue *lhs_ptr = lhs->dyn_cast_region_svalue ())
	if (region_svalue *rhs_ptr = rhs->dyn_cast_region_svalue ())
	  {
	    tristate res = region_svalue::eval_condition (lhs_ptr, op, rhs_ptr);
	    if (res.is_known ())
	      return res;
	    /* Otherwise, only known through constraints.  */
	  }

      /* If we have a pair of constants, compare them.  */
      if (constant_svalue *cst_lhs = lhs->dyn_cast_constant_svalue ())
	if (constant_svalue *cst_rhs = rhs->dyn_cast_constant_svalue ())
	  return constant_svalue::eval_condition (cst_lhs, op, cst_rhs);

      /* Handle comparison of a region_svalue against zero.  */
      if (region_svalue *ptr = lhs->dyn_cast_region_svalue ())
	if (constant_svalue *cst_rhs = rhs->dyn_cast_constant_svalue ())
	  if (zerop (cst_rhs->get_constant ()))
	    {
	      /* A region_svalue is a non-NULL pointer, except in certain
		 special cases (see the comment for region::non_null_p.  */
	      region *pointee = get_region (ptr->get_pointee ());
	      if (pointee->non_null_p (*this))
		{
		  switch (op)
		    {
		    default:
		      gcc_unreachable ();

		    case EQ_EXPR:
		    case GE_EXPR:
		    case LE_EXPR:
		      return tristate::TS_FALSE;

		    case NE_EXPR:
		    case GT_EXPR:
		    case LT_EXPR:
		      return tristate::TS_TRUE;
		    }
		}
	    }
    }

  return tristate::TS_UNKNOWN;
}

/* Attempt to add the constraint "LHS OP RHS" to this region_model.
   If it is consistent with existing constraints, add it, and return true.
   Return false if it contradicts existing constraints.
   Use CTXT for reporting any diagnostics associated with the accesses.  */

bool
region_model::add_constraint (tree lhs, enum tree_code op, tree rhs,
			      region_model_context *ctxt)
{
  /* For now, make no attempt to capture constraints on floating-point
     values.  */
  if (FLOAT_TYPE_P (TREE_TYPE (lhs)) || FLOAT_TYPE_P (TREE_TYPE (rhs)))
    return true;

  svalue_id lhs_sid = get_rvalue (lhs, ctxt);
  svalue_id rhs_sid = get_rvalue (rhs, ctxt);

  tristate t_cond = eval_condition (lhs_sid, op, rhs_sid);

  /* If we already have the condition, do nothing.  */
  if (t_cond.is_true ())
    return true;

  /* Reject a constraint that would contradict existing knowledge, as
     unsatisfiable.  */
  if (t_cond.is_false ())
    return false;

  /* Store the constraint.  */
  m_constraints->add_constraint (lhs_sid, op, rhs_sid);

  add_any_constraints_from_ssa_def_stmt (lhs, op, rhs, ctxt);

  /* If we now know a symbolic_region is non-NULL, clear its
     m_possibly_null.  */
  if (zerop (rhs) && op == NE_EXPR)
    if (region_svalue *ptr = get_svalue (lhs_sid)->dyn_cast_region_svalue ())
      {
	region *pointee = get_region (ptr->get_pointee ());
	if (symbolic_region *sym_reg = pointee->dyn_cast_symbolic_region ())
	  sym_reg->m_possibly_null = false;
      }

  /* Notify the context, if any.  This exists so that the state machines
     in a program_state can be notified about the condition, and so can
     set sm-state for e.g. unchecked->checked, both for cfg-edges, and
     when synthesizing constraints as above.  */
  if (ctxt)
    ctxt->on_condition (lhs, op, rhs);

  return true;
}

/* Subroutine of region_model::add_constraint for handling optimized
   && and || conditionals.

   If we have an SSA_NAME for a boolean compared against 0,
   look at anything implied by the def stmt and call add_constraint
   for it (which could recurse).

   For example, if we have
      _1 = p_6 == 0B;
      _2 = p_8 == 0B
      _3 = _1 | _2
    and add the constraint
      (_3 == 0),
    then the def stmt for _3 implies that _1 and _2 are both false,
    and hence we can add the constraints:
      p_6 != 0B
      p_8 != 0B.  */

void
region_model::add_any_constraints_from_ssa_def_stmt (tree lhs,
						     enum tree_code op,
						     tree rhs,
						     region_model_context *ctxt)
{
  if (TREE_CODE (lhs) != SSA_NAME)
    return;

  if (!zerop (rhs))
    return;

  if (op != NE_EXPR && op != EQ_EXPR)
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (lhs);
  if (const gassign *assign = dyn_cast<gassign *> (def_stmt))
    add_any_constraints_from_gassign (op, rhs, assign, ctxt);
  else if (gcall *call = dyn_cast<gcall *> (def_stmt))
    add_any_constraints_from_gcall (op, rhs, call, ctxt);
}

/* Add any constraints for an SSA_NAME defined by ASSIGN
   where the result OP RHS.  */

void
region_model::add_any_constraints_from_gassign (enum tree_code op,
						tree rhs,
						const gassign *assign,
						region_model_context *ctxt)
{
  /* We have either
     - "LHS != false" (i.e. LHS is true), or
     - "LHS == false" (i.e. LHS is false).  */
  bool is_true = op == NE_EXPR;

  enum tree_code rhs_code = gimple_assign_rhs_code (assign);

  switch (rhs_code)
    {
    default:
      break;

    case NOP_EXPR:
      {
	add_constraint (gimple_assign_rhs1 (assign), op, rhs, ctxt);
      }
      break;

    case BIT_AND_EXPR:
      {
	if (is_true)
	  {
	    /* ...and "LHS == (rhs1 & rhs2) i.e. "(rhs1 & rhs2)" is true
	       then both rhs1 and rhs2 must be true.  */
	    tree rhs1 = gimple_assign_rhs1 (assign);
	    tree rhs2 = gimple_assign_rhs2 (assign);
	    add_constraint (rhs1, NE_EXPR, boolean_false_node, ctxt);
	    add_constraint (rhs2, NE_EXPR, boolean_false_node, ctxt);
	  }
      }
      break;

    case BIT_IOR_EXPR:
      {
	if (!is_true)
	  {
	    /* ...and "LHS == (rhs1 | rhs2)
	       i.e. "(rhs1 | rhs2)" is false
	       then both rhs1 and rhs2 must be false.  */
	    tree rhs1 = gimple_assign_rhs1 (assign);
	    tree rhs2 = gimple_assign_rhs2 (assign);
	    add_constraint (rhs1, EQ_EXPR, boolean_false_node, ctxt);
	    add_constraint (rhs2, EQ_EXPR, boolean_false_node, ctxt);
	  }
      }
      break;

    case EQ_EXPR:
    case NE_EXPR:
      {
	/* ...and "LHS == (rhs1 OP rhs2)"
	   then rhs1 OP rhs2 must have the same logical value as LHS.  */
	tree rhs1 = gimple_assign_rhs1 (assign);
	tree rhs2 = gimple_assign_rhs2 (assign);
	if (!is_true)
	  rhs_code
	    = invert_tree_comparison (rhs_code, false /* honor_nans */);
	add_constraint (rhs1, rhs_code, rhs2, ctxt);
      }
      break;
    }
}

/* Add any constraints for an SSA_NAME defined by CALL
   where the result OP RHS.  */

void
region_model::add_any_constraints_from_gcall (enum tree_code op,
					      tree rhs,
					      const gcall *call,
					      region_model_context *ctxt)
{
  if (gimple_call_builtin_p (call, BUILT_IN_EXPECT)
      || gimple_call_builtin_p (call, BUILT_IN_EXPECT_WITH_PROBABILITY)
      || gimple_call_internal_p (call, IFN_BUILTIN_EXPECT))
    {
      /* __builtin_expect's return value is its initial argument.  */
      add_constraint (gimple_call_arg (call, 0), op, rhs, ctxt);
    }
}

/* Determine what is known about the condition "LHS OP RHS" within
   this model.
   Use CTXT for reporting any diagnostics associated with the accesses.  */

tristate
region_model::eval_condition (tree lhs,
			      enum tree_code op,
			      tree rhs,
			      region_model_context *ctxt)
{
  /* For now, make no attempt to model constraints on floating-point
     values.  */
  if (FLOAT_TYPE_P (TREE_TYPE (lhs)) || FLOAT_TYPE_P (TREE_TYPE (rhs)))
    return tristate::unknown ();

  return eval_condition (get_rvalue (lhs, ctxt), op, get_rvalue (rhs, ctxt));
}

/* If SID is a constant value, return the underlying tree constant.
   Otherwise, return NULL_TREE.  */

tree
region_model::maybe_get_constant (svalue_id sid) const
{
  gcc_assert (!sid.null_p ());
  svalue *sval = get_svalue (sid);
  return sval->maybe_get_constant ();
}

/* Create a new child region of the heap (creating the heap region if
   necessary).
   Return the region_id of the new child region.  */

region_id
region_model::add_new_malloc_region ()
{
  region_id heap_rid
    = get_root_region ()->ensure_heap_region (this);
  return add_region (new symbolic_region (heap_rid, NULL_TREE, true));
}

/* Attempt to return a tree that represents SID, or return NULL_TREE.  */

tree
region_model::get_representative_tree (svalue_id sid) const
{
  if (sid.null_p ())
    return NULL_TREE;

  /* Find the first region that stores the value (e.g. a local) and
     generate a representative tree for it.  */
  unsigned i;
  region *region;
  FOR_EACH_VEC_ELT (m_regions, i, region)
    if (sid == region->get_value_direct ())
      {
	path_var pv = get_representative_path_var (region_id::from_int (i));
	if (pv.m_tree)
	  return pv.m_tree;
      }

  /* Handle string literals and various other pointers.  */
  svalue *sval = get_svalue (sid);
  if (region_svalue *ptr_sval = sval->dyn_cast_region_svalue ())
    {
      region_id rid = ptr_sval->get_pointee ();
      path_var pv = get_representative_path_var (rid);
      if (pv.m_tree)
	return build1 (ADDR_EXPR,
		       TREE_TYPE (sval->get_type ()),
		       pv.m_tree);
    }

  return maybe_get_constant (sid);
}

/* Attempt to return a path_var that represents the region, or return
   the NULL path_var.
   For example, a region for a field of a local would be a path_var
   wrapping a COMPONENT_REF.  */

path_var
region_model::get_representative_path_var (region_id rid) const
{
  region *reg = get_region (rid);
  region *parent_reg = get_region (reg->get_parent ());
  region_id stack_rid = get_stack_region_id ();
  if (!stack_rid.null_p ())
    if (parent_reg && parent_reg->get_parent () == stack_rid)
      {
	frame_region *parent_frame = (frame_region *)parent_reg;
	tree t = parent_frame->get_tree_for_child_region (rid);
	return path_var (t, parent_frame->get_depth ());
    }
  if (reg->get_parent () == get_globals_region_id ())
    {
      map_region *globals = get_root_region ()->get_globals_region (this);
      if (globals)
	return path_var (globals->get_tree_for_child_region (rid), -1);
    }

  /* Handle e.g. fields of a local by recursing.  */
  region_id parent_rid = reg->get_parent ();
  if (parent_reg)
    {
      if (reg->is_view_p ())
	{
	  path_var parent_pv = get_representative_path_var (parent_rid);
	  if (parent_pv.m_tree && reg->get_type ())
	    return path_var (build1 (NOP_EXPR,
				     reg->get_type (),
				     parent_pv.m_tree),
			     parent_pv.m_stack_depth);
	}

      if (parent_reg->get_kind () == RK_STRUCT)
	{
	  map_region *parent_map_region = (map_region *)parent_reg;
	  /* This can fail if we have a view, rather than a field.  */
	  if (tree child_key
		= parent_map_region->get_tree_for_child_region (rid))
	    {
	      path_var parent_pv = get_representative_path_var (parent_rid);
	      if (parent_pv.m_tree && TREE_CODE (child_key) == FIELD_DECL)
		return path_var (build3 (COMPONENT_REF,
					 TREE_TYPE (child_key),
					 parent_pv.m_tree, child_key,
					 NULL_TREE),
				 parent_pv.m_stack_depth);
	    }
	}

      /* Handle elements within an array.  */
      if (array_region *array_reg = parent_reg->dyn_cast_array_region ())
      {
	array_region::key_t key;
	if (array_reg->get_key_for_child_region (rid, &key))
	  {
	    path_var parent_pv = get_representative_path_var (parent_rid);
	    if (parent_pv.m_tree && reg->get_type ())
	      {
		tree index = array_reg->constant_from_key (key);
		return path_var (build4 (ARRAY_REF,
					 reg->get_type (),
					 parent_pv.m_tree, index,
					 NULL_TREE, NULL_TREE),
				 parent_pv.m_stack_depth);
	      }
	  }
      }
    }

  /* Handle string literals.  */
  svalue_id sid = reg->get_value_direct ();
  if (svalue *sval = get_svalue (sid))
    if (tree cst = sval->maybe_get_constant ())
      if (TREE_CODE (cst) == STRING_CST)
	return path_var (cst, 0);

  return path_var (NULL_TREE, 0);
}

/* Locate all regions that directly have value SID and append representative
   path_var instances for them into *OUT.  */

void
region_model::get_path_vars_for_svalue (svalue_id sid, vec<path_var> *out) const
{
  unsigned i;
  region *region;
  FOR_EACH_VEC_ELT (m_regions, i, region)
    if (sid == region->get_value_direct ())
      {
	path_var pv = get_representative_path_var (region_id::from_int (i));
	if (pv.m_tree)
	  out->safe_push (pv);
      }
}

/* Set DST_RID value to be a new unknown value of type TYPE.  */

svalue_id
region_model::set_to_new_unknown_value (region_id dst_rid, tree type,
					region_model_context *ctxt)
{
  gcc_assert (!dst_rid.null_p ());
  svalue_id new_sid = add_svalue (new unknown_svalue (type));
  set_value (dst_rid, new_sid, ctxt);

  // TODO: presumably purge all child regions too (but do this in set_value?)

  return new_sid;
}

/* Update this model for any phis in SNODE, assuming we came from
   LAST_CFG_SUPEREDGE.  */

void
region_model::update_for_phis (const supernode *snode,
			       const cfg_superedge *last_cfg_superedge,
			       region_model_context *ctxt)
{
  gcc_assert (last_cfg_superedge);

  for (gphi_iterator gpi = const_cast<supernode *>(snode)->start_phis ();
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();

      tree src = last_cfg_superedge->get_phi_arg (phi);
      tree lhs = gimple_phi_result (phi);

      /* Update next_state based on phi.  */
      bool is_back_edge = last_cfg_superedge->back_edge_p ();
      handle_phi (phi, lhs, src, is_back_edge, ctxt);
    }
}

/* Attempt to update this model for taking EDGE (where the last statement
   was LAST_STMT), returning true if the edge can be taken, false
   otherwise.

   For CFG superedges where LAST_STMT is a conditional or a switch
   statement, attempt to add the relevant conditions for EDGE to this
   model, returning true if they are feasible, or false if they are
   impossible.

   For call superedges, push frame information and store arguments
   into parameters.

   For return superedges, pop frame information and store return
   values into any lhs.

   Rejection of call/return superedges happens elsewhere, in
   program_point::on_edge (i.e. based on program point, rather
   than program state).  */

bool
region_model::maybe_update_for_edge (const superedge &edge,
				     const gimple *last_stmt,
				     region_model_context *ctxt)
{
  /* Handle frame updates for interprocedural edges.  */
  switch (edge.m_kind)
    {
    default:
      break;

    case SUPEREDGE_CALL:
      {
	const call_superedge *call_edge = as_a <const call_superedge *> (&edge);
	update_for_call_superedge (*call_edge, ctxt);
      }
      break;

    case SUPEREDGE_RETURN:
      {
	const return_superedge *return_edge
	  = as_a <const return_superedge *> (&edge);
	update_for_return_superedge (*return_edge, ctxt);
      }
      break;

    case SUPEREDGE_INTRAPROCEDURAL_CALL:
      {
	const callgraph_superedge *cg_sedge
	  = as_a <const callgraph_superedge *> (&edge);
	update_for_call_summary (*cg_sedge, ctxt);
      }
      break;
    }

  if (last_stmt == NULL)
    return true;

  /* Apply any constraints for conditionals/switch statements.  */

  if (const gcond *cond_stmt = dyn_cast <const gcond *> (last_stmt))
    {
      const cfg_superedge *cfg_sedge = as_a <const cfg_superedge *> (&edge);
      return apply_constraints_for_gcond (*cfg_sedge, cond_stmt, ctxt);
    }

  if (const gswitch *switch_stmt = dyn_cast <const gswitch *> (last_stmt))
    {
      const switch_cfg_superedge *switch_sedge
	= as_a <const switch_cfg_superedge *> (&edge);
      return apply_constraints_for_gswitch (*switch_sedge, switch_stmt, ctxt);
    }

  return true;
}

/* Push a new frame_region on to the stack region.
   Populate the frame_region with child regions for the function call's
   parameters, using values from the arguments at the callsite in the
   caller's frame.  */

void
region_model::update_for_call_superedge (const call_superedge &call_edge,
					 region_model_context *ctxt)
{
  /* Build a vec of argument svalue_id, using the current top
     frame for resolving tree expressions.  */
  const gcall *call_stmt = call_edge.get_call_stmt ();
  auto_vec<svalue_id> arg_sids (gimple_call_num_args (call_stmt));

  for (unsigned i = 0; i < gimple_call_num_args (call_stmt); i++)
    {
      tree arg = gimple_call_arg (call_stmt, i);
      arg_sids.quick_push (get_rvalue (arg, ctxt));
    }

  push_frame (call_edge.get_callee_function (), &arg_sids, ctxt);
}

/* Pop the top-most frame_region from the stack, and copy the return
   region's values (if any) into the region for the lvalue of the LHS of
   the call (if any).  */

void
region_model::update_for_return_superedge (const return_superedge &return_edge,
					   region_model_context *ctxt)
{
  region_id stack_rid = get_stack_region_id ();
  stack_region *stack = get_region <stack_region> (stack_rid);

  /* Get the region for the result of the call, within the caller frame.  */
  region_id result_dst_rid;
  const gcall *call_stmt = return_edge.get_call_stmt ();
  tree lhs = gimple_call_lhs (call_stmt);
  if (lhs)
    {
      /* Normally we access the top-level frame, which is:
	   path_var (expr, stack->get_num_frames () - 1)
	 whereas here we need the caller frame, hence "- 2" here.  */
      gcc_assert (stack->get_num_frames () >= 2);
      result_dst_rid = get_lvalue (path_var (lhs, stack->get_num_frames () - 2),
				   ctxt);
    }

  purge_stats stats;
  stack->pop_frame (this, result_dst_rid, true, &stats, ctxt);
  // TODO: do something with the stats?

  if (!lhs)
    {
      /* This could be a leak; try purging again, but this time,
	 don't special-case the result sids (as was done in pop_frame).  */
      purge_unused_svalues (&stats, ctxt);
    }
}

/* Update this region_model with a summary of the effect of calling
   and returning from CG_SEDGE.

   TODO: Currently this is extremely simplistic: we merely set the
   return value to "unknown".  A proper implementation would e.g. update
   sm-state, and presumably be reworked to support multiple outcomes.  */

void
region_model::update_for_call_summary (const callgraph_superedge &cg_sedge,
				       region_model_context *ctxt)
{
  /* For now, set any return value to "unknown".  */
  const gcall *call_stmt = cg_sedge.get_call_stmt ();
  tree lhs = gimple_call_lhs (call_stmt);
  if (lhs)
    set_to_new_unknown_value (get_lvalue (lhs, ctxt), TREE_TYPE (lhs), ctxt);

  // TODO: actually implement some kind of summary here
}

/* Given a true or false edge guarded by conditional statement COND_STMT,
   determine appropriate constraints for the edge to be taken.

   If they are feasible, add the constraints and return true.

   Return false if the constraints contradict existing knowledge
   (and so the edge should not be taken).  */

bool
region_model::apply_constraints_for_gcond (const cfg_superedge &sedge,
					   const gcond *cond_stmt,
					   region_model_context *ctxt)
{
  ::edge cfg_edge = sedge.get_cfg_edge ();
  gcc_assert (cfg_edge != NULL);
  gcc_assert (cfg_edge->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE));

  enum tree_code op = gimple_cond_code (cond_stmt);
  tree lhs = gimple_cond_lhs (cond_stmt);
  tree rhs = gimple_cond_rhs (cond_stmt);
  if (cfg_edge->flags & EDGE_FALSE_VALUE)
    op = invert_tree_comparison (op, false /* honor_nans */);
  return add_constraint (lhs, op, rhs, ctxt);
}

/* Given an EDGE guarded by SWITCH_STMT, determine appropriate constraints
   for the edge to be taken.

   If they are feasible, add the constraints and return true.

   Return false if the constraints contradict existing knowledge
   (and so the edge should not be taken).  */

bool
region_model::apply_constraints_for_gswitch (const switch_cfg_superedge &edge,
					     const gswitch *switch_stmt,
					     region_model_context *ctxt)
{
  tree index  = gimple_switch_index (switch_stmt);
  tree case_label = edge.get_case_label ();
  gcc_assert (TREE_CODE (case_label) == CASE_LABEL_EXPR);
  tree lower_bound = CASE_LOW (case_label);
  tree upper_bound = CASE_HIGH (case_label);
  if (lower_bound)
    {
      if (upper_bound)
	{
	  /* Range.  */
	  if (!add_constraint (index, GE_EXPR, lower_bound, ctxt))
	    return false;
	  return add_constraint (index, LE_EXPR, upper_bound, ctxt);
	}
      else
	/* Single-value.  */
	return add_constraint (index, EQ_EXPR, lower_bound, ctxt);
    }
  else
    {
      /* The default case.
	 Add exclusions based on the other cases.  */
      for (unsigned other_idx = 1;
	   other_idx < gimple_switch_num_labels (switch_stmt);
	   other_idx++)
	{
	  tree other_label = gimple_switch_label (switch_stmt,
						  other_idx);
	  tree other_lower_bound = CASE_LOW (other_label);
	  tree other_upper_bound = CASE_HIGH (other_label);
	  gcc_assert (other_lower_bound);
	  if (other_upper_bound)
	    {
	      /* Exclude this range-valued case.
		 For now, we just exclude the boundary values.
		 TODO: exclude the values within the region.  */
	      if (!add_constraint (index, NE_EXPR, other_lower_bound, ctxt))
		return false;
	      if (!add_constraint (index, NE_EXPR, other_upper_bound, ctxt))
		return false;
	    }
	  else
	    /* Exclude this single-valued case.  */
	    if (!add_constraint (index, NE_EXPR, other_lower_bound, ctxt))
	      return false;
	}
      return true;
    }
}

/* Get the root_region within this model (guaranteed to be non-null).  */

root_region *
region_model::get_root_region () const
{
  return get_region<root_region> (m_root_rid);
}

/* Get the region_id of this model's stack region (if any).  */

region_id
region_model::get_stack_region_id () const
{
  return get_root_region ()->get_stack_region_id ();
}

/* Create a new frame_region for a call to FUN and push it onto
   the stack.

   If ARG_SIDS is non-NULL, use it to populate the parameters
   in the new frame.
   Otherwise, populate them with unknown values.

   Return the region_id of the new frame_region.  */

region_id
region_model::push_frame (function *fun, vec<svalue_id> *arg_sids,
			  region_model_context *ctxt)
{
  return get_root_region ()->push_frame (this, fun, arg_sids, ctxt);
}

/* Get the region_id of the top-most frame in this region_model's stack,
   if any.  */

region_id
region_model::get_current_frame_id () const
{
  return get_root_region ()->get_current_frame_id (*this);
}

/* Get the function of the top-most frame in this region_model's stack.
   There must be such a frame.  */

function *
region_model::get_current_function () const
{
  region_id frame_id = get_current_frame_id ();
  frame_region *frame = get_region<frame_region> (frame_id);
  return frame->get_function ();
}

/* Pop the topmost frame_region from this region_model's stack;
   see the comment for stack_region::pop_frame.  */

void
region_model::pop_frame (region_id result_dst_rid,
			 bool purge, purge_stats *out,
			 region_model_context *ctxt)
{
  get_root_region ()->pop_frame (this, result_dst_rid, purge, out, ctxt);
}

/* Get the number of frames in this region_model's stack.  */

int
region_model::get_stack_depth () const
{
  stack_region *stack = get_root_region ()->get_stack_region (this);
  if (stack)
    return stack->get_num_frames ();
  else
    return 0;
}

/* Get the function * at DEPTH within the call stack.  */

function *
region_model::get_function_at_depth (unsigned depth) const
{
  stack_region *stack = get_root_region ()->get_stack_region (this);
  gcc_assert (stack);
  region_id frame_rid = stack->get_frame_rid (depth);
  frame_region *frame = get_region <frame_region> (frame_rid);
  return frame->get_function ();
}

/* Get the region_id of this model's globals region (if any).  */

region_id
region_model::get_globals_region_id () const
{
  return get_root_region ()->get_globals_region_id ();
}

/* Add SVAL to this model, taking ownership, and returning its new
   svalue_id.  */

svalue_id
region_model::add_svalue (svalue *sval)
{
  gcc_assert (sval);
  m_svalues.safe_push (sval);
  return svalue_id::from_int (m_svalues.length () - 1);
}

/* Change the meaning of SID to be NEW_SVAL
   (e.g. when deferencing an unknown pointer, the pointer
   becomes a pointer to a symbolic region, so that all users
   of the former unknown pointer are now effectively pointing
   at the same region).  */

void
region_model::replace_svalue (svalue_id sid, svalue *new_sval)
{
  gcc_assert (!sid.null_p ());
  int idx = sid.as_int ();

  gcc_assert (m_svalues[idx]);
  gcc_assert (m_svalues[idx]->get_type () == new_sval->get_type ());
  delete m_svalues[idx];

  m_svalues[idx] = new_sval;
}

/* Add region R to this model, taking ownership, and returning its new
   region_id.  */

region_id
region_model::add_region (region *r)
{
  gcc_assert (r);
  m_regions.safe_push (r);
  return region_id::from_int (m_regions.length () - 1);
}

/* Return the svalue with id SVAL_ID, or NULL for a null id.  */

svalue *
region_model::get_svalue (svalue_id sval_id) const
{
  if (sval_id.null_p ())
    return NULL;
  return m_svalues[sval_id.as_int ()];
}

/* Return the region with id RID, or NULL for a null id.  */

region *
region_model::get_region (region_id rid) const
{
  if (rid.null_p ())
    return NULL;
  return m_regions[rid.as_int ()];
}

/* Make a region of an appropriate subclass for TYPE,
   with parent PARENT_RID, or return NULL for types we don't yet know
   how to handle.  */

static region *
make_region_for_type (region_id parent_rid, tree type)
{
  gcc_assert (TYPE_P (type));

  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type)
      || POINTER_TYPE_P (type)
      || TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    return new primitive_region (parent_rid, type);

  if (TREE_CODE (type) == RECORD_TYPE)
    return new struct_region (parent_rid, type);

  if (TREE_CODE (type) == ARRAY_TYPE)
    return new array_region (parent_rid, type);

  if (TREE_CODE (type) == UNION_TYPE)
    return new union_region (parent_rid, type);

  if (FUNC_OR_METHOD_TYPE_P (type))
    return new function_region (parent_rid, type);

  /* If we have a void *, make a new symbolic region.  */
  if (VOID_TYPE_P (type))
    return new symbolic_region (parent_rid, type, false);

  return NULL;
}

/* Add a region with type TYPE and parent PARENT_RID.  */

region_id
region_model::add_region_for_type (region_id parent_rid, tree type,
				   region_model_context *ctxt)
{
  gcc_assert (TYPE_P (type));

  if (region *new_region = make_region_for_type (parent_rid, type))
    return add_region (new_region);

  /* If we can't handle TYPE, return a placeholder region, and stop
     exploring this path.  */
  return make_region_for_unexpected_tree_code (ctxt, type,
					       dump_location_t ());
}

/* Helper class for region_model::purge_unused_svalues.  */

class restrict_to_used_svalues : public purge_criteria
{
public:
  restrict_to_used_svalues (const auto_sbitmap &used) : m_used (used) {}

  bool should_purge_p (svalue_id sid) const FINAL OVERRIDE
  {
    gcc_assert (!sid.null_p ());
    return !bitmap_bit_p (m_used, sid.as_int ());
  }

private:
  const auto_sbitmap &m_used;
};

/* Remove unused svalues from this model, accumulating stats into STATS.
   Unused svalues are deleted.  Doing so could reorder the svalues, and
   thus change the meaning of svalue_ids.

   If CTXT is non-NULL, then it is notified about svalue_id remappings,
   and about svalue_ids that are about to be deleted.  This allows e.g.
   for warning about resource leaks, for the case where the svalue
   represents a resource handle in the user code (e.g. a FILE * or a malloc
   buffer).

   Amongst other things, removing unused svalues is important for ensuring
   that the analysis of loops terminates.  Otherwise, we could generate a
   succession of models with unreferenced "unknown" values, where the
   number of redundant unknown values could grow without bounds, and each
   such model would be treated as distinct.

   If KNOWN_USED_SIDS is non-NULL, treat *KNOWN_USED_SIDS as used (this is for
   handling values being returned from functions as their frame is popped,
   since otherwise we'd have to simultaneously determine both the rvalue
   of the return expr in the callee frame and the lvalue for the gcall's
   assignment in the caller frame, and it seems cleaner to express all
   lvalue and rvalue lookups implicitly relative to a "current" frame).
   The svalue_ids in *KNOWN_USED_SIDS are not remapped and hence this
   call makes it invalid.  */

void
region_model::purge_unused_svalues (purge_stats *stats,
				    region_model_context *ctxt,
				    svalue_id_set *known_used_sids)
{
  // TODO: might want to avoid a vfunc call just to do logging here:
  logger *logger = ctxt ? ctxt->get_logger () : NULL;

  LOG_SCOPE (logger);

  auto_sbitmap used (m_svalues.length ());
  bitmap_clear (used);

  if (known_used_sids)
    {
      /* We can't use an sbitmap for known_used_sids as the number of
	 svalues could have grown since it was created.  */
      for (unsigned i = 0; i < get_num_svalues (); i++)
	if (known_used_sids->svalue_p (svalue_id::from_int (i)))
	  bitmap_set_bit (used, i);
    }

  /* Walk the regions, marking sids that are used.  */
  unsigned i;
  region *r;
  FOR_EACH_VEC_ELT (m_regions, i, r)
    {
      svalue_id sid = r->get_value_direct ();
      if (!sid.null_p ())
	bitmap_set_bit (used, sid.as_int ());
    }

  /* Now purge any constraints involving svalues we don't care about.  */
  restrict_to_used_svalues criterion (used);
  m_constraints->purge (criterion, stats);

  /* Mark any sids that are in constraints that survived.  */
  {
    equiv_class *ec;
    FOR_EACH_VEC_ELT (m_constraints->m_equiv_classes, i, ec)
      {
	int j;
	svalue_id *sid;
	FOR_EACH_VEC_ELT (ec->m_vars, j, sid)
	  {
	    gcc_assert (!sid->null_p ());
	    bitmap_set_bit (used, sid->as_int ());
	  }
      }
  }

  /* Build a mapping from old-sid to new-sid so that we can preserve
     order of the used IDs and move all redundant ones to the end.
     Iterate though svalue IDs, adding used ones to the front of
     the new list, and unused ones to the back.  */
  svalue_id_map map (m_svalues.length ());
  int next_used_new_sid = 0;
  int after_next_unused_new_sid = m_svalues.length ();
  for (unsigned i = 0; i < m_svalues.length (); i++)
    {
      svalue_id src (svalue_id::from_int (i));
      if (bitmap_bit_p (used, i))
	{
	  if (logger)
	    logger->log ("sv%i is used", i);
	  map.put (src, svalue_id::from_int (next_used_new_sid++));
	}
      else
	{
	  if (logger)
	    logger->log ("sv%i is unused", i);
	  map.put (src, svalue_id::from_int (--after_next_unused_new_sid));
	}
    }
  /* The two insertion points should have met.  */
  gcc_assert (next_used_new_sid == after_next_unused_new_sid);

  /* Now walk the regions and the constraints, remapping sids,
     so that all the redundant svalues are at the end.  */
  remap_svalue_ids (map);

  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("map: ");
      map.dump_to_pp (logger->get_printer ());
      logger->end_log_line ();
    }

  /* Notify any client about the remapping and pending deletion.
     Potentially this could trigger leak warnings.  */
  if (ctxt)
    {
      ctxt->remap_svalue_ids (map);
      int num_client_items_purged
	= ctxt->on_svalue_purge (svalue_id::from_int (next_used_new_sid), map);
      if (stats)
	stats->m_num_client_items += num_client_items_purged;
    }

  /* Drop the redundant svalues from the end of the vector.  */
  while ((signed)m_svalues.length () > next_used_new_sid)
    {
      if (logger)
	{
	  svalue_id victim = svalue_id::from_int (m_svalues.length () - 1);
	  logger->log ("deleting sv%i (was sv%i)",
		       victim.as_int (),
		       map.get_src_for_dst (victim).as_int ());
	}
      delete m_svalues.pop ();
      if (stats)
	stats->m_num_svalues++;
    }

  validate ();
}

/* Renumber the svalues within this model according to MAP.  */

void
region_model::remap_svalue_ids (const svalue_id_map &map)
{
  /* Update IDs within regions.  */
  unsigned i;
  region *r;
  FOR_EACH_VEC_ELT (m_regions, i, r)
    r->remap_svalue_ids (map);

  /* Update IDs within ECs within constraints.  */
  m_constraints->remap_svalue_ids (map);

  /* Build a reordered svalues vector.  */
  auto_vec<svalue *> new_svalues (m_svalues.length ());
  for (unsigned i = 0; i < m_svalues.length (); i++)
    {
      svalue_id dst (svalue_id::from_int (i));
      svalue_id src = map.get_src_for_dst (dst);
      new_svalues.quick_push (get_svalue (src));
    }

  /* Copy over the reordered vec to m_svalues.  */
  m_svalues.truncate (0);
  gcc_assert (m_svalues.space (new_svalues.length ()));
  svalue *sval;
  FOR_EACH_VEC_ELT (new_svalues, i, sval)
    m_svalues.quick_push (sval);
}

/* Renumber the regions within this model according to MAP.  */

void
region_model::remap_region_ids (const region_id_map &map)
{
  /* Update IDs within regions.  */
  unsigned i;
  region *r;
  FOR_EACH_VEC_ELT (m_regions, i, r)
    r->remap_region_ids (map);

  /* Update IDs within svalues.  */
  svalue *sval;
  FOR_EACH_VEC_ELT (m_svalues, i, sval)
    sval->remap_region_ids (map);

  /* Build a reordered regions vector.  */
  auto_vec<region *> new_regions (m_regions.length ());
  for (unsigned i = 0; i < m_regions.length (); i++)
    {
      region_id dst (region_id::from_int (i));
      region_id src = map.get_src_for_dst (dst);
      new_regions.quick_push (get_region (src));
    }

  /* Copy over the reordered vec to m_regions.  */
  m_regions.truncate (0);
  gcc_assert (m_regions.space (new_regions.length ()));
  FOR_EACH_VEC_ELT (new_regions, i, r)
    m_regions.quick_push (r);
}

/* Delete all regions within SET_TO_PURGE, remapping region IDs for
   other regions.  It's required that there are no uses of the
   regions within the set (or the region IDs will become invalid).

   Accumulate stats to STATS.  */

void
region_model::purge_regions (const region_id_set &set_to_purge,
			     purge_stats *stats,
			     logger *)
{
  /* Build a mapping from old-rid to new-rid so that we can preserve
     order of the used IDs and move all redundant ones to the end.
     Iterate though region IDs, adding used ones to the front of
     the new list, and unused ones to the back.  */
  region_id_map map (m_regions.length ());
  int next_used_new_rid = 0;
  int after_next_unused_new_rid = m_regions.length ();
  for (unsigned i = 0; i < m_regions.length (); i++)
    {
      region_id src (region_id::from_int (i));
      if (set_to_purge.region_p (src))
	map.put (src, region_id::from_int (--after_next_unused_new_rid));
      else
	map.put (src, region_id::from_int (next_used_new_rid++));
    }
  /* The two insertion points should have met.  */
  gcc_assert (next_used_new_rid == after_next_unused_new_rid);

  /* Now walk the regions and svalues, remapping rids,
     so that all the redundant regions are at the end.  */
  remap_region_ids (map);

  /* Drop the redundant regions from the end of the vector.  */
  while ((signed)m_regions.length () > next_used_new_rid)
    {
      delete m_regions.pop ();
      if (stats)
	stats->m_num_regions++;
    }
}

/* Populate *OUT with RID and all of its descendents.
   If EXCLUDE_RID is non-null, then don't add it or its descendents.  */

void
region_model::get_descendents (region_id rid, region_id_set *out,
			       region_id exclude_rid) const
{
  out->add_region (rid);

  bool changed = true;
  while (changed)
    {
      changed = false;
      unsigned i;
      region *r;
      FOR_EACH_VEC_ELT (m_regions, i, r)
	{
	  region_id iter_rid = region_id::from_int (i);
	  if (iter_rid == exclude_rid)
	    continue;
	  if (!out->region_p (iter_rid))
	    {
	      region_id parent_rid = r->get_parent ();
	      if (!parent_rid.null_p ())
		if (out->region_p (parent_rid))
		  {
		    out->add_region (iter_rid);
		    changed = true;
		  }
	    }
	}
    }
}

/* Delete RID and all descendent regions.
   Find any pointers to such regions; convert them to
   poisoned values of kind PKIND.
   Accumulate stats on purged entities into STATS.  */

void
region_model::delete_region_and_descendents (region_id rid,
					     enum poison_kind pkind,
					     purge_stats *stats,
					     logger *logger)
{
  /* Find all child and descendent regions.  */
  region_id_set descendents (this);
  get_descendents (rid, &descendents, region_id::null ());

  /* Find any pointers to such regions; convert to poisoned.  */
  poison_any_pointers_to_bad_regions (descendents, pkind);

  /* Delete all such regions.  */
  purge_regions (descendents, stats, logger);
}

/* Find any pointers to regions within BAD_REGIONS; convert them to
   poisoned values of kind PKIND.  */

void
region_model::poison_any_pointers_to_bad_regions (const region_id_set &
						    bad_regions,
						  enum poison_kind pkind)
{
  int i;
  svalue *sval;
  FOR_EACH_VEC_ELT (m_svalues, i, sval)
    if (region_svalue *ptr_sval = sval->dyn_cast_region_svalue ())
      {
	region_id ptr_dst = ptr_sval->get_pointee ();
	if (!ptr_dst.null_p ())
	  if (bad_regions.region_p (ptr_dst))
	    replace_svalue
	      (svalue_id::from_int (i),
	       new poisoned_svalue (pkind, sval->get_type ()));
      }
}

/* Attempt to merge THIS with OTHER_MODEL, writing the result
   to OUT_MODEL, and populating SID_MAPPING.  */

bool
region_model::can_merge_with_p (const region_model &other_model,
				region_model *out_model,
				svalue_id_merger_mapping *sid_mapping) const
{
  gcc_assert (m_root_rid == other_model.m_root_rid);
  gcc_assert (m_root_rid.as_int () == 0);
  gcc_assert (sid_mapping);
  gcc_assert (out_model);

  model_merger merger (this, &other_model, out_model, sid_mapping);

  if (!root_region::can_merge_p (get_root_region (),
				 other_model.get_root_region (),
				 out_model->get_root_region (),
				 &merger))
    return false;

  /* Merge constraints.  */
  constraint_manager::merge (*m_constraints,
			     *other_model.m_constraints,
			     out_model->m_constraints,
			     merger);

  out_model->validate ();

  /* The merged model should be simpler (or as simple) as the inputs.  */
#if 0
  gcc_assert (out_model->m_svalues.length () <= m_svalues.length ());
  gcc_assert (out_model->m_svalues.length ()
	      <= other_model.m_svalues.length ());
#endif
  gcc_assert (out_model->m_regions.length () <= m_regions.length ());
  gcc_assert (out_model->m_regions.length ()
	      <= other_model.m_regions.length ());
  // TODO: same, for constraints

  return true;
}

/* As above, but supply a placeholder svalue_id_merger_mapping
   instance to be used and receive output.  For use in selftests.  */

bool
region_model::can_merge_with_p (const region_model &other_model,
				region_model *out_model) const
{
  svalue_id_merger_mapping sid_mapping (*this, other_model);
  return can_merge_with_p (other_model, out_model, &sid_mapping);
}

/* For debugging purposes: look for a region within this region_model
   for a decl named NAME (or an SSA_NAME for such a decl),
   returning its value, or svalue_id::null if none are found.  */

svalue_id
region_model::get_value_by_name (const char *name) const
{
  gcc_assert (name);
  tree identifier = get_identifier (name);
  return get_root_region ()->get_value_by_name (identifier, *this);
}

/* Generate or reuse an svalue_id within this model for an index
   into an array of type PTR_TYPE, based on OFFSET_SID.  */

svalue_id
region_model::convert_byte_offset_to_array_index (tree ptr_type,
						  svalue_id offset_sid)
{
  gcc_assert (POINTER_TYPE_P (ptr_type));

  if (tree offset_cst = maybe_get_constant (offset_sid))
    {
      tree elem_type = TREE_TYPE (ptr_type);

      /* Arithmetic on void-pointers is a GNU C extension, treating the size
	 of a void as 1.
	 https://gcc.gnu.org/onlinedocs/gcc/Pointer-Arith.html  */
      if (TREE_CODE (elem_type) == VOID_TYPE)
	return offset_sid;

      /* First, use int_size_in_bytes, to reject the case where we have an
	 incomplete type, or a non-constant value.  */
      HOST_WIDE_INT hwi_byte_size = int_size_in_bytes (elem_type);
      if (hwi_byte_size > 0)
	{
	  /* Now call size_in_bytes to get the answer in tree form.  */
	  tree byte_size = size_in_bytes (elem_type);
	  gcc_assert (byte_size);
	  /* Try to get a constant by dividing, ensuring that we're in a
	     signed representation first.  */
	  tree index
	    = fold_binary (TRUNC_DIV_EXPR, ssizetype,
			   fold_convert (ssizetype, offset_cst),
			   fold_convert (ssizetype, byte_size));
	  if (index && TREE_CODE (index) == INTEGER_CST)
	    return get_or_create_constant_svalue (index);
	}
    }

  /* Otherwise, we don't know the array index; generate a new unknown value.
     TODO: do we need to capture the relationship between two unknown
     values (the offset and the index)?  */
  return add_svalue (new unknown_svalue (integer_type_node));
}

/* Get a region of type TYPE for PTR_SID[OFFSET_SID/sizeof (*PTR_SID)].

   If OFFSET_SID is known to be zero, then dereference PTR_SID.
   Otherwise, impose a view of "typeof(*PTR_SID)[]" on *PTR_SID,
   and then get a view of type TYPE on the relevant array element.  */

region_id
region_model::get_or_create_mem_ref (tree type,
				     svalue_id ptr_sid,
				     svalue_id offset_sid,
				     region_model_context *ctxt)
{
  svalue *ptr_sval = get_svalue (ptr_sid);
  tree ptr_type = ptr_sval->get_type ();
  gcc_assert (ptr_type);

  region_id raw_rid = deref_rvalue (ptr_sid, ctxt);

  svalue *offset_sval = get_svalue (offset_sid);
  tree offset_type = offset_sval->get_type ();
  gcc_assert (offset_type);

  if (constant_svalue *cst_sval = offset_sval->dyn_cast_constant_svalue ())
    {
      if (zerop (cst_sval->get_constant ()))
	{
	  /* Handle the zero offset case.  */
	  return get_or_create_view (raw_rid, type, ctxt);
	}

      /* If we're already within an array of the correct type,
	 then we want to reuse that array, rather than starting
	 a new view.
	 If so, figure out our raw_rid's offset from its parent,
	 if we can, and use that to offset OFFSET_SID, and create
	 the element within the parent region.  */
      region *raw_reg = get_region (raw_rid);
      region_id parent_rid = raw_reg->get_parent ();
      tree parent_type = get_region (parent_rid)->get_type ();
      if (parent_type
	  && TREE_CODE (parent_type) == ARRAY_TYPE)
	{
	  // TODO: check we have the correct parent type
	  array_region *parent_array = get_region <array_region> (parent_rid);
	  array_region::key_t key_for_raw_rid;
	  if (parent_array->get_key_for_child_region (raw_rid,
						      &key_for_raw_rid))
	    {
	      /* Convert from offset to index.  */
	      svalue_id index_sid
		= convert_byte_offset_to_array_index (ptr_type, offset_sid);
	      if (tree index_cst
		    = get_svalue (index_sid)->maybe_get_constant ())
		{
		  array_region::key_t index_offset
		    = array_region::key_from_constant (index_cst);
		  array_region::key_t index_rel_to_parent
		    = key_for_raw_rid + index_offset;
		  tree index_rel_to_parent_cst
		    = wide_int_to_tree (integer_type_node,
					index_rel_to_parent);
		  svalue_id index_sid
		    = get_or_create_constant_svalue (index_rel_to_parent_cst);

		  /* Carry on, using the parent region and adjusted index.  */
		  region_id element_rid
		    = parent_array->get_element (this, raw_rid, index_sid,
						 ctxt);
		  return get_or_create_view (element_rid, type, ctxt);
		}
	    }
	}
    }

  tree array_type = build_array_type (TREE_TYPE (ptr_type),
				      integer_type_node);
  region_id array_view_rid = get_or_create_view (raw_rid, array_type, ctxt);
  array_region *array_reg = get_region <array_region> (array_view_rid);

  svalue_id index_sid
    = convert_byte_offset_to_array_index (ptr_type, offset_sid);

  region_id element_rid
    = array_reg->get_element (this, array_view_rid, index_sid, ctxt);

  return get_or_create_view (element_rid, type, ctxt);
}

/* Get a region of type TYPE for PTR_SID + OFFSET_SID.

   If OFFSET_SID is known to be zero, then dereference PTR_SID.
   Otherwise, impose a view of "typeof(*PTR_SID)[]" on *PTR_SID,
   and then get a view of type TYPE on the relevant array element.  */

region_id
region_model::get_or_create_pointer_plus_expr (tree type,
					       svalue_id ptr_sid,
					       svalue_id offset_in_bytes_sid,
					       region_model_context *ctxt)
{
  return get_or_create_mem_ref (type,
				ptr_sid,
				offset_in_bytes_sid,
				ctxt);
}

/* Get or create a view of type TYPE of the region with id RAW_ID.
   Return the id of the view (or RAW_ID if it of the same type).  */

region_id
region_model::get_or_create_view (region_id raw_rid, tree type,
				  region_model_context *ctxt)
{
  region *raw_region = get_region (raw_rid);

  gcc_assert (TYPE_P (type));
  if (type != raw_region->get_type ())
    {
      /* If the region already has a view of the requested type,
	 reuse it.  */
      region_id existing_view_rid = raw_region->get_view (type, this);
      if (!existing_view_rid.null_p ())
	return existing_view_rid;

      /* Otherwise, make one (adding it to the region_model and
	 to the viewed region).  */
      region_id view_rid = add_region_for_type (raw_rid, type, ctxt);
      raw_region->add_view (view_rid, this);
      // TODO: something to signify that this is a "view"
      return view_rid;
    }

  return raw_rid;
}

/* Attempt to get the fndecl used at CALL, if known, or NULL_TREE
   otherwise.  */

tree
region_model::get_fndecl_for_call (const gcall *call,
				   region_model_context *ctxt)
{
  tree fn_ptr = gimple_call_fn (call);
  if (fn_ptr == NULL_TREE)
    return NULL_TREE;
  svalue_id fn_ptr_sid = get_rvalue (fn_ptr, ctxt);
  svalue *fn_ptr_sval = get_svalue (fn_ptr_sid);
  if (region_svalue *fn_ptr_ptr = fn_ptr_sval->dyn_cast_region_svalue ())
    {
      region_id fn_rid = fn_ptr_ptr->get_pointee ();
      code_region *code = get_root_region ()->get_code_region (this);
      if (code)
	{
	  tree fn_decl = code->get_tree_for_child_region (fn_rid);
	  if (!fn_decl)
	    return NULL_TREE;
	  cgraph_node *node = cgraph_node::get (fn_decl);
	  if (!node)
	    return NULL_TREE;
	  const cgraph_node *ultimate_node = node->ultimate_alias_target ();
	  if (ultimate_node)
	    return ultimate_node->decl;
	}
    }

  return NULL_TREE;
}

/* struct model_merger.  */

/* Dump a multiline representation of this merger to PP.  */

void
model_merger::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "model A:");
  pp_newline (pp);
  m_model_a->dump_to_pp (pp, false);
  pp_newline (pp);

  pp_string (pp, "model B:");
  pp_newline (pp);
  m_model_b->dump_to_pp (pp, false);
  pp_newline (pp);

  pp_string (pp, "merged model:");
  pp_newline (pp);
  m_merged_model->dump_to_pp (pp, false);
  pp_newline (pp);

  pp_string (pp, "region map: model A to merged model:");
  pp_newline (pp);
  m_map_regions_from_a_to_m.dump_to_pp (pp);
  pp_newline (pp);

  pp_string (pp, "region map: model B to merged model:");
  pp_newline (pp);
  m_map_regions_from_b_to_m.dump_to_pp (pp);
  pp_newline (pp);

  m_sid_mapping->dump_to_pp (pp);
}

/* Dump a multiline representation of this merger to FILE.  */

void
model_merger::dump (FILE *fp) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Dump a multiline representation of this merger to stderr.  */

DEBUG_FUNCTION void
model_merger::dump () const
{
  dump (stderr);
}

/* Attempt to merge the svalues of SID_A and SID_B (from their
   respective models), writing the id of the resulting svalue
   into *MERGED_SID.
   Return true if the merger is possible, false otherwise.  */

bool
model_merger::can_merge_values_p (svalue_id sid_a,
				  svalue_id sid_b,
				  svalue_id *merged_sid)
{
  gcc_assert (merged_sid);
  svalue *sval_a = m_model_a->get_svalue (sid_a);
  svalue *sval_b = m_model_b->get_svalue (sid_b);

  /* If both are NULL, then the "values" are trivially mergeable.  */
  if (!sval_a && !sval_b)
    return true;

  /* If one is NULL and the other non-NULL, then the "values"
     are not mergeable.  */
  if (!(sval_a && sval_b))
    return false;

  /* Have they both already been mapped to the same new svalue_id?
     If so, use it.  */
  svalue_id sid_a_in_m
    = m_sid_mapping->m_map_from_a_to_m.get_dst_for_src (sid_a);
  svalue_id sid_b_in_m
    = m_sid_mapping->m_map_from_b_to_m.get_dst_for_src (sid_b);
  if (!sid_a_in_m.null_p ()
      && !sid_b_in_m.null_p ()
      && sid_a_in_m == sid_b_in_m)
    {
      *merged_sid = sid_a_in_m;
      return true;
    }

  tree type = sval_a->get_type ();
  if (type == NULL_TREE)
    type = sval_b->get_type ();

  /* If the values have different kinds, or are both unknown,
     then merge as "unknown".  */
  if (sval_a->get_kind () != sval_b->get_kind ()
      || sval_a->get_kind () == SK_UNKNOWN)
    {
      svalue *merged_sval = new unknown_svalue (type);
      *merged_sid = m_merged_model->add_svalue (merged_sval);
      record_svalues (sid_a, sid_b, *merged_sid);
      return true;
    }

  gcc_assert (sval_a->get_kind () == sval_b->get_kind ());

  switch (sval_a->get_kind ())
    {
    default:
    case SK_UNKNOWN: /* SK_UNKNOWN handled above.  */
       gcc_unreachable ();

    case SK_REGION:
      {
	/* If we have two region pointers, then we can merge (possibly to
	   "unknown").  */
	const region_svalue &region_sval_a = *as_a <region_svalue *> (sval_a);
	const region_svalue &region_sval_b = *as_a <region_svalue *> (sval_b);
	region_svalue::merge_values (region_sval_a, region_sval_b,
				     merged_sid, type,
				     this);
	record_svalues (sid_a, sid_b, *merged_sid);
	return true;
      }
      break;
    case SK_CONSTANT:
      {
	/* If we have two constants, then we can merge.  */
	const constant_svalue &cst_sval_a = *as_a <constant_svalue *> (sval_a);
	const constant_svalue &cst_sval_b = *as_a <constant_svalue *> (sval_b);
	constant_svalue::merge_values (cst_sval_a, cst_sval_b,
				       merged_sid, this);
	record_svalues (sid_a, sid_b, *merged_sid);
	return true;
      }
      break;

    case SK_POISONED:
    case SK_SETJMP:
      return false;
    }
}

/* Record that A_RID in model A and B_RID in model B
   correspond to MERGED_RID in the merged model, so
   that pointers can be accurately merged.  */

void
model_merger::record_regions (region_id a_rid,
			      region_id b_rid,
			      region_id merged_rid)
{
  m_map_regions_from_a_to_m.put (a_rid, merged_rid);
  m_map_regions_from_b_to_m.put (b_rid, merged_rid);
}

/* Record that A_SID in model A and B_SID in model B
   correspond to MERGED_SID in the merged model.  */

void
model_merger::record_svalues (svalue_id a_sid,
			      svalue_id b_sid,
			      svalue_id merged_sid)
{
  gcc_assert (m_sid_mapping);
  m_sid_mapping->m_map_from_a_to_m.put (a_sid, merged_sid);
  m_sid_mapping->m_map_from_b_to_m.put (b_sid, merged_sid);
}

/* struct svalue_id_merger_mapping.  */

/* svalue_id_merger_mapping's ctor.  */

svalue_id_merger_mapping::svalue_id_merger_mapping (const region_model &a,
						    const region_model &b)
: m_map_from_a_to_m (a.get_num_svalues ()),
  m_map_from_b_to_m (b.get_num_svalues ())
{
}

/* Dump a multiline representation of this to PP.  */

void
svalue_id_merger_mapping::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "svalue_id map: model A to merged model:");
  pp_newline (pp);
  m_map_from_a_to_m.dump_to_pp (pp);
  pp_newline (pp);

  pp_string (pp, "svalue_id map: model B to merged model:");
  pp_newline (pp);
  m_map_from_b_to_m.dump_to_pp (pp);
  pp_newline (pp);
}

/* Dump a multiline representation of this to FILE.  */

void
svalue_id_merger_mapping::dump (FILE *fp) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Dump a multiline representation of this to stderr.  */

DEBUG_FUNCTION void
svalue_id_merger_mapping::dump () const
{
  dump (stderr);
}

/* struct canonicalization.  */

/* canonicalization's ctor.  */

canonicalization::canonicalization (const region_model &model)
: m_model (model),
  m_rid_map (model.get_num_regions ()),
  m_sid_map (model.get_num_svalues ()),
  m_next_rid_int (0),
  m_next_sid_int (0)
{
}

/* If we've not seen RID yet, assign it a canonicalized region_id,
   and walk the region's svalue and then the region.  */

void
canonicalization::walk_rid (region_id rid)
{
  /* Stop if we've already seen RID.  */
  if (!m_rid_map.get_dst_for_src (rid).null_p ())
    return;

  region *region = m_model.get_region (rid);
  if (region)
    {
      m_rid_map.put (rid, region_id::from_int (m_next_rid_int++));
      walk_sid (region->get_value_direct ());
      region->walk_for_canonicalization (this);
    }
}

/* If we've not seen SID yet, assign it a canonicalized svalue_id,
   and walk the svalue (and potentially regions e.g. for ptr values).  */

void
canonicalization::walk_sid (svalue_id sid)
{
  /* Stop if we've already seen SID.  */
  if (!m_sid_map.get_dst_for_src (sid).null_p ())
    return;

  svalue *sval = m_model.get_svalue (sid);
  if (sval)
    {
      m_sid_map.put (sid, svalue_id::from_int (m_next_sid_int++));
      /* Potentially walk regions e.g. for ptrs.  */
      sval->walk_for_canonicalization (this);
    }
}

/* Dump a multiline representation of this to PP.  */

void
canonicalization::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "region_id map:");
  pp_newline (pp);
  m_rid_map.dump_to_pp (pp);
  pp_newline (pp);

  pp_string (pp, "svalue_id map:");
  pp_newline (pp);
  m_sid_map.dump_to_pp (pp);
  pp_newline (pp);
}

/* Dump a multiline representation of this to FILE.  */

void
canonicalization::dump (FILE *fp) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Dump a multiline representation of this to stderr.  */

DEBUG_FUNCTION void
canonicalization::dump () const
{
  dump (stderr);
}

} // namespace ana

/* Update HSTATE with a hash of SID.  */

void
inchash::add (svalue_id sid, inchash::hash &hstate)
{
  hstate.add_int (sid.as_int ());
}

/* Update HSTATE with a hash of RID.  */

void
inchash::add (region_id rid, inchash::hash &hstate)
{
  hstate.add_int (rid.as_int ());
}

/* Dump RMODEL fully to stderr (i.e. without summarization).  */

DEBUG_FUNCTION void
debug (const region_model &rmodel)
{
  rmodel.dump (false);
}

namespace ana {

#if CHECKING_P

namespace selftest {

/* Build a constant tree of the given type from STR.  */

static tree
build_real_cst_from_string (tree type, const char *str)
{
  REAL_VALUE_TYPE real;
  real_from_string (&real, str);
  return build_real (type, real);
}

/* Append various "interesting" constants to OUT (e.g. NaN).  */

static void
append_interesting_constants (auto_vec<tree> *out)
{
  out->safe_push (build_int_cst (integer_type_node, 0));
  out->safe_push (build_int_cst (integer_type_node, 42));
  out->safe_push (build_int_cst (unsigned_type_node, 0));
  out->safe_push (build_int_cst (unsigned_type_node, 42));
  out->safe_push (build_real_cst_from_string (float_type_node, "QNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-QNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "SNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-SNaN"));
  out->safe_push (build_real_cst_from_string (float_type_node, "0.0"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-0.0"));
  out->safe_push (build_real_cst_from_string (float_type_node, "Inf"));
  out->safe_push (build_real_cst_from_string (float_type_node, "-Inf"));
}

/* Verify that tree_cmp is a well-behaved comparator for qsort, even
   if the underlying constants aren't comparable.  */

static void
test_tree_cmp_on_constants ()
{
  auto_vec<tree> csts;
  append_interesting_constants (&csts);

  /* Try sorting every triple. */
  const unsigned num = csts.length ();
  for (unsigned i = 0; i < num; i++)
    for (unsigned j = 0; j < num; j++)
      for (unsigned k = 0; k < num; k++)
	{
	  auto_vec<tree> v (3);
	  v.quick_push (csts[i]);
	  v.quick_push (csts[j]);
	  v.quick_push (csts[k]);
	  v.qsort (tree_cmp);
	}
}

/* Implementation detail of the ASSERT_CONDITION_* macros.  */

void
assert_condition (const location &loc,
		  region_model &model,
		  tree lhs, tree_code op, tree rhs,
		  tristate expected)
{
  tristate actual = model.eval_condition (lhs, op, rhs, NULL);
  ASSERT_EQ_AT (loc, actual, expected);
}

/* Implementation detail of ASSERT_DUMP_TREE_EQ.  */

static void
assert_dump_tree_eq (const location &loc, tree t, const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_tree (&pp, t);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that dump_tree (T) is EXPECTED.  */

#define ASSERT_DUMP_TREE_EQ(T, EXPECTED) \
  SELFTEST_BEGIN_STMT							\
  assert_dump_tree_eq ((SELFTEST_LOCATION), (T), (EXPECTED)); \
  SELFTEST_END_STMT

/* Implementation detail of ASSERT_DUMP_EQ.  */

static void
assert_dump_eq (const location &loc,
		const region_model &model,
		bool summarize,
		const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  model.dump_to_pp (&pp, summarize);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that MODEL.dump_to_pp (SUMMARIZE) is EXPECTED.  */

#define ASSERT_DUMP_EQ(MODEL, SUMMARIZE, EXPECTED) \
  SELFTEST_BEGIN_STMT							\
  assert_dump_eq ((SELFTEST_LOCATION), (MODEL), (SUMMARIZE), (EXPECTED)); \
  SELFTEST_END_STMT

/* Smoketest for region_model::dump_to_pp.  */

static void
test_dump ()
{
  region_model model;
  model.get_root_region ()->ensure_stack_region (&model);
  model.get_root_region ()->ensure_globals_region (&model);
  model.get_root_region ()->ensure_heap_region (&model);

  ASSERT_DUMP_EQ (model, false,
		  "r0: {kind: `root', parent: null, sval: null}\n"
		  "|-stack: r1: {kind: `stack', parent: r0, sval: sv0}\n"
		  "|  |: sval: sv0: {poisoned: uninit}\n"
		  "|-globals: r2: {kind: `globals', parent: r0, sval: null, map: {}}\n"
		  "`-heap: r3: {kind: `heap', parent: r0, sval: sv1}\n"
		  "  |: sval: sv1: {poisoned: uninit}\n"
		  "svalues:\n"
		  "  sv0: {poisoned: uninit}\n"
		  "  sv1: {poisoned: uninit}\n"
		  "constraint manager:\n"
		  "  equiv classes:\n"
		  "  constraints:\n");
  ASSERT_DUMP_EQ (model, true, "");
}

/* Helper function for selftests.  Create a struct or union type named NAME,
   with the fields given by the FIELD_DECLS in FIELDS.
   If IS_STRUCT is true create a RECORD_TYPE (aka a struct), otherwise
   create a UNION_TYPE.  */

static tree
make_test_compound_type (const char *name, bool is_struct,
			 const auto_vec<tree> *fields)
{
  tree t = make_node (is_struct ? RECORD_TYPE : UNION_TYPE);
  TYPE_NAME (t) = get_identifier (name);
  TYPE_SIZE (t) = 0;

  tree fieldlist = NULL;
  int i;
  tree field;
  FOR_EACH_VEC_ELT (*fields, i, field)
    {
      gcc_assert (TREE_CODE (field) == FIELD_DECL);
      DECL_CONTEXT (field) = t;
      fieldlist = chainon (field, fieldlist);
    }
  fieldlist = nreverse (fieldlist);
  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);
  return t;
}

/* Selftest fixture for creating the type "struct coord {int x; int y; };".  */

struct coord_test
{
  coord_test ()
  {
    auto_vec<tree> fields;
    m_x_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			       get_identifier ("x"), integer_type_node);
    fields.safe_push (m_x_field);
    m_y_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			       get_identifier ("y"), integer_type_node);
    fields.safe_push (m_y_field);
    m_coord_type = make_test_compound_type ("coord", true, &fields);
  }

  tree m_x_field;
  tree m_y_field;
  tree m_coord_type;
};

/* Verify that dumps can show struct fields.  */

static void
test_dump_2 ()
{
  coord_test ct;

  tree c = build_global_decl ("c", ct.m_coord_type);
  tree c_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     c, ct.m_x_field, NULL_TREE);
  tree c_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		     c, ct.m_y_field, NULL_TREE);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree int_m3 = build_int_cst (integer_type_node, -3);

  region_model model;
  model.set_value (c_x, int_17, NULL);
  model.set_value (c_y, int_m3, NULL);

  /* Simplified dump.  */
  ASSERT_DUMP_EQ (model, true, "c.x: 17, c.y: -3");

  /* Full dump.  */
  ASSERT_DUMP_EQ
    (model, false,
     "r0: {kind: `root', parent: null, sval: null}\n"
     "`-globals: r1: {kind: `globals', parent: r0, sval: null, map: {`c': r2}}\n"
     "  `-`c': r2: {kind: `struct', parent: r1, sval: null, type: `struct coord', map: {`x': r3, `y': r4}}\n"
     "    |: type: `struct coord'\n"
     "    |-`x': r3: {kind: `primitive', parent: r2, sval: sv0, type: `int'}\n"
     "    |  |: sval: sv0: {type: `int', `17'}\n"
     "    |  |: type: `int'\n"
     "    `-`y': r4: {kind: `primitive', parent: r2, sval: sv1, type: `int'}\n"
     "      |: sval: sv1: {type: `int', `-3'}\n"
     "      |: type: `int'\n"
     "svalues:\n"
     "  sv0: {type: `int', `17'}\n"
     "  sv1: {type: `int', `-3'}\n"
     "constraint manager:\n"
     "  equiv classes:\n"
     "  constraints:\n");
}

/* Verify that dumps can show array elements.  */

static void
test_dump_3 ()
{
  tree tlen = size_int (10);
  tree arr_type = build_array_type (char_type_node, build_index_type (tlen));

  tree a = build_global_decl ("a", arr_type);

  region_model model;
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree a_0 = build4 (ARRAY_REF, char_type_node,
		     a, int_0, NULL_TREE, NULL_TREE);
  tree char_A = build_int_cst (char_type_node, 'A');
  model.set_value (a_0, char_A, NULL);

  /* Simplified dump.  */
  ASSERT_DUMP_EQ (model, true, "a[0]: 65");

  /* Full dump.  */
  ASSERT_DUMP_EQ
    (model, false,
     "r0: {kind: `root', parent: null, sval: null}\n"
     "`-globals: r1: {kind: `globals', parent: r0, sval: null, map: {`a': r2}}\n"
     "  `-`a': r2: {kind: `array', parent: r1, sval: null, type: `char[11]', array: {[0]: r3}}\n"
     "    |: type: `char[11]'\n"
     "    `-[0]: r3: {kind: `primitive', parent: r2, sval: sv1, type: `char'}\n"
     "      |: sval: sv1: {type: `char', `65'}\n"
     "      |: type: `char'\n"
     "svalues:\n"
     "  sv0: {type: `int', `0'}\n"
     "  sv1: {type: `char', `65'}\n"
     "constraint manager:\n"
     "  equiv classes:\n"
     "  constraints:\n");
}

/* Verify that region_model::get_representative_tree works as expected.  */

static void
test_get_representative_tree ()
{
  /* STRING_CST.  */
  {
    tree string_cst = build_string (4, "foo");
    region_model m;
    svalue_id str_sid = m.get_rvalue (string_cst, NULL);
    tree rep = m.get_representative_tree (str_sid);
    ASSERT_EQ (rep, string_cst);
  }

  /* String literal.  */
  {
    tree string_cst_ptr = build_string_literal (4, "foo");
    region_model m;
    svalue_id str_sid = m.get_rvalue (string_cst_ptr, NULL);
    tree rep = m.get_representative_tree (str_sid);
    ASSERT_DUMP_TREE_EQ (rep, "&\"foo\"[0]");
  }
}

/* Verify that calling region_model::get_rvalue repeatedly on the same
   tree constant retrieves the same svalue_id.  */

static void
test_unique_constants ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_42 = build_int_cst (integer_type_node, 42);

  test_region_model_context ctxt;
  region_model model;
  ASSERT_EQ (model.get_rvalue (int_0, &ctxt), model.get_rvalue (int_0, &ctxt));
  ASSERT_EQ (model.get_rvalue (int_42, &ctxt),
	     model.get_rvalue (int_42, &ctxt));
  ASSERT_NE (model.get_rvalue (int_0, &ctxt), model.get_rvalue (int_42, &ctxt));
  ASSERT_EQ (ctxt.get_num_diagnostics (), 0);
}

/* Check that operator== and hashing works as expected for the
   various svalue subclasses.  */

static void
test_svalue_equality ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_0 = build_int_cst (integer_type_node, 0);

  /* Create pairs instances of the various subclasses of svalue,
     testing for hash and equality between (this, this) and
     (this, other of same subclass). */
  svalue *ptr_to_r0
    = new region_svalue (ptr_type_node, region_id::from_int (0));
  svalue *ptr_to_r1
    = new region_svalue (ptr_type_node, region_id::from_int (1));

  ASSERT_EQ (ptr_to_r0->hash (), ptr_to_r0->hash ());
  ASSERT_EQ (*ptr_to_r0, *ptr_to_r0);

  ASSERT_NE (ptr_to_r0->hash (), ptr_to_r1->hash ());
  ASSERT_NE (*ptr_to_r0, *ptr_to_r1);

  svalue *cst_int_42 = new constant_svalue (int_42);
  svalue *cst_int_0 = new constant_svalue (int_0);

  ASSERT_EQ (cst_int_42->hash (), cst_int_42->hash ());
  ASSERT_EQ (*cst_int_42, *cst_int_42);

  ASSERT_NE (cst_int_42->hash (), cst_int_0->hash ());
  ASSERT_NE (*cst_int_42, *cst_int_0);

  svalue *uninit = new poisoned_svalue (POISON_KIND_UNINIT, NULL_TREE);
  svalue *freed = new poisoned_svalue (POISON_KIND_FREED, NULL_TREE);

  ASSERT_EQ (uninit->hash (), uninit->hash ());
  ASSERT_EQ (*uninit, *uninit);

  ASSERT_NE (uninit->hash (), freed->hash ());
  ASSERT_NE (*uninit, *freed);

  svalue *unknown_0 = new unknown_svalue (ptr_type_node);
  svalue *unknown_1 = new unknown_svalue (ptr_type_node);
  ASSERT_EQ (unknown_0->hash (), unknown_0->hash ());
  ASSERT_EQ (*unknown_0, *unknown_0);
  ASSERT_EQ (*unknown_1, *unknown_1);

  /* Comparisons between different kinds of svalue.  */
  ASSERT_NE (*ptr_to_r0, *cst_int_42);
  ASSERT_NE (*ptr_to_r0, *uninit);
  ASSERT_NE (*ptr_to_r0, *unknown_0);
  ASSERT_NE (*cst_int_42, *ptr_to_r0);
  ASSERT_NE (*cst_int_42, *uninit);
  ASSERT_NE (*cst_int_42, *unknown_0);
  ASSERT_NE (*uninit, *ptr_to_r0);
  ASSERT_NE (*uninit, *cst_int_42);
  ASSERT_NE (*uninit, *unknown_0);
  ASSERT_NE (*unknown_0, *ptr_to_r0);
  ASSERT_NE (*unknown_0, *cst_int_42);
  ASSERT_NE (*unknown_0, *uninit);

  delete ptr_to_r0;
  delete ptr_to_r1;
  delete cst_int_42;
  delete cst_int_0;
  delete uninit;
  delete freed;
  delete unknown_0;
  delete unknown_1;
}

/* Check that operator== and hashing works as expected for the
   various region subclasses.  */

static void
test_region_equality ()
{
  region *r0
    = new primitive_region (region_id::from_int (3), integer_type_node);
  region *r1
    = new primitive_region (region_id::from_int (4), integer_type_node);

  ASSERT_EQ (*r0, *r0);
  ASSERT_EQ (r0->hash (), r0->hash ());
  ASSERT_NE (*r0, *r1);
  ASSERT_NE (r0->hash (), r1->hash ());

  delete r0;
  delete r1;

  // TODO: test coverage for the map within a map_region
}

/* A subclass of purge_criteria for selftests: purge all svalue_id instances.  */

class purge_all_svalue_ids : public purge_criteria
{
public:
  bool should_purge_p (svalue_id) const FINAL OVERRIDE
  {
    return true;
  }
};

/* A subclass of purge_criteria: purge a specific svalue_id.  */

class purge_one_svalue_id : public purge_criteria
{
public:
  purge_one_svalue_id (svalue_id victim) : m_victim (victim) {}

  purge_one_svalue_id (region_model model, tree expr)
  : m_victim (model.get_rvalue (expr, NULL)) {}

  bool should_purge_p (svalue_id sid) const FINAL OVERRIDE
  {
    return sid == m_victim;
  }

private:
  svalue_id m_victim;
};

/* Check that constraint_manager::purge works for individual svalue_ids.  */

static void
test_purging_by_criteria ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_0 = build_int_cst (integer_type_node, 0);

  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  {
    region_model model0;
    region_model model1;

    ADD_SAT_CONSTRAINT (model1, x, EQ_EXPR, y);
    ASSERT_NE (model0, model1);

    purge_stats stats_for_px;
    purge_one_svalue_id px (model1, x);
    model1.get_constraints ()->purge (px, &stats_for_px);
    ASSERT_EQ (stats_for_px.m_num_equiv_classes, 0);

    purge_stats stats_for_py;
    purge_one_svalue_id py (model1.get_rvalue (y, NULL));
    model1.get_constraints ()->purge (py, &stats_for_py);
    ASSERT_EQ (stats_for_py.m_num_equiv_classes, 1);

    ASSERT_EQ (*model0.get_constraints (), *model1.get_constraints ());
  }

  {
    region_model model0;
    region_model model1;

    ADD_SAT_CONSTRAINT (model1, x, EQ_EXPR, int_42);
    ASSERT_NE (model0, model1);
    ASSERT_CONDITION_TRUE (model1, x, EQ_EXPR, int_42);

    purge_stats stats;
    model1.get_constraints ()->purge (purge_one_svalue_id (model1, x), &stats);

    ASSERT_CONDITION_UNKNOWN (model1, x, EQ_EXPR, int_42);
  }

  {
    region_model model0;
    region_model model1;

    ADD_SAT_CONSTRAINT (model1, x, GE_EXPR, int_0);
    ADD_SAT_CONSTRAINT (model1, x, LE_EXPR, int_42);
    ASSERT_NE (model0, model1);

    ASSERT_CONDITION_TRUE (model1, x, GE_EXPR, int_0);
    ASSERT_CONDITION_TRUE (model1, x, LE_EXPR, int_42);

    purge_stats stats;
    model1.get_constraints ()->purge (purge_one_svalue_id (model1, x), &stats);

    ASSERT_CONDITION_UNKNOWN (model1, x, GE_EXPR, int_0);
    ASSERT_CONDITION_UNKNOWN (model1, x, LE_EXPR, int_42);
  }

  {
    region_model model0;
    region_model model1;

    ADD_SAT_CONSTRAINT (model1, x, NE_EXPR, int_42);
    ADD_SAT_CONSTRAINT (model1, y, NE_EXPR, int_0);
    ASSERT_NE (model0, model1);
    ASSERT_CONDITION_TRUE (model1, x, NE_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model1, y, NE_EXPR, int_0);

    purge_stats stats;
    model1.get_constraints ()->purge (purge_one_svalue_id (model1, x), &stats);
    ASSERT_NE (model0, model1);

    ASSERT_CONDITION_UNKNOWN (model1, x, NE_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model1, y, NE_EXPR, int_0);
  }

  {
    region_model model0;
    region_model model1;

    ADD_SAT_CONSTRAINT (model1, x, NE_EXPR, int_42);
    ADD_SAT_CONSTRAINT (model1, y, NE_EXPR, int_0);
    ASSERT_NE (model0, model1);
    ASSERT_CONDITION_TRUE (model1, x, NE_EXPR, int_42);
    ASSERT_CONDITION_TRUE (model1, y, NE_EXPR, int_0);

    purge_stats stats;
    model1.get_constraints ()->purge (purge_all_svalue_ids (), &stats);
    ASSERT_CONDITION_UNKNOWN (model1, x, NE_EXPR, int_42);
    ASSERT_CONDITION_UNKNOWN (model1, y, NE_EXPR, int_0);
  }

}

/* Test that region_model::purge_unused_svalues works as expected.  */

static void
test_purge_unused_svalues ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  test_region_model_context ctxt;
  region_model model;
  model.set_to_new_unknown_value (model.get_lvalue (x, &ctxt), TREE_TYPE (x),
				  &ctxt);
  model.set_to_new_unknown_value (model.get_lvalue (x, &ctxt), TREE_TYPE (x),
				  &ctxt);
  model.set_to_new_unknown_value (model.get_lvalue (x, &ctxt), TREE_TYPE (x),
				  &ctxt);
  model.add_constraint (x, NE_EXPR, int_42, &ctxt);

  model.set_value (model.get_lvalue (x, &ctxt),
		   model.get_rvalue (int_42, &ctxt),
		   &ctxt);
  model.add_constraint (y, GT_EXPR, int_0, &ctxt);

  /* The redundant unknown values should have been purged.  */
  purge_stats purged;
  model.purge_unused_svalues (&purged, NULL);
  ASSERT_EQ (purged.m_num_svalues, 3);

  /* and the redundant constraint on an old, unknown value for x should
     have been purged.  */
  ASSERT_EQ (purged.m_num_equiv_classes, 1);
  ASSERT_EQ (purged.m_num_constraints, 1);
  ASSERT_EQ (model.get_constraints ()->m_constraints.length (), 2);

  /* ...but we should still have x == 42.  */
  ASSERT_EQ (model.eval_condition (x, EQ_EXPR, int_42, &ctxt),
	     tristate::TS_TRUE);

  /* ...and we should still have the constraint on y.  */
  ASSERT_EQ (model.eval_condition (y, GT_EXPR, int_0, &ctxt),
	     tristate::TS_TRUE);

  ASSERT_EQ (ctxt.get_num_diagnostics (), 0);
}

/* Verify that simple assignments work as expected.  */

static void
test_assignment ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  /* "x == 0", then use of y, then "y = 0;".  */
  region_model model;
  ADD_SAT_CONSTRAINT (model, x, EQ_EXPR, int_0);
  ASSERT_CONDITION_UNKNOWN (model, y, EQ_EXPR, int_0);
  model.set_value (model.get_lvalue (y, NULL),
		   model.get_rvalue (int_0, NULL),
		   NULL);
  ASSERT_CONDITION_TRUE (model, y, EQ_EXPR, int_0);
  ASSERT_CONDITION_TRUE (model, y, EQ_EXPR, x);

  ASSERT_DUMP_EQ (model, true, "y: 0, {x}: unknown, x == y");
}

/* Verify that compound assignments work as expected.  */

static void
test_compound_assignment ()
{
  coord_test ct;

  tree c = build_global_decl ("c", ct.m_coord_type);
  tree c_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     c, ct.m_x_field, NULL_TREE);
  tree c_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		     c, ct.m_y_field, NULL_TREE);
  tree d = build_global_decl ("d", ct.m_coord_type);
  tree d_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		     d, ct.m_x_field, NULL_TREE);
  tree d_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		     d, ct.m_y_field, NULL_TREE);

  tree int_17 = build_int_cst (integer_type_node, 17);
  tree int_m3 = build_int_cst (integer_type_node, -3);

  region_model model;
  model.set_value (c_x, int_17, NULL);
  model.set_value (c_y, int_m3, NULL);

  ASSERT_DUMP_EQ (model, true, "c.x: 17, c.y: -3");

  /* Copy c to d.  */
  model.copy_region (model.get_lvalue (d, NULL), model.get_lvalue (c, NULL),
		     NULL);
  /* Check that the fields have the same svalues.  */
  ASSERT_EQ (model.get_rvalue (c_x, NULL), model.get_rvalue (d_x, NULL));
  ASSERT_EQ (model.get_rvalue (c_y, NULL), model.get_rvalue (d_y, NULL));
}

/* Verify the details of pushing and popping stack frames.  */

static void
test_stack_frames ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_10 = build_int_cst (integer_type_node, 10);
  tree int_5 = build_int_cst (integer_type_node, 5);
  tree int_0 = build_int_cst (integer_type_node, 0);

  auto_vec <tree> param_types;
  tree parent_fndecl = make_fndecl (integer_type_node,
				    "parent_fn",
				    param_types);
  allocate_struct_function (parent_fndecl, true);

  tree child_fndecl = make_fndecl (integer_type_node,
				   "child_fn",
				   param_types);
  allocate_struct_function (child_fndecl, true);

  /* "a" and "b" in the parent frame.  */
  tree a = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("a"),
		       integer_type_node);
  tree b = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("b"),
		       integer_type_node);
  /* "x" and "y" in a child frame.  */
  tree x = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("x"),
		       integer_type_node);
  tree y = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("y"),
		       integer_type_node);

  /* "p" global.  */
  tree p = build_global_decl ("p", ptr_type_node);

  /* "q" global.  */
  tree q = build_global_decl ("q", ptr_type_node);

  test_region_model_context ctxt;
  region_model model;

  /* Push stack frame for "parent_fn".  */
  region_id parent_frame_rid
    = model.push_frame (DECL_STRUCT_FUNCTION (parent_fndecl), NULL, &ctxt);
  ASSERT_EQ (model.get_current_frame_id (), parent_frame_rid);
  region_id a_in_parent_rid = model.get_lvalue (a, &ctxt);
  model.set_value (a_in_parent_rid, model.get_rvalue (int_42, &ctxt), &ctxt);
  model.set_to_new_unknown_value (model.get_lvalue (b, &ctxt),
				  integer_type_node, &ctxt);
  model.add_constraint (b, LT_EXPR, int_10, &ctxt);
  ASSERT_EQ (model.eval_condition (b, LT_EXPR, int_10, &ctxt),
	     tristate (tristate::TS_TRUE));

  /* Push stack frame for "child_fn".  */
  region_id child_frame_rid
    = model.push_frame (DECL_STRUCT_FUNCTION (child_fndecl), NULL, &ctxt);
  ASSERT_EQ (model.get_current_frame_id (), child_frame_rid);
  region_id x_in_child_rid = model.get_lvalue (x, &ctxt);
  model.set_value (x_in_child_rid, model.get_rvalue (int_0, &ctxt), &ctxt);
  model.set_to_new_unknown_value (model.get_lvalue (y, &ctxt),
				  integer_type_node, &ctxt);
  model.add_constraint (y, NE_EXPR, int_5, &ctxt);
  ASSERT_EQ (model.eval_condition (y, NE_EXPR, int_5, &ctxt),
	     tristate (tristate::TS_TRUE));

  /* Point a global pointer at a local in the child frame:  p = &x.  */
  region_id p_in_globals_rid = model.get_lvalue (p, &ctxt);
  model.set_value (p_in_globals_rid,
		   model.get_or_create_ptr_svalue (ptr_type_node,
						   x_in_child_rid),
		   &ctxt);

  /* Point another global pointer at p: q = &p.  */
  region_id q_in_globals_rid = model.get_lvalue (q, &ctxt);
  model.set_value (q_in_globals_rid,
		   model.get_or_create_ptr_svalue (ptr_type_node,
						   p_in_globals_rid),
		   &ctxt);

  /* Test get_descendents.  */
  region_id_set descendents (&model);
  model.get_descendents (child_frame_rid, &descendents, region_id::null ());
  ASSERT_TRUE (descendents.region_p (child_frame_rid));
  ASSERT_TRUE (descendents.region_p (x_in_child_rid));
  ASSERT_FALSE (descendents.region_p (a_in_parent_rid));
  ASSERT_EQ (descendents.num_regions (), 3);
#if 0
  auto_vec<region_id> test_vec;
  for (region_id_set::iterator_t iter = descendents.begin ();
       iter != descendents.end ();
       ++iter)
    test_vec.safe_push (*iter);
  gcc_unreachable (); // TODO
  //ASSERT_EQ ();
#endif

  ASSERT_DUMP_EQ (model, true,
		  "a: 42, x: 0, p: &x, q: &p, {b, y}: unknown, b < 10, y != 5");

  /* Pop the "child_fn" frame from the stack.  */
  purge_stats purged;
  model.pop_frame (region_id::null (), true, &purged, &ctxt);

  /* We should have purged the unknown values for x and y. */
  ASSERT_EQ (purged.m_num_svalues, 2);

  /* We should have purged the frame region and the regions for x and y. */
  ASSERT_EQ (purged.m_num_regions, 3);

  /* We should have purged the constraint on y.  */
  ASSERT_EQ (purged.m_num_equiv_classes, 1);
  ASSERT_EQ (purged.m_num_constraints, 1);

  /* Verify that p (which was pointing at the local "x" in the popped
     frame) has been poisoned.  */
  svalue *new_p_sval = model.get_svalue (model.get_rvalue (p, &ctxt));
  ASSERT_EQ (new_p_sval->get_kind (), SK_POISONED);
  ASSERT_EQ (new_p_sval->dyn_cast_poisoned_svalue ()->get_poison_kind (),
	     POISON_KIND_POPPED_STACK);

  /* Verify that q still points to p, in spite of the region
     renumbering.  */
  svalue *new_q_sval = model.get_svalue (model.get_rvalue (q, &ctxt));
  ASSERT_EQ (new_q_sval->get_kind (), SK_REGION);
  ASSERT_EQ (new_q_sval->dyn_cast_region_svalue ()->get_pointee (),
	     model.get_lvalue (p, &ctxt));

  /* Verify that top of stack has been updated.  */
  ASSERT_EQ (model.get_current_frame_id (), parent_frame_rid);

  /* Verify locals in parent frame.  */
  /* Verify "a" still has its value.  */
  svalue *new_a_sval = model.get_svalue (model.get_rvalue (a, &ctxt));
  ASSERT_EQ (new_a_sval->get_kind (), SK_CONSTANT);
  ASSERT_EQ (new_a_sval->dyn_cast_constant_svalue ()->get_constant (),
	     int_42);
  /* Verify "b" still has its constraint.  */
  ASSERT_EQ (model.eval_condition (b, LT_EXPR, int_10, &ctxt),
	     tristate (tristate::TS_TRUE));
}

/* Verify that get_representative_path_var works as expected, that
   we can map from region ids to parms and back within a recursive call
   stack.  */

static void
test_get_representative_path_var ()
{
  auto_vec <tree> param_types;
  tree fndecl = make_fndecl (integer_type_node,
			     "factorial",
			     param_types);
  allocate_struct_function (fndecl, true);

  /* Parm "n".  */
  tree n = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("n"),
		       integer_type_node);

  region_model model;

  /* Push 5 stack frames for "factorial", each with a param  */
  auto_vec<region_id> parm_rids;
  auto_vec<svalue_id> parm_sids;
  for (int depth = 0; depth < 5; depth++)
    {
      region_id frame_rid
	= model.push_frame (DECL_STRUCT_FUNCTION (fndecl), NULL, NULL);
      region_id rid_n = model.get_lvalue (path_var (n, depth), NULL);
      parm_rids.safe_push (rid_n);

      ASSERT_EQ (model.get_region (rid_n)->get_parent (), frame_rid);

      svalue_id sid_n
	= model.set_to_new_unknown_value (rid_n, integer_type_node, NULL);
      parm_sids.safe_push (sid_n);
    }

  /* Verify that we can recognize that the regions are the parms,
     at every depth.  */
  for (int depth = 0; depth < 5; depth++)
    {
      ASSERT_EQ (model.get_representative_path_var (parm_rids[depth]),
		 path_var (n, depth));
      /* ...and that we can lookup lvalues for locals for all frames,
	 not just the top.  */
      ASSERT_EQ (model.get_lvalue (path_var (n, depth), NULL),
		 parm_rids[depth]);
      /* ...and that we can locate the svalues.  */
      auto_vec<path_var> pvs;
      model.get_path_vars_for_svalue (parm_sids[depth], &pvs);
      ASSERT_EQ (pvs.length (), 1);
      ASSERT_EQ (pvs[0], path_var (n, depth));
    }
}

/* Verify that the core regions within a region_model are in a consistent
   order after canonicalization.  */

static void
test_canonicalization_1 ()
{
  region_model model0;
  model0.get_root_region ()->ensure_stack_region (&model0);
  model0.get_root_region ()->ensure_globals_region (&model0);

  region_model model1;
  model1.get_root_region ()->ensure_globals_region (&model1);
  model1.get_root_region ()->ensure_stack_region (&model1);

  model0.canonicalize (NULL);
  model1.canonicalize (NULL);
  ASSERT_EQ (model0, model1);
}

/* Verify that region models for
      x = 42; y = 113;
   and
      y = 113; x = 42;
   are equal after canonicalization.  */

static void
test_canonicalization_2 ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_113 = build_int_cst (integer_type_node, 113);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  region_model model0;
  model0.set_value (model0.get_lvalue (x, NULL),
		    model0.get_rvalue (int_42, NULL),
		    NULL);
  model0.set_value (model0.get_lvalue (y, NULL),
		    model0.get_rvalue (int_113, NULL),
		    NULL);

  region_model model1;
  model1.set_value (model1.get_lvalue (y, NULL),
		    model1.get_rvalue (int_113, NULL),
		    NULL);
  model1.set_value (model1.get_lvalue (x, NULL),
		    model1.get_rvalue (int_42, NULL),
		    NULL);

  model0.canonicalize (NULL);
  model1.canonicalize (NULL);
  ASSERT_EQ (model0, model1);
}

/* Verify that constraints for
     x > 3 && y > 42
   and
     y > 42 && x > 3
   are equal after canonicalization.  */

static void
test_canonicalization_3 ()
{
  tree int_3 = build_int_cst (integer_type_node, 3);
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);

  region_model model0;
  model0.add_constraint (x, GT_EXPR, int_3, NULL);
  model0.add_constraint (y, GT_EXPR, int_42, NULL);

  region_model model1;
  model1.add_constraint (y, GT_EXPR, int_42, NULL);
  model1.add_constraint (x, GT_EXPR, int_3, NULL);

  model0.canonicalize (NULL);
  model1.canonicalize (NULL);
  ASSERT_EQ (model0, model1);
}

/* Verify that we can canonicalize a model containing NaN and other real
   constants.  */

static void
test_canonicalization_4 ()
{
  auto_vec<tree> csts;
  append_interesting_constants (&csts);

  region_model model;

  unsigned i;
  tree cst;
  FOR_EACH_VEC_ELT (csts, i, cst)
    model.get_rvalue (cst, NULL);

  model.canonicalize (NULL);
}

/* Assert that if we have two region_model instances
   with values VAL_A and VAL_B for EXPR that they are
   mergable.  Write the merged model to *OUT_MERGED_MODEL,
   and the merged svalue ptr to *OUT_MERGED_SVALUE.
   If VAL_A or VAL_B are NULL_TREE, don't populate EXPR
   for that region_model.  */

static void
assert_region_models_merge (tree expr, tree val_a, tree val_b,
			    region_model *out_merged_model,
			    svalue **out_merged_svalue)
{
  test_region_model_context ctxt;
  region_model model0;
  region_model model1;
  if (val_a)
    model0.set_value (model0.get_lvalue (expr, &ctxt),
		      model0.get_rvalue (val_a, &ctxt),
		      &ctxt);
  if (val_b)
    model1.set_value (model1.get_lvalue (expr, &ctxt),
		      model1.get_rvalue (val_b, &ctxt),
		      &ctxt);

  /* They should be mergeable.  */
  ASSERT_TRUE (model0.can_merge_with_p (model1, out_merged_model));

  svalue_id merged_svalue_sid = out_merged_model->get_rvalue (expr, &ctxt);
  *out_merged_svalue = out_merged_model->get_svalue (merged_svalue_sid);
}

/* Verify that we can merge region_model instances.  */

static void
test_state_merging ()
{
  tree int_42 = build_int_cst (integer_type_node, 42);
  tree int_113 = build_int_cst (integer_type_node, 113);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);
  tree p = build_global_decl ("p", ptr_type_node);

  tree addr_of_y = build1 (ADDR_EXPR, ptr_type_node, y);
  tree addr_of_z = build1 (ADDR_EXPR, ptr_type_node, z);

  auto_vec <tree> param_types;
  tree test_fndecl = make_fndecl (integer_type_node, "test_fn", param_types);
  allocate_struct_function (test_fndecl, true);

  /* Param "a".  */
  tree a = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("a"),
		       integer_type_node);
  tree addr_of_a = build1 (ADDR_EXPR, ptr_type_node, a);

  /* Param "q", a pointer.  */
  tree q = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		       get_identifier ("q"),
		       ptr_type_node);

  {
    region_model model0;
    region_model model1;
    region_model merged;
    /* Verify empty models can be merged.  */
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    ASSERT_EQ (model0, merged);
  }

  /* Verify that we can merge two contradictory constraints on the
     value for a global.  */
  /* TODO: verify that the merged model doesn't have a value for
     the global  */
  {
    region_model model0;
    region_model model1;
    region_model merged;
    test_region_model_context ctxt;
    model0.add_constraint (x, EQ_EXPR, int_42, &ctxt);
    model1.add_constraint (x, EQ_EXPR, int_113, &ctxt);
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    ASSERT_NE (model0, merged);
    ASSERT_NE (model1, merged);
  }

  /* Verify handling of a PARM_DECL.  */
  {
    test_region_model_context ctxt;
    region_model model0;
    region_model model1;
    ASSERT_EQ (model0.get_stack_depth (), 0);
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, &ctxt);
    ASSERT_EQ (model0.get_stack_depth (), 1);
    ASSERT_EQ (model0.get_function_at_depth (0),
	       DECL_STRUCT_FUNCTION (test_fndecl));
    model1.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, &ctxt);

    svalue_id sid_a
      = model0.set_to_new_unknown_value (model0.get_lvalue (a, &ctxt),
					 integer_type_node, &ctxt);
    model1.set_to_new_unknown_value (model1.get_lvalue (a, &ctxt),
				     integer_type_node, &ctxt);
    ASSERT_EQ (model0, model1);

    /* Check that get_value_by_name works for locals.  */
    ASSERT_EQ (model0.get_value_by_name ("a"), sid_a);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    ASSERT_EQ (model0, merged);
    /* In particular, there should be an unknown value for "a".  */
    svalue *merged_a_sval = merged.get_svalue (merged.get_rvalue (a, &ctxt));
    ASSERT_EQ (merged_a_sval->get_kind (), SK_UNKNOWN);
  }

  /* Verify handling of a global.  */
  {
    test_region_model_context ctxt;
    region_model model0;
    region_model model1;
    svalue_id sid_x
      = model0.set_to_new_unknown_value (model0.get_lvalue (x, &ctxt),
					 integer_type_node, &ctxt);
    model1.set_to_new_unknown_value (model1.get_lvalue (x, &ctxt),
				     integer_type_node, &ctxt);
    ASSERT_EQ (model0, model1);

    /* Check that get_value_by_name works for globals.  */
    ASSERT_EQ (model0.get_value_by_name ("x"), sid_x);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    ASSERT_EQ (model0, merged);
    /* In particular, there should be an unknown value for "x".  */
    svalue *merged_x_sval = merged.get_svalue (merged.get_rvalue (x, &ctxt));
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Use global-handling to verify various combinations of values.  */

  /* Two equal constant values.  */
  {
    region_model merged;
    svalue *merged_x_sval;
    assert_region_models_merge (x, int_42, int_42, &merged, &merged_x_sval);

    /* In particular, there should be a constant value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_CONSTANT);
    ASSERT_EQ (merged_x_sval->dyn_cast_constant_svalue ()->get_constant (),
	       int_42);
  }

  /* Two non-equal constant values.  */
  {
    region_model merged;
    svalue *merged_x_sval;
    assert_region_models_merge (x, int_42, int_113, &merged, &merged_x_sval);

    /* In particular, there should be an unknown value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Uninit and constant.  */
  {
    region_model merged;
    svalue *merged_x_sval;
    assert_region_models_merge (x, NULL_TREE, int_113, &merged, &merged_x_sval);

    /* In particular, there should be an unknown value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Constant and uninit.  */
  {
    region_model merged;
    svalue *merged_x_sval;
    assert_region_models_merge (x, int_42, NULL_TREE, &merged, &merged_x_sval);

    /* In particular, there should be an unknown value for "x".  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Unknown and constant.  */
  // TODO

  /* Pointers: NULL and NULL.  */
  // TODO

  /* Pointers: NULL and non-NULL.  */
  // TODO

  /* Pointers: non-NULL and non-NULL: ptr to a local.  */
  {
    region_model model0;
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);
    model0.set_to_new_unknown_value (model0.get_lvalue (a, NULL),
				     integer_type_node, NULL);
    model0.set_value (model0.get_lvalue (p, NULL),
		      model0.get_rvalue (addr_of_a, NULL), NULL);

    region_model model1 (model0);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    ASSERT_EQ (model0, merged);
  }

  /* Pointers: non-NULL and non-NULL: ptr to a global.  */
  {
    region_model merged;
    /* p == &y in both input models.  */
    svalue *merged_p_sval;
    assert_region_models_merge (p, addr_of_y, addr_of_y, &merged,
				&merged_p_sval);

    /* We should get p == &y in the merged model.  */
    ASSERT_EQ (merged_p_sval->get_kind (), SK_REGION);
    region_svalue *merged_p_ptr = merged_p_sval->dyn_cast_region_svalue ();
    region_id merged_p_star_rid = merged_p_ptr->get_pointee ();
    ASSERT_EQ (merged_p_star_rid, merged.get_lvalue (y, NULL));
  }

  /* Pointers: non-NULL ptrs to different globals: should be unknown.  */
  {
    region_model merged;
    /* x == &y vs x == &z in the input models.  */
    svalue *merged_x_sval;
    assert_region_models_merge (x, addr_of_y, addr_of_z, &merged,
				&merged_x_sval);

    /* We should get x == unknown in the merged model.  */
    ASSERT_EQ (merged_x_sval->get_kind (), SK_UNKNOWN);
  }

  /* Pointers: non-NULL and non-NULL: ptr to a heap region.  */
  {
    test_region_model_context ctxt;
    region_model model0;
    region_id new_rid = model0.add_new_malloc_region ();
    svalue_id ptr_sid
      = model0.get_or_create_ptr_svalue (ptr_type_node, new_rid);
    model0.set_value (model0.get_lvalue (p, &ctxt),
		      ptr_sid, &ctxt);
    model0.canonicalize (&ctxt);

    region_model model1 (model0);

    ASSERT_EQ (model0, model1);

    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));

    merged.canonicalize (&ctxt);

    /* The merged model ought to be identical (after canonicalization,
       at least).  */
    ASSERT_EQ (model0, merged);
  }

  /* Two regions sharing the same unknown svalue should continue sharing
     an unknown svalue after self-merger.  */
  {
    test_region_model_context ctxt;
    region_model model0;
    svalue_id sid
      = model0.set_to_new_unknown_value (model0.get_lvalue (x, &ctxt),
					 integer_type_node, &ctxt);
    model0.set_value (model0.get_lvalue (y, &ctxt), sid, &ctxt);
    region_model model1 (model0);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    ASSERT_EQ (model0, merged);

    /* In particular, we should have x == y.  */
    ASSERT_EQ (merged.eval_condition (x, EQ_EXPR, y, &ctxt),
	       tristate (tristate::TS_TRUE));
  }

#if 0
  {
    region_model model0;
    region_model model1;
    test_region_model_context ctxt;
    model0.add_constraint (x, EQ_EXPR, int_42, &ctxt);
    model1.add_constraint (x, NE_EXPR, int_42, &ctxt);
    ASSERT_TRUE (model0.can_merge_with_p (model1));
  }

  {
    region_model model0;
    region_model model1;
    test_region_model_context ctxt;
    model0.add_constraint (x, EQ_EXPR, int_42, &ctxt);
    model1.add_constraint (x, NE_EXPR, int_42, &ctxt);
    model1.add_constraint (x, EQ_EXPR, int_113, &ctxt);
    ASSERT_TRUE (model0.can_merge_with_p (model1));
  }
#endif

  // TODO: what can't we merge? need at least one such test

  /* TODO: various things
     - heap regions
     - value merging:
       - every combination, but in particular
          - pairs of regions
   */

  /* Views.  */
  {
    test_region_model_context ctxt;
    region_model model0;

    region_id x_rid = model0.get_lvalue (x, &ctxt);
    region_id x_as_ptr = model0.get_or_create_view (x_rid, ptr_type_node,
						    &ctxt);
    model0.set_value (x_as_ptr, model0.get_rvalue (addr_of_y, &ctxt), &ctxt);

    region_model model1 (model0);
    ASSERT_EQ (model1, model0);

    /* They should be mergeable, and the result should be the same.  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
  }

  /* Verify that we can merge a model in which a local in an older stack
     frame points to a local in a more recent stack frame.  */
  {
    region_model model0;
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);
    region_id q_in_first_frame = model0.get_lvalue (q, NULL);

    /* Push a second frame.  */
    region_id rid_2nd_frame
      = model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);

    /* Have a pointer in the older frame point to a local in the
       more recent frame.  */
    svalue_id sid_ptr = model0.get_rvalue (addr_of_a, NULL);
    model0.set_value (q_in_first_frame, sid_ptr, NULL);

    /* Verify that it's pointing at the newer frame.  */
    region_id rid_pointee
      = model0.get_svalue (sid_ptr)->dyn_cast_region_svalue ()->get_pointee ();
    ASSERT_EQ (model0.get_region (rid_pointee)->get_parent (), rid_2nd_frame);

    model0.canonicalize (NULL);

    region_model model1 (model0);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same
       (after canonicalization, at least).  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    merged.canonicalize (NULL);
    ASSERT_EQ (model0, merged);
  }

  /* Verify that we can merge a model in which a local points to a global.  */
  {
    region_model model0;
    model0.push_frame (DECL_STRUCT_FUNCTION (test_fndecl), NULL, NULL);
    model0.set_value (model0.get_lvalue (q, NULL),
		      model0.get_rvalue (addr_of_y, NULL), NULL);

    model0.canonicalize (NULL);

    region_model model1 (model0);
    ASSERT_EQ (model0, model1);

    /* They should be mergeable, and the result should be the same
       (after canonicalization, at least).  */
    region_model merged;
    ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));
    merged.canonicalize (NULL);
    ASSERT_EQ (model0, merged);
  }
}

/* Verify that constraints are correctly merged when merging region_model
   instances.  */

static void
test_constraint_merging ()
{
  tree int_0 = build_int_cst (integer_type_node, 0);
  tree int_5 = build_int_cst (integer_type_node, 5);
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);
  tree n = build_global_decl ("n", integer_type_node);

  test_region_model_context ctxt;

  /* model0: 0 <= (x == y) < n.  */
  region_model model0;
  model0.set_to_new_unknown_value (model0.get_lvalue (x, &ctxt),
				   integer_type_node, &ctxt);
  model0.add_constraint (x, EQ_EXPR, y, &ctxt);
  model0.add_constraint (x, GE_EXPR, int_0, NULL);
  model0.add_constraint (x, LT_EXPR, n, NULL);

  /* model1: z != 5 && (0 <= x < n).  */
  region_model model1;
  model1.set_to_new_unknown_value (model1.get_lvalue (x, &ctxt),
				   integer_type_node, &ctxt);
  model1.add_constraint (z, NE_EXPR, int_5, NULL);
  model1.add_constraint (x, GE_EXPR, int_0, NULL);
  model1.add_constraint (x, LT_EXPR, n, NULL);

  /* They should be mergeable; the merged constraints should
     be: (0 <= x < n).  */
  region_model merged;
  ASSERT_TRUE (model0.can_merge_with_p (model1, &merged));

  ASSERT_EQ (merged.eval_condition (x, GE_EXPR, int_0, &ctxt),
	     tristate (tristate::TS_TRUE));
  ASSERT_EQ (merged.eval_condition (x, LT_EXPR, n, &ctxt),
	     tristate (tristate::TS_TRUE));

  ASSERT_EQ (merged.eval_condition (z, NE_EXPR, int_5, &ctxt),
	     tristate (tristate::TS_UNKNOWN));
  ASSERT_EQ (merged.eval_condition (x, LT_EXPR, y, &ctxt),
	     tristate (tristate::TS_UNKNOWN));
}

/* Verify that if we mark a pointer to a malloc-ed region as non-NULL,
   all cast pointers to that region are also known to be non-NULL.  */

static void
test_malloc_constraints ()
{
  region_model model;
  tree p = build_global_decl ("p", ptr_type_node);
  tree char_star = build_pointer_type (char_type_node);
  tree q = build_global_decl ("q", char_star);
  tree null_ptr = build_int_cst (ptr_type_node, 0);

  region_id rid = model.add_new_malloc_region ();
  svalue_id sid = model.get_or_create_ptr_svalue (ptr_type_node, rid);
  model.set_value (model.get_lvalue (p, NULL), sid, NULL);
  model.set_value (q, p, NULL);

  /* We should have a symbolic_region with m_possibly_null: true.  */
  region *pointee = model.get_region (rid);
  symbolic_region *sym_reg = pointee->dyn_cast_symbolic_region ();
  ASSERT_NE (sym_reg, NULL);
  ASSERT_TRUE (sym_reg->m_possibly_null);

  ASSERT_CONDITION_UNKNOWN (model, p, NE_EXPR, null_ptr);
  ASSERT_CONDITION_UNKNOWN (model, p, EQ_EXPR, null_ptr);
  ASSERT_CONDITION_UNKNOWN (model, q, NE_EXPR, null_ptr);
  ASSERT_CONDITION_UNKNOWN (model, q, EQ_EXPR, null_ptr);

  model.add_constraint (p, NE_EXPR, null_ptr, NULL);

  /* Adding the constraint should have cleared m_possibly_null.  */
  ASSERT_FALSE (sym_reg->m_possibly_null);

  ASSERT_CONDITION_TRUE (model, p, NE_EXPR, null_ptr);
  ASSERT_CONDITION_FALSE (model, p, EQ_EXPR, null_ptr);
  ASSERT_CONDITION_TRUE (model, q, NE_EXPR, null_ptr);
  ASSERT_CONDITION_FALSE (model, q, EQ_EXPR, null_ptr);
}

/* Run all of the selftests within this file.  */

void
analyzer_region_model_cc_tests ()
{
  test_tree_cmp_on_constants ();
  test_dump ();
  test_dump_2 ();
  test_dump_3 ();
  test_get_representative_tree ();
  test_unique_constants ();
  test_svalue_equality ();
  test_region_equality ();
  test_purging_by_criteria ();
  test_purge_unused_svalues ();
  test_assignment ();
  test_compound_assignment ();
  test_stack_frames ();
  test_get_representative_path_var ();
  test_canonicalization_1 ();
  test_canonicalization_2 ();
  test_canonicalization_3 ();
  test_canonicalization_4 ();
  test_state_merging ();
  test_constraint_merging ();
  test_malloc_constraints ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
