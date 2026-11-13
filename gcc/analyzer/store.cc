/* Classes for modeling the state of memory.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "ordered-hash-map.h"
#include "cfg.h"
#include "sbitmap.h"
#include "stor-layout.h"

#include "text-art/tree-widget.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/supergraph.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/call-summary.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

/* class uncertainty_t.  */

void
uncertainty_t::on_maybe_bound_value (const symbolic::value_node &val)
{
  m_maybe_bound_vals.add (&val);
}

void
uncertainty_t::
on_mutable_value_at_unknown_call (const symbolic::value_node &val)
{
  m_mutable_at_unknown_call_vals.add (&val);
}

bool
uncertainty_t::unknown_sm_state_p (const svalue *sval)
{
  if (m_maybe_bound_vals.contains (sval))
    return true;
  if (m_mutable_at_unknown_call_vals.contains (sval))
    return true;

  if (auto *from_bv = sval->dyn_cast_svalue_from_bit_vector ())
    {
      auto *bv = &from_bv->get_bit_vector ();
      if (m_maybe_bound_vals.contains (bv)
	  || m_mutable_at_unknown_call_vals.contains (bv))
	return true;
    }

  return false;
}

/* Mark all in BC as being maybe bound.  */

void
uncertainty_t::add_bit_contents (const symbolic::bit_contents &bc)
{
  bc.for_each_value ([] (const symbolic::value_node &val,
			 void *user_data) -> iter_result {
    uncertainty_t *self = static_cast<uncertainty_t *> (user_data);
    self->on_maybe_bound_value (val);
    return val.for_each_svalue ([] (const svalue &sval,
				    void *user_data) -> iter_result
    {
      uncertainty_t *uncertainty = static_cast<uncertainty_t *> (user_data);
      uncertainty->on_maybe_bound_value (sval);
      return iter_result::keep_going;
    }, user_data);
    return iter_result::keep_going;
  }, this);
}

/* Dump a tree-like representation of this object to stderr.  */

DEBUG_FUNCTION void
uncertainty_t::dump () const
{
  symbolic::dump_node_tree_to_stderr<uncertainty_t> (*this);
}

void
uncertainty_t::dump_to_logger (logger &log) const
{
  symbolic::dump_node_tree_to_logger<uncertainty_t> (*this, log);
}

std::unique_ptr<text_art::tree_widget>
uncertainty_t::make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> w
    (tree_widget::from_fmt (nds, nullptr, "uncertainty_t"));

  {
    std::unique_ptr<tree_widget> child
      (tree_widget::from_fmt (nds, nullptr, "m_maybe_bound_vals"));
    for (auto iter : m_maybe_bound_vals)
      child->add_child (iter->make_dump_widget (nds));
    w->add_child (std::move (child));
  }
  {
    std::unique_ptr<tree_widget> child
      (tree_widget::from_fmt (nds, nullptr, "m_mutable_at_unknown_call_vals"));
    for (auto iter : m_mutable_at_unknown_call_vals)
      child->add_child (iter->make_dump_widget (nds));
    w->add_child (std::move (child));
  }

  return w;
}

/* struct bit_range.  */

void
bit_range::dump_to_pp (pretty_printer *pp) const
{
  byte_range bytes (0, 0);
  if (as_byte_range (&bytes))
    bytes.dump_to_pp (pp);
  else
    {
      pp_string (pp, "start: ");
      pp_wide_int (pp, m_start_bit_offset, SIGNED);
      pp_string (pp, ", size: ");
      pp_wide_int (pp, m_size_in_bits, SIGNED);
      pp_string (pp, ", next: ");
      pp_wide_int (pp, get_next_bit_offset (), SIGNED);
    }
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
bit_range::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp);
  pp_newline (&pp);
}

/* Generate a JSON value for this bit_range.
   This is intended for debugging the analyzer rather
   than serialization.  */

std::unique_ptr<json::object>
bit_range::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("start_bit_offset",
	    bit_offset_to_json (m_start_bit_offset));
  obj->set ("size_in_bits",
	    bit_offset_to_json (m_size_in_bits));
  return obj;
}

/* If OTHER is a subset of this, return true and, if OUT is
   non-null, write to *OUT the relative range of OTHER within this.
   Otherwise return false.  */

bool
bit_range::contains_p (const bit_range &other, bit_range *out) const
{
  if (contains_p (other.get_start_bit_offset ())
      && contains_p (other.get_last_bit_offset ()))
    {
      if (out)
	{
	  out->m_start_bit_offset = other.m_start_bit_offset - m_start_bit_offset;
	  out->m_size_in_bits = other.m_size_in_bits;
	}
      return true;
    }
  else
    return false;
}

/* If OTHER intersects this, return true and write
   the relative range of OTHER within THIS to *OUT_THIS,
   and the relative range of THIS within OTHER to *OUT_OTHER.
   Otherwise return false.  */

bool
bit_range::intersects_p (const bit_range &other,
			 bit_range *out_this,
			 bit_range *out_other) const
{
  if (get_start_bit_offset () < other.get_next_bit_offset ()
      && other.get_start_bit_offset () < get_next_bit_offset ())
    {
      bit_offset_t overlap_start
	= MAX (get_start_bit_offset (),
	       other.get_start_bit_offset ());
      bit_offset_t overlap_next
	= MIN (get_next_bit_offset (),
	       other.get_next_bit_offset ());
      if (overlap_next <= overlap_start)
	/* If this has happened, some kind of overflow has happened in
	   our arithmetic.  For now, reject such cases.  */
	return false;
      bit_range abs_overlap_bits (overlap_start, overlap_next - overlap_start);
      *out_this = abs_overlap_bits - get_start_bit_offset ();
      *out_other = abs_overlap_bits - other.get_start_bit_offset ();
      return true;
    }
  else
    return false;
}

/* Return true if THIS and OTHER intersect and write the number
   of bits both buffers overlap to *OUT_NUM_OVERLAP_BITS.

   Otherwise return false.  */

bool
bit_range::intersects_p (const bit_range &other,
			 bit_size_t *out_num_overlap_bits) const
{
  if (get_start_bit_offset () < other.get_next_bit_offset ()
      && other.get_start_bit_offset () < get_next_bit_offset ())
    {
      bit_offset_t overlap_start = MAX (get_start_bit_offset (),
					 other.get_start_bit_offset ());
      bit_offset_t overlap_next = MIN (get_next_bit_offset (),
					other.get_next_bit_offset ());
      if (overlap_next <= overlap_start)
	/* If this has happened, some kind of overflow has happened in
	   our arithmetic.  For now, reject such cases.  */
	return false;
      *out_num_overlap_bits = overlap_next - overlap_start;
      return true;
    }
  else
    return false;
}

/* Return true if THIS exceeds OTHER and write the overhanging
   bit range to OUT_OVERHANGING_BIT_RANGE.  */

bool
bit_range::exceeds_p (const bit_range &other,
		      bit_range *out_overhanging_bit_range) const
{
  gcc_assert (!empty_p ());

  if (other.get_next_bit_offset () < get_next_bit_offset ())
    {
      /* THIS definitely exceeds OTHER.  */
      bit_offset_t start = MAX (get_start_bit_offset (),
				 other.get_next_bit_offset ());
      bit_offset_t size = get_next_bit_offset () - start;
      if (size <= 0)
	/* If this has happened, some kind of overflow has happened in
	   our arithmetic.  For now, reject such cases.  */
	return false;
      out_overhanging_bit_range->m_start_bit_offset = start;
      out_overhanging_bit_range->m_size_in_bits = size;
      return true;
    }
  else
    return false;
}

/* Return true if THIS falls short of OFFSET and write the
   bit range fallen short to OUT_FALL_SHORT_BITS.  */

bool
bit_range::falls_short_of_p (bit_offset_t offset,
			     bit_range *out_fall_short_bits) const
{
  gcc_assert (!empty_p ());

  if (get_start_bit_offset () < offset)
    {
      /* THIS falls short of OFFSET.  */
      bit_offset_t start = get_start_bit_offset ();
      bit_offset_t size = MIN (offset, get_next_bit_offset ()) - start;
      if (size <= 0)
	/* If this has happened, some kind of overflow has happened in
	   our arithmetic.  For now, reject such cases.  */
	return false;
      out_fall_short_bits->m_start_bit_offset = start;
      out_fall_short_bits->m_size_in_bits = size;
      return true;
    }
  else
    return false;
}

int
bit_range::cmp (const bit_range &br1, const bit_range &br2)
{
  if (int start_cmp = wi::cmps (br1.m_start_bit_offset,
				br2.m_start_bit_offset))
    return start_cmp;

  return wi::cmpu (br1.m_size_in_bits, br2.m_size_in_bits);
}

/* Offset this range by OFFSET.  */

bit_range
bit_range::operator- (bit_offset_t offset) const
{
  return bit_range (m_start_bit_offset - offset, m_size_in_bits);
}

/* Attempt to convert this bit_range to a byte_range.
   Return true if it is possible, writing the result to *OUT.
   Otherwise return false.  */

bool
bit_range::as_byte_range (byte_range *out) const
{
  if (m_start_bit_offset % BITS_PER_UNIT == 0
      && m_size_in_bits % BITS_PER_UNIT == 0)
    {
      out->m_start_byte_offset = m_start_bit_offset / BITS_PER_UNIT;
      out->m_size_in_bytes = m_size_in_bits / BITS_PER_UNIT;
      return true;
    }
  return false;
}

/* Dump this object to PP.  */

void
byte_range::dump_to_pp (pretty_printer *pp) const
{
  if (m_size_in_bytes == 0)
    {
      pp_string (pp, "empty");
    }
  else if (m_size_in_bytes == 1)
    {
      pp_string (pp, "byte ");
      pp_wide_int (pp, m_start_byte_offset, SIGNED);
    }
  else
    {
      pp_string (pp, "bytes ");
      pp_wide_int (pp, m_start_byte_offset, SIGNED);
      pp_string (pp, "-");
      pp_wide_int (pp, get_last_byte_offset (), SIGNED);
    }
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
byte_range::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp);
  pp_newline (&pp);
}

/* Generate a JSON value for this byte_range.
   This is intended for debugging the analyzer rather
   than serialization.  */

std::unique_ptr<json::object>
byte_range::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("start_byte_offset",
	    byte_offset_to_json (m_start_byte_offset));
  obj->set ("size_in_bytes",
	    byte_offset_to_json (m_size_in_bytes));
  return obj;
}

/* If OTHER is a subset of this, return true and write
   to *OUT the relative range of OTHER within this.
   Otherwise return false.  */

bool
byte_range::contains_p (const byte_range &other, byte_range *out) const
{
  if (contains_p (other.get_start_byte_offset ())
      && contains_p (other.get_last_byte_offset ()))
    {
      out->m_start_byte_offset = other.m_start_byte_offset - m_start_byte_offset;
      out->m_size_in_bytes = other.m_size_in_bytes;
      return true;
    }
  else
    return false;
}

/* qsort comparator for byte ranges.  */

int
byte_range::cmp (const byte_range &br1, const byte_range &br2)
{
  /* Order first by offset.  */
  if (int start_cmp = wi::cmps (br1.m_start_byte_offset,
				br2.m_start_byte_offset))
    return start_cmp;

  /* ...then by size.  */
  return wi::cmpu (br1.m_size_in_bytes, br2.m_size_in_bytes);
}

static const svalue *
simplify_for_binding (const svalue *sval)
{
  if (const svalue *cast_sval = sval->maybe_undo_cast ())
    sval = cast_sval;
  return sval;
}

/* class binding_cluster.  */

binding_cluster::binding_cluster (symbolic::node_manager &node_mgr,
				  const region *base_region,
				  const symbolic::bit_contents &contents)
: m_node_mgr (node_mgr),
  m_base_region (base_region),
  m_contents (&contents),
  m_escaped (false), m_touched (false)
{
  if (CHECKING_P)
    {
      gcc_assert (base_region);
      gcc_assert (base_region->base_region_p ());

      /* We shouldn't create clusters for dereferencing an UNKNOWN ptr.  */
      gcc_assert (!base_region->symbolic_for_unknown_ptr_p ());

      /* We shouldn't create clusters for base regions that aren't trackable.  */
      gcc_assert (base_region->tracked_p ());
    }
}

/* binding_cluster's copy ctor.  */

binding_cluster::binding_cluster (const binding_cluster &other)
: m_node_mgr (other.m_node_mgr),
  m_base_region (other.m_base_region),
  m_contents (other.m_contents),
  m_escaped (other.m_escaped), m_touched (other.m_touched)
{
}

/* binding_cluster's assignment operator.  */

binding_cluster&
binding_cluster::operator= (const binding_cluster &other)
{
  gcc_assert (m_base_region == other.m_base_region);
  m_contents = other.m_contents;
  m_escaped = other.m_escaped;
  m_touched = other.m_touched;
  return *this;
}

/* binding_cluster's equality operator.  */

bool
binding_cluster::operator== (const binding_cluster &other) const
{
  if (m_contents != other.m_contents)
    return false;

  if (m_base_region != other.m_base_region)
    return false;

  if (m_escaped != other.m_escaped)
    return false;

  if (m_touched != other.m_touched)
    return false;

  return true;
}

/* Generate a hash value for this binding_cluster.  */

hashval_t
binding_cluster::hash () const
{
  inchash::hash h;
  h.add_int (m_contents->get_id ());

  h.add_flag (m_escaped);
  h.add_flag (m_touched);
  h.commit_flag ();

  return h.end ();
}

/* Return true if this binding_cluster is symbolic
   i.e. its base region is symbolic.  */

bool
binding_cluster::symbolic_p () const
{
  return m_base_region->get_kind () == RK_SYMBOLIC;
}

DEBUG_FUNCTION void
binding_cluster::dump () const
{
  symbolic::dump_node_tree_to_stderr<binding_cluster> (*this);
}

/* Assert that this object is valid.  */

void
binding_cluster::validate () const
{
  // no-op
}

/* Return a new json::object of the form
   {"escaped": true/false,
    "touched": true/false,
    "map" : object for the binding_map.  */

std::unique_ptr<json::object>
binding_cluster::to_json () const
{
  auto cluster_obj = std::make_unique<json::object> ();

  cluster_obj->set_bool ("escaped", m_escaped);
  cluster_obj->set_bool ("touched", m_touched);
  cluster_obj->set ("contents", m_contents->to_json ());

  return cluster_obj;
}

std::unique_ptr<text_art::tree_widget>
binding_cluster::make_dump_widget (symbolic::node_dump_state &nds) const
{
  pretty_printer the_pp;
  pretty_printer * const pp = &the_pp;
  pp_format_decoder (pp) = default_tree_printer;
  pp_show_color (pp) = true;
  const bool simple = true;

  m_base_region->dump_to_pp (pp, simple);
  pp_string (pp, ": ");

  if (escaped_p ())
    pp_string (pp, " (ESCAPED)");
  if (touched_p ())
    pp_string (pp, " (TOUCHED)");

  std::unique_ptr<text_art::tree_widget> cluster_widget
    (text_art::tree_widget::make (nds, pp));

  gcc_assert (m_contents);
  cluster_widget->add_child (m_contents->make_dump_widget (nds));

  return cluster_widget;
}

const symbolic::bit_vector &
binding_cluster::get_bits (symbolic_bit_range bit_range) const
{
  return m_contents->get_bits (bit_range);
}

void
binding_cluster::set_bits (symbolic_bit_range bit_range,
			   const symbolic::bit_vector &bv,
			   uncertainty_t *uncertainty)
{
  m_contents = &m_contents->set_bits (bit_range, bv, uncertainty);
}

/* Mark this cluster as being unknown.

   If UNCERTAINTY is non-NULL, use it to record any svalues that
   had bindings to them removed, as being maybe-bound.
   If MAYBE_LIVE_VALUES is non-NULL, use it to record any svalues that
   had bindings to them removed, as being maybe-live.  */

void
binding_cluster::mark_as_untracked (symbolic::node_manager &node_mgr,
				    uncertainty_t *uncertainty,
				    value_set *maybe_live_values)
{
  if (uncertainty)
    {
      m_contents->for_each_value ([] (const symbolic::value_node &val,
				      void *user_data) -> iter_result
      {
	uncertainty_t *uncertainty = static_cast<uncertainty_t *> (user_data);
	uncertainty->on_maybe_bound_value (val);
	return val.for_each_svalue ([] (const svalue &sval,
					void *user_data) -> iter_result
	{
	  uncertainty_t *uncertainty = static_cast<uncertainty_t *> (user_data);
	  uncertainty->on_maybe_bound_value (sval);
	  return iter_result::keep_going;
	}, user_data);
      }, uncertainty);
    }
  if (maybe_live_values)
    {
      m_contents->for_each_value ([] (const symbolic::value_node &val,
				      void *user_data) -> iter_result
      {
	value_set *maybe_live_values = static_cast<value_set *> (user_data);
	maybe_live_values->add (&val);
	return val.for_each_svalue ([] (const svalue &sval,
					void *user_data) -> iter_result
	{
	  value_set *maybe_live_values = static_cast<value_set *> (user_data);
	  maybe_live_values->add (&sval);
	  return iter_result::keep_going;
	}, user_data);
      }, maybe_live_values);
    }

  value_set maybe_live;
  m_contents->for_each_value ([] (const symbolic::value_node &val,
				  void *user_data) -> iter_result
  {
    value_set *maybe_live = static_cast<value_set *> (user_data);
    maybe_live->add (&val);
    return iter_result::keep_going;
  }, &maybe_live);

  auto &bc_mgr = node_mgr.get_bit_contents_manager ();
  m_contents = &bc_mgr.get_untracked (std::move (maybe_live));
}

/* Purge state involving NODE.  */

void
binding_cluster::purge_state_involving (const symbolic::node &node)
{
  m_contents = &m_contents->purge_state_involving (node);
}

/* Attempt to merge CLUSTER_A and CLUSTER_B into OUT_CLUSTER, using
   MGR and MERGER.
   Return true if they can be merged, false otherwise.  */

bool
binding_cluster::can_merge_p (const binding_cluster *cluster_a,
			      const binding_cluster *cluster_b,
			      binding_cluster *out_cluster,
			      store *out_store,
			      model_merger *merger)
{
  gcc_assert (out_cluster);

  /* Merge flags ("ESCAPED" and "TOUCHED") by setting the merged flag to
     true if either of the inputs is true.  */
  if ((cluster_a && cluster_a->m_escaped)
      || (cluster_b && cluster_b->m_escaped))
    out_cluster->m_escaped = true;
  if ((cluster_a && cluster_a->m_touched)
      || (cluster_b && cluster_b->m_touched))
    out_cluster->m_touched = true;

  auto get_baseline_state = [merger] () -> baseline_state
  {
    return merger->m_model_a->get_baseline_state ();
  };

  /* At least one of CLUSTER_A and CLUSTER_B are non-NULL, but either
     could be NULL.  Handle these cases.  */
  if (cluster_a == nullptr)
    {
      gcc_assert (cluster_b != nullptr);
      gcc_assert (cluster_b->m_base_region == out_cluster->m_base_region);
      out_cluster->make_unknown_relative_to (cluster_b, out_store,
					     get_baseline_state ());
      return true;
    }
  if (cluster_b == nullptr)
    {
      gcc_assert (cluster_a != nullptr);
      gcc_assert (cluster_a->m_base_region == out_cluster->m_base_region);
      out_cluster->make_unknown_relative_to (cluster_a, out_store,
					     get_baseline_state ());
      return true;
    }

  /* The "both inputs are non-NULL" case.  */
  gcc_assert (cluster_a != nullptr && cluster_b != nullptr);
  gcc_assert (cluster_a->m_base_region == out_cluster->m_base_region);
  gcc_assert (cluster_b->m_base_region == out_cluster->m_base_region);

  tree type = nullptr;
  if (cluster_a->get_base_region ()->get_type ()
      && cluster_b->get_base_region ()->get_type ()
      && (cluster_a->get_base_region ()->get_type ()
	  == cluster_b->get_base_region ()->get_type ()))
    type = cluster_a->get_base_region ()->get_type ();

  if (auto *merged_contents
	= symbolic::bit_contents::can_merge_p (*cluster_a->m_contents,
					       *cluster_b->m_contents,
					       type,
					       merger))
    {
      out_cluster->m_contents = merged_contents;
      return true;
    }

  return false;
}

/* Concrete visitor subclass, where any svalues that are visited are
   to be treated as possibly alive: regions pointed to are marked
   as escaping, and the svalues are added to any uncertainty_y.  */

class possibly_overwritten_values_visitor : public symbolic::node_visitor
{
public:
  possibly_overwritten_values_visitor (store &store_,
				       uncertainty_t *uncertainty,
				       baseline_state bs)
  : m_store (store_),
    m_uncertainty (uncertainty),
    m_baseline_state (bs)
  {
  }

  bool
  visit_svalue (const svalue &n) final override
  {
    if (!visit_node (n))
      return false;
    m_maybe_live.add (&n);
    if (auto *sym_ptr = n.maybe_get_symbolic_pointer ())
      m_store.mark_as_escaped (&sym_ptr->get_base_region (),
			       m_baseline_state);
    if (m_uncertainty)
      m_uncertainty->on_maybe_bound_value (n);
    return true;
  }

  bool
  visit_bit_vector (const symbolic::bit_vector &bv) final override
  {
    if (!visit_node (bv))
      return false;
    m_maybe_live.add (&bv);
    if (m_uncertainty)
      m_uncertainty->on_maybe_bound_value (bv);
    return true;
  }

  const value_set &
  get_maybe_live () const { return m_maybe_live; }

private:
  store &m_store;
  uncertainty_t *m_uncertainty;
  baseline_state m_baseline_state;
  value_set m_maybe_live;
};

/* Update this cluster to reflect an attempt to merge OTHER where there
   is no other cluster to merge with, and so we're notionally merging the
   bound values in OTHER with the initial value of the relevant regions.

   Any bound keys in OTHER should be bound to unknown in this.  */

void
binding_cluster::make_unknown_relative_to (const binding_cluster *other,
					   store *out_store,
					   baseline_state bs)
{
  gcc_assert (other);
  gcc_assert (m_base_region == other->m_base_region);

  auto &bcm = m_node_mgr.get_bit_contents_manager ();

  possibly_overwritten_values_visitor v (*out_store,
					 nullptr,
					 bs);
  other->m_contents->accept_node_visitor (v);

  m_contents = &bcm.get_untracked (v.get_maybe_live ());
}

iter_result
binding_cluster::
for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				   void *user_data),
		void *user_data) const
{
  return m_contents->for_each_value (cb, user_data);
}

/* Mark this cluster as having escaped.  */

void
binding_cluster::mark_as_escaped ()
{
  m_escaped = true;
}

/* If this cluster has escaped (by this call, or by an earlier one, or
   by being an external param), then unbind all values and mark it
   as "touched", so that it has a conjured value, rather than an
   initial_svalue.
   Use P to purge state involving conjured_svalues.  */

void
binding_cluster::on_unknown_fncall (const gcall &call,
				    const conjured_purge &p)
{
  if (m_escaped)
    mark_region_as_conjured (call, p);
}

void
binding_cluster::mark_region_as_conjured (const gimple &stmt,
					  const conjured_purge &p)
{
  auto &bc_mgr = m_node_mgr.get_bit_contents_manager ();
  m_contents = &bc_mgr.get_conjured_bit_contents (*m_base_region,
						  &stmt,
						  p);
  m_touched = true;
}

/* Mark this cluster as having been clobbered by STMT.
   Use P to purge state involving conjured_svalues.  */

void
binding_cluster::on_asm (const gasm *stmt,
			 const conjured_purge &p)
{
  mark_region_as_conjured (*stmt, p);
}

/* Return true if this cluster has escaped.  */

bool
binding_cluster::escaped_p () const
{
  /* Consider the "errno" region to always have escaped.  */
  if (m_base_region->get_kind () == RK_ERRNO)
    return true;

  /* Symbolic regions arising from dereferencing a conjured pointer
     are escaped, as the conjured pointer originates from "outside".  */
  if (auto *sym_reg = m_base_region->dyn_cast_symbolic_region ())
    {
      const svalue *ptr = sym_reg->get_pointer ();
      if (ptr->conjured_p ())
	return true;
    }

  return m_escaped;
}

/* Return true if this binding_cluster has no information
   i.e. if it has its implicit contents, and it hasn't been marked as having
   escaped, or touched symbolically.  */

bool
binding_cluster::redundant_p (const store &store_,
			      baseline_state bs) const
{
  if (m_escaped || m_touched)
    return false;

  auto &implicit_bc = store_.get_implicit_contents (*m_base_region, bs);

  return m_contents == &implicit_bc;
}

/* Add PV to OUT_PVS, casting it to TYPE if it is not already of that type.  */

void
append_pathvar_with_type (path_var pv,
			  tree type,
			  auto_vec<path_var> *out_pvs)
{
  gcc_assert (pv.m_tree);

  if (type && TREE_TYPE (pv.m_tree) != type)
    pv.m_tree = build1 (NOP_EXPR, type, pv.m_tree);

  out_pvs->safe_push (pv);
}

/* Find representative path_vars for SVAL within this binding of BASE_REG,
   appending the results to OUT_PVS.  */

void
binding_cluster::get_representative_path_vars (const region_model *model,
					       value_set *visited,
					       const region *base_reg,
					       const svalue *sval,
					       logger *logger,
					       auto_vec<path_var> *out_pvs)
  const
{
  tree desired_type = sval->get_type ();
  sval = simplify_for_binding (sval);

  m_contents->get_representative_path_vars (model,
					    visited,
					    base_reg,
					    sval,
					    desired_type,
					    logger,
					    out_pvs);
}

void
binding_cluster::poison_any_pointers_to_descendents (const region *reg,
						     enum poison_kind pkind)
{
  m_contents = &m_contents->poison_any_pointers_to_descendents (reg, pkind);
}

bool
binding_cluster::
bits_live_p (const symbolic::bit_vector_from_ubc &bv_from_ubc) const
{
  return m_contents->bits_live_p (bv_from_ubc);
}

/* class store.  */

/* store's default ctor.  */

store::store (symbolic::node_manager &node_mgr)
: m_node_mgr (node_mgr),
  m_called_unknown_fn (false)
{
}

/* store's copy ctor.  */

store::store (const store &other)
: m_node_mgr (other.m_node_mgr),
  m_cluster_map (other.m_cluster_map.elements ()),
  m_called_unknown_fn (other.m_called_unknown_fn)
{
  for (cluster_map_t::iterator iter = other.m_cluster_map.begin ();
       iter != other.m_cluster_map.end ();
       ++iter)
    {
      const region *reg = (*iter).first;
      gcc_assert (reg);
      binding_cluster *c = (*iter).second;
      gcc_assert (c);
      m_cluster_map.put (reg, new binding_cluster (*c));
    }
}

/* store's dtor.  */

store::~store ()
{
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end ();
       ++iter)
    delete (*iter).second;
}

/* store's assignment operator.  */

store &
store::operator= (const store &other)
{
  /* Delete existing cluster map.  */
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end ();
       ++iter)
    delete (*iter).second;
  m_cluster_map.empty ();

  m_called_unknown_fn = other.m_called_unknown_fn;

  for (cluster_map_t::iterator iter = other.m_cluster_map.begin ();
       iter != other.m_cluster_map.end ();
       ++iter)
    {
      const region *reg = (*iter).first;
      gcc_assert (reg);
      binding_cluster *c = (*iter).second;
      gcc_assert (c);
      m_cluster_map.put (reg, new binding_cluster (*c));
    }
  return *this;
}

/* store's equality operator.  */

bool
store::operator== (const store &other) const
{
  if (m_called_unknown_fn != other.m_called_unknown_fn)
    return false;

  if (m_cluster_map.elements () != other.m_cluster_map.elements ())
    return false;

  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end ();
       ++iter)
    {
      const region *reg = (*iter).first;
      binding_cluster *c = (*iter).second;
      binding_cluster **other_slot
	= const_cast <cluster_map_t &> (other.m_cluster_map).get (reg);
      if (other_slot == nullptr)
	return false;
      if (*c != **other_slot)
	return false;
    }

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Get a hash value for this store.  */

hashval_t
store::hash () const
{
  hashval_t result = 0;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end ();
       ++iter)
    result ^= (*iter).second->hash ();
  return result;
}

/* Populate OUT with a sorted list of parent regions for the regions in IN,
   removing duplicate parents.  */

static void
get_sorted_parent_regions (auto_vec<const region *> *out,
			   auto_vec<const region *> &in)
{
  /* Get the set of parent regions.  */
  hash_set<const region *> parent_regions;
  const region *iter_reg;
  unsigned i;
  FOR_EACH_VEC_ELT (in, i, iter_reg)
    {
      const region *parent_reg = iter_reg->get_parent_region ();
      gcc_assert (parent_reg);
      parent_regions.add (parent_reg);
    }

  /* Write to OUT.  */
  for (hash_set<const region *>::iterator iter = parent_regions.begin();
       iter != parent_regions.end(); ++iter)
    out->safe_push (*iter);

  /* Sort OUT.  */
  out->qsort (region::cmp_ptr_ptr);
}

/* Dump a tree-like representation of this store to stderr.  */

DEBUG_FUNCTION void
store::dump () const
{
  symbolic::dump_node_tree_to_stderr<store> (*this);
}

/* Assert that this object is valid.  */

void
store::validate () const
{
  for (auto iter : m_cluster_map)
    iter.second->validate ();
}

/* Return a new json::object of the form
   {PARENT_REGION_DESC: {BASE_REGION_DESC: object for binding_map,
			 ... for each cluster within parent region},
    ...for each parent region,
    "called_unknown_fn": true/false}.  */

std::unique_ptr<json::object>
store::to_json () const
{
  auto store_obj = std::make_unique<json::object> ();

  /* Sort into some deterministic order.  */
  auto_vec<const region *> base_regions;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      base_regions.safe_push (base_reg);
    }
  base_regions.qsort (region::cmp_ptr_ptr);

  /* Gather clusters, organize by parent region, so that we can group
     together locals, globals, etc.  */
  auto_vec<const region *> parent_regions;
  get_sorted_parent_regions (&parent_regions, base_regions);

  const region *parent_reg;
  unsigned i;
  FOR_EACH_VEC_ELT (parent_regions, i, parent_reg)
    {
      gcc_assert (parent_reg);

      auto clusters_in_parent_reg_obj = std::make_unique<json::object> ();

      const region *base_reg;
      unsigned j;
      FOR_EACH_VEC_ELT (base_regions, j, base_reg)
	{
	  /* This is O(N * M), but N ought to be small.  */
	  if (base_reg->get_parent_region () != parent_reg)
	    continue;
	  binding_cluster *cluster
	    = *const_cast<cluster_map_t &> (m_cluster_map).get (base_reg);
	  label_text base_reg_desc = base_reg->get_desc ();
	  clusters_in_parent_reg_obj->set (base_reg_desc.get (),
					   cluster->to_json ());
	}
      label_text parent_reg_desc = parent_reg->get_desc ();
      store_obj->set (parent_reg_desc.get (),
		      std::move (clusters_in_parent_reg_obj));
    }

  store_obj->set_bool ("called_unknown_fn", m_called_unknown_fn);

  return store_obj;
}

static const char *
get_string_for_baseline_state (baseline_state bs)
{
  switch (bs)
    {
    default:
      gcc_unreachable ();

    case baseline_state::main:
      return "main";

    case baseline_state::after_main:
      return "after_main";
    }
}

std::unique_ptr<text_art::tree_widget>
store::make_dump_widget (symbolic::node_dump_state &nds) const
{
  std::unique_ptr<text_art::tree_widget> store_widget
    (text_art::tree_widget::make (nds, "Store"));

  store_widget->add_child
    (text_art::tree_widget::from_fmt (nds, nullptr,
				      "m_called_unknown_fn: %s",
				      m_called_unknown_fn ? "true" : "false"));

  /* Sort into some deterministic order.  */
  auto_vec<const region *> base_regions;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      base_regions.safe_push (base_reg);
    }
  base_regions.qsort (region::cmp_ptr_ptr);

  /* Gather clusters, organize by parent region, so that we can group
     together locals, globals, etc.  */
  auto_vec<const region *> parent_regions;
  get_sorted_parent_regions (&parent_regions, base_regions);

  const region *parent_reg;
  unsigned i;
  FOR_EACH_VEC_ELT (parent_regions, i, parent_reg)
    {
      gcc_assert (parent_reg);

      pretty_printer the_pp;
      pretty_printer * const pp = &the_pp;
      pp_format_decoder (pp) = default_tree_printer;
      pp_show_color (pp) = true;
      const bool simple = true;

      parent_reg->dump_to_pp (pp, simple);

      std::unique_ptr<text_art::tree_widget> parent_reg_widget
	(text_art::tree_widget::make (nds, pp));

      const region *base_reg;
      unsigned j;
      FOR_EACH_VEC_ELT (base_regions, j, base_reg)
	{
	  /* This is O(N * M), but N ought to be small.  */
	  if (base_reg->get_parent_region () != parent_reg)
	    continue;
	  binding_cluster *cluster
	    = *const_cast<cluster_map_t &> (m_cluster_map).get (base_reg);
	  parent_reg_widget->add_child
	    (cluster->make_dump_widget (nds));
	}
      store_widget->add_child (std::move (parent_reg_widget));
    }

  return store_widget;
}

const symbolic::bit_vector &
store::get_bits (const symbolic::memory_range &mem_range,
		 baseline_state bs) const
{
  const region &base_reg = mem_range.get_base_region ();
  binding_cluster **cluster_slot
    = const_cast <cluster_map_t &> (m_cluster_map).get (&base_reg);
  if (!cluster_slot)
    {
      auto &baseline = get_implicit_contents (base_reg, bs);
      return baseline.get_bits (mem_range.get_bit_range ());
    }

  return (*cluster_slot)->get_bits (mem_range.get_bit_range ());
}

const symbolic::bit_contents &
store::get_bit_contents (const region &base_reg,
			 baseline_state bs) const
{
  binding_cluster **cluster_slot
    = const_cast <cluster_map_t &> (m_cluster_map).get (&base_reg);
  if (!cluster_slot)
    {
      auto &bc = get_implicit_contents (base_reg, bs);
      return bc;
    }

  return (*cluster_slot)->get_bit_contents ();
}

void
store::set_bits (const region *lhs_reg,
		 const symbolic::bit_vector &bits,
		 uncertainty_t *uncertainty,
		 const region_model &model)
{
  auto mem_range = lhs_reg->get_memory_range (*model.get_manager ());
  set_bits (mem_range, bits, uncertainty, model);
}

void
store::set_bits (const symbolic::memory_range &mem_range,
		 const symbolic::bit_vector &bv,
		 uncertainty_t *uncertainty,
		 const region_model &model)
{
  logger *logger = model.get_manager ()->get_logger ();
  LOG_SCOPE (logger);

  const region *lhs_base_reg = &mem_range.get_base_region ();
  binding_cluster *lhs_cluster;
  if (lhs_base_reg->symbolic_for_unknown_ptr_p ())
    {
      /* Reject attempting to bind values into a symbolic region
	 for an unknown ptr; merely invalidate values below.  */
      lhs_cluster = nullptr;

      /* The LHS of the write is *UNKNOWN.  If BITS contains any pointers,
	 then treat any region being pointed to as having escaped.  */
      possibly_overwritten_values_visitor v (*this, uncertainty,
					     model.get_baseline_state ());
      bv.accept_node_visitor (v);
    }
  else if (lhs_base_reg->tracked_p ())
    {
      lhs_cluster = get_or_create_cluster (lhs_base_reg,
					   model.get_baseline_state ());
      lhs_cluster->set_bits (mem_range.get_bit_range (), bv,
			     uncertainty);
    }
  else
    {
      /* Reject attempting to bind values into an untracked region;
	 merely invalidate values below.  */
      lhs_cluster = nullptr;
    }

  /* Bindings to a cluster can affect other clusters if a symbolic
     base region is involved.
     Writes to concrete clusters can't affect other concrete clusters,
     but can affect symbolic clusters.
     Writes to symbolic clusters can affect both concrete and symbolic
     clusters.
     Invalidate our knowledge of other clusters that might have been
     affected by the write.
     Gather the set of all svalues that might still be live even if
     the store doesn't refer to them.  */
  value_set maybe_live_values;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      const region *iter_base_reg = (*iter).first;
      binding_cluster *iter_cluster = (*iter).second;
      if (iter_base_reg != lhs_base_reg
	  && (lhs_cluster == nullptr
	      || lhs_cluster->symbolic_p ()
	      || iter_cluster->symbolic_p ()))
	{
	  tristate t_alias = eval_alias (lhs_base_reg, iter_base_reg,
					 model);
	  switch (t_alias.get_value ())
	    {
	    default:
	      gcc_unreachable ();

	    case tristate::TS_UNKNOWN:
	      if (logger)
		{
		  pretty_printer *pp = logger->get_printer ();
		  logger->start_log_line ();
		  logger->log_partial ("possible aliasing of ");
		  iter_base_reg->dump_to_pp (pp, true);
		  logger->log_partial (" when writing SVAL: ");
#if 0
		  rhs_sval->dump_to_pp (pp, true);
		  logger->log_partial (" to LHS_REG: ");
		  lhs_reg->dump_to_pp (pp, true);
#endif
		  logger->end_log_line ();
		}
	      /* Mark all of iter_cluster's as untracked,
		 to handle symbolic vs concrete issues.  */
	      iter_cluster->mark_as_untracked
		(model.get_manager ()->get_node_manager (),
		 uncertainty,
		 &maybe_live_values);
	      break;

	    case tristate::TS_TRUE:
	      gcc_unreachable ();
	      break;

	    case tristate::TS_FALSE:
	      /* If they can't be aliases, then don't invalidate this
		 cluster.  */
	      break;
	    }
	}
    }

  /* Given the set of svalues that might still be live, process them
     (e.g. marking regions as escaped).
     We do this after the iteration to avoid potentially changing
     m_cluster_map whilst iterating over it.  */
  on_maybe_live_values (maybe_live_values,
			model.get_baseline_state ());
}

void
store::set_bit_contents_raw (const region &base_reg,
			     const symbolic::bit_contents &bc,
			     const region_model &model)
{
  logger *logger = model.get_manager ()->get_logger ();
  LOG_SCOPE (logger);

  if (base_reg.tracked_p ())
    {
      auto *cluster = get_or_create_cluster (&base_reg,
					     model.get_baseline_state ());
      cluster->set_bit_contents (bc);
    }
}

/* Determine if BASE_REG_A could be an alias of BASE_REG_B.
   Use information from MODEL when comparing pointers. */

tristate
store::eval_alias (const region *base_reg_a,
		   const region *base_reg_b,
		   const region_model &model) const
{
  if (base_reg_a == base_reg_b)
    return tristate (true);

  /* SSA names can't alias.  */
  tree decl_a = base_reg_a->maybe_get_decl ();
  if (decl_a && TREE_CODE (decl_a) == SSA_NAME)
    return tristate::TS_FALSE;
  tree decl_b = base_reg_b->maybe_get_decl ();
  if (decl_b && TREE_CODE (decl_b) == SSA_NAME)
    return tristate::TS_FALSE;

  /* Different decls don't alias.  */
  if (decl_a && decl_b && decl_a != decl_b)
    return tristate::TS_FALSE;

  /* Try both ways, for symmetry.  */
  tristate ts_ab = eval_alias_1 (base_reg_a, base_reg_b, model);
  if (ts_ab.is_false ())
    return tristate::TS_FALSE;
  tristate ts_ba = eval_alias_1 (base_reg_b, base_reg_a, model);
  if (ts_ba.is_false ())
    return tristate::TS_FALSE;

  gcc_assert (base_reg_a != base_reg_b);
  enum region_kind kind_a = base_reg_a->get_kind ();
  enum region_kind kind_b = base_reg_b->get_kind ();
  if (kind_a == kind_b)
    {
      /* We have two different base region instances of the same kind.
	 For many kinds of region this implies they must have different
	 addresses.  */
      switch (kind_a)
	{
	/* Invalid cases.  */

	default:
	  gcc_unreachable ();

	case RK_CODE:
	case RK_ERRNO:
	case RK_GLOBALS:
	case RK_HEAP:
	case RK_ROOT:
	case RK_STACK:
	case RK_THREAD_LOCAL:
	  /* We expect these to be singletons.  */
	  gcc_unreachable ();

	case RK_BIT_RANGE:
	case RK_CAST:
	case RK_ELEMENT:
	case RK_FIELD:
	case RK_OFFSET:
	case RK_SIZED:
	  /* These aren't base regions.  */
	  gcc_unreachable ();

	/* Valid cases.  */

	case RK_ALLOCA:
	case RK_DECL:
	case RK_FRAME:
	case RK_FUNCTION:
	case RK_LABEL:
	case RK_PRIVATE:
	case RK_VAR_ARG:
	  /* We expect different regions of these kinds to have
	     different addresses.  */
	  return tristate (false);

	case RK_HEAP_ALLOCATED:
	  /* We have results of two different heap allocations.
	     If they were non-zero-sized allocations they are different from
	     each other, but zero-sized allocations might alias.
	     Note that this doesn't handle the case where of a freed pointer
	     possibly being reused, but we don't have a way to distinguish
	     this case from the "two different allocations case", and it seems
	     much more important to keep the latter from aliasing each
	     other (for precision when tracking writes).   */
	  {
	    tristate size_a_gt_zero (tristate::unknown ());
	    tristate size_b_gt_zero (tristate::unknown ());
	    const svalue *zero_size
	      = model.get_manager ()->get_or_create_int_cst (size_type_node, 0);

	    if (auto sval_size_a = model.get_dynamic_extents (base_reg_a))
	      {
		size_a_gt_zero = model.eval_condition (sval_size_a,
						       GT_EXPR,
						       zero_size);
	      }
	    if (auto sval_size_b = model.get_dynamic_extents (base_reg_b))
	      {
		size_b_gt_zero = model.eval_condition (sval_size_b,
						       GT_EXPR,
						       zero_size);
	      }
	    if (size_a_gt_zero.is_known () && size_b_gt_zero.is_known ())
	      {
		/* We have results of two different heap allocations, and for
		   each we know if the size was non-zero.  */
		if (size_a_gt_zero.is_false () && size_b_gt_zero.is_false ())
		  {
		    /* Different allocations, both zero-sized.  We don't
		       know if they have the same address.  */
		    return tristate::unknown ();
		  }
		else
		  {
		    /* We either have different allocations of non-zero size,
		       or different allocations where one has non-zero size, the
		       other is zero-sized.  We know these have different
		       addresses.  */
		    return tristate (false);
		  }
	      }
	    if (size_a_gt_zero.is_true () || size_b_gt_zero.is_true ())
	      {
		/* Different allocations, of which at least one > 0 in size.
		   They must have different addresses.  */
		return tristate (false);
	      }
	    /* At least one size might be zero.  */
	    return tristate::unknown ();
	  }

	case RK_SYMBOLIC:
	case RK_UNKNOWN:
	  /* We don't know that such regions are different from each other.  */
	  return tristate::unknown ();

	case RK_STRING:
	  /* For now, don't attempt to decide if string literals alias
	     each other.  */
	  return tristate::unknown ();
	}
    }
  else
    {
      /* We have two base region instances of different kinds.
	 For many kinds of region this implies they must have different
	 addresses.  */
      if (kind_b == RK_SYMBOLIC || kind_b == RK_UNKNOWN)
	return tristate::unknown ();

      switch (kind_a)
	{
	/* Invalid cases.  */

	default:
	  gcc_unreachable ();

	case RK_CODE:
	case RK_ERRNO:
	case RK_GLOBALS:
	case RK_HEAP:
	case RK_ROOT:
	case RK_STACK:
	case RK_THREAD_LOCAL:
	  /* We expect these to be singletons.  */
	  gcc_unreachable ();

	case RK_BIT_RANGE:
	case RK_CAST:
	case RK_ELEMENT:
	case RK_FIELD:
	case RK_OFFSET:
	case RK_SIZED:
	  /* These aren't base regions.  */
	  gcc_unreachable ();

	/* Valid cases.  */

	case RK_ALLOCA:
	case RK_DECL:
	case RK_FRAME:
	case RK_FUNCTION:
	case RK_HEAP_ALLOCATED:
	case RK_LABEL:
	case RK_PRIVATE:
	case RK_STRING:
	case RK_VAR_ARG:
	  /* We expect regions of these kinds to have different addresses
	     from regions of other kinds (apart from symbolic/unknown).  */
	  return tristate (false);

	case RK_SYMBOLIC:
	case RK_UNKNOWN:
	  /* We don't know anything about such regions.  */
	  return tristate::unknown ();
	}
    }
}

/* Half of store::eval_alias; called twice for symmetry.  */

tristate
store::eval_alias_1 (const region *base_reg_a,
		     const region *base_reg_b,
		     const region_model &model) const
{
  gcc_assert (base_reg_a != base_reg_b);

  /* If they're in different memory spaces, they can't alias.  */
  {
    enum memory_space memspace_a = base_reg_a->get_memory_space ();
    if (memspace_a != MEMSPACE_UNKNOWN)
      {
	enum memory_space memspace_b = base_reg_b->get_memory_space ();
	if (memspace_b != MEMSPACE_UNKNOWN
	    && memspace_a != memspace_b)
	  return tristate::TS_FALSE;
      }
  }

  if (const symbolic_region *sym_reg_a
      = base_reg_a->dyn_cast_symbolic_region ())
    {
      const svalue *sval_a = sym_reg_a->get_pointer ();
      if (tree decl_b = base_reg_b->maybe_get_decl ())
	{
	  if (!may_be_aliased (decl_b))
	    return tristate::TS_FALSE;
	  if (sval_a->derived_from_initial_value_p ())
	    if (!is_global_var (decl_b))
	      {
		/* The initial value of a pointer can't point to a local.  */
		return tristate::TS_FALSE;
	      }
	}
      if (sval_a->derived_from_initial_value_p ()
	  && base_reg_b->get_kind () == RK_HEAP_ALLOCATED)
	{
	  /* The initial value of a pointer can't point to a
	     region that was allocated on the heap after the beginning of the
	     path.  */
	  return tristate::TS_FALSE;
	}
      if (const widening_svalue *widening_sval_a
	  = sval_a->dyn_cast_widening_svalue ())
	{
	  const svalue *base = widening_sval_a->get_base_svalue ();
	  if (const pointer_svalue *region_sval
		= base->dyn_cast_pointer_svalue ())
	    {
	      gcc_unreachable (); // FIXME
#if 0
	      const region *pointee = region_sval->get_pointee ();
	      /* If we have sval_a is WIDENING(&REGION, OP), and
		 B can't alias REGION, then B can't alias A either.
		 For example, A might arise from
		   for (ptr = &REGION; ...; ptr++)
		 where sval_a is ptr in the 2nd iteration of the loop.
		 We want to ensure that "*ptr" can only clobber things
		 within REGION's base region.  */
	      tristate ts = eval_alias (pointee->get_base_region (),
					base_reg_b,
					model);
	      if (ts.is_false ())
		return tristate::TS_FALSE;
#endif
	    }
	}
    }

  return tristate::TS_UNKNOWN;
}


const symbolic::bit_contents &
store::get_baseline_contents (const region &base_reg,
			      baseline_state bs) const
{
  auto &mgr = m_node_mgr.get_bit_contents_manager ();
  return mgr.get_baseline_contents (base_reg, bs);
}

/* Get the bit_contents for BASE_REG at BS, for use when BASE_REG
   does not have a binding_cluster (and is thus currently having
   its contents modelled implicitly).  */

const symbolic::bit_contents &
store::get_implicit_contents (const region &base_reg,
			      baseline_state bs) const
{
  if (m_called_unknown_fn
      && base_reg.get_parent_region ()->get_kind () == RK_GLOBALS)
    if (tree decl = base_reg.maybe_get_decl ())
      if (TREE_PUBLIC (decl) && !TREE_READONLY (decl))
	{
	  auto &bc_mgr = m_node_mgr.get_bit_contents_manager ();
	  return bc_mgr.get_untracked ({});
	}
  if (base_reg.symbolic_for_unknown_ptr_p ())
    {
      auto &bc_mgr = m_node_mgr.get_bit_contents_manager ();
      return bc_mgr.get_untracked ({});
    }

  return get_baseline_contents (base_reg, bs);
}

/* Record all of the values in MAYBE_LIVE_VALUES as being possibly live.  */

void
store::on_maybe_live_values (const value_set &maybe_live_values,
			     baseline_state bs)
{
  for (auto val : maybe_live_values)
    if (auto *sval = val->dyn_cast_svalue ())
      if (auto *sym_ptr = sval->maybe_get_symbolic_pointer ())
	{
	  const region *base_reg = &sym_ptr->get_base_region ();
	  mark_as_escaped (base_reg, bs);
	}
}

/* Return true iff the bits of BIT_RANGE from UBC are live within
   this store.  */

bool
store::bits_live_p (const symbolic::bit_vector_from_ubc &bv_from_ubc,
		    const region_model &model) const
{
  if (auto *baseline_bc
	= bv_from_ubc.get_bit_contents ().dyn_cast_baseline_bit_contents ())
    {
      /* Are the bits implicitly live due to the region
	 being untouched?  */
      const region *base_reg = baseline_bc->get_baseline_key ().m_base_reg;
      symbolic::memory_range mem_range (*base_reg,
					bv_from_ubc.get_bit_range ());
      if (&get_bits (mem_range, model.get_baseline_state ()) == &bv_from_ubc)
	return true;
    }

  for (auto iter : m_cluster_map)
    {
      const region *base_reg = iter.first;
      binding_cluster *cluster = iter.second;
      if (cluster->bits_live_p (bv_from_ubc))
	return true;
    }
  return false;
}

/* Remove all bindings overlapping REG within this store.  */

void
store::clobber_region (const region *reg,
		       baseline_state bs,
		       region_model_manager &rmm)
{
  const region *base_reg = reg->get_base_region ();
  binding_cluster **slot = m_cluster_map.get (base_reg);
  if (!slot)
    return;
  binding_cluster *cluster = *slot;

  if (reg == base_reg)
    {
      delete cluster;
      m_cluster_map.remove (base_reg);
    }
  else
    {
      auto mem_range = reg->get_memory_range (rmm);
      auto &bv_mgr = rmm.get_node_manager ().get_bit_vector_manager ();
      auto &uninit_bv
	= bv_mgr.get_uninitialized_bits (mem_range.get_size ());
      cluster->set_bits (mem_range.get_bit_range (),
			 uninit_bv,
			 nullptr);
      if (cluster->redundant_p (*this, bs))
	{
	  delete cluster;
	  m_cluster_map.remove (base_reg);
	}
    }
}

/* Remove any bindings for REG within this store.  */

void
store::purge_region (const region *reg)
{
  const region *base_reg = reg->get_base_region ();
  gcc_assert (reg == base_reg);
  binding_cluster **slot = m_cluster_map.get (base_reg);
  if (!slot)
    return;
  binding_cluster *cluster = *slot;
  delete cluster;
  m_cluster_map.remove (base_reg);
}

/* Mark REG as having unknown content.  */

void
store::mark_region_as_unknown (const region *reg,
			       baseline_state bs,
			       uncertainty_t *uncertainty,
			       const region_model &model)
{
  const region *base_reg = reg->get_base_region ();
  if (base_reg->symbolic_for_unknown_ptr_p ()
      || !base_reg->tracked_p ())
    return;

  auto &rmm = m_node_mgr.get_region_model_manager ();
  auto mem_range = reg->get_memory_range (rmm);
  if (mem_range.get_size ().is_zero_p ())
    return;

  auto &bv_mgr = m_node_mgr.get_bit_vector_manager ();
  auto &unknown_bv = bv_mgr.get_unknown_bits (mem_range.get_size ());
  set_bits (mem_range, unknown_bv,
	    uncertainty,
	    model);
}

/* Purge state involving NODE.  */

void
store::purge_state_involving (const symbolic::node &node)
{
  auto_vec <const region *> base_regs_to_purge;
  for (auto iter : m_cluster_map)
    {
      const region *base_reg = iter.first;
      if (base_reg->involves_p (node))
	base_regs_to_purge.safe_push (base_reg);
      else
	{
	  binding_cluster *cluster = iter.second;
	  cluster->purge_state_involving (node);
	}
    }

  for (auto iter : base_regs_to_purge)
    purge_cluster (iter);
}

/* Get the cluster for BASE_REG, or nullptr (const version).  */

const binding_cluster *
store::get_cluster (const region *base_reg) const
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);
  if (binding_cluster **slot
	= const_cast <cluster_map_t &> (m_cluster_map).get (base_reg))
    return *slot;
  else
    return nullptr;
}

/* Get the cluster for BASE_REG, or nullptr (non-const version).  */

binding_cluster *
store::get_cluster (const region *base_reg)
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);
  if (binding_cluster **slot = m_cluster_map.get (base_reg))
    return *slot;
  else
    return nullptr;
}

/* Get the cluster for BASE_REG, creating it if doesn't already exist.  */

binding_cluster *
store::get_or_create_cluster (const region *base_reg,
			      baseline_state bs)
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);

  /* We shouldn't create clusters for dereferencing an UNKNOWN ptr.  */
  gcc_assert (!base_reg->symbolic_for_unknown_ptr_p ());

  /* We shouldn't create clusters for base regions that aren't trackable.  */
  gcc_assert (base_reg->tracked_p ());

  if (binding_cluster **slot = m_cluster_map.get (base_reg))
    return *slot;

  auto &baseline = get_implicit_contents (*base_reg, bs);
  binding_cluster *cluster
    = new binding_cluster (m_node_mgr, base_reg, baseline);
  m_cluster_map.put (base_reg, cluster);

  return cluster;
}

/* Remove any cluster for BASE_REG, for use by
   region_model::unbind_region_and_descendents
   when popping stack frames and handling deleted heap regions.  */

void
store::purge_cluster (const region *base_reg)
{
  gcc_assert (base_reg->get_base_region () == base_reg);
  binding_cluster **slot = m_cluster_map.get (base_reg);
  if (!slot)
    return;
  binding_cluster *cluster = *slot;
  delete cluster;
  m_cluster_map.remove (base_reg);
}

/* Attempt to merge STORE_A and STORE_B into OUT_STORE.
   Return true if successful, or false if the stores can't be merged.  */

bool
store::can_merge_p (const store *store_a, const store *store_b,
		    store *out_store,
		    model_merger *merger)
{
  if (store_a->m_called_unknown_fn || store_b->m_called_unknown_fn)
    out_store->m_called_unknown_fn = true;

  /* Get the union of all base regions for STORE_A and STORE_B.  */
  hash_set<const region *> base_regions;
  for (cluster_map_t::iterator iter_a = store_a->m_cluster_map.begin ();
       iter_a != store_a->m_cluster_map.end (); ++iter_a)
    {
      const region *base_reg_a = (*iter_a).first;
      base_regions.add (base_reg_a);
    }
  for (cluster_map_t::iterator iter_b = store_b->m_cluster_map.begin ();
       iter_b != store_b->m_cluster_map.end (); ++iter_b)
    {
      const region *base_reg_b = (*iter_b).first;
      base_regions.add (base_reg_b);
    }

  /* Sort the base regions before considering them.  This ought not to
     affect the results, but can affect which types UNKNOWN_REGIONs are
     created for in a run; sorting them thus avoids minor differences
     in logfiles.  */
  auto_vec<const region *> vec_base_regions (base_regions.elements ());
  for (hash_set<const region *>::iterator iter = base_regions.begin ();
       iter != base_regions.end (); ++iter)
    vec_base_regions.quick_push (*iter);
  vec_base_regions.qsort (region::cmp_ptr_ptr);
  unsigned i;
  const region *base_reg;
  FOR_EACH_VEC_ELT (vec_base_regions, i, base_reg)
    {
      const binding_cluster *cluster_a = store_a->get_cluster (base_reg);
      const binding_cluster *cluster_b = store_b->get_cluster (base_reg);
      /* At least one of cluster_a and cluster_b must be non-NULL.  */
      binding_cluster *out_cluster
	= out_store->get_or_create_cluster
	    (base_reg,
	     merger->m_model_a->get_baseline_state ());
      if (!binding_cluster::can_merge_p (cluster_a, cluster_b,
					 out_cluster, out_store, merger))
	return false;
    }
  return true;
}

/* Mark the cluster for BASE_REG as having escaped.
   For use when handling an unrecognized function call, and
   for params to "top-level" calls.
   Further unknown function calls could touch it, even if the cluster
   isn't reachable from args of those calls.  */

void
store::mark_as_escaped (const region *base_reg,
			baseline_state bs)
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);

  if (base_reg->symbolic_for_unknown_ptr_p ()
      || !base_reg->tracked_p ())
    return;

  binding_cluster *cluster = get_or_create_cluster (base_reg, bs);
  cluster->mark_as_escaped ();
}

/* Handle an unknown fncall by updating any clusters that have escaped
   (either in this fncall, or in a prior one).  */

void
store::on_unknown_fncall (const gcall &call,
			  const conjured_purge &p)
{
  m_called_unknown_fn = true;

  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    (*iter).second->on_unknown_fncall (call, p);
}

/* Mark all of BASE_REG as having conjured contents.  */

void
store::mark_region_as_conjured (const region &base_reg,
				baseline_state bs,
				const gimple &stmt,
				const conjured_purge &p)
{
  gcc_assert (base_reg.get_base_region () == &base_reg);

  // TODO: what if symbolic?

  if (!base_reg.tracked_p ())
    return;

  binding_cluster *cluster = get_or_create_cluster (&base_reg, bs);
  cluster->mark_region_as_conjured (stmt, p);
}

/* Return true if a non-const pointer to BASE_REG (or something within it)
   has escaped to code outside of the TU being analyzed.  */

bool
store::escaped_p (const region *base_reg) const
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);

  /* "errno" can always be modified by external code.  */
  if (base_reg->get_kind () == RK_ERRNO)
    return true;

  if (binding_cluster **cluster_slot
      = const_cast <cluster_map_t &>(m_cluster_map).get (base_reg))
    return (*cluster_slot)->escaped_p ();
  return false;
}

/* Populate OUT_PVS with a list of path_vars for describing SVAL based on
   this store, using VISITED to ensure the traversal terminates.  */

void
store::get_representative_path_vars (const region_model *model,
				     value_set *visited,
				     const svalue *sval,
				     logger *logger,
				     auto_vec<path_var> *out_pvs) const
{
  gcc_assert (sval);

  /* Find all bindings that reference SVAL.  */
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      binding_cluster *cluster = (*iter).second;
      cluster->get_representative_path_vars (model, visited, base_reg, sval,
					     logger,
					     out_pvs);
    }

  if (const initial_svalue *init_sval = sval->dyn_cast_initial_svalue ())
    {
      const region *reg = init_sval->get_region ();
      if (path_var pv = model->get_representative_path_var (reg,
							    visited,
							    logger))
	out_pvs->safe_push (pv);
    }
}

/* Canonicalize this store, to maximize the chance of equality between
   instances.  */

void
store::canonicalize ()
{
  // No-op
}

static bool
needs_loop_replay_fixup_p (const symbolic::node &node)
{
  struct my_visitor : public symbolic::node_visitor
  {
    my_visitor () : m_found_widening (false) {}

    bool
    visit_svalue (const svalue &sval) final override
    {
      if (sval.dyn_cast_widening_svalue ())
	m_found_widening = true;
      return true;
    }

    bool m_found_widening;
  } v;

  node.accept_node_visitor (v);
  return v.m_found_widening;
}

/* Subroutine for use by exploded_path::feasible_p.

   We need to deal with state differences between:
   (a) when the exploded_graph is being initially constructed and
   (b) when replaying the state changes along a specific path in
   in exploded_path::feasible_p.

   In (a), state merging happens, so when exploring a loop
     for (i = 0; i < 1024; i++)
   on successive iterations we have i == 0, then i == WIDENING.

   In (b), no state merging happens, so naively replaying the path
   that goes twice through the loop then exits it
   would lead to i == 0, then i == 1, and then a (i >= 1024) eedge
   that exits the loop, which would be found to be infeasible as i == 1,
   and the path would be rejected.

   We need to fix up state during replay.  This subroutine is
   called whenever we enter a supernode that we've already
   visited along this exploded_path, passing in OTHER_STORE
   from the destination enode's state.

   Find bindings to widening values in OTHER_STORE.
   For all that are found, update the binding in this store to UNKNOWN.  */

void
store::loop_replay_fixup (const store *other_store,
			  baseline_state bs,
			  region_model_manager *mgr)
{
  gcc_assert (other_store);
  for (cluster_map_t::iterator iter = other_store->m_cluster_map.begin ();
       iter != other_store->m_cluster_map.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      binding_cluster *cluster = (*iter).second;

      if (needs_loop_replay_fixup_p (*cluster->m_contents))
	{
	  binding_cluster *this_cluster = get_or_create_cluster (base_reg, bs);
	  auto &node_mgr = mgr->get_node_manager ();
	  this_cluster->mark_as_untracked (node_mgr, nullptr, nullptr);
	}
    }
}

/* Attempt to use R to replay the bindings from SUMMARY into this object.
   Return true if successful.  */

bool
store::replay_call_summary (call_summary_replay &r,
			    const store &summary)
{
  if (summary.m_called_unknown_fn)
    {
      /* A call to an external function occurred in the summary.
	 Hence we need to invalidate our knowledge of globals,
	 escaped regions, etc.  */
      on_unknown_fncall (r.get_call_stmt (),
			 conjured_purge (r.get_caller_model (),
					 r.get_ctxt ()));
    }

  auto_vec<const region *> keys (summary.m_cluster_map.elements ());
  for (auto kv : summary.m_cluster_map)
    keys.quick_push (kv.first);
  keys.qsort (region::cmp_ptr_ptr);
  for (auto base_reg : keys)
    if (!replay_call_summary_cluster (r, summary, base_reg))
      return false;
  return true;
}

/* Attempt to use R and SUMMARY to replay the bindings in SUMMARY_CLUSTER
   into this object.
   Return true if successful.  */

bool
store::replay_call_summary_cluster (call_summary_replay &r,
				    const store &summary,
				    const region *summary_base_reg)
{
  const call_details &cd = r.get_call_details ();
  region_model_manager *reg_mgr = r.get_manager ();
  const binding_cluster *summary_cluster
    = summary.get_cluster (summary_base_reg);

  auto &bit_contents_mgr
    = reg_mgr->get_node_manager ().get_bit_contents_manager ();

  /* Handle "ESCAPED" and "TOUCHED" flags.  */
  if (summary_cluster->escaped_p () || summary_cluster->touched_p ())
    if (const region *caller_reg
	= r.convert_region_from_summary (summary_base_reg))
      {
	const region *caller_base_reg = caller_reg->get_base_region ();
	if (caller_base_reg->tracked_p ()
	    && !caller_base_reg->symbolic_for_unknown_ptr_p ())
	  {
	    binding_cluster *caller_cluster
	      = get_or_create_cluster
		  (caller_base_reg,
		   r.get_caller_model ()->get_baseline_state ());
	    if (summary_cluster->escaped_p ())
	      caller_cluster->mark_as_escaped ();
	    if (summary_cluster->touched_p ())
	      caller_cluster->m_touched = true;
	  }
      }

  switch (summary_base_reg->get_kind ())
    {
    /* Top-level regions.  */
    case RK_FRAME:
    case RK_GLOBALS:
    case RK_CODE:
    case RK_STACK:
    case RK_HEAP:
    case RK_THREAD_LOCAL:
    case RK_ROOT:
    /* Child regions.  */
    case RK_FIELD:
    case RK_ELEMENT:
    case RK_OFFSET:
    case RK_SIZED:
    case RK_CAST:
    case RK_BIT_RANGE:
    /* Other regions.  */
    case RK_VAR_ARG:
    case RK_UNKNOWN:
      /* These should never be the base region of a binding cluster.  */
      gcc_unreachable ();
      break;

    case RK_FUNCTION:
    case RK_LABEL:
    case RK_STRING:
      /* These can be marked as escaping.  */
      break;

    case RK_SYMBOLIC:
      {
	const symbolic_region *summary_symbolic_reg
	  = as_a <const symbolic_region *> (summary_base_reg);
	const svalue *summary_ptr_sval = summary_symbolic_reg->get_pointer ();
	const svalue *caller_ptr_sval
	  = r.convert_svalue_from_summary (summary_ptr_sval);
	if (!caller_ptr_sval)
	  return false;
	const region *caller_dest_reg
	  = cd.get_model ()->deref_rvalue (caller_ptr_sval,
					   NULL_TREE,
					   cd.get_ctxt ());
	auto &summary_bits
	  = summary.get_bit_contents (*summary_symbolic_reg,
				      r.get_caller_model ()->get_baseline_state ());
	if (auto *baseline_bc = summary_bits.dyn_cast_baseline_bit_contents ())
	  if (baseline_bc->get_baseline_key ().m_base_reg == summary_base_reg)
	    {
	      /* The summary's bit contents is just the baseline for
		 summary_base_reg; we don't need to write anything.  */
	      return true;
	    }
	auto *caller_bits = r.convert_bit_contents_from_summary (summary_bits);
	if (!caller_bits)
	  caller_bits = &bit_contents_mgr.get_untracked ({});
	return replay_call_summary_bit_contents
	  (r, summary, *summary_base_reg,
	   *caller_dest_reg, *caller_bits);
      }
      break;

    case RK_HEAP_ALLOCATED:
    case RK_DECL:
    case RK_ERRNO:
    case RK_PRIVATE:
      {
	const region *caller_dest_reg
	  = r.convert_region_from_summary (summary_base_reg);
	if (!caller_dest_reg)
	  return false;
	auto &summary_bits
	  = summary.get_bit_contents (*summary_base_reg,
				      r.get_caller_model ()->get_baseline_state ());
	if (auto *baseline_bc = summary_bits.dyn_cast_baseline_bit_contents ())
	  if (baseline_bc->get_baseline_key ().m_base_reg == summary_base_reg)
	    {
	      /* The summary's bit contents is just the baseline for
		 summary_base_reg; we don't need to write anything.  */
	      return true;
	    }
	auto *caller_bits = r.convert_bit_contents_from_summary (summary_bits);
	if (!caller_bits)
	  caller_bits = &bit_contents_mgr.get_untracked ({});
	return replay_call_summary_bit_contents
	  (r, summary, *summary_base_reg,
	   *caller_dest_reg, *caller_bits);
      }
      break;

    case RK_ALLOCA:
      /* Ignore bindings of alloca regions in the summary.  */
      break;
    }

  return true;
}

bool
store::replay_call_summary_bit_contents (call_summary_replay &r,
					 const store &summary,
					 const region &summary_base_reg,
					 const region &caller_dest_reg,
					 const symbolic::bit_contents &caller_bc)
{
  if (caller_dest_reg.symbolic_for_unknown_ptr_p ())
    return false;

  switch (caller_bc.get_kind ())
    {
    default:
      return false; // TODO

    case symbolic::bit_contents::kind::uninit:
      /* No-op: nothing was written in summary, so the replay
	 writes nothing.  */
      break;

    case symbolic::bit_contents::kind::untracked:
      {
	const region *base_reg = caller_dest_reg.get_base_region ();
	if (base_reg->tracked_p ())
	  {
	    binding_cluster *caller_cluster
	      = get_or_create_cluster (caller_dest_reg.get_base_region (),
				       r.get_caller_model ()->get_baseline_state ());
	    caller_cluster->set_bit_contents (caller_bc);
	  }
      }
      break;

    case symbolic::bit_contents::kind::zero:
      {
	return false; // TODO
      }
      break;

    case symbolic::bit_contents::kind::baseline:
    case symbolic::bit_contents::kind::offset:
      {
	auto &rmm = *r.get_manager ();
	auto caller_dest_mem_offset = caller_dest_reg.get_memory_offset (rmm);
	auto &bc_mgr = caller_bc.get_bit_contents_manager ();
	auto &caller_offset_bc = bc_mgr.get_offset_bit_contents
	  (caller_bc,
	   caller_dest_mem_offset.get_bit_offset ());
	// FIXME: is the sign of the offset correct?
	binding_cluster *caller_cluster
	  = get_or_create_cluster (&caller_dest_mem_offset.get_base_region (),
				   r.get_caller_model ()->get_baseline_state ()); // FIXME
	caller_cluster->set_bit_contents (caller_offset_bc);
	// FIXME: this bypasses access checks; should it?
      }
      break;
    case symbolic::bit_contents::kind::conjured:
      return false; // TODO

    case symbolic::bit_contents::kind::named_test:
      return false; // TODO

    case symbolic::bit_contents::kind::concrete_overlay:
      {
	auto &caller_overlay
	  = (const symbolic::concrete_bit_overlay &)caller_bc;

	/* First, apply the baseline.  */
	auto &caller_baseline = caller_overlay.get_baseline ();
	replay_call_summary_bit_contents (r, summary, summary_base_reg,
					  caller_dest_reg, caller_baseline);

	/* Now apply the bindings.  */
	for (auto iter : caller_overlay.m_concrete_range_map)
	  {
	    auto &conc_range = iter.first;
	    auto &bv = iter.second;
	    auto &rmm = *r.get_manager ();
	    auto *affected_region
	      = rmm.get_bit_range (&caller_dest_reg, NULL_TREE, conc_range);
	    set_bits (affected_region, bv, nullptr, *r.get_caller_model ());
	  }
      }
      break;
    case symbolic::bit_contents::kind::symbolic_overlay:
      {
	auto &caller_overlay
	  = (const symbolic::symbolic_bit_overlay &)caller_bc;

	/* First, apply the baseline.  */
	auto &caller_baseline = caller_overlay.get_baseline ();
	replay_call_summary_bit_contents (r, summary, summary_base_reg,
					  caller_dest_reg, caller_baseline);

	/* Now apply the bindings.  */
	for (auto iter : caller_overlay.m_symbolic_range_map)
	  {
	    return false; // TODO
	  }
      }
      break;
    }

  return true;
}

#if CHECKING_P

namespace selftest {

/* Verify that bit_range::intersects_p works as expected.  */

static void
test_bit_range_intersects_p ()
{
  bit_range b0 (0, 1);
  bit_range b1 (1, 1);
  bit_range b2 (2, 1);
  bit_range b3 (3, 1);
  bit_range b4 (4, 1);
  bit_range b5 (5, 1);
  bit_range b6 (6, 1);
  bit_range b7 (7, 1);
  bit_range b1_to_6 (1, 6);
  bit_range b0_to_7 (0, 8);
  bit_range b3_to_5 (3, 3);
  bit_range b6_to_7 (6, 2);

  /* self-intersection is true.  */
  ASSERT_TRUE (b0.intersects_p (b0));
  ASSERT_TRUE (b7.intersects_p (b7));
  ASSERT_TRUE (b1_to_6.intersects_p (b1_to_6));
  ASSERT_TRUE (b0_to_7.intersects_p (b0_to_7));

  ASSERT_FALSE (b0.intersects_p (b1));
  ASSERT_FALSE (b1.intersects_p (b0));
  ASSERT_FALSE (b0.intersects_p (b7));
  ASSERT_FALSE (b7.intersects_p (b0));

  ASSERT_TRUE (b0_to_7.intersects_p (b0));
  ASSERT_TRUE (b0_to_7.intersects_p (b7));
  ASSERT_TRUE (b0.intersects_p (b0_to_7));
  ASSERT_TRUE (b7.intersects_p (b0_to_7));

  ASSERT_FALSE (b0.intersects_p (b1_to_6));
  ASSERT_FALSE (b1_to_6.intersects_p (b0));
  ASSERT_TRUE (b1.intersects_p (b1_to_6));
  ASSERT_TRUE (b1_to_6.intersects_p (b1));
  ASSERT_TRUE (b1_to_6.intersects_p (b6));
  ASSERT_FALSE (b1_to_6.intersects_p (b7));

  ASSERT_TRUE (b1_to_6.intersects_p (b0_to_7));
  ASSERT_TRUE (b0_to_7.intersects_p (b1_to_6));

  ASSERT_FALSE (b3_to_5.intersects_p (b6_to_7));
  ASSERT_FALSE (b6_to_7.intersects_p (b3_to_5));

  bit_range r1 (0,0);
  bit_range r2 (0,0);
  ASSERT_TRUE (b1_to_6.intersects_p (b0_to_7, &r1, &r2));
  ASSERT_EQ (r1.get_start_bit_offset (), 0);
  ASSERT_EQ (r1.m_size_in_bits, 6);
  ASSERT_EQ (r2.get_start_bit_offset (), 1);
  ASSERT_EQ (r2.m_size_in_bits, 6);

  ASSERT_TRUE (b0_to_7.intersects_p (b1_to_6, &r1, &r2));
  ASSERT_EQ (r1.get_start_bit_offset (), 1);
  ASSERT_EQ (r1.m_size_in_bits, 6);
  ASSERT_EQ (r2.get_start_bit_offset (), 0);
  ASSERT_EQ (r2.m_size_in_bits, 6);
}

/* Run all of the selftests within this file.  */

void
analyzer_store_cc_tests ()
{
  test_bit_range_intersects_p ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
