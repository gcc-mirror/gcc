/* Symbolic maps of bits e.g. "bytes [0-4): <bitvec>, bytes[16-20): <bitvec>"
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "gimple-pretty-print.h"
#include "tm.h"

#include "diagnostics/markup.h"

#include "selftest.h"

#include "analyzer/symbolic-bit-contents.h"
#include "analyzer/region-model.h"
#include "analyzer/type.h"
#include "analyzer/record-layout.h"

#if ENABLE_ANALYZER

namespace ana {
namespace symbolic {

bool
baseline_key::operator< (const baseline_key &other) const
{
  if (int c = node::cmp (*m_base_reg, *other.m_base_reg))
    return c < 0;
  return m_baseline_state < other.m_baseline_state;
}

// class bit_contents : public symbolic::node

const bit_vector &
bit_contents::get_bytes (const symbolic_byte_range &byte_range) const
{
  return get_bits (bits_from_bytes (byte_range));
}

const bit_vector &
bit_contents::get_bits (const concrete_bit_range &bit_range) const
{
  return get_bits (symbolic_bit_range (bit_range,
				       m_node_mgr.get_scalars ().get_bits ()));
}

const bit_vector &
bit_contents::get_bytes (const concrete_byte_range &byte_range) const
{
  return get_bytes
    (symbolic_byte_range (byte_range,
			  m_node_mgr.get_scalars ().get_bytes ()));
}

const bit_contents &
bit_contents::set_bits (const symbolic_bit_range &bit_range,
			const bit_vector &bv,
			uncertainty_t *uncertainty) const
{
  auto &mgr = get_bit_contents_manager ();
  return mgr.set_bits (*this, bit_range, bv, uncertainty);
}

const bit_contents &
bit_contents::set_bits (const concrete_bit_range &conc_bit_range,
			const bit_vector &bv,
			uncertainty_t *uncertainty) const
{
  return set_bits
    (symbolic_bit_range (conc_bit_range,
			 m_node_mgr.get_scalars ().get_bits ()),
     bv,
     uncertainty);
}

const bit_contents &
bit_contents::set_bytes (const symbolic_byte_range &byte_range,
			 const bit_vector &bv,
			 uncertainty_t *uncertainty) const
{
  return set_bits (bits_from_bytes (byte_range), bv, uncertainty);
}

const bit_contents &
bit_contents::set_bytes (const concrete_byte_range &byte_range,
			 const bit_vector &bv,
			 uncertainty_t *uncertainty) const
{
  return set_bytes
    (symbolic_byte_range (byte_range,
			  m_node_mgr.get_scalars ().get_bytes ()),
     bv,
     uncertainty);
}

const bit_contents &
bit_contents::set_byte (integer_with_unit<units::byte> byte_offset,
			uint8_t value,
			uncertainty_t *uncertainty) const
{
  auto &bv_mgr = get_bit_vector_manager ();
  return set_bytes (concrete_byte_range (byte_offset, 1_byte),
		    bv_mgr.get_uint8 (value),
		    uncertainty);
}

const bit_contents *
bit_contents::can_merge_p (const bit_contents &bit_contents_a,
			   const bit_contents &bit_contents_b,
			   tree type,
			   model_merger *merger)
{
  if (&bit_contents_a == &bit_contents_b)
    return &bit_contents_a;

  auto &bit_contents_mgr = bit_contents_a.get_bit_contents_manager ();

  /* Attempt mergers of concrete_bit_overlay, and with their baselines.  */
  auto *conc_bit_overlay_a = bit_contents_a.dyn_cast_concrete_bit_overlay ();
  auto *conc_bit_overlay_b = bit_contents_b.dyn_cast_concrete_bit_overlay ();
  if (conc_bit_overlay_a)
    {
      if (conc_bit_overlay_b)
	{
	  if (&conc_bit_overlay_a->get_baseline ()
	      == &conc_bit_overlay_b->get_baseline ())
	    return concrete_bit_overlay::can_merge_p
	      (bit_contents_a,
	       bit_contents_b,
	       conc_bit_overlay_a->get_baseline (),
	       conc_bit_overlay_a,
	       conc_bit_overlay_b,
	       type,
	       merger);
	}
      else
	{
	  if (&conc_bit_overlay_a->get_baseline () == &bit_contents_b)
	    return concrete_bit_overlay::can_merge_p
	      (bit_contents_a,
	       bit_contents_b,
	       bit_contents_b,
	       conc_bit_overlay_a,
	       nullptr,
	       type,
	       merger);
	}
    }
  else
    {
      if (conc_bit_overlay_b)
	{
	  if (&conc_bit_overlay_b->get_baseline () == &bit_contents_a)
	    return concrete_bit_overlay::can_merge_p
	      (bit_contents_a,
	       bit_contents_b,
	       bit_contents_a,
	       nullptr,
	       conc_bit_overlay_b,
	       type,
	       merger);
	}
      else
	{
	  /* Neither are concrete bit overlays.  */
	}
    }

  /* Allow mergers of e.g. conjured_bit_vector.  */
  value_set maybe_live_in_a = bit_contents_a.get_maybe_live_values ();
  value_set maybe_live_in_b = bit_contents_b.get_maybe_live_values ();
  value_set maybe_live_in_either = maybe_live_in_a.union_ (maybe_live_in_b);
  return &bit_contents_mgr.get_untracked (std::move (maybe_live_in_either));
}

value_set
bit_contents::get_maybe_live_values () const
{
  value_set maybe_live;
  this->for_each_value ([] (const symbolic::value_node &val,
			    void *user_data) -> iter_result {
    value_set *maybe_live = static_cast<value_set *> (user_data);
    maybe_live->add (&val);
    return iter_result::keep_going;
  }, &maybe_live);
  return maybe_live;
}

// class uniform_bit_contents : public bit_contents

const bit_contents *
uniform_bit_contents::maybe_fold_offset (const symbolic_bit_offset &) const
{
  /* Ignore the offset and fold to this bit_contents.  */
  return this;
}

iter_result
uniform_bit_contents::
for_each_value (iter_result (*) (const symbolic::value_node &val,
				 void *user_data),
		void *) const
{
  // No-op
  return iter_result::keep_going;
}

// class uninit_contents : public uniform_bit_contents

uninit_contents::uninit_contents (node_manager &node_mgr)
: uniform_bit_contents (complexity (1, 1), node_mgr)
{
}

const bit_vector &
uninit_contents::get_bits (const symbolic_bit_range &bit_range) const
{
  return get_bit_vector_manager ().get_uninitialized_bits
    (bit_range.get_size ());
}

void
uninit_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "uninitialized contents");
}

void
uninit_contents::add_dump_widget_children (text_art::tree_widget &,
					   node_dump_state &) const
{
  // no-op
}

void
uninit_contents::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_contents (*this);
}

std::unique_ptr<json::object>
uninit_contents::to_json () const
{
  return make_json_obj ("uninit_contents");
}

std::unique_ptr<diagnostics::markup::node>
uninit_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("uninit_contents");
}

string_scan
uninit_contents::
scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset) const
{
  return string_scan::found_poison (start_byte_offset, poison_kind::uninit);
}

void
uninit_contents::
get_representative_path_vars (const region_model *,
			      value_set *,
			      const region *,
			      const svalue *,
			      tree,
			      logger *,
			      auto_vec<path_var> *) const
{
  // no-op
}

const bit_contents &
uninit_contents::
poison_any_pointers_to_descendents (const region *,
				    enum poison_kind) const
{
  // no-op
  return *this;
}

const bit_contents &
uninit_contents::purge_state_involving (const symbolic::node &) const
{
  // no-op
  return *this;
}

// class untracked_bit_contents : public uniform_bit_contents

bool
untracked_bit_contents::key::operator< (const key &other) const
{
  return m_maybe_live < other.m_maybe_live;
}

complexity
untracked_bit_contents::key::get_complexity () const
{
  complexity child_complexities (0, 0);

  for (auto iter : m_maybe_live)
    child_complexities += iter->get_complexity ();

  /* One extra node for the node itself; one more level of depth.  */
  return complexity (child_complexities.m_num_nodes + 1,
		     child_complexities.m_max_depth + 1);
}

untracked_bit_contents::untracked_bit_contents (node_manager &node_mgr,
						const key &k)
: uniform_bit_contents (k.get_complexity (), node_mgr),
  m_maybe_live (k.m_maybe_live)
{
  if (CHECKING_P)
    for (auto iter : m_maybe_live)
      gcc_assert (track_value_liveness_p (*iter));
}

const bit_vector &
untracked_bit_contents::get_bits (const symbolic_bit_range &bit_range) const
{
  return get_bit_vector_manager ().get_unknown_bits (bit_range.get_size ());
}

void
untracked_bit_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "untracked_bit_contents");
}

void
untracked_bit_contents::add_dump_widget_children (text_art::tree_widget &w,
						  node_dump_state &nds) const
{
  if (!m_maybe_live.empty ())
    {
      using tree_widget = text_art::tree_widget;
      std::unique_ptr<tree_widget> maybe_live_w
	(tree_widget::from_fmt (nds, nullptr, "maybe live values"));
      for (auto iter : m_maybe_live)
	maybe_live_w->add_child (iter->make_dump_widget (nds));
      w.add_child (std::move (maybe_live_w));
    }
}

void
untracked_bit_contents::accept_node_visitor (node_visitor &v) const
{
  if (v.visit_bit_contents (*this))
    for (auto iter : m_maybe_live)
      iter->accept_node_visitor (v);
}

std::unique_ptr<json::object>
untracked_bit_contents::to_json () const
{
  return make_json_obj ("untracked_bit_contents");
}

std::unique_ptr<diagnostics::markup::node>
untracked_bit_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("untracked_bit_contents");
}

string_scan
untracked_bit_contents::
scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset) const
{
  return string_scan::cannot_scan (start_byte_offset);
}

void
untracked_bit_contents::
get_representative_path_vars (const region_model *,
			      value_set *,
			      const region *,
			      const svalue *,
			      tree,
			      logger *,
			      auto_vec<path_var> *) const
{
  // no-op
}

const bit_contents &
untracked_bit_contents::
poison_any_pointers_to_descendents (const region *,
				    enum poison_kind) const
{
  // no-op
  return *this;
}

const bit_contents &
untracked_bit_contents::purge_state_involving (const symbolic::node &n) const
{
  value_set survivors;
  for (auto iter : m_maybe_live)
    if (!iter->involves_p (n))
      survivors.add (iter);

  return get_bit_contents_manager ().get_untracked (std::move (survivors));
}

bool
untracked_bit_contents::bits_live_p (const bit_vector_from_ubc &bv) const
{
  return false;
}

iter_result
untracked_bit_contents::
for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				   void *user_data),
		void *user_data) const
{
  for (auto iter : m_maybe_live)
    if (cb (*iter, user_data) == iter_result::stop)
      return iter_result::stop;

  return iter_result::keep_going;
}

bool
untracked_bit_contents::track_value_liveness_p (const symbolic::value_node &val)
{
  if (auto *sval = val.dyn_cast_svalue ())
    return track_svalue_liveness_p (*sval);
  else if (auto *bv = val.dyn_cast_bit_vector ())
    return track_bit_vector_liveness_p (*bv);
  gcc_unreachable ();
}

bool
untracked_bit_contents::track_svalue_liveness_p (const svalue &sval)
{
  switch (sval.get_kind ())
    {
    default:
      gcc_unreachable ();

    case SK_CONSTANT:
    case SK_UNKNOWN:
    case SK_POISONED:
      return false;

    case SK_POINTER:
    case SK_SETJMP:
    case SK_INITIAL:
    case SK_UNARYOP:
    case SK_BINOP:
    case SK_UNMERGEABLE:
    case SK_PLACEHOLDER:
    case SK_WIDENING:
    case SK_ASM_OUTPUT:
    case SK_CONST_FN_RESULT:
      return true;

    case SK_BIT_VECTOR:
      {
	auto &from_bv = static_cast<const svalue_from_bit_vector &> (sval);
	return track_bit_vector_liveness_p (from_bv.get_bit_vector ());
      }
      break;
    }

  gcc_unreachable ();
}

bool
untracked_bit_contents::
track_bit_vector_liveness_p (const symbolic::bit_vector &bv)
{
  switch (bv.get_kind ())
    {
    default:
      gcc_unreachable ();

    case bit_vector::kind::empty:
    case bit_vector::kind::poisoned:
    case bit_vector::kind::tristate:
      return false;

    case bit_vector::kind::conjured:
    case bit_vector::kind::from_ubc:
    case bit_vector::kind::named_test:
      return true;

    case bit_vector::kind::concat:
      {
	auto &concat_bv = static_cast<const concat_bit_vector &> (bv);
	for (auto iter : concat_bv)
	  if (track_bit_vector_liveness_p (*iter))
	    return true;
	return false;
      }
      break;

    case bit_vector::kind::cropped:
      {
	auto &cropped_bv = static_cast<const cropped_bit_vector &> (bv);
	return track_bit_vector_liveness_p
	  (cropped_bv.get_uncropped_bit_vector ());
      }
      break;

    case bit_vector::kind::repeated:
      {
	auto &repeated_bv = static_cast<const repeated_bit_vector &> (bv);
	return track_bit_vector_liveness_p
	  (repeated_bv.get_inner_bit_vector ());
      }
      break;

    case bit_vector::kind::svalue:
      {
	auto &from_sval = static_cast<const bit_vector_from_svalue &> (bv);
	return track_svalue_liveness_p (from_sval.get_svalue ());
      }
      break;
    }

  gcc_unreachable ();
}

// class zero_contents : public uniform_bit_contents

zero_contents::zero_contents (node_manager &node_mgr)
: uniform_bit_contents (complexity (1, 1), node_mgr)
{
}

const bit_vector &
zero_contents::get_bits (const symbolic_bit_range &bit_range) const
{
  return get_bit_vector_manager ().get_all_zeros (bit_range.get_size ());
}

void
zero_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "zero contents");
}

void
zero_contents::add_dump_widget_children (text_art::tree_widget &,
					 node_dump_state &) const
{
  // no-op
}

void
zero_contents::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_contents (*this);
}

std::unique_ptr<json::object>
zero_contents::to_json () const
{
  return make_json_obj ("zero_contents");
}

std::unique_ptr<diagnostics::markup::node>
zero_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("zero_contents");
}

string_scan
zero_contents::
scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset) const
{
  return string_scan::found_nul (start_byte_offset);
}

void
zero_contents::
get_representative_path_vars (const region_model *,
			      value_set *,
			      const region *,
			      const svalue *,
			      tree,
			      logger *,
			      auto_vec<path_var> *) const
{
  // no-op
}

const bit_contents &
zero_contents::
poison_any_pointers_to_descendents (const region *,
				    enum poison_kind) const
{
  // no-op
  return *this;
}

const bit_contents &
zero_contents::purge_state_involving (const symbolic::node &) const
{
  // no-op
  return *this;
}

// class unfoldable_bit_contents : public bit_contents

const symbolic::bit_vector &
unfoldable_bit_contents::get_bits (const symbolic_bit_range &bit_range) const
{
  auto &bv_mgr = get_bit_vector_manager ();
  if (bit_range.get_size ().is_zero_p ())
    return bv_mgr.get_empty ();

  auto iter = m_range_to_bv_map.find (bit_range);
  if (iter != m_range_to_bv_map.end ())
    return *(iter->second);

  auto bv_up
    = std::make_unique<symbolic::bit_vector_from_ubc> (*this, bit_range);
  auto &result = *bv_up;
  m_range_to_bv_map.insert ({bit_range, std::move (bv_up)});
  return result;
}

string_scan
unfoldable_bit_contents::
scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset) const
{
  return string_scan::cannot_scan (start_byte_offset);
}

bool
unfoldable_bit_contents::
bits_live_p (const symbolic::bit_vector_from_ubc &bv_from_ubc) const
{
  return &bv_from_ubc.get_bit_contents () == this;
}

/* Identify when SVAL is a cropping from this unfoldable_bit_contents,
   reporting it to OUT_PVS as the appropriate subregion of BASE_REG.  */

void
unfoldable_bit_contents::
get_representative_path_vars (const region_model *model,
			      value_set *visited,
			      const region *base_reg,
			      const svalue *sval,
			      tree desired_type,
			      logger *logger,
			      auto_vec<path_var> *out_pvs) const
{
  auto *from_bv = sval->dyn_cast_svalue_from_bit_vector ();
  if (!from_bv)
    return;

  auto &bv = from_bv->get_bit_vector ();
  auto *from_bc = bv.dyn_cast_bit_vector_from_ubc ();
  if (!from_bc)
    return;

  if (&from_bc->get_bit_contents () != this)
    return;

  symbolic::memory_range mem_range (*base_reg, from_bc->get_bit_range ());
  auto *subreg = model->maybe_get_region_for_mem_range (mem_range,
							desired_type,
							logger);
  if (!subreg)
    return;

  auto *cast_reg
    = model->get_manager ()->get_cast_region (subreg, desired_type);
  path_var pv = model->get_representative_path_var (cast_reg, visited, logger);
  if (!pv)
    return;

  out_pvs->safe_push (pv);
}

// class baseline_bit_contents : public unfoldable_bit_contents

baseline_bit_contents::
baseline_bit_contents (node_manager &node_mgr, const baseline_key &bk)
: symbolic::unfoldable_bit_contents (complexity (bk.m_base_reg), node_mgr),
  m_baseline_key (bk)
{
  gcc_assert (!m_baseline_key.region_has_meaningful_size_p (node_mgr));
}

void
baseline_bit_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "baseline_bit_contents");
}

void
baseline_bit_contents::add_dump_widget_children (text_art::tree_widget &w,
						 node_dump_state &nds) const
{
  w.add_child (m_baseline_key.m_base_reg->make_dump_widget (nds, "base_reg"));
}

void
baseline_bit_contents::accept_node_visitor (node_visitor &v) const
{
  if (v.visit_bit_contents (*this))
    m_baseline_key.m_base_reg->accept_node_visitor (v);
}

std::unique_ptr<json::object>
baseline_bit_contents::to_json () const
{
  auto obj = make_json_obj ("baseline_bit_contents");
  obj->set ("baseline_key", m_baseline_key.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
baseline_bit_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("baseline_bit_contents");
}

const bit_contents &
baseline_bit_contents::
poison_any_pointers_to_descendents (const region *reg,
				    enum poison_kind pkind) const
{
  if (reg == m_baseline_key.m_base_reg)
    {
      // TODO
    }
  return *this;
}

const bit_contents &
baseline_bit_contents::purge_state_involving (const symbolic::node &) const
{
  return *this;
}

iter_result
baseline_bit_contents::
for_each_value (iter_result (*) (const symbolic::value_node &val,
				 void *user_data),
		void *user_data) const
{
  // No-op
  return iter_result::keep_going;
}
// class conjured_bit_contents : public unfoldable_bit_contents

bool
conjured_bit_contents::key::operator< (const key &other) const
{
  if (int c = m_stmt->uid - other.m_stmt->uid)
    return c < 0;
  if (int c = node::cmp (*m_id_reg, *other.m_id_reg))
    return c < 0;
  return m_idx < other.m_idx;
}

complexity
conjured_bit_contents::key::get_complexity () const
{
  return complexity (m_id_reg);
}

conjured_bit_contents::
conjured_bit_contents (node_manager &node_mgr, const key &k)
: symbolic::unfoldable_bit_contents (k.get_complexity (), node_mgr),
  m_stmt (k.m_stmt),
  m_id_reg (k.m_id_reg),
  m_idx (k.m_idx)
{
}

void
conjured_bit_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "conjured_bit_contents (");
  pp_gimple_stmt_1 (pp, m_stmt, 0, (dump_flags_t)0);
  if (m_idx != 0)
    pp_printf (pp, ", %i", m_idx);
  pp_character (pp, ')');
}

void
conjured_bit_contents::add_dump_widget_children (text_art::tree_widget &w,
						 node_dump_state &nds) const
{
  w.add_child (m_id_reg->make_dump_widget (nds, "id_reg"));
}

void
conjured_bit_contents::accept_node_visitor (node_visitor &v) const
{
  if (v.visit_bit_contents (*this))
    m_id_reg->accept_node_visitor (v);
}

std::unique_ptr<json::object>
conjured_bit_contents::to_json () const
{
  auto obj = make_json_obj ("conjured_bit_contents");

  {
    pretty_printer pp;
    pp_format_decoder (&pp) = default_tree_printer;
    pp_gimple_stmt_1 (&pp, m_stmt, 0, (dump_flags_t)0);
    obj->set_string ("stmt", pp_formatted_text (&pp));
  }
  obj->set ("id_reg", m_id_reg->to_json ());
  obj->set_integer ("id", m_idx);

  return obj;
}

std::unique_ptr<diagnostics::markup::node>
conjured_bit_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("conjured_bit_contents");
}

const bit_contents &
conjured_bit_contents::
poison_any_pointers_to_descendents (const region *reg,
				    enum poison_kind pkind) const
{
  if (reg == m_id_reg)
    {
      // TODO
    }
  return *this;
}

const bit_contents &
conjured_bit_contents::purge_state_involving (const symbolic::node &) const
{
  return *this;
}

iter_result
conjured_bit_contents::
for_each_value (iter_result (*) (const symbolic::value_node &val,
				 void *user_data),
		void *) const
{
  // No-op
  return iter_result::keep_going;
}

#if CHECKING_P

// class named_test_bit_contents : public unfoldable_bit_contents

named_test_bit_contents::named_test_bit_contents (node_manager &node_mgr,
						  std::string name)
: unfoldable_bit_contents (complexity (1, 1), node_mgr),
  m_name (std::move (name))
{
}

void
named_test_bit_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "named_test_bit_contents: %s", m_name.c_str ());
}

void
named_test_bit_contents::
add_dump_widget_children (text_art::tree_widget &,
			  node_dump_state &) const
{
  // no-op
}

void
named_test_bit_contents::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_contents (*this);
}

std::unique_ptr<json::object>
named_test_bit_contents::to_json () const
{
  auto obj = make_json_obj ("named_test_bit_contents");
  obj->set_string ("name", m_name);
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
named_test_bit_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node (m_name);
}

const bit_contents &
named_test_bit_contents::
poison_any_pointers_to_descendents (const region *,
				    enum poison_kind) const
{
  // no-op
  return *this;
}

const bit_contents &
named_test_bit_contents::purge_state_involving (const symbolic::node &) const

{
  // no-op
  return *this;
}

iter_result
named_test_bit_contents::
for_each_value (iter_result (*) (const symbolic::value_node &val,
				 void *user_data),
		void *) const
{
  // No-op
  return iter_result::keep_going;
}

#endif /* #if CHECKING_P */

// class concrete_bit_overlay : public bit_overlay

bool
concrete_bit_overlay::key::
operator< (const concrete_bit_overlay::key &other) const
{
  if (int c = node::cmp (*m_baseline, *other.m_baseline))
      return c < 0;
  if (m_concrete_range_map.size () != other.m_concrete_range_map.size ())
    return m_concrete_range_map.size () < other.m_concrete_range_map.size ();

  auto iter_a = m_concrete_range_map.begin ();
  auto iter_b = other.m_concrete_range_map.begin ();

  while (iter_a != m_concrete_range_map.end ())
    {
      if (int c = node::cmp (iter_a->second, iter_b->second))
	return c < 0;
      if (int c = concrete_bit_range::cmp (iter_a->first, iter_b->first))
	return c < 0;
      ++iter_a;
      ++iter_b;
    }

  return false;
}

complexity
concrete_bit_overlay::key::get_complexity () const
{
  complexity child_complexities = m_baseline->get_complexity ();
  for (auto iter : m_concrete_range_map)
    child_complexities += iter.second.get_complexity ();
  return complexity::from_child_complexities (child_complexities);
}

/* Canonicalize by removing bindings where the bit_vector matches the
   baseline's bit_vector.  */

void
concrete_bit_overlay::key::remove_redundant_bindings ()
{
  for (auto iter = m_concrete_range_map.begin ();
       iter != m_concrete_range_map.end ();
       )
    {
      auto &conc_bit_range = iter->first;
      auto &new_bv = iter->second;
      auto &existing_bv = m_baseline->get_bits (conc_bit_range);
      if (&existing_bv == &new_bv)
	iter = m_concrete_range_map.erase (iter);
      else
	++iter;
    }
}

concrete_bit_overlay::concrete_bit_overlay (const key &k)
: bit_overlay (k.get_complexity (), *k.m_baseline),
  m_concrete_range_map (k.m_concrete_range_map)
{
  if (CHECKING_P)
    {
      gcc_assert (!m_concrete_range_map.empty ());

      /* Avoid nesting of overlays.  */
      gcc_assert
	(get_baseline ().get_kind () != bit_contents::kind::concrete_overlay
	 && get_baseline ().get_kind () != bit_contents::kind::symbolic_overlay);

      for (auto iter = m_concrete_range_map.begin ();
	   iter != m_concrete_range_map.end ();
	   ++iter)
	{
	  auto &conc_bit_range = iter->first;
	  auto &overlay_bv = iter->second;

	  auto next = iter;
	  ++next;

	  // Check for ordering and non-overlapping bit ranges
	  if (next != m_concrete_range_map.end ())
	    {
	      auto &next_conc_bit_range = next->first;
	      gcc_assert (conc_bit_range.get_start_offset ()
			  < next_conc_bit_range.get_start_offset ());
	      gcc_assert (conc_bit_range.get_last_offset ()
			  < next_conc_bit_range.get_start_offset ());
	    }

	  /* Must not have redundant bindings.  */
	  auto &baseline_bv = get_baseline ().get_bits (conc_bit_range);
	  gcc_assert (&overlay_bv != &baseline_bv);

	  /* The size of each bit_vector must be concrete and match that of
	     the bit range it is bound to.  */
	  concrete_bit_size conc_bv_size;
	  bool got_conc_bv_size
	    = overlay_bv.maybe_get_concrete_size_in_bits (&conc_bv_size);
	  gcc_assert (got_conc_bv_size);
	  gcc_assert (conc_bv_size == conc_bit_range.get_size ());

	  if (auto *concat_bv = overlay_bv.dyn_cast_concat_bit_vector ())
	    {
	      /* Check for no leading concrete-sized child elements.  */
	      auto first_child_bv = concat_bv->begin ();
	      gcc_assert
		(!(*first_child_bv)->get_size ().dyn_cast_concrete_scalar ());
	    }
	}
    }
}

const bit_vector &
concrete_bit_overlay::get_bits (const symbolic_bit_range &sym_bit_range) const
{
  concrete_bit_range conc_bit_range;
  if (concrete_from_symbolic (sym_bit_range, &conc_bit_range))
    return get_concrete_bits (conc_bit_range);

  // TODO: should we be returning a bit_vector_from_ubc here?
  auto &bv_mgr = get_bit_vector_manager ();
  return bv_mgr.get_unknown_bits (sym_bit_range.get_size ()); // FIXME
}

void
concrete_bit_overlay::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "concrete bit overlay");
}

void
concrete_bit_overlay::add_dump_widget_children (text_art::tree_widget &w,
						node_dump_state &nds) const
{
  using tree_widget = text_art::tree_widget;
  std::unique_ptr<tree_widget> ranges_widget
    (tree_widget::from_fmt (nds, nullptr, "concrete ranges"));
  for (auto iter : m_concrete_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;
      pretty_printer pp;
      bit_range.dump_to_pp_pretty (&pp);
      ranges_widget->add_child (bv.make_dump_widget (nds,
						     pp_formatted_text (&pp)));
    }
  w.add_child (std::move (ranges_widget));
  w.add_child (get_baseline ().make_dump_widget (nds, "baseline"));
}

void
concrete_bit_overlay::accept_node_visitor (node_visitor &v) const
{
  if (v.visit_bit_contents (*this))
    {
      get_baseline ().accept_node_visitor (v);
      for (auto iter : m_concrete_range_map)
	{
	  auto &bit_range = iter.first;
	  auto &bv = iter.second;
	  // FIXME: bit_range.accept_node_visitor (v);
	  bv.accept_node_visitor (v);
	}
    }
}

std::unique_ptr<json::object>
concrete_bit_overlay::to_json () const
{
  auto obj = make_json_obj ("concrete_bit_overlay");
  auto arr = std::make_unique<json::array> ();
  for (auto iter : m_concrete_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;
      auto element = std::make_unique<json::object> ();
      element->set ("range", bit_range.to_json ());
      element->set ("bv", bv.to_json ());
      arr->append (std::move (element));
    }
  obj->set ("concrete_range_map", std::move (arr));
  obj->set ("baseline", get_baseline ().to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
concrete_bit_overlay::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("concrete_bit_overlay");
}

string_scan
concrete_bit_overlay::
scan_for_first_zero_byte (string_scan::byte_offset_type start_scan_offset) const
{
  for (auto iter : m_concrete_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;

      concrete_byte_range byte_range;
      if (!maybe_bytes_from_bits (bit_range, &byte_range))
	return string_scan::cannot_scan (start_scan_offset);

      if (start_scan_offset < byte_range.get_start_offset ())
       {
	 /* We're looking before the range.  See it we have non-zero
	    bytes leading to the range.  */
	 string_scan scan
	   = get_baseline ().scan_for_first_zero_byte (start_scan_offset);
	 switch (scan.m_state)
	   {
	   default:
	     return scan;
	   case string_scan::state::beyond_domain:
	     /* We have non-zero bytes leading to the range.  */
	     start_scan_offset = byte_range.get_start_offset ();
	     break;
	   }
       }

      if (start_scan_offset < byte_range.get_next_offset ())
	{
	  /* Look within this bv.  */
	  auto bv_result = bv.scan_for_first_zero_byte
	    (start_scan_offset
	     - byte_range.get_start_offset ());
	  if (bv_result.m_state == string_scan::state::beyond_domain)
	    {
	      /* Try to carry on to the next range (or gap).  */
	      start_scan_offset = byte_range.get_next_offset ();
	      continue;
	    }
	  else
	    return string_scan (bv_result.m_byte_offset
				+ byte_range.get_start_offset (),
				bv_result.m_state);
	}
      else
	{
	  /* We're scannning after this overlay; ignore it.  */
	  continue;
	}
    }

  // Beyond the end of the overlay ranges; look at underlying bit contents:
  return get_baseline ().scan_for_first_zero_byte (start_scan_offset);
}

static bool
bit_vector_matches_sval_p (const symbolic::bit_vector &bv,
			   const svalue &sval)
{
  if (auto *bvs = bv.dyn_cast_bit_vector_from_svalue ())
    {
      if (&bvs->get_svalue () == &sval)
	return true;
      if (auto *sym_ptr_from_bv
	    = bvs->get_svalue ().maybe_get_symbolic_pointer ())
	if (auto *sym_ptr_from_sval = sval.maybe_get_symbolic_pointer ())
	  if (*sym_ptr_from_bv == *sym_ptr_from_sval)
	    return true;
    }
  auto &bv_mgr = bv.get_bit_vector_manager ();
  if (auto *bv_from_sval = bv_mgr.maybe_fold_bits_for_svalue (sval))
    if (&bv == bv_from_sval)
      return true;
  return false;
}

void
concrete_bit_overlay::
get_representative_path_vars (const region_model *model,
			      value_set *visited,
			      const region *base_reg,
			      const svalue *sval,
			      tree desired_type,
			      logger *logger,
			      auto_vec<path_var> *out_pvs) const
{
  /* Find if SVAL is present anywhere in m_concrete_range_map.  */
  for (auto iter : m_concrete_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;

      if (bit_vector_matches_sval_p (bv, *sval))
	{
	  /* Found SVAL at bit_range relative to BASE_REG.
	     Try to convert that to a subregion, and from there
	     to a path_var.  */
	  auto_vec<const region *> sub_regions;
	  base_reg->get_subregions_for_bit_range (model->get_manager (),
						  bit_range,
						  desired_type,
						  &sub_regions);
	  for (auto reg : sub_regions)
	    if (path_var pv
		= model->get_representative_path_var (reg,
						      visited,
						      logger))
	      append_pathvar_with_type (pv, desired_type, out_pvs);
	}
    }
}

/* Helper subroutine for poison_any_pointers_to_descendents implementations.
   Return true for descendents of BAD_ANCESTOR_REG, but not to zero-offset
   pointers (otherwise double-free detection doesn't work, since sm-state
   for "free" is stored on the original ptr svalue).  */

static bool
should_poison_sym_ptr_p (const symbolic::pointer &sym_ptr,
			 const region &bad_ancestor_reg)
{
  const region &base_reg = sym_ptr.get_base_region ();
  if (!base_reg.descendent_of_p (&bad_ancestor_reg))
    return false;
  if (&base_reg == &bad_ancestor_reg
      && sym_ptr.get_byte_offset ().is_zero_p ())
    return false;
  return true;
}

const bit_contents &
concrete_bit_overlay::
poison_any_pointers_to_descendents (const region *reg,
				    enum poison_kind pkind) const
{
  concrete_bit_overlay::key k {&get_baseline (), {}};

  for (auto iter : m_concrete_range_map)
    {
      auto &bit_range = iter.first;
      const bit_vector *bv = &iter.second;

      if (auto *bvs = bv->dyn_cast_bit_vector_from_svalue ())
	{
	  if (auto *sym_ptr = bvs->get_svalue ().maybe_get_symbolic_pointer ())
	    if (should_poison_sym_ptr_p (*sym_ptr, *reg))
	      {
		auto &bv_mgr = get_bit_vector_manager ();
		bv = &bv_mgr.get_poisoned_bits (bv->get_size (), pkind);
	      }
	}
      k.m_concrete_range_map.insert ({bit_range, *bv});
    }

  return get_bit_contents_manager ().get_concrete_bit_overlay (std::move (k),
							       nullptr);
}

const bit_contents &
concrete_bit_overlay::purge_state_involving (const symbolic::node &node) const
{
  concrete_bit_overlay::key k {&get_baseline (), {}};

  for (auto iter : m_concrete_range_map)
    {
      auto &bit_range = iter.first;
      const bit_vector *bv = &iter.second;

      if (bv->involves_p (node))
	{
	  auto &bv_mgr = get_bit_vector_manager ();
	  bv = &bv_mgr.get_unknown_bits (bv->get_size ());
	}
      k.m_concrete_range_map.insert ({bit_range, *bv});
    }

  return get_bit_contents_manager ().get_concrete_bit_overlay (std::move (k),
							       nullptr);
}

bool
concrete_bit_overlay::bits_live_p (const symbolic::bit_vector_from_ubc &) const
{
  return false;
}

iter_result
concrete_bit_overlay::
for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				   void *user_data),
		 void *user_data) const
{
  if (get_baseline ().for_each_value (cb, user_data) == iter_result::stop)
    return iter_result::stop;
  for (auto iter : m_concrete_range_map)
    {
      const bit_vector *bv = &iter.second;
      if (cb (*bv, user_data) == iter_result::stop)
	return iter_result::stop;
    }
  return iter_result::keep_going;
}

// Implementation detail of concrete_bit_overlay::get_concrete_bits

class pending_bit_vector
{
public:
  pending_bit_vector (bit_vector_manager &bv_mgr,
		      const bit_contents &baseline)
  : m_bv_mgr (bv_mgr),
    m_baseline (baseline),
    m_accum_size (0_bits)
  {}

  void
  on_concrete_mapping (const concrete_bit_range &key_range,
		       const bit_vector &bv_value,
		       const concrete_bit_range &crop_range)
  {
    concrete_bit_range overlap;
    if (key_range.intersects_p (crop_range, &overlap))
      {
	integer_with_unit<units::bit> start_offset_within_full_bv
	  = overlap.get_start_offset () - key_range.get_start_offset ();
	concrete_bit_range bits_to_use (start_offset_within_full_bv,
					overlap.get_size ());

	/* Add any necessary leading padding.  */
	integer_with_unit<units::bit> size_up_to_start_of_binding
	  = key_range.get_start_offset () - crop_range.get_start_offset ();
	append_any_gap
	  (concrete_bit_range (crop_range.get_start_offset () + m_accum_size,
			       size_up_to_start_of_binding - m_accum_size));

	auto &cropped_bv = bv_value.get_cropped_bits (bits_to_use);
	append_bit_vector (cropped_bv);
      }
  }

  void
  append_any_gap (concrete_bit_range gap_range)
  {
    if (gap_range.get_size () > 0_bits)
      append_bit_vector (m_baseline.get_bits (gap_range));
  }

  void
  add_any_trailing_gap (const concrete_bit_range &bit_range)
  {
    append_any_gap
      (concrete_bit_range (bit_range.get_start_offset () + m_accum_size,
			   bit_range.get_size () - m_accum_size));
  }

  const bit_vector &
  to_bit_vector ()
  {
    return m_bv_mgr.get_concat_bits (std::move (m_bit_vectors));
  }

private:
  void
  append_bit_vector (const symbolic::bit_vector &bv)
  {
    concrete_bit_size conc_size;
    if (!concrete_from_symbolic (bv.get_size (), &conc_size))
      gcc_unreachable ();
    m_bit_vectors.push_back (&bv);
    m_accum_size += conc_size;
  }

  bit_vector_manager &m_bv_mgr;
  const bit_contents &m_baseline;
  std::vector<const symbolic::bit_vector *> m_bit_vectors;
  concrete_bit_size m_accum_size;
};

const bit_vector &
concrete_bit_overlay::
get_concrete_bits (const concrete_bit_range &bit_range) const
{
  pending_bit_vector pending (get_bit_vector_manager (), get_baseline ());
  for (auto iter : m_concrete_range_map)
    pending.on_concrete_mapping (iter.first, iter.second, bit_range);
  pending.add_any_trailing_gap (bit_range);

  auto &result = pending.to_bit_vector ();
  gcc_assert (result.get_size ().dyn_cast_concrete_scalar ()->get_concrete ()
	      == bit_range.get_size ());
  return result;
}

/* Return true iff CONC_BIT_OFFSET is bound in one of this
   concrete_bit_overlay's ranges.  */

bool
concrete_bit_overlay::
bit_offset_is_bound_p (concrete_bit_offset conc_bit_offset) const
{
  /* The concrete range comparator sorts by offset, then by size,
     so we use a zero-sized range.  */
  auto iter = m_concrete_range_map.lower_bound
    (concrete_bit_range (conc_bit_offset, 0_bits));
  if (iter != m_concrete_range_map.end ()
      && iter->first.contains_p (conc_bit_offset))
    return true;
  if (iter == m_concrete_range_map.begin ())
    return false;
  --iter;
  return iter->first.contains_p (conc_bit_offset);
}

/* Populate a map for mapping BV to NEW_BIT_RANGE.
   Canonicalize concat_bit_vector into multiple ranges where
   possible.  */

std::map<concrete_bit_range, const bit_vector &>
concrete_bit_overlay::
canonicalize_new_bit_vec (const concrete_bit_range &new_bit_range,
			  const bit_vector &bv)
{
  if (auto *concat_bv = bv.dyn_cast_concat_bit_vector ())
    {
      /* Canonicalize by splitting out leading concrete-sized elements
	 from concat_bv as separate bindings.  */
      std::map<concrete_bit_range, const bit_vector &> split_map;
      concrete_bit_offset curr_start_offset = new_bit_range.get_start_offset ();
      for (auto iter : *concat_bv)
	{
	  concrete_bit_size conc_element_size;
	  if (iter->maybe_get_concrete_size_in_bits (&conc_element_size))
	    {
	      concrete_bit_range element_bit_range (curr_start_offset,
						    conc_element_size);
	      split_map.insert ({element_bit_range, *iter});
	      curr_start_offset += conc_element_size;
	    }
	  else
	    {
	      /* We have a symbolically-sized element; do as a single range.  */
	      return {{new_bit_range, bv}};
	    }
	}
      return split_map;
    }

  /* Do BV as a single range.  */
  return {{new_bit_range, bv}};
}

/* Generate a map from MAP where any adjacent bindings have been
   consolidated, combining the ranges and concatenating the bit_vectors.  */

std::map<concrete_bit_range, const bit_vector &>
concrete_bit_overlay::
consolidate_adjacent (std::map<concrete_bit_range, const bit_vector &> map,
		      bit_vector_manager &bv_mgr)
{
  std::vector<std::pair<concrete_bit_range, const bit_vector &>> result;
  for (auto iter : map)
    {
      if (!result.empty ())
	{
	  const concrete_bit_range &prev_range = result.back ().first;
	  const concrete_bit_range &iter_range = iter.first;
	  if (prev_range.get_next_offset () == iter_range.get_start_offset ())
	    {
	      /* They touch.  Try concatenating the vectors, to see if
		 the results simplifies.  */
	      const bit_vector &prev_bv = result.back ().second;
	      const bit_vector &iter_bv = iter.second;
	      auto &concat
		= bv_mgr.get_concat_bits ({&prev_bv, &iter_bv});
	      if (!concat.dyn_cast_concat_bit_vector ())
		{
		  /* We were able to consolidate adjacent vectors.
		     Overwrite with the consolidated range/bit_vector,
		     rather than inserting a new element.  */
		  concrete_bit_range combined_range
		    (prev_range.get_start_offset (),
		     prev_range.get_size () + iter_range.get_size ());
		  result.pop_back ();
		  result.push_back ({combined_range, concat});
		  continue;
		}
	    }
	}
      result.push_back (iter);
    }
  return std::map<concrete_bit_range, const bit_vector &> (result.begin (),
							   result.end ());
}

/* Consider:
   |  +------------------------------------+
   |  |             old binding            |
   |  +------------------------------------+
   |
   |       +----------------------+
   |       |      new binding     |
   |       +----------------------+

   This function makes a new overlay by adding the new
   binding, then "cutting a hole" out of any old bindings:
   |  +------+......................+------+
   |  |prefix| hole for new binding |suffix|
   |  +------+......................+------+
   and adding them where they are not overwritten.  */

const bit_contents &
concrete_bit_overlay::set_concrete_bits (const concrete_bit_range &new_bit_range,
					 const bit_vector &bv) const
{
  std::map<concrete_bit_range, const bit_vector &> new_concrete_range_map
    = canonicalize_new_bit_vec (new_bit_range, bv);

  for (auto iter : m_concrete_range_map)
    {
      auto &original_bit_range = iter.first;
      concrete_bit_range overlap;
      if (!new_bit_range.intersects_p (original_bit_range, &overlap))
	{
	  // No overlap
	  new_concrete_range_map.insert (iter);
	  continue;
	}

      /* Otherwise we have overlap.  There are various cases that can occur:

	 If NEW_BIT_RANGE is fully within original_bit_range:
	   |  +------------------------------------+
	   |  |          original_bit_range        |
	   |  +------------------------------------+
	   |         +----------------------+
	   |         |  new_bit_range       |
	   |         +----------------------+
	 then cut the old binding into two cropped pieces:
	   |  +------+......................+------+
	   |  |prefix| hole for new binding |suffix|
	   |  +------+......................+------+

	 If original_bit_range is fully within NEW_BIT_RANGE:
	   |       +----------------------+
	   |       |  original_bit_range  |
	   |       +----------------------+
	   |  +------------------------------------+
	   |  |         new_bit_range              |
	   |  +------------------------------------+
	 then the old binding is fully discarded

	 Otherwise, we have either this case:
	   |  +------------------+
	   |  |original_bit_range|
	   |  +------------------+
	   |         +----------------------+
	   |         |  new_bit_range       |
	   |         +----------------------+
	   |  +------+
	   |  |prefix|
	   |  +------+
	 or this case:
	   |                    +------------------+
	   |                    |original_bit_range|
	   |                    +------------------+
	   |         +----------------------+
	   |         |  new_bit_range       |
	   |         +----------------------+
	   |                                +------+
	   |                                |suffix|
	   |                                +------+

	 Handle these four cases by adding any prefix
	 and adding any suffix.  */

      /* Add any prefix.  */
      auto prefix_size = (overlap.get_start_offset ()
			 - original_bit_range.get_start_offset ());
      if (prefix_size > 0_bits)
	{
	  concrete_bit_range prefix_range
	    (original_bit_range.get_start_offset (),
	     prefix_size);
	  auto &prefix_bv = iter.second.get_prefix (prefix_size);
	  for (auto iter_prefix : canonicalize_new_bit_vec (prefix_range,
							    prefix_bv))
	    new_concrete_range_map.insert (iter_prefix);
	}

      /* Add any suffix.  */
      auto suffix_size = (original_bit_range.get_next_offset ()
			  - overlap.get_next_offset ());
      if (suffix_size > 0_bits)
	{
	  concrete_bit_range suffix_range
	    (overlap.get_next_offset (),
	     suffix_size);
	  auto &suffix_bv = iter.second.get_suffix (suffix_size);
	  for (auto iter_suffix : canonicalize_new_bit_vec (suffix_range,
							    suffix_bv))
	    new_concrete_range_map.insert (iter_suffix);
	}
    }

  // TODO: is there any other canonicalization we want to do here?
  new_concrete_range_map = consolidate_adjacent (new_concrete_range_map,
						 get_bit_vector_manager ());

  concrete_bit_overlay::key k {&get_baseline (),
			       std::move (new_concrete_range_map)};
  return get_bit_contents_manager ().get_concrete_bit_overlay (std::move (k),
							       nullptr);
}

/* Attempt to extract an svalue (with type), a special-case for handling
   interprocedural type-checking of variadic args.  */

const svalue *
concrete_bit_overlay::maybe_get_var_arg_svalue () const
{
  /* Look for an initial binding to a bit_vector.  */
  auto iter = m_concrete_range_map.begin ();
  if (iter == m_concrete_range_map.end ())
    return nullptr;
  auto &bv = iter->second;
  auto *bv_from_sval = bv.dyn_cast_bit_vector_from_svalue ();
  if (!bv_from_sval)
    return nullptr;
  return &bv_from_sval->get_svalue ();
}

static tree
maybe_get_type_for_range (tree type, const concrete_bit_range &bit_range)
{
  if (!type)
    return NULL_TREE;

  if (bit_range.get_start_offset () == 0_bits)
    {
      bit_size_t type_size;
      if (int_size_in_bits (type, &type_size))
	if (bit_range.get_size () == convert_from_bit_size_t (type_size))
	  /* We have the full size of TYPE, use it.  */
	  return type;
    }

  if (TREE_CODE (type) == RECORD_TYPE)
    {
      // TODO: maybe cache this?
      record_layout layout (type);

      if (auto *item = layout.get_item_at (bit_range.get_start_offset ()))
	{
	  const_tree field = item->m_field;
	  concrete_bit_range range_relative_to_field
	    (bit_range.get_start_offset ()
	     - item->get_start_offset (),
	     bit_range.get_size ());
	  return maybe_get_type_for_range (TREE_TYPE (field),
					   range_relative_to_field);
	}
    }

  return NULL_TREE;
}

/* Attempt to merge BIT_CONTENTS_A and BIT_CONTENTS_B.
   At least one is a concrete_bit_overlay with SHARED_BASELINE,
   otherwise they *are* SHARED_BASELINE.
   CONC_BIT_OVERLAY_{A,B} are the result of dynamic casting each of
   BIT_CONTENTS_{A,B} to concrete_bit_overlay.  */

const bit_contents *
concrete_bit_overlay::
can_merge_p (const bit_contents &bit_contents_a,
	     const bit_contents &bit_contents_b,
	     const bit_contents &shared_baseline,
	     const concrete_bit_overlay *conc_bit_overlay_a,
	     const concrete_bit_overlay *conc_bit_overlay_b,
	     tree type,
	     model_merger *merger)
{
  /* Locate all transitions.  */
  std::set<concrete_bit_offset> bit_offsets;
  if (conc_bit_overlay_a)
    for (auto iter : conc_bit_overlay_a->m_concrete_range_map)
      {
	bit_offsets.insert (iter.first.get_start_offset ());
	bit_offsets.insert (iter.first.get_next_offset ());
      }
  if (conc_bit_overlay_b)
    for (auto iter : conc_bit_overlay_b->m_concrete_range_map)
      {
	bit_offsets.insert (iter.first.get_start_offset ());
	bit_offsets.insert (iter.first.get_next_offset ());
      }

  /* Convert into a vector of ranges, expressing the ranges
     between elements in the set of transition offsets.  */
  concrete_bit_offset prev_offset;
  std::vector<concrete_bit_range> bit_ranges;
  for (auto iter = bit_offsets.begin ();
       iter != bit_offsets.end ();
       ++iter)
    {
      if (iter != bit_offsets.begin ())
	{
	  auto &next_offset = *iter;
	  auto size = next_offset - prev_offset;
	  bit_ranges.push_back (concrete_bit_range (prev_offset, size));
	}
      prev_offset = *iter;
    }

  /* Now try to merge the bit vectors at each range.  */
  concrete_bit_overlay::key k {&shared_baseline, {}};
  for (auto bit_range : bit_ranges)
    {
      tree type_for_range = maybe_get_type_for_range (type, bit_range);

      /* Subroutine: return true if the start offset of BIT_RANGE is bound
	 within CONC_BIT_OVERLAY.  */
      auto range_is_bound_p = []
	(const concrete_bit_range &bit_range,
	 const concrete_bit_overlay *conc_bit_overlay) -> bool
      {
	if (!conc_bit_overlay)
	  return false;
	return conc_bit_overlay->bit_offset_is_bound_p
	  (bit_range.get_start_offset ());
      };
      bool bound_in_a = range_is_bound_p (bit_range, conc_bit_overlay_a);
      bool bound_in_b = range_is_bound_p (bit_range, conc_bit_overlay_b);

      const bit_vector *merged_bv = nullptr;

      auto &bv_a = bit_contents_a.get_bits (bit_range);
      auto &bv_b = bit_contents_b.get_bits (bit_range);
      if (bound_in_a && bound_in_b)
	{
	  /* Bound in both.  */
	  merged_bv = bit_vector::can_merge_p (bv_a,
					       bv_b,
					       type_for_range,
					       merger);
	}
      else if (bound_in_a || bound_in_b)
	{
	  /* We only have a binding for iter in one of the concrete_range_maps,
	     i.e. an attempt to merge bound and not-bound.
	     We might be dealing with conditional initialization, or of an
	     initialization of a growing region of a buffer in a loop.  */
	  if ((bound_in_a
	       && !conc_bit_overlay_b
	       && bv_b.dyn_cast_poisoned_bit_vector ())
	      || (bound_in_b
		  && !conc_bit_overlay_a
		  && bv_a.dyn_cast_poisoned_bit_vector ()))
	    {
	      /* One is bound; the other is fully unbound and is uninitialized
		 over the range of interest.
		 Assume this is conditional initialization, and block the
		 merger.  */
	      return nullptr;
	    }

	  /* Otherwise, merge this bit range as "UNKNOWN" (assuming the other
	     bv is mergeable).  */
	  auto &bv_mgr = bit_contents_a.get_bit_vector_manager ();
	  auto &unknown_bv
	    = bv_mgr.get_unknown_bits (bit_range.get_size ());
	  auto &bound_bv = bound_in_a ? bv_a : bv_b;
	  if (bit_vector::can_merge_p (unknown_bv,
				       bound_bv,
				       type_for_range,
				       merger))
	    merged_bv = &unknown_bv;
	}
      else
	{
	  /* We have a "hole" where neither has a binding.  */
	  continue;
	}

      if (merged_bv)
	for (auto iter : canonicalize_new_bit_vec (bit_range, *merged_bv))
	  k.m_concrete_range_map.insert (iter);
      else
	return nullptr;
    }

  auto &bit_contents_mgr = shared_baseline.get_bit_contents_manager ();
  return &bit_contents_mgr.get_concrete_bit_overlay (std::move (k),
						     nullptr);
}

// class symbolic_bit_overlay : public bit_overlay

bool
symbolic_bit_overlay::key::
operator< (const symbolic_bit_overlay::key &other) const
{
  if (int c = node::cmp (*m_baseline, *other.m_baseline))
      return c < 0;
  if (m_symbolic_range_map.size () != other.m_symbolic_range_map.size ())
    return m_symbolic_range_map.size () < other.m_symbolic_range_map.size ();

  auto iter_a = m_symbolic_range_map.begin ();
  auto iter_b = other.m_symbolic_range_map.begin ();

  while (iter_a != m_symbolic_range_map.end ())
    {
      if (int c = node::cmp (iter_a->second, iter_b->second))
	return c < 0;
      if (int c = symbolic_bit_range::cmp (iter_a->first, iter_b->first))
	return c < 0;
      ++iter_a;
      ++iter_b;
    }

  return false;
}

complexity
symbolic_bit_overlay::key::get_complexity () const
{
  complexity child_complexities = m_baseline->get_complexity ();
  for (auto iter : m_symbolic_range_map)
    {
      child_complexities += iter.first.get_complexity ();
      child_complexities += iter.second.get_complexity ();
    }
  return complexity::from_child_complexities (child_complexities);
}

/* Canonicalize by removing bindings where the bit_vector matches the
   baseline's bit_vector.  */

void
symbolic_bit_overlay::key::remove_redundant_bindings ()
{
  for (auto iter = m_symbolic_range_map.begin ();
       iter != m_symbolic_range_map.end ();
       )
    {
      auto &sym_bit_range = iter->first;
      auto &new_bv = iter->second;
      auto &existing_bv = m_baseline->get_bits (sym_bit_range);
      if (&existing_bv == &new_bv)
	iter = m_symbolic_range_map.erase (iter);
      else
	++iter;
    }
}

symbolic_bit_overlay::symbolic_bit_overlay (const key &k)
: bit_overlay (k.get_complexity (), *k.m_baseline),
  m_symbolic_range_map (k.m_symbolic_range_map)
{
  if (CHECKING_P)
    {
      gcc_assert (!m_symbolic_range_map.empty ());
      gcc_assert (m_symbolic_range_map.size () == 1);

      /* Avoid nesting of overlays.  */
      gcc_assert
	(get_baseline ().get_kind () != bit_contents::kind::concrete_overlay
	 && get_baseline ().get_kind () != bit_contents::kind::symbolic_overlay);

      for (auto iter : m_symbolic_range_map)
	{
	  auto &bit_range = iter.first;
	  auto &overlay_bv = iter.second;

	  /* Start offsets are not allowed to be "unknown".  */
	  gcc_assert (!bit_range.get_start_offset ().is_unknown_p ());

	  // TODO(design question): do we want to allow unknown sizes?
#if 0
	  gcc_assert (!bit_range.get_size ().is_unknown_p ());
#endif

	  // Bit ranges in the map are not allowed to be fully concrete
	  concrete_bit_range conc_bit_range;
	  bool got_conc_bit_range
	    = concrete_from_symbolic (bit_range, &conc_bit_range);
	  gcc_assert (!got_conc_bit_range);

	  /* Must not have redundant bindings.  */
	  auto &baseline_bv = get_baseline ().get_bits (bit_range);
	  gcc_assert (&overlay_bv != &baseline_bv);

	  /* The size of each bit_vector must match that of the bit
	     range it is bound to.  */
	  gcc_assert (&overlay_bv.get_size ()
		      == &bit_range.get_size ());
	}
    }
}

const bit_vector &
symbolic_bit_overlay::get_bits (const symbolic_bit_range &sym_bit_range) const
{
  /* TODO: we currently ignore the baseline
     should we be clobbering the baseline when we create this?  */

  auto iter = m_symbolic_range_map.find (sym_bit_range);
  if (iter != m_symbolic_range_map.end ())
    return iter->second;

  /* TODO: check for crops, etc
     if no intersection, return the baseline.  */
 
  auto &bv_mgr = get_bit_vector_manager ();
  return bv_mgr.get_unknown_bits (sym_bit_range.get_size ());
}

void
symbolic_bit_overlay::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "symbolic bit overlay");
}

void
symbolic_bit_overlay::add_dump_widget_children (text_art::tree_widget &w,
						node_dump_state &nds) const
{
  using tree_widget = text_art::tree_widget;
  std::unique_ptr<tree_widget> ranges_widget
    (tree_widget::from_fmt (nds, nullptr, "symbolic ranges"));
  for (auto iter : m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;
      ranges_widget->add_child (bit_range.make_dump_widget (nds));
      ranges_widget->add_child (bv.make_dump_widget (nds));
    }
  w.add_child (std::move (ranges_widget));
  w.add_child (get_baseline ().make_dump_widget (nds, "baseline"));
}

void
symbolic_bit_overlay::accept_node_visitor (node_visitor &v) const
{
  if (v.visit_bit_contents (*this))
    {
      get_baseline ().accept_node_visitor (v);
      for (auto iter : m_symbolic_range_map)
	{
	  auto &bit_range = iter.first;
	  auto &bv = iter.second;
	  bit_range.accept_node_visitor (v);
	  bv.accept_node_visitor (v);
	}
    }
}

std::unique_ptr<json::object>
symbolic_bit_overlay::to_json () const
{
  auto obj = make_json_obj ("symbolic_bit_overlay");
  auto arr = std::make_unique<json::array> ();
  for (auto iter : m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;
      auto element = std::make_unique<json::object> ();
      element->set ("range", bit_range.to_json ());
      element->set ("bv", bv.to_json ());
      arr->append (std::move (element));
    }
  obj->set ("concrete_range_map", std::move (arr));
  obj->set ("baseline", get_baseline ().to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
symbolic_bit_overlay::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("symbolic_bit_overlay");
}

string_scan
symbolic_bit_overlay::
scan_for_first_zero_byte (string_scan::byte_offset_type start_scan_offset) const
{
  for (auto iter : m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;

      symbolic_byte_range byte_range;
      if (!maybe_bytes_from_bits (bit_range, &byte_range))
	return string_scan::cannot_scan (start_scan_offset);

      auto *concrete_start_offset
	= byte_range.get_start_offset ().dyn_cast_concrete_scalar ();
      if (!concrete_start_offset)
	return string_scan::cannot_scan (start_scan_offset);
      concrete_byte_offset start_of_range_byte_offset
	= concrete_start_offset->get_concrete ();

      if (start_scan_offset < start_of_range_byte_offset)
	{
	  /* TODO: look at underlying bit contents.  */
	  return string_scan::cannot_scan (start_scan_offset);
	}
      else if (start_scan_offset == start_of_range_byte_offset)
	{
	  /* Look within this bv.  */
	  auto bv_result = bv.scan_for_first_zero_byte (0_bytes);
	  if (bv_result.m_state == string_scan::state::beyond_domain)
	    {
	      /* Try to carry on to the next range (or gap).  */
	      auto *concrete_next_offset
		= byte_range.get_next_offset ().dyn_cast_concrete_scalar ();
	      if (concrete_next_offset)
		start_scan_offset = concrete_next_offset->get_concrete ();
	      else
		return string_scan::cannot_scan (start_scan_offset);
	      continue;
	    }
	}
      else
	/* We might be scannning within or after this overlay; give up.  */
	return string_scan::cannot_scan (start_scan_offset);
    }

  // Beyond the end of the overlay ranges; look at underlying bit contents:
  return get_baseline ().scan_for_first_zero_byte (start_scan_offset);
}

void
symbolic_bit_overlay::
get_representative_path_vars (const region_model *model,
			      value_set *visited,
			      const region *base_reg,
			      const svalue *sval,
			      tree desired_type,
			      logger *logger,
			      auto_vec<path_var> *out_pvs) const
{
  /* Find if SVAL is present anywhere in m_symbolic_range_map.  */
  for (auto iter : m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      auto &bv = iter.second;

      if (bit_vector_matches_sval_p (bv, *sval))
	{
	  /* Found SVAL at bit_range relative to BASE_REG.
	     Try to convert that to a subregion, and from there
	     to a path_var.  */
	  symbolic::memory_range mem_range (*base_reg,
					    bit_range);
	  if (auto *reg = model->maybe_get_region_for_mem_range
			    (mem_range,
			     desired_type,
			     logger))
	    if (path_var pv
		= model->get_representative_path_var (reg,
						      visited,
						      logger))
	      append_pathvar_with_type (pv, desired_type, out_pvs);
	}
    }
}

const bit_contents &
symbolic_bit_overlay::
poison_any_pointers_to_descendents (const region *reg,
				    enum poison_kind pkind) const
{
  symbolic_bit_overlay::key k {&get_baseline (), {}};

  for (auto iter : m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      const bit_vector *bv = &iter.second;

      if (auto *bvs = bv->dyn_cast_bit_vector_from_svalue ())
	{
	  if (auto *sym_ptr = bvs->get_svalue ().maybe_get_symbolic_pointer ())
	    if (should_poison_sym_ptr_p (*sym_ptr, *reg))
	      {
		auto &bv_mgr = get_bit_vector_manager ();
		bv = &bv_mgr.get_poisoned_bits (bv->get_size (), pkind);
	      }
	}
      k.m_symbolic_range_map.insert ({bit_range, *bv});
    }

  return get_bit_contents_manager ().get_symbolic_bit_overlay (std::move (k),
							       nullptr);
}

const bit_contents &
symbolic_bit_overlay::purge_state_involving (const symbolic::node &node) const
{
  symbolic_bit_overlay::key k {&get_baseline (), {}};

  for (auto iter : m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      // FIXME: also need to purge bit_range
      const bit_vector *bv = &iter.second;

      if (bv->involves_p (node))
	{
	  auto &bv_mgr = get_bit_vector_manager ();
	  bv = &bv_mgr.get_unknown_bits (bv->get_size ());
	}
      k.m_symbolic_range_map.insert ({bit_range, *bv});
    }

  return get_bit_contents_manager ().get_symbolic_bit_overlay (std::move (k),
							       nullptr);
}

bool
symbolic_bit_overlay::bits_live_p (const symbolic::bit_vector_from_ubc &) const
{
  return false;
}

iter_result
symbolic_bit_overlay::
for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				   void *user_data),
		void *user_data) const
{
  if (get_baseline ().for_each_value (cb, user_data) == iter_result::stop)
    return iter_result::stop;
  for (auto iter : m_symbolic_range_map)
    {
      const bit_vector *bv = &iter.second;
      if (cb (*bv, user_data) == iter_result::stop)
	return iter_result::stop;
    }
  return iter_result::keep_going;
}

// class offset_bit_contents : public bit_contents

bool
offset_bit_contents::key::operator< (const key &other) const
{
  if (int c = node::cmp (*m_inner_bc, *other.m_inner_bc))
    return c < 0;
  return node::cmp (*m_bit_offset, *other.m_bit_offset) < 0;
}

complexity
offset_bit_contents::key::get_complexity () const
{
  return complexity::from_pair (m_inner_bc->get_complexity (),
				m_bit_offset->get_complexity ());
}

offset_bit_contents::offset_bit_contents (node_manager &node_mgr,
					  const key &k)
: bit_contents (k.get_complexity (), node_mgr),
  m_inner_bc (*k.m_inner_bc),
  m_bit_offset (*k.m_bit_offset)
{
  gcc_assert (!m_bit_offset.is_zero_p ());
  gcc_assert (!m_inner_bc.dyn_cast_uniform_bit_contents ());

  /* No nesting.  */
  gcc_assert (!m_inner_bc.dyn_cast_offset_bit_contents ());
}

const bit_vector &
offset_bit_contents::get_bits (const symbolic_bit_range &bit_range) const
{
  symbolic_bit_range range_in_inner (bit_range.get_start_offset ()
				     + m_bit_offset,
				     bit_range.get_size ());
  return m_inner_bc.get_bits (range_in_inner);
}

void
offset_bit_contents::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "offset_bit_contents");
}

void
offset_bit_contents::add_dump_widget_children (text_art::tree_widget &w,
					       node_dump_state &nds) const
{
  w.add_child (m_inner_bc.make_dump_widget (nds, "inner_bc"));
  w.add_child (m_bit_offset.make_dump_widget (nds, "bit_offset"));
}

void
offset_bit_contents::accept_node_visitor (node_visitor &v) const
{
  if (v.visit_bit_contents (*this))
    {
      m_inner_bc.accept_node_visitor (v);
      m_bit_offset.accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
offset_bit_contents::to_json () const
{
  auto obj = make_json_obj ("offset_bit_contents");

  obj->set ("inner_bc", m_inner_bc.to_json ());
  obj->set ("bit_offset", m_bit_offset.to_json ());

  return obj;
}

std::unique_ptr<diagnostics::markup::node>
offset_bit_contents::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("offset_bit_contents");
}

string_scan
offset_bit_contents::
scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset) const
{
  concrete_bit_offset conc_bit_offset;
  if (concrete_from_symbolic (m_bit_offset, &conc_bit_offset))
    {
      concrete_byte_offset conc_byte_offset;
      if (maybe_bytes_from_bits (conc_bit_offset, &conc_byte_offset))
	{
	  string_scan inner
	    = m_inner_bc.scan_for_first_zero_byte (start_byte_offset
						   + conc_byte_offset);
	  inner.m_byte_offset -= conc_byte_offset;
	  return inner;
	}
    }

  return string_scan::cannot_scan (start_byte_offset);
}

void
offset_bit_contents::
get_representative_path_vars (const region_model *,
			      value_set *,
			      const region *,
			      const svalue *,
			      tree,
			      logger *,
			      auto_vec<path_var> *) const
{
  // no-op
}

const bit_contents &
offset_bit_contents::
poison_any_pointers_to_descendents (const region *reg,
				    enum poison_kind pkind) const
{
  auto &poisoned_inner
    = m_inner_bc.poison_any_pointers_to_descendents (reg, pkind);
  return get_bit_contents_manager ().get_offset_bit_contents (poisoned_inner,
							      m_bit_offset);
}

const bit_contents &
offset_bit_contents::purge_state_involving (const symbolic::node &n) const
{
  auto &purged_inner = m_inner_bc.purge_state_involving (n);
  return get_bit_contents_manager ().get_offset_bit_contents (purged_inner,
							      m_bit_offset);
}

bool
offset_bit_contents::
bits_live_p (const symbolic::bit_vector_from_ubc &bv_from_ubc) const
{
  return m_inner_bc.bits_live_p (bv_from_ubc);
}

iter_result
offset_bit_contents::
for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				   void *user_data),
		void *user_data) const
{
  return m_inner_bc.for_each_value (cb, user_data);
}

/* offset(offset(bc, x), y) --> offset(bc, x + y).  */

const bit_contents *
offset_bit_contents::
maybe_fold_offset (const symbolic_bit_offset &bit_offset) const
{
  return &(get_bit_contents_manager ()
	   .get_offset_bit_contents (m_inner_bc,
				     m_bit_offset + bit_offset));
}

// class bit_contents_manager

bit_contents_manager::bit_contents_manager (node_manager &node_mgr)
: m_node_mgr (node_mgr),
  m_uninit_contents (node_mgr),
  m_zero_contents (node_mgr)
{
}

const bit_contents &
bit_contents_manager::get_untracked (value_set maybe_live)
{
  value_set canonical_vs;
  for (auto iter : maybe_live)
    if (untracked_bit_contents::track_value_liveness_p (*iter))
      canonical_vs.add (iter);

  untracked_bit_contents::key k (std::move (canonical_vs));

  auto iter = m_untracked_bit_contents.find (k);
  if (iter != m_untracked_bit_contents.end ())
    return *(iter->second);

  auto p = std::make_unique<untracked_bit_contents> (m_node_mgr, k);
  auto &result = *p;
  m_untracked_bit_contents.insert ({k, std::move (p)});
  return result;
}

const bit_contents &
bit_contents_manager::set_bits (const bit_contents &original,
				const symbolic_bit_range &sym_bit_range,
				const bit_vector &bv,
				uncertainty_t *uncertainty)
{
  concrete_bit_range conc_bit_range;
  if (concrete_from_symbolic (sym_bit_range, &conc_bit_range))
    return set_bits (original, conc_bit_range, bv, uncertainty);

  std::map<symbolic_bit_range, const bit_vector &> overlays;
  overlays.insert ({sym_bit_range, bv});
  symbolic_bit_overlay::key k {&original, std::move (overlays)};
  return get_symbolic_bit_overlay (std::move (k), uncertainty);
}

const bit_contents &
bit_contents_manager::set_bits (const bit_contents &original,
				const concrete_bit_range &conc_bit_range,
				const bit_vector &bv,
				uncertainty_t *uncertainty)
{
  if (auto *conc_overlay = original.dyn_cast_concrete_bit_overlay ())
    return conc_overlay->set_concrete_bits (conc_bit_range, bv);

  std::map<concrete_bit_range, const bit_vector &> overlays
    = concrete_bit_overlay::canonicalize_new_bit_vec (conc_bit_range, bv);
  concrete_bit_overlay::key k {&original, std::move (overlays)};
  return get_concrete_bit_overlay (std::move (k), uncertainty);
}

const bit_contents &
bit_contents_manager::
get_concrete_bit_overlay (concrete_bit_overlay::key k,
			  uncertainty_t *uncertainty)
{
  gcc_assert (k.m_baseline);

  k.remove_redundant_bindings ();

  if (k.m_concrete_range_map.size () == 0)
    return *k.m_baseline;

  /* Avoid nesting of overlays.
     Any attempt to make a concrete_bit_overlay on top of another
     concrete_bit_overlay should have been flattened by now.  */
  gcc_assert (k.m_baseline->get_kind () != bit_contents::kind::concrete_overlay);
  if (k.m_baseline->get_kind () == bit_contents::kind::symbolic_overlay)
    {
      if (uncertainty)
	uncertainty->add_bit_contents (*k.m_baseline);
      value_set maybe_live = k.m_baseline->get_maybe_live_values ();
      k.m_baseline = &get_untracked (std::move (maybe_live));
    }

  k.remove_redundant_bindings ();
  if (k.m_concrete_range_map.size () == 0)
    return *k.m_baseline;

  auto iter = m_concrete_bit_overlays.find (k);
  if (iter != m_concrete_bit_overlays.end ())
    return *(iter->second);

  auto p = std::make_unique<concrete_bit_overlay> (k);
  auto &result = *p;
  m_concrete_bit_overlays.insert ({k, std::move (p)});
  return result;
}

const bit_contents &
bit_contents_manager::
get_symbolic_bit_overlay (symbolic_bit_overlay::key k,
			  uncertainty_t *uncertainty)
{
  gcc_assert (k.m_baseline);

  k.remove_redundant_bindings ();

  if (k.m_symbolic_range_map.size () == 0)
    return *k.m_baseline;

  for (auto &iter : k.m_symbolic_range_map)
    {
      auto &bit_range = iter.first;
      /* If any of the bit ranges start at an unknown offset,
	 fall back to the whole bit contents being unknown.  */
      if (bit_range.get_start_offset ().is_unknown_p ())
	{
	  if (uncertainty)
	    uncertainty->add_bit_contents (*k.m_baseline);

	  value_set maybe_live = k.m_baseline->get_maybe_live_values ();
	  for (auto &iter2 : k.m_symbolic_range_map)
	    {
	      auto &bv = iter2.second;
	      if (uncertainty)
		uncertainty->on_maybe_bound_value (bv);
	      maybe_live.add (&bv);
	    }
	  return get_untracked (std::move (maybe_live));
	}
    }

  /* Avoid nesting of overlays.  */
  if (k.m_baseline->get_kind () == bit_contents::kind::concrete_overlay
      || k.m_baseline->get_kind () == bit_contents::kind::symbolic_overlay)
    {
      if (uncertainty)
	uncertainty->add_bit_contents (*k.m_baseline);
      value_set maybe_live = k.m_baseline->get_maybe_live_values ();
      k.m_baseline = &get_untracked (std::move (maybe_live));
    }

  k.remove_redundant_bindings ();

  if (k.m_symbolic_range_map.size () == 0)
    return *k.m_baseline;

  /* Any attempt to create more than one symbolic key ought to have been
     folded into an "untracked" baseline by now.  */
  gcc_assert (k.m_symbolic_range_map.size () == 1);

  auto iter = m_symbolic_bit_overlays.find (k);
  if (iter != m_symbolic_bit_overlays.end ())
    return *(iter->second);

  auto p = std::make_unique<symbolic_bit_overlay> (k);
  auto &result = *p;
  m_symbolic_bit_overlays.insert ({k, std::move (p)});
  return result;
}

/* Create a bit_contents that is uninitialized everywhere, apart from
   bytes [0-sizeof(TREE_CST)), which are bound to TREE_CST's representation
   on the target.  */

const bit_contents &
bit_contents_manager::make_bit_contents_for_constant (tree tree_cst)
{
  auto &bv_mgr = m_node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = m_node_mgr.get_scalars ().get_bits ();
  auto &bv = bv_mgr.get_bits_for_constant (tree_cst);

  return (get_uninitialized ()
	  .set_bits (symbolic_bit_range (bit_mgr.get_concrete (0_bits),
					 bv.get_size ()),
		     bv,
		     nullptr));
}

const bit_contents &
bit_contents_manager::get_bit_contents_for_constant (tree tree_cst)
{
  auto iter = m_bit_contents_for_constants.find (tree_cst);
  if (iter != m_bit_contents_for_constants.end ())
    return iter->second;

  auto &bc = make_bit_contents_for_constant (tree_cst);
  m_bit_contents_for_constants.insert ({tree_cst, bc});
  return bc;
}

bool
baseline_key::
region_has_meaningful_size_p (symbolic::node_manager &node_mgr) const
{
  if (m_base_reg->get_kind () == RK_SYMBOLIC)
    {
      /* We don't know the size of the underlying region:
	 "T *ptr" could be just one T, but could be an array
	 of T, or within an array of T.  */
      return false;
    }

  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  if (m_base_reg->maybe_get_bit_size (bit_mgr))
    return true;

  return false;
}

std::unique_ptr<json::object>
baseline_key::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("base_reg", m_base_reg->to_json ());
  obj->set_string ("baseline_state", baseline_state_to_str (m_baseline_state));
  return obj;
}

const bit_contents &
bit_contents_manager::make_baseline_contents (baseline_key k)
{
  if (!k.m_base_reg->can_have_initial_svalue_p ())
    return get_uninitialized ();

  auto &rmm = m_node_mgr.get_svalue_manager ();
  switch (k.m_baseline_state)
    {
    default:
      gcc_unreachable ();

    case baseline_state::main:
      {
	if (auto *decl_reg = k.m_base_reg->dyn_cast_decl_region ())
	  {
	    /* Attempt to get the initializer value for base_reg as a
	       bit_contents.  */
	    if (auto *bc = decl_reg->maybe_get_bit_contents_for_initializer (&rmm))
	      return *bc;
	  }
      }
      break;

    case baseline_state::after_main:
      break;
    }

  if (k.region_has_meaningful_size_p (m_node_mgr))
    {
      /* Get or create a concrete_bit_overlay, with a baseline bit_vector
	 at [0, size), and uninitialized everywhere else.  */
      auto &bv_mgr = m_node_mgr.get_bit_vector_manager ();
      auto &bit_mgr = m_node_mgr.get_scalars ().get_bits ();
      auto &baseline_bv
	= bv_mgr.get_baseline_bit_vector (*k.m_base_reg,
					  k.m_baseline_state);
      return (get_uninitialized ()
	      .set_bits (symbolic_bit_range (bit_mgr.get_concrete (0_bits),
					     baseline_bv.get_size ()),
			 baseline_bv,
			 nullptr));
    }
  else
    {
      /* Make a baseline_bit_contents.  */
      auto bc
	= std::make_unique<baseline_bit_contents> (m_node_mgr, k);
      auto &result = *bc;
      m_unfoldable_bit_contents.push_back (std::move (bc));
      return result;
    }
}

const bit_contents &
bit_contents_manager::
get_baseline_contents (const region &base_reg,
		       baseline_state bs)
{
  /* Read-only var decls don't change,
     so always use baseline_state::main for them.  */
  if (tree decl = base_reg.maybe_get_decl ())
    if (VAR_P (decl) && TREE_READONLY (decl))
      bs = baseline_state::main;

  baseline_key k {&base_reg, bs};
  auto iter = m_baseline_contents.find (k);
  if (iter != m_baseline_contents.end ())
    return iter->second;

  auto &bc = make_baseline_contents (k);
  m_baseline_contents.insert ({k, bc});
  return bc;
}

const bit_contents &
bit_contents_manager::
get_conjured_bit_contents (const region &base_reg,
			   const gimple *stmt,
			   const conjured_purge &p,
			   unsigned idx)
{
  const symbolic::conjured_bit_contents::key k (stmt, &base_reg, idx);
  auto iter = m_conjured_map.find (k);
  if (iter != m_conjured_map.end ())
    {
      auto *bc = iter->second.get ();
      /* We're reusing an existing conjured_bit_contents, perhaps from a
	 different state within this analysis, or perhaps from an earlier state
	 on this execution path.  For the latter, purge any state involving the
	 "new" bit_contents from the current program_state.  */
      p.purge (*bc);
      return *bc;
    }
  auto bc = std::make_unique<symbolic::conjured_bit_contents> (m_node_mgr, k);
  auto &result = *bc.get ();
  m_conjured_map.insert ({k, std::move (bc)});
  return result;
}

#if CHECKING_P
const bit_contents &
bit_contents_manager::
get_named_test_bit_contents (std::string name)
{
  auto obj_up = std::make_unique<named_test_bit_contents> (m_node_mgr,
							   std::move (name));
  auto &result = *obj_up.get ();
  m_unfoldable_bit_contents.push_back (std::move (obj_up));
  return result;
}
#endif

const bit_contents &
bit_contents_manager::
get_offset_bit_contents (const bit_contents &inner_bc,
			 const symbolic_bit_offset &bit_offset)
{
  /* An offset of zero can always be ignored.  */
  if (bit_offset.is_zero_p ())
    return inner_bc;

  if (auto *folded = inner_bc.maybe_fold_offset (bit_offset))
    return *folded;

  const symbolic::offset_bit_contents::key k {&inner_bc, &bit_offset};
  auto iter = m_offset_map.find (k);
  if (iter != m_offset_map.end ())
    return *(iter->second);

  auto bc = std::make_unique<symbolic::offset_bit_contents> (m_node_mgr, k);
  auto &result = *bc.get ();
  m_offset_map.insert ({k, std::move (bc)});
  return result;
}

} // namespace ana::symbolic

#if CHECKING_P

namespace selftest {

static void
test_uninit ()
{
  symbolic::node_manager mgr;
  auto &bit_cont_mgr = mgr.get_bit_contents_manager ();
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &uninit_bc = bit_cont_mgr.get_uninitialized ();
  ASSERT_SAME_SYMBOLIC_NODE
    (uninit_bc.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
     bv_mgr.get_uninitialized_bytes (10_bytes));
}

static void
test_untracked ()
{
  symbolic::node_manager mgr;
  auto &bit_cont_mgr = mgr.get_bit_contents_manager ();
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &untracked_bc = bit_cont_mgr.get_untracked ({});
  ASSERT_SAME_SYMBOLIC_NODE
    (untracked_bc.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
     bv_mgr.get_unknown_bytes (10_bytes));

  ASSERT_SAME_SYMBOLIC_NODE
    (untracked_bc,
     bit_cont_mgr.get_untracked ({}));

  auto &custom_byte_a = bv_mgr.get_named_test_bit_vector (8_bits,
							  "custom_byte_a");
  auto &custom_byte_b = bv_mgr.get_named_test_bit_vector (8_bits,
							  "custom_byte_b");

  value_set maybe_live_a;
  maybe_live_a.add (&custom_byte_a);

  value_set maybe_live_b;
  maybe_live_b.add (&custom_byte_b);

  ASSERT_NE (maybe_live_a, maybe_live_b);

  value_set maybe_live_ab;
  maybe_live_ab.add (&custom_byte_a);
  maybe_live_ab.add (&custom_byte_b);

  value_set maybe_live_ba;
  maybe_live_ba.add (&custom_byte_b);
  maybe_live_ba.add (&custom_byte_a);

  // Insertion order shouldn't matter
  ASSERT_EQ (maybe_live_ab, maybe_live_ba);

  ASSERT_DIFFERENT_SYMBOLIC_NODE
    (untracked_bc,
     bit_cont_mgr.get_untracked (maybe_live_a));

  ASSERT_SAME_SYMBOLIC_NODE
    (bit_cont_mgr.get_untracked (maybe_live_a),
     bit_cont_mgr.get_untracked (maybe_live_a));

  ASSERT_DIFFERENT_SYMBOLIC_NODE
    (bit_cont_mgr.get_untracked (maybe_live_a),
     bit_cont_mgr.get_untracked (maybe_live_b));

  ASSERT_SAME_SYMBOLIC_NODE
    (bit_cont_mgr.get_untracked (maybe_live_ab),
     bit_cont_mgr.get_untracked (maybe_live_ba));
}

static void
test_all_zeros ()
{
  symbolic::node_manager mgr;
  auto &bit_cont_mgr = mgr.get_bit_contents_manager ();
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &all_zeros_bc = bit_cont_mgr.get_all_zeros ();
  ASSERT_SAME_SYMBOLIC_NODE
    (all_zeros_bc.get_bytes (concrete_byte_range (0_bytes, 1024_bytes)),
     bv_mgr.get_all_zeros (1024_bytes));
}

/* Simulation of "char buf[10];"  */

static void
test_static_sized_char_array ()
{
  symbolic::node_manager mgr;
  auto &bit_cont_mgr = mgr.get_bit_contents_manager ();
  auto &bv_mgr = mgr.get_bit_vector_manager ();

  const symbolic::bit_contents *zeroed_buf = &bit_cont_mgr.get_all_zeros ();
  const symbolic::bit_contents *uninit_buf = &bit_cont_mgr.get_uninitialized ();

  {
    auto &bv = zeroed_buf->get_bytes (concrete_byte_range (0_bytes, 10_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bv,
			       bv_mgr.get_all_zeros (10_bytes));
  }

  {
    auto &bv = uninit_buf->get_bytes (concrete_byte_range (0_bytes, 10_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bv,
			       bv_mgr.get_uninitialized_bytes (10_bytes));
  }

  auto set_abc0 = [] (const auto *buf)
  {
    buf = &buf->set_byte (0_bytes, 'a', nullptr);
    buf = &buf->set_byte (1_byte, 'b', nullptr);
    buf = &buf->set_byte (2_bytes, 'c', nullptr);
    buf = &buf->set_byte (3_bytes, '\0', nullptr);
    return buf;
  };

  zeroed_buf = set_abc0 (zeroed_buf);
  uninit_buf = set_abc0 (uninit_buf);

  zeroed_buf->to_json ();
  uninit_buf->to_json ();

  {
    auto &bv = zeroed_buf->get_bytes (concrete_byte_range (0_bytes, 10_bytes));
    ASSERT_SAME_SYMBOLIC_NODE
      (bv,
       bv_mgr.get_concat_bits
       ({&bv_mgr.get_uint8 ('a'),
	 &bv_mgr.get_uint8 ('b'),
	 &bv_mgr.get_uint8 ('c'),
	 &bv_mgr.get_all_zeros (7_bytes)}));
  }
  {
    auto &bv = uninit_buf->get_bytes (concrete_byte_range (0_bytes, 10_bytes));
    ASSERT_SAME_SYMBOLIC_NODE
      (bv,
       bv_mgr.get_concat_bits
       ({&bv_mgr.get_uint8 ('a'),
	 &bv_mgr.get_uint8 ('b'),
	 &bv_mgr.get_uint8 ('c'),
	 &bv_mgr.get_uint8 (0),
	 &bv_mgr.get_uninitialized_bytes (6_bytes)}));
  }

  /* Verify scanning for zero bytes.  */
  {
    /* For byte offsets [0, 3), both should find the NUL at offset 3.  */
    for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
      {
	ASSERT_EQ (zeroed_buf->scan_for_first_zero_byte (i),
		   string_scan::found_nul (3_bytes));
	ASSERT_EQ (uninit_buf->scan_for_first_zero_byte (i),
		   string_scan::found_nul (3_bytes));
      }

    /* Verify byte offsets outside that range (both negative and positive).
       For zeroed_buf, we should find NUL at every offset.
       For uninit_buf, we should report a problem at every offset.  */
    {
      /* Negative byte offsets.  */
      for (concrete_byte_offset i = -10_bytes; i < 0_bytes; ++i)
	{
	  ASSERT_EQ (zeroed_buf->scan_for_first_zero_byte (i),
		     string_scan::found_nul (i));
	  ASSERT_EQ (uninit_buf->scan_for_first_zero_byte (i),
		     string_scan::found_poison (i, poison_kind::uninit));
	}
      for (concrete_byte_offset i = 4_bytes; i < 10_bytes; ++i)
	{
	  ASSERT_EQ (zeroed_buf->scan_for_first_zero_byte (i),
		     string_scan::found_nul (i));
	  ASSERT_EQ (uninit_buf->scan_for_first_zero_byte (i),
		     string_scan::found_poison (i, poison_kind::uninit));
	}
    }
  }
}

/* Construct a non-trivial bit_contents, then sample it as a bit_vector
   at various ranges, including negative offsets.  */

static void
test_reading_nontrivial_bit_map ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  // Check reading from the baseline contents:
  ASSERT_SAME_SYMBOLIC_NODE
    (unbound.get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     unbound.get_bytes (concrete_byte_range (0_bytes, 100_bytes)));

  /* Set up this binding:
     |    [-4]: 'x'
     |    [-3]: 'y'
     |    [-2]: 'z'
     |    [-1]: unbound
     |[0 - 10): zero-filled  (10 bytes)
     | [10-16): unbound
     | [16-32): 0xff-filled  (16 bytes)
     |    [32): unbound
     |    [33]: 'a'
     |    [34): unbound
     |    [35]: 'b'
     |    [36): unbound
     |    [37]: 'c'
     |    [38]: 'c'
  */
  const symbolic::bit_contents *bmap = &unbound;
  bmap = &bmap->set_byte (-4_bytes, 'x', nullptr);
  bmap = &bmap->set_byte (-3_bytes, 'y', nullptr);
  bmap = &bmap->set_byte (-2_bytes, 'z', nullptr);
  bmap = &bmap->set_bytes (concrete_byte_range (0_bytes, 10_bytes),
			   bv_mgr.get_all_zeros (10_bytes),
			   nullptr);
  bmap = &bmap->set_bytes (concrete_byte_range(16_bytes, 16_bytes),
			   bv_mgr.get_all_ones (16_bytes),
			   nullptr);
  bmap = &bmap->set_byte (33_bytes, 'a', nullptr);
  bmap = &bmap->set_byte (35_bytes, 'b', nullptr);
  bmap = &bmap->set_byte (37_bytes, 'c', nullptr);
  bmap = &bmap->set_byte (38_bytes, 'c', nullptr);

  // We should have a single concrete overlay over "unbound"
  auto *conc_bit_overlay = bmap->dyn_cast_concrete_bit_overlay ();
  ASSERT_NE (conc_bit_overlay, nullptr);
  ASSERT_SAME_SYMBOLIC_NODE (conc_bit_overlay->get_baseline (),
			     unbound);

  bmap->to_json ();

  // Test individual bytes
  {
    /* Subroutines to determine if the initial/final bit of the byte at
       the given offset is bound in conc_bit_overlay.  */
    auto initial_bit_offset_is_bound_p
      = [conc_bit_overlay] (concrete_byte_offset byte_offset) -> bool
    {
      auto initial_bit_offset = bits_from_bytes (byte_offset);
      return conc_bit_overlay->bit_offset_is_bound_p (initial_bit_offset);
    };
    auto final_bit_offset_is_bound_p
      = [conc_bit_overlay] (concrete_byte_offset byte_offset) -> bool
    {
      auto initial_bit_offset = bits_from_bytes (byte_offset);
      return conc_bit_overlay->bit_offset_is_bound_p
	(initial_bit_offset + 7_bits);
    };

    // A long way before the first binding:
    ASSERT_FALSE (initial_bit_offset_is_bound_p (-100_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-100_bytes),
			       unbound.get_byte (-100_bytes));

    // Immediately before the first binding:
    ASSERT_FALSE (initial_bit_offset_is_bound_p (-5_bytes));
    ASSERT_FALSE (final_bit_offset_is_bound_p (-5_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-5_bytes),
			       unbound.get_byte (-5_bytes));

    // The xyz bindings:
    ASSERT_TRUE (initial_bit_offset_is_bound_p (-4_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-4_bytes),
			       bv_mgr.get_uint8 ('x'));

    ASSERT_TRUE (initial_bit_offset_is_bound_p (-3_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-3_bytes),
			       bv_mgr.get_uint8 ('y'));

    ASSERT_TRUE (initial_bit_offset_is_bound_p (-2_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-2_bytes),
			       bv_mgr.get_uint8 ('z'));

    // Immediately after the xyz bindings:
    ASSERT_FALSE (initial_bit_offset_is_bound_p (-1_byte));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-1_byte),
			       unbound.get_byte (-1_byte));

    for (auto i = 0_bytes; i < 10_bytes; ++i)
      {
	ASSERT_TRUE (initial_bit_offset_is_bound_p (i));
	ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (i),
				   bv_mgr.get_uint8 (0));
      }
    for (auto i = 10_bytes; i < 16_bytes; ++i)
      {
	ASSERT_FALSE (initial_bit_offset_is_bound_p (i));
	ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (i),
				   unbound.get_byte (i));
      }
    for (auto i = 16_bytes; i < 32_bytes; ++i)
      {
	ASSERT_TRUE (initial_bit_offset_is_bound_p (i));
	ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (i),
				   bv_mgr.get_uint8 (0xff));
      }

    ASSERT_FALSE (initial_bit_offset_is_bound_p (32_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (32_bytes),
			       unbound.get_byte (32_bytes));

    ASSERT_TRUE (initial_bit_offset_is_bound_p (33_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (33_bytes),
			       bv_mgr.get_uint8 ('a'));

    ASSERT_FALSE (initial_bit_offset_is_bound_p (34_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (34_bytes),
			       unbound.get_byte (34_bytes));

    ASSERT_TRUE (initial_bit_offset_is_bound_p (35_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (35_bytes),
			       bv_mgr.get_uint8 ('b'));

    ASSERT_FALSE (initial_bit_offset_is_bound_p (36_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (36_bytes),
			       unbound.get_byte (36_bytes));

    ASSERT_TRUE (initial_bit_offset_is_bound_p (37_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (37_bytes),
			       bv_mgr.get_uint8 ('c'));

    ASSERT_TRUE (initial_bit_offset_is_bound_p (38_bytes));
    ASSERT_TRUE (final_bit_offset_is_bound_p (38_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (38_bytes),
			       bv_mgr.get_uint8 ('c'));

    // Immediately after the last binding:
    ASSERT_FALSE (initial_bit_offset_is_bound_p (39_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (39_bytes),
			       unbound.get_byte (39_bytes));

    // A long way after the last binding:
    ASSERT_FALSE (initial_bit_offset_is_bound_p (100_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (100_bytes),
			       unbound.get_byte (100_bytes));
  }

  // Test ranges of bytes
  {
    /* Before the first binding.  */
    ASSERT_SAME_SYMBOLIC_NODE
      (bmap->get_bytes (concrete_byte_range (-100_bytes, 10_bytes)),
       unbound.get_bytes (concrete_byte_range (-100_bytes, 10_bytes)));

    /* Partial read of the zero bytes.  */
    ASSERT_SAME_SYMBOLIC_NODE
      (bmap->get_bytes (concrete_byte_range (5_bytes, 2_bytes)),
       bv_mgr.get_all_zeros (2_bytes));

    /* Test that the two neighboring 'c' chars are consolidated
       when read as a bit vector.  */
    ASSERT_SAME_SYMBOLIC_NODE
      (bmap->get_bytes (concrete_byte_range (37_bytes, 2_bytes)),
       bv_mgr.get_repeated_bits (bv_mgr.get_uint8 ('c'), 2));

    /* After the last binding.  */
    ASSERT_SAME_SYMBOLIC_NODE
      (bmap->get_bytes (concrete_byte_range (100_bytes, 10_bytes)),
       unbound.get_bytes (concrete_byte_range (100_bytes, 10_bytes)));
  }

  /* Verify scanning for NUL bytes.  */
  {
    // A long way before the first binding:
    ASSERT_EQ (bmap->scan_for_first_zero_byte (-100_bytes),
	       string_scan::cannot_scan (-100_bytes));

    // Immediately before the first binding:
    ASSERT_EQ (bmap->scan_for_first_zero_byte (-5_bytes),
	       string_scan::cannot_scan (-5_bytes));

    // Scanning the xyz bindings should fail, at the "unbound" successor.
    for (auto i = -4_bytes; i < -1_byte; ++i)
      ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		 string_scan::cannot_scan (-1_byte));

    // Immediately after the xyz bindings:
    ASSERT_EQ (bmap->scan_for_first_zero_byte (-1_byte),
	       string_scan::cannot_scan (-1_byte));

    for (auto i = 0_bytes; i < 10_bytes; ++i)
      ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		 string_scan::found_nul (i));
    for (auto i = 10_bytes; i < 16_bytes; ++i)
      ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		 string_scan::cannot_scan (i));

    /* Scanning after that point should hit an unbound byte
       and thus be unable to scan there.  */

    // Scans that should hit the unbound byte 32:
    for (auto i = 16_bytes; i < 32_bytes; ++i)
      ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		 string_scan::cannot_scan (32_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (32_bytes),
	       string_scan::cannot_scan (32_bytes));

    // Scans that should hit the unbound byte 34:
    ASSERT_EQ (bmap->scan_for_first_zero_byte (33_bytes),
	       string_scan::cannot_scan (34_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (34_bytes),
	       string_scan::cannot_scan (34_bytes));

    // Scans that should hit the unbound byte 36:
    ASSERT_EQ (bmap->scan_for_first_zero_byte (35_bytes),
	       string_scan::cannot_scan (36_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (36_bytes),
	       string_scan::cannot_scan (36_bytes));

    // Scans that should hit the unbound bytes 39 onwards:
    ASSERT_EQ (bmap->scan_for_first_zero_byte (37_bytes),
	       string_scan::cannot_scan (39_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (38_bytes),
	       string_scan::cannot_scan (39_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (39_bytes),
	       string_scan::cannot_scan (39_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (40_bytes),
	       string_scan::cannot_scan (40_bytes));
    ASSERT_EQ (bmap->scan_for_first_zero_byte (100_bytes),
	       string_scan::cannot_scan (100_bytes));
  }
}

/* Test of two successive updates to a bitmap which don't overlap,
   with a gap.  */

static void
test_update_no_overlap_with_gap ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Fill [20, 35) with a 15-byte custom bit vector.  */
  auto &first = bv_mgr.get_named_test_bit_vector (15_bytes, "first");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 15_bytes),
			   first, nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 15_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (35_bytes, 65_bytes))}));

  /* Fill [40, 90) with a second 50-byte custom vector.
     They don't overlap, so this should give us:
      [20, 35) : first
      [40, 90) : second.  */
  auto &second = bv_mgr.get_named_test_bit_vector (50_bytes, "second");
  bmap = &bmap->set_bytes (concrete_byte_range (40_bytes, 50_bytes),
			   second,
			   nullptr);
  bmap->to_json ();

  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 15_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (40_bytes, 50_bytes)),
     second);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (35_bytes, 5_bytes)),
       &second,
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));
}

/* Test of two successive updates to a bitmap which don't overlap,
   but do touch.  */

static void
test_update_no_overlap_touching ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Starts as unbound.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     unbound.get_bytes (concrete_byte_range (0_bytes, 100_bytes)));

  /* Fill [20, 35) with a 15-byte custom bit vector.  */
  auto &first = bv_mgr.get_named_test_bit_vector (15_bytes, "first");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 15_bytes),
			   first,
			   nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 15_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (35_bytes, 65_bytes))}));

  /* Fill [35, 75) with with a second custom vector (40 bytes).
     They don't overlap, so this should give us:
      [20, 35) : first
      [35, 75) : second.  */
  auto &second = bv_mgr.get_named_test_bit_vector (40_bytes, "second");
  bmap = &bmap->set_bytes (concrete_byte_range (35_bytes, 40_bytes),
			   second,
			   nullptr);
  bmap->to_json ();
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 15_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (35_bytes, 40_bytes)),
     second);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &second,
       &unbound.get_bytes (concrete_byte_range (75_bytes, 25_bytes))}));
}

/* Consider:
   |  +------------------------------------+
   |  |             old binding            |
   |  +------------------------------------+
   then writing to:
   |         +----------------------+
   |         |      new binding     |
   |         +----------------------+

   Verify that we "punch a hole" and fill it with the new binding:
   |  +------+----------------------+------+
   |  |prefix|      new binding     |suffix|
   |  +------+----------------------+------+
*/

static void
test_update_punching_a_hole ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Fill [20, 90) with a 70-byte custom bit vector.  */
  auto &first = bv_mgr.get_named_test_bit_vector (70_bytes, "first");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 70_bytes),
			   first,
			   nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));

  /* Fill [25, 75) with a second bit vector (50 byte), punching a hole
     in "first"
     This should give us:
      [20, 25) : first 5 bytes of first
      [25, 75) : "second"
      [75, 90) : last 15 bytes of first.  */
  auto &second = bv_mgr.get_named_test_bit_vector (50_bytes, "second");
  bmap = &bmap->set_bytes (concrete_byte_range (25_bytes, 50_bytes),
			   second,
			   nullptr);
  // Test what's now in the range that "first" was stored to:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     bv_mgr.get_concat_bits
     ({&first.get_prefix (5_bytes),
       &second,
       &first.get_suffix (15_bytes)}));
  // Test what's in the range that "second" was stored to:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (25_bytes, 50_bytes)),
     second);
  // Test everything:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first.get_prefix (5_bytes),
       &second,
       &first.get_suffix (15_bytes),
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));
}

/* Verify the effect of a pair of overwrites to the same range.  */

static void
test_update_full_overwrite ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Fill [20, 90) with a 70-byte custom bit vector.  */
  auto &first = bv_mgr.get_named_test_bit_vector (70_bytes, "first");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 70_bytes),
			   first,
			   nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));

  /* Overwrite it with a different 70-byte custom bit vector.  */
  auto &second = bv_mgr.get_named_test_bit_vector (70_bytes, "second");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 70_bytes),
			   second,
			   nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     second);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &second,
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));
}

/* Verify the correct handling of this pair of writes:
   |  +-----------------------+
   |  |first                  |
   |  +-----------------------+
   |         +----------------------+
   |         |  second              |
   |         +----------------------+

   We should end up with:
   |  +------+----------------------+
   |  |prefix|  second              |
   |  +------+----------------------+
*/

static void
test_update_overlap_with_prefix ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Fill [20, 90) with a 70-byte custom bit vector.  */
  auto &first = bv_mgr.get_named_test_bit_vector (70_bytes, "first");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 70_bytes),
			   first,
			   nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));

  /* Fill [30, 95) with a second bvec.
     This should give us:
      [20, 30) : first 10 bytes of first
      [30, 95): second.  */
  auto &second = bv_mgr.get_named_test_bit_vector (65_bytes, "second");
  bmap = &bmap->set_bytes (concrete_byte_range (30_bytes, 65_bytes),
			   second,
			   nullptr);
  // Test what's now in the range that "first" was stored to:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     bv_mgr.get_concat_bits
     ({&first.get_prefix (10_bytes),
       &second.get_prefix (60_bytes)}));
  // Test what's in the range that "second" was stored to:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (30_bytes, 65_bytes)),
     second);
  // Test everything:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first.get_prefix (10_bytes),
       &second,
       &unbound.get_bytes (concrete_byte_range (95_bytes, 5_bytes))}));
}

/* Verify the correct handling of this pair of writes:
   |         +-----------------------+
   |         |first                  |
   |         +-----------------------+
   |  +----------------------+
   |  |  second              |
   |  +----------------------+

   We should end up with:
   |  +----------------------+------+
   |  |  second              |suffix|
   |  +----------------------+------+
*/

static void
test_update_overlap_with_suffix ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Fill [20, 90) with a 70-byte custom bit vector.  */
  auto &first = bv_mgr.get_named_test_bit_vector (70_bytes, "first");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 70_bytes),
			   first,
			   nullptr);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     first);
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       &first,
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));

  /* Fill [5, 70) with a second bvec.
     This should give us:
      [5, 70) : second.
      [70, 90): last 20 bytes of first.  */
  auto &second = bv_mgr.get_named_test_bit_vector (65_bytes, "second");
  bmap = &bmap->set_bytes (concrete_byte_range (5_bytes, 65_bytes),
			   second,
			   nullptr);

  // Test what's now in the range that "first" was stored to:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (20_bytes, 70_bytes)),
     bv_mgr.get_concat_bits
     ({&second.get_suffix (50_bytes),
       &first.get_suffix (20_bytes)}));
  // Test what's in the range that "second" was stored to:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (5_bytes, 65_bytes)),
     second);
  // Test everything:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 5_bytes)),
       &second,
       &first.get_suffix (20_bytes),
       &unbound.get_bytes (concrete_byte_range (90_bytes, 10_bytes))}));
}

/* Verify the correct handling of this bit_map:
   |   +-------+   +-------+   +-------+   +-------+   +-------+
   |   |   A   |   |   B   |   |   C   |   |   D   |   |   E   |
   |   +-------+   +-------+   +-------+   +-------+   +-------+
   when overwritten with:
   |                   +----------------------+
   |                   |          F           |
   |                   +----------------------+

   We should end up with:
   |   +-------+   +---+----------------------+----+   +-------+
   |   |   A   |   | B'|          F           | D' |   |   E   |
   |   +-------+   +---+----------------------+----+   +-------+
*/

static void
test_update_multiple_overlaps ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  /* Populate it with:
     [10,15): A
     [20,30): B
     [40,60): C
     [70,80): D
     [90,95): E.  */
  auto &a = bv_mgr.get_named_test_bit_vector (5_bytes, "A");
  bmap = &bmap->set_bytes (concrete_byte_range (10_bytes, 5_bytes),
			   a, nullptr);
  auto &b = bv_mgr.get_named_test_bit_vector (10_bytes, "B");
  bmap = &bmap->set_bytes (concrete_byte_range (20_bytes, 10_bytes),
			   b, nullptr);
  auto &c = bv_mgr.get_named_test_bit_vector (20_bytes, "C");
  bmap = &bmap->set_bytes (concrete_byte_range (40_bytes, 20_bytes),
			   c, nullptr);
  auto &d = bv_mgr.get_named_test_bit_vector (10_bytes, "D");
  bmap = &bmap->set_bytes (concrete_byte_range (70_bytes, 10_bytes),
			   d, nullptr);
  auto &e = bv_mgr.get_named_test_bit_vector (5_bytes, "E");
  bmap = &bmap->set_bytes (concrete_byte_range (90_bytes, 5_bytes),
			   e, nullptr);

  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
       &a,
       &unbound.get_bytes (concrete_byte_range (15_bytes, 5_bytes)),
       &b,
       &unbound.get_bytes (concrete_byte_range (30_bytes, 10_bytes)),
       &c,
       &unbound.get_bytes (concrete_byte_range (60_bytes, 10_bytes)),
       &d,
       &unbound.get_bytes (concrete_byte_range (80_bytes, 10_bytes)),
       &e,
       &unbound.get_bytes (concrete_byte_range (95_bytes, 5_bytes))}));

  /* Overwrite [25, 75) with F.  */
  auto &f = bv_mgr.get_named_test_bit_vector (50_bytes, "F");
  bmap = &bmap->set_bytes (concrete_byte_range (25_bytes, 50_bytes),
			   f, nullptr);

  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (0_bytes, 100_bytes)),
     bv_mgr.get_concat_bits
     ({&unbound.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
       &a,
       &unbound.get_bytes (concrete_byte_range (15_bytes, 5_bytes)),
       &b.get_prefix (5_bytes),
       &f,
       &d.get_suffix (5_bytes),
       &unbound.get_bytes (concrete_byte_range (80_bytes, 10_bytes)),
       &e,
       &unbound.get_bytes (concrete_byte_range (95_bytes, 5_bytes))}));
}

static void
test_uint32 ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();
  auto &rmm = mgr.get_region_model_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  bmap = &bmap->set_bytes (concrete_byte_range (0_bytes, 4_bytes),
			   bv_mgr.get_all_zeros (32_bits),
			   nullptr);

  auto &zero_byte = bv_mgr.get_uint8 (0);

  ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (-1_byte),
			     unbound.get_byte (-1_byte));

  ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (0_bytes),
			     zero_byte);
  ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (1_byte),
			     zero_byte);
  ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (2_bytes),
			     zero_byte);
  ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (3_bytes),
			     zero_byte);

  ASSERT_SAME_SYMBOLIC_NODE (bmap->get_byte (4_bytes),
			     unbound.get_byte (4_bytes));

  // Read the zero, with a gap at both start and end:
  ASSERT_SAME_SYMBOLIC_NODE
    (bmap->get_bytes (concrete_byte_range (-2_bytes, 8_bytes)),
     bv_mgr.get_concat_bits
       ({&unbound.get_bytes (concrete_byte_range (-2_bytes, 2_bytes)),
	 &bv_mgr.get_all_zeros (4_bytes),
	 &unbound.get_bytes (concrete_byte_range (4_bytes, 2_bytes))}));

  // Read the zero bytes as an uint32_t svalue.
  {
    auto &bvec = bmap->get_bytes (concrete_byte_range (0_bytes, 4_bytes));
    auto &sval = *rmm.get_svalue_from_bit_vector (uint32_type_node, bvec);
    ASSERT_SAME_SYMBOLIC_NODE
      (sval,
       *rmm.get_or_create_int_cst (uint32_type_node, 0));
  }

  // Store (uint32_t)42, and try reading it back.
  {
    auto &int_42 = *rmm.get_or_create_int_cst (uint32_type_node, 42);
    bmap = &bmap->set_bytes (concrete_byte_range (0_bytes, 4_bytes),
			     bv_mgr.get_bit_vector_from_svalue (int_42),
			     nullptr);

    auto &bvec = bmap->get_bytes (concrete_byte_range (0_bytes, 4_bytes));
    auto &sval = *rmm.get_svalue_from_bit_vector (uint32_type_node, bvec);
    ASSERT_SAME_SYMBOLIC_NODE (sval, int_42);
  }
}

static void
assert_byte_eq (const ::selftest::location &loc,
		const symbolic::bit_contents *bmap,
		concrete_byte_offset byte_offset,
		uint8_t expected_value,
		symbolic::node_manager &mgr)
{
  auto &rmm = mgr.get_region_model_manager ();
  auto &actual_bvec = bmap->get_byte (byte_offset);
  auto &actual_sval
    = *rmm.get_svalue_from_bit_vector (unsigned_char_type_node, actual_bvec);
  auto &expected_sval
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, expected_value);
  ASSERT_SAME_SYMBOLIC_NODE_AT (loc, actual_sval, expected_sval);
}

#define ASSERT_BYTE_EQ(BMAP, BYTE_OFFSET, EXPECTED_VALUE, NODE_MGR) \
  SELFTEST_BEGIN_STMT						    \
  assert_byte_eq (SELFTEST_LOCATION, (BMAP), (BYTE_OFFSET),	    \
		  (EXPECTED_VALUE), (NODE_MGR));		    \
  SELFTEST_END_STMT

/* Consider these 12 bytes:

   struct foo {
     uint32_t x;
     uint32_t y;
     char str[]
   } f = {0x00000042, 0x12345678, "abc"};

   where we thus have:

     uint32_t x : 0x00000042
     uint32_t y : 0x12345678
     char str[] : "abc\0"

   +------------------------------+--------------------------------------------+
   | Field:                       | Bytes:                                     |
   |                              +--------+-----------------+-----------------+
   |                              |        |    Big-endian   |  Little-endian  |
   +------+----------+------------+--------+-----+-----------|-----+-----------+
   | name |   type   |    value   | offset | hex |   binary  | hex |   binary  |
   +------+----------+------------+--------+-----+-----------|-----+-----------+
   |    x | uint32_t | 0x00000042 |   [0]  |  00 | 0000 0000 |  42 | 0100 0010 |
   |      |          |            |   [1]  |  00 | 0000 0000 |  00 | 0000 0000 |
   |      |          |            |   [2]  |  00 | 0000 0000 |  00 | 0000 0000 |
   |      |          |            |   [3]  |  42 | 0100 0010 |  00 | 0000 0000 |
   +------+----------+------------+--------+-----+-----------|-----+-----------+
   |    y | uint32_t | 0x12345678 |   [4]  |  12 | 0001 0010 |  78 | 0111 1000 |
   |      |          |            |   [5]  |  34 | 0011 0100 |  56 | 0101 0110 |
   |      |          |            |   [6]  |  56 | 0101 0110 |  34 | 0011 0100 |
   |      |          |            |   [7]  |  78 | 0111 1000 |  12 | 0001 0010 |
   +------+----------+------------+--------+-----+-----------+-----+-----------+
   |  str |   char[] |   "abc\0"  |   [8]  |  61 | 0110 0001 |  61 | 0110 0001 |
   |      |          |            |   [9]  |  62 | 0110 0010 |  62 | 0110 0010 |
   |      |          |            |  [10]  |  63 | 0110 0011 |  63 | 0110 0011 |
   |      |          |            |  [11]  |  00 | 0000 0000 |  00 | 0000 0000 |
   +------+----------+------------+--------+-----+-----------+-----+-----------+

   Verify that we can store/retrieve these values, and then try type-punning
   it all to bytes as per the target's endianness.  */

static void
test_endianness ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();
  auto &rmm = mgr.get_region_model_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  const symbolic::bit_contents *bmap = &unbound;

  // Populate bmap as above.
  auto &int_0x42 = *rmm.get_or_create_int_cst (uint32_type_node, 0x42);
  bmap = &bmap->set_bytes
    (concrete_byte_range (0_bytes, 4_bytes),
     bv_mgr.get_bit_vector_from_svalue (int_0x42),
     nullptr);
  auto &int_0x12345678
    = *rmm.get_or_create_int_cst (uint32_type_node, 0x12345678);
  bmap = &bmap->set_bytes
    (concrete_byte_range (4_bytes, 4_bytes),
     bv_mgr.get_bit_vector_from_svalue (int_0x12345678),
     nullptr);
  bmap = &bmap->set_byte (8_bytes, 'a', nullptr);
  bmap = &bmap->set_byte (9_bytes, 'b', nullptr);
  bmap = &bmap->set_byte (10_bytes, 'c', nullptr);
  bmap = &bmap->set_byte (11_bytes, 0, nullptr);

  // Try reading the values back, using the types we used when setting them
  {
    {
      auto &bvec = bmap->get_bytes (concrete_byte_range (0_bytes, 4_bytes));
      auto &sval = *rmm.get_svalue_from_bit_vector (uint32_type_node, bvec);
      ASSERT_SAME_SYMBOLIC_NODE (sval, int_0x42);
    }
    {
      auto &bvec = bmap->get_bytes (concrete_byte_range (4_bytes, 4_bytes));
      auto &sval = *rmm.get_svalue_from_bit_vector (uint32_type_node, bvec);
      ASSERT_SAME_SYMBOLIC_NODE (sval, int_0x12345678);
    }
    ASSERT_BYTE_EQ (bmap, 8_bytes, 'a', mgr);
    ASSERT_BYTE_EQ (bmap, 9_bytes, 'b', mgr);
    ASSERT_BYTE_EQ (bmap, 10_bytes, 'c', mgr);
    ASSERT_BYTE_EQ (bmap, 11_bytes, 0, mgr);
  }

  // Try type-punning the uint32_t fields to bytes
  {
    if (BYTES_BIG_ENDIAN)
      {
	/* Big-endian.  */
	ASSERT_BYTE_EQ (bmap, 0_bytes, 0x00, mgr);
	ASSERT_BYTE_EQ (bmap, 1_byte, 0x00, mgr);
	ASSERT_BYTE_EQ (bmap, 2_bytes, 0x00, mgr);
	ASSERT_BYTE_EQ (bmap, 3_bytes, 0x42, mgr);

	ASSERT_BYTE_EQ (bmap, 4_bytes, 0x12, mgr);
	ASSERT_BYTE_EQ (bmap, 5_bytes, 0x34, mgr);
	ASSERT_BYTE_EQ (bmap, 6_bytes, 0x56, mgr);
	ASSERT_BYTE_EQ (bmap, 7_bytes, 0x78, mgr);
      }
    else
      {
	/* Little-endian.  */
	if (!WORDS_BIG_ENDIAN)
	  {
	    ASSERT_BYTE_EQ (bmap, 0_bytes, 0x42, mgr);
	    ASSERT_BYTE_EQ (bmap, 1_byte, 0x00, mgr);
	    ASSERT_BYTE_EQ (bmap, 2_bytes, 0x00, mgr);
	    ASSERT_BYTE_EQ (bmap, 3_bytes, 0x00, mgr);

	    ASSERT_BYTE_EQ (bmap, 4_bytes, 0x78, mgr);
	    ASSERT_BYTE_EQ (bmap, 5_bytes, 0x56, mgr);
	    ASSERT_BYTE_EQ (bmap, 6_bytes, 0x34, mgr);
	    ASSERT_BYTE_EQ (bmap, 7_bytes, 0x12, mgr);
	  }
      }
  }

  // Try scanning for zero bytes
  if (BYTES_BIG_ENDIAN)
    {
      /* Big-endian.  */
      /* First 3 bytes are all zero.  */
      for (auto i = 0_bytes; i < 3_bytes; ++i)
	ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		   string_scan::found_nul (i));
      /* Then non-zero until byte 11.  */
      for (auto i = 4_bytes; i < 11_bytes; ++i)
	ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		   string_scan::found_nul (11_bytes));
    }
  else
    {
      /* Little-endian.  */
      if (!WORDS_BIG_ENDIAN)
	{
	  /* First byte is non-zero (42).  */
	  ASSERT_EQ (bmap->scan_for_first_zero_byte (0_bytes),
		     string_scan::found_nul (1_byte));
	  /* Then 3 zero bytes.  */
	  for (auto i = 1_byte; i < 4_bytes; ++i)
	    ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		       string_scan::found_nul (i));
	  /* Then non-zero until byte 11.  */
	  for (auto i = 4_bytes; i < 11_bytes; ++i)
	    ASSERT_EQ (bmap->scan_for_first_zero_byte (i),
		       string_scan::found_nul (11_bytes));
	}
    }
}

/* Consider:
   |  struct foo {
   |    uint32_t x;
   |    uint32_t y;
   |  } f;
   |  f.x = 10;
   |  f.y = 42;
   Verify that we canonicalize this as two adjacent bindings of
   bit_vectors, rather than one binding to a concat bit_vector.  */

static void
test_canonicalize_concat ()
{
  symbolic::node_manager mgr;
  auto &rmm = mgr.get_region_model_manager ();
  auto &int_10 = *rmm.get_or_create_int_cst (uint32_type_node, 10);
  auto &int_42 = *rmm.get_or_create_int_cst (uint32_type_node, 42);

  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bv_10 = bv_mgr.get_bit_vector_from_svalue (int_10);
  auto &bv_42 = bv_mgr.get_bit_vector_from_svalue (int_42);

  auto &bc_mgr = mgr.get_bit_contents_manager ();
  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  /* Approach A: create it as two 4-byte bindings.  */
  const symbolic::bit_contents *bmap_a = &unbound;
  bmap_a = &bmap_a->set_bytes (concrete_byte_range (0_bytes, 4_bytes),
			       bv_10, nullptr);
  bmap_a = &bmap_a->set_bytes (concrete_byte_range(4_bytes, 4_bytes),
			       bv_42, nullptr);

  /* Approach B: create it as one 8-byte binding.  */
  const symbolic::bit_contents *bmap_b = &unbound;
  auto &concat_bv
    = bv_mgr.get_concat_bits ({&bv_10, &bv_42});
  bmap_b = &bmap_b->set_bytes (concrete_byte_range (0_bytes, 8_bytes),
			       concat_bv, nullptr);

  /* Verify that they are canonicalized to the same symbol.  */
  ASSERT_SAME_SYMBOLIC_NODE (*bmap_a, *bmap_b);

  /* Verify that internally we used approach A.  */
  ASSERT_EQ (bmap_a->dyn_cast_concrete_bit_overlay ()->size (), 2);
}

/* Consider a map of (10 bytes of zeros) then (5 bytes of zeros)
   as opposed to a map of (15 bytes of zeros).  */

static void
test_consolidation ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");

  /* Approach A: create it as two adjacent bindings.  */
  const symbolic::bit_contents *bmap_a = &unbound;
  bmap_a = &bmap_a->set_bytes (concrete_byte_range (0_bytes, 10_bytes),
			       bv_mgr.get_all_zeros (10_bytes),
			       nullptr);
  bmap_a = &bmap_a->set_bytes (concrete_byte_range(10_bytes, 5_bytes),
			       bv_mgr.get_all_zeros (5_bytes),
			       nullptr);

  /* Approach B: create it as one binding.  */
  const symbolic::bit_contents *bmap_b = &unbound;
  bmap_b = &bmap_b->set_bytes (concrete_byte_range (0_bytes, 15_bytes),
			       bv_mgr.get_all_zeros (15_bytes),
			       nullptr);

  /* Verify that they are canonicalized to the same symbolic node.  */
  ASSERT_SAME_SYMBOLIC_NODE (*bmap_a, *bmap_b);

  /* Verify that internally we used approach B.  */
  ASSERT_EQ (bmap_a->dyn_cast_concrete_bit_overlay ()->size (), 1);
}

/* Verify that we canonicalize redundant bindings of bit ranges
   when building concrete and symbolic overlays.  */

static void
test_redundant_bindings ()
{
  symbolic::node_manager mgr;
  auto &bit_contents_mgr = mgr.get_bit_contents_manager ();
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &byte_mgr = mgr.get_scalars ().get_bytes ();

  auto &zero_everywhere = bit_contents_mgr.get_all_zeros ();
  auto &untracked_bc = bit_contents_mgr.get_untracked ({});

  const symbolic_byte_size &one_byte
    = symbolic_from_concrete (1_byte, byte_mgr);

  auto &zero_byte = bv_mgr.get_uint8 (0);
  auto &custom_byte_a = bv_mgr.get_named_test_bit_vector (8_bits,
							  "custom_byte_a");
  auto &custom_byte_b = bv_mgr.get_named_test_bit_vector (8_bits,
							  "custom_byte_b");
  auto &unknown_byte = bv_mgr.get_unknown_bytes (1_byte);

  auto &kilobyte_of_zeros
    = untracked_bc.set_bytes (concrete_byte_range (0_bytes, 1024_bytes),
			      bv_mgr.get_all_zeros (1024_bytes),
			      nullptr);

  auto &offset_x = byte_mgr.get_named_test_scalar ("x");
  auto &offset_y = byte_mgr.get_named_test_scalar ("y");
  auto one_byte_at_offset_x = symbolic_byte_range (offset_x, one_byte);
  auto one_byte_at_offset_y = symbolic_byte_range (offset_y, one_byte);

  /* Concrete bit ranges.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (untracked_bc.set_bytes (concrete_byte_range (0_bytes, 1_byte),
			     unknown_byte,
			     nullptr),
     untracked_bc);
  ASSERT_SAME_SYMBOLIC_NODE
    (zero_everywhere.set_bytes (concrete_byte_range (0_bytes, 1_byte),
				zero_byte,
				nullptr),
     zero_everywhere);

  /* 1024 zero bytes; redundantly writing a zero into it at offset 16 ought to
     canonicalized to the same representation.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (kilobyte_of_zeros.set_bytes (concrete_byte_range (16_bytes, 1_byte),
				  zero_byte,
				  nullptr),
     kilobyte_of_zeros);
  /* Writing a redundant value matching the baseline should split
     the binding.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (kilobyte_of_zeros.set_bytes (concrete_byte_range (512_bytes, 1_byte),
				  unknown_byte,
				  nullptr),
     untracked_bc
     .set_bytes (concrete_byte_range (0_bytes, 512_bytes),
		 bv_mgr.get_all_zeros (512_bytes),
		 nullptr)
     .set_bytes (concrete_byte_range (513_bytes, 511_bytes),
		 bv_mgr.get_all_zeros (511_bytes),
		 nullptr));
  /* Writes to outside the bound range that match the baseline also ought
     to be canonicalized away.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (kilobyte_of_zeros.set_bytes (concrete_byte_range (4096_bytes, 1_byte),
				  unknown_byte,
				  nullptr),
     kilobyte_of_zeros);

  /* Symbolic start offsets.  */

  /* Unknown start offsets.  */
  auto one_byte_at_unknown_offset
    = symbolic_byte_range (byte_mgr.get_unknown (), one_byte);
  ASSERT_SAME_SYMBOLIC_NODE
    (untracked_bc.set_bytes (one_byte_at_unknown_offset,
			     unknown_byte,
			     nullptr),
     untracked_bc);
  ASSERT_SAME_SYMBOLIC_NODE
    (zero_everywhere.set_bytes (one_byte_at_unknown_offset,
				zero_byte,
				nullptr),
     zero_everywhere);

  /* Symbolic write on top of symbolic binding on top of "untracked".  */
  {
    auto &zero_at_x = untracked_bc.set_bytes (one_byte_at_offset_x,
					      zero_byte,
					      nullptr);
    auto &result = zero_at_x.set_bytes (one_byte_at_offset_y, custom_byte_a,
					nullptr);
    /* We should have the last bound bit_vec, on top of
       "untracked-with-A-maybe-live".  */
    value_set maybe_live;
    maybe_live.add (&zero_byte);
    ASSERT_SAME_SYMBOLIC_NODE
      (result,
       bit_contents_mgr.get_untracked (maybe_live)
       .set_bytes (one_byte_at_offset_y,
		   custom_byte_a,
		   nullptr));
  }

  /* Symbolic write on top of symbolic binding on top of "zero".  */
  {
    auto &custom_at_x = zero_everywhere.set_bytes (one_byte_at_offset_x,
						   custom_byte_a,
						   nullptr);
    auto &result = custom_at_x.set_bytes (one_byte_at_offset_y, custom_byte_b,
					  nullptr);
    /* We should have the last bound bit_vec, on top of
       "untracked, with custom_byte_a maybe-live".  */
    value_set maybe_live;
    maybe_live.add (&custom_byte_a);
    ASSERT_SAME_SYMBOLIC_NODE
      (result,
       bit_contents_mgr.get_untracked (maybe_live)
       .set_bytes (one_byte_at_offset_y,
		   custom_byte_b,
		   nullptr));
  }

  /* Concrete write on top of symbolic binding.  */
  {
    auto &custom_at_x = zero_everywhere.set_bytes (one_byte_at_offset_x,
						   custom_byte_a,
						   nullptr);
    auto &result = custom_at_x.set_bytes (concrete_byte_range (0_bytes, 1_byte),
					  custom_byte_b,
					  nullptr);
    /* We should have the last bound bit_vec, on top of
       "untracked, with custom_byte_a maybe-live".  */
    value_set maybe_live;
    maybe_live.add (&custom_byte_a);
    ASSERT_SAME_SYMBOLIC_NODE
      (result,
       bit_contents_mgr.get_untracked (maybe_live)
       .set_bytes (concrete_byte_range (0_bytes, 1_byte),
		   custom_byte_b,
		   nullptr));
  }
}

static void
test_offset_bit_contents ()
{
  symbolic::node_manager node_mgr;
  auto &bit_contents_mgr = node_mgr.get_bit_contents_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  symbolic::named_test_bit_contents placeholder_bc
    (node_mgr, "placeholder_bc");

  auto &x_bits = bit_mgr.get_named_test_scalar ("x");
  auto &y_bits = bit_mgr.get_named_test_scalar ("y");

  /* Verify that we fold away zero offsets.  */
  {
    auto &zero_offset = bit_mgr.get_concrete (0_bits);
    ASSERT_SAME_SYMBOLIC_NODE
      (placeholder_bc,
       bit_contents_mgr.get_offset_bit_contents
	 (placeholder_bc,
	  zero_offset));
  }

  /* Verify that we fold away offsets of the various uniform_bit_contents
     subclasses.  */
  {
    ASSERT_SAME_SYMBOLIC_NODE
      (bit_contents_mgr.get_untracked ({}),
       bit_contents_mgr.get_offset_bit_contents
       (bit_contents_mgr.get_untracked ({}),
	x_bits));
    ASSERT_SAME_SYMBOLIC_NODE
      (bit_contents_mgr.get_uninitialized (),
       bit_contents_mgr.get_offset_bit_contents
       (bit_contents_mgr.get_uninitialized (),
	x_bits));
    ASSERT_SAME_SYMBOLIC_NODE
      (bit_contents_mgr.get_all_zeros (),
       bit_contents_mgr.get_offset_bit_contents
       (bit_contents_mgr.get_all_zeros (),
	x_bits));
  }

  /* Verify the not-folded case. */
  auto &offset_bc = bit_contents_mgr.get_offset_bit_contents (placeholder_bc,
							      x_bits);
  {
    auto *as_offset_bc = offset_bc.dyn_cast_offset_bit_contents ();
    ASSERT_NE (as_offset_bc, nullptr);
    ASSERT_SAME_SYMBOLIC_NODE (as_offset_bc->get_inner_bit_contents (),
			placeholder_bc);
    ASSERT_SAME_SYMBOLIC_NODE (as_offset_bc->get_bit_offset (),
			x_bits);

    ASSERT_SAME_SYMBOLIC_NODE
      (offset_bc.get_byte (10_bytes),
       placeholder_bc.get_bits
       (symbolic_bit_range (bit_mgr.get_concrete (bits_from_bytes (10_bytes))
			    + x_bits,
			    bit_mgr.get_concrete (8_bits))));
  }

  /* Verify that offset-of-offset folds to sum of offsets. */
  ASSERT_SAME_SYMBOLIC_NODE
    (bit_contents_mgr.get_offset_bit_contents (offset_bc,
					       y_bits),
     bit_contents_mgr.get_offset_bit_contents (placeholder_bc,
					       x_bits + y_bits));
}

/* Consider:
   | struct str {
   |   uint32_t m_len;
   |   char m_buf[];
   | } *s;
   |
   | s->m_len = n;
   | memcpy (s->m_buf, src, n);
   | s->m_buf[n] = '\0';  */

static void
test_string_with_trailing_array ()
{
  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &bc_mgr = node_mgr.get_bit_contents_manager ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();

  auto &unbound = bc_mgr.get_named_test_bit_contents ("unbound");
  const symbolic::bit_contents *bmap = &unbound;

  placeholder_svalue n_sval
    (node_mgr.alloc_node_id (), uint32_type_node, "n");
  auto &n_bytes = byte_mgr.get_scalar_from_svalue (n_sval);
  auto &src_bv = bv_mgr.get_named_test_bit_vector (n_bytes, "src");

  /* "s->m_len = n;" */
  bmap = &bmap->set_bytes (concrete_byte_range (0_bytes, 4_bytes),
			   bv_mgr.get_bit_vector_from_svalue (n_sval),
			   nullptr);

  /* "memcpy (s->m_buf, src, n);" */
  bmap = &bmap->set_bytes (symbolic_byte_range (byte_mgr.get_concrete (4_bytes),
						n_bytes),
			   src_bv,
			   nullptr);

  /* "s->m_buf[n] = '\0';"  */
  bmap = &bmap->set_bytes (symbolic_byte_range (byte_mgr.get_concrete (4_bytes)
						+ n_bytes,
						byte_mgr.get_concrete (1_byte)),
			   bv_mgr.get_uint8 (0),
			   nullptr);
}


/* Run all of the selftests within this file.  */

void
analyzer_symbolic_bit_contents_cc_tests ()
{
  test_uninit ();
  test_untracked ();
  test_all_zeros ();
  // rule of 5 ?
  test_static_sized_char_array ();
  test_reading_nontrivial_bit_map ();
  test_update_no_overlap_with_gap ();
  test_update_no_overlap_touching ();
  test_update_punching_a_hole ();
  test_update_full_overwrite ();
  test_update_overlap_with_prefix ();
  test_update_overlap_with_suffix ();
  test_update_multiple_overlaps ();
  test_uint32 ();
  test_endianness ();
  test_canonicalize_concat ();
  test_consolidation ();
  test_redundant_bindings ();
  test_offset_bit_contents ();
  test_string_with_trailing_array ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
