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

#ifndef GCC_ANALYZER_SYMBOLIC_BIT_MAP_H
#define GCC_ANALYZER_SYMBOLIC_BIT_MAP_H

#include "analyzer/concrete-ranges.h"
#include "analyzer/symbolic-ranges.h"
#include "analyzer/string-scan.h"
#include "analyzer/symbolic-bit-vectors.h"

namespace ana {

namespace symbolic {

/* Abstract base class for representing a total function from
   symbolic bit ranges to symbolic bit vectors i.e. the contents
   of a region of memory.

   e.g. - "what is the value of bytes [8..16)?"
   - "what is the value of bit[100000]?"
   - "what is the value of byte[x]?" (for symbolic x)

   It's permissable to get/set things at negative offsets e.g. for the
   case of negative offsets from a symbolic pointer.

   Any range checking happens at a higher level.

   This is purely-functional: setters return a reference to a bit_contents
   instance holding the update.

   The class hierarchy looks like this (using indentation to show
   inheritance):

   symbolic::bit_contents (abstract)
     symbolic::uniform_bit_contents (abstract): something that ignores the input range
       symbolic::uninit_contents (concrete singleton): uninitialized everywhere
       symbolic::untracked_bit_contents (concrete): unknown everywhere, has a set of possibly-live values
       symbolic::zero_contents (concrete singleton): zero everywhere
     symbolic::unfoldable_bit_contents (abstract): unfoldable; hands out bit_vector_from_ubc instances
       symbolic::baseline_bit_contents (concrete): the initial contents of regions without known sizes
       symbolic::conjured_bit_contents (concrete): a bit_contents arising from a stmt
       symbolic::named_test_bit_contents (concrete): for use in selftests
     symbolic::bit_overlay (abstract): a baseline overridden at some places
       symbolic::concrete_bit_overlay (concrete)
       symbolic::symbolic_bit_overlay (concrete)
     symbolic::offset_bit_contents (concrete): offsets the coordinates of another bit_contents
*/

class bit_contents : public symbolic::node
{
public:
  enum class kind
  {
    uninit,
    untracked,
    zero,
    baseline,
    conjured,
    named_test,
    concrete_overlay,
    symbolic_overlay,
    offset
  };

  virtual enum kind get_kind () const = 0;

  node_manager &get_node_mgr () const { return m_node_mgr; }

  bit_contents_manager &
  get_bit_contents_manager () const
  {
    return m_node_mgr.get_bit_contents_manager ();
  }

  bit_vector_manager &
  get_bit_vector_manager () const
  {
    return m_node_mgr.get_bit_vector_manager ();
  }

  virtual const uniform_bit_contents *
  dyn_cast_uniform_bit_contents () const { return nullptr; }

  virtual const baseline_bit_contents *
  dyn_cast_baseline_bit_contents () const { return nullptr; }

  virtual const conjured_bit_contents *
  dyn_cast_conjured_bit_contents () const { return nullptr; }

  virtual const concrete_bit_overlay *
  dyn_cast_concrete_bit_overlay () const { return nullptr; }

  virtual const symbolic_bit_overlay *
  dyn_cast_symbolic_bit_overlay () const { return nullptr; }

  virtual const offset_bit_contents *
  dyn_cast_offset_bit_contents () const { return nullptr; }

  const char *
  get_any_dump_color () const override { return "bit_contents"; }

  // Getters

  virtual const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const = 0;

  const bit_vector &
  get_bits (const concrete_bit_range &bit_range) const;

  const bit_vector &
  get_bytes (const concrete_byte_range &byte_range) const;

  const bit_vector &
  get_bytes (const symbolic_byte_range &byte_range) const;

  const bit_vector &
  get_byte (integer_with_unit<units::byte> byte_offset) const
  {
    return get_bytes (concrete_byte_range (byte_offset, 1_byte));
  }

  // Setters

  const bit_contents &
  set_bits (const symbolic_bit_range &bit_range,
	    const bit_vector &bv,
	    uncertainty_t *uncertainty) const;

  const bit_contents &
  set_bits (const concrete_bit_range &bit_range,
	    const bit_vector &bv,
	    uncertainty_t *uncertainty) const;

  const bit_contents &
  set_bytes (const symbolic_byte_range &byte_range,
	     const bit_vector &bv,
	     uncertainty_t *uncertainty) const;

  const bit_contents &
  set_bytes (const concrete_byte_range &byte_range,
	     const bit_vector &bv,
	     uncertainty_t *uncertainty) const;

  const bit_contents &
  set_byte (integer_with_unit<units::byte> byte_offset,
	    uint8_t value,
	    uncertainty_t *uncertainty) const;

  virtual string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const = 0;

  virtual void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs) const = 0;

  virtual const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind) const = 0;

  virtual const bit_contents &
  purge_state_involving (const symbolic::node &node) const = 0;

  virtual bool
  bits_live_p (const bit_vector_from_ubc &bv_from_ubc) const = 0;


  /* vfunc for use by bit_contents_manager::get_offset_bit_contents.  */
  virtual const bit_contents *
  maybe_fold_offset (const symbolic_bit_offset &) const
  {
    return nullptr;
  }

  static const bit_contents *
  can_merge_p (const bit_contents &bit_contents_a,
	       const bit_contents &bit_contents_b,
	       tree type,
	       model_merger *merger);

  virtual bool
  conjured_p () const { return false; }

  virtual iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const = 0;

  value_set
  get_maybe_live_values () const;

protected:
  bit_contents (complexity c, node_manager &node_mgr)
  : node (c, node_mgr.alloc_node_id ()),
    m_node_mgr (node_mgr)
  {
  }

private:
  node_manager &m_node_mgr;
};

/* Abstract subclass of bit_contents for those classes that have
   the same bit content everywhere, and thus ignore the input bit_range.  */

class uniform_bit_contents : public bit_contents
{
public:
  const uniform_bit_contents *
  dyn_cast_uniform_bit_contents () const final override { return this; }

  const bit_contents *
  maybe_fold_offset (const symbolic_bit_offset &bit_offset)
    const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const override;

protected:
  uniform_bit_contents (complexity c, node_manager &node_mgr)
  : bit_contents (c, node_mgr)
  {
  }
};

/* Concrete subclass that's always uninitialized.  */

class uninit_contents : public uniform_bit_contents
{
public:
  uninit_contents (node_manager &node_mgr);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::uninit;
  }

  const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const final override;

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &) const final override
  {
    return false;
  }
};

/* Concrete subclass where the bits are unknown everywhere,
   with a set of possibly-live values.

   Consolidation/canonicalization:
   - every value stored in the set must have true for
     untracked_bit_contents::track_value_liveness_p (val); other
     value nodes are redundant to liveness and are never added
     to untracked_bit_contents::m_maybe_live.  */

class untracked_bit_contents : public uniform_bit_contents
{
public:
  struct key
  {
    key (value_set &&maybe_live) : m_maybe_live (maybe_live) {}

    value_set m_maybe_live;

    bool
    operator< (const key &other) const;

    complexity
    get_complexity () const;
  };

  untracked_bit_contents (node_manager &node_mgr, const key &k);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::untracked;
  }

  const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const final override;

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &) const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  const value_set &
  get_maybe_live () const { return m_maybe_live; }

  static bool
  track_value_liveness_p (const symbolic::value_node &val);

  static bool
  track_bit_vector_liveness_p (const symbolic::bit_vector &bv);

  static bool
  track_svalue_liveness_p (const svalue &val);

private:
  value_set m_maybe_live;
};

/* Concrete subclass that's a zero bit everywhere.  */

class zero_contents : public uniform_bit_contents
{
public:
  zero_contents (node_manager &node_mgr);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::zero;
  }

  const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const final override;

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &) const final override
  {
    return false;
  }
};

/* Abstract bit_contents subclass that can't be folded.
   Hands out bit_vector_from_ubc instances when its bits
   are queried.  */

class unfoldable_bit_contents : public bit_contents
{
public:
  const symbolic::bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &bv_from_ubc) const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

protected:
  unfoldable_bit_contents (complexity c, node_manager &node_mgr)
  : bit_contents (c, node_mgr)
  {}

private:
  using map_type = std::map<symbolic_bit_range,
			    std::unique_ptr<bit_vector_from_ubc>>;
  mutable map_type m_range_to_bv_map;
};

/* A bit_contents representing the initial content of a base region that
   doesn't have a meaningful size.

   Consolidation/canonicalization:
   - the base region must not have meaningful size (otherwise we can build
     an concrete_bit_overlay for its initial contents using initial_svalue).  */

class baseline_bit_contents : public unfoldable_bit_contents
{
public:
  baseline_bit_contents (node_manager &node_mgr, const baseline_key &k);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::baseline;
  }

  const baseline_bit_contents *
  dyn_cast_baseline_bit_contents () const final override { return this; }

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  const baseline_key &
  get_baseline_key () const { return m_baseline_key; }

private:
  baseline_key m_baseline_key;
};

/* A defined bit_contents arising from a statement.

   For example, this is used for the values of regions of unknown size
   that may have been touched when calling an unknown function.

   The bit_contents captures a region as well as a stmt in order to avoid
   falsely  aliasing the various values that could arise in one statement.
   For example, after:
      unknown_fn (&a, &b);
   we want nodes to clobber a and b with, but we don't want to use the
   same node, or it would falsely implicitly assume that a == b.  */

class conjured_bit_contents : public unfoldable_bit_contents
{
public:
  struct key
  {
    key (const gimple *stmt,
	 const region *id_reg,
	 unsigned idx)
    : m_stmt (stmt), m_id_reg (id_reg), m_idx (idx)
    {
    }

    bool operator< (const key &other) const;

    complexity
    get_complexity () const;

    const gimple *m_stmt;
    const region *m_id_reg;
    unsigned m_idx;
  };

  conjured_bit_contents (node_manager &node_mgr, const key &k);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::conjured;
  }

  const conjured_bit_contents *
  dyn_cast_conjured_bit_contents () const final override { return this; }

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  conjured_p () const final override { return true; }

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  const gimple *get_stmt () const { return m_stmt; }
  const region *get_id_region () const { return m_id_reg; }
  unsigned get_index () const { return m_idx; }

private:
  const gimple *m_stmt;
  const region *m_id_reg;
  unsigned m_idx;
};

/* Concrete subclass of unfoldable_bit_contents for use in selftests.  */

#if CHECKING_P

class named_test_bit_contents : public unfoldable_bit_contents
{
public:
  named_test_bit_contents (node_manager &node_mgr, std::string name);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::named_test;
  }

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  const char *
  get_name () const { return m_name.c_str (); }

private:
  std::string m_name;
};

#endif /* #if CHECKING_P */

/* Abstract bit_contents subclasses that uses a baseline
   bit_contents in most of the domain, but overrides it at
   one or more places; subclasses provide the concrete implementation.

   Consolidation/canonicalization:
   - no nesting of overlays: the baseline must not itself be a bit_overlay.  */

class bit_overlay : public bit_contents
{
public:
  const bit_contents &
  get_baseline () const { return m_baseline; }

protected:
  bit_overlay (complexity c, const bit_contents &baseline)
  : bit_contents (c, baseline.get_node_mgr ()),
    m_baseline (baseline)
  {
  }

private:
  const bit_contents &m_baseline;
};

/* A sparse mapping from concrete bit ranges to symbolic bit maps,
   overlaid on an underlying baseline bit_contents.

   Has no size; bounds-checking must happen at a higher level.

   Consolidation/canonicalization:
   - must have at least one entry in the map
   - no nesting of overlays: the baseline must not itself be a bit_overlay.
   - bit ranges in the map are ordered by start offset
   - no overlapping of bit ranges
   - no redundant bindings: the bit_vector bound to RANGE must not equal
     BASELINE.get_bits(RANGE)
   - the size of each bit_vector must be concrete and match that of the bit
     range it is bound to
   - no leading concrete-sized child elements of a concat_bit_vector: these
     are canonicalized by splitting out into separate mappings from range
     to component bit_vector.  */

class concrete_bit_overlay : public bit_overlay
{
public:
  struct key
  {
    const bit_contents *m_baseline;
    std::map<concrete_bit_range, const bit_vector &> m_concrete_range_map;

    bool
    operator< (const key &other) const;

    complexity
    get_complexity () const;

    void
    remove_redundant_bindings ();
  };

  const concrete_bit_overlay *
  dyn_cast_concrete_bit_overlay () const final override { return this; }

  concrete_bit_overlay (const key &k);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::concrete_overlay;
  }

  size_t size () const { return m_concrete_range_map.size (); }

  const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const final override;

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &bv_from_ubc) const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  const bit_vector &
  get_concrete_bits (const concrete_bit_range &bit_range) const;

  const bit_contents &
  set_concrete_bits (const concrete_bit_range &bit_range,
		     const bit_vector &bv) const;

  const svalue *
  maybe_get_var_arg_svalue () const;

  static const bit_contents *
  can_merge_p (const bit_contents &bit_contents_a,
	       const bit_contents &bit_contents_b,
	       const bit_contents &shared_baseline,
	       const concrete_bit_overlay *conc_bit_overlay_a,
	       const concrete_bit_overlay *conc_bit_overlay_b,
	       tree type,
	       model_merger *merger);

  static std::map<concrete_bit_range, const bit_vector &>
  canonicalize_new_bit_vec (const concrete_bit_range &new_bit_range,
			    const bit_vector &bv);

  static std::map<concrete_bit_range, const bit_vector &>
  consolidate_adjacent (std::map<concrete_bit_range, const bit_vector &> map,
			bit_vector_manager &bv_mgr);

  bool
  bit_offset_is_bound_p (concrete_bit_offset) const;

  /* Map from concrete ranges of bits to symbolic bit vectors.
     The sizes must match.
     By construction, the bit ranges do not overlap.  */
  std::map<concrete_bit_range, const bit_vector &> m_concrete_range_map;
};

/* A sparse mapping from symbolic bit ranges to symbolic bit vectors,
   overlaid on an underlying baseline bit_contents.

   Has no size; bounds-checking must happen at a higher level.

   Consolidation/canonicalization:
   - must have exactly one entry in the map (attempts to create more than
     one symbolic key ought to be folded into the baseline)
     TODO(can probably use this to greatly simplify this implementation,
     and possibly merge it with concrete_bit_overlay).
   - no nesting of overlays: the baseline must not itself be a bit_overlay.
   - bit ranges in the map are not allowed to have "unknown" start offset
     TODO(design): are they allowed to have "unknown" size?
   - bit ranges in the map are not allowed to be fully concrete
   - no redundant bindings: the bit_vector bound to RANGE must not equal
     BASELINE.get_bits(RANGE).
   - the size of each bit_vector must match that of the bit
     range it is bound to (this size may be either concrete or symbolic).  */

class symbolic_bit_overlay : public bit_overlay
{
public:
  struct key
  {
    const bit_contents *m_baseline;
    std::map<symbolic_bit_range, const bit_vector &> m_symbolic_range_map;

    bool
    operator< (const key &other) const;

    complexity
    get_complexity () const;

    void
    remove_redundant_bindings ();
  };

  const symbolic_bit_overlay *
  dyn_cast_symbolic_bit_overlay () const final override { return this; }

  symbolic_bit_overlay (const key &k);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::symbolic_overlay;
  }

  size_t size () const { return m_symbolic_range_map.size (); }

  const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const final override;

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &bv_from_ubc) const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  /* Map from symbolic ranges of bits to symbolic bit vectors.
     Note that in the current implementation the map has at most one
     entry, suggsesting future simplification possibilities.  */
  std::map<symbolic_bit_range, const bit_vector &> m_symbolic_range_map;
};

/* A bit_contents instance that offsets the coordinates of another
   bit_contents instance.

   Consolidation/canonicalization:
   - the offset cannot be zero
   - the inner bit_contents cannot be uniform
   - no nesting: the inner bit_contents cannot be an offset_bit_contents
     (we can simply fold by adding the offsets).  */

class offset_bit_contents : public bit_contents
{
public:
  struct key
  {
    const bit_contents *m_inner_bc;
    const symbolic_bit_offset *m_bit_offset;

    bool
    operator< (const key &other) const;

    complexity
    get_complexity () const;
  };

  const offset_bit_contents *
  dyn_cast_offset_bit_contents () const final override { return this; }

  offset_bit_contents (node_manager &node_mgr, const key &k);

  enum kind
  get_kind () const final override
  {
    return bit_contents::kind::offset;
  }

  const bit_vector &
  get_bits (const symbolic_bit_range &bit_range) const final override;

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  string_scan
  scan_for_first_zero_byte (string_scan::byte_offset_type start_byte_offset)
    const final override;

  void
  get_representative_path_vars (const region_model *model,
				value_set *visited,
				const region *base_reg,
				const svalue *sval,
				tree desired_type,
				logger *logger,
				auto_vec<path_var> *out_pvs)
    const final override;

  const bit_contents &
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind)
    const final override;

  const bit_contents &
  purge_state_involving (const symbolic::node &node) const final override;

  bool
  bits_live_p (const bit_vector_from_ubc &bv_from_ubc) const final override;

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const final override;

  const bit_contents *
  maybe_fold_offset (const symbolic_bit_offset &bit_offset)
    const final override;

  const bit_contents &
  get_inner_bit_contents () const { return m_inner_bc; }

  const symbolic_bit_offset &
  get_bit_offset () const { return m_bit_offset; }

private:
  const bit_contents &m_inner_bc;
  const symbolic_bit_offset &m_bit_offset;
};

/* A class with responsibility for allocating/consolidating
   bit_contents instances.  A singleton relative to a node_manager.  */

class bit_contents_manager
{
public:
  bit_contents_manager (node_manager &node_mgr);

  bit_contents_manager (const bit_contents_manager &) = delete;

  const uninit_contents &
  get_uninitialized () const { return m_uninit_contents; }

  const bit_contents &
  get_untracked (value_set maybe_live);

  const zero_contents &
  get_all_zeros () const { return m_zero_contents; }

  const bit_contents &
  set_bits (const bit_contents &original,
	    const symbolic_bit_range &bit_range,
	    const bit_vector &bv,
	    uncertainty_t *uncertainty);

  const bit_contents &
  set_bits (const bit_contents &original,
	    const concrete_bit_range &bit_range,
	    const bit_vector &bv,
	    uncertainty_t *uncertainty);

  const bit_contents &
  get_concrete_bit_overlay (concrete_bit_overlay::key k,
			    uncertainty_t *uncertainty);

  const bit_contents &
  get_symbolic_bit_overlay (symbolic_bit_overlay::key k,
			    uncertainty_t *uncertainty);

  const bit_contents &
  get_bit_contents_for_constant (tree tree_cst);

  const bit_contents &
  get_baseline_contents (const region &base_reg,
			 baseline_state bs);

  const bit_contents &
  get_conjured_bit_contents (const region &base_reg,
			     const gimple *stmt,
			     const conjured_purge &p,
			     unsigned idx = 0);

#if CHECKING_P
  const bit_contents &
  get_named_test_bit_contents (std::string name);
#endif

  const bit_contents &
  get_offset_bit_contents (const bit_contents &inner_bc,
			   const symbolic_bit_offset &bit_offset);

private:
  const bit_contents &
  make_bit_contents_for_constant (tree tree_cst);

  const bit_contents &
  make_baseline_contents (baseline_key k);

  node_manager &m_node_mgr;

  uninit_contents m_uninit_contents;
  zero_contents m_zero_contents;

  std::map<untracked_bit_contents::key,
	   std::unique_ptr<untracked_bit_contents>> m_untracked_bit_contents;

  std::map<concrete_bit_overlay::key,
	   std::unique_ptr<concrete_bit_overlay>> m_concrete_bit_overlays;

  std::map<symbolic_bit_overlay::key,
	   std::unique_ptr<symbolic_bit_overlay>> m_symbolic_bit_overlays;

  std::map<conjured_bit_contents::key,
	   std::unique_ptr<conjured_bit_contents>> m_conjured_map;
  std::vector<std::unique_ptr<unfoldable_bit_contents>> m_unfoldable_bit_contents;

  std::map<offset_bit_contents::key,
	   std::unique_ptr<offset_bit_contents>> m_offset_map;

  // Cached lookups
  std::map<tree, const bit_contents &> m_bit_contents_for_constants;
  std::map<baseline_key, const bit_contents &> m_baseline_contents;
};

} // namespace ana::symbolic
} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_BIT_MAP_H */
