/* Symbolic vectors of bits e.g. "(buf_a[0-10) + buf_b[0-len) + (0b * 8))"

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

#ifndef GCC_ANALYZER_SYMBOLIC_BIT_VECTORS_H
#define GCC_ANALYZER_SYMBOLIC_BIT_VECTORS_H

#include "analyzer/integer-with-unit.h"
#include "analyzer/concrete-ranges.h"
#include "analyzer/symbolic-nodes.h"
#include "analyzer/store.h"
#include "analyzer/program-point.h"
#include "analyzer/string-scan.h"
#include "text-art/widget.h"

namespace ana {

/* A concrete-sized vector of tristate bits.  */

using concrete_tristate_vector = std::vector<tristate>;

/* A bundle of options for use when printing hexdumps.  */

struct hexdump_options
{
  hexdump_options ()
  : m_show_headings (false),
    m_show_binary (false),
    m_show_hex (true),
    m_show_ascii (true),
    m_bytes_per_line (4_bytes)
  {
  }

  bool m_show_headings;
  bool m_show_binary;
  bool m_show_hex;
  bool m_show_ascii;
  concrete_byte_size m_bytes_per_line;
};

namespace symbolic {

class bit_vector_manager;

/* An abstract base class representing the contents of a buffer of memory,
   where the size is a symbolic_bit_size.

   Consider:

   | size_t len_a = strlen (a));
   | size_t len_b = strlen (b));
   |
   | memcpy (buf, a, len_a);
   | memcpy (buf + len_a, b, len_b);
   | buf[len_a + len_b] = '\0';

   and then trying to analyze the contents of "buf".  If len_a is known
   concretely as 10 bytes, but len_b is only known symbolically, the contents
   of buf might be represented as a symbolic::bit_vector as the
   concatenation of:

   - [buffer_a's bytes [0-10)],
   - [buffer_b's [bytes 0-'len_b')],
   - [8 zero bits])

   with size (11 + 'len_b') bytes (see selftest::test_example_strcat for
   a worked example similar to the above).

   bit_vectors can be concatenated, cropped, and repeated (with folding of
   various identities during these operations).

   The class hierarchy looks like this (using indentation to show
   inheritance, and with kinds shown for the concrete subclasses):

   symbolic::bit_vector
     concat_bit_vector (kind::concat): the concatenation of a vector of child bit_vectors
     conjured_bit_vector (kind::conjured): a bit vector arising from a stmt
     cropped_bit_vector (kind::cropped): another bit_vector, cropped and offset
     named_test_bit_vector (kind::named_test): an unfoldable bit_vector for use in selftests
     repeated_bit_vector (kind::repeated): repeating a bit_vector some number of times
     bit_vector_from_svalue (kind::svalue): the bit representation of an svalue on the target
     bit_vector_from_ubc (kind::from_ubc): a range of bits within an unfoldable_bit_contents
     uniform_bit_vector (abstract): a bit vector that is "the same bit" everywhere
       empty_bit_vector (kind::empty): the empty bit vector
       poisoned_bit_vector (kind::poisoned): unusable data
       tristate_bit_vector (kind::tristate): a range of bits that are all true/false/unknown.  */

class bit_vector : public symbolic::value_node
{
public:
  /* An enum for discriminating between the different concrete subclasses
     of symbolic bit_vector.  */
  enum class kind
  {
    concat,
    conjured,
    cropped,
    empty,
    named_test,
    poisoned,
    repeated,
    svalue,
    from_ubc,
    tristate
  };

  bit_vector_manager &
  get_bit_vector_manager () const { return m_bv_mgr; }

  const bit_vector *
  dyn_cast_bit_vector () const final override { return this; }

  void
  print_dump_widget_label (pretty_printer *pp) const final override;

  virtual void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const = 0;

  virtual void
  print_any_suffix_for_dump_widget_label (pretty_printer *) const {}

  const char *
  get_any_dump_color () const override { return "bit_vector"; }

  virtual bool
  maybe_print_for_user (pretty_printer *pp,
			const region_model &model,
			const symbolic::node *outer_node = nullptr) const;

  virtual ~bit_vector () {}

  virtual enum kind get_kind () const = 0;

  virtual const concat_bit_vector *
  dyn_cast_concat_bit_vector () const { return nullptr; }
  virtual const conjured_bit_vector *
  dyn_cast_conjured_bit_vector () const { return nullptr; }
  virtual const cropped_bit_vector *
  dyn_cast_cropped_bit_vector () const { return nullptr; }
  virtual const empty_bit_vector *
  dyn_cast_empty_bit_vector () const { return nullptr; }
  virtual const uniform_bit_vector *
  dyn_cast_uniform_bit_vector () const { return nullptr; }
  virtual const poisoned_bit_vector *
  dyn_cast_poisoned_bit_vector () const { return nullptr; }
  virtual const repeated_bit_vector *
  dyn_cast_repeated_bit_vector () const { return nullptr; }
  virtual const bit_vector_from_svalue *
  dyn_cast_bit_vector_from_svalue () const { return nullptr; }
  virtual const bit_vector_from_ubc *
  dyn_cast_bit_vector_from_ubc () const { return nullptr; }
  virtual const tristate_bit_vector *
  dyn_cast_tristate_bit_vector () const { return nullptr; }

  bool
  operator== (const bit_vector &other) const
  {
    return this == &other;
  }

  const symbolic_bit_size &
  get_size () const { return m_size_in_bits; }

  const symbolic_byte_size *
  maybe_get_size_in_bytes () const;

  bool
  maybe_get_concrete_size_in_bits (concrete_bit_size *out) const;

  bool
  maybe_get_concrete_size_in_bytes (concrete_byte_size *out) const;

  virtual tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const;

  virtual string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset) const = 0;

  void
  hexdump_to_pp (pretty_printer *,
		 const hexdump_options &hexdump_opts) const;

  void
  hexdump (const hexdump_options &hexdump_opts) const;

  void
  hexdump () const;

  virtual bool
  conjured_p () const;

  /* Convenience member functions (implemented using the bit_vector_manager).  */

  tristate
  get_tristate_at_offset (const concrete_bit_offset &bit) const;

  const bit_vector &
  get_cropped_bits (const symbolic_bit_range &bits_to_use) const;

  const bit_vector &
  get_cropped_bits (const concrete_bit_range &bits_to_use) const;

  const bit_vector &
  get_cropped_bytes (const symbolic_byte_range &bytes_to_use) const;

  const bit_vector &
  get_cropped_bytes (const concrete_byte_range &bytes_to_use) const;

  const bit_vector &
  get_prefix (const symbolic_bit_size &size) const;

  const bit_vector &
  get_prefix (const concrete_bit_size &size) const;

  const bit_vector &
  get_prefix (const symbolic_byte_size &size) const
  {
    return get_prefix (bits_from_bytes (size));
  }

  const bit_vector &
  get_prefix (const concrete_byte_size &size) const
  {
    return get_prefix (bits_from_bytes (size));
  }

  const bit_vector &
  get_suffix (const symbolic_bit_size &size) const;

  const bit_vector &
  get_suffix (const concrete_bit_size &size) const;

  const bit_vector &
  get_suffix (const symbolic_byte_size &size) const
  {
    return get_suffix (bits_from_bytes (size));
  }

  const bit_vector &
  get_suffix (const concrete_byte_size &size) const
  {
    return get_suffix (bits_from_bytes (size));
  }

  const bit_vector &
  get_byte_at_offset (const symbolic_byte_offset &byte_offset) const;

  const bit_vector &
  get_byte_at_offset (const concrete_byte_offset &byte_offset) const;

  virtual const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use) const;

  bool
  maybe_as_uint8 (uint8_t *out) const;

  static const bit_vector *
  can_merge_p (const bit_vector &bv_a,
	       const bit_vector &bv_b,
	       tree type,
	       model_merger *merger);

  bool
  maybe_get_irange_bitmask (irange_bitmask *out) const;

  bool derived_from_initial_value_p () const;

protected:
  bit_vector (bit_vector_manager &mgr,
	      complexity c,
	      const symbolic_bit_size &size_in_bits);

  std::unique_ptr<json::object>
  make_json_obj (const char *type_name) const;

  bit_vector_manager &m_bv_mgr;
  const symbolic_bit_size &m_size_in_bits;
};

/* A sequence of bit_vectors concatenated together.

   Consolidation/canonicalization:
   - we must have at least two child bit_vectors
   - all child bit_vectors must have non-zero size
   - no nesting: children must not be concat_bit_vector instances
   - consecutive children must not:
     - be the same bit_vector
     - be uniform_bit_vectors with the same underlying bit
     - be repeated_bit_vectors of the same underlying
       bit vector
     - be a bit_vector and a repeated_bit_vector of that
       bit_vector (in either order)
     - be bit_vectors taken from the same UBC at
       consecutive bit ranges.  */

class concat_bit_vector : public bit_vector
{
public:
  struct key
  {
    key (std::vector<const bit_vector *> bit_vectors)
    : m_bit_vectors (std::move (bit_vectors))
    {
    }

    bool operator< (const key &other) const;

    complexity
    get_complexity () const;

    const symbolic_bit_size &
    get_size (scalar_manager<concrete_bit_size> &bit_mgr) const;

    std::vector<const bit_vector *> m_bit_vectors;
  };

  enum kind get_kind () const final override
  {
    return kind::concat;
  }

  const concat_bit_vector *
  dyn_cast_concat_bit_vector () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  std::vector<const bit_vector *>::const_iterator
  begin () const { return m_bit_vectors.begin (); }
  std::vector<const bit_vector *>::const_iterator
  end () const { return m_bit_vectors.end (); }

  concat_bit_vector (bit_vector_manager &mgr,
		     const key &k);

  bool implicitly_live_p (symbolic::liveness_context &) const final override;

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;
private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::vector<const bit_vector *> m_bit_vectors;
};

/* A defined bit_vector arising from a statement, where we want to identify a
   particular unknown bundle of bits, so that the value can have sm-state.

   Comparisons of variables that share the same conjured_bit_vector are known
   to be equal, even if we don't know what the value is.

   For example, this is used for the values of regions of known size
   that may have been touched when calling an unknown function.

   The bit_vector captures a region as well as a stmt in order to avoid falsely
   aliasing the various bit vectors that could arise in one statement.  For
   example, after:
      unknown_fn (&a, &b);
   we want nodes to clobber a and b with, but we don't want to use the
   same node, or it would falsely implicitly assume that a == b.  */

class conjured_bit_vector : public bit_vector
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

    const symbolic_bit_size &
    get_size (scalar_manager<concrete_bit_size> &bit_mgr) const;

    const gimple *m_stmt;
    const region *m_id_reg;
    unsigned m_idx;
  };

  enum kind get_kind () const final override
  {
    return kind::conjured;
  }

  const conjured_bit_vector *
  dyn_cast_conjured_bit_vector () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  bool
  conjured_p () const final override
  {
    return true;
  }

  conjured_bit_vector (bit_vector_manager &mgr,
		       const key &k);

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;

  const gimple *get_stmt () const { return m_stmt; }
  const region *get_id_reg () const { return m_id_reg; }
  unsigned get_idx () const { return m_idx; }

private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  const gimple *m_stmt;
  const region *m_id_reg;
  unsigned m_idx;
};

/*  A cropping of another bit_vector.
    Visually, the cropped bit_vector is within the uncropped bit_vector:
    .
    .  uncropped_bit_vector:
    .  +--------------------------+
    .  |                          |
    .  +--------------------------+
    .
    .  +----+------------------+--+
    .  |    |cropped_bit_vector|  |
    .  +----+------------------+--+
    .       ^
    .       |
    .       crop_start_bit_offset (relative to uncropped)
    .
    but in terms of the object hierarchy the uncropped bit_vector is a child
    of the cropped bit_vector.

    The bounds can be concrete or symbolic.

    Consolidation/canonicalization:
    - no nesting: children must not be concat_bit_vector instances
    - we never have a cropped_bit_vector of a uniform_bit_vector, as
      these can be expressed simply as a uniform_bit_vector of the appropriate
      subclass of smaller size.  */

class cropped_bit_vector : public bit_vector
{
public:
  struct key
  {
    key (const bit_vector *uncropped_bv,
	 symbolic_bit_range bits_to_use)
    : m_uncropped_bv (uncropped_bv),
      m_bits_to_use (bits_to_use)
    {
    }

    bool operator< (const key &other) const;

    const symbolic_bit_size &
    get_size () const
    {
      return m_bits_to_use.get_size ();
    }

    const bit_vector *m_uncropped_bv;
    symbolic_bit_range m_bits_to_use;
  };

  enum kind get_kind () const final override
  {
    return kind::cropped;
  }

  const cropped_bit_vector *
  dyn_cast_cropped_bit_vector () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  cropped_bit_vector (bit_vector_manager &mgr,
		      const key &k);

  const bit_vector &
  get_uncropped_bit_vector () const { return m_uncropped_bv; }

  /* Get bit range of this bit_vector relative to the uncropped_bit_vector.  */
  symbolic_bit_range
  get_uncropped_bits_to_use () const
  {
    return symbolic_bit_range (m_crop_start_bit_offset, get_size ());
  }

  /* Try to get byte range of this bit_vector relative to the
     uncropped_bit_vector.  */
  bool
  get_uncropped_bytes_to_use (symbolic_byte_range *out) const
  {
    return maybe_bytes_from_bits (get_uncropped_bits_to_use (), out);
  }

  bool
  implicitly_live_p (symbolic::liveness_context &) const final override;

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;

private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  print_any_suffix_for_dump_widget_label (pretty_printer *pp)
    const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  const bit_vector &m_uncropped_bv;
  const symbolic_bit_offset &m_crop_start_bit_offset;
  // relative to the uncropped bit vector
};

/* An unfoldable bit_vector with a name, for use in selftests.  */

#if CHECKING_P

class named_test_bit_vector : public bit_vector
{
public:
  enum kind get_kind () const final override
  {
    return kind::named_test;
  }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const override;

  named_test_bit_vector (bit_vector_manager &mgr,
			 const symbolic_bit_size &size_in_bits,
			 std::string label);

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;
private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  print_any_suffix_for_dump_widget_label (pretty_printer *pp)
    const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  std::string m_label;
};

#endif /* #if CHECKING_P */

/* An abstract subclass for bit_vector subclasses that are the same
   at every bit offset (and for which we can thus simplify away "crop",
   "concatentate", and "repeat" operations by using a like-kinded
   instance of the appropriate size).  */

class uniform_bit_vector : public bit_vector
{
protected:
  uniform_bit_vector (bit_vector_manager &mgr,
		      complexity c,
		      const symbolic_bit_size &size_in_bits)
  : bit_vector (mgr, c, size_in_bits)
  {
  }
};

/* The empty bit vector.

   This is a uniform_bit_vector because, having no elements, all its
   elements are trivially equal.  This allows more canonicalization
   rules to be expresed purely in terms of uniform_bit_vector.

   Consolidation/canonicalization:
   - The only bit_vector with zero size should be the singleton
     empty_bit_vector; instances of all other subclasses should have
     non-zero size.  */

class empty_bit_vector : public uniform_bit_vector
{
public:
  enum kind get_kind () const final override
  {
    return kind::empty;
  }

  const empty_bit_vector *
  dyn_cast_empty_bit_vector () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  empty_bit_vector (bit_vector_manager &mgr);

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;
private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;
};

/* A run of bits that are unusable (e.g. uninitialized data).  */

class poisoned_bit_vector : public uniform_bit_vector
{
public:
  struct key
  {
    key (const symbolic_bit_size &size_in_bits,
	 enum poison_kind kind)
    : m_size_in_bits (size_in_bits),
      m_poison_kind (kind)
    {
    }

    bool operator< (const key &other) const;

    const symbolic_bit_size &m_size_in_bits;
    enum poison_kind m_poison_kind;
  };

  enum kind get_kind () const final override
  {
    return kind::poisoned;
  }

  const poisoned_bit_vector *
  dyn_cast_poisoned_bit_vector () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  poisoned_bit_vector (bit_vector_manager &mgr,
		       const key &k);

  enum poison_kind get_poison_kind () const { return m_poison_kind; }

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;
private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  print_any_suffix_for_dump_widget_label (pretty_printer *pp)
    const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  enum poison_kind m_poison_kind;
};

/* Concrete bit_vector subclass: N repeated copies of an underlying
   bit vector, where N could be symbolic.

   Consolidation/canonicalization:
   - we never have a concrete repeat count of zero or one, as these are
     trivially foldable to empty/inner_bv
   - no nesting: we never have a repeated_bit_vector of a inner_bv
   - we never have a repeated_bit_vector of a uniform_bit_vector, as
     these can be expressed simply as a uniform_bit_vector (of the appropriate
     subclass) of larger size.  */

class repeated_bit_vector : public bit_vector
{
public:
  struct key
  {
    key (const bit_vector *inner_bv,
	 const symbolic_integer *repeat_count)
    : m_inner_bv (inner_bv),
      m_repeat_count (repeat_count)
    {
    }

    bool operator< (const key &other) const;

    const symbolic_bit_size &
    get_size () const
    {
      return m_inner_bv->get_size () * (*m_repeat_count);
    }

    const bit_vector *m_inner_bv;
    const symbolic_integer *m_repeat_count;
  };

  enum kind get_kind () const final override
  {
    return kind::repeated;
  }

  const repeated_bit_vector *
  dyn_cast_repeated_bit_vector () const final override { return this; }

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  repeated_bit_vector (bit_vector_manager &mgr,
		       const key &k);

  const bit_vector &
  get_inner_bit_vector () const { return m_inner_bv; }

  const symbolic_integer &
  get_repeat_count () const { return m_repeat_count; }

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;
private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  const bit_vector &m_inner_bv;
  const symbolic_integer &m_repeat_count;
};

/* Concrete bit_vector subclass: the bit representation of an svalue
   on the target.

   Consolidation/canonicalization:
   - no nesting: inner svalue can't be a svalue_from_bit_vector
   - the inner svalue must not have zero size (or we'd have an empty_bit_vector)
   - no all zeros or all ones (we fold to tristate_bit_vector for this)
   - all other concrete 8-bit vectors are canonicalized to m_cached_bytes,
     each of which is the bit_vector_from_svalue of an unsigned char of the
     relevant value.  */

class bit_vector_from_svalue : public bit_vector
{
public:
  struct key
  {
    key (const svalue *sval)
    : m_sval (sval)
    {
    }

    bool operator< (const key &other) const;

    const symbolic_bit_size &
    get_size (scalar_manager<concrete_bit_size> &bit_mgr) const;

    const svalue *m_sval;
  };

  enum kind get_kind () const final override
  {
    return kind::svalue;
  }

  const bit_vector_from_svalue *
  dyn_cast_bit_vector_from_svalue () const final override { return this; }

  bool
  maybe_print_for_user (pretty_printer *pp,
			const region_model &model,
			const symbolic::node *outer_node = nullptr)
    const final override;

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  bit_vector_from_svalue (bit_vector_manager &mgr,
			  const key &k);

  const svalue &get_svalue () const { return m_sval; }

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  bool implicitly_live_p (symbolic::liveness_context &) const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;
private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  const bit_vector *
  maybe_fold_crop_from_cst (const concrete_bit_range &inner_bits_to_use) const;

  const bit_vector *
  maybe_fold_crop_from_bitmask (const concrete_bit_range &inner_bits_to_use)
    const;

  const bit_vector *
  maybe_fold_crop_from_initial_bits (const concrete_bit_range &inner_bits_to_use)
    const;

  const svalue &m_sval;
};

/* Concrete bit_vector subclass: a range of bits from an unfoldable_bit_contents
   instance.  These are managed by their unfoldable_bit_contents instance.

   Consolidation/canonicalization:
   - TODO.  */

class bit_vector_from_ubc : public bit_vector
{
public:
  enum kind get_kind () const final override
  {
    return kind::from_ubc;
  }

  const bit_vector_from_ubc *
  dyn_cast_bit_vector_from_ubc () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  bit_vector_from_ubc (const unfoldable_bit_contents &bc,
		       symbolic_bit_range bit_range);

  const unfoldable_bit_contents &
  get_bit_contents () const { return m_unfoldable_bit_contents; }

  const symbolic_bit_range &
  get_bit_range () const { return m_bit_range; }

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;

  bool implicitly_live_p (symbolic::liveness_context &) const final override;

  bool
  conjured_p () const final override;

 private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;

  void
  accept_node_visitor (node_visitor &v) const final override;

  const unfoldable_bit_contents &m_unfoldable_bit_contents;
  symbolic_bit_range m_bit_range;
};

/* A run of bits that are all 0, all 1, or all unknown.  */

class tristate_bit_vector : public uniform_bit_vector
{
public:
  struct key
  {
    key (const symbolic_bit_size &size_in_bits,
	 tristate bit_value)
    : m_size_in_bits (size_in_bits),
      m_bit_value (bit_value)
    {
    }

    bool operator< (const key &other) const;

    const symbolic_bit_size &m_size_in_bits;
    tristate m_bit_value;
  };

  enum kind get_kind () const final override
  {
    return kind::tristate;
  }

  const tristate_bit_vector *
  dyn_cast_tristate_bit_vector () const final override { return this; }

  tristate
  get_tristate_at_offset (const symbolic_bit_offset &bit) const final override;

  string_scan
  scan_for_first_zero_byte (concrete_byte_offset start_byte_offset)
    const final override;

  tristate_bit_vector (bit_vector_manager &mgr,
		       const key &k);

  tristate
  get_bit_value () const { return m_bit_value; }

  const bit_vector *
  maybe_fold_crop (const symbolic_bit_range &inner_bits_to_use)
    const final override;

  std::unique_ptr<json::object>
  to_json () const final override;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const final override;

  iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const final override;

private:
  void
  print_prefix_for_dump_widget_label (pretty_printer *pp) const final override;

  void
  print_any_suffix_for_dump_widget_label (pretty_printer *pp)
    const final override;

  void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const final override;
  void
  accept_node_visitor (node_visitor &v) const final override;

  tristate m_bit_value;
};

/* A class with responsibility for allocating/consolidating
   bit_vector instances.  A singleton relative to a node_manager.  */

class bit_vector_manager
{
public:
  bit_vector_manager (node_manager &node_mgr);

  bit_vector_manager (const bit_vector_manager &) = delete;

  /* bit_vector consolidation.  */

  const bit_vector &
  get_tristate_bits (const symbolic_bit_size &num_bits, tristate bit_val);

  const bit_vector &
  get_tristate_bits (const concrete_bit_size &num_bits, tristate bit_val);

  const bit_vector &
  get_all_zeros (const symbolic_bit_size &num_bits)
  {
    return get_tristate_bits (num_bits, tristate (false));
  }

  const bit_vector &
  get_all_zeros (const concrete_bit_size &num_bits)
  {
    return get_tristate_bits (num_bits, tristate (false));
  }

  const bit_vector &
  get_all_zeros (const symbolic_byte_size &num_bytes)
  {
    return get_tristate_bits (bits_from_bytes (num_bytes), tristate (false));
  }

  const bit_vector &
  get_all_zeros (const concrete_byte_size &num_bytes)
  {
    return get_tristate_bits (bits_from_bytes (num_bytes), tristate (false));
  }

  const bit_vector &
  get_all_ones (const symbolic_bit_size &num_bits)
  {
    return get_tristate_bits (num_bits, tristate (true));
  }

  const bit_vector &
  get_all_ones (const concrete_bit_size &num_bits)
  {
    return get_tristate_bits (num_bits, tristate (true));
  }

  const bit_vector &
  get_all_ones (const symbolic_byte_size &num_bytes)
  {
    return get_tristate_bits (bits_from_bytes (num_bytes), tristate (true));
  }

  const bit_vector &
  get_all_ones (const concrete_byte_size &num_bytes)
  {
    return get_tristate_bits (bits_from_bytes (num_bytes), tristate (true));
  }

  const bit_vector &
  get_concat_bits (std::vector<const bit_vector *> vec_of_bit_vectors);

  const bit_vector &
  get_conjured_bits (const gimple *stmt,
		     const region *id_reg,
		     const conjured_purge &p,
		     unsigned idx);

  const bit_vector &
  get_bits_for_constant (tree tree_cst);

  const bit_vector &
  get_empty () const { return m_empty_bv; }

  const bit_vector &
  get_unknown_bits (const symbolic_bit_size &num_bits)
  {
    return get_tristate_bits (num_bits, tristate::unknown ());
  }

  const bit_vector &
  get_unknown_bytes (const symbolic_byte_size &num_bytes)
  {
    return get_tristate_bits (bits_from_bytes (num_bytes),
			      tristate::unknown ());
  }

  const bit_vector &
  get_unknown_bits (const concrete_bit_size &num_bits)
  {
    return get_tristate_bits (num_bits, tristate::unknown ());
  }

  const bit_vector &
  get_unknown_bytes (const concrete_byte_size &num_bytes)
  {
    return get_tristate_bits (bits_from_bytes (num_bytes), tristate::unknown ());
  }

  // Unknown bits of unknown length
  const bit_vector &
  get_unknown_bits ();

  const bit_vector &
  get_poisoned_bits (const symbolic_bit_size &num_bits,
		     enum poison_kind kind);

  const bit_vector &
  get_poisoned_bits (const concrete_bit_size &num_bits,
		     enum poison_kind kind);

  const bit_vector &
  get_poisoned_bytes (const symbolic_byte_size &num_bytes,
		      enum poison_kind kind)
  {
    return get_poisoned_bits (bits_from_bytes (num_bytes), kind);
  }

  const bit_vector &
  get_uninitialized_bits (const symbolic_bit_size &num_bits)
  {
    return get_poisoned_bits (num_bits, poison_kind::uninit);
  }

  const bit_vector &
  get_uninitialized_bytes (const symbolic_byte_size &num_bytes)
  {
    return get_poisoned_bits (bits_from_bytes (num_bytes),
			      poison_kind::uninit);
  }

  const bit_vector &
  get_uninitialized_bytes (const concrete_byte_size &num_bytes)
  {
    return get_poisoned_bits (bits_from_bytes (num_bytes),
			      poison_kind::uninit);
  }

  const bit_vector &
  get_cropped_bits (const bit_vector &uncropped_bv,
		    const symbolic_bit_range &bits_to_use);

  const bit_vector &
  get_cropped_bytes (const bit_vector &uncropped_bv,
		     const symbolic_byte_range &bytes_to_use);

  const bit_vector &
  get_repeated_bits (const bit_vector &inner_bv,
		     const symbolic_integer &repeat_count);

  const bit_vector &
  get_repeated_bits (const bit_vector &inner_bv,
		     const concrete_integer &repeat_count);

  const bit_vector &
  get_bit_vector_from_svalue (const svalue &sval);

  const bit_vector *
  maybe_get_bit_vector_from_svalue (const svalue &sval);

  const bit_vector &
  get_uint8 (uint8_t value)
  {
    gcc_assert (m_cached_bytes[value]);
    return *m_cached_bytes[value];
  }

#if CHECKING_P
  const bit_vector &
  get_named_test_bit_vector (const symbolic_bit_size &size_in_bits,
			     std::string label);

  const bit_vector &
  get_named_test_bit_vector (const symbolic_byte_size &size_in_bytes,
			     std::string label);

  const bit_vector &
  get_named_test_bit_vector (const concrete_bit_size &size_in_bits,
			     std::string label);

  const bit_vector &
  get_named_test_bit_vector (const concrete_byte_size &size_in_bytes,
			     std::string label);
#endif /* #if CHECKING_P */

  node_manager &
  get_node_manager () const { return m_node_mgr; }

  scalar_manager<concrete_bit_size> &
  get_bit_manager () const
  {
    return m_node_mgr.get_scalars ().get_bits ();
  }

  region_model_manager &
  get_svalue_manager () const { return m_node_mgr.get_svalue_manager (); }

  const bit_vector &
  get_baseline_bit_vector (const region &base_reg,
			   baseline_state bs);

  const bit_vector *
  maybe_fold_bytes_within_constant (tree tree_cst,
				    concrete_byte_range conc_bytes);
  const bit_vector *
  maybe_fold_bits_for_svalue (const svalue &sval);

private:
  const bit_vector *
  maybe_fold_concat_bits (const std::vector<const bit_vector *> &vec);

  std::vector<const bit_vector *>
  canonicalize_concat_bits (const std::vector<const bit_vector *> &vec);

  const bit_vector *
  maybe_fold_bits_for_constant (tree tree_cst);

  const bit_vector *
  maybe_fold_bytes_within_string_cst (tree string_cst,
				      concrete_byte_range conc_bytes);
  const bit_vector *
  maybe_fold_bytes_within_raw_data_cst (tree raw_data_cst,
					concrete_byte_range conc_bytes);

  const bit_vector *
  maybe_fold_cropped_bits (const bit_vector &inner_bits,
			   const symbolic_bit_range &inner_bits_to_use);

  const bit_vector &
  make_baseline_bit_vector (baseline_key k);

  const bit_vector *
  maybe_fold_repeated_bits (const bit_vector &inner_bits,
			    const symbolic_integer &repeat_count);

  node_manager &m_node_mgr;

  std::map<concat_bit_vector::key,
	   std::unique_ptr<concat_bit_vector>> m_concat_map;
  std::map<conjured_bit_vector::key,
	   std::unique_ptr<conjured_bit_vector>> m_conjured_map;
  std::map<cropped_bit_vector::key,
	   std::unique_ptr<cropped_bit_vector>> m_cropped_map;
  empty_bit_vector m_empty_bv;
  std::map<poisoned_bit_vector::key,
	   std::unique_ptr<poisoned_bit_vector>> m_poisoned_map;
  std::map<repeated_bit_vector::key,
	   std::unique_ptr<repeated_bit_vector>> m_repeated_map;
  std::map<bit_vector_from_svalue::key,
	   std::unique_ptr<bit_vector_from_svalue>> m_svalue_to_bv_map;
  std::map<tristate_bit_vector::key,
	   std::unique_ptr<tristate_bit_vector>> m_tristate_map;

  std::map<baseline_key, const bit_vector &> m_baseline_map;

#if CHECKING_P
  std::vector<std::unique_ptr<named_test_bit_vector>> m_named_test_bvs;
#endif

  const bit_vector *m_cached_bytes[256];
};

} // namespace ana::symbolic
} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_BIT_VECTORS_H */
