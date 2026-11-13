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

#include "analyzer/common.h"

#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "tm.h"

#include "diagnostics/markup.h"
#include "selftest.h"

#include "analyzer/symbolic-bit-vectors.h"
#include "analyzer/region-model.h"
#include "analyzer/liveness-context.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

// TODO: move this
/* class conjured_purge.  */

/* Purge state relating to BV.  */

void
conjured_purge::purge (const symbolic::conjured_bit_vector &bv) const
{
  m_model->purge_state_involving (bv, m_ctxt);
}

/* Purge state relating to BV.  */

void
conjured_purge::purge (const symbolic::conjured_bit_contents &bc) const
{
  m_model->purge_state_involving (bc, m_ctxt);
}

namespace symbolic {

// class bit_vector : public symbolic::node

bit_vector::bit_vector (bit_vector_manager &bv_mgr,
			complexity c,
			const symbolic_bit_size &size_in_bits)
: value_node (c, bv_mgr.get_node_manager ().alloc_node_id ()),
  m_bv_mgr (bv_mgr),
  m_size_in_bits (size_in_bits)
{
}

void
bit_vector::print_dump_widget_label (pretty_printer *pp) const
{
  start_any_dump_color (pp);

  print_prefix_for_dump_widget_label (pp);

  pp_string (pp, "[size: ");

  end_any_dump_color (pp);
  m_size_in_bits.start_any_dump_color (pp);

  m_size_in_bits.dump_inline_to_pp (pp, units::role::size);

  m_size_in_bits.end_any_dump_color (pp);
  start_any_dump_color (pp);

  pp_string (pp, "]");

  print_any_suffix_for_dump_widget_label (pp);

  end_any_dump_color (pp);
}

bool
bit_vector::maybe_print_for_user (pretty_printer *,
				  const region_model &,
				  const symbolic::node *) const
{
  return false;
}

const symbolic_byte_size *
bit_vector::maybe_get_size_in_bytes () const
{
  return maybe_bytes_from_bits (m_size_in_bits);
}

bool
bit_vector::maybe_get_concrete_size_in_bits (concrete_bit_size *out) const
{
  return concrete_from_symbolic (m_size_in_bits, out);
}

bool
bit_vector::maybe_get_concrete_size_in_bytes (concrete_byte_size *out) const
{
  concrete_bit_size conc_bit_size;
  if (!maybe_get_concrete_size_in_bits (&conc_bit_size))
    return false;
  return maybe_bytes_from_bits (conc_bit_size, out);
}

/* Base implementation of get_tristate_at_offset: crop to one bit, and get
   that bit as a tristate.  */

tristate
bit_vector::get_tristate_at_offset (const symbolic_bit_offset &bit) const
{
  auto &bit_mgr = m_bv_mgr.get_node_manager ().get_scalars ().get_bits ();
  auto &bv_for_bit = get_cropped_bits
    (symbolic_bit_range (bit,
			 bit_mgr.get_concrete (1_bit)));
  gcc_assert (bv_for_bit.get_size ().is_one_p ());
  auto *ts_bv = bv_for_bit.dyn_cast_tristate_bit_vector ();
  if (!ts_bv)
    return tristate::unknown ();
  return ts_bv->get_bit_value ();
}

class hexdumper
{
public:
  hexdumper (text_art::style_manager &sm,
	     const hexdump_options &hexdump_opts)
  : m_sm (sm),
    m_hexdump_opts (hexdump_opts),
    m_num_columns (1),
    m_x_bit_range (0),
    m_x_binary (-1),
    m_x_hex (-1),
    m_x_ascii (-1)
  {
    if (hexdump_opts.m_show_binary)
      m_x_binary = m_num_columns++;
    if (hexdump_opts.m_show_hex)
      m_x_hex = m_num_columns++;
    if (hexdump_opts.m_show_ascii)
      m_x_ascii = m_num_columns++;
  }

  text_art::table
  make_table (const bit_vector &bv,
	      concrete_bit_size conc_bit_size)
  {
    using namespace text_art;

    auto bits_per_line = bits_from_bytes (m_hexdump_opts.m_bytes_per_line);

    table t (table::size_t (m_num_columns, 0));

    if (m_hexdump_opts.m_show_headings)
      {
	int y_headings = t.add_row ();
	if (m_x_binary >= 0)
	  t.set_cell (table::coord_t (m_x_binary, y_headings),
		      styled_string (m_sm, "Binary"));
	if (m_x_hex >= 0)
	  t.set_cell (table::coord_t (m_x_hex, y_headings),
		      styled_string (m_sm, "Hex"));
	if (m_x_ascii >= 0)
	  t.set_cell (table::coord_t (m_x_ascii, y_headings),
		      styled_string (m_sm, "ASCII"));
      }

    for (concrete_bit_offset start_of_this_line = 0_bits;
	 start_of_this_line < conc_bit_size;
	 start_of_this_line += bits_per_line)
      {
	auto start_of_next_line = start_of_this_line + bits_per_line;
	auto size_of_this_line = (start_of_next_line < conc_bit_size
				  ? bits_per_line
				  : conc_bit_size - start_of_this_line);
	add_hexdump_line (t,
			  bv,
			  concrete_bit_range (start_of_this_line,
					      size_of_this_line));
    }
    return t;
  }

private:
  void
  add_hexdump_line (text_art::table &t,
		    const bit_vector &bv,
		    concrete_bit_range bits_on_line)
  {
    using namespace text_art;

    gcc_assert (bits_on_line.get_start_offset () >= 0_bits);
    //gcc_assert (bits_on_line.get_last_offset () < bv.get_size ());

    auto row_y = t.add_row ();

    {
      pretty_printer pp;
      bits_on_line.dump_to_pp_pretty (&pp);
      t.set_cell (table::coord_t (0, row_y),
		  styled_string (m_sm, pp_formatted_text (&pp)),
		  x_align::RIGHT);
    }

    /* Try to print the individual bits.  */
    if (m_x_binary >= 0)
      {
	pretty_printer pp;
	int count = 0;
	for (concrete_bit_offset i = bits_on_line.get_start_offset ();
	     i < bits_on_line.get_next_offset ();
	     i += 1_bit, ++count)
	  {
	    tristate ts = bv.get_tristate_at_offset (i);
	    pp_character (&pp, get_dump_char_for_tristate_bit (ts));

	    // Add a space every 8 bits:
	    if (count % 8 == 7
		&& i < bits_on_line.get_last_offset ())
	      pp_space (&pp);
	  }
	t.set_cell (table::coord_t (m_x_binary, row_y),
		    styled_string (m_sm, pp_formatted_text (&pp)),
		    x_align::LEFT);
      }

    /* Try to print individual bytes.  */
    if (m_x_hex >= 0)
      {
	pretty_printer pp;
	bool first = true;
	for (concrete_bit_offset i = bits_on_line.get_start_offset ();
	     i < bits_on_line.get_next_offset ();
	     i += 8_bits)
	  {
	    if (i + 7_bits < bits_on_line.get_next_offset ())
	      {
		auto &byte
		  = bv.get_cropped_bits (concrete_bit_range (i, 8_bits));
		uint8_t byte_val;
		if (first)
		  first = false;
		else
		  pp_space (&pp);
		if (byte.maybe_as_uint8 (&byte_val))
		  print_byte_as_hex (&pp, byte_val);
		else
		  pp_string (&pp, "??");
	      }
	  }
	t.set_cell (table::coord_t (m_x_hex, row_y),
		    styled_string (m_sm, pp_formatted_text (&pp)),
		    x_align::LEFT);
      }

    // Try to print ASCII
    if (m_x_ascii >= 0)
      {
	pretty_printer pp;
	for (concrete_bit_offset i = bits_on_line.get_start_offset ();
	     i < bits_on_line.get_next_offset ();
	     i += 8_bits)
	  {
	    if (i + 7_bits < bits_on_line.get_next_offset ())
	      {
		auto &byte
		  = bv.get_cropped_bits (concrete_bit_range (i, 8_bits));
		pp_character (&pp, get_dump_char_for_byte (byte));
	      }
	  }
	t.set_cell (table::coord_t (m_x_ascii, row_y),
		    styled_string (m_sm, pp_formatted_text (&pp)),
		    x_align::LEFT);
      }
  }

  static char
  get_dump_char_for_tristate_bit (tristate t)
  {
    switch (t.get_value ())
      {
      default:
	gcc_unreachable ();
      case tristate::TS_UNKNOWN:
	return '?';
      case tristate::TS_TRUE:
	return '1';
      case tristate::TS_FALSE:
	return '0';
      }
  }

  static void
  print_byte_as_hex (pretty_printer *pp, uint8_t byte)
  {
    const char *const hexchars = "0123456789abcdef";
    pp_character (pp, hexchars[byte / 16]);
    pp_character (pp, hexchars[byte % 16]);
  }

  static char
  get_dump_char_for_byte (const bit_vector &bv)
  {
    uint8_t byte_val;
    if (!bv.maybe_as_uint8 (&byte_val))
      return '?';
    gcc_assert (byte_val >= 0 && byte_val <= 255);
    if (ISPRINT (byte_val))
      return byte_val;
    else
      return '.';
  }

  text_art::style_manager &m_sm;

  const hexdump_options &m_hexdump_opts;
  int m_num_columns;
  int m_x_bit_range;
  int m_x_binary;
  int m_x_hex;
  int m_x_ascii;
};

static std::unique_ptr<text_art::canvas>
make_hexdump_canvas (const bit_vector &bv,
		     const hexdump_options &hexdump_opts)
{
  concrete_bit_size conc_bit_size;
  if (!concrete_from_symbolic (bv.get_size (), &conc_bit_size))
    return nullptr;

  text_art::style_manager sm;
  text_art::ascii_theme theme;
  hexdumper h (sm, hexdump_opts);
  auto t = h.make_table (bv, conc_bit_size);
  return std::make_unique<text_art::canvas> (t.to_canvas (theme, sm));
}

void
bit_vector::hexdump_to_pp (pretty_printer *pp,
			   const hexdump_options &hexdump_opts) const
{
  auto c = make_hexdump_canvas (*this, hexdump_opts);
  if (!c)
    return;
  c->print_to_pp (pp);
}

void
bit_vector::hexdump (const hexdump_options &hexdump_opts) const
{
  tree_dump_pretty_printer pp (stderr);
  hexdump_to_pp (&pp, hexdump_opts);
}

void
bit_vector::hexdump () const
{
  hexdump_options hexdump_opts;
  hexdump_opts.m_show_headings = true;
  hexdump_opts.m_show_binary = false;
  hexdump_opts.m_show_hex = true;
  hexdump_opts.m_show_ascii = true;
  hexdump (hexdump_opts);
}

bool
bit_vector::conjured_p () const
{
  return false;
}

tristate
bit_vector::get_tristate_at_offset (const concrete_bit_offset &bit) const
{
  auto &bit_mgr = m_bv_mgr.get_node_manager ().get_scalars ().get_bits ();
  auto &sym_bit_offset = bit_mgr.get_concrete (bit);
  return get_tristate_at_offset (sym_bit_offset);
}

const bit_vector &
bit_vector::get_cropped_bits (const symbolic_bit_range &bits_to_use) const
{
  return m_bv_mgr.get_cropped_bits (*this, bits_to_use);
}

const bit_vector &
bit_vector::get_cropped_bits (const concrete_bit_range &bits_to_use) const
{
  auto &bit_mgr = m_bv_mgr.get_bit_manager ();
  return m_bv_mgr.get_cropped_bits (*this,
				    symbolic_from_concrete (bits_to_use,
							    bit_mgr));
}

const bit_vector &
bit_vector::get_cropped_bytes (const symbolic_byte_range &bytes_to_use) const
{
  return m_bv_mgr.get_cropped_bits (*this, bits_from_bytes (bytes_to_use));
}

const bit_vector &
bit_vector::get_cropped_bytes (const concrete_byte_range &bytes_to_use) const
{
  auto &bit_mgr = m_bv_mgr.get_bit_manager ();
  return m_bv_mgr.get_cropped_bits
    (*this,
     symbolic_from_concrete (bits_from_bytes (bytes_to_use),
			     bit_mgr));
}

/* Get the first SIZE bits of this bit_vector.  */

const bit_vector &
bit_vector::get_prefix (const symbolic_bit_size &size) const
{
  auto &bit_mgr = m_bv_mgr.get_bit_manager ();
  return m_bv_mgr.get_cropped_bits
    (*this,
     symbolic_bit_range (bit_mgr.get_concrete (0_bits),
			 size));
}

const bit_vector &
bit_vector::get_prefix (const concrete_bit_size &size) const
{
  return get_prefix (symbolic_from_concrete (size,
					     m_bv_mgr.get_bit_manager ()));
}

/* Get the last SIZE bits of this bit_vector.  */

const bit_vector &
bit_vector::get_suffix (const symbolic_bit_size &size) const
{
  return m_bv_mgr.get_cropped_bits (*this,
				    symbolic_bit_range (get_size () - size,
							size));
}

const bit_vector &
bit_vector::get_suffix (const concrete_bit_size &size) const
{
  return get_suffix (symbolic_from_concrete (size,
					     m_bv_mgr.get_bit_manager ()));
}

const bit_vector &
bit_vector::get_byte_at_offset (const symbolic_byte_offset &byte_offset) const
{
  auto &byte_mgr = m_bv_mgr.get_node_manager ().get_scalars ().get_bytes ();
  return m_bv_mgr.get_cropped_bytes
    (*this,
     symbolic_byte_range (byte_offset,
			  byte_mgr.get_concrete (1_byte)));
}

const bit_vector &
bit_vector::get_byte_at_offset (const concrete_byte_offset &byte_offset) const
{
  auto &byte_mgr = m_bv_mgr.get_node_manager ().get_scalars ().get_bytes ();
  return m_bv_mgr.get_cropped_bytes
    (*this,
     symbolic_byte_range (byte_mgr.get_concrete (byte_offset),
			  byte_mgr.get_concrete (1_byte)));
}

const bit_vector *
bit_vector::maybe_fold_crop (const symbolic_bit_range &) const
{
  return nullptr;
}

/* If this is an 8-bit constant value, return true and write it to *OUT.
   Otherwise return false.  */

bool
bit_vector::maybe_as_uint8 (uint8_t *out) const
{
  concrete_byte_size byte_size;
  if (!maybe_get_concrete_size_in_bytes (&byte_size))
    return false;
  if (byte_size != 1_byte)
    return false;

  if (auto *ts = dyn_cast_tristate_bit_vector ())
    switch (ts->get_bit_value ().get_value ())
      {
      default:
	gcc_unreachable ();
      case tristate::TS_UNKNOWN:
	return false;

      case tristate::TS_TRUE:
	*out = 0xff;
	return true;

      case tristate::TS_FALSE:
	*out = 0x00;
	return true;
      }

  if (auto *sval_bv = dyn_cast_bit_vector_from_svalue ())
    {
      auto &sval = sval_bv->get_svalue ();
      if (tree type = sval.get_type ())
	if (INTEGRAL_TYPE_P (type)
	    && TYPE_PRECISION (type) == 8)
	  if (tree cst = sval.maybe_get_constant ())
	    {
	      *out = TREE_INT_CST_LOW (cst);
	      return true;
	    }
    }

  return false;
}

const bit_vector *
bit_vector::can_merge_p (const bit_vector &bv_a,
			 const bit_vector &bv_b,
			 tree type,
			 model_merger *merger)
{
  if (&bv_a == &bv_b)
    return &bv_a;

  auto &bv_mgr = bv_a.m_bv_mgr;

  /* Reject attempts to merge poisoned bit vectors with other bit vectors
     (either non-poisoned, or other kinds of poison), so that e.g.
     we identify paths in which a variable is conditionally uninitialized.  */
  if (bv_a.get_kind () == kind::poisoned
      || bv_b.get_kind () == kind::poisoned)
    return nullptr;

  if (!merger->mergeable_bit_vector_p (bv_a))
    return nullptr;
  if (!merger->mergeable_bit_vector_p (bv_b))
    return nullptr;

  /* If we don't have a type, but both bvs are built from svalues with
     the same underlying type, try to use that type.  */
  if (!type)
    if (auto *from_sval_a = bv_a.dyn_cast_bit_vector_from_svalue ())
      if (auto *from_sval_b = bv_b.dyn_cast_bit_vector_from_svalue ())
	{
	  tree type_a = from_sval_a->get_svalue ().get_type ();
	  tree type_b = from_sval_b->get_svalue ().get_type ();
	  if (type_a == type_b)
	    type = type_a;
	}

  if (type)
    {
      auto &rmm = bv_mgr.get_node_manager ().get_svalue_manager ();
      const svalue *sval_a = rmm.get_svalue_from_bit_vector (type, bv_a);
      const svalue *sval_b = rmm.get_svalue_from_bit_vector (type, bv_b);
      if (const svalue *merged_sval = sval_a->can_merge_p (sval_b, &rmm,
							   merger))
	return &bv_mgr.get_bit_vector_from_svalue (*merged_sval);
      else
	/* svalues not mergeable; don't merge the bit vectors.  */
	return nullptr;
    }

  return &bv_mgr.get_unknown_bits (bv_a.get_size ());
}

std::unique_ptr<json::object>
bit_vector::make_json_obj (const char *type_name) const
{
  auto obj = value_node::make_json_obj (type_name);
  obj->set ("size_in_bits", m_size_in_bits.to_json ());
  return obj;
}


static bool
maybe_set_irange_bitmask_from_concrete_tristate_vector (const concrete_tristate_vector &ts_vec,
							irange_bitmask *out)
{
  unsigned HOST_WIDE_INT value = 0;
  unsigned HOST_WIDE_INT mask = -1;
  for (size_t i = 0; i < ts_vec.size (); ++i)
    {
      tristate ts = ts_vec[i];
      if (ts.is_known ())
	{
	  /* Clear the mask for known bits.  */
	  mask &= ~(1L << i);

	  /* Set the value.  */
	  if (ts.is_true ())
	    value |= (1L << i);
	}
    }
  *out = irange_bitmask (wi::uhwi (value, ts_vec.size ()),
			 wi::uhwi (mask, ts_vec.size ()));
  return true;
}

bool
bit_vector::maybe_get_irange_bitmask (irange_bitmask *out) const
{
  concrete_bit_size conc_size;
  if (!maybe_get_concrete_size_in_bits (&conc_size))
    return false;

  if (conc_size > 64_bits)
    return false;

  out->set_unknown (conc_size.as_unsigned_long ());
  concrete_tristate_vector ts_vec;
  ts_vec.reserve (conc_size.as_unsigned_long ());
  for (concrete_bit_offset i = 0_bits; i < conc_size; ++i)
    {
      /* TODO: this is a lot of vfuncs and sym to conc,
	 and O(n^2) e.g. on concat_bit_vector
	 We might want a bulk query interface that
	 returns a concrete_tristate_vector.  */
      tristate ts = get_tristate_at_offset (i);
      ts_vec.push_back (ts);
    }
  return maybe_set_irange_bitmask_from_concrete_tristate_vector (ts_vec,
								 out);
}

/* Return true if this bit vector is the initial value of a region,
   or derived directly from such a value (e.g. by casting or cropping).  */

bool
bit_vector::derived_from_initial_value_p () const
{
  switch (get_kind ())
    {
    default:
      return false;

    case bit_vector::kind::cropped:
      {
	auto &cropped_bv = static_cast <const cropped_bit_vector &> (*this);
	return cropped_bv
	  .get_uncropped_bit_vector ()
	  .derived_from_initial_value_p ();
      }

    case bit_vector::kind::svalue:
      {
	auto &from_sval = static_cast <const bit_vector_from_svalue &> (*this);
	return from_sval.get_svalue ().derived_from_initial_value_p ();
      }
    }
}

// class concat_bit_vector : public bit_vector

bool
concat_bit_vector::key::operator< (const key &other) const
{
  if (m_bit_vectors.size () != other.m_bit_vectors.size ())
    return m_bit_vectors.size () < other.m_bit_vectors.size ();
  for (size_t i = 0; i < m_bit_vectors.size (); ++i)
    if (m_bit_vectors[i] != other.m_bit_vectors[i])
      return m_bit_vectors[i]->get_id () < other.m_bit_vectors[i]->get_id ();
  return false;
}

complexity
concat_bit_vector::key::get_complexity () const
{
  unsigned num_child_nodes = 0;
  unsigned max_child_depth = 0;
  for (auto iter : m_bit_vectors)
    {
      const complexity &child_c = iter->get_complexity ();
      num_child_nodes += child_c.m_num_nodes;
      max_child_depth = std::max (max_child_depth, child_c.m_max_depth);
    }
  return complexity (num_child_nodes + 1, max_child_depth + 1);
}

const symbolic_bit_size &
concat_bit_vector::key::
get_size (scalar_manager<concrete_bit_size> &bit_mgr) const
{
  const symbolic_bit_size *sum = &bit_mgr.get_concrete (0_bits);
  for (auto iter : m_bit_vectors)
    sum = &(*sum + iter->get_size ());
  return *sum;
}

tristate
concat_bit_vector::get_tristate_at_offset (const symbolic_bit_offset &bit) const
{
  auto *remaining_offset = &bit;
  for (auto child : m_bit_vectors)
    {
      /* Is the bit of interest within child?
	 |    remaining_offset < child->get_size ()
	 | => (remaining_offset - child->get_size ()) < 0  */
      auto &diff = *remaining_offset - child->get_size ();
      if (auto *conc_diff = diff.dyn_cast_concrete_scalar ())
	{
	  if (conc_diff->get_concrete () < 0_bits)
	    return child->get_tristate_at_offset (*remaining_offset);
	  else
	    {
	      /* Skip this child.
		 | remaining_offset -= child_get_size
		 | ==> remaining_offset = remaining_offset - child_get_size
		 | ==> remaining_offset = diff  */
	      remaining_offset = &diff;
	      continue;
	    }
	}
      else
	{
	  /* We don't know whether we're in child or not.  */
	  return tristate::unknown ();
	}
    }
  return tristate::unknown ();
}

concat_bit_vector::concat_bit_vector (bit_vector_manager &bv_mgr,
				      const key &k)
: bit_vector (bv_mgr,
	      k.get_complexity (),
	      k.get_size (bv_mgr.get_node_manager ()
			  .get_scalars ().get_bits ())),
  m_bit_vectors (std::move (k.m_bit_vectors))
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());

  /* After folding, we shouldn't have empty or single-element
     concatentations.  */
  gcc_assert (m_bit_vectors.size () >= 2);

  if (CHECKING_P)
    for (auto iter = m_bit_vectors.begin ();
	 iter != m_bit_vectors.end ();
	 ++iter)
      {
	/* No empty children.  */
	gcc_assert (!(*iter)->get_size ().is_zero_p ());

	/* No nesting.  */
	gcc_assert (!(*iter)->dyn_cast_concat_bit_vector ());

	/* Check that we folded away consecutive children.  */
	auto next = iter;
	++next;
	if (next != m_bit_vectors.end ())
	  {
	    /* Must not have the same bit_vector appearing
	       consecutively as a child.  */
	    gcc_assert (*iter != *next);

	    /* Must not have consecutive children that are the
	       uniform_bit_vector with the same underlying bit.  */
	    if (auto *iter_poisoned = (*iter)->dyn_cast_poisoned_bit_vector ())
	      if (auto *next_poisoned = (*next)->dyn_cast_poisoned_bit_vector ())
		gcc_assert (iter_poisoned->get_poison_kind ()
			    != next_poisoned->get_poison_kind ());
	    if (auto *iter_tristate = (*iter)->dyn_cast_tristate_bit_vector ())
	      if (auto *next_tristate = (*next)->dyn_cast_tristate_bit_vector ())
		gcc_assert (iter_tristate->get_bit_value ()
			    != next_tristate->get_bit_value ());

	    /* Must not have consecutive repetitions of the same bit vector,
	       or a bit vector next to repetitions of that bit vector.  */
	    auto *iter_rep = (*iter)->dyn_cast_repeated_bit_vector ();
	    auto *next_rep = (*next)->dyn_cast_repeated_bit_vector ();
	    if (iter_rep)
	      {
		if (next_rep)
		  gcc_assert (&iter_rep->get_inner_bit_vector ()
			      != &next_rep->get_inner_bit_vector ());
		else
		  gcc_assert (&iter_rep->get_inner_bit_vector () != (*next));
	      }
	    else if (next_rep)
	      gcc_assert ((*iter) != &next_rep->get_inner_bit_vector ());

	    /* Must not have consecutive bit_vector_from_ubc taken from the
	       same UBC at consecutive positions.  */
	    if (auto *iter_from_ubc = (*iter)->dyn_cast_bit_vector_from_ubc ())
	      if (auto *next_from_ubc = (*next)->dyn_cast_bit_vector_from_ubc ())
		if (&iter_from_ubc->get_bit_contents ()
		    == &next_from_ubc->get_bit_contents ())
		  gcc_assert
		    (&iter_from_ubc->get_bit_range ().get_next_offset ()
		     != &next_from_ubc->get_bit_range ().get_start_offset ());
	  }
      }
}

void
concat_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "concat_bit_vector");
}

void
concat_bit_vector::add_dump_widget_children (text_art::tree_widget &w,
					     node_dump_state &nds) const
{
  for (auto iter : m_bit_vectors)
    w.add_child (iter->make_dump_widget (nds, nullptr));
}

void
concat_bit_vector::accept_node_visitor (node_visitor &v) const
{
  if (!v.visit_bit_vector (*this))
    return;
  for (auto iter : m_bit_vectors)
    iter->accept_node_visitor (v);
}

std::unique_ptr<json::object>
concat_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("concat_bit_vector");
  auto arr = std::make_unique<json::array> ();
  for (auto iter : m_bit_vectors)
    arr->append (iter->to_json ());
  obj->set ("bit_vectors", std::move (arr));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
concat_bit_vector::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("concat_bit_vector");
}

iter_result
concat_bit_vector::for_each_svalue (iter_result (*cb) (const svalue &sval,
						       void *user_data),
				    void *user_data) const
{
  for (auto iter : m_bit_vectors)
    if (iter->for_each_svalue (cb, user_data) == iter_result::stop)
      return iter_result::stop;
  return iter_result::keep_going;
}

bool
concat_bit_vector::implicitly_live_p (liveness_context &liveness_ctxt) const
{
  for (auto iter : m_bit_vectors)
    if (!liveness_ctxt.node_live_p (*iter))
      return false;
  return true;
}

/* crop of concat e.g.:
   concat(X1, ... Xn)[a, b] --> concat(X1[...], ..., Xn[...])
*/
const bit_vector *
concat_bit_vector::
maybe_fold_crop (const symbolic_bit_range &sym_inner_bits_to_use) const
{
  concrete_bit_range conc_inner_bits_to_use;
  if (!concrete_from_symbolic (sym_inner_bits_to_use, &conc_inner_bits_to_use))
    return nullptr;

  std::vector<const bit_vector *> vec_of_cropped;
  concrete_bit_size leading_bits_to_ignore
    = conc_inner_bits_to_use.get_start_offset ();
  concrete_bit_size remaining_bits_to_add
    = conc_inner_bits_to_use.get_size ();
  for (auto iter : *this)
    {
      auto &iter_size = iter->get_size ();
      concrete_bit_size conc_iter_size;
      if (!concrete_from_symbolic (iter_size, &conc_iter_size))
	return nullptr;

      concrete_bit_size bits_to_use
	= std::min (conc_iter_size - leading_bits_to_ignore,
		    remaining_bits_to_add);
      if (bits_to_use > 0_bits)
	{
	  auto &cropped_iter = iter->get_cropped_bits
	    (concrete_bit_range (leading_bits_to_ignore,
				 bits_to_use));
	  vec_of_cropped.push_back (&cropped_iter);
	  leading_bits_to_ignore = 0_bits;
	  remaining_bits_to_add -= bits_to_use;
	}
      else
	leading_bits_to_ignore -= conc_iter_size;

      if (remaining_bits_to_add <= 0_bits)
	break;
    }

  return &m_bv_mgr.get_concat_bits (std::move (vec_of_cropped));
}

// class conjured_bit_vector : public bit_vector

bool
conjured_bit_vector::key::operator< (const key &other) const
{
  if (int c = m_stmt->uid - other.m_stmt->uid)
    return c < 0;
  if (int c = node::cmp (*m_id_reg, *other.m_id_reg))
    return c < 0;
  return m_idx < other.m_idx;
}

complexity
conjured_bit_vector::key::get_complexity () const
{
  return complexity (m_id_reg);
}

const symbolic_bit_size &
conjured_bit_vector::key::get_size (scalar_manager<concrete_bit_size> &bit_mgr) const
{
  auto size = m_id_reg->maybe_get_bit_size (bit_mgr);
  if (!size)
    size = &bit_mgr.get_unknown ();
  return *size;
}

tristate
conjured_bit_vector::get_tristate_at_offset (const symbolic_bit_offset &) const
{
  return tristate::unknown ();
}

conjured_bit_vector::conjured_bit_vector (bit_vector_manager &bv_mgr,
					  const key &k)
: bit_vector (bv_mgr,
	      k.get_complexity (),
	      k.get_size (bv_mgr.get_node_manager ()
			  .get_scalars ().get_bits ())),
  m_stmt (k.m_stmt),
  m_id_reg (k.m_id_reg),
  m_idx (k.m_idx)
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());
}

void
conjured_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "conjured_bit_vector (");
  pp_gimple_stmt_1 (pp, m_stmt, 0, (dump_flags_t)0);
  if (m_idx != 0)
    pp_printf (pp, ", %i", m_idx);
  pp_character (pp, ')');
}

void
conjured_bit_vector::add_dump_widget_children (text_art::tree_widget &w,
					       node_dump_state &nds) const
{
  w.add_child (m_id_reg->make_dump_widget (nds, "id_reg"));
}

void
conjured_bit_vector::accept_node_visitor (node_visitor &v) const
{
  if (!v.visit_bit_vector (*this))
    return;
  m_id_reg->accept_node_visitor (v);
}

std::unique_ptr<json::object>
conjured_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("conjured_bit_vector");

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
conjured_bit_vector::make_markup_node (markup_context &markup_ctxt) const
{
  if (markup_ctxt.m_user_facing)
    return m_id_reg->make_markup_node (markup_ctxt);

  auto markup = std::make_unique<diagnostics::markup::compound_node> ();
  markup->add_child ("conjured_bit_vector(");
  markup->add_child (")");
  return markup;
}

iter_result
conjured_bit_vector::for_each_svalue (iter_result (*) (const svalue &sval,
							 void *user_data),
				      void *) const
{
  return iter_result::keep_going;
}

// class cropped_bit_vector : public bit_vector

bool
cropped_bit_vector::key::operator< (const key &other) const
{
  if (int c = node::cmp (*m_uncropped_bv,
			 *other.m_uncropped_bv))
    return c < 0;
  return symbolic_bit_range::cmp (m_bits_to_use, other.m_bits_to_use) < 0;
}

tristate
cropped_bit_vector::
get_tristate_at_offset (const symbolic_bit_offset &bit) const
{
  return m_uncropped_bv.get_tristate_at_offset
    (m_crop_start_bit_offset + bit);
}

cropped_bit_vector::cropped_bit_vector (bit_vector_manager &bv_mgr,
					const key &k)
: bit_vector (bv_mgr,
	      complexity (*k.m_uncropped_bv),
	      k.get_size ()),
  m_uncropped_bv (*k.m_uncropped_bv),
  m_crop_start_bit_offset (k.m_bits_to_use.get_start_offset ())
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());

  /* No nesting.  */
  gcc_assert (!m_uncropped_bv.dyn_cast_cropped_bit_vector ());

  /* Cropping of a uniform_bit_vector should have been folded
     before now.  */
  gcc_assert (!m_uncropped_bv.dyn_cast_uniform_bit_vector ());
}

void
cropped_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "cropped_bit_vector");
}

void
cropped_bit_vector::
print_any_suffix_for_dump_widget_label (pretty_printer *pp) const
{
  concrete_bit_range conc_bits;
  if (concrete_from_symbolic (get_uncropped_bits_to_use (), &conc_bits))
    {
      pp_string (pp, ": ");
      end_any_dump_color (pp);
      m_size_in_bits.start_any_dump_color (pp);
      conc_bits.dump_to_pp_pretty (pp);
    }
}

void
cropped_bit_vector::add_dump_widget_children (text_art::tree_widget &w,
					      node_dump_state &nds) const
{
  w.add_child (m_uncropped_bv.make_dump_widget (nds, "uncropped"));
  concrete_bit_range conc_bits;
  if (!concrete_from_symbolic (get_uncropped_bits_to_use (), &conc_bits))
    w.add_child (get_uncropped_bits_to_use ().make_dump_widget (nds,
								"bits to use"));
}

void
cropped_bit_vector::accept_node_visitor (node_visitor &v) const
{
  if (!v.visit_bit_vector (*this))
    return;
  m_uncropped_bv.accept_node_visitor (v);
}

std::unique_ptr<json::object>
cropped_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("cropped_bit_vector");
  obj->set ("crop_start_bit_offset", m_crop_start_bit_offset.to_json ());
  obj->set ("uncropped_bv", m_uncropped_bv.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
cropped_bit_vector::make_markup_node (markup_context &markup_ctxt) const
{
  auto result = std::make_unique<diagnostics::markup::compound_node> ();
  result->add_child ("cropped_bit_vector (start_bit_offset=");
  result->add_child (m_crop_start_bit_offset.make_markup_node (markup_ctxt));
  result->add_child (", uncropped_bv=");
  result->add_child (m_uncropped_bv.make_markup_node (markup_ctxt));
  result->add_child (")");
  return result;
}

iter_result
cropped_bit_vector::for_each_svalue (iter_result (*) (const svalue &sval,
						      void *user_data),
				     void *) const
{
  return iter_result::keep_going;
}

bool
cropped_bit_vector::implicitly_live_p (liveness_context &liveness_ctxt) const
{
  return m_uncropped_bv.implicitly_live_p (liveness_ctxt);
}

/* Crop of crop: A[m..n][p..q] --> A[m + n .. m + n + sizeof(p..q)].  */

const bit_vector *
cropped_bit_vector::
maybe_fold_crop (const symbolic_bit_range &bits_to_use) const
{
  symbolic_bit_range bit_range_relative_to_grandchild
    ((get_uncropped_bits_to_use ().get_start_offset ()
      + bits_to_use.get_start_offset ()),
     bits_to_use.get_size ());
  return &m_bv_mgr.get_cropped_bits (get_uncropped_bit_vector (),
				     bit_range_relative_to_grandchild);
}

// class named_test_bit_vector : public bit_vector

#if CHECKING_P

tristate
named_test_bit_vector::
get_tristate_at_offset (const symbolic_bit_offset &) const
{
  return tristate::unknown ();
}

named_test_bit_vector::
named_test_bit_vector (bit_vector_manager &bv_mgr,
		       const symbolic_bit_size &size_in_bits,
		       std::string label)
: bit_vector (bv_mgr, complexity (1, 1), size_in_bits),
  m_label (std::move (label))
{
}

void
named_test_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "named_test_bit_vector");
}

void
named_test_bit_vector::
print_any_suffix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, ": %qs", m_label.c_str ());
}

void
named_test_bit_vector::add_dump_widget_children (text_art::tree_widget &,
						 node_dump_state &) const
{
  // no-op
}

void
named_test_bit_vector::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_vector (*this);
}

std::unique_ptr<json::object>
named_test_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("named_test_bit_vector");
  obj->set_string ("label", m_label);
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
named_test_bit_vector::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node (m_label);
}

iter_result
named_test_bit_vector::for_each_svalue (iter_result (*) (const svalue &sval,
							 void *user_data),
					void *) const
{
  return iter_result::keep_going;
}

#endif /* #if CHECKING_P */

// class empty_bit_vector : public bit_vector

tristate
empty_bit_vector::get_tristate_at_offset (const symbolic_bit_offset &) const
{
  /* This should never be called; there are no bits to query.  */
  gcc_unreachable ();
}

empty_bit_vector::empty_bit_vector (bit_vector_manager &bv_mgr)
: uniform_bit_vector (bv_mgr,
		      complexity (1, 1),
		      (bv_mgr.get_node_manager ()
		       .get_scalars ().get_bits ().get_concrete (0_bits)))
{
}

void
empty_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "empty_bit_vector");
}

void
empty_bit_vector::add_dump_widget_children (text_art::tree_widget &,
					    node_dump_state &) const
{
  // No-op
}

void
empty_bit_vector::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_vector (*this);
}

std::unique_ptr<json::object>
empty_bit_vector::to_json () const
{
  return bit_vector::make_json_obj ("empty_bit_vector");
}

std::unique_ptr<diagnostics::markup::node>
empty_bit_vector::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("empty_bit_vector");
}

iter_result
empty_bit_vector::for_each_svalue (iter_result (*) (const svalue &sval,
						    void *user_data),
				   void *) const
{
  return iter_result::keep_going;
}

// class poisoned_bit_vector : public bit_vector

bool
poisoned_bit_vector::key::operator< (const key &other) const
{
  if (int c = node::cmp (m_size_in_bits, other.m_size_in_bits))
    return c < 0;
  return (int)m_poison_kind < (int)other.m_poison_kind;
}

tristate
poisoned_bit_vector::get_tristate_at_offset (const symbolic_bit_offset &) const
{
  return tristate::unknown ();
}

poisoned_bit_vector::poisoned_bit_vector (bit_vector_manager &bv_mgr,
					  const key &k)
: uniform_bit_vector (bv_mgr,
		      complexity (1, 1),
		      k.m_size_in_bits),
  m_poison_kind (k.m_poison_kind)
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());
}

void
poisoned_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "poisoned_bit_vector");
}

void
poisoned_bit_vector::
print_any_suffix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, ": %s", poison_kind_to_str (m_poison_kind));
}

void
poisoned_bit_vector::add_dump_widget_children (text_art::tree_widget &,
					       node_dump_state &) const
{
  // no-op
}

void
poisoned_bit_vector::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_vector (*this);
}

std::unique_ptr<json::object>
poisoned_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("poisoned_bit_vector");
  obj->set_string ("poison_kind", poison_kind_to_str (m_poison_kind));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
poisoned_bit_vector::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("poisoned_bit_vector");
}

iter_result
poisoned_bit_vector::for_each_svalue (iter_result (*) (const svalue &sval,
						       void *user_data),
				      void *) const
{
  return iter_result::keep_going;
}

/* poisoned(x)[m..n] --> poisoned(x, sizeof(m..n)).  */

const bit_vector *
poisoned_bit_vector::
maybe_fold_crop (const symbolic_bit_range &bits_to_use) const
{
  return &m_bv_mgr.get_poisoned_bits (bits_to_use.get_size (),
				      get_poison_kind ());
}

// class repeated_bit_vector : public bit_vector

bool
repeated_bit_vector::key::operator< (const key &other) const
{
  if (m_inner_bv != other.m_inner_bv)
    return m_inner_bv->get_id () < other.m_inner_bv->get_id ();
  return m_repeat_count < other.m_repeat_count;
}

repeated_bit_vector::repeated_bit_vector (bit_vector_manager &bv_mgr,
					  const key &k)
: bit_vector (bv_mgr,
	      complexity (*k.m_inner_bv),
	      k.get_size ()),
  m_inner_bv (*k.m_inner_bv),
  m_repeat_count (*k.m_repeat_count)
{
  gcc_assert (!m_repeat_count.is_zero_p ());
  gcc_assert (!m_repeat_count.is_one_p ());

  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());

  /* No nesting.  */
  gcc_assert (!m_inner_bv.dyn_cast_repeated_bit_vector ());

  /* Repetition of a uniform_bit_vector should have been folded
     before now.  */
  gcc_assert (!m_inner_bv.dyn_cast_uniform_bit_vector ());
}

void
repeated_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "repeated_bit_vector");
}

void
repeated_bit_vector::add_dump_widget_children (text_art::tree_widget &w,
					       node_dump_state &nds) const
{
  w.add_child (m_inner_bv.make_dump_widget (nds, "m_inner_bv"));
  w.add_child (m_repeat_count.make_dump_widget (nds, "m_repeat_count"));
}

void
repeated_bit_vector::accept_node_visitor (node_visitor &v) const
{
  if (!v.visit_bit_vector (*this))
    return;
  m_inner_bv.accept_node_visitor (v);
}

std::unique_ptr<json::object>
repeated_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("repeated_bit_vector");
  obj->set ("inner_bv", m_inner_bv.to_json ());
  obj->set ("repeat_count", m_repeat_count.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
repeated_bit_vector::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("repeated_bit_vector");
}

iter_result
repeated_bit_vector::for_each_svalue (iter_result (*cb) (const svalue &sval,
						       void *user_data),
				    void *user_data) const
{
  return m_inner_bv.for_each_svalue (cb, user_data);
}

const bit_vector *
repeated_bit_vector::
maybe_fold_crop (const symbolic_bit_range &bits_to_use) const
{
  concrete_bit_range conc_bits_to_use;
  if (!concrete_from_symbolic (bits_to_use, &conc_bits_to_use))
    return nullptr;

  concrete_bit_size conc_bit_size;
  if (!concrete_from_symbolic (get_size (), &conc_bit_size))
    return nullptr;

  concrete_bit_size conc_inner_bit_size;
  if (!concrete_from_symbolic (m_inner_bv.get_size (),
			       &conc_inner_bit_size))
    return nullptr;

  concrete_bit_size remaining_bits_to_add = conc_bits_to_use.get_size ();

  std::vector<const bit_vector *> vec_of_cropped;

  /* Do we have a leading fragment from one of the iterations?  */
  concrete_bit_offset bit_offset_within_repeat
    = conc_bits_to_use.get_start_offset ().floor_mod (conc_inner_bit_size);
  if (bit_offset_within_repeat > 0_bits)
    {
      concrete_bit_size leading_size
	= std::min (conc_inner_bit_size - bit_offset_within_repeat,
		    remaining_bits_to_add);
      auto &leading_suffix = m_inner_bv.get_cropped_bits
	(concrete_bit_range (bit_offset_within_repeat,
			     leading_size));
      vec_of_cropped.push_back (&leading_suffix);
      remaining_bits_to_add -= leading_size;
    }

  /* Do we have any full iterations of the inner bv?  */
  integer num_full_repeats
    = remaining_bits_to_add.floor_div (conc_inner_bit_size);
  if (num_full_repeats > 0)
    {
      auto &full_repeats = m_bv_mgr.get_repeated_bits (m_inner_bv,
						       num_full_repeats);
      vec_of_cropped.push_back (&full_repeats);
      remaining_bits_to_add -= (conc_inner_bit_size * num_full_repeats);
    }

  /* Do we have a trailing suffix from one of the iterations?  */
  if (remaining_bits_to_add > 0_bits)
    {
      gcc_assert (remaining_bits_to_add < conc_inner_bit_size);
      auto &trailing_prefix
	= m_inner_bv.get_prefix (remaining_bits_to_add);
      vec_of_cropped.push_back (&trailing_prefix);
    }

  return &m_bv_mgr.get_concat_bits (std::move (vec_of_cropped));
}

// class bit_vector_from_svalue : public bit_vector

bool
bit_vector_from_svalue::key::operator< (const key &other) const
{
  if (m_sval != other.m_sval)
    return m_sval->get_id () < other.m_sval->get_id ();
  return false;
}

const symbolic_bit_size &
bit_vector_from_svalue::key::
get_size (scalar_manager<concrete_bit_size> &bit_mgr) const
{
  concrete_bit_size conc_size;
  bool has_size = m_sval->has_concrete_size_p (&conc_size,
					       bit_mgr);
  gcc_assert (has_size);
  return bit_mgr.get_concrete (conc_size);
}

tristate
bit_vector_from_svalue::
get_tristate_at_offset (const symbolic_bit_offset &bit) const
{
  auto &bit_mgr = bit.get_mgr ();
  auto &cropped_bit
    = get_cropped_bits (symbolic_bit_range (bit, bit_mgr.get_concrete (1_bit)));
  if (auto *tristate_bit = cropped_bit.dyn_cast_tristate_bit_vector ())
    return tristate_bit->get_bit_value ();

  return tristate::unknown ();
}

bit_vector_from_svalue::bit_vector_from_svalue (bit_vector_manager &bv_mgr,
						const key &k)
: bit_vector (bv_mgr,
	      complexity (*k.m_sval),
	      k.get_size (bv_mgr.get_node_manager ()
			  .get_scalars ().get_bits ())),
  m_sval (*k.m_sval)
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());

  // No nesting
  gcc_assert (!m_sval.dyn_cast_svalue_from_bit_vector ());
}

void
bit_vector_from_svalue::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "bit_vector_from_svalue");
}

void
bit_vector_from_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  node_dump_state &nds) const
{
  w.add_child (m_sval.make_dump_widget (nds, "m_sval"));
}

bool
bit_vector_from_svalue::
maybe_print_for_user (pretty_printer *pp,
		      const region_model &model,
		      const symbolic::node *outer_node) const
{
  return m_sval.maybe_print_for_user (pp, model, nullptr);
}

void
bit_vector_from_svalue::accept_node_visitor (node_visitor &v) const
{
  if (!v.visit_bit_vector (*this))
    return;
  m_sval.accept_node_visitor (v);
}

std::unique_ptr<json::object>
bit_vector_from_svalue::to_json () const
{
  auto obj = bit_vector::make_json_obj ("bit_vector_from_svalue");
  obj->set ("sval", m_sval.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
bit_vector_from_svalue::make_markup_node (markup_context &markup_ctxt) const
{
  return m_sval.make_markup_node (markup_ctxt);
}

iter_result
bit_vector_from_svalue::for_each_svalue (iter_result (*cb) (const svalue &sval,
							    void *user_data),
					 void *user_data) const
{
  return m_sval.for_each_svalue (cb, user_data);
}

const bit_vector *
bit_vector_from_svalue::
maybe_fold_crop (const symbolic_bit_range &bits_to_use) const
{
  concrete_bit_range conc_bits_to_use;
  if (!concrete_from_symbolic (bits_to_use, &conc_bits_to_use))
    return nullptr;

  if (auto *folded = maybe_fold_crop_from_cst (conc_bits_to_use))
    return folded;

  if (auto *folded = maybe_fold_crop_from_bitmask (conc_bits_to_use))
    return folded;

  return nullptr;
}

const bit_vector *
bit_vector_from_svalue::
maybe_fold_crop_from_cst (const concrete_bit_range &inner_bits_to_use) const
{
  /* Attempt to extract bytes from constants.  */
  tree tree_cst  = m_sval.maybe_get_constant ();
  if (!tree_cst)
    if (auto *init_sval = m_sval.dyn_cast_initial_svalue ())
      if (auto *string_reg
	  = init_sval->get_region ()->dyn_cast_string_region ())
	tree_cst = string_reg->get_string_cst ();
  if (!tree_cst)
    return nullptr;

  concrete_byte_range conc_bytes_to_use;
  concrete_bit_offset bit_offset;
  get_smallest_enclosing_byte_range (inner_bits_to_use,
				     &conc_bytes_to_use,
				     &bit_offset);
  gcc_assert (bit_offset < 8_bits);
  if (auto p = m_bv_mgr.maybe_fold_bytes_within_constant (tree_cst,
						       conc_bytes_to_use))
    {
      auto effective_size = inner_bits_to_use.get_size ();
      if (p == this)
	{
	  /* We're looking at bits within one of the m_cached_bytes
	     instances; we need to avoid infinite recursion.  */
	  gcc_assert (TREE_TYPE (tree_cst) == unsigned_char_type_node);
	  gcc_assert (effective_size < 8_bits);
	  int byte_val = TREE_INT_CST_LOW (tree_cst);
	  gcc_assert (this == &m_bv_mgr.get_uint8 (byte_val));
	  std::vector<const symbolic::bit_vector *> raw_bits;
	  int idx_after_bits
	    = bit_offset.as_long () + effective_size.as_long ();
	  for (int bit_idx = bit_offset.as_long ();
	       bit_idx < idx_after_bits; ++bit_idx)
	    {
	      /* Add in LSB through MSB order.  */
	      bool bit_val = byte_val & (1 << bit_idx);
	      raw_bits.push_back
		(&m_bv_mgr.get_tristate_bits (1_bit, tristate (bit_val)));
	    }
	  return &m_bv_mgr.get_concat_bits (std::move (raw_bits));
	}
      else
	return &p->get_cropped_bits
	  (concrete_bit_range (bit_offset, effective_size));
    }

  return nullptr;
}

const bit_vector *
bit_vector_from_svalue::
maybe_fold_crop_from_bitmask (const concrete_bit_range &inner_bits_to_use) const
{
  if (inner_bits_to_use.get_next_offset () > 64_bits)
    return nullptr;

  value_range vr;
  if (!m_sval.maybe_get_value_range (vr))
    return nullptr;

  irange_bitmask bitmask = vr.get_bitmask ();

  /* The unknown bits are those for which the mask is one.
     Bail out if we have any unknown bits in the bit_range of interest.  */
  unsigned HOST_WIDE_INT mask_for_bits
    = wi::extract_uhwi
	(bitmask.mask (),
	 inner_bits_to_use.get_start_offset ().as_unsigned_long (),
	 inner_bits_to_use.get_size ().as_unsigned_long ());
  if (mask_for_bits != 0)
    return nullptr;

  // We have known bits; get their value.
  unsigned HOST_WIDE_INT known_value
    = wi::extract_uhwi
	(bitmask.value (),
	 inner_bits_to_use.get_start_offset ().as_unsigned_long (),
	 inner_bits_to_use.get_size ().as_unsigned_long ());
  if (inner_bits_to_use.get_size ().is_one_p ())
    return &m_bv_mgr.get_tristate_bits (1_bit, known_value != 0);
  else if (inner_bits_to_use.get_size () == 8_bits)
    return &m_bv_mgr.get_uint8 (known_value);
  else
    return nullptr;
}

bool
bit_vector_from_svalue::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  return liveness_ctxt.node_live_p (m_sval);
}

// class bit_vector_from_ubc : public bit_vector

tristate
bit_vector_from_ubc::
get_tristate_at_offset (const symbolic_bit_offset &) const
{
  return tristate::unknown ();
}

bit_vector_from_ubc::
bit_vector_from_ubc (const unfoldable_bit_contents &bc,
		     symbolic_bit_range bit_range)
: bit_vector (bc.get_node_mgr ().get_bit_vector_manager (),
	      complexity::from_pair
		(bc.get_complexity (),
		 bit_range.get_children_complexity ()),
	      // FIXME: what about symbolic?
	      bit_range.get_size ()),
  m_unfoldable_bit_contents (bc),
  m_bit_range (bit_range)
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());
}

void
bit_vector_from_ubc::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "bit_vector_from_ubc");
}

void
bit_vector_from_ubc::
add_dump_widget_children (text_art::tree_widget &w,
			  node_dump_state &nds) const
{
  w.add_child
    (m_unfoldable_bit_contents.make_dump_widget (nds, "bit_contents"));
  w.add_child (m_bit_range.make_dump_widget (nds, "range"));
}

void
bit_vector_from_ubc::accept_node_visitor (node_visitor &v) const
{
  if (!v.visit_bit_vector (*this))
    return;
  m_unfoldable_bit_contents.accept_node_visitor (v);
  m_bit_range.accept_node_visitor (v);
}

bool
bit_vector_from_ubc::conjured_p () const
{
  return m_unfoldable_bit_contents.conjured_p ();
}

std::unique_ptr<json::object>
bit_vector_from_ubc::to_json () const
{
  auto obj
    = bit_vector::make_json_obj ("bit_vector_from_ubc");
  obj->set ("bit_contents", m_unfoldable_bit_contents.to_json ());
  obj->set ("bit_range", m_bit_range.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
bit_vector_from_ubc::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("bit_vector_from_ubc");
}

iter_result
bit_vector_from_ubc::for_each_svalue (iter_result (*) (const svalue &sval,
						       void *user_data),
				      void *) const
{
  return iter_result::keep_going;
}

bool
bit_vector_from_ubc::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  return liveness_ctxt.bits_live_p (*this);
}

/* crop(bit_vector_from_ubc (UBC, RANGE), SUBRANGE)
   --> bit_vector_from_ubc (UBC, SUBRANGE OF RANGE).  */

const bit_vector *
bit_vector_from_ubc::
maybe_fold_crop (const symbolic_bit_range &bits_to_use) const
{
  auto &result_size = bits_to_use.get_size ();
  symbolic_bit_range subrange
    (get_bit_range ().get_start_offset ()
     + bits_to_use.get_start_offset (),
     result_size);
  return &get_bit_contents ().get_bits (subrange);
}

// class tristate_bit_vector : public bit_vector

bool
tristate_bit_vector::key::operator< (const key &other) const
{
  if (int c = node::cmp (m_size_in_bits, other.m_size_in_bits))
    return c < 0;
  return (static_cast<int> (m_bit_value.get_value ())
	  < static_cast<int> (other.m_bit_value.get_value ()));
}

tristate
tristate_bit_vector::get_tristate_at_offset (const symbolic_bit_offset &) const
{
  return m_bit_value;
}

tristate_bit_vector::tristate_bit_vector (bit_vector_manager &bv_mgr,
					  const key &k)
: uniform_bit_vector (bv_mgr, complexity (1, 1), k.m_size_in_bits),
  m_bit_value (k.m_bit_value)
{
  /* The only empty bit_vector should be the singleton empty_bit_vector.  */
  gcc_assert (!get_size ().is_zero_p ());
}

void
tristate_bit_vector::
print_prefix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "tristate_bit_vector");
}

void
tristate_bit_vector::
print_any_suffix_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, ": ");
  if (m_size_in_bits.is_one_p ())
    switch (m_bit_value.get_value ())
      {
      default:
	gcc_unreachable ();
      case tristate::TS_UNKNOWN:
	pp_string (pp, "unknown bit");
	break;
      case tristate::TS_TRUE:
	pp_string (pp, "true bit");
	break;
      case tristate::TS_FALSE:
	pp_string (pp, "false bit");
	break;
      }
  else
    switch (m_bit_value.get_value ())
      {
      default:
	gcc_unreachable ();
      case tristate::TS_UNKNOWN:
	pp_string (pp, "unknown bits");
	break;
      case tristate::TS_TRUE:
	pp_string (pp, "true bits");
	break;
      case tristate::TS_FALSE:
	pp_string (pp, "false bits");
	break;
      }
}

void
tristate_bit_vector::add_dump_widget_children (text_art::tree_widget &,
					       node_dump_state &) const
{
  // no-op
}

void
tristate_bit_vector::accept_node_visitor (node_visitor &v) const
{
  v.visit_bit_vector (*this);
}

std::unique_ptr<json::object>
tristate_bit_vector::to_json () const
{
  auto obj = bit_vector::make_json_obj ("tristate_bit_vector");
  obj->set_string ("bit_value", m_bit_value.as_string ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
tristate_bit_vector::make_markup_node (markup_context &) const
{
  return diagnostics::markup::make_text_node ("tristate_bit_vector");
}

iter_result
tristate_bit_vector::for_each_svalue (iter_result (*) (const svalue &sval,
						       void *user_data),
				      void *) const
{
  return iter_result::keep_going;
}

/* tristate(x)[m..n] --> tristate(x, sizeof(m..n)).  */

const bit_vector *
tristate_bit_vector::
maybe_fold_crop (const symbolic_bit_range &bits_to_use) const
{
  return &m_bv_mgr.get_tristate_bits (bits_to_use.get_size (),
				      get_bit_value ());
}

/* symbolic::bit_vector consolidation.  */

bit_vector_manager::bit_vector_manager (node_manager &node_mgr)
: m_node_mgr (node_mgr),
  m_empty_bv (*this)
{
  // Populate m_cached_bytes
  memset (m_cached_bytes, 0, sizeof (m_cached_bytes));
  m_cached_bytes[0x00] = &get_all_zeros (1_byte);
  for (int i = 1; i < 255; ++i)
    {
      auto &rmm = node_mgr.get_region_model_manager ();
      auto &sval = *rmm.get_or_create_int_cst (unsigned_char_type_node, i);
      m_cached_bytes[i] = &get_bit_vector_from_svalue (sval);
    }
  m_cached_bytes[0xff] = &get_all_ones (1_byte);
}


const bit_vector &
bit_vector_manager::get_tristate_bits (const symbolic_bit_size &num_bits,
				       tristate bit_val)
{
  if (num_bits.is_zero_p ())
    return m_empty_bv;

  const symbolic::tristate_bit_vector::key k {num_bits, bit_val};
  auto iter = m_tristate_map.find (k);
  if (iter != m_tristate_map.end ())
    return *(iter->second);
  auto bv_up = std::make_unique<symbolic::tristate_bit_vector> (*this, k);
  auto &result = *bv_up.get ();
  m_tristate_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector &
bit_vector_manager::get_tristate_bits (const concrete_bit_size &num_bits,
				       tristate bit_val)
{
  return get_tristate_bits (symbolic_from_concrete (num_bits,
						    get_bit_manager ()),
			    bit_val);
}

  // Unknown bits of unknown length
const bit_vector &
bit_vector_manager::get_unknown_bits ()
{
  const symbolic_bit_size &unknown_size = get_bit_manager ().get_unknown ();
  return get_tristate_bits (unknown_size, tristate::unknown ());
}

static void
append_canonically (std::vector<const bit_vector *> &canonical_bvs,
		    const bit_vector &bv_to_add,
		    bit_vector_manager &bv_mgr)
{
  if (bv_to_add.get_size ().is_zero_p ())
    return;

  /* Flatten adding concat bit_vectors.  */
  if (auto concat_bv = bv_to_add.dyn_cast_concat_bit_vector ())
    {
      for (auto iter : *concat_bv)
	append_canonically (canonical_bvs, *iter, bv_mgr);
      return;
    }

  if (!canonical_bvs.empty ())
    {
      /* Check for merging opportunities.  */
      auto &back = canonical_bvs.back ();
      gcc_assert (back);

      /* A + A --> A * 2  */
      if (&bv_to_add == back)
	{
	  back = &bv_mgr.get_repeated_bits (bv_to_add, 2);
	  return;
	}

      /* (A * n) + A --> A * (n + 1).  */
      if (auto repeated_bv = back->dyn_cast_repeated_bit_vector ())
	if (&repeated_bv->get_inner_bit_vector () == &bv_to_add)
	  {
	    auto &unitless_mgr
	      = bv_mgr.get_node_manager ().get_scalars ().get_unitless ();
	    back = &bv_mgr.get_repeated_bits
	      (repeated_bv->get_inner_bit_vector (),
	       (repeated_bv->get_repeat_count ()
		+ unitless_mgr.get_concrete (1)));
	    return;
	  }

      /* A + (A * n) --> A * (n + 1).  */
      if (auto repeated_bv = bv_to_add.dyn_cast_repeated_bit_vector ())
	if (back == &repeated_bv->get_inner_bit_vector ())
	  {
	    auto &unitless_mgr
	      = bv_mgr.get_node_manager ().get_scalars ().get_unitless ();
	    back = &bv_mgr.get_repeated_bits
	      (repeated_bv->get_inner_bit_vector (),
	       (repeated_bv->get_repeat_count ()
		+ unitless_mgr.get_concrete (1)));
	    return;
	  }

      /* (A * m) + (A * n) --> A * (m + n).  */
      if (auto repeated_lhs = back->dyn_cast_repeated_bit_vector ())
	if (auto repeated_rhs = bv_to_add.dyn_cast_repeated_bit_vector ())
	  if (&repeated_lhs->get_inner_bit_vector ()
	      == &repeated_rhs->get_inner_bit_vector ())
	    {
	      back = &bv_mgr.get_repeated_bits
		(repeated_lhs->get_inner_bit_vector (),
		 (repeated_lhs->get_repeat_count ()
		  + repeated_rhs->get_repeat_count ()));
	      return;
	    }

      /* concat(A[p, q], A[q, r]) --> A[p, r].  */
      if (auto cropped_lhs = back->dyn_cast_cropped_bit_vector ())
	if (auto cropped_rhs = bv_to_add.dyn_cast_cropped_bit_vector ())
	  if ((&cropped_lhs->get_uncropped_bit_vector ()
	       == &cropped_rhs->get_uncropped_bit_vector ())
	      && (&cropped_lhs->get_uncropped_bits_to_use ().get_next_offset ()
		  == &cropped_rhs->get_uncropped_bits_to_use ().get_start_offset ()))
	    {
	      symbolic_bit_range concat_bit_range
		(cropped_lhs->get_uncropped_bits_to_use ().get_start_offset (),
		 cropped_lhs->get_size () + cropped_rhs->get_size ());
	      back = &bv_mgr.get_cropped_bits (cropped_lhs->get_uncropped_bit_vector (),
					       concat_bit_range);
	      return;
	    }

      /* poisoned(m, X) + poisoned(n, X) --> poisoned(m + n, X)
	 for same kind of poison.  */
      if (auto poisoned_lhs = back->dyn_cast_poisoned_bit_vector ())
	if (auto poisoned_rhs = bv_to_add.dyn_cast_poisoned_bit_vector ())
	  if (poisoned_lhs->get_poison_kind () == poisoned_rhs->get_poison_kind ())
	    {
	      back = &bv_mgr.get_poisoned_bits
		((poisoned_lhs->get_size ()
		  + poisoned_rhs->get_size ()),
		 poisoned_lhs->get_poison_kind ());
	      return;
	    }

      /* tristate(m, X) + tristate(n, X) --> tristate(m + n, X).  */
      if (auto tristate_lhs = back->dyn_cast_tristate_bit_vector ())
	if (auto tristate_rhs = bv_to_add.dyn_cast_tristate_bit_vector ())
	  if (tristate_lhs->get_bit_value () == tristate_rhs->get_bit_value ())
	    {
	      back = &bv_mgr.get_tristate_bits
		((tristate_lhs->get_size ()
		  + tristate_rhs->get_size ()),
		 tristate_lhs->get_bit_value ());
	      return;
	    }

      /* bits_from_ubc (BC, R1) + bits_from_ubc (BC, R2)
	 --> bits_from_ubc (BC, R1 + R2)
	 if it's the same BC and the ranges touch.  */
      if (auto from_ubc_lhs = back->dyn_cast_bit_vector_from_ubc ())
	if (auto from_ubc_rhs = bv_to_add.dyn_cast_bit_vector_from_ubc ())
	  if (&from_ubc_lhs->get_bit_contents ()
	      == &from_ubc_rhs->get_bit_contents ())
	    if (&from_ubc_lhs->get_bit_range ().get_next_offset ()
		== &from_ubc_rhs->get_bit_range ().get_start_offset ())
	      {
		symbolic_bit_range combined_bit_range
		  (from_ubc_lhs->get_bit_range ().get_start_offset (),
		   (from_ubc_lhs->get_bit_range ().get_size ()
		    + from_ubc_rhs->get_bit_range ().get_size ()));
		auto &ubc = from_ubc_lhs->get_bit_contents ();
		back = &ubc.get_bits (combined_bit_range);
		return;
	      }
    }

  canonical_bvs.push_back (&bv_to_add);
}

/* Simplify BIT_VECTORS into a canonical form.  */

std::vector<const bit_vector *>
bit_vector_manager::
canonicalize_concat_bits (const std::vector<const bit_vector *> &bvs)
{
  std::vector<const bit_vector *> result;
  for (auto iter : bvs)
    append_canonically (result, *iter, *this);
  return result;
}

const bit_vector *
bit_vector_manager::
maybe_fold_concat_bits (const std::vector<const bit_vector *> &canonical_bvs)
{
  /* Unwrap single element: concat(X) --> X.  */
  if (canonical_bvs.size () == 1)
    return canonical_bvs[0];

  return nullptr;
}

const bit_vector &
bit_vector_manager::
get_concat_bits (std::vector<const bit_vector *> bvs)
{
  if (bvs.size () == 0)
    return m_empty_bv;

  /* Simplify into a canonical form.  */
  auto canonical_bvs = canonicalize_concat_bits (bvs);
  if (canonical_bvs.size () == 0)
    return m_empty_bv;

  if (auto *folded = maybe_fold_concat_bits (canonical_bvs))
    return *folded;

  const symbolic::concat_bit_vector::key k (std::move (canonical_bvs));
  auto iter = m_concat_map.find (k);
  if (iter != m_concat_map.end ())
    return *(iter->second);

  auto bv_up = std::make_unique<symbolic::concat_bit_vector> (*this, k);
  auto &result = *bv_up.get ();
  m_concat_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector &
bit_vector_manager::get_conjured_bits (const gimple *stmt,
				       const region *id_reg,
				       const conjured_purge &p,
				       unsigned idx)
{
  const symbolic::conjured_bit_vector::key k (stmt, id_reg, idx);
  auto iter = m_conjured_map.find (k);
  if (iter != m_conjured_map.end ())
    {
      auto &bv = iter->second;
      /* We're reusing an existing conjured_bit_vector, perhaps from a different
	 state within this analysis, or perhaps from an earlier state on this
	 execution path.  For the latter, purge any state involving the "new"
	 bit_vector from the current program_state.  */
      p.purge (*bv);
      return *bv;
    }
  auto bv_up = std::make_unique<symbolic::conjured_bit_vector> (*this, k);
  auto &result = *bv_up.get ();
  m_conjured_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector *
bit_vector_manager::maybe_fold_bits_for_constant (tree tree_cst)
{
  gcc_assert (tree_cst);

  switch (TREE_CODE (tree_cst))
    {
    default:
      break;

    case INTEGER_CST:
      {
	bit_size_t size;
	bool got_size = int_size_in_bits (TREE_TYPE (tree_cst),  &size);
	gcc_assert (got_size);

	integer_with_unit<units::bit> num_bits = convert_from_bit_size_t (size);

	/* Fold all-zeroes and all-ones.  */
	if (zerop (tree_cst))
	  return &get_all_zeros (num_bits);
	if (integer_all_onesp (tree_cst))
	  return &get_all_ones (num_bits);

	/* Fold bytes.  */
	if (num_bits == 8_bits)
	  {
	    uint8_t val = TREE_INT_CST_LOW (tree_cst);
	    if (auto *b = m_cached_bytes[val])
	      return b;
	  }
      }
      break;

    case RAW_DATA_CST:
      if (RAW_DATA_LENGTH (tree_cst) == 0)
	return &m_empty_bv;
      break;
    }
  return nullptr;
}

const bit_vector &
bit_vector_manager::get_bits_for_constant (tree tree_cst)
{
  if (auto *folded = maybe_fold_bits_for_constant (tree_cst))
    return *folded;

  const svalue *sval
    = get_svalue_manager ().get_or_create_constant_svalue (tree_cst);
  return get_bit_vector_from_svalue (*sval);
}

const bit_vector *
bit_vector_manager::
maybe_fold_bytes_within_constant (tree tree_cst,
				  concrete_byte_range conc_bytes)
{
  gcc_assert (tree_cst);
  gcc_assert (CONSTANT_CLASS_P (tree_cst));
  gcc_assert (conc_bytes.get_start_offset () >= 0_bytes);

  switch (TREE_CODE (tree_cst))
    {
    default:
      return nullptr;

    /* Special-case these.  */
    case STRING_CST:
      return maybe_fold_bytes_within_string_cst (tree_cst, conc_bytes);
    case RAW_DATA_CST:
      return maybe_fold_bytes_within_raw_data_cst (tree_cst, conc_bytes);

    /* Handle the constants that native_encode_expr can.  */
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case CONSTRUCTOR:
      {
	/* Use native_encode_expr and extract the pertinent byte(s).  */
	unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
	int size = native_encode_expr (tree_cst, buf, sizeof buf, 0);
	if (size == 0)
	  return nullptr;

	gcc_assert (conc_bytes.get_last_offset () < concrete_byte_size (size));

	std::vector<const bit_vector *> vec_of_bytes;
	const concrete_byte_offset next_offset = conc_bytes.get_next_offset ();
	for (concrete_byte_offset conc_offset = conc_bytes.get_start_offset ();
	     conc_offset < next_offset;
	     conc_offset += 1_byte)
	  vec_of_bytes.push_back
	    (&get_uint8 (buf[conc_offset.as_unsigned_long ()]));

	return &get_concat_bits (std::move (vec_of_bytes));
      }
    }
}

const bit_vector *
bit_vector_manager::
maybe_fold_bytes_within_string_cst (tree string_cst,
				    concrete_byte_range conc_bytes)
{
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);

  /* Adapted from fold_read_from_constant_string.  */
  scalar_int_mode char_mode;
  if (!is_int_mode (TYPE_MODE (TREE_TYPE (TREE_TYPE (string_cst))),
		    &char_mode))
    return nullptr;
  if (GET_MODE_SIZE (char_mode) != 1)
    return nullptr;

  gcc_assert (conc_bytes.get_start_offset () >= 0_bytes);
  if (conc_bytes.get_start_offset ()
      < integer_with_unit<units::byte> (TREE_STRING_LENGTH (string_cst)))
    {
      /* We're within the area defined by TREE_STRING_POINTER.  */
      if (conc_bytes.get_size () == 1_byte)
	return &get_uint8
	  (TREE_STRING_POINTER (string_cst)
	   [conc_bytes.get_start_offset ().get_integer ().as_unsigned_long ()]);
      else
	return nullptr;
    }
  else
    {
      /* We're in the padding area of trailing zeroes.  */
      return &get_all_zeros (conc_bytes.get_size ());
    }
}

const bit_vector *
bit_vector_manager::
maybe_fold_bytes_within_raw_data_cst (tree raw_data_cst,
				      concrete_byte_range conc_bytes)
{
  gcc_assert (TREE_CODE (raw_data_cst) == RAW_DATA_CST);

  if (conc_bytes.get_size () != 1_byte)
    return nullptr;

  gcc_assert (conc_bytes.get_start_offset () >= 0_bytes);
  gcc_assert
    (conc_bytes.get_start_offset ()
     < integer_with_unit<units::byte> (RAW_DATA_LENGTH (raw_data_cst)));

  size_t idx
    = conc_bytes.get_start_offset ().get_integer ().as_unsigned_long ();
  return &get_uint8 (RAW_DATA_UCHAR_ELT (raw_data_cst,  idx));
}

const bit_vector &
bit_vector_manager::
make_baseline_bit_vector (baseline_key k)
{
  gcc_assert (k.m_base_reg->can_have_initial_svalue_p ());

  auto &rmm = m_node_mgr.get_svalue_manager ();
  const svalue *sval = nullptr;
  switch (k.m_baseline_state)
    {
    default:
      gcc_unreachable ();

    case baseline_state::main:
      sval = k.m_base_reg->get_initial_value_at_main (&rmm);
      break;

    case baseline_state::after_main:
      sval = rmm.get_or_create_initial_value (k.m_base_reg);
      break;
    }

  gcc_assert (sval);
  return get_bit_vector_from_svalue (*sval);
}

const bit_vector &
bit_vector_manager::
get_baseline_bit_vector (const region &base_reg,
			 baseline_state bs)
{
  baseline_key k {&base_reg, bs};
  auto iter = m_baseline_map.find (k);
  if (iter != m_baseline_map.end ())
    return iter->second;

  auto &bc = make_baseline_bit_vector (k);
  m_baseline_map.insert ({k, bc});
  return bc;
}

const bit_vector *
bit_vector_manager::
maybe_fold_cropped_bits (const bit_vector &inner_bv,
			 const symbolic_bit_range &inner_bits_to_use)
{
  if (inner_bits_to_use.get_size ().is_zero_p ())
    return &m_empty_bv;

  // x[0..size(x)] --> x.  */
  if (inner_bits_to_use.get_start_offset ().is_zero_p ()
      && &inner_bits_to_use.get_size () == &inner_bv.get_size ())
    return &inner_bv;

  if (auto folded = inner_bv.maybe_fold_crop (inner_bits_to_use))
    return folded;

  return nullptr;
}

const bit_vector &
bit_vector_manager::
get_cropped_bits (const bit_vector &inner_bv,
		  const symbolic_bit_range &inner_bits_to_use)
{
  if (auto *folded = maybe_fold_cropped_bits (inner_bv, inner_bits_to_use))
    return *folded;

  const symbolic::cropped_bit_vector::key k {&inner_bv,
					     inner_bits_to_use};
  auto iter = m_cropped_map.find (k);
  if (iter != m_cropped_map.end ())
    return *(iter->second);
  auto bv_up = std::make_unique<cropped_bit_vector> (*this, k);
  auto &result = *bv_up.get ();
  m_cropped_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector &
bit_vector_manager::
get_cropped_bytes (const bit_vector &inner_bv,
		   const symbolic_byte_range &inner_bytes_to_use)
{
  return get_cropped_bits (inner_bv,
			   bits_from_bytes (inner_bytes_to_use));
}

const bit_vector *
bit_vector_manager::
maybe_fold_repeated_bits (const bit_vector &inner_bv,
			  const symbolic_integer &repeat_count)
{
  if (repeat_count.is_zero_p ())
    return &m_empty_bv;

  if (repeat_count.is_one_p ())
    return &inner_bv;

  if (auto poisoned_bv = inner_bv.dyn_cast_poisoned_bit_vector ())
    return &get_poisoned_bits
      (inner_bv.get_size () * repeat_count,
       poisoned_bv->get_poison_kind ());

  if (auto tristate_bv = inner_bv.dyn_cast_tristate_bit_vector ())
    return &get_tristate_bits
      (inner_bv.get_size () * repeat_count,
       tristate_bv->get_bit_value ());

  if (auto repeated_bv = inner_bv.dyn_cast_repeated_bit_vector ())
    return &get_repeated_bits (repeated_bv->get_inner_bit_vector (),
			       (repeated_bv->get_repeat_count ()
				* repeat_count));

  return nullptr;
}

const bit_vector &
bit_vector_manager::
get_repeated_bits (const bit_vector &inner_bv,
		   const symbolic_integer &repeat_count)
{
  if (auto *folded = maybe_fold_repeated_bits (inner_bv, repeat_count))
    return *folded;

  const symbolic::repeated_bit_vector::key k {&inner_bv, &repeat_count};
  auto iter = m_repeated_map.find (k);
  if (iter != m_repeated_map.end ())
    return *(iter->second);
  auto bv_up = std::make_unique<repeated_bit_vector> (*this, k);
  auto &result = *bv_up.get ();
  m_repeated_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector &
bit_vector_manager::
get_repeated_bits (const bit_vector &inner_bv,
		   const concrete_integer &repeat_count)
{
  auto &integer_mgr = get_node_manager ().get_scalars ().get_unitless ();
  return get_repeated_bits (inner_bv,
			    integer_mgr.get_concrete (repeat_count));
}

/* Return true if TYPE_A and TYPE_B are known to be the same size.  */

static bool
types_are_same_size_p (const_tree type_a, const_tree type_b)
{
  concrete_bit_size size_a;
  if (!get_type_size (type_a, &size_a))
    return false;
  concrete_bit_size size_b;
  if (!get_type_size (type_b, &size_b))
    return false;
  return size_a == size_b;
}

const bit_vector *
bit_vector_manager::maybe_fold_bits_for_svalue (const svalue &sval)
{
  switch (sval.get_kind ())
    {
    default:
      break;

    case SK_CONSTANT:
      {
	tree tree_cst = sval.maybe_get_constant ();
	if (auto *folded = maybe_fold_bits_for_constant (tree_cst))
	  return folded;

	/* Fold any 1-byte constants we missed.  */
	auto &bit_mgr = get_node_manager ().get_scalars ().get_bits ();
	concrete_bit_size conc_bit_size;
	if (sval.has_concrete_size_p (&conc_bit_size, bit_mgr)
	    && conc_bit_size == 8_bits)
	  {
	    unsigned char buf[1];
	    int size = native_encode_expr (tree_cst, buf, sizeof buf, 0);
	    gcc_assert (size == 1);
	    unsigned char val = buf[0];
	    if (auto *b = m_cached_bytes[val])
	      return b;
	  }
      }
      break;

      /* TODO: re-enabling this will require a rewrite of parts of
	 access-diagram.cc which expect string regions, rather than
	 constant_svalue of string_cst.  */
#if 0
    case SK_INITIAL:
      {
	auto &init_sval = *as_a <const initial_svalue *> (&sval);
	if (auto *string_reg
	      = init_sval.get_region ()->dyn_cast_string_region ())
	  return &get_bits_for_constant (string_reg->get_string_cst ());
      }
      break;
#endif

    case SK_BIT_VECTOR:
      {
	auto &bit_vector_sval = *as_a<const svalue_from_bit_vector *> (&sval);
	return &bit_vector_sval.get_bit_vector ();
      }
      break;

    case SK_UNKNOWN:
      {
	concrete_bit_size size;
	if (sval.has_concrete_size_p (&size, get_bit_manager ()))
	  return &get_unknown_bits (size);
	else
	  return &get_unknown_bits ();
      }
      break;

    case SK_UNARYOP:
      {
	auto &unaryop_sval = *as_a<const unaryop_svalue *> (&sval);
	if (CONVERT_EXPR_CODE_P (unaryop_sval.get_op ()))
	  {
	    /* If we have a type conversion for which no instructions
	       are needed, they are presumably bit-for-bit identical,
	       or for which the sizes are identical.
	       Fold away the conversion, so e.g.
	       e.g. BIT_VECTOR_FOR (UNARY_OP(signed, NOP_EXPR, SVAL(unsigned)))
	       folds to BIT_VECTOR_FOR (SVAL(unsigned)).  */
	    if (sval.get_type () == nullptr
		|| unaryop_sval.get_arg ()->get_type () == nullptr
		|| tree_nop_conversion_p (sval.get_type (),
					  unaryop_sval.get_arg ()->get_type ())
		|| types_are_same_size_p (sval.get_type (),
					  unaryop_sval.get_arg ()->get_type ()))
	      return &get_bit_vector_from_svalue (*unaryop_sval.get_arg ());
	  }
      }
      break;
    }

  return nullptr;
}

const bit_vector &
bit_vector_manager::get_bit_vector_from_svalue (const svalue &sval)
{
  if (auto *folded = maybe_fold_bits_for_svalue (sval))
    return *folded;

  /* We need to be able to determine the size of SVAL.  */
  concrete_bit_size conc_bit_size;
  gcc_assert (sval.has_concrete_size_p (&conc_bit_size,
					get_bit_manager ()));

  if (conc_bit_size == 0_bits)
    return get_empty ();

  const symbolic::bit_vector_from_svalue::key k {&sval};
  auto iter = m_svalue_to_bv_map.find (k);
  if (iter != m_svalue_to_bv_map.end ())
    return *(iter->second);
  auto bv_up = std::make_unique<symbolic::bit_vector_from_svalue> (*this, k);
  auto &result = *bv_up.get ();
  m_svalue_to_bv_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector *
bit_vector_manager::maybe_get_bit_vector_from_svalue (const svalue &sval)
{
  if (auto *folded = maybe_fold_bits_for_svalue (sval))
    return folded;

  const symbolic::bit_vector_from_svalue::key k {&sval};
  auto iter = m_svalue_to_bv_map.find (k);
  if (iter != m_svalue_to_bv_map.end ())
    return iter->second.get ();
  return nullptr;
}

const bit_vector &
bit_vector_manager::get_poisoned_bits (const symbolic_bit_size &num_bits,
				       enum poison_kind kind)
{
  if (num_bits.is_zero_p ())
    return m_empty_bv;

  poisoned_bit_vector::key k {num_bits, kind};

  auto iter = m_poisoned_map.find (k);
  if (iter != m_poisoned_map.end ())
    return *(iter->second);

  auto bv_up = std::make_unique<poisoned_bit_vector> (*this, k);
  auto &result = *bv_up.get ();
  m_poisoned_map.insert ({k, std::move (bv_up)});
  return result;
}

const bit_vector &
bit_vector_manager::get_poisoned_bits (const concrete_bit_size &num_bits,
				       enum poison_kind kind)
{
  return get_poisoned_bits (symbolic_from_concrete (num_bits,
						    get_bit_manager ()),
			    kind);
}

#if CHECKING_P

const bit_vector &
bit_vector_manager::
get_named_test_bit_vector (const symbolic_bit_size &size_in_bits,
			   std::string label)
{
  auto bv_up = std::make_unique<named_test_bit_vector> (*this,
							size_in_bits,
							std::move (label));
  auto &result = *bv_up.get ();
  m_named_test_bvs.push_back (std::move (bv_up));
  return result;
}

const bit_vector &
bit_vector_manager::
get_named_test_bit_vector (const symbolic_byte_size &size_in_bytes,
			   std::string label)
{
  auto &size_in_bits = bits_from_bytes (size_in_bytes);
  return get_named_test_bit_vector (size_in_bits, std::move (label));
}

const bit_vector &
bit_vector_manager::
get_named_test_bit_vector (const concrete_bit_size &conc_size_in_bits,
			   std::string label)
{
  auto &bit_mgr = get_node_manager ().get_scalars ().get_bits ();
  auto &sym_size_in_bits = symbolic_from_concrete (conc_size_in_bits, bit_mgr);
  return get_named_test_bit_vector (sym_size_in_bits, std::move (label));
}

const bit_vector &
bit_vector_manager::
get_named_test_bit_vector (const concrete_byte_size &conc_size_in_bytes,
			   std::string label)
{
  concrete_bit_size conc_size_in_bits = bits_from_bytes (conc_size_in_bytes);
  return get_named_test_bit_vector (conc_size_in_bits, std::move (label));
}

#endif /* #if CHECKING_P */

} // namespace ana::symbolic

#if CHECKING_P

namespace selftest {

static void
test_maybe_set_irange_bitmask_from_concrete_tristate_vector ()
{
  /* A combination of ones, zeros, and unknown, expressed in order
     LSB through MSB.  */
  concrete_tristate_vector ts_vec
    ({tristate (true),
      tristate (false),
      tristate::unknown (),
      tristate (true),
      tristate (false)});

  irange_bitmask irange_bmask;
  bool ok = symbolic::maybe_set_irange_bitmask_from_concrete_tristate_vector
    (ts_vec, &irange_bmask);
  ASSERT_TRUE (ok);
  irange_bmask.verify_mask ();

  /* We expect:
     index: [..., 4, 3, 2, 1, 0]
     VALUE: [..., 0, 1, 0, 0, 1]
     MASK:  [..., 0, 0, 1, 0, 0]
     |            ^  ^  ^  ^  ^
     |            |  |  |  |  bit 0: true
     |            |  |  |  bit 1: false
     |            |  |  bit 2: unknown
     |            | bit 3: true
     |            bit 4: false.  */
  ASSERT_EQ (irange_bmask.value (), wi::uhwi (0b01001, 5));
}

static void
test_tristate ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &false_bit = mgr.get_all_zeros (1_bit);
  ASSERT_SAME_SYMBOLIC_NODE (false_bit.get_size (),
			     bit_mgr.get_concrete (1_bit));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (false_bit,
     "tristate_bit_vector[size: 1 bit]: false bit\n");
  false_bit.to_json ();
  ASSERT_EQ (false_bit.get_tristate_at_offset (0_bits), tristate (false));

  auto &true_bit = mgr.get_tristate_bits (1_bit, tristate (true));
  ASSERT_SAME_SYMBOLIC_NODE (true_bit.get_size (),
			     bit_mgr.get_concrete (1_bit));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (true_bit,
     "tristate_bit_vector[size: 1 bit]: true bit\n");
  true_bit.to_json ();
  ASSERT_EQ (true_bit.get_tristate_at_offset (0_bits), tristate (true));

  auto &unknown_bit = mgr.get_unknown_bits (1_bit);
  ASSERT_SAME_SYMBOLIC_NODE (unknown_bit.get_size (),
			     bit_mgr.get_concrete (1_bit));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (unknown_bit,
     "tristate_bit_vector[size: 1 bit]: unknown bit\n");
  unknown_bit.to_json ();
  ASSERT_EQ (unknown_bit.get_tristate_at_offset (0_bits), tristate::unknown ());

  auto &three_zero_bits = mgr.get_all_zeros (3_bits);
  ASSERT_SAME_SYMBOLIC_NODE (three_zero_bits.get_size (),
			     bit_mgr.get_concrete (3_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (three_zero_bits,
     "tristate_bit_vector[size: 3 bits]: false bits\n");

  auto &byte_00 = mgr.get_all_zeros (1_byte);
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (byte_00,
     "tristate_bit_vector[size: 1 byte]: false bits\n");

  auto &byte_ff = mgr.get_tristate_bits (8_bits, tristate (true));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (byte_ff,
     "tristate_bit_vector[size: 1 byte]: true bits\n");

  auto &unknown_32_bits = mgr.get_unknown_bits (32_bits);
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (unknown_32_bits,
     "tristate_bit_vector[size: 4 bytes]: unknown bits\n");
  ASSERT_EQ (unknown_32_bits.get_tristate_at_offset (31_bits),
	     tristate::unknown ());

  auto &x = bit_mgr.get_named_test_scalar ("X");
  auto &sym_sized_zeros = mgr.get_all_zeros (x);
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (sym_sized_zeros,
     "tristate_bit_vector[size: X bits]: false bits\n");
  ASSERT_SAME_SYMBOLIC_NODE (sym_sized_zeros.get_size (),
			     x);
  sym_sized_zeros.to_json ();
  ASSERT_EQ (sym_sized_zeros.get_tristate_at_offset (0_bits), tristate (false));
  ASSERT_EQ (sym_sized_zeros.get_tristate_at_offset (x), tristate (false));

  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_all_zeros (0_bits),
			     mgr.get_empty ());
  mgr.get_empty ().to_json ();

  {
    pretty_printer pp;
    hexdump_options hexdump_opts;
    hexdump_opts.m_show_headings = true;
    hexdump_opts.m_show_binary = true;
    unknown_32_bits.hexdump_to_pp (&pp, hexdump_opts);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("             +-----------------------------------+-----------+-----+\n"
	"             |              Binary               |    Hex    |ASCII|\n"
	"+------------+-----------------------------------+-----------+-----+\n"
	"|[bytes 0..4)|???????? ???????? ???????? ????????|?? ?? ?? ??|???? |\n"
	"+------------+-----------------------------------+-----------+-----+\n"));
  }
  {
    pretty_printer pp;
    hexdump_options hexdump_opts;
    hexdump_opts.m_show_headings = false;
    hexdump_opts.m_show_binary = true;
    mgr.get_all_zeros (32_bits).hexdump_to_pp (&pp, hexdump_opts);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("+------------+-----------------------------------+-----------+----+\n"
	"|[bytes 0..4)|00000000 00000000 00000000 00000000|00 00 00 00|....|\n"
	"+------------+-----------------------------------+-----------+----+\n"));
  }
  {
    pretty_printer pp;
    hexdump_options hexdump_opts;
    hexdump_opts.m_show_headings = false;
    hexdump_opts.m_show_binary = true;
    mgr.get_all_ones (32_bits).hexdump_to_pp (&pp, hexdump_opts);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("+------------+-----------------------------------+-----------+----+\n"
	"|[bytes 0..4)|11111111 11111111 11111111 11111111|ff ff ff ff|....|\n"
	"+------------+-----------------------------------+-----------+----+\n"));
  }
}

static void
test_integer_constants ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  // Verify that we fold all zeroes and all ones

  auto &char_0
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 0));
  ASSERT_SAME_SYMBOLIC_NODE (char_0.get_size (),
			     bit_mgr.get_concrete (8_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (char_0,
     "tristate_bit_vector[size: 1 byte]: false bits\n");
  char_0.to_json ();

  auto &char_0xff
    = mgr.get_bits_for_constant (build_int_cst (unsigned_char_type_node,
						0xff));
  ASSERT_SAME_SYMBOLIC_NODE (char_0xff.get_size (),
			     bit_mgr.get_concrete (8_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (char_0xff,
     "tristate_bit_vector[size: 1 byte]: true bits\n");
  char_0xff.to_json ();

  // Verify that we handle constants that we can't simplify

  auto &char_A
    = mgr.get_bits_for_constant (build_int_cst (char_type_node,
						'A'));
  ASSERT_SAME_SYMBOLIC_NODE (char_A.get_size (),
			     bit_mgr.get_concrete (8_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (char_A,
     ("bit_vector_from_svalue[size: 1 byte]\n"
      "`- m_sval: 'unsigned char': constant_svalue ('65')\n"));

  auto &int_42
    = mgr.get_bits_for_constant (build_int_cst (uint32_type_node, 42));
  ASSERT_SAME_SYMBOLIC_NODE (int_42.get_size (),
			     bit_mgr.get_concrete (32_bits));
  int_42.to_json ();

  // -1 is simplifiable to all ones...
  auto &char_minus_one
    = mgr.get_bits_for_constant (build_int_cst (signed_char_type_node, -1));
  ASSERT_SAME_SYMBOLIC_NODE (char_minus_one.get_size (),
			     bit_mgr.get_concrete (8_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (char_minus_one,
     "tristate_bit_vector[size: 1 byte]: true bits\n");

  // ...but -2 isn't
  auto &char_minus_two
    = mgr.get_bits_for_constant (build_int_cst (signed_char_type_node,
						-2));
  ASSERT_SAME_SYMBOLIC_NODE (char_minus_two.get_size (),
			     bit_mgr.get_concrete (8_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (char_minus_two,
     ("bit_vector_from_svalue[size: 1 byte]\n"
      "`- m_sval: 'unsigned char': constant_svalue ('254')\n"));
  ASSERT_SAME_SYMBOLIC_NODE
    (char_minus_two,
     mgr.get_bits_for_constant (build_int_cst (unsigned_char_type_node,
					       254)));
}

static void
test_bytes ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_uint8 (0),
		      mgr.get_all_zeros (1_byte));
  for (int i = 0x01; i < 0xff; ++i)
    {
      auto &b = mgr.get_uint8 (i);
      ASSERT_SAME_SYMBOLIC_NODE (b.get_size (),
				 bit_mgr.get_concrete (8_bits));
    }
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_uint8 (0xff),
			     mgr.get_all_ones (1_byte));

  for (int i = 0; i < 0xff; ++i)
    {
      auto &bv = mgr.get_uint8 (i);
      uint8_t val;
      bool out = bv.maybe_as_uint8 (&val);
      ASSERT_TRUE (out);
      ASSERT_EQ (val, i);
      for (int bit_idx = 0; bit_idx < 8; ++bit_idx)
	ASSERT_EQ (bv.get_tristate_at_offset (concrete_bit_offset (bit_idx)),
		   tristate (i & (1 << bit_idx)));
    }
}

static void
test_string_constants ()
{
  tree string_cst_ptr = make_string_constant ("abc");

  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &abc0 = bv_mgr.get_bits_for_constant (string_cst_ptr);
  ASSERT_SAME_SYMBOLIC_NODE (abc0.get_size (),
			     bit_mgr.get_concrete (bits_from_bytes (4_bytes)));
  abc0.to_json ();

  {
    pretty_printer pp;
    abc0.hexdump_to_pp (&pp, hexdump_options ());
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("+------------+-----------+----+\n"
	"|[bytes 0..4)|61 62 63 00|abc.|\n"
	"+------------+-----------+----+\n"));
  }

  /* Verify accessing chars within a STRING_CST.  */
  {
    auto &a = abc0.get_byte_at_offset (0_bytes);
    ASSERT_SAME_SYMBOLIC_NODE (a, bv_mgr.get_uint8 ('a'));

    auto &b = abc0.get_byte_at_offset (1_byte);
    ASSERT_SAME_SYMBOLIC_NODE (b, bv_mgr.get_uint8 ('b'));

    auto &c = abc0.get_byte_at_offset (2_bytes);
    ASSERT_SAME_SYMBOLIC_NODE (c, bv_mgr.get_uint8 ('c'));

    auto &term = abc0.get_byte_at_offset (3_bytes);
    ASSERT_SAME_SYMBOLIC_NODE (term, bv_mgr.get_uint8 (0));
  }

  for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
    ASSERT_EQ (abc0.scan_for_first_zero_byte (i),
	       string_scan::found_nul (3_bytes));

  /* Example of concatenating known string literals.  */
  {
    const char *hello = "hello";
    const char *world = " world!";
    tree t_hello = make_string_constant (hello);
    tree t_world = make_string_constant (world);
    auto &bv_hello = bv_mgr.get_bits_for_constant (t_hello);
    auto &bv_world = bv_mgr.get_bits_for_constant (t_world);
    auto &concat_bv = bv_mgr.get_concat_bits
      ({&bv_hello.get_prefix (concrete_byte_size (strlen (hello))),
	&bv_world.get_prefix (concrete_byte_size (strlen (world))),
	&bv_mgr.get_uint8 (0)});
    pretty_printer pp;
    hexdump_options hexdump_opts;
    hexdump_opts.m_bytes_per_line = 8_bytes;
    concat_bv.hexdump_to_pp (&pp, hexdump_opts);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       "+-------------+-----------------------+--------+\n"
       "| [bytes 0..8)|68 65 6c 6c 6f 20 77 6f|hello wo|\n"
       "+-------------+-----------------------+--------+\n"
       "|[bytes 8..13)|72 6c 64 21 00         |rld!.   |\n"
       "+-------------+-----------------------+--------+\n");
    for (concrete_byte_offset i = 0_bytes; i < 13_bytes; ++i)
      ASSERT_EQ (concat_bv.scan_for_first_zero_byte (i),
		 string_scan::found_nul (12_bytes));
  }
}

static tree
make_raw_data_constant (const char *buf, size_t size)
{
  tree node = make_node (RAW_DATA_CST);
  TREE_TYPE (node) = integer_type_node;
  RAW_DATA_LENGTH (node) = size;
  RAW_DATA_POINTER (node) = buf;
  return node;
}

static void
test_raw_data_constant_1 ()
{
  tree raw_data_cst_ptr = make_raw_data_constant ("abc", 3);

  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &abc = mgr.get_bits_for_constant (raw_data_cst_ptr);
  ASSERT_SAME_SYMBOLIC_NODE (abc.get_size (),
			     bit_mgr.get_concrete (bits_from_bytes (3_bytes)));
  abc.to_json ();

  {
    pretty_printer pp;
    abc.hexdump_to_pp (&pp, hexdump_options ());
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("+------------+--------+---+\n"
	"|[bytes 0..3)|61 62 63|abc|\n"
	"+------------+--------+---+\n"));
  }

  /* Verify accessing chars within a RAW_DATA_CST.  */
  {
    auto &a = abc.get_byte_at_offset (0_bytes);
    ASSERT_SAME_SYMBOLIC_NODE (a, mgr.get_uint8 ('a'));

    auto &b = abc.get_byte_at_offset (1_byte);
    ASSERT_SAME_SYMBOLIC_NODE (b, mgr.get_uint8 ('b'));

    auto &c = abc.get_byte_at_offset (2_bytes);
    ASSERT_SAME_SYMBOLIC_NODE (c, mgr.get_uint8 ('c'));
  }
}

/* Test of a raw data cst with multiple embedded zero bytes.  */

static void
test_raw_data_constant_2 ()
{
  tree raw_data_cst_ptr = make_raw_data_constant ("a\0b\0c\0", 6);

  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &a0b0c0 = mgr.get_bits_for_constant (raw_data_cst_ptr);
  ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_size (),
			     bit_mgr.get_concrete (bits_from_bytes (6_bytes)));
  a0b0c0.to_json ();

  {
    pretty_printer pp;
    a0b0c0.hexdump_to_pp (&pp, hexdump_options ());
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("+------------+-----------+----+\n"
	"|[bytes 0..4)|61 00 62 00|a.b.|\n"
	"+------------+-----------+----+\n"
	"|[bytes 4..6)|63 00      |c.  |\n"
	"+------------+-----------+----+\n"));
  }

  /* Verify accessing chars within a RAW_DATA_CST.  */
  {
    ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_byte_at_offset (0_bytes),
			       mgr.get_uint8 ('a'));
    ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_byte_at_offset (1_byte),
			       mgr.get_uint8 (0));
    ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_byte_at_offset (2_bytes),
			       mgr.get_uint8 ('b'));
    ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_byte_at_offset (3_bytes),
			       mgr.get_uint8 (0));
    ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_byte_at_offset (4_bytes),
			       mgr.get_uint8 ('c'));
    ASSERT_SAME_SYMBOLIC_NODE (a0b0c0.get_byte_at_offset (5_bytes),
			       mgr.get_uint8 (0));
  }

  /* String scans should locate the NUL bytes.  */
  {
    ASSERT_EQ (a0b0c0.scan_for_first_zero_byte (0_bytes),
	       string_scan::found_nul (1_byte));
    ASSERT_EQ (a0b0c0.scan_for_first_zero_byte (1_byte),
	       string_scan::found_nul (1_byte));
    ASSERT_EQ (a0b0c0.scan_for_first_zero_byte (2_bytes),
	       string_scan::found_nul (3_bytes));
    ASSERT_EQ (a0b0c0.scan_for_first_zero_byte (3_bytes),
	       string_scan::found_nul (3_bytes));
    ASSERT_EQ (a0b0c0.scan_for_first_zero_byte (4_bytes),
	       string_scan::found_nul (5_bytes));
    ASSERT_EQ (a0b0c0.scan_for_first_zero_byte (5_bytes),
	       string_scan::found_nul (5_bytes));
  }
}

/* Test of an empty raw data cst.  */

static void
test_raw_data_constant_empty ()
{
  tree raw_data_cst_ptr = make_raw_data_constant ("", 0);

  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &bv = mgr.get_bits_for_constant (raw_data_cst_ptr);
  ASSERT_SAME_SYMBOLIC_NODE (bv.get_size (),
			     bit_mgr.get_concrete (bits_from_bytes (0_bytes)));
  ASSERT_SAME_SYMBOLIC_NODE (bv,
			     node_mgr.get_bit_vector_manager ().get_empty ());
}

static void
test_concat ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &char_a
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'a'));
  auto &char_b
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'b'));
  auto &char_x
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'x'));
  auto &char_y
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'y'));

  auto &ab = mgr.get_concat_bits ({&char_a, &char_b});
  ASSERT_SAME_SYMBOLIC_NODE (ab.get_size (),
			     bit_mgr.get_concrete (bits_from_bytes (2_bytes)));
  ab.to_json ();

  auto &xy
    = mgr.get_concat_bits ({&char_x, &char_y});
  ASSERT_SAME_SYMBOLIC_NODE (xy.get_size (),
			     bit_mgr.get_concrete (bits_from_bytes (2_bytes)));
  for (concrete_byte_offset i = 0_bytes; i < 2_bytes; ++i)
    ASSERT_EQ (xy.scan_for_first_zero_byte (i),
	       string_scan::beyond_domain (2_bytes));

  auto &single_element
    = mgr.get_concat_bits ({&char_a});
  ASSERT_SAME_SYMBOLIC_NODE (single_element, char_a);

  /* Verity concat (empty, empty) --> empty.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (mgr.get_concat_bits ({&mgr.get_empty (), &mgr.get_empty ()}),
     mgr.get_empty ());

  /* Verity concat (a, empty) --> a.  */
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_concat_bits ({&char_a, &mgr.get_empty ()}),
			     char_a);
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_concat_bits ({&mgr.get_empty (), &char_a}),
			     char_a);

  /* Verify concat(a, a) --> a * 2.  */
  auto &aa
    = mgr.get_concat_bits ({&char_a, &char_a});
  ASSERT_SAME_SYMBOLIC_NODE (aa.get_size (),
			     bit_mgr.get_concrete (16_bits));
  ASSERT_SAME_SYMBOLIC_NODE
    (aa,
     mgr.get_repeated_bits (char_a, 2));

  /* Verify concat(a, a, a) --> a * 3.  */
  auto &aaa
    = mgr.get_concat_bits
    ({&char_a, &char_a, &char_a});
  ASSERT_SAME_SYMBOLIC_NODE (aaa.get_size (),
			     bit_mgr.get_concrete (24_bits));
  ASSERT_SAME_SYMBOLIC_NODE
    (aaa,
     mgr.get_repeated_bits (char_a, 3));

  /* Verify concat(a * 2, a) --> a * 3.  */
  {
    auto &bits = mgr.get_concat_bits ({&aa, &char_a});
    ASSERT_SAME_SYMBOLIC_NODE (bits, aaa);
  }

  /* Verify concat(a, a * 2) --> a * 3.  */
  {
    auto &bits = mgr.get_concat_bits ({&char_a, &aa});
    ASSERT_SAME_SYMBOLIC_NODE (bits, aaa);
  }

  /* Verify concat(a * 2, a * 2) --> a * 4.  */
  {
    auto &bits = mgr.get_concat_bits ({&aa, &aa});
    ASSERT_SAME_SYMBOLIC_NODE
      (bits,
       mgr.get_repeated_bits (char_a, 4));
  }

  /* Verify concat(a * 2, a, a * 2) --> a * 5.  */
  {
    auto &bits = mgr.get_concat_bits ({&aa, &char_a, &aa});
    ASSERT_SAME_SYMBOLIC_NODE
      (bits,
       mgr.get_repeated_bits (char_a, 5));
  }

  /* Verify concat(a, a, b, b) --> concat(a * 2, b * 2).  */
  {
    auto &bits
      = mgr.get_concat_bits
      ({&char_a, &char_a, &char_b, &char_b});
    ASSERT_SAME_SYMBOLIC_NODE
      (bits,
       mgr.get_concat_bits
       ({&mgr.get_repeated_bits (char_a, 2),
	 &mgr.get_repeated_bits (char_b, 2)}));
  }

  /* Verify concat of poisoneds.  */
  {
    /* Verify concat(poisoned(m), poisoned(n) --> poisoned(m + n)
       for same kind of poison.  */
    {
      auto &bits = mgr.get_concat_bits
	({&mgr.get_poisoned_bits (2_bits, poison_kind::uninit),
	  &mgr.get_poisoned_bits (3_bits, poison_kind::uninit)});
      ASSERT_SAME_SYMBOLIC_NODE
	(bits,
	 mgr.get_poisoned_bits (5_bits, poison_kind::uninit));
    }

    // Verify that we don't fold the concat of non-equal kinds of poison
    {
      auto &bits = mgr.get_concat_bits
	({&mgr.get_poisoned_bits (2_bits, poison_kind::uninit),
	  &mgr.get_poisoned_bits (3_bits, poison_kind::freed)});
      ASSERT_SYMBOLIC_NODE_DUMP_EQ
	(bits,
	 ("concat_bit_vector[size: 5 bits]\n"
	  "+- poisoned_bit_vector[size: 2 bits]: uninit\n"
	  "`- poisoned_bit_vector[size: 3 bits]: freed\n"));
    }
  }

  /* Verify concat of tristates.  */
  {
    /* Verify concat(tristate(m), tristate(n) --> tristate(m + n)
       for same tristate.  */
    {
      auto &bits
	= mgr.get_concat_bits ({&mgr.get_all_zeros (2_bits),
				&mgr.get_all_zeros (3_bits)});
      ASSERT_SAME_SYMBOLIC_NODE (bits, mgr.get_all_zeros (5_bits));
    }

    // Verify that we don't fold the concat of non-equal tristates
    {
      auto &bits
	= mgr.get_concat_bits
	({
	   &mgr.get_all_zeros (2_bits),
	   &mgr.get_all_ones (3_bits)
	 });
      ASSERT_SYMBOLIC_NODE_DUMP_EQ
	(bits,
	 ("concat_bit_vector[size: 5 bits]\n"
	  "+- tristate_bit_vector[size: 2 bits]: false bits\n"
	  "`- tristate_bit_vector[size: 3 bits]: true bits\n"));
      ASSERT_EQ (bits.get_tristate_at_offset (0_bits), tristate (false));
      ASSERT_EQ (bits.get_tristate_at_offset (1_bit), tristate (false));
      ASSERT_EQ (bits.get_tristate_at_offset (2_bits), tristate (true));
      ASSERT_EQ (bits.get_tristate_at_offset (3_bits), tristate (true));
      ASSERT_EQ (bits.get_tristate_at_offset (4_bits), tristate (true));

      {
	pretty_printer pp;
	hexdump_options hexdump_opts;
	hexdump_opts.m_show_binary = true;
	hexdump_opts.m_show_hex = false;
	hexdump_opts.m_show_ascii = false;
	bits.hexdump_to_pp (&pp, hexdump_opts);
	ASSERT_STREQ
	  (pp_formatted_text (&pp),
	   "+-----------+-----+\n"
	   "|[bits 0..5)|00111|\n"
	   "+-----------+-----+\n");
      }
    }
  }

  /* Verify concat of concat --> concat.  */
  /*
     e.g.
     concat(concat(A, B), concat(X, Y)) --> concat(A, B, X, Y);
  */
  auto &abxy
    = mgr.get_concat_bits ({&ab, &xy});
  ASSERT_SAME_SYMBOLIC_NODE (abxy.get_size (),
			     bit_mgr.get_concrete (32_bits));
  ASSERT_SAME_SYMBOLIC_NODE
    (abxy,
     mgr.get_concat_bits ({&char_a, &char_b, &char_x, &char_y}));
  abxy.to_json ();
  for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
    ASSERT_EQ (abxy.scan_for_first_zero_byte (i),
	       string_scan::beyond_domain (4_bytes));

  /* Verify concat of concat.
     This doesn't repeat right now; it instead unwraps everything.  */
  auto &abab
    = mgr.get_concat_bits ({&ab, &ab});
  ASSERT_SAME_SYMBOLIC_NODE (abab.get_size (),
			     bit_mgr.get_concrete (32_bits));
  ASSERT_SAME_SYMBOLIC_NODE
    (abab,
     mgr.get_concat_bits ({&char_a, &char_b, &char_a, &char_b}));
  for (concrete_byte_offset i = 0_bytes; i < 4_bytes; ++i)
    ASSERT_EQ (abab.scan_for_first_zero_byte (i),
	       string_scan::beyond_domain (4_bytes));
}

static void
test_cropped ()
{
  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &a = bv_mgr.get_named_test_bit_vector (10_bytes, "A");
  auto &b = bv_mgr.get_named_test_bit_vector (10_bytes, "B");

  a.to_json ();

  /* Verify the "can't fold" case, and dumping.  */
  {
    auto &bits = a.get_cropped_bytes (concrete_byte_range (5_bytes, 2_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bits.get_size (),
			       bit_mgr.get_concrete (16_bits));
    ASSERT_SYMBOLIC_NODE_DUMP_EQ
      (bits,
       ("cropped_bit_vector[size: 2 bytes]: [bytes 5..7)\n"
	"`- uncropped: named_test_bit_vector[size: 10 bytes]: 'A'\n"));
    bits.to_json ();
    for (concrete_byte_offset i = 0_bytes; i < 2_bytes; ++i)
      ASSERT_EQ (bits.scan_for_first_zero_byte (i),
		 string_scan::cannot_scan (i));
    for (concrete_byte_offset i = 2_bytes; i < 5_bytes; ++i)
      ASSERT_EQ (bits.scan_for_first_zero_byte (i),
		 string_scan::beyond_domain (i));

    // Verify that consolidation works
    ASSERT_SAME_SYMBOLIC_NODE
      (a.get_cropped_bytes (concrete_byte_range (5_bytes, 2_bytes)),
       bits);

    // Verify that we don't erroneously consolidate...
    // ...different inner bit_vector:
    ASSERT_DIFFERENT_SYMBOLIC_NODE
      (b.get_cropped_bytes (concrete_byte_range (2_bytes, 2_bytes)),
       bits);
    // ...different start offset:
    ASSERT_DIFFERENT_SYMBOLIC_NODE
      (a.get_cropped_bytes (concrete_byte_range (6_bytes, 2_bytes)),
       bits);
    // ...different size:
    ASSERT_DIFFERENT_SYMBOLIC_NODE
      (a.get_cropped_bytes (concrete_byte_range (5_bytes, 1_byte)),
       bits);
  }

  /* Verify that A[0..size(A)] --> A.  */
  {
    auto &bits = a.get_cropped_bytes (concrete_byte_range (0_bytes, 10_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bits, a);
  }

  /* Verify that A[(empty)] --> empty.  */
  {
    auto &bits = a.get_cropped_bytes (concrete_byte_range (0_bytes, 0_bytes));
    ASSERT_SAME_SYMBOLIC_NODE (bits, bv_mgr.get_empty ());
  }

  /* Verify that concat(A[p, q], A[q, r]) --> A[p, r].  */
  {
    auto &a35 = a.get_cropped_bytes (concrete_byte_range (3_bytes, 3_bytes));
    ASSERT_SAME_SYMBOLIC_NODE
      (a35.get_size (),
       bit_mgr.get_concrete (bits_from_bytes (3_bytes)));
    auto &a67 = a.get_cropped_bytes (concrete_byte_range (6_bytes, 2_bytes));
    ASSERT_SAME_SYMBOLIC_NODE
      (a67.get_size (),
       bit_mgr.get_concrete (bits_from_bytes (2_bytes)));
    auto &a35a67 = bv_mgr.get_concat_bits ({&a35, &a67});
    ASSERT_SAME_SYMBOLIC_NODE
      (a35a67.get_size (),
       bit_mgr.get_concrete (bits_from_bytes (5_bytes)));
    ASSERT_SAME_SYMBOLIC_NODE
      (a35a67,
       a.get_cropped_bytes (concrete_byte_range (3_bytes, 5_bytes)));
  }

  /* Verify that concat(A[0, n], A[n, size(A)]) --> A.  */
  {
    auto &a06 = a.get_cropped_bytes (concrete_byte_range (0_bytes, 7_bytes));
    auto &a79 = a.get_cropped_bytes (concrete_byte_range (7_bytes, 3_bytes));
    auto &a06a79 = bv_mgr.get_concat_bits ({&a06, &a79});
    ASSERT_SAME_SYMBOLIC_NODE (a06a79, a);
  }

  /* Verify crop of crop: e.g. A[5..10][2..3] --> A[7..8].  */
  {
    auto &a59 = a.get_cropped_bytes (concrete_byte_range (5_bytes, 5_bytes));
    auto &a59_23
      = a59.get_cropped_bytes (concrete_byte_range (2_bytes, 2_bytes));
    ASSERT_SAME_SYMBOLIC_NODE
      (a59_23,
       a.get_cropped_bytes (concrete_byte_range (7_bytes, 2_bytes)));
  }

  // Verify poisoned(x)[m..n] --> poisoned(x, sizeof(m..n)).
  {
    auto &bits
      = (bv_mgr.get_poisoned_bits (bits_from_bytes (10_bytes),
				   poison_kind::uninit)
	 .get_cropped_bytes (concrete_byte_range (5_bytes, 2_bytes)));
    ASSERT_SAME_SYMBOLIC_NODE
      (bits.get_size (),
       bit_mgr.get_concrete (bits_from_bytes (2_bytes)));
    ASSERT_SAME_SYMBOLIC_NODE
      (bits,
       bv_mgr.get_poisoned_bits (16_bits, poison_kind::uninit));
  }

  // Verify tristate(x)[m..n] --> tristate(x, sizeof(m..n)).
  {
    auto &bits
      = (bv_mgr.get_all_zeros (10_bytes)
	 .get_cropped_bytes (concrete_byte_range (5_bytes, 2_bytes)));
    ASSERT_SAME_SYMBOLIC_NODE
      (bits.get_size (),
       bit_mgr.get_concrete (bits_from_bytes (2_bytes)));
    ASSERT_SAME_SYMBOLIC_NODE (bits, bv_mgr.get_all_zeros (16_bits));
  }

  /* crop of concat e.g.:
       concat(X1, ... Xn)[a, b] --> concat(X1[...], ..., Xn[...])
  */
  {
    auto &x = bv_mgr.get_named_test_bit_vector (1_byte, "X");
    auto &y = bv_mgr.get_named_test_bit_vector (1_byte, "Y");
    auto &z = bv_mgr.get_named_test_bit_vector (1_byte, "Z");
    auto &xyz = bv_mgr.get_concat_bits ({&x, &y, &z});
    ASSERT_SAME_SYMBOLIC_NODE
      (xyz.get_cropped_bytes (concrete_byte_range (0_bytes, 1_byte)),
       x);
    ASSERT_SAME_SYMBOLIC_NODE
      (xyz.get_cropped_bytes (concrete_byte_range (1_byte, 1_byte)),
       y);
    ASSERT_SAME_SYMBOLIC_NODE
      (xyz.get_cropped_bytes (concrete_byte_range (2_bytes, 1_byte)),
       z);
    ASSERT_SAME_SYMBOLIC_NODE
      (xyz.get_cropped_bytes (concrete_byte_range (0_bytes, 2_bytes)),
       bv_mgr.get_concat_bits ({&x, &y}));
    ASSERT_SAME_SYMBOLIC_NODE
      (xyz.get_cropped_bits (concrete_bit_range (3_bits, 9_bits)),
       bv_mgr.get_concat_bits
       ({&x.get_suffix (5_bits), &y.get_prefix (4_bits)}));
  }

  {
    /* 20 bytes: 5 repeats of a 4 byte vector:
       If x is [abcd] then we have bytes 0-19:
       .           1111111111
       . 01234567890123456789
       . abcdabcdabcdabcdabcd  */
    auto &x = bv_mgr.get_named_test_bit_vector (4_bytes, "X");
    auto &x_times_5 = bv_mgr.get_repeated_bits (x, 5);
    ASSERT_SAME_SYMBOLIC_NODE
      (x_times_5.get_cropped_bytes (concrete_byte_range (0_bytes, 20_bytes)),
       x_times_5);

    /* Getting individual bytes.  */
    for (concrete_byte_offset i = 0_bytes; i < 20_bytes; ++i)
      ASSERT_SAME_SYMBOLIC_NODE
	(x_times_5.get_byte_at_offset (i),
	 x.get_byte_at_offset (i.floor_mod (4_bytes)));

    // Cropping on an exact boundary should give us X again:
    ASSERT_SAME_SYMBOLIC_NODE
      (x_times_5.get_cropped_bytes (concrete_byte_range (0_bytes, 4_bytes)),
       x);
    ASSERT_SAME_SYMBOLIC_NODE
      (x_times_5.get_cropped_bytes (concrete_byte_range (4_bytes, 4_bytes)),
       x);

    /* Cropping at awkward boundaries.  */
    /* Bytes [3,5).  */
    ASSERT_SAME_SYMBOLIC_NODE
      (x_times_5.get_cropped_bytes (concrete_byte_range (3_bytes, 2_bytes)),
       bv_mgr.get_concat_bits
       ({&x.get_suffix (1_byte),
	 &x.get_prefix (1_byte)}));

    /* Bytes [3,19):
       .           1111111111
       . 01234567890123456789
       . abcdabcdabcdabcdabcd
       .   [PQQQQQQQQQQQQRRR]
       should give us:
       - P: the last byte of X
       - Q: 3 repeats of X
       - R: the first 3 bytes of X.  */
    ASSERT_SAME_SYMBOLIC_NODE
      (x_times_5.get_cropped_bytes (concrete_byte_range (3_bytes, 16_bytes)),
       bv_mgr.get_concat_bits
       ({&x.get_cropped_bytes (concrete_byte_range (3_bytes, 1_byte)),
	 &bv_mgr.get_repeated_bits (x, 3),
	 &x.get_cropped_bytes (concrete_byte_range (0_bytes, 3_bytes))}));
  }
}

static void
test_named_test_bit_vector ()
{
  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &x = bv_mgr.get_named_test_bit_vector (1024_bytes, "X");
  ASSERT_SAME_SYMBOLIC_NODE
    (x.get_size (),
     bit_mgr.get_concrete (bits_from_bytes (1024_bytes)));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (x,
     "named_test_bit_vector[size: 1024 bytes]: 'X'\n");
  x.to_json ();
}

static void
test_poisoned ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();

  auto &uninit_bit = mgr.get_poisoned_bits (1_bit, poison_kind::uninit);

  ASSERT_SAME_SYMBOLIC_NODE (uninit_bit.get_size (),
			     bit_mgr.get_concrete (1_bit));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (uninit_bit,
				"poisoned_bit_vector[size: 1 bit]: uninit\n");

  ASSERT_SAME_SYMBOLIC_NODE
    (mgr.get_poisoned_bits (0_bits, poison_kind::uninit),
     mgr.get_empty ());

  uninit_bit.to_json ();

  auto &freed_bit = mgr.get_poisoned_bits (1_bit, poison_kind::freed);
  ASSERT_SAME_SYMBOLIC_NODE (freed_bit.get_size (),
			     bit_mgr.get_concrete (1_bit));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (freed_bit,
				"poisoned_bit_vector[size: 1 bit]: freed\n");

  freed_bit.to_json ();

  auto &three_uninit_bits
    = mgr.get_poisoned_bits (3_bits, poison_kind::uninit);
  ASSERT_SAME_SYMBOLIC_NODE (three_uninit_bits.get_size (),
			     bit_mgr.get_concrete (3_bits));
  ASSERT_SYMBOLIC_NODE_DUMP_EQ (three_uninit_bits,
				"poisoned_bit_vector[size: 3 bits]: uninit\n");
}

static void
test_repeated ()
{
  symbolic::node_manager node_mgr;
  auto &mgr = node_mgr.get_bit_vector_manager ();

  auto &byte_00 = mgr.get_all_zeros (1_byte);
  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (byte_00,
     "tristate_bit_vector[size: 1 byte]: false bits\n");

  /* Verify that repeats of tristate_bit_vector are folded to
     tristate_bit_vector.  */
  auto &kilobyte_of_zeros = mgr.get_repeated_bits (byte_00, 1024);
  ASSERT_SAME_SYMBOLIC_NODE (kilobyte_of_zeros, mgr.get_all_zeros (1024_bytes));

  kilobyte_of_zeros.to_json ();

  // Verify that 0 repeats is the empty bit vector:
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_repeated_bits (byte_00, 0),
			     mgr.get_empty ());

  // Verify that we fold repeats of repeats into repeats:
  auto &char_A
    = mgr.get_bits_for_constant (build_int_cst (char_type_node, 'A'));
  auto &ten_As = mgr.get_repeated_bits (char_A, 10);
  auto &two_times_ten_As = mgr.get_repeated_bits (ten_As, 2);
  ASSERT_SAME_SYMBOLIC_NODE
    (two_times_ten_As,
     mgr.get_repeated_bits (char_A, 20));

  {
    pretty_printer pp;
    hexdump_options hexdump_opts;
    hexdump_opts.m_bytes_per_line = 10_bytes;
    two_times_ten_As.hexdump_to_pp (&pp, hexdump_opts);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       "+--------------+-----------------------------+----------+\n"
       "| [bytes 0..10)|41 41 41 41 41 41 41 41 41 41|AAAAAAAAAA|\n"
       "+--------------+-----------------------------+----------+\n"
       "|[bytes 10..20)|41 41 41 41 41 41 41 41 41 41|AAAAAAAAAA|\n"
       "+--------------+-----------------------------+----------+\n");
  }
  {
    pretty_printer pp;
    hexdump_options hexdump_opts;
    hexdump_opts.m_show_binary = true;
    two_times_ten_As.hexdump_to_pp (&pp, hexdump_opts);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("+--------------+-----------------------------------+-----------+----+\n"
	"|  [bytes 0..4)|10000010 10000010 10000010 10000010|41 41 41 41|AAAA|\n"
	"+--------------+-----------------------------------+-----------+----+\n"
	"|  [bytes 4..8)|10000010 10000010 10000010 10000010|41 41 41 41|AAAA|\n"
	"+--------------+-----------------------------------+-----------+----+\n"
	"| [bytes 8..12)|10000010 10000010 10000010 10000010|41 41 41 41|AAAA|\n"
	"+--------------+-----------------------------------+-----------+----+\n"
	"|[bytes 12..16)|10000010 10000010 10000010 10000010|41 41 41 41|AAAA|\n"
	"+--------------+-----------------------------------+-----------+----+\n"
	"|[bytes 16..20)|10000010 10000010 10000010 10000010|41 41 41 41|AAAA|\n"
	"+--------------+-----------------------------------+-----------+----+\n"));
  }

  // Verify that repeat(A, 1) --> A.
  ASSERT_SAME_SYMBOLIC_NODE (mgr.get_repeated_bits (char_A, 1), char_A);
}

static void
test_bit_vector_from_svalue ()
{
  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &rmm = node_mgr.get_svalue_manager ();

  auto *unknown_char_sval
    = rmm.get_or_create_unknown_svalue (unsigned_char_type_node);
  ASSERT_SAME_SYMBOLIC_NODE
    (bv_mgr.get_bit_vector_from_svalue (*unknown_char_sval),
     bv_mgr.get_unknown_bits (8_bits));

  unknown_char_sval->to_json ();

  /* Verify that we remove redundant casts
     e.g. BIT_VECTOR_FOR (UNARY_OP(signed, NOP_EXPR, SVAL(unsigned)))
     folds to BIT_VECTOR_FOR (SVAL(unsigned)).  */
  {
    placeholder_svalue sval (node_mgr.alloc_node_id (),
			     integer_type_node, "x");
    auto *cast_sval = rmm.get_or_create_cast (unsigned_type_node, &sval);
    ASSERT_DIFFERENT_SYMBOLIC_NODE (sval, *cast_sval);
    ASSERT_SAME_SYMBOLIC_NODE (bv_mgr.get_bit_vector_from_svalue (sval),
			       bv_mgr.get_bit_vector_from_svalue (*cast_sval));
  }

  /* Make a bit_vector from bitwise "and" and "or" against a concrete mask to
     verify what we "know" about individual bits and bytes (using the
     irange_bitmask).  */
  {
    placeholder_svalue y (node_mgr.alloc_node_id (),
			  uint32_type_node, "y");
    auto *mask = rmm.get_or_create_int_cst (uint32_type_node, 0xff00ff00);

    hexdump_options hexdump_opts;
    hexdump_opts.m_show_binary = true;
    hexdump_opts.m_show_ascii = false;

    {
      auto *bitwise_and = rmm.get_or_create_binop (uint32_type_node,
						   BIT_AND_EXPR,
						   &y,
						   mask);
      auto &bv = bv_mgr.get_bit_vector_from_svalue (*bitwise_and);

      // Check what we know about individual bits in the bit_vector
      for (auto i = 0_bits; i < 8_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate (false));
      for (auto i = 8_bits; i < 16_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate::unknown ());
      for (auto i = 16_bits; i < 24_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate (false));
      for (auto i = 24_bits; i < 32_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate::unknown ());

      // Check what we know about individual bytes in the bit_vector
      ASSERT_SAME_SYMBOLIC_NODE (bv.get_byte_at_offset (0_bytes),
				 bv_mgr.get_uint8 (0));
      ASSERT_SAME_SYMBOLIC_NODE (bv.get_byte_at_offset (2_bytes),
				 bv_mgr.get_uint8 (0));

      pretty_printer pp;
      bv.hexdump_to_pp (&pp, hexdump_opts);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 "+------------+-----------------------------------+-----------+\n"
	 "|[bytes 0..4)|00000000 ???????? 00000000 ????????|00 ?? 00 ??|\n"
	 "+------------+-----------------------------------+-----------+\n");
    }
    {
      auto *bitwise_or = rmm.get_or_create_binop (uint32_type_node,
						  BIT_IOR_EXPR,
						  &y,
						  mask);
      auto &bv = bv_mgr.get_bit_vector_from_svalue (*bitwise_or);

      // Check what we know about individual bits in the bit_vector
      for (auto i = 0_bits; i < 8_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate::unknown ());
      for (auto i = 8_bits; i < 16_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate (true));
      for (auto i = 16_bits; i < 24_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate::unknown ());
      for (auto i = 24_bits; i < 32_bits; ++i)
	ASSERT_EQ (bv.get_tristate_at_offset (i), tristate (true));

      // Check what we know about individual bytes in the bit_vector
      ASSERT_SAME_SYMBOLIC_NODE (bv.get_byte_at_offset (1_byte),
				 bv_mgr.get_uint8 (0xff));
      ASSERT_SAME_SYMBOLIC_NODE (bv.get_byte_at_offset (3_bytes),
				 bv_mgr.get_uint8 (0xff));

      pretty_printer pp;
      bv.hexdump_to_pp (&pp, hexdump_opts);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 "+------------+-----------------------------------+-----------+\n"
	 "|[bytes 0..4)|???????? 11111111 ???????? 11111111|?? ff ?? ff|\n"
	 "+------------+-----------------------------------+-----------+\n");
    }
  }
}

/* Consider this buggy C code (writing to a fixed-size buffer with no
   size checks on inputs):
   |   char buf[1024];
   |   len_a = strlen (str_a);
   |   len_b = strlen (str_b);
   |   memcpy (buf, str_a, len_a);
   |   memcpy (buf + len_a, str_b, len_b);
   |   buf[len_a + len_b] = '\0';
 */
static void
test_example_strcat ()
{
  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();

  auto &strlen_a = byte_mgr.get_named_test_scalar ("strlen(a)");
  auto &strlen_b = byte_mgr.get_named_test_scalar ("strlen(b)");
  auto &a = bv_mgr.get_named_test_bit_vector (strlen_a + 1_byte, "a");
  auto &b = bv_mgr.get_named_test_bit_vector (strlen_b + 1_byte, "b");

  auto &bv_for_buf = bv_mgr.get_concat_bits
    ({&a.get_prefix (strlen_a),
      &b.get_prefix (strlen_b),
      &bv_mgr.get_uint8 (0)});

  ASSERT_SAME_SYMBOLIC_NODE
    (bv_for_buf.get_size (),
     bits_from_bytes (strlen_a + strlen_b + 1_byte));
  ASSERT_SAME_SYMBOLIC_NODE
    (*maybe_bytes_from_bits (bv_for_buf.get_size ()),
     strlen_a + strlen_b + 1_byte);
}

static void
test_from_unfoldable_bit_contents ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &bc_mgr = mgr.get_bit_contents_manager ();

  auto &ubc_a = bc_mgr.get_named_test_bit_contents ("A");
  auto &ubc_b = bc_mgr.get_named_test_bit_contents ("B");

  ASSERT_SYMBOLIC_NODE_DUMP_EQ
    (ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
     ("bit_vector_from_ubc[size: 10 bytes]\n"
      "+- bit_contents: named_test_bit_contents: A\n"
      "`- range: symbolic range\n"
      "   +- start offset: concrete_scalar<bits>: 0\n"
      "   `- size: concrete_scalar<bits>: 80\n"));

  ASSERT_SAME_SYMBOLIC_NODE
    (ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
     ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)));

  ASSERT_DIFFERENT_SYMBOLIC_NODE
    (ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
     ubc_a.get_bytes (concrete_byte_range (0_bytes, 9_bytes)));

  ASSERT_DIFFERENT_SYMBOLIC_NODE
    (ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)),
     ubc_b.get_bytes (concrete_byte_range (0_bytes, 10_bytes)));

  /* Concat of touching ranges from same bit contents should
     be folded to a single range from that bit contents.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (bv_mgr.get_concat_bits
     ({&ubc_a.get_bytes (concrete_byte_range (0_bytes, 4_bytes)),
       &ubc_a.get_bytes (concrete_byte_range (4_bytes, 6_bytes))}),
     ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)));
  /* ...but not for those that don't touch:  */
  ASSERT_DIFFERENT_SYMBOLIC_NODE
    (bv_mgr.get_concat_bits
     ({&ubc_a.get_bytes (concrete_byte_range (0_bytes, 4_bytes)),
       &ubc_a.get_bytes (concrete_byte_range (5_bytes, 5_bytes))}),
     ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)));
  /* ...but not for those from different bit contents:  */
  ASSERT_DIFFERENT_SYMBOLIC_NODE
    (bv_mgr.get_concat_bits
     ({&ubc_a.get_bytes (concrete_byte_range (0_bytes, 4_bytes)),
       &ubc_b.get_bytes (concrete_byte_range (4_bytes, 6_bytes))}),
     ubc_a.get_bytes (concrete_byte_range (0_bytes, 10_bytes)));

  /* Cropping a range from a ubc should give the appropriate
     subrange from the ubc.  */
  auto &a_bytes_10_14
    = ubc_a.get_bytes (concrete_byte_range (10_bytes, 5_bytes));
  ASSERT_SAME_SYMBOLIC_NODE
    (a_bytes_10_14.get_cropped_bytes (concrete_byte_range (0_bytes, 5_bytes)),
     a_bytes_10_14);
  ASSERT_SAME_SYMBOLIC_NODE
    (a_bytes_10_14.get_cropped_bytes (concrete_byte_range (0_bytes, 1_byte)),
     ubc_a.get_bytes (concrete_byte_range (10_bytes, 1_byte)));
  ASSERT_SAME_SYMBOLIC_NODE
    (a_bytes_10_14.get_cropped_bytes (concrete_byte_range (4_bytes, 1_byte)),
     ubc_a.get_bytes (concrete_byte_range (14_bytes, 1_byte)));
}

/* Test type-punning uint32_t to bytes, using the target's endianness.  */

static void
test_uint32_t_to_bytes ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &rmm = mgr.get_region_model_manager ();

  auto &uint32_0x12345678
    = *rmm.get_or_create_int_cst (uint32_type_node, 0x12345678);
  auto &bv_0x12345678 = bv_mgr.get_bit_vector_from_svalue (uint32_0x12345678);

  auto &uint8_0x12
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x12);
  auto &bv_0x12 = bv_mgr.get_bit_vector_from_svalue (uint8_0x12);

  auto &uint8_0x34
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x34);
  auto &bv_0x34 = bv_mgr.get_bit_vector_from_svalue (uint8_0x34);

  auto &uint8_0x56
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x56);
  auto &bv_0x56 = bv_mgr.get_bit_vector_from_svalue (uint8_0x56);

  auto &uint8_0x78
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x78);
  auto &bv_0x78 = bv_mgr.get_bit_vector_from_svalue (uint8_0x78);

  auto &bv_byte_0 = bv_0x12345678.get_byte_at_offset (0_bytes);
  auto &bv_byte_1 = bv_0x12345678.get_byte_at_offset (1_byte);
  auto &bv_byte_2 = bv_0x12345678.get_byte_at_offset (2_bytes);
  auto &bv_byte_3 = bv_0x12345678.get_byte_at_offset (3_bytes);

  if (BYTES_BIG_ENDIAN)
    {
      ASSERT_SAME_SYMBOLIC_NODE (bv_byte_0, bv_0x12);
      ASSERT_SAME_SYMBOLIC_NODE (bv_byte_1, bv_0x34);
      ASSERT_SAME_SYMBOLIC_NODE (bv_byte_2, bv_0x56);
      ASSERT_SAME_SYMBOLIC_NODE (bv_byte_3, bv_0x78);
    }
  else
    {
      /* Little-endian.  */
      if (!WORDS_BIG_ENDIAN)
	{
	  ASSERT_SAME_SYMBOLIC_NODE (bv_byte_0, bv_0x78);
	  ASSERT_SAME_SYMBOLIC_NODE (bv_byte_1, bv_0x56);
	  ASSERT_SAME_SYMBOLIC_NODE (bv_byte_2, bv_0x34);
	  ASSERT_SAME_SYMBOLIC_NODE (bv_byte_3, bv_0x12);
	}
    }
}

/* Test type-punning bytes to uint32_t, using the target's endianness.  */

static void
test_bytes_to_uint32_t ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &rmm = mgr.get_region_model_manager ();

  auto &uint8_0x12
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x12);
  auto &bv_0x12 = bv_mgr.get_bit_vector_from_svalue (uint8_0x12);

  auto &uint8_0x34
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x34);
  auto &bv_0x34 = bv_mgr.get_bit_vector_from_svalue (uint8_0x34);

  auto &uint8_0x56
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x56);
  auto &bv_0x56 = bv_mgr.get_bit_vector_from_svalue (uint8_0x56);

  auto &uint8_0x78
    = *rmm.get_or_create_int_cst (unsigned_char_type_node, 0x78);
  auto &bv_0x78 = bv_mgr.get_bit_vector_from_svalue (uint8_0x78);

  auto &bytes
    = bv_mgr.get_concat_bits  ({&bv_0x12, &bv_0x34, &bv_0x56, &bv_0x78});

  const svalue *sval = rmm.get_svalue_from_bit_vector (uint32_type_node, bytes);

  const uint32_t expected_uint32 = (BYTES_BIG_ENDIAN
				    ? 0x12345678
				    : (WORDS_BIG_ENDIAN
				       ? 0x34127856
				       : 0x78563412));
  ASSERT_SAME_SYMBOLIC_NODE
    (*sval,
     *rmm.get_or_create_int_cst (uint32_type_node, expected_uint32));
}

static void
test_every_byte_value ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();

  std::vector<const symbolic::bit_vector *> vec_of_every_byte_value;
  for (int i = 0; i < 256; ++i)
    vec_of_every_byte_value.push_back (&bv_mgr.get_uint8 (i));
  auto &every_byte_value_bv
    = bv_mgr.get_concat_bits (std::move (vec_of_every_byte_value));

  {
    hexdump_options hexdump_opts;
    hexdump_opts.m_show_binary = true;
    hexdump_opts.m_show_hex = true;
    hexdump_opts.m_show_ascii = false;

    /* Hexdump of first 8 bytes.  */
    {
      pretty_printer pp;
      every_byte_value_bv
	.get_cropped_bytes (concrete_byte_range (0_bytes, 8_bytes))
	.hexdump_to_pp (&pp, hexdump_opts);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 ("+------------+-----------------------------------+-----------+\n"
	  "|[bytes 0..4)|00000000 10000000 01000000 11000000|00 01 02 03|\n"
	  "+------------+-----------------------------------+-----------+\n"
	  "|[bytes 4..8)|00100000 10100000 01100000 11100000|04 05 06 07|\n"
	  "+------------+-----------------------------------+-----------+\n"));
    }

    /* Hexdump of last 8 bytes.  */
    {
      pretty_printer pp;
      every_byte_value_bv
	.get_cropped_bytes (concrete_byte_range (248_bytes, 8_bytes))
	.hexdump_to_pp (&pp, hexdump_opts);
      ASSERT_STREQ
	(pp_formatted_text (&pp),
	 ("+------------+-----------------------------------+-----------+\n"
	  "|[bytes 0..4)|00011111 10011111 01011111 11011111|f8 f9 fa fb|\n"
	  "+------------+-----------------------------------+-----------+\n"
	  "|[bytes 4..8)|00111111 10111111 01111111 11111111|fc fd fe ff|\n"
	  "+------------+-----------------------------------+-----------+\n"));
    }
  }
}

/* Run all of the selftests within this file.  */

void
analyzer_symbolic_bit_vectors_cc_tests ()
{
  test_maybe_set_irange_bitmask_from_concrete_tristate_vector ();
  test_tristate ();
  test_integer_constants ();
  test_bytes ();
  test_string_constants ();
  test_raw_data_constant_1 ();
  test_raw_data_constant_2 ();
  test_raw_data_constant_empty ();
  test_concat ();
  test_cropped ();
  test_named_test_bit_vector ();
  test_poisoned ();
  test_repeated ();
  test_bit_vector_from_svalue ();
  test_from_unfoldable_bit_contents ();
  test_example_strcat ();
  test_uint32_t_to_bytes ();
  test_bytes_to_uint32_t ();
  test_every_byte_value ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
