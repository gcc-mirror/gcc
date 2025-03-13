/* Classes for modeling the state of memory.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
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
#include "bitmap.h"
#include "selftest.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cfg.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/call-summary.h"
#include "analyzer/analyzer-selftests.h"
#include "stor-layout.h"
#include "text-art/tree-widget.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

/* Dump SVALS to PP, sorting them to ensure determinism.  */

static void
dump_svalue_set (const hash_set <const svalue *> &svals,
		 pretty_printer *pp, bool simple)
{
  auto_vec <const svalue *> v;
  for (hash_set<const svalue *>::iterator iter = svals.begin ();
       iter != svals.end (); ++iter)
    {
      v.safe_push (*iter);
    }
  v.qsort (svalue::cmp_ptr_ptr);

  pp_character (pp, '{');
  const svalue *sval;
  unsigned i;
  FOR_EACH_VEC_ELT (v, i, sval)
    {
      if (i > 0)
	pp_string (pp, ", ");
      sval->dump_to_pp (pp, simple);
    }
  pp_character (pp, '}');
}

/* class uncertainty_t.  */

/* Dump this object to PP.  */

void
uncertainty_t::dump_to_pp (pretty_printer *pp, bool simple) const
{
  pp_string (pp, "{m_maybe_bound_svals: ");
  dump_svalue_set (m_maybe_bound_svals, pp, simple);

  pp_string (pp, ", m_mutable_at_unknown_call_svals: ");
  dump_svalue_set (m_mutable_at_unknown_call_svals, pp, simple);
  pp_string (pp, "}");
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
uncertainty_t::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
}

/* class binding_key.  */

const binding_key *
binding_key::make (store_manager *mgr, const region *r)
{
  region_offset offset = r->get_offset (mgr->get_svalue_manager ());
  if (offset.symbolic_p ())
    return mgr->get_symbolic_binding (r);
  else
    {
      bit_size_t bit_size;
      if (r->get_bit_size (&bit_size))
	{
	  /* Must be non-empty.  */
	  gcc_assert (bit_size > 0);
	  return mgr->get_concrete_binding (offset.get_bit_offset (),
					    bit_size);
	}
      else
	return mgr->get_symbolic_binding (r);
    }
}

/* Dump this binding_key to stderr.  */

DEBUG_FUNCTION void
binding_key::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
}

/* Get a description of this binding_key.  */

label_text
binding_key::get_desc (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_to_pp (&pp, simple);
  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

/* qsort callback.  */

int
binding_key::cmp_ptrs (const void *p1, const void *p2)
{
  const binding_key * const *pk1 = (const binding_key * const *)p1;
  const binding_key * const *pk2 = (const binding_key * const *)p2;
  return cmp (*pk1, *pk2);
}

/* Comparator for binding_keys.  */

int
binding_key::cmp (const binding_key *k1, const binding_key *k2)
{
  int concrete1 = k1->concrete_p ();
  int concrete2 = k2->concrete_p ();
  if (int concrete_cmp = concrete1 - concrete2)
    return concrete_cmp;
  if (concrete1)
    {
      const concrete_binding *b1 = (const concrete_binding *)k1;
      const concrete_binding *b2 = (const concrete_binding *)k2;
      if (int start_cmp = wi::cmp (b1->get_start_bit_offset (),
				   b2->get_start_bit_offset (),
				   SIGNED))
	return start_cmp;
      return wi::cmp (b1->get_next_bit_offset (), b2->get_next_bit_offset (),
		      SIGNED);
    }
  else
    {
      const symbolic_binding *s1 = (const symbolic_binding *)k1;
      const symbolic_binding *s2 = (const symbolic_binding *)k2;
      if (s1 > s2)
	return 1;
      if (s1 < s2)
	return -1;
      return 0;
    }
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
  auto obj = ::make_unique<json::object> ();
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

/* If MASK is a contiguous range of set bits, write them
   to *OUT and return true.
   Otherwise return false.  */

bool
bit_range::from_mask (unsigned HOST_WIDE_INT mask, bit_range *out)
{
  unsigned iter_bit_idx = 0;
  unsigned HOST_WIDE_INT iter_bit_mask = 1;

  /* Find the first contiguous run of set bits in MASK.  */

  /* Find first set bit in MASK.  */
  while (iter_bit_idx < HOST_BITS_PER_WIDE_INT)
    {
      if (mask & iter_bit_mask)
	break;
      iter_bit_idx++;
      iter_bit_mask <<= 1;
    }
  if (iter_bit_idx == HOST_BITS_PER_WIDE_INT)
    /* MASK is zero.  */
    return false;

  unsigned first_set_iter_bit_idx = iter_bit_idx;
  unsigned num_set_bits = 1;
  iter_bit_idx++;
  iter_bit_mask <<= 1;

  /* Find next unset bit in MASK.  */
  while (iter_bit_idx < HOST_BITS_PER_WIDE_INT)
    {
      if (!(mask & iter_bit_mask))
	break;
      num_set_bits++;
      iter_bit_idx++;
      iter_bit_mask <<= 1;
    }
  if (iter_bit_idx == HOST_BITS_PER_WIDE_INT)
    {
      *out = bit_range (first_set_iter_bit_idx, num_set_bits);
      return true;
    }

  /* We now have the first contiguous run of set bits in MASK.
     Fail if any other bits are set.  */
  while (iter_bit_idx < HOST_BITS_PER_WIDE_INT)
    {
      if (mask & iter_bit_mask)
	return false;
      iter_bit_idx++;
      iter_bit_mask <<= 1;
    }

  *out = bit_range (first_set_iter_bit_idx, num_set_bits);
  return true;
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
  auto obj = ::make_unique<json::object> ();
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

/* class concrete_binding : public binding_key.  */

/* Implementation of binding_key::dump_to_pp vfunc for concrete_binding.  */

void
concrete_binding::dump_to_pp (pretty_printer *pp, bool) const
{
  m_bit_range.dump_to_pp (pp);
}

/* Return true if this binding overlaps with OTHER.  */

bool
concrete_binding::overlaps_p (const concrete_binding &other) const
{
  if (get_start_bit_offset () < other.get_next_bit_offset ()
      && get_next_bit_offset () > other.get_start_bit_offset ())
    return true;
  return false;
}

/* If this is expressible as a concrete byte range, return true
   and write it to *OUT.  Otherwise return false.  */

bool
concrete_binding::get_byte_range (byte_range *out) const
{
  return m_bit_range.as_byte_range (out);
}

/* Comparator for use by vec<const concrete_binding *>::qsort.  */

int
concrete_binding::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const concrete_binding *b1 = *(const concrete_binding * const *)p1;
  const concrete_binding *b2 = *(const concrete_binding * const *)p2;

  return bit_range::cmp (b1->m_bit_range, b2->m_bit_range);
}

/* class symbolic_binding : public binding_key.  */

void
symbolic_binding::dump_to_pp (pretty_printer *pp, bool simple) const
{
  //binding_key::dump_to_pp (pp, simple);
  pp_string (pp, "region: ");
  m_region->dump_to_pp (pp, simple);
}

/* Comparator for use by vec<const symbolic_binding *>::qsort.  */

int
symbolic_binding::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const symbolic_binding *b1 = *(const symbolic_binding * const *)p1;
  const symbolic_binding *b2 = *(const symbolic_binding * const *)p2;

  return region::cmp_ids (b1->get_region (), b2->get_region ());
}

/* The store is oblivious to the types of the svalues bound within
   it: any type can get bound at any location.
   Simplify any casts before binding.

   For example, if we have:
     struct big { int ia[1024]; };
     struct big src, dst;
     memcpy (&dst, &src, sizeof (struct big));
   this reaches us in gimple form as:
     MEM <unsigned char[4096]> [(char * {ref-all})&dst]
       = MEM <unsigned char[4096]> [(char * {ref-all})&src];
   Using cast_region when handling the MEM_REF would give us:
     INIT_VAL(CAST_REG(unsigned char[4096], src))
   as rhs_sval, but we can fold that into a cast svalue:
     CAST(unsigned char[4096], INIT_VAL(src))
   We can discard that cast from the svalue when binding it in
   the store for "dst", and simply store:
     cluster for: dst
       key:   {kind: direct, start: 0, size: 32768, next: 32768}
       value: ‘struct big’ {INIT_VAL(src)}.  */

static const svalue *
simplify_for_binding (const svalue *sval)
{
  if (const svalue *cast_sval = sval->maybe_undo_cast ())
    sval = cast_sval;
  return sval;
}

/* class binding_map.  */

/* binding_map's copy ctor.  */

binding_map::binding_map (const binding_map &other)
: m_map (other.m_map)
{
}

/* binding_map's assignment operator.  */

binding_map&
binding_map::operator=(const binding_map &other)
{
  /* For now, assume we only ever copy to an empty cluster.  */
  gcc_assert (m_map.elements () == 0);
  for (map_t::iterator iter = other.m_map.begin (); iter != other.m_map.end ();
       ++iter)
    {
      const binding_key *key = (*iter).first;
      const svalue *sval = (*iter).second;
      m_map.put (key, sval);
    }
  return *this;
}

/* binding_map's equality operator.  */

bool
binding_map::operator== (const binding_map &other) const
{
  if (m_map.elements () != other.m_map.elements ())
    return false;

  for (map_t::iterator iter = m_map.begin (); iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      const svalue *sval = (*iter).second;
      const svalue **other_slot
	= const_cast <map_t &> (other.m_map).get (key);
      if (other_slot == NULL)
	return false;
      if (sval != *other_slot)
	return false;
    }
  gcc_checking_assert (hash () == other.hash ());
  return true;
}

/* Generate a hash value for this binding_map.  */

hashval_t
binding_map::hash () const
{
  hashval_t result = 0;
  for (map_t::iterator iter = m_map.begin (); iter != m_map.end (); ++iter)
    {
      /* Use a new hasher for each key to avoid depending on the ordering
	 of keys when accumulating the result.  */
      inchash::hash hstate;
      hstate.add_ptr ((*iter).first);
      hstate.add_ptr ((*iter).second);
      result ^= hstate.end ();
    }
  return result;
}

/* Dump a representation of this binding_map to PP.
   SIMPLE controls how values and regions are to be printed.
   If MULTILINE, then split the dump over multiple lines and
   use whitespace for readability, otherwise put all on one line.  */

void
binding_map::dump_to_pp (pretty_printer *pp, bool simple,
			 bool multiline) const
{
  auto_vec <const binding_key *> binding_keys;
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      binding_keys.safe_push (key);
    }
  binding_keys.qsort (binding_key::cmp_ptrs);

  const binding_key *key;
  unsigned i;
  FOR_EACH_VEC_ELT (binding_keys, i, key)
    {
      const svalue *value = *const_cast <map_t &> (m_map).get (key);
      if (multiline)
	{
	  pp_string (pp, "    key:   {");
	  key->dump_to_pp (pp, simple);
	  pp_string (pp, "}");
	  pp_newline (pp);
	  pp_string (pp, "    value: ");
	  if (tree t = value->get_type ())
	    dump_quoted_tree (pp, t);
	  pp_string (pp, " {");
	  value->dump_to_pp (pp, simple);
	  pp_string (pp, "}");
	  pp_newline (pp);
	}
      else
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  pp_string (pp, "binding key: {");
	  key->dump_to_pp (pp, simple);
	  pp_string (pp, "}, value: {");
	  value->dump_to_pp (pp, simple);
	  pp_string (pp, "}");
	}
    }
}

/* Dump a multiline representation of this binding_map to stderr.  */

DEBUG_FUNCTION void
binding_map::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple, true);
  pp_newline (&pp);
}

/* Return a new json::object of the form
   {KEY_DESC : SVALUE_DESC,
    ...for the various key/value pairs in this binding_map}.  */

std::unique_ptr<json::object>
binding_map::to_json () const
{
  auto map_obj = ::make_unique<json::object> ();

  auto_vec <const binding_key *> binding_keys;
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      binding_keys.safe_push (key);
    }
  binding_keys.qsort (binding_key::cmp_ptrs);

  const binding_key *key;
  unsigned i;
  FOR_EACH_VEC_ELT (binding_keys, i, key)
    {
      const svalue *value = *const_cast <map_t &> (m_map).get (key);
      label_text key_desc = key->get_desc ();
      map_obj->set (key_desc.get (), value->to_json ());
    }

  return map_obj;
}

/* Add a child to PARENT_WIDGET expressing a binding between
   KEY and SVAL.  */

static void
add_binding_to_tree_widget (text_art::tree_widget &parent_widget,
			    const text_art::dump_widget_info &dwi,
			    const binding_key *key,
			    const svalue *sval)
{
  pretty_printer the_pp;
  pretty_printer * const pp = &the_pp;
  pp_format_decoder (pp) = default_tree_printer;
  pp_show_color (pp) = true;
  const bool simple = true;

  key->dump_to_pp (pp, simple);
  pp_string (pp, ": ");
  if (tree t = sval->get_type ())
    dump_quoted_tree (pp, t);
  pp_string (pp, " {");
  sval->dump_to_pp (pp, simple);
  pp_string (pp, "}");

  parent_widget.add_child (text_art::tree_widget::make (dwi, pp));
}

void
binding_map::add_to_tree_widget (text_art::tree_widget &parent_widget,
				 const text_art::dump_widget_info &dwi) const
{
  auto_vec <const binding_key *> binding_keys;
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      binding_keys.safe_push (key);
    }
  binding_keys.qsort (binding_key::cmp_ptrs);

  const binding_key *key;
  unsigned i;
  FOR_EACH_VEC_ELT (binding_keys, i, key)
    {
      const svalue *sval = *const_cast <map_t &> (m_map).get (key);
      add_binding_to_tree_widget (parent_widget, dwi,
				  key, sval);
    }
}


/* Comparator for imposing an order on binding_maps.  */

int
binding_map::cmp (const binding_map &map1, const binding_map &map2)
{
  if (int count_cmp = map1.elements () - map2.elements ())
    return count_cmp;

  auto_vec <const binding_key *> keys1 (map1.elements ());
  for (map_t::iterator iter = map1.begin ();
       iter != map1.end (); ++iter)
    keys1.quick_push ((*iter).first);
  keys1.qsort (binding_key::cmp_ptrs);

  auto_vec <const binding_key *> keys2 (map2.elements ());
  for (map_t::iterator iter = map2.begin ();
       iter != map2.end (); ++iter)
    keys2.quick_push ((*iter).first);
  keys2.qsort (binding_key::cmp_ptrs);

  for (size_t i = 0; i < keys1.length (); i++)
    {
      const binding_key *k1 = keys1[i];
      const binding_key *k2 = keys2[i];
      if (int key_cmp = binding_key::cmp (k1, k2))
	return key_cmp;
      gcc_assert (k1 == k2);
      if (int sval_cmp = svalue::cmp_ptr (map1.get (k1), map2.get (k2)))
	return sval_cmp;
    }

  return 0;
}

/* Get the child region of PARENT_REG based upon INDEX within a
   CONSTRUCTOR.   */

static const region *
get_subregion_within_ctor (const region *parent_reg, tree index,
			   region_model_manager *mgr)
{
  switch (TREE_CODE (index))
    {
    default:
      gcc_unreachable ();
    case INTEGER_CST:
      {
	const svalue *index_sval
	  = mgr->get_or_create_constant_svalue (index);
	return mgr->get_element_region (parent_reg,
					TREE_TYPE (parent_reg->get_type ()),
					index_sval);
      }
      break;
    case FIELD_DECL:
      return mgr->get_field_region (parent_reg, index);
    }
}

/* Get the child region of PARENT_REG based upon (INDEX, VALUE) within a
   CONSTRUCTOR.   */

static const region *
get_subregion_within_ctor_for_ctor_pair (const region *parent_reg,
					 tree index,
					 tree value,
					 region_model_manager *mgr)
{
  if (TREE_CODE (index) == INTEGER_CST
      && TREE_CODE (value) == RAW_DATA_CST)
    {
      /* Special-case; see tree.def's description of CONSTRUCTOR.
	 We have RAW_DATA_LENGTH of bytes, starting at INDEX's start.  */
      const region *start_reg
	= get_subregion_within_ctor (parent_reg, index, mgr);
      /* Build a bit range, relative to PARENT_REG.  */
      region_offset start_offset = start_reg->get_offset (mgr);

      if (!start_offset.concrete_p ())
	return nullptr;
      bit_offset_t start_bit_offset = start_offset.get_bit_offset ();
      int length = RAW_DATA_LENGTH (value);
      bit_range bits (start_bit_offset, length * BITS_PER_UNIT);

      return mgr->get_bit_range (parent_reg, NULL_TREE, bits);
    }

  return get_subregion_within_ctor (parent_reg, index, mgr);
}

/* Get the svalue for VAL, a non-CONSTRUCTOR value within a CONSTRUCTOR.  */

static const svalue *
get_svalue_for_ctor_val (tree val, region_model_manager *mgr)
{
  /* Reuse the get_rvalue logic from region_model.  */
  region_model m (mgr);
  return m.get_rvalue (path_var (val, 0), NULL);
}

/* Bind values from CONSTRUCTOR to this map, relative to
   PARENT_REG's relationship to its base region.
   Return true if successful, false if there was a problem (e.g. due
   to hitting a complexity limit).  */

bool
binding_map::apply_ctor_to_region (const region *parent_reg, tree ctor,
				   region_model_manager *mgr)
{
  gcc_assert (parent_reg);
  gcc_assert (TREE_CODE (ctor) == CONSTRUCTOR);

  unsigned ix;
  tree index;
  tree val;
  tree parent_type = parent_reg->get_type ();
  tree field;
  if (TREE_CODE (parent_type) == RECORD_TYPE)
    field = TYPE_FIELDS (parent_type);
  else
    field = NULL_TREE;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), ix, index, val)
    {
      if (!index)
	{
	  /* If index is NULL, then iterate through the fields for
	     a RECORD_TYPE, or use an INTEGER_CST otherwise.
	     Compare with similar logic in output_constructor.  */
	  if (field)
	    {
	      index = field;
	      field = DECL_CHAIN (field);
	    }
	  else
	    index = build_int_cst (integer_type_node, ix);
	}
      else if (TREE_CODE (index) == RANGE_EXPR)
	{
	  tree min_index = TREE_OPERAND (index, 0);
	  tree max_index = TREE_OPERAND (index, 1);
	  if (min_index == max_index)
	    {
	      if (!apply_ctor_pair_to_child_region (parent_reg, mgr,
						    min_index, val))
		return false;
	    }
	  else
	    {
	      if (!apply_ctor_val_to_range (parent_reg, mgr,
					    min_index, max_index, val))
		return false;
	    }
	  continue;
	}
      if (!apply_ctor_pair_to_child_region (parent_reg, mgr, index, val))
	return false;
    }
  return true;
}

/* Bind the value VAL into the range of elements within PARENT_REF
   from MIN_INDEX to MAX_INDEX (including endpoints).
   For use in handling RANGE_EXPR within a CONSTRUCTOR.
   Return true if successful, false if there was a problem (e.g. due
   to hitting a complexity limit).  */

bool
binding_map::apply_ctor_val_to_range (const region *parent_reg,
				      region_model_manager *mgr,
				      tree min_index, tree max_index,
				      tree val)
{
  gcc_assert (TREE_CODE (min_index) == INTEGER_CST);
  gcc_assert (TREE_CODE (max_index) == INTEGER_CST);

  /* Generate a binding key for the range.  */
  const region *min_element
    = get_subregion_within_ctor (parent_reg, min_index, mgr);
  const region *max_element
    = get_subregion_within_ctor (parent_reg, max_index, mgr);
  region_offset min_offset = min_element->get_offset (mgr);
  if (min_offset.symbolic_p ())
    return false;
  bit_offset_t start_bit_offset = min_offset.get_bit_offset ();
  store_manager *smgr = mgr->get_store_manager ();
  if (max_element->empty_p ())
    return false;
  const binding_key *max_element_key = binding_key::make (smgr, max_element);
  if (max_element_key->symbolic_p ())
    return false;
  const concrete_binding *max_element_ckey
    = max_element_key->dyn_cast_concrete_binding ();
  bit_size_t range_size_in_bits
    = max_element_ckey->get_next_bit_offset () - start_bit_offset;
  const concrete_binding *range_key
    = smgr->get_concrete_binding (start_bit_offset, range_size_in_bits);
  if (range_key->symbolic_p ())
    return false;

  /* Get the value.  */
  if (TREE_CODE (val) == CONSTRUCTOR)
    return false;
  const svalue *sval = get_svalue_for_ctor_val (val, mgr);

  /* Bind the value to the range.  */
  put (range_key, sval);
  return true;
}

/* Bind the value VAL into INDEX within PARENT_REF.
   For use in handling a pair of entries within a CONSTRUCTOR.
   Return true if successful, false if there was a problem (e.g. due
   to hitting a complexity limit).  */

bool
binding_map::apply_ctor_pair_to_child_region (const region *parent_reg,
					      region_model_manager *mgr,
					      tree index, tree val)
{
  const region *child_reg
    = get_subregion_within_ctor_for_ctor_pair (parent_reg, index, val, mgr);
  if (!child_reg)
    return false;
  if (TREE_CODE (val) == CONSTRUCTOR)
    return apply_ctor_to_region (child_reg, val, mgr);
  else
    {
      const svalue *sval = get_svalue_for_ctor_val (val, mgr);
      if (child_reg->empty_p ())
	return false;
      const binding_key *k
	= binding_key::make (mgr->get_store_manager (), child_reg);
      /* Handle the case where we have an unknown size for child_reg
	 (e.g. due to it being a trailing field with incomplete array
	 type.  */
      if (!k->concrete_p ())
	{
	  /* Assume that sval has a well-defined size for this case.  */
	  tree sval_type = sval->get_type ();
	  gcc_assert (sval_type);
	  HOST_WIDE_INT sval_byte_size = int_size_in_bytes (sval_type);
	  gcc_assert (sval_byte_size != -1);
	  bit_size_t sval_bit_size = sval_byte_size * BITS_PER_UNIT;
	  /* Get offset of child relative to base region.  */
	  region_offset child_base_offset = child_reg->get_offset (mgr);
	  if (child_base_offset.symbolic_p ())
	    return false;
	  /* Convert to an offset relative to the parent region.  */
	  region_offset parent_base_offset = parent_reg->get_offset (mgr);
	  gcc_assert (!parent_base_offset.symbolic_p ());
	  bit_offset_t child_parent_offset
	    = (child_base_offset.get_bit_offset ()
	       - parent_base_offset.get_bit_offset ());
	  /* Create a concrete key for the child within the parent.  */
	  k = mgr->get_store_manager ()->get_concrete_binding
	    (child_parent_offset, sval_bit_size);
	}
      gcc_assert (k->concrete_p ());
      put (k, sval);
      return true;
    }
}

/* Populate OUT with all bindings within this map that overlap KEY.  */

void
binding_map::get_overlapping_bindings (const binding_key *key,
				       auto_vec<const binding_key *> *out)
{
  for (auto iter : *this)
    {
      const binding_key *iter_key = iter.first;
      if (const concrete_binding *ckey
	    = key->dyn_cast_concrete_binding ())
	{
	  if (const concrete_binding *iter_ckey
	      = iter_key->dyn_cast_concrete_binding ())
	    {
	      if (ckey->overlaps_p (*iter_ckey))
		out->safe_push (iter_key);
	    }
	  else
	    {
	      /* Assume overlap.  */
	      out->safe_push (iter_key);
	    }
	}
      else
	{
	  /* Assume overlap.  */
	  out->safe_push (iter_key);
	}
    }
}

/* Remove, truncate, and/or split any bindings within this map that
   overlap DROP_KEY.

   For example, if we have:

     +------------------------------------+
     |             old binding            |
     +------------------------------------+

   which is to be overwritten with:

     .......+----------------------+.......
     .......|      new binding     |.......
     .......+----------------------+.......

   this function "cuts a hole" out of the old binding:

     +------+......................+------+
     |prefix| hole for new binding |suffix|
     +------+......................+------+

   into which the new binding can be added without
   overlapping the prefix or suffix.

   The prefix and suffix (if added) will be bound to the pertinent
   parts of the value of the old binding.

   For example, given:
     struct s5
     {
       char arr[8];
     };
     void test_5 (struct s5 *p)
     {
       struct s5 f = *p;
       f.arr[3] = 42;
     }
   then after the "f = *p;" we have:
     cluster for: f: INIT_VAL((*INIT_VAL(p_33(D))))
   and at the "f.arr[3] = 42;" we remove the bindings overlapping
   "f.arr[3]", replacing it with a prefix (bytes 0-2) and suffix (bytes 4-7)
   giving:
     cluster for: f
       key:   {bytes 0-2}
       value:  {BITS_WITHIN(bytes 0-2, inner_val: INIT_VAL((*INIT_VAL(p_33(D))).arr))}
       key:   {bytes 4-7}
       value:  {BITS_WITHIN(bytes 4-7, inner_val: INIT_VAL((*INIT_VAL(p_33(D))).arr))}
   punching a hole into which the new value can be written at byte 3:
     cluster for: f
       key:   {bytes 0-2}
       value:  {BITS_WITHIN(bytes 0-2, inner_val: INIT_VAL((*INIT_VAL(p_33(D))).arr))}
       key:   {byte 3}
       value: 'char' {(char)42}
       key:   {bytes 4-7}
       value:  {BITS_WITHIN(bytes 4-7, inner_val: INIT_VAL((*INIT_VAL(p_33(D))).arr))}

   If UNCERTAINTY is non-NULL, use it to record any svalues that
   were removed, as being maybe-bound.

   If MAYBE_LIVE_VALUES is non-NULL, then use it to record any svalues that
   were removed as being maybe-live.

   If ALWAYS_OVERLAP, then assume that DROP_KEY can overlap anything
   in the map, due to one or both of the underlying clusters being
   symbolic (but not the same symbolic region).  Hence even if DROP_KEY is a
   concrete binding it could actually be referring to the same memory as
   distinct concrete bindings in the map.  Remove all bindings, but
   register any svalues with *UNCERTAINTY.  */

void
binding_map::remove_overlapping_bindings (store_manager *mgr,
					  const binding_key *drop_key,
					  uncertainty_t *uncertainty,
					  svalue_set *maybe_live_values,
					  bool always_overlap)
{
  /* Get the bindings of interest within this map.  */
  auto_vec<const binding_key *> bindings;
  if (always_overlap)
    for (auto iter : *this)
      bindings.safe_push (iter.first); /* Add all bindings.  */
  else
    /* Just add overlapping bindings.  */
    get_overlapping_bindings (drop_key, &bindings);

  unsigned i;
  const binding_key *iter_binding;
  FOR_EACH_VEC_ELT (bindings, i, iter_binding)
    {
      /* Record any svalues that were removed to *UNCERTAINTY as being
	 maybe-bound, provided at least some part of the binding is symbolic.

	 Specifically, if at least one of the bindings is symbolic, or we
	 have ALWAYS_OVERLAP for the case where we have possibly aliasing
	 regions, then we don't know that the svalue has been overwritten,
	 and should record that to *UNCERTAINTY.

	 However, if we have concrete keys accessing within the same symbolic
	 region, then we *know* that the symbolic region has been overwritten,
	 so we don't record it to *UNCERTAINTY, as this could be a genuine
	 leak.  */
      const svalue *old_sval = get (iter_binding);
      if (uncertainty
	  && (drop_key->symbolic_p ()
	      || iter_binding->symbolic_p ()
	      || always_overlap))
	uncertainty->on_maybe_bound_sval (old_sval);

      /* Record any svalues that were removed to *MAYBE_LIVE_VALUES as being
	 maybe-live. */
      if (maybe_live_values)
	maybe_live_values->add (old_sval);

      /* Begin by removing the old binding. */
      m_map.remove (iter_binding);

      /* Don't attempt to handle prefixes/suffixes for the
	 "always_overlap" case; everything's being removed.  */
      if (always_overlap)
	continue;

      /* Now potentially add the prefix and suffix.  */
      if (const concrete_binding *drop_ckey
	  = drop_key->dyn_cast_concrete_binding ())
	if (const concrete_binding *iter_ckey
	      = iter_binding->dyn_cast_concrete_binding ())
	  {
	    gcc_assert (drop_ckey->overlaps_p (*iter_ckey));

	    const bit_range &drop_bits = drop_ckey->get_bit_range ();
	    const bit_range &iter_bits = iter_ckey->get_bit_range ();

	    if (iter_bits.get_start_bit_offset ()
		  < drop_bits.get_start_bit_offset ())
	      {
		/* We have a truncated prefix.  */
		bit_range prefix_bits (iter_bits.get_start_bit_offset (),
				       (drop_bits.get_start_bit_offset ()
					- iter_bits.get_start_bit_offset ()));
		const concrete_binding *prefix_key
		  = mgr->get_concrete_binding (prefix_bits);
		bit_range rel_prefix (0, prefix_bits.m_size_in_bits);
		const svalue *prefix_sval
		  = old_sval->extract_bit_range (NULL_TREE,
						 rel_prefix,
						 mgr->get_svalue_manager ());
		m_map.put (prefix_key, prefix_sval);
	      }

	    if (iter_bits.get_next_bit_offset ()
		  > drop_bits.get_next_bit_offset ())
	      {
		/* We have a truncated suffix.  */
		bit_range suffix_bits (drop_bits.get_next_bit_offset (),
				       (iter_bits.get_next_bit_offset ()
					- drop_bits.get_next_bit_offset ()));
		const concrete_binding *suffix_key
		  = mgr->get_concrete_binding (suffix_bits);
		bit_range rel_suffix (drop_bits.get_next_bit_offset ()
					- iter_bits.get_start_bit_offset (),
				      suffix_bits.m_size_in_bits);
		const svalue *suffix_sval
		  = old_sval->extract_bit_range (NULL_TREE,
						 rel_suffix,
						 mgr->get_svalue_manager ());
		m_map.put (suffix_key, suffix_sval);
	      }
	  }
    }
}

/* class binding_cluster.  */

binding_cluster::binding_cluster (const region *base_region)
: m_base_region (base_region), m_map (),
  m_escaped (false), m_touched (false)
{
}

/* binding_cluster's copy ctor.  */

binding_cluster::binding_cluster (const binding_cluster &other)
: m_base_region (other.m_base_region), m_map (other.m_map),
  m_escaped (other.m_escaped), m_touched (other.m_touched)
{
}

/* binding_cluster's assignment operator.  */

binding_cluster&
binding_cluster::operator= (const binding_cluster &other)
{
  gcc_assert (m_base_region == other.m_base_region);
  m_map = other.m_map;
  m_escaped = other.m_escaped;
  m_touched = other.m_touched;
  return *this;
}

/* binding_cluster's equality operator.  */

bool
binding_cluster::operator== (const binding_cluster &other) const
{
  if (m_map != other.m_map)
    return false;

  if (m_base_region != other.m_base_region)
    return false;

  if (m_escaped != other.m_escaped)
    return false;

  if (m_touched != other.m_touched)
    return false;

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Generate a hash value for this binding_cluster.  */

hashval_t
binding_cluster::hash () const
{
  return m_map.hash ();
}

/* Return true if this binding_cluster is symbolic
   i.e. its base region is symbolic.  */

bool
binding_cluster::symbolic_p () const
{
  return m_base_region->get_kind () == RK_SYMBOLIC;
}

/* Dump a representation of this binding_cluster to PP.
   SIMPLE controls how values and regions are to be printed.
   If MULTILINE, then split the dump over multiple lines and
   use whitespace for readability, otherwise put all on one line.  */

void
binding_cluster::dump_to_pp (pretty_printer *pp, bool simple,
			     bool multiline) const
{
  if (m_escaped)
    {
      if (multiline)
	{
	  pp_string (pp, "    ESCAPED");
	  pp_newline (pp);
	}
      else
	pp_string (pp, "(ESCAPED)");
    }
  if (m_touched)
    {
      if (multiline)
	{
	  pp_string (pp, "    TOUCHED");
	  pp_newline (pp);
	}
      else
	pp_string (pp, "(TOUCHED)");
    }

  m_map.dump_to_pp (pp, simple, multiline);
}

/* Dump a multiline representation of this binding_cluster to stderr.  */

DEBUG_FUNCTION void
binding_cluster::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  pp_string (&pp, "  cluster for: ");
  m_base_region->dump_to_pp (&pp, simple);
  pp_string (&pp, ": ");
  pp_newline (&pp);
  dump_to_pp (&pp, simple, true);
  pp_newline (&pp);
}

/* Assert that this object is valid.  */

void
binding_cluster::validate () const
{
  int num_symbolic = 0;
  int num_concrete = 0;
  for (auto iter : m_map)
    {
      if (iter.first->symbolic_p ())
	num_symbolic++;
      else
	num_concrete++;
    }
  /* We shouldn't have more than one symbolic key per cluster
     (or one would have clobbered the other).  */
  gcc_assert (num_symbolic < 2);
  /* We can't have both concrete and symbolic keys.  */
  gcc_assert (num_concrete == 0 || num_symbolic == 0);
}

/* Return a new json::object of the form
   {"escaped": true/false,
    "touched": true/false,
    "map" : object for the binding_map.  */

std::unique_ptr<json::object>
binding_cluster::to_json () const
{
  auto cluster_obj = ::make_unique<json::object> ();

  cluster_obj->set_bool ("escaped", m_escaped);
  cluster_obj->set_bool ("touched", m_touched);
  cluster_obj->set ("map", m_map.to_json ());

  return cluster_obj;
}

std::unique_ptr<text_art::tree_widget>
binding_cluster::make_dump_widget (const text_art::dump_widget_info &dwi,
				   store_manager *mgr) const
{
  pretty_printer the_pp;
  pretty_printer * const pp = &the_pp;
  pp_format_decoder (pp) = default_tree_printer;
  pp_show_color (pp) = true;
  const bool simple = true;

  m_base_region->dump_to_pp (pp, simple);
  pp_string (pp, ": ");

  if (const svalue *sval = maybe_get_simple_value (mgr))
    {
      /* Special-case to simplify dumps for the common case where
	 we just have one value directly bound to the whole of a
	 region.  */
      sval->dump_to_pp (pp, simple);
      if (escaped_p ())
	pp_string (pp, " (ESCAPED)");
      if (touched_p ())
	pp_string (pp, " (TOUCHED)");

      return text_art::tree_widget::make (dwi, pp);
    }
  else
    {
      if (escaped_p ())
	pp_string (pp, " (ESCAPED)");
      if (touched_p ())
	pp_string (pp, " (TOUCHED)");

      std::unique_ptr<text_art::tree_widget> cluster_widget
	(text_art::tree_widget::make (dwi, pp));

      m_map.add_to_tree_widget (*cluster_widget, dwi);

      return cluster_widget;
    }
}

/* Add a binding of SVAL of kind KIND to REG, unpacking SVAL if it is a
   compound_sval.  */

void
binding_cluster::bind (store_manager *mgr,
		       const region *reg, const svalue *sval)
{
  if (const compound_svalue *compound_sval
	= sval->dyn_cast_compound_svalue ())
    {
      bind_compound_sval (mgr, reg, compound_sval);
      return;
    }

  if (reg->empty_p ())
    return;
  const binding_key *binding = binding_key::make (mgr, reg);
  bind_key (binding, sval);
}

/* Bind SVAL to KEY.
   Unpacking of compound_svalues should already have been done by the
   time this is called.  */

void
binding_cluster::bind_key (const binding_key *key, const svalue *sval)
{
  gcc_assert (sval->get_kind () != SK_COMPOUND);

  m_map.put (key, sval);
  if (key->symbolic_p ())
    m_touched = true;
}

/* Subroutine of binding_cluster::bind.
   Unpack compound_svals when binding them, so that we bind them
   element-wise.  */

void
binding_cluster::bind_compound_sval (store_manager *mgr,
				     const region *reg,
				     const compound_svalue *compound_sval)
{
  region_offset reg_offset
    = reg->get_offset (mgr->get_svalue_manager ());
  if (reg_offset.symbolic_p ())
    {
      m_touched = true;
      clobber_region (mgr, reg);
      return;
    }

  for (map_t::iterator iter = compound_sval->begin ();
       iter != compound_sval->end (); ++iter)
    {
      const binding_key *iter_key = (*iter).first;
      const svalue *iter_sval = (*iter).second;

      if (const concrete_binding *concrete_key
	  = iter_key->dyn_cast_concrete_binding ())
	{
	  bit_offset_t effective_start
	    = (concrete_key->get_start_bit_offset ()
	       + reg_offset.get_bit_offset ());
	  const concrete_binding *effective_concrete_key
	    = mgr->get_concrete_binding (effective_start,
					 concrete_key->get_size_in_bits ());
	  bind_key (effective_concrete_key, iter_sval);
	}
      else
	gcc_unreachable ();
    }
}

/* Remove all bindings overlapping REG within this cluster.  */

void
binding_cluster::clobber_region (store_manager *mgr, const region *reg)
{
  remove_overlapping_bindings (mgr, reg, NULL, NULL);
}

/* Remove any bindings for REG within this cluster.  */

void
binding_cluster::purge_region (store_manager *mgr, const region *reg)
{
  gcc_assert (reg->get_kind () == RK_DECL);
  if (reg->empty_p ())
    return;
  const binding_key *binding
    = binding_key::make (mgr, const_cast<region *> (reg));
  m_map.remove (binding);
}

/* Clobber REG and fill it with repeated copies of SVAL.  */

void
binding_cluster::fill_region (store_manager *mgr,
			      const region *reg,
			      const svalue *sval)
{
  clobber_region (mgr, reg);

  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  const svalue *byte_size_sval = reg->get_byte_size_sval (sval_mgr);
  const svalue *fill_sval
    = sval_mgr->get_or_create_repeated_svalue (reg->get_type (),
					       byte_size_sval, sval);
  bind (mgr, reg, fill_sval);
}

/* Clobber REG within this cluster and fill it with zeroes.  */

void
binding_cluster::zero_fill_region (store_manager *mgr, const region *reg)
{
  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  const svalue *zero_sval = sval_mgr->get_or_create_int_cst (char_type_node, 0);
  fill_region (mgr, reg, zero_sval);
}

/* Mark REG_TO_BIND within this cluster as being unknown.

   Remove any bindings overlapping REG_FOR_OVERLAP.
   If UNCERTAINTY is non-NULL, use it to record any svalues that
   had bindings to them removed, as being maybe-bound.
   If MAYBE_LIVE_VALUES is non-NULL, use it to record any svalues that
   had bindings to them removed, as being maybe-live.

   REG_TO_BIND and REG_FOR_OVERLAP are the same for
   store::mark_region_as_unknown, but are different in
   store::set_value's alias handling, for handling the case where
   we have a write to a symbolic REG_FOR_OVERLAP. */

void
binding_cluster::mark_region_as_unknown (store_manager *mgr,
					 const region *reg_to_bind,
					 const region *reg_for_overlap,
					 uncertainty_t *uncertainty,
					 svalue_set *maybe_live_values)
{
  if (reg_to_bind->empty_p ())
    return;

  remove_overlapping_bindings (mgr, reg_for_overlap, uncertainty,
			       maybe_live_values);

  /* Add a default binding to "unknown".  */
  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  const svalue *sval
    = sval_mgr->get_or_create_unknown_svalue (reg_to_bind->get_type ());
  bind (mgr, reg_to_bind, sval);
}

/* Purge state involving SVAL.  */

void
binding_cluster::purge_state_involving (const svalue *sval,
					region_model_manager *sval_mgr)
{
  auto_vec<const binding_key *> to_remove;
  auto_vec<std::pair<const binding_key *, tree> > to_make_unknown;
  for (auto iter : m_map)
    {
      const binding_key *iter_key = iter.first;
      if (const symbolic_binding *symbolic_key
	    = iter_key->dyn_cast_symbolic_binding ())
	{
	  const region *reg = symbolic_key->get_region ();
	  if (reg->involves_p (sval))
	    to_remove.safe_push (iter_key);
	}
      const svalue *iter_sval = iter.second;
      if (iter_sval->involves_p (sval))
	to_make_unknown.safe_push (std::make_pair(iter_key,
						  iter_sval->get_type ()));
    }
  for (auto iter : to_remove)
    {
      m_map.remove (iter);
      m_touched = true;
    }
  for (auto iter : to_make_unknown)
    {
      const svalue *new_sval
	= sval_mgr->get_or_create_unknown_svalue (iter.second);
      m_map.put (iter.first, new_sval);
    }
}

/* Get any SVAL bound to REG within this cluster via kind KIND,
   without checking parent regions of REG.  */

const svalue *
binding_cluster::get_binding (store_manager *mgr,
			      const region *reg) const
{
  if (reg->empty_p ())
    return NULL;
  const binding_key *reg_binding = binding_key::make (mgr, reg);
  const svalue *sval = m_map.get (reg_binding);
  if (sval)
    {
      /* If we have a struct with a single field, then the binding of
	 the field will equal that of the struct, and looking up e.g.
	 PARENT_REG.field within:
	    cluster for PARENT_REG: INIT_VAL(OTHER_REG)
	 will erroneously return INIT_VAL(OTHER_REG), rather than
	   SUB_VALUE(INIT_VAL(OTHER_REG), FIELD) == INIT_VAL(OTHER_REG.FIELD).
	 Fix this issue by iterating upwards whilst the bindings are equal,
	 expressing the lookups as subvalues.
	 We have to gather a list of subregion accesses, then walk it
	 in reverse to get the subvalues.  */
      auto_vec<const region *> regions;
      while (const region *parent_reg = reg->get_parent_region ())
	{
	  const binding_key *parent_reg_binding
	    = binding_key::make (mgr, parent_reg);
	  if (parent_reg_binding == reg_binding
	      && sval->get_type ()
	      && reg->get_type ()
	      && sval->get_type () != reg->get_type ())
	    {
	      regions.safe_push (reg);
	      reg = parent_reg;
	    }
	  else
	    break;
	}
      if (sval->get_type ()
	  && reg->get_type ()
	  && sval->get_type () == reg->get_type ())
	{
	  unsigned i;
	  const region *iter_reg;
	  FOR_EACH_VEC_ELT_REVERSE (regions, i, iter_reg)
	    {
	      region_model_manager *rmm_mgr = mgr->get_svalue_manager ();
	      sval = rmm_mgr->get_or_create_sub_svalue (iter_reg->get_type (),
							sval, iter_reg);
	    }
	}
    }
  return sval;
}

/* Get any SVAL bound to REG within this cluster,
   either directly for REG, or recursively checking for bindings within
   parent regions and extracting subvalues if need be.  */

const svalue *
binding_cluster::get_binding_recursive (store_manager *mgr,
					const region *reg) const
{
  if (const svalue *sval = get_binding (mgr, reg))
    return sval;
  if (reg != m_base_region)
    if (const region *parent_reg = reg->get_parent_region ())
      if (const svalue *parent_sval
	  = get_binding_recursive (mgr, parent_reg))
	{
	  /* Extract child svalue from parent svalue.  */
	  region_model_manager *rmm_mgr = mgr->get_svalue_manager ();
	  return rmm_mgr->get_or_create_sub_svalue (reg->get_type (),
						    parent_sval, reg);
	}
  return NULL;
}

/* Get any value bound for REG within this cluster.  */

const svalue *
binding_cluster::get_any_binding (store_manager *mgr,
				  const region *reg) const
{
  /* Look for a direct binding.  */
  if (const svalue *direct_sval
      = get_binding_recursive (mgr, reg))
    return direct_sval;

  /* If we had a write to a cluster of unknown size, we might
     have a self-binding of the whole base region with an svalue,
     where the base region is symbolic.
     Handle such cases by returning sub_svalue instances.  */
  if (const svalue *cluster_sval = maybe_get_simple_value (mgr))
    {
      /* Extract child svalue from parent svalue.  */
      region_model_manager *rmm_mgr = mgr->get_svalue_manager ();
      return rmm_mgr->get_or_create_sub_svalue (reg->get_type (),
						cluster_sval, reg);
    }

  /* If this cluster has been touched by a symbolic write, then the content
     of any subregion not currently specifically bound is "UNKNOWN".  */
  if (m_touched)
    {
      region_model_manager *rmm_mgr = mgr->get_svalue_manager ();
      return rmm_mgr->get_or_create_unknown_svalue (reg->get_type ());
    }

  /* Alternatively, if this is a symbolic read and the cluster has any bindings,
     then we don't know if we're reading those values or not, so the result
     is also "UNKNOWN".  */
  if (reg->get_offset (mgr->get_svalue_manager ()).symbolic_p ()
      && m_map.elements () > 0)
    {
      region_model_manager *rmm_mgr = mgr->get_svalue_manager ();
      return rmm_mgr->get_or_create_unknown_svalue (reg->get_type ());
    }

  if (const svalue *compound_sval = maybe_get_compound_binding (mgr, reg))
    return compound_sval;

  /* Otherwise, the initial value, or uninitialized.  */
  return NULL;
}

/* Attempt to get a compound_svalue for the bindings within the cluster
   affecting REG (which could be the base region itself).

   Create a compound_svalue with the subset of bindings the affect REG,
   offsetting them so that the offsets are relative to the start of REG
   within the cluster.

   For example, REG could be one element within an array of structs.

   Return the resulting compound_svalue, or NULL if there's a problem.  */

const svalue *
binding_cluster::maybe_get_compound_binding (store_manager *mgr,
					     const region *reg) const
{
  region_offset cluster_offset
    = m_base_region->get_offset (mgr->get_svalue_manager ());
  if (cluster_offset.symbolic_p ())
    return NULL;
  region_offset reg_offset = reg->get_offset (mgr->get_svalue_manager ());
  if (reg_offset.symbolic_p ())
    return NULL;

  if (reg->empty_p ())
    return NULL;

  region_model_manager *sval_mgr = mgr->get_svalue_manager ();

  /* We will a build the result map in two parts:
     (a) result_map, holding the concrete keys from this cluster,

     (b) default_map, holding the initial values for the region
     (e.g. uninitialized, initializer values, or zero), unless this
     cluster has been touched.

     We will populate (a), and as we do, clobber (b), trimming and
     splitting its bindings as necessary.
     Finally, we will merge (b) into (a), giving a concrete map
     that merges both the initial values and the bound values from
     the binding_cluster.
     Doing it this way reduces N for the O(N^2) intersection-finding,
     perhaps we should have a spatial-organized data structure for
     concrete keys, though.  */

  binding_map result_map;
  binding_map default_map;

  /* Set up default values in default_map.  */
  const svalue *default_sval;
  if (m_touched)
    default_sval = sval_mgr->get_or_create_unknown_svalue (reg->get_type ());
  else
    default_sval = sval_mgr->get_or_create_initial_value (reg);
  const binding_key *default_key = binding_key::make (mgr, reg);

  /* Express the bit-range of the default key for REG relative to REG,
     rather than to the base region.  */
  const concrete_binding *concrete_default_key
    = default_key->dyn_cast_concrete_binding ();
  if (!concrete_default_key)
    return nullptr;
  const concrete_binding *default_key_relative_to_reg
     = mgr->get_concrete_binding (0, concrete_default_key->get_size_in_bits ());
  default_map.put (default_key_relative_to_reg, default_sval);

  for (map_t::iterator iter = m_map.begin (); iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      const svalue *sval = (*iter).second;

      if (const concrete_binding *concrete_key
	  = key->dyn_cast_concrete_binding ())
	{
	  const bit_range &bound_range = concrete_key->get_bit_range ();

	  bit_size_t reg_bit_size;
	  if (!reg->get_bit_size (&reg_bit_size))
	    return NULL;

	  bit_range reg_range (reg_offset.get_bit_offset (),
			       reg_bit_size);

	  /* Skip bindings that are outside the bit range of REG.  */
	  if (!bound_range.intersects_p (reg_range))
	    continue;

	  /* We shouldn't have an exact match; that should have been
	     handled already.  */
	  gcc_assert (!(reg_range == bound_range));

	  bit_range subrange (0, 0);
	  if (reg_range.contains_p (bound_range, &subrange))
	    {
	      /* We have a bound range fully within REG.
		 Add it to map, offsetting accordingly.  */

	      /* Get offset of KEY relative to REG, rather than to
		 the cluster.  */
	      const concrete_binding *offset_concrete_key
		= mgr->get_concrete_binding (subrange);
	      result_map.put (offset_concrete_key, sval);

	      /* Clobber default_map, removing/trimming/spliting where
		 it overlaps with offset_concrete_key.  */
	      default_map.remove_overlapping_bindings (mgr,
						       offset_concrete_key,
						       NULL, NULL, false);
	    }
	  else if (bound_range.contains_p (reg_range, &subrange))
	    {
	      /* REG is fully within the bound range, but
		 is not equal to it; we're extracting a subvalue.  */
	      return sval->extract_bit_range (reg->get_type (),
					      subrange,
					      mgr->get_svalue_manager ());
	    }
	  else
	    {
	      /* REG and the bound range partially overlap.  */
	      bit_range reg_subrange (0, 0);
	      bit_range bound_subrange (0, 0);
	      reg_range.intersects_p (bound_range,
				      &reg_subrange, &bound_subrange);

	      /* Get the bits from the bound value for the bits at the
		 intersection (relative to the bound value).  */
	      const svalue *overlap_sval
		= sval->extract_bit_range (NULL_TREE,
					   bound_subrange,
					   mgr->get_svalue_manager ());

	      /* Get key for overlap, relative to the REG.  */
	      const concrete_binding *overlap_concrete_key
		= mgr->get_concrete_binding (reg_subrange);
	      result_map.put (overlap_concrete_key, overlap_sval);

	      /* Clobber default_map, removing/trimming/spliting where
		 it overlaps with overlap_concrete_key.  */
	      default_map.remove_overlapping_bindings (mgr,
						       overlap_concrete_key,
						       NULL, NULL, false);
	    }
	}
      else
	/* Can't handle symbolic bindings.  */
	return NULL;
    }

  if (result_map.elements () == 0)
    return NULL;

  /* Merge any bindings from default_map into result_map.  */
  for (auto iter : default_map)
    {
      const binding_key *key = iter.first;
      const svalue *sval = iter.second;
      result_map.put (key, sval);
    }

  return sval_mgr->get_or_create_compound_svalue (reg->get_type (), result_map);
}

/* Remove, truncate, and/or split any bindings within this map that
   could overlap REG.

   If REG's base region or this cluster is symbolic and they're different
   base regions, then remove everything in this cluster's map, on the
   grounds that REG could be referring to the same memory as anything
   in the map.

   If UNCERTAINTY is non-NULL, use it to record any svalues that
   were removed, as being maybe-bound.

   If MAYBE_LIVE_VALUES is non-NULL, use it to record any svalues that
   were removed, as being maybe-live.  */

void
binding_cluster::remove_overlapping_bindings (store_manager *mgr,
					      const region *reg,
					      uncertainty_t *uncertainty,
					      svalue_set *maybe_live_values)
{
  if (reg->empty_p ())
    return;
  const binding_key *reg_binding = binding_key::make (mgr, reg);

  const region *cluster_base_reg = get_base_region ();
  const region *other_base_reg = reg->get_base_region ();
  /* If at least one of the base regions involved is symbolic, and they're
     not the same base region, then consider everything in the map as
     potentially overlapping with reg_binding (even if it's a concrete
     binding and things in the map are concrete - they could be referring
     to the same memory when the symbolic base regions are taken into
     account).  */
  bool always_overlap = (cluster_base_reg != other_base_reg
			 && (cluster_base_reg->get_kind () == RK_SYMBOLIC
			     || other_base_reg->get_kind () == RK_SYMBOLIC));
  m_map.remove_overlapping_bindings (mgr, reg_binding, uncertainty,
				     maybe_live_values,
				     always_overlap);
}

/* Attempt to merge CLUSTER_A and CLUSTER_B into OUT_CLUSTER, using
   MGR and MERGER.
   Return true if they can be merged, false otherwise.  */

bool
binding_cluster::can_merge_p (const binding_cluster *cluster_a,
			      const binding_cluster *cluster_b,
			      binding_cluster *out_cluster,
			      store *out_store,
			      store_manager *mgr,
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

  /* At least one of CLUSTER_A and CLUSTER_B are non-NULL, but either
     could be NULL.  Handle these cases.  */
  if (cluster_a == NULL)
    {
      gcc_assert (cluster_b != NULL);
      gcc_assert (cluster_b->m_base_region == out_cluster->m_base_region);
      out_cluster->make_unknown_relative_to (cluster_b, out_store, mgr);
      return true;
    }
  if (cluster_b == NULL)
    {
      gcc_assert (cluster_a != NULL);
      gcc_assert (cluster_a->m_base_region == out_cluster->m_base_region);
      out_cluster->make_unknown_relative_to (cluster_a, out_store, mgr);
      return true;
    }

  /* The "both inputs are non-NULL" case.  */
  gcc_assert (cluster_a != NULL && cluster_b != NULL);
  gcc_assert (cluster_a->m_base_region == out_cluster->m_base_region);
  gcc_assert (cluster_b->m_base_region == out_cluster->m_base_region);

  hash_set<const binding_key *> keys;
  for (map_t::iterator iter_a = cluster_a->m_map.begin ();
       iter_a != cluster_a->m_map.end (); ++iter_a)
    {
      const binding_key *key_a = (*iter_a).first;
      keys.add (key_a);
    }
  for (map_t::iterator iter_b = cluster_b->m_map.begin ();
       iter_b != cluster_b->m_map.end (); ++iter_b)
    {
      const binding_key *key_b = (*iter_b).first;
      keys.add (key_b);
    }
  int num_symbolic_keys = 0;
  int num_concrete_keys = 0;
  for (hash_set<const binding_key *>::iterator iter = keys.begin ();
       iter != keys.end (); ++iter)
    {
      region_model_manager *sval_mgr = mgr->get_svalue_manager ();
      const binding_key *key = *iter;
      const svalue *sval_a = cluster_a->get_any_value (key);
      const svalue *sval_b = cluster_b->get_any_value (key);

      if (key->symbolic_p ())
	num_symbolic_keys++;
      else
	num_concrete_keys++;

      if (sval_a == sval_b)
	{
	  gcc_assert (sval_a);
	  out_cluster->m_map.put (key, sval_a);
	  continue;
	}
      else if (sval_a && sval_b)
	{
	  if (const svalue *merged_sval
	      = sval_a->can_merge_p (sval_b, sval_mgr, merger))
	    {
	      out_cluster->m_map.put (key, merged_sval);
	      continue;
	    }
	  /* Merger of the svalues failed.  Reject merger of the cluster.   */
	  return false;
	}

      /* If we get here, then one cluster binds this key and the other
	 doesn't; merge them as "UNKNOWN".  */
      gcc_assert (sval_a || sval_b);

      const svalue *bound_sval = sval_a ? sval_a : sval_b;
      tree type = bound_sval->get_type ();
      const svalue *unknown_sval
	= mgr->get_svalue_manager ()->get_or_create_unknown_svalue (type);

      /* ...but reject the merger if this sval shouldn't be mergeable
	 (e.g. reject merging svalues that have non-purgable sm-state,
	 to avoid falsely reporting memory leaks by merging them
	 with something else).  */
      if (!bound_sval->can_merge_p (unknown_sval, sval_mgr, merger))
	return false;

      out_cluster->m_map.put (key, unknown_sval);
    }

  /* We can only have at most one symbolic key per cluster,
     and if we do, we can't have any concrete keys.
     If this happens, mark the cluster as touched, with no keys.  */
  if (num_symbolic_keys >= 2
      || (num_concrete_keys > 0 && num_symbolic_keys > 0))
    {
      out_cluster->m_touched = true;
      out_cluster->m_map.empty ();
    }

  /* We don't handle other kinds of overlaps yet.  */

  return true;
}

/* Update this cluster to reflect an attempt to merge OTHER where there
   is no other cluster to merge with, and so we're notionally merging the
   bound values in OTHER with the initial value of the relevant regions.

   Any bound keys in OTHER should be bound to unknown in this.  */

void
binding_cluster::make_unknown_relative_to (const binding_cluster *other,
					   store *out_store,
					   store_manager *mgr)
{
  for (map_t::iterator iter = other->m_map.begin ();
       iter != other->m_map.end (); ++iter)
    {
      const binding_key *iter_key = (*iter).first;
      const svalue *iter_sval = (*iter).second;
      const svalue *unknown_sval
	= mgr->get_svalue_manager ()->get_or_create_unknown_svalue
	  (iter_sval->get_type ());
      m_map.put (iter_key, unknown_sval);

      /* For any pointers in OTHER, the merger means that the
	 concrete pointer becomes an unknown value, which could
	 show up as a false report of a leak when considering what
	 pointers are live before vs after.
	 Avoid this by marking the base regions they point to as having
	 escaped.  */
      if (const region_svalue *region_sval
	  = iter_sval->dyn_cast_region_svalue ())
	{
	  const region *base_reg
	    = region_sval->get_pointee ()->get_base_region ();
	  if (base_reg->tracked_p ()
	      && !base_reg->symbolic_for_unknown_ptr_p ())
	    {
	      binding_cluster *c = out_store->get_or_create_cluster (base_reg);
	      c->mark_as_escaped ();
	    }
	}
    }
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
binding_cluster::on_unknown_fncall (const gcall *call,
				    store_manager *mgr,
				    const conjured_purge &p)
{
  if (m_escaped)
    {
      m_map.empty ();

      if (!m_base_region->empty_p ())
	{
	  /* Bind it to a new "conjured" value using CALL.  */
	  const svalue *sval
	    = mgr->get_svalue_manager ()->get_or_create_conjured_svalue
	    (m_base_region->get_type (), call, m_base_region, p);
	  bind (mgr, m_base_region, sval);
	}

      m_touched = true;
    }
}

/* Mark this cluster as having been clobbered by STMT.
   Use P to purge state involving conjured_svalues.  */

void
binding_cluster::on_asm (const gasm *stmt,
			 store_manager *mgr,
			 const conjured_purge &p)
{
  m_map.empty ();

  /* Bind it to a new "conjured" value using CALL.  */
  const svalue *sval
    = mgr->get_svalue_manager ()->get_or_create_conjured_svalue
    (m_base_region->get_type (), stmt, m_base_region, p);
  bind (mgr, m_base_region, sval);

  m_touched = true;
}

/* Return true if this cluster has escaped.  */

bool
binding_cluster::escaped_p () const
{
  /* Consider the "errno" region to always have escaped.  */
  if (m_base_region->get_kind () == RK_ERRNO)
    return true;
  return m_escaped;
}

/* Return true if this binding_cluster has no information
   i.e. if there are no bindings, and it hasn't been marked as having
   escaped, or touched symbolically.  */

bool
binding_cluster::redundant_p () const
{
  return (m_map.elements () == 0
	  && !m_escaped
	  && !m_touched);
}

/* Add PV to OUT_PVS, casting it to TYPE if it is not already of that type.  */

static void
append_pathvar_with_type (path_var pv,
			  tree type,
			  auto_vec<path_var> *out_pvs)
{
  gcc_assert (pv.m_tree);

  if (TREE_TYPE (pv.m_tree) != type)
    pv.m_tree = build1 (NOP_EXPR, type, pv.m_tree);

  out_pvs->safe_push (pv);
}

/* Find representative path_vars for SVAL within this binding of BASE_REG,
   appending the results to OUT_PVS.  */

void
binding_cluster::get_representative_path_vars (const region_model *model,
					       svalue_set *visited,
					       const region *base_reg,
					       const svalue *sval,
					       logger *logger,
					       auto_vec<path_var> *out_pvs)
  const
{
  sval = simplify_for_binding (sval);

  for (map_t::iterator iter = m_map.begin (); iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      const svalue *bound_sval = (*iter).second;
      if (bound_sval == sval)
	{
	  if (const concrete_binding *ckey
		= key->dyn_cast_concrete_binding ())
	    {
	      auto_vec <const region *> subregions;
	      base_reg->get_subregions_for_binding
		(model->get_manager (),
		 ckey->get_start_bit_offset (),
		 ckey->get_size_in_bits (),
		 sval->get_type (),
		 &subregions);
	      unsigned i;
	      const region *subregion;
	      FOR_EACH_VEC_ELT (subregions, i, subregion)
		{
		  if (path_var pv
		      = model->get_representative_path_var (subregion,
							    visited,
							    logger))
		    append_pathvar_with_type (pv, sval->get_type (), out_pvs);
		}
	    }
	  else
	    {
	      const symbolic_binding *skey = (const symbolic_binding *)key;
	      if (path_var pv
		  = model->get_representative_path_var (skey->get_region (),
							visited,
							logger))
		append_pathvar_with_type (pv, sval->get_type (), out_pvs);
	    }
	}
    }
}

/* Get any svalue bound to KEY, or NULL.  */

const svalue *
binding_cluster::get_any_value (const binding_key *key) const
{
  return m_map.get (key);
}

/* If this cluster has a single direct binding for the whole of the region,
   return it.
   For use in simplifying dumps.  */

const svalue *
binding_cluster::maybe_get_simple_value (store_manager *mgr) const
{
  /* Fail gracefully if MGR is NULL to make it easier to dump store
     instances in the debugger.  */
  if (mgr == NULL)
    return NULL;

  if (m_map.elements () != 1)
    return NULL;

  if (m_base_region->empty_p ())
    return NULL;

  const binding_key *key = binding_key::make (mgr, m_base_region);
  return get_any_value (key);
}

/* class store_manager.  */

logger *
store_manager::get_logger () const
{
  return m_mgr->get_logger ();
}

/* binding consolidation.  */

const concrete_binding *
store_manager::get_concrete_binding (bit_offset_t start_bit_offset,
				     bit_offset_t size_in_bits)
{
  concrete_binding b (start_bit_offset, size_in_bits);
  if (concrete_binding *existing = m_concrete_binding_key_mgr.get (b))
    return existing;

  concrete_binding *to_save = new concrete_binding (b);
  m_concrete_binding_key_mgr.put (b, to_save);
  return to_save;
}

const symbolic_binding *
store_manager::get_symbolic_binding (const region *reg)
{
  symbolic_binding b (reg);
  if (symbolic_binding *existing = m_symbolic_binding_key_mgr.get (b))
    return existing;

  symbolic_binding *to_save = new symbolic_binding (b);
  m_symbolic_binding_key_mgr.put (b, to_save);
  return to_save;
}

/* class store.  */

/* store's default ctor.  */

store::store ()
: m_called_unknown_fn (false)
{
}

/* store's copy ctor.  */

store::store (const store &other)
: m_cluster_map (other.m_cluster_map.elements ()),
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
      if (other_slot == NULL)
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

/* Dump a representation of this store to PP, using SIMPLE to control how
   svalues and regions are printed.
   MGR is used for simplifying dumps if non-NULL, but can also be NULL
   (to make it easier to use from the debugger).  */

void
store::dump_to_pp (pretty_printer *pp, bool simple, bool multiline,
		   store_manager *mgr) const
{
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
      pp_string (pp, "clusters within ");
      parent_reg->dump_to_pp (pp, simple);
      if (multiline)
	pp_newline (pp);
      else
	pp_string (pp, " {");

      const region *base_reg;
      unsigned j;
      FOR_EACH_VEC_ELT (base_regions, j, base_reg)
	{
	  /* This is O(N * M), but N ought to be small.  */
	  if (base_reg->get_parent_region () != parent_reg)
	    continue;
	  binding_cluster *cluster
	    = *const_cast<cluster_map_t &> (m_cluster_map).get (base_reg);
	  if (!multiline)
	    {
	      if (j > 0)
		pp_string (pp, ", ");
	    }
	  if (const svalue *sval = cluster->maybe_get_simple_value (mgr))
	    {
	      /* Special-case to simplify dumps for the common case where
		 we just have one value directly bound to the whole of a
		 region.  */
	      if (multiline)
		{
		  pp_string (pp, "  cluster for: ");
		  base_reg->dump_to_pp (pp, simple);
		  pp_string (pp, ": ");
		  sval->dump_to_pp (pp, simple);
		  if (cluster->escaped_p ())
		    pp_string (pp, " (ESCAPED)");
		  if (cluster->touched_p ())
		    pp_string (pp, " (TOUCHED)");
		  pp_newline (pp);
		}
	      else
		{
		  pp_string (pp, "region: {");
		  base_reg->dump_to_pp (pp, simple);
		  pp_string (pp, ", value: ");
		  sval->dump_to_pp (pp, simple);
		  if (cluster->escaped_p ())
		    pp_string (pp, " (ESCAPED)");
		  if (cluster->touched_p ())
		    pp_string (pp, " (TOUCHED)");
		  pp_string (pp, "}");
		}
	    }
	  else if (multiline)
	    {
	      pp_string (pp, "  cluster for: ");
	      base_reg->dump_to_pp (pp, simple);
	      pp_newline (pp);
	      cluster->dump_to_pp (pp, simple, multiline);
	    }
	  else
	    {
	      pp_string (pp, "base region: {");
	      base_reg->dump_to_pp (pp, simple);
	      pp_string (pp, "} has cluster: {");
	      cluster->dump_to_pp (pp, simple, multiline);
	      pp_string (pp, "}");
	    }
	}
      if (!multiline)
	pp_string (pp, "}");
    }
  pp_printf (pp, "m_called_unknown_fn: %s",
	     m_called_unknown_fn ? "TRUE" : "FALSE");
  if (multiline)
    pp_newline (pp);
}

/* Dump a multiline representation of this store to stderr.  */

DEBUG_FUNCTION void
store::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple, true, NULL);
  pp_newline (&pp);
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
  auto store_obj = ::make_unique<json::object> ();

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

      auto clusters_in_parent_reg_obj = ::make_unique<json::object> ();

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

std::unique_ptr<text_art::tree_widget>
store::make_dump_widget (const text_art::dump_widget_info &dwi,
			 store_manager *mgr) const
{
  std::unique_ptr<text_art::tree_widget> store_widget
    (text_art::tree_widget::make (dwi, "Store"));

  store_widget->add_child
    (text_art::tree_widget::from_fmt (dwi, nullptr,
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
	(text_art::tree_widget::make (dwi, pp));

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
	    (cluster->make_dump_widget (dwi, mgr));
	}
      store_widget->add_child (std::move (parent_reg_widget));
    }

  return store_widget;
}

/* Get any svalue bound to REG, or NULL.  */

const svalue *
store::get_any_binding (store_manager *mgr, const region *reg) const
{
  const region *base_reg = reg->get_base_region ();
  binding_cluster **cluster_slot
    = const_cast <cluster_map_t &> (m_cluster_map).get (base_reg);
  if (!cluster_slot)
    return NULL;
  return (*cluster_slot)->get_any_binding (mgr, reg);
}

/* Set the value of LHS_REG to RHS_SVAL.  */

void
store::set_value (store_manager *mgr, const region *lhs_reg,
		  const svalue *rhs_sval,
		  uncertainty_t *uncertainty)
{
  logger *logger = mgr->get_logger ();
  LOG_SCOPE (logger);

  remove_overlapping_bindings (mgr, lhs_reg, uncertainty);

  if (lhs_reg->get_type ())
    rhs_sval = simplify_for_binding (rhs_sval);
  /* ...but if we have no type for the region, retain any cast.  */

  const region *lhs_base_reg = lhs_reg->get_base_region ();
  binding_cluster *lhs_cluster;
  if (lhs_base_reg->symbolic_for_unknown_ptr_p ())
    {
      /* Reject attempting to bind values into a symbolic region
	 for an unknown ptr; merely invalidate values below.  */
      lhs_cluster = NULL;

      /* The LHS of the write is *UNKNOWN.  If the RHS is a pointer,
	 then treat the region being pointed to as having escaped.  */
      if (const region_svalue *ptr_sval = rhs_sval->dyn_cast_region_svalue ())
	{
	  const region *ptr_dst = ptr_sval->get_pointee ();
	  const region *ptr_base_reg = ptr_dst->get_base_region ();
	  mark_as_escaped (ptr_base_reg);
	}
      if (uncertainty)
	uncertainty->on_maybe_bound_sval (rhs_sval);
    }
  else if (lhs_base_reg->tracked_p ())
    {
      lhs_cluster = get_or_create_cluster (lhs_base_reg);
      lhs_cluster->bind (mgr, lhs_reg, rhs_sval);
    }
  else
    {
      /* Reject attempting to bind values into an untracked region;
	 merely invalidate values below.  */
      lhs_cluster = NULL;
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
  svalue_set maybe_live_values;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      const region *iter_base_reg = (*iter).first;
      binding_cluster *iter_cluster = (*iter).second;
      if (iter_base_reg != lhs_base_reg
	  && (lhs_cluster == NULL
	      || lhs_cluster->symbolic_p ()
	      || iter_cluster->symbolic_p ()))
	{
	  tristate t_alias = eval_alias (lhs_base_reg, iter_base_reg);
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
		  rhs_sval->dump_to_pp (pp, true);
		  logger->log_partial (" to LHS_REG: ");
		  lhs_reg->dump_to_pp (pp, true);
		  logger->end_log_line ();
		}
	      /* Mark all of iter_cluster's iter_base_reg as unknown,
		 using LHS_REG when considering overlaps, to handle
		 symbolic vs concrete issues.  */
	      iter_cluster->mark_region_as_unknown
		(mgr,
		 iter_base_reg, /* reg_to_bind */
		 lhs_reg, /* reg_for_overlap */
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
  on_maybe_live_values (maybe_live_values);
}

/* Determine if BASE_REG_A could be an alias of BASE_REG_B.  */

tristate
store::eval_alias (const region *base_reg_a,
		   const region *base_reg_b) const
{
  /* SSA names can't alias.  */
  tree decl_a = base_reg_a->maybe_get_decl ();
  if (decl_a && TREE_CODE (decl_a) == SSA_NAME)
    return tristate::TS_FALSE;
  tree decl_b = base_reg_b->maybe_get_decl ();
  if (decl_b && TREE_CODE (decl_b) == SSA_NAME)
    return tristate::TS_FALSE;

  /* Try both ways, for symmetry.  */
  tristate ts_ab = eval_alias_1 (base_reg_a, base_reg_b);
  if (ts_ab.is_false ())
    return tristate::TS_FALSE;
  tristate ts_ba = eval_alias_1 (base_reg_b, base_reg_a);
  if (ts_ba.is_false ())
    return tristate::TS_FALSE;
  return tristate::TS_UNKNOWN;
}

/* Half of store::eval_alias; called twice for symmetry.  */

tristate
store::eval_alias_1 (const region *base_reg_a,
		     const region *base_reg_b) const
{
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
	  if (sval_a->get_kind () == SK_INITIAL)
	    if (!is_global_var (decl_b))
	      {
		/* The initial value of a pointer can't point to a local.  */
		return tristate::TS_FALSE;
	      }
	}
      if (sval_a->get_kind () == SK_INITIAL
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
	  if (const region_svalue *region_sval
		= base->dyn_cast_region_svalue ())
	    {
	      const region *pointee = region_sval->get_pointee ();
	      /* If we have sval_a is WIDENING(&REGION, OP), and
		 B can't alias REGION, then B can't alias A either.
		 For example, A might arise from
		   for (ptr = &REGION; ...; ptr++)
		 where sval_a is ptr in the 2nd iteration of the loop.
		 We want to ensure that "*ptr" can only clobber things
		 within REGION's base region.  */
	      tristate ts = eval_alias (pointee->get_base_region (),
					base_reg_b);
	      if (ts.is_false ())
		return tristate::TS_FALSE;
	    }
	}
    }
  return tristate::TS_UNKNOWN;
}

/* Record all of the values in MAYBE_LIVE_VALUES as being possibly live.  */

void
store::on_maybe_live_values (const svalue_set &maybe_live_values)
{
  for (auto sval : maybe_live_values)
    {
      if (const region_svalue *ptr_sval = sval->dyn_cast_region_svalue ())
	{
	  const region *base_reg = ptr_sval->get_pointee ()->get_base_region ();
	  mark_as_escaped (base_reg);
	}
    }
}

/* Remove all bindings overlapping REG within this store.  */

void
store::clobber_region (store_manager *mgr, const region *reg)
{
  const region *base_reg = reg->get_base_region ();
  binding_cluster **slot = m_cluster_map.get (base_reg);
  if (!slot)
    return;
  binding_cluster *cluster = *slot;
  cluster->clobber_region (mgr, reg);
  if (cluster->redundant_p ())
    {
      delete cluster;
      m_cluster_map.remove (base_reg);
    }
}

/* Remove any bindings for REG within this store.  */

void
store::purge_region (store_manager *mgr, const region *reg)
{
  const region *base_reg = reg->get_base_region ();
  binding_cluster **slot = m_cluster_map.get (base_reg);
  if (!slot)
    return;
  binding_cluster *cluster = *slot;
  cluster->purge_region (mgr, reg);
  if (cluster->redundant_p ())
    {
      delete cluster;
      m_cluster_map.remove (base_reg);
    }
}

/* Fill REG with SVAL.  */

void
store::fill_region (store_manager *mgr, const region *reg, const svalue *sval)
{
  /* Filling an empty region is a no-op.  */
  if (reg->empty_p ())
    return;

  const region *base_reg = reg->get_base_region ();
  if (base_reg->symbolic_for_unknown_ptr_p ()
      || !base_reg->tracked_p ())
    return;
  binding_cluster *cluster = get_or_create_cluster (base_reg);
  cluster->fill_region (mgr, reg, sval);
}

/* Zero-fill REG.  */

void
store::zero_fill_region (store_manager *mgr, const region *reg)
{
  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  const svalue *zero_sval = sval_mgr->get_or_create_int_cst (char_type_node, 0);
  fill_region (mgr, reg, zero_sval);
}

/* Mark REG as having unknown content.  */

void
store::mark_region_as_unknown (store_manager *mgr, const region *reg,
			       uncertainty_t *uncertainty,
			       svalue_set *maybe_live_values)
{
  const region *base_reg = reg->get_base_region ();
  if (base_reg->symbolic_for_unknown_ptr_p ()
      || !base_reg->tracked_p ())
    return;
  binding_cluster *cluster = get_or_create_cluster (base_reg);
  cluster->mark_region_as_unknown (mgr, reg, reg, uncertainty,
				   maybe_live_values);
}

/* Purge state involving SVAL.  */

void
store::purge_state_involving (const svalue *sval,
			      region_model_manager *sval_mgr)
{
  auto_vec <const region *> base_regs_to_purge;
  for (auto iter : m_cluster_map)
    {
      const region *base_reg = iter.first;
      if (base_reg->involves_p (sval))
	base_regs_to_purge.safe_push (base_reg);
      else
	{
	  binding_cluster *cluster = iter.second;
	  cluster->purge_state_involving (sval, sval_mgr);
	}
    }

  for (auto iter : base_regs_to_purge)
    purge_cluster (iter);
}

/* Get the cluster for BASE_REG, or NULL (const version).  */

const binding_cluster *
store::get_cluster (const region *base_reg) const
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);
  if (binding_cluster **slot
	= const_cast <cluster_map_t &> (m_cluster_map).get (base_reg))
    return *slot;
  else
    return NULL;
}

/* Get the cluster for BASE_REG, or NULL (non-const version).  */

binding_cluster *
store::get_cluster (const region *base_reg)
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);
  if (binding_cluster **slot = m_cluster_map.get (base_reg))
    return *slot;
  else
    return NULL;
}

/* Get the cluster for BASE_REG, creating it if doesn't already exist.  */

binding_cluster *
store::get_or_create_cluster (const region *base_reg)
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);

  /* We shouldn't create clusters for dereferencing an UNKNOWN ptr.  */
  gcc_assert (!base_reg->symbolic_for_unknown_ptr_p ());

  /* We shouldn't create clusters for base regions that aren't trackable.  */
  gcc_assert (base_reg->tracked_p ());

  if (binding_cluster **slot = m_cluster_map.get (base_reg))
    return *slot;

  binding_cluster *cluster = new binding_cluster (base_reg);
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
		    store *out_store, store_manager *mgr,
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
	= out_store->get_or_create_cluster (base_reg);
      if (!binding_cluster::can_merge_p (cluster_a, cluster_b,
					 out_cluster, out_store, mgr, merger))
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
store::mark_as_escaped (const region *base_reg)
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);

  if (base_reg->symbolic_for_unknown_ptr_p ()
      || !base_reg->tracked_p ())
    return;

  binding_cluster *cluster = get_or_create_cluster (base_reg);
  cluster->mark_as_escaped ();
}

/* Handle an unknown fncall by updating any clusters that have escaped
   (either in this fncall, or in a prior one).  */

void
store::on_unknown_fncall (const gcall *call, store_manager *mgr,
			  const conjured_purge &p)
{
  m_called_unknown_fn = true;

  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    (*iter).second->on_unknown_fncall (call, mgr, p);
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
				     svalue_set *visited,
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

/* Remove all bindings overlapping REG within this store, removing
   any clusters that become redundant.

   If UNCERTAINTY is non-NULL, use it to record any svalues that
   were removed, as being maybe-bound.  */

void
store::remove_overlapping_bindings (store_manager *mgr, const region *reg,
				    uncertainty_t *uncertainty)
{
  const region *base_reg = reg->get_base_region ();
  if (binding_cluster **cluster_slot = m_cluster_map.get (base_reg))
    {
      binding_cluster *cluster = *cluster_slot;
      if (reg == base_reg && !escaped_p (base_reg))
	{
	  /* Remove whole cluster.  */
	  m_cluster_map.remove (base_reg);
	  delete cluster;
	  return;
	}
      /* Pass NULL for the maybe_live_values here, as we don't want to
	 record the old svalues as being maybe-bound.  */
      cluster->remove_overlapping_bindings (mgr, reg, uncertainty, NULL);
    }
}

/* Subclass of visitor that accumulates a hash_set of the regions that
   were visited.  */

struct region_finder : public visitor
{
  void visit_region (const region *reg) final override
  {
    m_regs.add (reg);
  }

  hash_set<const region *> m_regs;
};

/* Canonicalize this store, to maximize the chance of equality between
   instances.  */

void
store::canonicalize (store_manager *mgr)
{
  /* If we have e.g.:
         cluster for: HEAP_ALLOCATED_REGION(543)
           ESCAPED
           TOUCHED
     where the heap region is empty and unreferenced, then purge that
     cluster, to avoid unbounded state chains involving these.  */

  /* Find regions that are referenced by bound values in the store.  */
  region_finder s;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      binding_cluster *cluster = (*iter).second;
      for (binding_cluster::iterator_t bind_iter = cluster->m_map.begin ();
	   bind_iter != cluster->m_map.end (); ++bind_iter)
	(*bind_iter).second->accept (&s);
    }

  /* Locate heap-allocated regions that have empty bindings that weren't
     found above.  */
  hash_set<const region *> purgeable_regions;
  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      binding_cluster *cluster = (*iter).second;
      if (base_reg->get_kind () == RK_HEAP_ALLOCATED)
	{
	  /* Don't purge a heap-allocated region that's been marked as
	     escaping, since this could be recording that a ptr to it
	     was written to an unknown symbolic region along this
	     path, and so we don't know whether it's referenced or
	     not, and hence should report it as leaking
	     (PR analyzer/106473).  */
	  if (cluster->escaped_p ())
	    continue;

	  if (cluster->empty_p ())
	    if (!s.m_regs.contains (base_reg))
	      purgeable_regions.add (base_reg);

	  /* Also cover the UNKNOWN case.  */
	  if (const svalue *sval = cluster->maybe_get_simple_value (mgr))
	    if (sval->get_kind () == SK_UNKNOWN)
	      if (!s.m_regs.contains (base_reg))
		purgeable_regions.add (base_reg);
	}
    }

  /* Purge them.  */
  for (hash_set<const region *>::iterator iter = purgeable_regions.begin ();
       iter != purgeable_regions.end (); ++iter)
    {
      const region *base_reg = *iter;
      purge_cluster (base_reg);
    }
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
			  region_model_manager *mgr)
{
  gcc_assert (other_store);
  for (cluster_map_t::iterator iter = other_store->m_cluster_map.begin ();
       iter != other_store->m_cluster_map.end (); ++iter)
    {
      const region *base_reg = (*iter).first;
      binding_cluster *cluster = (*iter).second;
      for (binding_cluster::iterator_t bind_iter = cluster->m_map.begin ();
	   bind_iter != cluster->m_map.end (); ++bind_iter)
	{
	  const binding_key *key = (*bind_iter).first;
	  const svalue *sval = (*bind_iter).second;
	  if (sval->get_kind () == SK_WIDENING)
	    {
	      binding_cluster *this_cluster
		= get_or_create_cluster (base_reg);
	      const svalue *unknown
		= mgr->get_or_create_unknown_svalue (sval->get_type ());
	      this_cluster->bind_key (key, unknown);
	    }
	}
    }
}

/* Use R to replay the bindings from SUMMARY into this object.  */

void
store::replay_call_summary (call_summary_replay &r,
			    const store &summary)
{
  if (summary.m_called_unknown_fn)
    {
      /* A call to an external function occurred in the summary.
	 Hence we need to invalidate our knownledge of globals,
	 escaped regions, etc.  */
      on_unknown_fncall (r.get_call_stmt (),
			 r.get_store_manager (),
			 conjured_purge (r.get_caller_model (),
					 r.get_ctxt ()));
    }

  auto_vec<const region *> keys (summary.m_cluster_map.elements ());
  for (auto kv : summary.m_cluster_map)
    keys.quick_push (kv.first);
  keys.qsort (region::cmp_ptr_ptr);
  for (auto base_reg : keys)
    replay_call_summary_cluster (r, summary, base_reg);
}

/* Use R and SUMMARY to replay the bindings in SUMMARY_CLUSTER
   into this object.  */

void
store::replay_call_summary_cluster (call_summary_replay &r,
				    const store &summary,
				    const region *summary_base_reg)
{
  const call_details &cd = r.get_call_details ();
  region_model_manager *reg_mgr = r.get_manager ();
  store_manager *mgr = reg_mgr->get_store_manager ();
  const binding_cluster *summary_cluster
    = summary.get_cluster (summary_base_reg);

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
	      = get_or_create_cluster (caller_base_reg);
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
	  return;
	const region *caller_dest_reg
	  = cd.get_model ()->deref_rvalue (caller_ptr_sval,
					   NULL_TREE,
					   cd.get_ctxt ());
	const svalue *summary_sval
	  = summary.get_any_binding (mgr, summary_base_reg);
	if (!summary_sval)
	  return;
	const svalue *caller_sval
	  = r.convert_svalue_from_summary (summary_sval);
	if (!caller_sval)
	  caller_sval =
	    reg_mgr->get_or_create_unknown_svalue (summary_sval->get_type ());
	set_value (mgr, caller_dest_reg,
		   caller_sval, NULL /* uncertainty_t * */);
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
	  return;
	const svalue *summary_sval
	  = summary.get_any_binding (mgr, summary_base_reg);
	if (!summary_sval)
	  summary_sval = reg_mgr->get_or_create_compound_svalue
	    (summary_base_reg->get_type (),
	     summary_cluster->get_map ());
	const svalue *caller_sval
	  = r.convert_svalue_from_summary (summary_sval);
	if (!caller_sval)
	  caller_sval =
	    reg_mgr->get_or_create_unknown_svalue (summary_sval->get_type ());
	set_value (mgr, caller_dest_reg,
		   caller_sval, NULL /* uncertainty_t * */);
      }
      break;

    case RK_ALLOCA:
      /* Ignore bindings of alloca regions in the summary.  */
      break;
    }
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

/* Implementation detail of ASSERT_BIT_RANGE_FROM_MASK_EQ.  */

static void
assert_bit_range_from_mask_eq (const location &loc,
			       unsigned HOST_WIDE_INT mask,
			       const bit_range &expected)
{
  bit_range actual (0, 0);
  bool ok = bit_range::from_mask (mask, &actual);
  ASSERT_TRUE_AT (loc, ok);
  ASSERT_EQ_AT (loc, actual, expected);
}

/* Assert that bit_range::from_mask (MASK) returns true, and writes
   out EXPECTED_BIT_RANGE.  */

#define ASSERT_BIT_RANGE_FROM_MASK_EQ(MASK, EXPECTED_BIT_RANGE) \
  SELFTEST_BEGIN_STMT							\
  assert_bit_range_from_mask_eq (SELFTEST_LOCATION, MASK,		\
				 EXPECTED_BIT_RANGE);			\
  SELFTEST_END_STMT

/* Implementation detail of ASSERT_NO_BIT_RANGE_FROM_MASK.  */

static void
assert_no_bit_range_from_mask_eq (const location &loc,
				  unsigned HOST_WIDE_INT mask)
{
  bit_range actual (0, 0);
  bool ok = bit_range::from_mask (mask, &actual);
  ASSERT_FALSE_AT (loc, ok);
}

/* Assert that bit_range::from_mask (MASK) returns false.  */

#define ASSERT_NO_BIT_RANGE_FROM_MASK(MASK) \
  SELFTEST_BEGIN_STMT							\
  assert_no_bit_range_from_mask_eq (SELFTEST_LOCATION, MASK);		\
  SELFTEST_END_STMT

/* Verify that bit_range::from_mask works as expected.  */

static void
test_bit_range_from_mask ()
{
  /* Should fail on zero.  */
  ASSERT_NO_BIT_RANGE_FROM_MASK (0);

  /* Verify 1-bit masks.  */
  ASSERT_BIT_RANGE_FROM_MASK_EQ (1, bit_range (0, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (2, bit_range (1, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (4, bit_range (2, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (8, bit_range (3, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (16, bit_range (4, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (32, bit_range (5, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (64, bit_range (6, 1));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (128, bit_range (7, 1));

  /* Verify N-bit masks starting at bit 0.  */
  ASSERT_BIT_RANGE_FROM_MASK_EQ (3, bit_range (0, 2));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (7, bit_range (0, 3));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (15, bit_range (0, 4));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (31, bit_range (0, 5));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (63, bit_range (0, 6));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (127, bit_range (0, 7));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (255, bit_range (0, 8));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0xffff, bit_range (0, 16));

  /* Various other tests. */
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0x30, bit_range (4, 2));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0x700, bit_range (8, 3));
  ASSERT_BIT_RANGE_FROM_MASK_EQ (0x600, bit_range (9, 2));

  /* Multiple ranges of set bits should fail.  */
  ASSERT_NO_BIT_RANGE_FROM_MASK (0x101);
  ASSERT_NO_BIT_RANGE_FROM_MASK (0xf0f0f0f0);
}

/* Implementation detail of ASSERT_OVERLAP.  */

static void
assert_overlap (const location &loc,
		const concrete_binding *b1,
		const concrete_binding *b2)
{
  ASSERT_TRUE_AT (loc, b1->overlaps_p (*b2));
  ASSERT_TRUE_AT (loc, b2->overlaps_p (*b1));
}

/* Implementation detail of ASSERT_DISJOINT.  */

static void
assert_disjoint (const location &loc,
		 const concrete_binding *b1,
		 const concrete_binding *b2)
{
  ASSERT_FALSE_AT (loc, b1->overlaps_p (*b2));
  ASSERT_FALSE_AT (loc, b2->overlaps_p (*b1));
}

/* Assert that B1 and B2 overlap, checking both ways.  */

#define ASSERT_OVERLAP(B1, B2) \
  SELFTEST_BEGIN_STMT				\
  assert_overlap (SELFTEST_LOCATION, B1, B2);	\
  SELFTEST_END_STMT

/* Assert that B1 and B2 do not overlap, checking both ways.  */

#define ASSERT_DISJOINT(B1, B2) \
  SELFTEST_BEGIN_STMT				\
  assert_disjoint (SELFTEST_LOCATION, B1, B2);  \
  SELFTEST_END_STMT

/* Verify that concrete_binding::overlaps_p works as expected.  */

static void
test_binding_key_overlap ()
{
  store_manager mgr (NULL);

  /* Various 8-bit bindings.  */
  const concrete_binding *cb_0_7 = mgr.get_concrete_binding (0, 8);
  const concrete_binding *cb_8_15 = mgr.get_concrete_binding (8, 8);
  const concrete_binding *cb_16_23 = mgr.get_concrete_binding (16, 8);
  const concrete_binding *cb_24_31 = mgr.get_concrete_binding (24, 8);

  /* 16-bit bindings.  */
  const concrete_binding *cb_0_15 = mgr.get_concrete_binding (0, 16);
  const concrete_binding *cb_8_23 = mgr.get_concrete_binding (8, 16);
  const concrete_binding *cb_16_31 = mgr.get_concrete_binding (16, 16);

  /* 32-bit binding.  */
  const concrete_binding *cb_0_31 = mgr.get_concrete_binding (0, 32);

  /* Everything should self-overlap.  */
  ASSERT_OVERLAP (cb_0_7, cb_0_7);
  ASSERT_OVERLAP (cb_8_15, cb_8_15);
  ASSERT_OVERLAP (cb_16_23, cb_16_23);
  ASSERT_OVERLAP (cb_24_31, cb_24_31);
  ASSERT_OVERLAP (cb_0_15, cb_0_15);
  ASSERT_OVERLAP (cb_8_23, cb_8_23);
  ASSERT_OVERLAP (cb_16_31, cb_16_31);
  ASSERT_OVERLAP (cb_0_31, cb_0_31);

  /* Verify the 8-bit bindings that don't overlap each other.  */
  ASSERT_DISJOINT (cb_0_7, cb_8_15);
  ASSERT_DISJOINT (cb_8_15, cb_16_23);

  /* Check for overlap of differently-sized bindings.  */
  ASSERT_OVERLAP (cb_0_7, cb_0_31);
  /* ...and with differing start points.  */
  ASSERT_OVERLAP (cb_8_15, cb_0_31);
  ASSERT_DISJOINT (cb_8_15, cb_16_31);
  ASSERT_OVERLAP (cb_16_23, cb_0_31);
  ASSERT_OVERLAP (cb_16_31, cb_0_31);

  ASSERT_DISJOINT (cb_0_7, cb_8_23);
  ASSERT_OVERLAP (cb_8_23, cb_16_23);
  ASSERT_OVERLAP (cb_8_23, cb_16_31);
  ASSERT_DISJOINT (cb_8_23, cb_24_31);
}

/* Run all of the selftests within this file.  */

void
analyzer_store_cc_tests ()
{
  test_bit_range_intersects_p ();
  test_bit_range_from_mask ();
  test_binding_key_overlap ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
