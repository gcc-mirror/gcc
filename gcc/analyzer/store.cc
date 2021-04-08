/* Classes for modeling the state of memory.
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
#include "diagnostic-color.h"
#include "diagnostic-metadata.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/analyzer-selftests.h"
#include "stor-layout.h"

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
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Get a human-readable string for KIND for dumps.  */

const char *binding_kind_to_string (enum binding_kind kind)
{
  switch (kind)
    {
    default:
    case BK_empty:
    case BK_deleted:
      /* We shouldn't be attempting to print the hash kinds.  */
      gcc_unreachable ();
    case BK_direct:
      return "direct";
    case BK_default:
      return "default";
    }
}

/* class binding_key.  */

const binding_key *
binding_key::make (store_manager *mgr, const region *r,
		   enum binding_kind kind)
{
  region_offset offset = r->get_offset ();
  if (offset.symbolic_p ())
    return mgr->get_symbolic_binding (r, kind);
  else
    {
      bit_size_t bit_size;
      if (r->get_bit_size (&bit_size))
	return mgr->get_concrete_binding (offset.get_bit_offset (),
					  bit_size, kind);
      else
	return mgr->get_symbolic_binding (r, kind);
    }
}

/* Base class implementation of binding_key::dump_to_pp vfunc.  */

void
binding_key::dump_to_pp (pretty_printer *pp, bool /*simple*/) const
{
  pp_printf (pp, "kind: %s", binding_kind_to_string (m_kind));
}

/* Dump this binding_key to stderr.  */

DEBUG_FUNCTION void
binding_key::dump (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
  pp_flush (&pp);
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
  enum binding_kind kind1 = k1->get_kind ();
  enum binding_kind kind2 = k2->get_kind ();
  if (kind1 != kind2)
    return (int)kind1 - (int)kind2;

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

/* class concrete_binding : public binding_key.  */

/* Implementation of binding_key::dump_to_pp vfunc for concrete_binding.  */

void
concrete_binding::dump_to_pp (pretty_printer *pp, bool simple) const
{
  binding_key::dump_to_pp (pp, simple);
  pp_string (pp, ", start: ");
  pp_wide_int (pp, m_start_bit_offset, SIGNED);
  pp_string (pp, ", size: ");
  pp_wide_int (pp, m_size_in_bits, SIGNED);
  pp_string (pp, ", next: ");
  pp_wide_int (pp, get_next_bit_offset (), SIGNED);
}

/* Return true if this binding overlaps with OTHER.  */

bool
concrete_binding::overlaps_p (const concrete_binding &other) const
{
  if (m_start_bit_offset < other.get_next_bit_offset ()
      && get_next_bit_offset () > other.get_start_bit_offset ())
    return true;
  return false;
}

/* Comparator for use by vec<const concrete_binding *>::qsort.  */

int
concrete_binding::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const concrete_binding *b1 = *(const concrete_binding * const *)p1;
  const concrete_binding *b2 = *(const concrete_binding * const *)p2;

  if (int kind_cmp = b1->get_kind () - b2->get_kind ())
    return kind_cmp;

  if (int start_cmp = wi::cmps (b1->m_start_bit_offset, b2->m_start_bit_offset))
    return start_cmp;

  return wi::cmpu (b1->m_size_in_bits, b2->m_size_in_bits);
}

/* class symbolic_binding : public binding_key.  */

void
symbolic_binding::dump_to_pp (pretty_printer *pp, bool simple) const
{
  binding_key::dump_to_pp (pp, simple);
  pp_string (pp, ", region: ");
  m_region->dump_to_pp (pp, simple);
}

/* Comparator for use by vec<const symbolic_binding *>::qsort.  */

int
symbolic_binding::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const symbolic_binding *b1 = *(const symbolic_binding * const *)p1;
  const symbolic_binding *b2 = *(const symbolic_binding * const *)p2;

  if (int kind_cmp = b1->get_kind () - b2->get_kind ())
    return kind_cmp;

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
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple, true);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Return a new json::object of the form
   {KEY_DESC : SVALUE_DESC,
    ...for the various key/value pairs in this binding_map}.  */

json::object *
binding_map::to_json () const
{
  json::object *map_obj = new json::object ();

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
      map_obj->set (key_desc.m_buffer, value->to_json ());
      key_desc.maybe_free ();
    }

  return map_obj;
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
  region_offset min_offset = min_element->get_offset ();
  if (min_offset.symbolic_p ())
    return false;
  bit_offset_t start_bit_offset = min_offset.get_bit_offset ();
  store_manager *smgr = mgr->get_store_manager ();
  const binding_key *max_element_key
    = binding_key::make (smgr, max_element, BK_direct);
  if (max_element_key->symbolic_p ())
    return false;
  const concrete_binding *max_element_ckey
    = max_element_key->dyn_cast_concrete_binding ();
  bit_size_t range_size_in_bits
    = max_element_ckey->get_next_bit_offset () - start_bit_offset;
  const concrete_binding *range_key
    = smgr->get_concrete_binding (start_bit_offset, range_size_in_bits,
				  BK_direct);
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
    = get_subregion_within_ctor (parent_reg, index, mgr);
  if (TREE_CODE (val) == CONSTRUCTOR)
    return apply_ctor_to_region (child_reg, val, mgr);
  else
    {
      const svalue *sval = get_svalue_for_ctor_val (val, mgr);
      const binding_key *k
	= binding_key::make (mgr->get_store_manager (), child_reg,
			     BK_direct);
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
	  region_offset child_base_offset = child_reg->get_offset ();
	  if (child_base_offset.symbolic_p ())
	    return false;
	  /* Convert to an offset relative to the parent region.  */
	  region_offset parent_base_offset = parent_reg->get_offset ();
	  gcc_assert (!parent_base_offset.symbolic_p ());
	  bit_offset_t child_parent_offset
	    = (child_base_offset.get_bit_offset ()
	       - parent_base_offset.get_bit_offset ());
	  /* Create a concrete key for the child within the parent.  */
	  k = mgr->get_store_manager ()->get_concrete_binding
	    (child_parent_offset, sval_bit_size, BK_direct);
	}
      gcc_assert (k->concrete_p ());
      put (k, sval);
      return true;
    }
}

/* class binding_cluster.  */

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
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  pp_string (&pp, "  cluster for: ");
  m_base_region->dump_to_pp (&pp, simple);
  pp_string (&pp, ": ");
  pp_newline (&pp);
  dump_to_pp (&pp, simple, true);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Return a new json::object of the form
   {"escaped": true/false,
    "touched": true/false,
    "map" : object for the the binding_map.  */

json::object *
binding_cluster::to_json () const
{
  json::object *cluster_obj = new json::object ();

  cluster_obj->set ("escaped", new json::literal (m_escaped));
  cluster_obj->set ("touched", new json::literal (m_touched));
  cluster_obj->set ("map", m_map.to_json ());

  return cluster_obj;
}

/* Add a binding of SVAL of kind KIND to REG, unpacking SVAL if it is a
   compound_sval.  */

void
binding_cluster::bind (store_manager *mgr,
		       const region *reg, const svalue *sval,
		       binding_kind kind)
{
  if (const compound_svalue *compound_sval
	= sval->dyn_cast_compound_svalue ())
    {
      bind_compound_sval (mgr, reg, compound_sval);
      return;
    }

  const binding_key *binding = binding_key::make (mgr, reg, kind);
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
  region_offset reg_offset = reg->get_offset ();
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
					 concrete_key->get_size_in_bits (),
					 iter_key->get_kind ());
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
  remove_overlapping_bindings (mgr, reg, NULL);
}

/* Remove any bindings for REG within this cluster.  */

void
binding_cluster::purge_region (store_manager *mgr, const region *reg)
{
  gcc_assert (reg->get_kind () == RK_DECL);
  const binding_key *binding
    = binding_key::make (mgr, const_cast<region *> (reg),
			 BK_direct);
  m_map.remove (binding);
}

/* Mark REG within this cluster as being filled with zeroes.
   Remove all bindings, add a default binding to zero, and clear the
   TOUCHED flag.  */

void
binding_cluster::zero_fill_region (store_manager *mgr, const region *reg)
{
  clobber_region (mgr, reg);

  /* Add a default binding to zero.  */
  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  tree cst_zero = build_int_cst (integer_type_node, 0);
  const svalue *cst_sval = sval_mgr->get_or_create_constant_svalue (cst_zero);
  const svalue *bound_sval = cst_sval;
  if (reg->get_type ())
    bound_sval = sval_mgr->get_or_create_unaryop (reg->get_type (), NOP_EXPR,
						  cst_sval);
  bind (mgr, reg, bound_sval, BK_default);

  m_touched = false;
}

/* Mark REG within this cluster as being unknown.
   If UNCERTAINTY is non-NULL, use it to record any svalues that
   had bindings to them removed, as being maybe-bound.  */

void
binding_cluster::mark_region_as_unknown (store_manager *mgr,
					 const region *reg,
					 uncertainty_t *uncertainty)
{
  remove_overlapping_bindings (mgr, reg, uncertainty);

  /* Add a default binding to "unknown".  */
  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  const svalue *sval
    = sval_mgr->get_or_create_unknown_svalue (reg->get_type ());
  bind (mgr, reg, sval, BK_default);
}

/* Get any SVAL bound to REG within this cluster via kind KIND,
   without checking parent regions of REG.  */

const svalue *
binding_cluster::get_binding (store_manager *mgr,
			      const region *reg,
			      binding_kind kind) const
{
  const binding_key *reg_binding = binding_key::make (mgr, reg, kind);
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
	    = binding_key::make (mgr, parent_reg, kind);
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
	      sval = rmm_mgr->get_or_create_sub_svalue (reg->get_type (),
							sval, iter_reg);
	    }
	}
    }
  return sval;
}

/* Get any SVAL bound to REG within this cluster via kind KIND,
   either directly for REG, or recursively checking for bindings within
   parent regions and extracting subvalues if need be.  */

const svalue *
binding_cluster::get_binding_recursive (store_manager *mgr,
					const region *reg,
					enum binding_kind kind) const
{
  if (const svalue *sval = get_binding (mgr, reg, kind))
    return sval;
  if (reg != m_base_region)
    if (const region *parent_reg = reg->get_parent_region ())
      if (const svalue *parent_sval
	  = get_binding_recursive (mgr, parent_reg, kind))
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
  /* Look for a "direct" binding.  */
  if (const svalue *direct_sval
      = get_binding_recursive (mgr, reg, BK_direct))
    return direct_sval;

  /* Look for a "default" binding, but not if there's been a symbolic
     write.  */
  if (!m_touched)
    if (const svalue *default_sval
	= get_binding_recursive (mgr, reg, BK_default))
      return default_sval;

  /* If this cluster has been touched by a symbolic write, then the content
     of any subregion not currently specifically bound is "UNKNOWN".  */
  if (m_touched)
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
  binding_map map;

  region_offset cluster_offset = m_base_region->get_offset ();
  if (cluster_offset.symbolic_p ())
    return NULL;
  region_offset reg_offset = reg->get_offset ();
  if (reg_offset.symbolic_p ())
    return NULL;

  for (map_t::iterator iter = m_map.begin (); iter != m_map.end (); ++iter)
    {
      const binding_key *key = (*iter).first;
      const svalue *sval = (*iter).second;

      if (const concrete_binding *concrete_key
	  = key->dyn_cast_concrete_binding ())
	{
	  /* Skip bindings that are outside the bit range of REG.  */
	  if (concrete_key->get_start_bit_offset ()
	      < reg_offset.get_bit_offset ())
	    continue;
	  bit_size_t reg_bit_size;
	  if (reg->get_bit_size (&reg_bit_size))
	    if (concrete_key->get_start_bit_offset ()
	      >= reg_offset.get_bit_offset () + reg_bit_size)
	    continue;

	  /* Get offset of KEY relative to REG, rather than to
	     the cluster.  */
	  bit_offset_t relative_start
	    = (concrete_key->get_start_bit_offset ()
	       - reg_offset.get_bit_offset ());
	  const concrete_binding *offset_concrete_key
	    = mgr->get_concrete_binding (relative_start,
					 concrete_key->get_size_in_bits (),
					 key->get_kind ());
	  map.put (offset_concrete_key, sval);
	}
      else
	return NULL;
    }

  if (map.elements () == 0)
    return NULL;

  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
  return sval_mgr->get_or_create_compound_svalue (reg->get_type (), map);
}


/* Populate OUT with all bindings within this cluster that overlap REG.  */

void
binding_cluster::get_overlapping_bindings (store_manager *mgr,
					   const region *reg,
					   auto_vec<const binding_key *> *out)
{
  const binding_key *binding
    = binding_key::make (mgr, reg, BK_direct);
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    {
      const binding_key *iter_key = (*iter).first;
      if (const concrete_binding *ckey
	    = binding->dyn_cast_concrete_binding ())
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

/* Remove any bindings within this cluster that overlap REG,
   but retain default bindings that overlap but aren't fully covered
   by REG.
   If UNCERTAINTY is non-NULL, use it to record any svalues that
   were removed, as being maybe-bound.  */

void
binding_cluster::remove_overlapping_bindings (store_manager *mgr,
					      const region *reg,
					      uncertainty_t *uncertainty)
{
  auto_vec<const binding_key *> bindings;
  get_overlapping_bindings (mgr, reg, &bindings);

  unsigned i;
  const binding_key *iter_binding;
  FOR_EACH_VEC_ELT (bindings, i, iter_binding)
    {
      /* Don't remove default bindings, unless the default binding
	 is fully covered by REG.  */
      if (iter_binding->get_kind () == BK_default)
	{
	  const binding_key *reg_binding
	    = binding_key::make (mgr, reg, BK_default);
	  if (reg_binding != iter_binding)
	    continue;
	}
      if (uncertainty)
	uncertainty->on_maybe_bound_sval (m_map.get (iter_binding));
      m_map.remove (iter_binding);
    }
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
  for (hash_set<const binding_key *>::iterator iter = keys.begin ();
       iter != keys.end (); ++iter)
    {
      const binding_key *key = *iter;
      const svalue *sval_a = cluster_a->get_any_value (key);
      const svalue *sval_b = cluster_b->get_any_value (key);

      if (sval_a == sval_b)
	{
	  gcc_assert (sval_a);
	  out_cluster->m_map.put (key, sval_a);
	  continue;
	}
      else if (sval_a && sval_b)
	{
	  region_model_manager *sval_mgr = mgr->get_svalue_manager ();
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
      tree type = sval_a ? sval_a->get_type () : sval_b->get_type ();
      const svalue *unknown_sval
	= mgr->get_svalue_manager ()->get_or_create_unknown_svalue (type);
      out_cluster->m_map.put (key, unknown_sval);
    }

  /* Handle the case where we get a default binding from one and a direct
     binding from the other.  */
  auto_vec<const concrete_binding *> duplicate_keys;
  for (map_t::iterator iter = out_cluster->m_map.begin ();
       iter != out_cluster->m_map.end (); ++iter)
    {
      const concrete_binding *ckey
	= (*iter).first->dyn_cast_concrete_binding ();
      if (!ckey)
	continue;
      if (ckey->get_kind () != BK_direct)
	continue;
      const concrete_binding *def_ckey
	= mgr->get_concrete_binding (ckey->get_start_bit_offset (),
				     ckey->get_size_in_bits (),
				     BK_default);
      if (out_cluster->m_map.get (def_ckey))
	duplicate_keys.safe_push (def_ckey);
    }
  unsigned i;
  const concrete_binding *key;
  FOR_EACH_VEC_ELT (duplicate_keys, i, key)
    out_cluster->m_map.remove (key);

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
	  if (!base_reg->symbolic_for_unknown_ptr_p ())
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
   as "touched", so that it has an unknown value, rather than an
   initial_svalue.  */

void
binding_cluster::on_unknown_fncall (const gcall *call,
				    store_manager *mgr)
{
  if (m_escaped)
    {
      m_map.empty ();

      /* Bind it to a new "conjured" value using CALL.  */
      const svalue *sval
	= mgr->get_svalue_manager ()->get_or_create_conjured_svalue
	    (m_base_region->get_type (), call, m_base_region);
      bind (mgr, m_base_region, sval, BK_direct);

      m_touched = true;
    }
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

/* Add PV to OUT_PVS, casting it to TYPE if if is not already of that type.  */

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
							    visited))
		    append_pathvar_with_type (pv, sval->get_type (), out_pvs);
		}
	    }
	  else
	    {
	      const symbolic_binding *skey = (const symbolic_binding *)key;
	      if (path_var pv
		  = model->get_representative_path_var (skey->get_region (),
							visited))
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

  const binding_key *key = binding_key::make (mgr, m_base_region, BK_direct);
  return get_any_value (key);
}

/* class store_manager.  */

/* binding consolidation.  */

const concrete_binding *
store_manager::get_concrete_binding (bit_offset_t start_bit_offset,
				     bit_offset_t size_in_bits,
				     enum binding_kind kind)
{
  concrete_binding b (start_bit_offset, size_in_bits, kind);
  if (concrete_binding *existing = m_concrete_binding_key_mgr.get (b))
    return existing;

  concrete_binding *to_save = new concrete_binding (b);
  m_concrete_binding_key_mgr.put (b, to_save);
  return to_save;
}

const symbolic_binding *
store_manager::get_symbolic_binding (const region *reg,
				     enum binding_kind kind)
{
  symbolic_binding b (reg, kind);
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
: m_called_unknown_fn (other.m_called_unknown_fn)
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
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple, true, NULL);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Return a new json::object of the form
   {PARENT_REGION_DESC: {BASE_REGION_DESC: object for binding_map,
			 ... for each cluster within parent region},
    ...for each parent region,
    "called_unknown_fn": true/false}.  */

json::object *
store::to_json () const
{
  json::object *store_obj = new json::object ();

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

      json::object *clusters_in_parent_reg_obj = new json::object ();

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
	  clusters_in_parent_reg_obj->set (base_reg_desc.m_buffer,
					   cluster->to_json ());
	  base_reg_desc.maybe_free ();
	}
      label_text parent_reg_desc = parent_reg->get_desc ();
      store_obj->set (parent_reg_desc.m_buffer, clusters_in_parent_reg_obj);
      parent_reg_desc.maybe_free ();
    }

  store_obj->set ("called_unknown_fn", new json::literal (m_called_unknown_fn));

  return store_obj;
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
		  const svalue *rhs_sval, enum binding_kind kind,
		  uncertainty_t *uncertainty)
{
  remove_overlapping_bindings (mgr, lhs_reg);

  rhs_sval = simplify_for_binding (rhs_sval);

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
    }
  else
    {
      lhs_cluster = get_or_create_cluster (lhs_base_reg);
      lhs_cluster->bind (mgr, lhs_reg, rhs_sval, kind);
    }

  /* Bindings to a cluster can affect other clusters if a symbolic
     base region is involved.
     Writes to concrete clusters can't affect other concrete clusters,
     but can affect symbolic clusters.
     Writes to symbolic clusters can affect both concrete and symbolic
     clusters.
     Invalidate our knowledge of other clusters that might have been
     affected by the write.  */
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
	      iter_cluster->mark_region_as_unknown (mgr, iter_base_reg,
						    uncertainty);
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
  if (const symbolic_region *sym_reg_a
      = base_reg_a->dyn_cast_symbolic_region ())
    {
      const svalue *sval_a = sym_reg_a->get_pointer ();
      if (sval_a->get_kind () == SK_INITIAL)
	if (tree decl_b = base_reg_b->maybe_get_decl ())
	  if (!is_global_var (decl_b))
	    {
	      /* The initial value of a pointer can't point to a local.  */
	      return tristate::TS_FALSE;
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

/* Zero-fill REG.  */

void
store::zero_fill_region (store_manager *mgr, const region *reg)
{
  const region *base_reg = reg->get_base_region ();
  if (base_reg->symbolic_for_unknown_ptr_p ())
    return;
  binding_cluster *cluster = get_or_create_cluster (base_reg);
  cluster->zero_fill_region (mgr, reg);
}

/* Mark REG as having unknown content.  */

void
store::mark_region_as_unknown (store_manager *mgr, const region *reg,
			       uncertainty_t *uncertainty)
{
  const region *base_reg = reg->get_base_region ();
  if (base_reg->symbolic_for_unknown_ptr_p ())
    return;
  binding_cluster *cluster = get_or_create_cluster (base_reg);
  cluster->mark_region_as_unknown (mgr, reg, uncertainty);
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

  if (base_reg->symbolic_for_unknown_ptr_p ())
    return;

  binding_cluster *cluster = get_or_create_cluster (base_reg);
  cluster->mark_as_escaped ();
}

/* Handle an unknown fncall by updating any clusters that have escaped
   (either in this fncall, or in a prior one).  */

void
store::on_unknown_fncall (const gcall *call, store_manager *mgr)
{
  m_called_unknown_fn = true;

  for (cluster_map_t::iterator iter = m_cluster_map.begin ();
       iter != m_cluster_map.end (); ++iter)
    (*iter).second->on_unknown_fncall (call, mgr);
}

/* Return true if a non-const pointer to BASE_REG (or something within it)
   has escaped to code outside of the TU being analyzed.  */

bool
store::escaped_p (const region *base_reg) const
{
  gcc_assert (base_reg);
  gcc_assert (base_reg->get_base_region () == base_reg);

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
					     out_pvs);
    }

  if (const initial_svalue *init_sval = sval->dyn_cast_initial_svalue ())
    {
      const region *reg = init_sval->get_region ();
      if (path_var pv = model->get_representative_path_var (reg,
							    visited))
	out_pvs->safe_push (pv);
    }
}

/* Remove all bindings overlapping REG within this store, removing
   any clusters that become redundant.  */

void
store::remove_overlapping_bindings (store_manager *mgr, const region *reg)
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
      cluster->remove_overlapping_bindings (mgr, reg, NULL);
    }
}

/* Subclass of visitor that accumulates a hash_set of the regions that
   were visited.  */

struct region_finder : public visitor
{
  void visit_region (const region *reg) FINAL OVERRIDE
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

#if CHECKING_P

namespace selftest {

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
  const concrete_binding *cb_0_7
    = mgr.get_concrete_binding (0, 8, BK_direct);
  const concrete_binding *cb_8_15
    = mgr.get_concrete_binding (8, 8, BK_direct);
  const concrete_binding *cb_16_23
    = mgr.get_concrete_binding (16, 8, BK_direct);
  const concrete_binding *cb_24_31
    = mgr.get_concrete_binding (24, 8, BK_direct);

  /* 16-bit bindings.  */
  const concrete_binding *cb_0_15
    = mgr.get_concrete_binding (0, 16, BK_direct);
  const concrete_binding *cb_8_23
    = mgr.get_concrete_binding (8, 16, BK_direct);
  const concrete_binding *cb_16_31
    = mgr.get_concrete_binding (16, 16, BK_direct);

  /* 32-bit binding.  */
  const concrete_binding *cb_0_31
    = mgr.get_concrete_binding (0, 32, BK_direct);

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
  test_binding_key_overlap ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
