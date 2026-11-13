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

#ifndef GCC_ANALYZER_STORE_H
#define GCC_ANALYZER_STORE_H

#include "text-art/tree-widget.h"
#include "analyzer/complexity.h"
#include "analyzer/concrete-ranges.h"
#include "analyzer/symbolic-ranges.h"
#include "analyzer/symbolic-bit-contents.h"
#include "analyzer/symbolic-memory-ranges.h"

/* Implementation of the region-based ternary model described in:
     "A Memory Model for Static Analysis of C Programs"
      (Zhongxing Xu, Ted Kremenek, and Jian Zhang)
     http://lcs.ios.ac.cn/~xuzb/canalyze/memmodel.pdf  */

/* Binding clusters
   ================

   The store models memory as a collection of "clusters", where regions
   are partitioned into clusters via their base region.

   For example, given:
     int a, b, c;
     struct coord { double x; double y; } verts[3];
   then "verts[0].y" and "verts[1].x" both have "verts" as their base region.
   Each of a, b, c, and verts will have their own clusters, so that we
   know that writes to e.g. "verts[1].x".don't affect e.g. "a".

   For each cluster that's been touched by the user's code we track an
   immutable bit_contents instance, updating it to point to a different
   bit_contents instance after each change.

   Handling symbolic regions requires us to handle aliasing.

   In the example above, each of a, b, c and verts are non-symbolic
   base regions and so their clusters are "concrete clusters", whereas given:
       struct coord *p, *q;
   then "*p" and "*q" are symbolic base regions, and thus "*p" and "*q"
   have "symbolic clusters".

   In the above, "verts[i].x" will have a symbolic *binding* within a
   concrete cluster for "verts", whereas "*p" is a symbolic *cluster*.

   Writes to concrete clusters can't affect other concrete clusters,
   but can affect symbolic clusters; e.g. after:
       verts[0].x = 42;
   we bind 42 in the cluster for "verts", but the clusters for "b" and "c"
   can't be affected.  Any symbolic clusters for *p and for *q can be
   affected, *p and *q could alias "verts".

   Writes to a symbolic cluster can affect other clusters, both
   concrete and symbolic; e.g. after:
       p->x = 17;
   we bind 17 within the cluster for "*p".  The concrete clusters for a, b,
   c, and verts could be affected, depending on whether *p aliases them.
   Similarly, the symbolic cluster to *q could be affected.


   Tracking the state within a binding cluster
   ===========================================

   Reads and writes to memory are handled as reading and writing bits to
   a cluster, where the memory ranges relative to the start of the cluster
   can be either fully concrete, or be symbolic (with a symbolic start offset
   and/or size).

   Consider this example (taken from
   gcc/testsuite/c-c++-common/analyzer/symbolic-1.c, adding calls to
   __analyzer_dump (); at the pertinent places):

     char arr[1024];
     arr[2] = a;  (1)
     arr[3] = b;  (2)
       After (1) and (2), we have this state:
	 | program_state
	 | ╰─ Region Model
	 |    ├─ Current Frame: frame: ‘test_1’@1
	 |    ╰─ Store
	 |       ├─ m_called_unknown_fn: false
	 |       ╰─ frame: ‘test_1’@1
	 |          ╰─ arr:
	 |             ╰─ (616): concrete bit overlay
	 |                ├─ concrete ranges
	 |                │  ├─ [byte 2]: (605): bit_vector_from_svalue[size: 1 byte]
	 |                │  │  ╰─ m_sval: (604): ‘char’: initial_svalue
	 |                │  │     ├─ value_range: {[irange] char VARYING}
	 |                │  │     ╰─ m_reg: (601): ‘char’: decl_region(‘a_48(D)’)
	 |                │  │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │  │           ╰─ parent: (1): stack region
	 |                │  │              ╰─ parent: (0): root region
	 |                │  ╰─ [byte 3]: (613): bit_vector_from_svalue[size: 1 byte]
	 |                │     ╰─ m_sval: (612): ‘char’: initial_svalue
	 |                │        ├─ value_range: {[irange] char VARYING}
	 |                │        ╰─ m_reg: (611): ‘char’: decl_region(‘b_50(D)’)
	 |                │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │              ╰─ {as above; descendents: 3, depth: 3}
	 |                ╰─ baseline: (7): uninitialized contents
       The cluster for "arr" has a concrete_bit_overlay for its contents,
       with bytes 2 and 3 bound to bit_vectors for the target encoding
       of svalues for the initial values of "a" and "b" respectively.  This
       is overlaid on top of the baseline "uninitialized" everywhere else, so
       that attempting to query e.g. arr[4] will return "uninitialized".

       "a" and "b" are each in their own clusters, with no explicit
       bindings, and thus have their "implicit contents": in this case
       concrete_bit_overlay instances bound at [0..sizeof(int)) to the bit
       vectors for the initial_svalue of "a" and "b" respectively.

     arr[3] = c;  (3)
       After (3), the contents of the cluster for "arr" has been replaced by
       a new concrete_bit_overlay instance, the same as the previous one,
       but with byte 3 now bound to a bit vector for the
       initial_svalue for "c":
	 | program_state
	 | ╰─ Region Model
	 |    ├─ Current Frame: frame: ‘test_1’@1
	 |    ╰─ Store
	 |       ├─ m_called_unknown_fn: false
	 |       ╰─ frame: ‘test_1’@1
	 |          ╰─ arr:
	 |             ╰─ (648): concrete bit overlay
	 |                ├─ concrete ranges
	 |                │  ├─ [byte 2]: (605): bit_vector_from_svalue[size: 1 byte]
	 |                │  │  ╰─ m_sval: (604): ‘char’: initial_svalue
	 |                │  │     ├─ value_range: {[irange] char VARYING}
	 |                │  │     ╰─ m_reg: (601): ‘char’: decl_region(‘a_48(D)’)
	 |                │  │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │  │           ╰─ parent: (1): stack region
	 |                │  │              ╰─ parent: (0): root region
	 |                │  ╰─ [byte 3]: (645): bit_vector_from_svalue[size: 1 byte]
	 |                │     ╰─ m_sval: (644): ‘char’: initial_svalue
	 |                │        ├─ value_range: {[irange] char VARYING}
	 |                │        ╰─ m_reg: (643): ‘char’: decl_region(‘c_55(D)’)
	 |                │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │              ╰─ {as above; descendents: 3, depth: 3}
	 |                ╰─ baseline: (7): uninitialized contents
       Note that the concrete_bit_overlay (and other bit_contents instances)
       are immutable and pure-functional - when a cluster is written to, we
       update the cluster's m_contents to point to a different bit_contents.

     arr[i] = d;  (4)
       After the write to the symbolic array index at (4), the cluster for
       "arr" no longer uses the concrete_bit_overlay and instead points to
       a symbolic_bit_overlay, where the bytes at element [i] are bound to
       the bit_vector fot the initial value of "d":
	 | program_state
	 | ╰─ Region Model
	 |    ├─ Current Frame: frame: ‘test_1’@1
	 |    ╰─ Store
	 |       ├─ m_called_unknown_fn: false
	 |       ╰─ frame: ‘test_1’@1
	 |          ╰─ arr:
	 |             ╰─ (670): symbolic bit overlay
	 |                ├─ symbolic ranges
	 |                │  ├─ symbolic range
	 |                │  │  ├─ start offset: (571): bits_from_bytes_scalar
	 |                │  │  │  ╰─ (570): scalar_from_svalue<bytes>
	 |                │  │  │     ╰─ (565): ‘int’: initial_svalue
	 |                │  │  │        ├─ value_range: {[irange] int VARYING}
	 |                │  │  │        ╰─ m_reg: (564): ‘int’: decl_region(‘i_61(D)’)
	 |                │  │  │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │  │  │              ╰─ parent: (1): stack region
	 |                │  │  │                 ╰─ parent: (0): root region
	 |                │  │  ╰─ size: (11): concrete_scalar<bits>: 8
	 |                │  ╰─ (657): bit_vector_from_svalue[size: 1 byte]
	 |                │     ╰─ m_sval: (656): ‘char’: initial_svalue
	 |                │        ├─ value_range: {[irange] char VARYING}
	 |                │        ╰─ m_reg: (655): ‘char’: decl_region(‘d_62(D)’)
	 |                │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │              ╰─ {as above; descendents: 3, depth: 3}
	 |                ╰─ baseline: (669): untracked_bit_contents
	 |                   ╰─ maybe live values
	 |                      ├─ (605): bit_vector_from_svalue[size: 1 byte]
	 |                      │  ╰─ m_sval: (604): ‘char’: initial_svalue
	 |                      │     ├─ value_range: {[irange] char VARYING}
	 |                      │     ╰─ m_reg: (601): ‘char’: decl_region(‘a_48(D)’)
	 |                      │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                      │           ╰─ {as above; descendents: 3, depth: 3}
	 |                      ╰─ (645): bit_vector_from_svalue[size: 1 byte]
	 |                         ╰─ m_sval: (644): ‘char’: initial_svalue
	 |                            ├─ value_range: {[irange] char VARYING}
	 |                            ╰─ m_reg: (643): ‘char’: decl_region(‘c_55(D)’)
	 |                               ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                                  ╰─ {as above; descendents: 3, depth: 3}
       We also set the baseline for this overlay to an untracked_bit_contents
       instance.  This indicates that we've given up on tracking the contents
       of "arr" at other locations (reads will return "unknown", rather than
       "uninitialized").  The untracked_bit_contents has a set of values that
       are "maybe live", capturing that the initial values of "a" and "c" may
       still be live within "arr" (to avoid leak false +ves), since we don't
       know if i == 3 or 4.

       Note how the IDs of the "maybe live values" nodes below match those
       within the concrete_bit_overlay above: the symbolic::node instances
       persist through the analysis.

     arr[j] = e;  (5)
       After (5), the user's code has written to a possibly-different
       symbolic index within "arr", so we have a new symbolic_bit_overlay,
       binding the byte at [j] to the bit vector for the initial value of "e",
       and the "maybe live values" has gained the value that was at [i], since
       we don't know whether j == i (or j == 3, or j == 4):
	 | program_state
	 | ╰─ Region Model
	 |    ├─ Current Frame: frame: ‘test_1’@1
	 |    ╰─ Store
	 |       ├─ m_called_unknown_fn: false
	 |       ╰─ frame: ‘test_1’@1
	 |          ╰─ arr:
	 |             ╰─ (685): symbolic bit overlay
	 |                ├─ symbolic ranges
	 |                │  ├─ symbolic range
	 |                │  │  ├─ start offset: (590): bits_from_bytes_scalar
	 |                │  │  │  ╰─ (589): scalar_from_svalue<bytes>
	 |                │  │  │     ╰─ (584): ‘int’: initial_svalue
	 |                │  │  │        ├─ value_range: {[irange] int VARYING}
	 |                │  │  │        ╰─ m_reg: (583): ‘int’: decl_region(‘j_68(D)’)
	 |                │  │  │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │  │  │              ╰─ parent: (1): stack region
	 |                │  │  │                 ╰─ parent: (0): root region
	 |                │  │  ╰─ size: (11): concrete_scalar<bits>: 8
	 |                │  ╰─ (676): bit_vector_from_svalue[size: 1 byte]
	 |                │     ╰─ m_sval: (675): ‘char’: initial_svalue
	 |                │        ├─ value_range: {[irange] char VARYING}
	 |                │        ╰─ m_reg: (674): ‘char’: decl_region(‘e_69(D)’)
	 |                │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │              ╰─ {as above; descendents: 3, depth: 3}
	 |                ╰─ baseline: (684): untracked_bit_contents
	 |                   ╰─ maybe live values
	 |                      ├─ (605): bit_vector_from_svalue[size: 1 byte]
	 |                      │  ╰─ m_sval: (604): ‘char’: initial_svalue
	 |                      │     ├─ value_range: {[irange] char VARYING}
	 |                      │     ╰─ m_reg: (601): ‘char’: decl_region(‘a_48(D)’)
	 |                      │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                      │           ╰─ {as above; descendents: 3, depth: 3}
	 |                      ├─ (645): bit_vector_from_svalue[size: 1 byte]
	 |                      │  ╰─ m_sval: (644): ‘char’: initial_svalue
	 |                      │     ├─ value_range: {[irange] char VARYING}
	 |                      │     ╰─ m_reg: (643): ‘char’: decl_region(‘c_55(D)’)
	 |                      │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                      │           ╰─ {as above; descendents: 3, depth: 3}
	 |                      ╰─ (657): bit_vector_from_svalue[size: 1 byte]
	 |                         ╰─ m_sval: (656): ‘char’: initial_svalue
	 |                            ├─ value_range: {[irange] char VARYING}
	 |                            ╰─ m_reg: (655): ‘char’: decl_region(‘d_62(D)’)
	 |                               ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                                  ╰─ {as above; descendents: 3, depth: 3}

     arr[3] = f;  (6)
       After (6), we have a concrete_bit_overlay again, this, with a binding
       at byte [3], where the baseline is an untracked_bit_contents, with all
       the various other values we're written into "arr" as "maybe live":
	 | program_state
	 | ╰─ Region Model
	 |    ├─ Current Frame: frame: ‘test_1’@1
	 |    ╰─ Store
	 |       ├─ m_called_unknown_fn: false
	 |       ╰─ frame: ‘test_1’@1
	 |          ╰─ arr:
	 |             ╰─ (691): concrete bit overlay
	 |                ├─ concrete ranges
	 |                │  ╰─ [byte 3]: (688): bit_vector_from_svalue[size: 1 byte]
	 |                │     ╰─ m_sval: (687): ‘char’: initial_svalue
	 |                │        ├─ value_range: {[irange] char VARYING}
	 |                │        ╰─ m_reg: (686): ‘char’: decl_region(‘f_74(D)’)
	 |                │           ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                │              ╰─ parent: (1): stack region
	 |                │                 ╰─ parent: (0): root region
	 |                ╰─ baseline: (690): untracked_bit_contents
	 |                   ╰─ maybe live values
	 |                      ├─ (605): bit_vector_from_svalue[size: 1 byte]
	 |                      │  ╰─ m_sval: (604): ‘char’: initial_svalue
	 |                      │     ├─ value_range: {[irange] char VARYING}
	 |                      │     ╰─ m_reg: (601): ‘char’: decl_region(‘a_48(D)’)
	 |                      │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                      │           ╰─ {as above; descendents: 3, depth: 3}
	 |                      ├─ (645): bit_vector_from_svalue[size: 1 byte]
	 |                      │  ╰─ m_sval: (644): ‘char’: initial_svalue
	 |                      │     ├─ value_range: {[irange] char VARYING}
	 |                      │     ╰─ m_reg: (643): ‘char’: decl_region(‘c_55(D)’)
	 |                      │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                      │           ╰─ {as above; descendents: 3, depth: 3}
	 |                      ├─ (657): bit_vector_from_svalue[size: 1 byte]
	 |                      │  ╰─ m_sval: (656): ‘char’: initial_svalue
	 |                      │     ├─ value_range: {[irange] char VARYING}
	 |                      │     ╰─ m_reg: (655): ‘char’: decl_region(‘d_62(D)’)
	 |                      │        ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                      │           ╰─ {as above; descendents: 3, depth: 3}
	 |                      ╰─ (676): bit_vector_from_svalue[size: 1 byte]
	 |                         ╰─ m_sval: (675): ‘char’: initial_svalue
	 |                            ├─ value_range: {[irange] char VARYING}
	 |                            ╰─ m_reg: (674): ‘char’: decl_region(‘e_69(D)’)
	 |                               ╰─ parent: (522): frame_region(‘test_1’, index: 0, depth: 1)
	 |                                  ╰─ {as above; descendents: 3, depth: 3}
*/


namespace ana {

/* A class for keeping track of aspects of a program_state that we don't
   know about, to avoid false positives about leaks.

   Consider:

      p->field = malloc (1024);
      q->field = NULL;

   where we don't know whether or not p and q point to the same memory,
   and:

      p->field = malloc (1024);
      unknown_fn (p);

   In both cases, the svalue for the address of the allocated buffer
   goes from being bound to p->field to not having anything explicitly bound
   to it.

   Given that we conservatively discard bindings due to possible aliasing or
   calls to unknown function, the store loses references to svalues,
   but these svalues could still be live.  We don't want to warn about
   them leaking - they're effectively in a "maybe live" state.

   This "maybe live" information is somewhat transient.

   We don't want to store this "maybe live" information in the program_state,
   region_model, or store, since we don't want to bloat these objects (and
   potentially bloat the exploded_graph with more nodes).
   However, we can't store it in the region_model_context, as these context
   objects sometimes don't last long enough to be around when comparing the
   old vs the new state.

   This class is a way to track a set of such svalues, so that we can
   temporarily capture that they are in a "maybe live" state whilst
   comparing old and new states.  */

class uncertainty_t
{
public:
  typedef hash_set<const symbolic::value_node *>::iterator iterator;

  void on_maybe_bound_value (const symbolic::value_node &val);
  void on_mutable_value_at_unknown_call (const symbolic::value_node &val);

  bool unknown_sm_state_p (const svalue *sval);

  void add_bit_contents (const symbolic::bit_contents &bc);

  void dump () const;
  void dump_to_logger (logger &) const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (symbolic::node_dump_state &nds) const;

  iterator begin_maybe_bound_svals () const
  {
    return m_maybe_bound_vals.begin ();
  }
  iterator end_maybe_bound_svals () const
  {
    return m_maybe_bound_vals.end ();
  }

private:

  /* Values that might or might not still be bound.  */
  hash_set<const symbolic::value_node *> m_maybe_bound_vals;

  /* Values that have mutable sm-state at unknown calls.  */
  hash_set<const  symbolic::value_node *> m_mutable_at_unknown_call_vals;
};

class byte_range;

// TODO: eliminate this in favor of concrete_bit_range:
/* A concrete range of bits.  */

struct bit_range
{
  bit_range (bit_offset_t start_bit_offset, bit_size_t size_in_bits)
  : m_start_bit_offset (start_bit_offset),
    m_size_in_bits (size_in_bits)
  {}

  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;

  std::unique_ptr<json::object> to_json () const;

  bool empty_p () const
  {
    return m_size_in_bits == 0;
  }

  bit_offset_t get_start_bit_offset () const
  {
    return m_start_bit_offset;
  }
  bit_offset_t get_next_bit_offset () const
  {
    return m_start_bit_offset + m_size_in_bits;
  }
  bit_offset_t get_last_bit_offset () const
  {
    gcc_assert (!empty_p ());
    return get_next_bit_offset () - 1;
  }

  bool contains_p (bit_offset_t offset) const
  {
    return (offset >= get_start_bit_offset ()
	    && offset < get_next_bit_offset ());
  }

  bool contains_p (const bit_range &other, bit_range *out) const;

  bool operator== (const bit_range &other) const
  {
    return (m_start_bit_offset == other.m_start_bit_offset
	    && m_size_in_bits == other.m_size_in_bits);
  }

  bool intersects_p (const bit_range &other) const
  {
    return (get_start_bit_offset () < other.get_next_bit_offset ()
	    && other.get_start_bit_offset () < get_next_bit_offset ());
  }
  bool intersects_p (const bit_range &other,
		     bit_size_t *out_num_overlap_bits) const;
  bool intersects_p (const bit_range &other,
		     bit_range *out_this,
		     bit_range *out_other) const;

  bool exceeds_p (const bit_range &other,
		  bit_range *out_overhanging_bit_range) const;

  bool falls_short_of_p (bit_offset_t offset,
			 bit_range *out_fall_short_bits) const;

  static int cmp (const bit_range &br1, const bit_range &br2);

  bit_range operator- (bit_offset_t offset) const;

  bool as_byte_range (byte_range *out) const;

  bool
  operator< (const bit_range &other) const
  {
    if (m_start_bit_offset < other.m_start_bit_offset)
      return true;
    if (m_start_bit_offset > other.m_start_bit_offset)
      return false;
    return (m_size_in_bits < other.m_size_in_bits);
  }

  hashval_t hash () const
  {
    inchash::hash hstate;
    hstate.add_wide_int (m_start_bit_offset);
    hstate.add_wide_int (m_size_in_bits);
    return hstate.end ();
  }

  bit_offset_t m_start_bit_offset;
  bit_size_t m_size_in_bits;
};

inline concrete::range<integer_with_unit<units::bit>>
convert_from_bit_range (bit_range old_bits)
{
  return concrete::range<integer_with_unit<units::bit>>
    (convert_from_bit_offset_t (old_bits.get_start_bit_offset ()),
     convert_from_bit_size_t (old_bits.m_size_in_bits));
}

inline bit_range
convert_to_bit_range (concrete::range<integer_with_unit<units::bit>> new_bits)
{
  return bit_range
    (convert_to_bit_offset_t (new_bits.get_start_offset ()),
     convert_to_bit_size_t (new_bits.get_size ()));
}

// TODO: eliminate this in favor of concrete_byte_range:
/* A concrete range of bytes.  */

struct byte_range
{
  byte_range (byte_offset_t start_byte_offset, byte_size_t size_in_bytes)
  : m_start_byte_offset (start_byte_offset),
    m_size_in_bytes (size_in_bytes)
  {}

  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;

  std::unique_ptr<json::object> to_json () const;

  bool empty_p () const
  {
    return m_size_in_bytes == 0;
  }

  bool contains_p (byte_offset_t offset) const
  {
    return (offset >= get_start_byte_offset ()
	    && offset < get_next_byte_offset ());
  }
  bool contains_p (const byte_range &other, byte_range *out) const;

  bool operator== (const byte_range &other) const
  {
    return (m_start_byte_offset == other.m_start_byte_offset
	    && m_size_in_bytes == other.m_size_in_bytes);
  }

  byte_offset_t get_start_byte_offset () const
  {
    return m_start_byte_offset;
  }
  byte_offset_t get_next_byte_offset () const
  {
    return m_start_byte_offset + m_size_in_bytes;
  }
  byte_offset_t get_last_byte_offset () const
  {
    gcc_assert (!empty_p ());
    return m_start_byte_offset + m_size_in_bytes - 1;
  }

  bit_range as_bit_range () const
  {
    return bit_range (m_start_byte_offset * BITS_PER_UNIT,
		      m_size_in_bytes * BITS_PER_UNIT);
  }

  bit_offset_t get_start_bit_offset () const
  {
    return m_start_byte_offset * BITS_PER_UNIT;
  }
  bit_offset_t get_next_bit_offset () const
  {
    return get_next_byte_offset () * BITS_PER_UNIT;
  }
  bit_size_t get_size_in_bits () const
  {
    return m_size_in_bytes * BITS_PER_UNIT;
  }

  static int cmp (const byte_range &br1, const byte_range &br2);

  byte_offset_t m_start_byte_offset;
  byte_size_t m_size_in_bytes;
};

/* All of the bindings within a store for regions that share the same
   base region.

   Invariants:
   - the region must be a base region
   - region->tracked_p () must be true
   - region->symbolic_for_unknown_ptr_p () must be false (we must not be
     dereferencing an UNKNOWN ptr).  */

class binding_cluster
{
public:
  friend class store;

  binding_cluster (symbolic::node_manager &node_mgr,
		   const region *base_region,
		   const symbolic::bit_contents &contents);
  binding_cluster (const binding_cluster &other);
  binding_cluster& operator=(const binding_cluster &other);

  bool operator== (const binding_cluster &other) const;
  bool operator!= (const binding_cluster &other) const
  {
    return !(*this == other);
  }

  hashval_t hash () const;

  bool symbolic_p () const;

  const region *get_base_region () const { return m_base_region; }

  void dump () const;

  void validate () const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (symbolic::node_dump_state &nds) const;

  const symbolic::bit_vector &
  get_bits (symbolic_bit_range bit_range) const;

  void
  set_bits (symbolic_bit_range bit_range,
	    const symbolic::bit_vector &bits,
	    uncertainty_t *uncertainty);

  const symbolic::bit_contents &
  get_bit_contents () const { return *m_contents; }

  void
  set_bit_contents (const symbolic::bit_contents &bc)
  {
    m_contents = &bc;
  }

  void mark_as_untracked (symbolic::node_manager &node_mgr,
			  uncertainty_t *uncertainty,
			  value_set *maybe_live_values);
  void purge_state_involving (const symbolic::node &node);

  iter_result
  for_each_value (iter_result (*cb) (const symbolic::value_node &val,
				     void *user_data),
		  void *user_data) const;

  static bool can_merge_p (const binding_cluster *cluster_a,
			   const binding_cluster *cluster_b,
			   binding_cluster *out_cluster,
			   store *out_store,
			   model_merger *merger);
  void make_unknown_relative_to (const binding_cluster *other_cluster,
				 store *out_store,
				 baseline_state bs);

  void mark_as_escaped ();
  void on_unknown_fncall (const gcall &call, 
			  const conjured_purge &p);
  void
  mark_region_as_conjured (const gimple &stmt,
			   const conjured_purge &p);

  void on_asm (const gasm *stmt, 
	       const conjured_purge &p);

  bool escaped_p () const;
  bool touched_p () const { return m_touched; }

  bool redundant_p (const store &store_,
		    baseline_state bs) const;

  void get_representative_path_vars (const region_model *model,
				     value_set *visited,
				     const region *base_reg,
				     const svalue *sval,
				     logger *logger,
				     auto_vec<path_var> *out_pvs) const;

  void
  poison_any_pointers_to_descendents (const region *reg,
				      enum poison_kind pkind);

  bool
  bits_live_p (const symbolic::bit_vector_from_ubc &bv_from_ubc) const;

private:
  symbolic::node_manager &m_node_mgr;

  const region *m_base_region;

  const symbolic::bit_contents *m_contents;

  /* Has a pointer to this cluster "escaped" into a part of the program
     we don't know about (via a call to a function with an unknown body,
     or by being passed in as a pointer param of a "top-level" function call).
     Such regions could be overwritten when other such functions are called,
     even if the region is no longer reachable by pointers that we are
     tracking. */
  bool m_escaped;

  /* Has this cluster been written to via a symbolic binding?
     If so, then we don't know anything about unbound subregions,
     so we can't use initial_svalue, treat them as uninitialized, or
     inherit values from a parent region.  */
  bool m_touched;
};

/* The mapping from regions to svalues.
   This is actually expressed by subdividing into clusters, to better
   handle aliasing.  */

class store
{
public:
  typedef hash_map <const region *, binding_cluster *> cluster_map_t;

  store (symbolic::node_manager &node_mgr);
  store (const store &other);
  ~store ();

  store &operator= (const store &other);

  bool operator== (const store &other) const;
  bool operator!= (const store &other) const
  {
    return !(*this == other);
  }

  hashval_t hash () const;

  void dump () const;
  void summarize_to_pp (pretty_printer *pp, bool simple) const;

  void validate () const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (symbolic::node_dump_state &nds) const;

  const symbolic::bit_vector &
  get_bits (const symbolic::memory_range &mem_range,
	    baseline_state bs) const;

  const symbolic::bit_contents &
  get_bit_contents (const region &base_reg,
		    baseline_state bs) const;

  bool called_unknown_fn_p () const { return m_called_unknown_fn; }

  void
  set_bits (const region *lhs_reg,
	    const symbolic::bit_vector &bv,
	    uncertainty_t *uncertainty,
	    const region_model &model);

  void
  set_bits (const symbolic::memory_range &mem_range,
	    const symbolic::bit_vector &bv,
	    uncertainty_t *uncertainty,
	    const region_model &model);

  void
  set_bit_contents_raw (const region &base_reg,
			const symbolic::bit_contents &bc,
			const region_model &model);

  void clobber_region (const region *reg,
		       baseline_state bs,
		       region_model_manager &rmm);
  void purge_region (const region *reg);
  void mark_region_as_unknown (const region *reg,
			       baseline_state bs,
			       uncertainty_t *uncertainty,
			       const region_model &model);
  void purge_state_involving (const symbolic::node &node);

  const binding_cluster *get_cluster (const region *base_reg) const;
  binding_cluster *get_cluster (const region *base_reg);
  binding_cluster *
  get_or_create_cluster (const region *base_reg,
			 baseline_state bs);
  void purge_cluster (const region *base_reg);

  template <typename T>
  void for_each_cluster (void (*cb) (const region *base_reg, T user_data),
			 T user_data) const
  {
    for (cluster_map_t::iterator iter = m_cluster_map.begin ();
	 iter != m_cluster_map.end (); ++iter)
      cb ((*iter).first, user_data);
  }

  static bool can_merge_p (const store *store_a, const store *store_b,
			   store *out_store, 
			   model_merger *merger);

  void mark_as_escaped (const region *base_reg,
			baseline_state bs);
  void on_unknown_fncall (const gcall &call, 
			  const conjured_purge &p);
  void
  mark_region_as_conjured (const region &base_reg,
			   baseline_state bs,
			   const gimple &stmt,
			   const conjured_purge &p);

  bool escaped_p (const region *reg) const;

  void get_representative_path_vars (const region_model *model,
				     value_set *visited,
				     const svalue *sval,
				     logger *logger,
				     auto_vec<path_var> *out_pvs) const;

  cluster_map_t::iterator begin () const { return m_cluster_map.begin (); }
  cluster_map_t::iterator end () const { return m_cluster_map.end (); }

  tristate eval_alias (const region *base_reg_a,
		       const region *base_reg_b,
		       const region_model &model) const;

  void canonicalize ();
  void loop_replay_fixup (const store *other_store,
			  baseline_state bs,
			  region_model_manager *mgr);

  bool replay_call_summary (call_summary_replay &r,
			    const store &summary);
  bool replay_call_summary_cluster (call_summary_replay &r,
				    const store &summary,
				    const region *base_reg);
  bool
  replay_call_summary_bit_contents (call_summary_replay &r,
				    const store &summary,
				    const region &summary_base_reg,
				    const region &caller_dest_reg,
				    const symbolic::bit_contents &caller_bc);

  void on_maybe_live_values (const value_set &maybe_live_values,
			     baseline_state bs);

  bool
  bits_live_p (const symbolic::bit_vector_from_ubc &bv_from_ubc,
	       const region_model &model) const;

  const symbolic::bit_contents &
  get_implicit_contents (const region &base_reg,
			 baseline_state bs) const;

private:
  const symbolic::bit_contents &
  get_baseline_contents (const region &base_reg,
			 baseline_state bs) const;

  tristate eval_alias_1 (const region *base_reg_a,
			 const region *base_reg_b,
			 const region_model &model) const;

  symbolic::node_manager &m_node_mgr;

  cluster_map_t m_cluster_map;

  /* If this is true, then unknown code has been called since the beginning
     of some path reaching this point in the analysis.  All externally
     visible non-readonly global variables that aren't currently modelled by
     the store are affected by this flag: they are implicitly either all in their
     "initial state" (m_called_unknown_fn==false), or all in unknown state
     (m_called_unknown_fn==true).

     This is to avoid having to mark (and thus explicitly track)
     every global when an unknown function is called; instead, they
     can be tracked implicitly.  */
  bool m_called_unknown_fn;
};

} // namespace ana

#endif /* GCC_ANALYZER_STORE_H */
