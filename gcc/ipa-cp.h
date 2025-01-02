/* Interprocedural constant propagation
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef IPA_CP_H
#define IPA_CP_H

template <typename valtype> class ipcp_value;

/* Describes a particular source for an IPA-CP value.  */

template <typename valtype>
struct ipcp_value_source
{
public:
  /* Aggregate offset of the source, negative if the source is scalar value of
     the argument itself.  */
  HOST_WIDE_INT offset;
  /* The incoming edge that brought the value.  */
  cgraph_edge *cs;
  /* If the jump function that resulted into his value was a pass-through or an
     ancestor, this is the ipcp_value of the caller from which the described
     value has been derived.  Otherwise it is NULL.  */
  ipcp_value<valtype> *val;
  /* Next pointer in a linked list of sources of a value.  */
  ipcp_value_source *next;
  /* If the jump function that resulted into his value was a pass-through or an
     ancestor, this is the index of the parameter of the caller the jump
     function references.  */
  int index;
};

/* Common ancestor for all ipcp_value instantiations.  */

class ipcp_value_base
{
public:
  /* Time benefit and that specializing the function for this value would bring
     about in this function alone.  */
  sreal local_time_benefit = 0;
  /* Time benefit that specializing the function for this value can bring about
     in it's callees.  */
  sreal prop_time_benefit = 0;
  /* Size cost that specializing the function for this value would bring about
     in this function alone.  */
  int local_size_cost = 0;
  /* Size cost that specializing the function for this value can bring about in
     it's callees.  */
  int prop_size_cost = 0;
};

/* Describes one particular value stored in struct ipcp_lattice.  */

template <typename valtype>
class ipcp_value : public ipcp_value_base
{
public:
  /* The actual value for the given parameter.  */
  valtype value;
  /* The list of sources from which this value originates.  */
  ipcp_value_source <valtype> *sources = nullptr;
  /* Next pointers in a linked list of all values in a lattice.  */
  ipcp_value *next = nullptr;
  /* Next pointers in a linked list of values in a strongly connected component
     of values. */
  ipcp_value *scc_next = nullptr;
  /* Next pointers in a linked list of SCCs of values sorted topologically
     according their sources.  */
  ipcp_value  *topo_next = nullptr;
  /* A specialized node created for this value, NULL if none has been (so far)
     created.  */
  cgraph_node *spec_node = nullptr;
  /* Depth first search number and low link for topological sorting of
     values.  */
  int dfs = 0;
  int low_link = 0;
  /* SCC number to identify values which recursively feed into each other.
     Values in the same SCC have the same SCC number.  */
  int scc_no = 0;
  /* Non zero if the value is generated from another value in the same lattice
     for a self-recursive call, the actual number is how many times the
     operation has been performed.  In the unlikely event of the value being
     present in two chains fo self-recursive value generation chains, it is the
     maximum.  */
  unsigned self_recursion_generated_level = 0;
  /* True if this value is currently on the topo-sort stack.  */
  bool on_stack = false;

  void add_source (cgraph_edge *cs, ipcp_value *src_val, int src_idx,
		   HOST_WIDE_INT offset);

  /* Return true if both THIS value and O feed into each other.  */

  bool same_scc (const ipcp_value<valtype> *o)
  {
    return o->scc_no == scc_no;
  }

/* Return true, if a this value has been generated for a self-recursive call as
   a result of an arithmetic pass-through jump-function acting on a value in
   the same lattice function.  */

  bool self_recursion_generated_p ()
  {
    return self_recursion_generated_level > 0;
  }
};

/* Lattice describing potential values of a formal parameter of a function, or
   a part of an aggregate.  TOP is represented by a lattice with zero values
   and with contains_variable and bottom flags cleared.  BOTTOM is represented
   by a lattice with the bottom flag set.  In that case, values and
   contains_variable flag should be disregarded.  */

template <typename valtype>
struct ipcp_lattice
{
public:
  /* The list of known values and types in this lattice.  Note that values are
     not deallocated if a lattice is set to bottom because there may be value
     sources referencing them.  */
  ipcp_value<valtype> *values = nullptr;
  /* Number of known values and types in this lattice.  */
  int values_count = 0;
  /* The lattice contains a variable component (in addition to values).  */
  bool contains_variable = false;
  /* The value of the lattice is bottom (i.e. variable and unusable for any
     propagation).  */
  bool bottom = false;

  inline bool is_single_const ();
  inline bool set_to_bottom ();
  inline bool set_contains_variable ();
  bool add_value (valtype newval, cgraph_edge *cs,
		  ipcp_value<valtype> *src_val = NULL,
		  int src_idx = 0, HOST_WIDE_INT offset = -1,
		  ipcp_value<valtype> **val_p = NULL,
		  unsigned same_lat_gen_level = 0);
  void print (FILE * f, bool dump_sources, bool dump_benefits);
};

/* Lattice of tree values with an offset to describe a part of an
   aggregate.  */

struct ipcp_agg_lattice : public ipcp_lattice<tree>
{
public:
  /* Offset that is being described by this lattice. */
  HOST_WIDE_INT offset = 0;
  /* Size so that we don't have to re-compute it every time we traverse the
     list.  Must correspond to TYPE_SIZE of all lat values.  */
  HOST_WIDE_INT size = 0;
  /* Next element of the linked list.  */
  struct ipcp_agg_lattice *next = nullptr;
};

/* Lattice of known bits, only capable of holding one value.
   Bitwise constant propagation propagates which bits of a
   value are constant.
   For eg:
   int f(int x)
   {
     return some_op (x);
   }

   int f1(int y)
   {
     if (cond)
      return f (y & 0xff);
     else
      return f (y & 0xf);
   }

   In the above case, the param 'x' will always have all
   the bits (except the bits in lsb) set to 0.
   Hence the mask of 'x' would be 0xff. The mask
   reflects that the bits in lsb are unknown.
   The actual propagated value is given by m_value & ~m_mask.  */

class ipcp_bits_lattice
{
public:
  bool bottom_p () const { return m_lattice_val == IPA_BITS_VARYING; }
  bool top_p () const { return m_lattice_val == IPA_BITS_UNDEFINED; }
  bool constant_p () const { return m_lattice_val == IPA_BITS_CONSTANT; }
  bool set_to_bottom ();
  bool set_to_constant (widest_int, widest_int);
  bool known_nonzero_p () const;

  widest_int get_value () const { return m_value; }
  widest_int get_mask () const { return m_mask; }

  bool meet_with (ipcp_bits_lattice& other, unsigned, signop,
		  enum tree_code, tree, bool);

  bool meet_with (widest_int, widest_int, unsigned);

  void print (FILE *);

private:
  enum { IPA_BITS_UNDEFINED, IPA_BITS_CONSTANT, IPA_BITS_VARYING }
    m_lattice_val = IPA_BITS_UNDEFINED;

  /* Similar to ccp_lattice_t, mask represents which bits of value are constant.
     If a bit in mask is set to 0, then the corresponding bit in
     value is known to be constant.  */
  widest_int m_value, m_mask;

  bool meet_with_1 (widest_int, widest_int, unsigned, bool);
  void get_value_and_mask (tree, widest_int *, widest_int *);
};

/* Lattice of value ranges.  */

class ipcp_vr_lattice
{
public:
  value_range m_vr;

  inline bool bottom_p () const;
  inline bool top_p () const;
  inline bool set_to_bottom ();
  bool meet_with (const vrange &p_vr);
  bool meet_with (const ipcp_vr_lattice &other);
  void init (tree type);
  void print (FILE * f);

private:
  bool meet_with_1 (const vrange &other_vr);
};

inline void
ipcp_vr_lattice::init (tree type)
{
  if (type)
    m_vr.set_type (type);

  // Otherwise m_vr will default to unsupported_range.
}

/* Structure containing lattices for a parameter itself and for pieces of
   aggregates that are passed in the parameter or by a reference in a parameter
   plus some other useful flags.

   Even after construction, m_value_range parts still need to be initialized
   with the type they represent with the init method.  */

class ipcp_param_lattices
{
public:
  /* Lattice describing the value of the parameter itself.  */
  ipcp_lattice<tree> itself;
  /* Lattice describing the polymorphic contexts of a parameter.  */
  ipcp_lattice<ipa_polymorphic_call_context> ctxlat;
  /* Lattices describing aggregate parts.  */
  ipcp_agg_lattice *aggs = nullptr;
  /* Lattice describing known bits.  */
  ipcp_bits_lattice bits_lattice;
  /* Lattice describing value range.  */
  ipcp_vr_lattice m_value_range;
  /* Number of aggregate lattices */
  int aggs_count = 0;
  /* True if aggregate data were passed by reference (as opposed to by
     value).  */
  bool aggs_by_ref = false;
  /* All aggregate lattices contain a variable component (in addition to
     values).  */
  bool aggs_contain_variable = false;
  /* The value of all aggregate lattices is bottom (i.e. variable and unusable
     for any propagation).  */
  bool aggs_bottom = false;

  /* There is a virtual call based on this parameter.  */
  bool virt_call = false;
};

bool values_equal_for_ipcp_p (tree x, tree y);

/* Return TRUE if IPA supports ranges of TYPE.  */

static inline bool
ipa_vr_supported_type_p (tree type)
{
  return irange::supports_p (type) || prange::supports_p (type);
}

class ipa_vr;

bool ipa_vr_operation_and_type_effects (vrange &dst_vr,
					const vrange &src_vr,
					enum tree_code operation,
					tree dst_type, tree src_type);
bool ipa_vr_operation_and_type_effects (vrange &dst_vr,
					const ipa_vr &src_vr,
					enum tree_code operation,
					tree dst_type, tree src_type);



#endif /* IPA_CP_H */
