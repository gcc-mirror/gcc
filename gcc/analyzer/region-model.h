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

#ifndef GCC_ANALYZER_REGION_MODEL_H
#define GCC_ANALYZER_REGION_MODEL_H

/* Implementation of the region-based ternary model described in:
     "A Memory Model for Static Analysis of C Programs"
      (Zhongxing Xu, Ted Kremenek, and Jian Zhang)
     http://lcs.ios.ac.cn/~xuzb/canalyze/memmodel.pdf  */

using namespace ana;

namespace inchash
{
  extern void add_path_var (path_var pv, hash &hstate);
} // namespace inchash

namespace ana {

template <typename T>
class one_way_id_map
{
 public:
  one_way_id_map (int num_ids);
  void put (T src, T dst);
  T get_dst_for_src (T src) const;
  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;
  void update (T *) const;

 private:
  auto_vec<T> m_src_to_dst;
 };

/* class one_way_id_map.  */

/* one_way_id_map's ctor, which populates the map with dummy null values.  */

template <typename T>
inline one_way_id_map<T>::one_way_id_map (int num_svalues)
: m_src_to_dst (num_svalues)
{
  for (int i = 0; i < num_svalues; i++)
    m_src_to_dst.quick_push (T::null ());
}

/* Record that SRC is to be mapped to DST.  */

template <typename T>
inline void
one_way_id_map<T>::put (T src, T dst)
{
  m_src_to_dst[src.as_int ()] = dst;
}

/* Get the new value for SRC within the map.  */

template <typename T>
inline T
one_way_id_map<T>::get_dst_for_src (T src) const
{
  if (src.null_p ())
    return src;
  return m_src_to_dst[src.as_int ()];
}

/* Dump this map to PP.  */

template <typename T>
inline void
one_way_id_map<T>::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "src to dst: {");
  unsigned i;
  T *dst;
  FOR_EACH_VEC_ELT (m_src_to_dst, i, dst)
    {
      if (i > 0)
	pp_string (pp, ", ");
      T src (T::from_int (i));
      src.print (pp);
      pp_string (pp, " -> ");
      dst->print (pp);
    }
  pp_string (pp, "}");
  pp_newline (pp);
}

/* Dump this map to stderr.  */

template <typename T>
DEBUG_FUNCTION inline void
one_way_id_map<T>::dump () const
{
  pretty_printer pp;
  pp.buffer->stream = stderr;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Update *ID from the old value to its new value in this map.  */

template <typename T>
inline void
one_way_id_map<T>::update (T *id) const
{
  *id = get_dst_for_src (*id);
}

/* Various operations delete information from a region_model.

   This struct tracks how many of each kind of entity were purged (e.g.
   for selftests, and for debugging).  */

struct purge_stats
{
  purge_stats ()
  : m_num_svalues (0),
    m_num_regions (0),
    m_num_equiv_classes (0),
    m_num_constraints (0),
    m_num_client_items (0)
  {}

  int m_num_svalues;
  int m_num_regions;
  int m_num_equiv_classes;
  int m_num_constraints;
  int m_num_client_items;
};

/* A measurement of the complexity of an svalue or region, so that
   we can impose bounds on the growth of these tree-like structures
   and thus avoid infinite chains of analysis.  */

struct complexity
{
  complexity (unsigned num_nodes, unsigned max_depth)
  : m_num_nodes (num_nodes), m_max_depth (max_depth)
  {}

  complexity (const region *reg);
  complexity (const svalue *sval);
  static complexity from_pair (const complexity &c1, const complexity &c);

  /* The total number of svalues and regions in the tree of this
     entity, including the entity itself.  */
  unsigned m_num_nodes;

  /* The maximum depth of the tree of this entity, including the
     entity itself.  */
  unsigned m_max_depth;
};

/* A base class for visiting regions and svalues, with do-nothing
   base implementations of the per-subclass vfuncs.  */

class visitor
{
public:
  virtual void visit_region_svalue (const region_svalue *) {}
  virtual void visit_constant_svalue (const constant_svalue *) {}
  virtual void visit_unknown_svalue (const unknown_svalue *) {}
  virtual void visit_poisoned_svalue (const poisoned_svalue *) {}
  virtual void visit_setjmp_svalue (const setjmp_svalue *) {}
  virtual void visit_initial_svalue (const initial_svalue *) {}
  virtual void visit_unaryop_svalue (const unaryop_svalue *) {}
  virtual void visit_binop_svalue (const binop_svalue *) {}
  virtual void visit_sub_svalue (const sub_svalue *) {}
  virtual void visit_unmergeable_svalue (const unmergeable_svalue *) {}
  virtual void visit_placeholder_svalue (const placeholder_svalue *) {}
  virtual void visit_widening_svalue (const widening_svalue *) {}
  virtual void visit_compound_svalue (const compound_svalue *) {}
  virtual void visit_conjured_svalue (const conjured_svalue *) {}

  virtual void visit_region (const region *) {}
};

/* An enum for discriminating between the different concrete subclasses
   of svalue.  */

enum svalue_kind
{
  SK_REGION,
  SK_CONSTANT,
  SK_UNKNOWN,
  SK_POISONED,
  SK_SETJMP,
  SK_INITIAL,
  SK_UNARYOP,
  SK_BINOP,
  SK_SUB,
  SK_UNMERGEABLE,
  SK_PLACEHOLDER,
  SK_WIDENING,
  SK_COMPOUND,
  SK_CONJURED
};

/* svalue and its subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with svalue_kinds shown for the concrete subclasses):

   svalue
     region_svalue (SK_REGION): a pointer to a region
     constant_svalue (SK_CONSTANT): a constant
     unknown_svalue (SK_UNKNOWN): an unknowable value
     poisoned_svalue (SK_POISONED): a unusable value (undefined)
     setjmp_svalue (SK_SETJMP): a setjmp/longjmp buffer
     initial_svalue (SK_INITIAL): the initial value of a region
     unaryop_svalue (SK_UNARYOP): unary operation on another svalue
     binop_svalue (SK_BINOP): binary operation on two svalues
     sub_svalue (SK_SUB): the result of accessing a subregion
     unmergeable_svalue (SK_UNMERGEABLE): a value that is so interesting
       from a control-flow perspective that it can inhibit state-merging
     placeholder_svalue (SK_PLACEHOLDER): for use in selftests.
     widening_svalue (SK_WIDENING): a merger of two svalues (possibly
       in an iteration).
     compound_svalue (SK_COMPOUND): a mapping of bit-ranges to svalues
     conjured_svalue (SK_CONJURED): a value arising from a stmt.  */

/* An abstract base class representing a value held by a region of memory.  */

class svalue
{
public:
  virtual ~svalue () {}

  tree get_type () const { return m_type; }

  virtual enum svalue_kind get_kind () const = 0;

  void print (const region_model &model,
	      pretty_printer *pp) const;

  virtual void dump_to_pp (pretty_printer *pp, bool simple) const = 0;
  void dump (bool simple=true) const;
  label_text get_desc (bool simple=true) const;

  virtual const region_svalue *
  dyn_cast_region_svalue () const { return NULL; }
  virtual const constant_svalue *
  dyn_cast_constant_svalue () const { return NULL; }
  virtual const poisoned_svalue *
  dyn_cast_poisoned_svalue () const { return NULL; }
  virtual const setjmp_svalue *
  dyn_cast_setjmp_svalue () const { return NULL; }
  virtual const initial_svalue *
  dyn_cast_initial_svalue () const { return NULL; }
  virtual const unaryop_svalue *
  dyn_cast_unaryop_svalue () const { return NULL; }
  virtual const binop_svalue *
  dyn_cast_binop_svalue () const { return NULL; }
  virtual const sub_svalue *
  dyn_cast_sub_svalue () const { return NULL; }
  virtual const unmergeable_svalue *
  dyn_cast_unmergeable_svalue () const { return NULL; }
  virtual const widening_svalue *
  dyn_cast_widening_svalue () const { return NULL; }
  virtual const compound_svalue *
  dyn_cast_compound_svalue () const { return NULL; }
  virtual const conjured_svalue *
  dyn_cast_conjured_svalue () const { return NULL; }

  tree maybe_get_constant () const;
  const svalue *maybe_undo_cast () const;
  const svalue *unwrap_any_unmergeable () const;

  const svalue *can_merge_p (const svalue *other,
			      region_model_manager *mgr,
			      model_merger *merger) const;

  const complexity &get_complexity () const { return m_complexity; }

  virtual void accept (visitor *v) const  = 0;

  bool live_p (const svalue_set &live_svalues,
	       const region_model *model) const;
  virtual bool implicitly_live_p (const svalue_set &live_svalues,
				  const region_model *model) const;

 protected:
  svalue (complexity c, tree type)
  : m_complexity (c), m_type (type)
  {}

 private:
  complexity m_complexity;
  tree m_type;
};

/* Concrete subclass of svalue representing a pointer value that points to
   a known region  */

class region_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of region_svalue.  */
  struct key_t
  {
    key_t (tree type, const region *reg)
    : m_type (type), m_reg (reg)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_type);
      hstate.add_ptr (m_reg);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type && m_reg == other.m_reg);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    tree m_type;
    const region *m_reg;
  };

  region_svalue (tree type, const region *reg)
  : svalue (complexity (reg), type),
    m_reg (reg)
  {
    gcc_assert (m_reg != NULL);
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_REGION; }
  const region_svalue *
  dyn_cast_region_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

  const region * get_pointee () const { return m_reg; }

  static tristate eval_condition (const region_svalue *lhs_ptr,
				  enum tree_code op,
				  const region_svalue *rhs_ptr);

 private:
  const region *m_reg;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const region_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_REGION;
}

template <> struct default_hash_traits<region_svalue::key_t>
: public member_function_hash_traits<region_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete subclass of svalue representing a specific constant value.  */

class constant_svalue : public svalue
{
public:
  constant_svalue (tree cst_expr)
  : svalue (complexity (1, 1), TREE_TYPE (cst_expr)), m_cst_expr (cst_expr)
  {
    gcc_assert (cst_expr);
    gcc_assert (CONSTANT_CLASS_P (cst_expr));
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_CONSTANT; }
  const constant_svalue *
  dyn_cast_constant_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
  bool implicitly_live_p (const svalue_set &,
			  const region_model *) const FINAL OVERRIDE;

  tree get_constant () const { return m_cst_expr; }
  static tristate eval_condition (const constant_svalue *lhs,
				  enum tree_code op,
				  const constant_svalue *rhs);

 private:
  tree m_cst_expr;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const constant_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_CONSTANT;
}

namespace ana {

/* Concrete subclass of svalue representing an unknowable value, the bottom
   value when thinking of svalues as a lattice.
   This is a singleton (w.r.t. its manager): there is a single unknown_svalue
   per type.  Self-comparisons of such instances yield "unknown".  */

class unknown_svalue : public svalue
{
public:
  unknown_svalue (tree type)
  : svalue (complexity (1, 1), type)
  {}

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_UNKNOWN; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
};

/* An enum describing a particular kind of "poisoned" value.  */

enum poison_kind
{
  /* For use to describe freed memory.  */
  POISON_KIND_FREED,

  /* For use on pointers to regions within popped stack frames.  */
  POISON_KIND_POPPED_STACK
};

extern const char *poison_kind_to_str (enum poison_kind);

/* Concrete subclass of svalue representing a value that should not
   be used (e.g. uninitialized memory, freed memory).  */

class poisoned_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of poisoned_svalue.  */
  struct key_t
  {
    key_t (enum poison_kind kind, tree type)
    : m_kind (kind), m_type (type)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_int (m_kind);
      hstate.add_ptr (m_type);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_kind == other.m_kind && m_type == other.m_type);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    enum poison_kind m_kind;
    tree m_type;
  };

  poisoned_svalue (enum poison_kind kind, tree type)
  : svalue (complexity (1, 1), type), m_kind (kind) {}

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_POISONED; }
  const poisoned_svalue *
  dyn_cast_poisoned_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

  enum poison_kind get_poison_kind () const { return m_kind; }

 private:
  enum poison_kind m_kind;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const poisoned_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_POISONED;
}

template <> struct default_hash_traits<poisoned_svalue::key_t>
: public member_function_hash_traits<poisoned_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A bundle of information recording a setjmp/sigsetjmp call, corresponding
   roughly to a jmp_buf.  */

struct setjmp_record
{
  setjmp_record (const exploded_node *enode,
		 const gcall *setjmp_call)
  : m_enode (enode), m_setjmp_call (setjmp_call)
  {
  }

  bool operator== (const setjmp_record &other) const
  {
    return (m_enode == other.m_enode
	    && m_setjmp_call == other.m_setjmp_call);
  }

  void add_to_hash (inchash::hash *hstate) const
  {
    hstate->add_ptr (m_enode);
    hstate->add_ptr (m_setjmp_call);
  }

  const exploded_node *m_enode;
  const gcall *m_setjmp_call;
};

/* Concrete subclass of svalue representing buffers for setjmp/sigsetjmp,
   so that longjmp/siglongjmp can potentially "return" to an entirely
   different function.  */

class setjmp_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of poisoned_svalue.  */
  struct key_t
  {
    key_t (const setjmp_record &record, tree type)
    : m_record (record), m_type (type)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      m_record.add_to_hash (&hstate);
      hstate.add_ptr (m_type);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_record == other.m_record && m_type == other.m_type);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    setjmp_record m_record;
    tree m_type;
  };

  setjmp_svalue (const setjmp_record &setjmp_record,
		  tree type)
  : svalue (complexity (1, 1), type), m_setjmp_record (setjmp_record)
  {}

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_SETJMP; }
  const setjmp_svalue *
  dyn_cast_setjmp_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

  int get_enode_index () const;

  const setjmp_record &get_setjmp_record () const { return m_setjmp_record; }

 private:
  setjmp_record m_setjmp_record;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const setjmp_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_SETJMP;
}

template <> struct default_hash_traits<setjmp_svalue::key_t>
: public member_function_hash_traits<setjmp_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete subclass of svalue representing the initial value of a
   specific region.

   This represents the initial value at the start of the analysis path,
   as opposed to the first time the region is accessed during the path.
   Hence as soon as we have a call to an unknown function, all previously
   unmodelled globals become implicitly "unknown" rathen than "initial".  */

class initial_svalue : public svalue
{
public:
  initial_svalue (tree type, const region *reg)
  : svalue (complexity (reg), type), m_reg (reg)
  {
    gcc_assert (m_reg != NULL);
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_INITIAL; }
  const initial_svalue *
  dyn_cast_initial_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
  bool implicitly_live_p (const svalue_set &,
			  const region_model *) const FINAL OVERRIDE;

  const region *get_region () const { return m_reg; }

 private:
  const region *m_reg;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const initial_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_INITIAL;
}

namespace ana {

/* Concrete subclass of svalue representing a unary operation on
   another svalues (e.g. a cast).  */

class unaryop_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of unaryop_svalue.  */
  struct key_t
  {
    key_t (tree type, enum tree_code op, const svalue *arg)
    : m_type (type), m_op (op), m_arg (arg)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_type);
      hstate.add_int (m_op);
      hstate.add_ptr (m_arg);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type
	      && m_op == other.m_op
	      && m_arg == other.m_arg);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    tree m_type;
    enum tree_code m_op;
    const svalue *m_arg;
  };

  unaryop_svalue (tree type, enum tree_code op, const svalue *arg)
  : svalue (complexity (arg), type), m_op (op), m_arg (arg)
  {
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_UNARYOP; }
  const unaryop_svalue *
  dyn_cast_unaryop_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
  bool implicitly_live_p (const svalue_set &,
			  const region_model *) const FINAL OVERRIDE;

  enum tree_code get_op () const { return m_op; }
  const svalue *get_arg () const { return m_arg; }

 private:
  enum tree_code m_op;
  const svalue *m_arg;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const unaryop_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_UNARYOP;
}

template <> struct default_hash_traits<unaryop_svalue::key_t>
: public member_function_hash_traits<unaryop_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete subclass of svalue representing a binary operation of
   two svalues.  */

class binop_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of binop_svalue.  */
  struct key_t
  {
    key_t (tree type, enum tree_code op,
	   const svalue *arg0, const svalue *arg1)
    : m_type (type), m_op (op), m_arg0 (arg0), m_arg1 (arg1)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_type);
      hstate.add_int (m_op);
      hstate.add_ptr (m_arg0);
      hstate.add_ptr (m_arg1);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type
	      && m_op == other.m_op
	      && m_arg0 == other.m_arg0
	      && m_arg1 == other.m_arg1);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    tree m_type;
    enum tree_code m_op;
    const svalue *m_arg0;
    const svalue *m_arg1;
  };

  binop_svalue (tree type, enum tree_code op,
		 const svalue *arg0, const svalue *arg1)
  : svalue (complexity::from_pair (arg0->get_complexity (),
				    arg1->get_complexity ()),
	     type),
    m_op (op), m_arg0 (arg0), m_arg1 (arg1)
  {
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_BINOP; }
  virtual const binop_svalue *dyn_cast_binop_svalue () const { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
  bool implicitly_live_p (const svalue_set &,
			  const region_model *) const FINAL OVERRIDE;

  enum tree_code get_op () const { return m_op; }
  const svalue *get_arg0 () const { return m_arg0; }
  const svalue *get_arg1 () const { return m_arg1; }

 private:
  enum tree_code m_op;
  const svalue *m_arg0;
  const svalue *m_arg1;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const binop_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_BINOP;
}

template <> struct default_hash_traits<binop_svalue::key_t>
: public member_function_hash_traits<binop_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete subclass of svalue representing the result of accessing a subregion
   of another svalue (the value of a component/field of a struct, or an element
   from an array).  */

class sub_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of sub_svalue.  */
  struct key_t
  {
    key_t (tree type, const svalue *parent_svalue, const region *subregion)
    : m_type (type), m_parent_svalue (parent_svalue), m_subregion (subregion)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_type);
      hstate.add_ptr (m_parent_svalue);
      hstate.add_ptr (m_subregion);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type
	      && m_parent_svalue == other.m_parent_svalue
	      && m_subregion == other.m_subregion);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    tree m_type;
    const svalue *m_parent_svalue;
    const region *m_subregion;
  };
  sub_svalue (tree type, const svalue *parent_svalue,
	       const region *subregion);

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_SUB; }
  const sub_svalue *dyn_cast_sub_svalue () const FINAL OVERRIDE
  {
    return this;
  }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
  bool implicitly_live_p (const svalue_set &,
			  const region_model *) const FINAL OVERRIDE;

  const svalue *get_parent () const { return m_parent_svalue; }
  const region *get_subregion () const { return m_subregion; }

 private:
  const svalue *m_parent_svalue;
  const region *m_subregion;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const sub_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_SUB;
}

template <> struct default_hash_traits<sub_svalue::key_t>
: public member_function_hash_traits<sub_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete subclass of svalue: decorate another svalue,
   so that the resulting svalue can be identified as being
   "interesting to control flow".
   For example, consider the return value from setjmp.  We
   don't want to merge states in which the result is 0 with
   those in which the result is non-zero.  By using an
   unmergeable_svalue for the result, we can inhibit such merges
   and have separate exploded nodes for those states, keeping
   the first and second returns from setjmp distinct in the exploded
   graph.  */

class unmergeable_svalue : public svalue
{
public:
  unmergeable_svalue (const svalue *arg)
  : svalue (complexity (arg), arg->get_type ()), m_arg (arg)
  {
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_UNMERGEABLE; }
  const unmergeable_svalue *
  dyn_cast_unmergeable_svalue () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;
  bool implicitly_live_p (const svalue_set &,
			  const region_model *) const FINAL OVERRIDE;

  const svalue *get_arg () const { return m_arg; }

 private:
  const svalue *m_arg;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const unmergeable_svalue *>::test (const svalue *sval)
{
  return sval->get_kind () == SK_UNMERGEABLE;
}

namespace ana {

/* Concrete subclass of svalue for use in selftests, where
   we want a specific but unknown svalue.
   Unlike other svalue subclasses these aren't managed by
   region_model_manager.  */

class placeholder_svalue : public svalue
{
public:
  placeholder_svalue (tree type, const char *name)
  : svalue (complexity (1, 1), type), m_name (name)
  {
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_PLACEHOLDER; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

 private:
  const char *m_name;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <placeholder_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_PLACEHOLDER;
}

namespace ana {

/* Concrete subclass of svalue representing a "widening" seen when merging
   states, widening from a base value to {base value, iter value} and thus
   representing a possible fixed point in an iteration from the base to
   +ve infinity, or -ve infinity, and thus useful for representing a value
   within a loop.
   We also need to capture the program_point at which the merger happens,
   so that distinguish between different iterators, and thus handle
   nested loops.  (currently we capture the function_point instead, for
   simplicity of hashing).  */

class widening_svalue : public svalue
{
public:
  /* A support class for uniquifying instances of widening_svalue.  */
  struct key_t
  {
    key_t (tree type, const program_point &point,
	   const svalue *base_sval, const svalue *iter_sval)
    : m_type (type), m_point (point.get_function_point ()),
      m_base_sval (base_sval), m_iter_sval (iter_sval)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_base_sval);
      hstate.add_ptr (m_iter_sval);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type
	      && m_point == other.m_point
	      && m_base_sval == other.m_base_sval
	      && m_iter_sval == other.m_iter_sval);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    tree m_type;
    function_point m_point;
    const svalue *m_base_sval;
    const svalue *m_iter_sval;
  };

  enum direction_t
    {
     DIR_ASCENDING,
     DIR_DESCENDING,
     DIR_UNKNOWN
    };

  widening_svalue (tree type, const program_point &point,
		   const svalue *base_sval, const svalue *iter_sval)
  : svalue (complexity::from_pair (base_sval->get_complexity (),
				   iter_sval->get_complexity ()),
	    type),
    m_point (point.get_function_point ()),
    m_base_sval (base_sval), m_iter_sval (iter_sval)
  {
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_WIDENING; }
  const widening_svalue *dyn_cast_widening_svalue () const { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

  const svalue *get_base_svalue () const { return m_base_sval; }
  const svalue *get_iter_svalue () const { return m_iter_sval; }

  enum direction_t get_direction () const;

  tristate eval_condition_without_cm (enum tree_code op,
				      tree rhs_cst) const;

 private:
  function_point m_point;
  const svalue *m_base_sval;
  const svalue *m_iter_sval;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <widening_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_WIDENING;
}

template <> struct default_hash_traits<widening_svalue::key_t>
: public member_function_hash_traits<widening_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete subclass of svalue representing a mapping of bit-ranges
   to svalues, analogous to a cluster within the store.

   This is for use in places where we want to represent a store-like
   mapping, but are required to use an svalue, such as when handling
   compound assignments and compound return values.

   All keys within the underlying binding_map are required to be concrete,
   not symbolic.

   Instances of this class shouldn't be bound as-is into the store;
   instead they should be unpacked.  Similarly, they should not be
   nested.  */

class compound_svalue : public svalue
{
public:
  typedef binding_map::iterator_t iterator_t;

  /* A support class for uniquifying instances of compound_svalue.
     Note that to avoid copies, keys store pointers to binding_maps,
     rather than the maps themselves.  */
  struct key_t
  {
    key_t (tree type, const binding_map *map_ptr)
    : m_type (type), m_map_ptr (map_ptr)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_type);
      //hstate.add_ptr (m_map_ptr); // TODO
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type
	      && *m_map_ptr == *other.m_map_ptr);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    tree m_type;
    const binding_map *m_map_ptr;
  };

  compound_svalue (tree type, const binding_map &map);

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_COMPOUND; }
  const compound_svalue *dyn_cast_compound_svalue () const { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

  iterator_t begin () const { return m_map.begin (); }
  iterator_t end () const { return m_map.end (); }

  struct key_t make_key () const
  {
    return key_t (get_type (), &m_map);
  }

 private:
  static complexity calc_complexity (const binding_map &map);

  binding_map m_map;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <compound_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_COMPOUND;
}

template <> struct default_hash_traits<compound_svalue::key_t>
: public member_function_hash_traits<compound_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A defined value arising from a statement, where we want to identify a
   particular unknown value, rather than resorting to the unknown_value
   singleton, so that the value can have sm-state.

   Comparisons of variables that share the same conjured_svalue are known
   to be equal, even if we don't know what the value is.

   For example, this is used for the values of regions that may have been
   touched when calling an unknown function.

   The value captures a region as well as a stmt in order to avoid falsely
   aliasing the various values that could arise in one statement.  For
   example, after:
      unknown_fn (&a, &b);
   we want values to clobber a and b with, but we don't want to use the
   same value, or it would falsely implicitly assume that a == b.  */

class conjured_svalue : public svalue
{
public:
  typedef binding_map::iterator_t iterator_t;

  /* A support class for uniquifying instances of conjured_svalue.  */
  struct key_t
  {
    key_t (tree type, const gimple *stmt, const region *id_reg)
    : m_type (type), m_stmt (stmt), m_id_reg (id_reg)
    {}

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_type);
      hstate.add_ptr (m_stmt);
      hstate.add_ptr (m_id_reg);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_type == other.m_type
	      && m_stmt == other.m_stmt
	      && m_id_reg == other.m_id_reg);
    }

    /* Use m_stmt to mark empty/deleted, as m_type can be NULL for
       legitimate instances.  */
    void mark_deleted () { m_stmt = reinterpret_cast<const gimple *> (1); }
    void mark_empty () { m_stmt = NULL; }
    bool is_deleted () const
    {
      return m_stmt == reinterpret_cast<const gimple *> (1);
    }
    bool is_empty () const { return m_stmt == NULL; }

    tree m_type;
    const gimple *m_stmt;
    const region *m_id_reg;
  };

  conjured_svalue (tree type, const gimple *stmt, const region *id_reg)
  : svalue (complexity (id_reg), type),
    m_stmt (stmt), m_id_reg (id_reg)
  {
    gcc_assert (m_stmt != NULL);
  }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_CONJURED; }
  const conjured_svalue *dyn_cast_conjured_svalue () const { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  void accept (visitor *v) const FINAL OVERRIDE;

 private:
  const gimple *m_stmt;
  const region *m_id_reg;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <conjured_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_CONJURED;
}

template <> struct default_hash_traits<conjured_svalue::key_t>
: public member_function_hash_traits<conjured_svalue::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* An enum for discriminating between the different concrete subclasses
   of region.  */

enum region_kind
{
  RK_FRAME,
  RK_GLOBALS,
  RK_CODE,
  RK_FUNCTION,
  RK_LABEL,
  RK_STACK,
  RK_HEAP,
  RK_ROOT,
  RK_SYMBOLIC,
  RK_DECL,
  RK_FIELD,
  RK_ELEMENT,
  RK_OFFSET,
  RK_CAST,
  RK_HEAP_ALLOCATED,
  RK_ALLOCA,
  RK_STRING,
  RK_UNKNOWN
};

/* Region and its subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with region_kinds shown for the concrete subclasses):

   region
     space_region
       frame_region (RK_FRAME)
       globals_region (RK_GLOBALS)
       code_region (RK_CODE)
       stack_region (RK_STACK)
       heap_region (RK_HEAP)
     root_region (RK_ROOT)
     function_region (RK_FUNCTION)
     label_region (RK_LABEL)
     symbolic_region (RK_SYMBOLIC)
     decl_region (RK_DECL),
     field_region (RK_FIELD)
     element_region (RK_ELEMENT)
     offset_region (RK_OFFSET)
     cast_region (RK_CAST)
     heap_allocated_region (RK_HEAP_ALLOCATED)
     alloca_region (RK_ALLOCA)
     string_region (RK_STRING)
     unknown_region (RK_UNKNOWN).  */

/* Abstract base class for representing ways of accessing chunks of memory.

   Regions form a tree-like hierarchy, with a root region at the base,
   with memory space regions within it, representing the stack and
   globals, with frames within the stack, and regions for variables
   within the frames and the "globals" region.  Regions for structs
   can have subregions for fields.  */

class region
{
public:
  virtual ~region ();

  unsigned get_id () const { return m_id; }
  static int cmp_ids (const region *reg1, const region *reg2);

  virtual enum region_kind get_kind () const = 0;
  virtual const frame_region *
  dyn_cast_frame_region () const { return NULL; }
  virtual const function_region *
  dyn_cast_function_region () const { return NULL; }
  virtual const symbolic_region *
  dyn_cast_symbolic_region () const { return NULL; }
  virtual const decl_region *
  dyn_cast_decl_region () const { return NULL; }
  virtual const field_region *
  dyn_cast_field_region () const { return NULL; }
  virtual const element_region *
  dyn_cast_element_region () const { return NULL; }
  virtual const offset_region *
  dyn_cast_offset_region () const { return NULL; }
  virtual const cast_region *
  dyn_cast_cast_region () const { return NULL; }
  virtual const string_region *
  dyn_cast_string_region () const { return NULL; }

  virtual void accept (visitor *v) const;

  const region *get_parent_region () const { return m_parent; }
  const region *get_base_region () const;
  bool base_region_p () const;
  bool descendent_of_p (const region *elder) const;
  const frame_region *maybe_get_frame_region () const;

  tree maybe_get_decl () const;

  tree get_type () const { return m_type; }

  void print (const region_model &model,
	      pretty_printer *pp) const;
  label_text get_desc (bool simple=true) const;

  void dump_to_pp (const region_model &model,
		   pretty_printer *pp,
		   const char *prefix,
		   bool is_last_child) const;

  virtual void dump_to_pp (pretty_printer *pp, bool simple) const = 0;
  void dump (bool simple) const;

  bool non_null_p () const;

  static int cmp_ptrs (const void *, const void *);

  region_offset get_offset () const;
  bool get_byte_size (byte_size_t *out) const;
  bool get_bit_size (bit_size_t *out) const;

  void
  get_subregions_for_binding (region_model_manager *mgr,
			      bit_offset_t start_bit_offset,
			      bit_size_t size_in_bits,
			      tree type,
			      auto_vec <const region *> *out) const;

  bool symbolic_for_unknown_ptr_p () const;

  const complexity &get_complexity () const { return m_complexity; }

 protected:
  region (complexity c, unsigned id, const region *parent, tree type);

 private:
  region_offset calc_offset () const;

  complexity m_complexity;
  unsigned m_id; // purely for deterministic sorting at this stage, for dumps
  const region *m_parent;
  tree m_type;

  mutable region_offset *m_cached_offset;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const region *>::test (const region *)
{
  return true;
}

namespace ana {

/* Abstract subclass of region, for regions that represent an untyped
   space within memory, such as the stack or the heap.  */

class space_region : public region
{
protected:
  space_region (unsigned id, const region *parent)
  : region (complexity (parent), id, parent, NULL_TREE)
  {}
};

/* Concrete space_region subclass, representing a function frame on the stack,
   to contain the locals.
   The parent is the stack region; there's also a hierarchy of call-stack
   prefixes expressed via m_calling_frame.
   For example, given "oldest" calling "middle" called "newest" we would have
   - a stack depth of 3
   - frame (A) for "oldest" with index 0 for depth 1, calling_frame == NULL
   - frame (B) for "middle" with index 1 for depth 2, calling_frame == (A)
   - frame (C) for "newest" with index 2 for depth 3, calling_frame == (B)
   where the parent region for each of the frames is the "stack" region.
   The index is the count of frames earlier than this in the stack.  */

class frame_region : public space_region
{
public:
  /* A support class for uniquifying instances of frame_region.  */
  struct key_t
  {
    key_t (const frame_region *calling_frame, function *fun)
    : m_calling_frame (calling_frame), m_fun (fun)
    {
      /* calling_frame can be NULL.  */
      gcc_assert (fun);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_calling_frame);
      hstate.add_ptr (m_fun);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_calling_frame == other.m_calling_frame && m_fun == other.m_fun);
    }

    void mark_deleted () { m_fun = reinterpret_cast<function *> (1); }
    void mark_empty () { m_fun = NULL; }
    bool is_deleted () const
    {
      return m_fun == reinterpret_cast<function *> (1);
    }
    bool is_empty () const { return m_fun == NULL; }

    const frame_region *m_calling_frame;
    function *m_fun;
  };

  frame_region (unsigned id, const region *parent,
		const frame_region *calling_frame,
		function *fun, int index)
  : space_region (id, parent), m_calling_frame (calling_frame),
    m_fun (fun), m_index (index)
  {}
  ~frame_region ();

  /* region vfuncs.  */
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_FRAME; }
  const frame_region * dyn_cast_frame_region () const FINAL OVERRIDE
  {
    return this;
  }
  void accept (visitor *v) const FINAL OVERRIDE;
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  /* Accessors.  */
  const frame_region *get_calling_frame () const { return m_calling_frame; }
  function *get_function () const { return m_fun; }
  int get_index () const { return m_index; }
  int get_stack_depth () const { return m_index + 1; }

  const decl_region *get_region_for_local (region_model_manager *mgr,
					   tree expr) const;

  unsigned get_num_locals () const { return m_locals.elements (); }

 private:
  const frame_region *m_calling_frame;
  function *m_fun;
  int m_index;

  /* The regions for the decls within this frame are managed by this
     object, rather than the region_model_manager, to make it a simple
     lookup by tree.  */
  typedef hash_map<tree, decl_region *> map_t;
  map_t m_locals;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const frame_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_FRAME;
}

template <> struct default_hash_traits<frame_region::key_t>
: public member_function_hash_traits<frame_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete space_region subclass, to hold global variables (data and bss).  */

class globals_region : public space_region
{
 public:
  globals_region (unsigned id, const region *parent)
  : space_region (id, parent)
  {}

  /* region vfuncs.  */
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_GLOBALS; }
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const globals_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_GLOBALS;
}

namespace ana {

/* Concrete space_region subclass, representing the code segment
   containing functions.  */

class code_region : public space_region
{
public:
  code_region (unsigned id, const region *parent)
  : space_region (id, parent)
  {}

  /* region vfuncs.  */
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_CODE; }

  const region *get_element (region_model *model,
			const svalue *index,
			region_model_context *ctxt);
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const code_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_CODE;
}

namespace ana {

/* Concrete region subclass.  A region representing the code for
   a particular function.  */

class function_region : public region
{
public:
  function_region (unsigned id, const code_region *parent, tree fndecl)
  : region (complexity (parent), id, parent, TREE_TYPE (fndecl)),
    m_fndecl (fndecl)
  {
    gcc_assert (FUNC_OR_METHOD_TYPE_P (TREE_TYPE (fndecl)));
  }

  /* region vfuncs.  */
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_FUNCTION; }
  const function_region *
  dyn_cast_function_region () const FINAL OVERRIDE{ return this; }

  tree get_fndecl () const { return m_fndecl; }

  region *get_element (region_model *model,
			const svalue *index_sid,
			region_model_context *ctxt);

private:
  tree m_fndecl;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const function_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_FUNCTION;
}

namespace ana {

/* Concrete region subclass.  A region representing a particular label
   within a function.  */

class label_region : public region
{
public:
  label_region (unsigned id, const function_region *parent, tree label)
  : region (complexity (parent), id, parent, NULL_TREE), m_label (label)
  {
    gcc_assert (TREE_CODE (label) == LABEL_DECL);
  }

  /* region vfuncs.  */
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_LABEL; }

private:
  tree m_label;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const label_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_LABEL;
}

namespace ana {

/* Concrete space_region subclass representing a stack, containing all stack
   frames.  */

class stack_region : public space_region
{
public:
  stack_region (unsigned id, region *parent)
  : space_region (id, parent)
  {}

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_STACK; }
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const stack_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_STACK;
}

namespace ana {

/* Concrete space_region subclass: a region within which regions can be
   dynamically allocated.  */

class heap_region : public space_region
{
public:
  heap_region (unsigned id, region *parent)
  : space_region (id, parent)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_HEAP; }
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const heap_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_HEAP;
}

namespace ana {

/* Concrete region subclass.  The root region, containing all regions
   (either directly, or as descendents).
   Unique within a region_model_manager.  */

class root_region : public region
{
public:
  root_region (unsigned id);

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_ROOT; }
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const root_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_ROOT;
}

namespace ana {

/* Concrete region subclass: a region to use when dereferencing an unknown
   pointer.  */

class symbolic_region : public region
{
public:
  /* A support class for uniquifying instances of symbolic_region.  */
  struct key_t
  {
    key_t (const region *parent, const svalue *sval_ptr)
    : m_parent (parent), m_sval_ptr (sval_ptr)
    {
      gcc_assert (sval_ptr);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_sval_ptr);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent && m_sval_ptr == other.m_sval_ptr);
    }

    void mark_deleted () { m_sval_ptr = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_sval_ptr = NULL; }
    bool is_deleted () const
    {
      return m_sval_ptr == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_sval_ptr == NULL; }

    const region *m_parent;
    const svalue *m_sval_ptr;
  };

  symbolic_region (unsigned id, region *parent, const svalue *sval_ptr)
  : region (complexity::from_pair (parent, sval_ptr), id, parent,
	    TREE_TYPE (sval_ptr->get_type ())),
    m_sval_ptr (sval_ptr)
  {}

  const symbolic_region *
  dyn_cast_symbolic_region () const FINAL OVERRIDE { return this; }

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_SYMBOLIC; }
  void accept (visitor *v) const FINAL OVERRIDE;
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  const svalue *get_pointer () const { return m_sval_ptr; }

private:
  const svalue *m_sval_ptr;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const symbolic_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_SYMBOLIC;
}

template <> struct default_hash_traits<symbolic_region::key_t>
: public member_function_hash_traits<symbolic_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete region subclass representing the memory occupied by a
   variable (whether for a global or a local).  */

class decl_region : public region
{
public:
  decl_region (unsigned id, const region *parent, tree decl)
  : region (complexity (parent), id, parent, TREE_TYPE (decl)), m_decl (decl)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_DECL; }
  const decl_region *
  dyn_cast_decl_region () const FINAL OVERRIDE { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  tree get_decl () const { return m_decl; }
  int get_stack_depth () const;

  const svalue *maybe_get_constant_value (region_model_manager *mgr) const;
  const svalue *get_svalue_for_constructor (tree ctor,
					    region_model_manager *mgr) const;
  const svalue *get_svalue_for_initializer (region_model_manager *mgr) const;

private:
  tree m_decl;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const decl_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_DECL;
}

namespace ana {

/* Concrete region subclass representing the memory occupied by a
   field within a struct or union.  */

class field_region : public region
{
public:
  /* A support class for uniquifying instances of field_region.  */
  struct key_t
  {
    key_t (const region *parent, tree field)
    : m_parent (parent), m_field (field)
    {
      gcc_assert (field);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_field);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent && m_field == other.m_field);
    }

    void mark_deleted () { m_field = reinterpret_cast<tree> (1); }
    void mark_empty () { m_field = NULL_TREE; }
    bool is_deleted () const { return m_field == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_field == NULL_TREE; }

    const region *m_parent;
    tree m_field;
  };

  field_region (unsigned id, const region *parent, tree field)
  : region (complexity (parent), id, parent, TREE_TYPE (field)),
    m_field (field)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_FIELD; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
  const field_region *
  dyn_cast_field_region () const FINAL OVERRIDE { return this; }

  tree get_field () const { return m_field; }

private:
  tree m_field;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const field_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_FIELD;
}

template <> struct default_hash_traits<field_region::key_t>
: public member_function_hash_traits<field_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* An element within an array.  */

class element_region : public region
{
public:
  /* A support class for uniquifying instances of element_region.  */
  struct key_t
  {
    key_t (const region *parent, tree element_type, const svalue *index)
    : m_parent (parent), m_element_type (element_type), m_index (index)
    {
      gcc_assert (index);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_element_type);
      hstate.add_ptr (m_index);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_element_type == other.m_element_type
	      && m_index == other.m_index);
    }

    void mark_deleted () { m_index = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_index = NULL; }
    bool is_deleted () const
    {
      return m_index == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_index == NULL; }

    const region *m_parent;
    tree m_element_type;
    const svalue *m_index;
  };

  element_region (unsigned id, const region *parent, tree element_type,
		  const svalue *index)
  : region (complexity::from_pair (parent, index), id, parent, element_type),
    m_index (index)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_ELEMENT; }
  const element_region *
  dyn_cast_element_region () const FINAL OVERRIDE { return this; }

  void accept (visitor *v) const FINAL OVERRIDE;

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  const svalue *get_index () const { return m_index; }

private:
  const svalue *m_index;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const element_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_ELEMENT;
}

template <> struct default_hash_traits<element_region::key_t>
: public member_function_hash_traits<element_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A byte-offset within another region, for handling pointer arithmetic
   as a region.  */

class offset_region : public region
{
public:
  /* A support class for uniquifying instances of offset_region.  */
  struct key_t
  {
    key_t (const region *parent, tree element_type, const svalue *byte_offset)
    : m_parent (parent), m_element_type (element_type), m_byte_offset (byte_offset)
    {
      gcc_assert (byte_offset);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_element_type);
      hstate.add_ptr (m_byte_offset);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_element_type == other.m_element_type
	      && m_byte_offset == other.m_byte_offset);
    }

    void mark_deleted () { m_byte_offset = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_byte_offset = NULL; }
    bool is_deleted () const
    {
      return m_byte_offset == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_byte_offset == NULL; }

    const region *m_parent;
    tree m_element_type;
    const svalue *m_byte_offset;
  };

  offset_region (unsigned id, const region *parent, tree type,
		 const svalue *byte_offset)
  : region (complexity::from_pair (parent, byte_offset), id, parent, type),
    m_byte_offset (byte_offset)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_OFFSET; }
  const offset_region *
  dyn_cast_offset_region () const FINAL OVERRIDE { return this; }

  void accept (visitor *v) const FINAL OVERRIDE;

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  const svalue *get_byte_offset () const { return m_byte_offset; }

private:
  const svalue *m_byte_offset;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const offset_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_OFFSET;
}

template <> struct default_hash_traits<offset_region::key_t>
: public member_function_hash_traits<offset_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A region that views another region using a different type.  */

class cast_region : public region
{
public:
  /* A support class for uniquifying instances of cast_region.  */
  struct key_t
  {
    key_t (const region *original_region, tree type)
    : m_original_region (original_region), m_type (type)
    {
      gcc_assert (type);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_original_region);
      hstate.add_ptr (m_type);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_original_region == other.m_original_region
	      && m_type == other.m_type);
    }

    void mark_deleted () { m_type = reinterpret_cast<tree> (1); }
    void mark_empty () { m_type = NULL_TREE; }
    bool is_deleted () const { return m_type == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_type == NULL_TREE; }

    const region *m_original_region;
    tree m_type;
  };

  cast_region (unsigned id, const region *original_region, tree type)
  : region (complexity (original_region), id,
	    original_region->get_parent_region (), type),
    m_original_region (original_region)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_CAST; }
  const cast_region *
  dyn_cast_cast_region () const FINAL OVERRIDE { return this; }
  void accept (visitor *v) const FINAL OVERRIDE;
  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  const region *get_original_region () const { return m_original_region; }

private:
  const region *m_original_region;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const cast_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_CAST;
}

template <> struct default_hash_traits<cast_region::key_t>
: public member_function_hash_traits<cast_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* An untyped region dynamically allocated on the heap via "malloc"
   or similar.  */

class heap_allocated_region : public region
{
public:
  heap_allocated_region (unsigned id, const region *parent)
  : region (complexity (parent), id, parent, NULL_TREE)
  {}

  enum region_kind
  get_kind () const FINAL OVERRIDE { return RK_HEAP_ALLOCATED; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
};

/* An untyped region dynamically allocated on the stack via "alloca".  */

class alloca_region : public region
{
public:
  alloca_region (unsigned id, const frame_region *parent)
  : region (complexity (parent), id, parent, NULL_TREE)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_ALLOCA; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
};

/* A region for a STRING_CST.  */

class string_region : public region
{
public:
  string_region (unsigned id, const region *parent, tree string_cst)
  : region (complexity (parent), id, parent, TREE_TYPE (string_cst)),
    m_string_cst (string_cst)
  {}

  const string_region *
  dyn_cast_string_region () const FINAL OVERRIDE { return this; }

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_STRING; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;

  tree get_string_cst () const { return m_string_cst; }

private:
  tree m_string_cst;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const string_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_STRING;
}

namespace ana {

/* An unknown region, for handling unimplemented tree codes.  */

class unknown_region : public region
{
public:
  unknown_region (unsigned id, const region *parent, tree type)
  : region (complexity (parent), id, parent, type)
  {}

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_UNKNOWN; }

  void dump_to_pp (pretty_printer *pp, bool simple) const FINAL OVERRIDE;
};

/* A class responsible for owning and consolidating region and svalue
   instances.
   region and svalue instances are immutable as far as clients are
   concerned, so they are provided as "const" ptrs.  */

class region_model_manager
{
public:
  region_model_manager ();
  ~region_model_manager ();

  /* svalue consolidation.  */
  const svalue *get_or_create_constant_svalue (tree cst_expr);
  const svalue *get_or_create_unknown_svalue (tree type);
  const svalue *get_or_create_setjmp_svalue (const setjmp_record &r,
					     tree type);
  const svalue *get_or_create_poisoned_svalue (enum poison_kind kind,
					       tree type);
  const svalue *get_or_create_initial_value (const region *reg);
  const svalue *get_ptr_svalue (tree ptr_type, const region *pointee);
  const svalue *get_or_create_unaryop (tree type, enum tree_code op,
				       const svalue *arg);
  const svalue *get_or_create_cast (tree type, const svalue *arg);
  const svalue *get_or_create_binop (tree type,
				     enum tree_code op,
				     const svalue *arg0, const svalue *arg1);
  const svalue *get_or_create_sub_svalue (tree type,
					  const svalue *parent_svalue,
					  const region *subregion);
  const svalue *get_or_create_unmergeable (const svalue *arg);
  const svalue *get_or_create_widening_svalue (tree type,
					       const program_point &point,
					       const svalue *base_svalue,
					       const svalue *iter_svalue);
  const svalue *get_or_create_compound_svalue (tree type,
					       const binding_map &map);
  const svalue *get_or_create_conjured_svalue (tree type, const gimple *stmt,
					       const region *id_reg);

  const svalue *maybe_get_char_from_string_cst (tree string_cst,
						tree byte_offset_cst);

  /* region consolidation.  */
  const stack_region * get_stack_region () const { return &m_stack_region; }
  const heap_region *get_heap_region () const { return &m_heap_region; }
  const code_region *get_code_region () const { return &m_code_region; }
  const globals_region *get_globals_region () const
  {
    return &m_globals_region;
  }
  const function_region *get_region_for_fndecl (tree fndecl);
  const label_region *get_region_for_label (tree label);
  const decl_region *get_region_for_global (tree expr);
  const region *get_field_region (const region *parent, tree field);
  const region *get_element_region (const region *parent,
				    tree element_type,
				    const svalue *index);
  const region *get_offset_region (const region *parent,
				   tree type,
				   const svalue *byte_offset);
  const region *get_cast_region (const region *original_region,
				 tree type);
  const frame_region *get_frame_region (const frame_region *calling_frame,
					function *fun);
  const region *get_symbolic_region (const svalue *sval);
  const string_region *get_region_for_string (tree string_cst);

  const region *
  get_region_for_unexpected_tree_code (region_model_context *ctxt,
				       tree t,
				       const dump_location_t &loc);

  unsigned alloc_region_id () { return m_next_region_id++; }

  store_manager *get_store_manager () { return &m_store_mgr; }

  /* Dynamically-allocated region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  const region *create_region_for_heap_alloc ();
  const region *create_region_for_alloca (const frame_region *frame);

  void log_stats (logger *logger, bool show_objs) const;

private:
  bool too_complex_p (const complexity &c) const;
  bool reject_if_too_complex (svalue *sval);

  const svalue *maybe_fold_unaryop (tree type, enum tree_code op,
				    const svalue *arg);
  const svalue *maybe_fold_binop (tree type, enum tree_code op,
				  const svalue *arg0, const svalue *arg1);
  const svalue *maybe_fold_sub_svalue (tree type,
				       const svalue *parent_svalue,
				       const region *subregion);

  unsigned m_next_region_id;
  root_region m_root_region;
  stack_region m_stack_region;
  heap_region m_heap_region;

  /* svalue consolidation.  */
  typedef hash_map<tree, constant_svalue *> constants_map_t;
  constants_map_t m_constants_map;

  typedef hash_map<tree, unknown_svalue *> unknowns_map_t;
  unknowns_map_t m_unknowns_map;
  const unknown_svalue *m_unknown_NULL;

  typedef hash_map<poisoned_svalue::key_t,
		   poisoned_svalue *> poisoned_values_map_t;
  poisoned_values_map_t m_poisoned_values_map;

  typedef hash_map<setjmp_svalue::key_t,
		   setjmp_svalue *> setjmp_values_map_t;
  setjmp_values_map_t m_setjmp_values_map;

  typedef hash_map<const region *, initial_svalue *> initial_values_map_t;
  initial_values_map_t m_initial_values_map;

  typedef hash_map<region_svalue::key_t, region_svalue *> pointer_values_map_t;
  pointer_values_map_t m_pointer_values_map;

  typedef hash_map<unaryop_svalue::key_t,
		   unaryop_svalue *> unaryop_values_map_t;
  unaryop_values_map_t m_unaryop_values_map;

  typedef hash_map<binop_svalue::key_t, binop_svalue *> binop_values_map_t;
  binop_values_map_t m_binop_values_map;

  typedef hash_map<sub_svalue::key_t, sub_svalue *> sub_values_map_t;
  sub_values_map_t m_sub_values_map;

  typedef hash_map<const svalue *,
		   unmergeable_svalue *> unmergeable_values_map_t;
  unmergeable_values_map_t m_unmergeable_values_map;

  typedef hash_map<widening_svalue::key_t,
		   widening_svalue */*,
		   widening_svalue::key_t::hash_map_traits*/>
    widening_values_map_t;
  widening_values_map_t m_widening_values_map;

  typedef hash_map<compound_svalue::key_t,
		   compound_svalue *> compound_values_map_t;
  compound_values_map_t m_compound_values_map;

  typedef hash_map<conjured_svalue::key_t,
		   conjured_svalue *> conjured_values_map_t;
  conjured_values_map_t m_conjured_values_map;

  /* Maximum complexity of svalues that weren't rejected.  */
  complexity m_max_complexity;

  /* region consolidation.  */

  code_region m_code_region;
  typedef hash_map<tree, function_region *> fndecls_map_t;
  typedef fndecls_map_t::iterator fndecls_iterator_t;
  fndecls_map_t m_fndecls_map;

  typedef hash_map<tree, label_region *> labels_map_t;
  typedef labels_map_t::iterator labels_iterator_t;
  labels_map_t m_labels_map;

  globals_region m_globals_region;
  typedef hash_map<tree, decl_region *> globals_map_t;
  typedef globals_map_t::iterator globals_iterator_t;
  globals_map_t m_globals_map;

  consolidation_map<field_region> m_field_regions;
  consolidation_map<element_region> m_element_regions;
  consolidation_map<offset_region> m_offset_regions;
  consolidation_map<cast_region> m_cast_regions;
  consolidation_map<frame_region> m_frame_regions;
  consolidation_map<symbolic_region> m_symbolic_regions;

  typedef hash_map<tree, string_region *> string_map_t;
  string_map_t m_string_map;

  store_manager m_store_mgr;

  /* "Dynamically-allocated" region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  auto_delete_vec<region> m_managed_dynamic_regions;
};

struct append_ssa_names_cb_data;

/* Helper class for handling calls to functions with known behavior.
   Implemented in region-model-impl-calls.c.  */

class call_details
{
public:
  call_details (const gcall *call, region_model *model,
		region_model_context *ctxt);

  region_model_context *get_ctxt () const { return m_ctxt; }
  tree get_lhs_type () const { return m_lhs_type; }
  const region *get_lhs_region () const { return m_lhs_region; }

  bool maybe_set_lhs (const svalue *result) const;

  tree get_arg_tree (unsigned idx) const;
  const svalue *get_arg_svalue (unsigned idx) const;

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (bool simple) const;

private:
  const gcall *m_call;
  region_model *m_model;
  region_model_context *m_ctxt;
  tree m_lhs_type;
  const region *m_lhs_region;
};

/* A region_model encapsulates a representation of the state of memory, with
   a tree of regions, along with their associated values.
   The representation is graph-like because values can be pointers to
   regions.
   It also stores a constraint_manager, capturing relationships between
   the values.  */

class region_model
{
 public:
  region_model (region_model_manager *mgr);
  region_model (const region_model &other);
  ~region_model ();

#if 0//__cplusplus >= 201103
  region_model (region_model &&other);
#endif

  region_model &operator= (const region_model &other);

  bool operator== (const region_model &other) const;
  bool operator!= (const region_model &other) const
  {
    return !(*this == other);
  }

  hashval_t hash () const;

  void print (pretty_printer *pp) const;

  void dump_to_pp (pretty_printer *pp, bool simple, bool multiline) const;
  void dump (FILE *fp, bool simple, bool multiline) const;
  void dump (bool simple) const;

  void debug () const;

  void validate () const;

  void canonicalize ();
  bool canonicalized_p () const;

  void on_assignment (const gassign *stmt, region_model_context *ctxt);
  const svalue *get_gassign_result (const gassign *assign,
				    region_model_context *ctxt);
  bool on_call_pre (const gcall *stmt, region_model_context *ctxt);
  void on_call_post (const gcall *stmt,
		     bool unknown_side_effects,
		     region_model_context *ctxt);

  /* Specific handling for on_call_pre.  */
  bool impl_call_alloca (const call_details &cd);
  void impl_call_analyzer_describe (const gcall *call,
				    region_model_context *ctxt);
  void impl_call_analyzer_eval (const gcall *call,
				region_model_context *ctxt);
  bool impl_call_builtin_expect (const call_details &cd);
  bool impl_call_calloc (const call_details &cd);
  void impl_call_free (const call_details &cd);
  bool impl_call_malloc (const call_details &cd);
  void impl_call_memcpy (const call_details &cd);
  bool impl_call_memset (const call_details &cd);
  void impl_call_strcpy (const call_details &cd);
  bool impl_call_strlen (const call_details &cd);
  bool impl_call_operator_new (const call_details &cd);
  bool impl_call_operator_delete (const call_details &cd);

  void handle_unrecognized_call (const gcall *call,
				 region_model_context *ctxt);
  void get_reachable_svalues (svalue_set *out,
			      const svalue *extra_sval);

  void on_return (const greturn *stmt, region_model_context *ctxt);
  void on_setjmp (const gcall *stmt, const exploded_node *enode,
		  region_model_context *ctxt);
  void on_longjmp (const gcall *longjmp_call, const gcall *setjmp_call,
		   int setjmp_stack_depth, region_model_context *ctxt);

  void update_for_phis (const supernode *snode,
			const cfg_superedge *last_cfg_superedge,
			region_model_context *ctxt);

  void handle_phi (const gphi *phi, tree lhs, tree rhs,
		   region_model_context *ctxt);

  bool maybe_update_for_edge (const superedge &edge,
			      const gimple *last_stmt,
			      region_model_context *ctxt);

  const region *push_frame (function *fun, const vec<const svalue *> *arg_sids,
			    region_model_context *ctxt);
  const frame_region *get_current_frame () const { return m_current_frame; }
  function * get_current_function () const;
  void pop_frame (const region *result_dst,
		  const svalue **out_result,
		  region_model_context *ctxt);
  int get_stack_depth () const;
  const frame_region *get_frame_at_index (int index) const;

  const region *get_lvalue (path_var pv, region_model_context *ctxt);
  const region *get_lvalue (tree expr, region_model_context *ctxt);
  const svalue *get_rvalue (path_var pv, region_model_context *ctxt);
  const svalue *get_rvalue (tree expr, region_model_context *ctxt);

  const region *deref_rvalue (const svalue *ptr_sval, tree ptr_tree,
			       region_model_context *ctxt);

  void set_value (const region *lhs_reg, const svalue *rhs_sval,
		  region_model_context *ctxt);
  void set_value (tree lhs, tree rhs, region_model_context *ctxt);
  void clobber_region (const region *reg);
  void purge_region (const region *reg);
  void zero_fill_region (const region *reg);
  void mark_region_as_unknown (const region *reg);

  void copy_region (const region *dst_reg, const region *src_reg,
		    region_model_context *ctxt);
  tristate eval_condition (const svalue *lhs,
			   enum tree_code op,
			   const svalue *rhs) const;
  tristate eval_condition_without_cm (const svalue *lhs,
				      enum tree_code op,
				      const svalue *rhs) const;
  tristate compare_initial_and_pointer (const initial_svalue *init,
					const region_svalue *ptr) const;
  tristate eval_condition (tree lhs,
			   enum tree_code op,
			   tree rhs,
			   region_model_context *ctxt);
  bool add_constraint (tree lhs, enum tree_code op, tree rhs,
		       region_model_context *ctxt);

  const region *create_region_for_heap_alloc (const svalue *size_in_bytes);
  const region *create_region_for_alloca (const svalue *size_in_bytes);

  tree get_representative_tree (const svalue *sval) const;
  path_var
  get_representative_path_var (const svalue *sval,
			       svalue_set *visited) const;
  path_var
  get_representative_path_var (const region *reg,
			       svalue_set *visited) const;

  /* For selftests.  */
  constraint_manager *get_constraints ()
  {
    return m_constraints;
  }

  store *get_store () { return &m_store; }
  const store *get_store () const { return &m_store; }

  region_model_manager *get_manager () const { return m_mgr; }

  void unbind_region_and_descendents (const region *reg,
				      enum poison_kind pkind);

  bool can_merge_with_p (const region_model &other_model,
			 const program_point &point,
			 region_model *out_model) const;

  tree get_fndecl_for_call (const gcall *call,
			    region_model_context *ctxt);

  void get_ssa_name_regions_for_current_frame
    (auto_vec<const decl_region *> *out) const;
  static void append_ssa_names_cb (const region *base_reg,
				   struct append_ssa_names_cb_data *data);

  const svalue *get_store_value (const region *reg) const;

  bool region_exists_p (const region *reg) const;

  void loop_replay_fixup (const region_model *dst_state);

 private:
  const region *get_lvalue_1 (path_var pv, region_model_context *ctxt);
  const svalue *get_rvalue_1 (path_var pv, region_model_context *ctxt);

  void add_any_constraints_from_ssa_def_stmt (tree lhs,
					      enum tree_code op,
					      tree rhs,
					      region_model_context *ctxt);
  void add_any_constraints_from_gassign (enum tree_code op,
					 tree rhs,
					 const gassign *assign,
					 region_model_context *ctxt);
  void add_any_constraints_from_gcall (enum tree_code op,
				       tree rhs,
				       const gcall *call,
				       region_model_context *ctxt);

  void update_for_call_superedge (const call_superedge &call_edge,
				  region_model_context *ctxt);
  void update_for_return_superedge (const return_superedge &return_edge,
				    region_model_context *ctxt);
  void update_for_call_summary (const callgraph_superedge &cg_sedge,
				region_model_context *ctxt);
  bool apply_constraints_for_gcond (const cfg_superedge &edge,
				    const gcond *cond_stmt,
				    region_model_context *ctxt);
  bool apply_constraints_for_gswitch (const switch_cfg_superedge &edge,
				      const gswitch *switch_stmt,
				      region_model_context *ctxt);
  bool apply_constraints_for_exception (const gimple *last_stmt,
					region_model_context *ctxt);

  int poison_any_pointers_to_descendents (const region *reg,
					  enum poison_kind pkind);

  void on_top_level_param (tree param, region_model_context *ctxt);

  void record_dynamic_extents (const region *reg,
			       const svalue *size_in_bytes);

  bool called_from_main_p () const;
  const svalue *get_initial_value_for_global (const region *reg) const;

  /* Storing this here to avoid passing it around everywhere.  */
  region_model_manager *const m_mgr;

  store m_store;

  constraint_manager *m_constraints; // TODO: embed, rather than dynalloc?

  const frame_region *m_current_frame;
};

/* Some region_model activity could lead to warnings (e.g. attempts to use an
   uninitialized value).  This abstract base class encapsulates an interface
   for the region model to use when emitting such warnings.

   Having this as an abstract base class allows us to support the various
   operations needed by program_state in the analyzer within region_model,
   whilst keeping them somewhat modularized.  */

class region_model_context
{
 public:
  virtual void warn (pending_diagnostic *d) = 0;

  /* Hook for clients to be notified when an SVAL that was reachable
     in a previous state is no longer live, so that clients can emit warnings
     about leaks.  */
  virtual void on_svalue_leak (const svalue *sval) = 0;

  /* Hook for clients to be notified when the set of explicitly live
     svalues changes, so that they can purge state relating to dead
     svalues.  */
  virtual void on_liveness_change (const svalue_set &live_svalues,
				   const region_model *model) = 0;

  virtual logger *get_logger () = 0;

  /* Hook for clients to be notified when the condition
     "LHS OP RHS" is added to the region model.
     This exists so that state machines can detect tests on edges,
     and use them to trigger sm-state transitions (e.g. transitions due
     to ptrs becoming known to be NULL or non-NULL, rather than just
     "unchecked") */
  virtual void on_condition (tree lhs, enum tree_code op, tree rhs) = 0;

  /* Hooks for clients to be notified when an unknown change happens
     to SVAL (in response to a call to an unknown function).  */
  virtual void on_unknown_change (const svalue *sval, bool is_mutable) = 0;

  /* Hooks for clients to be notified when a phi node is handled,
     where RHS is the pertinent argument.  */
  virtual void on_phi (const gphi *phi, tree rhs) = 0;

  /* Hooks for clients to be notified when the region model doesn't
     know how to handle the tree code of T at LOC.  */
  virtual void on_unexpected_tree_code (tree t,
					const dump_location_t &loc) = 0;
};

/* A "do nothing" subclass of region_model_context.  */

class noop_region_model_context : public region_model_context
{
public:
  void warn (pending_diagnostic *) OVERRIDE {}
  void on_svalue_leak (const svalue *) OVERRIDE {}
  void on_liveness_change (const svalue_set &,
			   const region_model *) OVERRIDE {}
  logger *get_logger () OVERRIDE { return NULL; }
  void on_condition (tree lhs ATTRIBUTE_UNUSED,
		     enum tree_code op ATTRIBUTE_UNUSED,
		     tree rhs ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_unknown_change (const svalue *sval ATTRIBUTE_UNUSED,
			  bool is_mutable ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_phi (const gphi *phi ATTRIBUTE_UNUSED,
	       tree rhs ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_unexpected_tree_code (tree, const dump_location_t &) OVERRIDE {}
};

/* A subclass of region_model_context for determining if operations fail
   e.g. "can we generate a region for the lvalue of EXPR?".  */

class tentative_region_model_context : public noop_region_model_context
{
public:
  tentative_region_model_context () : m_num_unexpected_codes (0) {}

  void on_unexpected_tree_code (tree, const dump_location_t &)
    FINAL OVERRIDE
  {
    m_num_unexpected_codes++;
  }

  bool had_errors_p () const { return m_num_unexpected_codes > 0; }

private:
  int m_num_unexpected_codes;
};

/* A bundle of data for use when attempting to merge two region_model
   instances to make a third.  */

struct model_merger
{
  model_merger (const region_model *model_a,
		const region_model *model_b,
		const program_point &point,
		region_model *merged_model)
  : m_model_a (model_a), m_model_b (model_b),
    m_point (point),
    m_merged_model (merged_model)
  {
  }

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (FILE *fp, bool simple) const;
  void dump (bool simple) const;

  region_model_manager *get_manager () const
  {
    return m_model_a->get_manager ();
  }

  const region_model *m_model_a;
  const region_model *m_model_b;
  const program_point &m_point;
  region_model *m_merged_model;
};

/* A bundle of state.  */

class engine
{
public:
  region_model_manager *get_model_manager () { return &m_mgr; }

  void log_stats (logger *logger) const;

private:
  region_model_manager m_mgr;

};

} // namespace ana

extern void debug (const region_model &rmodel);

namespace ana {

#if CHECKING_P

namespace selftest {

using namespace ::selftest;

/* An implementation of region_model_context for use in selftests, which
   stores any pending_diagnostic instances passed to it.  */

class test_region_model_context : public noop_region_model_context
{
public:
  void warn (pending_diagnostic *d) FINAL OVERRIDE
  {
    m_diagnostics.safe_push (d);
  }

  unsigned get_num_diagnostics () const { return m_diagnostics.length (); }

  void on_unexpected_tree_code (tree t, const dump_location_t &)
    FINAL OVERRIDE
  {
    internal_error ("unhandled tree code: %qs",
		    get_tree_code_name (TREE_CODE (t)));
  }

private:
  /* Implicitly delete any diagnostics in the dtor.  */
  auto_delete_vec<pending_diagnostic> m_diagnostics;
};

/* Attempt to add the constraint (LHS OP RHS) to MODEL.
   Verify that MODEL remains satisfiable.  */

#define ADD_SAT_CONSTRAINT(MODEL, LHS, OP, RHS)	\
  SELFTEST_BEGIN_STMT					\
    bool sat = (MODEL).add_constraint (LHS, OP, RHS, NULL);	\
    ASSERT_TRUE (sat);					\
  SELFTEST_END_STMT

/* Attempt to add the constraint (LHS OP RHS) to MODEL.
   Verify that the result is not satisfiable.  */

#define ADD_UNSAT_CONSTRAINT(MODEL, LHS, OP, RHS)	\
  SELFTEST_BEGIN_STMT					\
    bool sat = (MODEL).add_constraint (LHS, OP, RHS, NULL);	\
    ASSERT_FALSE (sat);				\
  SELFTEST_END_STMT

/* Implementation detail of the ASSERT_CONDITION_* macros.  */

void assert_condition (const location &loc,
		       region_model &model,
		       const svalue *lhs, tree_code op, const svalue *rhs,
		       tristate expected);

void assert_condition (const location &loc,
		       region_model &model,
		       tree lhs, tree_code op, tree rhs,
		       tristate expected);

/* Assert that REGION_MODEL evaluates the condition "LHS OP RHS"
   as "true".  */

#define ASSERT_CONDITION_TRUE(REGION_MODEL, LHS, OP, RHS) \
  SELFTEST_BEGIN_STMT							\
  assert_condition (SELFTEST_LOCATION, REGION_MODEL, LHS, OP, RHS,	\
		    tristate (tristate::TS_TRUE));		\
  SELFTEST_END_STMT

/* Assert that REGION_MODEL evaluates the condition "LHS OP RHS"
   as "false".  */

#define ASSERT_CONDITION_FALSE(REGION_MODEL, LHS, OP, RHS) \
  SELFTEST_BEGIN_STMT							\
  assert_condition (SELFTEST_LOCATION, REGION_MODEL, LHS, OP, RHS,	\
		    tristate (tristate::TS_FALSE));		\
  SELFTEST_END_STMT

/* Assert that REGION_MODEL evaluates the condition "LHS OP RHS"
   as "unknown".  */

#define ASSERT_CONDITION_UNKNOWN(REGION_MODEL, LHS, OP, RHS) \
  SELFTEST_BEGIN_STMT							\
  assert_condition (SELFTEST_LOCATION, REGION_MODEL, LHS, OP, RHS,	\
		    tristate (tristate::TS_UNKNOWN));		\
  SELFTEST_END_STMT

} /* end of namespace selftest.  */

#endif /* #if CHECKING_P */

} // namespace ana

#endif /* GCC_ANALYZER_REGION_MODEL_H */
