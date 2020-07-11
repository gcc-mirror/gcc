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

/* A tree, extended with stack frame information for locals, so that
   we can distinguish between different values of locals within a potentially
   recursive callstack.  */
// TODO: would this be better as a new tree code?

using namespace ana;

namespace ana {

class path_var
{
public:
  path_var (tree t, int stack_depth)
  : m_tree (t), m_stack_depth (stack_depth)
  {
    // TODO: ignore stack depth for globals and constants
  }

  bool operator== (const path_var &other) const
  {
    return (m_tree == other.m_tree
	    && m_stack_depth == other.m_stack_depth);
  }

  void dump (pretty_printer *pp) const;

  tree m_tree;
  int m_stack_depth; // or -1 for globals?
};

} // namespace ana

namespace inchash
{
  extern void add_path_var (path_var pv, hash &hstate);
} // namespace inchash


namespace ana {

/* A region_model is effectively a graph of regions and symbolic values.
   We store per-model IDs rather than pointers to make it easier to clone
   and to compare graphs.  */

/* An ID for an svalue within a region_model.  Internally, this is an index
   into a vector of svalue * within the region_model.  */

class svalue_id
{
public:
  static svalue_id null () { return svalue_id (-1); }

  svalue_id () : m_idx (-1) {}

  bool operator== (const svalue_id &other) const
  {
    return m_idx == other.m_idx;
  }

  bool operator!= (const svalue_id &other) const
  {
    return m_idx != other.m_idx;
  }

  bool null_p () const { return m_idx == -1; }

  static svalue_id from_int (int idx) { return svalue_id (idx); }
  int as_int () const { return m_idx; }

  void print (pretty_printer *pp) const;
  void dump_node_name_to_pp (pretty_printer *pp) const;

  void validate (const region_model &model) const;

private:
  svalue_id (int idx) : m_idx (idx) {}

  int m_idx;
};

/* An ID for a region within a region_model.  Internally, this is an index
   into a vector of region * within the region_model.  */

class region_id
{
public:
  static region_id null () { return region_id (-1); }

  region_id () : m_idx (-1) {}

  bool operator== (const region_id &other) const
  {
    return m_idx == other.m_idx;
  }

  bool operator!= (const region_id &other) const
  {
    return m_idx != other.m_idx;
  }

  bool null_p () const { return m_idx == -1; }

  static region_id from_int (int idx) { return region_id (idx); }
  int as_int () const { return m_idx; }

  void print (pretty_printer *pp) const;
  void dump_node_name_to_pp (pretty_printer *pp) const;

  void validate (const region_model &model) const;

private:
  region_id (int idx) : m_idx (idx) {}

  int m_idx;
};

/* A class for renumbering IDs within a region_model, mapping old IDs
   to new IDs (e.g. when removing one or more elements, thus needing to
   renumber).  */
// TODO: could this be useful for equiv_class_ids?

template <typename T>
class id_map
{
 public:
  id_map (int num_ids);
  void put (T src, T dst);
  T get_dst_for_src (T src) const;
  T get_src_for_dst (T dst) const;
  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;
  void update (T *) const;

 private:
  auto_vec<T> m_src_to_dst;
  auto_vec<T> m_dst_to_src;
};

typedef id_map<svalue_id> svalue_id_map;
typedef id_map<region_id> region_id_map;

/* class id_map.  */

/* id_map's ctor, which populates the map with dummy null values.  */

template <typename T>
inline id_map<T>::id_map (int num_svalues)
: m_src_to_dst (num_svalues),
  m_dst_to_src (num_svalues)
{
  for (int i = 0; i < num_svalues; i++)
    {
      m_src_to_dst.quick_push (T::null ());
      m_dst_to_src.quick_push (T::null ());
    }
}

/* Record that SRC is to be mapped to DST.  */

template <typename T>
inline void
id_map<T>::put (T src, T dst)
{
  m_src_to_dst[src.as_int ()] = dst;
  m_dst_to_src[dst.as_int ()] = src;
}

/* Get the new value for SRC within the map.  */

template <typename T>
inline T
id_map<T>::get_dst_for_src (T src) const
{
  if (src.null_p ())
    return src;
  return m_src_to_dst[src.as_int ()];
}

/* Given DST, a new value, determine which old value will be mapped to it
   (the inverse of the map).  */

template <typename T>
inline T
id_map<T>::get_src_for_dst (T dst) const
{
  if (dst.null_p ())
    return dst;
  return m_dst_to_src[dst.as_int ()];
}

/* Dump this id_map to PP.  */

template <typename T>
inline void
id_map<T>::dump_to_pp (pretty_printer *pp) const
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

  pp_string (pp, "dst to src: {");
  T *src;
  FOR_EACH_VEC_ELT (m_dst_to_src, i, src)
    {
      if (i > 0)
	pp_string (pp, ", ");
      T dst (T::from_int (i));
      dst.print (pp);
      pp_string (pp, " <- ");
      src->print (pp);
    }
  pp_string (pp, "}");
  pp_newline (pp);
}

/* Dump this id_map to stderr.  */

template <typename T>
DEBUG_FUNCTION inline void
id_map<T>::dump () const
{
  pretty_printer pp;
  pp.buffer->stream = stderr;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Update *ID from the old value to its new value in this map.  */

template <typename T>
inline void
id_map<T>::update (T *id) const
{
  *id = get_dst_for_src (*id);
}

/* Variant of the above, which only stores things in one direction.
   (e.g. for merging, when the number of destination regions is not
   the same of the src regions, and can grow).  */

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

typedef one_way_id_map<svalue_id> one_way_svalue_id_map;
typedef one_way_id_map<region_id> one_way_region_id_map;

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

/* A set of region_ids within a region_model.  */

class region_id_set
{
public:
  region_id_set (const region_model *model);

  void add_region (region_id rid)
  {
    if (!rid.null_p ())
      bitmap_set_bit (m_bitmap, rid.as_int ());
  }

  bool region_p (region_id rid) const
  {
    gcc_assert (!rid.null_p ());
    return bitmap_bit_p (const_cast <auto_sbitmap &> (m_bitmap),
			 rid.as_int ());
  }

  unsigned int num_regions ()
  {
    return bitmap_count_bits (m_bitmap);
  }

private:
  auto_sbitmap m_bitmap;
};

/* A set of svalue_ids within a region_model.  */

class svalue_id_set
{
public:
  svalue_id_set ();

  void add_svalue (svalue_id sid)
  {
    if (!sid.null_p ())
      bitmap_set_bit (m_bitmap, sid.as_int ());
  }

  bool svalue_p (svalue_id sid) const
  {
    gcc_assert (!sid.null_p ());
    return bitmap_bit_p (const_cast <auto_bitmap &> (m_bitmap),
			 sid.as_int ());
  }

private:
  auto_bitmap m_bitmap;
};

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

/* An enum for discriminating between the different concrete subclasses
   of svalue.  */

enum svalue_kind
{
  SK_REGION,
  SK_CONSTANT,
  SK_UNKNOWN,
  SK_POISONED,
  SK_SETJMP
};

/* svalue and its subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with svalue_kinds shown for the concrete subclasses):

   svalue
     region_svalue (SK_REGION)
     constant_svalue (SK_CONSTANT)
     unknown_svalue (SK_UNKNOWN)
     poisoned_svalue (SK_POISONED)
     setjmp_svalue (SK_SETJMP).  */

/* An abstract base class representing a value held by a region of memory.  */

class svalue
{
public:
  virtual ~svalue () {}

  bool operator== (const svalue &other) const;
  bool operator!= (const svalue &other) const { return !(*this == other); }

  virtual svalue *clone () const = 0;

  tree get_type () const { return m_type; }

  virtual enum svalue_kind get_kind () const = 0;

  hashval_t hash () const;

  void print (const region_model &model,
	      svalue_id this_sid,
	      pretty_printer *pp) const;

  virtual void dump_dot_to_pp (const region_model &model,
			       svalue_id this_sid,
			       pretty_printer *pp) const;

  virtual region_svalue *dyn_cast_region_svalue () { return NULL; }
  virtual constant_svalue *dyn_cast_constant_svalue () { return NULL; }
  virtual const constant_svalue *dyn_cast_constant_svalue () const
  { return NULL; }
  virtual poisoned_svalue *dyn_cast_poisoned_svalue () { return NULL; }
  virtual unknown_svalue *dyn_cast_unknown_svalue () { return NULL; }
  virtual setjmp_svalue *dyn_cast_setjmp_svalue () { return NULL; }

  virtual void remap_region_ids (const region_id_map &map);

  virtual void walk_for_canonicalization (canonicalization *c) const;

  virtual svalue_id get_child_sid (region *parent, region *child,
				   region_model &model,
				   region_model_context *ctxt);

  tree maybe_get_constant () const;

 protected:
  svalue (tree type) : m_type (type) {}

  virtual void add_to_hash (inchash::hash &hstate) const = 0;

 private:
  virtual void print_details (const region_model &model,
			      svalue_id this_sid,
			      pretty_printer *pp) const = 0;
  tree m_type;
};

/* Concrete subclass of svalue representing a pointer value that points to
   a known region  */

class region_svalue : public svalue
{
public:
  region_svalue (tree type, region_id rid) : svalue (type), m_rid (rid)
  {
    /* Should we support NULL ptrs here?  */
    gcc_assert (!rid.null_p ());
  }

  bool compare_fields (const region_svalue &other) const;

  svalue *clone () const FINAL OVERRIDE
  { return new region_svalue (get_type (), m_rid); }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_REGION; }

  void dump_dot_to_pp (const region_model &model,
		       svalue_id this_sid,
		       pretty_printer *pp) const
    FINAL OVERRIDE;

  region_svalue *dyn_cast_region_svalue () FINAL OVERRIDE { return this; }

  region_id get_pointee () const { return m_rid; }

  void remap_region_ids (const region_id_map &map) FINAL OVERRIDE;

  static void merge_values (const region_svalue &region_sval_a,
			    const region_svalue &region_sval_b,
			    svalue_id *merged_sid,
			    tree type,
			    model_merger *merger);

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

  static tristate eval_condition (region_svalue *lhs_ptr,
				  enum tree_code op,
				  region_svalue *rhs_ptr);

  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;

 private:
  void print_details (const region_model &model,
		      svalue_id this_sid,
		      pretty_printer *pp) const
    FINAL OVERRIDE;

  region_id m_rid;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <region_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_REGION;
}

namespace ana {

/* Concrete subclass of svalue representing a specific constant value.  */

class constant_svalue : public svalue
{
public:
  constant_svalue (tree cst_expr)
  : svalue (TREE_TYPE (cst_expr)), m_cst_expr (cst_expr)
  {
    gcc_assert (cst_expr);
    gcc_assert (CONSTANT_CLASS_P (cst_expr));
  }

  bool compare_fields (const constant_svalue &other) const;

  svalue *clone () const FINAL OVERRIDE
  { return new constant_svalue (m_cst_expr); }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_CONSTANT; }

  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;

  constant_svalue *dyn_cast_constant_svalue () FINAL OVERRIDE { return this; }
  const constant_svalue *dyn_cast_constant_svalue () const FINAL OVERRIDE
  { return this; }

  tree get_constant () const { return m_cst_expr; }

  static void merge_values (const constant_svalue &cst_sval_a,
			    const constant_svalue &cst_sval_b,
			    svalue_id *merged_sid,
			    model_merger *merger);

  static tristate eval_condition (constant_svalue *lhs,
				  enum tree_code op,
				  constant_svalue *rhs);

  svalue_id get_child_sid (region *parent, region *child,
			   region_model &model,
			   region_model_context *ctxt) FINAL OVERRIDE;

 private:
  void print_details (const region_model &model,
		      svalue_id this_sid,
		      pretty_printer *pp) const
    FINAL OVERRIDE;

  tree m_cst_expr;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <constant_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_CONSTANT;
}

namespace ana {

/* Concrete subclass of svalue representing a unique but unknown value.
   Comparisons of variables that share the same unknown value are known
   to be equal, even if we don't know what the value is.  */

class unknown_svalue : public svalue
{
public:
  unknown_svalue (tree type)
  : svalue (type)
  {}

  bool compare_fields (const unknown_svalue &other) const;

  svalue *clone () const FINAL OVERRIDE
  { return new unknown_svalue (get_type ()); }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_UNKNOWN; }

  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;

  unknown_svalue *dyn_cast_unknown_svalue () FINAL OVERRIDE { return this; }

 private:
  void print_details (const region_model &model,
		      svalue_id this_sid,
		      pretty_printer *pp) const
    FINAL OVERRIDE;
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
  poisoned_svalue (enum poison_kind kind, tree type)
  : svalue (type), m_kind (kind) {}

  bool compare_fields (const poisoned_svalue &other) const;

  svalue *clone () const FINAL OVERRIDE
  { return new poisoned_svalue (m_kind, get_type ()); }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_POISONED; }

  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;

  poisoned_svalue *dyn_cast_poisoned_svalue () FINAL OVERRIDE { return this; }

  enum poison_kind get_poison_kind () const { return m_kind; }

 private:
  void print_details (const region_model &model,
		      svalue_id this_sid,
		      pretty_printer *pp) const
    FINAL OVERRIDE;

  enum poison_kind m_kind;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <poisoned_svalue *>::test (svalue *sval)
{
  return sval->get_kind () == SK_POISONED;
}

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

  const exploded_node *m_enode;
  const gcall *m_setjmp_call;
};

/* Concrete subclass of svalue representing buffers for setjmp/sigsetjmp,
   so that longjmp/siglongjmp can potentially "return" to an entirely
   different function.  */

class setjmp_svalue : public svalue
{
public:
  setjmp_svalue (const setjmp_record &setjmp_record,
		 tree type)
  : svalue (type), m_setjmp_record (setjmp_record)
  {}

  bool compare_fields (const setjmp_svalue &other) const;

  svalue *clone () const FINAL OVERRIDE
  { return new setjmp_svalue (m_setjmp_record, get_type ()); }

  enum svalue_kind get_kind () const FINAL OVERRIDE { return SK_SETJMP; }

  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;

  setjmp_svalue *dyn_cast_setjmp_svalue () FINAL OVERRIDE { return this; }

  int get_enode_index () const;

  const setjmp_record &get_setjmp_record () const { return m_setjmp_record; }

 private:
  void print_details (const region_model &model,
		      svalue_id this_sid,
		      pretty_printer *pp) const
    FINAL OVERRIDE;

  setjmp_record m_setjmp_record;
};

/* An enum for discriminating between the different concrete subclasses
   of region.  */

enum region_kind
{
  RK_PRIMITIVE,
  RK_STRUCT,
  RK_UNION,
  RK_FRAME,
  RK_GLOBALS,
  RK_CODE,
  RK_FUNCTION,
  RK_ARRAY,
  RK_STACK,
  RK_HEAP,
  RK_ROOT,
  RK_SYMBOLIC
};

extern const char *region_kind_to_str (enum region_kind);

/* Region and its subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with region_kinds shown for the concrete subclasses):

   region
     primitive_region (RK_PRIMITIVE)
     map_region
       struct_or_union_region
         struct_region (RK_STRUCT)
         union_region (RK_UNION)
       scope_region
         frame_region (RK_FRAME)
         globals_region (RK_GLOBALS)
       code_region (RK_CODE)
       function_region (RK_FUNCTION)
     array_region (RK_ARRAY)
     stack_region (RK_STACK)
     heap_region (RK_HEAP)
     root_region (RK_ROOT)
     label_region (RK_FUNCTION)
     symbolic_region (RK_SYMBOLIC).  */

/* Abstract base class representing a chunk of memory.

   Regions form a tree-like hierarchy, with a root region at the base,
   with memory space regions within it, representing the stack and
   globals, with frames within the stack, and regions for variables
   within the frames and the "globals" region.  Regions for structs
   can have subregions for fields.

   A region can optionally have a value, or inherit its value from
   the first ancestor with a value.  For example, the stack region
   has a "uninitialized" poison value which is inherited by all
   descendent regions that don't themselves have a value.  */

class region
{
public:
  virtual ~region () {}

  bool operator== (const region &other) const;
  bool operator!= (const region &other) const { return !(*this == other); }

  virtual region *clone () const = 0;

  virtual enum region_kind get_kind () const = 0;
  virtual map_region *dyn_cast_map_region () { return NULL; }
  virtual array_region *dyn_cast_array_region () { return NULL; }
  virtual symbolic_region *dyn_cast_symbolic_region () { return NULL; }
  virtual const symbolic_region *dyn_cast_symbolic_region () const { return NULL; }

  region_id get_parent () const { return m_parent_rid; }
  region *get_parent_region (const region_model &model) const;

  void set_value (region_model &model, region_id this_rid, svalue_id rhs_sid,
		  region_model_context *ctxt);
  svalue_id get_value (region_model &model, bool non_null,
		       region_model_context *ctxt);
  svalue_id get_value_direct () const { return m_sval_id; }

  svalue_id get_inherited_child_sid (region *child,
				     region_model &model,
				     region_model_context *ctxt);

  tree get_type () const { return m_type; }

  hashval_t hash () const;

  void print (const region_model &model,
	      region_id this_rid,
	      pretty_printer *pp) const;

  virtual void dump_dot_to_pp (const region_model &model,
			       region_id this_rid,
			       pretty_printer *pp) const;

  void dump_to_pp (const region_model &model,
		   region_id this_rid,
		   pretty_printer *pp,
		   const char *prefix,
		   bool is_last_child) const;
  virtual void dump_child_label (const region_model &model,
				 region_id this_rid,
				 region_id child_rid,
				 pretty_printer *pp) const;

  void remap_svalue_ids (const svalue_id_map &map);
  virtual void remap_region_ids (const region_id_map &map);

  virtual void walk_for_canonicalization (canonicalization *c) const = 0;

  void add_view (region_id view_rid, region_model *model);
  region_id get_view (tree type, region_model *model) const;
  region_id get_active_view () const { return m_active_view_rid; }
  bool is_view_p () const { return m_is_view; }

  virtual void validate (const region_model &model) const;

  bool non_null_p (const region_model &model) const;

 protected:
  region (region_id parent_rid, svalue_id sval_id, tree type);
  region (const region &other);

  virtual void add_to_hash (inchash::hash &hstate) const;
  virtual void print_fields (const region_model &model,
			     region_id this_rid,
			     pretty_printer *pp) const;

 private:
  void become_active_view (region_model &model, region_id this_rid);
  void deactivate_any_active_view (region_model &model);
  void deactivate_view (region_model &model, region_id this_view_rid);

  region_id m_parent_rid;
  svalue_id m_sval_id;
  tree m_type;
  /* Child regions that are "views" (one per type).  */
  auto_vec<region_id> m_view_rids;

  bool m_is_view;
  region_id m_active_view_rid;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <region *>::test (region *)
{
  return true;
}

namespace ana {

/* Concrete region subclass for storing "primitive" types (integral types,
   pointers, etc).  */

class primitive_region : public region
{
public:
  primitive_region (region_id parent_rid, tree type)
  : region (parent_rid, svalue_id::null (), type)
  {}

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_PRIMITIVE; }

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;
};

/* A region that has children identified by tree keys.
   For example a stack frame has subregions per local, and a region
   for a struct has subregions per field.  */

class map_region : public region
{
public:
  typedef ordered_hash_map<tree, region_id> map_t;
  typedef map_t::iterator iterator_t;

  map_region (region_id parent_rid, tree type)
  : region (parent_rid, svalue_id::null (), type)
  {}
  map_region (const map_region &other);

  map_region *dyn_cast_map_region () FINAL OVERRIDE { return this; }

  void dump_dot_to_pp (const region_model &model,
		       region_id this_rid,
		       pretty_printer *pp) const
    FINAL OVERRIDE;

  void dump_child_label (const region_model &model,
			 region_id this_rid,
			 region_id child_rid,
			 pretty_printer *pp) const
    FINAL OVERRIDE;

  region_id get_or_create (region_model *model,
			   region_id this_rid,
			   tree expr, tree type,
			   region_model_context *ctxt);
  void unbind (tree expr);
  region_id *get (tree expr);

  void remap_region_ids (const region_id_map &map) FINAL OVERRIDE;

  tree get_tree_for_child_region (region_id child_rid) const;

  tree get_tree_for_child_region (region *child,
				  const region_model &model) const;

  static bool can_merge_p (const map_region *map_region_a,
			   const map_region *map_region_b,
			   map_region *merged_map_region,
			   region_id merged_rid,
			   model_merger *merger);

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

  virtual bool valid_key_p (tree key) const = 0;

  svalue_id get_value_by_name (tree identifier,
			       const region_model &model) const;

  iterator_t begin () { return m_map.begin (); }
  iterator_t end () { return m_map.end (); }
  size_t elements () const { return m_map.elements (); }

 protected:
  bool compare_fields (const map_region &other) const;
  void add_to_hash (inchash::hash &hstate) const OVERRIDE;
  void print_fields (const region_model &model,
		     region_id this_rid,
		     pretty_printer *pp) const
    OVERRIDE;
  void validate (const region_model &model) const FINAL OVERRIDE;

 private:
  /* Mapping from tree to child region.  */
  map_t m_map;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <map_region *>::test (region *reg)
{
  return (reg->dyn_cast_map_region () != NULL);
}

namespace ana {

/* Abstract subclass representing a region with fields
   (either a struct or a union).  */

class struct_or_union_region : public map_region
{
public:
  bool valid_key_p (tree key) const FINAL OVERRIDE;

 protected:
  struct_or_union_region (region_id parent_rid, tree type)
  : map_region (parent_rid, type)
  {}

  bool compare_fields (const struct_or_union_region &other) const;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <struct_or_union_region *>::test (region *reg)
{
  return (reg->get_kind () == RK_STRUCT
	  || reg->get_kind () == RK_UNION);
}

namespace ana {

/* Concrete region subclass.  A map_region representing a struct, using
   FIELD_DECLs for its keys.  */

class struct_region : public struct_or_union_region
{
public:
  struct_region (region_id parent_rid, tree type)
  : struct_or_union_region (parent_rid, type)
  {
    gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  }

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_STRUCT; }

  bool compare_fields (const struct_region &other) const;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <struct_region *>::test (region *reg)
{
  return reg->get_kind () == RK_STRUCT;
}

namespace ana {

/* Concrete region subclass.  A map_region representing a union, using
   FIELD_DECLs for its keys.  */

class union_region : public struct_or_union_region
{
public:
  union_region (region_id parent_rid, tree type)
  : struct_or_union_region (parent_rid, type)
  {
    gcc_assert (TREE_CODE (type) == UNION_TYPE);
  }

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_UNION; }

  bool compare_fields (const union_region &other) const;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <union_region *>::test (region *reg)
{
  return reg->get_kind () == RK_UNION;
}

namespace ana {

/* Abstract map_region subclass for accessing decls, used as a base class
   for function frames and for the globals region.  */

class scope_region : public map_region
{
 public:

 protected:
  scope_region (region_id parent_rid)
  : map_region (parent_rid, NULL_TREE)
  {}

  scope_region (const scope_region &other)
  : map_region (other)
  {
  }

  bool compare_fields (const scope_region &other) const;
};

/* Concrete region subclass, representing a function frame on the stack,
   to contain the locals.  */

class frame_region : public scope_region
{
public:
  frame_region (region_id parent_rid, function *fun, int depth)
  : scope_region (parent_rid), m_fun (fun), m_depth (depth)
  {}

  frame_region (const frame_region &other)
  : scope_region (other), m_fun (other.m_fun), m_depth (other.m_depth)
  {
  }

  /* region vfuncs.  */
  region *clone () const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_FRAME; }
  void print_fields (const region_model &model,
		     region_id this_rid,
		     pretty_printer *pp) const
    FINAL OVERRIDE;
  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;

  /* map_region vfuncs.  */
  bool valid_key_p (tree key) const FINAL OVERRIDE;

  /* Accessors.  */
  function *get_function () const { return m_fun; }
  int get_depth () const { return m_depth; }

  bool compare_fields (const frame_region &other) const;

 private:
  function *m_fun;
  int m_depth;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <frame_region *>::test (region *reg)
{
  return reg->get_kind () == RK_FRAME;
}

namespace ana {

/* Concrete region subclass, to hold global variables (data and bss).  */

class globals_region : public scope_region
{
 public:
  globals_region (region_id parent_rid)
  : scope_region (parent_rid)
  {}

  globals_region (const globals_region &other)
  : scope_region (other)
  {
  }

  /* region vfuncs.  */
  region *clone () const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_GLOBALS; }

  /* map_region vfuncs.  */
  bool valid_key_p (tree key) const FINAL OVERRIDE;

  bool compare_fields (const globals_region &other) const;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <globals_region *>::test (region *reg)
{
  return reg->get_kind () == RK_GLOBALS;
}

namespace ana {

/* Concrete region subclass.  A map_region representing the code, using
   FUNCTION_DECLs for its keys.  */

class code_region : public map_region
{
public:
  code_region (region_id parent_rid)
  : map_region (parent_rid, NULL_TREE)
  {}
  code_region (const code_region &other)
  : map_region (other)
  {}

  /* region vfuncs.  */
  region *clone () const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_CODE; }

  /* map_region vfunc.  */
  bool valid_key_p (tree key) const FINAL OVERRIDE;

  region_id get_element (region_model *model,
			 region_id this_rid,
			 svalue_id index_sid,
			 region_model_context *ctxt);

  bool compare_fields (const code_region &other) const;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <code_region *>::test (region *reg)
{
  return reg->get_kind () == RK_CODE;
}

namespace ana {

/* Concrete region subclass.  A map_region representing the code for
   a particular function, using LABEL_DECLs for its keys.  */

class function_region : public map_region
{
public:
  function_region (region_id parent_rid, tree type)
  : map_region (parent_rid, type)
  {
    gcc_assert (FUNC_OR_METHOD_TYPE_P (type));
  }
  function_region (const function_region &other)
  : map_region (other)
  {}

  /* region vfuncs.  */
  region *clone () const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_FUNCTION; }

  /* map_region vfunc.  */
  bool valid_key_p (tree key) const FINAL OVERRIDE;

  region_id get_element (region_model *model,
			 region_id this_rid,
			 svalue_id index_sid,
			 region_model_context *ctxt);

  bool compare_fields (const function_region &other) const;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <function_region *>::test (region *reg)
{
  return reg->get_kind () == RK_FUNCTION;
}

namespace ana {

/* Concrete region subclass representing an array (or an array-like view
   of a parent region of memory.
   This can't be a map_region as we can't use trees as the keys: there's
   no guarantee about the uniqueness of an INTEGER_CST.  */

class array_region : public region
{
public:
#if 0
  wide_int m_test;

  typedef wide_int key_t;
  typedef int_hash <wide_int, -1, -2> hash_t;
  typedef ordered_hash_map<hash_t, region_id> map_t;
#else
  typedef int key_t;
  typedef int_hash <int, -1, -2> int_hash_t;
  typedef ordered_hash_map<int_hash_t, region_id> map_t;
#endif
  typedef map_t::iterator iterator_t;

  array_region (region_id parent_rid, tree type)
  : region (parent_rid, svalue_id::null (), type)
  {
    gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
  }
  array_region (const array_region &other);

  void dump_dot_to_pp (const region_model &model,
		       region_id this_rid,
		       pretty_printer *pp) const
    FINAL OVERRIDE;

  void dump_child_label (const region_model &model,
			 region_id this_rid,
			 region_id child_rid,
			 pretty_printer *pp) const
    FINAL OVERRIDE;

  /* region vfuncs.  */
  region *clone () const FINAL OVERRIDE;
  enum region_kind get_kind () const FINAL OVERRIDE { return RK_ARRAY; }
  array_region *dyn_cast_array_region () { return this; }

  region_id get_element (region_model *model,
			 region_id this_rid,
			 svalue_id index_sid,
			 region_model_context *ctxt);

  bool compare_fields (const array_region &other) const;

  static bool can_merge_p (const array_region *array_region_a,
			   const array_region *array_region_b,
			   array_region *merged_array_region,
			   region_id merged_rid,
			   model_merger *merger);

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

  iterator_t begin () { return m_map.begin (); }
  iterator_t end () { return m_map.end (); }
  size_t elements () const { return m_map.elements (); }

  region_id get_or_create (region_model *model,
			   region_id this_rid,
			   key_t key, tree type,
			   region_model_context *ctxt);
//  void unbind (int expr);
  region_id *get (key_t key);

  void remap_region_ids (const region_id_map &map) FINAL OVERRIDE;

  bool get_key_for_child_region (region_id child_rid,
				 key_t *out) const;

#if 0
  bool get_key_for_child_region (region *child,
				 const region_model &model,
				 key_t *out) const;
#endif

  void add_to_hash (inchash::hash &hstate) const OVERRIDE;
  void print_fields (const region_model &model,
		     region_id this_rid,
		     pretty_printer *pp) const
    OVERRIDE;
  void validate (const region_model &model) const FINAL OVERRIDE;

  static key_t key_from_constant (tree cst);
  tree constant_from_key (key_t key);

 private:
  static int key_cmp (const void *, const void *);

  /* Mapping from tree to child region.  */
  map_t m_map;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <array_region *>::test (region *reg)
{
  return reg->get_kind () == RK_ARRAY;
}

namespace ana {

/* Concrete region subclass representing a stack, containing all stack
   frames, and implicitly providing a POISON_KIND_UNINIT value to all
   child regions by default.  */

class stack_region : public region
{
public:
  stack_region (region_id parent_rid, svalue_id sval_id)
  : region (parent_rid, sval_id, NULL_TREE)
  {}

  stack_region (const stack_region &other);

  bool compare_fields (const stack_region &other) const;

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_STACK; }

  void dump_child_label (const region_model &model,
			 region_id this_rid,
			 region_id child_rid,
			 pretty_printer *pp) const
    FINAL OVERRIDE;

  void push_frame (region_id frame_rid);
  region_id get_current_frame_id () const;
  void pop_frame (region_model *model, region_id result_dst_rid,
		  bool purge, purge_stats *stats,
		  region_model_context *ctxt);

  void remap_region_ids (const region_id_map &map) FINAL OVERRIDE;

  unsigned get_num_frames () const { return m_frame_rids.length (); }
  region_id get_frame_rid (unsigned i) const { return m_frame_rids[i]; }

  static bool can_merge_p (const stack_region *stack_region_a,
			   const stack_region *stack_region_b,
			   model_merger *merger);

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

  svalue_id get_value_by_name (tree identifier,
			       const region_model &model) const;

  void validate (const region_model &model) const FINAL OVERRIDE;

 private:
  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;
  void print_fields (const region_model &model,
		     region_id this_rid,
		     pretty_printer *pp) const
    FINAL OVERRIDE;

  auto_vec<region_id> m_frame_rids;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <stack_region *>::test (region *reg)
{
  return reg->get_kind () == RK_STACK;
}

namespace ana {

/* Concrete region subclass: a region within which regions can be
   dynamically allocated.  */

class heap_region : public region
{
public:
  heap_region (region_id parent_rid, svalue_id sval_id)
  : region (parent_rid, sval_id, NULL_TREE)
  {}
  heap_region (const heap_region &other);

  bool compare_fields (const heap_region &other) const;

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_HEAP; }

  static bool can_merge_p (const heap_region *heap_a, region_id heap_a_rid,
			   const heap_region *heap_b, region_id heap_b_rid,
			   heap_region *merged_heap, region_id merged_heap_rid,
			   model_merger *merger);

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <heap_region *>::test (region *reg)
{
  return reg->get_kind () == RK_HEAP;
}

namespace ana {

/* Concrete region subclass.  The root region, containing all regions
   (either directly, or as descendents).
   Unique within a region_model.  */

class root_region : public region
{
public:
  root_region ();
  root_region (const root_region &other);

  bool compare_fields (const root_region &other) const;

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_ROOT; }

  void dump_child_label (const region_model &model,
			 region_id this_rid,
			 region_id child_rid,
			 pretty_printer *pp) const
    FINAL OVERRIDE;

  region_id push_frame (region_model *model, function *fun,
			vec<svalue_id> *arg_sids,
			region_model_context *ctxt);
  region_id get_current_frame_id (const region_model &model) const;
  void pop_frame (region_model *model, region_id result_dst_rid,
		  bool purge, purge_stats *stats,
		  region_model_context *ctxt);

  region_id ensure_stack_region (region_model *model);
  region_id get_stack_region_id () const { return m_stack_rid; }
  stack_region *get_stack_region (const region_model *model) const;

  region_id ensure_globals_region (region_model *model);
  region_id get_globals_region_id () const { return m_globals_rid; }
  globals_region *get_globals_region (const region_model *model) const;

  region_id ensure_code_region (region_model *model);
  code_region *get_code_region (const region_model *model) const;

  region_id ensure_heap_region (region_model *model);
  heap_region *get_heap_region (const region_model *model) const;

  void remap_region_ids (const region_id_map &map) FINAL OVERRIDE;

  static bool can_merge_p (const root_region *root_region_a,
			   const root_region *root_region_b,
			   root_region *merged_root_region,
			   model_merger *merger);

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

  svalue_id get_value_by_name (tree identifier,
			       const region_model &model) const;

  void validate (const region_model &model) const FINAL OVERRIDE;

private:
  void add_to_hash (inchash::hash &hstate) const FINAL OVERRIDE;
  void print_fields (const region_model &model,
		     region_id this_rid,
		     pretty_printer *pp) const
    FINAL OVERRIDE;

  region_id m_stack_rid;
  region_id m_globals_rid;
  region_id m_code_rid;
  region_id m_heap_rid;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <root_region *>::test (region *reg)
{
  return reg->get_kind () == RK_ROOT;
}

namespace ana {

/* Concrete region subclass: a region to use when dereferencing an unknown
   pointer.  */

class symbolic_region : public region
{
public:
  symbolic_region (region_id parent_rid, tree type, bool possibly_null)
  : region (parent_rid, svalue_id::null (), type),
    m_possibly_null (possibly_null)
  {}
  symbolic_region (const symbolic_region &other);

  const symbolic_region *dyn_cast_symbolic_region () const FINAL OVERRIDE
  { return this; }
  symbolic_region *dyn_cast_symbolic_region () FINAL OVERRIDE
  { return this; }

  bool compare_fields (const symbolic_region &other) const;

  region *clone () const FINAL OVERRIDE;

  enum region_kind get_kind () const FINAL OVERRIDE { return RK_SYMBOLIC; }

  void walk_for_canonicalization (canonicalization *c) const FINAL OVERRIDE;

  void print_fields (const region_model &model,
		     region_id this_rid,
		     pretty_printer *pp) const FINAL OVERRIDE;

  bool m_possibly_null;
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
  region_model ();
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

  void print_svalue (svalue_id sid, pretty_printer *pp) const;

  void dump_dot_to_pp (pretty_printer *pp) const;
  void dump_dot_to_file (FILE *fp) const;
  void dump_dot (const char *path) const;

  void dump_to_pp (pretty_printer *pp, bool summarize) const;
  void dump (FILE *fp, bool summarize) const;
  void dump (bool summarize) const;

  void debug () const;

  void validate () const;

  void canonicalize (region_model_context *ctxt);
  bool canonicalized_p () const;

  void check_for_poison (tree expr, region_model_context *ctxt);
  void on_assignment (const gassign *stmt, region_model_context *ctxt);
  bool on_call_pre (const gcall *stmt, region_model_context *ctxt);
  void on_call_post (const gcall *stmt,
		     bool unknown_side_effects,
		     region_model_context *ctxt);
  void handle_unrecognized_call (const gcall *call,
				 region_model_context *ctxt);
  void on_return (const greturn *stmt, region_model_context *ctxt);
  void on_setjmp (const gcall *stmt, const exploded_node *enode,
		  region_model_context *ctxt);
  void on_longjmp (const gcall *longjmp_call, const gcall *setjmp_call,
		   int setjmp_stack_depth, region_model_context *ctxt);

  void update_for_phis (const supernode *snode,
			const cfg_superedge *last_cfg_superedge,
			region_model_context *ctxt);

  void handle_phi (const gphi *phi,
		   tree lhs, tree rhs, bool is_back_edge,
		   region_model_context *ctxt);

  bool maybe_update_for_edge (const superedge &edge,
			      const gimple *last_stmt,
			      region_model_context *ctxt);

  region_id get_root_rid () const { return m_root_rid; }
  root_region *get_root_region () const;

  region_id get_stack_region_id () const;
  region_id push_frame (function *fun, vec<svalue_id> *arg_sids,
			region_model_context *ctxt);
  region_id get_current_frame_id () const;
  function * get_current_function () const;
  void pop_frame (region_id result_dst_rid,
		  bool purge, purge_stats *stats,
		  region_model_context *ctxt);
  int get_stack_depth () const;
  function *get_function_at_depth (unsigned depth) const;

  region_id get_globals_region_id () const;

  svalue_id add_svalue (svalue *sval);
  void replace_svalue (svalue_id sid, svalue *new_sval);

  region_id add_region (region *r);

  region_id add_region_for_type (region_id parent_rid, tree type,
				 region_model_context *ctxt);

  svalue *get_svalue (svalue_id sval_id) const;
  region *get_region (region_id rid) const;

  template <typename Subclass>
  Subclass *get_region (region_id rid) const
  {
    region *result = get_region (rid);
    if (result)
      gcc_assert (is_a<Subclass *> (result));
    return (Subclass *)result;
  }

  region_id get_lvalue (path_var pv, region_model_context *ctxt);
  region_id get_lvalue (tree expr, region_model_context *ctxt);
  svalue_id get_rvalue (path_var pv, region_model_context *ctxt);
  svalue_id get_rvalue (tree expr, region_model_context *ctxt);

  svalue_id get_or_create_ptr_svalue (tree ptr_type, region_id id);
  svalue_id get_or_create_constant_svalue (tree cst_expr);
  svalue_id get_svalue_for_fndecl (tree ptr_type, tree fndecl,
				   region_model_context *ctxt);
  svalue_id get_svalue_for_label (tree ptr_type, tree label,
				  region_model_context *ctxt);

  region_id get_region_for_fndecl (tree fndecl, region_model_context *ctxt);
  region_id get_region_for_label (tree label, region_model_context *ctxt);

  svalue_id maybe_cast (tree type, svalue_id sid, region_model_context *ctxt);
  svalue_id maybe_cast_1 (tree type, svalue_id sid);

  region_id get_field_region (region_id rid, tree field,
			      region_model_context *ctxt);

  region_id deref_rvalue (svalue_id ptr_sid, region_model_context *ctxt);
  region_id deref_rvalue (tree ptr, region_model_context *ctxt);

  void set_value (region_id lhs_rid, svalue_id rhs_sid,
		  region_model_context *ctxt);
  void set_value (tree lhs, tree rhs, region_model_context *ctxt);
  svalue_id set_to_new_unknown_value (region_id dst_rid, tree type,
				      region_model_context *ctxt);

  void copy_region (region_id dst_rid, region_id src_rid,
		    region_model_context *ctxt);

  tristate eval_condition (svalue_id lhs,
			   enum tree_code op,
			   svalue_id rhs) const;
  tristate eval_condition_without_cm (svalue_id lhs,
				      enum tree_code op,
				      svalue_id rhs) const;
  tristate eval_condition (tree lhs,
			   enum tree_code op,
			   tree rhs,
			   region_model_context *ctxt);
  bool add_constraint (tree lhs, enum tree_code op, tree rhs,
		       region_model_context *ctxt);

  tree maybe_get_constant (svalue_id sid) const;

  region_id add_new_malloc_region ();

  tree get_representative_tree (svalue_id sid) const;
  path_var get_representative_path_var (region_id rid) const;
  void get_path_vars_for_svalue (svalue_id sid, vec<path_var> *out) const;

  void purge_unused_svalues (purge_stats *out,
			     region_model_context *ctxt,
			     svalue_id_set *known_used_sids = NULL);
  void remap_svalue_ids (const svalue_id_map &map);
  void remap_region_ids (const region_id_map &map);

  void purge_regions (const region_id_set &set,
		      purge_stats *stats,
		      logger *logger);

  unsigned get_num_svalues () const { return m_svalues.length (); }
  unsigned get_num_regions () const { return m_regions.length (); }

  /* For selftests.  */
  constraint_manager *get_constraints ()
  {
    return m_constraints;
  }

  void get_descendents (region_id rid, region_id_set *out,
			region_id exclude_rid) const;

  void delete_region_and_descendents (region_id rid,
				      enum poison_kind pkind,
				      purge_stats *stats,
				      logger *logger);

  bool can_merge_with_p (const region_model &other_model,
			 region_model *out_model,
			 svalue_id_merger_mapping *out) const;
  bool can_merge_with_p (const region_model &other_model,
			 region_model *out_model) const;

  svalue_id get_value_by_name (const char *name) const;

  svalue_id convert_byte_offset_to_array_index (tree ptr_type,
						svalue_id offset_sid);

  region_id get_or_create_mem_ref (tree type,
				   svalue_id ptr_sid,
				   svalue_id offset_sid,
				   region_model_context *ctxt);
  region_id get_or_create_pointer_plus_expr (tree type,
					     svalue_id ptr_sid,
					     svalue_id offset_sid,
					     region_model_context *ctxt);
  region_id get_or_create_view (region_id raw_rid, tree type,
				region_model_context *ctxt);

  tree get_fndecl_for_call (const gcall *call,
			    region_model_context *ctxt);

 private:
  region_id get_lvalue_1 (path_var pv, region_model_context *ctxt);
  svalue_id get_rvalue_1 (path_var pv, region_model_context *ctxt);

  void copy_struct_region (region_id dst_rid, struct_region *dst_reg,
			   struct_region *src_reg, region_model_context *ctxt);
  void copy_union_region (region_id dst_rid, union_region *src_reg,
			  region_model_context *ctxt);
  void copy_array_region (region_id dst_rid, array_region *dst_reg,
			  array_region *src_reg, region_model_context *ctxt);

  region_id make_region_for_unexpected_tree_code (region_model_context *ctxt,
						  tree t,
						  const dump_location_t &loc);

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

  void poison_any_pointers_to_bad_regions (const region_id_set &bad_regions,
					   enum poison_kind pkind);

  void dump_summary_of_rep_path_vars (pretty_printer *pp,
				      auto_vec<path_var> *rep_path_vars,
				      bool *is_first);

  auto_delete_vec<svalue> m_svalues;
  auto_delete_vec<region> m_regions;
  region_id m_root_rid;
  constraint_manager *m_constraints; // TODO: embed, rather than dynalloc?
};

/* Some region_model activity could lead to warnings (e.g. attempts to use an
   uninitialized value).  This abstract base class encapsulates an interface
   for the region model to use when emitting such warnings.

   It also provides an interface for being notified about svalue_ids being
   remapped, and being deleted.

   Having this as an abstract base class allows us to support the various
   operations needed by program_state in the analyzer within region_model,
   whilst keeping them somewhat modularized.  */

class region_model_context
{
 public:
  virtual void warn (pending_diagnostic *d) = 0;

  /* Hook for clients that store svalue_id instances, so that they
     can remap their IDs when the underlying region_model renumbers
     the IDs.  */
  virtual void remap_svalue_ids (const svalue_id_map &map) = 0;

#if 0
  /* Return true if if's OK to purge SID when simplifying state.
     Subclasses can return false for values that have sm state,
     to avoid generating "leak" false positives.  */
  virtual bool can_purge_p (svalue_id sid) = 0;
#endif

  /* Hook for clients to be notified when a range of SIDs have
     been purged, so that they can purge state relating to those
     values (and potentially emit warnings about leaks).
     All SIDs from FIRST_PURGED_SID numerically upwards are being
     purged.
     The return values is a count of how many items of data the client
     has purged (potentially for use in selftests).
     MAP has already been applied to the IDs, but is provided in case
     the client needs to figure out the old IDs.  */
  virtual int on_svalue_purge (svalue_id first_purged_sid,
			       const svalue_id_map &map) = 0;

  virtual logger *get_logger () = 0;

  /* Hook for clients to be notified when CHILD_SID is created
     from PARENT_SID, when "inheriting" a value for a region from a
     parent region.
     This exists so that state machines that inherit state can
     propagate the state from parent to child.  */
  virtual void on_inherited_svalue (svalue_id parent_sid,
				    svalue_id child_sid) = 0;

  /* Hook for clients to be notified when DST_SID is created
     (or reused) as a cast from SRC_SID.
     This exists so that state machines can propagate the state
     from SRC_SID to DST_SID.  */
  virtual void on_cast (svalue_id src_sid,
			svalue_id dst_sid) = 0;

  /* Hook for clients to be notified when the condition
     "LHS OP RHS" is added to the region model.
     This exists so that state machines can detect tests on edges,
     and use them to trigger sm-state transitions (e.g. transitions due
     to ptrs becoming known to be NULL or non-NULL, rather than just
     "unchecked") */
  virtual void on_condition (tree lhs, enum tree_code op, tree rhs) = 0;

  /* Hooks for clients to be notified when an unknown change happens
     to SID (in response to a call to an unknown function).  */
  virtual void on_unknown_change (svalue_id sid) = 0;

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
  void remap_svalue_ids (const svalue_id_map &) OVERRIDE {}
  int on_svalue_purge (svalue_id, const svalue_id_map &) OVERRIDE
  {
    return 0;
  }
  logger *get_logger () OVERRIDE { return NULL; }
  void on_inherited_svalue (svalue_id parent_sid ATTRIBUTE_UNUSED,
			    svalue_id child_sid  ATTRIBUTE_UNUSED)
    OVERRIDE
  {
  }
  void on_cast (svalue_id src_sid ATTRIBUTE_UNUSED,
		svalue_id dst_sid ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_condition (tree lhs ATTRIBUTE_UNUSED,
		     enum tree_code op ATTRIBUTE_UNUSED,
		     tree rhs ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_unknown_change (svalue_id sid ATTRIBUTE_UNUSED) OVERRIDE
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
		region_model *merged_model,
		svalue_id_merger_mapping *sid_mapping)
  : m_model_a (model_a), m_model_b (model_b),
    m_merged_model (merged_model),
    m_map_regions_from_a_to_m (model_a->get_num_regions ()),
    m_map_regions_from_b_to_m (model_b->get_num_regions ()),
    m_sid_mapping (sid_mapping)
  {
    gcc_assert (sid_mapping);
  }

  void dump_to_pp (pretty_printer *pp) const;
  void dump (FILE *fp) const;
  void dump () const;

  template <typename Subclass>
  Subclass *get_region_a (region_id rid_a) const
  {
    return m_model_a->get_region <Subclass> (rid_a);
  }

  template <typename Subclass>
  Subclass *get_region_b (region_id rid_b) const
  {
    return m_model_b->get_region <Subclass> (rid_b);
  }

  bool can_merge_values_p (svalue_id sid_a,
			   svalue_id sid_b,
			   svalue_id *merged_sid);

  void record_regions (region_id a_rid,
		       region_id b_rid,
		       region_id merged_rid);

  void record_svalues (svalue_id a_sid,
		       svalue_id b_sid,
		       svalue_id merged_sid);

  const region_model *m_model_a;
  const region_model *m_model_b;
  region_model *m_merged_model;

  one_way_region_id_map m_map_regions_from_a_to_m;
  one_way_region_id_map m_map_regions_from_b_to_m;
  svalue_id_merger_mapping *m_sid_mapping;
};

/* A bundle of data that can be optionally generated during merger of two
   region_models that describes how svalue_ids in each of the two inputs
   are mapped to svalue_ids in the merged output.

   For use when merging sm-states within program_state.  */

struct svalue_id_merger_mapping
{
  svalue_id_merger_mapping (const region_model &a,
			    const region_model &b);

  void dump_to_pp (pretty_printer *pp) const;
  void dump (FILE *fp) const;
  void dump () const;

  one_way_svalue_id_map m_map_from_a_to_m;
  one_way_svalue_id_map m_map_from_b_to_m;
};

/* A bundle of data used when canonicalizing a region_model so that the
   order of regions and svalues is in a predictable order (thus increasing
   the chance of two region_models being equal).

   This object is used to keep track of a recursive traversal across the
   svalues and regions within the model, made in a deterministic order,
   assigning new ids the first time each region or svalue is
   encountered.  */

struct canonicalization
{
  canonicalization (const region_model &model);
  void walk_rid (region_id rid);
  void walk_sid (svalue_id sid);

  void dump_to_pp (pretty_printer *pp) const;
  void dump (FILE *fp) const;
  void dump () const;

  const region_model &m_model;
  /* Maps from existing IDs to new IDs.  */
  region_id_map m_rid_map;
  svalue_id_map m_sid_map;
  /* The next IDs to hand out.  */
  int m_next_rid_int;
  int m_next_sid_int;
};

} // namespace ana

namespace inchash
{
  extern void add (svalue_id sid, hash &hstate);
  extern void add (region_id rid, hash &hstate);
} // namespace inchash

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
		    t ? get_tree_code_name (TREE_CODE (t)) : "(null)");
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
