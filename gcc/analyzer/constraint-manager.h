/* Tracking equivalence classes and constraints at a point on an execution path.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_CONSTRAINT_MANAGER_H
#define GCC_ANALYZER_CONSTRAINT_MANAGER_H

namespace ana {

class constraint_manager;

enum bound_kind
{
  BK_LOWER,
  BK_UPPER
};

/* One of the end-points of a range.  */

struct bound
{
  bound () : m_constant (NULL_TREE), m_closed (false) {}
  bound (tree constant, bool closed)
  : m_constant (constant), m_closed (closed) {}

  void ensure_closed (enum bound_kind bound_kind);

  const char * get_relation_as_str () const;

  tree m_constant;
  bool m_closed;
};

/* A range of values, used for determining if a value has been
   constrained to just one possible constant value.  */

class range
{
public:
  range () : m_lower_bound (), m_upper_bound () {}
  range (const bound &lower, const bound &upper)
  : m_lower_bound (lower), m_upper_bound (upper) {}

  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;

  tree constrained_to_single_element ();

  tristate eval_condition (enum tree_code op,
			   tree rhs_const) const;
  bool below_lower_bound (tree rhs_const) const;
  bool above_upper_bound (tree rhs_const) const;

  bool add_bound (bound b, enum bound_kind bound_kind);
  bool add_bound (enum tree_code op, tree rhs_const);

private:
  bound m_lower_bound;
  bound m_upper_bound;
};

/* A closed range of values with constant integer bounds
   e.g. [3, 5] for the set {3, 4, 5}.  */

struct bounded_range
{
  bounded_range (const_tree lower, const_tree upper);

  void dump_to_pp (pretty_printer *pp, bool show_types) const;
  void dump (bool show_types) const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::widget>
  make_dump_widget (const text_art::dump_widget_info &dwi) const;

  bool contains_p (tree cst) const;

  bool intersects_p (const bounded_range &other,
		     bounded_range *out) const;

  bool operator== (const bounded_range &other) const;
  bool operator!= (const bounded_range &other) const
  {
    return !(*this == other);
  }

  static int cmp (const bounded_range &a, const bounded_range &b);

  bool singleton_p () const
  {
    return tree_int_cst_equal (m_lower, m_upper);
  }

  tree m_lower;
  tree m_upper;

private:
  static void set_json_attr (json::object &obj, const char *name, tree value);
};

/* A collection of bounded_range instances, suitable
   for representing the ranges on a case label within a switch
   statement.  */

struct bounded_ranges
{
public:
  typedef bounded_ranges key_t;

  bounded_ranges (const bounded_range &range);
  bounded_ranges (const vec<bounded_range> &ranges);
  bounded_ranges (enum tree_code op, tree rhs_const);

  bool operator== (const bounded_ranges &other) const;

  hashval_t get_hash () const { return m_hash; }

  void dump_to_pp (pretty_printer *pp, bool show_types) const;
  void dump (bool show_types) const;

  std::unique_ptr<json::value> to_json () const;

  void add_to_dump_widget (text_art::tree_widget &parent,
			   const text_art::dump_widget_info &dwi) const;

  tristate eval_condition (enum tree_code op,
			   tree rhs_const,
			   bounded_ranges_manager *mgr) const;

  bool contain_p (tree cst) const;
  bool empty_p () const { return m_ranges.length () == 0; }

  static int cmp (const bounded_ranges *a, const bounded_ranges *b);

  unsigned get_count () const { return m_ranges.length (); }
  const bounded_range &get_range (unsigned idx) const { return m_ranges[idx]; }

private:
  void canonicalize ();
  void validate () const;

  friend class bounded_ranges_manager;

  auto_vec<bounded_range> m_ranges;
  hashval_t m_hash;
};

} // namespace ana

template <> struct default_hash_traits<bounded_ranges::key_t>
: public member_function_hash_traits<bounded_ranges::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* An object to own and consolidate bounded_ranges instances.
   This also caches the mapping from switch_cfg_superedge
   bounded_ranges instances, so that get_or_create_ranges_for_switch is
   memoized.  */

class bounded_ranges_manager
{
public:
  ~bounded_ranges_manager ();

  const bounded_ranges *
  get_or_create_ranges_for_switch (const switch_cfg_superedge *edge,
				   const gswitch *switch_stmt);

  const bounded_ranges *get_or_create_empty ();
  const bounded_ranges *get_or_create_point (const_tree value);
  const bounded_ranges *get_or_create_range (const_tree lower_bound,
					     const_tree upper_bound);
  const bounded_ranges *
  get_or_create_union (const vec <const bounded_ranges *> &others);
  const bounded_ranges *
  get_or_create_intersection (const bounded_ranges *a,
			      const bounded_ranges *b);
  const bounded_ranges *
  get_or_create_inverse (const bounded_ranges *other, tree type);

  void log_stats (logger *logger, bool show_objs) const;

private:
  const bounded_ranges *
  create_ranges_for_switch (const switch_cfg_superedge &edge,
			    const gswitch *switch_stmt);

  const bounded_ranges *
  make_case_label_ranges (const gswitch *switch_stmt,
			  tree case_label);

  const bounded_ranges *consolidate (bounded_ranges *);

  struct hash_traits_t : public typed_noop_remove<bounded_ranges *>
  {
    typedef bounded_ranges *key_type;
    typedef bounded_ranges *value_type;

    static inline bool
    equal (const key_type &k1, const key_type &k2)
    {
      return *k1 == *k2;
    }
    static inline hashval_t
    hash (const key_type &k)
    {
      return k->get_hash ();
    }
    static inline bool is_empty (key_type k) { return k == NULL; }
    static inline void mark_empty (key_type &k) { k = NULL; }
    static inline bool is_deleted (key_type k)
    {
      return k == reinterpret_cast<key_type> (1);
    }

    static const bool empty_zero_p = true;
  };
  struct traits_t : public simple_hashmap_traits<hash_traits_t,
						 bounded_ranges *>
  {
  };
  typedef hash_map<bounded_ranges *, bounded_ranges *, traits_t> map_t;
  map_t m_map;

  typedef hash_map<const switch_cfg_superedge *,
		   const bounded_ranges *> edge_cache_t;
  edge_cache_t m_edge_cache;
};

/* An equivalence class within a constraint manager: a set of
   svalues that are known to all be equal to each other,
   together with an optional tree constant that they are equal to.  */

class equiv_class
{
public:
  equiv_class ();
  equiv_class (const equiv_class &other);

  hashval_t hash () const;
  bool operator== (const equiv_class &other);

  void add (const svalue *sval);
  bool del (const svalue *sval);

  tree get_any_constant () const { return m_constant; }

  const svalue *get_representative () const;

  void canonicalize ();

  void print (pretty_printer *pp) const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi,
		    unsigned id) const;

  bool contains_non_constant_p () const;

  /* An equivalence class can contain multiple constants (e.g. multiple
     different zeroes, for different types); these are just for the last
     constant added.  */
  tree m_constant;
  const svalue *m_cst_sval;

  // TODO: should this be a set rather than a vec?
  auto_vec<const svalue *> m_vars;
};

/* The various kinds of constraint.  */

enum constraint_op
{
  CONSTRAINT_NE,
  CONSTRAINT_LT,
  CONSTRAINT_LE
};

const char *constraint_op_code (enum constraint_op c_op);

/* An ID for an equiv_class within a constraint_manager.  Internally, this
   is an index into a vector of equiv_class * within the constraint_manager.  */

class equiv_class_id
{
public:
  static equiv_class_id null () { return equiv_class_id (-1); }

  equiv_class_id (unsigned idx) : m_idx (idx) {}
  const equiv_class &get_obj (const constraint_manager &cm) const;
  equiv_class &get_obj (constraint_manager &cm) const;

  bool operator== (const equiv_class_id &other) const
  {
    return m_idx == other.m_idx;
  }
  bool operator!= (const equiv_class_id &other) const
  {
    return m_idx != other.m_idx;
  }

  bool null_p () const { return m_idx == -1; }

  static equiv_class_id from_int (int idx) { return equiv_class_id (idx); }
  int as_int () const { return m_idx; }

  void print (pretty_printer *pp) const;

  void update_for_removal (equiv_class_id other)
  {
    if (m_idx > other.m_idx)
      m_idx--;
  }

  int m_idx;
};

/* A relationship between two equivalence classes in a constraint_manager.  */

class constraint
{
 public:
  constraint (equiv_class_id lhs, enum constraint_op c_op, equiv_class_id rhs)
  : m_lhs (lhs), m_op (c_op), m_rhs (rhs)
  {
    gcc_assert (!lhs.null_p ());
    gcc_assert (!rhs.null_p ());
  }

  void print (pretty_printer *pp, const constraint_manager &cm) const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::widget>
  make_dump_widget (const text_art::dump_widget_info &dwi,
		    const constraint_manager &cm) const;

  hashval_t hash () const;
  bool operator== (const constraint &other) const;

  /* Is this an ordering, rather than a "!=".  */
  bool is_ordering_p () const
  {
    return m_op != CONSTRAINT_NE;
  }

  bool implied_by (const constraint &other,
		   const constraint_manager &cm) const;

  equiv_class_id m_lhs;
  enum constraint_op m_op;
  equiv_class_id m_rhs;
};

/* An abstract base class for use with constraint_manager::for_each_fact.  */

class fact_visitor
{
 public:
  virtual ~fact_visitor () {}
  virtual void on_fact (const svalue *lhs,
			enum tree_code,
			const svalue *rhs) = 0;
  virtual void on_ranges (const svalue *lhs,
			  const bounded_ranges *ranges) = 0;
};

class bounded_ranges_constraint
{
public:
  bounded_ranges_constraint (equiv_class_id ec_id,
			     const bounded_ranges *ranges)
  : m_ec_id (ec_id), m_ranges (ranges)
  {
  }

  void print (pretty_printer *pp, const constraint_manager &cm) const;

  std::unique_ptr<json::object> to_json () const;

  bool operator== (const bounded_ranges_constraint &other) const;
  bool operator!= (const bounded_ranges_constraint &other) const
  {
    return !(*this == other);
  }

  void add_to_hash (inchash::hash *hstate) const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi) const;

  equiv_class_id m_ec_id;
  const bounded_ranges *m_ranges;
};

/* A collection of equivalence classes and constraints on them.

   Given N svalues, this can be thought of as representing a subset of
   N-dimensional space.  When we call add_constraint,
   we are effectively taking an intersection with that constraint.  */

class constraint_manager
{
public:
  constraint_manager (region_model_manager *mgr) : m_mgr (mgr) {}
  constraint_manager (const constraint_manager &other);
  virtual ~constraint_manager () {}

  constraint_manager& operator= (const constraint_manager &other);

  hashval_t hash () const;
  bool operator== (const constraint_manager &other) const;
  bool operator!= (const constraint_manager &other) const
  {
    return !(*this == other);
  }

  void print (pretty_printer *pp) const;
  void dump_to_pp (pretty_printer *pp, bool multiline) const;
  void dump (FILE *fp) const;
  void dump () const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi) const;

  const equiv_class &get_equiv_class_by_index (unsigned idx) const
  {
    return *m_equiv_classes[idx];
  }
  equiv_class &get_equiv_class_by_index (unsigned idx)
  {
    return *m_equiv_classes[idx];
  }

  equiv_class &get_equiv_class (const svalue *sval)
  {
    equiv_class_id ec_id = get_or_add_equiv_class (sval);
    return ec_id.get_obj (*this);
  }

  bool add_constraint (const svalue *lhs,
		       enum tree_code op,
		       const svalue *rhs);

  bool add_constraint (equiv_class_id lhs_ec_id,
		       enum tree_code op,
		       equiv_class_id rhs_ec_id);

  void add_unknown_constraint (equiv_class_id lhs_ec_id,
			       enum tree_code op,
			       equiv_class_id rhs_ec_id);

  bool add_bounded_ranges (const svalue *sval,
			   const bounded_ranges *ranges);

  bool get_equiv_class_by_svalue (const svalue *sval,
				    equiv_class_id *out) const;
  bool sval_constrained_p (const svalue *sval) const;
  equiv_class_id get_or_add_equiv_class (const svalue *sval);
  tristate eval_condition (equiv_class_id lhs,
			   enum tree_code op,
			   equiv_class_id rhs) const;
  tristate eval_condition (equiv_class_id lhs_ec,
			   enum tree_code op,
			   tree rhs_const) const;
  tristate eval_condition (const svalue *lhs,
			   enum tree_code op,
			   const svalue *rhs) const;
  range get_ec_bounds (equiv_class_id ec_id) const;

  /* PurgeCriteria should have:
     bool should_purge_p (const svalue *sval) const.  */
  template <typename PurgeCriteria>
  void purge (const PurgeCriteria &p, purge_stats *stats);

  void on_liveness_change (const svalue_set &live_svalues,
			   const region_model *model);
  void purge_state_involving (const svalue *sval);

  void canonicalize ();

  static void merge (const constraint_manager &cm_a,
		     const constraint_manager &cm_b,
		     constraint_manager *out);

  void for_each_fact (fact_visitor *) const;

  void validate () const;

  bounded_ranges_manager *get_range_manager () const;

  bool replay_call_summary (call_summary_replay &r,
			    const constraint_manager &summary);

  auto_delete_vec<equiv_class> m_equiv_classes;
  auto_vec<constraint> m_constraints;
  auto_vec<bounded_ranges_constraint> m_bounded_ranges_constraints;

 private:
  void add_constraint_internal (equiv_class_id lhs_id,
				enum constraint_op c_op,
				equiv_class_id rhs_id);
  bool impossible_derived_conditions_p (const svalue *lhs,
					const svalue *rhs) const;

  region_model_manager *m_mgr;
};

} // namespace ana

#endif /* GCC_ANALYZER_CONSTRAINT_MANAGER_H */
