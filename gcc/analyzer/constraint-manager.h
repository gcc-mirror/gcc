/* Tracking equivalence classes and constraints at a point on an execution path.
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

#ifndef GCC_ANALYZER_CONSTRAINT_MANAGER_H
#define GCC_ANALYZER_CONSTRAINT_MANAGER_H

namespace ana {

class constraint_manager;

/* Abstract base class for specifying how state should be purged.  */

class purge_criteria
{
public:
  virtual ~purge_criteria () {}
  virtual bool should_purge_p (svalue_id sid) const = 0;
};

/* An equivalence class within a constraint manager: a set of
   svalue_ids that are known to all be equal to each other,
   together with an optional tree constant that they are equal to.  */

class equiv_class
{
public:
  equiv_class ();
  equiv_class (const equiv_class &other);

  hashval_t hash () const;
  bool operator== (const equiv_class &other);

  void add (svalue_id sid, const constraint_manager &cm);
  bool del (svalue_id sid);

  tree get_any_constant () const { return m_constant; }

  svalue_id get_representative () const;

  void remap_svalue_ids (const svalue_id_map &map);

  void canonicalize ();

  void print (pretty_printer *pp) const;

  /* An equivalence class can contain multiple constants (e.g. multiple
     different zeroes, for different types); these are just for the last
     constant added.  */
  tree m_constant;
  svalue_id m_cst_sid;

  // TODO: should this be a set rather than a vec?
  auto_vec<svalue_id> m_vars;
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

  hashval_t hash () const;
  bool operator== (const constraint &other) const;

  /* Is this an ordering, rather than a "!=".  */
  bool is_ordering_p () const
  {
    return m_op != CONSTRAINT_NE;
  }

  equiv_class_id m_lhs;
  enum constraint_op m_op;
  equiv_class_id m_rhs;
};

/* An abstract base class for use with constraint_manager::for_each_fact.  */

class fact_visitor
{
 public:
  virtual ~fact_visitor () {}
  virtual void on_fact (svalue_id lhs, enum tree_code, svalue_id rhs) = 0;
};

/* A collection of equivalence classes and constraints on them.

   Given N svalues, this can be thought of as representing a subset of
   N-dimensional space.  When we call add_constraint,
   we are effectively taking an intersection with that constraint.  */

class constraint_manager
{
public:
  constraint_manager () {}
  constraint_manager (const constraint_manager &other);
  virtual ~constraint_manager () {}

  virtual constraint_manager *clone (region_model *) const = 0;
  virtual tree maybe_get_constant (svalue_id sid) const = 0;
  virtual svalue_id get_sid_for_constant (tree cst) const = 0;
  virtual int get_num_svalues () const = 0;

  constraint_manager& operator= (const constraint_manager &other);

  hashval_t hash () const;
  bool operator== (const constraint_manager &other) const;
  bool operator!= (const constraint_manager &other) const
  {
    return !(*this == other);
  }

  void print (pretty_printer *pp) const;
  void dump_to_pp (pretty_printer *pp) const;
  void dump (FILE *fp) const;
  void dump () const;

  const equiv_class &get_equiv_class_by_index (unsigned idx) const
  {
    return *m_equiv_classes[idx];
  }
  equiv_class &get_equiv_class_by_index (unsigned idx)
  {
    return *m_equiv_classes[idx];
  }

  equiv_class &get_equiv_class (svalue_id sid)
  {
    equiv_class_id ec_id = get_or_add_equiv_class (sid);
    return ec_id.get_obj (*this);
  }

  bool add_constraint (svalue_id lhs, enum tree_code op, svalue_id rhs);

  bool add_constraint (equiv_class_id lhs_ec_id,
		       enum tree_code op,
		       equiv_class_id rhs_ec_id);

  bool get_equiv_class_by_sid (svalue_id sid, equiv_class_id *out) const;
  equiv_class_id get_or_add_equiv_class (svalue_id sid);
  tristate eval_condition (equiv_class_id lhs,
			   enum tree_code op,
			   equiv_class_id rhs);
  tristate eval_condition (svalue_id lhs,
			   enum tree_code op,
			   svalue_id rhs);

  void purge (const purge_criteria &p, purge_stats *stats);

  void remap_svalue_ids (const svalue_id_map &map);

  void canonicalize (unsigned num_svalue_ids);

  static void merge (const constraint_manager &cm_a,
		     const constraint_manager &cm_b,
		     constraint_manager *out,
		     const model_merger &merger);

  void for_each_fact (fact_visitor *) const;

  void validate () const;

  auto_delete_vec<equiv_class> m_equiv_classes;
  auto_vec<constraint> m_constraints;

 private:
  static void clean_merger_input (const constraint_manager &cm_in,
				  const one_way_svalue_id_map &map_sid_to_m,
				  constraint_manager *out);

  void add_constraint_internal (equiv_class_id lhs_id,
				enum constraint_op c_op,
				equiv_class_id rhs_id);
};

} // namespace ana

#endif /* GCC_ANALYZER_CONSTRAINT_MANAGER_H */
