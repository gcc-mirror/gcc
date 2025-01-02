/* Header file for the GIMPLE fold_using_range interface.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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

#ifndef GCC_GIMPLE_RANGE_FOLD_H
#define GCC_GIMPLE_RANGE_FOLD_H

// This file is the main include point for gimple range folding.
// These routines will fold stmt S into the result range R.
// Any ssa_names on the stmt will be calculated using the range_query
// parameter via a call to range_of_expr.
// If no range_query is provided, current global range info will be used.
// The second variation specifies an edge, and stmt S is recalculated as if
// it appeared on that edge.

// Fold stmt S into range R using range query Q.
bool fold_range (vrange &r, gimple *s, range_query *q = NULL);
// Recalculate stmt S into R using range query Q as if it were on edge ON_EDGE.
bool fold_range (vrange &v, gimple *s, edge on_edge, range_query *q = NULL);

// These routines the operands to be specified when manually folding.
// Any excess queries will be drawn from the current range_query.
bool fold_range (vrange &r, gimple *s, vrange &r1, range_query *q = NULL);
bool fold_range (vrange &r, gimple *s, vrange &r1, vrange &r2,
		 range_query *q = NULL);
bool fold_range (vrange &r, gimple *s, unsigned num_elements, vrange **vector,
		 range_query *q = NULL);

// Calculate op1 on stmt S.
bool op1_range (vrange &, gimple *s, range_query *q = NULL);
bool op1_range (vrange &, gimple *s, const vrange &lhs, range_query *q = NULL);
// Calculate op2 on stmt S.
bool op2_range (vrange &, gimple *s, range_query *q = NULL);
bool op2_range (vrange &, gimple *s, const vrange &lhs, range_query *q = NULL);

// This routine will return a relation trio for stmt S.
relation_trio fold_relations (gimple *s, range_query *q = NULL);

// Return the type of range which statement S calculates.  If the type is
// unsupported or no type can be determined, return NULL_TREE.

inline tree
gimple_range_type (const gimple *s)
{
  tree lhs = gimple_get_lhs (s);
  tree type = NULL_TREE;
  if (lhs)
    type = TREE_TYPE (lhs);
  else
    {
      enum gimple_code code = gimple_code (s);
      if (code == GIMPLE_COND)
	type = boolean_type_node;
      else if (code == GIMPLE_PHI)
	type = TREE_TYPE (gimple_phi_result (s));
      else if (code == GIMPLE_CALL)
	{
	  type = gimple_call_fntype (s);
	  // If it has a type, get the return type.
	  if (type)
	    type = TREE_TYPE (type);
	}
    }
  if (type && value_range::supports_type_p (type))
    return type;
  return NULL_TREE;
}

// Return EXP if it is an SSA_NAME with a type supported by gimple ranges.

inline tree
gimple_range_ssa_p (tree exp)
{
  if (exp && TREE_CODE (exp) == SSA_NAME &&
      !SSA_NAME_IS_VIRTUAL_OPERAND (exp) &&
      !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (exp) &&
      value_range::supports_type_p (TREE_TYPE (exp)))
    return exp;
  return NULL_TREE;
}

// Source of all operands for fold_using_range and gori_compute.
// It abstracts out the source of an operand so it can come from a stmt or
// and edge or anywhere a derived class of fur_source wants.
// The default simply picks up ranges from the current range_query.

class fur_source
{
public:
  fur_source (range_query *q = NULL);
  inline range_query *query () const { return m_query; }
  inline gori_map *gori_ssa () const
    { return (m_depend_p && m_query) ? m_query->gori_ssa () : NULL; }
  inline class gimple_outgoing_range *gori ()
    { return m_depend_p ? &(m_query->gori ()) : NULL; }
  virtual bool get_operand (vrange &r, tree expr);
  virtual bool get_phi_operand (vrange &r, tree expr, edge e);
  virtual relation_kind query_relation (tree op1, tree op2);
  virtual void register_relation (gimple *stmt, relation_kind k, tree op1,
				  tree op2);
  virtual void register_relation (edge e, relation_kind k, tree op1,
				  tree op2);
  void register_outgoing_edges (gcond *, irange &lhs_range, edge e0, edge e1);
protected:
  range_query *m_query;
  bool m_depend_p;
};

// fur_stmt is the specification for drawing an operand from range_query Q
// via a range_of_Expr call on stmt S.

class fur_stmt : public fur_source
{
public:
  fur_stmt (gimple *s, range_query *q = NULL);
  virtual bool get_operand (vrange &r, tree expr) override;
  virtual bool get_phi_operand (vrange &r, tree expr, edge e) override;
  virtual relation_kind query_relation (tree op1, tree op2) override;
private:
  gimple *m_stmt;
};

// This version of fur_source will pick a range from a stmt, and also register
// dependencies via a gori_compute object.  This is mostly an internal API.

class fur_depend : public fur_stmt
{
public:
  fur_depend (gimple *s, range_query *q = NULL);
  virtual void register_relation (gimple *stmt, relation_kind k, tree op1,
				  tree op2) override;
  virtual void register_relation (edge e, relation_kind k, tree op1,
				  tree op2) override;
};


// This version of fur_source will pick a range up off an edge.

class fur_edge : public fur_source
{
public:
  fur_edge (edge e, range_query *q = NULL) : fur_source (q)
    { m_edge = e; }
  virtual bool get_operand (vrange &r, tree expr) override;
  virtual bool get_phi_operand (vrange &r, tree expr, edge e) override;
private:
  edge m_edge;
};

// This class uses ranges to fold a gimple statement producing a range for
// the LHS.  The source of all operands is supplied via the fur_source class
// which provides a range_query as well as a source location and any other
// required information.

class fold_using_range
{
public:
  bool fold_stmt (vrange &r, gimple *s, class fur_source &src,
		  tree name = NULL_TREE);
protected:
  bool range_of_range_op (vrange &r, gimple_range_op_handler &handler,
			  fur_source &src);
  bool range_of_call (vrange &r, gcall *call, fur_source &src);
  bool range_of_cond_expr (vrange &r, gassign* cond, fur_source &src);
  bool range_of_address (prange &r, gimple *s, fur_source &src);
  bool range_of_phi (vrange &r, gphi *phi, fur_source &src);
  void range_of_ssa_name_with_loop_info (vrange &, tree, class loop *, gphi *,
					 fur_source &src);
  void relation_fold_and_or (irange& lhs_range, gimple *s, fur_source &src,
			     vrange &op1, vrange &op2);
  bool condexpr_adjust (vrange &r1, vrange &r2, gimple *, tree cond, tree op1,
			tree op2, fur_source &src);
};
#endif // GCC_GIMPLE_RANGE_FOLD_H
