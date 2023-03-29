/* Support for simple predicate analysis.

   Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by Martin Sebor <msebor@redhat.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GIMPLE_PREDICATE_ANALYSIS_H_INCLUDED
#define GIMPLE_PREDICATE_ANALYSIS_H_INCLUDED


/* Represents a simple Boolean predicate.  */
struct pred_info
{
  tree pred_lhs;
  tree pred_rhs;
  enum tree_code cond_code;
  bool invert;
};

/* The type to represent a sequence of predicates grouped
   with .AND. operation.  */
typedef vec<pred_info, va_heap, vl_ptr> pred_chain;

/* The type to represent a sequence of pred_chains grouped
   with .OR. operation.  */
typedef vec<pred_chain, va_heap, vl_ptr> pred_chain_union;

/* Represents a complex Boolean predicate expression.  */
class predicate
{
 public:
  /* Construct with the specified EVAL object.  */
  predicate (bool empty_val) : m_preds (vNULL), m_cval (empty_val) { }

  /* Copy.  */
  predicate (const predicate &rhs) : m_preds (vNULL) { *this = rhs; }

  ~predicate ();

  /* Assign.  */
  predicate& operator= (const predicate &);

  bool is_empty () const
  {
    return m_preds.is_empty ();
  }

  bool is_true () const
  {
    return is_empty () && m_cval;
  }

  bool is_false () const
  {
    return is_empty () && !m_cval;
  }

  bool empty_val () const
  {
    return m_cval;
  }

  const pred_chain_union chain () const
  {
    return m_preds;
  }

  void init_from_control_deps (const vec<edge> *, unsigned, bool);

  void dump (FILE *) const;
  void dump (FILE *, gimple *, const char *) const;
  void debug () const;

  void normalize (gimple * = NULL, bool = false);
  void simplify (gimple * = NULL, bool = false);

  bool superset_of (const predicate &) const;

private:

  bool includes (const pred_chain &) const;
  void push_pred (const pred_info &);

  /* Normalization functions.  */
  void normalize (pred_chain *, pred_info, tree_code, pred_chain *,
		  hash_set<tree> *);
  void normalize (const pred_info &);
  void normalize (const pred_chain &);

  /* Simplification functions.  */
  bool simplify_2 ();
  bool simplify_3 ();
  bool simplify_4 ();

  /* Representation of the predicate expression(s).  The predicate is
     m_cval || m_preds[0] || ...  */
  pred_chain_union m_preds;
  bool m_cval;
};

/* Represents a complex Boolean predicate expression.  */
class uninit_analysis
{
 public:
  /* Base function object type used to determine whether an expression
     is of interest.  */
  struct func_t
  {
    typedef unsigned phi_arg_set_t;

    /* Return a bitset of PHI arguments of interest.  By default returns
       bitset with a bit set for each argument.  Should be called in
       the overriden function first and, if nonzero, the result then
       refined as appropriate.  */
    virtual phi_arg_set_t phi_arg_set (gphi *);

    /* Maximum number of PHI arguments supported by phi_arg_set().  */
    static constexpr unsigned max_phi_args =
      sizeof (phi_arg_set_t) * CHAR_BIT;
  };

  /* Construct with the specified EVAL object.  */
  uninit_analysis (func_t &eval)
    : m_phi_def_preds (false), m_eval (eval) { }

  /* Copy.  */
  uninit_analysis (const uninit_analysis &rhs) = delete;

  /* Assign.  */
  uninit_analysis& operator= (const uninit_analysis&) = delete;

  /* Return true if the use by a statement in the basic block of
     a PHI operand is ruled out (i.e., guarded) by *THIS.  */
  bool is_use_guarded (gimple *, basic_block, gphi *, unsigned);

private:
  bool is_use_guarded (gimple *, basic_block, gphi *, unsigned,
		       hash_set<gphi *> *);
  bool prune_phi_opnds (gphi *, unsigned, gphi *, tree, tree_code,
			hash_set<gphi *> *, bitmap *);
  bool overlap (gphi *, unsigned, hash_set<gphi *> *, const predicate &);

  void collect_phi_def_edges (gphi *, basic_block, vec<edge> *,
			      hash_set<gimple *> *);
  bool init_from_phi_def (gphi *);
  bool init_use_preds (predicate &, basic_block, basic_block);


  /* Representation of the predicate expression(s).  */
  predicate m_phi_def_preds;
  /* Callback to evaluate an operand.  Return true if it's interesting.  */
  func_t &m_eval;
};

/* Bit mask handling macros.  */
#define MASK_SET_BIT(mask, pos) mask |= (1 << pos)
#define MASK_TEST_BIT(mask, pos) (mask & (1 << pos))
#define MASK_EMPTY(mask) (mask == 0)

#endif // GIMPLE_PREDICATE_ANALYSIS_H_INCLUDED
