/* Derivation and subsumption rules for constraints.
   Copyright (C) 2013-2019 Free Software Foundation, Inc.
   Contributed by Andrew Sutton (andrew.n.sutton@gmail.com)

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

#include "config.h"
#define INCLUDE_LIST
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "timevar.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "intl.h"
#include "flags.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "cp-objcp-common.h"
#include "tree-inline.h"
#include "decl.h"
#include "toplev.h"
#include "type-utils.h"

/* Hash functions for atomic constrains.  */

struct constraint_hash : default_hash_traits<tree>
{
  static hashval_t hash (tree t)
  {
    return hash_atomic_constraint (t);
  }

  static bool equal (tree t1, tree t2)
  {
    return atomic_constraints_identical_p (t1, t2);
  }
};

/* A conjunctive or disjunctive clause.

   Each clause maintains an iterator that refers to the current
   term, which is used in the linear decomposition of a formula
   into CNF or DNF.  */

struct clause
{
  typedef std::list<tree>::iterator iterator;
  typedef std::list<tree>::const_iterator const_iterator;

  /* Initialize a clause with an initial term.  */

  clause (tree t)
  {
    m_terms.push_back (t);
    if (TREE_CODE (t) == ATOMIC_CONSTR)
      m_set.add (t);

    m_current = m_terms.begin ();
  }

  /* Create a copy of the current term. The current
     iterator is set to point to the same position in the
     copied list of terms.  */

  clause (clause const& c)
    : m_terms (c.m_terms), m_set (c.m_set), m_current (m_terms.begin ())
  {
    std::advance (m_current, std::distance (c.begin (), c.current ()));
  }

  /* Returns true when all terms are atoms.  */

  bool done () const
  {
    return m_current == end ();
  }

  /* Advance to the next term.  */

  void advance ()
  {
    gcc_assert (!done ());
    ++m_current;
  }

  /* Replaces the current term at position ITER with T.  If
     T is an atomic constraint that already appears in the
     clause, remove but do not replace ITER. Returns a pair
     containing an iterator to the replace object or past
     the erased object and a boolean value which is true if
     an object was erased.  */

  std::pair<iterator, bool> replace (iterator iter, tree t)
  {
    gcc_assert (TREE_CODE (*iter) != ATOMIC_CONSTR);
    if (TREE_CODE (t) == ATOMIC_CONSTR)
      {
	if (m_set.add (t))
	  return std::make_pair (m_terms.erase (iter), true);
      }
    *iter = t;
    return std::make_pair (iter, false);
  }

  /* Inserts T before ITER in the list of terms.  If T has 
     already is an atomic constraint that already appears in
     the clause, no action is taken, and the current iterator
     is returned. Returns a pair of an iterator to the inserted
     object or ITER if no insertion occurred and a boolean
     value which is true if an object was inserted.  */

  std::pair<iterator, bool> insert (iterator iter, tree t)
  {
    if (TREE_CODE (t) == ATOMIC_CONSTR)
    {
      if (m_set.add (t))
	return std::make_pair (iter, false);
    }
    return std::make_pair (m_terms.insert (iter, t), true);
  }

  /* Replaces the current term with T. In the case where the
     current term is erased (because T is redundant), update
     the position of the current term to the next term.  */

  void replace (tree t)
  {
    m_current = replace (m_current, t).first;
  }

  /* Replace the current term with T1 and T2, in that order.  */

  void replace (tree t1, tree t2)
  {
    /* Replace the current term with t1. Ensure that iter points
       to the term before which t2 will be inserted.  Update the
       current term as needed.  */
    std::pair<iterator, bool> rep = replace (m_current, t1);
    if (rep.second)
      m_current = rep.first;
    else
      ++rep.first;

    /* Insert the t2. Make this the current term if we erased
       the prior term.  */
    std::pair<iterator, bool> ins = insert (rep.first, t2);
    if (rep.second && ins.second)
      m_current = ins.first;
  }

  /* Returns true if the clause contains the term T.  */

  bool contains (tree t)
  {
    gcc_assert (TREE_CODE (t) == ATOMIC_CONSTR);
    return m_set.contains (t);
  }


  /* Returns an iterator to the first clause in the formula.  */

  iterator begin ()
  {
    return m_terms.begin ();
  }

  /* Returns an iterator to the first clause in the formula.  */

  const_iterator begin () const
  {
    return m_terms.begin ();
  }

  /* Returns an iterator past the last clause in the formula.  */

  iterator end ()
  {
    return m_terms.end ();
  }

  /* Returns an iterator past the last clause in the formula.  */

  const_iterator end () const
  {
    return m_terms.end ();
  }

  /* Returns the current iterator.  */

  const_iterator current () const
  {
    return m_current;
  }

  std::list<tree> m_terms; /* The list of terms.  */
  hash_set<tree, false, constraint_hash> m_set; /* The set of atomic constraints.  */
  iterator m_current; /* The current term.  */
};


/* A proof state owns a list of goals and tracks the
   current sub-goal. The class also provides facilities
   for managing subgoals and constructing term lists. */

struct formula
{
  typedef std::list<clause>::iterator iterator;
  typedef std::list<clause>::const_iterator const_iterator;

  /* Construct a formula with an initial formula in a
     single clause.  */

  formula (tree t)
  {
    /* This should call emplace_back(). There's a an extra copy being
       invoked by using push_back().  */
    m_clauses.push_back (t);
    m_current = m_clauses.begin ();
  }

  /* Returns true when all clauses are atomic.  */
  bool done () const
  {
    return m_current == end ();
  }

  /* Advance to the next term.  */
  void advance ()
  {
    gcc_assert (!done ());
    ++m_current;
  }

  /* Insert a copy of clause into the formula. This corresponds
     to a distribution of one logical operation over the other.  */

  clause& branch ()
  {
    gcc_assert (!done ());
    m_clauses.push_back (*m_current);
    return m_clauses.back ();
  }

  /* Returns the position of the current clause.  */

  iterator current ()
  {
    return m_current;
  }

  /* Returns an iterator to the first clause in the formula.  */

  iterator begin ()
  {
    return m_clauses.begin ();
  }

  /* Returns an iterator to the first clause in the formula.  */

  const_iterator begin () const
  {
    return m_clauses.begin ();
  }

  /* Returns an iterator past the last clause in the formula.  */

  iterator end ()
  {
    return m_clauses.end ();
  }

  /* Returns an iterator past the last clause in the formula.  */

  const_iterator end () const
  {
    return m_clauses.end ();
  }

  std::list<clause> m_clauses; /* The list of clauses.  */
  iterator m_current; /* The current clause.  */
};

void
debug (clause& c)
{
  for (clause::iterator i = c.begin(); i != c.end(); ++i)
    verbatim ("  # %E", *i);
}

void
debug (formula& f)
{
  for (formula::iterator i = f.begin(); i != f.end(); ++i)
    {
      verbatim ("(((");
      debug (*i);
      verbatim (")))");
    }
}

/* The logical rules used to analyze a logical formula. The
   "left" and "right" refer to the position of formula in a
   sequent (as in sequent calculus).  */

enum rules
{
  left, right
};

/* Distribution counting.  */

static inline bool
disjunction_p (tree t)
{
  return TREE_CODE (t) == DISJ_CONSTR;
}

static inline bool
conjunction_p (tree t)
{
  return TREE_CODE (t) == CONJ_CONSTR;
}

static inline bool
atomic_p (tree t)
{
  return TREE_CODE (t) == ATOMIC_CONSTR;
}

/* Recursively count the number of clauses produced when converting T
   to DNF. Returns a pair containing the number of clauses and a bool
   value signifying that the the tree would be rewritten as a result of
   distributing. In general, a conjunction for which this flag is set
   is considered a disjunction for the purpose of counting.  */

static std::pair<int, bool>
dnf_size_r (tree t)
{
  if (atomic_p (t))
    /* Atomic constraints produce no clauses.  */
    return std::make_pair (0, false);

  /* For compound constraints, recursively count clauses and unpack
     the results.  */
  tree lhs = TREE_OPERAND (t, 0);
  tree rhs = TREE_OPERAND (t, 1);
  std::pair<int, bool> p1 = dnf_size_r (lhs);
  std::pair<int, bool> p2 = dnf_size_r (rhs);
  int n1 = p1.first, n2 = p2.first;
  bool d1 = p1.second, d2 = p2.second;

  if (disjunction_p (t))
    {
      /* Matches constraints of the form P \/ Q. Disjunctions contribute
	 linearly to the number of constraints.  When both P and Q are
	 disjunctions, clauses are added. When only one of P and Q
	 is a disjunction, an additional clause is produced. When neither
	 P nor Q are disjunctions, two clauses are produced.  */
      if (disjunction_p (lhs))
	{
	  if (disjunction_p (rhs) || (conjunction_p (rhs) && d2))
	    /* Both P and Q are disjunctions.  */
	    return std::make_pair (n1 + n2, d1 | d2);
	  else
	    /* Only LHS is a disjunction.  */
	    return std::make_pair (1 + n1 + n2, d1 | d2);
	  gcc_unreachable ();
	}
      if (conjunction_p (lhs))
	{
	  if ((disjunction_p (rhs) && d1) || (conjunction_p (rhs) && d1 && d2))
	    /* Both P and Q are disjunctions.  */
	    return std::make_pair (n1 + n2, d1 | d2);
	  if (disjunction_p (rhs)
	      || (conjunction_p (rhs) && d1 != d2)
	      || (atomic_p (rhs) && d1))
	    /* Either LHS or RHS is a disjunction.  */
	    return std::make_pair (1 + n1 + n2, d1 | d2);
	  else
	    /* Neither LHS nor RHS is a disjunction.  */
	    return std::make_pair (2, false);
	}
      if (atomic_p (lhs))
	{
	  if (disjunction_p (rhs) || (conjunction_p (rhs) && d2))
	    /* Only RHS is a disjunction.  */
	    return std::make_pair (1 + n1 + n2, d1 | d2);
	  else
	    /* Neither LHS nor RHS is a disjunction.  */
	    return std::make_pair (2, false);
	}
    }
  else /* conjunction_p (t)  */
    {
      /* Matches constraints of the form P /\ Q, possibly resulting
         in the distribution of one side over the other. When both
         P and Q are disjunctions, the number of clauses are multiplied.
         When only one of P and Q is a disjunction, the the number of
         clauses are added. Otherwise, neither side is a disjunction and
         no clauses are created.  */
      if (disjunction_p (lhs))
	{
	  if (disjunction_p (rhs) || (conjunction_p (rhs) && d2))
	    /* Both P and Q are disjunctions.  */
	    return std::make_pair (n1 * n2, true);
	  else
	    /* Only LHS is a disjunction.  */
	    return std::make_pair (n1 + n2, true);
	  gcc_unreachable ();
	}
      if (conjunction_p (lhs))
	{
	  if ((disjunction_p (rhs) && d1) || (conjunction_p (rhs) && d1 && d2))
	    /* Both P and Q are disjunctions.  */
	    return std::make_pair (n1 * n2, true);
	  if (disjunction_p (rhs)
	      || (conjunction_p (rhs) && d1 != d2)
	      || (atomic_p (rhs) && d1))
	    /* Either LHS or RHS is a disjunction.  */
	    return std::make_pair (n1 + n2, true);
	  else
	    /* Neither LHS nor RHS is a disjunction.  */
	    return std::make_pair (0, false);
	}
      if (atomic_p (lhs))
	{
	  if (disjunction_p (rhs) || (conjunction_p (rhs) && d2))
	    /* Only RHS is a disjunction.  */
	    return std::make_pair (n1 + n2, true);
	  else
	    /* Neither LHS nor RHS is a disjunction.  */
	    return std::make_pair (0, false);
	}
    }
  gcc_unreachable ();
}

/* Recursively count the number of clauses produced when converting T
   to CNF. Returns a pair containing the number of clauses and a bool
   value signifying that the the tree would be rewritten as a result of
   distributing. In general, a disjunction for which this flag is set
   is considered a conjunction for the purpose of counting.  */

static std::pair<int, bool>
cnf_size_r (tree t)
{
  if (atomic_p (t))
    /* Atomic constraints produce no clauses.  */
    return std::make_pair (0, false);

  /* For compound constraints, recursively count clauses and unpack
     the results.  */
  tree lhs = TREE_OPERAND (t, 0);
  tree rhs = TREE_OPERAND (t, 1);
  std::pair<int, bool> p1 = cnf_size_r (lhs);
  std::pair<int, bool> p2 = cnf_size_r (rhs);
  int n1 = p1.first, n2 = p2.first;
  bool d1 = p1.second, d2 = p2.second;

  if (disjunction_p (t))
    {
      /* Matches constraints of the form P \/ Q, possibly resulting
         in the distribution of one side over the other. When both
         P and Q are conjunctions, the number of clauses are multiplied.
         When only one of P and Q is a conjunction, the the number of
         clauses are added. Otherwise, neither side is a conjunction and
         no clauses are created.  */
      if (disjunction_p (lhs))
	{
	  if ((disjunction_p (rhs) && d1 && d2) || (conjunction_p (rhs) && d1))
	    /* Both P and Q are conjunctions.  */
	    return std::make_pair (n1 * n2, true);
	  if ((disjunction_p (rhs) && d1 != d2)
	      || conjunction_p (rhs)
	      || (atomic_p (rhs) && d1))
	    /* Either LHS or RHS is a conjunction.  */
	    return std::make_pair (n1 + n2, true);
	  else
	    /* Neither LHS nor RHS is a conjunction.  */
	    return std::make_pair (0, false);
	  gcc_unreachable ();
	}
      if (conjunction_p (lhs))
	{
	  if ((disjunction_p (rhs) && d2) || conjunction_p (rhs))
	    /* Both LHS and RHS are conjunctions.  */
	    return std::make_pair (n1 * n2, true);
	  else
	    /* Only LHS is a conjunction.  */
	    return std::make_pair (n1 + n2, true);
	}
      if (atomic_p (lhs))
	{
	  if ((disjunction_p (rhs) && d2) || conjunction_p (rhs))
	    /* Only RHS is a disjunction.  */
	    return std::make_pair (n1 + n2, true);
	  else
	    /* Neither LHS nor RHS is a disjunction.  */
	    return std::make_pair (0, false);
	}
    }
  else /* conjunction_p (t)  */
    {
      /* Matches constraints of the form P /\ Q. Conjunctions contribute
	 linearly to the number of constraints.  When both P and Q are
	 conjunctions, clauses are added. When only one of P and Q
	 is a conjunction, an additional clause is produced. When neither
	 P nor Q are conjunctions, two clauses are produced.  */
      if (disjunction_p (lhs))
	{
	  if ((disjunction_p (rhs) && d1 && d2) || (conjunction_p (rhs) && d1))
	    /* Both P and Q are conjunctions.  */
	    return std::make_pair (n1 + n2, d1 | d2);
	  if ((disjunction_p (rhs) && d1 != d2)
	      || conjunction_p (rhs)
	      || (atomic_p (rhs) && d1))
	    /* Either LHS or RHS is a conjunction.  */
	    return std::make_pair (1 + n1 + n2, d1 | d2);
	  else
	    /* Neither LHS nor RHS is a conjunction.  */
	    return std::make_pair (2, false);
	  gcc_unreachable ();
	}
      if (conjunction_p (lhs))
	{
	  if ((disjunction_p (rhs) && d2) || conjunction_p (rhs))
	    /* Both LHS and RHS are conjunctions.  */
	    return std::make_pair (n1 + n2, d1 | d2);
	  else
	    /* Only LHS is a conjunction.  */
	    return std::make_pair (1 + n1 + n2, d1 | d2);
	}
      if (atomic_p (lhs))
	{
	  if ((disjunction_p (rhs) && d2) || conjunction_p (rhs))
	    /* Only RHS is a disjunction.  */
	    return std::make_pair (1 + n1 + n2, d1 | d2);
	  else
	    /* Neither LHS nor RHS is a disjunction.  */
	    return std::make_pair (2, false);
	}
    }
  gcc_unreachable ();
}

/* Count the number conjunctive clauses that would be created
   when rewriting T to DNF. */

static int
dnf_size (tree t)
{
  std::pair<int, bool> result = dnf_size_r (t);
  return result.first == 0 ? 1 : result.first;
}


/* Count the number disjunctive clauses that would be created
   when rewriting T to CNF. */

static int
cnf_size (tree t)
{
  std::pair<int, bool> result = cnf_size_r (t);
  return result.first == 0 ? 1 : result.first;
}


/* A left-conjunction is replaced by its operands.  */

void
replace_term (clause& c, tree t)
{
  tree t1 = TREE_OPERAND (t, 0);
  tree t2 = TREE_OPERAND (t, 1);
  return c.replace (t1, t2);
}

/* Create a new clause in the formula by copying the current
   clause. In the current clause, the term at CI is replaced
   by the first operand, and in the new clause, it is replaced
   by the second.  */

void
branch_clause (formula& f, clause& c1, tree t)
{
  tree t1 = TREE_OPERAND (t, 0);
  tree t2 = TREE_OPERAND (t, 1);
  clause& c2 = f.branch ();
  c1.replace (t1);
  c2.replace (t2);
}

/* Decompose t1 /\ t2 according to the rules R.  */

inline void
decompose_conjuntion (formula& f, clause& c, tree t, rules r)
{
  if (r == left)
    replace_term (c, t);
  else
    branch_clause (f, c, t);
}

/* Decompose t1 \/ t2 according to the rules R.  */

inline void
decompose_disjunction (formula& f, clause& c, tree t, rules r)
{
  if (r == right)
    replace_term (c, t);
  else
    branch_clause (f, c, t);
}

/* An atomic constraint is already decomposed.  */
inline void
decompose_atom (clause& c)
{
  c.advance ();
}

/* Decompose a term of clause C (in formula F) according to the
   logical rules R. */

void
decompose_term (formula& f, clause& c, tree t, rules r)
{
  switch (TREE_CODE (t))
    {
      case CONJ_CONSTR:
        return decompose_conjuntion (f, c, t, r);
      case DISJ_CONSTR:
	return decompose_disjunction (f, c, t, r);
      default:
	return decompose_atom (c);
    }
}

/* Decompose C (in F) using the logical rules R until it
   is comprised of only atomic constraints.  */

void
decompose_clause (formula& f, clause& c, rules r)
{
  while (!c.done ())
    decompose_term (f, c, *c.current (), r);
  f.advance ();
}

/* Decompose the logical formula F according to the logical
   rules determined by R.  The result is a formula containing
   clauses that contain only atomic terms.  */

void
decompose_formula (formula& f, rules r)
{
  while (!f.done ())
    decompose_clause (f, *f.current (), r);
}

/* Fully decomposing T into a list of sequents, each comprised of
   a list of atomic constraints, as if T were an antecedent.  */

static formula
decompose_antecedents (tree t)
{
  formula f (t);
  decompose_formula (f, left);
  return f;
}

/* Fully decomposing T into a list of sequents, each comprised of
   a list of atomic constraints, as if T were a consequent.  */

static formula
decompose_consequents (tree t)
{
  formula f (t);
  decompose_formula (f, right);
  return f;
}

static bool derive_proof (clause&, tree, rules);

/* Derive a proof of both operands of T.  */

static bool
derive_proof_for_both_operands (clause& c, tree t, rules r)
{
  if (!derive_proof (c, TREE_OPERAND (t, 0), r))
    return false;
  return derive_proof (c, TREE_OPERAND (t, 1), r);
}

/* Derive a proof of either operand of T.  */

static bool
derive_proof_for_either_operand (clause& c, tree t, rules r)
{
  if (derive_proof (c, TREE_OPERAND (t, 0), r))
    return true;
  return derive_proof (c, TREE_OPERAND (t, 1), r);
}

/* Derive a proof of the atomic constraint T in clause C.  */

static bool
derive_atomic_proof (clause& c, tree t)
{
  return c.contains (t);
}

/* Derive a proof of T from the terms in C.  */

static bool
derive_proof (clause& c, tree t, rules r)
{
  switch (TREE_CODE (t))
  {
    case CONJ_CONSTR:
      if (r == left)
        return derive_proof_for_both_operands (c, t, r);
      else
	return derive_proof_for_either_operand (c, t, r);
    case DISJ_CONSTR:
      if (r == left)
        return derive_proof_for_either_operand (c, t, r);
      else
	return derive_proof_for_both_operands (c, t, r);
    default:
      return derive_atomic_proof (c, t);
  }
}

/* Derive a proof of T from disjunctive clauses in F.  */

static bool
derive_proofs (formula& f, tree t, rules r)
{
  for (formula::iterator i = f.begin(); i != f.end(); ++i)
    if (!derive_proof (*i, t, r))
      return false;
  return true;
}

/* The largest number of clauses in CNF or DNF we accept as input
   for subsumption. This an upper bound of 2^16 expressions.  */
static int max_problem_size = 16;

static inline bool
diagnose_constraint_size (tree t)
{
  error_at (input_location, "%qE exceeds the maximum constraint complexity", t);
  return false;
}

/* Key/value pair for caching subsumption results. This associates a pair of
   constraints with a boolean value indicating the result.  */

struct GTY((for_user)) subsumption_entry
{
  tree lhs;
  tree rhs;
  bool result;
};

/* Hashing function and equality for constraint entries.  */

struct subsumption_hasher : ggc_ptr_hash<subsumption_entry>
{
  static hashval_t hash (subsumption_entry *e)
  {
    hashval_t val = 0;
    val = iterative_hash_constraint (e->lhs, val);
    val = iterative_hash_constraint (e->rhs, val);
    return val;
  }

  static bool equal (subsumption_entry *e1, subsumption_entry *e2)
  {
    if (!constraints_equivalent_p (e1->lhs, e2->lhs))
      return false;
    if (!constraints_equivalent_p (e1->rhs, e2->rhs))
      return false;
    return true;
  }
};

/* Caches the results of subsumes_non_null(t1, t1).  */

static GTY ((deletable)) hash_table<subsumption_hasher> *subsumption_cache;

/* Search for a previously cached subsumption result. */

static bool*
lookup_subsumption (tree t1, tree t2)
{
  if (!subsumption_cache)
    return NULL;
  subsumption_entry elt = { t1, t2, false };
  subsumption_entry* found = subsumption_cache->find (&elt);
  if (found)
    return &found->result;
  else
    return 0;
}

/* Save a subsumption result. */

static bool
save_subsumption (tree t1, tree t2, bool result)
{
  if (!subsumption_cache)
    subsumption_cache = hash_table<subsumption_hasher>::create_ggc(31);
  subsumption_entry elt = {t1, t2, result};
  subsumption_entry** slot = subsumption_cache->find_slot (&elt, INSERT);
  subsumption_entry* entry = ggc_alloc<subsumption_entry> ();
  *entry = elt;
  *slot = entry;
  return result;
}


/* Returns true if the LEFT constraint subsume the RIGHT constraints.
   This is done by deriving a proof of the conclusions on the RIGHT
   from the assumptions on the LEFT assumptions.  */

static bool
subsumes_constraints_nonnull (tree lhs, tree rhs)
{
  auto_timevar time (TV_CONSTRAINT_SUB);

  if (bool *b = lookup_subsumption(lhs, rhs))
    return *b;

  int n1 = dnf_size (lhs);
  int n2 = cnf_size (rhs);

  /* Make sure we haven't exceeded the largest acceptable problem.  */
  if (std::min (n1, n2) >= max_problem_size)
    {
      if (n1 < n2)
        diagnose_constraint_size (lhs);
      else
	diagnose_constraint_size (rhs);
      return false;
    }

  /* Decompose the smaller of the two formulas, and recursively
     check for implication of the larger.  */
  bool result;
  if (n1 <= n2)
    {
      formula dnf = decompose_antecedents (lhs);
      result = derive_proofs (dnf, rhs, left);
    }
  else
    {
      formula cnf = decompose_consequents (rhs);
      result = derive_proofs (cnf, lhs, right);
    }

  return save_subsumption (lhs, rhs, result);
}

/* Returns true if the LEFT constraints subsume the RIGHT
   constraints.  */

bool
subsumes (tree lhs, tree rhs)
{
  if (lhs == rhs)
    return true;
  if (!lhs || lhs == error_mark_node)
    return false;
  if (!rhs || rhs == error_mark_node)
    return true;
  return subsumes_constraints_nonnull (lhs, rhs);
}

#include "gt-cp-logic.h"
