/* Derivation and subsumption rules for constraints.
   Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

namespace {

// Helper algorithms

// Increment iter distance(first, last) times.
template<typename I1, typename I2, typename I3>
  I1 next_by_distance (I1 iter, I2 first, I3 last)
  {
    for ( ; first != last; ++first, ++iter)
      ;
    return iter;
  }

/*---------------------------------------------------------------------------
                           Proof state
---------------------------------------------------------------------------*/

/* A term list is a list of atomic constraints. It is used
   to maintain the lists of assumptions and conclusions in a
   proof goal.

   Each term list maintains an iterator that refers to the current
   term. This can be used by various tactics to support iteration
   and stateful manipulation of the list. */
struct term_list : std::list<tree>
{
  term_list ();
  term_list (const term_list &x);
  term_list& operator= (const term_list &x);

  tree       current_term ()       { return *current; }
  const_tree current_term () const { return *current; }


  void insert (tree t);
  tree erase ();

  void start ();
  void next ();
  bool done() const;

  iterator current;
};

inline
term_list::term_list ()
  : std::list<tree> (), current (end ())
{ }

inline
term_list::term_list (const term_list &x)
  : std::list<tree> (x)
  , current (next_by_distance (begin (), x.begin (), x.current))
{ }

inline term_list&
term_list::operator= (const term_list &x)
{
  std::list<tree>::operator=(x);
  current = next_by_distance (begin (), x.begin (), x.current);
  return *this;
}

/* Try saving the term T into the list of terms. If
   T is already in the list of terms, then no action is
   performed. Otherwise, insert T before the current
   position, making this term current.

   Note that not inserting terms is an optimization
   that corresponds to the structural rule of
   contraction.

   NOTE: With the contraction rule, this data structure
   would be more efficiently represented as an ordered set
   or hash set.  */
void
term_list::insert (tree t)
{
  /* Search the current term list. If there is already
     a matching term, do not add the new one.  */
  for (iterator i = begin(); i != end(); ++i)
    if (cp_tree_equal (*i, t))
      return;

  current = std::list<tree>::insert (current, t);
}

/* Remove the current term from the list, repositioning to
   the term following the removed term. Note that the new
   position could be past the end of the list.

   The removed term is returned. */
inline tree
term_list::erase ()
{
  tree t = *current;
  current = std::list<tree>::erase (current);
  return t;
}

/* Initialize the current term to the first in the list. */
inline void
term_list::start ()
{
  current = begin ();
}

/* Advance to the next term in the list. */
inline void
term_list::next ()
{
  ++current;
}

/* Returns true when the current position is past the end. */
inline bool
term_list::done () const
{
  return current == end ();
}


/* A goal (or subgoal) models a sequent of the form
   'A |- C' where A and C are lists of assumptions and
   conclusions written as propositions in the constraint
   language (i.e., lists of trees).
*/
struct proof_goal
{
  term_list assumptions;
  term_list conclusions;
};

/* A proof state owns a list of goals and tracks the
   current sub-goal. The class also provides facilities
   for managing subgoals and constructing term lists. */
struct proof_state : std::list<proof_goal>
{
  proof_state ();

  iterator branch (iterator i);
};

/* An alias for proof state iterators. */
typedef proof_state::iterator goal_iterator;

/* Initialize the state with a single empty goal,
   and set that goal as the current subgoal. */
inline
proof_state::proof_state ()
  : std::list<proof_goal> (1)
{ }


/* Branch the current goal by creating a new subgoal,
   returning a reference to // the new object. This does
   not update the current goal. */
inline proof_state::iterator
proof_state::branch (iterator i)
{
  gcc_assert (i != end());
  proof_goal& g = *i;
  return insert (++i, g);
}

/*---------------------------------------------------------------------------
                           Logical rules
---------------------------------------------------------------------------*/

/*These functions modify the current state and goal by decomposing
  logical expressions using the logical rules of sequent calculus for
  first order logic.

  Note that in each decomposition rule, the term T has been erased
  from term list before the specific rule is applied. */

/* The left logical rule for conjunction adds both operands
   to the current set of constraints. */
void
left_conjunction (proof_state &, goal_iterator i, tree t)
{
  gcc_assert (TREE_CODE (t) == CONJ_CONSTR);

  /* Insert the operands into the current branch. Note that the
     final order of insertion is left-to-right. */
  term_list &l = i->assumptions;
  l.insert (TREE_OPERAND (t, 1));
  l.insert (TREE_OPERAND (t, 0));
}

/* The left logical rule for disjunction creates a new goal,
   adding the first operand to the original set of
   constraints and the second operand to the new set
   of constraints. */
void
left_disjunction (proof_state &s, goal_iterator i, tree t)
{
  gcc_assert (TREE_CODE (t) == DISJ_CONSTR);

  /* Branch the current subgoal. */
  goal_iterator j = s.branch (i);
  term_list &l1 = i->assumptions;
  term_list &l2 = j->assumptions;

  /* Insert operands into the different branches. */
  l1.insert (TREE_OPERAND (t, 0));
  l2.insert (TREE_OPERAND (t, 1));
}

/* The left logical rules for parameterized constraints
   adds its operand to the current goal. The list of
   parameters are effectively discarded. */
void
left_parameterized_constraint (proof_state &, goal_iterator i, tree t)
{
  gcc_assert (TREE_CODE (t) == PARM_CONSTR);
  term_list &l = i->assumptions;
  l.insert (PARM_CONSTR_OPERAND (t));
}

/*---------------------------------------------------------------------------
                           Decomposition
---------------------------------------------------------------------------*/

/* The following algorithms decompose expressions into sets of
   atomic propositions. In terms of the sequent calculus, these
   functions exercise the logical rules only.

   This is equivalent, for the purpose of determining subsumption,
   to rewriting a constraint in disjunctive normal form. It also
   allows the resulting assumptions to be used as declarations
   for the purpose of separate checking. */

/* Apply the left logical rules to the proof state. */
void
decompose_left_term (proof_state &s, goal_iterator i)
{
  term_list &l = i->assumptions;
  tree t = l.current_term ();
  switch (TREE_CODE (t))
    {
    case CONJ_CONSTR:
      left_conjunction (s, i, l.erase ());
      break;
    case DISJ_CONSTR:
      left_disjunction (s, i, l.erase ());
      break;
    case PARM_CONSTR:
      left_parameterized_constraint (s, i, l.erase ());
      break;
    default:
      l.next ();
      break;
    }
}

/* Apply the left logical rules of the sequent calculus
   until the current goal is fully decomposed into atomic
   constraints. */
void
decompose_left_goal (proof_state &s, goal_iterator i)
{
  term_list& l = i->assumptions;
  l.start ();
  while (!l.done ())
    decompose_left_term (s, i);
}

/* Apply the left logical rules of the sequent calculus
   until the antecedents are fully decomposed into atomic
   constraints. */
void
decompose_left (proof_state& s)
{
  goal_iterator iter = s.begin ();
  goal_iterator end = s.end ();
  for ( ; iter != end; ++iter)
    decompose_left_goal (s, iter);
}

/* Returns a vector of terms from the term list L. */
tree
extract_terms (term_list& l)
{
  tree result = make_tree_vec (l.size());
  term_list::iterator iter = l.begin();
  term_list::iterator end = l.end();
  for (int n = 0; iter != end; ++iter, ++n)
    TREE_VEC_ELT (result, n) = *iter;
  return result;
}

/* Extract the assumptions from the proof state S
   as a vector of vectors of atomic constraints. */
inline tree
extract_assumptions (proof_state& s)
{
  tree result = make_tree_vec (s.size ());
  goal_iterator iter = s.begin ();
  goal_iterator end = s.end ();
  for (int n = 0; iter != end; ++iter, ++n)
    TREE_VEC_ELT (result, n) = extract_terms (iter->assumptions);
  return result;
}

} // namespace

/* Decompose the required expression T into a constraint set: a
   vector of vectors containing only atomic propositions. If T is
   invalid, return an error. */
tree
decompose_assumptions (tree t)
{
  if (!t || t == error_mark_node)
    return t;

  /* Create a proof state, and insert T as the sole assumption. */
  proof_state s;
  term_list &l = s.begin ()->assumptions;
  l.insert (t);

  /* Decompose the expression into a constraint set, and then
     extract the terms for the AST. */
  decompose_left (s);
  return extract_assumptions (s);
}


/*---------------------------------------------------------------------------
                           Subsumption Rules
---------------------------------------------------------------------------*/

namespace {

bool subsumes_constraint (tree, tree);
bool subsumes_conjunction (tree, tree);
bool subsumes_disjunction (tree, tree);
bool subsumes_parameterized_constraint (tree, tree);
bool subsumes_atomic_constraint (tree, tree);

/* Returns true if the assumption A matches the conclusion C. This
   is generally the case when A and C have the same syntax.

   NOTE: There will be specialized matching rules to accommodate
   type equivalence, conversion, inheritance, etc. But this is not
   in the current concepts draft. */
inline bool
match_terms (tree a, tree c)
{
  return cp_tree_equal (a, c);
}

/* Returns true if the list of assumptions AS subsumes the atomic
   proposition C. This is the case when we can find a proposition
  in AS that entails the conclusion C. */
bool
subsumes_atomic_constraint (tree as, tree c)
{
  for (int i = 0; i < TREE_VEC_LENGTH (as); ++i)
    if (match_terms (TREE_VEC_ELT (as, i), c))
      return true;
  return false;
}

/* Returns true when both operands of C are subsumed by the
   assumptions AS. */
inline bool
subsumes_conjunction (tree as, tree c)
{
  tree l = TREE_OPERAND (c, 0);
  tree r = TREE_OPERAND (c, 1);
  return subsumes_constraint (as, l) && subsumes_constraint (as, r);
}

/* Returns true when either operand of C is subsumed by the
   assumptions AS. */
inline bool
subsumes_disjunction (tree as, tree c)
{
  tree l = TREE_OPERAND (c, 0);
  tree r = TREE_OPERAND (c, 1);
  return subsumes_constraint (as, l) || subsumes_constraint (as, r);
}

/* Returns true when the operand of C is subsumed by the
   assumptions in AS. The parameters are not considered in
   the subsumption rules. */
bool
subsumes_parameterized_constraint (tree as, tree c)
{
  tree t = PARM_CONSTR_OPERAND (c);
  return subsumes_constraint (as, t);
}


/* Returns true when the list of assumptions AS subsumes the
   concluded proposition C. This is a simple recursive descent
   on C, matching against propositions in the assumption list AS. */
bool
subsumes_constraint (tree as, tree c)
{
  switch (TREE_CODE (c))
    {
    case CONJ_CONSTR:
      return subsumes_conjunction (as, c);
    case DISJ_CONSTR:
      return subsumes_disjunction (as, c);
    case PARM_CONSTR:
      return subsumes_parameterized_constraint (as, c);
    default:
      return subsumes_atomic_constraint (as, c);
    }
}

/* Returns true if the LEFT constraints subsume the RIGHT constraints.
   This is done by checking that the RIGHT requirements follow from
   each of the LEFT subgoals. */
bool
subsumes_constraints_nonnull (tree left, tree right)
{
  gcc_assert (check_constraint_info (left));
  gcc_assert (check_constraint_info (right));

  /* Check that the required expression in RIGHT is subsumed by each
     subgoal in the assumptions of LEFT. */
  tree as = CI_ASSUMPTIONS (left);
  tree c = CI_NORMALIZED_CONSTRAINTS (right);
  for (int i = 0; i < TREE_VEC_LENGTH (as); ++i)
    if (!subsumes_constraint (TREE_VEC_ELT (as, i), c))
      return false;
  return true;
}

} /* namespace */

/* Returns true if the LEFT constraints subsume the RIGHT
   constraints. */
bool
subsumes (tree left, tree right)
{
  if (left == right)
    return true;
  if (!left)
    return false;
  if (!right)
    return true;
  return subsumes_constraints_nonnull (left, right);
}
