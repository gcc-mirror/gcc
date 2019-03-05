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

namespace {

// Helper algorithms

template<typename I>
inline I
next (I iter)
{
  return ++iter;
}

template<typename I, typename P>
inline bool
any_p (I first, I last, P pred)
{
  while (first != last)
    {
      if (pred(*first))
        return true;
      ++first;
    }
  return false;
}

bool prove_implication (tree, tree);

/*---------------------------------------------------------------------------
                           Proof state
---------------------------------------------------------------------------*/

struct term_entry
{
  tree t;
};

/* Hashing function and equality for constraint entries.  */

struct term_hasher : ggc_ptr_hash<term_entry>
{
  static hashval_t hash (term_entry *e)
  {
    return iterative_hash_template_arg (e->t, 0);
  }

  static bool equal (term_entry *e1, term_entry *e2)
  {
    return cp_tree_equal (e1->t, e2->t);
  }
};

/* A term list is a list of atomic constraints. It is used
   to maintain the lists of assumptions and conclusions in a
   proof goal.

   Each term list maintains an iterator that refers to the current
   term. This can be used by various tactics to support iteration
   and stateful manipulation of the list. */
struct term_list
{
  typedef std::list<tree>::iterator iterator;

  term_list ();
  term_list (tree);

  bool includes (tree);
  iterator insert (iterator, tree);
  iterator push_back (tree);
  iterator erase (iterator);
  iterator replace (iterator, tree);
  iterator replace (iterator, tree, tree);

  iterator begin() { return seq.begin(); }
  iterator end() { return seq.end(); }

  std::list<tree>         seq;
  hash_table<term_hasher> tab;
};

inline
term_list::term_list ()
  : seq(), tab (11)
{
}

/* Initialize a term list with an initial term. */

inline
term_list::term_list (tree t)
  : seq (), tab (11)
{
  push_back (t);
}

/* Returns true if T is the in the tree. */

inline bool
term_list::includes (tree t)
{
  term_entry ent = {t};
  return tab.find (&ent);
}

/* Append a term to the list. */
inline term_list::iterator
term_list::push_back (tree t)
{
  return insert (end(), t);
}

/* Insert a new (unseen) term T into the list before the proposition
   indicated by ITER. Returns the iterator to the newly inserted
   element.  */

term_list::iterator
term_list::insert (iterator iter, tree t)
{
  gcc_assert (!includes (t));
  iter = seq.insert (iter, t);
  term_entry ent = {t};
  term_entry** slot = tab.find_slot (&ent, INSERT);
  term_entry* ptr = ggc_alloc<term_entry> ();
  *ptr = ent;
  *slot = ptr;
  return iter;
}

/* Remove an existing term from the list. Returns an iterator referring
   to the element after the removed term.  This may be end().  */

term_list::iterator
term_list::erase (iterator iter)
{
  gcc_assert (includes (*iter));
  term_entry ent = {*iter};
  tab.remove_elt (&ent);
  iter = seq.erase (iter);
  return iter;
}

/* Replace the given term with that specified. If the term has
   been previously seen, do not insert the term. Returns the
   first iterator past the current term.  */

term_list::iterator
term_list::replace (iterator iter, tree t)
{
  iter = erase (iter);
  if (!includes (t))
    insert (iter, t);
  return iter;
}


/* Replace the term at the given position by the supplied T1
   followed by t2. This is used in certain logical operators to
   load a list of assumptions or conclusions.  */

term_list::iterator
term_list::replace (iterator iter, tree t1, tree t2)
{
  iter = erase (iter);
  if (!includes (t1))
    insert (iter, t1);
  if (!includes (t2))
    insert (iter, t2);
  return iter;
}

/* A goal (or subgoal) models a sequent of the form
   'A |- C' where A and C are lists of assumptions and
   conclusions written as propositions in the constraint
   language (i.e., lists of trees). */

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
  iterator discharge (iterator i);
};

/* Initialize the state with a single empty goal, and set that goal
   as the current subgoal.  */

inline
proof_state::proof_state ()
  : std::list<proof_goal> (1)
{ }


/* Branch the current goal by creating a new subgoal, returning a
   reference to the new object. This does not update the current goal. */

inline proof_state::iterator
proof_state::branch (iterator i)
{
  gcc_assert (i != end());
  proof_goal& g = *i;
  return insert (++i, g);
}

/* Discharge the current goal, setting it equal to the
   next non-satisfied goal. */

inline proof_state::iterator
proof_state::discharge (iterator i)
{
  gcc_assert (i != end());
  return erase (i);
}


/*---------------------------------------------------------------------------
                        Debugging
---------------------------------------------------------------------------*/

// void
// debug (term_list& ts)
// {
//   for (term_list::iterator i = ts.begin(); i != ts.end(); ++i)
//     verbatim ("  # %E", *i);
// }
//
// void
// debug (proof_goal& g)
// {
//   debug (g.assumptions);
//   verbatim ("       |-");
//   debug (g.conclusions);
// }

/*---------------------------------------------------------------------------
                        Atomicity of constraints
---------------------------------------------------------------------------*/

/* Returns true if T is not an atomic constraint.  */

bool
non_atomic_constraint_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case PRED_CONSTR:
    case EXPR_CONSTR:
    case TYPE_CONSTR:
    case ICONV_CONSTR:
    case DEDUCT_CONSTR:
    case EXCEPT_CONSTR:
      /* A pack expansion isn't atomic, but it can't decompose to prove an
	 atom, so it shouldn't cause analyze_atom to return undecided.  */
    case EXPR_PACK_EXPANSION:
      return false;
    case CHECK_CONSTR:
    case PARM_CONSTR:
    case CONJ_CONSTR:
    case DISJ_CONSTR:
      return true;
    default:
      gcc_unreachable ();
    }
}

/* Returns true if any constraints in T are not atomic.  */

bool
any_non_atomic_constraints_p (term_list& t)
{
  return any_p (t.begin(), t.end(), non_atomic_constraint_p);
}

/*---------------------------------------------------------------------------
                           Proof validations
---------------------------------------------------------------------------*/

enum proof_result
{
  invalid,
  valid,
  undecided
};

proof_result check_term (term_list&, tree);


proof_result
analyze_atom (term_list& ts, tree t)
{
  /* FIXME: Hook into special cases, if any. */
  /*
  term_list::iterator iter = ts.begin();
  term_list::iterator end = ts.end();
  while (iter != end)
    {
      ++iter;
    }
  */

  if (non_atomic_constraint_p (t))
    return undecided;
  if (any_non_atomic_constraints_p (ts))
    return undecided;
  return invalid;
}

/* Search for a pack expansion in the list of assumptions that would
   make this expansion valid.  */

proof_result
analyze_pack (term_list& ts, tree t)
{
  tree c1 = normalize_expression (PACK_EXPANSION_PATTERN (t));
  term_list::iterator iter = ts.begin();
  term_list::iterator end = ts.end();
  while (iter != end)
    {
      if (TREE_CODE (*iter) == TREE_CODE (t))
        {
          tree c2 = normalize_expression (PACK_EXPANSION_PATTERN (*iter));
          if (prove_implication (c2, c1))
            return valid;
          else
            return invalid;
        }
      ++iter;
    }
  return invalid;
}

/* Search for concept checks in TS that we know subsume T. */

proof_result
search_known_subsumptions (term_list& ts, tree t)
{
  for (term_list::iterator i = ts.begin(); i != ts.end(); ++i)
    if (TREE_CODE (*i) == CHECK_CONSTR)
      {
        if (bool* b = lookup_subsumption_result (*i, t))
          return *b ? valid : invalid;
      }
  return undecided;
}

/* Determine if the terms in TS provide sufficient support for proving
   the proposition T. If any term in TS is a concept check that is known
   to subsume T, then the proof is valid. Otherwise, we have to expand T
   and continue searching for support.  */

proof_result
analyze_check (term_list& ts, tree t)
{
  proof_result r = search_known_subsumptions (ts, t);
  if (r != undecided)
    return r;

  tree tmpl = CHECK_CONSTR_CONCEPT (t);
  tree args = CHECK_CONSTR_ARGS (t);
  tree c = expand_concept (tmpl, args);
  return check_term (ts, c);
}

/* Recursively check constraints of the parameterized constraint. */

proof_result
analyze_parameterized (term_list& ts, tree t)
{
  return check_term (ts, PARM_CONSTR_OPERAND (t));
}

proof_result
analyze_conjunction (term_list& ts, tree t)
{
  proof_result r = check_term (ts, TREE_OPERAND (t, 0));
  if (r == invalid || r == undecided)
    return r;
  return check_term (ts, TREE_OPERAND (t, 1));
}

proof_result
analyze_disjunction (term_list& ts, tree t)
{
  proof_result r = check_term (ts, TREE_OPERAND (t, 0));
  if (r == valid)
    return r;
  return check_term (ts, TREE_OPERAND (t, 1));
}

proof_result
analyze_term (term_list& ts, tree t)
{
  switch (TREE_CODE (t))
    {
    case CHECK_CONSTR:
      return analyze_check (ts, t);

    case PARM_CONSTR:
      return analyze_parameterized (ts, t);

    case CONJ_CONSTR:
      return analyze_conjunction (ts, t);
    case DISJ_CONSTR:
      return analyze_disjunction (ts, t);

    case PRED_CONSTR:
    case EXPR_CONSTR:
    case TYPE_CONSTR:
    case ICONV_CONSTR:
    case DEDUCT_CONSTR:
    case EXCEPT_CONSTR:
      return analyze_atom (ts, t);

    case EXPR_PACK_EXPANSION:
      return analyze_pack (ts, t);

    case ERROR_MARK:
      /* Encountering an error anywhere in a constraint invalidates
         the proof, since the constraint is ill-formed.  */
      return invalid;
    default:
      gcc_unreachable ();
    }
}

/* Check if a single term can be proven from a set of assumptions.
   If the proof is not valid, then it is incomplete when either
   the given term is non-atomic or any term in the list of assumptions
   is not-atomic.  */

proof_result
check_term (term_list& ts, tree t)
{
  /* Try the easy way; search for an equivalent term.  */
  if (ts.includes (t))
    return valid;

  /* The hard way; actually consider what the term means.  */
  return analyze_term (ts, t);
}

/* Check to see if any term is proven by the assumptions in the
   proof goal. The proof is valid if the proof of any term is valid.
   If validity cannot be determined, but any particular
   check was undecided, then this goal is undecided.  */

proof_result
check_goal (proof_goal& g)
{
  term_list::iterator iter = g.conclusions.begin ();
  term_list::iterator end = g.conclusions.end ();
  bool incomplete = false;
  while (iter != end)
    {
      proof_result r = check_term (g.assumptions, *iter);
      if (r == valid)
        return r;
      if (r == undecided)
        incomplete = true;
      ++iter;
    }

    /* Was the proof complete? */
    if (incomplete)
      return undecided;
    else
      return invalid;
}

/* Check if the the proof is valid. This is the case when all
   goals can be discharged. If any goal is invalid, then the
   entire proof is invalid. Otherwise, the proof is undecided.  */

proof_result
check_proof (proof_state& p)
{
  proof_state::iterator iter = p.begin();
  proof_state::iterator end = p.end();
  while (iter != end)
    {
      proof_result r = check_goal (*iter);
      if (r == invalid)
        return r;
      if (r == valid)
        iter = p.discharge (iter);
      else
        ++iter;
    }

  /* If all goals are discharged, then the proof is valid.  */
  if (p.empty())
    return valid;
  else
    return undecided;
}

/*---------------------------------------------------------------------------
                           Left logical rules
---------------------------------------------------------------------------*/

term_list::iterator
load_check_assumption (term_list& ts, term_list::iterator i)
{
  tree decl = CHECK_CONSTR_CONCEPT (*i);
  tree tmpl = DECL_TI_TEMPLATE (decl);
  tree args = CHECK_CONSTR_ARGS (*i);
  return ts.replace(i, expand_concept (tmpl, args));
}

term_list::iterator
load_parameterized_assumption (term_list& ts, term_list::iterator i)
{
  return ts.replace(i, PARM_CONSTR_OPERAND(*i));
}

term_list::iterator
load_conjunction_assumption (term_list& ts, term_list::iterator i)
{
  tree t1 = TREE_OPERAND (*i, 0);
  tree t2 = TREE_OPERAND (*i, 1);
  return ts.replace(i, t1, t2);
}

/* Examine the terms in the list, and apply left-logical rules to move
   terms into the set of assumptions. */

void
load_assumptions (proof_goal& g)
{
  term_list::iterator iter = g.assumptions.begin();
  term_list::iterator end = g.assumptions.end();
  while (iter != end)
    {
      switch (TREE_CODE (*iter))
        {
        case CHECK_CONSTR:
          iter = load_check_assumption (g.assumptions, iter);
          break;
        case PARM_CONSTR:
          iter = load_parameterized_assumption (g.assumptions, iter);
          break;
        case CONJ_CONSTR:
          iter = load_conjunction_assumption (g.assumptions, iter);
          break;
        default:
          ++iter;
          break;
        }
    }
}

/* In each subgoal, load constraints into the assumption set.  */

void
load_assumptions(proof_state& p)
{
  proof_state::iterator iter = p.begin();
  while (iter != p.end())
    {
      load_assumptions (*iter);
      ++iter;
    }
}

void
explode_disjunction (proof_state& p, proof_state::iterator gi, term_list::iterator ti1)
{
  tree t1 = TREE_OPERAND (*ti1, 0);
  tree t2 = TREE_OPERAND (*ti1, 1);

  /* Erase the current term from the goal. */
  proof_goal& g1 = *gi;
  proof_goal& g2 = *p.branch (gi);

  /* Get an iterator to the equivalent position in th enew goal. */
  int n = std::distance (g1.assumptions.begin (), ti1);
  term_list::iterator ti2 = g2.assumptions.begin ();
  std::advance (ti2, n);

  /* Replace the disjunction in both branches. */
  g1.assumptions.replace (ti1, t1);
  g2.assumptions.replace (ti2, t2);
}


/* Search the assumptions of the goal for the first disjunction. */

bool
explode_goal (proof_state& p, proof_state::iterator gi)
{
  term_list& ts = gi->assumptions;
  term_list::iterator ti = ts.begin();
  term_list::iterator end = ts.end();
  while (ti != end)
    {
      if (TREE_CODE (*ti) == DISJ_CONSTR)
        {
          explode_disjunction (p, gi, ti);
          return true;
        }
      else ++ti;
    }
  return false;
}

/* Search for the first goal with a disjunction, and then branch
   creating a clone of that subgoal. */

void
explode_assumptions (proof_state& p)
{
  proof_state::iterator iter = p.begin();
  proof_state::iterator end = p.end();
  while (iter != end)
    {
      if (explode_goal (p, iter))
        return;
      ++iter;
    }
}


/*---------------------------------------------------------------------------
                           Right logical rules
---------------------------------------------------------------------------*/

term_list::iterator
load_disjunction_conclusion (term_list& g, term_list::iterator i)
{
  tree t1 = TREE_OPERAND (*i, 0);
  tree t2 = TREE_OPERAND (*i, 1);
  return g.replace(i, t1, t2);
}

/* Apply logical rules to the right hand side. This will load the
   conclusion set with all tpp-level disjunctions.  */

void
load_conclusions (proof_goal& g)
{
  term_list::iterator iter = g.conclusions.begin();
  term_list::iterator end = g.conclusions.end();
  while (iter != end)
    {
      if (TREE_CODE (*iter) == DISJ_CONSTR)
        iter = load_disjunction_conclusion (g.conclusions, iter);
      else
        ++iter;
    }
}

void
load_conclusions (proof_state& p)
{
  proof_state::iterator iter = p.begin();
  while (iter != p.end())
    {
      load_conclusions (*iter);
      ++iter;
    }
}


/*---------------------------------------------------------------------------
                          High-level proof tactics
---------------------------------------------------------------------------*/

/* Given two constraints A and C, try to derive a proof that
   A implies C.  */

bool
prove_implication (tree a, tree c)
{
  /* Quick accept. */
  if (cp_tree_equal (a, c))
    return true;

  /* Build the initial proof state. */
  proof_state proof;
  proof_goal& goal = proof.front();
  goal.assumptions.push_back(a);
  goal.conclusions.push_back(c);

  /* Perform an initial right-expansion in the off-chance that the right
     hand side contains disjunctions. */
  load_conclusions (proof);

  int step_max = 1 << 10;
  int step_count = 0;              /* FIXME: We shouldn't have this. */
  std::size_t branch_limit = 1024; /* FIXME: This needs to be configurable. */
  while (step_count < step_max && proof.size() < branch_limit)
    {
      /* Determine if we can prove that the assumptions entail the
         conclusions. If so, we're done. */
      load_assumptions (proof);

      /* Can we solve the proof based on this? */
      proof_result r = check_proof (proof);
      if (r != undecided)
        return r == valid;

      /* If not, then we need to dig into disjunctions.  */
      explode_assumptions (proof);

      ++step_count;
    }

  if (step_count == step_max)
    error ("subsumption failed to resolve");

  if (proof.size() == branch_limit)
    error ("exceeded maximum number of branches");

  return false;
}

/* Returns true if the LEFT constraint subsume the RIGHT constraints.
   This is done by deriving a proof of the conclusions on the RIGHT
   from the assumptions on the LEFT assumptions.  */

bool
subsumes_constraints_nonnull (tree left, tree right)
{
  gcc_assert (check_constraint_info (left));
  gcc_assert (check_constraint_info (right));

  auto_timevar time (TV_CONSTRAINT_SUB);
  tree a = CI_ASSOCIATED_CONSTRAINTS (left);
  tree c = CI_ASSOCIATED_CONSTRAINTS (right);
  return prove_implication (a, c);
}

} /* namespace */

/* Returns true if the LEFT constraints subsume the RIGHT
   constraints.  */

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
