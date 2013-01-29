/* Source code for an implementation of the Omega test, an integer
   programming algorithm for dependence analysis, by William Pugh,
   appeared in Supercomputing '91 and CACM Aug 92.

   This code has no license restrictions, and is considered public
   domain.

   Changes copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@inria.fr>

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

#include "config.h"
#include "params.h"

#ifndef GCC_OMEGA_H
#define GCC_OMEGA_H

#define OMEGA_MAX_VARS PARAM_VALUE (PARAM_OMEGA_MAX_VARS)
#define OMEGA_MAX_GEQS PARAM_VALUE (PARAM_OMEGA_MAX_GEQS)
#define OMEGA_MAX_EQS PARAM_VALUE (PARAM_OMEGA_MAX_EQS)

#define pos_infinity (0x7ffffff)
#define neg_infinity (-0x7ffffff)

/* Results of the Omega solver.  */
enum omega_result {
  omega_false = 0,
  omega_true = 1,

  /* Value returned when the solver is unable to determine an
     answer.  */
  omega_unknown = 2,

  /* Value used for asking the solver to simplify the system.  */
  omega_simplify = 3
};

/* Values used for labeling equations.  Private (not used outside the
   solver).  */
enum omega_eqn_color {
  omega_black = 0,
  omega_red = 1
};

/* Structure for equations.  */
typedef struct eqn_d
{
  int key;
  int touched;
  enum omega_eqn_color color;

  /* Array of coefficients for the equation.  The layout of the data
     is as follows: coef[0] is the constant, coef[i] for 1 <= i <=
     OMEGA_MAX_VARS, are the coefficients for each dimension.  Examples:
     the equation 0 = 9 + x + 0y + 5z is encoded as [9 1 0 5], the
     inequality 0 <= -8 + x + 2y + 3z is encoded as [-8 1 2 3].  */
  int *coef;
} *eqn;

typedef struct omega_pb_d
{
  /* The number of variables in the system of equations.  */
  int num_vars;

  /* Safe variables are not eliminated during the Fourier-Motzkin
     simplification of the system.  Safe variables are all those
     variables that are placed at the beginning of the array of
     variables: PB->var[1, ..., SAFE_VARS].  PB->var[0] is not used,
     as PB->eqs[x]->coef[0] represents the constant of the equation.  */
  int safe_vars;

  /* Number of elements in eqs[].  */
  int num_eqs;
  /* Number of elements in geqs[].  */
  int num_geqs;
  /* Number of elements in subs[].  */
  int num_subs;

  int hash_version;
  bool variables_initialized;
  bool variables_freed;

  /* Index or name of variables.  Negative integers are reserved for
     wildcard variables.  Maps the index of variables in the original
     problem to the new index of the variable.  The index for a
     variable in the coef array of an equation can change as some
     variables are eliminated.  */
  int *var;

  int *forwarding_address;

  /* Inequalities in the system of constraints.  */
  eqn geqs;

  /* Equations in the system of constraints.  */
  eqn eqs;

  /* A map of substituted variables.  */
  eqn subs;
} *omega_pb;

extern void omega_initialize (void);
extern omega_pb omega_alloc_problem (int, int);
extern enum omega_result omega_solve_problem (omega_pb, enum omega_result);
extern enum omega_result omega_simplify_problem (omega_pb);
extern enum omega_result omega_simplify_approximate (omega_pb);
extern enum omega_result omega_constrain_variable_sign (omega_pb,
							enum omega_eqn_color,
							int, int);
extern void debug_omega_problem (omega_pb);
extern void omega_print_problem (FILE *, omega_pb);
extern void omega_print_red_equations (FILE *, omega_pb);
extern int omega_count_red_equations (omega_pb);
extern void omega_pretty_print_problem (FILE *, omega_pb);
extern void omega_unprotect_variable (omega_pb, int var);
extern void omega_negate_geq (omega_pb, int);
extern void omega_convert_eq_to_geqs (omega_pb, int eq);
extern void omega_print_eqn (FILE *, omega_pb, eqn, bool, int);
extern bool omega_problem_has_red_equations (omega_pb);
extern enum omega_result omega_eliminate_redundant (omega_pb, bool);
extern void omega_eliminate_red (omega_pb, bool);
extern void omega_constrain_variable_value (omega_pb, enum omega_eqn_color,
					    int, int);
extern bool omega_query_variable (omega_pb, int, int *, int *);
extern int omega_query_variable_signs (omega_pb, int, int, int, int,
				       int, int, bool *, int *);
extern bool omega_query_variable_bounds (omega_pb, int, int *, int *);
extern void (*omega_when_reduced) (omega_pb);
extern void omega_no_procedure (omega_pb);

/* Return true when variable I in problem PB is a wildcard.  */

static inline bool
omega_wildcard_p (omega_pb pb, int i)
{
  return (pb->var[i] < 0);
}

/* Return true when variable I in problem PB is a safe variable.  */

static inline bool
omega_safe_var_p (omega_pb pb, int i)
{
  /* The constant of an equation is not a variable.  */
  gcc_assert (0 < i);
  return (i <= pb->safe_vars);
}

/* Print to FILE equality E from PB.  */

static inline void
omega_print_eq (FILE *file, omega_pb pb, eqn e)
{
  omega_print_eqn (file, pb, e, false, 0);
}

/* Print to FILE inequality E from PB.  */

static inline void
omega_print_geq (FILE *file, omega_pb pb, eqn e)
{
  omega_print_eqn (file, pb, e, true, 0);
}

/* Print to FILE inequality E from PB.  */

static inline void
omega_print_geq_extra (FILE *file, omega_pb pb, eqn e)
{
  omega_print_eqn (file, pb, e, true, 1);
}

/* E1 = E2, make a copy of E2 into E1.  Equations contain S variables.  */

static inline void
omega_copy_eqn (eqn e1, eqn e2, int s)
{
  e1->key = e2->key;
  e1->touched = e2->touched;
  e1->color = e2->color;

  memcpy (e1->coef, e2->coef, (s + 1) * sizeof (int));
}

/* Initialize E = 0.  Equation E contains S variables.  */

static inline void
omega_init_eqn_zero (eqn e, int s)
{
  e->key = 0;
  e->touched = 0;
  e->color = omega_black;

  memset (e->coef, 0, (s + 1) * sizeof (int));
}

/* Allocate N equations with S variables.  */

static inline eqn
omega_alloc_eqns (int s, int n)
{
  int i;
  eqn res = (eqn) (xcalloc (n, sizeof (struct eqn_d)));

  for (i = n - 1; i >= 0; i--)
    {
      res[i].coef = (int *) (xcalloc (OMEGA_MAX_VARS + 1, sizeof (int)));
      omega_init_eqn_zero (&res[i], s);
    }

  return res;
}

/* Free N equations from array EQ.  */

static inline void
omega_free_eqns (eqn eq, int n)
{
  int i;

  for (i = n - 1; i >= 0; i--)
    free (eq[i].coef);

  free (eq);
}

/* Returns true when E is an inequality with a single variable.  */

static inline bool
single_var_geq (eqn e, int nv ATTRIBUTE_UNUSED)
{
  return (e->key != 0
	  && -OMEGA_MAX_VARS <= e->key && e->key <= OMEGA_MAX_VARS);
}

/* Allocate a new equality with all coefficients 0, and tagged with
   COLOR.  Return the index of this equality in problem PB.  */

static inline int
omega_add_zero_eq (omega_pb pb, enum omega_eqn_color color)
{
  int idx = pb->num_eqs++;

  gcc_assert (pb->num_eqs <= OMEGA_MAX_EQS);
  omega_init_eqn_zero (&pb->eqs[idx], pb->num_vars);
  pb->eqs[idx].color = color;
  return idx;
}

/* Allocate a new inequality with all coefficients 0, and tagged with
   COLOR.  Return the index of this inequality in problem PB.  */

static inline int
omega_add_zero_geq (omega_pb pb, enum omega_eqn_color color)
{
  int idx = pb->num_geqs;

  pb->num_geqs++;
  gcc_assert (pb->num_geqs <= OMEGA_MAX_GEQS);
  omega_init_eqn_zero (&pb->geqs[idx], pb->num_vars);
  pb->geqs[idx].touched = 1;
  pb->geqs[idx].color = color;
  return idx;
}

/* Initialize variables for problem PB.  */

static inline void
omega_initialize_variables (omega_pb pb)
{
  int i;

  for (i = pb->num_vars; i >= 0; i--)
    pb->forwarding_address[i] = pb->var[i] = i;

  pb->variables_initialized = true;
}

/* Free problem PB.  */

static inline void
omega_free_problem (omega_pb pb)
{
  free (pb->var);
  free (pb->forwarding_address);
  omega_free_eqns (pb->geqs, OMEGA_MAX_GEQS);
  omega_free_eqns (pb->eqs, OMEGA_MAX_EQS);
  omega_free_eqns (pb->subs, OMEGA_MAX_VARS + 1);
  free (pb);
}

/* Copy omega problems: P1 = P2.  */

static inline void
omega_copy_problem (omega_pb p1, omega_pb p2)
{
  int e, i;

  p1->num_vars = p2->num_vars;
  p1->hash_version = p2->hash_version;
  p1->variables_initialized = p2->variables_initialized;
  p1->variables_freed = p2->variables_freed;
  p1->safe_vars = p2->safe_vars;
  p1->num_eqs = p2->num_eqs;
  p1->num_subs = p2->num_subs;
  p1->num_geqs = p2->num_geqs;

  for (e = p2->num_eqs - 1; e >= 0; e--)
    omega_copy_eqn (&(p1->eqs[e]), &(p2->eqs[e]), p2->num_vars);

  for (e = p2->num_geqs - 1; e >= 0; e--)
    omega_copy_eqn (&(p1->geqs[e]), &(p2->geqs[e]), p2->num_vars);

  for (e = p2->num_subs - 1; e >= 0; e--)
    omega_copy_eqn (&(p1->subs[e]), &(p2->subs[e]), p2->num_vars);

  for (i = p2->num_vars; i >= 0; i--)
    p1->var[i] = p2->var[i];

  for (i = OMEGA_MAX_VARS; i >= 0; i--)
    p1->forwarding_address[i] = p2->forwarding_address[i];
}

#endif /* GCC_OMEGA_H */
