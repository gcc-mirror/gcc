/* Compute some static properties of values.
   Copyright (C) 2026 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"

#include "a68.h"

/* This parser pass computes a set of static properties of certain parse tree
   nodes and symtab entries representing source constructs.  It is somewhat
   inspired on the treatment of static properties described in

     An Optimized Translation Process and Its Application to ALGOL 68
     P. Branquart et al.

   However, the compilation system described in Branquart et al translates
   constructs to an IR which is considerably lower in abstraction compared to
   GENERIC, and therefore our management of static properties is different.
   The underlying ideas, however, are very similar.  */

/* The origin
   ──────────

   The "origin" is a static property of source constructs that yield values.
   Its purpose is to record the properties of the original value from which the
   construct is derived, as well as the transformations the value has been
   submitted to before appearing in the construct.  This static property is
   composed by several fields described below.

   kindo is the "kind of the origin".
   ─────

     It keeps track of the fact that a value is issued from

      ╭─────────╮
      │KINDO_IDE│ an identifier in a VAR_DECL,
      ╰─────────╯
      ╭─────────╮
      │KINDO_VAR│ a variable in a VAR_DECL,
      ╰─────────╯
      ╭─────────╮
      │KINDO_CST│ a constant in a VAR_DECL with TREE_CONSTANT=1,
      ╰─────────╯
      ╭─────────╮
      │KINDO_GEN│ a generator i.e. a malloc or alloca,
      ╰─────────╯
      ╭─────────╮
      │KINDO_NIL│ or another construct.
      ╰─────────╯

     This property remains invariant through the static elaboration of a number
     of actions such as slices, selection and dereferencings.

    bno is the "block number of the origin".
    ───

     Its meaning for a particular construct is to be interpreted along with its
     kindo.

     ● For KINDO_IDE and KINDO_VAR, bno indicates the depth number of the block
       where the identifier or the variable is declared.  This is not to be
       confused with the scope of the value ascribed to the identifier or
       stored in the variable.

     ● For KINDO_GEN, when it corresponds to a local generator, bno indicates
       the depth number of the block where the generator appears.

     ● For KINDO_CST, and for KINDO_GEN when it corresponds to a heap
       generator, bno is always zero.

     ● For KINDO_NIL, bno indicates the depth number of the construct issuing
       the value.

    derefo is the "flag dereferencing of the origin".
    ──────

     This flag indicates whether a dereferencing action has taken place
     starting from the construct in whcih kindo has been set up.  This
     information allows, in some cases, to statically detect the absence of
     side-effects, and subsequently to avoid some copies of values.

    geno is the "flag local generator of the origin".
    ────

     This flag indicates whether a local generator is involved in the
     construction of this value.

    diago contains the "diagnostics of the origin".
    ─────

     This property contains location information of the construct originating
     the value.  It is useful in order to emit good diagnostic messages in
     dynamic checks pointing to the source program construct giving rise to the
     value involved in the dynamic check.  */

/* The access
   ──────────

   In the "low" pass parse tree nodes denoting units will be lowered to GCC
   GENERIC trees.  These trees compute the values yielded by the units.  For
   example, a DENOTATION node for an integral value may be lowered to an
   INTEGER_CST tree, a FORMULA node to a PLUS_EXPR tree, an applied IDENTIFIER
   to a VAR_DECL, and a DEREFERENCING node to an INDIRECT_REF tree.

   In principle, it would be expected for the resulting trees to directly
   compute the value denoted by the parse nodes originating them.  However, for
   reasons of efficiency, this is not always the case.

   Parse nodes lowering to trees that directly compute the value yielded by the
   unit are said to have "direct access".  Examples:

   ● Consider a denotation "10".  The parser builds a parse node DENOTATION
     with the value 10 for it, and the "low" pass lowers that parse node into a
     GENERIC tree consisting in a single INTEGER_CST tree node, with value 10.
     In this case, the tree indeed computes the value yielded by the DENOTATION
     node.

   ● An applied IDENTIFIER "maxint" with mode int that has been declared via an
     identity declaration, with some integral value ascribed to it.  The "low"
     pass lowers it into a VAR_DECL with TREE_TYPE int.  Again, this node has
     "direct access", because the VAR_DECL in a GENERIC r-value position
     computes the value ascribed to the IDENTIFIER.

   Parse nodes denoting units that yield names, lowering to VAR_DECL trees
   whose address is the value yielded by the unit are said to have "variable
   access".  Examples:

   ● Suppose that another applied IDENTIFIER "count" with mode int has been
     declared, this time via a variable declaration.  The value ascribed to the
     identifier is in this case a name with mode "ref int".  Such a name would
     generally be lowered to a tree of type *int, i.e. the address of the
     variable, but in this case the node will be lowered to a VAR_DECL with
     type int instead.  This is an optimization whose goal is to avoid
     unnecessary indirect addressing.  The tree represents the name and can be
     moved around as-is, but when it comes to access the actual value of the
     name, i.e. the address of the integral value referenced by the name, it
     becomes necessary to take the address of the VAR_DECL.

   Parse nodes lowering to trees that need to be indirected in order to compute
   the value yielded by the unit are said to have "indirect access".  Examples:

   ● Consider what happens when a DEREFERENCING node has a coercend with
     "variable access" which is of mode "ref int", or alternatively a coercend
     with "direct access" also of mode "ref int".  In the first case, the
     coercend will be lowered to a VAR_DECL with type int, as an optimization.
     In the second case, the coercend will be lowered to some tree with type
     *int.  It would make sense for the DEREFERENCING node to be of "direct
     class" and be lowered to an INDIRECT_REF tree taking as argument the
     address of the VAR_DECL in the first case, and just the value of the
     VAR_DECL in the second case.  However, as an optimization, the
     dereferencing of nodes with "direct class" or "variable class" doesn't
     require any run-time action (other than perhaps checking for nil) and the
     DEREFERENCING node is lowered to the coercend tree without any
     modification.  Only when the dereferenced value is actually used,
     indirection will be performed.

   ● The value yielded by the last unit in a serial clause doesn't need to be
     copied into the outer range if the yielded value is accessible in that
     range.  Instead, the unit gets lowered to a tree computing the address of
     the value, and is given indirect access.

   The different access classes are summarized below:

      ╭──────────╮
      │ACCESS_IND│ indirect access.
      ╰──────────╯
      ╭──────────╮
      │ACCESS_DIR│ direct access.
      ╰──────────╯
      ╭──────────╮
      │ACCESS_VAR│ variable access.
      ╰──────────╯


                    IND    DIR    VAR
                    ───    ───    ───
        node mode:  int    int    ref int
	tree type: *int    int    int


   The purpose of this static property is thus to guide the lowering pass, but
   there is certain level of circularity, as the calculation of the access is
   also influenced by the behavior of the lowering pass.

   It is important to remember that the access static property only makes sense
   for constructs that yield values, i.e. units.  Nodes that are not units are
   annotated with NO_ACCESS, which denotes no access.  */

/* Handling of choice constructs
   ─────────────────────────────

   Choice constructs include:

   ● Serial clauses with completers.
   ● Conditional clauses.
   ● Conformity clauses.
   ● Case clauses.

   The characteristic of these constructs is that they need to perform
   balancing on a set of alternatives, like the values yielded by the then-part
   and the else-part of a conditional clause, in order to determine the static
   properties of the value yielded by the choice construct.

   Given a choice construct involving sub-values V1, V2, .. Vn, with their
   respective "a priori" static properties, the "a posteriori" static
   properties of the resulting value Vr are derived as follows:

   ● The mode of Vr (which is a static property, albeit not handled here) is
     determined from the balancing of the modes of V1, V2, ... Vn.  This has
     already been done by the parser at this stage and the parse tree nodes are
     annotated with the "a posteriori" mode of the choice construct.  Suitable
     coercions are also in the parse tree.

   ● If all sub-values have the same "a priori" origin, then that's the origin
     of Vr.  Otherwise, we act conservatively by setting KINDO (Vr) to
     KINDO_NIL, DEREFO to true iff any of the sub-values have DEREFO set, and
     GENO to true iff any of the sub-values have GENO set.


   ● If all sub-values have the same "a priori" access, then that's the access
     of Vr.  Otherwise, use direct access.

   These rules are implemented in the corresponding handlers for choice
   constructs, below.  */

/* Allocate a new origin and return it.  The new origin is of kind NIL, and its
   properties are derived from the given parse node P.  */

static ORIGIN_T *
make_origin (NODE_T *p)
{
  ORIGIN_T *ori = ggc_alloc<ORIGIN_T> ();
  ori->kindo = KINDO_NIL;
  ori->diago = a68_get_node_location (p);
  ori->bno = LEX_LEVEL (p);
  ori->derefo = false;
  ori->geno = false;
  return ori;
}

/* Allocate a copy of ORI and return it.  */

static ORIGIN_T *
dup_origin (ORIGIN_T *ori)
{
  ORIGIN_T *res = ggc_alloc<ORIGIN_T> ();
  *res = *ori;
  return res;
}

/* Determine whether ORI1 and ORI2 reflect the same origin.  */

static bool
origin_equal_p (ORIGIN_T *ori1, ORIGIN_T *ori2)
{
  return (ori1 != NO_ORIGIN
	  && ori2 != NO_ORIGIN
	  && (ori1 == ori2
	      || (ori1->kindo == ori2->kindo
		  && ori1->derefo == ori2->derefo
		  && ori1->geno == ori2->geno
		  && ori1->diago == ori2->diago)));
}

/* Denotations
   ───────────

   Denotations of any mode introduce their own constant origin, and always have
   direct access.  */

static void
sprops_for_denotation (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  KINDO (p) = KINDO_CST;
  ACCESS (p) = ACCESS_DIR;
}

/* Widening coercion
   ─────────────────

   Widening of integral and real values result into direct real and complex
   values.  The widening of bits and bytes result into direct multiple values.
   This is a kernel invariant operation, so the origin stays unchanged.  */

static void
sprops_for_widening (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (SUB (p));
  ACCESS (p) = ACCESS_DIR;
}

/* Voiding coercion
   ────────────────

   The origin of the voiding construct is the origin of the voided value.  The
   resulting voided voided value has acess NIL, denoting the absence of value.
   This is a kernel invariant operation, so the origin stays unchanged.  */

static void
sprops_for_voiding (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (SUB (p));
  ACCESS (p) = ACCESS_NIL;
}

/* Dereferencing coercion
   ──────────────────────

   The origin of the dereferenced value is like the origin of the coercend but
   with DEREFO set.  We always use an indirect access for the coercee so we
   delay actual indirection for when (and if) the dereferenced value actually
   gets used.  */

static void
sprops_for_dereferencing (NODE_T *p)
{
  ORIGIN (p) = dup_origin (ORIGIN (SUB (p)));
  DEREFO (p) = true;
  ACCESS (p) = ACCESS_IND;
}

/* Deproceduring coercion
   ──────────────────────

   It is not possible to detemine the origin of the value yielded by the
   elaboration of the procedure, so we create a new one with KINDO_NIL.  The
   access of the yielded value is always direct.  */

static void
sprops_for_deproceduring (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_DIR;
}

/* Proceduring
   ───────────

   Procedured jumps result into a proc value.  The origin is new and is of
   KINDO_NIL.  The access for the resulting procedure is always direct.  */

static void
sprops_for_proceduring (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_DIR;
}

/* Rowing coercion
   ───────────────

   The origin of the coercee is the origin of the coercend.  The access of the
   resulting value is always direct: reals, complex or multiples of bools or
   chars.  This is a kernel invariant operation, so the origin stays
   unchanged. */

static void
sprops_for_rowing (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (SUB (p));
  ACCESS (p) = ACCESS_DIR;
}

/* Uniting coercion
   ────────────────

   The origin of the coercee is the origin of the coercend.  XXX this is a sort
   of multiple choices problem as well.  This is a kernel invariant operation,
   so the origin stays unchanged.  */

static void
sprops_for_uniting (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (SUB (p));
  ACCESS (p) = ACCESS_DIR; /* XXX */
}

/* Loop clauses
   ────────────

   The loop clause effectively acts like voiding, so it has its own origin.  We
   use NIL access, which denotes the absence of value.  */

static void
sprops_for_loop_clause (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_NIL;
}

/* Enclosed clauses
   ────────────────

   The origin and access of the value of an enclosed clause is the origin and
   access of the value of its enclosed clause :-O  */

static void
sprops_for_enclosed_clause (NODE_T *p)
{
  NODE_T *enclosed_clause = SUB (p);
  ORIGIN (p) = ORIGIN (enclosed_clause);
  ACCESS (p) = ACCESS (enclosed_clause);
}

/* Access clauses
   ──────────────

   The origin/access of the access clause is the origin/access of the
   controlled clause.  */

static void
sprops_for_access_clause (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (NEXT_SUB (p));
}

/* Enquiry clauses
   ───────────────

   The origin of the enquiry clause is the origin of the unit yielded by the
   underlying serial clause, which is assured to not have choices.  The enquiry
   clause always yields a boolean value, which much have direct access.  */

static void
sprops_for_enquiry_clause (NODE_T *p)
{
  if (IS (SUB (p), UNIT))
    ORIGIN (p) = ORIGIN (SUB (p));
  else if (IS (SUB (p), ENQUIRY_CLAUSE) || IS (SUB (p), INITIALISER_SERIES))
    ORIGIN (p) = ORIGIN (NEXT (NEXT_SUB (p)));
  else
    gcc_unreachable ();
  ACCESS (p) = ACCESS_DIR;
}

/* Serial clauses
   ──────────────

   Serial clauses that have completers are choice constructs, in the sense they
   yield one of several units determined at run-time, and therefore it becomes
   necessary to do some "balancing" at compile-time.  See the comment "Handling
   of choice constructs" above for a description of the strategy we follow for
   the "origin" and "access" static properties in such constructs.  */

static void
sprops_for_serial_clause (NODE_T *p)
{
  NODE_T *last_unit = NO_NODE, *completer = NO_NODE;

  if (IS (SUB (p), UNIT))
    last_unit = SUB (p);
  else if (IS (SUB (p), LABELED_UNIT))
    last_unit = NEXT_SUB (SUB (p));
  else
    {
      gcc_assert (IS (SUB (p), SERIAL_CLAUSE) || IS (SUB (p), INITIALISER_SERIES));

      if (IS (NEXT_SUB (p), EXIT_SYMBOL))
	completer = SUB (p);

      NODE_T *q = NEXT (NEXT_SUB (p));
      if (IS (q, UNIT))
	last_unit = q;
      else
	{
	  gcc_assert (IS (q, LABELED_UNIT));
	  last_unit = NEXT_SUB (q);
	}
    }

  gcc_assert (last_unit != NO_NODE);
  gcc_assert (ORIGIN (last_unit) != NO_ORIGIN);

  if (completer == NO_NODE)
    {
      ORIGIN (p) = ORIGIN (last_unit);
      ACCESS (p) = ACCESS (last_unit);
    }
  else
    {
      /* Balance origin.  */
      if (origin_equal_p (ORIGIN (completer), ORIGIN (last_unit)))
	ORIGIN (p) = ORIGIN (completer);
      else
	{
	  bool found_derefo = DEREFO (last_unit) | DEREFO (completer);
	  bool found_geno = GENO (last_unit) | GENO (completer);
	  ORIGIN (p) = make_origin (p);
	  DEREFO (p) = found_derefo;
	  GENO (p) = found_geno;
	}

      /* Balance access.  */
      if (ACCESS (completer) == ACCESS (last_unit))
	ACCESS (p) = ACCESS (completer);
      else
	ACCESS (p) = ACCESS_DIR;
    }
}

/* Parallel clauses
   ────────────────

   A parallel clause yields void, thus we create a fresh origin with KINDO_NIL.
   The access is NIL.  */

static void
sprops_for_parallel_clause (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_NIL;
}

/* Closed clauses
   ──────────────

   The origin/access of a closed clause is the origin/access of its contained
   serial clause.  */

static void
sprops_for_closed_clause (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (NEXT_SUB (p));
}

/* Conditional clauses
   ───────────────────

   The if-part and else-part of the conditional clause makes it a choice
   construct in terms of static properties propagation.  See "Handling of
   choice constructs" above.  */

static void
sprops_for_conditional_clause (NODE_T *p)
{
  NODE_T *then_part = NO_NODE, *else_part = NO_NODE;

  /* IF or ELIF part,SUB is an enquiry clause.  */
  NODE_T *s = SUB (p);

  /* THEN part, SUB is a serial clause.  */
  FORWARD (s);
  then_part = NEXT_SUB (s);

  /* ELSE part. */
  FORWARD (s);
  if (IS (s, CHOICE) || IS (s, ELSE_PART))
    else_part = NEXT (NEXT_SUB (s));
  else if (IS (s, CLOSE_SYMBOL) || IS (s, FI_SYMBOL))
    ;
  else
    {
      /* ELIF part.  Recurse.  */
      sprops_for_conditional_clause (s);
      else_part = s;
    }

  if (else_part == NO_NODE)
    {
      ORIGIN (p) = ORIGIN (then_part);
      ACCESS (p) = ACCESS (then_part);
    }
  else
    {
      /* Balance origin.  */
      if (origin_equal_p (ORIGIN (then_part), ORIGIN (else_part)))
	ORIGIN (p) = ORIGIN (else_part);
      else
	{
	  bool found_derefo = DEREFO (then_part) | DEREFO (else_part);
	  bool found_geno = GENO (then_part) | GENO (else_part);

	  ORIGIN (p) = make_origin (p);
	  DEREFO (p) = found_derefo;
	  GENO (p) = found_geno;
	}

      /* Balance access.  */
      if (ACCESS (then_part) == ACCESS (else_part))
	ACCESS (p) = ACCESS (else_part);
      else
	ACCESS (p) = ACCESS_DIR;
    }
}

/* Case clauses
   ────────────

   The existence of several alterantives in a case clause makes it a choice
   construct in terms of static properties propagation.  See "Handling of
   choice constructs" above.  */

static void
sprops_for_case_unit (NODE_T *p,
		      bool found_mismatch,
		      ORIGIN_T **postulated_origin,
		      bool *found_derefo, bool *found_geno,
		      ACCESS_T *postulated_access)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	{
	  *found_derefo |= DEREFO (p);
	  *found_geno |= GENO (p);

	  if (*postulated_origin == NO_ORIGIN && !found_mismatch)
	    *postulated_origin = ORIGIN (p);
	  else if (!origin_equal_p (ORIGIN (p), *postulated_origin))
	    {
	      *postulated_origin = NO_ORIGIN;
	      found_mismatch = true;
	    }
	}
      else
	sprops_for_case_unit (SUB (p),
			      found_mismatch,
			      postulated_origin,
			      found_derefo, found_geno,
			      postulated_access);
    }
}

static void
sprops_for_case_clause (NODE_T *p)
{
  NODE_T *out_part = NO_NODE;

  /* CASE or OUSE.  */
  NODE_T *s = SUB (p);

  /* IN.  */
  NODE_T *in_parts = FORWARD (s);

  /* OUT.  */
  FORWARD (s);
  if (IS (s, CHOICE) || IS (s, OUT_PART))
    out_part = NEXT (NEXT_SUB (s));
  else if (IS (s, CLOSE_SYMBOL) || IS (s, ESAC_SYMBOL))
    ;
  else
    {
      /* Recurse.  */
      sprops_for_case_clause (s);
      out_part = s;
    }

  /* We start by postulating the properties of the out-part, if it exists, then
     go through all the in-parts.  */

  ORIGIN_T *postulated_origin = NO_ORIGIN;
  ACCESS_T postulated_access = NO_ACCESS;

  bool found_derefo = false, found_geno = false;

  if (out_part != NO_NODE)
    postulated_origin = ORIGIN (out_part);

  sprops_for_case_unit (in_parts, false,
			      &postulated_origin,
			      &found_derefo, &found_geno,
			      &postulated_access);

  /* Balance origin.  */
  if (postulated_origin != NO_ORIGIN)
    ORIGIN (p) = postulated_origin;
  else
    {
      ORIGIN (p) = make_origin (p);
      DEREFO (p) = found_derefo;
      GENO (p) = found_geno;
    }

  /* Balance access.  */
  if (postulated_access != NO_ACCESS)
    ACCESS (p) = postulated_access;
  else
    ACCESS (p) = ACCESS_DIR;

}

/* Collateral clauses
   ──────────────────

   We distinguish between two cases when handling the static properties of
   collateral clauses:

   ● VOID-collateral-clauses yield void, and therefore they have origin of kind
     NIL and access NIL.

   ● Row- and struct-displays yield a multiple value, with origin constant and
     direct access.  */

static void
sprops_for_collateral_clause (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);

  if (MOID (p) == M_VOID)
    {
      KINDO (p) = KINDO_NIL;
      ACCESS (p) = ACCESS_NIL;
    }
  else
    {
      KINDO (p) = KINDO_CST;
      ACCESS (p) = ACCESS_DIR;
    }
}

/* Conformity clauses
   ──────────────────

   The existence of several alterantives in a conformity clause makes it a
   choice construct in terms of static properties propagation.  See "Handling
   of choice constructs" above.  */

static void
sprops_for_unite_case_unit (NODE_T *p,
			    bool found_mismatch,
			    ORIGIN_T **postulated_origin,
			    bool *found_derefo, bool *found_geno,
			    ACCESS_T *postulated_access)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, SPECIFIER))
	{
	  NODE_T *spec_unit = NEXT_NEXT (p);

	  *found_derefo |= DEREFO (spec_unit);
	  *found_geno |= GENO (spec_unit);

	  if (*postulated_origin == NO_ORIGIN && !found_mismatch)
	    *postulated_origin = ORIGIN (spec_unit);
	  else if (!origin_equal_p (ORIGIN (spec_unit), *postulated_origin))
	    {
	      *postulated_origin = NO_ORIGIN;
	      found_mismatch = true;
	    }

	  FORWARD (p); /* Skip specifier.  */
	  FORWARD (p); /* Skip unit.  */
	  /* The unit is skipped in the for loop post-action.  */
	}
      else
	sprops_for_unite_case_unit (SUB (p),
				    found_mismatch,
				    postulated_origin,
				    found_derefo, found_geno,
				    postulated_access);
    }
}

static void
sprops_for_conformity_clause (NODE_T *p)
{
  NODE_T *out_part = NO_NODE;

  /* CASE or OUSE.  */
  NODE_T *s = SUB (p);

  /* IN.  */
  NODE_T *in_parts = FORWARD (s);

  /* OUT.  */
  FORWARD (s);
  if (IS (s, CHOICE) || IS (s, OUT_PART))
    out_part = NEXT (NEXT_SUB (s));
  else if (IS (s, CLOSE_SYMBOL) || IS (s, ESAC_SYMBOL))
    ;
  else
    {
      /* Recurse.  */
      sprops_for_conformity_clause (s);
      out_part = s;
    }

  /* We start by postulating the properties of the out-part, if it exists, then
     go through all the in-parts.  */

  ORIGIN_T *postulated_origin = NO_ORIGIN;
  ACCESS_T postulated_access = NO_ACCESS;

  bool found_derefo = false, found_geno = false;

  if (out_part != NO_NODE)
    postulated_origin = ORIGIN (out_part);

  sprops_for_unite_case_unit (in_parts, false,
			      &postulated_origin,
			      &found_derefo, &found_geno,
			      &postulated_access);

  /* Balance origin.  */
  if (postulated_origin != NO_ORIGIN)
    ORIGIN (p) = postulated_origin;
  else
    {
      ORIGIN (p) = make_origin (p);
      DEREFO (p) = found_derefo;
      GENO (p) = found_geno;
    }

  /* Balance access.  */
  if (postulated_access != NO_ACCESS)
    ACCESS (p) = postulated_access;
  else
    ACCESS (p) = ACCESS_DIR;
}

/* Identity relations
   ──────────────────

   The identity relation yields a new boolean value with a fresh origin.  Its
   access is always direct.  */

static void
sprops_for_identity_relation (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_DIR;
}

/* Assignations
   ────────────

   If according to previsions the assignation is immediately dereferenced, the
   dereferencing is combined with the assignation by the lowerer, and we use
   the static properties of the source instaed of those of the destination.  */

static void
sprops_for_assignation (NODE_T *p)
{
  NODE_T *destination = SUB (p);
  ORIGIN (p) = ORIGIN (destination);
  /* XXX implement prevision.  */
  ACCESS (p) = ACCESS_DIR;
}

/* Slices
   ──────

   The origin of a slice is the origin of the primary being sliced.  The access
   is alwyas direct.  This is a kernel invariant operation, so the origin stays
   unchanged.  */

static void
sprops_for_slice (NODE_T *p)
{
  NODE_T *primary = SUB (p);
  ORIGIN (p) = ORIGIN (primary);
  ACCESS (p) = ACCESS_DIR;
}

/* Selections
   ──────────

   The origin of a selection is the origin of the secondary being selected.
   The access is always direct.  This is a kernel invariant operation, so the
   origin stays unchanged. */

static void
sprops_for_selection (NODE_T *p)
{
  NODE_T *secondary;

  if (IS (SUB (p), SELECTOR))
    secondary = NEXT_SUB (p);
  else
    {
      gcc_assert (IS (SUB (p), SECONDARY));
      secondary = SUB (p);
    }
  ORIGIN (p) = ORIGIN (secondary);
  ACCESS (p) = ACCESS_DIR;
}

/* Logical functions
   ─────────────────

   The logical function constructs (or_function, and_function) introduce a new
   origin, which is of class KINDO_NIL.  The value yielded by these constructs
   is a boolean and its access is always direct.  */

static void
sprops_for_logical_function (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_DIR;
}

/* Casts
   ─────

   Both origin and access of the coercee are the same origin and access o the
   coercend in the strong context introduced by the cast.  This is a kernel
   invariant operation, so the origin stays unchanged. */

static void
sprops_for_cast (NODE_T *p)
{
  NODE_T *coercend = NEXT_SUB (p);
  ORIGIN (p) = ORIGIN (coercend);
  ACCESS (p) = ACCESS (coercend);
}

/* Calls
   ─────

   The value yielded by a call has a fresh origin of class NIL.  The access is
   always direct.  */

static void
sprops_for_call (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_DIR;
}

/* Generators
   ──────────

   Generators introduce a fresh origin of class GEN, and appropriate
   attributes.  The access is always IND.  */

static void
sprops_for_generator (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  KINDO (p) = KINDO_GEN;

  if (IS (SUB (p), LOC_SYMBOL))
    {
      BNO (p) = LEX_LEVEL (p);
      GENO (p) = true;
    }
  else
    {
      BNO (p) = 0;
      GENO (p) = false;
    }
}

/* Monadic formulas
   ────────────────

   The value yielded by a monadic formula has the same origin than the single
   operand.  The access is always direct.  */

static void
sprops_for_monadic_formula (NODE_T *p)
{
  ORIGIN (p) = ORIGIN (NEXT (SUB (p)));
  ACCESS (p) = ACCESS_DIR;
}

/* Dyadic formulas
   ───────────────

   If both operands of the formula have the same origin, then that is the
   origin of the value yielded by the formula.  Otherwise a fresh origin is
   created with class NIL.  The value yielded by the formula always have direct
   access.  */

static void
sprops_for_formula (NODE_T *p)
{
  if (IS (SUB (p), MONADIC_FORMULA) && NEXT_SUB (p) == NO_NODE)
    ORIGIN (p) = ORIGIN (SUB (p));
  else
    {
      NODE_T *arg1 = SUB (p);
      NODE_T *arg2 = NEXT (NEXT (SUB (p)));

      if (origin_equal_p (ORIGIN (arg1), ORIGIN (arg2)))
	ORIGIN (p) = ORIGIN (arg1);
      else
	ORIGIN (p) = make_origin (p);
    }
  ACCESS (p) = ACCESS_DIR;
}

/* Identifiers
   ───────────

   Both the access and origin of the value yielded by an applied identifier is
   obtained from the symtab.  If the applied identifier appears before its
   declaration, the symtab will not have the properties installed; in that
   case, an origin is allocated in the symtab.  XXX we need to install a
   pointer to the access in the symtab. */

static void
sprops_for_applied_identifier (NODE_T *p)
{
  if (ORIGIN (TAX (p)) == NO_ORIGIN)
    ORIGIN (TAX (p)) = make_origin (p);
  ORIGIN (p) = ORIGIN (TAX (p));
  ACCESS (p) = ACCESS (TAX (p));
}

/* Assertions
   ──────────

   Assertions introduce a fresh origin of class NIL.  The access is also
   NIL.  */

static void
sprops_for_assertion (NODE_T *p)
{
  ORIGIN (p) = make_origin (p);
  ACCESS (p) = ACCESS_NIL;
}

/* Fill in the static properties for the symtab entry for the identifier
   declared in P, which is a parse node for a declaration.  This function shall
   handle all node types for which a68_is_declaration returns `true'.  */

static void
sprops_for_decl (NODE_T *p)
{
  /* Declarations do not yield values themselves, but we set the origin and
     access of their defining identifiers in their symtab entries.  */

  ORIGIN (p) = NO_ORIGIN;
  ACCESS (p) = NO_ACCESS;

  /* Get the defining identifier.  */
  NODE_T *defining_identifier = NO_NODE;

  switch (ATTRIBUTE (p))
    {
    case IDENTITY_DECLARATION:
      if (IS (SUB (p), IDENTITY_DECLARATION))
	defining_identifier = NEXT (NEXT_SUB (p));
      else if (IS (SUB (p), PUBLIC_SYMBOL))
	defining_identifier = NEXT (NEXT_SUB (p));
      else if (IS (SUB (p), DECLARER))
	defining_identifier = NEXT_SUB (p);
      else
	gcc_unreachable ();
      break;
    case PROCEDURE_DECLARATION:
      if (IS (SUB (p), PROCEDURE_DECLARATION))
	defining_identifier = NEXT (NEXT_SUB (p));
      else if (IS (SUB (p), PUBLIC_SYMBOL))
	defining_identifier = NEXT (NEXT_SUB (p));
      else if (IS (SUB (p), PROC_SYMBOL))
	defining_identifier = NEXT_SUB (p);
      else
	gcc_unreachable ();
      break;
    case BRIEF_OPERATOR_DECLARATION:
      if (IS (SUB (p), BRIEF_OPERATOR_DECLARATION))
	defining_identifier = NEXT (NEXT_SUB (p));
      else if (IS (SUB (p), PUBLIC_SYMBOL))
	defining_identifier = NEXT (NEXT_SUB (p));
      else
	defining_identifier = NEXT_SUB (p);
      break;
    case OPERATOR_DECLARATION:
      if (IS (SUB (p), OPERATOR_DECLARATION))
	defining_identifier = NEXT (NEXT_SUB (p));
      else if (IS (SUB (p), PUBLIC_SYMBOL))
	defining_identifier = NEXT (NEXT_SUB (p));
      else
	defining_identifier = NEXT_SUB (p);
      break;
    case VARIABLE_DECLARATION:
      if (IS (SUB (p), VARIABLE_DECLARATION))
	defining_identifier = NEXT (NEXT_SUB (p));
      else
	{
	  NODE_T *q = SUB (p);

	  if (IS (q, PUBLIC_SYMBOL))
	    FORWARD (q);

	  if (IS (q, QUALIFIER))
	    defining_identifier = NEXT (NEXT (q));
	  else if (IS (q, DECLARER))
	    defining_identifier = NEXT (q);
	  else
	    gcc_unreachable ();
	}
      break;
    case PROCEDURE_VARIABLE_DECLARATION:
      if (IS (SUB (p), PROCEDURE_VARIABLE_DECLARATION))
	defining_identifier = NEXT (NEXT_SUB (p));
      else
	{
	  NODE_T *q = SUB (p);

	  if (IS (q, PUBLIC_SYMBOL))
	    FORWARD (q);

	  if (IS (q, PROC_SYMBOL))
	    defining_identifier = NEXT (q);
	  else if (IS (q, QUALIFIER))
	    defining_identifier = NEXT (NEXT (q));
	  else
	    gcc_unreachable ();
	}
      break;
    default:
      break;
    }

  /* Set the static attributes in the symtab for the defining identifier.  */
  if (defining_identifier != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case IDENTITY_DECLARATION:
	case PROCEDURE_DECLARATION:
	  {
	    TAG_T *tax = TAX (defining_identifier);
	    if (ORIGIN (tax) == NO_ORIGIN)
	      ORIGIN (tax) = make_origin (p);
	    KINDO (tax) = KINDO_IDE;
	    BNO (tax) = LEX_LEVEL (defining_identifier);
	    ACCESS (tax) = ACCESS_DIR;
	    break;
	  }
	case VARIABLE_DECLARATION:
	case PROCEDURE_VARIABLE_DECLARATION:
	  {
	    TAG_T *tax = TAX (defining_identifier);
	    if (ORIGIN (tax) == NO_ORIGIN)
	      ORIGIN (tax) = make_origin (p);
	    KINDO (tax) = KINDO_VAR;
	    BNO (tax) = LEX_LEVEL (defining_identifier);
	    ACCESS (tax) = ACCESS_VAR;
	    break;
	  }
	case MODE_DECLARATION:
	case PRIORITY_DECLARATION:
	case BRIEF_OPERATOR_DECLARATION:
	case OPERATOR_DECLARATION:
	  /* These declarations do not ascribe values to identifiers so there
	     is nothing to do.  */
	  break;
	default:
	  gcc_unreachable ();
	}
    }
}

/* Fill in the static properties for P, which is a parse node for a construct
   yielding some value.  This function shall handle all node types for which
   a68_yields_value returns `true'.  */

static void
sprops_for_unit (NODE_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case UNIT:
    case PRIMARY:
    case SECONDARY:
    case TERTIARY:
      ORIGIN (p) = ORIGIN (SUB (p));
      break;
    case FORMAL_HOLE:
    case JUMP:
    case SKIP:
    case NIHIL:
    case EMPTY_SYMBOL:
    case ROUTINE_TEXT:
      ORIGIN (p) = make_origin (p);
      ACCESS (p) = ACCESS_DIR;
      break;
    case DENOTATION:
      sprops_for_denotation (p);
      break;
    case IDENTIFIER:
      sprops_for_applied_identifier (p);
      break;
    case MONADIC_FORMULA:
      sprops_for_monadic_formula (p);
      break;
    case FORMULA:
      sprops_for_formula (p);
      break;
    case GENERATOR:
      sprops_for_generator (p);
      break;
    case SELECTION:
      sprops_for_selection (p);
      break;
    case DEREFERENCING:
      sprops_for_dereferencing (p);
      break;
    case DEPROCEDURING:
      sprops_for_deproceduring (p);
      break;
    case PROCEDURING:
      sprops_for_proceduring (p);
      break;
    case SLICE:
      sprops_for_slice (p);
      break;
    case WIDENING:
      sprops_for_widening (p);
      break;
    case UNITING:
      sprops_for_uniting (p);
      break;
    case ROWING:
      sprops_for_rowing (p);
      break;
    case VOIDING:
      sprops_for_voiding (p);
      break;
    case ASSIGNATION:
      sprops_for_assignation (p);
      break;
    case IDENTITY_RELATION:
      sprops_for_identity_relation (p);
      break;
    case ACCESS_CLAUSE:
      sprops_for_access_clause (p);
      break;
    case ENQUIRY_CLAUSE:
      sprops_for_enquiry_clause (p);
      break;
    case LOOP_CLAUSE:
      sprops_for_loop_clause (p);
      break;
    case ENCLOSED_CLAUSE:
      sprops_for_enclosed_clause (p);
      break;
    case SERIAL_CLAUSE:
      sprops_for_serial_clause (p);
      break;
    case CLOSED_CLAUSE:
      sprops_for_closed_clause (p);
      break;
    case PARALLEL_CLAUSE:
      sprops_for_parallel_clause (p);
      break;
    case CONDITIONAL_CLAUSE:
      sprops_for_conditional_clause (p);
      break;
    case CONFORMITY_CLAUSE:
      sprops_for_conformity_clause (p);
      break;
    case CASE_CLAUSE:
      sprops_for_case_clause (p);
      break;
    case COLLATERAL_CLAUSE:
      sprops_for_collateral_clause (p);
      break;
    case CALL:
      sprops_for_call (p);
      break;
    case AND_FUNCTION:
    case OR_FUNCTION:
      sprops_for_logical_function (p);
      break;
    case ASSERTION:
      sprops_for_assertion (p);
      break;
    case CAST:
      sprops_for_cast (p);
      break;
    default:
      gcc_unreachable ();
    }

  /* Sanity check.  */
  gcc_assert (ACCESS (p) != NO_ACCESS);
  gcc_assert (ORIGIN (p) != NO_ORIGIN);
}

/* Entry point for the sprops parser pass.  */

void
a68_sprops (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      a68_sprops (SUB (p));

      if (a68_yields_value (p))
	sprops_for_unit (p);
      else if (a68_is_declaration (p))
	sprops_for_decl (p);
      else
	{
	  ORIGIN (p) = NO_ORIGIN;
	  ACCESS (p) = NO_ACCESS;
	}
    }
}
