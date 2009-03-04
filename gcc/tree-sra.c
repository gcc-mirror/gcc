/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "flags.h"
#include "bitmap.h"
#include "obstack.h"
#include "target.h"
/* expr.h is needed for MOVE_RATIO.  */
#include "expr.h"
#include "params.h"


/* This object of this pass is to replace a non-addressable aggregate with a
   set of independent variables.  Most of the time, all of these variables
   will be scalars.  But a secondary objective is to break up larger
   aggregates into smaller aggregates.  In the process we may find that some
   bits of the larger aggregate can be deleted as unreferenced.

   This substitution is done globally.  More localized substitutions would
   be the purvey of a load-store motion pass.

   The optimization proceeds in phases:

     (1) Identify variables that have types that are candidates for
	 decomposition.

     (2) Scan the function looking for the ways these variables are used.
	 In particular we're interested in the number of times a variable
	 (or member) is needed as a complete unit, and the number of times
	 a variable (or member) is copied.

     (3) Based on the usage profile, instantiate substitution variables.

     (4) Scan the function making replacements.
*/


/* True if this is the "early" pass, before inlining.  */
static bool early_sra;

/* The set of todo flags to return from tree_sra.  */
static unsigned int todoflags;

/* The set of aggregate variables that are candidates for scalarization.  */
static bitmap sra_candidates;

/* Set of scalarizable PARM_DECLs that need copy-in operations at the
   beginning of the function.  */
static bitmap needs_copy_in;

/* Sets of bit pairs that cache type decomposition and instantiation.  */
static bitmap sra_type_decomp_cache;
static bitmap sra_type_inst_cache;

/* One of these structures is created for each candidate aggregate and
   each (accessed) member or group of members of such an aggregate.  */
struct sra_elt
{
  /* A tree of the elements.  Used when we want to traverse everything.  */
  struct sra_elt *parent;
  struct sra_elt *groups;
  struct sra_elt *children;
  struct sra_elt *sibling;

  /* If this element is a root, then this is the VAR_DECL.  If this is
     a sub-element, this is some token used to identify the reference.
     In the case of COMPONENT_REF, this is the FIELD_DECL.  In the case
     of an ARRAY_REF, this is the (constant) index.  In the case of an
     ARRAY_RANGE_REF, this is the (constant) RANGE_EXPR.  In the case
     of a complex number, this is a zero or one.  */
  tree element;

  /* The type of the element.  */
  tree type;

  /* A VAR_DECL, for any sub-element we've decided to replace.  */
  tree replacement;

  /* The number of times the element is referenced as a whole.  I.e.
     given "a.b.c", this would be incremented for C, but not for A or B.  */
  unsigned int n_uses;

  /* The number of times the element is copied to or from another
     scalarizable element.  */
  unsigned int n_copies;

  /* True if TYPE is scalar.  */
  bool is_scalar;

  /* True if this element is a group of members of its parent.  */
  bool is_group;

  /* True if we saw something about this element that prevents scalarization,
     such as non-constant indexing.  */
  bool cannot_scalarize;

  /* True if we've decided that structure-to-structure assignment
     should happen via memcpy and not per-element.  */
  bool use_block_copy;

  /* True if everything under this element has been marked TREE_NO_WARNING.  */
  bool all_no_warning;

  /* A flag for use with/after random access traversals.  */
  bool visited;

  /* True if there is BIT_FIELD_REF on the lhs with a vector. */
  bool is_vector_lhs;

  /* 1 if the element is a field that is part of a block, 2 if the field
     is the block itself, 0 if it's neither.  */
  char in_bitfld_block;
};

#define IS_ELEMENT_FOR_GROUP(ELEMENT) (TREE_CODE (ELEMENT) == RANGE_EXPR)

#define FOR_EACH_ACTUAL_CHILD(CHILD, ELT)			\
  for ((CHILD) = (ELT)->is_group				\
		 ? next_child_for_group (NULL, (ELT))		\
		 : (ELT)->children;				\
       (CHILD);							\
       (CHILD) = (ELT)->is_group				\
		 ? next_child_for_group ((CHILD), (ELT))	\
		 : (CHILD)->sibling)

/* Helper function for above macro.  Return next child in group.  */
static struct sra_elt *
next_child_for_group (struct sra_elt *child, struct sra_elt *group)
{
  gcc_assert (group->is_group);

  /* Find the next child in the parent.  */
  if (child)
    child = child->sibling;
  else
    child = group->parent->children;

  /* Skip siblings that do not belong to the group.  */
  while (child)
    {
      tree g_elt = group->element;
      if (TREE_CODE (g_elt) == RANGE_EXPR)
	{
	  if (!tree_int_cst_lt (child->element, TREE_OPERAND (g_elt, 0))
	      && !tree_int_cst_lt (TREE_OPERAND (g_elt, 1), child->element))
	    break;
	}
      else
	gcc_unreachable ();

      child = child->sibling;
    }

  return child;
}

/* Random access to the child of a parent is performed by hashing.
   This prevents quadratic behavior, and allows SRA to function
   reasonably on larger records.  */
static htab_t sra_map;

/* All structures are allocated out of the following obstack.  */
static struct obstack sra_obstack;

/* Debugging functions.  */
static void dump_sra_elt_name (FILE *, struct sra_elt *);
extern void debug_sra_elt_name (struct sra_elt *);

/* Forward declarations.  */
static tree generate_element_ref (struct sra_elt *);
static gimple_seq sra_build_assignment (tree dst, tree src);
static void mark_all_v_defs_seq (gimple_seq);
static void mark_all_v_defs_stmt (gimple);


/* Return true if DECL is an SRA candidate.  */

static bool
is_sra_candidate_decl (tree decl)
{
  return DECL_P (decl) && bitmap_bit_p (sra_candidates, DECL_UID (decl));
}

/* Return true if TYPE is a scalar type.  */

static bool
is_sra_scalar_type (tree type)
{
  enum tree_code code = TREE_CODE (type);
  return (code == INTEGER_TYPE || code == REAL_TYPE || code == VECTOR_TYPE
	  || code == FIXED_POINT_TYPE
	  || code == ENUMERAL_TYPE || code == BOOLEAN_TYPE
	  || code == POINTER_TYPE || code == OFFSET_TYPE
	  || code == REFERENCE_TYPE);
}

/* Return true if TYPE can be decomposed into a set of independent variables.

   Note that this doesn't imply that all elements of TYPE can be
   instantiated, just that if we decide to break up the type into
   separate pieces that it can be done.  */

bool
sra_type_can_be_decomposed_p (tree type)
{
  unsigned int cache = TYPE_UID (TYPE_MAIN_VARIANT (type)) * 2;
  tree t;

  /* Avoid searching the same type twice.  */
  if (bitmap_bit_p (sra_type_decomp_cache, cache+0))
    return true;
  if (bitmap_bit_p (sra_type_decomp_cache, cache+1))
    return false;

  /* The type must have a definite nonzero size.  */
  if (TYPE_SIZE (type) == NULL || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
      || integer_zerop (TYPE_SIZE (type)))
    goto fail;

  /* The type must be a non-union aggregate.  */
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      {
	bool saw_one_field = false;

	for (t = TYPE_FIELDS (type); t ; t = TREE_CHAIN (t))
	  if (TREE_CODE (t) == FIELD_DECL)
	    {
	      /* Reject incorrectly represented bit fields.  */
	      if (DECL_BIT_FIELD (t)
		  && INTEGRAL_TYPE_P (TREE_TYPE (t))
		  && (tree_low_cst (DECL_SIZE (t), 1)
		      != TYPE_PRECISION (TREE_TYPE (t))))
		goto fail;

	      saw_one_field = true;
	    }

	/* Record types must have at least one field.  */
	if (!saw_one_field)
	  goto fail;
      }
      break;

    case ARRAY_TYPE:
      /* Array types must have a fixed lower and upper bound.  */
      t = TYPE_DOMAIN (type);
      if (t == NULL)
	goto fail;
      if (TYPE_MIN_VALUE (t) == NULL || !TREE_CONSTANT (TYPE_MIN_VALUE (t)))
	goto fail;
      if (TYPE_MAX_VALUE (t) == NULL || !TREE_CONSTANT (TYPE_MAX_VALUE (t)))
	goto fail;
      break;

    case COMPLEX_TYPE:
      break;

    default:
      goto fail;
    }

  bitmap_set_bit (sra_type_decomp_cache, cache+0);
  return true;

 fail:
  bitmap_set_bit (sra_type_decomp_cache, cache+1);
  return false;
}

/* Returns true if the TYPE is one of the available va_list types.
   Otherwise it returns false.
   Note, that for multiple calling conventions there can be more
   than just one va_list type present.  */

static bool
is_va_list_type (tree type)
{
  tree h;

  if (type == NULL_TREE)
    return false;
  h = targetm.canonical_va_list_type (type);
  if (h == NULL_TREE)
    return false;
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (h))
	 return true;
  return false;
}

/* Return true if DECL can be decomposed into a set of independent
   (though not necessarily scalar) variables.  */

static bool
decl_can_be_decomposed_p (tree var)
{
  /* Early out for scalars.  */
  if (is_sra_scalar_type (TREE_TYPE (var)))
    return false;

  /* The variable must not be aliased.  */
  if (!is_gimple_non_addressable (var))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Cannot scalarize variable ");
	  print_generic_expr (dump_file, var, dump_flags);
	  fprintf (dump_file, " because it must live in memory\n");
	}
      return false;
    }

  /* The variable must not be volatile.  */
  if (TREE_THIS_VOLATILE (var))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Cannot scalarize variable ");
	  print_generic_expr (dump_file, var, dump_flags);
	  fprintf (dump_file, " because it is declared volatile\n");
	}
      return false;
    }

  /* We must be able to decompose the variable's type.  */
  if (!sra_type_can_be_decomposed_p (TREE_TYPE (var)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Cannot scalarize variable ");
	  print_generic_expr (dump_file, var, dump_flags);
	  fprintf (dump_file, " because its type cannot be decomposed\n");
	}
      return false;
    }

  /* HACK: if we decompose a va_list_type_node before inlining, then we'll
     confuse tree-stdarg.c, and we won't be able to figure out which and
     how many arguments are accessed.  This really should be improved in
     tree-stdarg.c, as the decomposition is truly a win.  This could also
     be fixed if the stdarg pass ran early, but this can't be done until
     we've aliasing information early too.  See PR 30791.  */
  if (early_sra && is_va_list_type (TREE_TYPE (var)))
    return false;

  return true;
}

/* Return true if TYPE can be *completely* decomposed into scalars.  */

static bool
type_can_instantiate_all_elements (tree type)
{
  if (is_sra_scalar_type (type))
    return true;
  if (!sra_type_can_be_decomposed_p (type))
    return false;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      {
	unsigned int cache = TYPE_UID (TYPE_MAIN_VARIANT (type)) * 2;
	tree f;

	if (bitmap_bit_p (sra_type_inst_cache, cache+0))
	  return true;
	if (bitmap_bit_p (sra_type_inst_cache, cache+1))
	  return false;

	for (f = TYPE_FIELDS (type); f ; f = TREE_CHAIN (f))
	  if (TREE_CODE (f) == FIELD_DECL)
	    {
	      if (!type_can_instantiate_all_elements (TREE_TYPE (f)))
		{
		  bitmap_set_bit (sra_type_inst_cache, cache+1);
		  return false;
		}
	    }

	bitmap_set_bit (sra_type_inst_cache, cache+0);
	return true;
      }

    case ARRAY_TYPE:
      return type_can_instantiate_all_elements (TREE_TYPE (type));

    case COMPLEX_TYPE:
      return true;

    default:
      gcc_unreachable ();
    }
}

/* Test whether ELT or some sub-element cannot be scalarized.  */

static bool
can_completely_scalarize_p (struct sra_elt *elt)
{
  struct sra_elt *c;

  if (elt->cannot_scalarize)
    return false;

  for (c = elt->children; c; c = c->sibling)
    if (!can_completely_scalarize_p (c))
      return false;

  for (c = elt->groups; c; c = c->sibling)
    if (!can_completely_scalarize_p (c))
      return false;

  return true;
}


/* A simplified tree hashing algorithm that only handles the types of
   trees we expect to find in sra_elt->element.  */

static hashval_t
sra_hash_tree (tree t)
{
  hashval_t h;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      h = DECL_UID (t);
      break;

    case INTEGER_CST:
      h = TREE_INT_CST_LOW (t) ^ TREE_INT_CST_HIGH (t);
      break;

    case RANGE_EXPR:
      h = iterative_hash_expr (TREE_OPERAND (t, 0), 0);
      h = iterative_hash_expr (TREE_OPERAND (t, 1), h);
      break;

    case FIELD_DECL:
      /* We can have types that are compatible, but have different member
	 lists, so we can't hash fields by ID.  Use offsets instead.  */
      h = iterative_hash_expr (DECL_FIELD_OFFSET (t), 0);
      h = iterative_hash_expr (DECL_FIELD_BIT_OFFSET (t), h);
      break;

    case BIT_FIELD_REF:
      /* Don't take operand 0 into account, that's our parent.  */
      h = iterative_hash_expr (TREE_OPERAND (t, 1), 0);
      h = iterative_hash_expr (TREE_OPERAND (t, 2), h);
      break;

    default:
      gcc_unreachable ();
    }

  return h;
}

/* Hash function for type SRA_PAIR.  */

static hashval_t
sra_elt_hash (const void *x)
{
  const struct sra_elt *const e = (const struct sra_elt *) x;
  const struct sra_elt *p;
  hashval_t h;

  h = sra_hash_tree (e->element);

  /* Take into account everything except bitfield blocks back up the
     chain.  Given that chain lengths are rarely very long, this
     should be acceptable.  If we truly identify this as a performance
     problem, it should work to hash the pointer value
     "e->parent".  */
  for (p = e->parent; p ; p = p->parent)
    if (!p->in_bitfld_block)
      h = (h * 65521) ^ sra_hash_tree (p->element);

  return h;
}

/* Equality function for type SRA_PAIR.  */

static int
sra_elt_eq (const void *x, const void *y)
{
  const struct sra_elt *const a = (const struct sra_elt *) x;
  const struct sra_elt *const b = (const struct sra_elt *) y;
  tree ae, be;
  const struct sra_elt *ap = a->parent;
  const struct sra_elt *bp = b->parent;

  if (ap)
    while (ap->in_bitfld_block)
      ap = ap->parent;
  if (bp)
    while (bp->in_bitfld_block)
      bp = bp->parent;

  if (ap != bp)
    return false;

  ae = a->element;
  be = b->element;

  if (ae == be)
    return true;
  if (TREE_CODE (ae) != TREE_CODE (be))
    return false;

  switch (TREE_CODE (ae))
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* These are all pointer unique.  */
      return false;

    case INTEGER_CST:
      /* Integers are not pointer unique, so compare their values.  */
      return tree_int_cst_equal (ae, be);

    case RANGE_EXPR:
      return
	tree_int_cst_equal (TREE_OPERAND (ae, 0), TREE_OPERAND (be, 0))
	&& tree_int_cst_equal (TREE_OPERAND (ae, 1), TREE_OPERAND (be, 1));

    case FIELD_DECL:
      /* Fields are unique within a record, but not between
	 compatible records.  */
      if (DECL_FIELD_CONTEXT (ae) == DECL_FIELD_CONTEXT (be))
	return false;
      return fields_compatible_p (ae, be);

    case BIT_FIELD_REF:
      return
	tree_int_cst_equal (TREE_OPERAND (ae, 1), TREE_OPERAND (be, 1))
	&& tree_int_cst_equal (TREE_OPERAND (ae, 2), TREE_OPERAND (be, 2));

    default:
      gcc_unreachable ();
    }
}

/* Create or return the SRA_ELT structure for CHILD in PARENT.  PARENT
   may be null, in which case CHILD must be a DECL.  */

static struct sra_elt *
lookup_element (struct sra_elt *parent, tree child, tree type,
		enum insert_option insert)
{
  struct sra_elt dummy;
  struct sra_elt **slot;
  struct sra_elt *elt;

  if (parent)
    dummy.parent = parent->is_group ? parent->parent : parent;
  else
    dummy.parent = NULL;
  dummy.element = child;

  slot = (struct sra_elt **) htab_find_slot (sra_map, &dummy, insert);
  if (!slot && insert == NO_INSERT)
    return NULL;

  elt = *slot;
  if (!elt && insert == INSERT)
    {
      *slot = elt = XOBNEW (&sra_obstack, struct sra_elt);
      memset (elt, 0, sizeof (*elt));

      elt->parent = parent;
      elt->element = child;
      elt->type = type;
      elt->is_scalar = is_sra_scalar_type (type);

      if (parent)
	{
	  if (IS_ELEMENT_FOR_GROUP (elt->element))
	    {
	      elt->is_group = true;
	      elt->sibling = parent->groups;
	      parent->groups = elt;
	    }
	  else
	    {
	      elt->sibling = parent->children;
	      parent->children = elt;
	    }
	}

      /* If this is a parameter, then if we want to scalarize, we have
	 one copy from the true function parameter.  Count it now.  */
      if (TREE_CODE (child) == PARM_DECL)
	{
	  elt->n_copies = 1;
	  bitmap_set_bit (needs_copy_in, DECL_UID (child));
	}
    }

  return elt;
}

/* Create or return the SRA_ELT structure for EXPR if the expression
   refers to a scalarizable variable.  */

static struct sra_elt *
maybe_lookup_element_for_expr (tree expr)
{
  struct sra_elt *elt;
  tree child;

  switch (TREE_CODE (expr))
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      if (is_sra_candidate_decl (expr))
	return lookup_element (NULL, expr, TREE_TYPE (expr), INSERT);
      return NULL;

    case ARRAY_REF:
      /* We can't scalarize variable array indices.  */
      if (in_array_bounds_p (expr))
        child = TREE_OPERAND (expr, 1);
      else
	return NULL;
      break;

    case ARRAY_RANGE_REF:
      /* We can't scalarize variable array indices.  */
      if (range_in_array_bounds_p (expr))
	{
	  tree domain = TYPE_DOMAIN (TREE_TYPE (expr));
	  child = build2 (RANGE_EXPR, integer_type_node,
			  TYPE_MIN_VALUE (domain), TYPE_MAX_VALUE (domain));
	}
      else
	return NULL;
      break;

    case COMPONENT_REF:
      {
	tree type = TREE_TYPE (TREE_OPERAND (expr, 0));
	/* Don't look through unions.  */
	if (TREE_CODE (type) != RECORD_TYPE)
	  return NULL;
	/* Neither through variable-sized records.  */
	if (TYPE_SIZE (type) == NULL_TREE
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return NULL;
	child = TREE_OPERAND (expr, 1);
      }
      break;

    case REALPART_EXPR:
      child = integer_zero_node;
      break;
    case IMAGPART_EXPR:
      child = integer_one_node;
      break;

    default:
      return NULL;
    }

  elt = maybe_lookup_element_for_expr (TREE_OPERAND (expr, 0));
  if (elt)
    return lookup_element (elt, child, TREE_TYPE (expr), INSERT);
  return NULL;
}


/* Functions to walk just enough of the tree to see all scalarizable
   references, and categorize them.  */

/* A set of callbacks for phases 2 and 4.  They'll be invoked for the
   various kinds of references seen.  In all cases, *GSI is an iterator
   pointing to the statement being processed.  */
struct sra_walk_fns
{
  /* Invoked when ELT is required as a unit.  Note that ELT might refer to
     a leaf node, in which case this is a simple scalar reference.  *EXPR_P
     points to the location of the expression.  IS_OUTPUT is true if this
     is a left-hand-side reference.  USE_ALL is true if we saw something we
     couldn't quite identify and had to force the use of the entire object.  */
  void (*use) (struct sra_elt *elt, tree *expr_p,
	       gimple_stmt_iterator *gsi, bool is_output, bool use_all);

  /* Invoked when we have a copy between two scalarizable references.  */
  void (*copy) (struct sra_elt *lhs_elt, struct sra_elt *rhs_elt,
		gimple_stmt_iterator *gsi);

  /* Invoked when ELT is initialized from a constant.  VALUE may be NULL,
     in which case it should be treated as an empty CONSTRUCTOR.  */
  void (*init) (struct sra_elt *elt, tree value, gimple_stmt_iterator *gsi);

  /* Invoked when we have a copy between one scalarizable reference ELT
     and one non-scalarizable reference OTHER without side-effects. 
     IS_OUTPUT is true if ELT is on the left-hand side.  */
  void (*ldst) (struct sra_elt *elt, tree other,
		gimple_stmt_iterator *gsi, bool is_output);

  /* True during phase 2, false during phase 4.  */
  /* ??? This is a hack.  */
  bool initial_scan;
};

#ifdef ENABLE_CHECKING
/* Invoked via walk_tree, if *TP contains a candidate decl, return it.  */

static tree
sra_find_candidate_decl (tree *tp, int *walk_subtrees,
			 void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;
  enum tree_code code = TREE_CODE (t);

  if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    {
      *walk_subtrees = 0;
      if (is_sra_candidate_decl (t))
	return t;
    }
  else if (TYPE_P (t))
    *walk_subtrees = 0;

  return NULL;
}
#endif

/* Walk most expressions looking for a scalarizable aggregate.
   If we find one, invoke FNS->USE.  */

static void
sra_walk_expr (tree *expr_p, gimple_stmt_iterator *gsi, bool is_output,
	       const struct sra_walk_fns *fns)
{
  tree expr = *expr_p;
  tree inner = expr;
  bool disable_scalarization = false;
  bool use_all_p = false;

  /* We're looking to collect a reference expression between EXPR and INNER,
     such that INNER is a scalarizable decl and all other nodes through EXPR
     are references that we can scalarize.  If we come across something that
     we can't scalarize, we reset EXPR.  This has the effect of making it
     appear that we're referring to the larger expression as a whole.  */

  while (1)
    switch (TREE_CODE (inner))
      {
      case VAR_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	/* If there is a scalarizable decl at the bottom, then process it.  */
	if (is_sra_candidate_decl (inner))
	  {
	    struct sra_elt *elt = maybe_lookup_element_for_expr (expr);
	    if (disable_scalarization)
	      elt->cannot_scalarize = true;
	    else
	      fns->use (elt, expr_p, gsi, is_output, use_all_p);
	  }
	return;

      case ARRAY_REF:
	/* Non-constant index means any member may be accessed.  Prevent the
	   expression from being scalarized.  If we were to treat this as a
	   reference to the whole array, we can wind up with a single dynamic
	   index reference inside a loop being overridden by several constant
	   index references during loop setup.  It's possible that this could
	   be avoided by using dynamic usage counts based on BB trip counts
	   (based on loop analysis or profiling), but that hardly seems worth
	   the effort.  */
	/* ??? Hack.  Figure out how to push this into the scan routines
	   without duplicating too much code.  */
	if (!in_array_bounds_p (inner))
	  {
	    disable_scalarization = true;
	    goto use_all;
	  }
	/* ??? Are we assured that non-constant bounds and stride will have
	   the same value everywhere?  I don't think Fortran will...  */
	if (TREE_OPERAND (inner, 2) || TREE_OPERAND (inner, 3))
	  goto use_all;
	inner = TREE_OPERAND (inner, 0);
	break;

      case ARRAY_RANGE_REF:
	if (!range_in_array_bounds_p (inner))
	  {
	    disable_scalarization = true;
	    goto use_all;
	  }
	/* ??? See above non-constant bounds and stride .  */
	if (TREE_OPERAND (inner, 2) || TREE_OPERAND (inner, 3))
	  goto use_all;
	inner = TREE_OPERAND (inner, 0);
	break;

      case COMPONENT_REF:
	{
	  tree type = TREE_TYPE (TREE_OPERAND (inner, 0));
	  /* Don't look through unions.  */
	  if (TREE_CODE (type) != RECORD_TYPE)
	    goto use_all;
	  /* Neither through variable-sized records.  */
	  if (TYPE_SIZE (type) == NULL_TREE
	      || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	    goto use_all;
	  inner = TREE_OPERAND (inner, 0);
	}
	break;

      case REALPART_EXPR:
      case IMAGPART_EXPR:
	inner = TREE_OPERAND (inner, 0);
	break;

      case BIT_FIELD_REF:
	/* A bit field reference to a specific vector is scalarized but for
	   ones for inputs need to be marked as used on the left hand size so
	   when we scalarize it, we can mark that variable as non renamable.  */
	if (is_output
	    && TREE_CODE (TREE_TYPE (TREE_OPERAND (inner, 0))) == VECTOR_TYPE)
	  {
	    struct sra_elt *elt
	      = maybe_lookup_element_for_expr (TREE_OPERAND (inner, 0));
	    if (elt)
	      elt->is_vector_lhs = true;
	  }

	/* A bit field reference (access to *multiple* fields simultaneously)
	   is not currently scalarized.  Consider this an access to the full
	   outer element, to which walk_tree will bring us next.  */
	goto use_all;

      CASE_CONVERT:
	/* Similarly, a nop explicitly wants to look at an object in a
	   type other than the one we've scalarized.  */
	goto use_all;

      case VIEW_CONVERT_EXPR:
	/* Likewise for a view conversion, but with an additional twist:
	   it can be on the LHS and, in this case, an access to the full
	   outer element would mean a killing def.  So we need to punt
	   if we haven't already a full access to the current element,
	   because we cannot pretend to have a killing def if we only
	   have a partial access at some level.  */
	if (is_output && !use_all_p && inner != expr)
	  disable_scalarization = true;
	goto use_all;

      case WITH_SIZE_EXPR:
	/* This is a transparent wrapper.  The entire inner expression really
	   is being used.  */
	goto use_all;

      use_all:
        expr_p = &TREE_OPERAND (inner, 0);
	inner = expr = *expr_p;
	use_all_p = true;
	break;

      default:
#ifdef ENABLE_CHECKING
	/* Validate that we're not missing any references.  */
	gcc_assert (!walk_tree (&inner, sra_find_candidate_decl, NULL, NULL));
#endif
	return;
      }
}

/* Walk the arguments of a GIMPLE_CALL looking for scalarizable aggregates.
   If we find one, invoke FNS->USE.  */

static void
sra_walk_gimple_call (gimple stmt, gimple_stmt_iterator *gsi,
		    const struct sra_walk_fns *fns)
{
  int i;
  int nargs = gimple_call_num_args (stmt);

  for (i = 0; i < nargs; i++)
    sra_walk_expr (gimple_call_arg_ptr (stmt, i), gsi, false, fns);

  if (gimple_call_lhs (stmt))
    sra_walk_expr (gimple_call_lhs_ptr (stmt), gsi, true, fns);
}

/* Walk the inputs and outputs of a GIMPLE_ASM looking for scalarizable
   aggregates.  If we find one, invoke FNS->USE.  */

static void
sra_walk_gimple_asm (gimple stmt, gimple_stmt_iterator *gsi,
		   const struct sra_walk_fns *fns)
{
  size_t i;
  for (i = 0; i < gimple_asm_ninputs (stmt); i++)
    sra_walk_expr (&TREE_VALUE (gimple_asm_input_op (stmt, i)), gsi, false, fns);
  for (i = 0; i < gimple_asm_noutputs (stmt); i++)
    sra_walk_expr (&TREE_VALUE (gimple_asm_output_op (stmt, i)), gsi, true, fns);
}

/* Walk a GIMPLE_ASSIGN and categorize the assignment appropriately.  */

static void
sra_walk_gimple_assign (gimple stmt, gimple_stmt_iterator *gsi,
			const struct sra_walk_fns *fns)
{
  struct sra_elt *lhs_elt = NULL, *rhs_elt = NULL;
  tree lhs, rhs;

  /* If there is more than 1 element on the RHS, only walk the lhs.  */
  if (!gimple_assign_single_p (stmt))
    {
      sra_walk_expr (gimple_assign_lhs_ptr (stmt), gsi, true, fns);
      return;
    }

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  lhs_elt = maybe_lookup_element_for_expr (lhs);
  rhs_elt = maybe_lookup_element_for_expr (rhs);

  /* If both sides are scalarizable, this is a COPY operation.  */
  if (lhs_elt && rhs_elt)
    {
      fns->copy (lhs_elt, rhs_elt, gsi);
      return;
    }

  /* If the RHS is scalarizable, handle it.  There are only two cases.  */
  if (rhs_elt)
    {
      if (!rhs_elt->is_scalar && !TREE_SIDE_EFFECTS (lhs))
	fns->ldst (rhs_elt, lhs, gsi, false);
      else
	fns->use (rhs_elt, gimple_assign_rhs1_ptr (stmt), gsi, false, false);
    }

  /* If it isn't scalarizable, there may be scalarizable variables within, so
     check for a call or else walk the RHS to see if we need to do any
     copy-in operations.  We need to do it before the LHS is scalarized so
     that the statements get inserted in the proper place, before any
     copy-out operations.  */
  else
    sra_walk_expr (gimple_assign_rhs1_ptr (stmt), gsi, false, fns);

  /* Likewise, handle the LHS being scalarizable.  We have cases similar
     to those above, but also want to handle RHS being constant.  */
  if (lhs_elt)
    {
      /* If this is an assignment from a constant, or constructor, then
	 we have access to all of the elements individually.  Invoke INIT.  */
      if (TREE_CODE (rhs) == COMPLEX_EXPR
	  || TREE_CODE (rhs) == COMPLEX_CST
	  || TREE_CODE (rhs) == CONSTRUCTOR)
	fns->init (lhs_elt, rhs, gsi);

      /* If this is an assignment from read-only memory, treat this as if
	 we'd been passed the constructor directly.  Invoke INIT.  */
      else if (TREE_CODE (rhs) == VAR_DECL
	       && TREE_STATIC (rhs)
	       && !DECL_EXTERNAL (rhs)
	       && TREE_READONLY (rhs)
	       && targetm.binds_local_p (rhs))
	fns->init (lhs_elt, DECL_INITIAL (rhs), gsi);

      /* If this is a copy from a non-scalarizable lvalue, invoke LDST.
	 The lvalue requirement prevents us from trying to directly scalarize
	 the result of a function call.  Which would result in trying to call
	 the function multiple times, and other evil things.  */
      else if (!lhs_elt->is_scalar
	       && !TREE_SIDE_EFFECTS (rhs) && is_gimple_addressable (rhs))
	fns->ldst (lhs_elt, rhs, gsi, true);

      /* Otherwise we're being used in some context that requires the
	 aggregate to be seen as a whole.  Invoke USE.  */
      else
	fns->use (lhs_elt, gimple_assign_lhs_ptr (stmt), gsi, true, false);
    }

  /* Similarly to above, LHS_ELT being null only means that the LHS as a
     whole is not a scalarizable reference.  There may be occurrences of
     scalarizable variables within, which implies a USE.  */
  else
    sra_walk_expr (gimple_assign_lhs_ptr (stmt), gsi, true, fns);
}

/* Entry point to the walk functions.  Search the entire function,
   invoking the callbacks in FNS on each of the references to
   scalarizable variables.  */

static void
sra_walk_function (const struct sra_walk_fns *fns)
{
  basic_block bb;
  gimple_stmt_iterator si, ni;

  /* ??? Phase 4 could derive some benefit to walking the function in
     dominator tree order.  */

  FOR_EACH_BB (bb)
    for (si = gsi_start_bb (bb); !gsi_end_p (si); si = ni)
      {
	gimple stmt;

	stmt = gsi_stmt (si);

	ni = si;
	gsi_next (&ni);

	/* If the statement has no virtual operands, then it doesn't
	   make any structure references that we care about.  */
	if (gimple_aliases_computed_p (cfun)
	    && ZERO_SSA_OPERANDS (stmt, (SSA_OP_VIRTUAL_DEFS | SSA_OP_VUSE)))
	      continue;

	switch (gimple_code (stmt))
	  {
	  case GIMPLE_RETURN:
	    /* If we have "return <retval>" then the return value is
	       already exposed for our pleasure.  Walk it as a USE to
	       force all the components back in place for the return.
	       */
	    if (gimple_return_retval (stmt)  == NULL_TREE)
	      ;
	    else
	      sra_walk_expr (gimple_return_retval_ptr (stmt), &si, false,
                             fns);
	    break;

	  case GIMPLE_ASSIGN:
	    sra_walk_gimple_assign (stmt, &si, fns);
	    break;
	  case GIMPLE_CALL:
	    sra_walk_gimple_call (stmt, &si, fns);
	    break;
	  case GIMPLE_ASM:
	    sra_walk_gimple_asm (stmt, &si, fns);
	    break;

	  default:
	    break;
	  }
      }
}

/* Phase One: Scan all referenced variables in the program looking for
   structures that could be decomposed.  */

static bool
find_candidates_for_sra (void)
{
  bool any_set = false;
  tree var;
  referenced_var_iterator rvi;

  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      if (decl_can_be_decomposed_p (var))
        {
          bitmap_set_bit (sra_candidates, DECL_UID (var));
          any_set = true;
        }
    }

  return any_set;
}


/* Phase Two: Scan all references to scalarizable variables.  Count the
   number of times they are used or copied respectively.  */

/* Callbacks to fill in SRA_WALK_FNS.  Everything but USE is
   considered a copy, because we can decompose the reference such that
   the sub-elements needn't be contiguous.  */

static void
scan_use (struct sra_elt *elt, tree *expr_p ATTRIBUTE_UNUSED,
	  gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
	  bool is_output ATTRIBUTE_UNUSED, bool use_all ATTRIBUTE_UNUSED)
{
  elt->n_uses += 1;
}

static void
scan_copy (struct sra_elt *lhs_elt, struct sra_elt *rhs_elt,
	   gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED)
{
  lhs_elt->n_copies += 1;
  rhs_elt->n_copies += 1;
}

static void
scan_init (struct sra_elt *lhs_elt, tree rhs ATTRIBUTE_UNUSED,
	   gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED)
{
  lhs_elt->n_copies += 1;
}

static void
scan_ldst (struct sra_elt *elt, tree other ATTRIBUTE_UNUSED,
	   gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
	   bool is_output ATTRIBUTE_UNUSED)
{
  elt->n_copies += 1;
}

/* Dump the values we collected during the scanning phase.  */

static void
scan_dump (struct sra_elt *elt)
{
  struct sra_elt *c;

  dump_sra_elt_name (dump_file, elt);
  fprintf (dump_file, ": n_uses=%u n_copies=%u\n", elt->n_uses, elt->n_copies);

  for (c = elt->children; c ; c = c->sibling)
    scan_dump (c);

  for (c = elt->groups; c ; c = c->sibling)
    scan_dump (c);
}

/* Entry point to phase 2.  Scan the entire function, building up
   scalarization data structures, recording copies and uses.  */

static void
scan_function (void)
{
  static const struct sra_walk_fns fns = {
    scan_use, scan_copy, scan_init, scan_ldst, true
  };
  bitmap_iterator bi;

  sra_walk_function (&fns);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      unsigned i;

      fputs ("\nScan results:\n", dump_file);
      EXECUTE_IF_SET_IN_BITMAP (sra_candidates, 0, i, bi)
	{
	  tree var = referenced_var (i);
	  struct sra_elt *elt = lookup_element (NULL, var, NULL, NO_INSERT);
	  if (elt)
	    scan_dump (elt);
	}
      fputc ('\n', dump_file);
    }
}

/* Phase Three: Make decisions about which variables to scalarize, if any.
   All elements to be scalarized have replacement variables made for them.  */

/* A subroutine of build_element_name.  Recursively build the element
   name on the obstack.  */

static void
build_element_name_1 (struct sra_elt *elt)
{
  tree t;
  char buffer[32];

  if (elt->parent)
    {
      build_element_name_1 (elt->parent);
      obstack_1grow (&sra_obstack, '$');

      if (TREE_CODE (elt->parent->type) == COMPLEX_TYPE)
	{
	  if (elt->element == integer_zero_node)
	    obstack_grow (&sra_obstack, "real", 4);
	  else
	    obstack_grow (&sra_obstack, "imag", 4);
	  return;
	}
    }

  t = elt->element;
  if (TREE_CODE (t) == INTEGER_CST)
    {
      /* ??? Eh.  Don't bother doing double-wide printing.  */
      sprintf (buffer, HOST_WIDE_INT_PRINT_DEC, TREE_INT_CST_LOW (t));
      obstack_grow (&sra_obstack, buffer, strlen (buffer));
    }
  else if (TREE_CODE (t) == BIT_FIELD_REF)
    {
      sprintf (buffer, "B" HOST_WIDE_INT_PRINT_DEC,
	       tree_low_cst (TREE_OPERAND (t, 2), 1));
      obstack_grow (&sra_obstack, buffer, strlen (buffer));
      sprintf (buffer, "F" HOST_WIDE_INT_PRINT_DEC,
	       tree_low_cst (TREE_OPERAND (t, 1), 1));
      obstack_grow (&sra_obstack, buffer, strlen (buffer));
    }
  else
    {
      tree name = DECL_NAME (t);
      if (name)
	obstack_grow (&sra_obstack, IDENTIFIER_POINTER (name),
		      IDENTIFIER_LENGTH (name));
      else
	{
	  sprintf (buffer, "D%u", DECL_UID (t));
	  obstack_grow (&sra_obstack, buffer, strlen (buffer));
	}
    }
}

/* Construct a pretty variable name for an element's replacement variable.
   The name is built on the obstack.  */

static char *
build_element_name (struct sra_elt *elt)
{
  build_element_name_1 (elt);
  obstack_1grow (&sra_obstack, '\0');
  return XOBFINISH (&sra_obstack, char *);
}

/* Instantiate an element as an independent variable.  */

static void
instantiate_element (struct sra_elt *elt)
{
  struct sra_elt *base_elt;
  tree var, base;
  bool nowarn = TREE_NO_WARNING (elt->element);

  for (base_elt = elt; base_elt->parent; base_elt = base_elt->parent)
    if (!nowarn)
      nowarn = TREE_NO_WARNING (base_elt->parent->element);
  base = base_elt->element;

  elt->replacement = var = make_rename_temp (elt->type, "SR");

  if (DECL_P (elt->element)
      && !tree_int_cst_equal (DECL_SIZE (var), DECL_SIZE (elt->element)))
    {
      DECL_SIZE (var) = DECL_SIZE (elt->element);
      DECL_SIZE_UNIT (var) = DECL_SIZE_UNIT (elt->element);

      elt->in_bitfld_block = 1;
      elt->replacement = fold_build3 (BIT_FIELD_REF, elt->type, var,
				      DECL_SIZE (var),
				      BYTES_BIG_ENDIAN
				      ? size_binop (MINUS_EXPR,
						    TYPE_SIZE (elt->type),
						    DECL_SIZE (var))
				      : bitsize_int (0));
    }

  /* For vectors, if used on the left hand side with BIT_FIELD_REF,
     they are not a gimple register.  */
  if (TREE_CODE (TREE_TYPE (var)) == VECTOR_TYPE && elt->is_vector_lhs)
    DECL_GIMPLE_REG_P (var) = 0;

  DECL_SOURCE_LOCATION (var) = DECL_SOURCE_LOCATION (base);
  DECL_ARTIFICIAL (var) = 1;

  if (TREE_THIS_VOLATILE (elt->type))
    {
      TREE_THIS_VOLATILE (var) = 1;
      TREE_SIDE_EFFECTS (var) = 1;
    }

  if (DECL_NAME (base) && !DECL_IGNORED_P (base))
    {
      char *pretty_name = build_element_name (elt);
      DECL_NAME (var) = get_identifier (pretty_name);
      obstack_free (&sra_obstack, pretty_name);

      SET_DECL_DEBUG_EXPR (var, generate_element_ref (elt));
      DECL_DEBUG_EXPR_IS_FROM (var) = 1;
      
      DECL_IGNORED_P (var) = 0;
      TREE_NO_WARNING (var) = nowarn;
    }
  else
    {
      DECL_IGNORED_P (var) = 1;
      /* ??? We can't generate any warning that would be meaningful.  */
      TREE_NO_WARNING (var) = 1;
    }

  /* Zero-initialize bit-field scalarization variables, to avoid
     triggering undefined behavior.  */
  if (TREE_CODE (elt->element) == BIT_FIELD_REF
      || (var != elt->replacement
	  && TREE_CODE (elt->replacement) == BIT_FIELD_REF))
    {
      gimple_seq init = sra_build_assignment (var,
                                              fold_convert (TREE_TYPE (var),
                                                            integer_zero_node)
                                             );
      insert_edge_copies_seq (init, ENTRY_BLOCK_PTR);
      mark_all_v_defs_seq (init);
    }

  if (dump_file)
    {
      fputs ("  ", dump_file);
      dump_sra_elt_name (dump_file, elt);
      fputs (" -> ", dump_file);
      print_generic_expr (dump_file, var, dump_flags);
      fputc ('\n', dump_file);
    }
}

/* Make one pass across an element tree deciding whether or not it's
   profitable to instantiate individual leaf scalars.

   PARENT_USES and PARENT_COPIES are the sum of the N_USES and N_COPIES
   fields all the way up the tree.  */

static void
decide_instantiation_1 (struct sra_elt *elt, unsigned int parent_uses,
			unsigned int parent_copies)
{
  if (dump_file && !elt->parent)
    {
      fputs ("Initial instantiation for ", dump_file);
      dump_sra_elt_name (dump_file, elt);
      fputc ('\n', dump_file);
    }

  if (elt->cannot_scalarize)
    return;

  if (elt->is_scalar)
    {
      /* The decision is simple: instantiate if we're used more frequently
	 than the parent needs to be seen as a complete unit.  */
      if (elt->n_uses + elt->n_copies + parent_copies > parent_uses)
	instantiate_element (elt);
    }
  else
    {
      struct sra_elt *c, *group;
      unsigned int this_uses = elt->n_uses + parent_uses;
      unsigned int this_copies = elt->n_copies + parent_copies;

      /* Consider groups of sub-elements as weighing in favour of
	 instantiation whatever their size.  */
      for (group = elt->groups; group ; group = group->sibling)
	FOR_EACH_ACTUAL_CHILD (c, group)
	  {
	    c->n_uses += group->n_uses;
	    c->n_copies += group->n_copies;
	  }

      for (c = elt->children; c ; c = c->sibling)
	decide_instantiation_1 (c, this_uses, this_copies);
    }
}

/* Compute the size and number of all instantiated elements below ELT.
   We will only care about this if the size of the complete structure
   fits in a HOST_WIDE_INT, so we don't have to worry about overflow.  */

static unsigned int
sum_instantiated_sizes (struct sra_elt *elt, unsigned HOST_WIDE_INT *sizep)
{
  if (elt->replacement)
    {
      *sizep += TREE_INT_CST_LOW (TYPE_SIZE_UNIT (elt->type));
      return 1;
    }
  else
    {
      struct sra_elt *c;
      unsigned int count = 0;

      for (c = elt->children; c ; c = c->sibling)
	count += sum_instantiated_sizes (c, sizep);

      return count;
    }
}

/* Instantiate fields in ELT->TYPE that are not currently present as
   children of ELT.  */

static void instantiate_missing_elements (struct sra_elt *elt);

static struct sra_elt *
instantiate_missing_elements_1 (struct sra_elt *elt, tree child, tree type)
{
  struct sra_elt *sub = lookup_element (elt, child, type, INSERT);
  if (sub->is_scalar)
    {
      if (sub->replacement == NULL)
	instantiate_element (sub);
    }
  else
    instantiate_missing_elements (sub);
  return sub;
}

/* Obtain the canonical type for field F of ELEMENT.  */

static tree
canon_type_for_field (tree f, tree element)
{
  tree field_type = TREE_TYPE (f);

  /* canonicalize_component_ref() unwidens some bit-field types (not
     marked as DECL_BIT_FIELD in C++), so we must do the same, lest we
     may introduce type mismatches.  */
  if (INTEGRAL_TYPE_P (field_type)
      && DECL_MODE (f) != TYPE_MODE (field_type))
    field_type = TREE_TYPE (get_unwidened (build3 (COMPONENT_REF,
						   field_type,
						   element,
						   f, NULL_TREE),
					   NULL_TREE));

  return field_type;
}

/* Look for adjacent fields of ELT starting at F that we'd like to
   scalarize as a single variable.  Return the last field of the
   group.  */

static tree
try_instantiate_multiple_fields (struct sra_elt *elt, tree f)
{
  int count;
  unsigned HOST_WIDE_INT align, bit, size, alchk;
  enum machine_mode mode;
  tree first = f, prev;
  tree type, var;
  struct sra_elt *block;

  /* Point fields are typically best handled as standalone entities.  */
  if (POINTER_TYPE_P (TREE_TYPE (f)))
    return f;
    
  if (!is_sra_scalar_type (TREE_TYPE (f))
      || !host_integerp (DECL_FIELD_OFFSET (f), 1)
      || !host_integerp (DECL_FIELD_BIT_OFFSET (f), 1)
      || !host_integerp (DECL_SIZE (f), 1)
      || lookup_element (elt, f, NULL, NO_INSERT))
    return f;

  block = elt;

  /* For complex and array objects, there are going to be integer
     literals as child elements.  In this case, we can't just take the
     alignment and mode of the decl, so we instead rely on the element
     type.

     ??? We could try to infer additional alignment from the full
     object declaration and the location of the sub-elements we're
     accessing.  */
  for (count = 0; !DECL_P (block->element); count++)
    block = block->parent;

  align = DECL_ALIGN (block->element);
  alchk = GET_MODE_BITSIZE (DECL_MODE (block->element));

  if (count)
    {
      type = TREE_TYPE (block->element);
      while (count--)
	type = TREE_TYPE (type);

      align = TYPE_ALIGN (type);
      alchk = GET_MODE_BITSIZE (TYPE_MODE (type));
    }

  if (align < alchk)
    align = alchk;

  /* Coalescing wider fields is probably pointless and
     inefficient.  */
  if (align > BITS_PER_WORD)
    align = BITS_PER_WORD;

  bit = tree_low_cst (DECL_FIELD_OFFSET (f), 1) * BITS_PER_UNIT
    + tree_low_cst (DECL_FIELD_BIT_OFFSET (f), 1);
  size = tree_low_cst (DECL_SIZE (f), 1);

  alchk = align - 1;
  alchk = ~alchk;

  if ((bit & alchk) != ((bit + size - 1) & alchk))
    return f;

  /* Find adjacent fields in the same alignment word.  */

  for (prev = f, f = TREE_CHAIN (f);
       f && TREE_CODE (f) == FIELD_DECL
	 && is_sra_scalar_type (TREE_TYPE (f))
	 && host_integerp (DECL_FIELD_OFFSET (f), 1)
	 && host_integerp (DECL_FIELD_BIT_OFFSET (f), 1)
	 && host_integerp (DECL_SIZE (f), 1)
	 && !lookup_element (elt, f, NULL, NO_INSERT);
       prev = f, f = TREE_CHAIN (f))
    {
      unsigned HOST_WIDE_INT nbit, nsize;

      nbit = tree_low_cst (DECL_FIELD_OFFSET (f), 1) * BITS_PER_UNIT
	+ tree_low_cst (DECL_FIELD_BIT_OFFSET (f), 1);
      nsize = tree_low_cst (DECL_SIZE (f), 1);

      if (bit + size == nbit)
	{
	  if ((bit & alchk) != ((nbit + nsize - 1) & alchk))
	    {
	      /* If we're at an alignment boundary, don't bother
		 growing alignment such that we can include this next
		 field.  */
	      if ((nbit & alchk)
		  || GET_MODE_BITSIZE (DECL_MODE (f)) <= align)
		break;

	      align = GET_MODE_BITSIZE (DECL_MODE (f));
	      alchk = align - 1;
	      alchk = ~alchk;

	      if ((bit & alchk) != ((nbit + nsize - 1) & alchk))
		break;
	    }
	  size += nsize;
	}
      else if (nbit + nsize == bit)
	{
	  if ((nbit & alchk) != ((bit + size - 1) & alchk))
	    {
	      if ((bit & alchk)
		  || GET_MODE_BITSIZE (DECL_MODE (f)) <= align)
		break;

	      align = GET_MODE_BITSIZE (DECL_MODE (f));
	      alchk = align - 1;
	      alchk = ~alchk;

	      if ((nbit & alchk) != ((bit + size - 1) & alchk))
		break;
	    }
	  bit = nbit;
	  size += nsize;
	}
      else
	break;
    }

  f = prev;

  if (f == first)
    return f;

  gcc_assert ((bit & alchk) == ((bit + size - 1) & alchk));

  /* Try to widen the bit range so as to cover padding bits as well.  */

  if ((bit & ~alchk) || size != align)
    {
      unsigned HOST_WIDE_INT mbit = bit & alchk;
      unsigned HOST_WIDE_INT msize = align;

      for (f = TYPE_FIELDS (elt->type);
	   f; f = TREE_CHAIN (f))
	{
	  unsigned HOST_WIDE_INT fbit, fsize;

	  /* Skip the fields from first to prev.  */
	  if (f == first)
	    {
	      f = prev;
	      continue;
	    }

	  if (!(TREE_CODE (f) == FIELD_DECL
		&& host_integerp (DECL_FIELD_OFFSET (f), 1)
		&& host_integerp (DECL_FIELD_BIT_OFFSET (f), 1)))
	    continue;

	  fbit = tree_low_cst (DECL_FIELD_OFFSET (f), 1) * BITS_PER_UNIT
	    + tree_low_cst (DECL_FIELD_BIT_OFFSET (f), 1);

	  /* If we're past the selected word, we're fine.  */
	  if ((bit & alchk) < (fbit & alchk))
	    continue;

	  if (host_integerp (DECL_SIZE (f), 1))
	    fsize = tree_low_cst (DECL_SIZE (f), 1);
	  else
	    /* Assume a variable-sized field takes up all space till
	       the end of the word.  ??? Endianness issues?  */
	    fsize = align - (fbit & alchk);

	  if ((fbit & alchk) < (bit & alchk))
	    {
	      /* A large field might start at a previous word and
		 extend into the selected word.  Exclude those
		 bits.  ??? Endianness issues? */
	      HOST_WIDE_INT diff = fbit + fsize - mbit;

	      if (diff <= 0)
		continue;

	      mbit += diff;
	      msize -= diff;
	    }
	  else
	    {
	      /* Non-overlapping, great.  */
	      if (fbit + fsize <= mbit
		  || mbit + msize <= fbit)
		continue;

	      if (fbit <= mbit)
		{
		  unsigned HOST_WIDE_INT diff = fbit + fsize - mbit;
		  mbit += diff;
		  msize -= diff;
		}
	      else if (fbit > mbit)
		msize -= (mbit + msize - fbit);
	      else
		gcc_unreachable ();
	    }
	}

      bit = mbit;
      size = msize;
    }

  /* Now we know the bit range we're interested in.  Find the smallest
     machine mode we can use to access it.  */

  for (mode = smallest_mode_for_size (size, MODE_INT);
       ;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      gcc_assert (mode != VOIDmode);

      alchk = GET_MODE_PRECISION (mode) - 1;
      alchk = ~alchk;

      if ((bit & alchk) == ((bit + size - 1) & alchk))
	break;
    }

  gcc_assert (~alchk < align);

  /* Create the field group as a single variable.  */

  /* We used to create a type for the mode above, but size turns
     to be out not of mode-size.  As we need a matching type
     to build a BIT_FIELD_REF, use a nonstandard integer type as
     fallback.  */
  type = lang_hooks.types.type_for_size (size, 1);
  if (!type || TYPE_PRECISION (type) != size)
    type = build_nonstandard_integer_type (size, 1);
  gcc_assert (type);
  var = build3 (BIT_FIELD_REF, type, NULL_TREE,
		bitsize_int (size), bitsize_int (bit));

  block = instantiate_missing_elements_1 (elt, var, type);
  gcc_assert (block && block->is_scalar);

  var = block->replacement;
  block->in_bitfld_block = 2;

  /* Add the member fields to the group, such that they access
     portions of the group variable.  */

  for (f = first; f != TREE_CHAIN (prev); f = TREE_CHAIN (f))
    {
      tree field_type = canon_type_for_field (f, elt->element);
      struct sra_elt *fld = lookup_element (block, f, field_type, INSERT);

      gcc_assert (fld && fld->is_scalar && !fld->replacement);

      fld->replacement = fold_build3 (BIT_FIELD_REF, field_type, var,
				      bitsize_int (TYPE_PRECISION (field_type)),
				      bitsize_int
				      ((TREE_INT_CST_LOW (DECL_FIELD_OFFSET (f))
					* BITS_PER_UNIT
					+ (TREE_INT_CST_LOW
					   (DECL_FIELD_BIT_OFFSET (f)))
					- (TREE_INT_CST_LOW
					   (TREE_OPERAND (block->element, 2))))
				       & ~alchk));
      fld->in_bitfld_block = 1;
    }

  return prev;
}

static void
instantiate_missing_elements (struct sra_elt *elt)
{
  tree type = elt->type;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      {
	tree f;
	for (f = TYPE_FIELDS (type); f ; f = TREE_CHAIN (f))
	  if (TREE_CODE (f) == FIELD_DECL)
	    {
	      tree last = try_instantiate_multiple_fields (elt, f);

	      if (last != f)
		{
		  f = last;
		  continue;
		}

	      instantiate_missing_elements_1 (elt, f,
					      canon_type_for_field
					      (f, elt->element));
	    }
	break;
      }

    case ARRAY_TYPE:
      {
	tree i, max, subtype;

	i = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	max = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	subtype = TREE_TYPE (type);

	while (1)
	  {
	    instantiate_missing_elements_1 (elt, i, subtype);
	    if (tree_int_cst_equal (i, max))
	      break;
	    i = int_const_binop (PLUS_EXPR, i, integer_one_node, true);
	  }

	break;
      }

    case COMPLEX_TYPE:
      type = TREE_TYPE (type);
      instantiate_missing_elements_1 (elt, integer_zero_node, type);
      instantiate_missing_elements_1 (elt, integer_one_node, type);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Return true if there is only one non aggregate field in the record, TYPE.
   Return false otherwise.  */

static bool
single_scalar_field_in_record_p (tree type)
{
   int num_fields = 0;
   tree field;
   if (TREE_CODE (type) != RECORD_TYPE)
     return false;

   for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
     if (TREE_CODE (field) == FIELD_DECL)
       {
         num_fields++;

         if (num_fields == 2)
           return false;
	 
         if (AGGREGATE_TYPE_P (TREE_TYPE (field)))
           return false;
       }

   return true;
}

/* Make one pass across an element tree deciding whether to perform block
   or element copies.  If we decide on element copies, instantiate all
   elements.  Return true if there are any instantiated sub-elements.  */

static bool
decide_block_copy (struct sra_elt *elt)
{
  struct sra_elt *c;
  bool any_inst;

  /* We shouldn't be invoked on groups of sub-elements as they must
     behave like their parent as far as block copy is concerned.  */
  gcc_assert (!elt->is_group);

  /* If scalarization is disabled, respect it.  */
  if (elt->cannot_scalarize)
    {
      elt->use_block_copy = 1;

      if (dump_file)
	{
	  fputs ("Scalarization disabled for ", dump_file);
	  dump_sra_elt_name (dump_file, elt);
	  fputc ('\n', dump_file);
	}

      /* Disable scalarization of sub-elements */
      for (c = elt->children; c; c = c->sibling)
	{
	  c->cannot_scalarize = 1;
	  decide_block_copy (c);
	}

      /* Groups behave like their parent.  */
      for (c = elt->groups; c; c = c->sibling)
	{
	  c->cannot_scalarize = 1;
	  c->use_block_copy = 1;
	}

      return false;
    }

  /* Don't decide if we've no uses and no groups.  */
  if (elt->n_uses == 0 && elt->n_copies == 0 && elt->groups == NULL)
    ;

  else if (!elt->is_scalar)
    {
      tree size_tree = TYPE_SIZE_UNIT (elt->type);
      bool use_block_copy = true;

      /* Tradeoffs for COMPLEX types pretty much always make it better
	 to go ahead and split the components.  */
      if (TREE_CODE (elt->type) == COMPLEX_TYPE)
	use_block_copy = false;

      /* Don't bother trying to figure out the rest if the structure is
	 so large we can't do easy arithmetic.  This also forces block
	 copies for variable sized structures.  */
      else if (host_integerp (size_tree, 1))
	{
	  unsigned HOST_WIDE_INT full_size, inst_size = 0;
	  unsigned int max_size, max_count, inst_count, full_count;

	  /* If the sra-max-structure-size parameter is 0, then the
	     user has not overridden the parameter and we can choose a
	     sensible default.  */
	  max_size = SRA_MAX_STRUCTURE_SIZE
	    ? SRA_MAX_STRUCTURE_SIZE
	    : MOVE_RATIO (optimize_function_for_speed_p (cfun)) * UNITS_PER_WORD;
	  max_count = SRA_MAX_STRUCTURE_COUNT
	    ? SRA_MAX_STRUCTURE_COUNT
	    : MOVE_RATIO (optimize_function_for_speed_p (cfun));

	  full_size = tree_low_cst (size_tree, 1);
	  full_count = count_type_elements (elt->type, false);
	  inst_count = sum_instantiated_sizes (elt, &inst_size);

	  /* If there is only one scalar field in the record, don't block copy.  */
	  if (single_scalar_field_in_record_p (elt->type))
	    use_block_copy = false;

	  /* ??? What to do here.  If there are two fields, and we've only
	     instantiated one, then instantiating the other is clearly a win.
	     If there are a large number of fields then the size of the copy
	     is much more of a factor.  */

	  /* If the structure is small, and we've made copies, go ahead
	     and instantiate, hoping that the copies will go away.  */
	  if (full_size <= max_size
	      && (full_count - inst_count) <= max_count
	      && elt->n_copies > elt->n_uses)
	    use_block_copy = false;
	  else if (inst_count * 100 >= full_count * SRA_FIELD_STRUCTURE_RATIO
		   && inst_size * 100 >= full_size * SRA_FIELD_STRUCTURE_RATIO)
	    use_block_copy = false;

	  /* In order to avoid block copy, we have to be able to instantiate
	     all elements of the type.  See if this is possible.  */
	  if (!use_block_copy
	      && (!can_completely_scalarize_p (elt)
		  || !type_can_instantiate_all_elements (elt->type)))
	    use_block_copy = true;
	}

      elt->use_block_copy = use_block_copy;

      /* Groups behave like their parent.  */
      for (c = elt->groups; c; c = c->sibling)
	c->use_block_copy = use_block_copy;

      if (dump_file)
	{
	  fprintf (dump_file, "Using %s for ",
		   use_block_copy ? "block-copy" : "element-copy");
	  dump_sra_elt_name (dump_file, elt);
	  fputc ('\n', dump_file);
	}

      if (!use_block_copy)
	{
	  instantiate_missing_elements (elt);
	  return true;
	}
    }

  any_inst = elt->replacement != NULL;

  for (c = elt->children; c ; c = c->sibling)
    any_inst |= decide_block_copy (c);

  return any_inst;
}

/* Entry point to phase 3.  Instantiate scalar replacement variables.  */

static void
decide_instantiations (void)
{
  unsigned int i;
  bool cleared_any;
  bitmap_head done_head;
  bitmap_iterator bi;

  /* We cannot clear bits from a bitmap we're iterating over,
     so save up all the bits to clear until the end.  */
  bitmap_initialize (&done_head, &bitmap_default_obstack);
  cleared_any = false;

  EXECUTE_IF_SET_IN_BITMAP (sra_candidates, 0, i, bi)
    {
      tree var = referenced_var (i);
      struct sra_elt *elt = lookup_element (NULL, var, NULL, NO_INSERT);
      if (elt)
	{
	  decide_instantiation_1 (elt, 0, 0);
	  if (!decide_block_copy (elt))
	    elt = NULL;
	}
      if (!elt)
	{
	  bitmap_set_bit (&done_head, i);
	  cleared_any = true;
	}
    }

  if (cleared_any)
    {
      bitmap_and_compl_into (sra_candidates, &done_head);
      bitmap_and_compl_into (needs_copy_in, &done_head);
    }
  bitmap_clear (&done_head);
  
  mark_set_for_renaming (sra_candidates);

  if (dump_file)
    fputc ('\n', dump_file);
}


/* Phase Four: Update the function to match the replacements created.  */

/* Mark all the variables in VDEF/VUSE operators for STMT for
   renaming. This becomes necessary when we modify all of a
   non-scalar.  */

static void
mark_all_v_defs_stmt (gimple stmt)
{
  tree sym;
  ssa_op_iter iter;

  update_stmt_if_modified (stmt);

  FOR_EACH_SSA_TREE_OPERAND (sym, stmt, iter, SSA_OP_ALL_VIRTUALS)
    {
      if (TREE_CODE (sym) == SSA_NAME)
	sym = SSA_NAME_VAR (sym);
      mark_sym_for_renaming (sym);
    }
}


/* Mark all the variables in virtual operands in all the statements in
   LIST for renaming.  */

static void
mark_all_v_defs_seq (gimple_seq seq)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    mark_all_v_defs_stmt (gsi_stmt (gsi));
}

/* Mark every replacement under ELT with TREE_NO_WARNING.  */

static void
mark_no_warning (struct sra_elt *elt)
{
  if (!elt->all_no_warning)
    {
      if (elt->replacement)
	TREE_NO_WARNING (elt->replacement) = 1;
      else
	{
	  struct sra_elt *c;
	  FOR_EACH_ACTUAL_CHILD (c, elt)
	    mark_no_warning (c);
	}
      elt->all_no_warning = true;
    }
}

/* Build a single level component reference to ELT rooted at BASE.  */

static tree
generate_one_element_ref (struct sra_elt *elt, tree base)
{
  switch (TREE_CODE (TREE_TYPE (base)))
    {
    case RECORD_TYPE:
      {
	tree field = elt->element;

	/* We can't test elt->in_bitfld_block here because, when this is
	   called from instantiate_element, we haven't set this field
	   yet.  */
	if (TREE_CODE (field) == BIT_FIELD_REF)
	  {
	    tree ret = unshare_expr (field);
	    TREE_OPERAND (ret, 0) = base;
	    return ret;
	  }

	/* Watch out for compatible records with differing field lists.  */
	if (DECL_FIELD_CONTEXT (field) != TYPE_MAIN_VARIANT (TREE_TYPE (base)))
	  field = find_compatible_field (TREE_TYPE (base), field);

        return build3 (COMPONENT_REF, elt->type, base, field, NULL);
      }

    case ARRAY_TYPE:
      if (TREE_CODE (elt->element) == RANGE_EXPR)
	return build4 (ARRAY_RANGE_REF, elt->type, base,
		       TREE_OPERAND (elt->element, 0), NULL, NULL);
      else
	return build4 (ARRAY_REF, elt->type, base, elt->element, NULL, NULL);

    case COMPLEX_TYPE:
      if (elt->element == integer_zero_node)
	return build1 (REALPART_EXPR, elt->type, base);
      else
	return build1 (IMAGPART_EXPR, elt->type, base);

    default:
      gcc_unreachable ();
    }
}

/* Build a full component reference to ELT rooted at its native variable.  */

static tree
generate_element_ref (struct sra_elt *elt)
{
  if (elt->parent)
    return generate_one_element_ref (elt, generate_element_ref (elt->parent));
  else
    return elt->element;
}

/* Return true if BF is a bit-field that we can handle like a scalar.  */

static bool
scalar_bitfield_p (tree bf)
{
  return (TREE_CODE (bf) == BIT_FIELD_REF
	  && (is_gimple_reg (TREE_OPERAND (bf, 0))
	      || (TYPE_MODE (TREE_TYPE (TREE_OPERAND (bf, 0))) != BLKmode
		  && (!TREE_SIDE_EFFECTS (TREE_OPERAND (bf, 0))
		      || (GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE
						       (TREE_OPERAND (bf, 0))))
			  <= BITS_PER_WORD)))));
}

/* Create an assignment statement from SRC to DST.  */

static gimple_seq
sra_build_assignment (tree dst, tree src)
{
  gimple stmt;
  gimple_seq seq = NULL, seq2 = NULL;
  /* Turning BIT_FIELD_REFs into bit operations enables other passes
     to do a much better job at optimizing the code.
     From dst = BIT_FIELD_REF <var, sz, off> we produce

	SR.1 = (scalar type) var;
	SR.2 = SR.1 >> off;
	SR.3 = SR.2 & ((1 << sz) - 1);
	... possible sign extension of SR.3 ...
	dst = (destination type) SR.3;
   */
  if (scalar_bitfield_p (src))
    {
      tree var, shift, width;
      tree utype, stype;
      bool unsignedp = (INTEGRAL_TYPE_P (TREE_TYPE (src))
		        ? TYPE_UNSIGNED (TREE_TYPE (src)) : true);
      struct gimplify_ctx gctx;

      var = TREE_OPERAND (src, 0);
      width = TREE_OPERAND (src, 1);
      /* The offset needs to be adjusted to a right shift quantity
	 depending on the endianness.  */
      if (BYTES_BIG_ENDIAN)
	{
	  tree tmp = size_binop (PLUS_EXPR, width, TREE_OPERAND (src, 2));
	  shift = size_binop (MINUS_EXPR, TYPE_SIZE (TREE_TYPE (var)), tmp);
	}
      else
	shift = TREE_OPERAND (src, 2);

      /* In weird cases we have non-integral types for the source or
	 destination object.
	 ???  For unknown reasons we also want an unsigned scalar type.  */
      stype = TREE_TYPE (var);
      if (!INTEGRAL_TYPE_P (stype))
	stype = lang_hooks.types.type_for_size (TREE_INT_CST_LOW
						(TYPE_SIZE (stype)), 1);
      else if (!TYPE_UNSIGNED (stype))
	stype = unsigned_type_for (stype);

      utype = TREE_TYPE (dst);
      if (!INTEGRAL_TYPE_P (utype))
	utype = lang_hooks.types.type_for_size (TREE_INT_CST_LOW
						(TYPE_SIZE (utype)), 1);
      else if (!TYPE_UNSIGNED (utype))
	utype = unsigned_type_for (utype);

      /* Convert the base var of the BIT_FIELD_REF to the scalar type
	 we use for computation if we cannot use it directly.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (var)))
	var = fold_convert (stype, var);
      else
	var = fold_build1 (VIEW_CONVERT_EXPR, stype, var);

      if (!integer_zerop (shift))
	var = fold_build2 (RSHIFT_EXPR, stype, var, shift);

      /* If we need a masking operation, produce one.  */
      if (TREE_INT_CST_LOW (width) == TYPE_PRECISION (stype))
	unsignedp = true;
      else
	{
	  tree one = build_int_cst_wide (stype, 1, 0);
	  tree mask = int_const_binop (LSHIFT_EXPR, one, width, 0);
	  mask = int_const_binop (MINUS_EXPR, mask, one, 0);
	  var = fold_build2 (BIT_AND_EXPR, stype, var, mask);
	}

      /* After shifting and masking, convert to the target type.  */
      var = fold_convert (utype, var);

      /* Perform sign extension, if required.
	 ???  This should never be necessary.  */
      if (!unsignedp)
	{
	  tree signbit = int_const_binop (LSHIFT_EXPR,
					  build_int_cst_wide (utype, 1, 0),
					  size_binop (MINUS_EXPR, width,
						      bitsize_int (1)), 0);

	  var = fold_build2 (BIT_XOR_EXPR, utype, var, signbit);
	  var = fold_build2 (MINUS_EXPR, utype, var, signbit);
	}

      /* fold_build3 (BIT_FIELD_REF, ...) sometimes returns a cast.  */
      STRIP_NOPS (dst);

      /* Finally, move and convert to the destination.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (dst)))
	var = fold_convert (TREE_TYPE (dst), var);
      else
	var = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (dst), var);

      push_gimplify_context (&gctx);
      gctx.into_ssa = true;
      gctx.allow_rhs_cond_expr = true;

      gimplify_assign (dst, var, &seq);

      if (gimple_referenced_vars (cfun))
	for (var = gctx.temps; var; var = TREE_CHAIN (var))
	  add_referenced_var (var);
      pop_gimplify_context (NULL);

      return seq;
    }

  /* fold_build3 (BIT_FIELD_REF, ...) sometimes returns a cast.  */
  if (CONVERT_EXPR_P (dst))
    {
      STRIP_NOPS (dst);
      src = fold_convert (TREE_TYPE (dst), src);
    }
  /* It was hoped that we could perform some type sanity checking
     here, but since front-ends can emit accesses of fields in types
     different from their nominal types and copy structures containing
     them as a whole, we'd have to handle such differences here.
     Since such accesses under different types require compatibility
     anyway, there's little point in making tests and/or adding
     conversions to ensure the types of src and dst are the same.
     So we just assume type differences at this point are ok.
     The only exception we make here are pointer types, which can be different
     in e.g. structurally equal, but non-identical RECORD_TYPEs.  */
  else if (POINTER_TYPE_P (TREE_TYPE (dst))
	   && !useless_type_conversion_p (TREE_TYPE (dst), TREE_TYPE (src)))
    src = fold_convert (TREE_TYPE (dst), src);

  /* ???  Only call the gimplifier if we need to.  Otherwise we may 
     end up substituting with DECL_VALUE_EXPR - see PR37380.  */
  if (!handled_component_p (src)
      && !SSA_VAR_P (src))
    {
      src = force_gimple_operand (src, &seq2, false, NULL_TREE);
      gimple_seq_add_seq (&seq, seq2);
    }
  stmt = gimple_build_assign (dst, src);
  gimple_seq_add_stmt (&seq, stmt);
  return seq;
}

/* BIT_FIELD_REFs must not be shared.  sra_build_elt_assignment()
   takes care of assignments, but we must create copies for uses.  */
#define REPLDUP(t) (TREE_CODE (t) != BIT_FIELD_REF ? (t) : unshare_expr (t))

/* Emit an assignment from SRC to DST, but if DST is a scalarizable
   BIT_FIELD_REF, turn it into bit operations.  */

static gimple_seq
sra_build_bf_assignment (tree dst, tree src)
{
  tree var, type, utype, tmp, tmp2, tmp3;
  gimple_seq seq;
  gimple stmt;
  tree cst, cst2, mask;
  tree minshift, maxshift;

  if (TREE_CODE (dst) != BIT_FIELD_REF)
    return sra_build_assignment (dst, src);

  var = TREE_OPERAND (dst, 0);

  if (!scalar_bitfield_p (dst))
    return sra_build_assignment (REPLDUP (dst), src);

  seq = NULL;

  cst = fold_convert (bitsizetype, TREE_OPERAND (dst, 2));
  cst2 = size_binop (PLUS_EXPR,
		     fold_convert (bitsizetype, TREE_OPERAND (dst, 1)),
		     cst);

  if (BYTES_BIG_ENDIAN)
    {
      maxshift = size_binop (MINUS_EXPR, TYPE_SIZE (TREE_TYPE (var)), cst);
      minshift = size_binop (MINUS_EXPR, TYPE_SIZE (TREE_TYPE (var)), cst2);
    }
  else
    {
      maxshift = cst2;
      minshift = cst;
    }

  type = TREE_TYPE (var);
  if (!INTEGRAL_TYPE_P (type))
    type = lang_hooks.types.type_for_size
      (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (var))), 1);
  if (TYPE_UNSIGNED (type))
    utype = type;
  else
    utype = unsigned_type_for (type);

  mask = build_int_cst_wide (utype, 1, 0);
  if (TREE_INT_CST_LOW (maxshift) == TYPE_PRECISION (utype))
    cst = build_int_cst_wide (utype, 0, 0);
  else
    cst = int_const_binop (LSHIFT_EXPR, mask, maxshift, true);
  if (integer_zerop (minshift))
    cst2 = mask;
  else
    cst2 = int_const_binop (LSHIFT_EXPR, mask, minshift, true);
  mask = int_const_binop (MINUS_EXPR, cst, cst2, true);
  mask = fold_build1 (BIT_NOT_EXPR, utype, mask);

  if (TYPE_MAIN_VARIANT (utype) != TYPE_MAIN_VARIANT (TREE_TYPE (var))
      && !integer_zerop (mask))
    {
      tmp = var;
      if (!is_gimple_variable (tmp))
	tmp = unshare_expr (var);
      else
	TREE_NO_WARNING (var) = true;

      tmp2 = make_rename_temp (utype, "SR");

      if (INTEGRAL_TYPE_P (TREE_TYPE (var)))
	tmp = fold_convert (utype, tmp);
      else
	tmp = fold_build1 (VIEW_CONVERT_EXPR, utype, tmp);

      stmt = gimple_build_assign (tmp2, tmp);
      gimple_seq_add_stmt (&seq, stmt);
    }
  else
    tmp2 = var;

  if (!integer_zerop (mask))
    {
      tmp = make_rename_temp (utype, "SR");
      stmt = gimple_build_assign (tmp, fold_build2 (BIT_AND_EXPR, utype,
						    tmp2, mask));
      gimple_seq_add_stmt (&seq, stmt);
    }
  else
    tmp = mask;

  if (is_gimple_reg (src) && INTEGRAL_TYPE_P (TREE_TYPE (src)))
    tmp2 = src;
  else if (INTEGRAL_TYPE_P (TREE_TYPE (src)))
    {
      gimple_seq tmp_seq;
      tmp2 = make_rename_temp (TREE_TYPE (src), "SR");
      tmp_seq = sra_build_assignment (tmp2, src);
      gimple_seq_add_seq (&seq, tmp_seq);
    }
  else
    {
      gimple_seq tmp_seq;
      tmp2 = make_rename_temp
	(lang_hooks.types.type_for_size
	 (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (src))),
	  1), "SR");
      tmp_seq = sra_build_assignment (tmp2, fold_build1 (VIEW_CONVERT_EXPR,
						      TREE_TYPE (tmp2), src));
      gimple_seq_add_seq (&seq, tmp_seq);
    }

  if (!TYPE_UNSIGNED (TREE_TYPE (tmp2)))
    {
      gimple_seq tmp_seq;
      tree ut = unsigned_type_for (TREE_TYPE (tmp2));
      tmp3 = make_rename_temp (ut, "SR");
      tmp2 = fold_convert (ut, tmp2);
      tmp_seq = sra_build_assignment (tmp3, tmp2);
      gimple_seq_add_seq (&seq, tmp_seq);

      tmp2 = fold_build1 (BIT_NOT_EXPR, utype, mask);
      tmp2 = int_const_binop (RSHIFT_EXPR, tmp2, minshift, true);
      tmp2 = fold_convert (ut, tmp2);
      tmp2 = fold_build2 (BIT_AND_EXPR, ut, tmp3, tmp2);

      if (tmp3 != tmp2)
	{
	  tmp3 = make_rename_temp (ut, "SR");
	  tmp_seq = sra_build_assignment (tmp3, tmp2);
          gimple_seq_add_seq (&seq, tmp_seq);
	}

      tmp2 = tmp3;
    }

  if (TYPE_MAIN_VARIANT (TREE_TYPE (tmp2)) != TYPE_MAIN_VARIANT (utype))
    {
      gimple_seq tmp_seq;
      tmp3 = make_rename_temp (utype, "SR");
      tmp2 = fold_convert (utype, tmp2);
      tmp_seq = sra_build_assignment (tmp3, tmp2);
      gimple_seq_add_seq (&seq, tmp_seq);
      tmp2 = tmp3;
    }

  if (!integer_zerop (minshift))
    {
      tmp3 = make_rename_temp (utype, "SR");
      stmt = gimple_build_assign (tmp3, fold_build2 (LSHIFT_EXPR, utype,
						     tmp2, minshift));
      gimple_seq_add_stmt (&seq, stmt);
      tmp2 = tmp3;
    }

  if (utype != TREE_TYPE (var))
    tmp3 = make_rename_temp (utype, "SR");
  else
    tmp3 = var;
  stmt = gimple_build_assign (tmp3, fold_build2 (BIT_IOR_EXPR, utype,
						 tmp, tmp2));
      gimple_seq_add_stmt (&seq, stmt);

  if (tmp3 != var)
    {
      if (TREE_TYPE (var) == type)
	stmt = gimple_build_assign (var, fold_convert (type, tmp3));
      else
	stmt = gimple_build_assign (var, fold_build1 (VIEW_CONVERT_EXPR,
						      TREE_TYPE (var), tmp3));
      gimple_seq_add_stmt (&seq, stmt);
    }

  return seq;
}

/* Expand an assignment of SRC to the scalarized representation of
   ELT.  If it is a field group, try to widen the assignment to cover
   the full variable.  */

static gimple_seq
sra_build_elt_assignment (struct sra_elt *elt, tree src)
{
  tree dst = elt->replacement;
  tree var, tmp, cst, cst2;
  gimple stmt;
  gimple_seq seq;

  if (TREE_CODE (dst) != BIT_FIELD_REF
      || !elt->in_bitfld_block)
    return sra_build_assignment (REPLDUP (dst), src);

  var = TREE_OPERAND (dst, 0);

  /* Try to widen the assignment to the entire variable.
     We need the source to be a BIT_FIELD_REF as well, such that, for
     BIT_FIELD_REF<d,sz,dp> = BIT_FIELD_REF<s,sz,sp>,
     by design, conditions are met such that we can turn it into
     d = BIT_FIELD_REF<s,dw,sp-dp>.  */
  if (elt->in_bitfld_block == 2
      && TREE_CODE (src) == BIT_FIELD_REF)
    {
      tmp = src;
      cst = TYPE_SIZE (TREE_TYPE (var));
      cst2 = size_binop (MINUS_EXPR, TREE_OPERAND (src, 2),
			 TREE_OPERAND (dst, 2));

      src = TREE_OPERAND (src, 0);

      /* Avoid full-width bit-fields.  */
      if (integer_zerop (cst2)
	  && tree_int_cst_equal (cst, TYPE_SIZE (TREE_TYPE (src))))
	{
	  if (INTEGRAL_TYPE_P (TREE_TYPE (src))
	      && !TYPE_UNSIGNED (TREE_TYPE (src)))
	    src = fold_convert (unsigned_type_for (TREE_TYPE (src)), src);

	  /* If a single conversion won't do, we'll need a statement
	     list.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (var))
	      != TYPE_MAIN_VARIANT (TREE_TYPE (src)))
	    {
              gimple_seq tmp_seq;
	      seq = NULL;

	      if (!INTEGRAL_TYPE_P (TREE_TYPE (src)))
		src = fold_build1 (VIEW_CONVERT_EXPR,
				   lang_hooks.types.type_for_size
				   (TREE_INT_CST_LOW
				    (TYPE_SIZE (TREE_TYPE (src))),
				    1), src);
	      gcc_assert (TYPE_UNSIGNED (TREE_TYPE (src)));

	      tmp = make_rename_temp (TREE_TYPE (src), "SR");
	      stmt = gimple_build_assign (tmp, src);
	      gimple_seq_add_stmt (&seq, stmt);

	      tmp_seq = sra_build_assignment (var,
					      fold_convert (TREE_TYPE (var),
							    tmp));
	      gimple_seq_add_seq (&seq, tmp_seq);

	      return seq;
	    }

	  src = fold_convert (TREE_TYPE (var), src);
	}
      else
	{
	  src = fold_convert (TREE_TYPE (var), tmp);
	}

      return sra_build_assignment (var, src);
    }

  return sra_build_bf_assignment (dst, src);
}

/* Generate a set of assignment statements in *LIST_P to copy all
   instantiated elements under ELT to or from the equivalent structure
   rooted at EXPR.  COPY_OUT controls the direction of the copy, with
   true meaning to copy out of EXPR into ELT.  */

static void
generate_copy_inout (struct sra_elt *elt, bool copy_out, tree expr,
		     gimple_seq *seq_p)
{
  struct sra_elt *c;
  gimple_seq tmp_seq;
  tree t;

  if (!copy_out && TREE_CODE (expr) == SSA_NAME
      && TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree r, i;

      c = lookup_element (elt, integer_zero_node, NULL, NO_INSERT);
      r = c->replacement;
      c = lookup_element (elt, integer_one_node, NULL, NO_INSERT);
      i = c->replacement;

      t = build2 (COMPLEX_EXPR, elt->type, r, i);
      tmp_seq = sra_build_bf_assignment (expr, t);
      SSA_NAME_DEF_STMT (expr) = gimple_seq_last_stmt (tmp_seq);
      gimple_seq_add_seq (seq_p, tmp_seq);
    }
  else if (elt->replacement)
    {
      if (copy_out)
	tmp_seq = sra_build_elt_assignment (elt, expr);
      else
	tmp_seq = sra_build_bf_assignment (expr, REPLDUP (elt->replacement));
      gimple_seq_add_seq (seq_p, tmp_seq);
    }
  else
    {
      FOR_EACH_ACTUAL_CHILD (c, elt)
	{
	  t = generate_one_element_ref (c, unshare_expr (expr));
	  generate_copy_inout (c, copy_out, t, seq_p);
	}
    }
}

/* Generate a set of assignment statements in *LIST_P to copy all instantiated
   elements under SRC to their counterparts under DST.  There must be a 1-1
   correspondence of instantiated elements.  */

static void
generate_element_copy (struct sra_elt *dst, struct sra_elt *src, gimple_seq *seq_p)
{
  struct sra_elt *dc, *sc;

  FOR_EACH_ACTUAL_CHILD (dc, dst)
    {
      sc = lookup_element (src, dc->element, NULL, NO_INSERT);
      if (!sc && dc->in_bitfld_block == 2)
	{
	  struct sra_elt *dcs;

	  FOR_EACH_ACTUAL_CHILD (dcs, dc)
	    {
	      sc = lookup_element (src, dcs->element, NULL, NO_INSERT);
	      gcc_assert (sc);
	      generate_element_copy (dcs, sc, seq_p);
	    }

	  continue;
	}

      /* If DST and SRC are structs with the same elements, but do not have
	 the same TYPE_MAIN_VARIANT, then lookup of DST FIELD_DECL in SRC
	 will fail.  Try harder by finding the corresponding FIELD_DECL
	 in SRC.  */
      if (!sc)
	{
	  tree f;

	  gcc_assert (useless_type_conversion_p (dst->type, src->type));
	  gcc_assert (TREE_CODE (dc->element) == FIELD_DECL);
	  for (f = TYPE_FIELDS (src->type); f ; f = TREE_CHAIN (f))
	    if (simple_cst_equal (DECL_FIELD_OFFSET (f),
				  DECL_FIELD_OFFSET (dc->element)) > 0
		&& simple_cst_equal (DECL_FIELD_BIT_OFFSET (f),
				     DECL_FIELD_BIT_OFFSET (dc->element)) > 0
		&& simple_cst_equal (DECL_SIZE (f),
				     DECL_SIZE (dc->element)) > 0
		&& (useless_type_conversion_p (TREE_TYPE (dc->element),
					       TREE_TYPE (f))
		    || (POINTER_TYPE_P (TREE_TYPE (dc->element))
			&& POINTER_TYPE_P (TREE_TYPE (f)))))
	      break;
	  gcc_assert (f != NULL_TREE);
	  sc = lookup_element (src, f, NULL, NO_INSERT);
	}

      generate_element_copy (dc, sc, seq_p);
    }

  if (dst->replacement)
    {
      gimple_seq tmp_seq;

      gcc_assert (src->replacement);

      tmp_seq = sra_build_elt_assignment (dst, REPLDUP (src->replacement));
      gimple_seq_add_seq (seq_p, tmp_seq);
    }
}

/* Generate a set of assignment statements in *LIST_P to zero all instantiated
   elements under ELT.  In addition, do not assign to elements that have been
   marked VISITED but do reset the visited flag; this allows easy coordination
   with generate_element_init.  */

static void
generate_element_zero (struct sra_elt *elt, gimple_seq *seq_p)
{
  struct sra_elt *c;

  if (elt->visited)
    {
      elt->visited = false;
      return;
    }

  if (!elt->in_bitfld_block)
    FOR_EACH_ACTUAL_CHILD (c, elt)
      generate_element_zero (c, seq_p);

  if (elt->replacement)
    {
      tree t;
      gimple_seq tmp_seq;

      gcc_assert (elt->is_scalar);
      t = fold_convert (elt->type, integer_zero_node);

      tmp_seq = sra_build_elt_assignment (elt, t);
      gimple_seq_add_seq (seq_p, tmp_seq);
    }
}

/* Generate an assignment VAR = INIT, where INIT may need gimplification.
   Add the result to *LIST_P.  */

static void
generate_one_element_init (struct sra_elt *elt, tree init, gimple_seq *seq_p)
{
  gimple_seq tmp_seq = sra_build_elt_assignment (elt, init);
  gimple_seq_add_seq (seq_p, tmp_seq);
}

/* Generate a set of assignment statements in *LIST_P to set all instantiated
   elements under ELT with the contents of the initializer INIT.  In addition,
   mark all assigned elements VISITED; this allows easy coordination with
   generate_element_zero.  Return false if we found a case we couldn't
   handle.  */

static bool
generate_element_init_1 (struct sra_elt *elt, tree init, gimple_seq *seq_p)
{
  bool result = true;
  enum tree_code init_code;
  struct sra_elt *sub;
  tree t;
  unsigned HOST_WIDE_INT idx;
  tree value, purpose;

  /* We can be passed DECL_INITIAL of a static variable.  It might have a
     conversion, which we strip off here.  */
  STRIP_USELESS_TYPE_CONVERSION (init);
  init_code = TREE_CODE (init);

  if (elt->is_scalar)
    {
      if (elt->replacement)
	{
	  generate_one_element_init (elt, init, seq_p);
	  elt->visited = true;
	}
      return result;
    }

  switch (init_code)
    {
    case COMPLEX_CST:
    case COMPLEX_EXPR:
      FOR_EACH_ACTUAL_CHILD (sub, elt)
	{
	  if (sub->element == integer_zero_node)
	    t = (init_code == COMPLEX_EXPR
		 ? TREE_OPERAND (init, 0) : TREE_REALPART (init));
	  else
	    t = (init_code == COMPLEX_EXPR
		 ? TREE_OPERAND (init, 1) : TREE_IMAGPART (init));
	  result &= generate_element_init_1 (sub, t, seq_p);
	}
      break;

    case CONSTRUCTOR:
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), idx, purpose, value)
	{
	  /* Array constructors are routinely created with NULL indices.  */
	  if (purpose == NULL_TREE)
	    {
	      result = false;
	      break;
	    }
	  if (TREE_CODE (purpose) == RANGE_EXPR)
	    {
	      tree lower = TREE_OPERAND (purpose, 0);
	      tree upper = TREE_OPERAND (purpose, 1);

	      while (1)
		{
	  	  sub = lookup_element (elt, lower, NULL, NO_INSERT);
		  if (sub != NULL)
		    result &= generate_element_init_1 (sub, value, seq_p);
		  if (tree_int_cst_equal (lower, upper))
		    break;
		  lower = int_const_binop (PLUS_EXPR, lower,
					   integer_one_node, true);
		}
	    }
	  else
	    {
	      sub = lookup_element (elt, purpose, NULL, NO_INSERT);
	      if (sub != NULL)
		result &= generate_element_init_1 (sub, value, seq_p);
	    }
	}
      break;

    default:
      elt->visited = true;
      result = false;
    }

  return result;
}

/* A wrapper function for generate_element_init_1 that handles cleanup after
   gimplification.  */

static bool
generate_element_init (struct sra_elt *elt, tree init, gimple_seq *seq_p)
{
  bool ret;
  struct gimplify_ctx gctx;

  push_gimplify_context (&gctx);
  ret = generate_element_init_1 (elt, init, seq_p);
  pop_gimplify_context (NULL);

  /* The replacement can expose previously unreferenced variables.  */
  if (ret && *seq_p)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start (*seq_p); !gsi_end_p (i); gsi_next (&i))
	find_new_referenced_vars (gsi_stmt (i));
    }

  return ret;
}

/* Insert a gimple_seq SEQ on all the outgoing edges out of BB.  Note that
   if BB has more than one edge, STMT will be replicated for each edge.
   Also, abnormal edges will be ignored.  */

void
insert_edge_copies_seq (gimple_seq seq, basic_block bb)
{
  edge e;
  edge_iterator ei;
  unsigned n_copies = -1;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_ABNORMAL)) 
      n_copies++;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_ABNORMAL)) 
      gsi_insert_seq_on_edge (e, n_copies-- > 0 ? gimple_seq_copy (seq) : seq);
}

/* Helper function to insert LIST before GSI, and set up line number info.  */

void
sra_insert_before (gimple_stmt_iterator *gsi, gimple_seq seq)
{
  gimple stmt = gsi_stmt (*gsi);

  if (gimple_has_location (stmt))
    annotate_all_with_location (seq, gimple_location (stmt));
  gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);
}

/* Similarly, but insert after GSI.  Handles insertion onto edges as well.  */

void
sra_insert_after (gimple_stmt_iterator *gsi, gimple_seq seq)
{
  gimple stmt = gsi_stmt (*gsi);

  if (gimple_has_location (stmt))
    annotate_all_with_location (seq, gimple_location (stmt));

  if (stmt_ends_bb_p (stmt))
    insert_edge_copies_seq (seq, gsi_bb (*gsi));
  else
    gsi_insert_seq_after (gsi, seq, GSI_SAME_STMT);
}

/* Similarly, but replace the statement at GSI.  */

static void
sra_replace (gimple_stmt_iterator *gsi, gimple_seq seq)
{
  sra_insert_before (gsi, seq);
  gsi_remove (gsi, false);
  if (gsi_end_p (*gsi))
    *gsi = gsi_last (gsi_seq (*gsi));
  else
    gsi_prev (gsi);
}

/* Data structure that bitfield_overlaps_p fills in with information
   about the element passed in and how much of it overlaps with the
   bit-range passed it to.  */

struct bitfield_overlap_info
{
  /* The bit-length of an element.  */
  tree field_len;

  /* The bit-position of the element in its parent.  */
  tree field_pos;

  /* The number of bits of the element that overlap with the incoming
     bit range.  */
  tree overlap_len;

  /* The first bit of the element that overlaps with the incoming bit
     range.  */
  tree overlap_pos;
};

/* Return true if a BIT_FIELD_REF<(FLD->parent), BLEN, BPOS>
   expression (referenced as BF below) accesses any of the bits in FLD,
   false if it doesn't.  If DATA is non-null, its field_len and
   field_pos are filled in such that BIT_FIELD_REF<(FLD->parent),
   field_len, field_pos> (referenced as BFLD below) represents the
   entire field FLD->element, and BIT_FIELD_REF<BFLD, overlap_len,
   overlap_pos> represents the portion of the entire field that
   overlaps with BF.  */

static bool
bitfield_overlaps_p (tree blen, tree bpos, struct sra_elt *fld,
		     struct bitfield_overlap_info *data)
{
  tree flen, fpos;
  bool ret;

  if (TREE_CODE (fld->element) == FIELD_DECL)
    {
      flen = fold_convert (bitsizetype, DECL_SIZE (fld->element));
      fpos = fold_convert (bitsizetype, DECL_FIELD_OFFSET (fld->element));
      fpos = size_binop (MULT_EXPR, fpos, bitsize_int (BITS_PER_UNIT));
      fpos = size_binop (PLUS_EXPR, fpos, DECL_FIELD_BIT_OFFSET (fld->element));
    }
  else if (TREE_CODE (fld->element) == BIT_FIELD_REF)
    {
      flen = fold_convert (bitsizetype, TREE_OPERAND (fld->element, 1));
      fpos = fold_convert (bitsizetype, TREE_OPERAND (fld->element, 2));
    }
  else if (TREE_CODE (fld->element) == INTEGER_CST)
    {
      tree domain_type = TYPE_DOMAIN (TREE_TYPE (fld->parent->element));
      flen = fold_convert (bitsizetype, TYPE_SIZE (fld->type));
      fpos = fold_convert (bitsizetype, fld->element);
      if (domain_type && TYPE_MIN_VALUE (domain_type))
	fpos = size_binop (MINUS_EXPR, fpos,
			   fold_convert (bitsizetype,
			   		 TYPE_MIN_VALUE (domain_type)));
      fpos = size_binop (MULT_EXPR, flen, fpos);
    }
  else
    gcc_unreachable ();

  gcc_assert (host_integerp (blen, 1)
	      && host_integerp (bpos, 1)
	      && host_integerp (flen, 1)
	      && host_integerp (fpos, 1));

  ret = ((!tree_int_cst_lt (fpos, bpos)
	  && tree_int_cst_lt (size_binop (MINUS_EXPR, fpos, bpos),
			      blen))
	 || (!tree_int_cst_lt (bpos, fpos)
	     && tree_int_cst_lt (size_binop (MINUS_EXPR, bpos, fpos),
				 flen)));

  if (!ret)
    return ret;

  if (data)
    {
      tree bend, fend;

      data->field_len = flen;
      data->field_pos = fpos;

      fend = size_binop (PLUS_EXPR, fpos, flen);
      bend = size_binop (PLUS_EXPR, bpos, blen);

      if (tree_int_cst_lt (bend, fend))
	data->overlap_len = size_binop (MINUS_EXPR, bend, fpos);
      else
	data->overlap_len = NULL;

      if (tree_int_cst_lt (fpos, bpos))
	{
	  data->overlap_pos = size_binop (MINUS_EXPR, bpos, fpos);
	  data->overlap_len = size_binop (MINUS_EXPR,
					  data->overlap_len
					  ? data->overlap_len
					  : data->field_len,
					  data->overlap_pos);
	}
      else
	data->overlap_pos = NULL;
    }

  return ret;
}

/* Add to LISTP a sequence of statements that copies BLEN bits between
   VAR and the scalarized elements of ELT, starting a bit VPOS of VAR
   and at bit BPOS of ELT.  The direction of the copy is given by
   TO_VAR.  */

static void
sra_explode_bitfield_assignment (tree var, tree vpos, bool to_var,
				 gimple_seq *seq_p, tree blen, tree bpos,
				 struct sra_elt *elt)
{
  struct sra_elt *fld;
  struct bitfield_overlap_info flp;

  FOR_EACH_ACTUAL_CHILD (fld, elt)
    {
      tree flen, fpos;

      if (!bitfield_overlaps_p (blen, bpos, fld, &flp))
	continue;

      flen = flp.overlap_len ? flp.overlap_len : flp.field_len;
      fpos = flp.overlap_pos ? flp.overlap_pos : bitsize_int (0);

      if (fld->replacement)
	{
	  tree infld, invar, type;
          gimple_seq st;

	  infld = fld->replacement;

	  type = unsigned_type_for (TREE_TYPE (infld));
	  if (TYPE_PRECISION (type) != TREE_INT_CST_LOW (flen))
	    type = build_nonstandard_integer_type (TREE_INT_CST_LOW (flen), 1);

	  if (TREE_CODE (infld) == BIT_FIELD_REF)
	    {
	      fpos = size_binop (PLUS_EXPR, fpos, TREE_OPERAND (infld, 2));
	      infld = TREE_OPERAND (infld, 0);
	    }
	  else if (BYTES_BIG_ENDIAN && DECL_P (fld->element)
		   && !tree_int_cst_equal (TYPE_SIZE (TREE_TYPE (infld)),
					   DECL_SIZE (fld->element)))
	    {
	      fpos = size_binop (PLUS_EXPR, fpos,
				 TYPE_SIZE (TREE_TYPE (infld)));
	      fpos = size_binop (MINUS_EXPR, fpos,
				 DECL_SIZE (fld->element));
	    }

	  infld = fold_build3 (BIT_FIELD_REF, type, infld, flen, fpos);

	  invar = size_binop (MINUS_EXPR, flp.field_pos, bpos);
	  if (flp.overlap_pos)
	    invar = size_binop (PLUS_EXPR, invar, flp.overlap_pos);
	  invar = size_binop (PLUS_EXPR, invar, vpos);

	  invar = fold_build3 (BIT_FIELD_REF, type, var, flen, invar);

	  if (to_var)
	    st = sra_build_bf_assignment (invar, infld);
	  else
	    st = sra_build_bf_assignment (infld, invar);

	  gimple_seq_add_seq (seq_p, st);
	}
      else
	{
	  tree sub = size_binop (MINUS_EXPR, flp.field_pos, bpos);
	  sub = size_binop (PLUS_EXPR, vpos, sub);
	  if (flp.overlap_pos)
	    sub = size_binop (PLUS_EXPR, sub, flp.overlap_pos);

	  sra_explode_bitfield_assignment (var, sub, to_var, seq_p,
					   flen, fpos, fld);
	}
    }
}

/* Add to LISTBEFOREP statements that copy scalarized members of ELT
   that overlap with BIT_FIELD_REF<(ELT->element), BLEN, BPOS> back
   into the full variable, and to LISTAFTERP, if non-NULL, statements
   that copy the (presumably modified) overlapping portions of the
   full variable back to the scalarized variables.  */

static void
sra_sync_for_bitfield_assignment (gimple_seq *seq_before_p,
                                  gimple_seq *seq_after_p,
				  tree blen, tree bpos,
				  struct sra_elt *elt)
{
  struct sra_elt *fld;
  struct bitfield_overlap_info flp;

  FOR_EACH_ACTUAL_CHILD (fld, elt)
    if (bitfield_overlaps_p (blen, bpos, fld, &flp))
      {
	if (fld->replacement || (!flp.overlap_len && !flp.overlap_pos))
	  {
	    generate_copy_inout (fld, false, generate_element_ref (fld),
				 seq_before_p);
	    mark_no_warning (fld);
	    if (seq_after_p)
	      generate_copy_inout (fld, true, generate_element_ref (fld),
				   seq_after_p);
	  }
	else
	  {
	    tree flen = flp.overlap_len ? flp.overlap_len : flp.field_len;
	    tree fpos = flp.overlap_pos ? flp.overlap_pos : bitsize_int (0);

	    sra_sync_for_bitfield_assignment (seq_before_p, seq_after_p,
					      flen, fpos, fld);
	  }
      }
}

/* Scalarize a USE.  To recap, this is either a simple reference to ELT,
   if elt is scalar, or some occurrence of ELT that requires a complete
   aggregate.  IS_OUTPUT is true if ELT is being modified.  */

static void
scalarize_use (struct sra_elt *elt, tree *expr_p, gimple_stmt_iterator *gsi,
	       bool is_output, bool use_all)
{
  gimple stmt = gsi_stmt (*gsi);
  tree bfexpr;

  if (elt->replacement)
    {
      tree replacement = elt->replacement;

      /* If we have a replacement, then updating the reference is as
	 simple as modifying the existing statement in place.  */
      if (is_output
	  && TREE_CODE (elt->replacement) == BIT_FIELD_REF
	  && is_gimple_reg (TREE_OPERAND (elt->replacement, 0))
	  && is_gimple_assign (stmt)
	  && gimple_assign_lhs_ptr (stmt) == expr_p)
	{
          gimple_seq newseq;
          /* RHS must be a single operand. */
          gcc_assert (gimple_assign_single_p (stmt));
	  newseq = sra_build_elt_assignment (elt, gimple_assign_rhs1 (stmt));
	  sra_replace (gsi, newseq);
	  return;
	}
      else if (!is_output
	       && TREE_CODE (elt->replacement) == BIT_FIELD_REF
	       && is_gimple_assign (stmt)
	       && gimple_assign_rhs1_ptr (stmt) == expr_p)
	{
	  tree tmp = make_rename_temp
	    (TREE_TYPE (gimple_assign_lhs (stmt)), "SR");
	  gimple_seq newseq = sra_build_assignment (tmp, REPLDUP (elt->replacement));

	  sra_insert_before (gsi, newseq);
	  replacement = tmp;
	}
      if (is_output)
	  mark_all_v_defs_stmt (stmt);
      *expr_p = REPLDUP (replacement);
      update_stmt (stmt);
    }
  else if (use_all && is_output
	   && is_gimple_assign (stmt)
	   && TREE_CODE (bfexpr
			 = gimple_assign_lhs (stmt)) == BIT_FIELD_REF
	   && &TREE_OPERAND (bfexpr, 0) == expr_p
	   && INTEGRAL_TYPE_P (TREE_TYPE (bfexpr))
	   && TREE_CODE (TREE_TYPE (*expr_p)) == RECORD_TYPE)
    {
      gimple_seq seq_before = NULL;
      gimple_seq seq_after = NULL;
      tree blen = fold_convert (bitsizetype, TREE_OPERAND (bfexpr, 1));
      tree bpos = fold_convert (bitsizetype, TREE_OPERAND (bfexpr, 2));
      bool update = false;

      if (!elt->use_block_copy)
	{
	  tree type = TREE_TYPE (bfexpr);
	  tree var = make_rename_temp (type, "SR"), tmp, vpos;
          gimple st;

	  gimple_assign_set_lhs (stmt, var);
	  update = true;

	  if (!TYPE_UNSIGNED (type))
	    {
	      type = unsigned_type_for (type);
	      tmp = make_rename_temp (type, "SR");
	      st = gimple_build_assign (tmp, fold_convert (type, var));
	      gimple_seq_add_stmt (&seq_after, st);
	      var = tmp;
	    }

	  /* If VAR is wider than BLEN bits, it is padded at the
	     most-significant end.  We want to set VPOS such that
	     <BIT_FIELD_REF VAR BLEN VPOS> would refer to the
	     least-significant BLEN bits of VAR.  */
	  if (BYTES_BIG_ENDIAN)
	    vpos = size_binop (MINUS_EXPR, TYPE_SIZE (type), blen);
	  else
	    vpos = bitsize_int (0);
	  sra_explode_bitfield_assignment
	    (var, vpos, false, &seq_after, blen, bpos, elt);
	}
      else
	sra_sync_for_bitfield_assignment
	  (&seq_before, &seq_after, blen, bpos, elt);

      if (seq_before)
	{
	  mark_all_v_defs_seq (seq_before);
	  sra_insert_before (gsi, seq_before);
	}
      if (seq_after)
	{
	  mark_all_v_defs_seq (seq_after);
	  sra_insert_after (gsi, seq_after);
	}

      if (update)
	update_stmt (stmt);
    }
  else if (use_all && !is_output
	   && is_gimple_assign (stmt)
	   && TREE_CODE (bfexpr
			 = gimple_assign_rhs1 (stmt)) == BIT_FIELD_REF
	   && &TREE_OPERAND (gimple_assign_rhs1 (stmt), 0) == expr_p
	   && INTEGRAL_TYPE_P (TREE_TYPE (bfexpr))
	   && TREE_CODE (TREE_TYPE (*expr_p)) == RECORD_TYPE)
    {
      gimple_seq seq = NULL;
      tree blen = fold_convert (bitsizetype, TREE_OPERAND (bfexpr, 1));
      tree bpos = fold_convert (bitsizetype, TREE_OPERAND (bfexpr, 2));
      bool update = false;

      if (!elt->use_block_copy)
	{
	  tree type = TREE_TYPE (bfexpr);
	  tree var = make_rename_temp (type, "SR"), tmp, vpos;
	  gimple st = NULL;

	  gimple_assign_set_rhs1 (stmt, var);
	  update = true;

	  if (!TYPE_UNSIGNED (type))
	    {
	      type = unsigned_type_for (type);
	      tmp = make_rename_temp (type, "SR");
	      st = gimple_build_assign (var,
					fold_convert (TREE_TYPE (var), tmp));
	      var = tmp;
	    }

	  gimple_seq_add_stmt (&seq,
                               gimple_build_assign
				 (var, build_int_cst_wide (type, 0, 0)));

	  /* If VAR is wider than BLEN bits, it is padded at the
	     most-significant end.  We want to set VPOS such that
	     <BIT_FIELD_REF VAR BLEN VPOS> would refer to the
	     least-significant BLEN bits of VAR.  */
	  if (BYTES_BIG_ENDIAN)
	    vpos = size_binop (MINUS_EXPR, TYPE_SIZE (type), blen);
	  else
	    vpos = bitsize_int (0);
	  sra_explode_bitfield_assignment
	    (var, vpos, true, &seq, blen, bpos, elt);

	  if (st)
	    gimple_seq_add_stmt (&seq, st);
	}
      else
	sra_sync_for_bitfield_assignment
	  (&seq, NULL, blen, bpos, elt);

      if (seq)
	{
	  mark_all_v_defs_seq (seq);
	  sra_insert_before (gsi, seq);
	}

      if (update)
	update_stmt (stmt);
    }
  else
    {
      gimple_seq seq = NULL;

      /* Otherwise we need some copies.  If ELT is being read, then we
	 want to store all (modified) sub-elements back into the
	 structure before the reference takes place.  If ELT is being
	 written, then we want to load the changed values back into
	 our shadow variables.  */
      /* ??? We don't check modified for reads, we just always write all of
	 the values.  We should be able to record the SSA number of the VOP
	 for which the values were last read.  If that number matches the
	 SSA number of the VOP in the current statement, then we needn't
	 emit an assignment.  This would also eliminate double writes when
	 a structure is passed as more than one argument to a function call.
	 This optimization would be most effective if sra_walk_function
	 processed the blocks in dominator order.  */

      generate_copy_inout (elt, is_output, generate_element_ref (elt), &seq);
      if (seq == NULL)
	return;
      mark_all_v_defs_seq (seq);
      if (is_output)
	sra_insert_after (gsi, seq);
      else
	{
	  sra_insert_before (gsi, seq);
	  if (use_all)
	    mark_no_warning (elt);
	}
    }
}

/* Scalarize a COPY.  To recap, this is an assignment statement between
   two scalarizable references, LHS_ELT and RHS_ELT.  */

static void
scalarize_copy (struct sra_elt *lhs_elt, struct sra_elt *rhs_elt,
		gimple_stmt_iterator *gsi)
{
  gimple_seq seq;
  gimple stmt;

  if (lhs_elt->replacement && rhs_elt->replacement)
    {
      /* If we have two scalar operands, modify the existing statement.  */
      stmt = gsi_stmt (*gsi);

      /* See the commentary in sra_walk_function concerning
	 RETURN_EXPR, and why we should never see one here.  */
      gcc_assert (is_gimple_assign (stmt));
      gcc_assert (gimple_assign_copy_p (stmt));


      gimple_assign_set_lhs (stmt, lhs_elt->replacement);
      gimple_assign_set_rhs1 (stmt, REPLDUP (rhs_elt->replacement));
      update_stmt (stmt);
    }
  else if (lhs_elt->use_block_copy || rhs_elt->use_block_copy)
    {
      /* If either side requires a block copy, then sync the RHS back
	 to the original structure, leave the original assignment
	 statement (which will perform the block copy), then load the
	 LHS values out of its now-updated original structure.  */
      /* ??? Could perform a modified pair-wise element copy.  That
	 would at least allow those elements that are instantiated in
	 both structures to be optimized well.  */

      seq = NULL;
      generate_copy_inout (rhs_elt, false,
			   generate_element_ref (rhs_elt), &seq);
      if (seq)
	{
	  mark_all_v_defs_seq (seq);
	  sra_insert_before (gsi, seq);
	}

      seq = NULL;
      generate_copy_inout (lhs_elt, true,
			   generate_element_ref (lhs_elt), &seq);
      if (seq)
	{
	  mark_all_v_defs_seq (seq);
	  sra_insert_after (gsi, seq);
	}
    }
  else
    {
      /* Otherwise both sides must be fully instantiated.  In which
	 case perform pair-wise element assignments and replace the
	 original block copy statement.  */

      stmt = gsi_stmt (*gsi);
      mark_all_v_defs_stmt (stmt);

      seq = NULL;
      generate_element_copy (lhs_elt, rhs_elt, &seq);
      gcc_assert (seq);
      mark_all_v_defs_seq (seq);
      sra_replace (gsi, seq);
    }
}

/* Scalarize an INIT.  To recap, this is an assignment to a scalarizable
   reference from some form of constructor: CONSTRUCTOR, COMPLEX_CST or
   COMPLEX_EXPR.  If RHS is NULL, it should be treated as an empty
   CONSTRUCTOR.  */

static void
scalarize_init (struct sra_elt *lhs_elt, tree rhs, gimple_stmt_iterator *gsi)
{
  bool result = true;
  gimple_seq seq = NULL, init_seq = NULL;

  /* Generate initialization statements for all members extant in the RHS.  */
  if (rhs)
    {
      /* Unshare the expression just in case this is from a decl's initial.  */
      rhs = unshare_expr (rhs);
      result = generate_element_init (lhs_elt, rhs, &init_seq);
    }

  if (!result)
    {
      /* If we failed to convert the entire initializer, then we must
	 leave the structure assignment in place and must load values
	 from the structure into the slots for which we did not find
	 constants.  The easiest way to do this is to generate a complete
	 copy-out, and then follow that with the constant assignments
	 that we were able to build.  DCE will clean things up.  */
      gimple_seq seq0 = NULL;
      generate_copy_inout (lhs_elt, true, generate_element_ref (lhs_elt),
			   &seq0);
      gimple_seq_add_seq (&seq0, seq);
      seq = seq0;
    }
  else
    {
      /* CONSTRUCTOR is defined such that any member not mentioned is assigned
	 a zero value.  Initialize the rest of the instantiated elements.  */
      generate_element_zero (lhs_elt, &seq);
      gimple_seq_add_seq (&seq, init_seq);
    }

  if (lhs_elt->use_block_copy || !result)
    {
      /* Since LHS is not fully instantiated, we must leave the structure
	 assignment in place.  Treating this case differently from a USE
	 exposes constants to later optimizations.  */
      if (seq)
	{
	  mark_all_v_defs_seq (seq);
	  sra_insert_after (gsi, seq);
	}
    }
  else
    {
      /* The LHS is fully instantiated.  The list of initializations
	 replaces the original structure assignment.  */
      gcc_assert (seq);
      mark_all_v_defs_stmt (gsi_stmt (*gsi));
      mark_all_v_defs_seq (seq);
      sra_replace (gsi, seq);
    }
}

/* A subroutine of scalarize_ldst called via walk_tree.  Set TREE_NO_TRAP
   on all INDIRECT_REFs.  */

static tree
mark_notrap (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (TREE_CODE (t) == INDIRECT_REF)
    {
      TREE_THIS_NOTRAP (t) = 1;
      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

  return NULL;
}

/* Scalarize a LDST.  To recap, this is an assignment between one scalarizable
   reference ELT and one non-scalarizable reference OTHER.  IS_OUTPUT is true
   if ELT is on the left-hand side.  */

static void
scalarize_ldst (struct sra_elt *elt, tree other,
		gimple_stmt_iterator *gsi, bool is_output)
{
  /* Shouldn't have gotten called for a scalar.  */
  gcc_assert (!elt->replacement);

  if (elt->use_block_copy)
    {
      /* Since ELT is not fully instantiated, we have to leave the
	 block copy in place.  Treat this as a USE.  */
      scalarize_use (elt, NULL, gsi, is_output, false);
    }
  else
    {
      /* The interesting case is when ELT is fully instantiated.  In this
	 case we can have each element stored/loaded directly to/from the
	 corresponding slot in OTHER.  This avoids a block copy.  */

      gimple_seq seq = NULL;
      gimple stmt = gsi_stmt (*gsi);

      mark_all_v_defs_stmt (stmt);
      generate_copy_inout (elt, is_output, other, &seq);
      gcc_assert (seq);
      mark_all_v_defs_seq (seq);

      /* Preserve EH semantics.  */
      if (stmt_ends_bb_p (stmt))
	{
	  gimple_stmt_iterator si;
	  gimple first;
          gimple_seq blist = NULL;
	  bool thr = stmt_could_throw_p (stmt);

	  /* If the last statement of this BB created an EH edge
	     before scalarization, we have to locate the first
	     statement that can throw in the new statement list and
	     use that as the last statement of this BB, such that EH
	     semantics is preserved.  All statements up to this one
	     are added to the same BB.  All other statements in the
	     list will be added to normal outgoing edges of the same
	     BB.  If they access any memory, it's the same memory, so
	     we can assume they won't throw.  */
	  si = gsi_start (seq);
	  for (first = gsi_stmt (si);
	       thr && !gsi_end_p (si) && !stmt_could_throw_p (first);
	       first = gsi_stmt (si))
	    {
	      gsi_remove (&si, false);
	      gimple_seq_add_stmt (&blist, first);
	    }

	  /* Extract the first remaining statement from LIST, this is
	     the EH statement if there is one.  */
	  gsi_remove (&si, false);

	  if (blist)
	    sra_insert_before (gsi, blist);

	  /* Replace the old statement with this new representative.  */
	  gsi_replace (gsi, first, true);

	  if (!gsi_end_p (si))
	    {
	      /* If any reference would trap, then they all would.  And more
		 to the point, the first would.  Therefore none of the rest
		 will trap since the first didn't.  Indicate this by
		 iterating over the remaining statements and set
		 TREE_THIS_NOTRAP in all INDIRECT_REFs.  */
	      do
		{
		  walk_gimple_stmt (&si, NULL, mark_notrap, NULL);
		  gsi_next (&si);
		}
	      while (!gsi_end_p (si));

	      insert_edge_copies_seq (seq, gsi_bb (*gsi));
	    }
	}
      else
	sra_replace (gsi, seq);
    }
}

/* Generate initializations for all scalarizable parameters.  */

static void
scalarize_parms (void)
{
  gimple_seq seq = NULL;
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (needs_copy_in, 0, i, bi)
    {
      tree var = referenced_var (i);
      struct sra_elt *elt = lookup_element (NULL, var, NULL, NO_INSERT);
      generate_copy_inout (elt, true, var, &seq);
    }

  if (seq)
    {
      insert_edge_copies_seq (seq, ENTRY_BLOCK_PTR);
      mark_all_v_defs_seq (seq);
    }
}

/* Entry point to phase 4.  Update the function to match replacements.  */

static void
scalarize_function (void)
{
  static const struct sra_walk_fns fns = {
    scalarize_use, scalarize_copy, scalarize_init, scalarize_ldst, false
  };

  sra_walk_function (&fns);
  scalarize_parms ();
  gsi_commit_edge_inserts ();
}


/* Debug helper function.  Print ELT in a nice human-readable format.  */

static void
dump_sra_elt_name (FILE *f, struct sra_elt *elt)
{
  if (elt->parent && TREE_CODE (elt->parent->type) == COMPLEX_TYPE)
    {
      fputs (elt->element == integer_zero_node ? "__real__ " : "__imag__ ", f);
      dump_sra_elt_name (f, elt->parent);
    }
  else
    {
      if (elt->parent)
        dump_sra_elt_name (f, elt->parent);
      if (DECL_P (elt->element))
	{
	  if (TREE_CODE (elt->element) == FIELD_DECL)
	    fputc ('.', f);
	  print_generic_expr (f, elt->element, dump_flags);
	}
      else if (TREE_CODE (elt->element) == BIT_FIELD_REF)
	fprintf (f, "$B" HOST_WIDE_INT_PRINT_DEC "F" HOST_WIDE_INT_PRINT_DEC,
		 tree_low_cst (TREE_OPERAND (elt->element, 2), 1),
		 tree_low_cst (TREE_OPERAND (elt->element, 1), 1));
      else if (TREE_CODE (elt->element) == RANGE_EXPR)
	fprintf (f, "["HOST_WIDE_INT_PRINT_DEC".."HOST_WIDE_INT_PRINT_DEC"]",
		 TREE_INT_CST_LOW (TREE_OPERAND (elt->element, 0)),
		 TREE_INT_CST_LOW (TREE_OPERAND (elt->element, 1)));
      else
	fprintf (f, "[" HOST_WIDE_INT_PRINT_DEC "]",
		 TREE_INT_CST_LOW (elt->element));
    }
}

/* Likewise, but callable from the debugger.  */

void
debug_sra_elt_name (struct sra_elt *elt)
{
  dump_sra_elt_name (stderr, elt);
  fputc ('\n', stderr);
}

void 
sra_init_cache (void)
{
  if (sra_type_decomp_cache)
    return;

  sra_type_decomp_cache = BITMAP_ALLOC (NULL);
  sra_type_inst_cache = BITMAP_ALLOC (NULL);
}


/* Main entry point.  */

static unsigned int
tree_sra (void)
{
  /* Initialize local variables.  */
  todoflags = 0;
  gcc_obstack_init (&sra_obstack);
  sra_candidates = BITMAP_ALLOC (NULL);
  needs_copy_in = BITMAP_ALLOC (NULL);
  sra_init_cache ();
  sra_map = htab_create (101, sra_elt_hash, sra_elt_eq, NULL);

  /* Scan.  If we find anything, instantiate and scalarize.  */
  if (find_candidates_for_sra ())
    {
      scan_function ();
      decide_instantiations ();
      scalarize_function ();
      if (!bitmap_empty_p (sra_candidates))
	todoflags |= TODO_rebuild_alias;
    }

  /* Free allocated memory.  */
  htab_delete (sra_map);
  sra_map = NULL;
  BITMAP_FREE (sra_candidates);
  BITMAP_FREE (needs_copy_in);
  BITMAP_FREE (sra_type_decomp_cache);
  BITMAP_FREE (sra_type_inst_cache);
  obstack_free (&sra_obstack, NULL);
  return todoflags;
}

static unsigned int
tree_sra_early (void)
{
  unsigned int ret;

  early_sra = true;
  ret = tree_sra ();
  early_sra = false;

  return ret & ~TODO_rebuild_alias;
}

static bool
gate_sra (void)
{
  return flag_tree_sra != 0;
}

struct gimple_opt_pass pass_sra_early =
{
 {
  GIMPLE_PASS,
  "esra",				/* name */
  gate_sra,				/* gate */
  tree_sra_early,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,				        /* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
  | TODO_update_ssa
  | TODO_ggc_collect
  | TODO_verify_ssa			/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_sra =
{
 {
  GIMPLE_PASS,
  "sra",				/* name */
  gate_sra,				/* gate */
  tree_sra,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,				        /* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
  | TODO_update_ssa
  | TODO_ggc_collect
  | TODO_verify_ssa			/* todo_flags_finish */
 }
};
