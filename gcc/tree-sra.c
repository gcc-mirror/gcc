/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
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
#include "tree-gimple.h"
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


/* The set of aggregate variables that are candidates for scalarization.  */
static bitmap sra_candidates;

/* Set of scalarizable PARM_DECLs that need copy-in operations at the
   beginning of the function.  */
static bitmap needs_copy_in;

/* Sets of bit pairs that cache type decomposition and instantiation.  */
static bitmap sra_type_decomp_cache;
static bitmap sra_type_inst_cache;

/* One of these structures is created for each candidate aggregate
   and each (accessed) member of such an aggregate.  */
struct sra_elt
{
  /* A tree of the elements.  Used when we want to traverse everything.  */
  struct sra_elt *parent;
  struct sra_elt *children;
  struct sra_elt *sibling;

  /* If this element is a root, then this is the VAR_DECL.  If this is
     a sub-element, this is some token used to identify the reference.
     In the case of COMPONENT_REF, this is the FIELD_DECL.  In the case
     of an ARRAY_REF, this is the (constant) index.  In the case of a
     complex number, this is a zero or one.  */
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

  /* True if we saw something about this element that prevents scalarization,
     such as non-constant indexing.  */
  bool cannot_scalarize;

  /* True if we've decided that structure-to-structure assignment
     should happen via memcpy and not per-element.  */
  bool use_block_copy;

  /* A flag for use with/after random access traversals.  */
  bool visited;
};

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

/* Return true if DECL is an SRA candidate.  */

static bool
is_sra_candidate_decl (tree decl)
{
  return DECL_P (decl) && bitmap_bit_p (sra_candidates, var_ann (decl)->uid);
}

/* Return true if TYPE is a scalar type.  */

static bool
is_sra_scalar_type (tree type)
{
  enum tree_code code = TREE_CODE (type);
  return (code == INTEGER_TYPE || code == REAL_TYPE || code == VECTOR_TYPE
	  || code == ENUMERAL_TYPE || code == BOOLEAN_TYPE
	  || code == CHAR_TYPE || code == POINTER_TYPE || code == OFFSET_TYPE
	  || code == REFERENCE_TYPE);
}

/* Return true if TYPE can be decomposed into a set of independent variables.

   Note that this doesn't imply that all elements of TYPE can be
   instantiated, just that if we decide to break up the type into
   separate pieces that it can be done.  */

static bool
type_can_be_decomposed_p (tree type)
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
  if (!type_can_be_decomposed_p (TREE_TYPE (var)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Cannot scalarize variable ");
	  print_generic_expr (dump_file, var, dump_flags);
	  fprintf (dump_file, " because its type cannot be decomposed\n");
	}
      return false;
    }

  return true;
}

/* Return true if TYPE can be *completely* decomposed into scalars.  */

static bool
type_can_instantiate_all_elements (tree type)
{
  if (is_sra_scalar_type (type))
    return true;
  if (!type_can_be_decomposed_p (type))
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

  for (c = elt->children; c ; c = c->sibling)
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

    case FIELD_DECL:
      /* We can have types that are compatible, but have different member
	 lists, so we can't hash fields by ID.  Use offsets instead.  */
      h = iterative_hash_expr (DECL_FIELD_OFFSET (t), 0);
      h = iterative_hash_expr (DECL_FIELD_BIT_OFFSET (t), h);
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
  const struct sra_elt *e = x;
  const struct sra_elt *p;
  hashval_t h;

  h = sra_hash_tree (e->element);

  /* Take into account everything back up the chain.  Given that chain
     lengths are rarely very long, this should be acceptable.  If we
     truly identify this as a performance problem, it should work to
     hash the pointer value "e->parent".  */
  for (p = e->parent; p ; p = p->parent)
    h = (h * 65521) ^ sra_hash_tree (p->element);

  return h;
}

/* Equality function for type SRA_PAIR.  */

static int
sra_elt_eq (const void *x, const void *y)
{
  const struct sra_elt *a = x;
  const struct sra_elt *b = y;
  tree ae, be;

  if (a->parent != b->parent)
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

    case FIELD_DECL:
      /* Fields are unique within a record, but not between
	 compatible records.  */
      if (DECL_FIELD_CONTEXT (ae) == DECL_FIELD_CONTEXT (be))
	return false;
      return fields_compatible_p (ae, be);

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

  dummy.parent = parent;
  dummy.element = child;

  slot = (struct sra_elt **) htab_find_slot (sra_map, &dummy, insert);
  if (!slot && insert == NO_INSERT)
    return NULL;

  elt = *slot;
  if (!elt && insert == INSERT)
    {
      *slot = elt = obstack_alloc (&sra_obstack, sizeof (*elt));
      memset (elt, 0, sizeof (*elt));

      elt->parent = parent;
      elt->element = child;
      elt->type = type;
      elt->is_scalar = is_sra_scalar_type (type);

      if (parent)
	{
	  elt->sibling = parent->children;
	  parent->children = elt;
	}

      /* If this is a parameter, then if we want to scalarize, we have
	 one copy from the true function parameter.  Count it now.  */
      if (TREE_CODE (child) == PARM_DECL)
	{
	  elt->n_copies = 1;
	  bitmap_set_bit (needs_copy_in, var_ann (child)->uid);
	}
    }

  return elt;
}

/* Return true if the ARRAY_REF in EXPR is a constant, in bounds access.  */

static bool
is_valid_const_index (tree expr)
{
  tree dom, t, index = TREE_OPERAND (expr, 1);

  if (TREE_CODE (index) != INTEGER_CST)
    return false;

  /* Watch out for stupid user tricks, indexing outside the array.

     Careful, we're not called only on scalarizable types, so do not
     assume constant array bounds.  We needn't do anything with such
     cases, since they'll be referring to objects that we should have
     already rejected for scalarization, so returning false is fine.  */

  dom = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (expr, 0)));
  if (dom == NULL)
    return false;

  t = TYPE_MIN_VALUE (dom);
  if (!t || TREE_CODE (t) != INTEGER_CST)
    return false;
  if (tree_int_cst_lt (index, t))
    return false;

  t = TYPE_MAX_VALUE (dom);
  if (!t || TREE_CODE (t) != INTEGER_CST)
    return false;
  if (tree_int_cst_lt (t, index))
    return false;

  return true;
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
      /* We can't scalarize variable array indicies.  */
      if (is_valid_const_index (expr))
        child = TREE_OPERAND (expr, 1);
      else
	return NULL;
      break;

    case COMPONENT_REF:
      /* Don't look through unions.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) != RECORD_TYPE)
	return NULL;
      child = TREE_OPERAND (expr, 1);
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
   various kinds of references seen.  In all cases, *BSI is an iterator
   pointing to the statement being processed.  */
struct sra_walk_fns
{
  /* Invoked when ELT is required as a unit.  Note that ELT might refer to
     a leaf node, in which case this is a simple scalar reference.  *EXPR_P
     points to the location of the expression.  IS_OUTPUT is true if this
     is a left-hand-side reference.  */
  void (*use) (struct sra_elt *elt, tree *expr_p,
	       block_stmt_iterator *bsi, bool is_output);

  /* Invoked when we have a copy between two scalarizable references.  */
  void (*copy) (struct sra_elt *lhs_elt, struct sra_elt *rhs_elt,
		block_stmt_iterator *bsi);

  /* Invoked when ELT is initialized from a constant.  VALUE may be NULL,
     in which case it should be treated as an empty CONSTRUCTOR.  */
  void (*init) (struct sra_elt *elt, tree value, block_stmt_iterator *bsi);

  /* Invoked when we have a copy between one scalarizable reference ELT
     and one non-scalarizable reference OTHER.  IS_OUTPUT is true if ELT
     is on the left-hand side.  */
  void (*ldst) (struct sra_elt *elt, tree other,
		block_stmt_iterator *bsi, bool is_output);

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
sra_walk_expr (tree *expr_p, block_stmt_iterator *bsi, bool is_output,
	       const struct sra_walk_fns *fns)
{
  tree expr = *expr_p;
  tree inner = expr;
  bool disable_scalarization = false;

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
	      fns->use (elt, expr_p, bsi, is_output);
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
	if (!is_valid_const_index (inner))
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

      case COMPONENT_REF:
	/* A reference to a union member constitutes a reference to the
	   entire union.  */
	if (TREE_CODE (TREE_TYPE (TREE_OPERAND (inner, 0))) != RECORD_TYPE)
	  goto use_all;
	/* ??? See above re non-constant stride.  */
	if (TREE_OPERAND (inner, 2))
	  goto use_all;
	inner = TREE_OPERAND (inner, 0);
	break;

      case REALPART_EXPR:
      case IMAGPART_EXPR:
	inner = TREE_OPERAND (inner, 0);
	break;

      case BIT_FIELD_REF:
	/* A bit field reference (access to *multiple* fields simultaneously)
	   is not currently scalarized.  Consider this an access to the
	   complete outer element, to which walk_tree will bring us next.  */
	goto use_all;

      case ARRAY_RANGE_REF:
	/* Similarly, an subrange reference is used to modify indexing.  Which
	   means that the canonical element names that we have won't work.  */
	goto use_all;

      case VIEW_CONVERT_EXPR:
      case NOP_EXPR:
	/* Similarly, a view/nop explicitly wants to look at an object in a
	   type other than the one we've scalarized.  */
	goto use_all;

      case WITH_SIZE_EXPR:
	/* This is a transparent wrapper.  The entire inner expression really
	   is being used.  */
	goto use_all;

      use_all:
        expr_p = &TREE_OPERAND (inner, 0);
	inner = expr = *expr_p;
	break;

      default:
#ifdef ENABLE_CHECKING
	/* Validate that we're not missing any references.  */
	gcc_assert (!walk_tree (&inner, sra_find_candidate_decl, NULL, NULL));
#endif
	return;
      }
}

/* Walk a TREE_LIST of values looking for scalarizable aggregates.
   If we find one, invoke FNS->USE.  */

static void
sra_walk_tree_list (tree list, block_stmt_iterator *bsi, bool is_output,
		    const struct sra_walk_fns *fns)
{
  tree op;
  for (op = list; op ; op = TREE_CHAIN (op))
    sra_walk_expr (&TREE_VALUE (op), bsi, is_output, fns);
}

/* Walk the arguments of a CALL_EXPR looking for scalarizable aggregates.
   If we find one, invoke FNS->USE.  */

static void
sra_walk_call_expr (tree expr, block_stmt_iterator *bsi,
		    const struct sra_walk_fns *fns)
{
  sra_walk_tree_list (TREE_OPERAND (expr, 1), bsi, false, fns);
}

/* Walk the inputs and outputs of an ASM_EXPR looking for scalarizable
   aggregates.  If we find one, invoke FNS->USE.  */

static void
sra_walk_asm_expr (tree expr, block_stmt_iterator *bsi,
		   const struct sra_walk_fns *fns)
{
  sra_walk_tree_list (ASM_INPUTS (expr), bsi, false, fns);
  sra_walk_tree_list (ASM_OUTPUTS (expr), bsi, true, fns);
}

/* Walk a MODIFY_EXPR and categorize the assignment appropriately.  */

static void
sra_walk_modify_expr (tree expr, block_stmt_iterator *bsi,
		      const struct sra_walk_fns *fns)
{
  struct sra_elt *lhs_elt, *rhs_elt;
  tree lhs, rhs;

  lhs = TREE_OPERAND (expr, 0);
  rhs = TREE_OPERAND (expr, 1);
  lhs_elt = maybe_lookup_element_for_expr (lhs);
  rhs_elt = maybe_lookup_element_for_expr (rhs);

  /* If both sides are scalarizable, this is a COPY operation.  */
  if (lhs_elt && rhs_elt)
    {
      fns->copy (lhs_elt, rhs_elt, bsi);
      return;
    }

  /* If the RHS is scalarizable, handle it.  There are only two cases.  */
  if (rhs_elt)
    {
      if (!rhs_elt->is_scalar)
	fns->ldst (rhs_elt, lhs, bsi, false);
      else
	fns->use (rhs_elt, &TREE_OPERAND (expr, 1), bsi, false);
    }

  /* If it isn't scalarizable, there may be scalarizable variables within, so
     check for a call or else walk the RHS to see if we need to do any
     copy-in operations.  We need to do it before the LHS is scalarized so
     that the statements get inserted in the proper place, before any
     copy-out operations.  */
  else
    {
      tree call = get_call_expr_in (rhs);
      if (call)
	sra_walk_call_expr (call, bsi, fns);
      else
	sra_walk_expr (&TREE_OPERAND (expr, 1), bsi, false, fns);
    }

  /* Likewise, handle the LHS being scalarizable.  We have cases similar
     to those above, but also want to handle RHS being constant.  */
  if (lhs_elt)
    {
      /* If this is an assignment from a constant, or constructor, then
	 we have access to all of the elements individually.  Invoke INIT.  */
      if (TREE_CODE (rhs) == COMPLEX_EXPR
	  || TREE_CODE (rhs) == COMPLEX_CST
	  || TREE_CODE (rhs) == CONSTRUCTOR)
	fns->init (lhs_elt, rhs, bsi);

      /* If this is an assignment from read-only memory, treat this as if
	 we'd been passed the constructor directly.  Invoke INIT.  */
      else if (TREE_CODE (rhs) == VAR_DECL
	       && TREE_STATIC (rhs)
	       && TREE_READONLY (rhs)
	       && targetm.binds_local_p (rhs))
	fns->init (lhs_elt, DECL_INITIAL (rhs), bsi);

      /* If this is a copy from a non-scalarizable lvalue, invoke LDST.
	 The lvalue requirement prevents us from trying to directly scalarize
	 the result of a function call.  Which would result in trying to call
	 the function multiple times, and other evil things.  */
      else if (!lhs_elt->is_scalar && is_gimple_addressable (rhs))
	fns->ldst (lhs_elt, rhs, bsi, true);

      /* Otherwise we're being used in some context that requires the
	 aggregate to be seen as a whole.  Invoke USE.  */
      else
	fns->use (lhs_elt, &TREE_OPERAND (expr, 0), bsi, true);
    }

  /* Similarly to above, LHS_ELT being null only means that the LHS as a
     whole is not a scalarizable reference.  There may be occurrences of
     scalarizable variables within, which implies a USE.  */
  else
    sra_walk_expr (&TREE_OPERAND (expr, 0), bsi, true, fns);
}

/* Entry point to the walk functions.  Search the entire function,
   invoking the callbacks in FNS on each of the references to
   scalarizable variables.  */

static void
sra_walk_function (const struct sra_walk_fns *fns)
{
  basic_block bb;
  block_stmt_iterator si, ni;

  /* ??? Phase 4 could derive some benefit to walking the function in
     dominator tree order.  */

  FOR_EACH_BB (bb)
    for (si = bsi_start (bb); !bsi_end_p (si); si = ni)
      {
	tree stmt, t;
	stmt_ann_t ann;

	stmt = bsi_stmt (si);
	ann = stmt_ann (stmt);

	ni = si;
	bsi_next (&ni);

	/* If the statement has no virtual operands, then it doesn't
	   make any structure references that we care about.  */
	if (NUM_V_MAY_DEFS (V_MAY_DEF_OPS (ann)) == 0
	    && NUM_VUSES (VUSE_OPS (ann)) == 0
	    && NUM_V_MUST_DEFS (V_MUST_DEF_OPS (ann)) == 0)
	  continue;

	switch (TREE_CODE (stmt))
	  {
	  case RETURN_EXPR:
	    /* If we have "return <retval>" then the return value is
	       already exposed for our pleasure.  Walk it as a USE to
	       force all the components back in place for the return.

	       If we have an embedded assignment, then <retval> is of
	       a type that gets returned in registers in this ABI, and
	       we do not wish to extend their lifetimes.  Treat this
	       as a USE of the variable on the RHS of this assignment.  */

	    t = TREE_OPERAND (stmt, 0);
	    if (TREE_CODE (t) == MODIFY_EXPR)
	      sra_walk_expr (&TREE_OPERAND (t, 1), &si, false, fns);
	    else
	      sra_walk_expr (&TREE_OPERAND (stmt, 0), &si, false, fns);
	    break;

	  case MODIFY_EXPR:
	    sra_walk_modify_expr (stmt, &si, fns);
	    break;
	  case CALL_EXPR:
	    sra_walk_call_expr (stmt, &si, fns);
	    break;
	  case ASM_EXPR:
	    sra_walk_asm_expr (stmt, &si, fns);
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
  size_t i;
  bool any_set = false;

  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);
      if (decl_can_be_decomposed_p (var))
        {
          bitmap_set_bit (sra_candidates, var_ann (var)->uid);
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
	  block_stmt_iterator *bsi ATTRIBUTE_UNUSED,
	  bool is_output ATTRIBUTE_UNUSED)
{
  elt->n_uses += 1;
}

static void
scan_copy (struct sra_elt *lhs_elt, struct sra_elt *rhs_elt,
	   block_stmt_iterator *bsi ATTRIBUTE_UNUSED)
{
  lhs_elt->n_copies += 1;
  rhs_elt->n_copies += 1;
}

static void
scan_init (struct sra_elt *lhs_elt, tree rhs ATTRIBUTE_UNUSED,
	   block_stmt_iterator *bsi ATTRIBUTE_UNUSED)
{
  lhs_elt->n_copies += 1;
}

static void
scan_ldst (struct sra_elt *elt, tree other ATTRIBUTE_UNUSED,
	   block_stmt_iterator *bsi ATTRIBUTE_UNUSED,
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
  return obstack_finish (&sra_obstack);
}

/* Instantiate an element as an independent variable.  */

static void
instantiate_element (struct sra_elt *elt)
{
  struct sra_elt *base_elt;
  tree var, base;

  for (base_elt = elt; base_elt->parent; base_elt = base_elt->parent)
    continue;
  base = base_elt->element;

  elt->replacement = var = make_rename_temp (elt->type, "SR");
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

      DECL_DEBUG_EXPR (var) = generate_element_ref (elt);
      DECL_DEBUG_EXPR_IS_FROM (var) = 1;

      DECL_IGNORED_P (var) = 0;
      TREE_NO_WARNING (var) = TREE_NO_WARNING (base);
    }
  else
    {
      DECL_IGNORED_P (var) = 1;
      /* ??? We can't generate any warning that would be meaningful.  */
      TREE_NO_WARNING (var) = 1;
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
      struct sra_elt *c;
      unsigned int this_uses = elt->n_uses + parent_uses;
      unsigned int this_copies = elt->n_copies + parent_copies;

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

static void
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
	    instantiate_missing_elements_1 (elt, f, TREE_TYPE (f));
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

/* Make one pass across an element tree deciding whether to perform block
   or element copies.  If we decide on element copies, instantiate all
   elements.  Return true if there are any instantiated sub-elements.  */

static bool
decide_block_copy (struct sra_elt *elt)
{
  struct sra_elt *c;
  bool any_inst;

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
      return false;
    }

  /* Don't decide if we've no uses.  */
  if (elt->n_uses == 0 && elt->n_copies == 0)
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
	    : MOVE_RATIO * UNITS_PER_WORD;
	  max_count = SRA_MAX_STRUCTURE_COUNT
	    ? SRA_MAX_STRUCTURE_COUNT
	    : MOVE_RATIO;

	  full_size = tree_low_cst (size_tree, 1);
	  full_count = count_type_elements (elt->type);
	  inst_count = sum_instantiated_sizes (elt, &inst_size);

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

  if (dump_file)
    fputc ('\n', dump_file);
}


/* Phase Four: Update the function to match the replacements created.  */

/* Mark all the variables in V_MAY_DEF or V_MUST_DEF operands for STMT for
   renaming. This becomes necessary when we modify all of a non-scalar.  */

static void
mark_all_v_defs (tree stmt)
{
  tree sym;
  ssa_op_iter iter;

  get_stmt_operands (stmt);

  FOR_EACH_SSA_TREE_OPERAND (sym, stmt, iter, SSA_OP_ALL_VIRTUALS)
    {
      if (TREE_CODE (sym) == SSA_NAME)
	sym = SSA_NAME_VAR (sym);
      bitmap_set_bit (vars_to_rename, var_ann (sym)->uid);
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

	/* Watch out for compatible records with differing field lists.  */
	if (DECL_FIELD_CONTEXT (field) != TYPE_MAIN_VARIANT (TREE_TYPE (base)))
	  field = find_compatible_field (TREE_TYPE (base), field);

        return build (COMPONENT_REF, elt->type, base, field, NULL);
      }

    case ARRAY_TYPE:
      return build (ARRAY_REF, elt->type, base, elt->element, NULL, NULL);

    case COMPLEX_TYPE:
      if (elt->element == integer_zero_node)
	return build (REALPART_EXPR, elt->type, base);
      else
	return build (IMAGPART_EXPR, elt->type, base);

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

/* Generate a set of assignment statements in *LIST_P to copy all
   instantiated elements under ELT to or from the equivalent structure
   rooted at EXPR.  COPY_OUT controls the direction of the copy, with
   true meaning to copy out of EXPR into ELT.  */

static void
generate_copy_inout (struct sra_elt *elt, bool copy_out, tree expr,
		     tree *list_p)
{
  struct sra_elt *c;
  tree t;

  if (elt->replacement)
    {
      if (copy_out)
	t = build (MODIFY_EXPR, void_type_node, elt->replacement, expr);
      else
	t = build (MODIFY_EXPR, void_type_node, expr, elt->replacement);
      append_to_statement_list (t, list_p);
    }
  else
    {
      for (c = elt->children; c ; c = c->sibling)
	{
	  t = generate_one_element_ref (c, unshare_expr (expr));
	  generate_copy_inout (c, copy_out, t, list_p);
	}
    }
}

/* Generate a set of assignment statements in *LIST_P to copy all instantiated
   elements under SRC to their counterparts under DST.  There must be a 1-1
   correspondence of instantiated elements.  */

static void
generate_element_copy (struct sra_elt *dst, struct sra_elt *src, tree *list_p)
{
  struct sra_elt *dc, *sc;

  for (dc = dst->children; dc ; dc = dc->sibling)
    {
      sc = lookup_element (src, dc->element, NULL, NO_INSERT);
      gcc_assert (sc);
      generate_element_copy (dc, sc, list_p);
    }

  if (dst->replacement)
    {
      tree t;

      gcc_assert (src->replacement);

      t = build (MODIFY_EXPR, void_type_node, dst->replacement,
		 src->replacement);
      append_to_statement_list (t, list_p);
    }
}

/* Generate a set of assignment statements in *LIST_P to zero all instantiated
   elements under ELT.  In addition, do not assign to elements that have been
   marked VISITED but do reset the visited flag; this allows easy coordination
   with generate_element_init.  */

static void
generate_element_zero (struct sra_elt *elt, tree *list_p)
{
  struct sra_elt *c;

  if (elt->visited)
    {
      elt->visited = false;
      return;
    }

  for (c = elt->children; c ; c = c->sibling)
    generate_element_zero (c, list_p);

  if (elt->replacement)
    {
      tree t;

      gcc_assert (elt->is_scalar);
      t = fold_convert (elt->type, integer_zero_node);

      t = build (MODIFY_EXPR, void_type_node, elt->replacement, t);
      append_to_statement_list (t, list_p);
    }
}

/* Generate an assignment VAR = INIT, where INIT may need gimplification.
   Add the result to *LIST_P.  */

static void
generate_one_element_init (tree var, tree init, tree *list_p)
{
  /* The replacement can be almost arbitrarily complex.  Gimplify.  */
  tree stmt = build (MODIFY_EXPR, void_type_node, var, init);
  gimplify_and_add (stmt, list_p);
}

/* Generate a set of assignment statements in *LIST_P to set all instantiated
   elements under ELT with the contents of the initializer INIT.  In addition,
   mark all assigned elements VISITED; this allows easy coordination with
   generate_element_zero.  Return false if we found a case we couldn't
   handle.  */

static bool
generate_element_init_1 (struct sra_elt *elt, tree init, tree *list_p)
{
  bool result = true;
  enum tree_code init_code;
  struct sra_elt *sub;
  tree t;

  /* We can be passed DECL_INITIAL of a static variable.  It might have a
     conversion, which we strip off here.  */
  STRIP_USELESS_TYPE_CONVERSION (init);
  init_code = TREE_CODE (init);

  if (elt->is_scalar)
    {
      if (elt->replacement)
	{
	  generate_one_element_init (elt->replacement, init, list_p);
	  elt->visited = true;
	}
      return result;
    }

  switch (init_code)
    {
    case COMPLEX_CST:
    case COMPLEX_EXPR:
      for (sub = elt->children; sub ; sub = sub->sibling)
	{
	  if (sub->element == integer_zero_node)
	    t = (init_code == COMPLEX_EXPR
		 ? TREE_OPERAND (init, 0) : TREE_REALPART (init));
	  else
	    t = (init_code == COMPLEX_EXPR
		 ? TREE_OPERAND (init, 1) : TREE_IMAGPART (init));
	  result &= generate_element_init_1 (sub, t, list_p);
	}
      break;

    case CONSTRUCTOR:
      for (t = CONSTRUCTOR_ELTS (init); t ; t = TREE_CHAIN (t))
	{
	  tree purpose = TREE_PURPOSE (t);
	  tree value = TREE_VALUE (t);

	  if (TREE_CODE (purpose) == RANGE_EXPR)
	    {
	      tree lower = TREE_OPERAND (purpose, 0);
	      tree upper = TREE_OPERAND (purpose, 1);

	      while (1)
		{
	  	  sub = lookup_element (elt, lower, NULL, NO_INSERT);
		  if (sub != NULL)
		    result &= generate_element_init_1 (sub, value, list_p);
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
		result &= generate_element_init_1 (sub, value, list_p);
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
generate_element_init (struct sra_elt *elt, tree init, tree *list_p)
{
  bool ret;

  push_gimplify_context ();
  ret = generate_element_init_1 (elt, init, list_p);
  pop_gimplify_context (NULL);

  /* The replacement can expose previously unreferenced variables.  */
  if (ret && *list_p)
    {
      tree_stmt_iterator i;
      size_t old, new, j;

      old = num_referenced_vars;

      for (i = tsi_start (*list_p); !tsi_end_p (i); tsi_next (&i))
	find_new_referenced_vars (tsi_stmt_ptr (i));

      new = num_referenced_vars;
      for (j = old; j < new; ++j)
	bitmap_set_bit (vars_to_rename, j);
    }

  return ret;
}

/* Insert STMT on all the outgoing edges out of BB.  Note that if BB
   has more than one edge, STMT will be replicated for each edge.  Also,
   abnormal edges will be ignored.  */

void
insert_edge_copies (tree stmt, basic_block bb)
{
  edge e;
  edge_iterator ei;
  bool first_copy;

  first_copy = true;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      /* We don't need to insert copies on abnormal edges.  The
	 value of the scalar replacement is not guaranteed to
	 be valid through an abnormal edge.  */
      if (!(e->flags & EDGE_ABNORMAL))
	{
	  if (first_copy)
	    {
	      bsi_insert_on_edge (e, stmt);
	      first_copy = false;
	    }
	  else
	    bsi_insert_on_edge (e, unsave_expr_now (stmt));
	}
    }
}

/* Helper function to insert LIST before BSI, and set up line number info.  */

static void
sra_insert_before (block_stmt_iterator *bsi, tree list)
{
  tree stmt = bsi_stmt (*bsi);

  if (EXPR_HAS_LOCATION (stmt))
    annotate_all_with_locus (&list, EXPR_LOCATION (stmt));
  bsi_insert_before (bsi, list, BSI_SAME_STMT);
}

/* Similarly, but insert after BSI.  Handles insertion onto edges as well.  */

static void
sra_insert_after (block_stmt_iterator *bsi, tree list)
{
  tree stmt = bsi_stmt (*bsi);

  if (EXPR_HAS_LOCATION (stmt))
    annotate_all_with_locus (&list, EXPR_LOCATION (stmt));

  if (stmt_ends_bb_p (stmt))
    insert_edge_copies (list, bsi->bb);
  else
    bsi_insert_after (bsi, list, BSI_SAME_STMT);
}

/* Similarly, but replace the statement at BSI.  */

static void
sra_replace (block_stmt_iterator *bsi, tree list)
{
  sra_insert_before (bsi, list);
  bsi_remove (bsi);
  if (bsi_end_p (*bsi))
    *bsi = bsi_last (bsi->bb);
  else
    bsi_prev (bsi);
}

/* Scalarize a USE.  To recap, this is either a simple reference to ELT,
   if elt is scalar, or some occurrence of ELT that requires a complete
   aggregate.  IS_OUTPUT is true if ELT is being modified.  */

static void
scalarize_use (struct sra_elt *elt, tree *expr_p, block_stmt_iterator *bsi,
	       bool is_output)
{
  tree list = NULL, stmt = bsi_stmt (*bsi);

  if (elt->replacement)
    {
      /* If we have a replacement, then updating the reference is as
	 simple as modifying the existing statement in place.  */
      if (is_output)
	mark_all_v_defs (stmt);
      *expr_p = elt->replacement;
      modify_stmt (stmt);
    }
  else
    {
      /* Otherwise we need some copies.  If ELT is being read, then we want
	 to store all (modified) sub-elements back into the structure before
	 the reference takes place.  If ELT is being written, then we want to
	 load the changed values back into our shadow variables.  */
      /* ??? We don't check modified for reads, we just always write all of
	 the values.  We should be able to record the SSA number of the VOP
	 for which the values were last read.  If that number matches the
	 SSA number of the VOP in the current statement, then we needn't
	 emit an assignment.  This would also eliminate double writes when
	 a structure is passed as more than one argument to a function call.
	 This optimization would be most effective if sra_walk_function
	 processed the blocks in dominator order.  */

      generate_copy_inout (elt, is_output, generate_element_ref (elt), &list);
      if (list == NULL)
	return;
      mark_all_v_defs (expr_first (list));
      if (is_output)
	sra_insert_after (bsi, list);
      else
	sra_insert_before (bsi, list);
    }
}

/* Scalarize a COPY.  To recap, this is an assignment statement between
   two scalarizable references, LHS_ELT and RHS_ELT.  */

static void
scalarize_copy (struct sra_elt *lhs_elt, struct sra_elt *rhs_elt,
		block_stmt_iterator *bsi)
{
  tree list, stmt;

  if (lhs_elt->replacement && rhs_elt->replacement)
    {
      /* If we have two scalar operands, modify the existing statement.  */
      stmt = bsi_stmt (*bsi);

      /* See the commentary in sra_walk_function concerning
	 RETURN_EXPR, and why we should never see one here.  */
      gcc_assert (TREE_CODE (stmt) == MODIFY_EXPR);

      TREE_OPERAND (stmt, 0) = lhs_elt->replacement;
      TREE_OPERAND (stmt, 1) = rhs_elt->replacement;
      modify_stmt (stmt);
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

      list = NULL;
      generate_copy_inout (rhs_elt, false,
			   generate_element_ref (rhs_elt), &list);
      if (list)
	{
	  mark_all_v_defs (expr_first (list));
	  sra_insert_before (bsi, list);
	}

      list = NULL;
      generate_copy_inout (lhs_elt, true,
			   generate_element_ref (lhs_elt), &list);
      if (list)
	sra_insert_after (bsi, list);
    }
  else
    {
      /* Otherwise both sides must be fully instantiated.  In which
	 case perform pair-wise element assignments and replace the
	 original block copy statement.  */

      stmt = bsi_stmt (*bsi);
      mark_all_v_defs (stmt);

      list = NULL;
      generate_element_copy (lhs_elt, rhs_elt, &list);
      gcc_assert (list);
      sra_replace (bsi, list);
    }
}

/* Scalarize an INIT.  To recap, this is an assignment to a scalarizable
   reference from some form of constructor: CONSTRUCTOR, COMPLEX_CST or
   COMPLEX_EXPR.  If RHS is NULL, it should be treated as an empty
   CONSTRUCTOR.  */

static void
scalarize_init (struct sra_elt *lhs_elt, tree rhs, block_stmt_iterator *bsi)
{
  bool result = true;
  tree list = NULL;

  /* Generate initialization statements for all members extant in the RHS.  */
  if (rhs)
    {
      /* Unshare the expression just in case this is from a decl's initial.  */
      rhs = unshare_expr (rhs);
      result = generate_element_init (lhs_elt, rhs, &list);
    }

  /* CONSTRUCTOR is defined such that any member not mentioned is assigned
     a zero value.  Initialize the rest of the instantiated elements.  */
  generate_element_zero (lhs_elt, &list);

  if (!result)
    {
      /* If we failed to convert the entire initializer, then we must
	 leave the structure assignment in place and must load values
	 from the structure into the slots for which we did not find
	 constants.  The easiest way to do this is to generate a complete
	 copy-out, and then follow that with the constant assignments
	 that we were able to build.  DCE will clean things up.  */
      tree list0 = NULL;
      generate_copy_inout (lhs_elt, true, generate_element_ref (lhs_elt),
			   &list0);
      append_to_statement_list (list, &list0);
      list = list0;
    }

  if (lhs_elt->use_block_copy || !result)
    {
      /* Since LHS is not fully instantiated, we must leave the structure
	 assignment in place.  Treating this case differently from a USE
	 exposes constants to later optimizations.  */
      if (list)
	{
	  mark_all_v_defs (expr_first (list));
	  sra_insert_after (bsi, list);
	}
    }
  else
    {
      /* The LHS is fully instantiated.  The list of initializations
	 replaces the original structure assignment.  */
      gcc_assert (list);
      mark_all_v_defs (bsi_stmt (*bsi));
      sra_replace (bsi, list);
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
		block_stmt_iterator *bsi, bool is_output)
{
  /* Shouldn't have gotten called for a scalar.  */
  gcc_assert (!elt->replacement);

  if (elt->use_block_copy)
    {
      /* Since ELT is not fully instantiated, we have to leave the
	 block copy in place.  Treat this as a USE.  */
      scalarize_use (elt, NULL, bsi, is_output);
    }
  else
    {
      /* The interesting case is when ELT is fully instantiated.  In this
	 case we can have each element stored/loaded directly to/from the
	 corresponding slot in OTHER.  This avoids a block copy.  */

      tree list = NULL, stmt = bsi_stmt (*bsi);

      mark_all_v_defs (stmt);
      generate_copy_inout (elt, is_output, other, &list);
      gcc_assert (list);

      /* Preserve EH semantics.  */
      if (stmt_ends_bb_p (stmt))
	{
	  tree_stmt_iterator tsi;
	  tree first;

	  /* Extract the first statement from LIST.  */
	  tsi = tsi_start (list);
	  first = tsi_stmt (tsi);
	  tsi_delink (&tsi);

	  /* Replace the old statement with this new representative.  */
	  bsi_replace (bsi, first, true);

	  if (!tsi_end_p (tsi))
	    {
	      /* If any reference would trap, then they all would.  And more
		 to the point, the first would.  Therefore none of the rest
		 will trap since the first didn't.  Indicate this by
		 iterating over the remaining statements and set
		 TREE_THIS_NOTRAP in all INDIRECT_REFs.  */
	      do
		{
		  walk_tree (tsi_stmt_ptr (tsi), mark_notrap, NULL, NULL);
		  tsi_next (&tsi);
		}
	      while (!tsi_end_p (tsi));

	      insert_edge_copies (list, bsi->bb);
	    }
	}
      else
	sra_replace (bsi, list);
    }
}

/* Generate initializations for all scalarizable parameters.  */

static void
scalarize_parms (void)
{
  tree list = NULL;
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (needs_copy_in, 0, i, bi)
    {
      tree var = referenced_var (i);
      struct sra_elt *elt = lookup_element (NULL, var, NULL, NO_INSERT);
      generate_copy_inout (elt, true, var, &list);
    }

  if (list)
    insert_edge_copies (list, ENTRY_BLOCK_PTR);
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
  bsi_commit_edge_inserts ();
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

/* Main entry point.  */

static void
tree_sra (void)
{
  /* Initialize local variables.  */
  gcc_obstack_init (&sra_obstack);
  sra_candidates = BITMAP_ALLOC (NULL);
  needs_copy_in = BITMAP_ALLOC (NULL);
  sra_type_decomp_cache = BITMAP_ALLOC (NULL);
  sra_type_inst_cache = BITMAP_ALLOC (NULL);
  sra_map = htab_create (101, sra_elt_hash, sra_elt_eq, NULL);

  /* Scan.  If we find anything, instantiate and scalarize.  */
  if (find_candidates_for_sra ())
    {
      scan_function ();
      decide_instantiations ();
      scalarize_function ();
    }

  /* Free allocated memory.  */
  htab_delete (sra_map);
  sra_map = NULL;
  BITMAP_FREE (sra_candidates);
  BITMAP_FREE (needs_copy_in);
  BITMAP_FREE (sra_type_decomp_cache);
  BITMAP_FREE (sra_type_inst_cache);
  obstack_free (&sra_obstack, NULL);
}

static bool
gate_sra (void)
{
  return flag_tree_sra != 0;
}

struct tree_opt_pass pass_sra =
{
  "sra",				/* name */
  gate_sra,				/* gate */
  tree_sra,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa,  /* todo_flags_finish */
  0					/* letter */
};
