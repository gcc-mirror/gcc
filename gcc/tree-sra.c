/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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


/* Maximum number of fields that a structure should have to be scalarized.
   FIXME  This limit has been arbitrarily set to 5.  Experiment to find a
	  sensible setting.  */
#define MAX_NFIELDS_FOR_SRA	5

/* Codes indicating how to copy one structure into another.  */
enum sra_copy_mode { SCALAR_SCALAR, FIELD_SCALAR, SCALAR_FIELD };

/* Local functions.  */
static inline bool can_be_scalarized_p (tree);
static tree create_scalar_copies (tree lhs, tree rhs, enum sra_copy_mode mode);
static inline void scalarize_component_ref (tree, tree *tp);
static void scalarize_structures (void);
static void scalarize_stmt (block_stmt_iterator *);
static void scalarize_modify_expr (block_stmt_iterator *);
static void scalarize_call_expr (block_stmt_iterator *);
static void scalarize_asm_expr (block_stmt_iterator *);
static void scalarize_return_expr (block_stmt_iterator *);

/* The set of aggregate variables that are candidates for scalarization.  */
static bitmap sra_candidates;

/* Set of scalarizable PARM_DECLs that need copy-in operations at the
   beginning of the function.  */
static bitmap needs_copy_in;

/* This structure holds the mapping between and element of an aggregate
   and the scalar replacement variable.  */
struct sra_elt
{
  enum tree_code kind;
  tree base;
  tree field;
  tree replace;
};
    
static htab_t sra_map;

static hashval_t
sra_elt_hash (const void *x)
{
  const struct sra_elt *e = x;
  hashval_t h = (size_t) e->base * e->kind;
  if (e->kind == COMPONENT_REF)
    h ^= (size_t) e->field;
  return h;
}

static int
sra_elt_eq (const void *x, const void *y)
{
  const struct sra_elt *a = x;
  const struct sra_elt *b = y;

  if (a->kind != b->kind)
    return false;
  if (a->base != b->base)
    return false;
  if (a->kind == COMPONENT_REF)
    if (a->field != b->field)
      return false;

  return true;
}

/* Mark all the variables in V_MAY_DEF operands for STMT for renaming.
   This becomes necessary when we modify all of a non-scalar.  */

static void
mark_all_v_may_defs (tree stmt)
{
  v_may_def_optype v_may_defs;
  size_t i, n;

  get_stmt_operands (stmt);
  v_may_defs = V_MAY_DEF_OPS (stmt_ann (stmt));
  n = NUM_V_MAY_DEFS (v_may_defs);

  for (i = 0; i < n; i++)
    {
      tree sym = V_MAY_DEF_RESULT (v_may_defs, i);
      bitmap_set_bit (vars_to_rename, var_ann (sym)->uid);
    }
}

/* Mark all the variables in V_MUST_DEF operands for STMT for renaming.
   This becomes necessary when we modify all of a non-scalar.  */

static void
mark_all_v_must_defs (tree stmt)
{
  v_must_def_optype v_must_defs;
  size_t i, n;

  get_stmt_operands (stmt);
  v_must_defs = V_MUST_DEF_OPS (stmt_ann (stmt));
  n = NUM_V_MUST_DEFS (v_must_defs);

  for (i = 0; i < n; i++)
    {
      tree sym = V_MUST_DEF_OP (v_must_defs, i);
      bitmap_set_bit (vars_to_rename, var_ann (sym)->uid);
    }
}

/* Return true if DECL is an SRA candidate.  */

static bool
is_sra_candidate_decl (tree decl)
{
  return DECL_P (decl) && bitmap_bit_p (sra_candidates, var_ann (decl)->uid);
}

/* Return true if EXP is of the form <ref decl>, where REF is one of the
   field access references we handle and DECL is an SRA candidate. 

   Set ALLOW_BIT_FIELD_REF to accept BIT_FIELD_REF as well.  This is
   normally false, except when we're trying to work around it.  */

static bool
is_sra_candidate_ref (tree exp, bool allow_bit_field_ref)
{
  switch (TREE_CODE (exp))
    {
    case BIT_FIELD_REF:
      if (!allow_bit_field_ref)
	break;
      /* FALLTHRU */

    case COMPONENT_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return is_sra_candidate_decl (TREE_OPERAND (exp, 0));

    default:
      break;
    }

  return false;
}

/* Return the scalar in SRA_MAP[VAR_IX][FIELD_IX].  If none exists, create
   a new scalar with type TYPE.  */

static tree
lookup_scalar (struct sra_elt *key, tree type)
{
  struct sra_elt **slot, *res;

  slot = (struct sra_elt **) htab_find_slot (sra_map, key, INSERT);
  res = *slot;
  if (!res)
    {
      res = xmalloc (sizeof (*res));
      *slot = res;
      *res = *key;
      res->replace = make_rename_temp (type, "SR");

      if (DECL_NAME (key->base) && !DECL_IGNORED_P (key->base))
	{
	  char *name = NULL;
	  switch (key->kind)
	    {
	    case COMPONENT_REF:
	      if (!DECL_NAME (key->field))
		break;
	      name = concat (IDENTIFIER_POINTER (DECL_NAME (key->base)),
			     "$",
			     IDENTIFIER_POINTER (DECL_NAME (key->field)),
			     NULL);
	      break;
	    case REALPART_EXPR:
	      name = concat (IDENTIFIER_POINTER (DECL_NAME (key->base)),
			     "$real", NULL);
	      break;
	    case IMAGPART_EXPR:
	      name = concat (IDENTIFIER_POINTER (DECL_NAME (key->base)),
			     "$imag", NULL);
	      break;
	    default:
	      abort ();
	    }
	  if (name)
	    {
	      DECL_NAME (res->replace) = get_identifier (name);
	      free (name);
	    }
	}

      DECL_SOURCE_LOCATION (res->replace) = DECL_SOURCE_LOCATION (key->base);
      TREE_NO_WARNING (res->replace) = TREE_NO_WARNING (key->base);
      DECL_ARTIFICIAL (res->replace) = DECL_ARTIFICIAL (key->base);
    }

  return res->replace;
}


/* Given a structure reference VAR.FIELD, return a scalar representing it.
   If no scalar is found, a new one is created and added to the SRA_MAP
   matrix.  */

static tree
get_scalar_for_field (tree var, tree field)
{
  struct sra_elt key;

#ifdef ENABLE_CHECKING
  /* Validate that FIELD actually exists in VAR's type.  */
  {
    tree f;
    for (f = TYPE_FIELDS (TREE_TYPE (var)); f ; f = TREE_CHAIN (f))
      if (f == field)
	goto found;
    abort ();
   found:;
  }
#endif

  key.kind = COMPONENT_REF;
  key.base = var;
  key.field = field;

  return lookup_scalar (&key, TREE_TYPE (field));
}


/* Similarly for the parts of a complex type.  */

static tree
get_scalar_for_complex_part (tree var, enum tree_code part)
{
  struct sra_elt key;

  key.kind = part;
  key.base = var;

  return lookup_scalar (&key, TREE_TYPE (TREE_TYPE (var)));
}

/* Return true if the fields of VAR can be replaced by scalar temporaries.
   This only happens if VAR is not call-clobbered and it contains less
   than MAX_NFIELDS_FOR_SRA scalar fields.  */

static inline bool
can_be_scalarized_p (tree var)
{
  tree field, type;
  int nfields;

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

  /* Any COMPLEX_TYPE that has reached this point can be scalarized.  */
  if (TREE_CODE (TREE_TYPE (var)) == COMPLEX_TYPE)
    return true;

  type = TREE_TYPE (var);
  nfields = 0;
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      /* FIXME: We really should recurse down the type hierarchy and
	 scalarize the fields at the leaves.  */
      if (AGGREGATE_TYPE_P (TREE_TYPE (field)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Cannot scalarize variable ");
	      print_generic_expr (dump_file, var, dump_flags);
	      fprintf (dump_file,
		       " because it contains an aggregate type field, ");
	      print_generic_expr (dump_file, field, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	  return false;
	}

      /* FIXME: Similarly.  Indeed, considering that we treat complex
	 as an aggregate, this is exactly the same problem.
	 Structures with __complex__ fields are tested in the libstdc++
	 testsuite: 26_numerics/complex_inserters_extractors.cc.  */
      if (TREE_CODE (TREE_TYPE (field)) == COMPLEX_TYPE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Cannot scalarize variable ");
	      print_generic_expr (dump_file, var, dump_flags);
	      fprintf (dump_file,
		       " because it contains a __complex__ field, ");
	      print_generic_expr (dump_file, field, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	  return false;
	}

      /* FIXME.  We don't scalarize structures with bit fields yet.  To
	 support this, we should make sure that all the fields fit in one
	 word and modify every operation done on the scalarized bit fields
	 to mask them properly.  */
      if (DECL_BIT_FIELD (field))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Cannot scalarize variable ");
	      print_generic_expr (dump_file, var, dump_flags);
	      fprintf (dump_file,
		       " because it contains a bit-field, ");
	      print_generic_expr (dump_file, field, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	  return false;
	}

      nfields++;
      if (nfields > MAX_NFIELDS_FOR_SRA)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Cannot scalarize variable ");
	      print_generic_expr (dump_file, var, dump_flags);
	      fprintf (dump_file,
		       " because it contains more than %d fields\n", 
		       MAX_NFIELDS_FOR_SRA);
	    }
	  return false;
	}
    }

  /* If the structure had no FIELD_DECLs, then don't bother
     scalarizing it.  */
  return nfields > 0;
}


/* Replace the COMPONENT_REF, REALPART_EXPR or IMAGPART_EXPR pointed-to by
   TP inside STMT with the corresponding scalar replacement from SRA_MAP.  */

static inline void
scalarize_component_ref (tree stmt, tree *tp)
{
  tree t = *tp, obj = TREE_OPERAND (t, 0);

  /* When scalarizing a function argument, we will need to insert copy-in
     operations from the original PARM_DECLs. Note that these copy-in
     operations may end up being dead, but we won't know until we rename
     the new variables into SSA.  */
  if (TREE_CODE (obj) == PARM_DECL)
    bitmap_set_bit (needs_copy_in, var_ann (obj)->uid);

  switch (TREE_CODE (t))
    {
    case COMPONENT_REF:
      t = get_scalar_for_field (obj, TREE_OPERAND (t, 1));
      break;
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      t = get_scalar_for_complex_part (obj, TREE_CODE (t));
      break;
    default:
      abort ();
    }

  *tp = t;
  modify_stmt (stmt);
}


/* Scalarize the structure assignment for the statement pointed by SI_P.  */

static void
scalarize_structure_assignment (block_stmt_iterator *si_p)
{
  var_ann_t lhs_ann, rhs_ann;
  tree lhs, rhs, list, orig_stmt;
  bool lhs_can, rhs_can;

  orig_stmt = bsi_stmt (*si_p);
  lhs = TREE_OPERAND (orig_stmt, 0);
  rhs = TREE_OPERAND (orig_stmt, 1);
  list = NULL_TREE;

#if defined ENABLE_CHECKING
  if (TREE_CODE (orig_stmt) != MODIFY_EXPR)
    abort ();
#endif

  /* Remove all type casts from RHS.  This may seem heavy handed but
     it's actually safe and it is necessary in the presence of C++
     reinterpret_cast<> where structure assignments of different
     structures will be present in the IL.  This was the case of PR
     13347 (http://gcc.gnu.org/bugzilla/show_bug.cgi?id=13347) which
     had something like this:

	struct A f;
     	struct B g;
	f = (struct A)g;

     Both 'f' and 'g' were scalarizable, but the presence of the type
     cast was causing SRA to not replace the RHS of the assignment
     with g's scalar replacements.  Furthermore, the fact that this
     assignment reached this point without causing syntax errors means
     that the type cast is safe and that a field-by-field assignment
     from 'g' into 'f' is the right thing to do.  */
  STRIP_NOPS (rhs);

  lhs_ann = DECL_P (lhs) ? var_ann (lhs) : NULL;
  rhs_ann = DECL_P (rhs) ? var_ann (rhs) : NULL;

#if defined ENABLE_CHECKING
  /* Two different variables should not have the same UID.  */
  if (lhs_ann
      && rhs_ann
      && lhs != rhs
      && lhs_ann->uid == rhs_ann->uid)
    abort ();
#endif

  lhs_can = lhs_ann && bitmap_bit_p (sra_candidates, lhs_ann->uid);
  rhs_can = rhs_ann && bitmap_bit_p (sra_candidates, rhs_ann->uid);

  /* Both LHS and RHS are scalarizable.  */
  if (lhs_can && rhs_can)
    list = create_scalar_copies (lhs, rhs, SCALAR_SCALAR);

  /* Only RHS is scalarizable.  */
  else if (rhs_can)
    list = create_scalar_copies (lhs, rhs, FIELD_SCALAR);

  /* Only LHS is scalarizable.  */
  else if (lhs_can)
    list = create_scalar_copies (lhs, rhs, SCALAR_FIELD);

  /* If neither side is scalarizable, do nothing else.  */
  else
    return;

  /* Set line number information for our replacements.  */
  if (EXPR_HAS_LOCATION (orig_stmt))
    annotate_all_with_locus (&list, EXPR_LOCATION (orig_stmt));

  /* Replace the existing statement with the newly created list of
     scalarized copies.  When replacing the original statement, the first
     copy replaces it and the remaining copies are inserted either after
     the first copy or on the outgoing edges of the original statement's
     block.  */
  {
    tree_stmt_iterator tsi = tsi_start (list);
    bsi_replace (si_p, tsi_stmt (tsi), true);
    tsi_delink (&tsi);
    if (stmt_ends_bb_p (orig_stmt))
      insert_edge_copies (list, bb_for_stmt (orig_stmt));
    else
      bsi_insert_after (si_p, list, BSI_CONTINUE_LINKING);
  }
}


/* Traverse all the referenced variables in the program looking for
   structures that could be replaced with scalars.  */

static bool
find_candidates_for_sra (void)
{
  size_t i;
  bool any_set = false;

  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);

      if ((TREE_CODE (TREE_TYPE (var)) == RECORD_TYPE
	   || TREE_CODE (TREE_TYPE (var)) == COMPLEX_TYPE)
	  && can_be_scalarized_p (var))
	{
	  bitmap_set_bit (sra_candidates, var_ann (var)->uid);
	  any_set = true;
	}
    }

  return any_set;
}


/* Insert STMT on all the outgoing edges out of BB.  Note that if BB
   has more than one edge, STMT will be replicated for each edge.  Also,
   abnormal edges will be ignored.  */

void
insert_edge_copies (tree stmt, basic_block bb)
{
  edge e;
  bool first_copy;

  first_copy = true;
  for (e = bb->succ; e; e = e->succ_next)
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
	    bsi_insert_on_edge (e, lhd_unsave_expr_now (stmt));
	}
    }
}


/* Append a new assignment statement to TSI.  */

static tree
csc_assign (tree_stmt_iterator *tsi, tree lhs, tree rhs)
{
  tree stmt = build (MODIFY_EXPR, TREE_TYPE (lhs), lhs, rhs);
  modify_stmt (stmt);
  tsi_link_after (tsi, stmt, TSI_NEW_STMT);
  return stmt;
}

/* A subroutine of create_scalar_copies.  Construct a COMPONENT_REF
   expression for BASE referencing FIELD.  INDEX is the field index.  */

static tree
csc_build_component_ref (tree base, tree field)
{
  switch (TREE_CODE (base))
    {
    case CONSTRUCTOR:
      /* Only appears on RHS.  The only remaining CONSTRUCTORS for
	 record types that should remain are empty, and imply that
	 the entire structure should be zeroed.  */
      if (CONSTRUCTOR_ELTS (base))
	abort ();
      return fold_convert (TREE_TYPE (field), integer_zero_node);

    default:
      /* Avoid sharing BASE when building the different COMPONENT_REFs.
	 We let the first field have the original version.  */
      if (field != TYPE_FIELDS (TREE_TYPE (base)))
	base = unshare_expr (base);
      break;

    case VAR_DECL:
    case PARM_DECL:
      /* Special case for the above -- decls are always shared.  */
      break;
    }

  return build (COMPONENT_REF, TREE_TYPE (field), base, field);
}

/* Similarly for REALPART_EXPR and IMAGPART_EXPR for complex types.  */

static tree
csc_build_complex_part (tree base, enum tree_code part)
{
  switch (TREE_CODE (base))
    {
    case COMPLEX_CST:
      if (part == REALPART_EXPR)
	return TREE_REALPART (base);
      else
	return TREE_IMAGPART (base);

    case COMPLEX_EXPR:
      if (part == REALPART_EXPR)
        return TREE_OPERAND (base, 0);
      else
        return TREE_OPERAND (base, 1);

    default:
      /* Avoid sharing BASE when building the different references.
	 We let the real part have the original version.  */
      if (part != REALPART_EXPR)
	base = unshare_expr (base);
      break;

    case VAR_DECL:
    case PARM_DECL:
      /* Special case for the above -- decls are always shared.  */
      break;
    }

  return build1 (part, TREE_TYPE (TREE_TYPE (base)), base);
}

/* Create and return a list of assignments to perform a scalarized
   structure assignment 'LHS = RHS'.  Both LHS and RHS are assumed to be
   of an aggregate or complex type.  Three types of copies may be specified:

   SCALAR_SCALAR will emit assignments for all the scalar temporaries
      corresponding to the fields of LHS and RHS.

   FIELD_SCALAR will emit assignments from the scalar replacements of
      RHS into each of the fields of the LHS.

   SCALAR_FIELD will emit assignments from each field of the RHS into
      the scalar replacements of the LHS.  */

static tree
create_scalar_copies (tree lhs, tree rhs, enum sra_copy_mode mode)
{
  tree type, list;
  tree_stmt_iterator tsi;

#if defined ENABLE_CHECKING
  /* Sanity checking.  Check that we are not trying to scalarize a
     non-decl.  */
  if (!DECL_P (lhs) && (mode == SCALAR_FIELD || mode == SCALAR_SCALAR))
    abort ();
  if (!DECL_P (rhs) && (mode == FIELD_SCALAR || mode == SCALAR_SCALAR))
    abort ();
#endif

  type = TREE_TYPE (lhs);
  list = alloc_stmt_list ();
  tsi = tsi_start (list);

  /* VA_ARG_EXPRs have side effects, so we need to copy it first to a
     temporary before scalarizing.  FIXME: This should disappear once
     VA_ARG_EXPRs are properly lowered.  */
  if (TREE_CODE (rhs) == VA_ARG_EXPR)
    {
      tree stmt, tmp;

      /* Add TMP = VA_ARG_EXPR <>  */
      tmp = make_rename_temp (TREE_TYPE (rhs), NULL);
      stmt = csc_assign (&tsi, tmp, rhs);

      /* Mark all the variables in VDEF operands for renaming, because
	 the VA_ARG_EXPR will now be in a different statement.  */
      mark_all_v_may_defs (stmt);
      mark_all_v_must_defs (stmt);

      /* Set RHS to be the new temporary TMP.  */
      rhs = tmp;
    }

  /* When making *_SCALAR copies from PARM_DECLs, we will need to insert
     copy-in operations from the original PARM_DECLs.  Note that these
     copy-in operations may end up being dead, but we won't know until
     we rename the new variables into SSA.  */
  if ((mode == SCALAR_SCALAR || mode == FIELD_SCALAR)
      && TREE_CODE (rhs) == PARM_DECL)
    bitmap_set_bit (needs_copy_in, var_ann (rhs)->uid);

  /* Now create scalar copies for each individual field according to MODE.  */
  if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      /* Create scalar copies of both the real and imaginary parts.  */
      tree real_lhs, real_rhs, imag_lhs, imag_rhs;

      if (mode == SCALAR_FIELD)
	{
	  real_rhs = csc_build_complex_part (rhs, REALPART_EXPR);
	  imag_rhs = csc_build_complex_part (rhs, IMAGPART_EXPR);
	}
      else
	{
	  real_rhs = get_scalar_for_complex_part (rhs, REALPART_EXPR);
	  imag_rhs = get_scalar_for_complex_part (rhs, IMAGPART_EXPR);
	}

      if (mode == FIELD_SCALAR)
	{
	  /* In this case we do not need to create but one statement,
	     since we can create a new complex value whole.  */

	  if (TREE_CONSTANT (real_rhs) && TREE_CONSTANT (imag_rhs))
	    rhs = build_complex (type, real_rhs, imag_rhs);
	  else
	    rhs = build (COMPLEX_EXPR, type, real_rhs, imag_rhs);
	  csc_assign (&tsi, lhs, rhs);
	}
      else
	{
	  real_lhs = get_scalar_for_complex_part (lhs, REALPART_EXPR);
	  imag_lhs = get_scalar_for_complex_part (lhs, IMAGPART_EXPR);

	  csc_assign (&tsi, real_lhs, real_rhs);
	  csc_assign (&tsi, imag_lhs, imag_rhs);
	}
    }
  else
    {
      tree lf, rf;

      /* ??? C++ generates copies between different pointer-to-member
	 structures of different types.  To combat this, we must track
	 the field of both the left and right structures, so that we
	 index the variables with fields of their own type.  */

      for (lf = TYPE_FIELDS (type), rf = TYPE_FIELDS (TREE_TYPE (rhs));
	   lf;
	   lf = TREE_CHAIN (lf), rf = TREE_CHAIN (rf))
	{
	  tree lhs_var, rhs_var;

	  /* Only copy FIELD_DECLs.  */
	  if (TREE_CODE (lf) != FIELD_DECL)
	    continue;

	  if (mode == FIELD_SCALAR)
	    lhs_var = csc_build_component_ref (lhs, lf);
	  else
	    lhs_var = get_scalar_for_field (lhs, lf);

	  if (mode == SCALAR_FIELD)
	    rhs_var = csc_build_component_ref (rhs, rf);
	  else
	    rhs_var = get_scalar_for_field (rhs, rf);

	  csc_assign (&tsi, lhs_var, rhs_var);
	}
    }

  /* All the scalar copies just created will either create new definitions
     or remove existing definitions of LHS, so we need to mark it for
     renaming.  */
  if (TREE_SIDE_EFFECTS (list))
    {
      if (mode == SCALAR_FIELD || mode == SCALAR_SCALAR)
	{
	  /* If the LHS has been scalarized, mark it for renaming.  */
	  bitmap_set_bit (vars_to_rename, var_ann (lhs)->uid);
	}
      else if (mode == FIELD_SCALAR)
	{
	  /* Otherwise, mark all the symbols in the VDEFs for the last
	     scalarized statement just created.  Since all the statements
	     introduce the same VDEFs, we only need to check the last one.  */
	  mark_all_v_may_defs (tsi_stmt (tsi));
	  mark_all_v_must_defs (tsi_stmt (tsi));
	}
      else
	abort ();
    }

  return list;
}

/* A helper function that creates the copies, updates line info,
   and emits the code either before or after BSI.  */

static void
emit_scalar_copies (block_stmt_iterator *bsi, tree lhs, tree rhs,
		    enum sra_copy_mode mode)
{
  tree list = create_scalar_copies (lhs, rhs, mode);
  tree stmt = bsi_stmt (*bsi);

  if (EXPR_HAS_LOCATION (stmt))
    annotate_all_with_locus (&list, EXPR_LOCATION (stmt));

  bsi_insert_before (bsi, list, BSI_SAME_STMT);
}

/* Traverse all the statements in the function replacing references to
   scalarizable structures with their corresponding scalar temporaries.  */

static void
scalarize_structures (void)
{
  basic_block bb;
  block_stmt_iterator si;
  size_t i;

  FOR_EACH_BB (bb)
    for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
      {
	tree stmt;
	stmt_ann_t ann;

	stmt = bsi_stmt (si);
	ann = stmt_ann (stmt);

	/* If the statement has no virtual operands, then it doesn't make
	   structure references that we care about.  */
	if (NUM_V_MAY_DEFS (V_MAY_DEF_OPS (ann)) == 0
	    && NUM_VUSES (VUSE_OPS (ann)) == 0
	    && NUM_V_MUST_DEFS (V_MUST_DEF_OPS (ann)) == 0)
	  continue;

	/* Structure references may only appear in certain statements.  */
	if (TREE_CODE (stmt) != MODIFY_EXPR
	    && TREE_CODE (stmt) != CALL_EXPR
	    && TREE_CODE (stmt) != RETURN_EXPR
	    && TREE_CODE (stmt) != ASM_EXPR)
	  continue;

	scalarize_stmt (&si);
      }

  /* Initialize the scalar replacements for every structure that is a
     function argument.  */
  EXECUTE_IF_SET_IN_BITMAP (needs_copy_in, 0, i,
    {
      tree var = referenced_var (i);
      tree list = create_scalar_copies (var, var, SCALAR_FIELD);
      bsi_insert_on_edge (ENTRY_BLOCK_PTR->succ, list);
    });

  /* Commit edge insertions.  */
  bsi_commit_edge_inserts (NULL);
}


/* Scalarize structure references in the statement pointed by SI_P.  */

static void
scalarize_stmt (block_stmt_iterator *si_p)
{
  tree stmt = bsi_stmt (*si_p);

  /* Handle assignments.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR
      && TREE_CODE (TREE_OPERAND (stmt, 1)) != CALL_EXPR)
    scalarize_modify_expr (si_p);

  /* Handle RETURN_EXPR.  */
  else if (TREE_CODE (stmt) == RETURN_EXPR)
    scalarize_return_expr (si_p);

  /* Handle function calls (note that this must be handled after
     MODIFY_EXPR and RETURN_EXPR because a function call can appear in
     both).  */
  else if (get_call_expr_in (stmt) != NULL_TREE)
    scalarize_call_expr (si_p);

  /* Handle ASM_EXPRs.  */
  else if (TREE_CODE (stmt) == ASM_EXPR)
    scalarize_asm_expr (si_p);
}


/* Helper for scalarize_stmt to handle assignments.  */

static void
scalarize_modify_expr (block_stmt_iterator *si_p)
{
  tree stmt = bsi_stmt (*si_p);
  tree lhs = TREE_OPERAND (stmt, 0);
  tree rhs = TREE_OPERAND (stmt, 1);

  /* Found AGGREGATE.FIELD = ...  */
  if (is_sra_candidate_ref (lhs, false))
    {
      tree sym;
      v_may_def_optype v_may_defs;

      scalarize_component_ref (stmt, &TREE_OPERAND (stmt, 0));

      /* Mark the LHS to be renamed, as we have just removed the previous
	 V_MAY_DEF for AGGREGATE.  The statement should have exactly one 
	 V_MAY_DEF for variable AGGREGATE.  */
      v_may_defs = STMT_V_MAY_DEF_OPS (stmt);
      if (NUM_V_MAY_DEFS (v_may_defs) != 1)
	abort ();
      sym = SSA_NAME_VAR (V_MAY_DEF_RESULT (v_may_defs, 0));
      bitmap_set_bit (vars_to_rename, var_ann (sym)->uid);
    }

  /* Found ... = AGGREGATE.FIELD  */
  else if (is_sra_candidate_ref (rhs, false))
    scalarize_component_ref (stmt, &TREE_OPERAND (stmt, 1));

  /* Found ... = BIT_FIELD_REF <>.  This is similar to a CALL_EXPR, if the
     operand of the BIT_FIELD_REF is a scalarizable structure, we need to
     copy from its scalar replacements before doing the bitfield operation.

     FIXME: BIT_FIELD_REFs are often generated by fold-const.c.  This is
     not always desirable because they obfuscate the original predicates,
     limiting what the tree optimizers may do.  For instance, in
     testsuite/g++.dg/opt/nrv4.C the use of SRA allows the optimizers to
     optimize function main() to 'return 0;'.  However, the folder
     generates a BIT_FIELD_REF operation for one of the comparisons,
     preventing the optimizers from removing all the redundant
     operations.  */
  else if (is_sra_candidate_ref (rhs, true))
    {
      tree var = TREE_OPERAND (rhs, 0);
      emit_scalar_copies (si_p, var, var, FIELD_SCALAR);
    }

  /* Found AGGREGATE = ... or ... = AGGREGATE  */
  else if (DECL_P (lhs) || DECL_P (rhs))
    scalarize_structure_assignment (si_p);
}


/* Scalarize structure references in LIST.  Use DONE to avoid duplicates.  */

static inline void
scalarize_tree_list (tree list, block_stmt_iterator *si_p, bitmap done)
{
  tree op;

  for (op = list; op; op = TREE_CHAIN (op))
    {
      tree arg = TREE_VALUE (op);

      if (is_sra_candidate_decl (arg))
	{
	  int index = var_ann (arg)->uid;
	  if (!bitmap_bit_p (done, index))
	    {
	      emit_scalar_copies (si_p, arg, arg, FIELD_SCALAR);
	      bitmap_set_bit (done, index);
	    }
	}
      else if (is_sra_candidate_ref (arg, false))
	{
	  tree stmt = bsi_stmt (*si_p);
	  scalarize_component_ref (stmt, &TREE_VALUE (op));
	}
    }
}


/* Helper for scalarize_stmt to handle function calls.  */

static void
scalarize_call_expr (block_stmt_iterator *si_p)
{
  tree stmt = bsi_stmt (*si_p);
  tree call = (TREE_CODE (stmt) == MODIFY_EXPR) ? TREE_OPERAND (stmt, 1) : stmt;
  struct bitmap_head_def done_head;

  /* First scalarize the arguments.  Order is important, because the copy
     operations for the arguments need to go before the call.
     Scalarization of the return value needs to go after the call.  */
  bitmap_initialize (&done_head, 1);
  scalarize_tree_list (TREE_OPERAND (call, 1), si_p, &done_head);
  bitmap_clear (&done_head);

  /* Scalarize the return value, if any.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      tree var = TREE_OPERAND (stmt, 0);

      /* If the LHS of the assignment is a scalarizable structure, insert
	 copies into the scalar replacements after the call.  */
      if (is_sra_candidate_decl (var))
	{
	  tree list = create_scalar_copies (var, var, SCALAR_FIELD);
	  if (EXPR_HAS_LOCATION (stmt))
	    annotate_all_with_locus (&list, EXPR_LOCATION (stmt));
	  if (stmt_ends_bb_p (stmt))
	    insert_edge_copies (list, bb_for_stmt (stmt));
	  else
	    bsi_insert_after (si_p, list, BSI_NEW_STMT);
	}
    }
}


/* Helper for scalarize_stmt to handle ASM_EXPRs.  */

static void
scalarize_asm_expr (block_stmt_iterator *si_p)
{
  tree stmt = bsi_stmt (*si_p);
  struct bitmap_head_def done_head;

  bitmap_initialize (&done_head, 1);
  scalarize_tree_list (ASM_INPUTS (stmt), si_p, &done_head);
  scalarize_tree_list (ASM_OUTPUTS (stmt), si_p, &done_head);
  bitmap_clear (&done_head);

  /* ??? Process outputs after the asm.  */
}


/* Helper for scalarize_stmt to handle return expressions.  */

static void
scalarize_return_expr (block_stmt_iterator *si_p)
{
  tree stmt = bsi_stmt (*si_p);
  tree op = TREE_OPERAND (stmt, 0);

  if (op == NULL_TREE)
    return;

  /* Handle a bare RESULT_DECL.  This will handle for types needed
     constructors, or possibly after NRV type optimizations.  */
  if (is_sra_candidate_decl (op))
    emit_scalar_copies (si_p, op, op, FIELD_SCALAR);
  else if (TREE_CODE (op) == MODIFY_EXPR)
    {
      tree *rhs_p = &TREE_OPERAND (op, 1);
      tree rhs = *rhs_p;

      /* Handle 'return STRUCTURE;'  */
      if (is_sra_candidate_decl (rhs))
	emit_scalar_copies (si_p, rhs, rhs, FIELD_SCALAR);

      /* Handle 'return STRUCTURE.FIELD;'  */
      else if (is_sra_candidate_ref (rhs, false))
	scalarize_component_ref (stmt, rhs_p);

      /* Handle 'return CALL_EXPR;'  */
      else if (TREE_CODE (rhs) == CALL_EXPR)
	{
	  struct bitmap_head_def done_head;
	  bitmap_initialize (&done_head, 1);
	  scalarize_tree_list (TREE_OPERAND (rhs, 1), si_p, &done_head);
	  bitmap_clear (&done_head);
	}
    }
}


/* Debugging dump for the scalar replacement map.  */

static int
dump_sra_map_trav (void **slot, void *data)
{
  struct sra_elt *e = *slot;
  FILE *f = data;

  switch (e->kind)
    {
    case REALPART_EXPR:
      fputs ("__real__ ", f);
      print_generic_expr (dump_file, e->base, dump_flags);
      fprintf (f, " -> %s\n", get_name (e->replace));
      break;
    case IMAGPART_EXPR:
      fputs ("__imag__ ", f);
      print_generic_expr (dump_file, e->base, dump_flags);
      fprintf (f, " -> %s\n", get_name (e->replace));
      break;
    case COMPONENT_REF:
      print_generic_expr (dump_file, e->base, dump_flags);
      fprintf (f, ".%s -> %s\n", get_name (e->field), get_name (e->replace));
      break;
    default:
      abort ();
    }

  return 1;
}

static void
dump_sra_map (FILE *f)
{
  fputs ("Scalar replacements:\n", f);
  htab_traverse_noresize (sra_map, dump_sra_map_trav, f);
  fputs ("\n\n", f);
}

/* Main entry point to Scalar Replacement of Aggregates (SRA).  This pass
   re-writes non-aliased structure references into scalar temporaries.  The
   goal is to expose some/all structures to the scalar optimizers.

   Scalarization proceeds in two main phases.  First, every structure
   referenced in the program that complies with can_be_scalarized_p is
   marked for scalarization (find_candidates_for_sra).
   
   Second, a mapping between structure fields and scalar temporaries so
   that every time a particular field of a particular structure is
   referenced in the code, we replace it with its corresponding scalar
   temporary (scalarize_structures).

   TODO

   1- Scalarize COMPLEX_TYPEs
   2- Scalarize ARRAY_REFs that are always referenced with a
      constant index.
   3- Timings to determine when scalarization is not profitable.
   4- Determine what's a good value for MAX_NFIELDS_FOR_SRA.  */

static void
tree_sra (void)
{
  /* Initialize local variables.  */
  sra_candidates = BITMAP_XMALLOC ();
  sra_map = NULL;
  needs_copy_in = NULL;

  /* Find structures to be scalarized.  */
  if (!find_candidates_for_sra ())
    {
      BITMAP_XFREE (sra_candidates);
      return;
    }

  /* If we found any, re-write structure references with their
     corresponding scalar replacement.  */
  sra_map = htab_create (101, sra_elt_hash, sra_elt_eq, free);
  needs_copy_in = BITMAP_XMALLOC ();

  scalarize_structures ();

  if (dump_file)
    dump_sra_map (dump_file);

  /* Free allocated memory.  */
  htab_delete (sra_map);
  sra_map = NULL;
  BITMAP_XFREE (needs_copy_in);
  BITMAP_XFREE (sra_candidates);
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
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa  /* todo_flags_finish */
};
