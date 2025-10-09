/* Constraint builder for tree based points-to analysis
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "stmt.h"
#include "gimple-iterator.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "gimple-walk.h"
#include "varasm.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "gimple-range.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "attr-fnspec.h"

#include "tree-ssa-structalias.h"
#include "gimple-ssa-pta-constraints.h"

using namespace pointer_analysis;

/* Map from trees to variable infos.  */
static hash_map<tree, varinfo_t> *vi_for_tree;

/* A map mapping call statements to per-stmt variables for uses
   and clobbers specific to the call.  */
static hash_map<gimple *, varinfo_t> *call_stmt_vars;

static unsigned int create_variable_info_for (tree, const char *, bool);
static inline bool type_can_have_subvars (const_tree);
static void make_param_constraints (varinfo_t);

/* Lookup or create the variable for the call statement CALL.  */

static varinfo_t
get_call_vi (gcall *call)
{
  varinfo_t vi, vi2;

  bool existed;
  varinfo_t *slot_p = &call_stmt_vars->get_or_insert (call, &existed);
  if (existed)
    return *slot_p;

  vi = new_var_info (NULL_TREE, "CALLUSED", true);
  vi->offset = 0;
  vi->size = 1;
  vi->fullsize = 2;
  vi->is_full_var = true;
  vi->is_reg_var = true;

  vi2 = new_var_info (NULL_TREE, "CALLCLOBBERED", true);
  vi2->offset = 1;
  vi2->size = 1;
  vi2->fullsize = 2;
  vi2->is_full_var = true;
  vi2->is_reg_var = true;

  vi->next = vi2->id;

  *slot_p = vi;
  return vi;
}

/* Lookup or create the variable for the call statement CALL representing
   the uses.  */

static varinfo_t
get_call_use_vi (gcall *call)
{
  return get_call_vi (call);
}

/* Lookup or create the variable for the call statement CALL representing
   the clobbers.  */

static varinfo_t ATTRIBUTE_UNUSED
get_call_clobber_vi (gcall *call)
{
  return vi_next (get_call_vi (call));
}


static void get_constraint_for_1 (tree, vec<ce_s> *, bool, bool);
static void get_constraint_for (tree, vec<ce_s> *);
static void get_constraint_for_rhs (tree, vec<ce_s> *);
static void do_deref (vec<ce_s> *);

/* Allocator for 'constraints' vector.  */

static object_allocator<constraint> constraint_pool ("Constraint pool");

/* Create a new constraint consisting of LHS and RHS expressions.  */

static constraint_t
new_constraint (const struct constraint_expr lhs,
		const struct constraint_expr rhs)
{
  constraint_t ret = constraint_pool.allocate ();
  ret->lhs = lhs;
  ret->rhs = rhs;
  return ret;
}

/* Insert ID as the variable id for tree T in the vi_for_tree map.  */

static void
insert_vi_for_tree (tree t, varinfo_t vi)
{
  gcc_assert (vi);
  bool existed = vi_for_tree->put (t, vi);
  gcc_assert (!existed);
}

/* Return a printable name for DECL.  */

static const char *
alias_get_name (tree decl)
{
  const char *res = "NULL";
  if (dump_file)
    {
      char *temp = NULL;
      if (TREE_CODE (decl) == SSA_NAME)
	{
	  res = get_name (decl);
	  temp = xasprintf ("%s_%u", res ? res : "", SSA_NAME_VERSION (decl));
	}
      else if (HAS_DECL_ASSEMBLER_NAME_P (decl)
	       && DECL_ASSEMBLER_NAME_SET_P (decl))
	res = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME_RAW (decl));
      else if (DECL_P (decl))
	{
	  res = get_name (decl);
	  if (!res)
	    temp = xasprintf ("D.%u", DECL_UID (decl));
	}

      if (temp)
	{
	  res = ggc_strdup (temp);
	  free (temp);
	}
    }

  return res;
}

/* Find the variable id for tree T in the map.
   If T doesn't exist in the map, create an entry for it and return it.  */

static varinfo_t
get_vi_for_tree (tree t)
{
  varinfo_t *slot = vi_for_tree->get (t);
  if (slot == NULL)
    {
      unsigned int id = create_variable_info_for (t, alias_get_name (t), false);
      return get_varinfo (id);
    }

  return *slot;
}

/* Get a scalar constraint expression for a new temporary variable.  */

static struct constraint_expr
new_scalar_tmp_constraint_exp (const char *name, bool add_id)
{
  struct constraint_expr tmp;
  varinfo_t vi;

  vi = new_var_info (NULL_TREE, name, add_id);
  vi->offset = 0;
  vi->size = -1;
  vi->fullsize = -1;
  vi->is_full_var = 1;
  vi->is_reg_var = 1;

  tmp.var = vi->id;
  tmp.type = SCALAR;
  tmp.offset = 0;

  return tmp;
}

/* Get a constraint expression vector from an SSA_VAR_P node.
   If address_p is true, the result will be taken its address of.  */

static void
get_constraint_for_ssa_var (tree t, vec<ce_s> *results, bool address_p)
{
  struct constraint_expr cexpr;
  varinfo_t vi;

  /* We allow FUNCTION_DECLs here even though it doesn't make much sense.  */
  gcc_assert (TREE_CODE (t) == SSA_NAME || DECL_P (t));

  if (TREE_CODE (t) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (t))
    {
      /* For parameters, get at the points-to set for the actual parm
	 decl.  */
      if (TREE_CODE (SSA_NAME_VAR (t)) == PARM_DECL
	  || TREE_CODE (SSA_NAME_VAR (t)) == RESULT_DECL)
	{
	  get_constraint_for_ssa_var (SSA_NAME_VAR (t), results, address_p);
	  return;
	}
      /* For undefined SSA names return nothing.  */
      else if (!ssa_defined_default_def_p (t))
	{
	  cexpr.var = nothing_id;
	  cexpr.type = SCALAR;
	  cexpr.offset = 0;
	  results->safe_push (cexpr);
	  return;
	}
    }

  /* For global variables resort to the alias target.  */
  if (VAR_P (t) && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      varpool_node *node = varpool_node::get (t);
      if (node && node->alias && node->analyzed)
	{
	  node = node->ultimate_alias_target ();
	  /* Canonicalize the PT uid of all aliases to the ultimate target.
	     ???  Hopefully the set of aliases can't change in a way that
	     changes the ultimate alias target.  */
	  gcc_assert ((! DECL_PT_UID_SET_P (node->decl)
		       || DECL_PT_UID (node->decl) == DECL_UID (node->decl))
		      && (! DECL_PT_UID_SET_P (t)
			  || DECL_PT_UID (t) == DECL_UID (node->decl)));
	  DECL_PT_UID (t) = DECL_UID (node->decl);
	  t = node->decl;
	}

      /* If this is decl may bind to NULL note that.  */
      if (address_p
	  && (! node || ! node->nonzero_address ()))
	{
	  cexpr.var = nothing_id;
	  cexpr.type = SCALAR;
	  cexpr.offset = 0;
	  results->safe_push (cexpr);
	}
    }

  vi = get_vi_for_tree (t);
  cexpr.var = vi->id;
  cexpr.type = SCALAR;
  cexpr.offset = 0;

  /* If we are not taking the address of the constraint expr, add all
     sub-fiels of the variable as well.  */
  if (!address_p
      && !vi->is_full_var)
    {
      for (; vi; vi = vi_next (vi))
	{
	  cexpr.var = vi->id;
	  results->safe_push (cexpr);
	}
      return;
    }

  results->safe_push (cexpr);
}

/* Process constraint T, performing various simplifications and then
   adding it to our list of overall constraints.  */

static void
process_constraint (constraint_t t)
{
  struct constraint_expr rhs = t->rhs;
  struct constraint_expr lhs = t->lhs;

  gcc_assert (rhs.var < varmap.length ());
  gcc_assert (lhs.var < varmap.length ());

  /* If we didn't get any useful constraint from the lhs we get
     &ANYTHING as fallback from get_constraint_for.  Deal with
     it here by turning it into *ANYTHING.  */
  if (lhs.type == ADDRESSOF
      && lhs.var == anything_id)
    t->lhs.type = lhs.type = DEREF;

  /* ADDRESSOF on the lhs is invalid.  */
  gcc_assert (lhs.type != ADDRESSOF);

  /* We shouldn't add constraints from things that cannot have pointers.
     It's not completely trivial to avoid in the callers, so do it here.  */
  if (rhs.type != ADDRESSOF
      && !get_varinfo (rhs.var)->may_have_pointers)
    return;

  /* Likewise adding to the solution of a non-pointer var isn't useful.  */
  if (!get_varinfo (lhs.var)->may_have_pointers)
    return;

  /* This can happen in our IR with things like n->a = *p.  */
  if (rhs.type == DEREF && lhs.type == DEREF && rhs.var != anything_id)
    {
      /* Split into tmp = *rhs, *lhs = tmp.  */
      struct constraint_expr tmplhs;
      tmplhs = new_scalar_tmp_constraint_exp ("doubledereftmp", true);
      process_constraint (new_constraint (tmplhs, rhs));
      process_constraint (new_constraint (lhs, tmplhs));
    }
  else if ((rhs.type != SCALAR || rhs.offset != 0) && lhs.type == DEREF)
    {
      /* Split into tmp = &rhs, *lhs = tmp.  */
      struct constraint_expr tmplhs;
      tmplhs = new_scalar_tmp_constraint_exp ("derefaddrtmp", true);
      process_constraint (new_constraint (tmplhs, rhs));
      process_constraint (new_constraint (lhs, tmplhs));
    }
  else
    {
      gcc_assert (rhs.type != ADDRESSOF || rhs.offset == 0);
      if (rhs.type == ADDRESSOF)
	get_varinfo (get_varinfo (rhs.var)->head)->address_taken = true;
      constraints.safe_push (t);
    }
}


/* Return the position, in bits, of FIELD_DECL from the beginning of its
   structure.  */

static unsigned HOST_WIDE_INT
bitpos_of_field (const tree fdecl)
{
  if (!tree_fits_uhwi_p (DECL_FIELD_OFFSET (fdecl))
      || !tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (fdecl)))
    return -1;

  return (tree_to_uhwi (DECL_FIELD_OFFSET (fdecl)) * BITS_PER_UNIT
	  + tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fdecl)));
}


/* Get constraint expressions for offsetting PTR by OFFSET.  Stores the
   resulting constraint expressions in *RESULTS.  */

static void
get_constraint_for_ptr_offset (tree ptr, tree offset,
			       vec<ce_s> *results)
{
  struct constraint_expr c;
  unsigned int j, n;
  HOST_WIDE_INT rhsoffset;

  /* If we do not do field-sensitive PTA adding offsets to pointers
     does not change the points-to solution.  */
  if (!use_field_sensitive)
    {
      get_constraint_for_rhs (ptr, results);
      return;
    }

  /* If the offset is not a non-negative integer constant that fits
     in a HOST_WIDE_INT, we have to fall back to a conservative
     solution which includes all sub-fields of all pointed-to
     variables of ptr.  */
  if (offset == NULL_TREE
      || TREE_CODE (offset) != INTEGER_CST)
    rhsoffset = UNKNOWN_OFFSET;
  else
    {
      /* Sign-extend the offset.  */
      offset_int soffset = offset_int::from (wi::to_wide (offset), SIGNED);
      if (!wi::fits_shwi_p (soffset))
	rhsoffset = UNKNOWN_OFFSET;
      else
	{
	  /* Make sure the bit-offset also fits.  */
	  HOST_WIDE_INT rhsunitoffset = soffset.to_shwi ();
	  rhsoffset = rhsunitoffset * (unsigned HOST_WIDE_INT) BITS_PER_UNIT;
	  if (rhsunitoffset != rhsoffset / BITS_PER_UNIT)
	    rhsoffset = UNKNOWN_OFFSET;
	}
    }

  get_constraint_for_rhs (ptr, results);
  if (rhsoffset == 0)
    return;

  /* As we are eventually appending to the solution do not use
     vec::iterate here.  */
  n = results->length ();
  for (j = 0; j < n; j++)
    {
      varinfo_t curr;
      c = (*results)[j];
      curr = get_varinfo (c.var);

      if (c.type == ADDRESSOF
	  /* If this varinfo represents a full variable just use it.  */
	  && curr->is_full_var)
	;
      else if (c.type == ADDRESSOF
	       /* If we do not know the offset add all subfields.  */
	       && rhsoffset == UNKNOWN_OFFSET)
	{
	  varinfo_t temp = get_varinfo (curr->head);
	  do
	    {
	      struct constraint_expr c2;
	      c2.var = temp->id;
	      c2.type = ADDRESSOF;
	      c2.offset = 0;
	      if (c2.var != c.var)
		results->safe_push (c2);
	      temp = vi_next (temp);
	    }
	  while (temp);
	}
      else if (c.type == ADDRESSOF)
	{
	  varinfo_t temp;
	  unsigned HOST_WIDE_INT offset = curr->offset + rhsoffset;

	  /* If curr->offset + rhsoffset is less than zero adjust it.  */
	  if (rhsoffset < 0
	      && curr->offset < offset)
	    offset = 0;

	  /* We have to include all fields that overlap the current
	     field shifted by rhsoffset.  And we include at least
	     the last or the first field of the variable to represent
	     reachability of off-bound addresses, in particular &object + 1,
	     conservatively correct.  */
	  temp = first_or_preceding_vi_for_offset (curr, offset);
	  c.var = temp->id;
	  c.offset = 0;
	  temp = vi_next (temp);
	  while (temp
		 && temp->offset < offset + curr->size)
	    {
	      struct constraint_expr c2;
	      c2.var = temp->id;
	      c2.type = ADDRESSOF;
	      c2.offset = 0;
	      results->safe_push (c2);
	      temp = vi_next (temp);
	    }
	}
      else if (c.type == SCALAR)
	{
	  gcc_assert (c.offset == 0);
	  c.offset = rhsoffset;
	}
      else
	/* We shouldn't get any DEREFs here.  */
	gcc_unreachable ();

      (*results)[j] = c;
    }
}


/* Given a COMPONENT_REF T, return the constraint_expr vector for it.
   If address_p is true the result will be taken its address of.
   If lhs_p is true then the constraint expression is assumed to be used
   as the lhs.  */

static void
get_constraint_for_component_ref (tree t, vec<ce_s> *results,
				  bool address_p, bool lhs_p)
{
  tree orig_t = t;
  poly_int64 bitsize = -1;
  poly_int64 bitmaxsize = -1;
  poly_int64 bitpos;
  bool reverse;
  tree forzero;

  /* Some people like to do cute things like take the address of
     &0->a.b.  */
  forzero = t;
  while (handled_component_p (forzero)
	 || INDIRECT_REF_P (forzero)
	 || TREE_CODE (forzero) == MEM_REF)
    forzero = TREE_OPERAND (forzero, 0);

  if (CONSTANT_CLASS_P (forzero) && integer_zerop (forzero))
    {
      struct constraint_expr temp;

      temp.offset = 0;
      temp.var = integer_id;
      temp.type = SCALAR;
      results->safe_push (temp);
      return;
    }

  t = get_ref_base_and_extent (t, &bitpos, &bitsize, &bitmaxsize, &reverse);

  /* We can end up here for component references on a
     VIEW_CONVERT_EXPR <>(&foobar) or things like a
     BIT_FIELD_REF <&MEM[(void *)&b + 4B], ...>.  So for
     symbolic constants simply give up.  */
  if (TREE_CODE (t) == ADDR_EXPR)
    {
      constraint_expr result;
      result.type = SCALAR;
      result.var = anything_id;
      result.offset = 0;
      results->safe_push (result);
      return;
    }

  /* Avoid creating pointer-offset constraints, so handle MEM_REF
     offsets directly.  Pretend to take the address of the base,
     we'll take care of adding the required subset of sub-fields below.  */
  if (TREE_CODE (t) == MEM_REF
      && !integer_zerop (TREE_OPERAND (t, 0)))
    {
      poly_offset_int off = mem_ref_offset (t);
      off <<= LOG2_BITS_PER_UNIT;
      off += bitpos;
      poly_int64 off_hwi;
      if (off.to_shwi (&off_hwi))
	bitpos = off_hwi;
      else
	{
	  bitpos = 0;
	  bitmaxsize = -1;
	}
      get_constraint_for_1 (TREE_OPERAND (t, 0), results, false, lhs_p);
      do_deref (results);
    }
  else
    get_constraint_for_1 (t, results, true, lhs_p);

  /* Strip off nothing_id.  */
  if (results->length () == 2)
    {
      gcc_assert ((*results)[0].var == nothing_id);
      results->unordered_remove (0);
    }
  gcc_assert (results->length () == 1);
  struct constraint_expr &result = results->last ();

  if (result.type == SCALAR
      && get_varinfo (result.var)->is_full_var)
    /* For single-field vars do not bother about the offset.  */
    result.offset = 0;
  else if (result.type == SCALAR)
    {
      /* In languages like C, you can access one past the end of an
	 array.  You aren't allowed to dereference it, so we can
	 ignore this constraint.  When we handle pointer subtraction,
	 we may have to do something cute here.  */

      if (maybe_lt (poly_uint64 (bitpos), get_varinfo (result.var)->fullsize)
	  && maybe_ne (bitmaxsize, 0))
	{
	  /* It's also not true that the constraint will actually start at the
	     right offset, it may start in some padding.  We only care about
	     setting the constraint to the first actual field it touches, so
	     walk to find it.  */
	  struct constraint_expr cexpr = result;
	  varinfo_t curr;
	  results->pop ();
	  cexpr.offset = 0;
	  for (curr = get_varinfo (cexpr.var); curr; curr = vi_next (curr))
	    {
	      if (ranges_maybe_overlap_p (poly_int64 (curr->offset),
					  curr->size, bitpos, bitmaxsize))
		{
		  cexpr.var = curr->id;
		  results->safe_push (cexpr);
		  if (address_p)
		    break;
		}
	    }
	  /* If we are going to take the address of this field then
	     to be able to compute reachability correctly add at least
	     the last field of the variable.  */
	  if (address_p && results->length () == 0)
	    {
	      curr = get_varinfo (cexpr.var);
	      while (curr->next != 0)
		curr = vi_next (curr);
	      cexpr.var = curr->id;
	      results->safe_push (cexpr);
	    }
	  else if (results->length () == 0)
	    /* Assert that we found *some* field there.  The user couldn't be
	       accessing *only* padding.  */
	    /* Still the user could access one past the end of an array
	       embedded in a struct resulting in accessing *only* padding.  */
	    /* Or accessing only padding via type-punning to a type
	       that has a filed just in padding space.  */
	    {
	      cexpr.type = SCALAR;
	      cexpr.var = anything_id;
	      cexpr.offset = 0;
	      results->safe_push (cexpr);
	    }
	}
      else if (known_eq (bitmaxsize, 0))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Access to zero-sized part of variable, "
		     "ignoring\n");
	}
      else
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Access to past the end of variable, ignoring\n");
    }
  else if (result.type == DEREF)
    {
      /* If we do not know exactly where the access goes say so.  Note
	 that only for non-structure accesses we know that we access
	 at most one subfiled of any variable.  */
      HOST_WIDE_INT const_bitpos;
      if (!bitpos.is_constant (&const_bitpos)
	  || const_bitpos == -1
	  || maybe_ne (bitsize, bitmaxsize)
	  || AGGREGATE_TYPE_P (TREE_TYPE (orig_t))
	  || result.offset == UNKNOWN_OFFSET)
	result.offset = UNKNOWN_OFFSET;
      else
	result.offset += const_bitpos;
    }
  else if (result.type == ADDRESSOF)
    {
      /* We can end up here for component references on constants like
	 VIEW_CONVERT_EXPR <>({ 0, 1, 2, 3 })[i].  */
      result.type = SCALAR;
      result.var = anything_id;
      result.offset = 0;
    }
  else
    gcc_unreachable ();
}


/* Dereference the constraint expression CONS, and return the result.
   DEREF (ADDRESSOF) = SCALAR
   DEREF (SCALAR) = DEREF
   DEREF (DEREF) = (temp = DEREF1; result = DEREF (temp))
   This is needed so that we can handle dereferencing DEREF constraints.  */

static void
do_deref (vec<ce_s> *constraints)
{
  struct constraint_expr *c;
  unsigned int i = 0;

  FOR_EACH_VEC_ELT (*constraints, i, c)
    {
      if (c->type == SCALAR)
	c->type = DEREF;
      else if (c->type == ADDRESSOF)
	c->type = SCALAR;
      else if (c->type == DEREF)
	{
	  struct constraint_expr tmplhs;
	  tmplhs = new_scalar_tmp_constraint_exp ("dereftmp", true);
	  process_constraint (new_constraint (tmplhs, *c));
	  c->var = tmplhs.var;
	}
      else
	gcc_unreachable ();
    }
}

/* Given a tree T, return the constraint expression for taking the
   address of it.  */

static void
get_constraint_for_address_of (tree t, vec<ce_s> *results)
{
  struct constraint_expr *c;
  unsigned int i;

  get_constraint_for_1 (t, results, true, true);

  FOR_EACH_VEC_ELT (*results, i, c)
    {
      if (c->type == DEREF)
	c->type = SCALAR;
      else
	c->type = ADDRESSOF;
    }
}

/* Given a tree T, return the constraint expression for it.  */

static void
get_constraint_for_1 (tree t, vec<ce_s> *results, bool address_p,
		      bool lhs_p)
{
  struct constraint_expr temp;

  /* x = integer is all glommed to a single variable, which doesn't
     point to anything by itself.  That is, of course, unless it is an
     integer constant being treated as a pointer, in which case, we
     will return that this is really the addressof anything.  This
     happens below, since it will fall into the default case.  The only
     case we know something about an integer treated like a pointer is
     when it is the NULL pointer, and then we just say it points to
     NULL.

     Do not do that if -fno-delete-null-pointer-checks though, because
     in that case *NULL does not fail, so it _should_ alias *anything.
     It is not worth adding a new option or renaming the existing one,
     since this case is relatively obscure.  */
  if ((TREE_CODE (t) == INTEGER_CST
       && integer_zerop (t))
      /* The only valid CONSTRUCTORs in gimple with pointer typed
	 elements are zero-initializer.  But in IPA mode we also
	 process global initializers, so verify at least.  */
      || (TREE_CODE (t) == CONSTRUCTOR
	  && CONSTRUCTOR_NELTS (t) == 0))
    {
      if (flag_delete_null_pointer_checks)
	temp.var = nothing_id;
      else
	temp.var = nonlocal_id;
      temp.type = ADDRESSOF;
      temp.offset = 0;
      results->safe_push (temp);
      return;
    }

  /* String constants are read-only, ideally we'd have a CONST_DECL
     for those.  */
  if (TREE_CODE (t) == STRING_CST)
    {
      temp.var = string_id;
      temp.type = SCALAR;
      temp.offset = 0;
      results->safe_push (temp);
      return;
    }

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_expression:
      {
	switch (TREE_CODE (t))
	  {
	  case ADDR_EXPR:
	    get_constraint_for_address_of (TREE_OPERAND (t, 0), results);
	    return;
	  default:;
	  }
	break;
      }
    case tcc_reference:
      {
	if (!lhs_p && TREE_THIS_VOLATILE (t))
	  /* Fall back to anything.  */
	  break;

	switch (TREE_CODE (t))
	  {
	  case MEM_REF:
	    {
	      struct constraint_expr cs;
	      varinfo_t vi, curr;
	      get_constraint_for_ptr_offset (TREE_OPERAND (t, 0),
					     TREE_OPERAND (t, 1), results);
	      do_deref (results);

	      /* If we are not taking the address then make sure to process
		 all subvariables we might access.  */
	      if (address_p)
		return;

	      cs = results->last ();
	      if (cs.type == DEREF
		  && type_can_have_subvars (TREE_TYPE (t)))
		{
		  /* For dereferences this means we have to defer it
		     to solving time.  */
		  results->last ().offset = UNKNOWN_OFFSET;
		  return;
		}
	      if (cs.type != SCALAR)
		return;

	      vi = get_varinfo (cs.var);
	      curr = vi_next (vi);
	      if (!vi->is_full_var
		  && curr)
		{
		  unsigned HOST_WIDE_INT size;
		  if (tree_fits_uhwi_p (TYPE_SIZE (TREE_TYPE (t))))
		    size = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (t)));
		  else
		    size = -1;
		  for (; curr; curr = vi_next (curr))
		    {
		      /* The start of the access might happen anywhere
			 within vi, so conservatively assume it was
			 at its end.  */
		      if (curr->offset - (vi->offset + vi->size - 1) < size)
			{
			  cs.var = curr->id;
			  results->safe_push (cs);
			}
		      else
			break;
		    }
		}
	      return;
	    }
	  case ARRAY_REF:
	  case ARRAY_RANGE_REF:
	  case COMPONENT_REF:
	  case IMAGPART_EXPR:
	  case REALPART_EXPR:
	  case BIT_FIELD_REF:
	    get_constraint_for_component_ref (t, results, address_p, lhs_p);
	    return;
	  case VIEW_CONVERT_EXPR:
	    get_constraint_for_1 (TREE_OPERAND (t, 0), results, address_p,
				  lhs_p);
	    return;
	  /* We are missing handling for TARGET_MEM_REF here.  */
	  default:;
	  }
	break;
      }
    case tcc_exceptional:
      {
	switch (TREE_CODE (t))
	  {
	  case SSA_NAME:
	    {
	      get_constraint_for_ssa_var (t, results, address_p);
	      return;
	    }
	  case CONSTRUCTOR:
	    {
	      unsigned int i;
	      tree val;
	      auto_vec<ce_s> tmp;
	      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (t), i, val)
		{
		  struct constraint_expr *rhsp;
		  unsigned j;
		  get_constraint_for_1 (val, &tmp, address_p, lhs_p);
		  FOR_EACH_VEC_ELT (tmp, j, rhsp)
		    results->safe_push (*rhsp);
		  tmp.truncate (0);
		}
	      /* We do not know whether the constructor was complete,
		 so technically we have to add &NOTHING or &ANYTHING
		 like we do for an empty constructor as well.  */
	      return;
	    }
	  default:;
	  }
	break;
      }
    case tcc_declaration:
      {
	if (!lhs_p && VAR_P (t) && TREE_THIS_VOLATILE (t))
	  /* Fall back to anything.  */
	  break;
	get_constraint_for_ssa_var (t, results, address_p);
	return;
      }
    case tcc_constant:
      {
	/* We cannot refer to automatic variables through constants.  */
	temp.type = ADDRESSOF;
	temp.var = nonlocal_id;
	temp.offset = 0;
	results->safe_push (temp);
	return;
      }
    default:;
    }

  /* The default fallback is a constraint from anything.  */
  temp.type = ADDRESSOF;
  temp.var = anything_id;
  temp.offset = 0;
  results->safe_push (temp);
}

/* Given a gimple tree T, return the constraint expression vector for it.  */

static void
get_constraint_for (tree t, vec<ce_s> *results)
{
  gcc_assert (results->length () == 0);

  get_constraint_for_1 (t, results, false, true);
}

/* Given a gimple tree T, return the constraint expression vector for it
   to be used as the rhs of a constraint.  */

static void
get_constraint_for_rhs (tree t, vec<ce_s> *results)
{
  gcc_assert (results->length () == 0);

  get_constraint_for_1 (t, results, false, false);
}


/* Efficiently generates constraints from all entries in *RHSC to all
   entries in *LHSC.  */

static void
process_all_all_constraints (const vec<ce_s> &lhsc,
			     const vec<ce_s> &rhsc)
{
  struct constraint_expr *lhsp, *rhsp;
  unsigned i, j;

  if (lhsc.length () <= 1 || rhsc.length () <= 1)
    {
      FOR_EACH_VEC_ELT (lhsc, i, lhsp)
	FOR_EACH_VEC_ELT (rhsc, j, rhsp)
	  process_constraint (new_constraint (*lhsp, *rhsp));
    }
  else
    {
      struct constraint_expr tmp;
      tmp = new_scalar_tmp_constraint_exp ("allalltmp", true);
      FOR_EACH_VEC_ELT (rhsc, i, rhsp)
	process_constraint (new_constraint (tmp, *rhsp));
      FOR_EACH_VEC_ELT (lhsc, i, lhsp)
	process_constraint (new_constraint (*lhsp, tmp));
    }
}

/* Handle aggregate copies by expanding into copies of the respective
   fields of the structures.  */

static void
do_structure_copy (tree lhsop, tree rhsop)
{
  struct constraint_expr *lhsp, *rhsp;
  auto_vec<ce_s> lhsc;
  auto_vec<ce_s> rhsc;
  unsigned j;

  get_constraint_for (lhsop, &lhsc);
  get_constraint_for_rhs (rhsop, &rhsc);
  lhsp = &lhsc[0];
  rhsp = &rhsc[0];
  if (lhsp->type == DEREF
      || (lhsp->type == ADDRESSOF && lhsp->var == anything_id)
      || rhsp->type == DEREF)
    {
      if (lhsp->type == DEREF)
	{
	  gcc_assert (lhsc.length () == 1);
	  lhsp->offset = UNKNOWN_OFFSET;
	}
      if (rhsp->type == DEREF)
	{
	  gcc_assert (rhsc.length () == 1);
	  rhsp->offset = UNKNOWN_OFFSET;
	}
      process_all_all_constraints (lhsc, rhsc);
    }
  else if (lhsp->type == SCALAR
	   && (rhsp->type == SCALAR
	       || rhsp->type == ADDRESSOF))
    {
      HOST_WIDE_INT lhssize, lhsoffset;
      HOST_WIDE_INT rhssize, rhsoffset;
      bool reverse;
      unsigned k = 0;
      if (!get_ref_base_and_extent_hwi (lhsop, &lhsoffset, &lhssize, &reverse)
	  || !get_ref_base_and_extent_hwi (rhsop, &rhsoffset, &rhssize,
					   &reverse))
	{
	  process_all_all_constraints (lhsc, rhsc);
	  return;
	}
      for (j = 0; lhsc.iterate (j, &lhsp);)
	{
	  varinfo_t lhsv, rhsv;
	  rhsp = &rhsc[k];
	  lhsv = get_varinfo (lhsp->var);
	  rhsv = get_varinfo (rhsp->var);
	  if (lhsv->may_have_pointers
	      && (lhsv->is_full_var
		  || rhsv->is_full_var
		  || ranges_overlap_p (lhsv->offset + rhsoffset, lhsv->size,
				       rhsv->offset + lhsoffset, rhsv->size)))
	    process_constraint (new_constraint (*lhsp, *rhsp));
	  if (!rhsv->is_full_var
	      && (lhsv->is_full_var
		  || (lhsv->offset + rhsoffset + lhsv->size
		      > rhsv->offset + lhsoffset + rhsv->size)))
	    {
	      ++k;
	      if (k >= rhsc.length ())
		break;
	    }
	  else
	    ++j;
	}
    }
  else
    gcc_unreachable ();
}

/* Create constraints ID = { rhsc }.  */

static void
make_constraints_to (unsigned id, const vec<ce_s> &rhsc)
{
  struct constraint_expr *c;
  struct constraint_expr includes;
  unsigned int j;

  includes.var = id;
  includes.offset = 0;
  includes.type = SCALAR;

  FOR_EACH_VEC_ELT (rhsc, j, c)
    process_constraint (new_constraint (includes, *c));
}

/* Create a constraint ID = OP.  */

static void
make_constraint_to (unsigned id, tree op)
{
  auto_vec<ce_s> rhsc;
  get_constraint_for_rhs (op, &rhsc);
  make_constraints_to (id, rhsc);
}

/* Create a constraint ID = &FROM.  */

static void
make_constraint_from (varinfo_t vi, int from)
{
  struct constraint_expr lhs, rhs;

  lhs.var = vi->id;
  lhs.offset = 0;
  lhs.type = SCALAR;

  rhs.var = from;
  rhs.offset = 0;
  rhs.type = ADDRESSOF;
  process_constraint (new_constraint (lhs, rhs));
}

/* Create a constraint ID = FROM.  */

static void
make_copy_constraint (varinfo_t vi, int from)
{
  struct constraint_expr lhs, rhs;

  lhs.var = vi->id;
  lhs.offset = 0;
  lhs.type = SCALAR;

  rhs.var = from;
  rhs.offset = 0;
  rhs.type = SCALAR;
  process_constraint (new_constraint (lhs, rhs));
}

/* Make constraints necessary to make OP escape.  */

static void
make_escape_constraint (tree op)
{
  make_constraint_to (escaped_id, op);
}

/* Make constraint necessary to make all indirect references
   from VI escape.  */

static void
make_indirect_escape_constraint (varinfo_t vi)
{
  struct constraint_expr lhs, rhs;
  /* escaped = *(VAR + UNKNOWN);  */
  lhs.type = SCALAR;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = DEREF;
  rhs.var = vi->id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));
}

/* Add constraints to that the solution of VI is transitively closed.  */

static void
make_transitive_closure_constraints (varinfo_t vi)
{
  struct constraint_expr lhs, rhs;

  /* VAR = *(VAR + UNKNOWN);  */
  lhs.type = SCALAR;
  lhs.var = vi->id;
  lhs.offset = 0;
  rhs.type = DEREF;
  rhs.var = vi->id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));
}

/* Add constraints to that the solution of VI has all subvariables added.  */

static void
make_any_offset_constraints (varinfo_t vi)
{
  struct constraint_expr lhs, rhs;

  /* VAR = VAR + UNKNOWN;  */
  lhs.type = SCALAR;
  lhs.var = vi->id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = vi->id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));
}

/* Temporary storage for fake var decls.  */
struct obstack fake_var_decl_obstack;

/* Build a fake VAR_DECL acting as referrer to a DECL_UID.  */

static tree
build_fake_var_decl (tree type)
{
  tree decl = (tree) XOBNEW (&fake_var_decl_obstack, struct tree_var_decl);
  memset (decl, 0, sizeof (struct tree_var_decl));
  TREE_SET_CODE (decl, VAR_DECL);
  TREE_TYPE (decl) = type;
  DECL_UID (decl) = allocate_decl_uid ();
  SET_DECL_PT_UID (decl, -1);
  layout_decl (decl, 0);
  return decl;
}

/* Create a new artificial heap variable with NAME.
   Return the created variable.  */

static varinfo_t
make_heapvar (const char *name, bool add_id)
{
  varinfo_t vi;
  tree heapvar;

  heapvar = build_fake_var_decl (ptr_type_node);
  DECL_EXTERNAL (heapvar) = 1;

  vi = new_var_info (heapvar, name, add_id);
  vi->is_heap_var = true;
  vi->is_unknown_size_var = true;
  vi->offset = 0;
  vi->fullsize = ~0;
  vi->size = ~0;
  vi->is_full_var = true;
  insert_vi_for_tree (heapvar, vi);

  return vi;
}

/* Create a new artificial heap variable with NAME and make a
   constraint from it to LHS.  Set flags according to a tag used
   for tracking restrict pointers.  */

static varinfo_t
make_constraint_from_restrict (varinfo_t lhs, const char *name, bool add_id)
{
  varinfo_t vi = make_heapvar (name, add_id);
  vi->is_restrict_var = 1;
  vi->is_global_var = 1;
  vi->may_have_pointers = 1;
  make_constraint_from (lhs, vi->id);
  return vi;
}

/* Create a new artificial heap variable with NAME and make a
   constraint from it to LHS.  Set flags according to a tag used
   for tracking restrict pointers and make the artificial heap
   point to global memory.  */

static varinfo_t
make_constraint_from_global_restrict (varinfo_t lhs, const char *name,
				      bool add_id)
{
  varinfo_t vi = make_constraint_from_restrict (lhs, name, add_id);
  make_copy_constraint (vi, nonlocal_id);
  return vi;
}

/* Get a constraint for the requested part of a function designator FI
   when operating in IPA mode.  */

static struct constraint_expr
get_function_part_constraint (varinfo_t fi, unsigned part)
{
  struct constraint_expr c;

  gcc_assert (in_ipa_mode);

  if (fi->id == anything_id)
    {
      /* ???  We probably should have a ANYFN special variable.  */
      c.var = anything_id;
      c.offset = 0;
      c.type = SCALAR;
    }
  else if (fi->decl && TREE_CODE (fi->decl) == FUNCTION_DECL)
    {
      varinfo_t ai = first_vi_for_offset (fi, part);
      if (ai)
	c.var = ai->id;
      else
	c.var = anything_id;
      c.offset = 0;
      c.type = SCALAR;
    }
  else
    {
      c.var = fi->id;
      c.offset = part;
      c.type = DEREF;
    }

  return c;
}

/* Produce constraints for argument ARG of call STMT with eaf flags
   FLAGS.  RESULTS is array holding constraints for return value.
   CALLESCAPE_ID is variable where call loocal escapes are added.
   WRITES_GLOVEL_MEMORY is true if callee may write global memory.  */

static void
handle_call_arg (gcall *stmt, tree arg, vec<ce_s> *results, int flags,
		 int callescape_id, bool writes_global_memory)
{
  int relevant_indirect_flags = EAF_NO_INDIRECT_CLOBBER | EAF_NO_INDIRECT_READ
				| EAF_NO_INDIRECT_ESCAPE;
  int relevant_flags = relevant_indirect_flags
		       | EAF_NO_DIRECT_CLOBBER
		       | EAF_NO_DIRECT_READ
		       | EAF_NO_DIRECT_ESCAPE;
  if (gimple_call_lhs (stmt))
    {
      relevant_flags |= EAF_NOT_RETURNED_DIRECTLY | EAF_NOT_RETURNED_INDIRECTLY;
      relevant_indirect_flags |= EAF_NOT_RETURNED_INDIRECTLY;

      /* If value is never read from it can not be returned indirectly
	 (except through the escape solution).
	 For all flags we get these implications right except for
	 not_returned because we miss return functions in ipa-prop.  */

      if (flags & EAF_NO_DIRECT_READ)
	flags |= EAF_NOT_RETURNED_INDIRECTLY;
    }

  /* If the argument is not used we can ignore it.
     Similarly argument is invisile for us if it not clobbered, does not
     escape, is not read and can not be returned.  */
  if ((flags & EAF_UNUSED) || ((flags & relevant_flags) == relevant_flags))
    return;

  /* Produce varinfo for direct accesses to ARG.  */
  varinfo_t tem = new_var_info (NULL_TREE, "callarg", true);
  tem->is_reg_var = true;
  make_constraint_to (tem->id, arg);
  make_any_offset_constraints (tem);

  bool callarg_transitive = false;

  /* As an compile time optimization if we make no difference between
     direct and indirect accesses make arg transitively closed.
     This avoids the need to build indir arg and do everything twice.  */
  if (((flags & EAF_NO_INDIRECT_CLOBBER) != 0)
      == ((flags & EAF_NO_DIRECT_CLOBBER) != 0)
      && (((flags & EAF_NO_INDIRECT_READ) != 0)
	  == ((flags & EAF_NO_DIRECT_READ) != 0))
      && (((flags & EAF_NO_INDIRECT_ESCAPE) != 0)
	  == ((flags & EAF_NO_DIRECT_ESCAPE) != 0))
      && (((flags & EAF_NOT_RETURNED_INDIRECTLY) != 0)
	  == ((flags & EAF_NOT_RETURNED_DIRECTLY) != 0)))
    {
      make_transitive_closure_constraints (tem);
      callarg_transitive = true;
    }

  /* If necessary, produce varinfo for indirect accesses to ARG.  */
  varinfo_t indir_tem = NULL;
  if (!callarg_transitive
      && (flags & relevant_indirect_flags) != relevant_indirect_flags)
    {
      struct constraint_expr lhs, rhs;
      indir_tem = new_var_info (NULL_TREE, "indircallarg", true);
      indir_tem->is_reg_var = true;

      /* indir_term = *tem.  */
      lhs.type = SCALAR;
      lhs.var = indir_tem->id;
      lhs.offset = 0;

      rhs.type = DEREF;
      rhs.var = tem->id;
      rhs.offset = UNKNOWN_OFFSET;
      process_constraint (new_constraint (lhs, rhs));

      make_any_offset_constraints (indir_tem);

      /* If we do not read indirectly there is no need for transitive closure.
	 We know there is only one level of indirection.  */
      if (!(flags & EAF_NO_INDIRECT_READ))
	make_transitive_closure_constraints (indir_tem);
      gcc_checking_assert (!(flags & EAF_NO_DIRECT_READ));
    }

  if (gimple_call_lhs (stmt))
    {
      if (!(flags & EAF_NOT_RETURNED_DIRECTLY))
	{
	  struct constraint_expr cexpr;
	  cexpr.var = tem->id;
	  cexpr.type = SCALAR;
	  cexpr.offset = 0;
	  results->safe_push (cexpr);
	}
      if (!callarg_transitive & !(flags & EAF_NOT_RETURNED_INDIRECTLY))
	{
	  struct constraint_expr cexpr;
	  cexpr.var = indir_tem->id;
	  cexpr.type = SCALAR;
	  cexpr.offset = 0;
	  results->safe_push (cexpr);
	}
    }

  if (!(flags & EAF_NO_DIRECT_READ))
    {
      varinfo_t uses = get_call_use_vi (stmt);
      make_copy_constraint (uses, tem->id);
      if (!callarg_transitive & !(flags & EAF_NO_INDIRECT_READ))
	make_copy_constraint (uses, indir_tem->id);
    }
  else
    /* To read indirectly we need to read directly.  */
    gcc_checking_assert (flags & EAF_NO_INDIRECT_READ);

  if (!(flags & EAF_NO_DIRECT_CLOBBER))
    {
      struct constraint_expr lhs, rhs;

      /* *arg = callescape.  */
      lhs.type = DEREF;
      lhs.var = tem->id;
      lhs.offset = 0;

      rhs.type = SCALAR;
      rhs.var = callescape_id;
      rhs.offset = 0;
      process_constraint (new_constraint (lhs, rhs));

      /* callclobbered = arg.  */
      make_copy_constraint (get_call_clobber_vi (stmt), tem->id);
    }
  if (!callarg_transitive & !(flags & EAF_NO_INDIRECT_CLOBBER))
    {
      struct constraint_expr lhs, rhs;

      /* *indir_arg = callescape.  */
      lhs.type = DEREF;
      lhs.var = indir_tem->id;
      lhs.offset = 0;

      rhs.type = SCALAR;
      rhs.var = callescape_id;
      rhs.offset = 0;
      process_constraint (new_constraint (lhs, rhs));

      /* callclobbered = indir_arg.  */
      make_copy_constraint (get_call_clobber_vi (stmt), indir_tem->id);
    }

  if (!(flags & (EAF_NO_DIRECT_ESCAPE | EAF_NO_INDIRECT_ESCAPE)))
    {
      struct constraint_expr lhs, rhs;

      /* callescape = arg;  */
      lhs.var = callescape_id;
      lhs.offset = 0;
      lhs.type = SCALAR;

      rhs.var = tem->id;
      rhs.offset = 0;
      rhs.type = SCALAR;
      process_constraint (new_constraint (lhs, rhs));

      if (writes_global_memory)
	make_escape_constraint (arg);
    }
  else if (!callarg_transitive & !(flags & EAF_NO_INDIRECT_ESCAPE))
    {
      struct constraint_expr lhs, rhs;

      /* callescape = *(indir_arg + UNKNOWN);  */
      lhs.var = callescape_id;
      lhs.offset = 0;
      lhs.type = SCALAR;

      rhs.var = indir_tem->id;
      rhs.offset = 0;
      rhs.type = SCALAR;
      process_constraint (new_constraint (lhs, rhs));

      if (writes_global_memory)
	make_indirect_escape_constraint (tem);
    }
}

/* For non-IPA mode, generate constraints necessary for a call on the
   RHS and collect return value constraint to RESULTS to be used later in
   handle_lhs_call.

   IMPLICIT_EAF_FLAGS are added to each function argument.  If
   WRITES_GLOBAL_MEMORY is true function is assumed to possibly write to global
   memory.  Similar for READS_GLOBAL_MEMORY.  */

static void
handle_rhs_call (gcall *stmt, vec<ce_s> *results,
		 int implicit_eaf_flags,
		 bool writes_global_memory,
		 bool reads_global_memory)
{
  determine_global_memory_access (stmt, &writes_global_memory,
				  &reads_global_memory,
				  NULL);

  varinfo_t callescape = new_var_info (NULL_TREE, "callescape", true);

  /* If function can use global memory, add it to callescape
     and to possible return values.  If not we can still use/return addresses
     of global symbols.  */
  struct constraint_expr lhs, rhs;

  lhs.type = SCALAR;
  lhs.var = callescape->id;
  lhs.offset = 0;

  rhs.type = reads_global_memory ? SCALAR : ADDRESSOF;
  rhs.var = nonlocal_id;
  rhs.offset = 0;

  process_constraint (new_constraint (lhs, rhs));
  results->safe_push (rhs);

  varinfo_t uses = get_call_use_vi (stmt);
  make_copy_constraint (uses, callescape->id);

  for (unsigned i = 0; i < gimple_call_num_args (stmt); ++i)
    {
      tree arg = gimple_call_arg (stmt, i);
      int flags = gimple_call_arg_flags (stmt, i);
      handle_call_arg (stmt, arg, results,
		       flags | implicit_eaf_flags,
		       callescape->id, writes_global_memory);
    }

  /* The static chain escapes as well.  */
  if (gimple_call_chain (stmt))
    handle_call_arg (stmt, gimple_call_chain (stmt), results,
		     implicit_eaf_flags
		     | gimple_call_static_chain_flags (stmt),
		     callescape->id, writes_global_memory);

  /* And if we applied NRV the address of the return slot escapes as well.  */
  if (gimple_call_return_slot_opt_p (stmt)
      && gimple_call_lhs (stmt) != NULL_TREE
      && TREE_ADDRESSABLE (TREE_TYPE (gimple_call_lhs (stmt))))
    {
      int flags = gimple_call_retslot_flags (stmt);
      const int relevant_flags = EAF_NO_DIRECT_ESCAPE
				 | EAF_NOT_RETURNED_DIRECTLY;

      if (!(flags & EAF_UNUSED) && (flags & relevant_flags) != relevant_flags)
	{
	  auto_vec<ce_s> tmpc;

	  get_constraint_for_address_of (gimple_call_lhs (stmt), &tmpc);

	  if (!(flags & EAF_NO_DIRECT_ESCAPE))
	    {
	      make_constraints_to (callescape->id, tmpc);
	      if (writes_global_memory)
		make_constraints_to (escaped_id, tmpc);
	    }
	  if (!(flags & EAF_NOT_RETURNED_DIRECTLY))
	    {
	      struct constraint_expr *c;
	      unsigned i;
	      FOR_EACH_VEC_ELT (tmpc, i, c)
		results->safe_push (*c);
	    }
	}
    }
}

/* For non-IPA mode, generate constraints necessary for a call
   that returns a pointer and assigns it to LHS.  This simply makes
   the LHS point to global and escaped variables.  */

static void
handle_lhs_call (gcall *stmt, tree lhs, int flags, vec<ce_s> &rhsc,
		 tree fndecl)
{
  auto_vec<ce_s> lhsc;

  get_constraint_for (lhs, &lhsc);
  /* If the store is to a global decl make sure to
     add proper escape constraints.  */
  lhs = get_base_address (lhs);
  if (lhs
      && DECL_P (lhs)
      && is_global_var (lhs))
    {
      struct constraint_expr tmpc;
      tmpc.var = escaped_id;
      tmpc.offset = 0;
      tmpc.type = SCALAR;
      lhsc.safe_push (tmpc);
    }

  /* If the call returns an argument unmodified override the rhs
     constraints.  */
  if (flags & ERF_RETURNS_ARG
      && (flags & ERF_RETURN_ARG_MASK) < gimple_call_num_args (stmt))
    {
      tree arg;
      rhsc.truncate (0);
      arg = gimple_call_arg (stmt, flags & ERF_RETURN_ARG_MASK);
      get_constraint_for (arg, &rhsc);
      process_all_all_constraints (lhsc, rhsc);
      rhsc.truncate (0);
    }
  else if (flags & ERF_NOALIAS)
    {
      varinfo_t vi;
      struct constraint_expr tmpc;
      rhsc.truncate (0);
      vi = make_heapvar ("HEAP", true);
      /* We are marking allocated storage local, we deal with it becoming
	 global by escaping and setting of vars_contains_escaped_heap.  */
      DECL_EXTERNAL (vi->decl) = 0;
      vi->is_global_var = 0;
      /* If this is not a real malloc call assume the memory was
	 initialized and thus may point to global memory.  All
	 builtin functions with the malloc attribute behave in a sane way.  */
      if (!fndecl
	  || !fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
	make_constraint_from (vi, nonlocal_id);
      tmpc.var = vi->id;
      tmpc.offset = 0;
      tmpc.type = ADDRESSOF;
      rhsc.safe_push (tmpc);
      process_all_all_constraints (lhsc, rhsc);
      rhsc.truncate (0);
    }
  else
    process_all_all_constraints (lhsc, rhsc);
}


/* Create constraints for assigning call argument ARG to the incoming parameter
   INDEX of function FI.  */

static void
find_func_aliases_for_call_arg (varinfo_t fi, unsigned index, tree arg)
{
  struct constraint_expr lhs;
  lhs = get_function_part_constraint (fi, fi_parm_base + index);

  auto_vec<ce_s, 2> rhsc;
  get_constraint_for_rhs (arg, &rhsc);

  unsigned j;
  struct constraint_expr *rhsp;
  FOR_EACH_VEC_ELT (rhsc, j, rhsp)
    process_constraint (new_constraint (lhs, *rhsp));
}

/* Create constraints for the builtin call T.  Return true if the call
   was handled, otherwise false.  */

static bool
find_func_aliases_for_builtin_call (struct function *fn, gcall *t)
{
  tree fndecl = gimple_call_fndecl (t);
  auto_vec<ce_s, 2> lhsc;
  auto_vec<ce_s, 4> rhsc;
  varinfo_t fi;

  if (gimple_call_builtin_p (t, BUILT_IN_NORMAL))
    /* ???  All builtins that are handled here need to be handled
       in the alias-oracle query functions explicitly!  */
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      /* All the following functions return a pointer to the same object
	 as their first argument points to.  The functions do not add
	 to the ESCAPED solution.  The functions make the first argument
	 pointed to memory point to what the second argument pointed to
	 memory points to.  */
      case BUILT_IN_STRCPY:
      case BUILT_IN_STRNCPY:
      case BUILT_IN_BCOPY:
      case BUILT_IN_MEMCPY:
      case BUILT_IN_MEMMOVE:
      case BUILT_IN_MEMPCPY:
      case BUILT_IN_STPCPY:
      case BUILT_IN_STPNCPY:
      case BUILT_IN_STRCAT:
      case BUILT_IN_STRNCAT:
      case BUILT_IN_STRCPY_CHK:
      case BUILT_IN_STRNCPY_CHK:
      case BUILT_IN_MEMCPY_CHK:
      case BUILT_IN_MEMMOVE_CHK:
      case BUILT_IN_MEMPCPY_CHK:
      case BUILT_IN_STPCPY_CHK:
      case BUILT_IN_STPNCPY_CHK:
      case BUILT_IN_STRCAT_CHK:
      case BUILT_IN_STRNCAT_CHK:
      case BUILT_IN_TM_MEMCPY:
      case BUILT_IN_TM_MEMMOVE:
	{
	  tree res = gimple_call_lhs (t);
	  tree dest = gimple_call_arg (t, (DECL_FUNCTION_CODE (fndecl)
					   == BUILT_IN_BCOPY ? 1 : 0));
	  tree src = gimple_call_arg (t, (DECL_FUNCTION_CODE (fndecl)
					  == BUILT_IN_BCOPY ? 0 : 1));
	  if (res != NULL_TREE)
	    {
	      get_constraint_for (res, &lhsc);
	      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMPCPY
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STPCPY
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STPNCPY
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMPCPY_CHK
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STPCPY_CHK
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STPNCPY_CHK)
		get_constraint_for_ptr_offset (dest, NULL_TREE, &rhsc);
	      else
		get_constraint_for (dest, &rhsc);
	      process_all_all_constraints (lhsc, rhsc);
	      lhsc.truncate (0);
	      rhsc.truncate (0);
	    }
	  get_constraint_for_ptr_offset (dest, NULL_TREE, &lhsc);
	  get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	  do_deref (&lhsc);
	  do_deref (&rhsc);
	  process_all_all_constraints (lhsc, rhsc);
	  return true;
	}
      case BUILT_IN_MEMSET:
      case BUILT_IN_MEMSET_CHK:
      case BUILT_IN_TM_MEMSET:
	{
	  tree res = gimple_call_lhs (t);
	  tree dest = gimple_call_arg (t, 0);
	  unsigned i;
	  ce_s *lhsp;
	  struct constraint_expr ac;
	  if (res != NULL_TREE)
	    {
	      get_constraint_for (res, &lhsc);
	      get_constraint_for (dest, &rhsc);
	      process_all_all_constraints (lhsc, rhsc);
	      lhsc.truncate (0);
	    }
	  get_constraint_for_ptr_offset (dest, NULL_TREE, &lhsc);
	  do_deref (&lhsc);
	  if (flag_delete_null_pointer_checks
	      && integer_zerop (gimple_call_arg (t, 1)))
	    {
	      ac.type = ADDRESSOF;
	      ac.var = nothing_id;
	    }
	  else
	    {
	      ac.type = SCALAR;
	      ac.var = integer_id;
	    }
	  ac.offset = 0;
	  FOR_EACH_VEC_ELT (lhsc, i, lhsp)
	      process_constraint (new_constraint (*lhsp, ac));
	  return true;
	}
      case BUILT_IN_STACK_SAVE:
      case BUILT_IN_STACK_RESTORE:
	/* Nothing interesting happens.  */
	return true;
      case BUILT_IN_ALLOCA:
      case BUILT_IN_ALLOCA_WITH_ALIGN:
      case BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX:
	{
	  tree ptr = gimple_call_lhs (t);
	  if (ptr == NULL_TREE)
	    return true;
	  get_constraint_for (ptr, &lhsc);
	  varinfo_t vi = make_heapvar ("HEAP", true);
	  /* Alloca storage is never global.  To exempt it from escaped
	     handling make it a non-heap var.  */
	  DECL_EXTERNAL (vi->decl) = 0;
	  vi->is_global_var = 0;
	  vi->is_heap_var = 0;
	  struct constraint_expr tmpc;
	  tmpc.var = vi->id;
	  tmpc.offset = 0;
	  tmpc.type = ADDRESSOF;
	  rhsc.safe_push (tmpc);
	  process_all_all_constraints (lhsc, rhsc);
	  return true;
	}
      case BUILT_IN_POSIX_MEMALIGN:
	{
	  tree ptrptr = gimple_call_arg (t, 0);
	  get_constraint_for (ptrptr, &lhsc);
	  do_deref (&lhsc);
	  varinfo_t vi = make_heapvar ("HEAP", true);
	  /* We are marking allocated storage local, we deal with it becoming
	     global by escaping and setting of vars_contains_escaped_heap.  */
	  DECL_EXTERNAL (vi->decl) = 0;
	  vi->is_global_var = 0;
	  struct constraint_expr tmpc;
	  tmpc.var = vi->id;
	  tmpc.offset = 0;
	  tmpc.type = ADDRESSOF;
	  rhsc.safe_push (tmpc);
	  process_all_all_constraints (lhsc, rhsc);
	  return true;
	}
      case BUILT_IN_ASSUME_ALIGNED:
	{
	  tree res = gimple_call_lhs (t);
	  tree dest = gimple_call_arg (t, 0);
	  if (res != NULL_TREE)
	    {
	      get_constraint_for (res, &lhsc);
	      get_constraint_for (dest, &rhsc);
	      process_all_all_constraints (lhsc, rhsc);
	    }
	  return true;
	}
      /* All the following functions do not return pointers, do not
	 modify the points-to sets of memory reachable from their
	 arguments and do not add to the ESCAPED solution.  */
      case BUILT_IN_SINCOS:
      case BUILT_IN_SINCOSF:
      case BUILT_IN_SINCOSL:
      case BUILT_IN_FREXP:
      case BUILT_IN_FREXPF:
      case BUILT_IN_FREXPL:
      case BUILT_IN_GAMMA_R:
      case BUILT_IN_GAMMAF_R:
      case BUILT_IN_GAMMAL_R:
      case BUILT_IN_LGAMMA_R:
      case BUILT_IN_LGAMMAF_R:
      case BUILT_IN_LGAMMAL_R:
      case BUILT_IN_MODF:
      case BUILT_IN_MODFF:
      case BUILT_IN_MODFL:
      case BUILT_IN_REMQUO:
      case BUILT_IN_REMQUOF:
      case BUILT_IN_REMQUOL:
      case BUILT_IN_FREE:
	return true;
      case BUILT_IN_STRDUP:
      case BUILT_IN_STRNDUP:
      case BUILT_IN_REALLOC:
	if (gimple_call_lhs (t))
	  {
	    auto_vec<ce_s> rhsc;
	    handle_lhs_call (t, gimple_call_lhs (t),
			     gimple_call_return_flags (t) | ERF_NOALIAS,
			     rhsc, fndecl);
	    get_constraint_for_ptr_offset (gimple_call_lhs (t),
					   NULL_TREE, &lhsc);
	    get_constraint_for_ptr_offset (gimple_call_arg (t, 0),
					   NULL_TREE, &rhsc);
	    do_deref (&lhsc);
	    do_deref (&rhsc);
	    process_all_all_constraints (lhsc, rhsc);
	    lhsc.truncate (0);
	    rhsc.truncate (0);
	    /* For realloc the resulting pointer can be equal to the
	       argument as well.  But only doing this wouldn't be
	       correct because with ptr == 0 realloc behaves like malloc.  */
	    if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_REALLOC)
	      {
		get_constraint_for (gimple_call_lhs (t), &lhsc);
		get_constraint_for (gimple_call_arg (t, 0), &rhsc);
		process_all_all_constraints (lhsc, rhsc);
	      }
	    return true;
	  }
	break;
      /* String / character search functions return a pointer into the
	 source string or NULL.  */
      case BUILT_IN_INDEX:
      case BUILT_IN_STRCHR:
      case BUILT_IN_STRRCHR:
      case BUILT_IN_MEMCHR:
      case BUILT_IN_STRSTR:
      case BUILT_IN_STRPBRK:
	if (gimple_call_lhs (t))
	  {
	    tree src = gimple_call_arg (t, 0);
	    get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	    constraint_expr nul;
	    nul.var = nothing_id;
	    nul.offset = 0;
	    nul.type = ADDRESSOF;
	    rhsc.safe_push (nul);
	    get_constraint_for (gimple_call_lhs (t), &lhsc);
	    process_all_all_constraints (lhsc, rhsc);
	  }
	return true;
      /* Pure functions that return something not based on any object and
	 that use the memory pointed to by their arguments (but not
	 transitively).  */
      case BUILT_IN_STRCMP:
      case BUILT_IN_STRCMP_EQ:
      case BUILT_IN_STRNCMP:
      case BUILT_IN_STRNCMP_EQ:
      case BUILT_IN_STRCASECMP:
      case BUILT_IN_STRNCASECMP:
      case BUILT_IN_MEMCMP:
      case BUILT_IN_BCMP:
      case BUILT_IN_STRSPN:
      case BUILT_IN_STRCSPN:
	{
	  varinfo_t uses = get_call_use_vi (t);
	  make_any_offset_constraints (uses);
	  make_constraint_to (uses->id, gimple_call_arg (t, 0));
	  make_constraint_to (uses->id, gimple_call_arg (t, 1));
	  /* No constraints are necessary for the return value.  */
	  return true;
	}
      case BUILT_IN_STRLEN:
	{
	  varinfo_t uses = get_call_use_vi (t);
	  make_any_offset_constraints (uses);
	  make_constraint_to (uses->id, gimple_call_arg (t, 0));
	  /* No constraints are necessary for the return value.  */
	  return true;
	}
      case BUILT_IN_OBJECT_SIZE:
      case BUILT_IN_CONSTANT_P:
	{
	  /* No constraints are necessary for the return value or the
	     arguments.  */
	  return true;
	}
      /* Trampolines are special - they set up passing the static
	 frame.  */
      case BUILT_IN_INIT_TRAMPOLINE:
	{
	  tree tramp = gimple_call_arg (t, 0);
	  tree nfunc = gimple_call_arg (t, 1);
	  tree frame = gimple_call_arg (t, 2);
	  unsigned i;
	  struct constraint_expr lhs, *rhsp;
	  if (in_ipa_mode)
	    {
	      varinfo_t nfi = NULL;
	      gcc_assert (TREE_CODE (nfunc) == ADDR_EXPR);
	      nfi = lookup_vi_for_tree (TREE_OPERAND (nfunc, 0));
	      if (nfi)
		{
		  lhs = get_function_part_constraint (nfi, fi_static_chain);
		  get_constraint_for (frame, &rhsc);
		  FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		    process_constraint (new_constraint (lhs, *rhsp));
		  rhsc.truncate (0);

		  /* Make the frame point to the function for
		     the trampoline adjustment call.  */
		  get_constraint_for (tramp, &lhsc);
		  do_deref (&lhsc);
		  get_constraint_for (nfunc, &rhsc);
		  process_all_all_constraints (lhsc, rhsc);

		  return true;
		}
	    }
	  /* Else fallthru to generic handling which will let
	     the frame escape.  */
	  break;
	}
      case BUILT_IN_ADJUST_TRAMPOLINE:
	{
	  tree tramp = gimple_call_arg (t, 0);
	  tree res = gimple_call_lhs (t);
	  if (in_ipa_mode && res)
	    {
	      get_constraint_for (res, &lhsc);
	      get_constraint_for (tramp, &rhsc);
	      do_deref (&rhsc);
	      process_all_all_constraints (lhsc, rhsc);
	    }
	  return true;
	}
      CASE_BUILT_IN_TM_STORE (1):
      CASE_BUILT_IN_TM_STORE (2):
      CASE_BUILT_IN_TM_STORE (4):
      CASE_BUILT_IN_TM_STORE (8):
      CASE_BUILT_IN_TM_STORE (FLOAT):
      CASE_BUILT_IN_TM_STORE (DOUBLE):
      CASE_BUILT_IN_TM_STORE (LDOUBLE):
      CASE_BUILT_IN_TM_STORE (M64):
      CASE_BUILT_IN_TM_STORE (M128):
      CASE_BUILT_IN_TM_STORE (M256):
	{
	  tree addr = gimple_call_arg (t, 0);
	  tree src = gimple_call_arg (t, 1);

	  get_constraint_for (addr, &lhsc);
	  do_deref (&lhsc);
	  get_constraint_for (src, &rhsc);
	  process_all_all_constraints (lhsc, rhsc);
	  return true;
	}
      CASE_BUILT_IN_TM_LOAD (1):
      CASE_BUILT_IN_TM_LOAD (2):
      CASE_BUILT_IN_TM_LOAD (4):
      CASE_BUILT_IN_TM_LOAD (8):
      CASE_BUILT_IN_TM_LOAD (FLOAT):
      CASE_BUILT_IN_TM_LOAD (DOUBLE):
      CASE_BUILT_IN_TM_LOAD (LDOUBLE):
      CASE_BUILT_IN_TM_LOAD (M64):
      CASE_BUILT_IN_TM_LOAD (M128):
      CASE_BUILT_IN_TM_LOAD (M256):
	{
	  tree dest = gimple_call_lhs (t);
	  tree addr = gimple_call_arg (t, 0);

	  get_constraint_for (dest, &lhsc);
	  get_constraint_for (addr, &rhsc);
	  do_deref (&rhsc);
	  process_all_all_constraints (lhsc, rhsc);
	  return true;
	}
      /* Variadic argument handling needs to be handled in IPA
	 mode as well.  */
      case BUILT_IN_VA_START:
	{
	  tree valist = gimple_call_arg (t, 0);
	  struct constraint_expr rhs, *lhsp;
	  unsigned i;
	  get_constraint_for_ptr_offset (valist, NULL_TREE, &lhsc);
	  do_deref (&lhsc);
	  /* The va_list gets access to pointers in variadic
	     arguments.  Which we know in the case of IPA analysis
	     and otherwise are just all nonlocal variables.  */
	  if (in_ipa_mode)
	    {
	      fi = lookup_vi_for_tree (fn->decl);
	      rhs = get_function_part_constraint (fi, ~0);
	      rhs.type = ADDRESSOF;
	    }
	  else
	    {
	      rhs.var = nonlocal_id;
	      rhs.type = ADDRESSOF;
	      rhs.offset = 0;
	    }
	  FOR_EACH_VEC_ELT (lhsc, i, lhsp)
	    process_constraint (new_constraint (*lhsp, rhs));
	  /* va_list is clobbered.  */
	  make_constraint_to (get_call_clobber_vi (t)->id, valist);
	  return true;
	}
      /* va_end doesn't have any effect that matters.  */
      case BUILT_IN_VA_END:
	return true;
      /* Alternate return.  Simply give up for now.  */
      case BUILT_IN_RETURN:
	{
	  fi = NULL;
	  if (!in_ipa_mode
	      || !(fi = get_vi_for_tree (fn->decl)))
	    make_constraint_from (get_varinfo (escaped_id), anything_id);
	  else if (in_ipa_mode
		   && fi != NULL)
	    {
	      struct constraint_expr lhs, rhs;
	      lhs = get_function_part_constraint (fi, fi_result);
	      rhs.var = anything_id;
	      rhs.offset = 0;
	      rhs.type = SCALAR;
	      process_constraint (new_constraint (lhs, rhs));
	    }
	  return true;
	}
      case BUILT_IN_GOMP_PARALLEL:
      case BUILT_IN_GOACC_PARALLEL:
	{
	  if (in_ipa_mode)
	    {
	      unsigned int fnpos, argpos;
	      switch (DECL_FUNCTION_CODE (fndecl))
		{
		case BUILT_IN_GOMP_PARALLEL:
		  /* __builtin_GOMP_parallel (fn, data, num_threads, flags).  */
		  fnpos = 0;
		  argpos = 1;
		  break;
		case BUILT_IN_GOACC_PARALLEL:
		  /* __builtin_GOACC_parallel (flags_m, fn, mapnum, hostaddrs,
					       sizes, kinds, ...).  */
		  fnpos = 1;
		  argpos = 3;
		  break;
		default:
		  gcc_unreachable ();
		}

	      tree fnarg = gimple_call_arg (t, fnpos);
	      gcc_assert (TREE_CODE (fnarg) == ADDR_EXPR);
	      tree fndecl = TREE_OPERAND (fnarg, 0);
	      if (fndecl_maybe_in_other_partition (fndecl))
		/* Fallthru to general call handling.  */
		break;

	      tree arg = gimple_call_arg (t, argpos);

	      varinfo_t fi = get_vi_for_tree (fndecl);
	      find_func_aliases_for_call_arg (fi, 0, arg);
	      return true;
	    }
	  /* Else fallthru to generic call handling.  */
	  break;
	}
      /* printf-style functions may have hooks to set pointers to
	 point to somewhere into the generated string.  Leave them
	 for a later exercise...  */
      default:
	/* Fallthru to general call handling.  */;
      }

  return false;
}

/* Create constraints for the call T.  */

static void
find_func_aliases_for_call (struct function *fn, gcall *t)
{
  tree fndecl = gimple_call_fndecl (t);
  varinfo_t fi;

  if (fndecl != NULL_TREE
      && fndecl_built_in_p (fndecl)
      && find_func_aliases_for_builtin_call (fn, t))
    return;

  if (gimple_call_internal_p (t, IFN_DEFERRED_INIT))
    return;

  fi = get_fi_for_callee (t);
  if (!in_ipa_mode
      || (fi->decl && fndecl && !fi->is_fn_info))
    {
      auto_vec<ce_s, 16> rhsc;
      int flags = gimple_call_flags (t);

      /* Const functions can return their arguments and addresses
	 of global memory but not of escaped memory.  */
      if (flags & (ECF_CONST|ECF_NOVOPS))
	{
	  if (gimple_call_lhs (t))
	    handle_rhs_call (t, &rhsc, implicit_const_eaf_flags, false, false);
	}
      /* Pure functions can return addresses in and of memory
	 reachable from their arguments, but they are not an escape
	 point for reachable memory of their arguments.  */
      else if (flags & (ECF_PURE|ECF_LOOPING_CONST_OR_PURE))
	handle_rhs_call (t, &rhsc, implicit_pure_eaf_flags, false, true);
      /* If the call is to a replaceable operator delete and results
	 from a delete expression as opposed to a direct call to
	 such operator, then the effects for PTA (in particular
	 the escaping of the pointer) can be ignored.  */
      else if (fndecl
	       && DECL_IS_OPERATOR_DELETE_P (fndecl)
	       && gimple_call_from_new_or_delete (t))
	;
      else
	handle_rhs_call (t, &rhsc, 0, true, true);
      if (gimple_call_lhs (t))
	handle_lhs_call (t, gimple_call_lhs (t),
			 gimple_call_return_flags (t), rhsc, fndecl);
    }
  else
    {
      auto_vec<ce_s, 2> rhsc;
      tree lhsop;
      unsigned j;

      /* Assign all the passed arguments to the appropriate incoming
	 parameters of the function.  */
      for (j = 0; j < gimple_call_num_args (t); j++)
	{
	  tree arg = gimple_call_arg (t, j);
	  find_func_aliases_for_call_arg (fi, j, arg);
	}

      /* If we are returning a value, assign it to the result.  */
      lhsop = gimple_call_lhs (t);
      if (lhsop)
	{
	  auto_vec<ce_s, 2> lhsc;
	  struct constraint_expr rhs;
	  struct constraint_expr *lhsp;
	  bool aggr_p = aggregate_value_p (lhsop, gimple_call_fntype (t));

	  get_constraint_for (lhsop, &lhsc);
	  rhs = get_function_part_constraint (fi, fi_result);
	  if (aggr_p)
	    {
	      auto_vec<ce_s, 2> tem;
	      tem.quick_push (rhs);
	      do_deref (&tem);
	      gcc_checking_assert (tem.length () == 1);
	      rhs = tem[0];
	    }
	  FOR_EACH_VEC_ELT (lhsc, j, lhsp)
	    process_constraint (new_constraint (*lhsp, rhs));

	  /* If we pass the result decl by reference, honor that.  */
	  if (aggr_p)
	    {
	      struct constraint_expr lhs;
	      struct constraint_expr *rhsp;

	      get_constraint_for_address_of (lhsop, &rhsc);
	      lhs = get_function_part_constraint (fi, fi_result);
	      FOR_EACH_VEC_ELT (rhsc, j, rhsp)
		  process_constraint (new_constraint (lhs, *rhsp));
	      rhsc.truncate (0);
	    }
	}

      /* If we use a static chain, pass it along.  */
      if (gimple_call_chain (t))
	{
	  struct constraint_expr lhs;
	  struct constraint_expr *rhsp;

	  get_constraint_for (gimple_call_chain (t), &rhsc);
	  lhs = get_function_part_constraint (fi, fi_static_chain);
	  FOR_EACH_VEC_ELT (rhsc, j, rhsp)
	    process_constraint (new_constraint (lhs, *rhsp));
	}
    }
}

/* Walk statement T setting up aliasing constraints according to the
   references found in T.  This function is the main part of the
   constraint builder.  AI points to auxiliary alias information used
   when building alias sets and computing alias grouping heuristics.  */

static void
find_func_aliases (struct function *fn, gimple *origt)
{
  gimple *t = origt;
  auto_vec<ce_s, 16> lhsc;
  auto_vec<ce_s, 16> rhsc;
  varinfo_t fi;

  /* Now build constraints expressions.  */
  if (gimple_code (t) == GIMPLE_PHI)
    {
      /* For a phi node, assign all the arguments to
	 the result.  */
      get_constraint_for (gimple_phi_result (t), &lhsc);
      for (unsigned i = 0; i < gimple_phi_num_args (t); i++)
	{
	  get_constraint_for_rhs (gimple_phi_arg_def (t, i), &rhsc);
	  process_all_all_constraints (lhsc, rhsc);
	  rhsc.truncate (0);
	}
    }
  /* In IPA mode, we need to generate constraints to pass call
     arguments through their calls.  There are two cases,
     either a GIMPLE_CALL returning a value, or just a plain
     GIMPLE_CALL when we are not.

     In non-ipa mode, we need to generate constraints for each
     pointer passed by address.  */
  else if (is_gimple_call (t))
    find_func_aliases_for_call (fn, as_a <gcall *> (t));

  /* Otherwise, just a regular assignment statement.  Only care about
     operations with pointer result, others are dealt with as escape
     points if they have pointer operands.  */
  else if (is_gimple_assign (t))
    {
      /* Otherwise, just a regular assignment statement.  */
      tree lhsop = gimple_assign_lhs (t);
      tree rhsop = (gimple_num_ops (t) == 2) ? gimple_assign_rhs1 (t) : NULL;

      if (rhsop && TREE_CLOBBER_P (rhsop))
	/* Ignore clobbers, they don't actually store anything into
	   the LHS.  */
	;
      else if (rhsop && AGGREGATE_TYPE_P (TREE_TYPE (lhsop)))
	do_structure_copy (lhsop, rhsop);
      else
	{
	  enum tree_code code = gimple_assign_rhs_code (t);

	  get_constraint_for (lhsop, &lhsc);

	  if (code == POINTER_PLUS_EXPR)
	    get_constraint_for_ptr_offset (gimple_assign_rhs1 (t),
					   gimple_assign_rhs2 (t), &rhsc);
	  else if (code == POINTER_DIFF_EXPR)
	    /* The result is not a pointer (part).  */
	    ;
	  else if (code == BIT_AND_EXPR
		   && TREE_CODE (gimple_assign_rhs2 (t)) == INTEGER_CST)
	    {
	      /* Aligning a pointer via a BIT_AND_EXPR is offsetting
		 the pointer.  Handle it by offsetting it by UNKNOWN.  */
	      get_constraint_for_ptr_offset (gimple_assign_rhs1 (t),
					     NULL_TREE, &rhsc);
	    }
	  else if (code == TRUNC_DIV_EXPR
		   || code == CEIL_DIV_EXPR
		   || code == FLOOR_DIV_EXPR
		   || code == ROUND_DIV_EXPR
		   || code == EXACT_DIV_EXPR
		   || code == TRUNC_MOD_EXPR
		   || code == CEIL_MOD_EXPR
		   || code == FLOOR_MOD_EXPR
		   || code == ROUND_MOD_EXPR)
	    /* Division and modulo transfer the pointer from the LHS.  */
	    get_constraint_for_ptr_offset (gimple_assign_rhs1 (t),
					   NULL_TREE, &rhsc);
	  else if (CONVERT_EXPR_CODE_P (code)
		   || gimple_assign_single_p (t))
	    /* See through conversions, single RHS are handled by
	       get_constraint_for_rhs.  */
	    get_constraint_for_rhs (rhsop, &rhsc);
	  else if (code == COND_EXPR)
	    {
	      /* The result is a merge of both COND_EXPR arms.  */
	      auto_vec<ce_s, 2> tmp;
	      struct constraint_expr *rhsp;
	      unsigned i;
	      get_constraint_for_rhs (gimple_assign_rhs2 (t), &rhsc);
	      get_constraint_for_rhs (gimple_assign_rhs3 (t), &tmp);
	      FOR_EACH_VEC_ELT (tmp, i, rhsp)
		rhsc.safe_push (*rhsp);
	    }
	  else if (truth_value_p (code))
	    /* Truth value results are not pointer (parts).  Or at least
	       very unreasonable obfuscation of a part.  */
	    ;
	  else
	    {
	      /* All other operations are possibly offsetting merges.  */
	      auto_vec<ce_s, 4> tmp;
	      struct constraint_expr *rhsp;
	      unsigned i, j;
	      get_constraint_for_ptr_offset (gimple_assign_rhs1 (t),
					     NULL_TREE, &rhsc);
	      for (i = 2; i < gimple_num_ops (t); ++i)
		{
		  get_constraint_for_ptr_offset (gimple_op (t, i),
						 NULL_TREE, &tmp);
		  FOR_EACH_VEC_ELT (tmp, j, rhsp)
		    rhsc.safe_push (*rhsp);
		  tmp.truncate (0);
		}
	    }
	  process_all_all_constraints (lhsc, rhsc);
	}
      /* If there is a store to a global variable the rhs escapes.  */
      if ((lhsop = get_base_address (lhsop)) != NULL_TREE
	  && DECL_P (lhsop))
	{
	  varinfo_t vi = get_vi_for_tree (lhsop);
	  if ((! in_ipa_mode && vi->is_global_var)
	      || vi->is_ipa_escape_point)
	    make_escape_constraint (rhsop);
	}
    }
  /* Handle escapes through return.  */
  else if (gimple_code (t) == GIMPLE_RETURN
	   && gimple_return_retval (as_a <greturn *> (t)) != NULL_TREE)
    {
      greturn *return_stmt = as_a <greturn *> (t);
      tree retval = gimple_return_retval (return_stmt);
      if (!in_ipa_mode)
	make_constraint_to (escaped_return_id, retval);
      else
	{
	  struct constraint_expr lhs ;
	  struct constraint_expr *rhsp;
	  unsigned i;

	  fi = lookup_vi_for_tree (fn->decl);
	  lhs = get_function_part_constraint (fi, fi_result);
	  get_constraint_for_rhs (retval, &rhsc);
	  FOR_EACH_VEC_ELT (rhsc, i, rhsp)
	    process_constraint (new_constraint (lhs, *rhsp));
	}
    }
  /* Handle asms conservatively by adding escape constraints to everything.  */
  else if (gasm *asm_stmt = dyn_cast <gasm *> (t))
    {
      unsigned i, noutputs;
      const char **oconstraints;
      const char *constraint;
      bool allows_mem, allows_reg, is_inout;

      noutputs = gimple_asm_noutputs (asm_stmt);
      oconstraints = XALLOCAVEC (const char *, noutputs);

      for (i = 0; i < noutputs; ++i)
	{
	  tree link = gimple_asm_output_op (asm_stmt, i);
	  tree op = TREE_VALUE (link);

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
	  oconstraints[i] = constraint;
	  parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
				   &allows_reg, &is_inout, nullptr);

	  /* A memory constraint makes the address of the operand escape.  */
	  if (!allows_reg && allows_mem)
	    {
	      auto_vec<ce_s> tmpc;
	      get_constraint_for_address_of (op, &tmpc);
	      make_constraints_to (escaped_id, tmpc);
	    }

	  /* The asm may read global memory, so outputs may point to
	     any global memory.  */
	  if (op)
	    {
	      auto_vec<ce_s, 2> lhsc;
	      struct constraint_expr rhsc, *lhsp;
	      unsigned j;
	      get_constraint_for (op, &lhsc);
	      rhsc.var = nonlocal_id;
	      rhsc.offset = 0;
	      rhsc.type = SCALAR;
	      FOR_EACH_VEC_ELT (lhsc, j, lhsp)
		process_constraint (new_constraint (*lhsp, rhsc));
	    }
	}
      for (i = 0; i < gimple_asm_ninputs (asm_stmt); ++i)
	{
	  tree link = gimple_asm_input_op (asm_stmt, i);
	  tree op = TREE_VALUE (link);

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));

	  parse_input_constraint (&constraint, 0, 0, noutputs, 0, oconstraints,
				  &allows_mem, &allows_reg, nullptr);

	  /* A memory constraint makes the address of the operand escape.  */
	  if (!allows_reg && allows_mem)
	    {
	      auto_vec<ce_s> tmpc;
	      get_constraint_for_address_of (op, &tmpc);
	      make_constraints_to (escaped_id, tmpc);
	    }
	  /* Strictly we'd only need the constraint to ESCAPED if
	     the asm clobbers memory, otherwise using something
	     along the lines of per-call clobbers/uses would be enough.  */
	  else if (op)
	    make_escape_constraint (op);
	}
    }
}


/* Create a constraint adding to the clobber set of FI the memory
   pointed to by PTR.  */

static void
process_ipa_clobber (varinfo_t fi, tree ptr)
{
  vec<ce_s> ptrc = vNULL;
  struct constraint_expr *c, lhs;
  unsigned i;
  get_constraint_for_rhs (ptr, &ptrc);
  lhs = get_function_part_constraint (fi, fi_clobbers);
  FOR_EACH_VEC_ELT (ptrc, i, c)
    process_constraint (new_constraint (lhs, *c));
  ptrc.release ();
}

/* Walk statement T setting up clobber and use constraints according to the
   references found in T.  This function is a main part of the
   IPA constraint builder.  */

static void
find_func_clobbers (struct function *fn, gimple *origt)
{
  gimple *t = origt;
  auto_vec<ce_s, 16> lhsc;
  auto_vec<ce_s, 16> rhsc;
  varinfo_t fi;

  /* Add constraints for clobbered/used in IPA mode.
     We are not interested in what automatic variables are clobbered
     or used as we only use the information in the caller to which
     they do not escape.  */
  gcc_assert (in_ipa_mode);

  /* If the stmt refers to memory in any way it better had a VUSE.  */
  if (gimple_vuse (t) == NULL_TREE)
    return;

  /* We'd better have function information for the current function.  */
  fi = lookup_vi_for_tree (fn->decl);
  gcc_assert (fi != NULL);

  /* Account for stores in assignments and calls.  */
  if (gimple_vdef (t) != NULL_TREE
      && gimple_has_lhs (t))
    {
      tree lhs = gimple_get_lhs (t);
      tree tem = lhs;
      while (handled_component_p (tem))
	tem = TREE_OPERAND (tem, 0);
      if ((DECL_P (tem)
	   && !auto_var_in_fn_p (tem, fn->decl))
	  || INDIRECT_REF_P (tem)
	  || (TREE_CODE (tem) == MEM_REF
	      && !(TREE_CODE (TREE_OPERAND (tem, 0)) == ADDR_EXPR
		   && auto_var_in_fn_p
			(TREE_OPERAND (TREE_OPERAND (tem, 0), 0), fn->decl))))
	{
	  struct constraint_expr lhsc, *rhsp;
	  unsigned i;
	  lhsc = get_function_part_constraint (fi, fi_clobbers);
	  get_constraint_for_address_of (lhs, &rhsc);
	  FOR_EACH_VEC_ELT (rhsc, i, rhsp)
	    process_constraint (new_constraint (lhsc, *rhsp));
	  rhsc.truncate (0);
	}
    }

  /* Account for uses in assigments and returns.  */
  if (gimple_assign_single_p (t)
      || (gimple_code (t) == GIMPLE_RETURN
	  && gimple_return_retval (as_a <greturn *> (t)) != NULL_TREE))
    {
      tree rhs = (gimple_assign_single_p (t)
		  ? gimple_assign_rhs1 (t)
		  : gimple_return_retval (as_a <greturn *> (t)));
      tree tem = rhs;
      while (handled_component_p (tem))
	tem = TREE_OPERAND (tem, 0);
      if ((DECL_P (tem)
	   && !auto_var_in_fn_p (tem, fn->decl))
	  || INDIRECT_REF_P (tem)
	  || (TREE_CODE (tem) == MEM_REF
	      && !(TREE_CODE (TREE_OPERAND (tem, 0)) == ADDR_EXPR
		   && auto_var_in_fn_p
			(TREE_OPERAND (TREE_OPERAND (tem, 0), 0), fn->decl))))
	{
	  struct constraint_expr lhs, *rhsp;
	  unsigned i;
	  lhs = get_function_part_constraint (fi, fi_uses);
	  get_constraint_for_address_of (rhs, &rhsc);
	  FOR_EACH_VEC_ELT (rhsc, i, rhsp)
	    process_constraint (new_constraint (lhs, *rhsp));
	  rhsc.truncate (0);
	}
    }

  if (gcall *call_stmt = dyn_cast <gcall *> (t))
    {
      varinfo_t cfi = NULL;
      tree decl = gimple_call_fndecl (t);
      struct constraint_expr lhs, rhs;
      unsigned i, j;

      /* For builtins we do not have separate function info.  For those
	 we do not generate escapes for we have to generate clobbers/uses.  */
      if (gimple_call_builtin_p (t, BUILT_IN_NORMAL))
	switch (DECL_FUNCTION_CODE (decl))
	  {
	  /* The following functions use and clobber memory pointed to
	     by their arguments.  */
	  case BUILT_IN_STRCPY:
	  case BUILT_IN_STRNCPY:
	  case BUILT_IN_BCOPY:
	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMMOVE:
	  case BUILT_IN_MEMPCPY:
	  case BUILT_IN_STPCPY:
	  case BUILT_IN_STPNCPY:
	  case BUILT_IN_STRCAT:
	  case BUILT_IN_STRNCAT:
	  case BUILT_IN_STRCPY_CHK:
	  case BUILT_IN_STRNCPY_CHK:
	  case BUILT_IN_MEMCPY_CHK:
	  case BUILT_IN_MEMMOVE_CHK:
	  case BUILT_IN_MEMPCPY_CHK:
	  case BUILT_IN_STPCPY_CHK:
	  case BUILT_IN_STPNCPY_CHK:
	  case BUILT_IN_STRCAT_CHK:
	  case BUILT_IN_STRNCAT_CHK:
	    {
	      tree dest = gimple_call_arg (t, (DECL_FUNCTION_CODE (decl)
					       == BUILT_IN_BCOPY ? 1 : 0));
	      tree src = gimple_call_arg (t, (DECL_FUNCTION_CODE (decl)
					      == BUILT_IN_BCOPY ? 0 : 1));
	      unsigned i;
	      struct constraint_expr *rhsp, *lhsp;
	      get_constraint_for_ptr_offset (dest, NULL_TREE, &lhsc);
	      lhs = get_function_part_constraint (fi, fi_clobbers);
	      FOR_EACH_VEC_ELT (lhsc, i, lhsp)
		process_constraint (new_constraint (lhs, *lhsp));
	      get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	      lhs = get_function_part_constraint (fi, fi_uses);
	      FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		process_constraint (new_constraint (lhs, *rhsp));
	      return;
	    }
	  /* The following function clobbers memory pointed to by
	     its argument.  */
	  case BUILT_IN_MEMSET:
	  case BUILT_IN_MEMSET_CHK:
	  case BUILT_IN_POSIX_MEMALIGN:
	    {
	      tree dest = gimple_call_arg (t, 0);
	      unsigned i;
	      ce_s *lhsp;
	      get_constraint_for_ptr_offset (dest, NULL_TREE, &lhsc);
	      lhs = get_function_part_constraint (fi, fi_clobbers);
	      FOR_EACH_VEC_ELT (lhsc, i, lhsp)
		process_constraint (new_constraint (lhs, *lhsp));
	      return;
	    }
	  /* The following functions clobber their second and third
	     arguments.  */
	  case BUILT_IN_SINCOS:
	  case BUILT_IN_SINCOSF:
	  case BUILT_IN_SINCOSL:
	    {
	      process_ipa_clobber (fi, gimple_call_arg (t, 1));
	      process_ipa_clobber (fi, gimple_call_arg (t, 2));
	      return;
	    }
	  /* The following functions clobber their second argument.  */
	  case BUILT_IN_FREXP:
	  case BUILT_IN_FREXPF:
	  case BUILT_IN_FREXPL:
	  case BUILT_IN_LGAMMA_R:
	  case BUILT_IN_LGAMMAF_R:
	  case BUILT_IN_LGAMMAL_R:
	  case BUILT_IN_GAMMA_R:
	  case BUILT_IN_GAMMAF_R:
	  case BUILT_IN_GAMMAL_R:
	  case BUILT_IN_MODF:
	  case BUILT_IN_MODFF:
	  case BUILT_IN_MODFL:
	    {
	      process_ipa_clobber (fi, gimple_call_arg (t, 1));
	      return;
	    }
	  /* The following functions clobber their third argument.  */
	  case BUILT_IN_REMQUO:
	  case BUILT_IN_REMQUOF:
	  case BUILT_IN_REMQUOL:
	    {
	      process_ipa_clobber (fi, gimple_call_arg (t, 2));
	      return;
	    }
	  /* The following functions use what their first argument
	     points to.  */
	  case BUILT_IN_STRDUP:
	  case BUILT_IN_STRNDUP:
	  case BUILT_IN_REALLOC:
	  case BUILT_IN_INDEX:
	  case BUILT_IN_STRCHR:
	  case BUILT_IN_STRRCHR:
	  case BUILT_IN_MEMCHR:
	    {
	      tree src = gimple_call_arg (t, 0);
	      get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	      lhs = get_function_part_constraint (fi, fi_uses);
	      struct constraint_expr *rhsp;
	      FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		process_constraint (new_constraint (lhs, *rhsp));
	      return;
	    }
	  /* The following functions use what their first and second argument
	     point to.  */
	  case BUILT_IN_STRSTR:
	  case BUILT_IN_STRPBRK:
	    {
	      tree src = gimple_call_arg (t, 0);
	      get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	      lhs = get_function_part_constraint (fi, fi_uses);
	      struct constraint_expr *rhsp;
	      FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		process_constraint (new_constraint (lhs, *rhsp));
	      rhsc.truncate (0);
	      src = gimple_call_arg (t, 1);
	      get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	      FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		process_constraint (new_constraint (lhs, *rhsp));
	      return;
	    }
	  /* The following functions neither read nor clobber memory.  */
	  case BUILT_IN_ASSUME_ALIGNED:
	  case BUILT_IN_FREE:
	    return;
	  /* Trampolines are of no interest to us.  */
	  case BUILT_IN_INIT_TRAMPOLINE:
	  case BUILT_IN_ADJUST_TRAMPOLINE:
	    return;
	  case BUILT_IN_VA_START:
	  case BUILT_IN_VA_END:
	    return;
	  case BUILT_IN_GOMP_PARALLEL:
	  case BUILT_IN_GOACC_PARALLEL:
	    {
	      unsigned int fnpos, argpos;
	      unsigned int implicit_use_args[2];
	      unsigned int num_implicit_use_args = 0;
	      switch (DECL_FUNCTION_CODE (decl))
		{
		case BUILT_IN_GOMP_PARALLEL:
		  /* __builtin_GOMP_parallel (fn, data, num_threads, flags).  */
		  fnpos = 0;
		  argpos = 1;
		  break;
		case BUILT_IN_GOACC_PARALLEL:
		  /* __builtin_GOACC_parallel (flags_m, fn, mapnum, hostaddrs,
					       sizes, kinds, ...).  */
		  fnpos = 1;
		  argpos = 3;
		  implicit_use_args[num_implicit_use_args++] = 4;
		  implicit_use_args[num_implicit_use_args++] = 5;
		  break;
		default:
		  gcc_unreachable ();
		}

	      tree fnarg = gimple_call_arg (t, fnpos);
	      gcc_assert (TREE_CODE (fnarg) == ADDR_EXPR);
	      tree fndecl = TREE_OPERAND (fnarg, 0);
	      if (fndecl_maybe_in_other_partition (fndecl))
		/* Fallthru to general call handling.  */
		break;

	      varinfo_t cfi = get_vi_for_tree (fndecl);

	      tree arg = gimple_call_arg (t, argpos);

	      /* Parameter passed by value is used.  */
	      lhs = get_function_part_constraint (fi, fi_uses);
	      struct constraint_expr *rhsp;
	      get_constraint_for (arg, &rhsc);
	      FOR_EACH_VEC_ELT (rhsc, j, rhsp)
		process_constraint (new_constraint (lhs, *rhsp));
	      rhsc.truncate (0);

	      /* Handle parameters used by the call, but not used in cfi, as
		 implicitly used by cfi.  */
	      lhs = get_function_part_constraint (cfi, fi_uses);
	      for (unsigned i = 0; i < num_implicit_use_args; ++i)
		{
		  tree arg = gimple_call_arg (t, implicit_use_args[i]);
		  get_constraint_for (arg, &rhsc);
		  FOR_EACH_VEC_ELT (rhsc, j, rhsp)
		    process_constraint (new_constraint (lhs, *rhsp));
		  rhsc.truncate (0);
		}

	      /* The caller clobbers what the callee does.  */
	      lhs = get_function_part_constraint (fi, fi_clobbers);
	      rhs = get_function_part_constraint (cfi, fi_clobbers);
	      process_constraint (new_constraint (lhs, rhs));

	      /* The caller uses what the callee does.  */
	      lhs = get_function_part_constraint (fi, fi_uses);
	      rhs = get_function_part_constraint (cfi, fi_uses);
	      process_constraint (new_constraint (lhs, rhs));

	      return;
	    }
	  /* printf-style functions may have hooks to set pointers to
	     point to somewhere into the generated string.  Leave them
	     for a later exercise...  */
	  default:
	    /* Fallthru to general call handling.  */;
	  }

      /* Parameters passed by value are used.  */
      lhs = get_function_part_constraint (fi, fi_uses);
      for (i = 0; i < gimple_call_num_args (t); i++)
	{
	  struct constraint_expr *rhsp;
	  tree arg = gimple_call_arg (t, i);

	  if (TREE_CODE (arg) == SSA_NAME
	      || is_gimple_min_invariant (arg))
	    continue;

	  get_constraint_for_address_of (arg, &rhsc);
	  FOR_EACH_VEC_ELT (rhsc, j, rhsp)
	    process_constraint (new_constraint (lhs, *rhsp));
	  rhsc.truncate (0);
	}

      /* Build constraints for propagating clobbers/uses along the
	 callgraph edges.  */
      cfi = get_fi_for_callee (call_stmt);
      if (cfi->id == anything_id)
	{
	  if (gimple_vdef (t))
	    make_constraint_from (first_vi_for_offset (fi, fi_clobbers),
				  anything_id);
	  make_constraint_from (first_vi_for_offset (fi, fi_uses),
				anything_id);
	  return;
	}

      /* For callees without function info (that's external functions),
	 ESCAPED is clobbered and used.  */
      if (cfi->decl
	  && TREE_CODE (cfi->decl) == FUNCTION_DECL
	  && !cfi->is_fn_info)
	{
	  varinfo_t vi;

	  if (gimple_vdef (t))
	    make_copy_constraint (first_vi_for_offset (fi, fi_clobbers),
				  escaped_id);
	  make_copy_constraint (first_vi_for_offset (fi, fi_uses), escaped_id);

	  /* Also honor the call statement use/clobber info.  */
	  if ((vi = lookup_call_clobber_vi (call_stmt)) != NULL)
	    make_copy_constraint (first_vi_for_offset (fi, fi_clobbers),
				  vi->id);
	  if ((vi = lookup_call_use_vi (call_stmt)) != NULL)
	    make_copy_constraint (first_vi_for_offset (fi, fi_uses),
				  vi->id);
	  return;
	}

      /* Otherwise the caller clobbers and uses what the callee does.
	 ???  This should use a new complex constraint that filters
	 local variables of the callee.  */
      if (gimple_vdef (t))
	{
	  lhs = get_function_part_constraint (fi, fi_clobbers);
	  rhs = get_function_part_constraint (cfi, fi_clobbers);
	  process_constraint (new_constraint (lhs, rhs));
	}
      lhs = get_function_part_constraint (fi, fi_uses);
      rhs = get_function_part_constraint (cfi, fi_uses);
      process_constraint (new_constraint (lhs, rhs));
    }
  else if (gimple_code (t) == GIMPLE_ASM)
    {
      /* ???  Ick.  We can do better.  */
      if (gimple_vdef (t))
	make_constraint_from (first_vi_for_offset (fi, fi_clobbers),
			      anything_id);
      make_constraint_from (first_vi_for_offset (fi, fi_uses),
			    anything_id);
    }
}


/* This structure is used during pushing fields onto the fieldstack
   to track the offset of the field, since bitpos_of_field gives it
   relative to its immediate containing type, and we want it relative
   to the ultimate containing object.  */

struct fieldoff
{
  /* Offset from the base of the base containing object to this field.  */
  HOST_WIDE_INT offset;

  /* Size, in bits, of the field.  */
  unsigned HOST_WIDE_INT size;

  unsigned has_unknown_size : 1;

  unsigned must_have_pointers : 1;

  unsigned may_have_pointers : 1;

  unsigned only_restrict_pointers : 1;

  tree restrict_pointed_type;
};
typedef struct fieldoff fieldoff_s;


/* qsort comparison function for two fieldoff's PA and PB.  */

static int
fieldoff_compare (const void *pa, const void *pb)
{
  const fieldoff_s *foa = (const fieldoff_s *)pa;
  const fieldoff_s *fob = (const fieldoff_s *)pb;
  unsigned HOST_WIDE_INT foasize, fobsize;

  if (foa->offset < fob->offset)
    return -1;
  else if (foa->offset > fob->offset)
    return 1;

  foasize = foa->size;
  fobsize = fob->size;
  if (foasize < fobsize)
    return -1;
  else if (foasize > fobsize)
    return 1;
  return 0;
}

/* Sort a fieldstack according to the field offset and sizes.  */
static void
sort_fieldstack (vec<fieldoff_s> &fieldstack)
{
  fieldstack.qsort (fieldoff_compare);
}

/* Return true if T is a type that can have subvars.  */

static inline bool
type_can_have_subvars (const_tree t)
{
  /* Aggregates without overlapping fields can have subvars.  */
  return TREE_CODE (t) == RECORD_TYPE;
}

/* Return true if V is a tree that we can have subvars for.
   Normally, this is any aggregate type.  Also complex
   types which are not gimple registers can have subvars.  */

static inline bool
var_can_have_subvars (const_tree v)
{
  /* Volatile variables should never have subvars.  */
  if (TREE_THIS_VOLATILE (v))
    return false;

  /* Non decls or memory tags can never have subvars.  */
  if (!DECL_P (v))
    return false;

  return type_can_have_subvars (TREE_TYPE (v));
}

/* Return true if T is a type that does contain pointers.  */

static bool
type_must_have_pointers (tree type)
{
  if (POINTER_TYPE_P (type))
    return true;

  if (TREE_CODE (type) == ARRAY_TYPE)
    return type_must_have_pointers (TREE_TYPE (type));

  /* A function or method can have pointers as arguments, so track
     those separately.  */
  if (FUNC_OR_METHOD_TYPE_P (type))
    return true;

  return false;
}

static bool
field_must_have_pointers (tree t)
{
  return type_must_have_pointers (TREE_TYPE (t));
}

/* Given a TYPE, and a vector of field offsets FIELDSTACK, push all
   the fields of TYPE onto fieldstack, recording their offsets along
   the way.

   OFFSET is used to keep track of the offset in this entire
   structure, rather than just the immediately containing structure.
   Returns false if the caller is supposed to handle the field we
   recursed for.  */

static bool
push_fields_onto_fieldstack (tree type, vec<fieldoff_s> *fieldstack,
			     unsigned HOST_WIDE_INT offset)
{
  tree field;
  bool empty_p = true;

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  /* If the vector of fields is growing too big, bail out early.
     Callers check for vec::length <= param_max_fields_for_field_sensitive, make
     sure this fails.  */
  if (fieldstack->length () > (unsigned)param_max_fields_for_field_sensitive)
    return false;

  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      {
	bool push = false;
	unsigned HOST_WIDE_INT foff = bitpos_of_field (field);
	tree field_type = TREE_TYPE (field);

	if (!var_can_have_subvars (field)
	    || TREE_CODE (field_type) == QUAL_UNION_TYPE
	    || TREE_CODE (field_type) == UNION_TYPE)
	  push = true;
	else if (!push_fields_onto_fieldstack
		    (field_type, fieldstack, offset + foff)
		 && (DECL_SIZE (field)
		     && !integer_zerop (DECL_SIZE (field))))
	  /* Empty structures may have actual size, like in C++.  So
	     see if we didn't push any subfields and the size is
	     nonzero, push the field onto the stack.  */
	  push = true;

	if (push)
	  {
	    fieldoff_s *pair = NULL;
	    bool has_unknown_size = false;
	    bool must_have_pointers_p;

	    if (!fieldstack->is_empty ())
	      pair = &fieldstack->last ();

	    /* If there isn't anything at offset zero, create sth.  */
	    if (!pair
		&& offset + foff != 0)
	      {
		fieldoff_s e
		  = {0, offset + foff, false, false, true, false, NULL_TREE};
		pair = fieldstack->safe_push (e);
	      }

	    if (!DECL_SIZE (field)
		|| !tree_fits_uhwi_p (DECL_SIZE (field)))
	      has_unknown_size = true;

	    /* If adjacent fields do not contain pointers merge them.  */
	    must_have_pointers_p = field_must_have_pointers (field);
	    if (pair
		&& !has_unknown_size
		&& !must_have_pointers_p
		&& !pair->must_have_pointers
		&& !pair->has_unknown_size
		&& pair->offset + pair->size == offset + foff)
	      {
		pair->size += tree_to_uhwi (DECL_SIZE (field));
	      }
	    else
	      {
		fieldoff_s e;
		e.offset = offset + foff;
		e.has_unknown_size = has_unknown_size;
		if (!has_unknown_size)
		  e.size = tree_to_uhwi (DECL_SIZE (field));
		else
		  e.size = -1;
		e.must_have_pointers = must_have_pointers_p;
		e.may_have_pointers = true;
		e.only_restrict_pointers
		  = (!has_unknown_size
		     && POINTER_TYPE_P (field_type)
		     && TYPE_RESTRICT (field_type));
		if (e.only_restrict_pointers)
		  e.restrict_pointed_type = TREE_TYPE (field_type);
		fieldstack->safe_push (e);
	      }
	  }

	empty_p = false;
      }

  return !empty_p;
}

/* Count the number of arguments DECL has, and set IS_VARARGS to true
   if it is a varargs function.  */

static unsigned int
count_num_arguments (tree decl, bool *is_varargs)
{
  unsigned int num = 0;
  tree t;

  /* Capture named arguments for K&R functions.  They do not
     have a prototype and thus no TYPE_ARG_TYPES.  */
  for (t = DECL_ARGUMENTS (decl); t; t = DECL_CHAIN (t))
    ++num;

  /* Check if the function has variadic arguments.  */
  for (t = TYPE_ARG_TYPES (TREE_TYPE (decl)); t; t = TREE_CHAIN (t))
    if (TREE_VALUE (t) == void_type_node)
      break;
  if (!t)
    *is_varargs = true;

  return num;
}

/* Creation function node for DECL, using NAME, and return the index
   of the variable we've created for the function.  If NONLOCAL_p, create
   initial constraints.  */

static varinfo_t
create_function_info_for (tree decl, const char *name, bool add_id,
			  bool nonlocal_p)
{
  struct function *fn = DECL_STRUCT_FUNCTION (decl);
  varinfo_t vi, prev_vi;
  tree arg;
  unsigned int i;
  bool is_varargs = false;
  unsigned int num_args = count_num_arguments (decl, &is_varargs);

  /* Create the variable info.  */

  vi = new_var_info (decl, name, add_id);
  vi->offset = 0;
  vi->size = 1;
  vi->fullsize = fi_parm_base + num_args;
  vi->is_fn_info = 1;
  vi->may_have_pointers = false;
  if (is_varargs)
    vi->fullsize = ~0;
  insert_vi_for_tree (vi->decl, vi);

  prev_vi = vi;

  /* Create a variable for things the function clobbers and one for
     things the function uses.  */
    {
      varinfo_t clobbervi, usevi;
      const char *newname;
      char *tempname;

      tempname = xasprintf ("%s.clobber", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      clobbervi = new_var_info (NULL, newname, false);
      clobbervi->offset = fi_clobbers;
      clobbervi->size = 1;
      clobbervi->fullsize = vi->fullsize;
      clobbervi->is_full_var = true;
      clobbervi->is_global_var = false;
      clobbervi->is_reg_var = true;

      gcc_assert (prev_vi->offset < clobbervi->offset);
      prev_vi->next = clobbervi->id;
      prev_vi = clobbervi;

      tempname = xasprintf ("%s.use", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      usevi = new_var_info (NULL, newname, false);
      usevi->offset = fi_uses;
      usevi->size = 1;
      usevi->fullsize = vi->fullsize;
      usevi->is_full_var = true;
      usevi->is_global_var = false;
      usevi->is_reg_var = true;

      gcc_assert (prev_vi->offset < usevi->offset);
      prev_vi->next = usevi->id;
      prev_vi = usevi;
    }

  /* And one for the static chain.  */
  if (fn->static_chain_decl != NULL_TREE)
    {
      varinfo_t chainvi;
      const char *newname;
      char *tempname;

      tempname = xasprintf ("%s.chain", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      chainvi = new_var_info (fn->static_chain_decl, newname, false);
      chainvi->offset = fi_static_chain;
      chainvi->size = 1;
      chainvi->fullsize = vi->fullsize;
      chainvi->is_full_var = true;
      chainvi->is_global_var = false;

      insert_vi_for_tree (fn->static_chain_decl, chainvi);

      if (nonlocal_p
	  && chainvi->may_have_pointers)
	make_constraint_from (chainvi, nonlocal_id);

      gcc_assert (prev_vi->offset < chainvi->offset);
      prev_vi->next = chainvi->id;
      prev_vi = chainvi;
    }

  /* Create a variable for the return var.  */
  if (DECL_RESULT (decl) != NULL
      || !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
    {
      varinfo_t resultvi;
      const char *newname;
      char *tempname;
      tree resultdecl = decl;

      if (DECL_RESULT (decl))
	resultdecl = DECL_RESULT (decl);

      tempname = xasprintf ("%s.result", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      resultvi = new_var_info (resultdecl, newname, false);
      resultvi->offset = fi_result;
      resultvi->size = 1;
      resultvi->fullsize = vi->fullsize;
      resultvi->is_full_var = true;
      if (DECL_RESULT (decl))
	resultvi->may_have_pointers = true;

      if (DECL_RESULT (decl))
	insert_vi_for_tree (DECL_RESULT (decl), resultvi);

      if (nonlocal_p
	  && DECL_RESULT (decl)
	  && DECL_BY_REFERENCE (DECL_RESULT (decl)))
	make_constraint_from (resultvi, nonlocal_id);

      gcc_assert (prev_vi->offset < resultvi->offset);
      prev_vi->next = resultvi->id;
      prev_vi = resultvi;
    }

  /* We also need to make function return values escape.  Nothing
     escapes by returning from main though.  */
  if (nonlocal_p
      && !MAIN_NAME_P (DECL_NAME (decl)))
    {
      varinfo_t fi, rvi;
      fi = lookup_vi_for_tree (decl);
      rvi = first_vi_for_offset (fi, fi_result);
      if (rvi && rvi->offset == fi_result)
	make_copy_constraint (get_varinfo (escaped_id), rvi->id);
    }

  /* Set up variables for each argument.  */
  arg = DECL_ARGUMENTS (decl);
  for (i = 0; i < num_args; i++)
    {
      varinfo_t argvi;
      const char *newname;
      char *tempname;
      tree argdecl = decl;

      if (arg)
	argdecl = arg;

      tempname = xasprintf ("%s.arg%d", name, i);
      newname = ggc_strdup (tempname);
      free (tempname);

      argvi = new_var_info (argdecl, newname, false);
      argvi->offset = fi_parm_base + i;
      argvi->size = 1;
      argvi->is_full_var = true;
      argvi->fullsize = vi->fullsize;
      if (arg)
	argvi->may_have_pointers = true;

      if (arg)
	insert_vi_for_tree (arg, argvi);

      if (nonlocal_p
	  && argvi->may_have_pointers)
	make_constraint_from (argvi, nonlocal_id);

      gcc_assert (prev_vi->offset < argvi->offset);
      prev_vi->next = argvi->id;
      prev_vi = argvi;
      if (arg)
	arg = DECL_CHAIN (arg);
    }

  /* Add one representative for all further args.  */
  if (is_varargs)
    {
      varinfo_t argvi;
      const char *newname;
      char *tempname;
      tree decl;

      tempname = xasprintf ("%s.varargs", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      /* We need sth that can be pointed to for va_start.  */
      decl = build_fake_var_decl (ptr_type_node);

      argvi = new_var_info (decl, newname, false);
      argvi->offset = fi_parm_base + num_args;
      argvi->size = ~0;
      argvi->is_full_var = true;
      argvi->is_heap_var = true;
      argvi->fullsize = vi->fullsize;

      if (nonlocal_p
	  && argvi->may_have_pointers)
	make_constraint_from (argvi, nonlocal_id);

      gcc_assert (prev_vi->offset < argvi->offset);
      prev_vi->next = argvi->id;
    }

  return vi;
}


/* Return true if FIELDSTACK contains fields that overlap.
   FIELDSTACK is assumed to be sorted by offset.  */

static bool
check_for_overlaps (const vec<fieldoff_s> &fieldstack)
{
  fieldoff_s *fo = NULL;
  unsigned int i;
  HOST_WIDE_INT lastoffset = -1;

  FOR_EACH_VEC_ELT (fieldstack, i, fo)
    {
      if (fo->offset == lastoffset)
	return true;
      lastoffset = fo->offset;
    }
  return false;
}

/* Create a varinfo structure for NAME and DECL, and add it to VARMAP.
   This will also create any varinfo structures necessary for fields
   of DECL.  DECL is a function parameter if HANDLE_PARAM is set.
   HANDLED_STRUCT_TYPE is used to register struct types reached by following
   restrict pointers.  This is needed to prevent infinite recursion.
   If ADD_RESTRICT, pretend that the pointer NAME is restrict even if DECL
   does not advertise it.  */

static varinfo_t
create_variable_info_for_1 (tree decl, const char *name, bool add_id,
			    bool handle_param, bitmap handled_struct_type,
			    bool add_restrict = false)
{
  varinfo_t vi, newvi;
  tree decl_type = TREE_TYPE (decl);
  tree declsize = DECL_P (decl) ? DECL_SIZE (decl) : TYPE_SIZE (decl_type);
  auto_vec<fieldoff_s> fieldstack;
  fieldoff_s *fo;
  unsigned int i;

  if (!declsize
      || !tree_fits_uhwi_p (declsize))
    {
      vi = new_var_info (decl, name, add_id);
      vi->offset = 0;
      vi->size = ~0;
      vi->fullsize = ~0;
      vi->is_unknown_size_var = true;
      vi->is_full_var = true;
      vi->may_have_pointers = true;
      return vi;
    }

  /* Collect field information.  */
  if (use_field_sensitive
      && var_can_have_subvars (decl)
      /* ???  Force us to not use subfields for globals in IPA mode.
	 Else we'd have to parse arbitrary initializers.  */
      && !(in_ipa_mode
	   && is_global_var (decl)))
    {
      fieldoff_s *fo = NULL;
      bool notokay = false;
      unsigned int i;

      push_fields_onto_fieldstack (decl_type, &fieldstack, 0);

      for (i = 0; !notokay && fieldstack.iterate (i, &fo); i++)
	if (fo->has_unknown_size
	    || fo->offset < 0)
	  {
	    notokay = true;
	    break;
	  }

      /* We can't sort them if we have a field with a variable sized type,
	 which will make notokay = true.  In that case, we are going to return
	 without creating varinfos for the fields anyway, so sorting them is a
	 waste to boot.  */
      if (!notokay)
	{
	  sort_fieldstack (fieldstack);
	  /* Due to some C++ FE issues, like PR 22488, we might end up
	     what appear to be overlapping fields even though they,
	     in reality, do not overlap.  Until the C++ FE is fixed,
	     we will simply disable field-sensitivity for these cases.  */
	  notokay = check_for_overlaps (fieldstack);
	}

      if (notokay)
	fieldstack.release ();
    }

  /* If we didn't end up collecting sub-variables create a full
     variable for the decl.  */
  if (fieldstack.length () == 0
      || fieldstack.length () > (unsigned)param_max_fields_for_field_sensitive)
    {
      vi = new_var_info (decl, name, add_id);
      vi->offset = 0;
      vi->may_have_pointers = true;
      vi->fullsize = tree_to_uhwi (declsize);
      vi->size = vi->fullsize;
      vi->is_full_var = true;
      if (POINTER_TYPE_P (decl_type)
	  && (TYPE_RESTRICT (decl_type) || add_restrict))
	vi->only_restrict_pointers = 1;
      if (vi->only_restrict_pointers
	  && !type_contains_placeholder_p (TREE_TYPE (decl_type))
	  && handle_param
	  && !bitmap_bit_p (handled_struct_type,
			    TYPE_UID (TREE_TYPE (decl_type))))
	{
	  varinfo_t rvi;
	  tree heapvar = build_fake_var_decl (TREE_TYPE (decl_type));
	  DECL_EXTERNAL (heapvar) = 1;
	  if (var_can_have_subvars (heapvar))
	    bitmap_set_bit (handled_struct_type,
			    TYPE_UID (TREE_TYPE (decl_type)));
	  rvi = create_variable_info_for_1 (heapvar, "PARM_NOALIAS", true,
					    true, handled_struct_type);
	  if (var_can_have_subvars (heapvar))
	    bitmap_clear_bit (handled_struct_type,
			      TYPE_UID (TREE_TYPE (decl_type)));
	  rvi->is_restrict_var = 1;
	  insert_vi_for_tree (heapvar, rvi);
	  make_constraint_from (vi, rvi->id);
	  make_param_constraints (rvi);
	}
      fieldstack.release ();
      return vi;
    }

  vi = new_var_info (decl, name, add_id);
  vi->fullsize = tree_to_uhwi (declsize);
  if (fieldstack.length () == 1)
    vi->is_full_var = true;
  for (i = 0, newvi = vi;
       fieldstack.iterate (i, &fo);
       ++i, newvi = vi_next (newvi))
    {
      const char *newname = NULL;
      char *tempname;

      if (dump_file)
	{
	  if (fieldstack.length () != 1)
	    {
	      tempname
		= xasprintf ("%s." HOST_WIDE_INT_PRINT_DEC
			     "+" HOST_WIDE_INT_PRINT_DEC, name,
			     fo->offset, fo->size);
	      newname = ggc_strdup (tempname);
	      free (tempname);
	    }
	}
      else
	newname = "NULL";

      if (newname)
	  newvi->name = newname;
      newvi->offset = fo->offset;
      newvi->size = fo->size;
      newvi->fullsize = vi->fullsize;
      newvi->may_have_pointers = fo->may_have_pointers;
      newvi->only_restrict_pointers = fo->only_restrict_pointers;
      if (handle_param
	  && newvi->only_restrict_pointers
	  && !type_contains_placeholder_p (fo->restrict_pointed_type)
	  && !bitmap_bit_p (handled_struct_type,
			    TYPE_UID (fo->restrict_pointed_type)))
	{
	  varinfo_t rvi;
	  tree heapvar = build_fake_var_decl (fo->restrict_pointed_type);
	  DECL_EXTERNAL (heapvar) = 1;
	  if (var_can_have_subvars (heapvar))
	    bitmap_set_bit (handled_struct_type,
			    TYPE_UID (fo->restrict_pointed_type));
	  rvi = create_variable_info_for_1 (heapvar, "PARM_NOALIAS", true,
					    true, handled_struct_type);
	  if (var_can_have_subvars (heapvar))
	    bitmap_clear_bit (handled_struct_type,
			      TYPE_UID (fo->restrict_pointed_type));
	  rvi->is_restrict_var = 1;
	  insert_vi_for_tree (heapvar, rvi);
	  make_constraint_from (newvi, rvi->id);
	  make_param_constraints (rvi);
	}
      if (i + 1 < fieldstack.length ())
	{
	  varinfo_t tem = new_var_info (decl, name, false);
	  newvi->next = tem->id;
	  tem->head = vi->id;
	}
    }

  return vi;
}

static unsigned int
create_variable_info_for (tree decl, const char *name, bool add_id)
{
  /* First see if we are dealing with an ifunc resolver call and
     assiociate that with a call to the resolver function result.  */
  cgraph_node *node;
  if (in_ipa_mode
      && TREE_CODE (decl) == FUNCTION_DECL
      && (node = cgraph_node::get (decl))
      && node->ifunc_resolver)
    {
      varinfo_t fi = get_vi_for_tree (node->get_alias_target ()->decl);
      constraint_expr rhs
	= get_function_part_constraint (fi, fi_result);
      fi = new_var_info (NULL_TREE, "ifuncres", true);
      fi->is_reg_var = true;
      constraint_expr lhs;
      lhs.type = SCALAR;
      lhs.var = fi->id;
      lhs.offset = 0;
      process_constraint (new_constraint (lhs, rhs));
      insert_vi_for_tree (decl, fi);
      return fi->id;
    }

  varinfo_t vi = create_variable_info_for_1 (decl, name, add_id, false, NULL);
  unsigned int id = vi->id;

  insert_vi_for_tree (decl, vi);

  if (!VAR_P (decl))
    return id;

  /* Create initial constraints for globals.  */
  for (; vi; vi = vi_next (vi))
    {
      if (!vi->may_have_pointers
	  || !vi->is_global_var)
	continue;

      /* Mark global restrict qualified pointers.  */
      if ((POINTER_TYPE_P (TREE_TYPE (decl))
	   && TYPE_RESTRICT (TREE_TYPE (decl)))
	  || vi->only_restrict_pointers)
	{
	  varinfo_t rvi
	    = make_constraint_from_global_restrict (vi, "GLOBAL_RESTRICT",
						    true);
	  /* ???  For now exclude reads from globals as restrict sources
	     if those are not (indirectly) from incoming parameters.  */
	  rvi->is_restrict_var = false;
	  continue;
	}

      /* In non-IPA mode the initializer from nonlocal is all we need.  */
      if (!in_ipa_mode
	  || DECL_HARD_REGISTER (decl))
	make_copy_constraint (vi, nonlocal_id);

      /* In IPA mode parse the initializer and generate proper constraints
	 for it.  */
      else
	{
	  varpool_node *vnode = varpool_node::get (decl);

	  /* For escaped variables initialize them from nonlocal.  */
	  if (!vnode || !vnode->all_refs_explicit_p ())
	    make_copy_constraint (vi, nonlocal_id);

	  /* While we can in theory walk references for the varpool
	     node that does not cover zero-initialization or references
	     to the constant pool.  */
	  if (DECL_INITIAL (decl))
	    {
	      auto_vec<ce_s> rhsc;
	      struct constraint_expr lhs, *rhsp;
	      unsigned i;
	      lhs.var = vi->id;
	      lhs.offset = 0;
	      lhs.type = SCALAR;
	      get_constraint_for (DECL_INITIAL (decl), &rhsc);
	      FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		process_constraint (new_constraint (lhs, *rhsp));
	      /* If this is a variable that escapes from the unit
		 the initializer escapes as well.  */
	      if (!vnode || !vnode->all_refs_explicit_p ())
		{
		  lhs.var = escaped_id;
		  lhs.offset = 0;
		  lhs.type = SCALAR;
		  FOR_EACH_VEC_ELT (rhsc, i, rhsp)
		    process_constraint (new_constraint (lhs, *rhsp));
		}
	    }
	}
    }

  return id;
}

/* Register the constraints for function parameter related VI.  */

static void
make_param_constraints (varinfo_t vi)
{
  for (; vi; vi = vi_next (vi))
    {
      if (vi->only_restrict_pointers)
	;
      else if (vi->may_have_pointers)
	make_constraint_from (vi, nonlocal_id);

      if (vi->is_full_var)
	break;
    }
}

/* Create varinfo structures for all of the variables in the
   function for intraprocedural mode.  */

static void
intra_create_variable_infos (struct function *fn)
{
  tree t;
  bitmap handled_struct_type = NULL;
  bool this_parm_in_ctor = DECL_CXX_CONSTRUCTOR_P (fn->decl);

  /* For each incoming pointer argument arg, create the constraint ARG
     = NONLOCAL or a dummy variable if it is a restrict qualified
     passed-by-reference argument.  */
  for (t = DECL_ARGUMENTS (fn->decl); t; t = DECL_CHAIN (t))
    {
      if (handled_struct_type == NULL)
	handled_struct_type = BITMAP_ALLOC (NULL);

      varinfo_t p
	= create_variable_info_for_1 (t, alias_get_name (t), false, true,
				      handled_struct_type, this_parm_in_ctor);
      insert_vi_for_tree (t, p);

      make_param_constraints (p);

      this_parm_in_ctor = false;
    }

  if (handled_struct_type != NULL)
    BITMAP_FREE (handled_struct_type);

  /* Add a constraint for a result decl that is passed by reference.  */
  if (DECL_RESULT (fn->decl)
      && DECL_BY_REFERENCE (DECL_RESULT (fn->decl)))
    {
      varinfo_t p, result_vi = get_vi_for_tree (DECL_RESULT (fn->decl));

      for (p = result_vi; p; p = vi_next (p))
	make_constraint_from (p, nonlocal_id);
    }

  /* Add a constraint for the incoming static chain parameter.  */
  if (fn->static_chain_decl != NULL_TREE)
    {
      varinfo_t p, chain_vi = get_vi_for_tree (fn->static_chain_decl);

      for (p = chain_vi; p; p = vi_next (p))
	make_constraint_from (p, nonlocal_id);
    }
}

/* Initialize the always-existing constraint variables for NULL
   ANYTHING, READONLY, and INTEGER.  */

static void
init_base_vars (void)
{
  struct constraint_expr lhs, rhs;
  varinfo_t var_anything;
  varinfo_t var_nothing;
  varinfo_t var_string;
  varinfo_t var_escaped;
  varinfo_t var_nonlocal;
  varinfo_t var_escaped_return;
  varinfo_t var_storedanything;
  varinfo_t var_integer;

  /* Variable ID zero is reserved and should be NULL.  */
  varmap.safe_push (NULL);

  /* Create the NULL variable, used to represent that a variable points
     to NULL.  */
  var_nothing = new_var_info (NULL_TREE, "NULL", false);
  gcc_assert (var_nothing->id == nothing_id);
  var_nothing->is_artificial_var = 1;
  var_nothing->offset = 0;
  var_nothing->size = ~0;
  var_nothing->fullsize = ~0;
  var_nothing->is_special_var = 1;
  var_nothing->may_have_pointers = 0;
  var_nothing->is_global_var = 0;

  /* Create the ANYTHING variable, used to represent that a variable
     points to some unknown piece of memory.  */
  var_anything = new_var_info (NULL_TREE, "ANYTHING", false);
  gcc_assert (var_anything->id == anything_id);
  var_anything->is_artificial_var = 1;
  var_anything->size = ~0;
  var_anything->offset = 0;
  var_anything->fullsize = ~0;
  var_anything->is_special_var = 1;

  /* Anything points to anything.  This makes deref constraints just
     work in the presence of linked list and other p = *p type loops,
     by saying that *ANYTHING = ANYTHING.  */
  lhs.type = SCALAR;
  lhs.var = anything_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;

  /* This specifically does not use process_constraint because
     process_constraint ignores all anything = anything constraints, since all
     but this one are redundant.  */
  constraints.safe_push (new_constraint (lhs, rhs));

  /* Create the STRING variable, used to represent that a variable
     points to a string literal.  String literals don't contain
     pointers so STRING doesn't point to anything.  */
  var_string = new_var_info (NULL_TREE, "STRING", false);
  gcc_assert (var_string->id == string_id);
  var_string->is_artificial_var = 1;
  var_string->offset = 0;
  var_string->size = ~0;
  var_string->fullsize = ~0;
  var_string->is_special_var = 1;
  var_string->may_have_pointers = 0;

  /* Create the ESCAPED variable, used to represent the set of escaped
     memory.  */
  var_escaped = new_var_info (NULL_TREE, "ESCAPED", false);
  gcc_assert (var_escaped->id == escaped_id);
  var_escaped->is_artificial_var = 1;
  var_escaped->offset = 0;
  var_escaped->size = ~0;
  var_escaped->fullsize = ~0;
  var_escaped->is_special_var = 0;

  /* Create the NONLOCAL variable, used to represent the set of nonlocal
     memory.  */
  var_nonlocal = new_var_info (NULL_TREE, "NONLOCAL", false);
  gcc_assert (var_nonlocal->id == nonlocal_id);
  var_nonlocal->is_artificial_var = 1;
  var_nonlocal->offset = 0;
  var_nonlocal->size = ~0;
  var_nonlocal->fullsize = ~0;
  var_nonlocal->is_special_var = 1;

  /* Create the ESCAPED_RETURN variable, used to represent the set of escaped
     memory via a regular return stmt.  */
  var_escaped_return = new_var_info (NULL_TREE, "ESCAPED_RETURN", false);
  gcc_assert (var_escaped_return->id == escaped_return_id);
  var_escaped_return->is_artificial_var = 1;
  var_escaped_return->offset = 0;
  var_escaped_return->size = ~0;
  var_escaped_return->fullsize = ~0;
  var_escaped_return->is_special_var = 0;

  /* ESCAPED = *ESCAPED, because escaped is may-deref'd at calls, etc.  */
  lhs.type = SCALAR;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = DEREF;
  rhs.var = escaped_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* ESCAPED = ESCAPED + UNKNOWN_OFFSET, because if a sub-field escapes the
     whole variable escapes.  */
  lhs.type = SCALAR;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = escaped_id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));

  /* *ESCAPED = NONLOCAL.  This is true because we have to assume
     everything pointed to by escaped points to what global memory can
     point to.  */
  lhs.type = DEREF;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = nonlocal_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* NONLOCAL = &NONLOCAL, NONLOCAL = &ESCAPED.  This is true because
     global memory may point to global memory and escaped memory.  */
  lhs.type = SCALAR;
  lhs.var = nonlocal_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = nonlocal_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));
  rhs.type = ADDRESSOF;
  rhs.var = escaped_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* Transitively close ESCAPED_RETURN.
     ESCAPED_RETURN = ESCAPED_RETURN + UNKNOWN_OFFSET
     ESCAPED_RETURN = *ESCAPED_RETURN.  */
  lhs.type = SCALAR;
  lhs.var = escaped_return_id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = escaped_return_id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));
  lhs.type = SCALAR;
  lhs.var = escaped_return_id;
  lhs.offset = 0;
  rhs.type = DEREF;
  rhs.var = escaped_return_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* Create the STOREDANYTHING variable, used to represent the set of
     variables stored to *ANYTHING.  */
  var_storedanything = new_var_info (NULL_TREE, "STOREDANYTHING", false);
  gcc_assert (var_storedanything->id == storedanything_id);
  var_storedanything->is_artificial_var = 1;
  var_storedanything->offset = 0;
  var_storedanything->size = ~0;
  var_storedanything->fullsize = ~0;
  var_storedanything->is_special_var = 0;

  /* Create the INTEGER variable, used to represent that a variable points
     to what an INTEGER "points to".  */
  var_integer = new_var_info (NULL_TREE, "INTEGER", false);
  gcc_assert (var_integer->id == integer_id);
  var_integer->is_artificial_var = 1;
  var_integer->size = ~0;
  var_integer->fullsize = ~0;
  var_integer->offset = 0;
  var_integer->is_special_var = 1;

  /* INTEGER = ANYTHING, because we don't know where a dereference of
     a random integer will point to.  */
  lhs.type = SCALAR;
  lhs.var = integer_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));
}

/* Associate node with varinfo DATA.  Worker for
   cgraph_for_symbol_thunks_and_aliases.  */
static bool
associate_varinfo_to_alias (struct cgraph_node *node, void *data)
{
  if ((node->alias
       || (node->thunk
	   && ! node->inlined_to))
      && node->analyzed
      && !node->ifunc_resolver)
    insert_vi_for_tree (node->decl, (varinfo_t)data);
  return false;
}

/* Compute whether node is refered to non-locally.  Worker for
   cgraph_for_symbol_thunks_and_aliases.  */
static bool
refered_from_nonlocal_fn (struct cgraph_node *node, void *data)
{
  bool *nonlocal_p = (bool *)data;
  *nonlocal_p |= (node->used_from_other_partition
		  || DECL_EXTERNAL (node->decl)
		  || TREE_PUBLIC (node->decl)
		  || node->force_output
		  || lookup_attribute ("noipa", DECL_ATTRIBUTES (node->decl)));
  return false;
}

/* Same for varpool nodes.  */
static bool
refered_from_nonlocal_var (struct varpool_node *node, void *data)
{
  bool *nonlocal_p = (bool *)data;
  *nonlocal_p |= (node->used_from_other_partition
		  || DECL_EXTERNAL (node->decl)
		  || TREE_PUBLIC (node->decl)
		  || node->force_output);
  return false;
}

/* Create function infos.  */

static void
ipa_create_function_infos (void)
{
  struct cgraph_node *node;
  unsigned int constr_count = constraints.length ();

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      varinfo_t vi;
      /* Nodes without a body in this partition are not interesting.
	 Especially do not visit clones at this point for now - we
	 get duplicate decls there for inline clones at least.  */
      if (!node->has_gimple_body_p ()
	  || node->in_other_partition
	  || node->inlined_to)
	continue;
      node->get_body ();

      gcc_assert (!node->clone_of);

      /* For externally visible or attribute used annotated functions use
	 local constraints for their arguments.
	 For local functions we see all callers and thus do not need initial
	 constraints for parameters.  */
      bool nonlocal_p = (node->used_from_other_partition
			 || DECL_EXTERNAL (node->decl)
			 || TREE_PUBLIC (node->decl)
			 || node->force_output
			 || lookup_attribute ("noipa",
					      DECL_ATTRIBUTES (node->decl)));
      node->call_for_symbol_thunks_and_aliases (refered_from_nonlocal_fn,
						&nonlocal_p, true);

      vi = create_function_info_for (node->decl,
				     alias_get_name (node->decl), false,
				     nonlocal_p);
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && constr_count != constraints.length ())
	{
	  fprintf (dump_file,
		   "Generating initial constraints for %s",
		   node->dump_name ());
	  if (DECL_ASSEMBLER_NAME_SET_P (node->decl))
	    fprintf (dump_file, " (%s)",
		     IDENTIFIER_POINTER
		       (DECL_ASSEMBLER_NAME (node->decl)));
	  fprintf (dump_file, "\n\n");
	  dump_constraints (dump_file, constr_count);
	  fprintf (dump_file, "\n");

	  constr_count = constraints.length ();
	}

      node->call_for_symbol_thunks_and_aliases
	(associate_varinfo_to_alias, vi, true);
    }
}

/* Create constraints for global variables and their initializers.  */

static void
ipa_create_global_variable_infos (void)
{
  varpool_node *var;
  unsigned int constr_count = constraints.length ();

  FOR_EACH_VARIABLE (var)
    {
      if (var->alias && var->analyzed)
	continue;

      varinfo_t vi = get_vi_for_tree (var->decl);

      /* For the purpose of IPA PTA unit-local globals are not
	 escape points.  */
      bool nonlocal_p = (DECL_EXTERNAL (var->decl)
			 || TREE_PUBLIC (var->decl)
			 || var->used_from_other_partition
			 || var->force_output);
      var->call_for_symbol_and_aliases (refered_from_nonlocal_var,
					&nonlocal_p, true);
      if (nonlocal_p)
	vi->is_ipa_escape_point = true;
    }

  if (dump_file && (dump_flags & TDF_DETAILS)
      && constr_count != constraints.length ())
    {
      fprintf (dump_file,
	       "Generating constraints for global initializers\n\n");
      dump_constraints (dump_file, constr_count);
      fprintf (dump_file, "\n");
      constr_count = constraints.length ();
    }
}


namespace pointer_analysis {

/* Find the variable info for tree T in VI_FOR_TREE.  If T does not
   exist in the map, return NULL, otherwise, return the varinfo we found.  */

varinfo_t
lookup_vi_for_tree (tree t)
{
  varinfo_t *slot = vi_for_tree->get (t);
  if (slot == NULL)
    return NULL;

  return *slot;
}

/* Lookup the variable for the call statement CALL representing
   the uses.  Returns NULL if there is nothing special about this call.  */

varinfo_t
lookup_call_use_vi (gcall *call)
{
  varinfo_t *slot_p = call_stmt_vars->get (call);
  if (slot_p)
    return *slot_p;

  return NULL;
}

/* Lookup the variable for the call statement CALL representing
   the clobbers.  Returns NULL if there is nothing special about this call.  */

varinfo_t
lookup_call_clobber_vi (gcall *call)
{
  varinfo_t uses = lookup_call_use_vi (call);
  if (!uses)
    return NULL;

  return vi_next (uses);
}

/* Return the varinfo for the callee of CALL.  */

varinfo_t
get_fi_for_callee (gcall *call)
{
  tree decl, fn = gimple_call_fn (call);

  if (fn && TREE_CODE (fn) == OBJ_TYPE_REF)
    fn = OBJ_TYPE_REF_EXPR (fn);

  /* If we can directly resolve the function being called, do so.
     Otherwise, it must be some sort of indirect expression that
     we should still be able to handle.  */
  decl = gimple_call_addr_fndecl (fn);
  if (decl)
    return get_vi_for_tree (decl);

  /* If the function is anything other than a SSA name pointer we have no
     clue and should be getting ANYFN (well, ANYTHING for now).  */
  if (!fn || TREE_CODE (fn) != SSA_NAME)
    return get_varinfo (anything_id);

  if (SSA_NAME_IS_DEFAULT_DEF (fn)
      && (TREE_CODE (SSA_NAME_VAR (fn)) == PARM_DECL
	  || TREE_CODE (SSA_NAME_VAR (fn)) == RESULT_DECL))
    fn = SSA_NAME_VAR (fn);

  return get_vi_for_tree (fn);
}

/* Initialize constraint builder.  */

void
init_constraint_builder (void)
{
  vi_for_tree = new hash_map<tree, varinfo_t>;
  call_stmt_vars = new hash_map<gimple *, varinfo_t>;
  gcc_obstack_init (&fake_var_decl_obstack);

  init_base_vars ();
}

/* Deallocate constraint builder globals.  */

void
delete_constraint_builder (void)
{
  delete vi_for_tree;
  delete call_stmt_vars;
  constraint_pool.release ();
  obstack_free (&fake_var_decl_obstack, NULL);
}

/* Build constraints for intraprocedural mode.  */

void
intra_build_constraints (void)
{
  basic_block bb;

  intra_create_variable_infos (cfun);

  /* Now walk all statements and build the constraint set.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();

	  if (! virtual_operand_p (gimple_phi_result (phi)))
	    find_func_aliases (cfun, phi);
	}

      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  find_func_aliases (cfun, stmt);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Points-to analysis\n\nConstraints:\n\n");
      dump_constraints (dump_file, 0);
    }
}

/* Build constraints for ipa mode.  */

void
ipa_build_constraints (void)
{
  struct cgraph_node *node;

  ipa_create_function_infos ();
  ipa_create_global_variable_infos ();

  unsigned int constr_count = constraints.length ();

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      struct function *func;
      basic_block bb;

      /* Nodes without a body in this partition are not interesting.  */
      if (!node->has_gimple_body_p ()
	  || node->in_other_partition
	  || node->clone_of)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
		   "Generating constraints for %s", node->dump_name ());
	  if (DECL_ASSEMBLER_NAME_SET_P (node->decl))
	    fprintf (dump_file, " (%s)",
		     IDENTIFIER_POINTER
		       (DECL_ASSEMBLER_NAME (node->decl)));
	  fprintf (dump_file, "\n");
	}

      func = DECL_STRUCT_FUNCTION (node->decl);
      gcc_assert (cfun == NULL);

      /* Build constraints for the function body.  */
      FOR_EACH_BB_FN (bb, func)
	{
	  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *phi = gsi.phi ();

	      if (! virtual_operand_p (gimple_phi_result (phi)))
		find_func_aliases (func, phi);
	    }

	  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);

	      find_func_aliases (func, stmt);
	      find_func_clobbers (func, stmt);
	    }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n");
	  dump_constraints (dump_file, constr_count);
	  fprintf (dump_file, "\n");
	  constr_count = constraints.length ();
	}
    }
}

} // namespace pointer_analysis
