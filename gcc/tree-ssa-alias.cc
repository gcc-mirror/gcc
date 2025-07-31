/* Alias analysis for trees.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "timevar.h"	/* for TV_ALIAS_STMT_WALK */
#include "ssa.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "langhooks.h"
#include "dumpfile.h"
#include "tree-eh.h"
#include "tree-dfa.h"
#include "ipa-reference.h"
#include "varasm.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "attr-fnspec.h"
#include "errors.h"
#include "dbgcnt.h"
#include "gimple-pretty-print.h"
#include "print-tree.h"
#include "tree-ssa-alias-compare.h"
#include "builtins.h"
#include "internal-fn.h"
#include "ipa-utils.h"

/* Broad overview of how alias analysis on gimple works:

   Statements clobbering or using memory are linked through the
   virtual operand factored use-def chain.  The virtual operand
   is unique per function, its symbol is accessible via gimple_vop (cfun).
   Virtual operands are used for efficiently walking memory statements
   in the gimple IL and are useful for things like value-numbering as
   a generation count for memory references.

   SSA_NAME pointers may have associated points-to information
   accessible via the SSA_NAME_PTR_INFO macro.  Flow-insensitive
   points-to information is (re-)computed by the TODO_rebuild_alias
   pass manager todo.  Points-to information is also used for more
   precise tracking of call-clobbered and call-used variables and
   related disambiguations.

   This file contains functions for disambiguating memory references,
   the so called alias-oracle and tools for walking of the gimple IL.

   The main alias-oracle entry-points are

   bool stmt_may_clobber_ref_p (gimple *, tree)

     This function queries if a statement may invalidate (parts of)
     the memory designated by the reference tree argument.

   bool ref_maybe_used_by_stmt_p (gimple *, tree)

     This function queries if a statement may need (parts of) the
     memory designated by the reference tree argument.

   There are variants of these functions that only handle the call
   part of a statement, call_may_clobber_ref_p and ref_maybe_used_by_call_p.
   Note that these do not disambiguate against a possible call lhs.

   bool refs_may_alias_p (tree, tree)

     This function tries to disambiguate two reference trees.

   bool ptr_deref_may_alias_global_p (tree, bool)

     This function queries if dereferencing a pointer variable may
     alias global memory.  If bool argument is true, global memory
     is considered to also include function local memory that escaped.

   More low-level disambiguators are available and documented in
   this file.  Low-level disambiguators dealing with points-to
   information are in tree-ssa-structalias.cc.  */

static int nonoverlapping_refs_since_match_p (tree, tree, tree, tree, bool);
static bool nonoverlapping_component_refs_p (const_tree, const_tree);

/* Query statistics for the different low-level disambiguators.
   A high-level query may trigger multiple of them.  */

static struct {
  unsigned HOST_WIDE_INT refs_may_alias_p_may_alias;
  unsigned HOST_WIDE_INT refs_may_alias_p_no_alias;
  unsigned HOST_WIDE_INT ref_maybe_used_by_call_p_may_alias;
  unsigned HOST_WIDE_INT ref_maybe_used_by_call_p_no_alias;
  unsigned HOST_WIDE_INT call_may_clobber_ref_p_may_alias;
  unsigned HOST_WIDE_INT call_may_clobber_ref_p_no_alias;
  unsigned HOST_WIDE_INT aliasing_component_refs_p_may_alias;
  unsigned HOST_WIDE_INT aliasing_component_refs_p_no_alias;
  unsigned HOST_WIDE_INT nonoverlapping_component_refs_p_may_alias;
  unsigned HOST_WIDE_INT nonoverlapping_component_refs_p_no_alias;
  unsigned HOST_WIDE_INT nonoverlapping_refs_since_match_p_may_alias;
  unsigned HOST_WIDE_INT nonoverlapping_refs_since_match_p_must_overlap;
  unsigned HOST_WIDE_INT nonoverlapping_refs_since_match_p_no_alias;
  unsigned HOST_WIDE_INT stmt_kills_ref_p_no;
  unsigned HOST_WIDE_INT stmt_kills_ref_p_yes;
  unsigned HOST_WIDE_INT modref_use_may_alias;
  unsigned HOST_WIDE_INT modref_use_no_alias;
  unsigned HOST_WIDE_INT modref_clobber_may_alias;
  unsigned HOST_WIDE_INT modref_clobber_no_alias;
  unsigned HOST_WIDE_INT modref_kill_no;
  unsigned HOST_WIDE_INT modref_kill_yes;
  unsigned HOST_WIDE_INT modref_tests;
  unsigned HOST_WIDE_INT modref_baseptr_tests;
} alias_stats;

void
dump_alias_stats (FILE *s)
{
  fprintf (s, "\nAlias oracle query stats:\n");
  fprintf (s, "  refs_may_alias_p: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.refs_may_alias_p_no_alias,
	   alias_stats.refs_may_alias_p_no_alias
	   + alias_stats.refs_may_alias_p_may_alias);
  fprintf (s, "  ref_maybe_used_by_call_p: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.ref_maybe_used_by_call_p_no_alias,
	   alias_stats.refs_may_alias_p_no_alias
	   + alias_stats.ref_maybe_used_by_call_p_may_alias);
  fprintf (s, "  call_may_clobber_ref_p: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.call_may_clobber_ref_p_no_alias,
	   alias_stats.call_may_clobber_ref_p_no_alias
	   + alias_stats.call_may_clobber_ref_p_may_alias);
  fprintf (s, "  stmt_kills_ref_p: "
	   HOST_WIDE_INT_PRINT_DEC" kills, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.stmt_kills_ref_p_yes + alias_stats.modref_kill_yes,
	   alias_stats.stmt_kills_ref_p_yes + alias_stats.modref_kill_yes
	   + alias_stats.stmt_kills_ref_p_no + alias_stats.modref_kill_no);
  fprintf (s, "  nonoverlapping_component_refs_p: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.nonoverlapping_component_refs_p_no_alias,
	   alias_stats.nonoverlapping_component_refs_p_no_alias
	   + alias_stats.nonoverlapping_component_refs_p_may_alias);
  fprintf (s, "  nonoverlapping_refs_since_match_p: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" must overlaps, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.nonoverlapping_refs_since_match_p_no_alias,
	   alias_stats.nonoverlapping_refs_since_match_p_must_overlap,
	   alias_stats.nonoverlapping_refs_since_match_p_no_alias
	   + alias_stats.nonoverlapping_refs_since_match_p_may_alias
	   + alias_stats.nonoverlapping_refs_since_match_p_must_overlap);
  fprintf (s, "  aliasing_component_refs_p: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.aliasing_component_refs_p_no_alias,
	   alias_stats.aliasing_component_refs_p_no_alias
	   + alias_stats.aliasing_component_refs_p_may_alias);
  dump_alias_stats_in_alias_c (s);
  fprintf (s, "\nModref stats:\n");
  fprintf (s, "  modref kill: "
	   HOST_WIDE_INT_PRINT_DEC" kills, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.modref_kill_yes,
	   alias_stats.modref_kill_yes
	   + alias_stats.modref_kill_no);
  fprintf (s, "  modref use: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   alias_stats.modref_use_no_alias,
	   alias_stats.modref_use_no_alias
	   + alias_stats.modref_use_may_alias);
  fprintf (s, "  modref clobber: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n"
	   "  " HOST_WIDE_INT_PRINT_DEC" tbaa queries (%f per modref query)\n"
	   "  " HOST_WIDE_INT_PRINT_DEC" base compares (%f per modref query)\n",
	   alias_stats.modref_clobber_no_alias,
	   alias_stats.modref_clobber_no_alias
	   + alias_stats.modref_clobber_may_alias,
	   alias_stats.modref_tests,
	   ((double)alias_stats.modref_tests)
	   / (alias_stats.modref_clobber_no_alias
	      + alias_stats.modref_clobber_may_alias),
	   alias_stats.modref_baseptr_tests,
	   ((double)alias_stats.modref_baseptr_tests)
	   / (alias_stats.modref_clobber_no_alias
	      + alias_stats.modref_clobber_may_alias));
}


/* Return true, if dereferencing PTR may alias with a global variable.
   When ESCAPED_LOCAL_P is true escaped local memory is also considered
   global.  */

bool
ptr_deref_may_alias_global_p (tree ptr, bool escaped_local_p)
{
  struct ptr_info_def *pi;

  /* If we end up with a pointer constant here that may point
     to global memory.  */
  if (TREE_CODE (ptr) != SSA_NAME)
    return true;

  pi = SSA_NAME_PTR_INFO (ptr);

  /* If we do not have points-to information for this variable,
     we have to punt.  */
  if (!pi)
    return true;

  /* ???  This does not use TBAA to prune globals ptr may not access.  */
  return pt_solution_includes_global (&pi->pt, escaped_local_p);
}

/* Return true if dereferencing PTR may alias DECL.
   The caller is responsible for applying TBAA to see if PTR
   may access DECL at all.  */

static bool
ptr_deref_may_alias_decl_p (tree ptr, tree decl)
{
  struct ptr_info_def *pi;

  /* Conversions are irrelevant for points-to information and
     data-dependence analysis can feed us those.  */
  STRIP_NOPS (ptr);

  /* Anything we do not explicilty handle aliases.  */
  if ((TREE_CODE (ptr) != SSA_NAME
       && TREE_CODE (ptr) != ADDR_EXPR
       && TREE_CODE (ptr) != POINTER_PLUS_EXPR)
      || !POINTER_TYPE_P (TREE_TYPE (ptr))
      || (!VAR_P (decl)
	  && TREE_CODE (decl) != PARM_DECL
	  && TREE_CODE (decl) != RESULT_DECL))
    return true;

  /* Disregard pointer offsetting.  */
  if (TREE_CODE (ptr) == POINTER_PLUS_EXPR)
    {
      do
	{
	  ptr = TREE_OPERAND (ptr, 0);
	}
      while (TREE_CODE (ptr) == POINTER_PLUS_EXPR);
      return ptr_deref_may_alias_decl_p (ptr, decl);
    }

  /* ADDR_EXPR pointers either just offset another pointer or directly
     specify the pointed-to set.  */
  if (TREE_CODE (ptr) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (ptr, 0));
      if (base
	  && (TREE_CODE (base) == MEM_REF
	      || TREE_CODE (base) == TARGET_MEM_REF))
	ptr = TREE_OPERAND (base, 0);
      else if (base
	       && DECL_P (base))
	return compare_base_decls (base, decl) != 0;
      else if (base
	       && CONSTANT_CLASS_P (base))
	return false;
      else
	return true;
    }

  /* Non-aliased variables cannot be pointed to.  */
  if (!may_be_aliased (decl))
    return false;

  /* From here we require a SSA name pointer.  Anything else aliases.  */
  if (TREE_CODE (ptr) != SSA_NAME
      || !POINTER_TYPE_P (TREE_TYPE (ptr)))
    return true;

  /* If we do not have useful points-to information for this pointer
     we cannot disambiguate anything else.  */
  pi = SSA_NAME_PTR_INFO (ptr);
  if (!pi)
    return true;

  return pt_solution_includes (&pi->pt, decl);
}

/* Return true if dereferenced PTR1 and PTR2 may alias.
   The caller is responsible for applying TBAA to see if accesses
   through PTR1 and PTR2 may conflict at all.  */

bool
ptr_derefs_may_alias_p (tree ptr1, tree ptr2)
{
  struct ptr_info_def *pi1, *pi2;

  /* Conversions are irrelevant for points-to information and
     data-dependence analysis can feed us those.  */
  STRIP_NOPS (ptr1);
  STRIP_NOPS (ptr2);

  /* Disregard pointer offsetting.  */
  if (TREE_CODE (ptr1) == POINTER_PLUS_EXPR)
    {
      do
	{
	  ptr1 = TREE_OPERAND (ptr1, 0);
	}
      while (TREE_CODE (ptr1) == POINTER_PLUS_EXPR);
      return ptr_derefs_may_alias_p (ptr1, ptr2);
    }
  if (TREE_CODE (ptr2) == POINTER_PLUS_EXPR)
    {
      do
	{
	  ptr2 = TREE_OPERAND (ptr2, 0);
	}
      while (TREE_CODE (ptr2) == POINTER_PLUS_EXPR);
      return ptr_derefs_may_alias_p (ptr1, ptr2);
    }

  /* ADDR_EXPR pointers either just offset another pointer or directly
     specify the pointed-to set.  */
  if (TREE_CODE (ptr1) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (ptr1, 0));
      if (base
	  && (TREE_CODE (base) == MEM_REF
	      || TREE_CODE (base) == TARGET_MEM_REF))
	return ptr_derefs_may_alias_p (TREE_OPERAND (base, 0), ptr2);
      else if (base
	       && DECL_P (base))
	return ptr_deref_may_alias_decl_p (ptr2, base);
      /* Try ptr2 when ptr1 points to a constant.  */
      else if (base
	       && !CONSTANT_CLASS_P (base))
	return true;
    }
  if (TREE_CODE (ptr2) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (ptr2, 0));
      if (base
	  && (TREE_CODE (base) == MEM_REF
	      || TREE_CODE (base) == TARGET_MEM_REF))
	return ptr_derefs_may_alias_p (ptr1, TREE_OPERAND (base, 0));
      else if (base
	       && DECL_P (base))
	return ptr_deref_may_alias_decl_p (ptr1, base);
      else
	return true;
    }

  /* From here we require SSA name pointers.  Anything else aliases.  */
  if (TREE_CODE (ptr1) != SSA_NAME
      || TREE_CODE (ptr2) != SSA_NAME
      || !POINTER_TYPE_P (TREE_TYPE (ptr1))
      || !POINTER_TYPE_P (TREE_TYPE (ptr2)))
    return true;

  /* We may end up with two empty points-to solutions for two same pointers.
     In this case we still want to say both pointers alias, so shortcut
     that here.  */
  if (ptr1 == ptr2)
    return true;

  /* If we do not have useful points-to information for either pointer
     we cannot disambiguate anything else.  */
  pi1 = SSA_NAME_PTR_INFO (ptr1);
  pi2 = SSA_NAME_PTR_INFO (ptr2);
  if (!pi1 || !pi2)
    return true;

  /* ???  This does not use TBAA to prune decls from the intersection
     that not both pointers may access.  */
  return pt_solutions_intersect (&pi1->pt, &pi2->pt);
}

/* Return true if dereferencing PTR may alias *REF.
   The caller is responsible for applying TBAA to see if PTR
   may access *REF at all.  */

static bool
ptr_deref_may_alias_ref_p_1 (tree ptr, ao_ref *ref)
{
  tree base = ao_ref_base (ref);

  if (TREE_CODE (base) == MEM_REF
      || TREE_CODE (base) == TARGET_MEM_REF)
    return ptr_derefs_may_alias_p (ptr, TREE_OPERAND (base, 0));
  else if (DECL_P (base))
    return ptr_deref_may_alias_decl_p (ptr, base);

  return true;
}

/* Returns true if PTR1 and PTR2 compare unequal because of points-to.  */

bool
ptrs_compare_unequal (tree ptr1, tree ptr2)
{
  /* First resolve the pointers down to a SSA name pointer base or
     a VAR_DECL, PARM_DECL or RESULT_DECL.  This explicitely does
     not yet try to handle LABEL_DECLs, FUNCTION_DECLs, CONST_DECLs
     or STRING_CSTs which needs points-to adjustments to track them
     in the points-to sets.  */
  tree obj1 = NULL_TREE;
  tree obj2 = NULL_TREE;
  if (TREE_CODE (ptr1) == ADDR_EXPR)
    {
      tree tem = get_base_address (TREE_OPERAND (ptr1, 0));
      if (! tem)
	return false;
      if (VAR_P (tem)
	  || TREE_CODE (tem) == PARM_DECL
	  || TREE_CODE (tem) == RESULT_DECL)
	obj1 = tem;
      else if (TREE_CODE (tem) == MEM_REF)
	ptr1 = TREE_OPERAND (tem, 0);
    }
  if (TREE_CODE (ptr2) == ADDR_EXPR)
    {
      tree tem = get_base_address (TREE_OPERAND (ptr2, 0));
      if (! tem)
	return false;
      if (VAR_P (tem)
	  || TREE_CODE (tem) == PARM_DECL
	  || TREE_CODE (tem) == RESULT_DECL)
	obj2 = tem;
      else if (TREE_CODE (tem) == MEM_REF)
	ptr2 = TREE_OPERAND (tem, 0);
    }

  /* Canonicalize ptr vs. object.  */
  if (TREE_CODE (ptr1) == SSA_NAME && obj2)
    {
      std::swap (ptr1, ptr2);
      std::swap (obj1, obj2);
    }

  if (obj1 && obj2)
    /* Other code handles this correctly, no need to duplicate it here.  */;
  else if (obj1 && TREE_CODE (ptr2) == SSA_NAME)
    {
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr2);
      /* We may not use restrict to optimize pointer comparisons.
         See PR71062.  So we have to assume that restrict-pointed-to
	 may be in fact obj1.  */
      if (!pi
	  || pi->pt.vars_contains_restrict
	  || pi->pt.vars_contains_interposable)
	return false;
      if (VAR_P (obj1)
	  && (TREE_STATIC (obj1) || DECL_EXTERNAL (obj1)))
	{
	  varpool_node *node = varpool_node::get (obj1);
	  /* If obj1 may bind to NULL give up (see below).  */
	  if (! node
	      || ! node->nonzero_address ()
	      || ! decl_binds_to_current_def_p (obj1))
	    return false;
	}
      return !pt_solution_includes (&pi->pt, obj1);
    }
  else if (TREE_CODE (ptr1) == SSA_NAME)
    {
      struct ptr_info_def *pi1 = SSA_NAME_PTR_INFO (ptr1);
      if (!pi1
	  || pi1->pt.vars_contains_restrict
	  || pi1->pt.vars_contains_interposable)
	return false;
      if (integer_zerop (ptr2) && !pi1->pt.null)
	return true;
      if (TREE_CODE (ptr2) == SSA_NAME)
	{
	  struct ptr_info_def *pi2 = SSA_NAME_PTR_INFO (ptr2);
	  if (!pi2
	      || pi2->pt.vars_contains_restrict
	      || pi2->pt.vars_contains_interposable)
	    return false;
	  if ((!pi1->pt.null || !pi2->pt.null)
	      /* ???  We do not represent FUNCTION_DECL and LABEL_DECL
		 in pt.vars but only set pt.vars_contains_nonlocal.  This
		 makes compares involving those and other nonlocals
		 imprecise.  */
	      && (!pi1->pt.vars_contains_nonlocal
		  || !pi2->pt.vars_contains_nonlocal)
	      && (!pt_solution_includes_const_pool (&pi1->pt)
		  || !pt_solution_includes_const_pool (&pi2->pt)))
	    return !pt_solutions_intersect (&pi1->pt, &pi2->pt);
	}
    }

  return false;
}

/* Returns whether reference REF to BASE may refer to global memory.
   When ESCAPED_LOCAL_P is true escaped local memory is also considered
   global.  */

static bool
ref_may_alias_global_p_1 (tree base, bool escaped_local_p)
{
  if (DECL_P (base))
    return (is_global_var (base)
	    || (escaped_local_p
		&& pt_solution_includes (&cfun->gimple_df->escaped_return,
					 base)));
  else if (TREE_CODE (base) == MEM_REF
	   || TREE_CODE (base) == TARGET_MEM_REF)
    return ptr_deref_may_alias_global_p (TREE_OPERAND (base, 0),
					 escaped_local_p);
  return true;
}

bool
ref_may_alias_global_p (ao_ref *ref, bool escaped_local_p)
{
  tree base = ao_ref_base (ref);
  return ref_may_alias_global_p_1 (base, escaped_local_p);
}

bool
ref_may_alias_global_p (tree ref, bool escaped_local_p)
{
  tree base = get_base_address (ref);
  return ref_may_alias_global_p_1 (base, escaped_local_p);
}

/* Return true whether STMT may clobber global memory.
   When ESCAPED_LOCAL_P is true escaped local memory is also considered
   global.  */

bool
stmt_may_clobber_global_p (gimple *stmt, bool escaped_local_p)
{
  tree lhs;

  if (!gimple_vdef (stmt))
    return false;

  /* ???  We can ask the oracle whether an artificial pointer
     dereference with a pointer with points-to information covering
     all global memory (what about non-address taken memory?) maybe
     clobbered by this call.  As there is at the moment no convenient
     way of doing that without generating garbage do some manual
     checking instead.
     ???  We could make a NULL ao_ref argument to the various
     predicates special, meaning any global memory.  */

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      lhs = gimple_assign_lhs (stmt);
      return (TREE_CODE (lhs) != SSA_NAME
	      && ref_may_alias_global_p (lhs, escaped_local_p));
    case GIMPLE_CALL:
      return true;
    default:
      return true;
    }
}


/* Dump alias information on FILE.  */

void
dump_alias_info (FILE *file)
{
  unsigned i;
  tree ptr;
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);
  tree var;

  fprintf (file, "\n\nAlias information for %s\n\n", funcname);

  fprintf (file, "Aliased symbols\n\n");

  FOR_EACH_LOCAL_DECL (cfun, i, var)
    {
      if (may_be_aliased (var))
	dump_variable (file, var);
    }

  fprintf (file, "\nCall clobber information\n");

  fprintf (file, "\nESCAPED");
  dump_points_to_solution (file, &cfun->gimple_df->escaped);

  fprintf (file, "\nESCAPED_RETURN");
  dump_points_to_solution (file, &cfun->gimple_df->escaped_return);

  fprintf (file, "\n\nFlow-insensitive points-to information\n\n");

  FOR_EACH_SSA_NAME (i, ptr, cfun)
    {
      struct ptr_info_def *pi;

      if (!POINTER_TYPE_P (TREE_TYPE (ptr))
	  || SSA_NAME_IN_FREE_LIST (ptr))
	continue;

      pi = SSA_NAME_PTR_INFO (ptr);
      if (pi)
	dump_points_to_info_for (file, ptr);
    }

  fprintf (file, "\n");
}


/* Dump alias information on stderr.  */

DEBUG_FUNCTION void
debug_alias_info (void)
{
  dump_alias_info (stderr);
}


/* Dump the points-to set *PT into FILE.  */

void
dump_points_to_solution (FILE *file, struct pt_solution *pt)
{
  if (pt->anything)
    fprintf (file, ", points-to anything");

  if (pt->nonlocal)
    fprintf (file, ", points-to non-local");

  if (pt->escaped)
    fprintf (file, ", points-to escaped");

  if (pt->ipa_escaped)
    fprintf (file, ", points-to unit escaped");

  if (pt->null)
    fprintf (file, ", points-to NULL");

  if (pt->const_pool)
    fprintf (file, ", points-to const-pool");

  if (pt->vars)
    {
      fprintf (file, ", points-to vars: ");
      dump_decl_set (file, pt->vars);
      if (pt->vars_contains_nonlocal
	  || pt->vars_contains_escaped
	  || pt->vars_contains_escaped_heap
	  || pt->vars_contains_restrict
	  || pt->vars_contains_interposable)
	{
	  const char *comma = "";
	  fprintf (file, " (");
	  if (pt->vars_contains_nonlocal)
	    {
	      fprintf (file, "nonlocal");
	      comma = ", ";
	    }
	  if (pt->vars_contains_escaped)
	    {
	      fprintf (file, "%sescaped", comma);
	      comma = ", ";
	    }
	  if (pt->vars_contains_escaped_heap)
	    {
	      fprintf (file, "%sescaped heap", comma);
	      comma = ", ";
	    }
	  if (pt->vars_contains_restrict)
	    {
	      fprintf (file, "%srestrict", comma);
	      comma = ", ";
	    }
	  if (pt->vars_contains_interposable)
	    fprintf (file, "%sinterposable", comma);
	  fprintf (file, ")");
	}
    }
}


/* Unified dump function for pt_solution.  */

DEBUG_FUNCTION void
debug (pt_solution &ref)
{
  dump_points_to_solution (stderr, &ref);
}

DEBUG_FUNCTION void
debug (pt_solution *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Dump points-to information for SSA_NAME PTR into FILE.  */

void
dump_points_to_info_for (FILE *file, tree ptr)
{
  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);

  print_generic_expr (file, ptr, dump_flags);

  if (pi)
    dump_points_to_solution (file, &pi->pt);
  else
    fprintf (file, ", points-to anything");

  fprintf (file, "\n");
}


/* Dump points-to information for VAR into stderr.  */

DEBUG_FUNCTION void
debug_points_to_info_for (tree var)
{
  dump_points_to_info_for (stderr, var);
}


/* Initializes the alias-oracle reference representation *R from REF.  */

void
ao_ref_init (ao_ref *r, tree ref)
{
  r->ref = ref;
  r->base = NULL_TREE;
  r->offset = 0;
  r->size = -1;
  r->max_size = -1;
  r->ref_alias_set = -1;
  r->base_alias_set = -1;
  r->volatile_p = ref ? TREE_THIS_VOLATILE (ref) : false;
}

/* Returns the base object of the memory reference *REF.  */

tree
ao_ref_base (ao_ref *ref)
{
  bool reverse;

  if (ref->base)
    return ref->base;
  ref->base = get_ref_base_and_extent (ref->ref, &ref->offset, &ref->size,
				       &ref->max_size, &reverse);
  return ref->base;
}

/* Returns the base object alias set of the memory reference *REF.  */

alias_set_type
ao_ref_base_alias_set (ao_ref *ref)
{
  tree base_ref;
  if (ref->base_alias_set != -1)
    return ref->base_alias_set;
  if (!ref->ref)
    return 0;
  base_ref = ref->ref;
  if (TREE_CODE (base_ref) == WITH_SIZE_EXPR)
    base_ref = TREE_OPERAND (base_ref, 0);
  while (handled_component_p (base_ref))
    base_ref = TREE_OPERAND (base_ref, 0);
  ref->base_alias_set = get_alias_set (base_ref);
  return ref->base_alias_set;
}

/* Returns the reference alias set of the memory reference *REF.  */

alias_set_type
ao_ref_alias_set (ao_ref *ref)
{
  if (ref->ref_alias_set != -1)
    return ref->ref_alias_set;
  if (!ref->ref)
    return 0;
  ref->ref_alias_set = get_alias_set (ref->ref);
  return ref->ref_alias_set;
}

/* Returns a type satisfying
   get_deref_alias_set (type) == ao_ref_base_alias_set (REF).  */

tree
ao_ref_base_alias_ptr_type (ao_ref *ref)
{
  tree base_ref;

  if (!ref->ref)
    return NULL_TREE;
  base_ref = ref->ref;
  if (TREE_CODE (base_ref) == WITH_SIZE_EXPR)
    base_ref = TREE_OPERAND (base_ref, 0);
  while (handled_component_p (base_ref))
    base_ref = TREE_OPERAND (base_ref, 0);
  tree ret = reference_alias_ptr_type (base_ref);
  return ret;
}

/* Returns a type satisfying
   get_deref_alias_set (type) == ao_ref_alias_set (REF).  */

tree
ao_ref_alias_ptr_type (ao_ref *ref)
{
  if (!ref->ref)
    return NULL_TREE;
  tree ret = reference_alias_ptr_type (ref->ref);
  return ret;
}

/* Return the alignment of the access *REF and store it in the *ALIGN
   and *BITPOS pairs.  Returns false if no alignment could be determined.
   See get_object_alignment_2 for details.  */

bool
ao_ref_alignment (ao_ref *ref, unsigned int *align,
		  unsigned HOST_WIDE_INT *bitpos)
{
  if (ref->ref)
    return get_object_alignment_1 (ref->ref, align, bitpos);

  /* When we just have ref->base we cannot use get_object_alignment since
     that will eventually use the type of the appearant access while for
     example ao_ref_init_from_ptr_and_range is not careful to adjust that.  */
  *align = BITS_PER_UNIT;
  HOST_WIDE_INT offset;
  if (!ref->offset.is_constant (&offset)
      || !get_object_alignment_2 (ref->base, align, bitpos, true))
    return false;
  *bitpos += (unsigned HOST_WIDE_INT)offset * BITS_PER_UNIT;
  *bitpos = *bitpos & (*align - 1);
  return true;
}

/* Init an alias-oracle reference representation from a gimple pointer
   PTR a range specified by OFFSET, SIZE and MAX_SIZE under the assumption
   that RANGE_KNOWN is set.

   The access is assumed to be only to or after of the pointer target adjusted
   by the offset, not before it (even in the case RANGE_KNOWN is false).  */

void
ao_ref_init_from_ptr_and_range (ao_ref *ref, tree ptr,
				bool range_known,
				poly_int64 offset,
				poly_int64 size,
				poly_int64 max_size)
{
  poly_int64 t, extra_offset = 0;

  ref->ref = NULL_TREE;
  if (TREE_CODE (ptr) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (ptr);
      if (gimple_assign_single_p (stmt)
	  && gimple_assign_rhs_code (stmt) == ADDR_EXPR)
	ptr = gimple_assign_rhs1 (stmt);
      else if (is_gimple_assign (stmt)
	       && gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
	       && ptrdiff_tree_p (gimple_assign_rhs2 (stmt), &extra_offset))
	{
	  ptr = gimple_assign_rhs1 (stmt);
	  extra_offset *= BITS_PER_UNIT;
	}
    }

  if (TREE_CODE (ptr) == ADDR_EXPR)
    {
      ref->base = get_addr_base_and_unit_offset (TREE_OPERAND (ptr, 0), &t);
      if (ref->base
	  && coeffs_in_range_p (t, -HOST_WIDE_INT_MAX / BITS_PER_UNIT,
				HOST_WIDE_INT_MAX / BITS_PER_UNIT))
	ref->offset = BITS_PER_UNIT * t;
      else
	{
	  range_known = false;
	  ref->offset = 0;
	  ref->base = get_base_address (TREE_OPERAND (ptr, 0));
	}
    }
  else
    {
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
      ref->base = build2 (MEM_REF, char_type_node,
			  ptr, null_pointer_node);
      ref->offset = 0;
    }
  ref->offset += extra_offset + offset;
  if (range_known)
    {
      ref->max_size = max_size;
      ref->size = size;
    }
  else
    ref->max_size = ref->size = -1;
  ref->ref_alias_set = 0;
  ref->base_alias_set = 0;
  ref->volatile_p = false;
}

/* Init an alias-oracle reference representation from a gimple pointer
   PTR and a gimple size SIZE in bytes.  If SIZE is NULL_TREE then the
   size is assumed to be unknown.  The access is assumed to be only
   to or after of the pointer target, not before it.  */

void
ao_ref_init_from_ptr_and_size (ao_ref *ref, tree ptr, tree size)
{
  poly_int64 size_hwi;
  if (size
      && poly_int_tree_p (size, &size_hwi)
      && coeffs_in_range_p (size_hwi, 0, HOST_WIDE_INT_MAX / BITS_PER_UNIT))
    {
      size_hwi = size_hwi * BITS_PER_UNIT;
      ao_ref_init_from_ptr_and_range (ref, ptr, true, 0, size_hwi, size_hwi);
    }
  else
    ao_ref_init_from_ptr_and_range (ref, ptr, false, 0, -1, -1);
}

/* S1 and S2 are TYPE_SIZE or DECL_SIZE.  Compare them:
   Return -1 if S1 < S2
   Return 1 if S1 > S2
   Return 0 if equal or incomparable.  */

static int
compare_sizes (tree s1, tree s2)
{
  if (!s1 || !s2)
    return 0;

  poly_uint64 size1;
  poly_uint64 size2;

  if (!poly_int_tree_p (s1, &size1) || !poly_int_tree_p (s2, &size2))
    return 0;
  if (known_lt (size1, size2))
    return -1;
  if (known_lt (size2, size1))
    return 1;
  return 0;
}

/* Compare TYPE1 and TYPE2 by its size.
   Return -1 if size of TYPE1 < size of TYPE2
   Return 1 if size of TYPE1 > size of TYPE2
   Return 0 if types are of equal sizes or we can not compare them.  */

static int
compare_type_sizes (tree type1, tree type2)
{
  /* Be conservative for arrays and vectors.  We want to support partial
     overlap on int[3] and int[3] as tested in gcc.dg/torture/alias-2.c.  */
  while (TREE_CODE (type1) == ARRAY_TYPE
	 || VECTOR_TYPE_P (type1))
    type1 = TREE_TYPE (type1);
  while (TREE_CODE (type2) == ARRAY_TYPE
	 || VECTOR_TYPE_P (type2))
    type2 = TREE_TYPE (type2);
  return compare_sizes (TYPE_SIZE (type1), TYPE_SIZE (type2));
}

/* Return 1 if TYPE1 and TYPE2 are to be considered equivalent for the
   purpose of TBAA.  Return 0 if they are distinct and -1 if we cannot
   decide.  */

static inline int
same_type_for_tbaa (tree type1, tree type2)
{
  type1 = TYPE_MAIN_VARIANT (type1);
  type2 = TYPE_MAIN_VARIANT (type2);

  /* Handle the most common case first.  */
  if (type1 == type2)
    return 1;

  /* If we would have to do structural comparison bail out.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (type1)
      || TYPE_STRUCTURAL_EQUALITY_P (type2))
    return -1;

  /* Compare the canonical types.  */
  if (TYPE_CANONICAL (type1) == TYPE_CANONICAL (type2))
    return 1;

  /* ??? Array types are not properly unified in all cases as we have
     spurious changes in the index types for example.  Removing this
     causes all sorts of problems with the Fortran frontend.  */
  if (TREE_CODE (type1) == ARRAY_TYPE
      && TREE_CODE (type2) == ARRAY_TYPE)
    return -1;

  /* ??? In Ada, an lvalue of an unconstrained type can be used to access an
     object of one of its constrained subtypes, e.g. when a function with an
     unconstrained parameter passed by reference is called on an object and
     inlined.  But, even in the case of a fixed size, type and subtypes are
     not equivalent enough as to share the same TYPE_CANONICAL, since this
     would mean that conversions between them are useless, whereas they are
     not (e.g. type and subtypes can have different modes).  So, in the end,
     they are only guaranteed to have the same alias set.  */
  alias_set_type set1 = get_alias_set (type1);
  alias_set_type set2 = get_alias_set (type2);
  if (set1 == set2)
    return -1;

  /* Pointers to void are considered compatible with all other pointers,
     so for two pointers see what the alias set resolution thinks.  */
  if (POINTER_TYPE_P (type1)
      && POINTER_TYPE_P (type2)
      && alias_sets_conflict_p (set1, set2))
    return -1;

  /* The types are known to be not equal.  */
  return 0;
}

/* Return true if TYPE is a composite type (i.e. we may apply one of handled
   components on it).  */

static bool
type_has_components_p (tree type)
{
  return AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type)
	 || TREE_CODE (type) == COMPLEX_TYPE;
}

/* MATCH1 and MATCH2 which are part of access path of REF1 and REF2
   respectively are either pointing to same address or are completely
   disjoint. If PARTIAL_OVERLAP is true, assume that outermost arrays may
   just partly overlap.

   Try to disambiguate using the access path starting from the match
   and return false if there is no conflict.

   Helper for aliasing_component_refs_p.  */

static bool
aliasing_matching_component_refs_p (tree match1, tree ref1,
				    poly_int64 offset1, poly_int64 max_size1,
				    tree match2, tree ref2,
				    poly_int64 offset2, poly_int64 max_size2,
				    bool partial_overlap)
{
  poly_int64 offadj, sztmp, msztmp;
  bool reverse;

  if (!partial_overlap)
    {
      get_ref_base_and_extent (match2, &offadj, &sztmp, &msztmp, &reverse);
      offset2 -= offadj;
      get_ref_base_and_extent (match1, &offadj, &sztmp, &msztmp, &reverse);
      offset1 -= offadj;
      if (!ranges_maybe_overlap_p (offset1, max_size1, offset2, max_size2))
	{
	  ++alias_stats.aliasing_component_refs_p_no_alias;
	  return false;
	}
    }

  int cmp = nonoverlapping_refs_since_match_p (match1, ref1, match2, ref2,
					       partial_overlap);
  if (cmp == 1
      || (cmp == -1 && nonoverlapping_component_refs_p (ref1, ref2)))
    {
      ++alias_stats.aliasing_component_refs_p_no_alias;
      return false;
    }
  ++alias_stats.aliasing_component_refs_p_may_alias;
  return true;
}

/* Return true if REF is reference to zero sized trailing array. I.e.
   struct foo {int bar; int array[0];} *fooptr;
   fooptr->array.  */

static bool
component_ref_to_zero_sized_trailing_array_p (tree ref)
{
  return (TREE_CODE (ref) == COMPONENT_REF
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (ref, 1))) == ARRAY_TYPE
	  && (!TYPE_SIZE (TREE_TYPE (TREE_OPERAND (ref, 1)))
	      || integer_zerop (TYPE_SIZE (TREE_TYPE (TREE_OPERAND (ref, 1)))))
	  && array_ref_flexible_size_p (ref));
}

/* Worker for aliasing_component_refs_p. Most parameters match parameters of
   aliasing_component_refs_p.

   Walk access path REF2 and try to find type matching TYPE1
   (which is a start of possibly aliasing access path REF1).
   If match is found, try to disambiguate.

   Return 0 for sucessful disambiguation.
   Return 1 if match was found but disambiguation failed
   Return -1 if there is no match.
   In this case MAYBE_MATCH is set to 0 if there is no type matching TYPE1
   in access patch REF2 and -1 if we are not sure.  */

static int
aliasing_component_refs_walk (tree ref1, tree type1, tree base1,
			      poly_int64 offset1, poly_int64 max_size1,
			      tree end_struct_ref1,
			      tree ref2, tree base2,
			      poly_int64 offset2, poly_int64 max_size2,
			      bool *maybe_match)
{
  tree ref = ref2;
  int same_p = 0;

  while (true)
    {
      /* We walk from inner type to the outer types. If type we see is
	 already too large to be part of type1, terminate the search.  */
      int cmp = compare_type_sizes (type1, TREE_TYPE (ref));

      if (cmp < 0
	  && (!end_struct_ref1
	      || compare_type_sizes (TREE_TYPE (end_struct_ref1),
				     TREE_TYPE (ref)) < 0))
	break;
      /* If types may be of same size, see if we can decide about their
	 equality.  */
      if (cmp == 0)
	{
	  same_p = same_type_for_tbaa (TREE_TYPE (ref), type1);
	  if (same_p == 1)
	    break;
	  /* In case we can't decide whether types are same try to
	     continue looking for the exact match.
	     Remember however that we possibly saw a match
	     to bypass the access path continuations tests we do later.  */
	  if (same_p == -1)
	    *maybe_match = true;
	}
      if (!handled_component_p (ref))
	break;
      ref = TREE_OPERAND (ref, 0);
    }
  if (same_p == 1)
    {
      bool partial_overlap = false;

      /* We assume that arrays can overlap by multiple of their elements
	 size as tested in gcc.dg/torture/alias-2.c.
	 This partial overlap happen only when both arrays are bases of
	 the access and not contained within another component ref.
	 To be safe we also assume partial overlap for VLAs. */
      if (TREE_CODE (TREE_TYPE (base1)) == ARRAY_TYPE
	  && (!TYPE_SIZE (TREE_TYPE (base1))
	      || TREE_CODE (TYPE_SIZE (TREE_TYPE (base1))) != INTEGER_CST
	      || ref == base2))
	{
	  /* Setting maybe_match to true triggers
	     nonoverlapping_component_refs_p test later that still may do
	     useful disambiguation.  */
	  *maybe_match = true;
	  partial_overlap = true;
	}
      return aliasing_matching_component_refs_p (base1, ref1,
						 offset1, max_size1,
						 ref, ref2,
						 offset2, max_size2,
						 partial_overlap);
    }
  return -1;
}

/* Consider access path1 base1....ref1 and access path2 base2...ref2.
   Return true if they can be composed to single access path
   base1...ref1...base2...ref2.

   REF_TYPE1 if type of REF1.  END_STRUCT_PAST_END1 is true if there is
   a trailing array access after REF1 in the non-TBAA part of the access.
   REF1_ALIAS_SET is the alias set of REF1.

   BASE_TYPE2 is type of base2.  END_STRUCT_REF2 is non-NULL if there is
   a trailing array access in the TBAA part of access path2.
   BASE2_ALIAS_SET is the alias set of base2.  */

bool
access_path_may_continue_p (tree ref_type1, bool end_struct_past_end1,
			    alias_set_type ref1_alias_set,
			    tree base_type2, tree end_struct_ref2,
			    alias_set_type base2_alias_set)
{
  /* Access path can not continue past types with no components.  */
  if (!type_has_components_p (ref_type1))
    return false;

  /* If first access path ends by too small type to hold base of
     the second access path, typically paths can not continue.

     Punt if end_struct_past_end1 is true.  We want to support arbitrary
     type puning past first COMPONENT_REF to union because redundant store
     elimination depends on this, see PR92152.  For this reason we can not
     check size of the reference because types may partially overlap.  */
  if (!end_struct_past_end1)
    {
      if (compare_type_sizes (ref_type1, base_type2) < 0)
	return false;
      /* If the path2 contains trailing array access we can strenghten the check
	 to verify that also the size of element of the trailing array fits.
	 In fact we could check for offset + type_size, but we do not track
	 offsets and this is quite side case.  */
      if (end_struct_ref2
	  && compare_type_sizes (ref_type1, TREE_TYPE (end_struct_ref2)) < 0)
	return false;
    }
  return (base2_alias_set == ref1_alias_set
	  || alias_set_subset_of (base2_alias_set, ref1_alias_set));
}

/* Determine if the two component references REF1 and REF2 which are
   based on access types TYPE1 and TYPE2 and of which at least one is based
   on an indirect reference may alias.
   REF1_ALIAS_SET, BASE1_ALIAS_SET, REF2_ALIAS_SET and BASE2_ALIAS_SET
   are the respective alias sets.  */

static bool
aliasing_component_refs_p (tree ref1,
			   alias_set_type ref1_alias_set,
			   alias_set_type base1_alias_set,
			   poly_int64 offset1, poly_int64 max_size1,
			   tree ref2,
			   alias_set_type ref2_alias_set,
			   alias_set_type base2_alias_set,
			   poly_int64 offset2, poly_int64 max_size2)
{
  /* If one reference is a component references through pointers try to find a
     common base and apply offset based disambiguation.  This handles
     for example
       struct A { int i; int j; } *q;
       struct B { struct A a; int k; } *p;
     disambiguating q->i and p->a.j.  */
  tree base1, base2;
  tree type1, type2;
  bool maybe_match = false;
  tree end_struct_ref1 = NULL, end_struct_ref2 = NULL;
  bool end_struct_past_end1 = false;
  bool end_struct_past_end2 = false;

  /* Choose bases and base types to search for.
     The access path is as follows:
       base....end_of_tbaa_ref...actual_ref
     At one place in the access path may be a reference to zero sized or
     trailing array.

     We generally discard the segment after end_of_tbaa_ref however
     we need to be careful in case it contains zero sized or trailing array.
     These may happen after reference to union and in this case we need to
     not disambiguate type puning scenarios.

     We set:
	base1 to point to base

	ref1 to point to end_of_tbaa_ref

	end_struct_ref1 to point the trailing reference (if it exists
 	in range base....end_of_tbaa_ref

	end_struct_past_end1 is true if this trailing reference occurs in
	end_of_tbaa_ref...actual_ref.  */
  base1 = ref1;
  while (handled_component_p (base1))
    {
      /* Generally access paths are monotous in the size of object. The
	 exception are trailing arrays of structures. I.e.
	   struct a {int array[0];};
	 or
	   struct a {int array1[0]; int array[];};
	 Such struct has size 0 but accesses to a.array may have non-zero size.
	 In this case the size of TREE_TYPE (base1) is smaller than
	 size of TREE_TYPE (TREE_OPERAND (base1, 0)).

	 Because we compare sizes of arrays just by sizes of their elements,
	 we only need to care about zero sized array fields here.  */
      if (component_ref_to_zero_sized_trailing_array_p (base1))
	{
	  gcc_checking_assert (!end_struct_ref1);
          end_struct_ref1 = base1;
	}
      if (ends_tbaa_access_path_p (base1))
	{
	  ref1 = TREE_OPERAND (base1, 0);
	  if (end_struct_ref1)
	    {
	      end_struct_past_end1 = true;
	      end_struct_ref1 = NULL;
	    }
	}
      base1 = TREE_OPERAND (base1, 0);
    }
  type1 = TREE_TYPE (base1);
  base2 = ref2;
  while (handled_component_p (base2))
    {
      if (component_ref_to_zero_sized_trailing_array_p (base2))
	{
	  gcc_checking_assert (!end_struct_ref2);
	  end_struct_ref2 = base2;
	}
      if (ends_tbaa_access_path_p (base2))
	{
	  ref2 = TREE_OPERAND (base2, 0);
	  if (end_struct_ref2)
	    {
	      end_struct_past_end2 = true;
	      end_struct_ref2 = NULL;
	    }
	}
      base2 = TREE_OPERAND (base2, 0);
    }
  type2 = TREE_TYPE (base2);

  /* Now search for the type1 in the access path of ref2.  This
     would be a common base for doing offset based disambiguation on.
     This however only makes sense if type2 is big enough to hold type1.  */
  int cmp_outer = compare_type_sizes (type2, type1);

  /* If type2 is big enough to contain type1 walk its access path.
     We also need to care of arrays at the end of structs that may extend
     beyond the end of structure.  If this occurs in the TBAA part of the
     access path, we need to consider the increased type as well.  */
  if (cmp_outer >= 0
      || (end_struct_ref2
	  && compare_type_sizes (TREE_TYPE (end_struct_ref2), type1) >= 0))
    {
      int res = aliasing_component_refs_walk (ref1, type1, base1,
					      offset1, max_size1,
					      end_struct_ref1,
					      ref2, base2, offset2, max_size2,
					      &maybe_match);
      if (res != -1)
	return res;
    }

  /* If we didn't find a common base, try the other way around.  */
  if (cmp_outer <= 0
      || (end_struct_ref1
	  && compare_type_sizes (TREE_TYPE (end_struct_ref1), type2) <= 0))
    {
      int res = aliasing_component_refs_walk (ref2, type2, base2,
					      offset2, max_size2,
					      end_struct_ref2,
					      ref1, base1, offset1, max_size1,
					      &maybe_match);
      if (res != -1)
	return res;
    }

  /* In the following code we make an assumption that the types in access
     paths do not overlap and thus accesses alias only if one path can be
     continuation of another.  If we was not able to decide about equivalence,
     we need to give up.  */
  if (maybe_match)
    {
      if (!nonoverlapping_component_refs_p (ref1, ref2))
	{
	  ++alias_stats.aliasing_component_refs_p_may_alias;
	  return true;
	}
      ++alias_stats.aliasing_component_refs_p_no_alias;
      return false;
    }

  if (access_path_may_continue_p (TREE_TYPE (ref1), end_struct_past_end1,
				  ref1_alias_set,
				  type2, end_struct_ref2,
				  base2_alias_set)
      || access_path_may_continue_p (TREE_TYPE (ref2), end_struct_past_end2,
				     ref2_alias_set,
				     type1, end_struct_ref1,
				     base1_alias_set))
    {
      ++alias_stats.aliasing_component_refs_p_may_alias;
      return true;
    }
  ++alias_stats.aliasing_component_refs_p_no_alias;
  return false;
}

/* FIELD1 and FIELD2 are two fields of component refs.  We assume
   that bases of both component refs are either equivalent or nonoverlapping.
   We do not assume that the containers of FIELD1 and FIELD2 are of the
   same type or size.

   Return 0 in case the base address of component_refs are same then
   FIELD1 and FIELD2 have same address. Note that FIELD1 and FIELD2
   may not be of same type or size.

   Return 1 if FIELD1 and FIELD2 are non-overlapping.

   Return -1 otherwise.

   Main difference between 0 and -1 is to let
   nonoverlapping_component_refs_since_match_p discover the semantically
   equivalent part of the access path.

   Note that this function is used even with -fno-strict-aliasing
   and makes use of no TBAA assumptions.  */

static int
nonoverlapping_component_refs_p_1 (const_tree field1, const_tree field2)
{
  /* If both fields are of the same type, we could save hard work of
     comparing offsets.  */
  tree type1 = DECL_CONTEXT (field1);
  tree type2 = DECL_CONTEXT (field2);

  if (TREE_CODE (type1) == RECORD_TYPE
      && DECL_BIT_FIELD_REPRESENTATIVE (field1))
    field1 = DECL_BIT_FIELD_REPRESENTATIVE (field1);
  if (TREE_CODE (type2) == RECORD_TYPE
      && DECL_BIT_FIELD_REPRESENTATIVE (field2))
    field2 = DECL_BIT_FIELD_REPRESENTATIVE (field2);

  /* ??? Bitfields can overlap at RTL level so punt on them.
     FIXME: RTL expansion should be fixed by adjusting the access path
     when producing MEM_ATTRs for MEMs which are wider than
     the bitfields similarly as done in set_mem_attrs_minus_bitpos.  */
  if (DECL_BIT_FIELD (field1) && DECL_BIT_FIELD (field2))
    return -1;

  /* Assume that different FIELD_DECLs never overlap within a RECORD_TYPE.  */
  if (type1 == type2 && TREE_CODE (type1) == RECORD_TYPE)
    return field1 != field2;

  /* In common case the offsets and bit offsets will be the same.
     However if frontends do not agree on the alignment, they may be
     different even if they actually represent same address.
     Try the common case first and if that fails calcualte the
     actual bit offset.  */
  if (tree_int_cst_equal (DECL_FIELD_OFFSET (field1),
			  DECL_FIELD_OFFSET (field2))
      && tree_int_cst_equal (DECL_FIELD_BIT_OFFSET (field1),
			     DECL_FIELD_BIT_OFFSET (field2)))
    return 0;

  /* Note that it may be possible to use component_ref_field_offset
     which would provide offsets as trees. However constructing and folding
     trees is expensive and does not seem to be worth the compile time
     cost.  */

  poly_uint64 offset1, offset2;
  poly_uint64 bit_offset1, bit_offset2;

  if (poly_int_tree_p (DECL_FIELD_OFFSET (field1), &offset1)
      && poly_int_tree_p (DECL_FIELD_OFFSET (field2), &offset2)
      && poly_int_tree_p (DECL_FIELD_BIT_OFFSET (field1), &bit_offset1)
      && poly_int_tree_p (DECL_FIELD_BIT_OFFSET (field2), &bit_offset2))
    {
      offset1 = (offset1 << LOG2_BITS_PER_UNIT) + bit_offset1;
      offset2 = (offset2 << LOG2_BITS_PER_UNIT) + bit_offset2;

      if (known_eq (offset1, offset2))
	return 0;

      poly_uint64 size1, size2;

      if (poly_int_tree_p (DECL_SIZE (field1), &size1)
	  && poly_int_tree_p (DECL_SIZE (field2), &size2)
	  && !ranges_maybe_overlap_p (offset1, size1, offset2, size2))
	return 1;
    }
  /* Resort to slower overlap checking by looking for matching types in
     the middle of access path.  */
  return -1;
}

/* Return low bound of array. Do not produce new trees
   and thus do not care about particular type of integer constant
   and placeholder exprs.  */

static tree
cheap_array_ref_low_bound (tree ref)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (ref, 0)));

  /* Avoid expensive array_ref_low_bound.
     low bound is either stored in operand2, or it is TYPE_MIN_VALUE of domain
     type or it is zero.  */
  if (TREE_OPERAND (ref, 2))
    return TREE_OPERAND (ref, 2);
  else if (domain_type && TYPE_MIN_VALUE (domain_type))
    return TYPE_MIN_VALUE (domain_type);
  else
    return integer_zero_node;
}

/* REF1 and REF2 are ARRAY_REFs with either same base address or which are
   completely disjoint.

   Return 1 if the refs are non-overlapping.
   Return 0 if they are possibly overlapping but if so the overlap again
   starts on the same address.
   Return -1 otherwise.  */

int
nonoverlapping_array_refs_p (tree ref1, tree ref2)
{
  tree index1 = TREE_OPERAND (ref1, 1);
  tree index2 = TREE_OPERAND (ref2, 1);
  tree low_bound1 = cheap_array_ref_low_bound (ref1);
  tree low_bound2 = cheap_array_ref_low_bound (ref2);

  /* Handle zero offsets first: we do not need to match type size in this
     case.  */
  if (operand_equal_p (index1, low_bound1, 0)
      && operand_equal_p (index2, low_bound2, 0))
    return 0;

  /* If type sizes are different, give up.

     Avoid expensive array_ref_element_size.
     If operand 3 is present it denotes size in the alignmnet units.
     Otherwise size is TYPE_SIZE of the element type.
     Handle only common cases where types are of the same "kind".  */
  if ((TREE_OPERAND (ref1, 3) == NULL) != (TREE_OPERAND (ref2, 3) == NULL))
    return -1;

  tree elmt_type1 = TREE_TYPE (TREE_TYPE (TREE_OPERAND (ref1, 0)));
  tree elmt_type2 = TREE_TYPE (TREE_TYPE (TREE_OPERAND (ref2, 0)));

  if (TREE_OPERAND (ref1, 3))
    {
      if (TYPE_ALIGN (elmt_type1) != TYPE_ALIGN (elmt_type2)
	  || !operand_equal_p (TREE_OPERAND (ref1, 3),
			       TREE_OPERAND (ref2, 3), 0))
	return -1;
    }
  else
    {
      if (!operand_equal_p (TYPE_SIZE_UNIT (elmt_type1),
			    TYPE_SIZE_UNIT (elmt_type2), 0))
	return -1;
    }

  /* Since we know that type sizes are the same, there is no need to return
     -1 after this point. Partial overlap can not be introduced.  */

  /* We may need to fold trees in this case.
     TODO: Handle integer constant case at least.  */
  if (!operand_equal_p (low_bound1, low_bound2, 0))
    return 0;

  if (TREE_CODE (index1) == INTEGER_CST && TREE_CODE (index2) == INTEGER_CST)
    {
      if (tree_int_cst_equal (index1, index2))
	return 0;
      return 1;
    }
  /* TODO: We can use VRP to further disambiguate here. */
  return 0;
}

/* Try to disambiguate REF1 and REF2 under the assumption that MATCH1 and
   MATCH2 either point to the same address or are disjoint.
   MATCH1 and MATCH2 are assumed to be ref in the access path of REF1 and REF2
   respectively or NULL in the case we established equivalence of bases.
   If PARTIAL_OVERLAP is true assume that the toplevel arrays may actually
   overlap by exact multiply of their element size.

   This test works by matching the initial segment of the access path
   and does not rely on TBAA thus is safe for !flag_strict_aliasing if
   match was determined without use of TBAA oracle.

   Return 1 if we can determine that component references REF1 and REF2,
   that are within a common DECL, cannot overlap.

   Return 0 if paths are same and thus there is nothing to disambiguate more
   (i.e. there is must alias assuming there is must alias between MATCH1 and
   MATCH2)

   Return -1 if we can not determine 0 or 1 - this happens when we met
   non-matching types was met in the path.
   In this case it may make sense to continue by other disambiguation
   oracles.  */

static int
nonoverlapping_refs_since_match_p (tree match1, tree ref1,
				   tree match2, tree ref2,
				   bool partial_overlap)
{
  int ntbaa1 = 0, ntbaa2 = 0;
  /* Early return if there are no references to match, we do not need
     to walk the access paths.

     Do not consider this as may-alias for stats - it is more useful
     to have information how many disambiguations happened provided that
     the query was meaningful.  */

  if (match1 == ref1 || !handled_component_p (ref1)
      || match2 == ref2 || !handled_component_p (ref2))
    return -1;

  auto_vec<tree, 16> component_refs1;
  auto_vec<tree, 16> component_refs2;

  /* Create the stack of handled components for REF1.  */
  while (handled_component_p (ref1) && ref1 != match1)
    {
      /* We use TBAA only to re-synchronize after mismatched refs.  So we
	 do not need to truncate access path after TBAA part ends.  */
      if (ends_tbaa_access_path_p (ref1))
	ntbaa1 = 0;
      else
	ntbaa1++;
      component_refs1.safe_push (ref1);
      ref1 = TREE_OPERAND (ref1, 0);
    }

  /* Create the stack of handled components for REF2.  */
  while (handled_component_p (ref2) && ref2 != match2)
    {
      if (ends_tbaa_access_path_p (ref2))
	ntbaa2 = 0;
      else
	ntbaa2++;
      component_refs2.safe_push (ref2);
      ref2 = TREE_OPERAND (ref2, 0);
    }

  if (!flag_strict_aliasing)
    {
      ntbaa1 = 0;
      ntbaa2 = 0;
    }

  bool mem_ref1 = TREE_CODE (ref1) == MEM_REF && ref1 != match1;
  bool mem_ref2 = TREE_CODE (ref2) == MEM_REF && ref2 != match2;

  /* If only one of access path starts with MEM_REF check that offset is 0
     so the addresses stays the same after stripping it.
     TODO: In this case we may walk the other access path until we get same
     offset.

     If both starts with MEM_REF, offset has to be same.  */
  if ((mem_ref1 && !mem_ref2 && !integer_zerop (TREE_OPERAND (ref1, 1)))
      || (mem_ref2 && !mem_ref1 && !integer_zerop (TREE_OPERAND (ref2, 1)))
      || (mem_ref1 && mem_ref2
	  && !tree_int_cst_equal (TREE_OPERAND (ref1, 1),
				  TREE_OPERAND (ref2, 1))))
    {
      ++alias_stats.nonoverlapping_refs_since_match_p_may_alias;
      return -1;
    }

  /* TARGET_MEM_REF are never wrapped in handled components, so we do not need
     to handle them here at all.  */
  gcc_checking_assert (TREE_CODE (ref1) != TARGET_MEM_REF
		       && TREE_CODE (ref2) != TARGET_MEM_REF);

  /* Pop the stacks in parallel and examine the COMPONENT_REFs of the same
     rank.  This is sufficient because we start from the same DECL and you
     cannot reference several fields at a time with COMPONENT_REFs (unlike
     with ARRAY_RANGE_REFs for arrays) so you always need the same number
     of them to access a sub-component, unless you're in a union, in which
     case the return value will precisely be false.  */
  while (true)
    {
      /* Track if we seen unmatched ref with non-zero offset.  In this case
	 we must look for partial overlaps.  */
      bool seen_unmatched_ref_p = false;

      /* First match ARRAY_REFs an try to disambiguate.  */
      if (!component_refs1.is_empty ()
	  && !component_refs2.is_empty ())
	{
	  unsigned int narray_refs1=0, narray_refs2=0;

	  /* We generally assume that both access paths starts by same sequence
	     of refs.  However if number of array refs is not in sync, try
	     to recover and pop elts until number match.  This helps the case
	     where one access path starts by array and other by element.  */
	  for (narray_refs1 = 0; narray_refs1 < component_refs1.length ();
	       narray_refs1++)
	    if (TREE_CODE (component_refs1 [component_refs1.length()
					    - 1 - narray_refs1]) != ARRAY_REF)
	      break;

	  for (narray_refs2 = 0; narray_refs2 < component_refs2.length ();
	       narray_refs2++)
	    if (TREE_CODE (component_refs2 [component_refs2.length()
					    - 1 - narray_refs2]) != ARRAY_REF)
	      break;
	  for (; narray_refs1 > narray_refs2; narray_refs1--)
	    {
	      ref1 = component_refs1.pop ();
	      ntbaa1--;

	      /* If index is non-zero we need to check whether the reference
		 does not break the main invariant that bases are either
		 disjoint or equal.  Consider the example:

		 unsigned char out[][1];
		 out[1]="a";
		 out[i][0];

		 Here bases out and out are same, but after removing the
		 [i] index, this invariant no longer holds, because
		 out[i] points to the middle of array out.

		 TODO: If size of type of the skipped reference is an integer
		 multiply of the size of type of the other reference this
		 invariant can be verified, but even then it is not completely
		 safe with !flag_strict_aliasing if the other reference contains
		 unbounded array accesses.
		 See   */

	      if (!operand_equal_p (TREE_OPERAND (ref1, 1),
				    cheap_array_ref_low_bound (ref1), 0))
		return 0;
	    }
	  for (; narray_refs2 > narray_refs1; narray_refs2--)
	    {
	      ref2 = component_refs2.pop ();
	      ntbaa2--;
	      if (!operand_equal_p (TREE_OPERAND (ref2, 1),
				    cheap_array_ref_low_bound (ref2), 0))
		return 0;
	    }
	  /* Try to disambiguate matched arrays.  */
	  for (unsigned int i = 0; i < narray_refs1; i++)
	    {
	      int cmp = nonoverlapping_array_refs_p (component_refs1.pop (),
						     component_refs2.pop ());
	      ntbaa1--;
	      ntbaa2--;
	      if (cmp == 1 && !partial_overlap)
		{
		  ++alias_stats
		    .nonoverlapping_refs_since_match_p_no_alias;
		  return 1;
		}
	      if (cmp == -1)
		{
		  seen_unmatched_ref_p = true;
		  /* We can not maintain the invariant that bases are either
		     same or completely disjoint.  However we can still recover
		     from type based alias analysis if we reach references to
		     same sizes.  We do not attempt to match array sizes, so
		     just finish array walking and look for component refs.  */
		  if (ntbaa1 < 0 || ntbaa2 < 0)
		    {
		      ++alias_stats.nonoverlapping_refs_since_match_p_may_alias;
		      return -1;
		    }
		  for (i++; i < narray_refs1; i++)
		    {
		      component_refs1.pop ();
		      component_refs2.pop ();
		      ntbaa1--;
		      ntbaa2--;
		    }
		  break;
		}
	      partial_overlap = false;
	    }
	}

      /* Next look for component_refs.  */
      do
	{
	  if (component_refs1.is_empty ())
	    {
	      ++alias_stats
		.nonoverlapping_refs_since_match_p_must_overlap;
	      return 0;
	    }
	  ref1 = component_refs1.pop ();
	  ntbaa1--;
	  if (TREE_CODE (ref1) != COMPONENT_REF)
	    {
	      seen_unmatched_ref_p = true;
	      if (ntbaa1 < 0 || ntbaa2 < 0)
		{
		  ++alias_stats.nonoverlapping_refs_since_match_p_may_alias;
		  return -1;
		}
	    }
	}
      while (!RECORD_OR_UNION_TYPE_P (TREE_TYPE (TREE_OPERAND (ref1, 0))));

      do
	{
	  if (component_refs2.is_empty ())
	    {
	      ++alias_stats
		.nonoverlapping_refs_since_match_p_must_overlap;
	      return 0;
	    }
	  ref2 = component_refs2.pop ();
	  ntbaa2--;
	  if (TREE_CODE (ref2) != COMPONENT_REF)
	    {
	      if (ntbaa1 < 0 || ntbaa2 < 0)
		{
		  ++alias_stats.nonoverlapping_refs_since_match_p_may_alias;
		  return -1;
		}
	      seen_unmatched_ref_p = true;
	    }
	}
      while (!RECORD_OR_UNION_TYPE_P (TREE_TYPE (TREE_OPERAND (ref2, 0))));

      /* BIT_FIELD_REF and VIEW_CONVERT_EXPR are taken off the vectors
	 earlier.  */
      gcc_checking_assert (TREE_CODE (ref1) == COMPONENT_REF
			   && TREE_CODE (ref2) == COMPONENT_REF);

      tree field1 = TREE_OPERAND (ref1, 1);
      tree field2 = TREE_OPERAND (ref2, 1);

      /* ??? We cannot simply use the type of operand #0 of the refs here
	 as the Fortran compiler smuggles type punning into COMPONENT_REFs
	 for common blocks instead of using unions like everyone else.  */
      tree type1 = DECL_CONTEXT (field1);
      tree type2 = DECL_CONTEXT (field2);

      partial_overlap = false;

      /* If we skipped array refs on type of different sizes, we can
	 no longer be sure that there are not partial overlaps.  */
      if (seen_unmatched_ref_p && ntbaa1 >= 0 && ntbaa2 >= 0
	  && !operand_equal_p (TYPE_SIZE (type1), TYPE_SIZE (type2), 0))
	{
	  ++alias_stats
	    .nonoverlapping_refs_since_match_p_may_alias;
	  return -1;
	}

      int cmp = nonoverlapping_component_refs_p_1 (field1, field2);
      if (cmp == -1)
	{
	  ++alias_stats
	    .nonoverlapping_refs_since_match_p_may_alias;
	  return -1;
	}
      else if (cmp == 1)
	{
	  ++alias_stats
	    .nonoverlapping_refs_since_match_p_no_alias;
	  return 1;
	}
    }
}

/* Return TYPE_UID which can be used to match record types we consider
   same for TBAA purposes.  */

static inline int
ncr_type_uid (const_tree field)
{
  /* ??? We cannot simply use the type of operand #0 of the refs here
     as the Fortran compiler smuggles type punning into COMPONENT_REFs
     for common blocks instead of using unions like everyone else.  */
  tree type = DECL_FIELD_CONTEXT (field);
  /* With LTO types considered same_type_for_tbaa_p
     from different translation unit may not have same
     main variant.  They however have same TYPE_CANONICAL.  */
  if (TYPE_CANONICAL (type))
    return TYPE_UID (TYPE_CANONICAL (type));
  return TYPE_UID (type);
}

/* qsort compare function to sort FIELD_DECLs after their
   DECL_FIELD_CONTEXT TYPE_UID.  */

static inline int
ncr_compar (const void *field1_, const void *field2_)
{
  const_tree field1 = *(const_tree *) const_cast <void *>(field1_);
  const_tree field2 = *(const_tree *) const_cast <void *>(field2_);
  unsigned int uid1 = ncr_type_uid (field1);
  unsigned int uid2 = ncr_type_uid (field2);

  if (uid1 < uid2)
    return -1;
  else if (uid1 > uid2)
    return 1;
  return 0;
}

/* Return true if we can determine that the fields referenced cannot
   overlap for any pair of objects.  This relies on TBAA.  */

static bool
nonoverlapping_component_refs_p (const_tree x, const_tree y)
{
  /* Early return if we have nothing to do.

     Do not consider this as may-alias for stats - it is more useful
     to have information how many disambiguations happened provided that
     the query was meaningful.  */
  if (!flag_strict_aliasing
      || !x || !y
      || !handled_component_p (x)
      || !handled_component_p (y))
    return false;

  auto_vec<const_tree, 16> fieldsx;
  while (handled_component_p (x))
    {
      if (TREE_CODE (x) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (x, 1);
	  tree type = DECL_FIELD_CONTEXT (field);
	  if (TREE_CODE (type) == RECORD_TYPE)
	    fieldsx.safe_push (field);
	}
      else if (ends_tbaa_access_path_p (x))
	fieldsx.truncate (0);
      x = TREE_OPERAND (x, 0);
    }
  if (fieldsx.length () == 0)
    return false;
  auto_vec<const_tree, 16> fieldsy;
  while (handled_component_p (y))
    {
      if (TREE_CODE (y) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (y, 1);
	  tree type = DECL_FIELD_CONTEXT (field);
	  if (TREE_CODE (type) == RECORD_TYPE)
	    fieldsy.safe_push (TREE_OPERAND (y, 1));
	}
      else if (ends_tbaa_access_path_p (y))
	fieldsy.truncate (0);
      y = TREE_OPERAND (y, 0);
    }
  if (fieldsy.length () == 0)
    {
      ++alias_stats.nonoverlapping_component_refs_p_may_alias;
      return false;
    }

  /* Most common case first.  */
  if (fieldsx.length () == 1
      && fieldsy.length () == 1)
   {
     if (same_type_for_tbaa (DECL_FIELD_CONTEXT (fieldsx[0]),
			     DECL_FIELD_CONTEXT (fieldsy[0])) == 1
	 && nonoverlapping_component_refs_p_1 (fieldsx[0], fieldsy[0]) == 1)
      {
         ++alias_stats.nonoverlapping_component_refs_p_no_alias;
         return true;
      }
     else
      {
         ++alias_stats.nonoverlapping_component_refs_p_may_alias;
         return false;
      }
   }

  if (fieldsx.length () == 2)
    {
      if (ncr_compar (&fieldsx[0], &fieldsx[1]) == 1)
	std::swap (fieldsx[0], fieldsx[1]);
    }
  else
    fieldsx.qsort (ncr_compar);

  if (fieldsy.length () == 2)
    {
      if (ncr_compar (&fieldsy[0], &fieldsy[1]) == 1)
	std::swap (fieldsy[0], fieldsy[1]);
    }
  else
    fieldsy.qsort (ncr_compar);

  unsigned i = 0, j = 0;
  do
    {
      const_tree fieldx = fieldsx[i];
      const_tree fieldy = fieldsy[j];

      /* We're left with accessing different fields of a structure,
	 no possible overlap.  */
      if (same_type_for_tbaa (DECL_FIELD_CONTEXT (fieldx),
			      DECL_FIELD_CONTEXT (fieldy)) == 1
	  && nonoverlapping_component_refs_p_1 (fieldx, fieldy) == 1)
	{
	  ++alias_stats.nonoverlapping_component_refs_p_no_alias;
	  return true;
	}

      if (ncr_type_uid (fieldx) < ncr_type_uid (fieldy))
	{
	  i++;
	  if (i == fieldsx.length ())
	    break;
	}
      else
	{
	  j++;
	  if (j == fieldsy.length ())
	    break;
	}
    }
  while (1);

  ++alias_stats.nonoverlapping_component_refs_p_may_alias;
  return false;
}


/* Return true if two memory references based on the variables BASE1
   and BASE2 constrained to [OFFSET1, OFFSET1 + MAX_SIZE1) and
   [OFFSET2, OFFSET2 + MAX_SIZE2) may alias.  REF1 and REF2
   if non-NULL are the complete memory reference trees.  */

static bool
decl_refs_may_alias_p (tree ref1, tree base1,
		       poly_int64 offset1, poly_int64 max_size1,
		       poly_int64 size1,
		       tree ref2, tree base2,
		       poly_int64 offset2, poly_int64 max_size2,
		       poly_int64 size2)
{
  gcc_checking_assert (DECL_P (base1) && DECL_P (base2));

  /* If both references are based on different variables, they cannot alias.  */
  if (compare_base_decls (base1, base2) == 0)
    return false;

  /* If both references are based on the same variable, they cannot alias if
     the accesses do not overlap.  */
  if (!ranges_maybe_overlap_p (offset1, max_size1, offset2, max_size2))
    return false;

  /* If there is must alias, there is no use disambiguating further.  */
  if (known_eq (size1, max_size1) && known_eq (size2, max_size2))
    return true;

  /* For components with variable position, the above test isn't sufficient,
     so we disambiguate component references manually.  */
  if (ref1 && ref2
      && handled_component_p (ref1) && handled_component_p (ref2)
      && nonoverlapping_refs_since_match_p (NULL, ref1, NULL, ref2, false) == 1)
    return false;

  return true;
}

/* Return true if access with BASE is view converted.
   Base must not be stripped from inner MEM_REF (&decl)
   which is done by ao_ref_base and thus one extra walk
   of handled components is needed.  */

bool
view_converted_memref_p (tree base)
{
  if (TREE_CODE (base) != MEM_REF && TREE_CODE (base) != TARGET_MEM_REF)
    return false;
  return (same_type_for_tbaa (TREE_TYPE (base),
			      TREE_TYPE (TREE_TYPE (TREE_OPERAND (base, 1))))
	  != 1);
}

/* Return true if an indirect reference based on *PTR1 constrained
   to [OFFSET1, OFFSET1 + MAX_SIZE1) may alias a variable based on BASE2
   constrained to [OFFSET2, OFFSET2 + MAX_SIZE2).  *PTR1 and BASE2 have
   the alias sets BASE1_ALIAS_SET and BASE2_ALIAS_SET which can be -1
   in which case they are computed on-demand.  REF1 and REF2
   if non-NULL are the complete memory reference trees.  */

static bool
indirect_ref_may_alias_decl_p (tree ref1 ATTRIBUTE_UNUSED, tree base1,
			       poly_int64 offset1, poly_int64 max_size1,
			       poly_int64 size1,
			       alias_set_type ref1_alias_set,
			       alias_set_type base1_alias_set,
			       tree ref2 ATTRIBUTE_UNUSED, tree base2,
			       poly_int64 offset2, poly_int64 max_size2,
			       poly_int64 size2,
			       alias_set_type ref2_alias_set,
			       alias_set_type base2_alias_set, bool tbaa_p)
{
  tree ptr1;
  tree ptrtype1, dbase2;

  gcc_checking_assert ((TREE_CODE (base1) == MEM_REF
			|| TREE_CODE (base1) == TARGET_MEM_REF)
		       && DECL_P (base2));

  ptr1 = TREE_OPERAND (base1, 0);
  poly_offset_int moff = mem_ref_offset (base1) << LOG2_BITS_PER_UNIT;

  /* If only one reference is based on a variable, they cannot alias if
     the pointer access is beyond the extent of the variable access.
     (the pointer base cannot validly point to an offset less than zero
     of the variable).
     ???  IVOPTs creates bases that do not honor this restriction,
     so do not apply this optimization for TARGET_MEM_REFs.  */
  if (TREE_CODE (base1) != TARGET_MEM_REF
      && !ranges_maybe_overlap_p (offset1 + moff, -1, offset2, max_size2))
    return false;

  /* If the pointer based access is bigger than the variable they cannot
     alias.  This is similar to the check below where we use TBAA to
     increase the size of the pointer based access based on the dynamic
     type of a containing object we can infer from it.  */
  poly_int64 dsize2;
  if (known_size_p (size1)
      && poly_int_tree_p (DECL_SIZE (base2), &dsize2)
      && known_lt (dsize2, size1))
    return false;

  /* They also cannot alias if the pointer may not point to the decl.  */
  if (!ptr_deref_may_alias_decl_p (ptr1, base2))
    return false;

  /* Disambiguations that rely on strict aliasing rules follow.  */
  if (!flag_strict_aliasing || !tbaa_p)
    return true;

  /* If the alias set for a pointer access is zero all bets are off.  */
  if (base1_alias_set == 0 || base2_alias_set == 0)
    return true;

  /* When we are trying to disambiguate an access with a pointer dereference
     as base versus one with a decl as base we can use both the size
     of the decl and its dynamic type for extra disambiguation.
     ???  We do not know anything about the dynamic type of the decl
     other than that its alias-set contains base2_alias_set as a subset
     which does not help us here.  */
  /* As we know nothing useful about the dynamic type of the decl just
     use the usual conflict check rather than a subset test.
     ???  We could introduce -fvery-strict-aliasing when the language
     does not allow decls to have a dynamic type that differs from their
     static type.  Then we can check
     !alias_set_subset_of (base1_alias_set, base2_alias_set) instead.  */
  if (base1_alias_set != base2_alias_set
      && !alias_sets_conflict_p (base1_alias_set, base2_alias_set))
    return false;

  ptrtype1 = TREE_TYPE (TREE_OPERAND (base1, 1));

  /* If the size of the access relevant for TBAA through the pointer
     is bigger than the size of the decl we can't possibly access the
     decl via that pointer.  */
  if (/* ???  This in turn may run afoul when a decl of type T which is
	 a member of union type U is accessed through a pointer to
	 type U and sizeof T is smaller than sizeof U.  */
      TREE_CODE (TREE_TYPE (ptrtype1)) != UNION_TYPE
      && TREE_CODE (TREE_TYPE (ptrtype1)) != QUAL_UNION_TYPE
      && compare_sizes (DECL_SIZE (base2),
		        TYPE_SIZE (TREE_TYPE (ptrtype1))) < 0)
    return false;

  if (!ref2)
    return true;

  /* If the decl is accessed via a MEM_REF, reconstruct the base
     we can use for TBAA and an appropriately adjusted offset.  */
  dbase2 = ref2;
  while (handled_component_p (dbase2))
    dbase2 = TREE_OPERAND (dbase2, 0);
  poly_int64 doffset1 = offset1;
  poly_offset_int doffset2 = offset2;
  if (TREE_CODE (dbase2) == MEM_REF
      || TREE_CODE (dbase2) == TARGET_MEM_REF)
    {
      doffset2 -= mem_ref_offset (dbase2) << LOG2_BITS_PER_UNIT;
      tree ptrtype2 = TREE_TYPE (TREE_OPERAND (dbase2, 1));
      /* If second reference is view-converted, give up now.  */
      if (same_type_for_tbaa (TREE_TYPE (dbase2), TREE_TYPE (ptrtype2)) != 1)
	return true;
    }

  /* If first reference is view-converted, give up now.  */
  if (same_type_for_tbaa (TREE_TYPE (base1), TREE_TYPE (ptrtype1)) != 1)
    return true;

  /* If both references are through the same type, they do not alias
     if the accesses do not overlap.  This does extra disambiguation
     for mixed/pointer accesses but requires strict aliasing.
     For MEM_REFs we require that the component-ref offset we computed
     is relative to the start of the type which we ensure by
     comparing rvalue and access type and disregarding the constant
     pointer offset.

     But avoid treating variable length arrays as "objects", instead assume they
     can overlap by an exact multiple of their element size.
     See gcc.dg/torture/alias-2.c.  */
  if (((TREE_CODE (base1) != TARGET_MEM_REF
       || (!TMR_INDEX (base1) && !TMR_INDEX2 (base1)))
       && (TREE_CODE (dbase2) != TARGET_MEM_REF
	   || (!TMR_INDEX (dbase2) && !TMR_INDEX2 (dbase2))))
      && same_type_for_tbaa (TREE_TYPE (base1), TREE_TYPE (dbase2)) == 1)
    {
      bool partial_overlap = (TREE_CODE (TREE_TYPE (base1)) == ARRAY_TYPE
			      && (TYPE_SIZE (TREE_TYPE (base1))
			      && TREE_CODE (TYPE_SIZE (TREE_TYPE (base1)))
				 != INTEGER_CST));
      if (!partial_overlap
	  && !ranges_maybe_overlap_p (doffset1, max_size1, doffset2, max_size2))
	return false;
      if (!ref1 || !ref2
	  /* If there is must alias, there is no use disambiguating further.  */
	  || (!partial_overlap
	      && known_eq (size1, max_size1) && known_eq (size2, max_size2)))
	return true;
      int res = nonoverlapping_refs_since_match_p (base1, ref1, base2, ref2,
						   partial_overlap);
      if (res == -1)
	return !nonoverlapping_component_refs_p (ref1, ref2);
      return !res;
    }

  /* Do access-path based disambiguation.  */
  if (ref1 && ref2
      && (handled_component_p (ref1) || handled_component_p (ref2)))
    return aliasing_component_refs_p (ref1,
				      ref1_alias_set, base1_alias_set,
				      offset1, max_size1,
				      ref2,
				      ref2_alias_set, base2_alias_set,
				      offset2, max_size2);

  return true;
}

/* Return true if two indirect references based on *PTR1
   and *PTR2 constrained to [OFFSET1, OFFSET1 + MAX_SIZE1) and
   [OFFSET2, OFFSET2 + MAX_SIZE2) may alias.  *PTR1 and *PTR2 have
   the alias sets BASE1_ALIAS_SET and BASE2_ALIAS_SET which can be -1
   in which case they are computed on-demand.  REF1 and REF2
   if non-NULL are the complete memory reference trees. */

static bool
indirect_refs_may_alias_p (tree ref1 ATTRIBUTE_UNUSED, tree base1,
			   poly_int64 offset1, poly_int64 max_size1,
			   poly_int64 size1,
			   alias_set_type ref1_alias_set,
			   alias_set_type base1_alias_set,
			   tree ref2 ATTRIBUTE_UNUSED, tree base2,
			   poly_int64 offset2, poly_int64 max_size2,
			   poly_int64 size2,
			   alias_set_type ref2_alias_set,
			   alias_set_type base2_alias_set, bool tbaa_p)
{
  tree ptr1;
  tree ptr2;
  tree ptrtype1, ptrtype2;

  gcc_checking_assert ((TREE_CODE (base1) == MEM_REF
			|| TREE_CODE (base1) == TARGET_MEM_REF)
		       && (TREE_CODE (base2) == MEM_REF
			   || TREE_CODE (base2) == TARGET_MEM_REF));

  ptr1 = TREE_OPERAND (base1, 0);
  ptr2 = TREE_OPERAND (base2, 0);

  /* If both bases are based on pointers they cannot alias if they may not
     point to the same memory object or if they point to the same object
     and the accesses do not overlap.  */
  if ((!cfun || gimple_in_ssa_p (cfun))
      && operand_equal_p (ptr1, ptr2, 0)
      && (((TREE_CODE (base1) != TARGET_MEM_REF
	    || (!TMR_INDEX (base1) && !TMR_INDEX2 (base1)))
	   && (TREE_CODE (base2) != TARGET_MEM_REF
	       || (!TMR_INDEX (base2) && !TMR_INDEX2 (base2))))
	  || (TREE_CODE (base1) == TARGET_MEM_REF
	      && TREE_CODE (base2) == TARGET_MEM_REF
	      && (TMR_STEP (base1) == TMR_STEP (base2)
		  || (TMR_STEP (base1) && TMR_STEP (base2)
		      && operand_equal_p (TMR_STEP (base1),
					  TMR_STEP (base2), 0)))
	      && (TMR_INDEX (base1) == TMR_INDEX (base2)
		  || (TMR_INDEX (base1) && TMR_INDEX (base2)
		      && operand_equal_p (TMR_INDEX (base1),
					  TMR_INDEX (base2), 0)))
	      && (TMR_INDEX2 (base1) == TMR_INDEX2 (base2)
		  || (TMR_INDEX2 (base1) && TMR_INDEX2 (base2)
		      && operand_equal_p (TMR_INDEX2 (base1),
					  TMR_INDEX2 (base2), 0))))))
    {
      poly_offset_int moff1 = mem_ref_offset (base1) << LOG2_BITS_PER_UNIT;
      poly_offset_int moff2 = mem_ref_offset (base2) << LOG2_BITS_PER_UNIT;
      if (!ranges_maybe_overlap_p (offset1 + moff1, max_size1,
				   offset2 + moff2, max_size2))
	return false;
      /* If there is must alias, there is no use disambiguating further.  */
      if (known_eq (size1, max_size1) && known_eq (size2, max_size2))
	return true;
      if (ref1 && ref2)
	{
	  int res = nonoverlapping_refs_since_match_p (NULL, ref1, NULL, ref2,
						       false);
	  if (res != -1)
	    return !res;
	}
    }
  if (!ptr_derefs_may_alias_p (ptr1, ptr2))
    return false;

  /* Disambiguations that rely on strict aliasing rules follow.  */
  if (!flag_strict_aliasing || !tbaa_p)
    return true;

  ptrtype1 = TREE_TYPE (TREE_OPERAND (base1, 1));
  ptrtype2 = TREE_TYPE (TREE_OPERAND (base2, 1));

  /* If the alias set for a pointer access is zero all bets are off.  */
  if (base1_alias_set == 0
      || base2_alias_set == 0)
    return true;

  /* Do type-based disambiguation.  */
  if (base1_alias_set != base2_alias_set
      && !alias_sets_conflict_p (base1_alias_set, base2_alias_set))
    return false;

  /* If either reference is view-converted, give up now.  */
  if (same_type_for_tbaa (TREE_TYPE (base1), TREE_TYPE (ptrtype1)) != 1
      || same_type_for_tbaa (TREE_TYPE (base2), TREE_TYPE (ptrtype2)) != 1)
    return true;

  /* If both references are through the same type, they do not alias
     if the accesses do not overlap.  This does extra disambiguation
     for mixed/pointer accesses but requires strict aliasing.  */
  if ((TREE_CODE (base1) != TARGET_MEM_REF
       || (!TMR_INDEX (base1) && !TMR_INDEX2 (base1)))
      && (TREE_CODE (base2) != TARGET_MEM_REF
	  || (!TMR_INDEX (base2) && !TMR_INDEX2 (base2)))
      && same_type_for_tbaa (TREE_TYPE (ptrtype1),
			     TREE_TYPE (ptrtype2)) == 1)
    {
      /* But avoid treating arrays as "objects", instead assume they
         can overlap by an exact multiple of their element size.
         See gcc.dg/torture/alias-2.c.  */
      bool partial_overlap = TREE_CODE (TREE_TYPE (ptrtype1)) == ARRAY_TYPE;

      if (!partial_overlap
	  && !ranges_maybe_overlap_p (offset1, max_size1, offset2, max_size2))
	return false;
      if (!ref1 || !ref2
	  || (!partial_overlap
	      && known_eq (size1, max_size1) && known_eq (size2, max_size2)))
	return true;
      int res = nonoverlapping_refs_since_match_p (base1, ref1, base2, ref2,
						   partial_overlap);
      if (res == -1)
	return !nonoverlapping_component_refs_p (ref1, ref2);
      return !res;
    }

  /* Do access-path based disambiguation.  */
  if (ref1 && ref2
      && (handled_component_p (ref1) || handled_component_p (ref2)))
    return aliasing_component_refs_p (ref1,
				      ref1_alias_set, base1_alias_set,
				      offset1, max_size1,
				      ref2,
				      ref2_alias_set, base2_alias_set,
				      offset2, max_size2);

  return true;
}

/* Return true, if the two memory references REF1 and REF2 may alias.  */

static bool
refs_may_alias_p_2 (ao_ref *ref1, ao_ref *ref2, bool tbaa_p)
{
  tree base1, base2;
  poly_int64 offset1 = 0, offset2 = 0;
  poly_int64 max_size1 = -1, max_size2 = -1;
  bool var1_p, var2_p, ind1_p, ind2_p;

  gcc_checking_assert ((!ref1->ref
			|| TREE_CODE (ref1->ref) == SSA_NAME
			|| DECL_P (ref1->ref)
			|| TREE_CODE (ref1->ref) == STRING_CST
			|| handled_component_p (ref1->ref)
			|| TREE_CODE (ref1->ref) == MEM_REF
			|| TREE_CODE (ref1->ref) == TARGET_MEM_REF
			|| TREE_CODE (ref1->ref) == WITH_SIZE_EXPR)
		       && (!ref2->ref
			   || TREE_CODE (ref2->ref) == SSA_NAME
			   || DECL_P (ref2->ref)
			   || TREE_CODE (ref2->ref) == STRING_CST
			   || handled_component_p (ref2->ref)
			   || TREE_CODE (ref2->ref) == MEM_REF
			   || TREE_CODE (ref2->ref) == TARGET_MEM_REF
			   || TREE_CODE (ref2->ref) == WITH_SIZE_EXPR));

  /* Decompose the references into their base objects and the access.  */
  base1 = ao_ref_base (ref1);
  offset1 = ref1->offset;
  max_size1 = ref1->max_size;
  base2 = ao_ref_base (ref2);
  offset2 = ref2->offset;
  max_size2 = ref2->max_size;

  /* We can end up with registers or constants as bases for example from
     *D.1663_44 = VIEW_CONVERT_EXPR<struct DB_LSN>(__tmp$B0F64_59);
     which is seen as a struct copy.  */
  if (TREE_CODE (base1) == SSA_NAME
      || TREE_CODE (base1) == CONST_DECL
      || TREE_CODE (base1) == CONSTRUCTOR
      || TREE_CODE (base1) == ADDR_EXPR
      || CONSTANT_CLASS_P (base1)
      || TREE_CODE (base2) == SSA_NAME
      || TREE_CODE (base2) == CONST_DECL
      || TREE_CODE (base2) == CONSTRUCTOR
      || TREE_CODE (base2) == ADDR_EXPR
      || CONSTANT_CLASS_P (base2))
    return false;

  /* Two volatile accesses always conflict.  */
  if (ref1->volatile_p
      && ref2->volatile_p)
    return true;

  /* refN->ref may convey size information, do not confuse our workers
     with that but strip it - ao_ref_base took it into account already.  */
  tree ref1ref = ref1->ref;
  if (ref1ref && TREE_CODE (ref1ref) == WITH_SIZE_EXPR)
    ref1ref = TREE_OPERAND (ref1ref, 0);
  tree ref2ref = ref2->ref;
  if (ref2ref && TREE_CODE (ref2ref) == WITH_SIZE_EXPR)
    ref2ref = TREE_OPERAND (ref2ref, 0);

  /* Defer to simple offset based disambiguation if we have
     references based on two decls.  Do this before defering to
     TBAA to handle must-alias cases in conformance with the
     GCC extension of allowing type-punning through unions.  */
  var1_p = DECL_P (base1);
  var2_p = DECL_P (base2);
  if (var1_p && var2_p)
    return decl_refs_may_alias_p (ref1ref, base1, offset1, max_size1,
				  ref1->size,
				  ref2ref, base2, offset2, max_size2,
				  ref2->size);

  /* We can end up referring to code via function and label decls.
     As we likely do not properly track code aliases conservatively
     bail out.  */
  if (TREE_CODE (base1) == FUNCTION_DECL
      || TREE_CODE (base1) == LABEL_DECL
      || TREE_CODE (base2) == FUNCTION_DECL
      || TREE_CODE (base2) == LABEL_DECL)
    return true;

  /* Handle restrict based accesses.
     ???  ao_ref_base strips inner MEM_REF [&decl], recover from that
     here.  */
  tree rbase1 = base1;
  tree rbase2 = base2;
  if (var1_p)
    {
      rbase1 = ref1ref;
      if (rbase1)
	while (handled_component_p (rbase1))
	  rbase1 = TREE_OPERAND (rbase1, 0);
    }
  if (var2_p)
    {
      rbase2 = ref2ref;
      if (rbase2)
	while (handled_component_p (rbase2))
	  rbase2 = TREE_OPERAND (rbase2, 0);
    }
  if (rbase1 && rbase2
      && (TREE_CODE (rbase1) == MEM_REF || TREE_CODE (rbase1) == TARGET_MEM_REF)
      && (TREE_CODE (rbase2) == MEM_REF || TREE_CODE (rbase2) == TARGET_MEM_REF)
      /* If the accesses are in the same restrict clique... */
      && MR_DEPENDENCE_CLIQUE (rbase1) == MR_DEPENDENCE_CLIQUE (rbase2)
      /* But based on different pointers they do not alias.  */
      && MR_DEPENDENCE_BASE (rbase1) != MR_DEPENDENCE_BASE (rbase2))
    return false;

  ind1_p = (TREE_CODE (base1) == MEM_REF
	    || TREE_CODE (base1) == TARGET_MEM_REF);
  ind2_p = (TREE_CODE (base2) == MEM_REF
	    || TREE_CODE (base2) == TARGET_MEM_REF);

  /* Canonicalize the pointer-vs-decl case.  */
  if (ind1_p && var2_p)
    {
      std::swap (offset1, offset2);
      std::swap (max_size1, max_size2);
      std::swap (base1, base2);
      std::swap (ref1, ref2);
      std::swap (ref1ref, ref2ref);
      var1_p = true;
      ind1_p = false;
      var2_p = false;
      ind2_p = true;
    }

  /* First defer to TBAA if possible.  */
  if (tbaa_p
      && flag_strict_aliasing
      && !alias_sets_conflict_p (ao_ref_alias_set (ref1),
				 ao_ref_alias_set (ref2)))
    return false;

  /* If the reference is based on a pointer that points to memory
     that may not be written to then the other reference cannot possibly
     clobber it.  */
  if ((TREE_CODE (TREE_OPERAND (base2, 0)) == SSA_NAME
       && SSA_NAME_POINTS_TO_READONLY_MEMORY (TREE_OPERAND (base2, 0)))
      || (ind1_p
	  && TREE_CODE (TREE_OPERAND (base1, 0)) == SSA_NAME
	  && SSA_NAME_POINTS_TO_READONLY_MEMORY (TREE_OPERAND (base1, 0))))
    return false;

  /* Dispatch to the pointer-vs-decl or pointer-vs-pointer disambiguators.  */
  if (var1_p && ind2_p)
    return indirect_ref_may_alias_decl_p (ref2ref, base2,
					  offset2, max_size2, ref2->size,
					  ao_ref_alias_set (ref2),
					  ao_ref_base_alias_set (ref2),
					  ref1ref, base1,
					  offset1, max_size1, ref1->size,
					  ao_ref_alias_set (ref1),
					  ao_ref_base_alias_set (ref1),
					  tbaa_p);
  else if (ind1_p && ind2_p)
    return indirect_refs_may_alias_p (ref1ref, base1,
				      offset1, max_size1, ref1->size,
				      ao_ref_alias_set (ref1),
				      ao_ref_base_alias_set (ref1),
				      ref2ref, base2,
				      offset2, max_size2, ref2->size,
				      ao_ref_alias_set (ref2),
				      ao_ref_base_alias_set (ref2),
				      tbaa_p);

  gcc_unreachable ();
}

/* Return true, if the two memory references REF1 and REF2 may alias
   and update statistics.  */

bool
refs_may_alias_p_1 (ao_ref *ref1, ao_ref *ref2, bool tbaa_p)
{
  bool res = refs_may_alias_p_2 (ref1, ref2, tbaa_p);
  if (res)
    ++alias_stats.refs_may_alias_p_may_alias;
  else
    ++alias_stats.refs_may_alias_p_no_alias;
  return res;
}

static bool
refs_may_alias_p (tree ref1, ao_ref *ref2, bool tbaa_p)
{
  ao_ref r1;
  ao_ref_init (&r1, ref1);
  return refs_may_alias_p_1 (&r1, ref2, tbaa_p);
}

bool
refs_may_alias_p (tree ref1, tree ref2, bool tbaa_p)
{
  ao_ref r1, r2;
  ao_ref_init (&r1, ref1);
  ao_ref_init (&r2, ref2);
  return refs_may_alias_p_1 (&r1, &r2, tbaa_p);
}

/* Returns true if there is a anti-dependence for the STORE that
   executes after the LOAD.  */

bool
refs_anti_dependent_p (tree load, tree store)
{
  ao_ref r1, r2;
  ao_ref_init (&r1, load);
  ao_ref_init (&r2, store);
  return refs_may_alias_p_1 (&r1, &r2, false);
}

/* Returns true if there is a output dependence for the stores
   STORE1 and STORE2.  */

bool
refs_output_dependent_p (tree store1, tree store2)
{
  ao_ref r1, r2;
  ao_ref_init (&r1, store1);
  ao_ref_init (&r2, store2);
  return refs_may_alias_p_1 (&r1, &r2, false);
}

/* Returns true if and only if REF may alias any access stored in TT.
   IF TBAA_P is true, use TBAA oracle.  */

static bool
modref_may_conflict (const gcall *stmt,
		     modref_tree <alias_set_type> *tt, ao_ref *ref, bool tbaa_p)
{
  alias_set_type base_set, ref_set;
  bool global_memory_ok = false;

  if (tt->every_base)
    return true;

  if (!dbg_cnt (ipa_mod_ref))
    return true;

  base_set = ao_ref_base_alias_set (ref);

  ref_set = ao_ref_alias_set (ref);

  int num_tests = 0, max_tests = param_modref_max_tests;
  for (auto base_node : tt->bases)
    {
      if (tbaa_p && flag_strict_aliasing)
	{
	  if (num_tests >= max_tests)
	    return true;
	  alias_stats.modref_tests++;
	  if (!alias_sets_conflict_p (base_set, base_node->base))
	    continue;
	  num_tests++;
	}

      if (base_node->every_ref)
	return true;

      for (auto ref_node : base_node->refs)
	{
	  /* Do not repeat same test as before.  */
	  if ((ref_set != base_set || base_node->base != ref_node->ref)
	      && tbaa_p && flag_strict_aliasing)
	    {
	      if (num_tests >= max_tests)
		return true;
	      alias_stats.modref_tests++;
	      if (!alias_sets_conflict_p (ref_set, ref_node->ref))
		continue;
	      num_tests++;
	    }

	  if (ref_node->every_access)
	    return true;

	  /* TBAA checks did not disambiguate, try individual accesses.  */
	  for (auto access_node : ref_node->accesses)
	    {
	      if (num_tests >= max_tests)
		return true;

	      if (access_node.parm_index == MODREF_GLOBAL_MEMORY_PARM)
		{
		  if (global_memory_ok)
		    continue;
		  if (ref_may_alias_global_p (ref, true))
		    return true;
		  global_memory_ok = true;
		  num_tests++;
		  continue;
		}

	      tree arg = access_node.get_call_arg (stmt);
	      if (!arg)
		return true;

	      alias_stats.modref_baseptr_tests++;

	      if (integer_zerop (arg) && flag_delete_null_pointer_checks)
		continue;

	      /* PTA oracle will be unhapy of arg is not an pointer.  */
	      if (!POINTER_TYPE_P (TREE_TYPE (arg)))
		return true;

	      /* If we don't have base pointer, give up.  */
	      if (!ref->ref && !ref->base)
		continue;

	      ao_ref ref2;
	      if (access_node.get_ao_ref (stmt, &ref2))
		{
		  ref2.ref_alias_set = ref_node->ref;
		  ref2.base_alias_set = base_node->base;
		  if (refs_may_alias_p_1 (&ref2, ref, tbaa_p))
		    return true;
		}
	      else if (ptr_deref_may_alias_ref_p_1 (arg, ref))
		return true;

	      num_tests++;
	    }
	}
    }
  return false;
}

/* Check if REF conflicts with call using "fn spec" attribute.
   If CLOBBER is true we are checking for writes, otherwise check loads.

   Return 0 if there are no conflicts (except for possible function call
   argument reads), 1 if there are conflicts and -1 if we can not decide by
   fn spec.  */

static int
check_fnspec (gcall *call, ao_ref *ref, bool clobber)
{
  attr_fnspec fnspec = gimple_call_fnspec (call);
  if (fnspec.known_p ())
    {
      if (clobber
	  ? !fnspec.global_memory_written_p ()
	  : !fnspec.global_memory_read_p ())
	{
	  for (unsigned int i = 0; i < gimple_call_num_args (call); i++)
	    if (POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, i)))
		&& (!fnspec.arg_specified_p (i)
		    || (clobber ? fnspec.arg_maybe_written_p (i)
			: fnspec.arg_maybe_read_p (i))))
	      {
		ao_ref dref;
		tree size = NULL_TREE;
		unsigned int size_arg;

		if (!fnspec.arg_specified_p (i))
		  ;
		else if (fnspec.arg_max_access_size_given_by_arg_p
			   (i, &size_arg))
		  size = gimple_call_arg (call, size_arg);
		else if (fnspec.arg_access_size_given_by_type_p (i))
		  {
		    tree callee = gimple_call_fndecl (call);
		    tree t = TYPE_ARG_TYPES (TREE_TYPE (callee));

		    for (unsigned int p = 0; p < i; p++)
		      t = TREE_CHAIN (t);
		    size = TYPE_SIZE_UNIT (TREE_TYPE (TREE_VALUE (t)));
		  }
		poly_int64 size_hwi;
		if (size
		    && poly_int_tree_p (size, &size_hwi)
		    && coeffs_in_range_p (size_hwi, 0,
					  HOST_WIDE_INT_MAX / BITS_PER_UNIT))
		  {
		    size_hwi = size_hwi * BITS_PER_UNIT;
		    ao_ref_init_from_ptr_and_range (&dref,
						    gimple_call_arg (call, i),
						    true, 0, -1, size_hwi);
		  }
		else
		  ao_ref_init_from_ptr_and_range (&dref,
						  gimple_call_arg (call, i),
						  false, 0, -1, -1);
		if (refs_may_alias_p_1 (&dref, ref, false))
		  return 1;
	      }
	  if (clobber
	      && fnspec.errno_maybe_written_p ()
	      && flag_errno_math
	      && targetm.ref_may_alias_errno (ref))
	    return 1;
	  return 0;
	}
    }

 /* FIXME: we should handle barriers more consistently, but for now leave the
    check here.  */
  if (gimple_call_builtin_p (call, BUILT_IN_NORMAL))
    switch (DECL_FUNCTION_CODE (gimple_call_fndecl (call)))
      {
      /* __sync_* builtins and some OpenMP builtins act as threading
	 barriers.  */
#undef DEF_SYNC_BUILTIN
#define DEF_SYNC_BUILTIN(ENUM, NAME, TYPE, ATTRS) case ENUM:
#include "sync-builtins.def"
#undef DEF_SYNC_BUILTIN
      case BUILT_IN_GOMP_ATOMIC_START:
      case BUILT_IN_GOMP_ATOMIC_END:
      case BUILT_IN_GOMP_BARRIER:
      case BUILT_IN_GOMP_BARRIER_CANCEL:
      case BUILT_IN_GOMP_TASKWAIT:
      case BUILT_IN_GOMP_TASKGROUP_END:
      case BUILT_IN_GOMP_CRITICAL_START:
      case BUILT_IN_GOMP_CRITICAL_END:
      case BUILT_IN_GOMP_CRITICAL_NAME_START:
      case BUILT_IN_GOMP_CRITICAL_NAME_END:
      case BUILT_IN_GOMP_LOOP_END:
      case BUILT_IN_GOMP_LOOP_END_CANCEL:
      case BUILT_IN_GOMP_ORDERED_START:
      case BUILT_IN_GOMP_ORDERED_END:
      case BUILT_IN_GOMP_SECTIONS_END:
      case BUILT_IN_GOMP_SECTIONS_END_CANCEL:
      case BUILT_IN_GOMP_SINGLE_COPY_START:
      case BUILT_IN_GOMP_SINGLE_COPY_END:
	return 1;

      default:
	return -1;
      }
  return -1;
}

/* If the call CALL may use the memory reference REF return true,
   otherwise return false.  */

static bool
ref_maybe_used_by_call_p_1 (gcall *call, ao_ref *ref, bool tbaa_p)
{
  tree base, callee;
  unsigned i;
  int flags = gimple_call_flags (call);

  if (flags & (ECF_CONST|ECF_NOVOPS))
    goto process_args;

  /* A call that is not without side-effects might involve volatile
     accesses and thus conflicts with all other volatile accesses.  */
  if (ref->volatile_p)
    return true;

  if (gimple_call_internal_p (call))
    switch (gimple_call_internal_fn (call))
      {
      case IFN_MASK_STORE:
      case IFN_SCATTER_STORE:
      case IFN_MASK_SCATTER_STORE:
      case IFN_LEN_STORE:
      case IFN_MASK_LEN_STORE:
	return false;
      case IFN_MASK_STORE_LANES:
      case IFN_MASK_LEN_STORE_LANES:
	goto process_args;
      case IFN_MASK_LOAD:
      case IFN_LEN_LOAD:
      case IFN_MASK_LEN_LOAD:
      case IFN_MASK_LOAD_LANES:
      case IFN_MASK_LEN_LOAD_LANES:
	{
	  ao_ref rhs_ref;
	  tree lhs = gimple_call_lhs (call);
	  if (lhs)
	    {
	      ao_ref_init_from_ptr_and_size (&rhs_ref,
					     gimple_call_arg (call, 0),
					     TYPE_SIZE_UNIT (TREE_TYPE (lhs)));
	      /* We cannot make this a known-size access since otherwise
		 we disambiguate against refs to decls that are smaller.  */
	      rhs_ref.size = -1;
	      rhs_ref.ref_alias_set = rhs_ref.base_alias_set
		= tbaa_p ? get_deref_alias_set (TREE_TYPE
					(gimple_call_arg (call, 1))) : 0;
	      return refs_may_alias_p_1 (ref, &rhs_ref, tbaa_p);
	    }
	  break;
	}
      default:;
      }

  callee = gimple_call_fndecl (call);
  if (callee != NULL_TREE)
    {
      struct cgraph_node *node = cgraph_node::get (callee);
      /* We can not safely optimize based on summary of calle if it does
	 not always bind to current def: it is possible that memory load
	 was optimized out earlier and the interposed variant may not be
	 optimized this way.  */
      if (node && node->binds_to_current_def_p ())
	{
	  modref_summary *summary = get_modref_function_summary (node);
	  if (summary && !summary->calls_interposable)
	    {
	      if (!modref_may_conflict (call, summary->loads, ref, tbaa_p))
		{
		  alias_stats.modref_use_no_alias++;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file,
			       "ipa-modref: call stmt ");
		      print_gimple_stmt (dump_file, call, 0);
		      fprintf (dump_file,
			       "ipa-modref: call to %s does not use ",
			       node->dump_name ());
		      if (!ref->ref && ref->base)
			{
			  fprintf (dump_file, "base: ");
			  print_generic_expr (dump_file, ref->base);
			}
		      else if (ref->ref)
			{
			  fprintf (dump_file, "ref: ");
			  print_generic_expr (dump_file, ref->ref);
			}
		      fprintf (dump_file, " alias sets: %i->%i\n",
			       ao_ref_base_alias_set (ref),
			       ao_ref_alias_set (ref));
		    }
		  goto process_args;
		}
	      alias_stats.modref_use_may_alias++;
	    }
       }
    }

  base = ao_ref_base (ref);
  if (!base)
    return true;

  /* If the reference is based on a decl that is not aliased the call
     cannot possibly use it.  */
  if (DECL_P (base)
      && !may_be_aliased (base)
      /* But local statics can be used through recursion.  */
      && !is_global_var (base))
    goto process_args;

  if (int res = check_fnspec (call, ref, false))
    {
      if (res == 1)
	return true;
    }
  else
    goto process_args;

  /* Check if base is a global static variable that is not read
     by the function.  */
  if (callee != NULL_TREE && VAR_P (base) && TREE_STATIC (base))
    {
      struct cgraph_node *node = cgraph_node::get (callee);
      bitmap read;
      int id;

      /* FIXME: Callee can be an OMP builtin that does not have a call graph
	 node yet.  We should enforce that there are nodes for all decls in the
	 IL and remove this check instead.  */
      if (node
	  && (id = ipa_reference_var_uid (base)) != -1
	  && (read = ipa_reference_get_read_global (node))
	  && !bitmap_bit_p (read, id))
	goto process_args;
    }

  /* Check if the base variable is call-used.  */
  if (DECL_P (base))
    {
      if (pt_solution_includes (gimple_call_use_set (call), base))
	return true;
    }
  else if ((TREE_CODE (base) == MEM_REF
	    || TREE_CODE (base) == TARGET_MEM_REF)
	   && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
    {
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (TREE_OPERAND (base, 0));
      if (!pi)
	return true;

      if (pt_solutions_intersect (gimple_call_use_set (call), &pi->pt))
	return true;
    }
  else
    return true;

  /* Inspect call arguments for passed-by-value aliases.  */
process_args:
  for (i = 0; i < gimple_call_num_args (call); ++i)
    {
      tree op = gimple_call_arg (call, i);
      int flags = gimple_call_arg_flags (call, i);

      if (flags & (EAF_UNUSED | EAF_NO_DIRECT_READ))
	continue;

      if (TREE_CODE (op) == WITH_SIZE_EXPR)
	op = TREE_OPERAND (op, 0);

      if (TREE_CODE (op) != SSA_NAME
	  && !is_gimple_min_invariant (op))
	{
	  ao_ref r;
	  ao_ref_init (&r, op);
	  if (refs_may_alias_p_1 (&r, ref, tbaa_p))
	    return true;
	}
    }

  return false;
}

static bool
ref_maybe_used_by_call_p (gcall *call, ao_ref *ref, bool tbaa_p)
{
  bool res;
  res = ref_maybe_used_by_call_p_1 (call, ref, tbaa_p);
  if (res)
    ++alias_stats.ref_maybe_used_by_call_p_may_alias;
  else
    ++alias_stats.ref_maybe_used_by_call_p_no_alias;
  return res;
}


/* If the statement STMT may use the memory reference REF return
   true, otherwise return false.  */

bool
ref_maybe_used_by_stmt_p (gimple *stmt, ao_ref *ref, bool tbaa_p)
{
  if (is_gimple_assign (stmt))
    {
      tree rhs;

      /* All memory assign statements are single.  */
      if (!gimple_assign_single_p (stmt))
	return false;

      rhs = gimple_assign_rhs1 (stmt);
      if (is_gimple_reg (rhs)
	  || is_gimple_min_invariant (rhs)
	  || gimple_assign_rhs_code (stmt) == CONSTRUCTOR)
	return false;

      return refs_may_alias_p (rhs, ref, tbaa_p);
    }
  else if (is_gimple_call (stmt))
    return ref_maybe_used_by_call_p (as_a <gcall *> (stmt), ref, tbaa_p);
  else if (greturn *return_stmt = dyn_cast <greturn *> (stmt))
    {
      tree retval = gimple_return_retval (return_stmt);
      if (retval
	  && TREE_CODE (retval) != SSA_NAME
	  && !is_gimple_min_invariant (retval)
	  && refs_may_alias_p (retval, ref, tbaa_p))
	return true;
      /* If ref escapes the function then the return acts as a use.  */
      tree base = ao_ref_base (ref);
      if (!base)
	;
      else if (DECL_P (base))
	return is_global_var (base);
      else if (TREE_CODE (base) == MEM_REF
	       || TREE_CODE (base) == TARGET_MEM_REF)
	return ptr_deref_may_alias_global_p (TREE_OPERAND (base, 0), false);
      return false;
    }

  return true;
}

bool
ref_maybe_used_by_stmt_p (gimple *stmt, tree ref, bool tbaa_p)
{
  ao_ref r;
  ao_ref_init (&r, ref);
  return ref_maybe_used_by_stmt_p (stmt, &r, tbaa_p);
}

/* If the call in statement CALL may clobber the memory reference REF
   return true, otherwise return false.  */

bool
call_may_clobber_ref_p_1 (gcall *call, ao_ref *ref, bool tbaa_p)
{
  tree base;
  tree callee;

  /* If the call is pure or const it cannot clobber anything.  */
  if (gimple_call_flags (call)
      & (ECF_PURE|ECF_CONST|ECF_LOOPING_CONST_OR_PURE|ECF_NOVOPS))
    return false;
  if (gimple_call_internal_p (call))
    switch (auto fn = gimple_call_internal_fn (call))
      {
	/* Treat these internal calls like ECF_PURE for aliasing,
	   they don't write to any memory the program should care about.
	   They have important other side-effects, and read memory,
	   so can't be ECF_NOVOPS.  */
      case IFN_UBSAN_NULL:
      case IFN_UBSAN_BOUNDS:
      case IFN_UBSAN_VPTR:
      case IFN_UBSAN_OBJECT_SIZE:
      case IFN_UBSAN_PTR:
      case IFN_ASAN_CHECK:
	return false;
      case IFN_MASK_STORE:
      case IFN_LEN_STORE:
      case IFN_MASK_LEN_STORE:
      case IFN_MASK_STORE_LANES:
      case IFN_MASK_LEN_STORE_LANES:
	{
	  tree rhs = gimple_call_arg (call,
				      internal_fn_stored_value_index (fn));
	  ao_ref lhs_ref;
	  ao_ref_init_from_ptr_and_size (&lhs_ref, gimple_call_arg (call, 0),
					 TYPE_SIZE_UNIT (TREE_TYPE (rhs)));
	  /* We cannot make this a known-size access since otherwise
	     we disambiguate against refs to decls that are smaller.  */
	  lhs_ref.size = -1;
	  lhs_ref.ref_alias_set = lhs_ref.base_alias_set
	    = tbaa_p ? get_deref_alias_set
				   (TREE_TYPE (gimple_call_arg (call, 1))) : 0;
	  return refs_may_alias_p_1 (ref, &lhs_ref, tbaa_p);
	}
      default:
	break;
      }

  callee = gimple_call_fndecl (call);

  if (callee != NULL_TREE && !ref->volatile_p)
    {
      struct cgraph_node *node = cgraph_node::get (callee);
      if (node)
	{
	  modref_summary *summary = get_modref_function_summary (node);
	  if (summary)
	    {
	      if (!modref_may_conflict (call, summary->stores, ref, tbaa_p)
		  && (!summary->writes_errno
		      || !targetm.ref_may_alias_errno (ref)))
		{
		  alias_stats.modref_clobber_no_alias++;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file,
			       "ipa-modref: call stmt ");
		      print_gimple_stmt (dump_file, call, 0);
		      fprintf (dump_file,
			       "ipa-modref: call to %s does not clobber ",
			       node->dump_name ());
		      if (!ref->ref && ref->base)
			{
			  fprintf (dump_file, "base: ");
			  print_generic_expr (dump_file, ref->base);
			}
		      else if (ref->ref)
			{
			  fprintf (dump_file, "ref: ");
			  print_generic_expr (dump_file, ref->ref);
			}
		      fprintf (dump_file, " alias sets: %i->%i\n",
			       ao_ref_base_alias_set (ref),
			       ao_ref_alias_set (ref));
		    }
		  return false;
		}
	      alias_stats.modref_clobber_may_alias++;
	  }
	}
    }

  base = ao_ref_base (ref);
  if (!base)
    return true;

  if (TREE_CODE (base) == SSA_NAME
      || CONSTANT_CLASS_P (base))
    return false;

  /* A call that is not without side-effects might involve volatile
     accesses and thus conflicts with all other volatile accesses.  */
  if (ref->volatile_p)
    return true;

  /* If the reference is based on a decl that is not aliased the call
     cannot possibly clobber it.  */
  if (DECL_P (base)
      && !may_be_aliased (base)
      /* But local non-readonly statics can be modified through recursion
         or the call may implement a threading barrier which we must
	 treat as may-def.  */
      && (TREE_READONLY (base)
	  || !is_global_var (base)))
    return false;

  /* If the reference is based on a pointer that points to memory
     that may not be written to then the call cannot possibly clobber it.  */
  if ((TREE_CODE (base) == MEM_REF
       || TREE_CODE (base) == TARGET_MEM_REF)
      && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME
      && SSA_NAME_POINTS_TO_READONLY_MEMORY (TREE_OPERAND (base, 0)))
    return false;

  if (int res = check_fnspec (call, ref, true))
    {
      if (res == 1)
	return true;
    }
  else
    return false;

  /* Check if base is a global static variable that is not written
     by the function.  */
  if (callee != NULL_TREE && VAR_P (base) && TREE_STATIC (base))
    {
      struct cgraph_node *node = cgraph_node::get (callee);
      bitmap written;
      int id;

      if (node
	  && (id = ipa_reference_var_uid (base)) != -1
	  && (written = ipa_reference_get_written_global (node))
	  && !bitmap_bit_p (written, id))
	return false;
    }

  /* Check if the base variable is call-clobbered.  */
  if (DECL_P (base))
    return pt_solution_includes (gimple_call_clobber_set (call), base);
  else if ((TREE_CODE (base) == MEM_REF
	    || TREE_CODE (base) == TARGET_MEM_REF)
	   && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
    {
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (TREE_OPERAND (base, 0));
      if (!pi)
	return true;

      return pt_solutions_intersect (gimple_call_clobber_set (call), &pi->pt);
    }

  return true;
}

/* If the call in statement CALL may clobber the memory reference REF
   return true, otherwise return false.  */

bool
call_may_clobber_ref_p (gcall *call, tree ref, bool tbaa_p)
{
  bool res;
  ao_ref r;
  ao_ref_init (&r, ref);
  res = call_may_clobber_ref_p_1 (call, &r, tbaa_p);
  if (res)
    ++alias_stats.call_may_clobber_ref_p_may_alias;
  else
    ++alias_stats.call_may_clobber_ref_p_no_alias;
  return res;
}


/* If the statement STMT may clobber the memory reference REF return true,
   otherwise return false.  */

bool
stmt_may_clobber_ref_p_1 (gimple *stmt, ao_ref *ref, bool tbaa_p)
{
  if (is_gimple_call (stmt))
    {
      tree lhs = gimple_call_lhs (stmt);
      if (lhs
	  && TREE_CODE (lhs) != SSA_NAME)
	{
	  ao_ref r;
	  ao_ref_init (&r, lhs);
	  if (refs_may_alias_p_1 (ref, &r, tbaa_p))
	    return true;
	}

      return call_may_clobber_ref_p_1 (as_a <gcall *> (stmt), ref, tbaa_p);
    }
  else if (gimple_assign_single_p (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);
      if (TREE_CODE (lhs) != SSA_NAME)
	{
	  ao_ref r;
	  ao_ref_init (&r, lhs);
	  return refs_may_alias_p_1 (ref, &r, tbaa_p);
	}
    }
  else if (gimple_code (stmt) == GIMPLE_ASM)
    return true;

  return false;
}

bool
stmt_may_clobber_ref_p (gimple *stmt, tree ref, bool tbaa_p)
{
  ao_ref r;
  ao_ref_init (&r, ref);
  return stmt_may_clobber_ref_p_1 (stmt, &r, tbaa_p);
}

/* Return true if store1 and store2 described by corresponding tuples
   <BASE, OFFSET, SIZE, MAX_SIZE> have the same size and store to the same
   address.  */

static bool
same_addr_size_stores_p (tree base1, poly_int64 offset1, poly_int64 size1,
			 poly_int64 max_size1,
			 tree base2, poly_int64 offset2, poly_int64 size2,
			 poly_int64 max_size2)
{
  /* Offsets need to be 0.  */
  if (maybe_ne (offset1, 0)
      || maybe_ne (offset2, 0))
    return false;

  bool base1_obj_p = SSA_VAR_P (base1);
  bool base2_obj_p = SSA_VAR_P (base2);

  /* We need one object.  */
  if (base1_obj_p == base2_obj_p)
    return false;
  tree obj = base1_obj_p ? base1 : base2;

  /* And we need one MEM_REF.  */
  bool base1_memref_p = TREE_CODE (base1) == MEM_REF;
  bool base2_memref_p = TREE_CODE (base2) == MEM_REF;
  if (base1_memref_p == base2_memref_p)
    return false;
  tree memref = base1_memref_p ? base1 : base2;

  /* Sizes need to be valid.  */
  if (!known_size_p (max_size1)
      || !known_size_p (max_size2)
      || !known_size_p (size1)
      || !known_size_p (size2))
    return false;

  /* Max_size needs to match size.  */
  if (maybe_ne (max_size1, size1)
      || maybe_ne (max_size2, size2))
    return false;

  /* Sizes need to match.  */
  if (maybe_ne (size1, size2))
    return false;


  /* Check that memref is a store to pointer with singleton points-to info.  */
  if (!integer_zerop (TREE_OPERAND (memref, 1)))
    return false;
  tree ptr = TREE_OPERAND (memref, 0);
  if (TREE_CODE (ptr) != SSA_NAME)
    return false;
  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);
  unsigned int pt_uid;
  if (pi == NULL
      || !pt_solution_singleton_or_null_p (&pi->pt, &pt_uid))
    return false;

  /* Be conservative with non-call exceptions when the address might
     be NULL.  */
  if (cfun->can_throw_non_call_exceptions && pi->pt.null)
    return false;

  /* Check that ptr points relative to obj.  */
  unsigned int obj_uid = DECL_PT_UID (obj);
  if (obj_uid != pt_uid)
    return false;

  /* Check that the object size is the same as the store size.  That ensures us
     that ptr points to the start of obj.  */
  return (DECL_SIZE (obj)
	  && poly_int_tree_p (DECL_SIZE (obj))
	  && known_eq (wi::to_poly_offset (DECL_SIZE (obj)), size1));
}

/* Return true if REF is killed by an store described by
   BASE, OFFSET, SIZE and MAX_SIZE.  */

static bool
store_kills_ref_p (tree base, poly_int64 offset, poly_int64 size,
		   poly_int64 max_size, ao_ref *ref)
{
  poly_int64 ref_offset = ref->offset;
  /* We can get MEM[symbol: sZ, index: D.8862_1] here,
     so base == ref->base does not always hold.  */
  if (base != ref->base)
    {
      /* Try using points-to info.  */
      if (same_addr_size_stores_p (base, offset, size, max_size, ref->base,
				   ref->offset, ref->size, ref->max_size))
	return true;

      /* If both base and ref->base are MEM_REFs, only compare the
	 first operand, and if the second operand isn't equal constant,
	 try to add the offsets into offset and ref_offset.  */
      if (TREE_CODE (base) == MEM_REF && TREE_CODE (ref->base) == MEM_REF
	  && TREE_OPERAND (base, 0) == TREE_OPERAND (ref->base, 0))
	{
	  if (!tree_int_cst_equal (TREE_OPERAND (base, 1),
				   TREE_OPERAND (ref->base, 1)))
	    {
	      poly_offset_int off1 = mem_ref_offset (base);
	      off1 <<= LOG2_BITS_PER_UNIT;
	      off1 += offset;
	      poly_offset_int off2 = mem_ref_offset (ref->base);
	      off2 <<= LOG2_BITS_PER_UNIT;
	      off2 += ref_offset;
	      if (!off1.to_shwi (&offset) || !off2.to_shwi (&ref_offset))
		size = -1;
	    }
	}
      else
	size = -1;
    }
  /* For a must-alias check we need to be able to constrain
     the access properly.  */
  return (known_eq (size, max_size)
	  && known_subrange_p (ref_offset, ref->max_size, offset, size));
}

/* If STMT kills the memory reference REF return true, otherwise
   return false.  */

bool
stmt_kills_ref_p (gimple *stmt, ao_ref *ref)
{
  if (!ao_ref_base (ref))
    return false;

  if (gimple_has_lhs (stmt)
      && TREE_CODE (gimple_get_lhs (stmt)) != SSA_NAME
      /* The assignment is not necessarily carried out if it can throw
	 and we can catch it in the current function where we could inspect
	 the previous value.  Similarly if the function can throw externally
	 and the ref does not die on the function return.
	 ???  We only need to care about the RHS throwing.  For aggregate
	 assignments or similar calls and non-call exceptions the LHS
	 might throw as well.
	 ???  We also should care about possible longjmp, but since we
	 do not understand that longjmp is not using global memory we will
	 not consider a kill here since the function call will be considered
	 as possibly using REF.	 */
      && !stmt_can_throw_internal (cfun, stmt)
      && (!stmt_can_throw_external (cfun, stmt)
	  || !ref_may_alias_global_p (ref, false)))
    {
      tree lhs = gimple_get_lhs (stmt);
      /* If LHS is literally a base of the access we are done.  */
      if (ref->ref)
	{
	  tree base = ref->ref;
	  tree innermost_dropped_array_ref = NULL_TREE;
	  if (handled_component_p (base))
	    {
	      tree saved_lhs0 = NULL_TREE;
	      if (handled_component_p (lhs))
		{
		  saved_lhs0 = TREE_OPERAND (lhs, 0);
		  TREE_OPERAND (lhs, 0) = integer_zero_node;
		}
	      do
		{
		  /* Just compare the outermost handled component, if
		     they are equal we have found a possible common
		     base.  */
		  tree saved_base0 = TREE_OPERAND (base, 0);
		  TREE_OPERAND (base, 0) = integer_zero_node;
		  bool res = operand_equal_p (lhs, base, 0);
		  TREE_OPERAND (base, 0) = saved_base0;
		  if (res)
		    break;
		  /* Remember if we drop an array-ref that we need to
		     double-check not being at struct end.  */
		  if (TREE_CODE (base) == ARRAY_REF
		      || TREE_CODE (base) == ARRAY_RANGE_REF)
		    innermost_dropped_array_ref = base;
		  /* Otherwise drop handled components of the access.  */
		  base = saved_base0;
		}
	      while (handled_component_p (base));
	      if (saved_lhs0)
		TREE_OPERAND (lhs, 0) = saved_lhs0;
	    }
	  /* Finally check if the lhs has the same address and size as the
	     base candidate of the access.  Watch out if we have dropped
	     an array-ref that might have flexible size, this means ref->ref
	     may be outside of the TYPE_SIZE of its base.  */
	  if ((! innermost_dropped_array_ref
	       || ! array_ref_flexible_size_p (innermost_dropped_array_ref))
	      && (lhs == base
		  || (((TYPE_SIZE (TREE_TYPE (lhs))
			== TYPE_SIZE (TREE_TYPE (base)))
		       || (TYPE_SIZE (TREE_TYPE (lhs))
			   && TYPE_SIZE (TREE_TYPE (base))
			   && operand_equal_p (TYPE_SIZE (TREE_TYPE (lhs)),
					       TYPE_SIZE (TREE_TYPE (base)),
					       0)))
		      && operand_equal_p (lhs, base,
					  OEP_ADDRESS_OF
					  | OEP_MATCH_SIDE_EFFECTS))))
	    {
	      ++alias_stats.stmt_kills_ref_p_yes;
	      return true;
	    }
	}

      /* Now look for non-literal equal bases with the restriction of
         handling constant offset and size.  */
      /* For a must-alias check we need to be able to constrain
	 the access properly.  */
      if (!ref->max_size_known_p ())
	{
	  ++alias_stats.stmt_kills_ref_p_no;
	  return false;
	}
      poly_int64 size, offset, max_size;
      bool reverse;
      tree base = get_ref_base_and_extent (lhs, &offset, &size, &max_size,
					   &reverse);
      if (store_kills_ref_p (base, offset, size, max_size, ref))
	{
	  ++alias_stats.stmt_kills_ref_p_yes;
	  return true;
	}
    }

  if (is_gimple_call (stmt))
    {
      tree callee = gimple_call_fndecl (stmt);
      struct cgraph_node *node;
      modref_summary *summary;

      /* Try to disambiguate using modref summary.  Modref records a vector
	 of stores with known offsets relative to function parameters that must
	 happen every execution of function.  Find if we have a matching
	 store and verify that function can not use the value.  */
      if (callee != NULL_TREE
	  && (node = cgraph_node::get (callee)) != NULL
	  && node->binds_to_current_def_p ()
	  && (summary = get_modref_function_summary (node)) != NULL
	  && summary->kills.length ()
	  /* Check that we can not trap while evaulating function
	     parameters.  This check is overly conservative.  */
	  && (!cfun->can_throw_non_call_exceptions
	      || (!stmt_can_throw_internal (cfun, stmt)
		  && (!stmt_can_throw_external (cfun, stmt)
		      || !ref_may_alias_global_p (ref, false)))))
	{
	  for (auto kill : summary->kills)
	    {
	      ao_ref dref;

	      /* We only can do useful compares if we know the access range
		 precisely.  */
	      if (!kill.get_ao_ref (as_a <gcall *> (stmt), &dref))
		continue;
	      if (store_kills_ref_p (ao_ref_base (&dref), dref.offset,
				     dref.size, dref.max_size, ref))
		{
		  /* For store to be killed it needs to not be used
		     earlier.  */
		  if (ref_maybe_used_by_call_p_1 (as_a <gcall *> (stmt), ref,
						  true)
		      || !dbg_cnt (ipa_mod_ref))
		    break;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file,
			       "ipa-modref: call stmt ");
		      print_gimple_stmt (dump_file, stmt, 0);
		      fprintf (dump_file,
			       "ipa-modref: call to %s kills ",
			       node->dump_name ());
		      print_generic_expr (dump_file, ref->base);
		      fprintf (dump_file, "\n");
		    }
		    ++alias_stats.modref_kill_yes;
		    return true;
		}
	    }
	  ++alias_stats.modref_kill_no;
	}
      if (callee != NULL_TREE
	  && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	switch (DECL_FUNCTION_CODE (callee))
	  {
	  case BUILT_IN_FREE:
	    {
	      tree ptr = gimple_call_arg (stmt, 0);
	      tree base = ao_ref_base (ref);
	      if (base && TREE_CODE (base) == MEM_REF
		  && TREE_OPERAND (base, 0) == ptr)
		{
		  ++alias_stats.stmt_kills_ref_p_yes;
		  return true;
		}
	      break;
	    }

	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMPCPY:
	  case BUILT_IN_MEMMOVE:
	  case BUILT_IN_MEMSET:
	  case BUILT_IN_MEMCPY_CHK:
	  case BUILT_IN_MEMPCPY_CHK:
	  case BUILT_IN_MEMMOVE_CHK:
	  case BUILT_IN_MEMSET_CHK:
	  case BUILT_IN_STRNCPY:
	  case BUILT_IN_STPNCPY:
	  case BUILT_IN_CALLOC:
	    {
	      /* For a must-alias check we need to be able to constrain
		 the access properly.  */
	      if (!ref->max_size_known_p ())
		{
		  ++alias_stats.stmt_kills_ref_p_no;
		  return false;
		}
	      tree dest;
	      tree len;

	      /* In execution order a calloc call will never kill
		 anything.  However, DSE will (ab)use this interface
		 to ask if a calloc call writes the same memory locations
		 as a later assignment, memset, etc.  So handle calloc
		 in the expected way.  */
	      if (DECL_FUNCTION_CODE (callee) == BUILT_IN_CALLOC)
		{
		  tree arg0 = gimple_call_arg (stmt, 0);
		  tree arg1 = gimple_call_arg (stmt, 1);
		  if (TREE_CODE (arg0) != INTEGER_CST
		      || TREE_CODE (arg1) != INTEGER_CST)
		    {
		      ++alias_stats.stmt_kills_ref_p_no;
		      return false;
		    }

		  dest = gimple_call_lhs (stmt);
		  if (!dest)
		    {
		      ++alias_stats.stmt_kills_ref_p_no;
		      return false;
		    }
		  len = fold_build2 (MULT_EXPR, TREE_TYPE (arg0), arg0, arg1);
		}
	      else
		{
		  dest = gimple_call_arg (stmt, 0);
		  len = gimple_call_arg (stmt, 2);
		}
	      if (!poly_int_tree_p (len))
		return false;
	      ao_ref dref;
	      ao_ref_init_from_ptr_and_size (&dref, dest, len);
	      if (store_kills_ref_p (ao_ref_base (&dref), dref.offset,
				     dref.size, dref.max_size, ref))
		{
		  ++alias_stats.stmt_kills_ref_p_yes;
		  return true;
		}
	      break;
	    }

	  case BUILT_IN_VA_END:
	    {
	      tree ptr = gimple_call_arg (stmt, 0);
	      if (TREE_CODE (ptr) == ADDR_EXPR)
		{
		  tree base = ao_ref_base (ref);
		  if (TREE_OPERAND (ptr, 0) == base)
		    {
		      ++alias_stats.stmt_kills_ref_p_yes;
		      return true;
		    }
		}
	      break;
	    }

	  default:;
	  }
    }
  ++alias_stats.stmt_kills_ref_p_no;
  return false;
}

bool
stmt_kills_ref_p (gimple *stmt, tree ref)
{
  ao_ref r;
  ao_ref_init (&r, ref);
  return stmt_kills_ref_p (stmt, &r);
}

/* Return whether REF can be subject to store data races.  */

bool
ref_can_have_store_data_races (tree ref)
{
  /* With -fallow-store-data-races do not care about them.  */
  if (flag_store_data_races)
    return false;

  tree base = get_base_address (ref);
  if (auto_var_p (base)
      && ! may_be_aliased (base))
    /* Automatic variables not aliased are not subject to
       data races.  */
    return false;

  return true;
}


/* Walk the virtual use-def chain of VUSE until hitting the virtual operand
   TARGET or a statement clobbering the memory reference REF in which
   case false is returned.  The walk starts with VUSE, one argument of PHI.  */

static bool
maybe_skip_until (gimple *phi, tree &target, basic_block target_bb,
		  ao_ref *ref, tree vuse, bool tbaa_p, unsigned int &limit,
		  bitmap *visited, bool abort_on_visited,
		  void *(*translate)(ao_ref *, tree, void *, translate_flags *),
		  translate_flags disambiguate_only,
		  void *data)
{
  basic_block bb = gimple_bb (phi);

  if (!*visited)
    {
      *visited = BITMAP_ALLOC (NULL);
      bitmap_tree_view (*visited);
    }

  bitmap_set_bit (*visited, SSA_NAME_VERSION (PHI_RESULT (phi)));

  /* Walk until we hit the target.  */
  while (vuse != target)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (vuse);
      /* If we are searching for the target VUSE by walking up to
         TARGET_BB dominating the original PHI we are finished once
	 we reach a default def or a definition in a block dominating
	 that block.  Update TARGET and return.  */
      if (!target
	  && (gimple_nop_p (def_stmt)
	      || dominated_by_p (CDI_DOMINATORS,
				 target_bb, gimple_bb (def_stmt))))
	{
	  target = vuse;
	  return true;
	}

      /* Recurse for PHI nodes.  */
      if (gimple_code (def_stmt) == GIMPLE_PHI)
	{
	  /* An already visited PHI node ends the walk successfully.  */
	  if (bitmap_bit_p (*visited, SSA_NAME_VERSION (PHI_RESULT (def_stmt))))
	    return !abort_on_visited;
	  vuse = get_continuation_for_phi (def_stmt, ref, tbaa_p, limit,
					   visited, abort_on_visited,
					   translate, data, disambiguate_only);
	  if (!vuse)
	    return false;
	  continue;
	}
      else if (gimple_nop_p (def_stmt))
	return false;
      else
	{
	  /* A clobbering statement or the end of the IL ends it failing.  */
	  if ((int)limit <= 0)
	    return false;
	  --limit;
	  if (stmt_may_clobber_ref_p_1 (def_stmt, ref, tbaa_p))
	    {
	      translate_flags tf = disambiguate_only;
	      if (translate
		  && (*translate) (ref, vuse, data, &tf) == NULL)
		;
	      else
		return false;
	    }
	}
      /* If we reach a new basic-block see if we already skipped it
         in a previous walk that ended successfully.  */
      if (gimple_bb (def_stmt) != bb)
	{
	  if (!bitmap_set_bit (*visited, SSA_NAME_VERSION (vuse)))
	    return !abort_on_visited;
	  bb = gimple_bb (def_stmt);
	}
      vuse = gimple_vuse (def_stmt);
    }
  return true;
}


/* Starting from a PHI node for the virtual operand of the memory reference
   REF find a continuation virtual operand that allows to continue walking
   statements dominating PHI skipping only statements that cannot possibly
   clobber REF.  Decrements LIMIT for each alias disambiguation done
   and aborts the walk, returning NULL_TREE if it reaches zero.
   Returns NULL_TREE if no suitable virtual operand can be found.  */

tree
get_continuation_for_phi (gimple *phi, ao_ref *ref, bool tbaa_p,
			  unsigned int &limit, bitmap *visited,
			  bool abort_on_visited,
			  void *(*translate)(ao_ref *, tree, void *,
					     translate_flags *),
			  void *data,
			  translate_flags disambiguate_only)
{
  unsigned nargs = gimple_phi_num_args (phi);

  /* Through a single-argument PHI we can simply look through.  */
  if (nargs == 1)
    return PHI_ARG_DEF (phi, 0);

  /* For two or more arguments try to pairwise skip non-aliasing code
     until we hit the phi argument definition that dominates the other one.  */
  basic_block phi_bb = gimple_bb (phi);
  tree arg0, arg1;
  unsigned i;

  /* Find a candidate for the virtual operand which definition
     dominates those of all others.  */
  /* First look if any of the args themselves satisfy this.  */
  for (i = 0; i < nargs; ++i)
    {
      arg0 = PHI_ARG_DEF (phi, i);
      if (SSA_NAME_IS_DEFAULT_DEF (arg0))
	break;
      basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (arg0));
      if (def_bb != phi_bb
	  && dominated_by_p (CDI_DOMINATORS, phi_bb, def_bb))
	break;
      arg0 = NULL_TREE;
    }
  /* If not, look if we can reach such candidate by walking defs
     until we hit the immediate dominator.  maybe_skip_until will
     do that for us.  */
  basic_block dom = get_immediate_dominator (CDI_DOMINATORS, phi_bb);

  /* Then check against the (to be) found candidate.  */
  for (i = 0; i < nargs; ++i)
    {
      arg1 = PHI_ARG_DEF (phi, i);
      if (arg1 == arg0)
	;
      else if (! maybe_skip_until (phi, arg0, dom, ref, arg1, tbaa_p,
				   limit, visited,
				   abort_on_visited,
				   translate,
				   /* Do not valueize when walking over
				      backedges.  */
				   dominated_by_p
				     (CDI_DOMINATORS,
				      gimple_bb (SSA_NAME_DEF_STMT (arg1)),
				      phi_bb)
				   ? TR_DISAMBIGUATE
				   : disambiguate_only, data))
	return NULL_TREE;
    }

  return arg0;
}

/* Based on the memory reference REF and its virtual use VUSE call
   WALKER for each virtual use that is equivalent to VUSE, including VUSE
   itself.  That is, for each virtual use for which its defining statement
   does not clobber REF.

   WALKER is called with REF, the current virtual use and DATA.  If
   WALKER returns non-NULL the walk stops and its result is returned.
   At the end of a non-successful walk NULL is returned.

   TRANSLATE if non-NULL is called with a pointer to REF, the virtual
   use which definition is a statement that may clobber REF and DATA.
   If TRANSLATE returns (void *)-1 the walk stops and NULL is returned.
   If TRANSLATE returns non-NULL the walk stops and its result is returned.
   If TRANSLATE returns NULL the walk continues and TRANSLATE is supposed
   to adjust REF and *DATA to make that valid.

   VALUEIZE if non-NULL is called with the next VUSE that is considered
   and return value is substituted for that.  This can be used to
   implement optimistic value-numbering for example.  Note that the
   VUSE argument is assumed to be valueized already.

   LIMIT specifies the number of alias queries we are allowed to do,
   the walk stops when it reaches zero and NULL is returned.  LIMIT
   is decremented by the number of alias queries (plus adjustments
   done by the callbacks) upon return.

   TODO: Cache the vector of equivalent vuses per ref, vuse pair.  */

void *
walk_non_aliased_vuses (ao_ref *ref, tree vuse, bool tbaa_p,
			void *(*walker)(ao_ref *, tree, void *),
			void *(*translate)(ao_ref *, tree, void *,
					   translate_flags *),
			tree (*valueize)(tree),
			unsigned &limit, void *data)
{
  bitmap visited = NULL;
  void *res;
  bool translated = false;

  timevar_push (TV_ALIAS_STMT_WALK);

  do
    {
      gimple *def_stmt;

      /* ???  Do we want to account this to TV_ALIAS_STMT_WALK?  */
      res = (*walker) (ref, vuse, data);
      /* Abort walk.  */
      if (res == (void *)-1)
	{
	  res = NULL;
	  break;
	}
      /* Lookup succeeded.  */
      else if (res != NULL)
	break;

      if (valueize)
	{
	  vuse = valueize (vuse);
	  if (!vuse)
	    {
	      res = NULL;
	      break;
	    }
	}
      def_stmt = SSA_NAME_DEF_STMT (vuse);
      if (gimple_nop_p (def_stmt))
	break;
      else if (gimple_code (def_stmt) == GIMPLE_PHI)
	vuse = get_continuation_for_phi (def_stmt, ref, tbaa_p, limit,
					 &visited, translated, translate, data);
      else
	{
	  if ((int)limit <= 0)
	    {
	      res = NULL;
	      break;
	    }
	  --limit;
	  if (stmt_may_clobber_ref_p_1 (def_stmt, ref, tbaa_p))
	    {
	      if (!translate)
		break;
	      translate_flags disambiguate_only = TR_TRANSLATE;
	      res = (*translate) (ref, vuse, data, &disambiguate_only);
	      /* Failed lookup and translation.  */
	      if (res == (void *)-1)
		{
		  res = NULL;
		  break;
		}
	      /* Lookup succeeded.  */
	      else if (res != NULL)
		break;
	      /* Translation succeeded, continue walking.  */
	      translated = translated || disambiguate_only == TR_TRANSLATE;
	    }
	  vuse = gimple_vuse (def_stmt);
	}
    }
  while (vuse);

  if (visited)
    BITMAP_FREE (visited);

  timevar_pop (TV_ALIAS_STMT_WALK);

  return res;
}


/* Based on the memory reference REF call WALKER for each vdef whose
   defining statement may clobber REF, starting with VDEF.  If REF
   is NULL_TREE, each defining statement is visited.

   WALKER is called with REF, the current vdef and DATA.  If WALKER
   returns true the walk is stopped, otherwise it continues.

   If function entry is reached, FUNCTION_ENTRY_REACHED is set to true.
   The pointer may be NULL and then we do not track this information.

   At PHI nodes walk_aliased_vdefs forks into one walk for each
   PHI argument (but only one walk continues at merge points), the
   return value is true if any of the walks was successful.

   The function returns the number of statements walked or -1 if
   LIMIT stmts were walked and the walk was aborted at this point.
   If LIMIT is zero the walk is not aborted.  */

static int
walk_aliased_vdefs_1 (ao_ref *ref, tree vdef,
		      bool (*walker)(ao_ref *, tree, void *), void *data,
		      bitmap *visited, unsigned int cnt,
		      bool *function_entry_reached, unsigned limit)
{
  do
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (vdef);

      if (*visited
	  && !bitmap_set_bit (*visited, SSA_NAME_VERSION (vdef)))
	return cnt;

      if (gimple_nop_p (def_stmt))
	{
	  if (function_entry_reached)
	    *function_entry_reached = true;
	  return cnt;
	}
      else if (gimple_code (def_stmt) == GIMPLE_PHI)
	{
	  unsigned i;
	  if (!*visited)
	    {
	      *visited = BITMAP_ALLOC (NULL);
	      bitmap_tree_view (*visited);
	    }
	  for (i = 0; i < gimple_phi_num_args (def_stmt); ++i)
	    {
	      int res = walk_aliased_vdefs_1 (ref,
					      gimple_phi_arg_def (def_stmt, i),
					      walker, data, visited, cnt,
					      function_entry_reached, limit);
	      if (res == -1)
		return -1;
	      cnt = res;
	    }
	  return cnt;
	}

      /* ???  Do we want to account this to TV_ALIAS_STMT_WALK?  */
      cnt++;
      if (cnt == limit)
	return -1;
      if ((!ref
	   || stmt_may_clobber_ref_p_1 (def_stmt, ref))
	  && (*walker) (ref, vdef, data))
	return cnt;

      vdef = gimple_vuse (def_stmt);
    }
  while (1);
}

int
walk_aliased_vdefs (ao_ref *ref, tree vdef,
		    bool (*walker)(ao_ref *, tree, void *), void *data,
		    bitmap *visited,
		    bool *function_entry_reached, unsigned int limit)
{
  bitmap local_visited = NULL;
  int ret;

  timevar_push (TV_ALIAS_STMT_WALK);

  if (function_entry_reached)
    *function_entry_reached = false;

  ret = walk_aliased_vdefs_1 (ref, vdef, walker, data,
			      visited ? visited : &local_visited, 0,
			      function_entry_reached, limit);
  if (local_visited)
    BITMAP_FREE (local_visited);

  timevar_pop (TV_ALIAS_STMT_WALK);

  return ret;
}

/* Verify validity of the fnspec string.
   See attr-fnspec.h for details.  */

void
attr_fnspec::verify ()
{
  bool err = false;
  if (!len)
    return;

  /* Check return value specifier.  */
  if (len < return_desc_size)
    err = true;
  else if ((len - return_desc_size) % arg_desc_size)
    err = true;
  else if ((str[0] < '1' || str[0] > '4')
	   && str[0] != '.' && str[0] != 'm')
    err = true;

  switch (str[1])
    {
      case ' ':
      case 'p':
      case 'P':
      case 'c':
      case 'C':
	break;
      default:
	err = true;
    }
  if (err)
    internal_error ("invalid fn spec attribute \"%s\"", str);

  /* Now check all parameters.  */
  for (unsigned int i = 0; arg_specified_p (i); i++)
    {
      unsigned int idx = arg_idx (i);
      switch (str[idx])
	{
	  case 'x':
	  case 'X':
	  case 'r':
	  case 'R':
	  case 'o':
	  case 'O':
	  case 'w':
	  case 'W':
	  case '.':
	    if ((str[idx + 1] >= '1' && str[idx + 1] <= '9')
		|| str[idx + 1] == 't')
	      {
		if (str[idx] != 'r' && str[idx] != 'R'
		    && str[idx] != 'w' && str[idx] != 'W'
		    && str[idx] != 'o' && str[idx] != 'O')
		  err = true;
		if (str[idx + 1] != 't'
		    /* Size specified is scalar, so it should be described
		       by ". " if specified at all.  */
		    && (arg_specified_p (str[idx + 1] - '1')
			&& str[arg_idx (str[idx + 1] - '1')] != '.'))
		  err = true;
	      }
	    else if (str[idx + 1] != ' ')
	      err = true;
	    break;
	  default:
	    if (str[idx] < '1' || str[idx] > '9')
	      err = true;
	}
      if (err)
	internal_error ("invalid fn spec attribute \"%s\" arg %i", str, i);
    }
}

/* Return ture if TYPE1 and TYPE2 will always give the same answer
   when compared with other types using same_type_for_tbaa.  */

static bool
types_equal_for_same_type_for_tbaa_p (tree type1, tree type2,
				      bool lto_streaming_safe)
{
  /* We use same_type_for_tbaa_p to match types in the access path.
     This check is overly conservative.  */
  type1 = TYPE_MAIN_VARIANT (type1);
  type2 = TYPE_MAIN_VARIANT (type2);

  if (TYPE_STRUCTURAL_EQUALITY_P (type1)
      != TYPE_STRUCTURAL_EQUALITY_P (type2))
    return false;
  if (TYPE_STRUCTURAL_EQUALITY_P (type1))
    return true;

  if (lto_streaming_safe)
    return type1 == type2;
  else
    return TYPE_CANONICAL (type1) == TYPE_CANONICAL (type2);
}

/* Return ture if TYPE1 and TYPE2 will always give the same answer
   when compared with other types using same_type_for_tbaa.  */

bool
types_equal_for_same_type_for_tbaa_p (tree type1, tree type2)
{
  return types_equal_for_same_type_for_tbaa_p (type1, type2,
					       lto_streaming_expected_p ());
}

/* Compare REF1 and REF2 and return flags specifying their differences.
   If LTO_STREAMING_SAFE is true do not use alias sets and canonical
   types that are going to be recomputed.
   If TBAA is true also compare TBAA metadata.  */

int
ao_compare::compare_ao_refs (ao_ref *ref1, ao_ref *ref2,
			     bool lto_streaming_safe,
			     bool tbaa)
{
  if (TREE_THIS_VOLATILE (ref1->ref) != TREE_THIS_VOLATILE (ref2->ref))
    return SEMANTICS;
  tree base1 = ao_ref_base (ref1);
  tree base2 = ao_ref_base (ref2);

  if (!known_eq (ref1->offset, ref2->offset)
      || !known_eq (ref1->size, ref2->size)
      || !known_eq (ref1->max_size, ref2->max_size))
    return SEMANTICS;

  /* For variable accesses we need to compare actual paths
     to check that both refs are accessing same address and the access size.  */
  if (!known_eq (ref1->size, ref1->max_size))
    {
      if (!operand_equal_p (TYPE_SIZE (TREE_TYPE (ref1->ref)),
			    TYPE_SIZE (TREE_TYPE (ref2->ref)), 0))
	return SEMANTICS;
      tree r1 = ref1->ref;
      tree r2 = ref2->ref;

      /* Handle toplevel COMPONENT_REFs of bitfields.
	 Those are special since they are not allowed in
	 ADDR_EXPR.  */
      if (TREE_CODE (r1) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (r1, 1)))
	{
	  if (TREE_CODE (r2) != COMPONENT_REF
	      || !DECL_BIT_FIELD (TREE_OPERAND (r2, 1)))
	    return SEMANTICS;
	  tree field1 = TREE_OPERAND (r1, 1);
	  tree field2 = TREE_OPERAND (r2, 1);
	  if (!operand_equal_p (DECL_FIELD_OFFSET (field1),
				DECL_FIELD_OFFSET (field2), 0)
	      || !operand_equal_p (DECL_FIELD_BIT_OFFSET (field1),
				   DECL_FIELD_BIT_OFFSET (field2), 0)
	      || !operand_equal_p (DECL_SIZE (field1), DECL_SIZE (field2), 0)
	      || !types_compatible_p (TREE_TYPE (r1),
				      TREE_TYPE (r2)))
	    return SEMANTICS;
	  r1 = TREE_OPERAND (r1, 0);
	  r2 = TREE_OPERAND (r2, 0);
	}
      else if (TREE_CODE (r2) == COMPONENT_REF
	       && DECL_BIT_FIELD (TREE_OPERAND (r2, 1)))
	return SEMANTICS;

      /* Similarly for bit field refs.  */
      if (TREE_CODE (r1) == BIT_FIELD_REF)
	{
 	  if (TREE_CODE (r2) != BIT_FIELD_REF
	      || !operand_equal_p (TREE_OPERAND (r1, 1),
				   TREE_OPERAND (r2, 1), 0)
	      || !operand_equal_p (TREE_OPERAND (r1, 2),
				   TREE_OPERAND (r2, 2), 0)
	      || !types_compatible_p (TREE_TYPE (r1),
				      TREE_TYPE (r2)))
	    return SEMANTICS;
	  r1 = TREE_OPERAND (r1, 0);
	  r2 = TREE_OPERAND (r2, 0);
	}
      else if (TREE_CODE (r2) == BIT_FIELD_REF)
	return SEMANTICS;

      /* Now we can compare the address of actual memory access.  */
      if (!operand_equal_p (r1, r2, OEP_ADDRESS_OF | OEP_MATCH_SIDE_EFFECTS))
	return SEMANTICS;
    }
  /* For constant accesses we get more matches by comparing offset only.  */
  else if (!operand_equal_p (base1, base2,
			     OEP_ADDRESS_OF | OEP_MATCH_SIDE_EFFECTS))
    return SEMANTICS;

  /* We can't simply use get_object_alignment_1 on the full
     reference as for accesses with variable indexes this reports
     too conservative alignment.  */
  unsigned int align1, align2;
  unsigned HOST_WIDE_INT bitpos1, bitpos2;
  bool known1 = get_object_alignment_1 (base1, &align1, &bitpos1);
  bool known2 = get_object_alignment_1 (base2, &align2, &bitpos2);
  /* ??? For MEMREF get_object_alignment_1 determines aligned from
     TYPE_ALIGN but still returns false.  This seem to contradict
     its description.  So compare even if alignment is unknown.   */
  if (known1 != known2
      || (bitpos1 != bitpos2 || align1 != align2))
    return SEMANTICS;

  /* Now we know that accesses are semantically same.  */
  int flags = 0;

  /* ao_ref_base strips inner MEM_REF [&decl], recover from that here.  */
  tree rbase1 = ref1->ref;
  if (rbase1)
    while (handled_component_p (rbase1))
      rbase1 = TREE_OPERAND (rbase1, 0);
  tree rbase2 = ref2->ref;
  while (handled_component_p (rbase2))
    rbase2 = TREE_OPERAND (rbase2, 0);

  /* MEM_REFs and TARGET_MEM_REFs record dependence cliques which are used to
     implement restrict pointers.  MR_DEPENDENCE_CLIQUE 0 means no information.
     Otherwise we need to match bases and cliques.  */
  if ((((TREE_CODE (rbase1) == MEM_REF || TREE_CODE (rbase1) == TARGET_MEM_REF)
	&& MR_DEPENDENCE_CLIQUE (rbase1))
       || ((TREE_CODE (rbase2) == MEM_REF || TREE_CODE (rbase2) == TARGET_MEM_REF)
	   && MR_DEPENDENCE_CLIQUE (rbase2)))
      && (TREE_CODE (rbase1) != TREE_CODE (rbase2)
	  || MR_DEPENDENCE_CLIQUE (rbase1) != MR_DEPENDENCE_CLIQUE (rbase2)
	  || (MR_DEPENDENCE_BASE (rbase1) != MR_DEPENDENCE_BASE (rbase2))))
    flags |= DEPENDENCE_CLIQUE;

  if (!tbaa)
    return flags;

  /* Alias sets are not stable across LTO sreaming; be conservative here
     and compare types the alias sets are ultimately based on.  */
  if (lto_streaming_safe)
    {
      tree t1 = ao_ref_alias_ptr_type (ref1);
      tree t2 = ao_ref_alias_ptr_type (ref2);
      if (!alias_ptr_types_compatible_p (t1, t2))
	flags |= REF_ALIAS_SET;

      t1 = ao_ref_base_alias_ptr_type (ref1);
      t2 = ao_ref_base_alias_ptr_type (ref2);
      if (!alias_ptr_types_compatible_p (t1, t2))
	flags |= BASE_ALIAS_SET;
    }
  else
    {
      if (ao_ref_alias_set (ref1) != ao_ref_alias_set (ref2))
	flags |= REF_ALIAS_SET;
      if (ao_ref_base_alias_set (ref1) != ao_ref_base_alias_set (ref2))
	flags |= BASE_ALIAS_SET;
    }

  /* Access path is used only on non-view-converted references.  */
  bool view_converted = view_converted_memref_p (rbase1);
  if (view_converted_memref_p (rbase2) != view_converted)
    return flags | ACCESS_PATH;
  else if (view_converted)
    return flags;


  /* Find start of access paths and look for trailing arrays.  */
  tree c1 = ref1->ref, c2 = ref2->ref;
  tree end_struct_ref1 = NULL, end_struct_ref2 = NULL;
  int nskipped1 = 0, nskipped2 = 0;
  int i = 0;

  for (tree p1 = ref1->ref; handled_component_p (p1); p1 = TREE_OPERAND (p1, 0))
    {
      if (component_ref_to_zero_sized_trailing_array_p (p1))
	end_struct_ref1 = p1;
      if (ends_tbaa_access_path_p (p1))
	c1 = p1, nskipped1 = i;
      i++;
    }
  i = 0;
  for (tree p2 = ref2->ref; handled_component_p (p2); p2 = TREE_OPERAND (p2, 0))
    {
      if (component_ref_to_zero_sized_trailing_array_p (p2))
	end_struct_ref2 = p2;
      if (ends_tbaa_access_path_p (p2))
	c2 = p2, nskipped2 = i;
      i++;
    }

  /* For variable accesses we can not rely on offset match bellow.
     We know that paths are struturally same, so only check that
     starts of TBAA paths did not diverge.  */
  if (!known_eq (ref1->size, ref1->max_size)
      && nskipped1 != nskipped2)
    return flags | ACCESS_PATH;

  /* Information about trailing refs is used by
     aliasing_component_refs_p that is applied only if paths
     has handled components..  */
  if (!handled_component_p (c1) && !handled_component_p (c2))
    ;
  else if ((end_struct_ref1 != NULL) != (end_struct_ref2 != NULL))
    return flags | ACCESS_PATH;
  if (end_struct_ref1
      && same_type_for_tbaa (TREE_TYPE (end_struct_ref1),
			     TREE_TYPE (end_struct_ref2)) != 1)
    return flags | ACCESS_PATH;

  /* Now compare all handled components of the access path.
     We have three oracles that cares about access paths:
       - aliasing_component_refs_p
       - nonoverlapping_refs_since_match_p
       - nonoverlapping_component_refs_p
     We need to match things these oracles compare.

     It is only necessary to check types for compatibility
     and offsets.  Rest of what oracles compares are actual
     addresses.  Those are already known to be same:
       - for constant accesses we check offsets
       - for variable accesses we already matched
	 the path lexically with operand_equal_p.  */
  while (true)
    {
      bool comp1 = handled_component_p (c1);
      bool comp2 = handled_component_p (c2);

      if (comp1 != comp2)
	return flags | ACCESS_PATH;
      if (!comp1)
	break;

      if (TREE_CODE (c1) != TREE_CODE (c2))
	return flags | ACCESS_PATH;

      /* aliasing_component_refs_p attempts to find type match within
	 the paths.  For that reason both types needs to be equal
	 with respect to same_type_for_tbaa_p.  */
      if (!types_equal_for_same_type_for_tbaa_p (TREE_TYPE (c1),
						 TREE_TYPE (c2),
						 lto_streaming_safe))
	return flags | ACCESS_PATH;
      if (component_ref_to_zero_sized_trailing_array_p (c1)
	  != component_ref_to_zero_sized_trailing_array_p (c2))
	return flags | ACCESS_PATH;

      /* aliasing_matching_component_refs_p compares
	 offsets within the path.  Other properties are ignored.
	 Do not bother to verify offsets in variable accesses.  Here we
	 already compared them by operand_equal_p so they are
	 structurally same.  */
      if (!known_eq (ref1->size, ref1->max_size))
	{
	  poly_int64 offadj1, sztmc1, msztmc1;
	  bool reverse1;
	  get_ref_base_and_extent (c1, &offadj1, &sztmc1, &msztmc1, &reverse1);
	  poly_int64 offadj2, sztmc2, msztmc2;
	  bool reverse2;
	  get_ref_base_and_extent (c2, &offadj2, &sztmc2, &msztmc2, &reverse2);
	  if (!known_eq (offadj1, offadj2))
	    return flags | ACCESS_PATH;
	}
      c1 = TREE_OPERAND (c1, 0);
      c2 = TREE_OPERAND (c2, 0);
    }
  /* Finally test the access type.  */
  if (!types_equal_for_same_type_for_tbaa_p (TREE_TYPE (c1),
					     TREE_TYPE (c2),
					     lto_streaming_safe))
    return flags | ACCESS_PATH;
  return flags;
}

/* Hash REF to HSTATE.  If LTO_STREAMING_SAFE do not use alias sets
   and canonical types.  */
void
ao_compare::hash_ao_ref (ao_ref *ref, bool lto_streaming_safe, bool tbaa,
			 inchash::hash &hstate)
{
  tree base = ao_ref_base (ref);
  tree tbase = base;

  if (!known_eq (ref->size, ref->max_size))
    {
      tree r = ref->ref;
      if (TREE_CODE (r) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (r, 1)))
	{
	  tree field = TREE_OPERAND (r, 1);
	  hash_operand (DECL_FIELD_OFFSET (field), hstate, 0);
	  hash_operand (DECL_FIELD_BIT_OFFSET (field), hstate, 0);
	  hash_operand (DECL_SIZE (field), hstate, 0);
	  r = TREE_OPERAND (r, 0);
	}
      if (TREE_CODE (r) == BIT_FIELD_REF)
	{
	  hash_operand (TREE_OPERAND (r, 1), hstate, 0);
	  hash_operand (TREE_OPERAND (r, 2), hstate, 0);
	  r = TREE_OPERAND (r, 0);
	}
      hash_operand (TYPE_SIZE (TREE_TYPE (ref->ref)), hstate, 0);
      hash_operand (r, hstate, OEP_ADDRESS_OF | OEP_MATCH_SIDE_EFFECTS);
    }
  else
    {
      hash_operand (tbase, hstate, OEP_ADDRESS_OF | OEP_MATCH_SIDE_EFFECTS);
      hstate.add_poly_int (ref->offset);
      hstate.add_poly_int (ref->size);
      hstate.add_poly_int (ref->max_size);
    }
  if (!lto_streaming_safe && tbaa)
    {
      hstate.add_int (ao_ref_alias_set (ref));
      hstate.add_int (ao_ref_base_alias_set (ref));
    }
}
