/* Alias analysis for GNU C
   Copyright (C) 1997-2013 Free Software Foundation, Inc.
   Contributed by John Carr (jfc@mit.edu).

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
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "varasm.h"
#include "expr.h"
#include "tm_p.h"
#include "function.h"
#include "alias.h"
#include "emit-rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "flags.h"
#include "diagnostic-core.h"
#include "cselib.h"
#include "splay-tree.h"
#include "ggc.h"
#include "langhooks.h"
#include "timevar.h"
#include "dumpfile.h"
#include "target.h"
#include "df.h"
#include "tree-ssa-alias.h"
#include "pointer-set.h"
#include "gimple.h"
#include "gimple-ssa.h"

/* The aliasing API provided here solves related but different problems:

   Say there exists (in c)

   struct X {
     struct Y y1;
     struct Z z2;
   } x1, *px1,  *px2;

   struct Y y2, *py;
   struct Z z2, *pz;


   py = &x1.y1;
   px2 = &x1;

   Consider the four questions:

   Can a store to x1 interfere with px2->y1?
   Can a store to x1 interfere with px2->z2?
   Can a store to x1 change the value pointed to by with py?
   Can a store to x1 change the value pointed to by with pz?

   The answer to these questions can be yes, yes, yes, and maybe.

   The first two questions can be answered with a simple examination
   of the type system.  If structure X contains a field of type Y then
   a store through a pointer to an X can overwrite any field that is
   contained (recursively) in an X (unless we know that px1 != px2).

   The last two questions can be solved in the same way as the first
   two questions but this is too conservative.  The observation is
   that in some cases we can know which (if any) fields are addressed
   and if those addresses are used in bad ways.  This analysis may be
   language specific.  In C, arbitrary operations may be applied to
   pointers.  However, there is some indication that this may be too
   conservative for some C++ types.

   The pass ipa-type-escape does this analysis for the types whose
   instances do not escape across the compilation boundary.

   Historically in GCC, these two problems were combined and a single
   data structure that was used to represent the solution to these
   problems.  We now have two similar but different data structures,
   The data structure to solve the last two questions is similar to
   the first, but does not contain the fields whose address are never
   taken.  For types that do escape the compilation unit, the data
   structures will have identical information.
*/

/* The alias sets assigned to MEMs assist the back-end in determining
   which MEMs can alias which other MEMs.  In general, two MEMs in
   different alias sets cannot alias each other, with one important
   exception.  Consider something like:

     struct S { int i; double d; };

   a store to an `S' can alias something of either type `int' or type
   `double'.  (However, a store to an `int' cannot alias a `double'
   and vice versa.)  We indicate this via a tree structure that looks
   like:
	   struct S
	    /   \
	   /     \
	 |/_     _\|
	 int    double

   (The arrows are directed and point downwards.)
    In this situation we say the alias set for `struct S' is the
   `superset' and that those for `int' and `double' are `subsets'.

   To see whether two alias sets can point to the same memory, we must
   see if either alias set is a subset of the other. We need not trace
   past immediate descendants, however, since we propagate all
   grandchildren up one level.

   Alias set zero is implicitly a superset of all other alias sets.
   However, this is no actual entry for alias set zero.  It is an
   error to attempt to explicitly construct a subset of zero.  */

struct GTY(()) alias_set_entry_d {
  /* The alias set number, as stored in MEM_ALIAS_SET.  */
  alias_set_type alias_set;

  /* Nonzero if would have a child of zero: this effectively makes this
     alias set the same as alias set zero.  */
  int has_zero_child;

  /* The children of the alias set.  These are not just the immediate
     children, but, in fact, all descendants.  So, if we have:

       struct T { struct S s; float f; }

     continuing our example above, the children here will be all of
     `int', `double', `float', and `struct S'.  */
  splay_tree GTY((param1_is (int), param2_is (int))) children;
};
typedef struct alias_set_entry_d *alias_set_entry;

static int rtx_equal_for_memref_p (const_rtx, const_rtx);
static int memrefs_conflict_p (int, rtx, int, rtx, HOST_WIDE_INT);
static void record_set (rtx, const_rtx, void *);
static int base_alias_check (rtx, rtx, rtx, rtx, enum machine_mode,
			     enum machine_mode);
static rtx find_base_value (rtx);
static int mems_in_disjoint_alias_sets_p (const_rtx, const_rtx);
static int insert_subset_children (splay_tree_node, void*);
static alias_set_entry get_alias_set_entry (alias_set_type);
static bool nonoverlapping_component_refs_p (const_rtx, const_rtx);
static tree decl_for_component_ref (tree);
static int write_dependence_p (const_rtx,
			       const_rtx, enum machine_mode, rtx,
			       bool, bool, bool);

static void memory_modified_1 (rtx, const_rtx, void *);

/* Set up all info needed to perform alias analysis on memory references.  */

/* Returns the size in bytes of the mode of X.  */
#define SIZE_FOR_MODE(X) (GET_MODE_SIZE (GET_MODE (X)))

/* Cap the number of passes we make over the insns propagating alias
   information through set chains.
   ??? 10 is a completely arbitrary choice.  This should be based on the
   maximum loop depth in the CFG, but we do not have this information
   available (even if current_loops _is_ available).  */
#define MAX_ALIAS_LOOP_PASSES 10

/* reg_base_value[N] gives an address to which register N is related.
   If all sets after the first add or subtract to the current value
   or otherwise modify it so it does not point to a different top level
   object, reg_base_value[N] is equal to the address part of the source
   of the first set.

   A base address can be an ADDRESS, SYMBOL_REF, or LABEL_REF.  ADDRESS
   expressions represent three types of base:

     1. incoming arguments.  There is just one ADDRESS to represent all
	arguments, since we do not know at this level whether accesses
	based on different arguments can alias.  The ADDRESS has id 0.

     2. stack_pointer_rtx, frame_pointer_rtx, hard_frame_pointer_rtx
	(if distinct from frame_pointer_rtx) and arg_pointer_rtx.
	Each of these rtxes has a separate ADDRESS associated with it,
	each with a negative id.

	GCC is (and is required to be) precise in which register it
	chooses to access a particular region of stack.  We can therefore
	assume that accesses based on one of these rtxes do not alias
	accesses based on another of these rtxes.

     3. bases that are derived from malloc()ed memory (REG_NOALIAS).
	Each such piece of memory has a separate ADDRESS associated
	with it, each with an id greater than 0.

   Accesses based on one ADDRESS do not alias accesses based on other
   ADDRESSes.  Accesses based on ADDRESSes in groups (2) and (3) do not
   alias globals either; the ADDRESSes have Pmode to indicate this.
   The ADDRESS in group (1) _may_ alias globals; it has VOIDmode to
   indicate this.  */

static GTY(()) vec<rtx, va_gc> *reg_base_value;
static rtx *new_reg_base_value;

/* The single VOIDmode ADDRESS that represents all argument bases.
   It has id 0.  */
static GTY(()) rtx arg_base_value;

/* Used to allocate unique ids to each REG_NOALIAS ADDRESS.  */
static int unique_id;

/* We preserve the copy of old array around to avoid amount of garbage
   produced.  About 8% of garbage produced were attributed to this
   array.  */
static GTY((deletable)) vec<rtx, va_gc> *old_reg_base_value;

/* Values of XINT (address, 0) of Pmode ADDRESS rtxes for special
   registers.  */
#define UNIQUE_BASE_VALUE_SP	-1
#define UNIQUE_BASE_VALUE_ARGP	-2
#define UNIQUE_BASE_VALUE_FP	-3
#define UNIQUE_BASE_VALUE_HFP	-4

#define static_reg_base_value \
  (this_target_rtl->x_static_reg_base_value)

#define REG_BASE_VALUE(X)					\
  (REGNO (X) < vec_safe_length (reg_base_value)			\
   ? (*reg_base_value)[REGNO (X)] : 0)

/* Vector indexed by N giving the initial (unchanging) value known for
   pseudo-register N.  This vector is initialized in init_alias_analysis,
   and does not change until end_alias_analysis is called.  */
static GTY(()) vec<rtx, va_gc> *reg_known_value;

/* Vector recording for each reg_known_value whether it is due to a
   REG_EQUIV note.  Future passes (viz., reload) may replace the
   pseudo with the equivalent expression and so we account for the
   dependences that would be introduced if that happens.

   The REG_EQUIV notes created in assign_parms may mention the arg
   pointer, and there are explicit insns in the RTL that modify the
   arg pointer.  Thus we must ensure that such insns don't get
   scheduled across each other because that would invalidate the
   REG_EQUIV notes.  One could argue that the REG_EQUIV notes are
   wrong, but solving the problem in the scheduler will likely give
   better code, so we do it here.  */
static sbitmap reg_known_equiv_p;

/* True when scanning insns from the start of the rtl to the
   NOTE_INSN_FUNCTION_BEG note.  */
static bool copying_arguments;


/* The splay-tree used to store the various alias set entries.  */
static GTY (()) vec<alias_set_entry, va_gc> *alias_sets;

/* Build a decomposed reference object for querying the alias-oracle
   from the MEM rtx and store it in *REF.
   Returns false if MEM is not suitable for the alias-oracle.  */

static bool
ao_ref_from_mem (ao_ref *ref, const_rtx mem)
{
  tree expr = MEM_EXPR (mem);
  tree base;

  if (!expr)
    return false;

  ao_ref_init (ref, expr);

  /* Get the base of the reference and see if we have to reject or
     adjust it.  */
  base = ao_ref_base (ref);
  if (base == NULL_TREE)
    return false;

  /* The tree oracle doesn't like bases that are neither decls
     nor indirect references of SSA names.  */
  if (!(DECL_P (base)
	|| (TREE_CODE (base) == MEM_REF
	    && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
	|| (TREE_CODE (base) == TARGET_MEM_REF
	    && TREE_CODE (TMR_BASE (base)) == SSA_NAME)))
    return false;

  /* If this is a reference based on a partitioned decl replace the
     base with a MEM_REF of the pointer representative we
     created during stack slot partitioning.  */
  if (TREE_CODE (base) == VAR_DECL
      && ! is_global_var (base)
      && cfun->gimple_df->decls_to_pointers != NULL)
    {
      void *namep;
      namep = pointer_map_contains (cfun->gimple_df->decls_to_pointers, base);
      if (namep)
	ref->base = build_simple_mem_ref (*(tree *)namep);
    }

  ref->ref_alias_set = MEM_ALIAS_SET (mem);

  /* If MEM_OFFSET or MEM_SIZE are unknown what we got from MEM_EXPR
     is conservative, so trust it.  */
  if (!MEM_OFFSET_KNOWN_P (mem)
      || !MEM_SIZE_KNOWN_P (mem))
    return true;

  /* If the base decl is a parameter we can have negative MEM_OFFSET in
     case of promoted subregs on bigendian targets.  Trust the MEM_EXPR
     here.  */
  if (MEM_OFFSET (mem) < 0
      && (MEM_SIZE (mem) + MEM_OFFSET (mem)) * BITS_PER_UNIT == ref->size)
    return true;

  /* Otherwise continue and refine size and offset we got from analyzing
     MEM_EXPR by using MEM_SIZE and MEM_OFFSET.  */

  ref->offset += MEM_OFFSET (mem) * BITS_PER_UNIT;
  ref->size = MEM_SIZE (mem) * BITS_PER_UNIT;

  /* The MEM may extend into adjacent fields, so adjust max_size if
     necessary.  */
  if (ref->max_size != -1
      && ref->size > ref->max_size)
    ref->max_size = ref->size;

  /* If MEM_OFFSET and MEM_SIZE get us outside of the base object of
     the MEM_EXPR punt.  This happens for STRICT_ALIGNMENT targets a lot.  */
  if (MEM_EXPR (mem) != get_spill_slot_decl (false)
      && (ref->offset < 0
	  || (DECL_P (ref->base)
	      && (!tree_fits_uhwi_p (DECL_SIZE (ref->base))
		  || (TREE_INT_CST_LOW (DECL_SIZE ((ref->base)))
		      < (unsigned HOST_WIDE_INT)(ref->offset + ref->size))))))
    return false;

  return true;
}

/* Query the alias-oracle on whether the two memory rtx X and MEM may
   alias.  If TBAA_P is set also apply TBAA.  Returns true if the
   two rtxen may alias, false otherwise.  */

static bool
rtx_refs_may_alias_p (const_rtx x, const_rtx mem, bool tbaa_p)
{
  ao_ref ref1, ref2;

  if (!ao_ref_from_mem (&ref1, x)
      || !ao_ref_from_mem (&ref2, mem))
    return true;

  return refs_may_alias_p_1 (&ref1, &ref2,
			     tbaa_p
			     && MEM_ALIAS_SET (x) != 0
			     && MEM_ALIAS_SET (mem) != 0);
}

/* Returns a pointer to the alias set entry for ALIAS_SET, if there is
   such an entry, or NULL otherwise.  */

static inline alias_set_entry
get_alias_set_entry (alias_set_type alias_set)
{
  return (*alias_sets)[alias_set];
}

/* Returns nonzero if the alias sets for MEM1 and MEM2 are such that
   the two MEMs cannot alias each other.  */

static inline int
mems_in_disjoint_alias_sets_p (const_rtx mem1, const_rtx mem2)
{
/* Perform a basic sanity check.  Namely, that there are no alias sets
   if we're not using strict aliasing.  This helps to catch bugs
   whereby someone uses PUT_CODE, but doesn't clear MEM_ALIAS_SET, or
   where a MEM is allocated in some way other than by the use of
   gen_rtx_MEM, and the MEM_ALIAS_SET is not cleared.  If we begin to
   use alias sets to indicate that spilled registers cannot alias each
   other, we might need to remove this check.  */
  gcc_assert (flag_strict_aliasing
	      || (!MEM_ALIAS_SET (mem1) && !MEM_ALIAS_SET (mem2)));

  return ! alias_sets_conflict_p (MEM_ALIAS_SET (mem1), MEM_ALIAS_SET (mem2));
}

/* Insert the NODE into the splay tree given by DATA.  Used by
   record_alias_subset via splay_tree_foreach.  */

static int
insert_subset_children (splay_tree_node node, void *data)
{
  splay_tree_insert ((splay_tree) data, node->key, node->value);

  return 0;
}

/* Return true if the first alias set is a subset of the second.  */

bool
alias_set_subset_of (alias_set_type set1, alias_set_type set2)
{
  alias_set_entry ase;

  /* Everything is a subset of the "aliases everything" set.  */
  if (set2 == 0)
    return true;

  /* Otherwise, check if set1 is a subset of set2.  */
  ase = get_alias_set_entry (set2);
  if (ase != 0
      && (ase->has_zero_child
	  || splay_tree_lookup (ase->children,
			        (splay_tree_key) set1)))
    return true;
  return false;
}

/* Return 1 if the two specified alias sets may conflict.  */

int
alias_sets_conflict_p (alias_set_type set1, alias_set_type set2)
{
  alias_set_entry ase;

  /* The easy case.  */
  if (alias_sets_must_conflict_p (set1, set2))
    return 1;

  /* See if the first alias set is a subset of the second.  */
  ase = get_alias_set_entry (set1);
  if (ase != 0
      && (ase->has_zero_child
	  || splay_tree_lookup (ase->children,
				(splay_tree_key) set2)))
    return 1;

  /* Now do the same, but with the alias sets reversed.  */
  ase = get_alias_set_entry (set2);
  if (ase != 0
      && (ase->has_zero_child
	  || splay_tree_lookup (ase->children,
				(splay_tree_key) set1)))
    return 1;

  /* The two alias sets are distinct and neither one is the
     child of the other.  Therefore, they cannot conflict.  */
  return 0;
}

/* Return 1 if the two specified alias sets will always conflict.  */

int
alias_sets_must_conflict_p (alias_set_type set1, alias_set_type set2)
{
  if (set1 == 0 || set2 == 0 || set1 == set2)
    return 1;

  return 0;
}

/* Return 1 if any MEM object of type T1 will always conflict (using the
   dependency routines in this file) with any MEM object of type T2.
   This is used when allocating temporary storage.  If T1 and/or T2 are
   NULL_TREE, it means we know nothing about the storage.  */

int
objects_must_conflict_p (tree t1, tree t2)
{
  alias_set_type set1, set2;

  /* If neither has a type specified, we don't know if they'll conflict
     because we may be using them to store objects of various types, for
     example the argument and local variables areas of inlined functions.  */
  if (t1 == 0 && t2 == 0)
    return 0;

  /* If they are the same type, they must conflict.  */
  if (t1 == t2
      /* Likewise if both are volatile.  */
      || (t1 != 0 && TYPE_VOLATILE (t1) && t2 != 0 && TYPE_VOLATILE (t2)))
    return 1;

  set1 = t1 ? get_alias_set (t1) : 0;
  set2 = t2 ? get_alias_set (t2) : 0;

  /* We can't use alias_sets_conflict_p because we must make sure
     that every subtype of t1 will conflict with every subtype of
     t2 for which a pair of subobjects of these respective subtypes
     overlaps on the stack.  */
  return alias_sets_must_conflict_p (set1, set2);
}

/* Return the outermost parent of component present in the chain of
   component references handled by get_inner_reference in T with the
   following property:
     - the component is non-addressable, or
     - the parent has alias set zero,
   or NULL_TREE if no such parent exists.  In the former cases, the alias
   set of this parent is the alias set that must be used for T itself.  */

tree
component_uses_parent_alias_set_from (const_tree t)
{
  const_tree found = NULL_TREE;

  while (handled_component_p (t))
    {
      switch (TREE_CODE (t))
	{
	case COMPONENT_REF:
	  if (DECL_NONADDRESSABLE_P (TREE_OPERAND (t, 1)))
	    found = t;
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  if (TYPE_NONALIASED_COMPONENT (TREE_TYPE (TREE_OPERAND (t, 0))))
	    found = t;
	  break;

	case REALPART_EXPR:
	case IMAGPART_EXPR:
	  break;

	case BIT_FIELD_REF:
	case VIEW_CONVERT_EXPR:
	  /* Bitfields and casts are never addressable.  */
	  found = t;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (get_alias_set (TREE_TYPE (TREE_OPERAND (t, 0))) == 0)
	found = t;

      t = TREE_OPERAND (t, 0);
    }
 
  if (found)
    return TREE_OPERAND (found, 0);

  return NULL_TREE;
}


/* Return whether the pointer-type T effective for aliasing may
   access everything and thus the reference has to be assigned
   alias-set zero.  */

static bool
ref_all_alias_ptr_type_p (const_tree t)
{
  return (TREE_CODE (TREE_TYPE (t)) == VOID_TYPE
	  || TYPE_REF_CAN_ALIAS_ALL (t));
}

/* Return the alias set for the memory pointed to by T, which may be
   either a type or an expression.  Return -1 if there is nothing
   special about dereferencing T.  */

static alias_set_type
get_deref_alias_set_1 (tree t)
{
  /* All we care about is the type.  */
  if (! TYPE_P (t))
    t = TREE_TYPE (t);

  /* If we have an INDIRECT_REF via a void pointer, we don't
     know anything about what that might alias.  Likewise if the
     pointer is marked that way.  */
  if (ref_all_alias_ptr_type_p (t))
    return 0;

  return -1;
}

/* Return the alias set for the memory pointed to by T, which may be
   either a type or an expression.  */

alias_set_type
get_deref_alias_set (tree t)
{
  /* If we're not doing any alias analysis, just assume everything
     aliases everything else.  */
  if (!flag_strict_aliasing)
    return 0;

  alias_set_type set = get_deref_alias_set_1 (t);

  /* Fall back to the alias-set of the pointed-to type.  */
  if (set == -1)
    {
      if (! TYPE_P (t))
	t = TREE_TYPE (t);
      set = get_alias_set (TREE_TYPE (t));
    }

  return set;
}

/* Return the pointer-type relevant for TBAA purposes from the
   memory reference tree *T or NULL_TREE in which case *T is
   adjusted to point to the outermost component reference that
   can be used for assigning an alias set.  */
 
static tree
reference_alias_ptr_type_1 (tree *t)
{
  tree inner;

  /* Get the base object of the reference.  */
  inner = *t;
  while (handled_component_p (inner))
    {
      /* If there is a VIEW_CONVERT_EXPR in the chain we cannot use
	 the type of any component references that wrap it to
	 determine the alias-set.  */
      if (TREE_CODE (inner) == VIEW_CONVERT_EXPR)
	*t = TREE_OPERAND (inner, 0);
      inner = TREE_OPERAND (inner, 0);
    }

  /* Handle pointer dereferences here, they can override the
     alias-set.  */
  if (INDIRECT_REF_P (inner)
      && ref_all_alias_ptr_type_p (TREE_TYPE (TREE_OPERAND (inner, 0))))
    return TREE_TYPE (TREE_OPERAND (inner, 0));
  else if (TREE_CODE (inner) == TARGET_MEM_REF)
    return TREE_TYPE (TMR_OFFSET (inner));
  else if (TREE_CODE (inner) == MEM_REF
	   && ref_all_alias_ptr_type_p (TREE_TYPE (TREE_OPERAND (inner, 1))))
    return TREE_TYPE (TREE_OPERAND (inner, 1));

  /* If the innermost reference is a MEM_REF that has a
     conversion embedded treat it like a VIEW_CONVERT_EXPR above,
     using the memory access type for determining the alias-set.  */
  if (TREE_CODE (inner) == MEM_REF
      && (TYPE_MAIN_VARIANT (TREE_TYPE (inner))
	  != TYPE_MAIN_VARIANT
	       (TREE_TYPE (TREE_TYPE (TREE_OPERAND (inner, 1))))))
    return TREE_TYPE (TREE_OPERAND (inner, 1));

  /* Otherwise, pick up the outermost object that we could have
     a pointer to.  */
  tree tem = component_uses_parent_alias_set_from (*t);
  if (tem)
    *t = tem;

  return NULL_TREE;
}

/* Return the pointer-type relevant for TBAA purposes from the
   gimple memory reference tree T.  This is the type to be used for
   the offset operand of MEM_REF or TARGET_MEM_REF replacements of T
   and guarantees that get_alias_set will return the same alias
   set for T and the replacement.  */

tree
reference_alias_ptr_type (tree t)
{
  tree ptype = reference_alias_ptr_type_1 (&t);
  /* If there is a given pointer type for aliasing purposes, return it.  */
  if (ptype != NULL_TREE)
    return ptype;

  /* Otherwise build one from the outermost component reference we
     may use.  */
  if (TREE_CODE (t) == MEM_REF
      || TREE_CODE (t) == TARGET_MEM_REF)
    return TREE_TYPE (TREE_OPERAND (t, 1));
  else
    return build_pointer_type (TYPE_MAIN_VARIANT (TREE_TYPE (t)));
}

/* Return whether the pointer-types T1 and T2 used to determine
   two alias sets of two references will yield the same answer
   from get_deref_alias_set.  */

bool
alias_ptr_types_compatible_p (tree t1, tree t2)
{
  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return true;

  if (ref_all_alias_ptr_type_p (t1)
      || ref_all_alias_ptr_type_p (t2))
    return false;

  return (TYPE_MAIN_VARIANT (TREE_TYPE (t1))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (t2)));
}

/* Return the alias set for T, which may be either a type or an
   expression.  Call language-specific routine for help, if needed.  */

alias_set_type
get_alias_set (tree t)
{
  alias_set_type set;

  /* If we're not doing any alias analysis, just assume everything
     aliases everything else.  Also return 0 if this or its type is
     an error.  */
  if (! flag_strict_aliasing || t == error_mark_node
      || (! TYPE_P (t)
	  && (TREE_TYPE (t) == 0 || TREE_TYPE (t) == error_mark_node)))
    return 0;

  /* We can be passed either an expression or a type.  This and the
     language-specific routine may make mutually-recursive calls to each other
     to figure out what to do.  At each juncture, we see if this is a tree
     that the language may need to handle specially.  First handle things that
     aren't types.  */
  if (! TYPE_P (t))
    {
      /* Give the language a chance to do something with this tree
	 before we look at it.  */
      STRIP_NOPS (t);
      set = lang_hooks.get_alias_set (t);
      if (set != -1)
	return set;

      /* Get the alias pointer-type to use or the outermost object
         that we could have a pointer to.  */
      tree ptype = reference_alias_ptr_type_1 (&t);
      if (ptype != NULL)
	return get_deref_alias_set (ptype);

      /* If we've already determined the alias set for a decl, just return
	 it.  This is necessary for C++ anonymous unions, whose component
	 variables don't look like union members (boo!).  */
      if (TREE_CODE (t) == VAR_DECL
	  && DECL_RTL_SET_P (t) && MEM_P (DECL_RTL (t)))
	return MEM_ALIAS_SET (DECL_RTL (t));

      /* Now all we care about is the type.  */
      t = TREE_TYPE (t);
    }

  /* Variant qualifiers don't affect the alias set, so get the main
     variant.  */
  t = TYPE_MAIN_VARIANT (t);

  /* Always use the canonical type as well.  If this is a type that
     requires structural comparisons to identify compatible types
     use alias set zero.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (t))
    {
      /* Allow the language to specify another alias set for this
	 type.  */
      set = lang_hooks.get_alias_set (t);
      if (set != -1)
	return set;
      return 0;
    }

  t = TYPE_CANONICAL (t);

  /* The canonical type should not require structural equality checks.  */
  gcc_checking_assert (!TYPE_STRUCTURAL_EQUALITY_P (t));

  /* If this is a type with a known alias set, return it.  */
  if (TYPE_ALIAS_SET_KNOWN_P (t))
    return TYPE_ALIAS_SET (t);

  /* We don't want to set TYPE_ALIAS_SET for incomplete types.  */
  if (!COMPLETE_TYPE_P (t))
    {
      /* For arrays with unknown size the conservative answer is the
	 alias set of the element type.  */
      if (TREE_CODE (t) == ARRAY_TYPE)
	return get_alias_set (TREE_TYPE (t));

      /* But return zero as a conservative answer for incomplete types.  */
      return 0;
    }

  /* See if the language has special handling for this type.  */
  set = lang_hooks.get_alias_set (t);
  if (set != -1)
    return set;

  /* There are no objects of FUNCTION_TYPE, so there's no point in
     using up an alias set for them.  (There are, of course, pointers
     and references to functions, but that's different.)  */
  else if (TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE)
    set = 0;

  /* Unless the language specifies otherwise, let vector types alias
     their components.  This avoids some nasty type punning issues in
     normal usage.  And indeed lets vectors be treated more like an
     array slice.  */
  else if (TREE_CODE (t) == VECTOR_TYPE)
    set = get_alias_set (TREE_TYPE (t));

  /* Unless the language specifies otherwise, treat array types the
     same as their components.  This avoids the asymmetry we get
     through recording the components.  Consider accessing a
     character(kind=1) through a reference to a character(kind=1)[1:1].
     Or consider if we want to assign integer(kind=4)[0:D.1387] and
     integer(kind=4)[4] the same alias set or not.
     Just be pragmatic here and make sure the array and its element
     type get the same alias set assigned.  */
  else if (TREE_CODE (t) == ARRAY_TYPE && !TYPE_NONALIASED_COMPONENT (t))
    set = get_alias_set (TREE_TYPE (t));

  /* From the former common C and C++ langhook implementation:

     Unfortunately, there is no canonical form of a pointer type.
     In particular, if we have `typedef int I', then `int *', and
     `I *' are different types.  So, we have to pick a canonical
     representative.  We do this below.

     Technically, this approach is actually more conservative that
     it needs to be.  In particular, `const int *' and `int *'
     should be in different alias sets, according to the C and C++
     standard, since their types are not the same, and so,
     technically, an `int **' and `const int **' cannot point at
     the same thing.

     But, the standard is wrong.  In particular, this code is
     legal C++:

     int *ip;
     int **ipp = &ip;
     const int* const* cipp = ipp;
     And, it doesn't make sense for that to be legal unless you
     can dereference IPP and CIPP.  So, we ignore cv-qualifiers on
     the pointed-to types.  This issue has been reported to the
     C++ committee.

     In addition to the above canonicalization issue, with LTO
     we should also canonicalize `T (*)[]' to `T *' avoiding
     alias issues with pointer-to element types and pointer-to
     array types.

     Likewise we need to deal with the situation of incomplete
     pointed-to types and make `*(struct X **)&a' and
     `*(struct X {} **)&a' alias.  Otherwise we will have to
     guarantee that all pointer-to incomplete type variants
     will be replaced by pointer-to complete type variants if
     they are available.

     With LTO the convenient situation of using `void *' to
     access and store any pointer type will also become
     more apparent (and `void *' is just another pointer-to
     incomplete type).  Assigning alias-set zero to `void *'
     and all pointer-to incomplete types is a not appealing
     solution.  Assigning an effective alias-set zero only
     affecting pointers might be - by recording proper subset
     relationships of all pointer alias-sets.

     Pointer-to function types are another grey area which
     needs caution.  Globbing them all into one alias-set
     or the above effective zero set would work.

     For now just assign the same alias-set to all pointers.
     That's simple and avoids all the above problems.  */
  else if (POINTER_TYPE_P (t)
	   && t != ptr_type_node)
    set = get_alias_set (ptr_type_node);

  /* Otherwise make a new alias set for this type.  */
  else
    {
      /* Each canonical type gets its own alias set, so canonical types
	 shouldn't form a tree.  It doesn't really matter for types
	 we handle specially above, so only check it where it possibly
	 would result in a bogus alias set.  */
      gcc_checking_assert (TYPE_CANONICAL (t) == t);

      set = new_alias_set ();
    }

  TYPE_ALIAS_SET (t) = set;

  /* If this is an aggregate type or a complex type, we must record any
     component aliasing information.  */
  if (AGGREGATE_TYPE_P (t) || TREE_CODE (t) == COMPLEX_TYPE)
    record_component_aliases (t);

  return set;
}

/* Return a brand-new alias set.  */

alias_set_type
new_alias_set (void)
{
  if (flag_strict_aliasing)
    {
      if (alias_sets == 0)
	vec_safe_push (alias_sets, (alias_set_entry) 0);
      vec_safe_push (alias_sets, (alias_set_entry) 0);
      return alias_sets->length () - 1;
    }
  else
    return 0;
}

/* Indicate that things in SUBSET can alias things in SUPERSET, but that
   not everything that aliases SUPERSET also aliases SUBSET.  For example,
   in C, a store to an `int' can alias a load of a structure containing an
   `int', and vice versa.  But it can't alias a load of a 'double' member
   of the same structure.  Here, the structure would be the SUPERSET and
   `int' the SUBSET.  This relationship is also described in the comment at
   the beginning of this file.

   This function should be called only once per SUPERSET/SUBSET pair.

   It is illegal for SUPERSET to be zero; everything is implicitly a
   subset of alias set zero.  */

void
record_alias_subset (alias_set_type superset, alias_set_type subset)
{
  alias_set_entry superset_entry;
  alias_set_entry subset_entry;

  /* It is possible in complex type situations for both sets to be the same,
     in which case we can ignore this operation.  */
  if (superset == subset)
    return;

  gcc_assert (superset);

  superset_entry = get_alias_set_entry (superset);
  if (superset_entry == 0)
    {
      /* Create an entry for the SUPERSET, so that we have a place to
	 attach the SUBSET.  */
      superset_entry = ggc_alloc_cleared_alias_set_entry_d ();
      superset_entry->alias_set = superset;
      superset_entry->children
	= splay_tree_new_ggc (splay_tree_compare_ints,
			      ggc_alloc_splay_tree_scalar_scalar_splay_tree_s,
			      ggc_alloc_splay_tree_scalar_scalar_splay_tree_node_s);
      superset_entry->has_zero_child = 0;
      (*alias_sets)[superset] = superset_entry;
    }

  if (subset == 0)
    superset_entry->has_zero_child = 1;
  else
    {
      subset_entry = get_alias_set_entry (subset);
      /* If there is an entry for the subset, enter all of its children
	 (if they are not already present) as children of the SUPERSET.  */
      if (subset_entry)
	{
	  if (subset_entry->has_zero_child)
	    superset_entry->has_zero_child = 1;

	  splay_tree_foreach (subset_entry->children, insert_subset_children,
			      superset_entry->children);
	}

      /* Enter the SUBSET itself as a child of the SUPERSET.  */
      splay_tree_insert (superset_entry->children,
			 (splay_tree_key) subset, 0);
    }
}

/* Record that component types of TYPE, if any, are part of that type for
   aliasing purposes.  For record types, we only record component types
   for fields that are not marked non-addressable.  For array types, we
   only record the component type if it is not marked non-aliased.  */

void
record_component_aliases (tree type)
{
  alias_set_type superset = get_alias_set (type);
  tree field;

  if (superset == 0)
    return;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* Recursively record aliases for the base classes, if there are any.  */
      if (TYPE_BINFO (type))
	{
	  int i;
	  tree binfo, base_binfo;

	  for (binfo = TYPE_BINFO (type), i = 0;
	       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	    record_alias_subset (superset,
				 get_alias_set (BINFO_TYPE (base_binfo)));
	}
      for (field = TYPE_FIELDS (type); field != 0; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL && !DECL_NONADDRESSABLE_P (field))
	  record_alias_subset (superset, get_alias_set (TREE_TYPE (field)));
      break;

    case COMPLEX_TYPE:
      record_alias_subset (superset, get_alias_set (TREE_TYPE (type)));
      break;

    /* VECTOR_TYPE and ARRAY_TYPE share the alias set with their
       element type.  */

    default:
      break;
    }
}

/* Allocate an alias set for use in storing and reading from the varargs
   spill area.  */

static GTY(()) alias_set_type varargs_set = -1;

alias_set_type
get_varargs_alias_set (void)
{
#if 1
  /* We now lower VA_ARG_EXPR, and there's currently no way to attach the
     varargs alias set to an INDIRECT_REF (FIXME!), so we can't
     consistently use the varargs alias set for loads from the varargs
     area.  So don't use it anywhere.  */
  return 0;
#else
  if (varargs_set == -1)
    varargs_set = new_alias_set ();

  return varargs_set;
#endif
}

/* Likewise, but used for the fixed portions of the frame, e.g., register
   save areas.  */

static GTY(()) alias_set_type frame_set = -1;

alias_set_type
get_frame_alias_set (void)
{
  if (frame_set == -1)
    frame_set = new_alias_set ();

  return frame_set;
}

/* Create a new, unique base with id ID.  */

static rtx
unique_base_value (HOST_WIDE_INT id)
{
  return gen_rtx_ADDRESS (Pmode, id);
}

/* Return true if accesses based on any other base value cannot alias
   those based on X.  */

static bool
unique_base_value_p (rtx x)
{
  return GET_CODE (x) == ADDRESS && GET_MODE (x) == Pmode;
}

/* Return true if X is known to be a base value.  */

static bool
known_base_value_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case LABEL_REF:
    case SYMBOL_REF:
      return true;

    case ADDRESS:
      /* Arguments may or may not be bases; we don't know for sure.  */
      return GET_MODE (x) != VOIDmode;

    default:
      return false;
    }
}

/* Inside SRC, the source of a SET, find a base address.  */

static rtx
find_base_value (rtx src)
{
  unsigned int regno;

#if defined (FIND_BASE_TERM)
  /* Try machine-dependent ways to find the base term.  */
  src = FIND_BASE_TERM (src);
#endif

  switch (GET_CODE (src))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return src;

    case REG:
      regno = REGNO (src);
      /* At the start of a function, argument registers have known base
	 values which may be lost later.  Returning an ADDRESS
	 expression here allows optimization based on argument values
	 even when the argument registers are used for other purposes.  */
      if (regno < FIRST_PSEUDO_REGISTER && copying_arguments)
	return new_reg_base_value[regno];

      /* If a pseudo has a known base value, return it.  Do not do this
	 for non-fixed hard regs since it can result in a circular
	 dependency chain for registers which have values at function entry.

	 The test above is not sufficient because the scheduler may move
	 a copy out of an arg reg past the NOTE_INSN_FUNCTION_BEGIN.  */
      if ((regno >= FIRST_PSEUDO_REGISTER || fixed_regs[regno])
	  && regno < vec_safe_length (reg_base_value))
	{
	  /* If we're inside init_alias_analysis, use new_reg_base_value
	     to reduce the number of relaxation iterations.  */
	  if (new_reg_base_value && new_reg_base_value[regno]
	      && DF_REG_DEF_COUNT (regno) == 1)
	    return new_reg_base_value[regno];

	  if ((*reg_base_value)[regno])
	    return (*reg_base_value)[regno];
	}

      return 0;

    case MEM:
      /* Check for an argument passed in memory.  Only record in the
	 copying-arguments block; it is too hard to track changes
	 otherwise.  */
      if (copying_arguments
	  && (XEXP (src, 0) == arg_pointer_rtx
	      || (GET_CODE (XEXP (src, 0)) == PLUS
		  && XEXP (XEXP (src, 0), 0) == arg_pointer_rtx)))
	return arg_base_value;
      return 0;

    case CONST:
      src = XEXP (src, 0);
      if (GET_CODE (src) != PLUS && GET_CODE (src) != MINUS)
	break;

      /* ... fall through ...  */

    case PLUS:
    case MINUS:
      {
	rtx temp, src_0 = XEXP (src, 0), src_1 = XEXP (src, 1);

	/* If either operand is a REG that is a known pointer, then it
	   is the base.  */
	if (REG_P (src_0) && REG_POINTER (src_0))
	  return find_base_value (src_0);
	if (REG_P (src_1) && REG_POINTER (src_1))
	  return find_base_value (src_1);

	/* If either operand is a REG, then see if we already have
	   a known value for it.  */
	if (REG_P (src_0))
	  {
	    temp = find_base_value (src_0);
	    if (temp != 0)
	      src_0 = temp;
	  }

	if (REG_P (src_1))
	  {
	    temp = find_base_value (src_1);
	    if (temp!= 0)
	      src_1 = temp;
	  }

	/* If either base is named object or a special address
	   (like an argument or stack reference), then use it for the
	   base term.  */
	if (src_0 != 0 && known_base_value_p (src_0))
	  return src_0;

	if (src_1 != 0 && known_base_value_p (src_1))
	  return src_1;

	/* Guess which operand is the base address:
	   If either operand is a symbol, then it is the base.  If
	   either operand is a CONST_INT, then the other is the base.  */
	if (CONST_INT_P (src_1) || CONSTANT_P (src_0))
	  return find_base_value (src_0);
	else if (CONST_INT_P (src_0) || CONSTANT_P (src_1))
	  return find_base_value (src_1);

	return 0;
      }

    case LO_SUM:
      /* The standard form is (lo_sum reg sym) so look only at the
	 second operand.  */
      return find_base_value (XEXP (src, 1));

    case AND:
      /* If the second operand is constant set the base
	 address to the first operand.  */
      if (CONST_INT_P (XEXP (src, 1)) && INTVAL (XEXP (src, 1)) != 0)
	return find_base_value (XEXP (src, 0));
      return 0;

    case TRUNCATE:
      /* As we do not know which address space the pointer is referring to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	break;
      if (GET_MODE_SIZE (GET_MODE (src)) < GET_MODE_SIZE (Pmode))
	break;
      /* Fall through.  */
    case HIGH:
    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return find_base_value (XEXP (src, 0));

    case ZERO_EXTEND:
    case SIGN_EXTEND:	/* used for NT/Alpha pointers */
      /* As we do not know which address space the pointer is referring to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	break;

      {
	rtx temp = find_base_value (XEXP (src, 0));

	if (temp != 0 && CONSTANT_P (temp))
	  temp = convert_memory_address (Pmode, temp);

	return temp;
      }

    default:
      break;
    }

  return 0;
}

/* Called from init_alias_analysis indirectly through note_stores,
   or directly if DEST is a register with a REG_NOALIAS note attached.
   SET is null in the latter case.  */

/* While scanning insns to find base values, reg_seen[N] is nonzero if
   register N has been set in this function.  */
static sbitmap reg_seen;

static void
record_set (rtx dest, const_rtx set, void *data ATTRIBUTE_UNUSED)
{
  unsigned regno;
  rtx src;
  int n;

  if (!REG_P (dest))
    return;

  regno = REGNO (dest);

  gcc_checking_assert (regno < reg_base_value->length ());

  /* If this spans multiple hard registers, then we must indicate that every
     register has an unusable value.  */
  if (regno < FIRST_PSEUDO_REGISTER)
    n = hard_regno_nregs[regno][GET_MODE (dest)];
  else
    n = 1;
  if (n != 1)
    {
      while (--n >= 0)
	{
	  bitmap_set_bit (reg_seen, regno + n);
	  new_reg_base_value[regno + n] = 0;
	}
      return;
    }

  if (set)
    {
      /* A CLOBBER wipes out any old value but does not prevent a previously
	 unset register from acquiring a base address (i.e. reg_seen is not
	 set).  */
      if (GET_CODE (set) == CLOBBER)
	{
	  new_reg_base_value[regno] = 0;
	  return;
	}
      src = SET_SRC (set);
    }
  else
    {
      /* There's a REG_NOALIAS note against DEST.  */
      if (bitmap_bit_p (reg_seen, regno))
	{
	  new_reg_base_value[regno] = 0;
	  return;
	}
      bitmap_set_bit (reg_seen, regno);
      new_reg_base_value[regno] = unique_base_value (unique_id++);
      return;
    }

  /* If this is not the first set of REGNO, see whether the new value
     is related to the old one.  There are two cases of interest:

	(1) The register might be assigned an entirely new value
	    that has the same base term as the original set.

	(2) The set might be a simple self-modification that
	    cannot change REGNO's base value.

     If neither case holds, reject the original base value as invalid.
     Note that the following situation is not detected:

	 extern int x, y;  int *p = &x; p += (&y-&x);

     ANSI C does not allow computing the difference of addresses
     of distinct top level objects.  */
  if (new_reg_base_value[regno] != 0
      && find_base_value (src) != new_reg_base_value[regno])
    switch (GET_CODE (src))
      {
      case LO_SUM:
      case MINUS:
	if (XEXP (src, 0) != dest && XEXP (src, 1) != dest)
	  new_reg_base_value[regno] = 0;
	break;
      case PLUS:
	/* If the value we add in the PLUS is also a valid base value,
	   this might be the actual base value, and the original value
	   an index.  */
	{
	  rtx other = NULL_RTX;

	  if (XEXP (src, 0) == dest)
	    other = XEXP (src, 1);
	  else if (XEXP (src, 1) == dest)
	    other = XEXP (src, 0);

	  if (! other || find_base_value (other))
	    new_reg_base_value[regno] = 0;
	  break;
	}
      case AND:
	if (XEXP (src, 0) != dest || !CONST_INT_P (XEXP (src, 1)))
	  new_reg_base_value[regno] = 0;
	break;
      default:
	new_reg_base_value[regno] = 0;
	break;
      }
  /* If this is the first set of a register, record the value.  */
  else if ((regno >= FIRST_PSEUDO_REGISTER || ! fixed_regs[regno])
	   && ! bitmap_bit_p (reg_seen, regno) && new_reg_base_value[regno] == 0)
    new_reg_base_value[regno] = find_base_value (src);

  bitmap_set_bit (reg_seen, regno);
}

/* Return REG_BASE_VALUE for REGNO.  Selective scheduler uses this to avoid
   using hard registers with non-null REG_BASE_VALUE for renaming.  */
rtx
get_reg_base_value (unsigned int regno)
{
  return (*reg_base_value)[regno];
}

/* If a value is known for REGNO, return it.  */

rtx
get_reg_known_value (unsigned int regno)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < vec_safe_length (reg_known_value))
	return (*reg_known_value)[regno];
    }
  return NULL;
}

/* Set it.  */

static void
set_reg_known_value (unsigned int regno, rtx val)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < vec_safe_length (reg_known_value))
	(*reg_known_value)[regno] = val;
    }
}

/* Similarly for reg_known_equiv_p.  */

bool
get_reg_known_equiv_p (unsigned int regno)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < vec_safe_length (reg_known_value))
	return bitmap_bit_p (reg_known_equiv_p, regno);
    }
  return false;
}

static void
set_reg_known_equiv_p (unsigned int regno, bool val)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      regno -= FIRST_PSEUDO_REGISTER;
      if (regno < vec_safe_length (reg_known_value))
	{
	  if (val)
	    bitmap_set_bit (reg_known_equiv_p, regno);
	  else
	    bitmap_clear_bit (reg_known_equiv_p, regno);
	}
    }
}


/* Returns a canonical version of X, from the point of view alias
   analysis.  (For example, if X is a MEM whose address is a register,
   and the register has a known value (say a SYMBOL_REF), then a MEM
   whose address is the SYMBOL_REF is returned.)  */

rtx
canon_rtx (rtx x)
{
  /* Recursively look for equivalences.  */
  if (REG_P (x) && REGNO (x) >= FIRST_PSEUDO_REGISTER)
    {
      rtx t = get_reg_known_value (REGNO (x));
      if (t == x)
	return x;
      if (t)
	return canon_rtx (t);
    }

  if (GET_CODE (x) == PLUS)
    {
      rtx x0 = canon_rtx (XEXP (x, 0));
      rtx x1 = canon_rtx (XEXP (x, 1));

      if (x0 != XEXP (x, 0) || x1 != XEXP (x, 1))
	{
	  if (CONST_INT_P (x0))
	    return plus_constant (GET_MODE (x), x1, INTVAL (x0));
	  else if (CONST_INT_P (x1))
	    return plus_constant (GET_MODE (x), x0, INTVAL (x1));
	  return gen_rtx_PLUS (GET_MODE (x), x0, x1);
	}
    }

  /* This gives us much better alias analysis when called from
     the loop optimizer.   Note we want to leave the original
     MEM alone, but need to return the canonicalized MEM with
     all the flags with their original values.  */
  else if (MEM_P (x))
    x = replace_equiv_address_nv (x, canon_rtx (XEXP (x, 0)));

  return x;
}

/* Return 1 if X and Y are identical-looking rtx's.
   Expect that X and Y has been already canonicalized.

   We use the data in reg_known_value above to see if two registers with
   different numbers are, in fact, equivalent.  */

static int
rtx_equal_for_memref_p (const_rtx x, const_rtx y)
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0 && y == 0)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  if (x == y)
    return 1;

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* Some RTL can be compared without a recursive examination.  */
  switch (code)
    {
    case REG:
      return REGNO (x) == REGNO (y);

    case LABEL_REF:
      return XEXP (x, 0) == XEXP (y, 0);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case ENTRY_VALUE:
      /* This is magic, don't go through canonicalization et al.  */
      return rtx_equal_p (ENTRY_VALUE_EXP (x), ENTRY_VALUE_EXP (y));

    case VALUE:
    CASE_CONST_UNIQUE:
      /* There's no need to compare the contents of CONST_DOUBLEs or
	 CONST_INTs because pointer equality is a good enough
	 comparison for these nodes.  */
      return 0;

    default:
      break;
    }

  /* canon_rtx knows how to handle plus.  No need to canonicalize.  */
  if (code == PLUS)
    return ((rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 0))
	     && rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 1)))
	    || (rtx_equal_for_memref_p (XEXP (x, 0), XEXP (y, 1))
		&& rtx_equal_for_memref_p (XEXP (x, 1), XEXP (y, 0))));
  /* For commutative operations, the RTX match if the operand match in any
     order.  Also handle the simple binary and unary cases without a loop.  */
  if (COMMUTATIVE_P (x))
    {
      rtx xop0 = canon_rtx (XEXP (x, 0));
      rtx yop0 = canon_rtx (XEXP (y, 0));
      rtx yop1 = canon_rtx (XEXP (y, 1));

      return ((rtx_equal_for_memref_p (xop0, yop0)
	       && rtx_equal_for_memref_p (canon_rtx (XEXP (x, 1)), yop1))
	      || (rtx_equal_for_memref_p (xop0, yop1)
		  && rtx_equal_for_memref_p (canon_rtx (XEXP (x, 1)), yop0)));
    }
  else if (NON_COMMUTATIVE_P (x))
    {
      return (rtx_equal_for_memref_p (canon_rtx (XEXP (x, 0)),
				      canon_rtx (XEXP (y, 0)))
	      && rtx_equal_for_memref_p (canon_rtx (XEXP (x, 1)),
					 canon_rtx (XEXP (y, 1))));
    }
  else if (UNARY_P (x))
    return rtx_equal_for_memref_p (canon_rtx (XEXP (x, 0)),
				   canon_rtx (XEXP (y, 0)));

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.

     Limit cases to types which actually appear in addresses.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_for_memref_p (canon_rtx (XVECEXP (x, i, j)),
					canon_rtx (XVECEXP (y, i, j))) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_memref_p (canon_rtx (XEXP (x, i)),
				      canon_rtx (XEXP (y, i))) == 0)
	    return 0;
	  break;

	  /* This can happen for asm operands.  */
	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	/* This can happen for an asm which clobbers memory.  */
	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  gcc_unreachable ();
	}
    }
  return 1;
}

static rtx
find_base_term (rtx x)
{
  cselib_val *val;
  struct elt_loc_list *l, *f;
  rtx ret;

#if defined (FIND_BASE_TERM)
  /* Try machine-dependent ways to find the base term.  */
  x = FIND_BASE_TERM (x);
#endif

  switch (GET_CODE (x))
    {
    case REG:
      return REG_BASE_VALUE (x);

    case TRUNCATE:
      /* As we do not know which address space the pointer is referring to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	return 0;
      if (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (Pmode))
	return 0;
      /* Fall through.  */
    case HIGH:
    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return find_base_term (XEXP (x, 0));

    case ZERO_EXTEND:
    case SIGN_EXTEND:	/* Used for Alpha/NT pointers */
      /* As we do not know which address space the pointer is referring to, we can
	 handle this only if the target does not support different pointer or
	 address modes depending on the address space.  */
      if (!target_default_pointer_address_modes_p ())
	return 0;

      {
	rtx temp = find_base_term (XEXP (x, 0));

	if (temp != 0 && CONSTANT_P (temp))
	  temp = convert_memory_address (Pmode, temp);

	return temp;
      }

    case VALUE:
      val = CSELIB_VAL_PTR (x);
      ret = NULL_RTX;

      if (!val)
	return ret;

      if (cselib_sp_based_value_p (val))
	return static_reg_base_value[STACK_POINTER_REGNUM];

      f = val->locs;
      /* Temporarily reset val->locs to avoid infinite recursion.  */
      val->locs = NULL;

      for (l = f; l; l = l->next)
	if (GET_CODE (l->loc) == VALUE
	    && CSELIB_VAL_PTR (l->loc)->locs
	    && !CSELIB_VAL_PTR (l->loc)->locs->next
	    && CSELIB_VAL_PTR (l->loc)->locs->loc == x)
	  continue;
	else if ((ret = find_base_term (l->loc)) != 0)
	  break;

      val->locs = f;
      return ret;

    case LO_SUM:
      /* The standard form is (lo_sum reg sym) so look only at the
         second operand.  */
      return find_base_term (XEXP (x, 1));

    case CONST:
      x = XEXP (x, 0);
      if (GET_CODE (x) != PLUS && GET_CODE (x) != MINUS)
	return 0;
      /* Fall through.  */
    case PLUS:
    case MINUS:
      {
	rtx tmp1 = XEXP (x, 0);
	rtx tmp2 = XEXP (x, 1);

	/* This is a little bit tricky since we have to determine which of
	   the two operands represents the real base address.  Otherwise this
	   routine may return the index register instead of the base register.

	   That may cause us to believe no aliasing was possible, when in
	   fact aliasing is possible.

	   We use a few simple tests to guess the base register.  Additional
	   tests can certainly be added.  For example, if one of the operands
	   is a shift or multiply, then it must be the index register and the
	   other operand is the base register.  */

	if (tmp1 == pic_offset_table_rtx && CONSTANT_P (tmp2))
	  return find_base_term (tmp2);

	/* If either operand is known to be a pointer, then prefer it
	   to determine the base term.  */
	if (REG_P (tmp1) && REG_POINTER (tmp1))
	  ;
	else if (REG_P (tmp2) && REG_POINTER (tmp2))
	  {
	    rtx tem = tmp1;
	    tmp1 = tmp2;
	    tmp2 = tem;
	  }

	/* Go ahead and find the base term for both operands.  If either base
	   term is from a pointer or is a named object or a special address
	   (like an argument or stack reference), then use it for the
	   base term.  */
	rtx base = find_base_term (tmp1);
	if (base != NULL_RTX
	    && ((REG_P (tmp1) && REG_POINTER (tmp1))
		 || known_base_value_p (base)))
	  return base;
	base = find_base_term (tmp2);
	if (base != NULL_RTX
	    && ((REG_P (tmp2) && REG_POINTER (tmp2))
		 || known_base_value_p (base)))
	  return base;

	/* We could not determine which of the two operands was the
	   base register and which was the index.  So we can determine
	   nothing from the base alias check.  */
	return 0;
      }

    case AND:
      if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) != 0)
	return find_base_term (XEXP (x, 0));
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return x;

    default:
      return 0;
    }
}

/* Return true if accesses to address X may alias accesses based
   on the stack pointer.  */

bool
may_be_sp_based_p (rtx x)
{
  rtx base = find_base_term (x);
  return !base || base == static_reg_base_value[STACK_POINTER_REGNUM];
}

/* Return 0 if the addresses X and Y are known to point to different
   objects, 1 if they might be pointers to the same object.  */

static int
base_alias_check (rtx x, rtx x_base, rtx y, rtx y_base,
		  enum machine_mode x_mode, enum machine_mode y_mode)
{
  /* If the address itself has no known base see if a known equivalent
     value has one.  If either address still has no known base, nothing
     is known about aliasing.  */
  if (x_base == 0)
    {
      rtx x_c;

      if (! flag_expensive_optimizations || (x_c = canon_rtx (x)) == x)
	return 1;

      x_base = find_base_term (x_c);
      if (x_base == 0)
	return 1;
    }

  if (y_base == 0)
    {
      rtx y_c;
      if (! flag_expensive_optimizations || (y_c = canon_rtx (y)) == y)
	return 1;

      y_base = find_base_term (y_c);
      if (y_base == 0)
	return 1;
    }

  /* If the base addresses are equal nothing is known about aliasing.  */
  if (rtx_equal_p (x_base, y_base))
    return 1;

  /* The base addresses are different expressions.  If they are not accessed
     via AND, there is no conflict.  We can bring knowledge of object
     alignment into play here.  For example, on alpha, "char a, b;" can
     alias one another, though "char a; long b;" cannot.  AND addesses may
     implicitly alias surrounding objects; i.e. unaligned access in DImode
     via AND address can alias all surrounding object types except those
     with aligment 8 or higher.  */
  if (GET_CODE (x) == AND && GET_CODE (y) == AND)
    return 1;
  if (GET_CODE (x) == AND
      && (!CONST_INT_P (XEXP (x, 1))
	  || (int) GET_MODE_UNIT_SIZE (y_mode) < -INTVAL (XEXP (x, 1))))
    return 1;
  if (GET_CODE (y) == AND
      && (!CONST_INT_P (XEXP (y, 1))
	  || (int) GET_MODE_UNIT_SIZE (x_mode) < -INTVAL (XEXP (y, 1))))
    return 1;

  /* Differing symbols not accessed via AND never alias.  */
  if (GET_CODE (x_base) != ADDRESS && GET_CODE (y_base) != ADDRESS)
    return 0;

  if (unique_base_value_p (x_base) || unique_base_value_p (y_base))
    return 0;

  return 1;
}

/* Callback for for_each_rtx, that returns 1 upon encountering a VALUE
   whose UID is greater than the int uid that D points to.  */

static int
refs_newer_value_cb (rtx *x, void *d)
{
  if (GET_CODE (*x) == VALUE && CSELIB_VAL_PTR (*x)->uid > *(int *)d)
    return 1;

  return 0;
}

/* Return TRUE if EXPR refers to a VALUE whose uid is greater than
   that of V.  */

static bool
refs_newer_value_p (rtx expr, rtx v)
{
  int minuid = CSELIB_VAL_PTR (v)->uid;

  return for_each_rtx (&expr, refs_newer_value_cb, &minuid);
}

/* Convert the address X into something we can use.  This is done by returning
   it unchanged unless it is a value; in the latter case we call cselib to get
   a more useful rtx.  */

rtx
get_addr (rtx x)
{
  cselib_val *v;
  struct elt_loc_list *l;

  if (GET_CODE (x) != VALUE)
    return x;
  v = CSELIB_VAL_PTR (x);
  if (v)
    {
      bool have_equivs = cselib_have_permanent_equivalences ();
      if (have_equivs)
	v = canonical_cselib_val (v);
      for (l = v->locs; l; l = l->next)
	if (CONSTANT_P (l->loc))
	  return l->loc;
      for (l = v->locs; l; l = l->next)
	if (!REG_P (l->loc) && !MEM_P (l->loc)
	    /* Avoid infinite recursion when potentially dealing with
	       var-tracking artificial equivalences, by skipping the
	       equivalences themselves, and not choosing expressions
	       that refer to newer VALUEs.  */
	    && (!have_equivs
		|| (GET_CODE (l->loc) != VALUE
		    && !refs_newer_value_p (l->loc, x))))
	  return l->loc;
      if (have_equivs)
	{
	  for (l = v->locs; l; l = l->next)
	    if (REG_P (l->loc)
		|| (GET_CODE (l->loc) != VALUE
		    && !refs_newer_value_p (l->loc, x)))
	      return l->loc;
	  /* Return the canonical value.  */
	  return v->val_rtx;
	}
      if (v->locs)
	return v->locs->loc;
    }
  return x;
}

/*  Return the address of the (N_REFS + 1)th memory reference to ADDR
    where SIZE is the size in bytes of the memory reference.  If ADDR
    is not modified by the memory reference then ADDR is returned.  */

static rtx
addr_side_effect_eval (rtx addr, int size, int n_refs)
{
  int offset = 0;

  switch (GET_CODE (addr))
    {
    case PRE_INC:
      offset = (n_refs + 1) * size;
      break;
    case PRE_DEC:
      offset = -(n_refs + 1) * size;
      break;
    case POST_INC:
      offset = n_refs * size;
      break;
    case POST_DEC:
      offset = -n_refs * size;
      break;

    default:
      return addr;
    }

  if (offset)
    addr = gen_rtx_PLUS (GET_MODE (addr), XEXP (addr, 0),
			 gen_int_mode (offset, GET_MODE (addr)));
  else
    addr = XEXP (addr, 0);
  addr = canon_rtx (addr);

  return addr;
}

/* Return TRUE if an object X sized at XSIZE bytes and another object
   Y sized at YSIZE bytes, starting C bytes after X, may overlap.  If
   any of the sizes is zero, assume an overlap, otherwise use the
   absolute value of the sizes as the actual sizes.  */

static inline bool
offset_overlap_p (HOST_WIDE_INT c, int xsize, int ysize)
{
  return (xsize == 0 || ysize == 0
	  || (c >= 0
	      ? (abs (xsize) > c)
	      : (abs (ysize) > -c)));
}

/* Return one if X and Y (memory addresses) reference the
   same location in memory or if the references overlap.
   Return zero if they do not overlap, else return
   minus one in which case they still might reference the same location.

   C is an offset accumulator.  When
   C is nonzero, we are testing aliases between X and Y + C.
   XSIZE is the size in bytes of the X reference,
   similarly YSIZE is the size in bytes for Y.
   Expect that canon_rtx has been already called for X and Y.

   If XSIZE or YSIZE is zero, we do not know the amount of memory being
   referenced (the reference was BLKmode), so make the most pessimistic
   assumptions.

   If XSIZE or YSIZE is negative, we may access memory outside the object
   being referenced as a side effect.  This can happen when using AND to
   align memory references, as is done on the Alpha.

   Nice to notice that varying addresses cannot conflict with fp if no
   local variables had their addresses taken, but that's too hard now.

   ???  Contrary to the tree alias oracle this does not return
   one for X + non-constant and Y + non-constant when X and Y are equal.
   If that is fixed the TBAA hack for union type-punning can be removed.  */

static int
memrefs_conflict_p (int xsize, rtx x, int ysize, rtx y, HOST_WIDE_INT c)
{
  if (GET_CODE (x) == VALUE)
    {
      if (REG_P (y))
	{
	  struct elt_loc_list *l = NULL;
	  if (CSELIB_VAL_PTR (x))
	    for (l = canonical_cselib_val (CSELIB_VAL_PTR (x))->locs;
		 l; l = l->next)
	      if (REG_P (l->loc) && rtx_equal_for_memref_p (l->loc, y))
		break;
	  if (l)
	    x = y;
	  else
	    x = get_addr (x);
	}
      /* Don't call get_addr if y is the same VALUE.  */
      else if (x != y)
	x = get_addr (x);
    }
  if (GET_CODE (y) == VALUE)
    {
      if (REG_P (x))
	{
	  struct elt_loc_list *l = NULL;
	  if (CSELIB_VAL_PTR (y))
	    for (l = canonical_cselib_val (CSELIB_VAL_PTR (y))->locs;
		 l; l = l->next)
	      if (REG_P (l->loc) && rtx_equal_for_memref_p (l->loc, x))
		break;
	  if (l)
	    y = x;
	  else
	    y = get_addr (y);
	}
      /* Don't call get_addr if x is the same VALUE.  */
      else if (y != x)
	y = get_addr (y);
    }
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);
  else if (GET_CODE (x) == LO_SUM)
    x = XEXP (x, 1);
  else
    x = addr_side_effect_eval (x, abs (xsize), 0);
  if (GET_CODE (y) == HIGH)
    y = XEXP (y, 0);
  else if (GET_CODE (y) == LO_SUM)
    y = XEXP (y, 1);
  else
    y = addr_side_effect_eval (y, abs (ysize), 0);

  if (rtx_equal_for_memref_p (x, y))
    {
      return offset_overlap_p (c, xsize, ysize);
    }

  /* This code used to check for conflicts involving stack references and
     globals but the base address alias code now handles these cases.  */

  if (GET_CODE (x) == PLUS)
    {
      /* The fact that X is canonicalized means that this
	 PLUS rtx is canonicalized.  */
      rtx x0 = XEXP (x, 0);
      rtx x1 = XEXP (x, 1);

      if (GET_CODE (y) == PLUS)
	{
	  /* The fact that Y is canonicalized means that this
	     PLUS rtx is canonicalized.  */
	  rtx y0 = XEXP (y, 0);
	  rtx y1 = XEXP (y, 1);

	  if (rtx_equal_for_memref_p (x1, y1))
	    return memrefs_conflict_p (xsize, x0, ysize, y0, c);
	  if (rtx_equal_for_memref_p (x0, y0))
	    return memrefs_conflict_p (xsize, x1, ysize, y1, c);
	  if (CONST_INT_P (x1))
	    {
	      if (CONST_INT_P (y1))
		return memrefs_conflict_p (xsize, x0, ysize, y0,
					   c - INTVAL (x1) + INTVAL (y1));
	      else
		return memrefs_conflict_p (xsize, x0, ysize, y,
					   c - INTVAL (x1));
	    }
	  else if (CONST_INT_P (y1))
	    return memrefs_conflict_p (xsize, x, ysize, y0, c + INTVAL (y1));

	  return -1;
	}
      else if (CONST_INT_P (x1))
	return memrefs_conflict_p (xsize, x0, ysize, y, c - INTVAL (x1));
    }
  else if (GET_CODE (y) == PLUS)
    {
      /* The fact that Y is canonicalized means that this
	 PLUS rtx is canonicalized.  */
      rtx y0 = XEXP (y, 0);
      rtx y1 = XEXP (y, 1);

      if (CONST_INT_P (y1))
	return memrefs_conflict_p (xsize, x, ysize, y0, c + INTVAL (y1));
      else
	return -1;
    }

  if (GET_CODE (x) == GET_CODE (y))
    switch (GET_CODE (x))
      {
      case MULT:
	{
	  /* Handle cases where we expect the second operands to be the
	     same, and check only whether the first operand would conflict
	     or not.  */
	  rtx x0, y0;
	  rtx x1 = canon_rtx (XEXP (x, 1));
	  rtx y1 = canon_rtx (XEXP (y, 1));
	  if (! rtx_equal_for_memref_p (x1, y1))
	    return -1;
	  x0 = canon_rtx (XEXP (x, 0));
	  y0 = canon_rtx (XEXP (y, 0));
	  if (rtx_equal_for_memref_p (x0, y0))
	    return offset_overlap_p (c, xsize, ysize);

	  /* Can't properly adjust our sizes.  */
	  if (!CONST_INT_P (x1))
	    return -1;
	  xsize /= INTVAL (x1);
	  ysize /= INTVAL (x1);
	  c /= INTVAL (x1);
	  return memrefs_conflict_p (xsize, x0, ysize, y0, c);
	}

      default:
	break;
      }

  /* Deal with alignment ANDs by adjusting offset and size so as to
     cover the maximum range, without taking any previously known
     alignment into account.  Make a size negative after such an
     adjustments, so that, if we end up with e.g. two SYMBOL_REFs, we
     assume a potential overlap, because they may end up in contiguous
     memory locations and the stricter-alignment access may span over
     part of both.  */
  if (GET_CODE (x) == AND && CONST_INT_P (XEXP (x, 1)))
    {
      HOST_WIDE_INT sc = INTVAL (XEXP (x, 1));
      unsigned HOST_WIDE_INT uc = sc;
      if (sc < 0 && -uc == (uc & -uc))
	{
	  if (xsize > 0)
	    xsize = -xsize;
	  if (xsize)
	    xsize += sc + 1;
	  c -= sc + 1;
	  return memrefs_conflict_p (xsize, canon_rtx (XEXP (x, 0)),
				     ysize, y, c);
	}
    }
  if (GET_CODE (y) == AND && CONST_INT_P (XEXP (y, 1)))
    {
      HOST_WIDE_INT sc = INTVAL (XEXP (y, 1));
      unsigned HOST_WIDE_INT uc = sc;
      if (sc < 0 && -uc == (uc & -uc))
	{
	  if (ysize > 0)
	    ysize = -ysize;
	  if (ysize)
	    ysize += sc + 1;
	  c += sc + 1;
	  return memrefs_conflict_p (xsize, x,
				     ysize, canon_rtx (XEXP (y, 0)), c);
	}
    }

  if (CONSTANT_P (x))
    {
      if (CONST_INT_P (x) && CONST_INT_P (y))
	{
	  c += (INTVAL (y) - INTVAL (x));
	  return offset_overlap_p (c, xsize, ysize);
	}

      if (GET_CODE (x) == CONST)
	{
	  if (GET_CODE (y) == CONST)
	    return memrefs_conflict_p (xsize, canon_rtx (XEXP (x, 0)),
				       ysize, canon_rtx (XEXP (y, 0)), c);
	  else
	    return memrefs_conflict_p (xsize, canon_rtx (XEXP (x, 0)),
				       ysize, y, c);
	}
      if (GET_CODE (y) == CONST)
	return memrefs_conflict_p (xsize, x, ysize,
				   canon_rtx (XEXP (y, 0)), c);

      /* Assume a potential overlap for symbolic addresses that went
	 through alignment adjustments (i.e., that have negative
	 sizes), because we can't know how far they are from each
	 other.  */
      if (CONSTANT_P (y))
	return (xsize < 0 || ysize < 0 || offset_overlap_p (c, xsize, ysize));

      return -1;
    }

  return -1;
}

/* Functions to compute memory dependencies.

   Since we process the insns in execution order, we can build tables
   to keep track of what registers are fixed (and not aliased), what registers
   are varying in known ways, and what registers are varying in unknown
   ways.

   If both memory references are volatile, then there must always be a
   dependence between the two references, since their order can not be
   changed.  A volatile and non-volatile reference can be interchanged
   though.

   We also must allow AND addresses, because they may generate accesses
   outside the object being referenced.  This is used to generate aligned
   addresses from unaligned addresses, for instance, the alpha
   storeqi_unaligned pattern.  */

/* Read dependence: X is read after read in MEM takes place.  There can
   only be a dependence here if both reads are volatile, or if either is
   an explicit barrier.  */

int
read_dependence (const_rtx mem, const_rtx x)
{
  if (MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem))
    return true;
  if (MEM_ALIAS_SET (x) == ALIAS_SET_MEMORY_BARRIER
      || MEM_ALIAS_SET (mem) == ALIAS_SET_MEMORY_BARRIER)
    return true;
  return false;
}

/* Return true if we can determine that the fields referenced cannot
   overlap for any pair of objects.  */

static bool
nonoverlapping_component_refs_p (const_rtx rtlx, const_rtx rtly)
{
  const_tree x = MEM_EXPR (rtlx), y = MEM_EXPR (rtly);
  const_tree fieldx, fieldy, typex, typey, orig_y;

  if (!flag_strict_aliasing
      || !x || !y
      || TREE_CODE (x) != COMPONENT_REF
      || TREE_CODE (y) != COMPONENT_REF)
    return false;

  do
    {
      /* The comparison has to be done at a common type, since we don't
	 know how the inheritance hierarchy works.  */
      orig_y = y;
      do
	{
	  fieldx = TREE_OPERAND (x, 1);
	  typex = TYPE_MAIN_VARIANT (DECL_FIELD_CONTEXT (fieldx));

	  y = orig_y;
	  do
	    {
	      fieldy = TREE_OPERAND (y, 1);
	      typey = TYPE_MAIN_VARIANT (DECL_FIELD_CONTEXT (fieldy));

	      if (typex == typey)
		goto found;

	      y = TREE_OPERAND (y, 0);
	    }
	  while (y && TREE_CODE (y) == COMPONENT_REF);

	  x = TREE_OPERAND (x, 0);
	}
      while (x && TREE_CODE (x) == COMPONENT_REF);
      /* Never found a common type.  */
      return false;

    found:
      /* If we're left with accessing different fields of a structure, then no
	 possible overlap, unless they are both bitfields.  */
      if (TREE_CODE (typex) == RECORD_TYPE && fieldx != fieldy)
	return !(DECL_BIT_FIELD (fieldx) && DECL_BIT_FIELD (fieldy));

      /* The comparison on the current field failed.  If we're accessing
	 a very nested structure, look at the next outer level.  */
      x = TREE_OPERAND (x, 0);
      y = TREE_OPERAND (y, 0);
    }
  while (x && y
	 && TREE_CODE (x) == COMPONENT_REF
	 && TREE_CODE (y) == COMPONENT_REF);

  return false;
}

/* Look at the bottom of the COMPONENT_REF list for a DECL, and return it.  */

static tree
decl_for_component_ref (tree x)
{
  do
    {
      x = TREE_OPERAND (x, 0);
    }
  while (x && TREE_CODE (x) == COMPONENT_REF);

  return x && DECL_P (x) ? x : NULL_TREE;
}

/* Walk up the COMPONENT_REF list in X and adjust *OFFSET to compensate
   for the offset of the field reference.  *KNOWN_P says whether the
   offset is known.  */

static void
adjust_offset_for_component_ref (tree x, bool *known_p,
				 HOST_WIDE_INT *offset)
{
  if (!*known_p)
    return;
  do
    {
      tree xoffset = component_ref_field_offset (x);
      tree field = TREE_OPERAND (x, 1);

      if (! tree_fits_uhwi_p (xoffset))
	{
	  *known_p = false;
	  return;
	}
      *offset += (tree_to_uhwi (xoffset)
		  + (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field))
		     / BITS_PER_UNIT));

      x = TREE_OPERAND (x, 0);
    }
  while (x && TREE_CODE (x) == COMPONENT_REF);
}

/* Return nonzero if we can determine the exprs corresponding to memrefs
   X and Y and they do not overlap. 
   If LOOP_VARIANT is set, skip offset-based disambiguation */

int
nonoverlapping_memrefs_p (const_rtx x, const_rtx y, bool loop_invariant)
{
  tree exprx = MEM_EXPR (x), expry = MEM_EXPR (y);
  rtx rtlx, rtly;
  rtx basex, basey;
  bool moffsetx_known_p, moffsety_known_p;
  HOST_WIDE_INT moffsetx = 0, moffsety = 0;
  HOST_WIDE_INT offsetx = 0, offsety = 0, sizex, sizey, tem;

  /* Unless both have exprs, we can't tell anything.  */
  if (exprx == 0 || expry == 0)
    return 0;

  /* For spill-slot accesses make sure we have valid offsets.  */
  if ((exprx == get_spill_slot_decl (false)
       && ! MEM_OFFSET_KNOWN_P (x))
      || (expry == get_spill_slot_decl (false)
	  && ! MEM_OFFSET_KNOWN_P (y)))
    return 0;

  /* If the field reference test failed, look at the DECLs involved.  */
  moffsetx_known_p = MEM_OFFSET_KNOWN_P (x);
  if (moffsetx_known_p)
    moffsetx = MEM_OFFSET (x);
  if (TREE_CODE (exprx) == COMPONENT_REF)
    {
      tree t = decl_for_component_ref (exprx);
      if (! t)
	return 0;
      adjust_offset_for_component_ref (exprx, &moffsetx_known_p, &moffsetx);
      exprx = t;
    }

  moffsety_known_p = MEM_OFFSET_KNOWN_P (y);
  if (moffsety_known_p)
    moffsety = MEM_OFFSET (y);
  if (TREE_CODE (expry) == COMPONENT_REF)
    {
      tree t = decl_for_component_ref (expry);
      if (! t)
	return 0;
      adjust_offset_for_component_ref (expry, &moffsety_known_p, &moffsety);
      expry = t;
    }

  if (! DECL_P (exprx) || ! DECL_P (expry))
    return 0;

  /* With invalid code we can end up storing into the constant pool.
     Bail out to avoid ICEing when creating RTL for this.
     See gfortran.dg/lto/20091028-2_0.f90.  */
  if (TREE_CODE (exprx) == CONST_DECL
      || TREE_CODE (expry) == CONST_DECL)
    return 1;

  rtlx = DECL_RTL (exprx);
  rtly = DECL_RTL (expry);

  /* If either RTL is not a MEM, it must be a REG or CONCAT, meaning they
     can't overlap unless they are the same because we never reuse that part
     of the stack frame used for locals for spilled pseudos.  */
  if ((!MEM_P (rtlx) || !MEM_P (rtly))
      && ! rtx_equal_p (rtlx, rtly))
    return 1;

  /* If we have MEMs referring to different address spaces (which can
     potentially overlap), we cannot easily tell from the addresses
     whether the references overlap.  */
  if (MEM_P (rtlx) && MEM_P (rtly)
      && MEM_ADDR_SPACE (rtlx) != MEM_ADDR_SPACE (rtly))
    return 0;

  /* Get the base and offsets of both decls.  If either is a register, we
     know both are and are the same, so use that as the base.  The only
     we can avoid overlap is if we can deduce that they are nonoverlapping
     pieces of that decl, which is very rare.  */
  basex = MEM_P (rtlx) ? XEXP (rtlx, 0) : rtlx;
  if (GET_CODE (basex) == PLUS && CONST_INT_P (XEXP (basex, 1)))
    offsetx = INTVAL (XEXP (basex, 1)), basex = XEXP (basex, 0);

  basey = MEM_P (rtly) ? XEXP (rtly, 0) : rtly;
  if (GET_CODE (basey) == PLUS && CONST_INT_P (XEXP (basey, 1)))
    offsety = INTVAL (XEXP (basey, 1)), basey = XEXP (basey, 0);

  /* If the bases are different, we know they do not overlap if both
     are constants or if one is a constant and the other a pointer into the
     stack frame.  Otherwise a different base means we can't tell if they
     overlap or not.  */
  if (! rtx_equal_p (basex, basey))
    return ((CONSTANT_P (basex) && CONSTANT_P (basey))
	    || (CONSTANT_P (basex) && REG_P (basey)
		&& REGNO_PTR_FRAME_P (REGNO (basey)))
	    || (CONSTANT_P (basey) && REG_P (basex)
		&& REGNO_PTR_FRAME_P (REGNO (basex))));

  /* Offset based disambiguation not appropriate for loop invariant */
  if (loop_invariant)
    return 0;              

  sizex = (!MEM_P (rtlx) ? (int) GET_MODE_SIZE (GET_MODE (rtlx))
	   : MEM_SIZE_KNOWN_P (rtlx) ? MEM_SIZE (rtlx)
	   : -1);
  sizey = (!MEM_P (rtly) ? (int) GET_MODE_SIZE (GET_MODE (rtly))
	   : MEM_SIZE_KNOWN_P (rtly) ? MEM_SIZE (rtly)
	   : -1);

  /* If we have an offset for either memref, it can update the values computed
     above.  */
  if (moffsetx_known_p)
    offsetx += moffsetx, sizex -= moffsetx;
  if (moffsety_known_p)
    offsety += moffsety, sizey -= moffsety;

  /* If a memref has both a size and an offset, we can use the smaller size.
     We can't do this if the offset isn't known because we must view this
     memref as being anywhere inside the DECL's MEM.  */
  if (MEM_SIZE_KNOWN_P (x) && moffsetx_known_p)
    sizex = MEM_SIZE (x);
  if (MEM_SIZE_KNOWN_P (y) && moffsety_known_p)
    sizey = MEM_SIZE (y);

  /* Put the values of the memref with the lower offset in X's values.  */
  if (offsetx > offsety)
    {
      tem = offsetx, offsetx = offsety, offsety = tem;
      tem = sizex, sizex = sizey, sizey = tem;
    }

  /* If we don't know the size of the lower-offset value, we can't tell
     if they conflict.  Otherwise, we do the test.  */
  return sizex >= 0 && offsety >= offsetx + sizex;
}

/* Helper for true_dependence and canon_true_dependence.
   Checks for true dependence: X is read after store in MEM takes place.

   If MEM_CANONICALIZED is FALSE, then X_ADDR and MEM_ADDR should be
   NULL_RTX, and the canonical addresses of MEM and X are both computed
   here.  If MEM_CANONICALIZED, then MEM must be already canonicalized.

   If X_ADDR is non-NULL, it is used in preference of XEXP (x, 0).

   Returns 1 if there is a true dependence, 0 otherwise.  */

static int
true_dependence_1 (const_rtx mem, enum machine_mode mem_mode, rtx mem_addr,
		   const_rtx x, rtx x_addr, bool mem_canonicalized)
{
  rtx base;
  int ret;

  gcc_checking_assert (mem_canonicalized ? (mem_addr != NULL_RTX)
		       : (mem_addr == NULL_RTX && x_addr == NULL_RTX));

  if (MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem))
    return 1;

  /* (mem:BLK (scratch)) is a special mechanism to conflict with everything.
     This is used in epilogue deallocation functions, and in cselib.  */
  if (GET_MODE (x) == BLKmode && GET_CODE (XEXP (x, 0)) == SCRATCH)
    return 1;
  if (GET_MODE (mem) == BLKmode && GET_CODE (XEXP (mem, 0)) == SCRATCH)
    return 1;
  if (MEM_ALIAS_SET (x) == ALIAS_SET_MEMORY_BARRIER
      || MEM_ALIAS_SET (mem) == ALIAS_SET_MEMORY_BARRIER)
    return 1;

  /* Read-only memory is by definition never modified, and therefore can't
     conflict with anything.  We don't expect to find read-only set on MEM,
     but stupid user tricks can produce them, so don't die.  */
  if (MEM_READONLY_P (x))
    return 0;

  /* If we have MEMs referring to different address spaces (which can
     potentially overlap), we cannot easily tell from the addresses
     whether the references overlap.  */
  if (MEM_ADDR_SPACE (mem) != MEM_ADDR_SPACE (x))
    return 1;

  if (! mem_addr)
    {
      mem_addr = XEXP (mem, 0);
      if (mem_mode == VOIDmode)
	mem_mode = GET_MODE (mem);
    }

  if (! x_addr)
    {
      x_addr = XEXP (x, 0);
      if (!((GET_CODE (x_addr) == VALUE
	     && GET_CODE (mem_addr) != VALUE
	     && reg_mentioned_p (x_addr, mem_addr))
	    || (GET_CODE (x_addr) != VALUE
		&& GET_CODE (mem_addr) == VALUE
		&& reg_mentioned_p (mem_addr, x_addr))))
	{
	  x_addr = get_addr (x_addr);
	  if (! mem_canonicalized)
	    mem_addr = get_addr (mem_addr);
	}
    }

  base = find_base_term (x_addr);
  if (base && (GET_CODE (base) == LABEL_REF
	       || (GET_CODE (base) == SYMBOL_REF
		   && CONSTANT_POOL_ADDRESS_P (base))))
    return 0;

  rtx mem_base = find_base_term (mem_addr);
  if (! base_alias_check (x_addr, base, mem_addr, mem_base,
			  GET_MODE (x), mem_mode))
    return 0;

  x_addr = canon_rtx (x_addr);
  if (!mem_canonicalized)
    mem_addr = canon_rtx (mem_addr);

  if ((ret = memrefs_conflict_p (GET_MODE_SIZE (mem_mode), mem_addr,
				 SIZE_FOR_MODE (x), x_addr, 0)) != -1)
    return ret;

  if (mems_in_disjoint_alias_sets_p (x, mem))
    return 0;

  if (nonoverlapping_memrefs_p (mem, x, false))
    return 0;

  if (nonoverlapping_component_refs_p (mem, x))
    return 0;

  return rtx_refs_may_alias_p (x, mem, true);
}

/* True dependence: X is read after store in MEM takes place.  */

int
true_dependence (const_rtx mem, enum machine_mode mem_mode, const_rtx x)
{
  return true_dependence_1 (mem, mem_mode, NULL_RTX,
			    x, NULL_RTX, /*mem_canonicalized=*/false);
}

/* Canonical true dependence: X is read after store in MEM takes place.
   Variant of true_dependence which assumes MEM has already been
   canonicalized (hence we no longer do that here).
   The mem_addr argument has been added, since true_dependence_1 computed
   this value prior to canonicalizing.  */

int
canon_true_dependence (const_rtx mem, enum machine_mode mem_mode, rtx mem_addr,
		       const_rtx x, rtx x_addr)
{
  return true_dependence_1 (mem, mem_mode, mem_addr,
			    x, x_addr, /*mem_canonicalized=*/true);
}

/* Returns nonzero if a write to X might alias a previous read from
   (or, if WRITEP is true, a write to) MEM.
   If X_CANONCALIZED is true, then X_ADDR is the canonicalized address of X,
   and X_MODE the mode for that access.
   If MEM_CANONICALIZED is true, MEM is canonicalized.  */

static int
write_dependence_p (const_rtx mem,
		    const_rtx x, enum machine_mode x_mode, rtx x_addr,
		    bool mem_canonicalized, bool x_canonicalized, bool writep)
{
  rtx mem_addr;
  rtx base;
  int ret;

  gcc_checking_assert (x_canonicalized
		       ? (x_addr != NULL_RTX && x_mode != VOIDmode)
		       : (x_addr == NULL_RTX && x_mode == VOIDmode));

  if (MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem))
    return 1;

  /* (mem:BLK (scratch)) is a special mechanism to conflict with everything.
     This is used in epilogue deallocation functions.  */
  if (GET_MODE (x) == BLKmode && GET_CODE (XEXP (x, 0)) == SCRATCH)
    return 1;
  if (GET_MODE (mem) == BLKmode && GET_CODE (XEXP (mem, 0)) == SCRATCH)
    return 1;
  if (MEM_ALIAS_SET (x) == ALIAS_SET_MEMORY_BARRIER
      || MEM_ALIAS_SET (mem) == ALIAS_SET_MEMORY_BARRIER)
    return 1;

  /* A read from read-only memory can't conflict with read-write memory.  */
  if (!writep && MEM_READONLY_P (mem))
    return 0;

  /* If we have MEMs referring to different address spaces (which can
     potentially overlap), we cannot easily tell from the addresses
     whether the references overlap.  */
  if (MEM_ADDR_SPACE (mem) != MEM_ADDR_SPACE (x))
    return 1;

  mem_addr = XEXP (mem, 0);
  if (!x_addr)
    {
      x_addr = XEXP (x, 0);
      if (!((GET_CODE (x_addr) == VALUE
	     && GET_CODE (mem_addr) != VALUE
	     && reg_mentioned_p (x_addr, mem_addr))
	    || (GET_CODE (x_addr) != VALUE
		&& GET_CODE (mem_addr) == VALUE
		&& reg_mentioned_p (mem_addr, x_addr))))
	{
	  x_addr = get_addr (x_addr);
	  if (!mem_canonicalized)
	    mem_addr = get_addr (mem_addr);
	}
    }

  base = find_base_term (mem_addr);
  if (! writep
      && base
      && (GET_CODE (base) == LABEL_REF
	  || (GET_CODE (base) == SYMBOL_REF
	      && CONSTANT_POOL_ADDRESS_P (base))))
    return 0;

  rtx x_base = find_base_term (x_addr);
  if (! base_alias_check (x_addr, x_base, mem_addr, base, GET_MODE (x),
			  GET_MODE (mem)))
    return 0;

  if (!x_canonicalized)
    {
      x_addr = canon_rtx (x_addr);
      x_mode = GET_MODE (x);
    }
  if (!mem_canonicalized)
    mem_addr = canon_rtx (mem_addr);

  if ((ret = memrefs_conflict_p (SIZE_FOR_MODE (mem), mem_addr,
				 GET_MODE_SIZE (x_mode), x_addr, 0)) != -1)
    return ret;

  if (nonoverlapping_memrefs_p (x, mem, false))
    return 0;

  return rtx_refs_may_alias_p (x, mem, false);
}

/* Anti dependence: X is written after read in MEM takes place.  */

int
anti_dependence (const_rtx mem, const_rtx x)
{
  return write_dependence_p (mem, x, VOIDmode, NULL_RTX,
			     /*mem_canonicalized=*/false,
			     /*x_canonicalized*/false, /*writep=*/false);
}

/* Likewise, but we already have a canonicalized MEM, and X_ADDR for X.
   Also, consider X in X_MODE (which might be from an enclosing
   STRICT_LOW_PART / ZERO_EXTRACT).
   If MEM_CANONICALIZED is true, MEM is canonicalized.  */

int
canon_anti_dependence (const_rtx mem, bool mem_canonicalized,
		       const_rtx x, enum machine_mode x_mode, rtx x_addr)
{
  return write_dependence_p (mem, x, x_mode, x_addr,
			     mem_canonicalized, /*x_canonicalized=*/true,
			     /*writep=*/false);
}

/* Output dependence: X is written after store in MEM takes place.  */

int
output_dependence (const_rtx mem, const_rtx x)
{
  return write_dependence_p (mem, x, VOIDmode, NULL_RTX,
			     /*mem_canonicalized=*/false,
			     /*x_canonicalized*/false, /*writep=*/true);
}



/* Check whether X may be aliased with MEM.  Don't do offset-based
  memory disambiguation & TBAA.  */
int
may_alias_p (const_rtx mem, const_rtx x)
{
  rtx x_addr, mem_addr;

  if (MEM_VOLATILE_P (x) && MEM_VOLATILE_P (mem))
    return 1;

  /* (mem:BLK (scratch)) is a special mechanism to conflict with everything.
     This is used in epilogue deallocation functions.  */
  if (GET_MODE (x) == BLKmode && GET_CODE (XEXP (x, 0)) == SCRATCH)
    return 1;
  if (GET_MODE (mem) == BLKmode && GET_CODE (XEXP (mem, 0)) == SCRATCH)
    return 1;
  if (MEM_ALIAS_SET (x) == ALIAS_SET_MEMORY_BARRIER
      || MEM_ALIAS_SET (mem) == ALIAS_SET_MEMORY_BARRIER)
    return 1;

  /* Read-only memory is by definition never modified, and therefore can't
     conflict with anything.  We don't expect to find read-only set on MEM,
     but stupid user tricks can produce them, so don't die.  */
  if (MEM_READONLY_P (x))
    return 0;

  /* If we have MEMs referring to different address spaces (which can
     potentially overlap), we cannot easily tell from the addresses
     whether the references overlap.  */
  if (MEM_ADDR_SPACE (mem) != MEM_ADDR_SPACE (x))
    return 1;

  x_addr = XEXP (x, 0);
  mem_addr = XEXP (mem, 0);
  if (!((GET_CODE (x_addr) == VALUE
	 && GET_CODE (mem_addr) != VALUE
	 && reg_mentioned_p (x_addr, mem_addr))
	|| (GET_CODE (x_addr) != VALUE
	    && GET_CODE (mem_addr) == VALUE
	    && reg_mentioned_p (mem_addr, x_addr))))
    {
      x_addr = get_addr (x_addr);
      mem_addr = get_addr (mem_addr);
    }

  rtx x_base = find_base_term (x_addr);
  rtx mem_base = find_base_term (mem_addr);
  if (! base_alias_check (x_addr, x_base, mem_addr, mem_base,
			  GET_MODE (x), GET_MODE (mem_addr)))
    return 0;

  x_addr = canon_rtx (x_addr);
  mem_addr = canon_rtx (mem_addr);

  if (nonoverlapping_memrefs_p (mem, x, true))
    return 0;

  /* TBAA not valid for loop_invarint */
  return rtx_refs_may_alias_p (x, mem, false);
}

void
init_alias_target (void)
{
  int i;

  if (!arg_base_value)
    arg_base_value = gen_rtx_ADDRESS (VOIDmode, 0);

  memset (static_reg_base_value, 0, sizeof static_reg_base_value);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    /* Check whether this register can hold an incoming pointer
       argument.  FUNCTION_ARG_REGNO_P tests outgoing register
       numbers, so translate if necessary due to register windows.  */
    if (FUNCTION_ARG_REGNO_P (OUTGOING_REGNO (i))
	&& HARD_REGNO_MODE_OK (i, Pmode))
      static_reg_base_value[i] = arg_base_value;

  static_reg_base_value[STACK_POINTER_REGNUM]
    = unique_base_value (UNIQUE_BASE_VALUE_SP);
  static_reg_base_value[ARG_POINTER_REGNUM]
    = unique_base_value (UNIQUE_BASE_VALUE_ARGP);
  static_reg_base_value[FRAME_POINTER_REGNUM]
    = unique_base_value (UNIQUE_BASE_VALUE_FP);
#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
  static_reg_base_value[HARD_FRAME_POINTER_REGNUM]
    = unique_base_value (UNIQUE_BASE_VALUE_HFP);
#endif
}

/* Set MEMORY_MODIFIED when X modifies DATA (that is assumed
   to be memory reference.  */
static bool memory_modified;
static void
memory_modified_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  if (MEM_P (x))
    {
      if (anti_dependence (x, (const_rtx)data) || output_dependence (x, (const_rtx)data))
	memory_modified = true;
    }
}


/* Return true when INSN possibly modify memory contents of MEM
   (i.e. address can be modified).  */
bool
memory_modified_in_insn_p (const_rtx mem, const_rtx insn)
{
  if (!INSN_P (insn))
    return false;
  memory_modified = false;
  note_stores (PATTERN (insn), memory_modified_1, CONST_CAST_RTX(mem));
  return memory_modified;
}

/* Return TRUE if the destination of a set is rtx identical to
   ITEM.  */
static inline bool
set_dest_equal_p (const_rtx set, const_rtx item)
{
  rtx dest = SET_DEST (set);
  return rtx_equal_p (dest, item);
}

/* Like memory_modified_in_insn_p, but return TRUE if INSN will
   *DEFINITELY* modify the memory contents of MEM.  */
bool
memory_must_be_modified_in_insn_p (const_rtx mem, const_rtx insn)
{
  if (!INSN_P (insn))
    return false;
  insn = PATTERN (insn);
  if (GET_CODE (insn) == SET)
    return set_dest_equal_p (insn, mem);
  else if (GET_CODE (insn) == PARALLEL)
    {
      int i;
      for (i = 0; i < XVECLEN (insn, 0); i++)
	{
	  rtx sub = XVECEXP (insn, 0, i);
	  if (GET_CODE (sub) == SET
	      &&  set_dest_equal_p (sub, mem))
	    return true;
	}
    }
  return false;
}

/* Initialize the aliasing machinery.  Initialize the REG_KNOWN_VALUE
   array.  */

void
init_alias_analysis (void)
{
  unsigned int maxreg = max_reg_num ();
  int changed, pass;
  int i;
  unsigned int ui;
  rtx insn, val;
  int rpo_cnt;
  int *rpo;

  timevar_push (TV_ALIAS_ANALYSIS);

  vec_safe_grow_cleared (reg_known_value, maxreg - FIRST_PSEUDO_REGISTER);
  reg_known_equiv_p = sbitmap_alloc (maxreg - FIRST_PSEUDO_REGISTER);
  bitmap_clear (reg_known_equiv_p);

  /* If we have memory allocated from the previous run, use it.  */
  if (old_reg_base_value)
    reg_base_value = old_reg_base_value;

  if (reg_base_value)
    reg_base_value->truncate (0);

  vec_safe_grow_cleared (reg_base_value, maxreg);

  new_reg_base_value = XNEWVEC (rtx, maxreg);
  reg_seen = sbitmap_alloc (maxreg);

  /* The basic idea is that each pass through this loop will use the
     "constant" information from the previous pass to propagate alias
     information through another level of assignments.

     The propagation is done on the CFG in reverse post-order, to propagate
     things forward as far as possible in each iteration.

     This could get expensive if the assignment chains are long.  Maybe
     we should throttle the number of iterations, possibly based on
     the optimization level or flag_expensive_optimizations.

     We could propagate more information in the first pass by making use
     of DF_REG_DEF_COUNT to determine immediately that the alias information
     for a pseudo is "constant".

     A program with an uninitialized variable can cause an infinite loop
     here.  Instead of doing a full dataflow analysis to detect such problems
     we just cap the number of iterations for the loop.

     The state of the arrays for the set chain in question does not matter
     since the program has undefined behavior.  */

  rpo = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  rpo_cnt = pre_and_rev_post_order_compute (NULL, rpo, false);

  pass = 0;
  do
    {
      /* Assume nothing will change this iteration of the loop.  */
      changed = 0;

      /* We want to assign the same IDs each iteration of this loop, so
	 start counting from one each iteration of the loop.  */
      unique_id = 1;

      /* We're at the start of the function each iteration through the
	 loop, so we're copying arguments.  */
      copying_arguments = true;

      /* Wipe the potential alias information clean for this pass.  */
      memset (new_reg_base_value, 0, maxreg * sizeof (rtx));

      /* Wipe the reg_seen array clean.  */
      bitmap_clear (reg_seen);

      /* Initialize the alias information for this pass.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (static_reg_base_value[i])
	  {
	    new_reg_base_value[i] = static_reg_base_value[i];
	    bitmap_set_bit (reg_seen, i);
	  }

      /* Walk the insns adding values to the new_reg_base_value array.  */
      for (i = 0; i < rpo_cnt; i++)
	{
	  basic_block bb = BASIC_BLOCK (rpo[i]);
	  FOR_BB_INSNS (bb, insn)
	    {
	      if (NONDEBUG_INSN_P (insn))
		{
		  rtx note, set;

#if defined (HAVE_prologue) || defined (HAVE_epilogue)
		  /* The prologue/epilogue insns are not threaded onto the
		     insn chain until after reload has completed.  Thus,
		     there is no sense wasting time checking if INSN is in
		     the prologue/epilogue until after reload has completed.  */
		  if (reload_completed
		      && prologue_epilogue_contains (insn))
		    continue;
#endif

		  /* If this insn has a noalias note, process it,  Otherwise,
		     scan for sets.  A simple set will have no side effects
		     which could change the base value of any other register.  */

		  if (GET_CODE (PATTERN (insn)) == SET
		      && REG_NOTES (insn) != 0
		      && find_reg_note (insn, REG_NOALIAS, NULL_RTX))
		    record_set (SET_DEST (PATTERN (insn)), NULL_RTX, NULL);
		  else
		    note_stores (PATTERN (insn), record_set, NULL);

		  set = single_set (insn);

		  if (set != 0
		      && REG_P (SET_DEST (set))
		      && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER)
		    {
		      unsigned int regno = REGNO (SET_DEST (set));
		      rtx src = SET_SRC (set);
		      rtx t;

		      note = find_reg_equal_equiv_note (insn);
		      if (note && REG_NOTE_KIND (note) == REG_EQUAL
			  && DF_REG_DEF_COUNT (regno) != 1)
			note = NULL_RTX;

		      if (note != NULL_RTX
			  && GET_CODE (XEXP (note, 0)) != EXPR_LIST
			  && ! rtx_varies_p (XEXP (note, 0), 1)
			  && ! reg_overlap_mentioned_p (SET_DEST (set),
							XEXP (note, 0)))
			{
			  set_reg_known_value (regno, XEXP (note, 0));
			  set_reg_known_equiv_p (regno,
						 REG_NOTE_KIND (note) == REG_EQUIV);
			}
		      else if (DF_REG_DEF_COUNT (regno) == 1
			       && GET_CODE (src) == PLUS
			       && REG_P (XEXP (src, 0))
			       && (t = get_reg_known_value (REGNO (XEXP (src, 0))))
			       && CONST_INT_P (XEXP (src, 1)))
			{
			  t = plus_constant (GET_MODE (src), t,
					     INTVAL (XEXP (src, 1)));
			  set_reg_known_value (regno, t);
			  set_reg_known_equiv_p (regno, false);
			}
		      else if (DF_REG_DEF_COUNT (regno) == 1
			       && ! rtx_varies_p (src, 1))
			{
			  set_reg_known_value (regno, src);
			  set_reg_known_equiv_p (regno, false);
			}
		    }
		}
	      else if (NOTE_P (insn)
		       && NOTE_KIND (insn) == NOTE_INSN_FUNCTION_BEG)
		copying_arguments = false;
	    }
	}

      /* Now propagate values from new_reg_base_value to reg_base_value.  */
      gcc_assert (maxreg == (unsigned int) max_reg_num ());

      for (ui = 0; ui < maxreg; ui++)
	{
	  if (new_reg_base_value[ui]
	      && new_reg_base_value[ui] != (*reg_base_value)[ui]
	      && ! rtx_equal_p (new_reg_base_value[ui], (*reg_base_value)[ui]))
	    {
	      (*reg_base_value)[ui] = new_reg_base_value[ui];
	      changed = 1;
	    }
	}
    }
  while (changed && ++pass < MAX_ALIAS_LOOP_PASSES);
  XDELETEVEC (rpo);

  /* Fill in the remaining entries.  */
  FOR_EACH_VEC_ELT (*reg_known_value, i, val)
    {
      int regno = i + FIRST_PSEUDO_REGISTER;
      if (! val)
	set_reg_known_value (regno, regno_reg_rtx[regno]);
    }

  /* Clean up.  */
  free (new_reg_base_value);
  new_reg_base_value = 0;
  sbitmap_free (reg_seen);
  reg_seen = 0;
  timevar_pop (TV_ALIAS_ANALYSIS);
}

/* Equate REG_BASE_VALUE (reg1) to REG_BASE_VALUE (reg2).
   Special API for var-tracking pass purposes.  */

void
vt_equate_reg_base_value (const_rtx reg1, const_rtx reg2)
{
  (*reg_base_value)[REGNO (reg1)] = REG_BASE_VALUE (reg2);
}

void
end_alias_analysis (void)
{
  old_reg_base_value = reg_base_value;
  vec_free (reg_known_value);
  sbitmap_free (reg_known_equiv_p);
}

#include "gt-alias.h"
