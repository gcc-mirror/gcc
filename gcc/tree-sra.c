/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2008-2013 Free Software Foundation, Inc.
   Contributed by Martin Jambor <mjambor@suse.cz>

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

/* This file implements Scalar Reduction of Aggregates (SRA).  SRA is run
   twice, once in the early stages of compilation (early SRA) and once in the
   late stages (late SRA).  The aim of both is to turn references to scalar
   parts of aggregates into uses of independent scalar variables.

   The two passes are nearly identical, the only difference is that early SRA
   does not scalarize unions which are used as the result in a GIMPLE_RETURN
   statement because together with inlining this can lead to weird type
   conversions.

   Both passes operate in four stages:

   1. The declarations that have properties which make them candidates for
      scalarization are identified in function find_var_candidates().  The
      candidates are stored in candidate_bitmap.

   2. The function body is scanned.  In the process, declarations which are
      used in a manner that prevent their scalarization are removed from the
      candidate bitmap.  More importantly, for every access into an aggregate,
      an access structure (struct access) is created by create_access() and
      stored in a vector associated with the aggregate.  Among other
      information, the aggregate declaration, the offset and size of the access
      and its type are stored in the structure.

      On a related note, assign_link structures are created for every assign
      statement between candidate aggregates and attached to the related
      accesses.

   3. The vectors of accesses are analyzed.  They are first sorted according to
      their offset and size and then scanned for partially overlapping accesses
      (i.e. those which overlap but one is not entirely within another).  Such
      an access disqualifies the whole aggregate from being scalarized.

      If there is no such inhibiting overlap, a representative access structure
      is chosen for every unique combination of offset and size.  Afterwards,
      the pass builds a set of trees from these structures, in which children
      of an access are within their parent (in terms of offset and size).

      Then accesses  are propagated  whenever possible (i.e.  in cases  when it
      does not create a partially overlapping access) across assign_links from
      the right hand side to the left hand side.

      Then the set of trees for each declaration is traversed again and those
      accesses which should be replaced by a scalar are identified.

   4. The function is traversed again, and for every reference into an
      aggregate that has some component which is about to be scalarized,
      statements are amended and new statements are created as necessary.
      Finally, if a parameter got scalarized, the scalar replacements are
      initialized with values from respective parameter aggregates.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hash-table.h"
#include "alloc-pool.h"
#include "tm.h"
#include "tree.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "bitmap.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "ipa-prop.h"
#include "statistics.h"
#include "params.h"
#include "target.h"
#include "flags.h"
#include "dbgcnt.h"
#include "tree-inline.h"
#include "gimple-pretty-print.h"
#include "ipa-inline.h"
#include "ipa-utils.h"

/* Enumeration of all aggregate reductions we can do.  */
enum sra_mode { SRA_MODE_EARLY_IPA,   /* early call regularization */
		SRA_MODE_EARLY_INTRA, /* early intraprocedural SRA */
		SRA_MODE_INTRA };     /* late intraprocedural SRA */

/* Global variable describing which aggregate reduction we are performing at
   the moment.  */
static enum sra_mode sra_mode;

struct assign_link;

/* ACCESS represents each access to an aggregate variable (as a whole or a
   part).  It can also represent a group of accesses that refer to exactly the
   same fragment of an aggregate (i.e. those that have exactly the same offset
   and size).  Such representatives for a single aggregate, once determined,
   are linked in a linked list and have the group fields set.

   Moreover, when doing intraprocedural SRA, a tree is built from those
   representatives (by the means of first_child and next_sibling pointers), in
   which all items in a subtree are "within" the root, i.e. their offset is
   greater or equal to offset of the root and offset+size is smaller or equal
   to offset+size of the root.  Children of an access are sorted by offset.

   Note that accesses to parts of vector and complex number types always
   represented by an access to the whole complex number or a vector.  It is a
   duty of the modifying functions to replace them appropriately.  */

struct access
{
  /* Values returned by  `get_ref_base_and_extent' for each component reference
     If EXPR isn't a component reference  just set `BASE = EXPR', `OFFSET = 0',
     `SIZE = TREE_SIZE (TREE_TYPE (expr))'.  */
  HOST_WIDE_INT offset;
  HOST_WIDE_INT size;
  tree base;

  /* Expression.  It is context dependent so do not use it to create new
     expressions to access the original aggregate.  See PR 42154 for a
     testcase.  */
  tree expr;
  /* Type.  */
  tree type;

  /* The statement this access belongs to.  */
  gimple stmt;

  /* Next group representative for this aggregate. */
  struct access *next_grp;

  /* Pointer to the group representative.  Pointer to itself if the struct is
     the representative.  */
  struct access *group_representative;

  /* If this access has any children (in terms of the definition above), this
     points to the first one.  */
  struct access *first_child;

  /* In intraprocedural SRA, pointer to the next sibling in the access tree as
     described above.  In IPA-SRA this is a pointer to the next access
     belonging to the same group (having the same representative).  */
  struct access *next_sibling;

  /* Pointers to the first and last element in the linked list of assign
     links.  */
  struct assign_link *first_link, *last_link;

  /* Pointer to the next access in the work queue.  */
  struct access *next_queued;

  /* Replacement variable for this access "region."  Never to be accessed
     directly, always only by the means of get_access_replacement() and only
     when grp_to_be_replaced flag is set.  */
  tree replacement_decl;

  /* Is this particular access write access? */
  unsigned write : 1;

  /* Is this access an access to a non-addressable field? */
  unsigned non_addressable : 1;

  /* Is this access currently in the work queue?  */
  unsigned grp_queued : 1;

  /* Does this group contain a write access?  This flag is propagated down the
     access tree.  */
  unsigned grp_write : 1;

  /* Does this group contain a read access?  This flag is propagated down the
     access tree.  */
  unsigned grp_read : 1;

  /* Does this group contain a read access that comes from an assignment
     statement?  This flag is propagated down the access tree.  */
  unsigned grp_assignment_read : 1;

  /* Does this group contain a write access that comes from an assignment
     statement?  This flag is propagated down the access tree.  */
  unsigned grp_assignment_write : 1;

  /* Does this group contain a read access through a scalar type?  This flag is
     not propagated in the access tree in any direction.  */
  unsigned grp_scalar_read : 1;

  /* Does this group contain a write access through a scalar type?  This flag
     is not propagated in the access tree in any direction.  */
  unsigned grp_scalar_write : 1;

  /* Is this access an artificial one created to scalarize some record
     entirely? */
  unsigned grp_total_scalarization : 1;

  /* Other passes of the analysis use this bit to make function
     analyze_access_subtree create scalar replacements for this group if
     possible.  */
  unsigned grp_hint : 1;

  /* Is the subtree rooted in this access fully covered by scalar
     replacements?  */
  unsigned grp_covered : 1;

  /* If set to true, this access and all below it in an access tree must not be
     scalarized.  */
  unsigned grp_unscalarizable_region : 1;

  /* Whether data have been written to parts of the aggregate covered by this
     access which is not to be scalarized.  This flag is propagated up in the
     access tree.  */
  unsigned grp_unscalarized_data : 1;

  /* Does this access and/or group contain a write access through a
     BIT_FIELD_REF?  */
  unsigned grp_partial_lhs : 1;

  /* Set when a scalar replacement should be created for this variable.  */
  unsigned grp_to_be_replaced : 1;

  /* Set when we want a replacement for the sole purpose of having it in
     generated debug statements.  */
  unsigned grp_to_be_debug_replaced : 1;

  /* Should TREE_NO_WARNING of a replacement be set?  */
  unsigned grp_no_warning : 1;

  /* Is it possible that the group refers to data which might be (directly or
     otherwise) modified?  */
  unsigned grp_maybe_modified : 1;

  /* Set when this is a representative of a pointer to scalar (i.e. by
     reference) parameter which we consider for turning into a plain scalar
     (i.e. a by value parameter).  */
  unsigned grp_scalar_ptr : 1;

  /* Set when we discover that this pointer is not safe to dereference in the
     caller.  */
  unsigned grp_not_necessarilly_dereferenced : 1;
};

typedef struct access *access_p;


/* Alloc pool for allocating access structures.  */
static alloc_pool access_pool;

/* A structure linking lhs and rhs accesses from an aggregate assignment.  They
   are used to propagate subaccesses from rhs to lhs as long as they don't
   conflict with what is already there.  */
struct assign_link
{
  struct access *lacc, *racc;
  struct assign_link *next;
};

/* Alloc pool for allocating assign link structures.  */
static alloc_pool link_pool;

/* Base (tree) -> Vector (vec<access_p> *) map.  */
static struct pointer_map_t *base_access_vec;

/* Candidate hash table helpers.  */

struct uid_decl_hasher : typed_noop_remove <tree_node>
{
  typedef tree_node value_type;
  typedef tree_node compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Hash a tree in a uid_decl_map.  */

inline hashval_t
uid_decl_hasher::hash (const value_type *item)
{
  return item->decl_minimal.uid;
}

/* Return true if the DECL_UID in both trees are equal.  */

inline bool
uid_decl_hasher::equal (const value_type *a, const compare_type *b)
{
  return (a->decl_minimal.uid == b->decl_minimal.uid);
}

/* Set of candidates.  */
static bitmap candidate_bitmap;
static hash_table <uid_decl_hasher> candidates;

/* For a candidate UID return the candidates decl.  */

static inline tree
candidate (unsigned uid)
{
 tree_node t;
 t.decl_minimal.uid = uid;
 return candidates.find_with_hash (&t, static_cast <hashval_t> (uid));
}

/* Bitmap of candidates which we should try to entirely scalarize away and
   those which cannot be (because they are and need be used as a whole).  */
static bitmap should_scalarize_away_bitmap, cannot_scalarize_away_bitmap;

/* Obstack for creation of fancy names.  */
static struct obstack name_obstack;

/* Head of a linked list of accesses that need to have its subaccesses
   propagated to their assignment counterparts. */
static struct access *work_queue_head;

/* Number of parameters of the analyzed function when doing early ipa SRA.  */
static int func_param_count;

/* scan_function sets the following to true if it encounters a call to
   __builtin_apply_args.  */
static bool encountered_apply_args;

/* Set by scan_function when it finds a recursive call.  */
static bool encountered_recursive_call;

/* Set by scan_function when it finds a recursive call with less actual
   arguments than formal parameters..  */
static bool encountered_unchangable_recursive_call;

/* This is a table in which for each basic block and parameter there is a
   distance (offset + size) in that parameter which is dereferenced and
   accessed in that BB.  */
static HOST_WIDE_INT *bb_dereferences;
/* Bitmap of BBs that can cause the function to "stop" progressing by
   returning, throwing externally, looping infinitely or calling a function
   which might abort etc.. */
static bitmap final_bbs;

/* Representative of no accesses at all. */
static struct access  no_accesses_representant;

/* Predicate to test the special value.  */

static inline bool
no_accesses_p (struct access *access)
{
  return access == &no_accesses_representant;
}

/* Dump contents of ACCESS to file F in a human friendly way.  If GRP is true,
   representative fields are dumped, otherwise those which only describe the
   individual access are.  */

static struct
{
  /* Number of processed aggregates is readily available in
     analyze_all_variable_accesses and so is not stored here.  */

  /* Number of created scalar replacements.  */
  int replacements;

  /* Number of times sra_modify_expr or sra_modify_assign themselves changed an
     expression.  */
  int exprs;

  /* Number of statements created by generate_subtree_copies.  */
  int subtree_copies;

  /* Number of statements created by load_assign_lhs_subreplacements.  */
  int subreplacements;

  /* Number of times sra_modify_assign has deleted a statement.  */
  int deleted;

  /* Number of times sra_modify_assign has to deal with subaccesses of LHS and
     RHS reparately due to type conversions or nonexistent matching
     references.  */
  int separate_lhs_rhs_handling;

  /* Number of parameters that were removed because they were unused.  */
  int deleted_unused_parameters;

  /* Number of scalars passed as parameters by reference that have been
     converted to be passed by value.  */
  int scalar_by_ref_to_by_val;

  /* Number of aggregate parameters that were replaced by one or more of their
     components.  */
  int aggregate_params_reduced;

  /* Numbber of components created when splitting aggregate parameters.  */
  int param_reductions_created;
} sra_stats;

static void
dump_access (FILE *f, struct access *access, bool grp)
{
  fprintf (f, "access { ");
  fprintf (f, "base = (%d)'", DECL_UID (access->base));
  print_generic_expr (f, access->base, 0);
  fprintf (f, "', offset = " HOST_WIDE_INT_PRINT_DEC, access->offset);
  fprintf (f, ", size = " HOST_WIDE_INT_PRINT_DEC, access->size);
  fprintf (f, ", expr = ");
  print_generic_expr (f, access->expr, 0);
  fprintf (f, ", type = ");
  print_generic_expr (f, access->type, 0);
  if (grp)
    fprintf (f, ", grp_read = %d, grp_write = %d, grp_assignment_read = %d, "
	     "grp_assignment_write = %d, grp_scalar_read = %d, "
	     "grp_scalar_write = %d, grp_total_scalarization = %d, "
	     "grp_hint = %d, grp_covered = %d, "
	     "grp_unscalarizable_region = %d, grp_unscalarized_data = %d, "
	     "grp_partial_lhs = %d, grp_to_be_replaced = %d, "
	     "grp_to_be_debug_replaced = %d, grp_maybe_modified = %d, "
	     "grp_not_necessarilly_dereferenced = %d\n",
	     access->grp_read, access->grp_write, access->grp_assignment_read,
	     access->grp_assignment_write, access->grp_scalar_read,
	     access->grp_scalar_write, access->grp_total_scalarization,
	     access->grp_hint, access->grp_covered,
	     access->grp_unscalarizable_region, access->grp_unscalarized_data,
	     access->grp_partial_lhs, access->grp_to_be_replaced,
	     access->grp_to_be_debug_replaced, access->grp_maybe_modified,
	     access->grp_not_necessarilly_dereferenced);
  else
    fprintf (f, ", write = %d, grp_total_scalarization = %d, "
	     "grp_partial_lhs = %d\n",
	     access->write, access->grp_total_scalarization,
	     access->grp_partial_lhs);
}

/* Dump a subtree rooted in ACCESS to file F, indent by LEVEL.  */

static void
dump_access_tree_1 (FILE *f, struct access *access, int level)
{
  do
    {
      int i;

      for (i = 0; i < level; i++)
	fputs ("* ", dump_file);

      dump_access (f, access, true);

      if (access->first_child)
	dump_access_tree_1 (f, access->first_child, level + 1);

      access = access->next_sibling;
    }
  while (access);
}

/* Dump all access trees for a variable, given the pointer to the first root in
   ACCESS.  */

static void
dump_access_tree (FILE *f, struct access *access)
{
  for (; access; access = access->next_grp)
    dump_access_tree_1 (f, access, 0);
}

/* Return true iff ACC is non-NULL and has subaccesses.  */

static inline bool
access_has_children_p (struct access *acc)
{
  return acc && acc->first_child;
}

/* Return true iff ACC is (partly) covered by at least one replacement.  */

static bool
access_has_replacements_p (struct access *acc)
{
  struct access *child;
  if (acc->grp_to_be_replaced)
    return true;
  for (child = acc->first_child; child; child = child->next_sibling)
    if (access_has_replacements_p (child))
      return true;
  return false;
}

/* Return a vector of pointers to accesses for the variable given in BASE or
   NULL if there is none.  */

static vec<access_p> *
get_base_access_vector (tree base)
{
  void **slot;

  slot = pointer_map_contains (base_access_vec, base);
  if (!slot)
    return NULL;
  else
    return *(vec<access_p> **) slot;
}

/* Find an access with required OFFSET and SIZE in a subtree of accesses rooted
   in ACCESS.  Return NULL if it cannot be found.  */

static struct access *
find_access_in_subtree (struct access *access, HOST_WIDE_INT offset,
			HOST_WIDE_INT size)
{
  while (access && (access->offset != offset || access->size != size))
    {
      struct access *child = access->first_child;

      while (child && (child->offset + child->size <= offset))
	child = child->next_sibling;
      access = child;
    }

  return access;
}

/* Return the first group representative for DECL or NULL if none exists.  */

static struct access *
get_first_repr_for_decl (tree base)
{
  vec<access_p> *access_vec;

  access_vec = get_base_access_vector (base);
  if (!access_vec)
    return NULL;

  return (*access_vec)[0];
}

/* Find an access representative for the variable BASE and given OFFSET and
   SIZE.  Requires that access trees have already been built.  Return NULL if
   it cannot be found.  */

static struct access *
get_var_base_offset_size_access (tree base, HOST_WIDE_INT offset,
				 HOST_WIDE_INT size)
{
  struct access *access;

  access = get_first_repr_for_decl (base);
  while (access && (access->offset + access->size <= offset))
    access = access->next_grp;
  if (!access)
    return NULL;

  return find_access_in_subtree (access, offset, size);
}

/* Add LINK to the linked list of assign links of RACC.  */
static void
add_link_to_rhs (struct access *racc, struct assign_link *link)
{
  gcc_assert (link->racc == racc);

  if (!racc->first_link)
    {
      gcc_assert (!racc->last_link);
      racc->first_link = link;
    }
  else
    racc->last_link->next = link;

  racc->last_link = link;
  link->next = NULL;
}

/* Move all link structures in their linked list in OLD_RACC to the linked list
   in NEW_RACC.  */
static void
relink_to_new_repr (struct access *new_racc, struct access *old_racc)
{
  if (!old_racc->first_link)
    {
      gcc_assert (!old_racc->last_link);
      return;
    }

  if (new_racc->first_link)
    {
      gcc_assert (!new_racc->last_link->next);
      gcc_assert (!old_racc->last_link || !old_racc->last_link->next);

      new_racc->last_link->next = old_racc->first_link;
      new_racc->last_link = old_racc->last_link;
    }
  else
    {
      gcc_assert (!new_racc->last_link);

      new_racc->first_link = old_racc->first_link;
      new_racc->last_link = old_racc->last_link;
    }
  old_racc->first_link = old_racc->last_link = NULL;
}

/* Add ACCESS to the work queue (which is actually a stack).  */

static void
add_access_to_work_queue (struct access *access)
{
  if (!access->grp_queued)
    {
      gcc_assert (!access->next_queued);
      access->next_queued = work_queue_head;
      access->grp_queued = 1;
      work_queue_head = access;
    }
}

/* Pop an access from the work queue, and return it, assuming there is one.  */

static struct access *
pop_access_from_work_queue (void)
{
  struct access *access = work_queue_head;

  work_queue_head = access->next_queued;
  access->next_queued = NULL;
  access->grp_queued = 0;
  return access;
}


/* Allocate necessary structures.  */

static void
sra_initialize (void)
{
  candidate_bitmap = BITMAP_ALLOC (NULL);
  candidates.create (vec_safe_length (cfun->local_decls) / 2);
  should_scalarize_away_bitmap = BITMAP_ALLOC (NULL);
  cannot_scalarize_away_bitmap = BITMAP_ALLOC (NULL);
  gcc_obstack_init (&name_obstack);
  access_pool = create_alloc_pool ("SRA accesses", sizeof (struct access), 16);
  link_pool = create_alloc_pool ("SRA links", sizeof (struct assign_link), 16);
  base_access_vec = pointer_map_create ();
  memset (&sra_stats, 0, sizeof (sra_stats));
  encountered_apply_args = false;
  encountered_recursive_call = false;
  encountered_unchangable_recursive_call = false;
}

/* Hook fed to pointer_map_traverse, deallocate stored vectors.  */

static bool
delete_base_accesses (const void *key ATTRIBUTE_UNUSED, void **value,
		     void *data ATTRIBUTE_UNUSED)
{
  vec<access_p> *access_vec = (vec<access_p> *) *value;
  vec_free (access_vec);
  return true;
}

/* Deallocate all general structures.  */

static void
sra_deinitialize (void)
{
  BITMAP_FREE (candidate_bitmap);
  candidates.dispose ();
  BITMAP_FREE (should_scalarize_away_bitmap);
  BITMAP_FREE (cannot_scalarize_away_bitmap);
  free_alloc_pool (access_pool);
  free_alloc_pool (link_pool);
  obstack_free (&name_obstack, NULL);

  pointer_map_traverse (base_access_vec, delete_base_accesses, NULL);
  pointer_map_destroy (base_access_vec);
}

/* Remove DECL from candidates for SRA and write REASON to the dump file if
   there is one.  */
static void
disqualify_candidate (tree decl, const char *reason)
{
  if (bitmap_clear_bit (candidate_bitmap, DECL_UID (decl)))
    candidates.clear_slot (candidates.find_slot_with_hash (decl,
							   DECL_UID (decl),
							   NO_INSERT));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "! Disqualifying ");
      print_generic_expr (dump_file, decl, 0);
      fprintf (dump_file, " - %s\n", reason);
    }
}

/* Return true iff the type contains a field or an element which does not allow
   scalarization.  */

static bool
type_internals_preclude_sra_p (tree type, const char **msg)
{
  tree fld;
  tree et;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL)
	  {
	    tree ft = TREE_TYPE (fld);

	    if (TREE_THIS_VOLATILE (fld))
	      {
		*msg = "volatile structure field";
		return true;
	      }
	    if (!DECL_FIELD_OFFSET (fld))
	      {
		*msg = "no structure field offset";
		return true;
	      }
	    if (!DECL_SIZE (fld))
	      {
		*msg = "zero structure field size";
	        return true;
	      }
	    if (!host_integerp (DECL_FIELD_OFFSET (fld), 1))
	      {
		*msg = "structure field offset not fixed";
		return true;
	      }
	    if (!host_integerp (DECL_SIZE (fld), 1))
	      {
	        *msg = "structure field size not fixed";
		return true;
	      }
	    if (!host_integerp (bit_position (fld), 0))
	      {
	        *msg = "structure field size too big";
		return true;
	      }
	    if (AGGREGATE_TYPE_P (ft)
		    && int_bit_position (fld) % BITS_PER_UNIT != 0)
	      {
		*msg = "structure field is bit field";
	        return true;
	      }

	    if (AGGREGATE_TYPE_P (ft) && type_internals_preclude_sra_p (ft, msg))
	      return true;
	  }

      return false;

    case ARRAY_TYPE:
      et = TREE_TYPE (type);

      if (TYPE_VOLATILE (et))
	{
	  *msg = "element type is volatile";
	  return true;
	}

      if (AGGREGATE_TYPE_P (et) && type_internals_preclude_sra_p (et, msg))
	return true;

      return false;

    default:
      return false;
    }
}

/* If T is an SSA_NAME, return NULL if it is not a default def or return its
   base variable if it is.  Return T if it is not an SSA_NAME.  */

static tree
get_ssa_base_param (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (t))
	return SSA_NAME_VAR (t);
      else
	return NULL_TREE;
    }
  return t;
}

/* Mark a dereference of BASE of distance DIST in a basic block tht STMT
   belongs to, unless the BB has already been marked as a potentially
   final.  */

static void
mark_parm_dereference (tree base, HOST_WIDE_INT dist, gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  int idx, parm_index = 0;
  tree parm;

  if (bitmap_bit_p (final_bbs, bb->index))
    return;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm && parm != base;
       parm = DECL_CHAIN (parm))
    parm_index++;

  gcc_assert (parm_index < func_param_count);

  idx = bb->index * func_param_count + parm_index;
  if (bb_dereferences[idx] < dist)
    bb_dereferences[idx] = dist;
}

/* Allocate an access structure for BASE, OFFSET and SIZE, clear it, fill in
   the three fields.  Also add it to the vector of accesses corresponding to
   the base.  Finally, return the new access.  */

static struct access *
create_access_1 (tree base, HOST_WIDE_INT offset, HOST_WIDE_INT size)
{
  vec<access_p> *v;
  struct access *access;
  void **slot;

  access = (struct access *) pool_alloc (access_pool);
  memset (access, 0, sizeof (struct access));
  access->base = base;
  access->offset = offset;
  access->size = size;

  slot = pointer_map_contains (base_access_vec, base);
  if (slot)
    v = (vec<access_p> *) *slot;
  else
    vec_alloc (v, 32);

  v->safe_push (access);

  *((vec<access_p> **)
	pointer_map_insert (base_access_vec, base)) = v;

  return access;
}

/* Create and insert access for EXPR. Return created access, or NULL if it is
   not possible.  */

static struct access *
create_access (tree expr, gimple stmt, bool write)
{
  struct access *access;
  HOST_WIDE_INT offset, size, max_size;
  tree base = expr;
  bool ptr, unscalarizable_region = false;

  base = get_ref_base_and_extent (expr, &offset, &size, &max_size);

  if (sra_mode == SRA_MODE_EARLY_IPA
      && TREE_CODE (base) == MEM_REF)
    {
      base = get_ssa_base_param (TREE_OPERAND (base, 0));
      if (!base)
	return NULL;
      ptr = true;
    }
  else
    ptr = false;

  if (!DECL_P (base) || !bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

  if (sra_mode == SRA_MODE_EARLY_IPA)
    {
      if (size < 0 || size != max_size)
	{
	  disqualify_candidate (base, "Encountered a variable sized access.");
	  return NULL;
	}
      if (TREE_CODE (expr) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
	{
	  disqualify_candidate (base, "Encountered a bit-field access.");
	  return NULL;
	}
      gcc_checking_assert ((offset % BITS_PER_UNIT) == 0);

      if (ptr)
	mark_parm_dereference (base, offset + size, stmt);
    }
  else
    {
      if (size != max_size)
	{
	  size = max_size;
	  unscalarizable_region = true;
	}
      if (size < 0)
	{
	  disqualify_candidate (base, "Encountered an unconstrained access.");
	  return NULL;
	}
    }

  access = create_access_1 (base, offset, size);
  access->expr = expr;
  access->type = TREE_TYPE (expr);
  access->write = write;
  access->grp_unscalarizable_region = unscalarizable_region;
  access->stmt = stmt;

  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_NONADDRESSABLE_P (TREE_OPERAND (expr, 1)))
    access->non_addressable = 1;

  return access;
}


/* Return true iff TYPE is a RECORD_TYPE with fields that are either of gimple
   register types or (recursively) records with only these two kinds of fields.
   It also returns false if any of these records contains a bit-field.  */

static bool
type_consists_of_records_p (tree type)
{
  tree fld;

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FIELD_DECL)
      {
	tree ft = TREE_TYPE (fld);

	if (DECL_BIT_FIELD (fld))
	  return false;

	if (!is_gimple_reg_type (ft)
	    && !type_consists_of_records_p (ft))
	  return false;
      }

  return true;
}

/* Create total_scalarization accesses for all scalar type fields in DECL that
   must be of a RECORD_TYPE conforming to type_consists_of_records_p.  BASE
   must be the top-most VAR_DECL representing the variable, OFFSET must be the
   offset of DECL within BASE.  REF must be the memory reference expression for
   the given decl.  */

static void
completely_scalarize_record (tree base, tree decl, HOST_WIDE_INT offset,
			     tree ref)
{
  tree fld, decl_type = TREE_TYPE (decl);

  for (fld = TYPE_FIELDS (decl_type); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FIELD_DECL)
      {
	HOST_WIDE_INT pos = offset + int_bit_position (fld);
	tree ft = TREE_TYPE (fld);
	tree nref = build3 (COMPONENT_REF, TREE_TYPE (fld), ref, fld,
			    NULL_TREE);

	if (is_gimple_reg_type (ft))
	  {
	    struct access *access;
	    HOST_WIDE_INT size;

	    size = tree_low_cst (DECL_SIZE (fld), 1);
	    access = create_access_1 (base, pos, size);
	    access->expr = nref;
	    access->type = ft;
	    access->grp_total_scalarization = 1;
	    /* Accesses for intraprocedural SRA can have their stmt NULL.  */
	  }
	else
	  completely_scalarize_record (base, fld, pos, nref);
      }
}

/* Create total_scalarization accesses for all scalar type fields in VAR and
   for VAR a a whole.  VAR must be of a RECORD_TYPE conforming to
   type_consists_of_records_p.   */

static void
completely_scalarize_var (tree var)
{
  HOST_WIDE_INT size = tree_low_cst (DECL_SIZE (var), 1);
  struct access *access;

  access = create_access_1 (var, 0, size);
  access->expr = var;
  access->type = TREE_TYPE (var);
  access->grp_total_scalarization = 1;

  completely_scalarize_record (var, var, 0, var);
}

/* Return true if REF has an VIEW_CONVERT_EXPR somewhere in it.  */

static inline bool
contains_view_convert_expr_p (const_tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == VIEW_CONVERT_EXPR)
	return true;
      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Search the given tree for a declaration by skipping handled components and
   exclude it from the candidates.  */

static void
disqualify_base_of_expr (tree t, const char *reason)
{
  t = get_base_address (t);
  if (sra_mode == SRA_MODE_EARLY_IPA
      && TREE_CODE (t) == MEM_REF)
    t = get_ssa_base_param (TREE_OPERAND (t, 0));

  if (t && DECL_P (t))
    disqualify_candidate (t, reason);
}

/* Scan expression EXPR and create access structures for all accesses to
   candidates for scalarization.  Return the created access or NULL if none is
   created.  */

static struct access *
build_access_from_expr_1 (tree expr, gimple stmt, bool write)
{
  struct access *ret = NULL;
  bool partial_ref;

  if (TREE_CODE (expr) == BIT_FIELD_REF
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == REALPART_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      partial_ref = true;
    }
  else
    partial_ref = false;

  /* We need to dive through V_C_Es in order to get the size of its parameter
     and not the result type.  Ada produces such statements.  We are also
     capable of handling the topmost V_C_E but not any of those buried in other
     handled components.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    expr = TREE_OPERAND (expr, 0);

  if (contains_view_convert_expr_p (expr))
    {
      disqualify_base_of_expr (expr, "V_C_E under a different handled "
			       "component.");
      return NULL;
    }

  switch (TREE_CODE (expr))
    {
    case MEM_REF:
      if (TREE_CODE (TREE_OPERAND (expr, 0)) != ADDR_EXPR
	  && sra_mode != SRA_MODE_EARLY_IPA)
	return NULL;
      /* fall through */
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      ret = create_access (expr, stmt, write);
      break;

    default:
      break;
    }

  if (write && partial_ref && ret)
    ret->grp_partial_lhs = 1;

  return ret;
}

/* Scan expression EXPR and create access structures for all accesses to
   candidates for scalarization.  Return true if any access has been inserted.
   STMT must be the statement from which the expression is taken, WRITE must be
   true if the expression is a store and false otherwise. */

static bool
build_access_from_expr (tree expr, gimple stmt, bool write)
{
  struct access *access;

  access = build_access_from_expr_1 (expr, stmt, write);
  if (access)
    {
      /* This means the aggregate is accesses as a whole in a way other than an
	 assign statement and thus cannot be removed even if we had a scalar
	 replacement for everything.  */
      if (cannot_scalarize_away_bitmap)
	bitmap_set_bit (cannot_scalarize_away_bitmap, DECL_UID (access->base));
      return true;
    }
  return false;
}

/* Disqualify LHS and RHS for scalarization if STMT must end its basic block in
   modes in which it matters, return true iff they have been disqualified.  RHS
   may be NULL, in that case ignore it.  If we scalarize an aggregate in
   intra-SRA we may need to add statements after each statement.  This is not
   possible if a statement unconditionally has to end the basic block.  */
static bool
disqualify_ops_if_throwing_stmt (gimple stmt, tree lhs, tree rhs)
{
  if ((sra_mode == SRA_MODE_EARLY_INTRA || sra_mode == SRA_MODE_INTRA)
      && (stmt_can_throw_internal (stmt) || stmt_ends_bb_p (stmt)))
    {
      disqualify_base_of_expr (lhs, "LHS of a throwing stmt.");
      if (rhs)
	disqualify_base_of_expr (rhs, "RHS of a throwing stmt.");
      return true;
    }
  return false;
}

/* Scan expressions occurring in STMT, create access structures for all accesses
   to candidates for scalarization and remove those candidates which occur in
   statements or expressions that prevent them from being split apart.  Return
   true if any access has been inserted.  */

static bool
build_accesses_from_assign (gimple stmt)
{
  tree lhs, rhs;
  struct access *lacc, *racc;

  if (!gimple_assign_single_p (stmt)
      /* Scope clobbers don't influence scalarization.  */
      || gimple_clobber_p (stmt))
    return false;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);

  if (disqualify_ops_if_throwing_stmt (stmt, lhs, rhs))
    return false;

  racc = build_access_from_expr_1 (rhs, stmt, false);
  lacc = build_access_from_expr_1 (lhs, stmt, true);

  if (lacc)
    lacc->grp_assignment_write = 1;

  if (racc)
    {
      racc->grp_assignment_read = 1;
      if (should_scalarize_away_bitmap && !gimple_has_volatile_ops (stmt)
	  && !is_gimple_reg_type (racc->type))
	bitmap_set_bit (should_scalarize_away_bitmap, DECL_UID (racc->base));
    }

  if (lacc && racc
      && (sra_mode == SRA_MODE_EARLY_INTRA || sra_mode == SRA_MODE_INTRA)
      && !lacc->grp_unscalarizable_region
      && !racc->grp_unscalarizable_region
      && AGGREGATE_TYPE_P (TREE_TYPE (lhs))
      && lacc->size == racc->size
      && useless_type_conversion_p (lacc->type, racc->type))
    {
      struct assign_link *link;

      link = (struct assign_link *) pool_alloc (link_pool);
      memset (link, 0, sizeof (struct assign_link));

      link->lacc = lacc;
      link->racc = racc;

      add_link_to_rhs (racc, link);
    }

  return lacc || racc;
}

/* Callback of walk_stmt_load_store_addr_ops visit_addr used to determine
   GIMPLE_ASM operands with memory constrains which cannot be scalarized.  */

static bool
asm_visit_addr (gimple stmt ATTRIBUTE_UNUSED, tree op,
		void *data ATTRIBUTE_UNUSED)
{
  op = get_base_address (op);
  if (op
      && DECL_P (op))
    disqualify_candidate (op, "Non-scalarizable GIMPLE_ASM operand.");

  return false;
}

/* Return true iff callsite CALL has at least as many actual arguments as there
   are formal parameters of the function currently processed by IPA-SRA.  */

static inline bool
callsite_has_enough_arguments_p (gimple call)
{
  return gimple_call_num_args (call) >= (unsigned) func_param_count;
}

/* Scan function and look for interesting expressions and create access
   structures for them.  Return true iff any access is created.  */

static bool
scan_function (void)
{
  basic_block bb;
  bool ret = false;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree t;
	  unsigned i;

	  if (final_bbs && stmt_can_throw_external (stmt))
	    bitmap_set_bit (final_bbs, bb->index);
	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval (stmt);
	      if (t != NULL_TREE)
		ret |= build_access_from_expr (t, stmt, false);
	      if (final_bbs)
		bitmap_set_bit (final_bbs, bb->index);
	      break;

	    case GIMPLE_ASSIGN:
	      ret |= build_accesses_from_assign (stmt);
	      break;

	    case GIMPLE_CALL:
	      for (i = 0; i < gimple_call_num_args (stmt); i++)
		ret |= build_access_from_expr (gimple_call_arg (stmt, i),
					       stmt, false);

	      if (sra_mode == SRA_MODE_EARLY_IPA)
		{
		  tree dest = gimple_call_fndecl (stmt);
		  int flags = gimple_call_flags (stmt);

		  if (dest)
		    {
		      if (DECL_BUILT_IN_CLASS (dest) == BUILT_IN_NORMAL
			  && DECL_FUNCTION_CODE (dest) == BUILT_IN_APPLY_ARGS)
			encountered_apply_args = true;
		      if (recursive_call_p (current_function_decl, dest))
			{
			  encountered_recursive_call = true;
			  if (!callsite_has_enough_arguments_p (stmt))
			    encountered_unchangable_recursive_call = true;
			}
		    }

		  if (final_bbs
		      && (flags & (ECF_CONST | ECF_PURE)) == 0)
		    bitmap_set_bit (final_bbs, bb->index);
		}

	      t = gimple_call_lhs (stmt);
	      if (t && !disqualify_ops_if_throwing_stmt (stmt, t, NULL))
		ret |= build_access_from_expr (t, stmt, true);
	      break;

	    case GIMPLE_ASM:
	      walk_stmt_load_store_addr_ops (stmt, NULL, NULL, NULL,
					     asm_visit_addr);
	      if (final_bbs)
		bitmap_set_bit (final_bbs, bb->index);

	      for (i = 0; i < gimple_asm_ninputs (stmt); i++)
		{
		  t = TREE_VALUE (gimple_asm_input_op (stmt, i));
		  ret |= build_access_from_expr (t, stmt, false);
		}
	      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
		{
		  t = TREE_VALUE (gimple_asm_output_op (stmt, i));
		  ret |= build_access_from_expr (t, stmt, true);
		}
	      break;

	    default:
	      break;
	    }
	}
    }

  return ret;
}

/* Helper of QSORT function. There are pointers to accesses in the array.  An
   access is considered smaller than another if it has smaller offset or if the
   offsets are the same but is size is bigger. */

static int
compare_access_positions (const void *a, const void *b)
{
  const access_p *fp1 = (const access_p *) a;
  const access_p *fp2 = (const access_p *) b;
  const access_p f1 = *fp1;
  const access_p f2 = *fp2;

  if (f1->offset != f2->offset)
    return f1->offset < f2->offset ? -1 : 1;

  if (f1->size == f2->size)
    {
      if (f1->type == f2->type)
	return 0;
      /* Put any non-aggregate type before any aggregate type.  */
      else if (!is_gimple_reg_type (f1->type)
	  && is_gimple_reg_type (f2->type))
	return 1;
      else if (is_gimple_reg_type (f1->type)
	       && !is_gimple_reg_type (f2->type))
	return -1;
      /* Put any complex or vector type before any other scalar type.  */
      else if (TREE_CODE (f1->type) != COMPLEX_TYPE
	       && TREE_CODE (f1->type) != VECTOR_TYPE
	       && (TREE_CODE (f2->type) == COMPLEX_TYPE
		   || TREE_CODE (f2->type) == VECTOR_TYPE))
	return 1;
      else if ((TREE_CODE (f1->type) == COMPLEX_TYPE
		|| TREE_CODE (f1->type) == VECTOR_TYPE)
	       && TREE_CODE (f2->type) != COMPLEX_TYPE
	       && TREE_CODE (f2->type) != VECTOR_TYPE)
	return -1;
      /* Put the integral type with the bigger precision first.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	       && INTEGRAL_TYPE_P (f2->type))
	return TYPE_PRECISION (f2->type) - TYPE_PRECISION (f1->type);
      /* Put any integral type with non-full precision last.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	       && (TREE_INT_CST_LOW (TYPE_SIZE (f1->type))
		   != TYPE_PRECISION (f1->type)))
	return 1;
      else if (INTEGRAL_TYPE_P (f2->type)
	       && (TREE_INT_CST_LOW (TYPE_SIZE (f2->type))
		   != TYPE_PRECISION (f2->type)))
	return -1;
      /* Stabilize the sort.  */
      return TYPE_UID (f1->type) - TYPE_UID (f2->type);
    }

  /* We want the bigger accesses first, thus the opposite operator in the next
     line: */
  return f1->size > f2->size ? -1 : 1;
}


/* Append a name of the declaration to the name obstack.  A helper function for
   make_fancy_name.  */

static void
make_fancy_decl_name (tree decl)
{
  char buffer[32];

  tree name = DECL_NAME (decl);
  if (name)
    obstack_grow (&name_obstack, IDENTIFIER_POINTER (name),
		  IDENTIFIER_LENGTH (name));
  else
    {
      sprintf (buffer, "D%u", DECL_UID (decl));
      obstack_grow (&name_obstack, buffer, strlen (buffer));
    }
}

/* Helper for make_fancy_name.  */

static void
make_fancy_name_1 (tree expr)
{
  char buffer[32];
  tree index;

  if (DECL_P (expr))
    {
      make_fancy_decl_name (expr);
      return;
    }

  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      obstack_1grow (&name_obstack, '$');
      make_fancy_decl_name (TREE_OPERAND (expr, 1));
      break;

    case ARRAY_REF:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      obstack_1grow (&name_obstack, '$');
      /* Arrays with only one element may not have a constant as their
	 index. */
      index = TREE_OPERAND (expr, 1);
      if (TREE_CODE (index) != INTEGER_CST)
	break;
      sprintf (buffer, HOST_WIDE_INT_PRINT_DEC, TREE_INT_CST_LOW (index));
      obstack_grow (&name_obstack, buffer, strlen (buffer));
      break;

    case ADDR_EXPR:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      break;

    case MEM_REF:
      make_fancy_name_1 (TREE_OPERAND (expr, 0));
      if (!integer_zerop (TREE_OPERAND (expr, 1)))
	{
	  obstack_1grow (&name_obstack, '$');
	  sprintf (buffer, HOST_WIDE_INT_PRINT_DEC,
		   TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)));
	  obstack_grow (&name_obstack, buffer, strlen (buffer));
	}
      break;

    case BIT_FIELD_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      gcc_unreachable (); 	/* we treat these as scalars.  */
      break;
    default:
      break;
    }
}

/* Create a human readable name for replacement variable of ACCESS.  */

static char *
make_fancy_name (tree expr)
{
  make_fancy_name_1 (expr);
  obstack_1grow (&name_obstack, '\0');
  return XOBFINISH (&name_obstack, char *);
}

/* Construct a MEM_REF that would reference a part of aggregate BASE of type
   EXP_TYPE at the given OFFSET.  If BASE is something for which
   get_addr_base_and_unit_offset returns NULL, gsi must be non-NULL and is used
   to insert new statements either before or below the current one as specified
   by INSERT_AFTER.  This function is not capable of handling bitfields.

   BASE must be either a declaration or a memory reference that has correct
   alignment ifformation embeded in it (e.g. a pre-existing one in SRA).  */

tree
build_ref_for_offset (location_t loc, tree base, HOST_WIDE_INT offset,
		      tree exp_type, gimple_stmt_iterator *gsi,
		      bool insert_after)
{
  tree prev_base = base;
  tree off;
  tree mem_ref;
  HOST_WIDE_INT base_offset;
  unsigned HOST_WIDE_INT misalign;
  unsigned int align;

  gcc_checking_assert (offset % BITS_PER_UNIT == 0);
  get_object_alignment_1 (base, &align, &misalign);
  base = get_addr_base_and_unit_offset (base, &base_offset);

  /* get_addr_base_and_unit_offset returns NULL for references with a variable
     offset such as array[var_index].  */
  if (!base)
    {
      gimple stmt;
      tree tmp, addr;

      gcc_checking_assert (gsi);
      tmp = make_ssa_name (build_pointer_type (TREE_TYPE (prev_base)), NULL);
      addr = build_fold_addr_expr (unshare_expr (prev_base));
      STRIP_USELESS_TYPE_CONVERSION (addr);
      stmt = gimple_build_assign (tmp, addr);
      gimple_set_location (stmt, loc);
      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

      off = build_int_cst (reference_alias_ptr_type (prev_base),
			   offset / BITS_PER_UNIT);
      base = tmp;
    }
  else if (TREE_CODE (base) == MEM_REF)
    {
      off = build_int_cst (TREE_TYPE (TREE_OPERAND (base, 1)),
			   base_offset + offset / BITS_PER_UNIT);
      off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1), off);
      base = unshare_expr (TREE_OPERAND (base, 0));
    }
  else
    {
      off = build_int_cst (reference_alias_ptr_type (base),
			   base_offset + offset / BITS_PER_UNIT);
      base = build_fold_addr_expr (unshare_expr (base));
    }

  misalign = (misalign + offset) & (align - 1);
  if (misalign != 0)
    align = (misalign & -misalign);
  if (align < TYPE_ALIGN (exp_type))
    exp_type = build_aligned_type (exp_type, align);

  mem_ref = fold_build2_loc (loc, MEM_REF, exp_type, base, off);
  if (TREE_THIS_VOLATILE (prev_base))
    TREE_THIS_VOLATILE (mem_ref) = 1;
  if (TREE_SIDE_EFFECTS (prev_base))
    TREE_SIDE_EFFECTS (mem_ref) = 1;
  return mem_ref;
}

/* Construct a memory reference to a part of an aggregate BASE at the given
   OFFSET and of the same type as MODEL.  In case this is a reference to a
   bit-field, the function will replicate the last component_ref of model's
   expr to access it.  GSI and INSERT_AFTER have the same meaning as in
   build_ref_for_offset.  */

static tree
build_ref_for_model (location_t loc, tree base, HOST_WIDE_INT offset,
		     struct access *model, gimple_stmt_iterator *gsi,
		     bool insert_after)
{
  if (TREE_CODE (model->expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (model->expr, 1)))
    {
      /* This access represents a bit-field.  */
      tree t, exp_type, fld = TREE_OPERAND (model->expr, 1);

      offset -= int_bit_position (fld);
      exp_type = TREE_TYPE (TREE_OPERAND (model->expr, 0));
      t = build_ref_for_offset (loc, base, offset, exp_type, gsi, insert_after);
      return fold_build3_loc (loc, COMPONENT_REF, TREE_TYPE (fld), t, fld,
			      NULL_TREE);
    }
  else
    return build_ref_for_offset (loc, base, offset, model->type,
				 gsi, insert_after);
}

/* Attempt to build a memory reference that we could but into a gimple
   debug_bind statement.  Similar to build_ref_for_model but punts if it has to
   create statements and return s NULL instead.  This function also ignores
   alignment issues and so its results should never end up in non-debug
   statements.  */

static tree
build_debug_ref_for_model (location_t loc, tree base, HOST_WIDE_INT offset,
			   struct access *model)
{
  HOST_WIDE_INT base_offset;
  tree off;

  if (TREE_CODE (model->expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (model->expr, 1)))
    return NULL_TREE;

  base = get_addr_base_and_unit_offset (base, &base_offset);
  if (!base)
    return NULL_TREE;
  if (TREE_CODE (base) == MEM_REF)
    {
      off = build_int_cst (TREE_TYPE (TREE_OPERAND (base, 1)),
			   base_offset + offset / BITS_PER_UNIT);
      off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1), off);
      base = unshare_expr (TREE_OPERAND (base, 0));
    }
  else
    {
      off = build_int_cst (reference_alias_ptr_type (base),
			   base_offset + offset / BITS_PER_UNIT);
      base = build_fold_addr_expr (unshare_expr (base));
    }

  return fold_build2_loc (loc, MEM_REF, model->type, base, off);
}

/* Construct a memory reference consisting of component_refs and array_refs to
   a part of an aggregate *RES (which is of type TYPE).  The requested part
   should have type EXP_TYPE at be the given OFFSET.  This function might not
   succeed, it returns true when it does and only then *RES points to something
   meaningful.  This function should be used only to build expressions that we
   might need to present to user (e.g. in warnings).  In all other situations,
   build_ref_for_model or build_ref_for_offset should be used instead.  */

static bool
build_user_friendly_ref_for_offset (tree *res, tree type, HOST_WIDE_INT offset,
				    tree exp_type)
{
  while (1)
    {
      tree fld;
      tree tr_size, index, minidx;
      HOST_WIDE_INT el_size;

      if (offset == 0 && exp_type
	  && types_compatible_p (exp_type, type))
	return true;

      switch (TREE_CODE (type))
	{
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	case RECORD_TYPE:
	  for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	    {
	      HOST_WIDE_INT pos, size;
	      tree tr_pos, expr, *expr_ptr;

	      if (TREE_CODE (fld) != FIELD_DECL)
		continue;

	      tr_pos = bit_position (fld);
	      if (!tr_pos || !host_integerp (tr_pos, 1))
		continue;
	      pos = TREE_INT_CST_LOW (tr_pos);
	      gcc_assert (TREE_CODE (type) == RECORD_TYPE || pos == 0);
	      tr_size = DECL_SIZE (fld);
	      if (!tr_size || !host_integerp (tr_size, 1))
		continue;
	      size = TREE_INT_CST_LOW (tr_size);
	      if (size == 0)
		{
		  if (pos != offset)
		    continue;
		}
	      else if (pos > offset || (pos + size) <= offset)
		continue;

	      expr = build3 (COMPONENT_REF, TREE_TYPE (fld), *res, fld,
			     NULL_TREE);
	      expr_ptr = &expr;
	      if (build_user_friendly_ref_for_offset (expr_ptr, TREE_TYPE (fld),
						      offset - pos, exp_type))
		{
		  *res = expr;
		  return true;
		}
	    }
	  return false;

	case ARRAY_TYPE:
	  tr_size = TYPE_SIZE (TREE_TYPE (type));
	  if (!tr_size || !host_integerp (tr_size, 1))
	    return false;
	  el_size = tree_low_cst (tr_size, 1);

	  minidx = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	  if (TREE_CODE (minidx) != INTEGER_CST || el_size == 0)
	    return false;
	  index = build_int_cst (TYPE_DOMAIN (type), offset / el_size);
	  if (!integer_zerop (minidx))
	    index = int_const_binop (PLUS_EXPR, index, minidx);
	  *res = build4 (ARRAY_REF, TREE_TYPE (type), *res, index,
			 NULL_TREE, NULL_TREE);
	  offset = offset % el_size;
	  type = TREE_TYPE (type);
	  break;

	default:
	  if (offset != 0)
	    return false;

	  if (exp_type)
	    return false;
	  else
	    return true;
	}
    }
}

/* Return true iff TYPE is stdarg va_list type.  */

static inline bool
is_va_list_type (tree type)
{
  return TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (va_list_type_node);
}

/* Print message to dump file why a variable was rejected. */

static void
reject (tree var, const char *msg)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Rejected (%d): %s: ", DECL_UID (var), msg);
      print_generic_expr (dump_file, var, 0);
      fprintf (dump_file, "\n");
    }
}

/* Return true if VAR is a candidate for SRA.  */

static bool
maybe_add_sra_candidate (tree var)
{
  tree type = TREE_TYPE (var);
  const char *msg;
  tree_node **slot;

  if (!AGGREGATE_TYPE_P (type)) 
    {
      reject (var, "not aggregate");
      return false;
    }
  if (needs_to_live_in_memory (var))
    {
      reject (var, "needs to live in memory");
      return false;
    }
  if (TREE_THIS_VOLATILE (var))
    {
      reject (var, "is volatile");
      return false;
    }
  if (!COMPLETE_TYPE_P (type))
    {
      reject (var, "has incomplete type");
      return false;
    }
  if (!host_integerp (TYPE_SIZE (type), 1))
    {
      reject (var, "type size not fixed");
      return false;
    }
  if (tree_low_cst (TYPE_SIZE (type), 1) == 0)
    {
      reject (var, "type size is zero");
      return false;
    }
  if (type_internals_preclude_sra_p (type, &msg))
    {
      reject (var, msg);
      return false;
    }
  if (/* Fix for PR 41089.  tree-stdarg.c needs to have va_lists intact but
	 we also want to schedule it rather late.  Thus we ignore it in
	 the early pass. */
      (sra_mode == SRA_MODE_EARLY_INTRA
       && is_va_list_type (type)))
    {
      reject (var, "is va_list");
      return false;
    }

  bitmap_set_bit (candidate_bitmap, DECL_UID (var));
  slot = candidates.find_slot_with_hash (var, DECL_UID (var), INSERT);
  *slot = var;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Candidate (%d): ", DECL_UID (var));
      print_generic_expr (dump_file, var, 0);
      fprintf (dump_file, "\n");
    }

  return true;
}

/* The very first phase of intraprocedural SRA.  It marks in candidate_bitmap
   those with type which is suitable for scalarization.  */

static bool
find_var_candidates (void)
{
  tree var, parm;
  unsigned int i;
  bool ret = false;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = DECL_CHAIN (parm))
    ret |= maybe_add_sra_candidate (parm);

  FOR_EACH_LOCAL_DECL (cfun, i, var)
    {
      if (TREE_CODE (var) != VAR_DECL)
        continue;

      ret |= maybe_add_sra_candidate (var);
    }

  return ret;
}

/* Sort all accesses for the given variable, check for partial overlaps and
   return NULL if there are any.  If there are none, pick a representative for
   each combination of offset and size and create a linked list out of them.
   Return the pointer to the first representative and make sure it is the first
   one in the vector of accesses.  */

static struct access *
sort_and_splice_var_accesses (tree var)
{
  int i, j, access_count;
  struct access *res, **prev_acc_ptr = &res;
  vec<access_p> *access_vec;
  bool first = true;
  HOST_WIDE_INT low = -1, high = 0;

  access_vec = get_base_access_vector (var);
  if (!access_vec)
    return NULL;
  access_count = access_vec->length ();

  /* Sort by <OFFSET, SIZE>.  */
  access_vec->qsort (compare_access_positions);

  i = 0;
  while (i < access_count)
    {
      struct access *access = (*access_vec)[i];
      bool grp_write = access->write;
      bool grp_read = !access->write;
      bool grp_scalar_write = access->write
	&& is_gimple_reg_type (access->type);
      bool grp_scalar_read = !access->write
	&& is_gimple_reg_type (access->type);
      bool grp_assignment_read = access->grp_assignment_read;
      bool grp_assignment_write = access->grp_assignment_write;
      bool multiple_scalar_reads = false;
      bool total_scalarization = access->grp_total_scalarization;
      bool grp_partial_lhs = access->grp_partial_lhs;
      bool first_scalar = is_gimple_reg_type (access->type);
      bool unscalarizable_region = access->grp_unscalarizable_region;

      if (first || access->offset >= high)
	{
	  first = false;
	  low = access->offset;
	  high = access->offset + access->size;
	}
      else if (access->offset > low && access->offset + access->size > high)
	return NULL;
      else
	gcc_assert (access->offset >= low
		    && access->offset + access->size <= high);

      j = i + 1;
      while (j < access_count)
	{
	  struct access *ac2 = (*access_vec)[j];
	  if (ac2->offset != access->offset || ac2->size != access->size)
	    break;
	  if (ac2->write)
	    {
	      grp_write = true;
	      grp_scalar_write = (grp_scalar_write
				  || is_gimple_reg_type (ac2->type));
	    }
	  else
	    {
	      grp_read = true;
	      if (is_gimple_reg_type (ac2->type))
		{
		  if (grp_scalar_read)
		    multiple_scalar_reads = true;
		  else
		    grp_scalar_read = true;
		}
	    }
	  grp_assignment_read |= ac2->grp_assignment_read;
	  grp_assignment_write |= ac2->grp_assignment_write;
	  grp_partial_lhs |= ac2->grp_partial_lhs;
	  unscalarizable_region |= ac2->grp_unscalarizable_region;
	  total_scalarization |= ac2->grp_total_scalarization;
	  relink_to_new_repr (access, ac2);

	  /* If there are both aggregate-type and scalar-type accesses with
	     this combination of size and offset, the comparison function
	     should have put the scalars first.  */
	  gcc_assert (first_scalar || !is_gimple_reg_type (ac2->type));
	  ac2->group_representative = access;
	  j++;
	}

      i = j;

      access->group_representative = access;
      access->grp_write = grp_write;
      access->grp_read = grp_read;
      access->grp_scalar_read = grp_scalar_read;
      access->grp_scalar_write = grp_scalar_write;
      access->grp_assignment_read = grp_assignment_read;
      access->grp_assignment_write = grp_assignment_write;
      access->grp_hint = multiple_scalar_reads || total_scalarization;
      access->grp_total_scalarization = total_scalarization;
      access->grp_partial_lhs = grp_partial_lhs;
      access->grp_unscalarizable_region = unscalarizable_region;
      if (access->first_link)
	add_access_to_work_queue (access);

      *prev_acc_ptr = access;
      prev_acc_ptr = &access->next_grp;
    }

  gcc_assert (res == (*access_vec)[0]);
  return res;
}

/* Create a variable for the given ACCESS which determines the type, name and a
   few other properties.  Return the variable declaration and store it also to
   ACCESS->replacement.  */

static tree
create_access_replacement (struct access *access)
{
  tree repl;

  if (access->grp_to_be_debug_replaced)
    {
      repl = create_tmp_var_raw (access->type, NULL);
      DECL_CONTEXT (repl) = current_function_decl;
    }
  else
    repl = create_tmp_var (access->type, "SR");
  if (TREE_CODE (access->type) == COMPLEX_TYPE
      || TREE_CODE (access->type) == VECTOR_TYPE)
    {
      if (!access->grp_partial_lhs)
	DECL_GIMPLE_REG_P (repl) = 1;
    }
  else if (access->grp_partial_lhs
	   && is_gimple_reg_type (access->type))
    TREE_ADDRESSABLE (repl) = 1;

  DECL_SOURCE_LOCATION (repl) = DECL_SOURCE_LOCATION (access->base);
  DECL_ARTIFICIAL (repl) = 1;
  DECL_IGNORED_P (repl) = DECL_IGNORED_P (access->base);

  if (DECL_NAME (access->base)
      && !DECL_IGNORED_P (access->base)
      && !DECL_ARTIFICIAL (access->base))
    {
      char *pretty_name = make_fancy_name (access->expr);
      tree debug_expr = unshare_expr_without_location (access->expr), d;
      bool fail = false;

      DECL_NAME (repl) = get_identifier (pretty_name);
      obstack_free (&name_obstack, pretty_name);

      /* Get rid of any SSA_NAMEs embedded in debug_expr,
	 as DECL_DEBUG_EXPR isn't considered when looking for still
	 used SSA_NAMEs and thus they could be freed.  All debug info
	 generation cares is whether something is constant or variable
	 and that get_ref_base_and_extent works properly on the
	 expression.  It cannot handle accesses at a non-constant offset
	 though, so just give up in those cases.  */
      for (d = debug_expr;
	   !fail && (handled_component_p (d) || TREE_CODE (d) == MEM_REF);
	   d = TREE_OPERAND (d, 0))
	switch (TREE_CODE (d))
	  {
	  case ARRAY_REF:
	  case ARRAY_RANGE_REF:
	    if (TREE_OPERAND (d, 1)
		&& TREE_CODE (TREE_OPERAND (d, 1)) != INTEGER_CST)
	      fail = true;
	    if (TREE_OPERAND (d, 3)
		&& TREE_CODE (TREE_OPERAND (d, 3)) != INTEGER_CST)
	      fail = true;
	    /* FALLTHRU */
	  case COMPONENT_REF:
	    if (TREE_OPERAND (d, 2)
		&& TREE_CODE (TREE_OPERAND (d, 2)) != INTEGER_CST)
	      fail = true;
	    break;
	  case MEM_REF:
	    if (TREE_CODE (TREE_OPERAND (d, 0)) != ADDR_EXPR)
	      fail = true;
	    else
	      d = TREE_OPERAND (d, 0);
	    break;
	  default:
	    break;
	  }
      if (!fail)
	{
	  SET_DECL_DEBUG_EXPR (repl, debug_expr);
	  DECL_HAS_DEBUG_EXPR_P (repl) = 1;
	}
      if (access->grp_no_warning)
	TREE_NO_WARNING (repl) = 1;
      else
	TREE_NO_WARNING (repl) = TREE_NO_WARNING (access->base);
    }
  else
    TREE_NO_WARNING (repl) = 1;

  if (dump_file)
    {
      if (access->grp_to_be_debug_replaced)
	{
	  fprintf (dump_file, "Created a debug-only replacement for ");
	  print_generic_expr (dump_file, access->base, 0);
	  fprintf (dump_file, " offset: %u, size: %u\n",
		   (unsigned) access->offset, (unsigned) access->size);
	}
      else
	{
	  fprintf (dump_file, "Created a replacement for ");
	  print_generic_expr (dump_file, access->base, 0);
	  fprintf (dump_file, " offset: %u, size: %u: ",
		   (unsigned) access->offset, (unsigned) access->size);
	  print_generic_expr (dump_file, repl, 0);
	  fprintf (dump_file, "\n");
	}
    }
  sra_stats.replacements++;

  return repl;
}

/* Return ACCESS scalar replacement, create it if it does not exist yet.  */

static inline tree
get_access_replacement (struct access *access)
{
  gcc_checking_assert (access->replacement_decl);
  return access->replacement_decl;
}


/* Build a subtree of accesses rooted in *ACCESS, and move the pointer in the
   linked list along the way.  Stop when *ACCESS is NULL or the access pointed
   to it is not "within" the root.  Return false iff some accesses partially
   overlap.  */

static bool
build_access_subtree (struct access **access)
{
  struct access *root = *access, *last_child = NULL;
  HOST_WIDE_INT limit = root->offset + root->size;

  *access = (*access)->next_grp;
  while  (*access && (*access)->offset + (*access)->size <= limit)
    {
      if (!last_child)
	root->first_child = *access;
      else
	last_child->next_sibling = *access;
      last_child = *access;

      if (!build_access_subtree (access))
	return false;
    }

  if (*access && (*access)->offset < limit)
    return false;

  return true;
}

/* Build a tree of access representatives, ACCESS is the pointer to the first
   one, others are linked in a list by the next_grp field.  Return false iff
   some accesses partially overlap.  */

static bool
build_access_trees (struct access *access)
{
  while (access)
    {
      struct access *root = access;

      if (!build_access_subtree (&access))
	return false;
      root->next_grp = access;
    }
  return true;
}

/* Return true if expr contains some ARRAY_REFs into a variable bounded
   array.  */

static bool
expr_with_var_bounded_array_refs_p (tree expr)
{
  while (handled_component_p (expr))
    {
      if (TREE_CODE (expr) == ARRAY_REF
	  && !host_integerp (array_ref_low_bound (expr), 0))
	return true;
      expr = TREE_OPERAND (expr, 0);
    }
  return false;
}

/* Analyze the subtree of accesses rooted in ROOT, scheduling replacements when
   both seeming beneficial and when ALLOW_REPLACEMENTS allows it.  Also set all
   sorts of access flags appropriately along the way, notably always set
   grp_read and grp_assign_read according to MARK_READ and grp_write when
   MARK_WRITE is true.

   Creating a replacement for a scalar access is considered beneficial if its
   grp_hint is set (this means we are either attempting total scalarization or
   there is more than one direct read access) or according to the following
   table:

   Access written to through a scalar type (once or more times)
   |
   |	Written to in an assignment statement
   |	|
   |	|	Access read as scalar _once_
   |	|	|
   |   	|	|	Read in an assignment statement
   |	|	|	|
   |   	|	|	|	Scalarize	Comment
-----------------------------------------------------------------------------
   0	0	0	0			No access for the scalar
   0	0	0	1			No access for the scalar
   0	0	1	0	No		Single read - won't help
   0	0	1	1	No		The same case
   0	1	0	0			No access for the scalar
   0	1	0	1			No access for the scalar
   0	1	1	0	Yes		s = *g; return s.i;
   0	1	1	1       Yes		The same case as above
   1	0	0	0	No		Won't help
   1	0	0	1	Yes		s.i = 1; *g = s;
   1	0	1	0	Yes		s.i = 5; g = s.i;
   1	0	1	1	Yes		The same case as above
   1	1	0	0	No		Won't help.
   1	1	0	1	Yes		s.i = 1; *g = s;
   1	1	1	0	Yes		s = *g; return s.i;
   1	1	1	1	Yes		Any of the above yeses  */

static bool
analyze_access_subtree (struct access *root, struct access *parent,
			bool allow_replacements)
{
  struct access *child;
  HOST_WIDE_INT limit = root->offset + root->size;
  HOST_WIDE_INT covered_to = root->offset;
  bool scalar = is_gimple_reg_type (root->type);
  bool hole = false, sth_created = false;

  if (parent)
    {
      if (parent->grp_read)
	root->grp_read = 1;
      if (parent->grp_assignment_read)
	root->grp_assignment_read = 1;
      if (parent->grp_write)
	root->grp_write = 1;
      if (parent->grp_assignment_write)
	root->grp_assignment_write = 1;
      if (parent->grp_total_scalarization)
	root->grp_total_scalarization = 1;
    }

  if (root->grp_unscalarizable_region)
    allow_replacements = false;

  if (allow_replacements && expr_with_var_bounded_array_refs_p (root->expr))
    allow_replacements = false;

  for (child = root->first_child; child; child = child->next_sibling)
    {
      hole |= covered_to < child->offset;
      sth_created |= analyze_access_subtree (child, root,
					     allow_replacements && !scalar);

      root->grp_unscalarized_data |= child->grp_unscalarized_data;
      root->grp_total_scalarization &= child->grp_total_scalarization;
      if (child->grp_covered)
	covered_to += child->size;
      else
	hole = true;
    }

  if (allow_replacements && scalar && !root->first_child
      && (root->grp_hint
	  || ((root->grp_scalar_read || root->grp_assignment_read)
	      && (root->grp_scalar_write || root->grp_assignment_write))))
    {
      /* Always create access replacements that cover the whole access.
         For integral types this means the precision has to match.
	 Avoid assumptions based on the integral type kind, too.  */
      if (INTEGRAL_TYPE_P (root->type)
	  && (TREE_CODE (root->type) != INTEGER_TYPE
	      || TYPE_PRECISION (root->type) != root->size)
	  /* But leave bitfield accesses alone.  */
	  && (TREE_CODE (root->expr) != COMPONENT_REF
	      || !DECL_BIT_FIELD (TREE_OPERAND (root->expr, 1))))
	{
	  tree rt = root->type;
	  gcc_assert ((root->offset % BITS_PER_UNIT) == 0
		      && (root->size % BITS_PER_UNIT) == 0);
	  root->type = build_nonstandard_integer_type (root->size,
						       TYPE_UNSIGNED (rt));
	  root->expr = build_ref_for_offset (UNKNOWN_LOCATION,
					     root->base, root->offset,
					     root->type, NULL, false);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Changing the type of a replacement for ");
	      print_generic_expr (dump_file, root->base, 0);
	      fprintf (dump_file, " offset: %u, size: %u ",
		       (unsigned) root->offset, (unsigned) root->size);
	      fprintf (dump_file, " to an integer.\n");
	    }
	}

      root->grp_to_be_replaced = 1;
      root->replacement_decl = create_access_replacement (root);
      sth_created = true;
      hole = false;
    }
  else
    {
      if (allow_replacements
	  && scalar && !root->first_child
	  && (root->grp_scalar_write || root->grp_assignment_write)
	  && !bitmap_bit_p (cannot_scalarize_away_bitmap,
			    DECL_UID (root->base)))
	{
	  gcc_checking_assert (!root->grp_scalar_read
			       && !root->grp_assignment_read);
	  sth_created = true;
	  if (MAY_HAVE_DEBUG_STMTS)
	    {
	      root->grp_to_be_debug_replaced = 1;
	      root->replacement_decl = create_access_replacement (root);
	    }
	}

      if (covered_to < limit)
	hole = true;
      if (scalar)
	root->grp_total_scalarization = 0;
    }

  if (!hole || root->grp_total_scalarization)
    root->grp_covered = 1;
  else if (root->grp_write || TREE_CODE (root->base) == PARM_DECL)
    root->grp_unscalarized_data = 1; /* not covered and written to */
  return sth_created;
}

/* Analyze all access trees linked by next_grp by the means of
   analyze_access_subtree.  */
static bool
analyze_access_trees (struct access *access)
{
  bool ret = false;

  while (access)
    {
      if (analyze_access_subtree (access, NULL, true))
	ret = true;
      access = access->next_grp;
    }

  return ret;
}

/* Return true iff a potential new child of LACC at offset OFFSET and with size
   SIZE would conflict with an already existing one.  If exactly such a child
   already exists in LACC, store a pointer to it in EXACT_MATCH.  */

static bool
child_would_conflict_in_lacc (struct access *lacc, HOST_WIDE_INT norm_offset,
			      HOST_WIDE_INT size, struct access **exact_match)
{
  struct access *child;

  for (child = lacc->first_child; child; child = child->next_sibling)
    {
      if (child->offset == norm_offset && child->size == size)
	{
	  *exact_match = child;
	  return true;
	}

      if (child->offset < norm_offset + size
	  && child->offset + child->size > norm_offset)
	return true;
    }

  return false;
}

/* Create a new child access of PARENT, with all properties just like MODEL
   except for its offset and with its grp_write false and grp_read true.
   Return the new access or NULL if it cannot be created.  Note that this access
   is created long after all splicing and sorting, it's not located in any
   access vector and is automatically a representative of its group.  */

static struct access *
create_artificial_child_access (struct access *parent, struct access *model,
				HOST_WIDE_INT new_offset)
{
  struct access *access;
  struct access **child;
  tree expr = parent->base;

  gcc_assert (!model->grp_unscalarizable_region);

  access = (struct access *) pool_alloc (access_pool);
  memset (access, 0, sizeof (struct access));
  if (!build_user_friendly_ref_for_offset (&expr, TREE_TYPE (expr), new_offset,
					   model->type))
    {
      access->grp_no_warning = true;
      expr = build_ref_for_model (EXPR_LOCATION (parent->base), parent->base,
				  new_offset, model, NULL, false);
    }

  access->base = parent->base;
  access->expr = expr;
  access->offset = new_offset;
  access->size = model->size;
  access->type = model->type;
  access->grp_write = true;
  access->grp_read = false;

  child = &parent->first_child;
  while (*child && (*child)->offset < new_offset)
    child = &(*child)->next_sibling;

  access->next_sibling = *child;
  *child = access;

  return access;
}


/* Propagate all subaccesses of RACC across an assignment link to LACC. Return
   true if any new subaccess was created.  Additionally, if RACC is a scalar
   access but LACC is not, change the type of the latter, if possible.  */

static bool
propagate_subaccesses_across_link (struct access *lacc, struct access *racc)
{
  struct access *rchild;
  HOST_WIDE_INT norm_delta = lacc->offset - racc->offset;
  bool ret = false;

  if (is_gimple_reg_type (lacc->type)
      || lacc->grp_unscalarizable_region
      || racc->grp_unscalarizable_region)
    return false;

  if (is_gimple_reg_type (racc->type))
    {
      if (!lacc->first_child && !racc->first_child)
	{
	  tree t = lacc->base;

	  lacc->type = racc->type;
	  if (build_user_friendly_ref_for_offset (&t, TREE_TYPE (t),
						  lacc->offset, racc->type))
	    lacc->expr = t;
	  else
	    {
	      lacc->expr = build_ref_for_model (EXPR_LOCATION (lacc->base),
						lacc->base, lacc->offset,
						racc, NULL, false);
	      lacc->grp_no_warning = true;
	    }
	}
      return false;
    }

  for (rchild = racc->first_child; rchild; rchild = rchild->next_sibling)
    {
      struct access *new_acc = NULL;
      HOST_WIDE_INT norm_offset = rchild->offset + norm_delta;

      if (rchild->grp_unscalarizable_region)
	continue;

      if (child_would_conflict_in_lacc (lacc, norm_offset, rchild->size,
					&new_acc))
	{
	  if (new_acc)
	    {
	      rchild->grp_hint = 1;
	      new_acc->grp_hint |= new_acc->grp_read;
	      if (rchild->first_child)
		ret |= propagate_subaccesses_across_link (new_acc, rchild);
	    }
	  continue;
	}

      rchild->grp_hint = 1;
      new_acc = create_artificial_child_access (lacc, rchild, norm_offset);
      if (new_acc)
	{
	  ret = true;
	  if (racc->first_child)
	    propagate_subaccesses_across_link (new_acc, rchild);
	}
    }

  return ret;
}

/* Propagate all subaccesses across assignment links.  */

static void
propagate_all_subaccesses (void)
{
  while (work_queue_head)
    {
      struct access *racc = pop_access_from_work_queue ();
      struct assign_link *link;

      gcc_assert (racc->first_link);

      for (link = racc->first_link; link; link = link->next)
	{
	  struct access *lacc = link->lacc;

	  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (lacc->base)))
	    continue;
	  lacc = lacc->group_representative;
	  if (propagate_subaccesses_across_link (lacc, racc)
	      && lacc->first_link)
	    add_access_to_work_queue (lacc);
	}
    }
}

/* Go through all accesses collected throughout the (intraprocedural) analysis
   stage, exclude overlapping ones, identify representatives and build trees
   out of them, making decisions about scalarization on the way.  Return true
   iff there are any to-be-scalarized variables after this stage. */

static bool
analyze_all_variable_accesses (void)
{
  int res = 0;
  bitmap tmp = BITMAP_ALLOC (NULL);
  bitmap_iterator bi;
  unsigned i, max_total_scalarization_size;

  max_total_scalarization_size = UNITS_PER_WORD * BITS_PER_UNIT
    * MOVE_RATIO (optimize_function_for_speed_p (cfun));

  EXECUTE_IF_SET_IN_BITMAP (candidate_bitmap, 0, i, bi)
    if (bitmap_bit_p (should_scalarize_away_bitmap, i)
	&& !bitmap_bit_p (cannot_scalarize_away_bitmap, i))
      {
	tree var = candidate (i);

	if (TREE_CODE (var) == VAR_DECL
	    && type_consists_of_records_p (TREE_TYPE (var)))
	  {
	    if ((unsigned) tree_low_cst (TYPE_SIZE (TREE_TYPE (var)), 1)
		<= max_total_scalarization_size)
	      {
		completely_scalarize_var (var);
		if (dump_file && (dump_flags & TDF_DETAILS))
		  {
		    fprintf (dump_file, "Will attempt to totally scalarize ");
		    print_generic_expr (dump_file, var, 0);
		    fprintf (dump_file, " (UID: %u): \n", DECL_UID (var));
		  }
	      }
	    else if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "Too big to totally scalarize: ");
		print_generic_expr (dump_file, var, 0);
		fprintf (dump_file, " (UID: %u)\n", DECL_UID (var));
	      }
	  }
      }

  bitmap_copy (tmp, candidate_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      tree var = candidate (i);
      struct access *access;

      access = sort_and_splice_var_accesses (var);
      if (!access || !build_access_trees (access))
	disqualify_candidate (var,
			      "No or inhibitingly overlapping accesses.");
    }

  propagate_all_subaccesses ();

  bitmap_copy (tmp, candidate_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      tree var = candidate (i);
      struct access *access = get_first_repr_for_decl (var);

      if (analyze_access_trees (access))
	{
	  res++;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\nAccess trees for ");
	      print_generic_expr (dump_file, var, 0);
	      fprintf (dump_file, " (UID: %u): \n", DECL_UID (var));
	      dump_access_tree (dump_file, access);
	      fprintf (dump_file, "\n");
	    }
	}
      else
	disqualify_candidate (var, "No scalar replacements to be created.");
    }

  BITMAP_FREE (tmp);

  if (res)
    {
      statistics_counter_event (cfun, "Scalarized aggregates", res);
      return true;
    }
  else
    return false;
}

/* Generate statements copying scalar replacements of accesses within a subtree
   into or out of AGG.  ACCESS, all its children, siblings and their children
   are to be processed.  AGG is an aggregate type expression (can be a
   declaration but does not have to be, it can for example also be a mem_ref or
   a series of handled components).  TOP_OFFSET is the offset of the processed
   subtree which has to be subtracted from offsets of individual accesses to
   get corresponding offsets for AGG.  If CHUNK_SIZE is non-null, copy only
   replacements in the interval <start_offset, start_offset + chunk_size>,
   otherwise copy all.  GSI is a statement iterator used to place the new
   statements.  WRITE should be true when the statements should write from AGG
   to the replacement and false if vice versa.  if INSERT_AFTER is true, new
   statements will be added after the current statement in GSI, they will be
   added before the statement otherwise.  */

static void
generate_subtree_copies (struct access *access, tree agg,
			 HOST_WIDE_INT top_offset,
			 HOST_WIDE_INT start_offset, HOST_WIDE_INT chunk_size,
			 gimple_stmt_iterator *gsi, bool write,
			 bool insert_after, location_t loc)
{
  do
    {
      if (chunk_size && access->offset >= start_offset + chunk_size)
	return;

      if (access->grp_to_be_replaced
	  && (chunk_size == 0
	      || access->offset + access->size > start_offset))
	{
	  tree expr, repl = get_access_replacement (access);
	  gimple stmt;

	  expr = build_ref_for_model (loc, agg, access->offset - top_offset,
				      access, gsi, insert_after);

	  if (write)
	    {
	      if (access->grp_partial_lhs)
		expr = force_gimple_operand_gsi (gsi, expr, true, NULL_TREE,
						 !insert_after,
						 insert_after ? GSI_NEW_STMT
						 : GSI_SAME_STMT);
	      stmt = gimple_build_assign (repl, expr);
	    }
	  else
	    {
	      TREE_NO_WARNING (repl) = 1;
	      if (access->grp_partial_lhs)
		repl = force_gimple_operand_gsi (gsi, repl, true, NULL_TREE,
						 !insert_after,
						 insert_after ? GSI_NEW_STMT
						 : GSI_SAME_STMT);
	      stmt = gimple_build_assign (expr, repl);
	    }
	  gimple_set_location (stmt, loc);

	  if (insert_after)
	    gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
	  else
	    gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	  update_stmt (stmt);
	  sra_stats.subtree_copies++;
	}
      else if (write
	       && access->grp_to_be_debug_replaced
	       && (chunk_size == 0
		   || access->offset + access->size > start_offset))
	{
	  gimple ds;
	  tree drhs = build_debug_ref_for_model (loc, agg,
						 access->offset - top_offset,
						 access);
	  ds = gimple_build_debug_bind (get_access_replacement (access),
					drhs, gsi_stmt (*gsi));
	  if (insert_after)
	    gsi_insert_after (gsi, ds, GSI_NEW_STMT);
	  else
	    gsi_insert_before (gsi, ds, GSI_SAME_STMT);
	}

      if (access->first_child)
	generate_subtree_copies (access->first_child, agg, top_offset,
				 start_offset, chunk_size, gsi,
				 write, insert_after, loc);

      access = access->next_sibling;
    }
  while (access);
}

/* Assign zero to all scalar replacements in an access subtree.  ACCESS is the
   the root of the subtree to be processed.  GSI is the statement iterator used
   for inserting statements which are added after the current statement if
   INSERT_AFTER is true or before it otherwise.  */

static void
init_subtree_with_zero (struct access *access, gimple_stmt_iterator *gsi,
			bool insert_after, location_t loc)

{
  struct access *child;

  if (access->grp_to_be_replaced)
    {
      gimple stmt;

      stmt = gimple_build_assign (get_access_replacement (access),
				  build_zero_cst (access->type));
      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
      update_stmt (stmt);
      gimple_set_location (stmt, loc);
    }
  else if (access->grp_to_be_debug_replaced)
    {
      gimple ds = gimple_build_debug_bind (get_access_replacement (access),
					   build_zero_cst (access->type),
					   gsi_stmt (*gsi));
      if (insert_after)
	gsi_insert_after (gsi, ds, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, ds, GSI_SAME_STMT);
    }

  for (child = access->first_child; child; child = child->next_sibling)
    init_subtree_with_zero (child, gsi, insert_after, loc);
}

/* Search for an access representative for the given expression EXPR and
   return it or NULL if it cannot be found.  */

static struct access *
get_access_for_expr (tree expr)
{
  HOST_WIDE_INT offset, size, max_size;
  tree base;

  /* FIXME: This should not be necessary but Ada produces V_C_Es with a type of
     a different size than the size of its argument and we need the latter
     one.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    expr = TREE_OPERAND (expr, 0);

  base = get_ref_base_and_extent (expr, &offset, &size, &max_size);
  if (max_size == -1 || !DECL_P (base))
    return NULL;

  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

  return get_var_base_offset_size_access (base, offset, max_size);
}

/* Replace the expression EXPR with a scalar replacement if there is one and
   generate other statements to do type conversion or subtree copying if
   necessary.  GSI is used to place newly created statements, WRITE is true if
   the expression is being written to (it is on a LHS of a statement or output
   in an assembly statement).  */

static bool
sra_modify_expr (tree *expr, gimple_stmt_iterator *gsi, bool write)
{
  location_t loc;
  struct access *access;
  tree type, bfr;

  if (TREE_CODE (*expr) == BIT_FIELD_REF)
    {
      bfr = *expr;
      expr = &TREE_OPERAND (*expr, 0);
    }
  else
    bfr = NULL_TREE;

  if (TREE_CODE (*expr) == REALPART_EXPR || TREE_CODE (*expr) == IMAGPART_EXPR)
    expr = &TREE_OPERAND (*expr, 0);
  access = get_access_for_expr (*expr);
  if (!access)
    return false;
  type = TREE_TYPE (*expr);

  loc = gimple_location (gsi_stmt (*gsi));
  if (access->grp_to_be_replaced)
    {
      tree repl = get_access_replacement (access);
      /* If we replace a non-register typed access simply use the original
         access expression to extract the scalar component afterwards.
	 This happens if scalarizing a function return value or parameter
	 like in gcc.c-torture/execute/20041124-1.c, 20050316-1.c and
	 gcc.c-torture/compile/20011217-1.c.

         We also want to use this when accessing a complex or vector which can
         be accessed as a different type too, potentially creating a need for
         type conversion (see PR42196) and when scalarized unions are involved
         in assembler statements (see PR42398).  */
      if (!useless_type_conversion_p (type, access->type))
	{
	  tree ref;

	  ref = build_ref_for_model (loc, access->base, access->offset, access,
				     NULL, false);

	  if (write)
	    {
	      gimple stmt;

	      if (access->grp_partial_lhs)
		ref = force_gimple_operand_gsi (gsi, ref, true, NULL_TREE,
						 false, GSI_NEW_STMT);
	      stmt = gimple_build_assign (repl, ref);
	      gimple_set_location (stmt, loc);
	      gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
	    }
	  else
	    {
	      gimple stmt;

	      if (access->grp_partial_lhs)
		repl = force_gimple_operand_gsi (gsi, repl, true, NULL_TREE,
						 true, GSI_SAME_STMT);
	      stmt = gimple_build_assign (ref, repl);
	      gimple_set_location (stmt, loc);
	      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	    }
	}
      else
	*expr = repl;
      sra_stats.exprs++;
    }
  else if (write && access->grp_to_be_debug_replaced)
    {
      gimple ds = gimple_build_debug_bind (get_access_replacement (access),
					   NULL_TREE,
					   gsi_stmt (*gsi));
      gsi_insert_after (gsi, ds, GSI_NEW_STMT);
    }

  if (access->first_child)
    {
      HOST_WIDE_INT start_offset, chunk_size;
      if (bfr
	  && host_integerp (TREE_OPERAND (bfr, 1), 1)
	  && host_integerp (TREE_OPERAND (bfr, 2), 1))
	{
	  chunk_size = tree_low_cst (TREE_OPERAND (bfr, 1), 1);
	  start_offset = access->offset
	    + tree_low_cst (TREE_OPERAND (bfr, 2), 1);
	}
      else
	start_offset = chunk_size = 0;

      generate_subtree_copies (access->first_child, access->base, 0,
			       start_offset, chunk_size, gsi, write, write,
			       loc);
    }
  return true;
}

/* Where scalar replacements of the RHS have been written to when a replacement
   of a LHS of an assigments cannot be direclty loaded from a replacement of
   the RHS. */
enum unscalarized_data_handling { SRA_UDH_NONE,  /* Nothing done so far. */
				  SRA_UDH_RIGHT, /* Data flushed to the RHS. */
				  SRA_UDH_LEFT }; /* Data flushed to the LHS. */

/* Store all replacements in the access tree rooted in TOP_RACC either to their
   base aggregate if there are unscalarized data or directly to LHS of the
   statement that is pointed to by GSI otherwise.  */

static enum unscalarized_data_handling
handle_unscalarized_data_in_subtree (struct access *top_racc,
				     gimple_stmt_iterator *gsi)
{
  if (top_racc->grp_unscalarized_data)
    {
      generate_subtree_copies (top_racc->first_child, top_racc->base, 0, 0, 0,
			       gsi, false, false,
			       gimple_location (gsi_stmt (*gsi)));
      return SRA_UDH_RIGHT;
    }
  else
    {
      tree lhs = gimple_assign_lhs (gsi_stmt (*gsi));
      generate_subtree_copies (top_racc->first_child, lhs, top_racc->offset,
			       0, 0, gsi, false, false,
			       gimple_location (gsi_stmt (*gsi)));
      return SRA_UDH_LEFT;
    }
}


/* Try to generate statements to load all sub-replacements in an access subtree
   formed by children of LACC from scalar replacements in the TOP_RACC subtree.
   If that is not possible, refresh the TOP_RACC base aggregate and load the
   accesses from it.  LEFT_OFFSET is the offset of the left whole subtree being
   copied. NEW_GSI is stmt iterator used for statement insertions after the
   original assignment, OLD_GSI is used to insert statements before the
   assignment.  *REFRESHED keeps the information whether we have needed to
   refresh replacements of the LHS and from which side of the assignments this
   takes place.  */

static void
load_assign_lhs_subreplacements (struct access *lacc, struct access *top_racc,
				 HOST_WIDE_INT left_offset,
				 gimple_stmt_iterator *old_gsi,
				 gimple_stmt_iterator *new_gsi,
				 enum unscalarized_data_handling *refreshed)
{
  location_t loc = gimple_location (gsi_stmt (*old_gsi));
  for (lacc = lacc->first_child; lacc; lacc = lacc->next_sibling)
    {
      HOST_WIDE_INT offset = lacc->offset - left_offset + top_racc->offset;

      if (lacc->grp_to_be_replaced)
	{
	  struct access *racc;
	  gimple stmt;
	  tree rhs;

	  racc = find_access_in_subtree (top_racc, offset, lacc->size);
	  if (racc && racc->grp_to_be_replaced)
	    {
	      rhs = get_access_replacement (racc);
	      if (!useless_type_conversion_p (lacc->type, racc->type))
		rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, lacc->type, rhs);

	      if (racc->grp_partial_lhs && lacc->grp_partial_lhs)
		rhs = force_gimple_operand_gsi (old_gsi, rhs, true, NULL_TREE,
						true, GSI_SAME_STMT);
	    }
	  else
	    {
	      /* No suitable access on the right hand side, need to load from
		 the aggregate.  See if we have to update it first... */
	      if (*refreshed == SRA_UDH_NONE)
		*refreshed = handle_unscalarized_data_in_subtree (top_racc,
								  old_gsi);

	      if (*refreshed == SRA_UDH_LEFT)
		rhs = build_ref_for_model (loc, lacc->base, lacc->offset, lacc,
					    new_gsi, true);
	      else
		rhs = build_ref_for_model (loc, top_racc->base, offset, lacc,
					    new_gsi, true);
	      if (lacc->grp_partial_lhs)
		rhs = force_gimple_operand_gsi (new_gsi, rhs, true, NULL_TREE,
						false, GSI_NEW_STMT);
	    }

	  stmt = gimple_build_assign (get_access_replacement (lacc), rhs);
	  gsi_insert_after (new_gsi, stmt, GSI_NEW_STMT);
	  gimple_set_location (stmt, loc);
	  update_stmt (stmt);
	  sra_stats.subreplacements++;
	}
      else
	{
	  if (*refreshed == SRA_UDH_NONE
	      && lacc->grp_read && !lacc->grp_covered)
	    *refreshed = handle_unscalarized_data_in_subtree (top_racc,
							      old_gsi);
	  if (lacc && lacc->grp_to_be_debug_replaced)
	    {
	      gimple ds;
	      tree drhs;
	      struct access *racc = find_access_in_subtree (top_racc, offset,
							    lacc->size);

	      if (racc && racc->grp_to_be_replaced)
		{
		  if (racc->grp_write)
		    drhs = get_access_replacement (racc);
		  else
		    drhs = NULL;
		}
	      else if (*refreshed == SRA_UDH_LEFT)
		drhs = build_debug_ref_for_model (loc, lacc->base, lacc->offset,
						  lacc);
	      else if (*refreshed == SRA_UDH_RIGHT)
		drhs = build_debug_ref_for_model (loc, top_racc->base, offset,
						  lacc);
	      else
		drhs = NULL_TREE;
	      ds = gimple_build_debug_bind (get_access_replacement (lacc),
					    drhs, gsi_stmt (*old_gsi));
	      gsi_insert_after (new_gsi, ds, GSI_NEW_STMT);
	    }
	}

      if (lacc->first_child)
	load_assign_lhs_subreplacements (lacc, top_racc, left_offset,
					 old_gsi, new_gsi, refreshed);
    }
}

/* Result code for SRA assignment modification.  */
enum assignment_mod_result { SRA_AM_NONE,       /* nothing done for the stmt */
			     SRA_AM_MODIFIED,  /* stmt changed but not
						  removed */
			     SRA_AM_REMOVED };  /* stmt eliminated */

/* Modify assignments with a CONSTRUCTOR on their RHS.  STMT contains a pointer
   to the assignment and GSI is the statement iterator pointing at it.  Returns
   the same values as sra_modify_assign.  */

static enum assignment_mod_result
sra_modify_constructor_assign (gimple *stmt, gimple_stmt_iterator *gsi)
{
  tree lhs = gimple_assign_lhs (*stmt);
  struct access *acc;
  location_t loc;

  acc = get_access_for_expr (lhs);
  if (!acc)
    return SRA_AM_NONE;

  if (gimple_clobber_p (*stmt))
    {
      /* Remove clobbers of fully scalarized variables, otherwise
	 do nothing.  */
      if (acc->grp_covered)
	{
	  unlink_stmt_vdef (*stmt);
	  gsi_remove (gsi, true);
	  release_defs (*stmt);
	  return SRA_AM_REMOVED;
	}
      else
	return SRA_AM_NONE;
    }

  loc = gimple_location (*stmt);
  if (vec_safe_length (CONSTRUCTOR_ELTS (gimple_assign_rhs1 (*stmt))) > 0)
    {
      /* I have never seen this code path trigger but if it can happen the
	 following should handle it gracefully.  */
      if (access_has_children_p (acc))
	generate_subtree_copies (acc->first_child, acc->base, 0, 0, 0, gsi,
				 true, true, loc);
      return SRA_AM_MODIFIED;
    }

  if (acc->grp_covered)
    {
      init_subtree_with_zero (acc, gsi, false, loc);
      unlink_stmt_vdef (*stmt);
      gsi_remove (gsi, true);
      release_defs (*stmt);
      return SRA_AM_REMOVED;
    }
  else
    {
      init_subtree_with_zero (acc, gsi, true, loc);
      return SRA_AM_MODIFIED;
    }
}

/* Create and return a new suitable default definition SSA_NAME for RACC which
   is an access describing an uninitialized part of an aggregate that is being
   loaded.  */

static tree
get_repl_default_def_ssa_name (struct access *racc)
{
  gcc_checking_assert (!racc->grp_to_be_replaced
		       && !racc->grp_to_be_debug_replaced);
  if (!racc->replacement_decl)
    racc->replacement_decl = create_access_replacement (racc);
  return get_or_create_ssa_default_def (cfun, racc->replacement_decl);
}

/* Return true if REF has an VIEW_CONVERT_EXPR or a COMPONENT_REF with a
   bit-field field declaration somewhere in it.  */

static inline bool
contains_vce_or_bfcref_p (const_tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == VIEW_CONVERT_EXPR
	  || (TREE_CODE (ref) == COMPONENT_REF
	      && DECL_BIT_FIELD (TREE_OPERAND (ref, 1))))
	return true;
      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Examine both sides of the assignment statement pointed to by STMT, replace
   them with a scalare replacement if there is one and generate copying of
   replacements if scalarized aggregates have been used in the assignment.  GSI
   is used to hold generated statements for type conversions and subtree
   copying.  */

static enum assignment_mod_result
sra_modify_assign (gimple *stmt, gimple_stmt_iterator *gsi)
{
  struct access *lacc, *racc;
  tree lhs, rhs;
  bool modify_this_stmt = false;
  bool force_gimple_rhs = false;
  location_t loc;
  gimple_stmt_iterator orig_gsi = *gsi;

  if (!gimple_assign_single_p (*stmt))
    return SRA_AM_NONE;
  lhs = gimple_assign_lhs (*stmt);
  rhs = gimple_assign_rhs1 (*stmt);

  if (TREE_CODE (rhs) == CONSTRUCTOR)
    return sra_modify_constructor_assign (stmt, gsi);

  if (TREE_CODE (rhs) == REALPART_EXPR || TREE_CODE (lhs) == REALPART_EXPR
      || TREE_CODE (rhs) == IMAGPART_EXPR || TREE_CODE (lhs) == IMAGPART_EXPR
      || TREE_CODE (rhs) == BIT_FIELD_REF || TREE_CODE (lhs) == BIT_FIELD_REF)
    {
      modify_this_stmt = sra_modify_expr (gimple_assign_rhs1_ptr (*stmt),
					  gsi, false);
      modify_this_stmt |= sra_modify_expr (gimple_assign_lhs_ptr (*stmt),
					   gsi, true);
      return modify_this_stmt ? SRA_AM_MODIFIED : SRA_AM_NONE;
    }

  lacc = get_access_for_expr (lhs);
  racc = get_access_for_expr (rhs);
  if (!lacc && !racc)
    return SRA_AM_NONE;

  loc = gimple_location (*stmt);
  if (lacc && lacc->grp_to_be_replaced)
    {
      lhs = get_access_replacement (lacc);
      gimple_assign_set_lhs (*stmt, lhs);
      modify_this_stmt = true;
      if (lacc->grp_partial_lhs)
	force_gimple_rhs = true;
      sra_stats.exprs++;
    }

  if (racc && racc->grp_to_be_replaced)
    {
      rhs = get_access_replacement (racc);
      modify_this_stmt = true;
      if (racc->grp_partial_lhs)
	force_gimple_rhs = true;
      sra_stats.exprs++;
    }
  else if (racc
	   && !racc->grp_unscalarized_data
	   && TREE_CODE (lhs) == SSA_NAME
	   && !access_has_replacements_p (racc))
    {
      rhs = get_repl_default_def_ssa_name (racc);
      modify_this_stmt = true;
      sra_stats.exprs++;
    }

  if (modify_this_stmt)
    {
      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	{
	  /* If we can avoid creating a VIEW_CONVERT_EXPR do so.
	     ???  This should move to fold_stmt which we simply should
	     call after building a VIEW_CONVERT_EXPR here.  */
	  if (AGGREGATE_TYPE_P (TREE_TYPE (lhs))
	      && !contains_bitfld_component_ref_p (lhs))
	    {
	      lhs = build_ref_for_model (loc, lhs, 0, racc, gsi, false);
	      gimple_assign_set_lhs (*stmt, lhs);
	    }
	  else if (AGGREGATE_TYPE_P (TREE_TYPE (rhs))
		   && !contains_vce_or_bfcref_p (rhs))
	    rhs = build_ref_for_model (loc, rhs, 0, lacc, gsi, false);

	  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    {
	      rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (lhs),
				     rhs);
	      if (is_gimple_reg_type (TREE_TYPE (lhs))
		  && TREE_CODE (lhs) != SSA_NAME)
		force_gimple_rhs = true;
	    }
	}
    }

  if (lacc && lacc->grp_to_be_debug_replaced)
    {
      tree dlhs = get_access_replacement (lacc);
      tree drhs = unshare_expr (rhs);
      if (!useless_type_conversion_p (TREE_TYPE (dlhs), TREE_TYPE (drhs)))
	{
	  if (AGGREGATE_TYPE_P (TREE_TYPE (drhs))
	      && !contains_vce_or_bfcref_p (drhs))
	    drhs = build_debug_ref_for_model (loc, drhs, 0, lacc);
	  if (drhs
	      && !useless_type_conversion_p (TREE_TYPE (dlhs),
					     TREE_TYPE (drhs)))
	    drhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR,
				    TREE_TYPE (dlhs), drhs);
	}
      gimple ds = gimple_build_debug_bind (dlhs, drhs, *stmt);
      gsi_insert_before (gsi, ds, GSI_SAME_STMT);
    }

  /* From this point on, the function deals with assignments in between
     aggregates when at least one has scalar reductions of some of its
     components.  There are three possible scenarios: Both the LHS and RHS have
     to-be-scalarized components, 2) only the RHS has or 3) only the LHS has.

     In the first case, we would like to load the LHS components from RHS
     components whenever possible.  If that is not possible, we would like to
     read it directly from the RHS (after updating it by storing in it its own
     components).  If there are some necessary unscalarized data in the LHS,
     those will be loaded by the original assignment too.  If neither of these
     cases happen, the original statement can be removed.  Most of this is done
     by load_assign_lhs_subreplacements.

     In the second case, we would like to store all RHS scalarized components
     directly into LHS and if they cover the aggregate completely, remove the
     statement too.  In the third case, we want the LHS components to be loaded
     directly from the RHS (DSE will remove the original statement if it
     becomes redundant).

     This is a bit complex but manageable when types match and when unions do
     not cause confusion in a way that we cannot really load a component of LHS
     from the RHS or vice versa (the access representing this level can have
     subaccesses that are accessible only through a different union field at a
     higher level - different from the one used in the examined expression).
     Unions are fun.

     Therefore, I specially handle a fourth case, happening when there is a
     specific type cast or it is impossible to locate a scalarized subaccess on
     the other side of the expression.  If that happens, I simply "refresh" the
     RHS by storing in it is scalarized components leave the original statement
     there to do the copying and then load the scalar replacements of the LHS.
     This is what the first branch does.  */

  if (modify_this_stmt
      || gimple_has_volatile_ops (*stmt)
      || contains_vce_or_bfcref_p (rhs)
      || contains_vce_or_bfcref_p (lhs))
    {
      if (access_has_children_p (racc))
	generate_subtree_copies (racc->first_child, racc->base, 0, 0, 0,
				 gsi, false, false, loc);
      if (access_has_children_p (lacc))
	generate_subtree_copies (lacc->first_child, lacc->base, 0, 0, 0,
				 gsi, true, true, loc);
      sra_stats.separate_lhs_rhs_handling++;

      /* This gimplification must be done after generate_subtree_copies,
	 lest we insert the subtree copies in the middle of the gimplified
	 sequence.  */
      if (force_gimple_rhs)
	rhs = force_gimple_operand_gsi (&orig_gsi, rhs, true, NULL_TREE,
					true, GSI_SAME_STMT);
      if (gimple_assign_rhs1 (*stmt) != rhs)
	{
	  modify_this_stmt = true;
	  gimple_assign_set_rhs_from_tree (&orig_gsi, rhs);
	  gcc_assert (*stmt == gsi_stmt (orig_gsi));
	}

      return modify_this_stmt ? SRA_AM_MODIFIED : SRA_AM_NONE;
    }
  else
    {
      if (access_has_children_p (lacc)
	  && access_has_children_p (racc)
	  /* When an access represents an unscalarizable region, it usually
	     represents accesses with variable offset and thus must not be used
	     to generate new memory accesses.  */
	  && !lacc->grp_unscalarizable_region
	  && !racc->grp_unscalarizable_region)
	{
	  gimple_stmt_iterator orig_gsi = *gsi;
	  enum unscalarized_data_handling refreshed;

	  if (lacc->grp_read && !lacc->grp_covered)
	    refreshed = handle_unscalarized_data_in_subtree (racc, gsi);
	  else
	    refreshed = SRA_UDH_NONE;

	  load_assign_lhs_subreplacements (lacc, racc, lacc->offset,
					   &orig_gsi, gsi, &refreshed);
	  if (refreshed != SRA_UDH_RIGHT)
	    {
	      gsi_next (gsi);
	      unlink_stmt_vdef (*stmt);
	      gsi_remove (&orig_gsi, true);
	      release_defs (*stmt);
	      sra_stats.deleted++;
	      return SRA_AM_REMOVED;
	    }
	}
      else
	{
	  if (access_has_children_p (racc)
	      && !racc->grp_unscalarized_data)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Removing load: ");
		  print_gimple_stmt (dump_file, *stmt, 0, 0);
		}
	      generate_subtree_copies (racc->first_child, lhs,
				       racc->offset, 0, 0, gsi,
				       false, false, loc);
	      gcc_assert (*stmt == gsi_stmt (*gsi));
	      unlink_stmt_vdef (*stmt);
	      gsi_remove (gsi, true);
	      release_defs (*stmt);
	      sra_stats.deleted++;
	      return SRA_AM_REMOVED;
	    }
	  /* Restore the aggregate RHS from its components so the
	     prevailing aggregate copy does the right thing.  */
	  if (access_has_children_p (racc))
	    generate_subtree_copies (racc->first_child, racc->base, 0, 0, 0,
				     gsi, false, false, loc);
	  /* Re-load the components of the aggregate copy destination.
	     But use the RHS aggregate to load from to expose more
	     optimization opportunities.  */
	  if (access_has_children_p (lacc))
	    generate_subtree_copies (lacc->first_child, rhs, lacc->offset,
				     0, 0, gsi, true, true, loc);
	}

      return SRA_AM_NONE;
    }
}

/* Traverse the function body and all modifications as decided in
   analyze_all_variable_accesses.  Return true iff the CFG has been
   changed.  */

static bool
sra_modify_function_body (void)
{
  bool cfg_changed = false;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (bb);
      while (!gsi_end_p (gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  enum assignment_mod_result assign_result;
	  bool modified = false, deleted = false;
	  tree *t;
	  unsigned i;

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval_ptr (stmt);
	      if (*t != NULL_TREE)
		modified |= sra_modify_expr (t, &gsi, false);
	      break;

	    case GIMPLE_ASSIGN:
	      assign_result = sra_modify_assign (&stmt, &gsi);
	      modified |= assign_result == SRA_AM_MODIFIED;
	      deleted = assign_result == SRA_AM_REMOVED;
	      break;

	    case GIMPLE_CALL:
	      /* Operands must be processed before the lhs.  */
	      for (i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  t = gimple_call_arg_ptr (stmt, i);
		  modified |= sra_modify_expr (t, &gsi, false);
		}

	      if (gimple_call_lhs (stmt))
		{
		  t = gimple_call_lhs_ptr (stmt);
		  modified |= sra_modify_expr (t, &gsi, true);
		}
	      break;

	    case GIMPLE_ASM:
	      for (i = 0; i < gimple_asm_ninputs (stmt); i++)
		{
		  t = &TREE_VALUE (gimple_asm_input_op (stmt, i));
		  modified |= sra_modify_expr (t, &gsi, false);
		}
	      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
		{
		  t = &TREE_VALUE (gimple_asm_output_op (stmt, i));
		  modified |= sra_modify_expr (t, &gsi, true);
		}
	      break;

	    default:
	      break;
	    }

	  if (modified)
	    {
	      update_stmt (stmt);
	      if (maybe_clean_eh_stmt (stmt)
		  && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
		cfg_changed = true;
	    }
	  if (!deleted)
	    gsi_next (&gsi);
	}
    }

  return cfg_changed;
}

/* Generate statements initializing scalar replacements of parts of function
   parameters.  */

static void
initialize_parameter_reductions (void)
{
  gimple_stmt_iterator gsi;
  gimple_seq seq = NULL;
  tree parm;

  gsi = gsi_start (seq);
  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = DECL_CHAIN (parm))
    {
      vec<access_p> *access_vec;
      struct access *access;

      if (!bitmap_bit_p (candidate_bitmap, DECL_UID (parm)))
	continue;
      access_vec = get_base_access_vector (parm);
      if (!access_vec)
	continue;

      for (access = (*access_vec)[0];
	   access;
	   access = access->next_grp)
	generate_subtree_copies (access, parm, 0, 0, 0, &gsi, true, true,
				 EXPR_LOCATION (parm));
    }

  seq = gsi_seq (gsi);
  if (seq)
    gsi_insert_seq_on_edge_immediate (single_succ_edge (ENTRY_BLOCK_PTR), seq);
}

/* The "main" function of intraprocedural SRA passes.  Runs the analysis and if
   it reveals there are components of some aggregates to be scalarized, it runs
   the required transformations.  */
static unsigned int
perform_intra_sra (void)
{
  int ret = 0;
  sra_initialize ();

  if (!find_var_candidates ())
    goto out;

  if (!scan_function ())
    goto out;

  if (!analyze_all_variable_accesses ())
    goto out;

  if (sra_modify_function_body ())
    ret = TODO_update_ssa | TODO_cleanup_cfg;
  else
    ret = TODO_update_ssa;
  initialize_parameter_reductions ();

  statistics_counter_event (cfun, "Scalar replacements created",
			    sra_stats.replacements);
  statistics_counter_event (cfun, "Modified expressions", sra_stats.exprs);
  statistics_counter_event (cfun, "Subtree copy stmts",
			    sra_stats.subtree_copies);
  statistics_counter_event (cfun, "Subreplacement stmts",
			    sra_stats.subreplacements);
  statistics_counter_event (cfun, "Deleted stmts", sra_stats.deleted);
  statistics_counter_event (cfun, "Separate LHS and RHS handling",
			    sra_stats.separate_lhs_rhs_handling);

 out:
  sra_deinitialize ();
  return ret;
}

/* Perform early intraprocedural SRA.  */
static unsigned int
early_intra_sra (void)
{
  sra_mode = SRA_MODE_EARLY_INTRA;
  return perform_intra_sra ();
}

/* Perform "late" intraprocedural SRA.  */
static unsigned int
late_intra_sra (void)
{
  sra_mode = SRA_MODE_INTRA;
  return perform_intra_sra ();
}


static bool
gate_intra_sra (void)
{
  return flag_tree_sra != 0 && dbg_cnt (tree_sra);
}


namespace {

const pass_data pass_data_sra_early =
{
  GIMPLE_PASS, /* type */
  "esra", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_SRA, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_update_ssa | TODO_verify_ssa ), /* todo_flags_finish */
};

class pass_sra_early : public gimple_opt_pass
{
public:
  pass_sra_early (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sra_early, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_intra_sra (); }
  unsigned int execute () { return early_intra_sra (); }

}; // class pass_sra_early

} // anon namespace

gimple_opt_pass *
make_pass_sra_early (gcc::context *ctxt)
{
  return new pass_sra_early (ctxt);
}

namespace {

const pass_data pass_data_sra =
{
  GIMPLE_PASS, /* type */
  "sra", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_SRA, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  TODO_update_address_taken, /* todo_flags_start */
  ( TODO_update_ssa | TODO_verify_ssa ), /* todo_flags_finish */
};

class pass_sra : public gimple_opt_pass
{
public:
  pass_sra (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sra, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_intra_sra (); }
  unsigned int execute () { return late_intra_sra (); }

}; // class pass_sra

} // anon namespace

gimple_opt_pass *
make_pass_sra (gcc::context *ctxt)
{
  return new pass_sra (ctxt);
}


/* Return true iff PARM (which must be a parm_decl) is an unused scalar
   parameter.  */

static bool
is_unused_scalar_param (tree parm)
{
  tree name;
  return (is_gimple_reg (parm)
	  && (!(name = ssa_default_def (cfun, parm))
	      || has_zero_uses (name)));
}

/* Scan immediate uses of a default definition SSA name of a parameter PARM and
   examine whether there are any direct or otherwise infeasible ones.  If so,
   return true, otherwise return false.  PARM must be a gimple register with a
   non-NULL default definition.  */

static bool
ptr_parm_has_direct_uses (tree parm)
{
  imm_use_iterator ui;
  gimple stmt;
  tree name = ssa_default_def (cfun, parm);
  bool ret = false;

  FOR_EACH_IMM_USE_STMT (stmt, ui, name)
    {
      int uses_ok = 0;
      use_operand_p use_p;

      if (is_gimple_debug (stmt))
	continue;

      /* Valid uses include dereferences on the lhs and the rhs.  */
      if (gimple_has_lhs (stmt))
	{
	  tree lhs = gimple_get_lhs (stmt);
	  while (handled_component_p (lhs))
	    lhs = TREE_OPERAND (lhs, 0);
	  if (TREE_CODE (lhs) == MEM_REF
	      && TREE_OPERAND (lhs, 0) == name
	      && integer_zerop (TREE_OPERAND (lhs, 1))
	      && types_compatible_p (TREE_TYPE (lhs),
				     TREE_TYPE (TREE_TYPE (name)))
	      && !TREE_THIS_VOLATILE (lhs))
	    uses_ok++;
	}
      if (gimple_assign_single_p (stmt))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  while (handled_component_p (rhs))
	    rhs = TREE_OPERAND (rhs, 0);
	  if (TREE_CODE (rhs) == MEM_REF
	      && TREE_OPERAND (rhs, 0) == name
	      && integer_zerop (TREE_OPERAND (rhs, 1))
	      && types_compatible_p (TREE_TYPE (rhs),
				     TREE_TYPE (TREE_TYPE (name)))
	      && !TREE_THIS_VOLATILE (rhs))
	    uses_ok++;
	}
      else if (is_gimple_call (stmt))
	{
	  unsigned i;
	  for (i = 0; i < gimple_call_num_args (stmt); ++i)
	    {
	      tree arg = gimple_call_arg (stmt, i);
	      while (handled_component_p (arg))
		arg = TREE_OPERAND (arg, 0);
	      if (TREE_CODE (arg) == MEM_REF
		  && TREE_OPERAND (arg, 0) == name
		  && integer_zerop (TREE_OPERAND (arg, 1))
		  && types_compatible_p (TREE_TYPE (arg),
					 TREE_TYPE (TREE_TYPE (name)))
		  && !TREE_THIS_VOLATILE (arg))
		uses_ok++;
	    }
	}

      /* If the number of valid uses does not match the number of
         uses in this stmt there is an unhandled use.  */
      FOR_EACH_IMM_USE_ON_STMT (use_p, ui)
	--uses_ok;

      if (uses_ok != 0)
	ret = true;

      if (ret)
	BREAK_FROM_IMM_USE_STMT (ui);
    }

  return ret;
}

/* Identify candidates for reduction for IPA-SRA based on their type and mark
   them in candidate_bitmap.  Note that these do not necessarily include
   parameter which are unused and thus can be removed.  Return true iff any
   such candidate has been found.  */

static bool
find_param_candidates (void)
{
  tree parm;
  int count = 0;
  bool ret = false;
  const char *msg;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = DECL_CHAIN (parm))
    {
      tree type = TREE_TYPE (parm);
      tree_node **slot;

      count++;

      if (TREE_THIS_VOLATILE (parm)
	  || TREE_ADDRESSABLE (parm)
	  || (!is_gimple_reg_type (type) && is_va_list_type (type)))
	continue;

      if (is_unused_scalar_param (parm))
	{
	  ret = true;
	  continue;
	}

      if (POINTER_TYPE_P (type))
	{
	  type = TREE_TYPE (type);

	  if (TREE_CODE (type) == FUNCTION_TYPE
	      || TYPE_VOLATILE (type)
	      || (TREE_CODE (type) == ARRAY_TYPE
		  && TYPE_NONALIASED_COMPONENT (type))
	      || !is_gimple_reg (parm)
	      || is_va_list_type (type)
	      || ptr_parm_has_direct_uses (parm))
	    continue;
	}
      else if (!AGGREGATE_TYPE_P (type))
	continue;

      if (!COMPLETE_TYPE_P (type)
	  || !host_integerp (TYPE_SIZE (type), 1)
          || tree_low_cst (TYPE_SIZE (type), 1) == 0
	  || (AGGREGATE_TYPE_P (type)
	      && type_internals_preclude_sra_p (type, &msg)))
	continue;

      bitmap_set_bit (candidate_bitmap, DECL_UID (parm));
      slot = candidates.find_slot_with_hash (parm, DECL_UID (parm), INSERT);
      *slot = parm;

      ret = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Candidate (%d): ", DECL_UID (parm));
	  print_generic_expr (dump_file, parm, 0);
	  fprintf (dump_file, "\n");
	}
    }

  func_param_count = count;
  return ret;
}

/* Callback of walk_aliased_vdefs, marks the access passed as DATA as
   maybe_modified. */

static bool
mark_maybe_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef ATTRIBUTE_UNUSED,
		     void *data)
{
  struct access *repr = (struct access *) data;

  repr->grp_maybe_modified = 1;
  return true;
}

/* Analyze what representatives (in linked lists accessible from
   REPRESENTATIVES) can be modified by side effects of statements in the
   current function.  */

static void
analyze_modified_params (vec<access_p> representatives)
{
  int i;

  for (i = 0; i < func_param_count; i++)
    {
      struct access *repr;

      for (repr = representatives[i];
	   repr;
	   repr = repr->next_grp)
	{
	  struct access *access;
	  bitmap visited;
	  ao_ref ar;

	  if (no_accesses_p (repr))
	    continue;
	  if (!POINTER_TYPE_P (TREE_TYPE (repr->base))
	      || repr->grp_maybe_modified)
	    continue;

	  ao_ref_init (&ar, repr->expr);
	  visited = BITMAP_ALLOC (NULL);
	  for (access = repr; access; access = access->next_sibling)
	    {
	      /* All accesses are read ones, otherwise grp_maybe_modified would
		 be trivially set.  */
	      walk_aliased_vdefs (&ar, gimple_vuse (access->stmt),
				  mark_maybe_modified, repr, &visited);
	      if (repr->grp_maybe_modified)
		break;
	    }
	  BITMAP_FREE (visited);
	}
    }
}

/* Propagate distances in bb_dereferences in the opposite direction than the
   control flow edges, in each step storing the maximum of the current value
   and the minimum of all successors.  These steps are repeated until the table
   stabilizes.  Note that BBs which might terminate the functions (according to
   final_bbs bitmap) never updated in this way.  */

static void
propagate_dereference_distances (void)
{
  vec<basic_block> queue;
  basic_block bb;

  queue.create (last_basic_block_for_function (cfun));
  queue.quick_push (ENTRY_BLOCK_PTR);
  FOR_EACH_BB (bb)
    {
      queue.quick_push (bb);
      bb->aux = bb;
    }

  while (!queue.is_empty ())
    {
      edge_iterator ei;
      edge e;
      bool change = false;
      int i;

      bb = queue.pop ();
      bb->aux = NULL;

      if (bitmap_bit_p (final_bbs, bb->index))
	continue;

      for (i = 0; i < func_param_count; i++)
	{
	  int idx = bb->index * func_param_count + i;
	  bool first = true;
	  HOST_WIDE_INT inh = 0;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    int succ_idx = e->dest->index * func_param_count + i;

	    if (e->src == EXIT_BLOCK_PTR)
	      continue;

	    if (first)
	      {
		first = false;
		inh = bb_dereferences [succ_idx];
	      }
	    else if (bb_dereferences [succ_idx] < inh)
	      inh = bb_dereferences [succ_idx];
	  }

	  if (!first && bb_dereferences[idx] < inh)
	    {
	      bb_dereferences[idx] = inh;
	      change = true;
	    }
	}

      if (change && !bitmap_bit_p (final_bbs, bb->index))
	FOR_EACH_EDGE (e, ei, bb->preds)
	  {
	    if (e->src->aux)
	      continue;

	    e->src->aux = e->src;
	    queue.quick_push (e->src);
	  }
    }

  queue.release ();
}

/* Dump a dereferences TABLE with heading STR to file F.  */

static void
dump_dereferences_table (FILE *f, const char *str, HOST_WIDE_INT *table)
{
  basic_block bb;

  fprintf (dump_file, str);
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      fprintf (f, "%4i  %i   ", bb->index, bitmap_bit_p (final_bbs, bb->index));
      if (bb != EXIT_BLOCK_PTR)
	{
	  int i;
	  for (i = 0; i < func_param_count; i++)
	    {
	      int idx = bb->index * func_param_count + i;
	      fprintf (f, " %4" HOST_WIDE_INT_PRINT "d", table[idx]);
	    }
	}
      fprintf (f, "\n");
    }
  fprintf (dump_file, "\n");
}

/* Determine what (parts of) parameters passed by reference that are not
   assigned to are not certainly dereferenced in this function and thus the
   dereferencing cannot be safely moved to the caller without potentially
   introducing a segfault.  Mark such REPRESENTATIVES as
   grp_not_necessarilly_dereferenced.

   The dereferenced maximum "distance," i.e. the offset + size of the accessed
   part is calculated rather than simple booleans are calculated for each
   pointer parameter to handle cases when only a fraction of the whole
   aggregate is allocated (see testsuite/gcc.c-torture/execute/ipa-sra-2.c for
   an example).

   The maximum dereference distances for each pointer parameter and BB are
   already stored in bb_dereference.  This routine simply propagates these
   values upwards by propagate_dereference_distances and then compares the
   distances of individual parameters in the ENTRY BB to the equivalent
   distances of each representative of a (fraction of a) parameter.  */

static void
analyze_caller_dereference_legality (vec<access_p> representatives)
{
  int i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_dereferences_table (dump_file,
			     "Dereference table before propagation:\n",
			     bb_dereferences);

  propagate_dereference_distances ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_dereferences_table (dump_file,
			     "Dereference table after propagation:\n",
			     bb_dereferences);

  for (i = 0; i < func_param_count; i++)
    {
      struct access *repr = representatives[i];
      int idx = ENTRY_BLOCK_PTR->index * func_param_count + i;

      if (!repr || no_accesses_p (repr))
	continue;

      do
	{
	  if ((repr->offset + repr->size) > bb_dereferences[idx])
	    repr->grp_not_necessarilly_dereferenced = 1;
	  repr = repr->next_grp;
	}
      while (repr);
    }
}

/* Return the representative access for the parameter declaration PARM if it is
   a scalar passed by reference which is not written to and the pointer value
   is not used directly.  Thus, if it is legal to dereference it in the caller
   and we can rule out modifications through aliases, such parameter should be
   turned into one passed by value.  Return NULL otherwise.  */

static struct access *
unmodified_by_ref_scalar_representative (tree parm)
{
  int i, access_count;
  struct access *repr;
  vec<access_p> *access_vec;

  access_vec = get_base_access_vector (parm);
  gcc_assert (access_vec);
  repr = (*access_vec)[0];
  if (repr->write)
    return NULL;
  repr->group_representative = repr;

  access_count = access_vec->length ();
  for (i = 1; i < access_count; i++)
    {
      struct access *access = (*access_vec)[i];
      if (access->write)
	return NULL;
      access->group_representative = repr;
      access->next_sibling = repr->next_sibling;
      repr->next_sibling = access;
    }

  repr->grp_read = 1;
  repr->grp_scalar_ptr = 1;
  return repr;
}

/* Return true iff this ACCESS precludes IPA-SRA of the parameter it is
   associated with.  REQ_ALIGN is the minimum required alignment.  */

static bool
access_precludes_ipa_sra_p (struct access *access, unsigned int req_align)
{
  unsigned int exp_align;
  /* Avoid issues such as the second simple testcase in PR 42025.  The problem
     is incompatible assign in a call statement (and possibly even in asm
     statements).  This can be relaxed by using a new temporary but only for
     non-TREE_ADDRESSABLE types and is probably not worth the complexity. (In
     intraprocedural SRA we deal with this by keeping the old aggregate around,
     something we cannot do in IPA-SRA.)  */
  if (access->write
      && (is_gimple_call (access->stmt)
	  || gimple_code (access->stmt) == GIMPLE_ASM))
    return true;

  exp_align = get_object_alignment (access->expr);
  if (exp_align < req_align)
    return true;

  return false;
}


/* Sort collected accesses for parameter PARM, identify representatives for
   each accessed region and link them together.  Return NULL if there are
   different but overlapping accesses, return the special ptr value meaning
   there are no accesses for this parameter if that is the case and return the
   first representative otherwise.  Set *RO_GRP if there is a group of accesses
   with only read (i.e. no write) accesses.  */

static struct access *
splice_param_accesses (tree parm, bool *ro_grp)
{
  int i, j, access_count, group_count;
  int agg_size, total_size = 0;
  struct access *access, *res, **prev_acc_ptr = &res;
  vec<access_p> *access_vec;

  access_vec = get_base_access_vector (parm);
  if (!access_vec)
    return &no_accesses_representant;
  access_count = access_vec->length ();

  access_vec->qsort (compare_access_positions);

  i = 0;
  total_size = 0;
  group_count = 0;
  while (i < access_count)
    {
      bool modification;
      tree a1_alias_type;
      access = (*access_vec)[i];
      modification = access->write;
      if (access_precludes_ipa_sra_p (access, TYPE_ALIGN (access->type)))
	return NULL;
      a1_alias_type = reference_alias_ptr_type (access->expr);

      /* Access is about to become group representative unless we find some
	 nasty overlap which would preclude us from breaking this parameter
	 apart. */

      j = i + 1;
      while (j < access_count)
	{
	  struct access *ac2 = (*access_vec)[j];
	  if (ac2->offset != access->offset)
	    {
	      /* All or nothing law for parameters. */
	      if (access->offset + access->size > ac2->offset)
		return NULL;
	      else
		break;
	    }
	  else if (ac2->size != access->size)
	    return NULL;

	  if (access_precludes_ipa_sra_p (ac2, TYPE_ALIGN (access->type))
	      || (ac2->type != access->type
		  && (TREE_ADDRESSABLE (ac2->type)
		      || TREE_ADDRESSABLE (access->type)))
	      || (reference_alias_ptr_type (ac2->expr) != a1_alias_type))
	    return NULL;

	  modification |= ac2->write;
	  ac2->group_representative = access;
	  ac2->next_sibling = access->next_sibling;
	  access->next_sibling = ac2;
	  j++;
	}

      group_count++;
      access->grp_maybe_modified = modification;
      if (!modification)
	*ro_grp = true;
      *prev_acc_ptr = access;
      prev_acc_ptr = &access->next_grp;
      total_size += access->size;
      i = j;
    }

  if (POINTER_TYPE_P (TREE_TYPE (parm)))
    agg_size = tree_low_cst (TYPE_SIZE (TREE_TYPE (TREE_TYPE (parm))), 1);
  else
    agg_size = tree_low_cst (TYPE_SIZE (TREE_TYPE (parm)), 1);
  if (total_size >= agg_size)
    return NULL;

  gcc_assert (group_count > 0);
  return res;
}

/* Decide whether parameters with representative accesses given by REPR should
   be reduced into components.  */

static int
decide_one_param_reduction (struct access *repr)
{
  int total_size, cur_parm_size, agg_size, new_param_count, parm_size_limit;
  bool by_ref;
  tree parm;

  parm = repr->base;
  cur_parm_size = tree_low_cst (TYPE_SIZE (TREE_TYPE (parm)), 1);
  gcc_assert (cur_parm_size > 0);

  if (POINTER_TYPE_P (TREE_TYPE (parm)))
    {
      by_ref = true;
      agg_size = tree_low_cst (TYPE_SIZE (TREE_TYPE (TREE_TYPE (parm))), 1);
    }
  else
    {
      by_ref = false;
      agg_size = cur_parm_size;
    }

  if (dump_file)
    {
      struct access *acc;
      fprintf (dump_file, "Evaluating PARAM group sizes for ");
      print_generic_expr (dump_file, parm, 0);
      fprintf (dump_file, " (UID: %u): \n", DECL_UID (parm));
      for (acc = repr; acc; acc = acc->next_grp)
	dump_access (dump_file, acc, true);
    }

  total_size = 0;
  new_param_count = 0;

  for (; repr; repr = repr->next_grp)
    {
      gcc_assert (parm == repr->base);

      /* Taking the address of a non-addressable field is verboten.  */
      if (by_ref && repr->non_addressable)
	return 0;

      /* Do not decompose a non-BLKmode param in a way that would
         create BLKmode params.  Especially for by-reference passing
	 (thus, pointer-type param) this is hardly worthwhile.  */
      if (DECL_MODE (parm) != BLKmode
	  && TYPE_MODE (repr->type) == BLKmode)
	return 0;

      if (!by_ref || (!repr->grp_maybe_modified
		      && !repr->grp_not_necessarilly_dereferenced))
	total_size += repr->size;
      else
	total_size += cur_parm_size;

      new_param_count++;
    }

  gcc_assert (new_param_count > 0);

  if (optimize_function_for_size_p (cfun))
    parm_size_limit = cur_parm_size;
  else
    parm_size_limit = (PARAM_VALUE (PARAM_IPA_SRA_PTR_GROWTH_FACTOR)
                       * cur_parm_size);

  if (total_size < agg_size
      && total_size <= parm_size_limit)
    {
      if (dump_file)
	fprintf (dump_file, "    ....will be split into %i components\n",
		 new_param_count);
      return new_param_count;
    }
  else
    return 0;
}

/* The order of the following enums is important, we need to do extra work for
   UNUSED_PARAMS, BY_VAL_ACCESSES and UNMODIF_BY_REF_ACCESSES.  */
enum ipa_splicing_result { NO_GOOD_ACCESS, UNUSED_PARAMS, BY_VAL_ACCESSES,
			  MODIF_BY_REF_ACCESSES, UNMODIF_BY_REF_ACCESSES };

/* Identify representatives of all accesses to all candidate parameters for
   IPA-SRA.  Return result based on what representatives have been found. */

static enum ipa_splicing_result
splice_all_param_accesses (vec<access_p> &representatives)
{
  enum ipa_splicing_result result = NO_GOOD_ACCESS;
  tree parm;
  struct access *repr;

  representatives.create (func_param_count);

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = DECL_CHAIN (parm))
    {
      if (is_unused_scalar_param (parm))
	{
	  representatives.quick_push (&no_accesses_representant);
	  if (result == NO_GOOD_ACCESS)
	    result = UNUSED_PARAMS;
	}
      else if (POINTER_TYPE_P (TREE_TYPE (parm))
	       && is_gimple_reg_type (TREE_TYPE (TREE_TYPE (parm)))
	       && bitmap_bit_p (candidate_bitmap, DECL_UID (parm)))
	{
	  repr = unmodified_by_ref_scalar_representative (parm);
	  representatives.quick_push (repr);
	  if (repr)
	    result = UNMODIF_BY_REF_ACCESSES;
	}
      else if (bitmap_bit_p (candidate_bitmap, DECL_UID (parm)))
	{
	  bool ro_grp = false;
	  repr = splice_param_accesses (parm, &ro_grp);
	  representatives.quick_push (repr);

	  if (repr && !no_accesses_p (repr))
	    {
	      if (POINTER_TYPE_P (TREE_TYPE (parm)))
		{
		  if (ro_grp)
		    result = UNMODIF_BY_REF_ACCESSES;
		  else if (result < MODIF_BY_REF_ACCESSES)
		    result = MODIF_BY_REF_ACCESSES;
		}
	      else if (result < BY_VAL_ACCESSES)
		result = BY_VAL_ACCESSES;
	    }
	  else if (no_accesses_p (repr) && (result == NO_GOOD_ACCESS))
	    result = UNUSED_PARAMS;
	}
      else
	representatives.quick_push (NULL);
    }

  if (result == NO_GOOD_ACCESS)
    {
      representatives.release ();
      return NO_GOOD_ACCESS;
    }

  return result;
}

/* Return the index of BASE in PARMS.  Abort if it is not found.  */

static inline int
get_param_index (tree base, vec<tree> parms)
{
  int i, len;

  len = parms.length ();
  for (i = 0; i < len; i++)
    if (parms[i] == base)
      return i;
  gcc_unreachable ();
}

/* Convert the decisions made at the representative level into compact
   parameter adjustments.  REPRESENTATIVES are pointers to first
   representatives of each param accesses, ADJUSTMENTS_COUNT is the expected
   final number of adjustments.  */

static ipa_parm_adjustment_vec
turn_representatives_into_adjustments (vec<access_p> representatives,
				       int adjustments_count)
{
  vec<tree> parms;
  ipa_parm_adjustment_vec adjustments;
  tree parm;
  int i;

  gcc_assert (adjustments_count > 0);
  parms = ipa_get_vector_of_formal_parms (current_function_decl);
  adjustments.create (adjustments_count);
  parm = DECL_ARGUMENTS (current_function_decl);
  for (i = 0; i < func_param_count; i++, parm = DECL_CHAIN (parm))
    {
      struct access *repr = representatives[i];

      if (!repr || no_accesses_p (repr))
	{
	  struct ipa_parm_adjustment adj;

	  memset (&adj, 0, sizeof (adj));
	  adj.base_index = get_param_index (parm, parms);
	  adj.base = parm;
	  if (!repr)
	    adj.copy_param = 1;
	  else
	    adj.remove_param = 1;
	  adjustments.quick_push (adj);
	}
      else
	{
	  struct ipa_parm_adjustment adj;
	  int index = get_param_index (parm, parms);

	  for (; repr; repr = repr->next_grp)
	    {
	      memset (&adj, 0, sizeof (adj));
	      gcc_assert (repr->base == parm);
	      adj.base_index = index;
	      adj.base = repr->base;
	      adj.type = repr->type;
	      adj.alias_ptr_type = reference_alias_ptr_type (repr->expr);
	      adj.offset = repr->offset;
	      adj.by_ref = (POINTER_TYPE_P (TREE_TYPE (repr->base))
			    && (repr->grp_maybe_modified
				|| repr->grp_not_necessarilly_dereferenced));
	      adjustments.quick_push (adj);
	    }
	}
    }
  parms.release ();
  return adjustments;
}

/* Analyze the collected accesses and produce a plan what to do with the
   parameters in the form of adjustments, NULL meaning nothing.  */

static ipa_parm_adjustment_vec
analyze_all_param_acesses (void)
{
  enum ipa_splicing_result repr_state;
  bool proceed = false;
  int i, adjustments_count = 0;
  vec<access_p> representatives;
  ipa_parm_adjustment_vec adjustments;

  repr_state = splice_all_param_accesses (representatives);
  if (repr_state == NO_GOOD_ACCESS)
    return ipa_parm_adjustment_vec ();

  /* If there are any parameters passed by reference which are not modified
     directly, we need to check whether they can be modified indirectly.  */
  if (repr_state == UNMODIF_BY_REF_ACCESSES)
    {
      analyze_caller_dereference_legality (representatives);
      analyze_modified_params (representatives);
    }

  for (i = 0; i < func_param_count; i++)
    {
      struct access *repr = representatives[i];

      if (repr && !no_accesses_p (repr))
	{
	  if (repr->grp_scalar_ptr)
	    {
	      adjustments_count++;
	      if (repr->grp_not_necessarilly_dereferenced
		  || repr->grp_maybe_modified)
		representatives[i] = NULL;
	      else
		{
		  proceed = true;
		  sra_stats.scalar_by_ref_to_by_val++;
		}
	    }
	  else
	    {
	      int new_components = decide_one_param_reduction (repr);

	      if (new_components == 0)
		{
		  representatives[i] = NULL;
		  adjustments_count++;
		}
	      else
		{
		  adjustments_count += new_components;
		  sra_stats.aggregate_params_reduced++;
		  sra_stats.param_reductions_created += new_components;
		  proceed = true;
		}
	    }
	}
      else
	{
	  if (no_accesses_p (repr))
	    {
	      proceed = true;
	      sra_stats.deleted_unused_parameters++;
	    }
	  adjustments_count++;
	}
    }

  if (!proceed && dump_file)
    fprintf (dump_file, "NOT proceeding to change params.\n");

  if (proceed)
    adjustments = turn_representatives_into_adjustments (representatives,
							 adjustments_count);
  else
    adjustments = ipa_parm_adjustment_vec ();

  representatives.release ();
  return adjustments;
}

/* If a parameter replacement identified by ADJ does not yet exist in the form
   of declaration, create it and record it, otherwise return the previously
   created one.  */

static tree
get_replaced_param_substitute (struct ipa_parm_adjustment *adj)
{
  tree repl;
  if (!adj->new_ssa_base)
    {
      char *pretty_name = make_fancy_name (adj->base);

      repl = create_tmp_reg (TREE_TYPE (adj->base), "ISR");
      DECL_NAME (repl) = get_identifier (pretty_name);
      obstack_free (&name_obstack, pretty_name);

      adj->new_ssa_base = repl;
    }
  else
    repl = adj->new_ssa_base;
  return repl;
}

/* Find the first adjustment for a particular parameter BASE in a vector of
   ADJUSTMENTS which is not a copy_param.  Return NULL if there is no such
   adjustment. */

static struct ipa_parm_adjustment *
get_adjustment_for_base (ipa_parm_adjustment_vec adjustments, tree base)
{
  int i, len;

  len = adjustments.length ();
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;

      adj = &adjustments[i];
      if (!adj->copy_param && adj->base == base)
	return adj;
    }

  return NULL;
}

/* If the statement STMT defines an SSA_NAME of a parameter which is to be
   removed because its value is not used, replace the SSA_NAME with a one
   relating to a created VAR_DECL together all of its uses and return true.
   ADJUSTMENTS is a pointer to an adjustments vector.  */

static bool
replace_removed_params_ssa_names (gimple stmt,
				  ipa_parm_adjustment_vec adjustments)
{
  struct ipa_parm_adjustment *adj;
  tree lhs, decl, repl, name;

  if (gimple_code (stmt) == GIMPLE_PHI)
    lhs = gimple_phi_result (stmt);
  else if (is_gimple_assign (stmt))
    lhs = gimple_assign_lhs (stmt);
  else if (is_gimple_call (stmt))
    lhs = gimple_call_lhs (stmt);
  else
    gcc_unreachable ();

  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  decl = SSA_NAME_VAR (lhs);
  if (decl == NULL_TREE
      || TREE_CODE (decl) != PARM_DECL)
    return false;

  adj = get_adjustment_for_base (adjustments, decl);
  if (!adj)
    return false;

  repl = get_replaced_param_substitute (adj);
  name = make_ssa_name (repl, stmt);

  if (dump_file)
    {
      fprintf (dump_file, "replacing an SSA name of a removed param ");
      print_generic_expr (dump_file, lhs, 0);
      fprintf (dump_file, " with ");
      print_generic_expr (dump_file, name, 0);
      fprintf (dump_file, "\n");
    }

  if (is_gimple_assign (stmt))
    gimple_assign_set_lhs (stmt, name);
  else if (is_gimple_call (stmt))
    gimple_call_set_lhs (stmt, name);
  else
    gimple_phi_set_result (stmt, name);

  replace_uses_by (lhs, name);
  release_ssa_name (lhs);
  return true;
}

/* If the expression *EXPR should be replaced by a reduction of a parameter, do
   so.  ADJUSTMENTS is a pointer to a vector of adjustments.  CONVERT
   specifies whether the function should care about type incompatibility the
   current and new expressions.  If it is false, the function will leave
   incompatibility issues to the caller.  Return true iff the expression
   was modified. */

static bool
sra_ipa_modify_expr (tree *expr, bool convert,
		     ipa_parm_adjustment_vec adjustments)
{
  int i, len;
  struct ipa_parm_adjustment *adj, *cand = NULL;
  HOST_WIDE_INT offset, size, max_size;
  tree base, src;

  len = adjustments.length ();

  if (TREE_CODE (*expr) == BIT_FIELD_REF
      || TREE_CODE (*expr) == IMAGPART_EXPR
      || TREE_CODE (*expr) == REALPART_EXPR)
    {
      expr = &TREE_OPERAND (*expr, 0);
      convert = true;
    }

  base = get_ref_base_and_extent (*expr, &offset, &size, &max_size);
  if (!base || size == -1 || max_size == -1)
    return false;

  if (TREE_CODE (base) == MEM_REF)
    {
      offset += mem_ref_offset (base).low * BITS_PER_UNIT;
      base = TREE_OPERAND (base, 0);
    }

  base = get_ssa_base_param (base);
  if (!base || TREE_CODE (base) != PARM_DECL)
    return false;

  for (i = 0; i < len; i++)
    {
      adj = &adjustments[i];

      if (adj->base == base
	  && (adj->offset == offset || adj->remove_param))
	{
	  cand = adj;
	  break;
	}
    }
  if (!cand || cand->copy_param || cand->remove_param)
    return false;

  if (cand->by_ref)
    src = build_simple_mem_ref (cand->reduction);
  else
    src = cand->reduction;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "About to replace expr ");
      print_generic_expr (dump_file, *expr, 0);
      fprintf (dump_file, " with ");
      print_generic_expr (dump_file, src, 0);
      fprintf (dump_file, "\n");
    }

  if (convert && !useless_type_conversion_p (TREE_TYPE (*expr), cand->type))
    {
      tree vce = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (*expr), src);
      *expr = vce;
    }
  else
    *expr = src;
  return true;
}

/* If the statement pointed to by STMT_PTR contains any expressions that need
   to replaced with a different one as noted by ADJUSTMENTS, do so.  Handle any
   potential type incompatibilities (GSI is used to accommodate conversion
   statements and must point to the statement).  Return true iff the statement
   was modified.  */

static bool
sra_ipa_modify_assign (gimple *stmt_ptr, gimple_stmt_iterator *gsi,
		       ipa_parm_adjustment_vec adjustments)
{
  gimple stmt = *stmt_ptr;
  tree *lhs_p, *rhs_p;
  bool any;

  if (!gimple_assign_single_p (stmt))
    return false;

  rhs_p = gimple_assign_rhs1_ptr (stmt);
  lhs_p = gimple_assign_lhs_ptr (stmt);

  any = sra_ipa_modify_expr (rhs_p, false, adjustments);
  any |= sra_ipa_modify_expr (lhs_p, false, adjustments);
  if (any)
    {
      tree new_rhs = NULL_TREE;

      if (!useless_type_conversion_p (TREE_TYPE (*lhs_p), TREE_TYPE (*rhs_p)))
	{
	  if (TREE_CODE (*rhs_p) == CONSTRUCTOR)
	    {
	      /* V_C_Es of constructors can cause trouble (PR 42714).  */
	      if (is_gimple_reg_type (TREE_TYPE (*lhs_p)))
		*rhs_p = build_zero_cst (TREE_TYPE (*lhs_p));
	      else
		*rhs_p = build_constructor (TREE_TYPE (*lhs_p),
					    NULL);
	    }
	  else
	    new_rhs = fold_build1_loc (gimple_location (stmt),
				       VIEW_CONVERT_EXPR, TREE_TYPE (*lhs_p),
				       *rhs_p);
	}
      else if (REFERENCE_CLASS_P (*rhs_p)
	       && is_gimple_reg_type (TREE_TYPE (*lhs_p))
	       && !is_gimple_reg (*lhs_p))
	/* This can happen when an assignment in between two single field
	   structures is turned into an assignment in between two pointers to
	   scalars (PR 42237).  */
	new_rhs = *rhs_p;

      if (new_rhs)
	{
	  tree tmp = force_gimple_operand_gsi (gsi, new_rhs, true, NULL_TREE,
					       true, GSI_SAME_STMT);

	  gimple_assign_set_rhs_from_tree (gsi, tmp);
	}

      return true;
    }

  return false;
}

/* Traverse the function body and all modifications as described in
   ADJUSTMENTS.  Return true iff the CFG has been changed.  */

static bool
ipa_sra_modify_function_body (ipa_parm_adjustment_vec adjustments)
{
  bool cfg_changed = false;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	replace_removed_params_ssa_names (gsi_stmt (gsi), adjustments);

      gsi = gsi_start_bb (bb);
      while (!gsi_end_p (gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  bool modified = false;
	  tree *t;
	  unsigned i;

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval_ptr (stmt);
	      if (*t != NULL_TREE)
		modified |= sra_ipa_modify_expr (t, true, adjustments);
	      break;

	    case GIMPLE_ASSIGN:
	      modified |= sra_ipa_modify_assign (&stmt, &gsi, adjustments);
	      modified |= replace_removed_params_ssa_names (stmt, adjustments);
	      break;

	    case GIMPLE_CALL:
	      /* Operands must be processed before the lhs.  */
	      for (i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  t = gimple_call_arg_ptr (stmt, i);
		  modified |= sra_ipa_modify_expr (t, true, adjustments);
		}

	      if (gimple_call_lhs (stmt))
		{
		  t = gimple_call_lhs_ptr (stmt);
		  modified |= sra_ipa_modify_expr (t, false, adjustments);
		  modified |= replace_removed_params_ssa_names (stmt,
								adjustments);
		}
	      break;

	    case GIMPLE_ASM:
	      for (i = 0; i < gimple_asm_ninputs (stmt); i++)
		{
		  t = &TREE_VALUE (gimple_asm_input_op (stmt, i));
		  modified |= sra_ipa_modify_expr (t, true, adjustments);
		}
	      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
		{
		  t = &TREE_VALUE (gimple_asm_output_op (stmt, i));
		  modified |= sra_ipa_modify_expr (t, false, adjustments);
		}
	      break;

	    default:
	      break;
	    }

	  if (modified)
	    {
	      update_stmt (stmt);
	      if (maybe_clean_eh_stmt (stmt)
		  && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
		cfg_changed = true;
	    }
	  gsi_next (&gsi);
	}
    }

  return cfg_changed;
}

/* Call gimple_debug_bind_reset_value on all debug statements describing
   gimple register parameters that are being removed or replaced.  */

static void
sra_ipa_reset_debug_stmts (ipa_parm_adjustment_vec adjustments)
{
  int i, len;
  gimple_stmt_iterator *gsip = NULL, gsi;

  if (MAY_HAVE_DEBUG_STMTS && single_succ_p (ENTRY_BLOCK_PTR))
    {
      gsi = gsi_after_labels (single_succ (ENTRY_BLOCK_PTR));
      gsip = &gsi;
    }
  len = adjustments.length ();
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      imm_use_iterator ui;
      gimple stmt, def_temp;
      tree name, vexpr, copy = NULL_TREE;
      use_operand_p use_p;

      adj = &adjustments[i];
      if (adj->copy_param || !is_gimple_reg (adj->base))
	continue;
      name = ssa_default_def (cfun, adj->base);
      vexpr = NULL;
      if (name)
	FOR_EACH_IMM_USE_STMT (stmt, ui, name)
	  {
	    if (gimple_clobber_p (stmt))
	      {
		gimple_stmt_iterator cgsi = gsi_for_stmt (stmt);
		unlink_stmt_vdef (stmt);
		gsi_remove (&cgsi, true);
		release_defs (stmt);
		continue;
	      }
	    /* All other users must have been removed by
	       ipa_sra_modify_function_body.  */
	    gcc_assert (is_gimple_debug (stmt));
	    if (vexpr == NULL && gsip != NULL)
	      {
		gcc_assert (TREE_CODE (adj->base) == PARM_DECL);
		vexpr = make_node (DEBUG_EXPR_DECL);
		def_temp = gimple_build_debug_source_bind (vexpr, adj->base,
							   NULL);
		DECL_ARTIFICIAL (vexpr) = 1;
		TREE_TYPE (vexpr) = TREE_TYPE (name);
		DECL_MODE (vexpr) = DECL_MODE (adj->base);
		gsi_insert_before (gsip, def_temp, GSI_SAME_STMT);
	      }
	    if (vexpr)
	      {
		FOR_EACH_IMM_USE_ON_STMT (use_p, ui)
		  SET_USE (use_p, vexpr);
	      }
	    else
	      gimple_debug_bind_reset_value (stmt);
	    update_stmt (stmt);
	  }
      /* Create a VAR_DECL for debug info purposes.  */
      if (!DECL_IGNORED_P (adj->base))
	{
	  copy = build_decl (DECL_SOURCE_LOCATION (current_function_decl),
			     VAR_DECL, DECL_NAME (adj->base),
			     TREE_TYPE (adj->base));
	  if (DECL_PT_UID_SET_P (adj->base))
	    SET_DECL_PT_UID (copy, DECL_PT_UID (adj->base));
	  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (adj->base);
	  TREE_READONLY (copy) = TREE_READONLY (adj->base);
	  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (adj->base);
	  DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (adj->base);
	  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (adj->base);
	  DECL_IGNORED_P (copy) = DECL_IGNORED_P (adj->base);
	  DECL_ABSTRACT_ORIGIN (copy) = DECL_ORIGIN (adj->base);
	  DECL_SEEN_IN_BIND_EXPR_P (copy) = 1;
	  SET_DECL_RTL (copy, 0);
	  TREE_USED (copy) = 1;
	  DECL_CONTEXT (copy) = current_function_decl;
	  add_local_decl (cfun, copy);
	  DECL_CHAIN (copy) =
	    BLOCK_VARS (DECL_INITIAL (current_function_decl));
	  BLOCK_VARS (DECL_INITIAL (current_function_decl)) = copy;
	}
      if (gsip != NULL && copy && target_for_debug_bind (adj->base))
	{
	  gcc_assert (TREE_CODE (adj->base) == PARM_DECL);
	  if (vexpr)
	    def_temp = gimple_build_debug_bind (copy, vexpr, NULL);
	  else
	    def_temp = gimple_build_debug_source_bind (copy, adj->base,
						       NULL);
	  gsi_insert_before (gsip, def_temp, GSI_SAME_STMT);
	}
    }
}

/* Return false iff all callers have at least as many actual arguments as there
   are formal parameters in the current function.  */

static bool
not_all_callers_have_enough_arguments_p (struct cgraph_node *node,
					 void *data ATTRIBUTE_UNUSED)
{
  struct cgraph_edge *cs;
  for (cs = node->callers; cs; cs = cs->next_caller)
    if (!callsite_has_enough_arguments_p (cs->call_stmt))
      return true;

  return false;
}

/* Convert all callers of NODE.  */

static bool
convert_callers_for_node (struct cgraph_node *node,
		          void *data)
{
  ipa_parm_adjustment_vec *adjustments = (ipa_parm_adjustment_vec *) data;
  bitmap recomputed_callers = BITMAP_ALLOC (NULL);
  struct cgraph_edge *cs;

  for (cs = node->callers; cs; cs = cs->next_caller)
    {
      push_cfun (DECL_STRUCT_FUNCTION (cs->caller->decl));

      if (dump_file)
	fprintf (dump_file, "Adjusting call %s/%i -> %s/%i\n",
		 xstrdup (cs->caller->name ()),
		 cs->caller->order,
		 xstrdup (cs->callee->name ()),
		 cs->callee->order);

      ipa_modify_call_arguments (cs, cs->call_stmt, *adjustments);

      pop_cfun ();
    }

  for (cs = node->callers; cs; cs = cs->next_caller)
    if (bitmap_set_bit (recomputed_callers, cs->caller->uid)
	&& gimple_in_ssa_p (DECL_STRUCT_FUNCTION (cs->caller->decl)))
      compute_inline_parameters (cs->caller, true);
  BITMAP_FREE (recomputed_callers);

  return true;
}

/* Convert all callers of NODE to pass parameters as given in ADJUSTMENTS.  */

static void
convert_callers (struct cgraph_node *node, tree old_decl,
		 ipa_parm_adjustment_vec adjustments)
{
  basic_block this_block;

  cgraph_for_node_and_aliases (node, convert_callers_for_node,
			       &adjustments, false);

  if (!encountered_recursive_call)
    return;

  FOR_EACH_BB (this_block)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (this_block); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  gimple stmt = gsi_stmt (gsi);
	  tree call_fndecl;
	  if (gimple_code (stmt) != GIMPLE_CALL)
	    continue;
	  call_fndecl = gimple_call_fndecl (stmt);
	  if (call_fndecl == old_decl)
	    {
	      if (dump_file)
		fprintf (dump_file, "Adjusting recursive call");
	      gimple_call_set_fndecl (stmt, node->decl);
	      ipa_modify_call_arguments (NULL, stmt, adjustments);
	    }
	}
    }

  return;
}

/* Perform all the modification required in IPA-SRA for NODE to have parameters
   as given in ADJUSTMENTS.  Return true iff the CFG has been changed.  */

static bool
modify_function (struct cgraph_node *node, ipa_parm_adjustment_vec adjustments)
{
  struct cgraph_node *new_node;
  bool cfg_changed;
  vec<cgraph_edge_p> redirect_callers = collect_callers_of_node (node);

  rebuild_cgraph_edges ();
  free_dominance_info (CDI_DOMINATORS);
  pop_cfun ();

  new_node = cgraph_function_versioning (node, redirect_callers,
					 NULL,
					 NULL, false, NULL, NULL, "isra");
  redirect_callers.release ();

  push_cfun (DECL_STRUCT_FUNCTION (new_node->decl));
  ipa_modify_formal_parameters (current_function_decl, adjustments, "ISRA");
  cfg_changed = ipa_sra_modify_function_body (adjustments);
  sra_ipa_reset_debug_stmts (adjustments);
  convert_callers (new_node, node->decl, adjustments);
  cgraph_make_node_local (new_node);
  return cfg_changed;
}

/* If NODE has a caller, return true.  */

static bool
has_caller_p (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (node->callers)
    return true;
  return false;
}

/* Return false the function is apparently unsuitable for IPA-SRA based on it's
   attributes, return true otherwise.  NODE is the cgraph node of the current
   function.  */

static bool
ipa_sra_preliminary_function_checks (struct cgraph_node *node)
{
  if (!cgraph_node_can_be_local_p (node))
    {
      if (dump_file)
	fprintf (dump_file, "Function not local to this compilation unit.\n");
      return false;
    }

  if (!node->local.can_change_signature)
    {
      if (dump_file)
	fprintf (dump_file, "Function can not change signature.\n");
      return false;
    }

  if (!tree_versionable_function_p (node->decl))
    {
      if (dump_file)
	fprintf (dump_file, "Function is not versionable.\n");
      return false;
    }

  if (DECL_VIRTUAL_P (current_function_decl))
    {
      if (dump_file)
	fprintf (dump_file, "Function is a virtual method.\n");
      return false;
    }

  if ((DECL_COMDAT (node->decl) || DECL_EXTERNAL (node->decl))
      && inline_summary (node)->size >= MAX_INLINE_INSNS_AUTO)
    {
      if (dump_file)
	fprintf (dump_file, "Function too big to be made truly local.\n");
      return false;
    }

  if (!cgraph_for_node_and_aliases (node, has_caller_p, NULL, true))
    {
      if (dump_file)
	fprintf (dump_file,
		 "Function has no callers in this compilation unit.\n");
      return false;
    }

  if (cfun->stdarg)
    {
      if (dump_file)
	fprintf (dump_file, "Function uses stdarg. \n");
      return false;
    }

  if (TYPE_ATTRIBUTES (TREE_TYPE (node->decl)))
    return false;

  return true;
}

/* Perform early interprocedural SRA.  */

static unsigned int
ipa_early_sra (void)
{
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  ipa_parm_adjustment_vec adjustments;
  int ret = 0;

  if (!ipa_sra_preliminary_function_checks (node))
    return 0;

  sra_initialize ();
  sra_mode = SRA_MODE_EARLY_IPA;

  if (!find_param_candidates ())
    {
      if (dump_file)
	fprintf (dump_file, "Function has no IPA-SRA candidates.\n");
      goto simple_out;
    }

  if (cgraph_for_node_and_aliases (node, not_all_callers_have_enough_arguments_p,
				   NULL, true))
    {
      if (dump_file)
	fprintf (dump_file, "There are callers with insufficient number of "
		 "arguments.\n");
      goto simple_out;
    }

  bb_dereferences = XCNEWVEC (HOST_WIDE_INT,
				 func_param_count
				 * last_basic_block_for_function (cfun));
  final_bbs = BITMAP_ALLOC (NULL);

  scan_function ();
  if (encountered_apply_args)
    {
      if (dump_file)
	fprintf (dump_file, "Function calls  __builtin_apply_args().\n");
      goto out;
    }

  if (encountered_unchangable_recursive_call)
    {
      if (dump_file)
	fprintf (dump_file, "Function calls itself with insufficient "
		 "number of arguments.\n");
      goto out;
    }

  adjustments = analyze_all_param_acesses ();
  if (!adjustments.exists ())
    goto out;
  if (dump_file)
    ipa_dump_param_adjustments (dump_file, adjustments, current_function_decl);

  if (modify_function (node, adjustments))
    ret = TODO_update_ssa | TODO_cleanup_cfg;
  else
    ret = TODO_update_ssa;
  adjustments.release ();

  statistics_counter_event (cfun, "Unused parameters deleted",
			    sra_stats.deleted_unused_parameters);
  statistics_counter_event (cfun, "Scalar parameters converted to by-value",
			    sra_stats.scalar_by_ref_to_by_val);
  statistics_counter_event (cfun, "Aggregate parameters broken up",
			    sra_stats.aggregate_params_reduced);
  statistics_counter_event (cfun, "Aggregate parameter components created",
			    sra_stats.param_reductions_created);

 out:
  BITMAP_FREE (final_bbs);
  free (bb_dereferences);
 simple_out:
  sra_deinitialize ();
  return ret;
}

/* Return if early ipa sra shall be performed.  */
static bool
ipa_early_sra_gate (void)
{
  return flag_ipa_sra && dbg_cnt (eipa_sra);
}

namespace {

const pass_data pass_data_early_ipa_sra =
{
  GIMPLE_PASS, /* type */
  "eipa_sra", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_IPA_SRA, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_dump_symtab, /* todo_flags_finish */
};

class pass_early_ipa_sra : public gimple_opt_pass
{
public:
  pass_early_ipa_sra (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_ipa_sra, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return ipa_early_sra_gate (); }
  unsigned int execute () { return ipa_early_sra (); }

}; // class pass_early_ipa_sra

} // anon namespace

gimple_opt_pass *
make_pass_early_ipa_sra (gcc::context *ctxt)
{
  return new pass_early_ipa_sra (ctxt);
}
