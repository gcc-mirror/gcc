/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "tree-eh.h"
#include "stor-layout.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "dbgcnt.h"
#include "builtins.h"
#include "tree-sra.h"
#include "opts.h"

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
  gimple *stmt;

  /* Next group representative for this aggregate. */
  struct access *next_grp;

  /* Pointer to the group representative.  Pointer to itself if the struct is
     the representative.  */
  struct access *group_representative;

  /* After access tree has been constructed, this points to the parent of the
     current access, if there is one.  NULL for roots.  */
  struct access *parent;

  /* If this access has any children (in terms of the definition above), this
     points to the first one.  */
  struct access *first_child;

  /* In intraprocedural SRA, pointer to the next sibling in the access tree as
     described above.  */
  struct access *next_sibling;

  /* Pointers to the first and last element in the linked list of assign
     links for propagation from LHS to RHS.  */
  struct assign_link *first_rhs_link, *last_rhs_link;

  /* Pointers to the first and last element in the linked list of assign
     links for propagation from LHS to RHS.  */
  struct assign_link *first_lhs_link, *last_lhs_link;

  /* Pointer to the next access in the work queues.  */
  struct access *next_rhs_queued, *next_lhs_queued;

  /* Replacement variable for this access "region."  Never to be accessed
     directly, always only by the means of get_access_replacement() and only
     when grp_to_be_replaced flag is set.  */
  tree replacement_decl;

  /* Is this access made in reverse storage order? */
  unsigned reverse : 1;

  /* Is this particular access write access? */
  unsigned write : 1;

  /* Is this access currently in the rhs work queue?  */
  unsigned grp_rhs_queued : 1;

  /* Is this access currently in the lhs work queue?  */
  unsigned grp_lhs_queued : 1;

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

  /* In a root of an access tree, true means that the entire tree should be
     totally scalarized - that all scalar leafs should be scalarized and
     non-root grp_total_scalarization accesses should be honored.  Otherwise,
     non-root accesses with grp_total_scalarization should never get scalar
     replacements.  */
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

  /* Set if all accesses in the group consist of the same chain of
     COMPONENT_REFs and ARRAY_REFs.  */
  unsigned grp_same_access_path : 1;

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

  /* Result of propagation accross link from LHS to RHS.  */
  unsigned grp_result_of_prop_from_lhs : 1;
};

typedef struct access *access_p;


/* Alloc pool for allocating access structures.  */
static object_allocator<struct access> access_pool ("SRA accesses");

/* A structure linking lhs and rhs accesses from an aggregate assignment.  They
   are used to propagate subaccesses from rhs to lhs and vice versa as long as
   they don't conflict with what is already there.  In the RHS->LHS direction,
   we also propagate grp_write flag to lazily mark that the access contains any
   meaningful data.  */
struct assign_link
{
  struct access *lacc, *racc;
  struct assign_link *next_rhs, *next_lhs;
};

/* Alloc pool for allocating assign link structures.  */
static object_allocator<assign_link> assign_link_pool ("SRA links");

/* Base (tree) -> Vector (vec<access_p> *) map.  */
static hash_map<tree, auto_vec<access_p> > *base_access_vec;

/* Hash to limit creation of artificial accesses */
static hash_map<tree, unsigned> *propagation_budget;

/* Candidate hash table helpers.  */

struct uid_decl_hasher : nofree_ptr_hash <tree_node>
{
  static inline hashval_t hash (const tree_node *);
  static inline bool equal (const tree_node *, const tree_node *);
};

/* Hash a tree in a uid_decl_map.  */

inline hashval_t
uid_decl_hasher::hash (const tree_node *item)
{
  return item->decl_minimal.uid;
}

/* Return true if the DECL_UID in both trees are equal.  */

inline bool
uid_decl_hasher::equal (const tree_node *a, const tree_node *b)
{
  return (a->decl_minimal.uid == b->decl_minimal.uid);
}

/* Set of candidates.  */
static bitmap candidate_bitmap;
static hash_table<uid_decl_hasher> *candidates;

/* For a candidate UID return the candidates decl.  */

static inline tree
candidate (unsigned uid)
{
 tree_node t;
 t.decl_minimal.uid = uid;
 return candidates->find_with_hash (&t, static_cast <hashval_t> (uid));
}

/* Bitmap of candidates which we should try to entirely scalarize away and
   those which cannot be (because they are and need be used as a whole).  */
static bitmap should_scalarize_away_bitmap, cannot_scalarize_away_bitmap;

/* Bitmap of candidates in the constant pool, which cannot be scalarized
   because this would produce non-constant expressions (e.g. Ada).  */
static bitmap disqualified_constants;

/* Bitmap of candidates which are passed by reference in call arguments.  */
static bitmap passed_by_ref_in_call;

/* Obstack for creation of fancy names.  */
static struct obstack name_obstack;

/* Head of a linked list of accesses that need to have its subaccesses
   propagated to their assignment counterparts. */
static struct access *rhs_work_queue_head, *lhs_work_queue_head;

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

  /* Number of deferred_init calls that are modified.  */
  int deferred_init;

  /* Number of deferred_init calls that are created by
     generate_subtree_deferred_init.  */
  int subtree_deferred_init;
} sra_stats;

static void
dump_access (FILE *f, struct access *access, bool grp)
{
  fprintf (f, "access { ");
  fprintf (f, "base = (%d)'", DECL_UID (access->base));
  print_generic_expr (f, access->base);
  fprintf (f, "', offset = " HOST_WIDE_INT_PRINT_DEC, access->offset);
  fprintf (f, ", size = " HOST_WIDE_INT_PRINT_DEC, access->size);
  fprintf (f, ", expr = ");
  print_generic_expr (f, access->expr);
  fprintf (f, ", type = ");
  print_generic_expr (f, access->type);
  fprintf (f, ", reverse = %d", access->reverse);
  if (grp)
    fprintf (f, ", grp_read = %d, grp_write = %d, grp_assignment_read = %d, "
	     "grp_assignment_write = %d, grp_scalar_read = %d, "
	     "grp_scalar_write = %d, grp_total_scalarization = %d, "
	     "grp_hint = %d, grp_covered = %d, "
	     "grp_unscalarizable_region = %d, grp_unscalarized_data = %d, "
	     "grp_same_access_path = %d, grp_partial_lhs = %d, "
	     "grp_to_be_replaced = %d, grp_to_be_debug_replaced = %d}\n",
	     access->grp_read, access->grp_write, access->grp_assignment_read,
	     access->grp_assignment_write, access->grp_scalar_read,
	     access->grp_scalar_write, access->grp_total_scalarization,
	     access->grp_hint, access->grp_covered,
	     access->grp_unscalarizable_region, access->grp_unscalarized_data,
	     access->grp_same_access_path, access->grp_partial_lhs,
	     access->grp_to_be_replaced, access->grp_to_be_debug_replaced);
  else
    fprintf (f, ", write = %d, grp_total_scalarization = %d, "
	     "grp_partial_lhs = %d}\n",
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
	fputs ("* ", f);

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
  return base_access_vec->get (base);
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

  /* Total scalarization does not replace single field structures with their
     single field but rather creates an access for them underneath.  Look for
     it.  */
  if (access)
    while (access->first_child
	   && access->first_child->offset == offset
	   && access->first_child->size == size)
      access = access->first_child;

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

  if (!racc->first_rhs_link)
    {
      gcc_assert (!racc->last_rhs_link);
      racc->first_rhs_link = link;
    }
  else
    racc->last_rhs_link->next_rhs = link;

  racc->last_rhs_link = link;
  link->next_rhs = NULL;
}

/* Add LINK to the linked list of lhs assign links of LACC.  */

static void
add_link_to_lhs (struct access *lacc, struct assign_link *link)
{
  gcc_assert (link->lacc == lacc);

  if (!lacc->first_lhs_link)
    {
      gcc_assert (!lacc->last_lhs_link);
      lacc->first_lhs_link = link;
    }
  else
    lacc->last_lhs_link->next_lhs = link;

  lacc->last_lhs_link = link;
  link->next_lhs = NULL;
}

/* Move all link structures in their linked list in OLD_ACC to the linked list
   in NEW_ACC.  */
static void
relink_to_new_repr (struct access *new_acc, struct access *old_acc)
{
  if (old_acc->first_rhs_link)
    {

      if (new_acc->first_rhs_link)
	{
	  gcc_assert (!new_acc->last_rhs_link->next_rhs);
	  gcc_assert (!old_acc->last_rhs_link
		      || !old_acc->last_rhs_link->next_rhs);

	  new_acc->last_rhs_link->next_rhs = old_acc->first_rhs_link;
	  new_acc->last_rhs_link = old_acc->last_rhs_link;
	}
      else
	{
	  gcc_assert (!new_acc->last_rhs_link);

	  new_acc->first_rhs_link = old_acc->first_rhs_link;
	  new_acc->last_rhs_link = old_acc->last_rhs_link;
	}
      old_acc->first_rhs_link = old_acc->last_rhs_link = NULL;
    }
  else
    gcc_assert (!old_acc->last_rhs_link);

  if (old_acc->first_lhs_link)
    {

      if (new_acc->first_lhs_link)
	{
	  gcc_assert (!new_acc->last_lhs_link->next_lhs);
	  gcc_assert (!old_acc->last_lhs_link
		      || !old_acc->last_lhs_link->next_lhs);

	  new_acc->last_lhs_link->next_lhs = old_acc->first_lhs_link;
	  new_acc->last_lhs_link = old_acc->last_lhs_link;
	}
      else
	{
	  gcc_assert (!new_acc->last_lhs_link);

	  new_acc->first_lhs_link = old_acc->first_lhs_link;
	  new_acc->last_lhs_link = old_acc->last_lhs_link;
	}
      old_acc->first_lhs_link = old_acc->last_lhs_link = NULL;
    }
  else
    gcc_assert (!old_acc->last_lhs_link);

}

/* Add ACCESS to the work to queue for propagation of subaccesses from RHS to
   LHS (which is actually a stack).  */

static void
add_access_to_rhs_work_queue (struct access *access)
{
  if (access->first_rhs_link && !access->grp_rhs_queued)
    {
      gcc_assert (!access->next_rhs_queued);
      access->next_rhs_queued = rhs_work_queue_head;
      access->grp_rhs_queued = 1;
      rhs_work_queue_head = access;
    }
}

/* Add ACCESS to the work to queue for propagation of subaccesses from LHS to
   RHS (which is actually a stack).  */

static void
add_access_to_lhs_work_queue (struct access *access)
{
  if (access->first_lhs_link && !access->grp_lhs_queued)
    {
      gcc_assert (!access->next_lhs_queued);
      access->next_lhs_queued = lhs_work_queue_head;
      access->grp_lhs_queued = 1;
      lhs_work_queue_head = access;
    }
}

/* Pop an access from the work queue for propagating from RHS to LHS, and
   return it, assuming there is one.  */

static struct access *
pop_access_from_rhs_work_queue (void)
{
  struct access *access = rhs_work_queue_head;

  rhs_work_queue_head = access->next_rhs_queued;
  access->next_rhs_queued = NULL;
  access->grp_rhs_queued = 0;
  return access;
}

/* Pop an access from the work queue for propagating from LHS to RHS, and
   return it, assuming there is one.  */

static struct access *
pop_access_from_lhs_work_queue (void)
{
  struct access *access = lhs_work_queue_head;

  lhs_work_queue_head = access->next_lhs_queued;
  access->next_lhs_queued = NULL;
  access->grp_lhs_queued = 0;
  return access;
}

/* Allocate necessary structures.  */

static void
sra_initialize (void)
{
  candidate_bitmap = BITMAP_ALLOC (NULL);
  candidates = new hash_table<uid_decl_hasher>
    (vec_safe_length (cfun->local_decls) / 2);
  should_scalarize_away_bitmap = BITMAP_ALLOC (NULL);
  cannot_scalarize_away_bitmap = BITMAP_ALLOC (NULL);
  disqualified_constants = BITMAP_ALLOC (NULL);
  passed_by_ref_in_call = BITMAP_ALLOC (NULL);
  gcc_obstack_init (&name_obstack);
  base_access_vec = new hash_map<tree, auto_vec<access_p> >;
  memset (&sra_stats, 0, sizeof (sra_stats));
}

/* Deallocate all general structures.  */

static void
sra_deinitialize (void)
{
  BITMAP_FREE (candidate_bitmap);
  delete candidates;
  candidates = NULL;
  BITMAP_FREE (should_scalarize_away_bitmap);
  BITMAP_FREE (cannot_scalarize_away_bitmap);
  BITMAP_FREE (disqualified_constants);
  BITMAP_FREE (passed_by_ref_in_call);
  access_pool.release ();
  assign_link_pool.release ();
  obstack_free (&name_obstack, NULL);

  delete base_access_vec;
}

/* Return true if DECL is a VAR_DECL in the constant pool, false otherwise.  */

static bool constant_decl_p (tree decl)
{
  return VAR_P (decl) && DECL_IN_CONSTANT_POOL (decl);
}

/* Remove DECL from candidates for SRA and write REASON to the dump file if
   there is one.  */

static void
disqualify_candidate (tree decl, const char *reason)
{
  if (bitmap_clear_bit (candidate_bitmap, DECL_UID (decl)))
    candidates->remove_elt_with_hash (decl, DECL_UID (decl));
  if (constant_decl_p (decl))
    bitmap_set_bit (disqualified_constants, DECL_UID (decl));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "! Disqualifying ");
      print_generic_expr (dump_file, decl);
      fprintf (dump_file, " - %s\n", reason);
    }
}

/* Return true iff the type contains a field or an element which does not allow
   scalarization.  Use VISITED_TYPES to avoid re-checking already checked
   (sub-)types.  */

static bool
type_internals_preclude_sra_p_1 (tree type, const char **msg,
				 hash_set<tree> *visited_types)
{
  tree fld;
  tree et;

  if (visited_types->contains (type))
    return false;
  visited_types->add (type);

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL)
	  {
	    if (TREE_CODE (fld) == FUNCTION_DECL)
	      continue;
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
	    if (!tree_fits_uhwi_p (DECL_FIELD_OFFSET (fld)))
	      {
		*msg = "structure field offset not fixed";
		return true;
	      }
	    if (!tree_fits_uhwi_p (DECL_SIZE (fld)))
	      {
	        *msg = "structure field size not fixed";
		return true;
	      }
	    if (!tree_fits_shwi_p (bit_position (fld)))
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

	    if (AGGREGATE_TYPE_P (ft)
	      && type_internals_preclude_sra_p_1 (ft, msg, visited_types))
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

      if (AGGREGATE_TYPE_P (et)
	  && type_internals_preclude_sra_p_1 (et, msg, visited_types))
	return true;

      return false;

    default:
      return false;
    }
}

/* Return true iff the type contains a field or an element which does not allow
   scalarization.  */

bool
type_internals_preclude_sra_p (tree type, const char **msg)
{
  hash_set<tree> visited_types;
  return type_internals_preclude_sra_p_1 (type, msg, &visited_types);
}


/* Allocate an access structure for BASE, OFFSET and SIZE, clear it, fill in
   the three fields.  Also add it to the vector of accesses corresponding to
   the base.  Finally, return the new access.  */

static struct access *
create_access_1 (tree base, HOST_WIDE_INT offset, HOST_WIDE_INT size)
{
  struct access *access = access_pool.allocate ();

  memset (access, 0, sizeof (struct access));
  access->base = base;
  access->offset = offset;
  access->size = size;

  base_access_vec->get_or_insert (base).safe_push (access);

  return access;
}

static bool maybe_add_sra_candidate (tree);

/* Create and insert access for EXPR. Return created access, or NULL if it is
   not possible.  Also scan for uses of constant pool as we go along and add
   to candidates.  */

static struct access *
create_access (tree expr, gimple *stmt, bool write)
{
  struct access *access;
  poly_int64 poffset, psize, pmax_size;
  tree base = expr;
  bool reverse, unscalarizable_region = false;

  base = get_ref_base_and_extent (expr, &poffset, &psize, &pmax_size,
				  &reverse);

  /* For constant-pool entries, check we can substitute the constant value.  */
  if (constant_decl_p (base)
      && !bitmap_bit_p (disqualified_constants, DECL_UID (base)))
    {
      if (expr != base
	  && !is_gimple_reg_type (TREE_TYPE (expr))
	  && dump_file && (dump_flags & TDF_DETAILS))
	{
	  /* This occurs in Ada with accesses to ARRAY_RANGE_REFs,
	     and elements of multidimensional arrays (which are
	     multi-element arrays in their own right).  */
	  fprintf (dump_file, "Allowing non-reg-type load of part"
			      " of constant-pool entry: ");
	  print_generic_expr (dump_file, expr);
	}
      maybe_add_sra_candidate (base);
    }

  if (!DECL_P (base) || !bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

  if (write && TREE_READONLY (base))
    {
      disqualify_candidate (base, "Encountered a store to a read-only decl.");
      return NULL;
    }

  HOST_WIDE_INT offset, size, max_size;
  if (!poffset.is_constant (&offset)
      || !psize.is_constant (&size)
      || !pmax_size.is_constant (&max_size))
    {
      disqualify_candidate (base, "Encountered a polynomial-sized access.");
      return NULL;
    }

  if (size != max_size)
    {
      size = max_size;
      unscalarizable_region = true;
    }
  if (size == 0)
    return NULL;
  if (offset < 0)
    {
      disqualify_candidate (base, "Encountered a negative offset access.");
      return NULL;
    }
  if (size < 0)
    {
      disqualify_candidate (base, "Encountered an unconstrained access.");
      return NULL;
    }
  if (offset + size > tree_to_shwi (DECL_SIZE (base)))
    {
      disqualify_candidate (base, "Encountered an access beyond the base.");
      return NULL;
    }
  if (TREE_CODE (TREE_TYPE (expr)) == BITINT_TYPE
      && size > WIDE_INT_MAX_PRECISION - 1)
    {
      disqualify_candidate (base, "Encountered too large _BitInt access.");
      return NULL;
    }

  access = create_access_1 (base, offset, size);
  access->expr = expr;
  access->type = TREE_TYPE (expr);
  access->write = write;
  access->grp_unscalarizable_region = unscalarizable_region;
  access->stmt = stmt;
  access->reverse = reverse;

  return access;
}

/* Given an array type TYPE, extract element size to *EL_SIZE, minimum index to
   *IDX and maximum index to *MAX so that the caller can iterate over all
   elements and return true, except if the array is known to be zero-length,
   then return false.  */

static bool
prepare_iteration_over_array_elts (tree type, HOST_WIDE_INT *el_size,
				   offset_int *idx, offset_int *max)
{
  tree elem_size = TYPE_SIZE (TREE_TYPE (type));
  gcc_assert (elem_size && tree_fits_shwi_p (elem_size));
  *el_size = tree_to_shwi (elem_size);
  gcc_assert (*el_size > 0);

  tree minidx = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
  gcc_assert (TREE_CODE (minidx) == INTEGER_CST);
  tree maxidx = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
  /* Skip (some) zero-length arrays; others have MAXIDX == MINIDX - 1.  */
  if (!maxidx)
    return false;
  gcc_assert (TREE_CODE (maxidx) == INTEGER_CST);
  tree domain = TYPE_DOMAIN (type);
  /* MINIDX and MAXIDX are inclusive, and must be interpreted in
     DOMAIN (e.g. signed int, whereas min/max may be size_int).  */
  *idx = wi::to_offset (minidx);
  *max = wi::to_offset (maxidx);
  if (!TYPE_UNSIGNED (domain))
    {
      *idx = wi::sext (*idx, TYPE_PRECISION (domain));
      *max = wi::sext (*max, TYPE_PRECISION (domain));
    }
  return true;
}

/* A structure to track collecting padding and hold collected padding
   information.   */

class sra_padding_collecting
{
public:
  /* Given that there won't be any data until at least OFFSET, add an
     appropriate entry to the list of paddings or extend the last one.  */
  void record_padding (HOST_WIDE_INT offset);
  /* Vector of pairs describing contiguous pieces of padding, each pair
     consisting of offset and length.  */
  auto_vec<std::pair<HOST_WIDE_INT, HOST_WIDE_INT>, 10> m_padding;
  /* Offset where data should continue after the last seen actual bit of data
     if there was no padding.  */
  HOST_WIDE_INT m_data_until = 0;
};

/* Given that there won't be any data until at least OFFSET, add an appropriate
   entry to the list of paddings or extend the last one.  */

void sra_padding_collecting::record_padding (HOST_WIDE_INT offset)
{
  if (offset > m_data_until)
    {
      HOST_WIDE_INT psz = offset - m_data_until;
      if (!m_padding.is_empty ()
	  && ((m_padding[m_padding.length () - 1].first
	       + m_padding[m_padding.length () - 1].second) == offset))
	m_padding[m_padding.length () - 1].second += psz;
      else
	m_padding.safe_push (std::make_pair (m_data_until, psz));
    }
}

/* Return true iff TYPE is totally scalarizable - i.e. a RECORD_TYPE or
   fixed-length ARRAY_TYPE with fields that are either of gimple register types
   (excluding bit-fields) or (recursively) scalarizable types.  CONST_DECL must
   be true if we are considering a decl from constant pool.  If it is false,
   char arrays will be refused.

   TOTAL_OFFSET is the offset of TYPE within any outer type that is being
   examined.

   If PC is non-NULL, collect padding information into the vector within the
   structure.  The information is however only complete if the function returns
   true and does not contain any padding at its end.  */

static bool
totally_scalarizable_type_p (tree type, bool const_decl,
			     HOST_WIDE_INT total_offset,
			     sra_padding_collecting *pc)
{
  if (is_gimple_reg_type (type))
    {
      if (pc)
	{
	  pc->record_padding (total_offset);
	  pc->m_data_until = total_offset + tree_to_shwi (TYPE_SIZE (type));
	}
      return true;
    }
  if (type_contains_placeholder_p (type))
    return false;

  bool have_predecessor_field = false;
  HOST_WIDE_INT prev_pos = 0;

  switch (TREE_CODE (type))
  {
  case RECORD_TYPE:
    for (tree fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
      if (TREE_CODE (fld) == FIELD_DECL)
	{
	  tree ft = TREE_TYPE (fld);

	  if (!DECL_SIZE (fld))
	    return false;
	  if (zerop (DECL_SIZE (fld)))
	    continue;

	  HOST_WIDE_INT pos = int_bit_position (fld);
	  if (have_predecessor_field
	      && pos <= prev_pos)
	    return false;

	  have_predecessor_field = true;
	  prev_pos = pos;

	  if (DECL_BIT_FIELD (fld))
	    return false;

	  if (!totally_scalarizable_type_p (ft, const_decl, total_offset + pos,
					    pc))
	    return false;
	}

    return true;

  case ARRAY_TYPE:
    {
      HOST_WIDE_INT min_elem_size;
      if (const_decl)
	min_elem_size = 0;
      else
	min_elem_size = BITS_PER_UNIT;

      if (TYPE_DOMAIN (type) == NULL_TREE
	  || !tree_fits_shwi_p (TYPE_SIZE (type))
	  || !tree_fits_shwi_p (TYPE_SIZE (TREE_TYPE (type)))
	  || (tree_to_shwi (TYPE_SIZE (TREE_TYPE (type))) <= min_elem_size)
	  || !tree_fits_shwi_p (TYPE_MIN_VALUE (TYPE_DOMAIN (type))))
	return false;
      if (tree_to_shwi (TYPE_SIZE (type)) == 0
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == NULL_TREE)
	/* Zero-element array, should not prevent scalarization.  */
	;
      else if ((tree_to_shwi (TYPE_SIZE (type)) <= 0)
	       || !tree_fits_shwi_p (TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	/* Variable-length array, do not allow scalarization.  */
	return false;

      unsigned old_padding_len = 0;
      if (pc)
	old_padding_len = pc->m_padding.length ();
      tree elem = TREE_TYPE (type);
      if (!totally_scalarizable_type_p (elem, const_decl, total_offset, pc))
	return false;
      if (pc)
	{
	  unsigned new_padding_len = pc->m_padding.length ();
	  HOST_WIDE_INT el_size;
	  offset_int idx, max;
	  if (!prepare_iteration_over_array_elts (type, &el_size, &idx, &max))
	    return true;
	  pc->record_padding (total_offset + el_size);
	  ++idx;
	  for (HOST_WIDE_INT pos = total_offset + el_size;
	       idx <= max;
	       pos += el_size, ++idx)
	    {
	      for (unsigned i = old_padding_len; i < new_padding_len; i++)
		{
		  HOST_WIDE_INT pp
		    = pos + pc->m_padding[i].first - total_offset;
		  HOST_WIDE_INT psz = pc->m_padding[i].second;
		  pc->m_padding.safe_push (std::make_pair (pp, psz));
		}
	    }
	  pc->m_data_until = total_offset + tree_to_shwi (TYPE_SIZE (type));
	}
      return true;
    }
  default:
    return false;
  }
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

/* Return true if REF contains a VIEW_CONVERT_EXPR or a COMPONENT_REF with a
   bit-field field declaration.  If TYPE_CHANGING_P is non-NULL, set the bool
   it points to will be set if REF contains any of the above or a MEM_REF
   expression that effectively performs type conversion.  */

static bool
contains_vce_or_bfcref_p (const_tree ref, bool *type_changing_p = NULL)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == VIEW_CONVERT_EXPR
	  || (TREE_CODE (ref) == COMPONENT_REF
	      && DECL_BIT_FIELD (TREE_OPERAND (ref, 1))))
	{
	  if (type_changing_p)
	    *type_changing_p = true;
	  return true;
	}
      ref = TREE_OPERAND (ref, 0);
    }

  if (!type_changing_p
      || TREE_CODE (ref) != MEM_REF
      || TREE_CODE (TREE_OPERAND (ref, 0)) != ADDR_EXPR)
    return false;

  tree mem = TREE_OPERAND (TREE_OPERAND (ref, 0), 0);
  if (TYPE_MAIN_VARIANT (TREE_TYPE (ref))
      != TYPE_MAIN_VARIANT (TREE_TYPE (mem)))
    *type_changing_p = true;

  return false;
}

/* Search the given tree for a declaration by skipping handled components and
   exclude it from the candidates.  */

static void
disqualify_base_of_expr (tree t, const char *reason)
{
  t = get_base_address (t);
  if (t && DECL_P (t))
    disqualify_candidate (t, reason);
}

/* Return true if the BIT_FIELD_REF read EXPR is handled by SRA.  */

static bool
sra_handled_bf_read_p (tree expr)
{
  uint64_t size, offset;
  if (bit_field_size (expr).is_constant (&size)
      && bit_field_offset (expr).is_constant (&offset)
      && size % BITS_PER_UNIT == 0
      && offset % BITS_PER_UNIT == 0
      && pow2p_hwi (size))
    return true;
  return false;
}

/* Scan expression EXPR and create access structures for all accesses to
   candidates for scalarization.  Return the created access or NULL if none is
   created.  */

static struct access *
build_access_from_expr_1 (tree expr, gimple *stmt, bool write)
{
  /* We only allow ADDR_EXPRs in arguments of function calls and those must
     have been dealt with in build_access_from_call_arg.  Any other address
     taking should have been caught by scan_visit_addr.   */
  if (TREE_CODE (expr) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (expr, 0));
      gcc_assert (!DECL_P (base)
		  || !bitmap_bit_p (candidate_bitmap, DECL_UID (base)));
      return NULL;
    }

  struct access *ret = NULL;
  bool partial_ref;

  if ((TREE_CODE (expr) == BIT_FIELD_REF
       && (write || !sra_handled_bf_read_p (expr)))
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == REALPART_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      partial_ref = true;
    }
  else
    partial_ref = false;

  if (storage_order_barrier_p (expr))
    {
      disqualify_base_of_expr (expr, "storage order barrier.");
      return NULL;
    }

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
  if (TREE_THIS_VOLATILE (expr))
    {
      disqualify_base_of_expr (expr, "part of a volatile reference.");
      return NULL;
    }

  switch (TREE_CODE (expr))
    {
    case MEM_REF:
      if (TREE_CODE (TREE_OPERAND (expr, 0)) != ADDR_EXPR)
	return NULL;
      /* fall through */
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case BIT_FIELD_REF:
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
build_access_from_expr (tree expr, gimple *stmt, bool write)
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

enum out_edge_check { SRA_OUTGOING_EDGES_UNCHECKED, SRA_OUTGOING_EDGES_OK,
		      SRA_OUTGOING_EDGES_FAIL };

/* Return true if STMT terminates BB and there is an abnormal edge going out of
   the BB and remember the decision in OE_CHECK.  */

static bool
abnormal_edge_after_stmt_p (gimple *stmt, enum out_edge_check *oe_check)
{
  if (*oe_check == SRA_OUTGOING_EDGES_OK)
    return false;
  if (*oe_check == SRA_OUTGOING_EDGES_FAIL)
    return true;
  if (stmt_ends_bb_p (stmt))
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    *oe_check = SRA_OUTGOING_EDGES_FAIL;
	    return true;
	  }
    }
  *oe_check = SRA_OUTGOING_EDGES_OK;
  return false;
}

/* Scan expression EXPR which is an argument of a call and create access
   structures for all accesses to candidates for scalarization.  Return true
   if any access has been inserted.  STMT must be the statement from which the
   expression is taken.  CAN_BE_RETURNED must be true if call argument flags
   do not rule out that the argument is directly returned.  OE_CHECK is used
   to remember result of a test for abnormal outgoing edges after this
   statement.  */

static bool
build_access_from_call_arg (tree expr, gimple *stmt, bool can_be_returned,
			    enum out_edge_check *oe_check)
{
  if (gimple_call_flags (stmt) & ECF_RETURNS_TWICE)
    {
      tree base = expr;
      if (TREE_CODE (expr) == ADDR_EXPR)
	base = get_base_address (TREE_OPERAND (expr, 0));
      disqualify_base_of_expr (base, "Passed to a returns_twice call.");
      return false;
    }

  if (TREE_CODE (expr) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (expr, 0));

      if (can_be_returned)
	{
	  disqualify_base_of_expr (base, "Address possibly returned, "
				   "leading to an alis SRA may not know.");
	  return false;
	}
      if (abnormal_edge_after_stmt_p (stmt, oe_check))
	{
	  disqualify_base_of_expr (base, "May lead to need to add statements "
				   "to abnormal edge.");
	  return false;
	}

      bool read =  build_access_from_expr (base, stmt, false);
      bool write =  build_access_from_expr (base, stmt, true);
      if (read || write)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Allowed ADDR_EXPR of ");
	      print_generic_expr (dump_file, base);
	      fprintf (dump_file, " because of ");
	      print_gimple_stmt (dump_file, stmt, 0);
	      fprintf (dump_file, "\n");
	    }
	  bitmap_set_bit (passed_by_ref_in_call, DECL_UID (base));
	  return true;
	}
      else
	return false;
    }

  return build_access_from_expr (expr, stmt, false);
}


/* Return the single non-EH successor edge of BB or NULL if there is none or
   more than one.  */

static edge
single_non_eh_succ (basic_block bb)
{
  edge e, res = NULL;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_EH))
      {
	if (res)
	  return NULL;
	res = e;
      }

  return res;
}

/* Disqualify LHS and RHS for scalarization if STMT has to terminate its BB and
   there is no alternative spot where to put statements SRA might need to
   generate after it.  The spot we are looking for is an edge leading to a
   single non-EH successor, if it exists and is indeed single.  RHS may be
   NULL, in that case ignore it.  */

static bool
disqualify_if_bad_bb_terminating_stmt (gimple *stmt, tree lhs, tree rhs)
{
  if (stmt_ends_bb_p (stmt))
    {
      if (single_non_eh_succ (gimple_bb (stmt)))
	return false;

      disqualify_base_of_expr (lhs, "LHS of a throwing stmt.");
      if (rhs)
	disqualify_base_of_expr (rhs, "RHS of a throwing stmt.");
      return true;
    }
  return false;
}

/* Return true if the nature of BASE is such that it contains data even if
   there is no write to it in the function.  */

static bool
comes_initialized_p (tree base)
{
  return TREE_CODE (base) == PARM_DECL || constant_decl_p (base);
}

/* Scan expressions occurring in STMT, create access structures for all accesses
   to candidates for scalarization and remove those candidates which occur in
   statements or expressions that prevent them from being split apart.  Return
   true if any access has been inserted.  */

static bool
build_accesses_from_assign (gimple *stmt)
{
  tree lhs, rhs;
  struct access *lacc, *racc;

  if (!gimple_assign_single_p (stmt)
      /* Scope clobbers don't influence scalarization.  */
      || gimple_clobber_p (stmt))
    return false;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);

  if (disqualify_if_bad_bb_terminating_stmt (stmt, lhs, rhs))
    return false;

  racc = build_access_from_expr_1 (rhs, stmt, false);
  lacc = build_access_from_expr_1 (lhs, stmt, true);

  if (lacc)
    {
      lacc->grp_assignment_write = 1;
      if (storage_order_barrier_p (rhs))
	lacc->grp_unscalarizable_region = 1;

      if (should_scalarize_away_bitmap && !is_gimple_reg_type (lacc->type))
	{
	  bool type_changing_p = false;
	  contains_vce_or_bfcref_p (lhs, &type_changing_p);
	  if (type_changing_p)
	    bitmap_set_bit (cannot_scalarize_away_bitmap,
			    DECL_UID (lacc->base));
	}
    }

  if (racc)
    {
      racc->grp_assignment_read = 1;
      if (should_scalarize_away_bitmap && !is_gimple_reg_type (racc->type))
	{
	  bool type_changing_p = false;
	  contains_vce_or_bfcref_p (rhs, &type_changing_p);

	  if (type_changing_p || gimple_has_volatile_ops (stmt))
	    bitmap_set_bit (cannot_scalarize_away_bitmap,
			    DECL_UID (racc->base));
	  else
	    bitmap_set_bit (should_scalarize_away_bitmap,
			    DECL_UID (racc->base));
	}
      if (storage_order_barrier_p (lhs))
	racc->grp_unscalarizable_region = 1;
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

      link = assign_link_pool.allocate ();
      memset (link, 0, sizeof (struct assign_link));

      link->lacc = lacc;
      link->racc = racc;
      add_link_to_rhs (racc, link);
      add_link_to_lhs (lacc, link);
      add_access_to_rhs_work_queue (racc);
      add_access_to_lhs_work_queue (lacc);

      /* Let's delay marking the areas as written until propagation of accesses
	 across link, unless the nature of rhs tells us that its data comes
	 from elsewhere.  */
      if (!comes_initialized_p (racc->base))
	lacc->write = false;
    }

  return lacc || racc;
}

/* Callback of walk_stmt_load_store_addr_ops visit_addr used to detect taking
   addresses of candidates a places which are not call arguments.  Such
   candidates are disqalified from SRA.  This also applies to GIMPLE_ASM
   operands with memory constrains which cannot be scalarized.  */

static bool
scan_visit_addr (gimple *, tree op, tree, void *)
{
  op = get_base_address (op);
  if (op
      && DECL_P (op))
    disqualify_candidate (op, "Address taken in a non-call-argument context.");

  return false;
}

/* Scan function and look for interesting expressions and create access
   structures for them.  Return true iff any access is created.  */

static bool
scan_function (void)
{
  basic_block bb;
  bool ret = false;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	walk_stmt_load_store_addr_ops (gsi_stmt (gsi), NULL, NULL, NULL,
				       scan_visit_addr);

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree t;
	  unsigned i;

	  if (gimple_code (stmt) != GIMPLE_CALL)
	    walk_stmt_load_store_addr_ops (stmt, NULL, NULL, NULL,
					   scan_visit_addr);

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval (as_a <greturn *> (stmt));
	      if (t != NULL_TREE)
		ret |= build_access_from_expr (t, stmt, false);
	      break;

	    case GIMPLE_ASSIGN:
	      ret |= build_accesses_from_assign (stmt);
	      break;

	    case GIMPLE_CALL:
	      {
		enum out_edge_check oe_check = SRA_OUTGOING_EDGES_UNCHECKED;
		gcall *call = as_a <gcall *> (stmt);
		for (i = 0; i < gimple_call_num_args (call); i++)
		  {
		    bool can_be_returned;
		    if (gimple_call_lhs (call))
		      {
			int af = gimple_call_arg_flags (call, i);
			can_be_returned = !(af & EAF_NOT_RETURNED_DIRECTLY);
		      }
		    else
		      can_be_returned = false;
		    ret |= build_access_from_call_arg (gimple_call_arg (call,
									i),
						       stmt, can_be_returned,
						       &oe_check);
		  }
		if (gimple_call_chain(stmt))
		  ret |= build_access_from_call_arg (gimple_call_chain(call),
						     stmt, false,  &oe_check);
	      }

	      t = gimple_call_lhs (stmt);
	      if (t && !disqualify_if_bad_bb_terminating_stmt (stmt, t, NULL))
		{
		  /* If the STMT is a call to DEFERRED_INIT, avoid setting
		     cannot_scalarize_away_bitmap.  */
		  if (gimple_call_internal_p (stmt, IFN_DEFERRED_INIT))
		    ret |= !!build_access_from_expr_1 (t, stmt, true);
		  else
		    ret |= build_access_from_expr (t, stmt, true);
		}
	      break;

	    case GIMPLE_ASM:
	      {
		gasm *asm_stmt = as_a <gasm *> (stmt);
		if (stmt_ends_bb_p (asm_stmt)
		    && !single_succ_p (gimple_bb (asm_stmt)))
		  {
		    for (i = 0; i < gimple_asm_ninputs (asm_stmt); i++)
		      {
			t = TREE_VALUE (gimple_asm_input_op (asm_stmt, i));
			disqualify_base_of_expr (t, "OP of asm goto.");
		      }
		    for (i = 0; i < gimple_asm_noutputs (asm_stmt); i++)
		      {
			t = TREE_VALUE (gimple_asm_output_op (asm_stmt, i));
			disqualify_base_of_expr (t, "OP of asm goto.");
		      }
		  }
		else
		  {
		    for (i = 0; i < gimple_asm_ninputs (asm_stmt); i++)
		      {
			t = TREE_VALUE (gimple_asm_input_op (asm_stmt, i));
			ret |= build_access_from_expr (t, asm_stmt, false);
		      }
		    for (i = 0; i < gimple_asm_noutputs (asm_stmt); i++)
		      {
			t = TREE_VALUE (gimple_asm_output_op (asm_stmt, i));
			ret |= build_access_from_expr (t, asm_stmt, true);
		      }
		  }
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
		   || VECTOR_TYPE_P (f2->type)))
	return 1;
      else if ((TREE_CODE (f1->type) == COMPLEX_TYPE
		|| VECTOR_TYPE_P (f1->type))
	       && TREE_CODE (f2->type) != COMPLEX_TYPE
	       && TREE_CODE (f2->type) != VECTOR_TYPE)
	return -1;
      /* Put any integral type before any non-integral type.  When splicing, we
	 make sure that those with insufficient precision and occupying the
	 same space are not scalarized.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	       && !INTEGRAL_TYPE_P (f2->type))
	return -1;
      else if (!INTEGRAL_TYPE_P (f1->type)
	       && INTEGRAL_TYPE_P (f2->type))
	return 1;
      /* Put the integral type with the bigger precision first.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	       && INTEGRAL_TYPE_P (f2->type)
	       && (TYPE_PRECISION (f2->type) != TYPE_PRECISION (f1->type)))
	return TYPE_PRECISION (f2->type) - TYPE_PRECISION (f1->type);
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

    case BIT_FIELD_REF:
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
   EXP_TYPE at the given OFFSET and with storage order REVERSE.  If BASE is
   something for which get_addr_base_and_unit_offset returns NULL, gsi must
   be non-NULL and is used to insert new statements either before or below
   the current one as specified by INSERT_AFTER.  This function is not capable
   of handling bitfields.  */

tree
build_ref_for_offset (location_t loc, tree base, poly_int64 offset,
		      bool reverse, tree exp_type, gimple_stmt_iterator *gsi,
		      bool insert_after)
{
  tree prev_base = base;
  tree off;
  tree mem_ref;
  poly_int64 base_offset;
  unsigned HOST_WIDE_INT misalign;
  unsigned int align;

  /* Preserve address-space information.  */
  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (base));
  if (as != TYPE_ADDR_SPACE (exp_type))
    exp_type = build_qualified_type (exp_type,
				     TYPE_QUALS (exp_type)
				     | ENCODE_QUAL_ADDR_SPACE (as));

  poly_int64 byte_offset = exact_div (offset, BITS_PER_UNIT);
  get_object_alignment_1 (base, &align, &misalign);
  base = get_addr_base_and_unit_offset (base, &base_offset);

  /* get_addr_base_and_unit_offset returns NULL for references with a variable
     offset such as array[var_index].  */
  if (!base)
    {
      gassign *stmt;
      tree tmp, addr;

      gcc_checking_assert (gsi);
      tmp = make_ssa_name (build_pointer_type (TREE_TYPE (prev_base)));
      addr = build_fold_addr_expr (unshare_expr (prev_base));
      STRIP_USELESS_TYPE_CONVERSION (addr);
      stmt = gimple_build_assign (tmp, addr);
      gimple_set_location (stmt, loc);
      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

      off = build_int_cst (reference_alias_ptr_type (prev_base), byte_offset);
      base = tmp;
    }
  else if (TREE_CODE (base) == MEM_REF)
    {
      off = build_int_cst (TREE_TYPE (TREE_OPERAND (base, 1)),
			   base_offset + byte_offset);
      off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1), off);
      base = unshare_expr (TREE_OPERAND (base, 0));
    }
  else
    {
      off = build_int_cst (reference_alias_ptr_type (prev_base),
			   base_offset + byte_offset);
      base = build_fold_addr_expr (unshare_expr (base));
    }

  unsigned int align_bound = known_alignment (misalign + offset);
  if (align_bound != 0)
    align = MIN (align, align_bound);
  if (align != TYPE_ALIGN (exp_type))
    exp_type = build_aligned_type (exp_type, align);

  mem_ref = fold_build2_loc (loc, MEM_REF, exp_type, base, off);
  REF_REVERSE_STORAGE_ORDER (mem_ref) = reverse;
  if (TREE_THIS_VOLATILE (prev_base))
    TREE_THIS_VOLATILE (mem_ref) = 1;
  if (TREE_SIDE_EFFECTS (prev_base))
    TREE_SIDE_EFFECTS (mem_ref) = 1;
  return mem_ref;
}

/* Construct and return a memory reference that is equal to a portion of
   MODEL->expr but is based on BASE.  If this cannot be done, return NULL.  */

static tree
build_reconstructed_reference (location_t, tree base, struct access *model)
{
  tree expr = model->expr;
  /* We have to make sure to start just below the outermost union.  */
  tree start_expr = expr;
  while (handled_component_p (expr))
    {
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == UNION_TYPE)
	start_expr = expr;
      expr = TREE_OPERAND (expr, 0);
    }

  expr = start_expr;
  tree prev_expr = NULL_TREE;
  while (!types_compatible_p (TREE_TYPE (expr), TREE_TYPE (base)))
    {
      if (!handled_component_p (expr))
	return NULL_TREE;
      prev_expr = expr;
      expr = TREE_OPERAND (expr, 0);
    }

  /* Guard against broken VIEW_CONVERT_EXPRs...  */
  if (!prev_expr)
    return NULL_TREE;

  TREE_OPERAND (prev_expr, 0) = base;
  tree ref = unshare_expr (model->expr);
  TREE_OPERAND (prev_expr, 0) = expr;
  return ref;
}

/* Construct a memory reference to a part of an aggregate BASE at the given
   OFFSET and of the same type as MODEL.  In case this is a reference to a
   bit-field, the function will replicate the last component_ref of model's
   expr to access it.  INSERT_AFTER and GSI have the same meaning as in
   build_ref_for_offset, furthermore, when GSI is NULL, the function expects
   that it re-builds the entire reference from a DECL to the final access and
   so will create a MEM_REF when OFFSET does not exactly match offset of
   MODEL.  */

static tree
build_ref_for_model (location_t loc, tree base, HOST_WIDE_INT offset,
		     struct access *model, gimple_stmt_iterator *gsi,
		     bool insert_after)
{
  gcc_assert (offset >= 0);
  if (TREE_CODE (model->expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (model->expr, 1)))
    {
      /* This access represents a bit-field.  */
      tree t, exp_type, fld = TREE_OPERAND (model->expr, 1);

      offset -= int_bit_position (fld);
      exp_type = TREE_TYPE (TREE_OPERAND (model->expr, 0));
      t = build_ref_for_offset (loc, base, offset, model->reverse, exp_type,
				gsi, insert_after);
      /* The flag will be set on the record type.  */
      REF_REVERSE_STORAGE_ORDER (t) = 0;
      return fold_build3_loc (loc, COMPONENT_REF, TREE_TYPE (fld), t, fld,
			      NULL_TREE);
    }
  else
    {
      tree res;
      if (model->grp_same_access_path
	  && !TREE_THIS_VOLATILE (base)
	  && (TYPE_ADDR_SPACE (TREE_TYPE (base))
	      == TYPE_ADDR_SPACE (TREE_TYPE (model->expr)))
	  && (offset == model->offset
	      || (gsi && offset <= model->offset))
	  /* build_reconstructed_reference can still fail if we have already
	     massaged BASE because of another type incompatibility.  */
	  && (res = build_reconstructed_reference (loc, base, model)))
	return res;
      else
	return build_ref_for_offset (loc, base, offset, model->reverse,
				     model->type, gsi, insert_after);
    }
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
  poly_int64 base_offset;
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
	      if (!tr_pos || !tree_fits_uhwi_p (tr_pos))
		continue;
	      pos = tree_to_uhwi (tr_pos);
	      gcc_assert (TREE_CODE (type) == RECORD_TYPE || pos == 0);
	      tr_size = DECL_SIZE (fld);
	      if (!tr_size || !tree_fits_uhwi_p (tr_size))
		continue;
	      size = tree_to_uhwi (tr_size);
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
	  if (!tr_size || !tree_fits_uhwi_p (tr_size))
	    return false;
	  el_size = tree_to_uhwi (tr_size);

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

/* Print message to dump file why a variable was rejected. */

static void
reject (tree var, const char *msg)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Rejected (%d): %s: ", DECL_UID (var), msg);
      print_generic_expr (dump_file, var);
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

  if ((is_global_var (var)
       /* There are cases where non-addressable variables fail the
	  pt_solutions_check test, e.g in gcc.dg/uninit-40.c. */
       || (TREE_ADDRESSABLE (var)
	   && pt_solution_includes (&cfun->gimple_df->escaped_return, var))
       || (TREE_CODE (var) == RESULT_DECL
	   && !DECL_BY_REFERENCE (var)
	   && aggregate_value_p (var, current_function_decl)))
      /* Allow constant-pool entries that "need to live in memory".  */
      && !constant_decl_p (var))
    {
      reject (var, "needs to live in memory and escapes or global");
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
  if (!tree_fits_shwi_p (TYPE_SIZE (type)))
    {
      reject (var, "type size not fixed");
      return false;
    }
  if (tree_to_shwi (TYPE_SIZE (type)) == 0)
    {
      reject (var, "type size is zero");
      return false;
    }
  if (type_internals_preclude_sra_p (type, &msg))
    {
      reject (var, msg);
      return false;
    }
  if (/* Fix for PR 41089.  tree-stdarg.cc needs to have va_lists intact but
	 we also want to schedule it rather late.  Thus we ignore it in
	 the early pass. */
      (sra_mode == SRA_MODE_EARLY_INTRA
       && is_va_list_type (type)))
    {
      reject (var, "is va_list");
      return false;
    }

  bitmap_set_bit (candidate_bitmap, DECL_UID (var));
  slot = candidates->find_slot_with_hash (var, DECL_UID (var), INSERT);
  *slot = var;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Candidate (%d): ", DECL_UID (var));
      print_generic_expr (dump_file, var);
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
      if (!VAR_P (var))
        continue;

      ret |= maybe_add_sra_candidate (var);
    }

  return ret;
}

/* Return true if EXP is a reference chain of COMPONENT_REFs and AREAY_REFs
   ending either with a DECL or a MEM_REF with zero offset.  */

static bool
path_comparable_for_same_access (tree expr)
{
  while (handled_component_p (expr))
    {
      if (TREE_CODE (expr) == ARRAY_REF)
	{
	  /* SSA name indices can occur here too when the array is of sie one.
	     But we cannot just re-use array_refs with SSA names elsewhere in
	     the function, so disallow non-constant indices.  TODO: Remove this
	     limitation after teaching build_reconstructed_reference to replace
	     the index with the index type lower bound.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) != INTEGER_CST)
	    return false;
	}
      expr = TREE_OPERAND (expr, 0);
    }

  if (TREE_CODE (expr) == MEM_REF)
    {
      if (!zerop (TREE_OPERAND (expr, 1)))
	return false;
    }
  else
    gcc_assert (DECL_P (expr));

  return true;
}

/* Assuming that EXP1 consists of only COMPONENT_REFs and ARRAY_REFs, return
   true if the chain of these handled components are exactly the same as EXP2
   and the expression under them is the same DECL or an equivalent MEM_REF.
   The reference picked by compare_access_positions must go to EXP1.  */

static bool
same_access_path_p (tree exp1, tree exp2)
{
  if (TREE_CODE (exp1) != TREE_CODE (exp2))
    {
      /* Special case single-field structures loaded sometimes as the field
	 and sometimes as the structure.  If the field is of a scalar type,
	 compare_access_positions will put it into exp1.

	 TODO: The gimple register type condition can be removed if teach
	 compare_access_positions to put inner types first.  */
      if (is_gimple_reg_type (TREE_TYPE (exp1))
	  && TREE_CODE (exp1) == COMPONENT_REF
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (exp1, 0)))
	      == TYPE_MAIN_VARIANT (TREE_TYPE (exp2))))
	exp1 = TREE_OPERAND (exp1, 0);
      else
	return false;
    }

  if (!operand_equal_p (exp1, exp2, OEP_ADDRESS_OF))
    return false;

  return true;
}

/* Return true when either T1 is a type that, when loaded into a register and
   stored back to memory will yield the same bits or when both T1 and T2 are
   compatible.  */

static bool
types_risk_mangled_binary_repr_p (tree t1, tree t2)
{
  if (mode_can_transfer_bits (TYPE_MODE (t1)))
    return false;

  return !types_compatible_p (t1, t2);
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
      bool grp_partial_lhs = access->grp_partial_lhs;
      bool first_scalar = is_gimple_reg_type (access->type);
      bool unscalarizable_region = access->grp_unscalarizable_region;
      bool grp_same_access_path = true;
      bool bf_non_full_precision
	= (INTEGRAL_TYPE_P (access->type)
	   && TYPE_PRECISION (access->type) != access->size
	   && TREE_CODE (access->expr) == COMPONENT_REF
	   && DECL_BIT_FIELD (TREE_OPERAND (access->expr, 1)));

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

      if (INTEGRAL_TYPE_P (access->type)
	  && TYPE_PRECISION (access->type) != access->size
	  && bitmap_bit_p (passed_by_ref_in_call, DECL_UID (access->base)))
	{
	  /* This can lead to performance regressions because we can generate
	     excessive zero extensions.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Won't scalarize ");
	      print_generic_expr (dump_file, access->base);
	      fprintf (dump_file, "(%d), it is passed by reference to a call "
		       "and there are accesses with precision not covering "
		       "their type size.", DECL_UID (access->base));
	    }
	  return NULL;
	}

      grp_same_access_path = path_comparable_for_same_access (access->expr);

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
	  relink_to_new_repr (access, ac2);

	  /* If there are both aggregate-type and scalar-type accesses with
	     this combination of size and offset, the comparison function
	     should have put the scalars first.  */
	  gcc_assert (first_scalar || !is_gimple_reg_type (ac2->type));
	  /* It also prefers integral types to non-integral.  However, when the
	     precision of the selected type does not span the entire area and
	     should also be used for a non-integer (i.e. float), we must not
	     let that happen.  Normally analyze_access_subtree expands the type
	     to cover the entire area but for bit-fields it doesn't.  */
	  if (bf_non_full_precision && !INTEGRAL_TYPE_P (ac2->type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Cannot scalarize the following access "
			   "because insufficient precision integer type was "
			   "selected.\n  ");
		  dump_access (dump_file, access, false);
		}
	      unscalarizable_region = true;
	    }
	  else if (types_risk_mangled_binary_repr_p (access->type, ac2->type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Cannot scalarize the following access "
			   "because data would be held in a mode which is not "
			   "guaranteed to preserve all bits.\n  ");
		  dump_access (dump_file, access, false);
		}
	      unscalarizable_region = true;
	    }

	  if (grp_same_access_path
	      && !same_access_path_p (access->expr, ac2->expr))
	    grp_same_access_path = false;

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
      access->grp_hint = multiple_scalar_reads && !constant_decl_p (var);
      access->grp_partial_lhs = grp_partial_lhs;
      access->grp_unscalarizable_region = unscalarizable_region;
      access->grp_same_access_path = grp_same_access_path;

      *prev_acc_ptr = access;
      prev_acc_ptr = &access->next_grp;
    }

  gcc_assert (res == (*access_vec)[0]);
  return res;
}

/* Create a variable for the given ACCESS which determines the type, name and a
   few other properties.  Return the variable declaration and store it also to
   ACCESS->replacement.  REG_TREE is used when creating a declaration to base a
   default-definition SSA name on in order to facilitate an uninitialized
   warning.  It is used instead of the actual ACCESS type if that is not of a
   gimple register type.  */

static tree
create_access_replacement (struct access *access, tree reg_type = NULL_TREE)
{
  tree repl;

  tree type = access->type;
  if (reg_type && !is_gimple_reg_type (type))
    type = reg_type;

  if (access->grp_to_be_debug_replaced)
    {
      repl = create_tmp_var_raw (access->type);
      DECL_CONTEXT (repl) = current_function_decl;
    }
  else
    /* Drop any special alignment on the type if it's not on the main
       variant.  This avoids issues with weirdo ABIs like AAPCS.  */
    repl = create_tmp_var (build_qualified_type (TYPE_MAIN_VARIANT (type),
						 TYPE_QUALS (type)), "SR");
  if (access->grp_partial_lhs
      && is_gimple_reg_type (type))
    DECL_NOT_GIMPLE_REG_P (repl) = 1;

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
      DECL_NAMELESS (repl) = 1;
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
	suppress_warning (repl /* Be more selective! */);
      else
	copy_warning (repl, access->base);
    }
  else
    suppress_warning (repl /* Be more selective! */);

  if (dump_file)
    {
      if (access->grp_to_be_debug_replaced)
	{
	  fprintf (dump_file, "Created a debug-only replacement for ");
	  print_generic_expr (dump_file, access->base);
	  fprintf (dump_file, " offset: %u, size: %u\n",
		   (unsigned) access->offset, (unsigned) access->size);
	}
      else
	{
	  fprintf (dump_file, "Created a replacement for ");
	  print_generic_expr (dump_file, access->base);
	  fprintf (dump_file, " offset: %u, size: %u: ",
		   (unsigned) access->offset, (unsigned) access->size);
	  print_generic_expr (dump_file, repl, TDF_UID);
	  fprintf (dump_file, "\n");
	}
    }
  sra_stats.replacements++;

  return repl;
}

/* Return ACCESS scalar replacement, which must exist.  */

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
      (*access)->parent = root;
      (*access)->grp_write |= root->grp_write;

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

/* Traverse the access forest where ROOT is the first root and verify that
   various important invariants hold true.  */

DEBUG_FUNCTION void
verify_sra_access_forest (struct access *root)
{
  struct access *access = root;
  tree first_base = root->base;
  gcc_assert (DECL_P (first_base));
  do
    {
      gcc_assert (access->base == first_base);
      if (access->parent)
	gcc_assert (access->offset >= access->parent->offset
		    && access->size <= access->parent->size);
      if (access->next_sibling)
	gcc_assert (access->next_sibling->offset
		    >= access->offset + access->size);

      poly_int64 poffset, psize, pmax_size;
      bool reverse;
      tree base = get_ref_base_and_extent (access->expr, &poffset, &psize,
					   &pmax_size, &reverse);
      HOST_WIDE_INT offset, size, max_size;
      if (!poffset.is_constant (&offset)
	  || !psize.is_constant (&size)
	  || !pmax_size.is_constant (&max_size))
	gcc_unreachable ();
      gcc_assert (base == first_base);
      gcc_assert (offset == access->offset);
      gcc_assert (access->grp_unscalarizable_region
		  || access->grp_total_scalarization
		  || size == max_size);
      gcc_assert (access->grp_unscalarizable_region
		  || !is_gimple_reg_type (access->type)
		  || size == access->size);
      gcc_assert (reverse == access->reverse);

      if (access->first_child)
	{
	  gcc_assert (access->first_child->parent == access);
	  access = access->first_child;
	}
      else if (access->next_sibling)
	{
	  gcc_assert (access->next_sibling->parent == access->parent);
	  access = access->next_sibling;
	}
      else
	{
	  while (access->parent && !access->next_sibling)
	    access = access->parent;
	  if (access->next_sibling)
	    access = access->next_sibling;
	  else
	    {
	      gcc_assert (access == root);
	      root = root->next_grp;
	      access = root;
	    }
	}
    }
  while (access);
}

/* Verify access forests of all candidates with accesses by calling
   verify_access_forest on each on them.  */

DEBUG_FUNCTION void
verify_all_sra_access_forests (void)
{
  bitmap_iterator bi;
  unsigned i;
  EXECUTE_IF_SET_IN_BITMAP (candidate_bitmap, 0, i, bi)
    {
      tree var = candidate (i);
      struct access *access = get_first_repr_for_decl (var);
      if (access)
	{
	  gcc_assert (access->base == var);
	  verify_sra_access_forest (access);
	}
    }
}

/* Return true if expr contains some ARRAY_REFs into a variable bounded
   array.  */

static bool
expr_with_var_bounded_array_refs_p (tree expr)
{
  while (handled_component_p (expr))
    {
      if (TREE_CODE (expr) == ARRAY_REF
	  && !tree_fits_shwi_p (array_ref_low_bound (expr)))
	return true;
      expr = TREE_OPERAND (expr, 0);
    }
  return false;
}

/* Analyze the subtree of accesses rooted in ROOT, scheduling replacements when
   both seeming beneficial and when ALLOW_REPLACEMENTS allows it.  If TOTALLY
   is set, we are totally scalarizing the aggregate.  Also set all sorts of
   access flags appropriately along the way, notably always set grp_read and
   grp_assign_read according to MARK_READ and grp_write when MARK_WRITE is
   true.

   Creating a replacement for a scalar access is considered beneficial if its
   grp_hint ot TOTALLY is set (this means either that there is more than one
   direct read access or that we are attempting total scalarization) or
   according to the following table:

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
			bool allow_replacements, bool totally)
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
      if (!parent->grp_same_access_path)
	root->grp_same_access_path = 0;
    }

  if (root->grp_unscalarizable_region)
    allow_replacements = false;

  if (allow_replacements && expr_with_var_bounded_array_refs_p (root->expr))
    allow_replacements = false;

  if (!totally && root->grp_result_of_prop_from_lhs)
    allow_replacements = false;

  for (child = root->first_child; child; child = child->next_sibling)
    {
      hole |= covered_to < child->offset;
      sth_created |= analyze_access_subtree (child, root,
					     allow_replacements && !scalar
					     && !root->grp_partial_lhs,
					     totally);

      root->grp_unscalarized_data |= child->grp_unscalarized_data;
      if (child->grp_covered)
	covered_to += child->size;
      else
	hole = true;
    }

  if (allow_replacements && scalar && !root->first_child
      && (totally || !root->grp_total_scalarization)
      && (totally
	  || root->grp_hint
	  || ((root->grp_scalar_read || root->grp_assignment_read)
	      && (root->grp_scalar_write || root->grp_assignment_write))))
    {
      /* Always create access replacements that cover the whole access.
         For integral types this means the precision has to match.
	 Avoid assumptions based on the integral type kind, too.  */
      if (INTEGRAL_TYPE_P (root->type)
	  && ((TREE_CODE (root->type) != INTEGER_TYPE
	       && TREE_CODE (root->type) != BITINT_TYPE)
	      || TYPE_PRECISION (root->type) != root->size)
	  /* But leave bitfield accesses alone.  */
	  && (TREE_CODE (root->expr) != COMPONENT_REF
	      || !DECL_BIT_FIELD (TREE_OPERAND (root->expr, 1))))
	{
	  tree rt = root->type;
	  gcc_assert ((root->offset % BITS_PER_UNIT) == 0
		      && (root->size % BITS_PER_UNIT) == 0);
	  if (TREE_CODE (root->type) == BITINT_TYPE)
	    root->type = build_bitint_type (root->size, TYPE_UNSIGNED (rt));
	  else
	    root->type = build_nonstandard_integer_type (root->size,
							 TYPE_UNSIGNED (rt));
	  root->expr = build_ref_for_offset (UNKNOWN_LOCATION, root->base,
					     root->offset, root->reverse,
					     root->type, NULL, false);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Changing the type of a replacement for ");
	      print_generic_expr (dump_file, root->base);
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
	  && !root->grp_total_scalarization
	  && (root->grp_scalar_write || root->grp_assignment_write)
	  && !bitmap_bit_p (cannot_scalarize_away_bitmap,
			    DECL_UID (root->base)))
	{
	  gcc_checking_assert (!root->grp_scalar_read
			       && !root->grp_assignment_read);
	  sth_created = true;
	  if (MAY_HAVE_DEBUG_BIND_STMTS)
	    {
	      root->grp_to_be_debug_replaced = 1;
	      root->replacement_decl = create_access_replacement (root);
	    }
	}

      if (covered_to < limit)
	hole = true;
      if (scalar || !allow_replacements)
	root->grp_total_scalarization = 0;
    }

  if (!hole || totally)
    root->grp_covered = 1;
  else if (root->grp_write || comes_initialized_p (root->base))
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
      if (analyze_access_subtree (access, NULL, true,
				  access->grp_total_scalarization))
	ret = true;
      access = access->next_grp;
    }

  return ret;
}

/* Return true iff a potential new child of ACC at offset OFFSET and with size
   SIZE would conflict with an already existing one.  If exactly such a child
   already exists in ACC, store a pointer to it in EXACT_MATCH.  */

static bool
child_would_conflict_in_acc (struct access *acc, HOST_WIDE_INT norm_offset,
			      HOST_WIDE_INT size, struct access **exact_match)
{
  struct access *child;

  for (child = acc->first_child; child; child = child->next_sibling)
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
   Return the new access or NULL if it cannot be created.  Note that this
   access is created long after all splicing and sorting, it's not located in
   any access vector and is automatically a representative of its group.  Set
   the gpr_write flag of the new accesss if SET_GRP_WRITE is true.  */

static struct access *
create_artificial_child_access (struct access *parent, struct access *model,
				HOST_WIDE_INT new_offset,
				bool set_grp_read, bool set_grp_write)
{
  struct access **child;
  tree expr = parent->base;

  gcc_assert (!model->grp_unscalarizable_region);

  struct access *access = access_pool.allocate ();
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
  access->parent = parent;
  access->grp_read = set_grp_read;
  access->grp_write = set_grp_write;
  access->reverse = model->reverse;

  child = &parent->first_child;
  while (*child && (*child)->offset < new_offset)
    child = &(*child)->next_sibling;

  access->next_sibling = *child;
  *child = access;

  return access;
}


/* Beginning with ACCESS, traverse its whole access subtree and mark all
   sub-trees as written to.  If any of them has not been marked so previously
   and has assignment links leading from it, re-enqueue it.  */

static void
subtree_mark_written_and_rhs_enqueue (struct access *access)
{
  if (access->grp_write)
    return;
  access->grp_write = true;
  add_access_to_rhs_work_queue (access);

  struct access *child;
  for (child = access->first_child; child; child = child->next_sibling)
    subtree_mark_written_and_rhs_enqueue (child);
}

/* If there is still budget to create a propagation access for DECL, return
   true and decrement the budget.  Otherwise return false.  */

static bool
budget_for_propagation_access (tree decl)
{
  unsigned b, *p = propagation_budget->get (decl);
  if (p)
    b = *p;
  else
    b = param_sra_max_propagations;

  if (b == 0)
    return false;
  b--;

  if (b == 0 && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "The propagation budget of ");
      print_generic_expr (dump_file, decl);
      fprintf (dump_file, " (UID: %u) has been exhausted.\n", DECL_UID (decl));
    }
  propagation_budget->put (decl, b);
  return true;
}

/* Return true if ACC or any of its subaccesses has grp_child set.  */

static bool
access_or_its_child_written (struct access *acc)
{
  if (acc->grp_write)
    return true;
  for (struct access *sub = acc->first_child; sub; sub = sub->next_sibling)
    if (access_or_its_child_written (sub))
      return true;
  return false;
}

/* Propagate subaccesses and grp_write flags of RACC across an assignment link
   to LACC.  Enqueue sub-accesses as necessary so that the write flag is
   propagated transitively.  Return true if anything changed.  Additionally, if
   RACC is a scalar access but LACC is not, change the type of the latter, if
   possible.  */

static bool
propagate_subaccesses_from_rhs (struct access *lacc, struct access *racc)
{
  struct access *rchild;
  HOST_WIDE_INT norm_delta = lacc->offset - racc->offset;
  bool ret = false;

  /* IF the LHS is still not marked as being written to, we only need to do so
     if the RHS at this level actually was.  */
  if (!lacc->grp_write)
    {
      gcc_checking_assert (!comes_initialized_p (racc->base));
      if (racc->grp_write)
	{
	  subtree_mark_written_and_rhs_enqueue (lacc);
	  ret = true;
	}
    }

  if (is_gimple_reg_type (lacc->type)
      || lacc->grp_unscalarizable_region
      || racc->grp_unscalarizable_region)
    {
      if (!lacc->grp_write)
	{
	  ret = true;
	  subtree_mark_written_and_rhs_enqueue (lacc);
	}
      return ret;
    }

  if (is_gimple_reg_type (racc->type))
    {
      if (!lacc->grp_write)
	{
	  ret = true;
	  subtree_mark_written_and_rhs_enqueue (lacc);
	}
      if (!lacc->first_child
	  && !racc->first_child
	  && !types_risk_mangled_binary_repr_p (racc->type, lacc->type))
	{
	  /* We are about to change the access type from aggregate to scalar,
	     so we need to put the reverse flag onto the access, if any.  */
	  const bool reverse
	    = TYPE_REVERSE_STORAGE_ORDER (lacc->type)
	      && !POINTER_TYPE_P (racc->type)
	      && !VECTOR_TYPE_P (racc->type);
	  tree t = lacc->base;

	  lacc->type = racc->type;
	  if (build_user_friendly_ref_for_offset (&t, TREE_TYPE (t),
						  lacc->offset, racc->type))
	    {
	      lacc->expr = t;
	      lacc->grp_same_access_path = true;
	    }
	  else
	    {
	      lacc->expr = build_ref_for_model (EXPR_LOCATION (lacc->base),
						lacc->base, lacc->offset,
						racc, NULL, false);
	      if (TREE_CODE (lacc->expr) == MEM_REF)
		REF_REVERSE_STORAGE_ORDER (lacc->expr) = reverse;
	      lacc->grp_no_warning = true;
	      lacc->grp_same_access_path = false;
	    }
	  lacc->reverse = reverse;
	}
      return ret;
    }

  for (rchild = racc->first_child; rchild; rchild = rchild->next_sibling)
    {
      struct access *new_acc = NULL;
      HOST_WIDE_INT norm_offset = rchild->offset + norm_delta;

      if (child_would_conflict_in_acc (lacc, norm_offset, rchild->size,
					&new_acc))
	{
	  if (new_acc)
	    {
	      if (!new_acc->grp_write && rchild->grp_write)
		{
		  gcc_assert (!lacc->grp_write);
		  subtree_mark_written_and_rhs_enqueue (new_acc);
		  ret = true;
		}

	      rchild->grp_hint = 1;
	      new_acc->grp_hint |= new_acc->grp_read;
	      if (rchild->first_child
		  && propagate_subaccesses_from_rhs (new_acc, rchild))
		{
		  ret = 1;
		  add_access_to_rhs_work_queue (new_acc);
		}
	    }
	  else
	    {
	      if (!lacc->grp_write)
		{
		  ret = true;
		  subtree_mark_written_and_rhs_enqueue (lacc);
		}
	    }
	  continue;
	}

      if (rchild->grp_unscalarizable_region
	  || !budget_for_propagation_access (lacc->base))
	{
	  if (!lacc->grp_write && access_or_its_child_written (rchild))
	    {
	      ret = true;
	      subtree_mark_written_and_rhs_enqueue (lacc);
	    }
	  continue;
	}

      rchild->grp_hint = 1;
      /* Because get_ref_base_and_extent always includes padding in size for
	 accesses to DECLs but not necessarily for COMPONENT_REFs of the same
	 type, we might be actually attempting to here to create a child of the
	 same type as the parent.  */
      if (!types_compatible_p (lacc->type, rchild->type))
	new_acc = create_artificial_child_access (lacc, rchild, norm_offset,
						  false,
						  (lacc->grp_write
						   || rchild->grp_write));
      else
	new_acc = lacc;
      gcc_checking_assert (new_acc);
      if (racc->first_child)
	propagate_subaccesses_from_rhs (new_acc, rchild);

      add_access_to_rhs_work_queue (lacc);
      ret = true;
    }

  return ret;
}

/* Propagate subaccesses of LACC across an assignment link to RACC if they
   should inhibit total scalarization of the corresponding area.  No flags are
   being propagated in the process.  Return true if anything changed.  */

static bool
propagate_subaccesses_from_lhs (struct access *lacc, struct access *racc)
{
  if (is_gimple_reg_type (racc->type)
      || lacc->grp_unscalarizable_region
      || racc->grp_unscalarizable_region)
    return false;

  /* TODO: Do we want set some new racc flag to stop potential total
     scalarization if lacc is a scalar access (and none fo the two have
     children)?  */

  bool ret = false;
  HOST_WIDE_INT norm_delta = racc->offset - lacc->offset;
  for (struct access *lchild = lacc->first_child;
       lchild;
       lchild = lchild->next_sibling)
    {
      struct access *matching_acc = NULL;
      HOST_WIDE_INT norm_offset = lchild->offset + norm_delta;

      if (lchild->grp_unscalarizable_region
	  || child_would_conflict_in_acc (racc, norm_offset, lchild->size,
					  &matching_acc)
	  || !budget_for_propagation_access (racc->base))
	{
	  if (matching_acc
	      && propagate_subaccesses_from_lhs (lchild, matching_acc))
	    add_access_to_lhs_work_queue (matching_acc);
	  continue;
	}

      /* Because get_ref_base_and_extent always includes padding in size for
	 accesses to DECLs but not necessarily for COMPONENT_REFs of the same
	 type, we might be actually attempting to here to create a child of the
	 same type as the parent.  */
      if (!types_compatible_p (racc->type, lchild->type))
	{
	  struct access *new_acc
	    = create_artificial_child_access (racc, lchild, norm_offset,
					      true, false);
	  new_acc->grp_result_of_prop_from_lhs = 1;
	  propagate_subaccesses_from_lhs (lchild, new_acc);
	}
      else
	propagate_subaccesses_from_lhs (lchild, racc);
      ret = true;
    }
  return ret;
}

/* Propagate all subaccesses across assignment links.  */

static void
propagate_all_subaccesses (void)
{
  propagation_budget = new hash_map<tree, unsigned>;
  while (rhs_work_queue_head)
    {
      struct access *racc = pop_access_from_rhs_work_queue ();
      struct assign_link *link;

      if (racc->group_representative)
	racc= racc->group_representative;
      gcc_assert (racc->first_rhs_link);

      for (link = racc->first_rhs_link; link; link = link->next_rhs)
	{
	  struct access *lacc = link->lacc;

	  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (lacc->base)))
	    continue;
	  lacc = lacc->group_representative;

	  bool reque_parents = false;
	  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (racc->base)))
	    {
	      if (!lacc->grp_write)
		{
		  subtree_mark_written_and_rhs_enqueue (lacc);
		  reque_parents = true;
		}
	    }
	  else if (propagate_subaccesses_from_rhs (lacc, racc))
	    reque_parents = true;

	  if (reque_parents)
	    do
	      {
		add_access_to_rhs_work_queue (lacc);
		lacc = lacc->parent;
	      }
	    while (lacc);
	}
    }

  while (lhs_work_queue_head)
    {
      struct access *lacc = pop_access_from_lhs_work_queue ();
      struct assign_link *link;

      if (lacc->group_representative)
	lacc = lacc->group_representative;
      gcc_assert (lacc->first_lhs_link);

      if (!bitmap_bit_p (candidate_bitmap, DECL_UID (lacc->base)))
	continue;

      for (link = lacc->first_lhs_link; link; link = link->next_lhs)
	{
	  struct access *racc = link->racc;

	  if (racc->group_representative)
	    racc = racc->group_representative;
	  if (!bitmap_bit_p (candidate_bitmap, DECL_UID (racc->base)))
	    continue;
	  if (propagate_subaccesses_from_lhs (lacc, racc))
	    add_access_to_lhs_work_queue (racc);
	}
    }
  delete propagation_budget;
}

/* Return true if the forest beginning with ROOT does not contain
   unscalarizable regions or non-byte aligned accesses.  */

static bool
can_totally_scalarize_forest_p (struct access *root)
{
  struct access *access = root;
  do
    {
      if (access->grp_unscalarizable_region
	  || (access->offset % BITS_PER_UNIT) != 0
	  || (access->size % BITS_PER_UNIT) != 0
	  || (is_gimple_reg_type (access->type)
	      && access->first_child))
	return false;

      if (access->first_child)
	access = access->first_child;
      else if (access->next_sibling)
	access = access->next_sibling;
      else
	{
	  while (access->parent && !access->next_sibling)
	    access = access->parent;
	  if (access->next_sibling)
	    access = access->next_sibling;
	  else
	    {
	      gcc_assert (access == root);
	      root = root->next_grp;
	      access = root;
	    }
	}
    }
  while (access);
  return true;
}

/* Create and return an ACCESS in PARENT spanning from POS with SIZE, TYPE and
   reference EXPR for total scalarization purposes and mark it as such.  Within
   the children of PARENT, link it in between PTR and NEXT_SIBLING.  */

static struct access *
create_total_scalarization_access (struct access *parent, HOST_WIDE_INT pos,
				   HOST_WIDE_INT size, tree type, tree expr,
				   struct access **ptr,
				   struct access *next_sibling)
{
  struct access *access = access_pool.allocate ();
  memset (access, 0, sizeof (struct access));
  access->base = parent->base;
  access->offset = pos;
  access->size = size;
  access->expr = expr;
  access->type = type;
  access->parent = parent;
  access->grp_write = parent->grp_write;
  access->grp_total_scalarization = 1;
  access->grp_hint = 1;
  access->grp_same_access_path = path_comparable_for_same_access (expr);
  access->reverse = reverse_storage_order_for_component_p (expr);

  access->next_sibling = next_sibling;
  *ptr = access;
  return access;
}

/* Create and return an ACCESS in PARENT spanning from POS with SIZE, TYPE and
   reference EXPR for total scalarization purposes and mark it as such, link it
   at *PTR and reshape the tree so that those elements at *PTR and their
   siblings which fall within the part described by POS and SIZE are moved to
   be children of the new access.  If a partial overlap is detected, return
   NULL.  */

static struct access *
create_total_access_and_reshape (struct access *parent, HOST_WIDE_INT pos,
				 HOST_WIDE_INT size, tree type, tree expr,
				 struct access **ptr)
{
  struct access **p = ptr;

  while (*p && (*p)->offset < pos + size)
    {
      if ((*p)->offset + (*p)->size > pos + size)
	return NULL;
      p = &(*p)->next_sibling;
    }

  struct access *next_child = *ptr;
  struct access *new_acc
    = create_total_scalarization_access (parent, pos, size, type, expr,
					 ptr, *p);
  if (p != ptr)
    {
      new_acc->first_child = next_child;
      *p = NULL;
      for (struct access *a = next_child; a; a = a->next_sibling)
	a->parent = new_acc;
    }
  return new_acc;
}

static bool totally_scalarize_subtree (struct access *root);

/* Return true if INNER is either the same type as OUTER or if it is the type
   of a record field in OUTER at offset zero, possibly in nested
   sub-records.  */

static bool
access_and_field_type_match_p (tree outer, tree inner)
{
  if (TYPE_MAIN_VARIANT (outer) == TYPE_MAIN_VARIANT (inner))
    return true;
  if (TREE_CODE (outer) != RECORD_TYPE)
    return false;
  tree fld = TYPE_FIELDS (outer);
  while (fld)
    {
     if (TREE_CODE (fld) == FIELD_DECL)
       {
	if (!zerop (DECL_FIELD_OFFSET (fld)))
	  return false;
	if (TYPE_MAIN_VARIANT (TREE_TYPE (fld)) == inner)
	  return true;
	if (TREE_CODE (TREE_TYPE (fld)) == RECORD_TYPE)
	  fld = TYPE_FIELDS (TREE_TYPE (fld));
	else
	  return false;
       }
     else
       fld = DECL_CHAIN (fld);
    }
  return false;
}

/* Return type of total_should_skip_creating_access indicating whether a total
   scalarization access for a field/element should be created, whether it
   already exists or whether the entire total scalarization has to fail.  */

enum total_sra_field_state {TOTAL_FLD_CREATE, TOTAL_FLD_DONE, TOTAL_FLD_FAILED};

/* Do all the necessary steps in total scalarization when the given aggregate
   type has a TYPE at POS with the given SIZE should be put into PARENT and
   when we have processed all its siblings with smaller offsets up until and
   including LAST_SEEN_SIBLING (which can be NULL).

   If some further siblings are to be skipped, set *LAST_SEEN_SIBLING as
   appropriate.  Return TOTAL_FLD_CREATE id the caller should carry on with
   creating a new access, TOTAL_FLD_DONE if access or accesses capable of
   representing the described part of the aggregate for the purposes of total
   scalarization already exist or TOTAL_FLD_FAILED if there is a problem which
   prevents total scalarization from happening at all.  */

static enum total_sra_field_state
total_should_skip_creating_access (struct access *parent,
				   struct access **last_seen_sibling,
				   tree type, HOST_WIDE_INT pos,
				   HOST_WIDE_INT size)
{
  struct access *next_child;
  if (!*last_seen_sibling)
    next_child = parent->first_child;
  else
    next_child = (*last_seen_sibling)->next_sibling;

  /* First, traverse the chain of siblings until it points to an access with
     offset at least equal to POS.  Check all skipped accesses whether they
     span the POS boundary and if so, return with a failure.  */
  while (next_child && next_child->offset < pos)
    {
      if (next_child->offset + next_child->size > pos)
	return TOTAL_FLD_FAILED;
      *last_seen_sibling = next_child;
      next_child = next_child->next_sibling;
    }

  /* Now check whether next_child has exactly the right POS and SIZE and if so,
     whether it can represent what we need and can be totally scalarized
     itself.  */
  if (next_child && next_child->offset == pos
      && next_child->size == size)
    {
      if (!is_gimple_reg_type (next_child->type)
	  && (!access_and_field_type_match_p (type, next_child->type)
	      || !totally_scalarize_subtree (next_child)))
	return TOTAL_FLD_FAILED;

      *last_seen_sibling = next_child;
      return TOTAL_FLD_DONE;
    }

  /* If the child we're looking at would partially overlap, we just cannot
     totally scalarize.  */
  if (next_child
      && next_child->offset < pos + size
      && next_child->offset + next_child->size > pos + size)
    return TOTAL_FLD_FAILED;

  if (is_gimple_reg_type (type))
    {
      /* We don't scalarize accesses that are children of other scalar type
	 accesses, so if we go on and create an access for a register type,
	 there should not be any pre-existing children.  There are rare cases
	 where the requested type is a vector but we already have register
	 accesses for all its elements which is equally good.  Detect that
	 situation or whether we need to bail out.  */

      HOST_WIDE_INT covered = pos;
      bool skipping = false;
      while (next_child
	     && next_child->offset + next_child->size <= pos + size)
	{
	  if (next_child->offset != covered
	      || !is_gimple_reg_type (next_child->type))
	    return TOTAL_FLD_FAILED;

	  covered += next_child->size;
	  *last_seen_sibling = next_child;
	  next_child = next_child->next_sibling;
	  skipping = true;
	}

      if (skipping)
	{
	  if (covered != pos + size)
	    return TOTAL_FLD_FAILED;
	  else
	    return TOTAL_FLD_DONE;
	}
    }

  return TOTAL_FLD_CREATE;
}

/* Go over sub-tree rooted in ROOT and attempt to create scalar accesses
   spanning all uncovered areas covered by ROOT, return false if the attempt
   failed.  All created accesses will have grp_unscalarizable_region set (and
   should be ignored if the function returns false).  */

static bool
totally_scalarize_subtree (struct access *root)
{
  gcc_checking_assert (!root->grp_unscalarizable_region);
  gcc_checking_assert (!is_gimple_reg_type (root->type));

  struct access *last_seen_sibling = NULL;

  switch (TREE_CODE (root->type))
    {
    case RECORD_TYPE:
      for (tree fld = TYPE_FIELDS (root->type); fld; fld = DECL_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL)
	  {
	    tree ft = TREE_TYPE (fld);
	    HOST_WIDE_INT fsize = tree_to_uhwi (DECL_SIZE (fld));
	    if (!fsize)
	      continue;

	    HOST_WIDE_INT pos = root->offset + int_bit_position (fld);
	    if (pos + fsize > root->offset + root->size)
	      return false;
	    enum total_sra_field_state
	      state = total_should_skip_creating_access (root,
							 &last_seen_sibling,
							 ft, pos, fsize);
	    switch (state)
	      {
	      case TOTAL_FLD_FAILED:
		return false;
	      case TOTAL_FLD_DONE:
		continue;
	      case TOTAL_FLD_CREATE:
		break;
	      default:
		gcc_unreachable ();
	      }

	    struct access **p = (last_seen_sibling
				 ? &last_seen_sibling->next_sibling
				 : &root->first_child);
	    tree nref = build3 (COMPONENT_REF, ft, root->expr, fld, NULL_TREE);
	    struct access *new_child
	      = create_total_access_and_reshape (root, pos, fsize, ft, nref, p);
	    if (!new_child)
	      return false;

	    if (!is_gimple_reg_type (ft)
		&& !totally_scalarize_subtree (new_child))
	      return false;
	    last_seen_sibling = new_child;
	  }
      break;
    case ARRAY_TYPE:
      {
	tree elemtype = TREE_TYPE (root->type);
	HOST_WIDE_INT el_size;
	offset_int idx, max;
	if (!prepare_iteration_over_array_elts (root->type, &el_size,
						&idx, &max))
	  break;

	for (HOST_WIDE_INT pos = root->offset;
	     idx <= max;
	     pos += el_size, ++idx)
	  {
	    enum total_sra_field_state
	      state = total_should_skip_creating_access (root,
							 &last_seen_sibling,
							 elemtype, pos,
							 el_size);
	    switch (state)
	      {
	      case TOTAL_FLD_FAILED:
		return false;
	      case TOTAL_FLD_DONE:
		continue;
	      case TOTAL_FLD_CREATE:
		break;
	      default:
		gcc_unreachable ();
	      }

	    struct access **p = (last_seen_sibling
				 ? &last_seen_sibling->next_sibling
				 : &root->first_child);
	    tree nref = build4 (ARRAY_REF, elemtype, root->expr,
				wide_int_to_tree (TYPE_DOMAIN (root->type),
						  idx),
				NULL_TREE, NULL_TREE);
	    struct access *new_child
	      = create_total_access_and_reshape (root, pos, el_size, elemtype,
						 nref, p);
	    if (!new_child)
	      return false;

	    if (!is_gimple_reg_type (elemtype)
		&& !totally_scalarize_subtree (new_child))
	      return false;
	    last_seen_sibling = new_child;
	  }
      }
      break;
    default:
      gcc_unreachable ();
    }
  return true;
}

/* Get the total total scalarization size limit in the current function.  */

unsigned HOST_WIDE_INT
sra_get_max_scalarization_size (void)
{
  bool optimize_speed_p = !optimize_function_for_size_p (cfun);
  /* If the user didn't set PARAM_SRA_MAX_SCALARIZATION_SIZE_<...>,
     fall back to a target default.  */
  unsigned HOST_WIDE_INT max_scalarization_size
    = get_move_ratio (optimize_speed_p) * UNITS_PER_WORD;

  if (optimize_speed_p)
    {
      if (OPTION_SET_P (param_sra_max_scalarization_size_speed))
	max_scalarization_size = param_sra_max_scalarization_size_speed;
    }
  else
    {
      if (OPTION_SET_P (param_sra_max_scalarization_size_size))
	max_scalarization_size = param_sra_max_scalarization_size_size;
    }
  max_scalarization_size *= BITS_PER_UNIT;
  return max_scalarization_size;
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
  unsigned i;

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

  unsigned HOST_WIDE_INT max_scalarization_size
    = sra_get_max_scalarization_size ();
  EXECUTE_IF_SET_IN_BITMAP (candidate_bitmap, 0, i, bi)
    if (bitmap_bit_p (should_scalarize_away_bitmap, i)
	&& !bitmap_bit_p (cannot_scalarize_away_bitmap, i))
      {
	tree var = candidate (i);
	if (!VAR_P (var))
	  continue;

	if (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (var))) > max_scalarization_size)
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "Too big to totally scalarize: ");
		print_generic_expr (dump_file, var);
		fprintf (dump_file, " (UID: %u)\n", DECL_UID (var));
	      }
	    continue;
	  }

	bool all_types_ok = true;
	for (struct access *access = get_first_repr_for_decl (var);
	     access;
	     access = access->next_grp)
	  if (!can_totally_scalarize_forest_p (access)
	      || !totally_scalarizable_type_p (access->type,
					       constant_decl_p (var),
					       0, nullptr))
	    {
	      all_types_ok = false;
	      break;
	    }
	if (!all_types_ok)
	  continue;

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "Will attempt to totally scalarize ");
	    print_generic_expr (dump_file, var);
	    fprintf (dump_file, " (UID: %u): \n", DECL_UID (var));
	  }
	bool scalarized = true;
	for (struct access *access = get_first_repr_for_decl (var);
	     access;
	     access = access->next_grp)
	  if (!is_gimple_reg_type (access->type)
	      && !totally_scalarize_subtree (access))
	    {
	      scalarized = false;
	      break;
	    }

	if (scalarized)
	  for (struct access *access = get_first_repr_for_decl (var);
	       access;
	       access = access->next_grp)
	    access->grp_total_scalarization = true;
      }

  if (flag_checking)
    verify_all_sra_access_forests ();

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
	      print_generic_expr (dump_file, var);
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
  /* Never write anything into constant pool decls.  See PR70602.  */
  if (!write && constant_decl_p (agg))
    return;
  do
    {
      if (chunk_size && access->offset >= start_offset + chunk_size)
	return;

      if (access->grp_to_be_replaced
	  && (chunk_size == 0
	      || access->offset + access->size > start_offset))
	{
	  tree expr, repl = get_access_replacement (access);
	  gassign *stmt;

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
	      suppress_warning (repl /* Be more selective! */);
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
	  gdebug *ds;
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
   root of the subtree to be processed.  GSI is the statement iterator used
   for inserting statements which are added after the current statement if
   INSERT_AFTER is true or before it otherwise.  */

static void
init_subtree_with_zero (struct access *access, gimple_stmt_iterator *gsi,
			bool insert_after, location_t loc)

{
  struct access *child;

  if (access->grp_to_be_replaced)
    {
      gassign *stmt;

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
      gdebug *ds
	= gimple_build_debug_bind (get_access_replacement (access),
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

/* Clobber all scalar replacements in an access subtree.  ACCESS is the
   root of the subtree to be processed.  GSI is the statement iterator used
   for inserting statements which are added after the current statement if
   INSERT_AFTER is true or before it otherwise.  */

static void
clobber_subtree (struct access *access, gimple_stmt_iterator *gsi,
		bool insert_after, location_t loc)

{
  struct access *child;

  if (access->grp_to_be_replaced)
    {
      tree rep = get_access_replacement (access);
      tree clobber = build_clobber (access->type);
      gimple *stmt = gimple_build_assign (rep, clobber);

      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
      update_stmt (stmt);
      gimple_set_location (stmt, loc);
    }

  for (child = access->first_child; child; child = child->next_sibling)
    clobber_subtree (child, gsi, insert_after, loc);
}

/* Search for an access representative for the given expression EXPR and
   return it or NULL if it cannot be found.  */

static struct access *
get_access_for_expr (tree expr)
{
  poly_int64 poffset, psize, pmax_size;
  HOST_WIDE_INT offset, max_size;
  tree base;
  bool reverse;

  /* FIXME: This should not be necessary but Ada produces V_C_Es with a type of
     a different size than the size of its argument and we need the latter
     one.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    expr = TREE_OPERAND (expr, 0);

  base = get_ref_base_and_extent (expr, &poffset, &psize, &pmax_size,
				  &reverse);
  if (!known_size_p (pmax_size)
      || !pmax_size.is_constant (&max_size)
      || !poffset.is_constant (&offset)
      || !DECL_P (base))
    return NULL;

  if (tree basesize = DECL_SIZE (base))
    {
      poly_int64 sz;
      if (offset < 0
	  || !poly_int_tree_p (basesize, &sz)
	  || known_le (sz, offset))
	return NULL;
    }

  if (max_size == 0
      || !bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

  return get_var_base_offset_size_access (base, offset, max_size);
}

/* Replace the expression EXPR with a scalar replacement if there is one and
   generate other statements to do type conversion or subtree copying if
   necessary.  WRITE is true if the expression is being written to (it is on a
   LHS of a statement or output in an assembly statement).  STMT_GSI is used to
   place newly created statements before the processed statement, REFRESH_GSI
   is used to place them afterwards - unless the processed statement must end a
   BB in which case it is placed on the outgoing non-EH edge.  REFRESH_GSI and
   is then used to continue iteration over the BB.  If sra_modify_expr is
   called only once with WRITE equal to true on a given statement, both
   iterator parameters can point to the same one.  */

static bool
sra_modify_expr (tree *expr, bool write, gimple_stmt_iterator *stmt_gsi,
		 gimple_stmt_iterator *refresh_gsi)
{
  location_t loc;
  struct access *access;
  tree type, bfr, orig_expr;
  bool partial_cplx_access = false;

  if (TREE_CODE (*expr) == BIT_FIELD_REF
      && (write || !sra_handled_bf_read_p (*expr)))
    {
      bfr = *expr;
      expr = &TREE_OPERAND (*expr, 0);
    }
  else
    bfr = NULL_TREE;

  if (TREE_CODE (*expr) == REALPART_EXPR || TREE_CODE (*expr) == IMAGPART_EXPR)
    {
      expr = &TREE_OPERAND (*expr, 0);
      partial_cplx_access = true;
    }
  access = get_access_for_expr (*expr);
  if (!access)
    return false;
  type = TREE_TYPE (*expr);
  orig_expr = *expr;

  loc = gimple_location (gsi_stmt (*stmt_gsi));
  gimple_stmt_iterator alt_gsi = gsi_none ();
  if (write && stmt_ends_bb_p (gsi_stmt (*stmt_gsi)))
    {
      alt_gsi = gsi_start_edge (single_non_eh_succ (gsi_bb (*stmt_gsi)));
      refresh_gsi = &alt_gsi;
    }

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
      if (!bfr && !useless_type_conversion_p (type, access->type))
	{
	  tree ref;

	  ref = build_ref_for_model (loc, orig_expr, 0, access, stmt_gsi,
				     false);

	  if (partial_cplx_access)
	    {
	    /* VIEW_CONVERT_EXPRs in partial complex access are always fine in
	       the case of a write because in such case the replacement cannot
	       be a gimple register.  In the case of a load, we have to
	       differentiate in between a register an non-register
	       replacement.  */
	      tree t = build1 (VIEW_CONVERT_EXPR, type, repl);
	      gcc_checking_assert (!write || access->grp_partial_lhs);
	      if (!access->grp_partial_lhs)
		{
		  tree tmp = make_ssa_name (type);
		  gassign *stmt = gimple_build_assign (tmp, t);
		  /* This is always a read. */
		  gsi_insert_before (stmt_gsi, stmt, GSI_SAME_STMT);
		  t = tmp;
		}
	      *expr = t;
	    }
	  else if (write)
	    {
	      gassign *stmt;

	      if (access->grp_partial_lhs)
		ref = force_gimple_operand_gsi (refresh_gsi, ref, true,
						NULL_TREE, false, GSI_NEW_STMT);
	      stmt = gimple_build_assign (repl, ref);
	      gimple_set_location (stmt, loc);
	      gsi_insert_after (refresh_gsi, stmt, GSI_NEW_STMT);
	    }
	  else
	    {
	      gassign *stmt;

	      if (access->grp_partial_lhs)
		repl = force_gimple_operand_gsi (stmt_gsi, repl, true,
						 NULL_TREE, true,
						 GSI_SAME_STMT);
	      stmt = gimple_build_assign (ref, repl);
	      gimple_set_location (stmt, loc);
	      gsi_insert_before (stmt_gsi, stmt, GSI_SAME_STMT);
	    }
	}
      else
	{
	  /* If we are going to replace a scalar field in a structure with
	     reverse storage order by a stand-alone scalar, we are going to
	     effectively byte-swap the scalar and we also need to byte-swap
	     the portion of it represented by the bit-field.  */
	  if (bfr && REF_REVERSE_STORAGE_ORDER (bfr))
	    {
	      REF_REVERSE_STORAGE_ORDER (bfr) = 0;
	      TREE_OPERAND (bfr, 2)
		= size_binop (MINUS_EXPR, TYPE_SIZE (TREE_TYPE (repl)),
			      size_binop (PLUS_EXPR, TREE_OPERAND (bfr, 1),
						     TREE_OPERAND (bfr, 2)));
	    }

	  *expr = repl;
	}

      sra_stats.exprs++;
    }
  else if (write && access->grp_to_be_debug_replaced)
    {
      gdebug *ds = gimple_build_debug_bind (get_access_replacement (access),
					    NULL_TREE,
					    gsi_stmt (*stmt_gsi));
      gsi_insert_after (stmt_gsi, ds, GSI_NEW_STMT);
    }

  if (access->first_child && !TREE_READONLY (access->base))
    {
      HOST_WIDE_INT start_offset, chunk_size;
      if (bfr
	  && tree_fits_uhwi_p (TREE_OPERAND (bfr, 1))
	  && tree_fits_uhwi_p (TREE_OPERAND (bfr, 2)))
	{
	  chunk_size = tree_to_uhwi (TREE_OPERAND (bfr, 1));
	  start_offset = access->offset
	    + tree_to_uhwi (TREE_OPERAND (bfr, 2));
	}
      else
	start_offset = chunk_size = 0;

      generate_subtree_copies (access->first_child, orig_expr, access->offset,
			       start_offset, chunk_size,
			       write ? refresh_gsi : stmt_gsi,
			       write, write, loc);
    }
  return true;
}

/* If EXPR, which must be a call argument, is an ADDR_EXPR, generate writes and
   reads from its base before and after the call statement given in CALL_GSI
   and return true if any copying took place.  Otherwise call sra_modify_expr
   on EXPR and return its value.  FLAGS is what the gimple_call_arg_flags
   return for the given parameter.  */

static bool
sra_modify_call_arg (tree *expr, gimple_stmt_iterator *call_gsi,
		     gimple_stmt_iterator *refresh_gsi, int flags)
{
  if (TREE_CODE (*expr) != ADDR_EXPR)
    return sra_modify_expr (expr, false, call_gsi, refresh_gsi);

  if (flags & EAF_UNUSED)
    return false;

  tree base = get_base_address (TREE_OPERAND (*expr, 0));
  if (!DECL_P (base))
    return false;
  struct access *access = get_access_for_expr (base);
  if (!access)
    return false;

  gimple *stmt = gsi_stmt (*call_gsi);
  location_t loc = gimple_location (stmt);
  generate_subtree_copies (access, base, 0, 0, 0, call_gsi, false, false,
			   loc);

  if (flags & EAF_NO_DIRECT_CLOBBER)
    return true;

  if (!stmt_ends_bb_p (stmt))
    generate_subtree_copies (access, base, 0, 0, 0, refresh_gsi, true,
			     true, loc);
  else
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, gsi_bb (*call_gsi)->succs)
	{
	  gimple_stmt_iterator alt_gsi = gsi_start_edge (e);
	  generate_subtree_copies (access, base, 0, 0, 0, &alt_gsi, true,
				   true, loc);
	}
    }
  return true;
}

/* Where scalar replacements of the RHS have been written to when a replacement
   of a LHS of an assigments cannot be direclty loaded from a replacement of
   the RHS. */
enum unscalarized_data_handling { SRA_UDH_NONE,  /* Nothing done so far. */
				  SRA_UDH_RIGHT, /* Data flushed to the RHS. */
				  SRA_UDH_LEFT }; /* Data flushed to the LHS. */

struct subreplacement_assignment_data
{
  /* Offset of the access representing the lhs of the assignment.  */
  HOST_WIDE_INT left_offset;

  /* LHS and RHS of the original assignment.  */
  tree assignment_lhs, assignment_rhs;

  /* Access representing the rhs of the whole assignment.  */
  struct access *top_racc;

  /* Stmt iterator used for statement insertions after the original assignment.
   It points to the main GSI used to traverse a BB during function body
   modification.  */
  gimple_stmt_iterator *new_gsi;

  /* Stmt iterator used for statement insertions before the original
   assignment.  Keeps on pointing to the original statement.  */
  gimple_stmt_iterator old_gsi;

  /* Location of the assignment.   */
  location_t loc;

  /* Keeps the information whether we have needed to refresh replacements of
   the LHS and from which side of the assignments this takes place.  */
  enum unscalarized_data_handling refreshed;
};

/* Store all replacements in the access tree rooted in TOP_RACC either to their
   base aggregate if there are unscalarized data or directly to LHS of the
   statement that is pointed to by GSI otherwise.  */

static void
handle_unscalarized_data_in_subtree (struct subreplacement_assignment_data *sad)
{
  tree src;
  /* If the RHS is a load from a constant, we do not need to (and must not)
     flush replacements to it and can use it directly as if we did.  */
  if (TREE_READONLY (sad->top_racc->base))
    {
      sad->refreshed = SRA_UDH_RIGHT;
      return;
    }
  if (sad->top_racc->grp_unscalarized_data)
    {
      src = sad->assignment_rhs;
      sad->refreshed = SRA_UDH_RIGHT;
    }
  else
    {
      src = sad->assignment_lhs;
      sad->refreshed = SRA_UDH_LEFT;
    }
  generate_subtree_copies (sad->top_racc->first_child, src,
			   sad->top_racc->offset, 0, 0,
			   &sad->old_gsi, false, false, sad->loc);
}

/* Try to generate statements to load all sub-replacements in an access subtree
   formed by children of LACC from scalar replacements in the SAD->top_racc
   subtree.  If that is not possible, refresh the SAD->top_racc base aggregate
   and load the accesses from it.  */

static void
load_assign_lhs_subreplacements (struct access *lacc,
				 struct subreplacement_assignment_data *sad)
{
  for (lacc = lacc->first_child; lacc; lacc = lacc->next_sibling)
    {
      HOST_WIDE_INT offset;
      offset = lacc->offset - sad->left_offset + sad->top_racc->offset;

      if (lacc->grp_to_be_replaced)
	{
	  struct access *racc;
	  gassign *stmt;
	  tree rhs;

	  racc = find_access_in_subtree (sad->top_racc, offset, lacc->size);
	  if (racc && racc->grp_to_be_replaced)
	    {
	      rhs = get_access_replacement (racc);
	      bool vce = false;
	      if (!useless_type_conversion_p (lacc->type, racc->type))
		{
		  rhs = fold_build1_loc (sad->loc, VIEW_CONVERT_EXPR,
					 lacc->type, rhs);
		  vce = true;
		}

	      if (lacc->grp_partial_lhs && (vce || racc->grp_partial_lhs))
		rhs = force_gimple_operand_gsi (&sad->old_gsi, rhs, true,
						NULL_TREE, true, GSI_SAME_STMT);
	    }
	  else
	    {
	      /* No suitable access on the right hand side, need to load from
		 the aggregate.  See if we have to update it first... */
	      if (sad->refreshed == SRA_UDH_NONE)
		handle_unscalarized_data_in_subtree (sad);

	      if (sad->refreshed == SRA_UDH_LEFT)
		rhs = build_ref_for_model (sad->loc, sad->assignment_lhs,
					   lacc->offset - sad->left_offset,
					   lacc, sad->new_gsi, true);
	      else
		rhs = build_ref_for_model (sad->loc, sad->assignment_rhs,
					   lacc->offset - sad->left_offset,
					   lacc, sad->new_gsi, true);
	      if (lacc->grp_partial_lhs)
		rhs = force_gimple_operand_gsi (sad->new_gsi,
						rhs, true, NULL_TREE,
						false, GSI_NEW_STMT);
	    }

	  stmt = gimple_build_assign (get_access_replacement (lacc), rhs);
	  gsi_insert_after (sad->new_gsi, stmt, GSI_NEW_STMT);
	  gimple_set_location (stmt, sad->loc);
	  update_stmt (stmt);
	  sra_stats.subreplacements++;
	}
      else
	{
	  if (sad->refreshed == SRA_UDH_NONE
	      && lacc->grp_read && !lacc->grp_covered)
	    handle_unscalarized_data_in_subtree (sad);

	  if (lacc && lacc->grp_to_be_debug_replaced)
	    {
	      gdebug *ds;
	      tree drhs;
	      struct access *racc = find_access_in_subtree (sad->top_racc,
							    offset,
							    lacc->size);

	      if (racc && racc->grp_to_be_replaced)
		{
		  if (racc->grp_write || constant_decl_p (racc->base))
		    drhs = get_access_replacement (racc);
		  else
		    drhs = NULL;
		}
	      else if (sad->refreshed == SRA_UDH_LEFT)
		drhs = build_debug_ref_for_model (sad->loc, lacc->base,
						  lacc->offset, lacc);
	      else if (sad->refreshed == SRA_UDH_RIGHT)
		drhs = build_debug_ref_for_model (sad->loc, sad->top_racc->base,
						  offset, lacc);
	      else
		drhs = NULL_TREE;
	      if (drhs
		  && !useless_type_conversion_p (lacc->type, TREE_TYPE (drhs)))
		drhs = fold_build1_loc (sad->loc, VIEW_CONVERT_EXPR,
					lacc->type, drhs);
	      ds = gimple_build_debug_bind (get_access_replacement (lacc),
					    drhs, gsi_stmt (sad->old_gsi));
	      gsi_insert_after (sad->new_gsi, ds, GSI_NEW_STMT);
	    }
	}

      if (lacc->first_child)
	load_assign_lhs_subreplacements (lacc, sad);
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
  tree lhs = gimple_assign_lhs (stmt);
  struct access *acc = get_access_for_expr (lhs);
  if (!acc)
    return SRA_AM_NONE;
  location_t loc = gimple_location (stmt);

  if (gimple_clobber_p (stmt))
    {
      /* Clobber the replacement variable.  */
      clobber_subtree (acc, gsi, !acc->grp_covered, loc);
      /* Remove clobbers of fully scalarized variables, they are dead.  */
      if (acc->grp_covered)
	{
	  unlink_stmt_vdef (stmt);
	  gsi_remove (gsi, true);
	  release_defs (stmt);
	  return SRA_AM_REMOVED;
	}
      else
	return SRA_AM_MODIFIED;
    }

  if (CONSTRUCTOR_NELTS (gimple_assign_rhs1 (stmt)) > 0)
    {
      /* I have never seen this code path trigger but if it can happen the
	 following should handle it gracefully.  */
      if (access_has_children_p (acc))
	generate_subtree_copies (acc->first_child, lhs, acc->offset, 0, 0, gsi,
				 true, true, loc);
      return SRA_AM_MODIFIED;
    }

  if (acc->grp_covered)
    {
      init_subtree_with_zero (acc, gsi, false, loc);
      unlink_stmt_vdef (stmt);
      gsi_remove (gsi, true);
      release_defs (stmt);
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
   loaded.  REG_TREE is used instead of the actual RACC type if that is not of
   a gimple register type.  */

static tree
get_repl_default_def_ssa_name (struct access *racc, tree reg_type)
{
  gcc_checking_assert (!racc->grp_to_be_replaced
		       && !racc->grp_to_be_debug_replaced);
  if (!racc->replacement_decl)
    racc->replacement_decl = create_access_replacement (racc, reg_type);
  return get_or_create_ssa_default_def (cfun, racc->replacement_decl);
}


/* Generate statements to call .DEFERRED_INIT to initialize scalar replacements
   of accesses within a subtree ACCESS; all its children, siblings and their
   children are to be processed.
   GSI is a statement iterator used to place the new statements.  */
static void
generate_subtree_deferred_init (struct access *access,
				tree init_type,
				tree decl_name,
				gimple_stmt_iterator *gsi,
				location_t loc)
{
  do
    {
      if (access->grp_to_be_replaced)
	{
	  tree repl = get_access_replacement (access);
	  gimple *call
	    = gimple_build_call_internal (IFN_DEFERRED_INIT, 3,
					  TYPE_SIZE_UNIT (TREE_TYPE (repl)),
					  init_type, decl_name);
	  gimple_call_set_lhs (call, repl);
	  gsi_insert_before (gsi, call, GSI_SAME_STMT);
	  update_stmt (call);
	  gimple_set_location (call, loc);
	  sra_stats.subtree_deferred_init++;
	}
      if (access->first_child)
	generate_subtree_deferred_init (access->first_child, init_type,
					decl_name, gsi, loc);

      access = access ->next_sibling;
    }
  while (access);
}

/* For a call to .DEFERRED_INIT:
   var = .DEFERRED_INIT (size_of_var, init_type, name_of_var);
   examine the LHS variable VAR and replace it with a scalar replacement if
   there is one, also replace the RHS call to a call to .DEFERRED_INIT of
   the corresponding scalar relacement variable.  Examine the subtree and
   do the scalar replacements in the subtree too.  STMT is the call, GSI is
   the statment iterator to place newly created statement.  */

static enum assignment_mod_result
sra_modify_deferred_init (gimple *stmt, gimple_stmt_iterator *gsi)
{
  tree lhs = gimple_call_lhs (stmt);
  tree init_type = gimple_call_arg (stmt, 1);
  tree decl_name = gimple_call_arg (stmt, 2);

  struct access *lhs_access = get_access_for_expr (lhs);
  if (!lhs_access)
    return SRA_AM_NONE;

  location_t loc = gimple_location (stmt);

  if (lhs_access->grp_to_be_replaced)
    {
      tree lhs_repl = get_access_replacement (lhs_access);
      gimple_call_set_lhs (stmt, lhs_repl);
      tree arg0_repl = TYPE_SIZE_UNIT (TREE_TYPE (lhs_repl));
      gimple_call_set_arg (stmt, 0, arg0_repl);
      sra_stats.deferred_init++;
      gcc_assert (!lhs_access->first_child);
      return SRA_AM_MODIFIED;
    }

  if (lhs_access->first_child)
    generate_subtree_deferred_init (lhs_access->first_child,
				    init_type, decl_name, gsi, loc);
  if (lhs_access->grp_covered)
    {
      unlink_stmt_vdef (stmt);
      gsi_remove (gsi, true);
      release_defs (stmt);
      return SRA_AM_REMOVED;
    }

  return SRA_AM_MODIFIED;
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

  if (!gimple_assign_single_p (stmt))
    return SRA_AM_NONE;
  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (rhs) == CONSTRUCTOR)
    return sra_modify_constructor_assign (stmt, gsi);

  if (TREE_CODE (rhs) == REALPART_EXPR || TREE_CODE (lhs) == REALPART_EXPR
      || TREE_CODE (rhs) == IMAGPART_EXPR || TREE_CODE (lhs) == IMAGPART_EXPR
      || (TREE_CODE (rhs) == BIT_FIELD_REF && !sra_handled_bf_read_p (rhs))
      || TREE_CODE (lhs) == BIT_FIELD_REF)
    {
      modify_this_stmt = sra_modify_expr (gimple_assign_rhs1_ptr (stmt),
					  false, gsi, gsi);
      modify_this_stmt |= sra_modify_expr (gimple_assign_lhs_ptr (stmt),
					   true, gsi, gsi);
      return modify_this_stmt ? SRA_AM_MODIFIED : SRA_AM_NONE;
    }

  lacc = get_access_for_expr (lhs);
  racc = get_access_for_expr (rhs);
  if (!lacc && !racc)
    return SRA_AM_NONE;
  /* Avoid modifying initializations of constant-pool replacements.  */
  if (racc && (racc->replacement_decl == lhs))
    return SRA_AM_NONE;

  loc = gimple_location (stmt);
  if (lacc && lacc->grp_to_be_replaced)
    {
      lhs = get_access_replacement (lacc);
      gimple_assign_set_lhs (stmt, lhs);
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
	   && !racc->grp_unscalarizable_region
	   && TREE_CODE (lhs) == SSA_NAME
	   && !access_has_replacements_p (racc))
    {
      rhs = get_repl_default_def_ssa_name (racc, TREE_TYPE (lhs));
      modify_this_stmt = true;
      sra_stats.exprs++;
    }

  if (modify_this_stmt
      && !useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
    {
      /* If we can avoid creating a VIEW_CONVERT_EXPR, then do so.
	 ??? This should move to fold_stmt which we simply should
	 call after building a VIEW_CONVERT_EXPR here.  */
      if (AGGREGATE_TYPE_P (TREE_TYPE (lhs))
	  && TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (lhs)) == racc->reverse
	  && !contains_bitfld_component_ref_p (lhs))
	{
	  lhs = build_ref_for_model (loc, lhs, 0, racc, gsi, false);
	  gimple_assign_set_lhs (stmt, lhs);
	}
      else if (lacc
	       && AGGREGATE_TYPE_P (TREE_TYPE (rhs))
	       && TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (rhs)) == lacc->reverse
	       && !contains_vce_or_bfcref_p (rhs))
	rhs = build_ref_for_model (loc, rhs, 0, lacc, gsi, false);

      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	{
	  rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (lhs), rhs);
	  if (is_gimple_reg_type (TREE_TYPE (lhs))
	      && TREE_CODE (lhs) != SSA_NAME)
	    force_gimple_rhs = true;
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
      gdebug *ds = gimple_build_debug_bind (dlhs, drhs, stmt);
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
      || gimple_has_volatile_ops (stmt)
      || contains_vce_or_bfcref_p (rhs)
      || contains_vce_or_bfcref_p (lhs)
      || stmt_ends_bb_p (stmt))
    {
      /* No need to copy into a constant, it comes pre-initialized.  */
      if (access_has_children_p (racc) && !TREE_READONLY (racc->base))
	generate_subtree_copies (racc->first_child, rhs, racc->offset, 0, 0,
				 gsi, false, false, loc);
      if (access_has_children_p (lacc))
	{
	  gimple_stmt_iterator alt_gsi = gsi_none ();
	  if (stmt_ends_bb_p (stmt))
	    {
	      alt_gsi = gsi_start_edge (single_non_eh_succ (gsi_bb (*gsi)));
	      gsi = &alt_gsi;
	    }
	  generate_subtree_copies (lacc->first_child, lhs, lacc->offset, 0, 0,
				   gsi, true, true, loc);
	}
      sra_stats.separate_lhs_rhs_handling++;

      /* This gimplification must be done after generate_subtree_copies,
	 lest we insert the subtree copies in the middle of the gimplified
	 sequence.  */
      if (force_gimple_rhs)
	rhs = force_gimple_operand_gsi (&orig_gsi, rhs, true, NULL_TREE,
					true, GSI_SAME_STMT);
      if (gimple_assign_rhs1 (stmt) != rhs)
	{
	  modify_this_stmt = true;
	  gimple_assign_set_rhs_from_tree (&orig_gsi, rhs);
	  gcc_assert (stmt == gsi_stmt (orig_gsi));
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
	  struct subreplacement_assignment_data sad;

	  sad.left_offset = lacc->offset;
	  sad.assignment_lhs = lhs;
	  sad.assignment_rhs = rhs;
	  sad.top_racc = racc;
	  sad.old_gsi = *gsi;
	  sad.new_gsi = gsi;
	  sad.loc = gimple_location (stmt);
	  sad.refreshed = SRA_UDH_NONE;

	  if (lacc->grp_read && !lacc->grp_covered)
	    handle_unscalarized_data_in_subtree (&sad);

	  load_assign_lhs_subreplacements (lacc, &sad);
	  if (sad.refreshed != SRA_UDH_RIGHT)
	    {
	      gsi_next (gsi);
	      unlink_stmt_vdef (stmt);
	      gsi_remove (&sad.old_gsi, true);
	      release_defs (stmt);
	      sra_stats.deleted++;
	      return SRA_AM_REMOVED;
	    }
	}
      else
	{
	  if (access_has_children_p (racc)
	      && !racc->grp_unscalarized_data
	      && TREE_CODE (lhs) != SSA_NAME)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Removing load: ");
		  print_gimple_stmt (dump_file, stmt, 0);
		}
	      generate_subtree_copies (racc->first_child, lhs,
				       racc->offset, 0, 0, gsi,
				       false, false, loc);
	      gcc_assert (stmt == gsi_stmt (*gsi));
	      unlink_stmt_vdef (stmt);
	      gsi_remove (gsi, true);
	      release_defs (stmt);
	      sra_stats.deleted++;
	      return SRA_AM_REMOVED;
	    }
	  /* Restore the aggregate RHS from its components so the
	     prevailing aggregate copy does the right thing.  */
	  if (access_has_children_p (racc) && !TREE_READONLY (racc->base))
	    generate_subtree_copies (racc->first_child, rhs, racc->offset, 0, 0,
				     gsi, false, false, loc);
	  /* Re-load the components of the aggregate copy destination.
	     But use the RHS aggregate to load from to expose more
	     optimization opportunities.  */
	  if (access_has_children_p (lacc))
	    {
	      generate_subtree_copies (lacc->first_child, rhs, lacc->offset,
				       0, 0, gsi, true, true, loc);
	      if (lacc->grp_covered)
		{
		  unlink_stmt_vdef (stmt);
		  gsi_remove (& orig_gsi, true);
		  release_defs (stmt);
		  sra_stats.deleted++;
		  return SRA_AM_REMOVED;
		}
	    }
	}

      return SRA_AM_NONE;
    }
}

/* Set any scalar replacements of values in the constant pool to the initial
   value of the constant.  (Constant-pool decls like *.LC0 have effectively
   been initialized before the program starts, we must do the same for their
   replacements.)  Thus, we output statements like 'SR.1 = *.LC0[0];' into
   the function's entry block.  */

static void
initialize_constant_pool_replacements (void)
{
  gimple_seq seq = NULL;
  gimple_stmt_iterator gsi = gsi_start (seq);
  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (candidate_bitmap, 0, i, bi)
    {
      tree var = candidate (i);
      if (!constant_decl_p (var))
	continue;

      struct access *access = get_first_repr_for_decl (var);

      while (access)
	{
	  if (access->replacement_decl)
	    {
	      gassign *stmt
		= gimple_build_assign (get_access_replacement (access),
				       unshare_expr (access->expr));
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Generating constant initializer: ");
		  print_gimple_stmt (dump_file, stmt, 0);
		  fprintf (dump_file, "\n");
		}
	      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
	      update_stmt (stmt);
	    }

	  if (access->first_child)
	    access = access->first_child;
	  else if (access->next_sibling)
	    access = access->next_sibling;
	  else
	    {
	      while (access->parent && !access->next_sibling)
		access = access->parent;
	      if (access->next_sibling)
		access = access->next_sibling;
	      else
		access = access->next_grp;
	    }
	}
    }

  seq = gsi_seq (gsi);
  if (seq)
    gsi_insert_seq_on_edge_immediate (
      single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)), seq);
}

/* Traverse the function body and all modifications as decided in
   analyze_all_variable_accesses.  Return true iff the CFG has been
   changed.  */

static bool
sra_modify_function_body (void)
{
  bool cfg_changed = false;
  basic_block bb;

  initialize_constant_pool_replacements ();

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (bb);
      while (!gsi_end_p (gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  enum assignment_mod_result assign_result;
	  bool modified = false, deleted = false;
	  tree *t;
	  unsigned i;

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval_ptr (as_a <greturn *> (stmt));
	      if (*t != NULL_TREE)
		modified |= sra_modify_expr (t, false, &gsi, &gsi);
	      break;

	    case GIMPLE_ASSIGN:
	      assign_result = sra_modify_assign (stmt, &gsi);
	      modified |= assign_result == SRA_AM_MODIFIED;
	      deleted = assign_result == SRA_AM_REMOVED;
	      break;

	    case GIMPLE_CALL:
	      /* Handle calls to .DEFERRED_INIT specially.  */
	      if (gimple_call_internal_p (stmt, IFN_DEFERRED_INIT))
		{
		  assign_result = sra_modify_deferred_init (stmt, &gsi);
		  modified |= assign_result == SRA_AM_MODIFIED;
		  deleted = assign_result == SRA_AM_REMOVED;
		}
	      else
		{
		  gcall *call = as_a <gcall *> (stmt);
		  gimple_stmt_iterator call_gsi = gsi;

		  /* Operands must be processed before the lhs.  */
		  for (i = 0; i < gimple_call_num_args (call); i++)
		    {
		      int flags = gimple_call_arg_flags (call, i);
		      t = gimple_call_arg_ptr (call, i);
		      modified |= sra_modify_call_arg (t, &call_gsi, &gsi, flags);
		    }
		  if (gimple_call_chain (call))
		    {
		      t = gimple_call_chain_ptr (call);
		      int flags = gimple_call_static_chain_flags (call);
		      modified |= sra_modify_call_arg (t, &call_gsi, &gsi,
						       flags);
		    }
		  if (gimple_call_lhs (call))
		    {
		      t = gimple_call_lhs_ptr (call);
		      modified |= sra_modify_expr (t, true, &call_gsi, &gsi);
		    }
		}
	      break;

	    case GIMPLE_ASM:
	      {
		gimple_stmt_iterator stmt_gsi = gsi;
		gasm *asm_stmt = as_a <gasm *> (stmt);
		for (i = 0; i < gimple_asm_ninputs (asm_stmt); i++)
		  {
		    t = &TREE_VALUE (gimple_asm_input_op (asm_stmt, i));
		    modified |= sra_modify_expr (t, false, &stmt_gsi, &gsi);
		  }
		for (i = 0; i < gimple_asm_noutputs (asm_stmt); i++)
		  {
		    t = &TREE_VALUE (gimple_asm_output_op (asm_stmt, i));
		    modified |= sra_modify_expr (t, true, &stmt_gsi, &gsi);
		  }
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

  gsi_commit_edge_inserts ();
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
    gsi_insert_seq_on_edge_immediate (single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)), seq);
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
  TV_TREE_SRA, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_sra_early : public gimple_opt_pass
{
public:
  pass_sra_early (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sra_early, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return gate_intra_sra (); }
  unsigned int execute (function *) final override
  {
    return early_intra_sra ();
  }

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
  TV_TREE_SRA, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  TODO_update_address_taken, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_sra : public gimple_opt_pass
{
public:
  pass_sra (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sra, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return gate_intra_sra (); }
  unsigned int execute (function *) final override { return late_intra_sra (); }

}; // class pass_sra

} // anon namespace

gimple_opt_pass *
make_pass_sra (gcc::context *ctxt)
{
  return new pass_sra (ctxt);
}


/* If type T cannot be totally scalarized, return false.  Otherwise return true
   and push to the vector within PC offsets and lengths of all padding in the
   type as total scalarization would encounter it.  */

static bool
check_ts_and_push_padding_to_vec (tree type, sra_padding_collecting *pc)
{
  if (!totally_scalarizable_type_p (type, true /* optimistic value */,
				    0, pc))
    return false;

  pc->record_padding (tree_to_shwi (TYPE_SIZE (type)));
  return true;
}

/* Given two types in an assignment, return true either if any one cannot be
   totally scalarized or if they have padding (i.e. not copied bits)  */

bool
sra_total_scalarization_would_copy_same_data_p (tree t1, tree t2)
{
  sra_padding_collecting p1;
  if (!check_ts_and_push_padding_to_vec (t1, &p1))
    return true;

  sra_padding_collecting p2;
  if (!check_ts_and_push_padding_to_vec (t2, &p2))
    return true;

  unsigned l = p1.m_padding.length ();
  if (l != p2.m_padding.length ())
    return false;
  for (unsigned i = 0; i < l; i++)
    if (p1.m_padding[i].first != p2.m_padding[i].first
	|| p1.m_padding[i].second != p2.m_padding[i].second)
      return false;

  return true;
}

