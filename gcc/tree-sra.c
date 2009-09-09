/* Scalar Replacement of Aggregates (SRA) converts some structure
   references into scalar references, exposing them to the scalar
   optimizers.
   Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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
#include "alloc-pool.h"
#include "tm.h"
#include "tree.h"
#include "gimple.h"
#include "tree-flow.h"
#include "ipa-prop.h"
#include "diagnostic.h"
#include "statistics.h"
#include "tree-dump.h"
#include "timevar.h"
#include "params.h"
#include "target.h"
#include "flags.h"

/* Enumeration of all aggregate reductions we can do.  */
enum sra_mode { SRA_MODE_EARLY_INTRA, /* early intraprocedural SRA */
		SRA_MODE_INTRA };	     /* late intraprocedural SRA */

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

  /* Expression.  */
  tree expr;
  /* Type.  */
  tree type;

  /* Next group representative for this aggregate. */
  struct access *next_grp;

  /* Pointer to the group representative.  Pointer to itself if the struct is
     the representative.  */
  struct access *group_representative;

  /* If this access has any children (in terms of the definition above), this
     points to the first one.  */
  struct access *first_child;

  /* Pointer to the next sibling in the access tree as described above.  */
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

  /* Is this access currently in the work queue?  */
  unsigned grp_queued : 1;
  /* Does this group contain a write access?  This flag is propagated down the
     access tree.  */
  unsigned grp_write : 1;
  /* Does this group contain a read access?  This flag is propagated down the
     access tree.  */
  unsigned grp_read : 1;
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

  /* Set when a scalar replacement should be created for this variable.  We do
     the decision and creation at different places because create_tmp_var
     cannot be called from within FOR_EACH_REFERENCED_VAR. */
  unsigned grp_to_be_replaced : 1;
};

typedef struct access *access_p;

DEF_VEC_P (access_p);
DEF_VEC_ALLOC_P (access_p, heap);

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

/* Base (tree) -> Vector (VEC(access_p,heap) *) map.  */
static struct pointer_map_t *base_access_vec;

/* Bitmap of bases (candidates).  */
static bitmap candidate_bitmap;
/* Obstack for creation of fancy names.  */
static struct obstack name_obstack;

/* Head of a linked list of accesses that need to have its subaccesses
   propagated to their assignment counterparts. */
static struct access *work_queue_head;

/* Dump contents of ACCESS to file F in a human friendly way.  If GRP is true,
   representative fields are dumped, otherwise those which only describe the
   individual access are.  */

static struct
{
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

  /* Number of processed aggregates is readily available in
     analyze_all_variable_accesses and so is not stored here.  */
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
    fprintf (f, ", grp_write = %d, grp_read = %d, grp_hint = %d, "
	     "grp_covered = %d, grp_unscalarizable_region = %d, "
	     "grp_unscalarized_data = %d, grp_partial_lhs = %d, "
	     "grp_to_be_replaced = %d\n",
	     access->grp_write, access->grp_read, access->grp_hint,
	     access->grp_covered, access->grp_unscalarizable_region,
	     access->grp_unscalarized_data, access->grp_partial_lhs,
	     access->grp_to_be_replaced);
  else
    fprintf (f, ", write = %d, grp_partial_lhs = %d\n", access->write,
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

/* Return a vector of pointers to accesses for the variable given in BASE or
   NULL if there is none.  */

static VEC (access_p, heap) *
get_base_access_vector (tree base)
{
  void **slot;

  slot = pointer_map_contains (base_access_vec, base);
  if (!slot)
    return NULL;
  else
    return *(VEC (access_p, heap) **) slot;
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
  VEC (access_p, heap) *access_vec;

  access_vec = get_base_access_vector (base);
  if (!access_vec)
    return NULL;

  return VEC_index (access_p, access_vec, 0);
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
  gcc_obstack_init (&name_obstack);
  access_pool = create_alloc_pool ("SRA accesses", sizeof (struct access), 16);
  link_pool = create_alloc_pool ("SRA links", sizeof (struct assign_link), 16);
  base_access_vec = pointer_map_create ();
  memset (&sra_stats, 0, sizeof (sra_stats));
}

/* Hook fed to pointer_map_traverse, deallocate stored vectors.  */

static bool
delete_base_accesses (const void *key ATTRIBUTE_UNUSED, void **value,
		     void *data ATTRIBUTE_UNUSED)
{
  VEC (access_p, heap) *access_vec;
  access_vec = (VEC (access_p, heap) *) *value;
  VEC_free (access_p, heap, access_vec);

  return true;
}

/* Deallocate all general structures.  */

static void
sra_deinitialize (void)
{
  BITMAP_FREE (candidate_bitmap);
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
  bitmap_clear_bit (candidate_bitmap, DECL_UID (decl));

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
type_internals_preclude_sra_p (tree type)
{
  tree fld;
  tree et;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL)
	  {
	    tree ft = TREE_TYPE (fld);

	    if (TREE_THIS_VOLATILE (fld)
		|| !DECL_FIELD_OFFSET (fld) || !DECL_SIZE (fld)
		|| !host_integerp (DECL_FIELD_OFFSET (fld), 1)
		|| !host_integerp (DECL_SIZE (fld), 1))
	      return true;

	    if (AGGREGATE_TYPE_P (ft)
		&& type_internals_preclude_sra_p (ft))
	      return true;
	  }

      return false;

    case ARRAY_TYPE:
      et = TREE_TYPE (type);

      if (AGGREGATE_TYPE_P (et))
	return type_internals_preclude_sra_p (et);
      else
	return false;

    default:
      return false;
    }
}

/* Create and insert access for EXPR. Return created access, or NULL if it is
   not possible.  */

static struct access *
create_access (tree expr, bool write)
{
  struct access *access;
  void **slot;
  VEC (access_p,heap) *vec;
  HOST_WIDE_INT offset, size, max_size;
  tree base = expr;
  bool unscalarizable_region = false;

  base = get_ref_base_and_extent (expr, &offset, &size, &max_size);

  if (!DECL_P (base) || !bitmap_bit_p (candidate_bitmap, DECL_UID (base)))
    return NULL;

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

  access = (struct access *) pool_alloc (access_pool);
  memset (access, 0, sizeof (struct access));

  access->base = base;
  access->offset = offset;
  access->size = size;
  access->expr = expr;
  access->type = TREE_TYPE (expr);
  access->write = write;
  access->grp_unscalarizable_region = unscalarizable_region;

  slot = pointer_map_contains (base_access_vec, base);
  if (slot)
    vec = (VEC (access_p, heap) *) *slot;
  else
    vec = VEC_alloc (access_p, heap, 32);

  VEC_safe_push (access_p, heap, vec, access);

  *((struct VEC (access_p,heap) **)
	pointer_map_insert (base_access_vec, base)) = vec;

  return access;
}


/* Search the given tree for a declaration by skipping handled components and
   exclude it from the candidates.  */

static void
disqualify_base_of_expr (tree t, const char *reason)
{
  while (handled_component_p (t))
    t = TREE_OPERAND (t, 0);

  if (DECL_P (t))
    disqualify_candidate (t, reason);
}

/* Scan expression EXPR and create access structures for all accesses to
   candidates for scalarization.  Return the created access or NULL if none is
   created.  */

static struct access *
build_access_from_expr_1 (tree *expr_ptr, bool write)
{
  struct access *ret = NULL;
  tree expr = *expr_ptr;
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
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      ret = create_access (expr, write);
      break;

    default:
      break;
    }

  if (write && partial_ref && ret)
    ret->grp_partial_lhs = 1;

  return ret;
}

/* Callback of scan_function.  Scan expression EXPR and create access
   structures for all accesses to candidates for scalarization.  Return true if
   any access has been inserted.  */

static bool
build_access_from_expr (tree *expr_ptr,
			gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED, bool write,
			void *data ATTRIBUTE_UNUSED)
{
  return build_access_from_expr_1 (expr_ptr, write) != NULL;
}

/* Disqualify LHS and RHS for scalarization if STMT must end its basic block in
   modes in which it matters, return true iff they have been disqualified.  RHS
   may be NULL, in that case ignore it.  If we scalarize an aggregate in
   intra-SRA we may need to add statements after each statement.  This is not
   possible if a statement unconditionally has to end the basic block.  */
static bool
disqualify_ops_if_throwing_stmt (gimple stmt, tree lhs, tree rhs)
{
  if (stmt_can_throw_internal (stmt) || stmt_ends_bb_p (stmt))
    {
      disqualify_base_of_expr (lhs, "LHS of a throwing stmt.");
      if (rhs)
	disqualify_base_of_expr (rhs, "RHS of a throwing stmt.");
      return true;
    }
  return false;
}


/* Result code for scan_assign callback for scan_function.  */
enum scan_assign_result { SRA_SA_NONE,       /* nothing done for the stmt */
			  SRA_SA_PROCESSED,  /* stmt analyzed/changed */
			  SRA_SA_REMOVED };  /* stmt redundant and eliminated */


/* Callback of scan_function.  Scan expressions occuring in the statement
   pointed to by STMT_EXPR, create access structures for all accesses to
   candidates for scalarization and remove those candidates which occur in
   statements or expressions that prevent them from being split apart.  Return
   true if any access has been inserted.  */

static enum scan_assign_result
build_accesses_from_assign (gimple *stmt_ptr,
			    gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
			    void *data ATTRIBUTE_UNUSED)
{
  gimple stmt = *stmt_ptr;
  tree *lhs_ptr, *rhs_ptr;
  struct access *lacc, *racc;

  if (!gimple_assign_single_p (stmt))
    return SRA_SA_NONE;

  lhs_ptr = gimple_assign_lhs_ptr (stmt);
  rhs_ptr = gimple_assign_rhs1_ptr (stmt);

  if (disqualify_ops_if_throwing_stmt (stmt, *lhs_ptr, *rhs_ptr))
    return SRA_SA_NONE;

  racc = build_access_from_expr_1 (rhs_ptr, false);
  lacc = build_access_from_expr_1 (lhs_ptr, true);

  if (lacc && racc
      && !lacc->grp_unscalarizable_region
      && !racc->grp_unscalarizable_region
      && AGGREGATE_TYPE_P (TREE_TYPE (*lhs_ptr))
      /* FIXME: Turn the following line into an assert after PR 40058 is
	 fixed.  */
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

  return (lacc || racc) ? SRA_SA_PROCESSED : SRA_SA_NONE;
}

/* Callback of walk_stmt_load_store_addr_ops visit_addr used to determine
   GIMPLE_ASM operands with memory constrains which cannot be scalarized.  */

static bool
asm_visit_addr (gimple stmt ATTRIBUTE_UNUSED, tree op,
		void *data ATTRIBUTE_UNUSED)
{
  if (DECL_P (op))
    disqualify_candidate (op, "Non-scalarizable GIMPLE_ASM operand.");

  return false;
}


/* Scan function and look for interesting statements. Return true if any has
   been found or processed, as indicated by callbacks.  SCAN_EXPR is a callback
   called on all expressions within statements except assign statements and
   those deemed entirely unsuitable for some reason (all operands in such
   statements and expression are removed from candidate_bitmap).  SCAN_ASSIGN
   is a callback called on all assign statements, HANDLE_SSA_DEFS is a callback
   called on assign statements and those call statements which have a lhs and
   it is the only callback which can be NULL. ANALYSIS_STAGE is true when
   running in the analysis stage of a pass and thus no statement is being
   modified.  DATA is a pointer passed to all callbacks.  If any single
   callback returns true, this function also returns true, otherwise it returns
   false.  */

static bool
scan_function (bool (*scan_expr) (tree *, gimple_stmt_iterator *, bool, void *),
	       enum scan_assign_result (*scan_assign) (gimple *,
						       gimple_stmt_iterator *,
						       void *),
	       bool (*handle_ssa_defs)(gimple, void *),
	       bool analysis_stage, void *data)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  unsigned i;
  tree *t;
  bool ret = false;

  FOR_EACH_BB (bb)
    {
      bool bb_changed = false;

      gsi = gsi_start_bb (bb);
      while (!gsi_end_p (gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  enum scan_assign_result assign_result;
	  bool any = false, deleted = false;

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_RETURN:
	      t = gimple_return_retval_ptr (stmt);
	      if (*t != NULL_TREE)
		any |= scan_expr (t, &gsi, false, data);
	      break;

	    case GIMPLE_ASSIGN:
	      assign_result = scan_assign (&stmt, &gsi, data);
	      any |= assign_result == SRA_SA_PROCESSED;
	      deleted = assign_result == SRA_SA_REMOVED;
	      if (handle_ssa_defs && assign_result != SRA_SA_REMOVED)
		any |= handle_ssa_defs (stmt, data);
	      break;

	    case GIMPLE_CALL:
	      /* Operands must be processed before the lhs.  */
	      for (i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  tree *argp = gimple_call_arg_ptr (stmt, i);
		  any |= scan_expr (argp, &gsi, false, data);
		}

	      if (gimple_call_lhs (stmt))
		{
		  tree *lhs_ptr = gimple_call_lhs_ptr (stmt);
		  if (!analysis_stage
		      || !disqualify_ops_if_throwing_stmt (stmt,
							   *lhs_ptr, NULL))
		    {
		      any |= scan_expr (lhs_ptr, &gsi, true, data);
		      if (handle_ssa_defs)
			any |= handle_ssa_defs (stmt, data);
		    }
		}
	      break;

	    case GIMPLE_ASM:

	      if (analysis_stage)
		walk_stmt_load_store_addr_ops (stmt, NULL, NULL, NULL,
					       asm_visit_addr);
	      for (i = 0; i < gimple_asm_ninputs (stmt); i++)
		{
		  tree *op = &TREE_VALUE (gimple_asm_input_op (stmt, i));
		  any |= scan_expr (op, &gsi, false, data);
		}
	      for (i = 0; i < gimple_asm_noutputs (stmt); i++)
		{
		  tree *op = &TREE_VALUE (gimple_asm_output_op (stmt, i));
		  any |= scan_expr (op, &gsi, true, data);
		}

	    default:
	      break;
	    }

	  if (any)
	    {
	      ret = true;
	      bb_changed = true;

	      if (!analysis_stage)
		{
		  update_stmt (stmt);
		  if (!stmt_could_throw_p (stmt))
		    remove_stmt_from_eh_region (stmt);
		}
	    }
	  if (deleted)
	    bb_changed = true;
	  else
	    {
	      gsi_next (&gsi);
	      ret = true;
	    }
	}
      if (!analysis_stage && bb_changed)
	gimple_purge_dead_eh_edges (bb);
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
      /* Put any non-aggregate type before any aggregate type.  */
      if (!is_gimple_reg_type (f1->type)
	       && is_gimple_reg_type (f2->type))
	return 1;
      else if (is_gimple_reg_type (f1->type)
	       && !is_gimple_reg_type (f2->type))
	return -1;
      /* Put the integral type with the bigger precision first.  */
      else if (INTEGRAL_TYPE_P (f1->type)
	  && INTEGRAL_TYPE_P (f2->type))
	return TYPE_PRECISION (f1->type) > TYPE_PRECISION (f2->type) ? -1 : 1;
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

/* Helper function for build_ref_for_offset.  */

static bool
build_ref_for_offset_1 (tree *res, tree type, HOST_WIDE_INT offset,
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
	  /* Some ADA records are half-unions, treat all of them the same.  */
	  for (fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
	    {
	      HOST_WIDE_INT pos, size;
	      tree expr, *expr_ptr;

	      if (TREE_CODE (fld) != FIELD_DECL)
		continue;

	      pos = int_bit_position (fld);
	      gcc_assert (TREE_CODE (type) == RECORD_TYPE || pos == 0);
	      size = tree_low_cst (DECL_SIZE (fld), 1);
	      if (pos > offset || (pos + size) <= offset)
		continue;

	      if (res)
		{
		  expr = build3 (COMPONENT_REF, TREE_TYPE (fld), *res, fld,
				 NULL_TREE);
		  expr_ptr = &expr;
		}
	      else
		expr_ptr = NULL;
	      if (build_ref_for_offset_1 (expr_ptr, TREE_TYPE (fld),
					  offset - pos, exp_type))
		{
		  if (res)
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
	  if (TREE_CODE (minidx) != INTEGER_CST)
	    return false;
	  if (res)
	    {
	      index = build_int_cst (TYPE_DOMAIN (type), offset / el_size);
	      if (!integer_zerop (minidx))
		index = int_const_binop (PLUS_EXPR, index, minidx, 0);
	      *res = build4 (ARRAY_REF, TREE_TYPE (type), *res, index,
			     NULL_TREE, NULL_TREE);
	    }
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

/* Construct an expression that would reference a part of aggregate *EXPR of
   type TYPE at the given OFFSET of the type EXP_TYPE.  If EXPR is NULL, the
   function only determines whether it can build such a reference without
   actually doing it.

   FIXME: Eventually this should be replaced with
   maybe_fold_offset_to_reference() from tree-ssa-ccp.c but that requires a
   minor rewrite of fold_stmt.
 */

bool
build_ref_for_offset (tree *expr, tree type, HOST_WIDE_INT offset,
		      tree exp_type, bool allow_ptr)
{
  location_t loc = expr ? EXPR_LOCATION (*expr) : UNKNOWN_LOCATION;

  if (allow_ptr && POINTER_TYPE_P (type))
    {
      type = TREE_TYPE (type);
      if (expr)
	*expr = fold_build1_loc (loc, INDIRECT_REF, type, *expr);
    }

  return build_ref_for_offset_1 (expr, type, offset, exp_type);
}

/* The very first phase of intraprocedural SRA.  It marks in candidate_bitmap
   those with type which is suitable for scalarization.  */

static bool
find_var_candidates (void)
{
  tree var, type;
  referenced_var_iterator rvi;
  bool ret = false;

  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      if (TREE_CODE (var) != VAR_DECL && TREE_CODE (var) != PARM_DECL)
        continue;
      type = TREE_TYPE (var);

      if (!AGGREGATE_TYPE_P (type)
	  || needs_to_live_in_memory (var)
	  || TREE_THIS_VOLATILE (var)
	  || !COMPLETE_TYPE_P (type)
	  || !host_integerp (TYPE_SIZE (type), 1)
          || tree_low_cst (TYPE_SIZE (type), 1) == 0
	  || type_internals_preclude_sra_p (type)
	  /* Fix for PR 41089.  tree-stdarg.c needs to have va_lists intact but
	      we also want to schedule it rather late.  Thus we ignore it in
	      the early pass. */
	  || (sra_mode == SRA_MODE_EARLY_INTRA
	      && (TYPE_MAIN_VARIANT (TREE_TYPE (var))
		  == TYPE_MAIN_VARIANT (va_list_type_node))))
	continue;

      bitmap_set_bit (candidate_bitmap, DECL_UID (var));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Candidate (%d): ", DECL_UID (var));
	  print_generic_expr (dump_file, var, 0);
	  fprintf (dump_file, "\n");
	}
      ret = true;
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
  VEC (access_p, heap) *access_vec;
  bool first = true;
  HOST_WIDE_INT low = -1, high = 0;

  access_vec = get_base_access_vector (var);
  if (!access_vec)
    return NULL;
  access_count = VEC_length (access_p, access_vec);

  /* Sort by <OFFSET, SIZE>.  */
  qsort (VEC_address (access_p, access_vec), access_count, sizeof (access_p),
	 compare_access_positions);

  i = 0;
  while (i < access_count)
    {
      struct access *access = VEC_index (access_p, access_vec, i);
      bool grp_write = access->write;
      bool grp_read = !access->write;
      bool multiple_reads = false;
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
	  struct access *ac2 = VEC_index (access_p, access_vec, j);
	  if (ac2->offset != access->offset || ac2->size != access->size)
	    break;
	  if (ac2->write)
	    grp_write = true;
	  else
	    {
	      if (grp_read)
		multiple_reads = true;
	      else
		grp_read = true;
	    }
	  grp_partial_lhs |= ac2->grp_partial_lhs;
	  unscalarizable_region |= ac2->grp_unscalarizable_region;
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
      access->grp_hint = multiple_reads;
      access->grp_partial_lhs = grp_partial_lhs;
      access->grp_unscalarizable_region = unscalarizable_region;
      if (access->first_link)
	add_access_to_work_queue (access);

      *prev_acc_ptr = access;
      prev_acc_ptr = &access->next_grp;
    }

  gcc_assert (res == VEC_index (access_p, access_vec, 0));
  return res;
}

/* Create a variable for the given ACCESS which determines the type, name and a
   few other properties.  Return the variable declaration and store it also to
   ACCESS->replacement.  */

static tree
create_access_replacement (struct access *access)
{
  tree repl;

  repl = create_tmp_var (access->type, "SR");
  get_var_ann (repl);
  add_referenced_var (repl);
  mark_sym_for_renaming (repl);

  if (!access->grp_partial_lhs
      && (TREE_CODE (access->type) == COMPLEX_TYPE
	  || TREE_CODE (access->type) == VECTOR_TYPE))
    DECL_GIMPLE_REG_P (repl) = 1;

  DECL_SOURCE_LOCATION (repl) = DECL_SOURCE_LOCATION (access->base);
  DECL_ARTIFICIAL (repl) = 1;

  if (DECL_NAME (access->base)
      && !DECL_IGNORED_P (access->base)
      && !DECL_ARTIFICIAL (access->base))
    {
      char *pretty_name = make_fancy_name (access->expr);

      DECL_NAME (repl) = get_identifier (pretty_name);
      obstack_free (&name_obstack, pretty_name);

      SET_DECL_DEBUG_EXPR (repl, access->expr);
      DECL_DEBUG_EXPR_IS_FROM (repl) = 1;
      DECL_IGNORED_P (repl) = 0;
    }

  DECL_IGNORED_P (repl) = DECL_IGNORED_P (access->base);
  TREE_NO_WARNING (repl) = TREE_NO_WARNING (access->base);

  if (dump_file)
    {
      fprintf (dump_file, "Created a replacement for ");
      print_generic_expr (dump_file, access->base, 0);
      fprintf (dump_file, " offset: %u, size: %u: ",
	       (unsigned) access->offset, (unsigned) access->size);
      print_generic_expr (dump_file, repl, 0);
      fprintf (dump_file, "\n");
    }
  sra_stats.replacements++;

  return repl;
}

/* Return ACCESS scalar replacement, create it if it does not exist yet.  */

static inline tree
get_access_replacement (struct access *access)
{
  gcc_assert (access->grp_to_be_replaced);

  if (!access->replacement_decl)
    access->replacement_decl = create_access_replacement (access);
  return access->replacement_decl;
}

/* Build a subtree of accesses rooted in *ACCESS, and move the pointer in the
   linked list along the way.  Stop when *ACCESS is NULL or the access pointed
   to it is not "within" the root.  */

static void
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

      build_access_subtree (access);
    }
}

/* Build a tree of access representatives, ACCESS is the pointer to the first
   one, others are linked in a list by the next_grp field.  Decide about scalar
   replacements on the way, return true iff any are to be created.  */

static void
build_access_trees (struct access *access)
{
  while (access)
    {
      struct access *root = access;

      build_access_subtree (&access);
      root->next_grp = access;
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
	  && !host_integerp (array_ref_low_bound (expr), 0))
	return true;
      expr = TREE_OPERAND (expr, 0);
    }
  return false;
}

/* Analyze the subtree of accesses rooted in ROOT, scheduling replacements when
   both seeming beneficial and when ALLOW_REPLACEMENTS allows it.  Also set
   all sorts of access flags appropriately along the way, notably always ser
   grp_read when MARK_READ is true and grp_write when MARK_WRITE is true.  */

static bool
analyze_access_subtree (struct access *root, bool allow_replacements,
			bool mark_read, bool mark_write)
{
  struct access *child;
  HOST_WIDE_INT limit = root->offset + root->size;
  HOST_WIDE_INT covered_to = root->offset;
  bool scalar = is_gimple_reg_type (root->type);
  bool hole = false, sth_created = false;
  bool direct_read = root->grp_read;

  if (mark_read)
    root->grp_read = true;
  else if (root->grp_read)
    mark_read = true;

  if (mark_write)
    root->grp_write = true;
  else if (root->grp_write)
    mark_write = true;

  if (root->grp_unscalarizable_region)
    allow_replacements = false;

  if (allow_replacements && expr_with_var_bounded_array_refs_p (root->expr))
    allow_replacements = false;

  for (child = root->first_child; child; child = child->next_sibling)
    {
      if (!hole && child->offset < covered_to)
	hole = true;
      else
	covered_to += child->size;

      sth_created |= analyze_access_subtree (child, allow_replacements,
					     mark_read, mark_write);

      root->grp_unscalarized_data |= child->grp_unscalarized_data;
      hole |= !child->grp_covered;
    }

  if (allow_replacements && scalar && !root->first_child
      && (root->grp_hint
	  || (direct_read && root->grp_write)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Marking ");
	  print_generic_expr (dump_file, root->base, 0);
	  fprintf (dump_file, " offset: %u, size: %u: ",
		   (unsigned) root->offset, (unsigned) root->size);
	  fprintf (dump_file, " to be replaced.\n");
	}

      root->grp_to_be_replaced = 1;
      sth_created = true;
      hole = false;
    }
  else if (covered_to < limit)
    hole = true;

  if (sth_created && !hole)
    {
      root->grp_covered = 1;
      return true;
    }
  if (root->grp_write || TREE_CODE (root->base) == PARM_DECL)
    root->grp_unscalarized_data = 1; /* not covered and written to */
  if (sth_created)
    return true;
  return false;
}

/* Analyze all access trees linked by next_grp by the means of
   analyze_access_subtree.  */
static bool
analyze_access_trees (struct access *access)
{
  bool ret = false;

  while (access)
    {
      if (analyze_access_subtree (access, true, false, false))
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
  tree expr = parent->base;;

  gcc_assert (!model->grp_unscalarizable_region);

  if (!build_ref_for_offset (&expr, TREE_TYPE (expr), new_offset,
			     model->type, false))
    return NULL;

  access = (struct access *) pool_alloc (access_pool);
  memset (access, 0, sizeof (struct access));
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
propagate_subacesses_accross_link (struct access *lacc, struct access *racc)
{
  struct access *rchild;
  HOST_WIDE_INT norm_delta = lacc->offset - racc->offset;
  bool ret = false;

  if (is_gimple_reg_type (lacc->type)
      || lacc->grp_unscalarizable_region
      || racc->grp_unscalarizable_region)
    return false;

  if (!lacc->first_child && !racc->first_child
      && is_gimple_reg_type (racc->type))
    {
      tree t = lacc->base;

      if (build_ref_for_offset (&t, TREE_TYPE (t), lacc->offset, racc->type,
				false))
	{
	  lacc->expr = t;
	  lacc->type = racc->type;
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
		ret |= propagate_subacesses_accross_link (new_acc, rchild);
	    }
	  continue;
	}

      /* If a (part of) a union field is on the RHS of an assignment, it can
	 have sub-accesses which do not make sense on the LHS (PR 40351).
	 Check that this is not the case.  */
      if (!build_ref_for_offset (NULL, TREE_TYPE (lacc->base), norm_offset,
				 rchild->type, false))
	continue;

      rchild->grp_hint = 1;
      new_acc = create_artificial_child_access (lacc, rchild, norm_offset);
      if (new_acc)
	{
	  ret = true;
	  if (racc->first_child)
	    propagate_subacesses_accross_link (new_acc, rchild);
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
	  if (propagate_subacesses_accross_link (lacc, racc)
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
  tree var;
  referenced_var_iterator rvi;
  int res = 0;

  FOR_EACH_REFERENCED_VAR (var, rvi)
    if (bitmap_bit_p (candidate_bitmap, DECL_UID (var)))
      {
	struct access *access;

	access = sort_and_splice_var_accesses (var);
	if (access)
	  build_access_trees (access);
	else
	  disqualify_candidate (var,
				"No or inhibitingly overlapping accesses.");
      }

  propagate_all_subaccesses ();

  FOR_EACH_REFERENCED_VAR (var, rvi)
    if (bitmap_bit_p (candidate_bitmap, DECL_UID (var)))
      {
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

  if (res)
    {
      statistics_counter_event (cfun, "Scalarized aggregates", res);
      return true;
    }
  else
    return false;
}

/* Return true iff a reference statement into aggregate AGG can be built for
   every single to-be-replaced accesses that is a child of ACCESS, its sibling
   or a child of its sibling. TOP_OFFSET is the offset from the processed
   access subtree that has to be subtracted from offset of each access.  */

static bool
ref_expr_for_all_replacements_p (struct access *access, tree agg,
				 HOST_WIDE_INT top_offset)
{
  do
    {
      if (access->grp_to_be_replaced
	  && !build_ref_for_offset (NULL, TREE_TYPE (agg),
				    access->offset - top_offset,
				    access->type, false))
	return false;

      if (access->first_child
	  && !ref_expr_for_all_replacements_p (access->first_child, agg,
					       top_offset))
	return false;

      access = access->next_sibling;
    }
  while (access);

  return true;
}

/* Generate statements copying scalar replacements of accesses within a subtree
   into or out of AGG.  ACCESS is the first child of the root of the subtree to
   be processed.  AGG is an aggregate type expression (can be a declaration but
   does not have to be, it can for example also be an indirect_ref).
   TOP_OFFSET is the offset of the processed subtree which has to be subtracted
   from offsets of individual accesses to get corresponding offsets for AGG.
   If CHUNK_SIZE is non-null, copy only replacements in the interval
   <start_offset, start_offset + chunk_size>, otherwise copy all.  GSI is a
   statement iterator used to place the new statements.  WRITE should be true
   when the statements should write from AGG to the replacement and false if
   vice versa.  if INSERT_AFTER is true, new statements will be added after the
   current statement in GSI, they will be added before the statement
   otherwise.  */

static void
generate_subtree_copies (struct access *access, tree agg,
			 HOST_WIDE_INT top_offset,
			 HOST_WIDE_INT start_offset, HOST_WIDE_INT chunk_size,
			 gimple_stmt_iterator *gsi, bool write,
			 bool insert_after)
{
  do
    {
      tree expr = unshare_expr (agg);

      if (chunk_size && access->offset >= start_offset + chunk_size)
	return;

      if (access->grp_to_be_replaced
	  && (chunk_size == 0
	      || access->offset + access->size > start_offset))
	{
	  tree repl = get_access_replacement (access);
	  bool ref_found;
	  gimple stmt;

	  ref_found = build_ref_for_offset (&expr, TREE_TYPE (agg),
					     access->offset - top_offset,
					     access->type, false);
	  gcc_assert (ref_found);

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

	  if (insert_after)
	    gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
	  else
	    gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	  update_stmt (stmt);
	  sra_stats.subtree_copies++;
	}

      if (access->first_child)
	generate_subtree_copies (access->first_child, agg, top_offset,
				 start_offset, chunk_size, gsi,
				 write, insert_after);

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
			bool insert_after)

{
  struct access *child;

  if (access->grp_to_be_replaced)
    {
      gimple stmt;

      stmt = gimple_build_assign (get_access_replacement (access),
				  fold_convert (access->type,
						integer_zero_node));
      if (insert_after)
	gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
      else
	gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
      update_stmt (stmt);
    }

  for (child = access->first_child; child; child = child->next_sibling)
    init_subtree_with_zero (child, gsi, insert_after);
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

/* Callback for scan_function.  Replace the expression EXPR with a scalar
   replacement if there is one and generate other statements to do type
   conversion or subtree copying if necessary.  GSI is used to place newly
   created statements, WRITE is true if the expression is being written to (it
   is on a LHS of a statement or output in an assembly statement).  */

static bool
sra_modify_expr (tree *expr, gimple_stmt_iterator *gsi, bool write,
		 void *data ATTRIBUTE_UNUSED)
{
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

  if (access->grp_to_be_replaced)
    {
      tree repl = get_access_replacement (access);
      /* If we replace a non-register typed access simply use the original
         access expression to extract the scalar component afterwards.
	 This happens if scalarizing a function return value or parameter
	 like in gcc.c-torture/execute/20041124-1.c, 20050316-1.c and
	 gcc.c-torture/compile/20011217-1.c.  */
      if (!is_gimple_reg_type (type))
	{
	  gimple stmt;
	  if (write)
	    {
	      tree ref = unshare_expr (access->expr);
	      if (access->grp_partial_lhs)
		ref = force_gimple_operand_gsi (gsi, ref, true, NULL_TREE,
						 false, GSI_NEW_STMT);
	      stmt = gimple_build_assign (repl, ref);
	      gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
	    }
	  else
	    {
	      if (access->grp_partial_lhs)
		repl = force_gimple_operand_gsi (gsi, repl, true, NULL_TREE,
						 true, GSI_SAME_STMT);
	      stmt = gimple_build_assign (unshare_expr (access->expr), repl);
	      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	    }
	}
      else
	{
	  gcc_assert (useless_type_conversion_p (type, access->type));
	  *expr = repl;
	}
      sra_stats.exprs++;
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
			       start_offset, chunk_size, gsi, write, write);
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
   base aggregate if there are unscalarized data or directly to LHS
   otherwise.  */

static enum unscalarized_data_handling
handle_unscalarized_data_in_subtree (struct access *top_racc, tree lhs,
				     gimple_stmt_iterator *gsi)
{
  if (top_racc->grp_unscalarized_data)
    {
      generate_subtree_copies (top_racc->first_child, top_racc->base, 0, 0, 0,
			       gsi, false, false);
      return SRA_UDH_RIGHT;
    }
  else
    {
      generate_subtree_copies (top_racc->first_child, lhs, top_racc->offset,
			       0, 0, gsi, false, false);
      return SRA_UDH_LEFT;
    }
}


/* Try to generate statements to load all sub-replacements in an access
   (sub)tree (LACC is the first child) from scalar replacements in the TOP_RACC
   (sub)tree.  If that is not possible, refresh the TOP_RACC base aggregate and
   load the accesses from it.  LEFT_OFFSET is the offset of the left whole
   subtree being copied, RIGHT_OFFSET is the same thing for the right subtree.
   GSI is stmt iterator used for statement insertions.  *REFRESHED is true iff
   the rhs top aggregate has already been refreshed by contents of its scalar
   reductions and is set to true if this function has to do it.  */

static void
load_assign_lhs_subreplacements (struct access *lacc, struct access *top_racc,
				 HOST_WIDE_INT left_offset,
				 HOST_WIDE_INT right_offset,
				 gimple_stmt_iterator *old_gsi,
				 gimple_stmt_iterator *new_gsi,
				 enum unscalarized_data_handling *refreshed,
				 tree lhs)
{
  location_t loc = EXPR_LOCATION (lacc->expr);
  do
    {
      if (lacc->grp_to_be_replaced)
	{
	  struct access *racc;
	  HOST_WIDE_INT offset = lacc->offset - left_offset + right_offset;
	  gimple stmt;
	  tree rhs;

	  racc = find_access_in_subtree (top_racc, offset, lacc->size);
	  if (racc && racc->grp_to_be_replaced)
	    {
	      rhs = get_access_replacement (racc);
	      if (!useless_type_conversion_p (lacc->type, racc->type))
		rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, lacc->type, rhs);
	    }
	  else
	    {
	      bool repl_found;

	      /* No suitable access on the right hand side, need to load from
		 the aggregate.  See if we have to update it first... */
	      if (*refreshed == SRA_UDH_NONE)
		*refreshed = handle_unscalarized_data_in_subtree (top_racc,
								  lhs, old_gsi);

	      if (*refreshed == SRA_UDH_LEFT)
		rhs = unshare_expr (lacc->expr);
	      else
		{
		  rhs = unshare_expr (top_racc->base);
		  repl_found = build_ref_for_offset (&rhs,
						     TREE_TYPE (top_racc->base),
						     offset, lacc->type, false);
		  gcc_assert (repl_found);
		}
	    }

	  stmt = gimple_build_assign (get_access_replacement (lacc), rhs);
	  gsi_insert_after (new_gsi, stmt, GSI_NEW_STMT);
	  update_stmt (stmt);
	  sra_stats.subreplacements++;
	}
      else if (*refreshed == SRA_UDH_NONE
	       && lacc->grp_read && !lacc->grp_covered)
	*refreshed = handle_unscalarized_data_in_subtree (top_racc, lhs,
							  old_gsi);

      if (lacc->first_child)
	load_assign_lhs_subreplacements (lacc->first_child, top_racc,
					 left_offset, right_offset,
					 old_gsi, new_gsi, refreshed, lhs);
      lacc = lacc->next_sibling;
    }
  while (lacc);
}

/* Modify assignments with a CONSTRUCTOR on their RHS.  STMT contains a pointer
   to the assignment and GSI is the statement iterator pointing at it.  Returns
   the same values as sra_modify_assign.  */

static enum scan_assign_result
sra_modify_constructor_assign (gimple *stmt, gimple_stmt_iterator *gsi)
{
  tree lhs = gimple_assign_lhs (*stmt);
  struct access *acc;

  acc = get_access_for_expr (lhs);
  if (!acc)
    return SRA_SA_NONE;

  if (VEC_length (constructor_elt,
		  CONSTRUCTOR_ELTS (gimple_assign_rhs1 (*stmt))) > 0)
    {
      /* I have never seen this code path trigger but if it can happen the
	 following should handle it gracefully.  */
      if (access_has_children_p (acc))
	generate_subtree_copies (acc->first_child, acc->base, 0, 0, 0, gsi,
				 true, true);
      return SRA_SA_PROCESSED;
    }

  if (acc->grp_covered)
    {
      init_subtree_with_zero (acc, gsi, false);
      unlink_stmt_vdef (*stmt);
      gsi_remove (gsi, true);
      return SRA_SA_REMOVED;
    }
  else
    {
      init_subtree_with_zero (acc, gsi, true);
      return SRA_SA_PROCESSED;
    }
}


/* Callback of scan_function to process assign statements.  It examines both
   sides of the statement, replaces them with a scalare replacement if there is
   one and generating copying of replacements if scalarized aggregates have been
   used in the assignment.  STMT is a pointer to the assign statement, GSI is
   used to hold generated statements for type conversions and subtree
   copying.  */

static enum scan_assign_result
sra_modify_assign (gimple *stmt, gimple_stmt_iterator *gsi,
		   void *data ATTRIBUTE_UNUSED)
{
  struct access *lacc, *racc;
  tree lhs, rhs;
  bool modify_this_stmt = false;
  bool force_gimple_rhs = false;
  location_t loc = gimple_location (*stmt);

  if (!gimple_assign_single_p (*stmt))
    return SRA_SA_NONE;
  lhs = gimple_assign_lhs (*stmt);
  rhs = gimple_assign_rhs1 (*stmt);

  if (TREE_CODE (rhs) == CONSTRUCTOR)
    return sra_modify_constructor_assign (stmt, gsi);

  if (TREE_CODE (rhs) == REALPART_EXPR || TREE_CODE (lhs) == REALPART_EXPR
      || TREE_CODE (rhs) == IMAGPART_EXPR || TREE_CODE (lhs) == IMAGPART_EXPR
      || TREE_CODE (rhs) == BIT_FIELD_REF || TREE_CODE (lhs) == BIT_FIELD_REF)
    {
      modify_this_stmt = sra_modify_expr (gimple_assign_rhs1_ptr (*stmt),
					  gsi, false, data);
      modify_this_stmt |= sra_modify_expr (gimple_assign_lhs_ptr (*stmt),
					   gsi, true, data);
      return modify_this_stmt ? SRA_SA_PROCESSED : SRA_SA_NONE;
    }

  lacc = get_access_for_expr (lhs);
  racc = get_access_for_expr (rhs);
  if (!lacc && !racc)
    return SRA_SA_NONE;

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

  if (modify_this_stmt)
    {
      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	{
	  /* If we can avoid creating a VIEW_CONVERT_EXPR do so.
	     ???  This should move to fold_stmt which we simply should
	     call after building a VIEW_CONVERT_EXPR here.  */
	  if (AGGREGATE_TYPE_P (TREE_TYPE (lhs))
	      && !access_has_children_p (lacc))
	    {
	      tree expr = unshare_expr (lhs);
	      if (build_ref_for_offset (&expr, TREE_TYPE (lhs), 0,
					TREE_TYPE (rhs), false))
		{
		  lhs = expr;
		  gimple_assign_set_lhs (*stmt, expr);
		}
	    }
	  else if (AGGREGATE_TYPE_P (TREE_TYPE (rhs))
		   && !access_has_children_p (racc))
	    {
	      tree expr = unshare_expr (rhs);
	      if (build_ref_for_offset (&expr, TREE_TYPE (rhs), 0,
					TREE_TYPE (lhs), false))
		rhs = expr;
	    }
	  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    {
	      rhs = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (lhs), rhs);
	      if (!is_gimple_reg (lhs))
		force_gimple_rhs = true;
	    }
	}

      if (force_gimple_rhs)
	rhs = force_gimple_operand_gsi (gsi, rhs, true, NULL_TREE,
					true, GSI_SAME_STMT);
      if (gimple_assign_rhs1 (*stmt) != rhs)
	{
	  gimple_assign_set_rhs_from_tree (gsi, rhs);
	  gcc_assert (*stmt == gsi_stmt (*gsi));
	}
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

  if (contains_view_convert_expr_p (rhs) || contains_view_convert_expr_p (lhs)
      || (access_has_children_p (racc)
	  && !ref_expr_for_all_replacements_p (racc, lhs, racc->offset))
      || (access_has_children_p (lacc)
	  && !ref_expr_for_all_replacements_p (lacc, rhs, lacc->offset)))
    {
      if (access_has_children_p (racc))
	generate_subtree_copies (racc->first_child, racc->base, 0, 0, 0,
				 gsi, false, false);
      if (access_has_children_p (lacc))
	generate_subtree_copies (lacc->first_child, lacc->base, 0, 0, 0,
				 gsi, true, true);
      sra_stats.separate_lhs_rhs_handling++;
    }
  else
    {
      if (access_has_children_p (lacc) && access_has_children_p (racc))
	{
	  gimple_stmt_iterator orig_gsi = *gsi;
	  enum unscalarized_data_handling refreshed;

	  if (lacc->grp_read && !lacc->grp_covered)
	    refreshed = handle_unscalarized_data_in_subtree (racc, lhs, gsi);
	  else
	    refreshed = SRA_UDH_NONE;

	  load_assign_lhs_subreplacements (lacc->first_child, racc,
					   lacc->offset, racc->offset,
					   &orig_gsi, gsi, &refreshed, lhs);
	  if (refreshed != SRA_UDH_RIGHT)
	    {
	      if (*stmt == gsi_stmt (*gsi))
		gsi_next (gsi);

	      unlink_stmt_vdef (*stmt);
	      gsi_remove (&orig_gsi, true);
	      sra_stats.deleted++;
	      return SRA_SA_REMOVED;
	    }
	}
      else
	{
	  if (access_has_children_p (racc))
	    {
	      if (!racc->grp_unscalarized_data)
		{
		  generate_subtree_copies (racc->first_child, lhs,
					   racc->offset, 0, 0, gsi,
					   false, false);
		  gcc_assert (*stmt == gsi_stmt (*gsi));
		  unlink_stmt_vdef (*stmt);
		  gsi_remove (gsi, true);
		  sra_stats.deleted++;
		  return SRA_SA_REMOVED;
		}
	      else
		generate_subtree_copies (racc->first_child, lhs,
					 racc->offset, 0, 0, gsi, false, true);
	    }
	  else if (access_has_children_p (lacc))
	    generate_subtree_copies (lacc->first_child, rhs, lacc->offset,
				     0, 0, gsi, true, true);
	}
    }
  return modify_this_stmt ? SRA_SA_PROCESSED : SRA_SA_NONE;
}

/* Generate statements initializing scalar replacements of parts of function
   parameters.  */

static void
initialize_parameter_reductions (void)
{
  gimple_stmt_iterator gsi;
  gimple_seq seq = NULL;
  tree parm;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm;
       parm = TREE_CHAIN (parm))
    {
      VEC (access_p, heap) *access_vec;
      struct access *access;

      if (!bitmap_bit_p (candidate_bitmap, DECL_UID (parm)))
	continue;
      access_vec = get_base_access_vector (parm);
      if (!access_vec)
	continue;

      if (!seq)
	{
	  seq = gimple_seq_alloc ();
	  gsi = gsi_start (seq);
	}

      for (access = VEC_index (access_p, access_vec, 0);
	   access;
	   access = access->next_grp)
	generate_subtree_copies (access, parm, 0, 0, 0, &gsi, true, true);
    }

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

  if (!scan_function (build_access_from_expr, build_accesses_from_assign, NULL,
		      true, NULL))
    goto out;

  if (!analyze_all_variable_accesses ())
    goto out;

  scan_function (sra_modify_expr, sra_modify_assign, NULL,
		 false, NULL);
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

  ret = TODO_update_ssa;

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
  return flag_tree_sra != 0;
}


struct gimple_opt_pass pass_sra_early =
{
 {
  GIMPLE_PASS,
  "esra",	 			/* name */
  gate_intra_sra,			/* gate */
  early_intra_sra,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa,                  /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
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
  "sra",	 			/* name */
  gate_intra_sra,			/* gate */
  late_intra_sra,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SRA,				/* tv_id */
  PROP_cfg | PROP_ssa,                  /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  TODO_update_address_taken,		/* todo_flags_start */
  TODO_dump_func
  | TODO_update_ssa
  | TODO_ggc_collect
  | TODO_verify_ssa			/* todo_flags_finish */
 }
};
