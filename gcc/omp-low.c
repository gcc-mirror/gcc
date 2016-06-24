/* Lowering pass for OMP directives.  Converts OMP directives into explicit
   calls to the runtime library (libgomp), data marshalling to implement data
   sharing and copying clauses, offloading to accelerators, and more.

   Contributed by Diego Novillo <dnovillo@redhat.com>

   Copyright (C) 2005-2016 Free Software Foundation, Inc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "pretty-print.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "flags.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "except.h"
#include "splay-tree.h"
#include "cfgloop.h"
#include "common/common-target.h"
#include "omp-low.h"
#include "gimple-low.h"
#include "tree-cfgcleanup.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "tree-nested.h"
#include "tree-eh.h"
#include "cilk.h"
#include "context.h"
#include "lto-section-names.h"
#include "gomp-constants.h"
#include "gimple-pretty-print.h"
#include "symbol-summary.h"
#include "hsa.h"
#include "params.h"

/* Lowering of OMP parallel and workshare constructs proceeds in two
   phases.  The first phase scans the function looking for OMP statements
   and then for variables that must be replaced to satisfy data sharing
   clauses.  The second phase expands code for the constructs, as well as
   re-gimplifying things when variables have been replaced with complex
   expressions.

   Final code generation is done by pass_expand_omp.  The flowgraph is
   scanned for regions which are then moved to a new
   function, to be invoked by the thread library, or offloaded.  */

/* OMP region information.  Every parallel and workshare
   directive is enclosed between two markers, the OMP_* directive
   and a corresponding GIMPLE_OMP_RETURN statement.  */

struct omp_region
{
  /* The enclosing region.  */
  struct omp_region *outer;

  /* First child region.  */
  struct omp_region *inner;

  /* Next peer region.  */
  struct omp_region *next;

  /* Block containing the omp directive as its last stmt.  */
  basic_block entry;

  /* Block containing the GIMPLE_OMP_RETURN as its last stmt.  */
  basic_block exit;

  /* Block containing the GIMPLE_OMP_CONTINUE as its last stmt.  */
  basic_block cont;

  /* If this is a combined parallel+workshare region, this is a list
     of additional arguments needed by the combined parallel+workshare
     library call.  */
  vec<tree, va_gc> *ws_args;

  /* The code for the omp directive of this region.  */
  enum gimple_code type;

  /* Schedule kind, only used for GIMPLE_OMP_FOR type regions.  */
  enum omp_clause_schedule_kind sched_kind;

  /* Schedule modifiers.  */
  unsigned char sched_modifiers;

  /* True if this is a combined parallel+workshare region.  */
  bool is_combined_parallel;

  /* The ordered stmt if type is GIMPLE_OMP_ORDERED and it has
     a depend clause.  */
  gomp_ordered *ord_stmt;
};

/* Context structure.  Used to store information about each parallel
   directive in the code.  */

struct omp_context
{
  /* This field must be at the beginning, as we do "inheritance": Some
     callback functions for tree-inline.c (e.g., omp_copy_decl)
     receive a copy_body_data pointer that is up-casted to an
     omp_context pointer.  */
  copy_body_data cb;

  /* The tree of contexts corresponding to the encountered constructs.  */
  struct omp_context *outer;
  gimple *stmt;

  /* Map variables to fields in a structure that allows communication
     between sending and receiving threads.  */
  splay_tree field_map;
  tree record_type;
  tree sender_decl;
  tree receiver_decl;

  /* These are used just by task contexts, if task firstprivate fn is
     needed.  srecord_type is used to communicate from the thread
     that encountered the task construct to task firstprivate fn,
     record_type is allocated by GOMP_task, initialized by task firstprivate
     fn and passed to the task body fn.  */
  splay_tree sfield_map;
  tree srecord_type;

  /* A chain of variables to add to the top-level block surrounding the
     construct.  In the case of a parallel, this is in the child function.  */
  tree block_vars;

  /* Label to which GOMP_cancel{,llation_point} and explicit and implicit
     barriers should jump to during omplower pass.  */
  tree cancel_label;

  /* What to do with variables with implicitly determined sharing
     attributes.  */
  enum omp_clause_default_kind default_kind;

  /* Nesting depth of this context.  Used to beautify error messages re
     invalid gotos.  The outermost ctx is depth 1, with depth 0 being
     reserved for the main body of the function.  */
  int depth;

  /* True if this parallel directive is nested within another.  */
  bool is_nested;

  /* True if this construct can be cancelled.  */
  bool cancellable;
};

/* A structure holding the elements of:
   for (V = N1; V cond N2; V += STEP) [...] */

struct omp_for_data_loop
{
  tree v, n1, n2, step;
  enum tree_code cond_code;
};

/* A structure describing the main elements of a parallel loop.  */

struct omp_for_data
{
  struct omp_for_data_loop loop;
  tree chunk_size;
  gomp_for *for_stmt;
  tree pre, iter_type;
  int collapse;
  int ordered;
  bool have_nowait, have_ordered, simd_schedule;
  unsigned char sched_modifiers;
  enum omp_clause_schedule_kind sched_kind;
  struct omp_for_data_loop *loops;
};

/* Describe the OpenACC looping structure of a function.  The entire
   function is held in a 'NULL' loop.  */

struct oacc_loop
{
  oacc_loop *parent; /* Containing loop.  */

  oacc_loop *child; /* First inner loop.  */

  oacc_loop *sibling; /* Next loop within same parent.  */

  location_t loc; /* Location of the loop start.  */

  gcall *marker; /* Initial head marker.  */
  
  gcall *heads[GOMP_DIM_MAX];  /* Head marker functions. */
  gcall *tails[GOMP_DIM_MAX];  /* Tail marker functions. */

  tree routine;  /* Pseudo-loop enclosing a routine.  */

  unsigned mask;   /* Partitioning mask.  */
  unsigned inner;  /* Partitioning of inner loops.  */
  unsigned flags;  /* Partitioning flags.  */
  unsigned ifns;   /* Contained loop abstraction functions.  */
  tree chunk_size; /* Chunk size.  */
  gcall *head_end; /* Final marker of head sequence.  */
};

/*  Flags for an OpenACC loop.  */

enum oacc_loop_flags {
  OLF_SEQ	= 1u << 0,  /* Explicitly sequential  */
  OLF_AUTO	= 1u << 1,	/* Compiler chooses axes.  */
  OLF_INDEPENDENT = 1u << 2,	/* Iterations are known independent.  */
  OLF_GANG_STATIC = 1u << 3,	/* Gang partitioning is static (has op). */

  /* Explicitly specified loop axes.  */
  OLF_DIM_BASE = 4,
  OLF_DIM_GANG   = 1u << (OLF_DIM_BASE + GOMP_DIM_GANG),
  OLF_DIM_WORKER = 1u << (OLF_DIM_BASE + GOMP_DIM_WORKER),
  OLF_DIM_VECTOR = 1u << (OLF_DIM_BASE + GOMP_DIM_VECTOR),

  OLF_MAX = OLF_DIM_BASE + GOMP_DIM_MAX
};


static splay_tree all_contexts;
static int taskreg_nesting_level;
static int target_nesting_level;
static struct omp_region *root_omp_region;
static bitmap task_shared_vars;
static vec<omp_context *> taskreg_contexts;
static bool omp_any_child_fn_dumped;

static void scan_omp (gimple_seq *, omp_context *);
static tree scan_omp_1_op (tree *, int *, void *);
static gphi *find_phi_with_arg_on_edge (tree, edge);

#define WALK_SUBSTMTS  \
    case GIMPLE_BIND: \
    case GIMPLE_TRY: \
    case GIMPLE_CATCH: \
    case GIMPLE_EH_FILTER: \
    case GIMPLE_TRANSACTION: \
      /* The sub-statements for these should be walked.  */ \
      *handled_ops_p = false; \
      break;

/* Return true if CTX corresponds to an oacc parallel region.  */

static bool
is_oacc_parallel (omp_context *ctx)
{
  enum gimple_code outer_type = gimple_code (ctx->stmt);
  return ((outer_type == GIMPLE_OMP_TARGET)
	  && (gimple_omp_target_kind (ctx->stmt)
	      == GF_OMP_TARGET_KIND_OACC_PARALLEL));
}

/* Return true if CTX corresponds to an oacc kernels region.  */

static bool
is_oacc_kernels (omp_context *ctx)
{
  enum gimple_code outer_type = gimple_code (ctx->stmt);
  return ((outer_type == GIMPLE_OMP_TARGET)
	  && (gimple_omp_target_kind (ctx->stmt)
	      == GF_OMP_TARGET_KIND_OACC_KERNELS));
}

/* If DECL is the artificial dummy VAR_DECL created for non-static
   data member privatization, return the underlying "this" parameter,
   otherwise return NULL.  */

tree
omp_member_access_dummy_var (tree decl)
{
  if (!VAR_P (decl)
      || !DECL_ARTIFICIAL (decl)
      || !DECL_IGNORED_P (decl)
      || !DECL_HAS_VALUE_EXPR_P (decl)
      || !lang_hooks.decls.omp_disregard_value_expr (decl, false))
    return NULL_TREE;

  tree v = DECL_VALUE_EXPR (decl);
  if (TREE_CODE (v) != COMPONENT_REF)
    return NULL_TREE;

  while (1)
    switch (TREE_CODE (v))
      {
      case COMPONENT_REF:
      case MEM_REF:
      case INDIRECT_REF:
      CASE_CONVERT:
      case POINTER_PLUS_EXPR:
	v = TREE_OPERAND (v, 0);
	continue;
      case PARM_DECL:
	if (DECL_CONTEXT (v) == current_function_decl
	    && DECL_ARTIFICIAL (v)
	    && TREE_CODE (TREE_TYPE (v)) == POINTER_TYPE)
	  return v;
	return NULL_TREE;
      default:
	return NULL_TREE;
      }
}

/* Helper for unshare_and_remap, called through walk_tree.  */

static tree
unshare_and_remap_1 (tree *tp, int *walk_subtrees, void *data)
{
  tree *pair = (tree *) data;
  if (*tp == pair[0])
    {
      *tp = unshare_expr (pair[1]);
      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Return unshare_expr (X) with all occurrences of FROM
   replaced with TO.  */

static tree
unshare_and_remap (tree x, tree from, tree to)
{
  tree pair[2] = { from, to };
  x = unshare_expr (x);
  walk_tree (&x, unshare_and_remap_1, pair, NULL);
  return x;
}

/* Holds offload tables with decls.  */
vec<tree, va_gc> *offload_funcs, *offload_vars;

/* Convenience function for calling scan_omp_1_op on tree operands.  */

static inline tree
scan_omp_op (tree *tp, omp_context *ctx)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.info = ctx;
  wi.want_locations = true;

  return walk_tree (tp, scan_omp_1_op, &wi, NULL);
}

static void lower_omp (gimple_seq *, omp_context *);
static tree lookup_decl_in_outer_ctx (tree, omp_context *);
static tree maybe_lookup_decl_in_outer_ctx (tree, omp_context *);

/* Find an OMP clause of type KIND within CLAUSES.  */

tree
find_omp_clause (tree clauses, enum omp_clause_code kind)
{
  for (; clauses ; clauses = OMP_CLAUSE_CHAIN (clauses))
    if (OMP_CLAUSE_CODE (clauses) == kind)
      return clauses;

  return NULL_TREE;
}

/* Return true if CTX is for an omp parallel.  */

static inline bool
is_parallel_ctx (omp_context *ctx)
{
  return gimple_code (ctx->stmt) == GIMPLE_OMP_PARALLEL;
}


/* Return true if CTX is for an omp task.  */

static inline bool
is_task_ctx (omp_context *ctx)
{
  return gimple_code (ctx->stmt) == GIMPLE_OMP_TASK;
}


/* Return true if CTX is for an omp taskloop.  */

static inline bool
is_taskloop_ctx (omp_context *ctx)
{
  return gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	 && gimple_omp_for_kind (ctx->stmt) == GF_OMP_FOR_KIND_TASKLOOP;
}


/* Return true if CTX is for an omp parallel or omp task.  */

static inline bool
is_taskreg_ctx (omp_context *ctx)
{
  return is_parallel_ctx (ctx) || is_task_ctx (ctx);
}


/* Return true if REGION is a combined parallel+workshare region.  */

static inline bool
is_combined_parallel (struct omp_region *region)
{
  return region->is_combined_parallel;
}

/* Adjust *COND_CODE and *N2 so that the former is either LT_EXPR or
   GT_EXPR.  */

static void
adjust_for_condition (location_t loc, enum tree_code *cond_code, tree *n2)
{
  switch (*cond_code)
    {
    case LT_EXPR:
    case GT_EXPR:
    case NE_EXPR:
      break;
    case LE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	*n2 = fold_build_pointer_plus_hwi_loc (loc, *n2, 1);
      else
	*n2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = LT_EXPR;
      break;
    case GE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	*n2 = fold_build_pointer_plus_hwi_loc (loc, *n2, -1);
      else
	*n2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = GT_EXPR;
      break;
    default:
      gcc_unreachable ();
    }
}

/* Return the looping step from INCR, extracted from the step of a gimple omp
   for statement.  */

static tree
get_omp_for_step_from_incr (location_t loc, tree incr)
{
  tree step;
  switch (TREE_CODE (incr))
    {
    case PLUS_EXPR:
      step = TREE_OPERAND (incr, 1);
      break;
    case POINTER_PLUS_EXPR:
      step = fold_convert (ssizetype, TREE_OPERAND (incr, 1));
      break;
    case MINUS_EXPR:
      step = TREE_OPERAND (incr, 1);
      step = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (step), step);
      break;
    default:
      gcc_unreachable ();
    }
  return step;
}

/* Extract the header elements of parallel loop FOR_STMT and store
   them into *FD.  */

static void
extract_omp_for_data (gomp_for *for_stmt, struct omp_for_data *fd,
		      struct omp_for_data_loop *loops)
{
  tree t, var, *collapse_iter, *collapse_count;
  tree count = NULL_TREE, iter_type = long_integer_type_node;
  struct omp_for_data_loop *loop;
  int i;
  struct omp_for_data_loop dummy_loop;
  location_t loc = gimple_location (for_stmt);
  bool simd = gimple_omp_for_kind (for_stmt) & GF_OMP_FOR_SIMD;
  bool distribute = gimple_omp_for_kind (for_stmt)
		    == GF_OMP_FOR_KIND_DISTRIBUTE;
  bool taskloop = gimple_omp_for_kind (for_stmt)
		  == GF_OMP_FOR_KIND_TASKLOOP;
  tree iterv, countv;

  fd->for_stmt = for_stmt;
  fd->pre = NULL;
  if (gimple_omp_for_collapse (for_stmt) > 1)
    fd->loops = loops;
  else
    fd->loops = &fd->loop;

  fd->have_nowait = distribute || simd;
  fd->have_ordered = false;
  fd->collapse = 1;
  fd->ordered = 0;
  fd->sched_kind = OMP_CLAUSE_SCHEDULE_STATIC;
  fd->sched_modifiers = 0;
  fd->chunk_size = NULL_TREE;
  fd->simd_schedule = false;
  if (gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_CILKFOR)
    fd->sched_kind = OMP_CLAUSE_SCHEDULE_CILKFOR;
  collapse_iter = NULL;
  collapse_count = NULL;

  for (t = gimple_omp_for_clauses (for_stmt); t ; t = OMP_CLAUSE_CHAIN (t))
    switch (OMP_CLAUSE_CODE (t))
      {
      case OMP_CLAUSE_NOWAIT:
	fd->have_nowait = true;
	break;
      case OMP_CLAUSE_ORDERED:
	fd->have_ordered = true;
	if (OMP_CLAUSE_ORDERED_EXPR (t))
	  fd->ordered = tree_to_shwi (OMP_CLAUSE_ORDERED_EXPR (t));
	break;
      case OMP_CLAUSE_SCHEDULE:
	gcc_assert (!distribute && !taskloop);
	fd->sched_kind
	  = (enum omp_clause_schedule_kind)
	    (OMP_CLAUSE_SCHEDULE_KIND (t) & OMP_CLAUSE_SCHEDULE_MASK);
	fd->sched_modifiers = (OMP_CLAUSE_SCHEDULE_KIND (t)
			       & ~OMP_CLAUSE_SCHEDULE_MASK);
	fd->chunk_size = OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (t);
	fd->simd_schedule = OMP_CLAUSE_SCHEDULE_SIMD (t);
	break;
      case OMP_CLAUSE_DIST_SCHEDULE:
	gcc_assert (distribute);
	fd->chunk_size = OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (t);
	break;
      case OMP_CLAUSE_COLLAPSE:
	fd->collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (t));
	if (fd->collapse > 1)
	  {
	    collapse_iter = &OMP_CLAUSE_COLLAPSE_ITERVAR (t);
	    collapse_count = &OMP_CLAUSE_COLLAPSE_COUNT (t);
	  }
	break;
      default:
	break;
      }
  if (fd->ordered && fd->collapse == 1 && loops != NULL)
    {
      fd->loops = loops;
      iterv = NULL_TREE;
      countv = NULL_TREE;
      collapse_iter = &iterv;
      collapse_count = &countv;
    }

  /* FIXME: for now map schedule(auto) to schedule(static).
     There should be analysis to determine whether all iterations
     are approximately the same amount of work (then schedule(static)
     is best) or if it varies (then schedule(dynamic,N) is better).  */
  if (fd->sched_kind == OMP_CLAUSE_SCHEDULE_AUTO)
    {
      fd->sched_kind = OMP_CLAUSE_SCHEDULE_STATIC;
      gcc_assert (fd->chunk_size == NULL);
    }
  gcc_assert (fd->collapse == 1 || collapse_iter != NULL);
  if (taskloop)
    fd->sched_kind = OMP_CLAUSE_SCHEDULE_RUNTIME;
  if (fd->sched_kind == OMP_CLAUSE_SCHEDULE_RUNTIME)
    gcc_assert (fd->chunk_size == NULL);
  else if (fd->chunk_size == NULL)
    {
      /* We only need to compute a default chunk size for ordered
	 static loops and dynamic loops.  */
      if (fd->sched_kind != OMP_CLAUSE_SCHEDULE_STATIC
	  || fd->have_ordered)
	fd->chunk_size = (fd->sched_kind == OMP_CLAUSE_SCHEDULE_STATIC)
			 ? integer_zero_node : integer_one_node;
    }

  int cnt = fd->ordered ? fd->ordered : fd->collapse;
  for (i = 0; i < cnt; i++)
    {
      if (i == 0 && fd->collapse == 1 && (fd->ordered == 0 || loops == NULL))
	loop = &fd->loop;
      else if (loops != NULL)
	loop = loops + i;
      else
	loop = &dummy_loop;

      loop->v = gimple_omp_for_index (for_stmt, i);
      gcc_assert (SSA_VAR_P (loop->v));
      gcc_assert (TREE_CODE (TREE_TYPE (loop->v)) == INTEGER_TYPE
		  || TREE_CODE (TREE_TYPE (loop->v)) == POINTER_TYPE);
      var = TREE_CODE (loop->v) == SSA_NAME ? SSA_NAME_VAR (loop->v) : loop->v;
      loop->n1 = gimple_omp_for_initial (for_stmt, i);

      loop->cond_code = gimple_omp_for_cond (for_stmt, i);
      loop->n2 = gimple_omp_for_final (for_stmt, i);
      gcc_assert (loop->cond_code != NE_EXPR
		  || gimple_omp_for_kind (for_stmt) == GF_OMP_FOR_KIND_CILKSIMD
		  || gimple_omp_for_kind (for_stmt) == GF_OMP_FOR_KIND_CILKFOR);
      adjust_for_condition (loc, &loop->cond_code, &loop->n2);

      t = gimple_omp_for_incr (for_stmt, i);
      gcc_assert (TREE_OPERAND (t, 0) == var);
      loop->step = get_omp_for_step_from_incr (loc, t);

      if (simd
	  || (fd->sched_kind == OMP_CLAUSE_SCHEDULE_STATIC
	      && !fd->have_ordered))
	{
	  if (fd->collapse == 1)
	    iter_type = TREE_TYPE (loop->v);
	  else if (i == 0
		   || TYPE_PRECISION (iter_type)
		      < TYPE_PRECISION (TREE_TYPE (loop->v)))
	    iter_type
	      = build_nonstandard_integer_type
		  (TYPE_PRECISION (TREE_TYPE (loop->v)), 1);
	}
      else if (iter_type != long_long_unsigned_type_node)
	{
	  if (POINTER_TYPE_P (TREE_TYPE (loop->v)))
	    iter_type = long_long_unsigned_type_node;
	  else if (TYPE_UNSIGNED (TREE_TYPE (loop->v))
		   && TYPE_PRECISION (TREE_TYPE (loop->v))
		      >= TYPE_PRECISION (iter_type))
	    {
	      tree n;

	      if (loop->cond_code == LT_EXPR)
		n = fold_build2_loc (loc,
				 PLUS_EXPR, TREE_TYPE (loop->v),
				 loop->n2, loop->step);
	      else
		n = loop->n1;
	      if (TREE_CODE (n) != INTEGER_CST
		  || tree_int_cst_lt (TYPE_MAX_VALUE (iter_type), n))
		iter_type = long_long_unsigned_type_node;
	    }
	  else if (TYPE_PRECISION (TREE_TYPE (loop->v))
		   > TYPE_PRECISION (iter_type))
	    {
	      tree n1, n2;

	      if (loop->cond_code == LT_EXPR)
		{
		  n1 = loop->n1;
		  n2 = fold_build2_loc (loc,
				    PLUS_EXPR, TREE_TYPE (loop->v),
				    loop->n2, loop->step);
		}
	      else
		{
		  n1 = fold_build2_loc (loc,
				    MINUS_EXPR, TREE_TYPE (loop->v),
				    loop->n2, loop->step);
		  n2 = loop->n1;
		}
	      if (TREE_CODE (n1) != INTEGER_CST
		  || TREE_CODE (n2) != INTEGER_CST
		  || !tree_int_cst_lt (TYPE_MIN_VALUE (iter_type), n1)
		  || !tree_int_cst_lt (n2, TYPE_MAX_VALUE (iter_type)))
		iter_type = long_long_unsigned_type_node;
	    }
	}

      if (i >= fd->collapse)
	continue;

      if (collapse_count && *collapse_count == NULL)
	{
	  t = fold_binary (loop->cond_code, boolean_type_node,
			   fold_convert (TREE_TYPE (loop->v), loop->n1),
			   fold_convert (TREE_TYPE (loop->v), loop->n2));
	  if (t && integer_zerop (t))
	    count = build_zero_cst (long_long_unsigned_type_node);
	  else if ((i == 0 || count != NULL_TREE)
		   && TREE_CODE (TREE_TYPE (loop->v)) == INTEGER_TYPE
		   && TREE_CONSTANT (loop->n1)
		   && TREE_CONSTANT (loop->n2)
		   && TREE_CODE (loop->step) == INTEGER_CST)
	    {
	      tree itype = TREE_TYPE (loop->v);

	      if (POINTER_TYPE_P (itype))
		itype = signed_type_for (itype);
	      t = build_int_cst (itype, (loop->cond_code == LT_EXPR ? -1 : 1));
	      t = fold_build2_loc (loc,
			       PLUS_EXPR, itype,
			       fold_convert_loc (loc, itype, loop->step), t);
	      t = fold_build2_loc (loc, PLUS_EXPR, itype, t,
			       fold_convert_loc (loc, itype, loop->n2));
	      t = fold_build2_loc (loc, MINUS_EXPR, itype, t,
			       fold_convert_loc (loc, itype, loop->n1));
	      if (TYPE_UNSIGNED (itype) && loop->cond_code == GT_EXPR)
		t = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype,
				 fold_build1_loc (loc, NEGATE_EXPR, itype, t),
				 fold_build1_loc (loc, NEGATE_EXPR, itype,
					      fold_convert_loc (loc, itype,
								loop->step)));
	      else
		t = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype, t,
				 fold_convert_loc (loc, itype, loop->step));
	      t = fold_convert_loc (loc, long_long_unsigned_type_node, t);
	      if (count != NULL_TREE)
		count = fold_build2_loc (loc,
				     MULT_EXPR, long_long_unsigned_type_node,
				     count, t);
	      else
		count = t;
	      if (TREE_CODE (count) != INTEGER_CST)
		count = NULL_TREE;
	    }
	  else if (count && !integer_zerop (count))
	    count = NULL_TREE;
	}
    }

  if (count
      && !simd
      && (fd->sched_kind != OMP_CLAUSE_SCHEDULE_STATIC
	  || fd->have_ordered))
    {
      if (!tree_int_cst_lt (count, TYPE_MAX_VALUE (long_integer_type_node)))
	iter_type = long_long_unsigned_type_node;
      else
	iter_type = long_integer_type_node;
    }
  else if (collapse_iter && *collapse_iter != NULL)
    iter_type = TREE_TYPE (*collapse_iter);
  fd->iter_type = iter_type;
  if (collapse_iter && *collapse_iter == NULL)
    *collapse_iter = create_tmp_var (iter_type, ".iter");
  if (collapse_count && *collapse_count == NULL)
    {
      if (count)
	*collapse_count = fold_convert_loc (loc, iter_type, count);
      else
	*collapse_count = create_tmp_var (iter_type, ".count");
    }

  if (fd->collapse > 1 || (fd->ordered && loops))
    {
      fd->loop.v = *collapse_iter;
      fd->loop.n1 = build_int_cst (TREE_TYPE (fd->loop.v), 0);
      fd->loop.n2 = *collapse_count;
      fd->loop.step = build_int_cst (TREE_TYPE (fd->loop.v), 1);
      fd->loop.cond_code = LT_EXPR;
    }
  else if (loops)
    loops[0] = fd->loop;
}


/* Given two blocks PAR_ENTRY_BB and WS_ENTRY_BB such that WS_ENTRY_BB
   is the immediate dominator of PAR_ENTRY_BB, return true if there
   are no data dependencies that would prevent expanding the parallel
   directive at PAR_ENTRY_BB as a combined parallel+workshare region.

   When expanding a combined parallel+workshare region, the call to
   the child function may need additional arguments in the case of
   GIMPLE_OMP_FOR regions.  In some cases, these arguments are
   computed out of variables passed in from the parent to the child
   via 'struct .omp_data_s'.  For instance:

	#pragma omp parallel for schedule (guided, i * 4)
	for (j ...)

   Is lowered into:

   	# BLOCK 2 (PAR_ENTRY_BB)
	.omp_data_o.i = i;
	#pragma omp parallel [child fn: bar.omp_fn.0 ( ..., D.1598)

	# BLOCK 3 (WS_ENTRY_BB)
	.omp_data_i = &.omp_data_o;
	D.1667 = .omp_data_i->i;
	D.1598 = D.1667 * 4;
	#pragma omp for schedule (guided, D.1598)

   When we outline the parallel region, the call to the child function
   'bar.omp_fn.0' will need the value D.1598 in its argument list, but
   that value is computed *after* the call site.  So, in principle we
   cannot do the transformation.

   To see whether the code in WS_ENTRY_BB blocks the combined
   parallel+workshare call, we collect all the variables used in the
   GIMPLE_OMP_FOR header check whether they appear on the LHS of any
   statement in WS_ENTRY_BB.  If so, then we cannot emit the combined
   call.

   FIXME.  If we had the SSA form built at this point, we could merely
   hoist the code in block 3 into block 2 and be done with it.  But at
   this point we don't have dataflow information and though we could
   hack something up here, it is really not worth the aggravation.  */

static bool
workshare_safe_to_combine_p (basic_block ws_entry_bb)
{
  struct omp_for_data fd;
  gimple *ws_stmt = last_stmt (ws_entry_bb);

  if (gimple_code (ws_stmt) == GIMPLE_OMP_SECTIONS)
    return true;

  gcc_assert (gimple_code (ws_stmt) == GIMPLE_OMP_FOR);

  extract_omp_for_data (as_a <gomp_for *> (ws_stmt), &fd, NULL);

  if (fd.collapse > 1 && TREE_CODE (fd.loop.n2) != INTEGER_CST)
    return false;
  if (fd.iter_type != long_integer_type_node)
    return false;

  /* FIXME.  We give up too easily here.  If any of these arguments
     are not constants, they will likely involve variables that have
     been mapped into fields of .omp_data_s for sharing with the child
     function.  With appropriate data flow, it would be possible to
     see through this.  */
  if (!is_gimple_min_invariant (fd.loop.n1)
      || !is_gimple_min_invariant (fd.loop.n2)
      || !is_gimple_min_invariant (fd.loop.step)
      || (fd.chunk_size && !is_gimple_min_invariant (fd.chunk_size)))
    return false;

  return true;
}


static int omp_max_vf (void);

/* Adjust CHUNK_SIZE from SCHEDULE clause, depending on simd modifier
   presence (SIMD_SCHEDULE).  */

static tree
omp_adjust_chunk_size (tree chunk_size, bool simd_schedule)
{
  if (!simd_schedule)
    return chunk_size;

  int vf = omp_max_vf ();
  if (vf == 1)
    return chunk_size;

  tree type = TREE_TYPE (chunk_size);
  chunk_size = fold_build2 (PLUS_EXPR, type, chunk_size,
			    build_int_cst (type, vf - 1));
  return fold_build2 (BIT_AND_EXPR, type, chunk_size,
		      build_int_cst (type, -vf));
}


/* Collect additional arguments needed to emit a combined
   parallel+workshare call.  WS_STMT is the workshare directive being
   expanded.  */

static vec<tree, va_gc> *
get_ws_args_for (gimple *par_stmt, gimple *ws_stmt)
{
  tree t;
  location_t loc = gimple_location (ws_stmt);
  vec<tree, va_gc> *ws_args;

  if (gomp_for *for_stmt = dyn_cast <gomp_for *> (ws_stmt))
    {
      struct omp_for_data fd;
      tree n1, n2;

      extract_omp_for_data (for_stmt, &fd, NULL);
      n1 = fd.loop.n1;
      n2 = fd.loop.n2;

      if (gimple_omp_for_combined_into_p (for_stmt))
	{
	  tree innerc
	    = find_omp_clause (gimple_omp_parallel_clauses (par_stmt),
			       OMP_CLAUSE__LOOPTEMP_);
	  gcc_assert (innerc);
	  n1 = OMP_CLAUSE_DECL (innerc);
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  gcc_assert (innerc);
	  n2 = OMP_CLAUSE_DECL (innerc);
	}

      vec_alloc (ws_args, 3 + (fd.chunk_size != 0));

      t = fold_convert_loc (loc, long_integer_type_node, n1);
      ws_args->quick_push (t);

      t = fold_convert_loc (loc, long_integer_type_node, n2);
      ws_args->quick_push (t);

      t = fold_convert_loc (loc, long_integer_type_node, fd.loop.step);
      ws_args->quick_push (t);

      if (fd.chunk_size)
	{
	  t = fold_convert_loc (loc, long_integer_type_node, fd.chunk_size);
	  t = omp_adjust_chunk_size (t, fd.simd_schedule);
	  ws_args->quick_push (t);
	}

      return ws_args;
    }
  else if (gimple_code (ws_stmt) == GIMPLE_OMP_SECTIONS)
    {
      /* Number of sections is equal to the number of edges from the
	 GIMPLE_OMP_SECTIONS_SWITCH statement, except for the one to
	 the exit of the sections region.  */
      basic_block bb = single_succ (gimple_bb (ws_stmt));
      t = build_int_cst (unsigned_type_node, EDGE_COUNT (bb->succs) - 1);
      vec_alloc (ws_args, 1);
      ws_args->quick_push (t);
      return ws_args;
    }

  gcc_unreachable ();
}


/* Discover whether REGION is a combined parallel+workshare region.  */

static void
determine_parallel_type (struct omp_region *region)
{
  basic_block par_entry_bb, par_exit_bb;
  basic_block ws_entry_bb, ws_exit_bb;

  if (region == NULL || region->inner == NULL
      || region->exit == NULL || region->inner->exit == NULL
      || region->inner->cont == NULL)
    return;

  /* We only support parallel+for and parallel+sections.  */
  if (region->type != GIMPLE_OMP_PARALLEL
      || (region->inner->type != GIMPLE_OMP_FOR
	  && region->inner->type != GIMPLE_OMP_SECTIONS))
    return;

  /* Check for perfect nesting PAR_ENTRY_BB -> WS_ENTRY_BB and
     WS_EXIT_BB -> PAR_EXIT_BB.  */
  par_entry_bb = region->entry;
  par_exit_bb = region->exit;
  ws_entry_bb = region->inner->entry;
  ws_exit_bb = region->inner->exit;

  if (single_succ (par_entry_bb) == ws_entry_bb
      && single_succ (ws_exit_bb) == par_exit_bb
      && workshare_safe_to_combine_p (ws_entry_bb)
      && (gimple_omp_parallel_combined_p (last_stmt (par_entry_bb))
	  || (last_and_only_stmt (ws_entry_bb)
	      && last_and_only_stmt (par_exit_bb))))
    {
      gimple *par_stmt = last_stmt (par_entry_bb);
      gimple *ws_stmt = last_stmt (ws_entry_bb);

      if (region->inner->type == GIMPLE_OMP_FOR)
	{
	  /* If this is a combined parallel loop, we need to determine
	     whether or not to use the combined library calls.  There
	     are two cases where we do not apply the transformation:
	     static loops and any kind of ordered loop.  In the first
	     case, we already open code the loop so there is no need
	     to do anything else.  In the latter case, the combined
	     parallel loop call would still need extra synchronization
	     to implement ordered semantics, so there would not be any
	     gain in using the combined call.  */
	  tree clauses = gimple_omp_for_clauses (ws_stmt);
	  tree c = find_omp_clause (clauses, OMP_CLAUSE_SCHEDULE);
	  if (c == NULL
	      || ((OMP_CLAUSE_SCHEDULE_KIND (c) & OMP_CLAUSE_SCHEDULE_MASK)
		  == OMP_CLAUSE_SCHEDULE_STATIC)
	      || find_omp_clause (clauses, OMP_CLAUSE_ORDERED))
	    {
	      region->is_combined_parallel = false;
	      region->inner->is_combined_parallel = false;
	      return;
	    }
	}

      region->is_combined_parallel = true;
      region->inner->is_combined_parallel = true;
      region->ws_args = get_ws_args_for (par_stmt, ws_stmt);
    }
}


/* Return true if EXPR is variable sized.  */

static inline bool
is_variable_sized (const_tree expr)
{
  return !TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (expr)));
}

/* Return true if DECL is a reference type.  */

static inline bool
is_reference (tree decl)
{
  return lang_hooks.decls.omp_privatize_by_reference (decl);
}

/* Return the type of a decl.  If the decl is reference type,
   return its base type.  */
static inline tree
get_base_type (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (is_reference (decl))
    type = TREE_TYPE (type);
  return type;
}

/* Lookup variables.  The "maybe" form
   allows for the variable form to not have been entered, otherwise we
   assert that the variable must have been entered.  */

static inline tree
lookup_decl (tree var, omp_context *ctx)
{
  tree *n = ctx->cb.decl_map->get (var);
  return *n;
}

static inline tree
maybe_lookup_decl (const_tree var, omp_context *ctx)
{
  tree *n = ctx->cb.decl_map->get (const_cast<tree> (var));
  return n ? *n : NULL_TREE;
}

static inline tree
lookup_field (tree var, omp_context *ctx)
{
  splay_tree_node n;
  n = splay_tree_lookup (ctx->field_map, (splay_tree_key) var);
  return (tree) n->value;
}

static inline tree
lookup_sfield (splay_tree_key key, omp_context *ctx)
{
  splay_tree_node n;
  n = splay_tree_lookup (ctx->sfield_map
			 ? ctx->sfield_map : ctx->field_map, key);
  return (tree) n->value;
}

static inline tree
lookup_sfield (tree var, omp_context *ctx)
{
  return lookup_sfield ((splay_tree_key) var, ctx);
}

static inline tree
maybe_lookup_field (splay_tree_key key, omp_context *ctx)
{
  splay_tree_node n;
  n = splay_tree_lookup (ctx->field_map, key);
  return n ? (tree) n->value : NULL_TREE;
}

static inline tree
maybe_lookup_field (tree var, omp_context *ctx)
{
  return maybe_lookup_field ((splay_tree_key) var, ctx);
}

/* Return true if DECL should be copied by pointer.  SHARED_CTX is
   the parallel context if DECL is to be shared.  */

static bool
use_pointer_for_field (tree decl, omp_context *shared_ctx)
{
  if (AGGREGATE_TYPE_P (TREE_TYPE (decl)))
    return true;

  /* We can only use copy-in/copy-out semantics for shared variables
     when we know the value is not accessible from an outer scope.  */
  if (shared_ctx)
    {
      gcc_assert (!is_gimple_omp_oacc (shared_ctx->stmt));

      /* ??? Trivially accessible from anywhere.  But why would we even
	 be passing an address in this case?  Should we simply assert
	 this to be false, or should we have a cleanup pass that removes
	 these from the list of mappings?  */
      if (TREE_STATIC (decl) || DECL_EXTERNAL (decl))
	return true;

      /* For variables with DECL_HAS_VALUE_EXPR_P set, we cannot tell
	 without analyzing the expression whether or not its location
	 is accessible to anyone else.  In the case of nested parallel
	 regions it certainly may be.  */
      if (TREE_CODE (decl) != RESULT_DECL && DECL_HAS_VALUE_EXPR_P (decl))
	return true;

      /* Do not use copy-in/copy-out for variables that have their
	 address taken.  */
      if (TREE_ADDRESSABLE (decl))
	return true;

      /* lower_send_shared_vars only uses copy-in, but not copy-out
	 for these.  */
      if (TREE_READONLY (decl)
	  || ((TREE_CODE (decl) == RESULT_DECL
	       || TREE_CODE (decl) == PARM_DECL)
	      && DECL_BY_REFERENCE (decl)))
	return false;

      /* Disallow copy-in/out in nested parallel if
	 decl is shared in outer parallel, otherwise
	 each thread could store the shared variable
	 in its own copy-in location, making the
	 variable no longer really shared.  */
      if (shared_ctx->is_nested)
	{
	  omp_context *up;

	  for (up = shared_ctx->outer; up; up = up->outer)
	    if (is_taskreg_ctx (up) && maybe_lookup_decl (decl, up))
	      break;

	  if (up)
	    {
	      tree c;

	      for (c = gimple_omp_taskreg_clauses (up->stmt);
		   c; c = OMP_CLAUSE_CHAIN (c))
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
		    && OMP_CLAUSE_DECL (c) == decl)
		  break;

	      if (c)
		goto maybe_mark_addressable_and_ret;
	    }
	}

      /* For tasks avoid using copy-in/out.  As tasks can be
	 deferred or executed in different thread, when GOMP_task
	 returns, the task hasn't necessarily terminated.  */
      if (is_task_ctx (shared_ctx))
	{
	  tree outer;
	maybe_mark_addressable_and_ret:
	  outer = maybe_lookup_decl_in_outer_ctx (decl, shared_ctx);
	  if (is_gimple_reg (outer) && !omp_member_access_dummy_var (outer))
	    {
	      /* Taking address of OUTER in lower_send_shared_vars
		 might need regimplification of everything that uses the
		 variable.  */
	      if (!task_shared_vars)
		task_shared_vars = BITMAP_ALLOC (NULL);
	      bitmap_set_bit (task_shared_vars, DECL_UID (outer));
	      TREE_ADDRESSABLE (outer) = 1;
	    }
	  return true;
	}
    }

  return false;
}

/* Construct a new automatic decl similar to VAR.  */

static tree
omp_copy_decl_2 (tree var, tree name, tree type, omp_context *ctx)
{
  tree copy = copy_var_decl (var, name, type);

  DECL_CONTEXT (copy) = current_function_decl;
  DECL_CHAIN (copy) = ctx->block_vars;
  /* If VAR is listed in task_shared_vars, it means it wasn't
     originally addressable and is just because task needs to take
     it's address.  But we don't need to take address of privatizations
     from that var.  */
  if (TREE_ADDRESSABLE (var)
      && task_shared_vars
      && bitmap_bit_p (task_shared_vars, DECL_UID (var)))
    TREE_ADDRESSABLE (copy) = 0;
  ctx->block_vars = copy;

  return copy;
}

static tree
omp_copy_decl_1 (tree var, omp_context *ctx)
{
  return omp_copy_decl_2 (var, DECL_NAME (var), TREE_TYPE (var), ctx);
}

/* Build COMPONENT_REF and set TREE_THIS_VOLATILE and TREE_READONLY on it
   as appropriate.  */
static tree
omp_build_component_ref (tree obj, tree field)
{
  tree ret = build3 (COMPONENT_REF, TREE_TYPE (field), obj, field, NULL);
  if (TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ret) |= 1;
  if (TREE_READONLY (field))
    TREE_READONLY (ret) |= 1;
  return ret;
}

/* Build tree nodes to access the field for VAR on the receiver side.  */

static tree
build_receiver_ref (tree var, bool by_ref, omp_context *ctx)
{
  tree x, field = lookup_field (var, ctx);

  /* If the receiver record type was remapped in the child function,
     remap the field into the new record type.  */
  x = maybe_lookup_field (field, ctx);
  if (x != NULL)
    field = x;

  x = build_simple_mem_ref (ctx->receiver_decl);
  TREE_THIS_NOTRAP (x) = 1;
  x = omp_build_component_ref (x, field);
  if (by_ref)
    {
      x = build_simple_mem_ref (x);
      TREE_THIS_NOTRAP (x) = 1;
    }

  return x;
}

/* Build tree nodes to access VAR in the scope outer to CTX.  In the case
   of a parallel, this is a component reference; for workshare constructs
   this is some variable.  */

static tree
build_outer_var_ref (tree var, omp_context *ctx, bool lastprivate = false)
{
  tree x;

  if (is_global_var (maybe_lookup_decl_in_outer_ctx (var, ctx)))
    x = var;
  else if (is_variable_sized (var))
    {
      x = TREE_OPERAND (DECL_VALUE_EXPR (var), 0);
      x = build_outer_var_ref (x, ctx, lastprivate);
      x = build_simple_mem_ref (x);
    }
  else if (is_taskreg_ctx (ctx))
    {
      bool by_ref = use_pointer_for_field (var, NULL);
      x = build_receiver_ref (var, by_ref, ctx);
    }
  else if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	   && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
    {
      /* #pragma omp simd isn't a worksharing construct, and can reference even
	 private vars in its linear etc. clauses.  */
      x = NULL_TREE;
      if (ctx->outer && is_taskreg_ctx (ctx))
	x = lookup_decl (var, ctx->outer);
      else if (ctx->outer)
	x = maybe_lookup_decl_in_outer_ctx (var, ctx);
      if (x == NULL_TREE)
	x = var;
    }
  else if (lastprivate && is_taskloop_ctx (ctx))
    {
      gcc_assert (ctx->outer);
      splay_tree_node n
	= splay_tree_lookup (ctx->outer->field_map,
			     (splay_tree_key) &DECL_UID (var));
      if (n == NULL)
	{
	  if (is_global_var (maybe_lookup_decl_in_outer_ctx (var, ctx->outer)))
	    x = var;
	  else
	    x = lookup_decl (var, ctx->outer);
	}
      else
	{
	  tree field = (tree) n->value;
	  /* If the receiver record type was remapped in the child function,
	     remap the field into the new record type.  */
	  x = maybe_lookup_field (field, ctx->outer);
	  if (x != NULL)
	    field = x;

	  x = build_simple_mem_ref (ctx->outer->receiver_decl);
	  x = omp_build_component_ref (x, field);
	  if (use_pointer_for_field (var, ctx->outer))
	    x = build_simple_mem_ref (x);
	}
    }
  else if (ctx->outer)
    {
      omp_context *outer = ctx->outer;
      if (gimple_code (outer->stmt) == GIMPLE_OMP_GRID_BODY)
	{
	  outer = outer->outer;
	  gcc_assert (outer
		      && gimple_code (outer->stmt) != GIMPLE_OMP_GRID_BODY);
	}
	x = lookup_decl (var, outer);
    }
  else if (is_reference (var))
    /* This can happen with orphaned constructs.  If var is reference, it is
       possible it is shared and as such valid.  */
    x = var;
  else if (omp_member_access_dummy_var (var))
    x = var;
  else
    gcc_unreachable ();

  if (x == var)
    {
      tree t = omp_member_access_dummy_var (var);
      if (t)
	{
	  x = DECL_VALUE_EXPR (var);
	  tree o = maybe_lookup_decl_in_outer_ctx (t, ctx);
	  if (o != t)
	    x = unshare_and_remap (x, t, o);
	  else
	    x = unshare_expr (x);
	}
    }

  if (is_reference (var))
    x = build_simple_mem_ref (x);

  return x;
}

/* Build tree nodes to access the field for VAR on the sender side.  */

static tree
build_sender_ref (splay_tree_key key, omp_context *ctx)
{
  tree field = lookup_sfield (key, ctx);
  return omp_build_component_ref (ctx->sender_decl, field);
}

static tree
build_sender_ref (tree var, omp_context *ctx)
{
  return build_sender_ref ((splay_tree_key) var, ctx);
}

/* Add a new field for VAR inside the structure CTX->SENDER_DECL.  If
   BASE_POINTERS_RESTRICT, declare the field with restrict.  */

static void
install_var_field (tree var, bool by_ref, int mask, omp_context *ctx,
		   bool base_pointers_restrict = false)
{
  tree field, type, sfield = NULL_TREE;
  splay_tree_key key = (splay_tree_key) var;

  if ((mask & 8) != 0)
    {
      key = (splay_tree_key) &DECL_UID (var);
      gcc_checking_assert (key != (splay_tree_key) var);
    }
  gcc_assert ((mask & 1) == 0
	      || !splay_tree_lookup (ctx->field_map, key));
  gcc_assert ((mask & 2) == 0 || !ctx->sfield_map
	      || !splay_tree_lookup (ctx->sfield_map, key));
  gcc_assert ((mask & 3) == 3
	      || !is_gimple_omp_oacc (ctx->stmt));

  type = TREE_TYPE (var);
  /* Prevent redeclaring the var in the split-off function with a restrict
     pointer type.  Note that we only clear type itself, restrict qualifiers in
     the pointed-to type will be ignored by points-to analysis.  */
  if (POINTER_TYPE_P (type)
      && TYPE_RESTRICT (type))
    type = build_qualified_type (type, TYPE_QUALS (type) & ~TYPE_QUAL_RESTRICT);

  if (mask & 4)
    {
      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
      type = build_pointer_type (build_pointer_type (type));
    }
  else if (by_ref)
    {
      type = build_pointer_type (type);
      if (base_pointers_restrict)
	type = build_qualified_type (type, TYPE_QUAL_RESTRICT);
    }
  else if ((mask & 3) == 1 && is_reference (var))
    type = TREE_TYPE (type);

  field = build_decl (DECL_SOURCE_LOCATION (var),
		      FIELD_DECL, DECL_NAME (var), type);

  /* Remember what variable this field was created for.  This does have a
     side effect of making dwarf2out ignore this member, so for helpful
     debugging we clear it later in delete_omp_context.  */
  DECL_ABSTRACT_ORIGIN (field) = var;
  if (type == TREE_TYPE (var))
    {
      SET_DECL_ALIGN (field, DECL_ALIGN (var));
      DECL_USER_ALIGN (field) = DECL_USER_ALIGN (var);
      TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (var);
    }
  else
    SET_DECL_ALIGN (field, TYPE_ALIGN (type));

  if ((mask & 3) == 3)
    {
      insert_field_into_struct (ctx->record_type, field);
      if (ctx->srecord_type)
	{
	  sfield = build_decl (DECL_SOURCE_LOCATION (var),
			       FIELD_DECL, DECL_NAME (var), type);
	  DECL_ABSTRACT_ORIGIN (sfield) = var;
	  SET_DECL_ALIGN (sfield, DECL_ALIGN (field));
	  DECL_USER_ALIGN (sfield) = DECL_USER_ALIGN (field);
	  TREE_THIS_VOLATILE (sfield) = TREE_THIS_VOLATILE (field);
	  insert_field_into_struct (ctx->srecord_type, sfield);
	}
    }
  else
    {
      if (ctx->srecord_type == NULL_TREE)
	{
	  tree t;

	  ctx->srecord_type = lang_hooks.types.make_type (RECORD_TYPE);
	  ctx->sfield_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
	  for (t = TYPE_FIELDS (ctx->record_type); t ; t = TREE_CHAIN (t))
	    {
	      sfield = build_decl (DECL_SOURCE_LOCATION (t),
				   FIELD_DECL, DECL_NAME (t), TREE_TYPE (t));
	      DECL_ABSTRACT_ORIGIN (sfield) = DECL_ABSTRACT_ORIGIN (t);
	      insert_field_into_struct (ctx->srecord_type, sfield);
	      splay_tree_insert (ctx->sfield_map,
				 (splay_tree_key) DECL_ABSTRACT_ORIGIN (t),
				 (splay_tree_value) sfield);
	    }
	}
      sfield = field;
      insert_field_into_struct ((mask & 1) ? ctx->record_type
				: ctx->srecord_type, field);
    }

  if (mask & 1)
    splay_tree_insert (ctx->field_map, key, (splay_tree_value) field);
  if ((mask & 2) && ctx->sfield_map)
    splay_tree_insert (ctx->sfield_map, key, (splay_tree_value) sfield);
}

static tree
install_var_local (tree var, omp_context *ctx)
{
  tree new_var = omp_copy_decl_1 (var, ctx);
  insert_decl_map (&ctx->cb, var, new_var);
  return new_var;
}

/* Adjust the replacement for DECL in CTX for the new context.  This means
   copying the DECL_VALUE_EXPR, and fixing up the type.  */

static void
fixup_remapped_decl (tree decl, omp_context *ctx, bool private_debug)
{
  tree new_decl, size;

  new_decl = lookup_decl (decl, ctx);

  TREE_TYPE (new_decl) = remap_type (TREE_TYPE (decl), &ctx->cb);

  if ((!TREE_CONSTANT (DECL_SIZE (new_decl)) || private_debug)
      && DECL_HAS_VALUE_EXPR_P (decl))
    {
      tree ve = DECL_VALUE_EXPR (decl);
      walk_tree (&ve, copy_tree_body_r, &ctx->cb, NULL);
      SET_DECL_VALUE_EXPR (new_decl, ve);
      DECL_HAS_VALUE_EXPR_P (new_decl) = 1;
    }

  if (!TREE_CONSTANT (DECL_SIZE (new_decl)))
    {
      size = remap_decl (DECL_SIZE (decl), &ctx->cb);
      if (size == error_mark_node)
	size = TYPE_SIZE (TREE_TYPE (new_decl));
      DECL_SIZE (new_decl) = size;

      size = remap_decl (DECL_SIZE_UNIT (decl), &ctx->cb);
      if (size == error_mark_node)
	size = TYPE_SIZE_UNIT (TREE_TYPE (new_decl));
      DECL_SIZE_UNIT (new_decl) = size;
    }
}

/* The callback for remap_decl.  Search all containing contexts for a
   mapping of the variable; this avoids having to duplicate the splay
   tree ahead of time.  We know a mapping doesn't already exist in the
   given context.  Create new mappings to implement default semantics.  */

static tree
omp_copy_decl (tree var, copy_body_data *cb)
{
  omp_context *ctx = (omp_context *) cb;
  tree new_var;

  if (TREE_CODE (var) == LABEL_DECL)
    {
      new_var = create_artificial_label (DECL_SOURCE_LOCATION (var));
      DECL_CONTEXT (new_var) = current_function_decl;
      insert_decl_map (&ctx->cb, var, new_var);
      return new_var;
    }

  while (!is_taskreg_ctx (ctx))
    {
      ctx = ctx->outer;
      if (ctx == NULL)
	return var;
      new_var = maybe_lookup_decl (var, ctx);
      if (new_var)
	return new_var;
    }

  if (is_global_var (var) || decl_function_context (var) != ctx->cb.src_fn)
    return var;

  return error_mark_node;
}


/* Debugging dumps for parallel regions.  */
void dump_omp_region (FILE *, struct omp_region *, int);
void debug_omp_region (struct omp_region *);
void debug_all_omp_regions (void);

/* Dump the parallel region tree rooted at REGION.  */

void
dump_omp_region (FILE *file, struct omp_region *region, int indent)
{
  fprintf (file, "%*sbb %d: %s\n", indent, "", region->entry->index,
	   gimple_code_name[region->type]);

  if (region->inner)
    dump_omp_region (file, region->inner, indent + 4);

  if (region->cont)
    {
      fprintf (file, "%*sbb %d: GIMPLE_OMP_CONTINUE\n", indent, "",
	       region->cont->index);
    }

  if (region->exit)
    fprintf (file, "%*sbb %d: GIMPLE_OMP_RETURN\n", indent, "",
	     region->exit->index);
  else
    fprintf (file, "%*s[no exit marker]\n", indent, "");

  if (region->next)
    dump_omp_region (file, region->next, indent);
}

DEBUG_FUNCTION void
debug_omp_region (struct omp_region *region)
{
  dump_omp_region (stderr, region, 0);
}

DEBUG_FUNCTION void
debug_all_omp_regions (void)
{
  dump_omp_region (stderr, root_omp_region, 0);
}


/* Create a new parallel region starting at STMT inside region PARENT.  */

static struct omp_region *
new_omp_region (basic_block bb, enum gimple_code type,
		struct omp_region *parent)
{
  struct omp_region *region = XCNEW (struct omp_region);

  region->outer = parent;
  region->entry = bb;
  region->type = type;

  if (parent)
    {
      /* This is a nested region.  Add it to the list of inner
	 regions in PARENT.  */
      region->next = parent->inner;
      parent->inner = region;
    }
  else
    {
      /* This is a toplevel region.  Add it to the list of toplevel
	 regions in ROOT_OMP_REGION.  */
      region->next = root_omp_region;
      root_omp_region = region;
    }

  return region;
}

/* Release the memory associated with the region tree rooted at REGION.  */

static void
free_omp_region_1 (struct omp_region *region)
{
  struct omp_region *i, *n;

  for (i = region->inner; i ; i = n)
    {
      n = i->next;
      free_omp_region_1 (i);
    }

  free (region);
}

/* Release the memory for the entire omp region tree.  */

void
free_omp_regions (void)
{
  struct omp_region *r, *n;
  for (r = root_omp_region; r ; r = n)
    {
      n = r->next;
      free_omp_region_1 (r);
    }
  root_omp_region = NULL;
}


/* Create a new context, with OUTER_CTX being the surrounding context.  */

static omp_context *
new_omp_context (gimple *stmt, omp_context *outer_ctx)
{
  omp_context *ctx = XCNEW (omp_context);

  splay_tree_insert (all_contexts, (splay_tree_key) stmt,
		     (splay_tree_value) ctx);
  ctx->stmt = stmt;

  if (outer_ctx)
    {
      ctx->outer = outer_ctx;
      ctx->cb = outer_ctx->cb;
      ctx->cb.block = NULL;
      ctx->depth = outer_ctx->depth + 1;
    }
  else
    {
      ctx->cb.src_fn = current_function_decl;
      ctx->cb.dst_fn = current_function_decl;
      ctx->cb.src_node = cgraph_node::get (current_function_decl);
      gcc_checking_assert (ctx->cb.src_node);
      ctx->cb.dst_node = ctx->cb.src_node;
      ctx->cb.src_cfun = cfun;
      ctx->cb.copy_decl = omp_copy_decl;
      ctx->cb.eh_lp_nr = 0;
      ctx->cb.transform_call_graph_edges = CB_CGE_MOVE;
      ctx->depth = 1;
    }

  ctx->cb.decl_map = new hash_map<tree, tree>;

  return ctx;
}

static gimple_seq maybe_catch_exception (gimple_seq);

/* Finalize task copyfn.  */

static void
finalize_task_copyfn (gomp_task *task_stmt)
{
  struct function *child_cfun;
  tree child_fn;
  gimple_seq seq = NULL, new_seq;
  gbind *bind;

  child_fn = gimple_omp_task_copy_fn (task_stmt);
  if (child_fn == NULL_TREE)
    return;

  child_cfun = DECL_STRUCT_FUNCTION (child_fn);
  DECL_STRUCT_FUNCTION (child_fn)->curr_properties = cfun->curr_properties;

  push_cfun (child_cfun);
  bind = gimplify_body (child_fn, false);
  gimple_seq_add_stmt (&seq, bind);
  new_seq = maybe_catch_exception (seq);
  if (new_seq != seq)
    {
      bind = gimple_build_bind (NULL, new_seq, NULL);
      seq = NULL;
      gimple_seq_add_stmt (&seq, bind);
    }
  gimple_set_body (child_fn, seq);
  pop_cfun ();

  /* Inform the callgraph about the new function.  */
  cgraph_node *node = cgraph_node::get_create (child_fn);
  node->parallelized_function = 1;
  cgraph_node::add_new_function (child_fn, false);
}

/* Destroy a omp_context data structures.  Called through the splay tree
   value delete callback.  */

static void
delete_omp_context (splay_tree_value value)
{
  omp_context *ctx = (omp_context *) value;

  delete ctx->cb.decl_map;

  if (ctx->field_map)
    splay_tree_delete (ctx->field_map);
  if (ctx->sfield_map)
    splay_tree_delete (ctx->sfield_map);

  /* We hijacked DECL_ABSTRACT_ORIGIN earlier.  We need to clear it before
     it produces corrupt debug information.  */
  if (ctx->record_type)
    {
      tree t;
      for (t = TYPE_FIELDS (ctx->record_type); t ; t = DECL_CHAIN (t))
	DECL_ABSTRACT_ORIGIN (t) = NULL;
    }
  if (ctx->srecord_type)
    {
      tree t;
      for (t = TYPE_FIELDS (ctx->srecord_type); t ; t = DECL_CHAIN (t))
	DECL_ABSTRACT_ORIGIN (t) = NULL;
    }

  if (is_task_ctx (ctx))
    finalize_task_copyfn (as_a <gomp_task *> (ctx->stmt));

  XDELETE (ctx);
}

/* Fix up RECEIVER_DECL with a type that has been remapped to the child
   context.  */

static void
fixup_child_record_type (omp_context *ctx)
{
  tree f, type = ctx->record_type;

  if (!ctx->receiver_decl)
    return;
  /* ??? It isn't sufficient to just call remap_type here, because
     variably_modified_type_p doesn't work the way we expect for
     record types.  Testing each field for whether it needs remapping
     and creating a new record by hand works, however.  */
  for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
    if (variably_modified_type_p (TREE_TYPE (f), ctx->cb.src_fn))
      break;
  if (f)
    {
      tree name, new_fields = NULL;

      type = lang_hooks.types.make_type (RECORD_TYPE);
      name = DECL_NAME (TYPE_NAME (ctx->record_type));
      name = build_decl (DECL_SOURCE_LOCATION (ctx->receiver_decl),
			 TYPE_DECL, name, type);
      TYPE_NAME (type) = name;

      for (f = TYPE_FIELDS (ctx->record_type); f ; f = DECL_CHAIN (f))
	{
	  tree new_f = copy_node (f);
	  DECL_CONTEXT (new_f) = type;
	  TREE_TYPE (new_f) = remap_type (TREE_TYPE (f), &ctx->cb);
	  DECL_CHAIN (new_f) = new_fields;
	  walk_tree (&DECL_SIZE (new_f), copy_tree_body_r, &ctx->cb, NULL);
	  walk_tree (&DECL_SIZE_UNIT (new_f), copy_tree_body_r,
		     &ctx->cb, NULL);
	  walk_tree (&DECL_FIELD_OFFSET (new_f), copy_tree_body_r,
		     &ctx->cb, NULL);
	  new_fields = new_f;

	  /* Arrange to be able to look up the receiver field
	     given the sender field.  */
	  splay_tree_insert (ctx->field_map, (splay_tree_key) f,
			     (splay_tree_value) new_f);
	}
      TYPE_FIELDS (type) = nreverse (new_fields);
      layout_type (type);
    }

  /* In a target region we never modify any of the pointers in *.omp_data_i,
     so attempt to help the optimizers.  */
  if (is_gimple_omp_offloaded (ctx->stmt))
    type = build_qualified_type (type, TYPE_QUAL_CONST);

  TREE_TYPE (ctx->receiver_decl)
    = build_qualified_type (build_reference_type (type), TYPE_QUAL_RESTRICT);
}

/* Instantiate decls as necessary in CTX to satisfy the data sharing
   specified by CLAUSES.  If BASE_POINTERS_RESTRICT, install var field with
   restrict.  */

static void
scan_sharing_clauses (tree clauses, omp_context *ctx,
		      bool base_pointers_restrict = false)
{
  tree c, decl;
  bool scan_array_reductions = false;

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      bool by_ref;

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_PRIVATE:
	  decl = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_PRIVATE_OUTER_REF (c))
	    goto do_private;
	  else if (!is_variable_sized (decl))
	    install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_SHARED:
	  decl = OMP_CLAUSE_DECL (c);
	  /* Ignore shared directives in teams construct.  */
	  if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
	    {
	      /* Global variables don't need to be copied,
		 the receiver side will use them directly.  */
	      tree odecl = maybe_lookup_decl_in_outer_ctx (decl, ctx);
	      if (is_global_var (odecl))
		break;
	      insert_decl_map (&ctx->cb, decl, odecl);
	      break;
	    }
	  gcc_assert (is_taskreg_ctx (ctx));
	  gcc_assert (!COMPLETE_TYPE_P (TREE_TYPE (decl))
		      || !is_variable_sized (decl));
	  /* Global variables don't need to be copied,
	     the receiver side will use them directly.  */
	  if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx)))
	    break;
	  if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	    {
	      use_pointer_for_field (decl, ctx);
	      break;
	    }
	  by_ref = use_pointer_for_field (decl, NULL);
	  if ((! TREE_READONLY (decl) && !OMP_CLAUSE_SHARED_READONLY (c))
	      || TREE_ADDRESSABLE (decl)
	      || by_ref
	      || is_reference (decl))
	    {
	      by_ref = use_pointer_for_field (decl, ctx);
	      install_var_field (decl, by_ref, 3, ctx);
	      install_var_local (decl, ctx);
	      break;
	    }
	  /* We don't need to copy const scalar vars back.  */
	  OMP_CLAUSE_SET_CODE (c, OMP_CLAUSE_FIRSTPRIVATE);
	  goto do_private;

	case OMP_CLAUSE_REDUCTION:
	  decl = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	      && TREE_CODE (decl) == MEM_REF)
	    {
	      tree t = TREE_OPERAND (decl, 0);
	      if (TREE_CODE (t) == POINTER_PLUS_EXPR)
		t = TREE_OPERAND (t, 0);
	      if (TREE_CODE (t) == INDIRECT_REF
		  || TREE_CODE (t) == ADDR_EXPR)
		t = TREE_OPERAND (t, 0);
	      install_var_local (t, ctx);
	      if (is_taskreg_ctx (ctx)
		  && !is_global_var (maybe_lookup_decl_in_outer_ctx (t, ctx))
		  && !is_variable_sized (t))
		{
		  by_ref = use_pointer_for_field (t, ctx);
		  install_var_field (t, by_ref, 3, ctx);
		}
	      break;
	    }
	  goto do_private;

	case OMP_CLAUSE_LASTPRIVATE:
	  /* Let the corresponding firstprivate clause create
	     the variable.  */
	  if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
	    break;
	  /* FALLTHRU */

	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LINEAR:
	  decl = OMP_CLAUSE_DECL (c);
	do_private:
	  if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IS_DEVICE_PTR)
	      && is_gimple_omp_offloaded (ctx->stmt))
	    {
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
		install_var_field (decl, !is_reference (decl), 3, ctx);
	      else if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
		install_var_field (decl, true, 3, ctx);
	      else
		install_var_field (decl, false, 3, ctx);
	    }
	  if (is_variable_sized (decl))
	    {
	      if (is_task_ctx (ctx))
		install_var_field (decl, false, 1, ctx);
	      break;
	    }
	  else if (is_taskreg_ctx (ctx))
	    {
	      bool global
		= is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx));
	      by_ref = use_pointer_for_field (decl, NULL);

	      if (is_task_ctx (ctx)
		  && (global || by_ref || is_reference (decl)))
		{
		  install_var_field (decl, false, 1, ctx);
		  if (!global)
		    install_var_field (decl, by_ref, 2, ctx);
		}
	      else if (!global)
		install_var_field (decl, by_ref, 3, ctx);
	    }
	  install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_USE_DEVICE_PTR:
	  decl = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    install_var_field (decl, true, 3, ctx);
	  else
	    install_var_field (decl, false, 3, ctx);
	  if (DECL_SIZE (decl)
	      && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
	    {
	      tree decl2 = DECL_VALUE_EXPR (decl);
	      gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
	      decl2 = TREE_OPERAND (decl2, 0);
	      gcc_assert (DECL_P (decl2));
	      install_var_local (decl2, ctx);
	    }
	  install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_IS_DEVICE_PTR:
	  decl = OMP_CLAUSE_DECL (c);
	  goto do_private;

	case OMP_CLAUSE__LOOPTEMP_:
	  gcc_assert (is_taskreg_ctx (ctx));
	  decl = OMP_CLAUSE_DECL (c);
	  install_var_field (decl, false, 3, ctx);
	  install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_COPYIN:
	  decl = OMP_CLAUSE_DECL (c);
	  by_ref = use_pointer_for_field (decl, NULL);
	  install_var_field (decl, by_ref, 3, ctx);
	  break;

	case OMP_CLAUSE_DEFAULT:
	  ctx->default_kind = OMP_CLAUSE_DEFAULT_KIND (c);
	  break;

	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE__CILK_FOR_COUNT_:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	  if (ctx->outer)
	    scan_omp_op (&OMP_CLAUSE_OPERAND (c, 0), ctx->outer);
	  break;

	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_MAP:
	  if (ctx->outer)
	    scan_omp_op (&OMP_CLAUSE_SIZE (c), ctx->outer);
	  decl = OMP_CLAUSE_DECL (c);
	  /* Global variables with "omp declare target" attribute
	     don't need to be copied, the receiver side will use them
	     directly.  However, global variables with "omp declare target link"
	     attribute need to be copied.  */
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && DECL_P (decl)
	      && ((OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FIRSTPRIVATE_POINTER
		   && (OMP_CLAUSE_MAP_KIND (c)
		       != GOMP_MAP_FIRSTPRIVATE_REFERENCE))
		  || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	      && is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx))
	      && varpool_node::get_create (decl)->offloadable
	      && !lookup_attribute ("omp declare target link",
				    DECL_ATTRIBUTES (decl)))
	    break;
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER)
	    {
	      /* Ignore GOMP_MAP_POINTER kind for arrays in regions that are
		 not offloaded; there is nothing to map for those.  */
	      if (!is_gimple_omp_offloaded (ctx->stmt)
		  && !POINTER_TYPE_P (TREE_TYPE (decl))
		  && !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c))
		break;
	    }
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		  || (OMP_CLAUSE_MAP_KIND (c)
		      == GOMP_MAP_FIRSTPRIVATE_REFERENCE)))
	    {
	      if (TREE_CODE (decl) == COMPONENT_REF
		  || (TREE_CODE (decl) == INDIRECT_REF
		      && TREE_CODE (TREE_OPERAND (decl, 0)) == COMPONENT_REF
		      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (decl, 0)))
			  == REFERENCE_TYPE)))
		break;
	      if (DECL_SIZE (decl)
		  && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  install_var_local (decl2, ctx);
		}
	      install_var_local (decl, ctx);
	      break;
	    }
	  if (DECL_P (decl))
	    {
	      if (DECL_SIZE (decl)
		  && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  install_var_field (decl2, true, 3, ctx);
		  install_var_local (decl2, ctx);
		  install_var_local (decl, ctx);
		}
	      else
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		      && !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
		    install_var_field (decl, true, 7, ctx);
		  else
		    install_var_field (decl, true, 3, ctx,
				       base_pointers_restrict);
		  if (is_gimple_omp_offloaded (ctx->stmt)
		      && !OMP_CLAUSE_MAP_IN_REDUCTION (c))
		    install_var_local (decl, ctx);
		}
	    }
	  else
	    {
	      tree base = get_base_address (decl);
	      tree nc = OMP_CLAUSE_CHAIN (c);
	      if (DECL_P (base)
		  && nc != NULL_TREE
		  && OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
		  && OMP_CLAUSE_DECL (nc) == base
		  && OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_POINTER
		  && integer_zerop (OMP_CLAUSE_SIZE (nc)))
		{
		  OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c) = 1;
		  OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (nc) = 1;
		}
	      else
		{
		  if (ctx->outer)
		    {
		      scan_omp_op (&OMP_CLAUSE_DECL (c), ctx->outer);
		      decl = OMP_CLAUSE_DECL (c);
		    }
		  gcc_assert (!splay_tree_lookup (ctx->field_map,
						  (splay_tree_key) decl));
		  tree field
		    = build_decl (OMP_CLAUSE_LOCATION (c),
				  FIELD_DECL, NULL_TREE, ptr_type_node);
		  SET_DECL_ALIGN (field, TYPE_ALIGN (ptr_type_node));
		  insert_field_into_struct (ctx->record_type, field);
		  splay_tree_insert (ctx->field_map, (splay_tree_key) decl,
				     (splay_tree_value) field);
		}
	    }
	  break;

	case OMP_CLAUSE__GRIDDIM_:
	  if (ctx->outer)
	    {
	      scan_omp_op (&OMP_CLAUSE__GRIDDIM__SIZE (c), ctx->outer);
	      scan_omp_op (&OMP_CLAUSE__GRIDDIM__GROUP (c), ctx->outer);
	    }
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	  break;

	case OMP_CLAUSE_ALIGNED:
	  decl = OMP_CLAUSE_DECL (c);
	  if (is_global_var (decl)
	      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE__CACHE_:
	default:
	  gcc_unreachable ();
	}
    }

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_LASTPRIVATE:
	  /* Let the corresponding firstprivate clause create
	     the variable.  */
	  if (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c))
	    scan_array_reductions = true;
	  if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
	    break;
	  /* FALLTHRU */

	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_LINEAR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	  decl = OMP_CLAUSE_DECL (c);
	  if (is_variable_sized (decl))
	    {
	      if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IS_DEVICE_PTR)
		  && is_gimple_omp_offloaded (ctx->stmt))
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  install_var_local (decl2, ctx);
		  fixup_remapped_decl (decl2, ctx, false);
		}
	      install_var_local (decl, ctx);
	    }
	  fixup_remapped_decl (decl, ctx,
			       OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
			       && OMP_CLAUSE_PRIVATE_DEBUG (c));
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	      && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c))
	    scan_array_reductions = true;
	  break;

	case OMP_CLAUSE_REDUCTION:
	  decl = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (decl) != MEM_REF)
	    {
	      if (is_variable_sized (decl))
		install_var_local (decl, ctx);
	      fixup_remapped_decl (decl, ctx, false);
	    }
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    scan_array_reductions = true;
	  break;

	case OMP_CLAUSE_SHARED:
	  /* Ignore shared directives in teams construct.  */
	  if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
	    break;
	  decl = OMP_CLAUSE_DECL (c);
	  if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx)))
	    break;
	  if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	    {
	      if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl,
								 ctx->outer)))
		break;
	      bool by_ref = use_pointer_for_field (decl, ctx);
	      install_var_field (decl, by_ref, 11, ctx);
	      break;
	    }
	  fixup_remapped_decl (decl, ctx, false);
	  break;

	case OMP_CLAUSE_MAP:
	  if (!is_gimple_omp_offloaded (ctx->stmt))
	    break;
	  decl = OMP_CLAUSE_DECL (c);
	  if (DECL_P (decl)
	      && ((OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FIRSTPRIVATE_POINTER
		   && (OMP_CLAUSE_MAP_KIND (c)
		       != GOMP_MAP_FIRSTPRIVATE_REFERENCE))
		  || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	      && is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx))
	      && varpool_node::get_create (decl)->offloadable)
	    break;
	  if (DECL_P (decl))
	    {
	      if ((OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		   || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER)
		  && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
		  && !COMPLETE_TYPE_P (TREE_TYPE (decl)))
		{
		  tree new_decl = lookup_decl (decl, ctx);
		  TREE_TYPE (new_decl)
		    = remap_type (TREE_TYPE (decl), &ctx->cb);
		}
	      else if (DECL_SIZE (decl)
		       && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  fixup_remapped_decl (decl2, ctx, false);
		  fixup_remapped_decl (decl, ctx, true);
		}
	      else
		fixup_remapped_decl (decl, ctx, false);
	    }
	  break;

	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_ALIGNED:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE__LOOPTEMP_:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE__CILK_FOR_COUNT_:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE__GRIDDIM_:
	  break;

	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE__CACHE_:
	default:
	  gcc_unreachable ();
	}
    }

  gcc_checking_assert (!scan_array_reductions
		       || !is_gimple_omp_oacc (ctx->stmt));
  if (scan_array_reductions)
    {
      for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	    && OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	  {
	    scan_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c), ctx);
	    scan_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c), ctx);
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		 && OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c))
	  scan_omp (&OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c), ctx);
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		 && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c))
	  scan_omp (&OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c), ctx);
    }
}

/* Create a new name for omp child function.  Returns an identifier.  If
   IS_CILK_FOR is true then the suffix for the child function is
   "_cilk_for_fn."  */

static tree
create_omp_child_function_name (bool task_copy, bool is_cilk_for)
{
  if (is_cilk_for)
    return clone_function_name (current_function_decl, "_cilk_for_fn");
  return clone_function_name (current_function_decl,
			      task_copy ? "_omp_cpyfn" : "_omp_fn");
}

/* Returns the type of the induction variable for the child function for
   _Cilk_for and the types for _high and _low variables based on TYPE.  */

static tree
cilk_for_check_loop_diff_type (tree type)
{
  if (TYPE_PRECISION (type) <= TYPE_PRECISION (uint32_type_node))
    {
      if (TYPE_UNSIGNED (type))
	return uint32_type_node;
      else
	return integer_type_node;
    }
  else
    {
      if (TYPE_UNSIGNED (type))
	return uint64_type_node;
      else
	return long_long_integer_type_node;
    }
}

/* Build a decl for the omp child function.  It'll not contain a body
   yet, just the bare decl.  */

static void
create_omp_child_function (omp_context *ctx, bool task_copy)
{
  tree decl, type, name, t;

  tree cilk_for_count
    = (flag_cilkplus && gimple_code (ctx->stmt) == GIMPLE_OMP_PARALLEL)
      ? find_omp_clause (gimple_omp_parallel_clauses (ctx->stmt),
			 OMP_CLAUSE__CILK_FOR_COUNT_) : NULL_TREE;
  tree cilk_var_type = NULL_TREE;

  name = create_omp_child_function_name (task_copy,
					 cilk_for_count != NULL_TREE);
  if (task_copy)
    type = build_function_type_list (void_type_node, ptr_type_node,
				     ptr_type_node, NULL_TREE);
  else if (cilk_for_count)
    {
      type = TREE_TYPE (OMP_CLAUSE_OPERAND (cilk_for_count, 0));
      cilk_var_type = cilk_for_check_loop_diff_type (type);
      type = build_function_type_list (void_type_node, ptr_type_node,
				       cilk_var_type, cilk_var_type, NULL_TREE);
    }
  else
    type = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);

  decl = build_decl (gimple_location (ctx->stmt), FUNCTION_DECL, name, type);

  gcc_checking_assert (!is_gimple_omp_oacc (ctx->stmt)
		       || !task_copy);
  if (!task_copy)
    ctx->cb.dst_fn = decl;
  else
    gimple_omp_task_set_copy_fn (ctx->stmt, decl);

  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  if (cgraph_node::get (current_function_decl)->offloadable)
    cgraph_node::get_create (decl)->offloadable = 1;
  else
    {
      omp_context *octx;
      for (octx = ctx; octx; octx = octx->outer)
	if (is_gimple_omp_offloaded (octx->stmt))
	  {
	    cgraph_node::get_create (decl)->offloadable = 1;
	    if (ENABLE_OFFLOADING)
	      g->have_offload = true;

	    break;
	  }
    }

  if (cgraph_node::get_create (decl)->offloadable
      && !lookup_attribute ("omp declare target",
                           DECL_ATTRIBUTES (current_function_decl)))
    DECL_ATTRIBUTES (decl)
      = tree_cons (get_identifier ("omp target entrypoint"),
                   NULL_TREE, DECL_ATTRIBUTES (decl));

  t = build_decl (DECL_SOURCE_LOCATION (decl),
		  RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_CONTEXT (t) = decl;
  DECL_RESULT (decl) = t;

  /* _Cilk_for's child function requires two extra parameters called
     __low and __high that are set the by Cilk runtime when it calls this
     function.  */
  if (cilk_for_count)
    {
      t = build_decl (DECL_SOURCE_LOCATION (decl),
		      PARM_DECL, get_identifier ("__high"), cilk_var_type);
      DECL_ARTIFICIAL (t) = 1;
      DECL_NAMELESS (t) = 1;
      DECL_ARG_TYPE (t) = ptr_type_node;
      DECL_CONTEXT (t) = current_function_decl;
      TREE_USED (t) = 1;
      DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
      DECL_ARGUMENTS (decl) = t;

      t = build_decl (DECL_SOURCE_LOCATION (decl),
		      PARM_DECL, get_identifier ("__low"), cilk_var_type);
      DECL_ARTIFICIAL (t) = 1;
      DECL_NAMELESS (t) = 1;
      DECL_ARG_TYPE (t) = ptr_type_node;
      DECL_CONTEXT (t) = current_function_decl;
      TREE_USED (t) = 1;
      DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
      DECL_ARGUMENTS (decl) = t;
    }

  tree data_name = get_identifier (".omp_data_i");
  t = build_decl (DECL_SOURCE_LOCATION (decl), PARM_DECL, data_name,
		  ptr_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_NAMELESS (t) = 1;
  DECL_ARG_TYPE (t) = ptr_type_node;
  DECL_CONTEXT (t) = current_function_decl;
  TREE_USED (t) = 1;
  TREE_READONLY (t) = 1;
  if (cilk_for_count)
    DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
  DECL_ARGUMENTS (decl) = t;
  if (!task_copy)
    ctx->receiver_decl = t;
  else
    {
      t = build_decl (DECL_SOURCE_LOCATION (decl),
		      PARM_DECL, get_identifier (".omp_data_o"),
		      ptr_type_node);
      DECL_ARTIFICIAL (t) = 1;
      DECL_NAMELESS (t) = 1;
      DECL_ARG_TYPE (t) = ptr_type_node;
      DECL_CONTEXT (t) = current_function_decl;
      TREE_USED (t) = 1;
      TREE_ADDRESSABLE (t) = 1;
      DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
      DECL_ARGUMENTS (decl) = t;
    }

  /* Allocate memory for the function structure.  The call to
     allocate_struct_function clobbers CFUN, so we need to restore
     it afterward.  */
  push_struct_function (decl);
  cfun->function_end_locus = gimple_location (ctx->stmt);
  init_tree_ssa (cfun);
  pop_cfun ();
}

/* Callback for walk_gimple_seq.  Check if combined parallel
   contains gimple_omp_for_combined_into_p OMP_FOR.  */

static tree
find_combined_for (gimple_stmt_iterator *gsi_p,
		   bool *handled_ops_p,
		   struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_FOR:
      if (gimple_omp_for_combined_into_p (stmt)
	  && gimple_omp_for_kind (stmt)
	     == *(const enum gf_mask *) (wi->info))
	{
	  wi->info = stmt;
	  return integer_zero_node;
	}
      break;
    default:
      break;
    }
  return NULL;
}

/* Add _LOOPTEMP_ clauses on OpenMP parallel or task.  */

static void
add_taskreg_looptemp_clauses (enum gf_mask msk, gimple *stmt,
			      omp_context *outer_ctx)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.val_only = true;
  wi.info = (void *) &msk;
  walk_gimple_seq (gimple_omp_body (stmt), find_combined_for, NULL, &wi);
  if (wi.info != (void *) &msk)
    {
      gomp_for *for_stmt = as_a <gomp_for *> ((gimple *) wi.info);
      struct omp_for_data fd;
      extract_omp_for_data (for_stmt, &fd, NULL);
      /* We need two temporaries with fd.loop.v type (istart/iend)
	 and then (fd.collapse - 1) temporaries with the same
	 type for count2 ... countN-1 vars if not constant.  */
      size_t count = 2, i;
      tree type = fd.iter_type;
      if (fd.collapse > 1
	  && TREE_CODE (fd.loop.n2) != INTEGER_CST)
	{
	  count += fd.collapse - 1;
	  /* If there are lastprivate clauses on the inner
	     GIMPLE_OMP_FOR, add one more temporaries for the total number
	     of iterations (product of count1 ... countN-1).  */
	  if (find_omp_clause (gimple_omp_for_clauses (for_stmt),
			       OMP_CLAUSE_LASTPRIVATE))
	    count++;
	  else if (msk == GF_OMP_FOR_KIND_FOR
		   && find_omp_clause (gimple_omp_parallel_clauses (stmt),
				       OMP_CLAUSE_LASTPRIVATE))
	    count++;
	}
      for (i = 0; i < count; i++)
	{
	  tree temp = create_tmp_var (type);
	  tree c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__LOOPTEMP_);
	  insert_decl_map (&outer_ctx->cb, temp, temp);
	  OMP_CLAUSE_DECL (c) = temp;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	  gimple_omp_taskreg_set_clauses (stmt, c);
	}
    }
}

/* Scan an OpenMP parallel directive.  */

static void
scan_omp_parallel (gimple_stmt_iterator *gsi, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name;
  gomp_parallel *stmt = as_a <gomp_parallel *> (gsi_stmt (*gsi));

  /* Ignore parallel directives with empty bodies, unless there
     are copyin clauses.  */
  if (optimize > 0
      && empty_body_p (gimple_omp_body (stmt))
      && find_omp_clause (gimple_omp_parallel_clauses (stmt),
			  OMP_CLAUSE_COPYIN) == NULL)
    {
      gsi_replace (gsi, gimple_build_nop (), false);
      return;
    }

  if (gimple_omp_parallel_combined_p (stmt))
    add_taskreg_looptemp_clauses (GF_OMP_FOR_KIND_FOR, stmt, outer_ctx);

  ctx = new_omp_context (stmt, outer_ctx);
  taskreg_contexts.safe_push (ctx);
  if (taskreg_nesting_level > 1)
    ctx->is_nested = true;
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->default_kind = OMP_CLAUSE_DEFAULT_SHARED;
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_data_s");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  DECL_ARTIFICIAL (name) = 1;
  DECL_NAMELESS (name) = 1;
  TYPE_NAME (ctx->record_type) = name;
  TYPE_ARTIFICIAL (ctx->record_type) = 1;
  if (!gimple_omp_parallel_grid_phony (stmt))
    {
      create_omp_child_function (ctx, false);
      gimple_omp_parallel_set_child_fn (stmt, ctx->cb.dst_fn);
    }

  scan_sharing_clauses (gimple_omp_parallel_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    ctx->record_type = ctx->receiver_decl = NULL;
}

/* Scan an OpenMP task directive.  */

static void
scan_omp_task (gimple_stmt_iterator *gsi, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name, t;
  gomp_task *stmt = as_a <gomp_task *> (gsi_stmt (*gsi));

  /* Ignore task directives with empty bodies.  */
  if (optimize > 0
      && empty_body_p (gimple_omp_body (stmt)))
    {
      gsi_replace (gsi, gimple_build_nop (), false);
      return;
    }

  if (gimple_omp_task_taskloop_p (stmt))
    add_taskreg_looptemp_clauses (GF_OMP_FOR_KIND_TASKLOOP, stmt, outer_ctx);

  ctx = new_omp_context (stmt, outer_ctx);
  taskreg_contexts.safe_push (ctx);
  if (taskreg_nesting_level > 1)
    ctx->is_nested = true;
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->default_kind = OMP_CLAUSE_DEFAULT_SHARED;
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_data_s");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  DECL_ARTIFICIAL (name) = 1;
  DECL_NAMELESS (name) = 1;
  TYPE_NAME (ctx->record_type) = name;
  TYPE_ARTIFICIAL (ctx->record_type) = 1;
  create_omp_child_function (ctx, false);
  gimple_omp_task_set_child_fn (stmt, ctx->cb.dst_fn);

  scan_sharing_clauses (gimple_omp_task_clauses (stmt), ctx);

  if (ctx->srecord_type)
    {
      name = create_tmp_var_name (".omp_data_a");
      name = build_decl (gimple_location (stmt),
			 TYPE_DECL, name, ctx->srecord_type);
      DECL_ARTIFICIAL (name) = 1;
      DECL_NAMELESS (name) = 1;
      TYPE_NAME (ctx->srecord_type) = name;
      TYPE_ARTIFICIAL (ctx->srecord_type) = 1;
      create_omp_child_function (ctx, true);
    }

  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    {
      ctx->record_type = ctx->receiver_decl = NULL;
      t = build_int_cst (long_integer_type_node, 0);
      gimple_omp_task_set_arg_size (stmt, t);
      t = build_int_cst (long_integer_type_node, 1);
      gimple_omp_task_set_arg_align (stmt, t);
    }
}


/* If any decls have been made addressable during scan_omp,
   adjust their fields if needed, and layout record types
   of parallel/task constructs.  */

static void
finish_taskreg_scan (omp_context *ctx)
{
  if (ctx->record_type == NULL_TREE)
    return;

  /* If any task_shared_vars were needed, verify all
     OMP_CLAUSE_SHARED clauses on GIMPLE_OMP_{PARALLEL,TASK}
     statements if use_pointer_for_field hasn't changed
     because of that.  If it did, update field types now.  */
  if (task_shared_vars)
    {
      tree c;

      for (c = gimple_omp_taskreg_clauses (ctx->stmt);
	   c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
	    && !OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	  {
	    tree decl = OMP_CLAUSE_DECL (c);

	    /* Global variables don't need to be copied,
	       the receiver side will use them directly.  */
	    if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx)))
	      continue;
	    if (!bitmap_bit_p (task_shared_vars, DECL_UID (decl))
		|| !use_pointer_for_field (decl, ctx))
	      continue;
	    tree field = lookup_field (decl, ctx);
	    if (TREE_CODE (TREE_TYPE (field)) == POINTER_TYPE
		&& TREE_TYPE (TREE_TYPE (field)) == TREE_TYPE (decl))
	      continue;
	    TREE_TYPE (field) = build_pointer_type (TREE_TYPE (decl));
	    TREE_THIS_VOLATILE (field) = 0;
	    DECL_USER_ALIGN (field) = 0;
	    SET_DECL_ALIGN (field, TYPE_ALIGN (TREE_TYPE (field)));
	    if (TYPE_ALIGN (ctx->record_type) < DECL_ALIGN (field))
	      SET_TYPE_ALIGN (ctx->record_type, DECL_ALIGN (field));
	    if (ctx->srecord_type)
	      {
		tree sfield = lookup_sfield (decl, ctx);
		TREE_TYPE (sfield) = TREE_TYPE (field);
		TREE_THIS_VOLATILE (sfield) = 0;
		DECL_USER_ALIGN (sfield) = 0;
		SET_DECL_ALIGN (sfield, DECL_ALIGN (field));
		if (TYPE_ALIGN (ctx->srecord_type) < DECL_ALIGN (sfield))
		  SET_TYPE_ALIGN (ctx->srecord_type, DECL_ALIGN (sfield));
	      }
	  }
    }

  if (gimple_code (ctx->stmt) == GIMPLE_OMP_PARALLEL)
    {
      layout_type (ctx->record_type);
      fixup_child_record_type (ctx);
    }
  else
    {
      location_t loc = gimple_location (ctx->stmt);
      tree *p, vla_fields = NULL_TREE, *q = &vla_fields;
      /* Move VLA fields to the end.  */
      p = &TYPE_FIELDS (ctx->record_type);
      while (*p)
	if (!TYPE_SIZE_UNIT (TREE_TYPE (*p))
	    || ! TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (*p))))
	  {
	    *q = *p;
	    *p = TREE_CHAIN (*p);
	    TREE_CHAIN (*q) = NULL_TREE;
	    q = &TREE_CHAIN (*q);
	  }
	else
	  p = &DECL_CHAIN (*p);
      *p = vla_fields;
      if (gimple_omp_task_taskloop_p (ctx->stmt))
	{
	  /* Move fields corresponding to first and second _looptemp_
	     clause first.  There are filled by GOMP_taskloop
	     and thus need to be in specific positions.  */
	  tree c1 = gimple_omp_task_clauses (ctx->stmt);
	  c1 = find_omp_clause (c1, OMP_CLAUSE__LOOPTEMP_);
	  tree c2 = find_omp_clause (OMP_CLAUSE_CHAIN (c1),
				     OMP_CLAUSE__LOOPTEMP_);
	  tree f1 = lookup_field (OMP_CLAUSE_DECL (c1), ctx);
	  tree f2 = lookup_field (OMP_CLAUSE_DECL (c2), ctx);
	  p = &TYPE_FIELDS (ctx->record_type);
	  while (*p)
	    if (*p == f1 || *p == f2)
	      *p = DECL_CHAIN (*p);
	    else
	      p = &DECL_CHAIN (*p);
	  DECL_CHAIN (f1) = f2;
	  DECL_CHAIN (f2) = TYPE_FIELDS (ctx->record_type);
	  TYPE_FIELDS (ctx->record_type) = f1;
	  if (ctx->srecord_type)
	    {
	      f1 = lookup_sfield (OMP_CLAUSE_DECL (c1), ctx);
	      f2 = lookup_sfield (OMP_CLAUSE_DECL (c2), ctx);
	      p = &TYPE_FIELDS (ctx->srecord_type);
	      while (*p)
		if (*p == f1 || *p == f2)
		  *p = DECL_CHAIN (*p);
		else
		  p = &DECL_CHAIN (*p);
	      DECL_CHAIN (f1) = f2;
	      DECL_CHAIN (f2) = TYPE_FIELDS (ctx->srecord_type);
	      TYPE_FIELDS (ctx->srecord_type) = f1;
	    }
	}
      layout_type (ctx->record_type);
      fixup_child_record_type (ctx);
      if (ctx->srecord_type)
	layout_type (ctx->srecord_type);
      tree t = fold_convert_loc (loc, long_integer_type_node,
				 TYPE_SIZE_UNIT (ctx->record_type));
      gimple_omp_task_set_arg_size (ctx->stmt, t);
      t = build_int_cst (long_integer_type_node,
			 TYPE_ALIGN_UNIT (ctx->record_type));
      gimple_omp_task_set_arg_align (ctx->stmt, t);
    }
}

/* Find the enclosing offload context.  */

static omp_context *
enclosing_target_ctx (omp_context *ctx)
{
  for (; ctx; ctx = ctx->outer)
    if (gimple_code (ctx->stmt) == GIMPLE_OMP_TARGET)
      break;

  return ctx;
}

/* Return true if ctx is part of an oacc kernels region.  */

static bool
ctx_in_oacc_kernels_region (omp_context *ctx)
{
  for (;ctx != NULL; ctx = ctx->outer)
    {
      gimple *stmt = ctx->stmt;
      if (gimple_code (stmt) == GIMPLE_OMP_TARGET
	  && gimple_omp_target_kind (stmt) == GF_OMP_TARGET_KIND_OACC_KERNELS)
	return true;
    }

  return false;
}

/* Check the parallelism clauses inside a kernels regions.
   Until kernels handling moves to use the same loop indirection
   scheme as parallel, we need to do this checking early.  */

static unsigned
check_oacc_kernel_gwv (gomp_for *stmt, omp_context *ctx)
{
  bool checking = true;
  unsigned outer_mask = 0;
  unsigned this_mask = 0;
  bool has_seq = false, has_auto = false;

  if (ctx->outer)
    outer_mask = check_oacc_kernel_gwv (NULL,  ctx->outer);
  if (!stmt)
    {
      checking = false;
      if (gimple_code (ctx->stmt) != GIMPLE_OMP_FOR)
	return outer_mask;
      stmt = as_a <gomp_for *> (ctx->stmt);
    }

  for (tree c = gimple_omp_for_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_GANG:
	  this_mask |= GOMP_DIM_MASK (GOMP_DIM_GANG);
	  break;
	case OMP_CLAUSE_WORKER:
	  this_mask |= GOMP_DIM_MASK (GOMP_DIM_WORKER);
	  break;
	case OMP_CLAUSE_VECTOR:
	  this_mask |= GOMP_DIM_MASK (GOMP_DIM_VECTOR);
	  break;
	case OMP_CLAUSE_SEQ:
	  has_seq = true;
	  break;
	case OMP_CLAUSE_AUTO:
	  has_auto = true;
	  break;
	default:
	  break;
	}
    }

  if (checking)
    {
      if (has_seq && (this_mask || has_auto))
	error_at (gimple_location (stmt), "%<seq%> overrides other"
		  " OpenACC loop specifiers");
      else if (has_auto && this_mask)
	error_at (gimple_location (stmt), "%<auto%> conflicts with other"
		  " OpenACC loop specifiers");

      if (this_mask & outer_mask)
	error_at (gimple_location (stmt), "inner loop uses same"
		  " OpenACC parallelism as containing loop");
    }

  return outer_mask | this_mask;
}

/* Scan a GIMPLE_OMP_FOR.  */

static void
scan_omp_for (gomp_for *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;
  size_t i;
  tree clauses = gimple_omp_for_clauses (stmt);

  ctx = new_omp_context (stmt, outer_ctx);

  if (is_gimple_omp_oacc (stmt))
    {
      omp_context *tgt = enclosing_target_ctx (outer_ctx);

      if (!tgt || is_oacc_parallel (tgt))
	for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	  {
	    char const *check = NULL;

	    switch (OMP_CLAUSE_CODE (c))
	      {
	      case OMP_CLAUSE_GANG:
		check = "gang";
		break;

	      case OMP_CLAUSE_WORKER:
		check = "worker";
		break;

	      case OMP_CLAUSE_VECTOR:
		check = "vector";
		break;

	      default:
		break;
	      }

	    if (check && OMP_CLAUSE_OPERAND (c, 0))
	      error_at (gimple_location (stmt),
			"argument not permitted on %qs clause in"
			" OpenACC %<parallel%>", check);
	  }

      if (tgt && is_oacc_kernels (tgt))
	{
	  /* Strip out reductions, as they are not  handled yet.  */
	  tree *prev_ptr = &clauses;

	  while (tree probe = *prev_ptr)
	    {
	      tree *next_ptr = &OMP_CLAUSE_CHAIN (probe);
	      
	      if (OMP_CLAUSE_CODE (probe) == OMP_CLAUSE_REDUCTION)
		*prev_ptr = *next_ptr;
	      else
		prev_ptr = next_ptr;
	    }

	  gimple_omp_for_set_clauses (stmt, clauses);
	  check_oacc_kernel_gwv (stmt, ctx);
	}
    }

  scan_sharing_clauses (clauses, ctx);

  scan_omp (gimple_omp_for_pre_body_ptr (stmt), ctx);
  for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
    {
      scan_omp_op (gimple_omp_for_index_ptr (stmt, i), ctx);
      scan_omp_op (gimple_omp_for_initial_ptr (stmt, i), ctx);
      scan_omp_op (gimple_omp_for_final_ptr (stmt, i), ctx);
      scan_omp_op (gimple_omp_for_incr_ptr (stmt, i), ctx);
    }
  scan_omp (gimple_omp_body_ptr (stmt), ctx);
}

/* Scan an OpenMP sections directive.  */

static void
scan_omp_sections (gomp_sections *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;

  ctx = new_omp_context (stmt, outer_ctx);
  scan_sharing_clauses (gimple_omp_sections_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);
}

/* Scan an OpenMP single directive.  */

static void
scan_omp_single (gomp_single *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name;

  ctx = new_omp_context (stmt, outer_ctx);
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_copy_s");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  TYPE_NAME (ctx->record_type) = name;

  scan_sharing_clauses (gimple_omp_single_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    ctx->record_type = NULL;
  else
    layout_type (ctx->record_type);
}

/* Return true if the CLAUSES of an omp target guarantee that the base pointers
   used in the corresponding offloaded function are restrict.  */

static bool
omp_target_base_pointers_restrict_p (tree clauses)
{
  /* The analysis relies on the GOMP_MAP_FORCE_* mapping kinds, which are only
     used by OpenACC.  */
  if (flag_openacc == 0)
    return false;

  /* I.  Basic example:

       void foo (void)
       {
	 unsigned int a[2], b[2];

	 #pragma acc kernels \
	   copyout (a) \
	   copyout (b)
	 {
	   a[0] = 0;
	   b[0] = 1;
	 }
       }

     After gimplification, we have:

       #pragma omp target oacc_kernels \
	 map(force_from:a [len: 8]) \
	 map(force_from:b [len: 8])
       {
	 a[0] = 0;
	 b[0] = 1;
       }

     Because both mappings have the force prefix, we know that they will be
     allocated when calling the corresponding offloaded function, which means we
     can mark the base pointers for a and b in the offloaded function as
     restrict.  */

  tree c;
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
	return false;

      switch (OMP_CLAUSE_MAP_KIND (c))
	{
	case GOMP_MAP_FORCE_ALLOC:
	case GOMP_MAP_FORCE_TO:
	case GOMP_MAP_FORCE_FROM:
	case GOMP_MAP_FORCE_TOFROM:
	  break;
	default:
	  return false;
	}
    }

  return true;
}

/* Scan a GIMPLE_OMP_TARGET.  */

static void
scan_omp_target (gomp_target *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name;
  bool offloaded = is_gimple_omp_offloaded (stmt);
  tree clauses = gimple_omp_target_clauses (stmt);

  ctx = new_omp_context (stmt, outer_ctx);
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->default_kind = OMP_CLAUSE_DEFAULT_SHARED;
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_data_t");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  DECL_ARTIFICIAL (name) = 1;
  DECL_NAMELESS (name) = 1;
  TYPE_NAME (ctx->record_type) = name;
  TYPE_ARTIFICIAL (ctx->record_type) = 1;

  bool base_pointers_restrict = false;
  if (offloaded)
    {
      create_omp_child_function (ctx, false);
      gimple_omp_target_set_child_fn (stmt, ctx->cb.dst_fn);

      base_pointers_restrict = omp_target_base_pointers_restrict_p (clauses);
      if (base_pointers_restrict
	  && dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Base pointers in offloaded function are restrict\n");
    }

  scan_sharing_clauses (clauses, ctx, base_pointers_restrict);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    ctx->record_type = ctx->receiver_decl = NULL;
  else
    {
      TYPE_FIELDS (ctx->record_type)
	= nreverse (TYPE_FIELDS (ctx->record_type));
      if (flag_checking)
	{
	  unsigned int align = DECL_ALIGN (TYPE_FIELDS (ctx->record_type));
	  for (tree field = TYPE_FIELDS (ctx->record_type);
	       field;
	       field = DECL_CHAIN (field))
	    gcc_assert (DECL_ALIGN (field) == align);
	}
      layout_type (ctx->record_type);
      if (offloaded)
	fixup_child_record_type (ctx);
    }
}

/* Scan an OpenMP teams directive.  */

static void
scan_omp_teams (gomp_teams *stmt, omp_context *outer_ctx)
{
  omp_context *ctx = new_omp_context (stmt, outer_ctx);
  scan_sharing_clauses (gimple_omp_teams_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);
}

/* Check nesting restrictions.  */
static bool
check_omp_nesting_restrictions (gimple *stmt, omp_context *ctx)
{
  tree c;

  if (ctx && gimple_code (ctx->stmt) == GIMPLE_OMP_GRID_BODY)
    /* GRID_BODY is an artificial construct, nesting rules will be checked in
       the original copy of its contents.  */
    return true;

  /* No nesting of non-OpenACC STMT (that is, an OpenMP one, or a GOMP builtin)
     inside an OpenACC CTX.  */
  if (!(is_gimple_omp (stmt)
	&& is_gimple_omp_oacc (stmt))
      /* Except for atomic codes that we share with OpenMP.  */
      && !(gimple_code (stmt) == GIMPLE_OMP_ATOMIC_LOAD
	   || gimple_code (stmt) == GIMPLE_OMP_ATOMIC_STORE))
    {
      if (get_oacc_fn_attrib (cfun->decl) != NULL)
	{
	  error_at (gimple_location (stmt),
		    "non-OpenACC construct inside of OpenACC routine");
	  return false;
	}
      else
	for (omp_context *octx = ctx; octx != NULL; octx = octx->outer)
	  if (is_gimple_omp (octx->stmt)
	      && is_gimple_omp_oacc (octx->stmt))
	    {
	      error_at (gimple_location (stmt),
			"non-OpenACC construct inside of OpenACC region");
	      return false;
	    }
    }

  if (ctx != NULL)
    {
      if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	  && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
	{
	  c = NULL_TREE;
	  if (gimple_code (stmt) == GIMPLE_OMP_ORDERED)
	    {
	      c = gimple_omp_ordered_clauses (as_a <gomp_ordered *> (stmt));
	      if (find_omp_clause (c, OMP_CLAUSE_SIMD))
		{
		  if (find_omp_clause (c, OMP_CLAUSE_THREADS)
		      && (ctx->outer == NULL
			  || !gimple_omp_for_combined_into_p (ctx->stmt)
			  || gimple_code (ctx->outer->stmt) != GIMPLE_OMP_FOR
			  || (gimple_omp_for_kind (ctx->outer->stmt)
			      != GF_OMP_FOR_KIND_FOR)
			  || !gimple_omp_for_combined_p (ctx->outer->stmt)))
		    {
		      error_at (gimple_location (stmt),
				"%<ordered simd threads%> must be closely "
				"nested inside of %<for simd%> region");
		      return false;
		    }
		  return true;
		}
	    }
	  error_at (gimple_location (stmt),
		    "OpenMP constructs other than %<#pragma omp ordered simd%>"
		    " may not be nested inside %<simd%> region");
	  return false;
	}
      else if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
	{
	  if ((gimple_code (stmt) != GIMPLE_OMP_FOR
	       || (gimple_omp_for_kind (stmt)
		   != GF_OMP_FOR_KIND_DISTRIBUTE))
	      && gimple_code (stmt) != GIMPLE_OMP_PARALLEL)
	    {
	      error_at (gimple_location (stmt),
			"only %<distribute%> or %<parallel%> regions are "
			"allowed to be strictly nested inside %<teams%> "
			"region");
	      return false;
	    }
	}
    }
  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_FOR:
      if (gimple_omp_for_kind (stmt) & GF_OMP_FOR_SIMD)
	return true;
      if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_DISTRIBUTE)
	{
	  if (ctx != NULL && gimple_code (ctx->stmt) != GIMPLE_OMP_TEAMS)
	    {
	      error_at (gimple_location (stmt),
			"%<distribute%> region must be strictly nested "
			"inside %<teams%> construct");
	      return false;
	    }
	  return true;
	}
      /* We split taskloop into task and nested taskloop in it.  */
      if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_TASKLOOP)
	return true;
      if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_OACC_LOOP)
	{
	  bool ok = false;
	  
	  if (ctx)
	    switch (gimple_code (ctx->stmt))
	      {
	      case GIMPLE_OMP_FOR:
		ok = (gimple_omp_for_kind (ctx->stmt)
		      == GF_OMP_FOR_KIND_OACC_LOOP);
		break;

	      case GIMPLE_OMP_TARGET:
		switch (gimple_omp_target_kind (ctx->stmt))
		  {
		  case GF_OMP_TARGET_KIND_OACC_PARALLEL:
		  case GF_OMP_TARGET_KIND_OACC_KERNELS:
		    ok = true;
		    break;

		  default:
		    break;
		  }

	      default:
		break;
	      }
	  else if (get_oacc_fn_attrib (current_function_decl))
	    ok = true;
	  if (!ok)
	    {
	      error_at (gimple_location (stmt),
			"OpenACC loop directive must be associated with"
			" an OpenACC compute region");
	      return false;
	    }
	}
      /* FALLTHRU */
    case GIMPLE_CALL:
      if (is_gimple_call (stmt)
	  && (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
	      == BUILT_IN_GOMP_CANCEL
	      || DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		 == BUILT_IN_GOMP_CANCELLATION_POINT))
	{
	  const char *bad = NULL;
	  const char *kind = NULL;
	  const char *construct
	    = (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
	       == BUILT_IN_GOMP_CANCEL)
	      ? "#pragma omp cancel"
	      : "#pragma omp cancellation point";
	  if (ctx == NULL)
	    {
	      error_at (gimple_location (stmt), "orphaned %qs construct",
			construct);
	      return false;
	    }
	  switch (tree_fits_shwi_p (gimple_call_arg (stmt, 0))
		  ? tree_to_shwi (gimple_call_arg (stmt, 0))
		  : 0)
	    {
	    case 1:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_PARALLEL)
		bad = "#pragma omp parallel";
	      else if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		       == BUILT_IN_GOMP_CANCEL
		       && !integer_zerop (gimple_call_arg (stmt, 1)))
		ctx->cancellable = true;
	      kind = "parallel";
	      break;
	    case 2:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_FOR
		  || gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_FOR)
		bad = "#pragma omp for";
	      else if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		       == BUILT_IN_GOMP_CANCEL
		       && !integer_zerop (gimple_call_arg (stmt, 1)))
		{
		  ctx->cancellable = true;
		  if (find_omp_clause (gimple_omp_for_clauses (ctx->stmt),
				       OMP_CLAUSE_NOWAIT))
		    warning_at (gimple_location (stmt), 0,
				"%<#pragma omp cancel for%> inside "
				"%<nowait%> for construct");
		  if (find_omp_clause (gimple_omp_for_clauses (ctx->stmt),
				       OMP_CLAUSE_ORDERED))
		    warning_at (gimple_location (stmt), 0,
				"%<#pragma omp cancel for%> inside "
				"%<ordered%> for construct");
		}
	      kind = "for";
	      break;
	    case 4:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_SECTIONS
		  && gimple_code (ctx->stmt) != GIMPLE_OMP_SECTION)
		bad = "#pragma omp sections";
	      else if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		       == BUILT_IN_GOMP_CANCEL
		       && !integer_zerop (gimple_call_arg (stmt, 1)))
		{
		  if (gimple_code (ctx->stmt) == GIMPLE_OMP_SECTIONS)
		    {
		      ctx->cancellable = true;
		      if (find_omp_clause (gimple_omp_sections_clauses
								(ctx->stmt),
					   OMP_CLAUSE_NOWAIT))
			warning_at (gimple_location (stmt), 0,
				    "%<#pragma omp cancel sections%> inside "
				    "%<nowait%> sections construct");
		    }
		  else
		    {
		      gcc_assert (ctx->outer
				  && gimple_code (ctx->outer->stmt)
				     == GIMPLE_OMP_SECTIONS);
		      ctx->outer->cancellable = true;
		      if (find_omp_clause (gimple_omp_sections_clauses
							(ctx->outer->stmt),
					   OMP_CLAUSE_NOWAIT))
			warning_at (gimple_location (stmt), 0,
				    "%<#pragma omp cancel sections%> inside "
				    "%<nowait%> sections construct");
		    }
		}
	      kind = "sections";
	      break;
	    case 8:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_TASK)
		bad = "#pragma omp task";
	      else
		{
		  for (omp_context *octx = ctx->outer;
		       octx; octx = octx->outer)
		    {
		      switch (gimple_code (octx->stmt))
			{
			case GIMPLE_OMP_TASKGROUP:
			  break;
			case GIMPLE_OMP_TARGET:
			  if (gimple_omp_target_kind (octx->stmt)
			      != GF_OMP_TARGET_KIND_REGION)
			    continue;
			  /* FALLTHRU */
			case GIMPLE_OMP_PARALLEL:
			case GIMPLE_OMP_TEAMS:
			  error_at (gimple_location (stmt),
				    "%<%s taskgroup%> construct not closely "
				    "nested inside of %<taskgroup%> region",
				    construct);
			  return false;
			default:
			  continue;
			}
		      break;
		    }
		  ctx->cancellable = true;
		}
	      kind = "taskgroup";
	      break;
	    default:
	      error_at (gimple_location (stmt), "invalid arguments");
	      return false;
	    }
	  if (bad)
	    {
	      error_at (gimple_location (stmt),
			"%<%s %s%> construct not closely nested inside of %qs",
			construct, kind, bad);
	      return false;
	    }
	}
      /* FALLTHRU */
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
      for (; ctx != NULL; ctx = ctx->outer)
	switch (gimple_code (ctx->stmt))
	  {
	  case GIMPLE_OMP_FOR:
	    if (gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_FOR
		&& gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_TASKLOOP)
	      break;
	    /* FALLTHRU */
	  case GIMPLE_OMP_SECTIONS:
	  case GIMPLE_OMP_SINGLE:
	  case GIMPLE_OMP_ORDERED:
	  case GIMPLE_OMP_MASTER:
	  case GIMPLE_OMP_TASK:
	  case GIMPLE_OMP_CRITICAL:
	    if (is_gimple_call (stmt))
	      {
		if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		    != BUILT_IN_GOMP_BARRIER)
		  return true;
		error_at (gimple_location (stmt),
			  "barrier region may not be closely nested inside "
			  "of work-sharing, %<critical%>, %<ordered%>, "
			  "%<master%>, explicit %<task%> or %<taskloop%> "
			  "region");
		return false;
	      }
	    error_at (gimple_location (stmt),
		      "work-sharing region may not be closely nested inside "
		      "of work-sharing, %<critical%>, %<ordered%>, "
		      "%<master%>, explicit %<task%> or %<taskloop%> region");
	    return false;
	  case GIMPLE_OMP_PARALLEL:
	  case GIMPLE_OMP_TEAMS:
	    return true;
	  case GIMPLE_OMP_TARGET:
	    if (gimple_omp_target_kind (ctx->stmt)
		== GF_OMP_TARGET_KIND_REGION)
	      return true;
	    break;
	  default:
	    break;
	  }
      break;
    case GIMPLE_OMP_MASTER:
      for (; ctx != NULL; ctx = ctx->outer)
	switch (gimple_code (ctx->stmt))
	  {
	  case GIMPLE_OMP_FOR:
	    if (gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_FOR
		&& gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_TASKLOOP)
	      break;
	    /* FALLTHRU */
	  case GIMPLE_OMP_SECTIONS:
	  case GIMPLE_OMP_SINGLE:
	  case GIMPLE_OMP_TASK:
	    error_at (gimple_location (stmt),
		      "%<master%> region may not be closely nested inside "
		      "of work-sharing, explicit %<task%> or %<taskloop%> "
		      "region");
	    return false;
	  case GIMPLE_OMP_PARALLEL:
	  case GIMPLE_OMP_TEAMS:
	    return true;
	  case GIMPLE_OMP_TARGET:
	    if (gimple_omp_target_kind (ctx->stmt)
		== GF_OMP_TARGET_KIND_REGION)
	      return true;
	    break;
	  default:
	    break;
	  }
      break;
    case GIMPLE_OMP_TASK:
      for (c = gimple_omp_task_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE
		|| OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK))
	  {
	    enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_KIND (c);
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<depend(%s)%> is only allowed in %<omp ordered%>",
		      kind == OMP_CLAUSE_DEPEND_SOURCE ? "source" : "sink");
	    return false;
	  }
      break;
    case GIMPLE_OMP_ORDERED:
      for (c = gimple_omp_ordered_clauses (as_a <gomp_ordered *> (stmt));
	   c; c = OMP_CLAUSE_CHAIN (c))
	{
	  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND)
	    {
	      gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_THREADS
			  || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SIMD);
	      continue;
	    }
	  enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_KIND (c);
	  if (kind == OMP_CLAUSE_DEPEND_SOURCE
	      || kind == OMP_CLAUSE_DEPEND_SINK)
	    {
	      tree oclause;
	      /* Look for containing ordered(N) loop.  */
	      if (ctx == NULL
		  || gimple_code (ctx->stmt) != GIMPLE_OMP_FOR
		  || (oclause
			= find_omp_clause (gimple_omp_for_clauses (ctx->stmt),
					   OMP_CLAUSE_ORDERED)) == NULL_TREE)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<ordered%> construct with %<depend%> clause "
			    "must be closely nested inside an %<ordered%> "
			    "loop");
		  return false;
		}
	      else if (OMP_CLAUSE_ORDERED_EXPR (oclause) == NULL_TREE)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<ordered%> construct with %<depend%> clause "
			    "must be closely nested inside a loop with "
			    "%<ordered%> clause with a parameter");
		  return false;
		}
	    }
	  else
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"invalid depend kind in omp %<ordered%> %<depend%>");
	      return false;
	    }
	}
      c = gimple_omp_ordered_clauses (as_a <gomp_ordered *> (stmt));
      if (find_omp_clause (c, OMP_CLAUSE_SIMD))
	{
	  /* ordered simd must be closely nested inside of simd region,
	     and simd region must not encounter constructs other than
	     ordered simd, therefore ordered simd may be either orphaned,
	     or ctx->stmt must be simd.  The latter case is handled already
	     earlier.  */
	  if (ctx != NULL)
	    {
	      error_at (gimple_location (stmt),
			"%<ordered%> %<simd%> must be closely nested inside "
			"%<simd%> region");
	      return false;
	    }
	}
      for (; ctx != NULL; ctx = ctx->outer)
	switch (gimple_code (ctx->stmt))
	  {
	  case GIMPLE_OMP_CRITICAL:
	  case GIMPLE_OMP_TASK:
	  case GIMPLE_OMP_ORDERED:
	  ordered_in_taskloop:
	    error_at (gimple_location (stmt),
		      "%<ordered%> region may not be closely nested inside "
		      "of %<critical%>, %<ordered%>, explicit %<task%> or "
		      "%<taskloop%> region");
	    return false;
	  case GIMPLE_OMP_FOR:
	    if (gimple_omp_for_kind (ctx->stmt) == GF_OMP_FOR_KIND_TASKLOOP)
	      goto ordered_in_taskloop;
	    if (find_omp_clause (gimple_omp_for_clauses (ctx->stmt),
				 OMP_CLAUSE_ORDERED) == NULL)
	      {
		error_at (gimple_location (stmt),
			  "%<ordered%> region must be closely nested inside "
			  "a loop region with an %<ordered%> clause");
		return false;
	      }
	    return true;
	  case GIMPLE_OMP_TARGET:
	    if (gimple_omp_target_kind (ctx->stmt)
		!= GF_OMP_TARGET_KIND_REGION)
	      break;
	    /* FALLTHRU */
	  case GIMPLE_OMP_PARALLEL:
	  case GIMPLE_OMP_TEAMS:
	    error_at (gimple_location (stmt),
		      "%<ordered%> region must be closely nested inside "
		      "a loop region with an %<ordered%> clause");
	    return false;
	  default:
	    break;
	  }
      break;
    case GIMPLE_OMP_CRITICAL:
      {
	tree this_stmt_name
	  = gimple_omp_critical_name (as_a <gomp_critical *> (stmt));
	for (; ctx != NULL; ctx = ctx->outer)
	  if (gomp_critical *other_crit
	        = dyn_cast <gomp_critical *> (ctx->stmt))
	    if (this_stmt_name == gimple_omp_critical_name (other_crit))
	      {
		error_at (gimple_location (stmt),
			  "%<critical%> region may not be nested inside "
			   "a %<critical%> region with the same name");
		return false;
	      }
      }
      break;
    case GIMPLE_OMP_TEAMS:
      if (ctx == NULL
	  || gimple_code (ctx->stmt) != GIMPLE_OMP_TARGET
	  || gimple_omp_target_kind (ctx->stmt) != GF_OMP_TARGET_KIND_REGION)
	{
	  error_at (gimple_location (stmt),
		    "%<teams%> construct not closely nested inside of "
		    "%<target%> construct");
	  return false;
	}
      break;
    case GIMPLE_OMP_TARGET:
      for (c = gimple_omp_target_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE
		|| OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK))
	  {
	    enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_KIND (c);
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<depend(%s)%> is only allowed in %<omp ordered%>",
		      kind == OMP_CLAUSE_DEPEND_SOURCE ? "source" : "sink");
	    return false;
	  }
      if (is_gimple_omp_offloaded (stmt)
	  && get_oacc_fn_attrib (cfun->decl) != NULL)
	{
	  error_at (gimple_location (stmt),
		    "OpenACC region inside of OpenACC routine, nested "
		    "parallelism not supported yet");
	  return false;
	}
      for (; ctx != NULL; ctx = ctx->outer)
	{
	  if (gimple_code (ctx->stmt) != GIMPLE_OMP_TARGET)
	    {
	      if (is_gimple_omp (stmt)
		  && is_gimple_omp_oacc (stmt)
		  && is_gimple_omp (ctx->stmt))
		{
		  error_at (gimple_location (stmt),
			    "OpenACC construct inside of non-OpenACC region");
		  return false;
		}
	      continue;
	    }

	  const char *stmt_name, *ctx_stmt_name;
	  switch (gimple_omp_target_kind (stmt))
	    {
	    case GF_OMP_TARGET_KIND_REGION: stmt_name = "target"; break;
	    case GF_OMP_TARGET_KIND_DATA: stmt_name = "target data"; break;
	    case GF_OMP_TARGET_KIND_UPDATE: stmt_name = "target update"; break;
	    case GF_OMP_TARGET_KIND_ENTER_DATA:
	      stmt_name = "target enter data"; break;
	    case GF_OMP_TARGET_KIND_EXIT_DATA:
	      stmt_name = "target exit data"; break;
	    case GF_OMP_TARGET_KIND_OACC_PARALLEL: stmt_name = "parallel"; break;
	    case GF_OMP_TARGET_KIND_OACC_KERNELS: stmt_name = "kernels"; break;
	    case GF_OMP_TARGET_KIND_OACC_DATA: stmt_name = "data"; break;
	    case GF_OMP_TARGET_KIND_OACC_UPDATE: stmt_name = "update"; break;
	    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
	      stmt_name = "enter/exit data"; break;
	    case GF_OMP_TARGET_KIND_OACC_HOST_DATA: stmt_name = "host_data";
	      break;
	    default: gcc_unreachable ();
	    }
	  switch (gimple_omp_target_kind (ctx->stmt))
	    {
	    case GF_OMP_TARGET_KIND_REGION: ctx_stmt_name = "target"; break;
	    case GF_OMP_TARGET_KIND_DATA: ctx_stmt_name = "target data"; break;
	    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
	      ctx_stmt_name = "parallel"; break;
	    case GF_OMP_TARGET_KIND_OACC_KERNELS:
	      ctx_stmt_name = "kernels"; break;
	    case GF_OMP_TARGET_KIND_OACC_DATA: ctx_stmt_name = "data"; break;
	    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
	      ctx_stmt_name = "host_data"; break;
	    default: gcc_unreachable ();
	    }

	  /* OpenACC/OpenMP mismatch?  */
	  if (is_gimple_omp_oacc (stmt)
	      != is_gimple_omp_oacc (ctx->stmt))
	    {
	      error_at (gimple_location (stmt),
			"%s %qs construct inside of %s %qs region",
			(is_gimple_omp_oacc (stmt)
			 ? "OpenACC" : "OpenMP"), stmt_name,
			(is_gimple_omp_oacc (ctx->stmt)
			 ? "OpenACC" : "OpenMP"), ctx_stmt_name);
	      return false;
	    }
	  if (is_gimple_omp_offloaded (ctx->stmt))
	    {
	      /* No GIMPLE_OMP_TARGET inside offloaded OpenACC CTX.  */
	      if (is_gimple_omp_oacc (ctx->stmt))
		{
		  error_at (gimple_location (stmt),
			    "%qs construct inside of %qs region",
			    stmt_name, ctx_stmt_name);
		  return false;
		}
	      else
		{
		  warning_at (gimple_location (stmt), 0,
			      "%qs construct inside of %qs region",
			      stmt_name, ctx_stmt_name);
		}
	    }
	}
      break;
    default:
      break;
    }
  return true;
}


/* Helper function scan_omp.

   Callback for walk_tree or operators in walk_gimple_stmt used to
   scan for OMP directives in TP.  */

static tree
scan_omp_1_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  omp_context *ctx = (omp_context *) wi->info;
  tree t = *tp;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case LABEL_DECL:
    case RESULT_DECL:
      if (ctx)
	{
	  tree repl = remap_decl (t, &ctx->cb);
	  gcc_checking_assert (TREE_CODE (repl) != ERROR_MARK);
	  *tp = repl;
	}
      break;

    default:
      if (ctx && TYPE_P (t))
	*tp = remap_type (t, &ctx->cb);
      else if (!DECL_P (t))
	{
	  *walk_subtrees = 1;
	  if (ctx)
	    {
	      tree tem = remap_type (TREE_TYPE (t), &ctx->cb);
	      if (tem != TREE_TYPE (t))
		{
		  if (TREE_CODE (t) == INTEGER_CST)
		    *tp = wide_int_to_tree (tem, t);
		  else
		    TREE_TYPE (t) = tem;
		}
	    }
	}
      break;
    }

  return NULL_TREE;
}

/* Return true if FNDECL is a setjmp or a longjmp.  */

static bool
setjmp_or_longjmp_p (const_tree fndecl)
{
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_SETJMP
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_LONGJMP))
    return true;

  tree declname = DECL_NAME (fndecl);
  if (!declname)
    return false;
  const char *name = IDENTIFIER_POINTER (declname);
  return !strcmp (name, "setjmp") || !strcmp (name, "longjmp");
}


/* Helper function for scan_omp.

   Callback for walk_gimple_stmt used to scan for OMP directives in
   the current statement in GSI.  */

static tree
scan_omp_1_stmt (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		 struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi);
  omp_context *ctx = (omp_context *) wi->info;

  if (gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  /* Check the nesting restrictions.  */
  bool remove = false;
  if (is_gimple_omp (stmt))
    remove = !check_omp_nesting_restrictions (stmt, ctx);
  else if (is_gimple_call (stmt))
    {
      tree fndecl = gimple_call_fndecl (stmt);
      if (fndecl)
	{
	  if (setjmp_or_longjmp_p (fndecl)
	      && ctx
	      && gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	      && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
	    {
	      remove = true;
	      error_at (gimple_location (stmt),
			"setjmp/longjmp inside simd construct");
	    }
	  else if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	    switch (DECL_FUNCTION_CODE (fndecl))
	      {
	      case BUILT_IN_GOMP_BARRIER:
	      case BUILT_IN_GOMP_CANCEL:
	      case BUILT_IN_GOMP_CANCELLATION_POINT:
	      case BUILT_IN_GOMP_TASKYIELD:
	      case BUILT_IN_GOMP_TASKWAIT:
	      case BUILT_IN_GOMP_TASKGROUP_START:
	      case BUILT_IN_GOMP_TASKGROUP_END:
		remove = !check_omp_nesting_restrictions (stmt, ctx);
		break;
	      default:
		break;
	      }
	}
    }
  if (remove)
    {
      stmt = gimple_build_nop ();
      gsi_replace (gsi, stmt, false);
    }

  *handled_ops_p = true;

  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_PARALLEL:
      taskreg_nesting_level++;
      scan_omp_parallel (gsi, ctx);
      taskreg_nesting_level--;
      break;

    case GIMPLE_OMP_TASK:
      taskreg_nesting_level++;
      scan_omp_task (gsi, ctx);
      taskreg_nesting_level--;
      break;

    case GIMPLE_OMP_FOR:
      scan_omp_for (as_a <gomp_for *> (stmt), ctx);
      break;

    case GIMPLE_OMP_SECTIONS:
      scan_omp_sections (as_a <gomp_sections *> (stmt), ctx);
      break;

    case GIMPLE_OMP_SINGLE:
      scan_omp_single (as_a <gomp_single *> (stmt), ctx);
      break;

    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_GRID_BODY:
      ctx = new_omp_context (stmt, ctx);
      scan_omp (gimple_omp_body_ptr (stmt), ctx);
      break;

    case GIMPLE_OMP_TARGET:
      scan_omp_target (as_a <gomp_target *> (stmt), ctx);
      break;

    case GIMPLE_OMP_TEAMS:
      scan_omp_teams (as_a <gomp_teams *> (stmt), ctx);
      break;

    case GIMPLE_BIND:
      {
	tree var;

	*handled_ops_p = false;
	if (ctx)
	  for (var = gimple_bind_vars (as_a <gbind *> (stmt));
	       var ;
	       var = DECL_CHAIN (var))
	    insert_decl_map (&ctx->cb, var, var);
      }
      break;
    default:
      *handled_ops_p = false;
      break;
    }

  return NULL_TREE;
}


/* Scan all the statements starting at the current statement.  CTX
   contains context information about the OMP directives and
   clauses found during the scan.  */

static void
scan_omp (gimple_seq *body_p, omp_context *ctx)
{
  location_t saved_location;
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.info = ctx;
  wi.want_locations = true;

  saved_location = input_location;
  walk_gimple_seq_mod (body_p, scan_omp_1_stmt, scan_omp_1_op, &wi);
  input_location = saved_location;
}

/* Re-gimplification and code generation routines.  */

/* Build a call to GOMP_barrier.  */

static gimple *
build_omp_barrier (tree lhs)
{
  tree fndecl = builtin_decl_explicit (lhs ? BUILT_IN_GOMP_BARRIER_CANCEL
					   : BUILT_IN_GOMP_BARRIER);
  gcall *g = gimple_build_call (fndecl, 0);
  if (lhs)
    gimple_call_set_lhs (g, lhs);
  return g;
}

/* If a context was created for STMT when it was scanned, return it.  */

static omp_context *
maybe_lookup_ctx (gimple *stmt)
{
  splay_tree_node n;
  n = splay_tree_lookup (all_contexts, (splay_tree_key) stmt);
  return n ? (omp_context *) n->value : NULL;
}


/* Find the mapping for DECL in CTX or the immediately enclosing
   context that has a mapping for DECL.

   If CTX is a nested parallel directive, we may have to use the decl
   mappings created in CTX's parent context.  Suppose that we have the
   following parallel nesting (variable UIDs showed for clarity):

	iD.1562 = 0;
     	#omp parallel shared(iD.1562)		-> outer parallel
	  iD.1562 = iD.1562 + 1;

	  #omp parallel shared (iD.1562)	-> inner parallel
	     iD.1562 = iD.1562 - 1;

   Each parallel structure will create a distinct .omp_data_s structure
   for copying iD.1562 in/out of the directive:

  	outer parallel		.omp_data_s.1.i -> iD.1562
	inner parallel		.omp_data_s.2.i -> iD.1562

   A shared variable mapping will produce a copy-out operation before
   the parallel directive and a copy-in operation after it.  So, in
   this case we would have:

  	iD.1562 = 0;
	.omp_data_o.1.i = iD.1562;
	#omp parallel shared(iD.1562)		-> outer parallel
	  .omp_data_i.1 = &.omp_data_o.1
	  .omp_data_i.1->i = .omp_data_i.1->i + 1;

	  .omp_data_o.2.i = iD.1562;		-> **
	  #omp parallel shared(iD.1562)		-> inner parallel
	    .omp_data_i.2 = &.omp_data_o.2
	    .omp_data_i.2->i = .omp_data_i.2->i - 1;


    ** This is a problem.  The symbol iD.1562 cannot be referenced
       inside the body of the outer parallel region.  But since we are
       emitting this copy operation while expanding the inner parallel
       directive, we need to access the CTX structure of the outer
       parallel directive to get the correct mapping:

	  .omp_data_o.2.i = .omp_data_i.1->i

    Since there may be other workshare or parallel directives enclosing
    the parallel directive, it may be necessary to walk up the context
    parent chain.  This is not a problem in general because nested
    parallelism happens only rarely.  */

static tree
lookup_decl_in_outer_ctx (tree decl, omp_context *ctx)
{
  tree t;
  omp_context *up;

  for (up = ctx->outer, t = NULL; up && t == NULL; up = up->outer)
    t = maybe_lookup_decl (decl, up);

  gcc_assert (!ctx->is_nested || t || is_global_var (decl));

  return t ? t : decl;
}


/* Similar to lookup_decl_in_outer_ctx, but return DECL if not found
   in outer contexts.  */

static tree
maybe_lookup_decl_in_outer_ctx (tree decl, omp_context *ctx)
{
  tree t = NULL;
  omp_context *up;

  for (up = ctx->outer, t = NULL; up && t == NULL; up = up->outer)
    t = maybe_lookup_decl (decl, up);

  return t ? t : decl;
}


/* Construct the initialization value for reduction operation OP.  */

tree
omp_reduction_init_op (location_t loc, enum tree_code op, tree type)
{
  switch (op)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_XOR_EXPR:
    case NE_EXPR:
      return build_zero_cst (type);

    case MULT_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case EQ_EXPR:
      return fold_convert_loc (loc, type, integer_one_node);

    case BIT_AND_EXPR:
      return fold_convert_loc (loc, type, integer_minus_one_node);

    case MAX_EXPR:
      if (SCALAR_FLOAT_TYPE_P (type))
	{
	  REAL_VALUE_TYPE max, min;
	  if (HONOR_INFINITIES (type))
	    {
	      real_inf (&max);
	      real_arithmetic (&min, NEGATE_EXPR, &max, NULL);
	    }
	  else
	    real_maxval (&min, 1, TYPE_MODE (type));
	  return build_real (type, min);
	}
      else if (POINTER_TYPE_P (type))
	{
	  wide_int min
	    = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
	  return wide_int_to_tree (type, min);
	}
      else
	{
	  gcc_assert (INTEGRAL_TYPE_P (type));
	  return TYPE_MIN_VALUE (type);
	}

    case MIN_EXPR:
      if (SCALAR_FLOAT_TYPE_P (type))
	{
	  REAL_VALUE_TYPE max;
	  if (HONOR_INFINITIES (type))
	    real_inf (&max);
	  else
	    real_maxval (&max, 0, TYPE_MODE (type));
	  return build_real (type, max);
	}
      else if (POINTER_TYPE_P (type))
	{
	  wide_int max
	    = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
	  return wide_int_to_tree (type, max);
	}
      else
	{
	  gcc_assert (INTEGRAL_TYPE_P (type));
	  return TYPE_MAX_VALUE (type);
	}

    default:
      gcc_unreachable ();
    }
}

/* Construct the initialization value for reduction CLAUSE.  */

tree
omp_reduction_init (tree clause, tree type)
{
  return omp_reduction_init_op (OMP_CLAUSE_LOCATION (clause),
				OMP_CLAUSE_REDUCTION_CODE (clause), type);
}

/* Return alignment to be assumed for var in CLAUSE, which should be
   OMP_CLAUSE_ALIGNED.  */

static tree
omp_clause_aligned_alignment (tree clause)
{
  if (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause))
    return OMP_CLAUSE_ALIGNED_ALIGNMENT (clause);

  /* Otherwise return implementation defined alignment.  */
  unsigned int al = 1;
  machine_mode mode, vmode;
  int vs = targetm.vectorize.autovectorize_vector_sizes ();
  if (vs)
    vs = 1 << floor_log2 (vs);
  static enum mode_class classes[]
    = { MODE_INT, MODE_VECTOR_INT, MODE_FLOAT, MODE_VECTOR_FLOAT };
  for (int i = 0; i < 4; i += 2)
    for (mode = GET_CLASS_NARROWEST_MODE (classes[i]);
	 mode != VOIDmode;
	 mode = GET_MODE_WIDER_MODE (mode))
      {
	vmode = targetm.vectorize.preferred_simd_mode (mode);
	if (GET_MODE_CLASS (vmode) != classes[i + 1])
	  continue;
	while (vs
	       && GET_MODE_SIZE (vmode) < vs
	       && GET_MODE_2XWIDER_MODE (vmode) != VOIDmode)
	  vmode = GET_MODE_2XWIDER_MODE (vmode);
	
	tree type = lang_hooks.types.type_for_mode (mode, 1);
	if (type == NULL_TREE || TYPE_MODE (type) != mode)
	  continue;
	type = build_vector_type (type, GET_MODE_SIZE (vmode)
					/ GET_MODE_SIZE (mode));
	if (TYPE_MODE (type) != vmode)
	  continue;
	if (TYPE_ALIGN_UNIT (type) > al)
	  al = TYPE_ALIGN_UNIT (type);
      }
  return build_int_cst (integer_type_node, al);
}

/* Return maximum possible vectorization factor for the target.  */

static int
omp_max_vf (void)
{
  if (!optimize
      || optimize_debug
      || !flag_tree_loop_optimize
      || (!flag_tree_loop_vectorize
	  && (global_options_set.x_flag_tree_loop_vectorize
              || global_options_set.x_flag_tree_vectorize)))
    return 1;

  int vs = targetm.vectorize.autovectorize_vector_sizes ();
  if (vs)
    {
      vs = 1 << floor_log2 (vs);
      return vs;
    }
  machine_mode vqimode = targetm.vectorize.preferred_simd_mode (QImode);
  if (GET_MODE_CLASS (vqimode) == MODE_VECTOR_INT)
    return GET_MODE_NUNITS (vqimode);
  return 1;
}

/* Helper function of lower_rec_input_clauses, used for #pragma omp simd
   privatization.  */

static bool
lower_rec_simd_input_clauses (tree new_var, omp_context *ctx, int &max_vf,
			      tree &idx, tree &lane, tree &ivar, tree &lvar)
{
  if (max_vf == 0)
    {
      max_vf = omp_max_vf ();
      if (max_vf > 1)
	{
	  tree c = find_omp_clause (gimple_omp_for_clauses (ctx->stmt),
				    OMP_CLAUSE_SAFELEN);
	  if (c && TREE_CODE (OMP_CLAUSE_SAFELEN_EXPR (c)) != INTEGER_CST)
	    max_vf = 1;
	  else if (c && compare_tree_int (OMP_CLAUSE_SAFELEN_EXPR (c),
					  max_vf) == -1)
	    max_vf = tree_to_shwi (OMP_CLAUSE_SAFELEN_EXPR (c));
	}
      if (max_vf > 1)
	{
	  idx = create_tmp_var (unsigned_type_node);
	  lane = create_tmp_var (unsigned_type_node);
	}
    }
  if (max_vf == 1)
    return false;

  tree atype = build_array_type_nelts (TREE_TYPE (new_var), max_vf);
  tree avar = create_tmp_var_raw (atype);
  if (TREE_ADDRESSABLE (new_var))
    TREE_ADDRESSABLE (avar) = 1;
  DECL_ATTRIBUTES (avar)
    = tree_cons (get_identifier ("omp simd array"), NULL,
		 DECL_ATTRIBUTES (avar));
  gimple_add_tmp_var (avar);
  ivar = build4 (ARRAY_REF, TREE_TYPE (new_var), avar, idx,
		 NULL_TREE, NULL_TREE);
  lvar = build4 (ARRAY_REF, TREE_TYPE (new_var), avar, lane,
		 NULL_TREE, NULL_TREE);
  if (DECL_P (new_var))
    {
      SET_DECL_VALUE_EXPR (new_var, lvar);
      DECL_HAS_VALUE_EXPR_P (new_var) = 1;
    }
  return true;
}

/* Helper function of lower_rec_input_clauses.  For a reference
   in simd reduction, add an underlying variable it will reference.  */

static void
handle_simd_reference (location_t loc, tree new_vard, gimple_seq *ilist)
{
  tree z = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_vard)));
  if (TREE_CONSTANT (z))
    {
      z = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (new_vard)),
			      get_name (new_vard));
      gimple_add_tmp_var (z);
      TREE_ADDRESSABLE (z) = 1;
      z = build_fold_addr_expr_loc (loc, z);
      gimplify_assign (new_vard, z, ilist);
    }
}

/* Generate code to implement the input clauses, FIRSTPRIVATE and COPYIN,
   from the receiver (aka child) side and initializers for REFERENCE_TYPE
   private variables.  Initialization statements go in ILIST, while calls
   to destructors go in DLIST.  */

static void
lower_rec_input_clauses (tree clauses, gimple_seq *ilist, gimple_seq *dlist,
			 omp_context *ctx, struct omp_for_data *fd)
{
  tree c, dtor, copyin_seq, x, ptr;
  bool copyin_by_ref = false;
  bool lastprivate_firstprivate = false;
  bool reduction_omp_orig_ref = false;
  int pass;
  bool is_simd = (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
		  && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD);
  int max_vf = 0;
  tree lane = NULL_TREE, idx = NULL_TREE;
  tree ivar = NULL_TREE, lvar = NULL_TREE;
  gimple_seq llist[2] = { NULL, NULL };

  copyin_seq = NULL;

  /* Set max_vf=1 (which will later enforce safelen=1) in simd loops
     with data sharing clauses referencing variable sized vars.  That
     is unnecessarily hard to support and very unlikely to result in
     vectorized code anyway.  */
  if (is_simd)
    for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_LINEAR:
	  if (OMP_CLAUSE_LINEAR_ARRAY (c))
	    max_vf = 1;
	  /* FALLTHRU */
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LASTPRIVATE:
	  if (is_variable_sized (OMP_CLAUSE_DECL (c)))
	    max_vf = 1;
	  break;
	case OMP_CLAUSE_REDUCTION:
	  if (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF
	      || is_variable_sized (OMP_CLAUSE_DECL (c)))
	    max_vf = 1;
	  break;
	default:
	  continue;
	}

  /* Do all the fixed sized types in the first pass, and the variable sized
     types in the second pass.  This makes sure that the scalar arguments to
     the variable sized types are processed before we use them in the
     variable sized operations.  */
  for (pass = 0; pass < 2; ++pass)
    {
      for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	{
	  enum omp_clause_code c_kind = OMP_CLAUSE_CODE (c);
	  tree var, new_var;
	  bool by_ref;
	  location_t clause_loc = OMP_CLAUSE_LOCATION (c);

	  switch (c_kind)
	    {
	    case OMP_CLAUSE_PRIVATE:
	      if (OMP_CLAUSE_PRIVATE_DEBUG (c))
		continue;
	      break;
	    case OMP_CLAUSE_SHARED:
	      /* Ignore shared directives in teams construct.  */
	      if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
		continue;
	      if (maybe_lookup_decl (OMP_CLAUSE_DECL (c), ctx) == NULL)
		{
		  gcc_assert (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c)
			      || is_global_var (OMP_CLAUSE_DECL (c)));
		  continue;
		}
	    case OMP_CLAUSE_FIRSTPRIVATE:
	    case OMP_CLAUSE_COPYIN:
	      break;
	    case OMP_CLAUSE_LINEAR:
	      if (!OMP_CLAUSE_LINEAR_NO_COPYIN (c)
		  && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		lastprivate_firstprivate = true;
	      break;
	    case OMP_CLAUSE_REDUCTION:
	      if (OMP_CLAUSE_REDUCTION_OMP_ORIG_REF (c))
		reduction_omp_orig_ref = true;
	      break;
	    case OMP_CLAUSE__LOOPTEMP_:
	      /* Handle _looptemp_ clauses only on parallel/task.  */
	      if (fd)
		continue;
	      break;
	    case OMP_CLAUSE_LASTPRIVATE:
	      if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
		{
		  lastprivate_firstprivate = true;
		  if (pass != 0 || is_taskloop_ctx (ctx))
		    continue;
		}
	      /* Even without corresponding firstprivate, if
		 decl is Fortran allocatable, it needs outer var
		 reference.  */
	      else if (pass == 0
		       && lang_hooks.decls.omp_private_outer_ref
							(OMP_CLAUSE_DECL (c)))
		lastprivate_firstprivate = true;
	      break;
	    case OMP_CLAUSE_ALIGNED:
	      if (pass == 0)
		continue;
	      var = OMP_CLAUSE_DECL (c);
	      if (TREE_CODE (TREE_TYPE (var)) == POINTER_TYPE
		  && !is_global_var (var))
		{
		  new_var = maybe_lookup_decl (var, ctx);
		  if (new_var == NULL_TREE)
		    new_var = maybe_lookup_decl_in_outer_ctx (var, ctx);
		  x = builtin_decl_explicit (BUILT_IN_ASSUME_ALIGNED);
		  tree alarg = omp_clause_aligned_alignment (c);
		  alarg = fold_convert_loc (clause_loc, size_type_node, alarg);
		  x = build_call_expr_loc (clause_loc, x, 2, new_var, alarg);
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		  x = build2 (MODIFY_EXPR, TREE_TYPE (new_var), new_var, x);
		  gimplify_and_add (x, ilist);
		}
	      else if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE
		       && is_global_var (var))
		{
		  tree ptype = build_pointer_type (TREE_TYPE (var)), t, t2;
		  new_var = lookup_decl (var, ctx);
		  t = maybe_lookup_decl_in_outer_ctx (var, ctx);
		  t = build_fold_addr_expr_loc (clause_loc, t);
		  t2 = builtin_decl_explicit (BUILT_IN_ASSUME_ALIGNED);
		  tree alarg = omp_clause_aligned_alignment (c);
		  alarg = fold_convert_loc (clause_loc, size_type_node, alarg);
		  t = build_call_expr_loc (clause_loc, t2, 2, t, alarg);
		  t = fold_convert_loc (clause_loc, ptype, t);
		  x = create_tmp_var (ptype);
		  t = build2 (MODIFY_EXPR, ptype, x, t);
		  gimplify_and_add (t, ilist);
		  t = build_simple_mem_ref_loc (clause_loc, x);
		  SET_DECL_VALUE_EXPR (new_var, t);
		  DECL_HAS_VALUE_EXPR_P (new_var) = 1;
		}
	      continue;
	    default:
	      continue;
	    }

	  new_var = var = OMP_CLAUSE_DECL (c);
	  if (c_kind == OMP_CLAUSE_REDUCTION && TREE_CODE (var) == MEM_REF)
	    {
	      var = TREE_OPERAND (var, 0);
	      if (TREE_CODE (var) == POINTER_PLUS_EXPR)
		var = TREE_OPERAND (var, 0);
	      if (TREE_CODE (var) == INDIRECT_REF
		  || TREE_CODE (var) == ADDR_EXPR)
		var = TREE_OPERAND (var, 0);
	      if (is_variable_sized (var))
		{
		  gcc_assert (DECL_HAS_VALUE_EXPR_P (var));
		  var = DECL_VALUE_EXPR (var);
		  gcc_assert (TREE_CODE (var) == INDIRECT_REF);
		  var = TREE_OPERAND (var, 0);
		  gcc_assert (DECL_P (var));
		}
	      new_var = var;
	    }
	  if (c_kind != OMP_CLAUSE_COPYIN)
	    new_var = lookup_decl (var, ctx);

	  if (c_kind == OMP_CLAUSE_SHARED || c_kind == OMP_CLAUSE_COPYIN)
	    {
	      if (pass != 0)
		continue;
	    }
	  /* C/C++ array section reductions.  */
	  else if (c_kind == OMP_CLAUSE_REDUCTION
		   && var != OMP_CLAUSE_DECL (c))
	    {
	      if (pass == 0)
		continue;

	      tree bias = TREE_OPERAND (OMP_CLAUSE_DECL (c), 1);
	      tree orig_var = TREE_OPERAND (OMP_CLAUSE_DECL (c), 0);
	      if (TREE_CODE (orig_var) == POINTER_PLUS_EXPR)
		{
		  tree b = TREE_OPERAND (orig_var, 1);
		  b = maybe_lookup_decl (b, ctx);
		  if (b == NULL)
		    {
		      b = TREE_OPERAND (orig_var, 1);
		      b = maybe_lookup_decl_in_outer_ctx (b, ctx);
		    }
		  if (integer_zerop (bias))
		    bias = b;
		  else
		    {
		      bias = fold_convert_loc (clause_loc,
					       TREE_TYPE (b), bias);
		      bias = fold_build2_loc (clause_loc, PLUS_EXPR,
					      TREE_TYPE (b), b, bias);
		    }
		  orig_var = TREE_OPERAND (orig_var, 0);
		}
	      if (TREE_CODE (orig_var) == INDIRECT_REF
		  || TREE_CODE (orig_var) == ADDR_EXPR)
		orig_var = TREE_OPERAND (orig_var, 0);
	      tree d = OMP_CLAUSE_DECL (c);
	      tree type = TREE_TYPE (d);
	      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
	      tree v = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	      const char *name = get_name (orig_var);
	      if (TREE_CONSTANT (v))
		{
		  x = create_tmp_var_raw (type, name);
		  gimple_add_tmp_var (x);
		  TREE_ADDRESSABLE (x) = 1;
		  x = build_fold_addr_expr_loc (clause_loc, x);
		}
	      else
		{
		  tree atmp
		    = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		  tree t = maybe_lookup_decl (v, ctx);
		  if (t)
		    v = t;
		  else
		    v = maybe_lookup_decl_in_outer_ctx (v, ctx);
		  gimplify_expr (&v, ilist, NULL, is_gimple_val, fb_rvalue);
		  t = fold_build2_loc (clause_loc, PLUS_EXPR,
				       TREE_TYPE (v), v,
				       build_int_cst (TREE_TYPE (v), 1));
		  t = fold_build2_loc (clause_loc, MULT_EXPR,
				       TREE_TYPE (v), t,
				       TYPE_SIZE_UNIT (TREE_TYPE (type)));
		  tree al = size_int (TYPE_ALIGN (TREE_TYPE (type)));
		  x = build_call_expr_loc (clause_loc, atmp, 2, t, al);
		}

	      tree ptype = build_pointer_type (TREE_TYPE (type));
	      x = fold_convert_loc (clause_loc, ptype, x);
	      tree y = create_tmp_var (ptype, name);
	      gimplify_assign (y, x, ilist);
	      x = y;
	      tree yb = y;

	      if (!integer_zerop (bias))
		{
		  bias = fold_convert_loc (clause_loc, pointer_sized_int_node,
					   bias);
		  yb = fold_convert_loc (clause_loc, pointer_sized_int_node,
					 x);
		  yb = fold_build2_loc (clause_loc, MINUS_EXPR,
					pointer_sized_int_node, yb, bias);
		  x = fold_convert_loc (clause_loc, TREE_TYPE (x), yb);
		  yb = create_tmp_var (ptype, name);
		  gimplify_assign (yb, x, ilist);
		  x = yb;
		}

	      d = TREE_OPERAND (d, 0);
	      if (TREE_CODE (d) == POINTER_PLUS_EXPR)
		d = TREE_OPERAND (d, 0);
	      if (TREE_CODE (d) == ADDR_EXPR)
		{
		  if (orig_var != var)
		    {
		      gcc_assert (is_variable_sized (orig_var));
		      x = fold_convert_loc (clause_loc, TREE_TYPE (new_var),
					    x);
		      gimplify_assign (new_var, x, ilist);
		      tree new_orig_var = lookup_decl (orig_var, ctx);
		      tree t = build_fold_indirect_ref (new_var);
		      DECL_IGNORED_P (new_var) = 0;
		      TREE_THIS_NOTRAP (t);
		      SET_DECL_VALUE_EXPR (new_orig_var, t);
		      DECL_HAS_VALUE_EXPR_P (new_orig_var) = 1;
		    }
		  else
		    {
		      x = build2 (MEM_REF, TREE_TYPE (new_var), x,
				  build_int_cst (ptype, 0));
		      SET_DECL_VALUE_EXPR (new_var, x);
		      DECL_HAS_VALUE_EXPR_P (new_var) = 1;
		    }
		}
	      else
		{
		  gcc_assert (orig_var == var);
		  if (TREE_CODE (d) == INDIRECT_REF)
		    {
		      x = create_tmp_var (ptype, name);
		      TREE_ADDRESSABLE (x) = 1;
		      gimplify_assign (x, yb, ilist);
		      x = build_fold_addr_expr_loc (clause_loc, x);
		    }
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		  gimplify_assign (new_var, x, ilist);
		}
	      tree y1 = create_tmp_var (ptype, NULL);
	      gimplify_assign (y1, y, ilist);
	      tree i2 = NULL_TREE, y2 = NULL_TREE;
	      tree body2 = NULL_TREE, end2 = NULL_TREE;
	      tree y3 = NULL_TREE, y4 = NULL_TREE;
	      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) || is_simd)
		{
		  y2 = create_tmp_var (ptype, NULL);
		  gimplify_assign (y2, y, ilist);
		  tree ref = build_outer_var_ref (var, ctx);
		  /* For ref build_outer_var_ref already performs this.  */
		  if (TREE_CODE (d) == INDIRECT_REF)
		    gcc_assert (is_reference (var));
		  else if (TREE_CODE (d) == ADDR_EXPR)
		    ref = build_fold_addr_expr (ref);
		  else if (is_reference (var))
		    ref = build_fold_addr_expr (ref);
		  ref = fold_convert_loc (clause_loc, ptype, ref);
		  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		      && OMP_CLAUSE_REDUCTION_OMP_ORIG_REF (c))
		    {
		      y3 = create_tmp_var (ptype, NULL);
		      gimplify_assign (y3, unshare_expr (ref), ilist);
		    }
		  if (is_simd)
		    {
		      y4 = create_tmp_var (ptype, NULL);
		      gimplify_assign (y4, ref, dlist);
		    }
		}
	      tree i = create_tmp_var (TREE_TYPE (v), NULL);
	      gimplify_assign (i, build_int_cst (TREE_TYPE (v), 0), ilist);
	      tree body = create_artificial_label (UNKNOWN_LOCATION);
	      tree end = create_artificial_label (UNKNOWN_LOCATION);
	      gimple_seq_add_stmt (ilist, gimple_build_label (body));
	      if (y2)
		{
		  i2 = create_tmp_var (TREE_TYPE (v), NULL);
		  gimplify_assign (i2, build_int_cst (TREE_TYPE (v), 0), dlist);
		  body2 = create_artificial_label (UNKNOWN_LOCATION);
		  end2 = create_artificial_label (UNKNOWN_LOCATION);
		  gimple_seq_add_stmt (dlist, gimple_build_label (body2));
		}
	      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
		{
		  tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);
		  tree decl_placeholder
		    = OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c);
		  SET_DECL_VALUE_EXPR (decl_placeholder,
				       build_simple_mem_ref (y1));
		  DECL_HAS_VALUE_EXPR_P (decl_placeholder) = 1;
		  SET_DECL_VALUE_EXPR (placeholder,
				       y3 ? build_simple_mem_ref (y3)
				       : error_mark_node);
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
		  x = lang_hooks.decls.omp_clause_default_ctor
				(c, build_simple_mem_ref (y1),
				 y3 ? build_simple_mem_ref (y3) : NULL_TREE);
		  if (x)
		    gimplify_and_add (x, ilist);
		  if (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c))
		    {
		      gimple_seq tseq = OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (ilist, tseq);
		    }
		  OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
		  if (is_simd)
		    {
		      SET_DECL_VALUE_EXPR (decl_placeholder,
					   build_simple_mem_ref (y2));
		      SET_DECL_VALUE_EXPR (placeholder,
					   build_simple_mem_ref (y4));
		      gimple_seq tseq = OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (dlist, tseq);
		      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
		    }
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 0;
		  DECL_HAS_VALUE_EXPR_P (decl_placeholder) = 0;
		  x = lang_hooks.decls.omp_clause_dtor
					(c, build_simple_mem_ref (y2));
		  if (x)
		    {
		      gimple_seq tseq = NULL;
		      dtor = x;
		      gimplify_stmt (&dtor, &tseq);
		      gimple_seq_add_seq (dlist, tseq);
		    }
		}
	      else
		{
		  x = omp_reduction_init (c, TREE_TYPE (type));
		  enum tree_code code = OMP_CLAUSE_REDUCTION_CODE (c);

		  /* reduction(-:var) sums up the partial results, so it
		     acts identically to reduction(+:var).  */
		  if (code == MINUS_EXPR)
		    code = PLUS_EXPR;

		  gimplify_assign (build_simple_mem_ref (y1), x, ilist);
		  if (is_simd)
		    {
		      x = build2 (code, TREE_TYPE (type),
				  build_simple_mem_ref (y4),
				  build_simple_mem_ref (y2));
		      gimplify_assign (build_simple_mem_ref (y4), x, dlist);
		    }
		}
	      gimple *g
		= gimple_build_assign (y1, POINTER_PLUS_EXPR, y1,
				       TYPE_SIZE_UNIT (TREE_TYPE (type)));
	      gimple_seq_add_stmt (ilist, g);
	      if (y3)
		{
		  g = gimple_build_assign (y3, POINTER_PLUS_EXPR, y3,
					   TYPE_SIZE_UNIT (TREE_TYPE (type)));
		  gimple_seq_add_stmt (ilist, g);
		}
	      g = gimple_build_assign (i, PLUS_EXPR, i,
				       build_int_cst (TREE_TYPE (i), 1));
	      gimple_seq_add_stmt (ilist, g);
	      g = gimple_build_cond (LE_EXPR, i, v, body, end);
	      gimple_seq_add_stmt (ilist, g);
	      gimple_seq_add_stmt (ilist, gimple_build_label (end));
	      if (y2)
		{
		  g = gimple_build_assign (y2, POINTER_PLUS_EXPR, y2,
					   TYPE_SIZE_UNIT (TREE_TYPE (type)));
		  gimple_seq_add_stmt (dlist, g);
		  if (y4)
		    {
		      g = gimple_build_assign
					(y4, POINTER_PLUS_EXPR, y4,
					 TYPE_SIZE_UNIT (TREE_TYPE (type)));
		      gimple_seq_add_stmt (dlist, g);
		    }
		  g = gimple_build_assign (i2, PLUS_EXPR, i2,
					   build_int_cst (TREE_TYPE (i2), 1));
		  gimple_seq_add_stmt (dlist, g);
		  g = gimple_build_cond (LE_EXPR, i2, v, body2, end2);
		  gimple_seq_add_stmt (dlist, g);
		  gimple_seq_add_stmt (dlist, gimple_build_label (end2));
		}
	      continue;
	    }
	  else if (is_variable_sized (var))
	    {
	      /* For variable sized types, we need to allocate the
		 actual storage here.  Call alloca and store the
		 result in the pointer decl that we created elsewhere.  */
	      if (pass == 0)
		continue;

	      if (c_kind != OMP_CLAUSE_FIRSTPRIVATE || !is_task_ctx (ctx))
		{
		  gcall *stmt;
		  tree tmp, atmp;

		  ptr = DECL_VALUE_EXPR (new_var);
		  gcc_assert (TREE_CODE (ptr) == INDIRECT_REF);
		  ptr = TREE_OPERAND (ptr, 0);
		  gcc_assert (DECL_P (ptr));
		  x = TYPE_SIZE_UNIT (TREE_TYPE (new_var));

		  /* void *tmp = __builtin_alloca */
		  atmp = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		  stmt = gimple_build_call (atmp, 2, x,
					    size_int (DECL_ALIGN (var)));
		  tmp = create_tmp_var_raw (ptr_type_node);
		  gimple_add_tmp_var (tmp);
		  gimple_call_set_lhs (stmt, tmp);

		  gimple_seq_add_stmt (ilist, stmt);

		  x = fold_convert_loc (clause_loc, TREE_TYPE (ptr), tmp);
		  gimplify_assign (ptr, x, ilist);
		}
	    }
	  else if (is_reference (var))
	    {
	      /* For references that are being privatized for Fortran,
		 allocate new backing storage for the new pointer
		 variable.  This allows us to avoid changing all the
		 code that expects a pointer to something that expects
		 a direct variable.  */
	      if (pass == 0)
		continue;

	      x = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_var)));
	      if (c_kind == OMP_CLAUSE_FIRSTPRIVATE && is_task_ctx (ctx))
		{
		  x = build_receiver_ref (var, false, ctx);
		  x = build_fold_addr_expr_loc (clause_loc, x);
		}
	      else if (TREE_CONSTANT (x))
		{
		  /* For reduction in SIMD loop, defer adding the
		     initialization of the reference, because if we decide
		     to use SIMD array for it, the initilization could cause
		     expansion ICE.  */
		  if (c_kind == OMP_CLAUSE_REDUCTION && is_simd)
		    x = NULL_TREE;
		  else
		    {
		      x = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (new_var)),
					      get_name (var));
		      gimple_add_tmp_var (x);
		      TREE_ADDRESSABLE (x) = 1;
		      x = build_fold_addr_expr_loc (clause_loc, x);
		    }
		}
	      else
		{
		  tree atmp
		    = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		  tree rtype = TREE_TYPE (TREE_TYPE (new_var));
		  tree al = size_int (TYPE_ALIGN (rtype));
		  x = build_call_expr_loc (clause_loc, atmp, 2, x, al);
		}

	      if (x)
		{
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		  gimplify_assign (new_var, x, ilist);
		}

	      new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	    }
	  else if (c_kind == OMP_CLAUSE_REDUCTION
		   && OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    {
	      if (pass == 0)
		continue;
	    }
	  else if (pass != 0)
	    continue;

	  switch (OMP_CLAUSE_CODE (c))
	    {
	    case OMP_CLAUSE_SHARED:
	      /* Ignore shared directives in teams construct.  */
	      if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
		continue;
	      /* Shared global vars are just accessed directly.  */
	      if (is_global_var (new_var))
		break;
	      /* For taskloop firstprivate/lastprivate, represented
		 as firstprivate and shared clause on the task, new_var
		 is the firstprivate var.  */
	      if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
		break;
	      /* Set up the DECL_VALUE_EXPR for shared variables now.  This
		 needs to be delayed until after fixup_child_record_type so
		 that we get the correct type during the dereference.  */
	      by_ref = use_pointer_for_field (var, ctx);
	      x = build_receiver_ref (var, by_ref, ctx);
	      SET_DECL_VALUE_EXPR (new_var, x);
	      DECL_HAS_VALUE_EXPR_P (new_var) = 1;

	      /* ??? If VAR is not passed by reference, and the variable
		 hasn't been initialized yet, then we'll get a warning for
		 the store into the omp_data_s structure.  Ideally, we'd be
		 able to notice this and not store anything at all, but
		 we're generating code too early.  Suppress the warning.  */
	      if (!by_ref)
		TREE_NO_WARNING (var) = 1;
	      break;

	    case OMP_CLAUSE_LASTPRIVATE:
	      if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
		break;
	      /* FALLTHRU */

	    case OMP_CLAUSE_PRIVATE:
	      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_PRIVATE)
		x = build_outer_var_ref (var, ctx);
	      else if (OMP_CLAUSE_PRIVATE_OUTER_REF (c))
		{
		  if (is_task_ctx (ctx))
		    x = build_receiver_ref (var, false, ctx);
		  else
		    x = build_outer_var_ref (var, ctx);
		}
	      else
		x = NULL;
	    do_private:
	      tree nx;
	      nx = lang_hooks.decls.omp_clause_default_ctor
						(c, unshare_expr (new_var), x);
	      if (is_simd)
		{
		  tree y = lang_hooks.decls.omp_clause_dtor (c, new_var);
		  if ((TREE_ADDRESSABLE (new_var) || nx || y
		       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
		      && lower_rec_simd_input_clauses (new_var, ctx, max_vf,
						       idx, lane, ivar, lvar))
		    {
		      if (nx)
			x = lang_hooks.decls.omp_clause_default_ctor
						(c, unshare_expr (ivar), x);
		      if (nx && x)
			gimplify_and_add (x, &llist[0]);
		      if (y)
			{
			  y = lang_hooks.decls.omp_clause_dtor (c, ivar);
			  if (y)
			    {
			      gimple_seq tseq = NULL;

			      dtor = y;
			      gimplify_stmt (&dtor, &tseq);
			      gimple_seq_add_seq (&llist[1], tseq);
			    }
			}
		      break;
		    }
		}
	      if (nx)
		gimplify_and_add (nx, ilist);
	      /* FALLTHRU */

	    do_dtor:
	      x = lang_hooks.decls.omp_clause_dtor (c, new_var);
	      if (x)
		{
		  gimple_seq tseq = NULL;

		  dtor = x;
		  gimplify_stmt (&dtor, &tseq);
		  gimple_seq_add_seq (dlist, tseq);
		}
	      break;

	    case OMP_CLAUSE_LINEAR:
	      if (!OMP_CLAUSE_LINEAR_NO_COPYIN (c))
		goto do_firstprivate;
	      if (OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		x = NULL;
	      else
		x = build_outer_var_ref (var, ctx);
	      goto do_private;

	    case OMP_CLAUSE_FIRSTPRIVATE:
	      if (is_task_ctx (ctx))
		{
		  if (is_reference (var) || is_variable_sized (var))
		    goto do_dtor;
		  else if (is_global_var (maybe_lookup_decl_in_outer_ctx (var,
									  ctx))
			   || use_pointer_for_field (var, NULL))
		    {
		      x = build_receiver_ref (var, false, ctx);
		      SET_DECL_VALUE_EXPR (new_var, x);
		      DECL_HAS_VALUE_EXPR_P (new_var) = 1;
		      goto do_dtor;
		    }
		}
	    do_firstprivate:
	      x = build_outer_var_ref (var, ctx);
	      if (is_simd)
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		      && gimple_omp_for_combined_into_p (ctx->stmt))
		    {
		      tree t = OMP_CLAUSE_LINEAR_STEP (c);
		      tree stept = TREE_TYPE (t);
		      tree ct = find_omp_clause (clauses,
						 OMP_CLAUSE__LOOPTEMP_);
		      gcc_assert (ct);
		      tree l = OMP_CLAUSE_DECL (ct);
		      tree n1 = fd->loop.n1;
		      tree step = fd->loop.step;
		      tree itype = TREE_TYPE (l);
		      if (POINTER_TYPE_P (itype))
			itype = signed_type_for (itype);
		      l = fold_build2 (MINUS_EXPR, itype, l, n1);
		      if (TYPE_UNSIGNED (itype)
			  && fd->loop.cond_code == GT_EXPR)
			l = fold_build2 (TRUNC_DIV_EXPR, itype,
					 fold_build1 (NEGATE_EXPR, itype, l),
					 fold_build1 (NEGATE_EXPR,
						      itype, step));
		      else
			l = fold_build2 (TRUNC_DIV_EXPR, itype, l, step);
		      t = fold_build2 (MULT_EXPR, stept,
				       fold_convert (stept, l), t);

		      if (OMP_CLAUSE_LINEAR_ARRAY (c))
			{
			  x = lang_hooks.decls.omp_clause_linear_ctor
							(c, new_var, x, t);
			  gimplify_and_add (x, ilist);
			  goto do_dtor;
			}

		      if (POINTER_TYPE_P (TREE_TYPE (x)))
			x = fold_build2 (POINTER_PLUS_EXPR,
					 TREE_TYPE (x), x, t);
		      else
			x = fold_build2 (PLUS_EXPR, TREE_TYPE (x), x, t);
		    }

		  if ((OMP_CLAUSE_CODE (c) != OMP_CLAUSE_LINEAR
		       || TREE_ADDRESSABLE (new_var))
		      && lower_rec_simd_input_clauses (new_var, ctx, max_vf,
						       idx, lane, ivar, lvar))
		    {
		      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR)
			{
			  tree iv = create_tmp_var (TREE_TYPE (new_var));
			  x = lang_hooks.decls.omp_clause_copy_ctor (c, iv, x);
			  gimplify_and_add (x, ilist);
			  gimple_stmt_iterator gsi
			    = gsi_start_1 (gimple_omp_body_ptr (ctx->stmt));
			  gassign *g
			    = gimple_build_assign (unshare_expr (lvar), iv);
			  gsi_insert_before_without_update (&gsi, g,
							    GSI_SAME_STMT);
			  tree t = OMP_CLAUSE_LINEAR_STEP (c);
			  enum tree_code code = PLUS_EXPR;
			  if (POINTER_TYPE_P (TREE_TYPE (new_var)))
			    code = POINTER_PLUS_EXPR;
			  g = gimple_build_assign (iv, code, iv, t);
			  gsi_insert_before_without_update (&gsi, g,
							    GSI_SAME_STMT);
			  break;
			}
		      x = lang_hooks.decls.omp_clause_copy_ctor
						(c, unshare_expr (ivar), x);
		      gimplify_and_add (x, &llist[0]);
		      x = lang_hooks.decls.omp_clause_dtor (c, ivar);
		      if (x)
			{
			  gimple_seq tseq = NULL;

			  dtor = x;
			  gimplify_stmt (&dtor, &tseq);
			  gimple_seq_add_seq (&llist[1], tseq);
			}
		      break;
		    }
		}
	      x = lang_hooks.decls.omp_clause_copy_ctor
						(c, unshare_expr (new_var), x);
	      gimplify_and_add (x, ilist);
	      goto do_dtor;

	    case OMP_CLAUSE__LOOPTEMP_:
	      gcc_assert (is_taskreg_ctx (ctx));
	      x = build_outer_var_ref (var, ctx);
	      x = build2 (MODIFY_EXPR, TREE_TYPE (new_var), new_var, x);
	      gimplify_and_add (x, ilist);
	      break;

	    case OMP_CLAUSE_COPYIN:
	      by_ref = use_pointer_for_field (var, NULL);
	      x = build_receiver_ref (var, by_ref, ctx);
	      x = lang_hooks.decls.omp_clause_assign_op (c, new_var, x);
	      append_to_statement_list (x, &copyin_seq);
	      copyin_by_ref |= by_ref;
	      break;

	    case OMP_CLAUSE_REDUCTION:
	      /* OpenACC reductions are initialized using the
		 GOACC_REDUCTION internal function.  */
	      if (is_gimple_omp_oacc (ctx->stmt))
		break;
	      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
		{
		  tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);
		  gimple *tseq;
		  x = build_outer_var_ref (var, ctx);

		  if (is_reference (var)
		      && !useless_type_conversion_p (TREE_TYPE (placeholder),
						     TREE_TYPE (x)))
		    x = build_fold_addr_expr_loc (clause_loc, x);
		  SET_DECL_VALUE_EXPR (placeholder, x);
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
		  tree new_vard = new_var;
		  if (is_reference (var))
		    {
		      gcc_assert (TREE_CODE (new_var) == MEM_REF);
		      new_vard = TREE_OPERAND (new_var, 0);
		      gcc_assert (DECL_P (new_vard));
		    }
		  if (is_simd
		      && lower_rec_simd_input_clauses (new_var, ctx, max_vf,
						       idx, lane, ivar, lvar))
		    {
		      if (new_vard == new_var)
			{
			  gcc_assert (DECL_VALUE_EXPR (new_var) == lvar);
			  SET_DECL_VALUE_EXPR (new_var, ivar);
			}
		      else
			{
			  SET_DECL_VALUE_EXPR (new_vard,
					       build_fold_addr_expr (ivar));
			  DECL_HAS_VALUE_EXPR_P (new_vard) = 1;
			}
		      x = lang_hooks.decls.omp_clause_default_ctor
				(c, unshare_expr (ivar),
				 build_outer_var_ref (var, ctx));
		      if (x)
			gimplify_and_add (x, &llist[0]);
		      if (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c))
			{
			  tseq = OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c);
			  lower_omp (&tseq, ctx);
			  gimple_seq_add_seq (&llist[0], tseq);
			}
		      OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
		      tseq = OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (&llist[1], tseq);
		      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
		      DECL_HAS_VALUE_EXPR_P (placeholder) = 0;
		      if (new_vard == new_var)
			SET_DECL_VALUE_EXPR (new_var, lvar);
		      else
			SET_DECL_VALUE_EXPR (new_vard,
					     build_fold_addr_expr (lvar));
		      x = lang_hooks.decls.omp_clause_dtor (c, ivar);
		      if (x)
			{
			  tseq = NULL;
			  dtor = x;
			  gimplify_stmt (&dtor, &tseq);
			  gimple_seq_add_seq (&llist[1], tseq);
			}
		      break;
		    }
		  /* If this is a reference to constant size reduction var
		     with placeholder, we haven't emitted the initializer
		     for it because it is undesirable if SIMD arrays are used.
		     But if they aren't used, we need to emit the deferred
		     initialization now.  */
		  else if (is_reference (var) && is_simd)
		    handle_simd_reference (clause_loc, new_vard, ilist);
		  x = lang_hooks.decls.omp_clause_default_ctor
				(c, unshare_expr (new_var),
				 build_outer_var_ref (var, ctx));
		  if (x)
		    gimplify_and_add (x, ilist);
		  if (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c))
		    {
		      tseq = OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (ilist, tseq);
		    }
		  OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
		  if (is_simd)
		    {
		      tseq = OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (dlist, tseq);
		      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
		    }
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 0;
		  goto do_dtor;
		}
	      else
		{
		  x = omp_reduction_init (c, TREE_TYPE (new_var));
		  gcc_assert (TREE_CODE (TREE_TYPE (new_var)) != ARRAY_TYPE);
		  enum tree_code code = OMP_CLAUSE_REDUCTION_CODE (c);

		  /* reduction(-:var) sums up the partial results, so it
		     acts identically to reduction(+:var).  */
		  if (code == MINUS_EXPR)
		    code = PLUS_EXPR;

		  tree new_vard = new_var;
		  if (is_simd && is_reference (var))
		    {
		      gcc_assert (TREE_CODE (new_var) == MEM_REF);
		      new_vard = TREE_OPERAND (new_var, 0);
		      gcc_assert (DECL_P (new_vard));
		    }
		  if (is_simd
		      && lower_rec_simd_input_clauses (new_var, ctx, max_vf,
						       idx, lane, ivar, lvar))
		    {
		      tree ref = build_outer_var_ref (var, ctx);

		      gimplify_assign (unshare_expr (ivar), x, &llist[0]);

		      x = build2 (code, TREE_TYPE (ref), ref, ivar);
		      ref = build_outer_var_ref (var, ctx);
		      gimplify_assign (ref, x, &llist[1]);

		      if (new_vard != new_var)
			{
			  SET_DECL_VALUE_EXPR (new_vard,
					       build_fold_addr_expr (lvar));
			  DECL_HAS_VALUE_EXPR_P (new_vard) = 1;
			}
		    }
		  else
		    {
		      if (is_reference (var) && is_simd)
			handle_simd_reference (clause_loc, new_vard, ilist);
		      gimplify_assign (new_var, x, ilist);
		      if (is_simd)
			{
			  tree ref = build_outer_var_ref (var, ctx);

			  x = build2 (code, TREE_TYPE (ref), ref, new_var);
			  ref = build_outer_var_ref (var, ctx);
			  gimplify_assign (ref, x, dlist);
			}
		    }
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  if (lane)
    {
      tree uid = create_tmp_var (ptr_type_node, "simduid");
      /* Don't want uninit warnings on simduid, it is always uninitialized,
	 but we use it not for the value, but for the DECL_UID only.  */
      TREE_NO_WARNING (uid) = 1;
      gimple *g
	= gimple_build_call_internal (IFN_GOMP_SIMD_LANE, 1, uid);
      gimple_call_set_lhs (g, lane);
      gimple_stmt_iterator gsi = gsi_start_1 (gimple_omp_body_ptr (ctx->stmt));
      gsi_insert_before_without_update (&gsi, g, GSI_SAME_STMT);
      c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__SIMDUID_);
      OMP_CLAUSE__SIMDUID__DECL (c) = uid;
      OMP_CLAUSE_CHAIN (c) = gimple_omp_for_clauses (ctx->stmt);
      gimple_omp_for_set_clauses (ctx->stmt, c);
      g = gimple_build_assign (lane, INTEGER_CST,
			       build_int_cst (unsigned_type_node, 0));
      gimple_seq_add_stmt (ilist, g);
      for (int i = 0; i < 2; i++)
	if (llist[i])
	  {
	    tree vf = create_tmp_var (unsigned_type_node);
	    g = gimple_build_call_internal (IFN_GOMP_SIMD_VF, 1, uid);
	    gimple_call_set_lhs (g, vf);
	    gimple_seq *seq = i == 0 ? ilist : dlist;
	    gimple_seq_add_stmt (seq, g);
	    tree t = build_int_cst (unsigned_type_node, 0);
	    g = gimple_build_assign (idx, INTEGER_CST, t);
	    gimple_seq_add_stmt (seq, g);
	    tree body = create_artificial_label (UNKNOWN_LOCATION);
	    tree header = create_artificial_label (UNKNOWN_LOCATION);
	    tree end = create_artificial_label (UNKNOWN_LOCATION);
	    gimple_seq_add_stmt (seq, gimple_build_goto (header));
	    gimple_seq_add_stmt (seq, gimple_build_label (body));
	    gimple_seq_add_seq (seq, llist[i]);
	    t = build_int_cst (unsigned_type_node, 1);
	    g = gimple_build_assign (idx, PLUS_EXPR, idx, t);
	    gimple_seq_add_stmt (seq, g);
	    gimple_seq_add_stmt (seq, gimple_build_label (header));
	    g = gimple_build_cond (LT_EXPR, idx, vf, body, end);
	    gimple_seq_add_stmt (seq, g);
	    gimple_seq_add_stmt (seq, gimple_build_label (end));
	  }
    }

  /* The copyin sequence is not to be executed by the main thread, since
     that would result in self-copies.  Perhaps not visible to scalars,
     but it certainly is to C++ operator=.  */
  if (copyin_seq)
    {
      x = build_call_expr (builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM),
			   0);
      x = build2 (NE_EXPR, boolean_type_node, x,
		  build_int_cst (TREE_TYPE (x), 0));
      x = build3 (COND_EXPR, void_type_node, x, copyin_seq, NULL);
      gimplify_and_add (x, ilist);
    }

  /* If any copyin variable is passed by reference, we must ensure the
     master thread doesn't modify it before it is copied over in all
     threads.  Similarly for variables in both firstprivate and
     lastprivate clauses we need to ensure the lastprivate copying
     happens after firstprivate copying in all threads.  And similarly
     for UDRs if initializer expression refers to omp_orig.  */
  if (copyin_by_ref || lastprivate_firstprivate || reduction_omp_orig_ref)
    {
      /* Don't add any barrier for #pragma omp simd or
	 #pragma omp distribute.  */
      if (gimple_code (ctx->stmt) != GIMPLE_OMP_FOR
	  || gimple_omp_for_kind (ctx->stmt) == GF_OMP_FOR_KIND_FOR)
	gimple_seq_add_stmt (ilist, build_omp_barrier (NULL_TREE));
    }

  /* If max_vf is non-zero, then we can use only a vectorization factor
     up to the max_vf we chose.  So stick it into the safelen clause.  */
  if (max_vf)
    {
      tree c = find_omp_clause (gimple_omp_for_clauses (ctx->stmt),
				OMP_CLAUSE_SAFELEN);
      if (c == NULL_TREE
	  || (TREE_CODE (OMP_CLAUSE_SAFELEN_EXPR (c)) == INTEGER_CST
	      && compare_tree_int (OMP_CLAUSE_SAFELEN_EXPR (c),
				   max_vf) == 1))
	{
	  c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_SAFELEN);
	  OMP_CLAUSE_SAFELEN_EXPR (c) = build_int_cst (integer_type_node,
						       max_vf);
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_for_clauses (ctx->stmt);
	  gimple_omp_for_set_clauses (ctx->stmt, c);
	}
    }
}


/* Generate code to implement the LASTPRIVATE clauses.  This is used for
   both parallel and workshare constructs.  PREDICATE may be NULL if it's
   always true.   */

static void
lower_lastprivate_clauses (tree clauses, tree predicate, gimple_seq *stmt_list,
			   omp_context *ctx)
{
  tree x, c, label = NULL, orig_clauses = clauses;
  bool par_clauses = false;
  tree simduid = NULL, lastlane = NULL;

  /* Early exit if there are no lastprivate or linear clauses.  */
  for (; clauses ; clauses = OMP_CLAUSE_CHAIN (clauses))
    if (OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_LASTPRIVATE
	|| (OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_LINEAR
	    && !OMP_CLAUSE_LINEAR_NO_COPYOUT (clauses)))
      break;
  if (clauses == NULL)
    {
      /* If this was a workshare clause, see if it had been combined
	 with its parallel.  In that case, look for the clauses on the
	 parallel statement itself.  */
      if (is_parallel_ctx (ctx))
	return;

      ctx = ctx->outer;
      if (ctx == NULL || !is_parallel_ctx (ctx))
	return;

      clauses = find_omp_clause (gimple_omp_parallel_clauses (ctx->stmt),
				 OMP_CLAUSE_LASTPRIVATE);
      if (clauses == NULL)
	return;
      par_clauses = true;
    }

  if (predicate)
    {
      gcond *stmt;
      tree label_true, arm1, arm2;

      label = create_artificial_label (UNKNOWN_LOCATION);
      label_true = create_artificial_label (UNKNOWN_LOCATION);
      arm1 = TREE_OPERAND (predicate, 0);
      arm2 = TREE_OPERAND (predicate, 1);
      gimplify_expr (&arm1, stmt_list, NULL, is_gimple_val, fb_rvalue);
      gimplify_expr (&arm2, stmt_list, NULL, is_gimple_val, fb_rvalue);
      stmt = gimple_build_cond (TREE_CODE (predicate), arm1, arm2,
				label_true, label);
      gimple_seq_add_stmt (stmt_list, stmt);
      gimple_seq_add_stmt (stmt_list, gimple_build_label (label_true));
    }

  if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
      && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
    {
      simduid = find_omp_clause (orig_clauses, OMP_CLAUSE__SIMDUID_);
      if (simduid)
	simduid = OMP_CLAUSE__SIMDUID__DECL (simduid);
    }

  for (c = clauses; c ;)
    {
      tree var, new_var;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	  || (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	      && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c)))
	{
	  var = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c)
	      && is_taskloop_ctx (ctx))
	    {
	      gcc_checking_assert (ctx->outer && is_task_ctx (ctx->outer));
	      new_var = lookup_decl (var, ctx->outer);
	    }
	  else
	    new_var = lookup_decl (var, ctx);

	  if (simduid && DECL_HAS_VALUE_EXPR_P (new_var))
	    {
	      tree val = DECL_VALUE_EXPR (new_var);
	      if (TREE_CODE (val) == ARRAY_REF
		  && VAR_P (TREE_OPERAND (val, 0))
		  && lookup_attribute ("omp simd array",
				       DECL_ATTRIBUTES (TREE_OPERAND (val,
								      0))))
		{
		  if (lastlane == NULL)
		    {
		      lastlane = create_tmp_var (unsigned_type_node);
		      gcall *g
			= gimple_build_call_internal (IFN_GOMP_SIMD_LAST_LANE,
						      2, simduid,
						      TREE_OPERAND (val, 1));
		      gimple_call_set_lhs (g, lastlane);
		      gimple_seq_add_stmt (stmt_list, g);
		    }
		  new_var = build4 (ARRAY_REF, TREE_TYPE (val),
				    TREE_OPERAND (val, 0), lastlane,
				    NULL_TREE, NULL_TREE);
		}
	    }

	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c))
	    {
	      lower_omp (&OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c), ctx);
	      gimple_seq_add_seq (stmt_list,
				  OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c));
	      OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c) = NULL;
	    }
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		   && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c))
	    {
	      lower_omp (&OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c), ctx);
	      gimple_seq_add_seq (stmt_list,
				  OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c));
	      OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c) = NULL;
	    }

	  x = NULL_TREE;
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_TASKLOOP_IV (c))
	    {
	      gcc_checking_assert (is_taskloop_ctx (ctx));
	      tree ovar = maybe_lookup_decl_in_outer_ctx (var,
							  ctx->outer->outer);
	      if (is_global_var (ovar))
		x = ovar;
	    }
	  if (!x)
	    x = build_outer_var_ref (var, ctx, true);
	  if (is_reference (var))
	    new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	  x = lang_hooks.decls.omp_clause_assign_op (c, x, new_var);
	  gimplify_and_add (x, stmt_list);
	}
      c = OMP_CLAUSE_CHAIN (c);
      if (c == NULL && !par_clauses)
	{
	  /* If this was a workshare clause, see if it had been combined
	     with its parallel.  In that case, continue looking for the
	     clauses also on the parallel statement itself.  */
	  if (is_parallel_ctx (ctx))
	    break;

	  ctx = ctx->outer;
	  if (ctx == NULL || !is_parallel_ctx (ctx))
	    break;

	  c = find_omp_clause (gimple_omp_parallel_clauses (ctx->stmt),
			       OMP_CLAUSE_LASTPRIVATE);
	  par_clauses = true;
	}
    }

  if (label)
    gimple_seq_add_stmt (stmt_list, gimple_build_label (label));
}

/* Lower the OpenACC reductions of CLAUSES for compute axis LEVEL
   (which might be a placeholder).  INNER is true if this is an inner
   axis of a multi-axis loop.  FORK and JOIN are (optional) fork and
   join markers.  Generate the before-loop forking sequence in
   FORK_SEQ and the after-loop joining sequence to JOIN_SEQ.  The
   general form of these sequences is

     GOACC_REDUCTION_SETUP
     GOACC_FORK
     GOACC_REDUCTION_INIT
     ...
     GOACC_REDUCTION_FINI
     GOACC_JOIN
     GOACC_REDUCTION_TEARDOWN.  */

static void
lower_oacc_reductions (location_t loc, tree clauses, tree level, bool inner,
		       gcall *fork, gcall *join, gimple_seq *fork_seq,
		       gimple_seq *join_seq, omp_context *ctx)
{
  gimple_seq before_fork = NULL;
  gimple_seq after_fork = NULL;
  gimple_seq before_join = NULL;
  gimple_seq after_join = NULL;
  tree init_code = NULL_TREE, fini_code = NULL_TREE,
    setup_code = NULL_TREE, teardown_code = NULL_TREE;
  unsigned offset = 0;

  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION)
      {
	tree orig = OMP_CLAUSE_DECL (c);
	tree var = maybe_lookup_decl (orig, ctx);
	tree ref_to_res = NULL_TREE;
	tree incoming, outgoing, v1, v2, v3;
	bool is_private = false;

	enum tree_code rcode = OMP_CLAUSE_REDUCTION_CODE (c);
	if (rcode == MINUS_EXPR)
	  rcode = PLUS_EXPR;
	else if (rcode == TRUTH_ANDIF_EXPR)
	  rcode = BIT_AND_EXPR;
	else if (rcode == TRUTH_ORIF_EXPR)
	  rcode = BIT_IOR_EXPR;
	tree op = build_int_cst (unsigned_type_node, rcode);

	if (!var)
	  var = orig;

	incoming = outgoing = var;
	
	if (!inner)
	  {
	    /* See if an outer construct also reduces this variable.  */
	    omp_context *outer = ctx;

	    while (omp_context *probe = outer->outer)
	      {
		enum gimple_code type = gimple_code (probe->stmt);
		tree cls;

		switch (type)
		  {
		  case GIMPLE_OMP_FOR:
		    cls = gimple_omp_for_clauses (probe->stmt);
		    break;

		  case GIMPLE_OMP_TARGET:
		    if (gimple_omp_target_kind (probe->stmt)
			!= GF_OMP_TARGET_KIND_OACC_PARALLEL)
		      goto do_lookup;

		    cls = gimple_omp_target_clauses (probe->stmt);
		    break;

		  default:
		    goto do_lookup;
		  }
		
		outer = probe;
		for (; cls;  cls = OMP_CLAUSE_CHAIN (cls))
		  if (OMP_CLAUSE_CODE (cls) == OMP_CLAUSE_REDUCTION
		      && orig == OMP_CLAUSE_DECL (cls))
		    {
		      incoming = outgoing = lookup_decl (orig, probe);
		      goto has_outer_reduction;
		    }
		  else if ((OMP_CLAUSE_CODE (cls) == OMP_CLAUSE_FIRSTPRIVATE
			    || OMP_CLAUSE_CODE (cls) == OMP_CLAUSE_PRIVATE)
			   && orig == OMP_CLAUSE_DECL (cls))
		    {
		      is_private = true;
		      goto do_lookup;
		    }
	      }

	  do_lookup:
	    /* This is the outermost construct with this reduction,
	       see if there's a mapping for it.  */
	    if (gimple_code (outer->stmt) == GIMPLE_OMP_TARGET
		&& maybe_lookup_field (orig, outer) && !is_private)
	      {
		ref_to_res = build_receiver_ref (orig, false, outer);
		if (is_reference (orig))
		  ref_to_res = build_simple_mem_ref (ref_to_res);

		tree type = TREE_TYPE (var);
		if (POINTER_TYPE_P (type))
		  type = TREE_TYPE (type);

		outgoing = var;
		incoming = omp_reduction_init_op (loc, rcode, type);
	      }
	    else if (ctx->outer)
	      incoming = outgoing = lookup_decl (orig, ctx->outer);
	    else
	      incoming = outgoing = orig;
	      
	  has_outer_reduction:;
	  }

	if (!ref_to_res)
	  ref_to_res = integer_zero_node;

        if (is_reference (orig))
	  {
	    tree type = TREE_TYPE (var);
	    const char *id = IDENTIFIER_POINTER (DECL_NAME (var));

	    if (!inner)
	      {
		tree x = create_tmp_var (TREE_TYPE (type), id);
		gimplify_assign (var, build_fold_addr_expr (x), fork_seq);
	      }

	    v1 = create_tmp_var (type, id);
	    v2 = create_tmp_var (type, id);
	    v3 = create_tmp_var (type, id);

	    gimplify_assign (v1, var, fork_seq);
	    gimplify_assign (v2, var, fork_seq);
	    gimplify_assign (v3, var, fork_seq);

	    var = build_simple_mem_ref (var);
	    v1 = build_simple_mem_ref (v1);
	    v2 = build_simple_mem_ref (v2);
	    v3 = build_simple_mem_ref (v3);
	    outgoing = build_simple_mem_ref (outgoing);

	    if (!TREE_CONSTANT (incoming))
	      incoming = build_simple_mem_ref (incoming);
	  }
	else
	  v1 = v2 = v3 = var;

	/* Determine position in reduction buffer, which may be used
	   by target.  */
	enum machine_mode mode = TYPE_MODE (TREE_TYPE (var));
	unsigned align = GET_MODE_ALIGNMENT (mode) /  BITS_PER_UNIT;
	offset = (offset + align - 1) & ~(align - 1);
	tree off = build_int_cst (sizetype, offset);
	offset += GET_MODE_SIZE (mode);

	if (!init_code)
	  {
	    init_code = build_int_cst (integer_type_node,
				       IFN_GOACC_REDUCTION_INIT);
	    fini_code = build_int_cst (integer_type_node,
				       IFN_GOACC_REDUCTION_FINI);
	    setup_code = build_int_cst (integer_type_node,
					IFN_GOACC_REDUCTION_SETUP);
	    teardown_code = build_int_cst (integer_type_node,
					   IFN_GOACC_REDUCTION_TEARDOWN);
	  }

	tree setup_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, setup_code,
					  unshare_expr (ref_to_res),
					  incoming, level, op, off);
	tree init_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, init_code,
					  unshare_expr (ref_to_res),
					  v1, level, op, off);
	tree fini_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, fini_code,
					  unshare_expr (ref_to_res),
					  v2, level, op, off);
	tree teardown_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, teardown_code,
					  ref_to_res, v3, level, op, off);

	gimplify_assign (v1, setup_call, &before_fork);
	gimplify_assign (v2, init_call, &after_fork);
	gimplify_assign (v3, fini_call, &before_join);
	gimplify_assign (outgoing, teardown_call, &after_join);
      }

  /* Now stitch things together.  */
  gimple_seq_add_seq (fork_seq, before_fork);
  if (fork)
    gimple_seq_add_stmt (fork_seq, fork);
  gimple_seq_add_seq (fork_seq, after_fork);

  gimple_seq_add_seq (join_seq, before_join);
  if (join)
    gimple_seq_add_stmt (join_seq, join);
  gimple_seq_add_seq (join_seq, after_join);
}

/* Generate code to implement the REDUCTION clauses.  */

static void
lower_reduction_clauses (tree clauses, gimple_seq *stmt_seqp, omp_context *ctx)
{
  gimple_seq sub_seq = NULL;
  gimple *stmt;
  tree x, c;
  int count = 0;

  /* OpenACC loop reductions are handled elsewhere.  */
  if (is_gimple_omp_oacc (ctx->stmt))
    return;

  /* SIMD reductions are handled in lower_rec_input_clauses.  */
  if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
      && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
    return;

  /* First see if there is exactly one reduction clause.  Use OMP_ATOMIC
     update in that case, otherwise use a lock.  */
  for (c = clauses; c && count < 2; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION)
      {
	if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
	    || TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF)
	  {
	    /* Never use OMP_ATOMIC for array reductions or UDRs.  */
	    count = -1;
	    break;
	  }
	count++;
      }

  if (count == 0)
    return;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    {
      tree var, ref, new_var, orig_var;
      enum tree_code code;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION)
	continue;

      orig_var = var = OMP_CLAUSE_DECL (c);
      if (TREE_CODE (var) == MEM_REF)
	{
	  var = TREE_OPERAND (var, 0);
	  if (TREE_CODE (var) == POINTER_PLUS_EXPR)
	    var = TREE_OPERAND (var, 0);
	  if (TREE_CODE (var) == INDIRECT_REF
	      || TREE_CODE (var) == ADDR_EXPR)
	    var = TREE_OPERAND (var, 0);
	  orig_var = var;
	  if (is_variable_sized (var))
	    {
	      gcc_assert (DECL_HAS_VALUE_EXPR_P (var));
	      var = DECL_VALUE_EXPR (var);
	      gcc_assert (TREE_CODE (var) == INDIRECT_REF);
	      var = TREE_OPERAND (var, 0);
	      gcc_assert (DECL_P (var));
	    }
	}
      new_var = lookup_decl (var, ctx);
      if (var == OMP_CLAUSE_DECL (c) && is_reference (var))
	new_var = build_simple_mem_ref_loc (clause_loc, new_var);
      ref = build_outer_var_ref (var, ctx);
      code = OMP_CLAUSE_REDUCTION_CODE (c);

      /* reduction(-:var) sums up the partial results, so it acts
	 identically to reduction(+:var).  */
      if (code == MINUS_EXPR)
        code = PLUS_EXPR;

      if (count == 1)
	{
	  tree addr = build_fold_addr_expr_loc (clause_loc, ref);

	  addr = save_expr (addr);
	  ref = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (addr)), addr);
	  x = fold_build2_loc (clause_loc, code, TREE_TYPE (ref), ref, new_var);
	  x = build2 (OMP_ATOMIC, void_type_node, addr, x);
	  gimplify_and_add (x, stmt_seqp);
	  return;
	}
      else if (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF)
	{
	  tree d = OMP_CLAUSE_DECL (c);
	  tree type = TREE_TYPE (d);
	  tree v = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	  tree i = create_tmp_var (TREE_TYPE (v), NULL);
	  tree ptype = build_pointer_type (TREE_TYPE (type));
	  tree bias = TREE_OPERAND (d, 1);
	  d = TREE_OPERAND (d, 0);
	  if (TREE_CODE (d) == POINTER_PLUS_EXPR)
	    {
	      tree b = TREE_OPERAND (d, 1);
	      b = maybe_lookup_decl (b, ctx);
	      if (b == NULL)
		{
		  b = TREE_OPERAND (d, 1);
		  b = maybe_lookup_decl_in_outer_ctx (b, ctx);
		}
	      if (integer_zerop (bias))
		bias = b;
	      else
		{
		  bias = fold_convert_loc (clause_loc, TREE_TYPE (b), bias);
		  bias = fold_build2_loc (clause_loc, PLUS_EXPR,
					  TREE_TYPE (b), b, bias);
		}
	      d = TREE_OPERAND (d, 0);
	    }
	  /* For ref build_outer_var_ref already performs this, so
	     only new_var needs a dereference.  */
	  if (TREE_CODE (d) == INDIRECT_REF)
	    {
	      new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	      gcc_assert (is_reference (var) && var == orig_var);
	    }
	  else if (TREE_CODE (d) == ADDR_EXPR)
	    {
	      if (orig_var == var)
		{
		  new_var = build_fold_addr_expr (new_var);
		  ref = build_fold_addr_expr (ref);
		}
	    }
	  else
	    {
	      gcc_assert (orig_var == var);
	      if (is_reference (var))
		ref = build_fold_addr_expr (ref);
	    }
	  if (DECL_P (v))
	    {
	      tree t = maybe_lookup_decl (v, ctx);
	      if (t)
		v = t;
	      else
		v = maybe_lookup_decl_in_outer_ctx (v, ctx);
	      gimplify_expr (&v, stmt_seqp, NULL, is_gimple_val, fb_rvalue);
	    }
	  if (!integer_zerop (bias))
	    {
	      bias = fold_convert_loc (clause_loc, sizetype, bias);
	      new_var = fold_build2_loc (clause_loc, POINTER_PLUS_EXPR,
					 TREE_TYPE (new_var), new_var,
					 unshare_expr (bias));
	      ref = fold_build2_loc (clause_loc, POINTER_PLUS_EXPR,
					 TREE_TYPE (ref), ref, bias);
	    }
	  new_var = fold_convert_loc (clause_loc, ptype, new_var);
	  ref = fold_convert_loc (clause_loc, ptype, ref);
	  tree m = create_tmp_var (ptype, NULL);
	  gimplify_assign (m, new_var, stmt_seqp);
	  new_var = m;
	  m = create_tmp_var (ptype, NULL);
	  gimplify_assign (m, ref, stmt_seqp);
	  ref = m;
	  gimplify_assign (i, build_int_cst (TREE_TYPE (v), 0), stmt_seqp);
	  tree body = create_artificial_label (UNKNOWN_LOCATION);
	  tree end = create_artificial_label (UNKNOWN_LOCATION);
	  gimple_seq_add_stmt (&sub_seq, gimple_build_label (body));
	  tree priv = build_simple_mem_ref_loc (clause_loc, new_var);
	  tree out = build_simple_mem_ref_loc (clause_loc, ref);
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    {
	      tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);
	      tree decl_placeholder
		= OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c);
	      SET_DECL_VALUE_EXPR (placeholder, out);
	      DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
	      SET_DECL_VALUE_EXPR (decl_placeholder, priv);
	      DECL_HAS_VALUE_EXPR_P (decl_placeholder) = 1;
	      lower_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c), ctx);
	      gimple_seq_add_seq (&sub_seq,
				  OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c));
	      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
	      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL;
	      OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c) = NULL;
	    }
	  else
	    {
	      x = build2 (code, TREE_TYPE (out), out, priv);
	      out = unshare_expr (out);
	      gimplify_assign (out, x, &sub_seq);
	    }
	  gimple *g = gimple_build_assign (new_var, POINTER_PLUS_EXPR, new_var,
					   TYPE_SIZE_UNIT (TREE_TYPE (type)));
	  gimple_seq_add_stmt (&sub_seq, g);
	  g = gimple_build_assign (ref, POINTER_PLUS_EXPR, ref,
				   TYPE_SIZE_UNIT (TREE_TYPE (type)));
	  gimple_seq_add_stmt (&sub_seq, g);
	  g = gimple_build_assign (i, PLUS_EXPR, i,
				   build_int_cst (TREE_TYPE (i), 1));
	  gimple_seq_add_stmt (&sub_seq, g);
	  g = gimple_build_cond (LE_EXPR, i, v, body, end);
	  gimple_seq_add_stmt (&sub_seq, g);
	  gimple_seq_add_stmt (&sub_seq, gimple_build_label (end));
	}
      else if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	{
	  tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);

	  if (is_reference (var)
	      && !useless_type_conversion_p (TREE_TYPE (placeholder),
					     TREE_TYPE (ref)))
	    ref = build_fold_addr_expr_loc (clause_loc, ref);
	  SET_DECL_VALUE_EXPR (placeholder, ref);
	  DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
	  lower_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c), ctx);
	  gimple_seq_add_seq (&sub_seq, OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c));
	  OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
	  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL;
	}
      else
	{
	  x = build2 (code, TREE_TYPE (ref), ref, new_var);
	  ref = build_outer_var_ref (var, ctx);
	  gimplify_assign (ref, x, &sub_seq);
	}
    }

  stmt = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ATOMIC_START),
			    0);
  gimple_seq_add_stmt (stmt_seqp, stmt);

  gimple_seq_add_seq (stmt_seqp, sub_seq);

  stmt = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ATOMIC_END),
			    0);
  gimple_seq_add_stmt (stmt_seqp, stmt);
}


/* Generate code to implement the COPYPRIVATE clauses.  */

static void
lower_copyprivate_clauses (tree clauses, gimple_seq *slist, gimple_seq *rlist,
			    omp_context *ctx)
{
  tree c;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    {
      tree var, new_var, ref, x;
      bool by_ref;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_COPYPRIVATE)
	continue;

      var = OMP_CLAUSE_DECL (c);
      by_ref = use_pointer_for_field (var, NULL);

      ref = build_sender_ref (var, ctx);
      x = new_var = lookup_decl_in_outer_ctx (var, ctx);
      if (by_ref)
	{
	  x = build_fold_addr_expr_loc (clause_loc, new_var);
	  x = fold_convert_loc (clause_loc, TREE_TYPE (ref), x);
	}
      gimplify_assign (ref, x, slist);

      ref = build_receiver_ref (var, false, ctx);
      if (by_ref)
	{
	  ref = fold_convert_loc (clause_loc,
				  build_pointer_type (TREE_TYPE (new_var)),
				  ref);
	  ref = build_fold_indirect_ref_loc (clause_loc, ref);
	}
      if (is_reference (var))
	{
	  ref = fold_convert_loc (clause_loc, TREE_TYPE (new_var), ref);
	  ref = build_simple_mem_ref_loc (clause_loc, ref);
	  new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	}
      x = lang_hooks.decls.omp_clause_assign_op (c, new_var, ref);
      gimplify_and_add (x, rlist);
    }
}


/* Generate code to implement the clauses, FIRSTPRIVATE, COPYIN, LASTPRIVATE,
   and REDUCTION from the sender (aka parent) side.  */

static void
lower_send_clauses (tree clauses, gimple_seq *ilist, gimple_seq *olist,
    		    omp_context *ctx)
{
  tree c, t;
  int ignored_looptemp = 0;
  bool is_taskloop = false;

  /* For taskloop, ignore first two _looptemp_ clauses, those are initialized
     by GOMP_taskloop.  */
  if (is_task_ctx (ctx) && gimple_omp_task_taskloop_p (ctx->stmt))
    {
      ignored_looptemp = 2;
      is_taskloop = true;
    }

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    {
      tree val, ref, x, var;
      bool by_ref, do_in = false, do_out = false;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_PRIVATE:
	  if (OMP_CLAUSE_PRIVATE_OUTER_REF (c))
	    break;
	  continue;
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_LASTPRIVATE:
	case OMP_CLAUSE_REDUCTION:
	  break;
	case OMP_CLAUSE_SHARED:
	  if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	    break;
	  continue;
	case OMP_CLAUSE__LOOPTEMP_:
	  if (ignored_looptemp)
	    {
	      ignored_looptemp--;
	      continue;
	    }
	  break;
	default:
	  continue;
	}

      val = OMP_CLAUSE_DECL (c);
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	  && TREE_CODE (val) == MEM_REF)
	{
	  val = TREE_OPERAND (val, 0);
	  if (TREE_CODE (val) == POINTER_PLUS_EXPR)
	    val = TREE_OPERAND (val, 0);
	  if (TREE_CODE (val) == INDIRECT_REF
	      || TREE_CODE (val) == ADDR_EXPR)
	    val = TREE_OPERAND (val, 0);
	  if (is_variable_sized (val))
	    continue;
	}

      /* For OMP_CLAUSE_SHARED_FIRSTPRIVATE, look beyond the
	 outer taskloop region.  */
      omp_context *ctx_for_o = ctx;
      if (is_taskloop
	  && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
	  && OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	ctx_for_o = ctx->outer;

      var = lookup_decl_in_outer_ctx (val, ctx_for_o);

      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_COPYIN
	  && is_global_var (var))
	continue;

      t = omp_member_access_dummy_var (var);
      if (t)
	{
	  var = DECL_VALUE_EXPR (var);
	  tree o = maybe_lookup_decl_in_outer_ctx (t, ctx_for_o);
	  if (o != t)
	    var = unshare_and_remap (var, t, o);
	  else
	    var = unshare_expr (var);
	}

      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED)
	{
	  /* Handle taskloop firstprivate/lastprivate, where the
	     lastprivate on GIMPLE_OMP_TASK is represented as
	     OMP_CLAUSE_SHARED_FIRSTPRIVATE.  */
	  tree f = lookup_sfield ((splay_tree_key) &DECL_UID (val), ctx);
	  x = omp_build_component_ref (ctx->sender_decl, f);
	  if (use_pointer_for_field (val, ctx))
	    var = build_fold_addr_expr (var);
	  gimplify_assign (x, var, ilist);
	  DECL_ABSTRACT_ORIGIN (f) = NULL;
	  continue;
	}

      if ((OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION
	   || val == OMP_CLAUSE_DECL (c))
	  && is_variable_sized (val))
	continue;
      by_ref = use_pointer_for_field (val, NULL);

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_FIRSTPRIVATE:
	  if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c)
	      && !by_ref
	      && is_task_ctx (ctx))
	    TREE_NO_WARNING (var) = 1;
	  do_in = true;
	  break;

	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE__LOOPTEMP_:
	  do_in = true;
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (by_ref || is_reference (val))
	    {
	      if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
		continue;
	      do_in = true;
	    }
	  else
	    {
	      do_out = true;
	      if (lang_hooks.decls.omp_private_outer_ref (val))
		do_in = true;
	    }
	  break;

	case OMP_CLAUSE_REDUCTION:
	  do_in = true;
	  if (val == OMP_CLAUSE_DECL (c))
	    do_out = !(by_ref || is_reference (val));
	  else
	    by_ref = TREE_CODE (TREE_TYPE (val)) == ARRAY_TYPE;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (do_in)
	{
	  ref = build_sender_ref (val, ctx);
	  x = by_ref ? build_fold_addr_expr_loc (clause_loc, var) : var;
	  gimplify_assign (ref, x, ilist);
	  if (is_task_ctx (ctx))
	    DECL_ABSTRACT_ORIGIN (TREE_OPERAND (ref, 1)) = NULL;
	}

      if (do_out)
	{
	  ref = build_sender_ref (val, ctx);
	  gimplify_assign (var, ref, olist);
	}
    }
}

/* Generate code to implement SHARED from the sender (aka parent)
   side.  This is trickier, since GIMPLE_OMP_PARALLEL_CLAUSES doesn't
   list things that got automatically shared.  */

static void
lower_send_shared_vars (gimple_seq *ilist, gimple_seq *olist, omp_context *ctx)
{
  tree var, ovar, nvar, t, f, x, record_type;

  if (ctx->record_type == NULL)
    return;

  record_type = ctx->srecord_type ? ctx->srecord_type : ctx->record_type;
  for (f = TYPE_FIELDS (record_type); f ; f = DECL_CHAIN (f))
    {
      ovar = DECL_ABSTRACT_ORIGIN (f);
      if (!ovar || TREE_CODE (ovar) == FIELD_DECL)
	continue;

      nvar = maybe_lookup_decl (ovar, ctx);
      if (!nvar || !DECL_HAS_VALUE_EXPR_P (nvar))
	continue;

      /* If CTX is a nested parallel directive.  Find the immediately
	 enclosing parallel or workshare construct that contains a
	 mapping for OVAR.  */
      var = lookup_decl_in_outer_ctx (ovar, ctx);

      t = omp_member_access_dummy_var (var);
      if (t)
	{
	  var = DECL_VALUE_EXPR (var);
	  tree o = maybe_lookup_decl_in_outer_ctx (t, ctx);
	  if (o != t)
	    var = unshare_and_remap (var, t, o);
	  else
	    var = unshare_expr (var);
	}

      if (use_pointer_for_field (ovar, ctx))
	{
	  x = build_sender_ref (ovar, ctx);
	  var = build_fold_addr_expr (var);
	  gimplify_assign (x, var, ilist);
	}
      else
	{
	  x = build_sender_ref (ovar, ctx);
	  gimplify_assign (x, var, ilist);

	  if (!TREE_READONLY (var)
	      /* We don't need to receive a new reference to a result
	         or parm decl.  In fact we may not store to it as we will
		 invalidate any pending RSO and generate wrong gimple
		 during inlining.  */
	      && !((TREE_CODE (var) == RESULT_DECL
		    || TREE_CODE (var) == PARM_DECL)
		   && DECL_BY_REFERENCE (var)))
	    {
	      x = build_sender_ref (ovar, ctx);
	      gimplify_assign (var, x, olist);
	    }
	}
    }
}

/* Emit an OpenACC head marker call, encapulating the partitioning and
   other information that must be processed by the target compiler.
   Return the maximum number of dimensions the associated loop might
   be partitioned over.  */

static unsigned
lower_oacc_head_mark (location_t loc, tree ddvar, tree clauses,
		      gimple_seq *seq, omp_context *ctx)
{
  unsigned levels = 0;
  unsigned tag = 0;
  tree gang_static = NULL_TREE;
  auto_vec<tree, 5> args;

  args.quick_push (build_int_cst
		   (integer_type_node, IFN_UNIQUE_OACC_HEAD_MARK));
  args.quick_push (ddvar);
  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_GANG:
	  tag |= OLF_DIM_GANG;
	  gang_static = OMP_CLAUSE_GANG_STATIC_EXPR (c);
	  /* static:* is represented by -1, and we can ignore it, as
	     scheduling is always static.  */
	  if (gang_static && integer_minus_onep (gang_static))
	    gang_static = NULL_TREE;
	  levels++;
	  break;

	case OMP_CLAUSE_WORKER:
	  tag |= OLF_DIM_WORKER;
	  levels++;
	  break;

	case OMP_CLAUSE_VECTOR:
	  tag |= OLF_DIM_VECTOR;
	  levels++;
	  break;

	case OMP_CLAUSE_SEQ:
	  tag |= OLF_SEQ;
	  break;

	case OMP_CLAUSE_AUTO:
	  tag |= OLF_AUTO;
	  break;

	case OMP_CLAUSE_INDEPENDENT:
	  tag |= OLF_INDEPENDENT;
	  break;

	default:
	  continue;
	}
    }

  if (gang_static)
    {
      if (DECL_P (gang_static))
	gang_static = build_outer_var_ref (gang_static, ctx);
      tag |= OLF_GANG_STATIC;
    }

  /* In a parallel region, loops are implicitly INDEPENDENT.  */
  omp_context *tgt = enclosing_target_ctx (ctx);
  if (!tgt || is_oacc_parallel (tgt))
    tag |= OLF_INDEPENDENT;

  /* A loop lacking SEQ, GANG, WORKER and/or VECTOR is implicitly AUTO.  */
  if (!(tag & (((GOMP_DIM_MASK (GOMP_DIM_MAX) - 1) << OLF_DIM_BASE)
	       | OLF_SEQ)))
      tag |= OLF_AUTO;

  /* Ensure at least one level.  */
  if (!levels)
    levels++;

  args.quick_push (build_int_cst (integer_type_node, levels));
  args.quick_push (build_int_cst (integer_type_node, tag));
  if (gang_static)
    args.quick_push (gang_static);

  gcall *call = gimple_build_call_internal_vec (IFN_UNIQUE, args);
  gimple_set_location (call, loc);
  gimple_set_lhs (call, ddvar);
  gimple_seq_add_stmt (seq, call);

  return levels;
}

/* Emit an OpenACC lopp head or tail marker to SEQ.  LEVEL is the
   partitioning level of the enclosed region.  */ 

static void
lower_oacc_loop_marker (location_t loc, tree ddvar, bool head,
			tree tofollow, gimple_seq *seq)
{
  int marker_kind = (head ? IFN_UNIQUE_OACC_HEAD_MARK
		     : IFN_UNIQUE_OACC_TAIL_MARK);
  tree marker = build_int_cst (integer_type_node, marker_kind);
  int nargs = 2 + (tofollow != NULL_TREE);
  gcall *call = gimple_build_call_internal (IFN_UNIQUE, nargs,
					    marker, ddvar, tofollow);
  gimple_set_location (call, loc);
  gimple_set_lhs (call, ddvar);
  gimple_seq_add_stmt (seq, call);
}

/* Generate the before and after OpenACC loop sequences.  CLAUSES are
   the loop clauses, from which we extract reductions.  Initialize
   HEAD and TAIL.  */

static void
lower_oacc_head_tail (location_t loc, tree clauses,
		      gimple_seq *head, gimple_seq *tail, omp_context *ctx)
{
  bool inner = false;
  tree ddvar = create_tmp_var (integer_type_node, ".data_dep");
  gimple_seq_add_stmt (head, gimple_build_assign (ddvar, integer_zero_node));

  unsigned count = lower_oacc_head_mark (loc, ddvar, clauses, head, ctx);
  tree fork_kind = build_int_cst (unsigned_type_node, IFN_UNIQUE_OACC_FORK);
  tree join_kind = build_int_cst (unsigned_type_node, IFN_UNIQUE_OACC_JOIN);

  gcc_assert (count);
  for (unsigned done = 1; count; count--, done++)
    {
      gimple_seq fork_seq = NULL;
      gimple_seq join_seq = NULL;

      tree place = build_int_cst (integer_type_node, -1);
      gcall *fork = gimple_build_call_internal (IFN_UNIQUE, 3,
						fork_kind, ddvar, place);
      gimple_set_location (fork, loc);
      gimple_set_lhs (fork, ddvar);

      gcall *join = gimple_build_call_internal (IFN_UNIQUE, 3,
						join_kind, ddvar, place);
      gimple_set_location (join, loc);
      gimple_set_lhs (join, ddvar);

      /* Mark the beginning of this level sequence.  */
      if (inner)
	lower_oacc_loop_marker (loc, ddvar, true,
				build_int_cst (integer_type_node, count),
				&fork_seq);
      lower_oacc_loop_marker (loc, ddvar, false,
			      build_int_cst (integer_type_node, done),
			      &join_seq);

      lower_oacc_reductions (loc, clauses, place, inner,
			     fork, join, &fork_seq, &join_seq,  ctx);

      /* Append this level to head. */
      gimple_seq_add_seq (head, fork_seq);
      /* Prepend it to tail.  */
      gimple_seq_add_seq (&join_seq, *tail);
      *tail = join_seq;

      inner = true;
    }

  /* Mark the end of the sequence.  */
  lower_oacc_loop_marker (loc, ddvar, true, NULL_TREE, head);
  lower_oacc_loop_marker (loc, ddvar, false, NULL_TREE, tail);
}

/* A convenience function to build an empty GIMPLE_COND with just the
   condition.  */

static gcond *
gimple_build_cond_empty (tree cond)
{
  enum tree_code pred_code;
  tree lhs, rhs;

  gimple_cond_get_ops_from_tree (cond, &pred_code, &lhs, &rhs);
  return gimple_build_cond (pred_code, lhs, rhs, NULL_TREE, NULL_TREE);
}

/* Return true if a parallel REGION is within a declare target function or
   within a target region and is not a part of a gridified target.  */

static bool
parallel_needs_hsa_kernel_p (struct omp_region *region)
{
  bool indirect = false;
  for (region = region->outer; region; region = region->outer)
    {
      if (region->type == GIMPLE_OMP_PARALLEL)
	indirect = true;
      else if (region->type == GIMPLE_OMP_TARGET)
	{
	  gomp_target *tgt_stmt
	    = as_a <gomp_target *> (last_stmt (region->entry));

	  if (find_omp_clause (gimple_omp_target_clauses (tgt_stmt),
			       OMP_CLAUSE__GRIDDIM_))
	    return indirect;
	  else
	    return true;
	}
    }

  if (lookup_attribute ("omp declare target",
			DECL_ATTRIBUTES (current_function_decl)))
    return true;

  return false;
}

static void expand_omp_build_assign (gimple_stmt_iterator *, tree, tree,
				     bool = false);

/* Build the function calls to GOMP_parallel_start etc to actually
   generate the parallel operation.  REGION is the parallel region
   being expanded.  BB is the block where to insert the code.  WS_ARGS
   will be set if this is a call to a combined parallel+workshare
   construct, it contains the list of additional arguments needed by
   the workshare construct.  */

static void
expand_parallel_call (struct omp_region *region, basic_block bb,
		      gomp_parallel *entry_stmt,
		      vec<tree, va_gc> *ws_args)
{
  tree t, t1, t2, val, cond, c, clauses, flags;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  enum built_in_function start_ix;
  int start_ix2;
  location_t clause_loc;
  vec<tree, va_gc> *args;

  clauses = gimple_omp_parallel_clauses (entry_stmt);

  /* Determine what flavor of GOMP_parallel we will be
     emitting.  */
  start_ix = BUILT_IN_GOMP_PARALLEL;
  if (is_combined_parallel (region))
    {
      switch (region->inner->type)
	{
	case GIMPLE_OMP_FOR:
	  gcc_assert (region->inner->sched_kind != OMP_CLAUSE_SCHEDULE_AUTO);
	  switch (region->inner->sched_kind)
	    {
	    case OMP_CLAUSE_SCHEDULE_RUNTIME:
	      start_ix2 = 3;
	      break;
	    case OMP_CLAUSE_SCHEDULE_DYNAMIC:
	    case OMP_CLAUSE_SCHEDULE_GUIDED:
	      if (region->inner->sched_modifiers
		  & OMP_CLAUSE_SCHEDULE_NONMONOTONIC)
		{
		  start_ix2 = 3 + region->inner->sched_kind;
		  break;
		}
	      /* FALLTHRU */
	    default:
	      start_ix2 = region->inner->sched_kind;
	      break;
	    }
	  start_ix2 += (int) BUILT_IN_GOMP_PARALLEL_LOOP_STATIC;
	  start_ix = (enum built_in_function) start_ix2;
	  break;
	case GIMPLE_OMP_SECTIONS:
	  start_ix = BUILT_IN_GOMP_PARALLEL_SECTIONS;
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  /* By default, the value of NUM_THREADS is zero (selected at run time)
     and there is no conditional.  */
  cond = NULL_TREE;
  val = build_int_cst (unsigned_type_node, 0);
  flags = build_int_cst (unsigned_type_node, 0);

  c = find_omp_clause (clauses, OMP_CLAUSE_IF);
  if (c)
    cond = OMP_CLAUSE_IF_EXPR (c);

  c = find_omp_clause (clauses, OMP_CLAUSE_NUM_THREADS);
  if (c)
    {
      val = OMP_CLAUSE_NUM_THREADS_EXPR (c);
      clause_loc = OMP_CLAUSE_LOCATION (c);
    }
  else
    clause_loc = gimple_location (entry_stmt);

  c = find_omp_clause (clauses, OMP_CLAUSE_PROC_BIND);
  if (c)
    flags = build_int_cst (unsigned_type_node, OMP_CLAUSE_PROC_BIND_KIND (c));

  /* Ensure 'val' is of the correct type.  */
  val = fold_convert_loc (clause_loc, unsigned_type_node, val);

  /* If we found the clause 'if (cond)', build either
     (cond != 0) or (cond ? val : 1u).  */
  if (cond)
    {
      cond = gimple_boolify (cond);

      if (integer_zerop (val))
	val = fold_build2_loc (clause_loc,
			   EQ_EXPR, unsigned_type_node, cond,
			   build_int_cst (TREE_TYPE (cond), 0));
      else
	{
	  basic_block cond_bb, then_bb, else_bb;
	  edge e, e_then, e_else;
	  tree tmp_then, tmp_else, tmp_join, tmp_var;

	  tmp_var = create_tmp_var (TREE_TYPE (val));
	  if (gimple_in_ssa_p (cfun))
	    {
	      tmp_then = make_ssa_name (tmp_var);
	      tmp_else = make_ssa_name (tmp_var);
	      tmp_join = make_ssa_name (tmp_var);
	    }
	  else
	    {
	      tmp_then = tmp_var;
	      tmp_else = tmp_var;
	      tmp_join = tmp_var;
	    }

	  e = split_block_after_labels (bb);
	  cond_bb = e->src;
	  bb = e->dest;
	  remove_edge (e);

	  then_bb = create_empty_bb (cond_bb);
	  else_bb = create_empty_bb (then_bb);
	  set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);
	  set_immediate_dominator (CDI_DOMINATORS, else_bb, cond_bb);

	  stmt = gimple_build_cond_empty (cond);
	  gsi = gsi_start_bb (cond_bb);
	  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

	  gsi = gsi_start_bb (then_bb);
	  expand_omp_build_assign (&gsi, tmp_then, val, true);

	  gsi = gsi_start_bb (else_bb);
	  expand_omp_build_assign (&gsi, tmp_else,
				   build_int_cst (unsigned_type_node, 1),
				   true);

	  make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
	  make_edge (cond_bb, else_bb, EDGE_FALSE_VALUE);
	  add_bb_to_loop (then_bb, cond_bb->loop_father);
	  add_bb_to_loop (else_bb, cond_bb->loop_father);
	  e_then = make_edge (then_bb, bb, EDGE_FALLTHRU);
	  e_else = make_edge (else_bb, bb, EDGE_FALLTHRU);

	  if (gimple_in_ssa_p (cfun))
	    {
	      gphi *phi = create_phi_node (tmp_join, bb);
	      add_phi_arg (phi, tmp_then, e_then, UNKNOWN_LOCATION);
	      add_phi_arg (phi, tmp_else, e_else, UNKNOWN_LOCATION);
	    }

	  val = tmp_join;
	}

      gsi = gsi_start_bb (bb);
      val = force_gimple_operand_gsi (&gsi, val, true, NULL_TREE,
				      false, GSI_CONTINUE_LINKING);
    }

  gsi = gsi_last_bb (bb);
  t = gimple_omp_parallel_data_arg (entry_stmt);
  if (t == NULL)
    t1 = null_pointer_node;
  else
    t1 = build_fold_addr_expr (t);
  tree child_fndecl = gimple_omp_parallel_child_fn (entry_stmt);
  t2 = build_fold_addr_expr (child_fndecl);

  vec_alloc (args, 4 + vec_safe_length (ws_args));
  args->quick_push (t2);
  args->quick_push (t1);
  args->quick_push (val);
  if (ws_args)
    args->splice (*ws_args);
  args->quick_push (flags);

  t = build_call_expr_loc_vec (UNKNOWN_LOCATION,
			       builtin_decl_explicit (start_ix), args);

  force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
			    false, GSI_CONTINUE_LINKING);

  if (hsa_gen_requested_p ()
      && parallel_needs_hsa_kernel_p (region))
    {
      cgraph_node *child_cnode = cgraph_node::get (child_fndecl);
      hsa_register_kernel (child_cnode);
    }
}

/* Insert a function call whose name is FUNC_NAME with the information from
   ENTRY_STMT into the basic_block BB.  */

static void
expand_cilk_for_call (basic_block bb, gomp_parallel *entry_stmt,
		      vec <tree, va_gc> *ws_args)
{
  tree t, t1, t2;
  gimple_stmt_iterator gsi;
  vec <tree, va_gc> *args;

  gcc_assert (vec_safe_length (ws_args) == 2);
  tree func_name = (*ws_args)[0];
  tree grain = (*ws_args)[1];

  tree clauses = gimple_omp_parallel_clauses (entry_stmt);
  tree count = find_omp_clause (clauses, OMP_CLAUSE__CILK_FOR_COUNT_);
  gcc_assert (count != NULL_TREE);
  count = OMP_CLAUSE_OPERAND (count, 0);

  gsi = gsi_last_bb (bb);
  t = gimple_omp_parallel_data_arg (entry_stmt);
  if (t == NULL)
    t1 = null_pointer_node;
  else
    t1 = build_fold_addr_expr (t);
  t2 = build_fold_addr_expr (gimple_omp_parallel_child_fn (entry_stmt));

  vec_alloc (args, 4);
  args->quick_push (t2);
  args->quick_push (t1);
  args->quick_push (count);
  args->quick_push (grain);
  t = build_call_expr_loc_vec (UNKNOWN_LOCATION, func_name, args);

  force_gimple_operand_gsi (&gsi, t, true, NULL_TREE, false,
			    GSI_CONTINUE_LINKING);
}

/* Build the function call to GOMP_task to actually
   generate the task operation.  BB is the block where to insert the code.  */

static void
expand_task_call (struct omp_region *region, basic_block bb,
		  gomp_task *entry_stmt)
{
  tree t1, t2, t3;
  gimple_stmt_iterator gsi;
  location_t loc = gimple_location (entry_stmt);

  tree clauses = gimple_omp_task_clauses (entry_stmt);

  tree ifc = find_omp_clause (clauses, OMP_CLAUSE_IF);
  tree untied = find_omp_clause (clauses, OMP_CLAUSE_UNTIED);
  tree mergeable = find_omp_clause (clauses, OMP_CLAUSE_MERGEABLE);
  tree depend = find_omp_clause (clauses, OMP_CLAUSE_DEPEND);
  tree finalc = find_omp_clause (clauses, OMP_CLAUSE_FINAL);
  tree priority = find_omp_clause (clauses, OMP_CLAUSE_PRIORITY);

  unsigned int iflags
    = (untied ? GOMP_TASK_FLAG_UNTIED : 0)
      | (mergeable ? GOMP_TASK_FLAG_MERGEABLE : 0)
      | (depend ? GOMP_TASK_FLAG_DEPEND : 0);

  bool taskloop_p = gimple_omp_task_taskloop_p (entry_stmt);
  tree startvar = NULL_TREE, endvar = NULL_TREE, step = NULL_TREE;
  tree num_tasks = NULL_TREE;
  bool ull = false;
  if (taskloop_p)
    {
      gimple *g = last_stmt (region->outer->entry);
      gcc_assert (gimple_code (g) == GIMPLE_OMP_FOR
		  && gimple_omp_for_kind (g) == GF_OMP_FOR_KIND_TASKLOOP);
      struct omp_for_data fd;
      extract_omp_for_data (as_a <gomp_for *> (g), &fd, NULL);
      startvar = find_omp_clause (clauses, OMP_CLAUSE__LOOPTEMP_);
      endvar = find_omp_clause (OMP_CLAUSE_CHAIN (startvar),
				OMP_CLAUSE__LOOPTEMP_);
      startvar = OMP_CLAUSE_DECL (startvar);
      endvar = OMP_CLAUSE_DECL (endvar);
      step = fold_convert_loc (loc, fd.iter_type, fd.loop.step);
      if (fd.loop.cond_code == LT_EXPR)
	iflags |= GOMP_TASK_FLAG_UP;
      tree tclauses = gimple_omp_for_clauses (g);
      num_tasks = find_omp_clause (tclauses, OMP_CLAUSE_NUM_TASKS);
      if (num_tasks)
	num_tasks = OMP_CLAUSE_NUM_TASKS_EXPR (num_tasks);
      else
	{
	  num_tasks = find_omp_clause (tclauses, OMP_CLAUSE_GRAINSIZE);
	  if (num_tasks)
	    {
	      iflags |= GOMP_TASK_FLAG_GRAINSIZE;
	      num_tasks = OMP_CLAUSE_GRAINSIZE_EXPR (num_tasks);
	    }
	  else
	    num_tasks = integer_zero_node;
	}
      num_tasks = fold_convert_loc (loc, long_integer_type_node, num_tasks);
      if (ifc == NULL_TREE)
	iflags |= GOMP_TASK_FLAG_IF;
      if (find_omp_clause (tclauses, OMP_CLAUSE_NOGROUP))
	iflags |= GOMP_TASK_FLAG_NOGROUP;
      ull = fd.iter_type == long_long_unsigned_type_node;
    }
  else if (priority)
    iflags |= GOMP_TASK_FLAG_PRIORITY;

  tree flags = build_int_cst (unsigned_type_node, iflags);

  tree cond = boolean_true_node;
  if (ifc)
    {
      if (taskloop_p)
	{
	  tree t = gimple_boolify (OMP_CLAUSE_IF_EXPR (ifc));
	  t = fold_build3_loc (loc, COND_EXPR, unsigned_type_node, t,
			       build_int_cst (unsigned_type_node,
					      GOMP_TASK_FLAG_IF),
			       build_int_cst (unsigned_type_node, 0));
	  flags = fold_build2_loc (loc, PLUS_EXPR, unsigned_type_node,
				   flags, t);
	}
      else
	cond = gimple_boolify (OMP_CLAUSE_IF_EXPR (ifc));
    }

  if (finalc)
    {
      tree t = gimple_boolify (OMP_CLAUSE_FINAL_EXPR (finalc));
      t = fold_build3_loc (loc, COND_EXPR, unsigned_type_node, t,
			   build_int_cst (unsigned_type_node,
					  GOMP_TASK_FLAG_FINAL),
			   build_int_cst (unsigned_type_node, 0));
      flags = fold_build2_loc (loc, PLUS_EXPR, unsigned_type_node, flags, t);
    }
  if (depend)
    depend = OMP_CLAUSE_DECL (depend);
  else
    depend = build_int_cst (ptr_type_node, 0);
  if (priority)
    priority = fold_convert (integer_type_node,
			     OMP_CLAUSE_PRIORITY_EXPR (priority));
  else
    priority = integer_zero_node;

  gsi = gsi_last_bb (bb);
  tree t = gimple_omp_task_data_arg (entry_stmt);
  if (t == NULL)
    t2 = null_pointer_node;
  else
    t2 = build_fold_addr_expr_loc (loc, t);
  t1 = build_fold_addr_expr_loc (loc, gimple_omp_task_child_fn (entry_stmt));
  t = gimple_omp_task_copy_fn (entry_stmt);
  if (t == NULL)
    t3 = null_pointer_node;
  else
    t3 = build_fold_addr_expr_loc (loc, t);

  if (taskloop_p)
    t = build_call_expr (ull
			 ? builtin_decl_explicit (BUILT_IN_GOMP_TASKLOOP_ULL)
			 : builtin_decl_explicit (BUILT_IN_GOMP_TASKLOOP),
			 11, t1, t2, t3,
			 gimple_omp_task_arg_size (entry_stmt),
			 gimple_omp_task_arg_align (entry_stmt), flags,
			 num_tasks, priority, startvar, endvar, step);
  else
    t = build_call_expr (builtin_decl_explicit (BUILT_IN_GOMP_TASK),
			 9, t1, t2, t3,
			 gimple_omp_task_arg_size (entry_stmt),
			 gimple_omp_task_arg_align (entry_stmt), cond, flags,
			 depend, priority);

  force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
			    false, GSI_CONTINUE_LINKING);
}


/* If exceptions are enabled, wrap the statements in BODY in a MUST_NOT_THROW
   catch handler and return it.  This prevents programs from violating the
   structured block semantics with throws.  */

static gimple_seq
maybe_catch_exception (gimple_seq body)
{
  gimple *g;
  tree decl;

  if (!flag_exceptions)
    return body;

  if (lang_hooks.eh_protect_cleanup_actions != NULL)
    decl = lang_hooks.eh_protect_cleanup_actions ();
  else
    decl = builtin_decl_explicit (BUILT_IN_TRAP);

  g = gimple_build_eh_must_not_throw (decl);
  g = gimple_build_try (body, gimple_seq_alloc_with_stmt (g),
      			GIMPLE_TRY_CATCH);

 return gimple_seq_alloc_with_stmt (g);
}

/* Chain all the DECLs in LIST by their TREE_CHAIN fields.  */

static tree
vec2chain (vec<tree, va_gc> *v)
{
  tree chain = NULL_TREE, t;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT_REVERSE (v, ix, t)
    {
      DECL_CHAIN (t) = chain;
      chain = t;
    }

  return chain;
}


/* Remove barriers in REGION->EXIT's block.  Note that this is only
   valid for GIMPLE_OMP_PARALLEL regions.  Since the end of a parallel region
   is an implicit barrier, any workshare inside the GIMPLE_OMP_PARALLEL that
   left a barrier at the end of the GIMPLE_OMP_PARALLEL region can now be
   removed.  */

static void
remove_exit_barrier (struct omp_region *region)
{
  gimple_stmt_iterator gsi;
  basic_block exit_bb;
  edge_iterator ei;
  edge e;
  gimple *stmt;
  int any_addressable_vars = -1;

  exit_bb = region->exit;

  /* If the parallel region doesn't return, we don't have REGION->EXIT
     block at all.  */
  if (! exit_bb)
    return;

  /* The last insn in the block will be the parallel's GIMPLE_OMP_RETURN.  The
     workshare's GIMPLE_OMP_RETURN will be in a preceding block.  The kinds of
     statements that can appear in between are extremely limited -- no
     memory operations at all.  Here, we allow nothing at all, so the
     only thing we allow to precede this GIMPLE_OMP_RETURN is a label.  */
  gsi = gsi_last_bb (exit_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_RETURN);
  gsi_prev (&gsi);
  if (!gsi_end_p (gsi) && gimple_code (gsi_stmt (gsi)) != GIMPLE_LABEL)
    return;

  FOR_EACH_EDGE (e, ei, exit_bb->preds)
    {
      gsi = gsi_last_bb (e->src);
      if (gsi_end_p (gsi))
	continue;
      stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) == GIMPLE_OMP_RETURN
	  && !gimple_omp_return_nowait_p (stmt))
	{
	  /* OpenMP 3.0 tasks unfortunately prevent this optimization
	     in many cases.  If there could be tasks queued, the barrier
	     might be needed to let the tasks run before some local
	     variable of the parallel that the task uses as shared
	     runs out of scope.  The task can be spawned either
	     from within current function (this would be easy to check)
	     or from some function it calls and gets passed an address
	     of such a variable.  */
	  if (any_addressable_vars < 0)
	    {
	      gomp_parallel *parallel_stmt
		= as_a <gomp_parallel *> (last_stmt (region->entry));
	      tree child_fun = gimple_omp_parallel_child_fn (parallel_stmt);
	      tree local_decls, block, decl;
	      unsigned ix;

	      any_addressable_vars = 0;
	      FOR_EACH_LOCAL_DECL (DECL_STRUCT_FUNCTION (child_fun), ix, decl)
		if (TREE_ADDRESSABLE (decl))
		  {
		    any_addressable_vars = 1;
		    break;
		  }
	      for (block = gimple_block (stmt);
		   !any_addressable_vars
		   && block
		   && TREE_CODE (block) == BLOCK;
		   block = BLOCK_SUPERCONTEXT (block))
		{
		  for (local_decls = BLOCK_VARS (block);
		       local_decls;
		       local_decls = DECL_CHAIN (local_decls))
		    if (TREE_ADDRESSABLE (local_decls))
		      {
			any_addressable_vars = 1;
			break;
		      }
		  if (block == gimple_block (parallel_stmt))
		    break;
		}
	    }
	  if (!any_addressable_vars)
	    gimple_omp_return_set_nowait (stmt);
	}
    }
}

static void
remove_exit_barriers (struct omp_region *region)
{
  if (region->type == GIMPLE_OMP_PARALLEL)
    remove_exit_barrier (region);

  if (region->inner)
    {
      region = region->inner;
      remove_exit_barriers (region);
      while (region->next)
	{
	  region = region->next;
	  remove_exit_barriers (region);
	}
    }
}

/* Optimize omp_get_thread_num () and omp_get_num_threads ()
   calls.  These can't be declared as const functions, but
   within one parallel body they are constant, so they can be
   transformed there into __builtin_omp_get_{thread_num,num_threads} ()
   which are declared const.  Similarly for task body, except
   that in untied task omp_get_thread_num () can change at any task
   scheduling point.  */

static void
optimize_omp_library_calls (gimple *entry_stmt)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  tree thr_num_tree = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
  tree thr_num_id = DECL_ASSEMBLER_NAME (thr_num_tree);
  tree num_thr_tree = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_THREADS);
  tree num_thr_id = DECL_ASSEMBLER_NAME (num_thr_tree);
  bool untied_task = (gimple_code (entry_stmt) == GIMPLE_OMP_TASK
		      && find_omp_clause (gimple_omp_task_clauses (entry_stmt),
					  OMP_CLAUSE_UNTIED) != NULL);

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *call = gsi_stmt (gsi);
	tree decl;

	if (is_gimple_call (call)
	    && (decl = gimple_call_fndecl (call))
	    && DECL_EXTERNAL (decl)
	    && TREE_PUBLIC (decl)
	    && DECL_INITIAL (decl) == NULL)
	  {
	    tree built_in;

	    if (DECL_NAME (decl) == thr_num_id)
	      {
		/* In #pragma omp task untied omp_get_thread_num () can change
		   during the execution of the task region.  */
		if (untied_task)
		  continue;
		built_in = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
	      }
	    else if (DECL_NAME (decl) == num_thr_id)
	      built_in = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_THREADS);
	    else
	      continue;

	    if (DECL_ASSEMBLER_NAME (decl) != DECL_ASSEMBLER_NAME (built_in)
		|| gimple_call_num_args (call) != 0)
	      continue;

	    if (flag_exceptions && !TREE_NOTHROW (decl))
	      continue;

	    if (TREE_CODE (TREE_TYPE (decl)) != FUNCTION_TYPE
		|| !types_compatible_p (TREE_TYPE (TREE_TYPE (decl)),
					TREE_TYPE (TREE_TYPE (built_in))))
	      continue;

	    gimple_call_set_fndecl (call, built_in);
	  }
      }
}

/* Callback for expand_omp_build_assign.  Return non-NULL if *tp needs to be
   regimplified.  */

static tree
expand_omp_regimplify_p (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;

  /* Any variable with DECL_VALUE_EXPR needs to be regimplified.  */
  if (TREE_CODE (t) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (t))
    return t;

  if (TREE_CODE (t) == ADDR_EXPR)
    recompute_tree_invariant_for_addr_expr (t);

  *walk_subtrees = !TYPE_P (t) && !DECL_P (t);
  return NULL_TREE;
}

/* Prepend or append TO = FROM assignment before or after *GSI_P.  */

static void
expand_omp_build_assign (gimple_stmt_iterator *gsi_p, tree to, tree from,
			 bool after)
{
  bool simple_p = DECL_P (to) && TREE_ADDRESSABLE (to);
  from = force_gimple_operand_gsi (gsi_p, from, simple_p, NULL_TREE,
				   !after, after ? GSI_CONTINUE_LINKING
						 : GSI_SAME_STMT);
  gimple *stmt = gimple_build_assign (to, from);
  if (after)
    gsi_insert_after (gsi_p, stmt, GSI_CONTINUE_LINKING);
  else
    gsi_insert_before (gsi_p, stmt, GSI_SAME_STMT);
  if (walk_tree (&from, expand_omp_regimplify_p, NULL, NULL)
      || walk_tree (&to, expand_omp_regimplify_p, NULL, NULL))
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gimple_regimplify_operands (stmt, &gsi);
    }
}

/* Expand the OpenMP parallel or task directive starting at REGION.  */

static void
expand_omp_taskreg (struct omp_region *region)
{
  basic_block entry_bb, exit_bb, new_bb;
  struct function *child_cfun;
  tree child_fn, block, t;
  gimple_stmt_iterator gsi;
  gimple *entry_stmt, *stmt;
  edge e;
  vec<tree, va_gc> *ws_args;

  entry_stmt = last_stmt (region->entry);
  child_fn = gimple_omp_taskreg_child_fn (entry_stmt);
  child_cfun = DECL_STRUCT_FUNCTION (child_fn);

  entry_bb = region->entry;
  if (gimple_code (entry_stmt) == GIMPLE_OMP_TASK)
    exit_bb = region->cont;
  else
    exit_bb = region->exit;

  bool is_cilk_for
    = (flag_cilkplus
       && gimple_code (entry_stmt) == GIMPLE_OMP_PARALLEL
       && find_omp_clause (gimple_omp_parallel_clauses (entry_stmt),
			   OMP_CLAUSE__CILK_FOR_COUNT_) != NULL_TREE);

  if (is_cilk_for)
    /* If it is a _Cilk_for statement, it is modelled *like* a parallel for,
       and the inner statement contains the name of the built-in function
       and grain.  */
    ws_args = region->inner->ws_args;
  else if (is_combined_parallel (region))
    ws_args = region->ws_args;
  else
    ws_args = NULL;

  if (child_cfun->cfg)
    {
      /* Due to inlining, it may happen that we have already outlined
	 the region, in which case all we need to do is make the
	 sub-graph unreachable and emit the parallel call.  */
      edge entry_succ_e, exit_succ_e;

      entry_succ_e = single_succ_edge (entry_bb);

      gsi = gsi_last_bb (entry_bb);
      gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_PARALLEL
		  || gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_TASK);
      gsi_remove (&gsi, true);

      new_bb = entry_bb;
      if (exit_bb)
	{
	  exit_succ_e = single_succ_edge (exit_bb);
	  make_edge (new_bb, exit_succ_e->dest, EDGE_FALLTHRU);
	}
      remove_edge_and_dominated_blocks (entry_succ_e);
    }
  else
    {
      unsigned srcidx, dstidx, num;

      /* If the parallel region needs data sent from the parent
	 function, then the very first statement (except possible
	 tree profile counter updates) of the parallel body
	 is a copy assignment .OMP_DATA_I = &.OMP_DATA_O.  Since
	 &.OMP_DATA_O is passed as an argument to the child function,
	 we need to replace it with the argument as seen by the child
	 function.

	 In most cases, this will end up being the identity assignment
	 .OMP_DATA_I = .OMP_DATA_I.  However, if the parallel body had
	 a function call that has been inlined, the original PARM_DECL
	 .OMP_DATA_I may have been converted into a different local
	 variable.  In which case, we need to keep the assignment.  */
      if (gimple_omp_taskreg_data_arg (entry_stmt))
	{
	  basic_block entry_succ_bb
	    = single_succ_p (entry_bb) ? single_succ (entry_bb)
				       : FALLTHRU_EDGE (entry_bb)->dest;
	  tree arg;
	  gimple *parcopy_stmt = NULL;

	  for (gsi = gsi_start_bb (entry_succ_bb); ; gsi_next (&gsi))
	    {
	      gimple *stmt;

	      gcc_assert (!gsi_end_p (gsi));
	      stmt = gsi_stmt (gsi);
	      if (gimple_code (stmt) != GIMPLE_ASSIGN)
		continue;

	      if (gimple_num_ops (stmt) == 2)
		{
		  tree arg = gimple_assign_rhs1 (stmt);

		  /* We're ignore the subcode because we're
		     effectively doing a STRIP_NOPS.  */

		  if (TREE_CODE (arg) == ADDR_EXPR
		      && TREE_OPERAND (arg, 0)
		        == gimple_omp_taskreg_data_arg (entry_stmt))
		    {
		      parcopy_stmt = stmt;
		      break;
		    }
		}
	    }

	  gcc_assert (parcopy_stmt != NULL);
	  arg = DECL_ARGUMENTS (child_fn);

	  if (!gimple_in_ssa_p (cfun))
	    {
	      if (gimple_assign_lhs (parcopy_stmt) == arg)
		gsi_remove (&gsi, true);
	      else
		{
	          /* ?? Is setting the subcode really necessary ??  */
		  gimple_omp_set_subcode (parcopy_stmt, TREE_CODE (arg));
		  gimple_assign_set_rhs1 (parcopy_stmt, arg);
		}
	    }
	  else
	    {
	      tree lhs = gimple_assign_lhs (parcopy_stmt);
	      gcc_assert (SSA_NAME_VAR (lhs) == arg);
	      /* We'd like to set the rhs to the default def in the child_fn,
		 but it's too early to create ssa names in the child_fn.
		 Instead, we set the rhs to the parm.  In
		 move_sese_region_to_fn, we introduce a default def for the
		 parm, map the parm to it's default def, and once we encounter
		 this stmt, replace the parm with the default def.  */
	      gimple_assign_set_rhs1 (parcopy_stmt, arg);
	      update_stmt (parcopy_stmt);
	    }
	}

      /* Declare local variables needed in CHILD_CFUN.  */
      block = DECL_INITIAL (child_fn);
      BLOCK_VARS (block) = vec2chain (child_cfun->local_decls);
      /* The gimplifier could record temporaries in parallel/task block
	 rather than in containing function's local_decls chain,
	 which would mean cgraph missed finalizing them.  Do it now.  */
      for (t = BLOCK_VARS (block); t; t = DECL_CHAIN (t))
	if (TREE_CODE (t) == VAR_DECL
	    && TREE_STATIC (t)
	    && !DECL_EXTERNAL (t))
	  varpool_node::finalize_decl (t);
      DECL_SAVED_TREE (child_fn) = NULL;
      /* We'll create a CFG for child_fn, so no gimple body is needed.  */
      gimple_set_body (child_fn, NULL);
      TREE_USED (block) = 1;

      /* Reset DECL_CONTEXT on function arguments.  */
      for (t = DECL_ARGUMENTS (child_fn); t; t = DECL_CHAIN (t))
	DECL_CONTEXT (t) = child_fn;

      /* Split ENTRY_BB at GIMPLE_OMP_PARALLEL or GIMPLE_OMP_TASK,
	 so that it can be moved to the child function.  */
      gsi = gsi_last_bb (entry_bb);
      stmt = gsi_stmt (gsi);
      gcc_assert (stmt && (gimple_code (stmt) == GIMPLE_OMP_PARALLEL
			   || gimple_code (stmt) == GIMPLE_OMP_TASK));
      e = split_block (entry_bb, stmt);
      gsi_remove (&gsi, true);
      entry_bb = e->dest;
      edge e2 = NULL;
      if (gimple_code (entry_stmt) == GIMPLE_OMP_PARALLEL)
	single_succ_edge (entry_bb)->flags = EDGE_FALLTHRU;
      else
	{
	  e2 = make_edge (e->src, BRANCH_EDGE (entry_bb)->dest, EDGE_ABNORMAL);
	  gcc_assert (e2->dest == region->exit);
	  remove_edge (BRANCH_EDGE (entry_bb));
	  set_immediate_dominator (CDI_DOMINATORS, e2->dest, e->src);
	  gsi = gsi_last_bb (region->exit);
	  gcc_assert (!gsi_end_p (gsi)
		      && gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_RETURN);
	  gsi_remove (&gsi, true);
	}

      /* Convert GIMPLE_OMP_{RETURN,CONTINUE} into a RETURN_EXPR.  */
      if (exit_bb)
	{
	  gsi = gsi_last_bb (exit_bb);
	  gcc_assert (!gsi_end_p (gsi)
		      && (gimple_code (gsi_stmt (gsi))
			  == (e2 ? GIMPLE_OMP_CONTINUE : GIMPLE_OMP_RETURN)));
	  stmt = gimple_build_return (NULL);
	  gsi_insert_after (&gsi, stmt, GSI_SAME_STMT);
	  gsi_remove (&gsi, true);
	}

      /* Move the parallel region into CHILD_CFUN.  */

      if (gimple_in_ssa_p (cfun))
	{
	  init_tree_ssa (child_cfun);
	  init_ssa_operands (child_cfun);
	  child_cfun->gimple_df->in_ssa_p = true;
	  block = NULL_TREE;
	}
      else
	block = gimple_block (entry_stmt);

      new_bb = move_sese_region_to_fn (child_cfun, entry_bb, exit_bb, block);
      if (exit_bb)
	single_succ_edge (new_bb)->flags = EDGE_FALLTHRU;
      if (e2)
	{
	  basic_block dest_bb = e2->dest;
	  if (!exit_bb)
	    make_edge (new_bb, dest_bb, EDGE_FALLTHRU);
	  remove_edge (e2);
	  set_immediate_dominator (CDI_DOMINATORS, dest_bb, new_bb);
	}
      /* When the OMP expansion process cannot guarantee an up-to-date
         loop tree arrange for the child function to fixup loops.  */
      if (loops_state_satisfies_p (LOOPS_NEED_FIXUP))
	child_cfun->x_current_loops->state |= LOOPS_NEED_FIXUP;

      /* Remove non-local VAR_DECLs from child_cfun->local_decls list.  */
      num = vec_safe_length (child_cfun->local_decls);
      for (srcidx = 0, dstidx = 0; srcidx < num; srcidx++)
	{
	  t = (*child_cfun->local_decls)[srcidx];
	  if (DECL_CONTEXT (t) == cfun->decl)
	    continue;
	  if (srcidx != dstidx)
	    (*child_cfun->local_decls)[dstidx] = t;
	  dstidx++;
	}
      if (dstidx != num)
	vec_safe_truncate (child_cfun->local_decls, dstidx);

      /* Inform the callgraph about the new function.  */
      child_cfun->curr_properties = cfun->curr_properties;
      child_cfun->has_simduid_loops |= cfun->has_simduid_loops;
      child_cfun->has_force_vectorize_loops |= cfun->has_force_vectorize_loops;
      cgraph_node *node = cgraph_node::get_create (child_fn);
      node->parallelized_function = 1;
      cgraph_node::add_new_function (child_fn, true);

      bool need_asm = DECL_ASSEMBLER_NAME_SET_P (current_function_decl)
		      && !DECL_ASSEMBLER_NAME_SET_P (child_fn);

      /* Fix the callgraph edges for child_cfun.  Those for cfun will be
	 fixed in a following pass.  */
      push_cfun (child_cfun);
      if (need_asm)
	assign_assembler_name_if_neeeded (child_fn);

      if (optimize)
	optimize_omp_library_calls (entry_stmt);
      cgraph_edge::rebuild_edges ();

      /* Some EH regions might become dead, see PR34608.  If
	 pass_cleanup_cfg isn't the first pass to happen with the
	 new child, these dead EH edges might cause problems.
	 Clean them up now.  */
      if (flag_exceptions)
	{
	  basic_block bb;
	  bool changed = false;

	  FOR_EACH_BB_FN (bb, cfun)
	    changed |= gimple_purge_dead_eh_edges (bb);
	  if (changed)
	    cleanup_tree_cfg ();
	}
      if (gimple_in_ssa_p (cfun))
	update_ssa (TODO_update_ssa);
      if (flag_checking && !loops_state_satisfies_p (LOOPS_NEED_FIXUP))
	verify_loop_structure ();
      pop_cfun ();

      if (dump_file && !gimple_in_ssa_p (cfun))
	{
	  omp_any_child_fn_dumped = true;
	  dump_function_header (dump_file, child_fn, dump_flags);
	  dump_function_to_file (child_fn, dump_file, dump_flags);
	}
    }

  /* Emit a library call to launch the children threads.  */
  if (is_cilk_for)
    expand_cilk_for_call (new_bb,
			  as_a <gomp_parallel *> (entry_stmt), ws_args);
  else if (gimple_code (entry_stmt) == GIMPLE_OMP_PARALLEL)
    expand_parallel_call (region, new_bb,
			  as_a <gomp_parallel *> (entry_stmt), ws_args);
  else
    expand_task_call (region, new_bb, as_a <gomp_task *> (entry_stmt));
  if (gimple_in_ssa_p (cfun))
    update_ssa (TODO_update_ssa_only_virtuals);
}

/* Information about members of an OpenACC collapsed loop nest.  */

struct oacc_collapse
{
  tree base;  /* Base value. */
  tree iters; /* Number of steps.  */
  tree step;  /* step size.  */
};

/* Helper for expand_oacc_for.  Determine collapsed loop information.
   Fill in COUNTS array.  Emit any initialization code before GSI.
   Return the calculated outer loop bound of BOUND_TYPE.  */

static tree
expand_oacc_collapse_init (const struct omp_for_data *fd,
			   gimple_stmt_iterator *gsi,
			   oacc_collapse *counts, tree bound_type)
{
  tree total = build_int_cst (bound_type, 1);
  int ix;
  
  gcc_assert (integer_onep (fd->loop.step));
  gcc_assert (integer_zerop (fd->loop.n1));

  for (ix = 0; ix != fd->collapse; ix++)
    {
      const omp_for_data_loop *loop = &fd->loops[ix];

      tree iter_type = TREE_TYPE (loop->v);
      tree diff_type = iter_type;
      tree plus_type = iter_type;

      gcc_assert (loop->cond_code == fd->loop.cond_code);
      
      if (POINTER_TYPE_P (iter_type))
	plus_type = sizetype;
      if (POINTER_TYPE_P (diff_type) || TYPE_UNSIGNED (diff_type))
	diff_type = signed_type_for (diff_type);

      tree b = loop->n1;
      tree e = loop->n2;
      tree s = loop->step;
      bool up = loop->cond_code == LT_EXPR;
      tree dir = build_int_cst (diff_type, up ? +1 : -1);
      bool negating;
      tree expr;

      b = force_gimple_operand_gsi (gsi, b, true, NULL_TREE,
				    true, GSI_SAME_STMT);
      e = force_gimple_operand_gsi (gsi, e, true, NULL_TREE,
				    true, GSI_SAME_STMT);

      /* Convert the step, avoiding possible unsigned->signed overflow. */
      negating = !up && TYPE_UNSIGNED (TREE_TYPE (s));
      if (negating)
	s = fold_build1 (NEGATE_EXPR, TREE_TYPE (s), s);
      s = fold_convert (diff_type, s);
      if (negating)
	s = fold_build1 (NEGATE_EXPR, diff_type, s);
      s = force_gimple_operand_gsi (gsi, s, true, NULL_TREE,
				    true, GSI_SAME_STMT);

      /* Determine the range, avoiding possible unsigned->signed overflow. */
      negating = !up && TYPE_UNSIGNED (iter_type);
      expr = fold_build2 (MINUS_EXPR, plus_type,
			  fold_convert (plus_type, negating ? b : e),
			  fold_convert (plus_type, negating ? e : b));
      expr = fold_convert (diff_type, expr);
      if (negating)
	expr = fold_build1 (NEGATE_EXPR, diff_type, expr);
      tree range = force_gimple_operand_gsi
	(gsi, expr, true, NULL_TREE, true, GSI_SAME_STMT);

      /* Determine number of iterations.  */
      expr = fold_build2 (MINUS_EXPR, diff_type, range, dir);
      expr = fold_build2 (PLUS_EXPR, diff_type, expr, s);
      expr = fold_build2 (TRUNC_DIV_EXPR, diff_type, expr, s);

      tree iters = force_gimple_operand_gsi (gsi, expr, true, NULL_TREE,
					     true, GSI_SAME_STMT);

      counts[ix].base = b;
      counts[ix].iters = iters;
      counts[ix].step = s;

      total = fold_build2 (MULT_EXPR, bound_type, total,
			   fold_convert (bound_type, iters));
    }

  return total;
}

/* Emit initializers for collapsed loop members.  IVAR is the outer
   loop iteration variable, from which collapsed loop iteration values
   are  calculated.  COUNTS array has been initialized by
   expand_oacc_collapse_inits.  */

static void
expand_oacc_collapse_vars (const struct omp_for_data *fd,
			   gimple_stmt_iterator *gsi,
			   const oacc_collapse *counts, tree ivar)
{
  tree ivar_type = TREE_TYPE (ivar);

  /*  The most rapidly changing iteration variable is the innermost
      one.  */
  for (int ix = fd->collapse; ix--;)
    {
      const omp_for_data_loop *loop = &fd->loops[ix];
      const oacc_collapse *collapse = &counts[ix];
      tree iter_type = TREE_TYPE (loop->v);
      tree diff_type = TREE_TYPE (collapse->step);
      tree plus_type = iter_type;
      enum tree_code plus_code = PLUS_EXPR;
      tree expr;

      if (POINTER_TYPE_P (iter_type))
	{
	  plus_code = POINTER_PLUS_EXPR;
	  plus_type = sizetype;
	}

      expr = fold_build2 (TRUNC_MOD_EXPR, ivar_type, ivar,
			  fold_convert (ivar_type, collapse->iters));
      expr = fold_build2 (MULT_EXPR, diff_type, fold_convert (diff_type, expr),
			  collapse->step);
      expr = fold_build2 (plus_code, iter_type, collapse->base,
			  fold_convert (plus_type, expr));
      expr = force_gimple_operand_gsi (gsi, expr, false, NULL_TREE,
				       true, GSI_SAME_STMT);
      gassign *ass = gimple_build_assign (loop->v, expr);
      gsi_insert_before (gsi, ass, GSI_SAME_STMT);

      if (ix)
	{
	  expr = fold_build2 (TRUNC_DIV_EXPR, ivar_type, ivar,
			      fold_convert (ivar_type, collapse->iters));
	  ivar = force_gimple_operand_gsi (gsi, expr, true, NULL_TREE,
					   true, GSI_SAME_STMT);
	}
    }
}


/* Helper function for expand_omp_{for_*,simd}.  If this is the outermost
   of the combined collapse > 1 loop constructs, generate code like:
	if (__builtin_expect (N32 cond3 N31, 0)) goto ZERO_ITER_BB;
	if (cond3 is <)
	  adj = STEP3 - 1;
	else
	  adj = STEP3 + 1;
	count3 = (adj + N32 - N31) / STEP3;
	if (__builtin_expect (N22 cond2 N21, 0)) goto ZERO_ITER_BB;
	if (cond2 is <)
	  adj = STEP2 - 1;
	else
	  adj = STEP2 + 1;
	count2 = (adj + N22 - N21) / STEP2;
	if (__builtin_expect (N12 cond1 N11, 0)) goto ZERO_ITER_BB;
	if (cond1 is <)
	  adj = STEP1 - 1;
	else
	  adj = STEP1 + 1;
	count1 = (adj + N12 - N11) / STEP1;
	count = count1 * count2 * count3;
   Furthermore, if ZERO_ITER_BB is NULL, create a BB which does:
	count = 0;
   and set ZERO_ITER_BB to that bb.  If this isn't the outermost
   of the combined loop constructs, just initialize COUNTS array
   from the _looptemp_ clauses.  */

/* NOTE: It *could* be better to moosh all of the BBs together,
   creating one larger BB with all the computation and the unexpected
   jump at the end.  I.e.

   bool zero3, zero2, zero1, zero;

   zero3 = N32 c3 N31;
   count3 = (N32 - N31) /[cl] STEP3;
   zero2 = N22 c2 N21;
   count2 = (N22 - N21) /[cl] STEP2;
   zero1 = N12 c1 N11;
   count1 = (N12 - N11) /[cl] STEP1;
   zero = zero3 || zero2 || zero1;
   count = count1 * count2 * count3;
   if (__builtin_expect(zero, false)) goto zero_iter_bb;

   After all, we expect the zero=false, and thus we expect to have to
   evaluate all of the comparison expressions, so short-circuiting
   oughtn't be a win.  Since the condition isn't protecting a
   denominator, we're not concerned about divide-by-zero, so we can
   fully evaluate count even if a numerator turned out to be wrong.

   It seems like putting this all together would create much better
   scheduling opportunities, and less pressure on the chip's branch
   predictor.  */

static void
expand_omp_for_init_counts (struct omp_for_data *fd, gimple_stmt_iterator *gsi,
			    basic_block &entry_bb, tree *counts,
			    basic_block &zero_iter1_bb, int &first_zero_iter1,
			    basic_block &zero_iter2_bb, int &first_zero_iter2,
			    basic_block &l2_dom_bb)
{
  tree t, type = TREE_TYPE (fd->loop.v);
  edge e, ne;
  int i;

  /* Collapsed loops need work for expansion into SSA form.  */
  gcc_assert (!gimple_in_ssa_p (cfun));

  if (gimple_omp_for_combined_into_p (fd->for_stmt)
      && TREE_CODE (fd->loop.n2) != INTEGER_CST)
    {
      gcc_assert (fd->ordered == 0);
      /* First two _looptemp_ clauses are for istart/iend, counts[0]
	 isn't supposed to be handled, as the inner loop doesn't
	 use it.  */
      tree innerc = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				     OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      for (i = 0; i < fd->collapse; i++)
	{
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  gcc_assert (innerc);
	  if (i)
	    counts[i] = OMP_CLAUSE_DECL (innerc);
	  else
	    counts[0] = NULL_TREE;
	}
      return;
    }

  for (i = fd->collapse; i < fd->ordered; i++)
    {
      tree itype = TREE_TYPE (fd->loops[i].v);
      counts[i] = NULL_TREE;
      t = fold_binary (fd->loops[i].cond_code, boolean_type_node,
		       fold_convert (itype, fd->loops[i].n1),
		       fold_convert (itype, fd->loops[i].n2));
      if (t && integer_zerop (t))
	{
	  for (i = fd->collapse; i < fd->ordered; i++)
	    counts[i] = build_int_cst (type, 0);
	  break;
	}
    }
  for (i = 0; i < (fd->ordered ? fd->ordered : fd->collapse); i++)
    {
      tree itype = TREE_TYPE (fd->loops[i].v);

      if (i >= fd->collapse && counts[i])
	continue;
      if ((SSA_VAR_P (fd->loop.n2) || i >= fd->collapse)
	  && ((t = fold_binary (fd->loops[i].cond_code, boolean_type_node,
				fold_convert (itype, fd->loops[i].n1),
				fold_convert (itype, fd->loops[i].n2)))
	      == NULL_TREE || !integer_onep (t)))
	{
	  gcond *cond_stmt;
	  tree n1, n2;
	  n1 = fold_convert (itype, unshare_expr (fd->loops[i].n1));
	  n1 = force_gimple_operand_gsi (gsi, n1, true, NULL_TREE,
					 true, GSI_SAME_STMT);
	  n2 = fold_convert (itype, unshare_expr (fd->loops[i].n2));
	  n2 = force_gimple_operand_gsi (gsi, n2, true, NULL_TREE,
					 true, GSI_SAME_STMT);
	  cond_stmt = gimple_build_cond (fd->loops[i].cond_code, n1, n2,
					 NULL_TREE, NULL_TREE);
	  gsi_insert_before (gsi, cond_stmt, GSI_SAME_STMT);
	  if (walk_tree (gimple_cond_lhs_ptr (cond_stmt),
			 expand_omp_regimplify_p, NULL, NULL)
	      || walk_tree (gimple_cond_rhs_ptr (cond_stmt),
			    expand_omp_regimplify_p, NULL, NULL))
	    {
	      *gsi = gsi_for_stmt (cond_stmt);
	      gimple_regimplify_operands (cond_stmt, gsi);
	    }
	  e = split_block (entry_bb, cond_stmt);
	  basic_block &zero_iter_bb
	    = i < fd->collapse ? zero_iter1_bb : zero_iter2_bb;
	  int &first_zero_iter
	    = i < fd->collapse ? first_zero_iter1 : first_zero_iter2;
	  if (zero_iter_bb == NULL)
	    {
	      gassign *assign_stmt;
	      first_zero_iter = i;
	      zero_iter_bb = create_empty_bb (entry_bb);
	      add_bb_to_loop (zero_iter_bb, entry_bb->loop_father);
	      *gsi = gsi_after_labels (zero_iter_bb);
	      if (i < fd->collapse)
		assign_stmt = gimple_build_assign (fd->loop.n2,
						   build_zero_cst (type));
	      else
		{
		  counts[i] = create_tmp_reg (type, ".count");
		  assign_stmt
		    = gimple_build_assign (counts[i], build_zero_cst (type));
		}
	      gsi_insert_before (gsi, assign_stmt, GSI_SAME_STMT);
	      set_immediate_dominator (CDI_DOMINATORS, zero_iter_bb,
				       entry_bb);
	    }
	  ne = make_edge (entry_bb, zero_iter_bb, EDGE_FALSE_VALUE);
	  ne->probability = REG_BR_PROB_BASE / 2000 - 1;
	  e->flags = EDGE_TRUE_VALUE;
	  e->probability = REG_BR_PROB_BASE - ne->probability;
	  if (l2_dom_bb == NULL)
	    l2_dom_bb = entry_bb;
	  entry_bb = e->dest;
	  *gsi = gsi_last_bb (entry_bb);
	}

      if (POINTER_TYPE_P (itype))
	itype = signed_type_for (itype);
      t = build_int_cst (itype, (fd->loops[i].cond_code == LT_EXPR
				 ? -1 : 1));
      t = fold_build2 (PLUS_EXPR, itype,
		       fold_convert (itype, fd->loops[i].step), t);
      t = fold_build2 (PLUS_EXPR, itype, t,
		       fold_convert (itype, fd->loops[i].n2));
      t = fold_build2 (MINUS_EXPR, itype, t,
		       fold_convert (itype, fd->loops[i].n1));
      /* ?? We could probably use CEIL_DIV_EXPR instead of
	 TRUNC_DIV_EXPR and adjusting by hand.  Unless we can't
	 generate the same code in the end because generically we
	 don't know that the values involved must be negative for
	 GT??  */
      if (TYPE_UNSIGNED (itype) && fd->loops[i].cond_code == GT_EXPR)
	t = fold_build2 (TRUNC_DIV_EXPR, itype,
			 fold_build1 (NEGATE_EXPR, itype, t),
			 fold_build1 (NEGATE_EXPR, itype,
				      fold_convert (itype,
						    fd->loops[i].step)));
      else
	t = fold_build2 (TRUNC_DIV_EXPR, itype, t,
			 fold_convert (itype, fd->loops[i].step));
      t = fold_convert (type, t);
      if (TREE_CODE (t) == INTEGER_CST)
	counts[i] = t;
      else
	{
	  if (i < fd->collapse || i != first_zero_iter2)
	    counts[i] = create_tmp_reg (type, ".count");
	  expand_omp_build_assign (gsi, counts[i], t);
	}
      if (SSA_VAR_P (fd->loop.n2) && i < fd->collapse)
	{
	  if (i == 0)
	    t = counts[0];
	  else
	    t = fold_build2 (MULT_EXPR, type, fd->loop.n2, counts[i]);
	  expand_omp_build_assign (gsi, fd->loop.n2, t);
	}
    }
}


/* Helper function for expand_omp_{for_*,simd}.  Generate code like:
	T = V;
	V3 = N31 + (T % count3) * STEP3;
	T = T / count3;
	V2 = N21 + (T % count2) * STEP2;
	T = T / count2;
	V1 = N11 + T * STEP1;
   if this loop doesn't have an inner loop construct combined with it.
   If it does have an inner loop construct combined with it and the
   iteration count isn't known constant, store values from counts array
   into its _looptemp_ temporaries instead.  */

static void
expand_omp_for_init_vars (struct omp_for_data *fd, gimple_stmt_iterator *gsi,
			  tree *counts, gimple *inner_stmt, tree startvar)
{
  int i;
  if (gimple_omp_for_combined_p (fd->for_stmt))
    {
      /* If fd->loop.n2 is constant, then no propagation of the counts
	 is needed, they are constant.  */
      if (TREE_CODE (fd->loop.n2) == INTEGER_CST)
	return;

      tree clauses = gimple_code (inner_stmt) != GIMPLE_OMP_FOR
		     ? gimple_omp_taskreg_clauses (inner_stmt)
		     : gimple_omp_for_clauses (inner_stmt);
      /* First two _looptemp_ clauses are for istart/iend, counts[0]
	 isn't supposed to be handled, as the inner loop doesn't
	 use it.  */
      tree innerc = find_omp_clause (clauses, OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      for (i = 0; i < fd->collapse; i++)
	{
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  gcc_assert (innerc);
	  if (i)
	    {
	      tree tem = OMP_CLAUSE_DECL (innerc);
	      tree t = fold_convert (TREE_TYPE (tem), counts[i]);
	      t = force_gimple_operand_gsi (gsi, t, false, NULL_TREE,
					    false, GSI_CONTINUE_LINKING);
	      gassign *stmt = gimple_build_assign (tem, t);
	      gsi_insert_after (gsi, stmt, GSI_CONTINUE_LINKING);
	    }
	}
      return;
    }

  tree type = TREE_TYPE (fd->loop.v);
  tree tem = create_tmp_reg (type, ".tem");
  gassign *stmt = gimple_build_assign (tem, startvar);
  gsi_insert_after (gsi, stmt, GSI_CONTINUE_LINKING);

  for (i = fd->collapse - 1; i >= 0; i--)
    {
      tree vtype = TREE_TYPE (fd->loops[i].v), itype, t;
      itype = vtype;
      if (POINTER_TYPE_P (vtype))
	itype = signed_type_for (vtype);
      if (i != 0)
	t = fold_build2 (TRUNC_MOD_EXPR, type, tem, counts[i]);
      else
	t = tem;
      t = fold_convert (itype, t);
      t = fold_build2 (MULT_EXPR, itype, t,
		       fold_convert (itype, fd->loops[i].step));
      if (POINTER_TYPE_P (vtype))
	t = fold_build_pointer_plus (fd->loops[i].n1, t);
      else
	t = fold_build2 (PLUS_EXPR, itype, fd->loops[i].n1, t);
      t = force_gimple_operand_gsi (gsi, t,
				    DECL_P (fd->loops[i].v)
				    && TREE_ADDRESSABLE (fd->loops[i].v),
				    NULL_TREE, false,
				    GSI_CONTINUE_LINKING);
      stmt = gimple_build_assign (fd->loops[i].v, t);
      gsi_insert_after (gsi, stmt, GSI_CONTINUE_LINKING);
      if (i != 0)
	{
	  t = fold_build2 (TRUNC_DIV_EXPR, type, tem, counts[i]);
	  t = force_gimple_operand_gsi (gsi, t, false, NULL_TREE,
					false, GSI_CONTINUE_LINKING);
	  stmt = gimple_build_assign (tem, t);
	  gsi_insert_after (gsi, stmt, GSI_CONTINUE_LINKING);
	}
    }
}


/* Helper function for expand_omp_for_*.  Generate code like:
    L10:
	V3 += STEP3;
	if (V3 cond3 N32) goto BODY_BB; else goto L11;
    L11:
	V3 = N31;
	V2 += STEP2;
	if (V2 cond2 N22) goto BODY_BB; else goto L12;
    L12:
	V2 = N21;
	V1 += STEP1;
	goto BODY_BB;  */

static basic_block
extract_omp_for_update_vars (struct omp_for_data *fd, basic_block cont_bb,
			     basic_block body_bb)
{
  basic_block last_bb, bb, collapse_bb = NULL;
  int i;
  gimple_stmt_iterator gsi;
  edge e;
  tree t;
  gimple *stmt;

  last_bb = cont_bb;
  for (i = fd->collapse - 1; i >= 0; i--)
    {
      tree vtype = TREE_TYPE (fd->loops[i].v);

      bb = create_empty_bb (last_bb);
      add_bb_to_loop (bb, last_bb->loop_father);
      gsi = gsi_start_bb (bb);

      if (i < fd->collapse - 1)
	{
	  e = make_edge (last_bb, bb, EDGE_FALSE_VALUE);
	  e->probability = REG_BR_PROB_BASE / 8;

	  t = fd->loops[i + 1].n1;
	  t = force_gimple_operand_gsi (&gsi, t,
					DECL_P (fd->loops[i + 1].v)
					&& TREE_ADDRESSABLE (fd->loops[i
								       + 1].v),
					NULL_TREE, false,
					GSI_CONTINUE_LINKING);
	  stmt = gimple_build_assign (fd->loops[i + 1].v, t);
	  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);
	}
      else
	collapse_bb = bb;

      set_immediate_dominator (CDI_DOMINATORS, bb, last_bb);

      if (POINTER_TYPE_P (vtype))
	t = fold_build_pointer_plus (fd->loops[i].v, fd->loops[i].step);
      else
	t = fold_build2 (PLUS_EXPR, vtype, fd->loops[i].v, fd->loops[i].step);
      t = force_gimple_operand_gsi (&gsi, t,
				    DECL_P (fd->loops[i].v)
				    && TREE_ADDRESSABLE (fd->loops[i].v),
				    NULL_TREE, false, GSI_CONTINUE_LINKING);
      stmt = gimple_build_assign (fd->loops[i].v, t);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      if (i > 0)
	{
	  t = fd->loops[i].n2;
	  t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					false, GSI_CONTINUE_LINKING);
	  tree v = fd->loops[i].v;
	  if (DECL_P (v) && TREE_ADDRESSABLE (v))
	    v = force_gimple_operand_gsi (&gsi, v, true, NULL_TREE,
					  false, GSI_CONTINUE_LINKING);
	  t = fold_build2 (fd->loops[i].cond_code, boolean_type_node, v, t);
	  stmt = gimple_build_cond_empty (t);
	  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);
	  e = make_edge (bb, body_bb, EDGE_TRUE_VALUE);
	  e->probability = REG_BR_PROB_BASE * 7 / 8;
	}
      else
	make_edge (bb, body_bb, EDGE_FALLTHRU);
      last_bb = bb;
    }

  return collapse_bb;
}


/* Expand #pragma omp ordered depend(source).  */

static void
expand_omp_ordered_source (gimple_stmt_iterator *gsi, struct omp_for_data *fd,
			   tree *counts, location_t loc)
{
  enum built_in_function source_ix
    = fd->iter_type == long_integer_type_node
      ? BUILT_IN_GOMP_DOACROSS_POST : BUILT_IN_GOMP_DOACROSS_ULL_POST;
  gimple *g
    = gimple_build_call (builtin_decl_explicit (source_ix), 1,
			 build_fold_addr_expr (counts[fd->ordered]));
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

/* Expand a single depend from #pragma omp ordered depend(sink:...).  */

static void
expand_omp_ordered_sink (gimple_stmt_iterator *gsi, struct omp_for_data *fd,
			 tree *counts, tree c, location_t loc)
{
  auto_vec<tree, 10> args;
  enum built_in_function sink_ix
    = fd->iter_type == long_integer_type_node
      ? BUILT_IN_GOMP_DOACROSS_WAIT : BUILT_IN_GOMP_DOACROSS_ULL_WAIT;
  tree t, off, coff = NULL_TREE, deps = OMP_CLAUSE_DECL (c), cond = NULL_TREE;
  int i;
  gimple_stmt_iterator gsi2 = *gsi;
  bool warned_step = false;

  for (i = 0; i < fd->ordered; i++)
    {
      off = TREE_PURPOSE (deps);
      if (!integer_zerop (off))
	{
	  gcc_assert (fd->loops[i].cond_code == LT_EXPR
		      || fd->loops[i].cond_code == GT_EXPR);
	  bool forward = fd->loops[i].cond_code == LT_EXPR;
	  if (forward ^ OMP_CLAUSE_DEPEND_SINK_NEGATIVE (deps))
	    warning_at (loc, 0, "%<depend(sink)%> clause waiting for "
				"lexically later iteration");
	  break;
	}
      deps = TREE_CHAIN (deps);
    }
  /* If all offsets corresponding to the collapsed loops are zero,
     this depend clause can be ignored.  FIXME: but there is still a
     flush needed.  We need to emit one __sync_synchronize () for it
     though (perhaps conditionally)?  Solve this together with the
     conservative dependence folding optimization.
  if (i >= fd->collapse)
    return;  */

  deps = OMP_CLAUSE_DECL (c);
  gsi_prev (&gsi2);
  edge e1 = split_block (gsi_bb (gsi2), gsi_stmt (gsi2));
  edge e2 = split_block_after_labels (e1->dest);

  *gsi = gsi_after_labels (e1->dest);
  for (i = 0; i < fd->ordered; i++)
    {
      tree itype = TREE_TYPE (fd->loops[i].v);
      if (POINTER_TYPE_P (itype))
	itype = sizetype;
      if (i)
	deps = TREE_CHAIN (deps);
      off = TREE_PURPOSE (deps);
      tree s = fold_convert_loc (loc, itype, fd->loops[i].step);

      if (integer_zerop (off))
	t = boolean_true_node;
      else
	{
	  tree a;
	  tree co = fold_convert_loc (loc, itype, off);
	  if (POINTER_TYPE_P (TREE_TYPE (fd->loops[i].v)))
	    {
	      if (OMP_CLAUSE_DEPEND_SINK_NEGATIVE (deps))
		co = fold_build1_loc (loc, NEGATE_EXPR, itype, co);
	      a = fold_build2_loc (loc, POINTER_PLUS_EXPR,
				   TREE_TYPE (fd->loops[i].v), fd->loops[i].v,
				   co);
	    }
	  else if (OMP_CLAUSE_DEPEND_SINK_NEGATIVE (deps))
	    a = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (fd->loops[i].v),
				 fd->loops[i].v, co);
	  else
	    a = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (fd->loops[i].v),
				 fd->loops[i].v, co);
	  if (fd->loops[i].cond_code == LT_EXPR)
	    {
	      if (OMP_CLAUSE_DEPEND_SINK_NEGATIVE (deps))
		t = fold_build2_loc (loc, GE_EXPR, boolean_type_node, a,
				     fd->loops[i].n1);
	      else
		t = fold_build2_loc (loc, LT_EXPR, boolean_type_node, a,
				     fd->loops[i].n2);
	    }
	  else if (OMP_CLAUSE_DEPEND_SINK_NEGATIVE (deps))
	    t = fold_build2_loc (loc, GT_EXPR, boolean_type_node, a,
				 fd->loops[i].n2);
	  else
	    t = fold_build2_loc (loc, LE_EXPR, boolean_type_node, a,
				 fd->loops[i].n1);
	}
      if (cond)
	cond = fold_build2_loc (loc, BIT_AND_EXPR, boolean_type_node, cond, t);
      else
	cond = t;

      off = fold_convert_loc (loc, itype, off);

      if (fd->loops[i].cond_code == LT_EXPR
	  ? !integer_onep (fd->loops[i].step)
	  : !integer_minus_onep (fd->loops[i].step))
	{
	  if (TYPE_UNSIGNED (itype) && fd->loops[i].cond_code == GT_EXPR)
	    t = fold_build2_loc (loc, TRUNC_MOD_EXPR, itype, off,
				 fold_build1_loc (loc, NEGATE_EXPR, itype,
						  s));
	  else
	    t = fold_build2_loc (loc, TRUNC_MOD_EXPR, itype, off, s);
	  t = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, t,
			       build_int_cst (itype, 0));
	  if (integer_zerop (t) && !warned_step)
	    {
	      warning_at (loc, 0, "%<depend(sink)%> refers to iteration never "
				  "in the iteration space");
	      warned_step = true;
	    }
	  cond = fold_build2_loc (loc, BIT_AND_EXPR, boolean_type_node,
				  cond, t);
	}

      if (i <= fd->collapse - 1 && fd->collapse > 1)
	t = fd->loop.v;
      else if (counts[i])
	t = counts[i];
      else
	{
	  t = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (fd->loops[i].v),
			       fd->loops[i].v, fd->loops[i].n1);
	  t = fold_convert_loc (loc, fd->iter_type, t);
	}
      if (TYPE_UNSIGNED (itype) && fd->loops[i].cond_code == GT_EXPR)
	off = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype, off,
			       fold_build1_loc (loc, NEGATE_EXPR, itype,
						s));
      else
	off = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype, off, s);
      if (OMP_CLAUSE_DEPEND_SINK_NEGATIVE (deps))
	off = fold_build1_loc (loc, NEGATE_EXPR, itype, off);
      off = fold_convert_loc (loc, fd->iter_type, off);
      if (i <= fd->collapse - 1 && fd->collapse > 1)
	{
	  if (i)
	    off = fold_build2_loc (loc, PLUS_EXPR, fd->iter_type, coff,
				   off);
	  if (i < fd->collapse - 1)
	    {
	      coff = fold_build2_loc (loc, MULT_EXPR, fd->iter_type, off,
				      counts[i]);
	      continue;
	    }
	}
      off = unshare_expr (off);
      t = fold_build2_loc (loc, PLUS_EXPR, fd->iter_type, t, off);
      t = force_gimple_operand_gsi (gsi, t, true, NULL_TREE,
				    true, GSI_SAME_STMT);
      args.safe_push (t);
    }
  gimple *g = gimple_build_call_vec (builtin_decl_explicit (sink_ix), args);
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  *gsi = gsi_last_bb (e1->src);
  cond = unshare_expr (cond);
  cond = force_gimple_operand_gsi (gsi, cond, true, NULL_TREE, false,
				   GSI_CONTINUE_LINKING);
  gsi_insert_after (gsi, gimple_build_cond_empty (cond), GSI_NEW_STMT);
  edge e3 = make_edge (e1->src, e2->dest, EDGE_FALSE_VALUE);
  e3->probability = REG_BR_PROB_BASE / 8;
  e1->probability = REG_BR_PROB_BASE - e3->probability;
  e1->flags = EDGE_TRUE_VALUE;
  set_immediate_dominator (CDI_DOMINATORS, e2->dest, e1->src);

  *gsi = gsi_after_labels (e2->dest);
}

/* Expand all #pragma omp ordered depend(source) and
   #pragma omp ordered depend(sink:...) constructs in the current
   #pragma omp for ordered(n) region.  */

static void
expand_omp_ordered_source_sink (struct omp_region *region,
				struct omp_for_data *fd, tree *counts,
				basic_block cont_bb)
{
  struct omp_region *inner;
  int i;
  for (i = fd->collapse - 1; i < fd->ordered; i++)
    if (i == fd->collapse - 1 && fd->collapse > 1)
      counts[i] = NULL_TREE;
    else if (i >= fd->collapse && !cont_bb)
      counts[i] = build_zero_cst (fd->iter_type);
    else if (!POINTER_TYPE_P (TREE_TYPE (fd->loops[i].v))
	     && integer_onep (fd->loops[i].step))
      counts[i] = NULL_TREE;
    else
      counts[i] = create_tmp_var (fd->iter_type, ".orditer");
  tree atype
    = build_array_type_nelts (fd->iter_type, fd->ordered - fd->collapse + 1);
  counts[fd->ordered] = create_tmp_var (atype, ".orditera");
  TREE_ADDRESSABLE (counts[fd->ordered]) = 1;

  for (inner = region->inner; inner; inner = inner->next)
    if (inner->type == GIMPLE_OMP_ORDERED)
      {
	gomp_ordered *ord_stmt = inner->ord_stmt;
	gimple_stmt_iterator gsi = gsi_for_stmt (ord_stmt);
	location_t loc = gimple_location (ord_stmt);
	tree c;
	for (c = gimple_omp_ordered_clauses (ord_stmt);
	     c; c = OMP_CLAUSE_CHAIN (c))
	  if (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE)
	    break;
	if (c)
	  expand_omp_ordered_source (&gsi, fd, counts, loc);
	for (c = gimple_omp_ordered_clauses (ord_stmt);
	     c; c = OMP_CLAUSE_CHAIN (c))
	  if (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK)
	    expand_omp_ordered_sink (&gsi, fd, counts, c, loc);
	gsi_remove (&gsi, true);
      }
}

/* Wrap the body into fd->ordered - fd->collapse loops that aren't
   collapsed.  */

static basic_block
expand_omp_for_ordered_loops (struct omp_for_data *fd, tree *counts,
			      basic_block cont_bb, basic_block body_bb,
			      bool ordered_lastprivate)
{
  if (fd->ordered == fd->collapse)
    return cont_bb;

  if (!cont_bb)
    {
      gimple_stmt_iterator gsi = gsi_after_labels (body_bb);
      for (int i = fd->collapse; i < fd->ordered; i++)
	{
	  tree type = TREE_TYPE (fd->loops[i].v);
	  tree n1 = fold_convert (type, fd->loops[i].n1);
	  expand_omp_build_assign (&gsi, fd->loops[i].v, n1);
	  tree aref = build4 (ARRAY_REF, fd->iter_type, counts[fd->ordered],
			      size_int (i - fd->collapse + 1),
			      NULL_TREE, NULL_TREE);
	  expand_omp_build_assign (&gsi, aref, build_zero_cst (fd->iter_type));
	}
      return NULL;
    }

  for (int i = fd->ordered - 1; i >= fd->collapse; i--)
    {
      tree t, type = TREE_TYPE (fd->loops[i].v);
      gimple_stmt_iterator gsi = gsi_after_labels (body_bb);
      expand_omp_build_assign (&gsi, fd->loops[i].v,
			       fold_convert (type, fd->loops[i].n1));
      if (counts[i])
	expand_omp_build_assign (&gsi, counts[i],
				 build_zero_cst (fd->iter_type));
      tree aref = build4 (ARRAY_REF, fd->iter_type, counts[fd->ordered],
			  size_int (i - fd->collapse + 1),
			  NULL_TREE, NULL_TREE);
      expand_omp_build_assign (&gsi, aref, build_zero_cst (fd->iter_type));
      if (!gsi_end_p (gsi))
	gsi_prev (&gsi);
      else
	gsi = gsi_last_bb (body_bb);
      edge e1 = split_block (body_bb, gsi_stmt (gsi));
      basic_block new_body = e1->dest;
      if (body_bb == cont_bb)
	cont_bb = new_body;
      edge e2 = NULL;
      basic_block new_header;
      if (EDGE_COUNT (cont_bb->preds) > 0)
	{
	  gsi = gsi_last_bb (cont_bb);
	  if (POINTER_TYPE_P (type))
	    t = fold_build_pointer_plus (fd->loops[i].v,
					 fold_convert (sizetype,
						       fd->loops[i].step));
	  else
	    t = fold_build2 (PLUS_EXPR, type, fd->loops[i].v,
			     fold_convert (type, fd->loops[i].step));
	  expand_omp_build_assign (&gsi, fd->loops[i].v, t);
	  if (counts[i])
	    {
	      t = fold_build2 (PLUS_EXPR, fd->iter_type, counts[i],
			       build_int_cst (fd->iter_type, 1));
	      expand_omp_build_assign (&gsi, counts[i], t);
	      t = counts[i];
	    }
	  else
	    {
	      t = fold_build2 (MINUS_EXPR, TREE_TYPE (fd->loops[i].v),
			       fd->loops[i].v, fd->loops[i].n1);
	      t = fold_convert (fd->iter_type, t);
	      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					    true, GSI_SAME_STMT);
	    }
	  aref = build4 (ARRAY_REF, fd->iter_type, counts[fd->ordered],
			 size_int (i - fd->collapse + 1),
			 NULL_TREE, NULL_TREE);
	  expand_omp_build_assign (&gsi, aref, t);
	  gsi_prev (&gsi);
	  e2 = split_block (cont_bb, gsi_stmt (gsi));
	  new_header = e2->dest;
	}
      else
	new_header = cont_bb;
      gsi = gsi_after_labels (new_header);
      tree v = force_gimple_operand_gsi (&gsi, fd->loops[i].v, true, NULL_TREE,
					 true, GSI_SAME_STMT);
      tree n2
	= force_gimple_operand_gsi (&gsi, fold_convert (type, fd->loops[i].n2),
				    true, NULL_TREE, true, GSI_SAME_STMT);
      t = build2 (fd->loops[i].cond_code, boolean_type_node, v, n2);
      gsi_insert_before (&gsi, gimple_build_cond_empty (t), GSI_NEW_STMT);
      edge e3 = split_block (new_header, gsi_stmt (gsi));
      cont_bb = e3->dest;
      remove_edge (e1);
      make_edge (body_bb, new_header, EDGE_FALLTHRU);
      e3->flags = EDGE_FALSE_VALUE;
      e3->probability = REG_BR_PROB_BASE / 8;
      e1 = make_edge (new_header, new_body, EDGE_TRUE_VALUE);
      e1->probability = REG_BR_PROB_BASE - e3->probability;

      set_immediate_dominator (CDI_DOMINATORS, new_header, body_bb);
      set_immediate_dominator (CDI_DOMINATORS, new_body, new_header);

      if (e2)
	{
	  struct loop *loop = alloc_loop ();
	  loop->header = new_header;
	  loop->latch = e2->src;
	  add_loop (loop, body_bb->loop_father);
	}
    }

  /* If there are any lastprivate clauses and it is possible some loops
     might have zero iterations, ensure all the decls are initialized,
     otherwise we could crash evaluating C++ class iterators with lastprivate
     clauses.  */
  bool need_inits = false;
  for (int i = fd->collapse; ordered_lastprivate && i < fd->ordered; i++)
    if (need_inits)
      {
	tree type = TREE_TYPE (fd->loops[i].v);
	gimple_stmt_iterator gsi = gsi_after_labels (body_bb);
	expand_omp_build_assign (&gsi, fd->loops[i].v,
				 fold_convert (type, fd->loops[i].n1));
      }
    else
      {
	tree type = TREE_TYPE (fd->loops[i].v);
	tree this_cond = fold_build2 (fd->loops[i].cond_code,
				      boolean_type_node,
				      fold_convert (type, fd->loops[i].n1),
				      fold_convert (type, fd->loops[i].n2));
	if (!integer_onep (this_cond))
	  need_inits = true;
      }

  return cont_bb;
}


/* A subroutine of expand_omp_for.  Generate code for a parallel
   loop with any schedule.  Given parameters:

	for (V = N1; V cond N2; V += STEP) BODY;

   where COND is "<" or ">", we generate pseudocode

	more = GOMP_loop_foo_start (N1, N2, STEP, CHUNK, &istart0, &iend0);
	if (more) goto L0; else goto L3;
    L0:
	V = istart0;
	iend = iend0;
    L1:
	BODY;
	V += STEP;
	if (V cond iend) goto L1; else goto L2;
    L2:
	if (GOMP_loop_foo_next (&istart0, &iend0)) goto L0; else goto L3;
    L3:

    If this is a combined omp parallel loop, instead of the call to
    GOMP_loop_foo_start, we call GOMP_loop_foo_next.
    If this is gimple_omp_for_combined_p loop, then instead of assigning
    V and iend in L0 we assign the first two _looptemp_ clause decls of the
    inner GIMPLE_OMP_FOR and V += STEP; and
    if (V cond iend) goto L1; else goto L2; are removed.

    For collapsed loops, given parameters:
      collapse(3)
      for (V1 = N11; V1 cond1 N12; V1 += STEP1)
	for (V2 = N21; V2 cond2 N22; V2 += STEP2)
	  for (V3 = N31; V3 cond3 N32; V3 += STEP3)
	    BODY;

    we generate pseudocode

	if (__builtin_expect (N32 cond3 N31, 0)) goto Z0;
	if (cond3 is <)
	  adj = STEP3 - 1;
	else
	  adj = STEP3 + 1;
	count3 = (adj + N32 - N31) / STEP3;
	if (__builtin_expect (N22 cond2 N21, 0)) goto Z0;
	if (cond2 is <)
	  adj = STEP2 - 1;
	else
	  adj = STEP2 + 1;
	count2 = (adj + N22 - N21) / STEP2;
	if (__builtin_expect (N12 cond1 N11, 0)) goto Z0;
	if (cond1 is <)
	  adj = STEP1 - 1;
	else
	  adj = STEP1 + 1;
	count1 = (adj + N12 - N11) / STEP1;
	count = count1 * count2 * count3;
	goto Z1;
    Z0:
	count = 0;
    Z1:
	more = GOMP_loop_foo_start (0, count, 1, CHUNK, &istart0, &iend0);
	if (more) goto L0; else goto L3;
    L0:
	V = istart0;
	T = V;
	V3 = N31 + (T % count3) * STEP3;
	T = T / count3;
	V2 = N21 + (T % count2) * STEP2;
	T = T / count2;
	V1 = N11 + T * STEP1;
	iend = iend0;
    L1:
	BODY;
	V += 1;
	if (V < iend) goto L10; else goto L2;
    L10:
	V3 += STEP3;
	if (V3 cond3 N32) goto L1; else goto L11;
    L11:
	V3 = N31;
	V2 += STEP2;
	if (V2 cond2 N22) goto L1; else goto L12;
    L12:
	V2 = N21;
	V1 += STEP1;
	goto L1;
    L2:
	if (GOMP_loop_foo_next (&istart0, &iend0)) goto L0; else goto L3;
    L3:

      */

static void
expand_omp_for_generic (struct omp_region *region,
			struct omp_for_data *fd,
			enum built_in_function start_fn,
			enum built_in_function next_fn,
			gimple *inner_stmt)
{
  tree type, istart0, iend0, iend;
  tree t, vmain, vback, bias = NULL_TREE;
  basic_block entry_bb, cont_bb, exit_bb, l0_bb, l1_bb, collapse_bb;
  basic_block l2_bb = NULL, l3_bb = NULL;
  gimple_stmt_iterator gsi;
  gassign *assign_stmt;
  bool in_combined_parallel = is_combined_parallel (region);
  bool broken_loop = region->cont == NULL;
  edge e, ne;
  tree *counts = NULL;
  int i;
  bool ordered_lastprivate = false;

  gcc_assert (!broken_loop || !in_combined_parallel);
  gcc_assert (fd->iter_type == long_integer_type_node
	      || !in_combined_parallel);

  entry_bb = region->entry;
  cont_bb = region->cont;
  collapse_bb = NULL;
  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2);
  gcc_assert (broken_loop
	      || BRANCH_EDGE (entry_bb)->dest == FALLTHRU_EDGE (cont_bb)->dest);
  l0_bb = split_edge (FALLTHRU_EDGE (entry_bb));
  l1_bb = single_succ (l0_bb);
  if (!broken_loop)
    {
      l2_bb = create_empty_bb (cont_bb);
      gcc_assert (BRANCH_EDGE (cont_bb)->dest == l1_bb
		  || (single_succ_edge (BRANCH_EDGE (cont_bb)->dest)->dest
		      == l1_bb));
      gcc_assert (EDGE_COUNT (cont_bb->succs) == 2);
    }
  else
    l2_bb = NULL;
  l3_bb = BRANCH_EDGE (entry_bb)->dest;
  exit_bb = region->exit;

  gsi = gsi_last_bb (entry_bb);

  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);
  if (fd->ordered
      && find_omp_clause (gimple_omp_for_clauses (gsi_stmt (gsi)),
			  OMP_CLAUSE_LASTPRIVATE))
    ordered_lastprivate = false;
  if (fd->collapse > 1 || fd->ordered)
    {
      int first_zero_iter1 = -1, first_zero_iter2 = -1;
      basic_block zero_iter1_bb = NULL, zero_iter2_bb = NULL, l2_dom_bb = NULL;

      counts = XALLOCAVEC (tree, fd->ordered ? fd->ordered + 1 : fd->collapse);
      expand_omp_for_init_counts (fd, &gsi, entry_bb, counts,
				  zero_iter1_bb, first_zero_iter1,
				  zero_iter2_bb, first_zero_iter2, l2_dom_bb);

      if (zero_iter1_bb)
	{
	  /* Some counts[i] vars might be uninitialized if
	     some loop has zero iterations.  But the body shouldn't
	     be executed in that case, so just avoid uninit warnings.  */
	  for (i = first_zero_iter1;
	       i < (fd->ordered ? fd->ordered : fd->collapse); i++)
	    if (SSA_VAR_P (counts[i]))
	      TREE_NO_WARNING (counts[i]) = 1;
	  gsi_prev (&gsi);
	  e = split_block (entry_bb, gsi_stmt (gsi));
	  entry_bb = e->dest;
	  make_edge (zero_iter1_bb, entry_bb, EDGE_FALLTHRU);
	  gsi = gsi_last_bb (entry_bb);
	  set_immediate_dominator (CDI_DOMINATORS, entry_bb,
				   get_immediate_dominator (CDI_DOMINATORS,
							    zero_iter1_bb));
	}
      if (zero_iter2_bb)
	{
	  /* Some counts[i] vars might be uninitialized if
	     some loop has zero iterations.  But the body shouldn't
	     be executed in that case, so just avoid uninit warnings.  */
	  for (i = first_zero_iter2; i < fd->ordered; i++)
	    if (SSA_VAR_P (counts[i]))
	      TREE_NO_WARNING (counts[i]) = 1;
	  if (zero_iter1_bb)
	    make_edge (zero_iter2_bb, entry_bb, EDGE_FALLTHRU);
	  else
	    {
	      gsi_prev (&gsi);
	      e = split_block (entry_bb, gsi_stmt (gsi));
	      entry_bb = e->dest;
	      make_edge (zero_iter2_bb, entry_bb, EDGE_FALLTHRU);
	      gsi = gsi_last_bb (entry_bb);
	      set_immediate_dominator (CDI_DOMINATORS, entry_bb,
				       get_immediate_dominator
					 (CDI_DOMINATORS, zero_iter2_bb));
	    }
	}
      if (fd->collapse == 1)
	{
	  counts[0] = fd->loop.n2;
	  fd->loop = fd->loops[0];
	}
    }

  type = TREE_TYPE (fd->loop.v);
  istart0 = create_tmp_var (fd->iter_type, ".istart0");
  iend0 = create_tmp_var (fd->iter_type, ".iend0");
  TREE_ADDRESSABLE (istart0) = 1;
  TREE_ADDRESSABLE (iend0) = 1;

  /* See if we need to bias by LLONG_MIN.  */
  if (fd->iter_type == long_long_unsigned_type_node
      && TREE_CODE (type) == INTEGER_TYPE
      && !TYPE_UNSIGNED (type)
      && fd->ordered == 0)
    {
      tree n1, n2;

      if (fd->loop.cond_code == LT_EXPR)
	{
	  n1 = fd->loop.n1;
	  n2 = fold_build2 (PLUS_EXPR, type, fd->loop.n2, fd->loop.step);
	}
      else
	{
	  n1 = fold_build2 (MINUS_EXPR, type, fd->loop.n2, fd->loop.step);
	  n2 = fd->loop.n1;
	}
      if (TREE_CODE (n1) != INTEGER_CST
	  || TREE_CODE (n2) != INTEGER_CST
	  || ((tree_int_cst_sgn (n1) < 0) ^ (tree_int_cst_sgn (n2) < 0)))
	bias = fold_convert (fd->iter_type, TYPE_MIN_VALUE (type));
    }

  gimple_stmt_iterator gsif = gsi;
  gsi_prev (&gsif);

  tree arr = NULL_TREE;
  if (in_combined_parallel)
    {
      gcc_assert (fd->ordered == 0);
      /* In a combined parallel loop, emit a call to
	 GOMP_loop_foo_next.  */
      t = build_call_expr (builtin_decl_explicit (next_fn), 2,
			   build_fold_addr_expr (istart0),
			   build_fold_addr_expr (iend0));
    }
  else
    {
      tree t0, t1, t2, t3, t4;
      /* If this is not a combined parallel loop, emit a call to
	 GOMP_loop_foo_start in ENTRY_BB.  */
      t4 = build_fold_addr_expr (iend0);
      t3 = build_fold_addr_expr (istart0);
      if (fd->ordered)
	{
	  t0 = build_int_cst (unsigned_type_node,
			      fd->ordered - fd->collapse + 1);
	  arr = create_tmp_var (build_array_type_nelts (fd->iter_type,
							fd->ordered
							- fd->collapse + 1),
				".omp_counts");
	  DECL_NAMELESS (arr) = 1;
	  TREE_ADDRESSABLE (arr) = 1;
	  TREE_STATIC (arr) = 1;
	  vec<constructor_elt, va_gc> *v;
	  vec_alloc (v, fd->ordered - fd->collapse + 1);
	  int idx;

	  for (idx = 0; idx < fd->ordered - fd->collapse + 1; idx++)
	    {
	      tree c;
	      if (idx == 0 && fd->collapse > 1)
		c = fd->loop.n2;
	      else
		c = counts[idx + fd->collapse - 1];
	      tree purpose = size_int (idx);
	      CONSTRUCTOR_APPEND_ELT (v, purpose, c);
	      if (TREE_CODE (c) != INTEGER_CST)
		TREE_STATIC (arr) = 0;
	    }

	  DECL_INITIAL (arr) = build_constructor (TREE_TYPE (arr), v);
	  if (!TREE_STATIC (arr))
	    force_gimple_operand_gsi (&gsi, build1 (DECL_EXPR,
						    void_type_node, arr),
				      true, NULL_TREE, true, GSI_SAME_STMT);
	  t1 = build_fold_addr_expr (arr);
	  t2 = NULL_TREE;
	}
      else
	{
	  t2 = fold_convert (fd->iter_type, fd->loop.step);
	  t1 = fd->loop.n2;
	  t0 = fd->loop.n1;
	  if (gimple_omp_for_combined_into_p (fd->for_stmt))
	    {
	      tree innerc
		= find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				   OMP_CLAUSE__LOOPTEMP_);
	      gcc_assert (innerc);
	      t0 = OMP_CLAUSE_DECL (innerc);
	      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
					OMP_CLAUSE__LOOPTEMP_);
	      gcc_assert (innerc);
	      t1 = OMP_CLAUSE_DECL (innerc);
	    }
	  if (POINTER_TYPE_P (TREE_TYPE (t0))
	      && TYPE_PRECISION (TREE_TYPE (t0))
		 != TYPE_PRECISION (fd->iter_type))
	    {
	      /* Avoid casting pointers to integer of a different size.  */
	      tree itype = signed_type_for (type);
	      t1 = fold_convert (fd->iter_type, fold_convert (itype, t1));
	      t0 = fold_convert (fd->iter_type, fold_convert (itype, t0));
	    }
	  else
	    {
	      t1 = fold_convert (fd->iter_type, t1);
	      t0 = fold_convert (fd->iter_type, t0);
	    }
	  if (bias)
	    {
	      t1 = fold_build2 (PLUS_EXPR, fd->iter_type, t1, bias);
	      t0 = fold_build2 (PLUS_EXPR, fd->iter_type, t0, bias);
	    }
	}
      if (fd->iter_type == long_integer_type_node || fd->ordered)
	{
	  if (fd->chunk_size)
	    {
	      t = fold_convert (fd->iter_type, fd->chunk_size);
	      t = omp_adjust_chunk_size (t, fd->simd_schedule);
	      if (fd->ordered)
		t = build_call_expr (builtin_decl_explicit (start_fn),
				     5, t0, t1, t, t3, t4);
	      else
		t = build_call_expr (builtin_decl_explicit (start_fn),
				     6, t0, t1, t2, t, t3, t4);
	    }
	  else if (fd->ordered)
	    t = build_call_expr (builtin_decl_explicit (start_fn),
				 4, t0, t1, t3, t4);
	  else
	    t = build_call_expr (builtin_decl_explicit (start_fn),
				 5, t0, t1, t2, t3, t4);
	}
      else
	{
	  tree t5;
	  tree c_bool_type;
	  tree bfn_decl;

	  /* The GOMP_loop_ull_*start functions have additional boolean
	     argument, true for < loops and false for > loops.
	     In Fortran, the C bool type can be different from
	     boolean_type_node.  */
	  bfn_decl = builtin_decl_explicit (start_fn);
	  c_bool_type = TREE_TYPE (TREE_TYPE (bfn_decl));
	  t5 = build_int_cst (c_bool_type,
			      fd->loop.cond_code == LT_EXPR ? 1 : 0);
	  if (fd->chunk_size)
	    {
	      tree bfn_decl = builtin_decl_explicit (start_fn);
	      t = fold_convert (fd->iter_type, fd->chunk_size);
	      t = omp_adjust_chunk_size (t, fd->simd_schedule);
	      t = build_call_expr (bfn_decl, 7, t5, t0, t1, t2, t, t3, t4);
	    }
	  else
	    t = build_call_expr (builtin_decl_explicit (start_fn),
				 6, t5, t0, t1, t2, t3, t4);
	}
    }
  if (TREE_TYPE (t) != boolean_type_node)
    t = fold_build2 (NE_EXPR, boolean_type_node,
		     t, build_int_cst (TREE_TYPE (t), 0));
  t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
			       	true, GSI_SAME_STMT);
  if (arr && !TREE_STATIC (arr))
    {
      tree clobber = build_constructor (TREE_TYPE (arr), NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gsi_insert_before (&gsi, gimple_build_assign (arr, clobber),
			 GSI_SAME_STMT);
    }
  gsi_insert_after (&gsi, gimple_build_cond_empty (t), GSI_SAME_STMT);

  /* Remove the GIMPLE_OMP_FOR statement.  */
  gsi_remove (&gsi, true);

  if (gsi_end_p (gsif))
    gsif = gsi_after_labels (gsi_bb (gsif));
  gsi_next (&gsif);

  /* Iteration setup for sequential loop goes in L0_BB.  */
  tree startvar = fd->loop.v;
  tree endvar = NULL_TREE;

  if (gimple_omp_for_combined_p (fd->for_stmt))
    {
      gcc_assert (gimple_code (inner_stmt) == GIMPLE_OMP_FOR
		  && gimple_omp_for_kind (inner_stmt)
		     == GF_OMP_FOR_KIND_SIMD);
      tree innerc = find_omp_clause (gimple_omp_for_clauses (inner_stmt),
				     OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      startvar = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      endvar = OMP_CLAUSE_DECL (innerc);
    }

  gsi = gsi_start_bb (l0_bb);
  t = istart0;
  if (fd->ordered && fd->collapse == 1)
    t = fold_build2 (MULT_EXPR, fd->iter_type, t,
		     fold_convert (fd->iter_type, fd->loop.step));
  else if (bias)
    t = fold_build2 (MINUS_EXPR, fd->iter_type, t, bias);
  if (fd->ordered && fd->collapse == 1)
    {
      if (POINTER_TYPE_P (TREE_TYPE (startvar)))
	t = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (startvar),
			 fd->loop.n1, fold_convert (sizetype, t));
      else
	{
	  t = fold_convert (TREE_TYPE (startvar), t);
	  t = fold_build2 (PLUS_EXPR, TREE_TYPE (startvar),
			   fd->loop.n1, t);
	}
    }
  else
    {
      if (POINTER_TYPE_P (TREE_TYPE (startvar)))
	t = fold_convert (signed_type_for (TREE_TYPE (startvar)), t);
      t = fold_convert (TREE_TYPE (startvar), t);
    }
  t = force_gimple_operand_gsi (&gsi, t,
				DECL_P (startvar)
				&& TREE_ADDRESSABLE (startvar),
				NULL_TREE, false, GSI_CONTINUE_LINKING);
  assign_stmt = gimple_build_assign (startvar, t);
  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);

  t = iend0;
  if (fd->ordered && fd->collapse == 1)
    t = fold_build2 (MULT_EXPR, fd->iter_type, t,
		     fold_convert (fd->iter_type, fd->loop.step));
  else if (bias)
    t = fold_build2 (MINUS_EXPR, fd->iter_type, t, bias);
  if (fd->ordered && fd->collapse == 1)
    {
      if (POINTER_TYPE_P (TREE_TYPE (startvar)))
	t = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (startvar),
			 fd->loop.n1, fold_convert (sizetype, t));
      else
	{
	  t = fold_convert (TREE_TYPE (startvar), t);
	  t = fold_build2 (PLUS_EXPR, TREE_TYPE (startvar),
			   fd->loop.n1, t);
	}
    }
  else
    {
      if (POINTER_TYPE_P (TREE_TYPE (startvar)))
	t = fold_convert (signed_type_for (TREE_TYPE (startvar)), t);
      t = fold_convert (TREE_TYPE (startvar), t);
    }
  iend = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				   false, GSI_CONTINUE_LINKING);
  if (endvar)
    {
      assign_stmt = gimple_build_assign (endvar, iend);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
      if (useless_type_conversion_p (TREE_TYPE (fd->loop.v), TREE_TYPE (iend)))
	assign_stmt = gimple_build_assign (fd->loop.v, iend);
      else
	assign_stmt = gimple_build_assign (fd->loop.v, NOP_EXPR, iend);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
    }
  /* Handle linear clause adjustments.  */
  tree itercnt = NULL_TREE;
  if (gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_FOR)
    for (tree c = gimple_omp_for_clauses (fd->for_stmt);
	 c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
	{
	  tree d = OMP_CLAUSE_DECL (c);
	  bool is_ref = is_reference (d);
	  tree t = d, a, dest;
	  if (is_ref)
	    t = build_simple_mem_ref_loc (OMP_CLAUSE_LOCATION (c), t);
	  tree type = TREE_TYPE (t);
	  if (POINTER_TYPE_P (type))
	    type = sizetype;
	  dest = unshare_expr (t);
	  tree v = create_tmp_var (TREE_TYPE (t), NULL);
	  expand_omp_build_assign (&gsif, v, t);
	  if (itercnt == NULL_TREE)
	    {
	      itercnt = startvar;
	      tree n1 = fd->loop.n1;
	      if (POINTER_TYPE_P (TREE_TYPE (itercnt)))
		{
		  itercnt
		    = fold_convert (signed_type_for (TREE_TYPE (itercnt)),
				    itercnt);
		  n1 = fold_convert (TREE_TYPE (itercnt), n1);
		}
	      itercnt = fold_build2 (MINUS_EXPR, TREE_TYPE (itercnt),
				     itercnt, n1);
	      itercnt = fold_build2 (EXACT_DIV_EXPR, TREE_TYPE (itercnt),
				     itercnt, fd->loop.step);
	      itercnt = force_gimple_operand_gsi (&gsi, itercnt, true,
						  NULL_TREE, false,
						  GSI_CONTINUE_LINKING);
	    }
	  a = fold_build2 (MULT_EXPR, type,
			   fold_convert (type, itercnt),
			   fold_convert (type, OMP_CLAUSE_LINEAR_STEP (c)));
	  t = fold_build2 (type == TREE_TYPE (t) ? PLUS_EXPR
			   : POINTER_PLUS_EXPR, TREE_TYPE (t), v, a);
	  t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					false, GSI_CONTINUE_LINKING);
	  assign_stmt = gimple_build_assign (dest, t);
	  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
	}
  if (fd->collapse > 1)
    expand_omp_for_init_vars (fd, &gsi, counts, inner_stmt, startvar);

  if (fd->ordered)
    {
      /* Until now, counts array contained number of iterations or
	 variable containing it for ith loop.  From now on, we need
	 those counts only for collapsed loops, and only for the 2nd
	 till the last collapsed one.  Move those one element earlier,
	 we'll use counts[fd->collapse - 1] for the first source/sink
	 iteration counter and so on and counts[fd->ordered]
	 as the array holding the current counter values for
	 depend(source).  */
      if (fd->collapse > 1)
	memmove (counts, counts + 1, (fd->collapse - 1) * sizeof (counts[0]));
      if (broken_loop)
	{
	  int i;
	  for (i = fd->collapse; i < fd->ordered; i++)
	    {
	      tree type = TREE_TYPE (fd->loops[i].v);
	      tree this_cond
		= fold_build2 (fd->loops[i].cond_code, boolean_type_node,
			       fold_convert (type, fd->loops[i].n1),
			       fold_convert (type, fd->loops[i].n2));
	      if (!integer_onep (this_cond))
		break;
	    }
	  if (i < fd->ordered)
	    {
	      cont_bb
		= create_empty_bb (EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb);
	      add_bb_to_loop (cont_bb, l1_bb->loop_father);
	      gimple_stmt_iterator gsi = gsi_after_labels (cont_bb);
	      gimple *g = gimple_build_omp_continue (fd->loop.v, fd->loop.v);
	      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	      make_edge (cont_bb, l3_bb, EDGE_FALLTHRU);
	      make_edge (cont_bb, l1_bb, 0);
	      l2_bb = create_empty_bb (cont_bb);
	      broken_loop = false;
	    }
	}
      expand_omp_ordered_source_sink (region, fd, counts, cont_bb);
      cont_bb = expand_omp_for_ordered_loops (fd, counts, cont_bb, l1_bb,
					      ordered_lastprivate);
      if (counts[fd->collapse - 1])
	{
	  gcc_assert (fd->collapse == 1);
	  gsi = gsi_last_bb (l0_bb);
	  expand_omp_build_assign (&gsi, counts[fd->collapse - 1],
				   istart0, true);
	  gsi = gsi_last_bb (cont_bb);
	  t = fold_build2 (PLUS_EXPR, fd->iter_type, counts[fd->collapse - 1],
			   build_int_cst (fd->iter_type, 1));
	  expand_omp_build_assign (&gsi, counts[fd->collapse - 1], t);
	  tree aref = build4 (ARRAY_REF, fd->iter_type, counts[fd->ordered],
			      size_zero_node, NULL_TREE, NULL_TREE);
	  expand_omp_build_assign (&gsi, aref, counts[fd->collapse - 1]);
	  t = counts[fd->collapse - 1];
	}
      else if (fd->collapse > 1)
	t = fd->loop.v;
      else
	{
	  t = fold_build2 (MINUS_EXPR, TREE_TYPE (fd->loops[0].v),
			   fd->loops[0].v, fd->loops[0].n1);
	  t = fold_convert (fd->iter_type, t);
	}
      gsi = gsi_last_bb (l0_bb);
      tree aref = build4 (ARRAY_REF, fd->iter_type, counts[fd->ordered],
			  size_zero_node, NULL_TREE, NULL_TREE);
      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				    false, GSI_CONTINUE_LINKING);
      expand_omp_build_assign (&gsi, aref, t, true);
    }

  if (!broken_loop)
    {
      /* Code to control the increment and predicate for the sequential
	 loop goes in the CONT_BB.  */
      gsi = gsi_last_bb (cont_bb);
      gomp_continue *cont_stmt = as_a <gomp_continue *> (gsi_stmt (gsi));
      gcc_assert (gimple_code (cont_stmt) == GIMPLE_OMP_CONTINUE);
      vmain = gimple_omp_continue_control_use (cont_stmt);
      vback = gimple_omp_continue_control_def (cont_stmt);

      if (!gimple_omp_for_combined_p (fd->for_stmt))
	{
	  if (POINTER_TYPE_P (type))
	    t = fold_build_pointer_plus (vmain, fd->loop.step);
	  else
	    t = fold_build2 (PLUS_EXPR, type, vmain, fd->loop.step);
	  t = force_gimple_operand_gsi (&gsi, t,
					DECL_P (vback)
					&& TREE_ADDRESSABLE (vback),
					NULL_TREE, true, GSI_SAME_STMT);
	  assign_stmt = gimple_build_assign (vback, t);
	  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

	  if (fd->ordered && counts[fd->collapse - 1] == NULL_TREE)
	    {
	      if (fd->collapse > 1)
		t = fd->loop.v;
	      else
		{
		  t = fold_build2 (MINUS_EXPR, TREE_TYPE (fd->loops[0].v),
				   fd->loops[0].v, fd->loops[0].n1);
		  t = fold_convert (fd->iter_type, t);
		}
	      tree aref = build4 (ARRAY_REF, fd->iter_type,
				  counts[fd->ordered], size_zero_node,
				  NULL_TREE, NULL_TREE);
	      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					    true, GSI_SAME_STMT);
	      expand_omp_build_assign (&gsi, aref, t);
	    }

	  t = build2 (fd->loop.cond_code, boolean_type_node,
		      DECL_P (vback) && TREE_ADDRESSABLE (vback) ? t : vback,
		      iend);
	  gcond *cond_stmt = gimple_build_cond_empty (t);
	  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);
	}

      /* Remove GIMPLE_OMP_CONTINUE.  */
      gsi_remove (&gsi, true);

      if (fd->collapse > 1 && !gimple_omp_for_combined_p (fd->for_stmt))
	collapse_bb = extract_omp_for_update_vars (fd, cont_bb, l1_bb);

      /* Emit code to get the next parallel iteration in L2_BB.  */
      gsi = gsi_start_bb (l2_bb);

      t = build_call_expr (builtin_decl_explicit (next_fn), 2,
			   build_fold_addr_expr (istart0),
			   build_fold_addr_expr (iend0));
      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				    false, GSI_CONTINUE_LINKING);
      if (TREE_TYPE (t) != boolean_type_node)
	t = fold_build2 (NE_EXPR, boolean_type_node,
			 t, build_int_cst (TREE_TYPE (t), 0));
      gcond *cond_stmt = gimple_build_cond_empty (t);
      gsi_insert_after (&gsi, cond_stmt, GSI_CONTINUE_LINKING);
    }

  /* Add the loop cleanup function.  */
  gsi = gsi_last_bb (exit_bb);
  if (gimple_omp_return_nowait_p (gsi_stmt (gsi)))
    t = builtin_decl_explicit (BUILT_IN_GOMP_LOOP_END_NOWAIT);
  else if (gimple_omp_return_lhs (gsi_stmt (gsi)))
    t = builtin_decl_explicit (BUILT_IN_GOMP_LOOP_END_CANCEL);
  else
    t = builtin_decl_explicit (BUILT_IN_GOMP_LOOP_END);
  gcall *call_stmt = gimple_build_call (t, 0);
  if (gimple_omp_return_lhs (gsi_stmt (gsi)))
    gimple_call_set_lhs (call_stmt, gimple_omp_return_lhs (gsi_stmt (gsi)));
  gsi_insert_after (&gsi, call_stmt, GSI_SAME_STMT);
  if (fd->ordered)
    {
      tree arr = counts[fd->ordered];
      tree clobber = build_constructor (TREE_TYPE (arr), NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gsi_insert_after (&gsi, gimple_build_assign (arr, clobber),
			GSI_SAME_STMT);
    }
  gsi_remove (&gsi, true);

  /* Connect the new blocks.  */
  find_edge (entry_bb, l0_bb)->flags = EDGE_TRUE_VALUE;
  find_edge (entry_bb, l3_bb)->flags = EDGE_FALSE_VALUE;

  if (!broken_loop)
    {
      gimple_seq phis;

      e = find_edge (cont_bb, l3_bb);
      ne = make_edge (l2_bb, l3_bb, EDGE_FALSE_VALUE);

      phis = phi_nodes (l3_bb);
      for (gsi = gsi_start (phis); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *phi = gsi_stmt (gsi);
	  SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, ne),
		   PHI_ARG_DEF_FROM_EDGE (phi, e));
	}
      remove_edge (e);

      make_edge (cont_bb, l2_bb, EDGE_FALSE_VALUE);
      e = find_edge (cont_bb, l1_bb);
      if (e == NULL)
	{
	  e = BRANCH_EDGE (cont_bb);
	  gcc_assert (single_succ (e->dest) == l1_bb);
	}
      if (gimple_omp_for_combined_p (fd->for_stmt))
	{
	  remove_edge (e);
	  e = NULL;
	}
      else if (fd->collapse > 1)
	{
	  remove_edge (e);
	  e = make_edge (cont_bb, collapse_bb, EDGE_TRUE_VALUE);
	}
      else
	e->flags = EDGE_TRUE_VALUE;
      if (e)
	{
	  e->probability = REG_BR_PROB_BASE * 7 / 8;
	  find_edge (cont_bb, l2_bb)->probability = REG_BR_PROB_BASE / 8;
	}
      else
	{
	  e = find_edge (cont_bb, l2_bb);
	  e->flags = EDGE_FALLTHRU;
	}
      make_edge (l2_bb, l0_bb, EDGE_TRUE_VALUE);

      if (gimple_in_ssa_p (cfun))
	{
	  /* Add phis to the outer loop that connect to the phis in the inner,
	     original loop, and move the loop entry value of the inner phi to
	     the loop entry value of the outer phi.  */
	  gphi_iterator psi;
	  for (psi = gsi_start_phis (l3_bb); !gsi_end_p (psi); gsi_next (&psi))
	    {
	      source_location locus;
	      gphi *nphi;
	      gphi *exit_phi = psi.phi ();

	      edge l2_to_l3 = find_edge (l2_bb, l3_bb);
	      tree exit_res = PHI_ARG_DEF_FROM_EDGE (exit_phi, l2_to_l3);

	      basic_block latch = BRANCH_EDGE (cont_bb)->dest;
	      edge latch_to_l1 = find_edge (latch, l1_bb);
	      gphi *inner_phi
		= find_phi_with_arg_on_edge (exit_res, latch_to_l1);

	      tree t = gimple_phi_result (exit_phi);
	      tree new_res = copy_ssa_name (t, NULL);
	      nphi = create_phi_node (new_res, l0_bb);

	      edge l0_to_l1 = find_edge (l0_bb, l1_bb);
	      t = PHI_ARG_DEF_FROM_EDGE (inner_phi, l0_to_l1);
	      locus = gimple_phi_arg_location_from_edge (inner_phi, l0_to_l1);
	      edge entry_to_l0 = find_edge (entry_bb, l0_bb);
	      add_phi_arg (nphi, t, entry_to_l0, locus);

	      edge l2_to_l0 = find_edge (l2_bb, l0_bb);
	      add_phi_arg (nphi, exit_res, l2_to_l0, UNKNOWN_LOCATION);

	      add_phi_arg (inner_phi, new_res, l0_to_l1, UNKNOWN_LOCATION);
	    };
	}

      set_immediate_dominator (CDI_DOMINATORS, l2_bb,
			       recompute_dominator (CDI_DOMINATORS, l2_bb));
      set_immediate_dominator (CDI_DOMINATORS, l3_bb,
			       recompute_dominator (CDI_DOMINATORS, l3_bb));
      set_immediate_dominator (CDI_DOMINATORS, l0_bb,
			       recompute_dominator (CDI_DOMINATORS, l0_bb));
      set_immediate_dominator (CDI_DOMINATORS, l1_bb,
			       recompute_dominator (CDI_DOMINATORS, l1_bb));

      /* We enter expand_omp_for_generic with a loop.  This original loop may
	 have its own loop struct, or it may be part of an outer loop struct
	 (which may be the fake loop).  */
      struct loop *outer_loop = entry_bb->loop_father;
      bool orig_loop_has_loop_struct = l1_bb->loop_father != outer_loop;

      add_bb_to_loop (l2_bb, outer_loop);

      /* We've added a new loop around the original loop.  Allocate the
	 corresponding loop struct.  */
      struct loop *new_loop = alloc_loop ();
      new_loop->header = l0_bb;
      new_loop->latch = l2_bb;
      add_loop (new_loop, outer_loop);

      /* Allocate a loop structure for the original loop unless we already
	 had one.  */
      if (!orig_loop_has_loop_struct
	  && !gimple_omp_for_combined_p (fd->for_stmt))
	{
	  struct loop *orig_loop = alloc_loop ();
	  orig_loop->header = l1_bb;
	  /* The loop may have multiple latches.  */
	  add_loop (orig_loop, new_loop);
	}
    }
}


/* A subroutine of expand_omp_for.  Generate code for a parallel
   loop with static schedule and no specified chunk size.  Given
   parameters:

	for (V = N1; V cond N2; V += STEP) BODY;

   where COND is "<" or ">", we generate pseudocode

	if ((__typeof (V)) -1 > 0 && N2 cond N1) goto L2;
	if (cond is <)
	  adj = STEP - 1;
	else
	  adj = STEP + 1;
	if ((__typeof (V)) -1 > 0 && cond is >)
	  n = -(adj + N2 - N1) / -STEP;
	else
	  n = (adj + N2 - N1) / STEP;
	q = n / nthreads;
	tt = n % nthreads;
	if (threadid < tt) goto L3; else goto L4;
    L3:
	tt = 0;
	q = q + 1;
    L4:
	s0 = q * threadid + tt;
	e0 = s0 + q;
	V = s0 * STEP + N1;
	if (s0 >= e0) goto L2; else goto L0;
    L0:
	e = e0 * STEP + N1;
    L1:
	BODY;
	V += STEP;
	if (V cond e) goto L1;
    L2:
*/

static void
expand_omp_for_static_nochunk (struct omp_region *region,
			       struct omp_for_data *fd,
			       gimple *inner_stmt)
{
  tree n, q, s0, e0, e, t, tt, nthreads, threadid;
  tree type, itype, vmain, vback;
  basic_block entry_bb, second_bb, third_bb, exit_bb, seq_start_bb;
  basic_block body_bb, cont_bb, collapse_bb = NULL;
  basic_block fin_bb;
  gimple_stmt_iterator gsi;
  edge ep;
  bool broken_loop = region->cont == NULL;
  tree *counts = NULL;
  tree n1, n2, step;

  itype = type = TREE_TYPE (fd->loop.v);
  if (POINTER_TYPE_P (type))
    itype = signed_type_for (type);

  entry_bb = region->entry;
  cont_bb = region->cont;
  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2);
  fin_bb = BRANCH_EDGE (entry_bb)->dest;
  gcc_assert (broken_loop
	      || (fin_bb == FALLTHRU_EDGE (cont_bb)->dest));
  seq_start_bb = split_edge (FALLTHRU_EDGE (entry_bb));
  body_bb = single_succ (seq_start_bb);
  if (!broken_loop)
    {
      gcc_assert (BRANCH_EDGE (cont_bb)->dest == body_bb
		  || single_succ (BRANCH_EDGE (cont_bb)->dest) == body_bb);
      gcc_assert (EDGE_COUNT (cont_bb->succs) == 2);
    }
  exit_bb = region->exit;

  /* Iteration space partitioning goes in ENTRY_BB.  */
  gsi = gsi_last_bb (entry_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);

  if (fd->collapse > 1)
    {
      int first_zero_iter = -1, dummy = -1;
      basic_block l2_dom_bb = NULL, dummy_bb = NULL;

      counts = XALLOCAVEC (tree, fd->collapse);
      expand_omp_for_init_counts (fd, &gsi, entry_bb, counts,
				  fin_bb, first_zero_iter,
				  dummy_bb, dummy, l2_dom_bb);
      t = NULL_TREE;
    }
  else if (gimple_omp_for_combined_into_p (fd->for_stmt))
    t = integer_one_node;
  else
    t = fold_binary (fd->loop.cond_code, boolean_type_node,
		     fold_convert (type, fd->loop.n1),
		     fold_convert (type, fd->loop.n2));
  if (fd->collapse == 1
      && TYPE_UNSIGNED (type)
      && (t == NULL_TREE || !integer_onep (t)))
    {
      n1 = fold_convert (type, unshare_expr (fd->loop.n1));
      n1 = force_gimple_operand_gsi (&gsi, n1, true, NULL_TREE,
				     true, GSI_SAME_STMT);
      n2 = fold_convert (type, unshare_expr (fd->loop.n2));
      n2 = force_gimple_operand_gsi (&gsi, n2, true, NULL_TREE,
				     true, GSI_SAME_STMT);
      gcond *cond_stmt = gimple_build_cond (fd->loop.cond_code, n1, n2,
						 NULL_TREE, NULL_TREE);
      gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);
      if (walk_tree (gimple_cond_lhs_ptr (cond_stmt),
		     expand_omp_regimplify_p, NULL, NULL)
	  || walk_tree (gimple_cond_rhs_ptr (cond_stmt),
			expand_omp_regimplify_p, NULL, NULL))
	{
	  gsi = gsi_for_stmt (cond_stmt);
	  gimple_regimplify_operands (cond_stmt, &gsi);
	}
      ep = split_block (entry_bb, cond_stmt);
      ep->flags = EDGE_TRUE_VALUE;
      entry_bb = ep->dest;
      ep->probability = REG_BR_PROB_BASE - (REG_BR_PROB_BASE / 2000 - 1);
      ep = make_edge (ep->src, fin_bb, EDGE_FALSE_VALUE);
      ep->probability = REG_BR_PROB_BASE / 2000 - 1;
      if (gimple_in_ssa_p (cfun))
	{
	  int dest_idx = find_edge (entry_bb, fin_bb)->dest_idx;
	  for (gphi_iterator gpi = gsi_start_phis (fin_bb);
	       !gsi_end_p (gpi); gsi_next (&gpi))
	    {
	      gphi *phi = gpi.phi ();
	      add_phi_arg (phi, gimple_phi_arg_def (phi, dest_idx),
			   ep, UNKNOWN_LOCATION);
	    }
	}
      gsi = gsi_last_bb (entry_bb);
    }

  switch (gimple_omp_for_kind (fd->for_stmt))
    {
    case GF_OMP_FOR_KIND_FOR:
      nthreads = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_THREADS);
      threadid = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
      break;
    case GF_OMP_FOR_KIND_DISTRIBUTE:
      nthreads = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_TEAMS);
      threadid = builtin_decl_explicit (BUILT_IN_OMP_GET_TEAM_NUM);
      break;
    default:
      gcc_unreachable ();
    }
  nthreads = build_call_expr (nthreads, 0);
  nthreads = fold_convert (itype, nthreads);
  nthreads = force_gimple_operand_gsi (&gsi, nthreads, true, NULL_TREE,
				       true, GSI_SAME_STMT);
  threadid = build_call_expr (threadid, 0);
  threadid = fold_convert (itype, threadid);
  threadid = force_gimple_operand_gsi (&gsi, threadid, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  n1 = fd->loop.n1;
  n2 = fd->loop.n2;
  step = fd->loop.step;
  if (gimple_omp_for_combined_into_p (fd->for_stmt))
    {
      tree innerc = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				     OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      n1 = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      n2 = OMP_CLAUSE_DECL (innerc);
    }
  n1 = force_gimple_operand_gsi (&gsi, fold_convert (type, n1),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  n2 = force_gimple_operand_gsi (&gsi, fold_convert (itype, n2),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  step = force_gimple_operand_gsi (&gsi, fold_convert (itype, step),
				   true, NULL_TREE, true, GSI_SAME_STMT);

  t = build_int_cst (itype, (fd->loop.cond_code == LT_EXPR ? -1 : 1));
  t = fold_build2 (PLUS_EXPR, itype, step, t);
  t = fold_build2 (PLUS_EXPR, itype, t, n2);
  t = fold_build2 (MINUS_EXPR, itype, t, fold_convert (itype, n1));
  if (TYPE_UNSIGNED (itype) && fd->loop.cond_code == GT_EXPR)
    t = fold_build2 (TRUNC_DIV_EXPR, itype,
		     fold_build1 (NEGATE_EXPR, itype, t),
		     fold_build1 (NEGATE_EXPR, itype, step));
  else
    t = fold_build2 (TRUNC_DIV_EXPR, itype, t, step);
  t = fold_convert (itype, t);
  n = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE, true, GSI_SAME_STMT);

  q = create_tmp_reg (itype, "q");
  t = fold_build2 (TRUNC_DIV_EXPR, itype, n, nthreads);
  t = force_gimple_operand_gsi (&gsi, t, false, NULL_TREE, true, GSI_SAME_STMT);
  gsi_insert_before (&gsi, gimple_build_assign (q, t), GSI_SAME_STMT);

  tt = create_tmp_reg (itype, "tt");
  t = fold_build2 (TRUNC_MOD_EXPR, itype, n, nthreads);
  t = force_gimple_operand_gsi (&gsi, t, false, NULL_TREE, true, GSI_SAME_STMT);
  gsi_insert_before (&gsi, gimple_build_assign (tt, t), GSI_SAME_STMT);

  t = build2 (LT_EXPR, boolean_type_node, threadid, tt);
  gcond *cond_stmt = gimple_build_cond_empty (t);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);

  second_bb = split_block (entry_bb, cond_stmt)->dest;
  gsi = gsi_last_bb (second_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);

  gsi_insert_before (&gsi, gimple_build_assign (tt, build_int_cst (itype, 0)),
		     GSI_SAME_STMT);
  gassign *assign_stmt
    = gimple_build_assign (q, PLUS_EXPR, q, build_int_cst (itype, 1));
  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

  third_bb = split_block (second_bb, assign_stmt)->dest;
  gsi = gsi_last_bb (third_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);

  t = build2 (MULT_EXPR, itype, q, threadid);
  t = build2 (PLUS_EXPR, itype, t, tt);
  s0 = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE, true, GSI_SAME_STMT);

  t = fold_build2 (PLUS_EXPR, itype, s0, q);
  e0 = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE, true, GSI_SAME_STMT);

  t = build2 (GE_EXPR, boolean_type_node, s0, e0);
  gsi_insert_before (&gsi, gimple_build_cond_empty (t), GSI_SAME_STMT);

  /* Remove the GIMPLE_OMP_FOR statement.  */
  gsi_remove (&gsi, true);

  /* Setup code for sequential iteration goes in SEQ_START_BB.  */
  gsi = gsi_start_bb (seq_start_bb);

  tree startvar = fd->loop.v;
  tree endvar = NULL_TREE;

  if (gimple_omp_for_combined_p (fd->for_stmt))
    {
      tree clauses = gimple_code (inner_stmt) == GIMPLE_OMP_PARALLEL
		     ? gimple_omp_parallel_clauses (inner_stmt)
		     : gimple_omp_for_clauses (inner_stmt);
      tree innerc = find_omp_clause (clauses, OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      startvar = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      endvar = OMP_CLAUSE_DECL (innerc);
      if (fd->collapse > 1 && TREE_CODE (fd->loop.n2) != INTEGER_CST
	  && gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_DISTRIBUTE)
	{
	  int i;
	  for (i = 1; i < fd->collapse; i++)
	    {
	      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
					OMP_CLAUSE__LOOPTEMP_);
	      gcc_assert (innerc);
	    }
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  if (innerc)
	    {
	      /* If needed (distribute parallel for with lastprivate),
		 propagate down the total number of iterations.  */
	      tree t = fold_convert (TREE_TYPE (OMP_CLAUSE_DECL (innerc)),
				     fd->loop.n2);
	      t = force_gimple_operand_gsi (&gsi, t, false, NULL_TREE, false,
					    GSI_CONTINUE_LINKING);
	      assign_stmt = gimple_build_assign (OMP_CLAUSE_DECL (innerc), t);
	      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
	    }
	}
    }
  t = fold_convert (itype, s0);
  t = fold_build2 (MULT_EXPR, itype, t, step);
  if (POINTER_TYPE_P (type))
    t = fold_build_pointer_plus (n1, t);
  else
    t = fold_build2 (PLUS_EXPR, type, t, n1);
  t = fold_convert (TREE_TYPE (startvar), t);
  t = force_gimple_operand_gsi (&gsi, t,
				DECL_P (startvar)
				&& TREE_ADDRESSABLE (startvar),
				NULL_TREE, false, GSI_CONTINUE_LINKING);
  assign_stmt = gimple_build_assign (startvar, t);
  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);

  t = fold_convert (itype, e0);
  t = fold_build2 (MULT_EXPR, itype, t, step);
  if (POINTER_TYPE_P (type))
    t = fold_build_pointer_plus (n1, t);
  else
    t = fold_build2 (PLUS_EXPR, type, t, n1);
  t = fold_convert (TREE_TYPE (startvar), t);
  e = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				false, GSI_CONTINUE_LINKING);
  if (endvar)
    {
      assign_stmt = gimple_build_assign (endvar, e);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
      if (useless_type_conversion_p (TREE_TYPE (fd->loop.v), TREE_TYPE (e)))
	assign_stmt = gimple_build_assign (fd->loop.v, e);
      else
	assign_stmt = gimple_build_assign (fd->loop.v, NOP_EXPR, e);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
    }
  /* Handle linear clause adjustments.  */
  tree itercnt = NULL_TREE;
  if (gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_FOR)
    for (tree c = gimple_omp_for_clauses (fd->for_stmt);
	 c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
	{
	  tree d = OMP_CLAUSE_DECL (c);
	  bool is_ref = is_reference (d);
	  tree t = d, a, dest;
	  if (is_ref)
	    t = build_simple_mem_ref_loc (OMP_CLAUSE_LOCATION (c), t);
	  if (itercnt == NULL_TREE)
	    {
	      if (gimple_omp_for_combined_into_p (fd->for_stmt))
		{
		  itercnt = fold_build2 (MINUS_EXPR, itype,
					 fold_convert (itype, n1),
					 fold_convert (itype, fd->loop.n1));
		  itercnt = fold_build2 (EXACT_DIV_EXPR, itype, itercnt, step);
		  itercnt = fold_build2 (PLUS_EXPR, itype, itercnt, s0);
		  itercnt = force_gimple_operand_gsi (&gsi, itercnt, true,
						      NULL_TREE, false,
						      GSI_CONTINUE_LINKING);
		}
	      else
		itercnt = s0;
	    }
	  tree type = TREE_TYPE (t);
	  if (POINTER_TYPE_P (type))
	    type = sizetype;
	  a = fold_build2 (MULT_EXPR, type,
			   fold_convert (type, itercnt),
			   fold_convert (type, OMP_CLAUSE_LINEAR_STEP (c)));
	  dest = unshare_expr (t);
	  t = fold_build2 (type == TREE_TYPE (t) ? PLUS_EXPR
			   : POINTER_PLUS_EXPR, TREE_TYPE (t), t, a);
	  t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					false, GSI_CONTINUE_LINKING);
	  assign_stmt = gimple_build_assign (dest, t);
	  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
	}
  if (fd->collapse > 1)
    expand_omp_for_init_vars (fd, &gsi, counts, inner_stmt, startvar);

  if (!broken_loop)
    {
      /* The code controlling the sequential loop replaces the
	 GIMPLE_OMP_CONTINUE.  */
      gsi = gsi_last_bb (cont_bb);
      gomp_continue *cont_stmt = as_a <gomp_continue *> (gsi_stmt (gsi));
      gcc_assert (gimple_code (cont_stmt) == GIMPLE_OMP_CONTINUE);
      vmain = gimple_omp_continue_control_use (cont_stmt);
      vback = gimple_omp_continue_control_def (cont_stmt);

      if (!gimple_omp_for_combined_p (fd->for_stmt))
	{
	  if (POINTER_TYPE_P (type))
	    t = fold_build_pointer_plus (vmain, step);
	  else
	    t = fold_build2 (PLUS_EXPR, type, vmain, step);
	  t = force_gimple_operand_gsi (&gsi, t,
					DECL_P (vback)
					&& TREE_ADDRESSABLE (vback),
					NULL_TREE, true, GSI_SAME_STMT);
	  assign_stmt = gimple_build_assign (vback, t);
	  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

	  t = build2 (fd->loop.cond_code, boolean_type_node,
		      DECL_P (vback) && TREE_ADDRESSABLE (vback)
		      ? t : vback, e);
	  gsi_insert_before (&gsi, gimple_build_cond_empty (t), GSI_SAME_STMT);
	}

      /* Remove the GIMPLE_OMP_CONTINUE statement.  */
      gsi_remove (&gsi, true);

      if (fd->collapse > 1 && !gimple_omp_for_combined_p (fd->for_stmt))
	collapse_bb = extract_omp_for_update_vars (fd, cont_bb, body_bb);
    }

  /* Replace the GIMPLE_OMP_RETURN with a barrier, or nothing.  */
  gsi = gsi_last_bb (exit_bb);
  if (!gimple_omp_return_nowait_p (gsi_stmt (gsi)))
    {
      t = gimple_omp_return_lhs (gsi_stmt (gsi));
      gsi_insert_after (&gsi, build_omp_barrier (t), GSI_SAME_STMT);
    }
  gsi_remove (&gsi, true);

  /* Connect all the blocks.  */
  ep = make_edge (entry_bb, third_bb, EDGE_FALSE_VALUE);
  ep->probability = REG_BR_PROB_BASE / 4 * 3;
  ep = find_edge (entry_bb, second_bb);
  ep->flags = EDGE_TRUE_VALUE;
  ep->probability = REG_BR_PROB_BASE / 4;
  find_edge (third_bb, seq_start_bb)->flags = EDGE_FALSE_VALUE;
  find_edge (third_bb, fin_bb)->flags = EDGE_TRUE_VALUE;

  if (!broken_loop)
    {
      ep = find_edge (cont_bb, body_bb);
      if (ep == NULL)
	{
	  ep = BRANCH_EDGE (cont_bb);
	  gcc_assert (single_succ (ep->dest) == body_bb);
	}
      if (gimple_omp_for_combined_p (fd->for_stmt))
	{
	  remove_edge (ep);
	  ep = NULL;
	}
      else if (fd->collapse > 1)
	{
	  remove_edge (ep);
	  ep = make_edge (cont_bb, collapse_bb, EDGE_TRUE_VALUE);
	}
      else
	ep->flags = EDGE_TRUE_VALUE;
      find_edge (cont_bb, fin_bb)->flags
	= ep ? EDGE_FALSE_VALUE : EDGE_FALLTHRU;
    }

  set_immediate_dominator (CDI_DOMINATORS, second_bb, entry_bb);
  set_immediate_dominator (CDI_DOMINATORS, third_bb, entry_bb);
  set_immediate_dominator (CDI_DOMINATORS, seq_start_bb, third_bb);

  set_immediate_dominator (CDI_DOMINATORS, body_bb,
			   recompute_dominator (CDI_DOMINATORS, body_bb));
  set_immediate_dominator (CDI_DOMINATORS, fin_bb,
			   recompute_dominator (CDI_DOMINATORS, fin_bb));

  struct loop *loop = body_bb->loop_father;
  if (loop != entry_bb->loop_father)
    {
      gcc_assert (loop->header == body_bb);
      gcc_assert (broken_loop
		  || loop->latch == region->cont
		  || single_pred (loop->latch) == region->cont);
      return;
    }

  if (!broken_loop && !gimple_omp_for_combined_p (fd->for_stmt))
    {
      loop = alloc_loop ();
      loop->header = body_bb;
      if (collapse_bb == NULL)
	loop->latch = cont_bb;
      add_loop (loop, body_bb->loop_father);
    }
}

/* Return phi in E->DEST with ARG on edge E.  */

static gphi *
find_phi_with_arg_on_edge (tree arg, edge e)
{
  basic_block bb = e->dest;

  for (gphi_iterator gpi = gsi_start_phis (bb);
       !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      if (PHI_ARG_DEF_FROM_EDGE (phi, e) == arg)
	return phi;
    }

  return NULL;
}

/* A subroutine of expand_omp_for.  Generate code for a parallel
   loop with static schedule and a specified chunk size.  Given
   parameters:

	for (V = N1; V cond N2; V += STEP) BODY;

   where COND is "<" or ">", we generate pseudocode

	if ((__typeof (V)) -1 > 0 && N2 cond N1) goto L2;
	if (cond is <)
	  adj = STEP - 1;
	else
	  adj = STEP + 1;
	if ((__typeof (V)) -1 > 0 && cond is >)
	  n = -(adj + N2 - N1) / -STEP;
	else
	  n = (adj + N2 - N1) / STEP;
	trip = 0;
	V = threadid * CHUNK * STEP + N1;  -- this extra definition of V is
					      here so that V is defined
					      if the loop is not entered
    L0:
	s0 = (trip * nthreads + threadid) * CHUNK;
	e0 = min(s0 + CHUNK, n);
	if (s0 < n) goto L1; else goto L4;
    L1:
	V = s0 * STEP + N1;
	e = e0 * STEP + N1;
    L2:
	BODY;
	V += STEP;
	if (V cond e) goto L2; else goto L3;
    L3:
	trip += 1;
	goto L0;
    L4:
*/

static void
expand_omp_for_static_chunk (struct omp_region *region,
			     struct omp_for_data *fd, gimple *inner_stmt)
{
  tree n, s0, e0, e, t;
  tree trip_var, trip_init, trip_main, trip_back, nthreads, threadid;
  tree type, itype, vmain, vback, vextra;
  basic_block entry_bb, exit_bb, body_bb, seq_start_bb, iter_part_bb;
  basic_block trip_update_bb = NULL, cont_bb, collapse_bb = NULL, fin_bb;
  gimple_stmt_iterator gsi;
  edge se;
  bool broken_loop = region->cont == NULL;
  tree *counts = NULL;
  tree n1, n2, step;

  itype = type = TREE_TYPE (fd->loop.v);
  if (POINTER_TYPE_P (type))
    itype = signed_type_for (type);

  entry_bb = region->entry;
  se = split_block (entry_bb, last_stmt (entry_bb));
  entry_bb = se->src;
  iter_part_bb = se->dest;
  cont_bb = region->cont;
  gcc_assert (EDGE_COUNT (iter_part_bb->succs) == 2);
  fin_bb = BRANCH_EDGE (iter_part_bb)->dest;
  gcc_assert (broken_loop
	      || fin_bb == FALLTHRU_EDGE (cont_bb)->dest);
  seq_start_bb = split_edge (FALLTHRU_EDGE (iter_part_bb));
  body_bb = single_succ (seq_start_bb);
  if (!broken_loop)
    {
      gcc_assert (BRANCH_EDGE (cont_bb)->dest == body_bb
		  || single_succ (BRANCH_EDGE (cont_bb)->dest) == body_bb);
      gcc_assert (EDGE_COUNT (cont_bb->succs) == 2);
      trip_update_bb = split_edge (FALLTHRU_EDGE (cont_bb));
    }
  exit_bb = region->exit;

  /* Trip and adjustment setup goes in ENTRY_BB.  */
  gsi = gsi_last_bb (entry_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);

  if (fd->collapse > 1)
    {
      int first_zero_iter = -1, dummy = -1;
      basic_block l2_dom_bb = NULL, dummy_bb = NULL;

      counts = XALLOCAVEC (tree, fd->collapse);
      expand_omp_for_init_counts (fd, &gsi, entry_bb, counts,
				  fin_bb, first_zero_iter,
				  dummy_bb, dummy, l2_dom_bb);
      t = NULL_TREE;
    }
  else if (gimple_omp_for_combined_into_p (fd->for_stmt))
    t = integer_one_node;
  else
    t = fold_binary (fd->loop.cond_code, boolean_type_node,
		     fold_convert (type, fd->loop.n1),
		     fold_convert (type, fd->loop.n2));
  if (fd->collapse == 1
      && TYPE_UNSIGNED (type)
      && (t == NULL_TREE || !integer_onep (t)))
    {
      n1 = fold_convert (type, unshare_expr (fd->loop.n1));
      n1 = force_gimple_operand_gsi (&gsi, n1, true, NULL_TREE,
				     true, GSI_SAME_STMT);
      n2 = fold_convert (type, unshare_expr (fd->loop.n2));
      n2 = force_gimple_operand_gsi (&gsi, n2, true, NULL_TREE,
				     true, GSI_SAME_STMT);
      gcond *cond_stmt = gimple_build_cond (fd->loop.cond_code, n1, n2,
						 NULL_TREE, NULL_TREE);
      gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);
      if (walk_tree (gimple_cond_lhs_ptr (cond_stmt),
		     expand_omp_regimplify_p, NULL, NULL)
	  || walk_tree (gimple_cond_rhs_ptr (cond_stmt),
			expand_omp_regimplify_p, NULL, NULL))
	{
	  gsi = gsi_for_stmt (cond_stmt);
	  gimple_regimplify_operands (cond_stmt, &gsi);
	}
      se = split_block (entry_bb, cond_stmt);
      se->flags = EDGE_TRUE_VALUE;
      entry_bb = se->dest;
      se->probability = REG_BR_PROB_BASE - (REG_BR_PROB_BASE / 2000 - 1);
      se = make_edge (se->src, fin_bb, EDGE_FALSE_VALUE);
      se->probability = REG_BR_PROB_BASE / 2000 - 1;
      if (gimple_in_ssa_p (cfun))
	{
	  int dest_idx = find_edge (iter_part_bb, fin_bb)->dest_idx;
	  for (gphi_iterator gpi = gsi_start_phis (fin_bb);
	       !gsi_end_p (gpi); gsi_next (&gpi))
	    {
	      gphi *phi = gpi.phi ();
	      add_phi_arg (phi, gimple_phi_arg_def (phi, dest_idx),
			   se, UNKNOWN_LOCATION);
	    }
	}
      gsi = gsi_last_bb (entry_bb);
    }

  switch (gimple_omp_for_kind (fd->for_stmt))
    {
    case GF_OMP_FOR_KIND_FOR:
      nthreads = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_THREADS);
      threadid = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
      break;
    case GF_OMP_FOR_KIND_DISTRIBUTE:
      nthreads = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_TEAMS);
      threadid = builtin_decl_explicit (BUILT_IN_OMP_GET_TEAM_NUM);
      break;
    default:
      gcc_unreachable ();
    }
  nthreads = build_call_expr (nthreads, 0);
  nthreads = fold_convert (itype, nthreads);
  nthreads = force_gimple_operand_gsi (&gsi, nthreads, true, NULL_TREE,
				       true, GSI_SAME_STMT);
  threadid = build_call_expr (threadid, 0);
  threadid = fold_convert (itype, threadid);
  threadid = force_gimple_operand_gsi (&gsi, threadid, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  n1 = fd->loop.n1;
  n2 = fd->loop.n2;
  step = fd->loop.step;
  if (gimple_omp_for_combined_into_p (fd->for_stmt))
    {
      tree innerc = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				     OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      n1 = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      n2 = OMP_CLAUSE_DECL (innerc);
    }
  n1 = force_gimple_operand_gsi (&gsi, fold_convert (type, n1),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  n2 = force_gimple_operand_gsi (&gsi, fold_convert (itype, n2),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  step = force_gimple_operand_gsi (&gsi, fold_convert (itype, step),
				   true, NULL_TREE, true, GSI_SAME_STMT);
  tree chunk_size = fold_convert (itype, fd->chunk_size);
  chunk_size = omp_adjust_chunk_size (chunk_size, fd->simd_schedule);
  chunk_size
    = force_gimple_operand_gsi (&gsi, chunk_size, true, NULL_TREE, true,
				GSI_SAME_STMT);

  t = build_int_cst (itype, (fd->loop.cond_code == LT_EXPR ? -1 : 1));
  t = fold_build2 (PLUS_EXPR, itype, step, t);
  t = fold_build2 (PLUS_EXPR, itype, t, n2);
  t = fold_build2 (MINUS_EXPR, itype, t, fold_convert (itype, n1));
  if (TYPE_UNSIGNED (itype) && fd->loop.cond_code == GT_EXPR)
    t = fold_build2 (TRUNC_DIV_EXPR, itype,
		     fold_build1 (NEGATE_EXPR, itype, t),
		     fold_build1 (NEGATE_EXPR, itype, step));
  else
    t = fold_build2 (TRUNC_DIV_EXPR, itype, t, step);
  t = fold_convert (itype, t);
  n = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				true, GSI_SAME_STMT);

  trip_var = create_tmp_reg (itype, ".trip");
  if (gimple_in_ssa_p (cfun))
    {
      trip_init = make_ssa_name (trip_var);
      trip_main = make_ssa_name (trip_var);
      trip_back = make_ssa_name (trip_var);
    }
  else
    {
      trip_init = trip_var;
      trip_main = trip_var;
      trip_back = trip_var;
    }

  gassign *assign_stmt
    = gimple_build_assign (trip_init, build_int_cst (itype, 0));
  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

  t = fold_build2 (MULT_EXPR, itype, threadid, chunk_size);
  t = fold_build2 (MULT_EXPR, itype, t, step);
  if (POINTER_TYPE_P (type))
    t = fold_build_pointer_plus (n1, t);
  else
    t = fold_build2 (PLUS_EXPR, type, t, n1);
  vextra = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				     true, GSI_SAME_STMT);

  /* Remove the GIMPLE_OMP_FOR.  */
  gsi_remove (&gsi, true);

  gimple_stmt_iterator gsif = gsi;

  /* Iteration space partitioning goes in ITER_PART_BB.  */
  gsi = gsi_last_bb (iter_part_bb);

  t = fold_build2 (MULT_EXPR, itype, trip_main, nthreads);
  t = fold_build2 (PLUS_EXPR, itype, t, threadid);
  t = fold_build2 (MULT_EXPR, itype, t, chunk_size);
  s0 = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				 false, GSI_CONTINUE_LINKING);

  t = fold_build2 (PLUS_EXPR, itype, s0, chunk_size);
  t = fold_build2 (MIN_EXPR, itype, t, n);
  e0 = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				 false, GSI_CONTINUE_LINKING);

  t = build2 (LT_EXPR, boolean_type_node, s0, n);
  gsi_insert_after (&gsi, gimple_build_cond_empty (t), GSI_CONTINUE_LINKING);

  /* Setup code for sequential iteration goes in SEQ_START_BB.  */
  gsi = gsi_start_bb (seq_start_bb);

  tree startvar = fd->loop.v;
  tree endvar = NULL_TREE;

  if (gimple_omp_for_combined_p (fd->for_stmt))
    {
      tree clauses = gimple_code (inner_stmt) == GIMPLE_OMP_PARALLEL
		     ? gimple_omp_parallel_clauses (inner_stmt)
		     : gimple_omp_for_clauses (inner_stmt);
      tree innerc = find_omp_clause (clauses, OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      startvar = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      endvar = OMP_CLAUSE_DECL (innerc);
      if (fd->collapse > 1 && TREE_CODE (fd->loop.n2) != INTEGER_CST
	  && gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_DISTRIBUTE)
	{
	  int i;
	  for (i = 1; i < fd->collapse; i++)
	    {
	      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
					OMP_CLAUSE__LOOPTEMP_);
	      gcc_assert (innerc);
	    }
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  if (innerc)
	    {
	      /* If needed (distribute parallel for with lastprivate),
		 propagate down the total number of iterations.  */
	      tree t = fold_convert (TREE_TYPE (OMP_CLAUSE_DECL (innerc)),
				     fd->loop.n2);
	      t = force_gimple_operand_gsi (&gsi, t, false, NULL_TREE, false,
					    GSI_CONTINUE_LINKING);
	      assign_stmt = gimple_build_assign (OMP_CLAUSE_DECL (innerc), t);
	      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
	    }
	}
    }

  t = fold_convert (itype, s0);
  t = fold_build2 (MULT_EXPR, itype, t, step);
  if (POINTER_TYPE_P (type))
    t = fold_build_pointer_plus (n1, t);
  else
    t = fold_build2 (PLUS_EXPR, type, t, n1);
  t = fold_convert (TREE_TYPE (startvar), t);
  t = force_gimple_operand_gsi (&gsi, t,
				DECL_P (startvar)
				&& TREE_ADDRESSABLE (startvar),
				NULL_TREE, false, GSI_CONTINUE_LINKING);
  assign_stmt = gimple_build_assign (startvar, t);
  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);

  t = fold_convert (itype, e0);
  t = fold_build2 (MULT_EXPR, itype, t, step);
  if (POINTER_TYPE_P (type))
    t = fold_build_pointer_plus (n1, t);
  else
    t = fold_build2 (PLUS_EXPR, type, t, n1);
  t = fold_convert (TREE_TYPE (startvar), t);
  e = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				false, GSI_CONTINUE_LINKING);
  if (endvar)
    {
      assign_stmt = gimple_build_assign (endvar, e);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
      if (useless_type_conversion_p (TREE_TYPE (fd->loop.v), TREE_TYPE (e)))
	assign_stmt = gimple_build_assign (fd->loop.v, e);
      else
	assign_stmt = gimple_build_assign (fd->loop.v, NOP_EXPR, e);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
    }
  /* Handle linear clause adjustments.  */
  tree itercnt = NULL_TREE, itercntbias = NULL_TREE;
  if (gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_FOR)
    for (tree c = gimple_omp_for_clauses (fd->for_stmt);
	 c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
	{
	  tree d = OMP_CLAUSE_DECL (c);
	  bool is_ref = is_reference (d);
	  tree t = d, a, dest;
	  if (is_ref)
	    t = build_simple_mem_ref_loc (OMP_CLAUSE_LOCATION (c), t);
	  tree type = TREE_TYPE (t);
	  if (POINTER_TYPE_P (type))
	    type = sizetype;
	  dest = unshare_expr (t);
	  tree v = create_tmp_var (TREE_TYPE (t), NULL);
	  expand_omp_build_assign (&gsif, v, t);
	  if (itercnt == NULL_TREE)
	    {
	      if (gimple_omp_for_combined_into_p (fd->for_stmt))
		{
		  itercntbias
		    = fold_build2 (MINUS_EXPR, itype, fold_convert (itype, n1),
				   fold_convert (itype, fd->loop.n1));
		  itercntbias = fold_build2 (EXACT_DIV_EXPR, itype,
					     itercntbias, step);
		  itercntbias
		    = force_gimple_operand_gsi (&gsif, itercntbias, true,
						NULL_TREE, true,
						GSI_SAME_STMT);
		  itercnt = fold_build2 (PLUS_EXPR, itype, itercntbias, s0);
		  itercnt = force_gimple_operand_gsi (&gsi, itercnt, true,
						      NULL_TREE, false,
						      GSI_CONTINUE_LINKING);
		}
	      else
		itercnt = s0;
	    }
	  a = fold_build2 (MULT_EXPR, type,
			   fold_convert (type, itercnt),
			   fold_convert (type, OMP_CLAUSE_LINEAR_STEP (c)));
	  t = fold_build2 (type == TREE_TYPE (t) ? PLUS_EXPR
			   : POINTER_PLUS_EXPR, TREE_TYPE (t), v, a);
	  t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					false, GSI_CONTINUE_LINKING);
	  assign_stmt = gimple_build_assign (dest, t);
	  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
	}
  if (fd->collapse > 1)
    expand_omp_for_init_vars (fd, &gsi, counts, inner_stmt, startvar);

  if (!broken_loop)
    {
      /* The code controlling the sequential loop goes in CONT_BB,
	 replacing the GIMPLE_OMP_CONTINUE.  */
      gsi = gsi_last_bb (cont_bb);
      gomp_continue *cont_stmt = as_a <gomp_continue *> (gsi_stmt (gsi));
      vmain = gimple_omp_continue_control_use (cont_stmt);
      vback = gimple_omp_continue_control_def (cont_stmt);

      if (!gimple_omp_for_combined_p (fd->for_stmt))
	{
	  if (POINTER_TYPE_P (type))
	    t = fold_build_pointer_plus (vmain, step);
	  else
	    t = fold_build2 (PLUS_EXPR, type, vmain, step);
	  if (DECL_P (vback) && TREE_ADDRESSABLE (vback))
	    t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
					  true, GSI_SAME_STMT);
	  assign_stmt = gimple_build_assign (vback, t);
	  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

	  if (tree_int_cst_equal (fd->chunk_size, integer_one_node))
	    t = build2 (EQ_EXPR, boolean_type_node,
			build_int_cst (itype, 0),
			build_int_cst (itype, 1));
	  else
	    t = build2 (fd->loop.cond_code, boolean_type_node,
			DECL_P (vback) && TREE_ADDRESSABLE (vback)
			? t : vback, e);
	  gsi_insert_before (&gsi, gimple_build_cond_empty (t), GSI_SAME_STMT);
	}

      /* Remove GIMPLE_OMP_CONTINUE.  */
      gsi_remove (&gsi, true);

      if (fd->collapse > 1 && !gimple_omp_for_combined_p (fd->for_stmt))
	collapse_bb = extract_omp_for_update_vars (fd, cont_bb, body_bb);

      /* Trip update code goes into TRIP_UPDATE_BB.  */
      gsi = gsi_start_bb (trip_update_bb);

      t = build_int_cst (itype, 1);
      t = build2 (PLUS_EXPR, itype, trip_main, t);
      assign_stmt = gimple_build_assign (trip_back, t);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
    }

  /* Replace the GIMPLE_OMP_RETURN with a barrier, or nothing.  */
  gsi = gsi_last_bb (exit_bb);
  if (!gimple_omp_return_nowait_p (gsi_stmt (gsi)))
    {
      t = gimple_omp_return_lhs (gsi_stmt (gsi));
      gsi_insert_after (&gsi, build_omp_barrier (t), GSI_SAME_STMT);
    }
  gsi_remove (&gsi, true);

  /* Connect the new blocks.  */
  find_edge (iter_part_bb, seq_start_bb)->flags = EDGE_TRUE_VALUE;
  find_edge (iter_part_bb, fin_bb)->flags = EDGE_FALSE_VALUE;

  if (!broken_loop)
    {
      se = find_edge (cont_bb, body_bb);
      if (se == NULL)
	{
	  se = BRANCH_EDGE (cont_bb);
	  gcc_assert (single_succ (se->dest) == body_bb);
	}
      if (gimple_omp_for_combined_p (fd->for_stmt))
	{
	  remove_edge (se);
	  se = NULL;
	}
      else if (fd->collapse > 1)
	{
	  remove_edge (se);
	  se = make_edge (cont_bb, collapse_bb, EDGE_TRUE_VALUE);
	}
      else
	se->flags = EDGE_TRUE_VALUE;
      find_edge (cont_bb, trip_update_bb)->flags
	= se ? EDGE_FALSE_VALUE : EDGE_FALLTHRU;

      redirect_edge_and_branch (single_succ_edge (trip_update_bb), iter_part_bb);
    }

  if (gimple_in_ssa_p (cfun))
    {
      gphi_iterator psi;
      gphi *phi;
      edge re, ene;
      edge_var_map *vm;
      size_t i;

      gcc_assert (fd->collapse == 1 && !broken_loop);

      /* When we redirect the edge from trip_update_bb to iter_part_bb, we
	 remove arguments of the phi nodes in fin_bb.  We need to create
	 appropriate phi nodes in iter_part_bb instead.  */
      se = find_edge (iter_part_bb, fin_bb);
      re = single_succ_edge (trip_update_bb);
      vec<edge_var_map> *head = redirect_edge_var_map_vector (re);
      ene = single_succ_edge (entry_bb);

      psi = gsi_start_phis (fin_bb);
      for (i = 0; !gsi_end_p (psi) && head->iterate (i, &vm);
	   gsi_next (&psi), ++i)
	{
	  gphi *nphi;
	  source_location locus;

	  phi = psi.phi ();
	  t = gimple_phi_result (phi);
	  gcc_assert (t == redirect_edge_var_map_result (vm));

	  if (!single_pred_p (fin_bb))
	    t = copy_ssa_name (t, phi);

	  nphi = create_phi_node (t, iter_part_bb);

	  t = PHI_ARG_DEF_FROM_EDGE (phi, se);
	  locus = gimple_phi_arg_location_from_edge (phi, se);

	  /* A special case -- fd->loop.v is not yet computed in
	     iter_part_bb, we need to use vextra instead.  */
	  if (t == fd->loop.v)
	    t = vextra;
	  add_phi_arg (nphi, t, ene, locus);
	  locus = redirect_edge_var_map_location (vm);
	  tree back_arg = redirect_edge_var_map_def (vm);
	  add_phi_arg (nphi, back_arg, re, locus);
	  edge ce = find_edge (cont_bb, body_bb);
	  if (ce == NULL)
	    {
	      ce = BRANCH_EDGE (cont_bb);
	      gcc_assert (single_succ (ce->dest) == body_bb);
	      ce = single_succ_edge (ce->dest);
	    }
	  gphi *inner_loop_phi = find_phi_with_arg_on_edge (back_arg, ce);
	  gcc_assert (inner_loop_phi != NULL);
	  add_phi_arg (inner_loop_phi, gimple_phi_result (nphi),
		       find_edge (seq_start_bb, body_bb), locus);

	  if (!single_pred_p (fin_bb))
	    add_phi_arg (phi, gimple_phi_result (nphi), se, locus);
	}
      gcc_assert (gsi_end_p (psi) && (head == NULL || i == head->length ()));
      redirect_edge_var_map_clear (re);
      if (single_pred_p (fin_bb))
	while (1)
	  {
	    psi = gsi_start_phis (fin_bb);
	    if (gsi_end_p (psi))
	      break;
	    remove_phi_node (&psi, false);
	  }

      /* Make phi node for trip.  */
      phi = create_phi_node (trip_main, iter_part_bb);
      add_phi_arg (phi, trip_back, single_succ_edge (trip_update_bb),
		   UNKNOWN_LOCATION);
      add_phi_arg (phi, trip_init, single_succ_edge (entry_bb),
		   UNKNOWN_LOCATION);
    }

  if (!broken_loop)
    set_immediate_dominator (CDI_DOMINATORS, trip_update_bb, cont_bb);
  set_immediate_dominator (CDI_DOMINATORS, iter_part_bb,
			   recompute_dominator (CDI_DOMINATORS, iter_part_bb));
  set_immediate_dominator (CDI_DOMINATORS, fin_bb,
			   recompute_dominator (CDI_DOMINATORS, fin_bb));
  set_immediate_dominator (CDI_DOMINATORS, seq_start_bb,
			   recompute_dominator (CDI_DOMINATORS, seq_start_bb));
  set_immediate_dominator (CDI_DOMINATORS, body_bb,
			   recompute_dominator (CDI_DOMINATORS, body_bb));

  if (!broken_loop)
    {
      struct loop *loop = body_bb->loop_father;
      struct loop *trip_loop = alloc_loop ();
      trip_loop->header = iter_part_bb;
      trip_loop->latch = trip_update_bb;
      add_loop (trip_loop, iter_part_bb->loop_father);

      if (loop != entry_bb->loop_father)
	{
	  gcc_assert (loop->header == body_bb);
	  gcc_assert (loop->latch == region->cont
		      || single_pred (loop->latch) == region->cont);
	  trip_loop->inner = loop;
	  return;
	}

      if (!gimple_omp_for_combined_p (fd->for_stmt))
	{
	  loop = alloc_loop ();
	  loop->header = body_bb;
	  if (collapse_bb == NULL)
	    loop->latch = cont_bb;
	  add_loop (loop, trip_loop);
	}
    }
}

/* A subroutine of expand_omp_for.  Generate code for _Cilk_for loop.
   Given parameters:
   for (V = N1; V cond N2; V += STEP) BODY;

   where COND is "<" or ">" or "!=", we generate pseudocode

   for (ind_var = low; ind_var < high; ind_var++)
     {
       V = n1 + (ind_var * STEP)

       <BODY>
     }

   In the above pseudocode, low and high are function parameters of the
   child function.  In the function below, we are inserting a temp.
   variable that will be making a call to two OMP functions that will not be
   found in the body of _Cilk_for (since OMP_FOR cannot be mixed
   with _Cilk_for).  These functions are replaced with low and high
   by the function that handles taskreg.  */


static void
expand_cilk_for (struct omp_region *region, struct omp_for_data *fd)
{
  bool broken_loop = region->cont == NULL;
  basic_block entry_bb = region->entry;
  basic_block cont_bb = region->cont;

  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2);
  gcc_assert (broken_loop
	      || BRANCH_EDGE (entry_bb)->dest == FALLTHRU_EDGE (cont_bb)->dest);
  basic_block l0_bb = FALLTHRU_EDGE (entry_bb)->dest;
  basic_block l1_bb, l2_bb;

  if (!broken_loop)
    {
      gcc_assert (BRANCH_EDGE (cont_bb)->dest == l0_bb);
      gcc_assert (EDGE_COUNT (cont_bb->succs) == 2);
      l1_bb = split_block (cont_bb, last_stmt (cont_bb))->dest;
      l2_bb = BRANCH_EDGE (entry_bb)->dest;
    }
  else
    {
      BRANCH_EDGE (entry_bb)->flags &= ~EDGE_ABNORMAL;
      l1_bb = split_edge (BRANCH_EDGE (entry_bb));
      l2_bb = single_succ (l1_bb);
    }
  basic_block exit_bb = region->exit;
  basic_block l2_dom_bb = NULL;

  gimple_stmt_iterator gsi = gsi_last_bb (entry_bb);

  /* Below statements until the "tree high_val = ..." are pseudo statements
     used to pass information to be used by expand_omp_taskreg.
     low_val and high_val will be replaced by the __low and __high
     parameter from the child function.

     The call_exprs part is a place-holder, it is mainly used
     to distinctly identify to the top-level part that this is
     where we should put low and high (reasoning given in header
     comment).  */

  tree child_fndecl
    = gimple_omp_parallel_child_fn (
        as_a <gomp_parallel *> (last_stmt (region->outer->entry)));
  tree t, low_val = NULL_TREE, high_val = NULL_TREE;
  for (t = DECL_ARGUMENTS (child_fndecl); t; t = TREE_CHAIN (t))
    {
      if (!strcmp (IDENTIFIER_POINTER (DECL_NAME (t)), "__high"))
	high_val = t;
      else if (!strcmp (IDENTIFIER_POINTER (DECL_NAME (t)), "__low"))
	low_val = t;
    }
  gcc_assert (low_val && high_val);

  tree type = TREE_TYPE (low_val);
  tree ind_var = create_tmp_reg (type, "__cilk_ind_var");
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);

  /* Not needed in SSA form right now.  */
  gcc_assert (!gimple_in_ssa_p (cfun));
  if (l2_dom_bb == NULL)
    l2_dom_bb = l1_bb;

  tree n1 = low_val;
  tree n2 = high_val;

  gimple *stmt = gimple_build_assign (ind_var, n1);

  /* Replace the GIMPLE_OMP_FOR statement.  */
  gsi_replace (&gsi, stmt, true);

  if (!broken_loop)
    {
      /* Code to control the increment goes in the CONT_BB.  */
      gsi = gsi_last_bb (cont_bb);
      stmt = gsi_stmt (gsi);
      gcc_assert (gimple_code (stmt) == GIMPLE_OMP_CONTINUE);
      stmt = gimple_build_assign (ind_var, PLUS_EXPR, ind_var,
				  build_one_cst (type));

      /* Replace GIMPLE_OMP_CONTINUE.  */
      gsi_replace (&gsi, stmt, true);
    }

  /* Emit the condition in L1_BB.  */
  gsi = gsi_after_labels (l1_bb);
  t = fold_build2 (MULT_EXPR, TREE_TYPE (fd->loop.step),
		   fold_convert (TREE_TYPE (fd->loop.step), ind_var),
		   fd->loop.step);
  if (POINTER_TYPE_P (TREE_TYPE (fd->loop.n1)))
    t = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (fd->loop.n1),
		     fd->loop.n1, fold_convert (sizetype, t));
  else
    t = fold_build2 (PLUS_EXPR, TREE_TYPE (fd->loop.n1),
		     fd->loop.n1, fold_convert (TREE_TYPE (fd->loop.n1), t));
  t = fold_convert (TREE_TYPE (fd->loop.v), t);
  expand_omp_build_assign (&gsi, fd->loop.v, t);

  /* The condition is always '<' since the runtime will fill in the low
     and high values.  */
  stmt = gimple_build_cond (LT_EXPR, ind_var, n2, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);

  /* Remove GIMPLE_OMP_RETURN.  */
  gsi = gsi_last_bb (exit_bb);
  gsi_remove (&gsi, true);

  /* Connect the new blocks.  */
  remove_edge (FALLTHRU_EDGE (entry_bb));

  edge e, ne;
  if (!broken_loop)
    {
      remove_edge (BRANCH_EDGE (entry_bb));
      make_edge (entry_bb, l1_bb, EDGE_FALLTHRU);

      e = BRANCH_EDGE (l1_bb);
      ne = FALLTHRU_EDGE (l1_bb);
      e->flags = EDGE_TRUE_VALUE;
    }
  else
    {
      single_succ_edge (entry_bb)->flags = EDGE_FALLTHRU;

      ne = single_succ_edge (l1_bb);
      e = make_edge (l1_bb, l0_bb, EDGE_TRUE_VALUE);

    }
  ne->flags = EDGE_FALSE_VALUE;
  e->probability = REG_BR_PROB_BASE * 7 / 8;
  ne->probability = REG_BR_PROB_BASE / 8;

  set_immediate_dominator (CDI_DOMINATORS, l1_bb, entry_bb);
  set_immediate_dominator (CDI_DOMINATORS, l2_bb, l2_dom_bb);
  set_immediate_dominator (CDI_DOMINATORS, l0_bb, l1_bb);

  if (!broken_loop)
    {
      struct loop *loop = alloc_loop ();
      loop->header = l1_bb;
      loop->latch = cont_bb;
      add_loop (loop, l1_bb->loop_father);
      loop->safelen = INT_MAX;
    }

  /* Pick the correct library function based on the precision of the
     induction variable type.  */
  tree lib_fun = NULL_TREE;
  if (TYPE_PRECISION (type) == 32)
    lib_fun = cilk_for_32_fndecl;
  else if (TYPE_PRECISION (type) == 64)
    lib_fun = cilk_for_64_fndecl;
  else
    gcc_unreachable ();

  gcc_assert (fd->sched_kind == OMP_CLAUSE_SCHEDULE_CILKFOR);

  /* WS_ARGS contains the library function flavor to call:
     __libcilkrts_cilk_for_64 or __libcilkrts_cilk_for_32), and the
     user-defined grain value.  If the user does not define one, then zero
     is passed in by the parser.  */
  vec_alloc (region->ws_args, 2);
  region->ws_args->quick_push (lib_fun);
  region->ws_args->quick_push (fd->chunk_size);
}

/* A subroutine of expand_omp_for.  Generate code for a simd non-worksharing
   loop.  Given parameters:

	for (V = N1; V cond N2; V += STEP) BODY;

   where COND is "<" or ">", we generate pseudocode

	V = N1;
	goto L1;
    L0:
	BODY;
	V += STEP;
    L1:
	if (V cond N2) goto L0; else goto L2;
    L2:

    For collapsed loops, given parameters:
      collapse(3)
      for (V1 = N11; V1 cond1 N12; V1 += STEP1)
	for (V2 = N21; V2 cond2 N22; V2 += STEP2)
	  for (V3 = N31; V3 cond3 N32; V3 += STEP3)
	    BODY;

    we generate pseudocode

	if (cond3 is <)
	  adj = STEP3 - 1;
	else
	  adj = STEP3 + 1;
	count3 = (adj + N32 - N31) / STEP3;
	if (cond2 is <)
	  adj = STEP2 - 1;
	else
	  adj = STEP2 + 1;
	count2 = (adj + N22 - N21) / STEP2;
	if (cond1 is <)
	  adj = STEP1 - 1;
	else
	  adj = STEP1 + 1;
	count1 = (adj + N12 - N11) / STEP1;
	count = count1 * count2 * count3;
	V = 0;
	V1 = N11;
	V2 = N21;
	V3 = N31;
	goto L1;
    L0:
	BODY;
	V += 1;
	V3 += STEP3;
	V2 += (V3 cond3 N32) ? 0 : STEP2;
	V3 = (V3 cond3 N32) ? V3 : N31;
	V1 += (V2 cond2 N22) ? 0 : STEP1;
	V2 = (V2 cond2 N22) ? V2 : N21;
    L1:
	if (V < count) goto L0; else goto L2;
    L2:

      */

static void
expand_omp_simd (struct omp_region *region, struct omp_for_data *fd)
{
  tree type, t;
  basic_block entry_bb, cont_bb, exit_bb, l0_bb, l1_bb, l2_bb, l2_dom_bb;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  gcond *cond_stmt;
  bool broken_loop = region->cont == NULL;
  edge e, ne;
  tree *counts = NULL;
  int i;
  tree safelen = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				  OMP_CLAUSE_SAFELEN);
  tree simduid = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				  OMP_CLAUSE__SIMDUID_);
  tree n1, n2;

  type = TREE_TYPE (fd->loop.v);
  entry_bb = region->entry;
  cont_bb = region->cont;
  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2);
  gcc_assert (broken_loop
	      || BRANCH_EDGE (entry_bb)->dest == FALLTHRU_EDGE (cont_bb)->dest);
  l0_bb = FALLTHRU_EDGE (entry_bb)->dest;
  if (!broken_loop)
    {
      gcc_assert (BRANCH_EDGE (cont_bb)->dest == l0_bb);
      gcc_assert (EDGE_COUNT (cont_bb->succs) == 2);
      l1_bb = split_block (cont_bb, last_stmt (cont_bb))->dest;
      l2_bb = BRANCH_EDGE (entry_bb)->dest;
    }
  else
    {
      BRANCH_EDGE (entry_bb)->flags &= ~EDGE_ABNORMAL;
      l1_bb = split_edge (BRANCH_EDGE (entry_bb));
      l2_bb = single_succ (l1_bb);
    }
  exit_bb = region->exit;
  l2_dom_bb = NULL;

  gsi = gsi_last_bb (entry_bb);

  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);
  /* Not needed in SSA form right now.  */
  gcc_assert (!gimple_in_ssa_p (cfun));
  if (fd->collapse > 1)
    {
      int first_zero_iter = -1, dummy = -1;
      basic_block zero_iter_bb = l2_bb, dummy_bb = NULL;

      counts = XALLOCAVEC (tree, fd->collapse);
      expand_omp_for_init_counts (fd, &gsi, entry_bb, counts,
				  zero_iter_bb, first_zero_iter,
				  dummy_bb, dummy, l2_dom_bb);
    }
  if (l2_dom_bb == NULL)
    l2_dom_bb = l1_bb;

  n1 = fd->loop.n1;
  n2 = fd->loop.n2;
  if (gimple_omp_for_combined_into_p (fd->for_stmt))
    {
      tree innerc = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				     OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      n1 = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      n2 = OMP_CLAUSE_DECL (innerc);
      expand_omp_build_assign (&gsi, fd->loop.v,
			       fold_convert (type, n1));
      if (fd->collapse > 1)
	{
	  gsi_prev (&gsi);
	  expand_omp_for_init_vars (fd, &gsi, counts, NULL, n1);
	  gsi_next (&gsi);
	}
    }
  else
    {
      expand_omp_build_assign (&gsi, fd->loop.v,
			       fold_convert (type, fd->loop.n1));
      if (fd->collapse > 1)
	for (i = 0; i < fd->collapse; i++)
	  {
	    tree itype = TREE_TYPE (fd->loops[i].v);
	    if (POINTER_TYPE_P (itype))
	      itype = signed_type_for (itype);
	    t = fold_convert (TREE_TYPE (fd->loops[i].v), fd->loops[i].n1);
	    expand_omp_build_assign (&gsi, fd->loops[i].v, t);
	  }
      }

  /* Remove the GIMPLE_OMP_FOR statement.  */
  gsi_remove (&gsi, true);

  if (!broken_loop)
    {
      /* Code to control the increment goes in the CONT_BB.  */
      gsi = gsi_last_bb (cont_bb);
      stmt = gsi_stmt (gsi);
      gcc_assert (gimple_code (stmt) == GIMPLE_OMP_CONTINUE);

      if (POINTER_TYPE_P (type))
	t = fold_build_pointer_plus (fd->loop.v, fd->loop.step);
      else
	t = fold_build2 (PLUS_EXPR, type, fd->loop.v, fd->loop.step);
      expand_omp_build_assign (&gsi, fd->loop.v, t);

      if (fd->collapse > 1)
	{
	  i = fd->collapse - 1;
	  if (POINTER_TYPE_P (TREE_TYPE (fd->loops[i].v)))
	    {
	      t = fold_convert (sizetype, fd->loops[i].step);
	      t = fold_build_pointer_plus (fd->loops[i].v, t);
	    }
	  else
	    {
	      t = fold_convert (TREE_TYPE (fd->loops[i].v),
				fd->loops[i].step);
	      t = fold_build2 (PLUS_EXPR, TREE_TYPE (fd->loops[i].v),
			       fd->loops[i].v, t);
	    }
	  expand_omp_build_assign (&gsi, fd->loops[i].v, t);

	  for (i = fd->collapse - 1; i > 0; i--)
	    {
	      tree itype = TREE_TYPE (fd->loops[i].v);
	      tree itype2 = TREE_TYPE (fd->loops[i - 1].v);
	      if (POINTER_TYPE_P (itype2))
		itype2 = signed_type_for (itype2);
	      t = build3 (COND_EXPR, itype2,
			  build2 (fd->loops[i].cond_code, boolean_type_node,
				  fd->loops[i].v,
				  fold_convert (itype, fd->loops[i].n2)),
			  build_int_cst (itype2, 0),
			  fold_convert (itype2, fd->loops[i - 1].step));
	      if (POINTER_TYPE_P (TREE_TYPE (fd->loops[i - 1].v)))
		t = fold_build_pointer_plus (fd->loops[i - 1].v, t);
	      else
		t = fold_build2 (PLUS_EXPR, itype2, fd->loops[i - 1].v, t);
	      expand_omp_build_assign (&gsi, fd->loops[i - 1].v, t);

	      t = build3 (COND_EXPR, itype,
			  build2 (fd->loops[i].cond_code, boolean_type_node,
				  fd->loops[i].v,
				  fold_convert (itype, fd->loops[i].n2)),
			  fd->loops[i].v,
			  fold_convert (itype, fd->loops[i].n1));
	      expand_omp_build_assign (&gsi, fd->loops[i].v, t);
	    }
	}

      /* Remove GIMPLE_OMP_CONTINUE.  */
      gsi_remove (&gsi, true);
    }

  /* Emit the condition in L1_BB.  */
  gsi = gsi_start_bb (l1_bb);

  t = fold_convert (type, n2);
  t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				false, GSI_CONTINUE_LINKING);
  tree v = fd->loop.v;
  if (DECL_P (v) && TREE_ADDRESSABLE (v))
    v = force_gimple_operand_gsi (&gsi, v, true, NULL_TREE,
				  false, GSI_CONTINUE_LINKING);
  t = build2 (fd->loop.cond_code, boolean_type_node, v, t);
  cond_stmt = gimple_build_cond_empty (t);
  gsi_insert_after (&gsi, cond_stmt, GSI_CONTINUE_LINKING);
  if (walk_tree (gimple_cond_lhs_ptr (cond_stmt), expand_omp_regimplify_p,
		 NULL, NULL)
      || walk_tree (gimple_cond_rhs_ptr (cond_stmt), expand_omp_regimplify_p,
		    NULL, NULL))
    {
      gsi = gsi_for_stmt (cond_stmt);
      gimple_regimplify_operands (cond_stmt, &gsi);
    }

  /* Remove GIMPLE_OMP_RETURN.  */
  gsi = gsi_last_bb (exit_bb);
  gsi_remove (&gsi, true);

  /* Connect the new blocks.  */
  remove_edge (FALLTHRU_EDGE (entry_bb));

  if (!broken_loop)
    {
      remove_edge (BRANCH_EDGE (entry_bb));
      make_edge (entry_bb, l1_bb, EDGE_FALLTHRU);

      e = BRANCH_EDGE (l1_bb);
      ne = FALLTHRU_EDGE (l1_bb);
      e->flags = EDGE_TRUE_VALUE;
    }
  else
    {
      single_succ_edge (entry_bb)->flags = EDGE_FALLTHRU;

      ne = single_succ_edge (l1_bb);
      e = make_edge (l1_bb, l0_bb, EDGE_TRUE_VALUE);

    }
  ne->flags = EDGE_FALSE_VALUE;
  e->probability = REG_BR_PROB_BASE * 7 / 8;
  ne->probability = REG_BR_PROB_BASE / 8;

  set_immediate_dominator (CDI_DOMINATORS, l1_bb, entry_bb);
  set_immediate_dominator (CDI_DOMINATORS, l2_bb, l2_dom_bb);
  set_immediate_dominator (CDI_DOMINATORS, l0_bb, l1_bb);

  if (!broken_loop)
    {
      struct loop *loop = alloc_loop ();
      loop->header = l1_bb;
      loop->latch = cont_bb;
      add_loop (loop, l1_bb->loop_father);
      if (safelen == NULL_TREE)
	loop->safelen = INT_MAX;
      else
	{
	  safelen = OMP_CLAUSE_SAFELEN_EXPR (safelen);
	  if (TREE_CODE (safelen) != INTEGER_CST)
	    loop->safelen = 0;
	  else if (!tree_fits_uhwi_p (safelen)
		   || tree_to_uhwi (safelen) > INT_MAX)
	    loop->safelen = INT_MAX;
	  else
	    loop->safelen = tree_to_uhwi (safelen);
	  if (loop->safelen == 1)
	    loop->safelen = 0;
	}
      if (simduid)
	{
	  loop->simduid = OMP_CLAUSE__SIMDUID__DECL (simduid);
	  cfun->has_simduid_loops = true;
	}
      /* If not -fno-tree-loop-vectorize, hint that we want to vectorize
	 the loop.  */
      if ((flag_tree_loop_vectorize
	   || (!global_options_set.x_flag_tree_loop_vectorize
               && !global_options_set.x_flag_tree_vectorize))
	  && flag_tree_loop_optimize
	  && loop->safelen > 1)
	{
	  loop->force_vectorize = true;
	  cfun->has_force_vectorize_loops = true;
	}
    }
  else if (simduid)
    cfun->has_simduid_loops = true;
}

/* Taskloop construct is represented after gimplification with
   two GIMPLE_OMP_FOR constructs with GIMPLE_OMP_TASK sandwiched
   in between them.  This routine expands the outer GIMPLE_OMP_FOR,
   which should just compute all the needed loop temporaries
   for GIMPLE_OMP_TASK.  */

static void
expand_omp_taskloop_for_outer (struct omp_region *region,
			       struct omp_for_data *fd,
			       gimple *inner_stmt)
{
  tree type, bias = NULL_TREE;
  basic_block entry_bb, cont_bb, exit_bb;
  gimple_stmt_iterator gsi;
  gassign *assign_stmt;
  tree *counts = NULL;
  int i;

  gcc_assert (inner_stmt);
  gcc_assert (region->cont);
  gcc_assert (gimple_code (inner_stmt) == GIMPLE_OMP_TASK
	      && gimple_omp_task_taskloop_p (inner_stmt));
  type = TREE_TYPE (fd->loop.v);

  /* See if we need to bias by LLONG_MIN.  */
  if (fd->iter_type == long_long_unsigned_type_node
      && TREE_CODE (type) == INTEGER_TYPE
      && !TYPE_UNSIGNED (type))
    {
      tree n1, n2;

      if (fd->loop.cond_code == LT_EXPR)
	{
	  n1 = fd->loop.n1;
	  n2 = fold_build2 (PLUS_EXPR, type, fd->loop.n2, fd->loop.step);
	}
      else
	{
	  n1 = fold_build2 (MINUS_EXPR, type, fd->loop.n2, fd->loop.step);
	  n2 = fd->loop.n1;
	}
      if (TREE_CODE (n1) != INTEGER_CST
	  || TREE_CODE (n2) != INTEGER_CST
	  || ((tree_int_cst_sgn (n1) < 0) ^ (tree_int_cst_sgn (n2) < 0)))
	bias = fold_convert (fd->iter_type, TYPE_MIN_VALUE (type));
    }

  entry_bb = region->entry;
  cont_bb = region->cont;
  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2);
  gcc_assert (BRANCH_EDGE (entry_bb)->dest == FALLTHRU_EDGE (cont_bb)->dest);
  exit_bb = region->exit;

  gsi = gsi_last_bb (entry_bb);
  gimple *for_stmt = gsi_stmt (gsi);
  gcc_assert (gimple_code (for_stmt) == GIMPLE_OMP_FOR);
  if (fd->collapse > 1)
    {
      int first_zero_iter = -1, dummy = -1;
      basic_block zero_iter_bb = NULL, dummy_bb = NULL, l2_dom_bb = NULL;

      counts = XALLOCAVEC (tree, fd->collapse);
      expand_omp_for_init_counts (fd, &gsi, entry_bb, counts,
				  zero_iter_bb, first_zero_iter,
				  dummy_bb, dummy, l2_dom_bb);

      if (zero_iter_bb)
	{
	  /* Some counts[i] vars might be uninitialized if
	     some loop has zero iterations.  But the body shouldn't
	     be executed in that case, so just avoid uninit warnings.  */
	  for (i = first_zero_iter; i < fd->collapse; i++)
	    if (SSA_VAR_P (counts[i]))
	      TREE_NO_WARNING (counts[i]) = 1;
	  gsi_prev (&gsi);
	  edge e = split_block (entry_bb, gsi_stmt (gsi));
	  entry_bb = e->dest;
	  make_edge (zero_iter_bb, entry_bb, EDGE_FALLTHRU);
	  gsi = gsi_last_bb (entry_bb);
	  set_immediate_dominator (CDI_DOMINATORS, entry_bb,
				   get_immediate_dominator (CDI_DOMINATORS,
							    zero_iter_bb));
	}
    }

  tree t0, t1;
  t1 = fd->loop.n2;
  t0 = fd->loop.n1;
  if (POINTER_TYPE_P (TREE_TYPE (t0))
      && TYPE_PRECISION (TREE_TYPE (t0))
	 != TYPE_PRECISION (fd->iter_type))
    {
      /* Avoid casting pointers to integer of a different size.  */
      tree itype = signed_type_for (type);
      t1 = fold_convert (fd->iter_type, fold_convert (itype, t1));
      t0 = fold_convert (fd->iter_type, fold_convert (itype, t0));
    }
  else
    {
      t1 = fold_convert (fd->iter_type, t1);
      t0 = fold_convert (fd->iter_type, t0);
    }
  if (bias)
    {
      t1 = fold_build2 (PLUS_EXPR, fd->iter_type, t1, bias);
      t0 = fold_build2 (PLUS_EXPR, fd->iter_type, t0, bias);
    }

  tree innerc = find_omp_clause (gimple_omp_task_clauses (inner_stmt),
				 OMP_CLAUSE__LOOPTEMP_);
  gcc_assert (innerc);
  tree startvar = OMP_CLAUSE_DECL (innerc);
  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc), OMP_CLAUSE__LOOPTEMP_);
  gcc_assert (innerc);
  tree endvar = OMP_CLAUSE_DECL (innerc);
  if (fd->collapse > 1 && TREE_CODE (fd->loop.n2) != INTEGER_CST)
    {
      gcc_assert (innerc);
      for (i = 1; i < fd->collapse; i++)
	{
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  gcc_assert (innerc);
	}
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      if (innerc)
	{
	  /* If needed (inner taskloop has lastprivate clause), propagate
	     down the total number of iterations.  */
	  tree t = force_gimple_operand_gsi (&gsi, fd->loop.n2, false,
					     NULL_TREE, false,
					     GSI_CONTINUE_LINKING);
	  assign_stmt = gimple_build_assign (OMP_CLAUSE_DECL (innerc), t);
	  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
	}
    }

  t0 = force_gimple_operand_gsi (&gsi, t0, false, NULL_TREE, false,
				 GSI_CONTINUE_LINKING);
  assign_stmt = gimple_build_assign (startvar, t0);
  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);

  t1 = force_gimple_operand_gsi (&gsi, t1, false, NULL_TREE, false,
				 GSI_CONTINUE_LINKING);
  assign_stmt = gimple_build_assign (endvar, t1);
  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
  if (fd->collapse > 1)
    expand_omp_for_init_vars (fd, &gsi, counts, inner_stmt, startvar);

  /* Remove the GIMPLE_OMP_FOR statement.  */
  gsi = gsi_for_stmt (for_stmt);
  gsi_remove (&gsi, true);

  gsi = gsi_last_bb (cont_bb);
  gsi_remove (&gsi, true);

  gsi = gsi_last_bb (exit_bb);
  gsi_remove (&gsi, true);

  FALLTHRU_EDGE (entry_bb)->probability = REG_BR_PROB_BASE;
  remove_edge (BRANCH_EDGE (entry_bb));
  FALLTHRU_EDGE (cont_bb)->probability = REG_BR_PROB_BASE;
  remove_edge (BRANCH_EDGE (cont_bb));
  set_immediate_dominator (CDI_DOMINATORS, exit_bb, cont_bb);
  set_immediate_dominator (CDI_DOMINATORS, region->entry,
			   recompute_dominator (CDI_DOMINATORS, region->entry));
}

/* Taskloop construct is represented after gimplification with
   two GIMPLE_OMP_FOR constructs with GIMPLE_OMP_TASK sandwiched
   in between them.  This routine expands the inner GIMPLE_OMP_FOR.
   GOMP_taskloop{,_ull} function arranges for each task to be given just
   a single range of iterations.  */

static void
expand_omp_taskloop_for_inner (struct omp_region *region,
			       struct omp_for_data *fd,
			       gimple *inner_stmt)
{
  tree e, t, type, itype, vmain, vback, bias = NULL_TREE;
  basic_block entry_bb, exit_bb, body_bb, cont_bb, collapse_bb = NULL;
  basic_block fin_bb;
  gimple_stmt_iterator gsi;
  edge ep;
  bool broken_loop = region->cont == NULL;
  tree *counts = NULL;
  tree n1, n2, step;

  itype = type = TREE_TYPE (fd->loop.v);
  if (POINTER_TYPE_P (type))
    itype = signed_type_for (type);

  /* See if we need to bias by LLONG_MIN.  */
  if (fd->iter_type == long_long_unsigned_type_node
      && TREE_CODE (type) == INTEGER_TYPE
      && !TYPE_UNSIGNED (type))
    {
      tree n1, n2;

      if (fd->loop.cond_code == LT_EXPR)
	{
	  n1 = fd->loop.n1;
	  n2 = fold_build2 (PLUS_EXPR, type, fd->loop.n2, fd->loop.step);
	}
      else
	{
	  n1 = fold_build2 (MINUS_EXPR, type, fd->loop.n2, fd->loop.step);
	  n2 = fd->loop.n1;
	}
      if (TREE_CODE (n1) != INTEGER_CST
	  || TREE_CODE (n2) != INTEGER_CST
	  || ((tree_int_cst_sgn (n1) < 0) ^ (tree_int_cst_sgn (n2) < 0)))
	bias = fold_convert (fd->iter_type, TYPE_MIN_VALUE (type));
    }

  entry_bb = region->entry;
  cont_bb = region->cont;
  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2);
  fin_bb = BRANCH_EDGE (entry_bb)->dest;
  gcc_assert (broken_loop
	      || (fin_bb == FALLTHRU_EDGE (cont_bb)->dest));
  body_bb = FALLTHRU_EDGE (entry_bb)->dest;
  if (!broken_loop)
    {
      gcc_assert (BRANCH_EDGE (cont_bb)->dest == body_bb);
      gcc_assert (EDGE_COUNT (cont_bb->succs) == 2);
    }
  exit_bb = region->exit;

  /* Iteration space partitioning goes in ENTRY_BB.  */
  gsi = gsi_last_bb (entry_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_FOR);

  if (fd->collapse > 1)
    {
      int first_zero_iter = -1, dummy = -1;
      basic_block l2_dom_bb = NULL, dummy_bb = NULL;

      counts = XALLOCAVEC (tree, fd->collapse);
      expand_omp_for_init_counts (fd, &gsi, entry_bb, counts,
				  fin_bb, first_zero_iter,
				  dummy_bb, dummy, l2_dom_bb);
      t = NULL_TREE;
    }
  else
    t = integer_one_node;

  step = fd->loop.step;
  tree innerc = find_omp_clause (gimple_omp_for_clauses (fd->for_stmt),
				 OMP_CLAUSE__LOOPTEMP_);
  gcc_assert (innerc);
  n1 = OMP_CLAUSE_DECL (innerc);
  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc), OMP_CLAUSE__LOOPTEMP_);
  gcc_assert (innerc);
  n2 = OMP_CLAUSE_DECL (innerc);
  if (bias)
    {
      n1 = fold_build2 (PLUS_EXPR, fd->iter_type, n1, bias);
      n2 = fold_build2 (PLUS_EXPR, fd->iter_type, n2, bias);
    }
  n1 = force_gimple_operand_gsi (&gsi, fold_convert (type, n1),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  n2 = force_gimple_operand_gsi (&gsi, fold_convert (itype, n2),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  step = force_gimple_operand_gsi (&gsi, fold_convert (itype, step),
				   true, NULL_TREE, true, GSI_SAME_STMT);

  tree startvar = fd->loop.v;
  tree endvar = NULL_TREE;

  if (gimple_omp_for_combined_p (fd->for_stmt))
    {
      tree clauses = gimple_omp_for_clauses (inner_stmt);
      tree innerc = find_omp_clause (clauses, OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      startvar = OMP_CLAUSE_DECL (innerc);
      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				OMP_CLAUSE__LOOPTEMP_);
      gcc_assert (innerc);
      endvar = OMP_CLAUSE_DECL (innerc);
    }
  t = fold_convert (TREE_TYPE (startvar), n1);
  t = force_gimple_operand_gsi (&gsi, t,
				DECL_P (startvar)
				&& TREE_ADDRESSABLE (startvar),
				NULL_TREE, false, GSI_CONTINUE_LINKING);
  gimple *assign_stmt = gimple_build_assign (startvar, t);
  gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);

  t = fold_convert (TREE_TYPE (startvar), n2);
  e = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				false, GSI_CONTINUE_LINKING);
  if (endvar)
    {
      assign_stmt = gimple_build_assign (endvar, e);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
      if (useless_type_conversion_p (TREE_TYPE (fd->loop.v), TREE_TYPE (e)))
	assign_stmt = gimple_build_assign (fd->loop.v, e);
      else
	assign_stmt = gimple_build_assign (fd->loop.v, NOP_EXPR, e);
      gsi_insert_after (&gsi, assign_stmt, GSI_CONTINUE_LINKING);
    }
  if (fd->collapse > 1)
    expand_omp_for_init_vars (fd, &gsi, counts, inner_stmt, startvar);

  if (!broken_loop)
    {
      /* The code controlling the sequential loop replaces the
	 GIMPLE_OMP_CONTINUE.  */
      gsi = gsi_last_bb (cont_bb);
      gomp_continue *cont_stmt = as_a <gomp_continue *> (gsi_stmt (gsi));
      gcc_assert (gimple_code (cont_stmt) == GIMPLE_OMP_CONTINUE);
      vmain = gimple_omp_continue_control_use (cont_stmt);
      vback = gimple_omp_continue_control_def (cont_stmt);

      if (!gimple_omp_for_combined_p (fd->for_stmt))
	{
	  if (POINTER_TYPE_P (type))
	    t = fold_build_pointer_plus (vmain, step);
	  else
	    t = fold_build2 (PLUS_EXPR, type, vmain, step);
	  t = force_gimple_operand_gsi (&gsi, t,
					DECL_P (vback)
					&& TREE_ADDRESSABLE (vback),
					NULL_TREE, true, GSI_SAME_STMT);
	  assign_stmt = gimple_build_assign (vback, t);
	  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

	  t = build2 (fd->loop.cond_code, boolean_type_node,
		      DECL_P (vback) && TREE_ADDRESSABLE (vback)
		      ? t : vback, e);
	  gsi_insert_before (&gsi, gimple_build_cond_empty (t), GSI_SAME_STMT);
	}

      /* Remove the GIMPLE_OMP_CONTINUE statement.  */
      gsi_remove (&gsi, true);

      if (fd->collapse > 1 && !gimple_omp_for_combined_p (fd->for_stmt))
	collapse_bb = extract_omp_for_update_vars (fd, cont_bb, body_bb);
    }

  /* Remove the GIMPLE_OMP_FOR statement.  */
  gsi = gsi_for_stmt (fd->for_stmt);
  gsi_remove (&gsi, true);

  /* Remove the GIMPLE_OMP_RETURN statement.  */
  gsi = gsi_last_bb (exit_bb);
  gsi_remove (&gsi, true);

  FALLTHRU_EDGE (entry_bb)->probability = REG_BR_PROB_BASE;
  if (!broken_loop)
    remove_edge (BRANCH_EDGE (entry_bb));
  else
    {
      remove_edge_and_dominated_blocks (BRANCH_EDGE (entry_bb));
      region->outer->cont = NULL;
    }

  /* Connect all the blocks.  */
  if (!broken_loop)
    {
      ep = find_edge (cont_bb, body_bb);
      if (gimple_omp_for_combined_p (fd->for_stmt))
	{
	  remove_edge (ep);
	  ep = NULL;
	}
      else if (fd->collapse > 1)
	{
	  remove_edge (ep);
	  ep = make_edge (cont_bb, collapse_bb, EDGE_TRUE_VALUE);
	}
      else
	ep->flags = EDGE_TRUE_VALUE;
      find_edge (cont_bb, fin_bb)->flags
	= ep ? EDGE_FALSE_VALUE : EDGE_FALLTHRU;
    }

  set_immediate_dominator (CDI_DOMINATORS, body_bb,
			   recompute_dominator (CDI_DOMINATORS, body_bb));
  if (!broken_loop)
    set_immediate_dominator (CDI_DOMINATORS, fin_bb,
			     recompute_dominator (CDI_DOMINATORS, fin_bb));

  if (!broken_loop && !gimple_omp_for_combined_p (fd->for_stmt))
    {
      struct loop *loop = alloc_loop ();
      loop->header = body_bb;
      if (collapse_bb == NULL)
	loop->latch = cont_bb;
      add_loop (loop, body_bb->loop_father);
    }
}

/* A subroutine of expand_omp_for.  Generate code for an OpenACC
   partitioned loop.  The lowering here is abstracted, in that the
   loop parameters are passed through internal functions, which are
   further lowered by oacc_device_lower, once we get to the target
   compiler.  The loop is of the form:

   for (V = B; V LTGT E; V += S) {BODY}

   where LTGT is < or >.  We may have a specified chunking size, CHUNKING
   (constant 0 for no chunking) and we will have a GWV partitioning
   mask, specifying dimensions over which the loop is to be
   partitioned (see note below).  We generate code that looks like:

   <entry_bb> [incoming FALL->body, BRANCH->exit]
     typedef signedintify (typeof (V)) T;  // underlying signed integral type
     T range = E - B;
     T chunk_no = 0;
     T DIR = LTGT == '<' ? +1 : -1;
     T chunk_max = GOACC_LOOP_CHUNK (dir, range, S, CHUNK_SIZE, GWV);
     T step = GOACC_LOOP_STEP (dir, range, S, CHUNK_SIZE, GWV);

   <head_bb> [created by splitting end of entry_bb]
     T offset = GOACC_LOOP_OFFSET (dir, range, S, CHUNK_SIZE, GWV, chunk_no);
     T bound = GOACC_LOOP_BOUND (dir, range, S, CHUNK_SIZE, GWV, offset);
     if (!(offset LTGT bound)) goto bottom_bb;

   <body_bb> [incoming]
     V = B + offset;
     {BODY}

   <cont_bb> [incoming, may == body_bb FALL->exit_bb, BRANCH->body_bb]
     offset += step;
     if (offset LTGT bound) goto body_bb; [*]

   <bottom_bb> [created by splitting start of exit_bb] insert BRANCH->head_bb
     chunk_no++;
     if (chunk < chunk_max) goto head_bb;

   <exit_bb> [incoming]
     V = B + ((range -/+ 1) / S +/- 1) * S [*]

   [*] Needed if V live at end of loop

   Note: CHUNKING & GWV mask are specified explicitly here.  This is a
   transition, and will be specified by a more general mechanism shortly.
 */

static void
expand_oacc_for (struct omp_region *region, struct omp_for_data *fd)
{
  tree v = fd->loop.v;
  enum tree_code cond_code = fd->loop.cond_code;
  enum tree_code plus_code = PLUS_EXPR;

  tree chunk_size = integer_minus_one_node;
  tree gwv = integer_zero_node;
  tree iter_type = TREE_TYPE (v);
  tree diff_type = iter_type;
  tree plus_type = iter_type;
  struct oacc_collapse *counts = NULL;

  gcc_checking_assert (gimple_omp_for_kind (fd->for_stmt)
		       == GF_OMP_FOR_KIND_OACC_LOOP);
  gcc_assert (!gimple_omp_for_combined_into_p (fd->for_stmt));
  gcc_assert (cond_code == LT_EXPR || cond_code == GT_EXPR);

  if (POINTER_TYPE_P (iter_type))
    {
      plus_code = POINTER_PLUS_EXPR;
      plus_type = sizetype;
    }
  if (POINTER_TYPE_P (diff_type) || TYPE_UNSIGNED (diff_type))
    diff_type = signed_type_for (diff_type);

  basic_block entry_bb = region->entry; /* BB ending in OMP_FOR */
  basic_block exit_bb = region->exit; /* BB ending in OMP_RETURN */
  basic_block cont_bb = region->cont; /* BB ending in OMP_CONTINUE  */
  basic_block bottom_bb = NULL;

  /* entry_bb has two sucessors; the branch edge is to the exit
     block,  fallthrough edge to body.  */
  gcc_assert (EDGE_COUNT (entry_bb->succs) == 2
	      && BRANCH_EDGE (entry_bb)->dest == exit_bb);

  /* If cont_bb non-NULL, it has 2 successors.  The branch successor is
     body_bb, or to a block whose only successor is the body_bb.  Its
     fallthrough successor is the final block (same as the branch
     successor of the entry_bb).  */
  if (cont_bb)
    {
      basic_block body_bb = FALLTHRU_EDGE (entry_bb)->dest;
      basic_block bed = BRANCH_EDGE (cont_bb)->dest;

      gcc_assert (FALLTHRU_EDGE (cont_bb)->dest == exit_bb);
      gcc_assert (bed == body_bb || single_succ_edge (bed)->dest == body_bb);
    }
  else
    gcc_assert (!gimple_in_ssa_p (cfun));

  /* The exit block only has entry_bb and cont_bb as predecessors.  */
  gcc_assert (EDGE_COUNT (exit_bb->preds) == 1 + (cont_bb != NULL));

  tree chunk_no;
  tree chunk_max = NULL_TREE;
  tree bound, offset;
  tree step = create_tmp_var (diff_type, ".step");
  bool up = cond_code == LT_EXPR;
  tree dir = build_int_cst (diff_type, up ? +1 : -1);
  bool chunking = !gimple_in_ssa_p (cfun);;
  bool negating;

  /* SSA instances.  */
  tree offset_incr = NULL_TREE;
  tree offset_init = NULL_TREE;

  gimple_stmt_iterator gsi;
  gassign *ass;
  gcall *call;
  gimple *stmt;
  tree expr;
  location_t loc;
  edge split, be, fte;

  /* Split the end of entry_bb to create head_bb.  */
  split = split_block (entry_bb, last_stmt (entry_bb));
  basic_block head_bb = split->dest;
  entry_bb = split->src;

  /* Chunk setup goes at end of entry_bb, replacing the omp_for.  */
  gsi = gsi_last_bb (entry_bb);
  gomp_for *for_stmt = as_a <gomp_for *> (gsi_stmt (gsi));
  loc = gimple_location (for_stmt);

  if (gimple_in_ssa_p (cfun))
    {
      offset_init = gimple_omp_for_index (for_stmt, 0);
      gcc_assert (integer_zerop (fd->loop.n1));
      /* The SSA parallelizer does gang parallelism.  */
      gwv = build_int_cst (integer_type_node, GOMP_DIM_MASK (GOMP_DIM_GANG));
    }

  if (fd->collapse > 1)
    {
      counts = XALLOCAVEC (struct oacc_collapse, fd->collapse);
      tree total = expand_oacc_collapse_init (fd, &gsi, counts,
					      TREE_TYPE (fd->loop.n2));

      if (SSA_VAR_P (fd->loop.n2))
	{
	  total = force_gimple_operand_gsi (&gsi, total, false, NULL_TREE,
					    true, GSI_SAME_STMT);
	  ass = gimple_build_assign (fd->loop.n2, total);
	  gsi_insert_before (&gsi, ass, GSI_SAME_STMT);
	}
      
    }

  tree b = fd->loop.n1;
  tree e = fd->loop.n2;
  tree s = fd->loop.step;

  b = force_gimple_operand_gsi (&gsi, b, true, NULL_TREE, true, GSI_SAME_STMT);
  e = force_gimple_operand_gsi (&gsi, e, true, NULL_TREE, true, GSI_SAME_STMT);

  /* Convert the step, avoiding possible unsigned->signed overflow. */
  negating = !up && TYPE_UNSIGNED (TREE_TYPE (s));
  if (negating)
    s = fold_build1 (NEGATE_EXPR, TREE_TYPE (s), s);
  s = fold_convert (diff_type, s);
  if (negating)
    s = fold_build1 (NEGATE_EXPR, diff_type, s);
  s = force_gimple_operand_gsi (&gsi, s, true, NULL_TREE, true, GSI_SAME_STMT);

  if (!chunking)
    chunk_size = integer_zero_node;
  expr = fold_convert (diff_type, chunk_size);
  chunk_size = force_gimple_operand_gsi (&gsi, expr, true,
					 NULL_TREE, true, GSI_SAME_STMT);
  /* Determine the range, avoiding possible unsigned->signed overflow. */
  negating = !up && TYPE_UNSIGNED (iter_type);
  expr = fold_build2 (MINUS_EXPR, plus_type,
		      fold_convert (plus_type, negating ? b : e),
		      fold_convert (plus_type, negating ? e : b));
  expr = fold_convert (diff_type, expr);
  if (negating)
    expr = fold_build1 (NEGATE_EXPR, diff_type, expr);
  tree range = force_gimple_operand_gsi (&gsi, expr, true,
					 NULL_TREE, true, GSI_SAME_STMT);

  chunk_no = build_int_cst (diff_type, 0);
  if (chunking)
    {
      gcc_assert (!gimple_in_ssa_p (cfun));

      expr = chunk_no;
      chunk_max = create_tmp_var (diff_type, ".chunk_max");
      chunk_no = create_tmp_var (diff_type, ".chunk_no");

      ass = gimple_build_assign (chunk_no, expr);
      gsi_insert_before (&gsi, ass, GSI_SAME_STMT);

      call = gimple_build_call_internal (IFN_GOACC_LOOP, 6,
					 build_int_cst (integer_type_node,
							IFN_GOACC_LOOP_CHUNKS),
					 dir, range, s, chunk_size, gwv);
      gimple_call_set_lhs (call, chunk_max);
      gimple_set_location (call, loc);
      gsi_insert_before (&gsi, call, GSI_SAME_STMT);
    }
  else
    chunk_size = chunk_no;

  call = gimple_build_call_internal (IFN_GOACC_LOOP, 6,
				     build_int_cst (integer_type_node,
						    IFN_GOACC_LOOP_STEP),
				     dir, range, s, chunk_size, gwv);
  gimple_call_set_lhs (call, step);
  gimple_set_location (call, loc);
  gsi_insert_before (&gsi, call, GSI_SAME_STMT);

  /* Remove the GIMPLE_OMP_FOR.  */
  gsi_remove (&gsi, true);

  /* Fixup edges from head_bb */
  be = BRANCH_EDGE (head_bb);
  fte = FALLTHRU_EDGE (head_bb);
  be->flags |= EDGE_FALSE_VALUE;
  fte->flags ^= EDGE_FALLTHRU | EDGE_TRUE_VALUE;

  basic_block body_bb = fte->dest;

  if (gimple_in_ssa_p (cfun))
    {
      gsi = gsi_last_bb (cont_bb);
      gomp_continue *cont_stmt = as_a <gomp_continue *> (gsi_stmt (gsi));

      offset = gimple_omp_continue_control_use (cont_stmt);
      offset_incr = gimple_omp_continue_control_def (cont_stmt);
    }
  else
    {
      offset = create_tmp_var (diff_type, ".offset");
      offset_init = offset_incr = offset;
    }
  bound = create_tmp_var (TREE_TYPE (offset), ".bound");

  /* Loop offset & bound go into head_bb.  */
  gsi = gsi_start_bb (head_bb);

  call = gimple_build_call_internal (IFN_GOACC_LOOP, 7,
				     build_int_cst (integer_type_node,
						    IFN_GOACC_LOOP_OFFSET),
				     dir, range, s,
				     chunk_size, gwv, chunk_no);
  gimple_call_set_lhs (call, offset_init);
  gimple_set_location (call, loc);
  gsi_insert_after (&gsi, call, GSI_CONTINUE_LINKING);

  call = gimple_build_call_internal (IFN_GOACC_LOOP, 7,
				     build_int_cst (integer_type_node,
						    IFN_GOACC_LOOP_BOUND),
				     dir, range, s,
				     chunk_size, gwv, offset_init);
  gimple_call_set_lhs (call, bound);
  gimple_set_location (call, loc);
  gsi_insert_after (&gsi, call, GSI_CONTINUE_LINKING);

  expr = build2 (cond_code, boolean_type_node, offset_init, bound);
  gsi_insert_after (&gsi, gimple_build_cond_empty (expr),
		    GSI_CONTINUE_LINKING);

  /* V assignment goes into body_bb.  */
  if (!gimple_in_ssa_p (cfun))
    {
      gsi = gsi_start_bb (body_bb);

      expr = build2 (plus_code, iter_type, b,
		     fold_convert (plus_type, offset));
      expr = force_gimple_operand_gsi (&gsi, expr, false, NULL_TREE,
				       true, GSI_SAME_STMT);
      ass = gimple_build_assign (v, expr);
      gsi_insert_before (&gsi, ass, GSI_SAME_STMT);
      if (fd->collapse > 1)
	expand_oacc_collapse_vars (fd, &gsi, counts, v);
    }

  /* Loop increment goes into cont_bb.  If this is not a loop, we
     will have spawned threads as if it was, and each one will
     execute one iteration.  The specification is not explicit about
     whether such constructs are ill-formed or not, and they can
     occur, especially when noreturn routines are involved.  */
  if (cont_bb)
    {
      gsi = gsi_last_bb (cont_bb);
      gomp_continue *cont_stmt = as_a <gomp_continue *> (gsi_stmt (gsi));
      loc = gimple_location (cont_stmt);

      /* Increment offset.  */
      if (gimple_in_ssa_p (cfun))
	expr= build2 (plus_code, iter_type, offset,
		      fold_convert (plus_type, step));
      else
	expr = build2 (PLUS_EXPR, diff_type, offset, step);
      expr = force_gimple_operand_gsi (&gsi, expr, false, NULL_TREE,
				       true, GSI_SAME_STMT);
      ass = gimple_build_assign (offset_incr, expr);
      gsi_insert_before (&gsi, ass, GSI_SAME_STMT);
      expr = build2 (cond_code, boolean_type_node, offset_incr, bound);
      gsi_insert_before (&gsi, gimple_build_cond_empty (expr), GSI_SAME_STMT);

      /*  Remove the GIMPLE_OMP_CONTINUE.  */
      gsi_remove (&gsi, true);

      /* Fixup edges from cont_bb */
      be = BRANCH_EDGE (cont_bb);
      fte = FALLTHRU_EDGE (cont_bb);
      be->flags |= EDGE_TRUE_VALUE;
      fte->flags ^= EDGE_FALLTHRU | EDGE_FALSE_VALUE;

      if (chunking)
	{
	  /* Split the beginning of exit_bb to make bottom_bb.  We
	     need to insert a nop at the start, because splitting is
  	     after a stmt, not before.  */
	  gsi = gsi_start_bb (exit_bb);
	  stmt = gimple_build_nop ();
	  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
	  split = split_block (exit_bb, stmt);
	  bottom_bb = split->src;
	  exit_bb = split->dest;
	  gsi = gsi_last_bb (bottom_bb);

	  /* Chunk increment and test goes into bottom_bb.  */
	  expr = build2 (PLUS_EXPR, diff_type, chunk_no,
			 build_int_cst (diff_type, 1));
	  ass = gimple_build_assign (chunk_no, expr);
	  gsi_insert_after (&gsi, ass, GSI_CONTINUE_LINKING);

	  /* Chunk test at end of bottom_bb.  */
	  expr = build2 (LT_EXPR, boolean_type_node, chunk_no, chunk_max);
	  gsi_insert_after (&gsi, gimple_build_cond_empty (expr),
			    GSI_CONTINUE_LINKING);

	  /* Fixup edges from bottom_bb. */
	  split->flags ^= EDGE_FALLTHRU | EDGE_FALSE_VALUE;
	  make_edge (bottom_bb, head_bb, EDGE_TRUE_VALUE);
	}
    }

  gsi = gsi_last_bb (exit_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_RETURN);
  loc = gimple_location (gsi_stmt (gsi));

  if (!gimple_in_ssa_p (cfun))
    {
      /* Insert the final value of V, in case it is live.  This is the
	 value for the only thread that survives past the join.  */
      expr = fold_build2 (MINUS_EXPR, diff_type, range, dir);
      expr = fold_build2 (PLUS_EXPR, diff_type, expr, s);
      expr = fold_build2 (TRUNC_DIV_EXPR, diff_type, expr, s);
      expr = fold_build2 (MULT_EXPR, diff_type, expr, s);
      expr = build2 (plus_code, iter_type, b, fold_convert (plus_type, expr));
      expr = force_gimple_operand_gsi (&gsi, expr, false, NULL_TREE,
				       true, GSI_SAME_STMT);
      ass = gimple_build_assign (v, expr);
      gsi_insert_before (&gsi, ass, GSI_SAME_STMT);
    }

  /* Remove the OMP_RETURN. */
  gsi_remove (&gsi, true);

  if (cont_bb)
    {
      /* We now have one or two nested loops.  Update the loop
	 structures.  */
      struct loop *parent = entry_bb->loop_father;
      struct loop *body = body_bb->loop_father;
      
      if (chunking)
	{
	  struct loop *chunk_loop = alloc_loop ();
	  chunk_loop->header = head_bb;
	  chunk_loop->latch = bottom_bb;
	  add_loop (chunk_loop, parent);
	  parent = chunk_loop;
	}
      else if (parent != body)
	{
	  gcc_assert (body->header == body_bb);
	  gcc_assert (body->latch == cont_bb
		      || single_pred (body->latch) == cont_bb);
	  parent = NULL;
	}

      if (parent)
	{
	  struct loop *body_loop = alloc_loop ();
	  body_loop->header = body_bb;
	  body_loop->latch = cont_bb;
	  add_loop (body_loop, parent);
	}
    }
}

/* Expand the OMP loop defined by REGION.  */

static void
expand_omp_for (struct omp_region *region, gimple *inner_stmt)
{
  struct omp_for_data fd;
  struct omp_for_data_loop *loops;

  loops
    = (struct omp_for_data_loop *)
      alloca (gimple_omp_for_collapse (last_stmt (region->entry))
	      * sizeof (struct omp_for_data_loop));
  extract_omp_for_data (as_a <gomp_for *> (last_stmt (region->entry)),
			&fd, loops);
  region->sched_kind = fd.sched_kind;
  region->sched_modifiers = fd.sched_modifiers;

  gcc_assert (EDGE_COUNT (region->entry->succs) == 2);
  BRANCH_EDGE (region->entry)->flags &= ~EDGE_ABNORMAL;
  FALLTHRU_EDGE (region->entry)->flags &= ~EDGE_ABNORMAL;
  if (region->cont)
    {
      gcc_assert (EDGE_COUNT (region->cont->succs) == 2);
      BRANCH_EDGE (region->cont)->flags &= ~EDGE_ABNORMAL;
      FALLTHRU_EDGE (region->cont)->flags &= ~EDGE_ABNORMAL;
    }
  else
    /* If there isn't a continue then this is a degerate case where
       the introduction of abnormal edges during lowering will prevent
       original loops from being detected.  Fix that up.  */
    loops_state_set (LOOPS_NEED_FIXUP);

  if (gimple_omp_for_kind (fd.for_stmt) & GF_OMP_FOR_SIMD)
    expand_omp_simd (region, &fd);
  else if (gimple_omp_for_kind (fd.for_stmt) == GF_OMP_FOR_KIND_CILKFOR)
    expand_cilk_for (region, &fd);
  else if (gimple_omp_for_kind (fd.for_stmt) == GF_OMP_FOR_KIND_OACC_LOOP)
    {
      gcc_assert (!inner_stmt);
      expand_oacc_for (region, &fd);
    }
  else if (gimple_omp_for_kind (fd.for_stmt) == GF_OMP_FOR_KIND_TASKLOOP)
    {
      if (gimple_omp_for_combined_into_p (fd.for_stmt))
	expand_omp_taskloop_for_inner (region, &fd, inner_stmt);
      else
	expand_omp_taskloop_for_outer (region, &fd, inner_stmt);
    }
  else if (fd.sched_kind == OMP_CLAUSE_SCHEDULE_STATIC
	   && !fd.have_ordered)
    {
      if (fd.chunk_size == NULL)
	expand_omp_for_static_nochunk (region, &fd, inner_stmt);
      else
	expand_omp_for_static_chunk (region, &fd, inner_stmt);
    }
  else
    {
      int fn_index, start_ix, next_ix;

      gcc_assert (gimple_omp_for_kind (fd.for_stmt)
		  == GF_OMP_FOR_KIND_FOR);
      if (fd.chunk_size == NULL
	  && fd.sched_kind == OMP_CLAUSE_SCHEDULE_STATIC)
	fd.chunk_size = integer_zero_node;
      gcc_assert (fd.sched_kind != OMP_CLAUSE_SCHEDULE_AUTO);
      switch (fd.sched_kind)
	{
	case OMP_CLAUSE_SCHEDULE_RUNTIME:
	  fn_index = 3;
	  break;
	case OMP_CLAUSE_SCHEDULE_DYNAMIC:
	case OMP_CLAUSE_SCHEDULE_GUIDED:
	  if ((fd.sched_modifiers & OMP_CLAUSE_SCHEDULE_NONMONOTONIC)
	      && !fd.ordered
	      && !fd.have_ordered)
	    {
	      fn_index = 3 + fd.sched_kind;
	      break;
	    }
	  /* FALLTHRU */
	default:
	  fn_index = fd.sched_kind;
	  break;
	}
      if (!fd.ordered)
	fn_index += fd.have_ordered * 6;
      if (fd.ordered)
	start_ix = ((int)BUILT_IN_GOMP_LOOP_DOACROSS_STATIC_START) + fn_index;
      else
	start_ix = ((int)BUILT_IN_GOMP_LOOP_STATIC_START) + fn_index;
      next_ix = ((int)BUILT_IN_GOMP_LOOP_STATIC_NEXT) + fn_index;
      if (fd.iter_type == long_long_unsigned_type_node)
	{
	  start_ix += ((int)BUILT_IN_GOMP_LOOP_ULL_STATIC_START
			- (int)BUILT_IN_GOMP_LOOP_STATIC_START);
	  next_ix += ((int)BUILT_IN_GOMP_LOOP_ULL_STATIC_NEXT
		      - (int)BUILT_IN_GOMP_LOOP_STATIC_NEXT);
	}
      expand_omp_for_generic (region, &fd, (enum built_in_function) start_ix,
			      (enum built_in_function) next_ix, inner_stmt);
    }

  if (gimple_in_ssa_p (cfun))
    update_ssa (TODO_update_ssa_only_virtuals);
}


/* Expand code for an OpenMP sections directive.  In pseudo code, we generate

	v = GOMP_sections_start (n);
    L0:
	switch (v)
	  {
	  case 0:
	    goto L2;
	  case 1:
	    section 1;
	    goto L1;
	  case 2:
	    ...
	  case n:
	    ...
	  default:
	    abort ();
	  }
    L1:
	v = GOMP_sections_next ();
	goto L0;
    L2:
	reduction;

    If this is a combined parallel sections, replace the call to
    GOMP_sections_start with call to GOMP_sections_next.  */

static void
expand_omp_sections (struct omp_region *region)
{
  tree t, u, vin = NULL, vmain, vnext, l2;
  unsigned len;
  basic_block entry_bb, l0_bb, l1_bb, l2_bb, default_bb;
  gimple_stmt_iterator si, switch_si;
  gomp_sections *sections_stmt;
  gimple *stmt;
  gomp_continue *cont;
  edge_iterator ei;
  edge e;
  struct omp_region *inner;
  unsigned i, casei;
  bool exit_reachable = region->cont != NULL;

  gcc_assert (region->exit != NULL);
  entry_bb = region->entry;
  l0_bb = single_succ (entry_bb);
  l1_bb = region->cont;
  l2_bb = region->exit;
  if (single_pred_p (l2_bb) && single_pred (l2_bb) == l0_bb)
    l2 = gimple_block_label (l2_bb);
  else
    {
      /* This can happen if there are reductions.  */
      len = EDGE_COUNT (l0_bb->succs);
      gcc_assert (len > 0);
      e = EDGE_SUCC (l0_bb, len - 1);
      si = gsi_last_bb (e->dest);
      l2 = NULL_TREE;
      if (gsi_end_p (si)
          || gimple_code (gsi_stmt (si)) != GIMPLE_OMP_SECTION)
	l2 = gimple_block_label (e->dest);
      else
	FOR_EACH_EDGE (e, ei, l0_bb->succs)
	  {
	    si = gsi_last_bb (e->dest);
	    if (gsi_end_p (si)
		|| gimple_code (gsi_stmt (si)) != GIMPLE_OMP_SECTION)
	      {
		l2 = gimple_block_label (e->dest);
		break;
	      }
	  }
    }
  if (exit_reachable)
    default_bb = create_empty_bb (l1_bb->prev_bb);
  else
    default_bb = create_empty_bb (l0_bb);

  /* We will build a switch() with enough cases for all the
     GIMPLE_OMP_SECTION regions, a '0' case to handle the end of more work
     and a default case to abort if something goes wrong.  */
  len = EDGE_COUNT (l0_bb->succs);

  /* Use vec::quick_push on label_vec throughout, since we know the size
     in advance.  */
  auto_vec<tree> label_vec (len);

  /* The call to GOMP_sections_start goes in ENTRY_BB, replacing the
     GIMPLE_OMP_SECTIONS statement.  */
  si = gsi_last_bb (entry_bb);
  sections_stmt = as_a <gomp_sections *> (gsi_stmt (si));
  gcc_assert (gimple_code (sections_stmt) == GIMPLE_OMP_SECTIONS);
  vin = gimple_omp_sections_control (sections_stmt);
  if (!is_combined_parallel (region))
    {
      /* If we are not inside a combined parallel+sections region,
	 call GOMP_sections_start.  */
      t = build_int_cst (unsigned_type_node, len - 1);
      u = builtin_decl_explicit (BUILT_IN_GOMP_SECTIONS_START);
      stmt = gimple_build_call (u, 1, t);
    }
  else
    {
      /* Otherwise, call GOMP_sections_next.  */
      u = builtin_decl_explicit (BUILT_IN_GOMP_SECTIONS_NEXT);
      stmt = gimple_build_call (u, 0);
    }
  gimple_call_set_lhs (stmt, vin);
  gsi_insert_after (&si, stmt, GSI_SAME_STMT);
  gsi_remove (&si, true);

  /* The switch() statement replacing GIMPLE_OMP_SECTIONS_SWITCH goes in
     L0_BB.  */
  switch_si = gsi_last_bb (l0_bb);
  gcc_assert (gimple_code (gsi_stmt (switch_si)) == GIMPLE_OMP_SECTIONS_SWITCH);
  if (exit_reachable)
    {
      cont = as_a <gomp_continue *> (last_stmt (l1_bb));
      gcc_assert (gimple_code (cont) == GIMPLE_OMP_CONTINUE);
      vmain = gimple_omp_continue_control_use (cont);
      vnext = gimple_omp_continue_control_def (cont);
    }
  else
    {
      vmain = vin;
      vnext = NULL_TREE;
    }

  t = build_case_label (build_int_cst (unsigned_type_node, 0), NULL, l2);
  label_vec.quick_push (t);
  i = 1;

  /* Convert each GIMPLE_OMP_SECTION into a CASE_LABEL_EXPR.  */
  for (inner = region->inner, casei = 1;
       inner;
       inner = inner->next, i++, casei++)
    {
      basic_block s_entry_bb, s_exit_bb;

      /* Skip optional reduction region.  */
      if (inner->type == GIMPLE_OMP_ATOMIC_LOAD)
	{
	  --i;
	  --casei;
	  continue;
	}

      s_entry_bb = inner->entry;
      s_exit_bb = inner->exit;

      t = gimple_block_label (s_entry_bb);
      u = build_int_cst (unsigned_type_node, casei);
      u = build_case_label (u, NULL, t);
      label_vec.quick_push (u);

      si = gsi_last_bb (s_entry_bb);
      gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_SECTION);
      gcc_assert (i < len || gimple_omp_section_last_p (gsi_stmt (si)));
      gsi_remove (&si, true);
      single_succ_edge (s_entry_bb)->flags = EDGE_FALLTHRU;

      if (s_exit_bb == NULL)
	continue;

      si = gsi_last_bb (s_exit_bb);
      gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_RETURN);
      gsi_remove (&si, true);

      single_succ_edge (s_exit_bb)->flags = EDGE_FALLTHRU;
    }

  /* Error handling code goes in DEFAULT_BB.  */
  t = gimple_block_label (default_bb);
  u = build_case_label (NULL, NULL, t);
  make_edge (l0_bb, default_bb, 0);
  add_bb_to_loop (default_bb, current_loops->tree_root);

  stmt = gimple_build_switch (vmain, u, label_vec);
  gsi_insert_after (&switch_si, stmt, GSI_SAME_STMT);
  gsi_remove (&switch_si, true);

  si = gsi_start_bb (default_bb);
  stmt = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  gsi_insert_after (&si, stmt, GSI_CONTINUE_LINKING);

  if (exit_reachable)
    {
      tree bfn_decl;

      /* Code to get the next section goes in L1_BB.  */
      si = gsi_last_bb (l1_bb);
      gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_CONTINUE);

      bfn_decl = builtin_decl_explicit (BUILT_IN_GOMP_SECTIONS_NEXT);
      stmt = gimple_build_call (bfn_decl, 0);
      gimple_call_set_lhs (stmt, vnext);
      gsi_insert_after (&si, stmt, GSI_SAME_STMT);
      gsi_remove (&si, true);

      single_succ_edge (l1_bb)->flags = EDGE_FALLTHRU;
    }

  /* Cleanup function replaces GIMPLE_OMP_RETURN in EXIT_BB.  */
  si = gsi_last_bb (l2_bb);
  if (gimple_omp_return_nowait_p (gsi_stmt (si)))
    t = builtin_decl_explicit (BUILT_IN_GOMP_SECTIONS_END_NOWAIT);
  else if (gimple_omp_return_lhs (gsi_stmt (si)))
    t = builtin_decl_explicit (BUILT_IN_GOMP_SECTIONS_END_CANCEL);
  else
    t = builtin_decl_explicit (BUILT_IN_GOMP_SECTIONS_END);
  stmt = gimple_build_call (t, 0);
  if (gimple_omp_return_lhs (gsi_stmt (si)))
    gimple_call_set_lhs (stmt, gimple_omp_return_lhs (gsi_stmt (si)));
  gsi_insert_after (&si, stmt, GSI_SAME_STMT);
  gsi_remove (&si, true);

  set_immediate_dominator (CDI_DOMINATORS, default_bb, l0_bb);
}


/* Expand code for an OpenMP single directive.  We've already expanded
   much of the code, here we simply place the GOMP_barrier call.  */

static void
expand_omp_single (struct omp_region *region)
{
  basic_block entry_bb, exit_bb;
  gimple_stmt_iterator si;

  entry_bb = region->entry;
  exit_bb = region->exit;

  si = gsi_last_bb (entry_bb);
  gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_SINGLE);
  gsi_remove (&si, true);
  single_succ_edge (entry_bb)->flags = EDGE_FALLTHRU;

  si = gsi_last_bb (exit_bb);
  if (!gimple_omp_return_nowait_p (gsi_stmt (si)))
    {
      tree t = gimple_omp_return_lhs (gsi_stmt (si));
      gsi_insert_after (&si, build_omp_barrier (t), GSI_SAME_STMT);
    }
  gsi_remove (&si, true);
  single_succ_edge (exit_bb)->flags = EDGE_FALLTHRU;
}


/* Generic expansion for OpenMP synchronization directives: master,
   ordered and critical.  All we need to do here is remove the entry
   and exit markers for REGION.  */

static void
expand_omp_synch (struct omp_region *region)
{
  basic_block entry_bb, exit_bb;
  gimple_stmt_iterator si;

  entry_bb = region->entry;
  exit_bb = region->exit;

  si = gsi_last_bb (entry_bb);
  gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_SINGLE
	      || gimple_code (gsi_stmt (si)) == GIMPLE_OMP_MASTER
	      || gimple_code (gsi_stmt (si)) == GIMPLE_OMP_TASKGROUP
	      || gimple_code (gsi_stmt (si)) == GIMPLE_OMP_ORDERED
	      || gimple_code (gsi_stmt (si)) == GIMPLE_OMP_CRITICAL
	      || gimple_code (gsi_stmt (si)) == GIMPLE_OMP_TEAMS);
  gsi_remove (&si, true);
  single_succ_edge (entry_bb)->flags = EDGE_FALLTHRU;

  if (exit_bb)
    {
      si = gsi_last_bb (exit_bb);
      gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_RETURN);
      gsi_remove (&si, true);
      single_succ_edge (exit_bb)->flags = EDGE_FALLTHRU;
    }
}

/* A subroutine of expand_omp_atomic.  Attempt to implement the atomic
   operation as a normal volatile load.  */

static bool
expand_omp_atomic_load (basic_block load_bb, tree addr,
			tree loaded_val, int index)
{
  enum built_in_function tmpbase;
  gimple_stmt_iterator gsi;
  basic_block store_bb;
  location_t loc;
  gimple *stmt;
  tree decl, call, type, itype;

  gsi = gsi_last_bb (load_bb);
  stmt = gsi_stmt (gsi);
  gcc_assert (gimple_code (stmt) == GIMPLE_OMP_ATOMIC_LOAD);
  loc = gimple_location (stmt);

  /* ??? If the target does not implement atomic_load_optab[mode], and mode
     is smaller than word size, then expand_atomic_load assumes that the load
     is atomic.  We could avoid the builtin entirely in this case.  */

  tmpbase = (enum built_in_function) (BUILT_IN_ATOMIC_LOAD_N + index + 1);
  decl = builtin_decl_explicit (tmpbase);
  if (decl == NULL_TREE)
    return false;

  type = TREE_TYPE (loaded_val);
  itype = TREE_TYPE (TREE_TYPE (decl));

  call = build_call_expr_loc (loc, decl, 2, addr,
			      build_int_cst (NULL,
					     gimple_omp_atomic_seq_cst_p (stmt)
					     ? MEMMODEL_SEQ_CST
					     : MEMMODEL_RELAXED));
  if (!useless_type_conversion_p (type, itype))
    call = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, call);
  call = build2_loc (loc, MODIFY_EXPR, void_type_node, loaded_val, call);

  force_gimple_operand_gsi (&gsi, call, true, NULL_TREE, true, GSI_SAME_STMT);
  gsi_remove (&gsi, true);

  store_bb = single_succ (load_bb);
  gsi = gsi_last_bb (store_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_ATOMIC_STORE);
  gsi_remove (&gsi, true);

  if (gimple_in_ssa_p (cfun))
    update_ssa (TODO_update_ssa_no_phi);

  return true;
}

/* A subroutine of expand_omp_atomic.  Attempt to implement the atomic
   operation as a normal volatile store.  */

static bool
expand_omp_atomic_store (basic_block load_bb, tree addr,
			 tree loaded_val, tree stored_val, int index)
{
  enum built_in_function tmpbase;
  gimple_stmt_iterator gsi;
  basic_block store_bb = single_succ (load_bb);
  location_t loc;
  gimple *stmt;
  tree decl, call, type, itype;
  machine_mode imode;
  bool exchange;

  gsi = gsi_last_bb (load_bb);
  stmt = gsi_stmt (gsi);
  gcc_assert (gimple_code (stmt) == GIMPLE_OMP_ATOMIC_LOAD);

  /* If the load value is needed, then this isn't a store but an exchange.  */
  exchange = gimple_omp_atomic_need_value_p (stmt);

  gsi = gsi_last_bb (store_bb);
  stmt = gsi_stmt (gsi);
  gcc_assert (gimple_code (stmt) == GIMPLE_OMP_ATOMIC_STORE);
  loc = gimple_location (stmt);

  /* ??? If the target does not implement atomic_store_optab[mode], and mode
     is smaller than word size, then expand_atomic_store assumes that the store
     is atomic.  We could avoid the builtin entirely in this case.  */

  tmpbase = (exchange ? BUILT_IN_ATOMIC_EXCHANGE_N : BUILT_IN_ATOMIC_STORE_N);
  tmpbase = (enum built_in_function) ((int) tmpbase + index + 1);
  decl = builtin_decl_explicit (tmpbase);
  if (decl == NULL_TREE)
    return false;

  type = TREE_TYPE (stored_val);

  /* Dig out the type of the function's second argument.  */
  itype = TREE_TYPE (decl);
  itype = TYPE_ARG_TYPES (itype);
  itype = TREE_CHAIN (itype);
  itype = TREE_VALUE (itype);
  imode = TYPE_MODE (itype);

  if (exchange && !can_atomic_exchange_p (imode, true))
    return false;

  if (!useless_type_conversion_p (itype, type))
    stored_val = fold_build1_loc (loc, VIEW_CONVERT_EXPR, itype, stored_val);
  call = build_call_expr_loc (loc, decl, 3, addr, stored_val,
			      build_int_cst (NULL,
					     gimple_omp_atomic_seq_cst_p (stmt)
					     ? MEMMODEL_SEQ_CST
					     : MEMMODEL_RELAXED));
  if (exchange)
    {
      if (!useless_type_conversion_p (type, itype))
	call = build1_loc (loc, VIEW_CONVERT_EXPR, type, call);
      call = build2_loc (loc, MODIFY_EXPR, void_type_node, loaded_val, call);
    }

  force_gimple_operand_gsi (&gsi, call, true, NULL_TREE, true, GSI_SAME_STMT);
  gsi_remove (&gsi, true);

  /* Remove the GIMPLE_OMP_ATOMIC_LOAD that we verified above.  */
  gsi = gsi_last_bb (load_bb);
  gsi_remove (&gsi, true);

  if (gimple_in_ssa_p (cfun))
    update_ssa (TODO_update_ssa_no_phi);

  return true;
}

/* A subroutine of expand_omp_atomic.  Attempt to implement the atomic
   operation as a __atomic_fetch_op builtin.  INDEX is log2 of the
   size of the data type, and thus usable to find the index of the builtin
   decl.  Returns false if the expression is not of the proper form.  */

static bool
expand_omp_atomic_fetch_op (basic_block load_bb,
			    tree addr, tree loaded_val,
			    tree stored_val, int index)
{
  enum built_in_function oldbase, newbase, tmpbase;
  tree decl, itype, call;
  tree lhs, rhs;
  basic_block store_bb = single_succ (load_bb);
  gimple_stmt_iterator gsi;
  gimple *stmt;
  location_t loc;
  enum tree_code code;
  bool need_old, need_new;
  machine_mode imode;
  bool seq_cst;

  /* We expect to find the following sequences:

   load_bb:
       GIMPLE_OMP_ATOMIC_LOAD (tmp, mem)

   store_bb:
       val = tmp OP something; (or: something OP tmp)
       GIMPLE_OMP_STORE (val)

  ???FIXME: Allow a more flexible sequence.
  Perhaps use data flow to pick the statements.

  */

  gsi = gsi_after_labels (store_bb);
  stmt = gsi_stmt (gsi);
  loc = gimple_location (stmt);
  if (!is_gimple_assign (stmt))
    return false;
  gsi_next (&gsi);
  if (gimple_code (gsi_stmt (gsi)) != GIMPLE_OMP_ATOMIC_STORE)
    return false;
  need_new = gimple_omp_atomic_need_value_p (gsi_stmt (gsi));
  need_old = gimple_omp_atomic_need_value_p (last_stmt (load_bb));
  seq_cst = gimple_omp_atomic_seq_cst_p (last_stmt (load_bb));
  gcc_checking_assert (!need_old || !need_new);

  if (!operand_equal_p (gimple_assign_lhs (stmt), stored_val, 0))
    return false;

  /* Check for one of the supported fetch-op operations.  */
  code = gimple_assign_rhs_code (stmt);
  switch (code)
    {
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      oldbase = BUILT_IN_ATOMIC_FETCH_ADD_N;
      newbase = BUILT_IN_ATOMIC_ADD_FETCH_N;
      break;
    case MINUS_EXPR:
      oldbase = BUILT_IN_ATOMIC_FETCH_SUB_N;
      newbase = BUILT_IN_ATOMIC_SUB_FETCH_N;
      break;
    case BIT_AND_EXPR:
      oldbase = BUILT_IN_ATOMIC_FETCH_AND_N;
      newbase = BUILT_IN_ATOMIC_AND_FETCH_N;
      break;
    case BIT_IOR_EXPR:
      oldbase = BUILT_IN_ATOMIC_FETCH_OR_N;
      newbase = BUILT_IN_ATOMIC_OR_FETCH_N;
      break;
    case BIT_XOR_EXPR:
      oldbase = BUILT_IN_ATOMIC_FETCH_XOR_N;
      newbase = BUILT_IN_ATOMIC_XOR_FETCH_N;
      break;
    default:
      return false;
    }

  /* Make sure the expression is of the proper form.  */
  if (operand_equal_p (gimple_assign_rhs1 (stmt), loaded_val, 0))
    rhs = gimple_assign_rhs2 (stmt);
  else if (commutative_tree_code (gimple_assign_rhs_code (stmt))
	   && operand_equal_p (gimple_assign_rhs2 (stmt), loaded_val, 0))
    rhs = gimple_assign_rhs1 (stmt);
  else
    return false;

  tmpbase = ((enum built_in_function)
	     ((need_new ? newbase : oldbase) + index + 1));
  decl = builtin_decl_explicit (tmpbase);
  if (decl == NULL_TREE)
    return false;
  itype = TREE_TYPE (TREE_TYPE (decl));
  imode = TYPE_MODE (itype);

  /* We could test all of the various optabs involved, but the fact of the
     matter is that (with the exception of i486 vs i586 and xadd) all targets
     that support any atomic operaton optab also implements compare-and-swap.
     Let optabs.c take care of expanding any compare-and-swap loop.  */
  if (!can_compare_and_swap_p (imode, true))
    return false;

  gsi = gsi_last_bb (load_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_ATOMIC_LOAD);

  /* OpenMP does not imply any barrier-like semantics on its atomic ops.
     It only requires that the operation happen atomically.  Thus we can
     use the RELAXED memory model.  */
  call = build_call_expr_loc (loc, decl, 3, addr,
			      fold_convert_loc (loc, itype, rhs),
			      build_int_cst (NULL,
					     seq_cst ? MEMMODEL_SEQ_CST
						     : MEMMODEL_RELAXED));

  if (need_old || need_new)
    {
      lhs = need_old ? loaded_val : stored_val;
      call = fold_convert_loc (loc, TREE_TYPE (lhs), call);
      call = build2_loc (loc, MODIFY_EXPR, void_type_node, lhs, call);
    }
  else
    call = fold_convert_loc (loc, void_type_node, call);
  force_gimple_operand_gsi (&gsi, call, true, NULL_TREE, true, GSI_SAME_STMT);
  gsi_remove (&gsi, true);

  gsi = gsi_last_bb (store_bb);
  gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_ATOMIC_STORE);
  gsi_remove (&gsi, true);
  gsi = gsi_last_bb (store_bb);
  stmt = gsi_stmt (gsi);
  gsi_remove (&gsi, true);

  if (gimple_in_ssa_p (cfun))
    {
      release_defs (stmt);
      update_ssa (TODO_update_ssa_no_phi);
    }

  return true;
}

/* A subroutine of expand_omp_atomic.  Implement the atomic operation as:

      oldval = *addr;
      repeat:
        newval = rhs;	 // with oldval replacing *addr in rhs
	oldval = __sync_val_compare_and_swap (addr, oldval, newval);
	if (oldval != newval)
	  goto repeat;

   INDEX is log2 of the size of the data type, and thus usable to find the
   index of the builtin decl.  */

static bool
expand_omp_atomic_pipeline (basic_block load_bb, basic_block store_bb,
			    tree addr, tree loaded_val, tree stored_val,
			    int index)
{
  tree loadedi, storedi, initial, new_storedi, old_vali;
  tree type, itype, cmpxchg, iaddr;
  gimple_stmt_iterator si;
  basic_block loop_header = single_succ (load_bb);
  gimple *phi, *stmt;
  edge e;
  enum built_in_function fncode;

  /* ??? We need a non-pointer interface to __atomic_compare_exchange in
     order to use the RELAXED memory model effectively.  */
  fncode = (enum built_in_function)((int)BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_N
				    + index + 1);
  cmpxchg = builtin_decl_explicit (fncode);
  if (cmpxchg == NULL_TREE)
    return false;
  type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (addr)));
  itype = TREE_TYPE (TREE_TYPE (cmpxchg));

  if (!can_compare_and_swap_p (TYPE_MODE (itype), true))
    return false;

  /* Load the initial value, replacing the GIMPLE_OMP_ATOMIC_LOAD.  */
  si = gsi_last_bb (load_bb);
  gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_ATOMIC_LOAD);

  /* For floating-point values, we'll need to view-convert them to integers
     so that we can perform the atomic compare and swap.  Simplify the
     following code by always setting up the "i"ntegral variables.  */
  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    {
      tree iaddr_val;

      iaddr = create_tmp_reg (build_pointer_type_for_mode (itype, ptr_mode,
							   true));
      iaddr_val
	= force_gimple_operand_gsi (&si,
				    fold_convert (TREE_TYPE (iaddr), addr),
				    false, NULL_TREE, true, GSI_SAME_STMT);
      stmt = gimple_build_assign (iaddr, iaddr_val);
      gsi_insert_before (&si, stmt, GSI_SAME_STMT);
      loadedi = create_tmp_var (itype);
      if (gimple_in_ssa_p (cfun))
	loadedi = make_ssa_name (loadedi);
    }
  else
    {
      iaddr = addr;
      loadedi = loaded_val;
    }

  fncode = (enum built_in_function) (BUILT_IN_ATOMIC_LOAD_N + index + 1);
  tree loaddecl = builtin_decl_explicit (fncode);
  if (loaddecl)
    initial
      = fold_convert (TREE_TYPE (TREE_TYPE (iaddr)),
		      build_call_expr (loaddecl, 2, iaddr,
				       build_int_cst (NULL_TREE,
						      MEMMODEL_RELAXED)));
  else
    initial = build2 (MEM_REF, TREE_TYPE (TREE_TYPE (iaddr)), iaddr,
		      build_int_cst (TREE_TYPE (iaddr), 0));

  initial
    = force_gimple_operand_gsi (&si, initial, true, NULL_TREE, true,
				GSI_SAME_STMT);

  /* Move the value to the LOADEDI temporary.  */
  if (gimple_in_ssa_p (cfun))
    {
      gcc_assert (gimple_seq_empty_p (phi_nodes (loop_header)));
      phi = create_phi_node (loadedi, loop_header);
      SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, single_succ_edge (load_bb)),
	       initial);
    }
  else
    gsi_insert_before (&si,
		       gimple_build_assign (loadedi, initial),
		       GSI_SAME_STMT);
  if (loadedi != loaded_val)
    {
      gimple_stmt_iterator gsi2;
      tree x;

      x = build1 (VIEW_CONVERT_EXPR, type, loadedi);
      gsi2 = gsi_start_bb (loop_header);
      if (gimple_in_ssa_p (cfun))
	{
	  gassign *stmt;
	  x = force_gimple_operand_gsi (&gsi2, x, true, NULL_TREE,
					true, GSI_SAME_STMT);
	  stmt = gimple_build_assign (loaded_val, x);
	  gsi_insert_before (&gsi2, stmt, GSI_SAME_STMT);
	}
      else
	{
	  x = build2 (MODIFY_EXPR, TREE_TYPE (loaded_val), loaded_val, x);
	  force_gimple_operand_gsi (&gsi2, x, true, NULL_TREE,
				    true, GSI_SAME_STMT);
	}
    }
  gsi_remove (&si, true);

  si = gsi_last_bb (store_bb);
  gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_ATOMIC_STORE);

  if (iaddr == addr)
    storedi = stored_val;
  else
    storedi =
      force_gimple_operand_gsi (&si,
				build1 (VIEW_CONVERT_EXPR, itype,
					stored_val), true, NULL_TREE, true,
				GSI_SAME_STMT);

  /* Build the compare&swap statement.  */
  new_storedi = build_call_expr (cmpxchg, 3, iaddr, loadedi, storedi);
  new_storedi = force_gimple_operand_gsi (&si,
					  fold_convert (TREE_TYPE (loadedi),
							new_storedi),
					  true, NULL_TREE,
					  true, GSI_SAME_STMT);

  if (gimple_in_ssa_p (cfun))
    old_vali = loadedi;
  else
    {
      old_vali = create_tmp_var (TREE_TYPE (loadedi));
      stmt = gimple_build_assign (old_vali, loadedi);
      gsi_insert_before (&si, stmt, GSI_SAME_STMT);

      stmt = gimple_build_assign (loadedi, new_storedi);
      gsi_insert_before (&si, stmt, GSI_SAME_STMT);
    }

  /* Note that we always perform the comparison as an integer, even for
     floating point.  This allows the atomic operation to properly
     succeed even with NaNs and -0.0.  */
  stmt = gimple_build_cond_empty
           (build2 (NE_EXPR, boolean_type_node,
		    new_storedi, old_vali));
  gsi_insert_before (&si, stmt, GSI_SAME_STMT);

  /* Update cfg.  */
  e = single_succ_edge (store_bb);
  e->flags &= ~EDGE_FALLTHRU;
  e->flags |= EDGE_FALSE_VALUE;

  e = make_edge (store_bb, loop_header, EDGE_TRUE_VALUE);

  /* Copy the new value to loadedi (we already did that before the condition
     if we are not in SSA).  */
  if (gimple_in_ssa_p (cfun))
    {
      phi = gimple_seq_first_stmt (phi_nodes (loop_header));
      SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e), new_storedi);
    }

  /* Remove GIMPLE_OMP_ATOMIC_STORE.  */
  gsi_remove (&si, true);

  struct loop *loop = alloc_loop ();
  loop->header = loop_header;
  loop->latch = store_bb;
  add_loop (loop, loop_header->loop_father);

  if (gimple_in_ssa_p (cfun))
    update_ssa (TODO_update_ssa_no_phi);

  return true;
}

/* A subroutine of expand_omp_atomic.  Implement the atomic operation as:

		 		  GOMP_atomic_start ();
		 		  *addr = rhs;
		 		  GOMP_atomic_end ();

   The result is not globally atomic, but works so long as all parallel
   references are within #pragma omp atomic directives.  According to
   responses received from omp@openmp.org, appears to be within spec.
   Which makes sense, since that's how several other compilers handle
   this situation as well.
   LOADED_VAL and ADDR are the operands of GIMPLE_OMP_ATOMIC_LOAD we're
   expanding.  STORED_VAL is the operand of the matching
   GIMPLE_OMP_ATOMIC_STORE.

   We replace
   GIMPLE_OMP_ATOMIC_LOAD (loaded_val, addr) with
   loaded_val = *addr;

   and replace
   GIMPLE_OMP_ATOMIC_STORE (stored_val)  with
   *addr = stored_val;
*/

static bool
expand_omp_atomic_mutex (basic_block load_bb, basic_block store_bb,
			 tree addr, tree loaded_val, tree stored_val)
{
  gimple_stmt_iterator si;
  gassign *stmt;
  tree t;

  si = gsi_last_bb (load_bb);
  gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_ATOMIC_LOAD);

  t = builtin_decl_explicit (BUILT_IN_GOMP_ATOMIC_START);
  t = build_call_expr (t, 0);
  force_gimple_operand_gsi (&si, t, true, NULL_TREE, true, GSI_SAME_STMT);

  stmt = gimple_build_assign (loaded_val, build_simple_mem_ref (addr));
  gsi_insert_before (&si, stmt, GSI_SAME_STMT);
  gsi_remove (&si, true);

  si = gsi_last_bb (store_bb);
  gcc_assert (gimple_code (gsi_stmt (si)) == GIMPLE_OMP_ATOMIC_STORE);

  stmt = gimple_build_assign (build_simple_mem_ref (unshare_expr (addr)),
			      stored_val);
  gsi_insert_before (&si, stmt, GSI_SAME_STMT);

  t = builtin_decl_explicit (BUILT_IN_GOMP_ATOMIC_END);
  t = build_call_expr (t, 0);
  force_gimple_operand_gsi (&si, t, true, NULL_TREE, true, GSI_SAME_STMT);
  gsi_remove (&si, true);

  if (gimple_in_ssa_p (cfun))
    update_ssa (TODO_update_ssa_no_phi);
  return true;
}

/* Expand an GIMPLE_OMP_ATOMIC statement.  We try to expand
   using expand_omp_atomic_fetch_op. If it failed, we try to
   call expand_omp_atomic_pipeline, and if it fails too, the
   ultimate fallback is wrapping the operation in a mutex
   (expand_omp_atomic_mutex).  REGION is the atomic region built
   by build_omp_regions_1().  */

static void
expand_omp_atomic (struct omp_region *region)
{
  basic_block load_bb = region->entry, store_bb = region->exit;
  gomp_atomic_load *load = as_a <gomp_atomic_load *> (last_stmt (load_bb));
  gomp_atomic_store *store = as_a <gomp_atomic_store *> (last_stmt (store_bb));
  tree loaded_val = gimple_omp_atomic_load_lhs (load);
  tree addr = gimple_omp_atomic_load_rhs (load);
  tree stored_val = gimple_omp_atomic_store_val (store);
  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (addr)));
  HOST_WIDE_INT index;

  /* Make sure the type is one of the supported sizes.  */
  index = tree_to_uhwi (TYPE_SIZE_UNIT (type));
  index = exact_log2 (index);
  if (index >= 0 && index <= 4)
    {
      unsigned int align = TYPE_ALIGN_UNIT (type);

      /* __sync builtins require strict data alignment.  */
      if (exact_log2 (align) >= index)
	{
	  /* Atomic load.  */
	  if (loaded_val == stored_val
	      && (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_INT
		  || GET_MODE_CLASS (TYPE_MODE (type)) == MODE_FLOAT)
	      && GET_MODE_BITSIZE (TYPE_MODE (type)) <= BITS_PER_WORD
	      && expand_omp_atomic_load (load_bb, addr, loaded_val, index))
	    return;

	  /* Atomic store.  */
	  if ((GET_MODE_CLASS (TYPE_MODE (type)) == MODE_INT
	       || GET_MODE_CLASS (TYPE_MODE (type)) == MODE_FLOAT)
	      && GET_MODE_BITSIZE (TYPE_MODE (type)) <= BITS_PER_WORD
	      && store_bb == single_succ (load_bb)
	      && first_stmt (store_bb) == store
	      && expand_omp_atomic_store (load_bb, addr, loaded_val,
					  stored_val, index))
	    return;

	  /* When possible, use specialized atomic update functions.  */
	  if ((INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type))
	      && store_bb == single_succ (load_bb)
	      && expand_omp_atomic_fetch_op (load_bb, addr,
					     loaded_val, stored_val, index))
	    return;

	  /* If we don't have specialized __sync builtins, try and implement
	     as a compare and swap loop.  */
	  if (expand_omp_atomic_pipeline (load_bb, store_bb, addr,
					  loaded_val, stored_val, index))
	    return;
	}
    }

  /* The ultimate fallback is wrapping the operation in a mutex.  */
  expand_omp_atomic_mutex (load_bb, store_bb, addr, loaded_val, stored_val);
}


/* Encode an oacc launch argument.  This matches the GOMP_LAUNCH_PACK
   macro on gomp-constants.h.  We do not check for overflow.  */

static tree
oacc_launch_pack (unsigned code, tree device, unsigned op)
{
  tree res;
  
  res = build_int_cst (unsigned_type_node, GOMP_LAUNCH_PACK (code, 0, op));
  if (device)
    {
      device = fold_build2 (LSHIFT_EXPR, unsigned_type_node,
			    device, build_int_cst (unsigned_type_node,
						   GOMP_LAUNCH_DEVICE_SHIFT));
      res = fold_build2 (BIT_IOR_EXPR, unsigned_type_node, res, device);
    }
  return res;
}

/* Look for compute grid dimension clauses and convert to an attribute
   attached to FN.  This permits the target-side code to (a) massage
   the dimensions, (b) emit that data and (c) optimize.  Non-constant
   dimensions are pushed onto ARGS.

   The attribute value is a TREE_LIST.  A set of dimensions is
   represented as a list of INTEGER_CST.  Those that are runtime
   exprs are represented as an INTEGER_CST of zero.

   TOOO. Normally the attribute will just contain a single such list.  If
   however it contains a list of lists, this will represent the use of
   device_type.  Each member of the outer list is an assoc list of
   dimensions, keyed by the device type.  The first entry will be the
   default.  Well, that's the plan.  */

#define OACC_FN_ATTRIB "oacc function"

/* Replace any existing oacc fn attribute with updated dimensions.  */

void
replace_oacc_fn_attrib (tree fn, tree dims)
{
  tree ident = get_identifier (OACC_FN_ATTRIB);
  tree attribs = DECL_ATTRIBUTES (fn);

  /* If we happen to be present as the first attrib, drop it.  */
  if (attribs && TREE_PURPOSE (attribs) == ident)
    attribs = TREE_CHAIN (attribs);
  DECL_ATTRIBUTES (fn) = tree_cons (ident, dims, attribs);
}

/* Scan CLAUSES for launch dimensions and attach them to the oacc
   function attribute.  Push any that are non-constant onto the ARGS
   list, along with an appropriate GOMP_LAUNCH_DIM tag.  IS_KERNEL is
   true, if these are for a kernels region offload function.  */

void
set_oacc_fn_attrib (tree fn, tree clauses, bool is_kernel, vec<tree> *args)
{
  /* Must match GOMP_DIM ordering.  */
  static const omp_clause_code ids[]
    = { OMP_CLAUSE_NUM_GANGS, OMP_CLAUSE_NUM_WORKERS,
	OMP_CLAUSE_VECTOR_LENGTH };
  unsigned ix;
  tree dims[GOMP_DIM_MAX];
  tree attr = NULL_TREE;
  unsigned non_const = 0;

  for (ix = GOMP_DIM_MAX; ix--;)
    {
      tree clause = find_omp_clause (clauses, ids[ix]);
      tree dim = NULL_TREE;

      if (clause)
	dim = OMP_CLAUSE_EXPR (clause, ids[ix]);
      dims[ix] = dim;
      if (dim && TREE_CODE (dim) != INTEGER_CST)
	{
	  dim = integer_zero_node;
	  non_const |= GOMP_DIM_MASK (ix);
	}
      attr = tree_cons (NULL_TREE, dim, attr);
      /* Note kernelness with TREE_PUBLIC.  */
      if (is_kernel)
	TREE_PUBLIC (attr) = 1;
    }

  replace_oacc_fn_attrib (fn, attr);

  if (non_const)
    {
      /* Push a dynamic argument set.  */
      args->safe_push (oacc_launch_pack (GOMP_LAUNCH_DIM,
					 NULL_TREE, non_const));
      for (unsigned ix = 0; ix != GOMP_DIM_MAX; ix++)
	if (non_const & GOMP_DIM_MASK (ix))
	  args->safe_push (dims[ix]);
    }
}

/*  Process the routine's dimension clauess to generate an attribute
    value.  Issue diagnostics as appropriate.  We default to SEQ
    (OpenACC 2.5 clarifies this). All dimensions have a size of zero
    (dynamic).  TREE_PURPOSE is set to indicate whether that dimension
    can have a loop partitioned on it.  non-zero indicates
    yes, zero indicates no.  By construction once a non-zero has been
    reached, further inner dimensions must also be non-zero.  We set
    TREE_VALUE to zero for the dimensions that may be partitioned and
    1 for the other ones -- if a loop is (erroneously) spawned at
    an outer level, we don't want to try and partition it.  */

tree
build_oacc_routine_dims (tree clauses)
{
  /* Must match GOMP_DIM ordering.  */
  static const omp_clause_code ids[] = 
    {OMP_CLAUSE_GANG, OMP_CLAUSE_WORKER, OMP_CLAUSE_VECTOR, OMP_CLAUSE_SEQ};
  int ix;
  int level = -1;

  for (; clauses; clauses = OMP_CLAUSE_CHAIN (clauses))
    for (ix = GOMP_DIM_MAX + 1; ix--;)
      if (OMP_CLAUSE_CODE (clauses) == ids[ix])
	{
	  if (level >= 0)
	    error_at (OMP_CLAUSE_LOCATION (clauses),
		      "multiple loop axes specified for routine");
	  level = ix;
	  break;
	}

  /* Default to SEQ.  */
  if (level < 0)
    level = GOMP_DIM_MAX;
  
  tree dims = NULL_TREE;

  for (ix = GOMP_DIM_MAX; ix--;)
    dims = tree_cons (build_int_cst (boolean_type_node, ix >= level),
		      build_int_cst (integer_type_node, ix < level), dims);

  return dims;
}

/* Retrieve the oacc function attrib and return it.  Non-oacc
   functions will return NULL.  */

tree
get_oacc_fn_attrib (tree fn)
{
  return lookup_attribute (OACC_FN_ATTRIB, DECL_ATTRIBUTES (fn));
}

/* Return true if this oacc fn attrib is for a kernels offload
   region.  We use the TREE_PUBLIC flag of each dimension -- only
   need to check the first one.  */

bool
oacc_fn_attrib_kernels_p (tree attr)
{
  return TREE_PUBLIC (TREE_VALUE (attr));
}

/* Return level at which oacc routine may spawn a partitioned loop, or
   -1 if it is not a routine (i.e. is an offload fn).  */

static int
oacc_fn_attrib_level (tree attr)
{
  tree pos = TREE_VALUE (attr);

  if (!TREE_PURPOSE (pos))
    return -1;
  
  int ix = 0;
  for (ix = 0; ix != GOMP_DIM_MAX;
       ix++, pos = TREE_CHAIN (pos))
    if (!integer_zerop (TREE_PURPOSE (pos)))
      break;

  return ix;
}

/* Extract an oacc execution dimension from FN.  FN must be an
   offloaded function or routine that has already had its execution
   dimensions lowered to the target-specific values.  */

int
get_oacc_fn_dim_size (tree fn, int axis)
{
  tree attrs = get_oacc_fn_attrib (fn);
  
  gcc_assert (axis < GOMP_DIM_MAX);

  tree dims = TREE_VALUE (attrs);
  while (axis--)
    dims = TREE_CHAIN (dims);

  int size = TREE_INT_CST_LOW (TREE_VALUE (dims));

  return size;
}

/* Extract the dimension axis from an IFN_GOACC_DIM_POS or
   IFN_GOACC_DIM_SIZE call.  */

int
get_oacc_ifn_dim_arg (const gimple *stmt)
{
  gcc_checking_assert (gimple_call_internal_fn (stmt) == IFN_GOACC_DIM_SIZE
		       || gimple_call_internal_fn (stmt) == IFN_GOACC_DIM_POS);
  tree arg = gimple_call_arg (stmt, 0);
  HOST_WIDE_INT axis = TREE_INT_CST_LOW (arg);

  gcc_checking_assert (axis >= 0 && axis < GOMP_DIM_MAX);
  return (int) axis;
}

/* Mark the loops inside the kernels region starting at REGION_ENTRY and ending
   at REGION_EXIT.  */

static void
mark_loops_in_oacc_kernels_region (basic_block region_entry,
				   basic_block region_exit)
{
  struct loop *outer = region_entry->loop_father;
  gcc_assert (region_exit == NULL || outer == region_exit->loop_father);

  /* Don't parallelize the kernels region if it contains more than one outer
     loop.  */
  unsigned int nr_outer_loops = 0;
  struct loop *single_outer = NULL;
  for (struct loop *loop = outer->inner; loop != NULL; loop = loop->next)
    {
      gcc_assert (loop_outer (loop) == outer);

      if (!dominated_by_p (CDI_DOMINATORS, loop->header, region_entry))
	continue;

      if (region_exit != NULL
	  && dominated_by_p (CDI_DOMINATORS, loop->header, region_exit))
	continue;

      nr_outer_loops++;
      single_outer = loop;
    }
  if (nr_outer_loops != 1)
    return;

  for (struct loop *loop = single_outer->inner; loop != NULL; loop = loop->inner)
    if (loop->next)
      return;

  /* Mark the loops in the region.  */
  for (struct loop *loop = single_outer; loop != NULL; loop = loop->inner)
    loop->in_oacc_kernels_region = true;
}

/* Types used to pass grid and wortkgroup sizes to kernel invocation.  */

struct GTY(()) grid_launch_attributes_trees
{
  tree kernel_dim_array_type;
  tree kernel_lattrs_dimnum_decl;
  tree kernel_lattrs_grid_decl;
  tree kernel_lattrs_group_decl;
  tree kernel_launch_attributes_type;
};

static GTY(()) struct grid_launch_attributes_trees *grid_attr_trees;

/* Create types used to pass kernel launch attributes to target.  */

static void
grid_create_kernel_launch_attr_types (void)
{
  if (grid_attr_trees)
    return;
  grid_attr_trees = ggc_alloc <grid_launch_attributes_trees> ();

  tree dim_arr_index_type
    = build_index_type (build_int_cst (integer_type_node, 2));
  grid_attr_trees->kernel_dim_array_type
    = build_array_type (uint32_type_node, dim_arr_index_type);

  grid_attr_trees->kernel_launch_attributes_type = make_node (RECORD_TYPE);
  grid_attr_trees->kernel_lattrs_dimnum_decl
    = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("ndim"),
		  uint32_type_node);
  DECL_CHAIN (grid_attr_trees->kernel_lattrs_dimnum_decl) = NULL_TREE;

  grid_attr_trees->kernel_lattrs_grid_decl
    = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("grid_size"),
		  grid_attr_trees->kernel_dim_array_type);
  DECL_CHAIN (grid_attr_trees->kernel_lattrs_grid_decl)
    = grid_attr_trees->kernel_lattrs_dimnum_decl;
  grid_attr_trees->kernel_lattrs_group_decl
    = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("group_size"),
		  grid_attr_trees->kernel_dim_array_type);
  DECL_CHAIN (grid_attr_trees->kernel_lattrs_group_decl)
    = grid_attr_trees->kernel_lattrs_grid_decl;
  finish_builtin_struct (grid_attr_trees->kernel_launch_attributes_type,
			 "__gomp_kernel_launch_attributes",
			 grid_attr_trees->kernel_lattrs_group_decl, NULL_TREE);
}

/* Insert before the current statement in GSI a store of VALUE to INDEX of
   array (of type kernel_dim_array_type) FLD_DECL of RANGE_VAR.  VALUE must be
   of type uint32_type_node.  */

static void
grid_insert_store_range_dim (gimple_stmt_iterator *gsi, tree range_var,
			     tree fld_decl, int index, tree value)
{
  tree ref = build4 (ARRAY_REF, uint32_type_node,
		     build3 (COMPONENT_REF,
			     grid_attr_trees->kernel_dim_array_type,
			     range_var, fld_decl, NULL_TREE),
		     build_int_cst (integer_type_node, index),
		     NULL_TREE, NULL_TREE);
  gsi_insert_before (gsi, gimple_build_assign (ref, value), GSI_SAME_STMT);
}

/* Return a tree representation of a pointer to a structure with grid and
   work-group size information.  Statements filling that information will be
   inserted before GSI, TGT_STMT is the target statement which has the
   necessary information in it.  */

static tree
grid_get_kernel_launch_attributes (gimple_stmt_iterator *gsi,
				   gomp_target *tgt_stmt)
{
  grid_create_kernel_launch_attr_types ();
  tree u32_one = build_one_cst (uint32_type_node);
  tree lattrs = create_tmp_var (grid_attr_trees->kernel_launch_attributes_type,
				"__kernel_launch_attrs");

  unsigned max_dim = 0;
  for (tree clause = gimple_omp_target_clauses (tgt_stmt);
       clause;
       clause = OMP_CLAUSE_CHAIN (clause))
    {
      if (OMP_CLAUSE_CODE (clause) != OMP_CLAUSE__GRIDDIM_)
	continue;

      unsigned dim = OMP_CLAUSE__GRIDDIM__DIMENSION (clause);
      max_dim = MAX (dim, max_dim);

      grid_insert_store_range_dim (gsi, lattrs,
				   grid_attr_trees->kernel_lattrs_grid_decl,
				   dim, OMP_CLAUSE__GRIDDIM__SIZE (clause));
      grid_insert_store_range_dim (gsi, lattrs,
				   grid_attr_trees->kernel_lattrs_group_decl,
				   dim, OMP_CLAUSE__GRIDDIM__GROUP (clause));
    }

  tree dimref = build3 (COMPONENT_REF, uint32_type_node, lattrs,
			grid_attr_trees->kernel_lattrs_dimnum_decl, NULL_TREE);
  /* At this moment we cannot gridify a loop with a collapse clause.  */
  /* TODO: Adjust when we support bigger collapse.  */
  gcc_assert (max_dim == 0);
  gsi_insert_before (gsi, gimple_build_assign (dimref, u32_one), GSI_SAME_STMT);
  TREE_ADDRESSABLE (lattrs) = 1;
  return build_fold_addr_expr (lattrs);
}

/* Build target argument identifier from the DEVICE identifier, value
   identifier ID and whether the element also has a SUBSEQUENT_PARAM.  */

static tree
get_target_argument_identifier_1 (int device, bool subseqent_param, int id)
{
  tree t = build_int_cst (integer_type_node, device);
  if (subseqent_param)
    t = fold_build2 (BIT_IOR_EXPR, integer_type_node, t,
		     build_int_cst (integer_type_node,
				    GOMP_TARGET_ARG_SUBSEQUENT_PARAM));
  t = fold_build2 (BIT_IOR_EXPR, integer_type_node, t,
		   build_int_cst (integer_type_node, id));
  return t;
}

/* Like above but return it in type that can be directly stored as an element
   of the argument array.  */

static tree
get_target_argument_identifier (int device, bool subseqent_param, int id)
{
  tree t = get_target_argument_identifier_1 (device, subseqent_param, id);
  return fold_convert (ptr_type_node, t);
}

/* Return a target argument consisting of DEVICE identifier, value identifier
   ID, and the actual VALUE.  */

static tree
get_target_argument_value (gimple_stmt_iterator *gsi, int device, int id,
			   tree value)
{
  tree t = fold_build2 (LSHIFT_EXPR, integer_type_node,
			fold_convert (integer_type_node, value),
			build_int_cst (unsigned_type_node,
				       GOMP_TARGET_ARG_VALUE_SHIFT));
  t = fold_build2 (BIT_IOR_EXPR, integer_type_node, t,
		   get_target_argument_identifier_1 (device, false, id));
  t = fold_convert (ptr_type_node, t);
  return force_gimple_operand_gsi (gsi, t, true, NULL, true, GSI_SAME_STMT);
}

/* If VALUE is an integer constant greater than -2^15 and smaller than 2^15,
   push one argument to ARGS with both the DEVICE, ID and VALUE embedded in it,
   otherwise push an identifier (with DEVICE and ID) and the VALUE in two
   arguments.  */

static void
push_target_argument_according_to_value (gimple_stmt_iterator *gsi, int device,
					 int id, tree value, vec <tree> *args)
{
  if (tree_fits_shwi_p (value)
      && tree_to_shwi (value) > -(1 << 15)
      && tree_to_shwi (value) < (1 << 15))
    args->quick_push (get_target_argument_value (gsi, device, id, value));
  else
    {
      args->quick_push (get_target_argument_identifier (device, true, id));
      value = fold_convert (ptr_type_node, value);
      value = force_gimple_operand_gsi (gsi, value, true, NULL, true,
					GSI_SAME_STMT);
      args->quick_push (value);
    }
}

/* Create an array of arguments that is then passed to GOMP_target.   */

static tree
get_target_arguments (gimple_stmt_iterator *gsi, gomp_target *tgt_stmt)
{
  auto_vec <tree, 6> args;
  tree clauses = gimple_omp_target_clauses (tgt_stmt);
  tree t, c = find_omp_clause (clauses, OMP_CLAUSE_NUM_TEAMS);
  if (c)
    t = OMP_CLAUSE_NUM_TEAMS_EXPR (c);
  else
    t = integer_minus_one_node;
  push_target_argument_according_to_value (gsi, GOMP_TARGET_ARG_DEVICE_ALL,
					   GOMP_TARGET_ARG_NUM_TEAMS, t, &args);

  c = find_omp_clause (clauses, OMP_CLAUSE_THREAD_LIMIT);
  if (c)
    t = OMP_CLAUSE_THREAD_LIMIT_EXPR (c);
  else
    t = integer_minus_one_node;
  push_target_argument_according_to_value (gsi, GOMP_TARGET_ARG_DEVICE_ALL,
					   GOMP_TARGET_ARG_THREAD_LIMIT, t,
					   &args);

  /* Add HSA-specific grid sizes, if available.  */
  if (find_omp_clause (gimple_omp_target_clauses (tgt_stmt),
		       OMP_CLAUSE__GRIDDIM_))
    {
      t = get_target_argument_identifier (GOMP_DEVICE_HSA, true,
					  GOMP_TARGET_ARG_HSA_KERNEL_ATTRIBUTES);
      args.quick_push (t);
      args.quick_push (grid_get_kernel_launch_attributes (gsi, tgt_stmt));
    }

  /* Produce more, perhaps device specific, arguments here.  */

  tree argarray = create_tmp_var (build_array_type_nelts (ptr_type_node,
							  args.length () + 1),
				  ".omp_target_args");
  for (unsigned i = 0; i < args.length (); i++)
    {
      tree ref = build4 (ARRAY_REF, ptr_type_node, argarray,
			 build_int_cst (integer_type_node, i),
			 NULL_TREE, NULL_TREE);
      gsi_insert_before (gsi, gimple_build_assign (ref, args[i]),
			 GSI_SAME_STMT);
    }
  tree ref = build4 (ARRAY_REF, ptr_type_node, argarray,
		     build_int_cst (integer_type_node, args.length ()),
		     NULL_TREE, NULL_TREE);
  gsi_insert_before (gsi, gimple_build_assign (ref, null_pointer_node),
		     GSI_SAME_STMT);
  TREE_ADDRESSABLE (argarray) = 1;
  return build_fold_addr_expr (argarray);
}

/* Expand the GIMPLE_OMP_TARGET starting at REGION.  */

static void
expand_omp_target (struct omp_region *region)
{
  basic_block entry_bb, exit_bb, new_bb;
  struct function *child_cfun;
  tree child_fn, block, t;
  gimple_stmt_iterator gsi;
  gomp_target *entry_stmt;
  gimple *stmt;
  edge e;
  bool offloaded, data_region;

  entry_stmt = as_a <gomp_target *> (last_stmt (region->entry));
  new_bb = region->entry;

  offloaded = is_gimple_omp_offloaded (entry_stmt);
  switch (gimple_omp_target_kind (entry_stmt))
    {
    case GF_OMP_TARGET_KIND_REGION:
    case GF_OMP_TARGET_KIND_UPDATE:
    case GF_OMP_TARGET_KIND_ENTER_DATA:
    case GF_OMP_TARGET_KIND_EXIT_DATA:
    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
    case GF_OMP_TARGET_KIND_OACC_KERNELS:
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      data_region = false;
      break;
    case GF_OMP_TARGET_KIND_DATA:
    case GF_OMP_TARGET_KIND_OACC_DATA:
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      data_region = true;
      break;
    default:
      gcc_unreachable ();
    }

  child_fn = NULL_TREE;
  child_cfun = NULL;
  if (offloaded)
    {
      child_fn = gimple_omp_target_child_fn (entry_stmt);
      child_cfun = DECL_STRUCT_FUNCTION (child_fn);
    }

  /* Supported by expand_omp_taskreg, but not here.  */
  if (child_cfun != NULL)
    gcc_checking_assert (!child_cfun->cfg);
  gcc_checking_assert (!gimple_in_ssa_p (cfun));

  entry_bb = region->entry;
  exit_bb = region->exit;

  if (gimple_omp_target_kind (entry_stmt) == GF_OMP_TARGET_KIND_OACC_KERNELS)
    mark_loops_in_oacc_kernels_region (region->entry, region->exit);

  if (offloaded)
    {
      unsigned srcidx, dstidx, num;

      /* If the offloading region needs data sent from the parent
	 function, then the very first statement (except possible
	 tree profile counter updates) of the offloading body
	 is a copy assignment .OMP_DATA_I = &.OMP_DATA_O.  Since
	 &.OMP_DATA_O is passed as an argument to the child function,
	 we need to replace it with the argument as seen by the child
	 function.

	 In most cases, this will end up being the identity assignment
	 .OMP_DATA_I = .OMP_DATA_I.  However, if the offloading body had
	 a function call that has been inlined, the original PARM_DECL
	 .OMP_DATA_I may have been converted into a different local
	 variable.  In which case, we need to keep the assignment.  */
      tree data_arg = gimple_omp_target_data_arg (entry_stmt);
      if (data_arg)
	{
	  basic_block entry_succ_bb = single_succ (entry_bb);
	  gimple_stmt_iterator gsi;
	  tree arg;
	  gimple *tgtcopy_stmt = NULL;
	  tree sender = TREE_VEC_ELT (data_arg, 0);

	  for (gsi = gsi_start_bb (entry_succ_bb); ; gsi_next (&gsi))
	    {
	      gcc_assert (!gsi_end_p (gsi));
	      stmt = gsi_stmt (gsi);
	      if (gimple_code (stmt) != GIMPLE_ASSIGN)
		continue;

	      if (gimple_num_ops (stmt) == 2)
		{
		  tree arg = gimple_assign_rhs1 (stmt);

		  /* We're ignoring the subcode because we're
		     effectively doing a STRIP_NOPS.  */

		  if (TREE_CODE (arg) == ADDR_EXPR
		      && TREE_OPERAND (arg, 0) == sender)
		    {
		      tgtcopy_stmt = stmt;
		      break;
		    }
		}
	    }

	  gcc_assert (tgtcopy_stmt != NULL);
	  arg = DECL_ARGUMENTS (child_fn);

	  gcc_assert (gimple_assign_lhs (tgtcopy_stmt) == arg);
	  gsi_remove (&gsi, true);
	}

      /* Declare local variables needed in CHILD_CFUN.  */
      block = DECL_INITIAL (child_fn);
      BLOCK_VARS (block) = vec2chain (child_cfun->local_decls);
      /* The gimplifier could record temporaries in the offloading block
	 rather than in containing function's local_decls chain,
	 which would mean cgraph missed finalizing them.  Do it now.  */
      for (t = BLOCK_VARS (block); t; t = DECL_CHAIN (t))
	if (TREE_CODE (t) == VAR_DECL
	    && TREE_STATIC (t)
	    && !DECL_EXTERNAL (t))
	  varpool_node::finalize_decl (t);
      DECL_SAVED_TREE (child_fn) = NULL;
      /* We'll create a CFG for child_fn, so no gimple body is needed.  */
      gimple_set_body (child_fn, NULL);
      TREE_USED (block) = 1;

      /* Reset DECL_CONTEXT on function arguments.  */
      for (t = DECL_ARGUMENTS (child_fn); t; t = DECL_CHAIN (t))
	DECL_CONTEXT (t) = child_fn;

      /* Split ENTRY_BB at GIMPLE_*,
	 so that it can be moved to the child function.  */
      gsi = gsi_last_bb (entry_bb);
      stmt = gsi_stmt (gsi);
      gcc_assert (stmt
		  && gimple_code (stmt) == gimple_code (entry_stmt));
      e = split_block (entry_bb, stmt);
      gsi_remove (&gsi, true);
      entry_bb = e->dest;
      single_succ_edge (entry_bb)->flags = EDGE_FALLTHRU;

      /* Convert GIMPLE_OMP_RETURN into a RETURN_EXPR.  */
      if (exit_bb)
	{
	  gsi = gsi_last_bb (exit_bb);
	  gcc_assert (!gsi_end_p (gsi)
		      && gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_RETURN);
	  stmt = gimple_build_return (NULL);
	  gsi_insert_after (&gsi, stmt, GSI_SAME_STMT);
	  gsi_remove (&gsi, true);
	}

      /* Move the offloading region into CHILD_CFUN.  */

      block = gimple_block (entry_stmt);

      new_bb = move_sese_region_to_fn (child_cfun, entry_bb, exit_bb, block);
      if (exit_bb)
	single_succ_edge (new_bb)->flags = EDGE_FALLTHRU;
      /* When the OMP expansion process cannot guarantee an up-to-date
	 loop tree arrange for the child function to fixup loops.  */
      if (loops_state_satisfies_p (LOOPS_NEED_FIXUP))
	child_cfun->x_current_loops->state |= LOOPS_NEED_FIXUP;

      /* Remove non-local VAR_DECLs from child_cfun->local_decls list.  */
      num = vec_safe_length (child_cfun->local_decls);
      for (srcidx = 0, dstidx = 0; srcidx < num; srcidx++)
	{
	  t = (*child_cfun->local_decls)[srcidx];
	  if (DECL_CONTEXT (t) == cfun->decl)
	    continue;
	  if (srcidx != dstidx)
	    (*child_cfun->local_decls)[dstidx] = t;
	  dstidx++;
	}
      if (dstidx != num)
	vec_safe_truncate (child_cfun->local_decls, dstidx);

      /* Inform the callgraph about the new function.  */
      child_cfun->curr_properties = cfun->curr_properties;
      child_cfun->has_simduid_loops |= cfun->has_simduid_loops;
      child_cfun->has_force_vectorize_loops |= cfun->has_force_vectorize_loops;
      cgraph_node *node = cgraph_node::get_create (child_fn);
      node->parallelized_function = 1;
      cgraph_node::add_new_function (child_fn, true);

      /* Add the new function to the offload table.  */
      if (ENABLE_OFFLOADING)
	vec_safe_push (offload_funcs, child_fn);

      bool need_asm = DECL_ASSEMBLER_NAME_SET_P (current_function_decl)
		      && !DECL_ASSEMBLER_NAME_SET_P (child_fn);

      /* Fix the callgraph edges for child_cfun.  Those for cfun will be
	 fixed in a following pass.  */
      push_cfun (child_cfun);
      if (need_asm)
	assign_assembler_name_if_neeeded (child_fn);
      cgraph_edge::rebuild_edges ();

      /* Some EH regions might become dead, see PR34608.  If
	 pass_cleanup_cfg isn't the first pass to happen with the
	 new child, these dead EH edges might cause problems.
	 Clean them up now.  */
      if (flag_exceptions)
	{
	  basic_block bb;
	  bool changed = false;

	  FOR_EACH_BB_FN (bb, cfun)
	    changed |= gimple_purge_dead_eh_edges (bb);
	  if (changed)
	    cleanup_tree_cfg ();
	}
      if (flag_checking && !loops_state_satisfies_p (LOOPS_NEED_FIXUP))
	verify_loop_structure ();
      pop_cfun ();

      if (dump_file && !gimple_in_ssa_p (cfun))
	{
	  omp_any_child_fn_dumped = true;
	  dump_function_header (dump_file, child_fn, dump_flags);
	  dump_function_to_file (child_fn, dump_file, dump_flags);
	}
    }

  /* Emit a library call to launch the offloading region, or do data
     transfers.  */
  tree t1, t2, t3, t4, device, cond, depend, c, clauses;
  enum built_in_function start_ix;
  location_t clause_loc;
  unsigned int flags_i = 0;
  bool oacc_kernels_p = false;

  switch (gimple_omp_target_kind (entry_stmt))
    {
    case GF_OMP_TARGET_KIND_REGION:
      start_ix = BUILT_IN_GOMP_TARGET;
      break;
    case GF_OMP_TARGET_KIND_DATA:
      start_ix = BUILT_IN_GOMP_TARGET_DATA;
      break;
    case GF_OMP_TARGET_KIND_UPDATE:
      start_ix = BUILT_IN_GOMP_TARGET_UPDATE;
      break;
    case GF_OMP_TARGET_KIND_ENTER_DATA:
      start_ix = BUILT_IN_GOMP_TARGET_ENTER_EXIT_DATA;
      break;
    case GF_OMP_TARGET_KIND_EXIT_DATA:
      start_ix = BUILT_IN_GOMP_TARGET_ENTER_EXIT_DATA;
      flags_i |= GOMP_TARGET_FLAG_EXIT_DATA;
      break;
    case GF_OMP_TARGET_KIND_OACC_KERNELS:
      oacc_kernels_p = true;
      /* FALLTHROUGH */
    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
      start_ix = BUILT_IN_GOACC_PARALLEL;
      break;
    case GF_OMP_TARGET_KIND_OACC_DATA:
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      start_ix = BUILT_IN_GOACC_DATA_START;
      break;
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
      start_ix = BUILT_IN_GOACC_UPDATE;
      break;
    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
      start_ix = BUILT_IN_GOACC_ENTER_EXIT_DATA;
      break;
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      start_ix = BUILT_IN_GOACC_DECLARE;
      break;
    default:
      gcc_unreachable ();
    }

  clauses = gimple_omp_target_clauses (entry_stmt);

  /* By default, the value of DEVICE is GOMP_DEVICE_ICV (let runtime
     library choose) and there is no conditional.  */
  cond = NULL_TREE;
  device = build_int_cst (integer_type_node, GOMP_DEVICE_ICV);

  c = find_omp_clause (clauses, OMP_CLAUSE_IF);
  if (c)
    cond = OMP_CLAUSE_IF_EXPR (c);

  c = find_omp_clause (clauses, OMP_CLAUSE_DEVICE);
  if (c)
    {
      /* Even if we pass it to all library function calls, it is currently only
	 defined/used for the OpenMP target ones.  */
      gcc_checking_assert (start_ix == BUILT_IN_GOMP_TARGET
			   || start_ix == BUILT_IN_GOMP_TARGET_DATA
			   || start_ix == BUILT_IN_GOMP_TARGET_UPDATE
			   || start_ix == BUILT_IN_GOMP_TARGET_ENTER_EXIT_DATA);

      device = OMP_CLAUSE_DEVICE_ID (c);
      clause_loc = OMP_CLAUSE_LOCATION (c);
    }
  else
    clause_loc = gimple_location (entry_stmt);

  c = find_omp_clause (clauses, OMP_CLAUSE_NOWAIT);
  if (c)
    flags_i |= GOMP_TARGET_FLAG_NOWAIT;

  /* Ensure 'device' is of the correct type.  */
  device = fold_convert_loc (clause_loc, integer_type_node, device);

  /* If we found the clause 'if (cond)', build
     (cond ? device : GOMP_DEVICE_HOST_FALLBACK).  */
  if (cond)
    {
      cond = gimple_boolify (cond);

      basic_block cond_bb, then_bb, else_bb;
      edge e;
      tree tmp_var;

      tmp_var = create_tmp_var (TREE_TYPE (device));
      if (offloaded)
	e = split_block_after_labels (new_bb);
      else
	{
	  gsi = gsi_last_bb (new_bb);
	  gsi_prev (&gsi);
	  e = split_block (new_bb, gsi_stmt (gsi));
	}
      cond_bb = e->src;
      new_bb = e->dest;
      remove_edge (e);

      then_bb = create_empty_bb (cond_bb);
      else_bb = create_empty_bb (then_bb);
      set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);
      set_immediate_dominator (CDI_DOMINATORS, else_bb, cond_bb);

      stmt = gimple_build_cond_empty (cond);
      gsi = gsi_last_bb (cond_bb);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      gsi = gsi_start_bb (then_bb);
      stmt = gimple_build_assign (tmp_var, device);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      gsi = gsi_start_bb (else_bb);
      stmt = gimple_build_assign (tmp_var,
				  build_int_cst (integer_type_node,
						 GOMP_DEVICE_HOST_FALLBACK));
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
      make_edge (cond_bb, else_bb, EDGE_FALSE_VALUE);
      add_bb_to_loop (then_bb, cond_bb->loop_father);
      add_bb_to_loop (else_bb, cond_bb->loop_father);
      make_edge (then_bb, new_bb, EDGE_FALLTHRU);
      make_edge (else_bb, new_bb, EDGE_FALLTHRU);

      device = tmp_var;
    }

  gsi = gsi_last_bb (new_bb);
  t = gimple_omp_target_data_arg (entry_stmt);
  if (t == NULL)
    {
      t1 = size_zero_node;
      t2 = build_zero_cst (ptr_type_node);
      t3 = t2;
      t4 = t2;
    }
  else
    {
      t1 = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (TREE_VEC_ELT (t, 1))));
      t1 = size_binop (PLUS_EXPR, t1, size_int (1));
      t2 = build_fold_addr_expr (TREE_VEC_ELT (t, 0));
      t3 = build_fold_addr_expr (TREE_VEC_ELT (t, 1));
      t4 = build_fold_addr_expr (TREE_VEC_ELT (t, 2));
    }

  gimple *g;
  bool tagging = false;
  /* The maximum number used by any start_ix, without varargs.  */
  auto_vec<tree, 11> args;
  args.quick_push (device);
  if (offloaded)
    args.quick_push (build_fold_addr_expr (child_fn));
  args.quick_push (t1);
  args.quick_push (t2);
  args.quick_push (t3);
  args.quick_push (t4);
  switch (start_ix)
    {
    case BUILT_IN_GOACC_DATA_START:
    case BUILT_IN_GOACC_DECLARE:
    case BUILT_IN_GOMP_TARGET_DATA:
      break;
    case BUILT_IN_GOMP_TARGET:
    case BUILT_IN_GOMP_TARGET_UPDATE:
    case BUILT_IN_GOMP_TARGET_ENTER_EXIT_DATA:
      args.quick_push (build_int_cst (unsigned_type_node, flags_i));
      c = find_omp_clause (clauses, OMP_CLAUSE_DEPEND);
      if (c)
	depend = OMP_CLAUSE_DECL (c);
      else
	depend = build_int_cst (ptr_type_node, 0);
      args.quick_push (depend);
      if (start_ix == BUILT_IN_GOMP_TARGET)
	args.quick_push (get_target_arguments (&gsi, entry_stmt));
      break;
    case BUILT_IN_GOACC_PARALLEL:
      {
	set_oacc_fn_attrib (child_fn, clauses, oacc_kernels_p, &args);
	tagging = true;
      }
      /* FALLTHRU */
    case BUILT_IN_GOACC_ENTER_EXIT_DATA:
    case BUILT_IN_GOACC_UPDATE:
      {
	tree t_async = NULL_TREE;

	/* If present, use the value specified by the respective
	   clause, making sure that is of the correct type.  */
	c = find_omp_clause (clauses, OMP_CLAUSE_ASYNC);
	if (c)
	  t_async = fold_convert_loc (OMP_CLAUSE_LOCATION (c),
				      integer_type_node,
				      OMP_CLAUSE_ASYNC_EXPR (c));
	else if (!tagging)
	  /* Default values for t_async.  */
	  t_async = fold_convert_loc (gimple_location (entry_stmt),
				      integer_type_node,
				      build_int_cst (integer_type_node,
						     GOMP_ASYNC_SYNC));
	if (tagging && t_async)
	  {
	    unsigned HOST_WIDE_INT i_async = GOMP_LAUNCH_OP_MAX;

	    if (TREE_CODE (t_async) == INTEGER_CST)
	      {
		/* See if we can pack the async arg in to the tag's
		   operand.  */
		i_async = TREE_INT_CST_LOW (t_async);
		if (i_async < GOMP_LAUNCH_OP_MAX)
		  t_async = NULL_TREE;
		else
		  i_async = GOMP_LAUNCH_OP_MAX;
	      }
	    args.safe_push (oacc_launch_pack (GOMP_LAUNCH_ASYNC, NULL_TREE,
					      i_async));
	  }
	if (t_async)
	  args.safe_push (t_async);

	/* Save the argument index, and ... */
	unsigned t_wait_idx = args.length ();
	unsigned num_waits = 0;
	c = find_omp_clause (clauses, OMP_CLAUSE_WAIT);
	if (!tagging || c)
	  /* ... push a placeholder.  */
	  args.safe_push (integer_zero_node);

	for (; c; c = OMP_CLAUSE_CHAIN (c))
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_WAIT)
	    {
	      args.safe_push (fold_convert_loc (OMP_CLAUSE_LOCATION (c),
						integer_type_node,
						OMP_CLAUSE_WAIT_EXPR (c)));
	      num_waits++;
	    }

	if (!tagging || num_waits)
	  {
	    tree len;

	    /* Now that we know the number, update the placeholder.  */
	    if (tagging)
	      len = oacc_launch_pack (GOMP_LAUNCH_WAIT, NULL_TREE, num_waits);
	    else
	      len = build_int_cst (integer_type_node, num_waits);
	    len = fold_convert_loc (gimple_location (entry_stmt),
				    unsigned_type_node, len);
	    args[t_wait_idx] = len;
	  }
      }
      break;
    default:
      gcc_unreachable ();
    }
  if (tagging)
    /*  Push terminal marker - zero.  */
    args.safe_push (oacc_launch_pack (0, NULL_TREE, 0));

  g = gimple_build_call_vec (builtin_decl_explicit (start_ix), args);
  gimple_set_location (g, gimple_location (entry_stmt));
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
  if (!offloaded)
    {
      g = gsi_stmt (gsi);
      gcc_assert (g && gimple_code (g) == GIMPLE_OMP_TARGET);
      gsi_remove (&gsi, true);
    }
  if (data_region && region->exit)
    {
      gsi = gsi_last_bb (region->exit);
      g = gsi_stmt (gsi);
      gcc_assert (g && gimple_code (g) == GIMPLE_OMP_RETURN);
      gsi_remove (&gsi, true);
    }
}

/* Expand KFOR loop as a GPGPU kernel, i.e. as a body only with iteration
   variable derived from the thread number.  */

static void
grid_expand_omp_for_loop (struct omp_region *kfor)
{
  tree t, threadid;
  tree type, itype;
  gimple_stmt_iterator gsi;
  tree n1, step;
  struct omp_for_data fd;

  gomp_for *for_stmt = as_a <gomp_for *> (last_stmt (kfor->entry));
  gcc_checking_assert (gimple_omp_for_kind (for_stmt)
		       == GF_OMP_FOR_KIND_GRID_LOOP);
  basic_block body_bb = FALLTHRU_EDGE (kfor->entry)->dest;

  gcc_assert (gimple_omp_for_collapse (for_stmt) == 1);
  gcc_assert (kfor->cont);
  extract_omp_for_data (for_stmt, &fd, NULL);

  itype = type = TREE_TYPE (fd.loop.v);
  if (POINTER_TYPE_P (type))
    itype = signed_type_for (type);

  gsi = gsi_start_bb (body_bb);

  n1 = fd.loop.n1;
  step = fd.loop.step;
  n1 = force_gimple_operand_gsi (&gsi, fold_convert (type, n1),
				 true, NULL_TREE, true, GSI_SAME_STMT);
  step = force_gimple_operand_gsi (&gsi, fold_convert (itype, step),
				   true, NULL_TREE, true, GSI_SAME_STMT);
  threadid = build_call_expr (builtin_decl_explicit
			      (BUILT_IN_OMP_GET_THREAD_NUM), 0);
  threadid = fold_convert (itype, threadid);
  threadid = force_gimple_operand_gsi (&gsi, threadid, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  tree startvar = fd.loop.v;
  t = fold_build2 (MULT_EXPR, itype, threadid, step);
  if (POINTER_TYPE_P (type))
    t = fold_build_pointer_plus (n1, t);
  else
    t = fold_build2 (PLUS_EXPR, type, t, n1);
  t = fold_convert (type, t);
  t = force_gimple_operand_gsi (&gsi, t,
				DECL_P (startvar)
				&& TREE_ADDRESSABLE (startvar),
				NULL_TREE, true, GSI_SAME_STMT);
  gassign *assign_stmt = gimple_build_assign (startvar, t);
  gsi_insert_before (&gsi, assign_stmt, GSI_SAME_STMT);

  /* Remove the omp for statement */
  gsi = gsi_last_bb (kfor->entry);
  gsi_remove (&gsi, true);

  /* Remove the GIMPLE_OMP_CONTINUE statement.  */
  gsi = gsi_last_bb (kfor->cont);
  gcc_assert (!gsi_end_p (gsi)
	      && gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_CONTINUE);
  gsi_remove (&gsi, true);

  /* Replace the GIMPLE_OMP_RETURN with a real return.  */
  gsi = gsi_last_bb (kfor->exit);
  gcc_assert (!gsi_end_p (gsi)
	      && gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_RETURN);
  gsi_remove (&gsi, true);

  /* Fixup the much simpler CFG.  */
  remove_edge (find_edge (kfor->cont, body_bb));

  if (kfor->cont != body_bb)
    set_immediate_dominator (CDI_DOMINATORS, kfor->cont, body_bb);
  set_immediate_dominator (CDI_DOMINATORS, kfor->exit, kfor->cont);
}

/* Structure passed to grid_remap_kernel_arg_accesses so that it can remap
   argument_decls.  */

struct grid_arg_decl_map
{
  tree old_arg;
  tree new_arg;
};

/* Invoked through walk_gimple_op, will remap all PARM_DECLs to the ones
   pertaining to kernel function.  */

static tree
grid_remap_kernel_arg_accesses (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct grid_arg_decl_map *adm = (struct grid_arg_decl_map *) wi->info;
  tree t = *tp;

  if (t == adm->old_arg)
    *tp = adm->new_arg;
  *walk_subtrees = !TYPE_P (t) && !DECL_P (t);
  return NULL_TREE;
}

static void expand_omp (struct omp_region *region);

/* If TARGET region contains a kernel body for loop, remove its region from the
   TARGET and expand it in GPGPU kernel fashion. */

static void
grid_expand_target_grid_body (struct omp_region *target)
{
  if (!hsa_gen_requested_p ())
    return;

  gomp_target *tgt_stmt = as_a <gomp_target *> (last_stmt (target->entry));
  struct omp_region **pp;

  for (pp = &target->inner; *pp; pp = &(*pp)->next)
    if ((*pp)->type == GIMPLE_OMP_GRID_BODY)
      break;

  struct omp_region *gpukernel = *pp;

  tree orig_child_fndecl = gimple_omp_target_child_fn (tgt_stmt);
  if (!gpukernel)
    {
      /* HSA cannot handle OACC stuff.  */
      if (gimple_omp_target_kind (tgt_stmt) != GF_OMP_TARGET_KIND_REGION)
	return;
      gcc_checking_assert (orig_child_fndecl);
      gcc_assert (!find_omp_clause (gimple_omp_target_clauses (tgt_stmt),
				    OMP_CLAUSE__GRIDDIM_));
      cgraph_node *n = cgraph_node::get (orig_child_fndecl);

      hsa_register_kernel (n);
      return;
    }

  gcc_assert (find_omp_clause (gimple_omp_target_clauses (tgt_stmt),
			       OMP_CLAUSE__GRIDDIM_));
  tree inside_block = gimple_block (first_stmt (single_succ (gpukernel->entry)));
  *pp = gpukernel->next;
  for (pp = &gpukernel->inner; *pp; pp = &(*pp)->next)
    if ((*pp)->type == GIMPLE_OMP_FOR)
      break;

  struct omp_region *kfor = *pp;
  gcc_assert (kfor);
  gcc_assert (gimple_omp_for_kind (last_stmt ((kfor)->entry))
	      == GF_OMP_FOR_KIND_GRID_LOOP);
  *pp = kfor->next;
  if (kfor->inner)
    expand_omp (kfor->inner);
  if (gpukernel->inner)
    expand_omp (gpukernel->inner);

  tree kern_fndecl = copy_node (orig_child_fndecl);
  DECL_NAME (kern_fndecl) = clone_function_name (kern_fndecl, "kernel");
  SET_DECL_ASSEMBLER_NAME (kern_fndecl, DECL_NAME (kern_fndecl));
  tree tgtblock = gimple_block (tgt_stmt);
  tree fniniblock = make_node (BLOCK);
  BLOCK_ABSTRACT_ORIGIN (fniniblock) = tgtblock;
  BLOCK_SOURCE_LOCATION (fniniblock) = BLOCK_SOURCE_LOCATION (tgtblock);
  BLOCK_SOURCE_END_LOCATION (fniniblock) = BLOCK_SOURCE_END_LOCATION (tgtblock);
  DECL_INITIAL (kern_fndecl) = fniniblock;
  push_struct_function (kern_fndecl);
  cfun->function_end_locus = gimple_location (tgt_stmt);
  init_tree_ssa (cfun);
  pop_cfun ();

  tree old_parm_decl = DECL_ARGUMENTS (kern_fndecl);
  gcc_assert (!DECL_CHAIN (old_parm_decl));
  tree new_parm_decl = copy_node (DECL_ARGUMENTS (kern_fndecl));
  DECL_CONTEXT (new_parm_decl) = kern_fndecl;
  DECL_ARGUMENTS (kern_fndecl) = new_parm_decl;
  gcc_assert (VOID_TYPE_P (TREE_TYPE (DECL_RESULT (kern_fndecl))));
  DECL_RESULT (kern_fndecl) = copy_node (DECL_RESULT (kern_fndecl));
  DECL_CONTEXT (DECL_RESULT (kern_fndecl)) = kern_fndecl;
  struct function *kern_cfun = DECL_STRUCT_FUNCTION (kern_fndecl);
  kern_cfun->curr_properties = cfun->curr_properties;

  remove_edge (BRANCH_EDGE (kfor->entry));
  grid_expand_omp_for_loop (kfor);

  /* Remove the omp for statement */
  gimple_stmt_iterator gsi = gsi_last_bb (gpukernel->entry);
  gsi_remove (&gsi, true);
  /* Replace the GIMPLE_OMP_RETURN at the end of the kernel region with a real
     return.  */
  gsi = gsi_last_bb (gpukernel->exit);
  gcc_assert (!gsi_end_p (gsi)
	      && gimple_code (gsi_stmt (gsi)) == GIMPLE_OMP_RETURN);
  gimple *ret_stmt = gimple_build_return (NULL);
  gsi_insert_after (&gsi, ret_stmt, GSI_SAME_STMT);
  gsi_remove (&gsi, true);

  /* Statements in the first BB in the target construct have been produced by
     target lowering and must be copied inside the GPUKERNEL, with the two
     exceptions of the first OMP statement and the OMP_DATA assignment
     statement.  */
  gsi = gsi_start_bb (single_succ (gpukernel->entry));
  tree data_arg = gimple_omp_target_data_arg (tgt_stmt);
  tree sender = data_arg ? TREE_VEC_ELT (data_arg, 0) : NULL;
  for (gimple_stmt_iterator tsi = gsi_start_bb (single_succ (target->entry));
       !gsi_end_p (tsi); gsi_next (&tsi))
    {
      gimple *stmt = gsi_stmt (tsi);
      if (is_gimple_omp (stmt))
	break;
      if (sender
	  && is_gimple_assign (stmt)
	  && TREE_CODE (gimple_assign_rhs1 (stmt)) == ADDR_EXPR
	  && TREE_OPERAND (gimple_assign_rhs1 (stmt), 0) == sender)
	continue;
      gimple *copy = gimple_copy (stmt);
      gsi_insert_before (&gsi, copy, GSI_SAME_STMT);
      gimple_set_block (copy, fniniblock);
    }

  move_sese_region_to_fn (kern_cfun, single_succ (gpukernel->entry),
			  gpukernel->exit, inside_block);

  cgraph_node *kcn = cgraph_node::get_create (kern_fndecl);
  kcn->mark_force_output ();
  cgraph_node *orig_child = cgraph_node::get (orig_child_fndecl);

  hsa_register_kernel (kcn, orig_child);

  cgraph_node::add_new_function (kern_fndecl, true);
  push_cfun (kern_cfun);
  cgraph_edge::rebuild_edges ();

  /* Re-map any mention of the PARM_DECL of the original function to the
     PARM_DECL of the new one.

     TODO: It would be great if lowering produced references into the GPU
     kernel decl straight away and we did not have to do this.  */
  struct grid_arg_decl_map adm;
  adm.old_arg = old_parm_decl;
  adm.new_arg = new_parm_decl;
  basic_block bb;
  FOR_EACH_BB_FN (bb, kern_cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  struct walk_stmt_info wi;
	  memset (&wi, 0, sizeof (wi));
	  wi.info = &adm;
	  walk_gimple_op (stmt, grid_remap_kernel_arg_accesses, &wi);
	}
    }
  pop_cfun ();

  return;
}

/* Expand the parallel region tree rooted at REGION.  Expansion
   proceeds in depth-first order.  Innermost regions are expanded
   first.  This way, parallel regions that require a new function to
   be created (e.g., GIMPLE_OMP_PARALLEL) can be expanded without having any
   internal dependencies in their body.  */

static void
expand_omp (struct omp_region *region)
{
  omp_any_child_fn_dumped = false;
  while (region)
    {
      location_t saved_location;
      gimple *inner_stmt = NULL;

      /* First, determine whether this is a combined parallel+workshare
       	 region.  */
      if (region->type == GIMPLE_OMP_PARALLEL)
	determine_parallel_type (region);
      else if (region->type == GIMPLE_OMP_TARGET)
	grid_expand_target_grid_body (region);

      if (region->type == GIMPLE_OMP_FOR
	  && gimple_omp_for_combined_p (last_stmt (region->entry)))
	inner_stmt = last_stmt (region->inner->entry);

      if (region->inner)
	expand_omp (region->inner);

      saved_location = input_location;
      if (gimple_has_location (last_stmt (region->entry)))
	input_location = gimple_location (last_stmt (region->entry));

      switch (region->type)
	{
	case GIMPLE_OMP_PARALLEL:
	case GIMPLE_OMP_TASK:
	  expand_omp_taskreg (region);
	  break;

	case GIMPLE_OMP_FOR:
	  expand_omp_for (region, inner_stmt);
	  break;

	case GIMPLE_OMP_SECTIONS:
	  expand_omp_sections (region);
	  break;

	case GIMPLE_OMP_SECTION:
	  /* Individual omp sections are handled together with their
	     parent GIMPLE_OMP_SECTIONS region.  */
	  break;

	case GIMPLE_OMP_SINGLE:
	  expand_omp_single (region);
	  break;

	case GIMPLE_OMP_ORDERED:
	  {
	    gomp_ordered *ord_stmt
	      = as_a <gomp_ordered *> (last_stmt (region->entry));
	    if (find_omp_clause (gimple_omp_ordered_clauses (ord_stmt),
				 OMP_CLAUSE_DEPEND))
	      {
		/* We'll expand these when expanding corresponding
		   worksharing region with ordered(n) clause.  */
		gcc_assert (region->outer
			    && region->outer->type == GIMPLE_OMP_FOR);
		region->ord_stmt = ord_stmt;
		break;
	      }
	  }
	  /* FALLTHRU */
	case GIMPLE_OMP_MASTER:
	case GIMPLE_OMP_TASKGROUP:
	case GIMPLE_OMP_CRITICAL:
	case GIMPLE_OMP_TEAMS:
	  expand_omp_synch (region);
	  break;

	case GIMPLE_OMP_ATOMIC_LOAD:
	  expand_omp_atomic (region);
	  break;

	case GIMPLE_OMP_TARGET:
	  expand_omp_target (region);
	  break;

	default:
	  gcc_unreachable ();
	}

      input_location = saved_location;
      region = region->next;
    }
  if (omp_any_child_fn_dumped)
    {
      if (dump_file)
	dump_function_header (dump_file, current_function_decl, dump_flags);
      omp_any_child_fn_dumped = false;
    }
}


/* Helper for build_omp_regions.  Scan the dominator tree starting at
   block BB.  PARENT is the region that contains BB.  If SINGLE_TREE is
   true, the function ends once a single tree is built (otherwise, whole
   forest of OMP constructs may be built).  */

static void
build_omp_regions_1 (basic_block bb, struct omp_region *parent,
		     bool single_tree)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  basic_block son;

  gsi = gsi_last_bb (bb);
  if (!gsi_end_p (gsi) && is_gimple_omp (gsi_stmt (gsi)))
    {
      struct omp_region *region;
      enum gimple_code code;

      stmt = gsi_stmt (gsi);
      code = gimple_code (stmt);
      if (code == GIMPLE_OMP_RETURN)
	{
	  /* STMT is the return point out of region PARENT.  Mark it
	     as the exit point and make PARENT the immediately
	     enclosing region.  */
	  gcc_assert (parent);
	  region = parent;
	  region->exit = bb;
	  parent = parent->outer;
	}
      else if (code == GIMPLE_OMP_ATOMIC_STORE)
	{
	  /* GIMPLE_OMP_ATOMIC_STORE is analoguous to
	     GIMPLE_OMP_RETURN, but matches with
	     GIMPLE_OMP_ATOMIC_LOAD.  */
	  gcc_assert (parent);
	  gcc_assert (parent->type == GIMPLE_OMP_ATOMIC_LOAD);
	  region = parent;
	  region->exit = bb;
	  parent = parent->outer;
	}
      else if (code == GIMPLE_OMP_CONTINUE)
	{
	  gcc_assert (parent);
	  parent->cont = bb;
	}
      else if (code == GIMPLE_OMP_SECTIONS_SWITCH)
	{
	  /* GIMPLE_OMP_SECTIONS_SWITCH is part of
	     GIMPLE_OMP_SECTIONS, and we do nothing for it.  */
	}
      else
	{
	  region = new_omp_region (bb, code, parent);
	  /* Otherwise...  */
	  if (code == GIMPLE_OMP_TARGET)
	    {
	      switch (gimple_omp_target_kind (stmt))
		{
		case GF_OMP_TARGET_KIND_REGION:
		case GF_OMP_TARGET_KIND_DATA:
		case GF_OMP_TARGET_KIND_OACC_PARALLEL:
		case GF_OMP_TARGET_KIND_OACC_KERNELS:
		case GF_OMP_TARGET_KIND_OACC_DATA:
		case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
		  break;
		case GF_OMP_TARGET_KIND_UPDATE:
		case GF_OMP_TARGET_KIND_ENTER_DATA:
		case GF_OMP_TARGET_KIND_EXIT_DATA:
		case GF_OMP_TARGET_KIND_OACC_UPDATE:
		case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
		case GF_OMP_TARGET_KIND_OACC_DECLARE:
		  /* ..., other than for those stand-alone directives...  */
		  region = NULL;
		  break;
		default:
		  gcc_unreachable ();
		}
	    }
	  else if (code == GIMPLE_OMP_ORDERED
		   && find_omp_clause (gimple_omp_ordered_clauses
					 (as_a <gomp_ordered *> (stmt)),
				       OMP_CLAUSE_DEPEND))
	    /* #pragma omp ordered depend is also just a stand-alone
	       directive.  */
	    region = NULL;
	  /* ..., this directive becomes the parent for a new region.  */
	  if (region)
	    parent = region;
	}
    }

  if (single_tree && !parent)
    return;

  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    build_omp_regions_1 (son, parent, single_tree);
}

/* Builds the tree of OMP regions rooted at ROOT, storing it to
   root_omp_region.  */

static void
build_omp_regions_root (basic_block root)
{
  gcc_assert (root_omp_region == NULL);
  build_omp_regions_1 (root, NULL, true);
  gcc_assert (root_omp_region != NULL);
}

/* Expands omp construct (and its subconstructs) starting in HEAD.  */

void
omp_expand_local (basic_block head)
{
  build_omp_regions_root (head);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nOMP region tree\n\n");
      dump_omp_region (dump_file, root_omp_region, 0);
      fprintf (dump_file, "\n");
    }

  remove_exit_barriers (root_omp_region);
  expand_omp (root_omp_region);

  free_omp_regions ();
}

/* Scan the CFG and build a tree of OMP regions.  Return the root of
   the OMP region tree.  */

static void
build_omp_regions (void)
{
  gcc_assert (root_omp_region == NULL);
  calculate_dominance_info (CDI_DOMINATORS);
  build_omp_regions_1 (ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, false);
}

/* Main entry point for expanding OMP-GIMPLE into runtime calls.  */

static unsigned int
execute_expand_omp (void)
{
  build_omp_regions ();

  if (!root_omp_region)
    return 0;

  if (dump_file)
    {
      fprintf (dump_file, "\nOMP region tree\n\n");
      dump_omp_region (dump_file, root_omp_region, 0);
      fprintf (dump_file, "\n");
    }

  remove_exit_barriers (root_omp_region);

  expand_omp (root_omp_region);

  if (flag_checking && !loops_state_satisfies_p (LOOPS_NEED_FIXUP))
    verify_loop_structure ();
  cleanup_tree_cfg ();

  free_omp_regions ();

  return 0;
}

/* OMP expansion -- the default pass, run before creation of SSA form.  */

namespace {

const pass_data pass_data_expand_omp =
{
  GIMPLE_PASS, /* type */
  "ompexp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  PROP_gimple_eomp, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_expand_omp : public gimple_opt_pass
{
public:
  pass_expand_omp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_expand_omp, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      bool gate = ((flag_cilkplus != 0 || flag_openacc != 0 || flag_openmp != 0
		    || flag_openmp_simd != 0)
		   && !seen_error ());

      /* This pass always runs, to provide PROP_gimple_eomp.
	 But often, there is nothing to do.  */
      if (!gate)
	return 0;

      return execute_expand_omp ();
    }

}; // class pass_expand_omp

} // anon namespace

gimple_opt_pass *
make_pass_expand_omp (gcc::context *ctxt)
{
  return new pass_expand_omp (ctxt);
}

namespace {

const pass_data pass_data_expand_omp_ssa =
{
  GIMPLE_PASS, /* type */
  "ompexpssa", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg | PROP_ssa, /* properties_required */
  PROP_gimple_eomp, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_cleanup_cfg | TODO_rebuild_alias, /* todo_flags_finish */
};

class pass_expand_omp_ssa : public gimple_opt_pass
{
public:
  pass_expand_omp_ssa (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_expand_omp_ssa, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fun)
    {
      return !(fun->curr_properties & PROP_gimple_eomp);
    }
  virtual unsigned int execute (function *) { return execute_expand_omp (); }
  opt_pass * clone () { return new pass_expand_omp_ssa (m_ctxt); }

}; // class pass_expand_omp_ssa

} // anon namespace

gimple_opt_pass *
make_pass_expand_omp_ssa (gcc::context *ctxt)
{
  return new pass_expand_omp_ssa (ctxt);
}

/* Routines to lower OMP directives into OMP-GIMPLE.  */

/* If ctx is a worksharing context inside of a cancellable parallel
   region and it isn't nowait, add lhs to its GIMPLE_OMP_RETURN
   and conditional branch to parallel's cancel_label to handle
   cancellation in the implicit barrier.  */

static void
maybe_add_implicit_barrier_cancel (omp_context *ctx, gimple_seq *body)
{
  gimple *omp_return = gimple_seq_last_stmt (*body);
  gcc_assert (gimple_code (omp_return) == GIMPLE_OMP_RETURN);
  if (gimple_omp_return_nowait_p (omp_return))
    return;
  if (ctx->outer
      && gimple_code (ctx->outer->stmt) == GIMPLE_OMP_PARALLEL
      && ctx->outer->cancellable)
    {
      tree fndecl = builtin_decl_explicit (BUILT_IN_GOMP_CANCEL);
      tree c_bool_type = TREE_TYPE (TREE_TYPE (fndecl));
      tree lhs = create_tmp_var (c_bool_type);
      gimple_omp_return_set_lhs (omp_return, lhs);
      tree fallthru_label = create_artificial_label (UNKNOWN_LOCATION);
      gimple *g = gimple_build_cond (NE_EXPR, lhs,
				    fold_convert (c_bool_type,
						  boolean_false_node),
				    ctx->outer->cancel_label, fallthru_label);
      gimple_seq_add_stmt (body, g);
      gimple_seq_add_stmt (body, gimple_build_label (fallthru_label));
    }
}

/* Lower the OpenMP sections directive in the current statement in GSI_P.
   CTX is the enclosing OMP context for the current statement.  */

static void
lower_omp_sections (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block, control;
  gimple_stmt_iterator tgsi;
  gomp_sections *stmt;
  gimple *t;
  gbind *new_stmt, *bind;
  gimple_seq ilist, dlist, olist, new_body;

  stmt = as_a <gomp_sections *> (gsi_stmt (*gsi_p));

  push_gimplify_context ();

  dlist = NULL;
  ilist = NULL;
  lower_rec_input_clauses (gimple_omp_sections_clauses (stmt),
      			   &ilist, &dlist, ctx, NULL);

  new_body = gimple_omp_body (stmt);
  gimple_omp_set_body (stmt, NULL);
  tgsi = gsi_start (new_body);
  for (; !gsi_end_p (tgsi); gsi_next (&tgsi))
    {
      omp_context *sctx;
      gimple *sec_start;

      sec_start = gsi_stmt (tgsi);
      sctx = maybe_lookup_ctx (sec_start);
      gcc_assert (sctx);

      lower_omp (gimple_omp_body_ptr (sec_start), sctx);
      gsi_insert_seq_after (&tgsi, gimple_omp_body (sec_start),
			    GSI_CONTINUE_LINKING);
      gimple_omp_set_body (sec_start, NULL);

      if (gsi_one_before_end_p (tgsi))
	{
	  gimple_seq l = NULL;
	  lower_lastprivate_clauses (gimple_omp_sections_clauses (stmt), NULL,
				     &l, ctx);
	  gsi_insert_seq_after (&tgsi, l, GSI_CONTINUE_LINKING);
	  gimple_omp_section_set_last (sec_start);
	}

      gsi_insert_after (&tgsi, gimple_build_omp_return (false),
			GSI_CONTINUE_LINKING);
    }

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, new_body, block);

  olist = NULL;
  lower_reduction_clauses (gimple_omp_sections_clauses (stmt), &olist, ctx);

  block = make_node (BLOCK);
  new_stmt = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, new_stmt, true);

  pop_gimplify_context (new_stmt);
  gimple_bind_append_vars (new_stmt, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (bind);
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;

  new_body = NULL;
  gimple_seq_add_seq (&new_body, ilist);
  gimple_seq_add_stmt (&new_body, stmt);
  gimple_seq_add_stmt (&new_body, gimple_build_omp_sections_switch ());
  gimple_seq_add_stmt (&new_body, bind);

  control = create_tmp_var (unsigned_type_node, ".section");
  t = gimple_build_omp_continue (control, control);
  gimple_omp_sections_set_control (stmt, control);
  gimple_seq_add_stmt (&new_body, t);

  gimple_seq_add_seq (&new_body, olist);
  if (ctx->cancellable)
    gimple_seq_add_stmt (&new_body, gimple_build_label (ctx->cancel_label));
  gimple_seq_add_seq (&new_body, dlist);

  new_body = maybe_catch_exception (new_body);

  t = gimple_build_omp_return
        (!!find_omp_clause (gimple_omp_sections_clauses (stmt),
			    OMP_CLAUSE_NOWAIT));
  gimple_seq_add_stmt (&new_body, t);
  maybe_add_implicit_barrier_cancel (ctx, &new_body);

  gimple_bind_set_body (new_stmt, new_body);
}


/* A subroutine of lower_omp_single.  Expand the simple form of
   a GIMPLE_OMP_SINGLE, without a copyprivate clause:

     	if (GOMP_single_start ())
	  BODY;
	[ GOMP_barrier (); ]	-> unless 'nowait' is present.

  FIXME.  It may be better to delay expanding the logic of this until
  pass_expand_omp.  The expanded logic may make the job more difficult
  to a synchronization analysis pass.  */

static void
lower_omp_single_simple (gomp_single *single_stmt, gimple_seq *pre_p)
{
  location_t loc = gimple_location (single_stmt);
  tree tlabel = create_artificial_label (loc);
  tree flabel = create_artificial_label (loc);
  gimple *call, *cond;
  tree lhs, decl;

  decl = builtin_decl_explicit (BUILT_IN_GOMP_SINGLE_START);
  lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (decl)));
  call = gimple_build_call (decl, 0);
  gimple_call_set_lhs (call, lhs);
  gimple_seq_add_stmt (pre_p, call);

  cond = gimple_build_cond (EQ_EXPR, lhs,
			    fold_convert_loc (loc, TREE_TYPE (lhs),
					      boolean_true_node),
			    tlabel, flabel);
  gimple_seq_add_stmt (pre_p, cond);
  gimple_seq_add_stmt (pre_p, gimple_build_label (tlabel));
  gimple_seq_add_seq (pre_p, gimple_omp_body (single_stmt));
  gimple_seq_add_stmt (pre_p, gimple_build_label (flabel));
}


/* A subroutine of lower_omp_single.  Expand the simple form of
   a GIMPLE_OMP_SINGLE, with a copyprivate clause:

	#pragma omp single copyprivate (a, b, c)

   Create a new structure to hold copies of 'a', 'b' and 'c' and emit:

      {
	if ((copyout_p = GOMP_single_copy_start ()) == NULL)
	  {
	    BODY;
	    copyout.a = a;
	    copyout.b = b;
	    copyout.c = c;
	    GOMP_single_copy_end (&copyout);
	  }
	else
	  {
	    a = copyout_p->a;
	    b = copyout_p->b;
	    c = copyout_p->c;
	  }
	GOMP_barrier ();
      }

  FIXME.  It may be better to delay expanding the logic of this until
  pass_expand_omp.  The expanded logic may make the job more difficult
  to a synchronization analysis pass.  */

static void
lower_omp_single_copy (gomp_single *single_stmt, gimple_seq *pre_p,
		       omp_context *ctx)
{
  tree ptr_type, t, l0, l1, l2, bfn_decl;
  gimple_seq copyin_seq;
  location_t loc = gimple_location (single_stmt);

  ctx->sender_decl = create_tmp_var (ctx->record_type, ".omp_copy_o");

  ptr_type = build_pointer_type (ctx->record_type);
  ctx->receiver_decl = create_tmp_var (ptr_type, ".omp_copy_i");

  l0 = create_artificial_label (loc);
  l1 = create_artificial_label (loc);
  l2 = create_artificial_label (loc);

  bfn_decl = builtin_decl_explicit (BUILT_IN_GOMP_SINGLE_COPY_START);
  t = build_call_expr_loc (loc, bfn_decl, 0);
  t = fold_convert_loc (loc, ptr_type, t);
  gimplify_assign (ctx->receiver_decl, t, pre_p);

  t = build2 (EQ_EXPR, boolean_type_node, ctx->receiver_decl,
	      build_int_cst (ptr_type, 0));
  t = build3 (COND_EXPR, void_type_node, t,
	      build_and_jump (&l0), build_and_jump (&l1));
  gimplify_and_add (t, pre_p);

  gimple_seq_add_stmt (pre_p, gimple_build_label (l0));

  gimple_seq_add_seq (pre_p, gimple_omp_body (single_stmt));

  copyin_seq = NULL;
  lower_copyprivate_clauses (gimple_omp_single_clauses (single_stmt), pre_p,
			      &copyin_seq, ctx);

  t = build_fold_addr_expr_loc (loc, ctx->sender_decl);
  bfn_decl = builtin_decl_explicit (BUILT_IN_GOMP_SINGLE_COPY_END);
  t = build_call_expr_loc (loc, bfn_decl, 1, t);
  gimplify_and_add (t, pre_p);

  t = build_and_jump (&l2);
  gimplify_and_add (t, pre_p);

  gimple_seq_add_stmt (pre_p, gimple_build_label (l1));

  gimple_seq_add_seq (pre_p, copyin_seq);

  gimple_seq_add_stmt (pre_p, gimple_build_label (l2));
}


/* Expand code for an OpenMP single directive.  */

static void
lower_omp_single (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block;
  gimple *t;
  gomp_single *single_stmt = as_a <gomp_single *> (gsi_stmt (*gsi_p));
  gbind *bind;
  gimple_seq bind_body, bind_body_tail = NULL, dlist;

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  bind_body = NULL;
  dlist = NULL;
  lower_rec_input_clauses (gimple_omp_single_clauses (single_stmt),
			   &bind_body, &dlist, ctx, NULL);
  lower_omp (gimple_omp_body_ptr (single_stmt), ctx);

  gimple_seq_add_stmt (&bind_body, single_stmt);

  if (ctx->record_type)
    lower_omp_single_copy (single_stmt, &bind_body, ctx);
  else
    lower_omp_single_simple (single_stmt, &bind_body);

  gimple_omp_set_body (single_stmt, NULL);

  gimple_seq_add_seq (&bind_body, dlist);

  bind_body = maybe_catch_exception (bind_body);

  t = gimple_build_omp_return
        (!!find_omp_clause (gimple_omp_single_clauses (single_stmt),
			    OMP_CLAUSE_NOWAIT));
  gimple_seq_add_stmt (&bind_body_tail, t);
  maybe_add_implicit_barrier_cancel (ctx, &bind_body_tail);
  if (ctx->record_type)
    {
      gimple_stmt_iterator gsi = gsi_start (bind_body_tail);
      tree clobber = build_constructor (ctx->record_type, NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gsi_insert_after (&gsi, gimple_build_assign (ctx->sender_decl,
						   clobber), GSI_SAME_STMT);
    }
  gimple_seq_add_seq (&bind_body, bind_body_tail);
  gimple_bind_set_body (bind, bind_body);

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;
}


/* Expand code for an OpenMP master directive.  */

static void
lower_omp_master (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block, lab = NULL, x, bfn_decl;
  gimple *stmt = gsi_stmt (*gsi_p);
  gbind *bind;
  location_t loc = gimple_location (stmt);
  gimple_seq tseq;

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  bfn_decl = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
  x = build_call_expr_loc (loc, bfn_decl, 0);
  x = build2 (EQ_EXPR, boolean_type_node, x, integer_zero_node);
  x = build3 (COND_EXPR, void_type_node, x, NULL, build_and_jump (&lab));
  tseq = NULL;
  gimplify_and_add (x, &tseq);
  gimple_bind_add_seq (bind, tseq);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_omp_set_body (stmt, maybe_catch_exception (gimple_omp_body (stmt)));
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  gimple_bind_add_stmt (bind, gimple_build_label (lab));

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
}


/* Expand code for an OpenMP taskgroup directive.  */

static void
lower_omp_taskgroup (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  gcall *x;
  gbind *bind;
  tree block = make_node (BLOCK);

  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  x = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_TASKGROUP_START),
			 0);
  gimple_bind_add_stmt (bind, x);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
}


/* Fold the OMP_ORDERED_CLAUSES for the OMP_ORDERED in STMT if possible.  */

static void
lower_omp_ordered_clauses (gimple_stmt_iterator *gsi_p, gomp_ordered *ord_stmt,
			   omp_context *ctx)
{
  struct omp_for_data fd;
  if (!ctx->outer || gimple_code (ctx->outer->stmt) != GIMPLE_OMP_FOR)
    return;

  unsigned int len = gimple_omp_for_collapse (ctx->outer->stmt);
  struct omp_for_data_loop *loops = XALLOCAVEC (struct omp_for_data_loop, len);
  extract_omp_for_data (as_a <gomp_for *> (ctx->outer->stmt), &fd, loops);
  if (!fd.ordered)
    return;

  tree *list_p = gimple_omp_ordered_clauses_ptr (ord_stmt);
  tree c = gimple_omp_ordered_clauses (ord_stmt);
  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
      && OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK)
    {
      /* Merge depend clauses from multiple adjacent
	 #pragma omp ordered depend(sink:...) constructs
	 into one #pragma omp ordered depend(sink:...), so that
	 we can optimize them together.  */
      gimple_stmt_iterator gsi = *gsi_p;
      gsi_next (&gsi);
      while (!gsi_end_p (gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt)
	      || gimple_code (stmt) == GIMPLE_NOP)
	    {
	      gsi_next (&gsi);
	      continue;
	    }
	  if (gimple_code (stmt) != GIMPLE_OMP_ORDERED)
	    break;
	  gomp_ordered *ord_stmt2 = as_a <gomp_ordered *> (stmt);
	  c = gimple_omp_ordered_clauses (ord_stmt2);
	  if (c == NULL_TREE
	      || OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	      || OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_SINK)
	    break;
	  while (*list_p)
	    list_p = &OMP_CLAUSE_CHAIN (*list_p);
	  *list_p = c;
	  gsi_remove (&gsi, true);
	}
    }

  /* Canonicalize sink dependence clauses into one folded clause if
     possible.

     The basic algorithm is to create a sink vector whose first
     element is the GCD of all the first elements, and whose remaining
     elements are the minimum of the subsequent columns.

     We ignore dependence vectors whose first element is zero because
     such dependencies are known to be executed by the same thread.

     We take into account the direction of the loop, so a minimum
     becomes a maximum if the loop is iterating forwards.  We also
     ignore sink clauses where the loop direction is unknown, or where
     the offsets are clearly invalid because they are not a multiple
     of the loop increment.

     For example:

	#pragma omp for ordered(2)
	for (i=0; i < N; ++i)
	  for (j=0; j < M; ++j)
	    {
	      #pragma omp ordered \
		depend(sink:i-8,j-2) \
		depend(sink:i,j-1) \	// Completely ignored because i+0.
		depend(sink:i-4,j-3) \
		depend(sink:i-6,j-4)
	      #pragma omp ordered depend(source)
	    }

     Folded clause is:

	depend(sink:-gcd(8,4,6),-min(2,3,4))
	  -or-
	depend(sink:-2,-2)
  */

  /* FIXME: Computing GCD's where the first element is zero is
     non-trivial in the presence of collapsed loops.  Do this later.  */
  if (fd.collapse > 1)
    return;

  wide_int *folded_deps = XALLOCAVEC (wide_int, 2 * len - 1);
  memset (folded_deps, 0, sizeof (*folded_deps) * (2 * len - 1));
  tree folded_dep = NULL_TREE;
  /* TRUE if the first dimension's offset is negative.  */
  bool neg_offset_p = false;

  list_p = gimple_omp_ordered_clauses_ptr (ord_stmt);
  unsigned int i;
  while ((c = *list_p) != NULL)
    {
      bool remove = false;

      gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND);
      if (OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_SINK)
	goto next_ordered_clause;

      tree vec;
      for (vec = OMP_CLAUSE_DECL (c), i = 0;
	   vec && TREE_CODE (vec) == TREE_LIST;
	   vec = TREE_CHAIN (vec), ++i)
	{
	  gcc_assert (i < len);

	  /* extract_omp_for_data has canonicalized the condition.  */
	  gcc_assert (fd.loops[i].cond_code == LT_EXPR
		      || fd.loops[i].cond_code == GT_EXPR);
	  bool forward = fd.loops[i].cond_code == LT_EXPR;
	  bool maybe_lexically_later = true;

	  /* While the committee makes up its mind, bail if we have any
	     non-constant steps.  */
	  if (TREE_CODE (fd.loops[i].step) != INTEGER_CST)
	    goto lower_omp_ordered_ret;

	  tree itype = TREE_TYPE (TREE_VALUE (vec));
	  if (POINTER_TYPE_P (itype))
	    itype = sizetype;
	  wide_int offset = wide_int::from (TREE_PURPOSE (vec),
					    TYPE_PRECISION (itype),
					    TYPE_SIGN (itype));

	  /* Ignore invalid offsets that are not multiples of the step.  */
	  if (!wi::multiple_of_p
	      (wi::abs (offset), wi::abs ((wide_int) fd.loops[i].step),
	       UNSIGNED))
	    {
	      warning_at (OMP_CLAUSE_LOCATION (c), 0,
			  "ignoring sink clause with offset that is not "
			  "a multiple of the loop step");
	      remove = true;
	      goto next_ordered_clause;
	    }

	  /* Calculate the first dimension.  The first dimension of
	     the folded dependency vector is the GCD of the first
	     elements, while ignoring any first elements whose offset
	     is 0.  */
	  if (i == 0)
	    {
	      /* Ignore dependence vectors whose first dimension is 0.  */
	      if (offset == 0)
		{
		  remove = true;
		  goto next_ordered_clause;
		}
	      else
		{
		  if (!TYPE_UNSIGNED (itype) && (forward ^ wi::neg_p (offset)))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"first offset must be in opposite direction "
				"of loop iterations");
		      goto lower_omp_ordered_ret;
		    }
		  if (forward)
		    offset = -offset;
		  neg_offset_p = forward;
		  /* Initialize the first time around.  */
		  if (folded_dep == NULL_TREE)
		    {
		      folded_dep = c;
		      folded_deps[0] = offset;
		    }
		  else
		    folded_deps[0] = wi::gcd (folded_deps[0],
					      offset, UNSIGNED);
		}
	    }
	  /* Calculate minimum for the remaining dimensions.  */
	  else
	    {
	      folded_deps[len + i - 1] = offset;
	      if (folded_dep == c)
		folded_deps[i] = offset;
	      else if (maybe_lexically_later
		       && !wi::eq_p (folded_deps[i], offset))
		{
		  if (forward ^ wi::gts_p (folded_deps[i], offset))
		    {
		      unsigned int j;
		      folded_dep = c;
		      for (j = 1; j <= i; j++)
			folded_deps[j] = folded_deps[len + j - 1];
		    }
		  else
		    maybe_lexically_later = false;
		}
	    }
	}
      gcc_assert (i == len);

      remove = true;

    next_ordered_clause:
      if (remove)
	*list_p = OMP_CLAUSE_CHAIN (c);
      else
	list_p = &OMP_CLAUSE_CHAIN (c);
    }

  if (folded_dep)
    {
      if (neg_offset_p)
	folded_deps[0] = -folded_deps[0];

      tree itype = TREE_TYPE (TREE_VALUE (OMP_CLAUSE_DECL (folded_dep)));
      if (POINTER_TYPE_P (itype))
	itype = sizetype;

      TREE_PURPOSE (OMP_CLAUSE_DECL (folded_dep))
	= wide_int_to_tree (itype, folded_deps[0]);
      OMP_CLAUSE_CHAIN (folded_dep) = gimple_omp_ordered_clauses (ord_stmt);
      *gimple_omp_ordered_clauses_ptr (ord_stmt) = folded_dep;
    }

 lower_omp_ordered_ret:

  /* Ordered without clauses is #pragma omp threads, while we want
     a nop instead if we remove all clauses.  */
  if (gimple_omp_ordered_clauses (ord_stmt) == NULL_TREE)
    gsi_replace (gsi_p, gimple_build_nop (), true);
}


/* Expand code for an OpenMP ordered directive.  */

static void
lower_omp_ordered (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block;
  gimple *stmt = gsi_stmt (*gsi_p);
  gomp_ordered *ord_stmt = as_a <gomp_ordered *> (stmt);
  gcall *x;
  gbind *bind;
  bool simd = find_omp_clause (gimple_omp_ordered_clauses (ord_stmt),
			       OMP_CLAUSE_SIMD);
  bool threads = find_omp_clause (gimple_omp_ordered_clauses (ord_stmt),
				  OMP_CLAUSE_THREADS);

  if (find_omp_clause (gimple_omp_ordered_clauses (ord_stmt),
		       OMP_CLAUSE_DEPEND))
    {
      /* FIXME: This is needs to be moved to the expansion to verify various
	 conditions only testable on cfg with dominators computed, and also
	 all the depend clauses to be merged still might need to be available
	 for the runtime checks.  */
      if (0)
	lower_omp_ordered_clauses (gsi_p, ord_stmt, ctx);
      return;
    }

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  if (simd)
    {
      x = gimple_build_call_internal (IFN_GOMP_SIMD_ORDERED_START, 1,
				      build_int_cst (NULL_TREE, threads));
      cfun->has_simduid_loops = true;
    }
  else
    x = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ORDERED_START),
			   0);
  gimple_bind_add_stmt (bind, x);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_omp_set_body (stmt, maybe_catch_exception (gimple_omp_body (stmt)));
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  if (simd)
    x = gimple_build_call_internal (IFN_GOMP_SIMD_ORDERED_END, 1,
				    build_int_cst (NULL_TREE, threads));
  else
    x = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ORDERED_END),
			   0);
  gimple_bind_add_stmt (bind, x);

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (bind);
}


/* Gimplify a GIMPLE_OMP_CRITICAL statement.  This is a relatively simple
   substitution of a couple of function calls.  But in the NAMED case,
   requires that languages coordinate a symbol name.  It is therefore
   best put here in common code.  */

static GTY(()) hash_map<tree, tree> *critical_name_mutexes;

static void
lower_omp_critical (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block;
  tree name, lock, unlock;
  gomp_critical *stmt = as_a <gomp_critical *> (gsi_stmt (*gsi_p));
  gbind *bind;
  location_t loc = gimple_location (stmt);
  gimple_seq tbody;

  name = gimple_omp_critical_name (stmt);
  if (name)
    {
      tree decl;

      if (!critical_name_mutexes)
	critical_name_mutexes = hash_map<tree, tree>::create_ggc (10);

      tree *n = critical_name_mutexes->get (name);
      if (n == NULL)
	{
	  char *new_str;

	  decl = create_tmp_var_raw (ptr_type_node);

	  new_str = ACONCAT ((".gomp_critical_user_",
			      IDENTIFIER_POINTER (name), NULL));
	  DECL_NAME (decl) = get_identifier (new_str);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_COMMON (decl) = 1;
	  DECL_ARTIFICIAL (decl) = 1;
	  DECL_IGNORED_P (decl) = 1;

	  varpool_node::finalize_decl (decl);

	  critical_name_mutexes->put (name, decl);
	}
      else
	decl = *n;

      /* If '#pragma omp critical' is inside offloaded region or
	 inside function marked as offloadable, the symbol must be
	 marked as offloadable too.  */
      omp_context *octx;
      if (cgraph_node::get (current_function_decl)->offloadable)
	varpool_node::get_create (decl)->offloadable = 1;
      else
	for (octx = ctx->outer; octx; octx = octx->outer)
	  if (is_gimple_omp_offloaded (octx->stmt))
	    {
	      varpool_node::get_create (decl)->offloadable = 1;
	      break;
	    }

      lock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_NAME_START);
      lock = build_call_expr_loc (loc, lock, 1, build_fold_addr_expr_loc (loc, decl));

      unlock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_NAME_END);
      unlock = build_call_expr_loc (loc, unlock, 1,
				build_fold_addr_expr_loc (loc, decl));
    }
  else
    {
      lock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_START);
      lock = build_call_expr_loc (loc, lock, 0);

      unlock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_END);
      unlock = build_call_expr_loc (loc, unlock, 0);
    }

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  tbody = gimple_bind_body (bind);
  gimplify_and_add (lock, &tbody);
  gimple_bind_set_body (bind, tbody);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_omp_set_body (stmt, maybe_catch_exception (gimple_omp_body (stmt)));
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  tbody = gimple_bind_body (bind);
  gimplify_and_add (unlock, &tbody);
  gimple_bind_set_body (bind, tbody);

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  pop_gimplify_context (bind);
  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (bind);
}


/* A subroutine of lower_omp_for.  Generate code to emit the predicate
   for a lastprivate clause.  Given a loop control predicate of (V
   cond N2), we gate the clause on (!(V cond N2)).  The lowered form
   is appended to *DLIST, iterator initialization is appended to
   *BODY_P.  */

static void
lower_omp_for_lastprivate (struct omp_for_data *fd, gimple_seq *body_p,
			   gimple_seq *dlist, struct omp_context *ctx)
{
  tree clauses, cond, vinit;
  enum tree_code cond_code;
  gimple_seq stmts;

  cond_code = fd->loop.cond_code;
  cond_code = cond_code == LT_EXPR ? GE_EXPR : LE_EXPR;

  /* When possible, use a strict equality expression.  This can let VRP
     type optimizations deduce the value and remove a copy.  */
  if (tree_fits_shwi_p (fd->loop.step))
    {
      HOST_WIDE_INT step = tree_to_shwi (fd->loop.step);
      if (step == 1 || step == -1)
	cond_code = EQ_EXPR;
    }

  tree n2 = fd->loop.n2;
  if (fd->collapse > 1
      && TREE_CODE (n2) != INTEGER_CST
      && gimple_omp_for_combined_into_p (fd->for_stmt))
    {
      struct omp_context *taskreg_ctx = NULL;
      if (gimple_code (ctx->outer->stmt) == GIMPLE_OMP_FOR)
	{
	  gomp_for *gfor = as_a <gomp_for *> (ctx->outer->stmt);
	  if (gimple_omp_for_kind (gfor) == GF_OMP_FOR_KIND_FOR
	      || gimple_omp_for_kind (gfor) == GF_OMP_FOR_KIND_DISTRIBUTE)
	    {
	      if (gimple_omp_for_combined_into_p (gfor))
		{
		  gcc_assert (ctx->outer->outer
			      && is_parallel_ctx (ctx->outer->outer));
		  taskreg_ctx = ctx->outer->outer;
		}
	      else
		{
		  struct omp_for_data outer_fd;
		  extract_omp_for_data (gfor, &outer_fd, NULL);
		  n2 = fold_convert (TREE_TYPE (n2), outer_fd.loop.n2);
		}
	    }
	  else if (gimple_omp_for_kind (gfor) == GF_OMP_FOR_KIND_TASKLOOP)
	    taskreg_ctx = ctx->outer->outer;
	}
      else if (is_taskreg_ctx (ctx->outer))
	taskreg_ctx = ctx->outer;
      if (taskreg_ctx)
	{
	  int i;
	  tree innerc
	    = find_omp_clause (gimple_omp_taskreg_clauses (taskreg_ctx->stmt),
			       OMP_CLAUSE__LOOPTEMP_);
	  gcc_assert (innerc);
	  for (i = 0; i < fd->collapse; i++)
	    {
	      innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
					OMP_CLAUSE__LOOPTEMP_);
	      gcc_assert (innerc);
	    }
	  innerc = find_omp_clause (OMP_CLAUSE_CHAIN (innerc),
				    OMP_CLAUSE__LOOPTEMP_);
	  if (innerc)
	    n2 = fold_convert (TREE_TYPE (n2),
			       lookup_decl (OMP_CLAUSE_DECL (innerc),
					    taskreg_ctx));
	}
    }
  cond = build2 (cond_code, boolean_type_node, fd->loop.v, n2);

  clauses = gimple_omp_for_clauses (fd->for_stmt);
  stmts = NULL;
  lower_lastprivate_clauses (clauses, cond, &stmts, ctx);
  if (!gimple_seq_empty_p (stmts))
    {
      gimple_seq_add_seq (&stmts, *dlist);
      *dlist = stmts;

      /* Optimize: v = 0; is usually cheaper than v = some_other_constant.  */
      vinit = fd->loop.n1;
      if (cond_code == EQ_EXPR
	  && tree_fits_shwi_p (fd->loop.n2)
	  && ! integer_zerop (fd->loop.n2))
	vinit = build_int_cst (TREE_TYPE (fd->loop.v), 0);
      else
	vinit = unshare_expr (vinit);

      /* Initialize the iterator variable, so that threads that don't execute
	 any iterations don't execute the lastprivate clauses by accident.  */
      gimplify_assign (fd->loop.v, vinit, body_p);
    }
}


/* Lower code for an OMP loop directive.  */

static void
lower_omp_for (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree *rhs_p, block;
  struct omp_for_data fd, *fdp = NULL;
  gomp_for *stmt = as_a <gomp_for *> (gsi_stmt (*gsi_p));
  gbind *new_stmt;
  gimple_seq omp_for_body, body, dlist;
  gimple_seq oacc_head = NULL, oacc_tail = NULL;
  size_t i;

  push_gimplify_context ();

  lower_omp (gimple_omp_for_pre_body_ptr (stmt), ctx);

  block = make_node (BLOCK);
  new_stmt = gimple_build_bind (NULL, NULL, block);
  /* Replace at gsi right away, so that 'stmt' is no member
     of a sequence anymore as we're going to add to a different
     one below.  */
  gsi_replace (gsi_p, new_stmt, true);

  /* Move declaration of temporaries in the loop body before we make
     it go away.  */
  omp_for_body = gimple_omp_body (stmt);
  if (!gimple_seq_empty_p (omp_for_body)
      && gimple_code (gimple_seq_first_stmt (omp_for_body)) == GIMPLE_BIND)
    {
      gbind *inner_bind
	= as_a <gbind *> (gimple_seq_first_stmt (omp_for_body));
      tree vars = gimple_bind_vars (inner_bind);
      gimple_bind_append_vars (new_stmt, vars);
      /* bind_vars/BLOCK_VARS are being moved to new_stmt/block, don't
	 keep them on the inner_bind and it's block.  */
      gimple_bind_set_vars (inner_bind, NULL_TREE);
      if (gimple_bind_block (inner_bind))
	BLOCK_VARS (gimple_bind_block (inner_bind)) = NULL_TREE;
    }

  if (gimple_omp_for_combined_into_p (stmt))
    {
      extract_omp_for_data (stmt, &fd, NULL);
      fdp = &fd;

      /* We need two temporaries with fd.loop.v type (istart/iend)
	 and then (fd.collapse - 1) temporaries with the same
	 type for count2 ... countN-1 vars if not constant.  */
      size_t count = 2;
      tree type = fd.iter_type;
      if (fd.collapse > 1
	  && TREE_CODE (fd.loop.n2) != INTEGER_CST)
	count += fd.collapse - 1;
      bool taskreg_for
	= (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_FOR
	   || gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_TASKLOOP);
      tree outerc = NULL, *pc = gimple_omp_for_clauses_ptr (stmt);
      tree clauses = *pc;
      if (taskreg_for)
	outerc
	  = find_omp_clause (gimple_omp_taskreg_clauses (ctx->outer->stmt),
			     OMP_CLAUSE__LOOPTEMP_);
      for (i = 0; i < count; i++)
	{
	  tree temp;
	  if (taskreg_for)
	    {
	      gcc_assert (outerc);
	      temp = lookup_decl (OMP_CLAUSE_DECL (outerc), ctx->outer);
	      outerc = find_omp_clause (OMP_CLAUSE_CHAIN (outerc),
					OMP_CLAUSE__LOOPTEMP_);
	    }
	  else
	    {
	      temp = create_tmp_var (type);
	      insert_decl_map (&ctx->outer->cb, temp, temp);
	    }
	  *pc = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__LOOPTEMP_);
	  OMP_CLAUSE_DECL (*pc) = temp;
	  pc = &OMP_CLAUSE_CHAIN (*pc);
	}
      *pc = clauses;
    }

  /* The pre-body and input clauses go before the lowered GIMPLE_OMP_FOR.  */
  dlist = NULL;
  body = NULL;
  lower_rec_input_clauses (gimple_omp_for_clauses (stmt), &body, &dlist, ctx,
			   fdp);
  gimple_seq_add_seq (&body, gimple_omp_for_pre_body (stmt));

  lower_omp (gimple_omp_body_ptr (stmt), ctx);

  /* Lower the header expressions.  At this point, we can assume that
     the header is of the form:

     	#pragma omp for (V = VAL1; V {<|>|<=|>=} VAL2; V = V [+-] VAL3)

     We just need to make sure that VAL1, VAL2 and VAL3 are lowered
     using the .omp_data_s mapping, if needed.  */
  for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
    {
      rhs_p = gimple_omp_for_initial_ptr (stmt, i);
      if (!is_gimple_min_invariant (*rhs_p))
	*rhs_p = get_formal_tmp_var (*rhs_p, &body);

      rhs_p = gimple_omp_for_final_ptr (stmt, i);
      if (!is_gimple_min_invariant (*rhs_p))
	*rhs_p = get_formal_tmp_var (*rhs_p, &body);

      rhs_p = &TREE_OPERAND (gimple_omp_for_incr (stmt, i), 1);
      if (!is_gimple_min_invariant (*rhs_p))
	*rhs_p = get_formal_tmp_var (*rhs_p, &body);
    }

  /* Once lowered, extract the bounds and clauses.  */
  extract_omp_for_data (stmt, &fd, NULL);

  if (is_gimple_omp_oacc (ctx->stmt)
      && !ctx_in_oacc_kernels_region (ctx))
    lower_oacc_head_tail (gimple_location (stmt),
			  gimple_omp_for_clauses (stmt),
			  &oacc_head, &oacc_tail, ctx);

  /* Add OpenACC partitioning and reduction markers just before the loop  */
  if (oacc_head)
    gimple_seq_add_seq (&body, oacc_head);
  
  lower_omp_for_lastprivate (&fd, &body, &dlist, ctx);

  if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_FOR)
    for (tree c = gimple_omp_for_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
	{
	  OMP_CLAUSE_DECL (c) = lookup_decl (OMP_CLAUSE_DECL (c), ctx);
	  if (DECL_P (OMP_CLAUSE_LINEAR_STEP (c)))
	    OMP_CLAUSE_LINEAR_STEP (c)
	      = maybe_lookup_decl_in_outer_ctx (OMP_CLAUSE_LINEAR_STEP (c),
						ctx);
	}

  if (!gimple_omp_for_grid_phony (stmt))
    gimple_seq_add_stmt (&body, stmt);
  gimple_seq_add_seq (&body, gimple_omp_body (stmt));

  if (!gimple_omp_for_grid_phony (stmt))
    gimple_seq_add_stmt (&body, gimple_build_omp_continue (fd.loop.v,
							   fd.loop.v));

  /* After the loop, add exit clauses.  */
  lower_reduction_clauses (gimple_omp_for_clauses (stmt), &body, ctx);

  if (ctx->cancellable)
    gimple_seq_add_stmt (&body, gimple_build_label (ctx->cancel_label));

  gimple_seq_add_seq (&body, dlist);

  body = maybe_catch_exception (body);

  if (!gimple_omp_for_grid_phony (stmt))
    {
      /* Region exit marker goes at the end of the loop body.  */
      gimple_seq_add_stmt (&body, gimple_build_omp_return (fd.have_nowait));
      maybe_add_implicit_barrier_cancel (ctx, &body);
    }

  /* Add OpenACC joining and reduction markers just after the loop.  */
  if (oacc_tail)
    gimple_seq_add_seq (&body, oacc_tail);

  pop_gimplify_context (new_stmt);

  gimple_bind_append_vars (new_stmt, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (new_stmt);
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;

  gimple_bind_set_body (new_stmt, body);
  gimple_omp_set_body (stmt, NULL);
  gimple_omp_for_set_pre_body (stmt, NULL);
}

/* Callback for walk_stmts.  Check if the current statement only contains
   GIMPLE_OMP_FOR or GIMPLE_OMP_SECTIONS.  */

static tree
check_combined_parallel (gimple_stmt_iterator *gsi_p,
    			 bool *handled_ops_p,
    			 struct walk_stmt_info *wi)
{
  int *info = (int *) wi->info;
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_FOR:
    case GIMPLE_OMP_SECTIONS:
      *info = *info == 0 ? 1 : -1;
      break;
    default:
      *info = -1;
      break;
    }
  return NULL;
}

struct omp_taskcopy_context
{
  /* This field must be at the beginning, as we do "inheritance": Some
     callback functions for tree-inline.c (e.g., omp_copy_decl)
     receive a copy_body_data pointer that is up-casted to an
     omp_context pointer.  */
  copy_body_data cb;
  omp_context *ctx;
};

static tree
task_copyfn_copy_decl (tree var, copy_body_data *cb)
{
  struct omp_taskcopy_context *tcctx = (struct omp_taskcopy_context *) cb;

  if (splay_tree_lookup (tcctx->ctx->sfield_map, (splay_tree_key) var))
    return create_tmp_var (TREE_TYPE (var));

  return var;
}

static tree
task_copyfn_remap_type (struct omp_taskcopy_context *tcctx, tree orig_type)
{
  tree name, new_fields = NULL, type, f;

  type = lang_hooks.types.make_type (RECORD_TYPE);
  name = DECL_NAME (TYPE_NAME (orig_type));
  name = build_decl (gimple_location (tcctx->ctx->stmt),
		     TYPE_DECL, name, type);
  TYPE_NAME (type) = name;

  for (f = TYPE_FIELDS (orig_type); f ; f = TREE_CHAIN (f))
    {
      tree new_f = copy_node (f);
      DECL_CONTEXT (new_f) = type;
      TREE_TYPE (new_f) = remap_type (TREE_TYPE (f), &tcctx->cb);
      TREE_CHAIN (new_f) = new_fields;
      walk_tree (&DECL_SIZE (new_f), copy_tree_body_r, &tcctx->cb, NULL);
      walk_tree (&DECL_SIZE_UNIT (new_f), copy_tree_body_r, &tcctx->cb, NULL);
      walk_tree (&DECL_FIELD_OFFSET (new_f), copy_tree_body_r,
		 &tcctx->cb, NULL);
      new_fields = new_f;
      tcctx->cb.decl_map->put (f, new_f);
    }
  TYPE_FIELDS (type) = nreverse (new_fields);
  layout_type (type);
  return type;
}

/* Create task copyfn.  */

static void
create_task_copyfn (gomp_task *task_stmt, omp_context *ctx)
{
  struct function *child_cfun;
  tree child_fn, t, c, src, dst, f, sf, arg, sarg, decl;
  tree record_type, srecord_type, bind, list;
  bool record_needs_remap = false, srecord_needs_remap = false;
  splay_tree_node n;
  struct omp_taskcopy_context tcctx;
  location_t loc = gimple_location (task_stmt);

  child_fn = gimple_omp_task_copy_fn (task_stmt);
  child_cfun = DECL_STRUCT_FUNCTION (child_fn);
  gcc_assert (child_cfun->cfg == NULL);
  DECL_SAVED_TREE (child_fn) = alloc_stmt_list ();

  /* Reset DECL_CONTEXT on function arguments.  */
  for (t = DECL_ARGUMENTS (child_fn); t; t = DECL_CHAIN (t))
    DECL_CONTEXT (t) = child_fn;

  /* Populate the function.  */
  push_gimplify_context ();
  push_cfun (child_cfun);

  bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  TREE_SIDE_EFFECTS (bind) = 1;
  list = NULL;
  DECL_SAVED_TREE (child_fn) = bind;
  DECL_SOURCE_LOCATION (child_fn) = gimple_location (task_stmt);

  /* Remap src and dst argument types if needed.  */
  record_type = ctx->record_type;
  srecord_type = ctx->srecord_type;
  for (f = TYPE_FIELDS (record_type); f ; f = DECL_CHAIN (f))
    if (variably_modified_type_p (TREE_TYPE (f), ctx->cb.src_fn))
      {
	record_needs_remap = true;
	break;
      }
  for (f = TYPE_FIELDS (srecord_type); f ; f = DECL_CHAIN (f))
    if (variably_modified_type_p (TREE_TYPE (f), ctx->cb.src_fn))
      {
	srecord_needs_remap = true;
	break;
      }

  if (record_needs_remap || srecord_needs_remap)
    {
      memset (&tcctx, '\0', sizeof (tcctx));
      tcctx.cb.src_fn = ctx->cb.src_fn;
      tcctx.cb.dst_fn = child_fn;
      tcctx.cb.src_node = cgraph_node::get (tcctx.cb.src_fn);
      gcc_checking_assert (tcctx.cb.src_node);
      tcctx.cb.dst_node = tcctx.cb.src_node;
      tcctx.cb.src_cfun = ctx->cb.src_cfun;
      tcctx.cb.copy_decl = task_copyfn_copy_decl;
      tcctx.cb.eh_lp_nr = 0;
      tcctx.cb.transform_call_graph_edges = CB_CGE_MOVE;
      tcctx.cb.decl_map = new hash_map<tree, tree>;
      tcctx.ctx = ctx;

      if (record_needs_remap)
	record_type = task_copyfn_remap_type (&tcctx, record_type);
      if (srecord_needs_remap)
	srecord_type = task_copyfn_remap_type (&tcctx, srecord_type);
    }
  else
    tcctx.cb.decl_map = NULL;

  arg = DECL_ARGUMENTS (child_fn);
  TREE_TYPE (arg) = build_pointer_type (record_type);
  sarg = DECL_CHAIN (arg);
  TREE_TYPE (sarg) = build_pointer_type (srecord_type);

  /* First pass: initialize temporaries used in record_type and srecord_type
     sizes and field offsets.  */
  if (tcctx.cb.decl_map)
    for (c = gimple_omp_task_clauses (task_stmt); c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	{
	  tree *p;

	  decl = OMP_CLAUSE_DECL (c);
	  p = tcctx.cb.decl_map->get (decl);
	  if (p == NULL)
	    continue;
	  n = splay_tree_lookup (ctx->sfield_map, (splay_tree_key) decl);
	  sf = (tree) n->value;
	  sf = *tcctx.cb.decl_map->get (sf);
	  src = build_simple_mem_ref_loc (loc, sarg);
	  src = omp_build_component_ref (src, sf);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (*p), *p, src);
	  append_to_statement_list (t, &list);
	}

  /* Second pass: copy shared var pointers and copy construct non-VLA
     firstprivate vars.  */
  for (c = gimple_omp_task_clauses (task_stmt); c; c = OMP_CLAUSE_CHAIN (c))
    switch (OMP_CLAUSE_CODE (c))
      {
	splay_tree_key key;
      case OMP_CLAUSE_SHARED:
	decl = OMP_CLAUSE_DECL (c);
	key = (splay_tree_key) decl;
	if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	  key = (splay_tree_key) &DECL_UID (decl);
	n = splay_tree_lookup (ctx->field_map, key);
	if (n == NULL)
	  break;
	f = (tree) n->value;
	if (tcctx.cb.decl_map)
	  f = *tcctx.cb.decl_map->get (f);
	n = splay_tree_lookup (ctx->sfield_map, key);
	sf = (tree) n->value;
	if (tcctx.cb.decl_map)
	  sf = *tcctx.cb.decl_map->get (sf);
	src = build_simple_mem_ref_loc (loc, sarg);
	src = omp_build_component_ref (src, sf);
	dst = build_simple_mem_ref_loc (loc, arg);
	dst = omp_build_component_ref (dst, f);
	t = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
	append_to_statement_list (t, &list);
	break;
      case OMP_CLAUSE_FIRSTPRIVATE:
	decl = OMP_CLAUSE_DECL (c);
	if (is_variable_sized (decl))
	  break;
	n = splay_tree_lookup (ctx->field_map, (splay_tree_key) decl);
	if (n == NULL)
	  break;
	f = (tree) n->value;
	if (tcctx.cb.decl_map)
	  f = *tcctx.cb.decl_map->get (f);
	n = splay_tree_lookup (ctx->sfield_map, (splay_tree_key) decl);
	if (n != NULL)
	  {
	    sf = (tree) n->value;
	    if (tcctx.cb.decl_map)
	      sf = *tcctx.cb.decl_map->get (sf);
	    src = build_simple_mem_ref_loc (loc, sarg);
	    src = omp_build_component_ref (src, sf);
	    if (use_pointer_for_field (decl, NULL) || is_reference (decl))
	      src = build_simple_mem_ref_loc (loc, src);
	  }
	else
	  src = decl;
	dst = build_simple_mem_ref_loc (loc, arg);
	dst = omp_build_component_ref (dst, f);
	t = lang_hooks.decls.omp_clause_copy_ctor (c, dst, src);
	append_to_statement_list (t, &list);
	break;
      case OMP_CLAUSE_PRIVATE:
	if (! OMP_CLAUSE_PRIVATE_OUTER_REF (c))
	  break;
	decl = OMP_CLAUSE_DECL (c);
	n = splay_tree_lookup (ctx->field_map, (splay_tree_key) decl);
	f = (tree) n->value;
	if (tcctx.cb.decl_map)
	  f = *tcctx.cb.decl_map->get (f);
	n = splay_tree_lookup (ctx->sfield_map, (splay_tree_key) decl);
	if (n != NULL)
	  {
	    sf = (tree) n->value;
	    if (tcctx.cb.decl_map)
	      sf = *tcctx.cb.decl_map->get (sf);
	    src = build_simple_mem_ref_loc (loc, sarg);
	    src = omp_build_component_ref (src, sf);
	    if (use_pointer_for_field (decl, NULL))
	      src = build_simple_mem_ref_loc (loc, src);
	  }
	else
	  src = decl;
	dst = build_simple_mem_ref_loc (loc, arg);
	dst = omp_build_component_ref (dst, f);
	t = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
	append_to_statement_list (t, &list);
	break;
      default:
	break;
      }

  /* Last pass: handle VLA firstprivates.  */
  if (tcctx.cb.decl_map)
    for (c = gimple_omp_task_clauses (task_stmt); c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	{
	  tree ind, ptr, df;

	  decl = OMP_CLAUSE_DECL (c);
	  if (!is_variable_sized (decl))
	    continue;
	  n = splay_tree_lookup (ctx->field_map, (splay_tree_key) decl);
	  if (n == NULL)
	    continue;
	  f = (tree) n->value;
	  f = *tcctx.cb.decl_map->get (f);
	  gcc_assert (DECL_HAS_VALUE_EXPR_P (decl));
	  ind = DECL_VALUE_EXPR (decl);
	  gcc_assert (TREE_CODE (ind) == INDIRECT_REF);
	  gcc_assert (DECL_P (TREE_OPERAND (ind, 0)));
	  n = splay_tree_lookup (ctx->sfield_map,
				 (splay_tree_key) TREE_OPERAND (ind, 0));
	  sf = (tree) n->value;
	  sf = *tcctx.cb.decl_map->get (sf);
	  src = build_simple_mem_ref_loc (loc, sarg);
	  src = omp_build_component_ref (src, sf);
	  src = build_simple_mem_ref_loc (loc, src);
	  dst = build_simple_mem_ref_loc (loc, arg);
	  dst = omp_build_component_ref (dst, f);
	  t = lang_hooks.decls.omp_clause_copy_ctor (c, dst, src);
	  append_to_statement_list (t, &list);
	  n = splay_tree_lookup (ctx->field_map,
				 (splay_tree_key) TREE_OPERAND (ind, 0));
	  df = (tree) n->value;
	  df = *tcctx.cb.decl_map->get (df);
	  ptr = build_simple_mem_ref_loc (loc, arg);
	  ptr = omp_build_component_ref (ptr, df);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (ptr), ptr,
		      build_fold_addr_expr_loc (loc, dst));
	  append_to_statement_list (t, &list);
	}

  t = build1 (RETURN_EXPR, void_type_node, NULL);
  append_to_statement_list (t, &list);

  if (tcctx.cb.decl_map)
    delete tcctx.cb.decl_map;
  pop_gimplify_context (NULL);
  BIND_EXPR_BODY (bind) = list;
  pop_cfun ();
}

static void
lower_depend_clauses (tree *pclauses, gimple_seq *iseq, gimple_seq *oseq)
{
  tree c, clauses;
  gimple *g;
  size_t n_in = 0, n_out = 0, idx = 2, i;

  clauses = find_omp_clause (*pclauses, OMP_CLAUSE_DEPEND);
  gcc_assert (clauses);
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
      switch (OMP_CLAUSE_DEPEND_KIND (c))
	{
	case OMP_CLAUSE_DEPEND_IN:
	  n_in++;
	  break;
	case OMP_CLAUSE_DEPEND_OUT:
	case OMP_CLAUSE_DEPEND_INOUT:
	  n_out++;
	  break;
	case OMP_CLAUSE_DEPEND_SOURCE:
	case OMP_CLAUSE_DEPEND_SINK:
	  /* FALLTHRU */
	default:
	  gcc_unreachable ();
	}
  tree type = build_array_type_nelts (ptr_type_node, n_in + n_out + 2);
  tree array = create_tmp_var (type);
  TREE_ADDRESSABLE (array) = 1;
  tree r = build4 (ARRAY_REF, ptr_type_node, array, size_int (0), NULL_TREE,
		   NULL_TREE);
  g = gimple_build_assign (r, build_int_cst (ptr_type_node, n_in + n_out));
  gimple_seq_add_stmt (iseq, g);
  r = build4 (ARRAY_REF, ptr_type_node, array, size_int (1), NULL_TREE,
	      NULL_TREE);
  g = gimple_build_assign (r, build_int_cst (ptr_type_node, n_out));
  gimple_seq_add_stmt (iseq, g);
  for (i = 0; i < 2; i++)
    {
      if ((i ? n_in : n_out) == 0)
	continue;
      for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && ((OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_IN) ^ i))
	  {
	    tree t = OMP_CLAUSE_DECL (c);
	    t = fold_convert (ptr_type_node, t);
	    gimplify_expr (&t, iseq, NULL, is_gimple_val, fb_rvalue);
	    r = build4 (ARRAY_REF, ptr_type_node, array, size_int (idx++),
			NULL_TREE, NULL_TREE);
	    g = gimple_build_assign (r, t);
	    gimple_seq_add_stmt (iseq, g);
	  }
    }
  c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_DEPEND);
  OMP_CLAUSE_DECL (c) = build_fold_addr_expr (array);
  OMP_CLAUSE_CHAIN (c) = *pclauses;
  *pclauses = c;
  tree clobber = build_constructor (type, NULL);
  TREE_THIS_VOLATILE (clobber) = 1;
  g = gimple_build_assign (array, clobber);
  gimple_seq_add_stmt (oseq, g);
}

/* Lower the OpenMP parallel or task directive in the current statement
   in GSI_P.  CTX holds context information for the directive.  */

static void
lower_omp_taskreg (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree clauses;
  tree child_fn, t;
  gimple *stmt = gsi_stmt (*gsi_p);
  gbind *par_bind, *bind, *dep_bind = NULL;
  gimple_seq par_body, olist, ilist, par_olist, par_rlist, par_ilist, new_body;
  location_t loc = gimple_location (stmt);

  clauses = gimple_omp_taskreg_clauses (stmt);
  par_bind
    = as_a <gbind *> (gimple_seq_first_stmt (gimple_omp_body (stmt)));
  par_body = gimple_bind_body (par_bind);
  child_fn = ctx->cb.dst_fn;
  if (gimple_code (stmt) == GIMPLE_OMP_PARALLEL
      && !gimple_omp_parallel_combined_p (stmt))
    {
      struct walk_stmt_info wi;
      int ws_num = 0;

      memset (&wi, 0, sizeof (wi));
      wi.info = &ws_num;
      wi.val_only = true;
      walk_gimple_seq (par_body, check_combined_parallel, NULL, &wi);
      if (ws_num == 1)
	gimple_omp_parallel_set_combined_p (stmt, true);
    }
  gimple_seq dep_ilist = NULL;
  gimple_seq dep_olist = NULL;
  if (gimple_code (stmt) == GIMPLE_OMP_TASK
      && find_omp_clause (clauses, OMP_CLAUSE_DEPEND))
    {
      push_gimplify_context ();
      dep_bind = gimple_build_bind (NULL, NULL, make_node (BLOCK));
      lower_depend_clauses (gimple_omp_task_clauses_ptr (stmt),
			    &dep_ilist, &dep_olist);
    }

  if (ctx->srecord_type)
    create_task_copyfn (as_a <gomp_task *> (stmt), ctx);

  push_gimplify_context ();

  par_olist = NULL;
  par_ilist = NULL;
  par_rlist = NULL;
  bool phony_construct = gimple_code (stmt) == GIMPLE_OMP_PARALLEL
    && gimple_omp_parallel_grid_phony (as_a <gomp_parallel *> (stmt));
  if (phony_construct && ctx->record_type)
    {
      gcc_checking_assert (!ctx->receiver_decl);
      ctx->receiver_decl = create_tmp_var
	(build_reference_type (ctx->record_type), ".omp_rec");
    }
  lower_rec_input_clauses (clauses, &par_ilist, &par_olist, ctx, NULL);
  lower_omp (&par_body, ctx);
  if (gimple_code (stmt) == GIMPLE_OMP_PARALLEL)
    lower_reduction_clauses (clauses, &par_rlist, ctx);

  /* Declare all the variables created by mapping and the variables
     declared in the scope of the parallel body.  */
  record_vars_into (ctx->block_vars, child_fn);
  record_vars_into (gimple_bind_vars (par_bind), child_fn);

  if (ctx->record_type)
    {
      ctx->sender_decl
	= create_tmp_var (ctx->srecord_type ? ctx->srecord_type
			  : ctx->record_type, ".omp_data_o");
      DECL_NAMELESS (ctx->sender_decl) = 1;
      TREE_ADDRESSABLE (ctx->sender_decl) = 1;
      gimple_omp_taskreg_set_data_arg (stmt, ctx->sender_decl);
    }

  olist = NULL;
  ilist = NULL;
  lower_send_clauses (clauses, &ilist, &olist, ctx);
  lower_send_shared_vars (&ilist, &olist, ctx);

  if (ctx->record_type)
    {
      tree clobber = build_constructor (TREE_TYPE (ctx->sender_decl), NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gimple_seq_add_stmt (&olist, gimple_build_assign (ctx->sender_decl,
							clobber));
    }

  /* Once all the expansions are done, sequence all the different
     fragments inside gimple_omp_body.  */

  new_body = NULL;

  if (ctx->record_type)
    {
      t = build_fold_addr_expr_loc (loc, ctx->sender_decl);
      /* fixup_child_record_type might have changed receiver_decl's type.  */
      t = fold_convert_loc (loc, TREE_TYPE (ctx->receiver_decl), t);
      gimple_seq_add_stmt (&new_body,
	  		   gimple_build_assign (ctx->receiver_decl, t));
    }

  gimple_seq_add_seq (&new_body, par_ilist);
  gimple_seq_add_seq (&new_body, par_body);
  gimple_seq_add_seq (&new_body, par_rlist);
  if (ctx->cancellable)
    gimple_seq_add_stmt (&new_body, gimple_build_label (ctx->cancel_label));
  gimple_seq_add_seq (&new_body, par_olist);
  new_body = maybe_catch_exception (new_body);
  if (gimple_code (stmt) == GIMPLE_OMP_TASK)
    gimple_seq_add_stmt (&new_body,
			 gimple_build_omp_continue (integer_zero_node,
						    integer_zero_node));
  if (!phony_construct)
    {
      gimple_seq_add_stmt (&new_body, gimple_build_omp_return (false));
      gimple_omp_set_body (stmt, new_body);
    }

  bind = gimple_build_bind (NULL, NULL, gimple_bind_block (par_bind));
  gsi_replace (gsi_p, dep_bind ? dep_bind : bind, true);
  gimple_bind_add_seq (bind, ilist);
  if (!phony_construct)
    gimple_bind_add_stmt (bind, stmt);
  else
    gimple_bind_add_seq (bind, new_body);
  gimple_bind_add_seq (bind, olist);

  pop_gimplify_context (NULL);

  if (dep_bind)
    {
      gimple_bind_add_seq (dep_bind, dep_ilist);
      gimple_bind_add_stmt (dep_bind, bind);
      gimple_bind_add_seq (dep_bind, dep_olist);
      pop_gimplify_context (dep_bind);
    }
}

/* Lower the GIMPLE_OMP_TARGET in the current statement
   in GSI_P.  CTX holds context information for the directive.  */

static void
lower_omp_target (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree clauses;
  tree child_fn, t, c;
  gomp_target *stmt = as_a <gomp_target *> (gsi_stmt (*gsi_p));
  gbind *tgt_bind, *bind, *dep_bind = NULL;
  gimple_seq tgt_body, olist, ilist, fplist, new_body;
  location_t loc = gimple_location (stmt);
  bool offloaded, data_region;
  unsigned int map_cnt = 0;

  offloaded = is_gimple_omp_offloaded (stmt);
  switch (gimple_omp_target_kind (stmt))
    {
    case GF_OMP_TARGET_KIND_REGION:
    case GF_OMP_TARGET_KIND_UPDATE:
    case GF_OMP_TARGET_KIND_ENTER_DATA:
    case GF_OMP_TARGET_KIND_EXIT_DATA:
    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
    case GF_OMP_TARGET_KIND_OACC_KERNELS:
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      data_region = false;
      break;
    case GF_OMP_TARGET_KIND_DATA:
    case GF_OMP_TARGET_KIND_OACC_DATA:
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      data_region = true;
      break;
    default:
      gcc_unreachable ();
    }

  clauses = gimple_omp_target_clauses (stmt);

  gimple_seq dep_ilist = NULL;
  gimple_seq dep_olist = NULL;
  if (find_omp_clause (clauses, OMP_CLAUSE_DEPEND))
    {
      push_gimplify_context ();
      dep_bind = gimple_build_bind (NULL, NULL, make_node (BLOCK));
      lower_depend_clauses (gimple_omp_target_clauses_ptr (stmt),
			    &dep_ilist, &dep_olist);
    }

  tgt_bind = NULL;
  tgt_body = NULL;
  if (offloaded)
    {
      tgt_bind = gimple_seq_first_stmt_as_a_bind (gimple_omp_body (stmt));
      tgt_body = gimple_bind_body (tgt_bind);
    }
  else if (data_region)
    tgt_body = gimple_omp_body (stmt);
  child_fn = ctx->cb.dst_fn;

  push_gimplify_context ();
  fplist = NULL;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    switch (OMP_CLAUSE_CODE (c))
      {
	tree var, x;

      default:
	break;
      case OMP_CLAUSE_MAP:
#if CHECKING_P
	/* First check what we're prepared to handle in the following.  */
	switch (OMP_CLAUSE_MAP_KIND (c))
	  {
	  case GOMP_MAP_ALLOC:
	  case GOMP_MAP_TO:
	  case GOMP_MAP_FROM:
	  case GOMP_MAP_TOFROM:
	  case GOMP_MAP_POINTER:
	  case GOMP_MAP_TO_PSET:
	  case GOMP_MAP_DELETE:
	  case GOMP_MAP_RELEASE:
	  case GOMP_MAP_ALWAYS_TO:
	  case GOMP_MAP_ALWAYS_FROM:
	  case GOMP_MAP_ALWAYS_TOFROM:
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  case GOMP_MAP_STRUCT:
	  case GOMP_MAP_ALWAYS_POINTER:
	    break;
	  case GOMP_MAP_FORCE_ALLOC:
	  case GOMP_MAP_FORCE_TO:
	  case GOMP_MAP_FORCE_FROM:
	  case GOMP_MAP_FORCE_TOFROM:
	  case GOMP_MAP_FORCE_PRESENT:
	  case GOMP_MAP_FORCE_DEVICEPTR:
	  case GOMP_MAP_DEVICE_RESIDENT:
	  case GOMP_MAP_LINK:
	    gcc_assert (is_gimple_omp_oacc (stmt));
	    break;
	  default:
	    gcc_unreachable ();
	  }
#endif
	  /* FALLTHRU */
      case OMP_CLAUSE_TO:
      case OMP_CLAUSE_FROM:
      oacc_firstprivate:
	var = OMP_CLAUSE_DECL (c);
	if (!DECL_P (var))
	  {
	    if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
		|| (!OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		    && (OMP_CLAUSE_MAP_KIND (c)
			!= GOMP_MAP_FIRSTPRIVATE_POINTER)))
	      map_cnt++;
	    continue;
	  }

	if (DECL_SIZE (var)
	    && TREE_CODE (DECL_SIZE (var)) != INTEGER_CST)
	  {
	    tree var2 = DECL_VALUE_EXPR (var);
	    gcc_assert (TREE_CODE (var2) == INDIRECT_REF);
	    var2 = TREE_OPERAND (var2, 0);
	    gcc_assert (DECL_P (var2));
	    var = var2;
	  }

	if (offloaded
	    && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	    && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE))
	  {
	    if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	      {
		if (is_global_var (maybe_lookup_decl_in_outer_ctx (var, ctx))
		    && varpool_node::get_create (var)->offloadable)
		  continue;

		tree type = build_pointer_type (TREE_TYPE (var));
		tree new_var = lookup_decl (var, ctx);
		x = create_tmp_var_raw (type, get_name (new_var));
		gimple_add_tmp_var (x);
		x = build_simple_mem_ref (x);
		SET_DECL_VALUE_EXPR (new_var, x);
		DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	      }
	    continue;
	  }

	if (!maybe_lookup_field (var, ctx))
	  continue;

	/* Don't remap oacc parallel reduction variables, because the
	   intermediate result must be local to each gang.  */
	if (offloaded && !(OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
			   && OMP_CLAUSE_MAP_IN_REDUCTION (c)))
	  {
	    x = build_receiver_ref (var, true, ctx);
	    tree new_var = lookup_decl (var, ctx);

	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		&& OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		&& !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		&& TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	      x = build_simple_mem_ref (x);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	      {
		gcc_assert (is_gimple_omp_oacc (ctx->stmt));
		if (is_reference (new_var))
		  {
		    /* Create a local object to hold the instance
		       value.  */
		    tree type = TREE_TYPE (TREE_TYPE (new_var));
		    const char *id = IDENTIFIER_POINTER (DECL_NAME (new_var));
		    tree inst = create_tmp_var (type, id);
		    gimplify_assign (inst, fold_indirect_ref (x), &fplist);
		    x = build_fold_addr_expr (inst);
		  }
		gimplify_assign (new_var, x, &fplist);
	      }
	    else if (DECL_P (new_var))
	      {
		SET_DECL_VALUE_EXPR (new_var, x);
		DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	      }
	    else
	      gcc_unreachable ();
	  }
	map_cnt++;
	break;

      case OMP_CLAUSE_FIRSTPRIVATE:
	if (is_oacc_parallel (ctx))
	  goto oacc_firstprivate;
	map_cnt++;
	var = OMP_CLAUSE_DECL (c);
	if (!is_reference (var)
	    && !is_gimple_reg_type (TREE_TYPE (var)))
	  {
	    tree new_var = lookup_decl (var, ctx);
	    if (is_variable_sized (var))
	      {
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_pvar = lookup_decl (pvar, ctx);
		x = build_fold_indirect_ref (new_pvar);
		TREE_THIS_NOTRAP (x) = 1;
	      }
	    else
	      x = build_receiver_ref (var, true, ctx);
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	break;

      case OMP_CLAUSE_PRIVATE:
	if (is_gimple_omp_oacc (ctx->stmt))
	  break;
	var = OMP_CLAUSE_DECL (c);
	if (is_variable_sized (var))
	  {
	    tree new_var = lookup_decl (var, ctx);
	    tree pvar = DECL_VALUE_EXPR (var);
	    gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
	    pvar = TREE_OPERAND (pvar, 0);
	    gcc_assert (DECL_P (pvar));
	    tree new_pvar = lookup_decl (pvar, ctx);
	    x = build_fold_indirect_ref (new_pvar);
	    TREE_THIS_NOTRAP (x) = 1;
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	break;

      case OMP_CLAUSE_USE_DEVICE_PTR:
      case OMP_CLAUSE_IS_DEVICE_PTR:
	var = OMP_CLAUSE_DECL (c);
	map_cnt++;
	if (is_variable_sized (var))
	  {
	    tree new_var = lookup_decl (var, ctx);
	    tree pvar = DECL_VALUE_EXPR (var);
	    gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
	    pvar = TREE_OPERAND (pvar, 0);
	    gcc_assert (DECL_P (pvar));
	    tree new_pvar = lookup_decl (pvar, ctx);
	    x = build_fold_indirect_ref (new_pvar);
	    TREE_THIS_NOTRAP (x) = 1;
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	else if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	  {
	    tree new_var = lookup_decl (var, ctx);
	    tree type = build_pointer_type (TREE_TYPE (var));
	    x = create_tmp_var_raw (type, get_name (new_var));
	    gimple_add_tmp_var (x);
	    x = build_simple_mem_ref (x);
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	else
	  {
	    tree new_var = lookup_decl (var, ctx);
	    x = create_tmp_var_raw (TREE_TYPE (new_var), get_name (new_var));
	    gimple_add_tmp_var (x);
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	break;
      }

  if (offloaded)
    {
      target_nesting_level++;
      lower_omp (&tgt_body, ctx);
      target_nesting_level--;
    }
  else if (data_region)
    lower_omp (&tgt_body, ctx);

  if (offloaded)
    {
      /* Declare all the variables created by mapping and the variables
	 declared in the scope of the target body.  */
      record_vars_into (ctx->block_vars, child_fn);
      record_vars_into (gimple_bind_vars (tgt_bind), child_fn);
    }

  olist = NULL;
  ilist = NULL;
  if (ctx->record_type)
    {
      ctx->sender_decl
	= create_tmp_var (ctx->record_type, ".omp_data_arr");
      DECL_NAMELESS (ctx->sender_decl) = 1;
      TREE_ADDRESSABLE (ctx->sender_decl) = 1;
      t = make_tree_vec (3);
      TREE_VEC_ELT (t, 0) = ctx->sender_decl;
      TREE_VEC_ELT (t, 1)
	= create_tmp_var (build_array_type_nelts (size_type_node, map_cnt),
			  ".omp_data_sizes");
      DECL_NAMELESS (TREE_VEC_ELT (t, 1)) = 1;
      TREE_ADDRESSABLE (TREE_VEC_ELT (t, 1)) = 1;
      TREE_STATIC (TREE_VEC_ELT (t, 1)) = 1;
      tree tkind_type = short_unsigned_type_node;
      int talign_shift = 8;
      TREE_VEC_ELT (t, 2)
	= create_tmp_var (build_array_type_nelts (tkind_type, map_cnt),
			  ".omp_data_kinds");
      DECL_NAMELESS (TREE_VEC_ELT (t, 2)) = 1;
      TREE_ADDRESSABLE (TREE_VEC_ELT (t, 2)) = 1;
      TREE_STATIC (TREE_VEC_ELT (t, 2)) = 1;
      gimple_omp_target_set_data_arg (stmt, t);

      vec<constructor_elt, va_gc> *vsize;
      vec<constructor_elt, va_gc> *vkind;
      vec_alloc (vsize, map_cnt);
      vec_alloc (vkind, map_cnt);
      unsigned int map_idx = 0;

      for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	    tree ovar, nc, s, purpose, var, x, type;
	    unsigned int talign;

	  default:
	    break;

	  case OMP_CLAUSE_MAP:
	  case OMP_CLAUSE_TO:
	  case OMP_CLAUSE_FROM:
	  oacc_firstprivate_map:
	    nc = c;
	    ovar = OMP_CLAUSE_DECL (c);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		&& (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		    || (OMP_CLAUSE_MAP_KIND (c)
			== GOMP_MAP_FIRSTPRIVATE_REFERENCE)))
	      break;
	    if (!DECL_P (ovar))
	      {
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		    && OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c))
		  {
		    gcc_checking_assert (OMP_CLAUSE_DECL (OMP_CLAUSE_CHAIN (c))
					 == get_base_address (ovar));
		    nc = OMP_CLAUSE_CHAIN (c);
		    ovar = OMP_CLAUSE_DECL (nc);
		  }
		else
		  {
		    tree x = build_sender_ref (ovar, ctx);
		    tree v
		      = build_fold_addr_expr_with_type (ovar, ptr_type_node);
		    gimplify_assign (x, v, &ilist);
		    nc = NULL_TREE;
		  }
	      }
	    else
	      {
		if (DECL_SIZE (ovar)
		    && TREE_CODE (DECL_SIZE (ovar)) != INTEGER_CST)
		  {
		    tree ovar2 = DECL_VALUE_EXPR (ovar);
		    gcc_assert (TREE_CODE (ovar2) == INDIRECT_REF);
		    ovar2 = TREE_OPERAND (ovar2, 0);
		    gcc_assert (DECL_P (ovar2));
		    ovar = ovar2;
		  }
		if (!maybe_lookup_field (ovar, ctx))
		  continue;
	      }

	    talign = TYPE_ALIGN_UNIT (TREE_TYPE (ovar));
	    if (DECL_P (ovar) && DECL_ALIGN_UNIT (ovar) > talign)
	      talign = DECL_ALIGN_UNIT (ovar);
	    if (nc)
	      {
		var = lookup_decl_in_outer_ctx (ovar, ctx);
		x = build_sender_ref (ovar, ctx);

		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		    && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		    && !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		    && TREE_CODE (TREE_TYPE (ovar)) == ARRAY_TYPE)
		  {
		    gcc_assert (offloaded);
		    tree avar
		      = create_tmp_var (TREE_TYPE (TREE_TYPE (x)));
		    mark_addressable (avar);
		    gimplify_assign (avar, build_fold_addr_expr (var), &ilist);
		    talign = DECL_ALIGN_UNIT (avar);
		    avar = build_fold_addr_expr (avar);
		    gimplify_assign (x, avar, &ilist);
		  }
		else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
		  {
		    gcc_assert (is_gimple_omp_oacc (ctx->stmt));
		    if (!is_reference (var))
		      {
			if (is_gimple_reg (var)
			    && OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
			  TREE_NO_WARNING (var) = 1;
			var = build_fold_addr_expr (var);
		      }
		    else
		      talign = TYPE_ALIGN_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
		    gimplify_assign (x, var, &ilist);
		  }
		else if (is_gimple_reg (var))
		  {
		    gcc_assert (offloaded);
		    tree avar = create_tmp_var (TREE_TYPE (var));
		    mark_addressable (avar);
		    enum gomp_map_kind map_kind = OMP_CLAUSE_MAP_KIND (c);
		    if (GOMP_MAP_COPY_TO_P (map_kind)
			|| map_kind == GOMP_MAP_POINTER
			|| map_kind == GOMP_MAP_TO_PSET
			|| map_kind == GOMP_MAP_FORCE_DEVICEPTR)
		      {
			/* If we need to initialize a temporary
			   with VAR because it is not addressable, and
			   the variable hasn't been initialized yet, then
			   we'll get a warning for the store to avar.
			   Don't warn in that case, the mapping might
			   be implicit.  */
			TREE_NO_WARNING (var) = 1;
			gimplify_assign (avar, var, &ilist);
		      }
		    avar = build_fold_addr_expr (avar);
		    gimplify_assign (x, avar, &ilist);
		    if ((GOMP_MAP_COPY_FROM_P (map_kind)
			 || map_kind == GOMP_MAP_FORCE_DEVICEPTR)
			&& !TYPE_READONLY (TREE_TYPE (var)))
		      {
			x = unshare_expr (x);
			x = build_simple_mem_ref (x);
			gimplify_assign (var, x, &olist);
		      }
		  }
		else
		  {
		    var = build_fold_addr_expr (var);
		    gimplify_assign (x, var, &ilist);
		  }
	      }
	    s = NULL_TREE;
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	      {
		gcc_checking_assert (is_gimple_omp_oacc (ctx->stmt));
		s = TREE_TYPE (ovar);
		if (TREE_CODE (s) == REFERENCE_TYPE)
		  s = TREE_TYPE (s);
		s = TYPE_SIZE_UNIT (s);
	      }
	    else
	      s = OMP_CLAUSE_SIZE (c);
	    if (s == NULL_TREE)
	      s = TYPE_SIZE_UNIT (TREE_TYPE (ovar));
	    s = fold_convert (size_type_node, s);
	    purpose = size_int (map_idx++);
	    CONSTRUCTOR_APPEND_ELT (vsize, purpose, s);
	    if (TREE_CODE (s) != INTEGER_CST)
	      TREE_STATIC (TREE_VEC_ELT (t, 1)) = 0;

	    unsigned HOST_WIDE_INT tkind, tkind_zero;
	    switch (OMP_CLAUSE_CODE (c))
	      {
	      case OMP_CLAUSE_MAP:
		tkind = OMP_CLAUSE_MAP_KIND (c);
		tkind_zero = tkind;
		if (OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c))
		  switch (tkind)
		    {
		    case GOMP_MAP_ALLOC:
		    case GOMP_MAP_TO:
		    case GOMP_MAP_FROM:
		    case GOMP_MAP_TOFROM:
		    case GOMP_MAP_ALWAYS_TO:
		    case GOMP_MAP_ALWAYS_FROM:
		    case GOMP_MAP_ALWAYS_TOFROM:
		    case GOMP_MAP_RELEASE:
		      tkind_zero = GOMP_MAP_ZERO_LEN_ARRAY_SECTION;
		      break;
		    case GOMP_MAP_DELETE:
		      tkind_zero = GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION;
		    default:
		      break;
		    }
		if (tkind_zero != tkind)
		  {
		    if (integer_zerop (s))
		      tkind = tkind_zero;
		    else if (integer_nonzerop (s))
		      tkind_zero = tkind;
		  }
		break;
	      case OMP_CLAUSE_FIRSTPRIVATE:
		gcc_checking_assert (is_gimple_omp_oacc (ctx->stmt));
		tkind = GOMP_MAP_TO;
		tkind_zero = tkind;
		break;
	      case OMP_CLAUSE_TO:
		tkind = GOMP_MAP_TO;
		tkind_zero = tkind;
		break;
	      case OMP_CLAUSE_FROM:
		tkind = GOMP_MAP_FROM;
		tkind_zero = tkind;
		break;
	      default:
		gcc_unreachable ();
	      }
	    gcc_checking_assert (tkind
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    gcc_checking_assert (tkind_zero
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    talign = ceil_log2 (talign);
	    tkind |= talign << talign_shift;
	    tkind_zero |= talign << talign_shift;
	    gcc_checking_assert (tkind
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    gcc_checking_assert (tkind_zero
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    if (tkind == tkind_zero)
	      x = build_int_cstu (tkind_type, tkind);
	    else
	      {
		TREE_STATIC (TREE_VEC_ELT (t, 2)) = 0;
		x = build3 (COND_EXPR, tkind_type,
			    fold_build2 (EQ_EXPR, boolean_type_node,
					 unshare_expr (s), size_zero_node),
			    build_int_cstu (tkind_type, tkind_zero),
			    build_int_cstu (tkind_type, tkind));
	      }
	    CONSTRUCTOR_APPEND_ELT (vkind, purpose, x);
	    if (nc && nc != c)
	      c = nc;
	    break;

	  case OMP_CLAUSE_FIRSTPRIVATE:
	    if (is_oacc_parallel (ctx))
	      goto oacc_firstprivate_map;
	    ovar = OMP_CLAUSE_DECL (c);
	    if (is_reference (ovar))
	      talign = TYPE_ALIGN_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
	    else
	      talign = DECL_ALIGN_UNIT (ovar);
	    var = lookup_decl_in_outer_ctx (ovar, ctx);
	    x = build_sender_ref (ovar, ctx);
	    tkind = GOMP_MAP_FIRSTPRIVATE;
	    type = TREE_TYPE (ovar);
	    if (is_reference (ovar))
	      type = TREE_TYPE (type);
	    if ((INTEGRAL_TYPE_P (type)
		 && TYPE_PRECISION (type) <= POINTER_SIZE)
		|| TREE_CODE (type) == POINTER_TYPE)
	      {
		tkind = GOMP_MAP_FIRSTPRIVATE_INT;
		tree t = var;
		if (is_reference (var))
		  t = build_simple_mem_ref (var);
		else if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
		  TREE_NO_WARNING (var) = 1;
		if (TREE_CODE (type) != POINTER_TYPE)
		  t = fold_convert (pointer_sized_int_node, t);
		t = fold_convert (TREE_TYPE (x), t);
		gimplify_assign (x, t, &ilist);
	      }
	    else if (is_reference (var))
	      gimplify_assign (x, var, &ilist);
	    else if (is_gimple_reg (var))
	      {
		tree avar = create_tmp_var (TREE_TYPE (var));
		mark_addressable (avar);
		if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
		  TREE_NO_WARNING (var) = 1;
		gimplify_assign (avar, var, &ilist);
		avar = build_fold_addr_expr (avar);
		gimplify_assign (x, avar, &ilist);
	      }
	    else
	      {
		var = build_fold_addr_expr (var);
		gimplify_assign (x, var, &ilist);
	      }
	    if (tkind == GOMP_MAP_FIRSTPRIVATE_INT)
	      s = size_int (0);
	    else if (is_reference (var))
	      s = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
	    else
	      s = TYPE_SIZE_UNIT (TREE_TYPE (ovar));
	    s = fold_convert (size_type_node, s);
	    purpose = size_int (map_idx++);
	    CONSTRUCTOR_APPEND_ELT (vsize, purpose, s);
	    if (TREE_CODE (s) != INTEGER_CST)
	      TREE_STATIC (TREE_VEC_ELT (t, 1)) = 0;

	    gcc_checking_assert (tkind
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    talign = ceil_log2 (talign);
	    tkind |= talign << talign_shift;
	    gcc_checking_assert (tkind
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    CONSTRUCTOR_APPEND_ELT (vkind, purpose,
				    build_int_cstu (tkind_type, tkind));
	    break;

	  case OMP_CLAUSE_USE_DEVICE_PTR:
	  case OMP_CLAUSE_IS_DEVICE_PTR:
	    ovar = OMP_CLAUSE_DECL (c);
	    var = lookup_decl_in_outer_ctx (ovar, ctx);
	    x = build_sender_ref (ovar, ctx);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR)
	      tkind = GOMP_MAP_USE_DEVICE_PTR;
	    else
	      tkind = GOMP_MAP_FIRSTPRIVATE_INT;
	    type = TREE_TYPE (ovar);
	    if (TREE_CODE (type) == ARRAY_TYPE)
	      var = build_fold_addr_expr (var);
	    else
	      {
		if (is_reference (ovar))
		  {
		    type = TREE_TYPE (type);
		    if (TREE_CODE (type) != ARRAY_TYPE)
		      var = build_simple_mem_ref (var);
		    var = fold_convert (TREE_TYPE (x), var);
		  }
	      }
	    gimplify_assign (x, var, &ilist);
	    s = size_int (0);
	    purpose = size_int (map_idx++);
	    CONSTRUCTOR_APPEND_ELT (vsize, purpose, s);
	    gcc_checking_assert (tkind
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    gcc_checking_assert (tkind
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    CONSTRUCTOR_APPEND_ELT (vkind, purpose,
				    build_int_cstu (tkind_type, tkind));
	    break;
	  }

      gcc_assert (map_idx == map_cnt);

      DECL_INITIAL (TREE_VEC_ELT (t, 1))
	= build_constructor (TREE_TYPE (TREE_VEC_ELT (t, 1)), vsize);
      DECL_INITIAL (TREE_VEC_ELT (t, 2))
	= build_constructor (TREE_TYPE (TREE_VEC_ELT (t, 2)), vkind);
      for (int i = 1; i <= 2; i++)
	if (!TREE_STATIC (TREE_VEC_ELT (t, i)))
	  {
	    gimple_seq initlist = NULL;
	    force_gimple_operand (build1 (DECL_EXPR, void_type_node,
					  TREE_VEC_ELT (t, i)),
				  &initlist, true, NULL_TREE);
	    gimple_seq_add_seq (&ilist, initlist);

	    tree clobber = build_constructor (TREE_TYPE (TREE_VEC_ELT (t, i)),
					      NULL);
	    TREE_THIS_VOLATILE (clobber) = 1;
	    gimple_seq_add_stmt (&olist,
				 gimple_build_assign (TREE_VEC_ELT (t, i),
						      clobber));
	  }

      tree clobber = build_constructor (ctx->record_type, NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gimple_seq_add_stmt (&olist, gimple_build_assign (ctx->sender_decl,
							clobber));
    }

  /* Once all the expansions are done, sequence all the different
     fragments inside gimple_omp_body.  */

  new_body = NULL;

  if (offloaded
      && ctx->record_type)
    {
      t = build_fold_addr_expr_loc (loc, ctx->sender_decl);
      /* fixup_child_record_type might have changed receiver_decl's type.  */
      t = fold_convert_loc (loc, TREE_TYPE (ctx->receiver_decl), t);
      gimple_seq_add_stmt (&new_body,
	  		   gimple_build_assign (ctx->receiver_decl, t));
    }
  gimple_seq_add_seq (&new_body, fplist);

  if (offloaded || data_region)
    {
      tree prev = NULL_TREE;
      for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	    tree var, x;
	  default:
	    break;
	  case OMP_CLAUSE_FIRSTPRIVATE:
	    if (is_gimple_omp_oacc (ctx->stmt))
	      break;
	    var = OMP_CLAUSE_DECL (c);
	    if (is_reference (var)
		|| is_gimple_reg_type (TREE_TYPE (var)))
	      {
		tree new_var = lookup_decl (var, ctx);
		tree type;
		type = TREE_TYPE (var);
		if (is_reference (var))
		  type = TREE_TYPE (type);
		if ((INTEGRAL_TYPE_P (type)
		     && TYPE_PRECISION (type) <= POINTER_SIZE)
		    || TREE_CODE (type) == POINTER_TYPE)
		  {
		    x = build_receiver_ref (var, false, ctx);
		    if (TREE_CODE (type) != POINTER_TYPE)
		      x = fold_convert (pointer_sized_int_node, x);
		    x = fold_convert (type, x);
		    gimplify_expr (&x, &new_body, NULL, is_gimple_val,
				   fb_rvalue);
		    if (is_reference (var))
		      {
			tree v = create_tmp_var_raw (type, get_name (var));
			gimple_add_tmp_var (v);
			TREE_ADDRESSABLE (v) = 1;
			gimple_seq_add_stmt (&new_body,
					     gimple_build_assign (v, x));
			x = build_fold_addr_expr (v);
		      }
		    gimple_seq_add_stmt (&new_body,
					 gimple_build_assign (new_var, x));
		  }
		else
		  {
		    x = build_receiver_ref (var, !is_reference (var), ctx);
		    gimplify_expr (&x, &new_body, NULL, is_gimple_val,
				   fb_rvalue);
		    gimple_seq_add_stmt (&new_body,
					 gimple_build_assign (new_var, x));
		  }
	      }
	    else if (is_variable_sized (var))
	      {
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_var = lookup_decl (pvar, ctx);
		x = build_receiver_ref (var, false, ctx);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  case OMP_CLAUSE_PRIVATE:
	    if (is_gimple_omp_oacc (ctx->stmt))
	      break;
	    var = OMP_CLAUSE_DECL (c);
	    if (is_reference (var))
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		tree new_var = lookup_decl (var, ctx);
		x = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_var)));
		if (TREE_CONSTANT (x))
		  {
		    x = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (new_var)),
					    get_name (var));
		    gimple_add_tmp_var (x);
		    TREE_ADDRESSABLE (x) = 1;
		    x = build_fold_addr_expr_loc (clause_loc, x);
		  }
		else
		  break;

		x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  case OMP_CLAUSE_USE_DEVICE_PTR:
	  case OMP_CLAUSE_IS_DEVICE_PTR:
	    var = OMP_CLAUSE_DECL (c);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR)
	      x = build_sender_ref (var, ctx);
	    else
	      x = build_receiver_ref (var, false, ctx);
	    if (is_variable_sized (var))
	      {
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_var = lookup_decl (pvar, ctx);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    else if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	      {
		tree new_var = lookup_decl (var, ctx);
		new_var = DECL_VALUE_EXPR (new_var);
		gcc_assert (TREE_CODE (new_var) == MEM_REF);
		new_var = TREE_OPERAND (new_var, 0);
		gcc_assert (DECL_P (new_var));
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    else
	      {
		tree type = TREE_TYPE (var);
		tree new_var = lookup_decl (var, ctx);
		if (is_reference (var))
		  {
		    type = TREE_TYPE (type);
		    if (TREE_CODE (type) != ARRAY_TYPE)
		      {
			tree v = create_tmp_var_raw (type, get_name (var));
			gimple_add_tmp_var (v);
			TREE_ADDRESSABLE (v) = 1;
			x = fold_convert (type, x);
			gimplify_expr (&x, &new_body, NULL, is_gimple_val,
				       fb_rvalue);
			gimple_seq_add_stmt (&new_body,
					     gimple_build_assign (v, x));
			x = build_fold_addr_expr (v);
		      }
		  }
		new_var = DECL_VALUE_EXPR (new_var);
		x = fold_convert (TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  }
      /* Handle GOMP_MAP_FIRSTPRIVATE_{POINTER,REFERENCE} in second pass,
	 so that firstprivate vars holding OMP_CLAUSE_SIZE if needed
	 are already handled.  Similarly OMP_CLAUSE_PRIVATE for VLAs
	 or references to VLAs.  */
      for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	    tree var;
	  default:
	    break;
	  case OMP_CLAUSE_MAP:
	    if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		HOST_WIDE_INT offset = 0;
		gcc_assert (prev);
		var = OMP_CLAUSE_DECL (c);
		if (DECL_P (var)
		    && TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE
		    && is_global_var (maybe_lookup_decl_in_outer_ctx (var,
								      ctx))
		    && varpool_node::get_create (var)->offloadable)
		  break;
		if (TREE_CODE (var) == INDIRECT_REF
		    && TREE_CODE (TREE_OPERAND (var, 0)) == COMPONENT_REF)
		  var = TREE_OPERAND (var, 0);
		if (TREE_CODE (var) == COMPONENT_REF)
		  {
		    var = get_addr_base_and_unit_offset (var, &offset);
		    gcc_assert (var != NULL_TREE && DECL_P (var));
		  }
		else if (DECL_SIZE (var)
			 && TREE_CODE (DECL_SIZE (var)) != INTEGER_CST)
		  {
		    tree var2 = DECL_VALUE_EXPR (var);
		    gcc_assert (TREE_CODE (var2) == INDIRECT_REF);
		    var2 = TREE_OPERAND (var2, 0);
		    gcc_assert (DECL_P (var2));
		    var = var2;
		  }
		tree new_var = lookup_decl (var, ctx), x;
		tree type = TREE_TYPE (new_var);
		bool is_ref;
		if (TREE_CODE (OMP_CLAUSE_DECL (c)) == INDIRECT_REF
		    && (TREE_CODE (TREE_OPERAND (OMP_CLAUSE_DECL (c), 0))
			== COMPONENT_REF))
		  {
		    type = TREE_TYPE (TREE_OPERAND (OMP_CLAUSE_DECL (c), 0));
		    is_ref = true;
		    new_var = build2 (MEM_REF, type,
				      build_fold_addr_expr (new_var),
				      build_int_cst (build_pointer_type (type),
						     offset));
		  }
		else if (TREE_CODE (OMP_CLAUSE_DECL (c)) == COMPONENT_REF)
		  {
		    type = TREE_TYPE (OMP_CLAUSE_DECL (c));
		    is_ref = TREE_CODE (type) == REFERENCE_TYPE;
		    new_var = build2 (MEM_REF, type,
				      build_fold_addr_expr (new_var),
				      build_int_cst (build_pointer_type (type),
						     offset));
		  }
		else
		  is_ref = is_reference (var);
		if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
		  is_ref = false;
		bool ref_to_array = false;
		if (is_ref)
		  {
		    type = TREE_TYPE (type);
		    if (TREE_CODE (type) == ARRAY_TYPE)
		      {
			type = build_pointer_type (type);
			ref_to_array = true;
		      }
		  }
		else if (TREE_CODE (type) == ARRAY_TYPE)
		  {
		    tree decl2 = DECL_VALUE_EXPR (new_var);
		    gcc_assert (TREE_CODE (decl2) == MEM_REF);
		    decl2 = TREE_OPERAND (decl2, 0);
		    gcc_assert (DECL_P (decl2));
		    new_var = decl2;
		    type = TREE_TYPE (new_var);
		  }
		x = build_receiver_ref (OMP_CLAUSE_DECL (prev), false, ctx);
		x = fold_convert_loc (clause_loc, type, x);
		if (!integer_zerop (OMP_CLAUSE_SIZE (c)))
		  {
		    tree bias = OMP_CLAUSE_SIZE (c);
		    if (DECL_P (bias))
		      bias = lookup_decl (bias, ctx);
		    bias = fold_convert_loc (clause_loc, sizetype, bias);
		    bias = fold_build1_loc (clause_loc, NEGATE_EXPR, sizetype,
					    bias);
		    x = fold_build2_loc (clause_loc, POINTER_PLUS_EXPR,
					 TREE_TYPE (x), x, bias);
		  }
		if (ref_to_array)
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		if (is_ref && !ref_to_array)
		  {
		    tree t = create_tmp_var_raw (type, get_name (var));
		    gimple_add_tmp_var (t);
		    TREE_ADDRESSABLE (t) = 1;
		    gimple_seq_add_stmt (&new_body,
					 gimple_build_assign (t, x));
		    x = build_fold_addr_expr_loc (clause_loc, t);
		  }
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
		prev = NULL_TREE;
	      }
	    else if (OMP_CLAUSE_CHAIN (c)
		     && OMP_CLAUSE_CODE (OMP_CLAUSE_CHAIN (c))
			== OMP_CLAUSE_MAP
		     && (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
			 == GOMP_MAP_FIRSTPRIVATE_POINTER
			 || (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
			     == GOMP_MAP_FIRSTPRIVATE_REFERENCE)))
	      prev = c;
	    break;
	  case OMP_CLAUSE_PRIVATE:
	    var = OMP_CLAUSE_DECL (c);
	    if (is_variable_sized (var))
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		tree new_var = lookup_decl (var, ctx);
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_pvar = lookup_decl (pvar, ctx);
		tree atmp = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		tree al = size_int (DECL_ALIGN (var));
		tree x = TYPE_SIZE_UNIT (TREE_TYPE (new_var));
		x = build_call_expr_loc (clause_loc, atmp, 2, x, al);
		x = fold_convert_loc (clause_loc, TREE_TYPE (new_pvar), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_pvar, x));
	      }
	    else if (is_reference (var) && !is_gimple_omp_oacc (ctx->stmt))
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		tree new_var = lookup_decl (var, ctx);
		tree x = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_var)));
		if (TREE_CONSTANT (x))
		  break;
		else
		  {
		    tree atmp
		      = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		    tree rtype = TREE_TYPE (TREE_TYPE (new_var));
		    tree al = size_int (TYPE_ALIGN (rtype));
		    x = build_call_expr_loc (clause_loc, atmp, 2, x, al);
		  }

		x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  }

      gimple_seq fork_seq = NULL;
      gimple_seq join_seq = NULL;

      if (is_oacc_parallel (ctx))
	{
	  /* If there are reductions on the offloaded region itself, treat
	     them as a dummy GANG loop.  */
	  tree level = build_int_cst (integer_type_node, GOMP_DIM_GANG);

	  lower_oacc_reductions (gimple_location (ctx->stmt), clauses, level,
				 false, NULL, NULL, &fork_seq, &join_seq, ctx);
	}

      gimple_seq_add_seq (&new_body, fork_seq);
      gimple_seq_add_seq (&new_body, tgt_body);
      gimple_seq_add_seq (&new_body, join_seq);

      if (offloaded)
	new_body = maybe_catch_exception (new_body);

      gimple_seq_add_stmt (&new_body, gimple_build_omp_return (false));
      gimple_omp_set_body (stmt, new_body);
    }

  bind = gimple_build_bind (NULL, NULL,
			    tgt_bind ? gimple_bind_block (tgt_bind)
				     : NULL_TREE);
  gsi_replace (gsi_p, dep_bind ? dep_bind : bind, true);
  gimple_bind_add_seq (bind, ilist);
  gimple_bind_add_stmt (bind, stmt);
  gimple_bind_add_seq (bind, olist);

  pop_gimplify_context (NULL);

  if (dep_bind)
    {
      gimple_bind_add_seq (dep_bind, dep_ilist);
      gimple_bind_add_stmt (dep_bind, bind);
      gimple_bind_add_seq (dep_bind, dep_olist);
      pop_gimplify_context (dep_bind);
    }
}

/* Expand code for an OpenMP teams directive.  */

static void
lower_omp_teams (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gomp_teams *teams_stmt = as_a <gomp_teams *> (gsi_stmt (*gsi_p));
  push_gimplify_context ();

  tree block = make_node (BLOCK);
  gbind *bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_seq bind_body = NULL;
  gimple_seq dlist = NULL;
  gimple_seq olist = NULL;

  tree num_teams = find_omp_clause (gimple_omp_teams_clauses (teams_stmt),
				    OMP_CLAUSE_NUM_TEAMS);
  if (num_teams == NULL_TREE)
    num_teams = build_int_cst (unsigned_type_node, 0);
  else
    {
      num_teams = OMP_CLAUSE_NUM_TEAMS_EXPR (num_teams);
      num_teams = fold_convert (unsigned_type_node, num_teams);
      gimplify_expr (&num_teams, &bind_body, NULL, is_gimple_val, fb_rvalue);
    }
  tree thread_limit = find_omp_clause (gimple_omp_teams_clauses (teams_stmt),
				       OMP_CLAUSE_THREAD_LIMIT);
  if (thread_limit == NULL_TREE)
    thread_limit = build_int_cst (unsigned_type_node, 0);
  else
    {
      thread_limit = OMP_CLAUSE_THREAD_LIMIT_EXPR (thread_limit);
      thread_limit = fold_convert (unsigned_type_node, thread_limit);
      gimplify_expr (&thread_limit, &bind_body, NULL, is_gimple_val,
		     fb_rvalue);
    }

  lower_rec_input_clauses (gimple_omp_teams_clauses (teams_stmt),
			   &bind_body, &dlist, ctx, NULL);
  lower_omp (gimple_omp_body_ptr (teams_stmt), ctx);
  lower_reduction_clauses (gimple_omp_teams_clauses (teams_stmt), &olist, ctx);
  if (!gimple_omp_teams_grid_phony (teams_stmt))
    {
      gimple_seq_add_stmt (&bind_body, teams_stmt);
      location_t loc = gimple_location (teams_stmt);
      tree decl = builtin_decl_explicit (BUILT_IN_GOMP_TEAMS);
      gimple *call = gimple_build_call (decl, 2, num_teams, thread_limit);
      gimple_set_location (call, loc);
      gimple_seq_add_stmt (&bind_body, call);
    }

  gimple_seq_add_seq (&bind_body, gimple_omp_body (teams_stmt));
  gimple_omp_set_body (teams_stmt, NULL);
  gimple_seq_add_seq (&bind_body, olist);
  gimple_seq_add_seq (&bind_body, dlist);
  if (!gimple_omp_teams_grid_phony (teams_stmt))
    gimple_seq_add_stmt (&bind_body, gimple_build_omp_return (true));
  gimple_bind_set_body (bind, bind_body);

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;
}

/* Expand code within an artificial GIMPLE_OMP_GRID_BODY OMP construct.  */

static void
lower_omp_grid_body (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_seq_add_stmt (gimple_omp_body_ptr (stmt),
		       gimple_build_omp_return (false));
}


/* Callback for lower_omp_1.  Return non-NULL if *tp needs to be
   regimplified.  If DATA is non-NULL, lower_omp_1 is outside
   of OMP context, but with task_shared_vars set.  */

static tree
lower_omp_regimplify_p (tree *tp, int *walk_subtrees,
    			void *data)
{
  tree t = *tp;

  /* Any variable with DECL_VALUE_EXPR needs to be regimplified.  */
  if (TREE_CODE (t) == VAR_DECL && data == NULL && DECL_HAS_VALUE_EXPR_P (t))
    return t;

  if (task_shared_vars
      && DECL_P (t)
      && bitmap_bit_p (task_shared_vars, DECL_UID (t)))
    return t;

  /* If a global variable has been privatized, TREE_CONSTANT on
     ADDR_EXPR might be wrong.  */
  if (data == NULL && TREE_CODE (t) == ADDR_EXPR)
    recompute_tree_invariant_for_addr_expr (t);

  *walk_subtrees = !IS_TYPE_OR_DECL_P (t);
  return NULL_TREE;
}

/* Data to be communicated between lower_omp_regimplify_operands and
   lower_omp_regimplify_operands_p.  */

struct lower_omp_regimplify_operands_data
{
  omp_context *ctx;
  vec<tree> *decls;
};

/* Helper function for lower_omp_regimplify_operands.  Find
   omp_member_access_dummy_var vars and adjust temporarily their
   DECL_VALUE_EXPRs if needed.  */

static tree
lower_omp_regimplify_operands_p (tree *tp, int *walk_subtrees,
				 void *data)
{
  tree t = omp_member_access_dummy_var (*tp);
  if (t)
    {
      struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
      lower_omp_regimplify_operands_data *ldata
	= (lower_omp_regimplify_operands_data *) wi->info;
      tree o = maybe_lookup_decl (t, ldata->ctx);
      if (o != t)
	{
	  ldata->decls->safe_push (DECL_VALUE_EXPR (*tp));
	  ldata->decls->safe_push (*tp);
	  tree v = unshare_and_remap (DECL_VALUE_EXPR (*tp), t, o);
	  SET_DECL_VALUE_EXPR (*tp, v);
	}
    }
  *walk_subtrees = !IS_TYPE_OR_DECL_P (*tp);
  return NULL_TREE;
}

/* Wrapper around gimple_regimplify_operands that adjusts DECL_VALUE_EXPRs
   of omp_member_access_dummy_var vars during regimplification.  */

static void
lower_omp_regimplify_operands (omp_context *ctx, gimple *stmt,
			       gimple_stmt_iterator *gsi_p)
{
  auto_vec<tree, 10> decls;
  if (ctx)
    {
      struct walk_stmt_info wi;
      memset (&wi, '\0', sizeof (wi));
      struct lower_omp_regimplify_operands_data data;
      data.ctx = ctx;
      data.decls = &decls;
      wi.info = &data;
      walk_gimple_op (stmt, lower_omp_regimplify_operands_p, &wi);
    }
  gimple_regimplify_operands (stmt, gsi_p);
  while (!decls.is_empty ())
    {
      tree t = decls.pop ();
      tree v = decls.pop ();
      SET_DECL_VALUE_EXPR (t, v);
    }
}

static void
lower_omp_1 (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  struct walk_stmt_info wi;
  gcall *call_stmt;

  if (gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  if (task_shared_vars)
    memset (&wi, '\0', sizeof (wi));

  /* If we have issued syntax errors, avoid doing any heavy lifting.
     Just replace the OMP directives with a NOP to avoid
     confusing RTL expansion.  */
  if (seen_error () && is_gimple_omp (stmt))
    {
      gsi_replace (gsi_p, gimple_build_nop (), true);
      return;
    }

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      {
	gcond *cond_stmt = as_a <gcond *> (stmt);
	if ((ctx || task_shared_vars)
	    && (walk_tree (gimple_cond_lhs_ptr (cond_stmt),
			   lower_omp_regimplify_p,
			   ctx ? NULL : &wi, NULL)
		|| walk_tree (gimple_cond_rhs_ptr (cond_stmt),
			      lower_omp_regimplify_p,
			      ctx ? NULL : &wi, NULL)))
	  lower_omp_regimplify_operands (ctx, cond_stmt, gsi_p);
      }
      break;
    case GIMPLE_CATCH:
      lower_omp (gimple_catch_handler_ptr (as_a <gcatch *> (stmt)), ctx);
      break;
    case GIMPLE_EH_FILTER:
      lower_omp (gimple_eh_filter_failure_ptr (stmt), ctx);
      break;
    case GIMPLE_TRY:
      lower_omp (gimple_try_eval_ptr (stmt), ctx);
      lower_omp (gimple_try_cleanup_ptr (stmt), ctx);
      break;
    case GIMPLE_TRANSACTION:
      lower_omp (gimple_transaction_body_ptr (
                   as_a <gtransaction *> (stmt)),
		 ctx);
      break;
    case GIMPLE_BIND:
      lower_omp (gimple_bind_body_ptr (as_a <gbind *> (stmt)), ctx);
      break;
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      if (ctx->cancellable)
	ctx->cancel_label = create_artificial_label (UNKNOWN_LOCATION);
      lower_omp_taskreg (gsi_p, ctx);
      break;
    case GIMPLE_OMP_FOR:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      if (ctx->cancellable)
	ctx->cancel_label = create_artificial_label (UNKNOWN_LOCATION);
      lower_omp_for (gsi_p, ctx);
      break;
    case GIMPLE_OMP_SECTIONS:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      if (ctx->cancellable)
	ctx->cancel_label = create_artificial_label (UNKNOWN_LOCATION);
      lower_omp_sections (gsi_p, ctx);
      break;
    case GIMPLE_OMP_SINGLE:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_single (gsi_p, ctx);
      break;
    case GIMPLE_OMP_MASTER:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_master (gsi_p, ctx);
      break;
    case GIMPLE_OMP_TASKGROUP:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_taskgroup (gsi_p, ctx);
      break;
    case GIMPLE_OMP_ORDERED:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_ordered (gsi_p, ctx);
      break;
    case GIMPLE_OMP_CRITICAL:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_critical (gsi_p, ctx);
      break;
    case GIMPLE_OMP_ATOMIC_LOAD:
      if ((ctx || task_shared_vars)
	  && walk_tree (gimple_omp_atomic_load_rhs_ptr (
			  as_a <gomp_atomic_load *> (stmt)),
			lower_omp_regimplify_p, ctx ? NULL : &wi, NULL))
	lower_omp_regimplify_operands (ctx, stmt, gsi_p);
      break;
    case GIMPLE_OMP_TARGET:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_target (gsi_p, ctx);
      break;
    case GIMPLE_OMP_TEAMS:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_teams (gsi_p, ctx);
      break;
    case GIMPLE_OMP_GRID_BODY:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_grid_body (gsi_p, ctx);
      break;
    case GIMPLE_CALL:
      tree fndecl;
      call_stmt = as_a <gcall *> (stmt);
      fndecl = gimple_call_fndecl (call_stmt);
      if (fndecl
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (fndecl))
	  {
	  case BUILT_IN_GOMP_BARRIER:
	    if (ctx == NULL)
	      break;
	    /* FALLTHRU */
	  case BUILT_IN_GOMP_CANCEL:
	  case BUILT_IN_GOMP_CANCELLATION_POINT:
	    omp_context *cctx;
	    cctx = ctx;
	    if (gimple_code (cctx->stmt) == GIMPLE_OMP_SECTION)
	      cctx = cctx->outer;
	    gcc_assert (gimple_call_lhs (call_stmt) == NULL_TREE);
	    if (!cctx->cancellable)
	      {
		if (DECL_FUNCTION_CODE (fndecl)
		    == BUILT_IN_GOMP_CANCELLATION_POINT)
		  {
		    stmt = gimple_build_nop ();
		    gsi_replace (gsi_p, stmt, false);
		  }
		break;
	      }
	    if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_GOMP_BARRIER)
	      {
		fndecl = builtin_decl_explicit (BUILT_IN_GOMP_BARRIER_CANCEL);
		gimple_call_set_fndecl (call_stmt, fndecl);
		gimple_call_set_fntype (call_stmt, TREE_TYPE (fndecl));
	      }
	    tree lhs;
	    lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (fndecl)));
	    gimple_call_set_lhs (call_stmt, lhs);
	    tree fallthru_label;
	    fallthru_label = create_artificial_label (UNKNOWN_LOCATION);
	    gimple *g;
	    g = gimple_build_label (fallthru_label);
	    gsi_insert_after (gsi_p, g, GSI_SAME_STMT);
	    g = gimple_build_cond (NE_EXPR, lhs,
				   fold_convert (TREE_TYPE (lhs),
						 boolean_false_node),
				   cctx->cancel_label, fallthru_label);
	    gsi_insert_after (gsi_p, g, GSI_SAME_STMT);
	    break;
	  default:
	    break;
	  }
      /* FALLTHRU */
    default:
      if ((ctx || task_shared_vars)
	  && walk_gimple_op (stmt, lower_omp_regimplify_p,
			     ctx ? NULL : &wi))
	{
	  /* Just remove clobbers, this should happen only if we have
	     "privatized" local addressable variables in SIMD regions,
	     the clobber isn't needed in that case and gimplifying address
	     of the ARRAY_REF into a pointer and creating MEM_REF based
	     clobber would create worse code than we get with the clobber
	     dropped.  */
	  if (gimple_clobber_p (stmt))
	    {
	      gsi_replace (gsi_p, gimple_build_nop (), true);
	      break;
	    }
	  lower_omp_regimplify_operands (ctx, stmt, gsi_p);
	}
      break;
    }
}

static void
lower_omp (gimple_seq *body, omp_context *ctx)
{
  location_t saved_location = input_location;
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (*body); !gsi_end_p (gsi); gsi_next (&gsi))
    lower_omp_1 (&gsi, ctx);
  /* During gimplification, we haven't folded statments inside offloading
     or taskreg regions (gimplify.c:maybe_fold_stmt); do that now.  */
  if (target_nesting_level || taskreg_nesting_level)
    for (gsi = gsi_start (*body); !gsi_end_p (gsi); gsi_next (&gsi))
      fold_stmt (&gsi);
  input_location = saved_location;
}

/* Returen true if STMT is an assignment of a register-type into a local
   VAR_DECL.  */

static bool
grid_reg_assignment_to_local_var_p (gimple *stmt)
{
  gassign *assign = dyn_cast <gassign *> (stmt);
  if (!assign)
    return false;
  tree lhs = gimple_assign_lhs (assign);
  if (TREE_CODE (lhs) != VAR_DECL
      || !is_gimple_reg_type (TREE_TYPE (lhs))
      || is_global_var (lhs))
    return false;
  return true;
}

/* Return true if all statements in SEQ are assignments to local register-type
   variables.  */

static bool
grid_seq_only_contains_local_assignments (gimple_seq seq)
{
  if (!seq)
    return true;

  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    if (!grid_reg_assignment_to_local_var_p (gsi_stmt (gsi)))
      return false;
  return true;
}

/* Scan statements in SEQ and call itself recursively on any bind.  If during
   whole search only assignments to register-type local variables and one
   single OMP statement is encountered, return true, otherwise return false.
   RET is where we store any OMP statement encountered.  TARGET_LOC and NAME
   are used for dumping a note about a failure.  */

static bool
grid_find_single_omp_among_assignments_1 (gimple_seq seq, location_t target_loc,
				     const char *name, gimple **ret)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (grid_reg_assignment_to_local_var_p (stmt))
	continue;
      if (gbind *bind = dyn_cast <gbind *> (stmt))
	{
	  if (!grid_find_single_omp_among_assignments_1 (gimple_bind_body (bind),
							 target_loc, name, ret))
	      return false;
	}
      else if (is_gimple_omp (stmt))
	{
	  if (*ret)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, target_loc,
				 "Will not turn target construct into a simple "
				 "GPGPU kernel because %s construct contains "
				 "multiple OpenMP constructs\n", name);
	      return false;
	    }
	  *ret = stmt;
	}
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, target_loc,
			     "Will not turn target construct into a simple "
			     "GPGPU kernel because %s construct contains "
			     "a complex statement\n", name);
	  return false;
	}
    }
  return true;
}

/* Scan statements in SEQ and make sure that it and any binds in it contain
   only assignments to local register-type variables and one OMP construct.  If
   so, return that construct, otherwise return NULL.  If dumping is enabled and
   function fails, use TARGET_LOC and NAME to dump a note with the reason for
   failure.  */

static gimple *
grid_find_single_omp_among_assignments (gimple_seq seq, location_t target_loc,
					const char *name)
{
  if (!seq)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, target_loc,
			 "Will not turn target construct into a simple "
			 "GPGPU kernel because %s construct has empty "
			 "body\n",
			 name);
      return NULL;
    }

  gimple *ret = NULL;
  if (grid_find_single_omp_among_assignments_1 (seq, target_loc, name, &ret))
    {
      if (!ret && dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, target_loc,
			 "Will not turn target construct into a simple "
			 "GPGPU kernel because %s construct does not contain"
			 "any other OpenMP construct\n", name);
      return ret;
    }
  else
    return NULL;
}

/* Walker function looking for statements there is no point gridifying (and for
   noreturn function calls which we cannot do).  Return non-NULL if such a
   function is found.  */

static tree
grid_find_ungridifiable_statement (gimple_stmt_iterator *gsi,
				   bool *handled_ops_p,
				   struct walk_stmt_info *wi)
{
  *handled_ops_p = false;
  gimple *stmt = gsi_stmt (*gsi);
  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      if (gimple_call_noreturn_p (as_a <gcall *> (stmt)))
	{
	  *handled_ops_p = true;
	  wi->info = stmt;
	  return error_mark_node;
	}
      break;

    /* We may reduce the following list if we find a way to implement the
       clauses, but now there is no point trying further.  */
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_ORDERED:
      *handled_ops_p = true;
      wi->info = stmt;
      return error_mark_node;

    case GIMPLE_OMP_FOR:
      if ((gimple_omp_for_kind (stmt) & GF_OMP_FOR_SIMD)
	  && gimple_omp_for_combined_into_p (stmt))
	{
	  *handled_ops_p = true;
	  wi->info = stmt;
	  return error_mark_node;
	}
      break;

    default:
      break;
    }
  return NULL;
}


/* If TARGET follows a pattern that can be turned into a gridified GPGPU
   kernel, return true, otherwise return false.  In the case of success, also
   fill in GROUP_SIZE_P with the requested group size or NULL if there is
   none.  */

static bool
grid_target_follows_gridifiable_pattern (gomp_target *target, tree *group_size_p)
{
  if (gimple_omp_target_kind (target) != GF_OMP_TARGET_KIND_REGION)
    return false;

  location_t tloc = gimple_location (target);
  gimple *stmt
    = grid_find_single_omp_among_assignments (gimple_omp_body (target),
					      tloc, "target");
  if (!stmt)
    return false;
  gomp_teams *teams = dyn_cast <gomp_teams *> (stmt);
  tree group_size = NULL;
  if (!teams)
    {
      dump_printf_loc (MSG_NOTE, tloc,
		       "Will not turn target construct into a simple "
		       "GPGPU kernel because it does not have a sole teams "
		       "construct in it.\n");
      return false;
    }

  tree clauses = gimple_omp_teams_clauses (teams);
  while (clauses)
    {
      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_NUM_TEAMS:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because we cannot "
			     "handle num_teams clause of teams "
			     "construct\n ");
	  return false;

	case OMP_CLAUSE_REDUCTION:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because a reduction "
			     "clause is present\n ");
	  return false;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because a lastprivate "
			     "clause is present\n ");
	  return false;

	case OMP_CLAUSE_THREAD_LIMIT:
	  group_size = OMP_CLAUSE_OPERAND (clauses, 0);
	  break;

	default:
	  break;
	}
      clauses = OMP_CLAUSE_CHAIN (clauses);
    }

  stmt = grid_find_single_omp_among_assignments (gimple_omp_body (teams), tloc,
						 "teams");
  if (!stmt)
    return false;
  gomp_for *dist = dyn_cast <gomp_for *> (stmt);
  if (!dist)
    {
      dump_printf_loc (MSG_NOTE, tloc,
		       "Will not turn target construct into a simple "
		       "GPGPU kernel because the teams construct  does not have "
		       "a sole distribute construct in it.\n");
      return false;
    }

  gcc_assert (gimple_omp_for_kind (dist) == GF_OMP_FOR_KIND_DISTRIBUTE);
  if (!gimple_omp_for_combined_p (dist))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, tloc,
			 "Will not turn target construct into a gridified GPGPU "
			 "kernel because we cannot handle a standalone "
			 "distribute construct\n ");
      return false;
    }
  if (dist->collapse > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, tloc,
			 "Will not turn target construct into a gridified GPGPU "
			 "kernel because the distribute construct contains "
			 "collapse clause\n");
      return false;
    }
  struct omp_for_data fd;
  extract_omp_for_data (dist, &fd, NULL);
  if (fd.chunk_size)
    {
      if (group_size && !operand_equal_p (group_size, fd.chunk_size, 0))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because the teams "
			     "thread limit is different from distribute "
			     "schedule chunk\n");
	  return false;
	}
      group_size = fd.chunk_size;
    }
  stmt = grid_find_single_omp_among_assignments (gimple_omp_body (dist), tloc,
						 "distribute");
  gomp_parallel *par;
  if (!stmt || !(par = dyn_cast <gomp_parallel *> (stmt)))
    return false;

  clauses = gimple_omp_parallel_clauses (par);
  while (clauses)
    {
      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_NUM_THREADS:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a gridified"
			     "GPGPU kernel because there is a num_threads "
			     "clause of the parallel construct\n");
	  return false;

	case OMP_CLAUSE_REDUCTION:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because a reduction "
			     "clause is present\n ");
	  return false;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because a lastprivate "
			     "clause is present\n ");
	  return false;

	default:
	  break;
	}
      clauses = OMP_CLAUSE_CHAIN (clauses);
    }

  stmt = grid_find_single_omp_among_assignments (gimple_omp_body (par), tloc,
						 "parallel");
  gomp_for *gfor;
  if (!stmt || !(gfor = dyn_cast <gomp_for *> (stmt)))
    return false;

  if (gimple_omp_for_kind (gfor) != GF_OMP_FOR_KIND_FOR)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, tloc,
			 "Will not turn target construct into a gridified GPGPU "
			 "kernel because the inner loop is not a simple for "
			 "loop\n");
      return false;
    }
  if (gfor->collapse > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, tloc,
			 "Will not turn target construct into a gridified GPGPU "
			 "kernel because the inner loop contains collapse "
			 "clause\n");
      return false;
    }

  if (!grid_seq_only_contains_local_assignments (gimple_omp_for_pre_body (gfor)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, tloc,
			 "Will not turn target construct into a gridified GPGPU "
			 "kernel because the inner loop pre_body contains"
			 "a complex instruction\n");
      return false;
    }

  clauses = gimple_omp_for_clauses (gfor);
  while (clauses)
    {
      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_SCHEDULE:
	  if (OMP_CLAUSE_SCHEDULE_KIND (clauses) != OMP_CLAUSE_SCHEDULE_AUTO)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, tloc,
				 "Will not turn target construct into a "
				 "gridified GPGPU kernel because the inner "
				 "loop has a non-automatic scheduling clause\n");
	      return false;
	    }
	  break;

	case OMP_CLAUSE_REDUCTION:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because a reduction "
			     "clause is present\n ");
	  return false;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a "
			     "gridified GPGPU kernel because a lastprivate "
			     "clause is present\n ");
	  return false;

	default:
	  break;
	}
      clauses = OMP_CLAUSE_CHAIN (clauses);
    }

  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  if (walk_gimple_seq (gimple_omp_body (gfor),
		       grid_find_ungridifiable_statement,
		       NULL, &wi))
    {
      gimple *bad = (gimple *) wi.info;
      if (dump_enabled_p ())
	{
	  if (is_gimple_call (bad))
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a gridified "
			     " GPGPU kernel because the inner loop contains "
			     "call to a noreturn function\n");
	  if (gimple_code (bad) == GIMPLE_OMP_FOR)
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a gridified "
			     " GPGPU kernel because the inner loop contains "
			     "a simd construct\n");
	  else
	    dump_printf_loc (MSG_NOTE, tloc,
			     "Will not turn target construct into a gridified "
			     "GPGPU kernel because the inner loop contains "
			     "statement %s which cannot be transformed\n",
			     gimple_code_name[(int) gimple_code (bad)]);
	}
      return false;
    }

  *group_size_p = group_size;
  return true;
}

/* Operand walker, used to remap pre-body declarations according to a hash map
   provided in DATA.  */

static tree
grid_remap_prebody_decls (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;

  if (DECL_P (t) || TYPE_P (t))
    *walk_subtrees = 0;
  else
    *walk_subtrees = 1;

  if (TREE_CODE (t) == VAR_DECL)
    {
      struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
      hash_map<tree, tree> *declmap = (hash_map<tree, tree> *) wi->info;
      tree *repl = declmap->get (t);
      if (repl)
	*tp = *repl;
    }
  return NULL_TREE;
}

/* Copy leading register-type assignments to local variables in SRC to just
   before DST, Creating temporaries, adjusting mapping of operands in WI and
   remapping operands as necessary.  Add any new temporaries to TGT_BIND.
   Return the first statement that does not conform to
   grid_reg_assignment_to_local_var_p or NULL.  */

static gimple *
grid_copy_leading_local_assignments (gimple_seq src, gimple_stmt_iterator *dst,
				gbind *tgt_bind, struct walk_stmt_info *wi)
{
  hash_map<tree, tree> *declmap = (hash_map<tree, tree> *) wi->info;
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (src); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gbind *bind = dyn_cast <gbind *> (stmt))
	{
	  gimple *r = grid_copy_leading_local_assignments
	    (gimple_bind_body (bind), dst, tgt_bind, wi);
	  if (r)
	    return r;
	  else
	    continue;
	}
      if (!grid_reg_assignment_to_local_var_p (stmt))
	return stmt;
      tree lhs = gimple_assign_lhs (as_a <gassign *> (stmt));
      tree repl = copy_var_decl (lhs, create_tmp_var_name (NULL),
				 TREE_TYPE (lhs));
      DECL_CONTEXT (repl) = current_function_decl;
      gimple_bind_append_vars (tgt_bind, repl);

      declmap->put (lhs, repl);
      gassign *copy = as_a <gassign *> (gimple_copy (stmt));
      walk_gimple_op (copy, grid_remap_prebody_decls, wi);
      gsi_insert_before (dst, copy, GSI_SAME_STMT);
    }
  return NULL;
}

/* Given freshly copied top level kernel SEQ, identify the individual OMP
   components, mark them as part of kernel and return the inner loop, and copy
   assignment leading to them just before DST, remapping them using WI and
   adding new temporaries to TGT_BIND.  */

static gomp_for *
grid_process_kernel_body_copy (gimple_seq seq, gimple_stmt_iterator *dst,
			       gbind *tgt_bind, struct walk_stmt_info *wi)
{
  gimple *stmt = grid_copy_leading_local_assignments (seq, dst, tgt_bind, wi);
  gomp_teams *teams = dyn_cast <gomp_teams *> (stmt);
  gcc_assert (teams);
  gimple_omp_teams_set_grid_phony (teams, true);
  stmt = grid_copy_leading_local_assignments (gimple_omp_body (teams), dst,
					 tgt_bind, wi);
  gcc_checking_assert (stmt);
  gomp_for *dist = dyn_cast <gomp_for *> (stmt);
  gcc_assert (dist);
  gimple_seq prebody = gimple_omp_for_pre_body (dist);
  if (prebody)
    grid_copy_leading_local_assignments (prebody, dst, tgt_bind, wi);
  gimple_omp_for_set_grid_phony (dist, true);
  stmt = grid_copy_leading_local_assignments (gimple_omp_body (dist), dst,
					 tgt_bind, wi);
  gcc_checking_assert (stmt);

  gomp_parallel *parallel = as_a <gomp_parallel *> (stmt);
  gimple_omp_parallel_set_grid_phony (parallel, true);
  stmt = grid_copy_leading_local_assignments (gimple_omp_body (parallel), dst,
					 tgt_bind, wi);
  gomp_for *inner_loop = as_a <gomp_for *> (stmt);
  gimple_omp_for_set_kind (inner_loop, GF_OMP_FOR_KIND_GRID_LOOP);
  prebody = gimple_omp_for_pre_body (inner_loop);
  if (prebody)
    grid_copy_leading_local_assignments (prebody, dst, tgt_bind, wi);

  return inner_loop;
}

/* If TARGET points to a GOMP_TARGET which follows a gridifiable pattern,
   create a GPU kernel for it.  GSI must point to the same statement, TGT_BIND
   is the bind into which temporaries inserted before TARGET should be
   added.  */

static void
grid_attempt_target_gridification (gomp_target *target,
				   gimple_stmt_iterator *gsi,
				   gbind *tgt_bind)
{
  tree group_size;
  if (!target || !grid_target_follows_gridifiable_pattern (target, &group_size))
    return;

  location_t loc = gimple_location (target);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
		     "Target construct will be turned into a gridified GPGPU "
		     "kernel\n");

  /* Copy target body to a GPUKERNEL construct:  */
  gimple_seq kernel_seq = copy_gimple_seq_and_replace_locals
    (gimple_omp_body (target));

  hash_map<tree, tree> *declmap = new hash_map<tree, tree>;
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (struct walk_stmt_info));
  wi.info = declmap;

  /* Copy assignments in between OMP statements before target, mark OMP
     statements within copy appropriatly.  */
  gomp_for *inner_loop = grid_process_kernel_body_copy (kernel_seq, gsi,
							tgt_bind, &wi);

  gbind *old_bind = as_a <gbind *> (gimple_seq_first (gimple_omp_body (target)));
  gbind *new_bind = as_a <gbind *> (gimple_seq_first (kernel_seq));
  tree new_block = gimple_bind_block (new_bind);
  tree enc_block = BLOCK_SUPERCONTEXT (gimple_bind_block (old_bind));
  BLOCK_CHAIN (new_block) = BLOCK_SUBBLOCKS (enc_block);
  BLOCK_SUBBLOCKS (enc_block) = new_block;
  BLOCK_SUPERCONTEXT (new_block) = enc_block;
  gimple *gpukernel = gimple_build_omp_grid_body (kernel_seq);
  gimple_seq_add_stmt
    (gimple_bind_body_ptr (as_a <gbind *> (gimple_omp_body (target))),
     gpukernel);

  walk_tree (&group_size, grid_remap_prebody_decls, &wi, NULL);
  push_gimplify_context ();
  size_t collapse = gimple_omp_for_collapse (inner_loop);
  for (size_t i = 0; i < collapse; i++)
    {
      tree itype, type = TREE_TYPE (gimple_omp_for_index (inner_loop, i));
      if (POINTER_TYPE_P (type))
	itype = signed_type_for (type);
      else
	itype = type;

      enum tree_code cond_code = gimple_omp_for_cond (inner_loop, i);
      tree n1 = unshare_expr (gimple_omp_for_initial (inner_loop, i));
      walk_tree (&n1, grid_remap_prebody_decls, &wi, NULL);
      tree n2 = unshare_expr (gimple_omp_for_final (inner_loop, i));
      walk_tree (&n2, grid_remap_prebody_decls, &wi, NULL);
      adjust_for_condition (loc, &cond_code, &n2);
      tree step;
      step = get_omp_for_step_from_incr (loc,
					 gimple_omp_for_incr (inner_loop, i));
      gimple_seq tmpseq = NULL;
      n1 = fold_convert (itype, n1);
      n2 = fold_convert (itype, n2);
      tree t = build_int_cst (itype, (cond_code == LT_EXPR ? -1 : 1));
      t = fold_build2 (PLUS_EXPR, itype, step, t);
      t = fold_build2 (PLUS_EXPR, itype, t, n2);
      t = fold_build2 (MINUS_EXPR, itype, t, n1);
      if (TYPE_UNSIGNED (itype) && cond_code == GT_EXPR)
	t = fold_build2 (TRUNC_DIV_EXPR, itype,
			 fold_build1 (NEGATE_EXPR, itype, t),
			 fold_build1 (NEGATE_EXPR, itype, step));
      else
	t = fold_build2 (TRUNC_DIV_EXPR, itype, t, step);
      tree gs = fold_convert (uint32_type_node, t);
      gimplify_expr (&gs, &tmpseq, NULL, is_gimple_val, fb_rvalue);
      if (!gimple_seq_empty_p (tmpseq))
	gsi_insert_seq_before (gsi, tmpseq, GSI_SAME_STMT);

      tree ws;
      if (i == 0 && group_size)
	{
	  ws = fold_convert (uint32_type_node, group_size);
	  tmpseq = NULL;
	  gimplify_expr (&ws, &tmpseq, NULL, is_gimple_val, fb_rvalue);
	  if (!gimple_seq_empty_p (tmpseq))
	    gsi_insert_seq_before (gsi, tmpseq, GSI_SAME_STMT);
	}
      else
	ws = build_zero_cst (uint32_type_node);

      tree c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__GRIDDIM_);
      OMP_CLAUSE__GRIDDIM__DIMENSION (c) = i;
      OMP_CLAUSE__GRIDDIM__SIZE (c) = gs;
      OMP_CLAUSE__GRIDDIM__GROUP (c) = ws;
      OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (target);
      gimple_omp_target_set_clauses (target, c);
    }
  pop_gimplify_context (tgt_bind);
  delete declmap;
  return;
}

/* Walker function doing all the work for create_target_kernels. */

static tree
grid_gridify_all_targets_stmt (gimple_stmt_iterator *gsi,
				   bool *handled_ops_p,
				   struct walk_stmt_info *incoming)
{
  *handled_ops_p = false;

  gimple *stmt = gsi_stmt (*gsi);
  gomp_target *target = dyn_cast <gomp_target *> (stmt);
  if (target)
    {
      gbind *tgt_bind = (gbind *) incoming->info;
      gcc_checking_assert (tgt_bind);
      grid_attempt_target_gridification (target, gsi, tgt_bind);
      return NULL_TREE;
    }
  gbind *bind = dyn_cast <gbind *> (stmt);
  if (bind)
    {
      *handled_ops_p = true;
      struct walk_stmt_info wi;
      memset (&wi, 0, sizeof (wi));
      wi.info = bind;
      walk_gimple_seq_mod (gimple_bind_body_ptr (bind),
			   grid_gridify_all_targets_stmt, NULL, &wi);
    }
  return NULL_TREE;
}

/* Attempt to gridify all target constructs in BODY_P.  All such targets will
   have their bodies duplicated, with the new copy being put into a
   gimple_omp_grid_body statement.  All kernel-related construct within the
   grid_body will be marked with phony flags or kernel kinds.  Moreover, some
   re-structuring is often needed, such as copying pre-bodies before the target
   construct so that kernel grid sizes can be computed.  */

static void
grid_gridify_all_targets (gimple_seq *body_p)
{
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq_mod (body_p, grid_gridify_all_targets_stmt, NULL, &wi);
}


/* Main entry point.  */

static unsigned int
execute_lower_omp (void)
{
  gimple_seq body;
  int i;
  omp_context *ctx;

  /* This pass always runs, to provide PROP_gimple_lomp.
     But often, there is nothing to do.  */
  if (flag_cilkplus == 0 && flag_openacc == 0 && flag_openmp == 0
      && flag_openmp_simd == 0)
    return 0;

  all_contexts = splay_tree_new (splay_tree_compare_pointers, 0,
				 delete_omp_context);

  body = gimple_body (current_function_decl);

  if (hsa_gen_requested_p ())
    grid_gridify_all_targets (&body);

  scan_omp (&body, NULL);
  gcc_assert (taskreg_nesting_level == 0);
  FOR_EACH_VEC_ELT (taskreg_contexts, i, ctx)
    finish_taskreg_scan (ctx);
  taskreg_contexts.release ();

  if (all_contexts->root)
    {
      if (task_shared_vars)
	push_gimplify_context ();
      lower_omp (&body, NULL);
      if (task_shared_vars)
	pop_gimplify_context (NULL);
    }

  if (all_contexts)
    {
      splay_tree_delete (all_contexts);
      all_contexts = NULL;
    }
  BITMAP_FREE (task_shared_vars);
  return 0;
}

namespace {

const pass_data pass_data_lower_omp =
{
  GIMPLE_PASS, /* type */
  "omplower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  PROP_gimple_lomp, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_omp : public gimple_opt_pass
{
public:
  pass_lower_omp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_omp, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return execute_lower_omp (); }

}; // class pass_lower_omp

} // anon namespace

gimple_opt_pass *
make_pass_lower_omp (gcc::context *ctxt)
{
  return new pass_lower_omp (ctxt);
}

/* The following is a utility to diagnose structured block violations.
   It is not part of the "omplower" pass, as that's invoked too late.  It
   should be invoked by the respective front ends after gimplification.  */

static splay_tree all_labels;

/* Check for mismatched contexts and generate an error if needed.  Return
   true if an error is detected.  */

static bool
diagnose_sb_0 (gimple_stmt_iterator *gsi_p,
	       gimple *branch_ctx, gimple *label_ctx)
{
  gcc_checking_assert (!branch_ctx || is_gimple_omp (branch_ctx));
  gcc_checking_assert (!label_ctx || is_gimple_omp (label_ctx));

  if (label_ctx == branch_ctx)
    return false;

  const char* kind = NULL;

  if (flag_cilkplus)
    {
      if ((branch_ctx
	   && gimple_code (branch_ctx) == GIMPLE_OMP_FOR
	   && gimple_omp_for_kind (branch_ctx) == GF_OMP_FOR_KIND_CILKSIMD)
	  || (label_ctx
	      && gimple_code (label_ctx) == GIMPLE_OMP_FOR
	      && gimple_omp_for_kind (label_ctx) == GF_OMP_FOR_KIND_CILKSIMD))
	kind = "Cilk Plus";
    }
  if (flag_openacc)
    {
      if ((branch_ctx && is_gimple_omp_oacc (branch_ctx))
	  || (label_ctx && is_gimple_omp_oacc (label_ctx)))
	{
	  gcc_checking_assert (kind == NULL);
	  kind = "OpenACC";
	}
    }
  if (kind == NULL)
    {
      gcc_checking_assert (flag_openmp);
      kind = "OpenMP";
    }

  /*
     Previously we kept track of the label's entire context in diagnose_sb_[12]
     so we could traverse it and issue a correct "exit" or "enter" error
     message upon a structured block violation.

     We built the context by building a list with tree_cons'ing, but there is
     no easy counterpart in gimple tuples.  It seems like far too much work
     for issuing exit/enter error messages.  If someone really misses the
     distinct error message... patches welcome.
   */

#if 0
  /* Try to avoid confusing the user by producing and error message
     with correct "exit" or "enter" verbiage.  We prefer "exit"
     unless we can show that LABEL_CTX is nested within BRANCH_CTX.  */
  if (branch_ctx == NULL)
    exit_p = false;
  else
    {
      while (label_ctx)
	{
	  if (TREE_VALUE (label_ctx) == branch_ctx)
	    {
	      exit_p = false;
	      break;
	    }
	  label_ctx = TREE_CHAIN (label_ctx);
	}
    }

  if (exit_p)
    error ("invalid exit from %s structured block", kind);
  else
    error ("invalid entry to %s structured block", kind);
#endif

  /* If it's obvious we have an invalid entry, be specific about the error.  */
  if (branch_ctx == NULL)
    error ("invalid entry to %s structured block", kind);
  else
    {
      /* Otherwise, be vague and lazy, but efficient.  */
      error ("invalid branch to/from %s structured block", kind);
    }

  gsi_replace (gsi_p, gimple_build_nop (), false);
  return true;
}

/* Pass 1: Create a minimal tree of structured blocks, and record
   where each label is found.  */

static tree
diagnose_sb_1 (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
    	       struct walk_stmt_info *wi)
{
  gimple *context = (gimple *) wi->info;
  gimple *inner_context;
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;

  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_TASKGROUP:
      /* The minimal context here is just the current OMP construct.  */
      inner_context = stmt;
      wi->info = inner_context;
      walk_gimple_seq (gimple_omp_body (stmt), diagnose_sb_1, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_OMP_FOR:
      inner_context = stmt;
      wi->info = inner_context;
      /* gimple_omp_for_{index,initial,final} are all DECLs; no need to
	 walk them.  */
      walk_gimple_seq (gimple_omp_for_pre_body (stmt),
	  	       diagnose_sb_1, NULL, wi);
      walk_gimple_seq (gimple_omp_body (stmt), diagnose_sb_1, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_LABEL:
      splay_tree_insert (all_labels,
			 (splay_tree_key) gimple_label_label (
					    as_a <glabel *> (stmt)),
			 (splay_tree_value) context);
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Pass 2: Check each branch and see if its context differs from that of
   the destination label's context.  */

static tree
diagnose_sb_2 (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
    	       struct walk_stmt_info *wi)
{
  gimple *context = (gimple *) wi->info;
  splay_tree_node n;
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;

  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_TASKGROUP:
      wi->info = stmt;
      walk_gimple_seq_mod (gimple_omp_body_ptr (stmt), diagnose_sb_2, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_OMP_FOR:
      wi->info = stmt;
      /* gimple_omp_for_{index,initial,final} are all DECLs; no need to
	 walk them.  */
      walk_gimple_seq_mod (gimple_omp_for_pre_body_ptr (stmt),
			   diagnose_sb_2, NULL, wi);
      walk_gimple_seq_mod (gimple_omp_body_ptr (stmt), diagnose_sb_2, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_COND:
	{
	  gcond *cond_stmt = as_a <gcond *> (stmt);
	  tree lab = gimple_cond_true_label (cond_stmt);
	  if (lab)
	    {
	      n = splay_tree_lookup (all_labels,
				     (splay_tree_key) lab);
	      diagnose_sb_0 (gsi_p, context,
			     n ? (gimple *) n->value : NULL);
	    }
	  lab = gimple_cond_false_label (cond_stmt);
	  if (lab)
	    {
	      n = splay_tree_lookup (all_labels,
				     (splay_tree_key) lab);
	      diagnose_sb_0 (gsi_p, context,
			     n ? (gimple *) n->value : NULL);
	    }
	}
      break;

    case GIMPLE_GOTO:
      {
	tree lab = gimple_goto_dest (stmt);
	if (TREE_CODE (lab) != LABEL_DECL)
	  break;

	n = splay_tree_lookup (all_labels, (splay_tree_key) lab);
	diagnose_sb_0 (gsi_p, context, n ? (gimple *) n->value : NULL);
      }
      break;

    case GIMPLE_SWITCH:
      {
	gswitch *switch_stmt = as_a <gswitch *> (stmt);
	unsigned int i;
	for (i = 0; i < gimple_switch_num_labels (switch_stmt); ++i)
	  {
	    tree lab = CASE_LABEL (gimple_switch_label (switch_stmt, i));
	    n = splay_tree_lookup (all_labels, (splay_tree_key) lab);
	    if (n && diagnose_sb_0 (gsi_p, context, (gimple *) n->value))
	      break;
	  }
      }
      break;

    case GIMPLE_RETURN:
      diagnose_sb_0 (gsi_p, context, NULL);
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Called from tree-cfg.c::make_edges to create cfg edges for all relevant
   GIMPLE_* codes.  */
bool
make_gimple_omp_edges (basic_block bb, struct omp_region **region,
		       int *region_idx)
{
  gimple *last = last_stmt (bb);
  enum gimple_code code = gimple_code (last);
  struct omp_region *cur_region = *region;
  bool fallthru = false;

  switch (code)
    {
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_FOR:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_GRID_BODY:
      cur_region = new_omp_region (bb, code, cur_region);
      fallthru = true;
      break;

    case GIMPLE_OMP_ORDERED:
      cur_region = new_omp_region (bb, code, cur_region);
      fallthru = true;
      if (find_omp_clause (gimple_omp_ordered_clauses
			     (as_a <gomp_ordered *> (last)),
			   OMP_CLAUSE_DEPEND))
	cur_region = cur_region->outer;
      break;

    case GIMPLE_OMP_TARGET:
      cur_region = new_omp_region (bb, code, cur_region);
      fallthru = true;
      switch (gimple_omp_target_kind (last))
	{
	case GF_OMP_TARGET_KIND_REGION:
	case GF_OMP_TARGET_KIND_DATA:
	case GF_OMP_TARGET_KIND_OACC_PARALLEL:
	case GF_OMP_TARGET_KIND_OACC_KERNELS:
	case GF_OMP_TARGET_KIND_OACC_DATA:
	case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
	  break;
	case GF_OMP_TARGET_KIND_UPDATE:
	case GF_OMP_TARGET_KIND_ENTER_DATA:
	case GF_OMP_TARGET_KIND_EXIT_DATA:
	case GF_OMP_TARGET_KIND_OACC_UPDATE:
	case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
	case GF_OMP_TARGET_KIND_OACC_DECLARE:
	  cur_region = cur_region->outer;
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case GIMPLE_OMP_SECTIONS:
      cur_region = new_omp_region (bb, code, cur_region);
      fallthru = true;
      break;

    case GIMPLE_OMP_SECTIONS_SWITCH:
      fallthru = false;
      break;

    case GIMPLE_OMP_ATOMIC_LOAD:
    case GIMPLE_OMP_ATOMIC_STORE:
       fallthru = true;
       break;

    case GIMPLE_OMP_RETURN:
      /* In the case of a GIMPLE_OMP_SECTION, the edge will go
	 somewhere other than the next block.  This will be
	 created later.  */
      cur_region->exit = bb;
      if (cur_region->type == GIMPLE_OMP_TASK)
	/* Add an edge corresponding to not scheduling the task
	   immediately.  */
	make_edge (cur_region->entry, bb, EDGE_ABNORMAL);
      fallthru = cur_region->type != GIMPLE_OMP_SECTION;
      cur_region = cur_region->outer;
      break;

    case GIMPLE_OMP_CONTINUE:
      cur_region->cont = bb;
      switch (cur_region->type)
	{
	case GIMPLE_OMP_FOR:
	  /* Mark all GIMPLE_OMP_FOR and GIMPLE_OMP_CONTINUE
	     succs edges as abnormal to prevent splitting
	     them.  */
	  single_succ_edge (cur_region->entry)->flags |= EDGE_ABNORMAL;
	  /* Make the loopback edge.  */
	  make_edge (bb, single_succ (cur_region->entry),
		     EDGE_ABNORMAL);

	  /* Create an edge from GIMPLE_OMP_FOR to exit, which
	     corresponds to the case that the body of the loop
	     is not executed at all.  */
	  make_edge (cur_region->entry, bb->next_bb, EDGE_ABNORMAL);
	  make_edge (bb, bb->next_bb, EDGE_FALLTHRU | EDGE_ABNORMAL);
	  fallthru = false;
	  break;

	case GIMPLE_OMP_SECTIONS:
	  /* Wire up the edges into and out of the nested sections.  */
	  {
	    basic_block switch_bb = single_succ (cur_region->entry);

	    struct omp_region *i;
	    for (i = cur_region->inner; i ; i = i->next)
	      {
		gcc_assert (i->type == GIMPLE_OMP_SECTION);
		make_edge (switch_bb, i->entry, 0);
		make_edge (i->exit, bb, EDGE_FALLTHRU);
	      }

	    /* Make the loopback edge to the block with
	       GIMPLE_OMP_SECTIONS_SWITCH.  */
	    make_edge (bb, switch_bb, 0);

	    /* Make the edge from the switch to exit.  */
	    make_edge (switch_bb, bb->next_bb, 0);
	    fallthru = false;
	  }
	  break;

	case GIMPLE_OMP_TASK:
	  fallthru = true;
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

    default:
      gcc_unreachable ();
    }

  if (*region != cur_region)
    {
      *region = cur_region;
      if (cur_region)
	*region_idx = cur_region->entry->index;
      else
	*region_idx = 0;
    }

  return fallthru;
}

static unsigned int
diagnose_omp_structured_block_errors (void)
{
  struct walk_stmt_info wi;
  gimple_seq body = gimple_body (current_function_decl);

  all_labels = splay_tree_new (splay_tree_compare_pointers, 0, 0);

  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq (body, diagnose_sb_1, NULL, &wi);

  memset (&wi, 0, sizeof (wi));
  wi.want_locations = true;
  walk_gimple_seq_mod (&body, diagnose_sb_2, NULL, &wi);

  gimple_set_body (current_function_decl, body);

  splay_tree_delete (all_labels);
  all_labels = NULL;

  return 0;
}

namespace {

const pass_data pass_data_diagnose_omp_blocks =
{
  GIMPLE_PASS, /* type */
  "*diagnose_omp_blocks", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_diagnose_omp_blocks : public gimple_opt_pass
{
public:
  pass_diagnose_omp_blocks (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_diagnose_omp_blocks, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return flag_cilkplus || flag_openacc || flag_openmp;
  }
  virtual unsigned int execute (function *)
    {
      return diagnose_omp_structured_block_errors ();
    }

}; // class pass_diagnose_omp_blocks

} // anon namespace

gimple_opt_pass *
make_pass_diagnose_omp_blocks (gcc::context *ctxt)
{
  return new pass_diagnose_omp_blocks (ctxt);
}

/* Helper function for omp_finish_file routine.  Takes decls from V_DECLS and
   adds their addresses and sizes to constructor-vector V_CTOR.  */
static void
add_decls_addresses_to_decl_constructor (vec<tree, va_gc> *v_decls,
					 vec<constructor_elt, va_gc> *v_ctor)
{
  unsigned len = vec_safe_length (v_decls);
  for (unsigned i = 0; i < len; i++)
    {
      tree it = (*v_decls)[i];
      bool is_var = TREE_CODE (it) == VAR_DECL;
      bool is_link_var
	= is_var
#ifdef ACCEL_COMPILER
	  && DECL_HAS_VALUE_EXPR_P (it)
#endif
	  && lookup_attribute ("omp declare target link", DECL_ATTRIBUTES (it));

      tree size = NULL_TREE;
      if (is_var)
	size = fold_convert (const_ptr_type_node, DECL_SIZE_UNIT (it));

      tree addr;
      if (!is_link_var)
	addr = build_fold_addr_expr (it);
      else
	{
#ifdef ACCEL_COMPILER
	  /* For "omp declare target link" vars add address of the pointer to
	     the target table, instead of address of the var.  */
	  tree value_expr = DECL_VALUE_EXPR (it);
	  tree link_ptr_decl = TREE_OPERAND (value_expr, 0);
	  varpool_node::finalize_decl (link_ptr_decl);
	  addr = build_fold_addr_expr (link_ptr_decl);
#else
	  addr = build_fold_addr_expr (it);
#endif

	  /* Most significant bit of the size marks "omp declare target link"
	     vars in host and target tables.  */
	  unsigned HOST_WIDE_INT isize = tree_to_uhwi (size);
	  isize |= 1ULL << (int_size_in_bytes (const_ptr_type_node)
			    * BITS_PER_UNIT - 1);
	  size = wide_int_to_tree (const_ptr_type_node, isize);
	}

      CONSTRUCTOR_APPEND_ELT (v_ctor, NULL_TREE, addr);
      if (is_var)
	CONSTRUCTOR_APPEND_ELT (v_ctor, NULL_TREE, size);
    }
}

/* Create new symbols containing (address, size) pairs for global variables,
   marked with "omp declare target" attribute, as well as addresses for the
   functions, which are outlined offloading regions.  */
void
omp_finish_file (void)
{
  unsigned num_funcs = vec_safe_length (offload_funcs);
  unsigned num_vars = vec_safe_length (offload_vars);

  if (num_funcs == 0 && num_vars == 0)
    return;

  if (targetm_common.have_named_sections)
    {
      vec<constructor_elt, va_gc> *v_f, *v_v;
      vec_alloc (v_f, num_funcs);
      vec_alloc (v_v, num_vars * 2);

      add_decls_addresses_to_decl_constructor (offload_funcs, v_f);
      add_decls_addresses_to_decl_constructor (offload_vars, v_v);

      tree vars_decl_type = build_array_type_nelts (pointer_sized_int_node,
						    num_vars * 2);
      tree funcs_decl_type = build_array_type_nelts (pointer_sized_int_node,
						     num_funcs);
      SET_TYPE_ALIGN (vars_decl_type, TYPE_ALIGN (pointer_sized_int_node));
      SET_TYPE_ALIGN (funcs_decl_type, TYPE_ALIGN (pointer_sized_int_node));
      tree ctor_v = build_constructor (vars_decl_type, v_v);
      tree ctor_f = build_constructor (funcs_decl_type, v_f);
      TREE_CONSTANT (ctor_v) = TREE_CONSTANT (ctor_f) = 1;
      TREE_STATIC (ctor_v) = TREE_STATIC (ctor_f) = 1;
      tree funcs_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				    get_identifier (".offload_func_table"),
				    funcs_decl_type);
      tree vars_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				   get_identifier (".offload_var_table"),
				   vars_decl_type);
      TREE_STATIC (funcs_decl) = TREE_STATIC (vars_decl) = 1;
      /* Do not align tables more than TYPE_ALIGN (pointer_sized_int_node),
	 otherwise a joint table in a binary will contain padding between
	 tables from multiple object files.  */
      DECL_USER_ALIGN (funcs_decl) = DECL_USER_ALIGN (vars_decl) = 1;
      SET_DECL_ALIGN (funcs_decl, TYPE_ALIGN (funcs_decl_type));
      SET_DECL_ALIGN (vars_decl, TYPE_ALIGN (vars_decl_type));
      DECL_INITIAL (funcs_decl) = ctor_f;
      DECL_INITIAL (vars_decl) = ctor_v;
      set_decl_section_name (funcs_decl, OFFLOAD_FUNC_TABLE_SECTION_NAME);
      set_decl_section_name (vars_decl, OFFLOAD_VAR_TABLE_SECTION_NAME);

      varpool_node::finalize_decl (vars_decl);
      varpool_node::finalize_decl (funcs_decl);
    }
  else
    {
      for (unsigned i = 0; i < num_funcs; i++)
	{
	  tree it = (*offload_funcs)[i];
	  targetm.record_offload_symbol (it);
	}
      for (unsigned i = 0; i < num_vars; i++)
	{
	  tree it = (*offload_vars)[i];
	  targetm.record_offload_symbol (it);
	}
    }
}

/* Find the number of threads (POS = false), or thread number (POS =
   true) for an OpenACC region partitioned as MASK.  Setup code
   required for the calculation is added to SEQ.  */

static tree
oacc_thread_numbers (bool pos, int mask, gimple_seq *seq)
{
  tree res = pos ? NULL_TREE : build_int_cst (unsigned_type_node, 1);
  unsigned ix;

  /* Start at gang level, and examine relevant dimension indices.  */
  for (ix = GOMP_DIM_GANG; ix != GOMP_DIM_MAX; ix++)
    if (GOMP_DIM_MASK (ix) & mask)
      {
	tree arg = build_int_cst (unsigned_type_node, ix);

	if (res)
	  {
	    /* We had an outer index, so scale that by the size of
	       this dimension.  */
	    tree n = create_tmp_var (integer_type_node);
	    gimple *call
	      = gimple_build_call_internal (IFN_GOACC_DIM_SIZE, 1, arg);
	    
	    gimple_call_set_lhs (call, n);
	    gimple_seq_add_stmt (seq, call);
	    res = fold_build2 (MULT_EXPR, integer_type_node, res, n);
	  }
	if (pos)
	  {
	    /* Determine index in this dimension.  */
	    tree id = create_tmp_var (integer_type_node);
	    gimple *call = gimple_build_call_internal
	      (IFN_GOACC_DIM_POS, 1, arg);

	    gimple_call_set_lhs (call, id);
	    gimple_seq_add_stmt (seq, call);
	    if (res)
	      res = fold_build2 (PLUS_EXPR, integer_type_node, res, id);
	    else
	      res = id;
	  }
      }

  if (res == NULL_TREE)
    res = integer_zero_node;

  return res;
}

/* Transform IFN_GOACC_LOOP calls to actual code.  See
   expand_oacc_for for where these are generated.  At the vector
   level, we stride loops, such that each member of a warp will
   operate on adjacent iterations.  At the worker and gang level,
   each gang/warp executes a set of contiguous iterations.  Chunking
   can override this such that each iteration engine executes a
   contiguous chunk, and then moves on to stride to the next chunk.   */

static void
oacc_xform_loop (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  enum ifn_goacc_loop_kind code
    = (enum ifn_goacc_loop_kind) TREE_INT_CST_LOW (gimple_call_arg (call, 0));
  tree dir = gimple_call_arg (call, 1);
  tree range = gimple_call_arg (call, 2);
  tree step = gimple_call_arg (call, 3);
  tree chunk_size = NULL_TREE;
  unsigned mask = (unsigned) TREE_INT_CST_LOW (gimple_call_arg (call, 5));
  tree lhs = gimple_call_lhs (call);
  tree type = TREE_TYPE (lhs);
  tree diff_type = TREE_TYPE (range);
  tree r = NULL_TREE;
  gimple_seq seq = NULL;
  bool chunking = false, striding = true;
  unsigned outer_mask = mask & (~mask + 1); // Outermost partitioning
  unsigned inner_mask = mask & ~outer_mask; // Inner partitioning (if any)

#ifdef ACCEL_COMPILER
  chunk_size = gimple_call_arg (call, 4);
  if (integer_minus_onep (chunk_size)  /* Force static allocation.  */
      || integer_zerop (chunk_size))   /* Default (also static).  */
    {
      /* If we're at the gang level, we want each to execute a
	 contiguous run of iterations.  Otherwise we want each element
	 to stride.  */
      striding = !(outer_mask & GOMP_DIM_MASK (GOMP_DIM_GANG));
      chunking = false;
    }
  else
    {
      /* Chunk of size 1 is striding.  */
      striding = integer_onep (chunk_size);
      chunking = !striding;
    }
#endif

  /* striding=true, chunking=true
       -> invalid.
     striding=true, chunking=false
       -> chunks=1
     striding=false,chunking=true
       -> chunks=ceil (range/(chunksize*threads*step))
     striding=false,chunking=false
       -> chunk_size=ceil(range/(threads*step)),chunks=1  */
  push_gimplify_context (true);

  switch (code)
    {
    default: gcc_unreachable ();

    case IFN_GOACC_LOOP_CHUNKS:
      if (!chunking)
	r = build_int_cst (type, 1);
      else
	{
	  /* chunk_max
	     = (range - dir) / (chunks * step * num_threads) + dir  */
	  tree per = oacc_thread_numbers (false, mask, &seq);
	  per = fold_convert (type, per);
	  chunk_size = fold_convert (type, chunk_size);
	  per = fold_build2 (MULT_EXPR, type, per, chunk_size);
	  per = fold_build2 (MULT_EXPR, type, per, step);
	  r = build2 (MINUS_EXPR, type, range, dir);
	  r = build2 (PLUS_EXPR, type, r, per);
	  r = build2 (TRUNC_DIV_EXPR, type, r, per);
	}
      break;

    case IFN_GOACC_LOOP_STEP:
      {
	/* If striding, step by the entire compute volume, otherwise
	   step by the inner volume.  */
	unsigned volume = striding ? mask : inner_mask;

	r = oacc_thread_numbers (false, volume, &seq);
	r = build2 (MULT_EXPR, type, fold_convert (type, r), step);
      }
      break;

    case IFN_GOACC_LOOP_OFFSET:
      if (striding)
	{
	  r = oacc_thread_numbers (true, mask, &seq);
	  r = fold_convert (diff_type, r);
	}
      else
	{
	  tree inner_size = oacc_thread_numbers (false, inner_mask, &seq);
	  tree outer_size = oacc_thread_numbers (false, outer_mask, &seq);
	  tree volume = fold_build2 (MULT_EXPR, TREE_TYPE (inner_size),
				     inner_size, outer_size);

	  volume = fold_convert (diff_type, volume);
	  if (chunking)
	    chunk_size = fold_convert (diff_type, chunk_size);
	  else
	    {
	      tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);

	      chunk_size = build2 (MINUS_EXPR, diff_type, range, dir);
	      chunk_size = build2 (PLUS_EXPR, diff_type, chunk_size, per);
	      chunk_size = build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);
	    }

	  tree span = build2 (MULT_EXPR, diff_type, chunk_size,
			      fold_convert (diff_type, inner_size));
	  r = oacc_thread_numbers (true, outer_mask, &seq);
	  r = fold_convert (diff_type, r);
	  r = build2 (MULT_EXPR, diff_type, r, span);

	  tree inner = oacc_thread_numbers (true, inner_mask, &seq);
	  inner = fold_convert (diff_type, inner);
	  r = fold_build2 (PLUS_EXPR, diff_type, r, inner);

	  if (chunking)
	    {
	      tree chunk = fold_convert (diff_type, gimple_call_arg (call, 6));
	      tree per
		= fold_build2 (MULT_EXPR, diff_type, volume, chunk_size);
	      per = build2 (MULT_EXPR, diff_type, per, chunk);

	      r = build2 (PLUS_EXPR, diff_type, r, per);
	    }
	}
      r = fold_build2 (MULT_EXPR, diff_type, r, step);
      if (type != diff_type)
	r = fold_convert (type, r);
      break;

    case IFN_GOACC_LOOP_BOUND:
      if (striding)
	r = range;
      else
	{
	  tree inner_size = oacc_thread_numbers (false, inner_mask, &seq);
	  tree outer_size = oacc_thread_numbers (false, outer_mask, &seq);
	  tree volume = fold_build2 (MULT_EXPR, TREE_TYPE (inner_size),
				     inner_size, outer_size);

	  volume = fold_convert (diff_type, volume);
	  if (chunking)
	    chunk_size = fold_convert (diff_type, chunk_size);
	  else
	    {
	      tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);

	      chunk_size = build2 (MINUS_EXPR, diff_type, range, dir);
	      chunk_size = build2 (PLUS_EXPR, diff_type, chunk_size, per);
	      chunk_size = build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);
	    }

	  tree span = build2 (MULT_EXPR, diff_type, chunk_size,
			      fold_convert (diff_type, inner_size));

	  r = fold_build2 (MULT_EXPR, diff_type, span, step);

	  tree offset = gimple_call_arg (call, 6);
	  r = build2 (PLUS_EXPR, diff_type, r,
		      fold_convert (diff_type, offset));
	  r = build2 (integer_onep (dir) ? MIN_EXPR : MAX_EXPR,
		      diff_type, r, range);
	}
      if (diff_type != type)
	r = fold_convert (type, r);
      break;
    }

  gimplify_assign (lhs, r, &seq);

  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* Default partitioned and minimum partitioned dimensions.  */

static int oacc_default_dims[GOMP_DIM_MAX];
static int oacc_min_dims[GOMP_DIM_MAX];

/* Parse the default dimension parameter.  This is a set of
   :-separated optional compute dimensions.  Each specified dimension
   is a positive integer.  When device type support is added, it is
   planned to be a comma separated list of such compute dimensions,
   with all but the first prefixed by the colon-terminated device
   type.  */

static void
oacc_parse_default_dims (const char *dims)
{
  int ix;

  for (ix = GOMP_DIM_MAX; ix--;)
    {
      oacc_default_dims[ix] = -1;
      oacc_min_dims[ix] = 1;
    }

#ifndef ACCEL_COMPILER
  /* Cannot be overridden on the host.  */
  dims = NULL;
#endif
  if (dims)
    {
      const char *pos = dims;

      for (ix = 0; *pos && ix != GOMP_DIM_MAX; ix++)
	{
	  if (ix)
	    {
	      if (*pos != ':')
		goto malformed;
	      pos++;
	    }

	  if (*pos != ':')
	    {
	      long val;
	      const char *eptr;

	      errno = 0;
	      val = strtol (pos, CONST_CAST (char **, &eptr), 10);
	      if (errno || val <= 0 || (int) val != val)
		goto malformed;
	      pos = eptr;
	      oacc_default_dims[ix] = (int) val;
	    }
	}
      if (*pos)
	{
	malformed:
	  error_at (UNKNOWN_LOCATION,
		    "-fopenacc-dim operand is malformed at '%s'", pos);
	}
    }

  /* Allow the backend to validate the dimensions.  */
  targetm.goacc.validate_dims (NULL_TREE, oacc_default_dims, -1);
  targetm.goacc.validate_dims (NULL_TREE, oacc_min_dims, -2);
}

/* Validate and update the dimensions for offloaded FN.  ATTRS is the
   raw attribute.  DIMS is an array of dimensions, which is filled in.
   LEVEL is the partitioning level of a routine, or -1 for an offload
   region itself. USED is the mask of partitioned execution in the
   function.  */

static void
oacc_validate_dims (tree fn, tree attrs, int *dims, int level, unsigned used)
{
  tree purpose[GOMP_DIM_MAX];
  unsigned ix;
  tree pos = TREE_VALUE (attrs);
  bool is_kernel = oacc_fn_attrib_kernels_p (attrs);

  /* Make sure the attribute creator attached the dimension
     information.  */
  gcc_assert (pos);

  for (ix = 0; ix != GOMP_DIM_MAX; ix++)
    {
      purpose[ix] = TREE_PURPOSE (pos);
      tree val = TREE_VALUE (pos);
      dims[ix] = val ? TREE_INT_CST_LOW (val) : -1;
      pos = TREE_CHAIN (pos);
    }

  bool changed = targetm.goacc.validate_dims (fn, dims, level);

  /* Default anything left to 1 or a partitioned default.  */
  for (ix = 0; ix != GOMP_DIM_MAX; ix++)
    if (dims[ix] < 0)
      {
	/* The OpenACC spec says 'If the [num_gangs] clause is not
	   specified, an implementation-defined default will be used;
	   the default may depend on the code within the construct.' 
	   (2.5.6).  Thus an implementation is free to choose
	   non-unity default for a parallel region that doesn't have
	   any gang-partitioned loops.  However, it appears that there
	   is a sufficient body of user code that expects non-gang
	   partitioned regions to not execute in gang-redundant mode.
	   So we (a) don't warn about the non-portability and (b) pick
	   the minimum permissible dimension size when there is no
	   partitioned execution.  Otherwise we pick the global
	   default for the dimension, which the user can control.  The
	   same wording and logic applies to num_workers and
	   vector_length, however the worker- or vector- single
	   execution doesn't have the same impact as gang-redundant
	   execution.  (If the minimum gang-level partioning is not 1,
	   the target is probably too confusing.)  */
	dims[ix] = (used & GOMP_DIM_MASK (ix)
		    ? oacc_default_dims[ix] : oacc_min_dims[ix]);
	changed = true;
      }

  if (changed)
    {
      /* Replace the attribute with new values.  */
      pos = NULL_TREE;
      for (ix = GOMP_DIM_MAX; ix--;)
	{
	  pos = tree_cons (purpose[ix],
			   build_int_cst (integer_type_node, dims[ix]),
			   pos);
	  if (is_kernel)
	    TREE_PUBLIC (pos) = 1;
	}
      replace_oacc_fn_attrib (fn, pos);
    }
}

/* Create an empty OpenACC loop structure at LOC.  */

static oacc_loop *
new_oacc_loop_raw (oacc_loop *parent, location_t loc)
{
  oacc_loop *loop = XCNEW (oacc_loop);

  loop->parent = parent;
  loop->child = loop->sibling = NULL;

  if (parent)
    {
      loop->sibling = parent->child;
      parent->child = loop;
    }

  loop->loc = loc;
  loop->marker = NULL;
  memset (loop->heads, 0, sizeof (loop->heads));
  memset (loop->tails, 0, sizeof (loop->tails));
  loop->routine = NULL_TREE;

  loop->mask = loop->flags = loop->inner = 0;
  loop->ifns = 0;
  loop->chunk_size = 0;
  loop->head_end = NULL;

  return loop;
}

/* Create an outermost, dummy OpenACC loop for offloaded function
   DECL.  */

static oacc_loop *
new_oacc_loop_outer (tree decl)
{
  return new_oacc_loop_raw (NULL, DECL_SOURCE_LOCATION (decl));
}

/* Start a new OpenACC loop  structure beginning at head marker HEAD.
   Link into PARENT loop.  Return the new loop.  */

static oacc_loop *
new_oacc_loop (oacc_loop *parent, gcall *marker)
{
  oacc_loop *loop = new_oacc_loop_raw (parent, gimple_location (marker));

  loop->marker = marker;
  
  /* TODO: This is where device_type flattening would occur for the loop
     flags.   */

  loop->flags = TREE_INT_CST_LOW (gimple_call_arg (marker, 3));

  tree chunk_size = integer_zero_node;
  if (loop->flags & OLF_GANG_STATIC)
    chunk_size = gimple_call_arg (marker, 4);
  loop->chunk_size = chunk_size;

  return loop;
}

/* Create a dummy loop encompassing a call to a openACC routine.
   Extract the routine's partitioning requirements.  */

static void
new_oacc_loop_routine (oacc_loop *parent, gcall *call, tree decl, tree attrs)
{
  oacc_loop *loop = new_oacc_loop_raw (parent, gimple_location (call));
  int level = oacc_fn_attrib_level (attrs);

  gcc_assert (level >= 0);

  loop->marker = call;
  loop->routine = decl;
  loop->mask = ((GOMP_DIM_MASK (GOMP_DIM_MAX) - 1)
		^ (GOMP_DIM_MASK (level) - 1));
}

/* Finish off the current OpenACC loop ending at tail marker TAIL.
   Return the parent loop.  */

static oacc_loop *
finish_oacc_loop (oacc_loop *loop)
{
  /* If the loop has been collapsed, don't partition it.  */
  if (!loop->ifns)
    loop->mask = loop->flags = 0;
  return loop->parent;
}

/* Free all OpenACC loop structures within LOOP (inclusive).  */

static void
free_oacc_loop (oacc_loop *loop)
{
  if (loop->sibling)
    free_oacc_loop (loop->sibling);
  if (loop->child)
    free_oacc_loop (loop->child);

  free (loop);
}

/* Dump out the OpenACC loop head or tail beginning at FROM.  */

static void
dump_oacc_loop_part (FILE *file, gcall *from, int depth,
		     const char *title, int level)
{
  enum ifn_unique_kind kind
    = (enum ifn_unique_kind) TREE_INT_CST_LOW (gimple_call_arg (from, 0));

  fprintf (file, "%*s%s-%d:\n", depth * 2, "", title, level);
  for (gimple_stmt_iterator gsi = gsi_for_stmt (from);;)
    {
      gimple *stmt = gsi_stmt (gsi);

      if (is_gimple_call (stmt)
	  && gimple_call_internal_p (stmt)
	  && gimple_call_internal_fn (stmt) == IFN_UNIQUE)
	{
	  enum ifn_unique_kind k
	    = ((enum ifn_unique_kind) TREE_INT_CST_LOW
	       (gimple_call_arg (stmt, 0)));

	  if (k == kind && stmt != from)
	    break;
	}
      print_gimple_stmt (file, stmt, depth * 2 + 2, 0);

      gsi_next (&gsi);
      while (gsi_end_p (gsi))
	gsi = gsi_start_bb (single_succ (gsi_bb (gsi)));
    }
}

/* Dump OpenACC loops LOOP, its siblings and its children.  */

static void
dump_oacc_loop (FILE *file, oacc_loop *loop, int depth)
{
  int ix;
  
  fprintf (file, "%*sLoop %x(%x) %s:%u\n", depth * 2, "",
	   loop->flags, loop->mask,
	   LOCATION_FILE (loop->loc), LOCATION_LINE (loop->loc));

  if (loop->marker)
    print_gimple_stmt (file, loop->marker, depth * 2, 0);

  if (loop->routine)
    fprintf (file, "%*sRoutine %s:%u:%s\n",
	     depth * 2, "", DECL_SOURCE_FILE (loop->routine),
	     DECL_SOURCE_LINE (loop->routine),
	     IDENTIFIER_POINTER (DECL_NAME (loop->routine)));

  for (ix = GOMP_DIM_GANG; ix != GOMP_DIM_MAX; ix++)
    if (loop->heads[ix])
      dump_oacc_loop_part (file, loop->heads[ix], depth, "Head", ix);
  for (ix = GOMP_DIM_MAX; ix--;)
    if (loop->tails[ix])
      dump_oacc_loop_part (file, loop->tails[ix], depth, "Tail", ix);

  if (loop->child)
    dump_oacc_loop (file, loop->child, depth + 1);
  if (loop->sibling)
    dump_oacc_loop (file, loop->sibling, depth);
}

void debug_oacc_loop (oacc_loop *);

/* Dump loops to stderr.  */

DEBUG_FUNCTION void
debug_oacc_loop (oacc_loop *loop)
{
  dump_oacc_loop (stderr, loop, 0);
}

/* DFS walk of basic blocks BB onwards, creating OpenACC loop
   structures as we go.  By construction these loops are properly
   nested.  */

static void
oacc_loop_discover_walk (oacc_loop *loop, basic_block bb)
{
  int marker = 0;
  int remaining = 0;

  if (bb->flags & BB_VISITED)
    return;

 follow:
  bb->flags |= BB_VISITED;

  /* Scan for loop markers.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (!is_gimple_call (stmt))
	continue;

      gcall *call = as_a <gcall *> (stmt);
      
      /* If this is a routine, make a dummy loop for it.  */
      if (tree decl = gimple_call_fndecl (call))
	if (tree attrs = get_oacc_fn_attrib (decl))
	  {
	    gcc_assert (!marker);
	    new_oacc_loop_routine (loop, call, decl, attrs);
	  }

      if (!gimple_call_internal_p (call))
	continue;

      switch (gimple_call_internal_fn (call))
	{
	default:
	  break;

	case IFN_GOACC_LOOP:
	  /* Count the goacc loop abstraction fns, to determine if the
	     loop was collapsed already.  */
	  loop->ifns++;
	  break;

	case IFN_UNIQUE:
	  enum ifn_unique_kind kind
	    = (enum ifn_unique_kind) (TREE_INT_CST_LOW
				      (gimple_call_arg (call, 0)));
	  if (kind == IFN_UNIQUE_OACC_HEAD_MARK
	      || kind == IFN_UNIQUE_OACC_TAIL_MARK)
	    {
	      if (gimple_call_num_args (call) == 2)
		{
		  gcc_assert (marker && !remaining);
		  marker = 0;
		  if (kind == IFN_UNIQUE_OACC_TAIL_MARK)
		    loop = finish_oacc_loop (loop);
		  else
		    loop->head_end = call;
		}
	      else
		{
		  int count = TREE_INT_CST_LOW (gimple_call_arg (call, 2));

		  if (!marker)
		    {
		      if (kind == IFN_UNIQUE_OACC_HEAD_MARK)
			loop = new_oacc_loop (loop, call);
		      remaining = count;
		    }
		  gcc_assert (count == remaining);
		  if (remaining)
		    {
		      remaining--;
		      if (kind == IFN_UNIQUE_OACC_HEAD_MARK)
			loop->heads[marker] = call;
		      else
			loop->tails[remaining] = call;
		    }
		  marker++;
		}
	    }
	}
    }
  if (remaining || marker)
    {
      bb = single_succ (bb);
      gcc_assert (single_pred_p (bb) && !(bb->flags & BB_VISITED));
      goto follow;
    }

  /* Walk successor blocks.  */
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    oacc_loop_discover_walk (loop, e->dest);
}

/* LOOP is the first sibling.  Reverse the order in place and return
   the new first sibling.  Recurse to child loops.  */

static oacc_loop *
oacc_loop_sibling_nreverse (oacc_loop *loop)
{
  oacc_loop *last = NULL;
  do
    {
      if (loop->child)
	loop->child = oacc_loop_sibling_nreverse  (loop->child);

      oacc_loop *next = loop->sibling;
      loop->sibling = last;
      last = loop;
      loop = next;
    }
  while (loop);

  return last;
}

/* Discover the OpenACC loops marked up by HEAD and TAIL markers for
   the current function.  */

static oacc_loop *
oacc_loop_discovery ()
{
  basic_block bb;
  
  oacc_loop *top = new_oacc_loop_outer (current_function_decl);
  oacc_loop_discover_walk (top, ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* The siblings were constructed in reverse order, reverse them so
     that diagnostics come out in an unsurprising order.  */
  top = oacc_loop_sibling_nreverse (top);

  /* Reset the visited flags.  */
  FOR_ALL_BB_FN (bb, cfun)
    bb->flags &= ~BB_VISITED;

  return top;
}

/* Transform the abstract internal function markers starting at FROM
   to be for partitioning level LEVEL.  Stop when we meet another HEAD
   or TAIL  marker.  */

static void
oacc_loop_xform_head_tail (gcall *from, int level)
{
  enum ifn_unique_kind kind
    = (enum ifn_unique_kind) TREE_INT_CST_LOW (gimple_call_arg (from, 0));
  tree replacement = build_int_cst (unsigned_type_node, level);

  for (gimple_stmt_iterator gsi = gsi_for_stmt (from);;)
    {
      gimple *stmt = gsi_stmt (gsi);
      
      if (is_gimple_call (stmt)
	  && gimple_call_internal_p (stmt)
	  && gimple_call_internal_fn (stmt) == IFN_UNIQUE)
	{
	  enum ifn_unique_kind k
	    = ((enum ifn_unique_kind)
	       TREE_INT_CST_LOW (gimple_call_arg (stmt, 0)));

	  if (k == IFN_UNIQUE_OACC_FORK || k == IFN_UNIQUE_OACC_JOIN)
	    *gimple_call_arg_ptr (stmt, 2) = replacement;
	  else if (k == kind && stmt != from)
	    break;
	}
      else if (is_gimple_call (stmt)
	       && gimple_call_internal_p (stmt)
	       && gimple_call_internal_fn (stmt) == IFN_GOACC_REDUCTION)
	*gimple_call_arg_ptr (stmt, 3) = replacement;

      gsi_next (&gsi);
      while (gsi_end_p (gsi))
	gsi = gsi_start_bb (single_succ (gsi_bb (gsi)));
    }
}

/* Transform the IFN_GOACC_LOOP internal functions by providing the
   determined partitioning mask and chunking argument.  END_MARKER
   points at the end IFN_HEAD_TAIL call intgroducing the loop.  IFNS
   is the number of IFN_GOACC_LOOP calls for the loop.  MASK_ARG is
   the replacement partitioning mask and CHUNK_ARG is the replacement
   chunking arg.  */

static void
oacc_loop_xform_loop (gcall *end_marker, unsigned ifns,
		      tree mask_arg, tree chunk_arg)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (end_marker);
  
  gcc_checking_assert (ifns);
  for (;;)
    {
      for (; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (!is_gimple_call (stmt))
	    continue;

	  gcall *call = as_a <gcall *> (stmt);
      
	  if (!gimple_call_internal_p (call))
	    continue;

	  if (gimple_call_internal_fn (call) != IFN_GOACC_LOOP)
	    continue;

	  *gimple_call_arg_ptr (call, 5) = mask_arg;
	  *gimple_call_arg_ptr (call, 4) = chunk_arg;
	  ifns--;
	  if (!ifns)
	    return;
	}

      /* The LOOP_BOUND ifn could be in the single successor
	 block.  */
      basic_block bb = single_succ (gsi_bb (gsi));
      gsi = gsi_start_bb (bb);
    }
}

/* Process the discovered OpenACC loops, setting the correct
   partitioning level etc.  */

static void
oacc_loop_process (oacc_loop *loop)
{
  if (loop->child)
    oacc_loop_process (loop->child);

  if (loop->mask && !loop->routine)
    {
      int ix;
      unsigned mask = loop->mask;
      unsigned dim = GOMP_DIM_GANG;
      tree mask_arg = build_int_cst (unsigned_type_node, mask);
      tree chunk_arg = loop->chunk_size;

      oacc_loop_xform_loop (loop->head_end, loop->ifns, mask_arg, chunk_arg);

      for (ix = 0; ix != GOMP_DIM_MAX && mask; ix++)
	{
	  while (!(GOMP_DIM_MASK (dim) & mask))
	    dim++;

	  oacc_loop_xform_head_tail (loop->heads[ix], dim);
	  oacc_loop_xform_head_tail (loop->tails[ix], dim);

	  mask ^= GOMP_DIM_MASK (dim);
	}
    }

  if (loop->sibling)
    oacc_loop_process (loop->sibling);
}

/* Walk the OpenACC loop heirarchy checking and assigning the
   programmer-specified partitionings.  OUTER_MASK is the partitioning
   this loop is contained within.  Return mask of partitioning
   encountered.  If any auto loops are discovered, set GOMP_DIM_MAX
   bit.  */

static unsigned
oacc_loop_fixed_partitions (oacc_loop *loop, unsigned outer_mask)
{
  unsigned this_mask = loop->mask;
  unsigned mask_all = 0;
  bool noisy = true;

#ifdef ACCEL_COMPILER
  /* When device_type is supported, we want the device compiler to be
     noisy, if the loop parameters are device_type-specific.  */
  noisy = false;
#endif

  if (!loop->routine)
    {
      bool auto_par = (loop->flags & OLF_AUTO) != 0;
      bool seq_par = (loop->flags & OLF_SEQ) != 0;

      this_mask = ((loop->flags >> OLF_DIM_BASE)
		   & (GOMP_DIM_MASK (GOMP_DIM_MAX) - 1));

      if ((this_mask != 0) + auto_par + seq_par > 1)
	{
	  if (noisy)
	    error_at (loop->loc,
		      seq_par
		      ? "%<seq%> overrides other OpenACC loop specifiers"
		      : "%<auto%> conflicts with other OpenACC loop specifiers");
	  auto_par = false;
	  loop->flags &= ~OLF_AUTO;
	  if (seq_par)
	    {
	      loop->flags &=
		~((GOMP_DIM_MASK (GOMP_DIM_MAX) - 1) << OLF_DIM_BASE);
	      this_mask = 0;
	    }
	}
      if (auto_par && (loop->flags & OLF_INDEPENDENT))
	mask_all |= GOMP_DIM_MASK (GOMP_DIM_MAX);
    }

  if (this_mask & outer_mask)
    {
      const oacc_loop *outer;
      for (outer = loop->parent; outer; outer = outer->parent)
	if (outer->mask & this_mask)
	  break;

      if (noisy)
	{
	  if (outer)
	    {
	      error_at (loop->loc,
			"%s uses same OpenACC parallelism as containing loop",
			loop->routine ? "routine call" : "inner loop");
	      inform (outer->loc, "containing loop here");
	    }
	  else
	    error_at (loop->loc,
		      "%s uses OpenACC parallelism disallowed by containing routine",
		      loop->routine ? "routine call" : "loop");
      
	  if (loop->routine)
	    inform (DECL_SOURCE_LOCATION (loop->routine),
		    "routine %qD declared here", loop->routine);
	}
      this_mask &= ~outer_mask;
    }
  else
    {
      unsigned outermost = this_mask & -this_mask;

      if (outermost && outermost <= outer_mask)
	{
	  if (noisy)
	    {
	      error_at (loop->loc,
			"incorrectly nested OpenACC loop parallelism");

	      const oacc_loop *outer;
	      for (outer = loop->parent;
		   outer->flags && outer->flags < outermost;
		   outer = outer->parent)
		continue;
	      inform (outer->loc, "containing loop here");
	    }

	  this_mask &= ~outermost;
	}
    }

  loop->mask = this_mask;
  mask_all |= this_mask;
  
  if (loop->child)
    {
      loop->inner = oacc_loop_fixed_partitions (loop->child,
						outer_mask | this_mask); 
      mask_all |= loop->inner;
    }

  if (loop->sibling)
    mask_all |= oacc_loop_fixed_partitions (loop->sibling, outer_mask);

  return mask_all;
}

/* Walk the OpenACC loop heirarchy to assign auto-partitioned loops.
   OUTER_MASK is the partitioning this loop is contained within.
   Return the cumulative partitioning used by this loop, siblings and
   children.  */

static unsigned
oacc_loop_auto_partitions (oacc_loop *loop, unsigned outer_mask)
{
  bool assign = (loop->flags & OLF_AUTO) && (loop->flags & OLF_INDEPENDENT);
  bool noisy = true;

#ifdef ACCEL_COMPILER
  /* When device_type is supported, we want the device compiler to be
     noisy, if the loop parameters are device_type-specific.  */
  noisy = false;
#endif

  if (assign && outer_mask < GOMP_DIM_MASK (GOMP_DIM_MAX - 1))
    {
      /* Allocate the outermost loop at the outermost available
	 level.  */
      unsigned this_mask = outer_mask + 1;

      if (!(this_mask & loop->inner))
	loop->mask = this_mask;
    }

  if (loop->child)
    {
      unsigned child_mask = outer_mask | loop->mask;

      if (loop->mask || assign)
	child_mask |= GOMP_DIM_MASK (GOMP_DIM_MAX);

      loop->inner = oacc_loop_auto_partitions (loop->child, child_mask);
    }

  if (assign && !loop->mask)
    {
      /* Allocate the loop at the innermost available level.  */
      unsigned this_mask = 0;
      
      /* Determine the outermost partitioning used within this loop. */
      this_mask = loop->inner | GOMP_DIM_MASK (GOMP_DIM_MAX);
      this_mask = (this_mask & -this_mask);

      /* Pick the partitioning just inside that one.  */
      this_mask >>= 1;

      /* And avoid picking one use by an outer loop. */
      this_mask &= ~outer_mask;

      if (!this_mask && noisy)
	warning_at (loop->loc, 0,
		    "insufficient partitioning available to parallelize loop");

      loop->mask = this_mask;
    }

  if (assign && dump_file)
    fprintf (dump_file, "Auto loop %s:%d assigned %d\n",
	     LOCATION_FILE (loop->loc), LOCATION_LINE (loop->loc),
	     loop->mask);

  unsigned inner_mask = 0;
  
  if (loop->sibling)
    inner_mask |= oacc_loop_auto_partitions (loop->sibling, outer_mask);
  
  inner_mask |= loop->inner | loop->mask;

  return inner_mask;
}

/* Walk the OpenACC loop heirarchy to check and assign partitioning
   axes.  Return mask of partitioning.  */

static unsigned
oacc_loop_partition (oacc_loop *loop, unsigned outer_mask)
{
  unsigned mask_all = oacc_loop_fixed_partitions (loop, outer_mask);

  if (mask_all & GOMP_DIM_MASK (GOMP_DIM_MAX))
    {
      mask_all ^= GOMP_DIM_MASK (GOMP_DIM_MAX);
      mask_all |= oacc_loop_auto_partitions (loop, outer_mask);
    }
  return mask_all;
}

/* Default fork/join early expander.  Delete the function calls if
   there is no RTL expander.  */

bool
default_goacc_fork_join (gcall *ARG_UNUSED (call),
			 const int *ARG_UNUSED (dims), bool is_fork)
{
  if (is_fork)
    return targetm.have_oacc_fork ();
  else
    return targetm.have_oacc_join ();
}

/* Default goacc.reduction early expander.

   LHS-opt = IFN_REDUCTION (KIND, RES_PTR, VAR, LEVEL, OP, OFFSET)
   If RES_PTR is not integer-zerop:
       SETUP - emit 'LHS = *RES_PTR', LHS = NULL
       TEARDOWN - emit '*RES_PTR = VAR'
   If LHS is not NULL
       emit 'LHS = VAR'   */

void
default_goacc_reduction (gcall *call)
{
  unsigned code = (unsigned)TREE_INT_CST_LOW (gimple_call_arg (call, 0));
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  gimple_seq seq = NULL;

  if (code == IFN_GOACC_REDUCTION_SETUP
      || code == IFN_GOACC_REDUCTION_TEARDOWN)
    {
      /* Setup and Teardown need to copy from/to the receiver object,
	 if there is one.  */
      tree ref_to_res = gimple_call_arg (call, 1);
      
      if (!integer_zerop (ref_to_res))
	{
	  tree dst = build_simple_mem_ref (ref_to_res);
	  tree src = var;
	  
	  if (code == IFN_GOACC_REDUCTION_SETUP)
	    {
	      src = dst;
	      dst = lhs;
	      lhs = NULL;
	    }
	  gimple_seq_add_stmt (&seq, gimple_build_assign (dst, src));
	}
    }

  /* Copy VAR to LHS, if there is an LHS.  */
  if (lhs)
    gimple_seq_add_stmt (&seq, gimple_build_assign (lhs, var));

  gsi_replace_with_seq (&gsi, seq, true);
}

/* Main entry point for oacc transformations which run on the device
   compiler after LTO, so we know what the target device is at this
   point (including the host fallback).  */

static unsigned int
execute_oacc_device_lower ()
{
  tree attrs = get_oacc_fn_attrib (current_function_decl);
  
  if (!attrs)
    /* Not an offloaded function.  */
    return 0;

  /* Parse the default dim argument exactly once.  */
  if ((const void *)flag_openacc_dims != &flag_openacc_dims)
    {
      oacc_parse_default_dims (flag_openacc_dims);
      flag_openacc_dims = (char *)&flag_openacc_dims;
    } 

  /* Discover, partition and process the loops.  */
  oacc_loop *loops = oacc_loop_discovery ();
  int fn_level = oacc_fn_attrib_level (attrs);

  if (dump_file)
    fprintf (dump_file, oacc_fn_attrib_kernels_p (attrs)
	     ? "Function is kernels offload\n"
	     : fn_level < 0 ? "Function is parallel offload\n"
	     : "Function is routine level %d\n", fn_level);

  unsigned outer_mask = fn_level >= 0 ? GOMP_DIM_MASK (fn_level) - 1 : 0;
  unsigned used_mask = oacc_loop_partition (loops, outer_mask);
  int dims[GOMP_DIM_MAX];

  oacc_validate_dims (current_function_decl, attrs, dims, fn_level, used_mask);

  if (dump_file)
    {
      const char *comma = "Compute dimensions [";
      for (int ix = 0; ix != GOMP_DIM_MAX; ix++, comma = ", ")
	fprintf (dump_file, "%s%d", comma, dims[ix]);
      fprintf (dump_file, "]\n");
    }

  oacc_loop_process (loops);
  if (dump_file)
    {
      fprintf (dump_file, "OpenACC loops\n");
      dump_oacc_loop (dump_file, loops, 0);
      fprintf (dump_file, "\n");
    }

  /* Offloaded targets may introduce new basic blocks, which require
     dominance information to update SSA.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Now lower internal loop functions to target-specific code
     sequences.  */
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
      {
	gimple *stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt))
	  {
	    gsi_next (&gsi);
	    continue;
	  }

	gcall *call = as_a <gcall *> (stmt);
	if (!gimple_call_internal_p (call))
	  {
	    gsi_next (&gsi);
	    continue;
	  }

	/* Rewind to allow rescan.  */
	gsi_prev (&gsi);
	bool rescan = false, remove = false;
	enum  internal_fn ifn_code = gimple_call_internal_fn (call);

	switch (ifn_code)
	  {
	  default: break;

	  case IFN_GOACC_LOOP:
	    oacc_xform_loop (call);
	    rescan = true;
	    break;

	  case IFN_GOACC_REDUCTION:
	    /* Mark the function for SSA renaming.  */
	    mark_virtual_operands_for_renaming (cfun);

	    /* If the level is -1, this ended up being an unused
	       axis.  Handle as a default.  */
	    if (integer_minus_onep (gimple_call_arg (call, 3)))
	      default_goacc_reduction (call);
	    else
	      targetm.goacc.reduction (call);
	    rescan = true;
	    break;

	  case IFN_UNIQUE:
	    {
	      enum ifn_unique_kind kind
		= ((enum ifn_unique_kind)
		   TREE_INT_CST_LOW (gimple_call_arg (call, 0)));

	      switch (kind)
		{
		default:
		  gcc_unreachable ();

		case IFN_UNIQUE_OACC_FORK:
		case IFN_UNIQUE_OACC_JOIN:
		  if (integer_minus_onep (gimple_call_arg (call, 2)))
		    remove = true;
		  else if (!targetm.goacc.fork_join
			   (call, dims, kind == IFN_UNIQUE_OACC_FORK))
		    remove = true;
		  break;

		case IFN_UNIQUE_OACC_HEAD_MARK:
		case IFN_UNIQUE_OACC_TAIL_MARK:
		  remove = true;
		  break;
		}
	      break;
	    }
	  }

	if (gsi_end_p (gsi))
	  /* We rewound past the beginning of the BB.  */
	  gsi = gsi_start_bb (bb);
	else
	  /* Undo the rewind.  */
	  gsi_next (&gsi);

	if (remove)
	  {
	    if (gimple_vdef (call))
	      replace_uses_by (gimple_vdef (call), gimple_vuse (call));
	    if (gimple_call_lhs (call))
	      {
		/* Propagate the data dependency var.  */
		gimple *ass = gimple_build_assign (gimple_call_lhs (call),
						   gimple_call_arg (call, 1));
		gsi_replace (&gsi, ass,  false);
	      }
	    else
	      gsi_remove (&gsi, true);
	  }
	else if (!rescan)
	  /* If not rescanning, advance over the call.  */
	  gsi_next (&gsi);
      }

  free_oacc_loop (loops);

  return 0;
}

/* Default launch dimension validator.  Force everything to 1.  A
   backend that wants to provide larger dimensions must override this
   hook.  */

bool
default_goacc_validate_dims (tree ARG_UNUSED (decl), int *dims,
			     int ARG_UNUSED (fn_level))
{
  bool changed = false;

  for (unsigned ix = 0; ix != GOMP_DIM_MAX; ix++)
    {
      if (dims[ix] != 1)
	{
	  dims[ix] = 1;
	  changed = true;
	}
    }

  return changed;
}

/* Default dimension bound is unknown on accelerator and 1 on host. */

int
default_goacc_dim_limit (int ARG_UNUSED (axis))
{
#ifdef ACCEL_COMPILER
  return 0;
#else
  return 1;
#endif
}

namespace {

const pass_data pass_data_oacc_device_lower =
{
  GIMPLE_PASS, /* type */
  "oaccdevlow", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0 /* Possibly PROP_gimple_eomp.  */, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_oacc_device_lower : public gimple_opt_pass
{
public:
  pass_oacc_device_lower (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_oacc_device_lower, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      bool gate = flag_openacc != 0;

      if (!gate)
	return 0;

      return execute_oacc_device_lower ();
    }

}; // class pass_oacc_device_lower

} // anon namespace

gimple_opt_pass *
make_pass_oacc_device_lower (gcc::context *ctxt)
{
  return new pass_oacc_device_lower (ctxt);
}

/* "omp declare target link" handling pass.  */

namespace {

const pass_data pass_data_omp_target_link =
{
  GIMPLE_PASS,			/* type */
  "omptargetlink",		/* name */
  OPTGROUP_NONE,		/* optinfo_flags */
  TV_NONE,			/* tv_id */
  PROP_ssa,			/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_update_ssa,		/* todo_flags_finish */
};

class pass_omp_target_link : public gimple_opt_pass
{
public:
  pass_omp_target_link (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_omp_target_link, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fun)
    {
#ifdef ACCEL_COMPILER
      tree attrs = DECL_ATTRIBUTES (fun->decl);
      return lookup_attribute ("omp declare target", attrs)
	     || lookup_attribute ("omp target entrypoint", attrs);
#else
      (void) fun;
      return false;
#endif
    }

  virtual unsigned execute (function *);
};

/* Callback for walk_gimple_stmt used to scan for link var operands.  */

static tree
find_link_var_op (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;

  if (TREE_CODE (t) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (t)
      && lookup_attribute ("omp declare target link", DECL_ATTRIBUTES (t)))
    {
      *walk_subtrees = 0;
      return t;
    }

  return NULL_TREE;
}

unsigned
pass_omp_target_link::execute (function *fun)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if (walk_gimple_stmt (&gsi, NULL, find_link_var_op, NULL))
	  gimple_regimplify_operands (gsi_stmt (gsi), &gsi);
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_omp_target_link (gcc::context *ctxt)
{
  return new pass_omp_target_link (ctxt);
}

#include "gt-omp-low.h"
