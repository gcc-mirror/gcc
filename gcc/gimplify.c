/* Tree lowering pass.  This pass converts the GENERIC functions-as-trees
   tree representation into the GIMPLE form.
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
   Major work done by Sebastian Pop <s.pop@laposte.net>,
   Diego Novillo <dnovillo@redhat.com> and Jason Merrill <jason@redhat.com>.

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
#include "memmodel.h"
#include "tm_p.h"
#include "gimple.h"
#include "gimple-predict.h"
#include "tree-pass.h"		/* FIXME: only for PROP_gimple_any */
#include "ssa.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "calls.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "omp-general.h"
#include "omp-low.h"
#include "gimple-low.h"
#include "gomp-constants.h"
#include "splay-tree.h"
#include "gimple-walk.h"
#include "langhooks-def.h"	/* FIXME: for lhd_set_decl_assembler_name */
#include "builtins.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "dbgcnt.h"

/* Hash set of poisoned variables in a bind expr.  */
static hash_set<tree> *asan_poisoned_variables = NULL;

enum gimplify_omp_var_data
{
  GOVD_SEEN = 1,
  GOVD_EXPLICIT = 2,
  GOVD_SHARED = 4,
  GOVD_PRIVATE = 8,
  GOVD_FIRSTPRIVATE = 16,
  GOVD_LASTPRIVATE = 32,
  GOVD_REDUCTION = 64,
  GOVD_LOCAL = 128,
  GOVD_MAP = 256,
  GOVD_DEBUG_PRIVATE = 512,
  GOVD_PRIVATE_OUTER_REF = 1024,
  GOVD_LINEAR = 2048,
  GOVD_ALIGNED = 4096,

  /* Flag for GOVD_MAP: don't copy back.  */
  GOVD_MAP_TO_ONLY = 8192,

  /* Flag for GOVD_LINEAR or GOVD_LASTPRIVATE: no outer reference.  */
  GOVD_LINEAR_LASTPRIVATE_NO_OUTER = 16384,

  GOVD_MAP_0LEN_ARRAY = 32768,

  /* Flag for GOVD_MAP, if it is always, to or always, tofrom mapping.  */
  GOVD_MAP_ALWAYS_TO = 65536,

  /* Flag for shared vars that are or might be stored to in the region.  */
  GOVD_WRITTEN = 131072,

  /* Flag for GOVD_MAP, if it is a forced mapping.  */
  GOVD_MAP_FORCE = 262144,

  /* Flag for GOVD_MAP: must be present already.  */
  GOVD_MAP_FORCE_PRESENT = 524288,

  GOVD_DATA_SHARE_CLASS = (GOVD_SHARED | GOVD_PRIVATE | GOVD_FIRSTPRIVATE
			   | GOVD_LASTPRIVATE | GOVD_REDUCTION | GOVD_LINEAR
			   | GOVD_LOCAL)
};


enum omp_region_type
{
  ORT_WORKSHARE = 0x00,
  ORT_SIMD 	= 0x01,

  ORT_PARALLEL	= 0x02,
  ORT_COMBINED_PARALLEL = 0x03,

  ORT_TASK	= 0x04,
  ORT_UNTIED_TASK = 0x05,

  ORT_TEAMS	= 0x08,
  ORT_COMBINED_TEAMS = 0x09,

  /* Data region.  */
  ORT_TARGET_DATA = 0x10,

  /* Data region with offloading.  */
  ORT_TARGET	= 0x20,
  ORT_COMBINED_TARGET = 0x21,

  /* OpenACC variants.  */
  ORT_ACC	= 0x40,  /* A generic OpenACC region.  */
  ORT_ACC_DATA	= ORT_ACC | ORT_TARGET_DATA, /* Data construct.  */
  ORT_ACC_PARALLEL = ORT_ACC | ORT_TARGET,  /* Parallel construct */
  ORT_ACC_KERNELS  = ORT_ACC | ORT_TARGET | 0x80,  /* Kernels construct.  */
  ORT_ACC_HOST_DATA = ORT_ACC | ORT_TARGET_DATA | 0x80,  /* Host data.  */

  /* Dummy OpenMP region, used to disable expansion of
     DECL_VALUE_EXPRs in taskloop pre body.  */
  ORT_NONE	= 0x100
};

/* Gimplify hashtable helper.  */

struct gimplify_hasher : free_ptr_hash <elt_t>
{
  static inline hashval_t hash (const elt_t *);
  static inline bool equal (const elt_t *, const elt_t *);
};

struct gimplify_ctx
{
  struct gimplify_ctx *prev_context;

  vec<gbind *> bind_expr_stack;
  tree temps;
  gimple_seq conditional_cleanups;
  tree exit_label;
  tree return_temp;

  vec<tree> case_labels;
  hash_set<tree> *live_switch_vars;
  /* The formal temporary table.  Should this be persistent?  */
  hash_table<gimplify_hasher> *temp_htab;

  int conditions;
  unsigned into_ssa : 1;
  unsigned allow_rhs_cond_expr : 1;
  unsigned in_cleanup_point_expr : 1;
  unsigned keep_stack : 1;
  unsigned save_stack : 1;
  unsigned in_switch_expr : 1;
};

struct gimplify_omp_ctx
{
  struct gimplify_omp_ctx *outer_context;
  splay_tree variables;
  hash_set<tree> *privatized_types;
  /* Iteration variables in an OMP_FOR.  */
  vec<tree> loop_iter_var;
  location_t location;
  enum omp_clause_default_kind default_kind;
  enum omp_region_type region_type;
  bool combined_loop;
  bool distribute;
  bool target_map_scalars_firstprivate;
  bool target_map_pointers_as_0len_arrays;
  bool target_firstprivatize_array_bases;
};

static struct gimplify_ctx *gimplify_ctxp;
static struct gimplify_omp_ctx *gimplify_omp_ctxp;

/* Forward declaration.  */
static enum gimplify_status gimplify_compound_expr (tree *, gimple_seq *, bool);
static hash_map<tree, tree> *oacc_declare_returns;
static enum gimplify_status gimplify_expr (tree *, gimple_seq *, gimple_seq *,
					   bool (*) (tree), fallback_t, bool);

/* Shorter alias name for the above function for use in gimplify.c
   only.  */

static inline void
gimplify_seq_add_stmt (gimple_seq *seq_p, gimple *gs)
{
  gimple_seq_add_stmt_without_update (seq_p, gs);
}

/* Append sequence SRC to the end of sequence *DST_P.  If *DST_P is
   NULL, a new sequence is allocated.   This function is
   similar to gimple_seq_add_seq, but does not scan the operands.
   During gimplification, we need to manipulate statement sequences
   before the def/use vectors have been constructed.  */

static void
gimplify_seq_add_seq (gimple_seq *dst_p, gimple_seq src)
{
  gimple_stmt_iterator si;

  if (src == NULL)
    return;

  si = gsi_last (*dst_p);
  gsi_insert_seq_after_without_update (&si, src, GSI_NEW_STMT);
}


/* Pointer to a list of allocated gimplify_ctx structs to be used for pushing
   and popping gimplify contexts.  */

static struct gimplify_ctx *ctx_pool = NULL;

/* Return a gimplify context struct from the pool.  */

static inline struct gimplify_ctx *
ctx_alloc (void)
{
  struct gimplify_ctx * c = ctx_pool;

  if (c)
    ctx_pool = c->prev_context;
  else
    c = XNEW (struct gimplify_ctx);

  memset (c, '\0', sizeof (*c));
  return c;
}

/* Put gimplify context C back into the pool.  */

static inline void
ctx_free (struct gimplify_ctx *c)
{
  c->prev_context = ctx_pool;
  ctx_pool = c;
}

/* Free allocated ctx stack memory.  */

void
free_gimplify_stack (void)
{
  struct gimplify_ctx *c;

  while ((c = ctx_pool))
    {
      ctx_pool = c->prev_context;
      free (c);
    }
}


/* Set up a context for the gimplifier.  */

void
push_gimplify_context (bool in_ssa, bool rhs_cond_ok)
{
  struct gimplify_ctx *c = ctx_alloc ();

  c->prev_context = gimplify_ctxp;
  gimplify_ctxp = c;
  gimplify_ctxp->into_ssa = in_ssa;
  gimplify_ctxp->allow_rhs_cond_expr = rhs_cond_ok;
}

/* Tear down a context for the gimplifier.  If BODY is non-null, then
   put the temporaries into the outer BIND_EXPR.  Otherwise, put them
   in the local_decls.

   BODY is not a sequence, but the first tuple in a sequence.  */

void
pop_gimplify_context (gimple *body)
{
  struct gimplify_ctx *c = gimplify_ctxp;

  gcc_assert (c
              && (!c->bind_expr_stack.exists ()
		  || c->bind_expr_stack.is_empty ()));
  c->bind_expr_stack.release ();
  gimplify_ctxp = c->prev_context;

  if (body)
    declare_vars (c->temps, body, false);
  else
    record_vars (c->temps);

  delete c->temp_htab;
  c->temp_htab = NULL;
  ctx_free (c);
}

/* Push a GIMPLE_BIND tuple onto the stack of bindings.  */

static void
gimple_push_bind_expr (gbind *bind_stmt)
{
  gimplify_ctxp->bind_expr_stack.reserve (8);
  gimplify_ctxp->bind_expr_stack.safe_push (bind_stmt);
}

/* Pop the first element off the stack of bindings.  */

static void
gimple_pop_bind_expr (void)
{
  gimplify_ctxp->bind_expr_stack.pop ();
}

/* Return the first element of the stack of bindings.  */

gbind *
gimple_current_bind_expr (void)
{
  return gimplify_ctxp->bind_expr_stack.last ();
}

/* Return the stack of bindings created during gimplification.  */

vec<gbind *>
gimple_bind_expr_stack (void)
{
  return gimplify_ctxp->bind_expr_stack;
}

/* Return true iff there is a COND_EXPR between us and the innermost
   CLEANUP_POINT_EXPR.  This info is used by gimple_push_cleanup.  */

static bool
gimple_conditional_context (void)
{
  return gimplify_ctxp->conditions > 0;
}

/* Note that we've entered a COND_EXPR.  */

static void
gimple_push_condition (void)
{
#ifdef ENABLE_GIMPLE_CHECKING
  if (gimplify_ctxp->conditions == 0)
    gcc_assert (gimple_seq_empty_p (gimplify_ctxp->conditional_cleanups));
#endif
  ++(gimplify_ctxp->conditions);
}

/* Note that we've left a COND_EXPR.  If we're back at unconditional scope
   now, add any conditional cleanups we've seen to the prequeue.  */

static void
gimple_pop_condition (gimple_seq *pre_p)
{
  int conds = --(gimplify_ctxp->conditions);

  gcc_assert (conds >= 0);
  if (conds == 0)
    {
      gimplify_seq_add_seq (pre_p, gimplify_ctxp->conditional_cleanups);
      gimplify_ctxp->conditional_cleanups = NULL;
    }
}

/* A stable comparison routine for use with splay trees and DECLs.  */

static int
splay_tree_compare_decl_uid (splay_tree_key xa, splay_tree_key xb)
{
  tree a = (tree) xa;
  tree b = (tree) xb;

  return DECL_UID (a) - DECL_UID (b);
}

/* Create a new omp construct that deals with variable remapping.  */

static struct gimplify_omp_ctx *
new_omp_context (enum omp_region_type region_type)
{
  struct gimplify_omp_ctx *c;

  c = XCNEW (struct gimplify_omp_ctx);
  c->outer_context = gimplify_omp_ctxp;
  c->variables = splay_tree_new (splay_tree_compare_decl_uid, 0, 0);
  c->privatized_types = new hash_set<tree>;
  c->location = input_location;
  c->region_type = region_type;
  if ((region_type & ORT_TASK) == 0)
    c->default_kind = OMP_CLAUSE_DEFAULT_SHARED;
  else
    c->default_kind = OMP_CLAUSE_DEFAULT_UNSPECIFIED;

  return c;
}

/* Destroy an omp construct that deals with variable remapping.  */

static void
delete_omp_context (struct gimplify_omp_ctx *c)
{
  splay_tree_delete (c->variables);
  delete c->privatized_types;
  c->loop_iter_var.release ();
  XDELETE (c);
}

static void omp_add_variable (struct gimplify_omp_ctx *, tree, unsigned int);
static bool omp_notice_variable (struct gimplify_omp_ctx *, tree, bool);

/* Both gimplify the statement T and append it to *SEQ_P.  This function
   behaves exactly as gimplify_stmt, but you don't have to pass T as a
   reference.  */

void
gimplify_and_add (tree t, gimple_seq *seq_p)
{
  gimplify_stmt (&t, seq_p);
}

/* Gimplify statement T into sequence *SEQ_P, and return the first
   tuple in the sequence of generated tuples for this statement.
   Return NULL if gimplifying T produced no tuples.  */

static gimple *
gimplify_and_return_first (tree t, gimple_seq *seq_p)
{
  gimple_stmt_iterator last = gsi_last (*seq_p);

  gimplify_and_add (t, seq_p);

  if (!gsi_end_p (last))
    {
      gsi_next (&last);
      return gsi_stmt (last);
    }
  else
    return gimple_seq_first_stmt (*seq_p);
}

/* Returns true iff T is a valid RHS for an assignment to an un-renamed
   LHS, or for a call argument.  */

static bool
is_gimple_mem_rhs (tree t)
{
  /* If we're dealing with a renamable type, either source or dest must be
     a renamed variable.  */
  if (is_gimple_reg_type (TREE_TYPE (t)))
    return is_gimple_val (t);
  else
    return is_gimple_val (t) || is_gimple_lvalue (t);
}

/* Return true if T is a CALL_EXPR or an expression that can be
   assigned to a temporary.  Note that this predicate should only be
   used during gimplification.  See the rationale for this in
   gimplify_modify_expr.  */

static bool
is_gimple_reg_rhs_or_call (tree t)
{
  return (get_gimple_rhs_class (TREE_CODE (t)) != GIMPLE_INVALID_RHS
	  || TREE_CODE (t) == CALL_EXPR);
}

/* Return true if T is a valid memory RHS or a CALL_EXPR.  Note that
   this predicate should only be used during gimplification.  See the
   rationale for this in gimplify_modify_expr.  */

static bool
is_gimple_mem_rhs_or_call (tree t)
{
  /* If we're dealing with a renamable type, either source or dest must be
     a renamed variable.  */
  if (is_gimple_reg_type (TREE_TYPE (t)))
    return is_gimple_val (t);
  else
    return (is_gimple_val (t)
	    || is_gimple_lvalue (t)
	    || TREE_CLOBBER_P (t)
	    || TREE_CODE (t) == CALL_EXPR);
}

/* Create a temporary with a name derived from VAL.  Subroutine of
   lookup_tmp_var; nobody else should call this function.  */

static inline tree
create_tmp_from_val (tree val)
{
  /* Drop all qualifiers and address-space information from the value type.  */
  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (val));
  tree var = create_tmp_var (type, get_name (val));
  if (TREE_CODE (TREE_TYPE (var)) == COMPLEX_TYPE
      || TREE_CODE (TREE_TYPE (var)) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (var) = 1;
  return var;
}

/* Create a temporary to hold the value of VAL.  If IS_FORMAL, try to reuse
   an existing expression temporary.  */

static tree
lookup_tmp_var (tree val, bool is_formal)
{
  tree ret;

  /* If not optimizing, never really reuse a temporary.  local-alloc
     won't allocate any variable that is used in more than one basic
     block, which means it will go into memory, causing much extra
     work in reload and final and poorer code generation, outweighing
     the extra memory allocation here.  */
  if (!optimize || !is_formal || TREE_SIDE_EFFECTS (val))
    ret = create_tmp_from_val (val);
  else
    {
      elt_t elt, *elt_p;
      elt_t **slot;

      elt.val = val;
      if (!gimplify_ctxp->temp_htab)
        gimplify_ctxp->temp_htab = new hash_table<gimplify_hasher> (1000);
      slot = gimplify_ctxp->temp_htab->find_slot (&elt, INSERT);
      if (*slot == NULL)
	{
	  elt_p = XNEW (elt_t);
	  elt_p->val = val;
	  elt_p->temp = ret = create_tmp_from_val (val);
	  *slot = elt_p;
	}
      else
	{
	  elt_p = *slot;
          ret = elt_p->temp;
	}
    }

  return ret;
}

/* Helper for get_formal_tmp_var and get_initialized_tmp_var.  */

static tree
internal_get_tmp_var (tree val, gimple_seq *pre_p, gimple_seq *post_p,
                      bool is_formal, bool allow_ssa)
{
  tree t, mod;

  /* Notice that we explicitly allow VAL to be a CALL_EXPR so that we
     can create an INIT_EXPR and convert it into a GIMPLE_CALL below.  */
  gimplify_expr (&val, pre_p, post_p, is_gimple_reg_rhs_or_call,
		 fb_rvalue);

  if (allow_ssa
      && gimplify_ctxp->into_ssa
      && is_gimple_reg_type (TREE_TYPE (val)))
    {
      t = make_ssa_name (TYPE_MAIN_VARIANT (TREE_TYPE (val)));
      if (! gimple_in_ssa_p (cfun))
	{
	  const char *name = get_name (val);
	  if (name)
	    SET_SSA_NAME_VAR_OR_IDENTIFIER (t, create_tmp_var_name (name));
	}
    }
  else
    t = lookup_tmp_var (val, is_formal);

  mod = build2 (INIT_EXPR, TREE_TYPE (t), t, unshare_expr (val));

  SET_EXPR_LOCATION (mod, EXPR_LOC_OR_LOC (val, input_location));

  /* gimplify_modify_expr might want to reduce this further.  */
  gimplify_and_add (mod, pre_p);
  ggc_free (mod);

  return t;
}

/* Return a formal temporary variable initialized with VAL.  PRE_P is as
   in gimplify_expr.  Only use this function if:

   1) The value of the unfactored expression represented by VAL will not
      change between the initialization and use of the temporary, and
   2) The temporary will not be otherwise modified.

   For instance, #1 means that this is inappropriate for SAVE_EXPR temps,
   and #2 means it is inappropriate for && temps.

   For other cases, use get_initialized_tmp_var instead.  */

tree
get_formal_tmp_var (tree val, gimple_seq *pre_p)
{
  return internal_get_tmp_var (val, pre_p, NULL, true, true);
}

/* Return a temporary variable initialized with VAL.  PRE_P and POST_P
   are as in gimplify_expr.  */

tree
get_initialized_tmp_var (tree val, gimple_seq *pre_p, gimple_seq *post_p,
			 bool allow_ssa)
{
  return internal_get_tmp_var (val, pre_p, post_p, false, allow_ssa);
}

/* Declare all the variables in VARS in SCOPE.  If DEBUG_INFO is true,
   generate debug info for them; otherwise don't.  */

void
declare_vars (tree vars, gimple *gs, bool debug_info)
{
  tree last = vars;
  if (last)
    {
      tree temps, block;

      gbind *scope = as_a <gbind *> (gs);

      temps = nreverse (last);

      block = gimple_bind_block (scope);
      gcc_assert (!block || TREE_CODE (block) == BLOCK);
      if (!block || !debug_info)
	{
	  DECL_CHAIN (last) = gimple_bind_vars (scope);
	  gimple_bind_set_vars (scope, temps);
	}
      else
	{
	  /* We need to attach the nodes both to the BIND_EXPR and to its
	     associated BLOCK for debugging purposes.  The key point here
	     is that the BLOCK_VARS of the BIND_EXPR_BLOCK of a BIND_EXPR
	     is a subchain of the BIND_EXPR_VARS of the BIND_EXPR.  */
	  if (BLOCK_VARS (block))
	    BLOCK_VARS (block) = chainon (BLOCK_VARS (block), temps);
	  else
	    {
	      gimple_bind_set_vars (scope,
	      			    chainon (gimple_bind_vars (scope), temps));
	      BLOCK_VARS (block) = temps;
	    }
	}
    }
}

/* For VAR a VAR_DECL of variable size, try to find a constant upper bound
   for the size and adjust DECL_SIZE/DECL_SIZE_UNIT accordingly.  Abort if
   no such upper bound can be obtained.  */

static void
force_constant_size (tree var)
{
  /* The only attempt we make is by querying the maximum size of objects
     of the variable's type.  */

  HOST_WIDE_INT max_size;

  gcc_assert (VAR_P (var));

  max_size = max_int_size_in_bytes (TREE_TYPE (var));

  gcc_assert (max_size >= 0);

  DECL_SIZE_UNIT (var)
    = build_int_cst (TREE_TYPE (DECL_SIZE_UNIT (var)), max_size);
  DECL_SIZE (var)
    = build_int_cst (TREE_TYPE (DECL_SIZE (var)), max_size * BITS_PER_UNIT);
}

/* Push the temporary variable TMP into the current binding.  */

void
gimple_add_tmp_var_fn (struct function *fn, tree tmp)
{
  gcc_assert (!DECL_CHAIN (tmp) && !DECL_SEEN_IN_BIND_EXPR_P (tmp));

  /* Later processing assumes that the object size is constant, which might
     not be true at this point.  Force the use of a constant upper bound in
     this case.  */
  if (!tree_fits_poly_uint64_p (DECL_SIZE_UNIT (tmp)))
    force_constant_size (tmp);

  DECL_CONTEXT (tmp) = fn->decl;
  DECL_SEEN_IN_BIND_EXPR_P (tmp) = 1;

  record_vars_into (tmp, fn->decl);
}

/* Push the temporary variable TMP into the current binding.  */

void
gimple_add_tmp_var (tree tmp)
{
  gcc_assert (!DECL_CHAIN (tmp) && !DECL_SEEN_IN_BIND_EXPR_P (tmp));

  /* Later processing assumes that the object size is constant, which might
     not be true at this point.  Force the use of a constant upper bound in
     this case.  */
  if (!tree_fits_poly_uint64_p (DECL_SIZE_UNIT (tmp)))
    force_constant_size (tmp);

  DECL_CONTEXT (tmp) = current_function_decl;
  DECL_SEEN_IN_BIND_EXPR_P (tmp) = 1;

  if (gimplify_ctxp)
    {
      DECL_CHAIN (tmp) = gimplify_ctxp->temps;
      gimplify_ctxp->temps = tmp;

      /* Mark temporaries local within the nearest enclosing parallel.  */
      if (gimplify_omp_ctxp)
	{
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
	  while (ctx
		 && (ctx->region_type == ORT_WORKSHARE
		     || ctx->region_type == ORT_SIMD
		     || ctx->region_type == ORT_ACC))
	    ctx = ctx->outer_context;
	  if (ctx)
	    omp_add_variable (ctx, tmp, GOVD_LOCAL | GOVD_SEEN);
	}
    }
  else if (cfun)
    record_vars (tmp);
  else
    {
      gimple_seq body_seq;

      /* This case is for nested functions.  We need to expose the locals
	 they create.  */
      body_seq = gimple_body (current_function_decl);
      declare_vars (tmp, gimple_seq_first_stmt (body_seq), false);
    }
}



/* This page contains routines to unshare tree nodes, i.e. to duplicate tree
   nodes that are referenced more than once in GENERIC functions.  This is
   necessary because gimplification (translation into GIMPLE) is performed
   by modifying tree nodes in-place, so gimplication of a shared node in a
   first context could generate an invalid GIMPLE form in a second context.

   This is achieved with a simple mark/copy/unmark algorithm that walks the
   GENERIC representation top-down, marks nodes with TREE_VISITED the first
   time it encounters them, duplicates them if they already have TREE_VISITED
   set, and finally removes the TREE_VISITED marks it has set.

   The algorithm works only at the function level, i.e. it generates a GENERIC
   representation of a function with no nodes shared within the function when
   passed a GENERIC function (except for nodes that are allowed to be shared).

   At the global level, it is also necessary to unshare tree nodes that are
   referenced in more than one function, for the same aforementioned reason.
   This requires some cooperation from the front-end.  There are 2 strategies:

     1. Manual unsharing.  The front-end needs to call unshare_expr on every
        expression that might end up being shared across functions.

     2. Deep unsharing.  This is an extension of regular unsharing.  Instead
        of calling unshare_expr on expressions that might be shared across
        functions, the front-end pre-marks them with TREE_VISITED.  This will
        ensure that they are unshared on the first reference within functions
        when the regular unsharing algorithm runs.  The counterpart is that
        this algorithm must look deeper than for manual unsharing, which is
        specified by LANG_HOOKS_DEEP_UNSHARING.

  If there are only few specific cases of node sharing across functions, it is
  probably easier for a front-end to unshare the expressions manually.  On the
  contrary, if the expressions generated at the global level are as widespread
  as expressions generated within functions, deep unsharing is very likely the
  way to go.  */

/* Similar to copy_tree_r but do not copy SAVE_EXPR or TARGET_EXPR nodes.
   These nodes model computations that must be done once.  If we were to
   unshare something like SAVE_EXPR(i++), the gimplification process would
   create wrong code.  However, if DATA is non-null, it must hold a pointer
   set that is used to unshare the subtrees of these nodes.  */

static tree
mostly_copy_tree_r (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;
  enum tree_code code = TREE_CODE (t);

  /* Do not copy SAVE_EXPR, TARGET_EXPR or BIND_EXPR nodes themselves, but
     copy their subtrees if we can make sure to do it only once.  */
  if (code == SAVE_EXPR || code == TARGET_EXPR || code == BIND_EXPR)
    {
      if (data && !((hash_set<tree> *)data)->add (t))
	;
      else
	*walk_subtrees = 0;
    }

  /* Stop at types, decls, constants like copy_tree_r.  */
  else if (TREE_CODE_CLASS (code) == tcc_type
	   || TREE_CODE_CLASS (code) == tcc_declaration
	   || TREE_CODE_CLASS (code) == tcc_constant)
    *walk_subtrees = 0;

  /* Cope with the statement expression extension.  */
  else if (code == STATEMENT_LIST)
    ;

  /* Leave the bulk of the work to copy_tree_r itself.  */
  else
    copy_tree_r (tp, walk_subtrees, NULL);

  return NULL_TREE;
}

/* Callback for walk_tree to unshare most of the shared trees rooted at *TP.
   If *TP has been visited already, then *TP is deeply copied by calling
   mostly_copy_tree_r.  DATA is passed to mostly_copy_tree_r unmodified.  */

static tree
copy_if_shared_r (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;
  enum tree_code code = TREE_CODE (t);

  /* Skip types, decls, and constants.  But we do want to look at their
     types and the bounds of types.  Mark them as visited so we properly
     unmark their subtrees on the unmark pass.  If we've already seen them,
     don't look down further.  */
  if (TREE_CODE_CLASS (code) == tcc_type
      || TREE_CODE_CLASS (code) == tcc_declaration
      || TREE_CODE_CLASS (code) == tcc_constant)
    {
      if (TREE_VISITED (t))
	*walk_subtrees = 0;
      else
	TREE_VISITED (t) = 1;
    }

  /* If this node has been visited already, unshare it and don't look
     any deeper.  */
  else if (TREE_VISITED (t))
    {
      walk_tree (tp, mostly_copy_tree_r, data, NULL);
      *walk_subtrees = 0;
    }

  /* Otherwise, mark the node as visited and keep looking.  */
  else
    TREE_VISITED (t) = 1;

  return NULL_TREE;
}

/* Unshare most of the shared trees rooted at *TP.  DATA is passed to the
   copy_if_shared_r callback unmodified.  */

static inline void
copy_if_shared (tree *tp, void *data)
{
  walk_tree (tp, copy_if_shared_r, data, NULL);
}

/* Unshare all the trees in the body of FNDECL, as well as in the bodies of
   any nested functions.  */

static void
unshare_body (tree fndecl)
{
  struct cgraph_node *cgn = cgraph_node::get (fndecl);
  /* If the language requires deep unsharing, we need a pointer set to make
     sure we don't repeatedly unshare subtrees of unshareable nodes.  */
  hash_set<tree> *visited
    = lang_hooks.deep_unsharing ? new hash_set<tree> : NULL;

  copy_if_shared (&DECL_SAVED_TREE (fndecl), visited);
  copy_if_shared (&DECL_SIZE (DECL_RESULT (fndecl)), visited);
  copy_if_shared (&DECL_SIZE_UNIT (DECL_RESULT (fndecl)), visited);

  delete visited;

  if (cgn)
    for (cgn = cgn->nested; cgn; cgn = cgn->next_nested)
      unshare_body (cgn->decl);
}

/* Callback for walk_tree to unmark the visited trees rooted at *TP.
   Subtrees are walked until the first unvisited node is encountered.  */

static tree
unmark_visited_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  /* If this node has been visited, unmark it and keep looking.  */
  if (TREE_VISITED (t))
    TREE_VISITED (t) = 0;

  /* Otherwise, don't look any deeper.  */
  else
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Unmark the visited trees rooted at *TP.  */

static inline void
unmark_visited (tree *tp)
{
  walk_tree (tp, unmark_visited_r, NULL, NULL);
}

/* Likewise, but mark all trees as not visited.  */

static void
unvisit_body (tree fndecl)
{
  struct cgraph_node *cgn = cgraph_node::get (fndecl);

  unmark_visited (&DECL_SAVED_TREE (fndecl));
  unmark_visited (&DECL_SIZE (DECL_RESULT (fndecl)));
  unmark_visited (&DECL_SIZE_UNIT (DECL_RESULT (fndecl)));

  if (cgn)
    for (cgn = cgn->nested; cgn; cgn = cgn->next_nested)
      unvisit_body (cgn->decl);
}

/* Unconditionally make an unshared copy of EXPR.  This is used when using
   stored expressions which span multiple functions, such as BINFO_VTABLE,
   as the normal unsharing process can't tell that they're shared.  */

tree
unshare_expr (tree expr)
{
  walk_tree (&expr, mostly_copy_tree_r, NULL, NULL);
  return expr;
}

/* Worker for unshare_expr_without_location.  */

static tree
prune_expr_location (tree *tp, int *walk_subtrees, void *)
{
  if (EXPR_P (*tp))
    SET_EXPR_LOCATION (*tp, UNKNOWN_LOCATION);
  else
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Similar to unshare_expr but also prune all expression locations
   from EXPR.  */

tree
unshare_expr_without_location (tree expr)
{
  walk_tree (&expr, mostly_copy_tree_r, NULL, NULL);
  if (EXPR_P (expr))
    walk_tree (&expr, prune_expr_location, NULL, NULL);
  return expr;
}

/* Return the EXPR_LOCATION of EXPR, if it (maybe recursively) has
   one, OR_ELSE otherwise.  The location of a STATEMENT_LISTs
   comprising at least one DEBUG_BEGIN_STMT followed by exactly one
   EXPR is the location of the EXPR.  */

static location_t
rexpr_location (tree expr, location_t or_else = UNKNOWN_LOCATION)
{
  if (!expr)
    return or_else;

  if (EXPR_HAS_LOCATION (expr))
    return EXPR_LOCATION (expr);

  if (TREE_CODE (expr) != STATEMENT_LIST)
    return or_else;

  tree_stmt_iterator i = tsi_start (expr);

  bool found = false;
  while (!tsi_end_p (i) && TREE_CODE (tsi_stmt (i)) == DEBUG_BEGIN_STMT)
    {
      found = true;
      tsi_next (&i);
    }

  if (!found || !tsi_one_before_end_p (i))
    return or_else;

  return rexpr_location (tsi_stmt (i), or_else);
}

/* Return TRUE iff EXPR (maybe recursively) has a location; see
   rexpr_location for the potential recursion.  */

static inline bool
rexpr_has_location (tree expr)
{
  return rexpr_location (expr) != UNKNOWN_LOCATION;
}


/* WRAPPER is a code such as BIND_EXPR or CLEANUP_POINT_EXPR which can both
   contain statements and have a value.  Assign its value to a temporary
   and give it void_type_node.  Return the temporary, or NULL_TREE if
   WRAPPER was already void.  */

tree
voidify_wrapper_expr (tree wrapper, tree temp)
{
  tree type = TREE_TYPE (wrapper);
  if (type && !VOID_TYPE_P (type))
    {
      tree *p;

      /* Set p to point to the body of the wrapper.  Loop until we find
	 something that isn't a wrapper.  */
      for (p = &wrapper; p && *p; )
	{
	  switch (TREE_CODE (*p))
	    {
	    case BIND_EXPR:
	      TREE_SIDE_EFFECTS (*p) = 1;
	      TREE_TYPE (*p) = void_type_node;
	      /* For a BIND_EXPR, the body is operand 1.  */
	      p = &BIND_EXPR_BODY (*p);
	      break;

	    case CLEANUP_POINT_EXPR:
	    case TRY_FINALLY_EXPR:
	    case TRY_CATCH_EXPR:
	      TREE_SIDE_EFFECTS (*p) = 1;
	      TREE_TYPE (*p) = void_type_node;
	      p = &TREE_OPERAND (*p, 0);
	      break;

	    case STATEMENT_LIST:
	      {
		tree_stmt_iterator i = tsi_last (*p);
		TREE_SIDE_EFFECTS (*p) = 1;
		TREE_TYPE (*p) = void_type_node;
		p = tsi_end_p (i) ? NULL : tsi_stmt_ptr (i);
	      }
	      break;

	    case COMPOUND_EXPR:
	      /* Advance to the last statement.  Set all container types to
		 void.  */
	      for (; TREE_CODE (*p) == COMPOUND_EXPR; p = &TREE_OPERAND (*p, 1))
		{
		  TREE_SIDE_EFFECTS (*p) = 1;
		  TREE_TYPE (*p) = void_type_node;
		}
	      break;

	    case TRANSACTION_EXPR:
	      TREE_SIDE_EFFECTS (*p) = 1;
	      TREE_TYPE (*p) = void_type_node;
	      p = &TRANSACTION_EXPR_BODY (*p);
	      break;

	    default:
	      /* Assume that any tree upon which voidify_wrapper_expr is
		 directly called is a wrapper, and that its body is op0.  */
	      if (p == &wrapper)
		{
		  TREE_SIDE_EFFECTS (*p) = 1;
		  TREE_TYPE (*p) = void_type_node;
		  p = &TREE_OPERAND (*p, 0);
		  break;
		}
	      goto out;
	    }
	}

    out:
      if (p == NULL || IS_EMPTY_STMT (*p))
	temp = NULL_TREE;
      else if (temp)
	{
	  /* The wrapper is on the RHS of an assignment that we're pushing
	     down.  */
	  gcc_assert (TREE_CODE (temp) == INIT_EXPR
		      || TREE_CODE (temp) == MODIFY_EXPR);
	  TREE_OPERAND (temp, 1) = *p;
	  *p = temp;
	}
      else
	{
	  temp = create_tmp_var (type, "retval");
	  *p = build2 (INIT_EXPR, type, temp, *p);
	}

      return temp;
    }

  return NULL_TREE;
}

/* Prepare calls to builtins to SAVE and RESTORE the stack as well as
   a temporary through which they communicate.  */

static void
build_stack_save_restore (gcall **save, gcall **restore)
{
  tree tmp_var;

  *save = gimple_build_call (builtin_decl_implicit (BUILT_IN_STACK_SAVE), 0);
  tmp_var = create_tmp_var (ptr_type_node, "saved_stack");
  gimple_call_set_lhs (*save, tmp_var);

  *restore
    = gimple_build_call (builtin_decl_implicit (BUILT_IN_STACK_RESTORE),
			 1, tmp_var);
}

/* Generate IFN_ASAN_MARK call that poisons shadow of a for DECL variable.  */

static tree
build_asan_poison_call_expr (tree decl)
{
  /* Do not poison variables that have size equal to zero.  */
  tree unit_size = DECL_SIZE_UNIT (decl);
  if (zerop (unit_size))
    return NULL_TREE;

  tree base = build_fold_addr_expr (decl);

  return build_call_expr_internal_loc (UNKNOWN_LOCATION, IFN_ASAN_MARK,
				       void_type_node, 3,
				       build_int_cst (integer_type_node,
						      ASAN_MARK_POISON),
				       base, unit_size);
}

/* Generate IFN_ASAN_MARK call that would poison or unpoison, depending
   on POISON flag, shadow memory of a DECL variable.  The call will be
   put on location identified by IT iterator, where BEFORE flag drives
   position where the stmt will be put.  */

static void
asan_poison_variable (tree decl, bool poison, gimple_stmt_iterator *it,
		      bool before)
{
  tree unit_size = DECL_SIZE_UNIT (decl);
  tree base = build_fold_addr_expr (decl);

  /* Do not poison variables that have size equal to zero.  */
  if (zerop (unit_size))
    return;

  /* It's necessary to have all stack variables aligned to ASAN granularity
     bytes.  */
  if (DECL_ALIGN_UNIT (decl) <= ASAN_SHADOW_GRANULARITY)
    SET_DECL_ALIGN (decl, BITS_PER_UNIT * ASAN_SHADOW_GRANULARITY);

  HOST_WIDE_INT flags = poison ? ASAN_MARK_POISON : ASAN_MARK_UNPOISON;

  gimple *g
    = gimple_build_call_internal (IFN_ASAN_MARK, 3,
				  build_int_cst (integer_type_node, flags),
				  base, unit_size);

  if (before)
    gsi_insert_before (it, g, GSI_NEW_STMT);
  else
    gsi_insert_after (it, g, GSI_NEW_STMT);
}

/* Generate IFN_ASAN_MARK internal call that depending on POISON flag
   either poisons or unpoisons a DECL.  Created statement is appended
   to SEQ_P gimple sequence.  */

static void
asan_poison_variable (tree decl, bool poison, gimple_seq *seq_p)
{
  gimple_stmt_iterator it = gsi_last (*seq_p);
  bool before = false;

  if (gsi_end_p (it))
    before = true;

  asan_poison_variable (decl, poison, &it, before);
}

/* Sort pair of VAR_DECLs A and B by DECL_UID.  */

static int
sort_by_decl_uid (const void *a, const void *b)
{
  const tree *t1 = (const tree *)a;
  const tree *t2 = (const tree *)b;

  int uid1 = DECL_UID (*t1);
  int uid2 = DECL_UID (*t2);

  if (uid1 < uid2)
    return -1;
  else if (uid1 > uid2)
    return 1;
  else
    return 0;
}

/* Generate IFN_ASAN_MARK internal call for all VARIABLES
   depending on POISON flag.  Created statement is appended
   to SEQ_P gimple sequence.  */

static void
asan_poison_variables (hash_set<tree> *variables, bool poison, gimple_seq *seq_p)
{
  unsigned c = variables->elements ();
  if (c == 0)
    return;

  auto_vec<tree> sorted_variables (c);

  for (hash_set<tree>::iterator it = variables->begin ();
       it != variables->end (); ++it)
    sorted_variables.safe_push (*it);

  sorted_variables.qsort (sort_by_decl_uid);

  unsigned i;
  tree var;
  FOR_EACH_VEC_ELT (sorted_variables, i, var)
    {
      asan_poison_variable (var, poison, seq_p);

      /* Add use_after_scope_memory attribute for the variable in order
	 to prevent re-written into SSA.  */
      if (!lookup_attribute (ASAN_USE_AFTER_SCOPE_ATTRIBUTE,
			     DECL_ATTRIBUTES (var)))
	DECL_ATTRIBUTES (var)
	  = tree_cons (get_identifier (ASAN_USE_AFTER_SCOPE_ATTRIBUTE),
		       integer_one_node,
		       DECL_ATTRIBUTES (var));
    }
}

/* Gimplify a BIND_EXPR.  Just voidify and recurse.  */

static enum gimplify_status
gimplify_bind_expr (tree *expr_p, gimple_seq *pre_p)
{
  tree bind_expr = *expr_p;
  bool old_keep_stack = gimplify_ctxp->keep_stack;
  bool old_save_stack = gimplify_ctxp->save_stack;
  tree t;
  gbind *bind_stmt;
  gimple_seq body, cleanup;
  gcall *stack_save;
  location_t start_locus = 0, end_locus = 0;
  tree ret_clauses = NULL;

  tree temp = voidify_wrapper_expr (bind_expr, NULL);

  /* Mark variables seen in this bind expr.  */
  for (t = BIND_EXPR_VARS (bind_expr); t ; t = DECL_CHAIN (t))
    {
      if (VAR_P (t))
	{
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;

	  /* Mark variable as local.  */
	  if (ctx && ctx->region_type != ORT_NONE && !DECL_EXTERNAL (t)
	      && (! DECL_SEEN_IN_BIND_EXPR_P (t)
		  || splay_tree_lookup (ctx->variables,
					(splay_tree_key) t) == NULL))
	    {
	      if (ctx->region_type == ORT_SIMD
		  && TREE_ADDRESSABLE (t)
		  && !TREE_STATIC (t))
		omp_add_variable (ctx, t, GOVD_PRIVATE | GOVD_SEEN);
	      else
		omp_add_variable (ctx, t, GOVD_LOCAL | GOVD_SEEN);
	    }

	  DECL_SEEN_IN_BIND_EXPR_P (t) = 1;

	  if (DECL_HARD_REGISTER (t) && !is_global_var (t) && cfun)
	    cfun->has_local_explicit_reg_vars = true;
	}

      /* Preliminarily mark non-addressed complex variables as eligible
	 for promotion to gimple registers.  We'll transform their uses
	 as we find them.  */
      if ((TREE_CODE (TREE_TYPE (t)) == COMPLEX_TYPE
	   || TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
	  && !TREE_THIS_VOLATILE (t)
	  && (VAR_P (t) && !DECL_HARD_REGISTER (t))
	  && !needs_to_live_in_memory (t))
	DECL_GIMPLE_REG_P (t) = 1;
    }

  bind_stmt = gimple_build_bind (BIND_EXPR_VARS (bind_expr), NULL,
				 BIND_EXPR_BLOCK (bind_expr));
  gimple_push_bind_expr (bind_stmt);

  gimplify_ctxp->keep_stack = false;
  gimplify_ctxp->save_stack = false;

  /* Gimplify the body into the GIMPLE_BIND tuple's body.  */
  body = NULL;
  gimplify_stmt (&BIND_EXPR_BODY (bind_expr), &body);
  gimple_bind_set_body (bind_stmt, body);

  /* Source location wise, the cleanup code (stack_restore and clobbers)
     belongs to the end of the block, so propagate what we have.  The
     stack_save operation belongs to the beginning of block, which we can
     infer from the bind_expr directly if the block has no explicit
     assignment.  */
  if (BIND_EXPR_BLOCK (bind_expr))
    {
      end_locus = BLOCK_SOURCE_END_LOCATION (BIND_EXPR_BLOCK (bind_expr));
      start_locus = BLOCK_SOURCE_LOCATION (BIND_EXPR_BLOCK (bind_expr));
    }
  if (start_locus == 0)
    start_locus = EXPR_LOCATION (bind_expr);

  cleanup = NULL;
  stack_save = NULL;

  /* If the code both contains VLAs and calls alloca, then we cannot reclaim
     the stack space allocated to the VLAs.  */
  if (gimplify_ctxp->save_stack && !gimplify_ctxp->keep_stack)
    {
      gcall *stack_restore;

      /* Save stack on entry and restore it on exit.  Add a try_finally
	 block to achieve this.  */
      build_stack_save_restore (&stack_save, &stack_restore);

      gimple_set_location (stack_save, start_locus);
      gimple_set_location (stack_restore, end_locus);

      gimplify_seq_add_stmt (&cleanup, stack_restore);
    }

  /* Add clobbers for all variables that go out of scope.  */
  for (t = BIND_EXPR_VARS (bind_expr); t ; t = DECL_CHAIN (t))
    {
      if (VAR_P (t)
	  && !is_global_var (t)
	  && DECL_CONTEXT (t) == current_function_decl)
	{
	  if (!DECL_HARD_REGISTER (t)
	      && !TREE_THIS_VOLATILE (t)
	      && !DECL_HAS_VALUE_EXPR_P (t)
	      /* Only care for variables that have to be in memory.  Others
		 will be rewritten into SSA names, hence moved to the
		 top-level.  */
	      && !is_gimple_reg (t)
	      && flag_stack_reuse != SR_NONE)
	    {
	      tree clobber = build_constructor (TREE_TYPE (t), NULL);
	      gimple *clobber_stmt;
	      TREE_THIS_VOLATILE (clobber) = 1;
	      clobber_stmt = gimple_build_assign (t, clobber);
	      gimple_set_location (clobber_stmt, end_locus);
	      gimplify_seq_add_stmt (&cleanup, clobber_stmt);
	    }

	  if (flag_openacc && oacc_declare_returns != NULL)
	    {
	      tree *c = oacc_declare_returns->get (t);
	      if (c != NULL)
		{
		  if (ret_clauses)
		    OMP_CLAUSE_CHAIN (*c) = ret_clauses;

		  ret_clauses = *c;

		  oacc_declare_returns->remove (t);

		  if (oacc_declare_returns->elements () == 0)
		    {
		      delete oacc_declare_returns;
		      oacc_declare_returns = NULL;
		    }
		}
	    }
	}

      if (asan_poisoned_variables != NULL
	  && asan_poisoned_variables->contains (t))
	{
	  asan_poisoned_variables->remove (t);
	  asan_poison_variable (t, true, &cleanup);
	}

      if (gimplify_ctxp->live_switch_vars != NULL
	  && gimplify_ctxp->live_switch_vars->contains (t))
	gimplify_ctxp->live_switch_vars->remove (t);
    }

  if (ret_clauses)
    {
      gomp_target *stmt;
      gimple_stmt_iterator si = gsi_start (cleanup);

      stmt = gimple_build_omp_target (NULL, GF_OMP_TARGET_KIND_OACC_DECLARE,
				      ret_clauses);
      gsi_insert_seq_before_without_update (&si, stmt, GSI_NEW_STMT);
    }

  if (cleanup)
    {
      gtry *gs;
      gimple_seq new_body;

      new_body = NULL;
      gs = gimple_build_try (gimple_bind_body (bind_stmt), cleanup,
	  		     GIMPLE_TRY_FINALLY);

      if (stack_save)
	gimplify_seq_add_stmt (&new_body, stack_save);
      gimplify_seq_add_stmt (&new_body, gs);
      gimple_bind_set_body (bind_stmt, new_body);
    }

  /* keep_stack propagates all the way up to the outermost BIND_EXPR.  */
  if (!gimplify_ctxp->keep_stack)
    gimplify_ctxp->keep_stack = old_keep_stack;
  gimplify_ctxp->save_stack = old_save_stack;

  gimple_pop_bind_expr ();

  gimplify_seq_add_stmt (pre_p, bind_stmt);

  if (temp)
    {
      *expr_p = temp;
      return GS_OK;
    }

  *expr_p = NULL_TREE;
  return GS_ALL_DONE;
}

/* Maybe add early return predict statement to PRE_P sequence.  */

static void
maybe_add_early_return_predict_stmt (gimple_seq *pre_p)
{
  /* If we are not in a conditional context, add PREDICT statement.  */
  if (gimple_conditional_context ())
    {
      gimple *predict = gimple_build_predict (PRED_TREE_EARLY_RETURN,
					      NOT_TAKEN);
      gimplify_seq_add_stmt (pre_p, predict);
    }
}

/* Gimplify a RETURN_EXPR.  If the expression to be returned is not a
   GIMPLE value, it is assigned to a new temporary and the statement is
   re-written to return the temporary.

   PRE_P points to the sequence where side effects that must happen before
   STMT should be stored.  */

static enum gimplify_status
gimplify_return_expr (tree stmt, gimple_seq *pre_p)
{
  greturn *ret;
  tree ret_expr = TREE_OPERAND (stmt, 0);
  tree result_decl, result;

  if (ret_expr == error_mark_node)
    return GS_ERROR;

  if (!ret_expr
      || TREE_CODE (ret_expr) == RESULT_DECL)
    {
      maybe_add_early_return_predict_stmt (pre_p);
      greturn *ret = gimple_build_return (ret_expr);
      gimple_set_no_warning (ret, TREE_NO_WARNING (stmt));
      gimplify_seq_add_stmt (pre_p, ret);
      return GS_ALL_DONE;
    }

  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))))
    result_decl = NULL_TREE;
  else
    {
      result_decl = TREE_OPERAND (ret_expr, 0);

      /* See through a return by reference.  */
      if (TREE_CODE (result_decl) == INDIRECT_REF)
	result_decl = TREE_OPERAND (result_decl, 0);

      gcc_assert ((TREE_CODE (ret_expr) == MODIFY_EXPR
		   || TREE_CODE (ret_expr) == INIT_EXPR)
		  && TREE_CODE (result_decl) == RESULT_DECL);
    }

  /* If aggregate_value_p is true, then we can return the bare RESULT_DECL.
     Recall that aggregate_value_p is FALSE for any aggregate type that is
     returned in registers.  If we're returning values in registers, then
     we don't want to extend the lifetime of the RESULT_DECL, particularly
     across another call.  In addition, for those aggregates for which
     hard_function_value generates a PARALLEL, we'll die during normal
     expansion of structure assignments; there's special code in expand_return
     to handle this case that does not exist in expand_expr.  */
  if (!result_decl)
    result = NULL_TREE;
  else if (aggregate_value_p (result_decl, TREE_TYPE (current_function_decl)))
    {
      if (TREE_CODE (DECL_SIZE (result_decl)) != INTEGER_CST)
	{
	  if (!TYPE_SIZES_GIMPLIFIED (TREE_TYPE (result_decl)))
	    gimplify_type_sizes (TREE_TYPE (result_decl), pre_p);
	  /* Note that we don't use gimplify_vla_decl because the RESULT_DECL
	     should be effectively allocated by the caller, i.e. all calls to
	     this function must be subject to the Return Slot Optimization.  */
	  gimplify_one_sizepos (&DECL_SIZE (result_decl), pre_p);
	  gimplify_one_sizepos (&DECL_SIZE_UNIT (result_decl), pre_p);
	}
      result = result_decl;
    }
  else if (gimplify_ctxp->return_temp)
    result = gimplify_ctxp->return_temp;
  else
    {
      result = create_tmp_reg (TREE_TYPE (result_decl));

      /* ??? With complex control flow (usually involving abnormal edges),
	 we can wind up warning about an uninitialized value for this.  Due
	 to how this variable is constructed and initialized, this is never
	 true.  Give up and never warn.  */
      TREE_NO_WARNING (result) = 1;

      gimplify_ctxp->return_temp = result;
    }

  /* Smash the lhs of the MODIFY_EXPR to the temporary we plan to use.
     Then gimplify the whole thing.  */
  if (result != result_decl)
    TREE_OPERAND (ret_expr, 0) = result;

  gimplify_and_add (TREE_OPERAND (stmt, 0), pre_p);

  maybe_add_early_return_predict_stmt (pre_p);
  ret = gimple_build_return (result);
  gimple_set_no_warning (ret, TREE_NO_WARNING (stmt));
  gimplify_seq_add_stmt (pre_p, ret);

  return GS_ALL_DONE;
}

/* Gimplify a variable-length array DECL.  */

static void
gimplify_vla_decl (tree decl, gimple_seq *seq_p)
{
  /* This is a variable-sized decl.  Simplify its size and mark it
     for deferred expansion.  */
  tree t, addr, ptr_type;

  gimplify_one_sizepos (&DECL_SIZE (decl), seq_p);
  gimplify_one_sizepos (&DECL_SIZE_UNIT (decl), seq_p);

  /* Don't mess with a DECL_VALUE_EXPR set by the front-end.  */
  if (DECL_HAS_VALUE_EXPR_P (decl))
    return;

  /* All occurrences of this decl in final gimplified code will be
     replaced by indirection.  Setting DECL_VALUE_EXPR does two
     things: First, it lets the rest of the gimplifier know what
     replacement to use.  Second, it lets the debug info know
     where to find the value.  */
  ptr_type = build_pointer_type (TREE_TYPE (decl));
  addr = create_tmp_var (ptr_type, get_name (decl));
  DECL_IGNORED_P (addr) = 0;
  t = build_fold_indirect_ref (addr);
  TREE_THIS_NOTRAP (t) = 1;
  SET_DECL_VALUE_EXPR (decl, t);
  DECL_HAS_VALUE_EXPR_P (decl) = 1;

  t = build_alloca_call_expr (DECL_SIZE_UNIT (decl), DECL_ALIGN (decl),
			      max_int_size_in_bytes (TREE_TYPE (decl)));
  /* The call has been built for a variable-sized object.  */
  CALL_ALLOCA_FOR_VAR_P (t) = 1;
  t = fold_convert (ptr_type, t);
  t = build2 (MODIFY_EXPR, TREE_TYPE (addr), addr, t);

  gimplify_and_add (t, seq_p);
}

/* A helper function to be called via walk_tree.  Mark all labels under *TP
   as being forced.  To be called for DECL_INITIAL of static variables.  */

static tree
force_labels_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  if (TREE_CODE (*tp) == LABEL_DECL)
    {
      FORCED_LABEL (*tp) = 1;
      cfun->has_forced_label_in_static = 1;
    }

  return NULL_TREE;
}

/* Gimplify a DECL_EXPR node *STMT_P by making any necessary allocation
   and initialization explicit.  */

static enum gimplify_status
gimplify_decl_expr (tree *stmt_p, gimple_seq *seq_p)
{
  tree stmt = *stmt_p;
  tree decl = DECL_EXPR_DECL (stmt);

  *stmt_p = NULL_TREE;

  if (TREE_TYPE (decl) == error_mark_node)
    return GS_ERROR;

  if ((TREE_CODE (decl) == TYPE_DECL
       || VAR_P (decl))
      && !TYPE_SIZES_GIMPLIFIED (TREE_TYPE (decl)))
    {
      gimplify_type_sizes (TREE_TYPE (decl), seq_p);
      if (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE)
	gimplify_type_sizes (TREE_TYPE (TREE_TYPE (decl)), seq_p);
    }

  /* ??? DECL_ORIGINAL_TYPE is streamed for LTO so it needs to be gimplified
     in case its size expressions contain problematic nodes like CALL_EXPR.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (decl)
      && !TYPE_SIZES_GIMPLIFIED (DECL_ORIGINAL_TYPE (decl)))
    {
      gimplify_type_sizes (DECL_ORIGINAL_TYPE (decl), seq_p);
      if (TREE_CODE (DECL_ORIGINAL_TYPE (decl)) == REFERENCE_TYPE)
	gimplify_type_sizes (TREE_TYPE (DECL_ORIGINAL_TYPE (decl)), seq_p);
    }

  if (VAR_P (decl) && !DECL_EXTERNAL (decl))
    {
      tree init = DECL_INITIAL (decl);
      bool is_vla = false;

      if (TREE_CODE (DECL_SIZE_UNIT (decl)) != INTEGER_CST
	  || (!TREE_STATIC (decl)
	      && flag_stack_check == GENERIC_STACK_CHECK
	      && compare_tree_int (DECL_SIZE_UNIT (decl),
				   STACK_CHECK_MAX_VAR_SIZE) > 0))
	{
	  gimplify_vla_decl (decl, seq_p);
	  is_vla = true;
	}

      if (asan_poisoned_variables
	  && !is_vla
	  && TREE_ADDRESSABLE (decl)
	  && !TREE_STATIC (decl)
	  && !DECL_HAS_VALUE_EXPR_P (decl)
	  && DECL_ALIGN (decl) <= MAX_SUPPORTED_STACK_ALIGNMENT
	  && dbg_cnt (asan_use_after_scope)
	  && !gimplify_omp_ctxp)
	{
	  asan_poisoned_variables->add (decl);
	  asan_poison_variable (decl, false, seq_p);
	  if (!DECL_ARTIFICIAL (decl) && gimplify_ctxp->live_switch_vars)
	    gimplify_ctxp->live_switch_vars->add (decl);
	}

      /* Some front ends do not explicitly declare all anonymous
	 artificial variables.  We compensate here by declaring the
	 variables, though it would be better if the front ends would
	 explicitly declare them.  */
      if (!DECL_SEEN_IN_BIND_EXPR_P (decl)
	  && DECL_ARTIFICIAL (decl) && DECL_NAME (decl) == NULL_TREE)
	gimple_add_tmp_var (decl);

      if (init && init != error_mark_node)
	{
	  if (!TREE_STATIC (decl))
	    {
	      DECL_INITIAL (decl) = NULL_TREE;
	      init = build2 (INIT_EXPR, void_type_node, decl, init);
	      gimplify_and_add (init, seq_p);
	      ggc_free (init);
	    }
	  else
	    /* We must still examine initializers for static variables
	       as they may contain a label address.  */
	    walk_tree (&init, force_labels_r, NULL, NULL);
	}
    }

  return GS_ALL_DONE;
}

/* Gimplify a LOOP_EXPR.  Normally this just involves gimplifying the body
   and replacing the LOOP_EXPR with goto, but if the loop contains an
   EXIT_EXPR, we need to append a label for it to jump to.  */

static enum gimplify_status
gimplify_loop_expr (tree *expr_p, gimple_seq *pre_p)
{
  tree saved_label = gimplify_ctxp->exit_label;
  tree start_label = create_artificial_label (UNKNOWN_LOCATION);

  gimplify_seq_add_stmt (pre_p, gimple_build_label (start_label));

  gimplify_ctxp->exit_label = NULL_TREE;

  gimplify_and_add (LOOP_EXPR_BODY (*expr_p), pre_p);

  gimplify_seq_add_stmt (pre_p, gimple_build_goto (start_label));

  if (gimplify_ctxp->exit_label)
    gimplify_seq_add_stmt (pre_p,
			   gimple_build_label (gimplify_ctxp->exit_label));

  gimplify_ctxp->exit_label = saved_label;

  *expr_p = NULL;
  return GS_ALL_DONE;
}

/* Gimplify a statement list onto a sequence.  These may be created either
   by an enlightened front-end, or by shortcut_cond_expr.  */

static enum gimplify_status
gimplify_statement_list (tree *expr_p, gimple_seq *pre_p)
{
  tree temp = voidify_wrapper_expr (*expr_p, NULL);

  tree_stmt_iterator i = tsi_start (*expr_p);

  while (!tsi_end_p (i))
    {
      gimplify_stmt (tsi_stmt_ptr (i), pre_p);
      tsi_delink (&i);
    }

  if (temp)
    {
      *expr_p = temp;
      return GS_OK;
    }

  return GS_ALL_DONE;
}

/* Callback for walk_gimple_seq.  */

static tree
warn_switch_unreachable_r (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
			   struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    case GIMPLE_TRY:
      /* A compiler-generated cleanup or a user-written try block.
	 If it's empty, don't dive into it--that would result in
	 worse location info.  */
      if (gimple_try_eval (stmt) == NULL)
	{
	  wi->info = stmt;
	  return integer_zero_node;
	}
      /* Fall through.  */
    case GIMPLE_BIND:
    case GIMPLE_CATCH:
    case GIMPLE_EH_FILTER:
    case GIMPLE_TRANSACTION:
      /* Walk the sub-statements.  */
      *handled_ops_p = false;
      break;

    case GIMPLE_DEBUG:
      /* Ignore these.  We may generate them before declarations that
	 are never executed.  If there's something to warn about,
	 there will be non-debug stmts too, and we'll catch those.  */
      break;

    case GIMPLE_CALL:
      if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
	{
	  *handled_ops_p = false;
	  break;
	}
      /* Fall through.  */
    default:
      /* Save the first "real" statement (not a decl/lexical scope/...).  */
      wi->info = stmt;
      return integer_zero_node;
    }
  return NULL_TREE;
}

/* Possibly warn about unreachable statements between switch's controlling
   expression and the first case.  SEQ is the body of a switch expression.  */

static void
maybe_warn_switch_unreachable (gimple_seq seq)
{
  if (!warn_switch_unreachable
      /* This warning doesn't play well with Fortran when optimizations
	 are on.  */
      || lang_GNU_Fortran ()
      || seq == NULL)
    return;

  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq (seq, warn_switch_unreachable_r, NULL, &wi);
  gimple *stmt = (gimple *) wi.info;

  if (stmt && gimple_code (stmt) != GIMPLE_LABEL)
    {
      if (gimple_code (stmt) == GIMPLE_GOTO
	  && TREE_CODE (gimple_goto_dest (stmt)) == LABEL_DECL
	  && DECL_ARTIFICIAL (gimple_goto_dest (stmt)))
	/* Don't warn for compiler-generated gotos.  These occur
	   in Duff's devices, for example.  */;
      else
	warning_at (gimple_location (stmt), OPT_Wswitch_unreachable,
		    "statement will never be executed");
    }
}


/* A label entry that pairs label and a location.  */
struct label_entry
{
  tree label;
  location_t loc;
};

/* Find LABEL in vector of label entries VEC.  */

static struct label_entry *
find_label_entry (const auto_vec<struct label_entry> *vec, tree label)
{
  unsigned int i;
  struct label_entry *l;

  FOR_EACH_VEC_ELT (*vec, i, l)
    if (l->label == label)
      return l;
  return NULL;
}

/* Return true if LABEL, a LABEL_DECL, represents a case label
   in a vector of labels CASES.  */

static bool
case_label_p (const vec<tree> *cases, tree label)
{
  unsigned int i;
  tree l;

  FOR_EACH_VEC_ELT (*cases, i, l)
    if (CASE_LABEL (l) == label)
      return true;
  return false;
}

/* Find the last nondebug statement in a scope STMT.  */

static gimple *
last_stmt_in_scope (gimple *stmt)
{
  if (!stmt)
    return NULL;

  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      {
	gbind *bind = as_a <gbind *> (stmt);
	stmt = gimple_seq_last_nondebug_stmt (gimple_bind_body (bind));
	return last_stmt_in_scope (stmt);
      }

    case GIMPLE_TRY:
      {
	gtry *try_stmt = as_a <gtry *> (stmt);
	stmt = gimple_seq_last_nondebug_stmt (gimple_try_eval (try_stmt));
	gimple *last_eval = last_stmt_in_scope (stmt);
	if (gimple_stmt_may_fallthru (last_eval)
	    && (last_eval == NULL
		|| !gimple_call_internal_p (last_eval, IFN_FALLTHROUGH))
	    && gimple_try_kind (try_stmt) == GIMPLE_TRY_FINALLY)
	  {
	    stmt = gimple_seq_last_nondebug_stmt (gimple_try_cleanup (try_stmt));
	    return last_stmt_in_scope (stmt);
	  }
	else
	  return last_eval;
      }

    case GIMPLE_DEBUG:
      gcc_unreachable ();

    default:
      return stmt;
    }
}

/* Collect interesting labels in LABELS and return the statement preceding
   another case label, or a user-defined label.  */

static gimple *
collect_fallthrough_labels (gimple_stmt_iterator *gsi_p,
			    auto_vec <struct label_entry> *labels)
{
  gimple *prev = NULL;

  do
    {
      if (gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_BIND)
	{
	  /* Recognize the special GIMPLE_BIND added by gimplify_switch_expr,
	     which starts on a GIMPLE_SWITCH and ends with a break label.
	     Handle that as a single statement that can fall through.  */
	  gbind *bind = as_a <gbind *> (gsi_stmt (*gsi_p));
	  gimple *first = gimple_seq_first_stmt (gimple_bind_body (bind));
	  gimple *last = gimple_seq_last_stmt (gimple_bind_body (bind));
	  if (last
	      && gimple_code (first) == GIMPLE_SWITCH
	      && gimple_code (last) == GIMPLE_LABEL)
	    {
	      tree label = gimple_label_label (as_a <glabel *> (last));
	      if (SWITCH_BREAK_LABEL_P (label))
		{
		  prev = bind;
		  gsi_next (gsi_p);
		  continue;
		}
	    }
	}
      if (gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_BIND
	  || gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_TRY)
	{
	  /* Nested scope.  Only look at the last statement of
	     the innermost scope.  */
	  location_t bind_loc = gimple_location (gsi_stmt (*gsi_p));
	  gimple *last = last_stmt_in_scope (gsi_stmt (*gsi_p));
	  if (last)
	    {
	      prev = last;
	      /* It might be a label without a location.  Use the
		 location of the scope then.  */
	      if (!gimple_has_location (prev))
		gimple_set_location (prev, bind_loc);
	    }
	  gsi_next (gsi_p);
	  continue;
	}

      /* Ifs are tricky.  */
      if (gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_COND)
	{
	  gcond *cond_stmt = as_a <gcond *> (gsi_stmt (*gsi_p));
	  tree false_lab = gimple_cond_false_label (cond_stmt);
	  location_t if_loc = gimple_location (cond_stmt);

	  /* If we have e.g.
	       if (i > 1) goto <D.2259>; else goto D;
	     we can't do much with the else-branch.  */
	  if (!DECL_ARTIFICIAL (false_lab))
	    break;

	  /* Go on until the false label, then one step back.  */
	  for (; !gsi_end_p (*gsi_p); gsi_next (gsi_p))
	    {
	      gimple *stmt = gsi_stmt (*gsi_p);
	      if (gimple_code (stmt) == GIMPLE_LABEL
		  && gimple_label_label (as_a <glabel *> (stmt)) == false_lab)
		break;
	    }

	  /* Not found?  Oops.  */
	  if (gsi_end_p (*gsi_p))
	    break;

	  struct label_entry l = { false_lab, if_loc };
	  labels->safe_push (l);

	  /* Go to the last statement of the then branch.  */
	  gsi_prev (gsi_p);

	  /* if (i != 0) goto <D.1759>; else goto <D.1760>;
	     <D.1759>:
	     <stmt>;
	     goto <D.1761>;
	     <D.1760>:
	   */
	  if (gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_GOTO
	      && !gimple_has_location (gsi_stmt (*gsi_p)))
	    {
	      /* Look at the statement before, it might be
		 attribute fallthrough, in which case don't warn.  */
	      gsi_prev (gsi_p);
	      bool fallthru_before_dest
		= gimple_call_internal_p (gsi_stmt (*gsi_p), IFN_FALLTHROUGH);
	      gsi_next (gsi_p);
	      tree goto_dest = gimple_goto_dest (gsi_stmt (*gsi_p));
	      if (!fallthru_before_dest)
		{
		  struct label_entry l = { goto_dest, if_loc };
		  labels->safe_push (l);
		}
	    }
	  /* And move back.  */
	  gsi_next (gsi_p);
	}

      /* Remember the last statement.  Skip labels that are of no interest
	 to us.  */
      if (gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_LABEL)
	{
	  tree label = gimple_label_label (as_a <glabel *> (gsi_stmt (*gsi_p)));
	  if (find_label_entry (labels, label))
	    prev = gsi_stmt (*gsi_p);
	}
      else if (gimple_call_internal_p (gsi_stmt (*gsi_p), IFN_ASAN_MARK))
	;
      else if (!is_gimple_debug (gsi_stmt (*gsi_p)))
	prev = gsi_stmt (*gsi_p);
      gsi_next (gsi_p);
    }
  while (!gsi_end_p (*gsi_p)
	 /* Stop if we find a case or a user-defined label.  */
	 && (gimple_code (gsi_stmt (*gsi_p)) != GIMPLE_LABEL
	     || !gimple_has_location (gsi_stmt (*gsi_p))));

  return prev;
}

/* Return true if the switch fallthough warning should occur.  LABEL is
   the label statement that we're falling through to.  */

static bool
should_warn_for_implicit_fallthrough (gimple_stmt_iterator *gsi_p, tree label)
{
  gimple_stmt_iterator gsi = *gsi_p;

  /* Don't warn if the label is marked with a "falls through" comment.  */
  if (FALLTHROUGH_LABEL_P (label))
    return false;

  /* Don't warn for non-case labels followed by a statement:
       case 0:
	 foo ();
       label:
	 bar ();
     as these are likely intentional.  */
  if (!case_label_p (&gimplify_ctxp->case_labels, label))
    {
      tree l;
      while (!gsi_end_p (gsi)
	     && gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL
	     && (l = gimple_label_label (as_a <glabel *> (gsi_stmt (gsi))))
	     && !case_label_p (&gimplify_ctxp->case_labels, l))
	gsi_next_nondebug (&gsi);
      if (gsi_end_p (gsi) || gimple_code (gsi_stmt (gsi)) != GIMPLE_LABEL)
	return false;
    }

  /* Don't warn for terminated branches, i.e. when the subsequent case labels
     immediately breaks.  */
  gsi = *gsi_p;

  /* Skip all immediately following labels.  */
  while (!gsi_end_p (gsi)
	 && (gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL
	     || gimple_code (gsi_stmt (gsi)) == GIMPLE_PREDICT))
    gsi_next_nondebug (&gsi);

  /* { ... something; default:; } */
  if (gsi_end_p (gsi)
      /* { ... something; default: break; } or
	 { ... something; default: goto L; } */
      || gimple_code (gsi_stmt (gsi)) == GIMPLE_GOTO
      /* { ... something; default: return; } */
      || gimple_code (gsi_stmt (gsi)) == GIMPLE_RETURN)
    return false;

  return true;
}

/* Callback for walk_gimple_seq.  */

static tree
warn_implicit_fallthrough_r (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
			     struct walk_stmt_info *)
{
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    case GIMPLE_TRY:
    case GIMPLE_BIND:
    case GIMPLE_CATCH:
    case GIMPLE_EH_FILTER:
    case GIMPLE_TRANSACTION:
      /* Walk the sub-statements.  */
      *handled_ops_p = false;
      break;

    /* Find a sequence of form:

       GIMPLE_LABEL
       [...]
       <may fallthru stmt>
       GIMPLE_LABEL

       and possibly warn.  */
    case GIMPLE_LABEL:
      {
	/* Found a label.  Skip all immediately following labels.  */
	while (!gsi_end_p (*gsi_p)
	       && gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_LABEL)
	  gsi_next_nondebug (gsi_p);

	/* There might be no more statements.  */
	if (gsi_end_p (*gsi_p))
	  return integer_zero_node;

	/* Vector of labels that fall through.  */
	auto_vec <struct label_entry> labels;
	gimple *prev = collect_fallthrough_labels (gsi_p, &labels);

	/* There might be no more statements.  */
	if (gsi_end_p (*gsi_p))
	  return integer_zero_node;

	gimple *next = gsi_stmt (*gsi_p);
	tree label;
	/* If what follows is a label, then we may have a fallthrough.  */
	if (gimple_code (next) == GIMPLE_LABEL
	    && gimple_has_location (next)
	    && (label = gimple_label_label (as_a <glabel *> (next)))
	    && prev != NULL)
	  {
	    struct label_entry *l;
	    bool warned_p = false;
	    if (!should_warn_for_implicit_fallthrough (gsi_p, label))
	      /* Quiet.  */;
	    else if (gimple_code (prev) == GIMPLE_LABEL
		     && (label = gimple_label_label (as_a <glabel *> (prev)))
		     && (l = find_label_entry (&labels, label)))
	      warned_p = warning_at (l->loc, OPT_Wimplicit_fallthrough_,
				     "this statement may fall through");
	    else if (!gimple_call_internal_p (prev, IFN_FALLTHROUGH)
		     /* Try to be clever and don't warn when the statement
			can't actually fall through.  */
		     && gimple_stmt_may_fallthru (prev)
		     && gimple_has_location (prev))
	      warned_p = warning_at (gimple_location (prev),
				     OPT_Wimplicit_fallthrough_,
				     "this statement may fall through");
	    if (warned_p)
	      inform (gimple_location (next), "here");

	    /* Mark this label as processed so as to prevent multiple
	       warnings in nested switches.  */
	    FALLTHROUGH_LABEL_P (label) = true;

	    /* So that next warn_implicit_fallthrough_r will start looking for
	       a new sequence starting with this label.  */
	    gsi_prev (gsi_p);
	  }
      }
      break;
   default:
      break;
    }
  return NULL_TREE;
}

/* Warn when a switch case falls through.  */

static void
maybe_warn_implicit_fallthrough (gimple_seq seq)
{
  if (!warn_implicit_fallthrough)
    return;

  /* This warning is meant for C/C++/ObjC/ObjC++ only.  */
  if (!(lang_GNU_C ()
	|| lang_GNU_CXX ()
	|| lang_GNU_OBJC ()))
    return;

  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq (seq, warn_implicit_fallthrough_r, NULL, &wi);
}

/* Callback for walk_gimple_seq.  */

static tree
expand_FALLTHROUGH_r (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
		      struct walk_stmt_info *)
{
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    case GIMPLE_TRY:
    case GIMPLE_BIND:
    case GIMPLE_CATCH:
    case GIMPLE_EH_FILTER:
    case GIMPLE_TRANSACTION:
      /* Walk the sub-statements.  */
      *handled_ops_p = false;
      break;
    case GIMPLE_CALL:
      if (gimple_call_internal_p (stmt, IFN_FALLTHROUGH))
	{
	  gsi_remove (gsi_p, true);
	  if (gsi_end_p (*gsi_p))
	    return integer_zero_node;

	  bool found = false;
	  location_t loc = gimple_location (stmt);

	  gimple_stmt_iterator gsi2 = *gsi_p;
	  stmt = gsi_stmt (gsi2);
	  if (gimple_code (stmt) == GIMPLE_GOTO && !gimple_has_location (stmt))
	    {
	      /* Go on until the artificial label.  */
	      tree goto_dest = gimple_goto_dest (stmt);
	      for (; !gsi_end_p (gsi2); gsi_next (&gsi2))
		{
		  if (gimple_code (gsi_stmt (gsi2)) == GIMPLE_LABEL
		      && gimple_label_label (as_a <glabel *> (gsi_stmt (gsi2)))
			   == goto_dest)
		    break;
		}

	      /* Not found?  Stop.  */
	      if (gsi_end_p (gsi2))
		break;

	      /* Look one past it.  */
	      gsi_next (&gsi2);
	    }

	  /* We're looking for a case label or default label here.  */
	  while (!gsi_end_p (gsi2))
	    {
	      stmt = gsi_stmt (gsi2);
	      if (gimple_code (stmt) == GIMPLE_LABEL)
		{
		  tree label = gimple_label_label (as_a <glabel *> (stmt));
		  if (gimple_has_location (stmt) && DECL_ARTIFICIAL (label))
		    {
		      found = true;
		      break;
		    }
		}
	      else if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
		;
	      else if (!is_gimple_debug (stmt))
		/* Anything else is not expected.  */
		break;
	      gsi_next (&gsi2);
	    }
	  if (!found)
	    warning_at (loc, 0, "attribute %<fallthrough%> not preceding "
			"a case label or default label");
	}
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Expand all FALLTHROUGH () calls in SEQ.  */

static void
expand_FALLTHROUGH (gimple_seq *seq_p)
{
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq_mod (seq_p, expand_FALLTHROUGH_r, NULL, &wi);
}


/* Gimplify a SWITCH_EXPR, and collect the vector of labels it can
   branch to.  */

static enum gimplify_status
gimplify_switch_expr (tree *expr_p, gimple_seq *pre_p)
{
  tree switch_expr = *expr_p;
  gimple_seq switch_body_seq = NULL;
  enum gimplify_status ret;
  tree index_type = TREE_TYPE (switch_expr);
  if (index_type == NULL_TREE)
    index_type = TREE_TYPE (SWITCH_COND (switch_expr));

  ret = gimplify_expr (&SWITCH_COND (switch_expr), pre_p, NULL, is_gimple_val,
                       fb_rvalue);
  if (ret == GS_ERROR || ret == GS_UNHANDLED)
    return ret;

  if (SWITCH_BODY (switch_expr))
    {
      vec<tree> labels;
      vec<tree> saved_labels;
      hash_set<tree> *saved_live_switch_vars = NULL;
      tree default_case = NULL_TREE;
      gswitch *switch_stmt;

      /* Save old labels, get new ones from body, then restore the old
         labels.  Save all the things from the switch body to append after.  */
      saved_labels = gimplify_ctxp->case_labels;
      gimplify_ctxp->case_labels.create (8);

      /* Do not create live_switch_vars if SWITCH_BODY is not a BIND_EXPR.  */
      saved_live_switch_vars = gimplify_ctxp->live_switch_vars;
      tree_code body_type = TREE_CODE (SWITCH_BODY (switch_expr));
      if (body_type == BIND_EXPR || body_type == STATEMENT_LIST)
	gimplify_ctxp->live_switch_vars = new hash_set<tree> (4);
      else
	gimplify_ctxp->live_switch_vars = NULL;

      bool old_in_switch_expr = gimplify_ctxp->in_switch_expr;
      gimplify_ctxp->in_switch_expr = true;

      gimplify_stmt (&SWITCH_BODY (switch_expr), &switch_body_seq);

      gimplify_ctxp->in_switch_expr = old_in_switch_expr;
      maybe_warn_switch_unreachable (switch_body_seq);
      maybe_warn_implicit_fallthrough (switch_body_seq);
      /* Only do this for the outermost GIMPLE_SWITCH.  */
      if (!gimplify_ctxp->in_switch_expr)
	expand_FALLTHROUGH (&switch_body_seq);

      labels = gimplify_ctxp->case_labels;
      gimplify_ctxp->case_labels = saved_labels;

      if (gimplify_ctxp->live_switch_vars)
	{
	  gcc_assert (gimplify_ctxp->live_switch_vars->elements () == 0);
	  delete gimplify_ctxp->live_switch_vars;
	}
      gimplify_ctxp->live_switch_vars = saved_live_switch_vars;

      preprocess_case_label_vec_for_gimple (labels, index_type,
					    &default_case);

      bool add_bind = false;
      if (!default_case)
	{
	  glabel *new_default;

	  default_case
	    = build_case_label (NULL_TREE, NULL_TREE,
				create_artificial_label (UNKNOWN_LOCATION));
	  if (old_in_switch_expr)
	    {
	      SWITCH_BREAK_LABEL_P (CASE_LABEL (default_case)) = 1;
	      add_bind = true;
	    }
	  new_default = gimple_build_label (CASE_LABEL (default_case));
	  gimplify_seq_add_stmt (&switch_body_seq, new_default);
	}
      else if (old_in_switch_expr)
	{
	  gimple *last = gimple_seq_last_stmt (switch_body_seq);
	  if (last && gimple_code (last) == GIMPLE_LABEL)
	    {
	      tree label = gimple_label_label (as_a <glabel *> (last));
	      if (SWITCH_BREAK_LABEL_P (label))
		add_bind = true;
	    }
	}

      switch_stmt = gimple_build_switch (SWITCH_COND (switch_expr),
					 default_case, labels);
      /* For the benefit of -Wimplicit-fallthrough, if switch_body_seq
	 ends with a GIMPLE_LABEL holding SWITCH_BREAK_LABEL_P LABEL_DECL,
	 wrap the GIMPLE_SWITCH up to that GIMPLE_LABEL into a GIMPLE_BIND,
	 so that we can easily find the start and end of the switch
	 statement.  */
      if (add_bind)
	{
	  gimple_seq bind_body = NULL;
	  gimplify_seq_add_stmt (&bind_body, switch_stmt);
	  gimple_seq_add_seq (&bind_body, switch_body_seq);
	  gbind *bind = gimple_build_bind (NULL_TREE, bind_body, NULL_TREE);
	  gimple_set_location (bind, EXPR_LOCATION (switch_expr));
	  gimplify_seq_add_stmt (pre_p, bind);
	}
      else
	{
	  gimplify_seq_add_stmt (pre_p, switch_stmt);
	  gimplify_seq_add_seq (pre_p, switch_body_seq);
	}
      labels.release ();
    }
  else
    gcc_unreachable ();

  return GS_ALL_DONE;
}

/* Gimplify the LABEL_EXPR pointed to by EXPR_P.  */

static enum gimplify_status
gimplify_label_expr (tree *expr_p, gimple_seq *pre_p)
{
  gcc_assert (decl_function_context (LABEL_EXPR_LABEL (*expr_p))
	      == current_function_decl);

  tree label = LABEL_EXPR_LABEL (*expr_p);
  glabel *label_stmt = gimple_build_label (label);
  gimple_set_location (label_stmt, EXPR_LOCATION (*expr_p));
  gimplify_seq_add_stmt (pre_p, label_stmt);

  if (lookup_attribute ("cold", DECL_ATTRIBUTES (label)))
    gimple_seq_add_stmt (pre_p, gimple_build_predict (PRED_COLD_LABEL,
						      NOT_TAKEN));
  else if (lookup_attribute ("hot", DECL_ATTRIBUTES (label)))
    gimple_seq_add_stmt (pre_p, gimple_build_predict (PRED_HOT_LABEL,
						      TAKEN));

  return GS_ALL_DONE;
}

/* Gimplify the CASE_LABEL_EXPR pointed to by EXPR_P.  */

static enum gimplify_status
gimplify_case_label_expr (tree *expr_p, gimple_seq *pre_p)
{
  struct gimplify_ctx *ctxp;
  glabel *label_stmt;

  /* Invalid programs can play Duff's Device type games with, for example,
     #pragma omp parallel.  At least in the C front end, we don't
     detect such invalid branches until after gimplification, in the
     diagnose_omp_blocks pass.  */
  for (ctxp = gimplify_ctxp; ; ctxp = ctxp->prev_context)
    if (ctxp->case_labels.exists ())
      break;

  label_stmt = gimple_build_label (CASE_LABEL (*expr_p));
  gimple_set_location (label_stmt, EXPR_LOCATION (*expr_p));
  ctxp->case_labels.safe_push (*expr_p);
  gimplify_seq_add_stmt (pre_p, label_stmt);

  return GS_ALL_DONE;
}

/* Build a GOTO to the LABEL_DECL pointed to by LABEL_P, building it first
   if necessary.  */

tree
build_and_jump (tree *label_p)
{
  if (label_p == NULL)
    /* If there's nowhere to jump, just fall through.  */
    return NULL_TREE;

  if (*label_p == NULL_TREE)
    {
      tree label = create_artificial_label (UNKNOWN_LOCATION);
      *label_p = label;
    }

  return build1 (GOTO_EXPR, void_type_node, *label_p);
}

/* Gimplify an EXIT_EXPR by converting to a GOTO_EXPR inside a COND_EXPR.
   This also involves building a label to jump to and communicating it to
   gimplify_loop_expr through gimplify_ctxp->exit_label.  */

static enum gimplify_status
gimplify_exit_expr (tree *expr_p)
{
  tree cond = TREE_OPERAND (*expr_p, 0);
  tree expr;

  expr = build_and_jump (&gimplify_ctxp->exit_label);
  expr = build3 (COND_EXPR, void_type_node, cond, expr, NULL_TREE);
  *expr_p = expr;

  return GS_OK;
}

/* *EXPR_P is a COMPONENT_REF being used as an rvalue.  If its type is
   different from its canonical type, wrap the whole thing inside a
   NOP_EXPR and force the type of the COMPONENT_REF to be the canonical
   type.

   The canonical type of a COMPONENT_REF is the type of the field being
   referenced--unless the field is a bit-field which can be read directly
   in a smaller mode, in which case the canonical type is the
   sign-appropriate type corresponding to that mode.  */

static void
canonicalize_component_ref (tree *expr_p)
{
  tree expr = *expr_p;
  tree type;

  gcc_assert (TREE_CODE (expr) == COMPONENT_REF);

  if (INTEGRAL_TYPE_P (TREE_TYPE (expr)))
    type = TREE_TYPE (get_unwidened (expr, NULL_TREE));
  else
    type = TREE_TYPE (TREE_OPERAND (expr, 1));

  /* One could argue that all the stuff below is not necessary for
     the non-bitfield case and declare it a FE error if type
     adjustment would be needed.  */
  if (TREE_TYPE (expr) != type)
    {
#ifdef ENABLE_TYPES_CHECKING
      tree old_type = TREE_TYPE (expr);
#endif
      int type_quals;

      /* We need to preserve qualifiers and propagate them from
	 operand 0.  */
      type_quals = TYPE_QUALS (type)
	| TYPE_QUALS (TREE_TYPE (TREE_OPERAND (expr, 0)));
      if (TYPE_QUALS (type) != type_quals)
	type = build_qualified_type (TYPE_MAIN_VARIANT (type), type_quals);

      /* Set the type of the COMPONENT_REF to the underlying type.  */
      TREE_TYPE (expr) = type;

#ifdef ENABLE_TYPES_CHECKING
      /* It is now a FE error, if the conversion from the canonical
	 type to the original expression type is not useless.  */
      gcc_assert (useless_type_conversion_p (old_type, type));
#endif
    }
}

/* If a NOP conversion is changing a pointer to array of foo to a pointer
   to foo, embed that change in the ADDR_EXPR by converting
      T array[U];
      (T *)&array
   ==>
      &array[L]
   where L is the lower bound.  For simplicity, only do this for constant
   lower bound.
   The constraint is that the type of &array[L] is trivially convertible
   to T *.  */

static void
canonicalize_addr_expr (tree *expr_p)
{
  tree expr = *expr_p;
  tree addr_expr = TREE_OPERAND (expr, 0);
  tree datype, ddatype, pddatype;

  /* We simplify only conversions from an ADDR_EXPR to a pointer type.  */
  if (!POINTER_TYPE_P (TREE_TYPE (expr))
      || TREE_CODE (addr_expr) != ADDR_EXPR)
    return;

  /* The addr_expr type should be a pointer to an array.  */
  datype = TREE_TYPE (TREE_TYPE (addr_expr));
  if (TREE_CODE (datype) != ARRAY_TYPE)
    return;

  /* The pointer to element type shall be trivially convertible to
     the expression pointer type.  */
  ddatype = TREE_TYPE (datype);
  pddatype = build_pointer_type (ddatype);
  if (!useless_type_conversion_p (TYPE_MAIN_VARIANT (TREE_TYPE (expr)),
				  pddatype))
    return;

  /* The lower bound and element sizes must be constant.  */
  if (!TYPE_SIZE_UNIT (ddatype)
      || TREE_CODE (TYPE_SIZE_UNIT (ddatype)) != INTEGER_CST
      || !TYPE_DOMAIN (datype) || !TYPE_MIN_VALUE (TYPE_DOMAIN (datype))
      || TREE_CODE (TYPE_MIN_VALUE (TYPE_DOMAIN (datype))) != INTEGER_CST)
    return;

  /* All checks succeeded.  Build a new node to merge the cast.  */
  *expr_p = build4 (ARRAY_REF, ddatype, TREE_OPERAND (addr_expr, 0),
		    TYPE_MIN_VALUE (TYPE_DOMAIN (datype)),
		    NULL_TREE, NULL_TREE);
  *expr_p = build1 (ADDR_EXPR, pddatype, *expr_p);

  /* We can have stripped a required restrict qualifier above.  */
  if (!useless_type_conversion_p (TREE_TYPE (expr), TREE_TYPE (*expr_p)))
    *expr_p = fold_convert (TREE_TYPE (expr), *expr_p);
}

/* *EXPR_P is a NOP_EXPR or CONVERT_EXPR.  Remove it and/or other conversions
   underneath as appropriate.  */

static enum gimplify_status
gimplify_conversion (tree *expr_p)
{
  location_t loc = EXPR_LOCATION (*expr_p);
  gcc_assert (CONVERT_EXPR_P (*expr_p));

  /* Then strip away all but the outermost conversion.  */
  STRIP_SIGN_NOPS (TREE_OPERAND (*expr_p, 0));

  /* And remove the outermost conversion if it's useless.  */
  if (tree_ssa_useless_type_conversion (*expr_p))
    *expr_p = TREE_OPERAND (*expr_p, 0);

  /* If we still have a conversion at the toplevel,
     then canonicalize some constructs.  */
  if (CONVERT_EXPR_P (*expr_p))
    {
      tree sub = TREE_OPERAND (*expr_p, 0);

      /* If a NOP conversion is changing the type of a COMPONENT_REF
	 expression, then canonicalize its type now in order to expose more
	 redundant conversions.  */
      if (TREE_CODE (sub) == COMPONENT_REF)
	canonicalize_component_ref (&TREE_OPERAND (*expr_p, 0));

      /* If a NOP conversion is changing a pointer to array of foo
	 to a pointer to foo, embed that change in the ADDR_EXPR.  */
      else if (TREE_CODE (sub) == ADDR_EXPR)
	canonicalize_addr_expr (expr_p);
    }

  /* If we have a conversion to a non-register type force the
     use of a VIEW_CONVERT_EXPR instead.  */
  if (CONVERT_EXPR_P (*expr_p) && !is_gimple_reg_type (TREE_TYPE (*expr_p)))
    *expr_p = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (*expr_p),
			       TREE_OPERAND (*expr_p, 0));

  /* Canonicalize CONVERT_EXPR to NOP_EXPR.  */
  if (TREE_CODE (*expr_p) == CONVERT_EXPR)
    TREE_SET_CODE (*expr_p, NOP_EXPR);

  return GS_OK;
}

/* Nonlocal VLAs seen in the current function.  */
static hash_set<tree> *nonlocal_vlas;

/* The VAR_DECLs created for nonlocal VLAs for debug info purposes.  */
static tree nonlocal_vla_vars;

/* Gimplify a VAR_DECL or PARM_DECL.  Return GS_OK if we expanded a
   DECL_VALUE_EXPR, and it's worth re-examining things.  */

static enum gimplify_status
gimplify_var_or_parm_decl (tree *expr_p)
{
  tree decl = *expr_p;

  /* ??? If this is a local variable, and it has not been seen in any
     outer BIND_EXPR, then it's probably the result of a duplicate
     declaration, for which we've already issued an error.  It would
     be really nice if the front end wouldn't leak these at all.
     Currently the only known culprit is C++ destructors, as seen
     in g++.old-deja/g++.jason/binding.C.  */
  if (VAR_P (decl)
      && !DECL_SEEN_IN_BIND_EXPR_P (decl)
      && !TREE_STATIC (decl) && !DECL_EXTERNAL (decl)
      && decl_function_context (decl) == current_function_decl)
    {
      gcc_assert (seen_error ());
      return GS_ERROR;
    }

  /* When within an OMP context, notice uses of variables.  */
  if (gimplify_omp_ctxp && omp_notice_variable (gimplify_omp_ctxp, decl, true))
    return GS_ALL_DONE;

  /* If the decl is an alias for another expression, substitute it now.  */
  if (DECL_HAS_VALUE_EXPR_P (decl))
    {
      tree value_expr = DECL_VALUE_EXPR (decl);

      /* For referenced nonlocal VLAs add a decl for debugging purposes
	 to the current function.  */
      if (VAR_P (decl)
	  && TREE_CODE (DECL_SIZE_UNIT (decl)) != INTEGER_CST
	  && nonlocal_vlas != NULL
	  && TREE_CODE (value_expr) == INDIRECT_REF
	  && TREE_CODE (TREE_OPERAND (value_expr, 0)) == VAR_DECL
	  && decl_function_context (decl) != current_function_decl)
	{
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
	  while (ctx
		 && (ctx->region_type == ORT_WORKSHARE
		     || ctx->region_type == ORT_SIMD
		     || ctx->region_type == ORT_ACC))
	    ctx = ctx->outer_context;
	  if (!ctx && !nonlocal_vlas->add (decl))
	    {
	      tree copy = copy_node (decl);

	      lang_hooks.dup_lang_specific_decl (copy);
	      SET_DECL_RTL (copy, 0);
	      TREE_USED (copy) = 1;
	      DECL_CHAIN (copy) = nonlocal_vla_vars;
	      nonlocal_vla_vars = copy;
	      SET_DECL_VALUE_EXPR (copy, unshare_expr (value_expr));
	      DECL_HAS_VALUE_EXPR_P (copy) = 1;
	    }
	}

      *expr_p = unshare_expr (value_expr);
      return GS_OK;
    }

  return GS_ALL_DONE;
}

/* Recalculate the value of the TREE_SIDE_EFFECTS flag for T.  */

static void
recalculate_side_effects (tree t)
{
  enum tree_code code = TREE_CODE (t);
  int len = TREE_OPERAND_LENGTH (t);
  int i;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_expression:
      switch (code)
	{
	case INIT_EXPR:
	case MODIFY_EXPR:
	case VA_ARG_EXPR:
	case PREDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  /* All of these have side-effects, no matter what their
	     operands are.  */
	  return;

	default:
	  break;
	}
      /* Fall through.  */

    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:       /* a unary arithmetic expression */
    case tcc_binary:      /* a binary arithmetic expression */
    case tcc_reference:   /* a reference */
    case tcc_vl_exp:        /* a function call */
      TREE_SIDE_EFFECTS (t) = TREE_THIS_VOLATILE (t);
      for (i = 0; i < len; ++i)
	{
	  tree op = TREE_OPERAND (t, i);
	  if (op && TREE_SIDE_EFFECTS (op))
	    TREE_SIDE_EFFECTS (t) = 1;
	}
      break;

    case tcc_constant:
      /* No side-effects.  */
      return;

    default:
      gcc_unreachable ();
   }
}

/* Gimplify the COMPONENT_REF, ARRAY_REF, REALPART_EXPR or IMAGPART_EXPR
   node *EXPR_P.

      compound_lval
	      : min_lval '[' val ']'
	      | min_lval '.' ID
	      | compound_lval '[' val ']'
	      | compound_lval '.' ID

   This is not part of the original SIMPLE definition, which separates
   array and member references, but it seems reasonable to handle them
   together.  Also, this way we don't run into problems with union
   aliasing; gcc requires that for accesses through a union to alias, the
   union reference must be explicit, which was not always the case when we
   were splitting up array and member refs.

   PRE_P points to the sequence where side effects that must happen before
     *EXPR_P should be stored.

   POST_P points to the sequence where side effects that must happen after
     *EXPR_P should be stored.  */

static enum gimplify_status
gimplify_compound_lval (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
			fallback_t fallback)
{
  tree *p;
  enum gimplify_status ret = GS_ALL_DONE, tret;
  int i;
  location_t loc = EXPR_LOCATION (*expr_p);
  tree expr = *expr_p;

  /* Create a stack of the subexpressions so later we can walk them in
     order from inner to outer.  */
  auto_vec<tree, 10> expr_stack;

  /* We can handle anything that get_inner_reference can deal with.  */
  for (p = expr_p; ; p = &TREE_OPERAND (*p, 0))
    {
    restart:
      /* Fold INDIRECT_REFs now to turn them into ARRAY_REFs.  */
      if (TREE_CODE (*p) == INDIRECT_REF)
	*p = fold_indirect_ref_loc (loc, *p);

      if (handled_component_p (*p))
	;
      /* Expand DECL_VALUE_EXPR now.  In some cases that may expose
	 additional COMPONENT_REFs.  */
      else if ((VAR_P (*p) || TREE_CODE (*p) == PARM_DECL)
	       && gimplify_var_or_parm_decl (p) == GS_OK)
	goto restart;
      else
	break;

      expr_stack.safe_push (*p);
    }

  gcc_assert (expr_stack.length ());

  /* Now EXPR_STACK is a stack of pointers to all the refs we've
     walked through and P points to the innermost expression.

     Java requires that we elaborated nodes in source order.  That
     means we must gimplify the inner expression followed by each of
     the indices, in order.  But we can't gimplify the inner
     expression until we deal with any variable bounds, sizes, or
     positions in order to deal with PLACEHOLDER_EXPRs.

     So we do this in three steps.  First we deal with the annotations
     for any variables in the components, then we gimplify the base,
     then we gimplify any indices, from left to right.  */
  for (i = expr_stack.length () - 1; i >= 0; i--)
    {
      tree t = expr_stack[i];

      if (TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	{
	  /* Gimplify the low bound and element type size and put them into
	     the ARRAY_REF.  If these values are set, they have already been
	     gimplified.  */
	  if (TREE_OPERAND (t, 2) == NULL_TREE)
	    {
	      tree low = unshare_expr (array_ref_low_bound (t));
	      if (!is_gimple_min_invariant (low))
		{
		  TREE_OPERAND (t, 2) = low;
		  tret = gimplify_expr (&TREE_OPERAND (t, 2), pre_p,
					post_p, is_gimple_reg,
					fb_rvalue);
		  ret = MIN (ret, tret);
		}
	    }
	  else
	    {
	      tret = gimplify_expr (&TREE_OPERAND (t, 2), pre_p, post_p,
				    is_gimple_reg, fb_rvalue);
	      ret = MIN (ret, tret);
	    }

	  if (TREE_OPERAND (t, 3) == NULL_TREE)
	    {
	      tree elmt_type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 0)));
	      tree elmt_size = unshare_expr (array_ref_element_size (t));
	      tree factor = size_int (TYPE_ALIGN_UNIT (elmt_type));

	      /* Divide the element size by the alignment of the element
		 type (above).  */
	      elmt_size
		= size_binop_loc (loc, EXACT_DIV_EXPR, elmt_size, factor);

	      if (!is_gimple_min_invariant (elmt_size))
		{
		  TREE_OPERAND (t, 3) = elmt_size;
		  tret = gimplify_expr (&TREE_OPERAND (t, 3), pre_p,
					post_p, is_gimple_reg,
					fb_rvalue);
		  ret = MIN (ret, tret);
		}
	    }
	  else
	    {
	      tret = gimplify_expr (&TREE_OPERAND (t, 3), pre_p, post_p,
				    is_gimple_reg, fb_rvalue);
	      ret = MIN (ret, tret);
	    }
	}
      else if (TREE_CODE (t) == COMPONENT_REF)
	{
	  /* Set the field offset into T and gimplify it.  */
	  if (TREE_OPERAND (t, 2) == NULL_TREE)
	    {
	      tree offset = unshare_expr (component_ref_field_offset (t));
	      tree field = TREE_OPERAND (t, 1);
	      tree factor
		= size_int (DECL_OFFSET_ALIGN (field) / BITS_PER_UNIT);

	      /* Divide the offset by its alignment.  */
	      offset = size_binop_loc (loc, EXACT_DIV_EXPR, offset, factor);

	      if (!is_gimple_min_invariant (offset))
		{
		  TREE_OPERAND (t, 2) = offset;
		  tret = gimplify_expr (&TREE_OPERAND (t, 2), pre_p,
					post_p, is_gimple_reg,
					fb_rvalue);
		  ret = MIN (ret, tret);
		}
	    }
	  else
	    {
	      tret = gimplify_expr (&TREE_OPERAND (t, 2), pre_p, post_p,
				    is_gimple_reg, fb_rvalue);
	      ret = MIN (ret, tret);
	    }
	}
    }

  /* Step 2 is to gimplify the base expression.  Make sure lvalue is set
     so as to match the min_lval predicate.  Failure to do so may result
     in the creation of large aggregate temporaries.  */
  tret = gimplify_expr (p, pre_p, post_p, is_gimple_min_lval,
			fallback | fb_lvalue);
  ret = MIN (ret, tret);

  /* And finally, the indices and operands of ARRAY_REF.  During this
     loop we also remove any useless conversions.  */
  for (; expr_stack.length () > 0; )
    {
      tree t = expr_stack.pop ();

      if (TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	{
	  /* Gimplify the dimension.  */
	  if (!is_gimple_min_invariant (TREE_OPERAND (t, 1)))
	    {
	      tret = gimplify_expr (&TREE_OPERAND (t, 1), pre_p, post_p,
				    is_gimple_val, fb_rvalue);
	      ret = MIN (ret, tret);
	    }
	}

      STRIP_USELESS_TYPE_CONVERSION (TREE_OPERAND (t, 0));

      /* The innermost expression P may have originally had
	 TREE_SIDE_EFFECTS set which would have caused all the outer
	 expressions in *EXPR_P leading to P to also have had
	 TREE_SIDE_EFFECTS set.  */
      recalculate_side_effects (t);
    }

  /* If the outermost expression is a COMPONENT_REF, canonicalize its type.  */
  if ((fallback & fb_rvalue) && TREE_CODE (*expr_p) == COMPONENT_REF)
    {
      canonicalize_component_ref (expr_p);
    }

  expr_stack.release ();

  gcc_assert (*expr_p == expr || ret != GS_ALL_DONE);

  return ret;
}

/*  Gimplify the self modifying expression pointed to by EXPR_P
    (++, --, +=, -=).

    PRE_P points to the list where side effects that must happen before
	*EXPR_P should be stored.

    POST_P points to the list where side effects that must happen after
	*EXPR_P should be stored.

    WANT_VALUE is nonzero iff we want to use the value of this expression
	in another expression.

    ARITH_TYPE is the type the computation should be performed in.  */

enum gimplify_status
gimplify_self_mod_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
			bool want_value, tree arith_type)
{
  enum tree_code code;
  tree lhs, lvalue, rhs, t1;
  gimple_seq post = NULL, *orig_post_p = post_p;
  bool postfix;
  enum tree_code arith_code;
  enum gimplify_status ret;
  location_t loc = EXPR_LOCATION (*expr_p);

  code = TREE_CODE (*expr_p);

  gcc_assert (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR
	      || code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR);

  /* Prefix or postfix?  */
  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    /* Faster to treat as prefix if result is not used.  */
    postfix = want_value;
  else
    postfix = false;

  /* For postfix, make sure the inner expression's post side effects
     are executed after side effects from this expression.  */
  if (postfix)
    post_p = &post;

  /* Add or subtract?  */
  if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
    arith_code = PLUS_EXPR;
  else
    arith_code = MINUS_EXPR;

  /* Gimplify the LHS into a GIMPLE lvalue.  */
  lvalue = TREE_OPERAND (*expr_p, 0);
  ret = gimplify_expr (&lvalue, pre_p, post_p, is_gimple_lvalue, fb_lvalue);
  if (ret == GS_ERROR)
    return ret;

  /* Extract the operands to the arithmetic operation.  */
  lhs = lvalue;
  rhs = TREE_OPERAND (*expr_p, 1);

  /* For postfix operator, we evaluate the LHS to an rvalue and then use
     that as the result value and in the postqueue operation.  */
  if (postfix)
    {
      ret = gimplify_expr (&lhs, pre_p, post_p, is_gimple_val, fb_rvalue);
      if (ret == GS_ERROR)
	return ret;

      lhs = get_initialized_tmp_var (lhs, pre_p, NULL);
    }

  /* For POINTERs increment, use POINTER_PLUS_EXPR.  */
  if (POINTER_TYPE_P (TREE_TYPE (lhs)))
    {
      rhs = convert_to_ptrofftype_loc (loc, rhs);
      if (arith_code == MINUS_EXPR)
	rhs = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (rhs), rhs);
      t1 = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (*expr_p), lhs, rhs);
    }
  else
    t1 = fold_convert (TREE_TYPE (*expr_p),
		       fold_build2 (arith_code, arith_type,
				    fold_convert (arith_type, lhs),
				    fold_convert (arith_type, rhs)));

  if (postfix)
    {
      gimplify_assign (lvalue, t1, pre_p);
      gimplify_seq_add_seq (orig_post_p, post);
      *expr_p = lhs;
      return GS_ALL_DONE;
    }
  else
    {
      *expr_p = build2 (MODIFY_EXPR, TREE_TYPE (lvalue), lvalue, t1);
      return GS_OK;
    }
}

/* If *EXPR_P has a variable sized type, wrap it in a WITH_SIZE_EXPR.  */

static void
maybe_with_size_expr (tree *expr_p)
{
  tree expr = *expr_p;
  tree type = TREE_TYPE (expr);
  tree size;

  /* If we've already wrapped this or the type is error_mark_node, we can't do
     anything.  */
  if (TREE_CODE (expr) == WITH_SIZE_EXPR
      || type == error_mark_node)
    return;

  /* If the size isn't known or is a constant, we have nothing to do.  */
  size = TYPE_SIZE_UNIT (type);
  if (!size || poly_int_tree_p (size))
    return;

  /* Otherwise, make a WITH_SIZE_EXPR.  */
  size = unshare_expr (size);
  size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, expr);
  *expr_p = build2 (WITH_SIZE_EXPR, type, expr, size);
}

/* Helper for gimplify_call_expr.  Gimplify a single argument *ARG_P
   Store any side-effects in PRE_P.  CALL_LOCATION is the location of
   the CALL_EXPR.  If ALLOW_SSA is set the actual parameter may be
   gimplified to an SSA name.  */

enum gimplify_status
gimplify_arg (tree *arg_p, gimple_seq *pre_p, location_t call_location,
	      bool allow_ssa)
{
  bool (*test) (tree);
  fallback_t fb;

  /* In general, we allow lvalues for function arguments to avoid
     extra overhead of copying large aggregates out of even larger
     aggregates into temporaries only to copy the temporaries to
     the argument list.  Make optimizers happy by pulling out to
     temporaries those types that fit in registers.  */
  if (is_gimple_reg_type (TREE_TYPE (*arg_p)))
    test = is_gimple_val, fb = fb_rvalue;
  else
    {
      test = is_gimple_lvalue, fb = fb_either;
      /* Also strip a TARGET_EXPR that would force an extra copy.  */
      if (TREE_CODE (*arg_p) == TARGET_EXPR)
	{
	  tree init = TARGET_EXPR_INITIAL (*arg_p);
	  if (init
	      && !VOID_TYPE_P (TREE_TYPE (init)))
	    *arg_p = init;
	}
    }

  /* If this is a variable sized type, we must remember the size.  */
  maybe_with_size_expr (arg_p);

  /* FIXME diagnostics: This will mess up gcc.dg/Warray-bounds.c.  */
  /* Make sure arguments have the same location as the function call
     itself.  */
  protected_set_expr_location (*arg_p, call_location);

  /* There is a sequence point before a function call.  Side effects in
     the argument list must occur before the actual call. So, when
     gimplifying arguments, force gimplify_expr to use an internal
     post queue which is then appended to the end of PRE_P.  */
  return gimplify_expr (arg_p, pre_p, NULL, test, fb, allow_ssa);
}

/* Don't fold inside offloading or taskreg regions: it can break code by
   adding decl references that weren't in the source.  We'll do it during
   omplower pass instead.  */

static bool
maybe_fold_stmt (gimple_stmt_iterator *gsi)
{
  struct gimplify_omp_ctx *ctx;
  for (ctx = gimplify_omp_ctxp; ctx; ctx = ctx->outer_context)
    if ((ctx->region_type & (ORT_TARGET | ORT_PARALLEL | ORT_TASK)) != 0)
      return false;
  return fold_stmt (gsi);
}

/* Gimplify the CALL_EXPR node *EXPR_P into the GIMPLE sequence PRE_P.
   WANT_VALUE is true if the result of the call is desired.  */

static enum gimplify_status
gimplify_call_expr (tree *expr_p, gimple_seq *pre_p, bool want_value)
{
  tree fndecl, parms, p, fnptrtype;
  enum gimplify_status ret;
  int i, nargs;
  gcall *call;
  bool builtin_va_start_p = false;
  location_t loc = EXPR_LOCATION (*expr_p);

  gcc_assert (TREE_CODE (*expr_p) == CALL_EXPR);

  /* For reliable diagnostics during inlining, it is necessary that
     every call_expr be annotated with file and line.  */
  if (! EXPR_HAS_LOCATION (*expr_p))
    SET_EXPR_LOCATION (*expr_p, input_location);

  /* Gimplify internal functions created in the FEs.  */
  if (CALL_EXPR_FN (*expr_p) == NULL_TREE)
    {
      if (want_value)
	return GS_ALL_DONE;

      nargs = call_expr_nargs (*expr_p);
      enum internal_fn ifn = CALL_EXPR_IFN (*expr_p);
      auto_vec<tree> vargs (nargs);

      for (i = 0; i < nargs; i++)
	{
	  gimplify_arg (&CALL_EXPR_ARG (*expr_p, i), pre_p,
			EXPR_LOCATION (*expr_p));
	  vargs.quick_push (CALL_EXPR_ARG (*expr_p, i));
	}

      gcall *call = gimple_build_call_internal_vec (ifn, vargs);
      gimple_call_set_nothrow (call, TREE_NOTHROW (*expr_p));
      gimplify_seq_add_stmt (pre_p, call);
      return GS_ALL_DONE;
    }

  /* This may be a call to a builtin function.

     Builtin function calls may be transformed into different
     (and more efficient) builtin function calls under certain
     circumstances.  Unfortunately, gimplification can muck things
     up enough that the builtin expanders are not aware that certain
     transformations are still valid.

     So we attempt transformation/gimplification of the call before
     we gimplify the CALL_EXPR.  At this time we do not manage to
     transform all calls in the same manner as the expanders do, but
     we do transform most of them.  */
  fndecl = get_callee_fndecl (*expr_p);
  if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      CASE_BUILT_IN_ALLOCA:
	/* If the call has been built for a variable-sized object, then we
	   want to restore the stack level when the enclosing BIND_EXPR is
	   exited to reclaim the allocated space; otherwise, we precisely
	   need to do the opposite and preserve the latest stack level.  */
	if (CALL_ALLOCA_FOR_VAR_P (*expr_p))
	  gimplify_ctxp->save_stack = true;
	else
	  gimplify_ctxp->keep_stack = true;
	break;

      case BUILT_IN_VA_START:
        {
	  builtin_va_start_p = TRUE;
	  if (call_expr_nargs (*expr_p) < 2)
	    {
	      error ("too few arguments to function %<va_start%>");
	      *expr_p = build_empty_stmt (EXPR_LOCATION (*expr_p));
	      return GS_OK;
	    }

	  if (fold_builtin_next_arg (*expr_p, true))
	    {
	      *expr_p = build_empty_stmt (EXPR_LOCATION (*expr_p));
	      return GS_OK;
	    }
	  break;
	}

      default:
        ;
      }
  if (fndecl && DECL_BUILT_IN (fndecl))
    {
      tree new_tree = fold_call_expr (input_location, *expr_p, !want_value);
      if (new_tree && new_tree != *expr_p)
	{
	  /* There was a transformation of this call which computes the
	     same value, but in a more efficient way.  Return and try
	     again.  */
	  *expr_p = new_tree;
	  return GS_OK;
	}
    }

  /* Remember the original function pointer type.  */
  fnptrtype = TREE_TYPE (CALL_EXPR_FN (*expr_p));

  /* There is a sequence point before the call, so any side effects in
     the calling expression must occur before the actual call.  Force
     gimplify_expr to use an internal post queue.  */
  ret = gimplify_expr (&CALL_EXPR_FN (*expr_p), pre_p, NULL,
		       is_gimple_call_addr, fb_rvalue);

  nargs = call_expr_nargs (*expr_p);

  /* Get argument types for verification.  */
  fndecl = get_callee_fndecl (*expr_p);
  parms = NULL_TREE;
  if (fndecl)
    parms = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  else
    parms = TYPE_ARG_TYPES (TREE_TYPE (fnptrtype));

  if (fndecl && DECL_ARGUMENTS (fndecl))
    p = DECL_ARGUMENTS (fndecl);
  else if (parms)
    p = parms;
  else
    p = NULL_TREE;
  for (i = 0; i < nargs && p; i++, p = TREE_CHAIN (p))
    ;

  /* If the last argument is __builtin_va_arg_pack () and it is not
     passed as a named argument, decrease the number of CALL_EXPR
     arguments and set instead the CALL_EXPR_VA_ARG_PACK flag.  */
  if (!p
      && i < nargs
      && TREE_CODE (CALL_EXPR_ARG (*expr_p, nargs - 1)) == CALL_EXPR)
    {
      tree last_arg = CALL_EXPR_ARG (*expr_p, nargs - 1);
      tree last_arg_fndecl = get_callee_fndecl (last_arg);

      if (last_arg_fndecl
	  && TREE_CODE (last_arg_fndecl) == FUNCTION_DECL
	  && DECL_BUILT_IN_CLASS (last_arg_fndecl) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (last_arg_fndecl) == BUILT_IN_VA_ARG_PACK)
	{
	  tree call = *expr_p;

	  --nargs;
	  *expr_p = build_call_array_loc (loc, TREE_TYPE (call),
					  CALL_EXPR_FN (call),
					  nargs, CALL_EXPR_ARGP (call));

	  /* Copy all CALL_EXPR flags, location and block, except
	     CALL_EXPR_VA_ARG_PACK flag.  */
	  CALL_EXPR_STATIC_CHAIN (*expr_p) = CALL_EXPR_STATIC_CHAIN (call);
	  CALL_EXPR_TAILCALL (*expr_p) = CALL_EXPR_TAILCALL (call);
	  CALL_EXPR_RETURN_SLOT_OPT (*expr_p)
	    = CALL_EXPR_RETURN_SLOT_OPT (call);
	  CALL_FROM_THUNK_P (*expr_p) = CALL_FROM_THUNK_P (call);
	  SET_EXPR_LOCATION (*expr_p, EXPR_LOCATION (call));

	  /* Set CALL_EXPR_VA_ARG_PACK.  */
	  CALL_EXPR_VA_ARG_PACK (*expr_p) = 1;
	}
    }

  /* If the call returns twice then after building the CFG the call
     argument computations will no longer dominate the call because
     we add an abnormal incoming edge to the call.  So do not use SSA
     vars there.  */
  bool returns_twice = call_expr_flags (*expr_p) & ECF_RETURNS_TWICE;

  /* Gimplify the function arguments.  */
  if (nargs > 0)
    {
      for (i = (PUSH_ARGS_REVERSED ? nargs - 1 : 0);
           PUSH_ARGS_REVERSED ? i >= 0 : i < nargs;
           PUSH_ARGS_REVERSED ? i-- : i++)
        {
          enum gimplify_status t;

          /* Avoid gimplifying the second argument to va_start, which needs to
             be the plain PARM_DECL.  */
          if ((i != 1) || !builtin_va_start_p)
            {
              t = gimplify_arg (&CALL_EXPR_ARG (*expr_p, i), pre_p,
				EXPR_LOCATION (*expr_p), ! returns_twice);

              if (t == GS_ERROR)
                ret = GS_ERROR;
            }
        }
    }

  /* Gimplify the static chain.  */
  if (CALL_EXPR_STATIC_CHAIN (*expr_p))
    {
      if (fndecl && !DECL_STATIC_CHAIN (fndecl))
	CALL_EXPR_STATIC_CHAIN (*expr_p) = NULL;
      else
	{
	  enum gimplify_status t;
	  t = gimplify_arg (&CALL_EXPR_STATIC_CHAIN (*expr_p), pre_p,
			    EXPR_LOCATION (*expr_p), ! returns_twice);
	  if (t == GS_ERROR)
	    ret = GS_ERROR;
	}
    }

  /* Verify the function result.  */
  if (want_value && fndecl
      && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fnptrtype))))
    {
      error_at (loc, "using result of function returning %<void%>");
      ret = GS_ERROR;
    }

  /* Try this again in case gimplification exposed something.  */
  if (ret != GS_ERROR)
    {
      tree new_tree = fold_call_expr (input_location, *expr_p, !want_value);

      if (new_tree && new_tree != *expr_p)
	{
	  /* There was a transformation of this call which computes the
	     same value, but in a more efficient way.  Return and try
	     again.  */
	  *expr_p = new_tree;
	  return GS_OK;
	}
    }
  else
    {
      *expr_p = error_mark_node;
      return GS_ERROR;
    }

  /* If the function is "const" or "pure", then clear TREE_SIDE_EFFECTS on its
     decl.  This allows us to eliminate redundant or useless
     calls to "const" functions.  */
  if (TREE_CODE (*expr_p) == CALL_EXPR)
    {
      int flags = call_expr_flags (*expr_p);
      if (flags & (ECF_CONST | ECF_PURE)
	  /* An infinite loop is considered a side effect.  */
	  && !(flags & (ECF_LOOPING_CONST_OR_PURE)))
	TREE_SIDE_EFFECTS (*expr_p) = 0;
    }

  /* If the value is not needed by the caller, emit a new GIMPLE_CALL
     and clear *EXPR_P.  Otherwise, leave *EXPR_P in its gimplified
     form and delegate the creation of a GIMPLE_CALL to
     gimplify_modify_expr.  This is always possible because when
     WANT_VALUE is true, the caller wants the result of this call into
     a temporary, which means that we will emit an INIT_EXPR in
     internal_get_tmp_var which will then be handled by
     gimplify_modify_expr.  */
  if (!want_value)
    {
      /* The CALL_EXPR in *EXPR_P is already in GIMPLE form, so all we
	 have to do is replicate it as a GIMPLE_CALL tuple.  */
      gimple_stmt_iterator gsi;
      call = gimple_build_call_from_tree (*expr_p, fnptrtype);
      notice_special_calls (call);
      gimplify_seq_add_stmt (pre_p, call);
      gsi = gsi_last (*pre_p);
      maybe_fold_stmt (&gsi);
      *expr_p = NULL_TREE;
    }
  else
    /* Remember the original function type.  */
    CALL_EXPR_FN (*expr_p) = build1 (NOP_EXPR, fnptrtype,
				     CALL_EXPR_FN (*expr_p));

  return ret;
}

/* Handle shortcut semantics in the predicate operand of a COND_EXPR by
   rewriting it into multiple COND_EXPRs, and possibly GOTO_EXPRs.

   TRUE_LABEL_P and FALSE_LABEL_P point to the labels to jump to if the
   condition is true or false, respectively.  If null, we should generate
   our own to skip over the evaluation of this specific expression.

   LOCUS is the source location of the COND_EXPR.

   This function is the tree equivalent of do_jump.

   shortcut_cond_r should only be called by shortcut_cond_expr.  */

static tree
shortcut_cond_r (tree pred, tree *true_label_p, tree *false_label_p,
		 location_t locus)
{
  tree local_label = NULL_TREE;
  tree t, expr = NULL;

  /* OK, it's not a simple case; we need to pull apart the COND_EXPR to
     retain the shortcut semantics.  Just insert the gotos here;
     shortcut_cond_expr will append the real blocks later.  */
  if (TREE_CODE (pred) == TRUTH_ANDIF_EXPR)
    {
      location_t new_locus;

      /* Turn if (a && b) into

	 if (a); else goto no;
	 if (b) goto yes; else goto no;
	 (no:) */

      if (false_label_p == NULL)
	false_label_p = &local_label;

      /* Keep the original source location on the first 'if'.  */
      t = shortcut_cond_r (TREE_OPERAND (pred, 0), NULL, false_label_p, locus);
      append_to_statement_list (t, &expr);

      /* Set the source location of the && on the second 'if'.  */
      new_locus = rexpr_location (pred, locus);
      t = shortcut_cond_r (TREE_OPERAND (pred, 1), true_label_p, false_label_p,
			   new_locus);
      append_to_statement_list (t, &expr);
    }
  else if (TREE_CODE (pred) == TRUTH_ORIF_EXPR)
    {
      location_t new_locus;

      /* Turn if (a || b) into

	 if (a) goto yes;
	 if (b) goto yes; else goto no;
	 (yes:) */

      if (true_label_p == NULL)
	true_label_p = &local_label;

      /* Keep the original source location on the first 'if'.  */
      t = shortcut_cond_r (TREE_OPERAND (pred, 0), true_label_p, NULL, locus);
      append_to_statement_list (t, &expr);

      /* Set the source location of the || on the second 'if'.  */
      new_locus = rexpr_location (pred, locus);
      t = shortcut_cond_r (TREE_OPERAND (pred, 1), true_label_p, false_label_p,
			   new_locus);
      append_to_statement_list (t, &expr);
    }
  else if (TREE_CODE (pred) == COND_EXPR
	   && !VOID_TYPE_P (TREE_TYPE (TREE_OPERAND (pred, 1)))
	   && !VOID_TYPE_P (TREE_TYPE (TREE_OPERAND (pred, 2))))
    {
      location_t new_locus;

      /* As long as we're messing with gotos, turn if (a ? b : c) into
	 if (a)
	   if (b) goto yes; else goto no;
	 else
	   if (c) goto yes; else goto no;

	 Don't do this if one of the arms has void type, which can happen
	 in C++ when the arm is throw.  */

      /* Keep the original source location on the first 'if'.  Set the source
	 location of the ? on the second 'if'.  */
      new_locus = rexpr_location (pred, locus);
      expr = build3 (COND_EXPR, void_type_node, TREE_OPERAND (pred, 0),
		     shortcut_cond_r (TREE_OPERAND (pred, 1), true_label_p,
				      false_label_p, locus),
		     shortcut_cond_r (TREE_OPERAND (pred, 2), true_label_p,
				      false_label_p, new_locus));
    }
  else
    {
      expr = build3 (COND_EXPR, void_type_node, pred,
		     build_and_jump (true_label_p),
		     build_and_jump (false_label_p));
      SET_EXPR_LOCATION (expr, locus);
    }

  if (local_label)
    {
      t = build1 (LABEL_EXPR, void_type_node, local_label);
      append_to_statement_list (t, &expr);
    }

  return expr;
}

/* If EXPR is a GOTO_EXPR, return it.  If it is a STATEMENT_LIST, skip
   any of its leading DEBUG_BEGIN_STMTS and recurse on the subsequent
   statement, if it is the last one.  Otherwise, return NULL.  */

static tree
find_goto (tree expr)
{
  if (!expr)
    return NULL_TREE;

  if (TREE_CODE (expr) == GOTO_EXPR)
    return expr;

  if (TREE_CODE (expr) != STATEMENT_LIST)
    return NULL_TREE;

  tree_stmt_iterator i = tsi_start (expr);

  while (!tsi_end_p (i) && TREE_CODE (tsi_stmt (i)) == DEBUG_BEGIN_STMT)
    tsi_next (&i);

  if (!tsi_one_before_end_p (i))
    return NULL_TREE;

  return find_goto (tsi_stmt (i));
}

/* Same as find_goto, except that it returns NULL if the destination
   is not a LABEL_DECL.  */

static inline tree
find_goto_label (tree expr)
{
  tree dest = find_goto (expr);
  if (dest && TREE_CODE (GOTO_DESTINATION (dest)) == LABEL_DECL)
    return dest;
  return NULL_TREE;
}

/* Given a conditional expression EXPR with short-circuit boolean
   predicates using TRUTH_ANDIF_EXPR or TRUTH_ORIF_EXPR, break the
   predicate apart into the equivalent sequence of conditionals.  */

static tree
shortcut_cond_expr (tree expr)
{
  tree pred = TREE_OPERAND (expr, 0);
  tree then_ = TREE_OPERAND (expr, 1);
  tree else_ = TREE_OPERAND (expr, 2);
  tree true_label, false_label, end_label, t;
  tree *true_label_p;
  tree *false_label_p;
  bool emit_end, emit_false, jump_over_else;
  bool then_se = then_ && TREE_SIDE_EFFECTS (then_);
  bool else_se = else_ && TREE_SIDE_EFFECTS (else_);

  /* First do simple transformations.  */
  if (!else_se)
    {
      /* If there is no 'else', turn
	   if (a && b) then c
	 into
	   if (a) if (b) then c.  */
      while (TREE_CODE (pred) == TRUTH_ANDIF_EXPR)
	{
	  /* Keep the original source location on the first 'if'.  */
	  location_t locus = EXPR_LOC_OR_LOC (expr, input_location);
	  TREE_OPERAND (expr, 0) = TREE_OPERAND (pred, 1);
	  /* Set the source location of the && on the second 'if'.  */
	  if (rexpr_has_location (pred))
	    SET_EXPR_LOCATION (expr, rexpr_location (pred));
	  then_ = shortcut_cond_expr (expr);
	  then_se = then_ && TREE_SIDE_EFFECTS (then_);
	  pred = TREE_OPERAND (pred, 0);
	  expr = build3 (COND_EXPR, void_type_node, pred, then_, NULL_TREE);
	  SET_EXPR_LOCATION (expr, locus);
	}
    }

  if (!then_se)
    {
      /* If there is no 'then', turn
	   if (a || b); else d
	 into
	   if (a); else if (b); else d.  */
      while (TREE_CODE (pred) == TRUTH_ORIF_EXPR)
	{
	  /* Keep the original source location on the first 'if'.  */
	  location_t locus = EXPR_LOC_OR_LOC (expr, input_location);
	  TREE_OPERAND (expr, 0) = TREE_OPERAND (pred, 1);
	  /* Set the source location of the || on the second 'if'.  */
	  if (rexpr_has_location (pred))
	    SET_EXPR_LOCATION (expr, rexpr_location (pred));
	  else_ = shortcut_cond_expr (expr);
	  else_se = else_ && TREE_SIDE_EFFECTS (else_);
	  pred = TREE_OPERAND (pred, 0);
	  expr = build3 (COND_EXPR, void_type_node, pred, NULL_TREE, else_);
	  SET_EXPR_LOCATION (expr, locus);
	}
    }

  /* If we're done, great.  */
  if (TREE_CODE (pred) != TRUTH_ANDIF_EXPR
      && TREE_CODE (pred) != TRUTH_ORIF_EXPR)
    return expr;

  /* Otherwise we need to mess with gotos.  Change
       if (a) c; else d;
     to
       if (a); else goto no;
       c; goto end;
       no: d; end:
     and recursively gimplify the condition.  */

  true_label = false_label = end_label = NULL_TREE;

  /* If our arms just jump somewhere, hijack those labels so we don't
     generate jumps to jumps.  */

  if (tree then_goto = find_goto_label (then_))
    {
      true_label = GOTO_DESTINATION (then_goto);
      then_ = NULL;
      then_se = false;
    }

  if (tree else_goto = find_goto_label (else_))
    {
      false_label = GOTO_DESTINATION (else_goto);
      else_ = NULL;
      else_se = false;
    }

  /* If we aren't hijacking a label for the 'then' branch, it falls through.  */
  if (true_label)
    true_label_p = &true_label;
  else
    true_label_p = NULL;

  /* The 'else' branch also needs a label if it contains interesting code.  */
  if (false_label || else_se)
    false_label_p = &false_label;
  else
    false_label_p = NULL;

  /* If there was nothing else in our arms, just forward the label(s).  */
  if (!then_se && !else_se)
    return shortcut_cond_r (pred, true_label_p, false_label_p,
			    EXPR_LOC_OR_LOC (expr, input_location));

  /* If our last subexpression already has a terminal label, reuse it.  */
  if (else_se)
    t = expr_last (else_);
  else if (then_se)
    t = expr_last (then_);
  else
    t = NULL;
  if (t && TREE_CODE (t) == LABEL_EXPR)
    end_label = LABEL_EXPR_LABEL (t);

  /* If we don't care about jumping to the 'else' branch, jump to the end
     if the condition is false.  */
  if (!false_label_p)
    false_label_p = &end_label;

  /* We only want to emit these labels if we aren't hijacking them.  */
  emit_end = (end_label == NULL_TREE);
  emit_false = (false_label == NULL_TREE);

  /* We only emit the jump over the else clause if we have to--if the
     then clause may fall through.  Otherwise we can wind up with a
     useless jump and a useless label at the end of gimplified code,
     which will cause us to think that this conditional as a whole
     falls through even if it doesn't.  If we then inline a function
     which ends with such a condition, that can cause us to issue an
     inappropriate warning about control reaching the end of a
     non-void function.  */
  jump_over_else = block_may_fallthru (then_);

  pred = shortcut_cond_r (pred, true_label_p, false_label_p,
			  EXPR_LOC_OR_LOC (expr, input_location));

  expr = NULL;
  append_to_statement_list (pred, &expr);

  append_to_statement_list (then_, &expr);
  if (else_se)
    {
      if (jump_over_else)
	{
	  tree last = expr_last (expr);
	  t = build_and_jump (&end_label);
	  if (rexpr_has_location (last))
	    SET_EXPR_LOCATION (t, rexpr_location (last));
	  append_to_statement_list (t, &expr);
	}
      if (emit_false)
	{
	  t = build1 (LABEL_EXPR, void_type_node, false_label);
	  append_to_statement_list (t, &expr);
	}
      append_to_statement_list (else_, &expr);
    }
  if (emit_end && end_label)
    {
      t = build1 (LABEL_EXPR, void_type_node, end_label);
      append_to_statement_list (t, &expr);
    }

  return expr;
}

/* EXPR is used in a boolean context; make sure it has BOOLEAN_TYPE.  */

tree
gimple_boolify (tree expr)
{
  tree type = TREE_TYPE (expr);
  location_t loc = EXPR_LOCATION (expr);

  if (TREE_CODE (expr) == NE_EXPR
      && TREE_CODE (TREE_OPERAND (expr, 0)) == CALL_EXPR
      && integer_zerop (TREE_OPERAND (expr, 1)))
    {
      tree call = TREE_OPERAND (expr, 0);
      tree fn = get_callee_fndecl (call);

      /* For __builtin_expect ((long) (x), y) recurse into x as well
	 if x is truth_value_p.  */
      if (fn
	  && DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (fn) == BUILT_IN_EXPECT
	  && call_expr_nargs (call) == 2)
	{
	  tree arg = CALL_EXPR_ARG (call, 0);
	  if (arg)
	    {
	      if (TREE_CODE (arg) == NOP_EXPR
		  && TREE_TYPE (arg) == TREE_TYPE (call))
		arg = TREE_OPERAND (arg, 0);
	      if (truth_value_p (TREE_CODE (arg)))
		{
		  arg = gimple_boolify (arg);
		  CALL_EXPR_ARG (call, 0)
		    = fold_convert_loc (loc, TREE_TYPE (call), arg);
		}
	    }
	}
    }

  switch (TREE_CODE (expr))
    {
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      /* Also boolify the arguments of truth exprs.  */
      TREE_OPERAND (expr, 1) = gimple_boolify (TREE_OPERAND (expr, 1));
      /* FALLTHRU */

    case TRUTH_NOT_EXPR:
      TREE_OPERAND (expr, 0) = gimple_boolify (TREE_OPERAND (expr, 0));

      /* These expressions always produce boolean results.  */
      if (TREE_CODE (type) != BOOLEAN_TYPE)
	TREE_TYPE (expr) = boolean_type_node;
      return expr;

    case ANNOTATE_EXPR:
      switch ((enum annot_expr_kind) TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)))
	{
	case annot_expr_ivdep_kind:
	case annot_expr_unroll_kind:
	case annot_expr_no_vector_kind:
	case annot_expr_vector_kind:
	case annot_expr_parallel_kind:
	  TREE_OPERAND (expr, 0) = gimple_boolify (TREE_OPERAND (expr, 0));
	  if (TREE_CODE (type) != BOOLEAN_TYPE)
	    TREE_TYPE (expr) = boolean_type_node;
	  return expr;
	default:
	  gcc_unreachable ();
	}

    default:
      if (COMPARISON_CLASS_P (expr))
	{
	  /* There expressions always prduce boolean results.  */
	  if (TREE_CODE (type) != BOOLEAN_TYPE)
	    TREE_TYPE (expr) = boolean_type_node;
	  return expr;
	}
      /* Other expressions that get here must have boolean values, but
	 might need to be converted to the appropriate mode.  */
      if (TREE_CODE (type) == BOOLEAN_TYPE)
	return expr;
      return fold_convert_loc (loc, boolean_type_node, expr);
    }
}

/* Given a conditional expression *EXPR_P without side effects, gimplify
   its operands.  New statements are inserted to PRE_P.  */

static enum gimplify_status
gimplify_pure_cond_expr (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p, cond;
  enum gimplify_status ret, tret;
  enum tree_code code;

  cond = gimple_boolify (COND_EXPR_COND (expr));

  /* We need to handle && and || specially, as their gimplification
     creates pure cond_expr, thus leading to an infinite cycle otherwise.  */
  code = TREE_CODE (cond);
  if (code == TRUTH_ANDIF_EXPR)
    TREE_SET_CODE (cond, TRUTH_AND_EXPR);
  else if (code == TRUTH_ORIF_EXPR)
    TREE_SET_CODE (cond, TRUTH_OR_EXPR);
  ret = gimplify_expr (&cond, pre_p, NULL, is_gimple_condexpr, fb_rvalue);
  COND_EXPR_COND (*expr_p) = cond;

  tret = gimplify_expr (&COND_EXPR_THEN (expr), pre_p, NULL,
				   is_gimple_val, fb_rvalue);
  ret = MIN (ret, tret);
  tret = gimplify_expr (&COND_EXPR_ELSE (expr), pre_p, NULL,
				   is_gimple_val, fb_rvalue);

  return MIN (ret, tret);
}

/* Return true if evaluating EXPR could trap.
   EXPR is GENERIC, while tree_could_trap_p can be called
   only on GIMPLE.  */

static bool
generic_expr_could_trap_p (tree expr)
{
  unsigned i, n;

  if (!expr || is_gimple_val (expr))
    return false;

  if (!EXPR_P (expr) || tree_could_trap_p (expr))
    return true;

  n = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < n; i++)
    if (generic_expr_could_trap_p (TREE_OPERAND (expr, i)))
      return true;

  return false;
}

/*  Convert the conditional expression pointed to by EXPR_P '(p) ? a : b;'
    into

    if (p)			if (p)
      t1 = a;			  a;
    else		or	else
      t1 = b;			  b;
    t1;

    The second form is used when *EXPR_P is of type void.

    PRE_P points to the list where side effects that must happen before
      *EXPR_P should be stored.  */

static enum gimplify_status
gimplify_cond_expr (tree *expr_p, gimple_seq *pre_p, fallback_t fallback)
{
  tree expr = *expr_p;
  tree type = TREE_TYPE (expr);
  location_t loc = EXPR_LOCATION (expr);
  tree tmp, arm1, arm2;
  enum gimplify_status ret;
  tree label_true, label_false, label_cont;
  bool have_then_clause_p, have_else_clause_p;
  gcond *cond_stmt;
  enum tree_code pred_code;
  gimple_seq seq = NULL;

  /* If this COND_EXPR has a value, copy the values into a temporary within
     the arms.  */
  if (!VOID_TYPE_P (type))
    {
      tree then_ = TREE_OPERAND (expr, 1), else_ = TREE_OPERAND (expr, 2);
      tree result;

      /* If either an rvalue is ok or we do not require an lvalue, create the
	 temporary.  But we cannot do that if the type is addressable.  */
      if (((fallback & fb_rvalue) || !(fallback & fb_lvalue))
	  && !TREE_ADDRESSABLE (type))
	{
	  if (gimplify_ctxp->allow_rhs_cond_expr
	      /* If either branch has side effects or could trap, it can't be
		 evaluated unconditionally.  */
	      && !TREE_SIDE_EFFECTS (then_)
	      && !generic_expr_could_trap_p (then_)
	      && !TREE_SIDE_EFFECTS (else_)
	      && !generic_expr_could_trap_p (else_))
	    return gimplify_pure_cond_expr (expr_p, pre_p);

	  tmp = create_tmp_var (type, "iftmp");
	  result = tmp;
	}

      /* Otherwise, only create and copy references to the values.  */
      else
	{
	  type = build_pointer_type (type);

	  if (!VOID_TYPE_P (TREE_TYPE (then_)))
	    then_ = build_fold_addr_expr_loc (loc, then_);

	  if (!VOID_TYPE_P (TREE_TYPE (else_)))
	    else_ = build_fold_addr_expr_loc (loc, else_);
 
	  expr
	    = build3 (COND_EXPR, type, TREE_OPERAND (expr, 0), then_, else_);

	  tmp = create_tmp_var (type, "iftmp");
	  result = build_simple_mem_ref_loc (loc, tmp);
	}

      /* Build the new then clause, `tmp = then_;'.  But don't build the
	 assignment if the value is void; in C++ it can be if it's a throw.  */
      if (!VOID_TYPE_P (TREE_TYPE (then_)))
	TREE_OPERAND (expr, 1) = build2 (MODIFY_EXPR, type, tmp, then_);

      /* Similarly, build the new else clause, `tmp = else_;'.  */
      if (!VOID_TYPE_P (TREE_TYPE (else_)))
	TREE_OPERAND (expr, 2) = build2 (MODIFY_EXPR, type, tmp, else_);

      TREE_TYPE (expr) = void_type_node;
      recalculate_side_effects (expr);

      /* Move the COND_EXPR to the prequeue.  */
      gimplify_stmt (&expr, pre_p);

      *expr_p = result;
      return GS_ALL_DONE;
    }

  /* Remove any COMPOUND_EXPR so the following cases will be caught.  */
  STRIP_TYPE_NOPS (TREE_OPERAND (expr, 0));
  if (TREE_CODE (TREE_OPERAND (expr, 0)) == COMPOUND_EXPR)
    gimplify_compound_expr (&TREE_OPERAND (expr, 0), pre_p, true);

  /* Make sure the condition has BOOLEAN_TYPE.  */
  TREE_OPERAND (expr, 0) = gimple_boolify (TREE_OPERAND (expr, 0));

  /* Break apart && and || conditions.  */
  if (TREE_CODE (TREE_OPERAND (expr, 0)) == TRUTH_ANDIF_EXPR
      || TREE_CODE (TREE_OPERAND (expr, 0)) == TRUTH_ORIF_EXPR)
    {
      expr = shortcut_cond_expr (expr);

      if (expr != *expr_p)
	{
	  *expr_p = expr;

	  /* We can't rely on gimplify_expr to re-gimplify the expanded
	     form properly, as cleanups might cause the target labels to be
	     wrapped in a TRY_FINALLY_EXPR.  To prevent that, we need to
	     set up a conditional context.  */
	  gimple_push_condition ();
	  gimplify_stmt (expr_p, &seq);
	  gimple_pop_condition (pre_p);
	  gimple_seq_add_seq (pre_p, seq);

	  return GS_ALL_DONE;
	}
    }

  /* Now do the normal gimplification.  */

  /* Gimplify condition.  */
  ret = gimplify_expr (&TREE_OPERAND (expr, 0), pre_p, NULL, is_gimple_condexpr,
		       fb_rvalue);
  if (ret == GS_ERROR)
    return GS_ERROR;
  gcc_assert (TREE_OPERAND (expr, 0) != NULL_TREE);

  gimple_push_condition ();

  have_then_clause_p = have_else_clause_p = false;
  label_true = find_goto_label (TREE_OPERAND (expr, 1));
  if (label_true
      && DECL_CONTEXT (GOTO_DESTINATION (label_true)) == current_function_decl
      /* For -O0 avoid this optimization if the COND_EXPR and GOTO_EXPR
	 have different locations, otherwise we end up with incorrect
	 location information on the branches.  */
      && (optimize
	  || !EXPR_HAS_LOCATION (expr)
	  || !rexpr_has_location (label_true)
	  || EXPR_LOCATION (expr) == rexpr_location (label_true)))
    {
      have_then_clause_p = true;
      label_true = GOTO_DESTINATION (label_true);
    }
  else
    label_true = create_artificial_label (UNKNOWN_LOCATION);
  label_false = find_goto_label (TREE_OPERAND (expr, 2));
  if (label_false
      && DECL_CONTEXT (GOTO_DESTINATION (label_false)) == current_function_decl
      /* For -O0 avoid this optimization if the COND_EXPR and GOTO_EXPR
	 have different locations, otherwise we end up with incorrect
	 location information on the branches.  */
      && (optimize
	  || !EXPR_HAS_LOCATION (expr)
	  || !rexpr_has_location (label_false)
	  || EXPR_LOCATION (expr) == rexpr_location (label_false)))
    {
      have_else_clause_p = true;
      label_false = GOTO_DESTINATION (label_false);
    }
  else
    label_false = create_artificial_label (UNKNOWN_LOCATION);

  gimple_cond_get_ops_from_tree (COND_EXPR_COND (expr), &pred_code, &arm1,
				 &arm2);
  cond_stmt = gimple_build_cond (pred_code, arm1, arm2, label_true,
				 label_false);
  gimple_set_no_warning (cond_stmt, TREE_NO_WARNING (COND_EXPR_COND (expr)));
  gimplify_seq_add_stmt (&seq, cond_stmt);
  gimple_stmt_iterator gsi = gsi_last (seq);
  maybe_fold_stmt (&gsi);

  label_cont = NULL_TREE;
  if (!have_then_clause_p)
    {
      /* For if (...) {} else { code; } put label_true after
	 the else block.  */
      if (TREE_OPERAND (expr, 1) == NULL_TREE
	  && !have_else_clause_p
	  && TREE_OPERAND (expr, 2) != NULL_TREE)
	label_cont = label_true;
      else
	{
	  gimplify_seq_add_stmt (&seq, gimple_build_label (label_true));
	  have_then_clause_p = gimplify_stmt (&TREE_OPERAND (expr, 1), &seq);
	  /* For if (...) { code; } else {} or
	     if (...) { code; } else goto label; or
	     if (...) { code; return; } else { ... }
	     label_cont isn't needed.  */
	  if (!have_else_clause_p
	      && TREE_OPERAND (expr, 2) != NULL_TREE
	      && gimple_seq_may_fallthru (seq))
	    {
	      gimple *g;
	      label_cont = create_artificial_label (UNKNOWN_LOCATION);

	      g = gimple_build_goto (label_cont);

	      /* GIMPLE_COND's are very low level; they have embedded
		 gotos.  This particular embedded goto should not be marked
		 with the location of the original COND_EXPR, as it would
		 correspond to the COND_EXPR's condition, not the ELSE or the
		 THEN arms.  To avoid marking it with the wrong location, flag
		 it as "no location".  */
	      gimple_set_do_not_emit_location (g);

	      gimplify_seq_add_stmt (&seq, g);
	    }
	}
    }
  if (!have_else_clause_p)
    {
      gimplify_seq_add_stmt (&seq, gimple_build_label (label_false));
      have_else_clause_p = gimplify_stmt (&TREE_OPERAND (expr, 2), &seq);
    }
  if (label_cont)
    gimplify_seq_add_stmt (&seq, gimple_build_label (label_cont));

  gimple_pop_condition (pre_p);
  gimple_seq_add_seq (pre_p, seq);

  if (ret == GS_ERROR)
    ; /* Do nothing.  */
  else if (have_then_clause_p || have_else_clause_p)
    ret = GS_ALL_DONE;
  else
    {
      /* Both arms are empty; replace the COND_EXPR with its predicate.  */
      expr = TREE_OPERAND (expr, 0);
      gimplify_stmt (&expr, pre_p);
    }

  *expr_p = NULL;
  return ret;
}

/* Prepare the node pointed to by EXPR_P, an is_gimple_addressable expression,
   to be marked addressable.

   We cannot rely on such an expression being directly markable if a temporary
   has been created by the gimplification.  In this case, we create another
   temporary and initialize it with a copy, which will become a store after we
   mark it addressable.  This can happen if the front-end passed us something
   that it could not mark addressable yet, like a Fortran pass-by-reference
   parameter (int) floatvar.  */

static void
prepare_gimple_addressable (tree *expr_p, gimple_seq *seq_p)
{
  while (handled_component_p (*expr_p))
    expr_p = &TREE_OPERAND (*expr_p, 0);
  if (is_gimple_reg (*expr_p))
    {
      /* Do not allow an SSA name as the temporary.  */
      tree var = get_initialized_tmp_var (*expr_p, seq_p, NULL, false);
      DECL_GIMPLE_REG_P (var) = 0;
      *expr_p = var;
    }
}

/* A subroutine of gimplify_modify_expr.  Replace a MODIFY_EXPR with
   a call to __builtin_memcpy.  */

static enum gimplify_status
gimplify_modify_expr_to_memcpy (tree *expr_p, tree size, bool want_value,
    				gimple_seq *seq_p)
{
  tree t, to, to_ptr, from, from_ptr;
  gcall *gs;
  location_t loc = EXPR_LOCATION (*expr_p);

  to = TREE_OPERAND (*expr_p, 0);
  from = TREE_OPERAND (*expr_p, 1);

  /* Mark the RHS addressable.  Beware that it may not be possible to do so
     directly if a temporary has been created by the gimplification.  */
  prepare_gimple_addressable (&from, seq_p);

  mark_addressable (from);
  from_ptr = build_fold_addr_expr_loc (loc, from);
  gimplify_arg (&from_ptr, seq_p, loc);

  mark_addressable (to);
  to_ptr = build_fold_addr_expr_loc (loc, to);
  gimplify_arg (&to_ptr, seq_p, loc);

  t = builtin_decl_implicit (BUILT_IN_MEMCPY);

  gs = gimple_build_call (t, 3, to_ptr, from_ptr, size);

  if (want_value)
    {
      /* tmp = memcpy() */
      t = create_tmp_var (TREE_TYPE (to_ptr));
      gimple_call_set_lhs (gs, t);
      gimplify_seq_add_stmt (seq_p, gs);

      *expr_p = build_simple_mem_ref (t);
      return GS_ALL_DONE;
    }

  gimplify_seq_add_stmt (seq_p, gs);
  *expr_p = NULL;
  return GS_ALL_DONE;
}

/* A subroutine of gimplify_modify_expr.  Replace a MODIFY_EXPR with
   a call to __builtin_memset.  In this case we know that the RHS is
   a CONSTRUCTOR with an empty element list.  */

static enum gimplify_status
gimplify_modify_expr_to_memset (tree *expr_p, tree size, bool want_value,
    				gimple_seq *seq_p)
{
  tree t, from, to, to_ptr;
  gcall *gs;
  location_t loc = EXPR_LOCATION (*expr_p);

  /* Assert our assumptions, to abort instead of producing wrong code
     silently if they are not met.  Beware that the RHS CONSTRUCTOR might
     not be immediately exposed.  */
  from = TREE_OPERAND (*expr_p, 1);
  if (TREE_CODE (from) == WITH_SIZE_EXPR)
    from = TREE_OPERAND (from, 0);

  gcc_assert (TREE_CODE (from) == CONSTRUCTOR
	      && vec_safe_is_empty (CONSTRUCTOR_ELTS (from)));

  /* Now proceed.  */
  to = TREE_OPERAND (*expr_p, 0);

  to_ptr = build_fold_addr_expr_loc (loc, to);
  gimplify_arg (&to_ptr, seq_p, loc);
  t = builtin_decl_implicit (BUILT_IN_MEMSET);

  gs = gimple_build_call (t, 3, to_ptr, integer_zero_node, size);

  if (want_value)
    {
      /* tmp = memset() */
      t = create_tmp_var (TREE_TYPE (to_ptr));
      gimple_call_set_lhs (gs, t);
      gimplify_seq_add_stmt (seq_p, gs);

      *expr_p = build1 (INDIRECT_REF, TREE_TYPE (to), t);
      return GS_ALL_DONE;
    }

  gimplify_seq_add_stmt (seq_p, gs);
  *expr_p = NULL;
  return GS_ALL_DONE;
}

/* A subroutine of gimplify_init_ctor_preeval.  Called via walk_tree,
   determine, cautiously, if a CONSTRUCTOR overlaps the lhs of an
   assignment.  Return non-null if we detect a potential overlap.  */

struct gimplify_init_ctor_preeval_data
{
  /* The base decl of the lhs object.  May be NULL, in which case we
     have to assume the lhs is indirect.  */
  tree lhs_base_decl;

  /* The alias set of the lhs object.  */
  alias_set_type lhs_alias_set;
};

static tree
gimplify_init_ctor_preeval_1 (tree *tp, int *walk_subtrees, void *xdata)
{
  struct gimplify_init_ctor_preeval_data *data
    = (struct gimplify_init_ctor_preeval_data *) xdata;
  tree t = *tp;

  /* If we find the base object, obviously we have overlap.  */
  if (data->lhs_base_decl == t)
    return t;

  /* If the constructor component is indirect, determine if we have a
     potential overlap with the lhs.  The only bits of information we
     have to go on at this point are addressability and alias sets.  */
  if ((INDIRECT_REF_P (t)
       || TREE_CODE (t) == MEM_REF)
      && (!data->lhs_base_decl || TREE_ADDRESSABLE (data->lhs_base_decl))
      && alias_sets_conflict_p (data->lhs_alias_set, get_alias_set (t)))
    return t;

  /* If the constructor component is a call, determine if it can hide a
     potential overlap with the lhs through an INDIRECT_REF like above.
     ??? Ugh - this is completely broken.  In fact this whole analysis
     doesn't look conservative.  */
  if (TREE_CODE (t) == CALL_EXPR)
    {
      tree type, fntype = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (t)));

      for (type = TYPE_ARG_TYPES (fntype); type; type = TREE_CHAIN (type))
	if (POINTER_TYPE_P (TREE_VALUE (type))
	    && (!data->lhs_base_decl || TREE_ADDRESSABLE (data->lhs_base_decl))
	    && alias_sets_conflict_p (data->lhs_alias_set,
				      get_alias_set
				        (TREE_TYPE (TREE_VALUE (type)))))
	  return t;
    }

  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;
  return NULL;
}

/* A subroutine of gimplify_init_constructor.  Pre-evaluate EXPR,
   force values that overlap with the lhs (as described by *DATA)
   into temporaries.  */

static void
gimplify_init_ctor_preeval (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
			    struct gimplify_init_ctor_preeval_data *data)
{
  enum gimplify_status one;

  /* If the value is constant, then there's nothing to pre-evaluate.  */
  if (TREE_CONSTANT (*expr_p))
    {
      /* Ensure it does not have side effects, it might contain a reference to
	 the object we're initializing.  */
      gcc_assert (!TREE_SIDE_EFFECTS (*expr_p));
      return;
    }

  /* If the type has non-trivial constructors, we can't pre-evaluate.  */
  if (TREE_ADDRESSABLE (TREE_TYPE (*expr_p)))
    return;

  /* Recurse for nested constructors.  */
  if (TREE_CODE (*expr_p) == CONSTRUCTOR)
    {
      unsigned HOST_WIDE_INT ix;
      constructor_elt *ce;
      vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (*expr_p);

      FOR_EACH_VEC_SAFE_ELT (v, ix, ce)
	gimplify_init_ctor_preeval (&ce->value, pre_p, post_p, data);

      return;
    }

  /* If this is a variable sized type, we must remember the size.  */
  maybe_with_size_expr (expr_p);

  /* Gimplify the constructor element to something appropriate for the rhs
     of a MODIFY_EXPR.  Given that we know the LHS is an aggregate, we know
     the gimplifier will consider this a store to memory.  Doing this
     gimplification now means that we won't have to deal with complicated
     language-specific trees, nor trees like SAVE_EXPR that can induce
     exponential search behavior.  */
  one = gimplify_expr (expr_p, pre_p, post_p, is_gimple_mem_rhs, fb_rvalue);
  if (one == GS_ERROR)
    {
      *expr_p = NULL;
      return;
    }

  /* If we gimplified to a bare decl, we can be sure that it doesn't overlap
     with the lhs, since "a = { .x=a }" doesn't make sense.  This will
     always be true for all scalars, since is_gimple_mem_rhs insists on a
     temporary variable for them.  */
  if (DECL_P (*expr_p))
    return;

  /* If this is of variable size, we have no choice but to assume it doesn't
     overlap since we can't make a temporary for it.  */
  if (TREE_CODE (TYPE_SIZE (TREE_TYPE (*expr_p))) != INTEGER_CST)
    return;

  /* Otherwise, we must search for overlap ...  */
  if (!walk_tree (expr_p, gimplify_init_ctor_preeval_1, data, NULL))
    return;

  /* ... and if found, force the value into a temporary.  */
  *expr_p = get_formal_tmp_var (*expr_p, pre_p);
}

/* A subroutine of gimplify_init_ctor_eval.  Create a loop for
   a RANGE_EXPR in a CONSTRUCTOR for an array.

      var = lower;
    loop_entry:
      object[var] = value;
      if (var == upper)
	goto loop_exit;
      var = var + 1;
      goto loop_entry;
    loop_exit:

   We increment var _after_ the loop exit check because we might otherwise
   fail if upper == TYPE_MAX_VALUE (type for upper).

   Note that we never have to deal with SAVE_EXPRs here, because this has
   already been taken care of for us, in gimplify_init_ctor_preeval().  */

static void gimplify_init_ctor_eval (tree, vec<constructor_elt, va_gc> *,
				     gimple_seq *, bool);

static void
gimplify_init_ctor_eval_range (tree object, tree lower, tree upper,
			       tree value, tree array_elt_type,
			       gimple_seq *pre_p, bool cleared)
{
  tree loop_entry_label, loop_exit_label, fall_thru_label;
  tree var, var_type, cref, tmp;

  loop_entry_label = create_artificial_label (UNKNOWN_LOCATION);
  loop_exit_label = create_artificial_label (UNKNOWN_LOCATION);
  fall_thru_label = create_artificial_label (UNKNOWN_LOCATION);

  /* Create and initialize the index variable.  */
  var_type = TREE_TYPE (upper);
  var = create_tmp_var (var_type);
  gimplify_seq_add_stmt (pre_p, gimple_build_assign (var, lower));

  /* Add the loop entry label.  */
  gimplify_seq_add_stmt (pre_p, gimple_build_label (loop_entry_label));

  /* Build the reference.  */
  cref = build4 (ARRAY_REF, array_elt_type, unshare_expr (object),
		 var, NULL_TREE, NULL_TREE);

  /* If we are a constructor, just call gimplify_init_ctor_eval to do
     the store.  Otherwise just assign value to the reference.  */

  if (TREE_CODE (value) == CONSTRUCTOR)
    /* NB we might have to call ourself recursively through
       gimplify_init_ctor_eval if the value is a constructor.  */
    gimplify_init_ctor_eval (cref, CONSTRUCTOR_ELTS (value),
			     pre_p, cleared);
  else
    gimplify_seq_add_stmt (pre_p, gimple_build_assign (cref, value));

  /* We exit the loop when the index var is equal to the upper bound.  */
  gimplify_seq_add_stmt (pre_p,
			 gimple_build_cond (EQ_EXPR, var, upper,
					    loop_exit_label, fall_thru_label));

  gimplify_seq_add_stmt (pre_p, gimple_build_label (fall_thru_label));

  /* Otherwise, increment the index var...  */
  tmp = build2 (PLUS_EXPR, var_type, var,
		fold_convert (var_type, integer_one_node));
  gimplify_seq_add_stmt (pre_p, gimple_build_assign (var, tmp));

  /* ...and jump back to the loop entry.  */
  gimplify_seq_add_stmt (pre_p, gimple_build_goto (loop_entry_label));

  /* Add the loop exit label.  */
  gimplify_seq_add_stmt (pre_p, gimple_build_label (loop_exit_label));
}

/* Return true if FDECL is accessing a field that is zero sized.  */

static bool
zero_sized_field_decl (const_tree fdecl)
{
  if (TREE_CODE (fdecl) == FIELD_DECL && DECL_SIZE (fdecl)
      && integer_zerop (DECL_SIZE (fdecl)))
    return true;
  return false;
}

/* Return true if TYPE is zero sized.  */

static bool
zero_sized_type (const_tree type)
{
  if (AGGREGATE_TYPE_P (type) && TYPE_SIZE (type)
      && integer_zerop (TYPE_SIZE (type)))
    return true;
  return false;
}

/* A subroutine of gimplify_init_constructor.  Generate individual
   MODIFY_EXPRs for a CONSTRUCTOR.  OBJECT is the LHS against which the
   assignments should happen.  ELTS is the CONSTRUCTOR_ELTS of the
   CONSTRUCTOR.  CLEARED is true if the entire LHS object has been
   zeroed first.  */

static void
gimplify_init_ctor_eval (tree object, vec<constructor_elt, va_gc> *elts,
			 gimple_seq *pre_p, bool cleared)
{
  tree array_elt_type = NULL;
  unsigned HOST_WIDE_INT ix;
  tree purpose, value;

  if (TREE_CODE (TREE_TYPE (object)) == ARRAY_TYPE)
    array_elt_type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (object)));

  FOR_EACH_CONSTRUCTOR_ELT (elts, ix, purpose, value)
    {
      tree cref;

      /* NULL values are created above for gimplification errors.  */
      if (value == NULL)
	continue;

      if (cleared && initializer_zerop (value))
	continue;

      /* ??? Here's to hoping the front end fills in all of the indices,
	 so we don't have to figure out what's missing ourselves.  */
      gcc_assert (purpose);

      /* Skip zero-sized fields, unless value has side-effects.  This can
	 happen with calls to functions returning a zero-sized type, which
	 we shouldn't discard.  As a number of downstream passes don't
	 expect sets of zero-sized fields, we rely on the gimplification of
	 the MODIFY_EXPR we make below to drop the assignment statement.  */
      if (! TREE_SIDE_EFFECTS (value) && zero_sized_field_decl (purpose))
	continue;

      /* If we have a RANGE_EXPR, we have to build a loop to assign the
	 whole range.  */
      if (TREE_CODE (purpose) == RANGE_EXPR)
	{
	  tree lower = TREE_OPERAND (purpose, 0);
	  tree upper = TREE_OPERAND (purpose, 1);

	  /* If the lower bound is equal to upper, just treat it as if
	     upper was the index.  */
	  if (simple_cst_equal (lower, upper))
	    purpose = upper;
	  else
	    {
	      gimplify_init_ctor_eval_range (object, lower, upper, value,
					     array_elt_type, pre_p, cleared);
	      continue;
	    }
	}

      if (array_elt_type)
	{
	  /* Do not use bitsizetype for ARRAY_REF indices.  */
	  if (TYPE_DOMAIN (TREE_TYPE (object)))
	    purpose
	      = fold_convert (TREE_TYPE (TYPE_DOMAIN (TREE_TYPE (object))),
			      purpose);
	  cref = build4 (ARRAY_REF, array_elt_type, unshare_expr (object),
			 purpose, NULL_TREE, NULL_TREE);
	}
      else
	{
	  gcc_assert (TREE_CODE (purpose) == FIELD_DECL);
	  cref = build3 (COMPONENT_REF, TREE_TYPE (purpose),
			 unshare_expr (object), purpose, NULL_TREE);
	}

      if (TREE_CODE (value) == CONSTRUCTOR
	  && TREE_CODE (TREE_TYPE (value)) != VECTOR_TYPE)
	gimplify_init_ctor_eval (cref, CONSTRUCTOR_ELTS (value),
				 pre_p, cleared);
      else
	{
	  tree init = build2 (INIT_EXPR, TREE_TYPE (cref), cref, value);
	  gimplify_and_add (init, pre_p);
	  ggc_free (init);
	}
    }
}

/* Return the appropriate RHS predicate for this LHS.  */

gimple_predicate
rhs_predicate_for (tree lhs)
{
  if (is_gimple_reg (lhs))
    return is_gimple_reg_rhs_or_call;
  else
    return is_gimple_mem_rhs_or_call;
}

/* Return the initial guess for an appropriate RHS predicate for this LHS,
   before the LHS has been gimplified.  */

static gimple_predicate
initial_rhs_predicate_for (tree lhs)
{
  if (is_gimple_reg_type (TREE_TYPE (lhs)))
    return is_gimple_reg_rhs_or_call;
  else
    return is_gimple_mem_rhs_or_call;
}

/* Gimplify a C99 compound literal expression.  This just means adding
   the DECL_EXPR before the current statement and using its anonymous
   decl instead.  */

static enum gimplify_status
gimplify_compound_literal_expr (tree *expr_p, gimple_seq *pre_p,
				bool (*gimple_test_f) (tree),
				fallback_t fallback)
{
  tree decl_s = COMPOUND_LITERAL_EXPR_DECL_EXPR (*expr_p);
  tree decl = DECL_EXPR_DECL (decl_s);
  tree init = DECL_INITIAL (decl);
  /* Mark the decl as addressable if the compound literal
     expression is addressable now, otherwise it is marked too late
     after we gimplify the initialization expression.  */
  if (TREE_ADDRESSABLE (*expr_p))
    TREE_ADDRESSABLE (decl) = 1;
  /* Otherwise, if we don't need an lvalue and have a literal directly
     substitute it.  Check if it matches the gimple predicate, as
     otherwise we'd generate a new temporary, and we can as well just
     use the decl we already have.  */
  else if (!TREE_ADDRESSABLE (decl)
	   && init
	   && (fallback & fb_lvalue) == 0
	   && gimple_test_f (init))
    {
      *expr_p = init;
      return GS_OK;
    }

  /* Preliminarily mark non-addressed complex variables as eligible
     for promotion to gimple registers.  We'll transform their uses
     as we find them.  */
  if ((TREE_CODE (TREE_TYPE (decl)) == COMPLEX_TYPE
       || TREE_CODE (TREE_TYPE (decl)) == VECTOR_TYPE)
      && !TREE_THIS_VOLATILE (decl)
      && !needs_to_live_in_memory (decl))
    DECL_GIMPLE_REG_P (decl) = 1;

  /* If the decl is not addressable, then it is being used in some
     expression or on the right hand side of a statement, and it can
     be put into a readonly data section.  */
  if (!TREE_ADDRESSABLE (decl) && (fallback & fb_lvalue) == 0)
    TREE_READONLY (decl) = 1;

  /* This decl isn't mentioned in the enclosing block, so add it to the
     list of temps.  FIXME it seems a bit of a kludge to say that
     anonymous artificial vars aren't pushed, but everything else is.  */
  if (DECL_NAME (decl) == NULL_TREE && !DECL_SEEN_IN_BIND_EXPR_P (decl))
    gimple_add_tmp_var (decl);

  gimplify_and_add (decl_s, pre_p);
  *expr_p = decl;
  return GS_OK;
}

/* Optimize embedded COMPOUND_LITERAL_EXPRs within a CONSTRUCTOR,
   return a new CONSTRUCTOR if something changed.  */

static tree
optimize_compound_literals_in_ctor (tree orig_ctor)
{
  tree ctor = orig_ctor;
  vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ctor);
  unsigned int idx, num = vec_safe_length (elts);

  for (idx = 0; idx < num; idx++)
    {
      tree value = (*elts)[idx].value;
      tree newval = value;
      if (TREE_CODE (value) == CONSTRUCTOR)
	newval = optimize_compound_literals_in_ctor (value);
      else if (TREE_CODE (value) == COMPOUND_LITERAL_EXPR)
	{
	  tree decl_s = COMPOUND_LITERAL_EXPR_DECL_EXPR (value);
	  tree decl = DECL_EXPR_DECL (decl_s);
	  tree init = DECL_INITIAL (decl);

	  if (!TREE_ADDRESSABLE (value)
	      && !TREE_ADDRESSABLE (decl)
	      && init
	      && TREE_CODE (init) == CONSTRUCTOR)
	    newval = optimize_compound_literals_in_ctor (init);
	}
      if (newval == value)
	continue;

      if (ctor == orig_ctor)
	{
	  ctor = copy_node (orig_ctor);
	  CONSTRUCTOR_ELTS (ctor) = vec_safe_copy (elts);
	  elts = CONSTRUCTOR_ELTS (ctor);
	}
      (*elts)[idx].value = newval;
    }
  return ctor;
}

/* A subroutine of gimplify_modify_expr.  Break out elements of a
   CONSTRUCTOR used as an initializer into separate MODIFY_EXPRs.

   Note that we still need to clear any elements that don't have explicit
   initializers, so if not all elements are initialized we keep the
   original MODIFY_EXPR, we just remove all of the constructor elements.

   If NOTIFY_TEMP_CREATION is true, do not gimplify, just return
   GS_ERROR if we would have to create a temporary when gimplifying
   this constructor.  Otherwise, return GS_OK.

   If NOTIFY_TEMP_CREATION is false, just do the gimplification.  */

static enum gimplify_status
gimplify_init_constructor (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
			   bool want_value, bool notify_temp_creation)
{
  tree object, ctor, type;
  enum gimplify_status ret;
  vec<constructor_elt, va_gc> *elts;

  gcc_assert (TREE_CODE (TREE_OPERAND (*expr_p, 1)) == CONSTRUCTOR);

  if (!notify_temp_creation)
    {
      ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			   is_gimple_lvalue, fb_lvalue);
      if (ret == GS_ERROR)
	return ret;
    }

  object = TREE_OPERAND (*expr_p, 0);
  ctor = TREE_OPERAND (*expr_p, 1)
    = optimize_compound_literals_in_ctor (TREE_OPERAND (*expr_p, 1));
  type = TREE_TYPE (ctor);
  elts = CONSTRUCTOR_ELTS (ctor);
  ret = GS_ALL_DONE;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case ARRAY_TYPE:
      {
	struct gimplify_init_ctor_preeval_data preeval_data;
	HOST_WIDE_INT num_ctor_elements, num_nonzero_elements;
	bool cleared, complete_p, valid_const_initializer;

	/* Aggregate types must lower constructors to initialization of
	   individual elements.  The exception is that a CONSTRUCTOR node
	   with no elements indicates zero-initialization of the whole.  */
	if (vec_safe_is_empty (elts))
	  {
	    if (notify_temp_creation)
	      return GS_OK;
	    break;
	  }

	/* Fetch information about the constructor to direct later processing.
	   We might want to make static versions of it in various cases, and
	   can only do so if it known to be a valid constant initializer.  */
	valid_const_initializer
	  = categorize_ctor_elements (ctor, &num_nonzero_elements,
				      &num_ctor_elements, &complete_p);

	/* If a const aggregate variable is being initialized, then it
	   should never be a lose to promote the variable to be static.  */
	if (valid_const_initializer
	    && num_nonzero_elements > 1
	    && TREE_READONLY (object)
	    && VAR_P (object)
	    && (flag_merge_constants >= 2 || !TREE_ADDRESSABLE (object)))
	  {
	    if (notify_temp_creation)
	      return GS_ERROR;
	    DECL_INITIAL (object) = ctor;
	    TREE_STATIC (object) = 1;
	    if (!DECL_NAME (object))
	      DECL_NAME (object) = create_tmp_var_name ("C");
	    walk_tree (&DECL_INITIAL (object), force_labels_r, NULL, NULL);

	    /* ??? C++ doesn't automatically append a .<number> to the
	       assembler name, and even when it does, it looks at FE private
	       data structures to figure out what that number should be,
	       which are not set for this variable.  I suppose this is
	       important for local statics for inline functions, which aren't
	       "local" in the object file sense.  So in order to get a unique
	       TU-local symbol, we must invoke the lhd version now.  */
	    lhd_set_decl_assembler_name (object);

	    *expr_p = NULL_TREE;
	    break;
	  }

	/* If there are "lots" of initialized elements, even discounting
	   those that are not address constants (and thus *must* be
	   computed at runtime), then partition the constructor into
	   constant and non-constant parts.  Block copy the constant
	   parts in, then generate code for the non-constant parts.  */
	/* TODO.  There's code in cp/typeck.c to do this.  */

	if (int_size_in_bytes (TREE_TYPE (ctor)) < 0)
	  /* store_constructor will ignore the clearing of variable-sized
	     objects.  Initializers for such objects must explicitly set
	     every field that needs to be set.  */
	  cleared = false;
	else if (!complete_p && !CONSTRUCTOR_NO_CLEARING (ctor))
	  /* If the constructor isn't complete, clear the whole object
	     beforehand, unless CONSTRUCTOR_NO_CLEARING is set on it.

	     ??? This ought not to be needed.  For any element not present
	     in the initializer, we should simply set them to zero.  Except
	     we'd need to *find* the elements that are not present, and that
	     requires trickery to avoid quadratic compile-time behavior in
	     large cases or excessive memory use in small cases.  */
	  cleared = true;
	else if (num_ctor_elements - num_nonzero_elements
		 > CLEAR_RATIO (optimize_function_for_speed_p (cfun))
		 && num_nonzero_elements < num_ctor_elements / 4)
	  /* If there are "lots" of zeros, it's more efficient to clear
	     the memory and then set the nonzero elements.  */
	  cleared = true;
	else
	  cleared = false;

	/* If there are "lots" of initialized elements, and all of them
	   are valid address constants, then the entire initializer can
	   be dropped to memory, and then memcpy'd out.  Don't do this
	   for sparse arrays, though, as it's more efficient to follow
	   the standard CONSTRUCTOR behavior of memset followed by
	   individual element initialization.  Also don't do this for small
	   all-zero initializers (which aren't big enough to merit
	   clearing), and don't try to make bitwise copies of
	   TREE_ADDRESSABLE types.

	   We cannot apply such transformation when compiling chkp static
	   initializer because creation of initializer image in the memory
	   will require static initialization of bounds for it.  It should
	   result in another gimplification of similar initializer and we
	   may fall into infinite loop.  */
	if (valid_const_initializer
	    && !(cleared || num_nonzero_elements == 0)
	    && !TREE_ADDRESSABLE (type)
	    && (!current_function_decl
		|| !lookup_attribute ("chkp ctor",
				      DECL_ATTRIBUTES (current_function_decl))))
	  {
	    HOST_WIDE_INT size = int_size_in_bytes (type);
	    unsigned int align;

	    /* ??? We can still get unbounded array types, at least
	       from the C++ front end.  This seems wrong, but attempt
	       to work around it for now.  */
	    if (size < 0)
	      {
		size = int_size_in_bytes (TREE_TYPE (object));
		if (size >= 0)
		  TREE_TYPE (ctor) = type = TREE_TYPE (object);
	      }

	    /* Find the maximum alignment we can assume for the object.  */
	    /* ??? Make use of DECL_OFFSET_ALIGN.  */
	    if (DECL_P (object))
	      align = DECL_ALIGN (object);
	    else
	      align = TYPE_ALIGN (type);

	    /* Do a block move either if the size is so small as to make
	       each individual move a sub-unit move on average, or if it
	       is so large as to make individual moves inefficient.  */
	    if (size > 0
		&& num_nonzero_elements > 1
		&& (size < num_nonzero_elements
		    || !can_move_by_pieces (size, align)))
	      {
		if (notify_temp_creation)
		  return GS_ERROR;

		walk_tree (&ctor, force_labels_r, NULL, NULL);
		ctor = tree_output_constant_def (ctor);
		if (!useless_type_conversion_p (type, TREE_TYPE (ctor)))
		  ctor = build1 (VIEW_CONVERT_EXPR, type, ctor);
		TREE_OPERAND (*expr_p, 1) = ctor;

		/* This is no longer an assignment of a CONSTRUCTOR, but
		   we still may have processing to do on the LHS.  So
		   pretend we didn't do anything here to let that happen.  */
		return GS_UNHANDLED;
	      }
	  }

	/* If the target is volatile, we have non-zero elements and more than
	   one field to assign, initialize the target from a temporary.  */
	if (TREE_THIS_VOLATILE (object)
	    && !TREE_ADDRESSABLE (type)
	    && num_nonzero_elements > 0
	    && vec_safe_length (elts) > 1)
	  {
	    tree temp = create_tmp_var (TYPE_MAIN_VARIANT (type));
	    TREE_OPERAND (*expr_p, 0) = temp;
	    *expr_p = build2 (COMPOUND_EXPR, TREE_TYPE (*expr_p),
			      *expr_p,
			      build2 (MODIFY_EXPR, void_type_node,
				      object, temp));
	    return GS_OK;
	  }

	if (notify_temp_creation)
	  return GS_OK;

	/* If there are nonzero elements and if needed, pre-evaluate to capture
	   elements overlapping with the lhs into temporaries.  We must do this
	   before clearing to fetch the values before they are zeroed-out.  */
	if (num_nonzero_elements > 0 && TREE_CODE (*expr_p) != INIT_EXPR)
	  {
	    preeval_data.lhs_base_decl = get_base_address (object);
	    if (!DECL_P (preeval_data.lhs_base_decl))
	      preeval_data.lhs_base_decl = NULL;
	    preeval_data.lhs_alias_set = get_alias_set (object);

	    gimplify_init_ctor_preeval (&TREE_OPERAND (*expr_p, 1),
					pre_p, post_p, &preeval_data);
	  }

	bool ctor_has_side_effects_p
	  = TREE_SIDE_EFFECTS (TREE_OPERAND (*expr_p, 1));

	if (cleared)
	  {
	    /* Zap the CONSTRUCTOR element list, which simplifies this case.
	       Note that we still have to gimplify, in order to handle the
	       case of variable sized types.  Avoid shared tree structures.  */
	    CONSTRUCTOR_ELTS (ctor) = NULL;
	    TREE_SIDE_EFFECTS (ctor) = 0;
	    object = unshare_expr (object);
	    gimplify_stmt (expr_p, pre_p);
	  }

	/* If we have not block cleared the object, or if there are nonzero
	   elements in the constructor, or if the constructor has side effects,
	   add assignments to the individual scalar fields of the object.  */
	if (!cleared
	    || num_nonzero_elements > 0
	    || ctor_has_side_effects_p)
	  gimplify_init_ctor_eval (object, elts, pre_p, cleared);

	*expr_p = NULL_TREE;
      }
      break;

    case COMPLEX_TYPE:
      {
	tree r, i;

	if (notify_temp_creation)
	  return GS_OK;

	/* Extract the real and imaginary parts out of the ctor.  */
	gcc_assert (elts->length () == 2);
	r = (*elts)[0].value;
	i = (*elts)[1].value;
	if (r == NULL || i == NULL)
	  {
	    tree zero = build_zero_cst (TREE_TYPE (type));
	    if (r == NULL)
	      r = zero;
	    if (i == NULL)
	      i = zero;
	  }

	/* Complex types have either COMPLEX_CST or COMPLEX_EXPR to
	   represent creation of a complex value.  */
	if (TREE_CONSTANT (r) && TREE_CONSTANT (i))
	  {
	    ctor = build_complex (type, r, i);
	    TREE_OPERAND (*expr_p, 1) = ctor;
	  }
	else
	  {
	    ctor = build2 (COMPLEX_EXPR, type, r, i);
	    TREE_OPERAND (*expr_p, 1) = ctor;
	    ret = gimplify_expr (&TREE_OPERAND (*expr_p, 1),
				 pre_p,
				 post_p,
				 rhs_predicate_for (TREE_OPERAND (*expr_p, 0)),
				 fb_rvalue);
	  }
      }
      break;

    case VECTOR_TYPE:
      {
	unsigned HOST_WIDE_INT ix;
	constructor_elt *ce;

	if (notify_temp_creation)
	  return GS_OK;

	/* Go ahead and simplify constant constructors to VECTOR_CST.  */
	if (TREE_CONSTANT (ctor))
	  {
	    bool constant_p = true;
	    tree value;

	    /* Even when ctor is constant, it might contain non-*_CST
	       elements, such as addresses or trapping values like
	       1.0/0.0 - 1.0/0.0.  Such expressions don't belong
	       in VECTOR_CST nodes.  */
	    FOR_EACH_CONSTRUCTOR_VALUE (elts, ix, value)
	      if (!CONSTANT_CLASS_P (value))
		{
		  constant_p = false;
		  break;
		}

	    if (constant_p)
	      {
		TREE_OPERAND (*expr_p, 1) = build_vector_from_ctor (type, elts);
		break;
	      }

	    TREE_CONSTANT (ctor) = 0;
	  }

	/* Vector types use CONSTRUCTOR all the way through gimple
	   compilation as a general initializer.  */
	FOR_EACH_VEC_SAFE_ELT (elts, ix, ce)
	  {
	    enum gimplify_status tret;
	    tret = gimplify_expr (&ce->value, pre_p, post_p, is_gimple_val,
				  fb_rvalue);
	    if (tret == GS_ERROR)
	      ret = GS_ERROR;
	    else if (TREE_STATIC (ctor)
		     && !initializer_constant_valid_p (ce->value,
						       TREE_TYPE (ce->value)))
	      TREE_STATIC (ctor) = 0;
	  }
	if (!is_gimple_reg (TREE_OPERAND (*expr_p, 0)))
	  TREE_OPERAND (*expr_p, 1) = get_formal_tmp_var (ctor, pre_p);
      }
      break;

    default:
      /* So how did we get a CONSTRUCTOR for a scalar type?  */
      gcc_unreachable ();
    }

  if (ret == GS_ERROR)
    return GS_ERROR;
  /* If we have gimplified both sides of the initializer but have
     not emitted an assignment, do so now.  */
  if (*expr_p)
    {
      tree lhs = TREE_OPERAND (*expr_p, 0);
      tree rhs = TREE_OPERAND (*expr_p, 1);
      if (want_value && object == lhs)
	lhs = unshare_expr (lhs);
      gassign *init = gimple_build_assign (lhs, rhs);
      gimplify_seq_add_stmt (pre_p, init);
    }
  if (want_value)
    {
      *expr_p = object;
      return GS_OK;
    }
  else
    {
      *expr_p = NULL;
      return GS_ALL_DONE;
    }
}

/* Given a pointer value OP0, return a simplified version of an
   indirection through OP0, or NULL_TREE if no simplification is
   possible.  This may only be applied to a rhs of an expression.
   Note that the resulting type may be different from the type pointed
   to in the sense that it is still compatible from the langhooks
   point of view. */

static tree
gimple_fold_indirect_ref_rhs (tree t)
{
  return gimple_fold_indirect_ref (t);
}

/* Subroutine of gimplify_modify_expr to do simplifications of
   MODIFY_EXPRs based on the code of the RHS.  We loop for as long as
   something changes.  */

static enum gimplify_status
gimplify_modify_expr_rhs (tree *expr_p, tree *from_p, tree *to_p,
			  gimple_seq *pre_p, gimple_seq *post_p,
			  bool want_value)
{
  enum gimplify_status ret = GS_UNHANDLED;
  bool changed;

  do
    {
      changed = false;
      switch (TREE_CODE (*from_p))
	{
	case VAR_DECL:
	  /* If we're assigning from a read-only variable initialized with
	     a constructor, do the direct assignment from the constructor,
	     but only if neither source nor target are volatile since this
	     latter assignment might end up being done on a per-field basis.  */
	  if (DECL_INITIAL (*from_p)
	      && TREE_READONLY (*from_p)
	      && !TREE_THIS_VOLATILE (*from_p)
	      && !TREE_THIS_VOLATILE (*to_p)
	      && TREE_CODE (DECL_INITIAL (*from_p)) == CONSTRUCTOR)
	    {
	      tree old_from = *from_p;
	      enum gimplify_status subret;

	      /* Move the constructor into the RHS.  */
	      *from_p = unshare_expr (DECL_INITIAL (*from_p));

	      /* Let's see if gimplify_init_constructor will need to put
		 it in memory.  */
	      subret = gimplify_init_constructor (expr_p, NULL, NULL,
						  false, true);
	      if (subret == GS_ERROR)
		{
		  /* If so, revert the change.  */
		  *from_p = old_from;
		}
	      else
		{
		  ret = GS_OK;
		  changed = true;
		}
	    }
	  break;
	case INDIRECT_REF:
	  {
	    /* If we have code like

	     *(const A*)(A*)&x

	     where the type of "x" is a (possibly cv-qualified variant
	     of "A"), treat the entire expression as identical to "x".
	     This kind of code arises in C++ when an object is bound
	     to a const reference, and if "x" is a TARGET_EXPR we want
	     to take advantage of the optimization below.  */
	    bool volatile_p = TREE_THIS_VOLATILE (*from_p);
	    tree t = gimple_fold_indirect_ref_rhs (TREE_OPERAND (*from_p, 0));
	    if (t)
	      {
		if (TREE_THIS_VOLATILE (t) != volatile_p)
		  {
		    if (DECL_P (t))
		      t = build_simple_mem_ref_loc (EXPR_LOCATION (*from_p),
						    build_fold_addr_expr (t));
		    if (REFERENCE_CLASS_P (t))
		      TREE_THIS_VOLATILE (t) = volatile_p;
		  }
		*from_p = t;
		ret = GS_OK;
		changed = true;
	      }
	    break;
	  }

	case TARGET_EXPR:
	  {
	    /* If we are initializing something from a TARGET_EXPR, strip the
	       TARGET_EXPR and initialize it directly, if possible.  This can't
	       be done if the initializer is void, since that implies that the
	       temporary is set in some non-trivial way.

	       ??? What about code that pulls out the temp and uses it
	       elsewhere? I think that such code never uses the TARGET_EXPR as
	       an initializer.  If I'm wrong, we'll die because the temp won't
	       have any RTL.  In that case, I guess we'll need to replace
	       references somehow.  */
	    tree init = TARGET_EXPR_INITIAL (*from_p);

	    if (init
		&& (TREE_CODE (*expr_p) != MODIFY_EXPR
		    || !TARGET_EXPR_NO_ELIDE (*from_p))
		&& !VOID_TYPE_P (TREE_TYPE (init)))
	      {
		*from_p = init;
		ret = GS_OK;
		changed = true;
	      }
	  }
	  break;

	case COMPOUND_EXPR:
	  /* Remove any COMPOUND_EXPR in the RHS so the following cases will be
	     caught.  */
	  gimplify_compound_expr (from_p, pre_p, true);
	  ret = GS_OK;
	  changed = true;
	  break;

	case CONSTRUCTOR:
	  /* If we already made some changes, let the front end have a
	     crack at this before we break it down.  */
	  if (ret != GS_UNHANDLED)
	    break;
	  /* If we're initializing from a CONSTRUCTOR, break this into
	     individual MODIFY_EXPRs.  */
	  return gimplify_init_constructor (expr_p, pre_p, post_p, want_value,
					    false);

	case COND_EXPR:
	  /* If we're assigning to a non-register type, push the assignment
	     down into the branches.  This is mandatory for ADDRESSABLE types,
	     since we cannot generate temporaries for such, but it saves a
	     copy in other cases as well.  */
	  if (!is_gimple_reg_type (TREE_TYPE (*from_p)))
	    {
	      /* This code should mirror the code in gimplify_cond_expr. */
	      enum tree_code code = TREE_CODE (*expr_p);
	      tree cond = *from_p;
	      tree result = *to_p;

	      ret = gimplify_expr (&result, pre_p, post_p,
				   is_gimple_lvalue, fb_lvalue);
	      if (ret != GS_ERROR)
		ret = GS_OK;

	      /* If we are going to write RESULT more than once, clear
		 TREE_READONLY flag, otherwise we might incorrectly promote
		 the variable to static const and initialize it at compile
		 time in one of the branches.  */
	      if (VAR_P (result)
		  && TREE_TYPE (TREE_OPERAND (cond, 1)) != void_type_node
		  && TREE_TYPE (TREE_OPERAND (cond, 2)) != void_type_node)
		TREE_READONLY (result) = 0;
	      if (TREE_TYPE (TREE_OPERAND (cond, 1)) != void_type_node)
		TREE_OPERAND (cond, 1)
		  = build2 (code, void_type_node, result,
			    TREE_OPERAND (cond, 1));
	      if (TREE_TYPE (TREE_OPERAND (cond, 2)) != void_type_node)
		TREE_OPERAND (cond, 2)
		  = build2 (code, void_type_node, unshare_expr (result),
			    TREE_OPERAND (cond, 2));

	      TREE_TYPE (cond) = void_type_node;
	      recalculate_side_effects (cond);

	      if (want_value)
		{
		  gimplify_and_add (cond, pre_p);
		  *expr_p = unshare_expr (result);
		}
	      else
		*expr_p = cond;
	      return ret;
	    }
	  break;

	case CALL_EXPR:
	  /* For calls that return in memory, give *to_p as the CALL_EXPR's
	     return slot so that we don't generate a temporary.  */
	  if (!CALL_EXPR_RETURN_SLOT_OPT (*from_p)
	      && aggregate_value_p (*from_p, *from_p))
	    {
	      bool use_target;

	      if (!(rhs_predicate_for (*to_p))(*from_p))
		/* If we need a temporary, *to_p isn't accurate.  */
		use_target = false;
	      /* It's OK to use the return slot directly unless it's an NRV. */
	      else if (TREE_CODE (*to_p) == RESULT_DECL
		       && DECL_NAME (*to_p) == NULL_TREE
		       && needs_to_live_in_memory (*to_p))
		use_target = true;
	      else if (is_gimple_reg_type (TREE_TYPE (*to_p))
		       || (DECL_P (*to_p) && DECL_REGISTER (*to_p)))
		/* Don't force regs into memory.  */
		use_target = false;
	      else if (TREE_CODE (*expr_p) == INIT_EXPR)
		/* It's OK to use the target directly if it's being
		   initialized. */
		use_target = true;
	      else if (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (*to_p)))
		       != INTEGER_CST)
		/* Always use the target and thus RSO for variable-sized types.
		   GIMPLE cannot deal with a variable-sized assignment
		   embedded in a call statement.  */
		use_target = true;
	      else if (TREE_CODE (*to_p) != SSA_NAME
		      && (!is_gimple_variable (*to_p)
			  || needs_to_live_in_memory (*to_p)))
		/* Don't use the original target if it's already addressable;
		   if its address escapes, and the called function uses the
		   NRV optimization, a conforming program could see *to_p
		   change before the called function returns; see c++/19317.
		   When optimizing, the return_slot pass marks more functions
		   as safe after we have escape info.  */
		use_target = false;
	      else
		use_target = true;

	      if (use_target)
		{
		  CALL_EXPR_RETURN_SLOT_OPT (*from_p) = 1;
		  mark_addressable (*to_p);
		}
	    }
	  break;

	case WITH_SIZE_EXPR:
	  /* Likewise for calls that return an aggregate of non-constant size,
	     since we would not be able to generate a temporary at all.  */
	  if (TREE_CODE (TREE_OPERAND (*from_p, 0)) == CALL_EXPR)
	    {
	      *from_p = TREE_OPERAND (*from_p, 0);
	      /* We don't change ret in this case because the
		 WITH_SIZE_EXPR might have been added in
		 gimplify_modify_expr, so returning GS_OK would lead to an
		 infinite loop.  */
	      changed = true;
	    }
	  break;

	  /* If we're initializing from a container, push the initialization
	     inside it.  */
	case CLEANUP_POINT_EXPR:
	case BIND_EXPR:
	case STATEMENT_LIST:
	  {
	    tree wrap = *from_p;
	    tree t;

	    ret = gimplify_expr (to_p, pre_p, post_p, is_gimple_min_lval,
				 fb_lvalue);
	    if (ret != GS_ERROR)
	      ret = GS_OK;

	    t = voidify_wrapper_expr (wrap, *expr_p);
	    gcc_assert (t == *expr_p);

	    if (want_value)
	      {
		gimplify_and_add (wrap, pre_p);
		*expr_p = unshare_expr (*to_p);
	      }
	    else
	      *expr_p = wrap;
	    return GS_OK;
	  }

	case COMPOUND_LITERAL_EXPR:
	  {
	    tree complit = TREE_OPERAND (*expr_p, 1);
	    tree decl_s = COMPOUND_LITERAL_EXPR_DECL_EXPR (complit);
	    tree decl = DECL_EXPR_DECL (decl_s);
	    tree init = DECL_INITIAL (decl);

	    /* struct T x = (struct T) { 0, 1, 2 } can be optimized
	       into struct T x = { 0, 1, 2 } if the address of the
	       compound literal has never been taken.  */
	    if (!TREE_ADDRESSABLE (complit)
		&& !TREE_ADDRESSABLE (decl)
		&& init)
	      {
		*expr_p = copy_node (*expr_p);
		TREE_OPERAND (*expr_p, 1) = init;
		return GS_OK;
	      }
	  }

	default:
	  break;
	}
    }
  while (changed);

  return ret;
}


/* Return true if T looks like a valid GIMPLE statement.  */

static bool
is_gimple_stmt (tree t)
{
  const enum tree_code code = TREE_CODE (t);

  switch (code)
    {
    case NOP_EXPR:
      /* The only valid NOP_EXPR is the empty statement.  */
      return IS_EMPTY_STMT (t);

    case BIND_EXPR:
    case COND_EXPR:
      /* These are only valid if they're void.  */
      return TREE_TYPE (t) == NULL || VOID_TYPE_P (TREE_TYPE (t));

    case SWITCH_EXPR:
    case GOTO_EXPR:
    case RETURN_EXPR:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
    case EH_FILTER_EXPR:
    case CATCH_EXPR:
    case ASM_EXPR:
    case STATEMENT_LIST:
    case OACC_PARALLEL:
    case OACC_KERNELS:
    case OACC_DATA:
    case OACC_HOST_DATA:
    case OACC_DECLARE:
    case OACC_UPDATE:
    case OACC_ENTER_DATA:
    case OACC_EXIT_DATA:
    case OACC_CACHE:
    case OMP_PARALLEL:
    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OACC_LOOP:
    case OMP_SECTIONS:
    case OMP_SECTION:
    case OMP_SINGLE:
    case OMP_MASTER:
    case OMP_TASKGROUP:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_TASK:
    case OMP_TARGET:
    case OMP_TARGET_DATA:
    case OMP_TARGET_UPDATE:
    case OMP_TARGET_ENTER_DATA:
    case OMP_TARGET_EXIT_DATA:
    case OMP_TASKLOOP:
    case OMP_TEAMS:
      /* These are always void.  */
      return true;

    case CALL_EXPR:
    case MODIFY_EXPR:
    case PREDICT_EXPR:
      /* These are valid regardless of their type.  */
      return true;

    default:
      return false;
    }
}


/* Promote partial stores to COMPLEX variables to total stores.  *EXPR_P is
   a MODIFY_EXPR with a lhs of a REAL/IMAGPART_EXPR of a variable with
   DECL_GIMPLE_REG_P set.

   IMPORTANT NOTE: This promotion is performed by introducing a load of the
   other, unmodified part of the complex object just before the total store.
   As a consequence, if the object is still uninitialized, an undefined value
   will be loaded into a register, which may result in a spurious exception
   if the register is floating-point and the value happens to be a signaling
   NaN for example.  Then the fully-fledged complex operations lowering pass
   followed by a DCE pass are necessary in order to fix things up.  */

static enum gimplify_status
gimplify_modify_expr_complex_part (tree *expr_p, gimple_seq *pre_p,
                                   bool want_value)
{
  enum tree_code code, ocode;
  tree lhs, rhs, new_rhs, other, realpart, imagpart;

  lhs = TREE_OPERAND (*expr_p, 0);
  rhs = TREE_OPERAND (*expr_p, 1);
  code = TREE_CODE (lhs);
  lhs = TREE_OPERAND (lhs, 0);

  ocode = code == REALPART_EXPR ? IMAGPART_EXPR : REALPART_EXPR;
  other = build1 (ocode, TREE_TYPE (rhs), lhs);
  TREE_NO_WARNING (other) = 1;
  other = get_formal_tmp_var (other, pre_p);

  realpart = code == REALPART_EXPR ? rhs : other;
  imagpart = code == REALPART_EXPR ? other : rhs;

  if (TREE_CONSTANT (realpart) && TREE_CONSTANT (imagpart))
    new_rhs = build_complex (TREE_TYPE (lhs), realpart, imagpart);
  else
    new_rhs = build2 (COMPLEX_EXPR, TREE_TYPE (lhs), realpart, imagpart);

  gimplify_seq_add_stmt (pre_p, gimple_build_assign (lhs, new_rhs));
  *expr_p = (want_value) ? rhs : NULL_TREE;

  return GS_ALL_DONE;
}

/* Gimplify the MODIFY_EXPR node pointed to by EXPR_P.

      modify_expr
	      : varname '=' rhs
	      | '*' ID '=' rhs

    PRE_P points to the list where side effects that must happen before
	*EXPR_P should be stored.

    POST_P points to the list where side effects that must happen after
	*EXPR_P should be stored.

    WANT_VALUE is nonzero iff we want to use the value of this expression
	in another expression.  */

static enum gimplify_status
gimplify_modify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
		      bool want_value)
{
  tree *from_p = &TREE_OPERAND (*expr_p, 1);
  tree *to_p = &TREE_OPERAND (*expr_p, 0);
  enum gimplify_status ret = GS_UNHANDLED;
  gimple *assign;
  location_t loc = EXPR_LOCATION (*expr_p);
  gimple_stmt_iterator gsi;

  gcc_assert (TREE_CODE (*expr_p) == MODIFY_EXPR
	      || TREE_CODE (*expr_p) == INIT_EXPR);

  /* Trying to simplify a clobber using normal logic doesn't work,
     so handle it here.  */
  if (TREE_CLOBBER_P (*from_p))
    {
      ret = gimplify_expr (to_p, pre_p, post_p, is_gimple_lvalue, fb_lvalue);
      if (ret == GS_ERROR)
	return ret;
      gcc_assert (!want_value
		  && (VAR_P (*to_p) || TREE_CODE (*to_p) == MEM_REF));
      gimplify_seq_add_stmt (pre_p, gimple_build_assign (*to_p, *from_p));
      *expr_p = NULL;
      return GS_ALL_DONE;
    }

  /* Insert pointer conversions required by the middle-end that are not
     required by the frontend.  This fixes middle-end type checking for
     for example gcc.dg/redecl-6.c.  */
  if (POINTER_TYPE_P (TREE_TYPE (*to_p)))
    {
      STRIP_USELESS_TYPE_CONVERSION (*from_p);
      if (!useless_type_conversion_p (TREE_TYPE (*to_p), TREE_TYPE (*from_p)))
	*from_p = fold_convert_loc (loc, TREE_TYPE (*to_p), *from_p);
    }

  /* See if any simplifications can be done based on what the RHS is.  */
  ret = gimplify_modify_expr_rhs (expr_p, from_p, to_p, pre_p, post_p,
				  want_value);
  if (ret != GS_UNHANDLED)
    return ret;

  /* For zero sized types only gimplify the left hand side and right hand
     side as statements and throw away the assignment.  Do this after
     gimplify_modify_expr_rhs so we handle TARGET_EXPRs of addressable
     types properly.  */
  if (zero_sized_type (TREE_TYPE (*from_p))
      && !want_value
      /* Don't do this for calls that return addressable types, expand_call
	 relies on those having a lhs.  */
      && !(TREE_ADDRESSABLE (TREE_TYPE (*from_p))
	   && TREE_CODE (*from_p) == CALL_EXPR))
    {
      gimplify_stmt (from_p, pre_p);
      gimplify_stmt (to_p, pre_p);
      *expr_p = NULL_TREE;
      return GS_ALL_DONE;
    }

  /* If the value being copied is of variable width, compute the length
     of the copy into a WITH_SIZE_EXPR.   Note that we need to do this
     before gimplifying any of the operands so that we can resolve any
     PLACEHOLDER_EXPRs in the size.  Also note that the RTL expander uses
     the size of the expression to be copied, not of the destination, so
     that is what we must do here.  */
  maybe_with_size_expr (from_p);

  /* As a special case, we have to temporarily allow for assignments
     with a CALL_EXPR on the RHS.  Since in GIMPLE a function call is
     a toplevel statement, when gimplifying the GENERIC expression
     MODIFY_EXPR <a, CALL_EXPR <foo>>, we cannot create the tuple
     GIMPLE_ASSIGN <a, GIMPLE_CALL <foo>>.

     Instead, we need to create the tuple GIMPLE_CALL <a, foo>.  To
     prevent gimplify_expr from trying to create a new temporary for
     foo's LHS, we tell it that it should only gimplify until it
     reaches the CALL_EXPR.  On return from gimplify_expr, the newly
     created GIMPLE_CALL <foo> will be the last statement in *PRE_P
     and all we need to do here is set 'a' to be its LHS.  */

  /* Gimplify the RHS first for C++17 and bug 71104.  */
  gimple_predicate initial_pred = initial_rhs_predicate_for (*to_p);
  ret = gimplify_expr (from_p, pre_p, post_p, initial_pred, fb_rvalue);
  if (ret == GS_ERROR)
    return ret;

  /* Then gimplify the LHS.  */
  /* If we gimplified the RHS to a CALL_EXPR and that call may return
     twice we have to make sure to gimplify into non-SSA as otherwise
     the abnormal edge added later will make those defs not dominate
     their uses.
     ???  Technically this applies only to the registers used in the
     resulting non-register *TO_P.  */
  bool saved_into_ssa = gimplify_ctxp->into_ssa;
  if (saved_into_ssa
      && TREE_CODE (*from_p) == CALL_EXPR
      && call_expr_flags (*from_p) & ECF_RETURNS_TWICE)
    gimplify_ctxp->into_ssa = false;
  ret = gimplify_expr (to_p, pre_p, post_p, is_gimple_lvalue, fb_lvalue);
  gimplify_ctxp->into_ssa = saved_into_ssa;
  if (ret == GS_ERROR)
    return ret;

  /* Now that the LHS is gimplified, re-gimplify the RHS if our initial
     guess for the predicate was wrong.  */
  gimple_predicate final_pred = rhs_predicate_for (*to_p);
  if (final_pred != initial_pred)
    {
      ret = gimplify_expr (from_p, pre_p, post_p, final_pred, fb_rvalue);
      if (ret == GS_ERROR)
	return ret;
    }

  /* In case of va_arg internal fn wrappped in a WITH_SIZE_EXPR, add the type
     size as argument to the call.  */
  if (TREE_CODE (*from_p) == WITH_SIZE_EXPR)
    {
      tree call = TREE_OPERAND (*from_p, 0);
      tree vlasize = TREE_OPERAND (*from_p, 1);

      if (TREE_CODE (call) == CALL_EXPR
	  && CALL_EXPR_IFN (call) == IFN_VA_ARG)
	{
	  int nargs = call_expr_nargs (call);
	  tree type = TREE_TYPE (call);
	  tree ap = CALL_EXPR_ARG (call, 0);
	  tree tag = CALL_EXPR_ARG (call, 1);
	  tree aptag = CALL_EXPR_ARG (call, 2);
	  tree newcall = build_call_expr_internal_loc (EXPR_LOCATION (call),
						       IFN_VA_ARG, type,
						       nargs + 1, ap, tag,
						       aptag, vlasize);
	  TREE_OPERAND (*from_p, 0) = newcall;
	}
    }

  /* Now see if the above changed *from_p to something we handle specially.  */
  ret = gimplify_modify_expr_rhs (expr_p, from_p, to_p, pre_p, post_p,
				  want_value);
  if (ret != GS_UNHANDLED)
    return ret;

  /* If we've got a variable sized assignment between two lvalues (i.e. does
     not involve a call), then we can make things a bit more straightforward
     by converting the assignment to memcpy or memset.  */
  if (TREE_CODE (*from_p) == WITH_SIZE_EXPR)
    {
      tree from = TREE_OPERAND (*from_p, 0);
      tree size = TREE_OPERAND (*from_p, 1);

      if (TREE_CODE (from) == CONSTRUCTOR)
	return gimplify_modify_expr_to_memset (expr_p, size, want_value, pre_p);

      if (is_gimple_addressable (from))
	{
	  *from_p = from;
	  return gimplify_modify_expr_to_memcpy (expr_p, size, want_value,
	      					 pre_p);
	}
    }

  /* Transform partial stores to non-addressable complex variables into
     total stores.  This allows us to use real instead of virtual operands
     for these variables, which improves optimization.  */
  if ((TREE_CODE (*to_p) == REALPART_EXPR
       || TREE_CODE (*to_p) == IMAGPART_EXPR)
      && is_gimple_reg (TREE_OPERAND (*to_p, 0)))
    return gimplify_modify_expr_complex_part (expr_p, pre_p, want_value);

  /* Try to alleviate the effects of the gimplification creating artificial
     temporaries (see for example is_gimple_reg_rhs) on the debug info, but
     make sure not to create DECL_DEBUG_EXPR links across functions.  */
  if (!gimplify_ctxp->into_ssa
      && VAR_P (*from_p)
      && DECL_IGNORED_P (*from_p)
      && DECL_P (*to_p)
      && !DECL_IGNORED_P (*to_p)
      && decl_function_context (*to_p) == current_function_decl
      && decl_function_context (*from_p) == current_function_decl)
    {
      if (!DECL_NAME (*from_p) && DECL_NAME (*to_p))
	DECL_NAME (*from_p)
	  = create_tmp_var_name (IDENTIFIER_POINTER (DECL_NAME (*to_p)));
      DECL_HAS_DEBUG_EXPR_P (*from_p) = 1;
      SET_DECL_DEBUG_EXPR (*from_p, *to_p);
   }

  if (want_value && TREE_THIS_VOLATILE (*to_p))
    *from_p = get_initialized_tmp_var (*from_p, pre_p, post_p);

  if (TREE_CODE (*from_p) == CALL_EXPR)
    {
      /* Since the RHS is a CALL_EXPR, we need to create a GIMPLE_CALL
	 instead of a GIMPLE_ASSIGN.  */
      gcall *call_stmt;
      if (CALL_EXPR_FN (*from_p) == NULL_TREE)
	{
	  /* Gimplify internal functions created in the FEs.  */
	  int nargs = call_expr_nargs (*from_p), i;
	  enum internal_fn ifn = CALL_EXPR_IFN (*from_p);
	  auto_vec<tree> vargs (nargs);

	  for (i = 0; i < nargs; i++)
	    {
	      gimplify_arg (&CALL_EXPR_ARG (*from_p, i), pre_p,
			    EXPR_LOCATION (*from_p));
	      vargs.quick_push (CALL_EXPR_ARG (*from_p, i));
	    }
	  call_stmt = gimple_build_call_internal_vec (ifn, vargs);
	  gimple_call_set_nothrow (call_stmt, TREE_NOTHROW (*from_p));
	  gimple_set_location (call_stmt, EXPR_LOCATION (*expr_p));
	}
      else
	{
	  tree fnptrtype = TREE_TYPE (CALL_EXPR_FN (*from_p));
	  CALL_EXPR_FN (*from_p) = TREE_OPERAND (CALL_EXPR_FN (*from_p), 0);
	  STRIP_USELESS_TYPE_CONVERSION (CALL_EXPR_FN (*from_p));
	  tree fndecl = get_callee_fndecl (*from_p);
	  if (fndecl
	      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_EXPECT
	      && call_expr_nargs (*from_p) == 3)
	    call_stmt = gimple_build_call_internal (IFN_BUILTIN_EXPECT, 3,
						    CALL_EXPR_ARG (*from_p, 0),
						    CALL_EXPR_ARG (*from_p, 1),
						    CALL_EXPR_ARG (*from_p, 2));
	  else
	    {
	      call_stmt = gimple_build_call_from_tree (*from_p, fnptrtype);
	    }
	}
      notice_special_calls (call_stmt);
      if (!gimple_call_noreturn_p (call_stmt) || !should_remove_lhs_p (*to_p))
	gimple_call_set_lhs (call_stmt, *to_p);
      else if (TREE_CODE (*to_p) == SSA_NAME)
	/* The above is somewhat premature, avoid ICEing later for a
	   SSA name w/o a definition.  We may have uses in the GIMPLE IL.
	   ???  This doesn't make it a default-def.  */
	SSA_NAME_DEF_STMT (*to_p) = gimple_build_nop ();

      assign = call_stmt;
    }
  else
    {
      assign = gimple_build_assign (*to_p, *from_p);
      gimple_set_location (assign, EXPR_LOCATION (*expr_p));
      if (COMPARISON_CLASS_P (*from_p))
	gimple_set_no_warning (assign, TREE_NO_WARNING (*from_p));
    }

  if (gimplify_ctxp->into_ssa && is_gimple_reg (*to_p))
    {
      /* We should have got an SSA name from the start.  */
      gcc_assert (TREE_CODE (*to_p) == SSA_NAME
		  || ! gimple_in_ssa_p (cfun));
    }

  gimplify_seq_add_stmt (pre_p, assign);
  gsi = gsi_last (*pre_p);
  maybe_fold_stmt (&gsi);

  if (want_value)
    {
      *expr_p = TREE_THIS_VOLATILE (*to_p) ? *from_p : unshare_expr (*to_p);
      return GS_OK;
    }
  else
    *expr_p = NULL;

  return GS_ALL_DONE;
}

/* Gimplify a comparison between two variable-sized objects.  Do this
   with a call to BUILT_IN_MEMCMP.  */

static enum gimplify_status
gimplify_variable_sized_compare (tree *expr_p)
{
  location_t loc = EXPR_LOCATION (*expr_p);
  tree op0 = TREE_OPERAND (*expr_p, 0);
  tree op1 = TREE_OPERAND (*expr_p, 1);
  tree t, arg, dest, src, expr;

  arg = TYPE_SIZE_UNIT (TREE_TYPE (op0));
  arg = unshare_expr (arg);
  arg = SUBSTITUTE_PLACEHOLDER_IN_EXPR (arg, op0);
  src = build_fold_addr_expr_loc (loc, op1);
  dest = build_fold_addr_expr_loc (loc, op0);
  t = builtin_decl_implicit (BUILT_IN_MEMCMP);
  t = build_call_expr_loc (loc, t, 3, dest, src, arg);

  expr
    = build2 (TREE_CODE (*expr_p), TREE_TYPE (*expr_p), t, integer_zero_node);
  SET_EXPR_LOCATION (expr, loc);
  *expr_p = expr;

  return GS_OK;
}

/* Gimplify a comparison between two aggregate objects of integral scalar
   mode as a comparison between the bitwise equivalent scalar values.  */

static enum gimplify_status
gimplify_scalar_mode_aggregate_compare (tree *expr_p)
{
  location_t loc = EXPR_LOCATION (*expr_p);
  tree op0 = TREE_OPERAND (*expr_p, 0);
  tree op1 = TREE_OPERAND (*expr_p, 1);

  tree type = TREE_TYPE (op0);
  tree scalar_type = lang_hooks.types.type_for_mode (TYPE_MODE (type), 1);

  op0 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, scalar_type, op0);
  op1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, scalar_type, op1);

  *expr_p
    = fold_build2_loc (loc, TREE_CODE (*expr_p), TREE_TYPE (*expr_p), op0, op1);

  return GS_OK;
}

/* Gimplify an expression sequence.  This function gimplifies each
   expression and rewrites the original expression with the last
   expression of the sequence in GIMPLE form.

   PRE_P points to the list where the side effects for all the
       expressions in the sequence will be emitted.

   WANT_VALUE is true when the result of the last COMPOUND_EXPR is used.  */

static enum gimplify_status
gimplify_compound_expr (tree *expr_p, gimple_seq *pre_p, bool want_value)
{
  tree t = *expr_p;

  do
    {
      tree *sub_p = &TREE_OPERAND (t, 0);

      if (TREE_CODE (*sub_p) == COMPOUND_EXPR)
	gimplify_compound_expr (sub_p, pre_p, false);
      else
	gimplify_stmt (sub_p, pre_p);

      t = TREE_OPERAND (t, 1);
    }
  while (TREE_CODE (t) == COMPOUND_EXPR);

  *expr_p = t;
  if (want_value)
    return GS_OK;
  else
    {
      gimplify_stmt (expr_p, pre_p);
      return GS_ALL_DONE;
    }
}

/* Gimplify a SAVE_EXPR node.  EXPR_P points to the expression to
   gimplify.  After gimplification, EXPR_P will point to a new temporary
   that holds the original value of the SAVE_EXPR node.

   PRE_P points to the list where side effects that must happen before
   *EXPR_P should be stored.  */

static enum gimplify_status
gimplify_save_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  enum gimplify_status ret = GS_ALL_DONE;
  tree val;

  gcc_assert (TREE_CODE (*expr_p) == SAVE_EXPR);
  val = TREE_OPERAND (*expr_p, 0);

  /* If the SAVE_EXPR has not been resolved, then evaluate it once.  */
  if (!SAVE_EXPR_RESOLVED_P (*expr_p))
    {
      /* The operand may be a void-valued expression.  It is
	 being executed only for its side-effects.  */
      if (TREE_TYPE (val) == void_type_node)
	{
	  ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			       is_gimple_stmt, fb_none);
	  val = NULL;
	}
      else
	/* The temporary may not be an SSA name as later abnormal and EH
	   control flow may invalidate use/def domination.  */
	val = get_initialized_tmp_var (val, pre_p, post_p, false);

      TREE_OPERAND (*expr_p, 0) = val;
      SAVE_EXPR_RESOLVED_P (*expr_p) = 1;
    }

  *expr_p = val;

  return ret;
}

/* Rewrite the ADDR_EXPR node pointed to by EXPR_P

      unary_expr
	      : ...
	      | '&' varname
	      ...

    PRE_P points to the list where side effects that must happen before
	*EXPR_P should be stored.

    POST_P points to the list where side effects that must happen after
	*EXPR_P should be stored.  */

static enum gimplify_status
gimplify_addr_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  tree expr = *expr_p;
  tree op0 = TREE_OPERAND (expr, 0);
  enum gimplify_status ret;
  location_t loc = EXPR_LOCATION (*expr_p);

  switch (TREE_CODE (op0))
    {
    case INDIRECT_REF:
    do_indirect_ref:
      /* Check if we are dealing with an expression of the form '&*ptr'.
	 While the front end folds away '&*ptr' into 'ptr', these
	 expressions may be generated internally by the compiler (e.g.,
	 builtins like __builtin_va_end).  */
      /* Caution: the silent array decomposition semantics we allow for
	 ADDR_EXPR means we can't always discard the pair.  */
      /* Gimplification of the ADDR_EXPR operand may drop
	 cv-qualification conversions, so make sure we add them if
	 needed.  */
      {
	tree op00 = TREE_OPERAND (op0, 0);
	tree t_expr = TREE_TYPE (expr);
	tree t_op00 = TREE_TYPE (op00);

        if (!useless_type_conversion_p (t_expr, t_op00))
	  op00 = fold_convert_loc (loc, TREE_TYPE (expr), op00);
        *expr_p = op00;
        ret = GS_OK;
      }
      break;

    case VIEW_CONVERT_EXPR:
      /* Take the address of our operand and then convert it to the type of
	 this ADDR_EXPR.

	 ??? The interactions of VIEW_CONVERT_EXPR and aliasing is not at
	 all clear.  The impact of this transformation is even less clear.  */

      /* If the operand is a useless conversion, look through it.  Doing so
	 guarantees that the ADDR_EXPR and its operand will remain of the
	 same type.  */
      if (tree_ssa_useless_type_conversion (TREE_OPERAND (op0, 0)))
	op0 = TREE_OPERAND (op0, 0);

      *expr_p = fold_convert_loc (loc, TREE_TYPE (expr),
				  build_fold_addr_expr_loc (loc,
							TREE_OPERAND (op0, 0)));
      ret = GS_OK;
      break;

    case MEM_REF:
      if (integer_zerop (TREE_OPERAND (op0, 1)))
	goto do_indirect_ref;

      /* fall through */

    default:
      /* If we see a call to a declared builtin or see its address
	 being taken (we can unify those cases here) then we can mark
	 the builtin for implicit generation by GCC.  */
      if (TREE_CODE (op0) == FUNCTION_DECL
	  && DECL_BUILT_IN_CLASS (op0) == BUILT_IN_NORMAL
	  && builtin_decl_declared_p (DECL_FUNCTION_CODE (op0)))
	set_builtin_decl_implicit_p (DECL_FUNCTION_CODE (op0), true);

      /* We use fb_either here because the C frontend sometimes takes
	 the address of a call that returns a struct; see
	 gcc.dg/c99-array-lval-1.c.  The gimplifier will correctly make
	 the implied temporary explicit.  */

      /* Make the operand addressable.  */
      ret = gimplify_expr (&TREE_OPERAND (expr, 0), pre_p, post_p,
			   is_gimple_addressable, fb_either);
      if (ret == GS_ERROR)
	break;

      /* Then mark it.  Beware that it may not be possible to do so directly
	 if a temporary has been created by the gimplification.  */
      prepare_gimple_addressable (&TREE_OPERAND (expr, 0), pre_p);

      op0 = TREE_OPERAND (expr, 0);

      /* For various reasons, the gimplification of the expression
	 may have made a new INDIRECT_REF.  */
      if (TREE_CODE (op0) == INDIRECT_REF)
	goto do_indirect_ref;

      mark_addressable (TREE_OPERAND (expr, 0));

      /* The FEs may end up building ADDR_EXPRs early on a decl with
	 an incomplete type.  Re-build ADDR_EXPRs in canonical form
	 here.  */
      if (!types_compatible_p (TREE_TYPE (op0), TREE_TYPE (TREE_TYPE (expr))))
	*expr_p = build_fold_addr_expr (op0);

      /* Make sure TREE_CONSTANT and TREE_SIDE_EFFECTS are set properly.  */
      recompute_tree_invariant_for_addr_expr (*expr_p);

      /* If we re-built the ADDR_EXPR add a conversion to the original type
         if required.  */
      if (!useless_type_conversion_p (TREE_TYPE (expr), TREE_TYPE (*expr_p)))
	*expr_p = fold_convert (TREE_TYPE (expr), *expr_p);

      break;
    }

  return ret;
}

/* Gimplify the operands of an ASM_EXPR.  Input operands should be a gimple
   value; output operands should be a gimple lvalue.  */

static enum gimplify_status
gimplify_asm_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  tree expr;
  int noutputs;
  const char **oconstraints;
  int i;
  tree link;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;
  enum gimplify_status ret, tret;
  gasm *stmt;
  vec<tree, va_gc> *inputs;
  vec<tree, va_gc> *outputs;
  vec<tree, va_gc> *clobbers;
  vec<tree, va_gc> *labels;
  tree link_next;

  expr = *expr_p;
  noutputs = list_length (ASM_OUTPUTS (expr));
  oconstraints = (const char **) alloca ((noutputs) * sizeof (const char *));

  inputs = NULL;
  outputs = NULL;
  clobbers = NULL;
  labels = NULL;

  ret = GS_ALL_DONE;
  link_next = NULL_TREE;
  for (i = 0, link = ASM_OUTPUTS (expr); link; ++i, link = link_next)
    {
      bool ok;
      size_t constraint_len;

      link_next = TREE_CHAIN (link);

      oconstraints[i]
	= constraint
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      constraint_len = strlen (constraint);
      if (constraint_len == 0)
        continue;

      ok = parse_output_constraint (&constraint, i, 0, 0,
				    &allows_mem, &allows_reg, &is_inout);
      if (!ok)
	{
	  ret = GS_ERROR;
	  is_inout = false;
	}

      if (!allows_reg && allows_mem)
	mark_addressable (TREE_VALUE (link));

      tret = gimplify_expr (&TREE_VALUE (link), pre_p, post_p,
			    is_inout ? is_gimple_min_lval : is_gimple_lvalue,
			    fb_lvalue | fb_mayfail);
      if (tret == GS_ERROR)
	{
	  error ("invalid lvalue in asm output %d", i);
	  ret = tret;
	}

      /* If the constraint does not allow memory make sure we gimplify
         it to a register if it is not already but its base is.  This
	 happens for complex and vector components.  */
      if (!allows_mem)
	{
	  tree op = TREE_VALUE (link);
	  if (! is_gimple_val (op)
	      && is_gimple_reg_type (TREE_TYPE (op))
	      && is_gimple_reg (get_base_address (op)))
	    {
	      tree tem = create_tmp_reg (TREE_TYPE (op));
	      tree ass;
	      if (is_inout)
		{
		  ass = build2 (MODIFY_EXPR, TREE_TYPE (tem),
				tem, unshare_expr (op));
		  gimplify_and_add (ass, pre_p);
		}
	      ass = build2 (MODIFY_EXPR, TREE_TYPE (tem), op, tem);
	      gimplify_and_add (ass, post_p);

	      TREE_VALUE (link) = tem;
	      tret = GS_OK;
	    }
	}

      vec_safe_push (outputs, link);
      TREE_CHAIN (link) = NULL_TREE;

      if (is_inout)
	{
	  /* An input/output operand.  To give the optimizers more
	     flexibility, split it into separate input and output
 	     operands.  */
	  tree input;
	  /* Buffer big enough to format a 32-bit UINT_MAX into.  */
	  char buf[11];

	  /* Turn the in/out constraint into an output constraint.  */
	  char *p = xstrdup (constraint);
	  p[0] = '=';
	  TREE_VALUE (TREE_PURPOSE (link)) = build_string (constraint_len, p);

	  /* And add a matching input constraint.  */
	  if (allows_reg)
	    {
	      sprintf (buf, "%u", i);

	      /* If there are multiple alternatives in the constraint,
		 handle each of them individually.  Those that allow register
		 will be replaced with operand number, the others will stay
		 unchanged.  */
	      if (strchr (p, ',') != NULL)
		{
		  size_t len = 0, buflen = strlen (buf);
		  char *beg, *end, *str, *dst;

		  for (beg = p + 1;;)
		    {
		      end = strchr (beg, ',');
		      if (end == NULL)
			end = strchr (beg, '\0');
		      if ((size_t) (end - beg) < buflen)
			len += buflen + 1;
		      else
			len += end - beg + 1;
		      if (*end)
			beg = end + 1;
		      else
			break;
		    }

		  str = (char *) alloca (len);
		  for (beg = p + 1, dst = str;;)
		    {
		      const char *tem;
		      bool mem_p, reg_p, inout_p;

		      end = strchr (beg, ',');
		      if (end)
			*end = '\0';
		      beg[-1] = '=';
		      tem = beg - 1;
		      parse_output_constraint (&tem, i, 0, 0,
					       &mem_p, &reg_p, &inout_p);
		      if (dst != str)
			*dst++ = ',';
		      if (reg_p)
			{
			  memcpy (dst, buf, buflen);
			  dst += buflen;
			}
		      else
			{
			  if (end)
			    len = end - beg;
			  else
			    len = strlen (beg);
			  memcpy (dst, beg, len);
			  dst += len;
			}
		      if (end)
			beg = end + 1;
		      else
			break;
		    }
		  *dst = '\0';
		  input = build_string (dst - str, str);
		}
	      else
		input = build_string (strlen (buf), buf);
	    }
	  else
	    input = build_string (constraint_len - 1, constraint + 1);

	  free (p);

	  input = build_tree_list (build_tree_list (NULL_TREE, input),
				   unshare_expr (TREE_VALUE (link)));
	  ASM_INPUTS (expr) = chainon (ASM_INPUTS (expr), input);
	}
    }

  link_next = NULL_TREE;
  for (link = ASM_INPUTS (expr); link; ++i, link = link_next)
    {
      link_next = TREE_CHAIN (link);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0,
			      oconstraints, &allows_mem, &allows_reg);

      /* If we can't make copies, we can only accept memory.  */
      if (TREE_ADDRESSABLE (TREE_TYPE (TREE_VALUE (link))))
	{
	  if (allows_mem)
	    allows_reg = 0;
	  else
	    {
	      error ("impossible constraint in %<asm%>");
	      error ("non-memory input %d must stay in memory", i);
	      return GS_ERROR;
	    }
	}

      /* If the operand is a memory input, it should be an lvalue.  */
      if (!allows_reg && allows_mem)
	{
	  tree inputv = TREE_VALUE (link);
	  STRIP_NOPS (inputv);
	  if (TREE_CODE (inputv) == PREDECREMENT_EXPR
	      || TREE_CODE (inputv) == PREINCREMENT_EXPR
	      || TREE_CODE (inputv) == POSTDECREMENT_EXPR
	      || TREE_CODE (inputv) == POSTINCREMENT_EXPR
	      || TREE_CODE (inputv) == MODIFY_EXPR)
	    TREE_VALUE (link) = error_mark_node;
	  tret = gimplify_expr (&TREE_VALUE (link), pre_p, post_p,
				is_gimple_lvalue, fb_lvalue | fb_mayfail);
	  if (tret != GS_ERROR)
	    {
	      /* Unlike output operands, memory inputs are not guaranteed
		 to be lvalues by the FE, and while the expressions are
		 marked addressable there, if it is e.g. a statement
		 expression, temporaries in it might not end up being
		 addressable.  They might be already used in the IL and thus
		 it is too late to make them addressable now though.  */
	      tree x = TREE_VALUE (link);
	      while (handled_component_p (x))
		x = TREE_OPERAND (x, 0);
	      if (TREE_CODE (x) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (x, 0)) == ADDR_EXPR)
		x = TREE_OPERAND (TREE_OPERAND (x, 0), 0);
	      if ((VAR_P (x)
		   || TREE_CODE (x) == PARM_DECL
		   || TREE_CODE (x) == RESULT_DECL)
		  && !TREE_ADDRESSABLE (x)
		  && is_gimple_reg (x))
		{
		  warning_at (EXPR_LOC_OR_LOC (TREE_VALUE (link),
					       input_location), 0,
			      "memory input %d is not directly addressable",
			      i);
		  prepare_gimple_addressable (&TREE_VALUE (link), pre_p);
		}
	    }
	  mark_addressable (TREE_VALUE (link));
	  if (tret == GS_ERROR)
	    {
	      error_at (EXPR_LOC_OR_LOC (TREE_VALUE (link), input_location),
			"memory input %d is not directly addressable", i);
	      ret = tret;
	    }
	}
      else
	{
	  tret = gimplify_expr (&TREE_VALUE (link), pre_p, post_p,
				is_gimple_asm_val, fb_rvalue);
	  if (tret == GS_ERROR)
	    ret = tret;
	}

      TREE_CHAIN (link) = NULL_TREE;
      vec_safe_push (inputs, link);
    }

  link_next = NULL_TREE;
  for (link = ASM_CLOBBERS (expr); link; ++i, link = link_next)
    {
      link_next = TREE_CHAIN (link);
      TREE_CHAIN (link) = NULL_TREE;
      vec_safe_push (clobbers, link);
    }

  link_next = NULL_TREE;
  for (link = ASM_LABELS (expr); link; ++i, link = link_next)
    {
      link_next = TREE_CHAIN (link);
      TREE_CHAIN (link) = NULL_TREE;
      vec_safe_push (labels, link);
    }

  /* Do not add ASMs with errors to the gimple IL stream.  */
  if (ret != GS_ERROR)
    {
      stmt = gimple_build_asm_vec (TREE_STRING_POINTER (ASM_STRING (expr)),
				   inputs, outputs, clobbers, labels);

      gimple_asm_set_volatile (stmt, ASM_VOLATILE_P (expr) || noutputs == 0);
      gimple_asm_set_input (stmt, ASM_INPUT_P (expr));

      gimplify_seq_add_stmt (pre_p, stmt);
    }

  return ret;
}

/* Gimplify a CLEANUP_POINT_EXPR.  Currently this works by adding
   GIMPLE_WITH_CLEANUP_EXPRs to the prequeue as we encounter cleanups while
   gimplifying the body, and converting them to TRY_FINALLY_EXPRs when we
   return to this function.

   FIXME should we complexify the prequeue handling instead?  Or use flags
   for all the cleanups and let the optimizer tighten them up?  The current
   code seems pretty fragile; it will break on a cleanup within any
   non-conditional nesting.  But any such nesting would be broken, anyway;
   we can't write a TRY_FINALLY_EXPR that starts inside a nesting construct
   and continues out of it.  We can do that at the RTL level, though, so
   having an optimizer to tighten up try/finally regions would be a Good
   Thing.  */

static enum gimplify_status
gimplify_cleanup_point_expr (tree *expr_p, gimple_seq *pre_p)
{
  gimple_stmt_iterator iter;
  gimple_seq body_sequence = NULL;

  tree temp = voidify_wrapper_expr (*expr_p, NULL);

  /* We only care about the number of conditions between the innermost
     CLEANUP_POINT_EXPR and the cleanup.  So save and reset the count and
     any cleanups collected outside the CLEANUP_POINT_EXPR.  */
  int old_conds = gimplify_ctxp->conditions;
  gimple_seq old_cleanups = gimplify_ctxp->conditional_cleanups;
  bool old_in_cleanup_point_expr = gimplify_ctxp->in_cleanup_point_expr;
  gimplify_ctxp->conditions = 0;
  gimplify_ctxp->conditional_cleanups = NULL;
  gimplify_ctxp->in_cleanup_point_expr = true;

  gimplify_stmt (&TREE_OPERAND (*expr_p, 0), &body_sequence);

  gimplify_ctxp->conditions = old_conds;
  gimplify_ctxp->conditional_cleanups = old_cleanups;
  gimplify_ctxp->in_cleanup_point_expr = old_in_cleanup_point_expr;

  for (iter = gsi_start (body_sequence); !gsi_end_p (iter); )
    {
      gimple *wce = gsi_stmt (iter);

      if (gimple_code (wce) == GIMPLE_WITH_CLEANUP_EXPR)
	{
	  if (gsi_one_before_end_p (iter))
	    {
              /* Note that gsi_insert_seq_before and gsi_remove do not
                 scan operands, unlike some other sequence mutators.  */
	      if (!gimple_wce_cleanup_eh_only (wce))
		gsi_insert_seq_before_without_update (&iter,
						      gimple_wce_cleanup (wce),
						      GSI_SAME_STMT);
	      gsi_remove (&iter, true);
	      break;
	    }
	  else
	    {
	      gtry *gtry;
	      gimple_seq seq;
	      enum gimple_try_flags kind;

	      if (gimple_wce_cleanup_eh_only (wce))
		kind = GIMPLE_TRY_CATCH;
	      else
		kind = GIMPLE_TRY_FINALLY;
	      seq = gsi_split_seq_after (iter);

	      gtry = gimple_build_try (seq, gimple_wce_cleanup (wce), kind);
              /* Do not use gsi_replace here, as it may scan operands.
                 We want to do a simple structural modification only.  */
	      gsi_set_stmt (&iter, gtry);
	      iter = gsi_start (gtry->eval);
	    }
	}
      else
	gsi_next (&iter);
    }

  gimplify_seq_add_seq (pre_p, body_sequence);
  if (temp)
    {
      *expr_p = temp;
      return GS_OK;
    }
  else
    {
      *expr_p = NULL;
      return GS_ALL_DONE;
    }
}

/* Insert a cleanup marker for gimplify_cleanup_point_expr.  CLEANUP
   is the cleanup action required.  EH_ONLY is true if the cleanup should
   only be executed if an exception is thrown, not on normal exit.
   If FORCE_UNCOND is true perform the cleanup unconditionally;  this is
   only valid for clobbers.  */

static void
gimple_push_cleanup (tree var, tree cleanup, bool eh_only, gimple_seq *pre_p,
		     bool force_uncond = false)
{
  gimple *wce;
  gimple_seq cleanup_stmts = NULL;

  /* Errors can result in improperly nested cleanups.  Which results in
     confusion when trying to resolve the GIMPLE_WITH_CLEANUP_EXPR.  */
  if (seen_error ())
    return;

  if (gimple_conditional_context ())
    {
      /* If we're in a conditional context, this is more complex.  We only
	 want to run the cleanup if we actually ran the initialization that
	 necessitates it, but we want to run it after the end of the
	 conditional context.  So we wrap the try/finally around the
	 condition and use a flag to determine whether or not to actually
	 run the destructor.  Thus

	   test ? f(A()) : 0

	 becomes (approximately)

	   flag = 0;
	   try {
	     if (test) { A::A(temp); flag = 1; val = f(temp); }
	     else { val = 0; }
	   } finally {
	     if (flag) A::~A(temp);
	   }
	   val
      */
      if (force_uncond)
	{
	  gimplify_stmt (&cleanup, &cleanup_stmts);
	  wce = gimple_build_wce (cleanup_stmts);
	  gimplify_seq_add_stmt (&gimplify_ctxp->conditional_cleanups, wce);
	}
      else
	{
	  tree flag = create_tmp_var (boolean_type_node, "cleanup");
	  gassign *ffalse = gimple_build_assign (flag, boolean_false_node);
	  gassign *ftrue = gimple_build_assign (flag, boolean_true_node);

	  cleanup = build3 (COND_EXPR, void_type_node, flag, cleanup, NULL);
	  gimplify_stmt (&cleanup, &cleanup_stmts);
	  wce = gimple_build_wce (cleanup_stmts);

	  gimplify_seq_add_stmt (&gimplify_ctxp->conditional_cleanups, ffalse);
	  gimplify_seq_add_stmt (&gimplify_ctxp->conditional_cleanups, wce);
	  gimplify_seq_add_stmt (pre_p, ftrue);

	  /* Because of this manipulation, and the EH edges that jump
	     threading cannot redirect, the temporary (VAR) will appear
	     to be used uninitialized.  Don't warn.  */
	  TREE_NO_WARNING (var) = 1;
	}
    }
  else
    {
      gimplify_stmt (&cleanup, &cleanup_stmts);
      wce = gimple_build_wce (cleanup_stmts);
      gimple_wce_set_cleanup_eh_only (wce, eh_only);
      gimplify_seq_add_stmt (pre_p, wce);
    }
}

/* Gimplify a TARGET_EXPR which doesn't appear on the rhs of an INIT_EXPR.  */

static enum gimplify_status
gimplify_target_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  tree targ = *expr_p;
  tree temp = TARGET_EXPR_SLOT (targ);
  tree init = TARGET_EXPR_INITIAL (targ);
  enum gimplify_status ret;

  bool unpoison_empty_seq = false;
  gimple_stmt_iterator unpoison_it;

  if (init)
    {
      tree cleanup = NULL_TREE;

      /* TARGET_EXPR temps aren't part of the enclosing block, so add it
	 to the temps list.  Handle also variable length TARGET_EXPRs.  */
      if (TREE_CODE (DECL_SIZE (temp)) != INTEGER_CST)
	{
	  if (!TYPE_SIZES_GIMPLIFIED (TREE_TYPE (temp)))
	    gimplify_type_sizes (TREE_TYPE (temp), pre_p);
	  gimplify_vla_decl (temp, pre_p);
	}
      else
	{
	  /* Save location where we need to place unpoisoning.  It's possible
	     that a variable will be converted to needs_to_live_in_memory.  */
	  unpoison_it = gsi_last (*pre_p);
	  unpoison_empty_seq = gsi_end_p (unpoison_it);

	  gimple_add_tmp_var (temp);
	}

      /* If TARGET_EXPR_INITIAL is void, then the mere evaluation of the
	 expression is supposed to initialize the slot.  */
      if (VOID_TYPE_P (TREE_TYPE (init)))
	ret = gimplify_expr (&init, pre_p, post_p, is_gimple_stmt, fb_none);
      else
	{
	  tree init_expr = build2 (INIT_EXPR, void_type_node, temp, init);
	  init = init_expr;
	  ret = gimplify_expr (&init, pre_p, post_p, is_gimple_stmt, fb_none);
	  init = NULL;
	  ggc_free (init_expr);
	}
      if (ret == GS_ERROR)
	{
	  /* PR c++/28266 Make sure this is expanded only once. */
	  TARGET_EXPR_INITIAL (targ) = NULL_TREE;
	  return GS_ERROR;
	}
      if (init)
	gimplify_and_add (init, pre_p);

      /* If needed, push the cleanup for the temp.  */
      if (TARGET_EXPR_CLEANUP (targ))
	{
	  if (CLEANUP_EH_ONLY (targ))
	    gimple_push_cleanup (temp, TARGET_EXPR_CLEANUP (targ),
				 CLEANUP_EH_ONLY (targ), pre_p);
	  else
	    cleanup = TARGET_EXPR_CLEANUP (targ);
	}

      /* Add a clobber for the temporary going out of scope, like
	 gimplify_bind_expr.  */
      if (gimplify_ctxp->in_cleanup_point_expr
	  && needs_to_live_in_memory (temp))
	{
	  if (flag_stack_reuse == SR_ALL)
	    {
	      tree clobber = build_constructor (TREE_TYPE (temp),
						NULL);
	      TREE_THIS_VOLATILE (clobber) = true;
	      clobber = build2 (MODIFY_EXPR, TREE_TYPE (temp), temp, clobber);
	      gimple_push_cleanup (temp, clobber, false, pre_p, true);
	    }
	  if (asan_poisoned_variables
	      && DECL_ALIGN (temp) <= MAX_SUPPORTED_STACK_ALIGNMENT
	      && dbg_cnt (asan_use_after_scope)
	      && !gimplify_omp_ctxp)
	    {
	      tree asan_cleanup = build_asan_poison_call_expr (temp);
	      if (asan_cleanup)
		{
		  if (unpoison_empty_seq)
		    unpoison_it = gsi_start (*pre_p);

		  asan_poison_variable (temp, false, &unpoison_it,
					unpoison_empty_seq);
		  gimple_push_cleanup (temp, asan_cleanup, false, pre_p);
		}
	    }
	}
      if (cleanup)
	gimple_push_cleanup (temp, cleanup, false, pre_p);

      /* Only expand this once.  */
      TREE_OPERAND (targ, 3) = init;
      TARGET_EXPR_INITIAL (targ) = NULL_TREE;
    }
  else
    /* We should have expanded this before.  */
    gcc_assert (DECL_SEEN_IN_BIND_EXPR_P (temp));

  *expr_p = temp;
  return GS_OK;
}

/* Gimplification of expression trees.  */

/* Gimplify an expression which appears at statement context.  The
   corresponding GIMPLE statements are added to *SEQ_P.  If *SEQ_P is
   NULL, a new sequence is allocated.

   Return true if we actually added a statement to the queue.  */

bool
gimplify_stmt (tree *stmt_p, gimple_seq *seq_p)
{
  gimple_seq_node last;

  last = gimple_seq_last (*seq_p);
  gimplify_expr (stmt_p, seq_p, NULL, is_gimple_stmt, fb_none);
  return last != gimple_seq_last (*seq_p);
}

/* Add FIRSTPRIVATE entries for DECL in the OpenMP the surrounding parallels
   to CTX.  If entries already exist, force them to be some flavor of private.
   If there is no enclosing parallel, do nothing.  */

void
omp_firstprivatize_variable (struct gimplify_omp_ctx *ctx, tree decl)
{
  splay_tree_node n;

  if (decl == NULL || !DECL_P (decl) || ctx->region_type == ORT_NONE)
    return;

  do
    {
      n = splay_tree_lookup (ctx->variables, (splay_tree_key)decl);
      if (n != NULL)
	{
	  if (n->value & GOVD_SHARED)
	    n->value = GOVD_FIRSTPRIVATE | (n->value & GOVD_SEEN);
	  else if (n->value & GOVD_MAP)
	    n->value |= GOVD_MAP_TO_ONLY;
	  else
	    return;
	}
      else if ((ctx->region_type & ORT_TARGET) != 0)
	{
	  if (ctx->target_map_scalars_firstprivate)
	    omp_add_variable (ctx, decl, GOVD_FIRSTPRIVATE);
	  else
	    omp_add_variable (ctx, decl, GOVD_MAP | GOVD_MAP_TO_ONLY);
	}
      else if (ctx->region_type != ORT_WORKSHARE
	       && ctx->region_type != ORT_SIMD
	       && ctx->region_type != ORT_ACC
	       && !(ctx->region_type & ORT_TARGET_DATA))
	omp_add_variable (ctx, decl, GOVD_FIRSTPRIVATE);

      ctx = ctx->outer_context;
    }
  while (ctx);
}

/* Similarly for each of the type sizes of TYPE.  */

static void
omp_firstprivatize_type_sizes (struct gimplify_omp_ctx *ctx, tree type)
{
  if (type == NULL || type == error_mark_node)
    return;
  type = TYPE_MAIN_VARIANT (type);

  if (ctx->privatized_types->add (type))
    return;

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
      omp_firstprivatize_variable (ctx, TYPE_MIN_VALUE (type));
      omp_firstprivatize_variable (ctx, TYPE_MAX_VALUE (type));
      break;

    case ARRAY_TYPE:
      omp_firstprivatize_type_sizes (ctx, TREE_TYPE (type));
      omp_firstprivatize_type_sizes (ctx, TYPE_DOMAIN (type));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree field;
	for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	  if (TREE_CODE (field) == FIELD_DECL)
	    {
	      omp_firstprivatize_variable (ctx, DECL_FIELD_OFFSET (field));
	      omp_firstprivatize_type_sizes (ctx, TREE_TYPE (field));
	    }
      }
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      omp_firstprivatize_type_sizes (ctx, TREE_TYPE (type));
      break;

    default:
      break;
    }

  omp_firstprivatize_variable (ctx, TYPE_SIZE (type));
  omp_firstprivatize_variable (ctx, TYPE_SIZE_UNIT (type));
  lang_hooks.types.omp_firstprivatize_type_sizes (ctx, type);
}

/* Add an entry for DECL in the OMP context CTX with FLAGS.  */

static void
omp_add_variable (struct gimplify_omp_ctx *ctx, tree decl, unsigned int flags)
{
  splay_tree_node n;
  unsigned int nflags;
  tree t;

  if (error_operand_p (decl) || ctx->region_type == ORT_NONE)
    return;

  /* Never elide decls whose type has TREE_ADDRESSABLE set.  This means
     there are constructors involved somewhere.  Exception is a shared clause,
     there is nothing privatized in that case.  */
  if ((flags & GOVD_SHARED) == 0
      && (TREE_ADDRESSABLE (TREE_TYPE (decl))
	  || TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl))))
    flags |= GOVD_SEEN;

  n = splay_tree_lookup (ctx->variables, (splay_tree_key)decl);
  if (n != NULL && (n->value & GOVD_DATA_SHARE_CLASS) != 0)
    {
      /* We shouldn't be re-adding the decl with the same data
	 sharing class.  */
      gcc_assert ((n->value & GOVD_DATA_SHARE_CLASS & flags) == 0);
      nflags = n->value | flags;
      /* The only combination of data sharing classes we should see is
	 FIRSTPRIVATE and LASTPRIVATE.  However, OpenACC permits
	 reduction variables to be used in data sharing clauses.  */
      gcc_assert ((ctx->region_type & ORT_ACC) != 0
		  || ((nflags & GOVD_DATA_SHARE_CLASS)
		      == (GOVD_FIRSTPRIVATE | GOVD_LASTPRIVATE))
		  || (flags & GOVD_DATA_SHARE_CLASS) == 0);
      n->value = nflags;
      return;
    }

  /* When adding a variable-sized variable, we have to handle all sorts
     of additional bits of data: the pointer replacement variable, and
     the parameters of the type.  */
  if (DECL_SIZE (decl) && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
    {
      /* Add the pointer replacement variable as PRIVATE if the variable
	 replacement is private, else FIRSTPRIVATE since we'll need the
	 address of the original variable either for SHARED, or for the
	 copy into or out of the context.  */
      if (!(flags & GOVD_LOCAL))
	{
	  if (flags & GOVD_MAP)
	    nflags = GOVD_MAP | GOVD_MAP_TO_ONLY | GOVD_EXPLICIT;
	  else if (flags & GOVD_PRIVATE)
	    nflags = GOVD_PRIVATE;
	  else if ((ctx->region_type & (ORT_TARGET | ORT_TARGET_DATA)) != 0
		   && (flags & GOVD_FIRSTPRIVATE))
	    nflags = GOVD_PRIVATE | GOVD_EXPLICIT;
	  else
	    nflags = GOVD_FIRSTPRIVATE;
	  nflags |= flags & GOVD_SEEN;
	  t = DECL_VALUE_EXPR (decl);
	  gcc_assert (TREE_CODE (t) == INDIRECT_REF);
	  t = TREE_OPERAND (t, 0);
	  gcc_assert (DECL_P (t));
	  omp_add_variable (ctx, t, nflags);
	}

      /* Add all of the variable and type parameters (which should have
	 been gimplified to a formal temporary) as FIRSTPRIVATE.  */
      omp_firstprivatize_variable (ctx, DECL_SIZE_UNIT (decl));
      omp_firstprivatize_variable (ctx, DECL_SIZE (decl));
      omp_firstprivatize_type_sizes (ctx, TREE_TYPE (decl));

      /* The variable-sized variable itself is never SHARED, only some form
	 of PRIVATE.  The sharing would take place via the pointer variable
	 which we remapped above.  */
      if (flags & GOVD_SHARED)
	flags = GOVD_SHARED | GOVD_DEBUG_PRIVATE
		| (flags & (GOVD_SEEN | GOVD_EXPLICIT));

      /* We're going to make use of the TYPE_SIZE_UNIT at least in the
	 alloca statement we generate for the variable, so make sure it
	 is available.  This isn't automatically needed for the SHARED
	 case, since we won't be allocating local storage then.
	 For local variables TYPE_SIZE_UNIT might not be gimplified yet,
	 in this case omp_notice_variable will be called later
	 on when it is gimplified.  */
      else if (! (flags & (GOVD_LOCAL | GOVD_MAP))
	       && DECL_P (TYPE_SIZE_UNIT (TREE_TYPE (decl))))
	omp_notice_variable (ctx, TYPE_SIZE_UNIT (TREE_TYPE (decl)), true);
    }
  else if ((flags & (GOVD_MAP | GOVD_LOCAL)) == 0
	   && lang_hooks.decls.omp_privatize_by_reference (decl))
    {
      omp_firstprivatize_type_sizes (ctx, TREE_TYPE (decl));

      /* Similar to the direct variable sized case above, we'll need the
	 size of references being privatized.  */
      if ((flags & GOVD_SHARED) == 0)
	{
	  t = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)));
	  if (DECL_P (t))
	    omp_notice_variable (ctx, t, true);
	}
    }

  if (n != NULL)
    n->value |= flags;
  else
    splay_tree_insert (ctx->variables, (splay_tree_key)decl, flags);

  /* For reductions clauses in OpenACC loop directives, by default create a
     copy clause on the enclosing parallel construct for carrying back the
     results.  */
  if (ctx->region_type == ORT_ACC && (flags & GOVD_REDUCTION))
    {
      struct gimplify_omp_ctx *outer_ctx = ctx->outer_context;
      while (outer_ctx)
	{
	  n = splay_tree_lookup (outer_ctx->variables, (splay_tree_key)decl);
	  if (n != NULL)
	    {
	      /* Ignore local variables and explicitly declared clauses.  */
	      if (n->value & (GOVD_LOCAL | GOVD_EXPLICIT))
		break;
	      else if (outer_ctx->region_type == ORT_ACC_KERNELS)
		{
		  /* According to the OpenACC spec, such a reduction variable
		     should already have a copy map on a kernels construct,
		     verify that here.  */
		  gcc_assert (!(n->value & GOVD_FIRSTPRIVATE)
			      && (n->value & GOVD_MAP));
		}
	      else if (outer_ctx->region_type == ORT_ACC_PARALLEL)
		{
		  /* Remove firstprivate and make it a copy map.  */
		  n->value &= ~GOVD_FIRSTPRIVATE;
		  n->value |= GOVD_MAP;
		}
	    }
	  else if (outer_ctx->region_type == ORT_ACC_PARALLEL)
	    {
	      splay_tree_insert (outer_ctx->variables, (splay_tree_key)decl,
				 GOVD_MAP | GOVD_SEEN);
	      break;
	    }
	  outer_ctx = outer_ctx->outer_context;
	}
    }
}

/* Notice a threadprivate variable DECL used in OMP context CTX.
   This just prints out diagnostics about threadprivate variable uses
   in untied tasks.  If DECL2 is non-NULL, prevent this warning
   on that variable.  */

static bool
omp_notice_threadprivate_variable (struct gimplify_omp_ctx *ctx, tree decl,
				   tree decl2)
{
  splay_tree_node n;
  struct gimplify_omp_ctx *octx;

  for (octx = ctx; octx; octx = octx->outer_context)
    if ((octx->region_type & ORT_TARGET) != 0)
      {
	n = splay_tree_lookup (octx->variables, (splay_tree_key)decl);
	if (n == NULL)
	  {
	    error ("threadprivate variable %qE used in target region",
		   DECL_NAME (decl));
	    error_at (octx->location, "enclosing target region");
	    splay_tree_insert (octx->variables, (splay_tree_key)decl, 0);
	  }
	if (decl2)
	  splay_tree_insert (octx->variables, (splay_tree_key)decl2, 0);
      }

  if (ctx->region_type != ORT_UNTIED_TASK)
    return false;
  n = splay_tree_lookup (ctx->variables, (splay_tree_key)decl);
  if (n == NULL)
    {
      error ("threadprivate variable %qE used in untied task",
	     DECL_NAME (decl));
      error_at (ctx->location, "enclosing task");
      splay_tree_insert (ctx->variables, (splay_tree_key)decl, 0);
    }
  if (decl2)
    splay_tree_insert (ctx->variables, (splay_tree_key)decl2, 0);
  return false;
}

/* Return true if global var DECL is device resident.  */

static bool
device_resident_p (tree decl)
{
  tree attr = lookup_attribute ("oacc declare target", DECL_ATTRIBUTES (decl));

  if (!attr)
    return false;

  for (tree t = TREE_VALUE (attr); t; t = TREE_PURPOSE (t))
    {
      tree c = TREE_VALUE (t);
      if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DEVICE_RESIDENT)
	return true;
    }

  return false;
}

/* Return true if DECL has an ACC DECLARE attribute.  */

static bool
is_oacc_declared (tree decl)
{
  tree t = TREE_CODE (decl) == MEM_REF ? TREE_OPERAND (decl, 0) : decl;
  tree declared = lookup_attribute ("oacc declare target", DECL_ATTRIBUTES (t));
  return declared != NULL_TREE;
}

/* Determine outer default flags for DECL mentioned in an OMP region
   but not declared in an enclosing clause.

   ??? Some compiler-generated variables (like SAVE_EXPRs) could be
   remapped firstprivate instead of shared.  To some extent this is
   addressed in omp_firstprivatize_type_sizes, but not
   effectively.  */

static unsigned
omp_default_clause (struct gimplify_omp_ctx *ctx, tree decl,
		    bool in_code, unsigned flags)
{
  enum omp_clause_default_kind default_kind = ctx->default_kind;
  enum omp_clause_default_kind kind;

  kind = lang_hooks.decls.omp_predetermined_sharing (decl);
  if (kind != OMP_CLAUSE_DEFAULT_UNSPECIFIED)
    default_kind = kind;

  switch (default_kind)
    {
    case OMP_CLAUSE_DEFAULT_NONE:
      {
	const char *rtype;

	if (ctx->region_type & ORT_PARALLEL)
	  rtype = "parallel";
	else if (ctx->region_type & ORT_TASK)
	  rtype = "task";
	else if (ctx->region_type & ORT_TEAMS)
	  rtype = "teams";
	else
	  gcc_unreachable ();
	
	error ("%qE not specified in enclosing %qs",
	       DECL_NAME (lang_hooks.decls.omp_report_decl (decl)), rtype);
	error_at (ctx->location, "enclosing %qs", rtype);
      }
      /* FALLTHRU */
    case OMP_CLAUSE_DEFAULT_SHARED:
      flags |= GOVD_SHARED;
      break;
    case OMP_CLAUSE_DEFAULT_PRIVATE:
      flags |= GOVD_PRIVATE;
      break;
    case OMP_CLAUSE_DEFAULT_FIRSTPRIVATE:
      flags |= GOVD_FIRSTPRIVATE;
      break;
    case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
      /* decl will be either GOVD_FIRSTPRIVATE or GOVD_SHARED.  */
      gcc_assert ((ctx->region_type & ORT_TASK) != 0);
      if (struct gimplify_omp_ctx *octx = ctx->outer_context)
	{
	  omp_notice_variable (octx, decl, in_code);
	  for (; octx; octx = octx->outer_context)
	    {
	      splay_tree_node n2;

	      n2 = splay_tree_lookup (octx->variables, (splay_tree_key) decl);
	      if ((octx->region_type & (ORT_TARGET_DATA | ORT_TARGET)) != 0
		  && (n2 == NULL || (n2->value & GOVD_DATA_SHARE_CLASS) == 0))
		continue;
	      if (n2 && (n2->value & GOVD_DATA_SHARE_CLASS) != GOVD_SHARED)
		{
		  flags |= GOVD_FIRSTPRIVATE;
		  goto found_outer;
		}
	      if ((octx->region_type & (ORT_PARALLEL | ORT_TEAMS)) != 0)
		{
		  flags |= GOVD_SHARED;
		  goto found_outer;
		}
	    }
	}
      
      if (TREE_CODE (decl) == PARM_DECL
	  || (!is_global_var (decl)
	      && DECL_CONTEXT (decl) == current_function_decl))
	flags |= GOVD_FIRSTPRIVATE;
      else
	flags |= GOVD_SHARED;
    found_outer:
      break;

    default:
      gcc_unreachable ();
    }

  return flags;
}


/* Determine outer default flags for DECL mentioned in an OACC region
   but not declared in an enclosing clause.  */

static unsigned
oacc_default_clause (struct gimplify_omp_ctx *ctx, tree decl, unsigned flags)
{
  const char *rkind;
  bool on_device = false;
  bool declared = is_oacc_declared (decl);
  tree type = TREE_TYPE (decl);

  if (lang_hooks.decls.omp_privatize_by_reference (decl))
    type = TREE_TYPE (type);

  if ((ctx->region_type & (ORT_ACC_PARALLEL | ORT_ACC_KERNELS)) != 0
      && is_global_var (decl)
      && device_resident_p (decl))
    {
      on_device = true;
      flags |= GOVD_MAP_TO_ONLY;
    }

  switch (ctx->region_type)
    {
    case ORT_ACC_KERNELS:
      rkind = "kernels";

      if (AGGREGATE_TYPE_P (type))
	{
	  /* Aggregates default to 'present_or_copy', or 'present'.  */
	  if (ctx->default_kind != OMP_CLAUSE_DEFAULT_PRESENT)
	    flags |= GOVD_MAP;
	  else
	    flags |= GOVD_MAP | GOVD_MAP_FORCE_PRESENT;
	}
      else
	/* Scalars default to 'copy'.  */
	flags |= GOVD_MAP | GOVD_MAP_FORCE;

      break;

    case ORT_ACC_PARALLEL:
      rkind = "parallel";

      if (on_device || declared)
	flags |= GOVD_MAP;
      else if (AGGREGATE_TYPE_P (type))
	{
	  /* Aggregates default to 'present_or_copy', or 'present'.  */
	  if (ctx->default_kind != OMP_CLAUSE_DEFAULT_PRESENT)
	    flags |= GOVD_MAP;
	  else
	    flags |= GOVD_MAP | GOVD_MAP_FORCE_PRESENT;
	}
      else
	/* Scalars default to 'firstprivate'.  */
	flags |= GOVD_FIRSTPRIVATE;

      break;

    default:
      gcc_unreachable ();
    }

  if (DECL_ARTIFICIAL (decl))
    ; /* We can get compiler-generated decls, and should not complain
	 about them.  */
  else if (ctx->default_kind == OMP_CLAUSE_DEFAULT_NONE)
    {
      error ("%qE not specified in enclosing OpenACC %qs construct",
	     DECL_NAME (lang_hooks.decls.omp_report_decl (decl)), rkind);
      inform (ctx->location, "enclosing OpenACC %qs construct", rkind);
    }
  else if (ctx->default_kind == OMP_CLAUSE_DEFAULT_PRESENT)
    ; /* Handled above.  */
  else
    gcc_checking_assert (ctx->default_kind == OMP_CLAUSE_DEFAULT_SHARED);

  return flags;
}

/* Record the fact that DECL was used within the OMP context CTX.
   IN_CODE is true when real code uses DECL, and false when we should
   merely emit default(none) errors.  Return true if DECL is going to
   be remapped and thus DECL shouldn't be gimplified into its
   DECL_VALUE_EXPR (if any).  */

static bool
omp_notice_variable (struct gimplify_omp_ctx *ctx, tree decl, bool in_code)
{
  splay_tree_node n;
  unsigned flags = in_code ? GOVD_SEEN : 0;
  bool ret = false, shared;

  if (error_operand_p (decl))
    return false;

  if (ctx->region_type == ORT_NONE)
    return lang_hooks.decls.omp_disregard_value_expr (decl, false);

  if (is_global_var (decl))
    {
      /* Threadprivate variables are predetermined.  */
      if (DECL_THREAD_LOCAL_P (decl))
	return omp_notice_threadprivate_variable (ctx, decl, NULL_TREE);

      if (DECL_HAS_VALUE_EXPR_P (decl))
	{
	  tree value = get_base_address (DECL_VALUE_EXPR (decl));

	  if (value && DECL_P (value) && DECL_THREAD_LOCAL_P (value))
	    return omp_notice_threadprivate_variable (ctx, decl, value);
	}

      if (gimplify_omp_ctxp->outer_context == NULL
	  && VAR_P (decl)
	  && oacc_get_fn_attrib (current_function_decl))
	{
	  location_t loc = DECL_SOURCE_LOCATION (decl);

	  if (lookup_attribute ("omp declare target link",
				DECL_ATTRIBUTES (decl)))
	    {
	      error_at (loc,
			"%qE with %<link%> clause used in %<routine%> function",
			DECL_NAME (decl));
	      return false;
	    }
	  else if (!lookup_attribute ("omp declare target",
				      DECL_ATTRIBUTES (decl)))
	    {
	      error_at (loc,
			"%qE requires a %<declare%> directive for use "
			"in a %<routine%> function", DECL_NAME (decl));
	      return false;
	    }
	}
    }

  n = splay_tree_lookup (ctx->variables, (splay_tree_key)decl);
  if ((ctx->region_type & ORT_TARGET) != 0)
    {
      ret = lang_hooks.decls.omp_disregard_value_expr (decl, true);
      if (n == NULL)
	{
	  unsigned nflags = flags;
	  if (ctx->target_map_pointers_as_0len_arrays
	      || ctx->target_map_scalars_firstprivate)
	    {
	      bool is_declare_target = false;
	      bool is_scalar = false;
	      if (is_global_var (decl)
		  && varpool_node::get_create (decl)->offloadable)
		{
		  struct gimplify_omp_ctx *octx;
		  for (octx = ctx->outer_context;
		       octx; octx = octx->outer_context)
		    {
		      n = splay_tree_lookup (octx->variables,
					     (splay_tree_key)decl);
		      if (n
			  && (n->value & GOVD_DATA_SHARE_CLASS) != GOVD_SHARED
			  && (n->value & GOVD_DATA_SHARE_CLASS) != 0)
			break;
		    }
		  is_declare_target = octx == NULL;
		}
	      if (!is_declare_target && ctx->target_map_scalars_firstprivate)
		is_scalar = lang_hooks.decls.omp_scalar_p (decl);
	      if (is_declare_target)
		;
	      else if (ctx->target_map_pointers_as_0len_arrays
		       && (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
			   || (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE
			       && TREE_CODE (TREE_TYPE (TREE_TYPE (decl)))
				  == POINTER_TYPE)))
		nflags |= GOVD_MAP | GOVD_MAP_0LEN_ARRAY;
	      else if (is_scalar)
		nflags |= GOVD_FIRSTPRIVATE;
	    }

	  struct gimplify_omp_ctx *octx = ctx->outer_context;
	  if ((ctx->region_type & ORT_ACC) && octx)
	    {
	      /* Look in outer OpenACC contexts, to see if there's a
		 data attribute for this variable.  */
	      omp_notice_variable (octx, decl, in_code);

	      for (; octx; octx = octx->outer_context)
		{
		  if (!(octx->region_type & (ORT_TARGET_DATA | ORT_TARGET)))
		    break;
		  splay_tree_node n2
		    = splay_tree_lookup (octx->variables,
					 (splay_tree_key) decl);
		  if (n2)
		    {
		      if (octx->region_type == ORT_ACC_HOST_DATA)
		        error ("variable %qE declared in enclosing "
			       "%<host_data%> region", DECL_NAME (decl));
		      nflags |= GOVD_MAP;
		      if (octx->region_type == ORT_ACC_DATA
			  && (n2->value & GOVD_MAP_0LEN_ARRAY))
			nflags |= GOVD_MAP_0LEN_ARRAY;
		      goto found_outer;
		    }
		}
	    }

	  {
	    tree type = TREE_TYPE (decl);

	    if (nflags == flags
		&& gimplify_omp_ctxp->target_firstprivatize_array_bases
		&& lang_hooks.decls.omp_privatize_by_reference (decl))
	      type = TREE_TYPE (type);
	    if (nflags == flags
		&& !lang_hooks.types.omp_mappable_type (type))
	      {
		error ("%qD referenced in target region does not have "
		       "a mappable type", decl);
		nflags |= GOVD_MAP | GOVD_EXPLICIT;
	      }
	    else if (nflags == flags)
	      {
		if ((ctx->region_type & ORT_ACC) != 0)
		  nflags = oacc_default_clause (ctx, decl, flags);
		else
		  nflags |= GOVD_MAP;
	      }
	  }
	found_outer:
	  omp_add_variable (ctx, decl, nflags);
	}
      else
	{
	  /* If nothing changed, there's nothing left to do.  */
	  if ((n->value & flags) == flags)
	    return ret;
	  flags |= n->value;
	  n->value = flags;
	}
      goto do_outer;
    }

  if (n == NULL)
    {
      if (ctx->region_type == ORT_WORKSHARE
	  || ctx->region_type == ORT_SIMD
	  || ctx->region_type == ORT_ACC
	  || (ctx->region_type & ORT_TARGET_DATA) != 0)
	goto do_outer;

      flags = omp_default_clause (ctx, decl, in_code, flags);

      if ((flags & GOVD_PRIVATE)
	  && lang_hooks.decls.omp_private_outer_ref (decl))
	flags |= GOVD_PRIVATE_OUTER_REF;

      omp_add_variable (ctx, decl, flags);

      shared = (flags & GOVD_SHARED) != 0;
      ret = lang_hooks.decls.omp_disregard_value_expr (decl, shared);
      goto do_outer;
    }

  if ((n->value & (GOVD_SEEN | GOVD_LOCAL)) == 0
      && (flags & (GOVD_SEEN | GOVD_LOCAL)) == GOVD_SEEN
      && DECL_SIZE (decl))
    {
      if (TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
	{
	  splay_tree_node n2;
	  tree t = DECL_VALUE_EXPR (decl);
	  gcc_assert (TREE_CODE (t) == INDIRECT_REF);
	  t = TREE_OPERAND (t, 0);
	  gcc_assert (DECL_P (t));
	  n2 = splay_tree_lookup (ctx->variables, (splay_tree_key) t);
	  n2->value |= GOVD_SEEN;
	}
      else if (lang_hooks.decls.omp_privatize_by_reference (decl)
	       && TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)))
	       && (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl))))
		   != INTEGER_CST))
	{
	  splay_tree_node n2;
	  tree t = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)));
	  gcc_assert (DECL_P (t));
	  n2 = splay_tree_lookup (ctx->variables, (splay_tree_key) t);
	  if (n2)
	    omp_notice_variable (ctx, t, true);
	}
    }

  shared = ((flags | n->value) & GOVD_SHARED) != 0;
  ret = lang_hooks.decls.omp_disregard_value_expr (decl, shared);

  /* If nothing changed, there's nothing left to do.  */
  if ((n->value & flags) == flags)
    return ret;
  flags |= n->value;
  n->value = flags;

 do_outer:
  /* If the variable is private in the current context, then we don't
     need to propagate anything to an outer context.  */
  if ((flags & GOVD_PRIVATE) && !(flags & GOVD_PRIVATE_OUTER_REF))
    return ret;
  if ((flags & (GOVD_LINEAR | GOVD_LINEAR_LASTPRIVATE_NO_OUTER))
      == (GOVD_LINEAR | GOVD_LINEAR_LASTPRIVATE_NO_OUTER))
    return ret;
  if ((flags & (GOVD_FIRSTPRIVATE | GOVD_LASTPRIVATE
		| GOVD_LINEAR_LASTPRIVATE_NO_OUTER))
      == (GOVD_LASTPRIVATE | GOVD_LINEAR_LASTPRIVATE_NO_OUTER))
    return ret;
  if (ctx->outer_context
      && omp_notice_variable (ctx->outer_context, decl, in_code))
    return true;
  return ret;
}

/* Verify that DECL is private within CTX.  If there's specific information
   to the contrary in the innermost scope, generate an error.  */

static bool
omp_is_private (struct gimplify_omp_ctx *ctx, tree decl, int simd)
{
  splay_tree_node n;

  n = splay_tree_lookup (ctx->variables, (splay_tree_key)decl);
  if (n != NULL)
    {
      if (n->value & GOVD_SHARED)
	{
	  if (ctx == gimplify_omp_ctxp)
	    {
	      if (simd)
		error ("iteration variable %qE is predetermined linear",
		       DECL_NAME (decl));
	      else
		error ("iteration variable %qE should be private",
		       DECL_NAME (decl));
	      n->value = GOVD_PRIVATE;
	      return true;
	    }
	  else
	    return false;
	}
      else if ((n->value & GOVD_EXPLICIT) != 0
	       && (ctx == gimplify_omp_ctxp
		   || (ctx->region_type == ORT_COMBINED_PARALLEL
		       && gimplify_omp_ctxp->outer_context == ctx)))
	{
	  if ((n->value & GOVD_FIRSTPRIVATE) != 0)
	    error ("iteration variable %qE should not be firstprivate",
		   DECL_NAME (decl));
	  else if ((n->value & GOVD_REDUCTION) != 0)
	    error ("iteration variable %qE should not be reduction",
		   DECL_NAME (decl));
	  else if (simd == 0 && (n->value & GOVD_LINEAR) != 0)
	    error ("iteration variable %qE should not be linear",
		   DECL_NAME (decl));
	  else if (simd == 1 && (n->value & GOVD_LASTPRIVATE) != 0)
	    error ("iteration variable %qE should not be lastprivate",
		   DECL_NAME (decl));
	  else if (simd && (n->value & GOVD_PRIVATE) != 0)
	    error ("iteration variable %qE should not be private",
		   DECL_NAME (decl));
	  else if (simd == 2 && (n->value & GOVD_LINEAR) != 0)
	    error ("iteration variable %qE is predetermined linear",
		   DECL_NAME (decl));
	}
      return (ctx == gimplify_omp_ctxp
	      || (ctx->region_type == ORT_COMBINED_PARALLEL
		  && gimplify_omp_ctxp->outer_context == ctx));
    }

  if (ctx->region_type != ORT_WORKSHARE
      && ctx->region_type != ORT_SIMD
      && ctx->region_type != ORT_ACC)
    return false;
  else if (ctx->outer_context)
    return omp_is_private (ctx->outer_context, decl, simd);
  return false;
}

/* Return true if DECL is private within a parallel region
   that binds to the current construct's context or in parallel
   region's REDUCTION clause.  */

static bool
omp_check_private (struct gimplify_omp_ctx *ctx, tree decl, bool copyprivate)
{
  splay_tree_node n;

  do
    {
      ctx = ctx->outer_context;
      if (ctx == NULL)
	{
	  if (is_global_var (decl))
	    return false;

	  /* References might be private, but might be shared too,
	     when checking for copyprivate, assume they might be
	     private, otherwise assume they might be shared.  */
	  if (copyprivate)
	    return true;

	  if (lang_hooks.decls.omp_privatize_by_reference (decl))
	    return false;

	  /* Treat C++ privatized non-static data members outside
	     of the privatization the same.  */
	  if (omp_member_access_dummy_var (decl))
	    return false;

	  return true;
	}

      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);

      if ((ctx->region_type & (ORT_TARGET | ORT_TARGET_DATA)) != 0
	  && (n == NULL || (n->value & GOVD_DATA_SHARE_CLASS) == 0))
	continue;

      if (n != NULL)
	{
	  if ((n->value & GOVD_LOCAL) != 0
	      && omp_member_access_dummy_var (decl))
	    return false;
	  return (n->value & GOVD_SHARED) == 0;
	}
    }
  while (ctx->region_type == ORT_WORKSHARE
	 || ctx->region_type == ORT_SIMD
	 || ctx->region_type == ORT_ACC);
  return false;
}

/* Callback for walk_tree to find a DECL_EXPR for the given DECL.  */

static tree
find_decl_expr (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;

  /* If this node has been visited, unmark it and keep looking.  */
  if (TREE_CODE (t) == DECL_EXPR && DECL_EXPR_DECL (t) == (tree) data)
    return t;

  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Scan the OMP clauses in *LIST_P, installing mappings into a new
   and previous omp contexts.  */

static void
gimplify_scan_omp_clauses (tree *list_p, gimple_seq *pre_p,
			   enum omp_region_type region_type,
			   enum tree_code code)
{
  struct gimplify_omp_ctx *ctx, *outer_ctx;
  tree c;
  hash_map<tree, tree> *struct_map_to_clause = NULL;
  tree *prev_list_p = NULL;

  ctx = new_omp_context (region_type);
  outer_ctx = ctx->outer_context;
  if (code == OMP_TARGET)
    {
      if (!lang_GNU_Fortran ())
	ctx->target_map_pointers_as_0len_arrays = true;
      ctx->target_map_scalars_firstprivate = true;
    }
  if (!lang_GNU_Fortran ())
    switch (code)
      {
      case OMP_TARGET:
      case OMP_TARGET_DATA:
      case OMP_TARGET_ENTER_DATA:
      case OMP_TARGET_EXIT_DATA:
      case OACC_DECLARE:
      case OACC_HOST_DATA:
	ctx->target_firstprivatize_array_bases = true;
      default:
	break;
      }

  while ((c = *list_p) != NULL)
    {
      bool remove = false;
      bool notice_outer = true;
      const char *check_non_private = NULL;
      unsigned int flags;
      tree decl;

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_PRIVATE:
	  flags = GOVD_PRIVATE | GOVD_EXPLICIT;
	  if (lang_hooks.decls.omp_private_outer_ref (OMP_CLAUSE_DECL (c)))
	    {
	      flags |= GOVD_PRIVATE_OUTER_REF;
	      OMP_CLAUSE_PRIVATE_OUTER_REF (c) = 1;
	    }
	  else
	    notice_outer = false;
	  goto do_add;
	case OMP_CLAUSE_SHARED:
	  flags = GOVD_SHARED | GOVD_EXPLICIT;
	  goto do_add;
	case OMP_CLAUSE_FIRSTPRIVATE:
	  flags = GOVD_FIRSTPRIVATE | GOVD_EXPLICIT;
	  check_non_private = "firstprivate";
	  goto do_add;
	case OMP_CLAUSE_LASTPRIVATE:
	  flags = GOVD_LASTPRIVATE | GOVD_SEEN | GOVD_EXPLICIT;
	  check_non_private = "lastprivate";
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    goto do_add;
	  else if (outer_ctx
		   && (outer_ctx->region_type == ORT_COMBINED_PARALLEL
		       || outer_ctx->region_type == ORT_COMBINED_TEAMS)
		   && splay_tree_lookup (outer_ctx->variables,
					 (splay_tree_key) decl) == NULL)
	    {
	      omp_add_variable (outer_ctx, decl, GOVD_SHARED | GOVD_SEEN);
	      if (outer_ctx->outer_context)
		omp_notice_variable (outer_ctx->outer_context, decl, true);
	    }
	  else if (outer_ctx
		   && (outer_ctx->region_type & ORT_TASK) != 0
		   && outer_ctx->combined_loop
		   && splay_tree_lookup (outer_ctx->variables,
					 (splay_tree_key) decl) == NULL)
	    {
	      omp_add_variable (outer_ctx, decl, GOVD_LASTPRIVATE | GOVD_SEEN);
	      if (outer_ctx->outer_context)
		omp_notice_variable (outer_ctx->outer_context, decl, true);
	    }
	  else if (outer_ctx
		   && (outer_ctx->region_type == ORT_WORKSHARE
		       || outer_ctx->region_type == ORT_ACC)
		   && outer_ctx->combined_loop
		   && splay_tree_lookup (outer_ctx->variables,
					 (splay_tree_key) decl) == NULL
		   && !omp_check_private (outer_ctx, decl, false))
	    {
	      omp_add_variable (outer_ctx, decl, GOVD_LASTPRIVATE | GOVD_SEEN);
	      if (outer_ctx->outer_context
		  && (outer_ctx->outer_context->region_type
		      == ORT_COMBINED_PARALLEL)
		  && splay_tree_lookup (outer_ctx->outer_context->variables,
					(splay_tree_key) decl) == NULL)
		{
		  struct gimplify_omp_ctx *octx = outer_ctx->outer_context;
		  omp_add_variable (octx, decl, GOVD_SHARED | GOVD_SEEN);
		  if (octx->outer_context)
		    {
		      octx = octx->outer_context;
		      if (octx->region_type == ORT_WORKSHARE
			  && octx->combined_loop
			  && splay_tree_lookup (octx->variables,
						(splay_tree_key) decl) == NULL
			  && !omp_check_private (octx, decl, false))
			{
			  omp_add_variable (octx, decl,
					    GOVD_LASTPRIVATE | GOVD_SEEN);
			  octx = octx->outer_context;
			  if (octx
			      && octx->region_type == ORT_COMBINED_TEAMS
			      && (splay_tree_lookup (octx->variables,
						     (splay_tree_key) decl)
				  == NULL))
			    {
			      omp_add_variable (octx, decl,
						GOVD_SHARED | GOVD_SEEN);
			      octx = octx->outer_context;
			    }
			}
		      if (octx)
			omp_notice_variable (octx, decl, true);
		    }
		}
	      else if (outer_ctx->outer_context)
		omp_notice_variable (outer_ctx->outer_context, decl, true);
	    }
	  goto do_add;
	case OMP_CLAUSE_REDUCTION:
	  flags = GOVD_REDUCTION | GOVD_SEEN | GOVD_EXPLICIT;
	  /* OpenACC permits reductions on private variables.  */
	  if (!(region_type & ORT_ACC))
	    check_non_private = "reduction";
	  decl = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (decl) == MEM_REF)
	    {
	      tree type = TREE_TYPE (decl);
	      if (gimplify_expr (&TYPE_MAX_VALUE (TYPE_DOMAIN (type)), pre_p,
				 NULL, is_gimple_val, fb_rvalue, false)
		  == GS_ERROR)
		{
		  remove = true;
		  break;
		}
	      tree v = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	      if (DECL_P (v))
		{
		  omp_firstprivatize_variable (ctx, v);
		  omp_notice_variable (ctx, v, true);
		}
	      decl = TREE_OPERAND (decl, 0);
	      if (TREE_CODE (decl) == POINTER_PLUS_EXPR)
		{
		  if (gimplify_expr (&TREE_OPERAND (decl, 1), pre_p,
				     NULL, is_gimple_val, fb_rvalue, false)
		      == GS_ERROR)
		    {
		      remove = true;
		      break;
		    }
		  v = TREE_OPERAND (decl, 1);
		  if (DECL_P (v))
		    {
		      omp_firstprivatize_variable (ctx, v);
		      omp_notice_variable (ctx, v, true);
		    }
		  decl = TREE_OPERAND (decl, 0);
		}
	      if (TREE_CODE (decl) == ADDR_EXPR
		  || TREE_CODE (decl) == INDIRECT_REF)
		decl = TREE_OPERAND (decl, 0);
	    }
	  goto do_add_decl;
	case OMP_CLAUSE_LINEAR:
	  if (gimplify_expr (&OMP_CLAUSE_LINEAR_STEP (c), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    {
	      remove = true;
	      break;
	    }
	  else
	    {
	      if (code == OMP_SIMD
		  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
		{
		  struct gimplify_omp_ctx *octx = outer_ctx;
		  if (octx
		      && octx->region_type == ORT_WORKSHARE
		      && octx->combined_loop
		      && !octx->distribute)
		    {
		      if (octx->outer_context
			  && (octx->outer_context->region_type
			      == ORT_COMBINED_PARALLEL))
			octx = octx->outer_context->outer_context;
		      else
			octx = octx->outer_context;
		    }
		  if (octx
		      && octx->region_type == ORT_WORKSHARE
		      && octx->combined_loop
		      && octx->distribute)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%<linear%> clause for variable other than "
				"loop iterator specified on construct "
				"combined with %<distribute%>");
		      remove = true;
		      break;
		    }
		}
	      /* For combined #pragma omp parallel for simd, need to put
		 lastprivate and perhaps firstprivate too on the
		 parallel.  Similarly for #pragma omp for simd.  */
	      struct gimplify_omp_ctx *octx = outer_ctx;
	      decl = NULL_TREE;
	      do
		{
		  if (OMP_CLAUSE_LINEAR_NO_COPYIN (c)
		      && OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		    break;
		  decl = OMP_CLAUSE_DECL (c);
		  if (error_operand_p (decl))
		    {
		      decl = NULL_TREE;
		      break;
		    }
		  flags = GOVD_SEEN;
		  if (!OMP_CLAUSE_LINEAR_NO_COPYIN (c))
		    flags |= GOVD_FIRSTPRIVATE;
		  if (!OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		    flags |= GOVD_LASTPRIVATE;
		  if (octx
		      && octx->region_type == ORT_WORKSHARE
		      && octx->combined_loop)
		    {
		      if (octx->outer_context
			  && (octx->outer_context->region_type
			      == ORT_COMBINED_PARALLEL))
			octx = octx->outer_context;
		      else if (omp_check_private (octx, decl, false))
			break;
		    }
		  else if (octx
			   && (octx->region_type & ORT_TASK) != 0
			   && octx->combined_loop)
		    ;
		  else if (octx
			   && octx->region_type == ORT_COMBINED_PARALLEL
			   && ctx->region_type == ORT_WORKSHARE
			   && octx == outer_ctx)
		    flags = GOVD_SEEN | GOVD_SHARED;
		  else if (octx
			   && octx->region_type == ORT_COMBINED_TEAMS)
		    flags = GOVD_SEEN | GOVD_SHARED;
		  else if (octx
			   && octx->region_type == ORT_COMBINED_TARGET)
		    {
		      flags &= ~GOVD_LASTPRIVATE;
		      if (flags == GOVD_SEEN)
			break;
		    }
		  else
		    break;
		  splay_tree_node on
		    = splay_tree_lookup (octx->variables,
					 (splay_tree_key) decl);
		  if (on && (on->value & GOVD_DATA_SHARE_CLASS) != 0)
		    {
		      octx = NULL;
		      break;
		    }
		  omp_add_variable (octx, decl, flags);
		  if (octx->outer_context == NULL)
		    break;
		  octx = octx->outer_context;
		}
	      while (1);
	      if (octx
		  && decl
		  && (!OMP_CLAUSE_LINEAR_NO_COPYIN (c)
		      || !OMP_CLAUSE_LINEAR_NO_COPYOUT (c)))
		omp_notice_variable (octx, decl, true);
	    }
	  flags = GOVD_LINEAR | GOVD_EXPLICIT;
	  if (OMP_CLAUSE_LINEAR_NO_COPYIN (c)
	      && OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
	    {
	      notice_outer = false;
	      flags |= GOVD_LINEAR_LASTPRIVATE_NO_OUTER;
	    }
	  goto do_add;

	case OMP_CLAUSE_MAP:
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    remove = true;
	  switch (code)
	    {
	    case OMP_TARGET:
	      break;
	    case OACC_DATA:
	      if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
		break;
	      /* FALLTHRU */
	    case OMP_TARGET_DATA:
	    case OMP_TARGET_ENTER_DATA:
	    case OMP_TARGET_EXIT_DATA:
	    case OACC_ENTER_DATA:
	    case OACC_EXIT_DATA:
	    case OACC_HOST_DATA:
	      if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		  || (OMP_CLAUSE_MAP_KIND (c)
		      == GOMP_MAP_FIRSTPRIVATE_REFERENCE))
		/* For target {,enter ,exit }data only the array slice is
		   mapped, but not the pointer to it.  */
		remove = true;
	      break;
	    default:
	      break;
	    }
	  if (remove)
	    break;
	  if (DECL_P (decl) && outer_ctx && (region_type & ORT_ACC))
	    {
	      struct gimplify_omp_ctx *octx;
	      for (octx = outer_ctx; octx; octx = octx->outer_context)
	        {
		  if (octx->region_type != ORT_ACC_HOST_DATA)
		    break;
		  splay_tree_node n2
		    = splay_tree_lookup (octx->variables,
					 (splay_tree_key) decl);
		  if (n2)
		    error_at (OMP_CLAUSE_LOCATION (c), "variable %qE "
			      "declared in enclosing %<host_data%> region",
			      DECL_NAME (decl));
		}
	    }
	  if (OMP_CLAUSE_SIZE (c) == NULL_TREE)
	    OMP_CLAUSE_SIZE (c) = DECL_P (decl) ? DECL_SIZE_UNIT (decl)
				  : TYPE_SIZE_UNIT (TREE_TYPE (decl));
	  if (gimplify_expr (&OMP_CLAUSE_SIZE (c), pre_p,
			     NULL, is_gimple_val, fb_rvalue) == GS_ERROR)
	    {
	      remove = true;
	      break;
	    }
	  else if ((OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		    || (OMP_CLAUSE_MAP_KIND (c)
			== GOMP_MAP_FIRSTPRIVATE_REFERENCE))
		   && TREE_CODE (OMP_CLAUSE_SIZE (c)) != INTEGER_CST)
	    {
	      OMP_CLAUSE_SIZE (c)
		= get_initialized_tmp_var (OMP_CLAUSE_SIZE (c), pre_p, NULL,
					   false);
	      omp_add_variable (ctx, OMP_CLAUSE_SIZE (c),
				GOVD_FIRSTPRIVATE | GOVD_SEEN);
	    }
	  if (!DECL_P (decl))
	    {
	      tree d = decl, *pd;
	      if (TREE_CODE (d) == ARRAY_REF)
		{
		  while (TREE_CODE (d) == ARRAY_REF)
		    d = TREE_OPERAND (d, 0);
		  if (TREE_CODE (d) == COMPONENT_REF
		      && TREE_CODE (TREE_TYPE (d)) == ARRAY_TYPE)
		    decl = d;
		}
	      pd = &OMP_CLAUSE_DECL (c);
	      if (d == decl
		  && TREE_CODE (decl) == INDIRECT_REF
		  && TREE_CODE (TREE_OPERAND (decl, 0)) == COMPONENT_REF
		  && (TREE_CODE (TREE_TYPE (TREE_OPERAND (decl, 0)))
		      == REFERENCE_TYPE))
		{
		  pd = &TREE_OPERAND (decl, 0);
		  decl = TREE_OPERAND (decl, 0);
		}
	      if (TREE_CODE (decl) == COMPONENT_REF)
		{
		  while (TREE_CODE (decl) == COMPONENT_REF)
		    decl = TREE_OPERAND (decl, 0);
		  if (TREE_CODE (decl) == INDIRECT_REF
		      && DECL_P (TREE_OPERAND (decl, 0))
		      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (decl, 0)))
			  == REFERENCE_TYPE))
		    decl = TREE_OPERAND (decl, 0);
		}
	      if (gimplify_expr (pd, pre_p, NULL, is_gimple_lvalue, fb_lvalue)
		  == GS_ERROR)
		{
		  remove = true;
		  break;
		}
	      if (DECL_P (decl))
		{
		  if (error_operand_p (decl))
		    {
		      remove = true;
		      break;
		    }

		  tree stype = TREE_TYPE (decl);
		  if (TREE_CODE (stype) == REFERENCE_TYPE)
		    stype = TREE_TYPE (stype);
		  if (TYPE_SIZE_UNIT (stype) == NULL
		      || TREE_CODE (TYPE_SIZE_UNIT (stype)) != INTEGER_CST)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"mapping field %qE of variable length "
				"structure", OMP_CLAUSE_DECL (c));
		      remove = true;
		      break;
		    }

		  if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_POINTER)
		    {
		      /* Error recovery.  */
		      if (prev_list_p == NULL)
			{
			  remove = true;
			  break;
			}
		      if (OMP_CLAUSE_CHAIN (*prev_list_p) != c)
			{
			  tree ch = OMP_CLAUSE_CHAIN (*prev_list_p);
			  if (ch == NULL_TREE || OMP_CLAUSE_CHAIN (ch) != c)
			    {
			      remove = true;
			      break;
			    }
			}
		    }

		  tree offset;
		  poly_int64 bitsize, bitpos;
		  machine_mode mode;
		  int unsignedp, reversep, volatilep = 0;
		  tree base = OMP_CLAUSE_DECL (c);
		  while (TREE_CODE (base) == ARRAY_REF)
		    base = TREE_OPERAND (base, 0);
		  if (TREE_CODE (base) == INDIRECT_REF)
		    base = TREE_OPERAND (base, 0);
		  base = get_inner_reference (base, &bitsize, &bitpos, &offset,
					      &mode, &unsignedp, &reversep,
					      &volatilep);
		  tree orig_base = base;
		  if ((TREE_CODE (base) == INDIRECT_REF
		       || (TREE_CODE (base) == MEM_REF
			   && integer_zerop (TREE_OPERAND (base, 1))))
		      && DECL_P (TREE_OPERAND (base, 0))
		      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (base, 0)))
			  == REFERENCE_TYPE))
		    base = TREE_OPERAND (base, 0);
		  gcc_assert (base == decl
			      && (offset == NULL_TREE
				  || poly_int_tree_p (offset)));

		  splay_tree_node n
		    = splay_tree_lookup (ctx->variables, (splay_tree_key)decl);
		  bool ptr = (OMP_CLAUSE_MAP_KIND (c)
			      == GOMP_MAP_ALWAYS_POINTER);
		  if (n == NULL || (n->value & GOVD_MAP) == 0)
		    {
		      tree l = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						 OMP_CLAUSE_MAP);
		      OMP_CLAUSE_SET_MAP_KIND (l, GOMP_MAP_STRUCT);
		      if (orig_base != base)
			OMP_CLAUSE_DECL (l) = unshare_expr (orig_base);
		      else
			OMP_CLAUSE_DECL (l) = decl;
		      OMP_CLAUSE_SIZE (l) = size_int (1);
		      if (struct_map_to_clause == NULL)
			struct_map_to_clause = new hash_map<tree, tree>;
		      struct_map_to_clause->put (decl, l);
		      if (ptr)
			{
			  enum gomp_map_kind mkind
			    = code == OMP_TARGET_EXIT_DATA
			      ? GOMP_MAP_RELEASE : GOMP_MAP_ALLOC;
			  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						      OMP_CLAUSE_MAP);
			  OMP_CLAUSE_SET_MAP_KIND (c2, mkind);
			  OMP_CLAUSE_DECL (c2)
			    = unshare_expr (OMP_CLAUSE_DECL (c));
			  OMP_CLAUSE_CHAIN (c2) = *prev_list_p;
			  OMP_CLAUSE_SIZE (c2)
			    = TYPE_SIZE_UNIT (ptr_type_node);
			  OMP_CLAUSE_CHAIN (l) = c2;
			  if (OMP_CLAUSE_CHAIN (*prev_list_p) != c)
			    {
			      tree c4 = OMP_CLAUSE_CHAIN (*prev_list_p);
			      tree c3
				= build_omp_clause (OMP_CLAUSE_LOCATION (c),
						    OMP_CLAUSE_MAP);
			      OMP_CLAUSE_SET_MAP_KIND (c3, mkind);
			      OMP_CLAUSE_DECL (c3)
				= unshare_expr (OMP_CLAUSE_DECL (c4));
			      OMP_CLAUSE_SIZE (c3)
				= TYPE_SIZE_UNIT (ptr_type_node);
			      OMP_CLAUSE_CHAIN (c3) = *prev_list_p;
			      OMP_CLAUSE_CHAIN (c2) = c3;
			    }
			  *prev_list_p = l;
			  prev_list_p = NULL;
			}
		      else
			{
			  OMP_CLAUSE_CHAIN (l) = c;
			  *list_p = l;
			  list_p = &OMP_CLAUSE_CHAIN (l);
			}
		      if (orig_base != base && code == OMP_TARGET)
			{
			  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						      OMP_CLAUSE_MAP);
			  enum gomp_map_kind mkind
			    = GOMP_MAP_FIRSTPRIVATE_REFERENCE;
			  OMP_CLAUSE_SET_MAP_KIND (c2, mkind);
			  OMP_CLAUSE_DECL (c2) = decl;
			  OMP_CLAUSE_SIZE (c2) = size_zero_node;
			  OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (l);
			  OMP_CLAUSE_CHAIN (l) = c2;
			}
		      flags = GOVD_MAP | GOVD_EXPLICIT;
		      if (GOMP_MAP_ALWAYS_P (OMP_CLAUSE_MAP_KIND (c)) || ptr)
			flags |= GOVD_SEEN;
		      goto do_add_decl;
		    }
		  else
		    {
		      tree *osc = struct_map_to_clause->get (decl);
		      tree *sc = NULL, *scp = NULL;
		      if (GOMP_MAP_ALWAYS_P (OMP_CLAUSE_MAP_KIND (c)) || ptr)
			n->value |= GOVD_SEEN;
		      poly_offset_int o1, o2;
		      if (offset)
			o1 = wi::to_poly_offset (offset);
		      else
			o1 = 0;
		      if (maybe_ne (bitpos, 0))
			o1 += bits_to_bytes_round_down (bitpos);
		      sc = &OMP_CLAUSE_CHAIN (*osc);
		      if (*sc != c
			  && (OMP_CLAUSE_MAP_KIND (*sc)
			      == GOMP_MAP_FIRSTPRIVATE_REFERENCE)) 
			sc = &OMP_CLAUSE_CHAIN (*sc);
		      for (; *sc != c; sc = &OMP_CLAUSE_CHAIN (*sc))
			if (ptr && sc == prev_list_p)
			  break;
			else if (TREE_CODE (OMP_CLAUSE_DECL (*sc))
				 != COMPONENT_REF
				 && (TREE_CODE (OMP_CLAUSE_DECL (*sc))
				     != INDIRECT_REF)
				 && (TREE_CODE (OMP_CLAUSE_DECL (*sc))
				     != ARRAY_REF))
			  break;
			else
			  {
			    tree offset2;
			    poly_int64 bitsize2, bitpos2;
			    base = OMP_CLAUSE_DECL (*sc);
			    if (TREE_CODE (base) == ARRAY_REF)
			      {
				while (TREE_CODE (base) == ARRAY_REF)
				  base = TREE_OPERAND (base, 0);
				if (TREE_CODE (base) != COMPONENT_REF
				    || (TREE_CODE (TREE_TYPE (base))
					!= ARRAY_TYPE))
				  break;
			      }
			    else if (TREE_CODE (base) == INDIRECT_REF
				     && (TREE_CODE (TREE_OPERAND (base, 0))
					 == COMPONENT_REF)
				     && (TREE_CODE (TREE_TYPE
						     (TREE_OPERAND (base, 0)))
					 == REFERENCE_TYPE))
			      base = TREE_OPERAND (base, 0);
			    base = get_inner_reference (base, &bitsize2,
							&bitpos2, &offset2,
							&mode, &unsignedp,
							&reversep, &volatilep);
			    if ((TREE_CODE (base) == INDIRECT_REF
				 || (TREE_CODE (base) == MEM_REF
				     && integer_zerop (TREE_OPERAND (base,
								     1))))
				&& DECL_P (TREE_OPERAND (base, 0))
				&& (TREE_CODE (TREE_TYPE (TREE_OPERAND (base,
									0)))
				    == REFERENCE_TYPE))
			      base = TREE_OPERAND (base, 0);
			    if (base != decl)
			      break;
			    if (scp)
			      continue;
			    gcc_assert (offset == NULL_TREE
					|| poly_int_tree_p (offset));
			    tree d1 = OMP_CLAUSE_DECL (*sc);
			    tree d2 = OMP_CLAUSE_DECL (c);
			    while (TREE_CODE (d1) == ARRAY_REF)
			      d1 = TREE_OPERAND (d1, 0);
			    while (TREE_CODE (d2) == ARRAY_REF)
			      d2 = TREE_OPERAND (d2, 0);
			    if (TREE_CODE (d1) == INDIRECT_REF)
			      d1 = TREE_OPERAND (d1, 0);
			    if (TREE_CODE (d2) == INDIRECT_REF)
			      d2 = TREE_OPERAND (d2, 0);
			    while (TREE_CODE (d1) == COMPONENT_REF)
			      if (TREE_CODE (d2) == COMPONENT_REF
				  && TREE_OPERAND (d1, 1)
				     == TREE_OPERAND (d2, 1))
				{
				  d1 = TREE_OPERAND (d1, 0);
				  d2 = TREE_OPERAND (d2, 0);
				}
			      else
				break;
			    if (d1 == d2)
			      {
				error_at (OMP_CLAUSE_LOCATION (c),
					  "%qE appears more than once in map "
					  "clauses", OMP_CLAUSE_DECL (c));
				remove = true;
				break;
			      }
			    if (offset2)
			      o2 = wi::to_poly_offset (offset2);
			    else
			      o2 = 0;
			    o2 += bits_to_bytes_round_down (bitpos2);
			    if (maybe_lt (o1, o2)
				|| (known_eq (o1, 2)
				    && maybe_lt (bitpos, bitpos2)))
			      {
				if (ptr)
				  scp = sc;
				else
				  break;
			      }
			  }
		      if (remove)
			break;
		      OMP_CLAUSE_SIZE (*osc)
			= size_binop (PLUS_EXPR, OMP_CLAUSE_SIZE (*osc),
				      size_one_node);
		      if (ptr)
			{
			  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						      OMP_CLAUSE_MAP);
			  tree cl = NULL_TREE;
			  enum gomp_map_kind mkind
			    = code == OMP_TARGET_EXIT_DATA
			      ? GOMP_MAP_RELEASE : GOMP_MAP_ALLOC;
			  OMP_CLAUSE_SET_MAP_KIND (c2, mkind);
			  OMP_CLAUSE_DECL (c2)
			    = unshare_expr (OMP_CLAUSE_DECL (c));
			  OMP_CLAUSE_CHAIN (c2) = scp ? *scp : *prev_list_p;
			  OMP_CLAUSE_SIZE (c2)
			    = TYPE_SIZE_UNIT (ptr_type_node);
			  cl = scp ? *prev_list_p : c2;
			  if (OMP_CLAUSE_CHAIN (*prev_list_p) != c)
			    {
			      tree c4 = OMP_CLAUSE_CHAIN (*prev_list_p);
			      tree c3
				= build_omp_clause (OMP_CLAUSE_LOCATION (c),
						    OMP_CLAUSE_MAP);
			      OMP_CLAUSE_SET_MAP_KIND (c3, mkind);
			      OMP_CLAUSE_DECL (c3)
				= unshare_expr (OMP_CLAUSE_DECL (c4));
			      OMP_CLAUSE_SIZE (c3)
				= TYPE_SIZE_UNIT (ptr_type_node);
			      OMP_CLAUSE_CHAIN (c3) = *prev_list_p;
			      if (!scp)
				OMP_CLAUSE_CHAIN (c2) = c3;
			      else
				cl = c3;
			    }
			  if (scp)
			    *scp = c2;
			  if (sc == prev_list_p)
			    {
			      *sc = cl;
			      prev_list_p = NULL;
			    }
			  else
			    {
			      *prev_list_p = OMP_CLAUSE_CHAIN (c);
			      list_p = prev_list_p;
			      prev_list_p = NULL;
			      OMP_CLAUSE_CHAIN (c) = *sc;
			      *sc = cl;
			      continue;
			    }
			}
		      else if (*sc != c)
			{
			  *list_p = OMP_CLAUSE_CHAIN (c);
			  OMP_CLAUSE_CHAIN (c) = *sc;
			  *sc = c;
			  continue;
			}
		    }
		}
	      if (!remove
		  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ALWAYS_POINTER
		  && OMP_CLAUSE_CHAIN (c)
		  && OMP_CLAUSE_CODE (OMP_CLAUSE_CHAIN (c)) == OMP_CLAUSE_MAP
		  && (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
		      == GOMP_MAP_ALWAYS_POINTER))
		prev_list_p = list_p;
	      break;
	    }
	  flags = GOVD_MAP | GOVD_EXPLICIT;
	  if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_TO
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_TOFROM)
	    flags |= GOVD_MAP_ALWAYS_TO;
	  goto do_add;

	case OMP_CLAUSE_DEPEND:
	  if (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK)
	    {
	      tree deps = OMP_CLAUSE_DECL (c);
	      while (deps && TREE_CODE (deps) == TREE_LIST)
		{
		  if (TREE_CODE (TREE_PURPOSE (deps)) == TRUNC_DIV_EXPR
		      && DECL_P (TREE_OPERAND (TREE_PURPOSE (deps), 1)))
		    gimplify_expr (&TREE_OPERAND (TREE_PURPOSE (deps), 1),
				   pre_p, NULL, is_gimple_val, fb_rvalue);
		  deps = TREE_CHAIN (deps);
		}
	      break;
	    }
	  else if (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE)
	    break;
	  if (TREE_CODE (OMP_CLAUSE_DECL (c)) == COMPOUND_EXPR)
	    {
	      gimplify_expr (&TREE_OPERAND (OMP_CLAUSE_DECL (c), 0), pre_p,
			     NULL, is_gimple_val, fb_rvalue);
	      OMP_CLAUSE_DECL (c) = TREE_OPERAND (OMP_CLAUSE_DECL (c), 1);
	    }
	  if (error_operand_p (OMP_CLAUSE_DECL (c)))
	    {
	      remove = true;
	      break;
	    }
	  OMP_CLAUSE_DECL (c) = build_fold_addr_expr (OMP_CLAUSE_DECL (c));
	  if (gimplify_expr (&OMP_CLAUSE_DECL (c), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    {
	      remove = true;
	      break;
	    }
	  break;

	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE__CACHE_:
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    {
	      remove = true;
	      break;
	    }
	  if (OMP_CLAUSE_SIZE (c) == NULL_TREE)
	    OMP_CLAUSE_SIZE (c) = DECL_P (decl) ? DECL_SIZE_UNIT (decl)
				  : TYPE_SIZE_UNIT (TREE_TYPE (decl));
	  if (gimplify_expr (&OMP_CLAUSE_SIZE (c), pre_p,
			     NULL, is_gimple_val, fb_rvalue) == GS_ERROR)
	    {
	      remove = true;
	      break;
	    }
	  if (!DECL_P (decl))
	    {
	      if (gimplify_expr (&OMP_CLAUSE_DECL (c), pre_p,
				 NULL, is_gimple_lvalue, fb_lvalue)
		  == GS_ERROR)
		{
		  remove = true;
		  break;
		}
	      break;
	    }
	  goto do_notice;

	case OMP_CLAUSE_USE_DEVICE_PTR:
	  flags = GOVD_FIRSTPRIVATE | GOVD_EXPLICIT;
	  goto do_add;
	case OMP_CLAUSE_IS_DEVICE_PTR:
	  flags = GOVD_FIRSTPRIVATE | GOVD_EXPLICIT;
	  goto do_add;

	do_add:
	  decl = OMP_CLAUSE_DECL (c);
	do_add_decl:
	  if (error_operand_p (decl))
	    {
	      remove = true;
	      break;
	    }
	  if (DECL_NAME (decl) == NULL_TREE && (flags & GOVD_SHARED) == 0)
	    {
	      tree t = omp_member_access_dummy_var (decl);
	      if (t)
		{
		  tree v = DECL_VALUE_EXPR (decl);
		  DECL_NAME (decl) = DECL_NAME (TREE_OPERAND (v, 1));
		  if (outer_ctx)
		    omp_notice_variable (outer_ctx, t, true);
		}
	    }
	  if (code == OACC_DATA
	      && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER)
	    flags |= GOVD_MAP_0LEN_ARRAY;
	  omp_add_variable (ctx, decl, flags);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	      && OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    {
	      omp_add_variable (ctx, OMP_CLAUSE_REDUCTION_PLACEHOLDER (c),
				GOVD_LOCAL | GOVD_SEEN);
	      if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c)
		  && walk_tree (&OMP_CLAUSE_REDUCTION_INIT (c),
				find_decl_expr,
				OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c),
				NULL) == NULL_TREE)
		omp_add_variable (ctx,
				  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c),
				  GOVD_LOCAL | GOVD_SEEN);
	      gimplify_omp_ctxp = ctx;
	      push_gimplify_context ();

	      OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
	      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;

	      gimplify_and_add (OMP_CLAUSE_REDUCTION_INIT (c),
		  		&OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c));
	      pop_gimplify_context
		(gimple_seq_first_stmt (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c)));
	      push_gimplify_context ();
	      gimplify_and_add (OMP_CLAUSE_REDUCTION_MERGE (c),
		  		&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c));
	      pop_gimplify_context
		(gimple_seq_first_stmt (OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c)));
	      OMP_CLAUSE_REDUCTION_INIT (c) = NULL_TREE;
	      OMP_CLAUSE_REDUCTION_MERGE (c) = NULL_TREE;

	      gimplify_omp_ctxp = outer_ctx;
	    }
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		   && OMP_CLAUSE_LASTPRIVATE_STMT (c))
	    {
	      gimplify_omp_ctxp = ctx;
	      push_gimplify_context ();
	      if (TREE_CODE (OMP_CLAUSE_LASTPRIVATE_STMT (c)) != BIND_EXPR)
		{
		  tree bind = build3 (BIND_EXPR, void_type_node, NULL,
				      NULL, NULL);
		  TREE_SIDE_EFFECTS (bind) = 1;
		  BIND_EXPR_BODY (bind) = OMP_CLAUSE_LASTPRIVATE_STMT (c);
		  OMP_CLAUSE_LASTPRIVATE_STMT (c) = bind;
		}
	      gimplify_and_add (OMP_CLAUSE_LASTPRIVATE_STMT (c),
				&OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c));
	      pop_gimplify_context
		(gimple_seq_first_stmt (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c)));
	      OMP_CLAUSE_LASTPRIVATE_STMT (c) = NULL_TREE;

	      gimplify_omp_ctxp = outer_ctx;
	    }
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		   && OMP_CLAUSE_LINEAR_STMT (c))
	    {
	      gimplify_omp_ctxp = ctx;
	      push_gimplify_context ();
	      if (TREE_CODE (OMP_CLAUSE_LINEAR_STMT (c)) != BIND_EXPR)
		{
		  tree bind = build3 (BIND_EXPR, void_type_node, NULL,
				      NULL, NULL);
		  TREE_SIDE_EFFECTS (bind) = 1;
		  BIND_EXPR_BODY (bind) = OMP_CLAUSE_LINEAR_STMT (c);
		  OMP_CLAUSE_LINEAR_STMT (c) = bind;
		}
	      gimplify_and_add (OMP_CLAUSE_LINEAR_STMT (c),
				&OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c));
	      pop_gimplify_context
		(gimple_seq_first_stmt (OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c)));
	      OMP_CLAUSE_LINEAR_STMT (c) = NULL_TREE;

	      gimplify_omp_ctxp = outer_ctx;
	    }
	  if (notice_outer)
	    goto do_notice;
	  break;

	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COPYPRIVATE:
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    {
	      remove = true;
	      break;
	    }
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_COPYPRIVATE
	      && !remove
	      && !omp_check_private (ctx, decl, true))
	    {
	      remove = true;
	      if (is_global_var (decl))
		{
		  if (DECL_THREAD_LOCAL_P (decl))
		    remove = false;
		  else if (DECL_HAS_VALUE_EXPR_P (decl))
		    {
		      tree value = get_base_address (DECL_VALUE_EXPR (decl));

		      if (value
			  && DECL_P (value)
			  && DECL_THREAD_LOCAL_P (value))
			remove = false;
		    }
		}
	      if (remove)
		error_at (OMP_CLAUSE_LOCATION (c),
			  "copyprivate variable %qE is not threadprivate"
			  " or private in outer context", DECL_NAME (decl));
	    }
	do_notice:
	  if (outer_ctx)
	    omp_notice_variable (outer_ctx, decl, true);
	  if (check_non_private
	      && region_type == ORT_WORKSHARE
	      && (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION
		  || decl == OMP_CLAUSE_DECL (c)
		  || (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF
		      && (TREE_CODE (TREE_OPERAND (OMP_CLAUSE_DECL (c), 0))
			  == ADDR_EXPR
			  || (TREE_CODE (TREE_OPERAND (OMP_CLAUSE_DECL (c), 0))
			      == POINTER_PLUS_EXPR
			      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND
						(OMP_CLAUSE_DECL (c), 0), 0))
				  == ADDR_EXPR)))))
	      && omp_check_private (ctx, decl, false))
	    {
	      error ("%s variable %qE is private in outer context",
		     check_non_private, DECL_NAME (decl));
	      remove = true;
	    }
	  break;

	case OMP_CLAUSE_IF:
	  if (OMP_CLAUSE_IF_MODIFIER (c) != ERROR_MARK
	      && OMP_CLAUSE_IF_MODIFIER (c) != code)
	    {
	      const char *p[2];
	      for (int i = 0; i < 2; i++)
		switch (i ? OMP_CLAUSE_IF_MODIFIER (c) : code)
		  {
		  case OMP_PARALLEL: p[i] = "parallel"; break;
		  case OMP_TASK: p[i] = "task"; break;
		  case OMP_TASKLOOP: p[i] = "taskloop"; break;
		  case OMP_TARGET_DATA: p[i] = "target data"; break;
		  case OMP_TARGET: p[i] = "target"; break;
		  case OMP_TARGET_UPDATE: p[i] = "target update"; break;
		  case OMP_TARGET_ENTER_DATA:
		    p[i] = "target enter data"; break;
		  case OMP_TARGET_EXIT_DATA: p[i] = "target exit data"; break;
		  default: gcc_unreachable ();
		  }
	      error_at (OMP_CLAUSE_LOCATION (c),
			"expected %qs %<if%> clause modifier rather than %qs",
			p[0], p[1]);
	      remove = true;
	    }
	  /* Fall through.  */

	case OMP_CLAUSE_FINAL:
	  OMP_CLAUSE_OPERAND (c, 0)
	    = gimple_boolify (OMP_CLAUSE_OPERAND (c, 0));
	  /* Fall through.  */

	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	  if (gimplify_expr (&OMP_CLAUSE_OPERAND (c, 0), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    remove = true;
	  break;

	case OMP_CLAUSE_GANG:
	  if (gimplify_expr (&OMP_CLAUSE_OPERAND (c, 0), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    remove = true;
	  if (gimplify_expr (&OMP_CLAUSE_OPERAND (c, 1), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    remove = true;
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	  break;

	case OMP_CLAUSE_DEFAULTMAP:
	  ctx->target_map_scalars_firstprivate = false;
	  break;

	case OMP_CLAUSE_ALIGNED:
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    {
	      remove = true;
	      break;
	    }
	  if (gimplify_expr (&OMP_CLAUSE_ALIGNED_ALIGNMENT (c), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    {
	      remove = true;
	      break;
	    }
	  if (!is_global_var (decl)
	      && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE)
	    omp_add_variable (ctx, decl, GOVD_ALIGNED);
	  break;

	case OMP_CLAUSE_DEFAULT:
	  ctx->default_kind = OMP_CLAUSE_DEFAULT_KIND (c);
	  break;

	default:
	  gcc_unreachable ();
	}

      if (code == OACC_DATA
	  && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	  && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER)
	remove = true;
      if (remove)
	*list_p = OMP_CLAUSE_CHAIN (c);
      else
	list_p = &OMP_CLAUSE_CHAIN (c);
    }

  gimplify_omp_ctxp = ctx;
  if (struct_map_to_clause)
    delete struct_map_to_clause;
}

/* Return true if DECL is a candidate for shared to firstprivate
   optimization.  We only consider non-addressable scalars, not
   too big, and not references.  */

static bool
omp_shared_to_firstprivate_optimizable_decl_p (tree decl)
{
  if (TREE_ADDRESSABLE (decl))
    return false;
  tree type = TREE_TYPE (decl);
  if (!is_gimple_reg_type (type)
      || TREE_CODE (type) == REFERENCE_TYPE
      || TREE_ADDRESSABLE (type))
    return false;
  /* Don't optimize too large decls, as each thread/task will have
     its own.  */
  HOST_WIDE_INT len = int_size_in_bytes (type);
  if (len == -1 || len > 4 * POINTER_SIZE / BITS_PER_UNIT)
    return false;
  if (lang_hooks.decls.omp_privatize_by_reference (decl))
    return false;
  return true;
}

/* Helper function of omp_find_stores_op and gimplify_adjust_omp_clauses*.
   For omp_shared_to_firstprivate_optimizable_decl_p decl mark it as
   GOVD_WRITTEN in outer contexts.  */

static void
omp_mark_stores (struct gimplify_omp_ctx *ctx, tree decl)
{
  for (; ctx; ctx = ctx->outer_context)
    {
      splay_tree_node n = splay_tree_lookup (ctx->variables,
					     (splay_tree_key) decl);
      if (n == NULL)
	continue;
      else if (n->value & GOVD_SHARED)
	{
	  n->value |= GOVD_WRITTEN;
	  return;
	}
      else if (n->value & GOVD_DATA_SHARE_CLASS)
	return;
    }
}

/* Helper callback for walk_gimple_seq to discover possible stores
   to omp_shared_to_firstprivate_optimizable_decl_p decls and set
   GOVD_WRITTEN if they are GOVD_SHARED in some outer context
   for those.  */

static tree
omp_find_stores_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;

  *walk_subtrees = 0;
  if (!wi->is_lhs)
    return NULL_TREE;

  tree op = *tp;
  do
    {
      if (handled_component_p (op))
	op = TREE_OPERAND (op, 0);
      else if ((TREE_CODE (op) == MEM_REF || TREE_CODE (op) == TARGET_MEM_REF)
	       && TREE_CODE (TREE_OPERAND (op, 0)) == ADDR_EXPR)
	op = TREE_OPERAND (TREE_OPERAND (op, 0), 0);
      else
	break;
    }
  while (1);
  if (!DECL_P (op) || !omp_shared_to_firstprivate_optimizable_decl_p (op))
    return NULL_TREE;

  omp_mark_stores (gimplify_omp_ctxp, op);
  return NULL_TREE;
}

/* Helper callback for walk_gimple_seq to discover possible stores
   to omp_shared_to_firstprivate_optimizable_decl_p decls and set
   GOVD_WRITTEN if they are GOVD_SHARED in some outer context
   for those.  */

static tree
omp_find_stores_stmt (gimple_stmt_iterator *gsi_p,
		      bool *handled_ops_p,
		      struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  switch (gimple_code (stmt))
    {
    /* Don't recurse on OpenMP constructs for which
       gimplify_adjust_omp_clauses already handled the bodies,
       except handle gimple_omp_for_pre_body.  */
    case GIMPLE_OMP_FOR:
      *handled_ops_p = true;
      if (gimple_omp_for_pre_body (stmt))
	walk_gimple_seq (gimple_omp_for_pre_body (stmt),
			 omp_find_stores_stmt, omp_find_stores_op, wi);
      break;
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_CRITICAL:
      *handled_ops_p = true;
      break;
    default:
      break;
    }
  return NULL_TREE;
}

struct gimplify_adjust_omp_clauses_data
{
  tree *list_p;
  gimple_seq *pre_p;
};

/* For all variables that were not actually used within the context,
   remove PRIVATE, SHARED, and FIRSTPRIVATE clauses.  */

static int
gimplify_adjust_omp_clauses_1 (splay_tree_node n, void *data)
{
  tree *list_p = ((struct gimplify_adjust_omp_clauses_data *) data)->list_p;
  gimple_seq *pre_p
    = ((struct gimplify_adjust_omp_clauses_data *) data)->pre_p;
  tree decl = (tree) n->key;
  unsigned flags = n->value;
  enum omp_clause_code code;
  tree clause;
  bool private_debug;

  if (flags & (GOVD_EXPLICIT | GOVD_LOCAL))
    return 0;
  if ((flags & GOVD_SEEN) == 0)
    return 0;
  if (flags & GOVD_DEBUG_PRIVATE)
    {
      gcc_assert ((flags & GOVD_DATA_SHARE_CLASS) == GOVD_SHARED);
      private_debug = true;
    }
  else if (flags & GOVD_MAP)
    private_debug = false;
  else
    private_debug
      = lang_hooks.decls.omp_private_debug_clause (decl,
						   !!(flags & GOVD_SHARED));
  if (private_debug)
    code = OMP_CLAUSE_PRIVATE;
  else if (flags & GOVD_MAP)
    {
      code = OMP_CLAUSE_MAP;
      if ((gimplify_omp_ctxp->region_type & ORT_ACC) == 0
	  && TYPE_ATOMIC (strip_array_types (TREE_TYPE (decl))))
	{
	  error ("%<_Atomic%> %qD in implicit %<map%> clause", decl);
	  return 0;
	}
    }
  else if (flags & GOVD_SHARED)
    {
      if (is_global_var (decl))
	{
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp->outer_context;
	  while (ctx != NULL)
	    {
	      splay_tree_node on
		= splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      if (on && (on->value & (GOVD_FIRSTPRIVATE | GOVD_LASTPRIVATE
				      | GOVD_PRIVATE | GOVD_REDUCTION
				      | GOVD_LINEAR | GOVD_MAP)) != 0)
		break;
	      ctx = ctx->outer_context;
	    }
	  if (ctx == NULL)
	    return 0;
	}
      code = OMP_CLAUSE_SHARED;
    }
  else if (flags & GOVD_PRIVATE)
    code = OMP_CLAUSE_PRIVATE;
  else if (flags & GOVD_FIRSTPRIVATE)
    {
      code = OMP_CLAUSE_FIRSTPRIVATE;
      if ((gimplify_omp_ctxp->region_type & ORT_TARGET)
	  && (gimplify_omp_ctxp->region_type & ORT_ACC) == 0
	  && TYPE_ATOMIC (strip_array_types (TREE_TYPE (decl))))
	{
	  error ("%<_Atomic%> %qD in implicit %<firstprivate%> clause on "
		 "%<target%> construct", decl);
	  return 0;
	}
    }
  else if (flags & GOVD_LASTPRIVATE)
    code = OMP_CLAUSE_LASTPRIVATE;
  else if (flags & GOVD_ALIGNED)
    return 0;
  else
    gcc_unreachable ();

  if (((flags & GOVD_LASTPRIVATE)
       || (code == OMP_CLAUSE_SHARED && (flags & GOVD_WRITTEN)))
      && omp_shared_to_firstprivate_optimizable_decl_p (decl))
    omp_mark_stores (gimplify_omp_ctxp->outer_context, decl);

  tree chain = *list_p;
  clause = build_omp_clause (input_location, code);
  OMP_CLAUSE_DECL (clause) = decl;
  OMP_CLAUSE_CHAIN (clause) = chain;
  if (private_debug)
    OMP_CLAUSE_PRIVATE_DEBUG (clause) = 1;
  else if (code == OMP_CLAUSE_PRIVATE && (flags & GOVD_PRIVATE_OUTER_REF))
    OMP_CLAUSE_PRIVATE_OUTER_REF (clause) = 1;
  else if (code == OMP_CLAUSE_SHARED
	   && (flags & GOVD_WRITTEN) == 0
	   && omp_shared_to_firstprivate_optimizable_decl_p (decl))
    OMP_CLAUSE_SHARED_READONLY (clause) = 1;
  else if (code == OMP_CLAUSE_FIRSTPRIVATE && (flags & GOVD_EXPLICIT) == 0)
    OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (clause) = 1;
  else if (code == OMP_CLAUSE_MAP && (flags & GOVD_MAP_0LEN_ARRAY) != 0)
    {
      tree nc = build_omp_clause (input_location, OMP_CLAUSE_MAP);
      OMP_CLAUSE_DECL (nc) = decl;
      if (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) == POINTER_TYPE)
	OMP_CLAUSE_DECL (clause)
	  = build_simple_mem_ref_loc (input_location, decl);
      OMP_CLAUSE_DECL (clause)
	= build2 (MEM_REF, char_type_node, OMP_CLAUSE_DECL (clause),
		  build_int_cst (build_pointer_type (char_type_node), 0));
      OMP_CLAUSE_SIZE (clause) = size_zero_node;
      OMP_CLAUSE_SIZE (nc) = size_zero_node;
      OMP_CLAUSE_SET_MAP_KIND (clause, GOMP_MAP_ALLOC);
      OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (clause) = 1;
      OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_FIRSTPRIVATE_POINTER);
      OMP_CLAUSE_CHAIN (nc) = chain;
      OMP_CLAUSE_CHAIN (clause) = nc;
      struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
      gimplify_omp_ctxp = ctx->outer_context;
      gimplify_expr (&TREE_OPERAND (OMP_CLAUSE_DECL (clause), 0),
		     pre_p, NULL, is_gimple_val, fb_rvalue);
      gimplify_omp_ctxp = ctx;
    }
  else if (code == OMP_CLAUSE_MAP)
    {
      int kind;
      /* Not all combinations of these GOVD_MAP flags are actually valid.  */
      switch (flags & (GOVD_MAP_TO_ONLY
		       | GOVD_MAP_FORCE
		       | GOVD_MAP_FORCE_PRESENT))
	{
	case 0:
	  kind = GOMP_MAP_TOFROM;
	  break;
	case GOVD_MAP_FORCE:
	  kind = GOMP_MAP_TOFROM | GOMP_MAP_FLAG_FORCE;
	  break;
	case GOVD_MAP_TO_ONLY:
	  kind = GOMP_MAP_TO;
	  break;
	case GOVD_MAP_TO_ONLY | GOVD_MAP_FORCE:
	  kind = GOMP_MAP_TO | GOMP_MAP_FLAG_FORCE;
	  break;
	case GOVD_MAP_FORCE_PRESENT:
	  kind = GOMP_MAP_FORCE_PRESENT;
	  break;
	default:
	  gcc_unreachable ();
	}
      OMP_CLAUSE_SET_MAP_KIND (clause, kind);
      if (DECL_SIZE (decl)
	  && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
	{
	  tree decl2 = DECL_VALUE_EXPR (decl);
	  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
	  decl2 = TREE_OPERAND (decl2, 0);
	  gcc_assert (DECL_P (decl2));
	  tree mem = build_simple_mem_ref (decl2);
	  OMP_CLAUSE_DECL (clause) = mem;
	  OMP_CLAUSE_SIZE (clause) = TYPE_SIZE_UNIT (TREE_TYPE (decl));
	  if (gimplify_omp_ctxp->outer_context)
	    {
	      struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp->outer_context;
	      omp_notice_variable (ctx, decl2, true);
	      omp_notice_variable (ctx, OMP_CLAUSE_SIZE (clause), true);
	    }
	  tree nc = build_omp_clause (OMP_CLAUSE_LOCATION (clause),
				      OMP_CLAUSE_MAP);
	  OMP_CLAUSE_DECL (nc) = decl;
	  OMP_CLAUSE_SIZE (nc) = size_zero_node;
	  if (gimplify_omp_ctxp->target_firstprivatize_array_bases)
	    OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_FIRSTPRIVATE_POINTER);
	  else
	    OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_POINTER);
	  OMP_CLAUSE_CHAIN (nc) = OMP_CLAUSE_CHAIN (clause);
	  OMP_CLAUSE_CHAIN (clause) = nc;
	}
      else if (gimplify_omp_ctxp->target_firstprivatize_array_bases
	       && lang_hooks.decls.omp_privatize_by_reference (decl))
	{
	  OMP_CLAUSE_DECL (clause) = build_simple_mem_ref (decl);
	  OMP_CLAUSE_SIZE (clause)
	    = unshare_expr (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl))));
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
	  gimplify_omp_ctxp = ctx->outer_context;
	  gimplify_expr (&OMP_CLAUSE_SIZE (clause),
			 pre_p, NULL, is_gimple_val, fb_rvalue);
	  gimplify_omp_ctxp = ctx;
	  tree nc = build_omp_clause (OMP_CLAUSE_LOCATION (clause),
				      OMP_CLAUSE_MAP);
	  OMP_CLAUSE_DECL (nc) = decl;
	  OMP_CLAUSE_SIZE (nc) = size_zero_node;
	  OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_FIRSTPRIVATE_REFERENCE);
	  OMP_CLAUSE_CHAIN (nc) = OMP_CLAUSE_CHAIN (clause);
	  OMP_CLAUSE_CHAIN (clause) = nc;
	}
      else
	OMP_CLAUSE_SIZE (clause) = DECL_SIZE_UNIT (decl);
    }
  if (code == OMP_CLAUSE_FIRSTPRIVATE && (flags & GOVD_LASTPRIVATE) != 0)
    {
      tree nc = build_omp_clause (input_location, OMP_CLAUSE_LASTPRIVATE);
      OMP_CLAUSE_DECL (nc) = decl;
      OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (nc) = 1;
      OMP_CLAUSE_CHAIN (nc) = chain;
      OMP_CLAUSE_CHAIN (clause) = nc;
      struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
      gimplify_omp_ctxp = ctx->outer_context;
      lang_hooks.decls.omp_finish_clause (nc, pre_p);
      gimplify_omp_ctxp = ctx;
    }
  *list_p = clause;
  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
  gimplify_omp_ctxp = ctx->outer_context;
  lang_hooks.decls.omp_finish_clause (clause, pre_p);
  if (gimplify_omp_ctxp)
    for (; clause != chain; clause = OMP_CLAUSE_CHAIN (clause))
      if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP
	  && DECL_P (OMP_CLAUSE_SIZE (clause)))
	omp_notice_variable (gimplify_omp_ctxp, OMP_CLAUSE_SIZE (clause),
			     true);
  gimplify_omp_ctxp = ctx;
  return 0;
}

static void
gimplify_adjust_omp_clauses (gimple_seq *pre_p, gimple_seq body, tree *list_p,
			     enum tree_code code)
{
  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
  tree c, decl;

  if (body)
    {
      struct gimplify_omp_ctx *octx;
      for (octx = ctx; octx; octx = octx->outer_context)
	if ((octx->region_type & (ORT_PARALLEL | ORT_TASK | ORT_TEAMS)) != 0)
	  break;
      if (octx)
	{
	  struct walk_stmt_info wi;
	  memset (&wi, 0, sizeof (wi));
	  walk_gimple_seq (body, omp_find_stores_stmt,
			   omp_find_stores_op, &wi);
	}
    }
  while ((c = *list_p) != NULL)
    {
      splay_tree_node n;
      bool remove = false;

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_FIRSTPRIVATE:
	  if ((ctx->region_type & ORT_TARGET)
	      && (ctx->region_type & ORT_ACC) == 0
	      && TYPE_ATOMIC (strip_array_types
					(TREE_TYPE (OMP_CLAUSE_DECL (c)))))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<_Atomic%> %qD in %<firstprivate%> clause on "
			"%<target%> construct", OMP_CLAUSE_DECL (c));
	      remove = true;
	      break;
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_LINEAR:
	  decl = OMP_CLAUSE_DECL (c);
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  remove = !(n->value & GOVD_SEEN);
	  if (! remove)
	    {
	      bool shared = OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED;
	      if ((n->value & GOVD_DEBUG_PRIVATE)
		  || lang_hooks.decls.omp_private_debug_clause (decl, shared))
		{
		  gcc_assert ((n->value & GOVD_DEBUG_PRIVATE) == 0
			      || ((n->value & GOVD_DATA_SHARE_CLASS)
				  == GOVD_SHARED));
		  OMP_CLAUSE_SET_CODE (c, OMP_CLAUSE_PRIVATE);
		  OMP_CLAUSE_PRIVATE_DEBUG (c) = 1;
		}
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
		  && (n->value & GOVD_WRITTEN) == 0
		  && DECL_P (decl)
		  && omp_shared_to_firstprivate_optimizable_decl_p (decl))
		OMP_CLAUSE_SHARED_READONLY (c) = 1;
	      else if (DECL_P (decl)
		       && ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
			    && (n->value & GOVD_WRITTEN) != 0)
			   || (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
			       && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c)))
		       && omp_shared_to_firstprivate_optimizable_decl_p (decl))
		omp_mark_stores (gimplify_omp_ctxp->outer_context, decl);
	    }
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  /* Make sure OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE is set to
	     accurately reflect the presence of a FIRSTPRIVATE clause.  */
	  decl = OMP_CLAUSE_DECL (c);
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c)
	    = (n->value & GOVD_FIRSTPRIVATE) != 0;
	  if (code == OMP_DISTRIBUTE
	      && OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
	    {
	      remove = true;
	      error_at (OMP_CLAUSE_LOCATION (c),
			"same variable used in %<firstprivate%> and "
			"%<lastprivate%> clauses on %<distribute%> "
			"construct");
	    }
	  if (!remove
	      && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && DECL_P (decl)
	      && omp_shared_to_firstprivate_optimizable_decl_p (decl))
	    omp_mark_stores (gimplify_omp_ctxp->outer_context, decl);
	  break;

	case OMP_CLAUSE_ALIGNED:
	  decl = OMP_CLAUSE_DECL (c);
	  if (!is_global_var (decl))
	    {
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      remove = n == NULL || !(n->value & GOVD_SEEN);
	      if (!remove && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE)
		{
		  struct gimplify_omp_ctx *octx;
		  if (n != NULL
		      && (n->value & (GOVD_DATA_SHARE_CLASS
				      & ~GOVD_FIRSTPRIVATE)))
		    remove = true;
		  else
		    for (octx = ctx->outer_context; octx;
			 octx = octx->outer_context)
		      {
			n = splay_tree_lookup (octx->variables,
					       (splay_tree_key) decl);
			if (n == NULL)
			  continue;
			if (n->value & GOVD_LOCAL)
			  break;
			/* We have to avoid assigning a shared variable
			   to itself when trying to add
			   __builtin_assume_aligned.  */
			if (n->value & GOVD_SHARED)
			  {
			    remove = true;
			    break;
			  }
		      }
		}
	    }
	  else if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    {
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      if (n != NULL && (n->value & GOVD_DATA_SHARE_CLASS) != 0)
		remove = true;
	    }
	  break;

	case OMP_CLAUSE_MAP:
	  if (code == OMP_TARGET_EXIT_DATA
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_POINTER)
	    {
	      remove = true;
	      break;
	    }
	  decl = OMP_CLAUSE_DECL (c);
	  /* Data clauses associated with acc parallel reductions must be
	     compatible with present_or_copy.  Warn and adjust the clause
	     if that is not the case.  */
	  if (ctx->region_type == ORT_ACC_PARALLEL)
	    {
	      tree t = DECL_P (decl) ? decl : TREE_OPERAND (decl, 0);
	      n = NULL;

	      if (DECL_P (t))
		n = splay_tree_lookup (ctx->variables, (splay_tree_key) t);

	      if (n && (n->value & GOVD_REDUCTION))
		{
		  enum gomp_map_kind kind = OMP_CLAUSE_MAP_KIND (c);

		  OMP_CLAUSE_MAP_IN_REDUCTION (c) = 1;
		  if ((kind & GOMP_MAP_TOFROM) != GOMP_MAP_TOFROM
		      && kind != GOMP_MAP_FORCE_PRESENT
		      && kind != GOMP_MAP_POINTER)
		    {
		      warning_at (OMP_CLAUSE_LOCATION (c), 0,
				  "incompatible data clause with reduction "
				  "on %qE; promoting to present_or_copy",
				  DECL_NAME (t));
		      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TOFROM);
		    }
		}
	    }
	  if (!DECL_P (decl))
	    {
	      if ((ctx->region_type & ORT_TARGET) != 0
		  && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER)
		{
		  if (TREE_CODE (decl) == INDIRECT_REF
		      && TREE_CODE (TREE_OPERAND (decl, 0)) == COMPONENT_REF
		      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (decl, 0)))
			  == REFERENCE_TYPE))
		    decl = TREE_OPERAND (decl, 0);
		  if (TREE_CODE (decl) == COMPONENT_REF)
		    {
		      while (TREE_CODE (decl) == COMPONENT_REF)
			decl = TREE_OPERAND (decl, 0);
		      if (DECL_P (decl))
			{
			  n = splay_tree_lookup (ctx->variables,
						 (splay_tree_key) decl);
			  if (!(n->value & GOVD_SEEN))
			    remove = true;
			}
		    }
		}
	      break;
	    }
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  if ((ctx->region_type & ORT_TARGET) != 0
	      && !(n->value & GOVD_SEEN)
	      && GOMP_MAP_ALWAYS_P (OMP_CLAUSE_MAP_KIND (c)) == 0
	      && (!is_global_var (decl)
		  || !lookup_attribute ("omp declare target link",
					DECL_ATTRIBUTES (decl))))
	    {
	      remove = true;
	      /* For struct element mapping, if struct is never referenced
		 in target block and none of the mapping has always modifier,
		 remove all the struct element mappings, which immediately
		 follow the GOMP_MAP_STRUCT map clause.  */
	      if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_STRUCT)
		{
		  HOST_WIDE_INT cnt = tree_to_shwi (OMP_CLAUSE_SIZE (c));
		  while (cnt--)
		    OMP_CLAUSE_CHAIN (c)
		      = OMP_CLAUSE_CHAIN (OMP_CLAUSE_CHAIN (c));
		}
	    }
	  else if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_STRUCT
		   && code == OMP_TARGET_EXIT_DATA)
	    remove = true;
	  else if (DECL_SIZE (decl)
		   && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST
		   && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_POINTER
		   && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FIRSTPRIVATE_POINTER
		   && (OMP_CLAUSE_MAP_KIND (c)
		       != GOMP_MAP_FIRSTPRIVATE_REFERENCE))
	    {
	      /* For GOMP_MAP_FORCE_DEVICEPTR, we'll never enter here, because
		 for these, TREE_CODE (DECL_SIZE (decl)) will always be
		 INTEGER_CST.  */
	      gcc_assert (OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FORCE_DEVICEPTR);

	      tree decl2 = DECL_VALUE_EXPR (decl);
	      gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
	      decl2 = TREE_OPERAND (decl2, 0);
	      gcc_assert (DECL_P (decl2));
	      tree mem = build_simple_mem_ref (decl2);
	      OMP_CLAUSE_DECL (c) = mem;
	      OMP_CLAUSE_SIZE (c) = TYPE_SIZE_UNIT (TREE_TYPE (decl));
	      if (ctx->outer_context)
		{
		  omp_notice_variable (ctx->outer_context, decl2, true);
		  omp_notice_variable (ctx->outer_context,
				       OMP_CLAUSE_SIZE (c), true);
		}
	      if (((ctx->region_type & ORT_TARGET) != 0
		   || !ctx->target_firstprivatize_array_bases)
		  && ((n->value & GOVD_SEEN) == 0
		      || (n->value & (GOVD_PRIVATE | GOVD_FIRSTPRIVATE)) == 0))
		{
		  tree nc = build_omp_clause (OMP_CLAUSE_LOCATION (c),
					      OMP_CLAUSE_MAP);
		  OMP_CLAUSE_DECL (nc) = decl;
		  OMP_CLAUSE_SIZE (nc) = size_zero_node;
		  if (ctx->target_firstprivatize_array_bases)
		    OMP_CLAUSE_SET_MAP_KIND (nc,
					     GOMP_MAP_FIRSTPRIVATE_POINTER);
		  else
		    OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_POINTER);
		  OMP_CLAUSE_CHAIN (nc) = OMP_CLAUSE_CHAIN (c);
		  OMP_CLAUSE_CHAIN (c) = nc;
		  c = nc;
		}
	    }
	  else
	    {
	      if (OMP_CLAUSE_SIZE (c) == NULL_TREE)
		OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (decl);
	      gcc_assert ((n->value & GOVD_SEEN) == 0
			  || ((n->value & (GOVD_PRIVATE | GOVD_FIRSTPRIVATE))
			      == 0));
	    }
	  break;

	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE__CACHE_:
	  decl = OMP_CLAUSE_DECL (c);
	  if (!DECL_P (decl))
	    break;
	  if (DECL_SIZE (decl)
	      && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
	    {
	      tree decl2 = DECL_VALUE_EXPR (decl);
	      gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
	      decl2 = TREE_OPERAND (decl2, 0);
	      gcc_assert (DECL_P (decl2));
	      tree mem = build_simple_mem_ref (decl2);
	      OMP_CLAUSE_DECL (c) = mem;
	      OMP_CLAUSE_SIZE (c) = TYPE_SIZE_UNIT (TREE_TYPE (decl));
	      if (ctx->outer_context)
		{
		  omp_notice_variable (ctx->outer_context, decl2, true);
		  omp_notice_variable (ctx->outer_context,
				       OMP_CLAUSE_SIZE (c), true);
		}
	    }
	  else if (OMP_CLAUSE_SIZE (c) == NULL_TREE)
	    OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (decl);
	  break;

	case OMP_CLAUSE_REDUCTION:
	  decl = OMP_CLAUSE_DECL (c);
	  /* OpenACC reductions need a present_or_copy data clause.
	     Add one if necessary.  Error is the reduction is private.  */
	  if (ctx->region_type == ORT_ACC_PARALLEL)
	    {
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      if (n->value & (GOVD_PRIVATE | GOVD_FIRSTPRIVATE))
		error_at (OMP_CLAUSE_LOCATION (c), "invalid private "
			  "reduction on %qE", DECL_NAME (decl));
	      else if ((n->value & GOVD_MAP) == 0)
		{
		  tree next = OMP_CLAUSE_CHAIN (c);
		  tree nc = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_MAP);
		  OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_TOFROM);
		  OMP_CLAUSE_DECL (nc) = decl;
		  OMP_CLAUSE_CHAIN (c) = nc;
		  lang_hooks.decls.omp_finish_clause (nc, pre_p);
		  while (1)
		    {
		      OMP_CLAUSE_MAP_IN_REDUCTION (nc) = 1;
		      if (OMP_CLAUSE_CHAIN (nc) == NULL)
			break;
		      nc = OMP_CLAUSE_CHAIN (nc);
		    }
		  OMP_CLAUSE_CHAIN (nc) = next;
		  n->value |= GOVD_MAP;
		}
	    }
	  if (DECL_P (decl)
	      && omp_shared_to_firstprivate_optimizable_decl_p (decl))
	    omp_mark_stores (gimplify_omp_ctxp->outer_context, decl);
	  break;
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_TILE:
	  break;

	default:
	  gcc_unreachable ();
	}

      if (remove)
	*list_p = OMP_CLAUSE_CHAIN (c);
      else
	list_p = &OMP_CLAUSE_CHAIN (c);
    }

  /* Add in any implicit data sharing.  */
  struct gimplify_adjust_omp_clauses_data data;
  data.list_p = list_p;
  data.pre_p = pre_p;
  splay_tree_foreach (ctx->variables, gimplify_adjust_omp_clauses_1, &data);

  gimplify_omp_ctxp = ctx->outer_context;
  delete_omp_context (ctx);
}

/* Gimplify OACC_CACHE.  */

static void
gimplify_oacc_cache (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p;

  gimplify_scan_omp_clauses (&OACC_CACHE_CLAUSES (expr), pre_p, ORT_ACC,
			     OACC_CACHE);
  gimplify_adjust_omp_clauses (pre_p, NULL, &OACC_CACHE_CLAUSES (expr),
			       OACC_CACHE);

  /* TODO: Do something sensible with this information.  */

  *expr_p = NULL_TREE;
}

/* Helper function of gimplify_oacc_declare.  The helper's purpose is to,
   if required, translate 'kind' in CLAUSE into an 'entry' kind and 'exit'
   kind.  The entry kind will replace the one in CLAUSE, while the exit
   kind will be used in a new omp_clause and returned to the caller.  */

static tree
gimplify_oacc_declare_1 (tree clause)
{
  HOST_WIDE_INT kind, new_op;
  bool ret = false;
  tree c = NULL;

  kind = OMP_CLAUSE_MAP_KIND (clause);

  switch (kind)
    {
      case GOMP_MAP_ALLOC:
      case GOMP_MAP_FORCE_ALLOC:
      case GOMP_MAP_FORCE_TO:
	new_op = GOMP_MAP_DELETE;
	ret = true;
	break;

      case GOMP_MAP_FORCE_FROM:
	OMP_CLAUSE_SET_MAP_KIND (clause, GOMP_MAP_FORCE_ALLOC);
	new_op = GOMP_MAP_FORCE_FROM;
	ret = true;
	break;

      case GOMP_MAP_FORCE_TOFROM:
	OMP_CLAUSE_SET_MAP_KIND (clause, GOMP_MAP_FORCE_TO);
	new_op = GOMP_MAP_FORCE_FROM;
	ret = true;
	break;

      case GOMP_MAP_FROM:
	OMP_CLAUSE_SET_MAP_KIND (clause, GOMP_MAP_FORCE_ALLOC);
	new_op = GOMP_MAP_FROM;
	ret = true;
	break;

      case GOMP_MAP_TOFROM:
	OMP_CLAUSE_SET_MAP_KIND (clause, GOMP_MAP_TO);
	new_op = GOMP_MAP_FROM;
	ret = true;
	break;

      case GOMP_MAP_DEVICE_RESIDENT:
      case GOMP_MAP_FORCE_DEVICEPTR:
      case GOMP_MAP_FORCE_PRESENT:
      case GOMP_MAP_LINK:
      case GOMP_MAP_POINTER:
      case GOMP_MAP_TO:
	break;

      default:
	gcc_unreachable ();
	break;
    }

  if (ret)
    {
      c = build_omp_clause (OMP_CLAUSE_LOCATION (clause), OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c, new_op);
      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clause);
    }

  return c;
}

/* Gimplify OACC_DECLARE.  */

static void
gimplify_oacc_declare (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p;
  gomp_target *stmt;
  tree clauses, t, decl;

  clauses = OACC_DECLARE_CLAUSES (expr);

  gimplify_scan_omp_clauses (&clauses, pre_p, ORT_TARGET_DATA, OACC_DECLARE);
  gimplify_adjust_omp_clauses (pre_p, NULL, &clauses, OACC_DECLARE);

  for (t = clauses; t; t = OMP_CLAUSE_CHAIN (t))
    {
      decl = OMP_CLAUSE_DECL (t);

      if (TREE_CODE (decl) == MEM_REF)
	decl = TREE_OPERAND (decl, 0);

      if (VAR_P (decl) && !is_oacc_declared (decl))
	{
	  tree attr = get_identifier ("oacc declare target");
	  DECL_ATTRIBUTES (decl) = tree_cons (attr, NULL_TREE,
					      DECL_ATTRIBUTES (decl));
	}

      if (VAR_P (decl)
	  && !is_global_var (decl)
	  && DECL_CONTEXT (decl) == current_function_decl)
	{
	  tree c = gimplify_oacc_declare_1 (t);
	  if (c)
	    {
	      if (oacc_declare_returns == NULL)
		oacc_declare_returns = new hash_map<tree, tree>;

	      oacc_declare_returns->put (decl, c);
	    }
	}

      if (gimplify_omp_ctxp)
	omp_add_variable (gimplify_omp_ctxp, decl, GOVD_SEEN);
    }

  stmt = gimple_build_omp_target (NULL, GF_OMP_TARGET_KIND_OACC_DECLARE,
				  clauses);

  gimplify_seq_add_stmt (pre_p, stmt);

  *expr_p = NULL_TREE;
}

/* Gimplify the contents of an OMP_PARALLEL statement.  This involves
   gimplification of the body, as well as scanning the body for used
   variables.  We need to do this scan now, because variable-sized
   decls will be decomposed during gimplification.  */

static void
gimplify_omp_parallel (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p;
  gimple *g;
  gimple_seq body = NULL;

  gimplify_scan_omp_clauses (&OMP_PARALLEL_CLAUSES (expr), pre_p,
			     OMP_PARALLEL_COMBINED (expr)
			     ? ORT_COMBINED_PARALLEL
			     : ORT_PARALLEL, OMP_PARALLEL);

  push_gimplify_context ();

  g = gimplify_and_return_first (OMP_PARALLEL_BODY (expr), &body);
  if (gimple_code (g) == GIMPLE_BIND)
    pop_gimplify_context (g);
  else
    pop_gimplify_context (NULL);

  gimplify_adjust_omp_clauses (pre_p, body, &OMP_PARALLEL_CLAUSES (expr),
			       OMP_PARALLEL);

  g = gimple_build_omp_parallel (body,
				 OMP_PARALLEL_CLAUSES (expr),
				 NULL_TREE, NULL_TREE);
  if (OMP_PARALLEL_COMBINED (expr))
    gimple_omp_set_subcode (g, GF_OMP_PARALLEL_COMBINED);
  gimplify_seq_add_stmt (pre_p, g);
  *expr_p = NULL_TREE;
}

/* Gimplify the contents of an OMP_TASK statement.  This involves
   gimplification of the body, as well as scanning the body for used
   variables.  We need to do this scan now, because variable-sized
   decls will be decomposed during gimplification.  */

static void
gimplify_omp_task (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p;
  gimple *g;
  gimple_seq body = NULL;

  gimplify_scan_omp_clauses (&OMP_TASK_CLAUSES (expr), pre_p,
			     omp_find_clause (OMP_TASK_CLAUSES (expr),
					      OMP_CLAUSE_UNTIED)
			     ? ORT_UNTIED_TASK : ORT_TASK, OMP_TASK);

  push_gimplify_context ();

  g = gimplify_and_return_first (OMP_TASK_BODY (expr), &body);
  if (gimple_code (g) == GIMPLE_BIND)
    pop_gimplify_context (g);
  else
    pop_gimplify_context (NULL);

  gimplify_adjust_omp_clauses (pre_p, body, &OMP_TASK_CLAUSES (expr),
			       OMP_TASK);

  g = gimple_build_omp_task (body,
			     OMP_TASK_CLAUSES (expr),
			     NULL_TREE, NULL_TREE,
			     NULL_TREE, NULL_TREE, NULL_TREE);
  gimplify_seq_add_stmt (pre_p, g);
  *expr_p = NULL_TREE;
}

/* Helper function of gimplify_omp_for, find OMP_FOR resp. OMP_SIMD
   with non-NULL OMP_FOR_INIT.  */

static tree
find_combined_omp_for (tree *tp, int *walk_subtrees, void *)
{
  *walk_subtrees = 0;
  switch (TREE_CODE (*tp))
    {
    case OMP_FOR:
      *walk_subtrees = 1;
      /* FALLTHRU */
    case OMP_SIMD:
      if (OMP_FOR_INIT (*tp) != NULL_TREE)
	return *tp;
      break;
    case BIND_EXPR:
    case STATEMENT_LIST:
    case OMP_PARALLEL:
      *walk_subtrees = 1;
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Gimplify the gross structure of an OMP_FOR statement.  */

static enum gimplify_status
gimplify_omp_for (tree *expr_p, gimple_seq *pre_p)
{
  tree for_stmt, orig_for_stmt, inner_for_stmt = NULL_TREE, decl, var, t;
  enum gimplify_status ret = GS_ALL_DONE;
  enum gimplify_status tret;
  gomp_for *gfor;
  gimple_seq for_body, for_pre_body;
  int i;
  bitmap has_decl_expr = NULL;
  enum omp_region_type ort = ORT_WORKSHARE;

  orig_for_stmt = for_stmt = *expr_p;

  switch (TREE_CODE (for_stmt))
    {
    case OMP_FOR:
    case OMP_DISTRIBUTE:
      break;
    case OACC_LOOP:
      ort = ORT_ACC;
      break;
    case OMP_TASKLOOP:
      if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_UNTIED))
	ort = ORT_UNTIED_TASK;
      else
	ort = ORT_TASK;
      break;
    case OMP_SIMD:
      ort = ORT_SIMD;
      break;
    default:
      gcc_unreachable ();
    }

  /* Set OMP_CLAUSE_LINEAR_NO_COPYIN flag on explicit linear
     clause for the IV.  */
  if (ort == ORT_SIMD && TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)) == 1)
    {
      t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), 0);
      gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
      decl = TREE_OPERAND (t, 0);
      for (tree c = OMP_FOR_CLAUSES (for_stmt); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	    && OMP_CLAUSE_DECL (c) == decl)
	  {
	    OMP_CLAUSE_LINEAR_NO_COPYIN (c) = 1;
	    break;
	  }
    }

  if (OMP_FOR_INIT (for_stmt) == NULL_TREE)
    {
      gcc_assert (TREE_CODE (for_stmt) != OACC_LOOP);
      inner_for_stmt = walk_tree (&OMP_FOR_BODY (for_stmt),
				  find_combined_omp_for, NULL, NULL);
      if (inner_for_stmt == NULL_TREE)
	{
	  gcc_assert (seen_error ());
	  *expr_p = NULL_TREE;
	  return GS_ERROR;
	}
    }

  if (TREE_CODE (for_stmt) != OMP_TASKLOOP)
    gimplify_scan_omp_clauses (&OMP_FOR_CLAUSES (for_stmt), pre_p, ort,
			       TREE_CODE (for_stmt));

  if (TREE_CODE (for_stmt) == OMP_DISTRIBUTE)
    gimplify_omp_ctxp->distribute = true;

  /* Handle OMP_FOR_INIT.  */
  for_pre_body = NULL;
  if (ort == ORT_SIMD && OMP_FOR_PRE_BODY (for_stmt))
    {
      has_decl_expr = BITMAP_ALLOC (NULL);
      if (TREE_CODE (OMP_FOR_PRE_BODY (for_stmt)) == DECL_EXPR
	  && TREE_CODE (DECL_EXPR_DECL (OMP_FOR_PRE_BODY (for_stmt)))
	     == VAR_DECL)
	{
	  t = OMP_FOR_PRE_BODY (for_stmt);
	  bitmap_set_bit (has_decl_expr, DECL_UID (DECL_EXPR_DECL (t)));
	}
      else if (TREE_CODE (OMP_FOR_PRE_BODY (for_stmt)) == STATEMENT_LIST)
	{
	  tree_stmt_iterator si;
	  for (si = tsi_start (OMP_FOR_PRE_BODY (for_stmt)); !tsi_end_p (si);
	       tsi_next (&si))
	    {
	      t = tsi_stmt (si);
	      if (TREE_CODE (t) == DECL_EXPR
		  && TREE_CODE (DECL_EXPR_DECL (t)) == VAR_DECL)
		bitmap_set_bit (has_decl_expr, DECL_UID (DECL_EXPR_DECL (t)));
	    }
	}
    }
  if (OMP_FOR_PRE_BODY (for_stmt))
    {
      if (TREE_CODE (for_stmt) != OMP_TASKLOOP || gimplify_omp_ctxp)
	gimplify_and_add (OMP_FOR_PRE_BODY (for_stmt), &for_pre_body);
      else
	{
	  struct gimplify_omp_ctx ctx;
	  memset (&ctx, 0, sizeof (ctx));
	  ctx.region_type = ORT_NONE;
	  gimplify_omp_ctxp = &ctx;
	  gimplify_and_add (OMP_FOR_PRE_BODY (for_stmt), &for_pre_body);
	  gimplify_omp_ctxp = NULL;
	}
    }
  OMP_FOR_PRE_BODY (for_stmt) = NULL_TREE;

  if (OMP_FOR_INIT (for_stmt) == NULL_TREE)
    for_stmt = inner_for_stmt;

  /* For taskloop, need to gimplify the start, end and step before the
     taskloop, outside of the taskloop omp context.  */
  if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP)
    {
      for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
	{
	  t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
	  if (!is_gimple_constant (TREE_OPERAND (t, 1)))
	    {
	      TREE_OPERAND (t, 1)
		= get_initialized_tmp_var (TREE_OPERAND (t, 1),
					   pre_p, NULL, false);
	      tree c = build_omp_clause (input_location,
					 OMP_CLAUSE_FIRSTPRIVATE);
	      OMP_CLAUSE_DECL (c) = TREE_OPERAND (t, 1);
	      OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (orig_for_stmt);
	      OMP_FOR_CLAUSES (orig_for_stmt) = c;
	    }

	  /* Handle OMP_FOR_COND.  */
	  t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
	  if (!is_gimple_constant (TREE_OPERAND (t, 1)))
	    {
	      TREE_OPERAND (t, 1)
		= get_initialized_tmp_var (TREE_OPERAND (t, 1),
					   gimple_seq_empty_p (for_pre_body)
					   ? pre_p : &for_pre_body, NULL,
					   false);
	      tree c = build_omp_clause (input_location,
					 OMP_CLAUSE_FIRSTPRIVATE);
	      OMP_CLAUSE_DECL (c) = TREE_OPERAND (t, 1);
	      OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (orig_for_stmt);
	      OMP_FOR_CLAUSES (orig_for_stmt) = c;
	    }

	  /* Handle OMP_FOR_INCR.  */
	  t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
	  if (TREE_CODE (t) == MODIFY_EXPR)
	    {
	      decl = TREE_OPERAND (t, 0);
	      t = TREE_OPERAND (t, 1);
	      tree *tp = &TREE_OPERAND (t, 1);
	      if (TREE_CODE (t) == PLUS_EXPR && *tp == decl)
		tp = &TREE_OPERAND (t, 0);

	      if (!is_gimple_constant (*tp))
		{
		  gimple_seq *seq = gimple_seq_empty_p (for_pre_body)
				    ? pre_p : &for_pre_body;
		  *tp = get_initialized_tmp_var (*tp, seq, NULL, false);
		  tree c = build_omp_clause (input_location,
					     OMP_CLAUSE_FIRSTPRIVATE);
		  OMP_CLAUSE_DECL (c) = *tp;
		  OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (orig_for_stmt);
		  OMP_FOR_CLAUSES (orig_for_stmt) = c;
		}
	    }
	}

      gimplify_scan_omp_clauses (&OMP_FOR_CLAUSES (orig_for_stmt), pre_p, ort,
				 OMP_TASKLOOP);
    }

  if (orig_for_stmt != for_stmt)
    gimplify_omp_ctxp->combined_loop = true;

  for_body = NULL;
  gcc_assert (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt))
	      == TREE_VEC_LENGTH (OMP_FOR_COND (for_stmt)));
  gcc_assert (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt))
	      == TREE_VEC_LENGTH (OMP_FOR_INCR (for_stmt)));

  tree c = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_ORDERED);
  bool is_doacross = false;
  if (c && OMP_CLAUSE_ORDERED_EXPR (c))
    {
      is_doacross = true;
      gimplify_omp_ctxp->loop_iter_var.create (TREE_VEC_LENGTH
						 (OMP_FOR_INIT (for_stmt))
					       * 2);
    }
  int collapse = 1, tile = 0;
  c = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_COLLAPSE);
  if (c)
    collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (c));
  c = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_TILE);
  if (c)
    tile = list_length (OMP_CLAUSE_TILE_LIST (c));
  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
    {
      t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
      gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
      decl = TREE_OPERAND (t, 0);
      gcc_assert (DECL_P (decl));
      gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (decl))
		  || POINTER_TYPE_P (TREE_TYPE (decl)));
      if (is_doacross)
	{
	  if (TREE_CODE (for_stmt) == OMP_FOR && OMP_FOR_ORIG_DECLS (for_stmt))
	    gimplify_omp_ctxp->loop_iter_var.quick_push
	      (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i));
	  else
	    gimplify_omp_ctxp->loop_iter_var.quick_push (decl);
	  gimplify_omp_ctxp->loop_iter_var.quick_push (decl);
	}

      /* Make sure the iteration variable is private.  */
      tree c = NULL_TREE;
      tree c2 = NULL_TREE;
      if (orig_for_stmt != for_stmt)
	/* Do this only on innermost construct for combined ones.  */;
      else if (ort == ORT_SIMD)
	{
	  splay_tree_node n = splay_tree_lookup (gimplify_omp_ctxp->variables,
						 (splay_tree_key) decl);
	  omp_is_private (gimplify_omp_ctxp, decl,
			  1 + (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt))
			       != 1));
	  if (n != NULL && (n->value & GOVD_DATA_SHARE_CLASS) != 0)
	    omp_notice_variable (gimplify_omp_ctxp, decl, true);
	  else if (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)) == 1)
	    {
	      c = build_omp_clause (input_location, OMP_CLAUSE_LINEAR);
	      OMP_CLAUSE_LINEAR_NO_COPYIN (c) = 1;
	      unsigned int flags = GOVD_LINEAR | GOVD_EXPLICIT | GOVD_SEEN;
	      if (has_decl_expr
		  && bitmap_bit_p (has_decl_expr, DECL_UID (decl)))
		{
		  OMP_CLAUSE_LINEAR_NO_COPYOUT (c) = 1;
		  flags |= GOVD_LINEAR_LASTPRIVATE_NO_OUTER;
		}
	      struct gimplify_omp_ctx *outer
		= gimplify_omp_ctxp->outer_context;
	      if (outer && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		{
		  if (outer->region_type == ORT_WORKSHARE
		      && outer->combined_loop)
		    {
		      n = splay_tree_lookup (outer->variables,
					     (splay_tree_key)decl);
		      if (n != NULL && (n->value & GOVD_LOCAL) != 0)
			{
			  OMP_CLAUSE_LINEAR_NO_COPYOUT (c) = 1;
			  flags |= GOVD_LINEAR_LASTPRIVATE_NO_OUTER;
			}
		      else
			{
			  struct gimplify_omp_ctx *octx = outer->outer_context;
			  if (octx
			      && octx->region_type == ORT_COMBINED_PARALLEL
			      && octx->outer_context
			      && (octx->outer_context->region_type
				  == ORT_WORKSHARE)
			      && octx->outer_context->combined_loop)
			    {
			      octx = octx->outer_context;
			      n = splay_tree_lookup (octx->variables,
						     (splay_tree_key)decl);
			      if (n != NULL && (n->value & GOVD_LOCAL) != 0)
				{
				  OMP_CLAUSE_LINEAR_NO_COPYOUT (c) = 1;
				  flags |= GOVD_LINEAR_LASTPRIVATE_NO_OUTER;
				}
			    }
			}
		    }
		}

	      OMP_CLAUSE_DECL (c) = decl;
	      OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (for_stmt);
	      OMP_FOR_CLAUSES (for_stmt) = c;
	      omp_add_variable (gimplify_omp_ctxp, decl, flags);
	      if (outer && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		{
		  if (outer->region_type == ORT_WORKSHARE
		      && outer->combined_loop)
		    {
		      if (outer->outer_context
			  && (outer->outer_context->region_type
			      == ORT_COMBINED_PARALLEL))
			outer = outer->outer_context;
		      else if (omp_check_private (outer, decl, false))
			outer = NULL;
		    }
		  else if (((outer->region_type & ORT_TASK) != 0)
			   && outer->combined_loop
			   && !omp_check_private (gimplify_omp_ctxp,
						  decl, false))
		    ;
		  else if (outer->region_type != ORT_COMBINED_PARALLEL)
		    {
		      omp_notice_variable (outer, decl, true);
		      outer = NULL;
		    }
		  if (outer)
		    {
		      n = splay_tree_lookup (outer->variables,
					     (splay_tree_key)decl);
		      if (n == NULL || (n->value & GOVD_DATA_SHARE_CLASS) == 0)
			{
			  omp_add_variable (outer, decl,
					    GOVD_LASTPRIVATE | GOVD_SEEN);
			  if (outer->region_type == ORT_COMBINED_PARALLEL
			      && outer->outer_context
			      && (outer->outer_context->region_type
				  == ORT_WORKSHARE)
			      && outer->outer_context->combined_loop)
			    {
			      outer = outer->outer_context;
			      n = splay_tree_lookup (outer->variables,
						     (splay_tree_key)decl);
			      if (omp_check_private (outer, decl, false))
				outer = NULL;
			      else if (n == NULL
				       || ((n->value & GOVD_DATA_SHARE_CLASS)
					   == 0))
				omp_add_variable (outer, decl,
						  GOVD_LASTPRIVATE
						  | GOVD_SEEN);
			      else
				outer = NULL;
			    }
			  if (outer && outer->outer_context
			      && (outer->outer_context->region_type
				  == ORT_COMBINED_TEAMS))
			    {
			      outer = outer->outer_context;
			      n = splay_tree_lookup (outer->variables,
						     (splay_tree_key)decl);
			      if (n == NULL
				  || (n->value & GOVD_DATA_SHARE_CLASS) == 0)
				omp_add_variable (outer, decl,
						  GOVD_SHARED | GOVD_SEEN);
			      else
				outer = NULL;
			    }
			  if (outer && outer->outer_context)
			    omp_notice_variable (outer->outer_context, decl,
						 true);
			}
		    }
		}
	    }
	  else
	    {
	      bool lastprivate
		= (!has_decl_expr
		   || !bitmap_bit_p (has_decl_expr, DECL_UID (decl)));
	      struct gimplify_omp_ctx *outer
		= gimplify_omp_ctxp->outer_context;
	      if (outer && lastprivate)
		{
		  if (outer->region_type == ORT_WORKSHARE
		      && outer->combined_loop)
		    {
		      n = splay_tree_lookup (outer->variables,
					     (splay_tree_key)decl);
		      if (n != NULL && (n->value & GOVD_LOCAL) != 0)
			{
			  lastprivate = false;
			  outer = NULL;
			}
		      else if (outer->outer_context
			       && (outer->outer_context->region_type
				   == ORT_COMBINED_PARALLEL))
			outer = outer->outer_context;
		      else if (omp_check_private (outer, decl, false))
			outer = NULL;
		    }
		  else if (((outer->region_type & ORT_TASK) != 0)
			   && outer->combined_loop
			   && !omp_check_private (gimplify_omp_ctxp,
						  decl, false))
		    ;
		  else if (outer->region_type != ORT_COMBINED_PARALLEL)
		    {
		      omp_notice_variable (outer, decl, true);
		      outer = NULL;
		    }
		  if (outer)
		    {
		      n = splay_tree_lookup (outer->variables,
					     (splay_tree_key)decl);
		      if (n == NULL || (n->value & GOVD_DATA_SHARE_CLASS) == 0)
			{
			  omp_add_variable (outer, decl,
					    GOVD_LASTPRIVATE | GOVD_SEEN);
			  if (outer->region_type == ORT_COMBINED_PARALLEL
			      && outer->outer_context
			      && (outer->outer_context->region_type
				  == ORT_WORKSHARE)
			      && outer->outer_context->combined_loop)
			    {
			      outer = outer->outer_context;
			      n = splay_tree_lookup (outer->variables,
						     (splay_tree_key)decl);
			      if (omp_check_private (outer, decl, false))
				outer = NULL;
			      else if (n == NULL
				       || ((n->value & GOVD_DATA_SHARE_CLASS)
					   == 0))
				omp_add_variable (outer, decl,
						  GOVD_LASTPRIVATE
						  | GOVD_SEEN);
			      else
				outer = NULL;
			    }
			  if (outer && outer->outer_context
			      && (outer->outer_context->region_type
				  == ORT_COMBINED_TEAMS))
			    {
			      outer = outer->outer_context;
			      n = splay_tree_lookup (outer->variables,
						     (splay_tree_key)decl);
			      if (n == NULL
				  || (n->value & GOVD_DATA_SHARE_CLASS) == 0)
				omp_add_variable (outer, decl,
						  GOVD_SHARED | GOVD_SEEN);
			      else
				outer = NULL;
			    }
			  if (outer && outer->outer_context)
			    omp_notice_variable (outer->outer_context, decl,
						 true);
			}
		    }
		}

	      c = build_omp_clause (input_location,
				    lastprivate ? OMP_CLAUSE_LASTPRIVATE
						: OMP_CLAUSE_PRIVATE);
	      OMP_CLAUSE_DECL (c) = decl;
	      OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (for_stmt);
	      OMP_FOR_CLAUSES (for_stmt) = c;
	      omp_add_variable (gimplify_omp_ctxp, decl,
				(lastprivate ? GOVD_LASTPRIVATE : GOVD_PRIVATE)
				| GOVD_EXPLICIT | GOVD_SEEN);
	      c = NULL_TREE;
	    }
	}
      else if (omp_is_private (gimplify_omp_ctxp, decl, 0))
	omp_notice_variable (gimplify_omp_ctxp, decl, true);
      else
	omp_add_variable (gimplify_omp_ctxp, decl, GOVD_PRIVATE | GOVD_SEEN);

      /* If DECL is not a gimple register, create a temporary variable to act
	 as an iteration counter.  This is valid, since DECL cannot be
	 modified in the body of the loop.  Similarly for any iteration vars
	 in simd with collapse > 1 where the iterator vars must be
	 lastprivate.  */
      if (orig_for_stmt != for_stmt)
	var = decl;
      else if (!is_gimple_reg (decl)
	       || (ort == ORT_SIMD
		   && TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)) > 1))
	{
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
	  /* Make sure omp_add_variable is not called on it prematurely.
	     We call it ourselves a few lines later.  */
	  gimplify_omp_ctxp = NULL;
	  var = create_tmp_var (TREE_TYPE (decl), get_name (decl));
	  gimplify_omp_ctxp = ctx;
	  TREE_OPERAND (t, 0) = var;

	  gimplify_seq_add_stmt (&for_body, gimple_build_assign (decl, var));

	  if (ort == ORT_SIMD
	      && TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)) == 1)
	    {
	      c2 = build_omp_clause (input_location, OMP_CLAUSE_LINEAR);
	      OMP_CLAUSE_LINEAR_NO_COPYIN (c2) = 1;
	      OMP_CLAUSE_LINEAR_NO_COPYOUT (c2) = 1;
	      OMP_CLAUSE_DECL (c2) = var;
	      OMP_CLAUSE_CHAIN (c2) = OMP_FOR_CLAUSES (for_stmt);
	      OMP_FOR_CLAUSES (for_stmt) = c2;
	      omp_add_variable (gimplify_omp_ctxp, var,
				GOVD_LINEAR | GOVD_EXPLICIT | GOVD_SEEN);
	      if (c == NULL_TREE)
		{
		  c = c2;
		  c2 = NULL_TREE;
		}
	    }
	  else
	    omp_add_variable (gimplify_omp_ctxp, var,
			      GOVD_PRIVATE | GOVD_SEEN);
	}
      else
	var = decl;

      tret = gimplify_expr (&TREE_OPERAND (t, 1), &for_pre_body, NULL,
			    is_gimple_val, fb_rvalue, false);
      ret = MIN (ret, tret);
      if (ret == GS_ERROR)
	return ret;

      /* Handle OMP_FOR_COND.  */
      t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
      gcc_assert (COMPARISON_CLASS_P (t));
      gcc_assert (TREE_OPERAND (t, 0) == decl);

      tret = gimplify_expr (&TREE_OPERAND (t, 1), &for_pre_body, NULL,
			    is_gimple_val, fb_rvalue, false);
      ret = MIN (ret, tret);

      /* Handle OMP_FOR_INCR.  */
      t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
      switch (TREE_CODE (t))
	{
	case PREINCREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  {
	    tree decl = TREE_OPERAND (t, 0);
	    /* c_omp_for_incr_canonicalize_ptr() should have been
	       called to massage things appropriately.  */
	    gcc_assert (!POINTER_TYPE_P (TREE_TYPE (decl)));

	    if (orig_for_stmt != for_stmt)
	      break;
	    t = build_int_cst (TREE_TYPE (decl), 1);
	    if (c)
	      OMP_CLAUSE_LINEAR_STEP (c) = t;
	    t = build2 (PLUS_EXPR, TREE_TYPE (decl), var, t);
	    t = build2 (MODIFY_EXPR, TREE_TYPE (var), var, t);
	    TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i) = t;
	    break;
	  }

	case PREDECREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	  /* c_omp_for_incr_canonicalize_ptr() should have been
	     called to massage things appropriately.  */
	  gcc_assert (!POINTER_TYPE_P (TREE_TYPE (decl)));
	  if (orig_for_stmt != for_stmt)
	    break;
	  t = build_int_cst (TREE_TYPE (decl), -1);
	  if (c)
	    OMP_CLAUSE_LINEAR_STEP (c) = t;
	  t = build2 (PLUS_EXPR, TREE_TYPE (decl), var, t);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (var), var, t);
	  TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i) = t;
	  break;

	case MODIFY_EXPR:
	  gcc_assert (TREE_OPERAND (t, 0) == decl);
	  TREE_OPERAND (t, 0) = var;

	  t = TREE_OPERAND (t, 1);
	  switch (TREE_CODE (t))
	    {
	    case PLUS_EXPR:
	      if (TREE_OPERAND (t, 1) == decl)
		{
		  TREE_OPERAND (t, 1) = TREE_OPERAND (t, 0);
		  TREE_OPERAND (t, 0) = var;
		  break;
		}

	      /* Fallthru.  */
	    case MINUS_EXPR:
	    case POINTER_PLUS_EXPR:
	      gcc_assert (TREE_OPERAND (t, 0) == decl);
	      TREE_OPERAND (t, 0) = var;
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  tret = gimplify_expr (&TREE_OPERAND (t, 1), &for_pre_body, NULL,
				is_gimple_val, fb_rvalue, false);
	  ret = MIN (ret, tret);
	  if (c)
	    {
	      tree step = TREE_OPERAND (t, 1);
	      tree stept = TREE_TYPE (decl);
	      if (POINTER_TYPE_P (stept))
		stept = sizetype;
	      step = fold_convert (stept, step);
	      if (TREE_CODE (t) == MINUS_EXPR)
		step = fold_build1 (NEGATE_EXPR, stept, step);
	      OMP_CLAUSE_LINEAR_STEP (c) = step;
	      if (step != TREE_OPERAND (t, 1))
		{
		  tret = gimplify_expr (&OMP_CLAUSE_LINEAR_STEP (c),
					&for_pre_body, NULL,
					is_gimple_val, fb_rvalue, false);
		  ret = MIN (ret, tret);
		}
	    }
	  break;

	default:
	  gcc_unreachable ();
	}

      if (c2)
	{
	  gcc_assert (c);
	  OMP_CLAUSE_LINEAR_STEP (c2) = OMP_CLAUSE_LINEAR_STEP (c);
	}

      if ((var != decl || collapse > 1 || tile) && orig_for_stmt == for_stmt)
	{
	  for (c = OMP_FOR_CLAUSES (for_stmt); c ; c = OMP_CLAUSE_CHAIN (c))
	    if (((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		  && OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c) == NULL)
		 || (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		     && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c)
		     && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c) == NULL))
		&& OMP_CLAUSE_DECL (c) == decl)
	      {
		if (is_doacross && (collapse == 1 || i >= collapse))
		  t = var;
		else
		  {
		    t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
		    gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
		    gcc_assert (TREE_OPERAND (t, 0) == var);
		    t = TREE_OPERAND (t, 1);
		    gcc_assert (TREE_CODE (t) == PLUS_EXPR
				|| TREE_CODE (t) == MINUS_EXPR
				|| TREE_CODE (t) == POINTER_PLUS_EXPR);
		    gcc_assert (TREE_OPERAND (t, 0) == var);
		    t = build2 (TREE_CODE (t), TREE_TYPE (decl),
				is_doacross ? var : decl,
				TREE_OPERAND (t, 1));
		  }
		gimple_seq *seq;
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
		  seq = &OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c);
		else
		  seq = &OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c);
		gimplify_assign (decl, t, seq);
	    }
	}
    }

  BITMAP_FREE (has_decl_expr);

  if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP)
    {
      push_gimplify_context ();
      if (TREE_CODE (OMP_FOR_BODY (orig_for_stmt)) != BIND_EXPR)
	{
	  OMP_FOR_BODY (orig_for_stmt)
	    = build3 (BIND_EXPR, void_type_node, NULL,
		      OMP_FOR_BODY (orig_for_stmt), NULL);
	  TREE_SIDE_EFFECTS (OMP_FOR_BODY (orig_for_stmt)) = 1;
	}
    }

  gimple *g = gimplify_and_return_first (OMP_FOR_BODY (orig_for_stmt),
					 &for_body);

  if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP)
    {
      if (gimple_code (g) == GIMPLE_BIND)
	pop_gimplify_context (g);
      else
	pop_gimplify_context (NULL);
    }

  if (orig_for_stmt != for_stmt)
    for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
      {
	t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
	decl = TREE_OPERAND (t, 0);
	struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
	if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP)
	  gimplify_omp_ctxp = ctx->outer_context;
	var = create_tmp_var (TREE_TYPE (decl), get_name (decl));
	gimplify_omp_ctxp = ctx;
	omp_add_variable (gimplify_omp_ctxp, var, GOVD_PRIVATE | GOVD_SEEN);
	TREE_OPERAND (t, 0) = var;
	t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
	TREE_OPERAND (t, 1) = copy_node (TREE_OPERAND (t, 1));
	TREE_OPERAND (TREE_OPERAND (t, 1), 0) = var;
      }

  gimplify_adjust_omp_clauses (pre_p, for_body,
			       &OMP_FOR_CLAUSES (orig_for_stmt),
			       TREE_CODE (orig_for_stmt));

  int kind;
  switch (TREE_CODE (orig_for_stmt))
    {
    case OMP_FOR: kind = GF_OMP_FOR_KIND_FOR; break;
    case OMP_SIMD: kind = GF_OMP_FOR_KIND_SIMD; break;
    case OMP_DISTRIBUTE: kind = GF_OMP_FOR_KIND_DISTRIBUTE; break;
    case OMP_TASKLOOP: kind = GF_OMP_FOR_KIND_TASKLOOP; break;
    case OACC_LOOP: kind = GF_OMP_FOR_KIND_OACC_LOOP; break;
    default:
      gcc_unreachable ();
    }
  gfor = gimple_build_omp_for (for_body, kind, OMP_FOR_CLAUSES (orig_for_stmt),
			       TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)),
			       for_pre_body);
  if (orig_for_stmt != for_stmt)
    gimple_omp_for_set_combined_p (gfor, true);
  if (gimplify_omp_ctxp
      && (gimplify_omp_ctxp->combined_loop
	  || (gimplify_omp_ctxp->region_type == ORT_COMBINED_PARALLEL
	      && gimplify_omp_ctxp->outer_context
	      && gimplify_omp_ctxp->outer_context->combined_loop)))
    {
      gimple_omp_for_set_combined_into_p (gfor, true);
      if (gimplify_omp_ctxp->combined_loop)
	gcc_assert (TREE_CODE (orig_for_stmt) == OMP_SIMD);
      else
	gcc_assert (TREE_CODE (orig_for_stmt) == OMP_FOR);
    }

  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
    {
      t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
      gimple_omp_for_set_index (gfor, i, TREE_OPERAND (t, 0));
      gimple_omp_for_set_initial (gfor, i, TREE_OPERAND (t, 1));
      t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
      gimple_omp_for_set_cond (gfor, i, TREE_CODE (t));
      gimple_omp_for_set_final (gfor, i, TREE_OPERAND (t, 1));
      t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
      gimple_omp_for_set_incr (gfor, i, TREE_OPERAND (t, 1));
    }

  /* OMP_TASKLOOP is gimplified as two GIMPLE_OMP_FOR taskloop
     constructs with GIMPLE_OMP_TASK sandwiched in between them.
     The outer taskloop stands for computing the number of iterations,
     counts for collapsed loops and holding taskloop specific clauses.
     The task construct stands for the effect of data sharing on the
     explicit task it creates and the inner taskloop stands for expansion
     of the static loop inside of the explicit task construct.  */
  if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP)
    {
      tree *gfor_clauses_ptr = gimple_omp_for_clauses_ptr (gfor);
      tree task_clauses = NULL_TREE;
      tree c = *gfor_clauses_ptr;
      tree *gtask_clauses_ptr = &task_clauses;
      tree outer_for_clauses = NULL_TREE;
      tree *gforo_clauses_ptr = &outer_for_clauses;
      for (; c; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	  /* These clauses are allowed on task, move them there.  */
	  case OMP_CLAUSE_SHARED:
	  case OMP_CLAUSE_FIRSTPRIVATE:
	  case OMP_CLAUSE_DEFAULT:
	  case OMP_CLAUSE_IF:
	  case OMP_CLAUSE_UNTIED:
	  case OMP_CLAUSE_FINAL:
	  case OMP_CLAUSE_MERGEABLE:
	  case OMP_CLAUSE_PRIORITY:
	    *gtask_clauses_ptr = c;
	    gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	    break;
	  case OMP_CLAUSE_PRIVATE:
	    if (OMP_CLAUSE_PRIVATE_TASKLOOP_IV (c))
	      {
		/* We want private on outer for and firstprivate
		   on task.  */
		*gtask_clauses_ptr
		  = build_omp_clause (OMP_CLAUSE_LOCATION (c),
				      OMP_CLAUSE_FIRSTPRIVATE);
		OMP_CLAUSE_DECL (*gtask_clauses_ptr) = OMP_CLAUSE_DECL (c);
		lang_hooks.decls.omp_finish_clause (*gtask_clauses_ptr, NULL);
		gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (*gtask_clauses_ptr);
		*gforo_clauses_ptr = c;
		gforo_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	      }
	    else
	      {
		*gtask_clauses_ptr = c;
		gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	      }
	    break;
	  /* These clauses go into outer taskloop clauses.  */
	  case OMP_CLAUSE_GRAINSIZE:
	  case OMP_CLAUSE_NUM_TASKS:
	  case OMP_CLAUSE_NOGROUP:
	    *gforo_clauses_ptr = c;
	    gforo_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	    break;
	  /* Taskloop clause we duplicate on both taskloops.  */
	  case OMP_CLAUSE_COLLAPSE:
	    *gfor_clauses_ptr = c;
	    gfor_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	    *gforo_clauses_ptr = copy_node (c);
	    gforo_clauses_ptr = &OMP_CLAUSE_CHAIN (*gforo_clauses_ptr);
	    break;
	  /* For lastprivate, keep the clause on inner taskloop, and add
	     a shared clause on task.  If the same decl is also firstprivate,
	     add also firstprivate clause on the inner taskloop.  */
	  case OMP_CLAUSE_LASTPRIVATE:
	    if (OMP_CLAUSE_LASTPRIVATE_TASKLOOP_IV (c))
	      {
		/* For taskloop C++ lastprivate IVs, we want:
		   1) private on outer taskloop
		   2) firstprivate and shared on task
		   3) lastprivate on inner taskloop  */
		*gtask_clauses_ptr
		  = build_omp_clause (OMP_CLAUSE_LOCATION (c),
				      OMP_CLAUSE_FIRSTPRIVATE);
		OMP_CLAUSE_DECL (*gtask_clauses_ptr) = OMP_CLAUSE_DECL (c);
		lang_hooks.decls.omp_finish_clause (*gtask_clauses_ptr, NULL);
		gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (*gtask_clauses_ptr);
		OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c) = 1;
		*gforo_clauses_ptr = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						       OMP_CLAUSE_PRIVATE);
		OMP_CLAUSE_DECL (*gforo_clauses_ptr) = OMP_CLAUSE_DECL (c);
		OMP_CLAUSE_PRIVATE_TASKLOOP_IV (*gforo_clauses_ptr) = 1;
		TREE_TYPE (*gforo_clauses_ptr) = TREE_TYPE (c);
		gforo_clauses_ptr = &OMP_CLAUSE_CHAIN (*gforo_clauses_ptr);
	      }
	    *gfor_clauses_ptr = c;
	    gfor_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	    *gtask_clauses_ptr
	      = build_omp_clause (OMP_CLAUSE_LOCATION (c), OMP_CLAUSE_SHARED);
	    OMP_CLAUSE_DECL (*gtask_clauses_ptr) = OMP_CLAUSE_DECL (c);
	    if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
	      OMP_CLAUSE_SHARED_FIRSTPRIVATE (*gtask_clauses_ptr) = 1;
	    gtask_clauses_ptr
	      = &OMP_CLAUSE_CHAIN (*gtask_clauses_ptr);
	    break;
	  default:
	    gcc_unreachable ();
	  }
      *gfor_clauses_ptr = NULL_TREE;
      *gtask_clauses_ptr = NULL_TREE;
      *gforo_clauses_ptr = NULL_TREE;
      g = gimple_build_bind (NULL_TREE, gfor, NULL_TREE);
      g = gimple_build_omp_task (g, task_clauses, NULL_TREE, NULL_TREE,
				 NULL_TREE, NULL_TREE, NULL_TREE);
      gimple_omp_task_set_taskloop_p (g, true);
      g = gimple_build_bind (NULL_TREE, g, NULL_TREE);
      gomp_for *gforo
	= gimple_build_omp_for (g, GF_OMP_FOR_KIND_TASKLOOP, outer_for_clauses,
				gimple_omp_for_collapse (gfor),
				gimple_omp_for_pre_body (gfor));
      gimple_omp_for_set_pre_body (gfor, NULL);
      gimple_omp_for_set_combined_p (gforo, true);
      gimple_omp_for_set_combined_into_p (gfor, true);
      for (i = 0; i < (int) gimple_omp_for_collapse (gfor); i++)
	{
	  tree type = TREE_TYPE (gimple_omp_for_index (gfor, i));
	  tree v = create_tmp_var (type);
	  gimple_omp_for_set_index (gforo, i, v);
	  t = unshare_expr (gimple_omp_for_initial (gfor, i));
	  gimple_omp_for_set_initial (gforo, i, t);
	  gimple_omp_for_set_cond (gforo, i,
				   gimple_omp_for_cond (gfor, i));
	  t = unshare_expr (gimple_omp_for_final (gfor, i));
	  gimple_omp_for_set_final (gforo, i, t);
	  t = unshare_expr (gimple_omp_for_incr (gfor, i));
	  gcc_assert (TREE_OPERAND (t, 0) == gimple_omp_for_index (gfor, i));
	  TREE_OPERAND (t, 0) = v;
	  gimple_omp_for_set_incr (gforo, i, t);
	  t = build_omp_clause (input_location, OMP_CLAUSE_PRIVATE);
	  OMP_CLAUSE_DECL (t) = v;
	  OMP_CLAUSE_CHAIN (t) = gimple_omp_for_clauses (gforo);
	  gimple_omp_for_set_clauses (gforo, t);
	}
      gimplify_seq_add_stmt (pre_p, gforo);
    }
  else
    gimplify_seq_add_stmt (pre_p, gfor);
  if (ret != GS_ALL_DONE)
    return GS_ERROR;
  *expr_p = NULL_TREE;
  return GS_ALL_DONE;
}

/* Helper function of optimize_target_teams, find OMP_TEAMS inside
   of OMP_TARGET's body.  */

static tree
find_omp_teams (tree *tp, int *walk_subtrees, void *)
{
  *walk_subtrees = 0;
  switch (TREE_CODE (*tp))
    {
    case OMP_TEAMS:
      return *tp;
    case BIND_EXPR:
    case STATEMENT_LIST:
      *walk_subtrees = 1;
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Helper function of optimize_target_teams, determine if the expression
   can be computed safely before the target construct on the host.  */

static tree
computable_teams_clause (tree *tp, int *walk_subtrees, void *)
{
  splay_tree_node n;

  if (TYPE_P (*tp))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  switch (TREE_CODE (*tp))
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      *walk_subtrees = 0;
      if (error_operand_p (*tp)
	  || !INTEGRAL_TYPE_P (TREE_TYPE (*tp))
	  || DECL_HAS_VALUE_EXPR_P (*tp)
	  || DECL_THREAD_LOCAL_P (*tp)
	  || TREE_SIDE_EFFECTS (*tp)
	  || TREE_THIS_VOLATILE (*tp))
	return *tp;
      if (is_global_var (*tp)
	  && (lookup_attribute ("omp declare target", DECL_ATTRIBUTES (*tp))
	      || lookup_attribute ("omp declare target link",
				   DECL_ATTRIBUTES (*tp))))
	return *tp;
      if (VAR_P (*tp)
	  && !DECL_SEEN_IN_BIND_EXPR_P (*tp)
	  && !is_global_var (*tp)
	  && decl_function_context (*tp) == current_function_decl)
	return *tp;
      n = splay_tree_lookup (gimplify_omp_ctxp->variables,
			     (splay_tree_key) *tp);
      if (n == NULL)
	{
	  if (gimplify_omp_ctxp->target_map_scalars_firstprivate)
	    return NULL_TREE;
	  return *tp;
	}
      else if (n->value & GOVD_LOCAL)
	return *tp;
      else if (n->value & GOVD_FIRSTPRIVATE)
	return NULL_TREE;
      else if ((n->value & (GOVD_MAP | GOVD_MAP_ALWAYS_TO))
	       == (GOVD_MAP | GOVD_MAP_ALWAYS_TO))
	return NULL_TREE;
      return *tp;
    case INTEGER_CST:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (*tp)))
	return *tp;
      return NULL_TREE;
    case TARGET_EXPR:
      if (TARGET_EXPR_INITIAL (*tp)
	  || TREE_CODE (TARGET_EXPR_SLOT (*tp)) != VAR_DECL)
	return *tp;
      return computable_teams_clause (&TARGET_EXPR_SLOT (*tp),
				      walk_subtrees, NULL);
    /* Allow some reasonable subset of integral arithmetics.  */
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case NON_LVALUE_EXPR:
    CASE_CONVERT:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (*tp)))
	return *tp;
      return NULL_TREE;
    /* And disallow anything else, except for comparisons.  */
    default:
      if (COMPARISON_CLASS_P (*tp))
	return NULL_TREE;
      return *tp;
    }
}

/* Try to determine if the num_teams and/or thread_limit expressions
   can have their values determined already before entering the
   target construct.
   INTEGER_CSTs trivially are,
   integral decls that are firstprivate (explicitly or implicitly)
   or explicitly map(always, to:) or map(always, tofrom:) on the target
   region too, and expressions involving simple arithmetics on those
   too, function calls are not ok, dereferencing something neither etc.
   Add NUM_TEAMS and THREAD_LIMIT clauses to the OMP_CLAUSES of
   EXPR based on what we find:
   0 stands for clause not specified at all, use implementation default
   -1 stands for value that can't be determined easily before entering
      the target construct.
   If teams construct is not present at all, use 1 for num_teams
   and 0 for thread_limit (only one team is involved, and the thread
   limit is implementation defined.  */

static void
optimize_target_teams (tree target, gimple_seq *pre_p)
{
  tree body = OMP_BODY (target);
  tree teams = walk_tree (&body, find_omp_teams, NULL, NULL);
  tree num_teams = integer_zero_node;
  tree thread_limit = integer_zero_node;
  location_t num_teams_loc = EXPR_LOCATION (target);
  location_t thread_limit_loc = EXPR_LOCATION (target);
  tree c, *p, expr;
  struct gimplify_omp_ctx *target_ctx = gimplify_omp_ctxp;

  if (teams == NULL_TREE)
    num_teams = integer_one_node;
  else
    for (c = OMP_TEAMS_CLAUSES (teams); c; c = OMP_CLAUSE_CHAIN (c))
      {
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS)
	  {
	    p = &num_teams;
	    num_teams_loc = OMP_CLAUSE_LOCATION (c);
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_THREAD_LIMIT)
	  {
	    p = &thread_limit;
	    thread_limit_loc = OMP_CLAUSE_LOCATION (c);
	  }
	else
	  continue;
	expr = OMP_CLAUSE_OPERAND (c, 0);
	if (TREE_CODE (expr) == INTEGER_CST)
	  {
	    *p = expr;
	    continue;
	  }
	if (walk_tree (&expr, computable_teams_clause, NULL, NULL))
	  {
	    *p = integer_minus_one_node;
	    continue;
	  }
	*p = expr;
	gimplify_omp_ctxp = gimplify_omp_ctxp->outer_context;
	if (gimplify_expr (p, pre_p, NULL, is_gimple_val, fb_rvalue, false)
	    == GS_ERROR)
	  {
	    gimplify_omp_ctxp = target_ctx;
	    *p = integer_minus_one_node;
	    continue;
	  }
	gimplify_omp_ctxp = target_ctx;
	if (!DECL_P (expr) && TREE_CODE (expr) != TARGET_EXPR)
	  OMP_CLAUSE_OPERAND (c, 0) = *p;
      }
  c = build_omp_clause (thread_limit_loc, OMP_CLAUSE_THREAD_LIMIT);
  OMP_CLAUSE_THREAD_LIMIT_EXPR (c) = thread_limit;
  OMP_CLAUSE_CHAIN (c) = OMP_TARGET_CLAUSES (target);
  OMP_TARGET_CLAUSES (target) = c;
  c = build_omp_clause (num_teams_loc, OMP_CLAUSE_NUM_TEAMS);
  OMP_CLAUSE_NUM_TEAMS_EXPR (c) = num_teams;
  OMP_CLAUSE_CHAIN (c) = OMP_TARGET_CLAUSES (target);
  OMP_TARGET_CLAUSES (target) = c;
}

/* Gimplify the gross structure of several OMP constructs.  */

static void
gimplify_omp_workshare (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p;
  gimple *stmt;
  gimple_seq body = NULL;
  enum omp_region_type ort;

  switch (TREE_CODE (expr))
    {
    case OMP_SECTIONS:
    case OMP_SINGLE:
      ort = ORT_WORKSHARE;
      break;
    case OMP_TARGET:
      ort = OMP_TARGET_COMBINED (expr) ? ORT_COMBINED_TARGET : ORT_TARGET;
      break;
    case OACC_KERNELS:
      ort = ORT_ACC_KERNELS;
      break;
    case OACC_PARALLEL:
      ort = ORT_ACC_PARALLEL;
      break;
    case OACC_DATA:
      ort = ORT_ACC_DATA;
      break;
    case OMP_TARGET_DATA:
      ort = ORT_TARGET_DATA;
      break;
    case OMP_TEAMS:
      ort = OMP_TEAMS_COMBINED (expr) ? ORT_COMBINED_TEAMS : ORT_TEAMS;
      break;
    case OACC_HOST_DATA:
      ort = ORT_ACC_HOST_DATA;
      break;
    default:
      gcc_unreachable ();
    }
  gimplify_scan_omp_clauses (&OMP_CLAUSES (expr), pre_p, ort,
			     TREE_CODE (expr));
  if (TREE_CODE (expr) == OMP_TARGET)
    optimize_target_teams (expr, pre_p);
  if ((ort & (ORT_TARGET | ORT_TARGET_DATA)) != 0)
    {
      push_gimplify_context ();
      gimple *g = gimplify_and_return_first (OMP_BODY (expr), &body);
      if (gimple_code (g) == GIMPLE_BIND)
	pop_gimplify_context (g);
      else
	pop_gimplify_context (NULL);
      if ((ort & ORT_TARGET_DATA) != 0)
	{
	  enum built_in_function end_ix;
	  switch (TREE_CODE (expr))
	    {
	    case OACC_DATA:
	    case OACC_HOST_DATA:
	      end_ix = BUILT_IN_GOACC_DATA_END;
	      break;
	    case OMP_TARGET_DATA:
	      end_ix = BUILT_IN_GOMP_TARGET_END_DATA;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  tree fn = builtin_decl_explicit (end_ix);
	  g = gimple_build_call (fn, 0);
	  gimple_seq cleanup = NULL;
	  gimple_seq_add_stmt (&cleanup, g);
	  g = gimple_build_try (body, cleanup, GIMPLE_TRY_FINALLY);
	  body = NULL;
	  gimple_seq_add_stmt (&body, g);
	}
    }
  else
    gimplify_and_add (OMP_BODY (expr), &body);
  gimplify_adjust_omp_clauses (pre_p, body, &OMP_CLAUSES (expr),
			       TREE_CODE (expr));

  switch (TREE_CODE (expr))
    {
    case OACC_DATA:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_DATA,
				      OMP_CLAUSES (expr));
      break;
    case OACC_KERNELS:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_KERNELS,
				      OMP_CLAUSES (expr));
      break;
    case OACC_HOST_DATA:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_HOST_DATA,
				      OMP_CLAUSES (expr));
      break;
    case OACC_PARALLEL:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_PARALLEL,
				      OMP_CLAUSES (expr));
      break;
    case OMP_SECTIONS:
      stmt = gimple_build_omp_sections (body, OMP_CLAUSES (expr));
      break;
    case OMP_SINGLE:
      stmt = gimple_build_omp_single (body, OMP_CLAUSES (expr));
      break;
    case OMP_TARGET:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_REGION,
				      OMP_CLAUSES (expr));
      break;
    case OMP_TARGET_DATA:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_DATA,
				      OMP_CLAUSES (expr));
      break;
    case OMP_TEAMS:
      stmt = gimple_build_omp_teams (body, OMP_CLAUSES (expr));
      break;
    default:
      gcc_unreachable ();
    }

  gimplify_seq_add_stmt (pre_p, stmt);
  *expr_p = NULL_TREE;
}

/* Gimplify the gross structure of OpenACC enter/exit data, update, and OpenMP
   target update constructs.  */

static void
gimplify_omp_target_update (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p;
  int kind;
  gomp_target *stmt;
  enum omp_region_type ort = ORT_WORKSHARE;

  switch (TREE_CODE (expr))
    {
    case OACC_ENTER_DATA:
    case OACC_EXIT_DATA:
      kind = GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA;
      ort = ORT_ACC;
      break;
    case OACC_UPDATE:
      kind = GF_OMP_TARGET_KIND_OACC_UPDATE;
      ort = ORT_ACC;
      break;
    case OMP_TARGET_UPDATE:
      kind = GF_OMP_TARGET_KIND_UPDATE;
      break;
    case OMP_TARGET_ENTER_DATA:
      kind = GF_OMP_TARGET_KIND_ENTER_DATA;
      break;
    case OMP_TARGET_EXIT_DATA:
      kind = GF_OMP_TARGET_KIND_EXIT_DATA;
      break;
    default:
      gcc_unreachable ();
    }
  gimplify_scan_omp_clauses (&OMP_STANDALONE_CLAUSES (expr), pre_p,
			     ort, TREE_CODE (expr));
  gimplify_adjust_omp_clauses (pre_p, NULL, &OMP_STANDALONE_CLAUSES (expr),
			       TREE_CODE (expr));
  stmt = gimple_build_omp_target (NULL, kind, OMP_STANDALONE_CLAUSES (expr));

  gimplify_seq_add_stmt (pre_p, stmt);
  *expr_p = NULL_TREE;
}

/* A subroutine of gimplify_omp_atomic.  The front end is supposed to have
   stabilized the lhs of the atomic operation as *ADDR.  Return true if
   EXPR is this stabilized form.  */

static bool
goa_lhs_expr_p (tree expr, tree addr)
{
  /* Also include casts to other type variants.  The C front end is fond
     of adding these for e.g. volatile variables.  This is like
     STRIP_TYPE_NOPS but includes the main variant lookup.  */
  STRIP_USELESS_TYPE_CONVERSION (expr);

  if (TREE_CODE (expr) == INDIRECT_REF)
    {
      expr = TREE_OPERAND (expr, 0);
      while (expr != addr
	     && (CONVERT_EXPR_P (expr)
		 || TREE_CODE (expr) == NON_LVALUE_EXPR)
	     && TREE_CODE (expr) == TREE_CODE (addr)
	     && types_compatible_p (TREE_TYPE (expr), TREE_TYPE (addr)))
	{
	  expr = TREE_OPERAND (expr, 0);
	  addr = TREE_OPERAND (addr, 0);
	}
      if (expr == addr)
	return true;
      return (TREE_CODE (addr) == ADDR_EXPR
	      && TREE_CODE (expr) == ADDR_EXPR
	      && TREE_OPERAND (addr, 0) == TREE_OPERAND (expr, 0));
    }
  if (TREE_CODE (addr) == ADDR_EXPR && expr == TREE_OPERAND (addr, 0))
    return true;
  return false;
}

/* Walk *EXPR_P and replace appearances of *LHS_ADDR with LHS_VAR.  If an
   expression does not involve the lhs, evaluate it into a temporary.
   Return 1 if the lhs appeared as a subexpression, 0 if it did not,
   or -1 if an error was encountered.  */

static int
goa_stabilize_expr (tree *expr_p, gimple_seq *pre_p, tree lhs_addr,
		    tree lhs_var)
{
  tree expr = *expr_p;
  int saw_lhs;

  if (goa_lhs_expr_p (expr, lhs_addr))
    {
      *expr_p = lhs_var;
      return 1;
    }
  if (is_gimple_val (expr))
    return 0;

  saw_lhs = 0;
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_binary:
    case tcc_comparison:
      saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 1), pre_p, lhs_addr,
				     lhs_var);
      /* FALLTHRU */
    case tcc_unary:
      saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p, lhs_addr,
				     lhs_var);
      break;
    case tcc_expression:
      switch (TREE_CODE (expr))
	{
	case TRUTH_ANDIF_EXPR:
	case TRUTH_ORIF_EXPR:
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	case BIT_INSERT_EXPR:
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 1), pre_p,
					 lhs_addr, lhs_var);
	  /* FALLTHRU */
	case TRUTH_NOT_EXPR:
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
					 lhs_addr, lhs_var);
	  break;
	case COMPOUND_EXPR:
	  /* Break out any preevaluations from cp_build_modify_expr.  */
	  for (; TREE_CODE (expr) == COMPOUND_EXPR;
	       expr = TREE_OPERAND (expr, 1))
	    gimplify_stmt (&TREE_OPERAND (expr, 0), pre_p);
	  *expr_p = expr;
	  return goa_stabilize_expr (expr_p, pre_p, lhs_addr, lhs_var);
	default:
	  break;
	}
      break;
    case tcc_reference:
      if (TREE_CODE (expr) == BIT_FIELD_REF)
	saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
				       lhs_addr, lhs_var);
      break;
    default:
      break;
    }

  if (saw_lhs == 0)
    {
      enum gimplify_status gs;
      gs = gimplify_expr (expr_p, pre_p, NULL, is_gimple_val, fb_rvalue);
      if (gs != GS_ALL_DONE)
	saw_lhs = -1;
    }

  return saw_lhs;
}

/* Gimplify an OMP_ATOMIC statement.  */

static enum gimplify_status
gimplify_omp_atomic (tree *expr_p, gimple_seq *pre_p)
{
  tree addr = TREE_OPERAND (*expr_p, 0);
  tree rhs = TREE_CODE (*expr_p) == OMP_ATOMIC_READ
	     ? NULL : TREE_OPERAND (*expr_p, 1);
  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (addr)));
  tree tmp_load;
  gomp_atomic_load *loadstmt;
  gomp_atomic_store *storestmt;

  tmp_load = create_tmp_reg (type);
  if (rhs && goa_stabilize_expr (&rhs, pre_p, addr, tmp_load) < 0)
    return GS_ERROR;

  if (gimplify_expr (&addr, pre_p, NULL, is_gimple_val, fb_rvalue)
      != GS_ALL_DONE)
    return GS_ERROR;

  loadstmt = gimple_build_omp_atomic_load (tmp_load, addr);
  gimplify_seq_add_stmt (pre_p, loadstmt);
  if (rhs && gimplify_expr (&rhs, pre_p, NULL, is_gimple_val, fb_rvalue)
      != GS_ALL_DONE)
    return GS_ERROR;

  if (TREE_CODE (*expr_p) == OMP_ATOMIC_READ)
    rhs = tmp_load;
  storestmt = gimple_build_omp_atomic_store (rhs);
  gimplify_seq_add_stmt (pre_p, storestmt);
  if (OMP_ATOMIC_SEQ_CST (*expr_p))
    {
      gimple_omp_atomic_set_seq_cst (loadstmt);
      gimple_omp_atomic_set_seq_cst (storestmt);
    }
  switch (TREE_CODE (*expr_p))
    {
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
      *expr_p = tmp_load;
      gimple_omp_atomic_set_need_value (loadstmt);
      break;
    case OMP_ATOMIC_CAPTURE_NEW:
      *expr_p = rhs;
      gimple_omp_atomic_set_need_value (storestmt);
      break;
    default:
      *expr_p = NULL;
      break;
    }

  return GS_ALL_DONE;
}

/* Gimplify a TRANSACTION_EXPR.  This involves gimplification of the
   body, and adding some EH bits.  */

static enum gimplify_status
gimplify_transaction (tree *expr_p, gimple_seq *pre_p)
{
  tree expr = *expr_p, temp, tbody = TRANSACTION_EXPR_BODY (expr);
  gimple *body_stmt;
  gtransaction *trans_stmt;
  gimple_seq body = NULL;
  int subcode = 0;

  /* Wrap the transaction body in a BIND_EXPR so we have a context
     where to put decls for OMP.  */
  if (TREE_CODE (tbody) != BIND_EXPR)
    {
      tree bind = build3 (BIND_EXPR, void_type_node, NULL, tbody, NULL);
      TREE_SIDE_EFFECTS (bind) = 1;
      SET_EXPR_LOCATION (bind, EXPR_LOCATION (tbody));
      TRANSACTION_EXPR_BODY (expr) = bind;
    }

  push_gimplify_context ();
  temp = voidify_wrapper_expr (*expr_p, NULL);

  body_stmt = gimplify_and_return_first (TRANSACTION_EXPR_BODY (expr), &body);
  pop_gimplify_context (body_stmt);

  trans_stmt = gimple_build_transaction (body);
  if (TRANSACTION_EXPR_OUTER (expr))
    subcode = GTMA_IS_OUTER;
  else if (TRANSACTION_EXPR_RELAXED (expr))
    subcode = GTMA_IS_RELAXED;
  gimple_transaction_set_subcode (trans_stmt, subcode);

  gimplify_seq_add_stmt (pre_p, trans_stmt);

  if (temp)
    {
      *expr_p = temp;
      return GS_OK;
    }

  *expr_p = NULL_TREE;
  return GS_ALL_DONE;
}

/* Gimplify an OMP_ORDERED construct.  EXPR is the tree version.  BODY
   is the OMP_BODY of the original EXPR (which has already been
   gimplified so it's not present in the EXPR).

   Return the gimplified GIMPLE_OMP_ORDERED tuple.  */

static gimple *
gimplify_omp_ordered (tree expr, gimple_seq body)
{
  tree c, decls;
  int failures = 0;
  unsigned int i;
  tree source_c = NULL_TREE;
  tree sink_c = NULL_TREE;

  if (gimplify_omp_ctxp)
    {
      for (c = OMP_ORDERED_CLAUSES (expr); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && gimplify_omp_ctxp->loop_iter_var.is_empty ()
	    && (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK
		|| OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE))
	  {
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<ordered%> construct with %<depend%> clause must be "
		      "closely nested inside a loop with %<ordered%> clause "
		      "with a parameter");
	    failures++;
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		 && OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK)
	  {
	    bool fail = false;
	    for (decls = OMP_CLAUSE_DECL (c), i = 0;
		 decls && TREE_CODE (decls) == TREE_LIST;
		 decls = TREE_CHAIN (decls), ++i)
	      if (i >= gimplify_omp_ctxp->loop_iter_var.length () / 2)
		continue;
	      else if (TREE_VALUE (decls)
		       != gimplify_omp_ctxp->loop_iter_var[2 * i])
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "variable %qE is not an iteration "
			    "of outermost loop %d, expected %qE",
			    TREE_VALUE (decls), i + 1,
			    gimplify_omp_ctxp->loop_iter_var[2 * i]);
		  fail = true;
		  failures++;
		}
	      else
		TREE_VALUE (decls)
		  = gimplify_omp_ctxp->loop_iter_var[2 * i + 1];
	    if (!fail && i != gimplify_omp_ctxp->loop_iter_var.length () / 2)
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "number of variables in %<depend(sink)%> "
			  "clause does not match number of "
			  "iteration variables");
		failures++;
	      }
	    sink_c = c;
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		 && OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE)
	  {
	    if (source_c)
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "more than one %<depend(source)%> clause on an "
			  "%<ordered%> construct");
		failures++;
	      }
	    else
	      source_c = c;
	  }
    }
  if (source_c && sink_c)
    {
      error_at (OMP_CLAUSE_LOCATION (source_c),
		"%<depend(source)%> clause specified together with "
		"%<depend(sink:)%> clauses on the same construct");
      failures++;
    }

  if (failures)
    return gimple_build_nop ();
  return gimple_build_omp_ordered (body, OMP_ORDERED_CLAUSES (expr));
}

/* Convert the GENERIC expression tree *EXPR_P to GIMPLE.  If the
   expression produces a value to be used as an operand inside a GIMPLE
   statement, the value will be stored back in *EXPR_P.  This value will
   be a tree of class tcc_declaration, tcc_constant, tcc_reference or
   an SSA_NAME.  The corresponding sequence of GIMPLE statements is
   emitted in PRE_P and POST_P.

   Additionally, this process may overwrite parts of the input
   expression during gimplification.  Ideally, it should be
   possible to do non-destructive gimplification.

   EXPR_P points to the GENERIC expression to convert to GIMPLE.  If
      the expression needs to evaluate to a value to be used as
      an operand in a GIMPLE statement, this value will be stored in
      *EXPR_P on exit.  This happens when the caller specifies one
      of fb_lvalue or fb_rvalue fallback flags.

   PRE_P will contain the sequence of GIMPLE statements corresponding
       to the evaluation of EXPR and all the side-effects that must
       be executed before the main expression.  On exit, the last
       statement of PRE_P is the core statement being gimplified.  For
       instance, when gimplifying 'if (++a)' the last statement in
       PRE_P will be 'if (t.1)' where t.1 is the result of
       pre-incrementing 'a'.

   POST_P will contain the sequence of GIMPLE statements corresponding
       to the evaluation of all the side-effects that must be executed
       after the main expression.  If this is NULL, the post
       side-effects are stored at the end of PRE_P.

       The reason why the output is split in two is to handle post
       side-effects explicitly.  In some cases, an expression may have
       inner and outer post side-effects which need to be emitted in
       an order different from the one given by the recursive
       traversal.  For instance, for the expression (*p--)++ the post
       side-effects of '--' must actually occur *after* the post
       side-effects of '++'.  However, gimplification will first visit
       the inner expression, so if a separate POST sequence was not
       used, the resulting sequence would be:

       	    1	t.1 = *p
       	    2	p = p - 1
       	    3	t.2 = t.1 + 1
       	    4	*p = t.2

       However, the post-decrement operation in line #2 must not be
       evaluated until after the store to *p at line #4, so the
       correct sequence should be:

       	    1	t.1 = *p
       	    2	t.2 = t.1 + 1
       	    3	*p = t.2
       	    4	p = p - 1

       So, by specifying a separate post queue, it is possible
       to emit the post side-effects in the correct order.
       If POST_P is NULL, an internal queue will be used.  Before
       returning to the caller, the sequence POST_P is appended to
       the main output sequence PRE_P.

   GIMPLE_TEST_F points to a function that takes a tree T and
       returns nonzero if T is in the GIMPLE form requested by the
       caller.  The GIMPLE predicates are in gimple.c.

   FALLBACK tells the function what sort of a temporary we want if
       gimplification cannot produce an expression that complies with
       GIMPLE_TEST_F.

       fb_none means that no temporary should be generated
       fb_rvalue means that an rvalue is OK to generate
       fb_lvalue means that an lvalue is OK to generate
       fb_either means that either is OK, but an lvalue is preferable.
       fb_mayfail means that gimplification may fail (in which case
       GS_ERROR will be returned)

   The return value is either GS_ERROR or GS_ALL_DONE, since this
   function iterates until EXPR is completely gimplified or an error
   occurs.  */

enum gimplify_status
gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
	       bool (*gimple_test_f) (tree), fallback_t fallback)
{
  tree tmp;
  gimple_seq internal_pre = NULL;
  gimple_seq internal_post = NULL;
  tree save_expr;
  bool is_statement;
  location_t saved_location;
  enum gimplify_status ret;
  gimple_stmt_iterator pre_last_gsi, post_last_gsi;
  tree label;

  save_expr = *expr_p;
  if (save_expr == NULL_TREE)
    return GS_ALL_DONE;

  /* If we are gimplifying a top-level statement, PRE_P must be valid.  */
  is_statement = gimple_test_f == is_gimple_stmt;
  if (is_statement)
    gcc_assert (pre_p);

  /* Consistency checks.  */
  if (gimple_test_f == is_gimple_reg)
    gcc_assert (fallback & (fb_rvalue | fb_lvalue));
  else if (gimple_test_f == is_gimple_val
           || gimple_test_f == is_gimple_call_addr
           || gimple_test_f == is_gimple_condexpr
           || gimple_test_f == is_gimple_mem_rhs
           || gimple_test_f == is_gimple_mem_rhs_or_call
           || gimple_test_f == is_gimple_reg_rhs
           || gimple_test_f == is_gimple_reg_rhs_or_call
           || gimple_test_f == is_gimple_asm_val
	   || gimple_test_f == is_gimple_mem_ref_addr)
    gcc_assert (fallback & fb_rvalue);
  else if (gimple_test_f == is_gimple_min_lval
	   || gimple_test_f == is_gimple_lvalue)
    gcc_assert (fallback & fb_lvalue);
  else if (gimple_test_f == is_gimple_addressable)
    gcc_assert (fallback & fb_either);
  else if (gimple_test_f == is_gimple_stmt)
    gcc_assert (fallback == fb_none);
  else
    {
      /* We should have recognized the GIMPLE_TEST_F predicate to
	 know what kind of fallback to use in case a temporary is
	 needed to hold the value or address of *EXPR_P.  */
      gcc_unreachable ();
    }

  /* We used to check the predicate here and return immediately if it
     succeeds.  This is wrong; the design is for gimplification to be
     idempotent, and for the predicates to only test for valid forms, not
     whether they are fully simplified.  */
  if (pre_p == NULL)
    pre_p = &internal_pre;

  if (post_p == NULL)
    post_p = &internal_post;

  /* Remember the last statements added to PRE_P and POST_P.  Every
     new statement added by the gimplification helpers needs to be
     annotated with location information.  To centralize the
     responsibility, we remember the last statement that had been
     added to both queues before gimplifying *EXPR_P.  If
     gimplification produces new statements in PRE_P and POST_P, those
     statements will be annotated with the same location information
     as *EXPR_P.  */
  pre_last_gsi = gsi_last (*pre_p);
  post_last_gsi = gsi_last (*post_p);

  saved_location = input_location;
  if (save_expr != error_mark_node
      && EXPR_HAS_LOCATION (*expr_p))
    input_location = EXPR_LOCATION (*expr_p);

  /* Loop over the specific gimplifiers until the toplevel node
     remains the same.  */
  do
    {
      /* Strip away as many useless type conversions as possible
	 at the toplevel.  */
      STRIP_USELESS_TYPE_CONVERSION (*expr_p);

      /* Remember the expr.  */
      save_expr = *expr_p;

      /* Die, die, die, my darling.  */
      if (error_operand_p (save_expr))
	{
	  ret = GS_ERROR;
	  break;
	}

      /* Do any language-specific gimplification.  */
      ret = ((enum gimplify_status)
	     lang_hooks.gimplify_expr (expr_p, pre_p, post_p));
      if (ret == GS_OK)
	{
	  if (*expr_p == NULL_TREE)
	    break;
	  if (*expr_p != save_expr)
	    continue;
	}
      else if (ret != GS_UNHANDLED)
	break;

      /* Make sure that all the cases set 'ret' appropriately.  */
      ret = GS_UNHANDLED;
      switch (TREE_CODE (*expr_p))
	{
	  /* First deal with the special cases.  */

	case POSTINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case PREDECREMENT_EXPR:
	  ret = gimplify_self_mod_expr (expr_p, pre_p, post_p,
					fallback != fb_none,
					TREE_TYPE (*expr_p));
	  break;

	case VIEW_CONVERT_EXPR:
	  if (is_gimple_reg_type (TREE_TYPE (*expr_p))
	      && is_gimple_reg_type (TREE_TYPE (TREE_OPERAND (*expr_p, 0))))
	    {
	      ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
				   post_p, is_gimple_val, fb_rvalue);
	      recalculate_side_effects (*expr_p);
	      break;
	    }
	  /* Fallthru.  */

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	case COMPONENT_REF:
	  ret = gimplify_compound_lval (expr_p, pre_p, post_p,
					fallback ? fallback : fb_rvalue);
	  break;

	case COND_EXPR:
	  ret = gimplify_cond_expr (expr_p, pre_p, fallback);

	  /* C99 code may assign to an array in a structure value of a
	     conditional expression, and this has undefined behavior
	     only on execution, so create a temporary if an lvalue is
	     required.  */
	  if (fallback == fb_lvalue)
	    {
	      *expr_p = get_initialized_tmp_var (*expr_p, pre_p, post_p, false);
	      mark_addressable (*expr_p);
	      ret = GS_OK;
	    }
	  break;

	case CALL_EXPR:
	  ret = gimplify_call_expr (expr_p, pre_p, fallback != fb_none);

	  /* C99 code may assign to an array in a structure returned
	     from a function, and this has undefined behavior only on
	     execution, so create a temporary if an lvalue is
	     required.  */
	  if (fallback == fb_lvalue)
	    {
	      *expr_p = get_initialized_tmp_var (*expr_p, pre_p, post_p, false);
	      mark_addressable (*expr_p);
	      ret = GS_OK;
	    }
	  break;

	case TREE_LIST:
	  gcc_unreachable ();

	case COMPOUND_EXPR:
	  ret = gimplify_compound_expr (expr_p, pre_p, fallback != fb_none);
	  break;

	case COMPOUND_LITERAL_EXPR:
	  ret = gimplify_compound_literal_expr (expr_p, pre_p,
						gimple_test_f, fallback);
	  break;

	case MODIFY_EXPR:
	case INIT_EXPR:
	  ret = gimplify_modify_expr (expr_p, pre_p, post_p,
				      fallback != fb_none);
	  break;

	case TRUTH_ANDIF_EXPR:
	case TRUTH_ORIF_EXPR:
	  {
	    /* Preserve the original type of the expression and the
	       source location of the outer expression.  */
	    tree org_type = TREE_TYPE (*expr_p);
	    *expr_p = gimple_boolify (*expr_p);
	    *expr_p = build3_loc (input_location, COND_EXPR,
				  org_type, *expr_p,
				  fold_convert_loc
				    (input_location,
				     org_type, boolean_true_node),
				  fold_convert_loc
				    (input_location,
				     org_type, boolean_false_node));
	    ret = GS_OK;
	    break;
	  }

	case TRUTH_NOT_EXPR:
	  {
	    tree type = TREE_TYPE (*expr_p);
	    /* The parsers are careful to generate TRUTH_NOT_EXPR
	       only with operands that are always zero or one.
	       We do not fold here but handle the only interesting case
	       manually, as fold may re-introduce the TRUTH_NOT_EXPR.  */
	    *expr_p = gimple_boolify (*expr_p);
	    if (TYPE_PRECISION (TREE_TYPE (*expr_p)) == 1)
	      *expr_p = build1_loc (input_location, BIT_NOT_EXPR,
				    TREE_TYPE (*expr_p),
				    TREE_OPERAND (*expr_p, 0));
	    else
	      *expr_p = build2_loc (input_location, BIT_XOR_EXPR,
				    TREE_TYPE (*expr_p),
				    TREE_OPERAND (*expr_p, 0),
				    build_int_cst (TREE_TYPE (*expr_p), 1));
	    if (!useless_type_conversion_p (type, TREE_TYPE (*expr_p)))
	      *expr_p = fold_convert_loc (input_location, type, *expr_p);
	    ret = GS_OK;
	    break;
	  }

	case ADDR_EXPR:
	  ret = gimplify_addr_expr (expr_p, pre_p, post_p);
	  break;

	case ANNOTATE_EXPR:
	  {
	    tree cond = TREE_OPERAND (*expr_p, 0);
	    tree kind = TREE_OPERAND (*expr_p, 1);
	    tree data = TREE_OPERAND (*expr_p, 2);
	    tree type = TREE_TYPE (cond);
	    if (!INTEGRAL_TYPE_P (type))
	      {
		*expr_p = cond;
		ret = GS_OK;
		break;
	      }
	    tree tmp = create_tmp_var (type);
	    gimplify_arg (&cond, pre_p, EXPR_LOCATION (*expr_p));
	    gcall *call
	      = gimple_build_call_internal (IFN_ANNOTATE, 3, cond, kind, data);
	    gimple_call_set_lhs (call, tmp);
	    gimplify_seq_add_stmt (pre_p, call);
	    *expr_p = tmp;
	    ret = GS_ALL_DONE;
	    break;
	  }

	case VA_ARG_EXPR:
	  ret = gimplify_va_arg_expr (expr_p, pre_p, post_p);
	  break;

	CASE_CONVERT:
	  if (IS_EMPTY_STMT (*expr_p))
	    {
	      ret = GS_ALL_DONE;
	      break;
	    }

	  if (VOID_TYPE_P (TREE_TYPE (*expr_p))
	      || fallback == fb_none)
	    {
	      /* Just strip a conversion to void (or in void context) and
		 try again.  */
	      *expr_p = TREE_OPERAND (*expr_p, 0);
	      ret = GS_OK;
	      break;
	    }

	  ret = gimplify_conversion (expr_p);
	  if (ret == GS_ERROR)
	    break;
	  if (*expr_p != save_expr)
	    break;
	  /* FALLTHRU */

	case FIX_TRUNC_EXPR:
	  /* unary_expr: ... | '(' cast ')' val | ...  */
	  ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			       is_gimple_val, fb_rvalue);
	  recalculate_side_effects (*expr_p);
	  break;

	case INDIRECT_REF:
	  {
	    bool volatilep = TREE_THIS_VOLATILE (*expr_p);
	    bool notrap = TREE_THIS_NOTRAP (*expr_p);
	    tree saved_ptr_type = TREE_TYPE (TREE_OPERAND (*expr_p, 0));

	    *expr_p = fold_indirect_ref_loc (input_location, *expr_p);
	    if (*expr_p != save_expr)
	      {
		ret = GS_OK;
		break;
	      }

	    ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
				 is_gimple_reg, fb_rvalue);
	    if (ret == GS_ERROR)
	      break;

	    recalculate_side_effects (*expr_p);
	    *expr_p = fold_build2_loc (input_location, MEM_REF,
				       TREE_TYPE (*expr_p),
				       TREE_OPERAND (*expr_p, 0),
				       build_int_cst (saved_ptr_type, 0));
	    TREE_THIS_VOLATILE (*expr_p) = volatilep;
	    TREE_THIS_NOTRAP (*expr_p) = notrap;
	    ret = GS_OK;
	    break;
	  }

	/* We arrive here through the various re-gimplifcation paths.  */
	case MEM_REF:
	  /* First try re-folding the whole thing.  */
	  tmp = fold_binary (MEM_REF, TREE_TYPE (*expr_p),
			     TREE_OPERAND (*expr_p, 0),
			     TREE_OPERAND (*expr_p, 1));
	  if (tmp)
	    {
	      REF_REVERSE_STORAGE_ORDER (tmp)
	        = REF_REVERSE_STORAGE_ORDER (*expr_p);
	      *expr_p = tmp;
	      recalculate_side_effects (*expr_p);
	      ret = GS_OK;
	      break;
	    }
	  /* Avoid re-gimplifying the address operand if it is already
	     in suitable form.  Re-gimplifying would mark the address
	     operand addressable.  Always gimplify when not in SSA form
	     as we still may have to gimplify decls with value-exprs.  */
	  if (!gimplify_ctxp || !gimple_in_ssa_p (cfun)
	      || !is_gimple_mem_ref_addr (TREE_OPERAND (*expr_p, 0)))
	    {
	      ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
				   is_gimple_mem_ref_addr, fb_rvalue);
	      if (ret == GS_ERROR)
		break;
	    }
	  recalculate_side_effects (*expr_p);
	  ret = GS_ALL_DONE;
	  break;

	/* Constants need not be gimplified.  */
	case INTEGER_CST:
	case REAL_CST:
	case FIXED_CST:
	case STRING_CST:
	case COMPLEX_CST:
	case VECTOR_CST:
	  /* Drop the overflow flag on constants, we do not want
	     that in the GIMPLE IL.  */
	  if (TREE_OVERFLOW_P (*expr_p))
	    *expr_p = drop_tree_overflow (*expr_p);
	  ret = GS_ALL_DONE;
	  break;

	case CONST_DECL:
	  /* If we require an lvalue, such as for ADDR_EXPR, retain the
	     CONST_DECL node.  Otherwise the decl is replaceable by its
	     value.  */
	  /* ??? Should be == fb_lvalue, but ADDR_EXPR passes fb_either.  */
	  if (fallback & fb_lvalue)
	    ret = GS_ALL_DONE;
	  else
	    {
	      *expr_p = DECL_INITIAL (*expr_p);
	      ret = GS_OK;
	    }
	  break;

	case DECL_EXPR:
	  ret = gimplify_decl_expr (expr_p, pre_p);
	  break;

	case BIND_EXPR:
	  ret = gimplify_bind_expr (expr_p, pre_p);
	  break;

	case LOOP_EXPR:
	  ret = gimplify_loop_expr (expr_p, pre_p);
	  break;

	case SWITCH_EXPR:
	  ret = gimplify_switch_expr (expr_p, pre_p);
	  break;

	case EXIT_EXPR:
	  ret = gimplify_exit_expr (expr_p);
	  break;

	case GOTO_EXPR:
	  /* If the target is not LABEL, then it is a computed jump
	     and the target needs to be gimplified.  */
	  if (TREE_CODE (GOTO_DESTINATION (*expr_p)) != LABEL_DECL)
	    {
	      ret = gimplify_expr (&GOTO_DESTINATION (*expr_p), pre_p,
				   NULL, is_gimple_val, fb_rvalue);
	      if (ret == GS_ERROR)
		break;
	    }
	  gimplify_seq_add_stmt (pre_p,
			  gimple_build_goto (GOTO_DESTINATION (*expr_p)));
	  ret = GS_ALL_DONE;
	  break;

	case PREDICT_EXPR:
	  gimplify_seq_add_stmt (pre_p,
			gimple_build_predict (PREDICT_EXPR_PREDICTOR (*expr_p),
					      PREDICT_EXPR_OUTCOME (*expr_p)));
	  ret = GS_ALL_DONE;
	  break;

	case LABEL_EXPR:
	  ret = gimplify_label_expr (expr_p, pre_p);
	  label = LABEL_EXPR_LABEL (*expr_p);
	  gcc_assert (decl_function_context (label) == current_function_decl);

	  /* If the label is used in a goto statement, or address of the label
	     is taken, we need to unpoison all variables that were seen so far.
	     Doing so would prevent us from reporting a false positives.  */
	  if (asan_poisoned_variables
	      && asan_used_labels != NULL
	      && asan_used_labels->contains (label))
	    asan_poison_variables (asan_poisoned_variables, false, pre_p);
	  break;

	case CASE_LABEL_EXPR:
	  ret = gimplify_case_label_expr (expr_p, pre_p);

	  if (gimplify_ctxp->live_switch_vars)
	    asan_poison_variables (gimplify_ctxp->live_switch_vars, false,
				   pre_p);
	  break;

	case RETURN_EXPR:
	  ret = gimplify_return_expr (*expr_p, pre_p);
	  break;

	case CONSTRUCTOR:
	  /* Don't reduce this in place; let gimplify_init_constructor work its
	     magic.  Buf if we're just elaborating this for side effects, just
	     gimplify any element that has side-effects.  */
	  if (fallback == fb_none)
	    {
	      unsigned HOST_WIDE_INT ix;
	      tree val;
	      tree temp = NULL_TREE;
	      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (*expr_p), ix, val)
		if (TREE_SIDE_EFFECTS (val))
		  append_to_statement_list (val, &temp);

	      *expr_p = temp;
	      ret = temp ? GS_OK : GS_ALL_DONE;
	    }
	  /* C99 code may assign to an array in a constructed
	     structure or union, and this has undefined behavior only
	     on execution, so create a temporary if an lvalue is
	     required.  */
	  else if (fallback == fb_lvalue)
	    {
	      *expr_p = get_initialized_tmp_var (*expr_p, pre_p, post_p, false);
	      mark_addressable (*expr_p);
	      ret = GS_OK;
	    }
	  else
	    ret = GS_ALL_DONE;
	  break;

	  /* The following are special cases that are not handled by the
	     original GIMPLE grammar.  */

	  /* SAVE_EXPR nodes are converted into a GIMPLE identifier and
	     eliminated.  */
	case SAVE_EXPR:
	  ret = gimplify_save_expr (expr_p, pre_p, post_p);
	  break;

	case BIT_FIELD_REF:
	  ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
			       post_p, is_gimple_lvalue, fb_either);
	  recalculate_side_effects (*expr_p);
	  break;

	case TARGET_MEM_REF:
	  {
	    enum gimplify_status r0 = GS_ALL_DONE, r1 = GS_ALL_DONE;

	    if (TMR_BASE (*expr_p))
	      r0 = gimplify_expr (&TMR_BASE (*expr_p), pre_p,
				  post_p, is_gimple_mem_ref_addr, fb_either);
	    if (TMR_INDEX (*expr_p))
	      r1 = gimplify_expr (&TMR_INDEX (*expr_p), pre_p,
				  post_p, is_gimple_val, fb_rvalue);
	    if (TMR_INDEX2 (*expr_p))
	      r1 = gimplify_expr (&TMR_INDEX2 (*expr_p), pre_p,
				  post_p, is_gimple_val, fb_rvalue);
	    /* TMR_STEP and TMR_OFFSET are always integer constants.  */
	    ret = MIN (r0, r1);
	  }
	  break;

	case NON_LVALUE_EXPR:
	  /* This should have been stripped above.  */
	  gcc_unreachable ();

	case ASM_EXPR:
	  ret = gimplify_asm_expr (expr_p, pre_p, post_p);
	  break;

	case TRY_FINALLY_EXPR:
	case TRY_CATCH_EXPR:
	  {
	    gimple_seq eval, cleanup;
	    gtry *try_;

	    /* Calls to destructors are generated automatically in FINALLY/CATCH
	       block. They should have location as UNKNOWN_LOCATION. However,
	       gimplify_call_expr will reset these call stmts to input_location
	       if it finds stmt's location is unknown. To prevent resetting for
	       destructors, we set the input_location to unknown.
	       Note that this only affects the destructor calls in FINALLY/CATCH
	       block, and will automatically reset to its original value by the
	       end of gimplify_expr.  */
	    input_location = UNKNOWN_LOCATION;
	    eval = cleanup = NULL;
	    gimplify_and_add (TREE_OPERAND (*expr_p, 0), &eval);
	    gimplify_and_add (TREE_OPERAND (*expr_p, 1), &cleanup);
	    /* Don't create bogus GIMPLE_TRY with empty cleanup.  */
	    if (gimple_seq_empty_p (cleanup))
	      {
		gimple_seq_add_seq (pre_p, eval);
		ret = GS_ALL_DONE;
		break;
	      }
	    try_ = gimple_build_try (eval, cleanup,
				     TREE_CODE (*expr_p) == TRY_FINALLY_EXPR
				     ? GIMPLE_TRY_FINALLY
				     : GIMPLE_TRY_CATCH);
	    if (EXPR_HAS_LOCATION (save_expr))
	      gimple_set_location (try_, EXPR_LOCATION (save_expr));
	    else if (LOCATION_LOCUS (saved_location) != UNKNOWN_LOCATION)
	      gimple_set_location (try_, saved_location);
	    if (TREE_CODE (*expr_p) == TRY_CATCH_EXPR)
	      gimple_try_set_catch_is_cleanup (try_,
					       TRY_CATCH_IS_CLEANUP (*expr_p));
	    gimplify_seq_add_stmt (pre_p, try_);
	    ret = GS_ALL_DONE;
	    break;
	  }

	case CLEANUP_POINT_EXPR:
	  ret = gimplify_cleanup_point_expr (expr_p, pre_p);
	  break;

	case TARGET_EXPR:
	  ret = gimplify_target_expr (expr_p, pre_p, post_p);
	  break;

	case CATCH_EXPR:
	  {
	    gimple *c;
	    gimple_seq handler = NULL;
	    gimplify_and_add (CATCH_BODY (*expr_p), &handler);
	    c = gimple_build_catch (CATCH_TYPES (*expr_p), handler);
	    gimplify_seq_add_stmt (pre_p, c);
	    ret = GS_ALL_DONE;
	    break;
	  }

	case EH_FILTER_EXPR:
	  {
	    gimple *ehf;
	    gimple_seq failure = NULL;

	    gimplify_and_add (EH_FILTER_FAILURE (*expr_p), &failure);
	    ehf = gimple_build_eh_filter (EH_FILTER_TYPES (*expr_p), failure);
	    gimple_set_no_warning (ehf, TREE_NO_WARNING (*expr_p));
	    gimplify_seq_add_stmt (pre_p, ehf);
	    ret = GS_ALL_DONE;
	    break;
	  }

	case OBJ_TYPE_REF:
	  {
	    enum gimplify_status r0, r1;
	    r0 = gimplify_expr (&OBJ_TYPE_REF_OBJECT (*expr_p), pre_p,
				post_p, is_gimple_val, fb_rvalue);
	    r1 = gimplify_expr (&OBJ_TYPE_REF_EXPR (*expr_p), pre_p,
				post_p, is_gimple_val, fb_rvalue);
	    TREE_SIDE_EFFECTS (*expr_p) = 0;
	    ret = MIN (r0, r1);
	  }
	  break;

	case LABEL_DECL:
	  /* We get here when taking the address of a label.  We mark
	     the label as "forced"; meaning it can never be removed and
	     it is a potential target for any computed goto.  */
	  FORCED_LABEL (*expr_p) = 1;
	  ret = GS_ALL_DONE;
	  break;

	case STATEMENT_LIST:
	  ret = gimplify_statement_list (expr_p, pre_p);
	  break;

	case WITH_SIZE_EXPR:
	  {
	    gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
			   post_p == &internal_post ? NULL : post_p,
			   gimple_test_f, fallback);
	    gimplify_expr (&TREE_OPERAND (*expr_p, 1), pre_p, post_p,
			   is_gimple_val, fb_rvalue);
	    ret = GS_ALL_DONE;
	  }
	  break;

	case VAR_DECL:
	case PARM_DECL:
	  ret = gimplify_var_or_parm_decl (expr_p);
	  break;

	case RESULT_DECL:
	  /* When within an OMP context, notice uses of variables.  */
	  if (gimplify_omp_ctxp)
	    omp_notice_variable (gimplify_omp_ctxp, *expr_p, true);
	  ret = GS_ALL_DONE;
	  break;

	case DEBUG_EXPR_DECL:
	  gcc_unreachable ();

	case DEBUG_BEGIN_STMT:
	  gimplify_seq_add_stmt (pre_p,
				 gimple_build_debug_begin_stmt
				 (TREE_BLOCK (*expr_p),
				  EXPR_LOCATION (*expr_p)));
	  ret = GS_ALL_DONE;
	  *expr_p = NULL;
	  break;

	case SSA_NAME:
	  /* Allow callbacks into the gimplifier during optimization.  */
	  ret = GS_ALL_DONE;
	  break;

	case OMP_PARALLEL:
	  gimplify_omp_parallel (expr_p, pre_p);
	  ret = GS_ALL_DONE;
	  break;

	case OMP_TASK:
	  gimplify_omp_task (expr_p, pre_p);
	  ret = GS_ALL_DONE;
	  break;

	case OMP_FOR:
	case OMP_SIMD:
	case OMP_DISTRIBUTE:
	case OMP_TASKLOOP:
	case OACC_LOOP:
	  ret = gimplify_omp_for (expr_p, pre_p);
	  break;

	case OACC_CACHE:
	  gimplify_oacc_cache (expr_p, pre_p);
	  ret = GS_ALL_DONE;
	  break;

	case OACC_DECLARE:
	  gimplify_oacc_declare (expr_p, pre_p);
	  ret = GS_ALL_DONE;
	  break;

	case OACC_HOST_DATA:
	case OACC_DATA:
	case OACC_KERNELS:
	case OACC_PARALLEL:
	case OMP_SECTIONS:
	case OMP_SINGLE:
	case OMP_TARGET:
	case OMP_TARGET_DATA:
	case OMP_TEAMS:
	  gimplify_omp_workshare (expr_p, pre_p);
	  ret = GS_ALL_DONE;
	  break;

	case OACC_ENTER_DATA:
	case OACC_EXIT_DATA:
	case OACC_UPDATE:
	case OMP_TARGET_UPDATE:
	case OMP_TARGET_ENTER_DATA:
	case OMP_TARGET_EXIT_DATA:
	  gimplify_omp_target_update (expr_p, pre_p);
	  ret = GS_ALL_DONE;
	  break;

	case OMP_SECTION:
	case OMP_MASTER:
	case OMP_TASKGROUP:
	case OMP_ORDERED:
	case OMP_CRITICAL:
	  {
	    gimple_seq body = NULL;
	    gimple *g;

	    gimplify_and_add (OMP_BODY (*expr_p), &body);
	    switch (TREE_CODE (*expr_p))
	      {
	      case OMP_SECTION:
	        g = gimple_build_omp_section (body);
	        break;
	      case OMP_MASTER:
	        g = gimple_build_omp_master (body);
		break;
	      case OMP_TASKGROUP:
		{
		  gimple_seq cleanup = NULL;
		  tree fn
		    = builtin_decl_explicit (BUILT_IN_GOMP_TASKGROUP_END);
		  g = gimple_build_call (fn, 0);
		  gimple_seq_add_stmt (&cleanup, g);
		  g = gimple_build_try (body, cleanup, GIMPLE_TRY_FINALLY);
		  body = NULL;
		  gimple_seq_add_stmt (&body, g);
		  g = gimple_build_omp_taskgroup (body);
		}
		break;
	      case OMP_ORDERED:
		g = gimplify_omp_ordered (*expr_p, body);
		break;
	      case OMP_CRITICAL:
		gimplify_scan_omp_clauses (&OMP_CRITICAL_CLAUSES (*expr_p),
					   pre_p, ORT_WORKSHARE, OMP_CRITICAL);
		gimplify_adjust_omp_clauses (pre_p, body,
					     &OMP_CRITICAL_CLAUSES (*expr_p),
					     OMP_CRITICAL);
		g = gimple_build_omp_critical (body,
		    			       OMP_CRITICAL_NAME (*expr_p),
		    			       OMP_CRITICAL_CLAUSES (*expr_p));
		break;
	      default:
		gcc_unreachable ();
	      }
	    gimplify_seq_add_stmt (pre_p, g);
	    ret = GS_ALL_DONE;
	    break;
	  }

	case OMP_ATOMIC:
	case OMP_ATOMIC_READ:
	case OMP_ATOMIC_CAPTURE_OLD:
	case OMP_ATOMIC_CAPTURE_NEW:
	  ret = gimplify_omp_atomic (expr_p, pre_p);
	  break;

	case TRANSACTION_EXPR:
	  ret = gimplify_transaction (expr_p, pre_p);
	  break;

	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	  {
	    tree orig_type = TREE_TYPE (*expr_p);
	    tree new_type, xop0, xop1;
	    *expr_p = gimple_boolify (*expr_p);
	    new_type = TREE_TYPE (*expr_p);
	    if (!useless_type_conversion_p (orig_type, new_type))
	      {
		*expr_p = fold_convert_loc (input_location, orig_type, *expr_p);
		ret = GS_OK;
		break;
	      }

	  /* Boolified binary truth expressions are semantically equivalent
	     to bitwise binary expressions.  Canonicalize them to the
	     bitwise variant.  */
	    switch (TREE_CODE (*expr_p))
	      {
	      case TRUTH_AND_EXPR:
		TREE_SET_CODE (*expr_p, BIT_AND_EXPR);
		break;
	      case TRUTH_OR_EXPR:
		TREE_SET_CODE (*expr_p, BIT_IOR_EXPR);
		break;
	      case TRUTH_XOR_EXPR:
		TREE_SET_CODE (*expr_p, BIT_XOR_EXPR);
		break;
	      default:
		break;
	      }
	    /* Now make sure that operands have compatible type to
	       expression's new_type.  */
	    xop0 = TREE_OPERAND (*expr_p, 0);
	    xop1 = TREE_OPERAND (*expr_p, 1);
	    if (!useless_type_conversion_p (new_type, TREE_TYPE (xop0)))
	      TREE_OPERAND (*expr_p, 0) = fold_convert_loc (input_location,
							    new_type,
	      						    xop0);
	    if (!useless_type_conversion_p (new_type, TREE_TYPE (xop1)))
	      TREE_OPERAND (*expr_p, 1) = fold_convert_loc (input_location,
							    new_type,
	      						    xop1);
	    /* Continue classified as tcc_binary.  */
	    goto expr_2;
	  }

	case VEC_COND_EXPR:
	  {
	    enum gimplify_status r0, r1, r2;

	    r0 = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
				post_p, is_gimple_condexpr, fb_rvalue);
	    r1 = gimplify_expr (&TREE_OPERAND (*expr_p, 1), pre_p,
				post_p, is_gimple_val, fb_rvalue);
	    r2 = gimplify_expr (&TREE_OPERAND (*expr_p, 2), pre_p,
				post_p, is_gimple_val, fb_rvalue);

	    ret = MIN (MIN (r0, r1), r2);
	    recalculate_side_effects (*expr_p);
	  }
	  break;

	case FMA_EXPR:
	case VEC_PERM_EXPR:
	  /* Classified as tcc_expression.  */
	  goto expr_3;

	case BIT_INSERT_EXPR:
	  /* Argument 3 is a constant.  */
	  goto expr_2;

	case POINTER_PLUS_EXPR:
	  {
	    enum gimplify_status r0, r1;
	    r0 = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
				post_p, is_gimple_val, fb_rvalue);
	    r1 = gimplify_expr (&TREE_OPERAND (*expr_p, 1), pre_p,
				post_p, is_gimple_val, fb_rvalue);
	    recalculate_side_effects (*expr_p);
	    ret = MIN (r0, r1);
	    break;
	  }

	default:
	  switch (TREE_CODE_CLASS (TREE_CODE (*expr_p)))
	    {
	    case tcc_comparison:
	      /* Handle comparison of objects of non scalar mode aggregates
	     	 with a call to memcmp.  It would be nice to only have to do
	     	 this for variable-sized objects, but then we'd have to allow
	     	 the same nest of reference nodes we allow for MODIFY_EXPR and
	     	 that's too complex.

		 Compare scalar mode aggregates as scalar mode values.  Using
		 memcmp for them would be very inefficient at best, and is
		 plain wrong if bitfields are involved.  */
		{
		  tree type = TREE_TYPE (TREE_OPERAND (*expr_p, 1));

		  /* Vector comparisons need no boolification.  */
		  if (TREE_CODE (type) == VECTOR_TYPE)
		    goto expr_2;
		  else if (!AGGREGATE_TYPE_P (type))
		    {
		      tree org_type = TREE_TYPE (*expr_p);
		      *expr_p = gimple_boolify (*expr_p);
		      if (!useless_type_conversion_p (org_type,
						      TREE_TYPE (*expr_p)))
			{
			  *expr_p = fold_convert_loc (input_location,
						      org_type, *expr_p);
			  ret = GS_OK;
			}
		      else
			goto expr_2;
		    }
		  else if (TYPE_MODE (type) != BLKmode)
		    ret = gimplify_scalar_mode_aggregate_compare (expr_p);
		  else
		    ret = gimplify_variable_sized_compare (expr_p);

		  break;
		}

	    /* If *EXPR_P does not need to be special-cased, handle it
	       according to its class.  */
	    case tcc_unary:
	      ret = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
				   post_p, is_gimple_val, fb_rvalue);
	      break;

	    case tcc_binary:
	    expr_2:
	      {
		enum gimplify_status r0, r1;

		r0 = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
		                    post_p, is_gimple_val, fb_rvalue);
		r1 = gimplify_expr (&TREE_OPERAND (*expr_p, 1), pre_p,
				    post_p, is_gimple_val, fb_rvalue);

		ret = MIN (r0, r1);
		break;
	      }

	    expr_3:
	      {
		enum gimplify_status r0, r1, r2;

		r0 = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p,
		                    post_p, is_gimple_val, fb_rvalue);
		r1 = gimplify_expr (&TREE_OPERAND (*expr_p, 1), pre_p,
				    post_p, is_gimple_val, fb_rvalue);
		r2 = gimplify_expr (&TREE_OPERAND (*expr_p, 2), pre_p,
				    post_p, is_gimple_val, fb_rvalue);

		ret = MIN (MIN (r0, r1), r2);
		break;
	      }

	    case tcc_declaration:
	    case tcc_constant:
	      ret = GS_ALL_DONE;
	      goto dont_recalculate;

	    default:
	      gcc_unreachable ();
	    }

	  recalculate_side_effects (*expr_p);

	dont_recalculate:
	  break;
	}

      gcc_assert (*expr_p || ret != GS_OK);
    }
  while (ret == GS_OK);

  /* If we encountered an error_mark somewhere nested inside, either
     stub out the statement or propagate the error back out.  */
  if (ret == GS_ERROR)
    {
      if (is_statement)
	*expr_p = NULL;
      goto out;
    }

  /* This was only valid as a return value from the langhook, which
     we handled.  Make sure it doesn't escape from any other context.  */
  gcc_assert (ret != GS_UNHANDLED);

  if (fallback == fb_none && *expr_p && !is_gimple_stmt (*expr_p))
    {
      /* We aren't looking for a value, and we don't have a valid
	 statement.  If it doesn't have side-effects, throw it away.
	 We can also get here with code such as "*&&L;", where L is
	 a LABEL_DECL that is marked as FORCED_LABEL.  */
      if (TREE_CODE (*expr_p) == LABEL_DECL
	  || !TREE_SIDE_EFFECTS (*expr_p))
	*expr_p = NULL;
      else if (!TREE_THIS_VOLATILE (*expr_p))
	{
	  /* This is probably a _REF that contains something nested that
	     has side effects.  Recurse through the operands to find it.  */
	  enum tree_code code = TREE_CODE (*expr_p);

	  switch (code)
	    {
	    case COMPONENT_REF:
	    case REALPART_EXPR:
	    case IMAGPART_EXPR:
	    case VIEW_CONVERT_EXPR:
	      gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			     gimple_test_f, fallback);
	      break;

	    case ARRAY_REF:
	    case ARRAY_RANGE_REF:
	      gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			     gimple_test_f, fallback);
	      gimplify_expr (&TREE_OPERAND (*expr_p, 1), pre_p, post_p,
			     gimple_test_f, fallback);
	      break;

	    default:
	       /* Anything else with side-effects must be converted to
		  a valid statement before we get here.  */
	      gcc_unreachable ();
	    }

	  *expr_p = NULL;
	}
      else if (COMPLETE_TYPE_P (TREE_TYPE (*expr_p))
	       && TYPE_MODE (TREE_TYPE (*expr_p)) != BLKmode)
	{
	  /* Historically, the compiler has treated a bare reference
	     to a non-BLKmode volatile lvalue as forcing a load.  */
	  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (*expr_p));

	  /* Normally, we do not want to create a temporary for a
	     TREE_ADDRESSABLE type because such a type should not be
	     copied by bitwise-assignment.  However, we make an
	     exception here, as all we are doing here is ensuring that
	     we read the bytes that make up the type.  We use
	     create_tmp_var_raw because create_tmp_var will abort when
	     given a TREE_ADDRESSABLE type.  */
	  tree tmp = create_tmp_var_raw (type, "vol");
	  gimple_add_tmp_var (tmp);
	  gimplify_assign (tmp, *expr_p, pre_p);
	  *expr_p = NULL;
	}
      else
	/* We can't do anything useful with a volatile reference to
	   an incomplete type, so just throw it away.  Likewise for
	   a BLKmode type, since any implicit inner load should
	   already have been turned into an explicit one by the
	   gimplification process.  */
	*expr_p = NULL;
    }

  /* If we are gimplifying at the statement level, we're done.  Tack
     everything together and return.  */
  if (fallback == fb_none || is_statement)
    {
      /* Since *EXPR_P has been converted into a GIMPLE tuple, clear
         it out for GC to reclaim it.  */
      *expr_p = NULL_TREE;

      if (!gimple_seq_empty_p (internal_pre)
	  || !gimple_seq_empty_p (internal_post))
	{
	  gimplify_seq_add_seq (&internal_pre, internal_post);
	  gimplify_seq_add_seq (pre_p, internal_pre);
	}

      /* The result of gimplifying *EXPR_P is going to be the last few
	 statements in *PRE_P and *POST_P.  Add location information
	 to all the statements that were added by the gimplification
	 helpers.  */
      if (!gimple_seq_empty_p (*pre_p))
	annotate_all_with_location_after (*pre_p, pre_last_gsi, input_location);

      if (!gimple_seq_empty_p (*post_p))
	annotate_all_with_location_after (*post_p, post_last_gsi,
					  input_location);

      goto out;
    }

#ifdef ENABLE_GIMPLE_CHECKING
  if (*expr_p)
    {
      enum tree_code code = TREE_CODE (*expr_p);
      /* These expressions should already be in gimple IR form.  */
      gcc_assert (code != MODIFY_EXPR
		  && code != ASM_EXPR
		  && code != BIND_EXPR
		  && code != CATCH_EXPR
		  && (code != COND_EXPR || gimplify_ctxp->allow_rhs_cond_expr)
		  && code != EH_FILTER_EXPR
		  && code != GOTO_EXPR
		  && code != LABEL_EXPR
		  && code != LOOP_EXPR
		  && code != SWITCH_EXPR
		  && code != TRY_FINALLY_EXPR
		  && code != OACC_PARALLEL
		  && code != OACC_KERNELS
		  && code != OACC_DATA
		  && code != OACC_HOST_DATA
		  && code != OACC_DECLARE
		  && code != OACC_UPDATE
		  && code != OACC_ENTER_DATA
		  && code != OACC_EXIT_DATA
		  && code != OACC_CACHE
		  && code != OMP_CRITICAL
		  && code != OMP_FOR
		  && code != OACC_LOOP
		  && code != OMP_MASTER
		  && code != OMP_TASKGROUP
		  && code != OMP_ORDERED
		  && code != OMP_PARALLEL
		  && code != OMP_SECTIONS
		  && code != OMP_SECTION
		  && code != OMP_SINGLE);
    }
#endif

  /* Otherwise we're gimplifying a subexpression, so the resulting
     value is interesting.  If it's a valid operand that matches
     GIMPLE_TEST_F, we're done. Unless we are handling some
     post-effects internally; if that's the case, we need to copy into
     a temporary before adding the post-effects to POST_P.  */
  if (gimple_seq_empty_p (internal_post) && (*gimple_test_f) (*expr_p))
    goto out;

  /* Otherwise, we need to create a new temporary for the gimplified
     expression.  */

  /* We can't return an lvalue if we have an internal postqueue.  The
     object the lvalue refers to would (probably) be modified by the
     postqueue; we need to copy the value out first, which means an
     rvalue.  */
  if ((fallback & fb_lvalue)
      && gimple_seq_empty_p (internal_post)
      && is_gimple_addressable (*expr_p))
    {
      /* An lvalue will do.  Take the address of the expression, store it
	 in a temporary, and replace the expression with an INDIRECT_REF of
	 that temporary.  */
      tmp = build_fold_addr_expr_loc (input_location, *expr_p);
      gimplify_expr (&tmp, pre_p, post_p, is_gimple_reg, fb_rvalue);
      *expr_p = build_simple_mem_ref (tmp);
    }
  else if ((fallback & fb_rvalue) && is_gimple_reg_rhs_or_call (*expr_p))
    {
      /* An rvalue will do.  Assign the gimplified expression into a
	 new temporary TMP and replace the original expression with
	 TMP.  First, make sure that the expression has a type so that
	 it can be assigned into a temporary.  */
      gcc_assert (!VOID_TYPE_P (TREE_TYPE (*expr_p)));
      *expr_p = get_formal_tmp_var (*expr_p, pre_p);
    }
  else
    {
#ifdef ENABLE_GIMPLE_CHECKING
      if (!(fallback & fb_mayfail))
	{
	  fprintf (stderr, "gimplification failed:\n");
	  print_generic_expr (stderr, *expr_p);
	  debug_tree (*expr_p);
	  internal_error ("gimplification failed");
	}
#endif
      gcc_assert (fallback & fb_mayfail);

      /* If this is an asm statement, and the user asked for the
	 impossible, don't die.  Fail and let gimplify_asm_expr
	 issue an error.  */
      ret = GS_ERROR;
      goto out;
    }

  /* Make sure the temporary matches our predicate.  */
  gcc_assert ((*gimple_test_f) (*expr_p));

  if (!gimple_seq_empty_p (internal_post))
    {
      annotate_all_with_location (internal_post, input_location);
      gimplify_seq_add_seq (pre_p, internal_post);
    }

 out:
  input_location = saved_location;
  return ret;
}

/* Like gimplify_expr but make sure the gimplified result is not itself
   a SSA name (but a decl if it were).  Temporaries required by
   evaluating *EXPR_P may be still SSA names.  */

static enum gimplify_status
gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
	       bool (*gimple_test_f) (tree), fallback_t fallback,
	       bool allow_ssa)
{
  bool was_ssa_name_p = TREE_CODE (*expr_p) == SSA_NAME;
  enum gimplify_status ret = gimplify_expr (expr_p, pre_p, post_p,
					    gimple_test_f, fallback);
  if (! allow_ssa
      && TREE_CODE (*expr_p) == SSA_NAME)
    {
      tree name = *expr_p;
      if (was_ssa_name_p)
	*expr_p = get_initialized_tmp_var (*expr_p, pre_p, NULL, false);
      else
	{
	  /* Avoid the extra copy if possible.  */
	  *expr_p = create_tmp_reg (TREE_TYPE (name));
	  gimple_set_lhs (SSA_NAME_DEF_STMT (name), *expr_p);
	  release_ssa_name (name);
	}
    }
  return ret;
}

/* Look through TYPE for variable-sized objects and gimplify each such
   size that we find.  Add to LIST_P any statements generated.  */

void
gimplify_type_sizes (tree type, gimple_seq *list_p)
{
  tree field, t;

  if (type == NULL || type == error_mark_node)
    return;

  /* We first do the main variant, then copy into any other variants.  */
  type = TYPE_MAIN_VARIANT (type);

  /* Avoid infinite recursion.  */
  if (TYPE_SIZES_GIMPLIFIED (type))
    return;

  TYPE_SIZES_GIMPLIFIED (type) = 1;

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
      gimplify_one_sizepos (&TYPE_MIN_VALUE (type), list_p);
      gimplify_one_sizepos (&TYPE_MAX_VALUE (type), list_p);

      for (t = TYPE_NEXT_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	{
	  TYPE_MIN_VALUE (t) = TYPE_MIN_VALUE (type);
	  TYPE_MAX_VALUE (t) = TYPE_MAX_VALUE (type);
	}
      break;

    case ARRAY_TYPE:
      /* These types may not have declarations, so handle them here.  */
      gimplify_type_sizes (TREE_TYPE (type), list_p);
      gimplify_type_sizes (TYPE_DOMAIN (type), list_p);
      /* Ensure VLA bounds aren't removed, for -O0 they should be variables
	 with assigned stack slots, for -O1+ -g they should be tracked
	 by VTA.  */
      if (!(TYPE_NAME (type)
	    && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	    && DECL_IGNORED_P (TYPE_NAME (type)))
	  && TYPE_DOMAIN (type)
	  && INTEGRAL_TYPE_P (TYPE_DOMAIN (type)))
	{
	  t = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	  if (t && VAR_P (t) && DECL_ARTIFICIAL (t))
	    DECL_IGNORED_P (t) = 0;
	  t = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	  if (t && VAR_P (t) && DECL_ARTIFICIAL (t))
	    DECL_IGNORED_P (t) = 0;
	}
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    gimplify_one_sizepos (&DECL_FIELD_OFFSET (field), list_p);
	    gimplify_one_sizepos (&DECL_SIZE (field), list_p);
	    gimplify_one_sizepos (&DECL_SIZE_UNIT (field), list_p);
	    gimplify_type_sizes (TREE_TYPE (field), list_p);
	  }
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
	/* We used to recurse on the pointed-to type here, which turned out to
	   be incorrect because its definition might refer to variables not
	   yet initialized at this point if a forward declaration is involved.

	   It was actually useful for anonymous pointed-to types to ensure
	   that the sizes evaluation dominates every possible later use of the
	   values.  Restricting to such types here would be safe since there
	   is no possible forward declaration around, but would introduce an
	   undesirable middle-end semantic to anonymity.  We then defer to
	   front-ends the responsibility of ensuring that the sizes are
	   evaluated both early and late enough, e.g. by attaching artificial
	   type declarations to the tree.  */
      break;

    default:
      break;
    }

  gimplify_one_sizepos (&TYPE_SIZE (type), list_p);
  gimplify_one_sizepos (&TYPE_SIZE_UNIT (type), list_p);

  for (t = TYPE_NEXT_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    {
      TYPE_SIZE (t) = TYPE_SIZE (type);
      TYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (type);
      TYPE_SIZES_GIMPLIFIED (t) = 1;
    }
}

/* A subroutine of gimplify_type_sizes to make sure that *EXPR_P,
   a size or position, has had all of its SAVE_EXPRs evaluated.
   We add any required statements to *STMT_P.  */

void
gimplify_one_sizepos (tree *expr_p, gimple_seq *stmt_p)
{
  tree expr = *expr_p;

  /* We don't do anything if the value isn't there, is constant, or contains
     A PLACEHOLDER_EXPR.  We also don't want to do anything if it's already
     a VAR_DECL.  If it's a VAR_DECL from another function, the gimplifier
     will want to replace it with a new variable, but that will cause problems
     if this type is from outside the function.  It's OK to have that here.  */
  if (expr == NULL_TREE
      || is_gimple_constant (expr)
      || TREE_CODE (expr) == VAR_DECL
      || CONTAINS_PLACEHOLDER_P (expr))
    return;

  *expr_p = unshare_expr (expr);

  /* SSA names in decl/type fields are a bad idea - they'll get reclaimed
     if the def vanishes.  */
  gimplify_expr (expr_p, stmt_p, NULL, is_gimple_val, fb_rvalue, false);

  /* If expr wasn't already is_gimple_sizepos or is_gimple_constant from the
     FE, ensure that it is a VAR_DECL, otherwise we might handle some decls
     as gimplify_vla_decl even when they would have all sizes INTEGER_CSTs.  */
  if (is_gimple_constant (*expr_p))
    *expr_p = get_initialized_tmp_var (*expr_p, stmt_p, NULL, false);
}

/* Gimplify the body of statements of FNDECL and return a GIMPLE_BIND node
   containing the sequence of corresponding GIMPLE statements.  If DO_PARMS
   is true, also gimplify the parameters.  */

gbind *
gimplify_body (tree fndecl, bool do_parms)
{
  location_t saved_location = input_location;
  gimple_seq parm_stmts, parm_cleanup = NULL, seq;
  gimple *outer_stmt;
  gbind *outer_bind;
  struct cgraph_node *cgn;

  timevar_push (TV_TREE_GIMPLIFY);

  init_tree_ssa (cfun);

  /* Initialize for optimize_insn_for_s{ize,peed}_p possibly called during
     gimplification.  */
  default_rtl_profile ();

  gcc_assert (gimplify_ctxp == NULL);
  push_gimplify_context (true);

  if (flag_openacc || flag_openmp)
    {
      gcc_assert (gimplify_omp_ctxp == NULL);
      if (lookup_attribute ("omp declare target", DECL_ATTRIBUTES (fndecl)))
	gimplify_omp_ctxp = new_omp_context (ORT_TARGET);
    }

  /* Unshare most shared trees in the body and in that of any nested functions.
     It would seem we don't have to do this for nested functions because
     they are supposed to be output and then the outer function gimplified
     first, but the g++ front end doesn't always do it that way.  */
  unshare_body (fndecl);
  unvisit_body (fndecl);

  cgn = cgraph_node::get (fndecl);
  if (cgn && cgn->origin)
    nonlocal_vlas = new hash_set<tree>;

  /* Make sure input_location isn't set to something weird.  */
  input_location = DECL_SOURCE_LOCATION (fndecl);

  /* Resolve callee-copies.  This has to be done before processing
     the body so that DECL_VALUE_EXPR gets processed correctly.  */
  parm_stmts = do_parms ? gimplify_parameters (&parm_cleanup) : NULL;

  /* Gimplify the function's body.  */
  seq = NULL;
  gimplify_stmt (&DECL_SAVED_TREE (fndecl), &seq);
  outer_stmt = gimple_seq_first_stmt (seq);
  if (!outer_stmt)
    {
      outer_stmt = gimple_build_nop ();
      gimplify_seq_add_stmt (&seq, outer_stmt);
    }

  /* The body must contain exactly one statement, a GIMPLE_BIND.  If this is
     not the case, wrap everything in a GIMPLE_BIND to make it so.  */
  if (gimple_code (outer_stmt) == GIMPLE_BIND
      && gimple_seq_first (seq) == gimple_seq_last (seq))
    outer_bind = as_a <gbind *> (outer_stmt);
  else
    outer_bind = gimple_build_bind (NULL_TREE, seq, NULL);

  DECL_SAVED_TREE (fndecl) = NULL_TREE;

  /* If we had callee-copies statements, insert them at the beginning
     of the function and clear DECL_VALUE_EXPR_P on the parameters.  */
  if (!gimple_seq_empty_p (parm_stmts))
    {
      tree parm;

      gimplify_seq_add_seq (&parm_stmts, gimple_bind_body (outer_bind));
      if (parm_cleanup)
	{
	  gtry *g = gimple_build_try (parm_stmts, parm_cleanup,
				      GIMPLE_TRY_FINALLY);
	  parm_stmts = NULL;
	  gimple_seq_add_stmt (&parm_stmts, g);
	}
      gimple_bind_set_body (outer_bind, parm_stmts);

      for (parm = DECL_ARGUMENTS (current_function_decl);
	   parm; parm = DECL_CHAIN (parm))
	if (DECL_HAS_VALUE_EXPR_P (parm))
	  {
	    DECL_HAS_VALUE_EXPR_P (parm) = 0;
	    DECL_IGNORED_P (parm) = 0;
	  }
    }

  if (nonlocal_vlas)
    {
      if (nonlocal_vla_vars)
	{
	  /* tree-nested.c may later on call declare_vars (..., true);
	     which relies on BLOCK_VARS chain to be the tail of the
	     gimple_bind_vars chain.  Ensure we don't violate that
	     assumption.  */
	  if (gimple_bind_block (outer_bind)
	      == DECL_INITIAL (current_function_decl))
	    declare_vars (nonlocal_vla_vars, outer_bind, true);
	  else
	    BLOCK_VARS (DECL_INITIAL (current_function_decl))
	      = chainon (BLOCK_VARS (DECL_INITIAL (current_function_decl)),
			 nonlocal_vla_vars);
	  nonlocal_vla_vars = NULL_TREE;
	}
      delete nonlocal_vlas;
      nonlocal_vlas = NULL;
    }

  if ((flag_openacc || flag_openmp || flag_openmp_simd)
      && gimplify_omp_ctxp)
    {
      delete_omp_context (gimplify_omp_ctxp);
      gimplify_omp_ctxp = NULL;
    }

  pop_gimplify_context (outer_bind);
  gcc_assert (gimplify_ctxp == NULL);

  if (flag_checking && !seen_error ())
    verify_gimple_in_seq (gimple_bind_body (outer_bind));

  timevar_pop (TV_TREE_GIMPLIFY);
  input_location = saved_location;

  return outer_bind;
}

typedef char *char_p; /* For DEF_VEC_P.  */

/* Return whether we should exclude FNDECL from instrumentation.  */

static bool
flag_instrument_functions_exclude_p (tree fndecl)
{
  vec<char_p> *v;

  v = (vec<char_p> *) flag_instrument_functions_exclude_functions;
  if (v && v->length () > 0)
    {
      const char *name;
      int i;
      char *s;

      name = lang_hooks.decl_printable_name (fndecl, 0);
      FOR_EACH_VEC_ELT (*v, i, s)
	if (strstr (name, s) != NULL)
	  return true;
    }

  v = (vec<char_p> *) flag_instrument_functions_exclude_files;
  if (v && v->length () > 0)
    {
      const char *name;
      int i;
      char *s;

      name = DECL_SOURCE_FILE (fndecl);
      FOR_EACH_VEC_ELT (*v, i, s)
	if (strstr (name, s) != NULL)
	  return true;
    }

  return false;
}

/* Entry point to the gimplification pass.  FNDECL is the FUNCTION_DECL
   node for the function we want to gimplify.

   Return the sequence of GIMPLE statements corresponding to the body
   of FNDECL.  */

void
gimplify_function_tree (tree fndecl)
{
  tree parm, ret;
  gimple_seq seq;
  gbind *bind;

  gcc_assert (!gimple_body (fndecl));

  if (DECL_STRUCT_FUNCTION (fndecl))
    push_cfun (DECL_STRUCT_FUNCTION (fndecl));
  else
    push_struct_function (fndecl);

  /* Tentatively set PROP_gimple_lva here, and reset it in gimplify_va_arg_expr
     if necessary.  */
  cfun->curr_properties |= PROP_gimple_lva;

  for (parm = DECL_ARGUMENTS (fndecl); parm ; parm = DECL_CHAIN (parm))
    {
      /* Preliminarily mark non-addressed complex variables as eligible
         for promotion to gimple registers.  We'll transform their uses
         as we find them.  */
      if ((TREE_CODE (TREE_TYPE (parm)) == COMPLEX_TYPE
	   || TREE_CODE (TREE_TYPE (parm)) == VECTOR_TYPE)
          && !TREE_THIS_VOLATILE (parm)
          && !needs_to_live_in_memory (parm))
        DECL_GIMPLE_REG_P (parm) = 1;
    }

  ret = DECL_RESULT (fndecl);
  if ((TREE_CODE (TREE_TYPE (ret)) == COMPLEX_TYPE
       || TREE_CODE (TREE_TYPE (ret)) == VECTOR_TYPE)
      && !needs_to_live_in_memory (ret))
    DECL_GIMPLE_REG_P (ret) = 1;

  if (asan_sanitize_use_after_scope () && sanitize_flags_p (SANITIZE_ADDRESS))
    asan_poisoned_variables = new hash_set<tree> ();
  bind = gimplify_body (fndecl, true);
  if (asan_poisoned_variables)
    {
      delete asan_poisoned_variables;
      asan_poisoned_variables = NULL;
    }

  /* The tree body of the function is no longer needed, replace it
     with the new GIMPLE body.  */
  seq = NULL;
  gimple_seq_add_stmt (&seq, bind);
  gimple_set_body (fndecl, seq);

  /* If we're instrumenting function entry/exit, then prepend the call to
     the entry hook and wrap the whole function in a TRY_FINALLY_EXPR to
     catch the exit hook.  */
  /* ??? Add some way to ignore exceptions for this TFE.  */
  if (flag_instrument_function_entry_exit
      && !DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (fndecl)
      /* Do not instrument extern inline functions.  */
      && !(DECL_DECLARED_INLINE_P (fndecl)
	   && DECL_EXTERNAL (fndecl)
	   && DECL_DISREGARD_INLINE_LIMITS (fndecl))
      && !flag_instrument_functions_exclude_p (fndecl))
    {
      tree x;
      gbind *new_bind;
      gimple *tf;
      gimple_seq cleanup = NULL, body = NULL;
      tree tmp_var;
      gcall *call;

      x = builtin_decl_implicit (BUILT_IN_RETURN_ADDRESS);
      call = gimple_build_call (x, 1, integer_zero_node);
      tmp_var = create_tmp_var (ptr_type_node, "return_addr");
      gimple_call_set_lhs (call, tmp_var);
      gimplify_seq_add_stmt (&cleanup, call);
      x = builtin_decl_implicit (BUILT_IN_PROFILE_FUNC_EXIT);
      call = gimple_build_call (x, 2,
				build_fold_addr_expr (current_function_decl),
				tmp_var);
      gimplify_seq_add_stmt (&cleanup, call);
      tf = gimple_build_try (seq, cleanup, GIMPLE_TRY_FINALLY);

      x = builtin_decl_implicit (BUILT_IN_RETURN_ADDRESS);
      call = gimple_build_call (x, 1, integer_zero_node);
      tmp_var = create_tmp_var (ptr_type_node, "return_addr");
      gimple_call_set_lhs (call, tmp_var);
      gimplify_seq_add_stmt (&body, call);
      x = builtin_decl_implicit (BUILT_IN_PROFILE_FUNC_ENTER);
      call = gimple_build_call (x, 2,
				build_fold_addr_expr (current_function_decl),
				tmp_var);
      gimplify_seq_add_stmt (&body, call);
      gimplify_seq_add_stmt (&body, tf);
      new_bind = gimple_build_bind (NULL, body, NULL);

      /* Replace the current function body with the body
         wrapped in the try/finally TF.  */
      seq = NULL;
      gimple_seq_add_stmt (&seq, new_bind);
      gimple_set_body (fndecl, seq);
      bind = new_bind;
    }

  if (sanitize_flags_p (SANITIZE_THREAD))
    {
      gcall *call = gimple_build_call_internal (IFN_TSAN_FUNC_EXIT, 0);
      gimple *tf = gimple_build_try (seq, call, GIMPLE_TRY_FINALLY);
      gbind *new_bind = gimple_build_bind (NULL, tf, NULL);
      /* Replace the current function body with the body
	 wrapped in the try/finally TF.  */
      seq = NULL;
      gimple_seq_add_stmt (&seq, new_bind);
      gimple_set_body (fndecl, seq);
    }

  DECL_SAVED_TREE (fndecl) = NULL_TREE;
  cfun->curr_properties |= PROP_gimple_any;

  pop_cfun ();

  dump_function (TDI_gimple, fndecl);
}

/* Return a dummy expression of type TYPE in order to keep going after an
   error.  */

static tree
dummy_object (tree type)
{
  tree t = build_int_cst (build_pointer_type (type), 0);
  return build2 (MEM_REF, type, t, t);
}

/* Gimplify __builtin_va_arg, aka VA_ARG_EXPR, which is not really a
   builtin function, but a very special sort of operator.  */

enum gimplify_status
gimplify_va_arg_expr (tree *expr_p, gimple_seq *pre_p,
		      gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree promoted_type, have_va_type;
  tree valist = TREE_OPERAND (*expr_p, 0);
  tree type = TREE_TYPE (*expr_p);
  tree t, tag, aptag;
  location_t loc = EXPR_LOCATION (*expr_p);

  /* Verify that valist is of the proper type.  */
  have_va_type = TREE_TYPE (valist);
  if (have_va_type == error_mark_node)
    return GS_ERROR;
  have_va_type = targetm.canonical_va_list_type (have_va_type);
  if (have_va_type == NULL_TREE
      && POINTER_TYPE_P (TREE_TYPE (valist)))
    /* Handle 'Case 1: Not an array type' from c-common.c/build_va_arg.  */
    have_va_type
      = targetm.canonical_va_list_type (TREE_TYPE (TREE_TYPE (valist)));
  gcc_assert (have_va_type != NULL_TREE);

  /* Generate a diagnostic for requesting data of a type that cannot
     be passed through `...' due to type promotion at the call site.  */
  if ((promoted_type = lang_hooks.types.type_promotes_to (type))
	   != type)
    {
      static bool gave_help;
      bool warned;
      /* Use the expansion point to handle cases such as passing bool (defined
	 in a system header) through `...'.  */
      source_location xloc
	= expansion_point_location_if_in_system_header (loc);

      /* Unfortunately, this is merely undefined, rather than a constraint
	 violation, so we cannot make this an error.  If this call is never
	 executed, the program is still strictly conforming.  */
      warned = warning_at (xloc, 0,
			   "%qT is promoted to %qT when passed through %<...%>",
			   type, promoted_type);
      if (!gave_help && warned)
	{
	  gave_help = true;
	  inform (xloc, "(so you should pass %qT not %qT to %<va_arg%>)",
		  promoted_type, type);
	}

      /* We can, however, treat "undefined" any way we please.
	 Call abort to encourage the user to fix the program.  */
      if (warned)
	inform (xloc, "if this code is reached, the program will abort");
      /* Before the abort, allow the evaluation of the va_list
	 expression to exit or longjmp.  */
      gimplify_and_add (valist, pre_p);
      t = build_call_expr_loc (loc,
			       builtin_decl_implicit (BUILT_IN_TRAP), 0);
      gimplify_and_add (t, pre_p);

      /* This is dead code, but go ahead and finish so that the
	 mode of the result comes out right.  */
      *expr_p = dummy_object (type);
      return GS_ALL_DONE;
    }

  tag = build_int_cst (build_pointer_type (type), 0);
  aptag = build_int_cst (TREE_TYPE (valist), 0);

  *expr_p = build_call_expr_internal_loc (loc, IFN_VA_ARG, type, 3,
					  valist, tag, aptag);

  /* Clear the tentatively set PROP_gimple_lva, to indicate that IFN_VA_ARG
     needs to be expanded.  */
  cfun->curr_properties &= ~PROP_gimple_lva;

  return GS_OK;
}

/* Build a new GIMPLE_ASSIGN tuple and append it to the end of *SEQ_P.

   DST/SRC are the destination and source respectively.  You can pass
   ungimplified trees in DST or SRC, in which case they will be
   converted to a gimple operand if necessary.

   This function returns the newly created GIMPLE_ASSIGN tuple.  */

gimple *
gimplify_assign (tree dst, tree src, gimple_seq *seq_p)
{
  tree t = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
  gimplify_and_add (t, seq_p);
  ggc_free (t);
  return gimple_seq_last_stmt (*seq_p);
}

inline hashval_t
gimplify_hasher::hash (const elt_t *p)
{
  tree t = p->val;
  return iterative_hash_expr (t, 0);
}

inline bool
gimplify_hasher::equal (const elt_t *p1, const elt_t *p2)
{
  tree t1 = p1->val;
  tree t2 = p2->val;
  enum tree_code code = TREE_CODE (t1);

  if (TREE_CODE (t2) != code
      || TREE_TYPE (t1) != TREE_TYPE (t2))
    return false;

  if (!operand_equal_p (t1, t2, 0))
    return false;

  /* Only allow them to compare equal if they also hash equal; otherwise
     results are nondeterminate, and we fail bootstrap comparison.  */
  gcc_checking_assert (hash (p1) == hash (p2));

  return true;
}
