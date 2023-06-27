/* Tree lowering pass.  This pass converts the GENERIC functions-as-trees
   tree representation into the GIMPLE form.
   Copyright (C) 2002-2023 Free Software Foundation, Inc.
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
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-hash-traits.h"
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
#include "omp-offload.h"
#include "context.h"
#include "tree-nested.h"

/* Hash set of poisoned variables in a bind expr.  */
static hash_set<tree> *asan_poisoned_variables = NULL;

enum gimplify_omp_var_data
{
  GOVD_SEEN = 0x000001,
  GOVD_EXPLICIT = 0x000002,
  GOVD_SHARED = 0x000004,
  GOVD_PRIVATE = 0x000008,
  GOVD_FIRSTPRIVATE = 0x000010,
  GOVD_LASTPRIVATE = 0x000020,
  GOVD_REDUCTION = 0x000040,
  GOVD_LOCAL = 0x00080,
  GOVD_MAP = 0x000100,
  GOVD_DEBUG_PRIVATE = 0x000200,
  GOVD_PRIVATE_OUTER_REF = 0x000400,
  GOVD_LINEAR = 0x000800,
  GOVD_ALIGNED = 0x001000,

  /* Flag for GOVD_MAP: don't copy back.  */
  GOVD_MAP_TO_ONLY = 0x002000,

  /* Flag for GOVD_LINEAR or GOVD_LASTPRIVATE: no outer reference.  */
  GOVD_LINEAR_LASTPRIVATE_NO_OUTER = 0x004000,

  GOVD_MAP_0LEN_ARRAY = 0x008000,

  /* Flag for GOVD_MAP, if it is always, to or always, tofrom mapping.  */
  GOVD_MAP_ALWAYS_TO = 0x010000,

  /* Flag for shared vars that are or might be stored to in the region.  */
  GOVD_WRITTEN = 0x020000,

  /* Flag for GOVD_MAP, if it is a forced mapping.  */
  GOVD_MAP_FORCE = 0x040000,

  /* Flag for GOVD_MAP: must be present already.  */
  GOVD_MAP_FORCE_PRESENT = 0x080000,

  /* Flag for GOVD_MAP: only allocate.  */
  GOVD_MAP_ALLOC_ONLY = 0x100000,

  /* Flag for GOVD_MAP: only copy back.  */
  GOVD_MAP_FROM_ONLY = 0x200000,

  GOVD_NONTEMPORAL = 0x400000,

  /* Flag for GOVD_LASTPRIVATE: conditional modifier.  */
  GOVD_LASTPRIVATE_CONDITIONAL = 0x800000,

  GOVD_CONDTEMP = 0x1000000,

  /* Flag for GOVD_REDUCTION: inscan seen in {in,ex}clusive clause.  */
  GOVD_REDUCTION_INSCAN = 0x2000000,

  /* Flag for GOVD_FIRSTPRIVATE: OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT.  */
  GOVD_FIRSTPRIVATE_IMPLICIT = 0x4000000,

  GOVD_DATA_SHARE_CLASS = (GOVD_SHARED | GOVD_PRIVATE | GOVD_FIRSTPRIVATE
			   | GOVD_LASTPRIVATE | GOVD_REDUCTION | GOVD_LINEAR
			   | GOVD_LOCAL)
};


enum omp_region_type
{
  ORT_WORKSHARE = 0x00,
  ORT_TASKGROUP = 0x01,
  ORT_SIMD 	= 0x04,

  ORT_PARALLEL	= 0x08,
  ORT_COMBINED_PARALLEL = ORT_PARALLEL | 1,

  ORT_TASK	= 0x10,
  ORT_UNTIED_TASK = ORT_TASK | 1,
  ORT_TASKLOOP  = ORT_TASK | 2,
  ORT_UNTIED_TASKLOOP = ORT_UNTIED_TASK | 2,

  ORT_TEAMS	= 0x20,
  ORT_COMBINED_TEAMS = ORT_TEAMS | 1,
  ORT_HOST_TEAMS = ORT_TEAMS | 2,
  ORT_COMBINED_HOST_TEAMS = ORT_COMBINED_TEAMS | 2,

  /* Data region.  */
  ORT_TARGET_DATA = 0x40,

  /* Data region with offloading.  */
  ORT_TARGET	= 0x80,
  ORT_COMBINED_TARGET = ORT_TARGET | 1,
  ORT_IMPLICIT_TARGET = ORT_TARGET | 2,

  /* OpenACC variants.  */
  ORT_ACC	= 0x100,  /* A generic OpenACC region.  */
  ORT_ACC_DATA	= ORT_ACC | ORT_TARGET_DATA, /* Data construct.  */
  ORT_ACC_PARALLEL = ORT_ACC | ORT_TARGET,  /* Parallel construct */
  ORT_ACC_KERNELS  = ORT_ACC | ORT_TARGET | 2,  /* Kernels construct.  */
  ORT_ACC_SERIAL   = ORT_ACC | ORT_TARGET | 4,  /* Serial construct.  */
  ORT_ACC_HOST_DATA = ORT_ACC | ORT_TARGET_DATA | 2,  /* Host data.  */

  /* Dummy OpenMP region, used to disable expansion of
     DECL_VALUE_EXPRs in taskloop pre body.  */
  ORT_NONE	= 0x200
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

enum gimplify_defaultmap_kind
{
  GDMK_SCALAR,
  GDMK_SCALAR_TARGET, /* w/ Fortran's target attr, implicit mapping, only.  */
  GDMK_AGGREGATE,
  GDMK_ALLOCATABLE,
  GDMK_POINTER
};

struct gimplify_omp_ctx
{
  struct gimplify_omp_ctx *outer_context;
  splay_tree variables;
  hash_set<tree> *privatized_types;
  tree clauses;
  /* Iteration variables in an OMP_FOR.  */
  vec<tree> loop_iter_var;
  location_t location;
  enum omp_clause_default_kind default_kind;
  enum omp_region_type region_type;
  enum tree_code code;
  bool combined_loop;
  bool distribute;
  bool target_firstprivatize_array_bases;
  bool add_safelen1;
  bool order_concurrent;
  bool has_depend;
  bool in_for_exprs;
  int defaultmap[5];
};

static struct gimplify_ctx *gimplify_ctxp;
static struct gimplify_omp_ctx *gimplify_omp_ctxp;
static bool in_omp_construct;

/* Forward declaration.  */
static enum gimplify_status gimplify_compound_expr (tree *, gimple_seq *, bool);
static hash_map<tree, tree> *oacc_declare_returns;
static enum gimplify_status gimplify_expr (tree *, gimple_seq *, gimple_seq *,
					   bool (*) (tree), fallback_t, bool);
static void prepare_gimple_addressable (tree *, gimple_seq *);

/* Shorter alias name for the above function for use in gimplify.cc
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
  c->defaultmap[GDMK_SCALAR] = GOVD_MAP;
  c->defaultmap[GDMK_SCALAR_TARGET] = GOVD_MAP;
  c->defaultmap[GDMK_AGGREGATE] = GOVD_MAP;
  c->defaultmap[GDMK_ALLOCATABLE] = GOVD_MAP;
  c->defaultmap[GDMK_POINTER] = GOVD_MAP;

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
  return var;
}

/* Create a temporary to hold the value of VAL.  If IS_FORMAL, try to reuse
   an existing expression temporary.  If NOT_GIMPLE_REG, mark it as such.  */

static tree
lookup_tmp_var (tree val, bool is_formal, bool not_gimple_reg)
{
  tree ret;

  /* We cannot mark a formal temporary with DECL_NOT_GIMPLE_REG_P.  */
  gcc_assert (!is_formal || !not_gimple_reg);

  /* If not optimizing, never really reuse a temporary.  local-alloc
     won't allocate any variable that is used in more than one basic
     block, which means it will go into memory, causing much extra
     work in reload and final and poorer code generation, outweighing
     the extra memory allocation here.  */
  if (!optimize || !is_formal || TREE_SIDE_EFFECTS (val))
    {
      ret = create_tmp_from_val (val);
      DECL_NOT_GIMPLE_REG_P (ret) = not_gimple_reg;
    }
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
		      bool is_formal, bool allow_ssa, bool not_gimple_reg)
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
    t = lookup_tmp_var (val, is_formal, not_gimple_reg);

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
  return internal_get_tmp_var (val, pre_p, NULL, true, true, false);
}

/* Return a temporary variable initialized with VAL.  PRE_P and POST_P
   are as in gimplify_expr.  */

tree
get_initialized_tmp_var (tree val, gimple_seq *pre_p,
			 gimple_seq *post_p /* = NULL */,
			 bool allow_ssa /* = true */)
{
  return internal_get_tmp_var (val, pre_p, post_p, false, allow_ssa, false);
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
	  int flag = GOVD_LOCAL | GOVD_SEEN;
	  while (ctx
		 && (ctx->region_type == ORT_WORKSHARE
		     || ctx->region_type == ORT_TASKGROUP
		     || ctx->region_type == ORT_SIMD
		     || ctx->region_type == ORT_ACC))
	    {
	      if (ctx->region_type == ORT_SIMD
		  && TREE_ADDRESSABLE (tmp)
		  && !TREE_STATIC (tmp))
		{
		  if (TREE_CODE (DECL_SIZE_UNIT (tmp)) != INTEGER_CST)
		    ctx->add_safelen1 = true;
		  else if (ctx->in_for_exprs)
		    flag = GOVD_PRIVATE;
		  else
		    flag = GOVD_PRIVATE | GOVD_SEEN;
		  break;
		}
	      ctx = ctx->outer_context;
	    }
	  if (ctx)
	    omp_add_variable (ctx, tmp, flag);
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

void
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
    for (cgn = first_nested_function (cgn); cgn;
	 cgn = next_nested_function (cgn))
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
    for (cgn = first_nested_function (cgn);
	 cgn; cgn = next_nested_function (cgn))
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
  gcc_assert (!hwasan_sanitize_p () || hwasan_sanitize_stack_p ());
  unsigned shadow_granularity
    = hwasan_sanitize_p () ? HWASAN_TAG_GRANULE_SIZE : ASAN_SHADOW_GRANULARITY;
  if (DECL_ALIGN_UNIT (decl) <= shadow_granularity)
    SET_DECL_ALIGN (decl, BITS_PER_UNIT * shadow_granularity);

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
	  if (ctx && ctx->region_type != ORT_NONE && !DECL_EXTERNAL (t))
	    {
	      if (! DECL_SEEN_IN_BIND_EXPR_P (t)
		  || splay_tree_lookup (ctx->variables,
					(splay_tree_key) t) == NULL)
		{
		  int flag = GOVD_LOCAL;
		  if (ctx->region_type == ORT_SIMD
		      && TREE_ADDRESSABLE (t)
		      && !TREE_STATIC (t))
		    {
		      if (TREE_CODE (DECL_SIZE_UNIT (t)) != INTEGER_CST)
			ctx->add_safelen1 = true;
		      else
			flag = GOVD_PRIVATE;
		    }
		  omp_add_variable (ctx, t, flag | GOVD_SEEN);
		}
	      /* Static locals inside of target construct or offloaded
		 routines need to be "omp declare target".  */
	      if (TREE_STATIC (t))
		for (; ctx; ctx = ctx->outer_context)
		  if ((ctx->region_type & ORT_TARGET) != 0)
		    {
		      if (!lookup_attribute ("omp declare target",
					     DECL_ATTRIBUTES (t)))
			{
			  tree id = get_identifier ("omp declare target");
			  DECL_ATTRIBUTES (t)
			    = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (t));
			  varpool_node *node = varpool_node::get (t);
			  if (node)
			    {
			      node->offloadable = 1;
			      if (ENABLE_OFFLOADING && !DECL_EXTERNAL (t))
				{
				  g->have_offload = true;
				  if (!in_lto_p)
				    vec_safe_push (offload_vars, t);
				}
			    }
			}
		      break;
		    }
	    }

	  DECL_SEEN_IN_BIND_EXPR_P (t) = 1;

	  if (DECL_HARD_REGISTER (t) && !is_global_var (t) && cfun)
	    cfun->has_local_explicit_reg_vars = true;
	}
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
	      tree clobber = build_clobber (TREE_TYPE (t), CLOBBER_EOL);
	      gimple *clobber_stmt;
	      clobber_stmt = gimple_build_assign (t, clobber);
	      gimple_set_location (clobber_stmt, end_locus);
	      gimplify_seq_add_stmt (&cleanup, clobber_stmt);
	    }

	  if (flag_openacc && oacc_declare_returns != NULL)
	    {
	      tree key = t;
	      if (DECL_HAS_VALUE_EXPR_P (key))
		{
		  key = DECL_VALUE_EXPR (key);
		  if (TREE_CODE (key) == INDIRECT_REF)
		    key = TREE_OPERAND (key, 0);
		}
	      tree *c = oacc_declare_returns->get (key);
	      if (c != NULL)
		{
		  if (ret_clauses)
		    OMP_CLAUSE_CHAIN (*c) = ret_clauses;

		  ret_clauses = unshare_expr (*c);

		  oacc_declare_returns->remove (key);

		  if (oacc_declare_returns->is_empty ())
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
      copy_warning (ret, stmt);
      gimplify_seq_add_stmt (pre_p, ret);
      return GS_ALL_DONE;
    }

  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))))
    result_decl = NULL_TREE;
  else if (TREE_CODE (ret_expr) == COMPOUND_EXPR)
    {
      /* Used in C++ for handling EH cleanup of the return value if a local
	 cleanup throws.  Assume the front-end knows what it's doing.  */
      result_decl = DECL_RESULT (current_function_decl);
      /* But crash if we end up trying to modify ret_expr below.  */
      ret_expr = NULL_TREE;
    }
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
      if (!poly_int_tree_p (DECL_SIZE (result_decl)))
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
      suppress_warning (result, OPT_Wuninitialized);

      gimplify_ctxp->return_temp = result;
    }

  /* Smash the lhs of the MODIFY_EXPR to the temporary we plan to use.
     Then gimplify the whole thing.  */
  if (result != result_decl)
    TREE_OPERAND (ret_expr, 0) = result;

  gimplify_and_add (TREE_OPERAND (stmt, 0), pre_p);

  maybe_add_early_return_predict_stmt (pre_p);
  ret = gimple_build_return (result);
  copy_warning (ret, stmt);
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

  /* Record the dynamic allocation associated with DECL if requested.  */
  if (flag_callgraph_info & CALLGRAPH_INFO_DYNAMIC_ALLOC)
    record_dynamic_alloc (decl);
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

/* Generate an initialization to automatic variable DECL based on INIT_TYPE.
   Build a call to internal const function DEFERRED_INIT:
   1st argument: SIZE of the DECL;
   2nd argument: INIT_TYPE;
   3rd argument: NAME of the DECL;

   as LHS = DEFERRED_INIT (SIZE of the DECL, INIT_TYPE, NAME of the DECL).  */

static void
gimple_add_init_for_auto_var (tree decl,
			      enum auto_init_type init_type,
			      gimple_seq *seq_p)
{
  gcc_assert (auto_var_p (decl));
  gcc_assert (init_type > AUTO_INIT_UNINITIALIZED);
  location_t loc = EXPR_LOCATION (decl);
  tree decl_size = TYPE_SIZE_UNIT (TREE_TYPE (decl));

  tree init_type_node
    = build_int_cst (integer_type_node, (int) init_type);

  tree decl_name = NULL_TREE;
  if (DECL_NAME (decl))

    decl_name = build_string_literal (DECL_NAME (decl));

  else
    {
      char decl_name_anonymous[3 + (HOST_BITS_PER_INT + 2) / 3];
      sprintf (decl_name_anonymous, "D.%u", DECL_UID (decl));
      decl_name = build_string_literal (decl_name_anonymous);
    }

  tree call = build_call_expr_internal_loc (loc, IFN_DEFERRED_INIT,
		 			    TREE_TYPE (decl), 3,
					    decl_size, init_type_node,
					    decl_name);

  gimplify_assign (decl, call, seq_p);
}

/* Generate padding initialization for automatic vairable DECL.
   C guarantees that brace-init with fewer initializers than members
   aggregate will initialize the rest of the aggregate as-if it were
   static initialization.  In turn static initialization guarantees
   that padding is initialized to zero. So, we always initialize paddings
   to zeroes regardless INIT_TYPE.
   To do the padding initialization, we insert a call to
   __builtin_clear_padding (&decl, 0, for_auto_init = true).
   Note, we add an additional dummy argument for __builtin_clear_padding,
   'for_auto_init' to distinguish whether this call is for automatic
   variable initialization or not.
   */
static void
gimple_add_padding_init_for_auto_var (tree decl, bool is_vla,
				      gimple_seq *seq_p)
{
  tree addr_of_decl = NULL_TREE;
  tree fn = builtin_decl_explicit (BUILT_IN_CLEAR_PADDING);

  if (is_vla)
    {
      /* The temporary address variable for this vla should be
	 created in gimplify_vla_decl.  */
      gcc_assert (DECL_HAS_VALUE_EXPR_P (decl));
      gcc_assert (TREE_CODE (DECL_VALUE_EXPR (decl)) == INDIRECT_REF);
      addr_of_decl = TREE_OPERAND (DECL_VALUE_EXPR (decl), 0);
    }
  else
    {
      mark_addressable (decl);
      addr_of_decl = build_fold_addr_expr (decl);
    }

  gimple *call = gimple_build_call (fn, 2, addr_of_decl,
				    build_one_cst (TREE_TYPE (addr_of_decl)));
  gimplify_seq_add_stmt (seq_p, call);
}

/* Return true if the DECL need to be automaticly initialized by the
   compiler.  */
static bool
is_var_need_auto_init (tree decl)
{
  if (auto_var_p (decl)
      && (TREE_CODE (decl) != VAR_DECL
	  || !DECL_HARD_REGISTER (decl))
      && (flag_auto_var_init > AUTO_INIT_UNINITIALIZED)
      && (!lookup_attribute ("uninitialized", DECL_ATTRIBUTES (decl)))
      && !OPAQUE_TYPE_P (TREE_TYPE (decl))
      && !is_empty_type (TREE_TYPE (decl)))
    return true;
  return false;
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
      /* Check whether a decl has FE created VALUE_EXPR here BEFORE
	 gimplify_vla_decl creates VALUE_EXPR for a vla decl.
	 If the decl has VALUE_EXPR that was created by FE (usually
	 C++FE), it's a proxy varaible, and FE already initialized
	 the VALUE_EXPR of it, we should not initialize it anymore.  */
      bool decl_had_value_expr_p = DECL_HAS_VALUE_EXPR_P (decl);

      poly_uint64 size;
      if (!poly_int_tree_p (DECL_SIZE_UNIT (decl), &size)
	  || (!TREE_STATIC (decl)
	      && flag_stack_check == GENERIC_STACK_CHECK
	      && maybe_gt (size,
			   (unsigned HOST_WIDE_INT) STACK_CHECK_MAX_VAR_SIZE)))
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
	  && !gimplify_omp_ctxp
	  /* GNAT introduces temporaries to hold return values of calls in
	     initializers of variables defined in other units, so the
	     declaration of the variable is discarded completely.  We do not
	     want to issue poison calls for such dropped variables.  */
	  && (DECL_SEEN_IN_BIND_EXPR_P (decl)
	      || (DECL_ARTIFICIAL (decl) && DECL_NAME (decl) == NULL_TREE)))
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
	      /* Clear TREE_READONLY if we really have an initialization.  */
	      if (!DECL_INITIAL (decl)
		  && !omp_privatize_by_reference (decl))
		TREE_READONLY (decl) = 0;
	    }
	  else
	    /* We must still examine initializers for static variables
	       as they may contain a label address.  */
	    walk_tree (&init, force_labels_r, NULL, NULL);
	}
      /* When there is no explicit initializer, if the user requested,
	 We should insert an artifical initializer for this automatic
	 variable.  */
      else if (is_var_need_auto_init (decl)
	       && !decl_had_value_expr_p)
	{
	  gimple_add_init_for_auto_var (decl,
					flag_auto_var_init,
					seq_p);
	  /* The expanding of a call to the above .DEFERRED_INIT will apply
	     block initialization to the whole space covered by this variable.
	     As a result, all the paddings will be initialized to zeroes
	     for zero initialization and 0xFE byte-repeatable patterns for
	     pattern initialization.
	     In order to make the paddings as zeroes for pattern init, We
	     should add a call to __builtin_clear_padding to clear the
	     paddings to zero in compatiple with CLANG.
	     We cannot insert this call if the variable is a gimple register
	     since __builtin_clear_padding will take the address of the
	     variable.  As a result, if a long double/_Complex long double
	     variable will spilled into stack later, its padding is 0XFE.  */
	  if (flag_auto_var_init == AUTO_INIT_PATTERN
	      && !is_gimple_reg (decl)
	      && clear_padding_type_may_have_padding_p (TREE_TYPE (decl)))
	    gimple_add_padding_init_for_auto_var (decl, is_vla, seq_p);
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


/* Emit warning for the unreachable statment STMT if needed.
   Return the gimple itself when the warning is emitted, otherwise
   return NULL.  */
static gimple *
emit_warn_switch_unreachable (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_GOTO
      && TREE_CODE (gimple_goto_dest (stmt)) == LABEL_DECL
      && DECL_ARTIFICIAL (gimple_goto_dest (stmt)))
  /* Don't warn for compiler-generated gotos.  These occur
     in Duff's devices, for example.  */
    return NULL;
  else if ((flag_auto_var_init > AUTO_INIT_UNINITIALIZED)
	   && ((gimple_call_internal_p (stmt, IFN_DEFERRED_INIT))
		|| (gimple_call_builtin_p (stmt, BUILT_IN_CLEAR_PADDING)
		    && (bool) TREE_INT_CST_LOW (gimple_call_arg (stmt, 1)))
		|| (is_gimple_assign (stmt)
		    && gimple_assign_single_p (stmt)
		    && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
		    && gimple_call_internal_p (
			 SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt)),
			 IFN_DEFERRED_INIT))))
  /* Don't warn for compiler-generated initializations for
     -ftrivial-auto-var-init.
     There are 3 cases:
	case 1: a call to .DEFERRED_INIT;
	case 2: a call to __builtin_clear_padding with the 2nd argument is
		present and non-zero;
	case 3: a gimple assign store right after the call to .DEFERRED_INIT
		that has the LHS of .DEFERRED_INIT as the RHS as following:
		  _1 = .DEFERRED_INIT (4, 2, &"i1"[0]);
		  i1 = _1.  */
    return NULL;
  else
    warning_at (gimple_location (stmt), OPT_Wswitch_unreachable,
		"statement will never be executed");
  return stmt;
}

/* Callback for walk_gimple_seq.  */

static tree
warn_switch_unreachable_and_auto_init_r (gimple_stmt_iterator *gsi_p,
					 bool *handled_ops_p,
					 struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  bool unreachable_issued = wi->info != NULL;

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    case GIMPLE_TRY:
      /* A compiler-generated cleanup or a user-written try block.
	 If it's empty, don't dive into it--that would result in
	 worse location info.  */
      if (gimple_try_eval (stmt) == NULL)
	{
	  if (warn_switch_unreachable && !unreachable_issued)
	    wi->info = emit_warn_switch_unreachable (stmt);

	  /* Stop when auto var init warning is not on.  */
	  if (!warn_trivial_auto_var_init)
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

    case GIMPLE_LABEL:
      /* Stop till the first Label.  */
      return integer_zero_node;
    case GIMPLE_CALL:
      if (gimple_call_internal_p (stmt, IFN_ASAN_MARK))
	{
	  *handled_ops_p = false;
	  break;
	}
      if (warn_trivial_auto_var_init
	  && flag_auto_var_init > AUTO_INIT_UNINITIALIZED
	  && gimple_call_internal_p (stmt, IFN_DEFERRED_INIT))
	{
	  /* Get the variable name from the 3rd argument of call.  */
	  tree var_name = gimple_call_arg (stmt, 2);
	  var_name = TREE_OPERAND (TREE_OPERAND (var_name, 0), 0);
	  const char *var_name_str = TREE_STRING_POINTER (var_name);

	  warning_at (gimple_location (stmt), OPT_Wtrivial_auto_var_init,
		      "%qs cannot be initialized with"
		      "%<-ftrivial-auto-var_init%>",
		      var_name_str);
	  break;
       }

      /* Fall through.  */
    default:
      /* check the first "real" statement (not a decl/lexical scope/...), issue
	 warning if needed.  */
      if (warn_switch_unreachable && !unreachable_issued)
	wi->info = emit_warn_switch_unreachable (stmt);
      /* Stop when auto var init warning is not on.  */
      if (!warn_trivial_auto_var_init)
	return integer_zero_node;
      break;
    }
  return NULL_TREE;
}


/* Possibly warn about unreachable statements between switch's controlling
   expression and the first case.  Also warn about -ftrivial-auto-var-init
   cannot initialize the auto variable under such situation.
   SEQ is the body of a switch expression.  */

static void
maybe_warn_switch_unreachable_and_auto_init (gimple_seq seq)
{
  if ((!warn_switch_unreachable && !warn_trivial_auto_var_init)
      /* This warning doesn't play well with Fortran when optimizations
	 are on.  */
      || lang_GNU_Fortran ()
      || seq == NULL)
    return;

  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq (seq, warn_switch_unreachable_and_auto_init_r, NULL, &wi);
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

/* Collect labels that may fall through into LABELS and return the statement
   preceding another case label, or a user-defined label.  Store a location
   useful to give warnings at *PREVLOC (usually the location of the returned
   statement or of its surrounding scope).  */

static gimple *
collect_fallthrough_labels (gimple_stmt_iterator *gsi_p,
			    auto_vec <struct label_entry> *labels,
			    location_t *prevloc)
{
  gimple *prev = NULL;

  *prevloc = UNKNOWN_LOCATION;
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
		*prevloc = bind_loc;
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

	  /* A dead label can't fall through.  */
	  if (!UNUSED_LABEL_P (false_lab))
	    {
	      struct label_entry l = { false_lab, if_loc };
	      labels->safe_push (l);
	    }

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
	  /* This case is about
	      if (1 != 0) goto <D.2022>; else goto <D.2023>;
	      <D.2022>:
	      n = n + 1; // #1
	      <D.2023>:  // #2
	      <D.1988>:  // #3
	     where #2 is UNUSED_LABEL_P and we want to warn about #1 falling
	     through to #3.  So set PREV to #1.  */
	  else if (UNUSED_LABEL_P (false_lab))
	    prev = gsi_stmt (*gsi_p);

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
      else if (gimple_code (gsi_stmt (*gsi_p)) == GIMPLE_PREDICT)
	;
      else if (!is_gimple_debug (gsi_stmt (*gsi_p)))
	prev = gsi_stmt (*gsi_p);
      gsi_next (gsi_p);
    }
  while (!gsi_end_p (*gsi_p)
	 /* Stop if we find a case or a user-defined label.  */
	 && (gimple_code (gsi_stmt (*gsi_p)) != GIMPLE_LABEL
	     || !gimple_has_location (gsi_stmt (*gsi_p))));

  if (prev && gimple_has_location (prev))
    *prevloc = gimple_location (prev);
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
	location_t prevloc;
	gimple *prev = collect_fallthrough_labels (gsi_p, &labels, &prevloc);

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
	    auto_diagnostic_group d;
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
		     && prevloc != UNKNOWN_LOCATION)
	      warned_p = warning_at (prevloc,
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
		      struct walk_stmt_info *wi)
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
	    {
	      *static_cast<location_t *>(wi->info) = gimple_location (stmt);
	      return integer_zero_node;
	    }

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
	    pedwarn (loc, 0, "attribute %<fallthrough%> not preceding "
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
  location_t loc;
  memset (&wi, 0, sizeof (wi));
  wi.info = (void *) &loc;
  walk_gimple_seq_mod (seq_p, expand_FALLTHROUGH_r, NULL, &wi);
  if (wi.callback_result == integer_zero_node)
    /* We've found [[fallthrough]]; at the end of a switch, which the C++
       standard says is ill-formed; see [dcl.attr.fallthrough].  */
    pedwarn (loc, 0, "attribute %<fallthrough%> not preceding "
	     "a case label or default label");
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
      maybe_warn_switch_unreachable_and_auto_init (switch_body_seq);
      maybe_warn_implicit_fallthrough (switch_body_seq);
      /* Only do this for the outermost GIMPLE_SWITCH.  */
      if (!gimplify_ctxp->in_switch_expr)
	expand_FALLTHROUGH (&switch_body_seq);

      labels = gimplify_ctxp->case_labels;
      gimplify_ctxp->case_labels = saved_labels;

      if (gimplify_ctxp->live_switch_vars)
	{
	  gcc_assert (gimplify_ctxp->live_switch_vars->is_empty ());
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

  tree label = CASE_LABEL (*expr_p);
  label_stmt = gimple_build_label (label);
  gimple_set_location (label_stmt, EXPR_LOCATION (*expr_p));
  ctxp->case_labels.safe_push (*expr_p);
  gimplify_seq_add_stmt (pre_p, label_stmt);

  if (lookup_attribute ("cold", DECL_ATTRIBUTES (label)))
    gimple_seq_add_stmt (pre_p, gimple_build_predict (PRED_COLD_LABEL,
						      NOT_TAKEN));
  else if (lookup_attribute ("hot", DECL_ATTRIBUTES (label)))
    gimple_seq_add_stmt (pre_p, gimple_build_predict (PRED_HOT_LABEL,
						      TAKEN));

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
     in g++.old-deja/g++.jason/binding.C.
     Another possible culpit are size expressions for variably modified
     types which are lost in the FE or not gimplified correctly.  */
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
      *expr_p = unshare_expr (DECL_VALUE_EXPR (decl));
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

     The base expression may contain a statement expression that
     has declarations used in size expressions, so has to be
     gimplified before gimplifying the size expressions.

     So we do this in three steps.  First we deal with variable
     bounds, sizes, and positions, then we gimplify the base and
     ensure it is memory if needed, then we deal with the annotations
     for any variables in the components and any indices, from left
     to right.  */

  bool need_non_reg = false;
  for (i = expr_stack.length () - 1; i >= 0; i--)
    {
      tree t = expr_stack[i];

      if (TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	{
	  /* Deal with the low bound and element type size and put them into
	     the ARRAY_REF.  If these values are set, they have already been
	     gimplified.  */
	  if (TREE_OPERAND (t, 2) == NULL_TREE)
	    {
	      tree low = unshare_expr (array_ref_low_bound (t));
	      if (!is_gimple_min_invariant (low))
		{
		  TREE_OPERAND (t, 2) = low;
		}
	    }

	  if (TREE_OPERAND (t, 3) == NULL_TREE)
	    {
	      tree elmt_size = array_ref_element_size (t);
	      if (!is_gimple_min_invariant (elmt_size))
		{
		  elmt_size = unshare_expr (elmt_size);
		  tree elmt_type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 0)));
		  tree factor = size_int (TYPE_ALIGN_UNIT (elmt_type));

		  /* Divide the element size by the alignment of the element
		     type (above).  */
		  elmt_size = size_binop_loc (loc, EXACT_DIV_EXPR,
					      elmt_size, factor);

		  TREE_OPERAND (t, 3) = elmt_size;
		}
	    }
	  need_non_reg = true;
	}
      else if (TREE_CODE (t) == COMPONENT_REF)
	{
	  /* Set the field offset into T and gimplify it.  */
	  if (TREE_OPERAND (t, 2) == NULL_TREE)
	    {
	      tree offset = component_ref_field_offset (t);
	      if (!is_gimple_min_invariant (offset))
		{
		  offset = unshare_expr (offset);
		  tree field = TREE_OPERAND (t, 1);
		  tree factor
		    = size_int (DECL_OFFSET_ALIGN (field) / BITS_PER_UNIT);

		  /* Divide the offset by its alignment.  */
		  offset = size_binop_loc (loc, EXACT_DIV_EXPR,
					   offset, factor);

		  TREE_OPERAND (t, 2) = offset;
		}
	    }
	  need_non_reg = true;
	}
    }

  /* Step 2 is to gimplify the base expression.  Make sure lvalue is set
     so as to match the min_lval predicate.  Failure to do so may result
     in the creation of large aggregate temporaries.  */
  tret = gimplify_expr (p, pre_p, post_p, is_gimple_min_lval,
			fallback | fb_lvalue);
  ret = MIN (ret, tret);
  if (ret == GS_ERROR)
    return GS_ERROR;

  /* Step 2a: if we have component references we do not support on
     registers then make sure the base isn't a register.  Of course
     we can only do so if an rvalue is OK.  */
  if (need_non_reg && (fallback & fb_rvalue))
    prepare_gimple_addressable (p, pre_p);

  /* Step 3: gimplify size expressions and the indices and operands of
     ARRAY_REF.  During this loop we also remove any useless conversions.  */

  for (; expr_stack.length () > 0; )
    {
      tree t = expr_stack.pop ();

      if (TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	{
	  /* Gimplify the low bound and element type size. */
	  tret = gimplify_expr (&TREE_OPERAND (t, 2), pre_p, post_p,
				is_gimple_reg, fb_rvalue);
	  ret = MIN (ret, tret);

	  tret = gimplify_expr (&TREE_OPERAND (t, 3), pre_p, post_p,
				is_gimple_reg, fb_rvalue);
	  ret = MIN (ret, tret);

	  /* Gimplify the dimension.  */
	  tret = gimplify_expr (&TREE_OPERAND (t, 1), pre_p, post_p,
				is_gimple_val, fb_rvalue);
	  ret = MIN (ret, tret);
	}
      else if (TREE_CODE (t) == COMPONENT_REF)
	{
	  tret = gimplify_expr (&TREE_OPERAND (t, 2), pre_p, post_p,
				is_gimple_reg, fb_rvalue);
	  ret = MIN (ret, tret);
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

      lhs = get_initialized_tmp_var (lhs, pre_p);
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
    else if ((ctx->region_type & ORT_HOST_TEAMS) == ORT_HOST_TEAMS)
      return false;
  /* Delay folding of builtins until the IL is in consistent state
     so the diagnostic machinery can do a better job.  */
  if (gimple_call_builtin_p (gsi_stmt (*gsi)))
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

      if (ifn == IFN_ASSUME)
	{
	  if (simple_condition_p (CALL_EXPR_ARG (*expr_p, 0)))
	    {
	      /* If the [[assume (cond)]]; condition is simple
		 enough and can be evaluated unconditionally
		 without side-effects, expand it as
		 if (!cond) __builtin_unreachable ();  */
	      tree fndecl = builtin_decl_explicit (BUILT_IN_UNREACHABLE);
	      *expr_p = build3 (COND_EXPR, void_type_node,
				CALL_EXPR_ARG (*expr_p, 0), void_node,
				build_call_expr_loc (EXPR_LOCATION (*expr_p),
						     fndecl, 0));
	      return GS_OK;
	    }
	  /* If not optimizing, ignore the assumptions.  */
	  if (!optimize || seen_error ())
	    {
	      *expr_p = NULL_TREE;
	      return GS_ALL_DONE;
	    }
	  /* Temporarily, until gimple lowering, transform
	     .ASSUME (cond);
	     into:
	     [[assume (guard)]]
	     {
	       guard = cond;
	     }
	     such that gimple lowering can outline the condition into
	     a separate function easily.  */
	  tree guard = create_tmp_var (boolean_type_node);
	  *expr_p = build2 (MODIFY_EXPR, void_type_node, guard,
			    gimple_boolify (CALL_EXPR_ARG (*expr_p, 0)));
	  *expr_p = build3 (BIND_EXPR, void_type_node, NULL, *expr_p, NULL);
	  push_gimplify_context ();
	  gimple_seq body = NULL;
	  gimple *g = gimplify_and_return_first (*expr_p, &body);
	  pop_gimplify_context (g);
	  g = gimple_build_assume (guard, body);
	  gimple_set_location (g, loc);
	  gimplify_seq_add_stmt (pre_p, g);
	  *expr_p = NULL_TREE;
	  return GS_ALL_DONE;
	}

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
  if (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
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

      case BUILT_IN_EH_RETURN:
	cfun->calls_eh_return = true;
	break;

      case BUILT_IN_CLEAR_PADDING:
	if (call_expr_nargs (*expr_p) == 1)
	  {
	    /* Remember the original type of the argument in an internal
	       dummy second argument, as in GIMPLE pointer conversions are
	       useless.  Also mark this call as not for automatic
	       initialization in the internal dummy third argument.  */
	    p = CALL_EXPR_ARG (*expr_p, 0);
	    *expr_p
	      = build_call_expr_loc (EXPR_LOCATION (*expr_p), fndecl, 2, p,
				     build_zero_cst (TREE_TYPE (p)));
	    return GS_OK;
	  }
	break;

      default:
        ;
      }
  if (fndecl && fndecl_built_in_p (fndecl))
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

  if (flag_openmp
      && fndecl
      && cfun
      && (cfun->curr_properties & PROP_gimple_any) == 0)
    {
      tree variant = omp_resolve_declare_variant (fndecl);
      if (variant != fndecl)
	CALL_EXPR_FN (*expr_p) = build1 (ADDR_EXPR, fnptrtype, variant);
    }

  /* There is a sequence point before the call, so any side effects in
     the calling expression must occur before the actual call.  Force
     gimplify_expr to use an internal post queue.  */
  ret = gimplify_expr (&CALL_EXPR_FN (*expr_p), pre_p, NULL,
		       is_gimple_call_addr, fb_rvalue);

  if (ret == GS_ERROR)
    return GS_ERROR;

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
	  && fndecl_built_in_p (last_arg_fndecl, BUILT_IN_VA_ARG_PACK))
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
	  && fndecl_built_in_p (fn, BUILT_IN_EXPECT)
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
	  /* These expressions always produce boolean results.  */
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
  ret = gimplify_expr (&cond, pre_p, NULL, is_gimple_val, fb_rvalue);
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

bool
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
	TREE_OPERAND (expr, 1) = build2 (INIT_EXPR, type, tmp, then_);

      /* Similarly, build the new else clause, `tmp = else_;'.  */
      if (!VOID_TYPE_P (TREE_TYPE (else_)))
	TREE_OPERAND (expr, 2) = build2 (INIT_EXPR, type, tmp, else_);

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
  ret = gimplify_expr (&TREE_OPERAND (expr, 0), pre_p, NULL,
		       is_gimple_condexpr_for_cond, fb_rvalue);
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
  gimple_set_location (cond_stmt, EXPR_LOCATION (expr));
  copy_warning (cond_stmt, COND_EXPR_COND (expr));
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
	{
	  /* For if (0) {} else { code; } tell -Wimplicit-fallthrough
	     handling that label_cont == label_true can be only reached
	     through fallthrough from { code; }.  */
	  if (integer_zerop (COND_EXPR_COND (expr)))
	    UNUSED_LABEL_P (label_true) = 1;
	  label_cont = label_true;
	}
      else
	{
	  bool then_side_effects
	    = (TREE_OPERAND (expr, 1)
	       && TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)));
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

	      /* For if (0) { non-side-effect-code } else { code }
		 tell -Wimplicit-fallthrough handling that label_cont can
		 be only reached through fallthrough from { code }.  */
	      if (integer_zerop (COND_EXPR_COND (expr)))
		{
		  UNUSED_LABEL_P (label_true) = 1;
		  if (!then_side_effects)
		    UNUSED_LABEL_P (label_cont) = 1;
		}

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
      /* For if (1) { code } or if (1) { code } else { non-side-effect-code }
	 tell -Wimplicit-fallthrough handling that label_false can be only
	 reached through fallthrough from { code }.  */
      if (integer_nonzerop (COND_EXPR_COND (expr))
	  && (TREE_OPERAND (expr, 2) == NULL_TREE
	      || !TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 2))))
	UNUSED_LABEL_P (label_false) = 1;
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

  /* Do not allow an SSA name as the temporary.  */
  if (is_gimple_reg (*expr_p))
    *expr_p = internal_get_tmp_var (*expr_p, seq_p, NULL, false, false, true);
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
  gimple_call_set_alloca_for_var (gs, true);

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
    {
      if (gimplify_expr (&value, pre_p, NULL, is_gimple_val, fb_rvalue)
	  != GS_ERROR)
	gimplify_seq_add_stmt (pre_p, gimple_build_assign (cref, value));
    }

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
	 happen with calls to functions returning a empty type, which
	 we shouldn't discard.  As a number of downstream passes don't
	 expect sets of empty type fields, we rely on the gimplification of
	 the MODIFY_EXPR we make below to drop the assignment statement.  */
      if (!TREE_SIDE_EFFECTS (value)
	  && TREE_CODE (purpose) == FIELD_DECL
	  && is_empty_type (TREE_TYPE (purpose)))
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
	   && !TREE_THIS_VOLATILE (decl)
	   && init
	   && (fallback & fb_lvalue) == 0
	   && gimple_test_f (init))
    {
      *expr_p = init;
      return GS_OK;
    }

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
  bool cleared = false;
  bool is_empty_ctor = false;
  bool is_init_expr = (TREE_CODE (*expr_p) == INIT_EXPR);

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
	/* Use readonly data for initializers of this or smaller size
	   regardless of the num_nonzero_elements / num_unique_nonzero_elements
	   ratio.  */
	const HOST_WIDE_INT min_unique_size = 64;
	/* If num_nonzero_elements / num_unique_nonzero_elements ratio
	   is smaller than this, use readonly data.  */
	const int unique_nonzero_ratio = 8;
	/* True if a single access of the object must be ensured.  This is the
	   case if the target is volatile, the type is non-addressable and more
	   than one field need to be assigned.  */
	const bool ensure_single_access
	  = TREE_THIS_VOLATILE (object)
	    && !TREE_ADDRESSABLE (type)
	    && vec_safe_length (elts) > 1;
	struct gimplify_init_ctor_preeval_data preeval_data;
	HOST_WIDE_INT num_ctor_elements, num_nonzero_elements;
	HOST_WIDE_INT num_unique_nonzero_elements;
	bool complete_p, valid_const_initializer;

	/* Aggregate types must lower constructors to initialization of
	   individual elements.  The exception is that a CONSTRUCTOR node
	   with no elements indicates zero-initialization of the whole.  */
	if (vec_safe_is_empty (elts))
	  {
	    if (notify_temp_creation)
	      return GS_OK;

	    /* The var will be initialized and so appear on lhs of
	       assignment, it can't be TREE_READONLY anymore.  */
	    if (VAR_P (object))
	      TREE_READONLY (object) = 0;

	    is_empty_ctor = true;
	    break;
	  }

	/* Fetch information about the constructor to direct later processing.
	   We might want to make static versions of it in various cases, and
	   can only do so if it known to be a valid constant initializer.  */
	valid_const_initializer
	  = categorize_ctor_elements (ctor, &num_nonzero_elements,
				      &num_unique_nonzero_elements,
				      &num_ctor_elements, &complete_p);

	/* If a const aggregate variable is being initialized, then it
	   should never be a lose to promote the variable to be static.  */
	if (valid_const_initializer
	    && num_nonzero_elements > 1
	    && TREE_READONLY (object)
	    && VAR_P (object)
	    && !DECL_REGISTER (object)
	    && (flag_merge_constants >= 2 || !TREE_ADDRESSABLE (object))
	    /* For ctors that have many repeated nonzero elements
	       represented through RANGE_EXPRs, prefer initializing
	       those through runtime loops over copies of large amounts
	       of data from readonly data section.  */
	    && (num_unique_nonzero_elements
		> num_nonzero_elements / unique_nonzero_ratio
		|| ((unsigned HOST_WIDE_INT) int_size_in_bytes (type)
		    <= (unsigned HOST_WIDE_INT) min_unique_size)))
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

	/* The var will be initialized and so appear on lhs of
	   assignment, it can't be TREE_READONLY anymore.  */
	if (VAR_P (object) && !notify_temp_creation)
	  TREE_READONLY (object) = 0;

	/* If there are "lots" of initialized elements, even discounting
	   those that are not address constants (and thus *must* be
	   computed at runtime), then partition the constructor into
	   constant and non-constant parts.  Block copy the constant
	   parts in, then generate code for the non-constant parts.  */
	/* TODO.  There's code in cp/typeck.cc to do this.  */

	if (int_size_in_bytes (TREE_TYPE (ctor)) < 0)
	  /* store_constructor will ignore the clearing of variable-sized
	     objects.  Initializers for such objects must explicitly set
	     every field that needs to be set.  */
	  cleared = false;
	else if (!complete_p)
	  /* If the constructor isn't complete, clear the whole object
	     beforehand, unless CONSTRUCTOR_NO_CLEARING is set on it.

	     ??? This ought not to be needed.  For any element not present
	     in the initializer, we should simply set them to zero.  Except
	     we'd need to *find* the elements that are not present, and that
	     requires trickery to avoid quadratic compile-time behavior in
	     large cases or excessive memory use in small cases.  */
	  cleared = !CONSTRUCTOR_NO_CLEARING (ctor);
	else if (num_ctor_elements - num_nonzero_elements
		 > CLEAR_RATIO (optimize_function_for_speed_p (cfun))
		 && num_nonzero_elements < num_ctor_elements / 4)
	  /* If there are "lots" of zeros, it's more efficient to clear
	     the memory and then set the nonzero elements.  */
	  cleared = true;
	else if (ensure_single_access && num_nonzero_elements == 0)
	  /* If a single access to the target must be ensured and all elements
	     are zero, then it's optimal to clear whatever their number.  */
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
	   TREE_ADDRESSABLE types.  */
	if (valid_const_initializer
	    && complete_p
	    && !(cleared || num_nonzero_elements == 0)
	    && !TREE_ADDRESSABLE (type))
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
		/* For ctors that have many repeated nonzero elements
		   represented through RANGE_EXPRs, prefer initializing
		   those through runtime loops over copies of large amounts
		   of data from readonly data section.  */
		&& (num_unique_nonzero_elements
		    > num_nonzero_elements / unique_nonzero_ratio
		    || size <= min_unique_size)
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

	/* If a single access to the target must be ensured and there are
	   nonzero elements or the zero elements are not assigned en masse,
	   initialize the target from a temporary.  */
	if (ensure_single_access && (num_nonzero_elements > 0 || !cleared))
	  {
	    if (notify_temp_creation)
	      return GS_ERROR;

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
	recompute_constructor_flags (ctor);

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
     not emitted an assignment, do so now.   */
  if (*expr_p
      /* If the type is an empty type, we don't need to emit the
	 assignment. */
      && !is_empty_type (TREE_TYPE (TREE_OPERAND (*expr_p, 0))))
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
      ret = GS_OK;
    }
  else
    {
      *expr_p = NULL;
      ret = GS_ALL_DONE;
    }

  /* If the user requests to initialize automatic variables, we
     should initialize paddings inside the variable.  Add a call to
     __builtin_clear_pading (&object, 0, for_auto_init = true) to
     initialize paddings of object always to zero regardless of
     INIT_TYPE.  Note, we will not insert this call if the aggregate
     variable has be completely cleared already or it's initialized
     with an empty constructor.  We cannot insert this call if the
     variable is a gimple register since __builtin_clear_padding will take
     the address of the variable.  As a result, if a long double/_Complex long
     double variable will be spilled into stack later, its padding cannot
     be cleared with __builtin_clear_padding.  We should clear its padding
     when it is spilled into memory.  */
  if (is_init_expr
      && !is_gimple_reg (object)
      && clear_padding_type_may_have_padding_p (type)
      && ((AGGREGATE_TYPE_P (type) && !cleared && !is_empty_ctor)
	  || !AGGREGATE_TYPE_P (type))
      && is_var_need_auto_init (object))
    gimple_add_padding_init_for_auto_var (object, false, pre_p);

  return ret;
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
	     a constructor and not volatile, do the direct assignment from
	     the constructor, but only if the target is not volatile either
	     since this latter assignment might end up being done on a per
	     field basis.  However, if the target is volatile and the type
	     is aggregate and non-addressable, gimplify_init_constructor
	     knows that it needs to ensure a single access to the target
	     and it will return GS_OK only in this case.  */
	  if (TREE_READONLY (*from_p)
	      && DECL_INITIAL (*from_p)
	      && TREE_CODE (DECL_INITIAL (*from_p)) == CONSTRUCTOR
	      && !TREE_THIS_VOLATILE (*from_p)
	      && (!TREE_THIS_VOLATILE (*to_p)
		  || (AGGREGATE_TYPE_P (TREE_TYPE (*to_p))
		      && !TREE_ADDRESSABLE (TREE_TYPE (*to_p)))))
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
	  if (!TREE_ADDRESSABLE (TREE_TYPE (*from_p)))
	    /* If we have code like

	     *(const A*)(A*)&x

	     where the type of "x" is a (possibly cv-qualified variant
	     of "A"), treat the entire expression as identical to "x".
	     This kind of code arises in C++ when an object is bound
	     to a const reference, and if "x" is a TARGET_EXPR we want
	     to take advantage of the optimization below.  But not if
	     the type is TREE_ADDRESSABLE; then C++17 says that the
	     TARGET_EXPR needs to be a temporary.  */
	    if (tree t
		= gimple_fold_indirect_ref_rhs (TREE_OPERAND (*from_p, 0)))
	      {
		bool volatile_p = TREE_THIS_VOLATILE (*from_p);
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
	  ret = gimplify_init_constructor (expr_p, pre_p, post_p, want_value,
					   false);
	  return ret;

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

	case NOP_EXPR:
	  /* Pull out compound literal expressions from a NOP_EXPR.
	     Those are created in the C FE to drop qualifiers during
	     lvalue conversion.  */
	  if ((TREE_CODE (TREE_OPERAND (*from_p, 0)) == COMPOUND_LITERAL_EXPR)
	      && tree_ssa_useless_type_conversion (*from_p))
	    {
	      *from_p = TREE_OPERAND (*from_p, 0);
	      ret = GS_OK;
	      changed = true;
	    }
	  break;

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
    case OACC_SERIAL:
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
    case OMP_LOOP:
    case OACC_LOOP:
    case OMP_SCAN:
    case OMP_SCOPE:
    case OMP_SECTIONS:
    case OMP_SECTION:
    case OMP_SINGLE:
    case OMP_MASTER:
    case OMP_MASKED:
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
   a MODIFY_EXPR with a lhs of a REAL/IMAGPART_EXPR of a gimple register.

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
  suppress_warning (other);
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

  if (error_operand_p (*from_p) || error_operand_p (*to_p))
    return GS_ERROR;

  gcc_assert (TREE_CODE (*expr_p) == MODIFY_EXPR
	      || TREE_CODE (*expr_p) == INIT_EXPR);

  /* Trying to simplify a clobber using normal logic doesn't work,
     so handle it here.  */
  if (TREE_CLOBBER_P (*from_p))
    {
      ret = gimplify_expr (to_p, pre_p, post_p, is_gimple_lvalue, fb_lvalue);
      if (ret == GS_ERROR)
	return ret;
      gcc_assert (!want_value);
      if (!VAR_P (*to_p) && TREE_CODE (*to_p) != MEM_REF)
	{
	  tree addr = get_initialized_tmp_var (build_fold_addr_expr (*to_p),
					       pre_p, post_p);
	  *to_p = build_simple_mem_ref_loc (EXPR_LOCATION (*to_p), addr);
	}
      gimplify_seq_add_stmt (pre_p, gimple_build_assign (*to_p, *from_p));
      *expr_p = NULL;
      return GS_ALL_DONE;
    }

  /* Convert initialization from an empty variable-size CONSTRUCTOR to
     memset.  */
  if (TREE_TYPE (*from_p) != error_mark_node
      && TYPE_SIZE_UNIT (TREE_TYPE (*from_p))
      && !poly_int_tree_p (TYPE_SIZE_UNIT (TREE_TYPE (*from_p)))
      && TREE_CODE (*from_p) == CONSTRUCTOR
      && CONSTRUCTOR_NELTS (*from_p) == 0)
    {
      maybe_with_size_expr (from_p);
      gcc_assert (TREE_CODE (*from_p) == WITH_SIZE_EXPR);
      return gimplify_modify_expr_to_memset (expr_p,
					     TREE_OPERAND (*from_p, 1),
					     want_value, pre_p);
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

  /* For empty types only gimplify the left hand side and right hand
     side as statements and throw away the assignment.  Do this after
     gimplify_modify_expr_rhs so we handle TARGET_EXPRs of addressable
     types properly.  */
  if (is_empty_type (TREE_TYPE (*from_p))
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
	      && fndecl_built_in_p (fndecl, BUILT_IN_EXPECT)
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
	copy_warning (assign, *from_p);
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

  if (val && TREE_TYPE (val) == error_mark_node)
    return GS_ERROR;

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
	   control flow may invalidate use/def domination.  When in SSA
	   form then assume there are no such issues and SAVE_EXPRs only
	   appear via GENERIC foldings.  */
	val = get_initialized_tmp_var (val, pre_p, post_p,
				       gimple_in_ssa_p (cfun));

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
	  && fndecl_built_in_p (op0, BUILT_IN_NORMAL)
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
      if (TREE_CODE (op0) == INDIRECT_REF
	  || (TREE_CODE (op0) == MEM_REF
	      && integer_zerop (TREE_OPERAND (op0, 1))))
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

      /* If we can't make copies, we can only accept memory.
	 Similarly for VLAs.  */
      tree outtype = TREE_TYPE (TREE_VALUE (link));
      if (outtype != error_mark_node
	  && (TREE_ADDRESSABLE (outtype)
	      || !COMPLETE_TYPE_P (outtype)
	      || !tree_fits_poly_uint64_p (TYPE_SIZE_UNIT (outtype))))
	{
	  if (allows_mem)
	    allows_reg = 0;
	  else
	    {
	      error ("impossible constraint in %<asm%>");
	      error ("non-memory output %d must stay in memory", i);
	      return GS_ERROR;
	    }
	}

      if (!allows_reg && allows_mem)
	mark_addressable (TREE_VALUE (link));

      tree orig = TREE_VALUE (link);
      tret = gimplify_expr (&TREE_VALUE (link), pre_p, post_p,
			    is_inout ? is_gimple_min_lval : is_gimple_lvalue,
			    fb_lvalue | fb_mayfail);
      if (tret == GS_ERROR)
	{
	  if (orig != error_mark_node)
	    error ("invalid lvalue in %<asm%> output %d", i);
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
      tree intype = TREE_TYPE (TREE_VALUE (link));
      if (intype != error_mark_node
	  && (TREE_ADDRESSABLE (intype)
	      || !COMPLETE_TYPE_P (intype)
	      || !tree_fits_poly_uint64_p (TYPE_SIZE_UNIT (intype))))
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
	      if (inputv != error_mark_node)
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

      /* asm is volatile if it was marked by the user as volatile or
	 there are no outputs or this is an asm goto.  */
      gimple_asm_set_volatile (stmt,
			       ASM_VOLATILE_P (expr)
			       || noutputs == 0
			       || labels);
      gimple_asm_set_input (stmt, ASM_INPUT_P (expr));
      gimple_asm_set_inline (stmt, ASM_INLINE_P (expr));

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
	  gimple_wce_set_cleanup_eh_only (wce, eh_only);

	  gimplify_seq_add_stmt (&gimplify_ctxp->conditional_cleanups, ffalse);
	  gimplify_seq_add_stmt (&gimplify_ctxp->conditional_cleanups, wce);
	  gimplify_seq_add_stmt (pre_p, ftrue);

	  /* Because of this manipulation, and the EH edges that jump
	     threading cannot redirect, the temporary (VAR) will appear
	     to be used uninitialized.  Don't warn.  */
	  suppress_warning (var, OPT_Wuninitialized);
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
      gimple_seq init_pre_p = NULL;

      /* TARGET_EXPR temps aren't part of the enclosing block, so add it
	 to the temps list.  Handle also variable length TARGET_EXPRs.  */
      if (!poly_int_tree_p (DECL_SIZE (temp)))
	{
	  if (!TYPE_SIZES_GIMPLIFIED (TREE_TYPE (temp)))
	    gimplify_type_sizes (TREE_TYPE (temp), &init_pre_p);
	  /* FIXME: this is correct only when the size of the type does
	     not depend on expressions evaluated in init.  */
	  gimplify_vla_decl (temp, &init_pre_p);
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
	ret = gimplify_expr (&init, &init_pre_p, post_p, is_gimple_stmt,
			     fb_none);
      else
	{
	  tree init_expr = build2 (INIT_EXPR, void_type_node, temp, init);
	  init = init_expr;
	  ret = gimplify_expr (&init, &init_pre_p, post_p, is_gimple_stmt,
			       fb_none);
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
	gimplify_and_add (init, &init_pre_p);

      /* Add a clobber for the temporary going out of scope, like
	 gimplify_bind_expr.  But only if we did not promote the
	 temporary to static storage.  */
      if (gimplify_ctxp->in_cleanup_point_expr
	  && !TREE_STATIC (temp)
	  && needs_to_live_in_memory (temp))
	{
	  if (flag_stack_reuse == SR_ALL)
	    {
	      tree clobber = build_clobber (TREE_TYPE (temp), CLOBBER_EOL);
	      clobber = build2 (MODIFY_EXPR, TREE_TYPE (temp), temp, clobber);
	      gimple_push_cleanup (temp, clobber, false, pre_p, true);
	    }
	  if (asan_poisoned_variables
	      && DECL_ALIGN (temp) <= MAX_SUPPORTED_STACK_ALIGNMENT
	      && !TREE_STATIC (temp)
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

      gimple_seq_add_seq (pre_p, init_pre_p);

      /* If needed, push the cleanup for the temp.  */
      if (TARGET_EXPR_CLEANUP (targ))
	gimple_push_cleanup (temp, TARGET_EXPR_CLEANUP (targ),
			     CLEANUP_EH_ONLY (targ), pre_p);

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
	  if (ctx->defaultmap[GDMK_SCALAR] & GOVD_FIRSTPRIVATE)
	    omp_add_variable (ctx, decl, GOVD_FIRSTPRIVATE);
	  else
	    omp_add_variable (ctx, decl, GOVD_MAP | GOVD_MAP_TO_ONLY);
	}
      else if (ctx->region_type != ORT_WORKSHARE
	       && ctx->region_type != ORT_TASKGROUP
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
      if (!(flags & GOVD_LOCAL) && ctx->region_type != ORT_TASKGROUP)
	{
	  if (flags & GOVD_MAP)
	    nflags = GOVD_MAP | GOVD_MAP_TO_ONLY | GOVD_EXPLICIT;
	  else if (flags & GOVD_PRIVATE)
	    nflags = GOVD_PRIVATE;
	  else if (((ctx->region_type & (ORT_TARGET | ORT_TARGET_DATA)) != 0
		    && (flags & GOVD_FIRSTPRIVATE))
		   || (ctx->region_type == ORT_TARGET_DATA
		       && (flags & GOVD_DATA_SHARE_CLASS) == 0))
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
	   && omp_privatize_by_reference (decl))
    {
      omp_firstprivatize_type_sizes (ctx, TREE_TYPE (decl));

      /* Similar to the direct variable sized case above, we'll need the
	 size of references being privatized.  */
      if ((flags & GOVD_SHARED) == 0)
	{
	  t = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)));
	  if (t && DECL_P (t))
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
    if ((octx->region_type & ORT_TARGET) != 0
	|| octx->order_concurrent)
      {
	n = splay_tree_lookup (octx->variables, (splay_tree_key)decl);
	if (n == NULL)
	  {
	    if (octx->order_concurrent)
	      {
		error ("threadprivate variable %qE used in a region with"
		       " %<order(concurrent)%> clause", DECL_NAME (decl));
		inform (octx->location, "enclosing region");
	      }
	    else
	      {
		error ("threadprivate variable %qE used in target region",
		       DECL_NAME (decl));
		inform (octx->location, "enclosing target region");
	      }
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
      inform (ctx->location, "enclosing task");
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
  if (ctx->region_type & ORT_TASK)
    {
      tree detach_clause = omp_find_clause (ctx->clauses, OMP_CLAUSE_DETACH);

      /* The event-handle specified by a detach clause should always be firstprivate,
	 regardless of the current default.  */
      if (detach_clause && OMP_CLAUSE_DECL (detach_clause) == decl)
	kind = OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;
    }
  if (kind != OMP_CLAUSE_DEFAULT_UNSPECIFIED)
    default_kind = kind;
  else if (VAR_P (decl) && TREE_STATIC (decl) && DECL_IN_CONSTANT_POOL (decl))
    default_kind = OMP_CLAUSE_DEFAULT_SHARED;
  /* For C/C++ default({,first}private), variables with static storage duration
     declared in a namespace or global scope and referenced in construct
     must be explicitly specified, i.e. acts as default(none).  */
  else if ((default_kind == OMP_CLAUSE_DEFAULT_PRIVATE
	    || default_kind == OMP_CLAUSE_DEFAULT_FIRSTPRIVATE)
	   && VAR_P (decl)
	   && is_global_var (decl)
	   && (DECL_FILE_SCOPE_P (decl)
	       || (DECL_CONTEXT (decl)
		   && TREE_CODE (DECL_CONTEXT (decl)) == NAMESPACE_DECL))
	   && !lang_GNU_Fortran ())
    default_kind = OMP_CLAUSE_DEFAULT_NONE;

  switch (default_kind)
    {
    case OMP_CLAUSE_DEFAULT_NONE:
      {
	const char *rtype;

	if (ctx->region_type & ORT_PARALLEL)
	  rtype = "parallel";
	else if ((ctx->region_type & ORT_TASKLOOP) == ORT_TASKLOOP)
	  rtype = "taskloop";
	else if (ctx->region_type & ORT_TASK)
	  rtype = "task";
	else if (ctx->region_type & ORT_TEAMS)
	  rtype = "teams";
	else
	  gcc_unreachable ();
	
	error ("%qE not specified in enclosing %qs",
	       DECL_NAME (lang_hooks.decls.omp_report_decl (decl)), rtype);
	inform (ctx->location, "enclosing %qs", rtype);
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
  bool is_private = false;
  bool declared = is_oacc_declared (decl);
  tree type = TREE_TYPE (decl);

  if (omp_privatize_by_reference (decl))
    type = TREE_TYPE (type);

  /* For Fortran COMMON blocks, only used variables in those blocks are
     transfered and remapped.  The block itself will have a private clause to
     avoid transfering the data twice.
     The hook evaluates to false by default.  For a variable in Fortran's COMMON
     or EQUIVALENCE block, returns 'true' (as we have shared=false) - as only
     the variables in such a COMMON/EQUIVALENCE block shall be privatized not
     the whole block.  For C++ and Fortran, it can also be true under certain
     other conditions, if DECL_HAS_VALUE_EXPR.  */
  if (RECORD_OR_UNION_TYPE_P (type))
    is_private = lang_hooks.decls.omp_disregard_value_expr (decl, false);

  if ((ctx->region_type & (ORT_ACC_PARALLEL | ORT_ACC_KERNELS)) != 0
      && is_global_var (decl)
      && device_resident_p (decl)
      && !is_private)
    {
      on_device = true;
      flags |= GOVD_MAP_TO_ONLY;
    }

  switch (ctx->region_type)
    {
    case ORT_ACC_KERNELS:
      rkind = "kernels";

      if (is_private)
	flags |= GOVD_FIRSTPRIVATE;
      else if (AGGREGATE_TYPE_P (type))
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
    case ORT_ACC_SERIAL:
      rkind = ctx->region_type == ORT_ACC_PARALLEL ? "parallel" : "serial";

      if (is_private)
	flags |= GOVD_FIRSTPRIVATE;
      else if (on_device || declared)
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
	  if (ctx->region_type & ORT_ACC)
	    /* For OpenACC, defer expansion of value to avoid transfering
	       privatized common block data instead of im-/explicitly transfered
	       variables which are in common blocks.  */
	    ;
	  else
	    {
	      tree value = get_base_address (DECL_VALUE_EXPR (decl));

	      if (value && DECL_P (value) && DECL_THREAD_LOCAL_P (value))
		return omp_notice_threadprivate_variable (ctx, decl, value);
	    }
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
      if (ctx->region_type & ORT_ACC)
	/* For OpenACC, as remarked above, defer expansion.  */
	shared = false;
      else
	shared = true;

      ret = lang_hooks.decls.omp_disregard_value_expr (decl, shared);
      if (n == NULL)
	{
	  unsigned nflags = flags;
	  if ((ctx->region_type & ORT_ACC) == 0)
	    {
	      bool is_declare_target = false;
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
	      if (!is_declare_target)
		{
		  int gdmk;
		  enum omp_clause_defaultmap_kind kind;
		  if (lang_hooks.decls.omp_allocatable_p (decl))
		    gdmk = GDMK_ALLOCATABLE;
		  else if (lang_hooks.decls.omp_scalar_target_p (decl))
		    gdmk = GDMK_SCALAR_TARGET;
		  else if (lang_hooks.decls.omp_scalar_p (decl, false))
		    gdmk = GDMK_SCALAR;
		  else if (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
			   || (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE
			       && (TREE_CODE (TREE_TYPE (TREE_TYPE (decl)))
				   == POINTER_TYPE)))
		    gdmk = GDMK_POINTER;
		  else
		    gdmk = GDMK_AGGREGATE;
		  kind = lang_hooks.decls.omp_predetermined_mapping (decl);
		  if (kind != OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED)
		    {
		      if (kind == OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE)
			nflags |= GOVD_FIRSTPRIVATE;
		      else if (kind == OMP_CLAUSE_DEFAULTMAP_TO)
			nflags |= GOVD_MAP | GOVD_MAP_TO_ONLY;
		      else
			gcc_unreachable ();
		    }
		  else if (ctx->defaultmap[gdmk] == 0)
		    {
		      tree d = lang_hooks.decls.omp_report_decl (decl);
		      error ("%qE not specified in enclosing %<target%>",
			     DECL_NAME (d));
		      inform (ctx->location, "enclosing %<target%>");
		    }
		  else if (ctx->defaultmap[gdmk]
			   & (GOVD_MAP_0LEN_ARRAY | GOVD_FIRSTPRIVATE))
		    nflags |= ctx->defaultmap[gdmk];
		  else
		    {
		      gcc_assert (ctx->defaultmap[gdmk] & GOVD_MAP);
		      nflags |= ctx->defaultmap[gdmk] & ~GOVD_MAP;
		    }
		}
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

	  if ((nflags & ~(GOVD_MAP_TO_ONLY | GOVD_MAP_FROM_ONLY
			  | GOVD_MAP_ALLOC_ONLY)) == flags)
	    {
	      tree type = TREE_TYPE (decl);

	      if (gimplify_omp_ctxp->target_firstprivatize_array_bases
		  && omp_privatize_by_reference (decl))
		type = TREE_TYPE (type);
	      if (!omp_mappable_type (type))
		{
		  error ("%qD referenced in target region does not have "
			 "a mappable type", decl);
		  nflags |= GOVD_MAP | GOVD_EXPLICIT;
		}
	      else
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
	  || ctx->region_type == ORT_TASKGROUP
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

  /* Don't mark as GOVD_SEEN addressable temporaries seen only in simd
     lb, b or incr expressions, those shouldn't be turned into simd arrays.  */
  if (ctx->region_type == ORT_SIMD
      && ctx->in_for_exprs
      && ((n->value & (GOVD_PRIVATE | GOVD_SEEN | GOVD_EXPLICIT))
	  == GOVD_PRIVATE))
    flags &= ~GOVD_SEEN;

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
      else if (omp_privatize_by_reference (decl)
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

  if (ctx->region_type & ORT_ACC)
    /* For OpenACC, as remarked above, defer expansion.  */
    shared = false;
  else
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
	  else if (simd != 1 && (n->value & GOVD_LINEAR) != 0)
	    error ("iteration variable %qE should not be linear",
		   DECL_NAME (decl));
	}
      return (ctx == gimplify_omp_ctxp
	      || (ctx->region_type == ORT_COMBINED_PARALLEL
		  && gimplify_omp_ctxp->outer_context == ctx));
    }

  if (ctx->region_type != ORT_WORKSHARE
      && ctx->region_type != ORT_TASKGROUP
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

	  if (omp_privatize_by_reference (decl))
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
	{
	  if ((ctx->region_type & ORT_TARGET_DATA) != 0
	      || n == NULL
	      || (n->value & GOVD_MAP) == 0)
	    continue;
	  return false;
	}

      if (n != NULL)
	{
	  if ((n->value & GOVD_LOCAL) != 0
	      && omp_member_access_dummy_var (decl))
	    return false;
	  return (n->value & GOVD_SHARED) == 0;
	}

      if (ctx->region_type == ORT_WORKSHARE
	  || ctx->region_type == ORT_TASKGROUP
	  || ctx->region_type == ORT_SIMD
	  || ctx->region_type == ORT_ACC)
	continue;

      break;
    }
  while (1);
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


/* Gimplify the affinity clause but effectively ignore it.
   Generate:
     var = begin;
     if ((step > 1) ? var <= end : var > end)
       locatator_var_expr;  */

static void
gimplify_omp_affinity (tree *list_p, gimple_seq *pre_p)
{
  tree last_iter = NULL_TREE;
  tree last_bind = NULL_TREE;
  tree label = NULL_TREE;
  tree *last_body = NULL;
  for (tree c = *list_p; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_AFFINITY)
      {
	tree t = OMP_CLAUSE_DECL (c);
	if (TREE_CODE (t) == TREE_LIST
		    && TREE_PURPOSE (t)
		    && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	  {
	    if (TREE_VALUE (t) == null_pointer_node)
	      continue;
	    if (TREE_PURPOSE (t) != last_iter)
	      {
		if (last_bind)
		  {
		    append_to_statement_list (label, last_body);
		    gimplify_and_add (last_bind, pre_p);
		    last_bind = NULL_TREE;
		  }
		for (tree it = TREE_PURPOSE (t); it; it = TREE_CHAIN (it))
		  {
		    if (gimplify_expr (&TREE_VEC_ELT (it, 1), pre_p, NULL,
				       is_gimple_val, fb_rvalue) == GS_ERROR
			|| gimplify_expr (&TREE_VEC_ELT (it, 2), pre_p, NULL,
					  is_gimple_val, fb_rvalue) == GS_ERROR
			|| gimplify_expr (&TREE_VEC_ELT (it, 3), pre_p, NULL,
					  is_gimple_val, fb_rvalue) == GS_ERROR
			|| (gimplify_expr (&TREE_VEC_ELT (it, 4), pre_p, NULL,
					   is_gimple_val, fb_rvalue)
			    == GS_ERROR))
		      return;
		  }
	    last_iter = TREE_PURPOSE (t);
	    tree block = TREE_VEC_ELT (TREE_PURPOSE (t), 5);
	    last_bind = build3 (BIND_EXPR, void_type_node, BLOCK_VARS (block),
				NULL, block);
	    last_body = &BIND_EXPR_BODY (last_bind);
	    tree cond = NULL_TREE;
	    location_t loc = OMP_CLAUSE_LOCATION (c);
	    for (tree it = TREE_PURPOSE (t); it; it = TREE_CHAIN (it))
	      {
		tree var = TREE_VEC_ELT (it, 0);
		tree begin = TREE_VEC_ELT (it, 1);
		tree end = TREE_VEC_ELT (it, 2);
		tree step = TREE_VEC_ELT (it, 3);
		loc = DECL_SOURCE_LOCATION (var);
		tree tem = build2_loc (loc, MODIFY_EXPR, void_type_node,
				       var, begin);
		append_to_statement_list_force (tem, last_body);

		tree cond1 = fold_build2_loc (loc, GT_EXPR, boolean_type_node,
			       step, build_zero_cst (TREE_TYPE (step)));
		tree cond2 = fold_build2_loc (loc, LE_EXPR, boolean_type_node,
					      var, end);
		tree cond3 = fold_build2_loc (loc, GT_EXPR, boolean_type_node,
					      var, end);
		cond1 = fold_build3_loc (loc, COND_EXPR, boolean_type_node,
					 cond1, cond2, cond3);
		if (cond)
		  cond = fold_build2_loc (loc, TRUTH_AND_EXPR,
					  boolean_type_node, cond, cond1);
		else
		  cond = cond1;
	      }
	    tree cont_label = create_artificial_label (loc);
	    label = build1 (LABEL_EXPR, void_type_node, cont_label);
	    tree tem = fold_build3_loc (loc, COND_EXPR, void_type_node, cond,
					void_node,
					build_and_jump (&cont_label));
	    append_to_statement_list_force (tem, last_body);
	      }
	    if (TREE_CODE (TREE_VALUE (t)) == COMPOUND_EXPR)
	      {
		append_to_statement_list (TREE_OPERAND (TREE_VALUE (t), 0),
					  last_body);
		TREE_VALUE (t) = TREE_OPERAND (TREE_VALUE (t), 1);
	      }
	    if (error_operand_p (TREE_VALUE (t)))
	      return;
	    append_to_statement_list_force (TREE_VALUE (t), last_body);
	    TREE_VALUE (t) = null_pointer_node;
	  }
	else
	  {
	    if (last_bind)
	      {
		append_to_statement_list (label, last_body);
		gimplify_and_add (last_bind, pre_p);
		last_bind = NULL_TREE;
	      }
	    if (TREE_CODE (OMP_CLAUSE_DECL (c)) == COMPOUND_EXPR)
	      {
		gimplify_expr (&TREE_OPERAND (OMP_CLAUSE_DECL (c), 0), pre_p,
			       NULL, is_gimple_val, fb_rvalue);
		OMP_CLAUSE_DECL (c) = TREE_OPERAND (OMP_CLAUSE_DECL (c), 1);
	      }
	    if (error_operand_p (OMP_CLAUSE_DECL (c)))
	      return;
	    if (gimplify_expr (&OMP_CLAUSE_DECL (c), pre_p, NULL,
			       is_gimple_lvalue, fb_lvalue) == GS_ERROR)
	      return;
	    gimplify_and_add (OMP_CLAUSE_DECL (c), pre_p);
	  }
      }
  if (last_bind)
    {
      append_to_statement_list (label, last_body);
      gimplify_and_add (last_bind, pre_p);
    }
  return;
}

/* If *LIST_P contains any OpenMP depend clauses with iterators,
   lower all the depend clauses by populating corresponding depend
   array.  Returns 0 if there are no such depend clauses, or
   2 if all depend clauses should be removed, 1 otherwise.  */

static int
gimplify_omp_depend (tree *list_p, gimple_seq *pre_p)
{
  tree c;
  gimple *g;
  size_t n[5] = { 0, 0, 0, 0, 0 };
  bool unused[5];
  tree counts[5] = { NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE };
  tree last_iter = NULL_TREE, last_count = NULL_TREE;
  size_t i, j;
  location_t first_loc = UNKNOWN_LOCATION;

  for (c = *list_p; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
      {
	switch (OMP_CLAUSE_DEPEND_KIND (c))
	  {
	  case OMP_CLAUSE_DEPEND_IN:
	    i = 2;
	    break;
	  case OMP_CLAUSE_DEPEND_OUT:
	  case OMP_CLAUSE_DEPEND_INOUT:
	    i = 0;
	    break;
	  case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
	    i = 1;
	    break;
	  case OMP_CLAUSE_DEPEND_DEPOBJ:
	    i = 3;
	    break;
	  case OMP_CLAUSE_DEPEND_INOUTSET:
	    i = 4;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	tree t = OMP_CLAUSE_DECL (c);
	if (first_loc == UNKNOWN_LOCATION)
	  first_loc = OMP_CLAUSE_LOCATION (c);
	if (TREE_CODE (t) == TREE_LIST
	    && TREE_PURPOSE (t)
	    && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	  {
	    if (TREE_PURPOSE (t) != last_iter)
	      {
		tree tcnt = size_one_node;
		for (tree it = TREE_PURPOSE (t); it; it = TREE_CHAIN (it))
		  {
		    if (gimplify_expr (&TREE_VEC_ELT (it, 1), pre_p, NULL,
				       is_gimple_val, fb_rvalue) == GS_ERROR
			|| gimplify_expr (&TREE_VEC_ELT (it, 2), pre_p, NULL,
					  is_gimple_val, fb_rvalue) == GS_ERROR
			|| gimplify_expr (&TREE_VEC_ELT (it, 3), pre_p, NULL,
					  is_gimple_val, fb_rvalue) == GS_ERROR
			|| (gimplify_expr (&TREE_VEC_ELT (it, 4), pre_p, NULL,
					   is_gimple_val, fb_rvalue)
			    == GS_ERROR))
		      return 2;
		    tree var = TREE_VEC_ELT (it, 0);
		    tree begin = TREE_VEC_ELT (it, 1);
		    tree end = TREE_VEC_ELT (it, 2);
		    tree step = TREE_VEC_ELT (it, 3);
		    tree orig_step = TREE_VEC_ELT (it, 4);
		    tree type = TREE_TYPE (var);
		    tree stype = TREE_TYPE (step);
		    location_t loc = DECL_SOURCE_LOCATION (var);
		    tree endmbegin;
		    /* Compute count for this iterator as
		       orig_step > 0
		       ? (begin < end ? (end - begin + (step - 1)) / step : 0)
		       : (begin > end ? (end - begin + (step + 1)) / step : 0)
		       and compute product of those for the entire depend
		       clause.  */
		    if (POINTER_TYPE_P (type))
		      endmbegin = fold_build2_loc (loc, POINTER_DIFF_EXPR,
						   stype, end, begin);
		    else
		      endmbegin = fold_build2_loc (loc, MINUS_EXPR, type,
						   end, begin);
		    tree stepm1 = fold_build2_loc (loc, MINUS_EXPR, stype,
						   step,
						   build_int_cst (stype, 1));
		    tree stepp1 = fold_build2_loc (loc, PLUS_EXPR, stype, step,
						   build_int_cst (stype, 1));
		    tree pos = fold_build2_loc (loc, PLUS_EXPR, stype,
						unshare_expr (endmbegin),
						stepm1);
		    pos = fold_build2_loc (loc, TRUNC_DIV_EXPR, stype,
					   pos, step);
		    tree neg = fold_build2_loc (loc, PLUS_EXPR, stype,
						endmbegin, stepp1);
		    if (TYPE_UNSIGNED (stype))
		      {
			neg = fold_build1_loc (loc, NEGATE_EXPR, stype, neg);
			step = fold_build1_loc (loc, NEGATE_EXPR, stype, step);
		      }
		    neg = fold_build2_loc (loc, TRUNC_DIV_EXPR, stype,
					   neg, step);
		    step = NULL_TREE;
		    tree cond = fold_build2_loc (loc, LT_EXPR,
						 boolean_type_node,
						 begin, end);
		    pos = fold_build3_loc (loc, COND_EXPR, stype, cond, pos,
					   build_int_cst (stype, 0));
		    cond = fold_build2_loc (loc, LT_EXPR, boolean_type_node,
					    end, begin);
		    neg = fold_build3_loc (loc, COND_EXPR, stype, cond, neg,
					   build_int_cst (stype, 0));
		    tree osteptype = TREE_TYPE (orig_step);
		    cond = fold_build2_loc (loc, GT_EXPR, boolean_type_node,
					    orig_step,
					    build_int_cst (osteptype, 0));
		    tree cnt = fold_build3_loc (loc, COND_EXPR, stype,
						cond, pos, neg);
		    cnt = fold_convert_loc (loc, sizetype, cnt);
		    if (gimplify_expr (&cnt, pre_p, NULL, is_gimple_val,
				       fb_rvalue) == GS_ERROR)
		      return 2;
		    tcnt = size_binop_loc (loc, MULT_EXPR, tcnt, cnt);
		  }
		if (gimplify_expr (&tcnt, pre_p, NULL, is_gimple_val,
				   fb_rvalue) == GS_ERROR)
		  return 2;
		last_iter = TREE_PURPOSE (t);
		last_count = tcnt;
	      }
	    if (counts[i] == NULL_TREE)
	      counts[i] = last_count;
	    else
	      counts[i] = size_binop_loc (OMP_CLAUSE_LOCATION (c),
					  PLUS_EXPR, counts[i], last_count);
	  }
	else
	  n[i]++;
      }
  for (i = 0; i < 5; i++)
    if (counts[i])
      break;
  if (i == 5)
    return 0;

  tree total = size_zero_node;
  for (i = 0; i < 5; i++)
    {
      unused[i] = counts[i] == NULL_TREE && n[i] == 0;
      if (counts[i] == NULL_TREE)
	counts[i] = size_zero_node;
      if (n[i])
	counts[i] = size_binop (PLUS_EXPR, counts[i], size_int (n[i]));
      if (gimplify_expr (&counts[i], pre_p, NULL, is_gimple_val,
			 fb_rvalue) == GS_ERROR)
	return 2;
      total = size_binop (PLUS_EXPR, total, counts[i]);
    }

  if (gimplify_expr (&total, pre_p, NULL, is_gimple_val, fb_rvalue)
      == GS_ERROR)
    return 2;
  bool is_old = unused[1] && unused[3] && unused[4];
  tree totalpx = size_binop (PLUS_EXPR, unshare_expr (total),
			     size_int (is_old ? 1 : 4));
  if (!unused[4])
    totalpx = size_binop (PLUS_EXPR, totalpx,
			  size_binop (MULT_EXPR, counts[4], size_int (2)));
  tree type = build_array_type (ptr_type_node, build_index_type (totalpx));
  tree array = create_tmp_var_raw (type);
  TREE_ADDRESSABLE (array) = 1;
  if (!poly_int_tree_p (totalpx))
    {
      if (!TYPE_SIZES_GIMPLIFIED (TREE_TYPE (array)))
	gimplify_type_sizes (TREE_TYPE (array), pre_p);
      if (gimplify_omp_ctxp)
	{
	  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
	  while (ctx
		 && (ctx->region_type == ORT_WORKSHARE
		     || ctx->region_type == ORT_TASKGROUP
		     || ctx->region_type == ORT_SIMD
		     || ctx->region_type == ORT_ACC))
	    ctx = ctx->outer_context;
	  if (ctx)
	    omp_add_variable (ctx, array, GOVD_LOCAL | GOVD_SEEN);
	}
      gimplify_vla_decl (array, pre_p);
    }
  else
    gimple_add_tmp_var (array);
  tree r = build4 (ARRAY_REF, ptr_type_node, array, size_int (0), NULL_TREE,
		   NULL_TREE);
  tree tem;
  if (!is_old)
    {
      tem = build2 (MODIFY_EXPR, void_type_node, r,
		    build_int_cst (ptr_type_node, 0));
      gimplify_and_add (tem, pre_p);
      r = build4 (ARRAY_REF, ptr_type_node, array, size_int (1), NULL_TREE,
		  NULL_TREE);
    }
  tem = build2 (MODIFY_EXPR, void_type_node, r,
		fold_convert (ptr_type_node, total));
  gimplify_and_add (tem, pre_p);
  for (i = 1; i < (is_old ? 2 : 4); i++)
    {
      r = build4 (ARRAY_REF, ptr_type_node, array, size_int (i + !is_old),
		  NULL_TREE, NULL_TREE);
      tem = build2 (MODIFY_EXPR, void_type_node, r, counts[i - 1]);
      gimplify_and_add (tem, pre_p);
    }

  tree cnts[6];
  for (j = 5; j; j--)
    if (!unused[j - 1])
      break;
  for (i = 0; i < 5; i++)
    {
      if (i && (i >= j || unused[i - 1]))
	{
	  cnts[i] = cnts[i - 1];
	  continue;
	}
      cnts[i] = create_tmp_var (sizetype);
      if (i == 0)
	g = gimple_build_assign (cnts[i], size_int (is_old ? 2 : 5));
      else
	{
	  tree t;
	  if (is_old)
	    t = size_binop (PLUS_EXPR, counts[0], size_int (2));
	  else
	    t = size_binop (PLUS_EXPR, cnts[i - 1], counts[i - 1]);
	  if (gimplify_expr (&t, pre_p, NULL, is_gimple_val, fb_rvalue)
	      == GS_ERROR)
	    return 2;
	  g = gimple_build_assign (cnts[i], t);
	}
      gimple_seq_add_stmt (pre_p, g);
    }
  if (unused[4])
    cnts[5] = NULL_TREE;
  else
    {
      tree t = size_binop (PLUS_EXPR, total, size_int (5));
      cnts[5] = create_tmp_var (sizetype);
      g = gimple_build_assign (cnts[i], t);
      gimple_seq_add_stmt (pre_p, g);
    }

  last_iter = NULL_TREE;
  tree last_bind = NULL_TREE;
  tree *last_body = NULL;
  for (c = *list_p; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
      {
	switch (OMP_CLAUSE_DEPEND_KIND (c))
	  {
	  case OMP_CLAUSE_DEPEND_IN:
	    i = 2;
	    break;
	  case OMP_CLAUSE_DEPEND_OUT:
	  case OMP_CLAUSE_DEPEND_INOUT:
	    i = 0;
	    break;
	  case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
	    i = 1;
	    break;
	  case OMP_CLAUSE_DEPEND_DEPOBJ:
	    i = 3;
	    break;
	  case OMP_CLAUSE_DEPEND_INOUTSET:
	    i = 4;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	tree t = OMP_CLAUSE_DECL (c);
	if (TREE_CODE (t) == TREE_LIST
	    && TREE_PURPOSE (t)
	    && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	  {
	    if (TREE_PURPOSE (t) != last_iter)
	      {
		if (last_bind)
		  gimplify_and_add (last_bind, pre_p);
		tree block = TREE_VEC_ELT (TREE_PURPOSE (t), 5);
		last_bind = build3 (BIND_EXPR, void_type_node,
				    BLOCK_VARS (block), NULL, block);
		TREE_SIDE_EFFECTS (last_bind) = 1;
		SET_EXPR_LOCATION (last_bind, OMP_CLAUSE_LOCATION (c));
		tree *p = &BIND_EXPR_BODY (last_bind);
		for (tree it = TREE_PURPOSE (t); it; it = TREE_CHAIN (it))
		  {
		    tree var = TREE_VEC_ELT (it, 0);
		    tree begin = TREE_VEC_ELT (it, 1);
		    tree end = TREE_VEC_ELT (it, 2);
		    tree step = TREE_VEC_ELT (it, 3);
		    tree orig_step = TREE_VEC_ELT (it, 4);
		    tree type = TREE_TYPE (var);
		    location_t loc = DECL_SOURCE_LOCATION (var);
		    /* Emit:
		       var = begin;
		       goto cond_label;
		       beg_label:
		       ...
		       var = var + step;
		       cond_label:
		       if (orig_step > 0) {
			 if (var < end) goto beg_label;
		       } else {
			 if (var > end) goto beg_label;
		       }
		       for each iterator, with inner iterators added to
		       the ... above.  */
		    tree beg_label = create_artificial_label (loc);
		    tree cond_label = NULL_TREE;
		    tem = build2_loc (loc, MODIFY_EXPR, void_type_node,
				      var, begin);
		    append_to_statement_list_force (tem, p);
		    tem = build_and_jump (&cond_label);
		    append_to_statement_list_force (tem, p);
		    tem = build1 (LABEL_EXPR, void_type_node, beg_label);
		    append_to_statement_list (tem, p);
		    tree bind = build3 (BIND_EXPR, void_type_node, NULL_TREE,
					NULL_TREE, NULL_TREE);
		    TREE_SIDE_EFFECTS (bind) = 1;
		    SET_EXPR_LOCATION (bind, loc);
		    append_to_statement_list_force (bind, p);
		    if (POINTER_TYPE_P (type))
		      tem = build2_loc (loc, POINTER_PLUS_EXPR, type,
					var, fold_convert_loc (loc, sizetype,
							       step));
		    else
		      tem = build2_loc (loc, PLUS_EXPR, type, var, step);
		    tem = build2_loc (loc, MODIFY_EXPR, void_type_node,
				      var, tem);
		    append_to_statement_list_force (tem, p);
		    tem = build1 (LABEL_EXPR, void_type_node, cond_label);
		    append_to_statement_list (tem, p);
		    tree cond = fold_build2_loc (loc, LT_EXPR,
						 boolean_type_node,
						 var, end);
		    tree pos
		      = fold_build3_loc (loc, COND_EXPR, void_type_node,
					 cond, build_and_jump (&beg_label),
					 void_node);
		    cond = fold_build2_loc (loc, GT_EXPR, boolean_type_node,
					    var, end);
		    tree neg
		      = fold_build3_loc (loc, COND_EXPR, void_type_node,
					 cond, build_and_jump (&beg_label),
					 void_node);
		    tree osteptype = TREE_TYPE (orig_step);
		    cond = fold_build2_loc (loc, GT_EXPR, boolean_type_node,
					    orig_step,
					    build_int_cst (osteptype, 0));
		    tem = fold_build3_loc (loc, COND_EXPR, void_type_node,
					   cond, pos, neg);
		    append_to_statement_list_force (tem, p);
		    p = &BIND_EXPR_BODY (bind);
		  }
		last_body = p;
	      }
	    last_iter = TREE_PURPOSE (t);
	    if (TREE_CODE (TREE_VALUE (t)) == COMPOUND_EXPR)
	      {
		append_to_statement_list (TREE_OPERAND (TREE_VALUE (t),
					  0), last_body);
		TREE_VALUE (t) = TREE_OPERAND (TREE_VALUE (t), 1);
	      }
	    if (error_operand_p (TREE_VALUE (t)))
	      return 2;
	    if (TREE_VALUE (t) != null_pointer_node)
	      TREE_VALUE (t) = build_fold_addr_expr (TREE_VALUE (t));
	    if (i == 4)
	      {
		r = build4 (ARRAY_REF, ptr_type_node, array, cnts[i],
			    NULL_TREE, NULL_TREE);
		tree r2 = build4 (ARRAY_REF, ptr_type_node, array, cnts[5],
				  NULL_TREE, NULL_TREE);
		r2 = build_fold_addr_expr_with_type (r2, ptr_type_node);
		tem = build2_loc (OMP_CLAUSE_LOCATION (c), MODIFY_EXPR,
				  void_type_node, r, r2);
		append_to_statement_list_force (tem, last_body);
		tem = build2_loc (OMP_CLAUSE_LOCATION (c), MODIFY_EXPR,
				  void_type_node, cnts[i],
				  size_binop (PLUS_EXPR, cnts[i],
					      size_int (1)));
		append_to_statement_list_force (tem, last_body);
		i = 5;
	      }
	    r = build4 (ARRAY_REF, ptr_type_node, array, cnts[i],
			NULL_TREE, NULL_TREE);
	    tem = build2_loc (OMP_CLAUSE_LOCATION (c), MODIFY_EXPR,
			      void_type_node, r, TREE_VALUE (t));
	    append_to_statement_list_force (tem, last_body);
	    if (i == 5)
	      {
		r = build4 (ARRAY_REF, ptr_type_node, array,
			    size_binop (PLUS_EXPR, cnts[i], size_int (1)),
			    NULL_TREE, NULL_TREE);
		tem = build_int_cst (ptr_type_node, GOMP_DEPEND_INOUTSET);
		tem = build2_loc (OMP_CLAUSE_LOCATION (c), MODIFY_EXPR,
				  void_type_node, r, tem);
		append_to_statement_list_force (tem, last_body);
	      }
	    tem = build2_loc (OMP_CLAUSE_LOCATION (c), MODIFY_EXPR,
			      void_type_node, cnts[i],
			      size_binop (PLUS_EXPR, cnts[i],
					  size_int (1 + (i == 5))));
	    append_to_statement_list_force (tem, last_body);
	    TREE_VALUE (t) = null_pointer_node;
	  }
	else
	  {
	    if (last_bind)
	      {
		gimplify_and_add (last_bind, pre_p);
		last_bind = NULL_TREE;
	      }
	    if (TREE_CODE (OMP_CLAUSE_DECL (c)) == COMPOUND_EXPR)
	      {
		gimplify_expr (&TREE_OPERAND (OMP_CLAUSE_DECL (c), 0), pre_p,
			       NULL, is_gimple_val, fb_rvalue);
		OMP_CLAUSE_DECL (c) = TREE_OPERAND (OMP_CLAUSE_DECL (c), 1);
	      }
	    if (error_operand_p (OMP_CLAUSE_DECL (c)))
	      return 2;
	    if (OMP_CLAUSE_DECL (c) != null_pointer_node)
	      OMP_CLAUSE_DECL (c) = build_fold_addr_expr (OMP_CLAUSE_DECL (c));
	    if (gimplify_expr (&OMP_CLAUSE_DECL (c), pre_p, NULL,
			       is_gimple_val, fb_rvalue) == GS_ERROR)
	      return 2;
	    if (i == 4)
	      {
		r = build4 (ARRAY_REF, ptr_type_node, array, cnts[i],
			    NULL_TREE, NULL_TREE);
		tree r2 = build4 (ARRAY_REF, ptr_type_node, array, cnts[5],
				  NULL_TREE, NULL_TREE);
		r2 = build_fold_addr_expr_with_type (r2, ptr_type_node);
		tem = build2 (MODIFY_EXPR, void_type_node, r, r2);
		gimplify_and_add (tem, pre_p);
		g = gimple_build_assign (cnts[i], size_binop (PLUS_EXPR,
							      cnts[i],
							      size_int (1)));
		gimple_seq_add_stmt (pre_p, g);
		i = 5;
	      }
	    r = build4 (ARRAY_REF, ptr_type_node, array, cnts[i],
			NULL_TREE, NULL_TREE);
	    tem = build2 (MODIFY_EXPR, void_type_node, r, OMP_CLAUSE_DECL (c));
	    gimplify_and_add (tem, pre_p);
	    if (i == 5)
	      {
		r = build4 (ARRAY_REF, ptr_type_node, array,
			    size_binop (PLUS_EXPR, cnts[i], size_int (1)),
			    NULL_TREE, NULL_TREE);
		tem = build_int_cst (ptr_type_node, GOMP_DEPEND_INOUTSET);
		tem = build2 (MODIFY_EXPR, void_type_node, r, tem);
		append_to_statement_list_force (tem, last_body);
		gimplify_and_add (tem, pre_p);
	      }
	    g = gimple_build_assign (cnts[i],
				     size_binop (PLUS_EXPR, cnts[i],
						 size_int (1 + (i == 5))));
	    gimple_seq_add_stmt (pre_p, g);
	  }
      }
  if (last_bind)
    gimplify_and_add (last_bind, pre_p);
  tree cond = boolean_false_node;
  if (is_old)
    {
      if (!unused[0])
	cond = build2_loc (first_loc, NE_EXPR, boolean_type_node, cnts[0],
			   size_binop_loc (first_loc, PLUS_EXPR, counts[0],
					   size_int (2)));
      if (!unused[2])
	cond = build2_loc (first_loc, TRUTH_OR_EXPR, boolean_type_node, cond,
			   build2_loc (first_loc, NE_EXPR, boolean_type_node,
				       cnts[2],
				       size_binop_loc (first_loc, PLUS_EXPR,
						       totalpx,
						       size_int (1))));
    }
  else
    {
      tree prev = size_int (5);
      for (i = 0; i < 5; i++)
	{
	  if (unused[i])
	    continue;
	  prev = size_binop_loc (first_loc, PLUS_EXPR, counts[i], prev);
	  cond = build2_loc (first_loc, TRUTH_OR_EXPR, boolean_type_node, cond,
			     build2_loc (first_loc, NE_EXPR, boolean_type_node,
					 cnts[i], unshare_expr (prev)));
	}
    }
  tem = build3_loc (first_loc, COND_EXPR, void_type_node, cond,
		    build_call_expr_loc (first_loc,
					 builtin_decl_explicit (BUILT_IN_TRAP),
					 0), void_node);
  gimplify_and_add (tem, pre_p);
  c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_DEPEND);
  OMP_CLAUSE_DEPEND_KIND (c) = OMP_CLAUSE_DEPEND_LAST;
  OMP_CLAUSE_DECL (c) = build_fold_addr_expr (array);
  OMP_CLAUSE_CHAIN (c) = *list_p;
  *list_p = c;
  return 1;
}

/* For a set of mappings describing an array section pointed to by a struct
   (or derived type, etc.) component, create an "alloc" or "release" node to
   insert into a list following a GOMP_MAP_STRUCT node.  For some types of
   mapping (e.g. Fortran arrays with descriptors), an additional mapping may
   be created that is inserted into the list of mapping nodes attached to the
   directive being processed -- not part of the sorted list of nodes after
   GOMP_MAP_STRUCT.

   CODE is the code of the directive being processed.  GRP_START and GRP_END
   are the first and last of two or three nodes representing this array section
   mapping (e.g. a data movement node like GOMP_MAP_{TO,FROM}, optionally a
   GOMP_MAP_TO_PSET, and finally a GOMP_MAP_ALWAYS_POINTER).  EXTRA_NODE is
   filled with the additional node described above, if needed.

   This function does not add the new nodes to any lists itself.  It is the
   responsibility of the caller to do that.  */

static tree
build_omp_struct_comp_nodes (enum tree_code code, tree grp_start, tree grp_end,
			     tree *extra_node)
{
  enum gomp_map_kind mkind
    = (code == OMP_TARGET_EXIT_DATA || code == OACC_EXIT_DATA)
      ? GOMP_MAP_RELEASE : GOMP_MAP_ALLOC;

  gcc_assert (grp_start != grp_end);

  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (grp_end), OMP_CLAUSE_MAP);
  OMP_CLAUSE_SET_MAP_KIND (c2, mkind);
  OMP_CLAUSE_DECL (c2) = unshare_expr (OMP_CLAUSE_DECL (grp_end));
  OMP_CLAUSE_CHAIN (c2) = NULL_TREE;
  tree grp_mid = NULL_TREE;
  if (OMP_CLAUSE_CHAIN (grp_start) != grp_end)
    grp_mid = OMP_CLAUSE_CHAIN (grp_start);

  if (grp_mid
      && OMP_CLAUSE_CODE (grp_mid) == OMP_CLAUSE_MAP
      && OMP_CLAUSE_MAP_KIND (grp_mid) == GOMP_MAP_TO_PSET)
    OMP_CLAUSE_SIZE (c2) = OMP_CLAUSE_SIZE (grp_mid);
  else
    OMP_CLAUSE_SIZE (c2) = TYPE_SIZE_UNIT (ptr_type_node);

  if (grp_mid
      && OMP_CLAUSE_CODE (grp_mid) == OMP_CLAUSE_MAP
      && (OMP_CLAUSE_MAP_KIND (grp_mid) == GOMP_MAP_ALWAYS_POINTER
	  || OMP_CLAUSE_MAP_KIND (grp_mid) == GOMP_MAP_ATTACH_DETACH))
    {
      tree c3
	= build_omp_clause (OMP_CLAUSE_LOCATION (grp_end), OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c3, mkind);
      OMP_CLAUSE_DECL (c3) = unshare_expr (OMP_CLAUSE_DECL (grp_mid));
      OMP_CLAUSE_SIZE (c3) = TYPE_SIZE_UNIT (ptr_type_node);
      OMP_CLAUSE_CHAIN (c3) = NULL_TREE;

      *extra_node = c3;
    }
  else
    *extra_node = NULL_TREE;

  return c2;
}

/* Strip ARRAY_REFS or an indirect ref off BASE, find the containing object,
   and set *BITPOSP and *POFFSETP to the bit offset of the access.
   If BASE_REF is non-NULL and the containing object is a reference, set
   *BASE_REF to that reference before dereferencing the object.
   If BASE_REF is NULL, check that the containing object is a COMPONENT_REF or
   has array type, else return NULL.  */

static tree
extract_base_bit_offset (tree base, poly_int64 *bitposp,
			 poly_offset_int *poffsetp)
{
  tree offset;
  poly_int64 bitsize, bitpos;
  machine_mode mode;
  int unsignedp, reversep, volatilep = 0;
  poly_offset_int poffset;

  STRIP_NOPS (base);

  base = get_inner_reference (base, &bitsize, &bitpos, &offset, &mode,
			      &unsignedp, &reversep, &volatilep);

  STRIP_NOPS (base);

  if (offset && poly_int_tree_p (offset))
    {
      poffset = wi::to_poly_offset (offset);
      offset = NULL_TREE;
    }
  else
    poffset = 0;

  if (maybe_ne (bitpos, 0))
    poffset += bits_to_bytes_round_down (bitpos);

  *bitposp = bitpos;
  *poffsetp = poffset;

  return base;
}

/* Used for topological sorting of mapping groups.  UNVISITED means we haven't
   started processing the group yet.  The TEMPORARY mark is used when we first
   encounter a group on a depth-first traversal, and the PERMANENT mark is used
   when we have processed all the group's children (i.e. all the base pointers
   referred to by the group's mapping nodes, recursively).  */

enum omp_tsort_mark {
  UNVISITED,
  TEMPORARY,
  PERMANENT
};

/* Hash for trees based on operand_equal_p.  Like tree_operand_hash
   but ignores side effects in the equality comparisons.  */

struct tree_operand_hash_no_se : tree_operand_hash
{
  static inline bool equal (const value_type &,
			    const compare_type &);
};

inline bool
tree_operand_hash_no_se::equal (const value_type &t1,
				const compare_type &t2)
{
  return operand_equal_p (t1, t2, OEP_MATCH_SIDE_EFFECTS);
}

/* A group of OMP_CLAUSE_MAP nodes that correspond to a single "map"
   clause.  */

struct omp_mapping_group {
  tree *grp_start;
  tree grp_end;
  omp_tsort_mark mark;
  /* If we've removed the group but need to reindex, mark the group as
     deleted.  */
  bool deleted;
  struct omp_mapping_group *sibling;
  struct omp_mapping_group *next;
};

DEBUG_FUNCTION void
debug_mapping_group (omp_mapping_group *grp)
{
  tree tmp = OMP_CLAUSE_CHAIN (grp->grp_end);
  OMP_CLAUSE_CHAIN (grp->grp_end) = NULL;
  debug_generic_expr (*grp->grp_start);
  OMP_CLAUSE_CHAIN (grp->grp_end) = tmp;
}

/* Return the OpenMP "base pointer" of an expression EXPR, or NULL if there
   isn't one.  */

static tree
omp_get_base_pointer (tree expr)
{
  while (TREE_CODE (expr) == ARRAY_REF
	 || TREE_CODE (expr) == COMPONENT_REF)
    expr = TREE_OPERAND (expr, 0);

  if (TREE_CODE (expr) == INDIRECT_REF
      || (TREE_CODE (expr) == MEM_REF
	  && integer_zerop (TREE_OPERAND (expr, 1))))
    {
      expr = TREE_OPERAND (expr, 0);
      while (TREE_CODE (expr) == COMPOUND_EXPR)
	expr = TREE_OPERAND (expr, 1);
      if (TREE_CODE (expr) == POINTER_PLUS_EXPR)
	expr = TREE_OPERAND (expr, 0);
      if (TREE_CODE (expr) == SAVE_EXPR)
	expr = TREE_OPERAND (expr, 0);
      STRIP_NOPS (expr);
      return expr;
    }

  return NULL_TREE;
}

/* Remove COMPONENT_REFS and indirections from EXPR.  */

static tree
omp_strip_components_and_deref (tree expr)
{
  while (TREE_CODE (expr) == COMPONENT_REF
	 || TREE_CODE (expr) == INDIRECT_REF
	 || (TREE_CODE (expr) == MEM_REF
	     && integer_zerop (TREE_OPERAND (expr, 1)))
	 || TREE_CODE (expr) == POINTER_PLUS_EXPR
	 || TREE_CODE (expr) == COMPOUND_EXPR)
      if (TREE_CODE (expr) == COMPOUND_EXPR)
	expr = TREE_OPERAND (expr, 1);
      else
	expr = TREE_OPERAND (expr, 0);

  STRIP_NOPS (expr);

  return expr;
}

static tree
omp_strip_indirections (tree expr)
{
  while (TREE_CODE (expr) == INDIRECT_REF
	 || (TREE_CODE (expr) == MEM_REF
	     && integer_zerop (TREE_OPERAND (expr, 1))))
    expr = TREE_OPERAND (expr, 0);

  return expr;
}

/* An attach or detach operation depends directly on the address being
   attached/detached.  Return that address, or none if there are no
   attachments/detachments.  */

static tree
omp_get_attachment (omp_mapping_group *grp)
{
  tree node = *grp->grp_start;

  switch (OMP_CLAUSE_MAP_KIND (node))
    {
    case GOMP_MAP_TO:
    case GOMP_MAP_FROM:
    case GOMP_MAP_TOFROM:
    case GOMP_MAP_ALWAYS_FROM:
    case GOMP_MAP_ALWAYS_TO:
    case GOMP_MAP_ALWAYS_TOFROM:
    case GOMP_MAP_FORCE_FROM:
    case GOMP_MAP_FORCE_TO:
    case GOMP_MAP_FORCE_TOFROM:
    case GOMP_MAP_FORCE_PRESENT:
    case GOMP_MAP_ALLOC:
    case GOMP_MAP_RELEASE:
    case GOMP_MAP_DELETE:
    case GOMP_MAP_FORCE_ALLOC:
      if (node == grp->grp_end)
	return NULL_TREE;

      node = OMP_CLAUSE_CHAIN (node);
      if (node && OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_TO_PSET)
	{
	  gcc_assert (node != grp->grp_end);
	  node = OMP_CLAUSE_CHAIN (node);
	}
      if (node)
	switch (OMP_CLAUSE_MAP_KIND (node))
	  {
	  case GOMP_MAP_POINTER:
	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
	    return NULL_TREE;

	  case GOMP_MAP_ATTACH_DETACH:
	  case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
	    return OMP_CLAUSE_DECL (node);

	  default:
	    internal_error ("unexpected mapping node");
	  }
      return error_mark_node;

    case GOMP_MAP_TO_PSET:
      gcc_assert (node != grp->grp_end);
      node = OMP_CLAUSE_CHAIN (node);
      if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_ATTACH
	  || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_DETACH)
	return OMP_CLAUSE_DECL (node);
      else
	internal_error ("unexpected mapping node");
      return error_mark_node;

    case GOMP_MAP_ATTACH:
    case GOMP_MAP_DETACH:
      node = OMP_CLAUSE_CHAIN (node);
      if (!node || *grp->grp_start == grp->grp_end)
	return OMP_CLAUSE_DECL (*grp->grp_start);
      if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_FIRSTPRIVATE_POINTER
	  || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
	return OMP_CLAUSE_DECL (*grp->grp_start);
      else
	internal_error ("unexpected mapping node");
      return error_mark_node;

    case GOMP_MAP_STRUCT:
    case GOMP_MAP_FORCE_DEVICEPTR:
    case GOMP_MAP_DEVICE_RESIDENT:
    case GOMP_MAP_LINK:
    case GOMP_MAP_IF_PRESENT:
    case GOMP_MAP_FIRSTPRIVATE:
    case GOMP_MAP_FIRSTPRIVATE_INT:
    case GOMP_MAP_USE_DEVICE_PTR:
    case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
      return NULL_TREE;

    default:
      internal_error ("unexpected mapping node");
    }

  return error_mark_node;
}

/* Given a pointer START_P to the start of a group of related (e.g. pointer)
   mappings, return the chain pointer to the end of that group in the list.  */

static tree *
omp_group_last (tree *start_p)
{
  tree c = *start_p, nc, *grp_last_p = start_p;

  gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP);

  nc = OMP_CLAUSE_CHAIN (c);

  if (!nc || OMP_CLAUSE_CODE (nc) != OMP_CLAUSE_MAP)
    return grp_last_p;

  switch (OMP_CLAUSE_MAP_KIND (c))
    {
    default:
      while (nc
	     && OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
	     && (OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_FIRSTPRIVATE_REFERENCE
		 || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_FIRSTPRIVATE_POINTER
		 || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_ATTACH_DETACH
		 || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_POINTER
		 || (OMP_CLAUSE_MAP_KIND (nc)
		     == GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION)
		 || (OMP_CLAUSE_MAP_KIND (nc)
		     == GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION)
		 || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_ALWAYS_POINTER
		 || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_TO_PSET))
	{
	  grp_last_p = &OMP_CLAUSE_CHAIN (c);
	  c = nc;
	  tree nc2 = OMP_CLAUSE_CHAIN (nc);
	  if (nc2
	      && OMP_CLAUSE_CODE (nc2) == OMP_CLAUSE_MAP
	      && (OMP_CLAUSE_MAP_KIND (nc)
		  == GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION)
	      && OMP_CLAUSE_MAP_KIND (nc2) == GOMP_MAP_ATTACH)
	    {
	      grp_last_p = &OMP_CLAUSE_CHAIN (nc);
	      c = nc2;
	      nc2 = OMP_CLAUSE_CHAIN (nc2);
	    }
	   nc = nc2;
	}
      break;

    case GOMP_MAP_ATTACH:
    case GOMP_MAP_DETACH:
      /* This is a weird artifact of how directives are parsed: bare attach or
	 detach clauses get a subsequent (meaningless) FIRSTPRIVATE_POINTER or
	 FIRSTPRIVATE_REFERENCE node.  FIXME.  */
      if (nc
	  && OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
	  && (OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_FIRSTPRIVATE_REFERENCE
	      || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_FIRSTPRIVATE_POINTER))
	grp_last_p = &OMP_CLAUSE_CHAIN (c);
      break;

    case GOMP_MAP_TO_PSET:
      if (OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
	  && (OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_ATTACH
	      || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_DETACH))
	grp_last_p = &OMP_CLAUSE_CHAIN (c);
      break;

    case GOMP_MAP_STRUCT:
      {
	unsigned HOST_WIDE_INT num_mappings
	  = tree_to_uhwi (OMP_CLAUSE_SIZE (c));
	if (OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_FIRSTPRIVATE_POINTER
	    || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_FIRSTPRIVATE_REFERENCE
	    || OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_ATTACH_DETACH)
	  grp_last_p = &OMP_CLAUSE_CHAIN (*grp_last_p);
	for (unsigned i = 0; i < num_mappings; i++)
	  grp_last_p = &OMP_CLAUSE_CHAIN (*grp_last_p);
      }
      break;
    }

  return grp_last_p;
}

/* Walk through LIST_P, and return a list of groups of mappings found (e.g.
   OMP_CLAUSE_MAP with GOMP_MAP_{TO/FROM/TOFROM} followed by one or two
   associated GOMP_MAP_POINTER mappings).  Return a vector of omp_mapping_group
   if we have more than one such group, else return NULL.  */

static void
omp_gather_mapping_groups_1 (tree *list_p, vec<omp_mapping_group> *groups,
			     tree gather_sentinel)
{
  for (tree *cp = list_p;
       *cp && *cp != gather_sentinel;
       cp = &OMP_CLAUSE_CHAIN (*cp))
    {
      if (OMP_CLAUSE_CODE (*cp) != OMP_CLAUSE_MAP)
	continue;

      tree *grp_last_p = omp_group_last (cp);
      omp_mapping_group grp;

      grp.grp_start = cp;
      grp.grp_end = *grp_last_p;
      grp.mark = UNVISITED;
      grp.sibling = NULL;
      grp.deleted = false;
      grp.next = NULL;
      groups->safe_push (grp);

      cp = grp_last_p;
    }
}

static vec<omp_mapping_group> *
omp_gather_mapping_groups (tree *list_p)
{
  vec<omp_mapping_group> *groups = new vec<omp_mapping_group> ();

  omp_gather_mapping_groups_1 (list_p, groups, NULL_TREE);

  if (groups->length () > 0)
    return groups;
  else
    {
      delete groups;
      return NULL;
    }
}

/* A pointer mapping group GRP may define a block of memory starting at some
   base address, and maybe also define a firstprivate pointer or firstprivate
   reference that points to that block.  The return value is a node containing
   the former, and the *FIRSTPRIVATE pointer is set if we have the latter.
   If we define several base pointers, i.e. for a GOMP_MAP_STRUCT mapping,
   return the number of consecutive chained nodes in CHAINED.  */

static tree
omp_group_base (omp_mapping_group *grp, unsigned int *chained,
		tree *firstprivate)
{
  tree node = *grp->grp_start;

  *firstprivate = NULL_TREE;
  *chained = 1;

  switch (OMP_CLAUSE_MAP_KIND (node))
    {
    case GOMP_MAP_TO:
    case GOMP_MAP_FROM:
    case GOMP_MAP_TOFROM:
    case GOMP_MAP_ALWAYS_FROM:
    case GOMP_MAP_ALWAYS_TO:
    case GOMP_MAP_ALWAYS_TOFROM:
    case GOMP_MAP_FORCE_FROM:
    case GOMP_MAP_FORCE_TO:
    case GOMP_MAP_FORCE_TOFROM:
    case GOMP_MAP_FORCE_PRESENT:
    case GOMP_MAP_ALLOC:
    case GOMP_MAP_RELEASE:
    case GOMP_MAP_DELETE:
    case GOMP_MAP_FORCE_ALLOC:
    case GOMP_MAP_IF_PRESENT:
      if (node == grp->grp_end)
	return node;

      node = OMP_CLAUSE_CHAIN (node);
      if (node && OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_TO_PSET)
	{
	  if (node == grp->grp_end)
	    return *grp->grp_start;
	  node = OMP_CLAUSE_CHAIN (node);
	}
      if (node)
	switch (OMP_CLAUSE_MAP_KIND (node))
	  {
	  case GOMP_MAP_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
	    *firstprivate = OMP_CLAUSE_DECL (node);
	    return *grp->grp_start;

	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_ATTACH_DETACH:
	  case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
	    return *grp->grp_start;

	  default:
	    internal_error ("unexpected mapping node");
	  }
      else
	internal_error ("unexpected mapping node");
      return error_mark_node;

    case GOMP_MAP_TO_PSET:
      gcc_assert (node != grp->grp_end);
      node = OMP_CLAUSE_CHAIN (node);
      if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_ATTACH
	  || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_DETACH)
	return NULL_TREE;
      else
	internal_error ("unexpected mapping node");
      return error_mark_node;

    case GOMP_MAP_ATTACH:
    case GOMP_MAP_DETACH:
      node = OMP_CLAUSE_CHAIN (node);
      if (!node || *grp->grp_start == grp->grp_end)
	return NULL_TREE;
      if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_FIRSTPRIVATE_POINTER
	  || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
	{
	  /* We're mapping the base pointer itself in a bare attach or detach
	     node.  This is a side effect of how parsing works, and the mapping
	     will be removed anyway (at least for enter/exit data directives).
	     We should ignore the mapping here.  FIXME.  */
	  return NULL_TREE;
	}
      else
	internal_error ("unexpected mapping node");
      return error_mark_node;

    case GOMP_MAP_STRUCT:
      {
	unsigned HOST_WIDE_INT num_mappings
	  = tree_to_uhwi (OMP_CLAUSE_SIZE (node));
	node = OMP_CLAUSE_CHAIN (node);
	if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_FIRSTPRIVATE_POINTER
	    || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
	  {
	    *firstprivate = OMP_CLAUSE_DECL (node);
	    node = OMP_CLAUSE_CHAIN (node);
	  }
	*chained = num_mappings;
	return node;
      }

    case GOMP_MAP_FORCE_DEVICEPTR:
    case GOMP_MAP_DEVICE_RESIDENT:
    case GOMP_MAP_LINK:
    case GOMP_MAP_FIRSTPRIVATE:
    case GOMP_MAP_FIRSTPRIVATE_INT:
    case GOMP_MAP_USE_DEVICE_PTR:
    case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
      return NULL_TREE;

    case GOMP_MAP_FIRSTPRIVATE_POINTER:
    case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
    case GOMP_MAP_POINTER:
    case GOMP_MAP_ALWAYS_POINTER:
    case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
      /* These shouldn't appear by themselves.  */
      if (!seen_error ())
	internal_error ("unexpected pointer mapping node");
      return error_mark_node;

    default:
      gcc_unreachable ();
    }

  return error_mark_node;
}

/* Given a vector of omp_mapping_groups, build a hash table so we can look up
   nodes by tree_operand_hash_no_se.  */

static void
omp_index_mapping_groups_1 (hash_map<tree_operand_hash_no_se,
				     omp_mapping_group *> *grpmap,
			    vec<omp_mapping_group> *groups,
			    tree reindex_sentinel)
{
  omp_mapping_group *grp;
  unsigned int i;
  bool reindexing = reindex_sentinel != NULL_TREE, above_hwm = false;

  FOR_EACH_VEC_ELT (*groups, i, grp)
    {
      if (reindexing && *grp->grp_start == reindex_sentinel)
	above_hwm = true;

      if (reindexing && !above_hwm)
	continue;

      tree fpp;
      unsigned int chained;
      tree node = omp_group_base (grp, &chained, &fpp);

      if (node == error_mark_node || (!node && !fpp))
	continue;

      for (unsigned j = 0;
	   node && j < chained;
	   node = OMP_CLAUSE_CHAIN (node), j++)
	{
	  tree decl = OMP_CLAUSE_DECL (node);
	  /* Sometimes we see zero-offset MEM_REF instead of INDIRECT_REF,
	     meaning node-hash lookups don't work.  This is a workaround for
	     that, but ideally we should just create the INDIRECT_REF at
	     source instead.  FIXME.  */
	  if (TREE_CODE (decl) == MEM_REF
	      && integer_zerop (TREE_OPERAND (decl, 1)))
	    decl = build_fold_indirect_ref (TREE_OPERAND (decl, 0));

	  omp_mapping_group **prev = grpmap->get (decl);

	  if (prev && *prev == grp)
	    /* Empty.  */;
	  else if (prev)
	    {
	      /* Mapping the same thing twice is normally diagnosed as an error,
		 but can happen under some circumstances, e.g. in pr99928-16.c,
		 the directive:

		 #pragma omp target simd reduction(+:a[:3]) \
					 map(always, tofrom: a[:6])
		 ...

		 will result in two "a[0]" mappings (of different sizes).  */

	      grp->sibling = (*prev)->sibling;
	      (*prev)->sibling = grp;
	    }
	  else
	    grpmap->put (decl, grp);
	}

      if (!fpp)
	continue;

      omp_mapping_group **prev = grpmap->get (fpp);
      if (prev && *prev != grp)
	{
	  grp->sibling = (*prev)->sibling;
	  (*prev)->sibling = grp;
	}
      else
	grpmap->put (fpp, grp);
    }
}

static hash_map<tree_operand_hash_no_se, omp_mapping_group *> *
omp_index_mapping_groups (vec<omp_mapping_group> *groups)
{
  hash_map<tree_operand_hash_no_se, omp_mapping_group *> *grpmap
    = new hash_map<tree_operand_hash_no_se, omp_mapping_group *>;

  omp_index_mapping_groups_1 (grpmap, groups, NULL_TREE);

  return grpmap;
}

/* Rebuild group map from partially-processed clause list (during
   omp_build_struct_sibling_lists).  We have already processed nodes up until
   a high-water mark (HWM).  This is a bit tricky because the list is being
   reordered as it is scanned, but we know:

   1. The list after HWM has not been touched yet, so we can reindex it safely.

   2. The list before and including HWM has been altered, but remains
      well-formed throughout the sibling-list building operation.

   so, we can do the reindex operation in two parts, on the processed and
   then the unprocessed halves of the list.  */

static hash_map<tree_operand_hash_no_se, omp_mapping_group *> *
omp_reindex_mapping_groups (tree *list_p,
			    vec<omp_mapping_group> *groups,
			    vec<omp_mapping_group> *processed_groups,
			    tree sentinel)
{
  hash_map<tree_operand_hash_no_se, omp_mapping_group *> *grpmap
    = new hash_map<tree_operand_hash_no_se, omp_mapping_group *>;

  processed_groups->truncate (0);

  omp_gather_mapping_groups_1 (list_p, processed_groups, sentinel);
  omp_index_mapping_groups_1 (grpmap, processed_groups, NULL_TREE);
  if (sentinel)
    omp_index_mapping_groups_1 (grpmap, groups, sentinel);

  return grpmap;
}

/* Find the immediately-containing struct for a component ref (etc.)
   expression EXPR.  */

static tree
omp_containing_struct (tree expr)
{
  tree expr0 = expr;

  STRIP_NOPS (expr);

  /* Note: don't strip NOPs unless we're also stripping off array refs or a
     component ref.  */
  if (TREE_CODE (expr) != ARRAY_REF && TREE_CODE (expr) != COMPONENT_REF)
    return expr0;

  while (TREE_CODE (expr) == ARRAY_REF)
    expr = TREE_OPERAND (expr, 0);

  if (TREE_CODE (expr) == COMPONENT_REF)
    expr = TREE_OPERAND (expr, 0);

  return expr;
}

/* Return TRUE if DECL describes a component that is part of a whole structure
   that is mapped elsewhere in GRPMAP.  *MAPPED_BY_GROUP is set to the group
   that maps that structure, if present.  */

static bool
omp_mapped_by_containing_struct (hash_map<tree_operand_hash_no_se,
					  omp_mapping_group *> *grpmap,
				 tree decl,
				 omp_mapping_group **mapped_by_group)
{
  tree wsdecl = NULL_TREE;

  *mapped_by_group = NULL;

  while (true)
    {
      wsdecl = omp_containing_struct (decl);
      if (wsdecl == decl)
	break;
      omp_mapping_group **wholestruct = grpmap->get (wsdecl);
      if (!wholestruct
	  && TREE_CODE (wsdecl) == MEM_REF
	  && integer_zerop (TREE_OPERAND (wsdecl, 1)))
	{
	  tree deref = TREE_OPERAND (wsdecl, 0);
	  deref = build_fold_indirect_ref (deref);
	  wholestruct = grpmap->get (deref);
	}
      if (wholestruct)
	{
	  *mapped_by_group = *wholestruct;
	  return true;
	}
      decl = wsdecl;
    }

  return false;
}

/* Helper function for omp_tsort_mapping_groups.  Returns TRUE on success, or
   FALSE on error.  */

static bool
omp_tsort_mapping_groups_1 (omp_mapping_group ***outlist,
			    vec<omp_mapping_group> *groups,
			    hash_map<tree_operand_hash_no_se,
				     omp_mapping_group *> *grpmap,
			    omp_mapping_group *grp)
{
  if (grp->mark == PERMANENT)
    return true;
  if (grp->mark == TEMPORARY)
    {
      fprintf (stderr, "when processing group:\n");
      debug_mapping_group (grp);
      internal_error ("base pointer cycle detected");
      return false;
    }
  grp->mark = TEMPORARY;

  tree attaches_to = omp_get_attachment (grp);

  if (attaches_to)
    {
      omp_mapping_group **basep = grpmap->get (attaches_to);

      if (basep && *basep != grp)
	{
	  for (omp_mapping_group *w = *basep; w; w = w->sibling)
	    if (!omp_tsort_mapping_groups_1 (outlist, groups, grpmap, w))
	      return false;
	}
    }

  tree decl = OMP_CLAUSE_DECL (*grp->grp_start);

  while (decl)
    {
      tree base = omp_get_base_pointer (decl);

      if (!base)
	break;

      omp_mapping_group **innerp = grpmap->get (base);
      omp_mapping_group *wholestruct;

      /* We should treat whole-structure mappings as if all (pointer, in this
	 case) members are mapped as individual list items.  Check if we have
	 such a whole-structure mapping, if we don't have an explicit reference
	 to the pointer member itself.  */
      if (!innerp
	  && TREE_CODE (base) == COMPONENT_REF
	  && omp_mapped_by_containing_struct (grpmap, base, &wholestruct))
	innerp = &wholestruct;

      if (innerp && *innerp != grp)
	{
	  for (omp_mapping_group *w = *innerp; w; w = w->sibling)
	    if (!omp_tsort_mapping_groups_1 (outlist, groups, grpmap, w))
	      return false;
	  break;
	}

      decl = base;
    }

  grp->mark = PERMANENT;

  /* Emit grp to output list.  */

  **outlist = grp;
  *outlist = &grp->next;

  return true;
}

/* Topologically sort GROUPS, so that OMP 5.0-defined base pointers come
   before mappings that use those pointers.  This is an implementation of the
   depth-first search algorithm, described e.g. at:

     https://en.wikipedia.org/wiki/Topological_sorting
*/

static omp_mapping_group *
omp_tsort_mapping_groups (vec<omp_mapping_group> *groups,
			  hash_map<tree_operand_hash_no_se, omp_mapping_group *>
			    *grpmap)
{
  omp_mapping_group *grp, *outlist = NULL, **cursor;
  unsigned int i;

  cursor = &outlist;

  FOR_EACH_VEC_ELT (*groups, i, grp)
    {
      if (grp->mark != PERMANENT)
	if (!omp_tsort_mapping_groups_1 (&cursor, groups, grpmap, grp))
	  return NULL;
    }

  return outlist;
}

/* Split INLIST into two parts, moving groups corresponding to
   ALLOC/RELEASE/DELETE mappings to one list, and other mappings to another.
   The former list is then appended to the latter.  Each sub-list retains the
   order of the original list.
   Note that ATTACH nodes are later moved to the end of the list in
   gimplify_adjust_omp_clauses, for target regions.  */

static omp_mapping_group *
omp_segregate_mapping_groups (omp_mapping_group *inlist)
{
  omp_mapping_group *ard_groups = NULL, *tf_groups = NULL;
  omp_mapping_group **ard_tail = &ard_groups, **tf_tail = &tf_groups;

  for (omp_mapping_group *w = inlist; w;)
    {
      tree c = *w->grp_start;
      omp_mapping_group *next = w->next;

      gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP);

      switch (OMP_CLAUSE_MAP_KIND (c))
	{
	case GOMP_MAP_ALLOC:
	case GOMP_MAP_RELEASE:
	case GOMP_MAP_DELETE:
	  *ard_tail = w;
	  w->next = NULL;
	  ard_tail = &w->next;
	  break;

	default:
	  *tf_tail = w;
	  w->next = NULL;
	  tf_tail = &w->next;
	}

      w = next;
    }

  /* Now splice the lists together...  */
  *tf_tail = ard_groups;

  return tf_groups;
}

/* Given a list LIST_P containing groups of mappings given by GROUPS, reorder
   those groups based on the output list of omp_tsort_mapping_groups --
   singly-linked, threaded through each element's NEXT pointer starting at
   HEAD.  Each list element appears exactly once in that linked list.

   Each element of GROUPS may correspond to one or several mapping nodes.
   Node groups are kept together, and in the reordered list, the positions of
   the original groups are reused for the positions of the reordered list.
   Hence if we have e.g.

     {to ptr ptr} firstprivate {tofrom ptr} ...
      ^             ^           ^
      first group  non-"map"    second group

   and say the second group contains a base pointer for the first so must be
   moved before it, the resulting list will contain:

     {tofrom ptr} firstprivate {to ptr ptr} ...
      ^ prev. second group      ^ prev. first group
*/

static tree *
omp_reorder_mapping_groups (vec<omp_mapping_group> *groups,
			    omp_mapping_group *head,
			    tree *list_p)
{
  omp_mapping_group *grp;
  unsigned int i;
  unsigned numgroups = groups->length ();
  auto_vec<tree> old_heads (numgroups);
  auto_vec<tree *> old_headps (numgroups);
  auto_vec<tree> new_heads (numgroups);
  auto_vec<tree> old_succs (numgroups);
  bool map_at_start = (list_p == (*groups)[0].grp_start);

  tree *new_grp_tail = NULL;

  /* Stash the start & end nodes of each mapping group before we start
     modifying the list.  */
  FOR_EACH_VEC_ELT (*groups, i, grp)
    {
      old_headps.quick_push (grp->grp_start);
      old_heads.quick_push (*grp->grp_start);
      old_succs.quick_push (OMP_CLAUSE_CHAIN (grp->grp_end));
    }

  /* And similarly, the heads of the groups in the order we want to rearrange
     the list to.  */
  for (omp_mapping_group *w = head; w; w = w->next)
    new_heads.quick_push (*w->grp_start);

  FOR_EACH_VEC_ELT (*groups, i, grp)
    {
      gcc_assert (head);

      if (new_grp_tail && old_succs[i - 1] == old_heads[i])
	{
	  /* a {b c d} {e f g} h i j   (original)
	     -->
	     a {k l m} {e f g} h i j   (inserted new group on last iter)
	     -->
	     a {k l m} {n o p} h i j   (this time, chain last group to new one)
		      ^new_grp_tail
	  */
	  *new_grp_tail = new_heads[i];
	}
      else if (new_grp_tail)
	{
	  /* a {b c d} e {f g h} i j k   (original)
	     -->
	     a {l m n} e {f g h} i j k   (gap after last iter's group)
	     -->
	     a {l m n} e {o p q} h i j   (chain last group to old successor)
		      ^new_grp_tail
	   */
	  *new_grp_tail = old_succs[i - 1];
	  *old_headps[i] = new_heads[i];
	}
      else
	{
	  /* The first inserted group -- point to new group, and leave end
	     open.
	     a {b c d} e f
	     -->
	     a {g h i...
	  */
	  *grp->grp_start = new_heads[i];
	}

      new_grp_tail = &OMP_CLAUSE_CHAIN (head->grp_end);

      head = head->next;
    }

  if (new_grp_tail)
    *new_grp_tail = old_succs[numgroups - 1];

  gcc_assert (!head);

  return map_at_start ? (*groups)[0].grp_start : list_p;
}

/* DECL is supposed to have lastprivate semantics in the outer contexts
   of combined/composite constructs, starting with OCTX.
   Add needed lastprivate, shared or map clause if no data sharing or
   mapping clause are present.  IMPLICIT_P is true if it is an implicit
   clause (IV on simd), in which case the lastprivate will not be
   copied to some constructs.  */

static void
omp_lastprivate_for_combined_outer_constructs (struct gimplify_omp_ctx *octx,
					       tree decl, bool implicit_p)
{
  struct gimplify_omp_ctx *orig_octx = octx;
  for (; octx; octx = octx->outer_context)
    {
      if ((octx->region_type == ORT_COMBINED_PARALLEL
	   || (octx->region_type & ORT_COMBINED_TEAMS) == ORT_COMBINED_TEAMS)
	  && splay_tree_lookup (octx->variables,
				(splay_tree_key) decl) == NULL)
	{
	  omp_add_variable (octx, decl, GOVD_SHARED | GOVD_SEEN);
	  continue;
	}
      if ((octx->region_type & ORT_TASK) != 0
	  && octx->combined_loop
	  && splay_tree_lookup (octx->variables,
				(splay_tree_key) decl) == NULL)
	{
	  omp_add_variable (octx, decl, GOVD_LASTPRIVATE | GOVD_SEEN);
	  continue;
	}
      if (implicit_p
	  && octx->region_type == ORT_WORKSHARE
	  && octx->combined_loop
	  && splay_tree_lookup (octx->variables,
				(splay_tree_key) decl) == NULL
	  && octx->outer_context
	  && octx->outer_context->region_type == ORT_COMBINED_PARALLEL
	  && splay_tree_lookup (octx->outer_context->variables,
				(splay_tree_key) decl) == NULL)
	{
	  octx = octx->outer_context;
	  omp_add_variable (octx, decl, GOVD_LASTPRIVATE | GOVD_SEEN);
	  continue;
	}
      if ((octx->region_type == ORT_WORKSHARE || octx->region_type == ORT_ACC)
	  && octx->combined_loop
	  && splay_tree_lookup (octx->variables,
				(splay_tree_key) decl) == NULL
	  && !omp_check_private (octx, decl, false))
	{
	  omp_add_variable (octx, decl, GOVD_LASTPRIVATE | GOVD_SEEN);
	  continue;
	}
      if (octx->region_type == ORT_COMBINED_TARGET)
	{
	  splay_tree_node n = splay_tree_lookup (octx->variables,
						 (splay_tree_key) decl);
	  if (n == NULL)
	    {
	      omp_add_variable (octx, decl, GOVD_MAP | GOVD_SEEN);
	      octx = octx->outer_context;
	    }
	  else if (!implicit_p
		   && (n->value & GOVD_FIRSTPRIVATE_IMPLICIT))
	    {
	      n->value &= ~(GOVD_FIRSTPRIVATE
			    | GOVD_FIRSTPRIVATE_IMPLICIT
			    | GOVD_EXPLICIT);
	      omp_add_variable (octx, decl, GOVD_MAP | GOVD_SEEN);
	      octx = octx->outer_context;
	    }
	}
      break;
    }
  if (octx && (implicit_p || octx != orig_octx))
    omp_notice_variable (octx, decl, true);
}

/* If we have mappings INNER and OUTER, where INNER is a component access and
   OUTER is a mapping of the whole containing struct, check that the mappings
   are compatible.  We'll be deleting the inner mapping, so we need to make
   sure the outer mapping does (at least) the same transfers to/from the device
   as the inner mapping.  */

bool
omp_check_mapping_compatibility (location_t loc,
				 omp_mapping_group *outer,
				 omp_mapping_group *inner)
{
  tree first_outer = *outer->grp_start, first_inner = *inner->grp_start;

  gcc_assert (OMP_CLAUSE_CODE (first_outer) == OMP_CLAUSE_MAP);
  gcc_assert (OMP_CLAUSE_CODE (first_inner) == OMP_CLAUSE_MAP);

  enum gomp_map_kind outer_kind = OMP_CLAUSE_MAP_KIND (first_outer);
  enum gomp_map_kind inner_kind = OMP_CLAUSE_MAP_KIND (first_inner);

  if (outer_kind == inner_kind)
    return true;

  switch (outer_kind)
    {
    case GOMP_MAP_ALWAYS_TO:
      if (inner_kind == GOMP_MAP_FORCE_PRESENT
	  || inner_kind == GOMP_MAP_ALLOC
	  || inner_kind == GOMP_MAP_TO)
	return true;
      break;

    case GOMP_MAP_ALWAYS_FROM:
      if (inner_kind == GOMP_MAP_FORCE_PRESENT
	  || inner_kind == GOMP_MAP_ALLOC
	  || inner_kind == GOMP_MAP_FROM)
	return true;
      break;

    case GOMP_MAP_TO:
    case GOMP_MAP_FROM:
      if (inner_kind == GOMP_MAP_FORCE_PRESENT
	  || inner_kind == GOMP_MAP_ALLOC)
	return true;
      break;

    case GOMP_MAP_ALWAYS_TOFROM:
    case GOMP_MAP_TOFROM:
      if (inner_kind == GOMP_MAP_FORCE_PRESENT
	  || inner_kind == GOMP_MAP_ALLOC
	  || inner_kind == GOMP_MAP_TO
	  || inner_kind == GOMP_MAP_FROM
	  || inner_kind == GOMP_MAP_TOFROM)
	return true;
      break;

    default:
      ;
    }

  error_at (loc, "data movement for component %qE is not compatible with "
	    "movement for struct %qE", OMP_CLAUSE_DECL (first_inner),
	    OMP_CLAUSE_DECL (first_outer));

  return false;
}

/* Similar to omp_resolve_clause_dependencies, but for OpenACC.  The only
   clause dependencies we handle for now are struct element mappings and
   whole-struct mappings on the same directive, and duplicate clause
   detection.  */

void
oacc_resolve_clause_dependencies (vec<omp_mapping_group> *groups,
				  hash_map<tree_operand_hash_no_se,
					   omp_mapping_group *> *grpmap)
{
  int i;
  omp_mapping_group *grp;
  hash_set<tree_operand_hash> *seen_components = NULL;
  hash_set<tree_operand_hash> *shown_error = NULL;

  FOR_EACH_VEC_ELT (*groups, i, grp)
    {
      tree grp_end = grp->grp_end;
      tree decl = OMP_CLAUSE_DECL (grp_end);

      gcc_assert (OMP_CLAUSE_CODE (grp_end) == OMP_CLAUSE_MAP);

      if (DECL_P (grp_end))
	continue;

      tree c = OMP_CLAUSE_DECL (*grp->grp_start);
      while (TREE_CODE (c) == ARRAY_REF)
	c = TREE_OPERAND (c, 0);
      if (TREE_CODE (c) != COMPONENT_REF)
	continue;
      if (!seen_components)
	seen_components = new hash_set<tree_operand_hash> ();
      if (!shown_error)
	shown_error = new hash_set<tree_operand_hash> ();
      if (seen_components->contains (c)
	  && !shown_error->contains (c))
	{
	  error_at (OMP_CLAUSE_LOCATION (grp_end),
		    "%qE appears more than once in map clauses",
		    OMP_CLAUSE_DECL (grp_end));
	  shown_error->add (c);
	}
      else
	seen_components->add (c);

      omp_mapping_group *struct_group;
      if (omp_mapped_by_containing_struct (grpmap, decl, &struct_group)
	  && *grp->grp_start == grp_end)
	{
	  omp_check_mapping_compatibility (OMP_CLAUSE_LOCATION (grp_end),
					   struct_group, grp);
	  /* Remove the whole of this mapping -- redundant.  */
	  grp->deleted = true;
	}
    }

  if (seen_components)
    delete seen_components;
  if (shown_error)
    delete shown_error;
}

/* Link node NEWNODE so it is pointed to by chain INSERT_AT.  NEWNODE's chain
   is linked to the previous node pointed to by INSERT_AT.  */

static tree *
omp_siblist_insert_node_after (tree newnode, tree *insert_at)
{
  OMP_CLAUSE_CHAIN (newnode) = *insert_at;
  *insert_at = newnode;
  return &OMP_CLAUSE_CHAIN (newnode);
}

/* Move NODE (which is currently pointed to by the chain OLD_POS) so it is
   pointed to by chain MOVE_AFTER instead.  */

static void
omp_siblist_move_node_after (tree node, tree *old_pos, tree *move_after)
{
  gcc_assert (node == *old_pos);
  *old_pos = OMP_CLAUSE_CHAIN (node);
  OMP_CLAUSE_CHAIN (node) = *move_after;
  *move_after = node;
}

/* Move nodes from FIRST_PTR (pointed to by previous node's chain) to
   LAST_NODE to after MOVE_AFTER chain.  Similar to below function, but no
   new nodes are prepended to the list before splicing into the new position.
   Return the position we should continue scanning the list at, or NULL to
   stay where we were.  */

static tree *
omp_siblist_move_nodes_after (tree *first_ptr, tree last_node,
			      tree *move_after)
{
  if (first_ptr == move_after)
    return NULL;

  tree tmp = *first_ptr;
  *first_ptr = OMP_CLAUSE_CHAIN (last_node);
  OMP_CLAUSE_CHAIN (last_node) = *move_after;
  *move_after = tmp;

  return first_ptr;
}

/* Concatenate two lists described by [FIRST_NEW, LAST_NEW_TAIL] and
   [FIRST_PTR, LAST_NODE], and insert them in the OMP clause list after chain
   pointer MOVE_AFTER.

   The latter list was previously part of the OMP clause list, and the former
   (prepended) part is comprised of new nodes.

   We start with a list of nodes starting with a struct mapping node.  We
   rearrange the list so that new nodes starting from FIRST_NEW and whose last
   node's chain is LAST_NEW_TAIL comes directly after MOVE_AFTER, followed by
   the group of mapping nodes we are currently processing (from the chain
   FIRST_PTR to LAST_NODE).  The return value is the pointer to the next chain
   we should continue processing from, or NULL to stay where we were.

   The transformation (in the case where MOVE_AFTER and FIRST_PTR are
   different) is worked through below.  Here we are processing LAST_NODE, and
   FIRST_PTR points at the preceding mapping clause:

  #. mapping node		chain
  ---------------------------------------------------
  A. struct_node		[->B]
  B. comp_1			[->C]
  C. comp_2			[->D (move_after)]
  D. map_to_3			[->E]
  E. attach_3			[->F (first_ptr)]
  F. map_to_4			[->G (continue_at)]
  G. attach_4 (last_node)	[->H]
  H. ...

     *last_new_tail = *first_ptr;

  I. new_node (first_new)	[->F (last_new_tail)]

     *first_ptr = OMP_CLAUSE_CHAIN (last_node)

  #. mapping node		chain
  ----------------------------------------------------
  A. struct_node		[->B]
  B. comp_1			[->C]
  C. comp_2			[->D (move_after)]
  D. map_to_3			[->E]
  E. attach_3			[->H (first_ptr)]
  F. map_to_4			[->G (continue_at)]
  G. attach_4 (last_node)	[->H]
  H. ...

  I. new_node (first_new)	[->F  (last_new_tail)]

     OMP_CLAUSE_CHAIN (last_node) = *move_after;

  #. mapping node		chain
  ---------------------------------------------------
  A. struct_node		[->B]
  B. comp_1			[->C]
  C. comp_2			[->D (move_after)]
  D. map_to_3			[->E]
  E. attach_3			[->H (continue_at)]
  F. map_to_4			[->G]
  G. attach_4 (last_node)	[->D]
  H. ...

  I. new_node (first_new)	[->F  (last_new_tail)]

     *move_after = first_new;

  #. mapping node		chain
  ---------------------------------------------------
  A. struct_node		[->B]
  B. comp_1			[->C]
  C. comp_2			[->I (move_after)]
  D. map_to_3			[->E]
  E. attach_3			[->H (continue_at)]
  F. map_to_4			[->G]
  G. attach_4 (last_node)	[->D]
  H. ...
  I. new_node (first_new)	[->F (last_new_tail)]

  or, in order:

  #. mapping node		chain
  ---------------------------------------------------
  A. struct_node		[->B]
  B. comp_1			[->C]
  C. comp_2			[->I (move_after)]
  I. new_node (first_new)	[->F (last_new_tail)]
  F. map_to_4			[->G]
  G. attach_4 (last_node)	[->D]
  D. map_to_3			[->E]
  E. attach_3			[->H (continue_at)]
  H. ...
*/

static tree *
omp_siblist_move_concat_nodes_after (tree first_new, tree *last_new_tail,
				     tree *first_ptr, tree last_node,
				     tree *move_after)
{
  tree *continue_at = NULL;
  *last_new_tail = *first_ptr;
  if (first_ptr == move_after)
    *move_after = first_new;
  else
    {
      *first_ptr = OMP_CLAUSE_CHAIN (last_node);
      continue_at = first_ptr;
      OMP_CLAUSE_CHAIN (last_node) = *move_after;
      *move_after = first_new;
    }
  return continue_at;
}

/* Mapping struct members causes an additional set of nodes to be created,
   starting with GOMP_MAP_STRUCT followed by a number of mappings equal to the
   number of members being mapped, in order of ascending position (address or
   bitwise).

   We scan through the list of mapping clauses, calling this function for each
   struct member mapping we find, and build up the list of mappings after the
   initial GOMP_MAP_STRUCT node.  For pointer members, these will be
   newly-created ALLOC nodes.  For non-pointer members, the existing mapping is
   moved into place in the sorted list.

     struct {
       int *a;
       int *b;
       int c;
       int *d;
     };

     #pragma (acc|omp directive) copy(struct.a[0:n], struct.b[0:n], struct.c,
				      struct.d[0:n])

     GOMP_MAP_STRUCT (4)
     [GOMP_MAP_FIRSTPRIVATE_REFERENCE -- for refs to structs]
     GOMP_MAP_ALLOC  (struct.a)
     GOMP_MAP_ALLOC  (struct.b)
     GOMP_MAP_TO     (struct.c)
     GOMP_MAP_ALLOC  (struct.d)
     ...

   In the case where we are mapping references to pointers, or in Fortran if
   we are mapping an array with a descriptor, additional nodes may be created
   after the struct node list also.

   The return code is either a pointer to the next node to process (if the
   list has been rearranged), else NULL to continue with the next node in the
   original list.  */

static tree *
omp_accumulate_sibling_list (enum omp_region_type region_type,
			     enum tree_code code,
			     hash_map<tree_operand_hash, tree>
			       *&struct_map_to_clause, tree *grp_start_p,
			     tree grp_end, tree *inner)
{
  poly_offset_int coffset;
  poly_int64 cbitpos;
  tree ocd = OMP_CLAUSE_DECL (grp_end);
  bool openmp = !(region_type & ORT_ACC);
  tree *continue_at = NULL;

  while (TREE_CODE (ocd) == ARRAY_REF)
    ocd = TREE_OPERAND (ocd, 0);

  if (TREE_CODE (ocd) == INDIRECT_REF)
    ocd = TREE_OPERAND (ocd, 0);

  tree base = extract_base_bit_offset (ocd, &cbitpos, &coffset);

  bool ptr = (OMP_CLAUSE_MAP_KIND (grp_end) == GOMP_MAP_ALWAYS_POINTER);
  bool attach_detach = ((OMP_CLAUSE_MAP_KIND (grp_end)
			 == GOMP_MAP_ATTACH_DETACH)
			|| (OMP_CLAUSE_MAP_KIND (grp_end)
			    == GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION));
  bool attach = (OMP_CLAUSE_MAP_KIND (grp_end) == GOMP_MAP_ATTACH
		 || OMP_CLAUSE_MAP_KIND (grp_end) == GOMP_MAP_DETACH);

  /* FIXME: If we're not mapping the base pointer in some other clause on this
     directive, I think we want to create ALLOC/RELEASE here -- i.e. not
     early-exit.  */
  if (openmp && attach_detach)
    return NULL;

  if (!struct_map_to_clause || struct_map_to_clause->get (base) == NULL)
    {
      tree l = build_omp_clause (OMP_CLAUSE_LOCATION (grp_end), OMP_CLAUSE_MAP);
      gomp_map_kind k = attach ? GOMP_MAP_FORCE_PRESENT : GOMP_MAP_STRUCT;

      OMP_CLAUSE_SET_MAP_KIND (l, k);

      OMP_CLAUSE_DECL (l) = unshare_expr (base);

      OMP_CLAUSE_SIZE (l)
	= (!attach ? size_int (1)
	   : (DECL_P (OMP_CLAUSE_DECL (l))
	      ? DECL_SIZE_UNIT (OMP_CLAUSE_DECL (l))
	      : TYPE_SIZE_UNIT (TREE_TYPE (OMP_CLAUSE_DECL (l)))));
      if (struct_map_to_clause == NULL)
	struct_map_to_clause = new hash_map<tree_operand_hash, tree>;
      struct_map_to_clause->put (base, l);

      if (ptr || attach_detach)
	{
	  tree extra_node;
	  tree alloc_node
	    = build_omp_struct_comp_nodes (code, *grp_start_p, grp_end,
					   &extra_node);
	  OMP_CLAUSE_CHAIN (l) = alloc_node;

	  tree *insert_node_pos = grp_start_p;

	  if (extra_node)
	    {
	      OMP_CLAUSE_CHAIN (extra_node) = *insert_node_pos;
	      OMP_CLAUSE_CHAIN (alloc_node) = extra_node;
	    }
	  else
	    OMP_CLAUSE_CHAIN (alloc_node) = *insert_node_pos;

	  *insert_node_pos = l;
	}
      else
	{
	  gcc_assert (*grp_start_p == grp_end);
	  grp_start_p = omp_siblist_insert_node_after (l, grp_start_p);
	}

      tree noind = omp_strip_indirections (base);

      if (!openmp
	  && (region_type & ORT_TARGET)
	  && TREE_CODE (noind) == COMPONENT_REF)
	{
	  /* The base for this component access is a struct component access
	     itself.  Insert a node to be processed on the next iteration of
	     our caller's loop, which will subsequently be turned into a new,
	     inner GOMP_MAP_STRUCT mapping.

	     We need to do this else the non-DECL_P base won't be
	     rewritten correctly in the offloaded region.  */
	  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (grp_end),
				      OMP_CLAUSE_MAP);
	  OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_FORCE_PRESENT);
	  OMP_CLAUSE_DECL (c2) = unshare_expr (noind);
	  OMP_CLAUSE_SIZE (c2) = TYPE_SIZE_UNIT (TREE_TYPE (noind));
	  *inner = c2;
	  return NULL;
	}

      tree sdecl = omp_strip_components_and_deref (base);

      if (POINTER_TYPE_P (TREE_TYPE (sdecl)) && (region_type & ORT_TARGET))
	{
	  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (grp_end),
				      OMP_CLAUSE_MAP);
	  bool base_ref
	    = (TREE_CODE (base) == INDIRECT_REF
	       && ((TREE_CODE (TREE_TYPE (TREE_OPERAND (base, 0)))
		    == REFERENCE_TYPE)
		   || ((TREE_CODE (TREE_OPERAND (base, 0))
			== INDIRECT_REF)
		       && (TREE_CODE (TREE_TYPE (TREE_OPERAND
						  (TREE_OPERAND (base, 0), 0)))
			   == REFERENCE_TYPE))));
	  enum gomp_map_kind mkind = base_ref ? GOMP_MAP_FIRSTPRIVATE_REFERENCE
					      : GOMP_MAP_FIRSTPRIVATE_POINTER;
	  OMP_CLAUSE_SET_MAP_KIND (c2, mkind);
	  OMP_CLAUSE_DECL (c2) = sdecl;
	  tree baddr = build_fold_addr_expr (base);
	  baddr = fold_convert_loc (OMP_CLAUSE_LOCATION (grp_end),
				    ptrdiff_type_node, baddr);
	  /* This isn't going to be good enough when we add support for more
	     complicated lvalue expressions.  FIXME.  */
	  if (TREE_CODE (TREE_TYPE (sdecl)) == REFERENCE_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (sdecl))) == POINTER_TYPE)
	    sdecl = build_simple_mem_ref (sdecl);
	  tree decladdr = fold_convert_loc (OMP_CLAUSE_LOCATION (grp_end),
					    ptrdiff_type_node, sdecl);
	  OMP_CLAUSE_SIZE (c2)
	    = fold_build2_loc (OMP_CLAUSE_LOCATION (grp_end), MINUS_EXPR,
			       ptrdiff_type_node, baddr, decladdr);
	  /* Insert after struct node.  */
	  OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (l);
	  OMP_CLAUSE_CHAIN (l) = c2;
	}

      return NULL;
    }
  else if (struct_map_to_clause)
    {
      tree *osc = struct_map_to_clause->get (base);
      tree *sc = NULL, *scp = NULL;
      sc = &OMP_CLAUSE_CHAIN (*osc);
      /* The struct mapping might be immediately followed by a
	 FIRSTPRIVATE_POINTER and/or FIRSTPRIVATE_REFERENCE -- if it's an
	 indirect access or a reference, or both.  (This added node is removed
	 in omp-low.c after it has been processed there.)  */
      if (*sc != grp_end
	  && (OMP_CLAUSE_MAP_KIND (*sc) == GOMP_MAP_FIRSTPRIVATE_POINTER
	      || OMP_CLAUSE_MAP_KIND (*sc) == GOMP_MAP_FIRSTPRIVATE_REFERENCE))
	sc = &OMP_CLAUSE_CHAIN (*sc);
      for (; *sc != grp_end; sc = &OMP_CLAUSE_CHAIN (*sc))
	if ((ptr || attach_detach) && sc == grp_start_p)
	  break;
	else if (TREE_CODE (OMP_CLAUSE_DECL (*sc)) != COMPONENT_REF
		 && TREE_CODE (OMP_CLAUSE_DECL (*sc)) != INDIRECT_REF
		 && TREE_CODE (OMP_CLAUSE_DECL (*sc)) != ARRAY_REF)
	  break;
	else
	  {
	    tree sc_decl = OMP_CLAUSE_DECL (*sc);
	    poly_offset_int offset;
	    poly_int64 bitpos;

	    if (TREE_CODE (sc_decl) == ARRAY_REF)
	      {
		while (TREE_CODE (sc_decl) == ARRAY_REF)
		  sc_decl = TREE_OPERAND (sc_decl, 0);
		if (TREE_CODE (sc_decl) != COMPONENT_REF
		    || TREE_CODE (TREE_TYPE (sc_decl)) != ARRAY_TYPE)
		  break;
	      }
	    else if (TREE_CODE (sc_decl) == INDIRECT_REF
		     && TREE_CODE (TREE_OPERAND (sc_decl, 0)) == COMPONENT_REF
		     && (TREE_CODE (TREE_TYPE (TREE_OPERAND (sc_decl, 0)))
			 == REFERENCE_TYPE))
	      sc_decl = TREE_OPERAND (sc_decl, 0);

	    tree base2 = extract_base_bit_offset (sc_decl, &bitpos, &offset);
	    if (!base2 || !operand_equal_p (base2, base, 0))
	      break;
	    if (scp)
	      continue;
	    if (maybe_lt (coffset, offset)
		|| (known_eq (coffset, offset)
		    && maybe_lt (cbitpos, bitpos)))
	      {
		if (ptr || attach_detach)
		  scp = sc;
		else
		  break;
	      }
	  }

      if (!attach)
	OMP_CLAUSE_SIZE (*osc)
	  = size_binop (PLUS_EXPR, OMP_CLAUSE_SIZE (*osc), size_one_node);
      if (ptr || attach_detach)
	{
	  tree cl = NULL_TREE, extra_node;
	  tree alloc_node = build_omp_struct_comp_nodes (code, *grp_start_p,
							 grp_end, &extra_node);
	  tree *tail_chain = NULL;

	  /* Here, we have:

	     grp_end : the last (or only) node in this group.
	     grp_start_p : pointer to the first node in a pointer mapping group
			   up to and including GRP_END.
	     sc : pointer to the chain for the end of the struct component
		  list.
	     scp : pointer to the chain for the sorted position at which we
		   should insert in the middle of the struct component list
		   (else NULL to insert at end).
	     alloc_node : the "alloc" node for the structure (pointer-type)
			  component. We insert at SCP (if present), else SC
			  (the end of the struct component list).
	     extra_node : a newly-synthesized node for an additional indirect
			  pointer mapping or a Fortran pointer set, if needed.
	     cl : first node to prepend before grp_start_p.
	     tail_chain : pointer to chain of last prepended node.

	     The general idea is we move the nodes for this struct mapping
	     together: the alloc node goes into the sorted list directly after
	     the struct mapping, and any extra nodes (together with the nodes
	     mapping arrays pointed to by struct components) get moved after
	     that list.  When SCP is NULL, we insert the nodes at SC, i.e. at
	     the end of the struct component mapping list.  It's important that
	     the alloc_node comes first in that case because it's part of the
	     sorted component mapping list (but subsequent nodes are not!).  */

	  if (scp)
	    omp_siblist_insert_node_after (alloc_node, scp);

	  /* Make [cl,tail_chain] a list of the alloc node (if we haven't
	     already inserted it) and the extra_node (if it is present).  The
	     list can be empty if we added alloc_node above and there is no
	     extra node.  */
	  if (scp && extra_node)
	    {
	      cl = extra_node;
	      tail_chain = &OMP_CLAUSE_CHAIN (extra_node);
	    }
	  else if (extra_node)
	    {
	      OMP_CLAUSE_CHAIN (alloc_node) = extra_node;
	      cl = alloc_node;
	      tail_chain = &OMP_CLAUSE_CHAIN (extra_node);
	    }
	  else if (!scp)
	    {
	      cl = alloc_node;
	      tail_chain = &OMP_CLAUSE_CHAIN (alloc_node);
	    }

	  continue_at
	    = cl ? omp_siblist_move_concat_nodes_after (cl, tail_chain,
							grp_start_p, grp_end,
							sc)
		 : omp_siblist_move_nodes_after (grp_start_p, grp_end, sc);
	}
      else if (*sc != grp_end)
	{
	  gcc_assert (*grp_start_p == grp_end);

	  /* We are moving the current node back to a previous struct node:
	     the node that used to point to the current node will now point to
	     the next node.  */
	  continue_at = grp_start_p;
	  /* In the non-pointer case, the mapping clause itself is moved into
	     the correct position in the struct component list, which in this
	     case is just SC.  */
	  omp_siblist_move_node_after (*grp_start_p, grp_start_p, sc);
	}
    }
  return continue_at;
}

/* Scan through GROUPS, and create sorted structure sibling lists without
   gimplifying.  */

static bool
omp_build_struct_sibling_lists (enum tree_code code,
				enum omp_region_type region_type,
				vec<omp_mapping_group> *groups,
				hash_map<tree_operand_hash_no_se,
					 omp_mapping_group *> **grpmap,
				tree *list_p)
{
  unsigned i;
  omp_mapping_group *grp;
  hash_map<tree_operand_hash, tree> *struct_map_to_clause = NULL;
  bool success = true;
  tree *new_next = NULL;
  tree *tail = &OMP_CLAUSE_CHAIN ((*groups)[groups->length () - 1].grp_end);
  auto_vec<omp_mapping_group> pre_hwm_groups;

  FOR_EACH_VEC_ELT (*groups, i, grp)
    {
      tree c = grp->grp_end;
      tree decl = OMP_CLAUSE_DECL (c);
      tree grp_end = grp->grp_end;
      tree sentinel = OMP_CLAUSE_CHAIN (grp_end);

      if (new_next)
	grp->grp_start = new_next;

      new_next = NULL;

      tree *grp_start_p = grp->grp_start;

      if (DECL_P (decl))
	continue;

      /* Skip groups we marked for deletion in
	 oacc_resolve_clause_dependencies.  */
      if (grp->deleted)
	continue;

      if (OMP_CLAUSE_CHAIN (*grp_start_p)
	  && OMP_CLAUSE_CHAIN (*grp_start_p) != grp_end)
	{
	  /* Don't process an array descriptor that isn't inside a derived type
	     as a struct (the GOMP_MAP_POINTER following will have the form
	     "var.data", but such mappings are handled specially).  */
	  tree grpmid = OMP_CLAUSE_CHAIN (*grp_start_p);
	  if (OMP_CLAUSE_CODE (grpmid) == OMP_CLAUSE_MAP
	      && OMP_CLAUSE_MAP_KIND (grpmid) == GOMP_MAP_TO_PSET
	      && DECL_P (OMP_CLAUSE_DECL (grpmid)))
	    continue;
	}

      tree d = decl;
      if (TREE_CODE (d) == ARRAY_REF)
	{
	  while (TREE_CODE (d) == ARRAY_REF)
	    d = TREE_OPERAND (d, 0);
	  if (TREE_CODE (d) == COMPONENT_REF
	      && TREE_CODE (TREE_TYPE (d)) == ARRAY_TYPE)
	    decl = d;
	}
      if (d == decl
	  && TREE_CODE (decl) == INDIRECT_REF
	  && TREE_CODE (TREE_OPERAND (decl, 0)) == COMPONENT_REF
	  && (TREE_CODE (TREE_TYPE (TREE_OPERAND (decl, 0)))
	      == REFERENCE_TYPE)
	  && (OMP_CLAUSE_MAP_KIND (c)
	      != GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION))
	decl = TREE_OPERAND (decl, 0);

      STRIP_NOPS (decl);

      if (TREE_CODE (decl) != COMPONENT_REF)
	continue;

      /* If we're mapping the whole struct in another node, skip adding this
	 node to a sibling list.  */
      omp_mapping_group *wholestruct;
      if (omp_mapped_by_containing_struct (*grpmap, OMP_CLAUSE_DECL (c),
					   &wholestruct))
	{
	  if (!(region_type & ORT_ACC)
	      && *grp_start_p == grp_end)
	    /* Remove the whole of this mapping -- redundant.  */
	    grp->deleted = true;

	  continue;
	}

      if (OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_TO_PSET
	  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ATTACH
	  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_DETACH
	  && code != OACC_UPDATE
	  && code != OMP_TARGET_UPDATE)
	{
	  if (error_operand_p (decl))
	    {
	      success = false;
	      goto error_out;
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
	      success = false;
	      goto error_out;
	    }

	  tree inner = NULL_TREE;

	  new_next
	    = omp_accumulate_sibling_list (region_type, code,
					   struct_map_to_clause, grp_start_p,
					   grp_end, &inner);

	  if (inner)
	    {
	      if (new_next && *new_next == NULL_TREE)
		*new_next = inner;
	      else
		*tail = inner;

	      OMP_CLAUSE_CHAIN (inner) = NULL_TREE;
	      omp_mapping_group newgrp;
	      newgrp.grp_start = new_next ? new_next : tail;
	      newgrp.grp_end = inner;
	      newgrp.mark = UNVISITED;
	      newgrp.sibling = NULL;
	      newgrp.deleted = false;
	      newgrp.next = NULL;
	      groups->safe_push (newgrp);

	      /* !!! Growing GROUPS might invalidate the pointers in the group
		 map.  Rebuild it here.  This is a bit inefficient, but
		 shouldn't happen very often.  */
	      delete (*grpmap);
	      *grpmap
		= omp_reindex_mapping_groups (list_p, groups, &pre_hwm_groups,
					      sentinel);

	      tail = &OMP_CLAUSE_CHAIN (inner);
	    }
	}
    }

  /* Delete groups marked for deletion above.  At this point the order of the
     groups may no longer correspond to the order of the underlying list,
     which complicates this a little.  First clear out OMP_CLAUSE_DECL for
     deleted nodes...  */

  FOR_EACH_VEC_ELT (*groups, i, grp)
    if (grp->deleted)
      for (tree d = *grp->grp_start;
	   d != OMP_CLAUSE_CHAIN (grp->grp_end);
	   d = OMP_CLAUSE_CHAIN (d))
	OMP_CLAUSE_DECL (d) = NULL_TREE;

  /* ...then sweep through the list removing the now-empty nodes.  */

  tail = list_p;
  while (*tail)
    {
      if (OMP_CLAUSE_CODE (*tail) == OMP_CLAUSE_MAP
	  && OMP_CLAUSE_DECL (*tail) == NULL_TREE)
	*tail = OMP_CLAUSE_CHAIN (*tail);
      else
	tail = &OMP_CLAUSE_CHAIN (*tail);
    }

error_out:
  if (struct_map_to_clause)
    delete struct_map_to_clause;

  return success;
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
  tree *prev_list_p = NULL, *orig_list_p = list_p;
  int handled_depend_iterators = -1;
  int nowait = -1;

  ctx = new_omp_context (region_type);
  ctx->code = code;
  outer_ctx = ctx->outer_context;
  if (code == OMP_TARGET)
    {
      if (!lang_GNU_Fortran ())
	ctx->defaultmap[GDMK_POINTER] = GOVD_MAP | GOVD_MAP_0LEN_ARRAY;
      ctx->defaultmap[GDMK_SCALAR] = GOVD_FIRSTPRIVATE;
      ctx->defaultmap[GDMK_SCALAR_TARGET] = (lang_GNU_Fortran ()
					     ? GOVD_MAP : GOVD_FIRSTPRIVATE);
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
      case OACC_PARALLEL:
      case OACC_KERNELS:
	ctx->target_firstprivatize_array_bases = true;
      default:
	break;
      }

  if (code == OMP_TARGET
      || code == OMP_TARGET_DATA
      || code == OMP_TARGET_ENTER_DATA
      || code == OMP_TARGET_EXIT_DATA)
    {
      vec<omp_mapping_group> *groups;
      groups = omp_gather_mapping_groups (list_p);
      if (groups)
	{
	  hash_map<tree_operand_hash_no_se, omp_mapping_group *> *grpmap;
	  grpmap = omp_index_mapping_groups (groups);

	  omp_build_struct_sibling_lists (code, region_type, groups, &grpmap,
					  list_p);

	  omp_mapping_group *outlist = NULL;

	  /* Topological sorting may fail if we have duplicate nodes, which
	     we should have detected and shown an error for already.  Skip
	     sorting in that case.  */
	  if (seen_error ())
	    goto failure;

	  delete grpmap;
	  delete groups;

	  /* Rebuild now we have struct sibling lists.  */
	  groups = omp_gather_mapping_groups (list_p);
	  grpmap = omp_index_mapping_groups (groups);

	  outlist = omp_tsort_mapping_groups (groups, grpmap);
	  outlist = omp_segregate_mapping_groups (outlist);
	  list_p = omp_reorder_mapping_groups (groups, outlist, list_p);

	failure:
	  delete grpmap;
	  delete groups;
	}
    }
  else if (region_type & ORT_ACC)
    {
      vec<omp_mapping_group> *groups;
      groups = omp_gather_mapping_groups (list_p);
      if (groups)
	{
	  hash_map<tree_operand_hash_no_se, omp_mapping_group *> *grpmap;
	  grpmap = omp_index_mapping_groups (groups);

	  oacc_resolve_clause_dependencies (groups, grpmap);
	  omp_build_struct_sibling_lists (code, region_type, groups, &grpmap,
					  list_p);

	  delete groups;
	  delete grpmap;
	}
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
	  if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
	    {
	      gcc_assert (code == OMP_TARGET);
	      flags |= GOVD_FIRSTPRIVATE_IMPLICIT;
	    }
	  goto do_add;
	case OMP_CLAUSE_LASTPRIVATE:
	  if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c))
	    switch (code)
	      {
	      case OMP_DISTRIBUTE:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "conditional %<lastprivate%> clause on "
			  "%qs construct", "distribute");
		OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c) = 0;
		break;
	      case OMP_TASKLOOP:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "conditional %<lastprivate%> clause on "
			  "%qs construct", "taskloop");
		OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c) = 0;
		break;
	      default:
		break;
	      }
	  flags = GOVD_LASTPRIVATE | GOVD_SEEN | GOVD_EXPLICIT;
	  if (code != OMP_LOOP)
	    check_non_private = "lastprivate";
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    goto do_add;
	  if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c)
	      && !lang_hooks.decls.omp_scalar_p (decl, true))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"non-scalar variable %qD in conditional "
			"%<lastprivate%> clause", decl);
	      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c) = 0;
	    }
	  if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c))
	    flags |= GOVD_LASTPRIVATE_CONDITIONAL;
	  omp_lastprivate_for_combined_outer_constructs (outer_ctx, decl,
							 false);
	  goto do_add;
	case OMP_CLAUSE_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_TASK (c))
	    {
	      if (region_type == ORT_WORKSHARE || code == OMP_SCOPE)
		{
		  if (nowait == -1)
		    nowait = omp_find_clause (*list_p,
					      OMP_CLAUSE_NOWAIT) != NULL_TREE;
		  if (nowait
		      && (outer_ctx == NULL
			  || outer_ctx->region_type != ORT_COMBINED_PARALLEL))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%<task%> reduction modifier on a construct "
				"with a %<nowait%> clause");
		      OMP_CLAUSE_REDUCTION_TASK (c) = 0;
		    }
		}
	      else if ((region_type & ORT_PARALLEL) != ORT_PARALLEL)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "invalid %<task%> reduction modifier on construct "
			    "other than %<parallel%>, %qs, %<sections%> or "
			    "%<scope%>", lang_GNU_Fortran () ? "do" : "for");
		  OMP_CLAUSE_REDUCTION_TASK (c) = 0;
		}
	    }
	  if (OMP_CLAUSE_REDUCTION_INSCAN (c))
	    switch (code)
	      {
	      case OMP_SECTIONS:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%<inscan%> %<reduction%> clause on "
			  "%qs construct", "sections");
		OMP_CLAUSE_REDUCTION_INSCAN (c) = 0;
		break;
	      case OMP_PARALLEL:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%<inscan%> %<reduction%> clause on "
			  "%qs construct", "parallel");
		OMP_CLAUSE_REDUCTION_INSCAN (c) = 0;
		break;
	      case OMP_TEAMS:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%<inscan%> %<reduction%> clause on "
			  "%qs construct", "teams");
		OMP_CLAUSE_REDUCTION_INSCAN (c) = 0;
		break;
	      case OMP_TASKLOOP:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%<inscan%> %<reduction%> clause on "
			  "%qs construct", "taskloop");
		OMP_CLAUSE_REDUCTION_INSCAN (c) = 0;
		break;
	      case OMP_SCOPE:
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%<inscan%> %<reduction%> clause on "
			  "%qs construct", "scope");
		OMP_CLAUSE_REDUCTION_INSCAN (c) = 0;
		break;
	      default:
		break;
	      }
	  /* FALLTHRU */
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  flags = GOVD_REDUCTION | GOVD_SEEN | GOVD_EXPLICIT;
	  /* OpenACC permits reductions on private variables.  */
	  if (!(region_type & ORT_ACC)
	      /* taskgroup is actually not a worksharing region.  */
	      && code != OMP_TASKGROUP)
	    check_non_private = omp_clause_code_name[OMP_CLAUSE_CODE (c)];
	  decl = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (decl) == MEM_REF)
	    {
	      tree type = TREE_TYPE (decl);
	      bool saved_into_ssa = gimplify_ctxp->into_ssa;
	      gimplify_ctxp->into_ssa = false;
	      if (gimplify_expr (&TYPE_MAX_VALUE (TYPE_DOMAIN (type)), pre_p,
				 NULL, is_gimple_val, fb_rvalue, false)
		  == GS_ERROR)
		{
		  gimplify_ctxp->into_ssa = saved_into_ssa;
		  remove = true;
		  break;
		}
	      gimplify_ctxp->into_ssa = saved_into_ssa;
	      tree v = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	      if (DECL_P (v))
		{
		  omp_firstprivatize_variable (ctx, v);
		  omp_notice_variable (ctx, v, true);
		}
	      decl = TREE_OPERAND (decl, 0);
	      if (TREE_CODE (decl) == POINTER_PLUS_EXPR)
		{
		  gimplify_ctxp->into_ssa = false;
		  if (gimplify_expr (&TREE_OPERAND (decl, 1), pre_p,
				     NULL, is_gimple_val, fb_rvalue, false)
		      == GS_ERROR)
		    {
		      gimplify_ctxp->into_ssa = saved_into_ssa;
		      remove = true;
		      break;
		    }
		  gimplify_ctxp->into_ssa = saved_into_ssa;
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
	      bool taskloop_seen = false;
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
		    taskloop_seen = true;
		  else if (octx
			   && octx->region_type == ORT_COMBINED_PARALLEL
			   && ((ctx->region_type == ORT_WORKSHARE
				&& octx == outer_ctx)
			       || taskloop_seen))
		    flags = GOVD_SEEN | GOVD_SHARED;
		  else if (octx
			   && ((octx->region_type & ORT_COMBINED_TEAMS)
			       == ORT_COMBINED_TEAMS))
		    flags = GOVD_SEEN | GOVD_SHARED;
		  else if (octx
			   && octx->region_type == ORT_COMBINED_TARGET)
		    {
		      if (flags & GOVD_LASTPRIVATE)
			flags = GOVD_SEEN | GOVD_MAP;
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
	  /* For Fortran, not only the pointer to the data is mapped but also
	     the address of the pointer, the array descriptor etc.; for
	     'exit data' - and in particular for 'delete:' - having an 'alloc:'
	     does not make sense.  Likewise, for 'update' only transferring the
	     data itself is needed as the rest has been handled in previous
	     directives.  However, for 'exit data', the array descriptor needs
	     to be delete; hence, we turn the MAP_TO_PSET into a MAP_DELETE.

	     NOTE: Generally, it is not safe to perform "enter data" operations
	     on arrays where the data *or the descriptor* may go out of scope
	     before a corresponding "exit data" operation -- and such a
	     descriptor may be synthesized temporarily, e.g. to pass an
	     explicit-shape array to a function expecting an assumed-shape
	     argument.  Performing "enter data" inside the called function
	     would thus be problematic.  */
	  if (code == OMP_TARGET_EXIT_DATA
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_TO_PSET)
	    OMP_CLAUSE_SET_MAP_KIND (c, OMP_CLAUSE_MAP_KIND (*prev_list_p)
					== GOMP_MAP_DELETE
					? GOMP_MAP_DELETE : GOMP_MAP_RELEASE);
	  else if ((code == OMP_TARGET_EXIT_DATA || code == OMP_TARGET_UPDATE)
		   && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		       || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_TO_PSET))
	    remove = true;

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
			== GOMP_MAP_FIRSTPRIVATE_REFERENCE)
		    || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH_DETACH)
		   && TREE_CODE (OMP_CLAUSE_SIZE (c)) != INTEGER_CST)
	    {
	      OMP_CLAUSE_SIZE (c)
		= get_initialized_tmp_var (OMP_CLAUSE_SIZE (c), pre_p, NULL,
					   false);
	      if ((region_type & ORT_TARGET) != 0)
		omp_add_variable (ctx, OMP_CLAUSE_SIZE (c),
				  GOVD_FIRSTPRIVATE | GOVD_SEEN);
	    }

	  if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_STRUCT)
	    {
	      tree base = omp_strip_components_and_deref (decl);
	      if (DECL_P (base))
		{
		  decl = base;
		  splay_tree_node n
		    = splay_tree_lookup (ctx->variables,
					 (splay_tree_key) decl);
		  if (seen_error ()
		      && n
		      && (n->value & (GOVD_MAP | GOVD_FIRSTPRIVATE)) != 0)
		    {
		      remove = true;
		      break;
		    }
		  flags = GOVD_MAP | GOVD_EXPLICIT;

		  goto do_add_decl;
		}
	    }

	  if (TREE_CODE (decl) == TARGET_EXPR)
	    {
	      if (gimplify_expr (&OMP_CLAUSE_DECL (c), pre_p, NULL,
				 is_gimple_lvalue, fb_lvalue)
		  == GS_ERROR)
		remove = true;
	    }
	  else if (!DECL_P (decl))
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
		      == REFERENCE_TYPE)
		  && (OMP_CLAUSE_MAP_KIND (c)
		      != GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION))
		{
		  pd = &TREE_OPERAND (decl, 0);
		  decl = TREE_OPERAND (decl, 0);
		}
	      /* An "attach/detach" operation on an update directive should
		 behave as a GOMP_MAP_ALWAYS_POINTER.  Beware that
		 unlike attach or detach map kinds, GOMP_MAP_ALWAYS_POINTER
		 depends on the previous mapping.  */
	      if (code == OACC_UPDATE
		  && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH_DETACH)
		OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_ALWAYS_POINTER);

	      if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH_DETACH)
		{
		  if (TREE_CODE (TREE_TYPE (OMP_CLAUSE_DECL (c)))
		      == ARRAY_TYPE)
		    remove = true;
		  else
		    {
		      gomp_map_kind k = ((code == OACC_EXIT_DATA
					  || code == OMP_TARGET_EXIT_DATA)
					 ? GOMP_MAP_DETACH : GOMP_MAP_ATTACH);
		      OMP_CLAUSE_SET_MAP_KIND (c, k);
		    }
		}

	      tree cref = decl;

	      while (TREE_CODE (cref) == ARRAY_REF)
		cref = TREE_OPERAND (cref, 0);

	      if (TREE_CODE (cref) == INDIRECT_REF)
		cref = TREE_OPERAND (cref, 0);

	      if (TREE_CODE (cref) == COMPONENT_REF)
		{
		  tree base = cref;
		  while (base && !DECL_P (base))
		    {
		      tree innerbase = omp_get_base_pointer (base);
		      if (!innerbase)
			break;
		      base = innerbase;
		    }
		  if (base
		      && DECL_P (base)
		      && GOMP_MAP_ALWAYS_P (OMP_CLAUSE_MAP_KIND (c))
		      && POINTER_TYPE_P (TREE_TYPE (base)))
		    {
		      splay_tree_node n
			= splay_tree_lookup (ctx->variables,
					     (splay_tree_key) base);
		      n->value |= GOVD_SEEN;
		    }
		}

	      if (code == OMP_TARGET && OMP_CLAUSE_MAP_IN_REDUCTION (c))
		{
		  /* Don't gimplify *pd fully at this point, as the base
		     will need to be adjusted during omp lowering.  */
		  auto_vec<tree, 10> expr_stack;
		  tree *p = pd;
		  while (handled_component_p (*p)
			 || TREE_CODE (*p) == INDIRECT_REF
			 || TREE_CODE (*p) == ADDR_EXPR
			 || TREE_CODE (*p) == MEM_REF
			 || TREE_CODE (*p) == NON_LVALUE_EXPR)
		    {
		      expr_stack.safe_push (*p);
		      p = &TREE_OPERAND (*p, 0);
		    }
		  for (int i = expr_stack.length () - 1; i >= 0; i--)
		    {
		      tree t = expr_stack[i];
		      if (TREE_CODE (t) == ARRAY_REF
			  || TREE_CODE (t) == ARRAY_RANGE_REF)
			{
			  if (TREE_OPERAND (t, 2) == NULL_TREE)
			    {
			      tree low = unshare_expr (array_ref_low_bound (t));
			      if (!is_gimple_min_invariant (low))
				{
				  TREE_OPERAND (t, 2) = low;
				  if (gimplify_expr (&TREE_OPERAND (t, 2),
						     pre_p, NULL,
						     is_gimple_reg,
						     fb_rvalue) == GS_ERROR)
				    remove = true;
				}
			    }
			  else if (gimplify_expr (&TREE_OPERAND (t, 2), pre_p,
						  NULL, is_gimple_reg,
						  fb_rvalue) == GS_ERROR)
			    remove = true;
			  if (TREE_OPERAND (t, 3) == NULL_TREE)
			    {
			      tree elmt_size = array_ref_element_size (t);
			      if (!is_gimple_min_invariant (elmt_size))
				{
				  elmt_size = unshare_expr (elmt_size);
				  tree elmt_type
				    = TREE_TYPE (TREE_TYPE (TREE_OPERAND (t,
									  0)));
				  tree factor
				    = size_int (TYPE_ALIGN_UNIT (elmt_type));
				  elmt_size
				    = size_binop (EXACT_DIV_EXPR, elmt_size,
						  factor);
				  TREE_OPERAND (t, 3) = elmt_size;
				  if (gimplify_expr (&TREE_OPERAND (t, 3),
						     pre_p, NULL,
						     is_gimple_reg,
						     fb_rvalue) == GS_ERROR)
				    remove = true;
				}
			    }
			  else if (gimplify_expr (&TREE_OPERAND (t, 3), pre_p,
						  NULL, is_gimple_reg,
						  fb_rvalue) == GS_ERROR)
			    remove = true;
			}
		      else if (TREE_CODE (t) == COMPONENT_REF)
			{
			  if (TREE_OPERAND (t, 2) == NULL_TREE)
			    {
			      tree offset = component_ref_field_offset (t);
			      if (!is_gimple_min_invariant (offset))
				{
				  offset = unshare_expr (offset);
				  tree field = TREE_OPERAND (t, 1);
				  tree factor
				    = size_int (DECL_OFFSET_ALIGN (field)
						/ BITS_PER_UNIT);
				  offset = size_binop (EXACT_DIV_EXPR, offset,
						       factor);
				  TREE_OPERAND (t, 2) = offset;
				  if (gimplify_expr (&TREE_OPERAND (t, 2),
						     pre_p, NULL,
						     is_gimple_reg,
						     fb_rvalue) == GS_ERROR)
				    remove = true;
				}
			    }
			  else if (gimplify_expr (&TREE_OPERAND (t, 2), pre_p,
						  NULL, is_gimple_reg,
						  fb_rvalue) == GS_ERROR)
			    remove = true;
			}
		    }
		  for (; expr_stack.length () > 0; )
		    {
		      tree t = expr_stack.pop ();

		      if (TREE_CODE (t) == ARRAY_REF
			  || TREE_CODE (t) == ARRAY_RANGE_REF)
			{
			  if (!is_gimple_min_invariant (TREE_OPERAND (t, 1))
			      && gimplify_expr (&TREE_OPERAND (t, 1), pre_p,
						NULL, is_gimple_val,
						fb_rvalue) == GS_ERROR)
			    remove = true;
			}
		    }
		}
	      else if (gimplify_expr (pd, pre_p, NULL, is_gimple_lvalue,
				      fb_lvalue) == GS_ERROR)
		{
		  remove = true;
		  break;
		}

	      if (!remove
		  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ALWAYS_POINTER
		  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ATTACH_DETACH
		  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_TO_PSET
		  && OMP_CLAUSE_CHAIN (c)
		  && OMP_CLAUSE_CODE (OMP_CLAUSE_CHAIN (c)) == OMP_CLAUSE_MAP
		  && ((OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
		       == GOMP_MAP_ALWAYS_POINTER)
		      || (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
			  == GOMP_MAP_ATTACH_DETACH)
		      || (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
			  == GOMP_MAP_TO_PSET)))
		prev_list_p = list_p;

	      break;
	    }
	  flags = GOVD_MAP | GOVD_EXPLICIT;
	  if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_TO
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_TOFROM)
	    flags |= GOVD_MAP_ALWAYS_TO;

	  if ((code == OMP_TARGET
	       || code == OMP_TARGET_DATA
	       || code == OMP_TARGET_ENTER_DATA
	       || code == OMP_TARGET_EXIT_DATA)
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH_DETACH)
	    {
	      for (struct gimplify_omp_ctx *octx = outer_ctx; octx;
		   octx = octx->outer_context)
		{
		  splay_tree_node n
		    = splay_tree_lookup (octx->variables,
					 (splay_tree_key) OMP_CLAUSE_DECL (c));
		  /* If this is contained in an outer OpenMP region as a
		     firstprivate value, remove the attach/detach.  */
		  if (n && (n->value & GOVD_FIRSTPRIVATE))
		    {
		      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_FIRSTPRIVATE_POINTER);
		      goto do_add;
		    }
		}

	      enum gomp_map_kind map_kind = (code == OMP_TARGET_EXIT_DATA
					     ? GOMP_MAP_DETACH
					     : GOMP_MAP_ATTACH);
	      OMP_CLAUSE_SET_MAP_KIND (c, map_kind);
	    }

	  goto do_add;

	case OMP_CLAUSE_AFFINITY:
	  gimplify_omp_affinity (list_p, pre_p);
	  remove = true;
	  break;
	case OMP_CLAUSE_DOACROSS:
	  if (OMP_CLAUSE_DOACROSS_KIND (c) == OMP_CLAUSE_DOACROSS_SINK)
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
	    }
	  else
	    gcc_assert (OMP_CLAUSE_DOACROSS_KIND (c)
			== OMP_CLAUSE_DOACROSS_SOURCE);
	  break;
	case OMP_CLAUSE_DEPEND:
	  if (handled_depend_iterators == -1)
	    handled_depend_iterators = gimplify_omp_depend (list_p, pre_p);
	  if (handled_depend_iterators)
	    {
	      if (handled_depend_iterators == 2)
		remove = true;
	      break;
	    }
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
	  if (OMP_CLAUSE_DECL (c) != null_pointer_node)
	    {
	      OMP_CLAUSE_DECL (c) = build_fold_addr_expr (OMP_CLAUSE_DECL (c));
	      if (gimplify_expr (&OMP_CLAUSE_DECL (c), pre_p, NULL,
				 is_gimple_val, fb_rvalue) == GS_ERROR)
		{
		  remove = true;
		  break;
		}
	    }
	  if (code == OMP_TASK)
	    ctx->has_depend = true;
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
	case OMP_CLAUSE_USE_DEVICE_ADDR:
	  flags = GOVD_EXPLICIT;
	  goto do_add;

	case OMP_CLAUSE_HAS_DEVICE_ADDR:
	  decl = OMP_CLAUSE_DECL (c);
	  while (TREE_CODE (decl) == INDIRECT_REF
		 || TREE_CODE (decl) == ARRAY_REF)
	    decl = TREE_OPERAND (decl, 0);
	  flags = GOVD_EXPLICIT;
	  goto do_add_decl;

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
	  if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
	      && OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    {
	      struct gimplify_omp_ctx *pctx
		= code == OMP_TARGET ? outer_ctx : ctx;
	      if (pctx)
		omp_add_variable (pctx, OMP_CLAUSE_REDUCTION_PLACEHOLDER (c),
				  GOVD_LOCAL | GOVD_SEEN);
	      if (pctx
		  && OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c)
		  && walk_tree (&OMP_CLAUSE_REDUCTION_INIT (c),
				find_decl_expr,
				OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c),
				NULL) == NULL_TREE)
		omp_add_variable (pctx,
				  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c),
				  GOVD_LOCAL | GOVD_SEEN);
	      gimplify_omp_ctxp = pctx;
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
	  if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
	      && outer_ctx
	      && ((region_type & ORT_TASKLOOP) == ORT_TASKLOOP
		   || (region_type == ORT_WORKSHARE
		       && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		       && (OMP_CLAUSE_REDUCTION_INSCAN (c)
			   || code == OMP_LOOP)))
	      && (outer_ctx->region_type == ORT_COMBINED_PARALLEL
		  || (code == OMP_LOOP
		      && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		      && ((outer_ctx->region_type & ORT_COMBINED_TEAMS)
			  == ORT_COMBINED_TEAMS))))
	    {
	      splay_tree_node on
		= splay_tree_lookup (outer_ctx->variables,
				     (splay_tree_key)decl);
	      if (on == NULL || (on->value & GOVD_DATA_SHARE_CLASS) == 0)
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		      && TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF
		      && (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
			  || (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE
			      && (TREE_CODE (TREE_TYPE (TREE_TYPE (decl)))
				  == POINTER_TYPE))))
		    omp_firstprivatize_variable (outer_ctx, decl);
		  else
		    {
		      omp_add_variable (outer_ctx, decl,
					GOVD_SEEN | GOVD_SHARED);
		      if (outer_ctx->outer_context)
			omp_notice_variable (outer_ctx->outer_context, decl,
					     true);
		    }
		}
	    }
	  if (outer_ctx)
	    omp_notice_variable (outer_ctx, decl, true);
	  if (check_non_private
	      && (region_type == ORT_WORKSHARE || code == OMP_SCOPE)
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

	case OMP_CLAUSE_DETACH:
	  flags = GOVD_FIRSTPRIVATE | GOVD_SEEN;
	  goto do_add;

	case OMP_CLAUSE_IF:
	  if (OMP_CLAUSE_IF_MODIFIER (c) != ERROR_MARK
	      && OMP_CLAUSE_IF_MODIFIER (c) != code)
	    {
	      const char *p[2];
	      for (int i = 0; i < 2; i++)
		switch (i ? OMP_CLAUSE_IF_MODIFIER (c) : code)
		  {
		  case VOID_CST: p[i] = "cancel"; break;
		  case OMP_PARALLEL: p[i] = "parallel"; break;
		  case OMP_SIMD: p[i] = "simd"; break;
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

	case OMP_CLAUSE_NUM_TEAMS:
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS
	      && OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c)
	      && !is_gimple_min_invariant (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c)))
	    {
	      if (error_operand_p (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c)))
		{
		  remove = true;
		  break;
		}
	      OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c)
		= get_initialized_tmp_var (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c),
					   pre_p, NULL, true);
	    }
	  /* Fall through.  */

	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_DEVICE:
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEVICE
	      && OMP_CLAUSE_DEVICE_ANCESTOR (c))
	    {
	      if (code != OMP_TARGET)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<device%> clause with %<ancestor%> is only "
			    "allowed on %<target%> construct");
		  remove = true;
		  break;
		}

	      tree clauses = *orig_list_p;
	      for (; clauses ; clauses = OMP_CLAUSE_CHAIN (clauses))
		if (OMP_CLAUSE_CODE (clauses) != OMP_CLAUSE_DEVICE
		    && OMP_CLAUSE_CODE (clauses) != OMP_CLAUSE_FIRSTPRIVATE
		    && OMP_CLAUSE_CODE (clauses) != OMP_CLAUSE_PRIVATE
		    && OMP_CLAUSE_CODE (clauses) != OMP_CLAUSE_DEFAULTMAP
		    && OMP_CLAUSE_CODE (clauses) != OMP_CLAUSE_MAP
		   )
		  {
		    error_at (OMP_CLAUSE_LOCATION (c),
			      "with %<ancestor%>, only the %<device%>, "
			      "%<firstprivate%>, %<private%>, %<defaultmap%>, "
			      "and %<map%> clauses may appear on the "
			      "construct");
		    remove = true;
		    break;
		  }
	    }
	  /* Fall through.  */

	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_FILTER:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	  if (OMP_CLAUSE_OPERAND (c, 0)
	      && !is_gimple_min_invariant (OMP_CLAUSE_OPERAND (c, 0)))
	    {
	      if (error_operand_p (OMP_CLAUSE_OPERAND (c, 0)))
		{
		  remove = true;
		  break;
		}
	      /* All these clauses care about value, not a particular decl,
		 so try to force it into a SSA_NAME or fresh temporary.  */
	      OMP_CLAUSE_OPERAND (c, 0)
		= get_initialized_tmp_var (OMP_CLAUSE_OPERAND (c, 0),
					   pre_p, NULL, true);
	    }
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
	  nowait = 1;
	  break;

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
	case OMP_CLAUSE_BIND:
	case OMP_CLAUSE_IF_PRESENT:
	case OMP_CLAUSE_FINALIZE:
	  break;

	case OMP_CLAUSE_ORDER:
	  ctx->order_concurrent = true;
	  break;

	case OMP_CLAUSE_DEFAULTMAP:
	  enum gimplify_defaultmap_kind gdmkmin, gdmkmax;
	  switch (OMP_CLAUSE_DEFAULTMAP_CATEGORY (c))
	    {
	    case OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED:
	      gdmkmin = GDMK_SCALAR;
	      gdmkmax = GDMK_POINTER;
	      break;
	    case OMP_CLAUSE_DEFAULTMAP_CATEGORY_SCALAR:
	      gdmkmin = GDMK_SCALAR;
	      gdmkmax = GDMK_SCALAR_TARGET;
	      break;
	    case OMP_CLAUSE_DEFAULTMAP_CATEGORY_AGGREGATE:
	      gdmkmin = gdmkmax = GDMK_AGGREGATE;
	      break;
	    case OMP_CLAUSE_DEFAULTMAP_CATEGORY_ALLOCATABLE:
	      gdmkmin = gdmkmax = GDMK_ALLOCATABLE;
	      break;
	    case OMP_CLAUSE_DEFAULTMAP_CATEGORY_POINTER:
	      gdmkmin = gdmkmax = GDMK_POINTER;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  for (int gdmk = gdmkmin; gdmk <= gdmkmax; gdmk++)
	    switch (OMP_CLAUSE_DEFAULTMAP_BEHAVIOR (c))
	      {
	      case OMP_CLAUSE_DEFAULTMAP_ALLOC:
		ctx->defaultmap[gdmk] = GOVD_MAP | GOVD_MAP_ALLOC_ONLY;
		break;
	      case OMP_CLAUSE_DEFAULTMAP_TO:
		ctx->defaultmap[gdmk] = GOVD_MAP | GOVD_MAP_TO_ONLY;
		break;
	      case OMP_CLAUSE_DEFAULTMAP_FROM:
		ctx->defaultmap[gdmk] = GOVD_MAP | GOVD_MAP_FROM_ONLY;
		break;
	      case OMP_CLAUSE_DEFAULTMAP_TOFROM:
		ctx->defaultmap[gdmk] = GOVD_MAP;
		break;
	      case OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE:
		ctx->defaultmap[gdmk] = GOVD_FIRSTPRIVATE;
		break;
	      case OMP_CLAUSE_DEFAULTMAP_NONE:
		ctx->defaultmap[gdmk] = 0;
		break;
	      case OMP_CLAUSE_DEFAULTMAP_DEFAULT:
		switch (gdmk)
		  {
		  case GDMK_SCALAR:
		    ctx->defaultmap[gdmk] = GOVD_FIRSTPRIVATE;
		    break;
		  case GDMK_SCALAR_TARGET:
		    ctx->defaultmap[gdmk] = (lang_GNU_Fortran ()
					     ? GOVD_MAP : GOVD_FIRSTPRIVATE);
		    break;
		  case GDMK_AGGREGATE:
		  case GDMK_ALLOCATABLE:
		    ctx->defaultmap[gdmk] = GOVD_MAP;
		    break;
		  case GDMK_POINTER:
		    ctx->defaultmap[gdmk] = GOVD_MAP;
		    if (!lang_GNU_Fortran ())
		      ctx->defaultmap[gdmk] |= GOVD_MAP_0LEN_ARRAY;
		    break;
		  default:
		    gcc_unreachable ();
		  }
		break;
	      default:
		gcc_unreachable ();
	      }
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

	case OMP_CLAUSE_NONTEMPORAL:
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    {
	      remove = true;
	      break;
	    }
	  omp_add_variable (ctx, decl, GOVD_NONTEMPORAL);
	  break;

	case OMP_CLAUSE_ALLOCATE:
	  decl = OMP_CLAUSE_DECL (c);
	  if (error_operand_p (decl))
	    {
	      remove = true;
	      break;
	    }
	  if (gimplify_expr (&OMP_CLAUSE_ALLOCATE_ALLOCATOR (c), pre_p, NULL,
			     is_gimple_val, fb_rvalue) == GS_ERROR)
	    {
	      remove = true;
	      break;
	    }
	  else if (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c) == NULL_TREE
		   || (TREE_CODE (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c))
		       == INTEGER_CST))
	    ;
	  else if (code == OMP_TASKLOOP
		   || !DECL_P (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)))
	    OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)
	      = get_initialized_tmp_var (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c),
					 pre_p, NULL, false);
	  break;

	case OMP_CLAUSE_DEFAULT:
	  ctx->default_kind = OMP_CLAUSE_DEFAULT_KIND (c);
	  break;

	case OMP_CLAUSE_INCLUSIVE:
	case OMP_CLAUSE_EXCLUSIVE:
	  decl = OMP_CLAUSE_DECL (c);
	  {
	    splay_tree_node n = splay_tree_lookup (outer_ctx->variables,
						   (splay_tree_key) decl);
	    if (n == NULL || (n->value & GOVD_REDUCTION) == 0)
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD specified in %qs clause but not in %<inscan%> "
			  "%<reduction%> clause on the containing construct",
			  decl, omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		remove = true;
	      }
	    else
	      {
		n->value |= GOVD_REDUCTION_INSCAN;
		if (outer_ctx->region_type == ORT_SIMD
		    && outer_ctx->outer_context
		    && outer_ctx->outer_context->region_type == ORT_WORKSHARE)
		  {
		    n = splay_tree_lookup (outer_ctx->outer_context->variables,
					   (splay_tree_key) decl);
		    if (n && (n->value & GOVD_REDUCTION) != 0)
		      n->value |= GOVD_REDUCTION_INSCAN;
		  }
	      }
	  }
	  break;

	case OMP_CLAUSE_NOHOST:
	default:
	  gcc_unreachable ();
	}

      if (code == OACC_DATA
	  && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	  && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE))
	remove = true;
      if (remove)
	*list_p = OMP_CLAUSE_CHAIN (c);
      else
	list_p = &OMP_CLAUSE_CHAIN (c);
    }

  ctx->clauses = *orig_list_p;
  gimplify_omp_ctxp = ctx;
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
  if (omp_privatize_by_reference (decl))
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
    case GIMPLE_OMP_SCOPE:
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

  if (gimplify_omp_ctxp->region_type == ORT_COMBINED_PARALLEL
      && (flags & GOVD_LASTPRIVATE_CONDITIONAL) != 0)
    flags = GOVD_SHARED | GOVD_SEEN | GOVD_WRITTEN;
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
      if (VAR_P (decl)
	  && DECL_IN_CONSTANT_POOL (decl)
          && !lookup_attribute ("omp declare target",
				DECL_ATTRIBUTES (decl)))
	{
	  tree id = get_identifier ("omp declare target");
	  DECL_ATTRIBUTES (decl)
	    = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (decl));
	  varpool_node *node = varpool_node::get (decl);
	  if (node)
	    {
	      node->offloadable = 1;
	      if (ENABLE_OFFLOADING)
		g->have_offload = true;
	    }
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
      /* Don't optimize shared into firstprivate for read-only vars
	 on tasks with depend clause, we shouldn't try to copy them
	 until the dependencies are satisfied.  */
      if (gimplify_omp_ctxp->has_depend)
	flags |= GOVD_WRITTEN;
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
  else if (flags & (GOVD_ALIGNED | GOVD_NONTEMPORAL))
    return 0;
  else if (flags & GOVD_CONDTEMP)
    {
      code = OMP_CLAUSE__CONDTEMP_;
      gimple_add_tmp_var (decl);
    }
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
		       | GOVD_MAP_FORCE_PRESENT
		       | GOVD_MAP_ALLOC_ONLY
		       | GOVD_MAP_FROM_ONLY))
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
	case GOVD_MAP_FROM_ONLY:
	  kind = GOMP_MAP_FROM;
	  break;
	case GOVD_MAP_ALLOC_ONLY:
	  kind = GOMP_MAP_ALLOC;
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
      /* Setting of the implicit flag for the runtime is currently disabled for
	 OpenACC.  */
      if ((gimplify_omp_ctxp->region_type & ORT_ACC) == 0)
	OMP_CLAUSE_MAP_RUNTIME_IMPLICIT_P (clause) = 1;
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
	       && omp_privatize_by_reference (decl))
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
      lang_hooks.decls.omp_finish_clause (nc, pre_p,
					  (ctx->region_type & ORT_ACC) != 0);
      gimplify_omp_ctxp = ctx;
    }
  *list_p = clause;
  struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
  gimplify_omp_ctxp = ctx->outer_context;
  /* Don't call omp_finish_clause on implicitly added OMP_CLAUSE_PRIVATE
     in simd.  Those are only added for the local vars inside of simd body
     and they don't need to be e.g. default constructible.  */
  if (code != OMP_CLAUSE_PRIVATE || ctx->region_type != ORT_SIMD) 
    lang_hooks.decls.omp_finish_clause (clause, pre_p,
					(ctx->region_type & ORT_ACC) != 0);
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
  tree *orig_list_p = list_p;
  tree c, decl;
  bool has_inscan_reductions = false;

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

  if (ctx->add_safelen1)
    {
      /* If there are VLAs in the body of simd loop, prevent
	 vectorization.  */
      gcc_assert (ctx->region_type == ORT_SIMD);
      c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_SAFELEN);
      OMP_CLAUSE_SAFELEN_EXPR (c) = integer_one_node;
      OMP_CLAUSE_CHAIN (c) = *list_p;
      *list_p = c;
      list_p = &OMP_CLAUSE_CHAIN (c);
    }

  if (ctx->region_type == ORT_WORKSHARE
      && ctx->outer_context
      && ctx->outer_context->region_type == ORT_COMBINED_PARALLEL)
    {
      for (c = ctx->outer_context->clauses; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	    && OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c))
	  {
	    decl = OMP_CLAUSE_DECL (c);
	    splay_tree_node n
	      = splay_tree_lookup (ctx->outer_context->variables,
				   (splay_tree_key) decl);
	    gcc_checking_assert (!splay_tree_lookup (ctx->variables,
						     (splay_tree_key) decl));
	    omp_add_variable (ctx, decl, n->value);
	    tree c2 = copy_node (c);
	    OMP_CLAUSE_CHAIN (c2) = *list_p;
	    *list_p = c2;
	    if ((n->value & GOVD_FIRSTPRIVATE) == 0)
	      continue;
	    c2 = build_omp_clause (OMP_CLAUSE_LOCATION (c),
				   OMP_CLAUSE_FIRSTPRIVATE);
	    OMP_CLAUSE_DECL (c2) = decl;
	    OMP_CLAUSE_CHAIN (c2) = *list_p;
	    *list_p = c2;
	  }
    }

  tree attach_list = NULL_TREE;
  tree *attach_tail = &attach_list;

  while ((c = *list_p) != NULL)
    {
      splay_tree_node n;
      bool remove = false;
      bool move_attach = false;

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
	  if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
	    {
	      decl = OMP_CLAUSE_DECL (c);
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      if ((n->value & GOVD_MAP) != 0)
		{
		  remove = true;
		  break;
		}
	      OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT_TARGET (c) = 0;
	      OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c) = 0;
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_LINEAR:
	  decl = OMP_CLAUSE_DECL (c);
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  remove = !(n->value & GOVD_SEEN);
	  if ((n->value & GOVD_LASTPRIVATE_CONDITIONAL) != 0
	      && code == OMP_PARALLEL
	      && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	    remove = true;
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
		  && ctx->has_depend
		  && DECL_P (decl))
		n->value |= GOVD_WRITTEN;
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
	  else
	    n->value &= ~GOVD_EXPLICIT;
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
	  if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c) && code == OMP_PARALLEL)
	    remove = true;
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

	case OMP_CLAUSE_HAS_DEVICE_ADDR:
	  decl = OMP_CLAUSE_DECL (c);
	  while (TREE_CODE (decl) == INDIRECT_REF
		 || TREE_CODE (decl) == ARRAY_REF)
	    decl = TREE_OPERAND (decl, 0);
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  remove = n == NULL || !(n->value & GOVD_SEEN);
	  break;

	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_NONTEMPORAL:
	  decl = OMP_CLAUSE_DECL (c);
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  remove = n == NULL || !(n->value & GOVD_SEEN);
	  break;

	case OMP_CLAUSE_MAP:
	  if (code == OMP_TARGET_EXIT_DATA
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_POINTER)
	    {
	      remove = true;
	      break;
	    }
	  /* If we have a target region, we can push all the attaches to the
	     end of the list (we may have standalone "attach" operations
	     synthesized for GOMP_MAP_STRUCT nodes that must be processed after
	     the attachment point AND the pointed-to block have been mapped).
	     If we have something else, e.g. "enter data", we need to keep
	     "attach" nodes together with the previous node they attach to so
	     that separate "exit data" operations work properly (see
	     libgomp/target.c).  */
	  if ((ctx->region_type & ORT_TARGET) != 0
	      && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH
		  || (OMP_CLAUSE_MAP_KIND (c)
		      == GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION)))
	    move_attach = true;
	  decl = OMP_CLAUSE_DECL (c);
	  /* Data clauses associated with reductions must be
	     compatible with present_or_copy.  Warn and adjust the clause
	     if that is not the case.  */
	  if (ctx->region_type == ORT_ACC_PARALLEL
	      || ctx->region_type == ORT_ACC_SERIAL)
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
				  "on %qE; promoting to %<present_or_copy%>",
				  DECL_NAME (t));
		      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TOFROM);
		    }
		}
	    }
	  if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_STRUCT
	      && (code == OMP_TARGET_EXIT_DATA || code == OACC_EXIT_DATA))
	    {
	      remove = true;
	      break;
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
	  if (OMP_CLAUSE_REDUCTION_INSCAN (c))
	    {
	      decl = OMP_CLAUSE_DECL (c);
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      if ((n->value & GOVD_REDUCTION_INSCAN) == 0)
		{
		  remove = true;
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD specified in %<inscan%> %<reduction%> clause "
			    "but not in %<scan%> directive clause", decl);
		  break;
		}
	      has_inscan_reductions = true;
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  decl = OMP_CLAUSE_DECL (c);
	  /* OpenACC reductions need a present_or_copy data clause.
	     Add one if necessary.  Emit error when the reduction is private.  */
	  if (ctx->region_type == ORT_ACC_PARALLEL
	      || ctx->region_type == ORT_ACC_SERIAL)
	    {
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	      if (n->value & (GOVD_PRIVATE | GOVD_FIRSTPRIVATE))
		{
		  remove = true;
		  error_at (OMP_CLAUSE_LOCATION (c), "invalid private "
			    "reduction on %qE", DECL_NAME (decl));
		}
	      else if ((n->value & GOVD_MAP) == 0)
		{
		  tree next = OMP_CLAUSE_CHAIN (c);
		  tree nc = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_MAP);
		  OMP_CLAUSE_SET_MAP_KIND (nc, GOMP_MAP_TOFROM);
		  OMP_CLAUSE_DECL (nc) = decl;
		  OMP_CLAUSE_CHAIN (c) = nc;
		  lang_hooks.decls.omp_finish_clause (nc, pre_p,
						      (ctx->region_type
						       & ORT_ACC) != 0);
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

	case OMP_CLAUSE_ALLOCATE:
	  decl = OMP_CLAUSE_DECL (c);
	  n = splay_tree_lookup (ctx->variables, (splay_tree_key) decl);
	  if (n != NULL && !(n->value & GOVD_SEEN))
	    {
	      if ((n->value & (GOVD_PRIVATE | GOVD_FIRSTPRIVATE | GOVD_LINEAR))
		  != 0
		  && (n->value & (GOVD_REDUCTION | GOVD_LASTPRIVATE)) == 0)
		remove = true;
	    }
	  if (!remove
	      && OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)
	      && TREE_CODE (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)) != INTEGER_CST
	      && ((ctx->region_type & (ORT_PARALLEL | ORT_TARGET)) != 0
		  || (ctx->region_type & ORT_TASKLOOP) == ORT_TASK
		  || (ctx->region_type & ORT_HOST_TEAMS) == ORT_HOST_TEAMS))
	    {
	      tree allocator = OMP_CLAUSE_ALLOCATE_ALLOCATOR (c);
	      n = splay_tree_lookup (ctx->variables, (splay_tree_key) allocator);
	      if (n == NULL)
		{
		  enum omp_clause_default_kind default_kind
		    = ctx->default_kind;
		  ctx->default_kind = OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;
		  omp_notice_variable (ctx, OMP_CLAUSE_ALLOCATE_ALLOCATOR (c),
				       true);
		  ctx->default_kind = default_kind;
		}
	      else
		omp_notice_variable (ctx, OMP_CLAUSE_ALLOCATE_ALLOCATOR (c),
				     true);
	    }
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
	case OMP_CLAUSE_DOACROSS:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_FILTER:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_ORDER:
	case OMP_CLAUSE_BIND:
	case OMP_CLAUSE_DETACH:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE_USE_DEVICE_ADDR:
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
	case OMP_CLAUSE_IF_PRESENT:
	case OMP_CLAUSE_FINALIZE:
	case OMP_CLAUSE_INCLUSIVE:
	case OMP_CLAUSE_EXCLUSIVE:
	  break;

	case OMP_CLAUSE_NOHOST:
	default:
	  gcc_unreachable ();
	}

      if (remove)
	*list_p = OMP_CLAUSE_CHAIN (c);
      else if (move_attach)
	{
	  /* Remove attach node from here, separate out into its own list.  */
	  *attach_tail = c;
	  *list_p = OMP_CLAUSE_CHAIN (c);
	  OMP_CLAUSE_CHAIN (c) = NULL_TREE;
	  attach_tail = &OMP_CLAUSE_CHAIN (c);
	}
      else
	list_p = &OMP_CLAUSE_CHAIN (c);
    }

  /* Splice attach nodes at the end of the list.  */
  if (attach_list)
    {
      *list_p = attach_list;
      list_p = attach_tail;
    }

  /* Add in any implicit data sharing.  */
  struct gimplify_adjust_omp_clauses_data data;
  if ((gimplify_omp_ctxp->region_type & ORT_ACC) == 0)
    {
      /* OpenMP.  Implicit clauses are added at the start of the clause list,
	 but after any non-map clauses.  */
      tree *implicit_add_list_p = orig_list_p;
      while (*implicit_add_list_p
	     && OMP_CLAUSE_CODE (*implicit_add_list_p) != OMP_CLAUSE_MAP)
	implicit_add_list_p = &OMP_CLAUSE_CHAIN (*implicit_add_list_p);
      data.list_p = implicit_add_list_p;
    }
  else
    /* OpenACC.  */
    data.list_p = list_p;
  data.pre_p = pre_p;
  splay_tree_foreach (ctx->variables, gimplify_adjust_omp_clauses_1, &data);

  if (has_inscan_reductions)
    for (c = *orig_list_p; c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "%<inscan%> %<reduction%> clause used together with "
		    "%<linear%> clause for a variable other than loop "
		    "iterator");
	  break;
	}

  gimplify_omp_ctxp = ctx->outer_context;
  delete_omp_context (ctx);
}

/* Return 0 if CONSTRUCTS selectors don't match the OpenMP context,
   -1 if unknown yet (simd is involved, won't be known until vectorization)
   and 1 if they do.  If SCORES is non-NULL, it should point to an array
   of at least 2*NCONSTRUCTS+2 ints, and will be filled with the positions
   of the CONSTRUCTS (position -1 if it will never match) followed by
   number of constructs in the OpenMP context construct trait.  If the
   score depends on whether it will be in a declare simd clone or not,
   the function returns 2 and there will be two sets of the scores, the first
   one for the case that it is not in a declare simd clone, the other
   that it is in a declare simd clone.  */

int
omp_construct_selector_matches (enum tree_code *constructs, int nconstructs,
				int *scores)
{
  int matched = 0, cnt = 0;
  bool simd_seen = false;
  bool target_seen = false;
  int declare_simd_cnt = -1;
  auto_vec<enum tree_code, 16> codes;
  for (struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp; ctx;)
    {
      if (((ctx->region_type & ORT_PARALLEL) && ctx->code == OMP_PARALLEL)
	  || ((ctx->region_type & (ORT_TARGET | ORT_IMPLICIT_TARGET | ORT_ACC))
	      == ORT_TARGET && ctx->code == OMP_TARGET)
	  || ((ctx->region_type & ORT_TEAMS) && ctx->code == OMP_TEAMS)
	  || (ctx->region_type == ORT_WORKSHARE && ctx->code == OMP_FOR)
	  || (ctx->region_type == ORT_SIMD
	      && ctx->code == OMP_SIMD
	      && !omp_find_clause (ctx->clauses, OMP_CLAUSE_BIND)))
	{
	  ++cnt;
	  if (scores)
	    codes.safe_push (ctx->code);
	  else if (matched < nconstructs && ctx->code == constructs[matched])
	    {
	      if (ctx->code == OMP_SIMD)
		{
		  if (matched)
		    return 0;
		  simd_seen = true;
		}
	      ++matched;
	    }
	  if (ctx->code == OMP_TARGET)
	    {
	      if (scores == NULL)
		return matched < nconstructs ? 0 : simd_seen ? -1 : 1;
	      target_seen = true;
	      break;
	    }
	}
      else if (ctx->region_type == ORT_WORKSHARE
	       && ctx->code == OMP_LOOP
	       && ctx->outer_context
	       && ctx->outer_context->region_type == ORT_COMBINED_PARALLEL
	       && ctx->outer_context->outer_context
	       && ctx->outer_context->outer_context->code == OMP_LOOP
	       && ctx->outer_context->outer_context->distribute)
	ctx = ctx->outer_context->outer_context;
      ctx = ctx->outer_context;
    }
  if (!target_seen
      && lookup_attribute ("omp declare simd",
			   DECL_ATTRIBUTES (current_function_decl)))
    {
      /* Declare simd is a maybe case, it is supposed to be added only to the
	 omp-simd-clone.cc added clones and not to the base function.  */
      declare_simd_cnt = cnt++;
      if (scores)
	codes.safe_push (OMP_SIMD);
      else if (cnt == 0
	       && constructs[0] == OMP_SIMD)
	{
	  gcc_assert (matched == 0);
	  simd_seen = true;
	  if (++matched == nconstructs)
	    return -1;
	}
    }
  if (tree attr = lookup_attribute ("omp declare variant variant",
				    DECL_ATTRIBUTES (current_function_decl)))
    {
      enum tree_code variant_constructs[5];
      int variant_nconstructs = 0;
      if (!target_seen)
	variant_nconstructs
	  = omp_constructor_traits_to_codes (TREE_VALUE (attr),
					     variant_constructs);
      for (int i = 0; i < variant_nconstructs; i++)
	{
	  ++cnt;
	  if (scores)
	    codes.safe_push (variant_constructs[i]);
	  else if (matched < nconstructs
		   && variant_constructs[i] == constructs[matched])
	    {
	      if (variant_constructs[i] == OMP_SIMD)
		{
		  if (matched)
		    return 0;
		  simd_seen = true;
		}
	      ++matched;
	    }
	}
    }
  if (!target_seen
      && lookup_attribute ("omp declare target block",
			   DECL_ATTRIBUTES (current_function_decl)))
    {
      if (scores)
	codes.safe_push (OMP_TARGET);
      else if (matched < nconstructs && constructs[matched] == OMP_TARGET)
	++matched;
    }
  if (scores)
    {
      for (int pass = 0; pass < (declare_simd_cnt == -1 ? 1 : 2); pass++)
	{
	  int j = codes.length () - 1;
	  for (int i = nconstructs - 1; i >= 0; i--)
	    {
	      while (j >= 0
		     && (pass != 0 || declare_simd_cnt != j)
		     && constructs[i] != codes[j])
		--j;
	      if (pass == 0 && declare_simd_cnt != -1 && j > declare_simd_cnt)
		*scores++ = j - 1;
	      else
		*scores++ = j;
	    }
	  *scores++ = ((pass == 0 && declare_simd_cnt != -1)
		       ? codes.length () - 1 : codes.length ());
	}
      return declare_simd_cnt == -1 ? 1 : 2;
    }
  if (matched == nconstructs)
    return simd_seen ? -1 : 1;
  return 0;
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
	new_op = GOMP_MAP_RELEASE;
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
  bool nowait = false;
  bool has_depend = false;

  if (OMP_TASK_BODY (expr) == NULL_TREE)
    {
      for (tree c = OMP_TASK_CLAUSES (expr); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
	  {
	    has_depend = true;
	    if (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_MUTEXINOUTSET)
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%<mutexinoutset%> kind in %<depend%> clause on a "
			  "%<taskwait%> construct");
		break;
	      }
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NOWAIT)
	  nowait = true;
      if (nowait && !has_depend)
	{
	  error_at (EXPR_LOCATION (expr),
		    "%<taskwait%> construct with %<nowait%> clause but no "
		    "%<depend%> clauses");
	  *expr_p = NULL_TREE;
	  return;
	}
    }

  gimplify_scan_omp_clauses (&OMP_TASK_CLAUSES (expr), pre_p,
			     omp_find_clause (OMP_TASK_CLAUSES (expr),
					      OMP_CLAUSE_UNTIED)
			     ? ORT_UNTIED_TASK : ORT_TASK, OMP_TASK);

  if (OMP_TASK_BODY (expr))
    {
      push_gimplify_context ();

      g = gimplify_and_return_first (OMP_TASK_BODY (expr), &body);
      if (gimple_code (g) == GIMPLE_BIND)
	pop_gimplify_context (g);
      else
	pop_gimplify_context (NULL);
    }

  gimplify_adjust_omp_clauses (pre_p, body, &OMP_TASK_CLAUSES (expr),
			       OMP_TASK);

  g = gimple_build_omp_task (body,
			     OMP_TASK_CLAUSES (expr),
			     NULL_TREE, NULL_TREE,
			     NULL_TREE, NULL_TREE, NULL_TREE);
  if (OMP_TASK_BODY (expr) == NULL_TREE)
    gimple_omp_task_set_taskwait_p (g, true);
  gimplify_seq_add_stmt (pre_p, g);
  *expr_p = NULL_TREE;
}

/* Helper function for gimplify_omp_for.  If *TP is not a gimple constant,
   force it into a temporary initialized in PRE_P and add firstprivate clause
   to ORIG_FOR_STMT.  */

static void
gimplify_omp_taskloop_expr (tree type, tree *tp, gimple_seq *pre_p,
			    tree orig_for_stmt)
{
  if (*tp == NULL || is_gimple_constant (*tp))
    return;

  *tp = get_initialized_tmp_var (*tp, pre_p, NULL, false);
  /* Reference to pointer conversion is considered useless,
     but is significant for firstprivate clause.  Force it
     here.  */
  if (type
      && TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (*tp)) == REFERENCE_TYPE)
    {
      tree v = create_tmp_var (TYPE_MAIN_VARIANT (type));
      tree m = build2 (INIT_EXPR, TREE_TYPE (v), v, *tp);
      gimplify_and_add (m, pre_p);
      *tp = v;
    }

  tree c = build_omp_clause (input_location, OMP_CLAUSE_FIRSTPRIVATE);
  OMP_CLAUSE_DECL (c) = *tp;
  OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (orig_for_stmt);
  OMP_FOR_CLAUSES (orig_for_stmt) = c;
}

/* Helper function of gimplify_omp_for, find OMP_ORDERED with
   null OMP_ORDERED_BODY inside of OMP_FOR's body.  */

static tree
find_standalone_omp_ordered (tree *tp, int *walk_subtrees, void *)
{
  switch (TREE_CODE (*tp))
    {
    case OMP_ORDERED:
      if (OMP_ORDERED_BODY (*tp) == NULL_TREE)
	return *tp;
      break;
    case OMP_SIMD:
    case OMP_PARALLEL:
    case OMP_TARGET:
      *walk_subtrees = 0;
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
  bool openacc = TREE_CODE (*expr_p) == OACC_LOOP;

  orig_for_stmt = for_stmt = *expr_p;

  bool loop_p = (omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_BIND)
		 != NULL_TREE);
  if (OMP_FOR_INIT (for_stmt) == NULL_TREE)
    {
      tree *data[4] = { NULL, NULL, NULL, NULL };
      gcc_assert (TREE_CODE (for_stmt) != OACC_LOOP);
      inner_for_stmt = walk_tree (&OMP_FOR_BODY (for_stmt),
				  find_combined_omp_for, data, NULL);
      if (inner_for_stmt == NULL_TREE)
	{
	  gcc_assert (seen_error ());
	  *expr_p = NULL_TREE;
	  return GS_ERROR;
	}
      if (data[2] && OMP_FOR_PRE_BODY (*data[2]))
	{
	  append_to_statement_list_force (OMP_FOR_PRE_BODY (*data[2]),
					  &OMP_FOR_PRE_BODY (for_stmt));
	  OMP_FOR_PRE_BODY (*data[2]) = NULL_TREE;
	}
      if (OMP_FOR_PRE_BODY (inner_for_stmt))
	{
	  append_to_statement_list_force (OMP_FOR_PRE_BODY (inner_for_stmt),
					  &OMP_FOR_PRE_BODY (for_stmt));
	  OMP_FOR_PRE_BODY (inner_for_stmt) = NULL_TREE;
	}

      if (data[0])
	{
	  /* We have some statements or variable declarations in between
	     the composite construct directives.  Move them around the
	     inner_for_stmt.  */
	  data[0] = expr_p;
	  for (i = 0; i < 3; i++)
	    if (data[i])
	      {
		tree t = *data[i];
		if (i < 2 && data[i + 1] == &OMP_BODY (t))
		  data[i + 1] = data[i];
		*data[i] = OMP_BODY (t);
		tree body = build3 (BIND_EXPR, void_type_node, NULL_TREE,
				    NULL_TREE, make_node (BLOCK));
		OMP_BODY (t) = body;
		append_to_statement_list_force (inner_for_stmt,
						&BIND_EXPR_BODY (body));
		*data[3] = t;
		data[3] = tsi_stmt_ptr (tsi_start (BIND_EXPR_BODY (body)));
		gcc_assert (*data[3] == inner_for_stmt);
	      }
	  return GS_OK;
	}

      for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (inner_for_stmt)); i++)
	if (!loop_p
	    && OMP_FOR_ORIG_DECLS (inner_for_stmt)
	    && TREE_CODE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt),
					i)) == TREE_LIST
	    && TREE_PURPOSE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt),
					   i)))
	  {
	    tree orig = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt), i);
	    /* Class iterators aren't allowed on OMP_SIMD, so the only
	       case we need to solve is distribute parallel for.  They are
	       allowed on the loop construct, but that is already handled
	       in gimplify_omp_loop.  */
	    gcc_assert (TREE_CODE (inner_for_stmt) == OMP_FOR
			&& TREE_CODE (for_stmt) == OMP_DISTRIBUTE
			&& data[1]);
	    tree orig_decl = TREE_PURPOSE (orig);
	    tree last = TREE_VALUE (orig);
	    tree *pc;
	    for (pc = &OMP_FOR_CLAUSES (inner_for_stmt);
		 *pc; pc = &OMP_CLAUSE_CHAIN (*pc))
	      if ((OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_PRIVATE
		   || OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_LASTPRIVATE)
		  && OMP_CLAUSE_DECL (*pc) == orig_decl)
		break;
	    if (*pc == NULL_TREE)
	      {
		tree *spc;
		for (spc = &OMP_PARALLEL_CLAUSES (*data[1]);
		     *spc; spc = &OMP_CLAUSE_CHAIN (*spc))
		  if (OMP_CLAUSE_CODE (*spc) == OMP_CLAUSE_PRIVATE
		      && OMP_CLAUSE_DECL (*spc) == orig_decl)
		    break;
		if (*spc)
		  {
		    tree c = *spc;
		    *spc = OMP_CLAUSE_CHAIN (c);
		    OMP_CLAUSE_CHAIN (c) = NULL_TREE;
		    *pc = c;
		  }
	      }
	    if (*pc == NULL_TREE)
	      ;
	    else if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_PRIVATE)
	      {
		/* private clause will appear only on inner_for_stmt.
		   Change it into firstprivate, and add private clause
		   on for_stmt.  */
		tree c = copy_node (*pc);
		OMP_CLAUSE_CHAIN (c) = OMP_FOR_CLAUSES (for_stmt);
		OMP_FOR_CLAUSES (for_stmt) = c;
		OMP_CLAUSE_CODE (*pc) = OMP_CLAUSE_FIRSTPRIVATE;
		lang_hooks.decls.omp_finish_clause (*pc, pre_p, openacc);
	      }
	    else
	      {
		/* lastprivate clause will appear on both inner_for_stmt
		   and for_stmt.  Add firstprivate clause to
		   inner_for_stmt.  */
		tree c = build_omp_clause (OMP_CLAUSE_LOCATION (*pc),
					   OMP_CLAUSE_FIRSTPRIVATE);
		OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (*pc);
		OMP_CLAUSE_CHAIN (c) = *pc;
		*pc = c;
		lang_hooks.decls.omp_finish_clause (*pc, pre_p, openacc);
	      }
	    tree c = build_omp_clause (UNKNOWN_LOCATION,
				       OMP_CLAUSE_FIRSTPRIVATE);
	    OMP_CLAUSE_DECL (c) = last;
	    OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (*data[1]);
	    OMP_PARALLEL_CLAUSES (*data[1]) = c;
	    c = build_omp_clause (UNKNOWN_LOCATION,
				  *pc ? OMP_CLAUSE_SHARED
				      : OMP_CLAUSE_FIRSTPRIVATE);
	    OMP_CLAUSE_DECL (c) = orig_decl;
	    OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (*data[1]);
	    OMP_PARALLEL_CLAUSES (*data[1]) = c;
	  }
      /* Similarly, take care of C++ range for temporaries, those should
	 be firstprivate on OMP_PARALLEL if any.  */
      if (data[1])
	for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (inner_for_stmt)); i++)
	  if (OMP_FOR_ORIG_DECLS (inner_for_stmt)
	      && TREE_CODE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt),
					  i)) == TREE_LIST
	      && TREE_CHAIN (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt),
					   i)))
	    {
	      tree orig
		= TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt), i);
	      tree v = TREE_CHAIN (orig);
	      tree c = build_omp_clause (UNKNOWN_LOCATION,
					 OMP_CLAUSE_FIRSTPRIVATE);
	      /* First add firstprivate clause for the __for_end artificial
		 decl.  */
	      OMP_CLAUSE_DECL (c) = TREE_VEC_ELT (v, 1);
	      if (TREE_CODE (TREE_TYPE (OMP_CLAUSE_DECL (c)))
		  == REFERENCE_TYPE)
		OMP_CLAUSE_FIRSTPRIVATE_NO_REFERENCE (c) = 1;
	      OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (*data[1]);
	      OMP_PARALLEL_CLAUSES (*data[1]) = c;
	      if (TREE_VEC_ELT (v, 0))
		{
		  /* And now the same for __for_range artificial decl if it
		     exists.  */
		  c = build_omp_clause (UNKNOWN_LOCATION,
					OMP_CLAUSE_FIRSTPRIVATE);
		  OMP_CLAUSE_DECL (c) = TREE_VEC_ELT (v, 0);
		  if (TREE_CODE (TREE_TYPE (OMP_CLAUSE_DECL (c)))
		      == REFERENCE_TYPE)
		    OMP_CLAUSE_FIRSTPRIVATE_NO_REFERENCE (c) = 1;
		  OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (*data[1]);
		  OMP_PARALLEL_CLAUSES (*data[1]) = c;
		}
	    }
    }

  switch (TREE_CODE (for_stmt))
    {
    case OMP_FOR:
      if (OMP_FOR_NON_RECTANGULAR (inner_for_stmt ? inner_for_stmt : for_stmt))
	{
	  if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
			       OMP_CLAUSE_SCHEDULE))
	    error_at (EXPR_LOCATION (for_stmt),
		      "%qs clause may not appear on non-rectangular %qs",
		      "schedule", lang_GNU_Fortran () ? "do" : "for");
	  if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_ORDERED))
	    error_at (EXPR_LOCATION (for_stmt),
		      "%qs clause may not appear on non-rectangular %qs",
		      "ordered", lang_GNU_Fortran () ? "do" : "for");
	}
      break;
    case OMP_DISTRIBUTE:
      if (OMP_FOR_NON_RECTANGULAR (inner_for_stmt ? inner_for_stmt : for_stmt)
	  && omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
			      OMP_CLAUSE_DIST_SCHEDULE))
	error_at (EXPR_LOCATION (for_stmt),
		  "%qs clause may not appear on non-rectangular %qs",
		  "dist_schedule", "distribute");
      break;
    case OACC_LOOP:
      ort = ORT_ACC;
      break;
    case OMP_TASKLOOP:
      if (OMP_FOR_NON_RECTANGULAR (inner_for_stmt ? inner_for_stmt : for_stmt))
	{
	  if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
			       OMP_CLAUSE_GRAINSIZE))
	    error_at (EXPR_LOCATION (for_stmt),
		      "%qs clause may not appear on non-rectangular %qs",
		      "grainsize", "taskloop");
	  if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
			       OMP_CLAUSE_NUM_TASKS))
	    error_at (EXPR_LOCATION (for_stmt),
		      "%qs clause may not appear on non-rectangular %qs",
		      "num_tasks", "taskloop");
	}
      if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_UNTIED))
	ort = ORT_UNTIED_TASKLOOP;
      else
	ort = ORT_TASKLOOP;
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

  if (TREE_CODE (for_stmt) != OMP_TASKLOOP)
    gimplify_scan_omp_clauses (&OMP_FOR_CLAUSES (for_stmt), pre_p, ort,
			       loop_p && TREE_CODE (for_stmt) != OMP_SIMD
			       ? OMP_LOOP : TREE_CODE (for_stmt));

  if (TREE_CODE (for_stmt) == OMP_DISTRIBUTE)
    gimplify_omp_ctxp->distribute = true;

  /* Handle OMP_FOR_INIT.  */
  for_pre_body = NULL;
  if ((ort == ORT_SIMD
       || (inner_for_stmt && TREE_CODE (inner_for_stmt) == OMP_SIMD))
      && OMP_FOR_PRE_BODY (for_stmt))
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
	  gimple_seq *for_pre_p = (gimple_seq_empty_p (for_pre_body)
				   ? pre_p : &for_pre_body);
	      tree type = TREE_TYPE (TREE_OPERAND (t, 0));
	  if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC)
	    {
	      tree v = TREE_OPERAND (t, 1);
	      gimplify_omp_taskloop_expr (type, &TREE_VEC_ELT (v, 1),
					  for_pre_p, orig_for_stmt);
	      gimplify_omp_taskloop_expr (type, &TREE_VEC_ELT (v, 2),
					  for_pre_p, orig_for_stmt);
	    }
	  else
	    gimplify_omp_taskloop_expr (type, &TREE_OPERAND (t, 1), for_pre_p,
					orig_for_stmt);

	  /* Handle OMP_FOR_COND.  */
	  t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
	  if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC)
	    {
	      tree v = TREE_OPERAND (t, 1);
	      gimplify_omp_taskloop_expr (type, &TREE_VEC_ELT (v, 1),
					  for_pre_p, orig_for_stmt);
	      gimplify_omp_taskloop_expr (type, &TREE_VEC_ELT (v, 2),
					  for_pre_p, orig_for_stmt);
	    }
	  else
	    gimplify_omp_taskloop_expr (type, &TREE_OPERAND (t, 1), for_pre_p,
					orig_for_stmt);

	  /* Handle OMP_FOR_INCR.  */
	  t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
	  if (TREE_CODE (t) == MODIFY_EXPR)
	    {
	      decl = TREE_OPERAND (t, 0);
	      t = TREE_OPERAND (t, 1);
	      tree *tp = &TREE_OPERAND (t, 1);
	      if (TREE_CODE (t) == PLUS_EXPR && *tp == decl)
		tp = &TREE_OPERAND (t, 0);

	      gimplify_omp_taskloop_expr (NULL_TREE, tp, for_pre_p,
					  orig_for_stmt);
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
  if (c && walk_tree_without_duplicates (&OMP_FOR_BODY (for_stmt),
					 find_standalone_omp_ordered, NULL))
    {
      OMP_CLAUSE_ORDERED_DOACROSS (c) = 1;
      is_doacross = true;
      int len = TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt));
      gimplify_omp_ctxp->loop_iter_var.create (len * 2);
      for (tree *pc = &OMP_FOR_CLAUSES (for_stmt); *pc; )
	if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_LINEAR)
	  {
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<linear%> clause may not be specified together "
		      "with %<ordered%> clause if stand-alone %<ordered%> "
		      "construct is nested in it");
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	  }
	else
	  pc = &OMP_CLAUSE_CHAIN (*pc);
    }
  int collapse = 1, tile = 0;
  c = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_COLLAPSE);
  if (c)
    collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (c));
  c = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_TILE);
  if (c)
    tile = list_length (OMP_CLAUSE_TILE_LIST (c));
  c = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_ALLOCATE);
  hash_set<tree> *allocate_uids = NULL;
  if (c)
    {
      allocate_uids = new hash_set<tree>;
      for (; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ALLOCATE)
	  allocate_uids->add (OMP_CLAUSE_DECL (c));
    }
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
	    {
	      tree orig_decl = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i);
	      if (TREE_CODE (orig_decl) == TREE_LIST)
		{
		  orig_decl = TREE_PURPOSE (orig_decl);
		  if (!orig_decl)
		    orig_decl = decl;
		}
	      gimplify_omp_ctxp->loop_iter_var.quick_push (orig_decl);
	    }
	  else
	    gimplify_omp_ctxp->loop_iter_var.quick_push (decl);
	  gimplify_omp_ctxp->loop_iter_var.quick_push (decl);
	}

      if (for_stmt == orig_for_stmt)
	{
	  tree orig_decl = decl;
	  if (OMP_FOR_ORIG_DECLS (for_stmt))
	    {
	      tree orig_decl = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i);
	      if (TREE_CODE (orig_decl) == TREE_LIST)
		{
		  orig_decl = TREE_PURPOSE (orig_decl);
		  if (!orig_decl)
		    orig_decl = decl;
		}
	    }
	  if (is_global_var (orig_decl) && DECL_THREAD_LOCAL_P (orig_decl))
	    error_at (EXPR_LOCATION (for_stmt),
		      "threadprivate iteration variable %qD", orig_decl);
	}

      /* Make sure the iteration variable is private.  */
      tree c = NULL_TREE;
      tree c2 = NULL_TREE;
      if (orig_for_stmt != for_stmt)
	{
	  /* Preserve this information until we gimplify the inner simd.  */
	  if (has_decl_expr
	      && bitmap_bit_p (has_decl_expr, DECL_UID (decl)))
	    TREE_PRIVATE (t) = 1;
	}
      else if (ort == ORT_SIMD)
	{
	  splay_tree_node n = splay_tree_lookup (gimplify_omp_ctxp->variables,
						 (splay_tree_key) decl);
	  omp_is_private (gimplify_omp_ctxp, decl,
			  1 + (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt))
			       != 1));
	  if (n != NULL && (n->value & GOVD_DATA_SHARE_CLASS) != 0)
	    {
	      omp_notice_variable (gimplify_omp_ctxp, decl, true);
	      if (n->value & GOVD_LASTPRIVATE_CONDITIONAL)
		for (tree c3 = omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
						OMP_CLAUSE_LASTPRIVATE);
		     c3; c3 = omp_find_clause (OMP_CLAUSE_CHAIN (c3),
					       OMP_CLAUSE_LASTPRIVATE))
		  if (OMP_CLAUSE_DECL (c3) == decl)
		    {
		      warning_at (OMP_CLAUSE_LOCATION (c3), 0,
				  "conditional %<lastprivate%> on loop "
				  "iterator %qD ignored", decl);
		      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c3) = 0;
		      n->value &= ~GOVD_LASTPRIVATE_CONDITIONAL;
		    }
	    }
	  else if (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)) == 1 && !loop_p)
	    {
	      c = build_omp_clause (input_location, OMP_CLAUSE_LINEAR);
	      OMP_CLAUSE_LINEAR_NO_COPYIN (c) = 1;
	      unsigned int flags = GOVD_LINEAR | GOVD_EXPLICIT | GOVD_SEEN;
	      if ((has_decl_expr
		   && bitmap_bit_p (has_decl_expr, DECL_UID (decl)))
		  || TREE_PRIVATE (t))
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
		omp_lastprivate_for_combined_outer_constructs (outer, decl,
							       true);
	    }
	  else
	    {
	      bool lastprivate
		= (!has_decl_expr
		   || !bitmap_bit_p (has_decl_expr, DECL_UID (decl)));
	      if (TREE_PRIVATE (t))
		lastprivate = false;
	      if (loop_p && OMP_FOR_ORIG_DECLS (for_stmt))
		{
		  tree elt = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i);
		  if (TREE_CODE (elt) == TREE_LIST && TREE_PURPOSE (elt))
		    lastprivate = false;
		}

	      struct gimplify_omp_ctx *outer
		= gimplify_omp_ctxp->outer_context;
	      if (outer && lastprivate)
		omp_lastprivate_for_combined_outer_constructs (outer, decl,
							       true);

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
	{
	  omp_notice_variable (gimplify_omp_ctxp, decl, true);
	  splay_tree_node n = splay_tree_lookup (gimplify_omp_ctxp->variables,
						 (splay_tree_key) decl);
	  if (n && (n->value & GOVD_LASTPRIVATE_CONDITIONAL))
	    for (tree c3 = omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
					    OMP_CLAUSE_LASTPRIVATE);
		 c3; c3 = omp_find_clause (OMP_CLAUSE_CHAIN (c3),
					   OMP_CLAUSE_LASTPRIVATE))
	      if (OMP_CLAUSE_DECL (c3) == decl)
		{
		  warning_at (OMP_CLAUSE_LOCATION (c3), 0,
			      "conditional %<lastprivate%> on loop "
			      "iterator %qD ignored", decl);
		  OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c3) = 0;
		  n->value &= ~GOVD_LASTPRIVATE_CONDITIONAL;
		}
	}
      else
	omp_add_variable (gimplify_omp_ctxp, decl, GOVD_PRIVATE | GOVD_SEEN);

      /* If DECL is not a gimple register, create a temporary variable to act
	 as an iteration counter.  This is valid, since DECL cannot be
	 modified in the body of the loop.  Similarly for any iteration vars
	 in simd with collapse > 1 where the iterator vars must be
	 lastprivate.  And similarly for vars mentioned in allocate clauses.  */
      if (orig_for_stmt != for_stmt)
	var = decl;
      else if (!is_gimple_reg (decl)
	       || (ort == ORT_SIMD
		   && TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)) > 1)
	       || (allocate_uids && allocate_uids->contains (decl)))
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

      gimplify_omp_ctxp->in_for_exprs = true;
      if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC)
	{
	  tree lb = TREE_OPERAND (t, 1);
	  tret = gimplify_expr (&TREE_VEC_ELT (lb, 1), &for_pre_body, NULL,
				is_gimple_val, fb_rvalue, false);
	  ret = MIN (ret, tret);
	  tret = gimplify_expr (&TREE_VEC_ELT (lb, 2), &for_pre_body, NULL,
				is_gimple_val, fb_rvalue, false);
	}
      else
	tret = gimplify_expr (&TREE_OPERAND (t, 1), &for_pre_body, NULL,
			      is_gimple_val, fb_rvalue, false);
      gimplify_omp_ctxp->in_for_exprs = false;
      ret = MIN (ret, tret);
      if (ret == GS_ERROR)
	return ret;

      /* Handle OMP_FOR_COND.  */
      t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
      gcc_assert (COMPARISON_CLASS_P (t));
      gcc_assert (TREE_OPERAND (t, 0) == decl);

      gimplify_omp_ctxp->in_for_exprs = true;
      if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC)
	{
	  tree ub = TREE_OPERAND (t, 1);
	  tret = gimplify_expr (&TREE_VEC_ELT (ub, 1), &for_pre_body, NULL,
				is_gimple_val, fb_rvalue, false);
	  ret = MIN (ret, tret);
	  tret = gimplify_expr (&TREE_VEC_ELT (ub, 2), &for_pre_body, NULL,
				is_gimple_val, fb_rvalue, false);
	}
      else
	tret = gimplify_expr (&TREE_OPERAND (t, 1), &for_pre_body, NULL,
			      is_gimple_val, fb_rvalue, false);
      gimplify_omp_ctxp->in_for_exprs = false;
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

	  gimplify_omp_ctxp->in_for_exprs = true;
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
	  gimplify_omp_ctxp->in_for_exprs = false;
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
		push_gimplify_context ();
		gimplify_assign (decl, t, seq);
		gimple *bind = NULL;
		if (gimplify_ctxp->temps)
		  {
		    bind = gimple_build_bind (NULL_TREE, *seq, NULL_TREE);
		    *seq = NULL;
		    gimplify_seq_add_stmt (seq, bind);
		  }
		pop_gimplify_context (bind);
	      }
	}
      if (OMP_FOR_NON_RECTANGULAR (for_stmt) && var != decl)
	for (int j = i + 1; j < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); j++)
	  {
	    t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), j);
	    gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
	    if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC
		&& TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) == decl)
	      TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) = var;
	    t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), j);
	    gcc_assert (COMPARISON_CLASS_P (t));
	    if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC
		&& TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) == decl)
	      TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) = var;
	  }
    }

  BITMAP_FREE (has_decl_expr);
  delete allocate_uids;

  if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP
      || (loop_p && orig_for_stmt == for_stmt))
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

  if (TREE_CODE (orig_for_stmt) == OMP_TASKLOOP
      || (loop_p && orig_for_stmt == for_stmt))
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
	if (OMP_FOR_NON_RECTANGULAR (for_stmt))
	  for (int j = i + 1;
	       j < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); j++)
	    {
	      t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), j);
	      gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
	      if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC
		  && TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) == decl)
		{
		  TREE_OPERAND (t, 1) = copy_node (TREE_OPERAND (t, 1));
		  TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) = var;
		}
	      t = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), j);
	      gcc_assert (COMPARISON_CLASS_P (t));
	      if (TREE_CODE (TREE_OPERAND (t, 1)) == TREE_VEC
		  && TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) == decl)
		{
		  TREE_OPERAND (t, 1) = copy_node (TREE_OPERAND (t, 1));
		  TREE_VEC_ELT (TREE_OPERAND (t, 1), 0) = var;
		}
	  }
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
  if (loop_p && kind == GF_OMP_FOR_KIND_SIMD)
    {
      gimplify_seq_add_seq (pre_p, for_pre_body);
      for_pre_body = NULL;
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
      bitmap lastprivate_uids = NULL;
      if (omp_find_clause (c, OMP_CLAUSE_ALLOCATE))
	{
	  c = omp_find_clause (c, OMP_CLAUSE_LASTPRIVATE);
	  if (c)
	    {
	      lastprivate_uids = BITMAP_ALLOC (NULL);
	      for (; c; c = omp_find_clause (OMP_CLAUSE_CHAIN (c),
					     OMP_CLAUSE_LASTPRIVATE))
		bitmap_set_bit (lastprivate_uids,
				DECL_UID (OMP_CLAUSE_DECL (c)));
	    }
	  c = *gfor_clauses_ptr;
	}
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
	  case OMP_CLAUSE_REDUCTION:
	  case OMP_CLAUSE_IN_REDUCTION:
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
		lang_hooks.decls.omp_finish_clause (*gtask_clauses_ptr, NULL,
						    openacc);
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
	  /* Collapse clause we duplicate on both taskloops.  */
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
	    if (OMP_CLAUSE_LASTPRIVATE_LOOP_IV (c))
	      {
		/* For taskloop C++ lastprivate IVs, we want:
		   1) private on outer taskloop
		   2) firstprivate and shared on task
		   3) lastprivate on inner taskloop  */
		*gtask_clauses_ptr
		  = build_omp_clause (OMP_CLAUSE_LOCATION (c),
				      OMP_CLAUSE_FIRSTPRIVATE);
		OMP_CLAUSE_DECL (*gtask_clauses_ptr) = OMP_CLAUSE_DECL (c);
		lang_hooks.decls.omp_finish_clause (*gtask_clauses_ptr, NULL,
						    openacc);
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
	  /* Allocate clause we duplicate on task and inner taskloop
	     if the decl is lastprivate, otherwise just put on task.  */
	  case OMP_CLAUSE_ALLOCATE:
	    if (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)
		&& DECL_P (OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)))
	      {
		/* Additionally, put firstprivate clause on task
		   for the allocator if it is not constant.  */
		*gtask_clauses_ptr
		  = build_omp_clause (OMP_CLAUSE_LOCATION (c),
				      OMP_CLAUSE_FIRSTPRIVATE);
		OMP_CLAUSE_DECL (*gtask_clauses_ptr)
		  = OMP_CLAUSE_ALLOCATE_ALLOCATOR (c);
		gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (*gtask_clauses_ptr);
	      }
	    if (lastprivate_uids
		&& bitmap_bit_p (lastprivate_uids,
				 DECL_UID (OMP_CLAUSE_DECL (c))))
	      {
		*gfor_clauses_ptr = c;
		gfor_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
		*gtask_clauses_ptr = copy_node (c);
		gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (*gtask_clauses_ptr);
	      }
	    else
	      {
		*gtask_clauses_ptr = c;
		gtask_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	      }
	    break;
	  default:
	    gcc_unreachable ();
	  }
      *gfor_clauses_ptr = NULL_TREE;
      *gtask_clauses_ptr = NULL_TREE;
      *gforo_clauses_ptr = NULL_TREE;
      BITMAP_FREE (lastprivate_uids);
      gimple_set_location (gfor, input_location);
      g = gimple_build_bind (NULL_TREE, gfor, NULL_TREE);
      g = gimple_build_omp_task (g, task_clauses, NULL_TREE, NULL_TREE,
				 NULL_TREE, NULL_TREE, NULL_TREE);
      gimple_set_location (g, input_location);
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
	  if (OMP_FOR_NON_RECTANGULAR (for_stmt))
	    {
	      tree *p1 = NULL, *p2 = NULL;
	      t = gimple_omp_for_initial (gforo, i);
	      if (TREE_CODE (t) == TREE_VEC)
		p1 = &TREE_VEC_ELT (t, 0);
	      t = gimple_omp_for_final (gforo, i);
	      if (TREE_CODE (t) == TREE_VEC)
		{
		  if (p1)
		    p2 = &TREE_VEC_ELT (t, 0);
		  else
		    p1 = &TREE_VEC_ELT (t, 0);
		}
	      if (p1)
		{
		  int j;
		  for (j = 0; j < i; j++)
		    if (*p1 == gimple_omp_for_index (gfor, j))
		      {
			*p1 = gimple_omp_for_index (gforo, j);
			if (p2)
			  *p2 = *p1;
			break;
		      }
		  gcc_assert (j < i);
		}
	    }
	}
      gimplify_seq_add_stmt (pre_p, gforo);
    }
  else
    gimplify_seq_add_stmt (pre_p, gfor);

  if (TREE_CODE (orig_for_stmt) == OMP_FOR)
    {
      struct gimplify_omp_ctx *ctx = gimplify_omp_ctxp;
      unsigned lastprivate_conditional = 0;
      while (ctx
	     && (ctx->region_type == ORT_TARGET_DATA
		 || ctx->region_type == ORT_TASKGROUP))
	ctx = ctx->outer_context;
      if (ctx && (ctx->region_type & ORT_PARALLEL) != 0)
	for (tree c = gimple_omp_for_clauses (gfor);
	     c; c = OMP_CLAUSE_CHAIN (c))
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c))
	    ++lastprivate_conditional;
      if (lastprivate_conditional)
	{
	  struct omp_for_data fd;
	  omp_extract_for_data (gfor, &fd, NULL);
	  tree type = build_array_type_nelts (unsigned_type_for (fd.iter_type),
					      lastprivate_conditional);
	  tree var = create_tmp_var_raw (type);
	  tree c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__CONDTEMP_);
	  OMP_CLAUSE_DECL (c) = var;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_for_clauses (gfor);
	  gimple_omp_for_set_clauses (gfor, c);
	  omp_add_variable (ctx, var, GOVD_CONDTEMP | GOVD_SEEN);
	}
    }
  else if (TREE_CODE (orig_for_stmt) == OMP_SIMD)
    {
      unsigned lastprivate_conditional = 0;
      for (tree c = gimple_omp_for_clauses (gfor); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	    && OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c))
	  ++lastprivate_conditional;
      if (lastprivate_conditional)
	{
	  struct omp_for_data fd;
	  omp_extract_for_data (gfor, &fd, NULL);
	  tree type = unsigned_type_for (fd.iter_type);
	  while (lastprivate_conditional--)
	    {
	      tree c = build_omp_clause (UNKNOWN_LOCATION,
					 OMP_CLAUSE__CONDTEMP_);
	      OMP_CLAUSE_DECL (c) = create_tmp_var (type);
	      OMP_CLAUSE_CHAIN (c) = gimple_omp_for_clauses (gfor);
	      gimple_omp_for_set_clauses (gfor, c);
	    }
	}
    }

  if (ret != GS_ALL_DONE)
    return GS_ERROR;
  *expr_p = NULL_TREE;
  return GS_ALL_DONE;
}

/* Helper for gimplify_omp_loop, called through walk_tree.  */

static tree
note_no_context_vars (tree *tp, int *, void *data)
{
  if (VAR_P (*tp)
      && DECL_CONTEXT (*tp) == NULL_TREE
      && !is_global_var (*tp))
    {
      vec<tree> *d = (vec<tree> *) data;
      d->safe_push (*tp);
      DECL_CONTEXT (*tp) = current_function_decl;
    }
  return NULL_TREE;
}

/* Gimplify the gross structure of an OMP_LOOP statement.  */

static enum gimplify_status
gimplify_omp_loop (tree *expr_p, gimple_seq *pre_p)
{
  tree for_stmt = *expr_p;
  tree clauses = OMP_FOR_CLAUSES (for_stmt);
  struct gimplify_omp_ctx *octx = gimplify_omp_ctxp;
  enum omp_clause_bind_kind kind = OMP_CLAUSE_BIND_THREAD;
  int i;

  /* If order is not present, the behavior is as if order(concurrent)
     appeared.  */
  tree order = omp_find_clause (clauses, OMP_CLAUSE_ORDER);
  if (order == NULL_TREE)
    {
      order = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_ORDER);
      OMP_CLAUSE_CHAIN (order) = clauses;
      OMP_FOR_CLAUSES (for_stmt) = clauses = order;
    }

  tree bind = omp_find_clause (clauses, OMP_CLAUSE_BIND);
  if (bind == NULL_TREE)
    {
      if (!flag_openmp) /* flag_openmp_simd */
	;
      else if (octx && (octx->region_type & ORT_TEAMS) != 0)
	kind = OMP_CLAUSE_BIND_TEAMS;
      else if (octx && (octx->region_type & ORT_PARALLEL) != 0)
	kind = OMP_CLAUSE_BIND_PARALLEL;
      else
	{
	  for (; octx; octx = octx->outer_context)
	    {
	      if ((octx->region_type & ORT_ACC) != 0
		  || octx->region_type == ORT_NONE
		  || octx->region_type == ORT_IMPLICIT_TARGET)
		continue;
	      break;
	    }
	  if (octx == NULL && !in_omp_construct)
	    error_at (EXPR_LOCATION (for_stmt),
		      "%<bind%> clause not specified on a %<loop%> "
		      "construct not nested inside another OpenMP construct");
	}
      bind = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_BIND);
      OMP_CLAUSE_CHAIN (bind) = clauses;
      OMP_CLAUSE_BIND_KIND (bind) = kind;
      OMP_FOR_CLAUSES (for_stmt) = bind;
    }
  else
    switch (OMP_CLAUSE_BIND_KIND (bind))
      {
      case OMP_CLAUSE_BIND_THREAD:
	break;
      case OMP_CLAUSE_BIND_PARALLEL:
	if (!flag_openmp) /* flag_openmp_simd */
	  {
	    OMP_CLAUSE_BIND_KIND (bind) = OMP_CLAUSE_BIND_THREAD;
	    break;
	  }
	for (; octx; octx = octx->outer_context)
	  if (octx->region_type == ORT_SIMD
	      && omp_find_clause (octx->clauses, OMP_CLAUSE_BIND) == NULL_TREE)
	    {
	      error_at (EXPR_LOCATION (for_stmt),
			"%<bind(parallel)%> on a %<loop%> construct nested "
			"inside %<simd%> construct");
	      OMP_CLAUSE_BIND_KIND (bind) = OMP_CLAUSE_BIND_THREAD;
	      break;
	    }
	kind = OMP_CLAUSE_BIND_PARALLEL;
	break;
      case OMP_CLAUSE_BIND_TEAMS:
	if (!flag_openmp) /* flag_openmp_simd */
	  {
	    OMP_CLAUSE_BIND_KIND (bind) = OMP_CLAUSE_BIND_THREAD;
	    break;
	  }
	if ((octx
	     && octx->region_type != ORT_IMPLICIT_TARGET
	     && octx->region_type != ORT_NONE
	     && (octx->region_type & ORT_TEAMS) == 0)
	    || in_omp_construct)
	  {
	    error_at (EXPR_LOCATION (for_stmt),
		      "%<bind(teams)%> on a %<loop%> region not strictly "
		      "nested inside of a %<teams%> region");
	    OMP_CLAUSE_BIND_KIND (bind) = OMP_CLAUSE_BIND_THREAD;
	    break;
	  }
	kind = OMP_CLAUSE_BIND_TEAMS;
	break;
      default:
	gcc_unreachable ();
      }

  for (tree *pc = &OMP_FOR_CLAUSES (for_stmt); *pc; )
    switch (OMP_CLAUSE_CODE (*pc))
      {
      case OMP_CLAUSE_REDUCTION:
	if (OMP_CLAUSE_REDUCTION_INSCAN (*pc))
	  {
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<inscan%> %<reduction%> clause on "
		      "%qs construct", "loop");
	    OMP_CLAUSE_REDUCTION_INSCAN (*pc) = 0;
	  }
	if (OMP_CLAUSE_REDUCTION_TASK (*pc))
	  {
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "invalid %<task%> reduction modifier on construct "
		      "other than %<parallel%>, %qs or %<sections%>",
		      lang_GNU_Fortran () ? "do" : "for");
	    OMP_CLAUSE_REDUCTION_TASK (*pc) = 0;
	  }
	pc = &OMP_CLAUSE_CHAIN (*pc);
	break;
      case OMP_CLAUSE_LASTPRIVATE:
	for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
	  {
	    tree t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
	    gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
	    if (OMP_CLAUSE_DECL (*pc) == TREE_OPERAND (t, 0))
	      break;
	    if (OMP_FOR_ORIG_DECLS (for_stmt)
		&& TREE_CODE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt),
					    i)) == TREE_LIST
		&& TREE_PURPOSE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt),
					       i)))
	      {
		tree orig = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i);
		if (OMP_CLAUSE_DECL (*pc) == TREE_PURPOSE (orig))
		  break;
	      }
	  }
	if (i == TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)))
	  {
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<lastprivate%> clause on a %<loop%> construct refers "
		      "to a variable %qD which is not the loop iterator",
		      OMP_CLAUSE_DECL (*pc));
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	    break;
	  }
	pc = &OMP_CLAUSE_CHAIN (*pc);
	break;
      default:
	pc = &OMP_CLAUSE_CHAIN (*pc);
	break;
    }

  TREE_SET_CODE (for_stmt, OMP_SIMD);

  int last;
  switch (kind)
    {
    case OMP_CLAUSE_BIND_THREAD: last = 0; break;
    case OMP_CLAUSE_BIND_PARALLEL: last = 1; break;
    case OMP_CLAUSE_BIND_TEAMS: last = 2; break;
    }
  for (int pass = 1; pass <= last; pass++)
    {
      if (pass == 2)
	{
	  tree bind = build3 (BIND_EXPR, void_type_node, NULL, NULL,
			      make_node (BLOCK));
	  append_to_statement_list (*expr_p, &BIND_EXPR_BODY (bind));
	  *expr_p = make_node (OMP_PARALLEL);
	  TREE_TYPE (*expr_p) = void_type_node;
	  OMP_PARALLEL_BODY (*expr_p) = bind;
	  OMP_PARALLEL_COMBINED (*expr_p) = 1;
	  SET_EXPR_LOCATION (*expr_p, EXPR_LOCATION (for_stmt));
	  tree *pc = &OMP_PARALLEL_CLAUSES (*expr_p);
	  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
	    if (OMP_FOR_ORIG_DECLS (for_stmt)
		&& (TREE_CODE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i))
		    == TREE_LIST))
	      {
		tree elt = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i);
		if (TREE_PURPOSE (elt) && TREE_VALUE (elt))
		  {
		    *pc = build_omp_clause (UNKNOWN_LOCATION,
					    OMP_CLAUSE_FIRSTPRIVATE);
		    OMP_CLAUSE_DECL (*pc) = TREE_VALUE (elt);
		    pc = &OMP_CLAUSE_CHAIN (*pc);
		  }
	      }
	}
      tree t = make_node (pass == 2 ? OMP_DISTRIBUTE : OMP_FOR);
      tree *pc = &OMP_FOR_CLAUSES (t);
      TREE_TYPE (t) = void_type_node;
      OMP_FOR_BODY (t) = *expr_p;
      SET_EXPR_LOCATION (t, EXPR_LOCATION (for_stmt));
      for (tree c = OMP_FOR_CLAUSES (for_stmt); c; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	  case OMP_CLAUSE_BIND:
	  case OMP_CLAUSE_ORDER:
	  case OMP_CLAUSE_COLLAPSE:
	    *pc = copy_node (c);
	    pc = &OMP_CLAUSE_CHAIN (*pc);
	    break;
	  case OMP_CLAUSE_PRIVATE:
	  case OMP_CLAUSE_FIRSTPRIVATE:
	    /* Only needed on innermost.  */
	    break;
	  case OMP_CLAUSE_LASTPRIVATE:
	    if (OMP_CLAUSE_LASTPRIVATE_LOOP_IV (c) && pass != last)
	      {
		*pc = build_omp_clause (OMP_CLAUSE_LOCATION (c),
					OMP_CLAUSE_FIRSTPRIVATE);
		OMP_CLAUSE_DECL (*pc) = OMP_CLAUSE_DECL (c);
		lang_hooks.decls.omp_finish_clause (*pc, NULL, false);
		pc = &OMP_CLAUSE_CHAIN (*pc);
	      }
	    *pc = copy_node (c);
	    OMP_CLAUSE_LASTPRIVATE_STMT (*pc) = NULL_TREE;
	    TREE_TYPE (*pc) = unshare_expr (TREE_TYPE (c));
	    if (OMP_CLAUSE_LASTPRIVATE_LOOP_IV (c))
	      {
		if (pass != last)
		  OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (*pc) = 1;
		else
		  lang_hooks.decls.omp_finish_clause (*pc, NULL, false);
		OMP_CLAUSE_LASTPRIVATE_LOOP_IV (*pc) = 0;
	      }
	    pc = &OMP_CLAUSE_CHAIN (*pc);
	    break;
	  case OMP_CLAUSE_REDUCTION:
	    *pc = copy_node (c);
	    OMP_CLAUSE_DECL (*pc) = unshare_expr (OMP_CLAUSE_DECL (c));
	    TREE_TYPE (*pc) = unshare_expr (TREE_TYPE (c));
	    if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (*pc))
	      {
		auto_vec<tree> no_context_vars;
		int walk_subtrees = 0;
		note_no_context_vars (&OMP_CLAUSE_REDUCTION_PLACEHOLDER (c),
				      &walk_subtrees, &no_context_vars);
		if (tree p = OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c))
		  note_no_context_vars (&p, &walk_subtrees, &no_context_vars);
		walk_tree_without_duplicates (&OMP_CLAUSE_REDUCTION_INIT (c),
					      note_no_context_vars,
					      &no_context_vars);
		walk_tree_without_duplicates (&OMP_CLAUSE_REDUCTION_MERGE (c),
					      note_no_context_vars,
					      &no_context_vars);

		OMP_CLAUSE_REDUCTION_PLACEHOLDER (*pc)
		  = copy_node (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c));
		if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (*pc))
		  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (*pc)
		    = copy_node (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c));

		hash_map<tree, tree> decl_map;
		decl_map.put (OMP_CLAUSE_DECL (c), OMP_CLAUSE_DECL (c));
		decl_map.put (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c),
			      OMP_CLAUSE_REDUCTION_PLACEHOLDER (*pc));
		if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (*pc))
		  decl_map.put (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c),
				OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (*pc));

		copy_body_data id;
		memset (&id, 0, sizeof (id));
		id.src_fn = current_function_decl;
		id.dst_fn = current_function_decl;
		id.src_cfun = cfun;
		id.decl_map = &decl_map;
		id.copy_decl = copy_decl_no_change;
		id.transform_call_graph_edges = CB_CGE_DUPLICATE;
		id.transform_new_cfg = true;
		id.transform_return_to_modify = false;
		id.eh_lp_nr = 0;
		walk_tree (&OMP_CLAUSE_REDUCTION_INIT (*pc), copy_tree_body_r,
			   &id, NULL);
		walk_tree (&OMP_CLAUSE_REDUCTION_MERGE (*pc), copy_tree_body_r,
			   &id, NULL);

		for (tree d : no_context_vars)
		  {
		    DECL_CONTEXT (d) = NULL_TREE;
		    DECL_CONTEXT (*decl_map.get (d)) = NULL_TREE;
		  }
	      }
	    else
	      {
		OMP_CLAUSE_REDUCTION_INIT (*pc)
		  = unshare_expr (OMP_CLAUSE_REDUCTION_INIT (c));
		OMP_CLAUSE_REDUCTION_MERGE (*pc)
		  = unshare_expr (OMP_CLAUSE_REDUCTION_MERGE (c));
	      }
	    pc = &OMP_CLAUSE_CHAIN (*pc);
	    break;
	  default:
	    gcc_unreachable ();
	  }
      *pc = NULL_TREE;
      *expr_p = t;
    }
  return gimplify_expr (expr_p, pre_p, NULL, is_gimple_stmt, fb_none);
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
	  if (gimplify_omp_ctxp->defaultmap[GDMK_SCALAR] & GOVD_FIRSTPRIVATE)
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
   -2 means that no explicit teams construct was specified
   If teams construct is not present at all, use 1 for num_teams
   and 0 for thread_limit (only one team is involved, and the thread
   limit is implementation defined.  */

static void
optimize_target_teams (tree target, gimple_seq *pre_p)
{
  tree body = OMP_BODY (target);
  tree teams = walk_tree (&body, find_omp_teams, NULL, NULL);
  tree num_teams_lower = NULL_TREE;
  tree num_teams_upper = integer_zero_node;
  tree thread_limit = integer_zero_node;
  location_t num_teams_loc = EXPR_LOCATION (target);
  location_t thread_limit_loc = EXPR_LOCATION (target);
  tree c, *p, expr;
  struct gimplify_omp_ctx *target_ctx = gimplify_omp_ctxp;

  if (teams == NULL_TREE)
    num_teams_upper = build_int_cst (integer_type_node, -2);
  else
    for (c = OMP_TEAMS_CLAUSES (teams); c; c = OMP_CLAUSE_CHAIN (c))
      {
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS)
	  {
	    p = &num_teams_upper;
	    num_teams_loc = OMP_CLAUSE_LOCATION (c);
	    if (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c))
	      {
		expr = OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c);
		if (TREE_CODE (expr) == INTEGER_CST)
		  num_teams_lower = expr;
		else if (walk_tree (&expr, computable_teams_clause,
				    NULL, NULL))
		  num_teams_lower = integer_minus_one_node;
		else
		  {
		    num_teams_lower = expr;
		    gimplify_omp_ctxp = gimplify_omp_ctxp->outer_context;
		    if (gimplify_expr (&num_teams_lower, pre_p, NULL,
				       is_gimple_val, fb_rvalue, false)
			== GS_ERROR)
		      {
			gimplify_omp_ctxp = target_ctx;
			num_teams_lower = integer_minus_one_node;
		      }
		    else
		      {
			gimplify_omp_ctxp = target_ctx;
			if (!DECL_P (expr) && TREE_CODE (expr) != TARGET_EXPR)
			  OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c)
			    = num_teams_lower;
		      }
		  }
	      }
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
  if (!omp_find_clause (OMP_TARGET_CLAUSES (target), OMP_CLAUSE_THREAD_LIMIT))
    {
      c = build_omp_clause (thread_limit_loc, OMP_CLAUSE_THREAD_LIMIT);
      OMP_CLAUSE_THREAD_LIMIT_EXPR (c) = thread_limit;
      OMP_CLAUSE_CHAIN (c) = OMP_TARGET_CLAUSES (target);
      OMP_TARGET_CLAUSES (target) = c;
    }
  c = build_omp_clause (num_teams_loc, OMP_CLAUSE_NUM_TEAMS);
  OMP_CLAUSE_NUM_TEAMS_UPPER_EXPR (c) = num_teams_upper;
  OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c) = num_teams_lower;
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
    case OMP_SCOPE:
      ort = ORT_TASKGROUP;
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
    case OACC_SERIAL:
      ort = ORT_ACC_SERIAL;
      break;
    case OACC_DATA:
      ort = ORT_ACC_DATA;
      break;
    case OMP_TARGET_DATA:
      ort = ORT_TARGET_DATA;
      break;
    case OMP_TEAMS:
      ort = OMP_TEAMS_COMBINED (expr) ? ORT_COMBINED_TEAMS : ORT_TEAMS;
      if (gimplify_omp_ctxp == NULL
	  || gimplify_omp_ctxp->region_type == ORT_IMPLICIT_TARGET)
	ort = (enum omp_region_type) (ort | ORT_HOST_TEAMS);
      break;
    case OACC_HOST_DATA:
      ort = ORT_ACC_HOST_DATA;
      break;
    default:
      gcc_unreachable ();
    }

  bool save_in_omp_construct = in_omp_construct;
  if ((ort & ORT_ACC) == 0)
    in_omp_construct = false;
  gimplify_scan_omp_clauses (&OMP_CLAUSES (expr), pre_p, ort,
			     TREE_CODE (expr));
  if (TREE_CODE (expr) == OMP_TARGET)
    optimize_target_teams (expr, pre_p);
  if ((ort & (ORT_TARGET | ORT_TARGET_DATA)) != 0
      || (ort & ORT_HOST_TEAMS) == ORT_HOST_TEAMS)
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
  in_omp_construct = save_in_omp_construct;

  switch (TREE_CODE (expr))
    {
    case OACC_DATA:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_DATA,
				      OMP_CLAUSES (expr));
      break;
    case OACC_HOST_DATA:
      if (omp_find_clause (OMP_CLAUSES (expr), OMP_CLAUSE_IF_PRESENT))
	{
	  for (tree c = OMP_CLAUSES (expr); c; c = OMP_CLAUSE_CHAIN (c))
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR)
	      OMP_CLAUSE_USE_DEVICE_PTR_IF_PRESENT (c) = 1;
	}

      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_HOST_DATA,
				      OMP_CLAUSES (expr));
      break;
    case OACC_KERNELS:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_KERNELS,
				      OMP_CLAUSES (expr));
      break;
    case OACC_PARALLEL:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_PARALLEL,
				      OMP_CLAUSES (expr));
      break;
    case OACC_SERIAL:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_OACC_SERIAL,
				      OMP_CLAUSES (expr));
      break;
    case OMP_SECTIONS:
      stmt = gimple_build_omp_sections (body, OMP_CLAUSES (expr));
      break;
    case OMP_SINGLE:
      stmt = gimple_build_omp_single (body, OMP_CLAUSES (expr));
      break;
    case OMP_SCOPE:
      stmt = gimple_build_omp_scope (body, OMP_CLAUSES (expr));
      break;
    case OMP_TARGET:
      stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_REGION,
				      OMP_CLAUSES (expr));
      break;
    case OMP_TARGET_DATA:
      /* Put use_device_{ptr,addr} clauses last, as map clauses are supposed
	 to be evaluated before the use_device_{ptr,addr} clauses if they
	 refer to the same variables.  */
      {
	tree use_device_clauses;
	tree *pc, *uc = &use_device_clauses;
	for (pc = &OMP_CLAUSES (expr); *pc; )
	  if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_USE_DEVICE_PTR
	      || OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_USE_DEVICE_ADDR)
	    {
	      *uc = *pc;
	      *pc = OMP_CLAUSE_CHAIN (*pc);
	      uc = &OMP_CLAUSE_CHAIN (*uc);
	    }
	  else
	    pc = &OMP_CLAUSE_CHAIN (*pc);
	*uc = NULL_TREE;
	*pc = use_device_clauses;
	stmt = gimple_build_omp_target (body, GF_OMP_TARGET_KIND_DATA,
					OMP_CLAUSES (expr));
      }
      break;
    case OMP_TEAMS:
      stmt = gimple_build_omp_teams (body, OMP_CLAUSES (expr));
      if ((ort & ORT_HOST_TEAMS) == ORT_HOST_TEAMS)
	gimple_omp_teams_set_host (as_a <gomp_teams *> (stmt), true);
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
      kind = GF_OMP_TARGET_KIND_OACC_ENTER_DATA;
      ort = ORT_ACC;
      break;
    case OACC_EXIT_DATA:
      kind = GF_OMP_TARGET_KIND_OACC_EXIT_DATA;
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
  if (TREE_CODE (expr) == OACC_UPDATE
      && omp_find_clause (OMP_STANDALONE_CLAUSES (expr),
			  OMP_CLAUSE_IF_PRESENT))
    {
      /* The runtime uses GOMP_MAP_{TO,FROM} to denote the if_present
	 clause.  */
      for (tree c = OMP_STANDALONE_CLAUSES (expr); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP)
	  switch (OMP_CLAUSE_MAP_KIND (c))
	    {
	    case GOMP_MAP_FORCE_TO:
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TO);
	      break;
	    case GOMP_MAP_FORCE_FROM:
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_FROM);
	      break;
	    default:
	      break;
	    }
    }
  else if (TREE_CODE (expr) == OACC_EXIT_DATA
	   && omp_find_clause (OMP_STANDALONE_CLAUSES (expr),
			       OMP_CLAUSE_FINALIZE))
    {
      /* Use GOMP_MAP_DELETE/GOMP_MAP_FORCE_FROM to denote "finalize"
	 semantics.  */
      bool have_clause = false;
      for (tree c = OMP_STANDALONE_CLAUSES (expr); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP)
	  switch (OMP_CLAUSE_MAP_KIND (c))
	    {
	    case GOMP_MAP_FROM:
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_FORCE_FROM);
	      have_clause = true;
	      break;
	    case GOMP_MAP_RELEASE:
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_DELETE);
	      have_clause = true;
	      break;
	    case GOMP_MAP_TO_PSET:
	      /* Fortran arrays with descriptors must map that descriptor when
		 doing standalone "attach" operations (in OpenACC).  In that
		 case GOMP_MAP_TO_PSET appears by itself with no preceding
		 clause (see trans-openmp.cc:gfc_trans_omp_clauses).  */
	      break;
	    case GOMP_MAP_POINTER:
	      /* TODO PR92929: we may see these here, but they'll always follow
		 one of the clauses above, and will be handled by libgomp as
		 one group, so no handling required here.  */
	      gcc_assert (have_clause);
	      break;
	    case GOMP_MAP_DETACH:
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_FORCE_DETACH);
	      have_clause = false;
	      break;
	    case GOMP_MAP_STRUCT:
	      have_clause = false;
	      break;
	    default:
	      gcc_unreachable ();
	    }
    }
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
		    tree lhs_var, tree &target_expr, bool rhs, int depth)
{
  tree expr = *expr_p;
  int saw_lhs = 0;

  if (goa_lhs_expr_p (expr, lhs_addr))
    {
      if (pre_p)
	*expr_p = lhs_var;
      return 1;
    }
  if (is_gimple_val (expr))
    return 0;

  /* Maximum depth of lhs in expression is for the
     __builtin_clear_padding (...), __builtin_clear_padding (...),
     __builtin_memcmp (&TARGET_EXPR <lhs, >, ...) == 0 ? ... : lhs;  */
  if (++depth > 7)
    goto finish;

  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_binary:
    case tcc_comparison:
      saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 1), pre_p, lhs_addr,
				     lhs_var, target_expr, true, depth);
      /* FALLTHRU */
    case tcc_unary:
      saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p, lhs_addr,
				     lhs_var, target_expr, true, depth);
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
					 lhs_addr, lhs_var, target_expr, true,
					 depth);
	  /* FALLTHRU */
	case TRUTH_NOT_EXPR:
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
					 lhs_addr, lhs_var, target_expr, true,
					 depth);
	  break;
	case MODIFY_EXPR:
	  if (pre_p && !goa_stabilize_expr (expr_p, NULL, lhs_addr, lhs_var,
					    target_expr, true, depth))
	    break;
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 1), pre_p,
					 lhs_addr, lhs_var, target_expr, true,
					 depth);
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
					 lhs_addr, lhs_var, target_expr, false,
					 depth);
	  break;
	  /* FALLTHRU */
	case ADDR_EXPR:
	  if (pre_p && !goa_stabilize_expr (expr_p, NULL, lhs_addr, lhs_var,
					    target_expr, true, depth))
	    break;
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
					 lhs_addr, lhs_var, target_expr, false,
					 depth);
	  break;
	case COMPOUND_EXPR:
	  /* Break out any preevaluations from cp_build_modify_expr.  */
	  for (; TREE_CODE (expr) == COMPOUND_EXPR;
	       expr = TREE_OPERAND (expr, 1))
	    {
	      /* Special-case __builtin_clear_padding call before
		 __builtin_memcmp.  */
	      if (TREE_CODE (TREE_OPERAND (expr, 0)) == CALL_EXPR)
		{
		  tree fndecl = get_callee_fndecl (TREE_OPERAND (expr, 0));
		  if (fndecl
		      && fndecl_built_in_p (fndecl, BUILT_IN_CLEAR_PADDING)
		      && VOID_TYPE_P (TREE_TYPE (TREE_OPERAND (expr, 0)))
		      && (!pre_p
			  || goa_stabilize_expr (&TREE_OPERAND (expr, 0), NULL,
						 lhs_addr, lhs_var,
						 target_expr, true, depth)))
		    {
		      if (pre_p)
			*expr_p = expr;
		      saw_lhs = goa_stabilize_expr (&TREE_OPERAND (expr, 0),
						    pre_p, lhs_addr, lhs_var,
						    target_expr, true, depth);
		      saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 1),
						     pre_p, lhs_addr, lhs_var,
						     target_expr, rhs, depth);
		      return saw_lhs;
		    }
		}

	      if (pre_p)
		gimplify_stmt (&TREE_OPERAND (expr, 0), pre_p);
	    }
	  if (!pre_p)
	    return goa_stabilize_expr (&expr, pre_p, lhs_addr, lhs_var,
				       target_expr, rhs, depth);
	  *expr_p = expr;
	  return goa_stabilize_expr (expr_p, pre_p, lhs_addr, lhs_var,
				     target_expr, rhs, depth);
	case COND_EXPR:
	  if (!goa_stabilize_expr (&TREE_OPERAND (expr, 0), NULL, lhs_addr,
				   lhs_var, target_expr, true, depth))
	    break;
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
					 lhs_addr, lhs_var, target_expr, true,
					 depth);
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 1), pre_p,
					 lhs_addr, lhs_var, target_expr, true,
					 depth);
	  saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 2), pre_p,
					 lhs_addr, lhs_var, target_expr, true,
					 depth);
	  break;
	case TARGET_EXPR:
	  if (TARGET_EXPR_INITIAL (expr))
	    {
	      if (pre_p && !goa_stabilize_expr (expr_p, NULL, lhs_addr,
						lhs_var, target_expr, true,
						depth))
		break;
	      if (expr == target_expr)
		saw_lhs = 1;
	      else
		{
		  saw_lhs = goa_stabilize_expr (&TARGET_EXPR_INITIAL (expr),
						pre_p, lhs_addr, lhs_var,
						target_expr, true, depth);
		  if (saw_lhs && target_expr == NULL_TREE && pre_p)
		    target_expr = expr;
		}
	    }
	  break;
	default:
	  break;
	}
      break;
    case tcc_reference:
      if (TREE_CODE (expr) == BIT_FIELD_REF
	  || TREE_CODE (expr) == VIEW_CONVERT_EXPR)
	saw_lhs |= goa_stabilize_expr (&TREE_OPERAND (expr, 0), pre_p,
				       lhs_addr, lhs_var, target_expr, true,
				       depth);
      break;
    case tcc_vl_exp:
      if (TREE_CODE (expr) == CALL_EXPR)
	{
	  if (tree fndecl = get_callee_fndecl (expr))
	    if (fndecl_built_in_p (fndecl, BUILT_IN_CLEAR_PADDING)
		|| fndecl_built_in_p (fndecl, BUILT_IN_MEMCMP))
	      {
		int nargs = call_expr_nargs (expr);
		for (int i = 0; i < nargs; i++)
		  saw_lhs |= goa_stabilize_expr (&CALL_EXPR_ARG (expr, i),
						 pre_p, lhs_addr, lhs_var,
						 target_expr, true, depth);
	      }
	}
      break;
    default:
      break;
    }

 finish:
  if (saw_lhs == 0 && pre_p)
    {
      enum gimplify_status gs;
      if (TREE_CODE (expr) == CALL_EXPR && VOID_TYPE_P (TREE_TYPE (expr)))
	{
	  gimplify_stmt (&expr, pre_p);
	  return saw_lhs;
	}
      else if (rhs)
	gs = gimplify_expr (expr_p, pre_p, NULL, is_gimple_val, fb_rvalue);
      else
	gs = gimplify_expr (expr_p, pre_p, NULL, is_gimple_lvalue, fb_lvalue);
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
  tree target_expr = NULL_TREE;

  tmp_load = create_tmp_reg (type);
  if (rhs
      && goa_stabilize_expr (&rhs, pre_p, addr, tmp_load, target_expr,
			     true, 0) < 0)
    return GS_ERROR;

  if (gimplify_expr (&addr, pre_p, NULL, is_gimple_val, fb_rvalue)
      != GS_ALL_DONE)
    return GS_ERROR;

  loadstmt = gimple_build_omp_atomic_load (tmp_load, addr,
					   OMP_ATOMIC_MEMORY_ORDER (*expr_p));
  gimplify_seq_add_stmt (pre_p, loadstmt);
  if (rhs)
    {
      /* BIT_INSERT_EXPR is not valid for non-integral bitfield
	 representatives.  Use BIT_FIELD_REF on the lhs instead.  */
      tree rhsarg = rhs;
      if (TREE_CODE (rhs) == COND_EXPR)
	rhsarg = TREE_OPERAND (rhs, 1);
      if (TREE_CODE (rhsarg) == BIT_INSERT_EXPR
	  && !INTEGRAL_TYPE_P (TREE_TYPE (tmp_load)))
	{
	  tree bitpos = TREE_OPERAND (rhsarg, 2);
	  tree op1 = TREE_OPERAND (rhsarg, 1);
	  tree bitsize;
	  tree tmp_store = tmp_load;
	  if (TREE_CODE (*expr_p) == OMP_ATOMIC_CAPTURE_OLD)
	    tmp_store = get_initialized_tmp_var (tmp_load, pre_p);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (op1)))
	    bitsize = bitsize_int (TYPE_PRECISION (TREE_TYPE (op1)));
	  else
	    bitsize = TYPE_SIZE (TREE_TYPE (op1));
	  gcc_assert (TREE_OPERAND (rhsarg, 0) == tmp_load);
	  tree t = build2_loc (EXPR_LOCATION (rhsarg),
			       MODIFY_EXPR, void_type_node,
			       build3_loc (EXPR_LOCATION (rhsarg),
					   BIT_FIELD_REF, TREE_TYPE (op1),
					   tmp_store, bitsize, bitpos), op1);
	  if (TREE_CODE (rhs) == COND_EXPR)
	    t = build3_loc (EXPR_LOCATION (rhs), COND_EXPR, void_type_node,
			    TREE_OPERAND (rhs, 0), t, void_node);
	  gimplify_and_add (t, pre_p);
	  rhs = tmp_store;
	}
      bool save_allow_rhs_cond_expr = gimplify_ctxp->allow_rhs_cond_expr;
      if (TREE_CODE (rhs) == COND_EXPR)
	gimplify_ctxp->allow_rhs_cond_expr = true;
      enum gimplify_status gs = gimplify_expr (&rhs, pre_p, NULL,
					       is_gimple_val, fb_rvalue);
      gimplify_ctxp->allow_rhs_cond_expr = save_allow_rhs_cond_expr;
      if (gs != GS_ALL_DONE)
	return GS_ERROR;
    }

  if (TREE_CODE (*expr_p) == OMP_ATOMIC_READ)
    rhs = tmp_load;
  storestmt
    = gimple_build_omp_atomic_store (rhs, OMP_ATOMIC_MEMORY_ORDER (*expr_p));
  if (TREE_CODE (*expr_p) != OMP_ATOMIC_READ && OMP_ATOMIC_WEAK (*expr_p))
    {
      gimple_omp_atomic_set_weak (loadstmt);
      gimple_omp_atomic_set_weak (storestmt);
    }
  gimplify_seq_add_stmt (pre_p, storestmt);
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
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DOACROSS
	    && gimplify_omp_ctxp->loop_iter_var.is_empty ())
	  {
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<ordered%> construct with %qs clause must be "
		      "closely nested inside a loop with %<ordered%> clause",
		      OMP_CLAUSE_DOACROSS_DEPEND (c) ? "depend" : "doacross");
	    failures++;
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DOACROSS
		 && OMP_CLAUSE_DOACROSS_KIND (c) == OMP_CLAUSE_DOACROSS_SINK)
	  {
	    bool fail = false;
	    sink_c = c;
	    if (OMP_CLAUSE_DECL (c) == NULL_TREE)
	      continue;  /* omp_cur_iteration - 1 */
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
			  "number of variables in %qs clause with "
			  "%<sink%> modifier does not match number of "
			  "iteration variables",
			  OMP_CLAUSE_DOACROSS_DEPEND (c)
			  ? "depend" : "doacross");
		failures++;
	      }
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DOACROSS
		 && OMP_CLAUSE_DOACROSS_KIND (c) == OMP_CLAUSE_DOACROSS_SOURCE)
	  {
	    if (source_c)
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "more than one %qs clause with %<source%> "
			  "modifier on an %<ordered%> construct",
			  OMP_CLAUSE_DOACROSS_DEPEND (source_c)
			  ? "depend" : "doacross");
		failures++;
	      }
	    else
	      source_c = c;
	  }
    }
  if (source_c && sink_c)
    {
      error_at (OMP_CLAUSE_LOCATION (source_c),
		"%qs clause with %<source%> modifier specified "
		"together with %qs clauses with %<sink%> modifier "
		"on the same construct",
		OMP_CLAUSE_DOACROSS_DEPEND (source_c) ? "depend" : "doacross",
		OMP_CLAUSE_DOACROSS_DEPEND (sink_c) ? "depend" : "doacross");
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
       caller.  The GIMPLE predicates are in gimple.cc.

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
	   || gimple_test_f == is_gimple_condexpr_for_cond
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
	  if ((fallback & fb_rvalue)
	      && is_gimple_reg_type (TREE_TYPE (*expr_p))
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
	      && asan_used_labels->contains (label)
	      && !gimplify_omp_ctxp)
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
	    if (TREE_CODE (*expr_p) == TRY_FINALLY_EXPR
		&& TREE_CODE (TREE_OPERAND (*expr_p, 1)) == EH_ELSE_EXPR)
	      {
		gimple_seq n = NULL, e = NULL;
		gimplify_and_add (TREE_OPERAND (TREE_OPERAND (*expr_p, 1),
						0), &n);
		gimplify_and_add (TREE_OPERAND (TREE_OPERAND (*expr_p, 1),
						1), &e);
		if (!gimple_seq_empty_p (n) && !gimple_seq_empty_p (e))
		  {
		    geh_else *stmt = gimple_build_eh_else (n, e);
		    gimple_seq_add_stmt (&cleanup, stmt);
		  }
	      }
	    else
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
	    copy_warning (ehf, *expr_p);
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

	case OMP_SIMD:
	  {
	    /* Temporarily disable into_ssa, as scan_omp_simd
	       which calls copy_gimple_seq_and_replace_locals can't deal
	       with SSA_NAMEs defined outside of the body properly.  */
	    bool saved_into_ssa = gimplify_ctxp->into_ssa;
	    gimplify_ctxp->into_ssa = false;
	    ret = gimplify_omp_for (expr_p, pre_p);
	    gimplify_ctxp->into_ssa = saved_into_ssa;
	    break;
	  }

	case OMP_FOR:
	case OMP_DISTRIBUTE:
	case OMP_TASKLOOP:
	case OACC_LOOP:
	  ret = gimplify_omp_for (expr_p, pre_p);
	  break;

	case OMP_LOOP:
	  ret = gimplify_omp_loop (expr_p, pre_p);
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
	case OACC_SERIAL:
	case OMP_SCOPE:
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
	case OMP_MASKED:
	case OMP_ORDERED:
	case OMP_CRITICAL:
	case OMP_SCAN:
	  {
	    gimple_seq body = NULL;
	    gimple *g;
	    bool saved_in_omp_construct = in_omp_construct;

	    in_omp_construct = true;
	    gimplify_and_add (OMP_BODY (*expr_p), &body);
	    in_omp_construct = saved_in_omp_construct;
	    switch (TREE_CODE (*expr_p))
	      {
	      case OMP_SECTION:
	        g = gimple_build_omp_section (body);
	        break;
	      case OMP_MASTER:
		g = gimple_build_omp_master (body);
		break;
	      case OMP_ORDERED:
		g = gimplify_omp_ordered (*expr_p, body);
		if (OMP_BODY (*expr_p) == NULL_TREE
		    && gimple_code (g) == GIMPLE_OMP_ORDERED)
		  gimple_omp_ordered_standalone (g);
		break;
	      case OMP_MASKED:
		gimplify_scan_omp_clauses (&OMP_MASKED_CLAUSES (*expr_p),
					   pre_p, ORT_WORKSHARE, OMP_MASKED);
		gimplify_adjust_omp_clauses (pre_p, body,
					     &OMP_MASKED_CLAUSES (*expr_p),
					     OMP_MASKED);
		g = gimple_build_omp_masked (body,
					     OMP_MASKED_CLAUSES (*expr_p));
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
	      case OMP_SCAN:
		gimplify_scan_omp_clauses (&OMP_SCAN_CLAUSES (*expr_p),
					   pre_p, ORT_WORKSHARE, OMP_SCAN);
		gimplify_adjust_omp_clauses (pre_p, body,
					     &OMP_SCAN_CLAUSES (*expr_p),
					     OMP_SCAN);
		g = gimple_build_omp_scan (body, OMP_SCAN_CLAUSES (*expr_p));
		break;
	      default:
		gcc_unreachable ();
	      }
	    gimplify_seq_add_stmt (pre_p, g);
	    ret = GS_ALL_DONE;
	    break;
	  }

	case OMP_TASKGROUP:
	  {
	    gimple_seq body = NULL;

	    tree *pclauses = &OMP_TASKGROUP_CLAUSES (*expr_p);
	    bool saved_in_omp_construct = in_omp_construct;
	    gimplify_scan_omp_clauses (pclauses, pre_p, ORT_TASKGROUP,
				       OMP_TASKGROUP);
	    gimplify_adjust_omp_clauses (pre_p, NULL, pclauses, OMP_TASKGROUP);

	    in_omp_construct = true;
	    gimplify_and_add (OMP_BODY (*expr_p), &body);
	    in_omp_construct = saved_in_omp_construct;
	    gimple_seq cleanup = NULL;
	    tree fn = builtin_decl_explicit (BUILT_IN_GOMP_TASKGROUP_END);
	    gimple *g = gimple_build_call (fn, 0);
	    gimple_seq_add_stmt (&cleanup, g);
	    g = gimple_build_try (body, cleanup, GIMPLE_TRY_FINALLY);
	    body = NULL;
	    gimple_seq_add_stmt (&body, g);
	    g = gimple_build_omp_taskgroup (body, *pclauses);
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
	  goto expr_3;

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
	      if (error_operand_p (TREE_OPERAND (*expr_p, 1)))
		ret = GS_ERROR;
	      else
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
		}
	      break;

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
	       && TYPE_MODE (TREE_TYPE (*expr_p)) != BLKmode
	       && !is_empty_type (TREE_TYPE (*expr_p)))
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
		  && code != EH_ELSE_EXPR
		  && code != OACC_PARALLEL
		  && code != OACC_KERNELS
		  && code != OACC_SERIAL
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
		  && code != OMP_MASKED
		  && code != OMP_TASKGROUP
		  && code != OMP_ORDERED
		  && code != OMP_PARALLEL
		  && code != OMP_SCAN
		  && code != OMP_SECTIONS
		  && code != OMP_SECTION
		  && code != OMP_SINGLE
		  && code != OMP_SCOPE);
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
      tree ref_alias_type = reference_alias_ptr_type (*expr_p);
      unsigned int ref_align = get_object_alignment (*expr_p);
      tree ref_type = TREE_TYPE (*expr_p);
      tmp = build_fold_addr_expr_loc (input_location, *expr_p);
      gimplify_expr (&tmp, pre_p, post_p, is_gimple_reg, fb_rvalue);
      if (TYPE_ALIGN (ref_type) != ref_align)
	ref_type = build_aligned_type (ref_type, ref_align);
      *expr_p = build2 (MEM_REF, ref_type,
			tmp, build_zero_cst (ref_alias_type));
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
  enum gimplify_status ret = gimplify_expr (expr_p, pre_p, post_p,
					    gimple_test_f, fallback);
  if (! allow_ssa
      && TREE_CODE (*expr_p) == SSA_NAME)
    *expr_p = get_initialized_tmp_var (*expr_p, pre_p, NULL, false);
  return ret;
}

/* Look through TYPE for variable-sized objects and gimplify each such
   size that we find.  Add to LIST_P any statements generated.  */

void
gimplify_type_sizes (tree type, gimple_seq *list_p)
{
  if (type == NULL || type == error_mark_node)
    return;

  const bool ignored_p
    = TYPE_NAME (type)
      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
      && DECL_IGNORED_P (TYPE_NAME (type));
  tree t;

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
      if (!ignored_p
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
      for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    gimplify_one_sizepos (&DECL_FIELD_OFFSET (field), list_p);
	    /* Likewise, ensure variable offsets aren't removed.  */
	    if (!ignored_p
		&& (t = DECL_FIELD_OFFSET (field))
		&& VAR_P (t)
		&& DECL_ARTIFICIAL (t))
	      DECL_IGNORED_P (t) = 0;
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
	gimplify_omp_ctxp = new_omp_context (ORT_IMPLICIT_TARGET);
    }

  /* Unshare most shared trees in the body and in that of any nested functions.
     It would seem we don't have to do this for nested functions because
     they are supposed to be output and then the outer function gimplified
     first, but the g++ front end doesn't always do it that way.  */
  unshare_body (fndecl);
  unvisit_body (fndecl);

  /* Make sure input_location isn't set to something weird.  */
  input_location = DECL_SOURCE_LOCATION (fndecl);

  /* Resolve callee-copies.  This has to be done before processing
     the body so that DECL_VALUE_EXPR gets processed correctly.  */
  parm_stmts = do_parms ? gimplify_parameters (&parm_cleanup) : NULL;

  /* Gimplify the function's body.  */
  seq = NULL;
  gimplify_stmt (&DECL_SAVED_TREE (fndecl), &seq);
  outer_stmt = gimple_seq_first_nondebug_stmt (seq);
  if (!outer_stmt)
    {
      outer_stmt = gimple_build_nop ();
      gimplify_seq_add_stmt (&seq, outer_stmt);
    }

  /* The body must contain exactly one statement, a GIMPLE_BIND.  If this is
     not the case, wrap everything in a GIMPLE_BIND to make it so.  */
  if (gimple_code (outer_stmt) == GIMPLE_BIND
      && (gimple_seq_first_nondebug_stmt (seq)
	  == gimple_seq_last_nondebug_stmt (seq)))
    {
      outer_bind = as_a <gbind *> (outer_stmt);
      if (gimple_seq_first_stmt (seq) != outer_stmt
	  || gimple_seq_last_stmt (seq) != outer_stmt)
	{
	  /* If there are debug stmts before or after outer_stmt, move them
	     inside of outer_bind body.  */
	  gimple_stmt_iterator gsi = gsi_for_stmt (outer_stmt, &seq);
	  gimple_seq second_seq = NULL;
	  if (gimple_seq_first_stmt (seq) != outer_stmt
	      && gimple_seq_last_stmt (seq) != outer_stmt)
	    {
	      second_seq = gsi_split_seq_after (gsi);
	      gsi_remove (&gsi, false);
	    }
	  else if (gimple_seq_first_stmt (seq) != outer_stmt)
	    gsi_remove (&gsi, false);
	  else
	    {
	      gsi_remove (&gsi, false);
	      second_seq = seq;
	      seq = NULL;
	    }
	  gimple_seq_add_seq_without_update (&seq,
					     gimple_bind_body (outer_bind));
	  gimple_seq_add_seq_without_update (&seq, second_seq);
	  gimple_bind_set_body (outer_bind, seq);
	}
    }
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

      name = lang_hooks.decl_printable_name (fndecl, 1);
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

/* Build a call to the instrumentation function FNCODE and add it to SEQ.
   If COND_VAR is not NULL, it is a boolean variable guarding the call to
   the instrumentation function.  IF STMT is not NULL, it is a statement
   to be executed just before the call to the instrumentation function.  */

static void
build_instrumentation_call (gimple_seq *seq, enum built_in_function fncode,
			    tree cond_var, gimple *stmt)
{
  /* The instrumentation hooks aren't going to call the instrumented
     function and the address they receive is expected to be matchable
     against symbol addresses.  Make sure we don't create a trampoline,
     in case the current function is nested.  */
  tree this_fn_addr = build_fold_addr_expr (current_function_decl);
  TREE_NO_TRAMPOLINE (this_fn_addr) = 1;

  tree label_true, label_false;
  if (cond_var)
    {
      label_true = create_artificial_label (UNKNOWN_LOCATION);
      label_false = create_artificial_label (UNKNOWN_LOCATION);
      gcond *cond = gimple_build_cond (EQ_EXPR, cond_var, boolean_false_node,
				      label_true, label_false);
      gimplify_seq_add_stmt (seq, cond);
      gimplify_seq_add_stmt (seq, gimple_build_label (label_true));
      gimplify_seq_add_stmt (seq, gimple_build_predict (PRED_COLD_LABEL,
							NOT_TAKEN));
    }

  if (stmt)
    gimplify_seq_add_stmt (seq, stmt);

  tree x = builtin_decl_implicit (BUILT_IN_RETURN_ADDRESS);
  gcall *call = gimple_build_call (x, 1, integer_zero_node);
  tree tmp_var = create_tmp_var (ptr_type_node, "return_addr");
  gimple_call_set_lhs (call, tmp_var);
  gimplify_seq_add_stmt (seq, call);
  x = builtin_decl_implicit (fncode);
  call = gimple_build_call (x, 2, this_fn_addr, tmp_var);
  gimplify_seq_add_stmt (seq, call);

  if (cond_var)
    gimplify_seq_add_stmt (seq, gimple_build_label (label_false));
}

/* Entry point to the gimplification pass.  FNDECL is the FUNCTION_DECL
   node for the function we want to gimplify.

   Return the sequence of GIMPLE statements corresponding to the body
   of FNDECL.  */

void
gimplify_function_tree (tree fndecl)
{
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

  if (asan_sanitize_use_after_scope ())
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
      gimple_seq body = NULL, cleanup = NULL;
      gassign *assign;
      tree cond_var;

      /* If -finstrument-functions-once is specified, generate:

	   static volatile bool C.0 = false;
	   bool tmp_called;

	   tmp_called = C.0;
	   if (!tmp_called)
	     {
	       C.0 = true;
	       [call profiling enter function]
	     }

	 without specific protection for data races.  */
      if (flag_instrument_function_entry_exit > 1)
	{
	  tree first_var
	    = build_decl (DECL_SOURCE_LOCATION (current_function_decl),
			  VAR_DECL,
			  create_tmp_var_name ("C"),
			  boolean_type_node);
	  DECL_ARTIFICIAL (first_var) = 1;
	  DECL_IGNORED_P (first_var) = 1;
	  TREE_STATIC (first_var) = 1;
	  TREE_THIS_VOLATILE (first_var) = 1;
	  TREE_USED (first_var) = 1;
	  DECL_INITIAL (first_var) = boolean_false_node;
	  varpool_node::add (first_var);

	  cond_var = create_tmp_var (boolean_type_node, "tmp_called");
	  assign = gimple_build_assign (cond_var, first_var);
	  gimplify_seq_add_stmt (&body, assign);

	  assign = gimple_build_assign (first_var, boolean_true_node);
	}

      else
	{
	  cond_var = NULL_TREE;
	  assign = NULL;
	}

      build_instrumentation_call (&body, BUILT_IN_PROFILE_FUNC_ENTER,
				  cond_var, assign);

      /* If -finstrument-functions-once is specified, generate:

	   if (!tmp_called)
	     [call profiling exit function]

	 without specific protection for data races.  */
      build_instrumentation_call (&cleanup, BUILT_IN_PROFILE_FUNC_EXIT,
				  cond_var, NULL);

      gimple *tf = gimple_build_try (seq, cleanup, GIMPLE_TRY_FINALLY);
      gimplify_seq_add_stmt (&body, tf);
      gbind *new_bind = gimple_build_bind (NULL, body, NULL);

      /* Replace the current function body with the body
         wrapped in the try/finally TF.  */
      seq = NULL;
      gimple_seq_add_stmt (&seq, new_bind);
      gimple_set_body (fndecl, seq);
      bind = new_bind;
    }

  if (sanitize_flags_p (SANITIZE_THREAD)
      && param_tsan_instrument_func_entry_exit)
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
    /* Handle 'Case 1: Not an array type' from c-common.cc/build_va_arg.  */
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
      location_t xloc
	= expansion_point_location_if_in_system_header (loc);

      /* Unfortunately, this is merely undefined, rather than a constraint
	 violation, so we cannot make this an error.  If this call is never
	 executed, the program is still strictly conforming.  */
      auto_diagnostic_group d;
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
