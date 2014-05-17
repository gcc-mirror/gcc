/* Passes for transactional memory support.
   Copyright (C) 2008-2014 Free Software Foundation, Inc.

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
#include "hash-table.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "calls.h"
#include "function.h"
#include "rtl.h"
#include "emit-rtl.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "tree-cfg.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-into-ssa.h"
#include "tree-pass.h"
#include "tree-inline.h"
#include "diagnostic-core.h"
#include "demangle.h"
#include "output.h"
#include "trans-mem.h"
#include "params.h"
#include "target.h"
#include "langhooks.h"
#include "gimple-pretty-print.h"
#include "cfgloop.h"
#include "tree-ssa-address.h"
#include "predict.h"


#define A_RUNINSTRUMENTEDCODE	0x0001
#define A_RUNUNINSTRUMENTEDCODE	0x0002
#define A_SAVELIVEVARIABLES	0x0004
#define A_RESTORELIVEVARIABLES	0x0008
#define A_ABORTTRANSACTION	0x0010

#define AR_USERABORT		0x0001
#define AR_USERRETRY		0x0002
#define AR_TMCONFLICT		0x0004
#define AR_EXCEPTIONBLOCKABORT	0x0008
#define AR_OUTERABORT		0x0010

#define MODE_SERIALIRREVOCABLE	0x0000


/* The representation of a transaction changes several times during the
   lowering process.  In the beginning, in the front-end we have the
   GENERIC tree TRANSACTION_EXPR.  For example,

	__transaction {
	  local++;
	  if (++global == 10)
	    __tm_abort;
	}

  During initial gimplification (gimplify.c) the TRANSACTION_EXPR node is
  trivially replaced with a GIMPLE_TRANSACTION node.

  During pass_lower_tm, we examine the body of transactions looking
  for aborts.  Transactions that do not contain an abort may be
  merged into an outer transaction.  We also add a TRY-FINALLY node
  to arrange for the transaction to be committed on any exit.

  [??? Think about how this arrangement affects throw-with-commit
  and throw-with-abort operations.  In this case we want the TRY to
  handle gotos, but not to catch any exceptions because the transaction
  will already be closed.]

	GIMPLE_TRANSACTION [label=NULL] {
	  try {
	    local = local + 1;
	    t0 = global;
	    t1 = t0 + 1;
	    global = t1;
	    if (t1 == 10)
	      __builtin___tm_abort ();
	  } finally {
	    __builtin___tm_commit ();
	  }
	}

  During pass_lower_eh, we create EH regions for the transactions,
  intermixed with the regular EH stuff.  This gives us a nice persistent
  mapping (all the way through rtl) from transactional memory operation
  back to the transaction, which allows us to get the abnormal edges
  correct to model transaction aborts and restarts:

	GIMPLE_TRANSACTION [label=over]
	local = local + 1;
	t0 = global;
	t1 = t0 + 1;
	global = t1;
	if (t1 == 10)
	  __builtin___tm_abort ();
	__builtin___tm_commit ();
	over:

  This is the end of all_lowering_passes, and so is what is present
  during the IPA passes, and through all of the optimization passes.

  During pass_ipa_tm, we examine all GIMPLE_TRANSACTION blocks in all
  functions and mark functions for cloning.

  At the end of gimple optimization, before exiting SSA form,
  pass_tm_edges replaces statements that perform transactional
  memory operations with the appropriate TM builtins, and swap
  out function calls with their transactional clones.  At this
  point we introduce the abnormal transaction restart edges and
  complete lowering of the GIMPLE_TRANSACTION node.

	x = __builtin___tm_start (MAY_ABORT);
	eh_label:
	if (x & abort_transaction)
	  goto over;
	local = local + 1;
	t0 = __builtin___tm_load (global);
	t1 = t0 + 1;
	__builtin___tm_store (&global, t1);
	if (t1 == 10)
	  __builtin___tm_abort ();
	__builtin___tm_commit ();
	over:
*/

static void *expand_regions (struct tm_region *,
			     void *(*callback)(struct tm_region *, void *),
			     void *, bool);


/* Return the attributes we want to examine for X, or NULL if it's not
   something we examine.  We look at function types, but allow pointers
   to function types and function decls and peek through.  */

static tree
get_attrs_for (const_tree x)
{
  switch (TREE_CODE (x))
    {
    case FUNCTION_DECL:
      return TYPE_ATTRIBUTES (TREE_TYPE (x));
      break;

    default:
      if (TYPE_P (x))
	return NULL;
      x = TREE_TYPE (x);
      if (TREE_CODE (x) != POINTER_TYPE)
	return NULL;
      /* FALLTHRU */

    case POINTER_TYPE:
      x = TREE_TYPE (x);
      if (TREE_CODE (x) != FUNCTION_TYPE && TREE_CODE (x) != METHOD_TYPE)
	return NULL;
      /* FALLTHRU */

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      return TYPE_ATTRIBUTES (x);
    }
}

/* Return true if X has been marked TM_PURE.  */

bool
is_tm_pure (const_tree x)
{
  unsigned flags;

  switch (TREE_CODE (x))
    {
    case FUNCTION_DECL:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      break;

    default:
      if (TYPE_P (x))
	return false;
      x = TREE_TYPE (x);
      if (TREE_CODE (x) != POINTER_TYPE)
	return false;
      /* FALLTHRU */

    case POINTER_TYPE:
      x = TREE_TYPE (x);
      if (TREE_CODE (x) != FUNCTION_TYPE && TREE_CODE (x) != METHOD_TYPE)
	return false;
      break;
    }

  flags = flags_from_decl_or_type (x);
  return (flags & ECF_TM_PURE) != 0;
}

/* Return true if X has been marked TM_IRREVOCABLE.  */

static bool
is_tm_irrevocable (tree x)
{
  tree attrs = get_attrs_for (x);

  if (attrs && lookup_attribute ("transaction_unsafe", attrs))
    return true;

  /* A call to the irrevocable builtin is by definition,
     irrevocable.  */
  if (TREE_CODE (x) == ADDR_EXPR)
    x = TREE_OPERAND (x, 0);
  if (TREE_CODE (x) == FUNCTION_DECL
      && DECL_BUILT_IN_CLASS (x) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (x) == BUILT_IN_TM_IRREVOCABLE)
    return true;

  return false;
}

/* Return true if X has been marked TM_SAFE.  */

bool
is_tm_safe (const_tree x)
{
  if (flag_tm)
    {
      tree attrs = get_attrs_for (x);
      if (attrs)
	{
	  if (lookup_attribute ("transaction_safe", attrs))
	    return true;
	  if (lookup_attribute ("transaction_may_cancel_outer", attrs))
	    return true;
	}
    }
  return false;
}

/* Return true if CALL is const, or tm_pure.  */

static bool
is_tm_pure_call (gimple call)
{
  tree fn = gimple_call_fn (call);

  if (TREE_CODE (fn) == ADDR_EXPR)
    {
      fn = TREE_OPERAND (fn, 0);
      gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);
    }
  else
    fn = TREE_TYPE (fn);

  return is_tm_pure (fn);
}

/* Return true if X has been marked TM_CALLABLE.  */

static bool
is_tm_callable (tree x)
{
  tree attrs = get_attrs_for (x);
  if (attrs)
    {
      if (lookup_attribute ("transaction_callable", attrs))
	return true;
      if (lookup_attribute ("transaction_safe", attrs))
	return true;
      if (lookup_attribute ("transaction_may_cancel_outer", attrs))
	return true;
    }
  return false;
}

/* Return true if X has been marked TRANSACTION_MAY_CANCEL_OUTER.  */

bool
is_tm_may_cancel_outer (tree x)
{
  tree attrs = get_attrs_for (x);
  if (attrs)
    return lookup_attribute ("transaction_may_cancel_outer", attrs) != NULL;
  return false;
}

/* Return true for built in functions that "end" a transaction.   */

bool
is_tm_ending_fndecl (tree fndecl)
{
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      case BUILT_IN_TM_COMMIT:
      case BUILT_IN_TM_COMMIT_EH:
      case BUILT_IN_TM_ABORT:
      case BUILT_IN_TM_IRREVOCABLE:
	return true;
      default:
	break;
      }

  return false;
}

/* Return true if STMT is a built in function call that "ends" a
   transaction.  */

bool
is_tm_ending (gimple stmt)
{
  tree fndecl;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  return (fndecl != NULL_TREE
	  && is_tm_ending_fndecl (fndecl));
}

/* Return true if STMT is a TM load.  */

static bool
is_tm_load (gimple stmt)
{
  tree fndecl;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  return (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	  && BUILTIN_TM_LOAD_P (DECL_FUNCTION_CODE (fndecl)));
}

/* Same as above, but for simple TM loads, that is, not the
   after-write, after-read, etc optimized variants.  */

static bool
is_tm_simple_load (gimple stmt)
{
  tree fndecl;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
      return (fcode == BUILT_IN_TM_LOAD_1
	      || fcode == BUILT_IN_TM_LOAD_2
	      || fcode == BUILT_IN_TM_LOAD_4
	      || fcode == BUILT_IN_TM_LOAD_8
	      || fcode == BUILT_IN_TM_LOAD_FLOAT
	      || fcode == BUILT_IN_TM_LOAD_DOUBLE
	      || fcode == BUILT_IN_TM_LOAD_LDOUBLE
	      || fcode == BUILT_IN_TM_LOAD_M64
	      || fcode == BUILT_IN_TM_LOAD_M128
	      || fcode == BUILT_IN_TM_LOAD_M256);
    }
  return false;
}

/* Return true if STMT is a TM store.  */

static bool
is_tm_store (gimple stmt)
{
  tree fndecl;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  return (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	  && BUILTIN_TM_STORE_P (DECL_FUNCTION_CODE (fndecl)));
}

/* Same as above, but for simple TM stores, that is, not the
   after-write, after-read, etc optimized variants.  */

static bool
is_tm_simple_store (gimple stmt)
{
  tree fndecl;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
      return (fcode == BUILT_IN_TM_STORE_1
	      || fcode == BUILT_IN_TM_STORE_2
	      || fcode == BUILT_IN_TM_STORE_4
	      || fcode == BUILT_IN_TM_STORE_8
	      || fcode == BUILT_IN_TM_STORE_FLOAT
	      || fcode == BUILT_IN_TM_STORE_DOUBLE
	      || fcode == BUILT_IN_TM_STORE_LDOUBLE
	      || fcode == BUILT_IN_TM_STORE_M64
	      || fcode == BUILT_IN_TM_STORE_M128
	      || fcode == BUILT_IN_TM_STORE_M256);
    }
  return false;
}

/* Return true if FNDECL is BUILT_IN_TM_ABORT.  */

static bool
is_tm_abort (tree fndecl)
{
  return (fndecl
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_TM_ABORT);
}

/* Build a GENERIC tree for a user abort.  This is called by front ends
   while transforming the __tm_abort statement.  */

tree
build_tm_abort_call (location_t loc, bool is_outer)
{
  return build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TM_ABORT), 1,
			      build_int_cst (integer_type_node,
					     AR_USERABORT
					     | (is_outer ? AR_OUTERABORT : 0)));
}

/* Map for aribtrary function replacement under TM, as created
   by the tm_wrap attribute.  */

static GTY((if_marked ("tree_map_marked_p"), param_is (struct tree_map)))
     htab_t tm_wrap_map;

void
record_tm_replacement (tree from, tree to)
{
  struct tree_map **slot, *h;

  /* Do not inline wrapper functions that will get replaced in the TM
     pass.

     Suppose you have foo() that will get replaced into tmfoo().  Make
     sure the inliner doesn't try to outsmart us and inline foo()
     before we get a chance to do the TM replacement.  */
  DECL_UNINLINABLE (from) = 1;

  if (tm_wrap_map == NULL)
    tm_wrap_map = htab_create_ggc (32, tree_map_hash, tree_map_eq, 0);

  h = ggc_alloc<tree_map> ();
  h->hash = htab_hash_pointer (from);
  h->base.from = from;
  h->to = to;

  slot = (struct tree_map **)
    htab_find_slot_with_hash (tm_wrap_map, h, h->hash, INSERT);
  *slot = h;
}

/* Return a TM-aware replacement function for DECL.  */

static tree
find_tm_replacement_function (tree fndecl)
{
  if (tm_wrap_map)
    {
      struct tree_map *h, in;

      in.base.from = fndecl;
      in.hash = htab_hash_pointer (fndecl);
      h = (struct tree_map *) htab_find_with_hash (tm_wrap_map, &in, in.hash);
      if (h)
	return h->to;
    }

  /* ??? We may well want TM versions of most of the common <string.h>
     functions.  For now, we've already these two defined.  */
  /* Adjust expand_call_tm() attributes as necessary for the cases
     handled here:  */
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      case BUILT_IN_MEMCPY:
	return builtin_decl_explicit (BUILT_IN_TM_MEMCPY);
      case BUILT_IN_MEMMOVE:
	return builtin_decl_explicit (BUILT_IN_TM_MEMMOVE);
      case BUILT_IN_MEMSET:
	return builtin_decl_explicit (BUILT_IN_TM_MEMSET);
      default:
	return NULL;
      }

  return NULL;
}

/* When appropriate, record TM replacement for memory allocation functions.

   FROM is the FNDECL to wrap.  */
void
tm_malloc_replacement (tree from)
{
  const char *str;
  tree to;

  if (TREE_CODE (from) != FUNCTION_DECL)
    return;

  /* If we have a previous replacement, the user must be explicitly
     wrapping malloc/calloc/free.  They better know what they're
     doing... */
  if (find_tm_replacement_function (from))
    return;

  str = IDENTIFIER_POINTER (DECL_NAME (from));

  if (!strcmp (str, "malloc"))
    to = builtin_decl_explicit (BUILT_IN_TM_MALLOC);
  else if (!strcmp (str, "calloc"))
    to = builtin_decl_explicit (BUILT_IN_TM_CALLOC);
  else if (!strcmp (str, "free"))
    to = builtin_decl_explicit (BUILT_IN_TM_FREE);
  else
    return;

  TREE_NOTHROW (to) = 0;

  record_tm_replacement (from, to);
}

/* Diagnostics for tm_safe functions/regions.  Called by the front end
   once we've lowered the function to high-gimple.  */

/* Subroutine of diagnose_tm_safe_errors, called through walk_gimple_seq.
   Process exactly one statement.  WI->INFO is set to non-null when in
   the context of a tm_safe function, and null for a __transaction block.  */

#define DIAG_TM_OUTER		1
#define DIAG_TM_SAFE		2
#define DIAG_TM_RELAXED		4

struct diagnose_tm
{
  unsigned int summary_flags : 8;
  unsigned int block_flags : 8;
  unsigned int func_flags : 8;
  unsigned int saw_volatile : 1;
  gimple stmt;
};

/* Return true if T is a volatile variable of some kind.  */

static bool
volatile_var_p (tree t)
{
  return (SSA_VAR_P (t)
	  && TREE_THIS_VOLATILE (TREE_TYPE (t)));
}

/* Tree callback function for diagnose_tm pass.  */

static tree
diagnose_tm_1_op (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct diagnose_tm *d = (struct diagnose_tm *) wi->info;

  if (volatile_var_p (*tp)
      && d->block_flags & DIAG_TM_SAFE
      && !d->saw_volatile)
    {
      d->saw_volatile = 1;
      error_at (gimple_location (d->stmt),
		"invalid volatile use of %qD inside transaction",
		*tp);
    }

  return NULL_TREE;
}

static inline bool
is_tm_safe_or_pure (const_tree x)
{
  return is_tm_safe (x) || is_tm_pure (x);
}

static tree
diagnose_tm_1 (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		    struct walk_stmt_info *wi)
{
  gimple stmt = gsi_stmt (*gsi);
  struct diagnose_tm *d = (struct diagnose_tm *) wi->info;

  /* Save stmt for use in leaf analysis.  */
  d->stmt = stmt;

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      {
	tree fn = gimple_call_fn (stmt);

	if ((d->summary_flags & DIAG_TM_OUTER) == 0
	    && is_tm_may_cancel_outer (fn))
	  error_at (gimple_location (stmt),
		    "%<transaction_may_cancel_outer%> function call not within"
		    " outer transaction or %<transaction_may_cancel_outer%>");

	if (d->summary_flags & DIAG_TM_SAFE)
	  {
	    bool is_safe, direct_call_p;
	    tree replacement;

	    if (TREE_CODE (fn) == ADDR_EXPR
		&& TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL)
	      {
		direct_call_p = true;
		replacement = TREE_OPERAND (fn, 0);
		replacement = find_tm_replacement_function (replacement);
		if (replacement)
		  fn = replacement;
	      }
	    else
	      {
		direct_call_p = false;
		replacement = NULL_TREE;
	      }

	    if (is_tm_safe_or_pure (fn))
	      is_safe = true;
	    else if (is_tm_callable (fn) || is_tm_irrevocable (fn))
	      {
		/* A function explicitly marked transaction_callable as
		   opposed to transaction_safe is being defined to be
		   unsafe as part of its ABI, regardless of its contents.  */
		is_safe = false;
	      }
	    else if (direct_call_p)
	      {
		if (IS_TYPE_OR_DECL_P (fn)
		    && flags_from_decl_or_type (fn) & ECF_TM_BUILTIN)
		  is_safe = true;
		else if (replacement)
		  {
		    /* ??? At present we've been considering replacements
		       merely transaction_callable, and therefore might
		       enter irrevocable.  The tm_wrap attribute has not
		       yet made it into the new language spec.  */
		    is_safe = false;
		  }
		else
		  {
		    /* ??? Diagnostics for unmarked direct calls moved into
		       the IPA pass.  Section 3.2 of the spec details how
		       functions not marked should be considered "implicitly
		       safe" based on having examined the function body.  */
		    is_safe = true;
		  }
	      }
	    else
	      {
		/* An unmarked indirect call.  Consider it unsafe even
		   though optimization may yet figure out how to inline.  */
		is_safe = false;
	      }

	    if (!is_safe)
	      {
		if (TREE_CODE (fn) == ADDR_EXPR)
		  fn = TREE_OPERAND (fn, 0);
		if (d->block_flags & DIAG_TM_SAFE)
		  {
		    if (direct_call_p)
		      error_at (gimple_location (stmt),
				"unsafe function call %qD within "
				"atomic transaction", fn);
		    else
		      {
			if (!DECL_P (fn) || DECL_NAME (fn))
			  error_at (gimple_location (stmt),
				    "unsafe function call %qE within "
				    "atomic transaction", fn);
			else
			  error_at (gimple_location (stmt),
				    "unsafe indirect function call within "
				    "atomic transaction");
		      }
		  }
		else
		  {
		    if (direct_call_p)
		      error_at (gimple_location (stmt),
				"unsafe function call %qD within "
				"%<transaction_safe%> function", fn);
		    else
		      {
			if (!DECL_P (fn) || DECL_NAME (fn))
			  error_at (gimple_location (stmt),
				    "unsafe function call %qE within "
				    "%<transaction_safe%> function", fn);
			else
			  error_at (gimple_location (stmt),
				    "unsafe indirect function call within "
				    "%<transaction_safe%> function");
		      }
		  }
	      }
	  }
      }
      break;

    case GIMPLE_ASM:
      /* ??? We ought to come up with a way to add attributes to
	 asm statements, and then add "transaction_safe" to it.
	 Either that or get the language spec to resurrect __tm_waiver.  */
      if (d->block_flags & DIAG_TM_SAFE)
	error_at (gimple_location (stmt),
		  "asm not allowed in atomic transaction");
      else if (d->func_flags & DIAG_TM_SAFE)
	error_at (gimple_location (stmt),
		  "asm not allowed in %<transaction_safe%> function");
      break;

    case GIMPLE_TRANSACTION:
      {
	unsigned char inner_flags = DIAG_TM_SAFE;

	if (gimple_transaction_subcode (stmt) & GTMA_IS_RELAXED)
	  {
	    if (d->block_flags & DIAG_TM_SAFE)
	      error_at (gimple_location (stmt),
			"relaxed transaction in atomic transaction");
	    else if (d->func_flags & DIAG_TM_SAFE)
	      error_at (gimple_location (stmt),
			"relaxed transaction in %<transaction_safe%> function");
	    inner_flags = DIAG_TM_RELAXED;
	  }
	else if (gimple_transaction_subcode (stmt) & GTMA_IS_OUTER)
	  {
	    if (d->block_flags)
	      error_at (gimple_location (stmt),
			"outer transaction in transaction");
	    else if (d->func_flags & DIAG_TM_OUTER)
	      error_at (gimple_location (stmt),
			"outer transaction in "
			"%<transaction_may_cancel_outer%> function");
	    else if (d->func_flags & DIAG_TM_SAFE)
	      error_at (gimple_location (stmt),
			"outer transaction in %<transaction_safe%> function");
	    inner_flags |= DIAG_TM_OUTER;
	  }

	*handled_ops_p = true;
	if (gimple_transaction_body (stmt))
	  {
	    struct walk_stmt_info wi_inner;
	    struct diagnose_tm d_inner;

	    memset (&d_inner, 0, sizeof (d_inner));
	    d_inner.func_flags = d->func_flags;
	    d_inner.block_flags = d->block_flags | inner_flags;
	    d_inner.summary_flags = d_inner.func_flags | d_inner.block_flags;

	    memset (&wi_inner, 0, sizeof (wi_inner));
	    wi_inner.info = &d_inner;

	    walk_gimple_seq (gimple_transaction_body (stmt),
			     diagnose_tm_1, diagnose_tm_1_op, &wi_inner);
	  }
      }
      break;

    default:
      break;
    }

  return NULL_TREE;
}

static unsigned int
diagnose_tm_blocks (void)
{
  struct walk_stmt_info wi;
  struct diagnose_tm d;

  memset (&d, 0, sizeof (d));
  if (is_tm_may_cancel_outer (current_function_decl))
    d.func_flags = DIAG_TM_OUTER | DIAG_TM_SAFE;
  else if (is_tm_safe (current_function_decl))
    d.func_flags = DIAG_TM_SAFE;
  d.summary_flags = d.func_flags;

  memset (&wi, 0, sizeof (wi));
  wi.info = &d;

  walk_gimple_seq (gimple_body (current_function_decl),
		   diagnose_tm_1, diagnose_tm_1_op, &wi);

  return 0;
}

namespace {

const pass_data pass_data_diagnose_tm_blocks =
{
  GIMPLE_PASS, /* type */
  "*diagnose_tm_blocks", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_diagnose_tm_blocks : public gimple_opt_pass
{
public:
  pass_diagnose_tm_blocks (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_diagnose_tm_blocks, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tm; }
  virtual unsigned int execute (function *) { return diagnose_tm_blocks (); }

}; // class pass_diagnose_tm_blocks

} // anon namespace

gimple_opt_pass *
make_pass_diagnose_tm_blocks (gcc::context *ctxt)
{
  return new pass_diagnose_tm_blocks (ctxt);
}

/* Instead of instrumenting thread private memory, we save the
   addresses in a log which we later use to save/restore the addresses
   upon transaction start/restart.

   The log is keyed by address, where each element contains individual
   statements among different code paths that perform the store.

   This log is later used to generate either plain save/restore of the
   addresses upon transaction start/restart, or calls to the ITM_L*
   logging functions.

   So for something like:

       struct large { int x[1000]; };
       struct large lala = { 0 };
       __transaction {
	 lala.x[i] = 123;
	 ...
       }

   We can either save/restore:

       lala = { 0 };
       trxn = _ITM_startTransaction ();
       if (trxn & a_saveLiveVariables)
	 tmp_lala1 = lala.x[i];
       else if (a & a_restoreLiveVariables)
	 lala.x[i] = tmp_lala1;

   or use the logging functions:

       lala = { 0 };
       trxn = _ITM_startTransaction ();
       _ITM_LU4 (&lala.x[i]);

   Obviously, if we use _ITM_L* to log, we prefer to call _ITM_L* as
   far up the dominator tree to shadow all of the writes to a given
   location (thus reducing the total number of logging calls), but not
   so high as to be called on a path that does not perform a
   write.  */

/* One individual log entry.  We may have multiple statements for the
   same location if neither dominate each other (on different
   execution paths).  */
typedef struct tm_log_entry
{
  /* Address to save.  */
  tree addr;
  /* Entry block for the transaction this address occurs in.  */
  basic_block entry_block;
  /* Dominating statements the store occurs in.  */
  gimple_vec stmts;
  /* Initially, while we are building the log, we place a nonzero
     value here to mean that this address *will* be saved with a
     save/restore sequence.  Later, when generating the save sequence
     we place the SSA temp generated here.  */
  tree save_var;
} *tm_log_entry_t;


/* Log entry hashtable helpers.  */

struct log_entry_hasher
{
  typedef tm_log_entry value_type;
  typedef tm_log_entry compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Htab support.  Return hash value for a `tm_log_entry'.  */
inline hashval_t
log_entry_hasher::hash (const value_type *log)
{
  return iterative_hash_expr (log->addr, 0);
}

/* Htab support.  Return true if two log entries are the same.  */
inline bool
log_entry_hasher::equal (const value_type *log1, const compare_type *log2)
{
  /* FIXME:

     rth: I suggest that we get rid of the component refs etc.
     I.e. resolve the reference to base + offset.

     We may need to actually finish a merge with mainline for this,
     since we'd like to be presented with Richi's MEM_REF_EXPRs more
     often than not.  But in the meantime your tm_log_entry could save
     the results of get_inner_reference.

     See: g++.dg/tm/pr46653.C
  */

  /* Special case plain equality because operand_equal_p() below will
     return FALSE if the addresses are equal but they have
     side-effects (e.g. a volatile address).  */
  if (log1->addr == log2->addr)
    return true;

  return operand_equal_p (log1->addr, log2->addr, 0);
}

/* Htab support.  Free one tm_log_entry.  */
inline void
log_entry_hasher::remove (value_type *lp)
{
  lp->stmts.release ();
  free (lp);
}


/* The actual log.  */
static hash_table <log_entry_hasher> tm_log;

/* Addresses to log with a save/restore sequence.  These should be in
   dominator order.  */
static vec<tree> tm_log_save_addresses;

enum thread_memory_type
  {
    mem_non_local = 0,
    mem_thread_local,
    mem_transaction_local,
    mem_max
  };

typedef struct tm_new_mem_map
{
  /* SSA_NAME being dereferenced.  */
  tree val;
  enum thread_memory_type local_new_memory;
} tm_new_mem_map_t;

/* Hashtable helpers.  */

struct tm_mem_map_hasher : typed_free_remove <tm_new_mem_map_t>
{
  typedef tm_new_mem_map_t value_type;
  typedef tm_new_mem_map_t compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

inline hashval_t
tm_mem_map_hasher::hash (const value_type *v)
{
  return (intptr_t)v->val >> 4;
}

inline bool
tm_mem_map_hasher::equal (const value_type *v, const compare_type *c)
{
  return v->val == c->val;
}

/* Map for an SSA_NAME originally pointing to a non aliased new piece
   of memory (malloc, alloc, etc).  */
static hash_table <tm_mem_map_hasher> tm_new_mem_hash;

/* Initialize logging data structures.  */
static void
tm_log_init (void)
{
  tm_log.create (10);
  tm_new_mem_hash.create (5);
  tm_log_save_addresses.create (5);
}

/* Free logging data structures.  */
static void
tm_log_delete (void)
{
  tm_log.dispose ();
  tm_new_mem_hash.dispose ();
  tm_log_save_addresses.release ();
}

/* Return true if MEM is a transaction invariant memory for the TM
   region starting at REGION_ENTRY_BLOCK.  */
static bool
transaction_invariant_address_p (const_tree mem, basic_block region_entry_block)
{
  if ((TREE_CODE (mem) == INDIRECT_REF || TREE_CODE (mem) == MEM_REF)
      && TREE_CODE (TREE_OPERAND (mem, 0)) == SSA_NAME)
    {
      basic_block def_bb;

      def_bb = gimple_bb (SSA_NAME_DEF_STMT (TREE_OPERAND (mem, 0)));
      return def_bb != region_entry_block
	&& dominated_by_p (CDI_DOMINATORS, region_entry_block, def_bb);
    }

  mem = strip_invariant_refs (mem);
  return mem && (CONSTANT_CLASS_P (mem) || decl_address_invariant_p (mem));
}

/* Given an address ADDR in STMT, find it in the memory log or add it,
   making sure to keep only the addresses highest in the dominator
   tree.

   ENTRY_BLOCK is the entry_block for the transaction.

   If we find the address in the log, make sure it's either the same
   address, or an equivalent one that dominates ADDR.

   If we find the address, but neither ADDR dominates the found
   address, nor the found one dominates ADDR, we're on different
   execution paths.  Add it.

   If known, ENTRY_BLOCK is the entry block for the region, otherwise
   NULL.  */
static void
tm_log_add (basic_block entry_block, tree addr, gimple stmt)
{
  tm_log_entry **slot;
  struct tm_log_entry l, *lp;

  l.addr = addr;
  slot = tm_log.find_slot (&l, INSERT);
  if (!*slot)
    {
      tree type = TREE_TYPE (addr);

      lp = XNEW (struct tm_log_entry);
      lp->addr = addr;
      *slot = lp;

      /* Small invariant addresses can be handled as save/restores.  */
      if (entry_block
	  && transaction_invariant_address_p (lp->addr, entry_block)
	  && TYPE_SIZE_UNIT (type) != NULL
	  && tree_fits_uhwi_p (TYPE_SIZE_UNIT (type))
	  && ((HOST_WIDE_INT) tree_to_uhwi (TYPE_SIZE_UNIT (type))
	      < PARAM_VALUE (PARAM_TM_MAX_AGGREGATE_SIZE))
	  /* We must be able to copy this type normally.  I.e., no
	     special constructors and the like.  */
	  && !TREE_ADDRESSABLE (type))
	{
	  lp->save_var = create_tmp_reg (TREE_TYPE (lp->addr), "tm_save");
	  lp->stmts.create (0);
	  lp->entry_block = entry_block;
	  /* Save addresses separately in dominator order so we don't
	     get confused by overlapping addresses in the save/restore
	     sequence.  */
	  tm_log_save_addresses.safe_push (lp->addr);
	}
      else
	{
	  /* Use the logging functions.  */
	  lp->stmts.create (5);
	  lp->stmts.quick_push (stmt);
	  lp->save_var = NULL;
	}
    }
  else
    {
      size_t i;
      gimple oldstmt;

      lp = *slot;

      /* If we're generating a save/restore sequence, we don't care
	 about statements.  */
      if (lp->save_var)
	return;

      for (i = 0; lp->stmts.iterate (i, &oldstmt); ++i)
	{
	  if (stmt == oldstmt)
	    return;
	  /* We already have a store to the same address, higher up the
	     dominator tree.  Nothing to do.  */
	  if (dominated_by_p (CDI_DOMINATORS,
			      gimple_bb (stmt), gimple_bb (oldstmt)))
	    return;
	  /* We should be processing blocks in dominator tree order.  */
	  gcc_assert (!dominated_by_p (CDI_DOMINATORS,
				       gimple_bb (oldstmt), gimple_bb (stmt)));
	}
      /* Store is on a different code path.  */
      lp->stmts.safe_push (stmt);
    }
}

/* Gimplify the address of a TARGET_MEM_REF.  Return the SSA_NAME
   result, insert the new statements before GSI.  */

static tree
gimplify_addr (gimple_stmt_iterator *gsi, tree x)
{
  if (TREE_CODE (x) == TARGET_MEM_REF)
    x = tree_mem_ref_addr (build_pointer_type (TREE_TYPE (x)), x);
  else
    x = build_fold_addr_expr (x);
  return force_gimple_operand_gsi (gsi, x, true, NULL, true, GSI_SAME_STMT);
}

/* Instrument one address with the logging functions.
   ADDR is the address to save.
   STMT is the statement before which to place it.  */
static void
tm_log_emit_stmt (tree addr, gimple stmt)
{
  tree type = TREE_TYPE (addr);
  tree size = TYPE_SIZE_UNIT (type);
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gimple log;
  enum built_in_function code = BUILT_IN_TM_LOG;

  if (type == float_type_node)
    code = BUILT_IN_TM_LOG_FLOAT;
  else if (type == double_type_node)
    code = BUILT_IN_TM_LOG_DOUBLE;
  else if (type == long_double_type_node)
    code = BUILT_IN_TM_LOG_LDOUBLE;
  else if (tree_fits_uhwi_p (size))
    {
      unsigned int n = tree_to_uhwi (size);
      switch (n)
	{
	case 1:
	  code = BUILT_IN_TM_LOG_1;
	  break;
	case 2:
	  code = BUILT_IN_TM_LOG_2;
	  break;
	case 4:
	  code = BUILT_IN_TM_LOG_4;
	  break;
	case 8:
	  code = BUILT_IN_TM_LOG_8;
	  break;
	default:
	  code = BUILT_IN_TM_LOG;
	  if (TREE_CODE (type) == VECTOR_TYPE)
	    {
	      if (n == 8 && builtin_decl_explicit (BUILT_IN_TM_LOG_M64))
		code = BUILT_IN_TM_LOG_M64;
	      else if (n == 16 && builtin_decl_explicit (BUILT_IN_TM_LOG_M128))
		code = BUILT_IN_TM_LOG_M128;
	      else if (n == 32 && builtin_decl_explicit (BUILT_IN_TM_LOG_M256))
		code = BUILT_IN_TM_LOG_M256;
	    }
	  break;
	}
    }

  addr = gimplify_addr (&gsi, addr);
  if (code == BUILT_IN_TM_LOG)
    log = gimple_build_call (builtin_decl_explicit (code), 2, addr,  size);
  else
    log = gimple_build_call (builtin_decl_explicit (code), 1, addr);
  gsi_insert_before (&gsi, log, GSI_SAME_STMT);
}

/* Go through the log and instrument address that must be instrumented
   with the logging functions.  Leave the save/restore addresses for
   later.  */
static void
tm_log_emit (void)
{
  hash_table <log_entry_hasher>::iterator hi;
  struct tm_log_entry *lp;

  FOR_EACH_HASH_TABLE_ELEMENT (tm_log, lp, tm_log_entry_t, hi)
    {
      size_t i;
      gimple stmt;

      if (dump_file)
	{
	  fprintf (dump_file, "TM thread private mem logging: ");
	  print_generic_expr (dump_file, lp->addr, 0);
	  fprintf (dump_file, "\n");
	}

      if (lp->save_var)
	{
	  if (dump_file)
	    fprintf (dump_file, "DUMPING to variable\n");
	  continue;
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "DUMPING with logging functions\n");
	  for (i = 0; lp->stmts.iterate (i, &stmt); ++i)
	    tm_log_emit_stmt (lp->addr, stmt);
	}
    }
}

/* Emit the save sequence for the corresponding addresses in the log.
   ENTRY_BLOCK is the entry block for the transaction.
   BB is the basic block to insert the code in.  */
static void
tm_log_emit_saves (basic_block entry_block, basic_block bb)
{
  size_t i;
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gimple stmt;
  struct tm_log_entry l, *lp;

  for (i = 0; i < tm_log_save_addresses.length (); ++i)
    {
      l.addr = tm_log_save_addresses[i];
      lp = *(tm_log.find_slot (&l, NO_INSERT));
      gcc_assert (lp->save_var != NULL);

      /* We only care about variables in the current transaction.  */
      if (lp->entry_block != entry_block)
	continue;

      stmt = gimple_build_assign (lp->save_var, unshare_expr (lp->addr));

      /* Make sure we can create an SSA_NAME for this type.  For
	 instance, aggregates aren't allowed, in which case the system
	 will create a VOP for us and everything will just work.  */
      if (is_gimple_reg_type (TREE_TYPE (lp->save_var)))
	{
	  lp->save_var = make_ssa_name (lp->save_var, stmt);
	  gimple_assign_set_lhs (stmt, lp->save_var);
	}

      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
    }
}

/* Emit the restore sequence for the corresponding addresses in the log.
   ENTRY_BLOCK is the entry block for the transaction.
   BB is the basic block to insert the code in.  */
static void
tm_log_emit_restores (basic_block entry_block, basic_block bb)
{
  int i;
  struct tm_log_entry l, *lp;
  gimple_stmt_iterator gsi;
  gimple stmt;

  for (i = tm_log_save_addresses.length () - 1; i >= 0; i--)
    {
      l.addr = tm_log_save_addresses[i];
      lp = *(tm_log.find_slot (&l, NO_INSERT));
      gcc_assert (lp->save_var != NULL);

      /* We only care about variables in the current transaction.  */
      if (lp->entry_block != entry_block)
	continue;

      /* Restores are in LIFO order from the saves in case we have
	 overlaps.  */
      gsi = gsi_start_bb (bb);

      stmt = gimple_build_assign (unshare_expr (lp->addr), lp->save_var);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);
    }
}


static tree lower_sequence_tm (gimple_stmt_iterator *, bool *,
			       struct walk_stmt_info *);
static tree lower_sequence_no_tm (gimple_stmt_iterator *, bool *,
				  struct walk_stmt_info *);

/* Evaluate an address X being dereferenced and determine if it
   originally points to a non aliased new chunk of memory (malloc,
   alloca, etc).

   Return MEM_THREAD_LOCAL if it points to a thread-local address.
   Return MEM_TRANSACTION_LOCAL if it points to a transaction-local address.
   Return MEM_NON_LOCAL otherwise.

   ENTRY_BLOCK is the entry block to the transaction containing the
   dereference of X.  */
static enum thread_memory_type
thread_private_new_memory (basic_block entry_block, tree x)
{
  gimple stmt = NULL;
  enum tree_code code;
  tm_new_mem_map_t **slot;
  tm_new_mem_map_t elt, *elt_p;
  tree val = x;
  enum thread_memory_type retval = mem_transaction_local;

  if (!entry_block
      || TREE_CODE (x) != SSA_NAME
      /* Possible uninitialized use, or a function argument.  In
	 either case, we don't care.  */
      || SSA_NAME_IS_DEFAULT_DEF (x))
    return mem_non_local;

  /* Look in cache first.  */
  elt.val = x;
  slot = tm_new_mem_hash.find_slot (&elt, INSERT);
  elt_p = *slot;
  if (elt_p)
    return elt_p->local_new_memory;

  /* Optimistically assume the memory is transaction local during
     processing.  This catches recursion into this variable.  */
  *slot = elt_p = XNEW (tm_new_mem_map_t);
  elt_p->val = val;
  elt_p->local_new_memory = mem_transaction_local;

  /* Search DEF chain to find the original definition of this address.  */
  do
    {
      if (ptr_deref_may_alias_global_p (x))
	{
	  /* Address escapes.  This is not thread-private.  */
	  retval = mem_non_local;
	  goto new_memory_ret;
	}

      stmt = SSA_NAME_DEF_STMT (x);

      /* If the malloc call is outside the transaction, this is
	 thread-local.  */
      if (retval != mem_thread_local
	  && !dominated_by_p (CDI_DOMINATORS, gimple_bb (stmt), entry_block))
	retval = mem_thread_local;

      if (is_gimple_assign (stmt))
	{
	  code = gimple_assign_rhs_code (stmt);
	  /* x = foo ==> foo */
	  if (code == SSA_NAME)
	    x = gimple_assign_rhs1 (stmt);
	  /* x = foo + n ==> foo */
	  else if (code == POINTER_PLUS_EXPR)
	    x = gimple_assign_rhs1 (stmt);
	  /* x = (cast*) foo ==> foo */
	  else if (code == VIEW_CONVERT_EXPR || code == NOP_EXPR)
	    x = gimple_assign_rhs1 (stmt);
	  /* x = c ? op1 : op2 == > op1 or op2 just like a PHI */
	  else if (code == COND_EXPR)
	    {
	      tree op1 = gimple_assign_rhs2 (stmt);
	      tree op2 = gimple_assign_rhs3 (stmt);
	      enum thread_memory_type mem;
	      retval = thread_private_new_memory (entry_block, op1);
	      if (retval == mem_non_local)
		goto new_memory_ret;
	      mem = thread_private_new_memory (entry_block, op2);
	      retval = MIN (retval, mem);
	      goto new_memory_ret;
	    }
	  else
	    {
	      retval = mem_non_local;
	      goto new_memory_ret;
	    }
	}
      else
	{
	  if (gimple_code (stmt) == GIMPLE_PHI)
	    {
	      unsigned int i;
	      enum thread_memory_type mem;
	      tree phi_result = gimple_phi_result (stmt);

	      /* If any of the ancestors are non-local, we are sure to
		 be non-local.  Otherwise we can avoid doing anything
		 and inherit what has already been generated.  */
	      retval = mem_max;
	      for (i = 0; i < gimple_phi_num_args (stmt); ++i)
		{
		  tree op = PHI_ARG_DEF (stmt, i);

		  /* Exclude self-assignment.  */
		  if (phi_result == op)
		    continue;

		  mem = thread_private_new_memory (entry_block, op);
		  if (mem == mem_non_local)
		    {
		      retval = mem;
		      goto new_memory_ret;
		    }
		  retval = MIN (retval, mem);
		}
	      goto new_memory_ret;
	    }
	  break;
	}
    }
  while (TREE_CODE (x) == SSA_NAME);

  if (stmt && is_gimple_call (stmt) && gimple_call_flags (stmt) & ECF_MALLOC)
    /* Thread-local or transaction-local.  */
    ;
  else
    retval = mem_non_local;

 new_memory_ret:
  elt_p->local_new_memory = retval;
  return retval;
}

/* Determine whether X has to be instrumented using a read
   or write barrier.

   ENTRY_BLOCK is the entry block for the region where stmt resides
   in.  NULL if unknown.

   STMT is the statement in which X occurs in.  It is used for thread
   private memory instrumentation.  If no TPM instrumentation is
   desired, STMT should be null.  */
static bool
requires_barrier (basic_block entry_block, tree x, gimple stmt)
{
  tree orig = x;
  while (handled_component_p (x))
    x = TREE_OPERAND (x, 0);

  switch (TREE_CODE (x))
    {
    case INDIRECT_REF:
    case MEM_REF:
      {
	enum thread_memory_type ret;

	ret = thread_private_new_memory (entry_block, TREE_OPERAND (x, 0));
	if (ret == mem_non_local)
	  return true;
	if (stmt && ret == mem_thread_local)
	  /* ?? Should we pass `orig', or the INDIRECT_REF X.  ?? */
	  tm_log_add (entry_block, orig, stmt);

	/* Transaction-locals require nothing at all.  For malloc, a
	   transaction restart frees the memory and we reallocate.
	   For alloca, the stack pointer gets reset by the retry and
	   we reallocate.  */
	return false;
      }

    case TARGET_MEM_REF:
      if (TREE_CODE (TMR_BASE (x)) != ADDR_EXPR)
	return true;
      x = TREE_OPERAND (TMR_BASE (x), 0);
      if (TREE_CODE (x) == PARM_DECL)
	return false;
      gcc_assert (TREE_CODE (x) == VAR_DECL);
      /* FALLTHRU */

    case PARM_DECL:
    case RESULT_DECL:
    case VAR_DECL:
      if (DECL_BY_REFERENCE (x))
	{
	  /* ??? This value is a pointer, but aggregate_value_p has been
	     jigged to return true which confuses needs_to_live_in_memory.
	     This ought to be cleaned up generically.

	     FIXME: Verify this still happens after the next mainline
	     merge.  Testcase ie g++.dg/tm/pr47554.C.
	  */
	  return false;
	}

      if (is_global_var (x))
	return !TREE_READONLY (x);
      if (/* FIXME: This condition should actually go below in the
	     tm_log_add() call, however is_call_clobbered() depends on
	     aliasing info which is not available during
	     gimplification.  Since requires_barrier() gets called
	     during lower_sequence_tm/gimplification, leave the call
	     to needs_to_live_in_memory until we eliminate
	     lower_sequence_tm altogether.  */
	  needs_to_live_in_memory (x))
	return true;
      else
	{
	  /* For local memory that doesn't escape (aka thread private
	     memory), we can either save the value at the beginning of
	     the transaction and restore on restart, or call a tm
	     function to dynamically save and restore on restart
	     (ITM_L*).  */
	  if (stmt)
	    tm_log_add (entry_block, orig, stmt);
	  return false;
	}

    default:
      return false;
    }
}

/* Mark the GIMPLE_ASSIGN statement as appropriate for being inside
   a transaction region.  */

static void
examine_assign_tm (unsigned *state, gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);

  if (requires_barrier (/*entry_block=*/NULL, gimple_assign_rhs1 (stmt), NULL))
    *state |= GTMA_HAVE_LOAD;
  if (requires_barrier (/*entry_block=*/NULL, gimple_assign_lhs (stmt), NULL))
    *state |= GTMA_HAVE_STORE;
}

/* Mark a GIMPLE_CALL as appropriate for being inside a transaction.  */

static void
examine_call_tm (unsigned *state, gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree fn;

  if (is_tm_pure_call (stmt))
    return;

  /* Check if this call is a transaction abort.  */
  fn = gimple_call_fndecl (stmt);
  if (is_tm_abort (fn))
    *state |= GTMA_HAVE_ABORT;

  /* Note that something may happen.  */
  *state |= GTMA_HAVE_LOAD | GTMA_HAVE_STORE;
}

/* Lower a GIMPLE_TRANSACTION statement.  */

static void
lower_transaction (gimple_stmt_iterator *gsi, struct walk_stmt_info *wi)
{
  gimple g, stmt = gsi_stmt (*gsi);
  unsigned int *outer_state = (unsigned int *) wi->info;
  unsigned int this_state = 0;
  struct walk_stmt_info this_wi;

  /* First, lower the body.  The scanning that we do inside gives
     us some idea of what we're dealing with.  */
  memset (&this_wi, 0, sizeof (this_wi));
  this_wi.info = (void *) &this_state;
  walk_gimple_seq_mod (gimple_transaction_body_ptr (stmt),
		       lower_sequence_tm, NULL, &this_wi);

  /* If there was absolutely nothing transaction related inside the
     transaction, we may elide it.  Likewise if this is a nested
     transaction and does not contain an abort.  */
  if (this_state == 0
      || (!(this_state & GTMA_HAVE_ABORT) && outer_state != NULL))
    {
      if (outer_state)
	*outer_state |= this_state;

      gsi_insert_seq_before (gsi, gimple_transaction_body (stmt),
			     GSI_SAME_STMT);
      gimple_transaction_set_body (stmt, NULL);

      gsi_remove (gsi, true);
      wi->removed_stmt = true;
      return;
    }

  /* Wrap the body of the transaction in a try-finally node so that
     the commit call is always properly called.  */
  g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TM_COMMIT), 0);
  if (flag_exceptions)
    {
      tree ptr;
      gimple_seq n_seq, e_seq;

      n_seq = gimple_seq_alloc_with_stmt (g);
      e_seq = NULL;

      g = gimple_build_call (builtin_decl_explicit (BUILT_IN_EH_POINTER),
			     1, integer_zero_node);
      ptr = create_tmp_var (ptr_type_node, NULL);
      gimple_call_set_lhs (g, ptr);
      gimple_seq_add_stmt (&e_seq, g);

      g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TM_COMMIT_EH),
			     1, ptr);
      gimple_seq_add_stmt (&e_seq, g);

      g = gimple_build_eh_else (n_seq, e_seq);
    }

  g = gimple_build_try (gimple_transaction_body (stmt),
			gimple_seq_alloc_with_stmt (g), GIMPLE_TRY_FINALLY);
  gsi_insert_after (gsi, g, GSI_CONTINUE_LINKING);

  gimple_transaction_set_body (stmt, NULL);

  /* If the transaction calls abort or if this is an outer transaction,
     add an "over" label afterwards.  */
  if ((this_state & (GTMA_HAVE_ABORT))
      || (gimple_transaction_subcode (stmt) & GTMA_IS_OUTER))
    {
      tree label = create_artificial_label (UNKNOWN_LOCATION);
      gimple_transaction_set_label (stmt, label);
      gsi_insert_after (gsi, gimple_build_label (label), GSI_CONTINUE_LINKING);
    }

  /* Record the set of operations found for use later.  */
  this_state |= gimple_transaction_subcode (stmt) & GTMA_DECLARATION_MASK;
  gimple_transaction_set_subcode (stmt, this_state);
}

/* Iterate through the statements in the sequence, lowering them all
   as appropriate for being in a transaction.  */

static tree
lower_sequence_tm (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		   struct walk_stmt_info *wi)
{
  unsigned int *state = (unsigned int *) wi->info;
  gimple stmt = gsi_stmt (*gsi);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      /* Only memory reads/writes need to be instrumented.  */
      if (gimple_assign_single_p (stmt))
	examine_assign_tm (state, gsi);
      break;

    case GIMPLE_CALL:
      examine_call_tm (state, gsi);
      break;

    case GIMPLE_ASM:
      *state |= GTMA_MAY_ENTER_IRREVOCABLE;
      break;

    case GIMPLE_TRANSACTION:
      lower_transaction (gsi, wi);
      break;

    default:
      *handled_ops_p = !gimple_has_substatements (stmt);
      break;
    }

  return NULL_TREE;
}

/* Iterate through the statements in the sequence, lowering them all
   as appropriate for being outside of a transaction.  */

static tree
lower_sequence_no_tm (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		      struct walk_stmt_info * wi)
{
  gimple stmt = gsi_stmt (*gsi);

  if (gimple_code (stmt) == GIMPLE_TRANSACTION)
    {
      *handled_ops_p = true;
      lower_transaction (gsi, wi);
    }
  else
    *handled_ops_p = !gimple_has_substatements (stmt);

  return NULL_TREE;
}

/* Main entry point for flattening GIMPLE_TRANSACTION constructs.  After
   this, GIMPLE_TRANSACTION nodes still exist, but the nested body has
   been moved out, and all the data required for constructing a proper
   CFG has been recorded.  */

static unsigned int
execute_lower_tm (void)
{
  struct walk_stmt_info wi;
  gimple_seq body;

  /* Transactional clones aren't created until a later pass.  */
  gcc_assert (!decl_is_tm_clone (current_function_decl));

  body = gimple_body (current_function_decl);
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq_mod (&body, lower_sequence_no_tm, NULL, &wi);
  gimple_set_body (current_function_decl, body);

  return 0;
}

namespace {

const pass_data pass_data_lower_tm =
{
  GIMPLE_PASS, /* type */
  "tmlower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  PROP_gimple_lcf, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_tm : public gimple_opt_pass
{
public:
  pass_lower_tm (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_tm, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tm; }
  virtual unsigned int execute (function *) { return execute_lower_tm (); }

}; // class pass_lower_tm

} // anon namespace

gimple_opt_pass *
make_pass_lower_tm (gcc::context *ctxt)
{
  return new pass_lower_tm (ctxt);
}

/* Collect region information for each transaction.  */

struct tm_region
{
  /* Link to the next unnested transaction.  */
  struct tm_region *next;

  /* Link to the next inner transaction.  */
  struct tm_region *inner;

  /* Link to the next outer transaction.  */
  struct tm_region *outer;

  /* The GIMPLE_TRANSACTION statement beginning this transaction.
     After TM_MARK, this gets replaced by a call to
     BUILT_IN_TM_START.  */
  gimple transaction_stmt;

  /* After TM_MARK expands the GIMPLE_TRANSACTION into a call to
     BUILT_IN_TM_START, this field is true if the transaction is an
     outer transaction.  */
  bool original_transaction_was_outer;

  /* Return value from BUILT_IN_TM_START.  */
  tree tm_state;

  /* The entry block to this region.  This will always be the first
     block of the body of the transaction.  */
  basic_block entry_block;

  /* The first block after an expanded call to _ITM_beginTransaction.  */
  basic_block restart_block;

  /* The set of all blocks that end the region; NULL if only EXIT_BLOCK.
     These blocks are still a part of the region (i.e., the border is
     inclusive). Note that this set is only complete for paths in the CFG
     starting at ENTRY_BLOCK, and that there is no exit block recorded for
     the edge to the "over" label.  */
  bitmap exit_blocks;

  /* The set of all blocks that have an TM_IRREVOCABLE call.  */
  bitmap irr_blocks;
};

typedef struct tm_region *tm_region_p;

/* True if there are pending edge statements to be committed for the
   current function being scanned in the tmmark pass.  */
bool pending_edge_inserts_p;

static struct tm_region *all_tm_regions;
static bitmap_obstack tm_obstack;


/* A subroutine of tm_region_init.  Record the existence of the
   GIMPLE_TRANSACTION statement in a tree of tm_region elements.  */

static struct tm_region *
tm_region_init_0 (struct tm_region *outer, basic_block bb, gimple stmt)
{
  struct tm_region *region;

  region = (struct tm_region *)
    obstack_alloc (&tm_obstack.obstack, sizeof (struct tm_region));

  if (outer)
    {
      region->next = outer->inner;
      outer->inner = region;
    }
  else
    {
      region->next = all_tm_regions;
      all_tm_regions = region;
    }
  region->inner = NULL;
  region->outer = outer;

  region->transaction_stmt = stmt;
  region->original_transaction_was_outer = false;
  region->tm_state = NULL;

  /* There are either one or two edges out of the block containing
     the GIMPLE_TRANSACTION, one to the actual region and one to the
     "over" label if the region contains an abort.  The former will
     always be the one marked FALLTHRU.  */
  region->entry_block = FALLTHRU_EDGE (bb)->dest;

  region->exit_blocks = BITMAP_ALLOC (&tm_obstack);
  region->irr_blocks = BITMAP_ALLOC (&tm_obstack);

  return region;
}

/* A subroutine of tm_region_init.  Record all the exit and
   irrevocable blocks in BB into the region's exit_blocks and
   irr_blocks bitmaps.  Returns the new region being scanned.  */

static struct tm_region *
tm_region_init_1 (struct tm_region *region, basic_block bb)
{
  gimple_stmt_iterator gsi;
  gimple g;

  if (!region
      || (!region->irr_blocks && !region->exit_blocks))
    return region;

  /* Check to see if this is the end of a region by seeing if it
     contains a call to __builtin_tm_commit{,_eh}.  Note that the
     outermost region for DECL_IS_TM_CLONE need not collect this.  */
  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      g = gsi_stmt (gsi);
      if (gimple_code (g) == GIMPLE_CALL)
	{
	  tree fn = gimple_call_fndecl (g);
	  if (fn && DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL)
	    {
	      if ((DECL_FUNCTION_CODE (fn) == BUILT_IN_TM_COMMIT
		   || DECL_FUNCTION_CODE (fn) == BUILT_IN_TM_COMMIT_EH)
		  && region->exit_blocks)
		{
		  bitmap_set_bit (region->exit_blocks, bb->index);
		  region = region->outer;
		  break;
		}
	      if (DECL_FUNCTION_CODE (fn) == BUILT_IN_TM_IRREVOCABLE)
		bitmap_set_bit (region->irr_blocks, bb->index);
	    }
	}
    }
  return region;
}

/* Collect all of the transaction regions within the current function
   and record them in ALL_TM_REGIONS.  The REGION parameter may specify
   an "outermost" region for use by tm clones.  */

static void
tm_region_init (struct tm_region *region)
{
  gimple g;
  edge_iterator ei;
  edge e;
  basic_block bb;
  auto_vec<basic_block> queue;
  bitmap visited_blocks = BITMAP_ALLOC (NULL);
  struct tm_region *old_region;
  auto_vec<tm_region_p> bb_regions;

  all_tm_regions = region;
  bb = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* We could store this information in bb->aux, but we may get called
     through get_all_tm_blocks() from another pass that may be already
     using bb->aux.  */
  bb_regions.safe_grow_cleared (last_basic_block_for_fn (cfun));

  queue.safe_push (bb);
  bb_regions[bb->index] = region;
  do
    {
      bb = queue.pop ();
      region = bb_regions[bb->index];
      bb_regions[bb->index] = NULL;

      /* Record exit and irrevocable blocks.  */
      region = tm_region_init_1 (region, bb);

      /* Check for the last statement in the block beginning a new region.  */
      g = last_stmt (bb);
      old_region = region;
      if (g && gimple_code (g) == GIMPLE_TRANSACTION)
	region = tm_region_init_0 (region, bb, g);

      /* Process subsequent blocks.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!bitmap_bit_p (visited_blocks, e->dest->index))
	  {
	    bitmap_set_bit (visited_blocks, e->dest->index);
	    queue.safe_push (e->dest);

	    /* If the current block started a new region, make sure that only
	       the entry block of the new region is associated with this region.
	       Other successors are still part of the old region.  */
	    if (old_region != region && e->dest != region->entry_block)
	      bb_regions[e->dest->index] = old_region;
	    else
	      bb_regions[e->dest->index] = region;
	  }
    }
  while (!queue.is_empty ());
  BITMAP_FREE (visited_blocks);
}

/* The "gate" function for all transactional memory expansion and optimization
   passes.  We collect region information for each top-level transaction, and
   if we don't find any, we skip all of the TM passes.  Each region will have
   all of the exit blocks recorded, and the originating statement.  */

static bool
gate_tm_init (void)
{
  if (!flag_tm)
    return false;

  calculate_dominance_info (CDI_DOMINATORS);
  bitmap_obstack_initialize (&tm_obstack);

  /* If the function is a TM_CLONE, then the entire function is the region.  */
  if (decl_is_tm_clone (current_function_decl))
    {
      struct tm_region *region = (struct tm_region *)
	obstack_alloc (&tm_obstack.obstack, sizeof (struct tm_region));
      memset (region, 0, sizeof (*region));
      region->entry_block = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
      /* For a clone, the entire function is the region.  But even if
	 we don't need to record any exit blocks, we may need to
	 record irrevocable blocks.  */
      region->irr_blocks = BITMAP_ALLOC (&tm_obstack);

      tm_region_init (region);
    }
  else
    {
      tm_region_init (NULL);

      /* If we didn't find any regions, cleanup and skip the whole tree
	 of tm-related optimizations.  */
      if (all_tm_regions == NULL)
	{
	  bitmap_obstack_release (&tm_obstack);
	  return false;
	}
    }

  return true;
}

namespace {

const pass_data pass_data_tm_init =
{
  GIMPLE_PASS, /* type */
  "*tminit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tm_init : public gimple_opt_pass
{
public:
  pass_tm_init (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tm_init, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return gate_tm_init (); }

}; // class pass_tm_init

} // anon namespace

gimple_opt_pass *
make_pass_tm_init (gcc::context *ctxt)
{
  return new pass_tm_init (ctxt);
}

/* Add FLAGS to the GIMPLE_TRANSACTION subcode for the transaction region
   represented by STATE.  */

static inline void
transaction_subcode_ior (struct tm_region *region, unsigned flags)
{
  if (region && region->transaction_stmt)
    {
      flags |= gimple_transaction_subcode (region->transaction_stmt);
      gimple_transaction_set_subcode (region->transaction_stmt, flags);
    }
}

/* Construct a memory load in a transactional context.  Return the
   gimple statement performing the load, or NULL if there is no
   TM_LOAD builtin of the appropriate size to do the load.

   LOC is the location to use for the new statement(s).  */

static gimple
build_tm_load (location_t loc, tree lhs, tree rhs, gimple_stmt_iterator *gsi)
{
  enum built_in_function code = END_BUILTINS;
  tree t, type = TREE_TYPE (rhs), decl;
  gimple gcall;

  if (type == float_type_node)
    code = BUILT_IN_TM_LOAD_FLOAT;
  else if (type == double_type_node)
    code = BUILT_IN_TM_LOAD_DOUBLE;
  else if (type == long_double_type_node)
    code = BUILT_IN_TM_LOAD_LDOUBLE;
  else if (TYPE_SIZE_UNIT (type) != NULL
	   && tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
    {
      switch (tree_to_uhwi (TYPE_SIZE_UNIT (type)))
	{
	case 1:
	  code = BUILT_IN_TM_LOAD_1;
	  break;
	case 2:
	  code = BUILT_IN_TM_LOAD_2;
	  break;
	case 4:
	  code = BUILT_IN_TM_LOAD_4;
	  break;
	case 8:
	  code = BUILT_IN_TM_LOAD_8;
	  break;
	}
    }

  if (code == END_BUILTINS)
    {
      decl = targetm.vectorize.builtin_tm_load (type);
      if (!decl)
	return NULL;
    }
  else
    decl = builtin_decl_explicit (code);

  t = gimplify_addr (gsi, rhs);
  gcall = gimple_build_call (decl, 1, t);
  gimple_set_location (gcall, loc);

  t = TREE_TYPE (TREE_TYPE (decl));
  if (useless_type_conversion_p (type, t))
    {
      gimple_call_set_lhs (gcall, lhs);
      gsi_insert_before (gsi, gcall, GSI_SAME_STMT);
    }
  else
    {
      gimple g;
      tree temp;

      temp = create_tmp_reg (t, NULL);
      gimple_call_set_lhs (gcall, temp);
      gsi_insert_before (gsi, gcall, GSI_SAME_STMT);

      t = fold_build1 (VIEW_CONVERT_EXPR, type, temp);
      g = gimple_build_assign (lhs, t);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  return gcall;
}


/* Similarly for storing TYPE in a transactional context.  */

static gimple
build_tm_store (location_t loc, tree lhs, tree rhs, gimple_stmt_iterator *gsi)
{
  enum built_in_function code = END_BUILTINS;
  tree t, fn, type = TREE_TYPE (rhs), simple_type;
  gimple gcall;

  if (type == float_type_node)
    code = BUILT_IN_TM_STORE_FLOAT;
  else if (type == double_type_node)
    code = BUILT_IN_TM_STORE_DOUBLE;
  else if (type == long_double_type_node)
    code = BUILT_IN_TM_STORE_LDOUBLE;
  else if (TYPE_SIZE_UNIT (type) != NULL
	   && tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
    {
      switch (tree_to_uhwi (TYPE_SIZE_UNIT (type)))
	{
	case 1:
	  code = BUILT_IN_TM_STORE_1;
	  break;
	case 2:
	  code = BUILT_IN_TM_STORE_2;
	  break;
	case 4:
	  code = BUILT_IN_TM_STORE_4;
	  break;
	case 8:
	  code = BUILT_IN_TM_STORE_8;
	  break;
	}
    }

  if (code == END_BUILTINS)
    {
      fn = targetm.vectorize.builtin_tm_store (type);
      if (!fn)
	return NULL;
    }
  else
    fn = builtin_decl_explicit (code);

  simple_type = TREE_VALUE (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (fn))));

  if (TREE_CODE (rhs) == CONSTRUCTOR)
    {
      /* Handle the easy initialization to zero.  */
      if (!CONSTRUCTOR_ELTS (rhs))
	rhs = build_int_cst (simple_type, 0);
      else
	{
	  /* ...otherwise punt to the caller and probably use
	    BUILT_IN_TM_MEMMOVE, because we can't wrap a
	    VIEW_CONVERT_EXPR around a CONSTRUCTOR (below) and produce
	    valid gimple.  */
	  return NULL;
	}
    }
  else if (!useless_type_conversion_p (simple_type, type))
    {
      gimple g;
      tree temp;

      temp = create_tmp_reg (simple_type, NULL);
      t = fold_build1 (VIEW_CONVERT_EXPR, simple_type, rhs);
      g = gimple_build_assign (temp, t);
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);

      rhs = temp;
    }

  t = gimplify_addr (gsi, lhs);
  gcall = gimple_build_call (fn, 2, t, rhs);
  gimple_set_location (gcall, loc);
  gsi_insert_before (gsi, gcall, GSI_SAME_STMT);

  return gcall;
}


/* Expand an assignment statement into transactional builtins.  */

static void
expand_assign_tm (struct tm_region *region, gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);
  bool store_p = requires_barrier (region->entry_block, lhs, NULL);
  bool load_p = requires_barrier (region->entry_block, rhs, NULL);
  gimple gcall = NULL;

  if (!load_p && !store_p)
    {
      /* Add thread private addresses to log if applicable.  */
      requires_barrier (region->entry_block, lhs, stmt);
      gsi_next (gsi);
      return;
    }

  // Remove original load/store statement.
  gsi_remove (gsi, true);

  if (load_p && !store_p)
    {
      transaction_subcode_ior (region, GTMA_HAVE_LOAD);
      gcall = build_tm_load (loc, lhs, rhs, gsi);
    }
  else if (store_p && !load_p)
    {
      transaction_subcode_ior (region, GTMA_HAVE_STORE);
      gcall = build_tm_store (loc, lhs, rhs, gsi);
    }
  if (!gcall)
    {
      tree lhs_addr, rhs_addr, tmp;

      if (load_p)
	transaction_subcode_ior (region, GTMA_HAVE_LOAD);
      if (store_p)
	transaction_subcode_ior (region, GTMA_HAVE_STORE);

      /* ??? Figure out if there's any possible overlap between the LHS
	 and the RHS and if not, use MEMCPY.  */

      if (load_p && is_gimple_reg (lhs))
	{
	  tmp = create_tmp_var (TREE_TYPE (lhs), NULL);
	  lhs_addr = build_fold_addr_expr (tmp);
	}
      else
	{
	  tmp = NULL_TREE;
	  lhs_addr = gimplify_addr (gsi, lhs);
	}
      rhs_addr = gimplify_addr (gsi, rhs);
      gcall = gimple_build_call (builtin_decl_explicit (BUILT_IN_TM_MEMMOVE),
				 3, lhs_addr, rhs_addr,
				 TYPE_SIZE_UNIT (TREE_TYPE (lhs)));
      gimple_set_location (gcall, loc);
      gsi_insert_before (gsi, gcall, GSI_SAME_STMT);

      if (tmp)
	{
	  gcall = gimple_build_assign (lhs, tmp);
	  gsi_insert_before (gsi, gcall, GSI_SAME_STMT);
	}
    }

  /* Now that we have the load/store in its instrumented form, add
     thread private addresses to the log if applicable.  */
  if (!store_p)
    requires_barrier (region->entry_block, lhs, gcall);

  // The calls to build_tm_{store,load} above inserted the instrumented
  // call into the stream.
  // gsi_insert_before (gsi, gcall, GSI_SAME_STMT);
}


/* Expand a call statement as appropriate for a transaction.  That is,
   either verify that the call does not affect the transaction, or
   redirect the call to a clone that handles transactions, or change
   the transaction state to IRREVOCABLE.  Return true if the call is
   one of the builtins that end a transaction.  */

static bool
expand_call_tm (struct tm_region *region,
		gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);
  tree fn_decl;
  struct cgraph_node *node;
  bool retval = false;

  fn_decl = gimple_call_fndecl (stmt);

  if (fn_decl == builtin_decl_explicit (BUILT_IN_TM_MEMCPY)
      || fn_decl == builtin_decl_explicit (BUILT_IN_TM_MEMMOVE))
    transaction_subcode_ior (region, GTMA_HAVE_STORE | GTMA_HAVE_LOAD);
  if (fn_decl == builtin_decl_explicit (BUILT_IN_TM_MEMSET))
    transaction_subcode_ior (region, GTMA_HAVE_STORE);

  if (is_tm_pure_call (stmt))
    return false;

  if (fn_decl)
    retval = is_tm_ending_fndecl (fn_decl);
  if (!retval)
    {
      /* Assume all non-const/pure calls write to memory, except
	 transaction ending builtins.  */
      transaction_subcode_ior (region, GTMA_HAVE_STORE);
    }

  /* For indirect calls, we already generated a call into the runtime.  */
  if (!fn_decl)
    {
      tree fn = gimple_call_fn (stmt);

      /* We are guaranteed never to go irrevocable on a safe or pure
	 call, and the pure call was handled above.  */
      if (is_tm_safe (fn))
	return false;
      else
	transaction_subcode_ior (region, GTMA_MAY_ENTER_IRREVOCABLE);

      return false;
    }

  node = cgraph_get_node (fn_decl);
  /* All calls should have cgraph here.  */
  if (!node)
    {
      /* We can have a nodeless call here if some pass after IPA-tm
	 added uninstrumented calls.  For example, loop distribution
	 can transform certain loop constructs into __builtin_mem*
	 calls.  In this case, see if we have a suitable TM
	 replacement and fill in the gaps.  */
      gcc_assert (DECL_BUILT_IN_CLASS (fn_decl) == BUILT_IN_NORMAL);
      enum built_in_function code = DECL_FUNCTION_CODE (fn_decl);
      gcc_assert (code == BUILT_IN_MEMCPY
		  || code == BUILT_IN_MEMMOVE
		  || code == BUILT_IN_MEMSET);

      tree repl = find_tm_replacement_function (fn_decl);
      if (repl)
	{
	  gimple_call_set_fndecl (stmt, repl);
	  update_stmt (stmt);
	  node = cgraph_create_node (repl);
	  node->local.tm_may_enter_irr = false;
	  return expand_call_tm (region, gsi);
	}
      gcc_unreachable ();
    }
  if (node->local.tm_may_enter_irr)
    transaction_subcode_ior (region, GTMA_MAY_ENTER_IRREVOCABLE);

  if (is_tm_abort (fn_decl))
    {
      transaction_subcode_ior (region, GTMA_HAVE_ABORT);
      return true;
    }

  /* Instrument the store if needed.

     If the assignment happens inside the function call (return slot
     optimization), there is no instrumentation to be done, since
     the callee should have done the right thing.  */
  if (lhs && requires_barrier (region->entry_block, lhs, stmt)
      && !gimple_call_return_slot_opt_p (stmt))
    {
      tree tmp = create_tmp_reg (TREE_TYPE (lhs), NULL);
      location_t loc = gimple_location (stmt);
      edge fallthru_edge = NULL;

      /* Remember if the call was going to throw.  */
      if (stmt_can_throw_internal (stmt))
	{
	  edge_iterator ei;
	  edge e;
	  basic_block bb = gimple_bb (stmt);

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_FALLTHRU)
	      {
		fallthru_edge = e;
		break;
	      }
	}

      gimple_call_set_lhs (stmt, tmp);
      update_stmt (stmt);
      stmt = gimple_build_assign (lhs, tmp);
      gimple_set_location (stmt, loc);

      /* We cannot throw in the middle of a BB.  If the call was going
	 to throw, place the instrumentation on the fallthru edge, so
	 the call remains the last statement in the block.  */
      if (fallthru_edge)
	{
	  gimple_seq fallthru_seq = gimple_seq_alloc_with_stmt (stmt);
	  gimple_stmt_iterator fallthru_gsi = gsi_start (fallthru_seq);
	  expand_assign_tm (region, &fallthru_gsi);
	  gsi_insert_seq_on_edge (fallthru_edge, fallthru_seq);
	  pending_edge_inserts_p = true;
	}
      else
	{
	  gsi_insert_after (gsi, stmt, GSI_CONTINUE_LINKING);
	  expand_assign_tm (region, gsi);
	}

      transaction_subcode_ior (region, GTMA_HAVE_STORE);
    }

  return retval;
}


/* Expand all statements in BB as appropriate for being inside
   a transaction.  */

static void
expand_block_tm (struct tm_region *region, basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
    {
      gimple stmt = gsi_stmt (gsi);
      switch (gimple_code (stmt))
	{
	case GIMPLE_ASSIGN:
	  /* Only memory reads/writes need to be instrumented.  */
	  if (gimple_assign_single_p (stmt)
	      && !gimple_clobber_p (stmt))
	    {
	      expand_assign_tm (region, &gsi);
	      continue;
	    }
	  break;

	case GIMPLE_CALL:
	  if (expand_call_tm (region, &gsi))
	    return;
	  break;

	case GIMPLE_ASM:
	  gcc_unreachable ();

	default:
	  break;
	}
      if (!gsi_end_p (gsi))
	gsi_next (&gsi);
    }
}

/* Return the list of basic-blocks in REGION.

   STOP_AT_IRREVOCABLE_P is true if caller is uninterested in blocks
   following a TM_IRREVOCABLE call.

   INCLUDE_UNINSTRUMENTED_P is TRUE if we should include the
   uninstrumented code path blocks in the list of basic blocks
   returned, false otherwise.  */

static vec<basic_block> 
get_tm_region_blocks (basic_block entry_block,
		      bitmap exit_blocks,
		      bitmap irr_blocks,
		      bitmap all_region_blocks,
		      bool stop_at_irrevocable_p,
		      bool include_uninstrumented_p = true)
{
  vec<basic_block> bbs = vNULL;
  unsigned i;
  edge e;
  edge_iterator ei;
  bitmap visited_blocks = BITMAP_ALLOC (NULL);

  i = 0;
  bbs.safe_push (entry_block);
  bitmap_set_bit (visited_blocks, entry_block->index);

  do
    {
      basic_block bb = bbs[i++];

      if (exit_blocks &&
	  bitmap_bit_p (exit_blocks, bb->index))
	continue;

      if (stop_at_irrevocable_p
	  && irr_blocks
	  && bitmap_bit_p (irr_blocks, bb->index))
	continue;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if ((include_uninstrumented_p
	     || !(e->flags & EDGE_TM_UNINSTRUMENTED))
	    && !bitmap_bit_p (visited_blocks, e->dest->index))
	  {
	    bitmap_set_bit (visited_blocks, e->dest->index);
	    bbs.safe_push (e->dest);
	  }
    }
  while (i < bbs.length ());

  if (all_region_blocks)
    bitmap_ior_into (all_region_blocks, visited_blocks);

  BITMAP_FREE (visited_blocks);
  return bbs;
}

// Callback data for collect_bb2reg.
struct bb2reg_stuff
{
  vec<tm_region_p> *bb2reg;
  bool include_uninstrumented_p;
};

// Callback for expand_regions, collect innermost region data for each bb.
static void *
collect_bb2reg (struct tm_region *region, void *data)
{
  struct bb2reg_stuff *stuff = (struct bb2reg_stuff *)data;
  vec<tm_region_p> *bb2reg = stuff->bb2reg;
  vec<basic_block> queue;
  unsigned int i;
  basic_block bb;

  queue = get_tm_region_blocks (region->entry_block,
				region->exit_blocks,
				region->irr_blocks,
				NULL,
				/*stop_at_irr_p=*/true,
				stuff->include_uninstrumented_p);

  // We expect expand_region to perform a post-order traversal of the region
  // tree.  Therefore the last region seen for any bb is the innermost.
  FOR_EACH_VEC_ELT (queue, i, bb)
    (*bb2reg)[bb->index] = region;

  queue.release ();
  return NULL;
}

// Returns a vector, indexed by BB->INDEX, of the innermost tm_region to
// which a basic block belongs.  Note that we only consider the instrumented
// code paths for the region; the uninstrumented code paths are ignored if
// INCLUDE_UNINSTRUMENTED_P is false.
//
// ??? This data is very similar to the bb_regions array that is collected
// during tm_region_init.  Or, rather, this data is similar to what could
// be used within tm_region_init.  The actual computation in tm_region_init
// begins and ends with bb_regions entirely full of NULL pointers, due to
// the way in which pointers are swapped in and out of the array.
//
// ??? Our callers expect that blocks are not shared between transactions.
// When the optimizers get too smart, and blocks are shared, then during
// the tm_mark phase we'll add log entries to only one of the two transactions,
// and in the tm_edge phase we'll add edges to the CFG that create invalid
// cycles.  The symptom being SSA defs that do not dominate their uses.
// Note that the optimizers were locally correct with their transformation,
// as we have no info within the program that suggests that the blocks cannot
// be shared.
//
// ??? There is currently a hack inside tree-ssa-pre.c to work around the
// only known instance of this block sharing.

static vec<tm_region_p>
get_bb_regions_instrumented (bool traverse_clones,
			     bool include_uninstrumented_p)
{
  unsigned n = last_basic_block_for_fn (cfun);
  struct bb2reg_stuff stuff;
  vec<tm_region_p> ret;

  ret.create (n);
  ret.safe_grow_cleared (n);
  stuff.bb2reg = &ret;
  stuff.include_uninstrumented_p = include_uninstrumented_p;
  expand_regions (all_tm_regions, collect_bb2reg, &stuff, traverse_clones);

  return ret;
}

/* Set the IN_TRANSACTION for all gimple statements that appear in a
   transaction.  */

void
compute_transaction_bits (void)
{
  struct tm_region *region;
  vec<basic_block> queue;
  unsigned int i;
  basic_block bb;

  /* ?? Perhaps we need to abstract gate_tm_init further, because we
     certainly don't need it to calculate CDI_DOMINATOR info.  */
  gate_tm_init ();

  FOR_EACH_BB_FN (bb, cfun)
    bb->flags &= ~BB_IN_TRANSACTION;

  for (region = all_tm_regions; region; region = region->next)
    {
      queue = get_tm_region_blocks (region->entry_block,
				    region->exit_blocks,
				    region->irr_blocks,
				    NULL,
				    /*stop_at_irr_p=*/true);
      for (i = 0; queue.iterate (i, &bb); ++i)
	bb->flags |= BB_IN_TRANSACTION;
      queue.release ();
    }

  if (all_tm_regions)
    bitmap_obstack_release (&tm_obstack);
}

/* Replace the GIMPLE_TRANSACTION in this region with the corresponding
   call to BUILT_IN_TM_START.  */

static void *
expand_transaction (struct tm_region *region, void *data ATTRIBUTE_UNUSED)
{
  tree tm_start = builtin_decl_explicit (BUILT_IN_TM_START);
  basic_block transaction_bb = gimple_bb (region->transaction_stmt);
  tree tm_state = region->tm_state;
  tree tm_state_type = TREE_TYPE (tm_state);
  edge abort_edge = NULL;
  edge inst_edge = NULL;
  edge uninst_edge = NULL;
  edge fallthru_edge = NULL;

  // Identify the various successors of the transaction start.
  {
    edge_iterator i;
    edge e;
    FOR_EACH_EDGE (e, i, transaction_bb->succs)
      {
        if (e->flags & EDGE_TM_ABORT)
	  abort_edge = e;
        else if (e->flags & EDGE_TM_UNINSTRUMENTED)
	  uninst_edge = e;
	else
	  inst_edge = e;
        if (e->flags & EDGE_FALLTHRU)
	  fallthru_edge = e;
      }
  }

  /* ??? There are plenty of bits here we're not computing.  */
  {
    int subcode = gimple_transaction_subcode (region->transaction_stmt);
    int flags = 0;
    if (subcode & GTMA_DOES_GO_IRREVOCABLE)
      flags |= PR_DOESGOIRREVOCABLE;
    if ((subcode & GTMA_MAY_ENTER_IRREVOCABLE) == 0)
      flags |= PR_HASNOIRREVOCABLE;
    /* If the transaction does not have an abort in lexical scope and is not
       marked as an outer transaction, then it will never abort.  */
    if ((subcode & GTMA_HAVE_ABORT) == 0 && (subcode & GTMA_IS_OUTER) == 0)
      flags |= PR_HASNOABORT;
    if ((subcode & GTMA_HAVE_STORE) == 0)
      flags |= PR_READONLY;
    if (inst_edge && !(subcode & GTMA_HAS_NO_INSTRUMENTATION))
      flags |= PR_INSTRUMENTEDCODE;
    if (uninst_edge)
      flags |= PR_UNINSTRUMENTEDCODE;
    if (subcode & GTMA_IS_OUTER)
      region->original_transaction_was_outer = true;
    tree t = build_int_cst (tm_state_type, flags);
    gimple call = gimple_build_call (tm_start, 1, t);
    gimple_call_set_lhs (call, tm_state);
    gimple_set_location (call, gimple_location (region->transaction_stmt));

    // Replace the GIMPLE_TRANSACTION with the call to BUILT_IN_TM_START.
    gimple_stmt_iterator gsi = gsi_last_bb (transaction_bb);
    gcc_assert (gsi_stmt (gsi) == region->transaction_stmt);
    gsi_insert_before (&gsi, call, GSI_SAME_STMT);
    gsi_remove (&gsi, true);
    region->transaction_stmt = call;
  }

  // Generate log saves.
  if (!tm_log_save_addresses.is_empty ())
    tm_log_emit_saves (region->entry_block, transaction_bb);

  // In the beginning, we've no tests to perform on transaction restart.
  // Note that after this point, transaction_bb becomes the "most recent
  // block containing tests for the transaction".
  region->restart_block = region->entry_block;

  // Generate log restores.
  if (!tm_log_save_addresses.is_empty ())
    {
      basic_block test_bb = create_empty_bb (transaction_bb);
      basic_block code_bb = create_empty_bb (test_bb);
      basic_block join_bb = create_empty_bb (code_bb);
      if (current_loops && transaction_bb->loop_father)
	{
	  add_bb_to_loop (test_bb, transaction_bb->loop_father);
	  add_bb_to_loop (code_bb, transaction_bb->loop_father);
	  add_bb_to_loop (join_bb, transaction_bb->loop_father);
	}
      if (region->restart_block == region->entry_block)
	region->restart_block = test_bb;

      tree t1 = create_tmp_reg (tm_state_type, NULL);
      tree t2 = build_int_cst (tm_state_type, A_RESTORELIVEVARIABLES);
      gimple stmt = gimple_build_assign_with_ops (BIT_AND_EXPR, t1,
						  tm_state, t2);
      gimple_stmt_iterator gsi = gsi_last_bb (test_bb);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      t2 = build_int_cst (tm_state_type, 0);
      stmt = gimple_build_cond (NE_EXPR, t1, t2, NULL, NULL);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      tm_log_emit_restores (region->entry_block, code_bb);

      edge ei = make_edge (transaction_bb, test_bb, EDGE_FALLTHRU);
      edge et = make_edge (test_bb, code_bb, EDGE_TRUE_VALUE);
      edge ef = make_edge (test_bb, join_bb, EDGE_FALSE_VALUE);
      redirect_edge_pred (fallthru_edge, join_bb);

      join_bb->frequency = test_bb->frequency = transaction_bb->frequency;
      join_bb->count = test_bb->count = transaction_bb->count;

      ei->probability = PROB_ALWAYS;
      et->probability = PROB_LIKELY;
      ef->probability = PROB_UNLIKELY;
      et->count = apply_probability (test_bb->count, et->probability);
      ef->count = apply_probability (test_bb->count, ef->probability);

      code_bb->count = et->count;
      code_bb->frequency = EDGE_FREQUENCY (et);

      transaction_bb = join_bb;
    }

  // If we have an ABORT edge, create a test to perform the abort.
  if (abort_edge)
    {
      basic_block test_bb = create_empty_bb (transaction_bb);
      if (current_loops && transaction_bb->loop_father)
	add_bb_to_loop (test_bb, transaction_bb->loop_father);
      if (region->restart_block == region->entry_block)
	region->restart_block = test_bb;

      tree t1 = create_tmp_reg (tm_state_type, NULL);
      tree t2 = build_int_cst (tm_state_type, A_ABORTTRANSACTION);
      gimple stmt = gimple_build_assign_with_ops (BIT_AND_EXPR, t1,
						  tm_state, t2);
      gimple_stmt_iterator gsi = gsi_last_bb (test_bb);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      t2 = build_int_cst (tm_state_type, 0);
      stmt = gimple_build_cond (NE_EXPR, t1, t2, NULL, NULL);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      edge ei = make_edge (transaction_bb, test_bb, EDGE_FALLTHRU);
      test_bb->frequency = transaction_bb->frequency;
      test_bb->count = transaction_bb->count;
      ei->probability = PROB_ALWAYS;

      // Not abort edge.  If both are live, chose one at random as we'll
      // we'll be fixing that up below.
      redirect_edge_pred (fallthru_edge, test_bb);
      fallthru_edge->flags = EDGE_FALSE_VALUE;
      fallthru_edge->probability = PROB_VERY_LIKELY;
      fallthru_edge->count
	= apply_probability (test_bb->count, fallthru_edge->probability);

      // Abort/over edge.
      redirect_edge_pred (abort_edge, test_bb);
      abort_edge->flags = EDGE_TRUE_VALUE;
      abort_edge->probability = PROB_VERY_UNLIKELY;
      abort_edge->count
	= apply_probability (test_bb->count, abort_edge->probability);

      transaction_bb = test_bb;
    }

  // If we have both instrumented and uninstrumented code paths, select one.
  if (inst_edge && uninst_edge)
    {
      basic_block test_bb = create_empty_bb (transaction_bb);
      if (current_loops && transaction_bb->loop_father)
	add_bb_to_loop (test_bb, transaction_bb->loop_father);
      if (region->restart_block == region->entry_block)
	region->restart_block = test_bb;

      tree t1 = create_tmp_reg (tm_state_type, NULL);
      tree t2 = build_int_cst (tm_state_type, A_RUNUNINSTRUMENTEDCODE);

      gimple stmt = gimple_build_assign_with_ops (BIT_AND_EXPR, t1,
						  tm_state, t2);
      gimple_stmt_iterator gsi = gsi_last_bb (test_bb);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      t2 = build_int_cst (tm_state_type, 0);
      stmt = gimple_build_cond (NE_EXPR, t1, t2, NULL, NULL);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

      // Create the edge into test_bb first, as we want to copy values
      // out of the fallthru edge.
      edge e = make_edge (transaction_bb, test_bb, fallthru_edge->flags);
      e->probability = fallthru_edge->probability;
      test_bb->count = e->count = fallthru_edge->count;
      test_bb->frequency = EDGE_FREQUENCY (e);

      // Now update the edges to the inst/uninist implementations.
      // For now assume that the paths are equally likely.  When using HTM,
      // we'll try the uninst path first and fallback to inst path if htm
      // buffers are exceeded.  Without HTM we start with the inst path and
      // use the uninst path when falling back to serial mode.
      redirect_edge_pred (inst_edge, test_bb);
      inst_edge->flags = EDGE_FALSE_VALUE;
      inst_edge->probability = REG_BR_PROB_BASE / 2;
      inst_edge->count
	= apply_probability (test_bb->count, inst_edge->probability);

      redirect_edge_pred (uninst_edge, test_bb);
      uninst_edge->flags = EDGE_TRUE_VALUE;
      uninst_edge->probability = REG_BR_PROB_BASE / 2;
      uninst_edge->count
	= apply_probability (test_bb->count, uninst_edge->probability);
    }

  // If we have no previous special cases, and we have PHIs at the beginning
  // of the atomic region, this means we have a loop at the beginning of the
  // atomic region that shares the first block.  This can cause problems with
  // the transaction restart abnormal edges to be added in the tm_edges pass.
  // Solve this by adding a new empty block to receive the abnormal edges.
  if (region->restart_block == region->entry_block
      && phi_nodes (region->entry_block))
    {
      basic_block empty_bb = create_empty_bb (transaction_bb);
      region->restart_block = empty_bb;
      if (current_loops && transaction_bb->loop_father)
	add_bb_to_loop (empty_bb, transaction_bb->loop_father);

      redirect_edge_pred (fallthru_edge, empty_bb);
      make_edge (transaction_bb, empty_bb, EDGE_FALLTHRU);
    }

  return NULL;
}

/* Generate the temporary to be used for the return value of
   BUILT_IN_TM_START.  */

static void *
generate_tm_state (struct tm_region *region, void *data ATTRIBUTE_UNUSED)
{
  tree tm_start = builtin_decl_explicit (BUILT_IN_TM_START);
  region->tm_state =
    create_tmp_reg (TREE_TYPE (TREE_TYPE (tm_start)), "tm_state");

  // Reset the subcode, post optimizations.  We'll fill this in
  // again as we process blocks.
  if (region->exit_blocks)
    {
      unsigned int subcode
	= gimple_transaction_subcode (region->transaction_stmt);

      if (subcode & GTMA_DOES_GO_IRREVOCABLE)
	subcode &= (GTMA_DECLARATION_MASK | GTMA_DOES_GO_IRREVOCABLE
		    | GTMA_MAY_ENTER_IRREVOCABLE
		    | GTMA_HAS_NO_INSTRUMENTATION);
      else
	subcode &= GTMA_DECLARATION_MASK;
      gimple_transaction_set_subcode (region->transaction_stmt, subcode);
    }

  return NULL;
}

// Propagate flags from inner transactions outwards.
static void
propagate_tm_flags_out (struct tm_region *region)
{
  if (region == NULL)
    return;
  propagate_tm_flags_out (region->inner);

  if (region->outer && region->outer->transaction_stmt)
    {
      unsigned s = gimple_transaction_subcode (region->transaction_stmt);
      s &= (GTMA_HAVE_ABORT | GTMA_HAVE_LOAD | GTMA_HAVE_STORE
            | GTMA_MAY_ENTER_IRREVOCABLE);
      s |= gimple_transaction_subcode (region->outer->transaction_stmt);
      gimple_transaction_set_subcode (region->outer->transaction_stmt, s);
    }

  propagate_tm_flags_out (region->next);
}

/* Entry point to the MARK phase of TM expansion.  Here we replace
   transactional memory statements with calls to builtins, and function
   calls with their transactional clones (if available).  But we don't
   yet lower GIMPLE_TRANSACTION or add the transaction restart back-edges.  */

static unsigned int
execute_tm_mark (void)
{
  pending_edge_inserts_p = false;

  expand_regions (all_tm_regions, generate_tm_state, NULL,
		  /*traverse_clones=*/true);

  tm_log_init ();

  vec<tm_region_p> bb_regions
    = get_bb_regions_instrumented (/*traverse_clones=*/true,
				   /*include_uninstrumented_p=*/false);
  struct tm_region *r;
  unsigned i;

  // Expand memory operations into calls into the runtime.
  // This collects log entries as well.
  FOR_EACH_VEC_ELT (bb_regions, i, r)
    {
      if (r != NULL)
	{
	  if (r->transaction_stmt)
	    {
	      unsigned sub = gimple_transaction_subcode (r->transaction_stmt);

	      /* If we're sure to go irrevocable, there won't be
		 anything to expand, since the run-time will go
		 irrevocable right away.  */
	      if (sub & GTMA_DOES_GO_IRREVOCABLE
		  && sub & GTMA_MAY_ENTER_IRREVOCABLE)
		continue;
	    }
	  expand_block_tm (r, BASIC_BLOCK_FOR_FN (cfun, i));
	}
    }

  bb_regions.release ();

  // Propagate flags from inner transactions outwards.
  propagate_tm_flags_out (all_tm_regions);

  // Expand GIMPLE_TRANSACTIONs into calls into the runtime.
  expand_regions (all_tm_regions, expand_transaction, NULL,
		  /*traverse_clones=*/false);

  tm_log_emit ();
  tm_log_delete ();

  if (pending_edge_inserts_p)
    gsi_commit_edge_inserts ();
  free_dominance_info (CDI_DOMINATORS);
  return 0;
}

namespace {

const pass_data pass_data_tm_mark =
{
  GIMPLE_PASS, /* type */
  "tmmark", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_tm_mark : public gimple_opt_pass
{
public:
  pass_tm_mark (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tm_mark, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return execute_tm_mark (); }

}; // class pass_tm_mark

} // anon namespace

gimple_opt_pass *
make_pass_tm_mark (gcc::context *ctxt)
{
  return new pass_tm_mark (ctxt);
}


/* Create an abnormal edge from STMT at iter, splitting the block
   as necessary.  Adjust *PNEXT as needed for the split block.  */

static inline void
split_bb_make_tm_edge (gimple stmt, basic_block dest_bb,
                       gimple_stmt_iterator iter, gimple_stmt_iterator *pnext)
{
  basic_block bb = gimple_bb (stmt);
  if (!gsi_one_before_end_p (iter))
    {
      edge e = split_block (bb, stmt);
      *pnext = gsi_start_bb (e->dest);
    }
  make_edge (bb, dest_bb, EDGE_ABNORMAL);

  // Record the need for the edge for the benefit of the rtl passes.
  if (cfun->gimple_df->tm_restart == NULL)
    cfun->gimple_df->tm_restart = htab_create_ggc (31, struct_ptr_hash,
						   struct_ptr_eq, ggc_free);

  struct tm_restart_node dummy;
  dummy.stmt = stmt;
  dummy.label_or_list = gimple_block_label (dest_bb);

  void **slot = htab_find_slot (cfun->gimple_df->tm_restart, &dummy, INSERT);
  struct tm_restart_node *n = (struct tm_restart_node *) *slot;
  if (n == NULL)
    {
      n = ggc_alloc<tm_restart_node> ();
      *n = dummy;
    }
  else
    {
      tree old = n->label_or_list;
      if (TREE_CODE (old) == LABEL_DECL)
        old = tree_cons (NULL, old, NULL);
      n->label_or_list = tree_cons (NULL, dummy.label_or_list, old);
    }
}

/* Split block BB as necessary for every builtin function we added, and
   wire up the abnormal back edges implied by the transaction restart.  */

static void
expand_block_edges (struct tm_region *const region, basic_block bb)
{
  gimple_stmt_iterator gsi, next_gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi = next_gsi)
    {
      gimple stmt = gsi_stmt (gsi);

      next_gsi = gsi;
      gsi_next (&next_gsi);

      // ??? Shouldn't we split for any non-pure, non-irrevocable function?
      if (gimple_code (stmt) != GIMPLE_CALL
	  || (gimple_call_flags (stmt) & ECF_TM_BUILTIN) == 0)
	continue;

      if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt)) == BUILT_IN_TM_ABORT)
	{
	  // If we have a ``_transaction_cancel [[outer]]'', there is only
	  // one abnormal edge: to the transaction marked OUTER.
	  // All compiler-generated instances of BUILT_IN_TM_ABORT have a
	  // constant argument, which we can examine here.  Users invoking
	  // TM_ABORT directly get what they deserve.
	  tree arg = gimple_call_arg (stmt, 0);
	  if (TREE_CODE (arg) == INTEGER_CST
	      && (TREE_INT_CST_LOW (arg) & AR_OUTERABORT) != 0
	      && !decl_is_tm_clone (current_function_decl))
	    {
	      // Find the GTMA_IS_OUTER transaction.
	      for (struct tm_region *o = region; o; o = o->outer)
		if (o->original_transaction_was_outer)
		  {
		    split_bb_make_tm_edge (stmt, o->restart_block,
					   gsi, &next_gsi);
		    break;
		  }

	      // Otherwise, the front-end should have semantically checked
	      // outer aborts, but in either case the target region is not
	      // within this function.
	      continue;
	    }

	  // Non-outer, TM aborts have an abnormal edge to the inner-most
	  // transaction, the one being aborted;
	  split_bb_make_tm_edge (stmt, region->restart_block, gsi, &next_gsi);
	}

      // All TM builtins have an abnormal edge to the outer-most transaction.
      // We never restart inner transactions.  For tm clones, we know a-priori
      // that the outer-most transaction is outside the function.
      if (decl_is_tm_clone (current_function_decl))
	continue;

      if (cfun->gimple_df->tm_restart == NULL)
	cfun->gimple_df->tm_restart
	  = htab_create_ggc (31, struct_ptr_hash, struct_ptr_eq, ggc_free);

      // All TM builtins have an abnormal edge to the outer-most transaction.
      // We never restart inner transactions.
      for (struct tm_region *o = region; o; o = o->outer)
	if (!o->outer)
	  {
            split_bb_make_tm_edge (stmt, o->restart_block, gsi, &next_gsi);
	    break;
	  }

      // Delete any tail-call annotation that may have been added.
      // The tail-call pass may have mis-identified the commit as being
      // a candidate because we had not yet added this restart edge.
      gimple_call_set_tail (stmt, false);
    }
}

/* Entry point to the final expansion of transactional nodes. */

namespace {

const pass_data pass_data_tm_edges =
{
  GIMPLE_PASS, /* type */
  "tmedge", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_tm_edges : public gimple_opt_pass
{
public:
  pass_tm_edges (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tm_edges, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_tm_edges

unsigned int
pass_tm_edges::execute (function *fun)
{
  vec<tm_region_p> bb_regions
    = get_bb_regions_instrumented (/*traverse_clones=*/false,
				   /*include_uninstrumented_p=*/true);
  struct tm_region *r;
  unsigned i;

  FOR_EACH_VEC_ELT (bb_regions, i, r)
    if (r != NULL)
      expand_block_edges (r, BASIC_BLOCK_FOR_FN (fun, i));

  bb_regions.release ();

  /* We've got to release the dominance info now, to indicate that it
     must be rebuilt completely.  Otherwise we'll crash trying to update
     the SSA web in the TODO section following this pass.  */
  free_dominance_info (CDI_DOMINATORS);
  bitmap_obstack_release (&tm_obstack);
  all_tm_regions = NULL;

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_tm_edges (gcc::context *ctxt)
{
  return new pass_tm_edges (ctxt);
}

/* Helper function for expand_regions.  Expand REGION and recurse to
   the inner region.  Call CALLBACK on each region.  CALLBACK returns
   NULL to continue the traversal, otherwise a non-null value which
   this function will return as well.  TRAVERSE_CLONES is true if we
   should traverse transactional clones.  */

static void *
expand_regions_1 (struct tm_region *region,
		  void *(*callback)(struct tm_region *, void *),
		  void *data,
		  bool traverse_clones)
{
  void *retval = NULL;
  if (region->exit_blocks
      || (traverse_clones && decl_is_tm_clone (current_function_decl)))
    {
      retval = callback (region, data);
      if (retval)
	return retval;
    }
  if (region->inner)
    {
      retval = expand_regions (region->inner, callback, data, traverse_clones);
      if (retval)
	return retval;
    }
  return retval;
}

/* Traverse the regions enclosed and including REGION.  Execute
   CALLBACK for each region, passing DATA.  CALLBACK returns NULL to
   continue the traversal, otherwise a non-null value which this
   function will return as well.  TRAVERSE_CLONES is true if we should
   traverse transactional clones.  */

static void *
expand_regions (struct tm_region *region,
		void *(*callback)(struct tm_region *, void *),
		void *data,
		bool traverse_clones)
{
  void *retval = NULL;
  while (region)
    {
      retval = expand_regions_1 (region, callback, data, traverse_clones);
      if (retval)
	return retval;
      region = region->next;
    }
  return retval;
}


/* A unique TM memory operation.  */
typedef struct tm_memop
{
  /* Unique ID that all memory operations to the same location have.  */
  unsigned int value_id;
  /* Address of load/store.  */
  tree addr;
} *tm_memop_t;

/* TM memory operation hashtable helpers.  */

struct tm_memop_hasher : typed_free_remove <tm_memop>
{
  typedef tm_memop value_type;
  typedef tm_memop compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Htab support.  Return a hash value for a `tm_memop'.  */
inline hashval_t
tm_memop_hasher::hash (const value_type *mem)
{
  tree addr = mem->addr;
  /* We drill down to the SSA_NAME/DECL for the hash, but equality is
     actually done with operand_equal_p (see tm_memop_eq).  */
  if (TREE_CODE (addr) == ADDR_EXPR)
    addr = TREE_OPERAND (addr, 0);
  return iterative_hash_expr (addr, 0);
}

/* Htab support.  Return true if two tm_memop's are the same.  */
inline bool
tm_memop_hasher::equal (const value_type *mem1, const compare_type *mem2)
{
  return operand_equal_p (mem1->addr, mem2->addr, 0);
}

/* Sets for solving data flow equations in the memory optimization pass.  */
struct tm_memopt_bitmaps
{
  /* Stores available to this BB upon entry.  Basically, stores that
     dominate this BB.  */
  bitmap store_avail_in;
  /* Stores available at the end of this BB.  */
  bitmap store_avail_out;
  bitmap store_antic_in;
  bitmap store_antic_out;
  /* Reads available to this BB upon entry.  Basically, reads that
     dominate this BB.  */
  bitmap read_avail_in;
  /* Reads available at the end of this BB.  */
  bitmap read_avail_out;
  /* Reads performed in this BB.  */
  bitmap read_local;
  /* Writes performed in this BB.  */
  bitmap store_local;

  /* Temporary storage for pass.  */
  /* Is the current BB in the worklist?  */
  bool avail_in_worklist_p;
  /* Have we visited this BB?  */
  bool visited_p;
};

static bitmap_obstack tm_memopt_obstack;

/* Unique counter for TM loads and stores. Loads and stores of the
   same address get the same ID.  */
static unsigned int tm_memopt_value_id;
static hash_table <tm_memop_hasher> tm_memopt_value_numbers;

#define STORE_AVAIL_IN(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->store_avail_in
#define STORE_AVAIL_OUT(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->store_avail_out
#define STORE_ANTIC_IN(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->store_antic_in
#define STORE_ANTIC_OUT(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->store_antic_out
#define READ_AVAIL_IN(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->read_avail_in
#define READ_AVAIL_OUT(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->read_avail_out
#define READ_LOCAL(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->read_local
#define STORE_LOCAL(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->store_local
#define AVAIL_IN_WORKLIST_P(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->avail_in_worklist_p
#define BB_VISITED_P(BB) \
  ((struct tm_memopt_bitmaps *) ((BB)->aux))->visited_p

/* Given a TM load/store in STMT, return the value number for the address
   it accesses.  */

static unsigned int
tm_memopt_value_number (gimple stmt, enum insert_option op)
{
  struct tm_memop tmpmem, *mem;
  tm_memop **slot;

  gcc_assert (is_tm_load (stmt) || is_tm_store (stmt));
  tmpmem.addr = gimple_call_arg (stmt, 0);
  slot = tm_memopt_value_numbers.find_slot (&tmpmem, op);
  if (*slot)
    mem = *slot;
  else if (op == INSERT)
    {
      mem = XNEW (struct tm_memop);
      *slot = mem;
      mem->value_id = tm_memopt_value_id++;
      mem->addr = tmpmem.addr;
    }
  else
    gcc_unreachable ();
  return mem->value_id;
}

/* Accumulate TM memory operations in BB into STORE_LOCAL and READ_LOCAL.  */

static void
tm_memopt_accumulate_memops (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      bitmap bits;
      unsigned int loc;

      if (is_tm_store (stmt))
	bits = STORE_LOCAL (bb);
      else if (is_tm_load (stmt))
	bits = READ_LOCAL (bb);
      else
	continue;

      loc = tm_memopt_value_number (stmt, INSERT);
      bitmap_set_bit (bits, loc);
      if (dump_file)
	{
	  fprintf (dump_file, "TM memopt (%s): value num=%d, BB=%d, addr=",
		   is_tm_load (stmt) ? "LOAD" : "STORE", loc,
		   gimple_bb (stmt)->index);
	  print_generic_expr (dump_file, gimple_call_arg (stmt, 0), 0);
	  fprintf (dump_file, "\n");
	}
    }
}

/* Prettily dump one of the memopt sets.  BITS is the bitmap to dump.  */

static void
dump_tm_memopt_set (const char *set_name, bitmap bits)
{
  unsigned i;
  bitmap_iterator bi;
  const char *comma = "";

  fprintf (dump_file, "TM memopt: %s: [", set_name);
  EXECUTE_IF_SET_IN_BITMAP (bits, 0, i, bi)
    {
      hash_table <tm_memop_hasher>::iterator hi;
      struct tm_memop *mem = NULL;

      /* Yeah, yeah, yeah.  Whatever.  This is just for debugging.  */
      FOR_EACH_HASH_TABLE_ELEMENT (tm_memopt_value_numbers, mem, tm_memop_t, hi)
	if (mem->value_id == i)
	  break;
      gcc_assert (mem->value_id == i);
      fprintf (dump_file, "%s", comma);
      comma = ", ";
      print_generic_expr (dump_file, mem->addr, 0);
    }
  fprintf (dump_file, "]\n");
}

/* Prettily dump all of the memopt sets in BLOCKS.  */

static void
dump_tm_memopt_sets (vec<basic_block> blocks)
{
  size_t i;
  basic_block bb;

  for (i = 0; blocks.iterate (i, &bb); ++i)
    {
      fprintf (dump_file, "------------BB %d---------\n", bb->index);
      dump_tm_memopt_set ("STORE_LOCAL", STORE_LOCAL (bb));
      dump_tm_memopt_set ("READ_LOCAL", READ_LOCAL (bb));
      dump_tm_memopt_set ("STORE_AVAIL_IN", STORE_AVAIL_IN (bb));
      dump_tm_memopt_set ("STORE_AVAIL_OUT", STORE_AVAIL_OUT (bb));
      dump_tm_memopt_set ("READ_AVAIL_IN", READ_AVAIL_IN (bb));
      dump_tm_memopt_set ("READ_AVAIL_OUT", READ_AVAIL_OUT (bb));
    }
}

/* Compute {STORE,READ}_AVAIL_IN for the basic block BB.  */

static void
tm_memopt_compute_avin (basic_block bb)
{
  edge e;
  unsigned ix;

  /* Seed with the AVOUT of any predecessor.  */
  for (ix = 0; ix < EDGE_COUNT (bb->preds); ix++)
    {
      e = EDGE_PRED (bb, ix);
      /* Make sure we have already visited this BB, and is thus
	 initialized.

	  If e->src->aux is NULL, this predecessor is actually on an
	  enclosing transaction.  We only care about the current
	  transaction, so ignore it.  */
      if (e->src->aux && BB_VISITED_P (e->src))
	{
	  bitmap_copy (STORE_AVAIL_IN (bb), STORE_AVAIL_OUT (e->src));
	  bitmap_copy (READ_AVAIL_IN (bb), READ_AVAIL_OUT (e->src));
	  break;
	}
    }

  for (; ix < EDGE_COUNT (bb->preds); ix++)
    {
      e = EDGE_PRED (bb, ix);
      if (e->src->aux && BB_VISITED_P (e->src))
	{
	  bitmap_and_into (STORE_AVAIL_IN (bb), STORE_AVAIL_OUT (e->src));
	  bitmap_and_into (READ_AVAIL_IN (bb), READ_AVAIL_OUT (e->src));
	}
    }

  BB_VISITED_P (bb) = true;
}

/* Compute the STORE_ANTIC_IN for the basic block BB.  */

static void
tm_memopt_compute_antin (basic_block bb)
{
  edge e;
  unsigned ix;

  /* Seed with the ANTIC_OUT of any successor.  */
  for (ix = 0; ix < EDGE_COUNT (bb->succs); ix++)
    {
      e = EDGE_SUCC (bb, ix);
      /* Make sure we have already visited this BB, and is thus
	 initialized.  */
      if (BB_VISITED_P (e->dest))
	{
	  bitmap_copy (STORE_ANTIC_IN (bb), STORE_ANTIC_OUT (e->dest));
	  break;
	}
    }

  for (; ix < EDGE_COUNT (bb->succs); ix++)
    {
      e = EDGE_SUCC (bb, ix);
      if (BB_VISITED_P  (e->dest))
	bitmap_and_into (STORE_ANTIC_IN (bb), STORE_ANTIC_OUT (e->dest));
    }

  BB_VISITED_P (bb) = true;
}

/* Compute the AVAIL sets for every basic block in BLOCKS.

   We compute {STORE,READ}_AVAIL_{OUT,IN} as follows:

     AVAIL_OUT[bb] = union (AVAIL_IN[bb], LOCAL[bb])
     AVAIL_IN[bb]  = intersect (AVAIL_OUT[predecessors])

   This is basically what we do in lcm's compute_available(), but here
   we calculate two sets of sets (one for STOREs and one for READs),
   and we work on a region instead of the entire CFG.

   REGION is the TM region.
   BLOCKS are the basic blocks in the region.  */

static void
tm_memopt_compute_available (struct tm_region *region,
			     vec<basic_block> blocks)
{
  edge e;
  basic_block *worklist, *qin, *qout, *qend, bb;
  unsigned int qlen, i;
  edge_iterator ei;
  bool changed;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks in the region.  */
  qlen = blocks.length () - 1;
  qin = qout = worklist =
    XNEWVEC (basic_block, qlen);

  /* Put every block in the region on the worklist.  */
  for (i = 0; blocks.iterate (i, &bb); ++i)
    {
      /* Seed AVAIL_OUT with the LOCAL set.  */
      bitmap_ior_into (STORE_AVAIL_OUT (bb), STORE_LOCAL (bb));
      bitmap_ior_into (READ_AVAIL_OUT (bb), READ_LOCAL (bb));

      AVAIL_IN_WORKLIST_P (bb) = true;
      /* No need to insert the entry block, since it has an AVIN of
	 null, and an AVOUT that has already been seeded in.  */
      if (bb != region->entry_block)
	*qin++ = bb;
    }

  /* The entry block has been initialized with the local sets.  */
  BB_VISITED_P (region->entry_block) = true;

  qin = worklist;
  qend = &worklist[qlen];

  /* Iterate until the worklist is empty.  */
  while (qlen)
    {
      /* Take the first entry off the worklist.  */
      bb = *qout++;
      qlen--;

      if (qout >= qend)
	qout = worklist;

      /* This block can be added to the worklist again if necessary.  */
      AVAIL_IN_WORKLIST_P (bb) = false;
      tm_memopt_compute_avin (bb);

      /* Note: We do not add the LOCAL sets here because we already
	 seeded the AVAIL_OUT sets with them.  */
      changed  = bitmap_ior_into (STORE_AVAIL_OUT (bb), STORE_AVAIL_IN (bb));
      changed |= bitmap_ior_into (READ_AVAIL_OUT (bb), READ_AVAIL_IN (bb));
      if (changed
	  && (region->exit_blocks == NULL
	      || !bitmap_bit_p (region->exit_blocks, bb->index)))
	/* If the out state of this block changed, then we need to add
	   its successors to the worklist if they are not already in.  */
	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (!AVAIL_IN_WORKLIST_P (e->dest)
	      && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	    {
	      *qin++ = e->dest;
	      AVAIL_IN_WORKLIST_P (e->dest) = true;
	      qlen++;

	      if (qin >= qend)
		qin = worklist;
	    }
    }

  free (worklist);

  if (dump_file)
    dump_tm_memopt_sets (blocks);
}

/* Compute ANTIC sets for every basic block in BLOCKS.

   We compute STORE_ANTIC_OUT as follows:

	STORE_ANTIC_OUT[bb] = union(STORE_ANTIC_IN[bb], STORE_LOCAL[bb])
	STORE_ANTIC_IN[bb]  = intersect(STORE_ANTIC_OUT[successors])

   REGION is the TM region.
   BLOCKS are the basic blocks in the region.  */

static void
tm_memopt_compute_antic (struct tm_region *region,
			 vec<basic_block> blocks)
{
  edge e;
  basic_block *worklist, *qin, *qout, *qend, bb;
  unsigned int qlen;
  int i;
  edge_iterator ei;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks in the region.  */
  qin = qout = worklist = XNEWVEC (basic_block, blocks.length ());

  for (qlen = 0, i = blocks.length () - 1; i >= 0; --i)
    {
      bb = blocks[i];

      /* Seed ANTIC_OUT with the LOCAL set.  */
      bitmap_ior_into (STORE_ANTIC_OUT (bb), STORE_LOCAL (bb));

      /* Put every block in the region on the worklist.  */
      AVAIL_IN_WORKLIST_P (bb) = true;
      /* No need to insert exit blocks, since their ANTIC_IN is NULL,
	 and their ANTIC_OUT has already been seeded in.  */
      if (region->exit_blocks
	  && !bitmap_bit_p (region->exit_blocks, bb->index))
	{
	  qlen++;
	  *qin++ = bb;
	}
    }

  /* The exit blocks have been initialized with the local sets.  */
  if (region->exit_blocks)
    {
      unsigned int i;
      bitmap_iterator bi;
      EXECUTE_IF_SET_IN_BITMAP (region->exit_blocks, 0, i, bi)
	BB_VISITED_P (BASIC_BLOCK_FOR_FN (cfun, i)) = true;
    }

  qin = worklist;
  qend = &worklist[qlen];

  /* Iterate until the worklist is empty.  */
  while (qlen)
    {
      /* Take the first entry off the worklist.  */
      bb = *qout++;
      qlen--;

      if (qout >= qend)
	qout = worklist;

      /* This block can be added to the worklist again if necessary.  */
      AVAIL_IN_WORKLIST_P (bb) = false;
      tm_memopt_compute_antin (bb);

      /* Note: We do not add the LOCAL sets here because we already
	 seeded the ANTIC_OUT sets with them.  */
      if (bitmap_ior_into (STORE_ANTIC_OUT (bb), STORE_ANTIC_IN (bb))
	  && bb != region->entry_block)
	/* If the out state of this block changed, then we need to add
	   its predecessors to the worklist if they are not already in.  */
	FOR_EACH_EDGE (e, ei, bb->preds)
	  if (!AVAIL_IN_WORKLIST_P (e->src))
	    {
	      *qin++ = e->src;
	      AVAIL_IN_WORKLIST_P (e->src) = true;
	      qlen++;

	      if (qin >= qend)
		qin = worklist;
	    }
    }

  free (worklist);

  if (dump_file)
    dump_tm_memopt_sets (blocks);
}

/* Offsets of load variants from TM_LOAD.  For example,
   BUILT_IN_TM_LOAD_RAR* is an offset of 1 from BUILT_IN_TM_LOAD*.
   See gtm-builtins.def.  */
#define TRANSFORM_RAR 1
#define TRANSFORM_RAW 2
#define TRANSFORM_RFW 3
/* Offsets of store variants from TM_STORE.  */
#define TRANSFORM_WAR 1
#define TRANSFORM_WAW 2

/* Inform about a load/store optimization.  */

static void
dump_tm_memopt_transform (gimple stmt)
{
  if (dump_file)
    {
      fprintf (dump_file, "TM memopt: transforming: ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
      fprintf (dump_file, "\n");
    }
}

/* Perform a read/write optimization.  Replaces the TM builtin in STMT
   by a builtin that is OFFSET entries down in the builtins table in
   gtm-builtins.def.  */

static void
tm_memopt_transform_stmt (unsigned int offset,
			  gimple stmt,
			  gimple_stmt_iterator *gsi)
{
  tree fn = gimple_call_fn (stmt);
  gcc_assert (TREE_CODE (fn) == ADDR_EXPR);
  TREE_OPERAND (fn, 0)
    = builtin_decl_explicit ((enum built_in_function)
			     (DECL_FUNCTION_CODE (TREE_OPERAND (fn, 0))
			      + offset));
  gimple_call_set_fn (stmt, fn);
  gsi_replace (gsi, stmt, true);
  dump_tm_memopt_transform (stmt);
}

/* Perform the actual TM memory optimization transformations in the
   basic blocks in BLOCKS.  */

static void
tm_memopt_transform_blocks (vec<basic_block> blocks)
{
  size_t i;
  basic_block bb;
  gimple_stmt_iterator gsi;

  for (i = 0; blocks.iterate (i, &bb); ++i)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  bitmap read_avail = READ_AVAIL_IN (bb);
	  bitmap store_avail = STORE_AVAIL_IN (bb);
	  bitmap store_antic = STORE_ANTIC_OUT (bb);
	  unsigned int loc;

	  if (is_tm_simple_load (stmt))
	    {
	      loc = tm_memopt_value_number (stmt, NO_INSERT);
	      if (store_avail && bitmap_bit_p (store_avail, loc))
		tm_memopt_transform_stmt (TRANSFORM_RAW, stmt, &gsi);
	      else if (store_antic && bitmap_bit_p (store_antic, loc))
		{
		  tm_memopt_transform_stmt (TRANSFORM_RFW, stmt, &gsi);
		  bitmap_set_bit (store_avail, loc);
		}
	      else if (read_avail && bitmap_bit_p (read_avail, loc))
		tm_memopt_transform_stmt (TRANSFORM_RAR, stmt, &gsi);
	      else
		bitmap_set_bit (read_avail, loc);
	    }
	  else if (is_tm_simple_store (stmt))
	    {
	      loc = tm_memopt_value_number (stmt, NO_INSERT);
	      if (store_avail && bitmap_bit_p (store_avail, loc))
		tm_memopt_transform_stmt (TRANSFORM_WAW, stmt, &gsi);
	      else
		{
		  if (read_avail && bitmap_bit_p (read_avail, loc))
		    tm_memopt_transform_stmt (TRANSFORM_WAR, stmt, &gsi);
		  bitmap_set_bit (store_avail, loc);
		}
	    }
	}
    }
}

/* Return a new set of bitmaps for a BB.  */

static struct tm_memopt_bitmaps *
tm_memopt_init_sets (void)
{
  struct tm_memopt_bitmaps *b
    = XOBNEW (&tm_memopt_obstack.obstack, struct tm_memopt_bitmaps);
  b->store_avail_in = BITMAP_ALLOC (&tm_memopt_obstack);
  b->store_avail_out = BITMAP_ALLOC (&tm_memopt_obstack);
  b->store_antic_in = BITMAP_ALLOC (&tm_memopt_obstack);
  b->store_antic_out = BITMAP_ALLOC (&tm_memopt_obstack);
  b->store_avail_out = BITMAP_ALLOC (&tm_memopt_obstack);
  b->read_avail_in = BITMAP_ALLOC (&tm_memopt_obstack);
  b->read_avail_out = BITMAP_ALLOC (&tm_memopt_obstack);
  b->read_local = BITMAP_ALLOC (&tm_memopt_obstack);
  b->store_local = BITMAP_ALLOC (&tm_memopt_obstack);
  return b;
}

/* Free sets computed for each BB.  */

static void
tm_memopt_free_sets (vec<basic_block> blocks)
{
  size_t i;
  basic_block bb;

  for (i = 0; blocks.iterate (i, &bb); ++i)
    bb->aux = NULL;
}

/* Clear the visited bit for every basic block in BLOCKS.  */

static void
tm_memopt_clear_visited (vec<basic_block> blocks)
{
  size_t i;
  basic_block bb;

  for (i = 0; blocks.iterate (i, &bb); ++i)
    BB_VISITED_P (bb) = false;
}

/* Replace TM load/stores with hints for the runtime.  We handle
   things like read-after-write, write-after-read, read-after-read,
   read-for-write, etc.  */

static unsigned int
execute_tm_memopt (void)
{
  struct tm_region *region;
  vec<basic_block> bbs;

  tm_memopt_value_id = 0;
  tm_memopt_value_numbers.create (10);

  for (region = all_tm_regions; region; region = region->next)
    {
      /* All the TM stores/loads in the current region.  */
      size_t i;
      basic_block bb;

      bitmap_obstack_initialize (&tm_memopt_obstack);

      /* Save all BBs for the current region.  */
      bbs = get_tm_region_blocks (region->entry_block,
				  region->exit_blocks,
				  region->irr_blocks,
				  NULL,
				  false);

      /* Collect all the memory operations.  */
      for (i = 0; bbs.iterate (i, &bb); ++i)
	{
	  bb->aux = tm_memopt_init_sets ();
	  tm_memopt_accumulate_memops (bb);
	}

      /* Solve data flow equations and transform each block accordingly.  */
      tm_memopt_clear_visited (bbs);
      tm_memopt_compute_available (region, bbs);
      tm_memopt_clear_visited (bbs);
      tm_memopt_compute_antic (region, bbs);
      tm_memopt_transform_blocks (bbs);

      tm_memopt_free_sets (bbs);
      bbs.release ();
      bitmap_obstack_release (&tm_memopt_obstack);
      tm_memopt_value_numbers.empty ();
    }

  tm_memopt_value_numbers.dispose ();
  return 0;
}

namespace {

const pass_data pass_data_tm_memopt =
{
  GIMPLE_PASS, /* type */
  "tmmemopt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tm_memopt : public gimple_opt_pass
{
public:
  pass_tm_memopt (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tm_memopt, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tm && optimize > 0; }
  virtual unsigned int execute (function *) { return execute_tm_memopt (); }

}; // class pass_tm_memopt

} // anon namespace

gimple_opt_pass *
make_pass_tm_memopt (gcc::context *ctxt)
{
  return new pass_tm_memopt (ctxt);
}


/* Interprocedual analysis for the creation of transactional clones.
   The aim of this pass is to find which functions are referenced in
   a non-irrevocable transaction context, and for those over which
   we have control (or user directive), create a version of the
   function which uses only the transactional interface to reference
   protected memories.  This analysis proceeds in several steps:

     (1) Collect the set of all possible transactional clones:

	(a) For all local public functions marked tm_callable, push
	    it onto the tm_callee queue.

	(b) For all local functions, scan for calls in transaction blocks.
	    Push the caller and callee onto the tm_caller and tm_callee
	    queues.  Count the number of callers for each callee.

	(c) For each local function on the callee list, assume we will
	    create a transactional clone.  Push *all* calls onto the
	    callee queues; count the number of clone callers separately
	    to the number of original callers.

     (2) Propagate irrevocable status up the dominator tree:

	(a) Any external function on the callee list that is not marked
	    tm_callable is irrevocable.  Push all callers of such onto
	    a worklist.

	(b) For each function on the worklist, mark each block that
	    contains an irrevocable call.  Use the AND operator to
	    propagate that mark up the dominator tree.

	(c) If we reach the entry block for a possible transactional
	    clone, then the transactional clone is irrevocable, and
	    we should not create the clone after all.  Push all
	    callers onto the worklist.

	(d) Place tm_irrevocable calls at the beginning of the relevant
	    blocks.  Special case here is the entry block for the entire
	    transaction region; there we mark it GTMA_DOES_GO_IRREVOCABLE for
	    the library to begin the region in serial mode.  Decrement
	    the call count for all callees in the irrevocable region.

     (3) Create the transactional clones:

	Any tm_callee that still has a non-zero call count is cloned.
*/

/* This structure is stored in the AUX field of each cgraph_node.  */
struct tm_ipa_cg_data
{
  /* The clone of the function that got created.  */
  struct cgraph_node *clone;

  /* The tm regions in the normal function.  */
  struct tm_region *all_tm_regions;

  /* The blocks of the normal/clone functions that contain irrevocable
     calls, or blocks that are post-dominated by irrevocable calls.  */
  bitmap irrevocable_blocks_normal;
  bitmap irrevocable_blocks_clone;

  /* The blocks of the normal function that are involved in transactions.  */
  bitmap transaction_blocks_normal;

  /* The number of callers to the transactional clone of this function
     from normal and transactional clones respectively.  */
  unsigned tm_callers_normal;
  unsigned tm_callers_clone;

  /* True if all calls to this function's transactional clone
     are irrevocable.  Also automatically true if the function
     has no transactional clone.  */
  bool is_irrevocable;

  /* Flags indicating the presence of this function in various queues.  */
  bool in_callee_queue;
  bool in_worklist;

  /* Flags indicating the kind of scan desired while in the worklist.  */
  bool want_irr_scan_normal;
};

typedef vec<cgraph_node_ptr> cgraph_node_queue;

/* Return the ipa data associated with NODE, allocating zeroed memory
   if necessary.  TRAVERSE_ALIASES is true if we must traverse aliases
   and set *NODE accordingly.  */

static struct tm_ipa_cg_data *
get_cg_data (struct cgraph_node **node, bool traverse_aliases)
{
  struct tm_ipa_cg_data *d;

  if (traverse_aliases && (*node)->alias)
    *node = cgraph_alias_target (*node);

  d = (struct tm_ipa_cg_data *) (*node)->aux;

  if (d == NULL)
    {
      d = (struct tm_ipa_cg_data *)
	obstack_alloc (&tm_obstack.obstack, sizeof (*d));
      (*node)->aux = (void *) d;
      memset (d, 0, sizeof (*d));
    }

  return d;
}

/* Add NODE to the end of QUEUE, unless IN_QUEUE_P indicates that
   it is already present.  */

static void
maybe_push_queue (struct cgraph_node *node,
		  cgraph_node_queue *queue_p, bool *in_queue_p)
{
  if (!*in_queue_p)
    {
      *in_queue_p = true;
      queue_p->safe_push (node);
    }
}

/* Duplicate the basic blocks in QUEUE for use in the uninstrumented
   code path.  QUEUE are the basic blocks inside the transaction
   represented in REGION.

   Later in split_code_paths() we will add the conditional to choose
   between the two alternatives.  */

static void
ipa_uninstrument_transaction (struct tm_region *region,
			      vec<basic_block> queue)
{
  gimple transaction = region->transaction_stmt;
  basic_block transaction_bb = gimple_bb (transaction);
  int n = queue.length ();
  basic_block *new_bbs = XNEWVEC (basic_block, n);

  copy_bbs (queue.address (), n, new_bbs, NULL, 0, NULL, NULL, transaction_bb,
	    true);
  edge e = make_edge (transaction_bb, new_bbs[0], EDGE_TM_UNINSTRUMENTED);
  add_phi_args_after_copy (new_bbs, n, e);

  // Now we will have a GIMPLE_ATOMIC with 3 possible edges out of it.
  //   a) EDGE_FALLTHRU into the transaction
  //   b) EDGE_TM_ABORT out of the transaction
  //   c) EDGE_TM_UNINSTRUMENTED into the uninstrumented blocks.

  free (new_bbs);
}

/* A subroutine of ipa_tm_scan_calls_transaction and ipa_tm_scan_calls_clone.
   Queue all callees within block BB.  */

static void
ipa_tm_scan_calls_block (cgraph_node_queue *callees_p,
			 basic_block bb, bool for_clone)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      if (is_gimple_call (stmt) && !is_tm_pure_call (stmt))
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  if (fndecl)
	    {
	      struct tm_ipa_cg_data *d;
	      unsigned *pcallers;
	      struct cgraph_node *node;

	      if (is_tm_ending_fndecl (fndecl))
		continue;
	      if (find_tm_replacement_function (fndecl))
		continue;

	      node = cgraph_get_node (fndecl);
	      gcc_assert (node != NULL);
	      d = get_cg_data (&node, true);

	      pcallers = (for_clone ? &d->tm_callers_clone
			  : &d->tm_callers_normal);
	      *pcallers += 1;

	      maybe_push_queue (node, callees_p, &d->in_callee_queue);
	    }
	}
    }
}

/* Scan all calls in NODE that are within a transaction region,
   and push the resulting nodes into the callee queue.  */

static void
ipa_tm_scan_calls_transaction (struct tm_ipa_cg_data *d,
			       cgraph_node_queue *callees_p)
{
  struct tm_region *r;

  d->transaction_blocks_normal = BITMAP_ALLOC (&tm_obstack);
  d->all_tm_regions = all_tm_regions;

  for (r = all_tm_regions; r; r = r->next)
    {
      vec<basic_block> bbs;
      basic_block bb;
      unsigned i;

      bbs = get_tm_region_blocks (r->entry_block, r->exit_blocks, NULL,
				  d->transaction_blocks_normal, false);

      // Generate the uninstrumented code path for this transaction.
      ipa_uninstrument_transaction (r, bbs);

      FOR_EACH_VEC_ELT (bbs, i, bb)
	ipa_tm_scan_calls_block (callees_p, bb, false);

      bbs.release ();
    }

  // ??? copy_bbs should maintain cgraph edges for the blocks as it is
  // copying them, rather than forcing us to do this externally.
  rebuild_cgraph_edges ();

  // ??? In ipa_uninstrument_transaction we don't try to update dominators
  // because copy_bbs doesn't return a VEC like iterate_fix_dominators expects.
  // Instead, just release dominators here so update_ssa recomputes them.
  free_dominance_info (CDI_DOMINATORS);

  // When building the uninstrumented code path, copy_bbs will have invoked
  // create_new_def_for starting an "ssa update context".  There is only one
  // instance of this context, so resolve ssa updates before moving on to
  // the next function.
  update_ssa (TODO_update_ssa);
}

/* Scan all calls in NODE as if this is the transactional clone,
   and push the destinations into the callee queue.  */

static void
ipa_tm_scan_calls_clone (struct cgraph_node *node,
			 cgraph_node_queue *callees_p)
{
  struct function *fn = DECL_STRUCT_FUNCTION (node->decl);
  basic_block bb;

  FOR_EACH_BB_FN (bb, fn)
    ipa_tm_scan_calls_block (callees_p, bb, true);
}

/* The function NODE has been detected to be irrevocable.  Push all
   of its callers onto WORKLIST for the purpose of re-scanning them.  */

static void
ipa_tm_note_irrevocable (struct cgraph_node *node,
			 cgraph_node_queue *worklist_p)
{
  struct tm_ipa_cg_data *d = get_cg_data (&node, true);
  struct cgraph_edge *e;

  d->is_irrevocable = true;

  for (e = node->callers; e ; e = e->next_caller)
    {
      basic_block bb;
      struct cgraph_node *caller;

      /* Don't examine recursive calls.  */
      if (e->caller == node)
	continue;
      /* Even if we think we can go irrevocable, believe the user
	 above all.  */
      if (is_tm_safe_or_pure (e->caller->decl))
	continue;

      caller = e->caller;
      d = get_cg_data (&caller, true);

      /* Check if the callee is in a transactional region.  If so,
	 schedule the function for normal re-scan as well.  */
      bb = gimple_bb (e->call_stmt);
      gcc_assert (bb != NULL);
      if (d->transaction_blocks_normal
	  && bitmap_bit_p (d->transaction_blocks_normal, bb->index))
	d->want_irr_scan_normal = true;

      maybe_push_queue (caller, worklist_p, &d->in_worklist);
    }
}

/* A subroutine of ipa_tm_scan_irr_blocks; return true iff any statement
   within the block is irrevocable.  */

static bool
ipa_tm_scan_irr_block (basic_block bb)
{
  gimple_stmt_iterator gsi;
  tree fn;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      switch (gimple_code (stmt))
	{
	case GIMPLE_ASSIGN:
	  if (gimple_assign_single_p (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs = gimple_assign_rhs1 (stmt);
	      if (volatile_var_p (lhs) || volatile_var_p (rhs))
		return true;
	    }
	  break;

	case GIMPLE_CALL:
	  {
	    tree lhs = gimple_call_lhs (stmt);
	    if (lhs && volatile_var_p (lhs))
	      return true;

	    if (is_tm_pure_call (stmt))
	      break;

	    fn = gimple_call_fn (stmt);

	    /* Functions with the attribute are by definition irrevocable.  */
	    if (is_tm_irrevocable (fn))
	      return true;

	    /* For direct function calls, go ahead and check for replacement
	       functions, or transitive irrevocable functions.  For indirect
	       functions, we'll ask the runtime.  */
	    if (TREE_CODE (fn) == ADDR_EXPR)
	      {
		struct tm_ipa_cg_data *d;
		struct cgraph_node *node;

		fn = TREE_OPERAND (fn, 0);
		if (is_tm_ending_fndecl (fn))
		  break;
		if (find_tm_replacement_function (fn))
		  break;

		node = cgraph_get_node (fn);
		d = get_cg_data (&node, true);

		/* Return true if irrevocable, but above all, believe
		   the user.  */
		if (d->is_irrevocable
		    && !is_tm_safe_or_pure (fn))
		  return true;
	      }
	    break;
	  }

	case GIMPLE_ASM:
	  /* ??? The Approved Method of indicating that an inline
	     assembly statement is not relevant to the transaction
	     is to wrap it in a __tm_waiver block.  This is not
	     yet implemented, so we can't check for it.  */
	  if (is_tm_safe (current_function_decl))
	    {
	      tree t = build1 (NOP_EXPR, void_type_node, size_zero_node);
	      SET_EXPR_LOCATION (t, gimple_location (stmt));
	      error ("%Kasm not allowed in %<transaction_safe%> function", t);
	    }
	  return true;

	default:
	  break;
	}
    }

  return false;
}

/* For each of the blocks seeded witin PQUEUE, walk the CFG looking
   for new irrevocable blocks, marking them in NEW_IRR.  Don't bother
   scanning past OLD_IRR or EXIT_BLOCKS.  */

static bool
ipa_tm_scan_irr_blocks (vec<basic_block> *pqueue, bitmap new_irr,
			bitmap old_irr, bitmap exit_blocks)
{
  bool any_new_irr = false;
  edge e;
  edge_iterator ei;
  bitmap visited_blocks = BITMAP_ALLOC (NULL);

  do
    {
      basic_block bb = pqueue->pop ();

      /* Don't re-scan blocks we know already are irrevocable.  */
      if (old_irr && bitmap_bit_p (old_irr, bb->index))
	continue;

      if (ipa_tm_scan_irr_block (bb))
	{
	  bitmap_set_bit (new_irr, bb->index);
	  any_new_irr = true;
	}
      else if (exit_blocks == NULL || !bitmap_bit_p (exit_blocks, bb->index))
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!bitmap_bit_p (visited_blocks, e->dest->index))
	      {
		bitmap_set_bit (visited_blocks, e->dest->index);
		pqueue->safe_push (e->dest);
	      }
	}
    }
  while (!pqueue->is_empty ());

  BITMAP_FREE (visited_blocks);

  return any_new_irr;
}

/* Propagate the irrevocable property both up and down the dominator tree.
   BB is the current block being scanned; EXIT_BLOCKS are the edges of the
   TM regions; OLD_IRR are the results of a previous scan of the dominator
   tree which has been fully propagated; NEW_IRR is the set of new blocks
   which are gaining the irrevocable property during the current scan.  */

static void
ipa_tm_propagate_irr (basic_block entry_block, bitmap new_irr,
		      bitmap old_irr, bitmap exit_blocks)
{
  vec<basic_block> bbs;
  bitmap all_region_blocks;

  /* If this block is in the old set, no need to rescan.  */
  if (old_irr && bitmap_bit_p (old_irr, entry_block->index))
    return;

  all_region_blocks = BITMAP_ALLOC (&tm_obstack);
  bbs = get_tm_region_blocks (entry_block, exit_blocks, NULL,
			      all_region_blocks, false);
  do
    {
      basic_block bb = bbs.pop ();
      bool this_irr = bitmap_bit_p (new_irr, bb->index);
      bool all_son_irr = false;
      edge_iterator ei;
      edge e;

      /* Propagate up.  If my children are, I am too, but we must have
	 at least one child that is.  */
      if (!this_irr)
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (!bitmap_bit_p (new_irr, e->dest->index))
		{
		  all_son_irr = false;
		  break;
		}
	      else
		all_son_irr = true;
	    }
	  if (all_son_irr)
	    {
	      /* Add block to new_irr if it hasn't already been processed. */
	      if (!old_irr || !bitmap_bit_p (old_irr, bb->index))
		{
		  bitmap_set_bit (new_irr, bb->index);
		  this_irr = true;
		}
	    }
	}

      /* Propagate down to everyone we immediately dominate.  */
      if (this_irr)
	{
	  basic_block son;
	  for (son = first_dom_son (CDI_DOMINATORS, bb);
	       son;
	       son = next_dom_son (CDI_DOMINATORS, son))
	    {
	      /* Make sure block is actually in a TM region, and it
		 isn't already in old_irr.  */
	      if ((!old_irr || !bitmap_bit_p (old_irr, son->index))
		  && bitmap_bit_p (all_region_blocks, son->index))
		bitmap_set_bit (new_irr, son->index);
	    }
	}
    }
  while (!bbs.is_empty ());

  BITMAP_FREE (all_region_blocks);
  bbs.release ();
}

static void
ipa_tm_decrement_clone_counts (basic_block bb, bool for_clone)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      if (is_gimple_call (stmt) && !is_tm_pure_call (stmt))
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  if (fndecl)
	    {
	      struct tm_ipa_cg_data *d;
	      unsigned *pcallers;
	      struct cgraph_node *tnode;

	      if (is_tm_ending_fndecl (fndecl))
		continue;
	      if (find_tm_replacement_function (fndecl))
		continue;

	      tnode = cgraph_get_node (fndecl);
	      d = get_cg_data (&tnode, true);

	      pcallers = (for_clone ? &d->tm_callers_clone
			  : &d->tm_callers_normal);

	      gcc_assert (*pcallers > 0);
	      *pcallers -= 1;
	    }
	}
    }
}

/* (Re-)Scan the transaction blocks in NODE for calls to irrevocable functions,
   as well as other irrevocable actions such as inline assembly.  Mark all
   such blocks as irrevocable and decrement the number of calls to
   transactional clones.  Return true if, for the transactional clone, the
   entire function is irrevocable.  */

static bool
ipa_tm_scan_irr_function (struct cgraph_node *node, bool for_clone)
{
  struct tm_ipa_cg_data *d;
  bitmap new_irr, old_irr;
  bool ret = false;

  /* Builtin operators (operator new, and such).  */
  if (DECL_STRUCT_FUNCTION (node->decl) == NULL
      || DECL_STRUCT_FUNCTION (node->decl)->cfg == NULL)
    return false;

  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  calculate_dominance_info (CDI_DOMINATORS);

  d = get_cg_data (&node, true);
  auto_vec<basic_block, 10> queue;
  new_irr = BITMAP_ALLOC (&tm_obstack);

  /* Scan each tm region, propagating irrevocable status through the tree.  */
  if (for_clone)
    {
      old_irr = d->irrevocable_blocks_clone;
      queue.quick_push (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
      if (ipa_tm_scan_irr_blocks (&queue, new_irr, old_irr, NULL))
	{
	  ipa_tm_propagate_irr (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
				new_irr,
				old_irr, NULL);
	  ret = bitmap_bit_p (new_irr,
			      single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun))->index);
	}
    }
  else
    {
      struct tm_region *region;

      old_irr = d->irrevocable_blocks_normal;
      for (region = d->all_tm_regions; region; region = region->next)
	{
	  queue.quick_push (region->entry_block);
	  if (ipa_tm_scan_irr_blocks (&queue, new_irr, old_irr,
				      region->exit_blocks))
	    ipa_tm_propagate_irr (region->entry_block, new_irr, old_irr,
				  region->exit_blocks);
	}
    }

  /* If we found any new irrevocable blocks, reduce the call count for
     transactional clones within the irrevocable blocks.  Save the new
     set of irrevocable blocks for next time.  */
  if (!bitmap_empty_p (new_irr))
    {
      bitmap_iterator bmi;
      unsigned i;

      EXECUTE_IF_SET_IN_BITMAP (new_irr, 0, i, bmi)
	ipa_tm_decrement_clone_counts (BASIC_BLOCK_FOR_FN (cfun, i),
				       for_clone);

      if (old_irr)
	{
	  bitmap_ior_into (old_irr, new_irr);
	  BITMAP_FREE (new_irr);
	}
      else if (for_clone)
	d->irrevocable_blocks_clone = new_irr;
      else
	d->irrevocable_blocks_normal = new_irr;

      if (dump_file && new_irr)
	{
	  const char *dname;
	  bitmap_iterator bmi;
	  unsigned i;

	  dname = lang_hooks.decl_printable_name (current_function_decl, 2);
	  EXECUTE_IF_SET_IN_BITMAP (new_irr, 0, i, bmi)
	    fprintf (dump_file, "%s: bb %d goes irrevocable\n", dname, i);
	}
    }
  else
    BITMAP_FREE (new_irr);

  pop_cfun ();

  return ret;
}

/* Return true if, for the transactional clone of NODE, any call
   may enter irrevocable mode.  */

static bool
ipa_tm_mayenterirr_function (struct cgraph_node *node)
{
  struct tm_ipa_cg_data *d;
  tree decl;
  unsigned flags;

  d = get_cg_data (&node, true);
  decl = node->decl;
  flags = flags_from_decl_or_type (decl);

  /* Handle some TM builtins.  Ordinarily these aren't actually generated
     at this point, but handling these functions when written in by the
     user makes it easier to build unit tests.  */
  if (flags & ECF_TM_BUILTIN)
    return false;

  /* Filter out all functions that are marked.  */
  if (flags & ECF_TM_PURE)
    return false;
  if (is_tm_safe (decl))
    return false;
  if (is_tm_irrevocable (decl))
    return true;
  if (is_tm_callable (decl))
    return true;
  if (find_tm_replacement_function (decl))
    return true;

  /* If we aren't seeing the final version of the function we don't
     know what it will contain at runtime.  */
  if (cgraph_function_body_availability (node) < AVAIL_AVAILABLE)
    return true;

  /* If the function must go irrevocable, then of course true.  */
  if (d->is_irrevocable)
    return true;

  /* If there are any blocks marked irrevocable, then the function
     as a whole may enter irrevocable.  */
  if (d->irrevocable_blocks_clone)
    return true;

  /* We may have previously marked this function as tm_may_enter_irr;
     see pass_diagnose_tm_blocks.  */
  if (node->local.tm_may_enter_irr)
    return true;

  /* Recurse on the main body for aliases.  In general, this will
     result in one of the bits above being set so that we will not
     have to recurse next time.  */
  if (node->alias)
    return ipa_tm_mayenterirr_function (cgraph_get_node (node->thunk.alias));

  /* What remains is unmarked local functions without items that force
     the function to go irrevocable.  */
  return false;
}

/* Diagnose calls from transaction_safe functions to unmarked
   functions that are determined to not be safe.  */

static void
ipa_tm_diagnose_tm_safe (struct cgraph_node *node)
{
  struct cgraph_edge *e;

  for (e = node->callees; e ; e = e->next_callee)
    if (!is_tm_callable (e->callee->decl)
	&& e->callee->local.tm_may_enter_irr)
      error_at (gimple_location (e->call_stmt),
		"unsafe function call %qD within "
		"%<transaction_safe%> function", e->callee->decl);
}

/* Diagnose call from atomic transactions to unmarked functions
   that are determined to not be safe.  */

static void
ipa_tm_diagnose_transaction (struct cgraph_node *node,
			   struct tm_region *all_tm_regions)
{
  struct tm_region *r;

  for (r = all_tm_regions; r ; r = r->next)
    if (gimple_transaction_subcode (r->transaction_stmt) & GTMA_IS_RELAXED)
      {
	/* Atomic transactions can be nested inside relaxed.  */
	if (r->inner)
	  ipa_tm_diagnose_transaction (node, r->inner);
      }
    else
      {
	vec<basic_block> bbs;
	gimple_stmt_iterator gsi;
	basic_block bb;
	size_t i;

	bbs = get_tm_region_blocks (r->entry_block, r->exit_blocks,
				    r->irr_blocks, NULL, false);

	for (i = 0; bbs.iterate (i, &bb); ++i)
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
	      tree fndecl;

	      if (gimple_code (stmt) == GIMPLE_ASM)
		{
		  error_at (gimple_location (stmt),
			    "asm not allowed in atomic transaction");
		  continue;
		}

	      if (!is_gimple_call (stmt))
		continue;
	      fndecl = gimple_call_fndecl (stmt);

	      /* Indirect function calls have been diagnosed already.  */
	      if (!fndecl)
		continue;

	      /* Stop at the end of the transaction.  */
	      if (is_tm_ending_fndecl (fndecl))
		{
		  if (bitmap_bit_p (r->exit_blocks, bb->index))
		    break;
		  continue;
		}

	      /* Marked functions have been diagnosed already.  */
	      if (is_tm_pure_call (stmt))
		continue;
	      if (is_tm_callable (fndecl))
		continue;

	      if (cgraph_local_info (fndecl)->tm_may_enter_irr)
		error_at (gimple_location (stmt),
			  "unsafe function call %qD within "
			  "atomic transaction", fndecl);
	    }

	bbs.release ();
      }
}

/* Return a transactional mangled name for the DECL_ASSEMBLER_NAME in
   OLD_DECL.  The returned value is a freshly malloced pointer that
   should be freed by the caller.  */

static tree
tm_mangle (tree old_asm_id)
{
  const char *old_asm_name;
  char *tm_name;
  void *alloc = NULL;
  struct demangle_component *dc;
  tree new_asm_id;

  /* Determine if the symbol is already a valid C++ mangled name.  Do this
     even for C, which might be interfacing with C++ code via appropriately
     ugly identifiers.  */
  /* ??? We could probably do just as well checking for "_Z" and be done.  */
  old_asm_name = IDENTIFIER_POINTER (old_asm_id);
  dc = cplus_demangle_v3_components (old_asm_name, DMGL_NO_OPTS, &alloc);

  if (dc == NULL)
    {
      char length[8];

    do_unencoded:
      sprintf (length, "%u", IDENTIFIER_LENGTH (old_asm_id));
      tm_name = concat ("_ZGTt", length, old_asm_name, NULL);
    }
  else
    {
      old_asm_name += 2;	/* Skip _Z */

      switch (dc->type)
	{
	case DEMANGLE_COMPONENT_TRANSACTION_CLONE:
	case DEMANGLE_COMPONENT_NONTRANSACTION_CLONE:
	  /* Don't play silly games, you!  */
	  goto do_unencoded;

	case DEMANGLE_COMPONENT_HIDDEN_ALIAS:
	  /* I'd really like to know if we can ever be passed one of
	     these from the C++ front end.  The Logical Thing would
	     seem that hidden-alias should be outer-most, so that we
	     get hidden-alias of a transaction-clone and not vice-versa.  */
	  old_asm_name += 2;
	  break;

	default:
	  break;
	}

      tm_name = concat ("_ZGTt", old_asm_name, NULL);
    }
  free (alloc);

  new_asm_id = get_identifier (tm_name);
  free (tm_name);

  return new_asm_id;
}

static inline void
ipa_tm_mark_force_output_node (struct cgraph_node *node)
{
  cgraph_mark_force_output_node (node);
  node->analyzed = true;
}

static inline void
ipa_tm_mark_forced_by_abi_node (struct cgraph_node *node)
{
  node->forced_by_abi = true;
  node->analyzed = true;
}

/* Callback data for ipa_tm_create_version_alias.  */
struct create_version_alias_info
{
  struct cgraph_node *old_node;
  tree new_decl;
};

/* A subroutine of ipa_tm_create_version, called via
   cgraph_for_node_and_aliases.  Create new tm clones for each of
   the existing aliases.  */
static bool
ipa_tm_create_version_alias (struct cgraph_node *node, void *data)
{
  struct create_version_alias_info *info
    = (struct create_version_alias_info *)data;
  tree old_decl, new_decl, tm_name;
  struct cgraph_node *new_node;

  if (!node->cpp_implicit_alias)
    return false;

  old_decl = node->decl;
  tm_name = tm_mangle (DECL_ASSEMBLER_NAME (old_decl));
  new_decl = build_decl (DECL_SOURCE_LOCATION (old_decl),
			 TREE_CODE (old_decl), tm_name,
			 TREE_TYPE (old_decl));

  SET_DECL_ASSEMBLER_NAME (new_decl, tm_name);
  SET_DECL_RTL (new_decl, NULL);

  /* Based loosely on C++'s make_alias_for().  */
  TREE_PUBLIC (new_decl) = TREE_PUBLIC (old_decl);
  DECL_CONTEXT (new_decl) = DECL_CONTEXT (old_decl);
  DECL_LANG_SPECIFIC (new_decl) = DECL_LANG_SPECIFIC (old_decl);
  TREE_READONLY (new_decl) = TREE_READONLY (old_decl);
  DECL_EXTERNAL (new_decl) = 0;
  DECL_ARTIFICIAL (new_decl) = 1;
  TREE_ADDRESSABLE (new_decl) = 1;
  TREE_USED (new_decl) = 1;
  TREE_SYMBOL_REFERENCED (tm_name) = 1;

  /* Perform the same remapping to the comdat group.  */
  if (DECL_ONE_ONLY (new_decl))
    DECL_COMDAT_GROUP (new_decl) = tm_mangle (DECL_COMDAT_GROUP (old_decl));

  new_node = cgraph_same_body_alias (NULL, new_decl, info->new_decl);
  new_node->tm_clone = true;
  new_node->externally_visible = info->old_node->externally_visible;
  /* ?? Do not traverse aliases here.  */
  get_cg_data (&node, false)->clone = new_node;

  record_tm_clone_pair (old_decl, new_decl);

  if (info->old_node->force_output
      || ipa_ref_list_first_referring (&info->old_node->ref_list))
    ipa_tm_mark_force_output_node (new_node);
  if (info->old_node->forced_by_abi)
    ipa_tm_mark_forced_by_abi_node (new_node);
  return false;
}

/* Create a copy of the function (possibly declaration only) of OLD_NODE,
   appropriate for the transactional clone.  */

static void
ipa_tm_create_version (struct cgraph_node *old_node)
{
  tree new_decl, old_decl, tm_name;
  struct cgraph_node *new_node;

  old_decl = old_node->decl;
  new_decl = copy_node (old_decl);

  /* DECL_ASSEMBLER_NAME needs to be set before we call
     cgraph_copy_node_for_versioning below, because cgraph_node will
     fill the assembler_name_hash.  */
  tm_name = tm_mangle (DECL_ASSEMBLER_NAME (old_decl));
  SET_DECL_ASSEMBLER_NAME (new_decl, tm_name);
  SET_DECL_RTL (new_decl, NULL);
  TREE_SYMBOL_REFERENCED (tm_name) = 1;

  /* Perform the same remapping to the comdat group.  */
  if (DECL_ONE_ONLY (new_decl))
    DECL_COMDAT_GROUP (new_decl) = tm_mangle (DECL_COMDAT_GROUP (old_decl));

  gcc_assert (!old_node->ipa_transforms_to_apply.exists ());
  new_node = cgraph_copy_node_for_versioning (old_node, new_decl, vNULL, NULL);
  new_node->local.local = false;
  new_node->externally_visible = old_node->externally_visible;
  new_node->lowered = true;
  new_node->tm_clone = 1;
  get_cg_data (&old_node, true)->clone = new_node;

  if (cgraph_function_body_availability (old_node) >= AVAIL_OVERWRITABLE)
    {
      /* Remap extern inline to static inline.  */
      /* ??? Is it worth trying to use make_decl_one_only?  */
      if (DECL_DECLARED_INLINE_P (new_decl) && DECL_EXTERNAL (new_decl))
	{
	  DECL_EXTERNAL (new_decl) = 0;
	  TREE_PUBLIC (new_decl) = 0;
	  DECL_WEAK (new_decl) = 0;
	}

      tree_function_versioning (old_decl, new_decl,
				NULL, false, NULL,
				false, NULL, NULL);
    }

  record_tm_clone_pair (old_decl, new_decl);

  cgraph_call_function_insertion_hooks (new_node);
  if (old_node->force_output
      || ipa_ref_list_first_referring (&old_node->ref_list))
    ipa_tm_mark_force_output_node (new_node);
  if (old_node->forced_by_abi)
    ipa_tm_mark_forced_by_abi_node (new_node);

  /* Do the same thing, but for any aliases of the original node.  */
  {
    struct create_version_alias_info data;
    data.old_node = old_node;
    data.new_decl = new_decl;
    cgraph_for_node_and_aliases (old_node, ipa_tm_create_version_alias,
				 &data, true);
  }
}

/* Construct a call to TM_IRREVOCABLE and insert it at the beginning of BB.  */

static void
ipa_tm_insert_irr_call (struct cgraph_node *node, struct tm_region *region,
			basic_block bb)
{
  gimple_stmt_iterator gsi;
  gimple g;

  transaction_subcode_ior (region, GTMA_MAY_ENTER_IRREVOCABLE);

  g = gimple_build_call (builtin_decl_explicit (BUILT_IN_TM_IRREVOCABLE),
			 1, build_int_cst (NULL_TREE, MODE_SERIALIRREVOCABLE));

  split_block_after_labels (bb);
  gsi = gsi_after_labels (bb);
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);

  cgraph_create_edge (node,
	       cgraph_get_create_node
		  (builtin_decl_explicit (BUILT_IN_TM_IRREVOCABLE)),
		      g, 0,
		      compute_call_stmt_bb_frequency (node->decl,
						      gimple_bb (g)));
}

/* Construct a call to TM_GETTMCLONE and insert it before GSI.  */

static bool
ipa_tm_insert_gettmclone_call (struct cgraph_node *node,
			       struct tm_region *region,
			       gimple_stmt_iterator *gsi, gimple stmt)
{
  tree gettm_fn, ret, old_fn, callfn;
  gimple g, g2;
  bool safe;

  old_fn = gimple_call_fn (stmt);

  if (TREE_CODE (old_fn) == ADDR_EXPR)
    {
      tree fndecl = TREE_OPERAND (old_fn, 0);
      tree clone = get_tm_clone_pair (fndecl);

      /* By transforming the call into a TM_GETTMCLONE, we are
	 technically taking the address of the original function and
	 its clone.  Explain this so inlining will know this function
	 is needed.  */
      cgraph_mark_address_taken_node (cgraph_get_node (fndecl));
      if (clone)
	cgraph_mark_address_taken_node (cgraph_get_node (clone));
    }

  safe = is_tm_safe (TREE_TYPE (old_fn));
  gettm_fn = builtin_decl_explicit (safe ? BUILT_IN_TM_GETTMCLONE_SAFE
				    : BUILT_IN_TM_GETTMCLONE_IRR);
  ret = create_tmp_var (ptr_type_node, NULL);

  if (!safe)
    transaction_subcode_ior (region, GTMA_MAY_ENTER_IRREVOCABLE);

  /* Discard OBJ_TYPE_REF, since we weren't able to fold it.  */
  if (TREE_CODE (old_fn) == OBJ_TYPE_REF)
    old_fn = OBJ_TYPE_REF_EXPR (old_fn);

  g = gimple_build_call (gettm_fn, 1, old_fn);
  ret = make_ssa_name (ret, g);
  gimple_call_set_lhs (g, ret);

  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  cgraph_create_edge (node, cgraph_get_create_node (gettm_fn), g, 0,
		      compute_call_stmt_bb_frequency (node->decl,
						      gimple_bb (g)));

  /* Cast return value from tm_gettmclone* into appropriate function
     pointer.  */
  callfn = create_tmp_var (TREE_TYPE (old_fn), NULL);
  g2 = gimple_build_assign (callfn,
			    fold_build1 (NOP_EXPR, TREE_TYPE (callfn), ret));
  callfn = make_ssa_name (callfn, g2);
  gimple_assign_set_lhs (g2, callfn);
  gsi_insert_before (gsi, g2, GSI_SAME_STMT);

  /* ??? This is a hack to preserve the NOTHROW bit on the call,
     which we would have derived from the decl.  Failure to save
     this bit means we might have to split the basic block.  */
  if (gimple_call_nothrow_p (stmt))
    gimple_call_set_nothrow (stmt, true);

  gimple_call_set_fn (stmt, callfn);

  /* Discarding OBJ_TYPE_REF above may produce incompatible LHS and RHS
     for a call statement.  Fix it.  */
  {
    tree lhs = gimple_call_lhs (stmt);
    tree rettype = TREE_TYPE (gimple_call_fntype (stmt));
    if (lhs
	&& !useless_type_conversion_p (TREE_TYPE (lhs), rettype))
    {
      tree temp;

      temp = create_tmp_reg (rettype, 0);
      gimple_call_set_lhs (stmt, temp);

      g2 = gimple_build_assign (lhs,
				fold_build1 (VIEW_CONVERT_EXPR,
					     TREE_TYPE (lhs), temp));
      gsi_insert_after (gsi, g2, GSI_SAME_STMT);
    }
  }

  update_stmt (stmt);

  return true;
}

/* Helper function for ipa_tm_transform_calls*.  Given a call
   statement in GSI which resides inside transaction REGION, redirect
   the call to either its wrapper function, or its clone.  */

static void
ipa_tm_transform_calls_redirect (struct cgraph_node *node,
				 struct tm_region *region,
				 gimple_stmt_iterator *gsi,
				 bool *need_ssa_rename_p)
{
  gimple stmt = gsi_stmt (*gsi);
  struct cgraph_node *new_node;
  struct cgraph_edge *e = cgraph_edge (node, stmt);
  tree fndecl = gimple_call_fndecl (stmt);

  /* For indirect calls, pass the address through the runtime.  */
  if (fndecl == NULL)
    {
      *need_ssa_rename_p |=
	ipa_tm_insert_gettmclone_call (node, region, gsi, stmt);
      return;
    }

  /* Handle some TM builtins.  Ordinarily these aren't actually generated
     at this point, but handling these functions when written in by the
     user makes it easier to build unit tests.  */
  if (flags_from_decl_or_type (fndecl) & ECF_TM_BUILTIN)
    return;

  /* Fixup recursive calls inside clones.  */
  /* ??? Why did cgraph_copy_node_for_versioning update the call edges
     for recursion but not update the call statements themselves?  */
  if (e->caller == e->callee && decl_is_tm_clone (current_function_decl))
    {
      gimple_call_set_fndecl (stmt, current_function_decl);
      return;
    }

  /* If there is a replacement, use it.  */
  fndecl = find_tm_replacement_function (fndecl);
  if (fndecl)
    {
      new_node = cgraph_get_create_node (fndecl);

      /* ??? Mark all transaction_wrap functions tm_may_enter_irr.

	 We can't do this earlier in record_tm_replacement because
	 cgraph_remove_unreachable_nodes is called before we inject
	 references to the node.  Further, we can't do this in some
	 nice central place in ipa_tm_execute because we don't have
	 the exact list of wrapper functions that would be used.
	 Marking more wrappers than necessary results in the creation
	 of unnecessary cgraph_nodes, which can cause some of the
	 other IPA passes to crash.

	 We do need to mark these nodes so that we get the proper
	 result in expand_call_tm.  */
      /* ??? This seems broken.  How is it that we're marking the
	 CALLEE as may_enter_irr?  Surely we should be marking the
	 CALLER.  Also note that find_tm_replacement_function also
	 contains mappings into the TM runtime, e.g. memcpy.  These
	 we know won't go irrevocable.  */
      new_node->local.tm_may_enter_irr = 1;
    }
  else
    {
      struct tm_ipa_cg_data *d;
      struct cgraph_node *tnode = e->callee;

      d = get_cg_data (&tnode, true);
      new_node = d->clone;

      /* As we've already skipped pure calls and appropriate builtins,
	 and we've already marked irrevocable blocks, if we can't come
	 up with a static replacement, then ask the runtime.  */
      if (new_node == NULL)
	{
	  *need_ssa_rename_p |=
	    ipa_tm_insert_gettmclone_call (node, region, gsi, stmt);
	  return;
	}

      fndecl = new_node->decl;
    }

  cgraph_redirect_edge_callee (e, new_node);
  gimple_call_set_fndecl (stmt, fndecl);
}

/* Helper function for ipa_tm_transform_calls.  For a given BB,
   install calls to tm_irrevocable when IRR_BLOCKS are reached,
   redirect other calls to the generated transactional clone.  */

static bool
ipa_tm_transform_calls_1 (struct cgraph_node *node, struct tm_region *region,
			  basic_block bb, bitmap irr_blocks)
{
  gimple_stmt_iterator gsi;
  bool need_ssa_rename = false;

  if (irr_blocks && bitmap_bit_p (irr_blocks, bb->index))
    {
      ipa_tm_insert_irr_call (node, region, bb);
      return true;
    }

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (!is_gimple_call (stmt))
	continue;
      if (is_tm_pure_call (stmt))
	continue;

      /* Redirect edges to the appropriate replacement or clone.  */
      ipa_tm_transform_calls_redirect (node, region, &gsi, &need_ssa_rename);
    }

  return need_ssa_rename;
}

/* Walk the CFG for REGION, beginning at BB.  Install calls to
   tm_irrevocable when IRR_BLOCKS are reached, redirect other calls to
   the generated transactional clone.  */

static bool
ipa_tm_transform_calls (struct cgraph_node *node, struct tm_region *region,
			basic_block bb, bitmap irr_blocks)
{
  bool need_ssa_rename = false;
  edge e;
  edge_iterator ei;
  auto_vec<basic_block> queue;
  bitmap visited_blocks = BITMAP_ALLOC (NULL);

  queue.safe_push (bb);
  do
    {
      bb = queue.pop ();

      need_ssa_rename |=
	ipa_tm_transform_calls_1 (node, region, bb, irr_blocks);

      if (irr_blocks && bitmap_bit_p (irr_blocks, bb->index))
	continue;

      if (region && bitmap_bit_p (region->exit_blocks, bb->index))
	continue;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!bitmap_bit_p (visited_blocks, e->dest->index))
	  {
	    bitmap_set_bit (visited_blocks, e->dest->index);
	    queue.safe_push (e->dest);
	  }
    }
  while (!queue.is_empty ());

  BITMAP_FREE (visited_blocks);

  return need_ssa_rename;
}

/* Transform the calls within the TM regions within NODE.  */

static void
ipa_tm_transform_transaction (struct cgraph_node *node)
{
  struct tm_ipa_cg_data *d;
  struct tm_region *region;
  bool need_ssa_rename = false;

  d = get_cg_data (&node, true);

  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  calculate_dominance_info (CDI_DOMINATORS);

  for (region = d->all_tm_regions; region; region = region->next)
    {
      /* If we're sure to go irrevocable, don't transform anything.  */
      if (d->irrevocable_blocks_normal
	  && bitmap_bit_p (d->irrevocable_blocks_normal,
			   region->entry_block->index))
	{
	  transaction_subcode_ior (region, GTMA_DOES_GO_IRREVOCABLE
				           | GTMA_MAY_ENTER_IRREVOCABLE
				   	   | GTMA_HAS_NO_INSTRUMENTATION);
	  continue;
	}

      need_ssa_rename |=
	ipa_tm_transform_calls (node, region, region->entry_block,
				d->irrevocable_blocks_normal);
    }

  if (need_ssa_rename)
    update_ssa (TODO_update_ssa_only_virtuals);

  pop_cfun ();
}

/* Transform the calls within the transactional clone of NODE.  */

static void
ipa_tm_transform_clone (struct cgraph_node *node)
{
  struct tm_ipa_cg_data *d;
  bool need_ssa_rename;

  d = get_cg_data (&node, true);

  /* If this function makes no calls and has no irrevocable blocks,
     then there's nothing to do.  */
  /* ??? Remove non-aborting top-level transactions.  */
  if (!node->callees && !node->indirect_calls && !d->irrevocable_blocks_clone)
    return;

  push_cfun (DECL_STRUCT_FUNCTION (d->clone->decl));
  calculate_dominance_info (CDI_DOMINATORS);

  need_ssa_rename =
    ipa_tm_transform_calls (d->clone, NULL,
			    single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
			    d->irrevocable_blocks_clone);

  if (need_ssa_rename)
    update_ssa (TODO_update_ssa_only_virtuals);

  pop_cfun ();
}

/* Main entry point for the transactional memory IPA pass.  */

static unsigned int
ipa_tm_execute (void)
{
  cgraph_node_queue tm_callees = cgraph_node_queue ();
  /* List of functions that will go irrevocable.  */
  cgraph_node_queue irr_worklist = cgraph_node_queue ();

  struct cgraph_node *node;
  struct tm_ipa_cg_data *d;
  enum availability a;
  unsigned int i;

#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif

  bitmap_obstack_initialize (&tm_obstack);
  initialize_original_copy_tables ();

  /* For all local functions marked tm_callable, queue them.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    if (is_tm_callable (node->decl)
	&& cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      {
	d = get_cg_data (&node, true);
	maybe_push_queue (node, &tm_callees, &d->in_callee_queue);
      }

  /* For all local reachable functions...  */
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->lowered
	&& cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      {
	/* ... marked tm_pure, record that fact for the runtime by
	   indicating that the pure function is its own tm_callable.
	   No need to do this if the function's address can't be taken.  */
	if (is_tm_pure (node->decl))
	  {
	    if (!node->local.local)
	      record_tm_clone_pair (node->decl, node->decl);
	    continue;
	  }

	push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	calculate_dominance_info (CDI_DOMINATORS);

	tm_region_init (NULL);
	if (all_tm_regions)
	  {
	    d = get_cg_data (&node, true);

	    /* Scan for calls that are in each transaction, and
	       generate the uninstrumented code path.  */
	    ipa_tm_scan_calls_transaction (d, &tm_callees);

	    /* Put it in the worklist so we can scan the function
	       later (ipa_tm_scan_irr_function) and mark the
	       irrevocable blocks.  */
	    maybe_push_queue (node, &irr_worklist, &d->in_worklist);
	    d->want_irr_scan_normal = true;
	  }

	pop_cfun ();
      }

  /* For every local function on the callee list, scan as if we will be
     creating a transactional clone, queueing all new functions we find
     along the way.  */
  for (i = 0; i < tm_callees.length (); ++i)
    {
      node = tm_callees[i];
      a = cgraph_function_body_availability (node);
      d = get_cg_data (&node, true);

      /* Put it in the worklist so we can scan the function later
	 (ipa_tm_scan_irr_function) and mark the irrevocable
	 blocks.  */
      maybe_push_queue (node, &irr_worklist, &d->in_worklist);

      /* Some callees cannot be arbitrarily cloned.  These will always be
	 irrevocable.  Mark these now, so that we need not scan them.  */
      if (is_tm_irrevocable (node->decl))
	ipa_tm_note_irrevocable (node, &irr_worklist);
      else if (a <= AVAIL_NOT_AVAILABLE
	       && !is_tm_safe_or_pure (node->decl))
	ipa_tm_note_irrevocable (node, &irr_worklist);
      else if (a >= AVAIL_OVERWRITABLE)
	{
	  if (!tree_versionable_function_p (node->decl))
	    ipa_tm_note_irrevocable (node, &irr_worklist);
	  else if (!d->is_irrevocable)
	    {
	      /* If this is an alias, make sure its base is queued as well.
		 we need not scan the callees now, as the base will do.  */
	      if (node->alias)
		{
		  node = cgraph_get_node (node->thunk.alias);
		  d = get_cg_data (&node, true);
		  maybe_push_queue (node, &tm_callees, &d->in_callee_queue);
		  continue;
		}

	      /* Add all nodes called by this function into
		 tm_callees as well.  */
	      ipa_tm_scan_calls_clone (node, &tm_callees);
	    }
	}
    }

  /* Iterate scans until no more work to be done.  Prefer not to use
     vec::pop because the worklist tends to follow a breadth-first
     search of the callgraph, which should allow convergance with a
     minimum number of scans.  But we also don't want the worklist
     array to grow without bound, so we shift the array up periodically.  */
  for (i = 0; i < irr_worklist.length (); ++i)
    {
      if (i > 256 && i == irr_worklist.length () / 8)
	{
	  irr_worklist.block_remove (0, i);
	  i = 0;
	}

      node = irr_worklist[i];
      d = get_cg_data (&node, true);
      d->in_worklist = false;

      if (d->want_irr_scan_normal)
	{
	  d->want_irr_scan_normal = false;
	  ipa_tm_scan_irr_function (node, false);
	}
      if (d->in_callee_queue && ipa_tm_scan_irr_function (node, true))
	ipa_tm_note_irrevocable (node, &irr_worklist);
    }

  /* For every function on the callee list, collect the tm_may_enter_irr
     bit on the node.  */
  irr_worklist.truncate (0);
  for (i = 0; i < tm_callees.length (); ++i)
    {
      node = tm_callees[i];
      if (ipa_tm_mayenterirr_function (node))
	{
	  d = get_cg_data (&node, true);
	  gcc_assert (d->in_worklist == false);
	  maybe_push_queue (node, &irr_worklist, &d->in_worklist);
	}
    }

  /* Propagate the tm_may_enter_irr bit to callers until stable.  */
  for (i = 0; i < irr_worklist.length (); ++i)
    {
      struct cgraph_node *caller;
      struct cgraph_edge *e;
      struct ipa_ref *ref;
      unsigned j;

      if (i > 256 && i == irr_worklist.length () / 8)
	{
	  irr_worklist.block_remove (0, i);
	  i = 0;
	}

      node = irr_worklist[i];
      d = get_cg_data (&node, true);
      d->in_worklist = false;
      node->local.tm_may_enter_irr = true;

      /* Propagate back to normal callers.  */
      for (e = node->callers; e ; e = e->next_caller)
	{
	  caller = e->caller;
	  if (!is_tm_safe_or_pure (caller->decl)
	      && !caller->local.tm_may_enter_irr)
	    {
	      d = get_cg_data (&caller, true);
	      maybe_push_queue (caller, &irr_worklist, &d->in_worklist);
	    }
	}

      /* Propagate back to referring aliases as well.  */
      for (j = 0; ipa_ref_list_referring_iterate (&node->ref_list, j, ref); j++)
	{
	  caller = cgraph (ref->referring);
	  if (ref->use == IPA_REF_ALIAS
	      && !caller->local.tm_may_enter_irr)
	    {
	      /* ?? Do not traverse aliases here.  */
	      d = get_cg_data (&caller, false);
	      maybe_push_queue (caller, &irr_worklist, &d->in_worklist);
	    }
	}
    }

  /* Now validate all tm_safe functions, and all atomic regions in
     other functions.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->lowered
	&& cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      {
	d = get_cg_data (&node, true);
	if (is_tm_safe (node->decl))
	  ipa_tm_diagnose_tm_safe (node);
	else if (d->all_tm_regions)
	  ipa_tm_diagnose_transaction (node, d->all_tm_regions);
      }

  /* Create clones.  Do those that are not irrevocable and have a
     positive call count.  Do those publicly visible functions that
     the user directed us to clone.  */
  for (i = 0; i < tm_callees.length (); ++i)
    {
      bool doit = false;

      node = tm_callees[i];
      if (node->cpp_implicit_alias)
	continue;

      a = cgraph_function_body_availability (node);
      d = get_cg_data (&node, true);

      if (a <= AVAIL_NOT_AVAILABLE)
	doit = is_tm_callable (node->decl);
      else if (a <= AVAIL_AVAILABLE && is_tm_callable (node->decl))
	doit = true;
      else if (!d->is_irrevocable
	       && d->tm_callers_normal + d->tm_callers_clone > 0)
	doit = true;

      if (doit)
	ipa_tm_create_version (node);
    }

  /* Redirect calls to the new clones, and insert irrevocable marks.  */
  for (i = 0; i < tm_callees.length (); ++i)
    {
      node = tm_callees[i];
      if (node->analyzed)
	{
	  d = get_cg_data (&node, true);
	  if (d->clone)
	    ipa_tm_transform_clone (node);
	}
    }
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->lowered
	&& cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      {
	d = get_cg_data (&node, true);
	if (d->all_tm_regions)
	  ipa_tm_transform_transaction (node);
      }

  /* Free and clear all data structures.  */
  tm_callees.release ();
  irr_worklist.release ();
  bitmap_obstack_release (&tm_obstack);
  free_original_copy_tables ();

  FOR_EACH_FUNCTION (node)
    node->aux = NULL;

#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif

  return 0;
}

namespace {

const pass_data pass_data_ipa_tm =
{
  SIMPLE_IPA_PASS, /* type */
  "tmipa", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRANS_MEM, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_tm : public simple_ipa_opt_pass
{
public:
  pass_ipa_tm (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_tm, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tm; }
  virtual unsigned int execute (function *) { return ipa_tm_execute (); }

}; // class pass_ipa_tm

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_tm (gcc::context *ctxt)
{
  return new pass_ipa_tm (ctxt);
}

#include "gt-trans-mem.h"
