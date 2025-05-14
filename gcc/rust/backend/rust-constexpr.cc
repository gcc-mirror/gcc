// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-constexpr.h"
#include "rust-location.h"
#include "rust-diagnostics.h"
#include "rust-tree.h"
#include "fold-const.h"
#include "realmpfr.h"
#include "convert.h"
#include "print-tree.h"
#include "gimplify.h"
#include "tree-iterator.h"
#include "timevar.h"
#include "varasm.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "vec.h"
#include "function.h"
#include "diagnostic.h"
#include "target.h"
#include "builtins.h"

#define VERIFY_CONSTANT(X)                                                     \
  do                                                                           \
    {                                                                          \
      if (verify_constant ((X), ctx->quiet, non_constant_p, overflow_p))       \
	return t;                                                              \
    }                                                                          \
  while (0)

namespace Rust {
namespace Compile {

/* Returns true iff FUN is an instantiation of a constexpr function
 template or a defaulted constexpr function.  */

bool
is_instantiation_of_constexpr (tree fun)
{
  return DECL_DECLARED_CONSTEXPR_P (fun);
}

/* Return true if T is a literal type.   */

bool
literal_type_p (tree t)
{
  if (SCALAR_TYPE_P (t) || VECTOR_TYPE_P (t) || TYPE_REF_P (t)
      || (VOID_TYPE_P (t)))
    return true;

  if (TREE_CODE (t) == ARRAY_TYPE)
    return literal_type_p (strip_array_types (t));
  return false;
}

static bool
verify_constant (tree, bool, bool *, bool *);

static HOST_WIDE_INT
find_array_ctor_elt (tree ary, tree dindex, bool insert = false);
static int
array_index_cmp (tree key, tree index);
static bool
potential_constant_expression_1 (tree t, bool want_rval, bool strict, bool now,
				 tsubst_flags_t flags, tree *jump_target);
bool
potential_constant_expression_1 (tree t, bool want_rval, bool strict, bool now,
				 tsubst_flags_t flags);
tree
unshare_constructor (tree t MEM_STAT_DECL);
void
maybe_save_constexpr_fundef (tree fun);

static bool
returns (tree *jump_target);
static bool
breaks (tree *jump_target);
static bool
continues (tree *jump_target);
static bool
switches (tree *jump_target);

struct constexpr_global_ctx
{
  /* Values for any temporaries or local variables within the
     constant-expression. */
  hash_map<tree, tree> values;
  /* Number of cxx_eval_constant_expression calls (except skipped ones,
     on simple constants or location wrappers) encountered during current
     cxx_eval_outermost_constant_expr call.  */
  HOST_WIDE_INT constexpr_ops_count;
  /* Heap VAR_DECLs created during the evaluation of the outermost constant
     expression.  */
  auto_vec<tree, 16> heap_vars;
  /* Cleanups that need to be evaluated at the end of CLEANUP_POINT_EXPR.  */
  vec<tree> *cleanups;
  /* Number of heap VAR_DECL deallocations.  */
  unsigned heap_dealloc_count;
  /* Constructor.  */
  constexpr_global_ctx ()
    : constexpr_ops_count (0), cleanups (NULL), heap_dealloc_count (0)
  {}
};

/* In constexpr.cc */
/* Representation of entries in the constexpr function definition table.  */

struct GTY ((for_user)) rust_constexpr_fundef
{
  tree decl;
  tree body;
  tree parms;
  tree result;
};

/* Objects of this type represent calls to constexpr functions
 along with the bindings of parameters to their arguments, for
 the purpose of compile time evaluation.  */

struct GTY ((for_user)) rust_constexpr_call
{
  /* Description of the constexpr function definition.  */
  rust_constexpr_fundef *fundef;
  /* Parameter bindings environment.  A TREE_VEC of arguments.  */
  tree bindings;
  /* Result of the call.
       NULL means the call is being evaluated.
       error_mark_node means that the evaluation was erroneous;
       otherwise, the actual value of the call.  */
  tree result;
  /* The hash of this call; we remember it here to avoid having to
     recalculate it when expanding the hash table.  */
  hashval_t hash;
  /* Whether __builtin_is_constant_evaluated() should evaluate to true.  */
  bool manifestly_const_eval;
};

struct rust_constexpr_call_hasher : ggc_ptr_hash<rust_constexpr_call>
{
  static hashval_t hash (rust_constexpr_call *);
  static bool equal (rust_constexpr_call *, rust_constexpr_call *);
};

enum constexpr_switch_state
{
  /* Used when processing a switch for the first time by cxx_eval_switch_expr
     and default: label for that switch has not been seen yet.  */
  css_default_not_seen,
  /* Used when processing a switch for the first time by cxx_eval_switch_expr
     and default: label for that switch has been seen already.  */
  css_default_seen,
  /* Used when processing a switch for the second time by
     cxx_eval_switch_expr, where default: label should match.  */
  css_default_processing
};

struct constexpr_ctx
{
  /* The part of the context that needs to be unique to the whole
     cxx_eval_outermost_constant_expr invocation.  */
  constexpr_global_ctx *global;
  /* The innermost call we're evaluating.  */
  rust_constexpr_call *call;
  /* SAVE_EXPRs and TARGET_EXPR_SLOT vars of TARGET_EXPRs that we've seen
     within the current LOOP_EXPR.  NULL if we aren't inside a loop.  */
  vec<tree> *save_exprs;
  /* The CONSTRUCTOR we're currently building up for an aggregate
     initializer.  */
  tree ctor;
  /* The object we're building the CONSTRUCTOR for.  */
  tree object;
  /* If inside SWITCH_EXPR.  */
  constexpr_switch_state *css_state;
  /* The aggregate initialization context inside which this one is nested.  This
     is used by lookup_placeholder to resolve PLACEHOLDER_EXPRs.  */
  const constexpr_ctx *parent;

  /* Whether we should error on a non-constant expression or fail quietly.
     This flag needs to be here, but some of the others could move to global
     if they get larger than a word.  */
  bool quiet;
  /* Whether we are strictly conforming to constant expression rules or
     trying harder to get a constant value.  */
  bool strict;
  /* Whether __builtin_is_constant_evaluated () should be true.  */
  bool manifestly_const_eval;
};

struct rust_constexpr_fundef_hasher : ggc_ptr_hash<rust_constexpr_fundef>
{
  static hashval_t hash (const rust_constexpr_fundef *);
  static bool equal (const rust_constexpr_fundef *,
		     const rust_constexpr_fundef *);
};

/* This table holds all constexpr function definitions seen in
   the current translation unit.  */

static GTY (())
  hash_table<rust_constexpr_fundef_hasher> *constexpr_fundef_table;

/* Utility function used for managing the constexpr function table.
   Return true if the entries pointed to by P and Q are for the
   same constexpr function.  */

inline bool
rust_constexpr_fundef_hasher::equal (const rust_constexpr_fundef *lhs,
				     const rust_constexpr_fundef *rhs)
{
  return lhs->decl == rhs->decl;
}

/* Utility function used for managing the constexpr function table.
   Return a hash value for the entry pointed to by Q.  */

inline hashval_t
rust_constexpr_fundef_hasher::hash (const rust_constexpr_fundef *fundef)
{
  return DECL_UID (fundef->decl);
}

/* Return a previously saved definition of function FUN.   */

rust_constexpr_fundef *
retrieve_constexpr_fundef (tree fun)
{
  if (constexpr_fundef_table == NULL)
    return NULL;

  rust_constexpr_fundef fundef = {fun, NULL_TREE, NULL_TREE, NULL_TREE};
  return constexpr_fundef_table->find (&fundef);
}

/* This internal flag controls whether we should avoid doing anything during
   constexpr evaluation that would cause extra DECL_UID generation, such as
   template instantiation and function body copying.  */

static bool uid_sensitive_constexpr_evaluation_value;

/* An internal counter that keeps track of the number of times
   uid_sensitive_constexpr_evaluation_p returned true.  */

static unsigned uid_sensitive_constexpr_evaluation_true_counter;

/* The accessor for uid_sensitive_constexpr_evaluation_value which also
   increments the corresponding counter.  */

static bool
uid_sensitive_constexpr_evaluation_p ()
{
  if (uid_sensitive_constexpr_evaluation_value)
    {
      ++uid_sensitive_constexpr_evaluation_true_counter;
      return true;
    }
  else
    return false;
}

/* RAII sentinel that saves the value of a variable, optionally
   overrides it right away, and restores its value when the sentinel
   id destructed.  */

template <typename T> class temp_override
{
  T &overridden_variable;
  T saved_value;

public:
  temp_override (T &var) : overridden_variable (var), saved_value (var) {}
  temp_override (T &var, T overrider)
    : overridden_variable (var), saved_value (var)
  {
    overridden_variable = overrider;
  }
  ~temp_override () { overridden_variable = saved_value; }
};

/* An RAII sentinel used to restrict constexpr evaluation so that it
   doesn't do anything that causes extra DECL_UID generation.  */

struct uid_sensitive_constexpr_evaluation_sentinel
{
  temp_override<bool> ovr;
  uid_sensitive_constexpr_evaluation_sentinel ();
};

/* Used to determine whether uid_sensitive_constexpr_evaluation_p was
   called and returned true, indicating that we've restricted constexpr
   evaluation in order to avoid UID generation.  We use this to control
   updates to the fold_cache and cv_cache.  */

struct uid_sensitive_constexpr_evaluation_checker
{
  const unsigned saved_counter;
  uid_sensitive_constexpr_evaluation_checker ();
  bool evaluation_restricted_p () const;
};

/* The default constructor for uid_sensitive_constexpr_evaluation_sentinel
   enables the internal flag for uid_sensitive_constexpr_evaluation_p
   during the lifetime of the sentinel object.  Upon its destruction, the
   previous value of uid_sensitive_constexpr_evaluation_p is restored.  */

uid_sensitive_constexpr_evaluation_sentinel ::
  uid_sensitive_constexpr_evaluation_sentinel ()
  : ovr (uid_sensitive_constexpr_evaluation_value, true)
{}

/* The default constructor for uid_sensitive_constexpr_evaluation_checker
   records the current number of times that uid_sensitive_constexpr_evaluation_p
   has been called and returned true.  */

uid_sensitive_constexpr_evaluation_checker ::
  uid_sensitive_constexpr_evaluation_checker ()
  : saved_counter (uid_sensitive_constexpr_evaluation_true_counter)
{}

/* Returns true iff uid_sensitive_constexpr_evaluation_p is true, and
   some constexpr evaluation was restricted due to u_s_c_e_p being called
   and returning true during the lifetime of this checker object.  */

bool
uid_sensitive_constexpr_evaluation_checker::evaluation_restricted_p () const
{
  return (uid_sensitive_constexpr_evaluation_value
	  && saved_counter != uid_sensitive_constexpr_evaluation_true_counter);
}

/* A table of all constexpr calls that have been evaluated by the
   compiler in this translation unit.  */

static GTY (()) hash_table<rust_constexpr_call_hasher> *constexpr_call_table;

/* Compute a hash value for a constexpr call representation.  */

inline hashval_t
rust_constexpr_call_hasher::hash (rust_constexpr_call *info)
{
  return info->hash;
}

/* Return true if the objects pointed to by P and Q represent calls
   to the same constexpr function with the same arguments.
   Otherwise, return false.  */

bool
rust_constexpr_call_hasher::equal (rust_constexpr_call *lhs,
				   rust_constexpr_call *rhs)
{
  if (lhs == rhs)
    return true;
  if (lhs->hash != rhs->hash)
    return false;
  if (lhs->manifestly_const_eval != rhs->manifestly_const_eval)
    return false;
  if (!rust_constexpr_fundef_hasher::equal (lhs->fundef, rhs->fundef))
    return false;
  return rs_tree_equal (lhs->bindings, rhs->bindings);
}

/* Initialize the constexpr call table, if needed.  */

static void
maybe_initialize_constexpr_call_table (void)
{
  if (constexpr_call_table == NULL)
    constexpr_call_table
      = hash_table<rust_constexpr_call_hasher>::create_ggc (101);
}

/* During constexpr CALL_EXPR evaluation, to avoid issues with sharing when
   a function happens to get called recursively, we unshare the callee
   function's body and evaluate this unshared copy instead of evaluating the
   original body.

   FUNDEF_COPIES_TABLE is a per-function freelist of these unshared function
   copies.  The underlying data structure of FUNDEF_COPIES_TABLE is a hash_map
   that's keyed off of the original FUNCTION_DECL and whose value is a
   TREE_LIST of this function's unused copies awaiting reuse.

   This is not GC-deletable to avoid GC affecting UID generation.  */

static GTY (()) decl_tree_map *fundef_copies_table;

/* Reuse a copy or create a new unshared copy of the function FUN.
   Return this copy.  We use a TREE_LIST whose PURPOSE is body, VALUE
   is parms, TYPE is result.  */

static tree
get_fundef_copy (rust_constexpr_fundef *fundef)
{
  tree copy;
  bool existed;
  tree *slot
    = &(hash_map_safe_get_or_insert<hm_ggc> (fundef_copies_table, fundef->decl,
					     &existed, 127));

  if (!existed)
    {
      /* There is no cached function available, or in use.  We can use
	 the function directly.  That the slot is now created records
	 that this function is now in use.  */
      copy = build_tree_list (fundef->body, fundef->parms);
      TREE_TYPE (copy) = fundef->result;
    }
  else if (*slot == NULL_TREE)
    {
      if (uid_sensitive_constexpr_evaluation_p ())
	return NULL_TREE;

      /* We've already used the function itself, so make a copy.  */
      copy = build_tree_list (NULL, NULL);
      tree saved_body = DECL_SAVED_TREE (fundef->decl);
      tree saved_parms = DECL_ARGUMENTS (fundef->decl);
      tree saved_result = DECL_RESULT (fundef->decl);
      tree saved_fn = current_function_decl;
      DECL_SAVED_TREE (fundef->decl) = fundef->body;
      DECL_ARGUMENTS (fundef->decl) = fundef->parms;
      DECL_RESULT (fundef->decl) = fundef->result;
      current_function_decl = fundef->decl;
      TREE_PURPOSE (copy)
	= copy_fn (fundef->decl, TREE_VALUE (copy), TREE_TYPE (copy));
      current_function_decl = saved_fn;
      DECL_RESULT (fundef->decl) = saved_result;
      DECL_ARGUMENTS (fundef->decl) = saved_parms;
      DECL_SAVED_TREE (fundef->decl) = saved_body;
    }
  else
    {
      /* We have a cached function available.  */
      copy = *slot;
      *slot = TREE_CHAIN (copy);
    }

  return copy;
}

/* Save the copy COPY of function FUN for later reuse by
   get_fundef_copy().  By construction, there will always be an entry
   to find.  */

static void
save_fundef_copy (tree fun, tree copy)
{
  tree *slot = fundef_copies_table->get (fun);
  TREE_CHAIN (copy) = *slot;
  *slot = copy;
}

static tree
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p);
tree
decl_constant_value (tree decl, bool unshare_p);

static void
non_const_var_error (location_t loc, tree r);

static tree
eval_constant_expression (const constexpr_ctx *ctx, tree, bool, bool *, bool *,
			  tree * = NULL);

static tree
constexpr_fn_retval (const constexpr_ctx *ctx, tree r);

static tree
eval_store_expression (const constexpr_ctx *ctx, tree r, bool, bool *, bool *);

static tree
eval_call_expression (const constexpr_ctx *ctx, tree r, bool, bool *, bool *);

static tree
eval_binary_expression (const constexpr_ctx *ctx, tree r, bool, bool *, bool *);

static tree
get_function_named_in_call (tree t);

static tree
eval_statement_list (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		     bool *overflow_p, tree *jump_target);
static tree
extract_string_elt (tree string, unsigned chars_per_elt, unsigned index);

static tree
eval_conditional_expression (const constexpr_ctx *ctx, tree t, bool lval,
			     bool *non_constant_p, bool *overflow_p,
			     tree *jump_target);

static tree
eval_bit_field_ref (const constexpr_ctx *ctx, tree t, bool lval,
		    bool *non_constant_p, bool *overflow_p);

static tree
eval_loop_expr (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		bool *overflow_p, tree *jump_target);

static tree
eval_switch_expr (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		  bool *overflow_p, tree *jump_target);

static tree
eval_unary_expression (const constexpr_ctx *ctx, tree t, bool /*lval*/,
		       bool *non_constant_p, bool *overflow_p);

/* Variables and functions to manage constexpr call expansion context.
   These do not need to be marked for PCH or GC.  */

/* FIXME remember and print actual constant arguments.  */
static vec<tree> call_stack;
static int call_stack_tick;
static int last_cx_error_tick;

static int
push_cx_call_context (tree call)
{
  ++call_stack_tick;
  if (!EXPR_HAS_LOCATION (call))
    SET_EXPR_LOCATION (call, input_location);
  call_stack.safe_push (call);
  int len = call_stack.length ();
  if (len > max_constexpr_depth)
    return false;
  return len;
}

static void
pop_cx_call_context (void)
{
  ++call_stack_tick;
  call_stack.pop ();
}

vec<tree>
cx_error_context (void)
{
  vec<tree> r = vNULL;
  if (call_stack_tick != last_cx_error_tick && !call_stack.is_empty ())
    r = call_stack;
  last_cx_error_tick = call_stack_tick;
  return r;
}

// this is ported from cxx_eval_outermost_constant_expr
tree
fold_expr (tree expr)
{
  bool allow_non_constant = false;
  bool strict = true;
  bool manifestly_const_eval = false;

  constexpr_global_ctx global_ctx;
  constexpr_ctx ctx
    = {&global_ctx, NULL,
       NULL,	    NULL,
       NULL,	    NULL,
       NULL,	    allow_non_constant,
       strict,	    manifestly_const_eval || !allow_non_constant};

  auto_vec<tree, 16> cleanups;
  global_ctx.cleanups = &cleanups;

  bool non_constant_p = false;
  bool overflow_p = false;

  tree folded = eval_constant_expression (&ctx, expr, false, &non_constant_p,
					  &overflow_p);
  rust_assert (folded != NULL_TREE);

  // more logic here to possibly port
  return folded;
}

static bool
same_type_ignoring_tlq_and_bounds_p (tree type1, tree type2)
{
  while (TREE_CODE (type1) == ARRAY_TYPE && TREE_CODE (type2) == ARRAY_TYPE
	 && (!TYPE_DOMAIN (type1) || !TYPE_DOMAIN (type2)))
    {
      type1 = TREE_TYPE (type1);
      type2 = TREE_TYPE (type2);
    }
  return same_type_ignoring_top_level_qualifiers_p (type1, type2);
}

// forked from gcc/cp/constexpr.cc cxx_union_active_member

/* Try to determine the currently active union member for an expression
   with UNION_TYPE.  If it can be determined, return the FIELD_DECL,
   otherwise return NULL_TREE.  */

static tree
union_active_member (const constexpr_ctx *ctx, tree t)
{
  constexpr_ctx new_ctx = *ctx;
  new_ctx.quiet = true;
  bool non_constant_p = false, overflow_p = false;
  tree ctor = eval_constant_expression (&new_ctx, t, false, &non_constant_p,
					&overflow_p);
  if (TREE_CODE (ctor) == CONSTRUCTOR && CONSTRUCTOR_NELTS (ctor) == 1
      && CONSTRUCTOR_ELT (ctor, 0)->index
      && TREE_CODE (CONSTRUCTOR_ELT (ctor, 0)->index) == FIELD_DECL)
    return CONSTRUCTOR_ELT (ctor, 0)->index;
  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc cxx_fold_indirect_ref_1

static tree
fold_indirect_ref_1 (const constexpr_ctx *ctx, location_t loc, tree type,
		     tree op, unsigned HOST_WIDE_INT off, bool *empty_base)
{
  tree optype = TREE_TYPE (op);
  unsigned HOST_WIDE_INT const_nunits;
  if (off == 0 && similar_type_p (optype, type))
    return op;
  else if (TREE_CODE (optype) == COMPLEX_TYPE
	   && similar_type_p (type, TREE_TYPE (optype)))
    {
      /* *(foo *)&complexfoo => __real__ complexfoo */
      if (off == 0)
	return build1_loc (loc, REALPART_EXPR, type, op);
      /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
      else if (tree_to_uhwi (TYPE_SIZE_UNIT (type)) == off)
	return build1_loc (loc, IMAGPART_EXPR, type, op);
    }
  /* ((foo*)&vectorfoo)[x] => BIT_FIELD_REF<vectorfoo,...> */
  else if (VECTOR_TYPE_P (optype) && similar_type_p (type, TREE_TYPE (optype))
	   && TYPE_VECTOR_SUBPARTS (optype).is_constant (&const_nunits))
    {
      unsigned HOST_WIDE_INT part_width = tree_to_uhwi (TYPE_SIZE_UNIT (type));
      unsigned HOST_WIDE_INT max_offset = part_width * const_nunits;
      if (off < max_offset && off % part_width == 0)
	{
	  tree index = bitsize_int (off * BITS_PER_UNIT);
	  return build3_loc (loc, BIT_FIELD_REF, type, op, TYPE_SIZE (type),
			     index);
	}
    }
  /* ((foo *)&fooarray)[x] => fooarray[x] */
  else if (TREE_CODE (optype) == ARRAY_TYPE
	   && tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (optype)))
	   && !integer_zerop (TYPE_SIZE_UNIT (TREE_TYPE (optype))))
    {
      tree type_domain = TYPE_DOMAIN (optype);
      tree min_val = size_zero_node;
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      unsigned HOST_WIDE_INT el_sz
	= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (optype)));
      unsigned HOST_WIDE_INT idx = off / el_sz;
      unsigned HOST_WIDE_INT rem = off % el_sz;
      if (tree_fits_uhwi_p (min_val))
	{
	  tree index = size_int (idx + tree_to_uhwi (min_val));
	  op = build4_loc (loc, ARRAY_REF, TREE_TYPE (optype), op, index,
			   NULL_TREE, NULL_TREE);
	  return fold_indirect_ref_1 (ctx, loc, type, op, rem, empty_base);
	}
    }
  /* ((foo *)&struct_with_foo_field)[x] => COMPONENT_REF */
  else if (TREE_CODE (optype) == RECORD_TYPE
	   || TREE_CODE (optype) == UNION_TYPE)
    {
      if (TREE_CODE (optype) == UNION_TYPE)
	/* For unions prefer the currently active member.  */
	if (tree field = union_active_member (ctx, op))
	  {
	    unsigned HOST_WIDE_INT el_sz
	      = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (field)));
	    if (off < el_sz)
	      {
		tree cop = build3 (COMPONENT_REF, TREE_TYPE (field), op, field,
				   NULL_TREE);
		if (tree ret = fold_indirect_ref_1 (ctx, loc, type, cop, off,
						    empty_base))
		  return ret;
	      }
	  }
      for (tree field = TYPE_FIELDS (optype); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && TREE_TYPE (field) != error_mark_node
	    && tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (field))))
	  {
	    tree pos = byte_position (field);
	    if (!tree_fits_uhwi_p (pos))
	      continue;
	    unsigned HOST_WIDE_INT upos = tree_to_uhwi (pos);
	    unsigned HOST_WIDE_INT el_sz
	      = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (field)));
	    if (upos <= off && off < upos + el_sz)
	      {
		tree cop = build3 (COMPONENT_REF, TREE_TYPE (field), op, field,
				   NULL_TREE);
		if (tree ret = fold_indirect_ref_1 (ctx, loc, type, cop,
						    off - upos, empty_base))
		  return ret;
	      }
	  }
      /* Also handle conversion to an empty base class, which
	 is represented with a NOP_EXPR.  */
      if (is_empty_class (type) && CLASS_TYPE_P (optype))
	{
	  *empty_base = true;
	  return op;
	}
    }

  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc cxx_fold_indirect_ref

/* A less strict version of fold_indirect_ref_1, which requires cv-quals to
   match.  We want to be less strict for simple *& folding; if we have a
   non-const temporary that we access through a const pointer, that should
   work.  We handle this here rather than change fold_indirect_ref_1
   because we're dealing with things like ADDR_EXPR of INTEGER_CST which
   don't really make sense outside of constant expression evaluation.  Also
   we want to allow folding to COMPONENT_REF, which could cause trouble
   with TBAA in fold_indirect_ref_1.  */

static tree
rs_fold_indirect_ref (const constexpr_ctx *ctx, location_t loc, tree type,
		      tree op0, bool *empty_base)
{
  tree sub = op0;
  tree subtype;

  /* STRIP_NOPS, but stop if REINTERPRET_CAST_P.  */
  while (CONVERT_EXPR_P (sub) || TREE_CODE (sub) == NON_LVALUE_EXPR
	 || TREE_CODE (sub) == VIEW_CONVERT_EXPR)
    {
      if (TREE_CODE (sub) == NOP_EXPR && REINTERPRET_CAST_P (sub))
	return NULL_TREE;
      sub = TREE_OPERAND (sub, 0);
    }

  subtype = TREE_TYPE (sub);
  if (!INDIRECT_TYPE_P (subtype))
    return NULL_TREE;

  /* Canonicalizes the given OBJ/OFF pair by iteratively absorbing
     the innermost component into the offset until it would make the
     offset positive, so that cxx_fold_indirect_ref_1 can identify
     more folding opportunities.  */
  auto canonicalize_obj_off = [] (tree &obj, tree &off) {
    while (TREE_CODE (obj) == COMPONENT_REF
	   && (tree_int_cst_sign_bit (off) || integer_zerop (off)))
      {
	tree field = TREE_OPERAND (obj, 1);
	tree pos = byte_position (field);
	if (integer_zerop (off) && integer_nonzerop (pos))
	  /* If the offset is already 0, keep going as long as the
	     component is at position 0.  */
	  break;
	off = int_const_binop (PLUS_EXPR, off, pos);
	obj = TREE_OPERAND (obj, 0);
      }
  };

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);

      /* *&CONST_DECL -> to the value of the const decl.  */
      if (TREE_CODE (op) == CONST_DECL)
	return DECL_INITIAL (op);
      /* *&p => p;  make sure to handle *&"str"[cst] here.  */
      if (similar_type_p (optype, type))
	{
	  tree fop = fold_read_from_constant_string (op);
	  if (fop)
	    return fop;
	  else
	    return op;
	}
      else
	{
	  tree off = integer_zero_node;
	  canonicalize_obj_off (op, off);
	  gcc_assert (integer_zerop (off));
	  return fold_indirect_ref_1 (ctx, loc, type, op, 0, empty_base);
	}
    }
  else if (TREE_CODE (sub) == POINTER_PLUS_EXPR
	   && tree_fits_uhwi_p (TREE_OPERAND (sub, 1)))
    {
      tree op00 = TREE_OPERAND (sub, 0);
      tree off = TREE_OPERAND (sub, 1);

      STRIP_NOPS (op00);
      if (TREE_CODE (op00) == ADDR_EXPR)
	{
	  tree obj = TREE_OPERAND (op00, 0);
	  canonicalize_obj_off (obj, off);
	  return fold_indirect_ref_1 (ctx, loc, type, obj, tree_to_uhwi (off),
				      empty_base);
	}
    }
  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  else if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
	   && similar_type_p (type, TREE_TYPE (TREE_TYPE (subtype))))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      tree newsub
	= rs_fold_indirect_ref (ctx, loc, TREE_TYPE (subtype), sub, NULL);
      if (newsub)
	sub = newsub;
      else
	sub = build1_loc (loc, INDIRECT_REF, TREE_TYPE (subtype), sub);
      type_domain = TYPE_DOMAIN (TREE_TYPE (sub));
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      return build4_loc (loc, ARRAY_REF, type, sub, min_val, NULL_TREE,
			 NULL_TREE);
    }

  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc cxx_eval_indirect_ref

static tree
rs_eval_indirect_ref (const constexpr_ctx *ctx, tree t, bool lval,
		      bool *non_constant_p, bool *overflow_p)
{
  tree orig_op0 = TREE_OPERAND (t, 0);
  bool empty_base = false;

  /* We can handle a MEM_REF like an INDIRECT_REF, if MEM_REF's second
     operand is an integer-zero.  Otherwise reject the MEM_REF for now.  */

  if (TREE_CODE (t) == MEM_REF
      && (!TREE_OPERAND (t, 1) || !integer_zerop (TREE_OPERAND (t, 1))))
    {
      gcc_assert (ctx->quiet);
      *non_constant_p = true;
      return t;
    }

  /* First try to simplify it directly.  */
  tree r = rs_fold_indirect_ref (ctx, EXPR_LOCATION (t), TREE_TYPE (t),
				 orig_op0, &empty_base);
  if (!r)
    {
      /* If that didn't work, evaluate the operand first.  */
      tree op0
	= eval_constant_expression (ctx, orig_op0,
				    /*lval*/ false, non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p)
	return t;

      if (!lval && integer_zerop (op0))
	{
	  if (!ctx->quiet)
	    error ("dereferencing a null pointer");
	  *non_constant_p = true;
	  return t;
	}

      r = rs_fold_indirect_ref (ctx, EXPR_LOCATION (t), TREE_TYPE (t), op0,
				&empty_base);
      if (r == NULL_TREE)
	{
	  /* We couldn't fold to a constant value.  Make sure it's not
	     something we should have been able to fold.  */
	  tree sub = op0;
	  STRIP_NOPS (sub);
	  if (TREE_CODE (sub) == ADDR_EXPR)
	    {
	      gcc_assert (
		!similar_type_p (TREE_TYPE (TREE_TYPE (sub)), TREE_TYPE (t)));
	      /* DR 1188 says we don't have to deal with this.  */
	      if (!ctx->quiet)
		error_at (rs_expr_loc_or_input_loc (t),
			  "accessing value of %qE through a %qT glvalue in a "
			  "constant expression",
			  build_fold_indirect_ref (sub), TREE_TYPE (t));
	      *non_constant_p = true;
	      return t;
	    }

	  if (lval && op0 != orig_op0)
	    return build1 (INDIRECT_REF, TREE_TYPE (t), op0);
	  if (!lval)
	    VERIFY_CONSTANT (t);
	  return t;
	}
    }

  r = eval_constant_expression (ctx, r, lval, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  /* If we're pulling out the value of an empty base, just return an empty
     CONSTRUCTOR.  */
  if (empty_base && !lval)
    {
      r = build_constructor (TREE_TYPE (t), NULL);
      TREE_CONSTANT (r) = true;
    }

  return r;
}

// forked from gcc/cp/constexpr.cc cxx_eval_logical_expression

/* Subroutine of cxx_eval_constant_expression.
   Evaluate a short-circuited logical expression T in the context
   of a given constexpr CALL.  BAILOUT_VALUE is the value for
   early return.  CONTINUE_VALUE is used here purely for
   sanity check purposes.  */

static tree
eval_logical_expression (const constexpr_ctx *ctx, tree t, tree bailout_value,
			 tree continue_value, bool lval, bool *non_constant_p,
			 bool *overflow_p)
{
  tree r;
  tree lhs = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				       non_constant_p, overflow_p);
  VERIFY_CONSTANT (lhs);
  if (tree_int_cst_equal (lhs, bailout_value))
    return lhs;
  gcc_assert (tree_int_cst_equal (lhs, continue_value));
  r = eval_constant_expression (ctx, TREE_OPERAND (t, 1), lval, non_constant_p,
				overflow_p);
  VERIFY_CONSTANT (r);
  return r;
}

// forked from gcc/cp/constexp.rcc lookup_placeholder

/* Find the object of TYPE under initialization in CTX.  */

static tree
lookup_placeholder (const constexpr_ctx *ctx, bool lval, tree type)
{
  if (!ctx)
    return NULL_TREE;

  /* Prefer the outermost matching object, but don't cross
     CONSTRUCTOR_PLACEHOLDER_BOUNDARY constructors.  */
  if (ctx->ctor && !CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ctx->ctor))
    if (tree outer_ob = lookup_placeholder (ctx->parent, lval, type))
      return outer_ob;

  /* We could use ctx->object unconditionally, but using ctx->ctor when we
     can is a minor optimization.  */
  if (!lval && ctx->ctor && same_type_p (TREE_TYPE (ctx->ctor), type))
    return ctx->ctor;

  if (!ctx->object)
    return NULL_TREE;

  /* Since an object cannot have a field of its own type, we can search outward
     from ctx->object to find the unique containing object of TYPE.  */
  tree ob = ctx->object;
  while (ob)
    {
      if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (ob), type))
	break;
      if (handled_component_p (ob))
	ob = TREE_OPERAND (ob, 0);
      else
	ob = NULL_TREE;
    }

  return ob;
}

// forked from gcc/cp/constexp.rcc inline_asm_in_constexpr_error

/* Complain about an attempt to evaluate inline assembly.  */

static void
inline_asm_in_constexpr_error (location_t loc)
{
  auto_diagnostic_group d;
  error_at (loc, "inline assembly is not a constant expression");
  inform (loc, "only unevaluated inline assembly is allowed in a "
	       "%<constexpr%> function in C++20");
}

// forked from gcc/cp/constexpr.cc verify_ctor_sanity

/* We're about to process an initializer for a class or array TYPE.  Make
   sure that CTX is set up appropriately.  */

static void
verify_ctor_sanity (const constexpr_ctx *ctx, tree type)
{
  /* We don't bother building a ctor for an empty base subobject.  */
  if (is_empty_class (type))
    return;

  /* We're in the middle of an initializer that might involve placeholders;
     our caller should have created a CONSTRUCTOR for us to put the
     initializer into.  We will either return that constructor or T.  */
  gcc_assert (ctx->ctor);
  gcc_assert (
    same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (ctx->ctor)));
  /* We used to check that ctx->ctor was empty, but that isn't the case when
     the object is zero-initialized before calling the constructor.  */
  if (ctx->object)
    {
      tree otype = TREE_TYPE (ctx->object);
      gcc_assert (same_type_ignoring_top_level_qualifiers_p (type, otype)
		  /* Handle flexible array members.  */
		  || (TREE_CODE (otype) == ARRAY_TYPE
		      && TYPE_DOMAIN (otype) == NULL_TREE
		      && TREE_CODE (type) == ARRAY_TYPE
		      && (same_type_ignoring_top_level_qualifiers_p (
			TREE_TYPE (type), TREE_TYPE (otype)))));
    }
  gcc_assert (!ctx->object || !DECL_P (ctx->object)
	      || *(ctx->global->values.get (ctx->object)) == ctx->ctor);
}

// forked from gcc/cp/constexpr.cc array_index_cmp

/* Some of the expressions fed to the constexpr mechanism are calls to
   constructors, which have type void.  In that case, return the type being
   initialized by the constructor.  */

static tree
initialized_type (tree t)
{
  if (TYPE_P (t))
    return t;
  tree type = TREE_TYPE (t);
  if (TREE_CODE (t) == CALL_EXPR)
    {
      /* A constructor call has void type, so we need to look deeper.  */
      tree fn = get_function_named_in_call (t);
      if (fn && TREE_CODE (fn) == FUNCTION_DECL && DECL_CXX_CONSTRUCTOR_P (fn))
	type = DECL_CONTEXT (fn);
    }
  else if (TREE_CODE (t) == COMPOUND_EXPR)
    return initialized_type (TREE_OPERAND (t, 1));

  return cv_unqualified (type);
}

// forked from gcc/cp/constexpr.cc init_subob_ctx

/* We're about to initialize element INDEX of an array or class from VALUE.
   Set up NEW_CTX appropriately by adjusting .object to refer to the
   subobject and creating a new CONSTRUCTOR if the element is itself
   a class or array.  */

static void
init_subob_ctx (const constexpr_ctx *ctx, constexpr_ctx &new_ctx, tree index,
		tree &value)
{
  new_ctx = *ctx;

  if (index && TREE_CODE (index) != INTEGER_CST
      && TREE_CODE (index) != FIELD_DECL && TREE_CODE (index) != RANGE_EXPR)
    /* This won't have an element in the new CONSTRUCTOR.  */
    return;

  tree type = initialized_type (value);
  if (!AGGREGATE_TYPE_P (type) && !VECTOR_TYPE_P (type))
    /* A non-aggregate member doesn't get its own CONSTRUCTOR.  */
    return;

  /* The sub-aggregate initializer might contain a placeholder;
     update object to refer to the subobject and ctor to refer to
     the (newly created) sub-initializer.  */
  if (ctx->object)
    {
      if (index == NULL_TREE || TREE_CODE (index) == RANGE_EXPR)
	/* There's no well-defined subobject for this index.  */
	new_ctx.object = NULL_TREE;
      else
	{
	  // Faisal: commenting this out as not sure if it's needed and it's
	  // huge new_ctx.object = build_ctor_subob_ref (index, type,
	  // ctx->object);
	}
    }
  tree elt = build_constructor (type, NULL);
  CONSTRUCTOR_NO_CLEARING (elt) = true;
  new_ctx.ctor = elt;

  if (TREE_CODE (value) == TARGET_EXPR)
    /* Avoid creating another CONSTRUCTOR when we expand the TARGET_EXPR.  */
    value = TARGET_EXPR_INITIAL (value);
}

// forked from gcc/cp/constexpr.cc base_field_constructor_elt

/* REF is a COMPONENT_REF designating a particular field.  V is a vector of
   CONSTRUCTOR elements to initialize (part of) an object containing that
   field.  Return a pointer to the constructor_elt corresponding to the
   initialization of the field.  */

static constructor_elt *
base_field_constructor_elt (vec<constructor_elt, va_gc> *v, tree ref)
{
  tree aggr = TREE_OPERAND (ref, 0);
  tree field = TREE_OPERAND (ref, 1);
  HOST_WIDE_INT i;
  constructor_elt *ce;

  gcc_assert (TREE_CODE (ref) == COMPONENT_REF);

  if (TREE_CODE (aggr) == COMPONENT_REF)
    {
      constructor_elt *base_ce = base_field_constructor_elt (v, aggr);
      v = CONSTRUCTOR_ELTS (base_ce->value);
    }

  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    if (ce->index == field)
      return ce;

  rust_unreachable ();
  return NULL;
}

/* Return a pointer to the constructor_elt of CTOR which matches INDEX.  If no
   matching constructor_elt exists, then add one to CTOR.

   As an optimization, if POS_HINT is non-negative then it is used as a guess
   for the (integer) index of the matching constructor_elt within CTOR.  */

static constructor_elt *
get_or_insert_ctor_field (tree ctor, tree index, int pos_hint = -1)
{
  /* Check the hint first.  */
  if (pos_hint >= 0 && (unsigned) pos_hint < CONSTRUCTOR_NELTS (ctor)
      && CONSTRUCTOR_ELT (ctor, pos_hint)->index == index)
    return CONSTRUCTOR_ELT (ctor, pos_hint);

  tree type = TREE_TYPE (ctor);
  if (TREE_CODE (type) == VECTOR_TYPE && index == NULL_TREE)
    {
      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (ctor), index, NULL_TREE);
      return &CONSTRUCTOR_ELTS (ctor)->last ();
    }
  else if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == VECTOR_TYPE)
    {
      if (TREE_CODE (index) == RANGE_EXPR)
	{
	  /* Support for RANGE_EXPR index lookups is currently limited to
	     accessing an existing element via POS_HINT, or appending a new
	     element to the end of CTOR.  ??? Support for other access
	     patterns may also be needed.  */
	  vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ctor);
	  if (vec_safe_length (elts))
	    {
	      tree lo = TREE_OPERAND (index, 0);
	      gcc_assert (array_index_cmp (elts->last ().index, lo) < 0);
	    }
	  CONSTRUCTOR_APPEND_ELT (elts, index, NULL_TREE);
	  return &elts->last ();
	}

      HOST_WIDE_INT i = find_array_ctor_elt (ctor, index, /*insert*/ true);
      gcc_assert (i >= 0);
      constructor_elt *cep = CONSTRUCTOR_ELT (ctor, i);
      gcc_assert (cep->index == NULL_TREE
		  || TREE_CODE (cep->index) != RANGE_EXPR);
      return cep;
    }
  else
    {
      gcc_assert (
	TREE_CODE (index) == FIELD_DECL
	&& (same_type_ignoring_top_level_qualifiers_p (DECL_CONTEXT (index),
						       TREE_TYPE (ctor))));

      /* We must keep the CONSTRUCTOR's ELTS in FIELD order.
	 Usually we meet initializers in that order, but it is
	 possible for base types to be placed not in program
	 order.  */
      tree fields = TYPE_FIELDS (DECL_CONTEXT (index));
      unsigned HOST_WIDE_INT idx = 0;
      constructor_elt *cep = NULL;

      /* Check if we're changing the active member of a union.  */
      if (TREE_CODE (type) == UNION_TYPE && CONSTRUCTOR_NELTS (ctor)
	  && CONSTRUCTOR_ELT (ctor, 0)->index != index)
	vec_safe_truncate (CONSTRUCTOR_ELTS (ctor), 0);
      /* If the bit offset of INDEX is larger than that of the last
	 constructor_elt, then we can just immediately append a new
	 constructor_elt to the end of CTOR.  */
      else if (CONSTRUCTOR_NELTS (ctor)
	       && tree_int_cst_compare (
		    bit_position (index),
		    bit_position (CONSTRUCTOR_ELTS (ctor)->last ().index))
		    > 0)
	{
	  idx = CONSTRUCTOR_NELTS (ctor);
	  goto insert;
	}

      /* Otherwise, we need to iterate over CTOR to find or insert INDEX
	 appropriately.  */

      for (; vec_safe_iterate (CONSTRUCTOR_ELTS (ctor), idx, &cep);
	   idx++, fields = DECL_CHAIN (fields))
	{
	  if (index == cep->index)
	    goto found;

	  /* The field we're initializing must be on the field
	     list.  Look to see if it is present before the
	     field the current ELT initializes.  */
	  for (; fields != cep->index; fields = DECL_CHAIN (fields))
	    if (index == fields)
	      goto insert;
	}
      /* We fell off the end of the CONSTRUCTOR, so insert a new
	 entry at the end.  */

      insert : {
	constructor_elt ce = {index, NULL_TREE};

	vec_safe_insert (CONSTRUCTOR_ELTS (ctor), idx, ce);
	cep = CONSTRUCTOR_ELT (ctor, idx);
      }
    found:;

      return cep;
    }
}

// forked from gcc/cp/constexpr.cc cxx_eval_vector_conditional_expression

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate vector condition expressions.  Unlike
   cxx_eval_conditional_expression, VEC_COND_EXPR acts like a normal
   ternary arithmetics operation, where all 3 arguments have to be
   evaluated as constants and then folding computes the result from
   them.  */

static tree
eval_vector_conditional_expression (const constexpr_ctx *ctx, tree t,
				    bool *non_constant_p, bool *overflow_p)
{
  tree arg1
    = eval_constant_expression (ctx, TREE_OPERAND (t, 0),
				/*lval*/ false, non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg1);
  tree arg2
    = eval_constant_expression (ctx, TREE_OPERAND (t, 1),
				/*lval*/ false, non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg2);
  tree arg3
    = eval_constant_expression (ctx, TREE_OPERAND (t, 2),
				/*lval*/ false, non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg3);
  location_t loc = EXPR_LOCATION (t);
  tree type = TREE_TYPE (t);
  tree r = fold_ternary_loc (loc, VEC_COND_EXPR, type, arg1, arg2, arg3);
  if (r == NULL_TREE)
    {
      if (arg1 == TREE_OPERAND (t, 0) && arg2 == TREE_OPERAND (t, 1)
	  && arg3 == TREE_OPERAND (t, 2))
	r = t;
      else
	r = build3_loc (loc, VEC_COND_EXPR, type, arg1, arg2, arg3);
    }
  VERIFY_CONSTANT (r);
  return r;
}

// forked from gcc/cp/constexpr.cc cxx_eval_bare_aggregate

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T denotes a C-style array or a C-style
   aggregate.  Reduce it to a constant expression.  */

static tree
eval_bare_aggregate (const constexpr_ctx *ctx, tree t, bool lval,
		     bool *non_constant_p, bool *overflow_p)
{
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
  bool changed = false;
  gcc_assert (!BRACE_ENCLOSED_INITIALIZER_P (t));
  tree type = TREE_TYPE (t);

  constexpr_ctx new_ctx;
  if (TYPE_PTRMEMFUNC_P (type) || VECTOR_TYPE_P (type))
    {
      /* We don't really need the ctx->ctor business for a PMF or
	 vector, but it's simpler to use the same code.  */
      new_ctx = *ctx;
      new_ctx.ctor = build_constructor (type, NULL);
      new_ctx.object = NULL_TREE;
      ctx = &new_ctx;
    };
  verify_ctor_sanity (ctx, type);
  vec<constructor_elt, va_gc> **p = &CONSTRUCTOR_ELTS (ctx->ctor);
  vec_alloc (*p, vec_safe_length (v));

  if (CONSTRUCTOR_PLACEHOLDER_BOUNDARY (t))
    CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ctx->ctor) = 1;

  unsigned i;
  tree index, value;
  bool constant_p = true;
  bool side_effects_p = false;
  FOR_EACH_CONSTRUCTOR_ELT (v, i, index, value)
    {
      tree orig_value = value;
      /* Like in cxx_eval_store_expression, omit entries for empty fields.  */
      bool no_slot = TREE_CODE (type) == RECORD_TYPE && is_empty_field (index);
      if (no_slot)
	new_ctx = *ctx;
      else
	init_subob_ctx (ctx, new_ctx, index, value);
      int pos_hint = -1;
      if (new_ctx.ctor != ctx->ctor)
	{
	  /* If we built a new CONSTRUCTOR, attach it now so that other
	     initializers can refer to it.  */
	  constructor_elt *cep = get_or_insert_ctor_field (ctx->ctor, index);
	  cep->value = new_ctx.ctor;
	  pos_hint = cep - (*p)->begin ();
	}
      else if (TREE_CODE (type) == UNION_TYPE)
	/* Otherwise if we're constructing a non-aggregate union member, set
	   the active union member now so that we can later detect and diagnose
	   if its initializer attempts to activate another member.  */
	get_or_insert_ctor_field (ctx->ctor, index);
      tree elt = eval_constant_expression (&new_ctx, value, lval,
					   non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (ctx->quiet && *non_constant_p)
	break;
      if (elt != orig_value)
	changed = true;

      if (!TREE_CONSTANT (elt))
	constant_p = false;
      if (TREE_SIDE_EFFECTS (elt))
	side_effects_p = true;
      if (index && TREE_CODE (index) == COMPONENT_REF)
	{
	  /* This is an initialization of a vfield inside a base
	     subaggregate that we already initialized; push this
	     initialization into the previous initialization.  */
	  constructor_elt *inner = base_field_constructor_elt (*p, index);
	  inner->value = elt;
	  changed = true;
	}
      else if (index
	       && (TREE_CODE (index) == NOP_EXPR
		   || TREE_CODE (index) == POINTER_PLUS_EXPR))
	{
	  /* This is an initializer for an empty base; now that we've
	     checked that it's constant, we can ignore it.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (index))));
	  changed = true;
	}
      else if (no_slot)
	changed = true;
      else
	{
	  if (TREE_CODE (type) == UNION_TYPE && (*p)->last ().index != index)
	    /* The initializer erroneously changed the active union member that
	       we're initializing.  */
	    gcc_assert (*non_constant_p);
	  else
	    {
	      /* The initializer might have mutated the underlying CONSTRUCTOR,
		 so recompute the location of the target constructer_elt.  */
	      constructor_elt *cep
		= get_or_insert_ctor_field (ctx->ctor, index, pos_hint);
	      cep->value = elt;
	    }

	  /* Adding or replacing an element might change the ctor's flags.  */
	  TREE_CONSTANT (ctx->ctor) = constant_p;
	  TREE_SIDE_EFFECTS (ctx->ctor) = side_effects_p;
	}
    }
  if (*non_constant_p || !changed)
    return t;
  t = ctx->ctor;
  /* We're done building this CONSTRUCTOR, so now we can interpret an
     element without an explicit initializer as value-initialized.  */
  CONSTRUCTOR_NO_CLEARING (t) = false;
  TREE_CONSTANT (t) = constant_p;
  TREE_SIDE_EFFECTS (t) = side_effects_p;
  if (VECTOR_TYPE_P (type))
    t = fold (t);
  return t;
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for trinary expressions.  */

static tree
cxx_eval_trinary_expression (const constexpr_ctx *ctx, tree t, bool lval,
			     bool *non_constant_p, bool *overflow_p)
{
  int i;
  tree args[3];
  tree val;

  for (i = 0; i < 3; i++)
    {
      args[i] = eval_constant_expression (ctx, TREE_OPERAND (t, i), lval,
					  non_constant_p, overflow_p);
      VERIFY_CONSTANT (args[i]);
    }

  val = fold_ternary_loc (EXPR_LOCATION (t), TREE_CODE (t), TREE_TYPE (t),
			  args[0], args[1], args[2]);
  if (val == NULL_TREE)
    return t;
  VERIFY_CONSTANT (val);
  return val;
}

/* Return true if T is a valid constant initializer.  If a CONSTRUCTOR
   initializes all the members, the CONSTRUCTOR_NO_CLEARING flag will be
   cleared.
   FIXME speed this up, it's taking 16% of compile time on sieve testcase.  */

bool
reduced_constant_expression_p (tree t)
{
  if (t == NULL_TREE)
    return false;

  switch (TREE_CODE (t))
    {
    case PTRMEM_CST:
      /* Even if we can't lower this yet, it's constant.  */
      return true;

    case CONSTRUCTOR:
      /* And we need to handle PTRMEM_CST wrapped in a CONSTRUCTOR.  */
      tree field;
      if (CONSTRUCTOR_NO_CLEARING (t))
	{
	  if (TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
	    /* An initialized vector would have a VECTOR_CST.  */
	    return false;
	  else if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    {
	      /* There must be a valid constant initializer at every array
		 index.  */
	      tree min = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
	      tree max = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
	      tree cursor = min;
	      for (auto &e : CONSTRUCTOR_ELTS (t))
		{
		  if (!reduced_constant_expression_p (e.value))
		    return false;
		  if (array_index_cmp (cursor, e.index) != 0)
		    return false;
		  if (TREE_CODE (e.index) == RANGE_EXPR)
		    cursor = TREE_OPERAND (e.index, 1);
		  cursor = int_const_binop (PLUS_EXPR, cursor, size_one_node);
		}
	      if (find_array_ctor_elt (t, max) == -1)
		return false;
	      goto ok;
	    }
	  else if (TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	    {
	      if (CONSTRUCTOR_NELTS (t) == 0)
		/* An initialized union has a constructor element.  */
		return false;
	      /* And it only initializes one member.  */
	      field = NULL_TREE;
	    }
	  else
	    field = next_initializable_field (TYPE_FIELDS (TREE_TYPE (t)));
	}
      else
	field = NULL_TREE;
      for (auto &e : CONSTRUCTOR_ELTS (t))
	{
	  /* If VAL is null, we're in the middle of initializing this
	     element.  */
	  if (!reduced_constant_expression_p (e.value))
	    return false;
	  /* Empty class field may or may not have an initializer.  */
	  for (; field && e.index != field;
	       field = next_initializable_field (DECL_CHAIN (field)))
	    if (!is_really_empty_class (TREE_TYPE (field),
					/*ignore_vptr*/ false))
	      return false;
	  if (field)
	    field = next_initializable_field (DECL_CHAIN (field));
	}
      /* There could be a non-empty field at the end.  */
      for (; field; field = next_initializable_field (DECL_CHAIN (field)))
	if (!is_really_empty_class (TREE_TYPE (field), /*ignore_vptr*/ false))
	  return false;
    ok:
      if (CONSTRUCTOR_NO_CLEARING (t))
	/* All the fields are initialized.  */
	CONSTRUCTOR_NO_CLEARING (t) = false;
      return true;

    default:
      /* FIXME are we calling this too much?  */
      return initializer_constant_valid_p (t, TREE_TYPE (t)) != NULL_TREE;
    }
}

/* TEMP is the constant value of a temporary object of type TYPE.  Adjust
   the type of the value to match.  */

static tree
adjust_temp_type (tree type, tree temp)
{
  if (same_type_p (TREE_TYPE (temp), type))
    return temp;

  gcc_assert (scalarish_type_p (type));
  /* Now we know we're dealing with a scalar, and a prvalue of non-class
     type is cv-unqualified.  */
  return fold_convert (cv_unqualified (type), temp);
}

// forked from gcc/cp/constexpr.cc free_constructor

/* If T is a CONSTRUCTOR, ggc_free T and any sub-CONSTRUCTORs.  */

static void
free_constructor (tree t)
{
  if (!t || TREE_CODE (t) != CONSTRUCTOR)
    return;
  releasing_vec ctors;
  vec_safe_push (ctors, t);
  while (!ctors->is_empty ())
    {
      tree c = ctors->pop ();
      if (vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (c))
	{
	  constructor_elt *ce;
	  for (HOST_WIDE_INT i = 0; vec_safe_iterate (elts, i, &ce); ++i)
	    if (TREE_CODE (ce->value) == CONSTRUCTOR)
	      vec_safe_push (ctors, ce->value);
	  ggc_free (elts);
	}
      ggc_free (c);
    }
}

static tree
eval_and_check_array_index (const constexpr_ctx *ctx, tree t,
			    bool allow_one_past, bool *non_constant_p,
			    bool *overflow_p);

// forked from gcc/cp/constexpr.cc cxx_eval_array_reference

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a reference to an array slot.  */

static tree
eval_array_reference (const constexpr_ctx *ctx, tree t, bool lval,
		      bool *non_constant_p, bool *overflow_p)
{
  tree oldary = TREE_OPERAND (t, 0);
  tree ary
    = eval_constant_expression (ctx, oldary, lval, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  if (!lval && TREE_CODE (ary) == VIEW_CONVERT_EXPR
      && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (ary, 0)))
      && TREE_TYPE (t) == TREE_TYPE (TREE_TYPE (TREE_OPERAND (ary, 0))))
    ary = TREE_OPERAND (ary, 0);

  tree oldidx = TREE_OPERAND (t, 1);
  tree index
    = eval_and_check_array_index (ctx, t, lval, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  if (lval && ary == oldary && index == oldidx)
    return t;
  else if (lval)
    return build4 (ARRAY_REF, TREE_TYPE (t), ary, index, NULL, NULL);

  unsigned len = 0, elem_nchars = 1;
  tree elem_type = TREE_TYPE (TREE_TYPE (ary));
  if (TREE_CODE (ary) == CONSTRUCTOR)
    len = CONSTRUCTOR_NELTS (ary);
  else if (TREE_CODE (ary) == STRING_CST)
    {
      elem_nchars
	= (TYPE_PRECISION (elem_type) / TYPE_PRECISION (char_type_node));
      len = (unsigned) TREE_STRING_LENGTH (ary) / elem_nchars;
    }
  else if (TREE_CODE (ary) == VECTOR_CST)
    /* We don't create variable-length VECTOR_CSTs.  */
    len = VECTOR_CST_NELTS (ary).to_constant ();
  else
    {
      /* We can't do anything with other tree codes, so use
	 VERIFY_CONSTANT to complain and fail.  */
      VERIFY_CONSTANT (ary);
      rust_unreachable ();
    }

  bool found;
  HOST_WIDE_INT i = 0;
  if (TREE_CODE (ary) == CONSTRUCTOR)
    {
      HOST_WIDE_INT ix = find_array_ctor_elt (ary, index);
      found = (ix >= 0);
      if (found)
	i = ix;
    }
  else
    {
      i = tree_to_shwi (index);
      found = (i < len);
    }

  if (found)
    {
      tree r;
      if (TREE_CODE (ary) == CONSTRUCTOR)
	r = (*CONSTRUCTOR_ELTS (ary))[i].value;
      else if (TREE_CODE (ary) == VECTOR_CST)
	r = VECTOR_CST_ELT (ary, i);
      else
	r = extract_string_elt (ary, elem_nchars, i);

      if (r)
	/* Don't VERIFY_CONSTANT here.  */
	return r;

      /* Otherwise the element doesn't have a value yet.  */
    }

  /* Not found.  */

  if (TREE_CODE (ary) == CONSTRUCTOR && CONSTRUCTOR_NO_CLEARING (ary))
    {
      /* 'ary' is part of the aggregate initializer we're currently
	 building; if there's no initializer for this element yet,
	 that's an error.  */
      if (!ctx->quiet)
	error ("accessing uninitialized array element");
      *non_constant_p = true;
      return t;
    }

  /* If it's within the array bounds but doesn't have an explicit
     initializer, it's initialized from {}.  But use build_value_init
     directly for non-aggregates to avoid creating a garbage CONSTRUCTOR.  */
  tree val = NULL_TREE;
  sorry ("array size expression is not supported yet");

  constexpr_ctx new_ctx;
  if (is_really_empty_class (elem_type, /*ignore_vptr*/ false))
    return build_constructor (elem_type, NULL);
  // else if (CP_AGGREGATE_TYPE_P (elem_type))
  // {
  //   tree empty_ctor = build_constructor (init_list_type_node, NULL);
  //    //val = digest_init (elem_type, empty_ctor, tf_warning_or_error);
  //  }
  // else
  //  val = build_value_init (elem_type, tf_warning_or_error);

  if (!SCALAR_TYPE_P (elem_type))
    {
      new_ctx = *ctx;
      if (ctx->object)
	/* If there was no object, don't add one: it could confuse us
	   into thinking we're modifying a const object.  */
	new_ctx.object = t;
      new_ctx.ctor = build_constructor (elem_type, NULL);
      ctx = &new_ctx;
    }
  t = eval_constant_expression (ctx, val, lval, non_constant_p, overflow_p);
  if (!SCALAR_TYPE_P (elem_type) && t != ctx->ctor)
    free_constructor (ctx->ctor);
  return t;
}

// forked from gcc/cp/constexpr.cc cxx_eval_component_reference

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type.  */

static tree
eval_component_reference (const constexpr_ctx *ctx, tree t, bool lval,
			  bool *non_constant_p, bool *overflow_p)
{
  unsigned HOST_WIDE_INT i;
  tree field;
  tree value;
  tree part = TREE_OPERAND (t, 1);
  tree orig_whole = TREE_OPERAND (t, 0);
  tree whole = eval_constant_expression (ctx, orig_whole, lval, non_constant_p,
					 overflow_p);
  if (INDIRECT_REF_P (whole) && integer_zerop (TREE_OPERAND (whole, 0)))
    {
      if (!ctx->quiet)
	error ("dereferencing a null pointer in %qE", orig_whole);
      *non_constant_p = true;
      return t;
    }

  if (whole == orig_whole)
    return t;
  if (lval)
    return fold_build3 (COMPONENT_REF, TREE_TYPE (t), whole, part, NULL_TREE);
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!ctx->quiet)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (DECL_MUTABLE_P (part))
    {
      if (!ctx->quiet)
	error ("mutable %qD is not usable in a constant expression", part);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;
  bool pmf = TYPE_PTRMEMFUNC_P (TREE_TYPE (whole));
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      /* Use name match for PMF fields, as a variant will have a
	 different FIELD_DECL with a different type.  */
      if (pmf ? DECL_NAME (field) == DECL_NAME (part) : field == part)
	{
	  if (value)
	    {
	      STRIP_ANY_LOCATION_WRAPPER (value);
	      return value;
	    }
	  else
	    /* We're in the middle of initializing it.  */
	    break;
	}
    }
  if (TREE_CODE (TREE_TYPE (whole)) == UNION_TYPE
      && CONSTRUCTOR_NELTS (whole) > 0)
    {
      /* DR 1188 says we don't have to deal with this.  */
      if (!ctx->quiet)
	{
	  constructor_elt *cep = CONSTRUCTOR_ELT (whole, 0);
	  if (cep->value == NULL_TREE)
	    error ("accessing uninitialized member %qD", part);
	  else
	    error ("accessing %qD member instead of initialized %qD member in "
		   "constant expression",
		   part, cep->index);
	}
      *non_constant_p = true;
      return t;
    }

  /* We only create a CONSTRUCTOR for a subobject when we modify it, so empty
     classes never get represented; throw together a value now.  */
  if (is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/ false))
    return build_constructor (TREE_TYPE (t), NULL);

  gcc_assert (DECL_CONTEXT (part) == TYPE_MAIN_VARIANT (TREE_TYPE (whole)));

  if (CONSTRUCTOR_NO_CLEARING (whole))
    {
      /* 'whole' is part of the aggregate initializer we're currently
	 building; if there's no initializer for this member yet, that's an
	 error.  */
      if (!ctx->quiet)
	error ("accessing uninitialized member %qD", part);
      *non_constant_p = true;
      return t;
    }

  value = NULL_TREE;
  sorry ("constant folding not supported for this tree code");
  /* If there's no explicit init for this field, it's value-initialized.  */
  // Faisal: commenting this out as not sure if we need this but we need to come
  // back to handle this to assign suitable value to value before sending it in
  // eval_constant_expression below
  // value = build_value_init (TREE_TYPE (t), tf_warning_or_error);
  return eval_constant_expression (ctx, value, lval, non_constant_p,
				   overflow_p);
}

/* Subroutine of cxx_eval_statement_list.  Determine whether the statement
   STMT matches *jump_target.  If we're looking for a case label and we see
   the default label, note it in ctx->css_state.  */

static bool
label_matches (const constexpr_ctx *ctx, tree *jump_target, tree stmt)
{
  switch (TREE_CODE (*jump_target))
    {
    case LABEL_DECL:
      if (TREE_CODE (stmt) == LABEL_EXPR
	  && LABEL_EXPR_LABEL (stmt) == *jump_target)
	return true;
      break;

    case INTEGER_CST:
      if (TREE_CODE (stmt) == CASE_LABEL_EXPR)
	{
	  gcc_assert (ctx->css_state != NULL);
	  if (!CASE_LOW (stmt))
	    {
	      /* default: should appear just once in a SWITCH_EXPR
		 body (excluding nested SWITCH_EXPR).  */
	      gcc_assert (*ctx->css_state != css_default_seen);
	      /* When evaluating SWITCH_EXPR body for the second time,
		 return true for the default: label.  */
	      if (*ctx->css_state == css_default_processing)
		return true;
	      *ctx->css_state = css_default_seen;
	    }
	  else if (CASE_HIGH (stmt))
	    {
	      if (tree_int_cst_le (CASE_LOW (stmt), *jump_target)
		  && tree_int_cst_le (*jump_target, CASE_HIGH (stmt)))
		return true;
	    }
	  else if (tree_int_cst_equal (*jump_target, CASE_LOW (stmt)))
	    return true;
	}
      break;

    case BREAK_STMT:
    case CONTINUE_STMT:
      /* These two are handled directly in cxx_eval_loop_expr by testing
	 breaks (jump_target) or continues (jump_target).  */
      break;

    default:
      rust_unreachable ();
    }
  return false;
}

static tree
eval_constant_expression (const constexpr_ctx *ctx, tree t, bool lval,
			  bool *non_constant_p, bool *overflow_p,
			  tree *jump_target /* = NULL */)
{
  if (jump_target && *jump_target)
    {
      /* If we are jumping, ignore all statements/expressions except those
	 that could have LABEL_EXPR or CASE_LABEL_EXPR in their bodies.  */
      switch (TREE_CODE (t))
	{
	case BIND_EXPR:
	case STATEMENT_LIST:
	case LOOP_EXPR:
	case COND_EXPR:
	case IF_STMT:
	case DO_STMT:
	case WHILE_STMT:
	case FOR_STMT:
	  break;
	case LABEL_EXPR:
	case CASE_LABEL_EXPR:
	  if (label_matches (ctx, jump_target, t))
	    /* Found it.  */
	    *jump_target = NULL_TREE;
	  return NULL_TREE;
	default:
	  return NULL_TREE;
	}
    }
  if (error_operand_p (t))
    {
      *non_constant_p = true;
      return t;
    }

  location_t loc = EXPR_LOCATION (t);

  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_OVERFLOW (t))
	{
	  error_at (loc, "overflow in constant expression");
	  return t;
	}

      return t;
    }

  // Avoid excessively long constexpr evaluations
  if (++ctx->global->constexpr_ops_count >= constexpr_ops_limit)
    {
      rust_error_at (
	loc,
	"%<constexpr%> evaluation operation count exceeds limit of "
	"%wd (use %<-fconstexpr-ops-limit=%> to increase the limit)",
	constexpr_ops_limit);

      return t;
    }

  constexpr_ctx new_ctx;
  tree r = t;
  tree_code tcode = TREE_CODE (t);
  switch (tcode)
    {
    case VAR_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	{
	  r = DECL_VALUE_EXPR (t);
	  return eval_constant_expression (ctx, r, lval, non_constant_p,
					   overflow_p);
	}
      /* fall through */
      case CONST_DECL: {
	/* We used to not check lval for CONST_DECL, but darwin.cc uses
	   CONST_DECL for aggregate constants.  */
	if (lval)
	  return t;
	else if (t == ctx->object)
	  return ctx->ctor;
	if (VAR_P (t))
	  if (tree *p = ctx->global->values.get (t))
	    if (*p != NULL_TREE)
	      {
		r = *p;
		break;
	      }
	r = decl_constant_value (t, /*unshare_p=*/false);
	if (TREE_CODE (r) == TARGET_EXPR
	    && TREE_CODE (TARGET_EXPR_INITIAL (r)) == CONSTRUCTOR)
	  r = TARGET_EXPR_INITIAL (r);
	if (DECL_P (r))
	  {
	    non_const_var_error (loc, r);
	    return r;
	  }
      }
      break;

    case PARM_DECL:
      if (lval && !TYPE_REF_P (TREE_TYPE (t)))
	/* glvalue use.  */;
      else if (tree *p = ctx->global->values.get (r))
	r = *p;
      else if (lval)
	/* Defer in case this is only used for its type.  */;
      else if (COMPLETE_TYPE_P (TREE_TYPE (t))
	       && is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/ false))
	{
	  /* If the class is empty, we aren't actually loading anything.  */
	  r = build_constructor (TREE_TYPE (t), NULL);
	  TREE_CONSTANT (r) = true;
	}
      else
	{
	  if (!ctx->quiet)
	    error ("%qE is not a constant expression", t);
	  *non_constant_p = true;
	}
      break;

    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case SPACESHIP_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      r = eval_binary_expression (ctx, t, lval, non_constant_p, overflow_p);
      break;

      /* fold can introduce non-IF versions of these; still treat them as
	 short-circuiting.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      r = eval_logical_expression (ctx, t, boolean_false_node,
				   boolean_true_node, lval, non_constant_p,
				   overflow_p);
      break;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      r = eval_logical_expression (ctx, t, boolean_true_node,
				   boolean_false_node, lval, non_constant_p,
				   overflow_p);
      break;

      case TARGET_EXPR: {
	tree type = TREE_TYPE (t);

	if (!literal_type_p (type))
	  {
	    if (!ctx->quiet)
	      {
		auto_diagnostic_group d;
		error ("temporary of non-literal type %qT in a "
		       "constant expression",
		       type);
		explain_non_literal_class (type);
	      }
	    *non_constant_p = true;
	    break;
	  }
	gcc_checking_assert (!TARGET_EXPR_DIRECT_INIT_P (t));
	/* Avoid evaluating a TARGET_EXPR more than once.  */
	tree slot = TARGET_EXPR_SLOT (t);
	if (tree *p = ctx->global->values.get (slot))
	  {
	    if (lval)
	      return slot;
	    r = *p;
	    break;
	  }
	if ((AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type)))
	  {
	    /* We're being expanded without an explicit target, so start
	       initializing a new object; expansion with an explicit target
	       strips the TARGET_EXPR before we get here.  */
	    new_ctx = *ctx;
	    /* Link CTX to NEW_CTX so that lookup_placeholder can resolve
	       any PLACEHOLDER_EXPR within the initializer that refers to the
	       former object under construction.  */
	    new_ctx.parent = ctx;
	    new_ctx.ctor = build_constructor (type, NULL);
	    CONSTRUCTOR_NO_CLEARING (new_ctx.ctor) = true;
	    new_ctx.object = slot;
	    ctx->global->values.put (new_ctx.object, new_ctx.ctor);
	    ctx = &new_ctx;
	  }
	/* Pass false for 'lval' because this indicates
	   initialization of a temporary.  */
	r = eval_constant_expression (ctx, TREE_OPERAND (t, 1), false,
				      non_constant_p, overflow_p);
	if (*non_constant_p)
	  break;
	/* Adjust the type of the result to the type of the temporary.  */
	r = adjust_temp_type (type, r);
	if (TARGET_EXPR_CLEANUP (t) && !CLEANUP_EH_ONLY (t))
	  ctx->global->cleanups->safe_push (TARGET_EXPR_CLEANUP (t));
	r = unshare_constructor (r);
	ctx->global->values.put (slot, r);
	if (ctx->save_exprs)
	  ctx->save_exprs->safe_push (slot);
	if (lval)
	  return slot;
      }
      break;

    case CALL_EXPR:
      r = eval_call_expression (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case RETURN_EXPR:
      if (TREE_OPERAND (t, 0) != NULL_TREE)
	r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				      non_constant_p, overflow_p);
      /* FALLTHRU */
    case BREAK_STMT:
    case CONTINUE_STMT:
      if (jump_target)
	*jump_target = t;
      else
	{
	  /* Can happen with ({ return true; }) && false; passed to
	     maybe_constant_value.  There is nothing to jump over in this
	     case, and the bug will be diagnosed later.  */
	  gcc_assert (ctx->quiet);
	  *non_constant_p = true;
	}
      break;

      case DECL_EXPR: {
	r = DECL_EXPR_DECL (t);

	if (AGGREGATE_TYPE_P (TREE_TYPE (r)) || VECTOR_TYPE_P (TREE_TYPE (r)))
	  {
	    new_ctx = *ctx;
	    new_ctx.object = r;
	    new_ctx.ctor = build_constructor (TREE_TYPE (r), NULL);
	    CONSTRUCTOR_NO_CLEARING (new_ctx.ctor) = true;
	    ctx->global->values.put (r, new_ctx.ctor);
	    ctx = &new_ctx;
	  }

	if (tree init = DECL_INITIAL (r))
	  {
	    init = eval_constant_expression (ctx, init, false, non_constant_p,
					     overflow_p);
	    /* Don't share a CONSTRUCTOR that might be changed.  */
	    init = unshare_constructor (init);
	    /* Remember that a constant object's constructor has already
	       run.  */
	    if (CLASS_TYPE_P (TREE_TYPE (r)) && RS_TYPE_CONST_P (TREE_TYPE (r)))
	      TREE_READONLY (init) = true;
	    ctx->global->values.put (r, init);
	  }
	else if (ctx == &new_ctx)
	  /* We gave it a CONSTRUCTOR above.  */;
	else
	  ctx->global->values.put (r, NULL_TREE);
      }
      break;

    /* These differ from cxx_eval_unary_expression in that this doesn't
	 check for a constant operand or result; an address can be
	 constant without its operand being, and vice versa.  */
    case MEM_REF:
    case INDIRECT_REF:
      r = rs_eval_indirect_ref (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case VEC_PERM_EXPR:
      r = cxx_eval_trinary_expression (ctx, t, lval, non_constant_p,
				       overflow_p);
      break;

    case PAREN_EXPR:
      gcc_assert (!REF_PARENTHESIZED_P (t));
      /* A PAREN_EXPR resulting from __builtin_assoc_barrier has no effect in
	 constant expressions since it's unaffected by -fassociative-math.  */
      r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				    non_constant_p, overflow_p);
      break;

    case MODIFY_EXPR:
      r = eval_store_expression (ctx, t, false, non_constant_p, overflow_p);
      break;

    case STATEMENT_LIST:
      new_ctx = *ctx;
      new_ctx.ctor = new_ctx.object = NULL_TREE;
      return eval_statement_list (&new_ctx, t, non_constant_p, overflow_p,
				  jump_target);

    case BIND_EXPR:
      return eval_constant_expression (ctx, BIND_EXPR_BODY (t), lval,
				       non_constant_p, overflow_p, jump_target);

    case OBJ_TYPE_REF:
      /* Virtual function lookup.  We don't need to do anything fancy.  */
      return eval_constant_expression (ctx, OBJ_TYPE_REF_EXPR (t), lval,
				       non_constant_p, overflow_p);

      case EXIT_EXPR: {
	tree cond = TREE_OPERAND (t, 0);
	cond = eval_constant_expression (ctx, cond, /*lval*/ false,
					 non_constant_p, overflow_p);
	VERIFY_CONSTANT (cond);
	if (integer_nonzerop (cond))
	  *jump_target = t;
      }
      break;

    case RESULT_DECL:
      if (lval)
	return t;
      /* We ask for an rvalue for the RESULT_DECL when indirecting
	 through an invisible reference, or in named return value
	 optimization.  */
      if (tree *p = ctx->global->values.get (t))
	return *p;
      else
	{
	  if (!ctx->quiet)
	    error ("%qE is not a constant expression", t);
	  *non_constant_p = true;
	}
      break;

    case SAVE_EXPR:
      /* Avoid evaluating a SAVE_EXPR more than once.  */
      if (tree *p = ctx->global->values.get (t))
	r = *p;
      else
	{
	  r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), false,
					non_constant_p, overflow_p);
	  if (*non_constant_p)
	    break;
	  ctx->global->values.put (t, r);
	  if (ctx->save_exprs)
	    ctx->save_exprs->safe_push (t);
	}
      break;

      case ADDR_EXPR: {
	tree oldop = TREE_OPERAND (t, 0);
	tree op = eval_constant_expression (ctx, oldop,
					    /*lval*/ true, non_constant_p,
					    overflow_p);
	/* Don't VERIFY_CONSTANT here.  */
	if (*non_constant_p)
	  return t;
	/* This function does more aggressive folding than fold itself.  */
	r = build_fold_addr_expr_with_type (op, TREE_TYPE (t));
	if (TREE_CODE (r) == ADDR_EXPR && TREE_OPERAND (r, 0) == oldop)
	  {
	    ggc_free (r);
	    return t;
	  }
	break;
      }

      case COMPOUND_EXPR: {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
	  r = eval_constant_expression (ctx, op0, lval, non_constant_p,
					overflow_p, jump_target);
	else
	  {
	    /* Check that the LHS is constant and then discard it.  */
	    eval_constant_expression (ctx, op0, true, non_constant_p,
				      overflow_p, jump_target);
	    if (*non_constant_p)
	      return t;
	    op1 = TREE_OPERAND (t, 1);
	    r = eval_constant_expression (ctx, op1, lval, non_constant_p,
					  overflow_p, jump_target);
	  }
      }
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (lval)
	{
	  r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
					non_constant_p, overflow_p);
	  if (r == error_mark_node)
	    ;
	  else if (r == TREE_OPERAND (t, 0))
	    r = t;
	  else
	    r = fold_build1 (TREE_CODE (t), TREE_TYPE (t), r);
	  break;
	}
      /* FALLTHRU */
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case ABSU_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
      r = eval_unary_expression (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case LOOP_EXPR:
    case WHILE_STMT:
    case FOR_STMT:
      eval_loop_expr (ctx, t, non_constant_p, overflow_p, jump_target);
      break;

    case SWITCH_EXPR:
    case SWITCH_STMT:
      eval_switch_expr (ctx, t, non_constant_p, overflow_p, jump_target);
      break;

    case ARRAY_REF:
      r = eval_array_reference (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case COMPONENT_REF:
      if (is_overloaded_fn (t))
	{
	  /* We can only get here in checking mode via
	     build_non_dependent_expr,  because any expression that
	     calls or takes the address of the function will have
	     pulled a FUNCTION_DECL out of the COMPONENT_REF.  */
	  gcc_checking_assert (ctx->quiet || errorcount);
	  *non_constant_p = true;
	  return t;
	}
      r = eval_component_reference (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case BIT_FIELD_REF:
      r = eval_bit_field_ref (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case COND_EXPR:
    case IF_STMT: // comes from cp-tree.def
      if (jump_target && *jump_target)
	{
	  tree orig_jump = *jump_target;
	  tree arg = ((TREE_CODE (t) != IF_STMT || TREE_OPERAND (t, 1))
			? TREE_OPERAND (t, 1)
			: void_node);
	  /* When jumping to a label, the label might be either in the
	     then or else blocks, so process then block first in skipping
	     mode first, and if we are still in the skipping mode at its end,
	     process the else block too.  */
	  r = eval_constant_expression (ctx, arg, lval, non_constant_p,
					overflow_p, jump_target);
	  /* It's possible that we found the label in the then block.  But
	     it could have been followed by another jumping statement, e.g.
	     say we're looking for case 1:
	      if (cond)
		{
		  // skipped statements
		  case 1:; // clears up *jump_target
		  return 1; // and sets it to a RETURN_EXPR
		}
	      else { ... }
	     in which case we need not go looking to the else block.
	     (goto is not allowed in a constexpr function.)  */
	  if (*jump_target == orig_jump)
	    {
	      arg = ((TREE_CODE (t) != IF_STMT || TREE_OPERAND (t, 2))
		       ? TREE_OPERAND (t, 2)
		       : void_node);
	      r = eval_constant_expression (ctx, arg, lval, non_constant_p,
					    overflow_p, jump_target);
	    }
	  break;
	}
      r = eval_conditional_expression (ctx, t, lval, non_constant_p, overflow_p,
				       jump_target);
      break;

    case VEC_COND_EXPR:
      r = eval_vector_conditional_expression (ctx, t, non_constant_p,
					      overflow_p);
      break;

    case TRY_CATCH_EXPR:
      if (TREE_OPERAND (t, 0) == NULL_TREE)
	{
	  r = void_node;
	  break;
	}
      r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				    non_constant_p, overflow_p, jump_target);
      break;

      case CLEANUP_POINT_EXPR: {
	auto_vec<tree, 2> cleanups;
	vec<tree> *prev_cleanups = ctx->global->cleanups;
	ctx->global->cleanups = &cleanups;
	r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				      non_constant_p, overflow_p, jump_target);
	ctx->global->cleanups = prev_cleanups;
	unsigned int i;
	tree cleanup;
	/* Evaluate the cleanups.  */
	FOR_EACH_VEC_ELT_REVERSE (cleanups, i, cleanup)
	  eval_constant_expression (ctx, cleanup, false, non_constant_p,
				    overflow_p);
      }
      break;

    case TRY_FINALLY_EXPR:
      r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				    non_constant_p, overflow_p, jump_target);
      if (!*non_constant_p)
	/* Also evaluate the cleanup.  */
	eval_constant_expression (ctx, TREE_OPERAND (t, 1), true,
				  non_constant_p, overflow_p);
      break;

    case CONSTRUCTOR:
      if (TREE_CONSTANT (t) && reduced_constant_expression_p (t))
	{
	  /* Don't re-process a constant CONSTRUCTOR, but do fold it to
	     VECTOR_CST if applicable.  */
	  verify_constructor_flags (t);
	  if (TREE_CONSTANT (t))
	    return fold (t);
	}
      r = eval_bare_aggregate (ctx, t, lval, non_constant_p, overflow_p);
      break;

      /* FALLTHROUGH.  */
    case NOP_EXPR:
    case CONVERT_EXPR:
      case VIEW_CONVERT_EXPR: {
	tree oldop = TREE_OPERAND (t, 0);

	tree op = eval_constant_expression (ctx, oldop, lval, non_constant_p,
					    overflow_p);
	if (*non_constant_p)
	  return t;
	tree type = TREE_TYPE (t);

	if (VOID_TYPE_P (type))
	  return void_node;

	if (TREE_CODE (t) == CONVERT_EXPR && ARITHMETIC_TYPE_P (type)
	    && INDIRECT_TYPE_P (TREE_TYPE (op)) && ctx->manifestly_const_eval)
	  {
	    if (!ctx->quiet)
	      error_at (loc,
			"conversion from pointer type %qT to arithmetic type "
			"%qT in a constant expression",
			TREE_TYPE (op), type);
	    *non_constant_p = true;
	    return t;
	  }

	if (TYPE_PTROB_P (type) && TYPE_PTR_P (TREE_TYPE (op))
	    && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (op))))
	  {
	    /* Likewise, don't error when casting from void* when OP is
	       &heap uninit and similar.  */
	    tree sop = tree_strip_nop_conversions (op);
	    if (TREE_CODE (sop) == ADDR_EXPR && VAR_P (TREE_OPERAND (sop, 0))
		&& DECL_ARTIFICIAL (TREE_OPERAND (sop, 0)))
	      /* OK */;
	    else
	      {
		if (!ctx->quiet)
		  error_at (loc, "cast from %qT is not allowed",
			    TREE_TYPE (op));
		*non_constant_p = true;
		return t;
	      }
	  }

	if (INDIRECT_TYPE_P (type) && TREE_CODE (op) == INTEGER_CST)
	  {
	    if (integer_zerop (op))
	      {
		if (TYPE_REF_P (type))
		  {
		    if (!ctx->quiet)
		      error_at (loc, "dereferencing a null pointer");
		    *non_constant_p = true;
		    return t;
		  }
	      }
	    else
	      {
		/* This detects for example:
		     reinterpret_cast<void*>(sizeof 0)
		*/
		if (!ctx->quiet)
		  error_at (loc,
			    "%<reinterpret_cast<%T>(%E)%> is not "
			    "a constant expression",
			    type, op);
		*non_constant_p = true;
		return t;
	      }
	  }

	if (INDIRECT_TYPE_P (type) && TREE_CODE (op) == NOP_EXPR
	    && TREE_TYPE (op) == ptr_type_node
	    && TREE_CODE (TREE_OPERAND (op, 0)) == ADDR_EXPR
	    && VAR_P (TREE_OPERAND (TREE_OPERAND (op, 0), 0))
	    && (DECL_NAME (TREE_OPERAND (TREE_OPERAND (op, 0), 0))
		  == heap_uninit_identifier
		|| DECL_NAME (TREE_OPERAND (TREE_OPERAND (op, 0), 0))
		     == heap_vec_uninit_identifier))
	  {
	    tree var = TREE_OPERAND (TREE_OPERAND (op, 0), 0);
	    tree var_size = TYPE_SIZE_UNIT (TREE_TYPE (var));
	    tree elt_type = TREE_TYPE (type);
	    tree cookie_size = NULL_TREE;
	    if (TREE_CODE (elt_type) == RECORD_TYPE
		&& TYPE_NAME (elt_type) == heap_identifier)
	      {
		tree fld1 = TYPE_FIELDS (elt_type);
		tree fld2 = DECL_CHAIN (fld1);
		elt_type = TREE_TYPE (TREE_TYPE (fld2));
		cookie_size = TYPE_SIZE_UNIT (TREE_TYPE (fld1));
	      }
	    DECL_NAME (var) = (DECL_NAME (var) == heap_uninit_identifier
				 ? heap_identifier
				 : heap_vec_identifier);
	    TREE_TYPE (var)
	      = build_new_constexpr_heap_type (elt_type, cookie_size, var_size);
	    TREE_TYPE (TREE_OPERAND (op, 0))
	      = build_pointer_type (TREE_TYPE (var));
	  }

	if (op == oldop)
	  /* We didn't fold at the top so we could check for ptr-int
	     conversion.  */
	  return fold (t);

	tree sop;

	/* Handle an array's bounds having been deduced after we built
	   the wrapping expression.  */
	if (same_type_ignoring_tlq_and_bounds_p (type, TREE_TYPE (op)))
	  r = op;
	else if (sop = tree_strip_nop_conversions (op),
		 sop != op
		   && (same_type_ignoring_tlq_and_bounds_p (type,
							    TREE_TYPE (sop))))
	  r = sop;
	else
	  r = fold_build1 (tcode, type, op);

	/* Conversion of an out-of-range value has implementation-defined
	   behavior; the language considers it different from arithmetic
	   overflow, which is undefined.  */
	if (TREE_OVERFLOW_P (r) && !TREE_OVERFLOW_P (op))
	  TREE_OVERFLOW (r) = false;
      }
      break;

    case PLACEHOLDER_EXPR:
      /* Use of the value or address of the current object.  */
      if (tree ctor = lookup_placeholder (ctx, lval, TREE_TYPE (t)))
	{
	  if (TREE_CODE (ctor) == CONSTRUCTOR)
	    return ctor;
	  else
	    return eval_constant_expression (ctx, ctor, lval, non_constant_p,
					     overflow_p);
	}
      /* A placeholder without a referent.  We can get here when
	 checking whether NSDMIs are noexcept, or in massage_init_elt;
	 just say it's non-constant for now.  */
      gcc_assert (ctx->quiet);
      *non_constant_p = true;
      break;

    case ANNOTATE_EXPR:
      r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				    non_constant_p, overflow_p, jump_target);
      break;

    case ASM_EXPR:
      if (!ctx->quiet)
	inline_asm_in_constexpr_error (loc);
      *non_constant_p = true;
      return t;

    default:
      break;
    }

  return r;
}

/* Complain about a const object OBJ being modified in a constant expression.
   EXPR is the MODIFY_EXPR expression performing the modification.  */

static void
modifying_const_object_error (tree expr, tree obj)
{
  location_t loc = EXPR_LOCATION (expr);
  auto_diagnostic_group d;
  error_at (loc,
	    "modifying a const object %qE is not allowed in "
	    "a constant expression",
	    TREE_OPERAND (expr, 0));
  inform (location_of (obj), "originally declared %<const%> here");
}

/* Return true iff DECL is an empty field, either for an empty base or a
   [[no_unique_address]] data member.  */

bool
is_empty_field (tree decl)
{
  if (!decl || TREE_CODE (decl) != FIELD_DECL)
    return false;

  bool r = is_empty_class (TREE_TYPE (decl));

  /* Empty fields should have size zero.  */
  gcc_checking_assert (!r || integer_zerop (DECL_SIZE (decl)));

  return r;
}

static tree
eval_store_expression (const constexpr_ctx *ctx, tree t, bool lval,
		       bool *non_constant_p, bool *overflow_p)
{
  constexpr_ctx new_ctx = *ctx;

  tree init = TREE_OPERAND (t, 1);
  if (TREE_CLOBBER_P (init))
    /* Just ignore clobbers.  */
    return void_node;

  /* First we figure out where we're storing to.  */
  tree target = TREE_OPERAND (t, 0);

  tree type = TREE_TYPE (target);
  bool preeval = SCALAR_TYPE_P (type) || TREE_CODE (t) == MODIFY_EXPR;
  if (preeval)
    {
      /* Evaluate the value to be stored without knowing what object it will be
	 stored in, so that any side-effects happen first.  */
      if (!SCALAR_TYPE_P (type))
	new_ctx.ctor = new_ctx.object = NULL_TREE;
      init = eval_constant_expression (&new_ctx, init, false, non_constant_p,
				       overflow_p);
      if (*non_constant_p)
	return t;
    }

  bool evaluated = false;
  if (lval)
    {
      /* If we want to return a reference to the target, we need to evaluate it
	 as a whole; otherwise, only evaluate the innermost piece to avoid
	 building up unnecessary *_REFs.  */
      target = eval_constant_expression (ctx, target, true, non_constant_p,
					 overflow_p);
      evaluated = true;
      if (*non_constant_p)
	return t;
    }

  /* Find the underlying variable.  */
  releasing_vec refs;
  tree object = NULL_TREE;
  /* If we're modifying a const object, save it.  */
  tree const_object_being_modified = NULL_TREE;
  // bool mutable_p = false;
  for (tree probe = target; object == NULL_TREE;)
    {
      switch (TREE_CODE (probe))
	{
	case BIT_FIELD_REF:
	case COMPONENT_REF:
	  case ARRAY_REF: {
	    tree ob = TREE_OPERAND (probe, 0);
	    tree elt = TREE_OPERAND (probe, 1);
	    if (TREE_CODE (elt) == FIELD_DECL /*&& DECL_MUTABLE_P (elt)*/)
	      {
		// mutable_p = true;
	      }
	    if (TREE_CODE (probe) == ARRAY_REF)
	      {
		elt = eval_and_check_array_index (ctx, probe, false,
						  non_constant_p, overflow_p);
		if (*non_constant_p)
		  return t;
	      }
	    /* We don't check modifying_const_object_p for ARRAY_REFs.  Given
	       "int a[10]", an ARRAY_REF "a[2]" can be "const int", even though
	       the array isn't const.  Instead, check "a" in the next iteration;
	       that will detect modifying "const int a[10]".  */
	    // else if (evaluated
	    //          && modifying_const_object_p (TREE_CODE (t), probe,
	    //     				  mutable_p)
	    //          && const_object_being_modified == NULL_TREE)
	    //   const_object_being_modified = probe;
	    vec_safe_push (refs, elt);
	    vec_safe_push (refs, TREE_TYPE (probe));
	    probe = ob;
	  }
	  break;

	default:
	  if (evaluated)
	    object = probe;
	  else
	    {
	      probe = eval_constant_expression (ctx, probe, true,
						non_constant_p, overflow_p);
	      evaluated = true;
	      if (*non_constant_p)
		return t;
	    }
	  break;
	}
    }

  // if (modifying_const_object_p (TREE_CODE (t), object, mutable_p)
  //   && const_object_being_modified == NULL_TREE)
  // const_object_being_modified = object;

  /* And then find/build up our initializer for the path to the subobject
     we're initializing.  */
  tree *valp;
  if (DECL_P (object))
    valp = ctx->global->values.get (object);
  else
    valp = NULL;
  if (!valp)
    {
      /* A constant-expression cannot modify objects from outside the
	 constant-expression.  */
      if (!ctx->quiet)
	error ("modification of %qE is not a constant expression", object);
      *non_constant_p = true;
      return t;
    }
  type = TREE_TYPE (object);
  bool no_zero_init = true;

  releasing_vec ctors, indexes;
  auto_vec<int> index_pos_hints;
  bool activated_union_member_p = false;
  while (!refs->is_empty ())
    {
      if (*valp == NULL_TREE)
	{
	  *valp = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_CLEARING (*valp) = no_zero_init;
	}
      else if (TREE_CODE (*valp) == STRING_CST)
	{
	  /* An array was initialized with a string constant, and now
	     we're writing into one of its elements.  Explode the
	     single initialization into a set of element
	     initializations.  */
	  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);

	  tree string = *valp;
	  tree elt_type = TREE_TYPE (type);
	  unsigned chars_per_elt
	    = (TYPE_PRECISION (elt_type) / TYPE_PRECISION (char_type_node));
	  unsigned num_elts = TREE_STRING_LENGTH (string) / chars_per_elt;
	  tree ary_ctor = build_constructor (type, NULL);

	  vec_safe_reserve (CONSTRUCTOR_ELTS (ary_ctor), num_elts);
	  for (unsigned ix = 0; ix != num_elts; ix++)
	    {
	      constructor_elt elt
		= {build_int_cst (size_type_node, ix),
		   extract_string_elt (string, chars_per_elt, ix)};
	      CONSTRUCTOR_ELTS (ary_ctor)->quick_push (elt);
	    }

	  *valp = ary_ctor;
	}

      /* If the value of object is already zero-initialized, any new ctors for
	 subobjects will also be zero-initialized.  */
      no_zero_init = CONSTRUCTOR_NO_CLEARING (*valp);

      enum tree_code code = TREE_CODE (type);
      type = refs->pop ();
      tree index = refs->pop ();

      if (code == RECORD_TYPE && is_empty_field (index))
	/* Don't build a sub-CONSTRUCTOR for an empty base or field, as they
	   have no data and might have an offset lower than previously declared
	   fields, which confuses the middle-end.  The code below will notice
	   that we don't have a CONSTRUCTOR for our inner target and just
	   return init.  */
	break;

      if (code == UNION_TYPE && CONSTRUCTOR_NELTS (*valp)
	  && CONSTRUCTOR_ELT (*valp, 0)->index != index)
	{
	  if (TREE_CODE (t) == MODIFY_EXPR && CONSTRUCTOR_NO_CLEARING (*valp))
	    {
	      /* Diagnose changing the active union member while the union
		 is in the process of being initialized.  */
	      if (!ctx->quiet)
		error_at (EXPR_LOCATION (t),
			  "change of the active member of a union "
			  "from %qD to %qD during initialization",
			  CONSTRUCTOR_ELT (*valp, 0)->index, index);
	      *non_constant_p = true;
	    }
	  no_zero_init = true;
	}

      vec_safe_push (ctors, *valp);
      vec_safe_push (indexes, index);

      constructor_elt *cep = get_or_insert_ctor_field (*valp, index);
      index_pos_hints.safe_push (cep - CONSTRUCTOR_ELTS (*valp)->begin ());

      if (code == UNION_TYPE)
	activated_union_member_p = true;

      valp = &cep->value;
    }

  /* Detect modifying a constant object in constexpr evaluation.
     We have found a const object that is being modified.  Figure out
     if we need to issue an error.  Consider

     struct A {
       int n;
       constexpr A() : n(1) { n = 2; } // #1
     };
     struct B {
       const A a;
       constexpr B() { a.n = 3; } // #2
     };
    constexpr B b{};

    #1 is OK, since we're modifying an object under construction, but
    #2 is wrong, since "a" is const and has been fully constructed.
    To track it, we use the TREE_READONLY bit in the object's CONSTRUCTOR
    which means that the object is read-only.  For the example above, the
    *ctors stack at the point of #2 will look like:

      ctors[0] = {.a={.n=2}}  TREE_READONLY = 0
      ctors[1] = {.n=2}       TREE_READONLY = 1

    and we're modifying "b.a", so we search the stack and see if the
    constructor for "b.a" has already run.  */
  if (const_object_being_modified)
    {
      bool fail = false;
      tree const_objtype
	= strip_array_types (TREE_TYPE (const_object_being_modified));
      if (!CLASS_TYPE_P (const_objtype))
	fail = true;
      else
	{
	  /* [class.ctor]p5 "A constructor can be invoked for a const,
	     volatile, or const volatile object.  const and volatile
	     semantics are not applied on an object under construction.
	     They come into effect when the constructor for the most
	     derived object ends."  */
	  for (tree elt : *ctors)
	    if (same_type_ignoring_top_level_qualifiers_p (
		  TREE_TYPE (const_object_being_modified), TREE_TYPE (elt)))
	      {
		fail = TREE_READONLY (elt);
		break;
	      }
	}
      if (fail)
	{
	  if (!ctx->quiet)
	    modifying_const_object_error (t, const_object_being_modified);
	  *non_constant_p = true;
	  return t;
	}
    }

  if (!preeval)
    {
      /* We're handling an INIT_EXPR of class type, so the value of the
	 initializer can depend on the object it's initializing.  */

      /* Create a new CONSTRUCTOR in case evaluation of the initializer
	 wants to modify it.  */
      if (*valp == NULL_TREE)
	{
	  *valp = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_CLEARING (*valp) = no_zero_init;
	}
      new_ctx.ctor = *valp;
      new_ctx.object = target;
      /* Avoid temporary materialization when initializing from a TARGET_EXPR.
	 We don't need to mess with AGGR_EXPR_SLOT/VEC_INIT_EXPR_SLOT because
	 expansion of those trees uses ctx instead.  */
      if (TREE_CODE (init) == TARGET_EXPR)
	if (tree tinit = TARGET_EXPR_INITIAL (init))
	  init = tinit;
      init = eval_constant_expression (&new_ctx, init, false, non_constant_p,
				       overflow_p);
      /* The hash table might have moved since the get earlier, and the
	 initializer might have mutated the underlying CONSTRUCTORs, so we must
	 recompute VALP. */
      valp = ctx->global->values.get (object);
      for (unsigned i = 0; i < vec_safe_length (indexes); i++)
	{
	  constructor_elt *cep
	    = get_or_insert_ctor_field (*valp, indexes[i], index_pos_hints[i]);
	  valp = &cep->value;
	}
    }

  if (*non_constant_p)
    return t;

  /* Don't share a CONSTRUCTOR that might be changed later.  */
  init = unshare_constructor (init);
  if (init == NULL_TREE)
    return t;

  if (*valp && TREE_CODE (*valp) == CONSTRUCTOR
      && TREE_CODE (init) == CONSTRUCTOR)
    {
      /* An outer ctx->ctor might be pointing to *valp, so replace
	 its contents.  */
      if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (init),
						      TREE_TYPE (*valp)))
	{
	  /* For initialization of an empty base, the original target will be
	   *(base*)this, evaluation of which resolves to the object
	   argument, which has the derived type rather than the base type.  In
	   this situation, just evaluate the initializer and return, since
	   there's no actual data to store.  */
	  gcc_assert (is_empty_class (TREE_TYPE (init)));
	  return lval ? target : init;
	}
      CONSTRUCTOR_ELTS (*valp) = CONSTRUCTOR_ELTS (init);
      TREE_CONSTANT (*valp) = TREE_CONSTANT (init);
      TREE_SIDE_EFFECTS (*valp) = TREE_SIDE_EFFECTS (init);
      CONSTRUCTOR_NO_CLEARING (*valp) = CONSTRUCTOR_NO_CLEARING (init);
    }
  // else if (TREE_CODE (init) == CONSTRUCTOR
  //          && !same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (init),
  //       						  type))
  //   {
  //     /* See above on initialization of empty bases.  */
  //     // gcc_assert (is_empty_class (TREE_TYPE (init)) && !lval);
  //     return init;
  //   }
  else
    *valp = init;

  /* After initialization, 'const' semantics apply to the value of the
     object.  Make a note of this fact by marking the CONSTRUCTOR
     TREE_READONLY.  */
  if (TREE_CODE (t) == INIT_EXPR && TREE_CODE (*valp) == CONSTRUCTOR
      && TYPE_READONLY (type))
    {
      // this vs self? can rust's self be anything other than self or &self in
      // constexpr mode? if (INDIRECT_REF_P (target)
      //     && (is_this_parameter (
      //       tree_strip_nop_conversions (TREE_OPERAND (target, 0)))))
      /* We've just initialized '*this' (perhaps via the target
	 constructor of a delegating constructor).  Leave it up to the
	 caller that set 'this' to set TREE_READONLY appropriately.  */
      //   gcc_checking_assert (
      //     same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (target),
      //     type));
      // else
      //   TREE_READONLY (*valp) = true;
    }

  /* Update TREE_CONSTANT and TREE_SIDE_EFFECTS on enclosing
     CONSTRUCTORs, if any.  */
  bool c = TREE_CONSTANT (init);
  bool s = TREE_SIDE_EFFECTS (init);
  if (!c || s || activated_union_member_p)
    for (tree elt : *ctors)
      {
	if (!c)
	  TREE_CONSTANT (elt) = false;
	if (s)
	  TREE_SIDE_EFFECTS (elt) = true;
	/* Clear CONSTRUCTOR_NO_CLEARING since we've activated a member of
	   this union.  */
	if (TREE_CODE (TREE_TYPE (elt)) == UNION_TYPE)
	  CONSTRUCTOR_NO_CLEARING (elt) = false;
      }

  if (*non_constant_p)
    return t;
  else if (lval)
    return target;
  else
    return init;
}

/* Subroutine of cxx_eval_constant_expression.
 Like cxx_eval_unary_expression, except for binary expressions.  */
static tree
eval_binary_expression (const constexpr_ctx *ctx, tree t, bool lval,
			bool *non_constant_p, bool *overflow_p)
{
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;

  lhs = eval_constant_expression (ctx, orig_lhs, lval, non_constant_p,
				  overflow_p);
  rhs = eval_constant_expression (ctx, orig_rhs, lval, non_constant_p,
				  overflow_p);

  location_t loc = EXPR_LOCATION (t);
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);

  return fold_binary_loc (loc, code, type, lhs, rhs);
}

/* Helper function of cxx_bind_parameters_in_call.  Return non-NULL
   if *TP is address of a static variable (or part of it) currently being
   constructed or of a heap artificial variable.  */

static tree
addr_of_non_const_var (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == ADDR_EXPR)
    if (tree var = get_base_address (TREE_OPERAND (*tp, 0)))
      if (VAR_P (var) && TREE_STATIC (var))
	{
	  if (DECL_NAME (var) == heap_uninit_identifier
	      || DECL_NAME (var) == heap_identifier
	      || DECL_NAME (var) == heap_vec_uninit_identifier
	      || DECL_NAME (var) == heap_vec_identifier)
	    return var;

	  constexpr_global_ctx *global = (constexpr_global_ctx *) data;
	  if (global->values.get (var))
	    return var;
	}
  if (TYPE_P (*tp))
    *walk_subtrees = false;
  return NULL_TREE;
}

/* Subroutine of cxx_eval_call_expression.
   We are processing a call expression (either CALL_EXPR or
   AGGR_INIT_EXPR) in the context of CTX.  Evaluate
   all arguments and bind their values to correspondings
   parameters, making up the NEW_CALL context.  */

static tree
rs_bind_parameters_in_call (const constexpr_ctx *ctx, tree t, tree fun,
			    bool *non_constant_p, bool *overflow_p,
			    bool *non_constant_args)
{
  const int nargs = call_expr_nargs (t);
  tree parms = DECL_ARGUMENTS (fun);
  int i;
  /* We don't record ellipsis args below.  */
  int nparms = list_length (parms);
  int nbinds = nargs < nparms ? nargs : nparms;
  tree binds = make_tree_vec (nbinds);
  for (i = 0; i < nargs; ++i)
    {
      tree x, arg;
      tree type = parms ? TREE_TYPE (parms) : void_type_node;
      if (parms && DECL_BY_REFERENCE (parms))
	type = TREE_TYPE (type);
      x = CALL_EXPR_ARG (t, i);

      if (TREE_ADDRESSABLE (type))
	/* Undo convert_for_arg_passing work here.  */
	x = convert_from_reference (x);
      /* Normally we would strip a TARGET_EXPR in an initialization context
	 such as this, but here we do the elision differently: we keep the
	 TARGET_EXPR, and use its CONSTRUCTOR as the value of the parm.  */
      arg = eval_constant_expression (ctx, x, /*lval=*/false, non_constant_p,
				      overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p && ctx->quiet)
	break;
      /* Just discard ellipsis args after checking their constantitude.  */
      if (!parms)
	continue;

      if (!*non_constant_p)
	{
	  /* Make sure the binding has the same type as the parm.  But
	     only for constant args.  */
	  if (!TYPE_REF_P (type))
	    arg = adjust_temp_type (type, arg);
	  if (!TREE_CONSTANT (arg))
	    *non_constant_args = true;
	  else if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	    /* The destructor needs to see any modifications the callee makes
	       to the argument.  */
	    *non_constant_args = true;
	  /* If arg is or contains address of a heap artificial variable or
	     of a static variable being constructed, avoid caching the
	     function call, as those variables might be modified by the
	     function, or might be modified by the callers in between
	     the cached function and just read by the function.  */
	  else if (!*non_constant_args
		   && rs_walk_tree (&arg, addr_of_non_const_var, ctx->global,
				    NULL))
	    *non_constant_args = true;

	  // /* For virtual calls, adjust the this argument, so that it is
	  //    the object on which the method is called, rather than
	  //    one of its bases.  */
	  // if (i == 0 && DECL_VIRTUAL_P (fun))
	  //   {
	  //     tree addr = arg;
	  //     STRIP_NOPS (addr);
	  //     if (TREE_CODE (addr) == ADDR_EXPR)
	  //       {
	  //         tree obj = TREE_OPERAND (addr, 0);
	  //         while (TREE_CODE (obj) == COMPONENT_REF
	  //       	 && DECL_FIELD_IS_BASE (TREE_OPERAND (obj, 1))
	  //       	 && !same_type_ignoring_top_level_qualifiers_p (
	  //       	   TREE_TYPE (obj), DECL_CONTEXT (fun)))
	  //           obj = TREE_OPERAND (obj, 0);
	  //         if (obj != TREE_OPERAND (addr, 0))
	  //           arg = build_fold_addr_expr_with_type (obj, TREE_TYPE
	  //           (arg));
	  //       }
	  //   }
	  TREE_VEC_ELT (binds, i) = arg;
	}
      parms = TREE_CHAIN (parms);
    }

  return binds;
}

// forked from gcc/cp/constexpr.cc cxx_eval_builtin_function_call

/* Attempt to evaluate T which represents a call to a builtin function.
   We assume here that all builtin functions evaluate to scalar types
   represented by _CST nodes.  */

static tree
eval_builtin_function_call (const constexpr_ctx *ctx, tree t, tree fun,
			    bool lval, bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree *args = (tree *) alloca (nargs * sizeof (tree));
  tree new_call;
  int i;

  /* Don't fold __builtin_constant_p within a constexpr function.  */
  bool bi_const_p = DECL_IS_BUILTIN_CONSTANT_P (fun);

  /* If we aren't requiring a constant expression, defer __builtin_constant_p
     in a constexpr function until we have values for the parameters.  */
  if (bi_const_p && !ctx->manifestly_const_eval && current_function_decl
      && DECL_DECLARED_CONSTEXPR_P (current_function_decl))
    {
      *non_constant_p = true;
      return t;
    }

  /* For __builtin_is_constant_evaluated, defer it if not
     ctx->manifestly_const_eval (as sometimes we try to constant evaluate
     without manifestly_const_eval even expressions or parts thereof which
     will later be manifestly const_eval evaluated), otherwise fold it to
     true.  */
  if (fndecl_built_in_p (fun, CP_BUILT_IN_IS_CONSTANT_EVALUATED,
			 BUILT_IN_FRONTEND))
    {
      if (!ctx->manifestly_const_eval)
	{
	  *non_constant_p = true;
	  return t;
	}
      return boolean_true_node;
    }

  if (fndecl_built_in_p (fun, CP_BUILT_IN_SOURCE_LOCATION, BUILT_IN_FRONTEND))
    {
      temp_override<tree> ovr (current_function_decl);
      if (ctx->call && ctx->call->fundef)
	current_function_decl = ctx->call->fundef->decl;
      return fold_builtin_source_location (EXPR_LOCATION (t));
    }

  int strops = 0;
  int strret = 0;
  if (fndecl_built_in_p (fun, BUILT_IN_NORMAL))
    switch (DECL_FUNCTION_CODE (fun))
      {
      case BUILT_IN_STRLEN:
      case BUILT_IN_STRNLEN:
	strops = 1;
	break;
      case BUILT_IN_MEMCHR:
      case BUILT_IN_STRCHR:
      case BUILT_IN_STRRCHR:
	strops = 1;
	strret = 1;
	break;
      case BUILT_IN_MEMCMP:
      case BUILT_IN_STRCMP:
	strops = 2;
	break;
      case BUILT_IN_STRSTR:
	strops = 2;
	strret = 1;
	break;
      case BUILT_IN_ASAN_POINTER_COMPARE:
      case BUILT_IN_ASAN_POINTER_SUBTRACT:
	/* These builtins shall be ignored during constant expression
	   evaluation.  */
	return void_node;
      default:
	break;
      }

  /* Be permissive for arguments to built-ins; __builtin_constant_p should
     return constant false for a non-constant argument.  */
  constexpr_ctx new_ctx = *ctx;
  new_ctx.quiet = true;
  for (i = 0; i < nargs; ++i)
    {
      tree arg = CALL_EXPR_ARG (t, i);
      tree oarg = arg;

      /* To handle string built-ins we need to pass ADDR_EXPR<STRING_CST> since
	 expand_builtin doesn't know how to look in the values table.  */
      bool strop = i < strops;
      if (strop)
	{
	  STRIP_NOPS (arg);
	  if (TREE_CODE (arg) == ADDR_EXPR)
	    arg = TREE_OPERAND (arg, 0);
	  else
	    strop = false;
	}

      /* If builtin_valid_in_constant_expr_p is true,
	 potential_constant_expression_1 has not recursed into the arguments
	 of the builtin, verify it here.  */
      if (!builtin_valid_in_constant_expr_p (fun)
	  || potential_constant_expression (arg))
	{
	  bool dummy1 = false, dummy2 = false;
	  arg
	    = eval_constant_expression (&new_ctx, arg, false, &dummy1, &dummy2);
	}

      if (bi_const_p)
	/* For __builtin_constant_p, fold all expressions with constant values
	   even if they aren't C++ constant-expressions.  */
	arg = cp_fold_rvalue (arg);
      else if (strop)
	{
	  if (TREE_CODE (arg) == CONSTRUCTOR)
	    arg = braced_lists_to_strings (TREE_TYPE (arg), arg);
	  if (TREE_CODE (arg) == STRING_CST)
	    arg = build_address (arg);
	  else
	    arg = oarg;
	}

      args[i] = arg;
    }

  bool save_ffbcp = force_folding_builtin_constant_p;
  force_folding_builtin_constant_p |= ctx->manifestly_const_eval;
  tree save_cur_fn = current_function_decl;
  /* Return name of ctx->call->fundef->decl for __builtin_FUNCTION ().  */
  if (fndecl_built_in_p (fun, BUILT_IN_FUNCTION) && ctx->call
      && ctx->call->fundef)
    current_function_decl = ctx->call->fundef->decl;
  if (fndecl_built_in_p (fun,
			 CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS,
			 BUILT_IN_FRONTEND))
    {
      location_t loc = EXPR_LOCATION (t);
      if (nargs >= 1)
	VERIFY_CONSTANT (args[0]);
      new_call
	= fold_builtin_is_pointer_inverconvertible_with_class (loc, nargs,
							       args);
    }
  else if (fndecl_built_in_p (fun, CP_BUILT_IN_IS_CORRESPONDING_MEMBER,
			      BUILT_IN_FRONTEND))
    {
      location_t loc = EXPR_LOCATION (t);
      if (nargs >= 2)
	{
	  VERIFY_CONSTANT (args[0]);
	  VERIFY_CONSTANT (args[1]);
	}
      new_call = fold_builtin_is_corresponding_member (loc, nargs, args);
    }
  else
    new_call = fold_builtin_call_array (EXPR_LOCATION (t), TREE_TYPE (t),
					CALL_EXPR_FN (t), nargs, args);
  current_function_decl = save_cur_fn;
  force_folding_builtin_constant_p = save_ffbcp;
  if (new_call == NULL)
    {
      if (!*non_constant_p && !ctx->quiet)
	{
	  /* Do not allow__builtin_unreachable in constexpr function.
	     The __builtin_unreachable call with BUILTINS_LOCATION
	     comes from cp_maybe_instrument_return.  */
	  if (fndecl_built_in_p (fun, BUILT_IN_UNREACHABLE)
	      && EXPR_LOCATION (t) == BUILTINS_LOCATION)
	    error ("%<constexpr%> call flows off the end of the function");
	  else
	    {
	      new_call = build_call_array_loc (EXPR_LOCATION (t), TREE_TYPE (t),
					       CALL_EXPR_FN (t), nargs, args);
	      error ("%q+E is not a constant expression", new_call);
	    }
	}
      *non_constant_p = true;
      return t;
    }

  if (!potential_constant_expression (new_call))
    {
      if (!*non_constant_p && !ctx->quiet)
	error ("%q+E is not a constant expression", new_call);
      *non_constant_p = true;
      return t;
    }

  if (strret)
    {
      /* memchr returns a pointer into the first argument, but we replaced the
	 argument above with a STRING_CST; put it back it now.  */
      tree op = CALL_EXPR_ARG (t, strret - 1);
      STRIP_NOPS (new_call);
      if (TREE_CODE (new_call) == POINTER_PLUS_EXPR)
	TREE_OPERAND (new_call, 0) = op;
      else if (TREE_CODE (new_call) == ADDR_EXPR)
	new_call = op;
    }

  return eval_constant_expression (&new_ctx, new_call, lval, non_constant_p,
				   overflow_p);
}

// Subroutine of cxx_eval_constant_expression.
// Evaluate the call expression tree T in the context of OLD_CALL expression
// evaluation.
static tree
eval_call_expression (const constexpr_ctx *ctx, tree t, bool lval,
		      bool *non_constant_p, bool *overflow_p)
{
  location_t loc = EXPR_LOCATION (t);
  tree fun = get_function_named_in_call (t);
  rust_constexpr_call new_call
    = {NULL, NULL, NULL, 0, ctx->manifestly_const_eval};
  int depth_ok;

  if (fun == NULL_TREE)
    {
      // return cxx_eval_internal_function (ctx, t, lval,
      //     			       non_constant_p, overflow_p);
      rust_unreachable ();
      return error_mark_node;
    }

  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      if (!ctx->quiet && !*non_constant_p)
	error_at (loc,
		  "expression %qE does not designate a %<constexpr%> "
		  "function",
		  fun);
      *non_constant_p = true;
      return t;
    }

  if (fndecl_built_in_p (fun))
    return eval_builtin_function_call (ctx, t, fun, lval, non_constant_p,
				       overflow_p);

  bool non_constant_args = false;
  new_call.bindings
    = rs_bind_parameters_in_call (ctx, t, fun, non_constant_p, overflow_p,
				  &non_constant_args);

  /* We build up the bindings list before we know whether we already have this
   call cached.  If we don't end up saving these bindings, ggc_free them when
   this function exits.  */
  class free_bindings
  {
    tree *bindings;

  public:
    free_bindings (tree &b) : bindings (&b) {}
    ~free_bindings ()
    {
      if (bindings)
	ggc_free (*bindings);
    }
    void preserve () { bindings = NULL; }
  } fb (new_call.bindings);

  if (*non_constant_p)
    return t;

  /* If in direct recursive call, optimize definition search.  */
  if (ctx && ctx->call && ctx->call->fundef && ctx->call->fundef->decl == fun)
    new_call.fundef = ctx->call->fundef;
  else
    {
      new_call.fundef = retrieve_constexpr_fundef (fun);
      if (new_call.fundef == NULL || new_call.fundef->body == NULL
	  || new_call.fundef->result == error_mark_node
	  || fun == current_function_decl)
	{
	  if (!ctx->quiet)
	    {
	      /* We need to check for current_function_decl here in case we're
		 being called during cp_fold_function, because at that point
		 DECL_INITIAL is set properly and we have a fundef but we
		 haven't lowered invisirefs yet (c++/70344).  */
	      if (DECL_INITIAL (fun) == error_mark_node
		  || fun == current_function_decl)
		error_at (loc,
			  "%qD called in a constant expression before its "
			  "definition is complete",
			  fun);
	      else if (DECL_INITIAL (fun))
		{
		  // /* The definition of fun was somehow unsuitable.  But
		  // pretend
		  //    that lambda static thunks don't exist.  */
		  // if (!lambda_static_thunk_p (fun))
		  //   error_at (loc, "%qD called in a constant expression",
		  //   fun);
		  explain_invalid_constexpr_fn (fun);
		}
	      else
		error_at (loc, "%qD used before its definition", fun);
	    }
	  *non_constant_p = true;
	  return t;
	}
    }

  depth_ok = push_cx_call_context (t);

  tree result = NULL_TREE;
  rust_constexpr_call *entry = NULL;
  if (depth_ok && !non_constant_args && ctx->strict)
    {
      new_call.hash = rust_constexpr_fundef_hasher::hash (new_call.fundef);
      new_call.hash = iterative_hash_object (new_call.bindings, new_call.hash);
      new_call.hash
	= iterative_hash_object (ctx->manifestly_const_eval, new_call.hash);

      /* If we have seen this call before, we are done.  */
      maybe_initialize_constexpr_call_table ();
      rust_constexpr_call **slot
	= constexpr_call_table->find_slot (&new_call, INSERT);
      entry = *slot;
      if (entry == NULL)
	{
	  /* Only cache up to constexpr_cache_depth to limit memory use.  */
	  if (depth_ok < constexpr_cache_depth)
	    {
	      /* We need to keep a pointer to the entry, not just the slot, as
		 the slot can move during evaluation of the body.  */
	      *slot = entry = ggc_alloc<rust_constexpr_call> ();
	      *entry = new_call;
	      fb.preserve ();
	    }
	}
      /* Calls that are in progress have their result set to NULL, so that we
	 can detect circular dependencies.  Now that we only cache up to
	 constexpr_cache_depth this won't catch circular dependencies that
	 start deeper, but they'll hit the recursion or ops limit.  */
      else if (entry->result == NULL)
	{
	  if (!ctx->quiet)
	    error ("call has circular dependency");
	  *non_constant_p = true;
	  entry->result = result = error_mark_node;
	}
      else
	result = entry->result;
    }

  if (!depth_ok)
    {
      if (!ctx->quiet)
	error ("%<constexpr%> evaluation depth exceeds maximum of %d (use "
	       "%<-fconstexpr-depth=%> to increase the maximum)",
	       max_constexpr_depth);
      *non_constant_p = true;
      result = error_mark_node;
    }
  else
    {
      bool cacheable = true;
      if (result && result != error_mark_node)
	/* OK */;
      else if (!DECL_SAVED_TREE (fun))
	{
	  /* When at_eof >= 2, cgraph has started throwing away
	     DECL_SAVED_TREE, so fail quietly.  FIXME we get here because of
	     late code generation for VEC_INIT_EXPR, which needs to be
	     completely reconsidered.  */
	  // gcc_assert (at_eof >= 2 && ctx->quiet);
	  *non_constant_p = true;
	}
      else if (tree copy = get_fundef_copy (new_call.fundef))
	{
	  tree body, parms, res;
	  releasing_vec ctors;

	  /* Reuse or create a new unshared copy of this function's body.  */
	  body = TREE_PURPOSE (copy);
	  parms = TREE_VALUE (copy);
	  res = TREE_TYPE (copy);

	  /* Associate the bindings with the remapped parms.  */
	  tree bound = new_call.bindings;
	  tree remapped = parms;
	  for (int i = 0; i < TREE_VEC_LENGTH (bound); ++i)
	    {
	      tree arg = TREE_VEC_ELT (bound, i);
	      if (entry)
		{
		  /* Unshare args going into the hash table to separate them
		     from the caller's context, for better GC and to avoid
		     problems with verify_gimple.  */
		  arg = unshare_expr_without_location (arg);
		  TREE_VEC_ELT (bound, i) = arg;

		  /* And then unshare again so the callee doesn't change the
		     argument values in the hash table. XXX Could we unshare
		     lazily in cxx_eval_store_expression?  */
		  arg = unshare_constructor (arg);
		  if (TREE_CODE (arg) == CONSTRUCTOR)
		    vec_safe_push (ctors, arg);
		}

	      ctx->global->values.put (remapped, arg);
	      remapped = DECL_CHAIN (remapped);
	    }
	  /* Add the RESULT_DECL to the values map, too.  */
	  gcc_assert (!DECL_BY_REFERENCE (res));
	  ctx->global->values.put (res, NULL_TREE);

	  /* Track the callee's evaluated SAVE_EXPRs and TARGET_EXPRs so that
	     we can forget their values after the call.  */
	  constexpr_ctx ctx_with_save_exprs = *ctx;
	  auto_vec<tree, 10> save_exprs;
	  ctx_with_save_exprs.save_exprs = &save_exprs;
	  ctx_with_save_exprs.call = &new_call;
	  unsigned save_heap_alloc_count = ctx->global->heap_vars.length ();
	  unsigned save_heap_dealloc_count = ctx->global->heap_dealloc_count;

	  tree jump_target = NULL_TREE;
	  eval_constant_expression (&ctx_with_save_exprs, body, lval,
				    non_constant_p, overflow_p, &jump_target);

	  if (VOID_TYPE_P (TREE_TYPE (res)))
	    result = void_node;
	  else
	    {
	      result = *ctx->global->values.get (res);
	      if (result == NULL_TREE && !*non_constant_p)
		{
		  *non_constant_p = true;
		}
	    }

	  /* Forget the saved values of the callee's SAVE_EXPRs and
	     TARGET_EXPRs.  */
	  for (tree save_expr : save_exprs)
	    ctx->global->values.remove (save_expr);

	  /* Remove the parms/result from the values map.  Is it worth
	     bothering to do this when the map itself is only live for
	     one constexpr evaluation?  If so, maybe also clear out
	     other vars from call, maybe in BIND_EXPR handling?  */
	  ctx->global->values.remove (res);
	  for (tree parm = parms; parm; parm = TREE_CHAIN (parm))
	    ctx->global->values.remove (parm);

	  /* Make the unshared function copy we used available for re-use.  */
	  save_fundef_copy (fun, copy);

	  /* If the call allocated some heap object that hasn't been
	     deallocated during the call, or if it deallocated some heap
	     object it has not allocated, the call isn't really stateless
	     for the constexpr evaluation and should not be cached.
	     It is fine if the call allocates something and deallocates it
	     too.  */
	  if (entry
	      && (save_heap_alloc_count != ctx->global->heap_vars.length ()
		  || (save_heap_dealloc_count
		      != ctx->global->heap_dealloc_count)))
	    {
	      tree heap_var;
	      unsigned int i;
	      if ((ctx->global->heap_vars.length ()
		   - ctx->global->heap_dealloc_count)
		  != save_heap_alloc_count - save_heap_dealloc_count)
		cacheable = false;
	      else
		FOR_EACH_VEC_ELT_FROM (ctx->global->heap_vars, i, heap_var,
				       save_heap_alloc_count)
		  if (DECL_NAME (heap_var) != heap_deleted_identifier)
		    {
		      cacheable = false;
		      break;
		    }
	    }
	}
      else
	/* Couldn't get a function copy to evaluate.  */
	*non_constant_p = true;

      if (result == error_mark_node)
	*non_constant_p = true;
      if (*non_constant_p || *overflow_p)
	result = error_mark_node;
      else if (!result)
	result = void_node;
      if (entry)
	entry->result = cacheable ? result : error_mark_node;
    }

  pop_cx_call_context ();
  return result;
}

/* Subroutine of build_data_member_initialization.  MEMBER is a COMPONENT_REF
   for a member of an anonymous aggregate, INIT is the initializer for that
   member, and VEC_OUTER is the vector of constructor elements for the class
   whose constructor we are processing.  Add the initializer to the vector
   and return true to indicate success.  */

// static bool
// build_anon_member_initialization (tree member, tree init,
// 				  vec<constructor_elt, va_gc> **vec_outer)
// {
//   /* MEMBER presents the relevant fields from the inside out, but we need
//      to build up the initializer from the outside in so that we can reuse
//      previously built CONSTRUCTORs if this is, say, the second field in an
//      anonymous struct.  So we use a vec as a stack.  */
//   auto_vec<tree, 2> fields;
//   do
//     {
//       fields.safe_push (TREE_OPERAND (member, 1));
//       member = TREE_OPERAND (member, 0);
//   } while (ANON_AGGR_TYPE_P (TREE_TYPE (member))
// 	   && TREE_CODE (member) == COMPONENT_REF);
//
//   /* VEC has the constructor elements vector for the context of FIELD.
//      If FIELD is an anonymous aggregate, we will push inside it.  */
//   vec<constructor_elt, va_gc> **vec = vec_outer;
//   tree field;
//   while (field = fields.pop (), ANON_AGGR_TYPE_P (TREE_TYPE (field)))
//     {
//       tree ctor;
//       /* If there is already an outer constructor entry for the anonymous
// 	 aggregate FIELD, use it; otherwise, insert one.  */
//       if (vec_safe_is_empty (*vec) || (*vec)->last ().index != field)
// 	{
// 	  ctor = build_constructor (TREE_TYPE (field), NULL);
// 	  CONSTRUCTOR_APPEND_ELT (*vec, field, ctor);
// 	}
//       else
// 	ctor = (*vec)->last ().value;
//       vec = &CONSTRUCTOR_ELTS (ctor);
//     }
//
//   /* Now we're at the innermost field, the one that isn't an anonymous
//      aggregate.  Add its initializer to the CONSTRUCTOR and we're done.  */
//   gcc_assert (fields.is_empty ());
//   CONSTRUCTOR_APPEND_ELT (*vec, field, init);
//
//   return true;
// }

///* V is a vector of constructor elements built up for the base and member
//   initializers of a constructor for TYPE.  They need to be in increasing
//   offset order, which they might not be yet if TYPE has a primary base
//   which is not first in the base-clause or a vptr and at least one base
//   all of which are non-primary.  */
//
// static vec<constructor_elt, va_gc> *
// sort_constexpr_mem_initializers (tree type, vec<constructor_elt, va_gc> *v)
//{
//  tree pri = CLASSTYPE_PRIMARY_BINFO (type);
//  tree field_type;
//  unsigned i;
//  constructor_elt *ce;
//
//  if (pri)
//    field_type = BINFO_TYPE (pri);
//  else if (TYPE_CONTAINS_VPTR_P (type))
//    field_type = vtbl_ptr_type_node;
//  else
//    return v;
//
//  /* Find the element for the primary base or vptr and move it to the
//     beginning of the vec.  */
//  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
//    if (TREE_TYPE (ce->index) == field_type)
//      break;
//
//  if (i > 0 && i < vec_safe_length (v))
//    {
//      vec<constructor_elt, va_gc> &vref = *v;
//      constructor_elt elt = vref[i];
//      for (; i > 0; --i)
//	vref[i] = vref[i - 1];
//      vref[0] = elt;
//    }
//
//  return v;
//}

/* Subroutine of  build_constexpr_constructor_member_initializers.
   The expression tree T represents a data member initialization
   in a (constexpr) constructor definition.  Build a pairing of
   the data member with its initializer, and prepend that pair
   to the existing initialization pair INITS.  */

static bool
build_data_member_initialization (tree t, vec<constructor_elt, va_gc> **vec)
{
  tree member;
  if (TREE_CODE (t) == CLEANUP_POINT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (t == error_mark_node)
    return false;
  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      for (tree stmt : tsi_range (t))
	if (!build_data_member_initialization (stmt, vec))
	  return false;
      return true;
    }
  if (TREE_CODE (t) == CONVERT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == INIT_EXPR
      /* vptr initialization shows up as a MODIFY_EXPR.  In C++14 we only
	 use what this function builds for cx_check_missing_mem_inits, and
	 assignment in the ctor body doesn't count.  */
      || (TREE_CODE (t) == MODIFY_EXPR))
    {
      member = TREE_OPERAND (t, 0);
      // Faisal: not sure if we need to port over break_out_target_exprs
      // if not, then not sure how to handle init in this case
      // init = break_out_target_exprs (TREE_OPERAND (t, 1));
    }
  else if (TREE_CODE (t) == CALL_EXPR)
    {
      tree fn = get_callee_fndecl (t);
      if (!fn || !DECL_CONSTRUCTOR_P (fn))
	/* We're only interested in calls to subobject constructors.  */
	return true;
      member = CALL_EXPR_ARG (t, 0);
      /* We don't use build_cplus_new here because it complains about
	 abstract bases.  Leaving the call unwrapped means that it has the
	 wrong type, but cxx_eval_constant_expression doesn't care.  */
      // Faisal: not sure if we need to port over break_out_target_exprs
      // if not, then not sure how to handle init in this case
      // init = break_out_target_exprs (t);
    }
  else if (TREE_CODE (t) == BIND_EXPR)
    return build_data_member_initialization (BIND_EXPR_BODY (t), vec);
  else
    /* Don't add anything else to the CONSTRUCTOR.  */
    return true;
  if (INDIRECT_REF_P (member))
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == NOP_EXPR)
    {
      tree op = member;
      STRIP_NOPS (op);
      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p (
	    TREE_TYPE (TREE_TYPE (op)), TREE_TYPE (TREE_TYPE (member))));
	  /* Initializing a cv-qualified member; we need to look through
	     the const_cast.  */
	  member = op;
	}
      else if (op == current_class_ptr
	       && (same_type_ignoring_top_level_qualifiers_p (
		 TREE_TYPE (TREE_TYPE (member)), current_class_type)))
	/* Delegating constructor.  */
	member = op;
      else
	{
	  /* This is an initializer for an empty base; keep it for now so
	     we can check it in cxx_eval_bare_aggregate.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (member))));
	}
    }
  if (TREE_CODE (member) == ADDR_EXPR)
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == COMPONENT_REF)
    {
      tree aggr = TREE_OPERAND (member, 0);
      if (TREE_CODE (aggr) == VAR_DECL)
	/* Initializing a local variable, don't add anything.  */
	return true;
      if (TREE_CODE (aggr) != COMPONENT_REF)
	/* Normal member initialization.  */
	member = TREE_OPERAND (member, 1);
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (aggr)))
	/* Initializing a member of an anonymous union.  */
	rust_sorry_at (UNDEF_LOCATION,
		       "cannot handle value initialization yet");
      // return build_anon_member_initialization (member, init, vec);
      else
	/* We're initializing a vtable pointer in a base.  Leave it as
	   COMPONENT_REF so we remember the path to get to the vfield.  */
	gcc_assert (TREE_TYPE (member) == vtbl_ptr_type_node);
    }

  /* Value-initialization can produce multiple initializers for the
     same field; use the last one.  */
  if (!vec_safe_is_empty (*vec) && (*vec)->last ().index == member)
    rust_sorry_at (UNDEF_LOCATION, "cannot handle value initialization yet");
  // (*vec)->last ().value = init;
  else
    rust_sorry_at (UNDEF_LOCATION, "cannot handle value initialization yet");
  // CONSTRUCTOR_APPEND_ELT (*vec, member, init);
  return true;
}

///* Build compile-time evalable representations of member-initializer list
//   for a constexpr constructor.  */
//
// static tree
// build_constexpr_constructor_member_initializers (tree type, tree body)
//{
//  vec<constructor_elt, va_gc> *vec = NULL;
//  bool ok = true;
//  while (true)
//    switch (TREE_CODE (body))
//      {
//      case STATEMENT_LIST:
//	for (tree stmt : tsi_range (body))
//	  {
//	    body = stmt;
//	    if (TREE_CODE (body) == BIND_EXPR)
//	      break;
//	  }
//	break;
//
//      case BIND_EXPR:
//	body = BIND_EXPR_BODY (body);
//	goto found;
//
//      default:
//	rust_unreachable ();
//      }
// found:
//
//  if (TREE_CODE (body) == CLEANUP_POINT_EXPR)
//    {
//      body = TREE_OPERAND (body, 0);
//      if (TREE_CODE (body) == EXPR_STMT)
//	body = TREE_OPERAND (body, 0);
//      if (TREE_CODE (body) == INIT_EXPR
//	  && (same_type_ignoring_top_level_qualifiers_p (
//	    TREE_TYPE (TREE_OPERAND (body, 0)), current_class_type)))
//	{
//	  /* Trivial copy.  */
//	  return TREE_OPERAND (body, 1);
//	}
//      ok = build_data_member_initialization (body, &vec);
//    }
//  else if (TREE_CODE (body) == STATEMENT_LIST)
//    {
//      for (tree stmt : tsi_range (body))
//	{
//	  ok = build_data_member_initialization (stmt, &vec);
//	  if (!ok)
//	    break;
//	}
//    }
//  else if (EXPR_P (body))
//    ok = build_data_member_initialization (body, &vec);
//  else
//    gcc_assert (errorcount > 0);
//  if (ok)
//    {
//      if (vec_safe_length (vec) > 0)
//	{
//	  /* In a delegating constructor, return the target.  */
//	  constructor_elt *ce = &(*vec)[0];
//	  if (ce->index == current_class_ptr)
//	    {
//	      body = ce->value;
//	      vec_free (vec);
//	      return body;
//	    }
//	}
//      vec = sort_constexpr_mem_initializers (type, vec);
//      return build_constructor (type, vec);
//    }
//  else
//    return error_mark_node;
//}

// Subroutine of check_constexpr_fundef.  BODY is the body of a function
// declared to be constexpr, or a sub-statement thereof.  Returns the
// return value if suitable, error_mark_node for a statement not allowed in
// a constexpr function, or NULL_TREE if no return value was found.
static tree
constexpr_fn_retval (const constexpr_ctx *ctx, tree body)
{
  switch (TREE_CODE (body))
    {
      case STATEMENT_LIST: {
	tree expr = NULL_TREE;
	for (tree stmt : tsi_range (body))
	  {
	    tree s = constexpr_fn_retval (ctx, stmt);
	    if (s == error_mark_node)
	      return error_mark_node;
	    else if (s == NULL_TREE)
	      /* Keep iterating.  */;
	    else if (expr)
	      /* Multiple return statements.  */
	      return error_mark_node;
	    else
	      expr = s;
	  }
	return expr;
      }

      case RETURN_EXPR: {
	bool non_constant_p = false;
	bool overflow_p = false;
	return eval_constant_expression (ctx, body, false, &non_constant_p,
					 &overflow_p);
      }
      case DECL_EXPR: {
	tree decl = DECL_EXPR_DECL (body);
	if (TREE_CODE (decl) == USING_DECL
	    /* Accept __func__, __FUNCTION__, and __PRETTY_FUNCTION__.  */
	    || DECL_ARTIFICIAL (decl))
	  return NULL_TREE;
	return error_mark_node;
      }

    case CLEANUP_POINT_EXPR:
      return constexpr_fn_retval (ctx, TREE_OPERAND (body, 0));

      case BIND_EXPR: {
	tree b = BIND_EXPR_BODY (body);
	return constexpr_fn_retval (ctx, b);
      }
      break;

    default:
      return error_mark_node;
    }
  return error_mark_node;
}

// Taken from cp/constexpr.cc
//
// If DECL is a scalar enumeration constant or variable with a
// constant initializer, return the initializer (or, its initializers,
// recursively); otherwise, return DECL.  If STRICT_P, the
// initializer is only returned if DECL is a
// constant-expression.  If RETURN_AGGREGATE_CST_OK_P, it is ok to
// return an aggregate constant.  If UNSHARE_P, return an unshared
// copy of the initializer.
static tree
constant_value_1 (tree decl, bool, bool, bool unshare_p)
{
  while (TREE_CODE (decl) == CONST_DECL)
    {
      tree init;
      /* If DECL is a static data member in a template
	 specialization, we must instantiate it here.  The
	 initializer for the static data member is not processed
	 until needed; we need it now.  */

      init = DECL_INITIAL (decl);
      if (init == error_mark_node)
	{
	  if (TREE_CODE (decl) == CONST_DECL)
	    /* Treat the error as a constant to avoid cascading errors on
	       excessively recursive template instantiation (c++/9335).  */
	    return init;
	  else
	    return decl;
	}

      decl = init;
    }
  return unshare_p ? unshare_expr (decl) : decl;
}

// A more relaxed version of decl_really_constant_value, used by the
// common C/C++ code.
tree
decl_constant_value (tree decl, bool unshare_p)
{
  return constant_value_1 (decl, /*strict_p=*/false,
			   /*return_aggregate_cst_ok_p=*/true,
			   /*unshare_p=*/unshare_p);
}

static void
non_const_var_error (location_t loc, tree r)
{
  error_at (loc,
	    "the value of %qD is not usable in a constant "
	    "expression",
	    r);
  /* Avoid error cascade.  */
  if (DECL_INITIAL (r) == error_mark_node)
    return;

  // more in cp/constexpr.cc
}

static tree
get_callee (tree call)
{
  if (call == NULL_TREE)
    return call;
  else if (TREE_CODE (call) == CALL_EXPR)
    return CALL_EXPR_FN (call);

  return NULL_TREE;
}

// We have an expression tree T that represents a call, either CALL_EXPR
// or AGGR_INIT_EXPR. If the call is lexically to a named function,
// return the _DECL for that function.
static tree
get_function_named_in_call (tree t)
{
  tree fun = get_callee (t);
  if (fun && TREE_CODE (fun) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fun, 0)) == FUNCTION_DECL)
    fun = TREE_OPERAND (fun, 0);
  return fun;
}

// forked from gcc/cp/constexpr.cc maybe_constexpr_fn

/* True if a function might be declared constexpr  */

bool
maybe_constexpr_fn (tree t)
{
  return (DECL_DECLARED_CONSTEXPR_P (t));
}

// forked from gcc/cp/constexpr.cc var_in_maybe_constexpr_fn

/* True if T was declared in a function that might be constexpr: either a
   function that was declared constexpr.  */

bool
var_in_maybe_constexpr_fn (tree t)
{
  return (DECL_FUNCTION_SCOPE_P (t) && maybe_constexpr_fn (DECL_CONTEXT (t)));
}

/* P0859: A function is needed for constant evaluation if it is a constexpr
   function that is named by an expression ([basic.def.odr]) that is
   potentially constant evaluated.

   So we need to instantiate any constexpr functions mentioned by the
   expression even if the definition isn't needed for evaluating the
   expression.  */

static tree
instantiate_cx_fn_r (tree *tp, int *walk_subtrees, void * /*data*/)
{
  if (TREE_CODE (*tp) == CALL_EXPR)
    {
      if (EXPR_HAS_LOCATION (*tp))
	input_location = EXPR_LOCATION (*tp);
    }

  if (!EXPR_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

static void
instantiate_constexpr_fns (tree t)
{
  location_t loc = input_location;
  rs_walk_tree_without_duplicates (&t, instantiate_cx_fn_r, NULL);
  input_location = loc;
}

/* Returns less than, equal to, or greater than zero if KEY is found to be
   less than, to match, or to be greater than the constructor_elt's INDEX.  */

static int
array_index_cmp (tree key, tree index)
{
  gcc_assert (TREE_CODE (key) == INTEGER_CST);

  switch (TREE_CODE (index))
    {
    case INTEGER_CST:
      return tree_int_cst_compare (key, index);
      case RANGE_EXPR: {
	tree lo = TREE_OPERAND (index, 0);
	tree hi = TREE_OPERAND (index, 1);
	if (tree_int_cst_lt (key, lo))
	  return -1;
	else if (tree_int_cst_lt (hi, key))
	  return 1;
	else
	  return 0;
      }
    default:
      rust_unreachable ();
    }
}

/* If T is a CONSTRUCTOR, return an unshared copy of T and any
   sub-CONSTRUCTORs.  Otherwise return T.

   We use this whenever we initialize an object as a whole, whether it's a
   parameter, a local variable, or a subobject, so that subsequent
   modifications don't affect other places where it was used.  */

tree
unshare_constructor (tree t MEM_STAT_DECL)
{
  if (!t || TREE_CODE (t) != CONSTRUCTOR)
    return t;
  auto_vec<tree *, 4> ptrs;
  ptrs.safe_push (&t);
  while (!ptrs.is_empty ())
    {
      tree *p = ptrs.pop ();
      tree n = copy_node (*p PASS_MEM_STAT);
      CONSTRUCTOR_ELTS (n)
	= vec_safe_copy (CONSTRUCTOR_ELTS (*p) PASS_MEM_STAT);
      *p = n;
      vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (n);
      constructor_elt *ce;
      for (HOST_WIDE_INT i = 0; vec_safe_iterate (v, i, &ce); ++i)
	if (ce->value && TREE_CODE (ce->value) == CONSTRUCTOR)
	  ptrs.safe_push (&ce->value);
    }
  return t;
}

/* Returns the index of the constructor_elt of ARY which matches DINDEX, or -1
   if none.  If INSERT is true, insert a matching element rather than fail. */

static HOST_WIDE_INT
find_array_ctor_elt (tree ary, tree dindex, bool insert)
{
  if (tree_int_cst_sgn (dindex) < 0)
    return -1;

  unsigned HOST_WIDE_INT i = tree_to_uhwi (dindex);
  vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ary);
  unsigned HOST_WIDE_INT len = vec_safe_length (elts);

  unsigned HOST_WIDE_INT end = len;
  unsigned HOST_WIDE_INT begin = 0;

  /* If the last element of the CONSTRUCTOR has its own index, we can assume
     that the same is true of the other elements and index directly.  */
  if (end > 0)
    {
      tree cindex = (*elts)[end - 1].index;
      if (cindex == NULL_TREE)
	{
	  /* Verify that if the last index is missing, all indexes
	     are missing.  */
	  if (flag_checking)
	    for (unsigned int j = 0; j < len - 1; ++j)
	      gcc_assert ((*elts)[j].index == NULL_TREE);
	  if (i < end)
	    return i;
	  else
	    {
	      begin = end;
	      if (i == end)
		/* If the element is to be added right at the end,
		   make sure it is added with cleared index too.  */
		dindex = NULL_TREE;
	      else if (insert)
		/* Otherwise, in order not to break the assumption
		   that CONSTRUCTOR either has all indexes or none,
		   we need to add indexes to all elements.  */
		for (unsigned int j = 0; j < len; ++j)
		  (*elts)[j].index = build_int_cst (TREE_TYPE (dindex), j);
	    }
	}
      else if (TREE_CODE (cindex) == INTEGER_CST
	       && compare_tree_int (cindex, end - 1) == 0)
	{
	  if (i < end)
	    return i;
	  else
	    begin = end;
	}
    }

  /* Otherwise, find a matching index by means of a binary search.  */
  while (begin != end)
    {
      unsigned HOST_WIDE_INT middle = (begin + end) / 2;
      constructor_elt &elt = (*elts)[middle];
      tree idx = elt.index;

      int cmp = array_index_cmp (dindex, idx);
      if (cmp < 0)
	end = middle;
      else if (cmp > 0)
	begin = middle + 1;
      else
	{
	  if (insert && TREE_CODE (idx) == RANGE_EXPR)
	    {
	      /* We need to split the range.  */
	      constructor_elt e;
	      tree lo = TREE_OPERAND (idx, 0);
	      tree hi = TREE_OPERAND (idx, 1);
	      tree value = elt.value;
	      dindex = fold_convert (sizetype, dindex);
	      if (tree_int_cst_lt (lo, dindex))
		{
		  /* There are still some lower elts; shorten the range.  */
		  tree new_hi
		    = int_const_binop (MINUS_EXPR, dindex, size_one_node);
		  if (tree_int_cst_equal (lo, new_hi))
		    /* Only one element left, no longer a range.  */
		    elt.index = lo;
		  else
		    TREE_OPERAND (idx, 1) = new_hi;
		  /* Append the element we want to insert.  */
		  ++middle;
		  e.index = dindex;
		  e.value = unshare_constructor (value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle, e);
		}
	      else
		/* No lower elts, the range elt is now ours.  */
		elt.index = dindex;

	      if (tree_int_cst_lt (dindex, hi))
		{
		  /* There are still some higher elts; append a range.  */
		  tree new_lo
		    = int_const_binop (PLUS_EXPR, dindex, size_one_node);
		  if (tree_int_cst_equal (new_lo, hi))
		    e.index = hi;
		  else
		    e.index = build2 (RANGE_EXPR, sizetype, new_lo, hi);
		  e.value = unshare_constructor (value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle + 1, e);
		}
	    }
	  return middle;
	}
    }

  if (insert)
    {
      constructor_elt e = {dindex, NULL_TREE};
      vec_safe_insert (CONSTRUCTOR_ELTS (ary), end, e);
      return end;
    }

  return -1;
}

/* Some expressions may have constant operands but are not constant
   themselves, such as 1/0.  Call this function to check for that
   condition.

   We only call this in places that require an arithmetic constant, not in
   places where we might have a non-constant expression that can be a
   component of a constant expression, such as the address of a constexpr
   variable that might be dereferenced later.  */

static bool
verify_constant (tree t, bool allow_non_constant, bool *non_constant_p,
		 bool *overflow_p)
{
  if (!*non_constant_p && !reduced_constant_expression_p (t) && t != void_node)
    {
      if (!allow_non_constant)
	error ("%q+E is not a constant expression", t);
      *non_constant_p = true;
    }
  if (TREE_OVERFLOW_P (t))
    {
      if (!allow_non_constant)
	{
	  permerror (input_location, "overflow in constant expression");
	  /* If we're being permissive (and are in an enforcing
	     context), ignore the overflow.  */
	  if (flag_permissive)
	    return *non_constant_p;
	}
      *overflow_p = true;
    }
  return *non_constant_p;
}

// forked from gcc/cp/constexpr.cc find_heap_var_refs

/* Look for heap variables in the expression *TP.  */

static tree
find_heap_var_refs (tree *tp, int *walk_subtrees, void * /*data*/)
{
  if (VAR_P (*tp)
      && (DECL_NAME (*tp) == heap_uninit_identifier
	  || DECL_NAME (*tp) == heap_identifier
	  || DECL_NAME (*tp) == heap_vec_uninit_identifier
	  || DECL_NAME (*tp) == heap_vec_identifier
	  || DECL_NAME (*tp) == heap_deleted_identifier))
    return *tp;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc find_immediate_fndecl

/* Find immediate function decls in *TP if any.  */

static tree
find_immediate_fndecl (tree *tp, int * /*walk_subtrees*/, void * /*data*/)
{
  if (TREE_CODE (*tp) == FUNCTION_DECL && DECL_IMMEDIATE_FUNCTION_P (*tp))
    return *tp;
  if (TREE_CODE (*tp) == PTRMEM_CST
      && TREE_CODE (PTRMEM_CST_MEMBER (*tp)) == FUNCTION_DECL
      && DECL_IMMEDIATE_FUNCTION_P (PTRMEM_CST_MEMBER (*tp)))
    return PTRMEM_CST_MEMBER (*tp);
  return NULL_TREE;
}

// forked in gcc/cp/constexpr.cc diag_array_subscript

/* Under the control of CTX, issue a detailed diagnostic for
   an out-of-bounds subscript INDEX into the expression ARRAY.  */

static void
diag_array_subscript (location_t loc, const constexpr_ctx *ctx, tree array,
		      tree index)
{
  if (!ctx->quiet)
    {
      tree arraytype = TREE_TYPE (array);

      /* Convert the unsigned array subscript to a signed integer to avoid
	 printing huge numbers for small negative values.  */
      tree sidx = fold_convert (ssizetype, index);
      STRIP_ANY_LOCATION_WRAPPER (array);
      if (DECL_P (array))
	{
	  if (TYPE_DOMAIN (arraytype))
	    error_at (loc,
		      "array subscript value %qE is outside the bounds "
		      "of array %qD of type %qT",
		      sidx, array, arraytype);
	  else
	    error_at (loc,
		      "nonzero array subscript %qE is used with array %qD of "
		      "type %qT with unknown bounds",
		      sidx, array, arraytype);
	  inform (DECL_SOURCE_LOCATION (array), "declared here");
	}
      else if (TYPE_DOMAIN (arraytype))
	error_at (loc,
		  "array subscript value %qE is outside the bounds "
		  "of array type %qT",
		  sidx, arraytype);
      else
	error_at (loc,
		  "nonzero array subscript %qE is used with array of type %qT "
		  "with unknown bounds",
		  sidx, arraytype);
    }
}

// forked from gcc/cp/constexpr.cc get_array_or_vector_nelts

/* Return the number of elements for TYPE (which is an ARRAY_TYPE or
   a VECTOR_TYPE).  */

static tree
get_array_or_vector_nelts (const constexpr_ctx *ctx, tree type,
			   bool *non_constant_p, bool *overflow_p)
{
  tree nelts;
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (type))
	nelts = array_type_nelts_top (type);
      else
	nelts = size_zero_node;
    }
  else if (VECTOR_TYPE_P (type))
    nelts = size_int (TYPE_VECTOR_SUBPARTS (type));
  else
    rust_unreachable ();

  /* For VLAs, the number of elements won't be an integer constant.  */
  nelts
    = eval_constant_expression (ctx, nelts, false, non_constant_p, overflow_p);
  return nelts;
}

// forked from gcc/cp/constexpr.cc eval_and_check_array_index

/* Subroutine of cxx_eval_array_reference.  T is an ARRAY_REF; evaluate the
   subscript, diagnose any problems with it, and return the result.  */

static tree
eval_and_check_array_index (const constexpr_ctx *ctx, tree t,
			    bool allow_one_past, bool *non_constant_p,
			    bool *overflow_p)
{
  location_t loc = rs_expr_loc_or_input_loc (t);
  tree ary = TREE_OPERAND (t, 0);
  t = TREE_OPERAND (t, 1);
  tree index = eval_constant_expression (ctx, t, allow_one_past, non_constant_p,
					 overflow_p);
  VERIFY_CONSTANT (index);

  if (!tree_fits_shwi_p (index) || tree_int_cst_sgn (index) < 0)
    {
      diag_array_subscript (loc, ctx, ary, index);
      *non_constant_p = true;
      return t;
    }

  tree nelts = get_array_or_vector_nelts (ctx, TREE_TYPE (ary), non_constant_p,
					  overflow_p);
  VERIFY_CONSTANT (nelts);
  if (allow_one_past ? !tree_int_cst_le (index, nelts)
		     : !tree_int_cst_lt (index, nelts))
    {
      diag_array_subscript (loc, ctx, ary, index);
      *non_constant_p = true;
      return t;
    }

  return index;
}

// forked from gcc/cp/constexpr.cc extract_string_elt

/* Extract element INDEX consisting of CHARS_PER_ELT chars from
   STRING_CST STRING.  */

static tree
extract_string_elt (tree string, unsigned chars_per_elt, unsigned index)
{
  tree type = cv_unqualified (TREE_TYPE (TREE_TYPE (string)));
  tree r;

  if (chars_per_elt == 1)
    r = build_int_cst (type, TREE_STRING_POINTER (string)[index]);
  else
    {
      const unsigned char *ptr
	= ((const unsigned char *) TREE_STRING_POINTER (string)
	   + index * chars_per_elt);
      r = native_interpret_expr (type, ptr, chars_per_elt);
    }
  return r;
}

/* Check whether the parameter and return types of FUN are valid for a
   constexpr function, and complain if COMPLAIN.  */

bool
is_valid_constexpr_fn (tree fun, bool complain)
{
  bool ret = true;

  for (tree parm = FUNCTION_FIRST_USER_PARM (fun); parm != NULL_TREE;
       parm = TREE_CHAIN (parm))
    if (!literal_type_p (TREE_TYPE (parm)))
      {
	ret = false;
	if (complain)
	  {
	    // auto_diagnostic_group d;
	    // error ("invalid type for parameter %d of %<constexpr%> "
	    //        "function %q+#D",
	    //        DECL_PARM_INDEX (parm), fun);
	    location_t locus = DECL_SOURCE_LOCATION (fun);
	    rust_error_at (
	      locus, "invalid type for parameter %d of %<constexpr%> function",
	      DECL_PARM_INDEX (parm));
	  }
      }

  return ret;
}

void
explain_invalid_constexpr_fn (tree fun)
{
  static hash_set<tree> *diagnosed;
  // tree body;

  if (diagnosed == NULL)
    diagnosed = new hash_set<tree>;
  if (diagnosed->add (fun))
    /* Already explained.  */
    return;

  iloc_sentinel ils = input_location;
  // if (!lambda_static_thunk_p (fun))
  //   {
  //     /* Diagnostics should completely ignore the static thunk, so leave
  //        input_location set to our caller's location.  */
  //     input_location = DECL_SOURCE_LOCATION (fun);
  //     inform (input_location,
  //             "%qD is not usable as a %<constexpr%> function because:",
  //             fun);
  //   }

  /* First check the declaration.  */
  if (is_valid_constexpr_fn (fun, true))
    {
      // /* Then if it's OK, the body.  */
      // if (!DECL_DECLARED_CONSTEXPR_P (fun))
      //   explain_implicit_non_constexpr (fun);
      // else
      //   {
      //     if (constexpr_fundef *fd = retrieve_constexpr_fundef (fun))
      //       body = fd->body;
      //     else
      //       body = DECL_SAVED_TREE (fun);
      //     body = massage_constexpr_body (fun, body);
      //     require_potential_rvalue_constant_expression (body);
      //   }
    }
}

/* BODY is a validated and massaged definition of a constexpr
   function.  Register it in the hash table.  */

void
register_constexpr_fundef (const rust_constexpr_fundef &value)
{
  /* Create the constexpr function table if necessary.  */
  if (constexpr_fundef_table == NULL)
    constexpr_fundef_table
      = hash_table<rust_constexpr_fundef_hasher>::create_ggc (101);

  rust_constexpr_fundef **slot = constexpr_fundef_table->find_slot (
    const_cast<rust_constexpr_fundef *> (&value), INSERT);

  gcc_assert (*slot == NULL);
  *slot = ggc_alloc<rust_constexpr_fundef> ();
  **slot = value;
}

/* We are processing the definition of the constexpr function FUN.
   Check that its body fulfills the apropriate requirements and
   enter it in the constexpr function definition table.  */

void
maybe_save_constexpr_fundef (tree fun)
{
  // FIXME

  rust_constexpr_fundef entry = {fun, NULL_TREE, NULL_TREE, NULL_TREE};
  bool clear_ctx = false;
  if (DECL_RESULT (fun) && DECL_CONTEXT (DECL_RESULT (fun)) == NULL_TREE)
    {
      clear_ctx = true;
      DECL_CONTEXT (DECL_RESULT (fun)) = fun;
    }
  tree saved_fn = current_function_decl;
  current_function_decl = fun;
  entry.body = copy_fn (entry.decl, entry.parms, entry.result);
  current_function_decl = saved_fn;
  if (clear_ctx)
    DECL_CONTEXT (DECL_RESULT (entry.decl)) = NULL_TREE;

  register_constexpr_fundef (entry);
}

/* Evaluate a STATEMENT_LIST for side-effects.  Handles various jump
   semantics, for switch, break, continue, and return.  */

static tree
eval_statement_list (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		     bool *overflow_p, tree *jump_target)
{
  tree local_target;
  /* In a statement-expression we want to return the last value.
     For empty statement expression return void_node.  */
  tree r = void_node;
  if (!jump_target)
    {
      local_target = NULL_TREE;
      jump_target = &local_target;
    }
  for (tree stmt : tsi_range (t))
    {
      /* We've found a continue, so skip everything until we reach
	 the label its jumping to.  */
      if (continues (jump_target))
	{
	  if (label_matches (ctx, jump_target, stmt))
	    /* Found it.  */
	    *jump_target = NULL_TREE;
	  else
	    continue;
	}
      if (TREE_CODE (stmt) == DEBUG_BEGIN_STMT)
	continue;
      r = eval_constant_expression (ctx, stmt, false, non_constant_p,
				    overflow_p, jump_target);
      if (*non_constant_p)
	break;
      if (returns (jump_target) || breaks (jump_target))
	break;
    }
  if (*jump_target && jump_target == &local_target)
    {
      /* We aren't communicating the jump to our caller, so give up.  We don't
	 need to support evaluation of jumps out of statement-exprs.  */
      if (!ctx->quiet)
	error_at (EXPR_LOCATION (r), "statement is not a constant expression");
      *non_constant_p = true;
    }
  return r;
}

// forked from gcc/cp/constexpr.cc cxx_eval_conditional_expression

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate condition expressions.  Dead branches are not
   looked into.  */

static tree
eval_conditional_expression (const constexpr_ctx *ctx, tree t, bool lval,
			     bool *non_constant_p, bool *overflow_p,
			     tree *jump_target)
{
  tree val
    = eval_constant_expression (ctx, TREE_OPERAND (t, 0),
				/*lval*/ false, non_constant_p, overflow_p);
  VERIFY_CONSTANT (val);
  if (TREE_CODE (t) == IF_STMT && IF_STMT_CONSTEVAL_P (t))
    {
      /* Evaluate the condition as if it was
	 if (__builtin_is_constant_evaluated ()), i.e. defer it if not
	 ctx->manifestly_const_eval (as sometimes we try to constant evaluate
	 without manifestly_const_eval even expressions or parts thereof which
	 will later be manifestly const_eval evaluated), otherwise fold it to
	 true.  */
      if (ctx->manifestly_const_eval)
	val = boolean_true_node;
      else
	{
	  *non_constant_p = true;
	  return t;
	}
    }
  /* Don't VERIFY_CONSTANT the other operands.  */
  if (integer_zerop (val))
    val = TREE_OPERAND (t, 2);
  else
    val = TREE_OPERAND (t, 1);
  if (/*TREE_CODE (t) == IF_STMT && */ !val)
    val = void_node;
  return eval_constant_expression (ctx, val, lval, non_constant_p, overflow_p,
				   jump_target);
}

// forked from gcc/cp/constexpr.cc cxx_eval_bit_field_ref

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type that is
   expressed as a BIT_FIELD_REF.  */

static tree
eval_bit_field_ref (const constexpr_ctx *ctx, tree t, bool lval,
		    bool *non_constant_p, bool *overflow_p)
{
  tree orig_whole = TREE_OPERAND (t, 0);
  tree retval, fldval, utype, mask;
  bool fld_seen = false;
  HOST_WIDE_INT istart, isize;
  tree whole = eval_constant_expression (ctx, orig_whole, lval, non_constant_p,
					 overflow_p);
  tree start, field, value;
  unsigned HOST_WIDE_INT i;

  if (whole == orig_whole)
    return t;
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p && TREE_CODE (whole) != VECTOR_CST
      && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!ctx->quiet)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;

  if (TREE_CODE (whole) == VECTOR_CST)
    return fold_ternary (BIT_FIELD_REF, TREE_TYPE (t), whole,
			 TREE_OPERAND (t, 1), TREE_OPERAND (t, 2));

  start = TREE_OPERAND (t, 2);
  istart = tree_to_shwi (start);
  isize = tree_to_shwi (TREE_OPERAND (t, 1));
  utype = TREE_TYPE (t);
  if (!TYPE_UNSIGNED (utype))
    utype = build_nonstandard_integer_type (TYPE_PRECISION (utype), 1);
  retval = build_int_cst (utype, 0);
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      tree bitpos = bit_position (field);
      STRIP_ANY_LOCATION_WRAPPER (value);
      if (bitpos == start && DECL_SIZE (field) == TREE_OPERAND (t, 1))
	return value;
      if (TREE_CODE (TREE_TYPE (field)) == INTEGER_TYPE
	  && TREE_CODE (value) == INTEGER_CST && tree_fits_shwi_p (bitpos)
	  && tree_fits_shwi_p (DECL_SIZE (field)))
	{
	  HOST_WIDE_INT bit = tree_to_shwi (bitpos);
	  HOST_WIDE_INT sz = tree_to_shwi (DECL_SIZE (field));
	  HOST_WIDE_INT shift;
	  if (bit >= istart && bit + sz <= istart + isize)
	    {
	      fldval = fold_convert (utype, value);
	      mask = build_int_cst_type (utype, -1);
	      mask = fold_build2 (LSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      mask = fold_build2 (RSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      fldval = fold_build2 (BIT_AND_EXPR, utype, fldval, mask);
	      shift = bit - istart;
	      if (BYTES_BIG_ENDIAN)
		shift = TYPE_PRECISION (utype) - shift - sz;
	      fldval
		= fold_build2 (LSHIFT_EXPR, utype, fldval, size_int (shift));
	      retval = fold_build2 (BIT_IOR_EXPR, utype, retval, fldval);
	      fld_seen = true;
	    }
	}
    }
  if (fld_seen)
    return fold_convert (TREE_TYPE (t), retval);
  rust_unreachable ();
  return error_mark_node;
}

// forked from gcc/cp/constexpr.cc returns

/* Predicates for the meaning of *jump_target.  */

static bool
returns (tree *jump_target)
{
  return *jump_target
	 && (TREE_CODE (*jump_target) == RETURN_EXPR
	     || (TREE_CODE (*jump_target) == LABEL_DECL
		 && LABEL_DECL_CDTOR (*jump_target)));
}

// forked from gcc/cp/constexpr.cc breaks

static bool
breaks (tree *jump_target)
{
  return *jump_target
	 && ((TREE_CODE (*jump_target) == LABEL_DECL
	      && LABEL_DECL_BREAK (*jump_target))
	     || TREE_CODE (*jump_target) == BREAK_STMT
	     || TREE_CODE (*jump_target) == EXIT_EXPR);
}

// forked from gcc/cp/constexpr.cc continues

static bool
continues (tree *jump_target)
{
  return *jump_target
	 && ((TREE_CODE (*jump_target) == LABEL_DECL
	      && LABEL_DECL_CONTINUE (*jump_target))
	     || TREE_CODE (*jump_target) == CONTINUE_STMT);
}

// forked from gcc/cp/constexpr.cc switches

static bool
switches (tree *jump_target)
{
  return *jump_target && TREE_CODE (*jump_target) == INTEGER_CST;
}

// forked from gcc/cp/constexpr.cc cxx_eval_loop_expr

/* Evaluate a LOOP_EXPR for side-effects.  Handles break and return
   semantics; continue semantics are covered by cxx_eval_statement_list.  */

static tree
eval_loop_expr (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		bool *overflow_p, tree *jump_target)
{
  constexpr_ctx new_ctx = *ctx;
  tree local_target;
  if (!jump_target)
    {
      local_target = NULL_TREE;
      jump_target = &local_target;
    }

  tree body, cond = NULL_TREE, expr = NULL_TREE;
  int count = 0;
  switch (TREE_CODE (t))
    {
    case LOOP_EXPR:
      body = LOOP_EXPR_BODY (t);
      break;
    case WHILE_STMT:
      body = WHILE_BODY (t);
      cond = WHILE_COND (t);
      count = -1;
      break;
    case FOR_STMT:
      if (FOR_INIT_STMT (t))
	eval_constant_expression (ctx, FOR_INIT_STMT (t), /*lval*/ false,
				  non_constant_p, overflow_p, jump_target);
      if (*non_constant_p)
	return NULL_TREE;
      body = FOR_BODY (t);
      cond = FOR_COND (t);
      expr = FOR_EXPR (t);
      count = -1;
      break;
    default:
      rust_unreachable ();
    }
  auto_vec<tree, 10> save_exprs;
  new_ctx.save_exprs = &save_exprs;
  do
    {
      if (count != -1)
	{
	  if (body)
	    eval_constant_expression (&new_ctx, body, /*lval*/ false,
				      non_constant_p, overflow_p, jump_target);
	  if (breaks (jump_target))
	    {
	      *jump_target = NULL_TREE;
	      break;
	    }

	  if (TREE_CODE (t) != LOOP_EXPR && continues (jump_target))
	    *jump_target = NULL_TREE;

	  if (expr)
	    eval_constant_expression (&new_ctx, expr, /*lval*/ false,
				      non_constant_p, overflow_p, jump_target);
	}

      if (cond)
	{
	  tree res = eval_constant_expression (&new_ctx, cond, /*lval*/ false,
					       non_constant_p, overflow_p,
					       jump_target);
	  if (res)
	    {
	      if (verify_constant (res, ctx->quiet, non_constant_p, overflow_p))
		break;
	      if (integer_zerop (res))
		break;
	    }
	  else
	    gcc_assert (*jump_target);
	}

      /* Forget saved values of SAVE_EXPRs and TARGET_EXPRs.  */
      for (tree save_expr : save_exprs)
	ctx->global->values.remove (save_expr);
      save_exprs.truncate (0);

      if (++count >= constexpr_loop_limit)
	{
	  if (!ctx->quiet)
	    error_at (rs_expr_loc_or_input_loc (t),
		      "%<constexpr%> loop iteration count exceeds limit of %d "
		      "(use %<-fconstexpr-loop-limit=%> to increase the limit)",
		      constexpr_loop_limit);
	  *non_constant_p = true;
	  break;
	}
    }
  while (!returns (jump_target) && !breaks (jump_target)
	 && !continues (jump_target) && (!switches (jump_target) || count == 0)
	 && !*non_constant_p);

  /* Forget saved values of SAVE_EXPRs and TARGET_EXPRs.  */
  for (tree save_expr : save_exprs)
    ctx->global->values.remove (save_expr);

  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc cxx_eval_switch_expr

/* Evaluate a SWITCH_EXPR for side-effects.  Handles switch and break jump
   semantics.  */

static tree
eval_switch_expr (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		  bool *overflow_p, tree *jump_target)
{
  tree cond
    = TREE_CODE (t) == SWITCH_STMT ? SWITCH_STMT_COND (t) : SWITCH_COND (t);
  cond
    = eval_constant_expression (ctx, cond, false, non_constant_p, overflow_p);
  VERIFY_CONSTANT (cond);
  *jump_target = cond;

  tree body
    = TREE_CODE (t) == SWITCH_STMT ? SWITCH_STMT_BODY (t) : SWITCH_BODY (t);
  constexpr_ctx new_ctx = *ctx;
  constexpr_switch_state css = css_default_not_seen;
  new_ctx.css_state = &css;
  eval_constant_expression (&new_ctx, body, false, non_constant_p, overflow_p,
			    jump_target);
  if (switches (jump_target) && css == css_default_seen)
    {
      /* If the SWITCH_EXPR body has default: label, process it once again,
	 this time instructing label_matches to return true for default:
	 label on switches (jump_target).  */
      css = css_default_processing;
      eval_constant_expression (&new_ctx, body, false, non_constant_p,
				overflow_p, jump_target);
    }
  if (breaks (jump_target) || switches (jump_target))
    *jump_target = NULL_TREE;
  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc eval_unary_expression

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce the unary expression tree T to a compile time value.
   If successful, return the value.  Otherwise issue a diagnostic
   and return error_mark_node.  */

static tree
eval_unary_expression (const constexpr_ctx *ctx, tree t, bool /*lval*/,
		       bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree orig_arg = TREE_OPERAND (t, 0);
  tree arg = eval_constant_expression (ctx, orig_arg, /*lval*/ false,
				       non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg);
  location_t loc = EXPR_LOCATION (t);
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);
  r = fold_unary_loc (loc, code, type, arg);
  if (r == NULL_TREE)
    {
      if (arg == orig_arg)
	r = t;
      else
	r = build1_loc (loc, code, type, arg);
    }
  VERIFY_CONSTANT (r);
  return r;
}

// forked from gcc/cp/constexpr.cc cxx_eval_outermost_constant_expr

/* ALLOW_NON_CONSTANT is false if T is required to be a constant expression.
   STRICT has the same sense as for constant_value_1: true if we only allow
   conforming C++ constant expressions, or false if we want a constant value
   even if it doesn't conform.
   MANIFESTLY_CONST_EVAL is true if T is manifestly const-evaluated as
   per P0595 even when ALLOW_NON_CONSTANT is true.
   CONSTEXPR_DTOR is true when evaluating the dtor of a constexpr variable.
   OBJECT must be non-NULL in that case.  */

static tree
cxx_eval_outermost_constant_expr (tree t, bool allow_non_constant,
				  bool strict = true,
				  bool manifestly_const_eval = false,
				  bool constexpr_dtor = false,
				  tree object = NULL_TREE)
{
  auto_timevar time (TV_CONSTEXPR);

  bool non_constant_p = false;
  bool overflow_p = false;

  if (BRACE_ENCLOSED_INITIALIZER_P (t))
    {
      gcc_checking_assert (allow_non_constant);
      return t;
    }

  constexpr_global_ctx global_ctx;
  constexpr_ctx ctx
    = {&global_ctx, NULL,
       NULL,	    NULL,
       NULL,	    NULL,
       NULL,	    allow_non_constant,
       strict,	    manifestly_const_eval || !allow_non_constant};

  /* Turn off -frounding-math for manifestly constant evaluation.  */
  warning_sentinel rm (flag_rounding_math, ctx.manifestly_const_eval);
  tree type = initialized_type (t);
  tree r = t;
  bool is_consteval = false;
  if (VOID_TYPE_P (type))
    {
      if (constexpr_dtor)
	/* Used for destructors of array elements.  */
	type = TREE_TYPE (object);
      else
	{
	  if (TREE_CODE (t) != CALL_EXPR)
	    return t;
	  /* Calls to immediate functions returning void need to be
	     evaluated.  */
	  tree fndecl = rs_get_callee_fndecl_nofold (t);
	  if (fndecl == NULL_TREE || !DECL_IMMEDIATE_FUNCTION_P (fndecl))
	    return t;
	  else
	    is_consteval = true;
	}
    }
  else if ((TREE_CODE (t) == CALL_EXPR || TREE_CODE (t) == TARGET_EXPR))
    {
      /* For non-concept checks, determine if it is consteval.  */
      tree x = t;
      if (TREE_CODE (x) == TARGET_EXPR)
	x = TARGET_EXPR_INITIAL (x);
      tree fndecl = rs_get_callee_fndecl_nofold (x);
      if (fndecl && DECL_IMMEDIATE_FUNCTION_P (fndecl))
	is_consteval = true;
    }
  if (AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type))
    {
      /* In C++14 an NSDMI can participate in aggregate initialization,
	 and can refer to the address of the object being initialized, so
	 we need to pass in the relevant VAR_DECL if we want to do the
	 evaluation in a single pass.  The evaluation will dynamically
	 update ctx.values for the VAR_DECL.  We use the same strategy
	 for C++11 constexpr constructors that refer to the object being
	 initialized.  */
      if (constexpr_dtor)
	{
	  gcc_assert (object && VAR_P (object));
	  gcc_assert (DECL_DECLARED_CONSTEXPR_P (object));
	  gcc_assert (DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (object));
	  if (error_operand_p (DECL_INITIAL (object)))
	    return t;
	  ctx.ctor = unshare_expr (DECL_INITIAL (object));
	  TREE_READONLY (ctx.ctor) = false;
	  /* Temporarily force decl_really_constant_value to return false
	     for it, we want to use ctx.ctor for the current value instead.  */
	  DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (object) = false;
	}
      else
	{
	  ctx.ctor = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_CLEARING (ctx.ctor) = true;
	}
      if (!object)
	{
	  if (TREE_CODE (t) == TARGET_EXPR)
	    object = TARGET_EXPR_SLOT (t);
	}
      ctx.object = object;
      if (object)
	gcc_assert (
	  same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (object)));
      if (object && DECL_P (object))
	global_ctx.values.put (object, ctx.ctor);
      if (TREE_CODE (r) == TARGET_EXPR)
	/* Avoid creating another CONSTRUCTOR when we expand the
	   TARGET_EXPR.  */
	r = TARGET_EXPR_INITIAL (r);
    }

  auto_vec<tree, 16> cleanups;
  global_ctx.cleanups = &cleanups;

  if (manifestly_const_eval)
    instantiate_constexpr_fns (r);
  r = eval_constant_expression (&ctx, r, false, &non_constant_p, &overflow_p);

  if (!constexpr_dtor)
    verify_constant (r, allow_non_constant, &non_constant_p, &overflow_p);
  else
    DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (object) = true;

  unsigned int i;
  tree cleanup;
  /* Evaluate the cleanups.  */
  FOR_EACH_VEC_ELT_REVERSE (cleanups, i, cleanup)
    eval_constant_expression (&ctx, cleanup, false, &non_constant_p,
			      &overflow_p);

  /* Mutable logic is a bit tricky: we want to allow initialization of
     constexpr variables with mutable members, but we can't copy those
     members to another constexpr variable.  */
  if (TREE_CODE (r) == CONSTRUCTOR && CONSTRUCTOR_MUTABLE_POISON (r))
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression because it refers to "
	       "mutable subobjects of %qT",
	       t, type);
      non_constant_p = true;
    }

  if (TREE_CODE (r) == CONSTRUCTOR && CONSTRUCTOR_NO_CLEARING (r))
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression because it refers to "
	       "an incompletely initialized variable",
	       t);
      TREE_CONSTANT (r) = false;
      non_constant_p = true;
    }

  if (!global_ctx.heap_vars.is_empty ())
    {
      tree heap_var
	= rs_walk_tree_without_duplicates (&r, find_heap_var_refs, NULL);
      unsigned int i;
      if (heap_var)
	{
	  if (!allow_non_constant && !non_constant_p)
	    error_at (DECL_SOURCE_LOCATION (heap_var),
		      "%qE is not a constant expression because it refers to "
		      "a result of %<operator new%>",
		      t);
	  r = t;
	  non_constant_p = true;
	}
      FOR_EACH_VEC_ELT (global_ctx.heap_vars, i, heap_var)
	{
	  if (DECL_NAME (heap_var) != heap_deleted_identifier)
	    {
	      if (!allow_non_constant && !non_constant_p)
		error_at (DECL_SOURCE_LOCATION (heap_var),
			  "%qE is not a constant expression because allocated "
			  "storage has not been deallocated",
			  t);
	      r = t;
	      non_constant_p = true;
	    }
	  varpool_node::get (heap_var)->remove ();
	}
    }

  /* Check that immediate invocation does not return an expression referencing
     any immediate function decls.  */
  if (is_consteval || in_immediate_context ())
    if (tree immediate_fndecl
	= rs_walk_tree_without_duplicates (&r, find_immediate_fndecl, NULL))
      {
	if (!allow_non_constant && !non_constant_p)
	  error_at (rs_expr_loc_or_input_loc (t),
		    "immediate evaluation returns address of immediate "
		    "function %qD",
		    immediate_fndecl);
	r = t;
	non_constant_p = true;
      }

  if (non_constant_p)
    /* If we saw something bad, go back to our argument.  The wrapping below is
       only for the cases of TREE_CONSTANT argument or overflow.  */
    r = t;

  if (!non_constant_p && overflow_p)
    non_constant_p = true;

  /* Unshare the result.  */
  bool should_unshare = true;
  if (r == t || (TREE_CODE (t) == TARGET_EXPR && TARGET_EXPR_INITIAL (t) == r))
    should_unshare = false;

  if (non_constant_p && !allow_non_constant)
    return error_mark_node;
  else if (constexpr_dtor)
    return r;
  else if (non_constant_p && TREE_CONSTANT (r))
    {
      /* This isn't actually constant, so unset TREE_CONSTANT.
	 Don't clear TREE_CONSTANT on ADDR_EXPR, as the middle-end requires
	 it to be set if it is invariant address, even when it is not
	 a valid C++ constant expression.  Wrap it with a NOP_EXPR
	 instead.  */
      if (EXPR_P (r) && TREE_CODE (r) != ADDR_EXPR)
	r = copy_node (r);
      else if (TREE_CODE (r) == CONSTRUCTOR)
	r = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (r), r);
      else
	r = build_nop (TREE_TYPE (r), r);
      TREE_CONSTANT (r) = false;
    }
  else if (non_constant_p)
    return t;

  if (should_unshare)
    r = unshare_expr (r);

  if (TREE_CODE (r) == CONSTRUCTOR && CLASS_TYPE_P (TREE_TYPE (r)))
    {
      r = adjust_temp_type (type, r);
      if (TREE_CODE (t) == TARGET_EXPR && TARGET_EXPR_INITIAL (t) == r)
	return t;
    }

  /* Remember the original location if that wouldn't need a wrapper.  */
  if (location_t loc = EXPR_LOCATION (t))
    protected_set_expr_location (r, loc);

  return r;
}

/* Like is_constant_expression, but allow const variables that are not allowed
   under constexpr rules.  */

bool
is_static_init_expression (tree t)
{
  return potential_constant_expression_1 (t, false, false, true, tf_none);
}

/* Like potential_constant_expression, but don't consider possible constexpr
   substitution of the current function.  That is, PARM_DECL qualifies under
   potential_constant_expression, but not here.

   This is basically what you can check when any actual constant values might
   be value-dependent.  */

bool
is_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, true, true, tf_none);
}

/* Returns true if T is a potential static initializer expression that is not
   instantiation-dependent.  */

bool
is_nondependent_static_init_expression (tree t)
{
  return (!type_unknown_p (t) && is_static_init_expression (t));
}

/* Like maybe_constant_value, but returns a CONSTRUCTOR directly, rather
   than wrapped in a TARGET_EXPR.
   ALLOW_NON_CONSTANT is false if T is required to be a constant expression.
   MANIFESTLY_CONST_EVAL is true if T is manifestly const-evaluated as
   per P0595 even when ALLOW_NON_CONSTANT is true.  */

static tree
maybe_constant_init_1 (tree t, tree decl, bool allow_non_constant,
		       bool manifestly_const_eval)
{
  if (!t)
    return t;
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == CONVERT_EXPR && VOID_TYPE_P (TREE_TYPE (t)))
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == INIT_EXPR)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == TARGET_EXPR)
    t = TARGET_EXPR_INITIAL (t);
  if (!is_nondependent_static_init_expression (t))
    /* Don't try to evaluate it.  */;
  else if (CONSTANT_CLASS_P (t) && allow_non_constant)
    /* No evaluation needed.  */;
  else
    t = cxx_eval_outermost_constant_expr (t, allow_non_constant,
					  /*strict*/ false,
					  manifestly_const_eval, false, decl);
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree init = TARGET_EXPR_INITIAL (t);
      if (TREE_CODE (init) == CONSTRUCTOR)
	t = init;
    }
  return t;
}

/* Wrapper for maybe_constant_init_1 which permits non constants.  */

tree
maybe_constant_init (tree t, tree decl, bool manifestly_const_eval)
{
  return maybe_constant_init_1 (t, decl, true, manifestly_const_eval);
}

/* Returns true if T is a potential constant expression that is not
   instantiation-dependent, and therefore a candidate for constant folding even
   in a template.  */

bool
is_nondependent_constant_expression (tree t)
{
  return (!type_unknown_p (t) && is_constant_expression (t)
	  && !instantiation_dependent_expression_p (t));
}

// forked from gcc/cp/parser.cc cp_unevaluated_operand

/* Nonzero if we are parsing an unevaluated operand: an operand to
   sizeof, typeof, or alignof.  */
int cp_unevaluated_operand;

// forked from gcc/cp/constexpr.cc cv_cache

/* If T is a constant expression, returns its reduced value.
   Otherwise, if T does not have TREE_CONSTANT set, returns T.
   Otherwise, returns a version of T without TREE_CONSTANT.
   MANIFESTLY_CONST_EVAL is true if T is manifestly const-evaluated
   as per P0595.  */

static GTY ((deletable)) hash_map<tree, tree> *cv_cache;

// forked from gcc/cp/constexpr.cc maybe_constant_value

tree
maybe_constant_value (tree t, tree decl, bool manifestly_const_eval)
{
  tree r;

  if (!is_nondependent_constant_expression (t))
    {
      if (TREE_OVERFLOW_P (t))
	{
	  t = build_nop (TREE_TYPE (t), t);
	  TREE_CONSTANT (t) = false;
	}
      return t;
    }
  else if (CONSTANT_CLASS_P (t))
    /* No caching or evaluation needed.  */
    return t;

  if (manifestly_const_eval)
    return cxx_eval_outermost_constant_expr (t, true, true, true, false, decl);

  if (cv_cache == NULL)
    cv_cache = hash_map<tree, tree>::create_ggc (101);
  if (tree *cached = cv_cache->get (t))
    {
      r = *cached;
      if (r != t)
	{
	  // Faisal: commenting this out as not sure if it's needed and it's
	  // huge r = break_out_target_exprs (r, /*clear_loc*/true);
	  protected_set_expr_location (r, EXPR_LOCATION (t));
	}
      return r;
    }

  /* Don't evaluate an unevaluated operand.  */
  if (cp_unevaluated_operand)
    return t;

  uid_sensitive_constexpr_evaluation_checker c;
  r = cxx_eval_outermost_constant_expr (t, true, true, false, false, decl);
  gcc_checking_assert (
    r == t || CONVERT_EXPR_P (t) || TREE_CODE (t) == VIEW_CONVERT_EXPR
    || (TREE_CONSTANT (t) && !TREE_CONSTANT (r)) || !rs_tree_equal (r, t));
  if (!c.evaluation_restricted_p ())
    cv_cache->put (t, r);
  return r;
}

// forked from gcc/cp/constexpr.cc

bool
potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, true, false, tf_none);
}

/* Data structure for passing data from potential_constant_expression_1
   to check_for_return_continue via cp_walk_tree.  */
struct check_for_return_continue_data
{
  hash_set<tree> *pset;
  tree continue_stmt;
  tree break_stmt;
};

/* Helper function for potential_constant_expression_1 SWITCH_STMT handling,
   called through cp_walk_tree.  Return the first RETURN_EXPR found, or note
   the first CONTINUE_STMT and/or BREAK_STMT if RETURN_EXPR is not found.  */
static tree
check_for_return_continue (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp, s, b;
  check_for_return_continue_data *d = (check_for_return_continue_data *) data;
  switch (TREE_CODE (t))
    {
    case RETURN_EXPR:
      return t;

    case CONTINUE_STMT:
      if (d->continue_stmt == NULL_TREE)
	d->continue_stmt = t;
      break;

    case BREAK_STMT:
      if (d->break_stmt == NULL_TREE)
	d->break_stmt = t;
      break;

#define RECUR(x)                                                               \
  if (tree r = rs_walk_tree (&x, check_for_return_continue, data, d->pset))    \
  return r

      /* For loops, walk subtrees manually, so that continue stmts found
	 inside of the bodies of the loops are ignored.  */

    case WHILE_STMT:
      *walk_subtrees = 0;
      RECUR (WHILE_COND (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (WHILE_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case FOR_STMT:
      *walk_subtrees = 0;
      RECUR (FOR_INIT_STMT (t));
      RECUR (FOR_COND (t));
      RECUR (FOR_EXPR (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (FOR_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case RANGE_FOR_STMT:
      *walk_subtrees = 0;
      RECUR (RANGE_FOR_EXPR (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (RANGE_FOR_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case SWITCH_STMT:
      *walk_subtrees = 0;
      RECUR (SWITCH_STMT_COND (t));
      b = d->break_stmt;
      RECUR (SWITCH_STMT_BODY (t));
      d->break_stmt = b;
      break;
#undef RECUR

    case STATEMENT_LIST:
    case CONSTRUCTOR:
      break;

    default:
      if (!EXPR_P (t))
	*walk_subtrees = 0;
      break;
    }

  return NULL_TREE;
}

/* Returns the namespace that contains DECL, whether directly or
   indirectly.  */

tree
decl_namespace_context (tree decl)
{
  while (1)
    {
      if (TREE_CODE (decl) == NAMESPACE_DECL)
	return decl;
      else if (TYPE_P (decl))
	decl = CP_DECL_CONTEXT (TYPE_MAIN_DECL (decl));
      else
	decl = CP_DECL_CONTEXT (decl);
    }
}

/* Returns true if DECL is in the std namespace.  */

bool
decl_in_std_namespace_p (tree decl)
{
  while (decl)
    {
      decl = decl_namespace_context (decl);
      if (DECL_NAMESPACE_STD_P (decl))
	return true;
      /* Allow inline namespaces inside of std namespace, e.g. with
	 --enable-symvers=gnu-versioned-namespace std::forward would be
	 actually std::_8::forward.  */
      if (!DECL_NAMESPACE_INLINE_P (decl))
	return false;
      decl = CP_DECL_CONTEXT (decl);
    }
  return false;
}

/* Return true if FNDECL is std::construct_at.  */

static inline bool
is_std_construct_at (tree fndecl)
{
  if (!decl_in_std_namespace_p (fndecl))
    return false;

  tree name = DECL_NAME (fndecl);
  return name && id_equal (name, "construct_at");
}

/* Return true if FNDECL is __dynamic_cast.  */

static inline bool
cxx_dynamic_cast_fn_p (tree fndecl)
{
  return (id_equal (DECL_NAME (fndecl), "__dynamic_cast")
	  && CP_DECL_CONTEXT (fndecl) == global_namespace);
}

/* Return true if FNDECL is std::allocator<T>::{,de}allocate.  */

static inline bool
is_std_allocator_allocate (tree fndecl)
{
  tree name = DECL_NAME (fndecl);
  if (name == NULL_TREE
      || !(id_equal (name, "allocate") || id_equal (name, "deallocate")))
    return false;

  tree ctx = DECL_CONTEXT (fndecl);
  if (ctx == NULL_TREE || !CLASS_TYPE_P (ctx) || !TYPE_MAIN_DECL (ctx))
    return false;

  tree decl = TYPE_MAIN_DECL (ctx);
  name = DECL_NAME (decl);
  if (name == NULL_TREE || !id_equal (name, "allocator"))
    return false;

  return decl_in_std_namespace_p (decl);
}

/* Overload for the above taking rust_constexpr_call*.  */

static inline bool
is_std_allocator_allocate (const rust_constexpr_call *call)
{
  return (call && call->fundef
	  && is_std_allocator_allocate (call->fundef->decl));
}

/* Return true if T denotes a potentially constant expression.  Issue
   diagnostic as appropriate under control of FLAGS.  If WANT_RVAL is true,
   an lvalue-rvalue conversion is implied.  If NOW is true, we want to
   consider the expression in the current context, independent of constexpr
   substitution.

   C++0x [expr.const] used to say

   6 An expression is a potential constant expression if it is
     a constant expression where all occurrences of function
     parameters are replaced by arbitrary constant expressions
     of the appropriate type.

   2  A conditional expression is a constant expression unless it
      involves one of the following as a potentially evaluated
      subexpression (3.2), but subexpressions of logical AND (5.14),
      logical OR (5.15), and conditional (5.16) operations that are
      not evaluated are not considered.   */

static bool
potential_constant_expression_1 (tree t, bool want_rval, bool strict, bool now,
				 tsubst_flags_t flags, tree *jump_target)
{
#define RECUR(T, RV)                                                           \
  potential_constant_expression_1 ((T), (RV), strict, now, flags, jump_target)

  enum
  {
    any = false,
    rval = true
  };
  int i;
  tree tmp;

  if (t == error_mark_node)
    return false;
  if (t == NULL_TREE)
    return true;
  location_t loc = rs_expr_loc_or_input_loc (t);

  if (*jump_target)
    /* If we are jumping, ignore everything.  This is simpler than the
       cxx_eval_constant_expression handling because we only need to be
       conservatively correct, and we don't necessarily have a constant value
       available, so we don't bother with switch tracking.  */
    return true;

  if (TREE_THIS_VOLATILE (t) && want_rval)
    {
      if (flags & tf_error)
	error_at (loc,
		  "lvalue-to-rvalue conversion of a volatile lvalue "
		  "%qE with type %qT",
		  t, TREE_TYPE (t));
      return false;
    }
  if (CONSTANT_CLASS_P (t))
    return true;
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPED)
      && TREE_TYPE (t) == error_mark_node)
    return false;

  switch (TREE_CODE (t))
    {
    case FUNCTION_DECL:
    case OVERLOAD:
    case LABEL_DECL:
    case CASE_LABEL_EXPR:
    case PREDICT_EXPR:
    case CONST_DECL:
    case IDENTIFIER_NODE:
      /* We can see a FIELD_DECL in a pointer-to-member expression.  */
    case FIELD_DECL:
    case RESULT_DECL:
    case PLACEHOLDER_EXPR:
    case STATIC_ASSERT:
      return true;

    case RETURN_EXPR:
      if (!RECUR (TREE_OPERAND (t, 0), any))
	return false;
      /* FALLTHROUGH */

    case BREAK_STMT:
    case CONTINUE_STMT:
      *jump_target = t;
      return true;

    case PARM_DECL:
      if (now && want_rval)
	{
	  tree type = TREE_TYPE (t);
	  if (is_really_empty_class (type, /*ignore_vptr*/ false))
	    /* An empty class has no data to read.  */
	    return true;
	  if (flags & tf_error)
	    error ("%qE is not a constant expression", t);
	  return false;
	}
      return true;

    case CALL_EXPR:
      /* -- an invocation of a function other than a constexpr function
	    or a constexpr constructor.  */
      {
	tree fun = get_function_named_in_call (t);
	const int nargs = call_expr_nargs (t);
	i = 0;

	if (fun == NULL_TREE)
	  {
	    /* Reset to allow the function to continue past the end
	       of the block below.  Otherwise return early.  */
	    bool bail = true;

	    if (TREE_CODE (t) == CALL_EXPR && CALL_EXPR_FN (t) == NULL_TREE)
	      switch (CALL_EXPR_IFN (t))
		{
		/* These should be ignored, they are optimized away from
		   constexpr functions.  */
		case IFN_UBSAN_NULL:
		case IFN_UBSAN_BOUNDS:
		case IFN_UBSAN_VPTR:
		case IFN_FALLTHROUGH:
		  return true;

		case IFN_ADD_OVERFLOW:
		case IFN_SUB_OVERFLOW:
		case IFN_MUL_OVERFLOW:
		case IFN_LAUNDER:
		case IFN_VEC_CONVERT:
		  bail = false;
		  break;

		default:
		  break;
		}

	    if (bail)
	      {
		/* fold_call_expr can't do anything with IFN calls.  */
		if (flags & tf_error)
		  error_at (loc, "call to internal function %qE", t);
		return false;
	      }
	  }

	if (fun && is_overloaded_fn (fun))
	  {
	    if (TREE_CODE (fun) == FUNCTION_DECL)
	      {
		if (builtin_valid_in_constant_expr_p (fun))
		  return true;
		if (!maybe_constexpr_fn (fun)
		    /* Allow any built-in function; if the expansion
		       isn't constant, we'll deal with that then.  */
		    && !fndecl_built_in_p (fun)
		    /* In C++20, replaceable global allocation functions
		       are constant expressions.  */
		    && (/* !cxx_replaceable_global_alloc_fn (fun)
			||*/ TREE_CODE (t) != CALL_EXPR
			|| (!CALL_FROM_NEW_OR_DELETE_P (t)
			    && (current_function_decl == NULL_TREE
				/*|| !is_std_allocator_allocate(current_function_decl)*/)))
		    /* Allow placement new in std::construct_at.  */
		    && (/*!cxx_placement_new_fn (fun)
			||*/ TREE_CODE (t) != CALL_EXPR
			|| current_function_decl == NULL_TREE
			/*|| !is_std_construct_at (current_function_decl)*/)
		  /*  && !cxx_dynamic_cast_fn_p (fun)*/)
		  {
		    if (flags & tf_error)
		      {
			error_at (loc, "call to non-%<constexpr%> function %qD",
				  fun);
			explain_invalid_constexpr_fn (fun);
		      }
		    return false;
		  }
		/* A call to a non-static member function takes the address
		   of the object as the first argument.  But in a constant
		   expression the address will be folded away, so look
		   through it now.  */
		if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
		    && !DECL_CONSTRUCTOR_P (fun))
		  {
		    tree x = CALL_EXPR_ARG (t, 0);

		    /* Don't require an immediately constant value, as
		       constexpr substitution might not use the value.  */
		    bool sub_now = false;
		    if (!potential_constant_expression_1 (x, rval, strict,
							  sub_now, flags,
							  jump_target))
		      return false;
		    i = 1;
		  }
	      }
	    else
	      {
		if (!RECUR (fun, true))
		  return false;
		fun = get_first_fn (fun);
	      }
	    fun = DECL_ORIGIN (fun);
	  }
	else if (fun)
	  {
	    if (RECUR (fun, rval))
	      /* Might end up being a constant function pointer.  */;
	    else
	      return false;
	  }
	for (; i < nargs; ++i)
	  {
	    tree x = CALL_EXPR_ARG (t, i);
	    /* In a template, reference arguments haven't been converted to
	       REFERENCE_TYPE and we might not even know if the parameter
	       is a reference, so accept lvalue constants too.  */
	    bool rv = rval;
	    /* Don't require an immediately constant value, as constexpr
	       substitution might not use the value of the argument.  */
	    bool sub_now = false;
	    if (!potential_constant_expression_1 (x, rv, strict, sub_now, flags,
						  jump_target))
	      return false;
	  }
	return true;
      }

    case NON_LVALUE_EXPR:
      /* -- an lvalue-to-rvalue conversion (4.1) unless it is applied to
	    -- an lvalue of integral type that refers to a non-volatile
	       const variable or static data member initialized with
	       constant expressions, or

	    -- an lvalue of literal type that refers to non-volatile
	       object defined with constexpr, or that refers to a
	       sub-object of such an object;  */
      return RECUR (TREE_OPERAND (t, 0), rval);

    case VAR_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	{
	  return RECUR (DECL_VALUE_EXPR (t), rval);
	}
      if (want_rval && !var_in_maybe_constexpr_fn (t)
	  && !decl_maybe_constant_var_p (t)
	  && (strict || !RS_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (t))
	      || (DECL_INITIAL (t)
		  && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (t)))
	  && COMPLETE_TYPE_P (TREE_TYPE (t))
	  && !is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/ false))
	{
	  if (flags & tf_error)
	    non_const_var_error (loc, t);
	  return false;
	}
      return true;

      /* FALLTHRU */
    case NOP_EXPR:
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
      /* -- a reinterpret_cast.  FIXME not implemented, and this rule
	 may change to something more specific to type-punning (DR 1312).  */
      {
	tree from = TREE_OPERAND (t, 0);
	if (location_wrapper_p (t))
	  return (RECUR (from, want_rval));
	if (INDIRECT_TYPE_P (TREE_TYPE (t)))
	  {
	    STRIP_ANY_LOCATION_WRAPPER (from);
	    if (TREE_CODE (from) == INTEGER_CST && !integer_zerop (from))
	      {
		if (flags & tf_error)
		  error_at (loc,
			    "%<reinterpret_cast%> from integer to pointer");
		return false;
	      }
	  }
	return (RECUR (from, TREE_CODE (t) != VIEW_CONVERT_EXPR));
      }

    case ADDR_EXPR:
      /* -- a unary operator & that is applied to an lvalue that
	    designates an object with thread or automatic storage
	    duration;  */
      t = TREE_OPERAND (t, 0);

      if (TREE_CODE (t) == OFFSET_REF && PTRMEM_OK_P (t))
	/* A pointer-to-member constant.  */
	return true;

	// handle_addr_expr:
#if 0
      /* FIXME adjust when issue 1197 is fully resolved.  For now don't do
         any checking here, as we might dereference the pointer later.  If
         we remove this code, also remove check_automatic_or_tls.  */
      i = check_automatic_or_tls (t);
      if (i == ck_ok)
	return true;
      if (i == ck_bad)
        {
          if (flags & tf_error)
            error ("address-of an object %qE with thread local or "
                   "automatic storage is not a constant expression", t);
          return false;
        }
#endif
      return RECUR (t, any);

    case COMPONENT_REF:
      /* -- a class member access unless its postfix-expression is
	    of literal type or of pointer to literal type.  */
      /* This test would be redundant, as it follows from the
	 postfix-expression being a potential constant expression.  */
      if (type_unknown_p (t))
	return true;
      if (is_overloaded_fn (t))
	/* In a template, a COMPONENT_REF of a function expresses ob.fn(),
	   which uses ob as an lvalue.  */
	want_rval = false;
      gcc_fallthrough ();

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case BIT_FIELD_REF:
      return RECUR (TREE_OPERAND (t, 0), want_rval);

      case INDIRECT_REF: {
	tree x = TREE_OPERAND (t, 0);
	STRIP_NOPS (x);
	return RECUR (x, rval);
      }

    case STATEMENT_LIST:
      for (tree stmt : tsi_range (t))
	if (!RECUR (stmt, any))
	  return false;
      return true;

    case MODIFY_EXPR:
      if (!RECUR (TREE_OPERAND (t, 0), any))
	return false;
      /* Just ignore clobbers.  */
      if (TREE_CLOBBER_P (TREE_OPERAND (t, 1)))
	return true;
      if (!RECUR (TREE_OPERAND (t, 1), rval))
	return false;
      return true;

    case FOR_STMT:
      if (!RECUR (FOR_INIT_STMT (t), any))
	return false;
      tmp = FOR_COND (t);
      if (!RECUR (tmp, rval))
	return false;
      if (tmp)
	{
	  tmp = cxx_eval_outermost_constant_expr (tmp, true);
	  /* If we couldn't evaluate the condition, it might not ever be
	     true.  */
	  if (!integer_onep (tmp))
	    {
	      /* Before returning true, check if the for body can contain
		 a return.  */
	      hash_set<tree> pset;
	      check_for_return_continue_data data
		= {&pset, NULL_TREE, NULL_TREE};
	      if (tree ret_expr
		  = rs_walk_tree (&FOR_BODY (t), check_for_return_continue,
				  &data, &pset))
		*jump_target = ret_expr;
	      return true;
	    }
	}
      if (!RECUR (FOR_EXPR (t), any))
	return false;
      if (!RECUR (FOR_BODY (t), any))
	return false;
      if (breaks (jump_target) || continues (jump_target))
	*jump_target = NULL_TREE;
      return true;

    case WHILE_STMT:
      tmp = WHILE_COND (t);
      if (!RECUR (tmp, rval))
	return false;

      tmp = cxx_eval_outermost_constant_expr (tmp, true);
      /* If we couldn't evaluate the condition, it might not ever be true.  */
      if (!integer_onep (tmp))
	{
	  /* Before returning true, check if the while body can contain
	     a return.  */
	  hash_set<tree> pset;
	  check_for_return_continue_data data = {&pset, NULL_TREE, NULL_TREE};
	  if (tree ret_expr
	      = rs_walk_tree (&WHILE_BODY (t), check_for_return_continue, &data,
			      &pset))
	    *jump_target = ret_expr;
	  return true;
	}
      if (!RECUR (WHILE_BODY (t), any))
	return false;
      if (breaks (jump_target) || continues (jump_target))
	*jump_target = NULL_TREE;
      return true;

    case SWITCH_STMT:
      if (!RECUR (SWITCH_STMT_COND (t), rval))
	return false;
      /* FIXME we don't check SWITCH_STMT_BODY currently, because even
	 unreachable labels would be checked and it is enough if there is
	 a single switch cond value for which it is a valid constant
	 expression.  We need to check if there are any RETURN_EXPRs
	 or CONTINUE_STMTs inside of the body though, as in that case
	 we need to set *jump_target.  */
      else
	{
	  hash_set<tree> pset;
	  check_for_return_continue_data data = {&pset, NULL_TREE, NULL_TREE};
	  if (tree ret_expr
	      = rs_walk_tree (&SWITCH_STMT_BODY (t), check_for_return_continue,
			      &data, &pset))
	    /* The switch might return.  */
	    *jump_target = ret_expr;
	  else if (data.continue_stmt)
	    /* The switch can't return, but might continue.  */
	    *jump_target = data.continue_stmt;
	}
      return true;

    case DYNAMIC_CAST_EXPR:
    case PSEUDO_DTOR_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case OMP_PARALLEL:
    case OMP_TASK:
    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OMP_TASKLOOP:
    case OMP_LOOP:
    case OMP_TEAMS:
    case OMP_TARGET_DATA:
    case OMP_TARGET:
    case OMP_SECTIONS:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_SINGLE:
    case OMP_SECTION:
    case OMP_MASTER:
    case OMP_MASKED:
    case OMP_TASKGROUP:
    case OMP_TARGET_UPDATE:
    case OMP_TARGET_ENTER_DATA:
    case OMP_TARGET_EXIT_DATA:
    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
    case OMP_DEPOBJ:
    case OACC_PARALLEL:
    case OACC_KERNELS:
    case OACC_SERIAL:
    case OACC_DATA:
    case OACC_HOST_DATA:
    case OACC_LOOP:
    case OACC_CACHE:
    case OACC_DECLARE:
    case OACC_ENTER_DATA:
    case OACC_EXIT_DATA:
    case OACC_UPDATE:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case TRANSACTION_EXPR:
    case AT_ENCODE_EXPR:

      if (flags & tf_error)
	error_at (loc, "expression %qE is not a constant expression", t);
      return false;

    case ASM_EXPR:
      if (flags & tf_error)
	inline_asm_in_constexpr_error (loc);
      return false;

    case OBJ_TYPE_REF:
      return true;

    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
      want_rval = true;
      goto binary;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case SPACESHIP_EXPR:
      want_rval = true;
      goto binary;

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      goto unary;

    case BIT_NOT_EXPR:
      /* A destructor.  */
      if (TYPE_P (TREE_OPERAND (t, 0)))
	return true;
      /* fall through.  */

    case CONJ_EXPR:
    case SAVE_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case ABSU_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    case UNARY_PLUS_EXPR:
    case UNARY_LEFT_FOLD_EXPR:
    case UNARY_RIGHT_FOLD_EXPR:
    unary:
      return RECUR (TREE_OPERAND (t, 0), rval);

    case BIND_EXPR:
      return RECUR (BIND_EXPR_BODY (t), want_rval);

    case CLEANUP_POINT_EXPR:
    case EXPR_STMT:
    case PAREN_EXPR:
      /* For convenience.  */
    case LOOP_EXPR:
    case EXIT_EXPR:
      return RECUR (TREE_OPERAND (t, 0), want_rval);

    case DECL_EXPR:
      tmp = DECL_EXPR_DECL (t);
      if (VAR_P (tmp) && !DECL_ARTIFICIAL (tmp))
	{
	  if (RS_DECL_THREAD_LOCAL_P (tmp))
	    {
	      if (flags & tf_error)
		error_at (DECL_SOURCE_LOCATION (tmp),
			  "%qD declared "
			  "%<thread_local%> in %<constexpr%> context",
			  tmp);
	      return false;
	    }
	  else if (TREE_STATIC (tmp))
	    {
	      if (flags & tf_error)
		error_at (DECL_SOURCE_LOCATION (tmp),
			  "%qD declared "
			  "%<static%> in %<constexpr%> context",
			  tmp);
	      return false;
	    }
	  else if (!check_for_uninitialized_const_var (
		     tmp, /*constexpr_context_p=*/true, flags))
	    return false;
	}
      return RECUR (tmp, want_rval);

    case TRY_FINALLY_EXPR:
      return (RECUR (TREE_OPERAND (t, 0), want_rval)
	      && RECUR (TREE_OPERAND (t, 1), any));

    case SCOPE_REF:
      return RECUR (TREE_OPERAND (t, 1), want_rval);

    case TARGET_EXPR:
      if (!TARGET_EXPR_DIRECT_INIT_P (t) && !literal_type_p (TREE_TYPE (t)))
	{
	  if (flags & tf_error)
	    {
	      auto_diagnostic_group d;
	      error_at (loc,
			"temporary of non-literal type %qT in a "
			"constant expression",
			TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  return false;
	}
      /* FALLTHRU */
    case INIT_EXPR:
      return RECUR (TREE_OPERAND (t, 1), rval);

      case CONSTRUCTOR: {
	vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
	constructor_elt *ce;
	for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
	  if (!RECUR (ce->value, want_rval))
	    return false;
	return true;
      }

      case TREE_LIST: {
	gcc_assert (TREE_PURPOSE (t) == NULL_TREE || DECL_P (TREE_PURPOSE (t)));
	if (!RECUR (TREE_VALUE (t), want_rval))
	  return false;
	if (TREE_CHAIN (t) == NULL_TREE)
	  return true;
	return RECUR (TREE_CHAIN (t), want_rval);
      }

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
      case ROUND_MOD_EXPR: {
	tree denom = TREE_OPERAND (t, 1);
	if (!RECUR (denom, rval))
	  return false;
	/* We can't call cxx_eval_outermost_constant_expr on an expression
	   that hasn't been through instantiate_non_dependent_expr yet.  */
	denom = cxx_eval_outermost_constant_expr (denom, true);
	if (integer_zerop (denom))
	  {
	    if (flags & tf_error)
	      error ("division by zero is not a constant expression");
	    return false;
	  }
	else
	  {
	    want_rval = true;
	    return RECUR (TREE_OPERAND (t, 0), want_rval);
	  }
      }

      case COMPOUND_EXPR: {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if (TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	  return RECUR (op0, want_rval);
	else
	  goto binary;
      }

      /* If the first operand is the non-short-circuit constant, look at
	 the second operand; otherwise we only care about the first one for
	 potentiality.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      tmp = boolean_true_node;
      goto truth;
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      tmp = boolean_false_node;
      truth : {
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	if (!RECUR (op0, rval))
	  return false;
	if (!(flags & tf_error) && RECUR (op1, rval))
	  /* When quiet, try to avoid expensive trial evaluation by first
	     checking potentiality of the second operand.  */
	  return true;
	op0 = cxx_eval_outermost_constant_expr (op0, true);
	if (tree_int_cst_equal (op0, tmp))
	  return (flags & tf_error) ? RECUR (op1, rval) : false;
	else
	  return true;
      }

    case PLUS_EXPR:
    case MULT_EXPR:
    case POINTER_PLUS_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      want_rval = true;
      /* Fall through.  */
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
    case MEM_REF:
    case BINARY_LEFT_FOLD_EXPR:
    case BINARY_RIGHT_FOLD_EXPR:
    binary:
      for (i = 0; i < 2; ++i)
	if (!RECUR (TREE_OPERAND (t, i), want_rval))
	  return false;
      return true;

    case VEC_PERM_EXPR:
      for (i = 0; i < 3; ++i)
	if (!RECUR (TREE_OPERAND (t, i), true))
	  return false;
      return true;

    case COND_EXPR:
      if (COND_EXPR_IS_VEC_DELETE (t))
	{
	  if (flags & tf_error)
	    error_at (loc, "%<delete[]%> is not a constant expression");
	  return false;
	}
      /* Fall through.  */
    case IF_STMT:
    case VEC_COND_EXPR:
      /* If the condition is a known constant, we know which of the legs we
	 care about; otherwise we only require that the condition and
	 either of the legs be potentially constant.  */
      tmp = TREE_OPERAND (t, 0);
      if (!RECUR (tmp, rval))
	return false;

      tmp = cxx_eval_outermost_constant_expr (tmp, true);
      /* potential_constant_expression* isn't told if it is called for
	 manifestly_const_eval or not, so for consteval if always
	 process both branches as if the condition is not a known
	 constant.  */
      if (TREE_CODE (t) != IF_STMT || !IF_STMT_CONSTEVAL_P (t))
	{
	  if (integer_zerop (tmp))
	    return RECUR (TREE_OPERAND (t, 2), want_rval);
	  else if (TREE_CODE (tmp) == INTEGER_CST)
	    return RECUR (TREE_OPERAND (t, 1), want_rval);
	}
      tmp = *jump_target;
      for (i = 1; i < 3; ++i)
	{
	  tree this_jump_target = tmp;
	  if (potential_constant_expression_1 (TREE_OPERAND (t, i), want_rval,
					       strict, now, tf_none,
					       &this_jump_target))
	    {
	      if (returns (&this_jump_target))
		*jump_target = this_jump_target;
	      else if (!returns (jump_target))
		{
		  if (breaks (&this_jump_target)
		      || continues (&this_jump_target))
		    *jump_target = this_jump_target;
		  if (i == 1)
		    {
		      /* If the then branch is potentially constant, but
			 does not return, check if the else branch
			 couldn't return, break or continue.  */
		      hash_set<tree> pset;
		      check_for_return_continue_data data
			= {&pset, NULL_TREE, NULL_TREE};
		      if (tree ret_expr
			  = rs_walk_tree (&TREE_OPERAND (t, 2),
					  check_for_return_continue, &data,
					  &pset))
			*jump_target = ret_expr;
		      else if (*jump_target == NULL_TREE)
			{
			  if (data.continue_stmt)
			    *jump_target = data.continue_stmt;
			  else if (data.break_stmt)
			    *jump_target = data.break_stmt;
			}
		    }
		}
	      return true;
	    }
	}
      if (flags & tf_error)
	error_at (loc, "expression %qE is not a constant expression", t);
      return false;

    case TYPE_DECL:
      /* We can see these in statement-expressions.  */
      return true;

    case LABEL_EXPR:
      t = LABEL_EXPR_LABEL (t);
      if (DECL_ARTIFICIAL (t))
	return true;
      else if (flags & tf_error)
	error_at (loc, "label definition in %<constexpr%> function only "
		       "available with %<-std=c++2b%> or %<-std=gnu++2b%>");
      return false;

    case ANNOTATE_EXPR:
      return RECUR (TREE_OPERAND (t, 0), rval);

    case BIT_CAST_EXPR:
      return RECUR (TREE_OPERAND (t, 0), rval);

    default:
      sorry ("unexpected AST of kind %s", get_tree_code_name (TREE_CODE (t)));
      rust_unreachable ();
      return false;
    }
#undef RECUR
}

bool
potential_constant_expression_1 (tree t, bool want_rval, bool strict, bool now,
				 tsubst_flags_t flags)
{
  if (flags & tf_error)
    {
      /* Check potentiality quietly first, as that could be performed more
	 efficiently in some cases (currently only for TRUTH_*_EXPR).  If
	 that fails, replay the check noisily to give errors.  */
      flags &= ~tf_error;
      if (potential_constant_expression_1 (t, want_rval, strict, now, flags))
	return true;
      flags |= tf_error;
    }

  tree target = NULL_TREE;
  return potential_constant_expression_1 (t, want_rval, strict, now, flags,
					  &target);
}

// forked from gcc/cp/constexpr.cc fold_non_dependent_init

/* Like maybe_constant_init but first fully instantiate the argument.  */

tree
fold_non_dependent_init (tree t, tsubst_flags_t /*=tf_warning_or_error*/,
			 bool manifestly_const_eval /*=false*/,
			 tree object /* = NULL_TREE */)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  return maybe_constant_init (t, object, manifestly_const_eval);
}

} // namespace Compile
} // namespace Rust

using namespace Rust::Compile;

#include "gt-rust-rust-constexpr.h"
