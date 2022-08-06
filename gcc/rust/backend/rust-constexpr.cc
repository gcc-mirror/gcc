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
#include "rust-target.h"
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
#include "rust-target.h"

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
inline tree
get_nth_callarg (tree t, int n);
tree
unshare_constructor (tree t MEM_STAT_DECL);
void
explain_invalid_constexpr_fn (tree fun);
void
maybe_save_constexpr_fundef (tree fun);

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

struct GTY ((for_user)) constexpr_fundef
{
  tree decl;
  tree body;
  tree parms;
  tree result;
};

/* Objects of this type represent calls to constexpr functions
 along with the bindings of parameters to their arguments, for
 the purpose of compile time evaluation.  */

struct GTY ((for_user)) constexpr_call
{
  /* Description of the constexpr function definition.  */
  constexpr_fundef *fundef;
  /* Parameter bindings environment.  A TREE_VEC of arguments.  */
  tree bindings;
  /* Result of the call.
       NULL means the call is being evaluated.
       error_mark_node means that the evaluation was erroneous;
       otherwise, the actuall value of the call.  */
  tree result;
  /* The hash of this call; we remember it here to avoid having to
     recalculate it when expanding the hash table.  */
  hashval_t hash;
  /* Whether __builtin_is_constant_evaluated() should evaluate to true.  */
  bool manifestly_const_eval;
};

struct constexpr_call_hasher : ggc_ptr_hash<constexpr_call>
{
  static hashval_t hash (constexpr_call *);
  static bool equal (constexpr_call *, constexpr_call *);
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
  constexpr_call *call;
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

struct constexpr_fundef_hasher : ggc_ptr_hash<constexpr_fundef>
{
  static hashval_t hash (const constexpr_fundef *);
  static bool equal (const constexpr_fundef *, const constexpr_fundef *);
};

/* This table holds all constexpr function definitions seen in
   the current translation unit.  */

static GTY (()) hash_table<constexpr_fundef_hasher> *constexpr_fundef_table;

/* Utility function used for managing the constexpr function table.
   Return true if the entries pointed to by P and Q are for the
   same constexpr function.  */

inline bool
constexpr_fundef_hasher::equal (const constexpr_fundef *lhs,
				const constexpr_fundef *rhs)
{
  return lhs->decl == rhs->decl;
}

/* Utility function used for managing the constexpr function table.
   Return a hash value for the entry pointed to by Q.  */

inline hashval_t
constexpr_fundef_hasher::hash (const constexpr_fundef *fundef)
{
  return DECL_UID (fundef->decl);
}

/* Return a previously saved definition of function FUN.   */

constexpr_fundef *
retrieve_constexpr_fundef (tree fun)
{
  if (constexpr_fundef_table == NULL)
    return NULL;

  constexpr_fundef fundef = {fun, NULL_TREE, NULL_TREE, NULL_TREE};
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

static GTY (()) hash_table<constexpr_call_hasher> *constexpr_call_table;

static tree
constexp_expression (const constexpr_ctx *, tree, bool, bool *, bool *,
		     tree * = NULL);

/* Compute a hash value for a constexpr call representation.  */

inline hashval_t
constexpr_call_hasher::hash (constexpr_call *info)
{
  return info->hash;
}

/* Return true if the objects pointed to by P and Q represent calls
   to the same constexpr function with the same arguments.
   Otherwise, return false.  */

bool
constexpr_call_hasher::equal (constexpr_call *lhs, constexpr_call *rhs)
{
  if (lhs == rhs)
    return true;
  if (lhs->hash != rhs->hash)
    return false;
  if (lhs->manifestly_const_eval != rhs->manifestly_const_eval)
    return false;
  if (!constexpr_fundef_hasher::equal (lhs->fundef, rhs->fundef))
    return false;
  return rs_tree_equal (lhs->bindings, rhs->bindings);
}

/* Initialize the constexpr call table, if needed.  */

static void
maybe_initialize_constexpr_call_table (void)
{
  if (constexpr_call_table == NULL)
    constexpr_call_table = hash_table<constexpr_call_hasher>::create_ggc (101);
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
get_fundef_copy (constexpr_fundef *fundef)
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
  poly_uint64 const_op01;

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
	// new_ctx.object = build_ctor_subob_ref (index, type, ctx->object);
	;
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

  gcc_unreachable ();
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

static tree
eval_constant_expression (const constexpr_ctx *ctx, tree t, bool lval,
			  bool *non_constant_p, bool *overflow_p,
			  tree *jump_target /* = NULL */)
{
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
	Location (loc),
	"%<constexpr%> evaluation operation count exceeds limit of "
	"%wd (use %<-fconstexpr-ops-limit=%> to increase the limit)",
	constexpr_ops_limit);

      return t;
    }

  tree r = t;
  tree_code tcode = TREE_CODE (t);
  switch (tcode)
    {
      case CONST_DECL: {
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
      r = eval_binary_expression (ctx, t, false, non_constant_p, overflow_p);
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

    case CALL_EXPR:
      r = eval_call_expression (ctx, t, false, non_constant_p, overflow_p);
      break;

    case RETURN_EXPR:
      rust_assert (TREE_OPERAND (t, 0) != NULL_TREE);
      r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), false,
				    non_constant_p, overflow_p);
      break;

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

    /* These differ from cxx_eval_unary_expression in that this doesn't
	 check for a constant operand or result; an address can be
	 constant without its operand being, and vice versa.  */
    case MEM_REF:
    case INDIRECT_REF:
      r = rs_eval_indirect_ref (ctx, t, lval, non_constant_p, overflow_p);
      break;

    case PAREN_EXPR:
      gcc_assert (!REF_PARENTHESIZED_P (t));
      /* A PAREN_EXPR resulting from __builtin_assoc_barrier has no effect in
	 constant expressions since it's unaffected by -fassociative-math.  */
      r = eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
				    non_constant_p, overflow_p);
      break;

    case NOP_EXPR:
      if (REINTERPRET_CAST_P (t))
	{
	  if (!ctx->quiet)
	    error_at (loc, "%<reinterpret_cast%> is not a constant expression");
	  *non_constant_p = true;
	  return t;
	}

    case MODIFY_EXPR:
      r = eval_store_expression (ctx, t, false, non_constant_p, overflow_p);
      break;

    case STATEMENT_LIST:
      // new_ctx = *ctx;
      // new_ctx.ctor = new_ctx.object = NULL_TREE;
      return eval_statement_list (ctx, t, non_constant_p, overflow_p,
				  jump_target);

    case BIND_EXPR:
      return eval_constant_expression (ctx, BIND_EXPR_BODY (t), lval,
				       non_constant_p, overflow_p, jump_target);

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
	gcc_checking_assert (TREE_CODE (op) != CONSTRUCTOR);
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
  bool mutable_p = false;
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
	      mutable_p = true;
	    if (TREE_CODE (probe) == ARRAY_REF)
	      {
		// TODO
		gcc_unreachable ();
		// elt = eval_and_check_array_index (ctx, probe, false,
		// 				  non_constant_p, overflow_p);
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

  /* Don't share a CONSTRUCTOR that might be changed later.  */
  init = unshare_constructor (init);

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
  else if (TREE_CODE (init) == CONSTRUCTOR
	   && !same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (init),
							  type))
    {
      /* See above on initialization of empty bases.  */
      gcc_assert (is_empty_class (TREE_TYPE (init)) && !lval);
      return init;
    }
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
      x = get_nth_callarg (t, i);

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

// Subroutine of cxx_eval_constant_expression.
// Evaluate the call expression tree T in the context of OLD_CALL expression
// evaluation.
static tree
eval_call_expression (const constexpr_ctx *ctx, tree t, bool lval,
		      bool *non_constant_p, bool *overflow_p)
{
  location_t loc = EXPR_LOCATION (t);
  tree fun = get_function_named_in_call (t);
  constexpr_call new_call = {NULL, NULL, NULL, 0, ctx->manifestly_const_eval};
  int depth_ok;

  if (fun == NULL_TREE)
    {
      // return cxx_eval_internal_function (ctx, t, lval,
      //     			       non_constant_p, overflow_p);
      gcc_unreachable ();
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
  constexpr_call *entry = NULL;
  if (depth_ok && !non_constant_args && ctx->strict)
    {
      new_call.hash = constexpr_fundef_hasher::hash (new_call.fundef);
      // new_call.hash
      //   = iterative_hash_template_arg (new_call.bindings, new_call.hash);
      new_call.hash
	= iterative_hash_object (ctx->manifestly_const_eval, new_call.hash);

      /* If we have seen this call before, we are done.  */
      maybe_initialize_constexpr_call_table ();
      constexpr_call **slot
	= constexpr_call_table->find_slot (&new_call, INSERT);
      entry = *slot;
      if (entry == NULL)
	{
	  /* Only cache up to constexpr_cache_depth to limit memory use.  */
	  if (depth_ok < constexpr_cache_depth)
	    {
	      /* We need to keep a pointer to the entry, not just the slot, as
		 the slot can move during evaluation of the body.  */
	      *slot = entry = ggc_alloc<constexpr_call> ();
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
		  if (!ctx->quiet)
		    error ("%<constexpr%> call flows off the end "
			   "of the function");
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
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p)
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

// forked from gcc/cp/constexpr.cc get_nth_callarg

/* We have an expression tree T that represents a call, either CALL_EXPR.
  Return the Nth argument.  */

inline tree
get_nth_callarg (tree t, int n)
{
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return CALL_EXPR_ARG (t, n);

    default:
      gcc_unreachable ();
      return NULL;
    }
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
      gcc_unreachable ();
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
    gcc_unreachable ();

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
	    auto_diagnostic_group d;
	    error ("invalid type for parameter %d of %<constexpr%> "
		   "function %q+#D",
		   DECL_PARM_INDEX (parm), fun);
	  }
      }

  return ret;
}

void
explain_invalid_constexpr_fn (tree fun)
{
  static hash_set<tree> *diagnosed;
  tree body;

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
register_constexpr_fundef (const constexpr_fundef &value)
{
  /* Create the constexpr function table if necessary.  */
  if (constexpr_fundef_table == NULL)
    constexpr_fundef_table
      = hash_table<constexpr_fundef_hasher>::create_ggc (101);

  constexpr_fundef **slot = constexpr_fundef_table->find_slot (
    const_cast<constexpr_fundef *> (&value), INSERT);

  gcc_assert (*slot == NULL);
  *slot = ggc_alloc<constexpr_fundef> ();
  **slot = value;
}

/* We are processing the definition of the constexpr function FUN.
   Check that its body fulfills the apropriate requirements and
   enter it in the constexpr function definition table.  */

void
maybe_save_constexpr_fundef (tree fun)
{
  // FIXME

  constexpr_fundef entry = {fun, NULL_TREE, NULL_TREE, NULL_TREE};
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
      // FIXME
      // if (continues (jump_target))
      //   {
      //     if (label_matches (ctx, jump_target, stmt))
      //       /* Found it.  */
      //       *jump_target = NULL_TREE;
      //     else
      //       continue;
      //   }
      if (TREE_CODE (stmt) == DEBUG_BEGIN_STMT)
	continue;
      r = eval_constant_expression (ctx, stmt, false, non_constant_p,
				    overflow_p, jump_target);
      if (*non_constant_p)
	break;
      // FIXME
      // if (returns (jump_target) || breaks (jump_target))
      //   break;
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
  if (TREE_CODE (t) == IF_STMT && !val)
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
  gcc_unreachable ();
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
      gcc_unreachable ();
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

// #include "gt-rust-rust-constexpr.h"

} // namespace Compile
} // namespace Rust
