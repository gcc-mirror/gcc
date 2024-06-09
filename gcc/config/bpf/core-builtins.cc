/* Subroutines used for code generation for eBPF.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "rtl.h"
#include "output.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "expr.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple-fold.h"
#include "tree-pass.h"
#include "plugin.h"
#include "gimplify.h"

#include "ctfc.h"
#include "btf.h"
#include "btfext-out.h"
#include "core-builtins.h"

/* BPF CO-RE builtins definition.

    The expansion of CO-RE builtins occur in three steps:
    1. - bpf_resolve_overloaded_core_builtin (pack step)
      Right after the front-end, all of the CO-RE builtins are converted to an
      internal builtin __builtin_core_reloc, which takes a single argument and
      has polymorphic return value to fit the particular expected return type
      from the original builtin.  The first argument contains an index argument
      which points to the information stored in a vec<struct cr_builtins> which
      collects the required information from the original CO-RE builtin in
      order to use it later on in the __builtin_core_reloc expansion (the next
      step).

    2. - bpf_expand_core_builtin
      In this step, the __builtin_core_reloc is expanded to a
      unspec:UNSPEC_CORE_RELOC with 3 operands, destination, source and the
      index.  The index operand is the index in the vec constructed in the
      previous step.

    3. - final asm output (process step)
      This is the output of the unspec:UNSPEC_CORE_RELOC.  The index passed in
      the third operand is read and extracted as a integer from the rtx node.
      The data is collected from the vec and it is used to create the proper
      CO-RE relocation as well as do the final assembly output.  It also
      creates a label to mark the location of the move instruction that is used
      in the CO-RE relocation.

    The initialization of the CO-RE builtins infrastructure occurs in
    bpf_init_core_builtins function.  It creates a struct builtin_helpers_t
    arrays which defines the kind argument position, the callback helpers,
    kind, compare, pack and process, for each individual type of builtin
    argument possible in the original CO-RE builtins.

    More precisely, field expression, type and enum value, used in the
    following relocations:
      - __builtin_core_field_info (<field_expr>, <kind>)
      - __builtin_core_type_id (<type>, <kind>)
      - __builtin_core_type_info (<type>, <kind>)
      - __builtin_core_enum_value (<enum_value>, <kind>)

    The kind helper allows to identify the proper relocation for the builtin
    call based on the value within the kind argument.

    The compare helper is used to identify if a new builtin call has similar
    arguments to any other builtin call with the compiling unit.  This enables
    the possibility to optimize consecutive similar calls of the builtins.

    The pack helper callbacks are suppose to decode the original CO-RE builtin
    call arguments, verify that it is a valid tree node for the particular
    builtin, allocate a struct cr_local in vector and write it with the
    relevant data for the particular builtin type.

    The process helper should take the data constructed in the pack helper and
    create a struct cr_final element which contains the essential information
    to create a CO-RE relocation.
    This information is further used by the final assembly output step to
    define the CO-RE relocation and pass-through the default value for the
    original CO-RE builtin.

    BPF CO-RE preserve access is supported in two forms:
    - A target builtin, __builtin_preserve_access_index

      This builtin accepts a single argument.  Any access to an aggregate data
      structure (struct, union or array), also referred through the document as
      an field expression,  within the argument will be recorded by the CO-RE
      machinery, resulting in one or more relocations being inserted in the
      .BTF.ext section of the output.

    - An attribute, __attribute__((preserve_access_index))

      This attribute can be applied to struct and union types.  Any access done
      through a node typed with this attribute will be recorded by the CO-RE
      machinery.  This conversion is done in an independent gimple pass very
      early in compilation, making sure that the field expression is
      originating from a tree node which his type is attributed.

    Both these variants of preserve_access_index rely on a tree walker that
    identifies and converts any CO-RE valid field expressions.  Apart from the
    gimple specific requirements for the attribute implementation both builtin
    and attribute implementations rely on the same mechanism.  */

struct GTY(()) cr_builtins
{
  tree type;
  tree expr;
  tree default_value;
  rtx rtx_default_value;
  enum btf_core_reloc_kind kind;
  enum bpf_builtins orig_builtin_code;
  tree orig_arg_expr;
  tree access_node;
};
typedef struct cr_builtins *cr_builtins_ref;

#define CORE_BUILTINS_DATA_EMPTY \
  { NULL_TREE, NULL_TREE, NULL_TREE, NULL_RTX, BPF_RELO_INVALID, \
    BPF_BUILTIN_UNUSED, NULL_TREE, NULL_TREE}

/* Vector definition and its access function.  */
static GTY(()) vec<cr_builtins_ref, va_gc> *builtins_data = NULL;

static inline int
allocate_builtin_data ()
{
  if (builtins_data == NULL)
    vec_alloc (builtins_data, 1);

  cr_builtins_ref data = ggc_cleared_alloc<struct cr_builtins> ();
  int ret = builtins_data->length ();
  vec_safe_push (builtins_data, data);
  return ret;
}

static inline struct cr_builtins *
get_builtin_data (int index)
{
  return (*builtins_data)[index];
}

typedef bool
(*builtin_local_data_compare_fn) (struct cr_builtins *a,
				  struct cr_builtins *b);
static inline int
search_builtin_data (builtin_local_data_compare_fn callback,
		     struct cr_builtins *elem)
{
  unsigned int i;
  if (builtins_data != NULL)
    for (i = 0; i < builtins_data->length (); i++)
      if ((callback != NULL && (callback) (elem, (*builtins_data)[i]))
	  || (callback == NULL
	      && ((*builtins_data)[i]->orig_arg_expr == elem->orig_arg_expr)))
	return (int) i;

  return -1;
}

/* Possible relocation decisions.  */
enum cr_decision
{
  FAILED_VALIDATION = 0,
  REPLACE_CREATE_RELOCATION,
  REPLACE_NO_RELOCATION
};

/* Core Relocation Pack local structure.  */
struct cr_local
{
  struct cr_builtins reloc_data;
  enum cr_decision reloc_decision;
  bool fail;
};
#define CR_LOCAL_EMPTY { CORE_BUILTINS_DATA_EMPTY, FAILED_VALIDATION, false }

/* Core Relocation Final data */
struct cr_final
{
  const char *str;
  tree type;
  enum btf_core_reloc_kind kind;
};

/* CO-RE builtin helpers struct.  Used and initialized in
   bpf_init_core_builtins.  */
struct builtin_helpers
{
  enum btf_core_reloc_kind (*kind) (tree *args, int nargs);
  bool (*compare) (struct cr_builtins *a, struct cr_builtins *b);
  struct cr_local (*pack) (tree *args,
			   enum btf_core_reloc_kind kind,
			   enum bpf_builtins code);
  struct cr_final (*process) (struct cr_builtins *data);
  bool is_pure;
  bool is_valid;
};

struct builtin_helpers
  core_builtin_helpers[(int) BPF_BUILTIN_MAX];

#define BPF_CORE_HELPER_NOTSET { NULL, NULL, NULL, NULL, false, false }
#define BPF_CORE_HELPER_SET(KIND, COMPARE, PACK, PROCESS, IS_PURE) \
	{ KIND, COMPARE, PACK, PROCESS, IS_PURE, true }

enum bpf_plugin_states
{
  BPF_PLUGIN_DISABLED = 0,
  BPF_PLUGIN_ENABLED,
  BPF_PLUGIN_REMOVED
};
enum bpf_plugin_states plugin_state = BPF_PLUGIN_DISABLED;

static void
remove_parser_plugin ()
{
  /* Restore state of the plugin system.  */
  if (flag_plugin_added == true && plugin_state != BPF_PLUGIN_REMOVED)
    {
      unregister_callback ("bpf_collect_enum_info", PLUGIN_FINISH_TYPE);
      flag_plugin_added = (bool) plugin_state == BPF_PLUGIN_ENABLED;
      plugin_state = BPF_PLUGIN_REMOVED;
    }
}

#define bpf_error(MSG) { \
  remove_parser_plugin (); \
  error (MSG); \
}

#define bpf_error_at(LOC, MSG) { \
  remove_parser_plugin (); \
  error_at (LOC, MSG); \
}


/* Helper compare functions used to verify if multiple builtin calls contain
   the same argument as input.  In that case the builtin calls can be optimized
   out by identifying redundat calls.  This happen since the internal
   __core_reloc builtin is marked as PURE.  */

static inline bool
compare_same_kind (struct cr_builtins *a, struct cr_builtins *b)
{
  return a->kind == b->kind;
}
static inline bool
compare_same_ptr_expr (struct cr_builtins *a, struct cr_builtins *b)
{
  return compare_same_kind (a, b) && a->expr == b->expr;
}
static inline bool
compare_same_ptr_type (struct cr_builtins *a, struct cr_builtins *b)
{
  return compare_same_kind (a, b) && a->type == b->type;
}

/* Handling for __attribute__((preserve_access_index)) for BPF CO-RE support.

   This attribute marks a structure/union/array type as "preserve", so that
   every access to that type should be recorded and replayed by the BPF loader;
   this is just the same functionality as __builtin_preserve_access_index,
   but in the form of an attribute for an entire aggregate type.

   Note also that nested structs behave as though they all have the attribute.
   For example:
     struct X { int a; };
     struct Y { struct X bar} __attribute__((preserve_access_index));
     struct Y foo;
     foo.bar.a;
   will record access all the way to 'a', even though struct X does not have
   the preserve_access_index attribute.

   This is to follow LLVM behavior.  */

/* True if tree T accesses any member of a struct/union/class which is marked
   with the PRESERVE_ACCESS_INDEX attribute.  */

static bool
is_attr_preserve_access (tree t)
{
  if (t == NULL_TREE)
    return false;

  poly_int64 bitsize, bitpos;
  tree var_off;
  machine_mode mode;
  int sign, reverse, vol;

  tree base = get_inner_reference (t, &bitsize, &bitpos, &var_off, &mode,
				   &sign, &reverse, &vol);

  if (TREE_CODE (t) == SSA_NAME
      || TREE_CODE (t) == VAR_DECL)
    return lookup_attribute ("preserve_access_index",
			     TYPE_ATTRIBUTES (TREE_TYPE (base)));

  if (TREE_CODE (base) == MEM_REF)
    return lookup_attribute ("preserve_access_index",
			     TYPE_ATTRIBUTES (TREE_TYPE (base)));

  if (TREE_CODE (t) == COMPONENT_REF)
    {
      /* preserve_access_index propagates into nested structures,
	 so check whether this is a component of another component
	 which in turn is part of such a struct.  */

      const tree op = TREE_OPERAND (t, 0);

      if (TREE_CODE (op) == COMPONENT_REF)
	return is_attr_preserve_access (op);

      const tree container = DECL_CONTEXT (TREE_OPERAND (t, 1));

      return lookup_attribute ("preserve_access_index",
			       TYPE_ATTRIBUTES (container))
	     || is_attr_preserve_access (op);
    }

  else if (TREE_CODE (t) == ADDR_EXPR)
    return is_attr_preserve_access (TREE_OPERAND (t, 0));

  return false;
}

static tree
root_for_core_field_info (tree node)
{
  bool done = false;
  while (!done)
    {
      switch (TREE_CODE (node))
	{
	case ADDR_EXPR:
	case NOP_EXPR:
	  node = TREE_OPERAND (node, 0);
	  break;
	default:
	  done = true;
	  break;
	}
    }
  return node;
}

/* Expand a call to __builtin_preserve_field_info by evaluating the requested
   information about SRC according to KIND, and return a tree holding
   the result.  */

static tree
core_field_info (tree src, enum btf_core_reloc_kind kind)
{
  unsigned int result;
  poly_int64 bitsize, bitpos;
  tree var_off = NULL_TREE;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  location_t loc = EXPR_LOCATION (src);
  tree type = TREE_TYPE (src);

  src = root_for_core_field_info (src);

  tree root = get_inner_reference (src, &bitsize, &bitpos, &var_off, &mode,
				   &unsignedp, &reversep, &volatilep);

  /* Note: Use DECL_BIT_FIELD_TYPE rather than DECL_BIT_FIELD here, because it
     remembers whether the field in question was originally declared as a
     bitfield, regardless of how it has been optimized.  */
  bool bitfieldp = (TREE_CODE (src) == COMPONENT_REF
		    && DECL_BIT_FIELD_TYPE (TREE_OPERAND (src, 1)));

  unsigned int align = TYPE_ALIGN (TREE_TYPE (src));
  if (TREE_CODE (src) == COMPONENT_REF)
    {
      tree field = TREE_OPERAND (src, 1);
      if (DECL_BIT_FIELD_TYPE (field))
	align = TYPE_ALIGN (DECL_BIT_FIELD_TYPE (field));
      else
	align = TYPE_ALIGN (TREE_TYPE (field));
    }

  unsigned int start_bitpos = bitpos & ~(align - 1);
  unsigned int end_bitpos = start_bitpos + align;

  switch (kind)
    {
    case BPF_RELO_FIELD_BYTE_OFFSET:
      {
	result = 0;
	if (var_off == NULL_TREE
	    && TREE_CODE (root) == INDIRECT_REF
	    && TREE_CODE (TREE_OPERAND (root, 0)) == POINTER_PLUS_EXPR)
	  {
	    tree node = TREE_OPERAND (root, 0);
	    tree offset = TREE_OPERAND (node, 1);
	    tree type = TREE_TYPE (TREE_OPERAND (node, 0));
	    type = TREE_TYPE (type);

	    gcc_assert (TREE_CODE (offset) == INTEGER_CST && tree_fits_shwi_p (offset)
		&& COMPLETE_TYPE_P (type) && tree_fits_shwi_p (TYPE_SIZE (type)));

	    HOST_WIDE_INT offset_i = tree_to_shwi (offset);
	    result += offset_i;
	  }

	type = unsigned_type_node;
	if (var_off != NULL_TREE)
	  {
	    bpf_error_at (loc, "unsupported variable field offset");
	    return error_mark_node;
	  }

	if (bitfieldp)
	  result += start_bitpos / 8;
	else
	  result += bitpos / 8;
      }
      break;

    case BPF_RELO_FIELD_BYTE_SIZE:
      {
	type = unsigned_type_node;
	if (mode == BLKmode && bitsize == -1)
	  {
	    bpf_error_at (loc, "unsupported variable size field access");
	    return error_mark_node;
	  }

	if (bitfieldp)
	  {
	    /* To match LLVM behavior, byte size of bitfields is recorded as
	       the full size of the base type.  A 3-bit bitfield of type int is
	       therefore recorded as having a byte size of 4 bytes.  */
	    result = end_bitpos - start_bitpos;
	    if (result & (result - 1))
	      {
		bpf_error_at (loc, "unsupported field expression");
		return error_mark_node;
	      }
	    result = result / 8;
	  }
	else
	  result = bitsize / 8;
      }
      break;

    case BPF_RELO_FIELD_EXISTS:
      type = unsigned_type_node;
      /* The field always exists at compile time.  */
      result = 1;
      break;

    case BPF_RELO_FIELD_SIGNED:
      type = unsigned_type_node;
      result = !unsignedp;
      break;

    case BPF_RELO_FIELD_LSHIFT_U64:
    case BPF_RELO_FIELD_RSHIFT_U64:
      {
	type = unsigned_type_node;
	if (mode == BLKmode && bitsize == -1)
	  {
	    bpf_error_at (loc, "unsupported variable size field access");
	    return error_mark_node;
	  }
	if (var_off != NULL_TREE)
	  {
	    bpf_error_at (loc, "unsupported variable field offset");
	    return error_mark_node;
	  }

	if (!bitfieldp)
	  {
	    if (bitsize > 64)
	      {
		bpf_error_at (loc, "field size too large");
		return error_mark_node;
	      }
	    result = 64 - bitsize;
	    break;
	  }

	if (end_bitpos - start_bitpos > 64)
	  {
	    bpf_error_at (loc, "field size too large");
	    return error_mark_node;
	  }

	if (kind == BPF_RELO_FIELD_LSHIFT_U64)
	  {
	    if (TARGET_BIG_ENDIAN)
	      result = bitpos + 64 - start_bitpos - align;
	    else
	      result = start_bitpos + 64 - bitpos - bitsize;
	  }
	else /* RSHIFT_U64 */
	  result = 64 - bitsize;
      }
      break;

    default:
      bpf_error ("invalid second argument to built-in function");
      return error_mark_node;
      break;
    }

  return build_int_cst (type, result);
}

/* Compute the index of the NODE in its immediate container.
   NODE should be a FIELD_DECL (i.e. of struct or union), or an ARRAY_REF.  */

static int
bpf_core_get_index (const tree node, bool *valid)
{
  enum tree_code code = TREE_CODE (node);

  if (code == FIELD_DECL)
    {
      /* Lookup the index from the type fields information.  */
      const tree container = DECL_CONTEXT (node);
      int i = 0;
      for (tree l = TYPE_FIELDS (container); l; l = DECL_CHAIN (l))
	{
	  if (l == node)
	    return i;
	  /* Skip unnamed padding, not represented by BTF.  */
	  if (DECL_NAME(l) != NULL_TREE
	      || TREE_CODE (TREE_TYPE (l)) == UNION_TYPE
	      || TREE_CODE (TREE_TYPE (l)) == RECORD_TYPE)
	    i++;
	}
    }
  else if (code == ARRAY_REF || code == ARRAY_RANGE_REF || code == MEM_REF)
    {
      /* For array accesses, the index is operand 1.  */
      tree index = TREE_OPERAND (node, 1);

      /* If the indexing operand is a constant, extracting is trivial.  */
      if (TREE_CODE (index) == INTEGER_CST && tree_fits_shwi_p (index))
	return tree_to_shwi (index);
    }
  else if (code == POINTER_PLUS_EXPR)
    {
      tree offset = TREE_OPERAND (node, 1);
      tree type = TREE_TYPE (TREE_OPERAND (node, 0));
      type = TREE_TYPE (type);

      if (TREE_CODE (offset) == INTEGER_CST && tree_fits_shwi_p (offset)
	  && COMPLETE_TYPE_P (type) && tree_fits_shwi_p (TYPE_SIZE (type)))
	{
	  HOST_WIDE_INT offset_i = tree_to_shwi (offset);
	  HOST_WIDE_INT type_size_i = tree_to_shwi (TYPE_SIZE_UNIT (type));
	  if ((offset_i % type_size_i) == 0)
	    return offset_i / type_size_i;
	}
    }

  if (valid != NULL)
    *valid = false;
  return -1;
}

#define PREPARE_FAKE_PTR(P) \
  _fake_##P; \
  if (P == NULL) \
    P = &_fake_##P; \
  _fake_##P

#define MAX_NR_ACCESSORS 100

/* This function validates and extracts information for CO-RE field expression.
   Any parametric expression is allowed to be passed in argument NODE.

   If NODE is a valid CO-RE expression VALID boolean pointer would be set to
   true.

   A CO-RE field expression is an expression accessing structures, arrays and
   unions.
   An examples of CO-RE valid expression is:
     A->B[2].UNION_C.D

   This function traverses the tree structure to verify if the expression in
   NODE is valid and extracts other characteristics of the expression, and
   returns it by updating the pointer arguments:

   ACCESSORS: is an array with the indexes of the particular fields
     within the expression.  The indexes are related to actual index on the
     struct/union type, or the array access index.  The RETURN of the function
     is the number of accessors required to represent this expression in CO-RE
     access string.
   VALID - boolean pointer that sets if expression is valid or not for CO-RE.
   ACCESS_NODE - It is the base of the expression.  Using the example below is
     the node that represents the A in the expression.

   ALLOW_ENTRY_CAST is an input arguments and specifies if the function should
   consider as valid expressions in which NODE entry is a cast expression (or
   tree code nop_expr).  */

static unsigned char
compute_field_expr (tree node, unsigned int *accessors,
		    bool *valid,
		    tree *access_node,
		    bool allow_entry_cast = true)
{
  unsigned char n = 0;
  unsigned int fake_accessors[MAX_NR_ACCESSORS];
  if (accessors == NULL)
    accessors = fake_accessors;
  bool PREPARE_FAKE_PTR (valid) = true;
  tree PREPARE_FAKE_PTR (access_node) = NULL_TREE;

  if (node == NULL_TREE)
    {
      *valid = false;
      return 0;
    }

  *access_node = node;

  switch (TREE_CODE (node))
    {
    case INDIRECT_REF:
      if (TREE_CODE (node = TREE_OPERAND (node, 0)) == POINTER_PLUS_EXPR)
	{
	  accessors[0] = bpf_core_get_index (node, valid);
	  *access_node = TREE_OPERAND (node, 0);
	  return 1;
	}
      else
	{
	  accessors[0] = 0;
	  return 1;
	}
    case COMPONENT_REF:
      n = compute_field_expr (TREE_OPERAND (node, 0), accessors,
			      valid,
			      access_node, false);
      accessors[n] = bpf_core_get_index (TREE_OPERAND (node, 1), valid);
      return n + 1;
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case MEM_REF:
      n = compute_field_expr (TREE_OPERAND (node, 0), accessors,
			      valid,
			      access_node, false);
      accessors[n++] = bpf_core_get_index (node, valid);
      return n;
    case NOP_EXPR:
      if (allow_entry_cast == true)
	{
	  *valid = false;
	  return 0;
	}
      n = compute_field_expr (TREE_OPERAND (node, 0), accessors,
			      valid,
			      access_node, false);
      return n;

    case ADDR_EXPR:
    case CALL_EXPR:
    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
      return 0;
    default:
      *valid = false;
      return 0;
    }
}
#undef PREPARE_FAKE_PTR


/* Pack helper for the __builtin_preserve_field_info.  */

static struct cr_local
pack_field_expr (tree *args,
		 enum btf_core_reloc_kind kind,
		 enum bpf_builtins code ATTRIBUTE_UNUSED)
{
  struct cr_local ret = CR_LOCAL_EMPTY;
  ret.fail = false;

  tree arg = args[0];
  tree root = arg;
  tree access_node = NULL_TREE;
  tree type = NULL_TREE;

  if (TREE_CODE (root) == ADDR_EXPR)
    root = TREE_OPERAND (root, 0);

  ret.reloc_decision = REPLACE_CREATE_RELOCATION;

  unsigned int accessors[100];
  bool valid = true;
  compute_field_expr (root, accessors, &valid, &access_node, false);

  type = TREE_TYPE (access_node);
  if (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);

  if (valid == true)
    {
      ret.reloc_data.expr = root;

      /* Note: the type of default_value is used to define the return type of
       __builtin_core_reloc in bpf_resolve_overloaded_core_builtin.  */
      ret.reloc_data.access_node = access_node;
      ret.reloc_data.type = type;
      ret.reloc_data.default_value = core_field_info (root, kind);
      ret.reloc_data.kind = kind;

      if (TREE_CODE (ret.reloc_data.default_value) == ERROR_MARK)
	ret.fail = true;
    }
  else
    {
      bpf_error_at (EXPR_LOC_OR_LOC (arg, UNKNOWN_LOCATION),
		    "argument is not a field access");
      ret.fail = true;
    }

  return ret;
}

/* Process helper for the __builtin_preserve_field_info.  */

static struct cr_final
process_field_expr (struct cr_builtins *data)
{
  gcc_assert (data->kind == BPF_RELO_FIELD_BYTE_OFFSET
	      || data->kind == BPF_RELO_FIELD_BYTE_SIZE
	      || data->kind == BPF_RELO_FIELD_LSHIFT_U64
	      || data->kind == BPF_RELO_FIELD_RSHIFT_U64
	      || data->kind == BPF_RELO_FIELD_SIGNED
	      || data->kind == BPF_RELO_FIELD_EXISTS);

  unsigned int accessors[MAX_NR_ACCESSORS];
  unsigned char nr_accessors = 0;
  tree expr = data->expr;
  tree type = data->type;

  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);

  expr = root_for_core_field_info (expr);
  nr_accessors = compute_field_expr (expr, accessors, NULL, NULL, false);

  struct cr_final ret = { NULL, type, data->kind };

  char str[100];
  if (nr_accessors > 0)
    {
      int n = 0;
      for (int i = 0; i < nr_accessors; i++)
	n += snprintf (str + n, sizeof (str) - n,
		       i == 0 ? "%u" : ":%u", accessors[i]);
      ret.str = CONST_CAST (char *, ggc_strdup (str));
    }
  else
    gcc_unreachable ();

  return ret;
}

static GTY(()) hash_map<tree, tree> *bpf_enum_mappings;
tree enum_value_type = NULL_TREE;

static int
get_index_for_enum_value (tree type, tree expr)
{
  gcc_assert (TREE_CODE (expr) == CONST_DECL
	      && TREE_CODE (type) == ENUMERAL_TYPE);

  unsigned int index = 0;
  for (tree l = TYPE_VALUES (type); l; l = TREE_CHAIN (l))
    {
      gcc_assert (index < (1 << 16));
      if (TREE_VALUE (l) == expr)
	return index;
      index++;
    }
  return -1;
}

/* Pack helper for the __builtin_preserve_enum_value.  */

static struct cr_local
pack_enum_value (tree *args, enum btf_core_reloc_kind kind,
		 enum bpf_builtins code ATTRIBUTE_UNUSED)
{
  struct cr_local ret = CR_LOCAL_EMPTY;
  ret.reloc_decision = REPLACE_CREATE_RELOCATION;
  ret.fail = false;

  tree *result = NULL;
  tree tmp = args[0];
  tree enum_value = args[1];
  tree type = NULL_TREE;

  /* Deconstructing "*(typeof (enum_type) *) enum_value" to collect both the
     enum_type and enum_value.  */
  if (TREE_CODE (tmp) != TARGET_EXPR
      || (type = TREE_TYPE (tmp)) == NULL_TREE
      || (TREE_CODE (type) != POINTER_TYPE)
      || (type = TREE_TYPE (type)) == NULL_TREE
      || (TREE_CODE (type) != ENUMERAL_TYPE))
    {
      bpf_error ("invalid type argument format for enum value builtin");
      ret.fail = true;
    }

  if (TREE_CODE (enum_value) != INTEGER_CST)
    goto pack_enum_value_fail;

  result = bpf_enum_mappings->get (enum_value);
  if (result == NULL)
    goto pack_enum_value_fail;

  tmp = *result;

  if (TREE_CODE (tmp) != CONST_DECL)
    {
pack_enum_value_fail:
      bpf_error ("invalid enum value argument for enum value builtin");
      ret.fail = true;
    }
  else
    {
      ret.reloc_data.expr = tmp;
      if (kind == BPF_RELO_ENUMVAL_VALUE)
	ret.reloc_data.default_value = enum_value;
      else
	ret.reloc_data.default_value = integer_one_node;
    }

  if (ret.fail == false )
    {
      int index = get_index_for_enum_value (type, tmp);
      if (index == -1 || index >= (1 << 16))
	{
	  bpf_error ("enum value in CO-RE builtin cannot be represented");
	  ret.fail = true;
	}
    }

  ret.reloc_data.type = type;
  ret.reloc_data.kind = kind;
  return ret;
}

/* Process helper for the __builtin_preserve_enum_value.  */

static struct cr_final
process_enum_value (struct cr_builtins *data)
{
  gcc_assert (data->kind == BPF_RELO_ENUMVAL_EXISTS
	      || data->kind == BPF_RELO_ENUMVAL_VALUE);

  tree expr = data->expr;
  tree type = data->type;

  struct cr_final ret = { NULL, type, data->kind };

  gcc_assert (TREE_CODE (expr) == CONST_DECL
	      && TREE_CODE (type) == ENUMERAL_TYPE);

  int index = get_index_for_enum_value (type, expr);
  gcc_assert (index != -1 && index < (1 << 16));

  /* Index can only be a value up to 2^16.  Should always fit
     in 6 chars.  */
  char tmp[6];
  sprintf (tmp, "%u", index);
  ret.str = CONST_CAST (char *, ggc_strdup(tmp));

  return ret;
}

/* Pack helper for the __builtin_preserve_type_info.  */

static struct cr_local
pack_type (tree *args, enum btf_core_reloc_kind kind,
	   enum bpf_builtins code ATTRIBUTE_UNUSED)
{
  struct cr_local ret = CR_LOCAL_EMPTY;
  ret.reloc_decision = FAILED_VALIDATION;
  ret.reloc_data.default_value = integer_zero_node;
  ret.fail = false;

  tree root_type = NULL_TREE;
  tree tmp = args[0];
  HOST_WIDE_INT type_size_i;

  if (TYPE_P (tmp))
    goto is_already_type;
  /* Typical structure to match:
	*({ extern typeof (TYPE) *<tmp_name>; <tmp_name>; })  */

  /* Extract Pointer dereference from the construct.  */
  while (tmp != NULL_TREE
	&& (TREE_CODE (tmp) == INDIRECT_REF
	    || TREE_CODE (tmp) == NOP_EXPR))
    tmp = TREE_OPERAND (tmp, 0);

  if (TREE_CODE (tmp) != TARGET_EXPR
      || TREE_CODE (tmp = TREE_OPERAND (tmp, 1)) != BIND_EXPR)
    goto pack_type_fail;

  tmp = BIND_EXPR_VARS (tmp);

  if (TREE_CODE (tmp) != TYPE_DECL
      && TREE_CODE (tmp) != VAR_DECL)
    goto pack_type_fail;

  tmp = TREE_TYPE (tmp);

is_already_type:
  if (TREE_CODE (tmp) == POINTER_TYPE)
    tmp = TREE_TYPE (tmp);

  root_type = tmp;

  if (TREE_CODE (tmp) != RECORD_TYPE
      && TREE_CODE (tmp) != UNION_TYPE
      && TREE_CODE (tmp) != ENUMERAL_TYPE
      && (TREE_CODE (tmp) != POINTER_TYPE
	  || TREE_CODE (TREE_TYPE (tmp)) == FUNCTION_TYPE)
      && (TREE_CODE (tmp) != POINTER_TYPE
	  || TREE_CODE (TREE_TYPE (tmp)) == VOID_TYPE)
      && TREE_CODE (tmp) != ARRAY_TYPE
      && TREE_CODE (tmp) != INTEGER_TYPE)
    goto pack_type_fail;

  ret.reloc_data.type = root_type;
  ret.reloc_decision = REPLACE_CREATE_RELOCATION;

  /* Force this type to be marked as used in dwarf2out.  */
  gcc_assert (cfun);
  if (cfun->used_types_hash == NULL)
    cfun->used_types_hash = hash_set<tree>::create_ggc (37);
  cfun->used_types_hash->add (root_type);

  type_size_i = tree_to_shwi (TYPE_SIZE_UNIT (ret.reloc_data.type));

  switch (kind)
    {
      case BPF_RELO_TYPE_SIZE:
	ret.reloc_data.default_value = build_int_cst (integer_type_node,
						      type_size_i);
	break;
      case BPF_RELO_TYPE_EXISTS:
      case BPF_RELO_TYPE_MATCHES:
	ret.reloc_data.default_value = integer_one_node;
	break;
      case BPF_RELO_TYPE_ID_LOCAL:
      case BPF_RELO_TYPE_ID_TARGET:
	ret.reloc_data.default_value = integer_zero_node;
	break;
      default:
	break;
    }

  ret.reloc_data.kind = kind;
  return ret;

pack_type_fail:
      bpf_error_at (EXPR_LOC_OR_LOC (args[0], UNKNOWN_LOCATION),
		    "invalid first argument format for enum value builtin");
      ret.fail = true;
  return ret;
}

/* Process helper for the __builtin_preserve_type_info.  */

static struct cr_final
process_type (struct cr_builtins *data)
{
  gcc_assert (data->kind == BPF_RELO_TYPE_ID_LOCAL
	      || data->kind == BPF_RELO_TYPE_ID_TARGET
	      || data->kind == BPF_RELO_TYPE_EXISTS
	      || data->kind == BPF_RELO_TYPE_SIZE
	      || data->kind == BPF_RELO_TYPE_MATCHES);

  struct cr_final ret;
  ret.str = ggc_strdup ("0");
  ret.type = data->type;
  ret.kind = data->kind;

  if ((data->kind == BPF_RELO_TYPE_ID_LOCAL
      || data->kind == BPF_RELO_TYPE_ID_TARGET)
      && data->default_value != NULL)
  {
    ctf_container_ref ctfc = ctf_get_tu_ctfc ();
    unsigned int btf_id = get_btf_id (ctf_lookup_tree_type (ctfc, ret.type));
    data->rtx_default_value = expand_normal (build_int_cst (integer_type_node,
							    btf_id));
  }

  return ret;
}

static bool
bpf_require_core_support ()
{
  if (!TARGET_BPF_CORE)
    {
      bpf_error ("BPF CO-RE is required but not enabled");
      return false;
    }
  return true;
}

/* BPF Compile Once - Run Everywhere (CO-RE) support.  Construct a CO-RE
   relocation record in DATA to be emitted in the .BTF.ext
   section.  Does nothing if we are not targetting BPF CO-RE, or if the
   constructed relocation would be a no-op.  */

static void
make_core_relo (struct cr_final *data, rtx_code_label *label)
{
  /* If we are not targetting BPF CO-RE, do not make a relocation.  We
     might not be generating any debug info at all.  */
  if (!bpf_require_core_support ())
    return;

  gcc_assert (data->type);

  /* Determine what output section this relocation will apply to.
     If this function is associated with a section, use that.  Otherwise,
     fall back on '.text'.  */
  const char * section_name;
  if (current_function_decl && DECL_SECTION_NAME (current_function_decl))
    section_name = DECL_SECTION_NAME (current_function_decl);
  else
    section_name = ".text";

  /* Add the CO-RE relocation information to the BTF container.  */
  bpf_core_reloc_add (data->type, section_name, data->str, label,
		      data->kind);
}

/* Support function to extract kind information for CO-RE builtin
   calls.  */

static inline char
read_kind (tree kind, char max_value, char enum_offset)
{
  char kind_val = 0;

  if (kind == NULL_TREE)
    goto invalid_kind_arg_error;

  if (TREE_CODE (kind) != CONST_DECL
      && TREE_CODE (kind) == NOP_EXPR)
    kind = TREE_OPERAND (kind, 0);

  if (TREE_CODE (kind) == CONST_DECL)
    kind = DECL_INITIAL (kind);

  if (TREE_CODE (kind) == INTEGER_CST
      && tree_fits_uhwi_p (kind))
    kind_val = tree_to_uhwi (kind);
  else
    goto invalid_kind_arg_error;

  if (kind_val > max_value)
    {
invalid_kind_arg_error:
      bpf_error ("invalid kind argument to core builtin");
      return -1;
    }
  return kind_val + enum_offset;
}

#define KIND_EXPECT_NARGS(N, MSG) \
  { if (nargs != N) { bpf_error (MSG); return BPF_RELO_INVALID; } }

/* Helper functions to extract kind information.  */
static inline enum btf_core_reloc_kind
kind_access_index (tree *args ATTRIBUTE_UNUSED, int nargs)
{
  KIND_EXPECT_NARGS (1,
	"wrong number of arguments for access index core builtin");
  return BPF_RELO_FIELD_BYTE_OFFSET;
}
static inline enum btf_core_reloc_kind
kind_preserve_field_info (tree *args, int nargs)
{
  KIND_EXPECT_NARGS (2,
	"wrong number of arguments for field info core builtin");
  return (enum btf_core_reloc_kind) read_kind (args[1], 5,
					       BPF_RELO_FIELD_BYTE_OFFSET);
}
static inline enum btf_core_reloc_kind
kind_enum_value (tree *args, int nargs)
{
  KIND_EXPECT_NARGS (3,
	"wrong number of arguments for enum value core builtin");
  return (enum btf_core_reloc_kind) read_kind (args[2], 1,
					       BPF_RELO_ENUMVAL_EXISTS);
}
static inline enum btf_core_reloc_kind
kind_type_id (tree *args, int nargs)
{
  KIND_EXPECT_NARGS (2,
	"wrong number of arguments for type id core builtin");
  return (enum btf_core_reloc_kind) read_kind (args[1], 1,
					       BPF_RELO_TYPE_ID_LOCAL);
}
static inline enum btf_core_reloc_kind
kind_preserve_type_info (tree *args, int nargs)
{
  KIND_EXPECT_NARGS (2,
	"wrong number of arguments for type info core builtin");
  char val = read_kind (args[1], 2, 0);
  switch (val)
    {
    case 0:
      return BPF_RELO_TYPE_EXISTS;
    case 1:
      return BPF_RELO_TYPE_SIZE;
    case 2:
      return BPF_RELO_TYPE_MATCHES;
    default:
      break;
    }
  return BPF_RELO_INVALID;
}

/* Plugin handler used in the parser that allows to collect enum value
   information that other wise would be folded and non recoverable.  */

void
bpf_handle_plugin_finish_type (void *event_data,
			       void *data ATTRIBUTE_UNUSED)
{
  tree type = (tree) event_data;

  if (bpf_enum_mappings == NULL)
    bpf_enum_mappings = hash_map<tree, tree>::create_ggc (10);

  if (TREE_CODE (type) == ENUMERAL_TYPE)
    for (tree l = TYPE_VALUES (type); l; l = TREE_CHAIN (l))
      {
	tree value = TREE_VALUE (l);

	tree initial = DECL_INITIAL (value);
	initial = copy_node (initial);
	DECL_INITIAL (value) = initial;

	tree *found = bpf_enum_mappings->get (initial);
	if (found == NULL)
	  bpf_enum_mappings->put (initial, value);
      }
}

/* Initializes support information to process CO-RE builtins.
   Defines information for the builtin processing, such as helper functions to
   support the builtin convertion.  */

void
bpf_init_core_builtins (void)
{
  memset (core_builtin_helpers, 0, sizeof (core_builtin_helpers));

  core_builtin_helpers[BPF_BUILTIN_PRESERVE_ACCESS_INDEX] =
    BPF_CORE_HELPER_SET (kind_access_index,
			 NULL,
			 NULL,
			 NULL,
			 true);
  core_builtin_helpers[BPF_BUILTIN_PRESERVE_FIELD_INFO] =
    BPF_CORE_HELPER_SET (kind_preserve_field_info,
			 NULL,
			 pack_field_expr,
			 process_field_expr,
			 true);
  core_builtin_helpers[BPF_BUILTIN_BTF_TYPE_ID] =
    BPF_CORE_HELPER_SET (kind_type_id,
			 compare_same_ptr_type,
			 pack_type,
			 process_type,
			 true);

  core_builtin_helpers[BPF_BUILTIN_PRESERVE_TYPE_INFO] =
    BPF_CORE_HELPER_SET (kind_preserve_type_info,
			 compare_same_ptr_type,
			 pack_type,
			 process_type,
			 true);

  core_builtin_helpers[BPF_BUILTIN_PRESERVE_ENUM_VALUE] =
    BPF_CORE_HELPER_SET (kind_enum_value,
			 compare_same_ptr_expr,
			 pack_enum_value,
			 process_enum_value,
			 true);

  core_builtin_helpers[BPF_BUILTIN_CORE_RELOC] =
    BPF_CORE_HELPER_SET (NULL, NULL, NULL, NULL, true);

  /* Initialize plugin handler to record enums value for use in
     __builtin_preserve_enum_value.  */
  plugin_state = (enum bpf_plugin_states) flag_plugin_added;
  flag_plugin_added = true;
  register_callback ("bpf_collect_enum_info", PLUGIN_FINISH_TYPE,
		     bpf_handle_plugin_finish_type, NULL);
}

/* This function returns the related __builtin_core_reloc call tree node to a
   particular CO-RE builtin definition in FNDECL when called with
   arguments ARGS.  */

static tree
construct_builtin_core_reloc (location_t loc, tree fndecl, tree *args,
			      int nargs)
{
  int code = DECL_MD_FUNCTION_CODE (fndecl);
  builtin_helpers helper = core_builtin_helpers[code];

  if (helper.is_valid)
    {
      gcc_assert (helper.kind);
      gcc_assert (helper.pack);
      gcc_assert (helper.process);

      struct cr_local local_data = CR_LOCAL_EMPTY;
      local_data.fail = false;

      enum btf_core_reloc_kind kind = helper.kind (args, nargs);
      if (kind == BPF_RELO_INVALID)
	local_data.fail = true;
      else if (helper.pack != NULL)
	{
	  local_data = helper.pack (args, kind, (enum bpf_builtins) code);
	  local_data.reloc_data.orig_builtin_code = (enum bpf_builtins) code;
	  local_data.reloc_data.orig_arg_expr = args[0];
	}
      else
	gcc_unreachable ();

      if (local_data.fail == true)
	return error_mark_node;

      if (local_data.reloc_decision == REPLACE_CREATE_RELOCATION)
	{
	  int index = search_builtin_data (helper.compare,
					   &local_data.reloc_data);
	  if (index == -1)
	    index = allocate_builtin_data ();
	  struct cr_builtins *data = get_builtin_data (index);
	  memcpy (data, &local_data.reloc_data, sizeof (struct cr_builtins));

	  tree fndecl = bpf_builtins[BPF_BUILTIN_CORE_RELOC];
	  return  build_call_expr_loc (loc,
				fndecl, 1,
				build_int_cst (integer_type_node, index));
	}
    }
  return NULL_TREE;
}

/* This function constructs the CO-RE safe code around field expressions.
   If the expression is an array access, create an offset by multiplying the
   index access by the __builtin_type_info call requesting the size of the
   array element and multiplying by the index.  The offset is added to the
   base pointer.
   In a more formal way:
     - base + (__blt_preserve_type_info (typeof(expr), SIZEOF) * array_i)

   Please notice that __builtin_preserve_type_info is never really created, but
   rather a call to __builtin_core_reloc that represents it.

   Any other case, it is assumed to be a field access and instead a CO-RE field
   expressions offset relocation is created and added to the base node.
   More precisely:
     - base + __builtin_preserve_field_expr (expr, SIZEOF)  */

static tree
core_expr_with_field_expr_plus_base (tree base, tree expr, bool leaf_node)
{
  tree type = TREE_TYPE (expr);
  tree args[2];

  if (base == expr)
    return expr;
  else if (TREE_CODE (expr) == ARRAY_REF
	   && leaf_node == false)
    {
      if (TREE_CODE (base) == MEM_REF)
	base = TREE_OPERAND (base, 0);

      tree array_index = TREE_OPERAND (expr, 1);
      tree fndecl = bpf_builtins[BPF_BUILTIN_PRESERVE_TYPE_INFO];

      tree type = TREE_TYPE (base);
      gcc_assert (POINTER_TYPE_P (type)
		  && TREE_CODE (type = TREE_TYPE (type)) == ARRAY_TYPE
		  && (type = TREE_TYPE (type)) != NULL_TREE);
      args[0] = type;
      args[1] = build_int_cst (integer_type_node, BPF_TYPE_SIZE);
      tree builtin_call = construct_builtin_core_reloc (UNKNOWN_LOCATION,
							fndecl,
							args, 2);

      tree offset = fold_build2 (MULT_EXPR, size_type_node,
			fold_build1 (NOP_EXPR, size_type_node, builtin_call),
			fold_build1 (NOP_EXPR, size_type_node, array_index));

      if (!POINTER_TYPE_P (TREE_TYPE (base)))
	base = fold_build1 (ADDR_EXPR,
			    build_pointer_type (TREE_TYPE (base)), base);

      tree tmp = fold_build2 (POINTER_PLUS_EXPR, ptr_type_node,
			 fold_build1 (NOP_EXPR, ptr_type_node, base),
			 offset);

      tmp = fold_build1 (NOP_EXPR, build_pointer_type (type), tmp);
      return tmp;
    }
  else
    {
      tree fndecl = bpf_builtins[BPF_BUILTIN_PRESERVE_FIELD_INFO];
      args[0] = expr;
      args[1] = build_int_cst (integer_type_node, BPF_FIELD_BYTE_OFFSET);
      tree builtin_call = construct_builtin_core_reloc (UNKNOWN_LOCATION,
							fndecl,
							args, 2);

      if (!POINTER_TYPE_P (TREE_TYPE (base)))
	base = fold_build1 (ADDR_EXPR,
			    build_pointer_type (TREE_TYPE (base)), base);

      tree tmp = fold_build2 (POINTER_PLUS_EXPR, ptr_type_node,
			 fold_build1 (NOP_EXPR, ptr_type_node, base),
			 fold_build1 (NOP_EXPR, size_type_node, builtin_call));
      tmp = fold_build1 (NOP_EXPR, build_pointer_type (type), tmp);
      return tmp;
    }
}

/* This function takes arbitrary field expression and returns a CO-RE
   compatible version by introducing CO-RE relocations.

   In cases where the expression is not supported in CO-RE with a single
   relocation, it creates multiple levels of access, i.e. if the expression
   contains multiple indirections.
   For example:
     - A->B->C

   It recursively traverses the expression to its leaf nodes and rollbacks
   constructing the CO-RE relocations.  It calls
   core_expr_with_field_expr_plus_base that creates the necessary CO-RE
   relocations.

   Arguments:
    - EXPR: is the expression to be converted.  It should be validated by
    compute_field_expr function before this function is called.
    - CHANGED: is set to true if the returned tree node is different from
    the input expr argument.
    - ENTRY: internal only.  Should not be set in a call.  */

static tree
make_core_safe_access_index (tree expr, bool *changed, bool entry = true)
{
  poly_int64 bitsize, bitpos;
  tree var_off;
  machine_mode mode;
  int sign, reverse, vol;

  tree base = get_inner_reference (expr, &bitsize, &bitpos, &var_off, &mode,
				   &sign, &reverse, &vol);

  if (base == NULL_TREE || base == expr)
    return expr;

  base = expr;

  tree ret = NULL_TREE;
  int n;
  bool valid = true;
  tree access_node = NULL_TREE;

  /* In case the base is itself a valid field expression, first convert the
     base in a CO-RE safe expression.
     This seems to be a requirement since get_inner_reference not always
     returns the true base of the expression.  */
  if ((n = compute_field_expr (base, NULL, &valid, &access_node)) > 0
      && valid == true)
    {
      if (TREE_CODE (access_node) == INDIRECT_REF)
	base = TREE_OPERAND (access_node, 0);
      else
	base = access_node;

      bool local_changed = false;
      ret = make_core_safe_access_index (base, &local_changed, false);
      if (local_changed == true)
	{
	  if (TREE_CODE (access_node) == INDIRECT_REF)
	    base = fold_build1 (INDIRECT_REF,
				TREE_TYPE (base),
				ret);
	  else
	    base = ret;
	}
    }

  /* The remaining is to traverse the field part of the field expression.  */
  if (mode != VOIDmode && var_off == NULL_TREE)
    {
      *changed = true;
      return core_expr_with_field_expr_plus_base (base, expr, true);
    }
  else
    {
      switch (TREE_CODE (expr))
	{
	case COMPONENT_REF:
	case ARRAY_REF:
	case ADDR_EXPR:
	  {
	    bool local_changed = false;
	    tree type = TREE_TYPE (TREE_OPERAND (expr, 0));
	    ret = make_core_safe_access_index (TREE_OPERAND (expr, 0),
					       &local_changed, false);

	    /* This variable is a replaced in the expr for algorithmic purposes.
	       It reduces the expression just to the remaining sub-expression
	       that still was not processed.  */
	    if (local_changed == true)
	      TREE_OPERAND (expr, 0) = create_tmp_var (type, "fake");
	  }
	  break;
	default:
	  ret = expr;
	  break;
	}
    }

  base = get_inner_reference (expr, &bitsize, &bitpos, &var_off, &mode,
			      &sign, &reverse, &vol);

  if ((ret != NULL_TREE
       && mode != VOIDmode && var_off != NULL_TREE)
      || entry == true)
    {
      *changed = true;
      ret = core_expr_with_field_expr_plus_base (ret, expr, false);
    }
  return ret;
}

/* This function verifies if the NODE expression is a field expression and
   changes and converts it to CO-RE.  This is used by a tree walker for any
   __builtin_preserve_access_index argument expression from within
   bpf_resolve_overloaded_core_builtin.  */

static tree
replace_core_access_index_comp_expr (tree *node, int *walk_subtrees,
				     void *data ATTRIBUTE_UNUSED)
{
  bool valid = true;
  gcc_assert (*node != NULL_TREE);

  tree *expr = node;
  bool should_indirect = false;
  if (TREE_CODE (*expr) == ADDR_EXPR)
    expr = &TREE_OPERAND (*expr, 0);
  else
    should_indirect = true;

  int n = compute_field_expr (*node, NULL, &valid, NULL);
  if (valid == true && n > 0)
    {
      bool changed = false;
      tree expr_test = make_core_safe_access_index (*expr, &changed);
      *walk_subtrees = 0;

      if (changed == true)
	{
	  if (should_indirect == true)
	    expr_test = fold_build1 (INDIRECT_REF,
				     TREE_TYPE (TREE_TYPE (expr_test)),
				     expr_test);

	  *expr = expr_test;
	}
    }
  return NULL_TREE;
}

/* This function is used by bpf_resolve_overloaded_builtin defined in bpf.cc.
   It is executed in a very early stage and processes any CO-RE builtins,
   adapting the code and creating the more generic __builtin_core_reloc calls.
   */

#define MAX_CORE_BUILTIN_ARGS 3
tree
bpf_resolve_overloaded_core_builtin (location_t loc, tree fndecl,
				     void *arglist)
{
  if (!bpf_require_core_support ())
    return error_mark_node;

  vec<tree, va_gc> *argsvec = static_cast<vec<tree, va_gc> *> (arglist);
  tree args[MAX_CORE_BUILTIN_ARGS];
  for (unsigned int i = 0; i < argsvec->length (); i++)
    args[i] = (*argsvec)[i];

  int code = DECL_MD_FUNCTION_CODE (fndecl);
  if (code == BPF_BUILTIN_PRESERVE_ACCESS_INDEX)
  {
    walk_tree (&args[0], replace_core_access_index_comp_expr, NULL, NULL);
    return args[0];
  }

  return construct_builtin_core_reloc (loc, fndecl, args, argsvec->length ());
}

/* Used in bpf_expand_builtin.  This function is called in RTL expand stage to
   convert the internal __builtin_core_reloc in unspec:UNSPEC_CORE_RELOC RTL,
   which will contain a third argument that is the index in the vec collected
   in bpf_resolve_overloaded_core_builtin.  */

rtx
bpf_expand_core_builtin (tree exp, enum bpf_builtins code)
{
  if (!TARGET_BPF_CORE)
    return NULL_RTX;

  switch (code)
    {
    case BPF_BUILTIN_CORE_RELOC:
      {
	tree index = CALL_EXPR_ARG (exp, 0);
	struct cr_builtins *data = get_builtin_data (TREE_INT_CST_LOW (index));

	rtx v = expand_normal (data->default_value);
	rtx i = expand_normal (index);
	  return gen_rtx_UNSPEC (DImode,
				 gen_rtvec (2, v, i),
				 UNSPEC_CORE_RELOC);
      }
      break;
    default:
      break;
    }

  return NULL_RTX;
}


/* This function is called in the final assembly output for the
   unspec:UNSPEC_CORE_RELOC.  It recovers the vec index kept as the third
   operand and collects the data from the vec.  With that it calls the process
   helper in order to construct the data required for the CO-RE relocation.
   Also it creates a label pointing to the unspec instruction and uses it in
   the CO-RE relocation creation.  */

void
bpf_output_core_reloc (rtx *operands, int nr_ops)
{
  /* Search for an UNSPEC_CORE_RELOC within the operands of the emitting
     intructions.  */
  rtx unspec_exp = NULL_RTX;
  for (int i = 0; i < nr_ops; i++)
    {
      rtx op = operands[i];

      /* An immediate CO-RE reloc.  */
      if (GET_CODE (op) == UNSPEC
	  && XINT (op, 1) == UNSPEC_CORE_RELOC)
	unspec_exp = op;

      /* In case of a MEM operation with an offset resolved in CO-RE.  */
      if (GET_CODE (op) == MEM
	  && (op = XEXP (op, 0)) != NULL_RTX
	  && (GET_CODE (op) == PLUS))
	{
	  rtx x0 = XEXP (op, 0);
	  rtx x1 = XEXP (op, 1);

	  if (GET_CODE (x0) == UNSPEC
	      && XINT (x0, 1) == UNSPEC_CORE_RELOC)
	    unspec_exp = x0;
	  if (GET_CODE (x1) == UNSPEC
	      && XINT (x1, 1) == UNSPEC_CORE_RELOC)
	    unspec_exp = x1;
	}
      if (unspec_exp != NULL_RTX)
	break;
    }

  if (unspec_exp != NULL_RTX)
    {
      int index = INTVAL (XVECEXP (unspec_exp, 0, 1));
      struct cr_builtins *data = get_builtin_data (index);
      builtin_helpers helper;
      helper = core_builtin_helpers[data->orig_builtin_code];

      rtx_code_label * tmp_label = gen_label_rtx ();
      output_asm_label (tmp_label);
      assemble_name (asm_out_file, ":\n");

      rtx orig_default_value = data->rtx_default_value;

      gcc_assert (helper.process != NULL);
      struct cr_final reloc_data = helper.process (data);
      make_core_relo (&reloc_data, tmp_label);

      /* Replace default value for later processing builtin types.
	 An example are the type id builtins.  */
      if (data->rtx_default_value != NULL_RTX
	  && orig_default_value != data->rtx_default_value)
	XVECEXP (unspec_exp, 0, 0) = data->rtx_default_value;
    }
}

static tree
maybe_get_base_for_field_expr (tree expr)
{
  poly_int64 bitsize, bitpos;
  tree var_off;
  machine_mode mode;
  int sign, reverse, vol;

  if (expr == NULL_TREE)
    return NULL_TREE;

  return get_inner_reference (expr, &bitsize, &bitpos, &var_off, &mode,
			      &sign, &reverse, &vol);
}

/* Access functions to mark sub expressions as attributed with
   __preserve_access_index.
   This is required since in gimple format, in order to convert an expression as
   CO-RE safe, we must create multiple gimple statements.
   Also, only the type of the base of the expression might be attributed with
   __preserve_access_index.  Nevertheless all the consecutive accesses to this
   attributed node should also be converted to CO-RE safe.
   Any LHS assigned values with CO-RE converted expressions are marked and
   any uses of these values are later checked for further convertion.
   The core_access_index_map functions allow to mark this nodes for later
   convertion to CO-RE.
   This mechanism are used by make_gimple_core_safe_access_index.  */

static GTY(()) hash_map<tree, tree> *core_access_index_map = NULL;

static void
core_access_clean (void)
{
  if (core_access_index_map == NULL)
    core_access_index_map = hash_map<tree, tree>::create_ggc (10);
  core_access_index_map->empty ();
}

static bool
core_is_access_index (tree expr)
{
  if (TREE_CODE (expr) == MEM_REF
      || TREE_CODE (expr) == INDIRECT_REF)
    expr = TREE_OPERAND (expr, 0);

  tree *def = core_access_index_map->get (expr);
  if (def)
    return true;
  return false;
}

static void
core_mark_as_access_index (tree expr)
{
  if (TREE_CODE (expr) == MEM_REF
      || TREE_CODE (expr) == INDIRECT_REF)
    expr = TREE_OPERAND (expr, 0);

  if (core_access_index_map->get (expr) == NULL)
    core_access_index_map->put (expr, NULL_TREE);
}

/* This function is an adaptation of make_core_safe_access_index but to be used
   in gimple format trees.  It is used by execute_lower_bpf_core, when
   traversing the gimple tree looking for nodes that would have its type
   attributed with __preserve_access_index.  In this particular cases any of
   the expressions using such attributed types must be made CO-RE safe.  */

static tree
make_gimple_core_safe_access_index (tree *tp,
				    int *walk_subtrees ATTRIBUTE_UNUSED,
				    void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  bool valid = true;
  int n = 0;

  tree *patch = tp;
  if (TREE_CODE (*patch) == ADDR_EXPR)
    patch = &(TREE_OPERAND (*tp, 0));
  tree orig_type = TREE_TYPE (*patch);

  if ((is_attr_preserve_access (*patch)
      || core_is_access_index (maybe_get_base_for_field_expr (*patch)))
      && (n = compute_field_expr (*patch, NULL, &valid, NULL)) > 0
      && valid == true)
    {
      bool changed = false;
      tree expr_test = make_core_safe_access_index (*patch, &changed);


      gimple_seq before = NULL;
      push_gimplify_context ();
      gimplify_expr (&expr_test, &before, NULL, is_gimple_val, fb_rvalue);

      /* In case the ADDR_EXPR bypassed above is no longer needed.  */
      if (patch != tp && TREE_TYPE (expr_test) == TREE_TYPE (*tp))
	*tp = expr_test;
      /* For non pointer value accesses.  */
      else if (TREE_TYPE (expr_test) == build_pointer_type (orig_type))
	*patch = fold_build2 (MEM_REF, TREE_TYPE (*patch),
			      expr_test, build_int_cst (ptr_type_node, 0));
      else
	*patch = expr_test;

      *tp = fold (*tp);

      gsi_insert_seq_before (&(wi->gsi), before, GSI_LAST_NEW_STMT);
      pop_gimplify_context (NULL);

      wi->changed = true;
      *walk_subtrees = false;

      tree lhs;
      if (!wi->is_lhs
	  && (lhs = gimple_get_lhs (wi->stmt)) != NULL_TREE)
	core_mark_as_access_index (lhs);
    }
  return NULL_TREE;
}

/* This is the entry point for the pass_data_lower_bpg_core.  It walks all the
   statements in gimple, looking for expressions that are suppose to be CO-RE
   preserve_access_index attributed.
   Those expressions are processed and split by
   make_gimple_core_safe_access_index function, which will both create the
   calls to __build_core_reloc and split the expression in smaller parts in
   case it cannot be represented CO-RE safeguarded by a single CO-RE
   relocation.  */

static unsigned int
execute_lower_bpf_core (void)
{
  remove_parser_plugin ();
  if (!TARGET_BPF_CORE)
    return 0;

  gimple_seq body = gimple_body (current_function_decl);

  struct walk_stmt_info wi;
  core_access_clean ();

  memset (&wi, 0, sizeof (wi));
  wi.info = NULL;

  /* Split preserve_access_index expressions when needed.  */
  walk_gimple_seq_mod (&body, NULL, make_gimple_core_safe_access_index, &wi);
  return 0;
}

namespace {

const pass_data pass_data_lower_bpf_core =
{
  GIMPLE_PASS, /* type */
  "bpf_core_lower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_bpf_core: public gimple_opt_pass
{
public:
  pass_lower_bpf_core (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_bpf_core, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute (function *) final override
  {
    return execute_lower_bpf_core ();
  }

}; /* class pass_lower_bpf_core */

} /* anon namespace */

gimple_opt_pass *
make_pass_lower_bpf_core (gcc::context *ctxt)
{
  return new pass_lower_bpf_core (ctxt);
}

#include "gt-core-builtins.h"
