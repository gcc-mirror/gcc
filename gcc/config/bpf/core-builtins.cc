/* Subroutines used for code generation for eBPF.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.

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
#include "tree-pass.h"
#include "plugin.h"

#include "ctfc.h"
#include "btf.h"
#include "coreout.h"
#include "core-builtins.h"

/*
 * BPF CO-RE builtins definition.

   The expansion of CO-RE builtins occur in three steps:
   1. - bpf_resolve_overloaded_core_builtin (pack step)
     Right after the front-end, all of the CO-RE builtins are converted to an
     internal builtin __builtin_core_reloc, which takes a single argument and
     has polymorphic return value to fit the particular expected return type
     from the original builtin.  The first argument contains an index argument
     which points to the information stored in a vec<struct cr_builtins>
     which collects the required information from the original CO-RE builtin in
     order to use it later on in the __builtin_core_reloc expansion (the next
     step).

   2. - bpf_expand_core_builtin
     In this step, the __builtin_core_reloc is expanded to a unspec:UNSPEC_CORE_RELOC
     with 3 operands, destination, source and the index. The index operand
     is the index in the vec constructed in the previous step.

   3. - final asm output (process step)
     This is the output of the unspec:UNSPEC_CORE_RELOC. The index passed in
     the third operand is read and extracted as a integer from the rtx node.
     The data is collected from the vec and it is used to create
     the proper CO-RE relocation as well as do the final assembly output.
     It also creates a label to mark the location of the move instruction that
     is used in the CO-RE relocation.

  The initialization of the CO-RE builtins infrastructure occurs in
  bpf_is function.  It creates a struct
  builtin_helpers_t arrays which defines the kind argument position,
  the callback helpers, kind, compare, pack and process, for each individual
  type of builtin argument possible in the original CO-RE builtins.

  More precisely, field expression, type and enum value, used in the following
  relocations:
    - __builtin_core_preserve_access_index (<field_expr>)
    - __builtin_core_field_info (<field_expr>, <kind>)
    - __builtin_core_type_id (<type>, <kind>)
    - __builtin_core_type_info (<type>, <kind>)
    - __builtin_core_enum_value (<enum_value>, <kind>)

  The kind helper allows to identify the proper relocation for the builtin
  call based on the value within the kind argument.

  The compare helper is used to identify if a new builtin call has similar
  arguments to any other builtin call with the compiling unit.
  This enables the possibility to optimize consecutive similar calls of the
  builtins.

  The pack helper callbacks are suppose to decode the original CO-RE builtin
  call arguments, verify that it is a valid tree node for the particular
  builtin, allocate a struct cr_local in vector and write it with the
  relevant data for the particular builtin type.

  The process helper should take the data constructed in the pack helper and
  create a struct cr_final element which contains the essential
  information to create a CO-RE relocation.
  This information is further used by the final assembly output step to define
  the CO-RE relocation and pass-through the default value for the original
  CO-RE builtin.


  BPF CO-RE preserve access is supported in two forms:
  - A target builtin, __builtin_preserve_access_index

    This builtin accepts a single argument.  Any access to an aggregate data
    structure (struct, union or array) within the argument will be recorded by
    the CO-RE machinery, resulting in a relocation record being placed in the
    .BTF.ext section of the output.

    It is implemented in bpf_resolve_overloaded_builtin () and
    bpf_expand_builtin (), using the supporting routines below.

  - An attribute, __attribute__((preserve_access_index))

    This attribute can be applied to struct and union types.  Any access to a
    type with this attribute will be recorded by the CO-RE machinery.
    In the expand, any move matching is checked if any of its operands is
    an expression to an attributed type, and if so, the expand will emit a
    unspec:UNSPEC_CORE_RELOC that later on, in final assembly output, will
    create the CO-RE relocation, just like it would happen if it was defined
    as a builtin.  */


struct GTY(()) cr_builtins
{
  tree type;
  tree expr;
  tree default_value;
  rtx rtx_default_value;
  enum btf_core_reloc_kind kind;
  enum bpf_builtins orig_builtin_code;
  tree orig_arg_expr;
};
typedef struct cr_builtins *cr_builtins_ref;

#define CORE_BUILTINS_DATA_EMPTY \
  { NULL_TREE, NULL_TREE, NULL_TREE, NULL_RTX, BPF_RELO_INVALID, \
    BPF_BUILTIN_UNUSED, NULL }

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
  KEEP_ORIGINAL_NO_RELOCATION,
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
  char *str;
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

   This attribute marks a structure/union/array type as "preseve", so that
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

   This is to follow LLVM behavior. */

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

  if (TREE_CODE (base) == MEM_REF)
    {
      return lookup_attribute ("preserve_access_index",
			       TYPE_ATTRIBUTES (TREE_TYPE (base)));
    }

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
			       TYPE_ATTRIBUTES (container));
    }

  else if (TREE_CODE (t) == ADDR_EXPR)
    return is_attr_preserve_access (TREE_OPERAND (t, 0));

  return false;
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

  get_inner_reference (src, &bitsize, &bitpos, &var_off, &mode, &unsignedp,
		       &reversep, &volatilep);

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
	type = unsigned_type_node;
	if (var_off != NULL_TREE)
	  {
	    bpf_error_at (loc, "unsupported variable field offset");
	    return error_mark_node;
	  }

	if (bitfieldp)
	  result = start_bitpos / 8;
	else
	  result = bitpos / 8;
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
bpf_core_get_index (const tree node)
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

      if (TREE_CODE (offset) == INTEGER_CST && tree_fits_shwi_p (offset)
	  && COMPLETE_TYPE_P (type) && tree_fits_shwi_p (TYPE_SIZE (type)))
	{
	  HOST_WIDE_INT offset_i = tree_to_shwi (offset);
	  HOST_WIDE_INT type_size_i = tree_to_shwi (TYPE_SIZE_UNIT (type));
	  if ((offset_i % type_size_i) == 0)
	    return offset_i / type_size_i;
	}
    }

  gcc_unreachable ();
  return -1;
}

/* This function takes a possible field expression (node) and verifies it is
   valid, extracts what should be the root of the valid field expression and
   composes the accessors array of indices.  The accessors are later used in the
   CO-RE relocation in the string field.  */

static unsigned char
compute_field_expr (tree node, unsigned int *accessors, bool *valid,
		    tree *root)
{
  unsigned char n = 0;
  if (node == NULL_TREE)
    {
      *valid = false;
      return 0;
    }

  switch (TREE_CODE (node))
    {
    case INDIRECT_REF:
    case ADDR_EXPR:
      accessors[0] = 0;
      n = compute_field_expr (TREE_OPERAND (node, 0), &accessors[0], valid,
			      root);
      *root = node;
      return n + 1;
    case POINTER_PLUS_EXPR:
      accessors[0] = bpf_core_get_index (node);
      *root = node;
      return 1;
    case COMPONENT_REF:
      n = compute_field_expr (TREE_OPERAND (node, 0), accessors, valid,
			      root);
      accessors[n] = bpf_core_get_index (TREE_OPERAND (node, 1));
      *root = node;
      return n + 1;
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case MEM_REF:
      n = compute_field_expr (TREE_OPERAND (node, 0), accessors, valid, root);
      accessors[n] = bpf_core_get_index (node);
      *root = node;
      return n + 1;
    case NOP_EXPR:
      n = compute_field_expr (TREE_OPERAND (node, 0), accessors, valid, root);
      *root = node;
      return n;
    case TARGET_EXPR:
      {
	tree value = TREE_OPERAND (node, 1);
	if (TREE_CODE (value) == BIND_EXPR
	    && TREE_CODE (value = BIND_EXPR_BODY (value)) == MODIFY_EXPR)
	  return compute_field_expr (TREE_OPERAND (value, 1), accessors, valid,
				     root);
      }
      *root = node;
      return 0;
    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
      return 0;
    default:
      *valid = false;
      return 0;
    }
}

static struct cr_local
pack_field_expr_for_access_index (tree *args,
				  enum btf_core_reloc_kind kind,
				  enum bpf_builtins code ATTRIBUTE_UNUSED)
{
  struct cr_local ret = CR_LOCAL_EMPTY;
  ret.fail = false;

  tree arg = args[0];
  tree root = arg;

  /* Avoid double-recording information if argument is an access to
     a struct/union marked __attribute__((preserve_access_index)).  This
     Will be handled by the attribute handling pass.  */
  if (is_attr_preserve_access (arg))
    {
      ret.reloc_decision = REPLACE_NO_RELOCATION;
      ret.reloc_data.expr = arg;
    }
  else
    {
      ret.reloc_decision = REPLACE_CREATE_RELOCATION;

      unsigned int accessors[100];
      bool valid = true;
      compute_field_expr (arg, accessors, &valid, &root);

      if (valid == true)
	ret.reloc_data.expr = root;
      else
	{
	  bpf_error_at (EXPR_LOC_OR_LOC (arg, UNKNOWN_LOCATION),
			"Cannot compute index for field argument");
	  ret.fail = true;
	}
    }

  /* Note: the type of default_value is used to define the return type of
   __builtin_core_reloc in bpf_resolve_overloaded_core_builtin.  */
  ret.reloc_data.type = TREE_TYPE (root);
  ret.reloc_data.default_value = build_int_cst (ret.reloc_data.type, 0);
  ret.reloc_data.kind = kind;

  if (TREE_CODE (ret.reloc_data.default_value) == ERROR_MARK)
    ret.fail = true;

  return ret;
}

static struct cr_local
pack_field_expr_for_preserve_field (tree *args,
				    enum btf_core_reloc_kind kind,
				    enum bpf_builtins code ATTRIBUTE_UNUSED)
{
  struct cr_local ret = CR_LOCAL_EMPTY;
  ret.fail = false;

  tree arg = args[0];
  tree tmp;
  tree root = arg;

  /* Remove cast to void * created by front-end to fit builtin type, when passed
   * a simple expression like f->u.  */
  if (TREE_CODE (arg) == NOP_EXPR && (tmp = TREE_OPERAND (arg, 0))
      && TREE_CODE (tmp) == ADDR_EXPR && (tmp = TREE_OPERAND (tmp, 0))
      && arg != NULL_TREE)
    arg = tmp;

  unsigned int accessors[100];
  bool valid = true;
  compute_field_expr (arg, accessors, &valid, &root);

  if (valid == true)
    ret.reloc_data.expr = root;
  else
    {
      bpf_error_at (EXPR_LOC_OR_LOC (arg, UNKNOWN_LOCATION),
		    "argument is not a field access");
      ret.fail = true;
    }

  ret.reloc_decision = REPLACE_CREATE_RELOCATION;
  ret.reloc_data.type = TREE_TYPE (root);
  ret.reloc_data.default_value = core_field_info (root, kind);
  ret.reloc_data.kind = kind;

  if (TREE_CODE (ret.reloc_data.default_value) == ERROR_MARK)
    ret.fail = true;

  return ret;
}

static struct cr_final
process_field_expr (struct cr_builtins *data)
{
  gcc_assert (data->kind == BPF_RELO_FIELD_BYTE_OFFSET
	      || data->kind == BPF_RELO_FIELD_BYTE_SIZE
	      || data->kind == BPF_RELO_FIELD_LSHIFT_U64
	      || data->kind == BPF_RELO_FIELD_RSHIFT_U64
	      || data->kind == BPF_RELO_FIELD_SIGNED
	      || data->kind == BPF_RELO_FIELD_EXISTS);

  unsigned int accessors[100];
  unsigned char nr_accessors = 0;
  bool valid = true;
  tree root = NULL_TREE;
  tree expr = data->expr;
  tree type = TREE_TYPE (data->expr);

  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);

  nr_accessors = compute_field_expr (expr, accessors, &valid, &root);

  struct cr_final ret = { NULL, type, data->kind};

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

hash_map <tree, tree> bpf_enum_mappings;

tree enum_value_type = NULL_TREE;
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
   * enum_type and enum_value.  */
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

  result = bpf_enum_mappings.get (enum_value);
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

  ret.reloc_data.type = type;
  ret.reloc_data.kind = kind;
  return ret;
}

static struct cr_final
process_enum_value (struct cr_builtins *data)
{
  gcc_assert (data->kind == BPF_RELO_ENUMVAL_EXISTS
	      || data->kind == BPF_RELO_ENUMVAL_VALUE);

  tree expr = data->expr;
  tree type = data->type;

  struct cr_final ret = { NULL, type, data->kind };

  if (TREE_CODE (expr) == CONST_DECL
     && TREE_CODE (type) == ENUMERAL_TYPE)
    {
      unsigned int index = 0;
      for (tree l = TYPE_VALUES (type); l; l = TREE_CHAIN (l))
	{
	  if (TREE_VALUE (l) == expr)
	    {
	      ret.str = (char *) ggc_alloc_atomic ((index / 10) + 1);
	      sprintf (ret.str, "%d", index);
	      break;
	    }
	  index++;
	}
    }
  else
    gcc_unreachable ();

  return ret;
}

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

  /* Typical structure to match:
   *    *({ extern typeof (TYPE) *<tmp_name>; <tmp_name>; })
   */

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

  /* Force this type to be marked as used in dwarf2out. */
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
		    "invelid first argument format for enum value builtin");
      ret.fail = true;
  return ret;
}

static struct cr_final
process_type (struct cr_builtins *data)
{
  gcc_assert (data->kind == BPF_RELO_TYPE_ID_LOCAL
	      || data->kind == BPF_RELO_TYPE_ID_TARGET
	      || data->kind == BPF_RELO_TYPE_EXISTS
	      || data->kind == BPF_RELO_TYPE_SIZE
	      || data->kind == BPF_RELO_TYPE_MATCHES);

  struct cr_final ret;
  ret.str = NULL;
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


/* Required to overcome having different return type builtins to avoid warnings
   at front-end and be able to share the same builtin definition and permitting
   the PURE attribute to work.  */
hash_map<tree, tree> core_builtin_type_defs;

static tree
get_core_builtin_fndecl_for_type (tree ret_type)
{
  tree *def = core_builtin_type_defs.get (ret_type);
  if (def)
    return *def;

  tree rettype = build_function_type_list (ret_type, integer_type_node, NULL);
  tree new_fndecl = add_builtin_function_ext_scope ("__builtin_core_reloc",
						    rettype,
						    BPF_BUILTIN_CORE_RELOC,
						    BUILT_IN_MD, NULL, NULL);
  DECL_PURE_P (new_fndecl) = 1;

  core_builtin_type_defs.put (ret_type, new_fndecl);

  return new_fndecl;
}

void
bpf_handle_plugin_finish_type (void *event_data,
			       void *data ATTRIBUTE_UNUSED)
{
  tree type = (tree) event_data;

  if (TREE_CODE (type) == ENUMERAL_TYPE)
    for (tree l = TYPE_VALUES (type); l; l = TREE_CHAIN (l))
      {
	tree value = TREE_VALUE (l);

	tree initial = DECL_INITIAL (value);
	initial = copy_node (initial);
	DECL_INITIAL (value) = initial;

	bpf_enum_mappings.put (initial, value);
      }
}

/* -- Header file exposed functions -- */

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
			 pack_field_expr_for_access_index,
			 process_field_expr,
			 true);
  core_builtin_helpers[BPF_BUILTIN_PRESERVE_FIELD_INFO] =
    BPF_CORE_HELPER_SET (kind_preserve_field_info,
			 NULL,
			 pack_field_expr_for_preserve_field,
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
   * __builtin_preserve_enum_value.  */
  plugin_state = (enum bpf_plugin_states) flag_plugin_added;
  flag_plugin_added = true;
  register_callback ("bpf_collect_enum_info", PLUGIN_FINISH_TYPE,
		     bpf_handle_plugin_finish_type, NULL);
}

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
	local_data.reloc_decision = KEEP_ORIGINAL_NO_RELOCATION;

      if (local_data.fail == true)
	return error_mark_node;

      if (local_data.reloc_decision == REPLACE_NO_RELOCATION)
	return local_data.reloc_data.expr;
      else if (local_data.reloc_decision == REPLACE_CREATE_RELOCATION)
	{
	  int index = search_builtin_data (helper.compare,
					   &local_data.reloc_data);
	  if (index == -1)
	    index = allocate_builtin_data ();
	  struct cr_builtins *data = get_builtin_data (index);
	  memcpy (data, &local_data.reloc_data, sizeof (struct cr_builtins));

	  tree new_fndecl = bpf_builtins[BPF_BUILTIN_CORE_RELOC];

	  tree ret_type = TREE_TYPE (local_data.reloc_data.default_value);
	  if (ret_type != ptr_type_node)
	    new_fndecl = get_core_builtin_fndecl_for_type (ret_type);
	  return build_call_expr_loc (loc,
				      new_fndecl, 1,
				      build_int_cst (integer_type_node,
						     index));
	}
    }
  return NULL_TREE;
}

/* This function is used by bpf_resolve_overloaded_builtin which is the
   implementation of the TARGET_RESOLVE_OVERLOADED_BUILTIN.  It is executed in
   a very early stage and allows to adapt the builtin to different arguments
   allowing the compiler to make builtins polymorphic.  In this particular
   implementation, it collects information of the specific builtin call,
   converts it to the internal __builtin_core_reloc, stores any required
   information from the original builtin call in a vec<cr_builtins> and assigns
   the index within the *vec*, replacing by __builtin_core_reloc.  In the
   process we also adjust return type of the __builtin_core_reloc to permit
   polymorphic return type, as it is expected in some of the BPF CO-RE
   builtins.  */

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

  remove_parser_plugin ();

  return construct_builtin_core_reloc (loc, fndecl, args, argsvec->length ());
}

/* Used in bpf_expand_builtin.  This function is called in RTL expand stage to
   convert the internal __builtin_core_reloc in unspec:UNSPEC_CORE_RELOC RTL,
   which will contain a third argument that is the index in the vec collected in
   bpf_resolve_overloaded_core_builtin.  */

rtx
bpf_expand_core_builtin (tree exp, enum bpf_builtins code)
{
  if (code == BPF_BUILTIN_CORE_RELOC)
    {
      tree index = CALL_EXPR_ARG (exp, 0);
      struct cr_builtins *data = get_builtin_data (TREE_INT_CST_LOW (index));

      rtx v = expand_normal (data->default_value);
      rtx i = expand_normal (index);
      return gen_rtx_UNSPEC (DImode,
			     gen_rtvec (2, v, i),
			     UNSPEC_CORE_RELOC);
    }

  return NULL_RTX;
}

/* This function is called in the final assembly output for the
   unspec:UNSPEC_CORE_RELOC.  It recovers the vec index kept as the third
   operand and collects the data from the vec.  With that it calls the process
   helper in order to construct the data required for the CO-RE relocation.
   Also it creates a label pointing to the unspec instruction and uses it
   in the CO-RE relocation creation.  */

const char *
bpf_add_core_reloc (rtx *operands, const char *templ)
{
  struct cr_builtins *data = get_builtin_data (INTVAL (operands[2]));
  builtin_helpers helper;
  helper = core_builtin_helpers[data->orig_builtin_code];

  rtx_code_label * tmp_label = gen_label_rtx ();
  output_asm_label (tmp_label);
  assemble_name (asm_out_file, ":\n");

  gcc_assert (helper.process != NULL);
  struct cr_final reloc_data = helper.process (data);
  make_core_relo (&reloc_data, tmp_label);

  /* Replace default value for later processing builtin types.
     Example if the type id builtins. */
  if (data->rtx_default_value != NULL_RTX)
    operands[1] = data->rtx_default_value;

  return templ;
}

/* This function is used within the defined_expand for mov in bpf.md file.
   It identifies if any of the operands in a move is a expression with a
   type with __attribute__((preserve_access_index)), which case it
   will emit an unspec:UNSPEC_CORE_RELOC such that it would later create a
   CO-RE relocation for this expression access.  */

void
bpf_replace_core_move_operands (rtx *operands)
{
  for (int i = 0; i < 2; i++)
    if (MEM_P (operands[i]))
      {
	tree expr = MEM_EXPR (operands[i]);

	if (expr == NULL_TREE)
	  continue;

	if (TREE_CODE (expr) == MEM_REF
	    && TREE_CODE (TREE_OPERAND (expr, 0)) == SSA_NAME)
	  {
	    gimple *def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (expr, 0));
	    if (def_stmt && is_gimple_assign (def_stmt))
		expr = gimple_assign_rhs1 (def_stmt);
	  }
	if (is_attr_preserve_access (expr)
	    && bpf_require_core_support ())
	  {
	    struct cr_local local_data = pack_field_expr_for_access_index (
					   &expr,
					   BPF_RELO_FIELD_BYTE_OFFSET,
					   BPF_BUILTIN_PRESERVE_ACCESS_INDEX);

	    local_data.reloc_decision = REPLACE_CREATE_RELOCATION;
	    local_data.reloc_data.orig_arg_expr = expr;
	    local_data.reloc_data.orig_builtin_code = BPF_BUILTIN_PRESERVE_ACCESS_INDEX;

	    int index = allocate_builtin_data ();
	    struct cr_builtins *data = get_builtin_data (index);
	    memcpy (data, &local_data.reloc_data, sizeof (struct cr_builtins));

	    rtx reg = XEXP (operands[i], 0);
	    if (!REG_P (reg))
	      {
		reg = gen_reg_rtx (Pmode);
		operands[i] = gen_rtx_MEM (GET_MODE (operands[i]), reg);
	      }

	    emit_insn (
	      gen_mov_reloc_coredi (reg,
				    gen_rtx_CONST_INT (Pmode, 0),
				    gen_rtx_CONST_INT (Pmode, index)));
	    return;
	  }
      }
}

#include "gt-core-builtins.h"
