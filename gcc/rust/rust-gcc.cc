// rust-gcc.cc -- Rust frontend to gcc IR.
// Copyright (C) 2011-2025 Free Software Foundation, Inc.
// Contributed by Ian Lance Taylor, Google.
// forked from gccgo

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

#include "rust-system.h"

// This has to be included outside of extern "C", so we have to
// include it here before tree.h includes it later.
#include <gmp.h>

#include "tree.h"
#include "opts.h"
#include "fold-const.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "tree-iterator.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "convert.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "langhooks.h"
#include "toplev.h"
#include "output.h"
#include "realmpfr.h"
#include "builtins.h"
#include "print-tree.h"
#include "attribs.h"

#include "rust-location.h"
#include "rust-linemap.h"
#include "rust-backend.h"
#include "rust-object-export.h"
#include "rust-gcc.h"

#include "backend/rust-tree.h"
#include "backend/rust-builtins.h"

// Get the tree of a variable for use as an expression.  If this is a
// zero-sized global, create an expression that refers to the decl but
// has zero size.
tree
Bvariable::get_tree (location_t location) const
{
  if (error_operand_p (this->t_))
    return error_mark_node;

  TREE_USED (this->t_) = 1;
  if (this->orig_type_ == NULL || TREE_TYPE (this->t_) == this->orig_type_)
    {
      return this->t_;
    }

  // Return *(orig_type*)&decl.  */
  tree t = build_fold_addr_expr_loc (location, this->t_);
  t = fold_build1_loc (location, NOP_EXPR,
		       build_pointer_type (this->orig_type_), t);
  return build_fold_indirect_ref_loc (location, t);
}

Bvariable *
Bvariable::error_variable ()
{
  return new Bvariable (error_mark_node);
}

// This file implements the interface between the Rust frontend proper
// and the gcc IR.  This implements specific instantiations of
// abstract classes defined by the Rust frontend proper.  The Rust
// frontend proper class methods of these classes to generate the
// backend representation.

// A helper function to create a GCC identifier from a C++ string.

static inline tree
get_identifier_from_string (const std::string &str)
{
  return get_identifier_with_length (str.data (), str.length ());
}

namespace Backend {

// Define the built-in functions that are exposed to GCCRust.

void
init ()
{
  /* We need to define the fetch_and_add functions, since we use them
     for ++ and --.  */
  // tree t = this->integer_type (true, BITS_PER_UNIT)->get_tree ();
  // tree p = build_pointer_type (build_qualified_type (t, TYPE_QUAL_VOLATILE));
  // this->define_builtin (BUILT_IN_SYNC_ADD_AND_FETCH_1,
  // "__sync_fetch_and_add_1",
  //       		NULL, build_function_type_list (t, p, t, NULL_TREE), 0);

  // t = this->integer_type (true, BITS_PER_UNIT * 2)->get_tree ();
  // p = build_pointer_type (build_qualified_type (t, TYPE_QUAL_VOLATILE));
  // this->define_builtin (BUILT_IN_SYNC_ADD_AND_FETCH_2,
  // "__sync_fetch_and_add_2",
  //       		NULL, build_function_type_list (t, p, t, NULL_TREE), 0);

  // t = this->integer_type (true, BITS_PER_UNIT * 4)->get_tree ();
  // p = build_pointer_type (build_qualified_type (t, TYPE_QUAL_VOLATILE));
  // this->define_builtin (BUILT_IN_SYNC_ADD_AND_FETCH_4,
  // "__sync_fetch_and_add_4",
  //       		NULL, build_function_type_list (t, p, t, NULL_TREE), 0);

  // t = this->integer_type (true, BITS_PER_UNIT * 8)->get_tree ();
  // p = build_pointer_type (build_qualified_type (t, TYPE_QUAL_VOLATILE));
  // this->define_builtin (BUILT_IN_SYNC_ADD_AND_FETCH_8,
  // "__sync_fetch_and_add_8",
  //       		NULL, build_function_type_list (t, p, t, NULL_TREE), 0);

  // // We use __builtin_expect for magic import functions.
  // this->define_builtin (BUILT_IN_EXPECT, "__builtin_expect", NULL,
  //       		build_function_type_list (long_integer_type_node,
  //       					  long_integer_type_node,
  //       					  long_integer_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);

  // // We use __builtin_memcmp for struct comparisons.
  // this->define_builtin (BUILT_IN_MEMCMP, "__builtin_memcmp", "memcmp",
  //       		build_function_type_list (integer_type_node,
  //       					  const_ptr_type_node,
  //       					  const_ptr_type_node,
  //       					  size_type_node, NULL_TREE),
  //       		0);

  // // We use __builtin_memmove for copying data.
  // this->define_builtin (BUILT_IN_MEMMOVE, "__builtin_memmove", "memmove",
  //       		build_function_type_list (void_type_node, ptr_type_node,
  //       					  const_ptr_type_node,
  //       					  size_type_node, NULL_TREE),
  //       		0);

  // // We use __builtin_memset for zeroing data.
  // this->define_builtin (BUILT_IN_MEMSET, "__builtin_memset", "memset",
  //       		build_function_type_list (void_type_node, ptr_type_node,
  //       					  integer_type_node,
  //       					  size_type_node, NULL_TREE),
  //       		0);

  // // Used by runtime/internal/sys and math/bits.
  // this->define_builtin (BUILT_IN_CTZ, "__builtin_ctz", "ctz",
  //       		build_function_type_list (integer_type_node,
  //       					  unsigned_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_CTZLL, "__builtin_ctzll", "ctzll",
  //       		build_function_type_list (integer_type_node,
  //       					  long_long_unsigned_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_CLZ, "__builtin_clz", "clz",
  //       		build_function_type_list (integer_type_node,
  //       					  unsigned_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_CLZLL, "__builtin_clzll", "clzll",
  //       		build_function_type_list (integer_type_node,
  //       					  long_long_unsigned_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_POPCOUNT, "__builtin_popcount", "popcount",
  //       		build_function_type_list (integer_type_node,
  //       					  unsigned_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_POPCOUNTLL, "__builtin_popcountll",
  //       		"popcountll",
  //       		build_function_type_list (integer_type_node,
  //       					  long_long_unsigned_type_node,
  //       					  NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_BSWAP16, "__builtin_bswap16", "bswap16",
  //       		build_function_type_list (uint16_type_node,
  //       					  uint16_type_node, NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_BSWAP32, "__builtin_bswap32", "bswap32",
  //       		build_function_type_list (uint32_type_node,
  //       					  uint32_type_node, NULL_TREE),
  //       		builtin_const);
  // this->define_builtin (BUILT_IN_BSWAP64, "__builtin_bswap64", "bswap64",
  //       		build_function_type_list (uint64_type_node,
  //       					  uint64_type_node, NULL_TREE),
  //       		builtin_const);

  // We provide some functions for the math library.

  // We use __builtin_return_address in the thunk we build for
  // functions which call recover, and for runtime.getcallerpc.
  // t = build_function_type_list (ptr_type_node, unsigned_type_node,
  // NULL_TREE); this->define_builtin (BUILT_IN_RETURN_ADDRESS,
  // "__builtin_return_address",
  //       		NULL, t, 0);

  // The runtime calls __builtin_dwarf_cfa for runtime.getcallersp.
  // t = build_function_type_list (ptr_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_DWARF_CFA, "__builtin_dwarf_cfa", NULL, t,
  // 0);

  // The runtime calls __builtin_extract_return_addr when recording
  // the address to which a function returns.
  // this->define_builtin (
  //   BUILT_IN_EXTRACT_RETURN_ADDR, "__builtin_extract_return_addr", NULL,
  //   build_function_type_list (ptr_type_node, ptr_type_node, NULL_TREE), 0);

  // The compiler uses __builtin_trap for some exception handling
  // cases.
  // this->define_builtin (BUILT_IN_TRAP, "__builtin_trap", NULL,
  //       		build_function_type (void_type_node, void_list_node),
  //       		builtin_noreturn);

  // The runtime uses __builtin_prefetch.
  // this->define_builtin (BUILT_IN_PREFETCH, "__builtin_prefetch", NULL,
  //       		build_varargs_function_type_list (void_type_node,
  //       						  const_ptr_type_node,
  //       						  NULL_TREE),
  //       		builtin_novops);

  // The compiler uses __builtin_unreachable for cases that cannot
  // occur.
  // this->define_builtin (BUILT_IN_UNREACHABLE, "__builtin_unreachable", NULL,
  //       		build_function_type (void_type_node, void_list_node),
  //       		builtin_const | builtin_noreturn);

  // We provide some atomic functions.
  // t = build_function_type_list (uint32_type_node, ptr_type_node,
  //       			integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_LOAD_4, "__atomic_load_4", NULL, t,
  // 0);

  // t = build_function_type_list (uint64_type_node, ptr_type_node,
  //       			integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_LOAD_8, "__atomic_load_8", NULL, t,
  // 0);

  // t = build_function_type_list (void_type_node, ptr_type_node,
  // uint32_type_node,
  //       			integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_STORE_4, "__atomic_store_4", NULL, t,
  //       		0);

  // t = build_function_type_list (void_type_node, ptr_type_node,
  // uint64_type_node,
  //       			integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_STORE_8, "__atomic_store_8", NULL, t,
  //       		0);

  // t = build_function_type_list (uint32_type_node, ptr_type_node,
  //       			uint32_type_node, integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_EXCHANGE_4, "__atomic_exchange_4",
  // NULL,
  //       		t, 0);

  // t = build_function_type_list (uint64_type_node, ptr_type_node,
  //       			uint64_type_node, integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_EXCHANGE_8, "__atomic_exchange_8",
  // NULL,
  //       		t, 0);

  // t = build_function_type_list (boolean_type_node, ptr_type_node,
  // ptr_type_node,
  //       			uint32_type_node, boolean_type_node,
  //       			integer_type_node, integer_type_node,
  //       			NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4,
  //       		"__atomic_compare_exchange_4", NULL, t, 0);

  // t = build_function_type_list (boolean_type_node, ptr_type_node,
  // ptr_type_node,
  //       			uint64_type_node, boolean_type_node,
  //       			integer_type_node, integer_type_node,
  //       			NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8,
  //       		"__atomic_compare_exchange_8", NULL, t, 0);

  // t = build_function_type_list (uint32_type_node, ptr_type_node,
  //       			uint32_type_node, integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_ADD_FETCH_4, "__atomic_add_fetch_4",
  //       		NULL, t, 0);

  // t = build_function_type_list (uint64_type_node, ptr_type_node,
  //       			uint64_type_node, integer_type_node, NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_ADD_FETCH_8, "__atomic_add_fetch_8",
  //       		NULL, t, 0);

  // t = build_function_type_list (unsigned_char_type_node, ptr_type_node,
  //       			unsigned_char_type_node, integer_type_node,
  //       			NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_AND_FETCH_1, "__atomic_and_fetch_1",
  //       		NULL, t, 0);
  // this->define_builtin (BUILT_IN_ATOMIC_FETCH_AND_1, "__atomic_fetch_and_1",
  //       		NULL, t, 0);

  // t = build_function_type_list (unsigned_char_type_node, ptr_type_node,
  //       			unsigned_char_type_node, integer_type_node,
  //       			NULL_TREE);
  // this->define_builtin (BUILT_IN_ATOMIC_OR_FETCH_1, "__atomic_or_fetch_1",
  // NULL,
  //       		t, 0);
  // this->define_builtin (BUILT_IN_ATOMIC_FETCH_OR_1, "__atomic_fetch_or_1",
  // NULL,
  //       		t, 0);
}

void
debug (tree t)
{
  debug_tree (t);
};

void
debug (Bvariable *t)
{
  debug_tree (t->get_decl ());
};

tree
get_identifier_node (const std::string &str)
{
  return get_identifier_with_length (str.data (), str.length ());
}

tree
wchar_type ()
{
  static tree wchar;

  if (wchar == NULL_TREE)
    {
      wchar = make_unsigned_type (32);
      TYPE_STRING_FLAG (wchar) = 1;
    }

  return wchar;
}

// Get an unnamed integer type.

int
get_pointer_size ()
{
  return POINTER_SIZE;
}

tree
raw_str_type ()
{
  tree char_ptr = build_pointer_type (char_type_node);
  tree const_char_type = build_qualified_type (char_ptr, TYPE_QUAL_CONST);
  return const_char_type;
}

tree
integer_type (bool is_unsigned, int bits)
{
  tree type;
  if (is_unsigned)
    {
      if (bits == INT_TYPE_SIZE)
	type = unsigned_type_node;
      else if (bits == SHORT_TYPE_SIZE)
	type = short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
	type = long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
	type = long_long_unsigned_type_node;
      else
	type = make_unsigned_type (bits);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
	type = integer_type_node;
      else if (bits == SHORT_TYPE_SIZE)
	type = short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
	type = long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
	type = long_long_integer_type_node;
      else
	type = make_signed_type (bits);
    }
  return type;
}

// Get an unnamed float type.

tree
float_type (int bits)
{
  tree type;
  if (bits == TYPE_PRECISION (float_type_node))
    type = float_type_node;
  else if (bits == TYPE_PRECISION (double_type_node))
    type = double_type_node;
  else if (bits == TYPE_PRECISION (long_double_type_node))
    type = long_double_type_node;
  else
    {
      type = make_node (REAL_TYPE);
      TYPE_PRECISION (type) = bits;
      layout_type (type);
    }
  return type;
}

// Get a pointer type.

tree
pointer_type (tree to_type)
{
  if (error_operand_p (to_type))
    return error_mark_node;
  tree type = build_pointer_type (to_type);
  return type;
}

// Get a reference type.

tree
reference_type (tree to_type)
{
  if (error_operand_p (to_type))
    return error_mark_node;
  tree type = build_reference_type (to_type);
  return type;
}

// Get immutable type

tree
immutable_type (tree base)
{
  if (error_operand_p (base))
    return error_mark_node;
  tree constified = build_qualified_type (base, TYPE_QUAL_CONST);
  return constified;
}

// Make a function type.

tree
function_type (const typed_identifier &receiver,
	       const std::vector<typed_identifier> &parameters,
	       const std::vector<typed_identifier> &results, tree result_struct,
	       location_t)
{
  tree args = NULL_TREE;
  tree *pp = &args;
  if (receiver.type != NULL_TREE)
    {
      tree t = receiver.type;
      if (error_operand_p (t))
	return error_mark_node;
      *pp = tree_cons (NULL_TREE, t, NULL_TREE);
      pp = &TREE_CHAIN (*pp);
    }

  for (const auto &p : parameters)
    {
      tree t = p.type;
      if (error_operand_p (t))
	return error_mark_node;
      *pp = tree_cons (NULL_TREE, t, NULL_TREE);
      pp = &TREE_CHAIN (*pp);
    }

  // Varargs is handled entirely at the Rust level.  When converted to
  // GENERIC functions are not varargs.
  *pp = void_list_node;

  tree result;
  if (results.empty ())
    result = void_type_node;
  else if (results.size () == 1)
    result = results.front ().type;
  else
    {
      gcc_assert (result_struct != NULL);
      result = result_struct;
    }
  if (error_operand_p (result))
    return error_mark_node;

  tree fntype = build_function_type (result, args);
  if (error_operand_p (fntype))
    return error_mark_node;

  return build_pointer_type (fntype);
}

tree
function_type_variadic (const typed_identifier &receiver,
			const std::vector<typed_identifier> &parameters,
			const std::vector<typed_identifier> &results,
			tree result_struct, location_t)
{
  size_t n = parameters.size () + (receiver.type != NULL_TREE ? 1 : 0);
  tree *args = XALLOCAVEC (tree, n);
  size_t offs = 0;
  if (error_operand_p (receiver.type))
    return error_mark_node;

  if (receiver.type != NULL_TREE)
    args[offs++] = receiver.type;

  for (const auto &p : parameters)
    {
      tree t = p.type;
      if (error_operand_p (t))
	return error_mark_node;
      args[offs++] = t;
    }

  tree result;
  if (results.empty ())
    result = void_type_node;
  else if (results.size () == 1)
    result = results.front ().type;
  else
    {
      gcc_assert (result_struct != NULL_TREE);
      result = result_struct;
    }
  if (error_operand_p (result))
    return error_mark_node;

  tree fntype = build_varargs_function_type_array (result, n, args);
  if (error_operand_p (fntype))
    return error_mark_node;

  return build_pointer_type (fntype);
}

tree
function_ptr_type (tree result_type, const std::vector<tree> &parameters,
		   location_t /* locus */)
{
  tree args = NULL_TREE;
  tree *pp = &args;

  for (auto &param : parameters)
    {
      if (error_operand_p (param))
	return error_mark_node;

      *pp = tree_cons (NULL_TREE, param, NULL_TREE);
      pp = &TREE_CHAIN (*pp);
    }

  *pp = void_list_node;

  tree result = result_type;
  if (result != void_type_node && int_size_in_bytes (result) == 0)
    result = void_type_node;

  tree fntype = build_function_type (result, args);
  if (error_operand_p (fntype))
    return error_mark_node;

  return build_pointer_type (fntype);
}

// Make a struct type.

tree
struct_type (const std::vector<typed_identifier> &fields, bool layout)
{
  return fill_in_fields (make_node (RECORD_TYPE), fields, layout);
}

// Make a union type.

tree
union_type (const std::vector<typed_identifier> &fields, bool layout)
{
  return fill_in_fields (make_node (UNION_TYPE), fields, layout);
}

// Fill in the fields of a struct or union type.

tree
fill_in_fields (tree fill, const std::vector<typed_identifier> &fields,
		bool layout)
{
  tree field_trees = NULL_TREE;
  tree *pp = &field_trees;
  for (const auto &p : fields)
    {
      tree name_tree = get_identifier_from_string (p.name);
      tree type_tree = p.type;
      if (error_operand_p (type_tree))
	return error_mark_node;
      tree field = build_decl (p.location, FIELD_DECL, name_tree, type_tree);
      DECL_CONTEXT (field) = fill;
      *pp = field;
      pp = &DECL_CHAIN (field);
    }
  TYPE_FIELDS (fill) = field_trees;

  if (layout)
    layout_type (fill);

  // Because Rust permits converting between named struct types and
  // equivalent struct types, for which we use VIEW_CONVERT_EXPR, and
  // because we don't try to maintain TYPE_CANONICAL for struct types,
  // we need to tell the middle-end to use structural equality.
  SET_TYPE_STRUCTURAL_EQUALITY (fill);

  return fill;
}

// Make an array type.

tree
array_type (tree element_type, tree length)
{
  return fill_in_array (make_node (ARRAY_TYPE), element_type, length);
}

// Fill in an array type.

tree
fill_in_array (tree fill, tree element_type, tree length_tree)
{
  if (error_operand_p (element_type) || error_operand_p (length_tree))
    return error_mark_node;

  gcc_assert (TYPE_SIZE (element_type) != NULL_TREE);

  length_tree = fold_convert (sizetype, length_tree);

  // build_index_type takes the maximum index, which is one less than
  // the length.
  tree index_type_tree = build_index_type (
    fold_build2 (MINUS_EXPR, sizetype, length_tree, size_one_node));

  TREE_TYPE (fill) = element_type;
  TYPE_DOMAIN (fill) = index_type_tree;
  TYPE_ADDR_SPACE (fill) = TYPE_ADDR_SPACE (element_type);
  layout_type (fill);

  if (TYPE_STRUCTURAL_EQUALITY_P (element_type))
    SET_TYPE_STRUCTURAL_EQUALITY (fill);
  else if (TYPE_CANONICAL (element_type) != element_type
	   || TYPE_CANONICAL (index_type_tree) != index_type_tree)
    TYPE_CANONICAL (fill) = build_array_type (TYPE_CANONICAL (element_type),
					      TYPE_CANONICAL (index_type_tree));

  return fill;
}

// Return a named version of a type.

tree
named_type (const std::string &name, tree type, location_t location)
{
  if (error_operand_p (type))
    return error_mark_node;

  // The middle-end expects a basic type to have a name.  In Rust every
  // basic type will have a name.  The first time we see a basic type,
  // give it whatever Rust name we have at this point.
  if (TYPE_NAME (type) == NULL_TREE && location == BUILTINS_LOCATION
      && (TREE_CODE (type) == INTEGER_TYPE || TREE_CODE (type) == REAL_TYPE
	  || TREE_CODE (type) == COMPLEX_TYPE
	  || TREE_CODE (type) == BOOLEAN_TYPE))
    {
      tree decl = build_decl (BUILTINS_LOCATION, TYPE_DECL,
			      get_identifier_from_string (name), type);
      TYPE_NAME (type) = decl;
      return type;
    }

  tree copy = build_variant_type_copy (type);
  tree decl
    = build_decl (location, TYPE_DECL, get_identifier_from_string (name), copy);
  DECL_ORIGINAL_TYPE (decl) = type;
  TYPE_NAME (copy) = decl;
  return copy;
}

// Return the size of a type.

int64_t
type_size (tree t)
{
  if (error_operand_p (t))
    return 1;
  if (t == void_type_node)
    return 0;
  t = TYPE_SIZE_UNIT (t);
  gcc_assert (tree_fits_uhwi_p (t));
  unsigned HOST_WIDE_INT val_wide = TREE_INT_CST_LOW (t);
  int64_t ret = static_cast<int64_t> (val_wide);
  if (ret < 0 || static_cast<unsigned HOST_WIDE_INT> (ret) != val_wide)
    return -1;
  return ret;
}

// Return the alignment of a type.

int64_t
type_alignment (tree t)
{
  if (error_operand_p (t))
    return 1;
  return TYPE_ALIGN_UNIT (t);
}

// Return the alignment of a struct field of type BTYPE.

int64_t
type_field_alignment (tree t)
{
  if (error_operand_p (t))
    return 1;
  return rust_field_alignment (t);
}

// Return the offset of a field in a struct.

int64_t
type_field_offset (tree struct_tree, size_t index)
{
  if (error_operand_p (struct_tree))
    return 0;
  gcc_assert (TREE_CODE (struct_tree) == RECORD_TYPE);
  tree field = TYPE_FIELDS (struct_tree);
  for (; index > 0; --index)
    {
      field = DECL_CHAIN (field);
      gcc_assert (field != NULL_TREE);
    }
  HOST_WIDE_INT offset_wide = int_byte_position (field);
  int64_t ret = static_cast<int64_t> (offset_wide);
  gcc_assert (ret == offset_wide);
  return ret;
}

// Return the zero value for a type.

tree
zero_expression (tree t)
{
  tree ret;
  if (error_operand_p (t))
    ret = error_mark_node;
  else
    ret = build_zero_cst (t);
  return ret;
}

// An expression that references a variable.

tree
var_expression (Bvariable *var, location_t location)
{
  return var->get_tree (location);
}

// Return a typed value as a constant floating-point number.

tree
float_constant_expression (tree t, mpfr_t val)
{
  tree ret;
  if (error_operand_p (t))
    return error_mark_node;

  REAL_VALUE_TYPE r1;
  real_from_mpfr (&r1, val, t, GMP_RNDN);
  REAL_VALUE_TYPE r2;
  real_convert (&r2, TYPE_MODE (t), &r1);
  ret = build_real (t, r2);
  return ret;
}

// Make a constant string expression.

tree
string_constant_expression (const std::string &val)
{
  tree index_type = build_index_type (size_int (val.length ()));
  tree const_char_type = build_qualified_type (char_type_node, TYPE_QUAL_CONST);
  tree string_type = build_array_type (const_char_type, index_type);
  TYPE_STRING_FLAG (string_type) = 1;
  tree string_val = build_string (val.length (), val.data ());
  TREE_TYPE (string_val) = string_type;

  return string_val;
}

tree
wchar_constant_expression (wchar_t c)
{
  return build_int_cst (wchar_type (), c);
}

tree
char_constant_expression (char c)
{
  return build_int_cst (char_type_node, c);
}

// Make a constant boolean expression.

tree
boolean_constant_expression (bool val)
{
  return val ? boolean_true_node : boolean_false_node;
}

// An expression that converts an expression to a different type.

tree
convert_expression (tree type_tree, tree expr_tree, location_t location)
{
  if (error_operand_p (type_tree) || error_operand_p (expr_tree))
    return error_mark_node;

  tree ret;
  if (type_size (type_tree) == 0 || TREE_TYPE (expr_tree) == void_type_node)
    {
      // Do not convert zero-sized types.
      ret = expr_tree;
    }
  else if (TREE_CODE (type_tree) == INTEGER_TYPE)
    ret = convert_to_integer (type_tree, expr_tree);
  else if (TREE_CODE (type_tree) == REAL_TYPE)
    ret = convert_to_real (type_tree, expr_tree);
  else if (TREE_CODE (type_tree) == COMPLEX_TYPE)
    ret = convert_to_complex (type_tree, expr_tree);
  else if (TREE_CODE (type_tree) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (expr_tree)) == INTEGER_TYPE)
    ret = convert_to_pointer (type_tree, expr_tree);
  else if (TREE_CODE (type_tree) == RECORD_TYPE
	   || TREE_CODE (type_tree) == ARRAY_TYPE)
    ret = fold_build1_loc (location, VIEW_CONVERT_EXPR, type_tree, expr_tree);
  else
    ret = fold_convert_loc (location, type_tree, expr_tree);

  return ret;
}

// Return an expression for the field at INDEX in BSTRUCT.

tree
struct_field_expression (tree struct_tree, size_t index, location_t location)
{
  if (error_operand_p (struct_tree))
    return error_mark_node;
  gcc_assert (TREE_CODE (TREE_TYPE (struct_tree)) == RECORD_TYPE
	      || TREE_CODE (TREE_TYPE (struct_tree)) == UNION_TYPE);
  tree field = TYPE_FIELDS (TREE_TYPE (struct_tree));
  if (field == NULL_TREE)
    {
      // This can happen for a type which refers to itself indirectly
      // and then turns out to be erroneous.
      return error_mark_node;
    }
  for (unsigned int i = index; i > 0; --i)
    {
      field = DECL_CHAIN (field);
      gcc_assert (field != NULL_TREE);
    }
  if (error_operand_p (TREE_TYPE (field)))
    return error_mark_node;
  tree ret = fold_build3_loc (location, COMPONENT_REF, TREE_TYPE (field),
			      struct_tree, field, NULL_TREE);
  if (TREE_CONSTANT (struct_tree))
    TREE_CONSTANT (ret) = 1;
  return ret;
}

// Return an expression that executes BSTAT before BEXPR.

tree
compound_expression (tree stat, tree expr, location_t location)
{
  if (error_operand_p (stat) || error_operand_p (expr))
    return error_mark_node;
  tree ret
    = fold_build2_loc (location, COMPOUND_EXPR, TREE_TYPE (expr), stat, expr);
  return ret;
}

// Return an expression that executes THEN_EXPR if CONDITION is true, or
// ELSE_EXPR otherwise.

tree
conditional_expression (tree, tree type_tree, tree cond_expr, tree then_expr,
			tree else_expr, location_t location)
{
  if (error_operand_p (type_tree) || error_operand_p (cond_expr)
      || error_operand_p (then_expr) || error_operand_p (else_expr))
    return error_mark_node;
  tree ret = build3_loc (location, COND_EXPR, type_tree, cond_expr, then_expr,
			 else_expr);
  return ret;
}

/* Helper function that converts rust operators to equivalent GCC tree_code.
   Note that CompoundAssignmentOperator don't get their corresponding tree_code,
   because they get compiled away when we lower AST to HIR. */
static enum tree_code
operator_to_tree_code (NegationOperator op)
{
  switch (op)
    {
    case NegationOperator::NEGATE:
      return NEGATE_EXPR;
    case NegationOperator::NOT:
      return BIT_NOT_EXPR;
    default:
      rust_unreachable ();
    }
}

/* Note that GCC tree code distinguishes floating point division and integer
   division. These two types of division are represented as the same rust
   operator, and can only be distinguished via context(i.e. the TREE_TYPE of the
   operands). */
static enum tree_code
operator_to_tree_code (ArithmeticOrLogicalOperator op, bool floating_point)
{
  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
      return PLUS_EXPR;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      return MINUS_EXPR;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return MULT_EXPR;
    case ArithmeticOrLogicalOperator::DIVIDE:
      if (floating_point)
	return RDIV_EXPR;
      else
	return TRUNC_DIV_EXPR;
    case ArithmeticOrLogicalOperator::MODULUS:
      return TRUNC_MOD_EXPR;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      return BIT_AND_EXPR;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      return BIT_IOR_EXPR;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return BIT_XOR_EXPR;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      return LSHIFT_EXPR;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return RSHIFT_EXPR;
    default:
      rust_unreachable ();
    }
}

static enum tree_code
operator_to_tree_code (ComparisonOperator op)
{
  switch (op)
    {
    case ComparisonOperator::EQUAL:
      return EQ_EXPR;
    case ComparisonOperator::NOT_EQUAL:
      return NE_EXPR;
    case ComparisonOperator::GREATER_THAN:
      return GT_EXPR;
    case ComparisonOperator::LESS_THAN:
      return LT_EXPR;
    case ComparisonOperator::GREATER_OR_EQUAL:
      return GE_EXPR;
    case ComparisonOperator::LESS_OR_EQUAL:
      return LE_EXPR;
    default:
      rust_unreachable ();
    }
}

static enum tree_code
operator_to_tree_code (LazyBooleanOperator op)
{
  switch (op)
    {
    case LazyBooleanOperator::LOGICAL_OR:
      return TRUTH_ORIF_EXPR;
    case LazyBooleanOperator::LOGICAL_AND:
      return TRUTH_ANDIF_EXPR;
    default:
      rust_unreachable ();
    }
}

/* Returns true if the type of EXP is a floating point type.
   False otherwise.  */
bool
is_floating_point (tree exp)
{
  return FLOAT_TYPE_P (TREE_TYPE (exp));
}

// Return an expression for the negation operation OP EXPR.
tree
negation_expression (NegationOperator op, tree expr_tree, location_t location)
{
  /* Check if the expression is an error, in which case we return an error
     expression. */
  if (error_operand_p (expr_tree))
    return error_mark_node;

  /* For negation operators, the resulting type should be the same as its
     operand. */
  auto tree_type = TREE_TYPE (expr_tree);
  auto original_type = tree_type;
  auto tree_code = operator_to_tree_code (op);

  /* For floating point operations we may need to extend the precision of type.
     For example, a 64-bit machine may not support operations on float32. */
  bool floating_point = is_floating_point (expr_tree);
  auto extended_type = NULL_TREE;
  if (floating_point)
    {
      extended_type = excess_precision_type (tree_type);
      if (extended_type != NULL_TREE)
	{
	  expr_tree = convert (extended_type, expr_tree);
	  tree_type = extended_type;
	}
    }

  /* Construct a new tree and build an expression from it. */
  auto new_tree = fold_build1_loc (location, tree_code, tree_type, expr_tree);
  if (floating_point && extended_type != NULL_TREE)
    new_tree = convert (original_type, expr_tree);
  return new_tree;
}

tree
arithmetic_or_logical_expression (ArithmeticOrLogicalOperator op, tree left,
				  tree right, location_t location)
{
  /* Check if either expression is an error, in which case we return an error
     expression. */
  if (error_operand_p (left) || error_operand_p (right))
    return error_mark_node;

  // unwrap the const decls if set
  if (TREE_CODE (left) == CONST_DECL)
    left = DECL_INITIAL (left);
  if (TREE_CODE (right) == CONST_DECL)
    right = DECL_INITIAL (right);

  /* We need to determine if we're doing floating point arithmetics of integer
     arithmetics. */
  bool floating_point = is_floating_point (left);
  auto ret = NULL_TREE;

  /* For arithmetic or logical operators, the resulting type should be the same
     as the lhs operand. */
  auto tree_type = TREE_TYPE (left);
  auto original_type = tree_type;
  auto tree_code = operator_to_tree_code (op, floating_point);

  /* For floating point operations we may need to extend the precision of type.
     For example, a 64-bit machine may not support operations on float32. */
  auto extended_type = NULL_TREE;
  if (floating_point)
    {
      extended_type = excess_precision_type (tree_type);
      if (extended_type != NULL_TREE)
	{
	  left = convert (extended_type, left);
	  right = convert (extended_type, right);
	  tree_type = extended_type;
	}
    }

  ret = fold_build2_loc (location, tree_code, tree_type, left, right);
  TREE_CONSTANT (ret) = TREE_CONSTANT (left) & TREE_CONSTANT (right);

  // TODO: How do we handle floating point?
  if (floating_point && extended_type != NULL_TREE)
    ret = convert (original_type, ret);

  if (op == ArithmeticOrLogicalOperator::DIVIDE
      && (integer_zerop (right) || fixed_zerop (right)))
    {
      rust_error_at (location, "division by zero");
    }
  else if (op == ArithmeticOrLogicalOperator::LEFT_SHIFT
	   && TREE_CODE (right) == INTEGER_CST
	   && (compare_tree_int (right, TYPE_PRECISION (TREE_TYPE (ret))) >= 0))
    {
      rust_error_at (location, "left shift count >= width of type");
    }

  return ret;
}

static bool
is_overflowing_expr (ArithmeticOrLogicalOperator op)
{
  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
    case ArithmeticOrLogicalOperator::SUBTRACT:
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return true;
    default:
      return false;
    }
}

static std::pair<tree, tree>
fetch_overflow_builtins (ArithmeticOrLogicalOperator op)
{
  auto builtin_ctx = Rust::Compile::BuiltinsContext::get ();

  auto builtin = NULL_TREE;
  auto abort = NULL_TREE;

  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
      builtin_ctx.lookup_simple_builtin ("__builtin_add_overflow", &builtin);
      break;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      builtin_ctx.lookup_simple_builtin ("__builtin_sub_overflow", &builtin);
      break;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      builtin_ctx.lookup_simple_builtin ("__builtin_mul_overflow", &builtin);
      break;
    default:
      rust_unreachable ();
      break;
    };

  builtin_ctx.lookup_simple_builtin ("__builtin_abort", &abort);

  rust_assert (abort);
  rust_assert (builtin);

  return {abort, builtin};
}

// Return an expression for the arithmetic or logical operation LEFT OP RIGHT
// with overflow checking when possible
tree
arithmetic_or_logical_expression_checked (ArithmeticOrLogicalOperator op,
					  tree left, tree right,
					  location_t location,
					  Bvariable *receiver_var)
{
  /* Check if either expression is an error, in which case we return an error
     expression. */
  if (error_operand_p (left) || error_operand_p (right))
    return error_mark_node;

  // FIXME: Add `if (!debug_mode)`
  // No overflow checks for floating point operations or divisions. In that
  // case, simply assign the result of the operation to the receiver variable
  if (is_floating_point (left) || !is_overflowing_expr (op))
    return assignment_statement (
      receiver_var->get_tree (location),
      arithmetic_or_logical_expression (op, left, right, location), location);

  auto receiver = receiver_var->get_tree (location);
  TREE_ADDRESSABLE (receiver) = 1;
  auto result_ref = build_fold_addr_expr_loc (location, receiver);

  auto builtins = fetch_overflow_builtins (op);
  auto abort = builtins.first;
  auto builtin = builtins.second;

  auto abort_call = build_call_expr_loc (location, abort, 0);

  auto builtin_call
    = build_call_expr_loc (location, builtin, 3, left, right, result_ref);
  auto overflow_check
    = build2_loc (location, EQ_EXPR, boolean_type_node, builtin_call,
		  boolean_constant_expression (true));

  auto if_block = build3_loc (location, COND_EXPR, void_type_node,
			      overflow_check, abort_call, NULL_TREE);

  return if_block;
}

// Return an expression for the comparison operation LEFT OP RIGHT.
tree
comparison_expression (ComparisonOperator op, tree left_tree, tree right_tree,
		       location_t location)
{
  /* Check if either expression is an error, in which case we return an error
     expression. */
  if (error_operand_p (left_tree) || error_operand_p (right_tree))
    return error_mark_node;

  /* For comparison operators, the resulting type should be boolean. */
  auto tree_type = boolean_type_node;
  auto tree_code = operator_to_tree_code (op);

  /* Construct a new tree and build an expression from it. */
  auto new_tree
    = fold_build2_loc (location, tree_code, tree_type, left_tree, right_tree);
  return new_tree;
}

// Return an expression for the lazy boolean operation LEFT OP RIGHT.
tree
lazy_boolean_expression (LazyBooleanOperator op, tree left_tree,
			 tree right_tree, location_t location)
{
  /* Check if either expression is an error, in which case we return an error
     expression. */
  if (error_operand_p (left_tree) || error_operand_p (right_tree))
    return error_mark_node;

  /* For lazy boolean operators, the resulting type should be the same as the
     rhs operand. */
  auto tree_type = TREE_TYPE (right_tree);
  auto tree_code = operator_to_tree_code (op);

  /* Construct a new tree and build an expression from it. */
  auto new_tree
    = fold_build2_loc (location, tree_code, tree_type, left_tree, right_tree);
  return new_tree;
}

// Return an expression that constructs BTYPE with VALS.

tree
constructor_expression (tree type_tree, bool is_variant,
			const std::vector<tree> &vals, int union_index,
			location_t location)
{
  if (error_operand_p (type_tree))
    return error_mark_node;

  vec<constructor_elt, va_gc> *init;
  vec_alloc (init, vals.size ());

  tree sink = NULL_TREE;
  bool is_constant = true;
  tree field = TYPE_FIELDS (type_tree);

  if (is_variant)
    {
      gcc_assert (union_index != -1);
      gcc_assert (TREE_CODE (type_tree) == UNION_TYPE);

      for (int i = 0; i < union_index; i++)
	{
	  gcc_assert (field != NULL_TREE);
	  field = DECL_CHAIN (field);
	}

      tree nested_ctor
	= constructor_expression (TREE_TYPE (field), false, vals, -1, location);

      constructor_elt empty = {NULL, NULL};
      constructor_elt *elt = init->quick_push (empty);
      elt->index = field;
      elt->value = convert_tree (TREE_TYPE (field), nested_ctor, location);
      if (!TREE_CONSTANT (elt->value))
	is_constant = false;
    }
  else
    {
      if (union_index != -1)
	{
	  gcc_assert (TREE_CODE (type_tree) == UNION_TYPE);
	  tree val = vals.front ();
	  for (int i = 0; i < union_index; i++)
	    {
	      gcc_assert (field != NULL_TREE);
	      field = DECL_CHAIN (field);
	    }

	  if (TREE_TYPE (field) == error_mark_node || error_operand_p (val))
	    return error_mark_node;

	  if (int_size_in_bytes (TREE_TYPE (field)) == 0)
	    {
	      // GIMPLE cannot represent indices of zero-sized types so
	      // trying to construct a map with zero-sized keys might lead
	      // to errors.  Instead, we evaluate each expression that
	      // would have been added as a map element for its
	      // side-effects and construct an empty map.
	      append_to_statement_list (val, &sink);
	    }
	  else
	    {
	      constructor_elt empty = {NULL, NULL};
	      constructor_elt *elt = init->quick_push (empty);
	      elt->index = field;
	      elt->value = convert_tree (TREE_TYPE (field), val, location);
	      if (!TREE_CONSTANT (elt->value))
		is_constant = false;
	    }
	}
      else
	{
	  gcc_assert (TREE_CODE (type_tree) == RECORD_TYPE);
	  for (std::vector<tree>::const_iterator p = vals.begin ();
	       p != vals.end (); ++p, field = DECL_CHAIN (field))
	    {
	      gcc_assert (field != NULL_TREE);
	      tree val = (*p);
	      if (TREE_TYPE (field) == error_mark_node || error_operand_p (val))
		return error_mark_node;

	      if (int_size_in_bytes (TREE_TYPE (field)) == 0)
		{
		  // GIMPLE cannot represent indices of zero-sized types so
		  // trying to construct a map with zero-sized keys might lead
		  // to errors.  Instead, we evaluate each expression that
		  // would have been added as a map element for its
		  // side-effects and construct an empty map.
		  append_to_statement_list (val, &sink);
		  continue;
		}

	      constructor_elt empty = {NULL, NULL};
	      constructor_elt *elt = init->quick_push (empty);
	      elt->index = field;
	      elt->value = convert_tree (TREE_TYPE (field), val, location);
	      if (!TREE_CONSTANT (elt->value))
		is_constant = false;
	    }
	  // gcc_assert (field == NULL_TREE);
	}
    }

  tree ret = build_constructor (type_tree, init);
  if (is_constant)
    TREE_CONSTANT (ret) = 1;
  if (sink != NULL_TREE)
    ret = fold_build2_loc (location, COMPOUND_EXPR, type_tree, sink, ret);
  return ret;
}

tree
array_constructor_expression (tree type_tree,
			      const std::vector<unsigned long> &indexes,
			      const std::vector<tree> &vals,
			      location_t location)
{
  if (error_operand_p (type_tree))
    return error_mark_node;

  gcc_assert (indexes.size () == vals.size ());

  tree element_type = TREE_TYPE (type_tree);
  HOST_WIDE_INT element_size = int_size_in_bytes (element_type);
  vec<constructor_elt, va_gc> *init;
  vec_alloc (init, element_size == 0 ? 0 : vals.size ());

  tree sink = NULL_TREE;
  bool is_constant = true;
  for (size_t i = 0; i < vals.size (); ++i)
    {
      tree index = size_int (indexes[i]);
      tree val = vals[i];

      if (error_operand_p (index) || error_operand_p (val))
	return error_mark_node;

      if (element_size == 0)
	{
	  // GIMPLE cannot represent arrays of zero-sized types so trying
	  // to construct an array of zero-sized values might lead to errors.
	  // Instead, we evaluate each expression that would have been added as
	  // an array value for its side-effects and construct an empty array.
	  append_to_statement_list (val, &sink);
	  continue;
	}

      if (!TREE_CONSTANT (val))
	is_constant = false;

      constructor_elt empty = {NULL, NULL};
      constructor_elt *elt = init->quick_push (empty);
      elt->index = index;
      elt->value = val;
    }

  tree ret = build_constructor (type_tree, init);
  if (is_constant)
    TREE_CONSTANT (ret) = 1;
  if (sink != NULL_TREE)
    ret = fold_build2_loc (location, COMPOUND_EXPR, type_tree, sink, ret);
  return ret;
}

// Build insns to create an array, initialize all elements of the array to
// value, and return it
tree
array_initializer (tree fndecl, tree block, tree array_type, tree length,
		   tree value, tree *tmp, location_t locus)
{
  std::vector<tree> stmts;

  // Temporary array we initialize with the desired value.
  tree t = NULL_TREE;
  Bvariable *tmp_array = temporary_variable (fndecl, block, array_type,
					     NULL_TREE, true, locus, &t);
  tree arr = tmp_array->get_tree (locus);
  stmts.push_back (t);

  // Temporary for the array length used for initialization loop guard.
  Bvariable *tmp_len = temporary_variable (fndecl, block, size_type_node,
					   length, true, locus, &t);
  tree len = tmp_len->get_tree (locus);
  stmts.push_back (t);

  // Temporary variable for pointer used to initialize elements.
  tree ptr_type = pointer_type (TREE_TYPE (array_type));
  tree ptr_init
    = build1_loc (locus, ADDR_EXPR, ptr_type,
		  array_index_expression (arr, integer_zero_node, locus));
  Bvariable *tmp_ptr
    = temporary_variable (fndecl, block, ptr_type, ptr_init, false, locus, &t);
  tree ptr = tmp_ptr->get_tree (locus);
  stmts.push_back (t);

  // push statement list for the loop
  std::vector<tree> loop_stmts;

  // Loop exit condition:
  //   if (length == 0) break;
  t = comparison_expression (ComparisonOperator::EQUAL, len,
			     zero_expression (TREE_TYPE (len)), locus);

  t = exit_expression (t, locus);
  loop_stmts.push_back (t);

  // Assign value to the current pointer position
  //   *ptr = value;
  t = assignment_statement (build_fold_indirect_ref (ptr), value, locus);
  loop_stmts.push_back (t);

  // Move pointer to next element
  //   ptr++;
  tree size = TYPE_SIZE_UNIT (TREE_TYPE (ptr_type));
  t = build2 (POSTINCREMENT_EXPR, ptr_type, ptr, convert (ptr_type, size));
  loop_stmts.push_back (t);

  // Decrement loop counter.
  //   length--;
  t = build2 (POSTDECREMENT_EXPR, TREE_TYPE (len), len,
	      convert (TREE_TYPE (len), integer_one_node));
  loop_stmts.push_back (t);

  // pop statments and finish loop
  tree loop_body = statement_list (loop_stmts);
  stmts.push_back (loop_expression (loop_body, locus));

  // Return the temporary in the provided pointer and the statement list which
  // initializes it.
  *tmp = tmp_array->get_tree (locus);
  return statement_list (stmts);
}

// Return an expression representing ARRAY[INDEX]

tree
array_index_expression (tree array_tree, tree index_tree, location_t location)
{
  if (error_operand_p (array_tree) || error_operand_p (index_tree))
    return error_mark_node;

  // A function call that returns a zero sized object will have been
  // changed to return void.  If we see void here, assume we are
  // dealing with a zero sized type and just evaluate the operands.
  tree ret;
  if (TREE_TYPE (array_tree) != void_type_node)
    ret = build4_loc (location, ARRAY_REF, TREE_TYPE (TREE_TYPE (array_tree)),
		      array_tree, index_tree, NULL_TREE, NULL_TREE);
  else
    ret = fold_build2_loc (location, COMPOUND_EXPR, void_type_node, array_tree,
			   index_tree);

  return ret;
}

// Create an expression for a call to FN_EXPR with FN_ARGS.
tree
call_expression (tree fn, const std::vector<tree> &fn_args, tree chain_expr,
		 location_t location)
{
  if (error_operand_p (fn))
    return error_mark_node;

  gcc_assert (FUNCTION_POINTER_TYPE_P (TREE_TYPE (fn)));
  tree rettype = TREE_TYPE (TREE_TYPE (TREE_TYPE (fn)));

  size_t nargs = fn_args.size ();
  tree *args = nargs == 0 ? NULL : new tree[nargs];
  for (size_t i = 0; i < nargs; ++i)
    {
      args[i] = fn_args.at (i);
    }

  tree fndecl = fn;
  if (TREE_CODE (fndecl) == ADDR_EXPR)
    fndecl = TREE_OPERAND (fndecl, 0);

  // This is to support builtin math functions when using 80387 math.
  tree excess_type = NULL_TREE;
  if (optimize && TREE_CODE (fndecl) == FUNCTION_DECL
      && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL)
      && DECL_IS_UNDECLARED_BUILTIN (fndecl) && nargs > 0
      && ((SCALAR_FLOAT_TYPE_P (rettype)
	   && SCALAR_FLOAT_TYPE_P (TREE_TYPE (args[0])))
	  || (COMPLEX_FLOAT_TYPE_P (rettype)
	      && COMPLEX_FLOAT_TYPE_P (TREE_TYPE (args[0])))))
    {
      excess_type = excess_precision_type (TREE_TYPE (args[0]));
      if (excess_type != NULL_TREE)
	{
	  tree excess_fndecl
	    = mathfn_built_in (excess_type, DECL_FUNCTION_CODE (fndecl));
	  if (excess_fndecl == NULL_TREE)
	    excess_type = NULL_TREE;
	  else
	    {
	      fn = build_fold_addr_expr_loc (location, excess_fndecl);
	      for (size_t i = 0; i < nargs; ++i)
		{
		  if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (args[i]))
		      || COMPLEX_FLOAT_TYPE_P (TREE_TYPE (args[i])))
		    args[i] = ::convert (excess_type, args[i]);
		}
	    }
	}
    }

  tree ret
    = build_call_array_loc (location,
			    excess_type != NULL_TREE ? excess_type : rettype,
			    fn, nargs, args);

  // check for deprecated function usage
  if (fndecl && TREE_DEPRECATED (fndecl))
    {
      // set up the call-site information for `warn_deprecated_use`
      input_location = location;
      warn_deprecated_use (fndecl, NULL_TREE);
    }

  if (chain_expr)
    CALL_EXPR_STATIC_CHAIN (ret) = chain_expr;

  if (excess_type != NULL_TREE)
    {
      // Calling convert here can undo our excess precision change.
      // That may or may not be a bug in convert_to_real.
      ret = build1_loc (location, NOP_EXPR, rettype, ret);
    }

  delete[] args;
  return ret;
}

// Variable initialization.

tree
init_statement (tree, Bvariable *var, tree init_tree)
{
  tree var_tree = var->get_decl ();
  if (error_operand_p (var_tree) || error_operand_p (init_tree))
    return error_mark_node;
  gcc_assert (TREE_CODE (var_tree) == VAR_DECL);

  // To avoid problems with GNU ld, we don't make zero-sized
  // externally visible variables.  That might lead us to doing an
  // initialization of a zero-sized expression to a non-zero sized
  // variable, or vice-versa.  Avoid crashes by omitting the
  // initializer.  Such initializations don't mean anything anyhow.
  if (int_size_in_bytes (TREE_TYPE (var_tree)) != 0 && init_tree != NULL_TREE
      && TREE_TYPE (init_tree) != void_type_node
      && int_size_in_bytes (TREE_TYPE (init_tree)) != 0)
    {
      DECL_INITIAL (var_tree) = init_tree;
      init_tree = NULL_TREE;
    }

  tree ret = build1_loc (DECL_SOURCE_LOCATION (var_tree), DECL_EXPR,
			 void_type_node, var_tree);
  if (init_tree != NULL_TREE)
    ret = build2_loc (DECL_SOURCE_LOCATION (var_tree), COMPOUND_EXPR,
		      void_type_node, init_tree, ret);

  return ret;
}

// Assignment.

tree
assignment_statement (tree lhs, tree rhs, location_t location)
{
  if (error_operand_p (lhs) || error_operand_p (rhs))
    return error_mark_node;

  // To avoid problems with GNU ld, we don't make zero-sized
  // externally visible variables.  That might lead us to doing an
  // assignment of a zero-sized expression to a non-zero sized
  // expression; avoid crashes here by avoiding assignments of
  // zero-sized expressions.  Such assignments don't really mean
  // anything anyhow.
  if (TREE_TYPE (lhs) == void_type_node
      || int_size_in_bytes (TREE_TYPE (lhs)) == 0
      || TREE_TYPE (rhs) == void_type_node
      || int_size_in_bytes (TREE_TYPE (rhs)) == 0)
    return compound_statement (lhs, rhs);

  rhs = convert_tree (TREE_TYPE (lhs), rhs, location);

  return fold_build2_loc (location, MODIFY_EXPR, void_type_node, lhs, rhs);
}

// Return.

tree
return_statement (tree fntree, tree val, location_t location)
{
  if (error_operand_p (fntree))
    return error_mark_node;

  tree result = DECL_RESULT (fntree);
  if (error_operand_p (result))
    return error_mark_node;

  if (error_operand_p (val))
    return error_mark_node;

  tree set
    = fold_build2_loc (location, MODIFY_EXPR, void_type_node, result, val);
  return fold_build1_loc (location, RETURN_EXPR, void_type_node, set);
}

// Create a statement that attempts to execute BSTAT and calls EXCEPT_STMT if an
// error occurs.  EXCEPT_STMT may be NULL.  FINALLY_STMT may be NULL and if not
// NULL, it will always be executed.  This is used for handling defers in Rust
// functions.  In C++, the resulting code is of this form:
//   try { BSTAT; } catch { EXCEPT_STMT; } finally { FINALLY_STMT; }

tree
exception_handler_statement (tree try_stmt, tree except_stmt, tree finally_stmt,
			     location_t location)
{
  if (error_operand_p (try_stmt) || error_operand_p (except_stmt)
      || error_operand_p (finally_stmt))
    return error_mark_node;

  if (except_stmt != NULL_TREE)
    try_stmt = build2_loc (location, TRY_CATCH_EXPR, void_type_node, try_stmt,
			   build2_loc (location, CATCH_EXPR, void_type_node,
				       NULL, except_stmt));
  if (finally_stmt != NULL_TREE)
    try_stmt = build2_loc (location, TRY_FINALLY_EXPR, void_type_node, try_stmt,
			   finally_stmt);
  return try_stmt;
}

// If.

tree
if_statement (tree, tree cond_tree, tree then_tree, tree else_tree,
	      location_t location)
{
  if (error_operand_p (cond_tree) || error_operand_p (then_tree)
      || error_operand_p (else_tree))
    return error_mark_node;
  tree ret = build3_loc (location, COND_EXPR, void_type_node, cond_tree,
			 then_tree, else_tree);
  return ret;
}

// Loops

tree
loop_expression (tree body, location_t locus)
{
  return fold_build1_loc (locus, LOOP_EXPR, void_type_node, body);
}

tree
exit_expression (tree cond_tree, location_t locus)
{
  return fold_build1_loc (locus, EXIT_EXPR, void_type_node, cond_tree);
}

// Pair of statements.

tree
compound_statement (tree s1, tree s2)
{
  if (error_operand_p (s1) || error_operand_p (s2))
    return error_mark_node;

  tree stmt_list = NULL_TREE;
  append_to_statement_list (s1, &stmt_list);
  append_to_statement_list (s2, &stmt_list);

  // If neither statement has any side effects, stmt_list can be NULL
  // at this point.
  if (stmt_list == NULL_TREE)
    stmt_list = integer_zero_node;

  return stmt_list;
}

// List of statements.

tree
statement_list (const std::vector<tree> &statements)
{
  tree stmt_list = NULL_TREE;
  for (tree t : statements)
    {
      if (error_operand_p (t))
	return error_mark_node;
      append_to_statement_list (t, &stmt_list);
    }
  return stmt_list;
}

// Make a block.  For some reason gcc uses a dual structure for
// blocks: BLOCK tree nodes and BIND_EXPR tree nodes.  Since the
// BIND_EXPR node points to the BLOCK node, we store the BIND_EXPR in
// the Bblock.

tree
block (tree fndecl, tree enclosing, const std::vector<Bvariable *> &vars,
       location_t start_location, location_t)
{
  tree block_tree = make_node (BLOCK);
  if (enclosing == NULL)
    {
      gcc_assert (fndecl != NULL_TREE);

      // We may have already created a block for local variables when
      // we take the address of a parameter.
      if (DECL_INITIAL (fndecl) == NULL_TREE)
	{
	  BLOCK_SUPERCONTEXT (block_tree) = fndecl;
	  DECL_INITIAL (fndecl) = block_tree;
	}
      else
	{
	  tree superblock_tree = DECL_INITIAL (fndecl);
	  BLOCK_SUPERCONTEXT (block_tree) = superblock_tree;
	  tree *pp;
	  for (pp = &BLOCK_SUBBLOCKS (superblock_tree); *pp != NULL_TREE;
	       pp = &BLOCK_CHAIN (*pp))
	    ;
	  *pp = block_tree;
	}
    }
  else
    {
      tree superblock_tree = BIND_EXPR_BLOCK (enclosing);
      gcc_assert (TREE_CODE (superblock_tree) == BLOCK);

      BLOCK_SUPERCONTEXT (block_tree) = superblock_tree;
      tree *pp;
      for (pp = &BLOCK_SUBBLOCKS (superblock_tree); *pp != NULL_TREE;
	   pp = &BLOCK_CHAIN (*pp))
	;
      *pp = block_tree;
    }

  // Chain the variables of the scope together so they are all connected
  // to the block.
  tree *pp = &BLOCK_VARS (block_tree);
  for (Bvariable *bv : vars)
    {
      *pp = bv->get_decl ();
      if (!error_operand_p (*pp))
	pp = &DECL_CHAIN (*pp);
    }
  *pp = NULL_TREE;

  TREE_USED (block_tree) = 1;

  tree bind_tree = build3_loc (start_location, BIND_EXPR, void_type_node,
			       BLOCK_VARS (block_tree), NULL_TREE, block_tree);
  TREE_SIDE_EFFECTS (bind_tree) = 1;
  return bind_tree;
}

// Add statements to a block.

void
block_add_statements (tree bind_tree, const std::vector<tree> &statements)
{
  tree stmt_list = NULL_TREE;
  for (tree s : statements)
    {
      if (!error_operand_p (s))
	append_to_statement_list (s, &stmt_list);
    }

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  BIND_EXPR_BODY (bind_tree) = stmt_list;
}

// This is not static because we declare it with GTY(()) in rust-c.h.
tree rust_non_zero_struct;

// Return a type corresponding to TYPE with non-zero size.

tree
non_zero_size_type (tree type)
{
  if (int_size_in_bytes (type) != 0)
    return type;

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      if (TYPE_FIELDS (type) != NULL_TREE)
	{
	  tree ns = make_node (RECORD_TYPE);
	  tree field_trees = NULL_TREE;
	  tree *pp = &field_trees;
	  for (tree field = TYPE_FIELDS (type); field != NULL_TREE;
	       field = DECL_CHAIN (field))
	    {
	      tree ft = TREE_TYPE (field);
	      if (field == TYPE_FIELDS (type))
		ft = non_zero_size_type (ft);
	      tree f = build_decl (DECL_SOURCE_LOCATION (field), FIELD_DECL,
				   DECL_NAME (field), ft);
	      DECL_CONTEXT (f) = ns;
	      *pp = f;
	      pp = &DECL_CHAIN (f);
	    }
	  TYPE_FIELDS (ns) = field_trees;
	  layout_type (ns);
	  return ns;
	}

      if (rust_non_zero_struct == NULL_TREE)
	{
	  type = make_node (RECORD_TYPE);
	  tree field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
				   get_identifier ("dummy"), boolean_type_node);
	  DECL_CONTEXT (field) = type;
	  TYPE_FIELDS (type) = field;
	  layout_type (type);
	  rust_non_zero_struct = type;
	}
      return rust_non_zero_struct;

      case ARRAY_TYPE: {
	tree element_type = non_zero_size_type (TREE_TYPE (type));
	return build_array_type_nelts (element_type, 1);
      }

    default:
      rust_unreachable ();
    }

  rust_unreachable ();
}

// Convert EXPR_TREE to TYPE_TREE.  Sometimes the same unnamed Rust type
// can be created multiple times and thus have multiple tree
// representations.  Make sure this does not confuse the middle-end.

tree
convert_tree (tree type_tree, tree expr_tree, location_t location)
{
  if (type_tree == TREE_TYPE (expr_tree))
    return expr_tree;

  if (error_operand_p (type_tree) || error_operand_p (expr_tree))
    return error_mark_node;

  if (POINTER_TYPE_P (type_tree) || INTEGRAL_TYPE_P (type_tree)
      || SCALAR_FLOAT_TYPE_P (type_tree) || COMPLEX_FLOAT_TYPE_P (type_tree))
    return fold_convert_loc (location, type_tree, expr_tree);
  else if (TREE_CODE (type_tree) == RECORD_TYPE
	   || TREE_CODE (type_tree) == UNION_TYPE
	   || TREE_CODE (type_tree) == ARRAY_TYPE)
    {
      gcc_assert (int_size_in_bytes (type_tree)
		  == int_size_in_bytes (TREE_TYPE (expr_tree)));
      if (TYPE_MAIN_VARIANT (type_tree)
	  == TYPE_MAIN_VARIANT (TREE_TYPE (expr_tree)))
	return fold_build1_loc (location, NOP_EXPR, type_tree, expr_tree);
      return fold_build1_loc (location, VIEW_CONVERT_EXPR, type_tree,
			      expr_tree);
    }

  rust_unreachable ();
}

// Make a global variable.

Bvariable *
global_variable (const std::string &var_name, const std::string &asm_name,
		 tree type_tree, bool is_external, bool is_hidden,
		 bool in_unique_section, location_t location)
{
  if (error_operand_p (type_tree))
    return Bvariable::error_variable ();

  // The GNU linker does not like dynamic variables with zero size.
  tree orig_type_tree = type_tree;
  if ((is_external || !is_hidden) && int_size_in_bytes (type_tree) == 0)
    type_tree = non_zero_size_type (type_tree);

  tree decl = build_decl (location, VAR_DECL,
			  get_identifier_from_string (var_name), type_tree);
  if (is_external)
    DECL_EXTERNAL (decl) = 1;
  else
    TREE_STATIC (decl) = 1;
  if (!is_hidden)
    {
      TREE_PUBLIC (decl) = 1;
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier_from_string (asm_name));
    }
  else
    {
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier_from_string (asm_name));
    }

  TREE_USED (decl) = 1;

  if (in_unique_section)
    resolve_unique_section (decl, 0, 1);

  rust_preserve_from_gc (decl);

  return new Bvariable (decl, orig_type_tree);
}

// Set the initial value of a global variable.

void
global_variable_set_init (Bvariable *var, tree expr_tree)
{
  if (error_operand_p (expr_tree))
    return;
  gcc_assert (TREE_CONSTANT (expr_tree));
  tree var_decl = var->get_decl ();
  if (error_operand_p (var_decl))
    return;
  DECL_INITIAL (var_decl) = expr_tree;

  // If this variable goes in a unique section, it may need to go into
  // a different one now that DECL_INITIAL is set.
  if (symtab_node::get (var_decl)
      && symtab_node::get (var_decl)->implicit_section)
    {
      set_decl_section_name (var_decl, (const char *) NULL);
      resolve_unique_section (var_decl, compute_reloc_for_constant (expr_tree),
			      1);
    }
}

// Make a local variable.

Bvariable *
local_variable (tree function, const std::string &name, tree type_tree,
		Bvariable *decl_var, location_t location)
{
  if (error_operand_p (type_tree))
    return Bvariable::error_variable ();
  tree decl = build_decl (location, VAR_DECL, get_identifier_from_string (name),
			  type_tree);
  DECL_CONTEXT (decl) = function;

  if (decl_var != NULL)
    {
      DECL_HAS_VALUE_EXPR_P (decl) = 1;
      SET_DECL_VALUE_EXPR (decl, decl_var->get_decl ());
    }
  rust_preserve_from_gc (decl);
  return new Bvariable (decl);
}

// Make a function parameter variable.

Bvariable *
parameter_variable (tree function, const std::string &name, tree type_tree,
		    location_t location)
{
  if (error_operand_p (type_tree))
    return Bvariable::error_variable ();
  tree decl = build_decl (location, PARM_DECL,
			  get_identifier_from_string (name), type_tree);
  DECL_CONTEXT (decl) = function;
  DECL_ARG_TYPE (decl) = type_tree;

  rust_preserve_from_gc (decl);
  return new Bvariable (decl);
}

// Make a static chain variable.

Bvariable *
static_chain_variable (tree fndecl, const std::string &name, tree type_tree,
		       location_t location)
{
  if (error_operand_p (type_tree))
    return Bvariable::error_variable ();
  tree decl = build_decl (location, PARM_DECL,
			  get_identifier_from_string (name), type_tree);
  DECL_CONTEXT (decl) = fndecl;
  DECL_ARG_TYPE (decl) = type_tree;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;

  struct function *f = DECL_STRUCT_FUNCTION (fndecl);
  if (f == NULL)
    {
      push_struct_function (fndecl);
      pop_cfun ();
      f = DECL_STRUCT_FUNCTION (fndecl);
    }
  gcc_assert (f->static_chain_decl == NULL);
  f->static_chain_decl = decl;
  DECL_STATIC_CHAIN (fndecl) = 1;

  rust_preserve_from_gc (decl);
  return new Bvariable (decl);
}

// Make a temporary variable.

Bvariable *
temporary_variable (tree fndecl, tree bind_tree, tree type_tree, tree init_tree,
		    bool is_address_taken, location_t location,
		    tree *pstatement)
{
  gcc_assert (fndecl != NULL_TREE);
  if (error_operand_p (type_tree) || error_operand_p (init_tree)
      || error_operand_p (fndecl))
    {
      *pstatement = error_mark_node;
      return Bvariable::error_variable ();
    }

  tree var;
  // We can only use create_tmp_var if the type is not addressable.
  if (!TREE_ADDRESSABLE (type_tree))
    {
      if (DECL_STRUCT_FUNCTION (fndecl) == NULL)
	push_struct_function (fndecl);
      else
	push_cfun (DECL_STRUCT_FUNCTION (fndecl));

      var = create_tmp_var (type_tree, "RUSTTMP");
      pop_cfun ();
    }
  else
    {
      gcc_assert (bind_tree != NULL_TREE);
      var = build_decl (location, VAR_DECL, create_tmp_var_name ("RUSTTMP"),
			type_tree);
      DECL_ARTIFICIAL (var) = 1;
      DECL_IGNORED_P (var) = 1;
      TREE_USED (var) = 1;
      DECL_CONTEXT (var) = fndecl;

      // We have to add this variable to the BLOCK and the BIND_EXPR.
      gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
      tree block_tree = BIND_EXPR_BLOCK (bind_tree);
      gcc_assert (TREE_CODE (block_tree) == BLOCK);
      DECL_CHAIN (var) = BLOCK_VARS (block_tree);
      BLOCK_VARS (block_tree) = var;
      BIND_EXPR_VARS (bind_tree) = BLOCK_VARS (block_tree);
    }

  if (type_size (type_tree) != 0 && init_tree != NULL_TREE
      && TREE_TYPE (init_tree) != void_type_node)
    DECL_INITIAL (var) = convert_tree (type_tree, init_tree, location);

  if (is_address_taken)
    TREE_ADDRESSABLE (var) = 1;

  *pstatement = build1_loc (location, DECL_EXPR, void_type_node, var);

  // For a zero sized type, don't initialize VAR with BINIT, but still
  // evaluate BINIT for its side effects.
  if (init_tree != NULL_TREE
      && (type_size (type_tree) == 0
	  || TREE_TYPE (init_tree) == void_type_node))
    *pstatement = compound_statement (init_tree, *pstatement);

  return new Bvariable (var);
}

// Make a label.

tree
label (tree func_tree, const std::string &name, location_t location)
{
  tree decl;
  if (name.empty ())
    {
      if (DECL_STRUCT_FUNCTION (func_tree) == NULL)
	push_struct_function (func_tree);
      else
	push_cfun (DECL_STRUCT_FUNCTION (func_tree));

      decl = create_artificial_label (location);

      pop_cfun ();
    }
  else
    {
      tree id = get_identifier_from_string (name);
      decl = build_decl (location, LABEL_DECL, id, void_type_node);
      DECL_CONTEXT (decl) = func_tree;
    }
  return decl;
}

// Make a statement which defines a label.

tree
label_definition_statement (tree label)
{
  return fold_build1_loc (DECL_SOURCE_LOCATION (label), LABEL_EXPR,
			  void_type_node, label);
}

// Make a goto statement.

tree
goto_statement (tree label, location_t location)
{
  return fold_build1_loc (location, GOTO_EXPR, void_type_node, label);
}

// Get the address of a label.

tree
label_address (tree label, location_t location)
{
  TREE_USED (label) = 1;
  TREE_ADDRESSABLE (label) = 1;
  tree ret = fold_convert_loc (location, ptr_type_node,
			       build_fold_addr_expr_loc (location, label));
  return ret;
}

// Declare or define a new function.

tree
function (tree functype, const std::string &name, const std::string &asm_name,
	  unsigned int flags, location_t location)
{
  if (error_operand_p (functype))
    return error_mark_node;

  gcc_assert (FUNCTION_POINTER_TYPE_P (functype));
  functype = TREE_TYPE (functype);
  tree id = get_identifier_from_string (name);
  if (error_operand_p (id))
    return error_mark_node;

  tree decl = build_decl (location, FUNCTION_DECL, id, functype);
  if (!asm_name.empty ())
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier_from_string (asm_name));

  if ((flags & function_is_declaration) != 0)
    DECL_EXTERNAL (decl) = 1;
  else
    {
      tree restype = TREE_TYPE (functype);
      tree resdecl = build_decl (location, RESULT_DECL, NULL_TREE, restype);
      DECL_ARTIFICIAL (resdecl) = 1;
      DECL_IGNORED_P (resdecl) = 1;
      DECL_CONTEXT (resdecl) = decl;
      DECL_RESULT (decl) = resdecl;
    }
  if ((flags & function_is_uninlinable) != 0)
    DECL_UNINLINABLE (decl) = 1;
  if ((flags & function_does_not_return) != 0)
    TREE_THIS_VOLATILE (decl) = 1;
  if ((flags & function_in_unique_section) != 0)
    resolve_unique_section (decl, 0, 1);

  rust_preserve_from_gc (decl);
  return decl;
}

// Create a statement that runs all deferred calls for FUNCTION.  This should
// be a statement that looks like this in C++:
//   finish:
//     try { UNDEFER; } catch { CHECK_DEFER; goto finish; }

tree
function_defer_statement (tree function, tree undefer_tree, tree defer_tree,
			  location_t location)
{
  if (error_operand_p (undefer_tree) || error_operand_p (defer_tree)
      || error_operand_p (function))
    return error_mark_node;

  if (DECL_STRUCT_FUNCTION (function) == NULL)
    push_struct_function (function);
  else
    push_cfun (DECL_STRUCT_FUNCTION (function));

  tree stmt_list = NULL;
  tree label = Backend::label (function, "", location);
  tree label_def = label_definition_statement (label);
  append_to_statement_list (label_def, &stmt_list);

  tree jump_stmt = goto_statement (label, location);
  tree catch_body
    = build2 (COMPOUND_EXPR, void_type_node, defer_tree, jump_stmt);
  catch_body = build2 (CATCH_EXPR, void_type_node, NULL, catch_body);
  tree try_catch
    = build2 (TRY_CATCH_EXPR, void_type_node, undefer_tree, catch_body);
  append_to_statement_list (try_catch, &stmt_list);
  pop_cfun ();

  return stmt_list;
}

// Record PARAM_VARS as the variables to use for the parameters of FUNCTION.
// This will only be called for a function definition.

bool
function_set_parameters (tree function,
			 const std::vector<Bvariable *> &param_vars)
{
  if (error_operand_p (function))
    return false;

  tree params = NULL_TREE;
  tree *pp = &params;
  for (Bvariable *bv : param_vars)
    {
      *pp = bv->get_decl ();
      gcc_assert (!error_operand_p (*pp));
      pp = &DECL_CHAIN (*pp);
    }
  *pp = NULL_TREE;
  DECL_ARGUMENTS (function) = params;
  return true;
}

// Write the definitions for all TYPE_DECLS, CONSTANT_DECLS,
// FUNCTION_DECLS, and VARIABLE_DECLS declared globally, as well as
// emit early debugging information.

void
write_global_definitions (const std::vector<tree> &type_decls,
			  const std::vector<tree> &constant_decls,
			  const std::vector<tree> &function_decls,
			  const std::vector<Bvariable *> &variable_decls)
{
  size_t count_definitions = type_decls.size () + constant_decls.size ()
			     + function_decls.size () + variable_decls.size ();

  tree *defs = new tree[count_definitions];

  // Convert all non-erroneous declarations into Gimple form.
  size_t i = 0;
  for (Bvariable *bv : variable_decls)
    {
      tree v = bv->get_decl ();
      if (error_operand_p (v))
	continue;
      defs[i] = v;
      rust_preserve_from_gc (defs[i]);
      ++i;
    }

  for (tree type_tree : type_decls)
    {
      if (!error_operand_p (type_tree) && IS_TYPE_OR_DECL_P (type_tree))
	{
	  defs[i] = TYPE_NAME (type_tree);
	  gcc_assert (defs[i] != NULL);
	  rust_preserve_from_gc (defs[i]);
	  ++i;
	}
    }
  for (tree t : constant_decls)
    {
      if (!error_operand_p (t))
	{
	  defs[i] = t;
	  rust_preserve_from_gc (defs[i]);
	  ++i;
	}
    }
  for (tree decl : function_decls)
    {
      if (!error_operand_p (decl))
	{
	  rust_preserve_from_gc (decl);
	  if (DECL_STRUCT_FUNCTION (decl) == NULL)
	    allocate_struct_function (decl, false);
	  dump_function (TDI_original, decl);
	  cgraph_node::finalize_function (decl, true);

	  defs[i] = decl;
	  ++i;
	}
    }

  // Pass everything back to the middle-end.

  wrapup_global_declarations (defs, i);

  delete[] defs;
}

} // namespace Backend
