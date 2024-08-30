/* ACLE support for Arm MVE (function shapes)
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "arm-mve-builtins.h"
#include "arm-mve-builtins-shapes.h"

/* In the comments below, _t0 represents the first type suffix
   (e.g. "_s8") and _t1 represents the second.  T0/T1 represent the
   type full names (e.g. int8x16_t). Square brackets enclose
   characters that are present in only the full name, not the
   overloaded name.  Governing predicate arguments and predicate
   suffixes are not shown, since they depend on the predication type,
   which is a separate piece of information from the shape.  */

namespace arm_mve {

/* Return a representation of "const T *".  */
static tree
build_const_pointer (tree t)
{
  return build_pointer_type (build_qualified_type (t, TYPE_QUAL_CONST));
}

/* If INSTANCE has a predicate, add it to the list of argument types
   in ARGUMENT_TYPES.  RETURN_TYPE is the type returned by the
   function.  */
static void
apply_predication (const function_instance &instance, tree return_type,
		   vec<tree> &argument_types)
{
  if (instance.pred != PRED_none)
    {
      /* When predicate is PRED_m, insert a first argument
	 ("inactive") with the same type as return_type.  */
      if (instance.has_inactive_argument ())
	argument_types.quick_insert (0, return_type);
      argument_types.quick_push (get_mve_pred16_t ());
    }
}

/* Parse and move past an element type in FORMAT and return it as a type
   suffix.  The format is:

   [01]    - the element type in type suffix 0 or 1 of INSTANCE.
   h<elt>  - a half-sized version of <elt>
   p<elt>  - a poly type with the same width as <elt>
   s<bits> - a signed type with the given number of bits
   s[01]   - a signed type with the same width as type suffix 0 or 1
   u<bits> - an unsigned type with the given number of bits
   u[01]   - an unsigned type with the same width as type suffix 0 or 1
   U<elt>  - an unsigned type with the double width as <elt>
   w<elt>  - a double-sized version of <elt>
   x<bits> - a type with the given number of bits and same signedness
             as the next argument.

   Future intrinsics will extend this format.  */
static type_suffix_index
parse_element_type (const function_instance &instance, const char *&format)
{
  int ch = *format++;


  if (ch == 's' || ch == 'u')
    {
      type_class_index tclass = (ch == 's' ? TYPE_signed
				 : TYPE_unsigned);
      char *end;
      unsigned int bits = strtol (format, &end, 10);
      format = end;
      if (bits == 0 || bits == 1)
	bits = instance.type_suffix (bits).element_bits;
      return find_type_suffix (tclass, bits);
    }

  if (ch == 'h')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return find_type_suffix (type_suffixes[suffix].tclass,
			       type_suffixes[suffix].element_bits / 2);
    }

   if (ch == 'w')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return find_type_suffix (type_suffixes[suffix].tclass,
			       type_suffixes[suffix].element_bits * 2);
    }

   if (ch == 'U')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return find_type_suffix (TYPE_unsigned,
			       type_suffixes[suffix].element_bits * 2);
    }

   if (ch == 'p')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return find_type_suffix (TYPE_poly,
			       type_suffixes[suffix].element_bits);
    }

  if (ch == 'x')
    {
      const char *next = format;
      next = strstr (format, ",");
      next+=2;
      type_suffix_index suffix = parse_element_type (instance, next);
      type_class_index tclass = type_suffixes[suffix].tclass;
      char *end;
      unsigned int bits = strtol (format, &end, 10);
      format = end;
      return find_type_suffix (tclass, bits);
    }

  if (ch == '0' || ch == '1')
    return instance.type_suffix_ids[ch - '0'];

  gcc_unreachable ();
}

/* Read and return a type from FORMAT for function INSTANCE.  Advance
   FORMAT beyond the type string.  The format is:

   _       - void
   al      - array pointer for loads
   as      - array pointer for stores
   p       - predicates with type mve_pred16_t
   s<elt>  - a scalar type with the given element suffix
   t<elt>  - a vector or tuple type with given element suffix [*1]
   v<elt>  - a vector with the given element suffix

   where <elt> has the format described above parse_element_type.

   Future intrinsics will extend this format.

   [*1] the vectors_per_tuple function indicates whether the type should
        be a tuple, and if so, how many vectors it should contain.  */
static tree
parse_type (const function_instance &instance, const char *&format)
{
  int ch = *format++;


  if (ch == '_')
    return void_type_node;

  if (ch == 'a')
    {
      ch = *format++;
      if (ch == 'l')
	return build_const_pointer (instance.memory_scalar_type ());
      if (ch == 's') {
	return build_pointer_type (instance.memory_scalar_type ());
      }
      gcc_unreachable ();
    }

  if (ch == 'p')
    return get_mve_pred16_t ();

  if (ch == 's')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return scalar_types[type_suffixes[suffix].vector_type];
    }

  if (ch == 't')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      vector_type_index vector_type = type_suffixes[suffix].vector_type;
      unsigned int num_vectors = instance.vectors_per_tuple ();
      return acle_vector_types[num_vectors - 1][vector_type];
    }

  if (ch == 'v')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return acle_vector_types[0][type_suffixes[suffix].vector_type];
    }

  gcc_unreachable ();
}

/* Read a type signature for INSTANCE from FORMAT.  Add the argument
   types to ARGUMENT_TYPES and return the return type.  Assert there
   are no more than MAX_ARGS arguments.

   The format is a comma-separated list of types (as for parse_type),
   with the first type being the return type and the rest being the
   argument types.  */
static tree
parse_signature (const function_instance &instance, const char *format,
		 vec<tree> &argument_types, unsigned int max_args)
{
  tree return_type = parse_type (instance, format);
  unsigned int args = 0;
  while (format[0] == ',')
    {
      gcc_assert (args < max_args);
      format += 1;
      tree argument_type = parse_type (instance, format);
      argument_types.quick_push (argument_type);
      args += 1;
    }
  gcc_assert (format[0] == 0);
  return return_type;
}

/* Add one function instance for GROUP, using mode suffix MODE_SUFFIX_ID,
   the type suffixes at index TI and the predication suffix at index PI.
   The other arguments are as for build_all.  */
static void
build_one (function_builder &b, const char *signature,
	   const function_group_info &group, mode_suffix_index mode_suffix_id,
	   unsigned int ti, unsigned int pi, bool preserve_user_namespace,
	   bool force_direct_overloads)
{
  /* Current functions take at most five arguments.  Match
     parse_signature parameter below.  */
  auto_vec<tree, 5> argument_types;
  function_instance instance (group.base_name, *group.base, *group.shape,
			      mode_suffix_id, group.types[ti],
			      group.preds[pi]);
  tree return_type = parse_signature (instance, signature, argument_types, 5);
  apply_predication (instance, return_type, argument_types);
  b.add_unique_function (instance, return_type, argument_types,
			 preserve_user_namespace, group.requires_float,
			 force_direct_overloads);
}

/* Add a function instance for every type and predicate combination in
   GROUP, except if requested to use only the predicates listed in
   RESTRICT_TO_PREDS.  Take the function base name from GROUP and the
   mode suffix from MODE_SUFFIX_ID. Use SIGNATURE to construct the
   function signature, then use apply_predication to add in the
   predicate.  */
static void
build_all (function_builder &b, const char *signature,
	   const function_group_info &group, mode_suffix_index mode_suffix_id,
	   bool preserve_user_namespace,
	   bool force_direct_overloads = false,
	   const predication_index *restrict_to_preds = NULL)
{
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    {
      unsigned int pi2 = 0;

      if (restrict_to_preds)
	for (; restrict_to_preds[pi2] != NUM_PREDS; ++pi2)
	  if (restrict_to_preds[pi2] == group.preds[pi])
	    break;

      if (restrict_to_preds == NULL || restrict_to_preds[pi2] != NUM_PREDS)
	for (unsigned int ti = 0;
	     ti == 0 || group.types[ti][0] != NUM_TYPE_SUFFIXES; ++ti)
	  build_one (b, signature, group, mode_suffix_id, ti, pi,
		     preserve_user_namespace, force_direct_overloads);
    }
}

/* Add a function instance for every type and predicate combination in
   GROUP, except if requested to use only the predicates listed in
   RESTRICT_TO_PREDS, and only for 16-bit and 32-bit integers.  Take
   the function base name from GROUP and the mode suffix from
   MODE_SUFFIX_ID. Use SIGNATURE to construct the function signature,
   then use apply_predication to add in the predicate.  */
static void
build_16_32 (function_builder &b, const char *signature,
	     const function_group_info &group, mode_suffix_index mode_suffix_id,
	     bool preserve_user_namespace,
	     bool force_direct_overloads = false,
	     const predication_index *restrict_to_preds = NULL)
{
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    {
      unsigned int pi2 = 0;

      if (restrict_to_preds)
	for (; restrict_to_preds[pi2] != NUM_PREDS; ++pi2)
	  if (restrict_to_preds[pi2] == group.preds[pi])
	    break;

      if (restrict_to_preds == NULL || restrict_to_preds[pi2] != NUM_PREDS)
	for (unsigned int ti = 0;
	     ti == 0 || group.types[ti][0] != NUM_TYPE_SUFFIXES; ++ti)
	  {
	    unsigned int element_bits = type_suffixes[group.types[ti][0]].element_bits;
	    type_class_index tclass = type_suffixes[group.types[ti][0]].tclass;
	    if ((tclass == TYPE_signed || tclass == TYPE_unsigned)
		&& (element_bits == 16 || element_bits == 32))
	      build_one (b, signature, group, mode_suffix_id, ti, pi,
			 preserve_user_namespace, force_direct_overloads);
	  }
    }
}

/* TYPE is the largest type suffix associated with the arguments of R, but the
   result is twice as wide.  Return the associated type suffix of
   EXPECTED_TCLASS if it exists, otherwise report an appropriate error and
   return NUM_TYPE_SUFFIXES.  */
static type_suffix_index
long_type_suffix (function_resolver &r,
		  type_suffix_index type,
		  type_class_index expected_tclass)
{
  unsigned int element_bits = type_suffixes[type].element_bits;
  if (expected_tclass == function_resolver::SAME_TYPE_CLASS)
    expected_tclass = type_suffixes[type].tclass;

  if (type_suffixes[type].integer_p && element_bits < 64)
    return find_type_suffix (expected_tclass, element_bits * 2);

  r.report_no_such_form (type);
  return NUM_TYPE_SUFFIXES;
}

/* Return the type suffix half as wide as TYPE with EXPECTED_TCLASS if it
   exists, otherwise report an appropriate error and return
   NUM_TYPE_SUFFIXES.  */
static type_suffix_index
half_type_suffix (function_resolver &r,
		  type_suffix_index type,
		  type_class_index expected_tclass)
{
  unsigned int element_bits = type_suffixes[type].element_bits;
  if (expected_tclass == function_resolver::SAME_TYPE_CLASS)
    expected_tclass = type_suffixes[type].tclass;

  if (type_suffixes[type].integer_p && element_bits > 8)
    return find_type_suffix (expected_tclass, element_bits / 2);

  r.report_no_such_form (type);
  return NUM_TYPE_SUFFIXES;
}

/* Declare the function shape NAME, pointing it to an instance
   of class <NAME>_def.  */
#define SHAPE(NAME) \
  static CONSTEXPR const NAME##_def NAME##_obj; \
  namespace shapes { const function_shape *const NAME = &NAME##_obj; }

/* Base class for functions that are not overloaded.  */
struct nonoverloaded_base : public function_shape
{
  bool
  explicit_type_suffix_p (unsigned int, enum predication_index,
			  enum mode_suffix_index, type_suffix_info) const override
  {
    return true;
  }

  bool
  explicit_mode_suffix_p (enum predication_index, enum mode_suffix_index) const override
  {
    return true;
  }

  bool
  skip_overload_p (enum predication_index, enum mode_suffix_index) const override
  {
    return false;
  }

  tree
  resolve (function_resolver &) const override
  {
    gcc_unreachable ();
  }
};

/* Base class for overloaded functions.  Bit N of EXPLICIT_MASK is true
   if type suffix N appears in the overloaded name.  */
template<unsigned int EXPLICIT_MASK>
struct overloaded_base : public function_shape
{
  bool
  explicit_type_suffix_p (unsigned int i, enum predication_index,
			  enum mode_suffix_index, type_suffix_info) const override
  {
    return (EXPLICIT_MASK >> i) & 1;
  }

  bool
  explicit_mode_suffix_p (enum predication_index, enum mode_suffix_index) const override
  {
    return false;
  }

  bool
  skip_overload_p (enum predication_index, enum mode_suffix_index) const override
  {
    return false;
  }
};

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t)

   i.e. the standard shape for binary operations that operate on
   uniform types.

   Example: vandq.
   int8x16_t [__arm_]vandq[_s8](int8x16_t a, int8x16_t b)
   int8x16_t [__arm_]vandq_m[_s8](int8x16_t inactive, int8x16_t a, int8x16_t b, mve_pred16_t p)
   int8x16_t [__arm_]vandq_x[_s8](int8x16_t a, int8x16_t b, mve_pred16_t p)  */
struct binary_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2);
  }
};
SHAPE (binary)

/* <[u]int32>_t vfoo[_<t0>](<T0>_t, <T0>_t)

   i.e. the shape for binary operations that operate on a pair of
   vectors and produce an int32_t or an uint32_t depending on the
   signedness of the input elements.

   Example: vmladavq.
   int32_t [__arm_]vmladavq[_s16](int16x8_t m1, int16x8_t m2)
   int32_t [__arm_]vmladavq_p[_s16](int16x8_t m1, int16x8_t m2, mve_pred16_t p)  */
struct binary_acc_int32_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sx32,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2);
  }
};
SHAPE (binary_acc_int32)

/* <[u]int64>_t vfoo[_<t0>](<T0>_t, <T0>_t)

   Example: vmlaldavq.
   int64_t [__arm_]vmlaldavq[_s16](int16x8_t m1, int16x8_t m2)
   int64_t [__arm_]vmlaldavq_p[_s16](int16x8_t m1, int16x8_t m2, mve_pred16_t p)  */
struct binary_acc_int64_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sx64,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2);
  }
};
SHAPE (binary_acc_int64)

/* <[u]int32>_t vfoo[_<t0>]([u]int32_t, <T0>_t, <T0>_t)

   Example: vmladavaq.
   int32_t [__arm_]vmladavaq[_s16](int32_t add, int16x8_t m1, int16x8_t m2)
   int32_t [__arm_]vmladavaq_p[_s16](int32_t add, int16x8_t m1, int16x8_t m2, mve_pred16_t p)  */
struct binary_acca_int32_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sx32,sx32,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    const char *first_type_name;

    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    first_type_name = (type_suffixes[type].unsigned_p
		       ? "uint32_t"
		       : "int32_t");
    if (!r.require_scalar_type (0, first_type_name))
      return error_mark_node;

    unsigned int last_arg = i + 1;
    for (i = 1; i < last_arg; i++)
      if (!r.require_matching_vector_type (i, type))
	return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_acca_int32)

/* [u]int64_t vfoo[_<t0>]([u]int64_t, <T0>_t, <T0>_t)

   Example: vmlaldavaq.
   int64_t [__arm_]vmlaldavaq[_s16](int64_t add, int16x8_t m1, int16x8_t m2)
   int64_t [__arm_]vmlaldavaq_p[_s16](int64_t add, int16x8_t m1, int16x8_t m2, mve_pred16_t p)  */
struct binary_acca_int64_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sx64,sx64,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    const char *first_type_name;

    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;


    first_type_name = (type_suffixes[type].unsigned_p
		       ? "uint64_t"
		       : "int64_t");
    if (!r.require_scalar_type (0, first_type_name))
      return error_mark_node;

    unsigned int last_arg = i + 1;
    for (i = 1; i < last_arg; i++)
      if (!r.require_matching_vector_type (i, type))
	return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_acca_int64)

/* <T0>_t vfoo[_n_t0](<T0>_t, int32_t)

   i.e. the shape for binary operations that operate on
   a vector and an int32_t.

   Example: vbrsrq.
   int16x8_t [__arm_]vbrsrq[_n_s16](int16x8_t a, int32_t b)
   int16x8_t [__arm_]vbrsrq_m[_n_s16](int16x8_t inactive, int16x8_t a, int32_t b, mve_pred16_t p)
   int16x8_t [__arm_]vbrsrq_x[_n_s16](int16x8_t a, int32_t b, mve_pred16_t p)  */
struct binary_imm32_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,ss32", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1, 1);
  }
};
SHAPE (binary_imm32)

/* <T0>_t vfoo[_n_t0](<T0>_t, const int)

   Shape for vector shift right operations that take a vector first
   argument and an integer, and produce a vector.

   Check that 'imm' is in the [1..#bits] range.

   Example: vrshrq.
   int8x16_t [__arm_]vrshrq[_n_s8](int8x16_t a, const int imm)
   int8x16_t [__arm_]vrshrq_m[_n_s8](int8x16_t inactive, int8x16_t a, const int imm, mve_pred16_t p)
   int8x16_t [__arm_]vrshrq_x[_n_s8](int8x16_t a, const int imm, mve_pred16_t p)  */
struct binary_rshift_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,ss32", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1, 1);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (1, 1, bits);
  }
};
SHAPE (binary_rshift)


/* <uT0>_t vfoo[_n_t0](<T0>_t, int)

   Shape for vector saturating shift left operations that take a
   vector of signed elements as first argument and an integer, and
   produce a vector of unsigned elements.

   Check that 'imm' is in the [0..#bits-1] range.

   Example: vqshluq.
   uint16x8_t [__arm_]vqshluq[_n_s16](int16x8_t a, const int imm)
   uint16x8_t [__arm_]vqshluq_m[_n_s16](uint16x8_t inactive, int16x8_t a, const int imm, mve_pred16_t p)  */
struct binary_lshift_unsigned_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "vu0,vs0,su64", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_integer_immediate (i)
	|| (type = r.infer_vector_type (i-1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    if (r.pred == PRED_m)
      {
	/* With PRED_m, check that the 'inactive' first argument has
	   the expeected unsigned type.  */
	type_suffix_index return_type
	  = find_type_suffix (TYPE_unsigned, type_suffixes[type].element_bits);

	if (!r.require_matching_vector_type (0, return_type))
	  return error_mark_node;
      }

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (1, 0, bits - 1);
  }

};
SHAPE (binary_lshift_unsigned)

/* <uT0>_t vfoo[_t0](<uT0>_t, <T0>_t)

   i.e. binary operations that take a vector of unsigned elements as first argument and a
   vector of signed elements as second argument, and produce a vector of unsigned elements.

   Example: vminaq.
   uint8x16_t [__arm_]vminaq[_s8](uint8x16_t a, int8x16_t b)
   uint8x16_t [__arm_]vminaq_m[_s8](uint8x16_t a, int8x16_t b, mve_pred16_t p)  */
struct binary_maxamina_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vu0,vu0,vs0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    /* Check that the first argument has the expeected unsigned
       type.  */
    type_suffix_index return_type
      = find_type_suffix (TYPE_unsigned, type_suffixes[type].element_bits);
    if (!r.require_matching_vector_type (0, return_type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_maxamina)

/* <uS0>_t vfoo[_<t0>](<uS0>_t, <T0>_t)

   Example: vmaxavq.
   uint8_t [__arm_]vmaxavq[_s8](uint8_t a, int8x16_t b)
   uint8_t [__arm_]vmaxavq_p[_s8](uint8_t a, int8x16_t b, mve_pred16_t p)  */
struct binary_maxavminav_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "su0,su0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_derived_scalar_type (0, TYPE_unsigned)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_maxavminav)

/* <S0>_t vfoo[_<t0>](<S0>_t, <T0>_t)

   Example: vmaxvq.
   int8_t [__arm_]vmaxvq[_s8](int8_t a, int8x16_t b)
   int8_t [__arm_]vmaxvq_p[_s8](int8_t a, int8x16_t b, mve_pred16_t p)  */
struct binary_maxvminv_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "s0,s0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_derived_scalar_type (0, r.SAME_TYPE_CLASS)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_maxvminv)

/* <T0:half>_t vfoo[_t0](<T0:half>_t, <T0>_t)

   Example: vmovnbq.
   int8x16_t [__arm_]vmovnbq[_s16](int8x16_t a, int16x8_t b)
   int8x16_t [__arm_]vmovnbq_m[_s16](int8x16_t a, int16x8_t b, mve_pred16_t p)  */
struct binary_move_narrow_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vh0,vh0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, narrow_suffix;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES
	|| ((narrow_suffix = half_type_suffix (r, type, r.SAME_TYPE_CLASS))
	    == NUM_TYPE_SUFFIXES))
      return error_mark_node;

    if (!r.require_matching_vector_type (0, narrow_suffix))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_move_narrow)

/* <uT0:half>_t vfoo[_t0](<uT0:half>_t, <T0>_t)

   Example: vqmovunbq.
   uint8x16_t [__arm_]vqmovunbq[_s16](uint8x16_t a, int16x8_t b)
   uint8x16_t [__arm_]vqmovunbq_m[_s16](uint8x16_t a, int16x8_t b, mve_pred16_t p)  */
struct binary_move_narrow_unsigned_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vhu0,vhu0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, narrow_suffix;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES
	|| ((narrow_suffix = half_type_suffix (r, type, TYPE_unsigned))
	    == NUM_TYPE_SUFFIXES))
      return error_mark_node;

    if (!r.require_matching_vector_type (0, narrow_suffix))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_move_narrow_unsigned)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t)
   <T0>_t vfoo[_n_t0](<T0>_t, <S0>_t)

   i.e. the standard shape for binary operations that operate on
   uniform types.

   Example: vaddq.
   int8x16_t [__arm_]vaddq[_s8](int8x16_t a, int8x16_t b)
   int8x16_t [__arm_]vaddq[_n_s8](int8x16_t a, int8_t b)
   int8x16_t [__arm_]vaddq_m[_s8](int8x16_t inactive, int8x16_t a, int8x16_t b, mve_pred16_t p)
   int8x16_t [__arm_]vaddq_m[_n_s8](int8x16_t inactive, int8x16_t a, int8_t b, mve_pred16_t p)
   int8x16_t [__arm_]vaddq_x[_s8](int8x16_t a, int8x16_t b, mve_pred16_t p)
   int8x16_t [__arm_]vaddq_x[_n_s8](int8x16_t a, int8_t b, mve_pred16_t p)  */
struct binary_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0", group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (binary_opt_n)

/* <T0>_t vfoo[t0](<T0>_t, <T0>_t)
   <T0>_t vfoo[_n_t0](<T0>_t, <S0>_t)

   Where the _n form only supports s16/s32/u16/u32 types as for vorrq.

   Example: vorrq.
   int16x8_t [__arm_]vorrq[_s16](int16x8_t a, int16x8_t b)
   int16x8_t [__arm_]vorrq_m[_s16](int16x8_t inactive, int16x8_t a, int16x8_t b, mve_pred16_t p)
   int16x8_t [__arm_]vorrq_x[_s16](int16x8_t a, int16x8_t b, mve_pred16_t p)
   int16x8_t [__arm_]vorrq[_n_s16](int16x8_t a, const int16_t imm)
   int16x8_t [__arm_]vorrq_m_n[_s16](int16x8_t a, const int16_t imm, mve_pred16_t p)

   No "_n" forms for floating-point, nor 8-bit integers:
   float16x8_t [__arm_]vorrq[_f16](float16x8_t a, float16x8_t b)
   float16x8_t [__arm_]vorrq_m[_f16](float16x8_t inactive, float16x8_t a, float16x8_t b, mve_pred16_t p)
   float16x8_t [__arm_]vorrq_x[_f16](float16x8_t a, float16x8_t b, mve_pred16_t p)  */
struct binary_orrq_def : public overloaded_base<0>
{
  bool
  explicit_mode_suffix_p (enum predication_index pred, enum mode_suffix_index mode) const override
  {
    return (mode == MODE_n
	    && pred == PRED_m);
  }

  bool
  skip_overload_p (enum predication_index pred, enum mode_suffix_index mode) const override
  {
    switch (mode)
      {
      case MODE_none:
	return false;

	/* For MODE_n, share the overloaded instance with MODE_none, except for PRED_m.  */
      case MODE_n:
	return pred != PRED_m;

      default:
	gcc_unreachable ();
      }
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,v0", group, MODE_none, preserve_user_namespace);
    build_16_32 (b, "v0,v0,s0", group, MODE_n, preserve_user_namespace, false, preds_m_or_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i, 0, type);
  }
};
SHAPE (binary_orrq)

/* <T0>_t vfoo[t0](<T0>_t, <T0>_t)
   <T0>_t vfoo[_n_t0](<T0>_t, int32_t)

   Shape for rounding shift left operations.

   Example: vrshlq.
   int8x16_t [__arm_]vrshlq[_n_s8](int8x16_t a, int32_t b)
   int8x16_t [__arm_]vrshlq_m_n[_s8](int8x16_t a, int32_t b, mve_pred16_t p)
   int8x16_t [__arm_]vrshlq[_s8](int8x16_t a, int8x16_t b)
   int8x16_t [__arm_]vrshlq_m[_s8](int8x16_t inactive, int8x16_t a, int8x16_t b, mve_pred16_t p)
   int8x16_t [__arm_]vrshlq_x[_s8](int8x16_t a, int8x16_t b, mve_pred16_t p)  */
struct binary_round_lshift_def : public overloaded_base<0>
{
  bool
  explicit_mode_suffix_p (enum predication_index pred, enum mode_suffix_index mode) const override
  {
    return ((mode == MODE_n)
	    && (pred == PRED_m));
  }

  bool
  skip_overload_p (enum predication_index pred, enum mode_suffix_index mode) const override
  {
    switch (mode)
      {
      case MODE_none:
	return false;

	/* For MODE_n, share the overloaded instance with MODE_none, except for PRED_m.  */
      case MODE_n:
	return pred != PRED_m;

      default:
	gcc_unreachable ();
      }
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,vs0", group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,ss32", group, MODE_n, preserve_user_namespace, false, preds_m_or_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i, 0, type, TYPE_signed);
  }
};
SHAPE (binary_round_lshift)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t)
   <T0>_t vfoo_n[_t0](<T0>_t, const int)

   i.e. the standard shape for left shift operations that operate on
   vector types.

   For the MODE_n versions, check that 'imm' is in the [0..#bits-1] range.

   Example: vshlq.
   int8x16_t [__arm_]vshlq[_s8](int8x16_t a, int8x16_t b)
   int8x16_t [__arm_]vshlq_m[_s8](int8x16_t inactive, int8x16_t a, int8x16_t b, mve_pred16_t p)
   int8x16_t [__arm_]vshlq_x[_s8](int8x16_t a, int8x16_t b, mve_pred16_t p)
   int8x16_t [__arm_]vshlq_n[_s8](int8x16_t a, const int imm)
   int8x16_t [__arm_]vshlq_m_n[_s8](int8x16_t inactive, int8x16_t a, const int imm, mve_pred16_t p)
   int8x16_t [__arm_]vshlq_x_n[_s8](int8x16_t a, const int imm, mve_pred16_t p)  */
struct binary_lshift_def : public overloaded_base<0>
{
  bool
  explicit_mode_suffix_p (enum predication_index, enum mode_suffix_index) const override
  {
    return true;
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,vs0", group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,ss32", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i, 0, type, TYPE_signed);
  }

  bool
  check (function_checker &c) const override
  {
    if (c.mode_suffix_id != MODE_n)
      return true;

    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (1, 0, bits - 1);
  }
};
SHAPE (binary_lshift)

/* Used with the above form, but only for the MODE_r case which does
   not always support the same set of predicates as MODE_none and
   MODE_n.  For vqshlq they are the same, but for vshlq they are not.

   <T0>_t vfoo_r[_t0](<T0>_t, int32_t)

   i.e. the standard shape for shift operations that operate on
   vector types.
   Example: vshlq.
   int8x16_t [__arm_]vshlq_r[_s8](int8x16_t a, int32_t b)
   int8x16_t [__arm_]vshlq_m_r[_s8](int8x16_t a, int32_t b, mve_pred16_t p)  */
struct binary_lshift_r_def : public overloaded_base<0>
{
  bool
  explicit_mode_suffix_p (enum predication_index, enum mode_suffix_index) const override
  {
    return true;
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_r, preserve_user_namespace);
    build_all (b, "v0,v0,ss32", group, MODE_r, preserve_user_namespace, false, preds_m_or_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i, 0, type, TYPE_signed);
  }
};
SHAPE (binary_lshift_r)

/* <T0:half>_t vfoo[_n_t0](<T0:half>_t, <T0>_t, const int)

   Narrowing right shifts.
   Check that 'imm' is in the [1..#bits/2] range.

   Example: vqrshrnbq.
   int8x16_t [__arm_]vqrshrnbq[_n_s16](int8x16_t a, int16x8_t b, const int imm)
   int8x16_t [__arm_]vqrshrnbq_m[_n_s16](int8x16_t a, int16x8_t b, const int imm, mve_pred16_t p)  */
struct binary_rshift_narrow_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "vh0,vh0,v0,su64", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, narrow_suffix;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES
	|| ((narrow_suffix = half_type_suffix (r, type, r.SAME_TYPE_CLASS))
	    == NUM_TYPE_SUFFIXES)
	|| !r.require_integer_immediate (i))
      return error_mark_node;

    if (!r.require_matching_vector_type (0, narrow_suffix))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (2, 1, bits / 2);
  }
};
SHAPE (binary_rshift_narrow)

/* <uT0:half>_t vfoo[_n_t0](<uT0:half>_t, <T0>_t, const int)

   Vector saturating rounding shift right and narrow.
   Check that 'imm' is in the [1..#bits/2] range.

   Example: vqshrunbq.
   uint8x16_t [__arm_]vqshrunbq[_n_s16](uint8x16_t a, int16x8_t b, const int imm)
   uint8x16_t [__arm_]vqshrunbq_m[_n_s16](uint8x16_t a, int16x8_t b, const int imm, mve_pred16_t p)  */
struct binary_rshift_narrow_unsigned_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "vhu0,vhu0,v0,su64", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, narrow_suffix;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES
	|| ((narrow_suffix = half_type_suffix (r, type, TYPE_unsigned))
	    == NUM_TYPE_SUFFIXES)
	|| !r.require_integer_immediate (i))
      return error_mark_node;

    if (!r.require_matching_vector_type (0, narrow_suffix))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (2, 1, bits / 2);
  }

};
SHAPE (binary_rshift_narrow_unsigned)

/* <T0:twice>_t vfoo[_t0](<T0>_t, <T0>_t)

   Example: vmullbq.
   int32x4_t [__arm_]vmullbq_int[_s16](int16x8_t a, int16x8_t b)
   int32x4_t [__arm_]vmullbq_int_m[_s16](int32x4_t inactive, int16x8_t a, int16x8_t b, mve_pred16_t p)
   int32x4_t [__arm_]vmullbq_int_x[_s16](int16x8_t a, int16x8_t b, mve_pred16_t p)  */
struct binary_widen_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vw0,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, wide_suffix;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i - 1)) == NUM_TYPE_SUFFIXES
	|| ((wide_suffix = long_type_suffix (r, type, r.SAME_TYPE_CLASS))
	    == NUM_TYPE_SUFFIXES))
      return error_mark_node;

    if (!r.require_matching_vector_type (i, type))
      return error_mark_node;

    /* Check the inactive argument has the wide type.  */
    if ((r.pred == PRED_m)
	&& (r.infer_vector_type (0) != wide_suffix))
      return r.report_no_such_form (type);

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_widen)

/* <T0:twice>_t vfoo[_t0](<T0>_t, <T0>_t)

   Example: vmullbq_poly.
   uint32x4_t [__arm_]vmullbq_poly[_p16](uint16x8_t a, uint16x8_t b)
   uint32x4_t [__arm_]vmullbq_poly_m[_p16](uint32x4_t inactive, uint16x8_t a, uint16x8_t b, mve_pred16_t p)
   uint32x4_t [__arm_]vmullbq_poly_x[_p16](uint16x8_t a, uint16x8_t b, mve_pred16_t p)  */
struct binary_widen_poly_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vU0,vp0,vp0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i - 1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    /* infer_vector_type found the 'unsigned' version of the 'poly'
       type we are looking for, so find the 'poly' type with the same
       width.  */
    type = find_type_suffix (TYPE_poly, type_suffixes[type].element_bits);

    type_suffix_index wide_suffix
      = find_type_suffix (TYPE_unsigned,
			  type_suffixes[type].element_bits * 2);

    /* Require the 'poly' type, require_matching_vector_type would try
       and fail with the 'unsigned' one.  */
    if (!r.require_vector_type (i, type_suffixes[type].vector_type))
      return error_mark_node;

    /* Check the inactive argument has the wide type.  */
    if ((r.pred == PRED_m)
	&& (r.infer_vector_type (0) != wide_suffix))
      return r.report_no_such_form (type);

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_widen_poly)

/* <T0:twice>_t vfoo[_n_t0](<T0>_t, const int)

   Check that 'imm' is in the [1..#bits] range.

   Example: vshllbq.
   int16x8_t [__arm_]vshllbq[_n_s8](int8x16_t a, const int imm)
   int16x8_t [__arm_]vshllbq_m[_n_s8](int16x8_t inactive, int8x16_t a, const int imm, mve_pred16_t p)
   int16x8_t [__arm_]vshllbq_x[_n_s8](int8x16_t a, const int imm, mve_pred16_t p)  */
struct binary_widen_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "vw0,v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, wide_suffix;
    tree res;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i - 1)) == NUM_TYPE_SUFFIXES
	|| ((wide_suffix = long_type_suffix (r, type, r.SAME_TYPE_CLASS))
	    == NUM_TYPE_SUFFIXES)
	|| !r.require_integer_immediate (i))
      return error_mark_node;

    /* Check the inactive argument has the wide type.  */
    if (((r.pred == PRED_m) && (r.infer_vector_type (0) == wide_suffix))
	|| r.pred == PRED_none
	|| r.pred == PRED_x)
      if ((res = r.lookup_form (r.mode_suffix_id, type)))
	return res;

    return r.report_no_such_form (type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (1, 1, bits);
  }

};
SHAPE (binary_widen_n)

/* <T0:twice>_t vfoo[_t0](<T0>_t, <T0>_t)
   <T0:twice>_t vfoo[_n_t0](<T0>_t, <S0>_t)

   Example: vqdmullbq.
   int32x4_t [__arm_]vqdmulltq[_n_s16](int16x8_t a, int16_t b)
   int32x4_t [__arm_]vqdmulltq_m[_n_s16](int32x4_t inactive, int16x8_t a, int16_t b, mve_pred16_t p)
   int32x4_t [__arm_]vqdmulltq[_s16](int16x8_t a, int16x8_t b)
   int32x4_t [__arm_]vqdmulltq_m[_s16](int32x4_t inactive, int16x8_t a, int16x8_t b, mve_pred16_t p)  */
struct binary_widen_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vw0,v0,v0", group, MODE_none, preserve_user_namespace);
    build_all (b, "vw0,v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, wide_suffix;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i - 1)) == NUM_TYPE_SUFFIXES
	|| ((wide_suffix = long_type_suffix (r, type, r.SAME_TYPE_CLASS))
	    == NUM_TYPE_SUFFIXES))
      return error_mark_node;

    /* Skip last argument, may be scalar, will be checked below by
       finish_opt_n_resolution.  */
    unsigned int last_arg = i--;
    for (; i > 0; i--)
      if (!r.require_matching_vector_type (i, type))
	return error_mark_node;

    /* Check the inactive argument has the wide type.  */
    if ((r.pred == PRED_m)
	&& (r.infer_vector_type (0) != wide_suffix))
      return r.report_no_such_form (type);

    return r.finish_opt_n_resolution (last_arg, 0, type);
  }
};
SHAPE (binary_widen_opt_n)

/* Shape for comparison operations that operate on
   uniform types.

   Examples: vcmpq.
   mve_pred16_t [__arm_]vcmpeqq[_s16](int16x8_t a, int16x8_t b)
   mve_pred16_t [__arm_]vcmpeqq[_n_s16](int16x8_t a, int16_t b)
   mve_pred16_t [__arm_]vcmpeqq_m[_s16](int16x8_t a, int16x8_t b, mve_pred16_t p)
   mve_pred16_t [__arm_]vcmpeqq_m[_n_s16](int16x8_t a, int16_t b, mve_pred16_t p)  */
struct cmp_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "p,v0,v0", group, MODE_none, preserve_user_namespace);
    build_all (b, "p,v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (cmp)

/* <T0>xN_t vfoo[_t0](uint64_t, uint64_t)

   where there are N arguments in total.
   Example: vcreateq.
   int16x8_t [__arm_]vcreateq_s16(uint64_t a, uint64_t b)  */
struct create_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    build_all (b, "v0,su64,su64", group, MODE_none, preserve_user_namespace);
  }
};
SHAPE (create)

/* <T0>[xN]_t vfoo_t0().

   Example: vuninitializedq.
   int8x16_t [__arm_]vuninitializedq_s8(void)
   int8x16_t [__arm_]vuninitializedq(int8x16_t t)  */
struct inherent_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    build_all (b, "t0", group, MODE_none, preserve_user_namespace);
  }
};
SHAPE (inherent)

/* sv<t0>_t svfoo[_t0](const <t0>_t *)

   Example: vld1q.
   int8x16_t [__arm_]vld1q[_s8](int8_t const *base)
   int8x16_t [__arm_]vld1q_z[_s8](int8_t const *base, mve_pred16_t p)  */
struct load_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "t0,al", group, MODE_none, preserve_user_namespace);
  }

  /* Resolve a call based purely on a pointer argument.  */
  tree
  resolve (function_resolver &r) const override
  {
    gcc_assert (r.mode_suffix_id == MODE_none);

    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (1, i, nargs)
	|| (type = r.infer_pointer_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (load)

/* <T0>_t vfoo[_t0](<T0>_t)
   <T0>_t vfoo_n_t0(<sT0>_t)

   For MODE_n, define only the 16 and 32 bits versions.

   Example: vmvnq.
   int16x8_t [__arm_]vmvnq[_s16](int16x8_t a)
   int16x8_t [__arm_]vmvnq_m[_s16](int16x8_t inactive, int16x8_t a, mve_pred16_t p)
   int16x8_t [__arm_]vmvnq_x[_s16](int16x8_t a, mve_pred16_t p)
   int16x8_t [__arm_]vmvnq_n_s16(const int16_t imm)
   int16x8_t [__arm_]vmvnq_m[_n_s16](int16x8_t inactive, const int16_t imm, mve_pred16_t p)
   int16x8_t [__arm_]vmvnq_x_n_s16(const int16_t imm, mve_pred16_t p)  */
struct mvn_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    /* Do not build a separate instance for MODE_n, since we want to
       share vmvnq_m[_n_s16] with vmvnq_m[_s16].  */
    build_all (b, "v0,v0", group, MODE_none, preserve_user_namespace);
    build_16_32 (b, "v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (1, i, nargs)
	/* Same type for arg 0 and 1 if _m, so using 0 is OK */
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

  /* Skip last argument, may be scalar.  */
    unsigned int last_arg = i;
    for (i = 0; i < last_arg; i++)
      if (!r.require_matching_vector_type (i, type))
	return error_mark_node;

    if (last_arg == 0)
      return r.resolve_to (r.mode_suffix_id, type);

    return r.finish_opt_n_resolution (last_arg, 0, type);
  }
};
SHAPE (mvn)

/* void vfoo[_t0](<X>_t *, v<t0>[xN]_t)

   where <X> might be tied to <t0> (for non-truncating stores) or might
   depend on the function base name (for truncating stores).

   Example: vst1q.
   void [__arm_]vst1q[_s8](int8_t *base, int8x16_t value)
   void [__arm_]vst1q_p[_s8](int8_t *base, int8x16_t value, mve_pred16_t p)  */
struct store_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "_,as,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    gcc_assert (r.mode_suffix_id == MODE_none);

    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_pointer_type (0)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (store)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t, <T0>_t)

   i.e. the standard shape for ternary operations that operate on
   uniform types.

   Example: vqrdmlsdhxq.
   int8x16_t [__arm_]vqrdmlsdhxq[_s8](int8x16_t inactive, int8x16_t a, int8x16_t b)
   int8x16_t [__arm_]vqrdmlsdhxq_m[_s8](int8x16_t inactive, int8x16_t a, int8x16_t b, mve_pred16_t p)  */
struct ternary_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (3);
  }
};
SHAPE (ternary)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t, const int)

   i.e. ternary operations that operate on a pair of vectors of the
   same type as the destination, and take a third integer argument.

   Check that 'imm' is in the [0..#bits-1] range.

   Example: vsliq.
   int16x8_t [__arm_]vsliq[_n_s16](int16x8_t a, int16x8_t b, const int imm)
   int16x8_t [__arm_]vsliq_m[_n_s16](int16x8_t a, int16x8_t b, const int imm, mve_pred16_t p)  */
struct ternary_lshift_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,v0,su64", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const override
  {
    if (c.mode_suffix_id != MODE_n)
      return true;

    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (2, 0, bits - 1);
  }
};
SHAPE (ternary_lshift)

/* <T0>_t vfoo[_n_t0](<T0>_t, <T0>_t, <S0>_t)

   i.e. the standard shape for ternary operations that operate on a
   pair of vectors of the same type as the destination, and take a
   third scalar argument of the same type as the vector elements.

   Example: vmlaq.
   int8x16_t [__arm_]vmlaq[_n_s8](int8x16_t add, int8x16_t m1, int8_t m2)
   int8x16_t [__arm_]vmlaq_m[_n_s8](int8x16_t add, int8x16_t m1, int8_t m2, mve_pred16_t p)  */
struct ternary_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }
};
SHAPE (ternary_n)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t, <T0>_t)
   <T0>_t vfoo[_n_t0](<T0>_t, <T0>_t, <S0>_t)

   i.e. the standard shape for ternary operations that operate on
   uniform types.

   Example: vfmaq.
   float16x8_t [__arm_]vfmaq[_n_f16](float16x8_t add, float16x8_t m1, float16_t m2)
   float16x8_t [__arm_]vfmaq_m[_n_f16](float16x8_t add, float16x8_t m1, float16_t m2, mve_pred16_t p)
   float16x8_t [__arm_]vfmaq[_f16](float16x8_t add, float16x8_t m1, float16x8_t m2)
   float16x8_t [__arm_]vfmaq_m[_f16](float16x8_t add, float16x8_t m1, float16x8_t m2, mve_pred16_t p)  */
struct ternary_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0,v0", group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (3);
  }
};
SHAPE (ternary_opt_n)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t, const int)

   i.e. ternary operations that operate on a pair of vectors of the
   same type as the destination, and take a third integer argument.

   Check that 'imm' is in the [1..#bits] range.

   Example: vsriq.
   int8x16_t [__arm_]vsriq[_n_s8](int8x16_t a, int8x16_t b, const int imm)
   int8x16_t [__arm_]vsriq_m[_n_s8](int8x16_t a, int8x16_t b, const int imm, mve_pred16_t p)  */
struct ternary_rshift_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v0,v0,su64", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const override
  {
    if (c.mode_suffix_id != MODE_n)
      return true;

    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (2, 1, bits);
  }
};
SHAPE (ternary_rshift)

/* <T0>_t vfoo[_t0](<T0>_t)

   i.e. the standard shape for unary operations that operate on
   uniform types.

   Example: vabsq.
   int8x16_t [__arm_]vabsq[_s8](int8x16_t a)
   int8x16_t [__arm_]vabsq_m[_s8](int8x16_t inactive, int8x16_t a, mve_pred16_t p)
   int8x16_t [__arm_]vabsq_x[_s8](int8x16_t a, mve_pred16_t p)  */
struct unary_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary ();
  }
};
SHAPE (unary)

/* <S0:twice>_t vfoo[_<t0>](<T0>_t)

   i.e. a version of "unary" in which the source elements are half the
   size of the destination scalar, but have the same type class.

   Example: vaddlvq.
   int64_t [__arm_]vaddlvq[_s32](int32x4_t a)
   int64_t [__arm_]vaddlvq_p[_s32](int32x4_t a, mve_pred16_t p) */
struct unary_acc_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sw0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    /* FIXME: check that the return value is actually
       twice as wide as arg 0.  */
    return r.resolve_unary ();
  }
};
SHAPE (unary_acc)

/* <T0>_t foo_t0[_t1](<T1>_t)

   where the target type <t0> must be specified explicitly but the source
   type <t1> can be inferred.

   Example: vreinterpretq.
   int16x8_t [__arm_]vreinterpretq_s16[_s8](int8x16_t a)
   int32x4_t [__arm_]vreinterpretq_s32[_s8](int8x16_t a)
   int8x16_t [__arm_]vreinterpretq_s8[_s16](int16x8_t a)
   int8x16_t [__arm_]vreinterpretq_s8[_s32](int32x4_t a)  */
struct unary_convert_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v1", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary ();
  }
};
SHAPE (unary_convert)

/* [u]int32_t vfoo[_<t0>](<T0>_t)

   i.e. a version of "unary" which generates a scalar of type int32_t
   or uint32_t depending on the signedness of the elements of of input
   vector.

   Example: vaddvq
   int32_t [__arm_]vaddvq[_s16](int16x8_t a)
   int32_t [__arm_]vaddvq_p[_s16](int16x8_t a, mve_pred16_t p)  */
struct unary_int32_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sx32,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (unary_int32)

/* [u]int32_t vfoo[_<t0>]([u]int32_t, <T0>_t)

   i.e. a version of "unary" which accumulates into scalar of type
   int32_t or uint32_t depending on the signedness of the elements of
   of input vector.

   Example: vaddvaq.
   int32_t [__arm_]vaddvaq[_s16](int32_t a, int16x8_t b)
   int32_t [__arm_]vaddvaq_p[_s16](int32_t a, int16x8_t b, mve_pred16_t p)  */
struct unary_int32_acc_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sx32,sx32,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    const char *first_type_name;

    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    first_type_name = (type_suffixes[type].unsigned_p
		       ? "uint32_t"
		       : "int32_t");
    if (!r.require_scalar_type (0, first_type_name))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (unary_int32_acc)

/* <T0>_t vfoo[_n]_t0(<S0>_t)

   Example: vdupq.
   int16x8_t [__arm_]vdupq_n_s16(int16_t a)
   int16x8_t [__arm_]vdupq_m[_n_s16](int16x8_t inactive, int16_t a, mve_pred16_t p)
   int16x8_t [__arm_]vdupq_x_n_s16(int16_t a, mve_pred16_t p)  */
struct unary_n_def : public overloaded_base<0>
{
  bool
  explicit_type_suffix_p (unsigned int, enum predication_index pred,
			  enum mode_suffix_index, type_suffix_info) const override
  {
    return pred != PRED_m;
  }

  bool
  explicit_mode_suffix_p (enum predication_index pred,
			  enum mode_suffix_index mode) const override
  {
    return ((mode == MODE_n)
	    && (pred != PRED_m));
  }

  bool
  skip_overload_p (enum predication_index pred, enum mode_suffix_index mode)
    const override
  {
    switch (mode)
      {
      case MODE_n:
	return pred != PRED_m;

      default:
	gcc_unreachable ();
      }
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,s0", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary_n ();
  }
};
SHAPE (unary_n)

/* <T0:twice>_t vfoo[_t0](<T0>_t)

   i.e. a version of "unary" in which the source elements are half the
   size of the destination, but have the same type class.

   Example: vmovlbq.
   int32x4_t [__arm_]vmovlbq[_s16](int16x8_t a)
   int32x4_t [__arm_]vmovlbq_m[_s16](int32x4_t inactive, int16x8_t a, mve_pred16_t p)
   int32x4_t [__arm_]vmovlbq_x[_s16](int16x8_t a, mve_pred16_t p)  */
struct unary_widen_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "vw0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, wide_suffix;
    tree res;
    if (!r.check_gp_argument (1, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| ((wide_suffix = long_type_suffix (r, type, r.SAME_TYPE_CLASS))
	    == NUM_TYPE_SUFFIXES))
      return error_mark_node;

    /* Check the inactive argument has the wide type.  */
    if ((r.pred == PRED_m)
	&& (r.infer_vector_type (0) != wide_suffix))
    return r.report_no_such_form (type);

    if ((res = r.lookup_form (r.mode_suffix_id, type)))
	return res;

    return r.report_no_such_form (type);
  }
};
SHAPE (unary_widen)

/* <S0:twice>_t vfoo[_<t0>](<S0:twice>_t, <T0>_t)

   i.e. a version of "unary" in which the source elements are half the
   size of the destination scalar and accumulator, but have the same
   type class.

   Example: vaddlvaq.
   int64_t [__arm_]vaddlvaq[_s32](int64_t a, int32x4_t b)
   int64_t [__arm_]vaddlvaq_p[_s32](int64_t a, int32x4_t b, mve_pred16_t p)  */
struct unary_widen_acc_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "sw0,sw0,v0", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_derived_scalar_type (0, r.SAME_TYPE_CLASS)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (unary_widen_acc)

/* <T0>_t vfoo[_t0](T0, T0, uint32_t*)

   Example: vadcq.
   int32x4_t [__arm_]vadcq[_s32](int32x4_t a, int32x4_t b, unsigned *carry)
   int32x4_t [__arm_]vadcq_m[_s32](int32x4_t inactive, int32x4_t a, int32x4_t b, unsigned *carry, mve_pred16_t p)  */
struct vadc_vsbc_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0,as", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    if (!r.require_matching_vector_type (1, type))
      return error_mark_node;

    /* Check that last arg is a pointer.  */
    if (!POINTER_TYPE_P (r.get_argument_type (i)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (vadc_vsbc)

/* mve_pred16_t foo_t0(uint32_t)

   Example: vctp16q.
   mve_pred16_t [__arm_]vctp16q(uint32_t a)
   mve_pred16_t [__arm_]vctp16q_m(uint32_t a, mve_pred16_t p)  */
struct vctp_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    build_all (b, "p,su32", group, MODE_none, preserve_user_namespace);
  }
};
SHAPE (vctp)

/* <T0>_t foo_t0[_t1](<T1>_t)
   <T0>_t foo_t0_n[_t1](<T1>_t, const int)

   Example: vcvtq.
   float32x4_t [__arm_]vcvtq[_f32_s32](int32x4_t a)
   float32x4_t [__arm_]vcvtq_m[_f32_s32](float32x4_t inactive, int32x4_t a, mve_pred16_t p)
   float32x4_t [__arm_]vcvtq_x[_f32_s32](int32x4_t a, mve_pred16_t p)
   float32x4_t [__arm_]vcvtq_n[_f32_s32](int32x4_t a, const int imm6)
   float32x4_t [__arm_]vcvtq_m_n[_f32_s32](float32x4_t inactive, int32x4_t a, const int imm6, mve_pred16_t p)
   float32x4_t [__arm_]vcvtq_x_n[_f32_s32](int32x4_t a, const int imm6, mve_pred16_t p)
   int32x4_t [__arm_]vcvtq_s32_f32(float32x4_t a)
   int32x4_t [__arm_]vcvtq_m[_s32_f32](int32x4_t inactive, float32x4_t a, mve_pred16_t p)
   int32x4_t [__arm_]vcvtq_x_s32_f32(float32x4_t a, mve_pred16_t p)
   int32x4_t [__arm_]vcvtq_n_s32_f32(float32x4_t a, const int imm6)
   int32x4_t [__arm_]vcvtq_m_n[_s32_f32](int32x4_t inactive, float32x4_t a, const int imm6, mve_pred16_t p)
   int32x4_t [__arm_]vcvtq_x_n_s32_f32(float32x4_t a, const int imm6, mve_pred16_t p)  */
struct vcvt_def : public overloaded_base<0>
{
  bool
  explicit_type_suffix_p (unsigned int i, enum predication_index pred,
			  enum mode_suffix_index,
			  type_suffix_info type_info) const override
  {
    if (pred != PRED_m
	&& ((i == 0 && type_info.integer_p)
	    || (i == 1 && type_info.float_p)))
      return true;
    return false;
  }

  bool
  explicit_mode_suffix_p (enum predication_index,
			  enum mode_suffix_index) const override
  {
    return true;
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    b.add_overloaded_functions (group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,v1", group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v1,su64", group, MODE_n, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index from_type;
    tree res;
    unsigned int nimm = (r.mode_suffix_id == MODE_none) ? 0 : 1;

    if (!r.check_gp_argument (1 + nimm, i, nargs)
	|| (from_type
	    = r.infer_vector_type (i - nimm)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    if (nimm > 0
	&& !r.require_integer_immediate (i))
      return error_mark_node;

    type_suffix_index to_type;

    if (type_suffixes[from_type].integer_p)
      {
	to_type = find_type_suffix (TYPE_float,
				    type_suffixes[from_type].element_bits);
      }
    else
      {
	/* This should not happen: when 'from_type' is float, the type
	   suffixes are not overloaded (except for "m" predication,
	   handled above). */
	gcc_assert (r.pred == PRED_m);

	/* Get the return type from the 'inactive' argument.  */
	to_type = r.infer_vector_type (0);
      }

    if ((res = r.lookup_form (r.mode_suffix_id, to_type, from_type)))
	return res;

    return r.report_no_such_form (from_type);
  }

  bool
  check (function_checker &c) const override
  {
    if (c.mode_suffix_id == MODE_none)
      return true;

    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (1, 1, bits);
  }
};
SHAPE (vcvt)

/* float16x8_t foo_f16_f32(float16x8_t, float32x4_t)

   Example: vcvttq_f16_f32.
   float16x8_t [__arm_]vcvttq_f16_f32(float16x8_t a, float32x4_t b)
   float16x8_t [__arm_]vcvttq_m_f16_f32(float16x8_t a, float32x4_t b, mve_pred16_t p)
*/
struct vcvt_f16_f32_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    build_all (b, "v0,v0,v1", group, MODE_none, preserve_user_namespace);
  }
};
SHAPE (vcvt_f16_f32)

/* float32x4_t foo_f32_f16(float16x8_t)

   Example: vcvttq_f32_f16.
   float32x4_t [__arm_]vcvttq_f32_f16(float16x8_t a)
   float32x4_t [__arm_]vcvttq_m_f32_f16(float32x4_t inactive, float16x8_t a, mve_pred16_t p)
   float32x4_t [__arm_]vcvttq_x_f32_f16(float16x8_t a, mve_pred16_t p)
*/
struct vcvt_f32_f16_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    build_all (b, "v0,v1", group, MODE_none, preserve_user_namespace);
  }
};
SHAPE (vcvt_f32_f16)

/* <T0>_t foo_t0[_t1](<T1>_t)

   Example: vcvtaq.
   int16x8_t [__arm_]vcvtaq_s16_f16(float16x8_t a)
   int16x8_t [__arm_]vcvtaq_m[_s16_f16](int16x8_t inactive, float16x8_t a, mve_pred16_t p)
   int16x8_t [__arm_]vcvtaq_x_s16_f16(float16x8_t a, mve_pred16_t p)
*/
struct vcvtx_def : public overloaded_base<0>
{
  bool
  explicit_type_suffix_p (unsigned int, enum predication_index pred,
			  enum mode_suffix_index,
			  type_suffix_info) const override
  {
    return pred != PRED_m;
  }

  bool
  skip_overload_p (enum predication_index pred, enum mode_suffix_index)
    const override
  {
    return pred != PRED_m;
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v1", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index from_type;
    tree res;

    if (!r.check_gp_argument (1, i, nargs)
	|| (from_type
	    = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    type_suffix_index to_type;

    gcc_assert (r.pred == PRED_m);

    /* Get the return type from the 'inactive' argument.  */
    to_type = r.infer_vector_type (0);

    if ((res = r.lookup_form (r.mode_suffix_id, to_type, from_type)))
	return res;

    return r.report_no_such_form (from_type);
  }
};
SHAPE (vcvtx)

/* <T0>_t vfoo[_n]_t0(uint32_t, const int)
   <T0>_t vfoo[_wb]_t0(uint32_t *, const int)

   Shape for vector increment or decrement and duplicate operations that take
   an integer or pointer to integer first argument and an immediate, and
   produce a vector.

   Check that 'imm' is one of 1, 2, 4 or 8.

   Example: vddupq.
   uint8x16_t [__arm_]vddupq[_n]_u8(uint32_t a, const int imm)
   uint8x16_t [__arm_]vddupq[_wb]_u8(uint32_t *a, const int imm)
   uint8x16_t [__arm_]vddupq_m[_n_u8](uint8x16_t inactive, uint32_t a, const int imm, mve_pred16_t p)
   uint8x16_t [__arm_]vddupq_m[_wb_u8](uint8x16_t inactive, uint32_t *a, const int imm, mve_pred16_t p)
   uint8x16_t [__arm_]vddupq_x[_n]_u8(uint32_t a, const int imm, mve_pred16_t p)
   uint8x16_t [__arm_]vddupq_x[_wb]_u8(uint32_t *a, const int imm, mve_pred16_t p)  */
struct viddup_def : public overloaded_base<0>
{
  bool
  explicit_type_suffix_p (unsigned int i, enum predication_index pred,
			  enum mode_suffix_index,
			  type_suffix_info) const override
  {
    return ((i == 0) && (pred != PRED_m));
  }

  bool
  skip_overload_p (enum predication_index, enum mode_suffix_index mode) const override
  {
    /* For MODE_wb, share the overloaded instance with MODE_n.  */
    if (mode == MODE_wb)
      return true;

    return false;
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,su32,su64", group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,as,su64", group, MODE_wb, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type_suffix = NUM_TYPE_SUFFIXES;
    if (!r.check_gp_argument (2, i, nargs))
      return error_mark_node;

    type_suffix = r.type_suffix_ids[0];
    /* With PRED_m, ther is no type suffix, so infer it from the first (inactive)
       argument.  */
    if (type_suffix == NUM_TYPE_SUFFIXES)
      type_suffix = r.infer_vector_type (0);

    unsigned int last_arg = i - 1;
    /* Check that last_arg is either scalar or pointer.  */
    if (!r.scalar_argument_p (last_arg))
      return error_mark_node;

    if (!r.require_integer_immediate (last_arg + 1))
      return error_mark_node;

    /* With MODE_n we expect a scalar, with MODE_wb we expect a pointer.  */
    mode_suffix_index mode_suffix;
    if (POINTER_TYPE_P (r.get_argument_type (last_arg)))
      mode_suffix = MODE_wb;
    else
      mode_suffix = MODE_n;

    return r.resolve_to (mode_suffix, type_suffix);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_one_of (1, 1, 2, 4, 8);
  }
};
SHAPE (viddup)

/* <T0>_t vfoo[_n]_t0(uint32_t, uint32_t, const int)
   <T0>_t vfoo[_wb]_t0(uint32_t *, uint32_t, const int)

   Shape for vector increment or decrement with wrap and duplicate operations
   that take an integer or pointer to integer first argument, an integer second
   argument and an immediate, and produce a vector.

   Check that 'imm' is one of 1, 2, 4 or 8.

   Example: vdwdupq.
   uint8x16_t [__arm_]vdwdupq[_n]_u8(uint32_t a, uint32_t b, const int imm)
   uint8x16_t [__arm_]vdwdupq[_wb]_u8(uint32_t *a, uint32_t b, const int imm)
   uint8x16_t [__arm_]vdwdupq_m[_n_u8](uint8x16_t inactive, uint32_t a, uint32_t b, const int imm, mve_pred16_t p)
   uint8x16_t [__arm_]vdwdupq_m[_wb_u8](uint8x16_t inactive, uint32_t *a, uint32_t b, const int imm, mve_pred16_t p)
   uint8x16_t [__arm_]vdwdupq_x[_n]_u8(uint32_t a, uint32_t b, const int imm, mve_pred16_t p)
   uint8x16_t [__arm_]vdwdupq_x[_wb]_u8(uint32_t *a, uint32_t b, const int imm, mve_pred16_t p)  */
struct vidwdup_def : public overloaded_base<0>
{
  bool
  explicit_type_suffix_p (unsigned int i, enum predication_index pred,
			  enum mode_suffix_index,
			  type_suffix_info) const override
  {
    return ((i == 0) && (pred != PRED_m));
  }

  bool
  skip_overload_p (enum predication_index, enum mode_suffix_index mode) const override
  {
    /* For MODE_wb, share the overloaded instance with MODE_n.  */
    if (mode == MODE_wb)
      return true;

    return false;
  }

  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,su32,su32,su64", group, MODE_n, preserve_user_namespace);
    build_all (b, "v0,as,su32,su64", group, MODE_wb, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type_suffix = NUM_TYPE_SUFFIXES;
    if (!r.check_gp_argument (3, i, nargs))
      return error_mark_node;

    type_suffix = r.type_suffix_ids[0];
    /* With PRED_m, ther is no type suffix, so infer it from the first (inactive)
       argument.  */
    if (type_suffix == NUM_TYPE_SUFFIXES)
      type_suffix = r.infer_vector_type (0);

    unsigned int last_arg = i - 2;
    /* Check that last_arg is either scalar or pointer.  */
    if (!r.scalar_argument_p (last_arg))
      return error_mark_node;

    if (!r.scalar_argument_p (last_arg + 1))
      return error_mark_node;

    if (!r.require_integer_immediate (last_arg + 2))
      return error_mark_node;

    /* With MODE_n we expect a scalar, with MODE_wb we expect a pointer.  */
    mode_suffix_index mode_suffix;
    if (POINTER_TYPE_P (r.get_argument_type (last_arg)))
      mode_suffix = MODE_wb;
    else
      mode_suffix = MODE_n;

    return r.resolve_to (mode_suffix, type_suffix);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_one_of (2, 1, 2, 4, 8);
  }
};
SHAPE (vidwdup)

/* <T0>_t vfoo[_t0](<T0>_t, <T0>_t, mve_pred16_t)

   i.e. a version of the standard ternary shape in which
   the final argument is always a set of predicates.

   Example: vpselq.
   int16x8_t [__arm_]vpselq[_s16](int16x8_t a, int16x8_t b, mve_pred16_t p)  */
struct vpsel_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,v0,p", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    unsigned int last_arg = i;
    for (i = 0; i < last_arg; i++)
      if (!r.require_matching_vector_type (i, type))
	return error_mark_node;

    if (!r.require_vector_type (2 , VECTOR_TYPE_mve_pred16_t))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (vpsel)

/* <T0>_t vfoo[_t0](T0, uint32_t* , const int)

   Check that 'imm' is in [1..32].

   Example: vshlcq.
   uint8x16_t [__arm_]vshlcq[_u8](uint8x16_t a, uint32_t *b, const int imm)
   uint8x16_t [__arm_]vshlcq_m[_u8](uint8x16_t a, uint32_t *b, const int imm, mve_pred16_t p)  */
struct vshlc_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group,
	 bool preserve_user_namespace) const override
  {
    b.add_overloaded_functions (group, MODE_none, preserve_user_namespace);
    build_all (b, "v0,v0,as,su64", group, MODE_none, preserve_user_namespace);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    /* Check that arg #2 is a pointer.  */
    if (!POINTER_TYPE_P (r.get_argument_type (i - 1)))
      return error_mark_node;

    if (!r.require_integer_immediate (i))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (2, 1, 32);
  }
};
SHAPE (vshlc)

} /* end namespace arm_mve */

#undef SHAPE
