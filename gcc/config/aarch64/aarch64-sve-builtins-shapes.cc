/* ACLE support for AArch64 SVE (function shapes)
   Copyright (C) 2018-2025 Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "aarch64-sve-builtins.h"
#include "aarch64-sve-builtins-shapes.h"
#include "aarch64-builtins.h"

/* In the comments below, _t0 represents the first type suffix and _t1
   represents the second.  Square brackets enclose characters that are
   present in only the full name, not the overloaded name.  Governing
   predicate arguments and predicate suffixes are not shown, since they
   depend on the predication type, which is a separate piece of
   information from the shape.

   Non-overloaded functions may have additional suffixes beyond the
   ones shown, if those suffixes don't affect the types in the type
   signature.  E.g. the predicate form of svtrn1 has a _b<bits> suffix,
   but this does not affect the prototype, which is always
   "svbool_t(svbool_t, svbool_t)".  */

namespace aarch64_sve {

/* Return a representation of "const T *".  */
static tree
build_const_pointer (tree t)
{
  return build_pointer_type (build_qualified_type (t, TYPE_QUAL_CONST));
}

/* GROUP's first type suffix is a ZA-related one.  Return true if the
   group exists only for the purpose of defining C overloads.  This is
   useful if some forms of an instruction require one feature and other
   forms require another feature, and neither feature implies the other.  */
static bool
za_group_is_pure_overload (const function_group_info &group)
{
  gcc_checking_assert (type_suffixes[group.types[0][0]].za_p);
  return group.types[0][1] == NUM_TYPE_SUFFIXES;
}

/* If INSTANCE has a governing predicate, add it to the list of argument
   types in ARGUMENT_TYPES.  RETURN_TYPE is the type returned by the
   function.  */
static void
apply_predication (const function_instance &instance, tree return_type,
		   vec<tree> &argument_types)
{
  /* There are currently no SME ZA instructions that have both merging and
     unpredicated forms, so for simplicity, the predicates are always included
     in the original format string.  */
  if (instance.pred != PRED_none && instance.pred != PRED_za_m)
    {
      argument_types.quick_insert (0, instance.gp_type ());
      /* For unary merge operations, the first argument is a vector with
	 the same type as the result.  For unary_convert_narrowt it also
	 provides the "bottom" half of active elements, and is present
	 for all types of predication.  */
      auto nargs = argument_types.length () - 1;
      if (instance.shape->has_merge_argument_p (instance, nargs))
	argument_types.quick_insert (0, return_type);
    }
}

/* Parse and move past an element type in FORMAT and return it as a type
   suffix.  The format is:

   [01]    - the element type in type suffix 0 or 1 of INSTANCE
   f<bits> - a floating-point type with the given number of bits
   f[01]   - a floating-point type with the same width as type suffix 0 or 1
   B       - bfloat16_t
   c       - a predicate-as-counter
   h<elt>  - a half-sized version of <elt>
   M       - mfloat8_t
   p       - a predicate (represented as TYPE_SUFFIX_b)
   q<elt>  - a quarter-sized version of <elt>
   s<bits> - a signed type with the given number of bits
   s[01]   - a signed type with the same width as type suffix 0 or 1
   u<bits> - an unsigned type with the given number of bits
   u[01]   - an unsigned type with the same width as type suffix 0 or 1
   w<elt>  - a 64-bit version of <elt> if <elt> is integral, otherwise <elt>

   where <elt> is another element type.  */
static type_suffix_index
parse_element_type (const function_instance &instance, const char *&format)
{
  int ch = *format++;

  if (ch == 'f' || ch == 's' || ch == 'u')
    {
      type_class_index tclass = (ch == 'f' ? TYPE_float
				 : ch == 's' ? TYPE_signed
				 : TYPE_unsigned);
      char *end;
      unsigned int bits = strtol (format, &end, 10);
      format = end;
      if (bits == 0 || bits == 1)
	bits = instance.type_suffix (bits).element_bits;
      return find_type_suffix (tclass, bits);
    }

  if (ch == 'w')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      if (type_suffixes[suffix].integer_p)
	return find_type_suffix (type_suffixes[suffix].tclass, 64);
      return suffix;
    }

  if (ch == 'c')
    return TYPE_SUFFIX_c;

  if (ch == 'p')
    return TYPE_SUFFIX_b;

  if (ch == 'B')
    return TYPE_SUFFIX_bf16;

  if (ch == 'M')
    return TYPE_SUFFIX_mf8;

  if (ch == 'q')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      return find_type_suffix (type_suffixes[suffix].tclass,
			       type_suffixes[suffix].element_bits / 4);
    }

  if (ch == 'h')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      /* Widening and narrowing doesn't change the type for predicates;
	 everything's still an svbool_t.  */
      if (suffix == TYPE_SUFFIX_b)
	return suffix;
      return find_type_suffix (type_suffixes[suffix].tclass,
			       type_suffixes[suffix].element_bits / 2);
    }

  if (ch == '0' || ch == '1')
    return instance.type_suffix_ids[ch - '0'];

  gcc_unreachable ();
}

/* Read and return a type from FORMAT for function INSTANCE.  Advance
   FORMAT beyond the type string.  The format is:

   _       - void
   al      - array pointer for loads
   ap      - array pointer for prefetches
   as      - array pointer for stores
   b       - base vector type (from a _<m0>base suffix)
   c0      - the result of a conversion, based on type and group suffixes
   c1      - the source of a conversion, based on type and group suffixes
   d       - displacement vector type (from a _<m1>index or _<m1>offset suffix)
   e<name> - an enum with the given name
   s<elt>  - a scalar type with the given element suffix
   t<elt>  - a vector or tuple type with given element suffix [*1]
   v<elt>  - a vector with the given element suffix
   D<elt>  - a 64 bit neon vector
   Q<elt>  - a 128 bit neon vector

   where <elt> has the format described above parse_element_type

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
      if (ch == 'p')
	return const_ptr_type_node;
      if (ch == 's')
	return build_pointer_type (instance.memory_scalar_type ());
      gcc_unreachable ();
    }

  if (ch == 'b')
    return instance.base_vector_type ();

  if (ch == 'c')
    {
      int ch = *format++;
      gcc_assert (ch == '0' || ch == '1');
      unsigned int id = (ch == '0' ? 0 : 1);
      auto vector_type = instance.type_suffix (id).vector_type;
      unsigned int num_vectors = instance.group_suffix ().vectors_per_tuple;
      if (num_vectors != 1)
	{
	  unsigned int bits = instance.type_suffix (id).element_bits;
	  unsigned int other_bits = instance.type_suffix (1 - id).element_bits;
	  if (other_bits > bits)
	    num_vectors /= other_bits / bits;
	}
      return acle_vector_types[num_vectors - 1][vector_type];
    }

  if (ch == 'd')
    return instance.displacement_vector_type ();

  if (ch == 'e')
    {
      if (startswith (format, "pattern"))
	{
	  format += 7;
	  return acle_svpattern;
	}
      if (startswith (format, "prfop"))
	{
	  format += 5;
	  return acle_svprfop;
	}
      gcc_unreachable ();
    }

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

  if (ch == 'D')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      int neon_index = type_suffixes[suffix].neon64_type;
      return aarch64_simd_types_trees[neon_index].itype;
    }

  if (ch == 'Q')
    {
      type_suffix_index suffix = parse_element_type (instance, format);
      int neon_index = type_suffixes[suffix].neon128_type;
      return aarch64_simd_types_trees[neon_index].itype;
    }

  gcc_unreachable ();
}

/* Read and move past any argument count at FORMAT for the function
   signature of INSTANCE.  The counts are:

   *q: one argument per element in a 128-bit quadword (as for svdupq)
   *t: one argument per vector in a tuple (as for svcreate)

   Otherwise the count is 1.  */
static unsigned int
parse_count (const function_instance &instance, const char *&format)
{
  if (format[0] == '*' && format[1] == 'q')
    {
      format += 2;
      return instance.elements_per_vq (0);
    }
  if (format[0] == '*' && format[1] == 't')
    {
      format += 2;
      return instance.vectors_per_tuple ();
    }
  return 1;
}

/* Read a type signature for INSTANCE from FORMAT.  Add the argument types
   to ARGUMENT_TYPES and return the return type.

   The format is a comma-separated list of types (as for parse_type),
   with the first type being the return type and the rest being the
   argument types.  Each argument type can be followed by an optional
   count (as for parse_count).  */
static tree
parse_signature (const function_instance &instance, const char *format,
		 vec<tree> &argument_types)
{
  tree return_type = parse_type (instance, format);
  while (format[0] == ',')
    {
      format += 1;
      tree argument_type = parse_type (instance, format);
      unsigned int count = parse_count (instance, format);
      for (unsigned int i = 0; i < count; ++i)
	argument_types.quick_push (argument_type);
    }
  gcc_assert (format[0] == 0);
  if (instance.fpm_mode == FPM_set)
    argument_types.quick_push (get_typenode_from_name (UINT64_TYPE));
  return return_type;
}

/* Add one function instance for GROUP, using mode suffix MODE_SUFFIX_ID,
   the type suffixes at index TI, the group suffixes at index GI, and the
   predication suffix at index PI.  The other arguments are as for
   build_all.  */
static void
build_one (function_builder &b, const char *signature,
	   const function_group_info &group, mode_suffix_index mode_suffix_id,
	   unsigned int ti, unsigned int gi, unsigned int pi,
	   bool force_direct_overloads)
{
  /* For simplicity, function definitions are allowed to use the group
     suffix lists vg2 and vg4 for shapes that have _single forms,
     even though the _single form applies only to vgNx2 and vgNx4,
     not to vgNx1.  */
  if (mode_suffix_id == MODE_single
      && group_suffixes[group.groups[gi]].vectors_per_tuple == 1)
    return;

  /* Byte forms of svdupq take 16 arguments.  */
  auto_vec<tree, 16> argument_types;
  function_instance instance (group.base_name, *group.base, *group.shape,
			      mode_suffix_id, group.types[ti], group.groups[gi],
			      group.preds[pi], group.fpm_mode);
  tree return_type = parse_signature (instance, signature, argument_types);
  apply_predication (instance, return_type, argument_types);
  b.add_unique_function (instance, return_type, argument_types,
			 group.required_extensions, force_direct_overloads);
}

/* GROUP describes some sort of gather or scatter operation.  There are
   two cases:

   - If the function has any type suffixes (as for loads and stores), the
     first function type suffix specifies either a 32-bit or a 64-bit type,
     which in turn selects either MODE32 or MODE64 as the addressing mode.
     Add a function instance for every type and predicate combination
     in GROUP for which the associated addressing mode is not MODE_none.

   - If the function has no type suffixes (as for prefetches), add one
     MODE32 form and one MODE64 form for each predication type.

   The other arguments are as for build_all.  */
static void
build_32_64 (function_builder &b, const char *signature,
	     const function_group_info &group, mode_suffix_index mode32,
	     mode_suffix_index mode64, bool force_direct_overloads = false)
{
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    for (unsigned int gi = 0; group.groups[gi] != NUM_GROUP_SUFFIXES; ++gi)
      if (group.types[0][0] == NUM_TYPE_SUFFIXES)
	{
	  gcc_assert (mode32 != MODE_none && mode64 != MODE_none);
	  build_one (b, signature, group, mode32, 0, gi, pi,
		     force_direct_overloads);
	  build_one (b, signature, group, mode64, 0, gi, pi,
		     force_direct_overloads);
	}
      else
	for (unsigned int ti = 0; group.types[ti][0] != NUM_TYPE_SUFFIXES;
	     ++ti)
	  {
	    unsigned int bits = type_suffixes[group.types[ti][0]].element_bits;
	    gcc_assert (bits == 32 || bits == 64);
	    mode_suffix_index mode = bits == 32 ? mode32 : mode64;
	    if (mode != MODE_none)
	      build_one (b, signature, group, mode, ti, gi, pi,
			 force_direct_overloads);
	  }
}

/* For every type and predicate combination in GROUP, add one function
   that takes a scalar (pointer) base and a signed vector array index,
   and another that instead takes an unsigned vector array index.
   The vector array index has the same element size as the first
   function type suffix.  SIGNATURE is as for build_all.  */
static void
build_sv_index (function_builder &b, const char *signature,
		const function_group_info &group)
{
  build_32_64 (b, signature, group, MODE_s32index, MODE_s64index);
  build_32_64 (b, signature, group, MODE_u32index, MODE_u64index);
}

/* Like build_sv_index, but only handle 64-bit types.  */
static void
build_sv_index64 (function_builder &b, const char *signature,
		  const function_group_info &group)
{
  build_32_64 (b, signature, group, MODE_none, MODE_s64index);
  build_32_64 (b, signature, group, MODE_none, MODE_u64index);
}

/* Like build_sv_index, but taking vector byte offsets instead of vector
   array indices.  */
static void
build_sv_offset (function_builder &b, const char *signature,
		 const function_group_info &group)
{
  build_32_64 (b, signature, group, MODE_s32offset, MODE_s64offset);
  build_32_64 (b, signature, group, MODE_u32offset, MODE_u64offset);
}

/* Like build_sv_offset, but exclude offsets that must be interpreted
   as signed (i.e. s32offset).  */
static void
build_sv_uint_offset (function_builder &b, const char *signature,
		      const function_group_info &group)
{
  build_32_64 (b, signature, group, MODE_none, MODE_s64offset);
  build_32_64 (b, signature, group, MODE_u32offset, MODE_u64offset);
}

/* For every type and predicate combination in GROUP, add a function
   that takes a vector base address and no displacement.  The vector
   base has the same element size as the first type suffix.

   The other arguments are as for build_all.  */
static void
build_v_base (function_builder &b, const char *signature,
	      const function_group_info &group,
	      bool force_direct_overloads = false)
{
  build_32_64 (b, signature, group, MODE_u32base, MODE_u64base,
	       force_direct_overloads);
}

/* Like build_v_base, but for functions that also take a scalar array
   index.  */
static void
build_vs_index (function_builder &b, const char *signature,
		const function_group_info &group,
		bool force_direct_overloads = false)
{
  build_32_64 (b, signature, group, MODE_u32base_index, MODE_u64base_index,
	       force_direct_overloads);
}

/* Like build_v_base, but for functions that also take a scalar byte
   offset.  */
static void
build_vs_offset (function_builder &b, const char *signature,
		 const function_group_info &group,
		 bool force_direct_overloads = false)
{
  build_32_64 (b, signature, group, MODE_u32base_offset, MODE_u64base_offset,
	       force_direct_overloads);
}

/* Add a function instance for every type and predicate combination
   in GROUP.  Take the function base name from GROUP and the mode suffix
   from MODE_SUFFIX_ID.  Use SIGNATURE to construct the function signature
   without a governing predicate, then use apply_predication to add in the
   predicate.  FORCE_DIRECT_OVERLOADS is true if there is a one-to-one
   mapping between "short" and "full" names, and if standard overload
   resolution therefore isn't necessary.  */
static void
build_all (function_builder &b, const char *signature,
	   const function_group_info &group, mode_suffix_index mode_suffix_id,
	   bool force_direct_overloads = false)
{
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    for (unsigned int gi = 0; group.groups[gi] != NUM_GROUP_SUFFIXES; ++gi)
      for (unsigned int ti = 0;
	   ti == 0 || group.types[ti][0] != NUM_TYPE_SUFFIXES; ++ti)
	build_one (b, signature, group, mode_suffix_id, ti, gi, pi,
		   force_direct_overloads);
}

/* TYPE is the largest type suffix associated with the arguments of R,
   but the result is twice as wide.  Return the associated type suffix
   if it exists, otherwise report an appropriate error and return
   NUM_TYPE_SUFFIXES.  */
static type_suffix_index
long_type_suffix (function_resolver &r, type_suffix_index type)
{
  unsigned int element_bits = type_suffixes[type].element_bits;
  if (type_suffixes[type].integer_p && element_bits < 64)
    return find_type_suffix (type_suffixes[type].tclass, element_bits * 2);

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
  explicit_type_suffix_p (unsigned int) const override
  {
    return true;
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
  explicit_type_suffix_p (unsigned int i) const override
  {
    return (EXPLICIT_MASK >> i) & 1;
  }
};

/* Base class for adr_index and adr_offset.  */
struct adr_base : public overloaded_base<0>
{
  /* The function takes two arguments: a vector base and a vector displacement
     (either an index or an offset).  Resolve based on them both.  */
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    mode_suffix_index mode;
    if (!r.check_gp_argument (2, i, nargs)
	|| (mode = r.resolve_adr_address (0)) == MODE_none)
      return error_mark_node;

    return r.resolve_to (mode);
  };
};

/* Base class for narrowing bottom binary functions that take an
   immediate second operand.  The result is half the size of input
   and has class CLASS.  */
template<type_class_index CLASS = function_resolver::SAME_TYPE_CLASS>
struct binary_imm_narrowb_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    STATIC_ASSERT (CLASS == function_resolver::SAME_TYPE_CLASS
		   || CLASS == TYPE_unsigned);
    if (CLASS == TYPE_unsigned)
      build_all (b, "vhu0,v0,su64", group, MODE_n);
    else
      build_all (b, "vh0,v0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1, 1);
  }
};

/* The top equivalent of binary_imm_narrowb_base.  It takes three arguments,
   with the first being the values of the even elements, which are typically
   the result of the narrowb operation.  */
template<type_class_index CLASS = function_resolver::SAME_TYPE_CLASS>
struct binary_imm_narrowt_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    STATIC_ASSERT (CLASS == function_resolver::SAME_TYPE_CLASS
		   || CLASS == TYPE_unsigned);
    if (CLASS == TYPE_unsigned)
      build_all (b, "vhu0,vhu0,v0,su64", group, MODE_n);
    else
      build_all (b, "vh0,vh0,v0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i, i + 1, type, CLASS, r.HALF_SIZE)
	|| !r.require_integer_immediate (i + 2))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};

/* Base class for long (i.e. narrow op narrow -> wide) binary functions
   that take an immediate second operand.  The type suffix specifies
   the wider type.  */
struct binary_imm_long_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,vh0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, result_type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_integer_immediate (i + 1)
	|| (result_type = long_type_suffix (r, type)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    if (tree res = r.lookup_form (r.mode_suffix_id, result_type))
      return res;

    return r.report_no_such_form (type);
  }
};

/* Base class for binary_za_m and similar shapes.  */
template<type_class_index TCLASS = function_resolver::SAME_TYPE_CLASS,
	 unsigned int BITS = function_resolver::SAME_SIZE>
struct binary_za_m_base : public overloaded_base<1>
{
  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    if (!r.check_num_arguments (5)
	|| !r.require_integer_immediate (0)
	|| !r.require_vector_type (1, VECTOR_TYPE_svbool_t)
	|| !r.require_vector_type (2, VECTOR_TYPE_svbool_t)
	|| (type = r.infer_vector_type (3)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (4, 3, type, TCLASS, BITS))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};

/* Base class for shapes like binary_za_slice_lane.  TCLASS is the type
   class of the final vector argument.  */
template<type_class_index TCLASS = function_resolver::SAME_TYPE_CLASS>
struct binary_za_slice_lane_base : public overloaded_base<1>
{
  constexpr binary_za_slice_lane_base (unsigned int lane_type_suffix)
    : m_lane_type_suffix (lane_type_suffix) {}

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1,v1,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (4)
	|| !r.require_scalar_type (0, "uint32_t")
	|| !(type = r.infer_tuple_type (1))
	|| !r.require_derived_vector_type (2, 1, type, TCLASS)
	|| !r.require_integer_immediate (3))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bytes = c.type_suffix (m_lane_type_suffix).element_bytes;
    return c.require_immediate_range (3, 0, 16 / bytes - 1);
  }

  unsigned int m_lane_type_suffix;
};

/* Base class for shapes like binary_za_slice_opt_single.  TCLASS is the
   type class of the final argument.  */
template<type_class_index TCLASS = function_resolver::SAME_TYPE_CLASS>
struct binary_za_slice_opt_single_base : public overloaded_base<1>
{
  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (3)
	|| !r.require_scalar_type (0, "uint32_t")
	|| !(type = r.infer_tuple_type (1)))
      return error_mark_node;

    return r.finish_opt_single_resolution (2, 1, type, TCLASS);
  }
};

/* Base class for ext and extq.  */
struct ext_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }
};

/* Base class for inc_dec and inc_dec_pat.  */
struct inc_dec_base : public overloaded_base<0>
{
  CONSTEXPR inc_dec_base (bool pat_p) : m_pat_p (pat_p) {}

  /* Resolve based on the first argument only, which must be either a
     scalar or a vector.  If it's a scalar, it must be a 32-bit or
     64-bit integer.  */
  tree
  resolve (function_resolver &r) const
  {
    unsigned int i, nargs;
    if (!r.check_gp_argument (m_pat_p ? 3 : 2, i, nargs)
	|| !r.require_vector_or_scalar_type (i))
      return error_mark_node;

    mode_suffix_index mode;
    type_suffix_index type;
    if (r.scalar_argument_p (i))
      {
	mode = MODE_n;
	type = r.infer_integer_scalar_type (i);
      }
    else
      {
	mode = MODE_none;
	type = r.infer_vector_type (i);
      }
    if (type == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    for (++i; i < nargs; ++i)
      if (!r.require_integer_immediate (i))
	return error_mark_node;

    return r.resolve_to (mode, type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (m_pat_p ? 2 : 1, 1, 16);
  }

  bool m_pat_p;
};

/* Base class for load and load_replicate.  */
struct load_contiguous_base : public overloaded_base<0>
{
  /* Resolve a call based purely on a pointer argument.  The other arguments
     are a governing predicate and (for MODE_vnum) a vnum offset.  */
  tree
  resolve (function_resolver &r) const override
  {
    bool vnum_p = r.mode_suffix_id == MODE_vnum;
    gcc_assert (r.mode_suffix_id == MODE_none || vnum_p);

    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (vnum_p ? 2 : 1, i, nargs)
	|| (type = r.infer_pointer_type (i)) == NUM_TYPE_SUFFIXES
	|| (vnum_p && !r.require_scalar_type (i + 1, "int64_t")))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type, NUM_TYPE_SUFFIXES,
			 r.group_suffix_id);
  }
};

/* Base class for gather loads that take a scalar base and a vector
   displacement (either an offset or an index).  */
struct load_gather_sv_base : public overloaded_base<0>
{
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    mode_suffix_index mode;
    type_suffix_index type;
    auto restrictions = get_target_type_restrictions (r);
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_pointer_type (i, true,
					 restrictions)) == NUM_TYPE_SUFFIXES
	|| (mode = r.resolve_sv_displacement (i + 1, type, true),
	    mode == MODE_none))
      return error_mark_node;

    return r.resolve_to (mode, type);
  }

  virtual function_resolver::target_type_restrictions
  get_target_type_restrictions (const function_instance &) const
  {
    return function_resolver::TARGET_32_64;
  }
};

/* Base class for load_gather64_sv_index and load_gather64_sv_offset.  */
struct load_gather64_sv_base : public load_gather_sv_base
{
  type_suffix_index
  vector_base_type (type_suffix_index) const override
  {
    return TYPE_SUFFIX_u64;
  }

  function_resolver::target_type_restrictions
  get_target_type_restrictions (const function_instance &) const override
  {
    return function_resolver::TARGET_ANY;
  }
};

/* Base class for load_ext_gather_index and load_ext_gather_offset,
   which differ only in the units of the displacement.  */
struct load_ext_gather_base : public overloaded_base<1>
{
  /* Resolve a gather load that takes one of:

     - a scalar pointer base and a vector displacement
     - a vector base with no displacement or
     - a vector base and a scalar displacement

     The function has an explicit type suffix that determines the type
     of the loaded data.  */
  tree
  resolve (function_resolver &r) const override
  {
    /* No resolution is needed for a vector base with no displacement;
       there's a one-to-one mapping between short and long names.  */
    gcc_assert (r.displacement_units () != UNITS_none);

    type_suffix_index type = r.type_suffix_ids[0];

    unsigned int i, nargs;
    mode_suffix_index mode;
    if (!r.check_gp_argument (2, i, nargs)
	|| (mode = r.resolve_gather_address (i, type, true)) == MODE_none)
      return error_mark_node;

    return r.resolve_to (mode, type);
  }
};


/* sv<v0>_t svlut[_<t0>_g](sv<t0>x<g>_t, svuint8_t, uint64_t)

   where the final argument is a constant index, the instruction divides
   the vector argument in BITS-bit quantities.  */
template<unsigned int BITS>
struct luti_base : public overloaded_base<0>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    /* Format: return type, table vector, indices vector, immediate value.  */
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,t0,vu8,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    auto max_range = c.type_suffix (0).element_bits / BITS - 1;
    return c.require_immediate_range (2, 0, max_range);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (3)
	|| !(type = r.infer_sve_type (0))
	|| !r.require_vector_type (1, VECTOR_TYPE_svuint8_t)
	|| !r.require_scalar_type (2, "uint64_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};

/* Specializations for 2-bit and 4-bit indices.  */
using luti2_def = luti_base<2>;
SHAPE (luti2)

using luti4_def = luti_base<4>;
SHAPE (luti4)


/* sv<t0>x<g>_t svfoo_t0_g(uint64_t, svuint8_t, uint64_t)

   where the first argument is the ZT register number (currently always 0)
   and the final argument is a constant index.  The instruction divides
   the vector argument in BITS-bit quantities.  */
template<unsigned int BITS>
struct luti_lane_zt_base : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t0,su64,vu8,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    auto nvectors = c.vectors_per_tuple ();
    return (c.require_immediate_range (0, 0, 0)
	    && c.require_immediate_range (2, 0, 32 / BITS / nvectors - 1));
  }
};

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:quarter>_t,
		       sv<t0:quarter>_t)  (for integer t0)
   sv<t0>_t svmmla[_t0](sv<t0>_t, sv<t0>_t, sv<t0>_t)  (for floating-point t0)

   The functions act like the equivalent of "ternary_qq" for integer elements
   and normal vector-only ternary functions for floating-point elements.  */
struct mmla_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    if (type_suffixes[group.types[0][0]].float_p)
      build_all (b, "v0,v0,v0,v0", group, MODE_none);
    else
      build_all (b, "v0,v0,vq0,vq0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    /* Make sure that the function exists now, since not all forms
       follow a set pattern after this point.  */
    tree res = r.resolve_to (r.mode_suffix_id, type);
    if (res == error_mark_node)
      return res;

    bool float_p = type_suffixes[type].float_p;
    unsigned int modifier = float_p ? r.SAME_SIZE : r.QUARTER_SIZE;
    if (!r.require_derived_vector_type (i + 1, i, type, r.SAME_TYPE_CLASS,
					modifier)
	|| !r.require_derived_vector_type (i + 2, i, type, r.SAME_TYPE_CLASS,
					   modifier))
      return error_mark_node;

    return res;
  }
};
SHAPE (mmla)

/* Base class for prefetch_gather_index and prefetch_gather_offset,
   which differ only in the units of the displacement.  */
struct prefetch_gather_base : public overloaded_base<0>
{
  /* Resolve a gather prefetch that takes one of:

     - a scalar pointer base (const void *) and a vector displacement
     - a vector base with no displacement or
     - a vector base and a scalar displacement

     The prefetch operation is the final argument.  This is purely a
     mode-based resolution; there are no type suffixes.  */
  tree
  resolve (function_resolver &r) const override
  {
    bool has_displacement_p = r.displacement_units () != UNITS_none;

    unsigned int i, nargs;
    mode_suffix_index mode;
    if (!r.check_gp_argument (has_displacement_p ? 3 : 2, i, nargs)
	|| (mode = r.resolve_gather_address (i, NUM_TYPE_SUFFIXES,
					     false)) == MODE_none
	|| !r.require_integer_immediate (nargs - 1))
      return error_mark_node;

    return r.resolve_to (mode);
  }
};

/* Wraps BASE to provide a narrowing shift right function.  Argument N
   is an immediate shift amount in the range [1, sizeof(<t0>_t) * 4].  */
template<typename BASE, unsigned int N>
struct shift_right_imm_narrow_wrapper : public BASE
{
  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits / 2;
    return c.require_immediate_range (N, 1, bits);
  }
};

/* Base class for store_scatter_index and store_scatter_offset,
   which differ only in the units of the displacement.  */
struct store_scatter_base : public overloaded_base<0>
{
  /* Resolve a scatter store that takes one of:

     - a scalar pointer base and a vector displacement
     - a vector base with no displacement or
     - a vector base and a scalar displacement

     The stored data is the final argument, and it determines the
     type suffix.  */
  tree
  resolve (function_resolver &r) const override
  {
    bool has_displacement_p = r.displacement_units () != UNITS_none;

    unsigned int i, nargs;
    mode_suffix_index mode;
    type_suffix_index type;
    if (!r.check_gp_argument (has_displacement_p ? 3 : 2, i, nargs)
	|| (type = infer_vector_type (r, nargs - 1)) == NUM_TYPE_SUFFIXES
	|| (mode = r.resolve_gather_address (i, type, false)) == MODE_none)
      return error_mark_node;

    return r.resolve_to (mode, type);
  }

  virtual type_suffix_index
  infer_vector_type (function_resolver &r, unsigned int argno) const
  {
    return r.infer_sd_vector_type (argno);
  }
};

/* Base class for store_scatter64_index and store_scatter64_offset.  */
struct store_scatter64_base : public store_scatter_base
{
  type_suffix_index
  vector_base_type (type_suffix_index) const override
  {
    return TYPE_SUFFIX_u64;
  }

  type_suffix_index
  infer_vector_type (function_resolver &r, unsigned int argno) const override
  {
    return r.infer_vector_type (argno);
  }
};

/* Base class for ternary operations in which the final argument is an
   immediate shift amount.  The derived class should check the range.  */
struct ternary_shift_imm_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,v0,v0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }
};

/* Base class for ternary operations in which the first argument has the
   same element type as the result, and in which the second and third
   arguments have an element type that is derived the first.

   MODIFIER is the number of element bits in the second and third
   arguments, or a function_resolver modifier that says how this
   precision is derived from the first argument's elements.

   TYPE_CLASS2 and TYPE_CLASS3 are the type classes of the second and
   third arguments, or function_resolver::SAME_TYPE_CLASS if the type
   class is the same as the first argument.  */
template<unsigned int MODIFIER,
	 type_class_index TYPE_CLASS2 = function_resolver::SAME_TYPE_CLASS,
	 type_class_index TYPE_CLASS3 = function_resolver::SAME_TYPE_CLASS>
struct ternary_resize2_opt_n_base : public overloaded_base<0>
{
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, TYPE_CLASS2,
					   MODIFIER))
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 2, i, type, TYPE_CLASS3, MODIFIER);
  }
};

/* Like ternary_resize2_opt_n_base, but for functions that don't take
   a final scalar argument.  */
template<unsigned int MODIFIER,
	 type_class_index TYPE_CLASS2 = function_resolver::SAME_TYPE_CLASS,
	 type_class_index TYPE_CLASS3 = function_resolver::SAME_TYPE_CLASS>
struct ternary_resize2_base : public overloaded_base<0>
{
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, TYPE_CLASS2,
					   MODIFIER)
	|| !r.require_derived_vector_type (i + 2, i, type, TYPE_CLASS3,
					   MODIFIER))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};

/* Like ternary_resize2_opt_n_base, but for functions that take a final
   lane argument.  */
template<unsigned int MODIFIER,
	 type_class_index TYPE_CLASS2 = function_resolver::SAME_TYPE_CLASS,
	 type_class_index TYPE_CLASS3 = function_resolver::SAME_TYPE_CLASS>
struct ternary_resize2_lane_base : public overloaded_base<0>
{
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (4, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, TYPE_CLASS2,
					   MODIFIER)
	|| !r.require_derived_vector_type (i + 2, i, type, TYPE_CLASS3,
					   MODIFIER)
	|| !r.require_integer_immediate (i + 3))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};

/* A specialization of ternary_resize2_lane_base for bfloat16 elements,
   indexed in groups of N elements.  */
template<unsigned int N>
struct ternary_bfloat_lane_base
  : public ternary_resize2_lane_base<16, TYPE_bfloat, TYPE_bfloat>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vB,vB,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (3, 2, N);
  }
};

/* A specialization of ternary_resize2_lane_base for quarter-sized
   elements.  */
template<type_class_index TYPE_CLASS2 = function_resolver::SAME_TYPE_CLASS,
	 type_class_index TYPE_CLASS3 = function_resolver::SAME_TYPE_CLASS>
struct ternary_qq_lane_base
  : public ternary_resize2_lane_base<function_resolver::QUARTER_SIZE,
				     TYPE_CLASS2, TYPE_CLASS3>
{
  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (3, 0);
  }
};

/* Base class for narrowing bottom unary functions.  The result is half
   the size of input and has class CLASS.  */
template<type_class_index CLASS = function_resolver::SAME_TYPE_CLASS>
struct unary_narrowb_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    STATIC_ASSERT (CLASS == function_resolver::SAME_TYPE_CLASS
		   || CLASS == TYPE_unsigned);
    if (CLASS == TYPE_unsigned)
      build_all (b, "vhu0,v0", group, MODE_none);
    else
      build_all (b, "vh0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary (CLASS, r.HALF_SIZE);
  }
};

/* The top equivalent of unary_imm_narrowb_base.  All forms take the values
   of the even elements as an extra argument, before any governing predicate.
   These even elements are typically the result of the narrowb operation.  */
template<type_class_index CLASS = function_resolver::SAME_TYPE_CLASS>
struct unary_narrowt_base : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    STATIC_ASSERT (CLASS == function_resolver::SAME_TYPE_CLASS
		   || CLASS == TYPE_unsigned);
    if (CLASS == TYPE_unsigned)
      build_all (b, "vhu0,vhu0,v0", group, MODE_none);
    else
      build_all (b, "vh0,vh0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i, i + 1, type, CLASS, r.HALF_SIZE))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};

/* sv<m0>_t svfoo[_m0base]_[m1]index(sv<m0>_t, sv<m1>_t)

   for all valid combinations of vector base type <m0> and vector
   displacement type <m1>.  */
struct adr_index_def : public adr_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_all (b, "b,b,d", group, MODE_u32base_s32index);
    build_all (b, "b,b,d", group, MODE_u32base_u32index);
    build_all (b, "b,b,d", group, MODE_u64base_s64index);
    build_all (b, "b,b,d", group, MODE_u64base_u64index);
  }
};
SHAPE (adr_index)

/* sv<m0>_t svfoo[_m0base]_[m1]offset(sv<m0>_t, sv<m1>_t).

   for all valid combinations of vector base type <m0> and vector
   displacement type <m1>.  */
struct adr_offset_def : public adr_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_offset);
    build_all (b, "b,b,d", group, MODE_u32base_s32offset);
    build_all (b, "b,b,d", group, MODE_u32base_u32offset);
    build_all (b, "b,b,d", group, MODE_u64base_s64offset);
    build_all (b, "b,b,d", group, MODE_u64base_u64offset);
  }
};
SHAPE (adr_offset)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t)

   i.e. a binary operation with uniform types, but with no scalar form.  */
struct binary_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2);
  }
};
SHAPE (binary)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:int>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0:int>_t).

   i.e. a version of the standard binary shape binary_opt_n in which
   the final argument is always a signed integer.  */
struct binary_int_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vs0", group, MODE_none);
    build_all (b, "v0,v0,ss0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 1, i, type, TYPE_signed);
  }
};
SHAPE (binary_int_opt_n)

/* Like binary_int_opt_n for single vectors.  For tuples:

   sv<t0>x<g>_t svfoo[_t0_g](sv<t0>x<g>_t, sv<t0:int>x<g>_t)
   sv<t0>x<g>_t svfoo[_single_t0_g](sv<t0>x<g>_t, sv<t0:int>_t).  */
struct binary_int_opt_single_n_def : public overloaded_base<0>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,ts0", group, MODE_none);
    if (group.groups[0] == GROUP_none)
      build_all (b, "v0,v0,ss0", group, MODE_n);
    else
      build_all (b, "t0,t0,vs0", group, MODE_single);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    sve_type type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !(type = r.infer_sve_type (i)))
      return error_mark_node;

    return (type.num_vectors == 1 && r.scalar_argument_p (i + 1)
	    ? r.finish_opt_n_resolution (i + 1, i, type.type, TYPE_signed)
	    : r.finish_opt_single_resolution (i + 1, i, type, TYPE_signed));
  }
};
SHAPE (binary_int_opt_single_n)

/* sv<t0>_t svfoo_<t0>(sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 16 / sizeof (<t0>_t) - 1].  */
struct binary_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (2, 1);
  }
};
SHAPE (binary_lane)

/* sv<t0>_t svfoo[_t0](sv<t0:half>_t, sv<t0:half>_t, uint64_t).

   where the final argument is an integer constant expression in the
   range [0, 32 / sizeof (<t0>_t) - 1].  */
struct binary_long_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vh0,vh0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, result_type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_matching_vector_type (i + 1, i, type)
	|| !r.require_integer_immediate (i + 2)
	|| (result_type = long_type_suffix (r, type)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    if (tree res = r.lookup_form (r.mode_suffix_id, result_type))
      return res;

    return r.report_no_such_form (type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (2, 1);
  }
};
SHAPE (binary_long_lane)

/* sv<t0>_t svfoo[_t0](sv<t0:half>_t, sv<t0:half>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0:half>_t, <t0:half>_t).  */
struct binary_long_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vh0,vh0", group, MODE_none);
    build_all (b, "v0,vh0,sh0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, result_type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| (result_type = long_type_suffix (r, type)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 1, i, type, r.SAME_TYPE_CLASS,
				      r.SAME_SIZE, result_type);
  }
};
SHAPE (binary_long_opt_n)

/* sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0>_t).

   i.e. a binary operation in which the final argument is always a scalar
   rather than a vector.  */
struct binary_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_scalar_type (i + 1, r.SAME_TYPE_CLASS))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_n)

/* sv<t0:half>_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   sv<t0:half>_t svfoo[_n_t0](sv<t0>_t, <t0>_t)

   i.e. a version of binary_opt_n in which the output elements are half the
   width of the input elements.  */
struct binary_narrowb_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vh0,v0,v0", group, MODE_none);
    build_all (b, "vh0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (binary_narrowb_opt_n)

/* sv<t0:half>_t svfoo[_t0](sv<t0:half>_t, sv<t0>_t, sv<t0>_t)
   sv<t0:half>_t svfoo[_n_t0](sv<t0:half>_t, sv<t0>_t, <t0>_t)

   This is the "top" counterpart to binary_narrowb_opt_n.  */
struct binary_narrowt_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vh0,vh0,v0,v0", group, MODE_none);
    build_all (b, "vh0,vh0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i, i + 1, type, r.SAME_TYPE_CLASS,
					   r.HALF_SIZE))
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 2, i + 1, type);
  }
};
SHAPE (binary_narrowt_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0>_t)

   i.e. the standard shape for binary operations that operate on
   uniform types.  */
struct binary_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0", group, MODE_none);
    /* _b functions do not have an _n form, but are classified as
       binary_opt_n so that they can be overloaded with vector
       functions.  */
    if (group.types[0][0] == TYPE_SUFFIX_b)
      gcc_assert (group.types[0][1] == NUM_TYPE_SUFFIXES);
    else
      build_all (b, "v0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (binary_opt_n)

/* Like binary_opt_n for single vectors.  For tuples:

   sv<t0>x<g>_t svfoo[_t0_g](sv<t0>x<g>_t, sv<t0>x<g>_t)
   sv<t0>x<g>_t svfoo[_single_t0_g](sv<t0>x<g>_t, sv<t0>_t).  */
struct binary_opt_single_n_def : public overloaded_base<0>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,t0", group, MODE_none);
    if (group.groups[0] == GROUP_none)
      build_all (b, "v0,v0,s0", group, MODE_n);
    else
      build_all (b, "t0,t0,v0", group, MODE_single);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    sve_type type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !(type = r.infer_sve_type (i)))
      return error_mark_node;

    return (type.num_vectors == 1 && r.scalar_argument_p (i + 1)
	    ? r.finish_opt_n_resolution (i + 1, i, type.type)
	    : r.finish_opt_single_resolution (i + 1, i, type));
  }
};
SHAPE (binary_opt_single_n)

/* svbool_t svfoo(svbool_t, svbool_t).  */
struct binary_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "v0,v0,v0", group, MODE_none);
  }
};
SHAPE (binary_pred)

/* sv<t0>_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument must be 90 or 270.  */
struct binary_rotate_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_either_or (2, 90, 270);
  }
};
SHAPE (binary_rotate)

/* sv<t0>_t svfoo_t0(<t0>_t, <t0>_t)

   i.e. a binary function that takes two scalars and returns a vector.
   An explicit type suffix is required.  */
struct binary_scalar_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "v0,s0,s0", group, MODE_none);
  }
};
SHAPE (binary_scalar)

/* sv<t0>x<g>_t svfoo[_single_t0_g](sv<t0>x<g>_t, sv<t0>_t).  */
struct binary_single_def : public overloaded_base<0>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,v0", group, MODE_single);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (2)
	|| !(type = r.infer_sve_type (0))
	|| !r.require_derived_vector_type (1, 0, type, r.SAME_TYPE_CLASS,
					   r.SAME_SIZE, 1))
      return error_mark_node;

    return r.resolve_to (MODE_single, type);
  }
};
SHAPE (binary_single)

/* sv<t0:uint>_t svfoo[_t0](sv<t0>_t, sv<t0>_t).

   i.e. a version of "binary" that returns unsigned integers.  */
struct binary_to_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vu0,v0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2);
  }
};
SHAPE (binary_to_uint)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:uint>_t)

   i.e. a version of "binary" in which the final argument is always an
   unsigned integer.  */
struct binary_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vu0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, TYPE_unsigned))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_uint)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, <t0:uint>_t)

   i.e. a version of binary_n in which the final argument is always an
   unsigned integer.  */
struct binary_uint_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,su0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_scalar_type (i + 1, TYPE_unsigned))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_uint_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:uint>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0:uint>_t)

   i.e. a version of the standard binary shape binary_opt_n in which
   the final argument is always an unsigned integer.  */
struct binary_uint_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vu0", group, MODE_none);
    build_all (b, "v0,v0,su0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 1, i, type, TYPE_unsigned);
  }
};
SHAPE (binary_uint_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, uint64_t).

   i.e. a version of binary_n in which the final argument is always
   a 64-bit unsigned integer.  */
struct binary_uint64_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_scalar_type (i + 1, "uint64_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_uint64_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svuint64_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, uint64_t)

   i.e. a version of the standard binary shape binary_opt_n in which
   the final argument is always a uint64_t.  */
struct binary_uint64_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vu64", group, MODE_none);
    build_all (b, "v0,v0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 1, i, type, TYPE_unsigned, 64);
  }
};
SHAPE (binary_uint64_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:half>_t).  */
struct binary_wide_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vh0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, r.SAME_TYPE_CLASS,
					   r.HALF_SIZE))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binary_wide)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:half>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0:half>_t).  */
struct binary_wide_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vh0", group, MODE_none);
    build_all (b, "v0,v0,sh0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 1, i, type, r.SAME_TYPE_CLASS,
				      r.HALF_SIZE);
  }
};
SHAPE (binary_wide_opt_n)

/* void svfoo_t0[_t1]_g(uint64_t, svbool_t, svbool_t, sv<t1>x<g>_t,
			sv<t1:int>x<g>_t)

   where the first argument is a ZA tile.  */
struct binary_za_int_m_def : public binary_za_m_base<TYPE_signed>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su64,vp,vp,t1,ts1", group, MODE_none);
  }
};
SHAPE (binary_za_int_m)

/* void svfoo_t0[_t1]_g(uint64_t, svbool_t, svbool_t, sv<t1>x<g>_t,
			sv<t1>x<g>_t)

   where the first argument is a ZA tile.  */
struct binary_za_m_def : public binary_za_m_base<>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    /* Allow the overloaded form to be specified seperately, with just
       a single suffix.  This is necessary for the 64-bit SME MOP intrinsics,
       which have some forms dependent on FEAT_SME_I16I64 and some forms
       dependent on FEAT_SME_F64F64.  The resolver needs to be defined
       for base SME.  */
    if (group.types[0][1] != NUM_TYPE_SUFFIXES)
      build_all (b, "_,su64,vp,vp,t1,t1", group, MODE_none);
  }
};
SHAPE (binary_za_m)

/* void svfoo_lane_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1>_t, uint64_t)

   where the first argument is a variable ZA slice and the final argument
   indexes a single element in the preceding vector argument.  */
struct binary_za_slice_lane_def : public binary_za_slice_lane_base<>
{
  constexpr binary_za_slice_lane_def () : binary_za_slice_lane_base<> (1) {}
};
SHAPE (binary_za_slice_lane)

/* void svfoo_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1:int>x<g>_t)
   void svfoo[_single]_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1:int>_t).

   where the first argument is a variable ZA slice.  */
struct binary_za_slice_int_opt_single_def
  : public binary_za_slice_opt_single_base<TYPE_signed>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1,ts1", group, MODE_none);
    build_all (b, "_,su32,t1,vs1", group, MODE_single);
  }
};
SHAPE (binary_za_slice_int_opt_single)

/* void svfoo_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1>x<g>_t)
   void svfoo[_single]_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1>_t)

   where the first argument is a variable ZA slice.  */
struct binary_za_slice_opt_single_def
  : public binary_za_slice_opt_single_base<>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1,t1", group, MODE_none);
    build_all (b, "_,su32,t1,v1", group, MODE_single);
  }
};
SHAPE (binary_za_slice_opt_single)

/* void svfoo_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1:uint>x<g>_t)
   void svfoo[_single]_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1:uint>_t)

   where the first argument is a variable ZA slice.  */
struct binary_za_slice_uint_opt_single_def
  : public binary_za_slice_opt_single_base<TYPE_unsigned>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1,tu1", group, MODE_none);
    build_all (b, "_,su32,t1,vu1", group, MODE_single);
  }
};
SHAPE (binary_za_slice_uint_opt_single)

/* void svfoo_t0[_t1]_g(uint64_t, svbool_t, svbool_t, sv<t1>x<g>_t,
			sv<t1:uint>x<g>_t)

   where the first argument is a ZA tile.  */
struct binary_za_uint_m_def : public binary_za_m_base<TYPE_unsigned>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su64,vp,vp,t1,tu1", group, MODE_none);
  }
};
SHAPE (binary_za_uint_m)

/* sv<t0>x<g>_t svfoo[_t0_t1_g](sv<t0>x<g>_t, sv<t0>x<g>_t).  */
struct binaryxn_def : public overloaded_base<0>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,t0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    vector_type_index pred_type;
    sve_type type;
    if (!r.check_num_arguments (3)
	|| (pred_type = r.infer_predicate_type (0)) == NUM_VECTOR_TYPES
	|| !(type = r.infer_sve_type (1))
	|| !r.require_matching_predicate_type (pred_type, type)
	|| !r.require_matching_vector_type (2, 1, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (binaryxn)

/* bool svfoo().  */
struct bool_inherent_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "sp", group, MODE_none);
  }
};
SHAPE (bool_inherent)

/* Either:

     sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, sv<t0>_t)

   for single vectors or:

     sv<t0>x<g>_t svfoo[_single_t0_g](sv<t0>x<g>_t, sv<t0>_t, sv<t0>_t)

   for tuples.  */
struct clamp_def : public overloaded_base<0>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,v0,v0", group,
	       group.groups[0] == GROUP_none ? MODE_none : MODE_single);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (3)
	|| !(type = r.infer_sve_type (0))
	|| !r.require_derived_vector_type (1, 0, type, r.SAME_TYPE_CLASS,
					   r.SAME_SIZE, 1)
	|| !r.require_derived_vector_type (2, 0, type, r.SAME_TYPE_CLASS,
					   r.SAME_SIZE, 1))
      return error_mark_node;

    auto mode = type.num_vectors == 1 ? MODE_none : MODE_single;
    return r.resolve_to (mode, type);
  }
};
SHAPE (clamp)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   <t0>_t svfoo[_n_t0](<t0>_t, sv<t0>_t).  */
struct clast_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0", group, MODE_none);
    build_all (b, "s0,s0,v0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_vector_or_scalar_type (i))
      return error_mark_node;

    if (r.scalar_argument_p (i))
      {
	type_suffix_index type;
	if (!r.require_derived_scalar_type (i, r.SAME_TYPE_CLASS)
	    || (type = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES)
	  return error_mark_node;
	return r.resolve_to (MODE_n, type);
      }
    else
      {
	type_suffix_index type;
	if ((type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	    || !r.require_matching_vector_type (i + 1, i, type))
	  return error_mark_node;
	return r.resolve_to (MODE_none, type);
      }
  }
};
SHAPE (clast)

/* svbool_t svfoo[_t0](sv<t0>_t, sv<t0>_t).  */
struct compare_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2);
  }
};
SHAPE (compare)

/* svbool_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   svbool_t svfoo[_n_t0](sv<t0>_t, <t0>_t)

   i.e. a comparison between two vectors, or between a vector and a scalar.  */
struct compare_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0,v0", group, MODE_none);
    build_all (b, "vp,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (compare_opt_n)

/* svbool_t svfoo[_t0](const <t0>_t *, const <t0>_t *).  */
struct compare_ptr_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,al,al", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_pointer_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_matching_pointer_type (i + 1, i, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (compare_ptr)

/* svboolx<g>_t svfoo_t0[_t1]_g(<t1>_t, <t1>_t)

   where _t0 is a _b<bits> suffix that describes the predicate result.
   There is no direct relationship between the element sizes of _t0
   and _t1.  */
struct compare_scalar_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "tp,s1,s1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_integer_scalar_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_matching_integer_scalar_type (i + 1, i, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type,
			 r.group_suffix_id);
  }
};
SHAPE (compare_scalar)

/* svcount_t svfoo_t0[_t1](<t1>_t, <t1>_t, uint64_t)

   where _t0 is a _c<bits> suffix that describes the predicate-as-counter
   result.  The final argument is an integer constant that specifies the
   number of vectors (2 or 4).  */
struct compare_scalar_count_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,s1,s1,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_64bit_scalar_integer_pair (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_integer_immediate (i + 2))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_either_or (2, 2, 4);
  }
};
SHAPE (compare_scalar_count)

/* svbool_t svfoo[_t0](sv<t0>_t, svint64_t)  (for signed t0)
   svbool_t svfoo[_n_t0](sv<t0>_t, int64_t)  (for signed t0)
   svbool_t svfoo[_t0](sv<t0>_t, svuint64_t)  (for unsigned t0)
   svbool_t svfoo[_n_t0](sv<t0>_t, uint64_t)  (for unsigned t0)

   i.e. a comparison in which the second argument is 64 bits.  */
struct compare_wide_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0,vw0", group, MODE_none);
    build_all (b, "vp,v0,sw0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 1, i, type, r.SAME_TYPE_CLASS, 64);
  }
};
SHAPE (compare_wide_opt_n)

/* uint64_t svfoo().  */
struct count_inherent_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "su64", group, MODE_none);
  }
};
SHAPE (count_inherent)

/* uint64_t svfoo(enum svpattern).  */
struct count_pat_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "su64,epattern", group, MODE_none);
  }
};
SHAPE (count_pat)

/* uint64_t svfoo(svbool_t).  */
struct count_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "su64,vp", group, MODE_none);
  }
};
SHAPE (count_pred)

/* uint64_t svfoo_t0(sv<t0>_t, uint64_t)

   where the final argument must be 2 or 4.  */
struct count_pred_c_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "su64,v0,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_either_or (1, 2, 4);
  }
};
SHAPE (count_pred_c)

/* uint64_t svfoo[_t0](sv<t0>_t).  */
struct count_vector_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "su64,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (count_vector)

/* sv<t0>xN_t svfoo[_t0](sv<t0>_t, ..., sv<t0>_t)

   where there are N arguments in total.  */
struct create_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,v0*t", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (r.vectors_per_tuple ());
  }
};
SHAPE (create)

/* void svfoo_lane_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1:int>_t, uint64_t)

   where the final argument indexes a <t0>-sized group of elements in the
   preceding vector argument.  */
struct dot_za_slice_int_lane_def
  : public binary_za_slice_lane_base<TYPE_signed>
{
  constexpr dot_za_slice_int_lane_def ()
    : binary_za_slice_lane_base<TYPE_signed> (0) {}

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1,vs1,su64", group, MODE_none);
  }
};
SHAPE (dot_za_slice_int_lane)

/* void svfoo_lane_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1>_t, uint64_t)

   where the final argument indexes a <t0>-sized group of elements in the
   preceding vector argument.  */
struct dot_za_slice_lane_def : public binary_za_slice_lane_base<>
{
  constexpr dot_za_slice_lane_def () : binary_za_slice_lane_base<> (0) {}
};
SHAPE (dot_za_slice_lane)

/* void svfoo_lane_t0[_t1]_g(uint32_t, sv<t1>x<g>_t, sv<t1:uint>_t, uint64_t)

   where the final argument indexes a <t0>-sized group of elements in the
   preceding vector argument.  */
struct dot_za_slice_uint_lane_def
  : public binary_za_slice_lane_base<TYPE_unsigned>
{
  constexpr dot_za_slice_uint_lane_def ()
    : binary_za_slice_lane_base<TYPE_unsigned> (0) {}

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1,vu1,su64", group, MODE_none);
  }
};
SHAPE (dot_za_slice_uint_lane)

/* sv<t0>_t svfoo[_n]_t0(<t0>_t, ..., <t0>_t)

   where there are enough arguments to fill 128 bits of data (or to
   control 128 bits of data in the case of predicates).  */
struct dupq_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    /* The "_n" suffix is optional; the full name has it, but the short
       name doesn't.  */
    build_all (b, "v0,s0*q", group, MODE_n, true);
  }

  tree
  resolve (function_resolver &) const override
  {
    /* The short forms just make "_n" implicit, so no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (dupq)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression that when
   multiplied by the number of bytes in t0 is in the range [0, 255].  */
struct ext_def : public ext_base
{
  bool
  check (function_checker &c) const override
  {
    unsigned int bytes = c.type_suffix (0).element_bytes;
    return c.require_immediate_range (2, 0, 256 / bytes - 1);
  }
};
SHAPE (ext)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression that when
   multiplied by the number of bytes in t0 is in the range [0, 15].  */
struct extq_def : public ext_base
{
  bool
  check (function_checker &c) const override
  {
    unsigned int bytes = c.type_suffix (0).element_bytes;
    return c.require_immediate_range (2, 0, 16 / bytes - 1);
  }
};
SHAPE (extq)

/* svboolx<g>_t svfoo_t0_g(sv<t0>_t, sv<t0>_t, uint32_t).  */
struct extract_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "tp,vc,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int size = c.vectors_per_tuple ();
    return c.require_immediate_range (1, 0, 4 / size - 1);
  }
};
SHAPE (extract_pred)

/* <t0>_t svfoo[_t0](<t0>_t, sv<t0>_t).  */
struct fold_left_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "s0,s0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !r.require_derived_scalar_type (i, r.SAME_TYPE_CLASS)
	|| (type = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (fold_left)

/* sv<t0>_t svfoo[_t0](sv<t0>xN_t, uint64_t)

   where the final argument is an integer constant expression in
   the range [0, N - 1].  */
struct get_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,t0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    sve_type type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !(type = r.infer_tuple_type (i))
	|| !r.require_integer_immediate (i + 1))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int nvectors = c.vectors_per_tuple ();
    return c.require_immediate_range (1, 0, nvectors - 1);
  }
};
SHAPE (get)

/* <t0>xN_t svfoo[_t0](sv<t0>_t).  */
struct get_neonq_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "Q0,v0", group, MODE_none);
  }
  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary ();
  }
};
SHAPE (get_neonq)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, <t0>xN_t).  */
struct set_neonq_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,Q0", group, MODE_none);
  }
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_neon128_vector_type (i + 1)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;
    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (set_neonq)

/* sv<t0>_t svfoo[_t0](<t0>xN_t).  */
struct dup_neonq_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,Q0", group, MODE_none);
  }
  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (1, i, nargs)
	|| (type = r.infer_neon128_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;
    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (dup_neonq)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, uint64_t)
   <t0>_t svfoo[_n_t0](<t0>_t, uint64_t)

   where the t0 in the vector form is a signed or unsigned integer
   whose size is tied to the [bhwd] suffix of "svfoo".  */
struct inc_dec_def : public inc_dec_base
{
  CONSTEXPR inc_dec_def () : inc_dec_base (false) {}

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    /* These functions are unusual in that the type suffixes for
       the scalar and vector forms are not related.  The vector
       form always has exactly two potential suffixes while the
       scalar form always has four.  */
    if (group.types[2][0] == NUM_TYPE_SUFFIXES)
      build_all (b, "v0,v0,su64", group, MODE_none);
    else
      build_all (b, "s0,s0,su64", group, MODE_n);
  }
};
SHAPE (inc_dec)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, enum svpattern, uint64_t)
   <t0>_t svfoo[_n_t0](<t0>_t, enum svpattern, uint64_t)

   where the t0 in the vector form is a signed or unsigned integer
   whose size is tied to the [bhwd] suffix of "svfoo".  */
struct inc_dec_pat_def : public inc_dec_base
{
  CONSTEXPR inc_dec_pat_def () : inc_dec_base (true) {}

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    /* These functions are unusual in that the type suffixes for
       the scalar and vector forms are not related.  The vector
       form always has exactly two potential suffixes while the
       scalar form always has four.  */
    if (group.types[2][0] == NUM_TYPE_SUFFIXES)
      build_all (b, "v0,v0,epattern,su64", group, MODE_none);
    else
      build_all (b, "s0,s0,epattern,su64", group, MODE_n);
  }
};
SHAPE (inc_dec_pat)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svbool_t).  */
struct inc_dec_pred_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vp", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (i + 1, VECTOR_TYPE_svbool_t))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (inc_dec_pred)

/* <t0>_t svfoo[_n_t0]_t1(<t0>_t, svbool_t)

   where _t1 is a _b<bits> suffix that describes the svbool_t argument.  */
struct inc_dec_pred_scalar_def : public overloaded_base<2>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "s0,s0,vp", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_integer_scalar_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (i + 1, VECTOR_TYPE_svbool_t))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type, r.type_suffix_ids[1]);
  }
};
SHAPE (inc_dec_pred_scalar)

/* sv<t0>[xN]_t svfoo_t0().  */
struct inherent_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t0", group, MODE_none);
  }
};
SHAPE (inherent)

/* svbool_t svfoo[_b]().  */
struct inherent_b_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    /* The "_b" suffix is optional; the full name has it, but the short
       name doesn't.  */
    build_all (b, "v0", group, MODE_none, true);
  }

  tree
  resolve (function_resolver &) const override
  {
    /* The short forms just make "_b" implicit, so no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (inherent_b)

/* void svfoo_t0().  */
struct inherent_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_", group, MODE_none);
  }
};
SHAPE (inherent_za)

/* void svfoo_t0(uint64_t).  */
struct inherent_za_slice_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su32", group, MODE_none);
  }
};
SHAPE (inherent_za_slice)

/* void svfoo_zt(uint64_t)

   where the argument must be zero.  */
struct inherent_zt_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, 0);
  }
};
SHAPE (inherent_zt)

/* void svfoo_t0(uint64_t)

   where the argument is an integer constant that specifies an 8-bit mask.  */
struct inherent_mask_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, 255);
  }
};
SHAPE (inherent_mask_za)

/* void svfoo_t0(uint32_t, const void *)
   void svfoo_vnum_t0(uint32_t, const void *, int64_t)

   where the first argument is a variable ZA slice.  */
struct ldr_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su32,al", group, MODE_none);
    build_all (b, "_,su32,al,ss64", group, MODE_vnum);
  }
};
SHAPE (ldr_za)

/* void svfoo_zt(uint64_t, const void *)

   where the first argument must be zero.  */
struct ldr_zt_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su64,al", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, 0);
  }
};
SHAPE (ldr_zt)

/* sv<t0>[xN]_t svfoo[_t0]_g(const <t0>_t *)
   sv<t0>[xN]_t svfoo_vnum[_t0]_g(const <t0>_t *, int64_t).  */
struct load_def : public load_contiguous_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_vnum);
    build_all (b, "t0,al", group, MODE_none);
    build_all (b, "t0,al,ss64", group, MODE_vnum);
  }
};
SHAPE (load)

/* sv<t0>_t svfoo_t0(const <X>_t *)
   sv<t0>_t svfoo_vnum_t0(const <X>_t *, int64_t)

   where <X> is determined by the function base name.  */
struct load_ext_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t0,al", group, MODE_none);
    build_all (b, "t0,al,ss64", group, MODE_vnum);
  }
};
SHAPE (load_ext)

/* sv<t0>_t svfoo_[s32]index_t0(const <X>_t *, svint32_t)
   sv<t0>_t svfoo_[s64]index_t0(const <X>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]index_t0(const <X>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]index_t0(const <X>_t *, svuint64_t)

   sv<t0>_t svfoo[_u32base]_index_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_index_t0(svuint64_t, int64_t)

   where <X> is determined by the function base name.  */
struct load_ext_gather_index_def : public load_ext_gather_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index (b, "t0,al,d", group);
    build_vs_index (b, "t0,b,ss64", group);
  }
};
SHAPE (load_ext_gather_index)

/* sv<t0>_t svfoo_[s64]index_t0(const <X>_t *, svint64_t)
   sv<t0>_t svfoo_[u64]index_t0(const <X>_t *, svuint64_t)

   sv<t0>_t svfoo[_u32base]_index_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_index_t0(svuint64_t, int64_t)

   where <X> is determined by the function base name.  This is
   load_ext_gather_index that doesn't support 32-bit vector indices.  */
struct load_ext_gather_index_restricted_def : public load_ext_gather_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index64 (b, "t0,al,d", group);
    build_vs_index (b, "t0,b,ss64", group);
  }
};
SHAPE (load_ext_gather_index_restricted)

/* sv<t0>_t svfoo_[s32]offset_t0(const <X>_t *, svint32_t)
   sv<t0>_t svfoo_[s64]offset_t0(const <X>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]offset_t0(const <X>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]offset_t0(const <X>_t *, svuint64_t)

   sv<t0>_t svfoo[_u32base]_t0(svuint32_t)
   sv<t0>_t svfoo[_u64base]_t0(svuint64_t)

   sv<t0>_t svfoo[_u32base]_offset_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_offset_t0(svuint64_t, int64_t)

   where <X> is determined by the function base name.  */
struct load_ext_gather_offset_def : public load_ext_gather_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_offset (b, "t0,al,d", group);
    build_v_base (b, "t0,b", group, true);
    build_vs_offset (b, "t0,b,ss64", group);
  }
};
SHAPE (load_ext_gather_offset)

/* sv<t0>_t svfoo_[s64]offset_t0(const <X>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]offset_t0(const <X>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]offset_t0(const <X>_t *, svuint64_t)

   sv<t0>_t svfoo[_u32base]_t0(svuint32_t)
   sv<t0>_t svfoo[_u64base]_t0(svuint64_t)

   sv<t0>_t svfoo[_u32base]_offset_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_offset_t0(svuint64_t, int64_t)

   where <X> is determined by the function base name.  This is
   load_ext_gather_offset without the s32 vector offset form.  */
struct load_ext_gather_offset_restricted_def : public load_ext_gather_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_uint_offset (b, "t0,al,d", group);
    build_v_base (b, "t0,b", group, true);
    build_vs_offset (b, "t0,b,ss64", group);
  }
};
SHAPE (load_ext_gather_offset_restricted)

/* sv<t0>_t svfoo_[s32]index[_t0](const <t0>_t *, svint32_t)
   sv<t0>_t svfoo_[s64]index[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]index[_t0](const <t0>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]index[_t0](const <t0>_t *, svuint64_t)

   sv<t0>_t svfoo_[s32]offset[_t0](const <t0>_t *, svint32_t)
   sv<t0>_t svfoo_[s64]offset[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]offset[_t0](const <t0>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]offset[_t0](const <t0>_t *, svuint64_t).  */
struct load_gather_sv_def : public load_gather_sv_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_index (b, "t0,al,d", group);
    build_sv_offset (b, "t0,al,d", group);
  }
};
SHAPE (load_gather_sv)

/* sv<t0>_t svfoo_[u32]index[_t0](const <t0>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]index[_t0](const <t0>_t *, svuint64_t)

   sv<t0>_t svfoo_[s64]offset[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]offset[_t0](const <t0>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]offset[_t0](const <t0>_t *, svuint64_t)

   This is load_gather_sv without the 32-bit vector index forms and
   without the s32 vector offset form.  */
struct load_gather_sv_restricted_def : public load_gather_sv_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_index64 (b, "t0,al,d", group);
    build_sv_uint_offset (b, "t0,al,d", group);
  }
};
SHAPE (load_gather_sv_restricted)

/* sv<t0>_t svfoo[_u32base]_t0(svuint32_t)
   sv<t0>_t svfoo[_u64base]_t0(svuint64_t)

   sv<t0>_t svfoo[_u32base]_index_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_index_t0(svuint64_t, int64_t)

   sv<t0>_t svfoo[_u32base]_offset_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_offset_t0(svuint64_t, int64_t).  */
struct load_gather_vs_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    /* The base vector mode is optional; the full name has it but the
       short name doesn't.  There is no ambiguity with SHAPE_load_gather_sv
       because the latter uses an implicit type suffix.  */
    build_v_base (b, "t0,b", group, true);
    build_vs_index (b, "t0,b,ss64", group, true);
    build_vs_offset (b, "t0,b,ss64", group, true);
  }

  tree
  resolve (function_resolver &) const override
  {
    /* The short name just makes the base vector mode implicit;
       no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (load_gather_vs)

/* sv<t0>_t svfoo_[s64]index[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u64]index[_t0](const <t0>_t *, svuint64_t).  */
struct load_gather64_sv_index_def : public load_gather64_sv_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_all (b, "t0,al,d", group, MODE_s64index);
    build_all (b, "t0,al,d", group, MODE_u64index);
  }
};
SHAPE (load_gather64_sv_index)

/* sv<t0>_t svfoo_[s64]offset[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u64]offset[_t0](const <t0>_t *, svuint64_t).  */
struct load_gather64_sv_offset_def : public load_gather64_sv_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_offset);
    build_all (b, "t0,al,d", group, MODE_s64offset);
    build_all (b, "t0,al,d", group, MODE_u64offset);
  }
};
SHAPE (load_gather64_sv_offset)

/* sv<t0>_t svfoo[_u64base]_index_t0(svuint64_t, int64_t).  */
struct load_gather64_vs_index_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t0,b,ss64", group, MODE_u64base_index, true);
  }

  tree
  resolve (function_resolver &) const override
  {
    /* The short name just makes the base vector mode implicit;
       no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (load_gather64_vs_index)

/* sv<t0>_t svfoo[_u64base]_t0(svuint64_t)

   sv<t0>_t svfoo[_u64base]_offset_t0(svuint64_t, int64_t).  */
struct load_gather64_vs_offset_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t0,b", group, MODE_u64base, true);
    build_all (b, "t0,b,ss64", group, MODE_u64base_offset, true);
  }

  tree
  resolve (function_resolver &) const override
  {
    /* The short name just makes the base vector mode implicit;
       no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (load_gather64_vs_offset)

/* sv<t0>_t svfoo[_t0](const <t0>_t *)

   The only difference from "load" is that this shape has no vnum form.  */
struct load_replicate_def : public load_contiguous_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,al", group, MODE_none);
  }
};
SHAPE (load_replicate)

/* void svfoo_t0(uint64_t, uint32_t, svbool_t, const void *)
   void svfoo_vnum_t0(uint64_t, uint32_t, svbool_t, const void *, int64_t)

   where the first two fields form a (ZA tile, slice) pair.  */
struct load_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su64,su32,vp,al", group, MODE_none);
    build_all (b, "_,su64,su32,vp,al,ss64", group, MODE_vnum);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (load_za)

using luti2_lane_zt_def = luti_lane_zt_base<2>;
SHAPE (luti2_lane_zt)

using luti4_lane_zt_def = luti_lane_zt_base<4>;
SHAPE (luti4_lane_zt)

/* svbool_t svfoo(enum svpattern).  */
struct pattern_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "vp,epattern", group, MODE_none);
  }
};
SHAPE (pattern_pred)

/* svbool_t svfoo[_t0](sv<t0>_t).  */
struct pmov_from_vector_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (pmov_from_vector)

/* svbool_t svfoo[_t0](sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, sizeof (<t0>_t) - 1].  */
struct pmov_from_vector_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1, 1);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bytes = c.type_suffix (0).element_bytes;
    return c.require_immediate_range (1, 0, bytes - 1);
  }
};
SHAPE (pmov_from_vector_lane)

/* sv<t0>_t svfoo_t0(uint64_t)

   where the final argument is an integer constant expression in the
   range [1, sizeof (<t0>_t) - 1].  */
struct pmov_to_vector_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    gcc_assert (r.pred == PRED_m);
    if (!r.check_num_arguments (3)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (1, VECTOR_TYPE_svbool_t)
	|| !r.require_integer_immediate (2))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int bytes = c.type_suffix (0).element_bytes;
    /* 1 to account for the vector argument.

       ??? This should probably be folded into function_checker::m_base_arg,
       but it doesn't currently have the necessary information.  */
    return c.require_immediate_range (1, 1, bytes - 1);
  }
};
SHAPE (pmov_to_vector_lane)

/* void svfoo(const void *, svprfop)
   void svfoo_vnum(const void *, int64_t, svprfop).  */
struct prefetch_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,ap,eprfop", group, MODE_none);
    build_all (b, "_,ap,ss64,eprfop", group, MODE_vnum);
  }
};
SHAPE (prefetch)

/* void svfoo_[s32]index(const void *, svint32_t, svprfop)
   void svfoo_[s64]index(const void *, svint64_t, svprfop)
   void svfoo_[u32]index(const void *, svuint32_t, svprfop)
   void svfoo_[u64]index(const void *, svuint64_t, svprfop)

   void svfoo[_u32base](svuint32_t, svprfop)
   void svfoo[_u64base](svuint64_t, svprfop)

   void svfoo[_u32base]_index(svuint32_t, int64_t, svprfop)
   void svfoo[_u64base]_index(svuint64_t, int64_t, svprfop).  */
struct prefetch_gather_index_def : public prefetch_gather_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index (b, "_,ap,d,eprfop", group);
    build_v_base (b, "_,b,eprfop", group);
    build_vs_index (b, "_,b,ss64,eprfop", group);
  }
};
SHAPE (prefetch_gather_index)

/* void svfoo_[s32]offset(const void *, svint32_t, svprfop)
   void svfoo_[s64]offset(const void *, svint64_t, svprfop)
   void svfoo_[u32]offset(const void *, svuint32_t, svprfop)
   void svfoo_[u64]offset(const void *, svuint64_t, svprfop)

   void svfoo[_u32base](svuint32_t, svprfop)
   void svfoo[_u64base](svuint64_t, svprfop)

   void svfoo[_u32base]_offset(svuint32_t, int64_t, svprfop)
   void svfoo[_u64base]_offset(svuint64_t, int64_t, svprfop).  */
struct prefetch_gather_offset_def : public prefetch_gather_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_offset (b, "_,ap,d,eprfop", group);
    build_v_base (b, "_,b,eprfop", group);
    build_vs_offset (b, "_,b,ss64,eprfop", group);
  }
};
SHAPE (prefetch_gather_offset)

/* bool svfoo(svbool_t).  */
struct ptest_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "sp,vp", group, MODE_none);
  }
};
SHAPE (ptest)

/* svbool_t svfoo().  */
struct rdffr_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "vp", group, MODE_none);
  }
};
SHAPE (rdffr)

/* sv<t1>x<g>_t svfoo_t0_t1_g(uint64_t, uint32_t).  */
struct read_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t1,su64,su32", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (read_za)

/* sv<t1>_t svfoo_t0[_t1](uint64_t, uint32_t)

   where the first two fields form a (ZA tile, slice) pair.  */
struct read_za_m_def : public overloaded_base<1>
{
  bool
  has_merge_argument_p (const function_instance &, unsigned int) const override
  {
    return true;
  }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t1,su64,su32", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    gcc_assert (r.pred == PRED_m);
    type_suffix_index type;
    if (!r.check_num_arguments (4)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (1, VECTOR_TYPE_svbool_t)
	|| !r.require_integer_immediate (2)
	|| !r.require_scalar_type (3, "uint32_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type);
  }

  bool
  check (function_checker &c) const override
  {
    gcc_assert (c.pred == PRED_m);
    return c.require_immediate_range (1, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (read_za_m)

/* sv<t1>x<g>_t svfoo_t0_t1_g(uint32_t).  */
struct read_za_slice_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "t1,su32", group, MODE_none);
  }
};
SHAPE (read_za_slice)

/* <t0>_t svfoo[_t0](sv<t0>_t).  */
struct reduction_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "s0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (reduction)

/* <t0>xN_t svfoo[_t0](sv<t0>_t).  */
struct reduction_neonq_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "Q0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (reduction_neonq)

/* int64_t svfoo[_t0](sv<t0>_t)  (for signed t0)
   uint64_t svfoo[_t0](sv<t0>_t)  (for unsigned t0)
   <t0>_t svfoo[_t0](sv<t0>_t)  (for floating-point t0)

   i.e. a version of "reduction" in which the return type for integers
   always has 64 bits.  */
struct reduction_wide_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "sw0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (reduction_wide)

/* sv<t0>x<g>_t svfoo_t0[_t1_g](sv<t1>x<g>_t)

   where the target type <t0> must be specified explicitly but the source
   type <t1> can be inferred.  */
struct reinterpret_def : public overloaded_base<1>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (1)
	|| !(type = r.infer_sve_type (0)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (reinterpret)

/* sv<t0>_t svfoo_t0(sv<t0>_t, sv<t0>_t, uint32_t).  */
struct select_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "v0,v0,vp,su32", group, MODE_none);
  }
};
SHAPE (select_pred)

/* sv<t0>xN_t svfoo[_t0](sv<t0>xN_t, uint64_t, sv<t0>_t)

   where the second argument is an integer constant expression in the
   range [0, N - 1].  */
struct set_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,su64,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    sve_type type;
    if (!r.check_gp_argument (3, i, nargs)
	|| !(type = r.infer_tuple_type (i))
	|| !r.require_integer_immediate (i + 1)
	|| !r.require_derived_vector_type (i + 2, i, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int nvectors = c.vectors_per_tuple ();
    return c.require_immediate_range (1, 0, nvectors - 1);
  }
};
SHAPE (set)

/* void svfoo().  */
struct setffr_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_", group, MODE_none);
  }
};
SHAPE (setffr)

/* sv<t0>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [0, sizeof (<t0>_t) * 8 - 1].  */
struct shift_left_imm_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,v0,su64", group, MODE_n);
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
    return c.require_immediate_range (1, 0, bits - 1);
  }
};
SHAPE (shift_left_imm)

/* sv<t0>_t svfoo[_n_t0])(sv<t0:half>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [0, sizeof (<t0>_t) * 4 - 1].  */
struct shift_left_imm_long_def : public binary_imm_long_base
{
  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits / 2;
    return c.require_immediate_range (1, 0, bits - 1);
  }
};
SHAPE (shift_left_imm_long)

/* sv<t0:uint>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [0, sizeof (<t0>_t) * 8 - 1].  */
struct shift_left_imm_to_uint_def : public shift_left_imm_def
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "vu0,v0,su64", group, MODE_n);
  }
};
SHAPE (shift_left_imm_to_uint)

/* sv<t0>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 8].  */
struct shift_right_imm_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,v0,su64", group, MODE_n);
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
SHAPE (shift_right_imm)

/* sv<t0:half>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 4].  */
typedef shift_right_imm_narrow_wrapper<binary_imm_narrowb_base<>, 1>
  shift_right_imm_narrowb_def;
SHAPE (shift_right_imm_narrowb)

/* sv<t0:half>_t svfoo[_n_t0])(sv<t0:half>_t, sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 4].  */
typedef shift_right_imm_narrow_wrapper<binary_imm_narrowt_base<>, 2>
  shift_right_imm_narrowt_def;
SHAPE (shift_right_imm_narrowt)

/* sv<t0:uint:half>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 4].  */
typedef binary_imm_narrowb_base<TYPE_unsigned>
  binary_imm_narrowb_base_unsigned;
typedef shift_right_imm_narrow_wrapper<binary_imm_narrowb_base_unsigned, 1>
  shift_right_imm_narrowb_to_uint_def;
SHAPE (shift_right_imm_narrowb_to_uint)

/* sv<t0:uint:half>_t svfoo[_n_t0])(sv<t0:uint:half>_t, sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 4].  */
typedef binary_imm_narrowt_base<TYPE_unsigned>
  binary_imm_narrowt_base_unsigned;
typedef shift_right_imm_narrow_wrapper<binary_imm_narrowt_base_unsigned, 2>
  shift_right_imm_narrowt_to_uint_def;
SHAPE (shift_right_imm_narrowt_to_uint)

/* sv<t0>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 8].  */
struct shift_right_imm_narrowxn_def : public overloaded_base<1>
{
  bool explicit_group_suffix_p () const override { return false; }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "c0,c1,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (2)
	|| !(type = r.infer_sve_type (0))
	|| !r.require_integer_immediate (1))
      return error_mark_node;
    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    unsigned int suffix = c.group_suffix_id == GROUP_x4 ? 1 : 0;
    unsigned int bits = c.type_suffix (suffix).element_bits;
    return c.require_immediate_range (1, 1, bits);
  }
};
SHAPE (shift_right_imm_narrowxn)

/* void svfoo[_t0](<X>_t *, sv<t0>[xN]_t)
   void svfoo_vnum[_t0](<X>_t *, int64_t, sv<t0>[xN]_t)

   where <X> might be tied to <t0> (for non-truncating stores) or might
   depend on the function base name (for truncating stores).  */
struct store_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_vnum);
    build_all (b, "_,as,t0", group, MODE_none);
    build_all (b, "_,as,ss64,t0", group, MODE_vnum);
  }

  tree
  resolve (function_resolver &r) const override
  {
    bool vnum_p = r.mode_suffix_id == MODE_vnum;
    gcc_assert (r.mode_suffix_id == MODE_none || vnum_p);

    unsigned int i, nargs;
    sve_type type;
    if (!r.check_gp_argument (vnum_p ? 3 : 2, i, nargs)
	|| !r.require_pointer_type (i)
	|| (vnum_p && !r.require_scalar_type (i + 1, "int64_t"))
	|| !(type = r.infer_tuple_type (nargs - 1)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (store)

/* void svfoo_[s32]index[_t0](<X>_t *, svint32_t, sv<t0>_t)
   void svfoo_[s64]index[_t0](<X>_t *, svint64_t, sv<t0>_t)
   void svfoo_[u32]index[_t0](<X>_t *, svuint32_t, sv<t0>_t)
   void svfoo_[u64]index[_t0](<X>_t *, svuint64_t, sv<t0>_t)

   void svfoo[_u32base]_index[_t0](svuint32_t, int64_t, sv<t0>_t)
   void svfoo[_u64base]_index[_t0](svuint64_t, int64_t, sv<t0>_t)

   where <X> might be tied to <t0> (for non-truncating stores) or might
   depend on the function base name (for truncating stores).  */
struct store_scatter_index_def : public store_scatter_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index (b, "_,as,d,t0", group);
    build_vs_index (b, "_,b,ss64,t0", group);
  }
};
SHAPE (store_scatter_index)

/* void svfoo_[s64]index[_t0](<X>_t *, svint64_t, sv<t0>_t)
   void svfoo_[u64]index[_t0](<X>_t *, svuint64_t, sv<t0>_t)

   void svfoo[_u32base]_index[_t0](svuint32_t, int64_t, sv<t0>_t)
   void svfoo[_u64base]_index[_t0](svuint64_t, int64_t, sv<t0>_t)

   i.e. a version of store_scatter_index that doesn't support 32-bit
   vector indices.  */
struct store_scatter_index_restricted_def : public store_scatter_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index64 (b, "_,as,d,t0", group);
    build_vs_index (b, "_,b,ss64,t0", group);
  }
};
SHAPE (store_scatter_index_restricted)

/* void svfoo_[s32]offset[_t0](<X>_t *, svint32_t, sv<t0>_t)
   void svfoo_[s64]offset[_t0](<X>_t *, svint64_t, sv<t0>_t)
   void svfoo_[u32]offset[_t0](<X>_t *, svuint32_t, sv<t0>_t)
   void svfoo_[u64]offset[_t0](<X>_t *, svuint64_t, sv<t0>_t)

   void svfoo[_u32base_t0](svuint32_t, sv<t0>_t)
   void svfoo[_u64base_t0](svuint64_t, sv<t0>_t)

   void svfoo[_u32base]_offset[_t0](svuint32_t, int64_t, sv<t0>_t)
   void svfoo[_u64base]_offset[_t0](svuint64_t, int64_t, sv<t0>_t)

   where <X> might be tied to <t0> (for non-truncating stores) or might
   depend on the function base name (for truncating stores).  */
struct store_scatter_offset_def : public store_scatter_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_offset (b, "_,as,d,t0", group);
    build_v_base (b, "_,b,t0", group);
    build_vs_offset (b, "_,b,ss64,t0", group);
  }
};
SHAPE (store_scatter_offset)

/* void svfoo_[s64]offset[_t0](<X>_t *, svint64_t, sv<t0>_t)
   void svfoo_[u32]offset[_t0](<X>_t *, svuint32_t, sv<t0>_t)
   void svfoo_[u64]offset[_t0](<X>_t *, svuint64_t, sv<t0>_t)

   void svfoo[_u32base_t0](svuint32_t, sv<t0>_t)
   void svfoo[_u64base_t0](svuint64_t, sv<t0>_t)

   void svfoo[_u32base]_offset[_t0](svuint32_t, int64_t, sv<t0>_t)
   void svfoo[_u64base]_offset[_t0](svuint64_t, int64_t, sv<t0>_t)

   i.e. a version of store_scatter_offset that doesn't support svint32_t
   offsets.  */
struct store_scatter_offset_restricted_def : public store_scatter_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_uint_offset (b, "_,as,d,t0", group);
    build_v_base (b, "_,b,t0", group);
    build_vs_offset (b, "_,b,ss64,t0", group);
  }
};
SHAPE (store_scatter_offset_restricted)

/* void svfoo_[s64]index[_t0](<t0>_t *, svint64_t, sv<t0>_t)
   void svfoo_[u64]index[_t0](<t0>_t *, svuint64_t, sv<t0>_t)

   void svfoo[_u64base]_index[_t0](svuint64_t, int64_t, sv<t0>_t).  */
struct store_scatter64_index_def : public store_scatter64_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_index);
    build_all (b, "_,as,d,t0", group, MODE_s64index);
    build_all (b, "_,as,d,t0", group, MODE_u64index);
    build_all (b, "_,b,ss64,t0", group, MODE_u64base_index);
  }
};
SHAPE (store_scatter64_index)

/* void svfoo_[s64]offset[_t0](<t0>_t *, svint64_t, sv<t0>_t)
   void svfoo_[u64]offset[_t0](<t0>_t *, svuint64_t, sv<t0>_t)

   void svfoo[_u64base_t0](svuint64_t, sv<t0>_t)

   void svfoo[_u64base]_offset[_t0](svuint64_t, int64_t, sv<t0>_t).  */
struct store_scatter64_offset_def : public store_scatter64_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_offset);
    build_all (b, "_,as,d,t0", group, MODE_s64offset);
    build_all (b, "_,as,d,t0", group, MODE_u64offset);
    build_all (b, "_,b,t0", group, MODE_u64base);
    build_all (b, "_,b,ss64,t0", group, MODE_u64base_offset);
  }
};
SHAPE (store_scatter64_offset)

/* void svfoo_t0(uint64_t, uint32_t, svbool_t, void *)
   void svfoo_vnum_t0(uint64_t, uint32_t, svbool_t, void *, int64_t)

   where the first two fields form a (ZA tile, slice) pair.  */
struct store_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su64,su32,vp,as", group, MODE_none);
    build_all (b, "_,su64,su32,vp,as,ss64", group, MODE_vnum);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (store_za)

/* void svfoo[_t0_g](<X>_t *, sv<t0>x<g>_t)
   void svfoo_vnum[_t0_g](<X>_t *, int64_t, sv<t0>x<g>_t)

   where <X> might be tied to <t0> (for non-truncating stores) or might
   depend on the function base name (for truncating stores).  */
struct storexn_def : public store_def
{
  bool explicit_group_suffix_p () const override { return false; }

  tree
  resolve (function_resolver &r) const override
  {
    bool vnum_p = r.mode_suffix_id == MODE_vnum;
    gcc_assert (r.mode_suffix_id == MODE_none || vnum_p);

    unsigned int nargs = vnum_p ? 4 : 3;
    vector_type_index pred_type;
    sve_type type;
    if (!r.check_num_arguments (nargs)
	|| (pred_type = r.infer_predicate_type (0)) == NUM_VECTOR_TYPES
	|| !r.require_pointer_type (1)
	|| (vnum_p && !r.require_scalar_type (2, "int64_t"))
	|| !(type = r.infer_sve_type (nargs - 1))
	|| !r.require_matching_predicate_type (pred_type, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (storexn)

/* void svfoo_t0(uint32_t, void *)
   void svfoo_vnum_t0(uint32_t, void *, int64_t)

   where the first argument is a variable ZA slice.  */
struct str_za_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su32,as", group, MODE_none);
    build_all (b, "_,su32,as,ss64", group, MODE_vnum);
  }
};
SHAPE (str_za)

/* void svfoo_zt(uint64_t, void *)

   where the first argument must be zero.  */
struct str_zt_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "_,su64,as", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, 0);
  }
};
SHAPE (str_zt)

/* sv<t0>_t svfoo[_t0](sv<t0>xN_t, sv<t0:uint>_t).  */
struct tbl_tuple_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,t0,vu0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    sve_type type;
    if (!r.check_gp_argument (2, i, nargs)
	|| !(type = r.infer_tuple_type (i))
	|| !r.require_derived_vector_type (i + 1, i, type, TYPE_unsigned))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (tbl_tuple)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svbfloatt16_t, svbfloat16_t).  */
struct ternary_bfloat_def
  : public ternary_resize2_base<16, TYPE_bfloat, TYPE_bfloat>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vB,vB", group, MODE_none);
  }
};
SHAPE (ternary_bfloat)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svmfloat8_t, svmfloat8_t).  */
struct ternary_mfloat8_def
    : public ternary_resize2_base<8, TYPE_mfloat, TYPE_mfloat>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    gcc_assert (group.fpm_mode == FPM_set);
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vM,vM", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    if (!r.check_num_arguments (4)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (1, VECTOR_TYPE_svmfloat8_t)
	|| !r.require_vector_type (2, VECTOR_TYPE_svmfloat8_t)
	|| !r.require_scalar_type (3, "uint64_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type, TYPE_SUFFIX_mf8, GROUP_none);
  }
};
SHAPE (ternary_mfloat8)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svbfloat16_t, svbfloat16_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 7].  */
typedef ternary_bfloat_lane_base<1> ternary_bfloat_lane_def;
SHAPE (ternary_bfloat_lane)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svbfloat16_t, svbfloat16_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 3].  */
typedef ternary_bfloat_lane_base<2> ternary_bfloat_lanex2_def;
SHAPE (ternary_bfloat_lanex2)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svmfloat8_t, svmfloat8_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 15].  */
struct ternary_mfloat8_lane_def
    : public ternary_resize2_lane_base<8, TYPE_mfloat, TYPE_mfloat>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    gcc_assert (group.fpm_mode == FPM_set);
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vM,vM,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (3, 2, 1);
  }

  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    if (!r.check_num_arguments (5)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (1, VECTOR_TYPE_svmfloat8_t)
	|| !r.require_vector_type (2, VECTOR_TYPE_svmfloat8_t)
	|| !r.require_integer_immediate (3)
	|| !r.require_scalar_type (4, "uint64_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type, TYPE_SUFFIX_mf8, GROUP_none);
  }
};
SHAPE (ternary_mfloat8_lane)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svmfloat8_t, svmfloat8_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 7] or [0, 3].  */
struct ternary_mfloat8_lane_group_selection_def
    : public ternary_mfloat8_lane_def
{
  bool
  check (function_checker &c) const override
  {
    machine_mode mode = c.vector_mode (0);
    if (mode == E_VNx8HFmode)
      return c.require_immediate_lane_index (3, 2, 2);
    else if (mode == E_VNx4SFmode)
      return c.require_immediate_lane_index (3, 2, 4);
    gcc_unreachable ();
  }
};
SHAPE (ternary_mfloat8_lane_group_selection)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svbfloatt16_t, svbfloat16_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, svbfloat16_t, bfloat16_t).  */
struct ternary_bfloat_opt_n_def
  : public ternary_resize2_opt_n_base<16, TYPE_bfloat, TYPE_bfloat>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vB,vB", group, MODE_none);
    build_all (b, "v0,v0,vB,sB", group, MODE_n);
  }
};
SHAPE (ternary_bfloat_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svmfloatt8_t, svmfloat8_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, svmfloat8_t, bfloat8_t).  */
struct ternary_mfloat8_opt_n_def
    : public ternary_resize2_opt_n_base<8, TYPE_mfloat, TYPE_mfloat>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    gcc_assert (group.fpm_mode == FPM_set);
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vM,vM", group, MODE_none);
    build_all (b, "v0,v0,vM,sM", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    if (!r.check_num_arguments (4)
	|| (type = r.infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_type (1, VECTOR_TYPE_svmfloat8_t)
	|| !r.require_vector_or_scalar_type (2)
	|| !r.require_scalar_type (3, "uint64_t"))
      return error_mark_node;

    auto mode = r.mode_suffix_id;
    if (r.scalar_argument_p (2))
      mode = MODE_n;
    else if (!r.require_vector_type (2, VECTOR_TYPE_svmfloat8_t))
      return error_mark_node;

    return r.resolve_to (mode, type, TYPE_SUFFIX_mf8, GROUP_none);
  }
};
SHAPE (ternary_mfloat8_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:int:quarter>_t, sv<t0:uint:quarter>_t,
		       uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 16 / sizeof (<t0>_t) - 1].  */
struct ternary_intq_uintq_lane_def
  : public ternary_qq_lane_base<TYPE_signed, TYPE_unsigned>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vqs0,vqu0,su64", group, MODE_none);
  }
};
SHAPE (ternary_intq_uintq_lane)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:int:quarter>_t, sv<t0:uint:quarter>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0:int:quarter>_t,
			 <t0:uint:quarter>_t).  */
struct ternary_intq_uintq_opt_n_def
  : public ternary_resize2_opt_n_base<function_resolver::QUARTER_SIZE,
				      TYPE_signed, TYPE_unsigned>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vqs0,vqu0", group, MODE_none);
    build_all (b, "v0,v0,vqs0,squ0", group, MODE_n);
  }
};
SHAPE (ternary_intq_uintq_opt_n)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 16 / sizeof (<t0>_t) - 1].  */
struct ternary_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (3, 1);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (3, 2);
  }
};
SHAPE (ternary_lane)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, sv<t0>_t, uint64_t, uint64_t)

   where the penultimate argument is an integer constant expression in
   the range [0, 8 / sizeof (<t0>_t) - 1] and where the final argument
   is an integer constant expression in {0, 90, 180, 270}.  */
struct ternary_lane_rotate_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0,su64,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (3, 2);
  }

  bool
  check (function_checker &c) const override
  {
    return (c.require_immediate_lane_index (3, 2, 2)
	    && c.require_immediate_one_of (4, 0, 90, 180, 270));
  }
};
SHAPE (ternary_lane_rotate)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:half>_t, sv<t0:half>_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 32 / sizeof (<t0>_t) - 1].  */
struct ternary_long_lane_def
  : public ternary_resize2_lane_base<function_resolver::HALF_SIZE>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vh0,vh0,su64", group, MODE_none);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (3, 2);
  }
};
SHAPE (ternary_long_lane)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:half>_t, sv<t0:half>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0:half>_t, <t0:half>_t)

   i.e. a version of the standard ternary shape ternary_opt_n in which
   the element type of the last two arguments is the half-sized
   equivalent of <t0>.  */
struct ternary_long_opt_n_def
  : public ternary_resize2_opt_n_base<function_resolver::HALF_SIZE>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vh0,vh0", group, MODE_none);
    build_all (b, "v0,v0,vh0,sh0", group, MODE_n);
  }
};
SHAPE (ternary_long_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, sv<t0>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0>_t, <t0>_t)

   i.e. the standard shape for ternary operations that operate on
   uniform types.  */
struct ternary_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0", group, MODE_none);
    build_all (b, "v0,v0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform_opt_n (3);
  }
};
SHAPE (ternary_opt_n)

/* A choice between:

   (1) sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:quarter>_t, sv<t0:quarter>_t,
			   uint64_t)

   (2) sv<t0>_t svfoo[_t0_t1](sv<t0>_t, sv<t1>_t, sv<t1>_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 16 / sizeof (<t0>_t) - 1].  */
struct ternary_qq_or_011_lane_def : public ternary_qq_lane_base<>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    if (group.types[0][1] == NUM_TYPE_SUFFIXES)
      build_all (b, "v0,v0,vq0,vq0,su64", group, MODE_none);
    else
      build_all (b, "v0,v0,v1,v1,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type0, type1;
    if (!r.check_gp_argument (4, i, nargs)
	|| (type0 = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| (type1 = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES
	|| !r.require_matching_vector_type (i + 2, i + 1, type1)
	|| !r.require_integer_immediate (i + 3))
      return error_mark_node;

    if ((type_suffixes[type0].element_bits
	 == 4 * type_suffixes[type1].element_bits)
	&& type_suffixes[type0].tclass == type_suffixes[type1].tclass)
      if (tree res = r.lookup_form (MODE_none, type0))
	return res;

    return r.resolve_to (r.mode_suffix_id, type0, type1);
  }
};
SHAPE (ternary_qq_or_011_lane)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0:quarter>_t, sv<t0:quarter>_t,
			 uint64_t)

   where the final argument is an integer constant expression in
   {0, 90, 180, 270}.  */
struct ternary_qq_lane_rotate_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vq0,vq0,su64,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (5, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, r.SAME_TYPE_CLASS,
					   r.QUARTER_SIZE)
	|| !r.require_derived_vector_type (i + 2, i, type, r.SAME_TYPE_CLASS,
					   r.QUARTER_SIZE)
	|| !r.require_integer_immediate (i + 3)
	|| !r.require_integer_immediate (i + 4))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    return (c.require_immediate_lane_index (3, 0)
	    && c.require_immediate_one_of (4, 0, 90, 180, 270));
  }
};
SHAPE (ternary_qq_lane_rotate)

/* A choice between:

   (1) sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:quarter>_t, sv<t0:quarter>_t)
       sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0:quarter>_t, <t0:quarter>_t)

       i.e. a version of the standard ternary shape ternary_opt_n in which
       the element type of the last two arguments is the quarter-sized
       equivalent of <t0>.

   (2) sv<t0>_t svfoo[_t0_t1](sv<t0>_t, sv<t1>_t, sv<t1>_t)

       where the element type of the last two arguments is specified
       explicitly.  */
struct ternary_qq_opt_n_or_011_def
  : public ternary_resize2_opt_n_base<function_resolver::QUARTER_SIZE>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    if (group.types[0][1] == NUM_TYPE_SUFFIXES)
      {
	build_all (b, "v0,v0,vq0,vq0", group, MODE_none);
	build_all (b, "v0,v0,vq0,sq0", group, MODE_n);
      }
    else
      build_all (b, "v0,v0,v1,v1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type0, type1;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type0 = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| (type1 = r.infer_vector_type (i + 1)) == NUM_TYPE_SUFFIXES
	|| !r.require_vector_or_scalar_type (i + 2))
      return error_mark_node;

    auto mode = r.scalar_argument_p (i + 2) ? MODE_n : MODE_none;
    if (mode == MODE_none
	&& !r.require_matching_vector_type (i + 2, i + 1, type1))
      return error_mark_node;

    if ((type_suffixes[type0].element_bits
	 == 4 * type_suffixes[type1].element_bits)
	&& type_suffixes[type0].tclass == type_suffixes[type1].tclass)
      if (tree res = r.lookup_form (mode, type0))
	return res;

    if (!r.require_nonscalar_type (i + 2))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type0, type1);
  }
};
SHAPE (ternary_qq_opt_n_or_011)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0:quarter>_t, sv<t0:quarter>_t,
			 uint64_t)

   where the final argument is an integer constant expression in
   {0, 90, 180, 270}.  */
struct ternary_qq_rotate_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vq0,vq0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (4, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, r.SAME_TYPE_CLASS,
					   r.QUARTER_SIZE)
	|| !r.require_derived_vector_type (i + 2, i, type, r.SAME_TYPE_CLASS,
					   r.QUARTER_SIZE)
	|| !r.require_integer_immediate (i + 3))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_one_of (3, 0, 90, 180, 270);
  }
};
SHAPE (ternary_qq_rotate)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in
   {0, 90, 180, 270}.  */
struct ternary_rotate_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (3, 1);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_one_of (3, 0, 90, 180, 270);
  }
};
SHAPE (ternary_rotate)

/* sv<t0>_t svfoo[_n_t0])(sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [0, sizeof (<t0>_t) * 8 - 1].  */
struct ternary_shift_left_imm_def : public ternary_shift_imm_base
{
  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (2, 0, bits - 1);
  }
};
SHAPE (ternary_shift_left_imm)

/* sv<t0>_t svfoo[_n_t0])(sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 8].  */
struct ternary_shift_right_imm_def : public ternary_shift_imm_base
{
  bool
  check (function_checker &c) const override
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (2, 1, bits);
  }
};
SHAPE (ternary_shift_right_imm)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, sv<t0:uint>_t).  */
struct ternary_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,vu0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_matching_vector_type (i + 1, i, type)
	|| !r.require_derived_vector_type (i + 2, i, type, TYPE_unsigned))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (ternary_uint)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, svu<t0:uint:quarter>_t,
		       sv<t0:int:quarter>_t).  */
struct ternary_uintq_intq_def
  : public ternary_resize2_base<function_resolver::QUARTER_SIZE,
				TYPE_unsigned, TYPE_signed>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vqu0,vqs0", group, MODE_none);
  }
};
SHAPE (ternary_uintq_intq)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:uint:quarter>_t, sv<t0:int:quarter>_t,
		       uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 16 / sizeof (<t0>_t) - 1].  */
struct ternary_uintq_intq_lane_def
  : public ternary_qq_lane_base<TYPE_unsigned, TYPE_signed>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vqu0,vqs0,su64", group, MODE_none);
  }
};
SHAPE (ternary_uintq_intq_lane)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:uint:quarter>_t, sv<t0:int:quarter>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0:uint:quarter>_t,
			 <t0:int:quarter>_t).  */
struct ternary_uintq_intq_opt_n_def
  : public ternary_resize2_opt_n_base<function_resolver::QUARTER_SIZE,
				      TYPE_unsigned, TYPE_signed>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vqu0,vqs0", group, MODE_none);
    build_all (b, "v0,v0,vqu0,sqs0", group, MODE_n);
  }
};
SHAPE (ternary_uintq_intq_opt_n)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 7].  */
struct tmad_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (2, 0, 7);
  }
};
SHAPE (tmad)

/* sv<t0>_t svfoo[_t0](sv<t0>_t)

   i.e. the standard shape for unary operations that operate on
   uniform types.  */
struct unary_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary ();
  }
};
SHAPE (unary)

/* sv<t0>_t svfoo_t0[_t1](sv<t1>_t)

   where the target type <t0> must be specified explicitly but the source
   type <t1> can be inferred.  */
struct unary_convert_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "c0,c1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary (r.type_suffix (0).tclass,
			    r.type_suffix (0).element_bits);
  }
};
SHAPE (unary_convert)

/* sv<t0>_t svfoo_t0[_t1](sv<t0>_t, sv<t1>_t)

   This is a version of unary_convert in which the even-indexed
   elements are passed in as a first parameter, before any governing
   predicate.  */
struct unary_convert_narrowt_def : public overloaded_base<1>
{
  bool
  has_merge_argument_p (const function_instance &, unsigned int) const override
  {
    return true;
  }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary (r.type_suffix (0).tclass,
			    r.type_suffix (0).element_bits, true);
  }
};
SHAPE (unary_convert_narrowt)

/* sv<t0>_t svfoo_t0[_t1_g](sv<t0>_t, sv<t1>x<g_t, fpm_t)

   Similar to unary_convert_narrowt but for tuple arguments with support for
   modal floating point.  */
struct unary_convertxn_narrowt_def : public overloaded_base<1>
{
  bool
  explicit_group_suffix_p () const override
  {
    return false;
  }

  bool
  has_merge_argument_p (const function_instance &, unsigned int) const override
  {
    return true;
  }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    gcc_assert(r.fpm_mode == FPM_set);
    sve_type type;
    if (!r.check_num_arguments (3)
        || !(type = r.infer_sve_type (1))
	|| !r.require_scalar_type (2, "uint64_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (unary_convertxn_narrowt)

/* sv<t0>x<g0>_t svfoo_t0[_t1_g](sv<t1>x<g1>_t)

   where the target type <t0> must be specified explicitly but the
   source type <t1> can be inferred.

   Functions with a group suffix are unpredicated.  For them:

   - If <t0> is N times wider than <t1>, the return value has N times
     more vectors than the argument.

   - If <t1> is N times wider than <t0>, the argument has N times
     more vectors than the return type.  */
struct unary_convertxn_def : public unary_convert_def
{
  bool explicit_group_suffix_p () const override { return false; }

  tree
  resolve (function_resolver &r) const override
  {
    if (r.pred != PRED_none)
      return unary_convert_def::resolve (r);

    sve_type type;
    if (!r.check_num_arguments (1)
	|| !(type = r.infer_sve_type (0)))
      return error_mark_node;

    return r.resolve_conversion (r.mode_suffix_id, type);
  }
};
SHAPE (unary_convertxn)

/* sv<t0>_t svfoo_t0[_t1_g](sv<t1>x<g1>_t)

   where the target type <t0> must be specified explicitly but the
   source type <t1> can be inferred.

   Functions with a group suffix are unpredicated. */
struct unary_convertxn_narrow_def : public unary_convert_def
{
  bool
  explicit_group_suffix_p () const override
  {
    return false;
  }

  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    gcc_assert(r.fpm_mode == FPM_set);
    sve_type type;
    if (!r.check_num_arguments (2)
        || !(type = r.infer_sve_type (0))
	|| !r.require_scalar_type (1, "uint64_t"))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (unary_convertxn_narrow)

/* sv<t0>_t svfoo_<t0>(sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 16 / sizeof (<t0>_t) - 1].  */
struct unary_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_uniform (1, 1);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_lane_index (1, 0);
  }
};
SHAPE (unary_lane)

/* sv<t0>_t svfoo[_t0](sv<t0:half>_t).  */
struct unary_long_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vh0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type, result_type;
    if (!r.check_gp_argument (1, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| (result_type = long_type_suffix (r, type)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    if (tree res = r.lookup_form (r.mode_suffix_id, result_type))
      return res;

    return r.report_no_such_form (type);
  }
};
SHAPE (unary_long)

/* sv<t0>_t svfoo[_n]_t0(<t0>_t).  */
struct unary_n_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    /* The "_n" suffix is optional; the full name has it, but the short
       name doesn't.  */
    build_all (b, "v0,s0", group, MODE_n, true);
  }

  tree
  resolve (function_resolver &) const override
  {
    /* The short forms just make "_n" implicit, so no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (unary_n)

/* sv<t0:half>_t svfoo[_t0](sv<t0>_t).  */
typedef unary_narrowb_base<> unary_narrowb_def;
SHAPE (unary_narrowb)

/* sv<t0:half>_t svfoo[_t0](sv<t0:half>_t, sv<t0>_t).  */
typedef unary_narrowt_base<> unary_narrowt_def;
SHAPE (unary_narrowt)

/* sv<t0:uint:half>_t svfoo[_t0](sv<t0>_t).  */
typedef unary_narrowb_base<TYPE_unsigned> unary_narrowb_to_uint_def;
SHAPE (unary_narrowb_to_uint)

/* sv<t0:uint:half>_t svfoo[_t0](sv<t0:uint:half>_t, sv<t0>_t).  */
typedef unary_narrowt_base<TYPE_unsigned> unary_narrowt_to_uint_def;
SHAPE (unary_narrowt_to_uint)

/* svbool_t svfoo(svbool_t).  */
struct unary_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    build_all (b, "v0,v0", group, MODE_none);
  }
};
SHAPE (unary_pred)

/* sv<t0:int>_t svfoo[_t0](sv<t0>_t)

   i.e. a version of "unary" in which the returned vector contains
   signed integers.  */
struct unary_to_int_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vs0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary (TYPE_signed);
  }
};
SHAPE (unary_to_int)

/* sv<t0:uint>_t svfoo[_t0](sv<t0>_t)

   i.e. a version of "unary" in which the returned vector contains
   unsigned integers.  */
struct unary_to_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vu0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    return r.resolve_unary (TYPE_unsigned);
  }
};
SHAPE (unary_to_uint)

/* sv<t0>_t svfoo[_t0](sv<t0:uint>_t)

   where <t0> always belongs a certain type class, and where <t0:uint>
   therefore uniquely determines <t0>.  */
struct unary_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vu0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (1, i, nargs)
	|| (type = r.infer_unsigned_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    /* Search for a valid suffix with the same number of bits as TYPE.  */
    unsigned int element_bits = type_suffixes[type].element_bits;
    if (type_suffixes[type].unsigned_p)
      for (unsigned int j = 0; j < NUM_TYPE_SUFFIXES; ++j)
	if (type_suffixes[j].element_bits == element_bits)
	  if (tree res = r.lookup_form (r.mode_suffix_id,
					type_suffix_index (j)))
	    return res;

    return r.report_no_such_form (type);
  }
};
SHAPE (unary_uint)

/* sv<t0>_t svfoo[_<t0>](sv<t0:half>_t)

   i.e. a version of "unary" in which the source elements are half the
   size of the destination elements, but have the same type class.  */
struct unary_widen_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vh0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (1, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    /* There is only a single form for predicates.  */
    if (type == TYPE_SUFFIX_b)
      return r.resolve_to (r.mode_suffix_id, type);

    if (type_suffixes[type].integer_p
	&& type_suffixes[type].element_bits < 64)
      {
	type_suffix_index wide_suffix
	  = find_type_suffix (type_suffixes[type].tclass,
			      type_suffixes[type].element_bits * 2);
	if (tree res = r.lookup_form (r.mode_suffix_id, wide_suffix))
	  return res;
      }

    return r.report_no_such_form (type);
  }
};
SHAPE (unary_widen)

/* void svfoo_t0[_t1](uint64_t, svbool_t, svbool_t, sv<t1>_t)

   where the first argument is a ZA tile.  */
struct unary_za_m_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su64,vp,vp,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    if (!r.check_num_arguments (4)
	|| !r.require_integer_immediate (0)
	|| !r.require_vector_type (1, VECTOR_TYPE_svbool_t)
	|| !r.require_vector_type (2, VECTOR_TYPE_svbool_t)
	|| (type = r.infer_vector_type (3)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (unary_za_m)

/* void svfoo_t0[_t1]_g(uint32_t, sv<t1>x<g>_t).  */
struct unary_za_slice_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    if (!za_group_is_pure_overload (group))
      build_all (b, "_,su32,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (2)
	|| !r.require_scalar_type (0, "uint32_t")
	|| !(type = r.infer_tuple_type (1)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (unary_za_slice)

/* sv<t0>x<g>_t svfoo[_t0_g](sv<t0>x<g>_t).  */
struct unaryxn_def : public unary_def
{
  bool explicit_group_suffix_p () const override { return false; }

  tree
  resolve (function_resolver &r) const override
  {
    if (r.pred != PRED_none)
      return unary_def::resolve (r);

    sve_type type;
    if (!r.check_num_arguments (1)
	|| !(type = r.infer_sve_type (0)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (unaryxn)

/* void svfoo_t0[_t1_g](uint64_t, uint32_t, sv<t1>x<g>_t).  */
struct write_za_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su64,su32,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (3)
	|| !r.require_integer_immediate (0)
	|| !r.require_scalar_type (1, "uint32_t")
	|| !(type = r.infer_tuple_type (2)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (write_za)

/* void svfoo_t0[_t1](uint64_t, uint32_t, svbool_t, sv<t1>_t)

   where the first two fields form a (ZA tile, slice) pair.  */
struct write_za_m_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su64,su32,vp,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    type_suffix_index type;
    if (!r.check_num_arguments (4)
	|| !r.require_integer_immediate (0)
	|| !r.require_scalar_type (1, "uint32_t")
	|| !r.require_vector_type (2, VECTOR_TYPE_svbool_t)
	|| (type = r.infer_vector_type (3)) == NUM_TYPE_SUFFIXES)
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type);
  }

  bool
  check (function_checker &c) const override
  {
    return c.require_immediate_range (0, 0, c.num_za_tiles () - 1);
  }
};
SHAPE (write_za_m)

/* void svfoo_t0[_t1_g](uint32_t, sv<t1>x<g>_t).  */
struct write_za_slice_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const override
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "_,su32,t1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const override
  {
    sve_type type;
    if (!r.check_num_arguments (2)
	|| !r.require_scalar_type (0, "uint32_t")
	|| !(type = r.infer_tuple_type (1)))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }
};
SHAPE (write_za_slice)

}
