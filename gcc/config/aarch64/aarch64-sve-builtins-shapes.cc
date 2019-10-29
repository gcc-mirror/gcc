/* ACLE support for AArch64 SVE (function shapes)
   Copyright (C) 2018-2019 Free Software Foundation, Inc.

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

/* If INSTANCE has a governing predicate, add it to the list of argument
   types in ARGUMENT_TYPES.  RETURN_TYPE is the type returned by the
   function.  */
static void
apply_predication (const function_instance &instance, tree return_type,
		   vec<tree> &argument_types)
{
  if (instance.pred != PRED_none)
    {
      argument_types.quick_insert (0, get_svbool_t ());
      /* For unary merge operations, the first argument is a vector with
	 the same type as the result.  */
      if (argument_types.length () == 2 && instance.pred == PRED_m)
	argument_types.quick_insert (0, return_type);
    }
}

/* Parse and move past an element type in FORMAT and return it as a type
   suffix.  The format is:

   [01]    - the element type in type suffix 0 or 1 of INSTANCE
   f<bits> - a floating-point type with the given number of bits
   f[01]   - a floating-point type with the same width as type suffix 0 or 1
   h<elt>  - a half-sized version of <elt>
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

  if (ch == 'p')
    return TYPE_SUFFIX_b;

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
   d       - displacement vector type (from a _<m1>index or _<m1>offset suffix)
   e<name> - an enum with the given name
   s<elt>  - a scalar type with the given element suffix
   t<elt>  - a vector or tuple type with given element suffix [*1]
   v<elt>  - a vector with the given element suffix

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

  if (ch == 'd')
    return instance.displacement_vector_type ();

  if (ch == 'e')
    {
      if (strncmp (format, "pattern", 7) == 0)
	{
	  format += 7;
	  return acle_svpattern;
	}
      if (strncmp (format, "prfop", 5) == 0)
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
  return return_type;
}

/* Add one function instance for GROUP, using mode suffix MODE_SUFFIX_ID,
   the type suffixes at index TI and the predication suffix at index PI.
   The other arguments are as for build_all.  */
static void
build_one (function_builder &b, const char *signature,
	   const function_group_info &group, mode_suffix_index mode_suffix_id,
	   unsigned int ti, unsigned int pi, bool force_direct_overloads)
{
  /* Byte forms of svdupq take 16 arguments.  */
  auto_vec<tree, 16> argument_types;
  function_instance instance (group.base_name, *group.base, *group.shape,
			      mode_suffix_id, group.types[ti],
			      group.preds[pi]);
  tree return_type = parse_signature (instance, signature, argument_types);
  apply_predication (instance, return_type, argument_types);
  b.add_unique_function (instance, return_type, argument_types,
			 group.required_extensions, force_direct_overloads);
}

/* Add a function instance for every type and predicate combination
   in GROUP, which describes some sort of gather or scatter operation.
   If the function has any type suffixes (as for loads and stores),
   the first function type suffix specifies either a 32-bit or a 64-bit
   type; use MODE32 for the former and MODE64 for the latter.  If the
   function has no type suffixes (as for prefetches), add one MODE32 form
   and one MODE64 form for each predication type.

   The other arguments are as for build_all.  */
static void
build_32_64 (function_builder &b, const char *signature,
	     const function_group_info &group, mode_suffix_index mode32,
	     mode_suffix_index mode64, bool force_direct_overloads = false)
{
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    if (group.types[0][0] == NUM_TYPE_SUFFIXES)
      {
	build_one (b, signature, group, mode32, 0, pi,
		   force_direct_overloads);
	build_one (b, signature, group, mode64, 0, pi,
		   force_direct_overloads);
      }
    else
      for (unsigned int ti = 0; group.types[ti][0] != NUM_TYPE_SUFFIXES; ++ti)
	{
	  unsigned int bits = type_suffixes[group.types[ti][0]].element_bits;
	  gcc_assert (bits == 32 || bits == 64);
	  mode_suffix_index mode = bits == 32 ? mode32 : mode64;
	  build_one (b, signature, group, mode, ti, pi,
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

/* Like build_sv_index, but taking vector byte offsets instead of vector
   array indices.  */
static void
build_sv_offset (function_builder &b, const char *signature,
		 const function_group_info &group)
{
  build_32_64 (b, signature, group, MODE_s32offset, MODE_s64offset);
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
    for (unsigned int ti = 0;
	 ti == 0 || group.types[ti][0] != NUM_TYPE_SUFFIXES; ++ti)
      build_one (b, signature, group, mode_suffix_id, ti, pi,
		 force_direct_overloads);
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
  explicit_type_suffix_p (unsigned int) const OVERRIDE
  {
    return true;
  }

  tree
  resolve (function_resolver &) const OVERRIDE
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
  explicit_type_suffix_p (unsigned int i) const OVERRIDE
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
  resolve (function_resolver &r) const OVERRIDE
  {
    unsigned int i, nargs;
    mode_suffix_index mode;
    if (!r.check_gp_argument (2, i, nargs)
	|| (mode = r.resolve_adr_address (0)) == MODE_none)
      return error_mark_node;

    return r.resolve_to (mode);
  };
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
  check (function_checker &c) const OVERRIDE
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
  resolve (function_resolver &r) const OVERRIDE
  {
    bool vnum_p = r.mode_suffix_id == MODE_vnum;
    gcc_assert (r.mode_suffix_id == MODE_none || vnum_p);

    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (vnum_p ? 2 : 1, i, nargs)
	|| (type = r.infer_pointer_type (i)) == NUM_TYPE_SUFFIXES
	|| (vnum_p && !r.require_scalar_type (i + 1, "int64_t")))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
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
  resolve (function_resolver &r) const OVERRIDE
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
  resolve (function_resolver &r) const OVERRIDE
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
  resolve (function_resolver &r) const OVERRIDE
  {
    bool has_displacement_p = r.displacement_units () != UNITS_none;

    unsigned int i, nargs;
    mode_suffix_index mode;
    type_suffix_index type;
    if (!r.check_gp_argument (has_displacement_p ? 3 : 2, i, nargs)
	|| (type = r.infer_sd_vector_type (nargs - 1)) == NUM_TYPE_SUFFIXES
	|| (mode = r.resolve_gather_address (i, type, false)) == MODE_none)
      return error_mark_node;

    return r.resolve_to (mode, type);
  }
};

/* sv<m0>_t svfoo[_m0base]_[m1]index(sv<m0>_t, sv<m1>_t)

   for all valid combinations of vector base type <m0> and vector
   displacement type <m1>.  */
struct adr_index_def : public adr_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vs0", group, MODE_none);
    build_all (b, "v0,v0,ss0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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

/* sv<t0>_t svfoo_<t0>(sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 16 / sizeof (<t0>_t) - 1].  */
struct binary_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    return c.require_immediate_lane_index (2);
  }
};
SHAPE (binary_lane)

/* sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0>_t).

   i.e. a binary operation in which the final argument is always a scalar
   rather than a vector.  */
struct binary_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, <t0>_t)

   i.e. the standard shape for binary operations that operate on
   uniform types.  */
struct binary_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (binary_opt_n)

/* svbool_t svfoo(svbool_t, svbool_t).  */
struct binary_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "v0,s0,s0", group, MODE_none);
  }
};
SHAPE (binary_scalar)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0:uint>_t)

   i.e. a version of "binary" in which the final argument is always an
   unsigned integer.  */
struct binary_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vu0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,su0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vu0", group, MODE_none);
    build_all (b, "v0,v0,su0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vu64", group, MODE_none);
    build_all (b, "v0,v0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   <t0>_t svfoo[_n_t0](<t0>_t, sv<t0>_t).  */
struct clast_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0", group, MODE_none);
    build_all (b, "s0,s0,v0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
	    || !r.require_matching_vector_type (i + 1, type))
	  return error_mark_node;
	return r.resolve_to (MODE_none, type);
      }
  }
};
SHAPE (clast)

/* svbool_t svfoo[_t0](sv<t0>_t, sv<t0>_t)
   svbool_t svfoo[_n_t0](sv<t0>_t, <t0>_t)

   i.e. a comparison between two vectors, or between a vector and a scalar.  */
struct compare_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0,v0", group, MODE_none);
    build_all (b, "vp,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform_opt_n (2);
  }
};
SHAPE (compare_opt_n)

/* svbool_t svfoo_t0[_t1](<t1>_t, <t1>_t)

   where _t0 is a _b<bits> suffix that describes the predicate result.
   There is no direct relationship between the element sizes of _t0
   and _t1.  */
struct compare_scalar_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,s1,s1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_integer_scalar_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_matching_integer_scalar_type (i + 1, i, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, r.type_suffix_ids[0], type);
  }
};
SHAPE (compare_scalar)

/* svbool_t svfoo[_t0](sv<t0>_t, svint64_t)  (for signed t0)
   svbool_t svfoo[_n_t0](sv<t0>_t, int64_t)  (for signed t0)
   svbool_t svfoo[_t0](sv<t0>_t, svuint64_t)  (for unsigned t0)
   svbool_t svfoo[_n_t0](sv<t0>_t, uint64_t)  (for unsigned t0)

   i.e. a comparison in which the second argument is 64 bits.  */
struct compare_wide_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vp,v0,vw0", group, MODE_none);
    build_all (b, "vp,v0,sw0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "su64", group, MODE_none);
  }
};
SHAPE (count_inherent)

/* uint64_t svfoo(enum svpattern).  */
struct count_pat_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "su64,epattern", group, MODE_none);
  }
};
SHAPE (count_pat)

/* uint64_t svfoo(svbool_t).  */
struct count_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "su64,vp", group, MODE_none);
  }
};
SHAPE (count_pred)

/* uint64_t svfoo[_t0](sv<t0>_t).  */
struct count_vector_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "su64,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,v0*t", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (r.vectors_per_tuple ());
  }
};
SHAPE (create)

/* sv<t0>_t svfoo[_n]_t0(<t0>_t, ..., <t0>_t)

   where there are enough arguments to fill 128 bits of data (or to
   control 128 bits of data in the case of predicates).  */
struct dupq_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    /* The "_n" suffix is optional; the full name has it, but the short
       name doesn't.  */
    build_all (b, "v0,s0*q", group, MODE_n, true);
  }

  tree
  resolve (function_resolver &) const OVERRIDE
  {
    /* The short forms just make "_n" implicit, so no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (dupq)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression that when
   multiplied by the number of bytes in t0 is in the range [0, 255].  */
struct ext_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    unsigned int bytes = c.type_suffix (0).element_bytes;
    return c.require_immediate_range (2, 0, 256 / bytes - 1);
  }
};
SHAPE (ext)

/* <t0>_t svfoo[_t0](<t0>_t, sv<t0>_t).  */
struct fold_left_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "s0,s0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,t0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_tuple_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_integer_immediate (i + 1))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    unsigned int nvectors = c.vectors_per_tuple ();
    return c.require_immediate_range (1, 0, nvectors - 1);
  }
};
SHAPE (get)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, uint64_t)
   <t0>_t svfoo[_n_t0](<t0>_t, uint64_t)

   where the t0 in the vector form is a signed or unsigned integer
   whose size is tied to the [bhwd] suffix of "svfoo".  */
struct inc_dec_def : public inc_dec_base
{
  CONSTEXPR inc_dec_def () : inc_dec_base (false) {}

  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vp", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "s0,s0,vp", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "t0", group, MODE_none);
  }
};
SHAPE (inherent)

/* svbool_t svfoo[_b]().  */
struct inherent_b_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    /* The "_b" suffix is optional; the full name has it, but the short
       name doesn't.  */
    build_all (b, "v0", group, MODE_none, true);
  }

  tree
  resolve (function_resolver &) const OVERRIDE
  {
    /* The short forms just make "_b" implicit, so no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (inherent_b)

/* sv<t0>[xN]_t svfoo[_t0](const <t0>_t *)
   sv<t0>[xN]_t svfoo_vnum[_t0](const <t0>_t *, int64_t).  */
struct load_def : public load_contiguous_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index (b, "t0,al,d", group);
    build_vs_index (b, "t0,b,ss64", group);
  }
};
SHAPE (load_ext_gather_index)

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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_offset (b, "t0,al,d", group);
    build_v_base (b, "t0,b", group, true);
    build_vs_offset (b, "t0,b,ss64", group);
  }
};
SHAPE (load_ext_gather_offset)

/* sv<t0>_t svfoo_[s32]index[_t0](const <t0>_t *, svint32_t)
   sv<t0>_t svfoo_[s64]index[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]index[_t0](const <t0>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]index[_t0](const <t0>_t *, svuint64_t)

   sv<t0>_t svfoo_[s32]offset[_t0](const <t0>_t *, svint32_t)
   sv<t0>_t svfoo_[s64]offset[_t0](const <t0>_t *, svint64_t)
   sv<t0>_t svfoo_[u32]offset[_t0](const <t0>_t *, svuint32_t)
   sv<t0>_t svfoo_[u64]offset[_t0](const <t0>_t *, svuint64_t).  */
struct load_gather_sv_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_index);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_index (b, "t0,al,d", group);
    build_sv_offset (b, "t0,al,d", group);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    unsigned int i, nargs;
    mode_suffix_index mode;
    type_suffix_index type;
    if (!r.check_gp_argument (2, i, nargs)
	|| (type = r.infer_pointer_type (i, true)) == NUM_TYPE_SUFFIXES
	|| (mode = r.resolve_sv_displacement (i + 1, type, true),
	    mode == MODE_none))
      return error_mark_node;

    return r.resolve_to (mode, type);
  }
};
SHAPE (load_gather_sv)

/* sv<t0>_t svfoo[_u32base]_t0(svuint32_t)
   sv<t0>_t svfoo[_u64base]_t0(svuint64_t)

   sv<t0>_t svfoo[_u32base]_index_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_index_t0(svuint64_t, int64_t)

   sv<t0>_t svfoo[_u32base]_offset_t0(svuint32_t, int64_t)
   sv<t0>_t svfoo[_u64base]_offset_t0(svuint64_t, int64_t).  */
struct load_gather_vs_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    /* The base vector mode is optional; the full name has it but the
       short name doesn't.  There is no ambiguity with SHAPE_load_gather_sv
       because the latter uses an implicit type suffix.  */
    build_v_base (b, "t0,b", group, true);
    build_vs_index (b, "t0,b,ss64", group, true);
    build_vs_offset (b, "t0,b,ss64", group, true);
  }

  tree
  resolve (function_resolver &) const OVERRIDE
  {
    /* The short name just makes the base vector mode implicit;
       no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (load_gather_vs)

/* sv<t0>_t svfoo[_t0](const <t0>_t *)

   The only difference from "load" is that this shape has no vnum form.  */
struct load_replicate_def : public load_contiguous_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,al", group, MODE_none);
  }
};
SHAPE (load_replicate)

/* svbool_t svfoo(enum svpattern).  */
struct pattern_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "vp,epattern", group, MODE_none);
  }
};
SHAPE (pattern_pred)

/* void svfoo(const void *, svprfop)
   void svfoo_vnum(const void *, int64_t, svprfop).  */
struct prefetch_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "sp,vp", group, MODE_none);
  }
};
SHAPE (ptest)

/* svbool_t svfoo().  */
struct rdffr_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "vp", group, MODE_none);
  }
};
SHAPE (rdffr)

/* <t0>_t svfoo[_t0](sv<t0>_t).  */
struct reduction_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "s0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (reduction)

/* int64_t svfoo[_t0](sv<t0>_t)  (for signed t0)
   uint64_t svfoo[_t0](sv<t0>_t)  (for unsigned t0)
   <t0>_t svfoo[_t0](sv<t0>_t)  (for floating-point t0)

   i.e. a version of "reduction" in which the return type for integers
   always has 64 bits.  */
struct reduction_wide_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "sw0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (1);
  }
};
SHAPE (reduction_wide)

/* sv<t0>xN_t svfoo[_t0](sv<t0>xN_t, uint64_t, sv<t0>_t)

   where the second argument is an integer constant expression in the
   range [0, N - 1].  */
struct set_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "t0,t0,su64,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_tuple_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_integer_immediate (i + 1)
	|| !r.require_derived_vector_type (i + 2, i, type))
      return error_mark_node;

    return r.resolve_to (r.mode_suffix_id, type);
  }

  bool
  check (function_checker &c) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "_", group, MODE_none);
  }
};
SHAPE (setffr)

/* sv<t0>_t svfoo[_n_t0])(sv<t0>_t, uint64_t)

   where the final argument must be an integer constant expression in the
   range [1, sizeof (<t0>_t) * 8].  */
struct shift_right_imm_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_n);
    build_all (b, "v0,v0,su64", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (1, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    unsigned int bits = c.type_suffix (0).element_bits;
    return c.require_immediate_range (1, 1, bits);
  }
};
SHAPE (shift_right_imm)

/* void svfoo[_t0](<X>_t *, sv<t0>[xN]_t)
   void svfoo_vnum[_t0](<X>_t *, int64_t, sv<t0>[xN]_t)

   where <X> might be tied to <t0> (for non-truncating stores) or might
   depend on the function base name (for truncating stores).  */
struct store_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_vnum);
    build_all (b, "_,as,t0", group, MODE_none);
    build_all (b, "_,as,ss64,t0", group, MODE_vnum);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    bool vnum_p = r.mode_suffix_id == MODE_vnum;
    gcc_assert (r.mode_suffix_id == MODE_none || vnum_p);

    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (vnum_p ? 3 : 2, i, nargs)
	|| !r.require_pointer_type (i)
	|| (vnum_p && !r.require_scalar_type (i + 1, "int64_t"))
	|| ((type = r.infer_tuple_type (nargs - 1)) == NUM_TYPE_SUFFIXES))
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_index);
    build_sv_index (b, "_,as,d,t0", group);
    build_vs_index (b, "_,b,ss64,t0", group);
  }
};
SHAPE (store_scatter_index)

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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    b.add_overloaded_functions (group, MODE_offset);
    build_sv_offset (b, "_,as,d,t0", group);
    build_v_base (b, "_,b,t0", group);
    build_vs_offset (b, "_,b,ss64,t0", group);
  }
};
SHAPE (store_scatter_offset)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 16 / sizeof (<t0>_t) - 1].  */
struct ternary_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (3, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    return c.require_immediate_lane_index (3);
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0,su64,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (3, 2);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    return (c.require_immediate_lane_index (3, 2)
	    && c.require_immediate_one_of (4, 0, 90, 180, 270));
  }
};
SHAPE (ternary_lane_rotate)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0>_t, sv<t0>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0>_t, <t0>_t)

   i.e. the standard shape for ternary operations that operate on
   uniform types.  */
struct ternary_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0", group, MODE_none);
    build_all (b, "v0,v0,v0,s0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform_opt_n (3);
  }
};
SHAPE (ternary_opt_n)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0.quarter>_t, sv<t0.quarter>_t, uint64_t)

   where the final argument is an integer constant expression in the range
   [0, 16 / sizeof (<t0>_t) - 1].  */
struct ternary_qq_lane_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vq0,vq0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  check (function_checker &c) const OVERRIDE
  {
    return c.require_immediate_lane_index (3, 4);
  }
};
SHAPE (ternary_qq_lane)

/* sv<t0>_t svfoo[_t0](sv<t0>_t, sv<t0.quarter>_t, sv<t0.quarter>_t)
   sv<t0>_t svfoo[_n_t0](sv<t0>_t, sv<t0.quarter>_t, <t0.quarter>_t)

   i.e. a version of the standard ternary shape ternary_opt_n in which
   the element type of the last two arguments is the quarter-sized
   equivalent of <t0>.  */
struct ternary_qq_opt_n_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,vq0,vq0", group, MODE_none);
    build_all (b, "v0,v0,vq0,sq0", group, MODE_n);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    unsigned int i, nargs;
    type_suffix_index type;
    if (!r.check_gp_argument (3, i, nargs)
	|| (type = r.infer_vector_type (i)) == NUM_TYPE_SUFFIXES
	|| !r.require_derived_vector_type (i + 1, i, type, r.SAME_TYPE_CLASS,
					   r.QUARTER_SIZE))
      return error_mark_node;

    return r.finish_opt_n_resolution (i + 2, i, type, r.SAME_TYPE_CLASS,
				      r.QUARTER_SIZE);
  }
};
SHAPE (ternary_qq_opt_n)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in
   {0, 90, 180, 270}.  */
struct ternary_rotate_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (3, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
  {
    return c.require_immediate_one_of (3, 0, 90, 180, 270);
  }
};
SHAPE (ternary_rotate)

/* svbool_t svfoo[_<t0>](sv<t0>_t, sv<t0>_t, uint64_t)

   where the final argument is an integer constant expression in the
   range [0, 7].  */
struct tmad_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0,v0,su64", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_uniform (2, 1);
  }

  bool
  check (function_checker &c) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_unary ();
  }
};
SHAPE (unary)

/* sv<t0>_t svfoo_t0[_t1](svbool_t, sv<t1>_t)

   where the target type <t0> must be specified explicitly but the source
   type <t1> can be inferred.  */
struct unary_convert_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,v1", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_unary (r.type_suffix (0).tclass,
			    r.type_suffix (0).element_bits);
  }
};
SHAPE (unary_convert)

/* sv<t0:uint>_t svfoo[_t0](sv<t0>_t)

   i.e. a version of "unary" in which the returned vector contains
   unsigned integers.  */
struct unary_count_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "vu0,v0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
  {
    return r.resolve_unary (TYPE_unsigned);
  }
};
SHAPE (unary_count)

/* sv<t0>_t svfoo[_n]_t0(<t0>_t).  */
struct unary_n_def : public overloaded_base<1>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    /* The "_n" suffix is optional; the full name has it, but the short
       name doesn't.  */
    build_all (b, "v0,s0", group, MODE_n, true);
  }

  tree
  resolve (function_resolver &) const OVERRIDE
  {
    /* The short forms just make "_n" implicit, so no resolution is needed.  */
    gcc_unreachable ();
  }
};
SHAPE (unary_n)

/* svbool_t svfoo(svbool_t).  */
struct unary_pred_def : public nonoverloaded_base
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    build_all (b, "v0,v0", group, MODE_none);
  }
};
SHAPE (unary_pred)

/* sv<t0>_t svfoo[_t0](sv<t0:uint>_t)

   where <t0> always belongs a certain type class, and where <t0:uint>
   therefore uniquely determines <t0>.  */
struct unary_uint_def : public overloaded_base<0>
{
  void
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vu0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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
  build (function_builder &b, const function_group_info &group) const OVERRIDE
  {
    b.add_overloaded_functions (group, MODE_none);
    build_all (b, "v0,vh0", group, MODE_none);
  }

  tree
  resolve (function_resolver &r) const OVERRIDE
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

}
