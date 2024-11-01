/* ACLE support for Arm MVE
   Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#define INCLUDE_MEMORY
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
#include "recog.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "explow.h"
#include "emit-rtl.h"
#include "langhooks.h"
#include "stringpool.h"
#include "attribs.h"
#include "diagnostic.h"
#include "arm-protos.h"
#include "arm-builtins.h"
#include "arm-mve-builtins.h"
#include "arm-mve-builtins-base.h"
#include "arm-mve-builtins-shapes.h"

namespace arm_mve {

/* Static information about each single-predicate or single-vector
   ACLE type.  */
struct vector_type_info
{
  /* The name of the type as declared by arm_mve.h.  */
  const char *acle_name;

  /* Whether the type requires a floating point abi.  */
  const bool requires_float;
};

/* Describes a function decl.  */
class GTY(()) registered_function
{
public:
  /* The ACLE function that the decl represents.  */
  function_instance instance GTY ((skip));

  /* The decl itself.  */
  tree decl;

  /* Whether the function requires a floating point abi.  */
  bool requires_float;

  /* True if the decl represents an overloaded function that needs to be
     resolved by function_resolver.  */
  bool overloaded_p;
};

/* Hash traits for registered_function.  */
struct registered_function_hasher : nofree_ptr_hash <registered_function>
{
  typedef function_instance compare_type;

  static hashval_t hash (value_type);
  static bool equal (value_type, const compare_type &);
};

/* Flag indicating whether the arm MVE types have been handled.  */
static bool handle_arm_mve_types_p;

/* Information about each single-predicate or single-vector type.  */
static CONSTEXPR const vector_type_info vector_types[] = {
#define DEF_MVE_TYPE(ACLE_NAME, SCALAR_TYPE) \
  { #ACLE_NAME, REQUIRES_FLOAT },
#include "arm-mve-builtins.def"
};

/* The function name suffix associated with each predication type.  */
static const char *const pred_suffixes[NUM_PREDS + 1] = {
  "",
  "_m",
  "_p",
  "_x",
  "_z",
  ""
};

/* Static information about each mode_suffix_index.  */
CONSTEXPR const mode_suffix_info mode_suffixes[] = {
#define VECTOR_TYPE_none NUM_VECTOR_TYPES
#define DEF_MVE_MODE(NAME, BASE, DISPLACEMENT, UNITS) \
  { "_" #NAME, VECTOR_TYPE_##BASE, VECTOR_TYPE_##DISPLACEMENT, UNITS_##UNITS },
#include "arm-mve-builtins.def"
#undef VECTOR_TYPE_none
  { "", NUM_VECTOR_TYPES, NUM_VECTOR_TYPES, UNITS_none }
};

/* Static information about each type_suffix_index.  */
CONSTEXPR const type_suffix_info type_suffixes[NUM_TYPE_SUFFIXES + 1] = {
#define DEF_MVE_TYPE_SUFFIX(NAME, ACLE_TYPE, CLASS, BITS, MODE)	\
  { "_" #NAME, \
    VECTOR_TYPE_##ACLE_TYPE, \
    TYPE_##CLASS, \
    BITS, \
    BITS / BITS_PER_UNIT, \
    TYPE_##CLASS == TYPE_signed || TYPE_##CLASS == TYPE_unsigned, \
    TYPE_##CLASS == TYPE_unsigned, \
    TYPE_##CLASS == TYPE_float, \
    TYPE_##CLASS == TYPE_poly, \
    0, \
    MODE },
#include "arm-mve-builtins.def"
  { "", NUM_VECTOR_TYPES, TYPE_bool, 0, 0, false, false, false, false,
    0, VOIDmode }
};

/* Define a TYPES_<combination> macro for each combination of type
   suffixes that an ACLE function can have, where <combination> is the
   name used in DEF_MVE_FUNCTION entries.

   Use S (T) for single type suffix T and D (T1, T2) for a pair of type
   suffixes T1 and T2.  Use commas to separate the suffixes.

   Although the order shouldn't matter, the convention is to sort the
   suffixes lexicographically after dividing suffixes into a type
   class ("b", "f", etc.) and a numerical bit count.  */

/* _f16.  */
#define TYPES_float_16(S, D) S (f16)

/* _f32.  */
#define TYPES_float_32(S, D) S (f32)

/* _f16 _f32.  */
#define TYPES_all_float(S, D) \
  S (f16), S (f32)

/* _s8  _u8 .  */
#define TYPES_integer_8(S, D) \
  S (s8), S (u8)

/* _s8 _s16
   _u8 _u16.  */
#define TYPES_integer_8_16(S, D) \
  S (s8), S (s16), S (u8), S(u16)

/* _s16 _s32
   _u16 _u32.  */
#define TYPES_integer_16_32(S, D)     \
  S (s16), S (s32),		      \
  S (u16), S (u32)

/* _s16 _s32.  */
#define TYPES_signed_16_32(S, D) \
  S (s16), S (s32)

/* _s8 _s16 _s32.  */
#define TYPES_all_signed(S, D) \
  S (s8), S (s16), S (s32)

/* _p8 _p16.  */
#define TYPES_poly_8_16(S, D) \
  S (p8), S (p16)

/* _u8 _u16 _u32.  */
#define TYPES_all_unsigned(S, D) \
  S (u8), S (u16), S (u32)

/* _s8 _s16 _s32
   _u8 _u16 _u32.  */
#define TYPES_all_integer(S, D) \
  TYPES_all_signed (S, D), TYPES_all_unsigned (S, D)

/* _s8 _s16 _s32 _s64
   _u8 _u16 _u32 _u64.  */
#define TYPES_all_integer_with_64(S, D) \
  TYPES_all_signed (S, D), S (s64), TYPES_all_unsigned (S, D), S (u64)

/* s32 _u32.  */
#define TYPES_integer_32(S, D) \
  S (s32), S (u32)

/* s32 .  */
#define TYPES_signed_32(S, D) \
  S (s32)

/* All the type combinations allowed by vcvtq.  */
#define TYPES_cvt(S, D) \
  D (f16, s16), \
  D (f16, u16), \
  \
  D (f32, s32), \
  D (f32, u32), \
  \
  D (s16, f16), \
  D (s32, f32), \
  \
  D (u16, f16), \
  D (u32, f32)

/* vcvt[bt]q_f16_f132.  */
#define TYPES_cvt_f16_f32(S, D) \
  D (f16, f32)

/* vcvt[bt]q_f32_f16.  */
#define TYPES_cvt_f32_f16(S, D) \
  D (f32, f16)

/* All the type combinations allowed by vcvtXq.  */
#define TYPES_cvtx(S, D) \
  D (s16, f16), \
  D (s32, f32), \
  \
  D (u16, f16), \
  D (u32, f32)

#define TYPES_reinterpret_signed1(D, A) \
  D (A, s8), D (A, s16), D (A, s32), D (A, s64)

#define TYPES_reinterpret_unsigned1(D, A) \
  D (A, u8), D (A, u16), D (A, u32), D (A, u64)

#define TYPES_reinterpret_integer(S, D) \
  TYPES_reinterpret_unsigned1 (D, s8), \
  D (s8, s16), D (s8, s32), D (s8, s64), \
  TYPES_reinterpret_unsigned1 (D, s16), \
  D (s16, s8), D (s16, s32), D (s16, s64), \
  TYPES_reinterpret_unsigned1 (D, s32), \
  D (s32, s8), D (s32, s16), D (s32, s64), \
  TYPES_reinterpret_unsigned1 (D, s64), \
  D (s64, s8), D (s64, s16), D (s64, s32), \
  TYPES_reinterpret_signed1 (D, u8), \
  D (u8, u16), D (u8, u32), D (u8, u64), \
  TYPES_reinterpret_signed1 (D, u16), \
  D (u16, u8), D (u16, u32), D (u16, u64), \
  TYPES_reinterpret_signed1 (D, u32), \
  D (u32, u8), D (u32, u16), D (u32, u64), \
  TYPES_reinterpret_signed1 (D, u64), \
  D (u64, u8), D (u64, u16), D (u64, u32)

/* { _s8  _s16 _s32 _s64 } x { _s8  _s16 _s32 _s64 }
   { _u8  _u16 _u32 _u64 }   { _u8  _u16 _u32 _u64 }.  */
#define TYPES_reinterpret_integer1(D, A) \
  TYPES_reinterpret_signed1 (D, A), \
  TYPES_reinterpret_unsigned1 (D, A)

#define TYPES_reinterpret_float1(D, A) \
  D (A, f16), D (A, f32)

#define TYPES_reinterpret_float(S, D) \
  TYPES_reinterpret_float1 (D, s8), \
  TYPES_reinterpret_float1 (D, s16), \
  TYPES_reinterpret_float1 (D, s32), \
  TYPES_reinterpret_float1 (D, s64), \
  TYPES_reinterpret_float1 (D, u8), \
  TYPES_reinterpret_float1 (D, u16), \
  TYPES_reinterpret_float1 (D, u32), \
  TYPES_reinterpret_float1 (D, u64), \
  TYPES_reinterpret_integer1 (D, f16), \
  TYPES_reinterpret_integer1 (D, f32), \
  D (f16, f32), D (f32, f16)

/* Describe a pair of type suffixes in which only the first is used.  */
#define DEF_VECTOR_TYPE(X) { TYPE_SUFFIX_ ## X, NUM_TYPE_SUFFIXES }

/* Describe a pair of type suffixes in which both are used.  */
#define DEF_DOUBLE_TYPE(X, Y) { TYPE_SUFFIX_ ## X, TYPE_SUFFIX_ ## Y }

/* Create an array that can be used in arm-mve-builtins.def to
   select the type suffixes in TYPES_<NAME>.  */
#define DEF_MVE_TYPES_ARRAY(NAME) \
  static const type_suffix_pair types_##NAME[] = { \
    TYPES_##NAME (DEF_VECTOR_TYPE, DEF_DOUBLE_TYPE), \
    { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES } \
  }

/* For functions that don't take any type suffixes.  */
static const type_suffix_pair types_none[] = {
  { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES },
  { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES }
};

DEF_MVE_TYPES_ARRAY (all_integer);
DEF_MVE_TYPES_ARRAY (all_integer_with_64);
DEF_MVE_TYPES_ARRAY (float_16);
DEF_MVE_TYPES_ARRAY (float_32);
DEF_MVE_TYPES_ARRAY (all_float);
DEF_MVE_TYPES_ARRAY (all_signed);
DEF_MVE_TYPES_ARRAY (all_unsigned);
DEF_MVE_TYPES_ARRAY (integer_8);
DEF_MVE_TYPES_ARRAY (integer_8_16);
DEF_MVE_TYPES_ARRAY (integer_16_32);
DEF_MVE_TYPES_ARRAY (integer_32);
DEF_MVE_TYPES_ARRAY (poly_8_16);
DEF_MVE_TYPES_ARRAY (signed_16_32);
DEF_MVE_TYPES_ARRAY (signed_32);
DEF_MVE_TYPES_ARRAY (cvt);
DEF_MVE_TYPES_ARRAY (cvt_f16_f32);
DEF_MVE_TYPES_ARRAY (cvt_f32_f16);
DEF_MVE_TYPES_ARRAY (cvtx);
DEF_MVE_TYPES_ARRAY (reinterpret_integer);
DEF_MVE_TYPES_ARRAY (reinterpret_float);

/* Used by functions that have no governing predicate.  */
static const predication_index preds_none[] = { PRED_none, NUM_PREDS };

/* Used by functions that have the m (merging) predicated form, and in
   addition have an unpredicated form.  */
const predication_index preds_m_or_none[] = {
  PRED_m, PRED_none, NUM_PREDS
};

/* Used by functions that have the mx (merging and "don't care"
   predicated forms, and in addition have an unpredicated form.  */
static const predication_index preds_mx_or_none[] = {
  PRED_m, PRED_x, PRED_none, NUM_PREDS
};

/* Used by functions that have the p predicated form, in addition to
   an unpredicated form.  */
static const predication_index preds_p_or_none[] = {
  PRED_p, PRED_none, NUM_PREDS
};

/* Used by functions that have the z predicated form, in addition to
   an unpredicated form.  */
static const predication_index preds_z_or_none[]
  = {PRED_z, PRED_none, NUM_PREDS};

/* A list of all MVE ACLE functions.  */
static CONSTEXPR const function_group_info function_groups[] = {
#define DEF_MVE_FUNCTION(NAME, SHAPE, TYPES, PREDS)			\
  { #NAME, &functions::NAME, &shapes::SHAPE, types_##TYPES, preds_##PREDS, \
    REQUIRES_FLOAT },
#include "arm-mve-builtins.def"
};

/* The scalar type associated with each vector type.  */
extern GTY(()) tree scalar_types[NUM_VECTOR_TYPES];
tree scalar_types[NUM_VECTOR_TYPES];

/* The single-predicate and single-vector types, with their built-in
   "__simd128_..._t" name.  Allow an index of NUM_VECTOR_TYPES, which always
   yields a null tree.  */
static GTY(()) tree abi_vector_types[NUM_VECTOR_TYPES + 1];

/* Same, but with the arm_mve.h names.  */
extern GTY(()) tree acle_vector_types[MAX_TUPLE_SIZE][NUM_VECTOR_TYPES + 1];
tree acle_vector_types[MAX_TUPLE_SIZE][NUM_VECTOR_TYPES + 1];

/* The list of all registered function decls, indexed by code.  */
static GTY(()) vec<registered_function *, va_gc> *registered_functions;

/* All registered function decls, hashed on the function_instance
   that they implement.  This is used for looking up implementations of
   overloaded functions.  */
static hash_table<registered_function_hasher> *function_table;

/* True if we've already complained about attempts to use functions
   when the required extension is disabled.  */
static bool reported_missing_float_p;

/* Return the MVE abi type with element of type TYPE.  */
static tree
arm_mve_type_for_scalar_type (tree eltype)
{
  for (unsigned int i = 0; i < __TYPE_FINAL; ++i)
      if (arm_simd_types[i].eltype == eltype
	  && GET_MODE_SIZE (arm_simd_types[i].mode) == 16)
	return arm_simd_types[i].itype;

  gcc_unreachable ();
}

/* Register the built-in MVE ABI vector types, such as uint32x4_t.  */
static void
register_builtin_types ()
{
#define DEF_MVE_TYPE(ACLE_NAME, SCALAR_TYPE) \
  scalar_types[VECTOR_TYPE_ ## ACLE_NAME] = SCALAR_TYPE;
#include "arm-mve-builtins.def"
  for (unsigned int i = 0; i < NUM_VECTOR_TYPES; ++i)
    {
      if (vector_types[i].requires_float && !TARGET_HAVE_MVE_FLOAT)
	continue;
      tree eltype = scalar_types[i];
      tree vectype;
      if (eltype == boolean_type_node)
	{
	  vectype = get_typenode_from_name (UINT16_TYPE);
	  gcc_assert (GET_MODE_SIZE (TYPE_MODE (vectype)) == 2);
	}
      else
	{
	  vectype = arm_mve_type_for_scalar_type (eltype);
	  gcc_assert (VECTOR_MODE_P (TYPE_MODE (vectype))
		      && GET_MODE_SIZE (TYPE_MODE (vectype)) == 16);
	}
      abi_vector_types[i] = vectype;
    }
}

/* Register vector type TYPE under its arm_mve.h name.  */
static void
register_vector_type (vector_type_index type)
{

  /* If the target does not have the mve.fp extension, but the type requires
     it, then it needs to be assigned a non-dummy type so that functions
     with those types in their signature can be registered.  This allows for
     diagnostics about the missing extension, rather than about a missing
     function definition.  */
  if (vector_types[type].requires_float && !TARGET_HAVE_MVE_FLOAT)
    {
      acle_vector_types[0][type] = void_type_node;
      return;
    }

  tree vectype = abi_vector_types[type];
  tree id = get_identifier (vector_types[type].acle_name);
  tree decl = build_decl (input_location, TYPE_DECL, id, vectype);
  decl = lang_hooks.decls.pushdecl (decl);

  /* Record the new ACLE type if pushdecl succeeded without error.  Use
     the ABI type otherwise, so that the type we record at least has the
     right form, even if it doesn't have the right name.  This should give
     better error recovery behavior than installing error_mark_node or
     installing an incorrect type.  */
  if (decl
      && TREE_CODE (decl) == TYPE_DECL
      && TREE_TYPE (decl) != error_mark_node
      && TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == vectype)
    vectype = TREE_TYPE (decl);
  acle_vector_types[0][type] = vectype;
}

/* Register tuple types of element type TYPE under their arm_mve_types.h
   names.  */
static void
register_builtin_tuple_types (vector_type_index type)
{
  const vector_type_info* info = &vector_types[type];

  /* If the target does not have the mve.fp extension, but the type requires
     it, then it needs to be assigned a non-dummy type so that functions
     with those types in their signature can be registered.  This allows for
     diagnostics about the missing extension, rather than about a missing
     function definition.  */
  if (scalar_types[type] == boolean_type_node
      || (info->requires_float && !TARGET_HAVE_MVE_FLOAT))
    {
      for (unsigned int num_vectors = 2; num_vectors <= 4; num_vectors += 2)
	acle_vector_types[num_vectors >> 1][type] = void_type_node;
    return;
    }

  const char *vector_type_name = info->acle_name;
  char buffer[sizeof ("float32x4x2_t")];
  for (unsigned int num_vectors = 2; num_vectors <= 4; num_vectors += 2)
    {
      snprintf (buffer, sizeof (buffer), "%.*sx%d_t",
		(int) strlen (vector_type_name) - 2, vector_type_name,
		num_vectors);

      tree vectype = acle_vector_types[0][type];
      tree arrtype = build_array_type_nelts (vectype, num_vectors);
      gcc_assert (TYPE_MODE_RAW (arrtype) == TYPE_MODE (arrtype));
      tree field = build_decl (input_location, FIELD_DECL,
			       get_identifier ("val"), arrtype);

      tree t = lang_hooks.types.simulate_record_decl (input_location, buffer,
						      make_array_slice (&field,
									1));
      gcc_assert (TYPE_MODE_RAW (t) == TYPE_MODE (t));
      acle_vector_types[num_vectors >> 1][type] = TREE_TYPE (t);
    }
}

/* Implement #pragma GCC arm "arm_mve_types.h".  */
void
handle_arm_mve_types_h ()
{
  if (handle_arm_mve_types_p)
    {
      error ("duplicate definition of %qs", "arm_mve_types.h");
      return;
    }
  handle_arm_mve_types_p = true;
  if (!TARGET_HAVE_MVE)
    {
      error ("this definition requires the MVE ISA extension");
      return;
    }
  register_builtin_types ();
  for (unsigned int type_i = 0; type_i < NUM_VECTOR_TYPES; ++type_i)
    {
      vector_type_index type = vector_type_index (type_i);
      register_vector_type (type);
      if (type_i != VECTOR_TYPE_mve_pred16_t)
	register_builtin_tuple_types (type);
    }
}

/* Implement #pragma GCC arm "arm_mve.h" <bool>.  */
void
handle_arm_mve_h (bool preserve_user_namespace)
{
  if (function_table)
    {
      error ("duplicate definition of %qs", "arm_mve.h");
      return;
    }

  if (!handle_arm_mve_types_p)
    {
      error ("this definition requires MVE types, please include %qs",
	     "arm_mve_types.h");
      return;
    }

  /* Define MVE functions.  */
  function_table = new hash_table<registered_function_hasher> (1023);
  function_builder builder;
  for (unsigned int i = 0; i < ARRAY_SIZE (function_groups); ++i)
    builder.register_function_group (function_groups[i],
				     preserve_user_namespace);
}

/* Return the function decl with MVE function subcode CODE, or error_mark_node
   if no such function exists.  */
tree
builtin_decl (unsigned int code)
{
  if (code >= vec_safe_length (registered_functions))
    return error_mark_node;
  return (*registered_functions)[code]->decl;
}

/* Return true if CANDIDATE is equivalent to MODEL_TYPE for overloading
   purposes.  */
static bool
matches_type_p (const_tree model_type, const_tree candidate)
{
  if (VECTOR_TYPE_P (model_type))
    {
      if (!VECTOR_TYPE_P (candidate)
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (model_type),
		       TYPE_VECTOR_SUBPARTS (candidate))
	  || TYPE_MODE (model_type) != TYPE_MODE (candidate))
	return false;

      model_type = TREE_TYPE (model_type);
      candidate = TREE_TYPE (candidate);
    }
  return (candidate != error_mark_node
	  && TYPE_MAIN_VARIANT (model_type) == TYPE_MAIN_VARIANT (candidate));
}

/* If TYPE is a valid MVE element type, return the corresponding type
   suffix, otherwise return NUM_TYPE_SUFFIXES.  */
static type_suffix_index
find_type_suffix_for_scalar_type (const_tree type)
{
  /* A linear search should be OK here, since the code isn't hot and
     the number of types is only small.  */
  for (unsigned int suffix_i = 0; suffix_i < NUM_TYPE_SUFFIXES; ++suffix_i)
      {
	vector_type_index vector_i = type_suffixes[suffix_i].vector_type;
	if (matches_type_p (scalar_types[vector_i], type))
	  return type_suffix_index (suffix_i);
      }
  return NUM_TYPE_SUFFIXES;
}

/* Report an error against LOCATION that the user has tried to use
   a floating point function when the mve.fp extension is disabled.  */
static void
report_missing_float (location_t location, tree fndecl)
{
  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_missing_float_p)
    return;

  error_at (location, "ACLE function %qD requires ISA extension %qs",
	    fndecl, "mve.fp");
  inform (location, "you can enable mve.fp by using the command-line"
	  " option %<-march%>, or by using the %<target%>"
	  " attribute or pragma");
  reported_missing_float_p = true;
}

/* Report that LOCATION has a call to FNDECL in which argument ARGNO
   was not an integer constant expression.  ARGNO counts from zero.  */
static void
report_non_ice (location_t location, tree fndecl, unsigned int argno)
{
  error_at (location, "argument %d of %qE must be an integer constant"
	    " expression", argno + 1, fndecl);
}

/* Report that LOCATION has a call to FNDECL in which argument ARGNO has
   the value ACTUAL, whereas the function requires a value in the range
   [MIN, MAX].  ARGNO counts from zero.  */
static void
report_out_of_range (location_t location, tree fndecl, unsigned int argno,
		     HOST_WIDE_INT actual, HOST_WIDE_INT min,
		     HOST_WIDE_INT max)
{
  error_at (location, "passing %wd to argument %d of %qE, which expects"
	    " a value in the range [%wd, %wd]", actual, argno + 1, fndecl,
	    min, max);
}

/* Report that LOCATION has a call to FNDECL in which argument ARGNO has
   the value ACTUAL, whereas the function requires a valid value of
   enum type ENUMTYPE.  ARGNO counts from zero.  */
static void
report_not_enum (location_t location, tree fndecl, unsigned int argno,
		 HOST_WIDE_INT actual, tree enumtype)
{
  error_at (location, "passing %wd to argument %d of %qE, which expects"
	    " a valid %qT value", actual, argno + 1, fndecl, enumtype);
}

/* Report that LOCATION has a call to FNDECL in which argument ARGNO has
   the value ACTUAL, whereas the function requires one of VALUE0..3.
   ARGNO counts from zero.  */
static void
report_not_one_of (location_t location, tree fndecl, unsigned int argno,
		   HOST_WIDE_INT actual, HOST_WIDE_INT value0,
		   HOST_WIDE_INT value1, HOST_WIDE_INT value2,
		   HOST_WIDE_INT value3)
{
  error_at (location, "passing %wd to argument %d of %qE, which expects"
	    " %wd, %wd, %wd or %wd", actual, argno + 1, fndecl, value0, value1,
	    value2, value3);
}

/* Checks that the mve.fp extension is enabled, given that REQUIRES_FLOAT
   indicates whether it is required or not for function FNDECL.
   Report an error against LOCATION if not.  */
static bool
check_requires_float (location_t location, tree fndecl,
		      bool requires_float)
{
  if (requires_float && !TARGET_HAVE_MVE_FLOAT)
    {
      report_missing_float (location, fndecl);
      return false;
    }

  return true;
}

/* Return a hash code for a function_instance.  */
hashval_t
function_instance::hash () const
{
  inchash::hash h;
  /* BASE uniquely determines BASE_NAME, so we don't need to hash both.  */
  h.add_ptr (base);
  h.add_ptr (shape);
  h.add_int (mode_suffix_id);
  h.add_int (type_suffix_ids[0]);
  h.add_int (type_suffix_ids[1]);
  h.add_int (pred);
  return h.end ();
}

/* Return a set of CP_* flags that describe what the function could do,
   taking the command-line flags into account.  */
unsigned int
function_instance::call_properties () const
{
  unsigned int flags = base->call_properties (*this);

  /* -fno-trapping-math means that we can assume any FP exceptions
     are not user-visible.  */
  if (!flag_trapping_math)
    flags &= ~CP_RAISE_FP_EXCEPTIONS;

  return flags;
}

/* Return true if calls to the function could read some form of
   global state.  */
bool
function_instance::reads_global_state_p () const
{
  unsigned int flags = call_properties ();

  /* Preserve any dependence on rounding mode, flush to zero mode, etc.
     There is currently no way of turning this off; in particular,
     -fno-rounding-math (which is the default) means that we should make
     the usual assumptions about rounding mode, which for intrinsics means
     acting as the instructions do.  */
  if (flags & CP_READ_FPCR)
    return true;

  return flags & CP_READ_MEMORY;
}

/* Return true if calls to the function could modify some form of
   global state.  */
bool
function_instance::modifies_global_state_p () const
{
  unsigned int flags = call_properties ();

  /* Preserve any exception state written back to the FPCR,
     unless -fno-trapping-math says this is unnecessary.  */
  if (flags & CP_RAISE_FP_EXCEPTIONS)
    return true;

  /* Handle direct modifications of global state.  */
  return flags & CP_WRITE_MEMORY;
}

/* Return true if calls to the function could raise a signal.  */
bool
function_instance::could_trap_p () const
{
  unsigned int flags = call_properties ();

  /* Handle functions that could raise SIGFPE.  */
  if (flags & CP_RAISE_FP_EXCEPTIONS)
    return true;

  /* Handle functions that could raise SIGBUS or SIGSEGV.  */
  if (flags & (CP_READ_MEMORY | CP_WRITE_MEMORY))
    return true;

  return false;
}

/* Return true if the function has an implicit "inactive" argument.
   This is the case of most _m predicated functions, but not all.
   The list will be updated as needed.  */
bool
function_instance::has_inactive_argument () const
{
  if (pred != PRED_m)
    return false;

  if (mode_suffix_id == MODE_r
      || (base == functions::vbicq && mode_suffix_id == MODE_n)
      || base == functions::vcmlaq
      || base == functions::vcmlaq_rot90
      || base == functions::vcmlaq_rot180
      || base == functions::vcmlaq_rot270
      || base == functions::vcmpeqq
      || base == functions::vcmpneq
      || base == functions::vcmpgeq
      || base == functions::vcmpgtq
      || base == functions::vcmpleq
      || base == functions::vcmpltq
      || base == functions::vcmpcsq
      || base == functions::vcmphiq
      || base == functions::vctp16q
      || base == functions::vctp32q
      || base == functions::vctp64q
      || base == functions::vctp8q
      || (base == functions::vcvtbq && type_suffix (0).element_bits == 16)
      || (base == functions::vcvttq && type_suffix (0).element_bits == 16)
      || base == functions::vfmaq
      || base == functions::vfmasq
      || base == functions::vfmsq
      || base == functions::vmaxaq
      || base == functions::vmaxnmaq
      || base == functions::vminaq
      || base == functions::vminnmaq
      || base == functions::vmlaq
      || base == functions::vmlasq
      || base == functions::vmovnbq
      || base == functions::vmovntq
      || base == functions::vqmovnbq
      || base == functions::vqmovntq
      || base == functions::vqmovunbq
      || base == functions::vqmovuntq
      || (base == functions::vorrq && mode_suffix_id == MODE_n)
      || base == functions::vqdmladhq
      || base == functions::vqdmladhxq
      || base == functions::vqdmlahq
      || base == functions::vqdmlashq
      || base == functions::vqdmlsdhq
      || base == functions::vqdmlsdhxq
      || base == functions::vqrdmladhq
      || base == functions::vqrdmladhxq
      || base == functions::vqrdmlahq
      || base == functions::vqrdmlashq
      || base == functions::vqrdmlsdhq
      || base == functions::vqrdmlsdhxq
      || (base == functions::vqrshlq && mode_suffix_id == MODE_n)
      || base == functions::vqrshrnbq
      || base == functions::vqrshrntq
      || base == functions::vqrshrunbq
      || base == functions::vqrshruntq
      || base == functions::vqshrnbq
      || base == functions::vqshrntq
      || base == functions::vqshrunbq
      || base == functions::vqshruntq
      || (base == functions::vrshlq && mode_suffix_id == MODE_n)
      || base == functions::vrshrnbq
      || base == functions::vrshrntq
      || base == functions::vshlcq
      || base == functions::vshrnbq
      || base == functions::vshrntq
      || base == functions::vsliq
      || base == functions::vsriq)
    return false;

  return true;
}

inline hashval_t
registered_function_hasher::hash (value_type value)
{
  return value->instance.hash ();
}

inline bool
registered_function_hasher::equal (value_type value, const compare_type &key)
{
  return value->instance == key;
}

function_builder::function_builder ()
{
  m_overload_type = build_function_type (void_type_node, void_list_node);
  m_direct_overloads = lang_GNU_CXX ();
  gcc_obstack_init (&m_string_obstack);
}

function_builder::~function_builder ()
{
  obstack_free (&m_string_obstack, NULL);
}

/* Add NAME to the end of the function name being built.  */
void
function_builder::append_name (const char *name)
{
  obstack_grow (&m_string_obstack, name, strlen (name));
}

/* Zero-terminate and complete the function name being built.  */
char *
function_builder::finish_name ()
{
  obstack_1grow (&m_string_obstack, 0);
  return (char *) obstack_finish (&m_string_obstack);
}

/* Return the overloaded or full function name for INSTANCE, with optional
   prefix; PRESERVE_USER_NAMESPACE selects the prefix, and OVERLOADED_P
   selects which the overloaded or full function name.  Allocate the string on
   m_string_obstack; the caller must use obstack_free to free it after use.  */
char *
function_builder::get_name (const function_instance &instance,
			    bool preserve_user_namespace,
			    bool overloaded_p)
{
  if (preserve_user_namespace)
    append_name ("__arm_");
  append_name (instance.base_name);
  append_name (pred_suffixes[instance.pred]);
  if (!overloaded_p
      || instance.shape->explicit_mode_suffix_p (instance.pred,
						 instance.mode_suffix_id))
    append_name (instance.mode_suffix ().string);
  for (unsigned int i = 0; i < 2; ++i)
    if (!overloaded_p
	|| instance.shape->explicit_type_suffix_p (i, instance.pred,
						   instance.mode_suffix_id,
						   instance.type_suffix (i)))
      append_name (instance.type_suffix (i).string);
  return finish_name ();
}

/* Add attribute NAME to ATTRS.  */
static tree
add_attribute (const char *name, tree attrs)
{
  return tree_cons (get_identifier (name), NULL_TREE, attrs);
}

/* Return the appropriate function attributes for INSTANCE.  */
tree
function_builder::get_attributes (const function_instance &instance)
{
  tree attrs = NULL_TREE;

  if (!instance.modifies_global_state_p ())
    {
      if (instance.reads_global_state_p ())
	attrs = add_attribute ("pure", attrs);
      else
	attrs = add_attribute ("const", attrs);
    }

  if (!flag_non_call_exceptions || !instance.could_trap_p ())
    attrs = add_attribute ("nothrow", attrs);

  return add_attribute ("leaf", attrs);
}

/* Add a function called NAME with type FNTYPE and attributes ATTRS.
   INSTANCE describes what the function does and OVERLOADED_P indicates
   whether it is overloaded.  REQUIRES_FLOAT indicates whether the function
   requires the mve.fp extension.  */
registered_function &
function_builder::add_function (const function_instance &instance,
				const char *name, tree fntype, tree attrs,
				bool requires_float,
				bool overloaded_p,
				bool placeholder_p)
{
  unsigned int code = vec_safe_length (registered_functions);
  code = (code << ARM_BUILTIN_SHIFT) | ARM_BUILTIN_MVE;

  /* We need to be able to generate placeholders to ensure that we have a
     consistent numbering scheme for function codes between the C and C++
     frontends, so that everything ties up in LTO.

     Currently, tree-streamer-in.cc:unpack_ts_function_decl_value_fields
     validates that tree nodes returned by TARGET_BUILTIN_DECL are non-NULL and
     some node other than error_mark_node.  This is a holdover from when builtin
     decls were streamed by code rather than by value.

     Ultimately, we should be able to remove this validation of BUILT_IN_MD
     nodes and remove the target hook.  For now, however, we need to appease the
     validation and return a non-NULL, non-error_mark_node node, so we
     arbitrarily choose integer_zero_node.  */
  tree decl = placeholder_p
    ? integer_zero_node
    : simulate_builtin_function_decl (input_location, name, fntype,
				      code, NULL, attrs);
  registered_function &rfn = *ggc_alloc <registered_function> ();
  rfn.instance = instance;
  rfn.decl = decl;
  rfn.requires_float = requires_float;
  rfn.overloaded_p = overloaded_p;
  vec_safe_push (registered_functions, &rfn);

  return rfn;
}

/* Add a built-in function for INSTANCE, with the argument types given
   by ARGUMENT_TYPES and the return type given by RETURN_TYPE.
   REQUIRES_FLOAT indicates whether the function requires the mve.fp extension,
   and PRESERVE_USER_NAMESPACE indicates whether the function should also be
   registered under its non-prefixed name.  */
void
function_builder::add_unique_function (const function_instance &instance,
				       tree return_type,
				       vec<tree> &argument_types,
				       bool preserve_user_namespace,
				       bool requires_float,
				       bool force_direct_overloads)
{
  /* Add the function under its full (unique) name with prefix.  */
  char *name = get_name (instance, true, false);
  tree fntype = build_function_type_array (return_type,
					   argument_types.length (),
					   argument_types.address ());
  tree attrs = get_attributes (instance);
  registered_function &rfn = add_function (instance, name, fntype, attrs,
					   requires_float, false, false);

  /* Enter the function into the hash table.  */
  hashval_t hash = instance.hash ();
  registered_function **rfn_slot
    = function_table->find_slot_with_hash (instance, hash, INSERT);
  gcc_assert (!*rfn_slot);
  *rfn_slot = &rfn;

  /* Also add the non-prefixed non-overloaded function, as placeholder
     if the user namespace does not need to be preserved.  */
  char *noprefix_name = get_name (instance, false, false);
  attrs = get_attributes (instance);
  add_function (instance, noprefix_name, fntype, attrs, requires_float,
		false, preserve_user_namespace);

  /* Also add the function under its overloaded alias, if we want
     a separate decl for each instance of an overloaded function.  */
  char *overload_name = get_name (instance, true, true);
  if (strcmp (name, overload_name) != 0)
    {
      /* Attribute lists shouldn't be shared.  */
      attrs = get_attributes (instance);
      bool placeholder_p = !(m_direct_overloads || force_direct_overloads);
      add_function (instance, overload_name, fntype, attrs,
		    requires_float, false, placeholder_p);

      /* Also add the non-prefixed overloaded function, as placeholder
	 if the user namespace does not need to be preserved.  */
      char *noprefix_overload_name = get_name (instance, false, true);
      attrs = get_attributes (instance);
      add_function (instance, noprefix_overload_name, fntype, attrs,
		    requires_float, false, preserve_user_namespace || placeholder_p);
    }

  obstack_free (&m_string_obstack, name);
}

/* Add one function decl for INSTANCE, to be used with manual overload
   resolution.  REQUIRES_FLOAT indicates whether the function requires the
   mve.fp extension.

   For simplicity, partition functions by instance and required extensions,
   and check whether the required extensions are available as part of resolving
   the function to the relevant unique function.  */
void
function_builder::add_overloaded_function (const function_instance &instance,
					   bool preserve_user_namespace,
					   bool requires_float)
{
  char *name = get_name (instance, true, true);
  if (registered_function **map_value = m_overload_names.get (name))
    {
      gcc_assert ((*map_value)->instance == instance);
      obstack_free (&m_string_obstack, name);
    }
  else
    {
      registered_function &rfn
	= add_function (instance, name, m_overload_type, NULL_TREE,
			requires_float, true, m_direct_overloads);
      m_overload_names.put (name, &rfn);

      /* Also add the non-prefixed function, as placeholder if the
	 user namespace does not need to be preserved.  */
      char *noprefix_name = get_name (instance, false, true);
      registered_function &noprefix_rfn
	= add_function (instance, noprefix_name, m_overload_type,
			NULL_TREE, requires_float, true,
			preserve_user_namespace || m_direct_overloads);
      m_overload_names.put (noprefix_name, &noprefix_rfn);
    }
}

/* If we are using manual overload resolution, add one function decl
   for each overloaded function in GROUP.  Take the function base name
   from GROUP and the mode from MODE.  */
void
function_builder::add_overloaded_functions (const function_group_info &group,
					    mode_suffix_index mode,
					    bool preserve_user_namespace)
{
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    {
      unsigned int explicit_type0
	= (*group.shape)->explicit_type_suffix_p (0, group.preds[pi], mode,
						  type_suffixes[NUM_TYPE_SUFFIXES]);
      unsigned int explicit_type1
	= (*group.shape)->explicit_type_suffix_p (1, group.preds[pi], mode,
						  type_suffixes[NUM_TYPE_SUFFIXES]);

      if ((*group.shape)->skip_overload_p (group.preds[pi], mode))
	continue;

      if (!explicit_type0 && !explicit_type1)
	{
	  /* Deal with the common case in which there is one overloaded
	     function for all type combinations.  */
	  function_instance instance (group.base_name, *group.base,
				      *group.shape, mode, types_none[0],
				      group.preds[pi]);
	  add_overloaded_function (instance, preserve_user_namespace,
				   group.requires_float);
	}
      else
	for (unsigned int ti = 0; group.types[ti][0] != NUM_TYPE_SUFFIXES;
	     ++ti)
	  {
	    /* Stub out the types that are determined by overload
	       resolution.  */
	    type_suffix_pair types = {
	      explicit_type0 ? group.types[ti][0] : NUM_TYPE_SUFFIXES,
	      explicit_type1 ? group.types[ti][1] : NUM_TYPE_SUFFIXES
	    };
	    function_instance instance (group.base_name, *group.base,
					*group.shape, mode, types,
					group.preds[pi]);
	    add_overloaded_function (instance, preserve_user_namespace,
				     group.requires_float);
	  }
    }
}

/* Register all the functions in GROUP.  */
void
function_builder::register_function_group (const function_group_info &group,
					   bool preserve_user_namespace)
{
  (*group.shape)->build (*this, group, preserve_user_namespace);
}

function_call_info::function_call_info (location_t location_in,
					const function_instance &instance_in,
					tree fndecl_in)
  : function_instance (instance_in), location (location_in), fndecl (fndecl_in)
{
}

function_resolver::function_resolver (location_t location,
				      const function_instance &instance,
				      tree fndecl, vec<tree, va_gc> &arglist)
  : function_call_info (location, instance, fndecl), m_arglist (arglist)
{
}

/* Return the vector type associated with type suffix TYPE.  */
tree
function_resolver::get_vector_type (type_suffix_index type)
{
  return acle_vector_types[0][type_suffixes[type].vector_type];
}

/* Return the <stdint.h> name associated with TYPE.  Using the <stdint.h>
   name should be more user-friendly than the underlying canonical type,
   since it makes the signedness and bitwidth explicit.  */
const char *
function_resolver::get_scalar_type_name (type_suffix_index type)
{
  return vector_types[type_suffixes[type].vector_type].acle_name + 2;
}

/* Return the type of argument I, or error_mark_node if it isn't
   well-formed.  */
tree
function_resolver::get_argument_type (unsigned int i)
{
  tree arg = m_arglist[i];
  return arg == error_mark_node ? arg : TREE_TYPE (arg);
}

/* Return true if argument I is some form of scalar value.  */
bool
function_resolver::scalar_argument_p (unsigned int i)
{
  tree type = get_argument_type (i);
  return (INTEGRAL_TYPE_P (type)
	  /* Allow pointer types, leaving the frontend to warn where
	     necessary.  */
	  || POINTER_TYPE_P (type)
	  || SCALAR_FLOAT_TYPE_P (type));
}

/* Report that the function has no form that takes type suffix TYPE.
   Return error_mark_node.  */
tree
function_resolver::report_no_such_form (type_suffix_index type)
{
  error_at (location, "%qE has no form that takes %qT arguments",
	    fndecl, get_vector_type (type));
  return error_mark_node;
}

/* Silently check whether there is an instance of the function with the
   mode suffix given by MODE and the type suffixes given by TYPE0 and TYPE1.
   Return its function decl if so, otherwise return null.  */
tree
function_resolver::lookup_form (mode_suffix_index mode,
				type_suffix_index type0,
				type_suffix_index type1)
{
  type_suffix_pair types = { type0, type1 };
  function_instance instance (base_name, base, shape, mode, types, pred);
  registered_function *rfn
    = function_table->find_with_hash (instance, instance.hash ());
  return rfn ? rfn->decl : NULL_TREE;
}

/* Resolve the function to one with the mode suffix given by MODE and the
   type suffixes given by TYPE0 and TYPE1.  Return its function decl on
   success, otherwise report an error and return error_mark_node.  */
tree
function_resolver::resolve_to (mode_suffix_index mode,
			       type_suffix_index type0,
			       type_suffix_index type1)
{
  tree res = lookup_form (mode, type0, type1);
  if (!res)
    {
      if (type1 == NUM_TYPE_SUFFIXES)
	return report_no_such_form (type0);
      if (type0 == type_suffix_ids[0])
	return report_no_such_form (type1);
      /* To be filled in when we have other cases.  */
      gcc_unreachable ();
    }
  return res;
}

/* Require argument ARGNO to be a pointer to a scalar type that has a
   corresponding type suffix.  Return that type suffix on success,
   otherwise report an error and return NUM_TYPE_SUFFIXES.  */
type_suffix_index
function_resolver::infer_pointer_type (unsigned int argno)
{
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return NUM_TYPE_SUFFIXES;

  if (TREE_CODE (actual) != POINTER_TYPE)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a pointer type", actual, argno + 1, fndecl);
      return NUM_TYPE_SUFFIXES;
    }

  tree target = TREE_TYPE (actual);
  type_suffix_index type = find_type_suffix_for_scalar_type (target);
  if (type == NUM_TYPE_SUFFIXES)
    {
      error_at (location, "passing %qT to argument %d of %qE, but %qT is not"
		" a valid MVE element type", actual, argno + 1, fndecl,
		build_qualified_type (target, 0));
      return NUM_TYPE_SUFFIXES;
    }

  return type;
}

/* Require argument ARGNO to be a single vector or a tuple of NUM_VECTORS
   vectors; NUM_VECTORS is 1 for the former.  Return the associated type
   suffix on success, using TYPE_SUFFIX_b for predicates.  Report an error
   and return NUM_TYPE_SUFFIXES on failure.  */
type_suffix_index
function_resolver::infer_vector_or_tuple_type (unsigned int argno,
					       unsigned int num_vectors)
{
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return NUM_TYPE_SUFFIXES;

  /* A linear search should be OK here, since the code isn't hot and
     the number of types is only small.  */
  for (unsigned int size_i = 0; size_i < MAX_TUPLE_SIZE; ++size_i)
    for (unsigned int suffix_i = 0; suffix_i < NUM_TYPE_SUFFIXES; ++suffix_i)
      {
	vector_type_index type_i = type_suffixes[suffix_i].vector_type;
	tree type = acle_vector_types[size_i][type_i];
	if (type && matches_type_p (type, actual))
	  {
	    if (size_i + 1 == num_vectors)
	      return type_suffix_index (suffix_i);

	    if (num_vectors == 1)
	      error_at (location, "passing %qT to argument %d of %qE, which"
			" expects a single MVE vector rather than a tuple",
			actual, argno + 1, fndecl);
	    else if (size_i == 0 && type_i != VECTOR_TYPE_mve_pred16_t)
	      /* num_vectors is always != 1, so the singular isn't needed.  */
	      error_n (location, num_vectors, "%qT%d%qE%d",
		       "passing single vector %qT to argument %d"
		       " of %qE, which expects a tuple of %d vectors",
		       actual, argno + 1, fndecl, num_vectors);
	    else
	      /* num_vectors is always != 1, so the singular isn't needed.  */
	      error_n (location, num_vectors, "%qT%d%qE%d",
		       "passing %qT to argument %d of %qE, which"
		       " expects a tuple of %d vectors", actual, argno + 1,
		       fndecl, num_vectors);
	    return NUM_TYPE_SUFFIXES;
	  }
      }

  if (num_vectors == 1)
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects an MVE vector type", actual, argno + 1, fndecl);
  else
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects an MVE tuple type", actual, argno + 1, fndecl);
  return NUM_TYPE_SUFFIXES;
}

/* Require argument ARGNO to have some form of vector type.  Return the
   associated type suffix on success, using TYPE_SUFFIX_b for predicates.
   Report an error and return NUM_TYPE_SUFFIXES on failure.  */
type_suffix_index
function_resolver::infer_vector_type (unsigned int argno)
{
  return infer_vector_or_tuple_type (argno, 1);
}

/* Require argument ARGNO to be a vector or scalar argument.  Return true
   if it is, otherwise report an appropriate error.  */
bool
function_resolver::require_vector_or_scalar_type (unsigned int argno)
{
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return false;

  if (!scalar_argument_p (argno) && !VECTOR_TYPE_P (actual))
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector or scalar type", actual, argno + 1, fndecl);
      return false;
    }

  return true;
}

/* Require argument ARGNO to have vector type TYPE, in cases where this
   requirement holds for all uses of the function.  Return true if the
   argument has the right form, otherwise report an appropriate error.  */
bool
function_resolver::require_vector_type (unsigned int argno,
					vector_type_index type)
{
  tree expected = acle_vector_types[0][type];
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return false;

  if (!matches_type_p (expected, actual))
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects %qT", actual, argno + 1, fndecl, expected);
      return false;
    }
  return true;
}

/* Like require_vector_type, but TYPE is inferred from previous arguments
   rather than being a fixed part of the function signature.  This changes
   the nature of the error messages.  */
bool
function_resolver::require_matching_vector_type (unsigned int argno,
						 type_suffix_index type)
{
  type_suffix_index new_type = infer_vector_type (argno);
  if (new_type == NUM_TYPE_SUFFIXES)
    return false;

  if (type != new_type)
    {
      error_at (location, "passing %qT to argument %d of %qE, but"
		" previous arguments had type %qT",
		get_vector_type (new_type), argno + 1, fndecl,
		get_vector_type (type));
      return false;
    }
  return true;
}

/* Require argument ARGNO to be a vector type with the following properties:

   - the type class must be the same as FIRST_TYPE's if EXPECTED_TCLASS
     is SAME_TYPE_CLASS, otherwise it must be EXPECTED_TCLASS itself.

   - the element size must be:

     - the same as FIRST_TYPE's if EXPECTED_BITS == SAME_SIZE
     - half of FIRST_TYPE's if EXPECTED_BITS == HALF_SIZE
     - a quarter of FIRST_TYPE's if EXPECTED_BITS == QUARTER_SIZE
     - EXPECTED_BITS itself otherwise

   Return true if the argument has the required type, otherwise report
   an appropriate error.

   FIRST_ARGNO is the first argument that is known to have type FIRST_TYPE.
   Usually it comes before ARGNO, but sometimes it is more natural to resolve
   arguments out of order.

   If the required properties depend on FIRST_TYPE then both FIRST_ARGNO and
   ARGNO contribute to the resolution process.  If the required properties
   are fixed, only FIRST_ARGNO contributes to the resolution process.

   This function is a bit of a Swiss army knife.  The complication comes
   from trying to give good error messages when FIRST_ARGNO and ARGNO are
   inconsistent, since either of them might be wrong.  */
bool function_resolver::
require_derived_vector_type (unsigned int argno,
			     unsigned int first_argno,
			     type_suffix_index first_type,
			     type_class_index expected_tclass,
			     unsigned int expected_bits)
{
  /* If the type needs to match FIRST_ARGNO exactly, use the preferred
     error message for that case.  The VECTOR_TYPE_P test excludes tuple
     types, which we handle below instead.  */
  bool both_vectors_p = VECTOR_TYPE_P (get_argument_type (first_argno));
  if (both_vectors_p
      && expected_tclass == SAME_TYPE_CLASS
      && expected_bits == SAME_SIZE)
    {
      /* There's no need to resolve this case out of order.  */
      gcc_assert (argno > first_argno);
      return require_matching_vector_type (argno, first_type);
    }

  /* Use FIRST_TYPE to get the expected type class and element size.  */
  type_class_index orig_expected_tclass = expected_tclass;
  if (expected_tclass == NUM_TYPE_CLASSES)
    expected_tclass = type_suffixes[first_type].tclass;

  unsigned int orig_expected_bits = expected_bits;
  if (expected_bits == SAME_SIZE)
    expected_bits = type_suffixes[first_type].element_bits;
  else if (expected_bits == HALF_SIZE)
    expected_bits = type_suffixes[first_type].element_bits / 2;
  else if (expected_bits == QUARTER_SIZE)
    expected_bits = type_suffixes[first_type].element_bits / 4;

  /* If the expected type doesn't depend on FIRST_TYPE at all,
     just check for the fixed choice of vector type.  */
  if (expected_tclass == orig_expected_tclass
      && expected_bits == orig_expected_bits)
    {
      const type_suffix_info &expected_suffix
	= type_suffixes[find_type_suffix (expected_tclass, expected_bits)];
      return require_vector_type (argno, expected_suffix.vector_type);
    }

  /* Require the argument to be some form of MVE vector type,
     without being specific about the type of vector we want.  */
  type_suffix_index actual_type = infer_vector_type (argno);
  if (actual_type == NUM_TYPE_SUFFIXES)
    return false;

  /* Exit now if we got the right type.  */
  bool tclass_ok_p = (type_suffixes[actual_type].tclass == expected_tclass);
  bool size_ok_p = (type_suffixes[actual_type].element_bits == expected_bits);
  if (tclass_ok_p && size_ok_p)
    return true;

  /* First look for cases in which the actual type contravenes a fixed
     size requirement, without having to refer to FIRST_TYPE.  */
  if (!size_ok_p && expected_bits == orig_expected_bits)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector of %d-bit elements",
		get_vector_type (actual_type), argno + 1, fndecl,
		expected_bits);
      return false;
    }

  /* Likewise for a fixed type class requirement.  This is only ever
     needed for signed and unsigned types, so don't create unnecessary
     translation work for other type classes.  */
  if (!tclass_ok_p && orig_expected_tclass == TYPE_signed)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector of signed integers",
		get_vector_type (actual_type), argno + 1, fndecl);
      return false;
    }
  if (!tclass_ok_p && orig_expected_tclass == TYPE_unsigned)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector of unsigned integers",
		get_vector_type (actual_type), argno + 1, fndecl);
      return false;
    }

  /* Make sure that FIRST_TYPE itself is sensible before using it
     as a basis for an error message.  */
  if (resolve_to (mode_suffix_id, first_type) == error_mark_node)
    return false;

  /* If the arguments have consistent type classes, but a link between
     the sizes has been broken, try to describe the error in those terms.  */
  if (both_vectors_p && tclass_ok_p && orig_expected_bits == SAME_SIZE)
    {
      if (argno < first_argno)
	{
	  std::swap (argno, first_argno);
	  std::swap (actual_type, first_type);
	}
      error_at (location, "arguments %d and %d of %qE must have the"
		" same element size, but the values passed here have type"
		" %qT and %qT respectively", first_argno + 1, argno + 1,
		fndecl, get_vector_type (first_type),
		get_vector_type (actual_type));
      return false;
    }

  /* Likewise in reverse: look for cases in which the sizes are consistent
     but a link between the type classes has been broken.  */
  if (both_vectors_p
      && size_ok_p
      && orig_expected_tclass == SAME_TYPE_CLASS
      && type_suffixes[first_type].integer_p
      && type_suffixes[actual_type].integer_p)
    {
      if (argno < first_argno)
	{
	  std::swap (argno, first_argno);
	  std::swap (actual_type, first_type);
	}
      error_at (location, "arguments %d and %d of %qE must have the"
		" same signedness, but the values passed here have type"
		" %qT and %qT respectively", first_argno + 1, argno + 1,
		fndecl, get_vector_type (first_type),
		get_vector_type (actual_type));
      return false;
    }

  /* The two arguments are wildly inconsistent.  */
  type_suffix_index expected_type
    = find_type_suffix (expected_tclass, expected_bits);
  error_at (location, "passing %qT instead of the expected %qT to argument"
	    " %d of %qE, after passing %qT to argument %d",
	    get_vector_type (actual_type), get_vector_type (expected_type),
	    argno + 1, fndecl, get_argument_type (first_argno),
	    first_argno + 1);
  return false;
}

/* Require argument ARGNO to be a (possibly variable) scalar, expecting it
   to have the following properties:

   - the type class must be the same as for type suffix 0 if EXPECTED_TCLASS
     is SAME_TYPE_CLASS, otherwise it must be EXPECTED_TCLASS itself.

   - the element size must be the same as for type suffix 0 if EXPECTED_BITS
     is SAME_TYPE_SIZE, otherwise it must be EXPECTED_BITS itself.

   Return true if the argument is valid, otherwise report an appropriate error.

   Note that we don't check whether the scalar type actually has the required
   properties, since that's subject to implicit promotions and conversions.
   Instead we just use the expected properties to tune the error message.  */
bool function_resolver::
require_derived_scalar_type (unsigned int argno,
			     type_class_index expected_tclass,
			     unsigned int expected_bits)
{
  gcc_assert (expected_tclass == SAME_TYPE_CLASS
	      || expected_tclass == TYPE_signed
	      || expected_tclass == TYPE_unsigned);

  /* If the expected type doesn't depend on the type suffix at all,
     just check for the fixed choice of scalar type.  */
  if (expected_tclass != SAME_TYPE_CLASS && expected_bits != SAME_SIZE)
    {
      type_suffix_index expected_type
	= find_type_suffix (expected_tclass, expected_bits);
      return require_scalar_type (argno, get_scalar_type_name (expected_type));
    }

  if (scalar_argument_p (argno))
    return true;

  if (expected_tclass == SAME_TYPE_CLASS)
    /* It doesn't really matter whether the element is expected to be
       the same size as type suffix 0.  */
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects a scalar element", get_argument_type (argno),
	      argno + 1, fndecl);
  else
    /* It doesn't seem useful to distinguish between signed and unsigned
       scalars here.  */
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects a scalar integer", get_argument_type (argno),
	      argno + 1, fndecl);
  return false;
}

/* Require argument ARGNO to be suitable for an integer constant expression.
   Return true if it is, otherwise report an appropriate error.

   function_checker checks whether the argument is actually constant and
   has a suitable range.  The reason for distinguishing immediate arguments
   here is because it provides more consistent error messages than
   require_scalar_type would.  */
bool
function_resolver::require_integer_immediate (unsigned int argno)
{
  if (!scalar_argument_p (argno))
    {
      report_non_ice (location, fndecl, argno);
      return false;
    }
  return true;
}

/* Require argument ARGNO to be a (possibly variable) scalar, using EXPECTED
   as the name of its expected type.  Return true if the argument has the
   right form, otherwise report an appropriate error.  */
bool
function_resolver::require_scalar_type (unsigned int argno,
					const char *expected)
{
  if (!scalar_argument_p (argno))
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects %qs", get_argument_type (argno), argno + 1,
		fndecl, expected);
      return false;
    }
  return true;
}

/* Require argument ARGNO to be some form of pointer, without being specific
   about its target type.  Return true if the argument has the right form,
   otherwise report an appropriate error.  */
bool
function_resolver::require_pointer_type (unsigned int argno)
{
  if (!scalar_argument_p (argno))
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a scalar pointer", get_argument_type (argno),
		argno + 1, fndecl);
      return false;
    }
  return true;
}

/* Require the function to have exactly EXPECTED arguments.  Return true
   if it does, otherwise report an appropriate error.  */
bool
function_resolver::check_num_arguments (unsigned int expected)
{
  if (m_arglist.length () < expected)
    error_at (location, "too few arguments to function %qE", fndecl);
  else if (m_arglist.length () > expected)
    error_at (location, "too many arguments to function %qE", fndecl);
  return m_arglist.length () == expected;
}

/* If the function is predicated, check that the last argument is a
   suitable predicate.  Also check that there are NOPS further
   arguments before any predicate, but don't check what they are.

   Return true on success, otherwise report a suitable error.
   When returning true:

   - set I to the number of the last unchecked argument.
   - set NARGS to the total number of arguments.  */
bool
function_resolver::check_gp_argument (unsigned int nops,
				      unsigned int &i, unsigned int &nargs)
{
  i = nops - 1;
  if (pred != PRED_none)
    {
      switch (pred)
	{
	case PRED_m:
	  /* Add first inactive argument if needed, and final predicate.  */
	  if (has_inactive_argument ())
	    nargs = nops + 2;
	  else
	    nargs = nops + 1;
	  break;

	case PRED_p:
	case PRED_x:
	case PRED_z:
	  /* Add final predicate.  */
	  nargs = nops + 1;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (!check_num_arguments (nargs)
	  || !require_vector_type (nargs - 1, VECTOR_TYPE_mve_pred16_t))
	return false;

      i = nargs - 2;
    }
  else
    {
      nargs = nops;
      if (!check_num_arguments (nargs))
	return false;
    }

  return true;
}

/* Finish resolving a function whose final argument can be a vector
   or a scalar, with the function having an implicit "_n" suffix
   in the latter case.  This "_n" form might only exist for certain
   type suffixes.

   ARGNO is the index of the final argument.  The inferred type suffix
   was obtained from argument FIRST_ARGNO, which has type FIRST_TYPE.
   EXPECTED_TCLASS and EXPECTED_BITS describe the expected properties
   of the final vector or scalar argument, in the same way as for
   require_derived_vector_type.  INFERRED_TYPE is the inferred type
   suffix itself, or NUM_TYPE_SUFFIXES if it's the same as FIRST_TYPE.

   Return the function decl of the resolved function on success,
   otherwise report a suitable error and return error_mark_node.  */
tree function_resolver::
finish_opt_n_resolution (unsigned int argno, unsigned int first_argno,
			 type_suffix_index first_type,
			 type_class_index expected_tclass,
			 unsigned int expected_bits,
			 type_suffix_index inferred_type)
{
  if (inferred_type == NUM_TYPE_SUFFIXES)
    inferred_type = first_type;
  mode_suffix_index scalar_mode = MODE_n;
  if (mode_suffix_id == MODE_r)
    scalar_mode = MODE_r;
  tree scalar_form = lookup_form (scalar_mode, inferred_type);

  /* Allow the final argument to be scalar, if an _n form exists.  */
  if (scalar_argument_p (argno))
    {
      if (scalar_form)
	return scalar_form;

      /* Check the vector form normally.  If that succeeds, raise an
	 error about having no corresponding _n form.  */
      tree res = resolve_to (mode_suffix_id, inferred_type);
      if (res != error_mark_node)
	error_at (location, "passing %qT to argument %d of %qE, but its"
		  " %qT form does not accept scalars",
		  get_argument_type (argno), argno + 1, fndecl,
		  get_vector_type (first_type));
      return error_mark_node;
    }

  /* If an _n form does exist, provide a more accurate message than
     require_derived_vector_type would for arguments that are neither
     vectors nor scalars.  */
  if (scalar_form && !require_vector_or_scalar_type (argno))
    return error_mark_node;

  /* Check for the correct vector type.  */
  if (!require_derived_vector_type (argno, first_argno, first_type,
				    expected_tclass, expected_bits))
    return error_mark_node;

  return resolve_to (mode_suffix_id, inferred_type);
}

/* Resolve a (possibly predicated) unary function.  If the function uses
   merge predication or if TREAT_AS_MERGE_P is true, there is an extra
   vector argument before the governing predicate that specifies the
   values of inactive elements.  This argument has the following
   properties:

   - the type class must be the same as for active elements if MERGE_TCLASS
     is SAME_TYPE_CLASS, otherwise it must be MERGE_TCLASS itself.

   - the element size must be the same as for active elements if MERGE_BITS
     is SAME_TYPE_SIZE, otherwise it must be MERGE_BITS itself.

   Return the function decl of the resolved function on success,
   otherwise report a suitable error and return error_mark_node.  */
tree
function_resolver::resolve_unary (type_class_index merge_tclass,
				  unsigned int merge_bits,
				  bool treat_as_merge_p)
{
  type_suffix_index type;
  if (pred == PRED_m || treat_as_merge_p)
    {
      if (!check_num_arguments (3))
	return error_mark_node;
      if (merge_tclass == SAME_TYPE_CLASS && merge_bits == SAME_SIZE)
	{
	  /* The inactive elements are the same as the active elements,
	     so we can use normal left-to-right resolution.  */
	  if ((type = infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	      /* Predicates are the last argument.  */
	      || !require_vector_type (2 , VECTOR_TYPE_mve_pred16_t)
	      || !require_matching_vector_type (1 , type))
	    return error_mark_node;
	}
      else
	{
	  /* The inactive element type is a function of the active one,
	     so resolve the active one first.  */
	  if (!require_vector_type (1, VECTOR_TYPE_mve_pred16_t)
	      || (type = infer_vector_type (2)) == NUM_TYPE_SUFFIXES
	      || !require_derived_vector_type (0, 2, type, merge_tclass,
					       merge_bits))
	    return error_mark_node;
	}
    }
  else
    {
      /* We just need to check the predicate (if any) and the single
	 vector argument.  */
      unsigned int i, nargs;
      if (!check_gp_argument (1, i, nargs)
	  || (type = infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
	return error_mark_node;
    }

  /* Handle convert-like functions in which the first type suffix is
     explicit.  */
  if (type_suffix_ids[0] != NUM_TYPE_SUFFIXES)
    return resolve_to (mode_suffix_id, type_suffix_ids[0], type);

  return resolve_to (mode_suffix_id, type);
}

/* Resolve a (possibly predicated) unary function taking a scalar
   argument (_n suffix).  If the function uses merge predication,
   there is an extra vector argument in the first position, and the
   final governing predicate that specifies the values of inactive
   elements.

   Return the function decl of the resolved function on success,
   otherwise report a suitable error and return error_mark_node.  */
tree
function_resolver::resolve_unary_n ()
{
  type_suffix_index type;

  /* Currently only support overrides for _m (vdupq).  */
  if (pred != PRED_m)
    return error_mark_node;

  if (pred == PRED_m)
    {
      if (!check_num_arguments (3))
	return error_mark_node;

      /* The inactive elements are the same as the active elements,
	 so we can use normal left-to-right resolution.  */
      if ((type = infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	  /* Predicates are the last argument.  */
	  || !require_vector_type (2 , VECTOR_TYPE_mve_pred16_t))
	return error_mark_node;
    }

  /* Make sure the argument is scalar.  */
  tree scalar_form = lookup_form (MODE_n, type);

  if (scalar_argument_p (1) && scalar_form)
    return scalar_form;

  return error_mark_node;
}

/* Resolve a (possibly predicated) function that takes NOPS like-typed
   vector arguments followed by NIMM integer immediates.  Return the
   function decl of the resolved function on success, otherwise report
   a suitable error and return error_mark_node.  */
tree
function_resolver::resolve_uniform (unsigned int nops, unsigned int nimm)
{
  unsigned int i, nargs;
  type_suffix_index type;
  if (!check_gp_argument (nops + nimm, i, nargs)
      || (type = infer_vector_type (0 )) == NUM_TYPE_SUFFIXES)
    return error_mark_node;

  unsigned int last_arg = i + 1 - nimm;
  for (i = 0; i < last_arg; i++)
    if (!require_matching_vector_type (i, type))
      return error_mark_node;

  for (i = last_arg; i < nargs; ++i)
    if (!require_integer_immediate (i))
      return error_mark_node;

  return resolve_to (mode_suffix_id, type);
}

/* Resolve a (possibly predicated) function that offers a choice between
   taking:

   - NOPS like-typed vector arguments or
   - NOPS - 1 like-typed vector arguments followed by a scalar argument

   Return the function decl of the resolved function on success,
   otherwise report a suitable error and return error_mark_node.  */
tree
function_resolver::resolve_uniform_opt_n (unsigned int nops)
{
  unsigned int i, nargs;
  type_suffix_index type;
  if (!check_gp_argument (nops, i, nargs)
      /* Unary operators should use resolve_unary, so using i - 1 is
	 safe.  */
      || (type = infer_vector_type (i - 1)) == NUM_TYPE_SUFFIXES)
    return error_mark_node;

  /* Skip last argument, may be scalar.  */
  unsigned int last_arg = i;
  for (i = 0; i < last_arg; i++)
    if (!require_matching_vector_type (i, type))
      return error_mark_node;

  return finish_opt_n_resolution (last_arg, 0, type);
}

/* If the call is erroneous, report an appropriate error and return
   error_mark_node.  Otherwise, if the function is overloaded, return
   the decl of the non-overloaded function.  Return NULL_TREE otherwise,
   indicating that the call should be processed in the normal way.  */
tree
function_resolver::resolve ()
{
  return shape->resolve (*this);
}

function_checker::function_checker (location_t location,
				    const function_instance &instance,
				    tree fndecl, tree fntype,
				    unsigned int nargs, tree *args)
  : function_call_info (location, instance, fndecl),
    m_fntype (fntype), m_nargs (nargs), m_args (args)
{
  if (instance.has_inactive_argument ())
    m_base_arg = 1;
  else
    m_base_arg = 0;
}

/* Return true if argument ARGNO exists. which it might not for
   erroneous calls.  It is safe to wave through checks if this
   function returns false.  */
bool
function_checker::argument_exists_p (unsigned int argno)
{
  gcc_assert (argno < (unsigned int) type_num_arguments (m_fntype));
  return argno < m_nargs;
}

/* Check that argument ARGNO is an integer constant expression and
   store its value in VALUE_OUT if so.  The caller should first
   check that argument ARGNO exists.  */
bool
function_checker::require_immediate (unsigned int argno,
				     HOST_WIDE_INT &value_out)
{
  gcc_assert (argno < m_nargs);
  tree arg = m_args[argno];

  /* The type and range are unsigned, so read the argument as an
     unsigned rather than signed HWI.  */
  if (!tree_fits_uhwi_p (arg))
    {
      report_non_ice (location, fndecl, argno);
      return false;
    }

  /* ...but treat VALUE_OUT as signed for error reporting, since printing
     -1 is more user-friendly than the maximum uint64_t value.  */
  value_out = tree_to_uhwi (arg);
  return true;
}

/* Check that argument REL_ARGNO is an integer constant expression that has
   a valid value for enumeration type TYPE.  REL_ARGNO counts from the end
   of the predication arguments.  */
bool
function_checker::require_immediate_enum (unsigned int rel_argno, tree type)
{
  unsigned int argno = m_base_arg + rel_argno;
  if (!argument_exists_p (argno))
    return true;

  HOST_WIDE_INT actual;
  if (!require_immediate (argno, actual))
    return false;

  for (tree entry = TYPE_VALUES (type); entry; entry = TREE_CHAIN (entry))
    {
      /* The value is an INTEGER_CST for C and a CONST_DECL wrapper
	 around an INTEGER_CST for C++.  */
      tree value = TREE_VALUE (entry);
      if (TREE_CODE (value) == CONST_DECL)
	value = DECL_INITIAL (value);
      if (wi::to_widest (value) == actual)
	return true;
    }

  report_not_enum (location, fndecl, argno, actual, type);
  return false;
}

/* Check that argument REL_ARGNO is an integer constant expression that
   has one of the given values.  */
bool
function_checker::require_immediate_one_of (unsigned int rel_argno,
					    HOST_WIDE_INT value0,
					    HOST_WIDE_INT value1,
					    HOST_WIDE_INT value2,
					    HOST_WIDE_INT value3)
{
  unsigned int argno = m_base_arg + rel_argno;
  if (!argument_exists_p (argno))
    return true;

  HOST_WIDE_INT actual;
  if (!require_immediate (argno, actual))
    return false;

  if (actual != value0
      && actual != value1
      && actual != value2
      && actual != value3)
    {
      report_not_one_of (location, fndecl, argno, actual,
			 value0, value1, value2, value3);
      return false;
    }

  return true;
}

/* Check that argument REL_ARGNO is an integer constant expression in the
   range [MIN, MAX].  REL_ARGNO counts from the end of the predication
   arguments.  */
bool
function_checker::require_immediate_range (unsigned int rel_argno,
					   HOST_WIDE_INT min,
					   HOST_WIDE_INT max)
{
  unsigned int argno = m_base_arg + rel_argno;
  if (!argument_exists_p (argno))
    return true;

  /* Required because of the tree_to_uhwi -> HOST_WIDE_INT conversion
     in require_immediate.  */
  gcc_assert (min >= 0 && min <= max);
  HOST_WIDE_INT actual;
  if (!require_immediate (argno, actual))
    return false;

  if (!IN_RANGE (actual, min, max))
    {
      report_out_of_range (location, fndecl, argno, actual, min, max);
      return false;
    }

  return true;
}

/* Perform semantic checks on the call.  Return true if the call is valid,
   otherwise report a suitable error.  */
bool
function_checker::check ()
{
  function_args_iterator iter;
  tree type;
  unsigned int i = 0;
  FOREACH_FUNCTION_ARGS (m_fntype, type, iter)
    {
      if (type == void_type_node || i >= m_nargs)
	break;

      if (i >= m_base_arg
	  && TREE_CODE (type) == ENUMERAL_TYPE
	  && !require_immediate_enum (i - m_base_arg, type))
	return false;

      i += 1;
    }

  return shape->check (*this);
}

gimple_folder::gimple_folder (const function_instance &instance, tree fndecl,
			      gcall *call_in)
  : function_call_info (gimple_location (call_in), instance, fndecl),
    call (call_in), lhs (gimple_call_lhs (call_in))
{
}

/* Try to fold the call.  Return the new statement on success and null
   on failure.  */
gimple *
gimple_folder::fold ()
{
  /* Don't fold anything when MVE is disabled; emit an error during
     expansion instead.  */
  if (!TARGET_HAVE_MVE)
    return NULL;

  /* Punt if the function has a return type and no result location is
     provided.  The attributes should allow target-independent code to
     remove the calls if appropriate.  */
  if (!lhs && TREE_TYPE (gimple_call_fntype (call)) != void_type_node)
    return NULL;

  return base->fold (*this);
}

function_expander::function_expander (const function_instance &instance,
				      tree fndecl, tree call_expr_in,
				      rtx possible_target_in)
  : function_call_info (EXPR_LOCATION (call_expr_in), instance, fndecl),
    call_expr (call_expr_in), possible_target (possible_target_in)
{
}

/* Return the handler of direct optab OP for type suffix SUFFIX_I.  */
insn_code
function_expander::direct_optab_handler (optab op, unsigned int suffix_i)
{
  return ::direct_optab_handler (op, vector_mode (suffix_i));
}

/* Return the base address for a contiguous load or store
   function.  */
rtx
function_expander::get_contiguous_base ()
{
  return args[0];
}

/* For a function that does the equivalent of:

     OUTPUT = COND ? FN (INPUTS) : FALLBACK;

   return the value of FALLBACK.

   MODE is the mode of OUTPUT.
   MERGE_ARGNO is the argument that provides FALLBACK for _m functions,
   or DEFAULT_MERGE_ARGNO if we should apply the usual rules.

   ARGNO is the caller's index into args.  If the returned value is
   argument 0 (as for unary _m operations), increment ARGNO past the
   returned argument.  */
rtx
function_expander::get_fallback_value (machine_mode mode,
				       unsigned int merge_argno,
				       unsigned int &argno)
{
  if (pred == PRED_z)
    return CONST0_RTX (mode);

  gcc_assert (pred == PRED_m || pred == PRED_x);

  if (merge_argno == 0)
    return args[argno++];

  return args[merge_argno];
}

/* Return a REG rtx that can be used for the result of the function,
   using the preferred target if suitable.  */
rtx
function_expander::get_reg_target ()
{
  machine_mode target_mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (fndecl)));
  if (!possible_target || GET_MODE (possible_target) != target_mode)
    possible_target = gen_reg_rtx (target_mode);
  return possible_target;
}

/* Add an output operand to the instruction we're building, which has
   code ICODE.  Bind the output to the preferred target rtx if possible.  */
void
function_expander::add_output_operand (insn_code icode)
{
  unsigned int opno = m_ops.length ();
  machine_mode mode = insn_data[icode].operand[opno].mode;
  m_ops.safe_grow (opno + 1, true);
  create_output_operand (&m_ops.last (), possible_target, mode);
}

/* Add an input operand to the instruction we're building, which has
   code ICODE.  Calculate the value of the operand as follows:

   - If the operand is a predicate, coerce X to have the
     mode that the instruction expects.

   - Otherwise use X directly.  The expand machinery checks that X has
     the right mode for the instruction.  */
void
function_expander::add_input_operand (insn_code icode, rtx x)
{
  unsigned int opno = m_ops.length ();
  const insn_operand_data &operand = insn_data[icode].operand[opno];
  machine_mode mode = operand.mode;
  if (mode == VOIDmode)
    {
      /* The only allowable use of VOIDmode is the wildcard
	 arm_any_register_operand, which is used to avoid
	 combinatorial explosion in the reinterpret patterns.  */
      gcc_assert (operand.predicate == arm_any_register_operand);
      mode = GET_MODE (x);
    }
  else if (VALID_MVE_PRED_MODE (mode))
    x = gen_lowpart (mode, x);

  m_ops.safe_grow (m_ops.length () + 1, true);
  create_input_operand (&m_ops.last (), x, mode);
}

/* Add an integer operand with value X to the instruction.  */
void
function_expander::add_integer_operand (HOST_WIDE_INT x)
{
  m_ops.safe_grow (m_ops.length () + 1, true);
  create_integer_operand (&m_ops.last (), x);
}

/* Add a memory operand with mode MODE and address ADDR.  */
void
function_expander::add_mem_operand (machine_mode mode, rtx addr)
{
  gcc_assert (VECTOR_MODE_P (mode));
  rtx mem = gen_rtx_MEM (mode, memory_address (mode, addr));
  /* The memory is only guaranteed to be element-aligned.  */
  set_mem_align (mem, GET_MODE_ALIGNMENT (GET_MODE_INNER (mode)));
  add_fixed_operand (mem);
}

/* Add an operand that must be X.  The only way of legitimizing an
   invalid X is to reload the address of a MEM.  */
void
function_expander::add_fixed_operand (rtx x)
{
  m_ops.safe_grow (m_ops.length () + 1, true);
  create_fixed_operand (&m_ops.last (), x);
}

/* Generate instruction ICODE, given that its operands have already
   been added to M_OPS.  Return the value of the first operand.  */
rtx
function_expander::generate_insn (insn_code icode)
{
  expand_insn (icode, m_ops.length (), m_ops.address ());
  return function_returns_void_p () ? const0_rtx : m_ops[0].value;
}

/* Implement the call using instruction ICODE, with a 1:1 mapping between
   arguments and input operands.  */
rtx
function_expander::use_exact_insn (insn_code icode)
{
  unsigned int nops = insn_data[icode].n_operands;
  if (!function_returns_void_p ())
    {
      add_output_operand (icode);
      nops -= 1;
    }
  for (unsigned int i = 0; i < nops; ++i)
    add_input_operand (icode, args[i]);
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which does not use a
   predicate.  */
rtx
function_expander::use_unpred_insn (insn_code icode)
{
  gcc_assert (pred == PRED_none);
  /* Discount the output operand.  */
  unsigned int nops = insn_data[icode].n_operands - 1;
  unsigned int i = 0;

  add_output_operand (icode);
  for (; i < nops; ++i)
    add_input_operand (icode, args[i]);

  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which is a predicated
   operation that returns arbitrary values for inactive lanes.  */
rtx
function_expander::use_pred_x_insn (insn_code icode)
{
  gcc_assert (pred == PRED_x);
  unsigned int nops = args.length ();

  add_output_operand (icode);
  /* Use first operand as arbitrary inactive input.  */
  add_input_operand (icode, possible_target);
  emit_clobber (possible_target);
  /* Copy remaining arguments, including the final predicate.  */
  for (unsigned int i = 0; i < nops; ++i)
      add_input_operand (icode, args[i]);

  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which does the equivalent of:

     OUTPUT = COND ? FN (INPUTS) : FALLBACK;

   The instruction operands are in the order above: OUTPUT, COND, INPUTS
   and FALLBACK.  MERGE_ARGNO is the argument that provides FALLBACK for _m
   functions, or DEFAULT_MERGE_ARGNO if we should apply the usual rules.  */
rtx
function_expander::use_cond_insn (insn_code icode, unsigned int merge_argno)
{
  /* At present we never need to handle PRED_none, which would involve
     creating a new predicate rather than using one supplied by the user.  */
  gcc_assert (pred != PRED_none);
  /* For MVE, we only handle PRED_m at present.  */
  gcc_assert (pred == PRED_m);

  /* Discount the output, predicate and fallback value.  */
  unsigned int nops = insn_data[icode].n_operands - 3;
  machine_mode mode = insn_data[icode].operand[0].mode;

  unsigned int opno = 0;
  rtx fallback_arg = NULL_RTX;
  fallback_arg = get_fallback_value (mode, merge_argno, opno);
  rtx pred_arg = args[nops + 1];

  add_output_operand (icode);
    add_input_operand (icode, fallback_arg);
  for (unsigned int i = 0; i < nops; ++i)
    add_input_operand (icode, args[opno + i]);
  add_input_operand (icode, pred_arg);
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which loads memory operand 1
   into register operand 0.  */
rtx
function_expander::use_contiguous_load_insn (insn_code icode)
{
  machine_mode mem_mode = memory_vector_mode ();

  add_output_operand (icode);
  add_mem_operand (mem_mode, get_contiguous_base ());
  if (pred == PRED_z)
    add_input_operand (icode, args[1]);
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which stores register operand 1
   into memory operand 0.  */
rtx
function_expander::use_contiguous_store_insn (insn_code icode)
{
  machine_mode mem_mode = memory_vector_mode ();

  add_mem_operand (mem_mode, get_contiguous_base ());
  add_input_operand (icode, args[1]);
  if (pred == PRED_p)
    add_input_operand (icode, args[2]);
  return generate_insn (icode);
}

/* Implement the call using a normal unpredicated optab for PRED_none.

   <optab> corresponds to:

   - CODE_FOR_SINT for signed integers
   - CODE_FOR_UINT for unsigned integers
   - CODE_FOR_FP for floating-point values  */
rtx
function_expander::map_to_rtx_codes (rtx_code code_for_sint,
				     rtx_code code_for_uint,
				     rtx_code code_for_fp)
{
  gcc_assert (pred == PRED_none);
  rtx_code code = type_suffix (0).integer_p ?
    (type_suffix (0).unsigned_p ? code_for_uint : code_for_sint)
    : code_for_fp;
  insn_code icode = direct_optab_handler (code_to_optab (code), 0);
  if (icode == CODE_FOR_nothing)
    gcc_unreachable ();

  return use_unpred_insn (icode);
}

/* Expand the call and return its lhs.  */
rtx
function_expander::expand ()
{
  unsigned int nargs = call_expr_nargs (call_expr);
  args.reserve (nargs);
  for (unsigned int i = 0; i < nargs; ++i)
    args.quick_push (expand_normal (CALL_EXPR_ARG (call_expr, i)));

  return base->expand (*this);
}

/* If we're implementing manual overloading, check whether the MVE
   function with subcode CODE is overloaded, and if so attempt to
   determine the corresponding non-overloaded function.  The call
   occurs at location LOCATION and has the arguments given by ARGLIST.

   If the call is erroneous, report an appropriate error and return
   error_mark_node.  Otherwise, if the function is overloaded, return
   the decl of the non-overloaded function.  Return NULL_TREE otherwise,
   indicating that the call should be processed in the normal way.  */
tree
resolve_overloaded_builtin (location_t location, unsigned int code,
			    vec<tree, va_gc> *arglist)
{
  if (code >= vec_safe_length (registered_functions))
    return NULL_TREE;

  registered_function &rfn = *(*registered_functions)[code];
  if (rfn.overloaded_p)
    return function_resolver (location, rfn.instance, rfn.decl,
			      *arglist).resolve ();
  return NULL_TREE;
}

/* Perform any semantic checks needed for a call to the MVE function
   with subcode CODE, such as testing for integer constant expressions.
   The call occurs at location LOCATION and has NARGS arguments,
   given by ARGS.  FNDECL is the original function decl, before
   overload resolution.

   Return true if the call is valid, otherwise report a suitable error.  */
bool
check_builtin_call (location_t location, vec<location_t>, unsigned int code,
		    tree fndecl, unsigned int nargs, tree *args)
{
  const registered_function &rfn = *(*registered_functions)[code];
  if (!check_requires_float (location, rfn.decl, rfn.requires_float))
    return false;

  return function_checker (location, rfn.instance, fndecl,
			   TREE_TYPE (rfn.decl), nargs, args).check ();
}

/* Attempt to fold STMT, given that it's a call to the MVE function
   with subcode CODE.  Return the new statement on success and null
   on failure.  Insert any other new statements at GSI.  */
gimple *
gimple_fold_builtin (unsigned int code, gcall *stmt)
{
  registered_function &rfn = *(*registered_functions)[code];
  return gimple_folder (rfn.instance, rfn.decl, stmt).fold ();
}

/* Expand a call to the MVE function with subcode CODE.  EXP is the call
   expression and TARGET is the preferred location for the result.
   Return the value of the lhs.  */
rtx
expand_builtin (unsigned int code, tree exp, rtx target)
{
  registered_function &rfn = *(*registered_functions)[code];
  if (!check_requires_float (EXPR_LOCATION (exp), rfn.decl,
			    rfn.requires_float))
    return target;
  return function_expander (rfn.instance, rfn.decl, exp, target).expand ();
}

} /* end namespace arm_mve */

using namespace arm_mve;

inline void
gt_ggc_mx (function_instance *)
{
}

inline void
gt_pch_nx (function_instance *)
{
}

inline void
gt_pch_nx (function_instance *, gt_pointer_operator, void *)
{
}

#include "gt-arm-mve-builtins.h"
