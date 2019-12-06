/* ACLE support for AArch64 SVE
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

#define IN_TARGET_CODE 1

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
#include "diagnostic.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "explow.h"
#include "emit-rtl.h"
#include "tree-vector-builder.h"
#include "stor-layout.h"
#include "regs.h"
#include "alias.h"
#include "gimple-fold.h"
#include "langhooks.h"
#include "stringpool.h"
#include "aarch64-sve-builtins.h"
#include "aarch64-sve-builtins-base.h"
#include "aarch64-sve-builtins-shapes.h"

namespace aarch64_sve {

/* Static information about each single-predicate or single-vector
   ABI and ACLE type.  */
struct vector_type_info
{
  /* The name of the type as declared by arm_sve.h.  */
  const char *acle_name;

  /* The name of the type specified in AAPCS64.  The type is always
     available under this name, even when arm_sve.h isn't included.  */
  const char *abi_name;

  /* The C++ mangling of ABI_NAME.  */
  const char *mangled_name;
};

/* Describes a function decl.  */
class GTY(()) registered_function
{
public:
  /* The ACLE function that the decl represents.  */
  function_instance instance GTY ((skip));

  /* The decl itself.  */
  tree decl;

  /* The architecture extensions that the function requires, as a set of
     AARCH64_FL_* flags.  */
  uint64_t required_extensions;

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

/* Information about each single-predicate or single-vector type.  */
static CONSTEXPR const vector_type_info vector_types[] = {
#define DEF_SVE_TYPE(ACLE_NAME, NCHARS, ABI_NAME, SCALAR_TYPE) \
  { #ACLE_NAME, #ABI_NAME, #NCHARS #ABI_NAME },
#include "aarch64-sve-builtins.def"
};

/* The function name suffix associated with each predication type.  */
static const char *const pred_suffixes[NUM_PREDS + 1] = {
  "",
  "",
  "_m",
  "_x",
  "_z",
  ""
};

/* Static information about each mode_suffix_index.  */
CONSTEXPR const mode_suffix_info mode_suffixes[] = {
#define VECTOR_TYPE_none NUM_VECTOR_TYPES
#define DEF_SVE_MODE(NAME, BASE, DISPLACEMENT, UNITS) \
  { "_" #NAME, VECTOR_TYPE_##BASE, VECTOR_TYPE_##DISPLACEMENT, UNITS_##UNITS },
#include "aarch64-sve-builtins.def"
#undef VECTOR_TYPE_none
  { "", NUM_VECTOR_TYPES, NUM_VECTOR_TYPES, UNITS_none }
};

/* Static information about each type_suffix_index.  */
CONSTEXPR const type_suffix_info type_suffixes[NUM_TYPE_SUFFIXES + 1] = {
#define DEF_SVE_TYPE_SUFFIX(NAME, ACLE_TYPE, CLASS, BITS, MODE) \
  { "_" #NAME, \
    VECTOR_TYPE_##ACLE_TYPE, \
    TYPE_##CLASS, \
    BITS, \
    BITS / BITS_PER_UNIT, \
    TYPE_##CLASS == TYPE_signed || TYPE_##CLASS == TYPE_unsigned, \
    TYPE_##CLASS == TYPE_unsigned, \
    TYPE_##CLASS == TYPE_float, \
    TYPE_##CLASS == TYPE_bool, \
    0, \
    MODE },
#include "aarch64-sve-builtins.def"
  { "", NUM_VECTOR_TYPES, TYPE_bool, 0, 0, false, false, false, false,
    0, VOIDmode }
};

/* Define a TYPES_<combination> macro for each combination of type
   suffixes that an ACLE function can have, where <combination> is the
   name used in DEF_SVE_FUNCTION entries.

   Use S (T) for single type suffix T and D (T1, T2) for a pair of type
   suffixes T1 and T2.  Use commas to separate the suffixes.

   Although the order shouldn't matter, the convention is to sort the
   suffixes lexicographically after dividing suffixes into a type
   class ("b", "f", etc.) and a numerical bit count.  */

/* _b8 _b16 _b32 _b64.  */
#define TYPES_all_pred(S, D) \
  S (b8), S (b16), S (b32), S (b64)

/* _f16 _f32 _f64.  */
#define TYPES_all_float(S, D) \
  S (f16), S (f32), S (f64)

/* _s8 _s16 _s32 _s64.  */
#define TYPES_all_signed(S, D) \
  S (s8), S (s16), S (s32), S (s64)

/*     _f16 _f32 _f64
   _s8 _s16 _s32 _s64.  */
#define TYPES_all_float_and_signed(S, D) \
  TYPES_all_float (S, D), TYPES_all_signed (S, D)

/* _u8 _u16 _u32 _u64.  */
#define TYPES_all_unsigned(S, D) \
  S (u8), S (u16), S (u32), S (u64)

/* _s8 _s16 _s32 _s64
   _u8 _u16 _u32 _u64.  */
#define TYPES_all_integer(S, D) \
  TYPES_all_signed (S, D), TYPES_all_unsigned (S, D)

/*     _f16 _f32 _f64
   _s8 _s16 _s32 _s64
   _u8 _u16 _u32 _u64.  */
#define TYPES_all_data(S, D) \
  TYPES_all_float (S, D), TYPES_all_integer (S, D)

/* _b only.  */
#define TYPES_b(S, D) \
  S (b)

/* _s8 _s16 _s32.  */
#define TYPES_bhs_signed(S, D) \
  S (s8), S (s16), S (s32)

/* _u8 _u16 _u32.  */
#define TYPES_bhs_unsigned(S, D) \
  S (u8), S (u16), S (u32)

/* _s8 _s16 _s32
   _u8 _u16 _u32.  */
#define TYPES_bhs_integer(S, D) \
  TYPES_bhs_signed (S, D), TYPES_bhs_unsigned (S, D)

/* _s16
   _u16.  */
#define TYPES_h_integer(S, D) \
  S (s16), S (u16)

/* _f16 _f32.  */
#define TYPES_hs_float(S, D) \
  S (f16), S (f32)

/* _s16 _s32 _s64
   _u16 _u32 _u64.  */
#define TYPES_hsd_integer(S, D) \
  S (s16), S (s32), S (s64), S (u16), S (u32), S (u64)

/* _s32 _u32.  */
#define TYPES_s_integer(S, D) \
  S (s32), S (u32)

/* _s32 _s64
   _u32 _u64.  */
#define TYPES_sd_integer(S, D) \
  S (s32), S (s64), S (u32), S (u64)

/* _f32 _f64
   _s32 _s64
   _u32 _u64.  */
#define TYPES_sd_data(S, D) \
  S (f32), S (f64), TYPES_sd_integer (S, D)

/* _f16 _f32 _f64
	_s32 _s64
	_u32 _u64.  */
#define TYPES_all_float_and_sd_integer(S, D) \
  TYPES_all_float (S, D), TYPES_sd_integer (S, D)

/* _s64
   _u64.  */
#define TYPES_d_integer(S, D) \
  S (s64), S (u64)

/* All the type combinations allowed by svcvt.  */
#define TYPES_cvt(S, D) \
  D (f16, f32), D (f16, f64), \
  D (f16, s16), D (f16, s32), D (f16, s64), \
  D (f16, u16), D (f16, u32), D (f16, u64), \
  \
  D (f32, f16), D (f32, f64), \
  D (f32, s32), D (f32, s64), \
  D (f32, u32), D (f32, u64), \
  \
  D (f64, f16), D (f64, f32), \
  D (f64, s32), D (f64, s64), \
  D (f64, u32), D (f64, u64), \
  \
  D (s16, f16), \
  D (s32, f16), D (s32, f32), D (s32, f64), \
  D (s64, f16), D (s64, f32), D (s64, f64), \
  \
  D (u16, f16), \
  D (u32, f16), D (u32, f32), D (u32, f64), \
  D (u64, f16), D (u64, f32), D (u64, f64)

/* { _s32 _s64 } x { _b8 _b16 _b32 _b64 }
   { _u32 _u64 }.  */
#define TYPES_inc_dec_n1(D, A) \
  D (A, b8), D (A, b16), D (A, b32), D (A, b64)
#define TYPES_inc_dec_n(S, D) \
  TYPES_inc_dec_n1 (D, s32), \
  TYPES_inc_dec_n1 (D, s64), \
  TYPES_inc_dec_n1 (D, u32), \
  TYPES_inc_dec_n1 (D, u64)

/* {     _f16 _f32 _f64 }   {     _f16 _f32 _f64 }
   { _s8 _s16 _s32 _s64 } x { _s8 _s16 _s32 _s64 }
   { _u8 _u16 _u32 _u64 }   { _u8 _u16 _u32 _u64 }.  */
#define TYPES_reinterpret1(D, A) \
  D (A, f16), D (A, f32), D (A, f64), \
  D (A, s8), D (A, s16), D (A, s32), D (A, s64), \
  D (A, u8), D (A, u16), D (A, u32), D (A, u64)
#define TYPES_reinterpret(S, D) \
  TYPES_reinterpret1 (D, f16), \
  TYPES_reinterpret1 (D, f32), \
  TYPES_reinterpret1 (D, f64), \
  TYPES_reinterpret1 (D, s8), \
  TYPES_reinterpret1 (D, s16), \
  TYPES_reinterpret1 (D, s32), \
  TYPES_reinterpret1 (D, s64), \
  TYPES_reinterpret1 (D, u8), \
  TYPES_reinterpret1 (D, u16), \
  TYPES_reinterpret1 (D, u32), \
  TYPES_reinterpret1 (D, u64)

/* { _b8 _b16 _b32 _b64 } x { _s32 _s64 }
			    { _u32 _u64 } */
#define TYPES_while1(D, bn) \
  D (bn, s32), D (bn, s64), D (bn, u32), D (bn, u64)
#define TYPES_while(S, D) \
  TYPES_while1 (D, b8), \
  TYPES_while1 (D, b16), \
  TYPES_while1 (D, b32), \
  TYPES_while1 (D, b64)

/* Describe a pair of type suffixes in which only the first is used.  */
#define DEF_VECTOR_TYPE(X) { TYPE_SUFFIX_ ## X, NUM_TYPE_SUFFIXES }

/* Describe a pair of type suffixes in which both are used.  */
#define DEF_DOUBLE_TYPE(X, Y) { TYPE_SUFFIX_ ## X, TYPE_SUFFIX_ ## Y }

/* Create an array that can be used in aarch64-sve-builtins.def to
   select the type suffixes in TYPES_<NAME>.  */
#define DEF_SVE_TYPES_ARRAY(NAME) \
  static const type_suffix_pair types_##NAME[] = { \
    TYPES_##NAME (DEF_VECTOR_TYPE, DEF_DOUBLE_TYPE), \
    { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES } \
  }

/* For functions that don't take any type suffixes.  */
static const type_suffix_pair types_none[] = {
  { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES },
  { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES }
};

/* Create an array for each TYPES_<combination> macro above.  */
DEF_SVE_TYPES_ARRAY (all_pred);
DEF_SVE_TYPES_ARRAY (all_float);
DEF_SVE_TYPES_ARRAY (all_signed);
DEF_SVE_TYPES_ARRAY (all_float_and_signed);
DEF_SVE_TYPES_ARRAY (all_unsigned);
DEF_SVE_TYPES_ARRAY (all_integer);
DEF_SVE_TYPES_ARRAY (all_data);
DEF_SVE_TYPES_ARRAY (b);
DEF_SVE_TYPES_ARRAY (bhs_signed);
DEF_SVE_TYPES_ARRAY (bhs_unsigned);
DEF_SVE_TYPES_ARRAY (bhs_integer);
DEF_SVE_TYPES_ARRAY (h_integer);
DEF_SVE_TYPES_ARRAY (hs_float);
DEF_SVE_TYPES_ARRAY (hsd_integer);
DEF_SVE_TYPES_ARRAY (s_integer);
DEF_SVE_TYPES_ARRAY (sd_integer);
DEF_SVE_TYPES_ARRAY (sd_data);
DEF_SVE_TYPES_ARRAY (all_float_and_sd_integer);
DEF_SVE_TYPES_ARRAY (d_integer);
DEF_SVE_TYPES_ARRAY (cvt);
DEF_SVE_TYPES_ARRAY (inc_dec_n);
DEF_SVE_TYPES_ARRAY (reinterpret);
DEF_SVE_TYPES_ARRAY (while);

/* Used by functions that have no governing predicate.  */
static const predication_index preds_none[] = { PRED_none, NUM_PREDS };

/* Used by functions that have a governing predicate but do not have an
   explicit suffix.  */
static const predication_index preds_implicit[] = { PRED_implicit, NUM_PREDS };

/* Used by functions that allow merging, zeroing and "don't care"
   predication.  */
static const predication_index preds_mxz[] = {
  PRED_m, PRED_x, PRED_z, NUM_PREDS
};

/* Used by functions that have the mxz predicated forms above, and in addition
   have an unpredicated form.  */
static const predication_index preds_mxz_or_none[] = {
  PRED_m, PRED_x, PRED_z, PRED_none, NUM_PREDS
};

/* Used by functions that allow merging and zeroing predication but have
   no "_x" form.  */
static const predication_index preds_mz[] = { PRED_m, PRED_z, NUM_PREDS };

/* Used by functions that have an unpredicated form and a _z predicated
   form.  */
static const predication_index preds_z_or_none[] = {
  PRED_z, PRED_none, NUM_PREDS
};

/* Used by (mostly predicate) functions that only support "_z" predication.  */
static const predication_index preds_z[] = { PRED_z, NUM_PREDS };

/* A list of all SVE ACLE functions.  */
static CONSTEXPR const function_group_info function_groups[] = {
#define DEF_SVE_FUNCTION(NAME, SHAPE, TYPES, PREDS) \
  { #NAME, &functions::NAME, &shapes::SHAPE, types_##TYPES, preds_##PREDS, \
    REQUIRED_EXTENSIONS | AARCH64_FL_SVE },
#include "aarch64-sve-builtins.def"
};

/* The scalar type associated with each vector type.  */
GTY(()) tree scalar_types[NUM_VECTOR_TYPES];

/* The single-predicate and single-vector types, with their built-in
   "__SV..._t" name.  Allow an index of NUM_VECTOR_TYPES, which always
   yields a null tree.  */
static GTY(()) tree abi_vector_types[NUM_VECTOR_TYPES + 1];

/* Same, but with the arm_sve.h "sv..._t" name.  */
GTY(()) tree acle_vector_types[MAX_TUPLE_SIZE][NUM_VECTOR_TYPES + 1];

/* The svpattern enum type.  */
GTY(()) tree acle_svpattern;

/* The svprfop enum type.  */
GTY(()) tree acle_svprfop;

/* The list of all registered function decls, indexed by code.  */
static GTY(()) vec<registered_function *, va_gc> *registered_functions;

/* All registered function decls, hashed on the function_instance
   that they implement.  This is used for looking up implementations of
   overloaded functions.  */
static hash_table<registered_function_hasher> *function_table;

/* True if we've already complained about attempts to use functions
   when the required extension is disabled.  */
static bool reported_missing_extension_p;

/* If TYPE is an ACLE vector type, return the associated vector_type,
   otherwise return NUM_VECTOR_TYPES.  */
static vector_type_index
find_vector_type (const_tree type)
{
  /* A linear search should be OK here, since the code isn't hot and
     the number of types is only small.  */
  type = TYPE_MAIN_VARIANT (type);
  for (unsigned int i = 0; i < NUM_VECTOR_TYPES; ++i)
    if (type == abi_vector_types[i])
      return vector_type_index (i);
  return NUM_VECTOR_TYPES;
}

/* If TYPE is a valid SVE element type, return the corresponding type
   suffix, otherwise return NUM_TYPE_SUFFIXES.  */
static type_suffix_index
find_type_suffix_for_scalar_type (const_tree type)
{
  /* A linear search should be OK here, since the code isn't hot and
     the number of types is only small.  */
  type = TYPE_MAIN_VARIANT (type);
  for (unsigned int suffix_i = 0; suffix_i < NUM_TYPE_SUFFIXES; ++suffix_i)
    if (!type_suffixes[suffix_i].bool_p)
      {
	vector_type_index vector_i = type_suffixes[suffix_i].vector_type;
	if (type == TYPE_MAIN_VARIANT (scalar_types[vector_i]))
	  return type_suffix_index (suffix_i);
      }
  return NUM_TYPE_SUFFIXES;
}

/* Report an error against LOCATION that the user has tried to use
   function FNDECL when extension EXTENSION is disabled.  */
static void
report_missing_extension (location_t location, tree fndecl,
			  const char *extension)
{
  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_missing_extension_p)
    return;

  error_at (location, "ACLE function %qD requires ISA extension %qs",
	    fndecl, extension);
  inform (location, "you can enable %qs using the command-line"
	  " option %<-march%>, or by using the %<target%>"
	  " attribute or pragma", extension);
  reported_missing_extension_p = true;
}

/* Check whether all the AARCH64_FL_* values in REQUIRED_EXTENSIONS are
   enabled, given that those extensions are required for function FNDECL.
   Report an error against LOCATION if not.  */
static bool
check_required_extensions (location_t location, tree fndecl,
			   uint64_t required_extensions)
{
  uint64_t missing_extensions = required_extensions & ~aarch64_isa_flags;
  if (missing_extensions == 0)
    return true;

  static const struct { uint64_t flag; const char *name; } extensions[] = {
#define AARCH64_OPT_EXTENSION(EXT_NAME, FLAG_CANONICAL, FLAGS_ON, FLAGS_OFF, \
			      SYNTHETIC, FEATURE_STRING) \
    { FLAG_CANONICAL, EXT_NAME },
#include "aarch64-option-extensions.def"
  };

  for (unsigned int i = 0; i < ARRAY_SIZE (extensions); ++i)
    if (missing_extensions & extensions[i].flag)
      {
	report_missing_extension (location, fndecl, extensions[i].name);
	return false;
      }
  gcc_unreachable ();
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
   the value ACTUAL, whereas the function requires either VALUE0 or
   VALUE1.  ARGNO counts from zero.  */
static void
report_neither_nor (location_t location, tree fndecl, unsigned int argno,
		    HOST_WIDE_INT actual, HOST_WIDE_INT value0,
		    HOST_WIDE_INT value1)
{
  error_at (location, "passing %wd to argument %d of %qE, which expects"
	    " either %wd or %wd", actual, argno + 1, fndecl, value0, value1);
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

  /* Handle direct reads of global state.  */
  return flags & (CP_READ_MEMORY | CP_READ_FFR);
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

  /* Treat prefetches as modifying global state, since that's the
     only means we have of keeping them in their correct position.  */
  if (flags & CP_PREFETCH_MEMORY)
    return true;

  /* Handle direct modifications of global state.  */
  return flags & (CP_WRITE_MEMORY | CP_WRITE_FFR);
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

sve_switcher::sve_switcher ()
  : m_old_isa_flags (aarch64_isa_flags)
{
  /* Changing the ISA flags and have_regs_of_mode should be enough here.
     We shouldn't need to pay the compile-time cost of a full target
     switch.  */
  aarch64_isa_flags = (AARCH64_FL_FP | AARCH64_FL_SIMD | AARCH64_FL_F16
		       | AARCH64_FL_SVE);

  memcpy (m_old_have_regs_of_mode, have_regs_of_mode,
	  sizeof (have_regs_of_mode));
  for (int i = 0; i < NUM_MACHINE_MODES; ++i)
    if (aarch64_sve_mode_p ((machine_mode) i))
      have_regs_of_mode[i] = true;
}

sve_switcher::~sve_switcher ()
{
  memcpy (have_regs_of_mode, m_old_have_regs_of_mode,
	  sizeof (have_regs_of_mode));
  aarch64_isa_flags = m_old_isa_flags;
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

/* Return the overloaded or full function name for INSTANCE; OVERLOADED_P
   selects which.  Allocate the string on m_string_obstack; the caller
   must use obstack_free to free it after use.  */
char *
function_builder::get_name (const function_instance &instance,
			    bool overloaded_p)
{
  append_name (instance.base_name);
  if (overloaded_p)
    switch (instance.displacement_units ())
      {
      case UNITS_none:
	break;

      case UNITS_bytes:
	append_name ("_offset");
	break;

      case UNITS_elements:
	append_name ("_index");
	break;

      case UNITS_vectors:
	append_name ("_vnum");
	break;
      }
  else
    append_name (instance.mode_suffix ().string);
  for (unsigned int i = 0; i < 2; ++i)
    if (!overloaded_p || instance.shape->explicit_type_suffix_p (i))
      append_name (instance.type_suffix (i).string);
  append_name (pred_suffixes[instance.pred]);
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
   whether it is overloaded.  REQUIRED_EXTENSIONS are the set of
   architecture extensions that the function requires.  */
registered_function &
function_builder::add_function (const function_instance &instance,
				const char *name, tree fntype, tree attrs,
				uint64_t required_extensions,
				bool overloaded_p)
{
  unsigned int code = vec_safe_length (registered_functions);
  code = (code << AARCH64_BUILTIN_SHIFT) | AARCH64_BUILTIN_SVE;
  tree decl = simulate_builtin_function_decl (input_location, name, fntype,
					      code, NULL, attrs);

  registered_function &rfn = *ggc_alloc <registered_function> ();
  rfn.instance = instance;
  rfn.decl = decl;
  rfn.required_extensions = required_extensions;
  rfn.overloaded_p = overloaded_p;
  vec_safe_push (registered_functions, &rfn);

  return rfn;
}

/* Add a built-in function for INSTANCE, with the argument types given
   by ARGUMENT_TYPES and the return type given by RETURN_TYPE.
   REQUIRED_EXTENSIONS are the set of architecture extensions that the
   function requires.  FORCE_DIRECT_OVERLOADS is true if there is a
   one-to-one mapping between "short" and "full" names, and if standard
   overload resolution therefore isn't necessary.  */
void
function_builder::add_unique_function (const function_instance &instance,
				       tree return_type,
				       vec<tree> &argument_types,
				       uint64_t required_extensions,
				       bool force_direct_overloads)
{
  /* Add the function under its full (unique) name.  */
  char *name = get_name (instance, false);
  tree fntype = build_function_type_array (return_type,
					   argument_types.length (),
					   argument_types.address ());
  tree attrs = get_attributes (instance);
  registered_function &rfn = add_function (instance, name, fntype, attrs,
					   required_extensions, false);

  /* Enter the function into the hash table.  */
  hashval_t hash = instance.hash ();
  registered_function **rfn_slot
    = function_table->find_slot_with_hash (instance, hash, INSERT);
  gcc_assert (!*rfn_slot);
  *rfn_slot = &rfn;

  /* Also add the function under its overloaded alias, if we want
     a separate decl for each instance of an overloaded function.  */
  if (m_direct_overloads || force_direct_overloads)
    {
      char *overload_name = get_name (instance, true);
      if (strcmp (name, overload_name) != 0)
	{
	  /* Attribute lists shouldn't be shared.  */
	  tree attrs = get_attributes (instance);
	  add_function (instance, overload_name, fntype, attrs,
			required_extensions, false);
	}
    }

  obstack_free (&m_string_obstack, name);
}

/* Add one function decl for INSTANCE, to be used with manual overload
   resolution.  REQUIRED_EXTENSIONS are the set of architecture extensions
   that the function requires.

   For simplicity, deal with duplicate attempts to add the same
   function.  */
void
function_builder::add_overloaded_function (const function_instance &instance,
					   uint64_t required_extensions)
{
  char *name = get_name (instance, true);
  if (registered_function **map_value = m_overload_names.get (name))
    gcc_assert ((*map_value)->instance == instance
		&& (*map_value)->required_extensions == required_extensions);
  else
    {
      registered_function &rfn
	= add_function (instance, name, m_overload_type, NULL_TREE,
			required_extensions, true);
      const char *permanent_name = IDENTIFIER_POINTER (DECL_NAME (rfn.decl));
      m_overload_names.put (permanent_name, &rfn);
    }
  obstack_free (&m_string_obstack, name);
}

/* If we are using manual overload resolution, add one function decl
   for each overloaded function in GROUP.  Take the function base name
   from GROUP and the mode from MODE.  */
void
function_builder::add_overloaded_functions (const function_group_info &group,
					    mode_suffix_index mode)
{
  if (m_direct_overloads)
    return;

  unsigned int explicit_type0 = (*group.shape)->explicit_type_suffix_p (0);
  unsigned int explicit_type1 = (*group.shape)->explicit_type_suffix_p (1);
  for (unsigned int pi = 0; group.preds[pi] != NUM_PREDS; ++pi)
    {
      if (!explicit_type0 && !explicit_type1)
	{
	  /* Deal with the common case in which there is one overloaded
	     function for all type combinations.  */
	  function_instance instance (group.base_name, *group.base,
				      *group.shape, mode, types_none[0],
				      group.preds[pi]);
	  add_overloaded_function (instance, group.required_extensions);
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
	    add_overloaded_function (instance, group.required_extensions);
	  }
    }
}

/* Register all the functions in GROUP.  */
void
function_builder::register_function_group (const function_group_info &group)
{
  (*group.shape)->build (*this, group);
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

/* Require argument ARGNO to be a 32-bit or 64-bit scalar integer type.
   Return the associated type suffix on success, otherwise report an
   error and return NUM_TYPE_SUFFIXES.  */
type_suffix_index
function_resolver::infer_integer_scalar_type (unsigned int argno)
{
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return NUM_TYPE_SUFFIXES;

  /* Allow enums and booleans to decay to integers, for compatibility
     with C++ overloading rules.  */
  if (INTEGRAL_TYPE_P (actual))
    {
      bool uns_p = TYPE_UNSIGNED (actual);
      /* Honor the usual integer promotions, so that resolution works
	 in the same way as for C++.  */
      if (TYPE_PRECISION (actual) < 32)
	return TYPE_SUFFIX_s32;
      if (TYPE_PRECISION (actual) == 32)
	return uns_p ? TYPE_SUFFIX_u32 : TYPE_SUFFIX_s32;
      if (TYPE_PRECISION (actual) == 64)
	return uns_p ? TYPE_SUFFIX_u64 : TYPE_SUFFIX_s64;
    }

  error_at (location, "passing %qT to argument %d of %qE, which expects"
	    " a 32-bit or 64-bit integer type", actual, argno + 1, fndecl);
  return NUM_TYPE_SUFFIXES;
}

/* Require argument ARGNO to be a pointer to a scalar type that has a
   corresponding type suffix.  Return that type suffix on success,
   otherwise report an error and return NUM_TYPE_SUFFIXES.
   GATHER_SCATTER_P is true if the function is a gather/scatter
   operation, and so requires a pointer to 32-bit or 64-bit data.  */
type_suffix_index
function_resolver::infer_pointer_type (unsigned int argno,
				       bool gather_scatter_p)
{
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return NUM_TYPE_SUFFIXES;

  if (TREE_CODE (actual) != POINTER_TYPE)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a pointer type", actual, argno + 1, fndecl);
      if (VECTOR_TYPE_P (actual) && gather_scatter_p)
	inform (location, "an explicit type suffix is needed"
		" when using a vector of base addresses");
      return NUM_TYPE_SUFFIXES;
    }

  tree target = TREE_TYPE (actual);
  type_suffix_index type = find_type_suffix_for_scalar_type (target);
  if (type == NUM_TYPE_SUFFIXES)
    {
      error_at (location, "passing %qT to argument %d of %qE, but %qT is not"
		" a valid SVE element type", actual, argno + 1, fndecl,
		target);
      return NUM_TYPE_SUFFIXES;
    }
  unsigned int bits = type_suffixes[type].element_bits;
  if (gather_scatter_p && bits != 32 && bits != 64)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a pointer to 32-bit or 64-bit elements",
		actual, argno + 1, fndecl);
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
	if (type && TYPE_MAIN_VARIANT (actual) == TYPE_MAIN_VARIANT (type))
	  {
	    if (size_i + 1 == num_vectors)
	      return type_suffix_index (suffix_i);

	    if (num_vectors == 1)
	      error_at (location, "passing %qT to argument %d of %qE, which"
			" expects a single SVE vector rather than a tuple",
			actual, argno + 1, fndecl);
	    else if (size_i == 0 && type_i != VECTOR_TYPE_svbool_t)
	      error_at (location, "passing single vector %qT to argument %d"
			" of %qE, which expects a tuple of %d vectors",
			actual, argno + 1, fndecl, num_vectors);
	    else
	      error_at (location, "passing %qT to argument %d of %qE, which"
			" expects a tuple of %d vectors", actual, argno + 1,
			fndecl, num_vectors);
	    return NUM_TYPE_SUFFIXES;
	  }
      }

  if (num_vectors == 1)
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects an SVE vector type", actual, argno + 1, fndecl);
  else
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects an SVE tuple type", actual, argno + 1, fndecl);
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

/* Like infer_vector_type, but also require the type to be integral.  */
type_suffix_index
function_resolver::infer_integer_vector_type (unsigned int argno)
{
  type_suffix_index type = infer_vector_type (argno);
  if (type == NUM_TYPE_SUFFIXES)
    return type;

  if (!type_suffixes[type].integer_p)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector of integers", get_argument_type (argno),
		argno + 1, fndecl);
      return NUM_TYPE_SUFFIXES;
    }

  return type;
}

/* Like infer_vector_type, but also require the type to be an unsigned
   integer.  */
type_suffix_index
function_resolver::infer_unsigned_vector_type (unsigned int argno)
{
  type_suffix_index type = infer_vector_type (argno);
  if (type == NUM_TYPE_SUFFIXES)
    return type;

  if (!type_suffixes[type].unsigned_p)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector of unsigned integers",
		get_argument_type (argno), argno + 1, fndecl);
      return NUM_TYPE_SUFFIXES;
    }

  return type;
}

/* Like infer_vector_type, but also require the element size to be
   32 or 64 bits.  */
type_suffix_index
function_resolver::infer_sd_vector_type (unsigned int argno)
{
  type_suffix_index type = infer_vector_type (argno);
  if (type == NUM_TYPE_SUFFIXES)
    return type;

  unsigned int bits = type_suffixes[type].element_bits;
  if (bits != 32 && bits != 64)
    {
      error_at (location, "passing %qT to argument %d of %qE, which"
		" expects a vector of 32-bit or 64-bit elements",
		get_argument_type (argno), argno + 1, fndecl);
      return NUM_TYPE_SUFFIXES;
    }

  return type;
}

/* If the function operates on tuples of vectors, require argument ARGNO to be
   a tuple with the appropriate number of vectors, otherwise require it to be
   a single vector.  Return the associated type suffix on success, using
   TYPE_SUFFIX_b for predicates.  Report an error and return NUM_TYPE_SUFFIXES
   on failure.  */
type_suffix_index
function_resolver::infer_tuple_type (unsigned int argno)
{
  return infer_vector_or_tuple_type (argno, vectors_per_tuple ());
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
  if (actual != error_mark_node
      && TYPE_MAIN_VARIANT (expected) != TYPE_MAIN_VARIANT (actual))
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

  /* Require the argument to be some form of SVE vector type,
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

/* Argument FIRST_ARGNO is a scalar with type EXPECTED_TYPE, and argument
   ARGNO should be consistent with it.  Return true if it is, otherwise
   report an appropriate error.  */
bool function_resolver::
require_matching_integer_scalar_type (unsigned int argno,
				      unsigned int first_argno,
				      type_suffix_index expected_type)
{
  type_suffix_index actual_type = infer_integer_scalar_type (argno);
  if (actual_type == NUM_TYPE_SUFFIXES)
    return false;

  if (actual_type == expected_type)
    return true;

  error_at (location, "call to %qE is ambiguous; argument %d has type"
	    " %qs but argument %d has type %qs", fndecl,
	    first_argno + 1, get_scalar_type_name (expected_type),
	    argno + 1, get_scalar_type_name (actual_type));
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

/* Require argument ARGNO to be a vector base in a gather-style address.
   Return its type on success, otherwise return NUM_VECTOR_TYPES.  */
vector_type_index
function_resolver::infer_vector_base_type (unsigned int argno)
{
  type_suffix_index type = infer_vector_type (argno);
  if (type == NUM_TYPE_SUFFIXES)
    return NUM_VECTOR_TYPES;

  if (type == TYPE_SUFFIX_u32 || type == TYPE_SUFFIX_u64)
    return type_suffixes[type].vector_type;

  error_at (location, "passing %qT to argument %d of %qE, which"
	    " expects %qs or %qs", get_argument_type (argno),
	    argno + 1, fndecl, "svuint32_t", "svuint64_t");
  return NUM_VECTOR_TYPES;
}

/* Require argument ARGNO to be a vector displacement in a gather-style
   address.  Return its type on success, otherwise return NUM_VECTOR_TYPES.  */
vector_type_index
function_resolver::infer_vector_displacement_type (unsigned int argno)
{
  type_suffix_index type = infer_integer_vector_type (argno);
  if (type == NUM_TYPE_SUFFIXES)
    return NUM_VECTOR_TYPES;

  if (type_suffixes[type].integer_p
      && (type_suffixes[type].element_bits == 32
	  || type_suffixes[type].element_bits == 64))
    return type_suffixes[type].vector_type;

  error_at (location, "passing %qT to argument %d of %qE, which"
	    " expects a vector of 32-bit or 64-bit integers",
	    get_argument_type (argno), argno + 1, fndecl);
  return NUM_VECTOR_TYPES;
}

/* Require argument ARGNO to be a vector displacement in a gather-style
   address.  There are three possible uses:

   - for loading into elements of type TYPE (when LOAD_P is true)
   - for storing from elements of type TYPE (when LOAD_P is false)
   - for prefetching data (when TYPE is NUM_TYPE_SUFFIXES)

   The overloaded function's mode suffix determines the units of the
   displacement (bytes for "_offset", elements for "_index").

   Return the associated mode on success, otherwise report an error
   and return MODE_none.  */
mode_suffix_index
function_resolver::resolve_sv_displacement (unsigned int argno,
					    type_suffix_index type,
					    bool load_p)
{
  if (type == NUM_TYPE_SUFFIXES)
    {
      /* For prefetches, the base is a void pointer and the displacement
	 can be any valid offset or index type.  */
      vector_type_index displacement_vector_type
	= infer_vector_displacement_type (argno);
      if (displacement_vector_type == NUM_VECTOR_TYPES)
	return MODE_none;

      mode_suffix_index mode = find_mode_suffix (NUM_VECTOR_TYPES,
						 displacement_vector_type,
						 displacement_units ());
      gcc_assert (mode != MODE_none);
      return mode;
    }

  /* Check for some form of vector type, without naming any in particular
     as being expected.  */
  type_suffix_index displacement_type = infer_vector_type (argno);
  if (displacement_type == NUM_TYPE_SUFFIXES)
    return MODE_none;

  /* If the displacement type is consistent with the data vector type,
     try to find the associated mode suffix.  This will fall through
     for non-integral displacement types.  */
  unsigned int required_bits = type_suffixes[type].element_bits;
  if (type_suffixes[displacement_type].element_bits == required_bits)
    {
      vector_type_index displacement_vector_type
	= type_suffixes[displacement_type].vector_type;
      mode_suffix_index mode = find_mode_suffix (NUM_VECTOR_TYPES,
						 displacement_vector_type,
						 displacement_units ());
      if (mode != MODE_none)
	return mode;
    }

  if (type_suffix_ids[0] == NUM_TYPE_SUFFIXES)
    {
      /* TYPE has been inferred rather than specified by the user,
	 so mention it in the error messages.  */
      if (load_p)
	error_at (location, "passing %qT to argument %d of %qE, which when"
		  " loading %qT expects a vector of %d-bit integers",
		  get_argument_type (argno), argno + 1, fndecl,
		  get_vector_type (type), required_bits);
      else
	error_at (location, "passing %qT to argument %d of %qE, which when"
		  " storing %qT expects a vector of %d-bit integers",
		  get_argument_type (argno), argno + 1, fndecl,
		  get_vector_type (type), required_bits);
    }
  else
    /* TYPE is part of the function name.  */
    error_at (location, "passing %qT to argument %d of %qE, which"
	      " expects a vector of %d-bit integers",
	      get_argument_type (argno), argno + 1, fndecl, required_bits);
  return MODE_none;
}

/* Require the arguments starting at ARGNO to form a gather-style address.
   There are three possible uses:

   - for loading into elements of type TYPE (when LOAD_P is true)
   - for storing from elements of type TYPE (when LOAD_P is false)
   - for prefetching data (when TYPE is NUM_TYPE_SUFFIXES)

   The three possible addresses are:

   - a vector base with no displacement
   - a vector base and a scalar displacement
   - a scalar (pointer) base and a vector displacement

   The overloaded function's mode suffix determines whether there is
   a displacement, and if so, what units it uses:

   - MODE_none: no displacement
   - MODE_offset: the displacement is measured in bytes
   - MODE_index: the displacement is measured in elements

   Return the mode of the non-overloaded function on success, otherwise
   report an error and return MODE_none.  */
mode_suffix_index
function_resolver::resolve_gather_address (unsigned int argno,
					   type_suffix_index type,
					   bool load_p)
{
  tree actual = get_argument_type (argno);
  if (actual == error_mark_node)
    return MODE_none;

  if (displacement_units () != UNITS_none)
    {
      /* Some form of displacement is needed.  First handle a scalar
	 pointer base and a vector displacement.  */
      if (scalar_argument_p (argno))
	/* Don't check the pointer type here, since there's only one valid
	   choice.  Leave that to the frontend.  */
	return resolve_sv_displacement (argno + 1, type, load_p);

      if (!VECTOR_TYPE_P (actual))
	{
	  error_at (location, "passing %qT to argument %d of %qE,"
		    " which expects a vector or pointer base address",
		    actual, argno + 1, fndecl);
	  return MODE_none;
	}
    }

  /* Check for the correct choice of vector base type.  */
  vector_type_index base_vector_type;
  if (type == NUM_TYPE_SUFFIXES)
    {
      /* Since prefetches have no type suffix, there is a free choice
	 between 32-bit and 64-bit base addresses.  */
      base_vector_type = infer_vector_base_type (argno);
      if (base_vector_type == NUM_VECTOR_TYPES)
	return MODE_none;
    }
  else
    {
      /* Check for some form of vector type, without saying which type
	 we expect.  */
      type_suffix_index base_type = infer_vector_type (argno);
      if (base_type == NUM_TYPE_SUFFIXES)
	return MODE_none;

      /* Check whether the type is the right one.  */
      unsigned int required_bits = type_suffixes[type].element_bits;
      gcc_assert (required_bits == 32 || required_bits == 64);
      type_suffix_index required_type = (required_bits == 32
					 ? TYPE_SUFFIX_u32
					 : TYPE_SUFFIX_u64);
      if (required_type != base_type)
	{
	  error_at (location, "passing %qT to argument %d of %qE,"
		    " which expects %qT", actual, argno + 1, fndecl,
		    get_vector_type (required_type));
	  return MODE_none;
	}
      base_vector_type = type_suffixes[base_type].vector_type;
    }

  /* Check the scalar displacement, if any.  */
  if (displacement_units () != UNITS_none
      && !require_scalar_type (argno + 1, "int64_t"))
    return MODE_none;

  /* Find the appropriate mode suffix.  The checks above should have
     weeded out all erroneous cases.  */
  for (unsigned int mode_i = 0; mode_i < ARRAY_SIZE (mode_suffixes); ++mode_i)
    {
      const mode_suffix_info &mode = mode_suffixes[mode_i];
      if (mode.base_vector_type == base_vector_type
	  && mode.displacement_vector_type == NUM_VECTOR_TYPES
	  && mode.displacement_units == displacement_units ())
	return mode_suffix_index (mode_i);
    }

  gcc_unreachable ();
}

/* Require arguments ARGNO and ARGNO + 1 to form an ADR-style address,
   i.e. one with a vector of base addresses and a vector of displacements.
   The overloaded function's mode suffix determines the units of the
   displacement (bytes for "_offset", elements for "_index").

   Return the associated mode suffix on success, otherwise report
   an error and return MODE_none.  */
mode_suffix_index
function_resolver::resolve_adr_address (unsigned int argno)
{
  vector_type_index base_type = infer_vector_base_type (argno);
  if (base_type == NUM_VECTOR_TYPES)
    return MODE_none;

  vector_type_index displacement_type
    = infer_vector_displacement_type (argno + 1);
  if (displacement_type == NUM_VECTOR_TYPES)
    return MODE_none;

  mode_suffix_index mode = find_mode_suffix (base_type, displacement_type,
					     displacement_units ());
  if (mode == MODE_none)
    {
      if (mode_suffix_id == MODE_offset)
	error_at (location, "cannot combine a base of type %qT with"
		  " an offset of type %qT",
		  get_argument_type (argno), get_argument_type (argno + 1));
      else
	error_at (location, "cannot combine a base of type %qT with"
		  " an index of type %qT",
		  get_argument_type (argno), get_argument_type (argno + 1));
    }
  return mode;
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

/* If the function is predicated, check that the first argument is a
   suitable governing predicate.  Also check that there are NOPS further
   arguments after any governing predicate, but don't check what they are.

   Return true on success, otherwise report a suitable error.
   When returning true:

   - set I to the number of the first unchecked argument.
   - set NARGS to the total number of arguments.  */
bool
function_resolver::check_gp_argument (unsigned int nops,
				      unsigned int &i, unsigned int &nargs)
{
  i = 0;
  if (pred != PRED_none)
    {
      /* Unary merge operations should use resolve_unary instead.  */
      gcc_assert (nops != 1 || pred != PRED_m);
      nargs = nops + 1;
      if (!check_num_arguments (nargs)
	  || !require_vector_type (i, VECTOR_TYPE_svbool_t))
	return false;
      i += 1;
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

   ARGNO is the index of the final argument.  The inferred type
   suffix is FIRST_TYPE, which was obtained from argument FIRST_ARGNO.
   EXPECTED_TCLASS and EXPECTED_BITS describe the expected properties
   of the final vector or scalar argument, in the same way as for
   require_derived_vector_type.

   Return the function decl of the resolved function on success,
   otherwise report a suitable error and return error_mark_node.  */
tree function_resolver::
finish_opt_n_resolution (unsigned int argno, unsigned int first_argno,
			 type_suffix_index first_type,
			 type_class_index expected_tclass,
			 unsigned int expected_bits)
{
  tree scalar_form = lookup_form (MODE_n, first_type);

  /* Allow the final argument to be scalar, if an _n form exists.  */
  if (scalar_argument_p (argno))
    {
      if (scalar_form)
	return scalar_form;

      /* Check the vector form normally.  If that succeeds, raise an
	 error about having no corresponding _n form.  */
      tree res = resolve_to (mode_suffix_id, first_type);
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

  return resolve_to (mode_suffix_id, first_type);
}

/* Resolve a (possibly predicated) unary function.  If the function uses
   merge predication, there is an extra vector argument before the
   governing predicate that specifies the values of inactive elements.
   This argument has the following properties:

   - the type class must be the same as for active elements if MERGE_TCLASS
     is SAME_TYPE_CLASS, otherwise it must be MERGE_TCLASS itself.

   - the element size must be the same as for active elements if MERGE_BITS
     is SAME_TYPE_SIZE, otherwise it must be MERGE_BITS itself.

   Return the function decl of the resolved function on success,
   otherwise report a suitable error and return error_mark_node.  */
tree
function_resolver::resolve_unary (type_class_index merge_tclass,
				  unsigned int merge_bits)
{
  type_suffix_index type;
  if (pred == PRED_m)
    {
      if (!check_num_arguments (3))
	return error_mark_node;
      if (merge_tclass == SAME_TYPE_CLASS && merge_bits == SAME_SIZE)
	{
	  /* The inactive elements are the same as the active elements,
	     so we can use normal left-to-right resolution.  */
	  if ((type = infer_vector_type (0)) == NUM_TYPE_SUFFIXES
	      || !require_vector_type (1, VECTOR_TYPE_svbool_t)
	      || !require_matching_vector_type (2, type))
	    return error_mark_node;
	}
      else
	{
	  /* The inactive element type is a function of the active one,
	     so resolve the active one first.  */
	  if (!require_vector_type (1, VECTOR_TYPE_svbool_t)
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
      || (type = infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
    return error_mark_node;

  i += 1;
  for (; i < nargs - nimm; ++i)
    if (!require_matching_vector_type (i, type))
      return error_mark_node;

  for (; i < nargs; ++i)
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
      || (type = infer_vector_type (i)) == NUM_TYPE_SUFFIXES)
    return error_mark_node;

  unsigned int first_arg = i++;
  for (; i < nargs - 1; ++i)
    if (!require_matching_vector_type (i, type))
      return error_mark_node;

  return finish_opt_n_resolution (i, first_arg, type);
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
    m_fntype (fntype), m_nargs (nargs), m_args (args),
    /* We don't have to worry about unary _m operations here, since they
       never have arguments that need checking.  */
    m_base_arg (pred != PRED_none ? 1 : 0)
{
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

/* Check that argument REL_ARGNO is an integer constant expression that
   has the value VALUE0 or VALUE1.  REL_ARGNO counts from the end of the
   predication arguments.  */
bool
function_checker::require_immediate_either_or (unsigned int rel_argno,
					       HOST_WIDE_INT value0,
					       HOST_WIDE_INT value1)
{
  unsigned int argno = m_base_arg + rel_argno;
  if (!argument_exists_p (argno))
    return true;

  HOST_WIDE_INT actual;
  if (!require_immediate (argno, actual))
    return false;

  if (actual != value0 && actual != value1)
    {
      report_neither_nor (location, fndecl, argno, actual, 90, 270);
      return false;
    }

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

/* Check that argument REL_ARGNO is suitable for indexing argument
   REL_ARGNO - 1, in groups of GROUP_SIZE elements.  REL_ARGNO counts
   from the end of the predication arguments.  */
bool
function_checker::require_immediate_lane_index (unsigned int rel_argno,
						unsigned int group_size)
{
  unsigned int argno = m_base_arg + rel_argno;
  if (!argument_exists_p (argno))
    return true;

  /* Get the type of the previous argument.  tree_argument_type wants a
     1-based number, whereas ARGNO is 0-based.  */
  machine_mode mode = TYPE_MODE (type_argument_type (m_fntype, argno));
  gcc_assert (VECTOR_MODE_P (mode));
  unsigned int nlanes = 128 / (group_size * GET_MODE_UNIT_BITSIZE (mode));
  return require_immediate_range (rel_argno, 0, nlanes - 1);
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
			      gimple_stmt_iterator *gsi_in, gcall *call_in)
  : function_call_info (gimple_location (call_in), instance, fndecl),
    gsi (gsi_in), call (call_in), lhs (gimple_call_lhs (call_in))
{
}

/* VALUE might be a vector of type VECTYPE or a single scalar element.
   Duplicate it into a vector of type VECTYPE in the latter case, adding any
   new statements to STMTS.  */
tree
gimple_folder::force_vector (gimple_seq &stmts, tree vectype, tree value)
{
  if (!VECTOR_TYPE_P (TREE_TYPE (value)))
    value = gimple_build_vector_from_val (&stmts, vectype, value);
  return value;
}

/* Convert predicate argument ARGNO so that it has the type appropriate for
   an operation on VECTYPE.  Add any new statements to STMTS.  */
tree
gimple_folder::convert_pred (gimple_seq &stmts, tree vectype,
			     unsigned int argno)
{
  tree predtype = truth_type_for (vectype);
  tree pred = gimple_call_arg (call, argno);
  return gimple_build (&stmts, VIEW_CONVERT_EXPR, predtype, pred);
}

/* Return a pointer to the address in a contiguous load or store,
   given that each memory vector has type VECTYPE.  Add any new
   statements to STMTS.  */
tree
gimple_folder::fold_contiguous_base (gimple_seq &stmts, tree vectype)
{
  tree base = gimple_call_arg (call, 1);
  if (mode_suffix_id == MODE_vnum)
    {
      tree offset = gimple_call_arg (call, 2);
      offset = gimple_convert (&stmts, sizetype, offset);
      offset = gimple_build (&stmts, MULT_EXPR, sizetype, offset,
			     TYPE_SIZE_UNIT (vectype));
      base = gimple_build (&stmts, POINTER_PLUS_EXPR, TREE_TYPE (base),
			   base, offset);
    }
  return base;
}

/* Return the alignment and TBAA argument to an internal load or store
   function like IFN_MASK_LOAD or IFN_MASK_STORE, given that it accesses
   memory elements of type TYPE.  */
tree
gimple_folder::load_store_cookie (tree type)
{
  return build_int_cst (build_pointer_type (type), TYPE_ALIGN_UNIT (type));
}

/* Fold the call to a PTRUE, taking the element size from type suffix 0.  */
gimple *
gimple_folder::fold_to_ptrue ()
{
  tree svbool_type = TREE_TYPE (lhs);
  tree bool_type = TREE_TYPE (svbool_type);
  unsigned int element_bytes = type_suffix (0).element_bytes;

  /* The return type is svbool_t for all type suffixes, thus for b8 we
     want { 1, 1, 1, 1, ... }, for b16 we want { 1, 0, 1, 0, ... }, etc.  */
  tree_vector_builder builder (svbool_type, element_bytes, 1);
  builder.quick_push (build_all_ones_cst (bool_type));
  for (unsigned int i = 1; i < element_bytes; ++i)
    builder.quick_push (build_zero_cst (bool_type));
  return gimple_build_assign (lhs, builder.build ());
}

/* Fold the call to a PFALSE.  */
gimple *
gimple_folder::fold_to_pfalse ()
{
  return gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
}

/* Fold an operation to a constant predicate in which the first VL
   elements are set and the rest are clear.  Take the element size
   from type suffix 0.  */
gimple *
gimple_folder::fold_to_vl_pred (unsigned int vl)
{
  tree vectype = TREE_TYPE (lhs);
  tree element_type = TREE_TYPE (vectype);
  tree minus_one = build_all_ones_cst (element_type);
  tree zero = build_zero_cst (element_type);
  unsigned int element_bytes = type_suffix (0).element_bytes;

  /* Construct COUNT elements that contain the ptrue followed by
     a repeating sequence of COUNT elements.  */
  unsigned int count = constant_lower_bound (TYPE_VECTOR_SUBPARTS (vectype));
  gcc_assert (vl * element_bytes <= count);
  tree_vector_builder builder (vectype, count, 2);
  for (unsigned int i = 0; i < count * 2; ++i)
    {
      bool bit = (i & (element_bytes - 1)) == 0 && i < vl * element_bytes;
      builder.quick_push (bit ? minus_one : zero);
    }
  return gimple_build_assign (lhs, builder.build ());
}

/* Try to fold the call.  Return the new statement on success and null
   on failure.  */
gimple *
gimple_folder::fold ()
{
  /* Don't fold anything when SVE is disabled; emit an error during
     expansion instead.  */
  if (!TARGET_SVE)
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

/* Choose between signed and unsigned direct optabs SIGNED_OP and
   UNSIGNED_OP based on the signedness of type suffix SUFFIX_I, then
   pick the appropriate optab handler for the mode.  Use MODE as the
   mode if given, otherwise use the mode of type suffix SUFFIX_I.  */
insn_code
function_expander::direct_optab_handler_for_sign (optab signed_op,
						  optab unsigned_op,
						  unsigned int suffix_i,
						  machine_mode mode)
{
  if (mode == VOIDmode)
    mode = vector_mode (suffix_i);
  optab op = type_suffix (suffix_i).unsigned_p ? unsigned_op : signed_op;
  return ::direct_optab_handler (op, mode);
}

/* Return true if X overlaps any input.  */
bool
function_expander::overlaps_input_p (rtx x)
{
  for (unsigned int i = 0; i < args.length (); ++i)
    if (reg_overlap_mentioned_p (x, args[i]))
      return true;
  return false;
}

/* Return the base address for a contiguous load or store function.
   MEM_MODE is the mode of the addressed memory.  */
rtx
function_expander::get_contiguous_base (machine_mode mem_mode)
{
  rtx base = args[1];
  if (mode_suffix_id == MODE_vnum)
    {
      /* Use the size of the memory mode for extending loads and truncating
	 stores.  Use the size of a full vector for non-extending loads
	 and non-truncating stores (including svld[234] and svst[234]).  */
      poly_int64 size = ordered_min (GET_MODE_SIZE (mem_mode),
				     BYTES_PER_SVE_VECTOR);
      rtx offset = gen_int_mode (size, Pmode);
      offset = simplify_gen_binary (MULT, Pmode, args[2], offset);
      base = simplify_gen_binary (PLUS, Pmode, base, offset);
    }
  return base;
}

/* For a function that does the equivalent of:

     OUTPUT = COND ? FN (INPUTS) : FALLBACK;

   return the value of FALLBACK.

   MODE is the mode of OUTPUT.  NOPS is the number of operands in INPUTS.
   MERGE_ARGNO is the argument that provides FALLBACK for _m functions,
   or DEFAULT_MERGE_ARGNO if we should apply the usual rules.

   ARGNO is the caller's index into args.  If the returned value is
   argument 0 (as for unary _m operations), increment ARGNO past the
   returned argument.  */
rtx
function_expander::get_fallback_value (machine_mode mode, unsigned int nops,
				       unsigned int merge_argno,
				       unsigned int &argno)
{
  if (pred == PRED_z)
    return CONST0_RTX (mode);

  gcc_assert (pred == PRED_m || pred == PRED_x);
  if (merge_argno == DEFAULT_MERGE_ARGNO)
    merge_argno = nops == 1 && pred == PRED_m ? 0 : 1;

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

/* As for get_reg_target, but make sure that the returned REG does not
   overlap any inputs.  */
rtx
function_expander::get_nonoverlapping_reg_target ()
{
  if (possible_target && overlaps_input_p (possible_target))
    possible_target = NULL_RTX;
  return get_reg_target ();
}

/* Add an output operand to the instruction we're building, which has
   code ICODE.  Bind the output to the preferred target rtx if possible.  */
void
function_expander::add_output_operand (insn_code icode)
{
  unsigned int opno = m_ops.length ();
  machine_mode mode = insn_data[icode].operand[opno].mode;
  m_ops.safe_grow (opno + 1);
  create_output_operand (&m_ops.last (), possible_target, mode);
}

/* Add an input operand to the instruction we're building, which has
   code ICODE.  Calculate the value of the operand as follows:

   - If the operand is a vector and X is not, broadcast X to fill a
     vector of the appropriate mode.

   - Otherwise, if the operand is a predicate, coerce X to have the
     mode that the instruction expects.  In this case X is known to be
     VNx16BImode (the mode of svbool_t).

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
	 aarch64_any_register_operand, which is used to avoid
	 combinatorial explosion in the reinterpret patterns.  */
      gcc_assert (operand.predicate == aarch64_any_register_operand);
      mode = GET_MODE (x);
    }
  else if (!VECTOR_MODE_P (GET_MODE (x)) && VECTOR_MODE_P (mode))
    x = expand_vector_broadcast (mode, x);
  else if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
    {
      gcc_assert (GET_MODE (x) == VNx16BImode);
      x = gen_lowpart (mode, x);
    }
  m_ops.safe_grow (m_ops.length () + 1);
  create_input_operand (&m_ops.last (), x, mode);
}

/* Add an integer operand with value X to the instruction.  */
void
function_expander::add_integer_operand (HOST_WIDE_INT x)
{
  m_ops.safe_grow (m_ops.length () + 1);
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

/* Add an address operand with value X.  The static operand data says
   what mode and form the address must have.  */
void
function_expander::add_address_operand (rtx x)
{
  m_ops.safe_grow (m_ops.length () + 1);
  create_address_operand (&m_ops.last (), x);
}

/* Add an operand that must be X.  The only way of legitimizing an
   invalid X is to reload the address of a MEM.  */
void
function_expander::add_fixed_operand (rtx x)
{
  m_ops.safe_grow (m_ops.length () + 1);
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

/* Convert the arguments to a gather/scatter function into the
   associated md operands.  Argument ARGNO is the scalar or vector base and
   argument ARGNO + 1 is the scalar or vector displacement (if applicable).
   The md pattern expects:

   - a scalar base
   - a vector displacement
   - a const_int that is 1 if the displacement is zero-extended from 32 bits
   - a scaling multiplier (1 for bytes, 2 for .h indices, etc.).  */
void
function_expander::prepare_gather_address_operands (unsigned int argno)
{
  machine_mode mem_mode = memory_vector_mode ();
  tree vector_type = base_vector_type ();
  units_index units = displacement_units ();
  if (units == UNITS_none)
    {
      /* Vector base, no displacement.  Convert to an integer zero base
	 and a vector byte offset.  */
      args.quick_insert (argno, const0_rtx);
      units = UNITS_bytes;
    }
  else if (vector_type)
    {
      /* Vector base, scalar displacement.  Convert to a scalar base and
	 a vector byte offset.  */
      std::swap (args[argno], args[argno + 1]);
      if (units == UNITS_elements)
	{
	  /* Convert the original scalar array index to a byte offset.  */
	  rtx size = gen_int_mode (GET_MODE_UNIT_SIZE (mem_mode), DImode);
	  args[argno] = simplify_gen_binary (MULT, DImode, args[argno], size);
	  units = UNITS_bytes;
	}
    }
  else
    {
      /* Scalar base, vector displacement.  This is what the md pattern wants,
	 so we just need to make sure that the scalar base has DImode.  */
      if (Pmode == SImode)
	args[argno] = simplify_gen_unary (ZERO_EXTEND, DImode,
					  args[argno], SImode);
      vector_type = displacement_vector_type ();
    }
  tree scalar_displacement_type = TREE_TYPE (vector_type);

  bool uxtw_p = (TYPE_PRECISION (scalar_displacement_type) < 64
		 && TYPE_UNSIGNED (scalar_displacement_type));
  unsigned int scale = (units == UNITS_bytes
			? 1 : GET_MODE_UNIT_SIZE (mem_mode));

  args.quick_insert (argno + 2, GEN_INT (uxtw_p));
  args.quick_insert (argno + 3, GEN_INT (scale));
}

/* The final argument is an immediate svprfop value.  Add two fake arguments
   to represent the rw and locality operands of a PREFETCH rtx.  */
void
function_expander::prepare_prefetch_operands ()
{
  unsigned int prfop = INTVAL (args.last ());
  /* Bit 3 of the prfop selects stores over loads.  */
  args.quick_push (GEN_INT ((prfop & 8) != 0));
  /* Bits 1 and 2 specify the locality; 0-based for svprfop but
     1-based for PREFETCH.  */
  args.quick_push (GEN_INT (((prfop >> 1) & 3) + 1));
}

/* Add a dummy argument to indicate whether predicate argument ARGNO
   is all-true when interpreted in mode PRED_MODE.  The hint goes
   immediately after ARGNO.  */
void
function_expander::add_ptrue_hint (unsigned int argno, machine_mode pred_mode)
{
  rtx pred = gen_lowpart (pred_mode, args[argno]);
  int hint = (pred == CONSTM1_RTX (pred_mode)
	      ? SVE_KNOWN_PTRUE : SVE_MAYBE_NOT_PTRUE);
  args.quick_insert (argno + 1, gen_int_mode (hint, SImode));
}

/* Rotate inputs args[START:END] one position to the left, so that
   args[START] becomes args[END - 1].  */
void
function_expander::rotate_inputs_left (unsigned int start, unsigned int end)
{
  rtx new_last = args[start];
  for (unsigned int i = start; i < end - 1; ++i)
    args[i] = args[i + 1];
  args[end - 1] = new_last;
}

/* Return true if the negation of argument ARGNO can be folded away,
   replacing it with the negated value if so.  MODE is the associated
   vector mode, but the argument could be a single element.  The main
   case this handles is constant arguments.  */
bool
function_expander::try_negating_argument (unsigned int argno,
					  machine_mode mode)
{
  rtx x = args[argno];
  if (!VECTOR_MODE_P (GET_MODE (x)))
    mode = GET_MODE_INNER (mode);

  x = simplify_unary_operation (NEG, mode, x, mode);
  if (!x)
    return false;

  args[argno] = x;
  return true;
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
   governing predicate.  We must therefore drop the GP from an _x call.  */
rtx
function_expander::use_unpred_insn (insn_code icode)
{
  /* We can't drop the predicate for _z and _m.  */
  gcc_assert (pred == PRED_x || pred == PRED_none);
  /* Discount the output operand.  */
  unsigned int nops = insn_data[icode].n_operands - 1;
  /* Drop the predicate argument in the case of _x predication.  */
  unsigned int bias = (pred == PRED_x ? 1 : 0);
  unsigned int i = 0;

  add_output_operand (icode);
  for (; i < nops; ++i)
    add_input_operand (icode, args[i + bias]);

  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which is a predicated
   operation that returns arbitrary values for inactive lanes.  */
rtx
function_expander::use_pred_x_insn (insn_code icode)
{
  /* At present we never need to handle PRED_none, which would involve
     creating a new predicate rather than using one supplied by the user.  */
  gcc_assert (pred == PRED_x);
  /* Discount the output operand.  */
  unsigned int nops = args.length () - 1;

  bool has_float_operand_p = FLOAT_MODE_P (insn_data[icode].operand[0].mode);

  /* Add the normal operands.  */
  add_output_operand (icode);
  add_input_operand (icode, args[0]);
  for (unsigned int i = 0; i < nops; ++i)
    {
      add_input_operand (icode, args[i + 1]);
      if (FLOAT_MODE_P (GET_MODE (args[i + 1])))
	has_float_operand_p = true;
    }

  if (has_float_operand_p)
    {
      /* Add a flag that indicates whether unpredicated instructions
	 are allowed.  */
      rtx pred = m_ops[1].value;
      if (flag_trapping_math && pred != CONST1_RTX (GET_MODE (pred)))
	add_integer_operand (SVE_STRICT_GP);
      else
	add_integer_operand (SVE_RELAXED_GP);
    }

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
  /* Discount the output, predicate and fallback value.  */
  unsigned int nops = insn_data[icode].n_operands - 3;
  machine_mode mode = insn_data[icode].operand[0].mode;

  unsigned int opno = 0;
  rtx fallback_arg = get_fallback_value (mode, nops, merge_argno, opno);
  rtx pred = args[opno++];

  add_output_operand (icode);
  add_input_operand (icode, pred);
  for (unsigned int i = 0; i < nops; ++i)
    add_input_operand (icode, args[opno + i]);
  add_input_operand (icode, fallback_arg);
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which is a select-like
   operation with the following operands:

   0: output
   1: true value
   2: false value
   3: predicate

   MERGE_ARGNO is the argument that provides the "false" value for _m
   functions, or DEFAULT_MERGE_ARGNO if we should apply the usual rules.  */
rtx
function_expander::use_vcond_mask_insn (insn_code icode,
					unsigned int merge_argno)
{
  machine_mode mode = vector_mode (0);

  unsigned int opno = 0;
  rtx false_arg = get_fallback_value (mode, 1, merge_argno, opno);
  rtx pred_arg = args[opno++];
  rtx true_arg = args[opno++];

  add_output_operand (icode);
  add_input_operand (icode, true_arg);
  add_input_operand (icode, false_arg);
  add_input_operand (icode, pred_arg);
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which loads memory operand 1
   into register operand 0 under the control of predicate operand 2.
   Extending loads have a further predicate (operand 3) that nominally
   controls the extension.  */
rtx
function_expander::use_contiguous_load_insn (insn_code icode)
{
  machine_mode mem_mode = memory_vector_mode ();

  add_output_operand (icode);
  add_mem_operand (mem_mode, get_contiguous_base (mem_mode));
  add_input_operand (icode, args[0]);
  if (GET_MODE_UNIT_BITSIZE (mem_mode) < type_suffix (0).element_bits)
    add_input_operand (icode, CONSTM1_RTX (VNx16BImode));
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which prefetches from
   address operand 1 under the control of predicate operand 0.
   Operands 2, 3 and 4 respectively specify the svprfop value,
   the PREFETCH rw flag and the PREFETCH locality.  */
rtx
function_expander::use_contiguous_prefetch_insn (insn_code icode)
{
  add_input_operand (icode, args[0]);
  add_address_operand (get_contiguous_base (VNx16QImode));
  for (unsigned int i = args.length () - 3; i < args.length (); ++i)
    add_input_operand (icode, args[i]);
  return generate_insn (icode);
}

/* Implement the call using instruction ICODE, which stores register operand 1
   into memory operand 0 under the control of predicate operand 2.  */
rtx
function_expander::use_contiguous_store_insn (insn_code icode)
{
  machine_mode mem_mode = memory_vector_mode ();

  add_mem_operand (mem_mode, get_contiguous_base (mem_mode));
  add_input_operand (icode, args.last ());
  add_input_operand (icode, args[0]);
  return generate_insn (icode);
}

/* Implement the call using one of the following strategies, chosen in order:

   (1) "aarch64_pred_<optab><mode>_z" for PRED_z predicate functions

   (2) "aarch64_pred_<optab><mode>" for PRED_x functions

   (3) a normal unpredicated optab for PRED_none and PRED_x functions,
       dropping the predicate in the latter case

   (4) "cond_<optab><mode>" otherwise

   where <optab> corresponds to:

   - CODE_FOR_SINT for signed integers
   - CODE_FOR_UINT for unsigned integers
   - UNSPEC_FOR_FP for floating-point values

   MERGE_ARGNO is the argument that provides the values of inactive lanes for
   _m functions, or DEFAULT_MERGE_ARGNO if we should apply the usual rules.  */
rtx
function_expander::map_to_rtx_codes (rtx_code code_for_sint,
				     rtx_code code_for_uint,
				     int unspec_for_fp,
				     unsigned int merge_argno)
{
  machine_mode mode = vector_mode (0);
  rtx_code code = (type_suffix (0).unsigned_p ? code_for_uint : code_for_sint);
  insn_code icode;

  /* Handle predicate logic operations, which always use _z predication.  */
  if (type_suffix (0).tclass == TYPE_bool)
    {
      gcc_assert (pred == PRED_z && code_for_uint == code_for_sint);
      return use_exact_insn (code_for_aarch64_pred_z (code, mode));
    }

  /* First try using UNSPEC_PRED_X patterns for _x predication,
     if available.  */
  if (pred == PRED_x)
    {
      if (type_suffix (0).integer_p)
	icode = maybe_code_for_aarch64_pred (code, mode);
      else
	icode = maybe_code_for_aarch64_pred (unspec_for_fp, mode);
      if (icode != CODE_FOR_nothing)
	return use_pred_x_insn (icode);
    }

  /* Otherwise expand PRED_none and PRED_x operations without a predicate.
     Floating-point operations conventionally use the signed rtx code.  */
  if (pred == PRED_none || pred == PRED_x)
    return use_unpred_insn (direct_optab_handler (code_to_optab (code), 0));

  /* Don't use cond_*_optabs here, since not all codes have one yet.  */
  if (type_suffix (0).integer_p)
    icode = code_for_cond (code, mode);
  else
    icode = code_for_cond (unspec_for_fp, mode);
  return use_cond_insn (icode, merge_argno);
}

/* Implement the call using one of the following strategies, chosen in order:

   (1) "aarch64_pred_<optab><mode>" for PRED_x functions; this is a
       predicated pattern

   (2) "aarch64_sve_<optab><mode>" for PRED_none and PRED_x functions;
       this is an unpredicated pattern

   (3) "cond_<optab><mode>" otherwise

   where <optab> corresponds to:

   - UNSPEC_FOR_SINT for signed integers
   - UNSPEC_FOR_UINT for unsigned integers
   - UNSPEC_FOR_FP for floating-point values

   MERGE_ARGNO is the argument that provides the values of inactive lanes for
   _m functions, or DEFAULT_MERGE_ARGNO if we should apply the usual rules.  */
rtx
function_expander::map_to_unspecs (int unspec_for_sint, int unspec_for_uint,
				   int unspec_for_fp, unsigned int merge_argno)
{
  machine_mode mode = vector_mode (0);
  int unspec = (!type_suffix (0).integer_p ? unspec_for_fp
		: type_suffix (0).unsigned_p ? unspec_for_uint
		: unspec_for_sint);

  if (pred == PRED_x)
    {
      insn_code icode = maybe_code_for_aarch64_pred (unspec, mode);
      if (icode != CODE_FOR_nothing)
	return use_pred_x_insn (icode);
    }

  if (pred == PRED_none || pred == PRED_x)
    return use_unpred_insn (code_for_aarch64_sve (unspec, mode));

  insn_code icode = code_for_cond (unspec, vector_mode (0));
  return use_cond_insn (icode, merge_argno);
}

/* Implement the call using an @aarch64 instruction and the
   instructions are parameterized by an rtx_code.  CODE_FOR_SINT
   is the rtx_code for signed integer operations, CODE_FOR_UINT
   is the rtx_code for unsigned integer operations.  */
rtx
function_expander::expand_signed_unpred_op (rtx_code code_for_sint,
					    rtx_code code_for_uint)
{
  insn_code icode;
  if (type_suffix (0).unsigned_p)
    icode = code_for_aarch64 (code_for_uint, code_for_uint, vector_mode (0));
  else
    icode = code_for_aarch64 (code_for_sint, code_for_sint, vector_mode (0));
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

/* Register the built-in SVE ABI types, such as __SVBool_t.  */
static void
register_builtin_types ()
{
#define DEF_SVE_TYPE(ACLE_NAME, NCHARS, ABI_NAME, SCALAR_TYPE) \
  scalar_types[VECTOR_TYPE_ ## ACLE_NAME] = SCALAR_TYPE;
#include "aarch64-sve-builtins.def"

  for (unsigned int i = 0; i < NUM_VECTOR_TYPES; ++i)
    {
      tree eltype = scalar_types[i];
      tree vectype;
      if (eltype == boolean_type_node)
	{
	  vectype = build_truth_vector_type_for_mode (BYTES_PER_SVE_VECTOR,
						      VNx16BImode);
	  gcc_assert (TYPE_MODE (vectype) == VNx16BImode
		      && TYPE_MODE (vectype) == TYPE_MODE_RAW (vectype)
		      && TYPE_ALIGN (vectype) == 16
		      && known_eq (wi::to_poly_offset (TYPE_SIZE (vectype)),
				   BYTES_PER_SVE_VECTOR));
	}
      else
	{
	  unsigned int elbytes = tree_to_uhwi (TYPE_SIZE_UNIT (eltype));
	  poly_uint64 nunits = exact_div (BYTES_PER_SVE_VECTOR, elbytes);
	  vectype = build_vector_type (eltype, nunits);
	  gcc_assert (VECTOR_MODE_P (TYPE_MODE (vectype))
		      && TYPE_MODE (vectype) == TYPE_MODE_RAW (vectype)
		      && TYPE_ALIGN (vectype) == 128
		      && known_eq (wi::to_poly_offset (TYPE_SIZE (vectype)),
				   BITS_PER_SVE_VECTOR));
	}
      vectype = build_distinct_type_copy (vectype);
      gcc_assert (vectype == TYPE_MAIN_VARIANT (vectype));
      SET_TYPE_STRUCTURAL_EQUALITY (vectype);
      TYPE_ARTIFICIAL (vectype) = 1;
      TYPE_INDIVISIBLE_P (vectype) = 1;
      abi_vector_types[i] = vectype;
      lang_hooks.types.register_builtin_type (vectype,
					      vector_types[i].abi_name);
    }
}

/* Initialize all compiler built-ins related to SVE that should be
   defined at start-up.  */
void
init_builtins ()
{
  sve_switcher sve;
  register_builtin_types ();
}

/* Register vector type TYPE under its arm_sve.h name.  */
static void
register_vector_type (vector_type_index type)
{
  tree vectype = abi_vector_types[type];
  tree id = get_identifier (vector_types[type].acle_name);
  tree decl = build_decl (input_location, TYPE_DECL, id, vectype);
  decl = lang_hooks.decls.pushdecl (decl);

  /* Record the new ACLE type if pushdecl succeeded without error.  Use
     the ABI type otherwise, so that the type we record at least has the
     right form, even if it doesn't have the right name.  This should give
     better error recovery behavior than installing error_mark_node or
     installing an incorrect type.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == vectype)
    vectype = TREE_TYPE (decl);
  acle_vector_types[0][type] = vectype;
}

/* Register the tuple type that contains NUM_VECTORS vectors of type TYPE.  */
static void
register_tuple_type (unsigned int num_vectors, vector_type_index type)
{
  tree tuple_type = lang_hooks.types.make_type (RECORD_TYPE);

  /* The contents of the type are opaque, so we can define them in any
     way that maps to the correct ABI type.

     Here we choose to use the same layout as for arm_neon.h, but with
     "__val" instead of "val":

	struct svfooxN_t { svfoo_t __val[N]; };

     (It wouldn't be possible to write that directly in C or C++ for
     sizeless types, but that's not a problem for this function.)

     Using arrays simplifies the handling of svget and svset for variable
     arguments.  */
  tree vector_type = acle_vector_types[0][type];
  tree array_type = build_array_type_nelts (vector_type, num_vectors);
  gcc_assert (VECTOR_MODE_P (TYPE_MODE (array_type))
	      && TYPE_MODE_RAW (array_type) == TYPE_MODE (array_type)
	      && TYPE_ALIGN (array_type) == 128);

  tree field = build_decl (input_location, FIELD_DECL,
			   get_identifier ("__val"), array_type);
  DECL_FIELD_CONTEXT (field) = tuple_type;
  TYPE_FIELDS (tuple_type) = field;
  layout_type (tuple_type);
  gcc_assert (VECTOR_MODE_P (TYPE_MODE (tuple_type))
	      && TYPE_MODE_RAW (tuple_type) == TYPE_MODE (tuple_type)
	      && TYPE_ALIGN (tuple_type) == 128);

  /* Work out the structure name.  */
  char buffer[sizeof ("svfloat64x4_t")];
  const char *vector_type_name = vector_types[type].acle_name;
  snprintf (buffer, sizeof (buffer), "%.*sx%d_t",
	    (int) strlen (vector_type_name) - 2, vector_type_name,
	    num_vectors);

  tree decl = build_decl (input_location, TYPE_DECL,
			  get_identifier (buffer), tuple_type);
  TYPE_NAME (tuple_type) = decl;
  TYPE_STUB_DECL (tuple_type) = decl;
  lang_hooks.decls.pushdecl (decl);
  /* ??? Undo the effect of set_underlying_type for C.  The C frontend
     doesn't recognize DECL as a built-in because (as intended) the decl has
     a real location instead of BUILTINS_LOCATION.  The frontend therefore
     treats the decl like a normal C "typedef struct foo foo;", expecting
     the type for tag "struct foo" to have a dummy unnamed TYPE_DECL instead
     of the named one we attached above.  It then sets DECL_ORIGINAL_TYPE
     on the supposedly unnamed decl, creating a circularity that upsets
     dwarf2out.

     We don't want to follow the normal C model and create "struct foo"
     tags for tuple types since (a) the types are supposed to be opaque
     and (b) they couldn't be defined as a real struct anyway.  Treating
     the TYPE_DECLs as "typedef struct foo foo;" without creating
     "struct foo" would lead to confusing error messages.  */
  DECL_ORIGINAL_TYPE (decl) = NULL_TREE;

  acle_vector_types[num_vectors - 1][type] = tuple_type;
}

/* Register the svpattern enum.  */
static void
register_svpattern ()
{
  auto_vec<string_int_pair, 32> values;
#define PUSH(UPPER, LOWER, VALUE) \
    values.quick_push (string_int_pair ("SV_" #UPPER, VALUE));
  AARCH64_FOR_SVPATTERN (PUSH)
#undef PUSH

  acle_svpattern = lang_hooks.types.simulate_enum_decl (input_location,
							"svpattern", values);
}

/* Register the svprfop enum.  */
static void
register_svprfop ()
{
  auto_vec<string_int_pair, 16> values;
#define PUSH(UPPER, LOWER, VALUE) \
    values.quick_push (string_int_pair ("SV_" #UPPER, VALUE));
  AARCH64_FOR_SVPRFOP (PUSH)
#undef PUSH

  acle_svprfop = lang_hooks.types.simulate_enum_decl (input_location,
						      "svprfop", values);
}

/* Implement #pragma GCC aarch64 "arm_sve.h".  */
void
handle_arm_sve_h ()
{
  if (function_table)
    {
      error ("duplicate definition of %qs", "arm_sve.h");
      return;
    }

  sve_switcher sve;

  /* Define the vector and tuple types.  */
  for (unsigned int type_i = 0; type_i < NUM_VECTOR_TYPES; ++type_i)
    {
      vector_type_index type = vector_type_index (type_i);
      register_vector_type (type);
      if (type != VECTOR_TYPE_svbool_t)
	for (unsigned int count = 2; count <= MAX_TUPLE_SIZE; ++count)
	  register_tuple_type (count, type);
    }

  /* Define the enums.  */
  register_svpattern ();
  register_svprfop ();

  /* Define the functions.  */
  function_table = new hash_table<registered_function_hasher> (1023);
  function_builder builder;
  for (unsigned int i = 0; i < ARRAY_SIZE (function_groups); ++i)
    builder.register_function_group (function_groups[i]);
}

/* Return the function decl with SVE function subcode CODE, or error_mark_node
   if no such function exists.  */
tree
builtin_decl (unsigned int code, bool)
{
  if (code >= vec_safe_length (registered_functions))
    return error_mark_node;
  return (*registered_functions)[code]->decl;
}

/* If we're implementing manual overloading, check whether the SVE
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

/* Perform any semantic checks needed for a call to the SVE function
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
  if (!check_required_extensions (location, rfn.decl, rfn.required_extensions))
    return false;
  return function_checker (location, rfn.instance, fndecl,
			   TREE_TYPE (rfn.decl), nargs, args).check ();
}

/* Attempt to fold STMT, given that it's a call to the SVE function
   with subcode CODE.  Return the new statement on success and null
   on failure.  Insert any other new statements at GSI.  */
gimple *
gimple_fold_builtin (unsigned int code, gimple_stmt_iterator *gsi, gcall *stmt)
{
  registered_function &rfn = *(*registered_functions)[code];
  return gimple_folder (rfn.instance, rfn.decl, gsi, stmt).fold ();
}

/* Expand a call to the SVE function with subcode CODE.  EXP is the call
   expression and TARGET is the preferred location for the result.
   Return the value of the lhs.  */
rtx
expand_builtin (unsigned int code, tree exp, rtx target)
{
  registered_function &rfn = *(*registered_functions)[code];
  if (!check_required_extensions (EXPR_LOCATION (exp), rfn.decl,
				  rfn.required_extensions))
    return target;
  return function_expander (rfn.instance, rfn.decl, exp, target).expand ();
}

/* Return true if TYPE is the ABI-defined __SVBool_t type.  */
bool
svbool_type_p (const_tree type)
{
  tree abi_type = abi_vector_types[VECTOR_TYPE_svbool_t];
  return type != error_mark_node && TYPE_MAIN_VARIANT (type) == abi_type;
}

/* If TYPE is a built-in type defined by the SVE ABI, return the mangled name,
   otherwise return NULL.  */
const char *
mangle_builtin_type (const_tree type)
{
  if (type == error_mark_node)
    return NULL;

  vector_type_index vtype = find_vector_type (type);
  if (vtype != NUM_VECTOR_TYPES)
    return vector_types[vtype].mangled_name;

  return NULL;
}

/* If TYPE is one of the ABI-defined SVE vector types, or an ACLE-defined
   tuple of them, return the number of vectors it contains.  Return 0
   otherwise.  */
unsigned int
nvectors_if_data_type (const_tree type)
{
  if (type == error_mark_node)
    return 0;

  type = TYPE_MAIN_VARIANT (type);
  if (VECTOR_TYPE_P (type))
    {
      vector_type_index type_id = find_vector_type (type);
      if (type_id != VECTOR_TYPE_svbool_t && type_id != NUM_VECTOR_TYPES)
	return 1;
    }
  else if (TREE_CODE (type) == RECORD_TYPE)
    {
      for (unsigned int size_i = 1; size_i < MAX_TUPLE_SIZE; ++size_i)
	for (unsigned int type_i = 0; type_i < NUM_VECTOR_TYPES; ++type_i)
	  {
	    tree tuple_type = acle_vector_types[size_i][type_i];
	    if (tuple_type && type == TYPE_MAIN_VARIANT (tuple_type))
	      return size_i + 1;
	  }
    }

  return 0;
}

/* Return true if TYPE is a built-in type defined by the SVE ABI.  */
bool
builtin_type_p (const_tree type)
{
  return svbool_type_p (type) || nvectors_if_data_type (type) > 0;
}

/* Implement TARGET_VERIFY_TYPE_CONTEXT for SVE types.  */
bool
verify_type_context (location_t loc, type_context_kind context,
		     const_tree type, bool silent_p)
{
  if (!builtin_type_p (type))
    return true;

  switch (context)
    {
    case TCTX_SIZEOF:
    case TCTX_STATIC_STORAGE:
      if (!silent_p)
	error_at (loc, "SVE type %qT does not have a fixed size", type);
      return false;

    case TCTX_ALIGNOF:
      if (!silent_p)
	error_at (loc, "SVE type %qT does not have a defined alignment", type);
      return false;

    case TCTX_THREAD_STORAGE:
      if (!silent_p)
	error_at (loc, "variables of type %qT cannot have thread-local"
		  " storage duration", type);
      return false;

    case TCTX_POINTER_ARITH:
      if (!silent_p)
	error_at (loc, "arithmetic on pointer to SVE type %qT", type);
      return false;

    case TCTX_FIELD:
      if (silent_p)
	;
      else if (lang_GNU_CXX ())
	error_at (loc, "member variables cannot have SVE type %qT", type);
      else
	error_at (loc, "fields cannot have SVE type %qT", type);
      return false;

    case TCTX_ARRAY_ELEMENT:
      if (!silent_p)
	error_at (loc, "array elements cannot have SVE type %qT", type);
      return false;
    }
  gcc_unreachable ();
}

}

using namespace aarch64_sve;

inline void
gt_ggc_mx (function_instance *)
{
}

inline void
gt_pch_nx (function_instance *)
{
}

inline void
gt_pch_nx (function_instance *, void (*) (void *, void *), void *)
{
}

#include "gt-aarch64-sve-builtins.h"
