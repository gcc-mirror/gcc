/* ACLE support for AArch64 NEON and SVE
   Copyright (C) 2018-2026 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_ACLE_BUILTINS_H
#define GCC_AARCH64_ACLE_BUILTINS_H

#include "aarch64-builtins.h"

/* The full name of an SVE ACLE function is the concatenation of:

   - the base name ("svadd", etc.)
   - the "mode" suffix ("_n", "_index", etc.)
   - the type suffixes ("_s32", "_b8", etc.)
   - the predication suffix ("_x", "_z", etc.)
   - the "_fpm" suffix when the floating point mode register is set

   Each piece of information is individually useful, so we retain this
   classification throughout:

   - function_base represents the base name

   - mode_suffix_index represents the mode suffix

   - type_suffix_index represents individual type suffixes, while
     type_suffix_triple represents a tuple of them

   - prediction_index extends the predication suffix with an additional
     alternative: PRED_implicit for implicitly-predicated operations

   - fpm_mode represents whether the fpm register is set or not

   In addition to its unique full name, a function may have a shorter
   overloaded alias.  This alias removes pieces of the suffixes that
   can be inferred from the arguments, such as by shortening the mode
   suffix or dropping some of the type suffixes.  The base name and the
   predication suffix stay the same.

   The function_shape class describes what arguments a given function
   takes and what its overloaded alias is called.  In broad terms,
   function_base describes how the underlying instruction behaves while
   function_shape describes how that instruction has been presented at
   the language level.

   The static arrays of function_group_info (function_groups,
   neon_sve_function_groups, sme_function_groups) use function_group to describe
   a group of related functions.  The function_builder class is responsible for
   expanding these static description into a list of individual functions
   and registering the associated built-in functions.  function_instance
   describes one of these individual functions in terms of the properties
   described above.

   The classes involved in compiling a function call are:

   - function_resolver, which resolves an overloaded function call to a
     specific function_instance and its associated function decl

   - function_checker, which checks whether the values of the arguments
     conform to the ACLE specification

   - gimple_folder, which tries to fold a function call at the gimple level

   - function_expander, which expands a function call into rtl instructions

   function_resolver and function_checker operate at the language level
   and so are associated with the function_shape.  gimple_folder and
   function_expander are concerned with the behavior of the function
   and so are associated with the function_base.

   Note that we've specifically chosen not to fold calls in the frontend,
   since SVE intrinsics will hardly ever fold a useful language-level
   constant.  */
namespace aarch64_acle {
/* The maximum number of vectors in an ACLE tuple type.  */
const unsigned int MAX_TUPLE_SIZE = 4;

/* Used to represent the default merge argument index for _m functions.
   The actual index depends on how many arguments the function takes.  */
const unsigned int DEFAULT_MERGE_ARGNO = ~0U;

/* Flags that describe what a function might do, in addition to reading
   its arguments and returning a result.  */
const unsigned int CP_READ_FPCR = 1U << 0;
const unsigned int CP_RAISE_FP_EXCEPTIONS = 1U << 1;
const unsigned int CP_READ_MEMORY = 1U << 2;
const unsigned int CP_PREFETCH_MEMORY = 1U << 3;
const unsigned int CP_WRITE_MEMORY = 1U << 4;
const unsigned int CP_READ_FFR = 1U << 5;
const unsigned int CP_WRITE_FFR = 1U << 6;
const unsigned int CP_READ_ZA = 1U << 7;
const unsigned int CP_WRITE_ZA = 1U << 8;
const unsigned int CP_READ_ZT0 = 1U << 9;
const unsigned int CP_WRITE_ZT0 = 1U << 10;

/* Enumerates the SVE predicate and (data) vector types, together called
   "vector types" for brevity.  */
enum vector_type_index
{
#define DEF_SVE_TYPE(ACLE_NAME, ...) VECTOR_TYPE_##ACLE_NAME,
#include "aarch64-sve-builtins.def"
  VECTOR_TYPE_none,
  NUM_VECTOR_TYPES = VECTOR_TYPE_none,
};

/* Classifies the available measurement units for an address displacement.  */
enum units_index
{
  UNITS_none,
  UNITS_bytes,
  UNITS_elements,
  UNITS_vectors
};

/* Enumerates the pragma handlers.  */
enum handle_pragma_index
{
  arm_neon_handle,
  arm_sve_handle,
  arm_sme_handle,
  arm_neon_sve_handle,
  NUM_PRAGMA_HANDLERS
};

/* Describes the various uses of a governing predicate.  */
enum predication_index
{
  /* No governing predicate is present.  */
  PRED_none,

  /* A governing predicate is present but there is no predication suffix
     associated with it.  This is used when the result is neither a vector
     nor a predicate, since the distinction between "zeroing" and "merging"
     doesn't apply in that case.  It is also used when a suffix would be
     redundant (such as for loads and comparisons, which are inherently
     zeroing operations).  */
  PRED_implicit,

  /* Merging predication: copy inactive lanes from the first data argument
     to the vector result.  */
  PRED_m,

  /* "Don't care" predication: set inactive lanes of the vector result
     to arbitrary values.  */
  PRED_x,

  /* Zero predication: set inactive lanes of the vector result to zero.  */
  PRED_z,

  /* Merging predication for SME's ZA: merge into slices of the array
     instead of overwriting the whole slices.  */
  PRED_za_m,

  NUM_PREDS
};

/* Classifies intrinsics on whether they set the FPM register */
enum fpm_mode_index
{
  FPM_unused,
  FPM_set,
  NUM_FPM_MODES
};

/* Classifies element types, based on type suffixes with the bit count
   removed.  "count" isn't really an element type, but we pretend it is
   for consistency.  */
enum type_class_index
{
  TYPE_bool,
  TYPE_bfloat,
  TYPE_count,
  TYPE_float,
  TYPE_mfloat,
  TYPE_signed,
  TYPE_unsigned,
  TYPE_poly,
  NUM_TYPE_CLASSES
};

/* Classifies an operation into "modes"; for example, to distinguish
   vector-scalar operations from vector-vector operations, or to
   distinguish between different addressing modes.  This classification
   accounts for the function suffixes that occur between the base name
   and the first type suffix.  */
enum mode_suffix_index
{
#define DEF_SVE_MODE(NAME, ...) MODE_##NAME,
#include "aarch64-sve-builtins.def"
  MODE_none
};

/* Enumerates the possible type suffixes.  Each suffix is associated with
   a vector type, but for predicates provides extra information about the
   element size.  */
enum type_suffix_index
{
#define DEF_SVE_NEON_TYPE_SUFFIX(NAME, ...) TYPE_SUFFIX_##NAME,
#define DEF_SME_ZA_SUFFIX(NAME, ...) TYPE_SUFFIX_##NAME,
#include "aarch64-sve-builtins.def"
  NUM_TYPE_SUFFIXES
};

/* Enumerates the possible group suffixes.  Each suffix combines two
   optional pieces of information: the vector group size in a ZA index,
   and the number of vectors in the largest tuple argument.  */
enum group_suffix_index
{
#define DEF_SVE_GROUP_SUFFIX(NAME, ...) GROUP_##NAME,
#include "aarch64-sve-builtins.def"
  GROUP_none,
  NUM_GROUP_SUFFIXES
};

/* Combines three type suffixes.  */
typedef enum type_suffix_index type_suffix_triple[3];

class function_base;
class function_shape;

/* Static information about a mode suffix.  */
struct mode_suffix_info
{
  /* The suffix string itself.  */
  const char *string;

  /* The type of the vector base address, or NUM_VECTOR_TYPES if the
     mode does not include a vector base address.  */
  vector_type_index base_vector_type;

  /* The type of the vector displacement, or NUM_VECTOR_TYPES if the
     mode does not include a vector displacement.  (Note that scalar
     displacements are always int64_t.)  */
  vector_type_index displacement_vector_type;

  /* The units in which the vector or scalar displacement is measured,
     or UNITS_none if the mode doesn't take a displacement.  */
  units_index displacement_units;
};

#define DEF_SIMD_TYPE(E, M, Q, G) E,
enum aarch64_simd_type
{
#include "aarch64-simd-builtin-types.def"
  ARM_NEON_H_TYPES_LAST
};

/* Static information about a type suffix.  */
struct type_suffix_info
{
  /* The suffix string itself.  */
  const char *string;

  /* The associated ACLE vector or predicate type.  */
  vector_type_index vector_type : 8;

  /* What kind of type the suffix represents.  */
  type_class_index tclass : 8;

  /* The number of bits and bytes in an element.  For predicates this
     measures the associated data elements.  */
  unsigned int element_bits : 8;
  unsigned int element_bytes : 8;

  /* True if the suffix is for an integer type.  */
  unsigned int integer_p : 1;
  /* True if the suffix is for an unsigned type.  */
  unsigned int unsigned_p : 1;
  /* True if the suffix is for a floating-point type.  */
  unsigned int float_p : 1;
  /* True if the suffix is for a vector type (integer or float).  */
  unsigned int vector_p : 1;
  /* True if the suffix is for a boolean type.  */
  unsigned int bool_p : 1;
  /* True if the suffix is for SME's ZA.  */
  unsigned int za_p : 1;
  unsigned int spare : 10;

  /* The associated vector or predicate mode.  */
  machine_mode vector_mode : 16;

  /* The corresponding 64-bit and 128-bit arm_neon.h types, or
     ARM_NEON_H_TYPES_LAST if none.  */
  aarch64_simd_type neon64_type;
  aarch64_simd_type neon128_type;
};

/* Static information about a group suffix.  */
struct group_suffix_info
{
  /* The suffix string itself.  */
  const char *string;

  /* If the suffix describes a vector group in a ZA index, this is the
     size of that group, otherwise it is zero.  */
  unsigned int vg;

  /* The number of vectors in the largest (or only) tuple argument,
     or 1 if the suffix does not convey this information.  */
  unsigned int vectors_per_tuple;
};

/* Represents an SVE vector, predicate, tuple of vectors, or tuple of
   predicates.  There is also a representation of "no type"/"invalid type".  */
struct sve_type
{
  sve_type () = default;
  sve_type (type_suffix_index type) : type (type), num_vectors (1) {}
  sve_type (type_suffix_index type, unsigned int num_vectors)
    : type (type), num_vectors (num_vectors) {}

  /* Return true if the type is valid.  */
  explicit operator bool () const { return type != NUM_TYPE_SUFFIXES; }

  bool operator== (const sve_type &) const;
  bool operator!= (const sve_type &x) const { return !operator== (x); }

  /* This is one of:

     - TYPE_SUFFIX_b for svbool_t-based types
     - TYPE_SUFFIX_c for svcount_t-based types
     - the type suffix of a data element for SVE data vectors and tuples
     - NUM_TYPE_SUFFIXES for invalid types.  */
  type_suffix_index type = NUM_TYPE_SUFFIXES;

  /* If the type is a tuple, this is the number of vectors in the tuple,
     otherwise it is 1.  */
  unsigned int num_vectors = 1;
};

inline bool
sve_type::operator== (const sve_type &other) const
{
  return type == other.type && num_vectors == other.num_vectors;
}

/* Static information about a set of functions.  */
struct function_group_info
{
  /* The base name, as a string.  */
  const char *base_name;

  /* Describes the behavior associated with the function base name.  */
  const function_base *const *base;

  /* The shape of the functions, as described above the class definition.
     It's possible to have entries with the same base name but different
     shapes.  */
  const function_shape *const *shape;

  /* A list of the available type suffixes, group suffixes, and predication
     types.  The function supports every combination of the three.

     The list of type suffixes is terminated by three NUM_TYPE_SUFFIXES.
     It is lexicographically ordered based on the index value.

     The list of group suffixes is terminated by NUM_GROUP_SUFFIXES
     and the list of predication types is terminated by NUM_PREDS.  */
  const type_suffix_triple *types;
  const group_suffix_index *groups;
  const predication_index *preds;

  /* The architecture extensions that the functions require.  */
  aarch64_required_extensions required_extensions;

  /* Whether the floating point register is set */
  fpm_mode_index fpm_mode;
};

/* Describes a single fully-resolved function (i.e. one that has a
   unique full name).  */
class GTY((user)) function_instance
{
public:
  function_instance (const char *, const function_base *,
		     const function_shape *, mode_suffix_index,
		     const type_suffix_triple &, group_suffix_index,
		     predication_index, fpm_mode_index);

  bool operator== (const function_instance &) const;
  bool operator!= (const function_instance &) const;
  hashval_t hash () const;

  unsigned int call_properties () const;
  bool reads_global_state_p () const;
  bool modifies_global_state_p () const;
  bool could_trap_p () const;

  vector_type_index gp_type_index () const;
  tree gp_value (gcall *) const;
  tree inactive_values (gcall *) const;
  tree gp_type () const;

  unsigned int vectors_per_tuple () const;
  tree memory_scalar_type () const;
  machine_mode memory_vector_mode () const;

  const mode_suffix_info &mode_suffix () const;
  tree base_vector_type () const;
  tree displacement_vector_type () const;
  units_index displacement_units () const;

  unsigned int num_za_tiles () const;

  const type_suffix_info &type_suffix (unsigned int) const;
  const group_suffix_info &group_suffix () const;

  tree scalar_type (unsigned int) const;
  tree vector_type (unsigned int) const;
  tree tuple_type (unsigned int) const;
  unsigned int elements_per_vq (unsigned int) const;
  machine_mode vector_mode (unsigned int) const;
  machine_mode tuple_mode (unsigned int) const;
  machine_mode gp_mode (unsigned int) const;

  /* The properties of the function.  */
  const char *base_name;
  const function_base *base;
  const function_shape *shape;
  mode_suffix_index mode_suffix_id;
  type_suffix_triple type_suffix_ids;
  group_suffix_index group_suffix_id;
  predication_index pred;
  fpm_mode_index fpm_mode;
  int gp_index;
};

class registered_function;

/* A class for building and registering function decls.  */
class function_builder
{
public:
  function_builder (handle_pragma_index, bool);
  ~function_builder ();

  void add_unique_function (const function_instance &, tree,
			    vec<tree> &, aarch64_required_extensions, bool);
  void add_overloaded_function (const function_instance &,
				aarch64_required_extensions);
  void add_overloaded_functions (const function_group_info &,
				 mode_suffix_index);

  void register_function_group (const function_group_info &);

private:
  void append_name (const char *);
  char *finish_name ();

  char *get_name (const function_instance &, bool);

  tree get_attributes (const function_instance &, aarch64_required_extensions);

  registered_function &add_function (const function_instance &,
				     const char *, tree, tree,
				     aarch64_required_extensions, bool, bool);

  /* The function type to use for functions that are resolved by
     function_resolver.  */
  tree m_overload_type;

  /* True if we should create a separate decl for each instance of an
     overloaded function, instead of using function_resolver.  */
  bool m_direct_overloads;

  /* Used for building up function names.  */
  obstack m_string_obstack;

  /* Used to store the index for the current function.  */
  unsigned int m_function_index;

  /* Stores the mode of the current pragma handler.  */
  bool m_function_nulls;
};

/* A base class for handling calls to built-in functions.  */
class function_call_info : public function_instance
{
public:
  function_call_info (location_t, const function_instance &, tree);

  bool function_returns_void_p ();

  /* The location of the call.  */
  location_t location;

  /* The FUNCTION_DECL that is being called.  */
  tree fndecl;
};

/* A class for resolving an overloaded function call.  */
class function_resolver : public function_call_info
{
public:
  enum target_type_restrictions { TARGET_ANY, TARGET_32_64 };
  enum { SAME_SIZE = 256, HALF_SIZE, QUARTER_SIZE };
  static const type_class_index SAME_TYPE_CLASS = NUM_TYPE_CLASSES;

  function_resolver (location_t, const function_instance &, tree,
		     vec<tree, va_gc> &);

  const char *get_scalar_type_name (type_suffix_index);
  tree get_argument_type (unsigned int);
  bool scalar_argument_p (unsigned int);

  void report_incorrect_num_vectors (unsigned int, sve_type, unsigned int);
  void report_mismatched_num_vectors (unsigned int, sve_type,
				      unsigned int, sve_type);

  tree report_no_such_form (sve_type);
  tree lookup_form (mode_suffix_index,
		    type_suffix_index = NUM_TYPE_SUFFIXES,
		    type_suffix_index = NUM_TYPE_SUFFIXES,
		    type_suffix_index = NUM_TYPE_SUFFIXES,
		    group_suffix_index = GROUP_none);
  tree lookup_form (mode_suffix_index, sve_type);
  tree resolve_to (mode_suffix_index,
		   type_suffix_index = NUM_TYPE_SUFFIXES,
		   type_suffix_index = NUM_TYPE_SUFFIXES,
		   type_suffix_index = NUM_TYPE_SUFFIXES,
		   group_suffix_index = GROUP_none);
  tree resolve_to (mode_suffix_index, sve_type);
  tree resolve_conversion (mode_suffix_index, sve_type);

  vector_type_index infer_predicate_type (unsigned int);
  type_suffix_index infer_integer_scalar_type (unsigned int);
  type_suffix_index infer_64bit_scalar_integer_pair (unsigned int);
  type_suffix_index infer_pointer_type (unsigned int, bool = false,
					target_type_restrictions = TARGET_ANY);
  sve_type infer_sve_type (unsigned int);
  sve_type infer_vector_or_tuple_type (unsigned int, unsigned int);
  type_suffix_index infer_vector_type (unsigned int);
  type_suffix_index infer_integer_vector_type (unsigned int);
  type_suffix_index infer_neon128_vector_type (unsigned int);
  type_suffix_index infer_unsigned_vector_type (unsigned int);
  type_suffix_index infer_sd_vector_type (unsigned int);
  sve_type infer_tuple_type (unsigned int);

  bool require_vector_or_scalar_type (unsigned int);

  bool require_matching_predicate_type (vector_type_index, sve_type);
  bool require_vector_type (unsigned int, vector_type_index);
  bool require_matching_vector_type (unsigned int, unsigned int, sve_type);
  bool require_derived_vector_type (unsigned int, unsigned int, sve_type,
				    type_class_index = SAME_TYPE_CLASS,
				    unsigned int = SAME_SIZE,
				    unsigned int = 1);

  bool require_scalar_type (unsigned int, const char *);
  bool require_nonscalar_type (unsigned int);
  bool require_pointer_type (unsigned int);
  bool require_matching_integer_scalar_type (unsigned int, unsigned int,
					     type_suffix_index);
  bool require_derived_scalar_type (unsigned int, type_class_index,
				    unsigned int = SAME_SIZE);
  bool require_matching_pointer_type (unsigned int, unsigned int,
				      type_suffix_index);
  bool require_integer_immediate (unsigned int);

  vector_type_index infer_vector_base_type (unsigned int);
  vector_type_index infer_vector_displacement_type (unsigned int);

  mode_suffix_index resolve_sv_displacement (unsigned int,
					     type_suffix_index, bool);
  mode_suffix_index resolve_gather_address (unsigned int,
					    type_suffix_index, bool);
  mode_suffix_index resolve_adr_address (unsigned int);

  bool check_num_arguments (unsigned int);
  bool check_gp_argument (unsigned int, unsigned int &, unsigned int &);
  tree resolve_unary (type_class_index = SAME_TYPE_CLASS,
		      unsigned int = SAME_SIZE, bool = false);
  tree resolve_uniform (unsigned int, unsigned int = 0);
  tree resolve_uniform_opt_n (unsigned int);
  tree finish_opt_n_resolution (unsigned int, unsigned int, type_suffix_index,
				type_class_index = SAME_TYPE_CLASS,
				unsigned int = SAME_SIZE,
				type_suffix_index = NUM_TYPE_SUFFIXES);
  tree finish_opt_n_resolution (unsigned int, unsigned int, type_suffix_index,
				type_class_index, unsigned int, sve_type);
  tree finish_opt_single_resolution (unsigned int, unsigned int, sve_type,
				     type_class_index = SAME_TYPE_CLASS);

  tree resolve ();

private:
  /* The arguments to the overloaded function.  */
  vec<tree, va_gc> &m_arglist;
};

/* A class for checking that the semantic constraints on a function call are
   satisfied, such as arguments being integer constant expressions with
   a particular range.  The parent class's FNDECL is the decl that was
   called in the original source, before overload resolution.  */
class function_checker : public function_call_info
{
public:
  function_checker (location_t, const function_instance &, tree,
		    tree, unsigned int, tree *);

  bool require_immediate_either_or (unsigned int, HOST_WIDE_INT,
				    HOST_WIDE_INT);
  bool require_immediate_enum (unsigned int, tree);
  bool require_immediate_lane_index (unsigned int, unsigned int,
				     unsigned int = 1);
  bool require_immediate_one_of (unsigned int, HOST_WIDE_INT, HOST_WIDE_INT,
				 HOST_WIDE_INT, HOST_WIDE_INT);
  bool require_immediate_range (unsigned int, HOST_WIDE_INT, HOST_WIDE_INT);

  bool check ();

private:
  bool argument_exists_p (unsigned int);

  bool require_immediate (unsigned int, HOST_WIDE_INT &);

  /* The type of the resolved function.  */
  tree m_fntype;

  /* The arguments to the function.  */
  unsigned int m_nargs;
  tree *m_args;

  /* The first argument not associated with the function's predication
     type.  */
  unsigned int m_base_arg;
};

/* A class for folding a gimple function call.  */
class gimple_folder : public function_call_info
{
public:
  gimple_folder (const function_instance &, tree,
		 gimple_stmt_iterator *, gcall *);

  tree force_vector (gimple_seq &, tree, tree);
  tree convert_pred (gimple_seq &, tree, unsigned int);
  tree fold_contiguous_base (gimple_seq &, tree);
  tree load_store_cookie (tree);

  gcall *redirect_call (const function_instance &);
  gimple *redirect_pred_x ();
  gimple *fold_pfalse ();
  gimple *convert_and_fold (tree, gimple *(*) (gimple_folder &,
					       tree, vec<tree> &));

  tree force_val (tree expr);
  gassign *assign (tree lhs, tree rhs);
  gassign *assign (tree lhs, tree_code code, tree rhs1, tree rhs2 = NULL_TREE,
		   tree rhs3 = NULL_TREE);

  gimple *fold_to_cstu (poly_uint64);
  gimple *fold_to_pfalse ();
  gimple *fold_to_ptrue ();
  gimple *fold_to_vl_pred (unsigned int);
  gimple *fold_const_binary (enum tree_code);
  gimple *fold_active_lanes_to (tree);
  gimple *fold_call_to (tree);
  gimple *fold_to_stmt_vops (gimple *);

  gimple *fold ();

  /* Where to insert extra statements that feed the final replacement.  */
  gimple_stmt_iterator *gsi;

  /* The call we're folding.  */
  gcall *call;

  /* The result of the call, or null if none.  */
  tree lhs;
};

/* A class for expanding a function call into RTL.  */
class function_expander : public function_call_info
{
public:
  function_expander (const function_instance &, tree, tree, rtx);
  rtx expand ();

  insn_code direct_optab_handler (optab, unsigned int = 0);
  insn_code direct_optab_handler_for_sign (optab, optab, unsigned int = 0,
					   machine_mode = E_VOIDmode);
  insn_code convert_optab_handler_for_sign (optab, optab, unsigned int,
					    machine_mode, machine_mode);

  machine_mode result_mode () const;

  bool overlaps_input_p (rtx);

  rtx convert_to_pmode (rtx);
  rtx get_contiguous_base (machine_mode, unsigned int = 1, unsigned int = 2,
			   aarch64_feature_flags = 0);
  rtx get_fallback_value (machine_mode, unsigned int,
			  unsigned int, unsigned int &);
  rtx get_reg_target ();
  rtx get_nonoverlapping_reg_target ();

  void add_output_operand (insn_code);
  void add_input_operand (insn_code, rtx);
  void add_integer_operand (poly_int64);
  void add_mem_operand (machine_mode, rtx);
  void add_address_operand (rtx);
  void add_fixed_operand (rtx);
  rtx generate_insn (insn_code);

  void prepare_gather_address_operands (unsigned int, bool = true);
  void prepare_prefetch_operands ();
  void add_ptrue_hint (unsigned int, machine_mode);
  void rotate_inputs_left (unsigned int, unsigned int);
  bool try_negating_argument (unsigned int, machine_mode);

  rtx use_exact_insn (insn_code);
  rtx use_unpred_insn (insn_code);
  rtx use_pred_x_insn (insn_code);
  rtx use_cond_insn (insn_code, unsigned int = DEFAULT_MERGE_ARGNO);
  rtx use_vcond_mask_insn (insn_code, unsigned int = DEFAULT_MERGE_ARGNO);
  rtx use_contiguous_load_insn (insn_code, bool = false);
  rtx use_contiguous_prefetch_insn (insn_code);
  rtx use_contiguous_store_insn (insn_code);

  rtx map_to_rtx_codes (rtx_code, rtx_code, unspec = UNSPEC_NONE,
			unspec = UNSPEC_NONE,
			unsigned int = DEFAULT_MERGE_ARGNO);
  rtx map_to_unspecs (unspec, unspec = UNSPEC_NONE, unspec = UNSPEC_NONE,
		      unsigned int = DEFAULT_MERGE_ARGNO);

  /* The function call expression.  */
  tree call_expr;

  /* For functions that return a value, this is the preferred location
     of that value.  It could be null or could have a different mode
     from the function return type.  */
  rtx possible_target;

  /* The expanded arguments.  */
  auto_vec<rtx, 16> args;

private:
  /* Used to build up the operands to an instruction.  */
  auto_vec<expand_operand, 8> m_ops;
};

/* Provides information about a particular function base name, and handles
   tasks related to the base name.  */
class function_base
{
public:
  /* Return a set of CP_* flags that describe what the function might do,
     in addition to reading its arguments and returning a result.  */
  virtual unsigned int call_properties (const function_instance &) const;

  /* If the function operates on tuples of vectors, return the number
     of vectors in the tuples, otherwise return 1.  */
  virtual unsigned int vectors_per_tuple (const function_instance &) const;

  /* If the function addresses memory, return the type of a single
     scalar memory element.  */
  virtual tree
  memory_scalar_type (const function_instance &) const
  {
    gcc_unreachable ();
  }

  /* If the function addresses memory, return a vector mode whose
     GET_MODE_NUNITS is the number of elements addressed and whose
     GET_MODE_INNER is the mode of a single scalar memory element.  */
  virtual machine_mode
  memory_vector_mode (const function_instance &) const
  {
    gcc_unreachable ();
  }

  /* Try to fold the given gimple call.  Return the new gimple statement
     on success, otherwise return null.  */
  virtual gimple *fold (gimple_folder &) const { return NULL; }

  /* Expand the given call into rtl.  Return the result of the function,
     or an arbitrary value if the function doesn't return a result.  */
  virtual rtx expand (function_expander &) const = 0;
};

/* Classifies functions into "shapes".  The idea is to take all the
   type signatures for a set of functions, remove the governing predicate
   (if any), and classify what's left based on:

   - the number of arguments

   - the process of determining the types in the signature from the mode
     and type suffixes in the function name (including types that are not
     affected by the suffixes)

   - which arguments must be integer constant expressions, and what range
     those arguments have

   - the process for mapping overloaded names to "full" names.  */
class function_shape
{
public:
  virtual bool has_merge_argument_p (const function_instance &,
				     unsigned int) const;

  virtual bool has_gp_argument_p (const function_instance &) const;

  virtual bool explicit_type_suffix_p (unsigned int) const = 0;

  /* True if the group suffix is present in overloaded names.
     This isn't meaningful for pre-SME intrinsics, and true is
     more common than false, so provide a default definition.  */
  virtual bool explicit_group_suffix_p () const { return true; }

  virtual type_suffix_index vector_base_type (type_suffix_index) const;

  /* Define all functions associated with the given group.  */
  virtual void build (function_builder &,
		      const function_group_info &) const = 0;

  /* Try to resolve the overloaded call.  Return the non-overloaded
     function decl on success and error_mark_node on failure.  */
  virtual tree resolve (function_resolver &) const = 0;

  /* Check whether the given call is semantically valid.  Return true
     if it is, otherwise report an error and return false.  */
  virtual bool check (function_checker &) const { return true; }
};

/* RAII class for temporarily disabling the effect of any -fpack-struct option.
   This is used to ensure that sve vector tuple types are defined with the
   correct alignment.  */
class sve_alignment_switcher
{
public:
  sve_alignment_switcher ();
  ~sve_alignment_switcher ();

private:
  unsigned int m_old_maximum_field_alignment;
};

extern const type_suffix_info type_suffixes[NUM_TYPE_SUFFIXES + 1];
extern const mode_suffix_info mode_suffixes[MODE_none + 1];
extern const group_suffix_info group_suffixes[NUM_GROUP_SUFFIXES];

extern tree scalar_types[NUM_VECTOR_TYPES + 1];
extern tree acle_vector_types[MAX_TUPLE_SIZE][NUM_VECTOR_TYPES + 1];
extern tree acle_svpattern;
extern tree acle_svprfop;

bool vector_cst_all_same (tree, unsigned int);
bool is_ptrue (tree, unsigned int);
bool is_pfalse (tree);
const function_instance *lookup_fndecl (tree);

/* Try to find a mode with the given mode_suffix_info fields.  Return the
   mode on success or MODE_none on failure.  */
inline mode_suffix_index
find_mode_suffix (vector_type_index base_vector_type,
		  vector_type_index displacement_vector_type,
		  units_index displacement_units)
{
  for (unsigned int mode_i = 0; mode_i < ARRAY_SIZE (mode_suffixes); ++mode_i)
    {
      const mode_suffix_info &mode = mode_suffixes[mode_i];
      if (mode.base_vector_type == base_vector_type
	  && mode.displacement_vector_type == displacement_vector_type
	  && mode.displacement_units == displacement_units)
	return mode_suffix_index (mode_i);
    }
  return MODE_none;
}

/* Return the type suffix associated with ELEMENT_BITS-bit elements of type
   class TCLASS.  */
inline type_suffix_index
find_type_suffix (type_class_index tclass, unsigned int element_bits)
{
  for (unsigned int i = 0; i < NUM_TYPE_SUFFIXES; ++i)
    if (type_suffixes[i].tclass == tclass
	&& type_suffixes[i].element_bits == element_bits)
      return type_suffix_index (i);
  gcc_unreachable ();
}

/* Return the single field in tuple type TYPE.  */
inline tree
tuple_type_field (tree type)
{
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      return field;
  gcc_unreachable ();
}

/* Return the vector type associated with TYPE.  */
inline tree
get_vector_type (sve_type type)
{
  auto vector_type = type_suffixes[type.type].vector_type;
  return acle_vector_types[type.num_vectors - 1][vector_type];
}

inline function_instance::
function_instance (const char *base_name_in, const function_base *base_in,
		   const function_shape *shape_in,
		   mode_suffix_index mode_suffix_id_in,
		   const type_suffix_triple &type_suffix_ids_in,
		   group_suffix_index group_suffix_id_in,
		   predication_index pred_in, fpm_mode_index fpm_mode_in)
  : base_name (base_name_in), base (base_in), shape (shape_in),
    mode_suffix_id (mode_suffix_id_in), group_suffix_id (group_suffix_id_in),
    pred (pred_in), fpm_mode (fpm_mode_in)
{
  memcpy (type_suffix_ids, type_suffix_ids_in, sizeof (type_suffix_ids));
}

inline bool
function_instance::operator== (const function_instance &other) const
{
  return (base == other.base
	  && shape == other.shape
	  && mode_suffix_id == other.mode_suffix_id
	  && type_suffix_ids[0] == other.type_suffix_ids[0]
	  && type_suffix_ids[1] == other.type_suffix_ids[1]
	  && type_suffix_ids[2] == other.type_suffix_ids[2]
	  && group_suffix_id == other.group_suffix_id
	  && pred == other.pred
	  && fpm_mode == other.fpm_mode);
}

inline bool
function_instance::operator!= (const function_instance &other) const
{
  return !operator== (other);
}

/* Return the index of the type that should be used as the governing
   predicate of this function.  */
inline vector_type_index
function_instance::gp_type_index () const
{
  if (group_suffix ().vectors_per_tuple > 1)
    return VECTOR_TYPE_svcount_t;
  return VECTOR_TYPE_svbool_t;
}

/* Return the type that should be used as the governing predicate of
   this function.  */
inline tree
function_instance::gp_type () const
{
  return acle_vector_types[0][gp_type_index ()];
}

/* Return the tree value that should be used as the governing predicate of
   this function.  If none then return NULL_TREE.  */
inline tree
function_instance::gp_value (gcall *call) const
{
  if (gp_index < 0)
    return NULL_TREE;

  return gimple_call_arg (call, gp_index);
}

/* Return the tree value that should be used for the inactive lanes should this
   function be a predicated function with a gp.  Otherwise return NULL_TREE.  */
inline tree
function_instance::inactive_values (gcall *call) const
{
  if (gp_index < 0)
    return NULL_TREE;

  /* Function is unary with m predicate.  */
  if (gp_index == 1)
    return gimple_call_arg (call, 0);

  /* Else the inactive values are the next element.  */
  return gimple_call_arg (call, 1);
}

/* If the function operates on tuples of vectors, return the number
   of vectors in the tuples, otherwise return 1.  */
inline unsigned int
function_instance::vectors_per_tuple () const
{
  return base->vectors_per_tuple (*this);
}

/* If the function addresses memory, return the type of a single
   scalar memory element.  */
inline tree
function_instance::memory_scalar_type () const
{
  return base->memory_scalar_type (*this);
}

/* If the function addresses memory, return a vector mode whose
   GET_MODE_NUNITS is the number of elements addressed and whose
   GET_MODE_INNER is the mode of a single scalar memory element.  */
inline machine_mode
function_instance::memory_vector_mode () const
{
  return base->memory_vector_mode (*this);
}

/* Return information about the function's mode suffix.  */
inline const mode_suffix_info &
function_instance::mode_suffix () const
{
  return mode_suffixes[mode_suffix_id];
}

/* Return the type of the function's vector base address argument,
   or null it doesn't have a vector base address.  */
inline tree
function_instance::base_vector_type () const
{
  return acle_vector_types[0][mode_suffix ().base_vector_type];
}

/* Return the type of the function's vector index or offset argument,
   or null if doesn't have a vector index or offset argument.  */
inline tree
function_instance::displacement_vector_type () const
{
  return acle_vector_types[0][mode_suffix ().displacement_vector_type];
}

/* Return the number of ZA tiles associated with the _za<N> suffix
   (which is always the first type suffix).  */
inline unsigned int
function_instance::num_za_tiles () const
{
  auto &suffix = type_suffix (0);
  gcc_checking_assert (suffix.za_p);
  return suffix.element_bytes;
}

/* If the function takes a vector or scalar displacement, return the units
   in which the displacement is measured, otherwise return UNITS_none.  */
inline units_index
function_instance::displacement_units () const
{
  return mode_suffix ().displacement_units;
}

/* Return information about type suffix I.  */
inline const type_suffix_info &
function_instance::type_suffix (unsigned int i) const
{
  return type_suffixes[type_suffix_ids[i]];
}

/* Return information about the function's group suffix.  */
inline const group_suffix_info &
function_instance::group_suffix () const
{
  return group_suffixes[group_suffix_id];
}

/* Return the scalar type associated with type suffix I.  */
inline tree
function_instance::scalar_type (unsigned int i) const
{
  return scalar_types[type_suffix (i).vector_type];
}

/* Return the vector type associated with type suffix I.  */
inline tree
function_instance::vector_type (unsigned int i) const
{
  return acle_vector_types[0][type_suffix (i).vector_type];
}

/* If the function operates on tuples of vectors, return the tuple type
   associated with type suffix I, otherwise return the vector type associated
   with type suffix I.  */
inline tree
function_instance::tuple_type (unsigned int i) const
{
  unsigned int num_vectors = vectors_per_tuple ();
  return acle_vector_types[num_vectors - 1][type_suffix (i).vector_type];
}

/* Return the number of elements of type suffix I that fit within a
   128-bit block.  */
inline unsigned int
function_instance::elements_per_vq (unsigned int i) const
{
  return 128 / type_suffix (i).element_bits;
}

/* Return the vector or predicate mode associated with type suffix I.  */
inline machine_mode
function_instance::vector_mode (unsigned int i) const
{
  return type_suffix (i).vector_mode;
}

/* Return the mode of tuple_type (I).  */
inline machine_mode
function_instance::tuple_mode (unsigned int i) const
{
  if (group_suffix ().vectors_per_tuple > 1)
    return TYPE_MODE (tuple_type (i));
  return vector_mode (i);
}

/* Return the mode of the governing predicate to use when operating on
   type suffix I.  */
inline machine_mode
function_instance::gp_mode (unsigned int i) const
{
  /* Multi-vector operations are predicated on an svcount_t, which has
     mode VNx16BI.  */
  if (group_suffix ().vectors_per_tuple > 1)
    return VNx16BImode;
  return aarch64_sve_pred_mode (type_suffix (i).element_bytes).require ();
}

/* Return true if the function has no return value.  */
inline bool
function_call_info::function_returns_void_p ()
{
  return TREE_TYPE (TREE_TYPE (fndecl)) == void_type_node;
}

/* Default implementation of function::call_properties, with conservatively
   correct behavior for floating-point instructions.  */
inline unsigned int
function_base::call_properties (const function_instance &instance) const
{
  unsigned int flags = 0;
  if (instance.type_suffix (0).float_p || instance.type_suffix (1).float_p)
    flags |= CP_READ_FPCR | CP_RAISE_FP_EXCEPTIONS;
  return flags;
}

inline unsigned int
function_base::vectors_per_tuple (const function_instance &instance) const
{
  return instance.group_suffix ().vectors_per_tuple;
}

/* Return true if INSTANCE (which has NARGS arguments) has an initial
   vector argument whose only purpose is to specify the values of
   inactive lanes.  */
inline bool
function_shape::has_merge_argument_p (const function_instance &instance,
				      unsigned int nargs) const
{
  return nargs == 1 && instance.pred == PRED_m;
}

/* Return true if INSTANCE has an predicate argument that can be used as the global
   predicate.  */
inline bool
function_shape::has_gp_argument_p (const function_instance &instance) const
{
  return instance.pred != PRED_none;
}

/* Return the mode of the result of a call.  */
inline machine_mode
function_expander::result_mode () const
{
  return TYPE_MODE (TREE_TYPE (TREE_TYPE (fndecl)));
}

/* Define a TYPES_<combination> macro for each combination of type
   suffixes that an ACLE function can have, where <combination> is the
   name used in DEF_SVE_FUNCTION entries.

   Use S (T) for single type suffix T and D (T1, T2) for a pair of type
   suffixes T1 and T2.  Use commas to separate the suffixes.

   Although the order shouldn't matter, the convention is to sort the
   suffixes lexicographically after dividing suffixes into a type
   class ("b", "f", etc.) and a numerical bit count.  */

/* _b8 _b16 _b32 _b64.  */
#define TYPES_all_pred(S, D, T) \
  S (b8), S (b16), S (b32), S (b64)

/* _c8 _c16 _c32 _c64.  */
#define TYPES_all_count(S, D, T) \
  S (c8), S (c16), S (c32), S (c64)

/* _b8 _b16 _b32 _b64
   _c8 _c16 _c32 _c64.  */
#define TYPES_all_pred_count(S, D, T) \
  TYPES_all_pred (S, D, T), \
  TYPES_all_count (S, D, T)

/* _f16 _f32 _f64.  */
#define TYPES_all_float(S, D, T) \
  S (f16), S (f32), S (f64)

/* _f32 _f64.  */
#define TYPES_sd_float(S, D, T) \
  S (f32), S (f64)

/* _s8 _s16 _s32 _s64.  */
#define TYPES_all_signed(S, D, T) \
  S (s8), S (s16), S (s32), S (s64)

/*     _f16 _f32 _f64
   _s8 _s16 _s32 _s64.  */
#define TYPES_all_float_and_signed(S, D, T) \
  TYPES_all_float (S, D, T), TYPES_all_signed (S, D, T)

/* _u8 _u16 _u32 _u64.  */
#define TYPES_all_unsigned(S, D, T) \
  S (u8), S (u16), S (u32), S (u64)

/* _s8 _s16 _s32 _s64
   _u8 _u16 _u32 _u64.  */
#define TYPES_all_integer(S, D, T) \
  TYPES_all_signed (S, D, T), TYPES_all_unsigned (S, D, T)

/*     _f16 _f32 _f64
   _s8 _s16 _s32 _s64
   _u8 _u16 _u32 _u64.  */
#define TYPES_all_arith(S, D, T) \
  TYPES_all_float (S, D, T), TYPES_all_integer (S, D, T)

/*	    _f32 _f64
   _s8 _s16 _s32 _s64
   _u8 _u16 _u32 _u64.  */
#define TYPES_all_arith_no_fp16(S, D, T) \
  S (f32), S (f64), \
  TYPES_all_integer (S, D, T)

#define TYPES_all_data(S, D, T) \
  TYPES_b_data (S, D, T), \
  TYPES_h_data (S, D, T), \
  TYPES_s_data (S, D, T), \
  TYPES_d_data (S, D, T)

/* _b only.  */
#define TYPES_b(S, D, T) \
  S (b)

/* _c only.  */
#define TYPES_c(S, D, T) \
  S (c)

/* _u8.  */
#define TYPES_b_unsigned(S, D, T) \
  S (u8)

/* _s8
   _u8.  */
#define TYPES_b_integer(S, D, T) \
  S (s8), TYPES_b_unsigned (S, D, T)

/* _mf8
   _s8
   _u8.  */
#define TYPES_b_data(S, D, T) \
  S (mf8), TYPES_b_integer (S, D, T)

/* _s8 _s16
   _u8 _u16.  */
#define TYPES_bh_integer(S, D, T) \
  S (s8), S (s16), S (u8), S (u16)

/* _u8 _u32.  */
#define TYPES_bs_unsigned(S, D, T) \
  S (u8), S (u32)

/* _s8 _s16 _s32.  */
#define TYPES_bhs_signed(S, D, T) \
  S (s8), S (s16), S (s32)

/* _u8 _u16 _u32.  */
#define TYPES_bhs_unsigned(S, D, T) \
  S (u8), S (u16), S (u32)

/* _s8 _s16 _s32
   _u8 _u16 _u32.  */
#define TYPES_bhs_integer(S, D, T) \
  TYPES_bhs_signed (S, D, T), TYPES_bhs_unsigned (S, D, T)

#define TYPES_bh_data(S, D, T)			\
  TYPES_b_data (S, D, T), \
  TYPES_h_data (S, D, T)

#define TYPES_bhs_data(S, D, T)			\
  TYPES_b_data (S, D, T), \
  TYPES_h_data (S, D, T), \
  TYPES_s_data (S, D, T)

/* _s16_s8  _s32_s16  _s64_s32
   _u16_u8  _u32_u16  _u64_u32.  */
#define TYPES_bhs_widen(S, D, T) \
  D (s16, s8), D (s32, s16), D (s64, s32), \
  D (u16, u8), D (u32, u16), D (u64, u32)

/* _mf8.  */
#define TYPES_b_float(S, D, T) \
  S (mf8)

/* _bf16.  */
#define TYPES_h_bfloat(S, D, T) \
  S (bf16)

/* _f16.  */
#define TYPES_h_float(S, D, T) \
  S (f16)

/* _s16
   _u16.  */
#define TYPES_h_integer(S, D, T) \
  S (s16), S (u16)

/* _bf16
   _f16
   _s16
   _u16.  */
#define TYPES_h_data(S, D, T) \
  S (bf16), S (f16), TYPES_h_integer (S, D, T)

/* _s16 _s32.  */
#define TYPES_hs_signed(S, D, T) \
  S (s16), S (s32)

/* _s16 _s32
   _u16 _u32.  */
#define TYPES_hs_integer(S, D, T) \
  TYPES_hs_signed (S, D, T), S (u16), S (u32)

/* _f16 _f32.  */
#define TYPES_hs_float(S, D, T) \
  S (f16), S (f32)

#define TYPES_hs_data(S, D, T) \
  TYPES_h_data (S, D, T), \
  TYPES_s_data (S, D, T)

/* _u16 _u64.  */
#define TYPES_hd_unsigned(S, D, T) \
  S (u16), S (u64)

/* _s16 _s32 _s64.  */
#define TYPES_hsd_signed(S, D, T) \
  S (s16), S (s32), S (s64)

/* _s16 _s32 _s64
   _u16 _u32 _u64.  */
#define TYPES_hsd_integer(S, D, T) \
  TYPES_hsd_signed (S, D, T), S (u16), S (u32), S (u64)

#define TYPES_hsd_data(S, D, T) \
  TYPES_h_data (S, D, T), \
  TYPES_s_data (S, D, T), \
  TYPES_d_data (S, D, T)

/* _f16_mf8.  */
#define TYPES_h_float_mf8(S, D, T) \
  D (f16, mf8)

/* _f32.  */
#define TYPES_s_float(S, D, T) \
  S (f32)

/* _f32_mf8.  */
#define TYPES_s_float_mf8(S, D, T) \
  D (f32, mf8)

/*      _f32
   _s16 _s32 _s64
   _u16 _u32 _u64.  */
#define TYPES_s_float_hsd_integer(S, D, T) \
  TYPES_s_float (S, D, T), TYPES_hsd_integer (S, D, T)

/* _f32
   _s32 _s64
   _u32 _u64.  */
#define TYPES_s_float_sd_integer(S, D, T) \
  TYPES_s_float (S, D, T), TYPES_sd_integer (S, D, T)

/* _s32.  */
#define TYPES_s_signed(S, D, T) \
  S (s32)

/* _u32.  */
#define TYPES_s_unsigned(S, D, T) \
  S (u32)

/* _s32
   _u32.  */
#define TYPES_s_integer(S, D, T) \
  TYPES_s_signed (S, D, T), TYPES_s_unsigned (S, D, T)

/* _f32
   _s32
   _u32.  */
#define TYPES_s_data(S, D, T) \
  TYPES_s_float (S, D, T), TYPES_s_integer (S, D, T)

/* _s32 _s64.  */
#define TYPES_sd_signed(S, D, T) \
  S (s32), S (s64)

/* _u32 _u64.  */
#define TYPES_sd_unsigned(S, D, T) \
  S (u32), S (u64)

/* _s32 _s64
   _u32 _u64.  */
#define TYPES_sd_integer(S, D, T) \
  TYPES_sd_signed (S, D, T), TYPES_sd_unsigned (S, D, T)

#define TYPES_sd_data(S, D, T) \
  TYPES_s_data (S, D, T), \
  TYPES_d_data (S, D, T)

/* _f16 _f32 _f64
	_s32 _s64
	_u32 _u64.  */
#define TYPES_all_float_and_sd_integer(S, D, T) \
  TYPES_all_float (S, D, T), TYPES_sd_integer (S, D, T)

/* _f64.  */
#define TYPES_d_float(S, D, T) \
  S (f64)

/* _u64.  */
#define TYPES_d_unsigned(S, D, T) \
  S (u64)

/* _s64
   _u64.  */
#define TYPES_d_integer(S, D, T) \
  S (s64), TYPES_d_unsigned (S, D, T)

/* _f64
   _s64
   _u64.  */
#define TYPES_d_data(S, D, T) \
  TYPES_d_float (S, D, T), TYPES_d_integer (S, D, T)

/* All the type combinations allowed by svcvt.  */
#define TYPES_cvt(S, D, T) \
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

/* _bf16_f32.  */
#define TYPES_cvt_bfloat(S, D, T) \
  D (bf16, f32)

/* { _bf16 _f16 } x _f32.  */
#define TYPES_cvt_h_s_float(S, D, T) \
  D (bf16, f32), D (f16, f32)

/* _f32_f16.  */
#define TYPES_cvt_f32_f16(S, D, T) \
  D (f32, f16)

/* _f32_f16
   _f64_f32.  */
#define TYPES_cvt_long(S, D, T) \
  D (f32, f16), D (f64, f32)

/* _f32_f64.  */
#define TYPES_cvt_narrow_s(S, D, T) \
  D (f32, f64)

/* _f16_f32
   _f32_f64.  */
#define TYPES_cvt_narrow(S, D, T) \
  D (f16, f32), TYPES_cvt_narrow_s (S, D, T)

/* { _s32 _u32 } x _f32

   _f32 x { _s32 _u32 }.  */
#define TYPES_cvt_s_s(S, D, T) \
  D (s32, f32), \
  D (u32, f32), \
  D (f32, s32), \
  D (f32, u32)

/* _f16_mf8
   _bf16_mf8.  */
#define TYPES_cvt_mf8(S, D, T) \
  D (f16, mf8), D (bf16, mf8)

/* _mf8_f16
   _mf8_bf16.  */
#define TYPES_cvtn_mf8(S, D, T) \
  D (mf8, f16), D (mf8, bf16)

/* _mf8_f32.  */
#define TYPES_cvtnx_mf8(S, D, T) \
  D (mf8, f32)

#define TYPES_cvtzn(S, D, T) \
  D (s8, f16), \
  D (u8, f16), \
  D (s16, f32), \
  D (u16, f32), \
  D (s32, f64), \
  D (u32, f64)

#define TYPES_cvttb(S, D, T) \
  D (f16, s8), \
  D (f16, u8), \
  D (f32, s16), \
  D (f32, u16), \
  D (f64, s32), \
  D (f64, u32)

/* { _s32 _s64 } x { _b8 _b16 _b32 _b64 }
   { _u32 _u64 }.  */
#define TYPES_inc_dec_n1(D, A) \
  D (A, b8), D (A, b16), D (A, b32), D (A, b64)
#define TYPES_inc_dec_n(S, D, T) \
  TYPES_inc_dec_n1 (D, s32), \
  TYPES_inc_dec_n1 (D, s64), \
  TYPES_inc_dec_n1 (D, u32), \
  TYPES_inc_dec_n1 (D, u64)

/* { _s16 _u16 } x _s32

   {      _u16 } x _u32.  */
#define TYPES_qcvt_x2(S, D, T) \
  D (s16, s32), \
  D (u16, u32), \
  D (u16, s32)

/* { _s8  _u8  } x _s32

   {      _u8  } x _u32

   { _s16 _u16 } x _s64

   {      _u16 } x _u64.  */
#define TYPES_qcvt_x4(S, D, T) \
  D (s8, s32), \
  D (u8, u32), \
  D (u8, s32), \
  D (s16, s64), \
  D (u16, u64), \
  D (u16, s64)

/* _s16_s32
   _u16_u32.  */
#define TYPES_qrshr_x2(S, D, T) \
  D (s16, s32), \
  D (u16, u32)

/* _s8_s16
   _u8_u16
   _s16_s32
   _u16_u32.  */
#define TYPES_qshr_x2(S, D, T) \
  D (s8, s16), \
  D (u8, u16), \
  D (s16, s32), \
  D (u16, u32)

/* _s8_s16
   _u8_u16.  */
#define TYPES_qrshr_x2_8(S, D, T) \
  D (s8, s16), \
  D (u8, u16)

/* _u16_s32.  */
#define TYPES_qrshru_x2(S, D, T) \
  D (u16, s32)

/* _u8_s16
   _u16_s32.  */
#define TYPES_qshru_x2(S, D, T) \
  D (u8, s16), \
  D (u16, s32)

/* _u8_s16.  */
#define TYPES_qrshrun_x2(S, D, T) \
  D (u8, s16)

/* _s8_s32
   _s16_s64
   _u8_u32
   _u16_u64.  */
#define TYPES_qrshr_x4(S, D, T) \
  D (s8, s32), \
  D (s16, s64), \
  D (u8, u32), \
  D (u16, u64)

/* _u8_s32
   _u16_s64.  */
#define TYPES_qrshru_x4(S, D, T) \
  D (u8, s32), \
  D (u16, s64)

/* { _mf8 _bf16		 }   { _mf8 _bf16	   }
   {      _f16 _f32 _f64 }   {      _f16 _f32 _f64 }
   { _s8  _s16 _s32 _s64 } x { _s8  _s16 _s32 _s64 }
   { _u8  _u16 _u32 _u64 }   { _u8  _u16 _u32 _u64 }.  */
#define TYPES_reinterpret1(D, A) \
  D (A, mf8), \
  D (A, bf16), \
  D (A, f16), D (A, f32), D (A, f64), \
  D (A, s8), D (A, s16), D (A, s32), D (A, s64), \
  D (A, u8), D (A, u16), D (A, u32), D (A, u64)
#define TYPES_reinterpret(S, D, T) \
  TYPES_reinterpret1 (D, mf8), \
  TYPES_reinterpret1 (D, bf16), \
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

/* _b_c
   _c_b.  */
#define TYPES_reinterpret_b(S, D, T) \
  D (b, c), \
  D (c, b)

/* { _b8 _b16 _b32 _b64 } x { _s32 _s64 }
			    { _u32 _u64 } */
#define TYPES_while1(D, bn) \
  D (bn, s32), D (bn, s64), D (bn, u32), D (bn, u64)
#define TYPES_while(S, D, T) \
  TYPES_while1 (D, b8), \
  TYPES_while1 (D, b16), \
  TYPES_while1 (D, b32), \
  TYPES_while1 (D, b64)

/* { _b8 _b16 _b32 _b64 } x { _s64 }
			    { _u64 } */
#define TYPES_while_x(S, D, T) \
  D (b8, s64), D (b8, u64), \
  D (b16, s64), D (b16, u64), \
  D (b32, s64), D (b32, u64), \
  D (b64, s64), D (b64, u64)

/* { _c8 _c16 _c32 _c64 } x { _s64 }
			    { _u64 } */
#define TYPES_while_x_c(S, D, T) \
  D (c8, s64), D (c8, u64), \
  D (c16, s64), D (c16, u64), \
  D (c32, s64), D (c32, u64), \
  D (c64, s64), D (c64, u64)

/* _f32_f16
   _s32_s16
   _u32_u16.  */
#define TYPES_s_narrow_fsu(S, D, T) \
  D (f32, f16), D (s32, s16), D (u32, u16)

/* _s16_s8
   _u16_u8.  */
#define TYPES_s_narrow_su(S, D, T) \
  D (s16, s8), D (u16, u8)

/* _za8 _za16 _za32 _za64 _za128.  */
#define TYPES_all_za(S, D, T) \
  S (za8), S (za16), S (za32), S (za64), S (za128)

/* _za64.  */
#define TYPES_d_za(S, D, T) \
  S (za64)

/* {   _za8 } x {  _mf8       _s8  _u8 }
   {  _za16 } x { _bf16 _f16 _s16 _u16 }
   {  _za32 } x {       _f32 _s32 _u32 }
   {  _za64 } x {       _f64 _s64 _u64 }.  */
#define TYPES_za_bhsd_data(S, D, T) \
  D (za8, mf8), D (za8, s8), D (za8, u8), \
  D (za16, bf16), D (za16, f16), D (za16, s16), D (za16, u16), \
  D (za32, f32), D (za32, s32), D (za32, u32), \
  D (za64, f64), D (za64, s64), D (za64, u64)

/* Likewise, plus:

   { _za128 } x {      _bf16	       }
		{       _f16 _f32 _f64 }
		{ _s8   _s16 _s32 _s64 }
		{ _u8   _u16 _u32 _u64 }.  */

#define TYPES_za_all_data(S, D, T) \
  TYPES_za_bhsd_data (S, D, T), \
  TYPES_reinterpret1 (D, za128)

/* _za16_mf8.  */
#define TYPES_za_h_mf8(S, D, T) \
  D (za16, mf8)

/* { _za_16 _za_32 } x _mf8.  */
#define TYPES_za_hs_mf8(S, D, T) \
  D (za16, mf8), D (za32, mf8)

/* _za16_bf16.  */
#define TYPES_za_h_bfloat(S, D, T) \
  D (za16, bf16)

/* _za16_f16.  */
#define TYPES_za_h_float(S, D, T) \
  D (za16, f16)

/* _za32_s8.  */
#define TYPES_za_s_b_signed(S, D, T) \
   D (za32, s8)

/* _za32_u8.  */
#define TYPES_za_s_b_unsigned(S, D, T) \
   D (za32, u8)

/* _za32 x { _s8 _u8 }.  */
#define TYPES_za_s_b_integer(S, D, T) \
  D (za32, s8), D (za32, u8)

/* _za32 x { _s16 _u16 }.  */
#define TYPES_za_s_h_integer(S, D, T) \
  D (za32, s16), D (za32, u16)

/* _za32 x { _bf16 _f16 _s16 _u16 }.  */
#define TYPES_za_s_h_data(S, D, T) \
  D (za32, bf16), D (za32, f16), D (za32, s16), D (za32, u16)

/* _za32_u32.  */
#define TYPES_za_s_unsigned(S, D, T) \
  D (za32, u32)

/* _za32 x { _s32 _u32 }.  */
#define TYPES_za_s_integer(S, D, T) \
  D (za32, s32), D (za32, u32)

/* _za32_mf8.  */
#define TYPES_za_s_mf8(S, D, T) \
  D (za32, mf8)

/* _za32_f32.  */
#define TYPES_za_s_float(S, D, T) \
  D (za32, f32)

/* _za32 x { _f32 _s32 _u32 }.  */
#define TYPES_za_s_data(S, D, T) \
  D (za32, f32), D (za32, s32), D (za32, u32)

/* _za64 x { _s16 _u16 }.  */
#define TYPES_za_d_h_integer(S, D, T) \
  D (za64, s16), D (za64, u16)

/* _za64_f64.  */
#define TYPES_za_d_float(S, D, T) \
  D (za64, f64)

/* _za64 x { _s64 _u64 }.  */
#define TYPES_za_d_integer(S, D, T) \
  D (za64, s64), D (za64, u64)

/* _za32 x { _s8 _u8 _bf16 _f16 _f32 }.  */
#define TYPES_mop_base(S, D, T) \
  D (za32, s8), D (za32, u8), D (za32, bf16), D (za32, f16), D (za32, f32)

/* _za32_s8.  */
#define TYPES_mop_base_signed(S, D, T) \
  D (za32, s8)

/* _za32_u8.  */
#define TYPES_mop_base_unsigned(S, D, T) \
  D (za32, u8)

/* _za64 x { _s16 _u16 }.  */
#define TYPES_mop_i16i64(S, D, T) \
  D (za64, s16), D (za64, u16)

/* _za64_s16.  */
#define TYPES_mop_i16i64_signed(S, D, T) \
  D (za64, s16)

/* _za64_u16.  */
#define TYPES_mop_i16i64_unsigned(S, D, T) \
  D (za64, u16)

// svmop4a[_1x1]_za32[_f32]
// svmop4a[_1x1]_za32[_f16_f16]
// svmop4a[_1x1]_za32[_bf16_bf16]
// svmop4a[_1x1]_za32[_s16_s16]
// svmop4a[_1x1]_za32[_u16_u16]
// svmop4a[_1x1]_za32[_s8_s8]
// svmop4a[_1x1]_za32[_u8_u8]
// svmop4a[_1x1]_za32[_s8_u8]
// svmop4a[_1x1]_za32[_u8_s8]
#define TYPES_mop4_base(S, D, T) \
  T (za32, f32, f32), \
  T (za32, f16, f16), \
  T (za32, bf16, bf16), \
  T (za32, s16, s16), \
  T (za32, u16, u16), \
  T (za32, s8, s8), \
  T (za32, u8, u8), \
  T (za32, s8, u8), \
  T (za32, u8, s8)

// svmop4a[_1x1]_za16[_bf16_bf16] (only if __ARM_FEATURE_SME_B16B16 != 0)
#define TYPES_mop4_b16b16(S, D, T) \
  T (za16, bf16, bf16)

// svmop4a[_1x1]_za16[_mf8_mf8]_fpm (only if __ARM_FEATURE_SME_F8F16 != 0)
#define TYPES_mop4_f8f16(S, D, T) \
  T (za16, mf8, mf8)

// svmop4a[_1x1]_za32[_mf8_mf8]_fpm (only if __ARM_FEATURE_SME_F8F32 != 0)
#define TYPES_mop4_f8f32(S, D, T) \
  T (za32, mf8, mf8)

// svmop4a[_1x1]_za16[_f16_f16] (only if __ARM_FEATURE_SME_F16F16 != 0)
#define TYPES_mop4_f16f16(S, D, T) \
  T (za16, f16, f16)

// svmop4a[_1x1]_za64[_f64_f64] (only if __ARM_FEATURE_SME_F64F64 != 0)
#define TYPES_mop4_f64f64(S, D, T) \
  T (za64, f64, f64)

// svmop4a[_1x1]_za64[_s16_s16] (only if __ARM_FEATURE_SME_I16I64 != 0)
// svmop4a[_1x1]_za64[_u16_u16] (only if __ARM_FEATURE_SME_I16I64 != 0)
// svmop4a[_1x1]_za64[_s16_u16] (only if __ARM_FEATURE_SME_I16I64 != 0)
// svmop4a[_1x1]_za64[_u16_s16] (only if __ARM_FEATURE_SME_I16I64 != 0)
#define TYPES_mop4_i16i64(S, D, T) \
  T (za64, s16, s16), \
  T (za64, u16, u16), \
  T (za64, s16, u16), \
  T (za64, u16, s16)

/* _za.  */
#define TYPES_za(S, D, T) \
  S (za)

/* _p16 _s16 _u16 _f16 _bf16.  */
#define TYPES_h_neon(S, D, T) \
  S (p16), S (s16), S (u16), S (f16), S (bf16)

/* _p8  _s8  _u8  _mf8
   _p16 _s16 _u16 _f16 _bf16
	_s32 _u32 _f32
   _p64 _s64 _u64 _f64.  */
#define TYPES_all_neon(S, D, T) \
  TYPES_bhsd_neon (S, D, T), \
  TYPES_h_bfloat (S, D, T)

/* _p8 _s8 _u8.  */
#define TYPES_b_neon(S, D, T) \
  S (p8), S (s8), S (u8)

/* _p8.  */
#define TYPES_b_poly(S, D, T) \
  S (p8)

/* _p8 _p16 _p64.  */
#define TYPES_bhd_poly(S, D, T) \
  S (p8), S (p16), S (p64)

/* _p8 _p16.  */
#define TYPES_bh_poly(S, D, T) \
  S (p8), S (p16)

/* _p16.  */
#define TYPES_h_poly(S, D, T) \
  S (p16)

/* _p8 _p16 _p64 _p128.  */
#define TYPES_bhdq_poly(S, D, T) \
  S (p8), S (p16), S (p64), S (p128)

/* _p8  _s8  _u8  _mf8
   _p16 _s16 _u16 _f16
	_s32 _u32 _f32.  */
#define TYPES_bhs_neon(S, D, T) \
  TYPES_bh_poly (S, D, T), S (mf8), \
  TYPES_bhs_integer (S, D, T), \
  TYPES_hs_float (S, D, T)

/* _p8  _s8  _u8  _mf8
   _p16 _s16 _u16 _f16
	_s32 _u32 _f32
   _p64 _s64 _u64 _f64.  */
#define TYPES_bhsd_neon(S, D, T) \
  TYPES_bhd_poly (S, D, T), S (mf8), \
  TYPES_all_integer (S, D, T), \
  TYPES_all_float (S, D, T)

/* _p8  _s8  _u8  _mf8
   _p16 _s16 _u16 _bf16
	_s32 _u32 _f32
   _p64 _s64 _u64 _f64.  */
#define TYPES_neon_copy_lane(S, D, T) \
  TYPES_bhd_poly (S, D, T), \
  TYPES_all_integer (S, D, T), \
  S (mf8), S (bf16), S (f32), S (f64)

/* _p8 _s8 _u8.  */
#define TYPES_b_neon(S, D, T) \
  S (p8), S (s8), S (u8)

/* _p8 _s8 _u8 _mf8.  */
#define TYPES_neon_rev16(S, D, T) \
  S (p8), S (s8), S (u8), S (mf8)

/* _p8  _s8  _u8  _mf8
   _p16 _s16 _u16.  */
#define TYPES_neon_rev32(S, D, T) \
  S (p8),  S (s8),  S (u8), S (mf8), \
  S (p16), S (s16), S (u16)

/* _p8  _s8  _u8  _mf8
   _p16 _s16 _u16 _f16
	_s32 _u32 _f32.  */
#define TYPES_neon_rev64(S, D, T) \
  S (p8),  S (s8),  S (u8),  S (mf8), \
  S (p16), S (s16), S (u16), S (f16), \
	   S (s32), S (u32), S (f32)

/* vreinterpret intrinsics are defined for any pair of element types.
   { _mf8		 }   { _mf8		   }
   {     _bf16		 }   {     _bf16	   }
   {      _f16 _f32 _f64 }   {      _f16 _f32 _f64 }
   { _s8  _s16 _s32 _s64 } x { _s8  _s16 _s32 _s64 }
   { _u8  _u16 _u32 _u64 }   { _u8  _u16 _u32 _u64 }
   { _p8  _p16      _p64 }   { _p8  _p16      _p64 }.  */
#define TYPES_neon_reinterpret1(D, A) \
  D (A, mf8), \
	     D (A, bf16), \
	     D (A, f16), D (A, f32), D (A, f64), \
  D (A, s8), D (A, s16), D (A, s32), D (A, s64), \
  D (A, u8), D (A, u16), D (A, u32), D (A, u64), \
  D (A, p8), D (A, p16)		   , D (A, p64)
#define TYPES_neon_reinterpret(S, D, T) \
  TYPES_neon_reinterpret1 (D, mf8), \
  TYPES_neon_reinterpret1 (D, bf16), \
  TYPES_neon_reinterpret1 (D, f16), \
  TYPES_neon_reinterpret1 (D, f32), \
  TYPES_neon_reinterpret1 (D, f64), \
  TYPES_neon_reinterpret1 (D, s8), \
  TYPES_neon_reinterpret1 (D, s16), \
  TYPES_neon_reinterpret1 (D, s32), \
  TYPES_neon_reinterpret1 (D, s64), \
  TYPES_neon_reinterpret1 (D, u8), \
  TYPES_neon_reinterpret1 (D, u16), \
  TYPES_neon_reinterpret1 (D, u32), \
  TYPES_neon_reinterpret1 (D, u64), \
  TYPES_neon_reinterpret1 (D, p8), \
  TYPES_neon_reinterpret1 (D, p16), \
  TYPES_neon_reinterpret1 (D, p64)

/* vreinterpretq intrinsics are additionally defined for p128.
   {     _bf16		       }   {     _bf16		       }
   {      _f16 _f32 _f64       }   {      _f16 _f32 _f64       }
   { _mf8		       }   { _mf8		       }
   { _s8  _s16 _s32 _s64       } x { _s8  _s16 _s32 _s64       }
   { _u8  _u16 _u32 _u64       }   { _u8  _u16 _u32 _u64       }
   { _p8  _p16      _p64 _p128 }   { _p8  _p16      _p64 _p128 }.  */
#define TYPES_neon_reinterpretq1(D, A) \
  D (A, mf8), \
	     D (A, bf16), \
	     D (A, f16), D (A, f32), D (A, f64), \
  D (A, s8), D (A, s16), D (A, s32), D (A, s64), \
  D (A, u8), D (A, u16), D (A, u32), D (A, u64), \
  D (A, p8), D (A, p16)		   , D (A, p64), D (A, p128)
#define TYPES_neon_reinterpretq(S, D, T) \
  TYPES_neon_reinterpretq1 (D, mf8), \
  TYPES_neon_reinterpretq1 (D, bf16), \
  TYPES_neon_reinterpretq1 (D, f16), \
  TYPES_neon_reinterpretq1 (D, f32), \
  TYPES_neon_reinterpretq1 (D, f64), \
  TYPES_neon_reinterpretq1 (D, s8), \
  TYPES_neon_reinterpretq1 (D, s16), \
  TYPES_neon_reinterpretq1 (D, s32), \
  TYPES_neon_reinterpretq1 (D, s64), \
  TYPES_neon_reinterpretq1 (D, u8), \
  TYPES_neon_reinterpretq1 (D, u16), \
  TYPES_neon_reinterpretq1 (D, u32), \
  TYPES_neon_reinterpretq1 (D, u64), \
  TYPES_neon_reinterpretq1 (D, p8), \
  TYPES_neon_reinterpretq1 (D, p16), \
  TYPES_neon_reinterpretq1 (D, p64), \
  TYPES_neon_reinterpretq1 (D, p128)

/* Describe a tuple of type suffixes in which only the first is used.  */
#define DEF_VECTOR_TYPE(X) \
  { TYPE_SUFFIX_ ## X, NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES }

/* Describe a tuple of type suffixes in which only the first two are used.  */
#define DEF_DOUBLE_TYPE(X, Y) \
  { TYPE_SUFFIX_ ## X, TYPE_SUFFIX_ ## Y, NUM_TYPE_SUFFIXES }

/* Describe a tuple of type suffixes in which three elements are used.  */
#define DEF_TRIPLE_TYPE(X, Y, Z) \
  { TYPE_SUFFIX_ ## X, TYPE_SUFFIX_ ## Y, TYPE_SUFFIX_ ## Z }

/* Create an array that can be used in aarch64-sve-builtins.def to
   select the type suffixes in TYPES_<NAME>.  */
#define DEF_SVE_TYPES_ARRAY(NAME) \
  static const type_suffix_triple types_##NAME[] = { \
    TYPES_##NAME (DEF_VECTOR_TYPE, DEF_DOUBLE_TYPE, DEF_TRIPLE_TYPE), \
    { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES } \
  }

/* For functions that don't take any type suffixes.  */
static const type_suffix_triple types_none[] = {
  { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES },
  { NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES, NUM_TYPE_SUFFIXES }
};

/* Create an array for each TYPES_<combination> macro above.  */
DEF_SVE_TYPES_ARRAY (all_pred);
DEF_SVE_TYPES_ARRAY (all_count);
DEF_SVE_TYPES_ARRAY (all_pred_count);
DEF_SVE_TYPES_ARRAY (all_float);
DEF_SVE_TYPES_ARRAY (sd_float);
DEF_SVE_TYPES_ARRAY (all_signed);
DEF_SVE_TYPES_ARRAY (all_float_and_signed);
DEF_SVE_TYPES_ARRAY (all_unsigned);
DEF_SVE_TYPES_ARRAY (all_integer);
DEF_SVE_TYPES_ARRAY (all_arith);
DEF_SVE_TYPES_ARRAY (all_arith_no_fp16);
DEF_SVE_TYPES_ARRAY (all_data);
DEF_SVE_TYPES_ARRAY (b);
DEF_SVE_TYPES_ARRAY (b_unsigned);
DEF_SVE_TYPES_ARRAY (b_integer);
DEF_SVE_TYPES_ARRAY (bh_integer);
DEF_SVE_TYPES_ARRAY (bs_unsigned);
DEF_SVE_TYPES_ARRAY (bhs_signed);
DEF_SVE_TYPES_ARRAY (bhs_unsigned);
DEF_SVE_TYPES_ARRAY (bhs_integer);
DEF_SVE_TYPES_ARRAY (bh_data);
DEF_SVE_TYPES_ARRAY (bhs_data);
DEF_SVE_TYPES_ARRAY (bhs_widen);
DEF_SVE_TYPES_ARRAY (c);
DEF_SVE_TYPES_ARRAY (h_bfloat);
DEF_SVE_TYPES_ARRAY (h_float);
DEF_SVE_TYPES_ARRAY (h_float_mf8);
DEF_SVE_TYPES_ARRAY (h_integer);
DEF_SVE_TYPES_ARRAY (h_data);
DEF_SVE_TYPES_ARRAY (hs_signed);
DEF_SVE_TYPES_ARRAY (hs_integer);
DEF_SVE_TYPES_ARRAY (hs_float);
DEF_SVE_TYPES_ARRAY (hs_data);
DEF_SVE_TYPES_ARRAY (hd_unsigned);
DEF_SVE_TYPES_ARRAY (hsd_signed);
DEF_SVE_TYPES_ARRAY (hsd_integer);
DEF_SVE_TYPES_ARRAY (hsd_data);
DEF_SVE_TYPES_ARRAY (s_float);
DEF_SVE_TYPES_ARRAY (s_float_hsd_integer);
DEF_SVE_TYPES_ARRAY (s_float_mf8);
DEF_SVE_TYPES_ARRAY (s_float_sd_integer);
DEF_SVE_TYPES_ARRAY (s_signed);
DEF_SVE_TYPES_ARRAY (s_unsigned);
DEF_SVE_TYPES_ARRAY (s_integer);
DEF_SVE_TYPES_ARRAY (s_data);
DEF_SVE_TYPES_ARRAY (sd_signed);
DEF_SVE_TYPES_ARRAY (sd_unsigned);
DEF_SVE_TYPES_ARRAY (sd_integer);
DEF_SVE_TYPES_ARRAY (sd_data);
DEF_SVE_TYPES_ARRAY (all_float_and_sd_integer);
DEF_SVE_TYPES_ARRAY (d_float);
DEF_SVE_TYPES_ARRAY (d_unsigned);
DEF_SVE_TYPES_ARRAY (d_integer);
DEF_SVE_TYPES_ARRAY (d_data);
DEF_SVE_TYPES_ARRAY (cvt);
DEF_SVE_TYPES_ARRAY (cvt_bfloat);
DEF_SVE_TYPES_ARRAY (cvt_h_s_float);
DEF_SVE_TYPES_ARRAY (cvt_f32_f16);
DEF_SVE_TYPES_ARRAY (cvt_long);
DEF_SVE_TYPES_ARRAY (cvt_mf8);
DEF_SVE_TYPES_ARRAY (cvt_narrow_s);
DEF_SVE_TYPES_ARRAY (cvt_narrow);
DEF_SVE_TYPES_ARRAY (cvt_s_s);
DEF_SVE_TYPES_ARRAY (cvtn_mf8);
DEF_SVE_TYPES_ARRAY (cvtnx_mf8);
DEF_SVE_TYPES_ARRAY (cvtzn);
DEF_SVE_TYPES_ARRAY (cvttb);
DEF_SVE_TYPES_ARRAY (inc_dec_n);
DEF_SVE_TYPES_ARRAY (qcvt_x2);
DEF_SVE_TYPES_ARRAY (qcvt_x4);
DEF_SVE_TYPES_ARRAY (qshr_x2);
DEF_SVE_TYPES_ARRAY (qrshr_x2);
DEF_SVE_TYPES_ARRAY (qrshr_x2_8);
DEF_SVE_TYPES_ARRAY (qrshr_x4);
DEF_SVE_TYPES_ARRAY (qrshru_x2);
DEF_SVE_TYPES_ARRAY (qrshrun_x2);
DEF_SVE_TYPES_ARRAY (qshru_x2);
DEF_SVE_TYPES_ARRAY (qrshru_x4);
DEF_SVE_TYPES_ARRAY (reinterpret);
DEF_SVE_TYPES_ARRAY (reinterpret_b);
DEF_SVE_TYPES_ARRAY (while);
DEF_SVE_TYPES_ARRAY (while_x);
DEF_SVE_TYPES_ARRAY (while_x_c);
DEF_SVE_TYPES_ARRAY (s_narrow_fsu);
DEF_SVE_TYPES_ARRAY (s_narrow_su);
DEF_SVE_TYPES_ARRAY (all_za);
DEF_SVE_TYPES_ARRAY (d_za);
DEF_SVE_TYPES_ARRAY (za_bhsd_data);
DEF_SVE_TYPES_ARRAY (za_all_data);
DEF_SVE_TYPES_ARRAY (za_h_mf8);
DEF_SVE_TYPES_ARRAY (za_h_bfloat);
DEF_SVE_TYPES_ARRAY (za_h_float);
DEF_SVE_TYPES_ARRAY (za_s_b_signed);
DEF_SVE_TYPES_ARRAY (za_s_b_unsigned);
DEF_SVE_TYPES_ARRAY (za_s_b_integer);
DEF_SVE_TYPES_ARRAY (za_s_h_integer);
DEF_SVE_TYPES_ARRAY (za_s_h_data);
DEF_SVE_TYPES_ARRAY (za_s_unsigned);
DEF_SVE_TYPES_ARRAY (za_s_integer);
DEF_SVE_TYPES_ARRAY (za_s_mf8);
DEF_SVE_TYPES_ARRAY (za_hs_mf8);
DEF_SVE_TYPES_ARRAY (za_s_float);
DEF_SVE_TYPES_ARRAY (za_s_data);
DEF_SVE_TYPES_ARRAY (za_d_h_integer);
DEF_SVE_TYPES_ARRAY (za_d_float);
DEF_SVE_TYPES_ARRAY (za_d_integer);
DEF_SVE_TYPES_ARRAY (mop_base);
DEF_SVE_TYPES_ARRAY (mop_base_signed);
DEF_SVE_TYPES_ARRAY (mop_base_unsigned);
DEF_SVE_TYPES_ARRAY (mop_i16i64);
DEF_SVE_TYPES_ARRAY (mop_i16i64_signed);
DEF_SVE_TYPES_ARRAY (mop_i16i64_unsigned);
DEF_SVE_TYPES_ARRAY (mop4_f16f16);
DEF_SVE_TYPES_ARRAY (mop4_b16b16);
DEF_SVE_TYPES_ARRAY (mop4_base);
DEF_SVE_TYPES_ARRAY (mop4_f64f64);
DEF_SVE_TYPES_ARRAY (mop4_i16i64);
DEF_SVE_TYPES_ARRAY (mop4_f8f16);
DEF_SVE_TYPES_ARRAY (mop4_f8f32);
DEF_SVE_TYPES_ARRAY (za);

DEF_SVE_TYPES_ARRAY (b_float);
DEF_SVE_TYPES_ARRAY (all_neon);
DEF_SVE_TYPES_ARRAY (b_neon);
DEF_SVE_TYPES_ARRAY (h_neon);
DEF_SVE_TYPES_ARRAY (b_poly);
DEF_SVE_TYPES_ARRAY (h_poly);
DEF_SVE_TYPES_ARRAY (bh_poly);
DEF_SVE_TYPES_ARRAY (bhd_poly);
DEF_SVE_TYPES_ARRAY (bhdq_poly);
DEF_SVE_TYPES_ARRAY (bhs_neon);
DEF_SVE_TYPES_ARRAY (bhsd_neon);
DEF_SVE_TYPES_ARRAY (neon_copy_lane);
DEF_SVE_TYPES_ARRAY (neon_rev16);
DEF_SVE_TYPES_ARRAY (neon_rev32);
DEF_SVE_TYPES_ARRAY (neon_rev64);
DEF_SVE_TYPES_ARRAY (neon_reinterpret);
DEF_SVE_TYPES_ARRAY (neon_reinterpretq);

static const group_suffix_index groups_none[] = {
  GROUP_none, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_x2[] = { GROUP_x2, NUM_GROUP_SUFFIXES };

static const group_suffix_index groups_x12[] = {
  GROUP_none, GROUP_x2, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_x4[] = { GROUP_x4, NUM_GROUP_SUFFIXES };

static const group_suffix_index groups_x24[] = {
  GROUP_x2, GROUP_x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_x124[] = {
  GROUP_none, GROUP_x2, GROUP_x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_x1234[] = {
  GROUP_none, GROUP_x2, GROUP_x3, GROUP_x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_vg1x2[] = {
  GROUP_vg1x2, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_vg1x4[] = {
  GROUP_vg1x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_vg1x24[] = {
  GROUP_vg1x2, GROUP_vg1x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_vg2[] = {
  GROUP_vg2x1, GROUP_vg2x2, GROUP_vg2x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_vg4[] = {
  GROUP_vg4x1, GROUP_vg4x2, GROUP_vg4x4, NUM_GROUP_SUFFIXES
};

static const group_suffix_index groups_vg24[] = {
  GROUP_vg2, GROUP_vg4, NUM_GROUP_SUFFIXES
};

/* Used by functions that have no governing predicate.  */
static const predication_index preds_none[] = { PRED_none, NUM_PREDS };

/* Used by functions that have a governing predicate but do not have an
   explicit suffix.  */
static const predication_index preds_implicit[] = { PRED_implicit, NUM_PREDS };

/* Used by functions that only support "_m" predication.  */
static const predication_index preds_m[] = { PRED_m, NUM_PREDS };

/* Used by functions that allow merging and "don't care" predication,
   but are not suitable for predicated MOVPRFX.  */
static const predication_index preds_mx[] = {
  PRED_m, PRED_x, NUM_PREDS
};

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

/* Used by SME instructions that always merge into ZA.  */
static const predication_index preds_za_m[] = { PRED_za_m, NUM_PREDS };

void build_all (function_builder &b, const char *signature,
		const function_group_info &group,
		mode_suffix_index mode_suffix_id,
		bool force_direct_overloads = false);
}

#endif
