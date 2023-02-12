/* ACLE support for AArch64 SVE
   Copyright (C) 2018-2023 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_SVE_BUILTINS_H
#define GCC_AARCH64_SVE_BUILTINS_H

/* The full name of an SVE ACLE function is the concatenation of:

   - the base name ("svadd", etc.)
   - the "mode" suffix ("_n", "_index", etc.)
   - the type suffixes ("_s32", "_b8", etc.)
   - the predication suffix ("_x", "_z", etc.)

   Each piece of information is individually useful, so we retain this
   classification throughout:

   - function_base represents the base name

   - mode_suffix_index represents the mode suffix

   - type_suffix_index represents individual type suffixes, while
     type_suffix_pair represents a pair of them

   - prediction_index extends the predication suffix with an additional
     alternative: PRED_implicit for implicitly-predicated operations

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

   The static list of functions uses function_group to describe a group
   of related functions.  The function_builder class is responsible for
   expanding this static description into a list of individual functions
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
namespace aarch64_sve
{
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

/* Enumerates the SVE predicate and (data) vector types, together called
   "vector types" for brevity.  */
enum vector_type_index
{
#define DEF_SVE_TYPE(ACLE_NAME, NCHARS, ABI_NAME, SCALAR_TYPE) \
  VECTOR_TYPE_ ## ACLE_NAME,
#include "aarch64-sve-builtins.def"
  NUM_VECTOR_TYPES
};

/* Classifies the available measurement units for an address displacement.  */
enum units_index
{
  UNITS_none,
  UNITS_bytes,
  UNITS_elements,
  UNITS_vectors
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

  NUM_PREDS
};

/* Classifies element types, based on type suffixes with the bit count
   removed.  */
enum type_class_index
{
  TYPE_bool,
  TYPE_bfloat,
  TYPE_float,
  TYPE_signed,
  TYPE_unsigned,
  NUM_TYPE_CLASSES
};

/* Classifies an operation into "modes"; for example, to distinguish
   vector-scalar operations from vector-vector operations, or to
   distinguish between different addressing modes.  This classification
   accounts for the function suffixes that occur between the base name
   and the first type suffix.  */
enum mode_suffix_index
{
#define DEF_SVE_MODE(NAME, BASE, DISPLACEMENT, UNITS) MODE_##NAME,
#include "aarch64-sve-builtins.def"
  MODE_none
};

/* Enumerates the possible type suffixes.  Each suffix is associated with
   a vector type, but for predicates provides extra information about the
   element size.  */
enum type_suffix_index
{
#define DEF_SVE_TYPE_SUFFIX(NAME, ACLE_TYPE, CLASS, BITS, MODE) \
  TYPE_SUFFIX_ ## NAME,
#include "aarch64-sve-builtins.def"
  NUM_TYPE_SUFFIXES
};

/* Combines two type suffixes.  */
typedef enum type_suffix_index type_suffix_pair[2];

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
  /* True if the suffix is for a boolean type.  */
  unsigned int bool_p : 1;
  unsigned int spare : 12;

  /* The associated vector or predicate mode.  */
  machine_mode vector_mode : 16;
};

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

  /* A list of the available type suffixes, and of the available predication
     types.  The function supports every combination of the two.

     The list of type suffixes is terminated by two NUM_TYPE_SUFFIXES
     while the list of predication types is terminated by NUM_PREDS.
     The list of type suffixes is lexicographically ordered based
     on the index value.  */
  const type_suffix_pair *types;
  const predication_index *preds;

  /* The architecture extensions that the functions require, as a set of
     AARCH64_FL_* flags.  */
  aarch64_feature_flags required_extensions;
};

/* Describes a single fully-resolved function (i.e. one that has a
   unique full name).  */
class GTY((user)) function_instance
{
public:
  function_instance (const char *, const function_base *,
		     const function_shape *, mode_suffix_index,
		     const type_suffix_pair &, predication_index);

  bool operator== (const function_instance &) const;
  bool operator!= (const function_instance &) const;
  hashval_t hash () const;

  unsigned int call_properties () const;
  bool reads_global_state_p () const;
  bool modifies_global_state_p () const;
  bool could_trap_p () const;

  unsigned int vectors_per_tuple () const;
  tree memory_scalar_type () const;
  machine_mode memory_vector_mode () const;

  const mode_suffix_info &mode_suffix () const;
  tree base_vector_type () const;
  tree displacement_vector_type () const;
  units_index displacement_units () const;

  const type_suffix_info &type_suffix (unsigned int) const;
  tree scalar_type (unsigned int) const;
  tree vector_type (unsigned int) const;
  tree tuple_type (unsigned int) const;
  unsigned int elements_per_vq (unsigned int i) const;
  machine_mode vector_mode (unsigned int) const;
  machine_mode gp_mode (unsigned int) const;

  /* The properties of the function.  (The explicit "enum"s are required
     for gengtype.)  */
  const char *base_name;
  const function_base *base;
  const function_shape *shape;
  enum mode_suffix_index mode_suffix_id;
  type_suffix_pair type_suffix_ids;
  enum predication_index pred;
};

class registered_function;

/* A class for building and registering function decls.  */
class function_builder
{
public:
  function_builder ();
  ~function_builder ();

  void add_unique_function (const function_instance &, tree,
			    vec<tree> &, aarch64_feature_flags, bool);
  void add_overloaded_function (const function_instance &,
				aarch64_feature_flags);
  void add_overloaded_functions (const function_group_info &,
				 mode_suffix_index);

  void register_function_group (const function_group_info &);

private:
  void append_name (const char *);
  char *finish_name ();

  char *get_name (const function_instance &, bool);

  tree get_attributes (const function_instance &);

  registered_function &add_function (const function_instance &,
				     const char *, tree, tree,
				     aarch64_feature_flags, bool, bool);

  /* The function type to use for functions that are resolved by
     function_resolver.  */
  tree m_overload_type;

  /* True if we should create a separate decl for each instance of an
     overloaded function, instead of using function_resolver.  */
  bool m_direct_overloads;

  /* Used for building up function names.  */
  obstack m_string_obstack;

  /* Maps all overloaded function names that we've registered so far
     to their associated function_instances.  */
  hash_map<nofree_string_hash, registered_function *> m_overload_names;
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
  enum { SAME_SIZE = 256, HALF_SIZE, QUARTER_SIZE };
  static const type_class_index SAME_TYPE_CLASS = NUM_TYPE_CLASSES;

  function_resolver (location_t, const function_instance &, tree,
		     vec<tree, va_gc> &);

  tree get_vector_type (type_suffix_index);
  const char *get_scalar_type_name (type_suffix_index);
  tree get_argument_type (unsigned int);
  bool scalar_argument_p (unsigned int);

  tree report_no_such_form (type_suffix_index);
  tree lookup_form (mode_suffix_index,
		    type_suffix_index = NUM_TYPE_SUFFIXES,
		    type_suffix_index = NUM_TYPE_SUFFIXES);
  tree resolve_to (mode_suffix_index,
		   type_suffix_index = NUM_TYPE_SUFFIXES,
		   type_suffix_index = NUM_TYPE_SUFFIXES);

  type_suffix_index infer_integer_scalar_type (unsigned int);
  type_suffix_index infer_pointer_type (unsigned int, bool = false);
  type_suffix_index infer_vector_or_tuple_type (unsigned int, unsigned int);
  type_suffix_index infer_vector_type (unsigned int);
  type_suffix_index infer_integer_vector_type (unsigned int);
  type_suffix_index infer_unsigned_vector_type (unsigned int);
  type_suffix_index infer_sd_vector_type (unsigned int);
  type_suffix_index infer_tuple_type (unsigned int);

  bool require_vector_or_scalar_type (unsigned int);

  bool require_vector_type (unsigned int, vector_type_index);
  bool require_matching_vector_type (unsigned int, type_suffix_index);
  bool require_derived_vector_type (unsigned int, unsigned int,
				    type_suffix_index,
				    type_class_index = SAME_TYPE_CLASS,
				    unsigned int = SAME_SIZE);

  bool require_scalar_type (unsigned int, const char *);
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
  bool require_immediate_lane_index (unsigned int, unsigned int = 1);
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

  gimple *redirect_call (const function_instance &);
  gimple *fold_to_cstu (poly_uint64);
  gimple *fold_to_pfalse ();
  gimple *fold_to_ptrue ();
  gimple *fold_to_vl_pred (unsigned int);

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

  bool overlaps_input_p (rtx);

  rtx convert_to_pmode (rtx);
  rtx get_contiguous_base (machine_mode);
  rtx get_fallback_value (machine_mode, unsigned int,
			  unsigned int, unsigned int &);
  rtx get_reg_target ();
  rtx get_nonoverlapping_reg_target ();

  void add_output_operand (insn_code);
  void add_input_operand (insn_code, rtx);
  void add_integer_operand (HOST_WIDE_INT);
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
  rtx use_contiguous_load_insn (insn_code);
  rtx use_contiguous_prefetch_insn (insn_code);
  rtx use_contiguous_store_insn (insn_code);

  rtx map_to_rtx_codes (rtx_code, rtx_code, int,
			unsigned int = DEFAULT_MERGE_ARGNO);
  rtx map_to_unspecs (int, int, int, unsigned int = DEFAULT_MERGE_ARGNO);

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
  virtual unsigned int vectors_per_tuple () const { return 1; }

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
  virtual bool explicit_type_suffix_p (unsigned int) const = 0;

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

/* RAII class for enabling enough SVE features to define the built-in
   types and implement the arm_sve.h pragma.  */
class sve_switcher : public aarch64_simd_switcher
{
public:
  sve_switcher ();
  ~sve_switcher ();

private:
  unsigned int m_old_maximum_field_alignment;
  bool m_old_have_regs_of_mode[MAX_MACHINE_MODE];
};

extern const type_suffix_info type_suffixes[NUM_TYPE_SUFFIXES + 1];
extern const mode_suffix_info mode_suffixes[MODE_none + 1];

extern tree scalar_types[NUM_VECTOR_TYPES];
extern tree acle_vector_types[MAX_TUPLE_SIZE][NUM_VECTOR_TYPES + 1];
extern tree acle_svpattern;
extern tree acle_svprfop;

/* Return the ACLE type svbool_t.  */
inline tree
get_svbool_t (void)
{
  return acle_vector_types[0][VECTOR_TYPE_svbool_t];
}

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

inline function_instance::
function_instance (const char *base_name_in,
		   const function_base *base_in,
		   const function_shape *shape_in,
		   mode_suffix_index mode_suffix_id_in,
		   const type_suffix_pair &type_suffix_ids_in,
		   predication_index pred_in)
  : base_name (base_name_in), base (base_in), shape (shape_in),
    mode_suffix_id (mode_suffix_id_in), pred (pred_in)
{
  memcpy (type_suffix_ids, type_suffix_ids_in, sizeof (type_suffix_ids));
}

inline bool
function_instance::operator== (const function_instance &other) const
{
  return (base == other.base
	  && shape == other.shape
	  && mode_suffix_id == other.mode_suffix_id
	  && pred == other.pred
	  && type_suffix_ids[0] == other.type_suffix_ids[0]
	  && type_suffix_ids[1] == other.type_suffix_ids[1]);
}

inline bool
function_instance::operator!= (const function_instance &other) const
{
  return !operator== (other);
}

/* If the function operates on tuples of vectors, return the number
   of vectors in the tuples, otherwise return 1.  */
inline unsigned int
function_instance::vectors_per_tuple () const
{
  return base->vectors_per_tuple ();
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

/* Return the mode of the governing predicate to use when operating on
   type suffix I.  */
inline machine_mode
function_instance::gp_mode (unsigned int i) const
{
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

}

#endif
