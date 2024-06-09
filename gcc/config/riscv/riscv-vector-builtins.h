/* Builtins definitions for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Ju-Zhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#ifndef GCC_RISCV_VECTOR_BUILTINS_H
#define GCC_RISCV_VECTOR_BUILTINS_H

/* The full name of an RVV intrinsic function is the concatenation of:

   - the base name ("vadd", etc.)
   - the operand suffix ("_vv", "_vx", etc.)
   - the type suffix ("_i32m1", "_i32mf2", etc.)
   - the predication suffix ("_tamu", "_tumu", etc.)

   Each piece of information is individually useful, so we retain this
   classification throughout:

   - function_base represents the base name.

   - operand_type_index can be used as an index to get operand suffix.

   - rvv_op_info can be used as an index to get argument suffix.

   - predication_type_index can be used as an index to get predication suffix.

   In addition to its unique full name, a function may have a shorter
   overloaded alias.  This alias removes pieces of the suffixes that
   can be inferred from the arguments, such as by shortening the mode
   suffix or dropping some of the type suffixes.  The base name and the
   predication suffix stay the same.

   - The function_instance class describes contains all properties of each
     individual function. Such these information will be used by
     function_builder, function_base, function_shape, gimple_folder,
     function_expander, etc.

   - The function_builder class provides several helper function to add an
     intrinsic function.

   - The function_shape class describes how that instruction has been presented
     at the language level:

      1. Determine the function name for C and C++ overload function which can
	 be recognized by compiler at language level for each instruction
	 according to members of function_instance (base name, operand suffix,
	 type suffix, predication suffix, etc.).

      2. Specify the arguments type and return type of each function to
	 describe how that instruction has presented at language level.

   - The function_base describes how the underlying instruction behaves.

   The static list of functions uses function_group to describe a group
   of related functions.  The function_builder class is responsible for
   expanding this static description into a list of individual functions
   and registering the associated built-in functions.  function_instance
   describes one of these individual functions in terms of the properties
   described above.

   The classes involved in compiling a function call are:

   - function_resolver, which resolves an overloaded function call to a
     specific function_instance and its associated function decl.

   - function_checker, which checks whether the values of the arguments
     conform to the RVV ISA specification.

   - gimple_folder, which tries to fold a function call at the gimple level

   - function_expander, which expands a function call into rtl instructions

   function_resolver and function_checker operate at the language level
   and so are associated with the function_shape.  gimple_folder and
   function_expander are concerned with the behavior of the function
   and so are associated with the function_base.  */

namespace riscv_vector {

/* Flags that describe what a function might do, in addition to reading
   its arguments and returning a result.  */
static const unsigned int CP_READ_FPCR = 1U << 0;
static const unsigned int CP_RAISE_FP_EXCEPTIONS = 1U << 1;
static const unsigned int CP_READ_MEMORY = 1U << 2;
static const unsigned int CP_WRITE_MEMORY = 1U << 3;
static const unsigned int CP_READ_CSR = 1U << 4;
static const unsigned int CP_WRITE_CSR = 1U << 5;

/* Bit values used to identify required extensions for RVV intrinsics.  */
#define RVV_REQUIRE_RV64BIT (1 << 0)	/* Require RV64.  */
#define RVV_REQUIRE_ELEN_64 (1 << 1)	/* Require TARGET_VECTOR_ELEN_64.  */
#define RVV_REQUIRE_ELEN_FP_32 (1 << 2) /* Require FP ELEN >= 32.  */
#define RVV_REQUIRE_ELEN_FP_64 (1 << 3) /* Require FP ELEN >= 64.  */
#define RVV_REQUIRE_FULL_V (1 << 4) /* Require Full 'V' extension.  */
#define RVV_REQUIRE_MIN_VLEN_64 (1 << 5)	/* Require TARGET_MIN_VLEN >= 64.  */
#define RVV_REQUIRE_ELEN_FP_16 (1 << 6) /* Require FP ELEN >= 32.  */

/* Enumerates the required extensions.  */
enum required_ext
{
  VECTOR_EXT,   /* Vector extension */
  ZVBB_EXT,    /* Cryto vector Zvbb sub-ext */
  ZVBB_OR_ZVKB_EXT, /* Cryto vector Zvbb or zvkb sub-ext */
  ZVBC_EXT,    /* Crypto vector Zvbc sub-ext */
  ZVKG_EXT,    /* Crypto vector Zvkg sub-ext */
  ZVKNED_EXT,  /* Crypto vector Zvkned sub-ext */
  ZVKNHA_OR_ZVKNHB_EXT, /* Crypto vector Zvknh[ab] sub-ext */
  ZVKNHB_EXT,  /* Crypto vector Zvknhb sub-ext */
  ZVKSED_EXT,  /* Crypto vector Zvksed sub-ext */
  ZVKSH_EXT,   /* Crypto vector Zvksh sub-ext */
  XTHEADVECTOR_EXT,   /* XTheadVector extension */
  /* Please update below to isa_name func when add or remove enum type(s).  */
};

static inline const char * reqired_ext_to_isa_name (enum required_ext required)
{
  switch (required)
  {
    case VECTOR_EXT:
      return "v";
    case ZVBB_EXT:
      return "zvbb";
    case ZVBB_OR_ZVKB_EXT:
      return "zvbb or zvkb";
    case ZVBC_EXT:
      return "zvbc";
    case ZVKG_EXT:
      return "zvkg";
    case ZVKNED_EXT:
      return "zvkned";
    case ZVKNHA_OR_ZVKNHB_EXT:
      return "zvknha or zvknhb";
    case ZVKNHB_EXT:
      return "zvknhb";
    case ZVKSED_EXT:
      return "zvksed";
    case ZVKSH_EXT:
      return "zvksh";
    case XTHEADVECTOR_EXT:
      return "xthreadvector";
    default:
      gcc_unreachable ();
  }

  gcc_unreachable ();
}

static inline bool required_extensions_specified (enum required_ext required)
{
  switch (required)
  {
    case VECTOR_EXT:
      return TARGET_VECTOR;;
    case ZVBB_EXT:
      return TARGET_ZVBB;
    case ZVBB_OR_ZVKB_EXT:
      return TARGET_ZVBB || TARGET_ZVKB;
    case ZVBC_EXT:
      return TARGET_ZVBC;
    case ZVKG_EXT:
      return TARGET_ZVKG;
    case ZVKNED_EXT:
      return TARGET_ZVKNED;
    case ZVKNHA_OR_ZVKNHB_EXT:
      return TARGET_ZVKNHA || TARGET_ZVKNHB;
    case ZVKNHB_EXT:
      return TARGET_ZVKNHB;
    case ZVKSED_EXT:
      return TARGET_ZVKSED;
    case ZVKSH_EXT:
      return TARGET_ZVKSH;
    case XTHEADVECTOR_EXT:
      return TARGET_XTHEADVECTOR;
    default:
      gcc_unreachable ();
  }

  gcc_unreachable ();
}

/* Enumerates the RVV operand types.  */
enum operand_type_index
{
  OP_TYPE_none,
#define DEF_RVV_OP_TYPE(NAME) OP_TYPE_##NAME,
#include "riscv-vector-builtins.def"
  NUM_OP_TYPES
};

/* Enumerates the RVV types, together called
   "vector types" for brevity.  */
enum vector_type_index
{
#define DEF_RVV_TYPE(NAME, ABI_NAME, NCHARS, ARGS...) VECTOR_TYPE_##NAME,
#define DEF_RVV_TUPLE_TYPE(NAME, ABI_NAME, NCHARS, ARGS...) VECTOR_TYPE_##NAME,
#include "riscv-vector-builtins.def"
  NUM_VECTOR_TYPES,
  VECTOR_TYPE_INVALID = NUM_VECTOR_TYPES
};

/* Enumerates the RVV governing predication types.  */
enum predication_type_index
{
  PRED_TYPE_none,
#define DEF_RVV_PRED_TYPE(NAME) PRED_TYPE_##NAME,
#include "riscv-vector-builtins.def"
  NUM_PRED_TYPES
};

/* Enumerates the RVV base types.  */
enum rvv_base_type
{
#define DEF_RVV_BASE_TYPE(NAME, ARGS...) RVV_BASE_##NAME,
#include "riscv-vector-builtins.def"
  NUM_BASE_TYPES
};

/* Builtin types that are used to register RVV intrinsics.  */
struct GTY (()) rvv_builtin_types_t
{
  tree vector;
  tree scalar;
  tree vector_ptr;
  tree scalar_ptr;
  tree scalar_const_ptr;
};

/* Builtin suffix that are used to register RVV intrinsics.  */
struct rvv_builtin_suffixes
{
  const char *vector;
  const char *scalar;
  const char *vsetvl;
};

/* Builtin base type used to specify the type of builtin function
   argument or return result.  */
struct function_type_info
{
  enum vector_type_index type_indexes[NUM_BASE_TYPES];
};

/* RVV Builtin argument information.  */
struct rvv_arg_type_info
{
  CONSTEXPR rvv_arg_type_info (rvv_base_type base_type_in)
    : base_type (base_type_in)
  {}
  enum rvv_base_type base_type;

  tree get_scalar_ptr_type (vector_type_index) const;
  tree get_scalar_const_ptr_type (vector_type_index) const;
  vector_type_index get_function_type_index (vector_type_index) const;
  tree get_scalar_type (vector_type_index) const;
  tree get_vector_type (vector_type_index) const;
  tree get_tree_type (vector_type_index) const;
  tree get_tuple_subpart_type (vector_type_index) const;
};

/* Static information for each operand.  */
struct rvv_type_info
{
  enum vector_type_index index;
  uint64_t required_extensions;
};

/* RVV Builtin operands information.  */
struct rvv_op_info
{
  const rvv_type_info *types;
  const operand_type_index op;
  rvv_arg_type_info ret;
  const rvv_arg_type_info *args;
};

class registered_function;
class function_base;
class function_shape;

/* Static information about a set of functions.  */
struct function_group_info
{
  /* Return true if required extension is enabled */
  bool match (required_ext ext_value) const
  {
    switch (ext_value)
    {
      case VECTOR_EXT:
        return TARGET_VECTOR;
      case ZVBB_EXT:
        return TARGET_ZVBB;
      case ZVBB_OR_ZVKB_EXT:
        return (TARGET_ZVBB || TARGET_ZVKB);
      case ZVBC_EXT:
        return TARGET_ZVBC;
      case ZVKG_EXT:
        return TARGET_ZVKG;
      case ZVKNED_EXT:
        return TARGET_ZVKNED;
      case ZVKNHA_OR_ZVKNHB_EXT:
        return (TARGET_ZVKNHA || TARGET_ZVKNHB);
      case ZVKNHB_EXT:
        return TARGET_ZVKNHB;
      case ZVKSED_EXT:
        return TARGET_ZVKSED;
      case ZVKSH_EXT:
        return TARGET_ZVKSH;
      case XTHEADVECTOR_EXT:
	return TARGET_XTHEADVECTOR;
      default:
        gcc_unreachable ();
    }
  }
  /* The base name, as a string.  */
  const char *base_name;

  /* Describes the behavior associated with the function base name.  */
  const function_base *const *base;

  /* The shape of the functions, as described above the class definition.
     It's possible to have entries with the same base name but different
     shapes.  */
  const function_shape *const *shape;

  /* A list of the available operand types, predication types,
     and of the available operand datatype.
     The function supports every combination of the two.
     The list of predication is terminated by two NUM_PRED_TYPES,
     while the list of operand info is terminated by NUM_BASE_TYPES.
     The list of these type suffix is lexicographically ordered based
     on the index value.  */
  const predication_type_index *preds;
  const rvv_op_info ops_infos;
  /* The required extension value, using it to get the enabled flag.  */
  required_ext required_extensions;
};

class GTY ((user)) function_instance
{
public:
  function_instance (const char *, const function_base *,
		     const function_shape *, rvv_type_info,
		     predication_type_index, const rvv_op_info *);

  bool operator== (const function_instance &) const;
  bool operator!= (const function_instance &) const;
  hashval_t hash () const;

  unsigned int call_properties () const;
  bool reads_global_state_p () const;
  bool modifies_global_state_p () const;
  bool could_trap_p () const;

  /* Return true if return type or arguments are floating point type.  */
  bool any_type_float_p () const;

  tree get_return_type () const;
  tree get_arg_type (unsigned opno) const;

  /* The properties of the function.  (The explicit "enum"s are required
     for gengtype.)  */
  const char *base_name;
  const function_base *base;
  const function_shape *shape;
  rvv_type_info type;
  enum predication_type_index pred;
  const rvv_op_info *op_info;
};

/* A class for building and registering function decls.  */
class function_builder
{
public:
  function_builder ();
  ~function_builder ();

  void allocate_argument_types (const function_instance &, vec<tree> &) const;
  void apply_predication (const function_instance &, tree, vec<tree> &) const;
  void add_unique_function (const function_instance &, const function_shape *,
			    tree, vec<tree> &, enum required_ext);
  void add_overloaded_function (const function_instance &,
				const function_shape *,
				enum required_ext);
  void register_function_group (const function_group_info &);
  void append_name (const char *);
  void append_base_name (const char *);
  void append_sew (int);
  void append_nf (int);
  char *finish_name ();

private:
  tree get_attributes (const function_instance &);

  registered_function &add_function (const function_instance &, const char *,
				     tree, tree, bool, const char *,
				     const vec<tree> &, enum required_ext,
				     bool);

  /* True if we should create a separate decl for each instance of an
     overloaded function, instead of using function_builder.  */
  bool m_direct_overloads;

  /* Used for building up function names.  */
  obstack m_string_obstack;
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

/* Return true if the function has no return value.  */
inline bool
function_call_info::function_returns_void_p ()
{
  return TREE_TYPE (TREE_TYPE (fndecl)) == void_type_node;
}

/* A class for folding a gimple function call.  */
class gimple_folder : public function_call_info
{
public:
  gimple_folder (const function_instance &, tree, gimple_stmt_iterator *,
		 gcall *);

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

  void add_input_operand (machine_mode, rtx);
  void add_input_operand (unsigned);
  void add_output_operand (machine_mode, rtx);
  void add_all_one_mask_operand (machine_mode);
  void add_scalar_move_mask_operand (machine_mode);
  void add_vundef_operand (machine_mode);
  void add_fixed_operand (rtx);
  void add_integer_operand (rtx);
  void add_mem_operand (machine_mode, unsigned);

  machine_mode vector_mode (void) const;
  machine_mode index_mode (void) const;
  machine_mode arg_mode (int) const;
  machine_mode mask_mode (void) const;
  machine_mode ret_mode (void) const;

  rtx use_exact_insn (insn_code);
  rtx use_contiguous_load_insn (insn_code);
  rtx use_contiguous_store_insn (insn_code);
  rtx use_compare_insn (rtx_code, insn_code);
  rtx use_ternop_insn (bool, insn_code);
  rtx use_widen_ternop_insn (insn_code);
  rtx use_scalar_move_insn (insn_code);
  rtx generate_insn (insn_code);

  /* The function call expression.  */
  tree exp;

  /* For functions that return a value, this is the preferred location
     of that value.  It could be null or could have a different mode
     from the function return type.  */
  rtx target;

  /* The number of the operands.  */
  int opno;

private:
  /* Used to build up the operands to an instruction.  */
  struct expand_operand m_ops[MAX_RECOG_OPERANDS];
};

/* Provides information about a particular function base name, and handles
   tasks related to the base name.  */
class function_base
{
public:
  /* Return a set of CP_* flags that describe what the function might do,
     in addition to reading its arguments and returning a result.  */
  virtual unsigned int call_properties (const function_instance &) const;

  /* Return true if intrinsics should apply vl operand.  */
  virtual bool apply_vl_p () const;

  /* Return true if intrinsics should apply tail policy operand.  */
  virtual bool apply_tail_policy_p () const;

  /* Return true if intrinsics should apply mask policy operand.  */
  virtual bool apply_mask_policy_p () const;

  /* Return true if intrinsic can be overloaded.  */
  virtual bool can_be_overloaded_p (enum predication_type_index) const;

  /* Return true if intrinsics use mask predication.  */
  virtual bool use_mask_predication_p () const;

  /* Return true if intrinsics has merge operand.  */
  virtual bool has_merge_operand_p () const;

  /* Return true if intrinsics has rounding mode operand.  */
  virtual bool has_rounding_mode_operand_p () const;

  /* Return true if intrinsics maybe require vxrm operand.  */
  virtual bool may_require_vxrm_p () const;

  /* Return true if intrinsics maybe require frm operand.  */
  virtual bool may_require_frm_p () const;

  /* Try to fold the given gimple call.  Return the new gimple statement
     on success, otherwise return null.  */
  virtual gimple *fold (gimple_folder &) const { return NULL; }

  /* Expand the given call into rtl.  Return the result of the function,
     or an arbitrary value if the function doesn't return a result.  */
  virtual rtx expand (function_expander &) const = 0;
};

/* A class for checking that the semantic constraints on a function call are
   satisfied, such as arguments being integer constant expressions with
   a particular range.  The parent class's FNDECL is the decl that was
   called in the original source, before overload resolution.  */
class function_checker : public function_call_info
{
public:
  function_checker (location_t, const function_instance &, tree, tree,
		    unsigned int, tree *);

  machine_mode arg_mode (unsigned int) const;
  machine_mode ret_mode (void) const;
  unsigned int arg_num (void) const;
  bool check (void);

  bool require_immediate (unsigned int, HOST_WIDE_INT, HOST_WIDE_INT) const;
  bool require_immediate_range_or (unsigned int, HOST_WIDE_INT,
				   HOST_WIDE_INT, HOST_WIDE_INT) const;

private:
  bool require_immediate_range (unsigned int, HOST_WIDE_INT,
				HOST_WIDE_INT) const;
  void report_non_ice (unsigned int) const;
  void report_out_of_range (unsigned int, HOST_WIDE_INT, HOST_WIDE_INT,
			    HOST_WIDE_INT) const;
  void report_out_of_range_and_not (unsigned int, HOST_WIDE_INT, HOST_WIDE_INT,
				    HOST_WIDE_INT, HOST_WIDE_INT) const;

  /* The type of the resolved function.  */
  tree m_fntype;

  /* The arguments to the function.  */
  unsigned int m_nargs;
  tree *m_args;
};

/* Classifies functions into "shapes" base on:

   - Base name of the intrinsic function.

   - Operand types list.

   - Argument type list.

   - Predication type list.  */
class function_shape
{
public:
  /* Shape the function name according to function_instance.  */
  virtual char *get_name (function_builder &, const function_instance &,
			  bool) const
    = 0;

  /* Define all functions associated with the given group.  */
  virtual void build (function_builder &, const function_group_info &) const
    = 0;

  /* Check whether the given call is semantically valid.  Return true
   if it is, otherwise report an error and return false.  */
  virtual bool check (function_checker &) const { return true; }
};

extern const char *const operand_suffixes[NUM_OP_TYPES];
extern const rvv_builtin_suffixes type_suffixes[NUM_VECTOR_TYPES + 1];
extern const char *const predication_suffixes[NUM_PRED_TYPES];
extern rvv_builtin_types_t builtin_types[NUM_VECTOR_TYPES + 1];
extern function_instance get_read_vl_instance (void);
extern tree get_read_vl_decl (void);

inline tree
rvv_arg_type_info::get_scalar_type (vector_type_index type_idx) const
{
  return get_function_type_index (type_idx) == VECTOR_TYPE_INVALID
	   ? NULL_TREE
	   : builtin_types[get_function_type_index (type_idx)].scalar;
}

inline tree
rvv_arg_type_info::get_vector_type (vector_type_index type_idx) const
{
  return get_function_type_index (type_idx) == VECTOR_TYPE_INVALID
	   ? NULL_TREE
	   : builtin_types[get_function_type_index (type_idx)].vector;
}

inline bool
function_instance::operator!= (const function_instance &other) const
{
  return !operator== (other);
}

/* Expand the call and return its lhs.  */
inline rtx
function_expander::expand ()
{
  return base->expand (*this);
}

/* Create op and add it into M_OPS and increase OPNO.  */
inline void
function_expander::add_input_operand (machine_mode mode, rtx op)
{
  create_input_operand (&m_ops[opno++], op, mode);
}

/* Create output and add it into M_OPS and increase OPNO.  */
inline void
function_expander::add_output_operand (machine_mode mode, rtx target)
{
  create_output_operand (&m_ops[opno++], target, mode);
}

/* Since we may normalize vop/vop_tu/vop_m/vop_tumu.. into a single patter.
   We add a fake all true mask for the intrinsics that don't need a real mask.
 */
inline void
function_expander::add_all_one_mask_operand (machine_mode mode)
{
  add_input_operand (mode, CONSTM1_RTX (mode));
}

/* Add mask operand for scalar move instruction.  */
inline void
function_expander::add_scalar_move_mask_operand (machine_mode mode)
{
  add_input_operand (mode, gen_scalar_move_mask (mode));
}

/* Add an operand that must be X.  The only way of legitimizing an
   invalid X is to reload the address of a MEM.  */
inline void
function_expander::add_fixed_operand (rtx x)
{
  create_fixed_operand (&m_ops[opno++], x);
}

/* Add an integer operand X.  */
inline void
function_expander::add_integer_operand (rtx x)
{
  create_integer_operand (&m_ops[opno++], INTVAL (x));
}

/* Return the machine_mode of the corresponding vector type.  */
inline machine_mode
function_expander::vector_mode (void) const
{
  return TYPE_MODE (builtin_types[type.index].vector);
}

/* Return the machine_mode of the corresponding index type.  */
inline machine_mode
function_expander::index_mode (void) const
{
  return TYPE_MODE (op_info->args[1].get_tree_type (type.index));
}

/* Return the machine_mode of the corresponding arg type.  */
inline machine_mode
function_expander::arg_mode (int idx) const
{
  return TYPE_MODE (op_info->args[idx].get_tree_type (type.index));
}

/* Return the machine_mode of the corresponding return type.  */
inline machine_mode
function_expander::ret_mode (void) const
{
  return TYPE_MODE (op_info->ret.get_tree_type (type.index));
}

inline machine_mode
function_checker::arg_mode (unsigned int argno) const
{
  return TYPE_MODE (TREE_TYPE (m_args[argno]));
}

inline machine_mode
function_checker::ret_mode () const
{
  return TYPE_MODE (TREE_TYPE (TREE_TYPE (fndecl)));
}

inline unsigned int
function_checker::arg_num () const
{
  return m_nargs;
}

/* Default implementation of function_base::call_properties, with conservatively
   correct behavior for floating-point instructions.  */
inline unsigned int
function_base::call_properties (const function_instance &instance) const
{
  unsigned int flags = 0;
  if (instance.any_type_float_p ())
    return flags | CP_READ_FPCR | CP_RAISE_FP_EXCEPTIONS;
  return flags;
}

/* We choose to apply vl operand by default since most of the intrinsics
   has vl operand.  */
inline bool
function_base::apply_vl_p () const
{
  return true;
}

/* We choose to apply tail policy operand by default since most of the
   intrinsics has tail policy operand.  */
inline bool
function_base::apply_tail_policy_p () const
{
  return true;
}

/* We choose to apply mask policy operand by default since most of the
   intrinsics has mask policy operand.  */
inline bool
function_base::apply_mask_policy_p () const
{
  return true;
}

/* We choose to return true by default since most of the intrinsics use
   mask predication.  */
inline bool
function_base::use_mask_predication_p () const
{
  return true;
}

/* We choose to return true by default since most of the intrinsics use
   has merge operand.  */
inline bool
function_base::has_merge_operand_p () const
{
  return true;
}

/* We choose to return false by default since most of the intrinsics does
   not have rounding mode operand.  */
inline bool
function_base::has_rounding_mode_operand_p () const
{
  return false;
}

/* We choose to return false by default since most of the intrinsics does
   not need frm operand.  */
inline bool
function_base::may_require_frm_p () const
{
  return false;
}

/* We choose to return false by default since most of the intrinsics does
   not need vxrm operand.  */
inline bool
function_base::may_require_vxrm_p () const
{
  return false;
}

/* Since most of intrinsics can be overloaded, we set it true by default.  */
inline bool
function_base::can_be_overloaded_p (enum predication_type_index) const
{
  return true;
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

} // end namespace riscv_vector

#endif
