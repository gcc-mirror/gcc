/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#define INCLUDE_STRING
#define INCLUDE_ALGORITHM
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "insn-attr.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "output.h"
#include "flags.h"
#include "explow.h"
#include "expr.h"
#include "reload.h"
#include "langhooks.h"
#include "opts.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "dwarf2out.h"
#include "gimple-iterator.h"
#include "tree-vectorizer.h"
#include "aarch64-cost-tables.h"
#include "dumpfile.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tm-constrs.h"
#include "sched-int.h"
#include "target-globals.h"
#include "common/common-target.h"
#include "cfgrtl.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "rtx-vector-builder.h"
#include "intl.h"
#include "expmed.h"
#include "function-abi.h"
#include "gimple-pretty-print.h"
#include "tree-ssa-loop-niter.h"
#include "fractional-cost.h"
#include "rtlanal.h"
#include "tree-dfa.h"
#include "asan.h"
#include "aarch64-feature-deps.h"
#include "config/arm/aarch-common.h"
#include "config/arm/aarch-common-protos.h"
#include "common/config/aarch64/cpuinfo.h"
#include "ssa.h"
#include "except.h"
#include "tree-pass.h"
#include "cfgbuild.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "hash-map.h"

/* This file should be included last.  */
#include "target-def.h"

/* Defined for convenience.  */
#define POINTER_BYTES (POINTER_SIZE / BITS_PER_UNIT)

/* Maximum bytes set for an inline memset expansion.  With -Os use 3 STP
   and 1 MOVI/DUP (same size as a call).  */
#define MAX_SET_SIZE(speed) (speed ? 256 : 96)

/* Flags that describe how a function shares certain architectural state
   with its callers.

   - AARCH64_STATE_SHARED indicates that the function does share the state
     with callers.

   - AARCH64_STATE_IN indicates that the function reads (or might read) the
     incoming state.  The converse is that the function ignores the incoming
     state.

   - AARCH64_STATE_OUT indicates that the function returns new state.
     The converse is that the state on return is the same as it was on entry.

   A function that partially modifies the state treats it as both IN
   and OUT (because the value on return depends to some extent on the
   value on input).  */
constexpr auto AARCH64_STATE_SHARED = 1U << 0;
constexpr auto AARCH64_STATE_IN = 1U << 1;
constexpr auto AARCH64_STATE_OUT = 1U << 2;

/* Enum to distinguish which type of check is to be done in
   aarch64_simd_valid_imm.  */
enum simd_immediate_check {
  AARCH64_CHECK_MOV,
  AARCH64_CHECK_ORR,
  AARCH64_CHECK_AND,
  AARCH64_CHECK_XOR
};

/* Information about a legitimate vector immediate operand.  */
struct simd_immediate_info
{
  enum insn_type { MOV, MVN, INDEX, PTRUE, SVE_MOV };
  enum modifier_type { LSL, MSL };

  simd_immediate_info () {}
  simd_immediate_info (scalar_float_mode, rtx);
  simd_immediate_info (scalar_int_mode, unsigned HOST_WIDE_INT,
		       insn_type = MOV, modifier_type = LSL,
		       unsigned int = 0);
  simd_immediate_info (scalar_mode, rtx, rtx);
  simd_immediate_info (scalar_int_mode, aarch64_svpattern);

  /* The mode of the elements.  */
  scalar_mode elt_mode;

  /* The instruction to use to move the immediate into a vector.  */
  insn_type insn;

  union
  {
    /* For MOV and MVN.  */
    struct
    {
      /* The value of each element.  */
      rtx value;

      /* The kind of shift modifier to use, and the number of bits to shift.
	 This is (LSL, 0) if no shift is needed.  */
      modifier_type modifier;
      unsigned int shift;
    } mov;

    /* For INDEX.  */
    struct
    {
      /* The value of the first element and the step to be added for each
	 subsequent element.  */
      rtx base, step;
    } index;

    /* For PTRUE.  */
    aarch64_svpattern pattern;
  } u;
};

/* Construct a floating-point immediate in which each element has mode
   ELT_MODE_IN and value VALUE_IN.  */
inline simd_immediate_info
::simd_immediate_info (scalar_float_mode elt_mode_in, rtx value_in)
  : elt_mode (elt_mode_in), insn (MOV)
{
  u.mov.value = value_in;
  u.mov.modifier = LSL;
  u.mov.shift = 0;
}

/* Construct an integer immediate in which each element has mode ELT_MODE_IN
   and value VALUE_IN.  The other parameters are as for the structure
   fields.  */
inline simd_immediate_info
::simd_immediate_info (scalar_int_mode elt_mode_in,
		       unsigned HOST_WIDE_INT value_in,
		       insn_type insn_in, modifier_type modifier_in,
		       unsigned int shift_in)
  : elt_mode (elt_mode_in), insn (insn_in)
{
  u.mov.value = gen_int_mode (value_in, elt_mode_in);
  u.mov.modifier = modifier_in;
  u.mov.shift = shift_in;
}

/* Construct an integer immediate in which each element has mode ELT_MODE_IN
   and where element I is equal to BASE_IN + I * STEP_IN.  */
inline simd_immediate_info
::simd_immediate_info (scalar_mode elt_mode_in, rtx base_in, rtx step_in)
  : elt_mode (elt_mode_in), insn (INDEX)
{
  u.index.base = base_in;
  u.index.step = step_in;
}

/* Construct a predicate that controls elements of mode ELT_MODE_IN
   and has PTRUE pattern PATTERN_IN.  */
inline simd_immediate_info
::simd_immediate_info (scalar_int_mode elt_mode_in,
		       aarch64_svpattern pattern_in)
  : elt_mode (elt_mode_in), insn (PTRUE)
{
  u.pattern = pattern_in;
}

namespace {

/* Describes types that map to Pure Scalable Types (PSTs) in the AAPCS64.  */
class pure_scalable_type_info
{
public:
  /* Represents the result of analyzing a type.  All values are nonzero,
     in the possibly forlorn hope that accidental conversions to bool
     trigger a warning.  */
  enum analysis_result
  {
    /* The type does not have an ABI identity; i.e. it doesn't contain
       at least one object whose type is a Fundamental Data Type.  */
    NO_ABI_IDENTITY = 1,

    /* The type is definitely a Pure Scalable Type.  */
    IS_PST,

    /* The type is definitely not a Pure Scalable Type.  */
    ISNT_PST,

    /* It doesn't matter for PCS purposes whether the type is a Pure
       Scalable Type or not, since the type will be handled the same
       way regardless.

       Specifically, this means that if the type is a Pure Scalable Type,
       there aren't enough argument registers to hold it, and so it will
       need to be passed or returned in memory.  If the type isn't a
       Pure Scalable Type, it's too big to be passed or returned in core
       or SIMD&FP registers, and so again will need to go in memory.  */
    DOESNT_MATTER
  };

  /* Aggregates of 17 bytes or more are normally passed and returned
     in memory, so aggregates of that size can safely be analyzed as
     DOESNT_MATTER.  We need to be able to collect enough pieces to
     represent a PST that is smaller than that.  Since predicates are
     2 bytes in size for -msve-vector-bits=128, that means we need to be
     able to store at least 8 pieces.

     We also need to be able to store enough pieces to represent
     a single vector in each vector argument register and a single
     predicate in each predicate argument register.  This means that
     we need at least 12 pieces.  */
  static const unsigned int MAX_PIECES = NUM_FP_ARG_REGS + NUM_PR_ARG_REGS;
  static_assert (MAX_PIECES >= 8, "Need to store at least 8 predicates");

  /* Describes one piece of a PST.  Each piece is one of:

     - a single Scalable Vector Type (SVT)
     - a single Scalable Predicate Type (SPT)
     - a PST containing 2, 3 or 4 SVTs, with no padding

     It either represents a single built-in type or a PST formed from
     multiple homogeneous built-in types.  */
  struct piece
  {
    rtx get_rtx (unsigned int, unsigned int) const;

    /* The number of vector and predicate registers that the piece
       occupies.  One of the two is always zero.  */
    unsigned int num_zr;
    unsigned int num_pr;

    /* The mode of the registers described above.  */
    machine_mode mode;

    /* If this piece is formed from multiple homogeneous built-in types,
       this is the mode of the built-in types, otherwise it is MODE.  */
    machine_mode orig_mode;

    /* The offset in bytes of the piece from the start of the type.  */
    poly_uint64 offset;
  };

  /* Divides types analyzed as IS_PST into individual pieces.  The pieces
     are in memory order.  */
  auto_vec<piece, MAX_PIECES> pieces;

  unsigned int num_zr () const;
  unsigned int num_pr () const;

  rtx get_rtx (machine_mode mode, unsigned int, unsigned int) const;

  analysis_result analyze (const_tree);
  bool analyze_registers (const_tree);

private:
  analysis_result analyze_array (const_tree);
  analysis_result analyze_record (const_tree);
  void add_piece (const piece &);
};
}

/* The current code model.  */
enum aarch64_code_model aarch64_cmodel;

enum aarch64_tp_reg aarch64_tpidr_register;

/* The number of 64-bit elements in an SVE vector.  */
poly_uint16 aarch64_sve_vg;

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS 1
#endif

static bool aarch64_composite_type_p (const_tree, machine_mode);
static bool aarch64_return_in_memory_1 (const_tree);
static bool aarch64_vfp_is_call_or_return_candidate (machine_mode,
						     const_tree,
						     machine_mode *, int *,
						     bool *, bool);
static void aarch64_elf_asm_constructor (rtx, int) ATTRIBUTE_UNUSED;
static void aarch64_elf_asm_destructor (rtx, int) ATTRIBUTE_UNUSED;
static void aarch64_override_options_after_change (void);
static bool aarch64_vector_mode_supported_p (machine_mode);
static int aarch64_address_cost (rtx, machine_mode, addr_space_t, bool);
static bool aarch64_builtin_support_vector_misalignment (machine_mode mode,
							 const_tree type,
							 int misalignment,
							 bool is_packed);
static machine_mode aarch64_simd_container_mode (scalar_mode, poly_int64);
static bool aarch64_print_address_internal (FILE*, machine_mode, rtx,
					    aarch64_addr_query_type);

/* The processor for which instructions should be scheduled.  */
enum aarch64_processor aarch64_tune = cortexa53;

/* Global flag for PC relative loads.  */
bool aarch64_pcrelative_literal_loads;

/* Global flag for whether frame pointer is enabled.  */
bool aarch64_use_frame_pointer;

/* Support for command line parsing of boolean flags in the tuning
   structures.  */
struct aarch64_flag_desc
{
  const char* name;
  unsigned int flag;
};

#define AARCH64_FUSION_PAIR(name, internal_name) \
  { name, AARCH64_FUSE_##internal_name },
static const struct aarch64_flag_desc aarch64_fusible_pairs[] =
{
  { "none", AARCH64_FUSE_NOTHING },
#include "aarch64-fusion-pairs.def"
  { "all", AARCH64_FUSE_ALL },
  { NULL, AARCH64_FUSE_NOTHING }
};

#define AARCH64_EXTRA_TUNING_OPTION(name, internal_name) \
  { name, AARCH64_EXTRA_TUNE_##internal_name },
static const struct aarch64_flag_desc aarch64_tuning_flags[] =
{
  { "none", AARCH64_EXTRA_TUNE_NONE },
#include "aarch64-tuning-flags.def"
  { "all", AARCH64_EXTRA_TUNE_ALL },
  { NULL, AARCH64_EXTRA_TUNE_NONE }
};

/* Tuning parameters.  */
#include "tuning_models/generic.h"
#include "tuning_models/generic_armv8_a.h"
#include "tuning_models/generic_armv9_a.h"
#include "tuning_models/cortexa35.h"
#include "tuning_models/cortexa53.h"
#include "tuning_models/cortexa57.h"
#include "tuning_models/cortexa72.h"
#include "tuning_models/cortexa73.h"
#include "tuning_models/cortexx925.h"
#include "tuning_models/exynosm1.h"
#include "tuning_models/thunderxt88.h"
#include "tuning_models/thunderx.h"
#include "tuning_models/tsv110.h"
#include "tuning_models/xgene1.h"
#include "tuning_models/emag.h"
#include "tuning_models/qdf24xx.h"
#include "tuning_models/saphira.h"
#include "tuning_models/thunderx2t99.h"
#include "tuning_models/thunderx3t110.h"
#include "tuning_models/neoversen1.h"
#include "tuning_models/ampere1.h"
#include "tuning_models/ampere1a.h"
#include "tuning_models/ampere1b.h"
#include "tuning_models/neoversev1.h"
#include "tuning_models/neoverse512tvb.h"
#include "tuning_models/neoversen2.h"
#include "tuning_models/neoversen3.h"
#include "tuning_models/neoversev2.h"
#include "tuning_models/neoversev3.h"
#include "tuning_models/neoversev3ae.h"
#include "tuning_models/a64fx.h"
#include "tuning_models/fujitsu_monaka.h"

/* Support for fine-grained override of the tuning structures.  */
struct aarch64_tuning_override_function
{
  const char* name;
  void (*parse_override)(const char*, struct tune_params*);
};

static void aarch64_parse_fuse_string (const char*, struct tune_params*);
static void aarch64_parse_tune_string (const char*, struct tune_params*);
static void aarch64_parse_sve_width_string (const char*, struct tune_params*);

static const struct aarch64_tuning_override_function
aarch64_tuning_override_functions[] =
{
  { "fuse", aarch64_parse_fuse_string },
  { "tune", aarch64_parse_tune_string },
  { "sve_width", aarch64_parse_sve_width_string },
  { NULL, NULL }
};

/* A processor implementing AArch64.  */
struct processor
{
  const char *name;
  aarch64_processor ident;
  aarch64_processor sched_core;
  aarch64_arch arch;
  aarch64_feature_flags flags;
  const tune_params *tune;
};

/* Architectures implementing AArch64.  */
static CONSTEXPR const processor all_architectures[] =
{
#define AARCH64_ARCH(NAME, CORE, ARCH_IDENT, D, E) \
  {NAME, CORE, CORE, AARCH64_ARCH_##ARCH_IDENT, \
   feature_deps::ARCH_IDENT ().enable, NULL},
#include "aarch64-arches.def"
  {NULL, aarch64_none, aarch64_none, aarch64_no_arch, 0, NULL}
};

/* Processor cores implementing AArch64.  */
static const struct processor all_cores[] =
{
#define AARCH64_CORE(NAME, IDENT, SCHED, ARCH, E, COSTS, G, H, I) \
  {NAME, IDENT, SCHED, AARCH64_ARCH_##ARCH, \
   feature_deps::cpu_##IDENT, &COSTS##_tunings},
#include "aarch64-cores.def"
  {NULL, aarch64_none, aarch64_none, aarch64_no_arch, 0, NULL}
};
/* Internal representation of system registers.  */
typedef struct {
  const char *name;
  /* Stringified sysreg encoding values, represented as
     s<sn>_<op1>_c<cn>_c<cm>_<op2>.  */
  const char *encoding;
  /* Flags affecting sysreg usage, such as read/write-only.  */
  unsigned properties;
  /* Architectural features implied by sysreg.  */
  aarch64_feature_flags arch_reqs;
} sysreg_t;

/* An aarch64_feature_set initializer for a single feature,
   AARCH64_FEATURE_<FEAT>.  */
#define AARCH64_FEATURE(FEAT) AARCH64_FL_##FEAT

/* Used by AARCH64_FEATURES.  */
#define AARCH64_OR_FEATURES_1(X, F1) \
  AARCH64_FEATURE (F1)
#define AARCH64_OR_FEATURES_2(X, F1, F2) \
  (AARCH64_FEATURE (F1) | AARCH64_OR_FEATURES_1 (X, F2))
#define AARCH64_OR_FEATURES_3(X, F1, ...) \
  (AARCH64_FEATURE (F1) | AARCH64_OR_FEATURES_2 (X, __VA_ARGS__))

/* An aarch64_feature_set initializer for the N features listed in "...".  */
#define AARCH64_FEATURES(N, ...) \
  AARCH64_OR_FEATURES_##N (0, __VA_ARGS__)

#define AARCH64_NO_FEATURES	   0

/* Flags associated with the properties of system registers.  It mainly serves
   to mark particular registers as read or write only.  */
#define F_DEPRECATED		   (1 << 1)
#define F_REG_READ		   (1 << 2)
#define F_REG_WRITE		   (1 << 3)
#define F_ARCHEXT		   (1 << 4)
/* Flag indicating register name is alias for another system register.  */
#define F_REG_ALIAS		   (1 << 5)
/* Flag indicatinig registers which may be implemented with 128-bits.  */
#define F_REG_128		   (1 << 6)

/* Database of system registers, their encodings and architectural
   requirements.  */
const sysreg_t aarch64_sysregs[] =
{
#define CPENC(SN, OP1, CN, CM, OP2) "s"#SN"_"#OP1"_c"#CN"_c"#CM"_"#OP2
#define SYSREG(NAME, ENC, FLAGS, ARCH) \
  { NAME, ENC, FLAGS, ARCH },
#include "aarch64-sys-regs.def"
#undef CPENC
};

#undef AARCH64_NO_FEATURES

using sysreg_map_t = hash_map<nofree_string_hash, const sysreg_t *>;
static sysreg_map_t *sysreg_map = nullptr;

/* Map system register names to their hardware metadata: encoding,
   feature flags and architectural feature requirements, all of which
   are encoded in a sysreg_t struct.  */
void
aarch64_register_sysreg (const char *name, const sysreg_t *metadata)
{
  bool dup = sysreg_map->put (name, metadata);
  gcc_checking_assert (!dup);
}

/* Lazily initialize hash table for system register validation,
   checking the validity of supplied register name and returning
   register's associated metadata.  */
static void
aarch64_init_sysregs (void)
{
  gcc_assert (!sysreg_map);
  sysreg_map = new sysreg_map_t;


  for (unsigned i = 0; i < ARRAY_SIZE (aarch64_sysregs); i++)
    {
      const sysreg_t *reg = aarch64_sysregs + i;
      aarch64_register_sysreg (reg->name, reg);
    }
}

/* No direct access to the sysreg hash-map should be made.  Doing so
   risks trying to acess an unitialized hash-map and dereferencing the
   returned double pointer without due care risks dereferencing a
   null-pointer.  */
const sysreg_t *
aarch64_lookup_sysreg_map (const char *regname)
{
  if (!sysreg_map)
    aarch64_init_sysregs ();

  const sysreg_t **sysreg_entry = sysreg_map->get (regname);
  if (sysreg_entry != NULL)
    return *sysreg_entry;
  return NULL;
}

/* The current tuning set.  */
struct tune_params aarch64_tune_params = generic_tunings;

/* If NAME is the name of an arm:: attribute that describes shared state,
   return its associated AARCH64_STATE_* flags, otherwise return 0.  */
static unsigned int
aarch64_attribute_shared_state_flags (const char *name)
{
  if (strcmp (name, "in") == 0)
    return AARCH64_STATE_SHARED | AARCH64_STATE_IN;
  if (strcmp (name, "inout") == 0)
    return AARCH64_STATE_SHARED | AARCH64_STATE_IN | AARCH64_STATE_OUT;
  if (strcmp (name, "out") == 0)
    return AARCH64_STATE_SHARED | AARCH64_STATE_OUT;
  if (strcmp (name, "preserves") == 0)
    return AARCH64_STATE_SHARED;
  return 0;
}

/* See whether attribute list ATTRS has any sharing information
   for state STATE_NAME.  Return the associated state flags if so,
   otherwise return 0.  */
static unsigned int
aarch64_lookup_shared_state_flags (tree attrs, const char *state_name)
{
  for (tree attr = attrs; attr; attr = TREE_CHAIN (attr))
    {
      if (!is_attribute_namespace_p ("arm", attr))
	continue;

      auto attr_name = IDENTIFIER_POINTER (get_attribute_name (attr));
      auto flags = aarch64_attribute_shared_state_flags (attr_name);
      if (!flags)
	continue;

      for (tree arg = TREE_VALUE (attr); arg; arg = TREE_CHAIN (arg))
	{
	  tree value = TREE_VALUE (arg);
	  if (TREE_CODE (value) == STRING_CST
	      && strcmp (TREE_STRING_POINTER (value), state_name) == 0)
	    return flags;
	}
    }
  return 0;
}

/* Return true if DECL creates a new scope for state STATE_STRING.  */
static bool
aarch64_fndecl_has_new_state (const_tree decl, const char *state_name)
{
  if (tree attr = lookup_attribute ("arm", "new", DECL_ATTRIBUTES (decl)))
    for (tree arg = TREE_VALUE (attr); arg; arg = TREE_CHAIN (arg))
      {
	tree value = TREE_VALUE (arg);
	if (TREE_CODE (value) == STRING_CST
	    && strcmp (TREE_STRING_POINTER (value), state_name) == 0)
	  return true;
      }
  return false;
}

/* Return true if attribute argument VALUE is a recognized state string,
   otherwise report an error.  NAME is the name of the attribute to which
   VALUE is being passed.  */
static bool
aarch64_check_state_string (tree name, tree value)
{
  if (TREE_CODE (value) != STRING_CST)
    {
      error ("the arguments to %qE must be constant strings", name);
      return false;
    }

  const char *state_name = TREE_STRING_POINTER (value);
  if (strcmp (state_name, "za") != 0
      && strcmp (state_name, "zt0") != 0)
    {
      error ("unrecognized state string %qs", state_name);
      return false;
    }

  return true;
}

/* qsort callback to compare two STRING_CSTs.  */
static int
cmp_string_csts (const void *a, const void *b)
{
  return strcmp (TREE_STRING_POINTER (*(const_tree const *) a),
		 TREE_STRING_POINTER (*(const_tree const *) b));
}

/* Canonicalize a list of state strings.  ARGS contains the arguments to
   a new attribute while OLD_ATTR, if nonnull, contains a previous attribute
   of the same type.  If CAN_MERGE_IN_PLACE, it is safe to adjust OLD_ATTR's
   arguments and drop the new attribute.  Otherwise, the new attribute must
   be kept and ARGS must include the information in OLD_ATTR.

   In both cases, the new arguments must be a sorted list of state strings
   with duplicates removed.

   Return true if new attribute should be kept, false if it should be
   dropped.  */
static bool
aarch64_merge_string_arguments (tree args, tree old_attr,
				bool can_merge_in_place)
{
  /* Get a sorted list of all state strings (including duplicates).  */
  auto add_args = [](vec<tree> &strings, const_tree args)
    {
      for (const_tree arg = args; arg; arg = TREE_CHAIN (arg))
	if (TREE_CODE (TREE_VALUE (arg)) == STRING_CST)
	  strings.safe_push (TREE_VALUE (arg));
    };
  auto_vec<tree, 16> strings;
  add_args (strings, args);
  if (old_attr)
    add_args (strings, TREE_VALUE (old_attr));
  strings.qsort (cmp_string_csts);

  /* The list can be empty if there was no previous attribute and if all
     the new arguments are erroneous.  Drop the attribute in that case.  */
  if (strings.is_empty ())
    return false;

  /* Destructively modify one of the argument lists, removing duplicates
     on the fly.  */
  bool use_old_attr = old_attr && can_merge_in_place;
  tree *end = use_old_attr ? &TREE_VALUE (old_attr) : &args;
  tree prev = NULL_TREE;
  for (tree arg : strings)
    {
      if (prev && simple_cst_equal (arg, prev))
	continue;
      prev = arg;
      if (!*end)
	*end = tree_cons (NULL_TREE, arg, NULL_TREE);
      else
	TREE_VALUE (*end) = arg;
      end = &TREE_CHAIN (*end);
    }
  *end = NULL_TREE;
  return !use_old_attr;
}

/* Check whether an 'aarch64_vector_pcs' attribute is valid.  */

static tree
handle_aarch64_vector_pcs_attribute (tree *node, tree name, tree,
				     int, bool *no_add_attrs)
{
  /* Since we set fn_type_req to true, the caller should have checked
     this for us.  */
  gcc_assert (FUNC_OR_METHOD_TYPE_P (*node));
  switch ((arm_pcs) fntype_abi (*node).id ())
    {
    case ARM_PCS_AAPCS64:
    case ARM_PCS_SIMD:
      return NULL_TREE;

    case ARM_PCS_SVE:
      error ("the %qE attribute cannot be applied to an SVE function type",
	     name);
      *no_add_attrs = true;
      return NULL_TREE;

    case ARM_PCS_TLSDESC:
    case ARM_PCS_UNKNOWN:
      break;
    }
  gcc_unreachable ();
}

/* Return true if arm::new(ARGS) is compatible with the type of decl DECL,
   otherwise report an error.  */
static bool
aarch64_check_arm_new_against_type (tree args, tree decl)
{
  tree type_attrs = TYPE_ATTRIBUTES (TREE_TYPE (decl));
  for (tree arg = args; arg; arg = TREE_CHAIN (arg))
    {
      tree value = TREE_VALUE (arg);
      if (TREE_CODE (value) == STRING_CST)
	{
	  const char *state_name = TREE_STRING_POINTER (value);
	  if (aarch64_lookup_shared_state_flags (type_attrs, state_name))
	    {
	      error_at (DECL_SOURCE_LOCATION (decl),
			"cannot create a new %qs scope since %qs is shared"
			" with callers", state_name, state_name);
	      return false;
	    }
	}
    }
  return true;
}

/* Callback for arm::new attributes.  */
static tree
handle_arm_new (tree *node, tree name, tree args, int, bool *no_add_attrs)
{
  tree decl = *node;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error ("%qE attribute applies only to function definitions", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  if (TREE_TYPE (decl) == error_mark_node)
    {
      *no_add_attrs = true;
      return NULL_TREE;
    }

  for (tree arg = args; arg; arg = TREE_CHAIN (arg))
    aarch64_check_state_string (name, TREE_VALUE (arg));

  if (!aarch64_check_arm_new_against_type (args, decl))
    {
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* If there is an old attribute, we should try to update it in-place,
     so that there is only one (definitive) arm::new attribute on the decl.  */
  tree old_attr = lookup_attribute ("arm", "new", DECL_ATTRIBUTES (decl));
  if (!aarch64_merge_string_arguments (args, old_attr, true))
    *no_add_attrs = true;

  return NULL_TREE;
}

/* Callback for arm::{in,out,inout,preserves} attributes.  */
static tree
handle_arm_shared (tree *node, tree name, tree args,
		   int, bool *no_add_attrs)
{
  tree type = *node;
  tree old_attrs = TYPE_ATTRIBUTES (type);
  auto flags = aarch64_attribute_shared_state_flags (IDENTIFIER_POINTER (name));
  for (tree arg = args; arg; arg = TREE_CHAIN (arg))
    {
      tree value = TREE_VALUE (arg);
      if (aarch64_check_state_string (name, value))
	{
	  const char *state_name = TREE_STRING_POINTER (value);
	  auto old_flags = aarch64_lookup_shared_state_flags (old_attrs,
							      state_name);
	  if (old_flags && old_flags != flags)
	    {
	      error ("inconsistent attributes for state %qs", state_name);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }
	}
    }

  /* We can't update an old attribute in-place, since types are shared.
     Instead make sure that this new attribute contains all the
     information, so that the old attribute becomes redundant.  */
  tree old_attr = lookup_attribute ("arm", IDENTIFIER_POINTER (name),
				    old_attrs);
  if (!aarch64_merge_string_arguments (args, old_attr, false))
    *no_add_attrs = true;

  return NULL_TREE;
}

/* Mutually-exclusive function type attributes for controlling PSTATE.SM.  */
static const struct attribute_spec::exclusions attr_streaming_exclusions[] =
{
  /* Attribute name     exclusion applies to:
			function, type, variable */
  { "streaming", false, true, false },
  { "streaming_compatible", false, true, false },
  { NULL, false, false, false }
};

/* Table of machine attributes.  */
static const attribute_spec aarch64_gnu_attributes[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "aarch64_vector_pcs", 0, 0, false, true,  true,  true,
			  handle_aarch64_vector_pcs_attribute, NULL },
  { "indirect_return",    0, 0, false, true, true, true, NULL, NULL },
  { "arm_sve_vector_bits", 1, 1, false, true,  false, true,
			  aarch64_sve::handle_arm_sve_vector_bits_attribute,
			  NULL },
  { "Advanced SIMD type", 1, 1, false, true,  false, true,  NULL, NULL },
  { "SVE type",		  3, 3, false, true,  false, true,  NULL, NULL },
  { "SVE sizeless type",  0, 0, false, true,  false, true,  NULL, NULL },
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
  { "dllimport", 0, 0, false, false, false, false, handle_dll_attribute, NULL },
  { "dllexport", 0, 0, false, false, false, false, handle_dll_attribute, NULL },
#endif
#ifdef SUBTARGET_ATTRIBUTE_TABLE
  SUBTARGET_ATTRIBUTE_TABLE
#endif
};

static const scoped_attribute_specs aarch64_gnu_attribute_table =
{
  "gnu", { aarch64_gnu_attributes }
};

static const attribute_spec aarch64_arm_attributes[] =
{
  { "streaming",	  0, 0, false, true,  true,  true,
			  NULL, attr_streaming_exclusions },
  { "streaming_compatible", 0, 0, false, true,  true,  true,
			  NULL, attr_streaming_exclusions },
  { "locally_streaming",  0, 0, true, false, false, false, NULL, NULL },
  { "new",		  1, -1, true, false, false, false,
			  handle_arm_new, NULL },
  { "preserves",	  1, -1, false, true,  true,  true,
			  handle_arm_shared, NULL },
  { "in",		  1, -1, false, true,  true,  true,
			  handle_arm_shared, NULL },
  { "out",		  1, -1, false, true,  true,  true,
			  handle_arm_shared, NULL },
  { "inout",		  1, -1, false, true,  true,  true,
			  handle_arm_shared, NULL }
};

static const scoped_attribute_specs aarch64_arm_attribute_table =
{
  "arm", { aarch64_arm_attributes }
};

static const scoped_attribute_specs *const aarch64_attribute_table[] =
{
  &aarch64_gnu_attribute_table,
  &aarch64_arm_attribute_table
};

typedef enum aarch64_cond_code
{
  AARCH64_EQ = 0, AARCH64_NE, AARCH64_CS, AARCH64_CC, AARCH64_MI, AARCH64_PL,
  AARCH64_VS, AARCH64_VC, AARCH64_HI, AARCH64_LS, AARCH64_GE, AARCH64_LT,
  AARCH64_GT, AARCH64_LE, AARCH64_AL, AARCH64_NV
}
aarch64_cc;

#define AARCH64_INVERSE_CONDITION_CODE(X) ((aarch64_cc) (((int) X) ^ 1))


/* The condition codes of the processor, and the inverse function.  */
static const char * const aarch64_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

/* The preferred condition codes for SVE conditions.  */
static const char *const aarch64_sve_condition_codes[] =
{
  "none", "any", "nlast", "last", "first", "nfrst", "vs", "vc",
  "pmore", "plast", "tcont", "tstop", "gt", "le", "al", "nv"
};

/* Return the assembly token for svpattern value VALUE.  */

static const char *
svpattern_token (enum aarch64_svpattern pattern)
{
  switch (pattern)
    {
#define CASE(UPPER, LOWER, VALUE) case AARCH64_SV_##UPPER: return #LOWER;
    AARCH64_FOR_SVPATTERN (CASE)
#undef CASE
    case AARCH64_NUM_SVPATTERNS:
      break;
    }
  gcc_unreachable ();
}

/* Return the location of a piece that is known to be passed or returned
   in registers.  FIRST_ZR is the first unused vector argument register
   and FIRST_PR is the first unused predicate argument register.  */

rtx
pure_scalable_type_info::piece::get_rtx (unsigned int first_zr,
					 unsigned int first_pr) const
{
  gcc_assert (VECTOR_MODE_P (mode)
	      && first_zr + num_zr <= V0_REGNUM + NUM_FP_ARG_REGS
	      && first_pr + num_pr <= P0_REGNUM + NUM_PR_ARG_REGS);

  if (num_zr > 0 && num_pr == 0)
    return gen_rtx_REG (mode, first_zr);

  if (num_zr == 0 && num_pr > 0)
    return gen_rtx_REG (mode, first_pr);

  gcc_unreachable ();
}

/* Return the total number of vector registers required by the PST.  */

unsigned int
pure_scalable_type_info::num_zr () const
{
  unsigned int res = 0;
  for (unsigned int i = 0; i < pieces.length (); ++i)
    res += pieces[i].num_zr;
  return res;
}

/* Return the total number of predicate registers required by the PST.  */

unsigned int
pure_scalable_type_info::num_pr () const
{
  unsigned int res = 0;
  for (unsigned int i = 0; i < pieces.length (); ++i)
    res += pieces[i].num_pr;
  return res;
}

/* Return the location of a PST that is known to be passed or returned
   in registers.  FIRST_ZR is the first unused vector argument register
   and FIRST_PR is the first unused predicate argument register.  */

rtx
pure_scalable_type_info::get_rtx (machine_mode mode,
				  unsigned int first_zr,
				  unsigned int first_pr) const
{
  /* Try to return a single REG if possible.  This leads to better
     code generation; it isn't required for correctness.  */
  if (mode == pieces[0].mode)
    {
      gcc_assert (pieces.length () == 1);
      return pieces[0].get_rtx (first_zr, first_pr);
    }

  /* Build up a PARALLEL that contains the individual pieces.  */
  rtvec rtxes = rtvec_alloc (pieces.length ());
  for (unsigned int i = 0; i < pieces.length (); ++i)
    {
      rtx reg = pieces[i].get_rtx (first_zr, first_pr);
      rtx offset = gen_int_mode (pieces[i].offset, Pmode);
      RTVEC_ELT (rtxes, i) = gen_rtx_EXPR_LIST (VOIDmode, reg, offset);
      first_zr += pieces[i].num_zr;
      first_pr += pieces[i].num_pr;
    }
  return gen_rtx_PARALLEL (mode, rtxes);
}

/* Analyze whether TYPE is a Pure Scalable Type according to the rules
   in the AAPCS64.  */

pure_scalable_type_info::analysis_result
pure_scalable_type_info::analyze (const_tree type)
{
  /* Prevent accidental reuse.  */
  gcc_assert (pieces.is_empty ());

  /* No code will be generated for erroneous types, so we won't establish
     an ABI mapping.  */
  if (type == error_mark_node)
    return NO_ABI_IDENTITY;

  /* Zero-sized types disappear in the language->ABI mapping.  */
  if (TYPE_SIZE (type) && integer_zerop (TYPE_SIZE (type)))
    return NO_ABI_IDENTITY;

  /* Check for SVTs, SPTs, and built-in tuple types that map to PSTs.  */
  piece p = {};
  if (aarch64_sve::builtin_type_p (type, &p.num_zr, &p.num_pr))
    {
      machine_mode mode = TYPE_MODE_RAW (type);
      gcc_assert (VECTOR_MODE_P (mode)
		  && (!TARGET_SVE || aarch64_sve_mode_p (mode)));

      p.mode = p.orig_mode = mode;
      add_piece (p);
      return IS_PST;
    }

  /* Check for user-defined PSTs.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    return analyze_array (type);
  if (TREE_CODE (type) == RECORD_TYPE)
    return analyze_record (type);

  return ISNT_PST;
}

/* Analyze a type that is known not to be passed or returned in memory.
   Return true if it has an ABI identity and is a Pure Scalable Type.  */

bool
pure_scalable_type_info::analyze_registers (const_tree type)
{
  analysis_result result = analyze (type);
  gcc_assert (result != DOESNT_MATTER);
  return result == IS_PST;
}

/* Subroutine of analyze for handling ARRAY_TYPEs.  */

pure_scalable_type_info::analysis_result
pure_scalable_type_info::analyze_array (const_tree type)
{
  /* Analyze the element type.  */
  pure_scalable_type_info element_info;
  analysis_result result = element_info.analyze (TREE_TYPE (type));
  if (result != IS_PST)
    return result;

  /* An array of unknown, flexible or variable length will be passed and
     returned by reference whatever we do.  */
  tree nelts_minus_one = array_type_nelts_minus_one (type);
  if (!tree_fits_uhwi_p (nelts_minus_one))
    return DOESNT_MATTER;

  /* Likewise if the array is constant-sized but too big to be interesting.
     The double checks against MAX_PIECES are to protect against overflow.  */
  unsigned HOST_WIDE_INT count = tree_to_uhwi (nelts_minus_one);
  if (count > MAX_PIECES)
    return DOESNT_MATTER;
  count += 1;
  if (count * element_info.pieces.length () > MAX_PIECES)
    return DOESNT_MATTER;

  /* The above checks should have weeded out elements of unknown size.  */
  poly_uint64 element_bytes;
  if (!poly_int_tree_p (TYPE_SIZE_UNIT (TREE_TYPE (type)), &element_bytes))
    gcc_unreachable ();

  /* Build up the list of individual vectors and predicates.  */
  gcc_assert (!element_info.pieces.is_empty ());
  for (unsigned int i = 0; i < count; ++i)
    for (unsigned int j = 0; j < element_info.pieces.length (); ++j)
      {
	piece p = element_info.pieces[j];
	p.offset += i * element_bytes;
	add_piece (p);
      }
  return IS_PST;
}

/* Subroutine of analyze for handling RECORD_TYPEs.  */

pure_scalable_type_info::analysis_result
pure_scalable_type_info::analyze_record (const_tree type)
{
  for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      /* Zero-sized fields disappear in the language->ABI mapping.  */
      if (DECL_SIZE (field) && integer_zerop (DECL_SIZE (field)))
	continue;

      /* All fields with an ABI identity must be PSTs for the record as
	 a whole to be a PST.  If any individual field is too big to be
	 interesting then the record is too.  */
      pure_scalable_type_info field_info;
      analysis_result subresult = field_info.analyze (TREE_TYPE (field));
      if (subresult == NO_ABI_IDENTITY)
	continue;
      if (subresult != IS_PST)
	return subresult;

      /* Since all previous fields are PSTs, we ought to be able to track
	 the field offset using poly_ints.  */
      tree bitpos = bit_position (field);
      gcc_assert (poly_int_tree_p (bitpos));

      /* For the same reason, it shouldn't be possible to create a PST field
	 whose offset isn't byte-aligned.  */
      poly_widest_int wide_bytepos = exact_div (wi::to_poly_widest (bitpos),
						BITS_PER_UNIT);

      /* Punt if the record is too big to be interesting.  */
      poly_uint64 bytepos;
      if (!wide_bytepos.to_uhwi (&bytepos)
	  || pieces.length () + field_info.pieces.length () > MAX_PIECES)
	return DOESNT_MATTER;

      /* Add the individual vectors and predicates in the field to the
	 record's list.  */
      gcc_assert (!field_info.pieces.is_empty ());
      for (unsigned int i = 0; i < field_info.pieces.length (); ++i)
	{
	  piece p = field_info.pieces[i];
	  p.offset += bytepos;
	  add_piece (p);
	}
    }
  /* Empty structures disappear in the language->ABI mapping.  */
  return pieces.is_empty () ? NO_ABI_IDENTITY : IS_PST;
}

/* Add P to the list of pieces in the type.  */

void
pure_scalable_type_info::add_piece (const piece &p)
{
  /* Try to fold the new piece into the previous one to form a
     single-mode PST.  For example, if we see three consecutive vectors
     of the same mode, we can represent them using the corresponding
     3-tuple mode.

     This is purely an optimization.  */
  if (!pieces.is_empty ())
    {
      piece &prev = pieces.last ();
      gcc_assert (VECTOR_MODE_P (p.mode) && VECTOR_MODE_P (prev.mode));
      unsigned int nelems1, nelems2;
      if (prev.orig_mode == p.orig_mode
	  && GET_MODE_CLASS (p.orig_mode) != MODE_VECTOR_BOOL
	  && known_eq (prev.offset + GET_MODE_SIZE (prev.mode), p.offset)
	  && constant_multiple_p (GET_MODE_NUNITS (prev.mode),
				  GET_MODE_NUNITS (p.orig_mode), &nelems1)
	  && constant_multiple_p (GET_MODE_NUNITS (p.mode),
				  GET_MODE_NUNITS (p.orig_mode), &nelems2)
	  && targetm.array_mode (p.orig_mode,
				 nelems1 + nelems2).exists (&prev.mode))
	{
	  prev.num_zr += p.num_zr;
	  prev.num_pr += p.num_pr;
	  return;
	}
    }
  pieces.quick_push (p);
}

/* Return true if at least one possible value of type TYPE includes at
   least one object of Pure Scalable Type, in the sense of the AAPCS64.

   This is a relatively expensive test for some types, so it should
   generally be made as late as possible.  */

static bool
aarch64_some_values_include_pst_objects_p (const_tree type)
{
  if (TYPE_SIZE (type) && integer_zerop (TYPE_SIZE (type)))
    return false;

  if (aarch64_sve::builtin_type_p (type))
    return true;

  if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == COMPLEX_TYPE)
    return aarch64_some_values_include_pst_objects_p (TREE_TYPE (type));

  if (RECORD_OR_UNION_TYPE_P (type))
    for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
      if (TREE_CODE (field) == FIELD_DECL
	  && aarch64_some_values_include_pst_objects_p (TREE_TYPE (field)))
	return true;

  return false;
}

/* Return the descriptor of the SIMD ABI.  */

static const predefined_function_abi &
aarch64_simd_abi (void)
{
  predefined_function_abi &simd_abi = function_abis[ARM_PCS_SIMD];
  if (!simd_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers
	= default_function_abi.full_reg_clobbers ();
      for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FP_SIMD_SAVED_REGNUM_P (regno))
	  CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      simd_abi.initialize (ARM_PCS_SIMD, full_reg_clobbers);
    }
  return simd_abi;
}

/* Return the descriptor of the SVE PCS.  */

static const predefined_function_abi &
aarch64_sve_abi (void)
{
  predefined_function_abi &sve_abi = function_abis[ARM_PCS_SVE];
  if (!sve_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers
	= default_function_abi.full_reg_clobbers ();
      for (int regno = V8_REGNUM; regno <= V23_REGNUM; ++regno)
	CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      for (int regno = P4_REGNUM; regno <= P15_REGNUM; ++regno)
	CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      sve_abi.initialize (ARM_PCS_SVE, full_reg_clobbers);
    }
  return sve_abi;
}

/* If X is an UNSPEC_SALT_ADDR expression, return the address that it
   wraps, otherwise return X itself.  */

static rtx
strip_salt (rtx x)
{
  rtx search = x;
  if (GET_CODE (search) == CONST)
    search = XEXP (search, 0);
  if (GET_CODE (search) == UNSPEC && XINT (search, 1) == UNSPEC_SALT_ADDR)
    x = XVECEXP (search, 0, 0);
  return x;
}

/* Like strip_offset, but also strip any UNSPEC_SALT_ADDR from the
   expression.  */

static rtx
strip_offset_and_salt (rtx addr, poly_int64 *offset)
{
  return strip_salt (strip_offset (addr, offset));
}

/* Generate code to enable conditional branches in functions over 1 MiB.  */
const char *
aarch64_gen_far_branch (rtx * operands, int pos_label, const char * dest,
			const char * branch_format)
{
    rtx_code_label * tmp_label = gen_label_rtx ();
    char label_buf[256];
    char buffer[128];
    ASM_GENERATE_INTERNAL_LABEL (label_buf, dest,
				 CODE_LABEL_NUMBER (tmp_label));
    const char *label_ptr = targetm.strip_name_encoding (label_buf);
    rtx dest_label = operands[pos_label];
    operands[pos_label] = tmp_label;

    snprintf (buffer, sizeof (buffer), "%s%s", branch_format, label_ptr);
    output_asm_insn (buffer, operands);

    snprintf (buffer, sizeof (buffer), "b\t%%l%d\n%s:", pos_label, label_ptr);
    operands[pos_label] = dest_label;
    output_asm_insn (buffer, operands);
    return "";
}

void
aarch64_err_no_fpadvsimd (machine_mode mode)
{
  if (TARGET_GENERAL_REGS_ONLY)
    if (FLOAT_MODE_P (mode))
      error ("%qs is incompatible with the use of floating-point types",
	     "-mgeneral-regs-only");
    else
      error ("%qs is incompatible with the use of vector types",
	     "-mgeneral-regs-only");
  else
    if (FLOAT_MODE_P (mode))
      error ("%qs feature modifier is incompatible with the use of"
	     " floating-point types", "+nofp");
    else
      error ("%qs feature modifier is incompatible with the use of"
	     " vector types", "+nofp");
}

/* Report when we try to do something that requires SVE when SVE is disabled.
   This is an error of last resort and isn't very high-quality.  It usually
   involves attempts to measure the vector length in some way.  */
static void
aarch64_report_sve_required (void)
{
  static bool reported_p = false;

  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_p)
    return;

  error ("this operation requires the SVE ISA extension");
  inform (input_location, "you can enable SVE using the command-line"
	  " option %<-march%>, or by using the %<target%>"
	  " attribute or pragma");
  reported_p = true;
}

/* Return true if REGNO is P0-P15 or one of the special FFR-related
   registers.  */
inline bool
pr_or_ffr_regnum_p (unsigned int regno)
{
  return PR_REGNUM_P (regno) || regno == FFR_REGNUM || regno == FFRT_REGNUM;
}

/* Implement TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS.
   The register allocator chooses POINTER_AND_FP_REGS if FP_REGS and
   GENERAL_REGS have the same cost - even if POINTER_AND_FP_REGS has a much
   higher cost.  POINTER_AND_FP_REGS is also used if the cost of both FP_REGS
   and GENERAL_REGS is lower than the memory cost (in this case the best class
   is the lowest cost one).  Using POINTER_AND_FP_REGS irrespectively of its
   cost results in bad allocations with many redundant int<->FP moves which
   are expensive on various cores.
   To avoid this we don't allow POINTER_AND_FP_REGS as the allocno class, but
   force a decision between FP_REGS and GENERAL_REGS.  We use the allocno class
   if it isn't POINTER_AND_FP_REGS.  Similarly, use the best class if it isn't
   POINTER_AND_FP_REGS.  Otherwise set the allocno class depending on the mode.
   The result of this is that it is no longer inefficient to have a higher
   memory move cost than the register move cost.
*/

static reg_class_t
aarch64_ira_change_pseudo_allocno_class (int regno, reg_class_t allocno_class,
					 reg_class_t best_class)
{
  machine_mode mode;

  if (!reg_class_subset_p (GENERAL_REGS, allocno_class)
      || !reg_class_subset_p (FP_REGS, allocno_class))
    return allocno_class;

  if (!reg_class_subset_p (GENERAL_REGS, best_class)
      || !reg_class_subset_p (FP_REGS, best_class))
    return best_class;

  mode = PSEUDO_REGNO_MODE (regno);
  return FLOAT_MODE_P (mode) || VECTOR_MODE_P (mode) ? FP_REGS : GENERAL_REGS;
}

static unsigned int
aarch64_min_divisions_for_recip_mul (machine_mode mode)
{
  if (GET_MODE_UNIT_SIZE (mode) == 4)
    return aarch64_tune_params.min_div_recip_mul_sf;
  return aarch64_tune_params.min_div_recip_mul_df;
}

/* Return the reassociation width of treeop OPC with mode MODE.  */
static int
aarch64_reassociation_width (unsigned opc, machine_mode mode)
{
  if (VECTOR_MODE_P (mode))
    return aarch64_tune_params.vec_reassoc_width;
  if (INTEGRAL_MODE_P (mode))
    return aarch64_tune_params.int_reassoc_width;
  /* Reassociation reduces the number of FMAs which may result in worse
     performance.  Use a per-CPU setting for FMA reassociation which allows
     narrow CPUs with few FP pipes to switch it off (value of 1), and wider
     CPUs with many FP pipes to enable reassociation.
     Since the reassociation pass doesn't understand FMA at all, assume
     that any FP addition might turn into FMA.  */
  if (FLOAT_MODE_P (mode))
    return opc == PLUS_EXPR ? aarch64_tune_params.fma_reassoc_width
			    : aarch64_tune_params.fp_reassoc_width;
  return 1;
}

/* Provide a mapping from gcc register numbers to dwarf register numbers.  */
unsigned
aarch64_debugger_regno (unsigned regno)
{
   if (GP_REGNUM_P (regno))
     return AARCH64_DWARF_R0 + regno - R0_REGNUM;
   else if (regno == SP_REGNUM)
     return AARCH64_DWARF_SP;
   else if (FP_REGNUM_P (regno))
     return AARCH64_DWARF_V0 + regno - V0_REGNUM;
   else if (PR_REGNUM_P (regno))
     return AARCH64_DWARF_P0 + regno - P0_REGNUM;
   else if (regno == VG_REGNUM)
     return AARCH64_DWARF_VG;

   /* Return values >= DWARF_FRAME_REGISTERS indicate that there is no
      equivalent DWARF register.  */
   return DWARF_FRAME_REGISTERS;
}

/* Implement TARGET_DWARF_FRAME_REG_MODE.  */
static machine_mode
aarch64_dwarf_frame_reg_mode (int regno)
{
  /* Predicate registers are call-clobbered in the EH ABI (which is
     ARM_PCS_AAPCS64), so they should not be described by CFI.
     Their size changes as VL changes, so any values computed by
     __builtin_init_dwarf_reg_size_table might not be valid for
     all frames.  */
  if (PR_REGNUM_P (regno))
    return VOIDmode;
  return default_dwarf_frame_reg_mode (regno);
}

/* Implement TARGET_OUTPUT_CFI_DIRECTIVE.  */
static bool
aarch64_output_cfi_directive (FILE *f, dw_cfi_ref cfi)
{
  bool found = false;
  if (cfi->dw_cfi_opc == DW_CFA_AARCH64_negate_ra_state)
    {
      fprintf (f, "\t.cfi_negate_ra_state\n");
      found = true;
    }
  return found;
}

/* Implement TARGET_DW_CFI_OPRND1_DESC.  */
static bool
aarch64_dw_cfi_oprnd1_desc (dwarf_call_frame_info cfi_opc,
			    dw_cfi_oprnd_type &oprnd_type)
{
  if (cfi_opc == DW_CFA_AARCH64_negate_ra_state)
    {
      oprnd_type = dw_cfi_oprnd_unused;
      return true;
    }
  return false;
}

/* If X is a CONST_DOUBLE, return its bit representation as a constant
   integer, otherwise return X unmodified.  */
static rtx
aarch64_bit_representation (rtx x)
{
  if (CONST_DOUBLE_P (x))
    x = gen_lowpart (int_mode_for_mode (GET_MODE (x)).require (), x);
  return x;
}

/* Return an estimate for the number of quadwords in an SVE vector.  This is
   equivalent to the number of Advanced SIMD vectors in an SVE vector.  */
static unsigned int
aarch64_estimated_sve_vq ()
{
  return estimated_poly_value (BITS_PER_SVE_VECTOR) / 128;
}

/* Return true if MODE is an SVE predicate mode.  */
static bool
aarch64_sve_pred_mode_p (machine_mode mode)
{
  return (TARGET_SVE
	  && (mode == VNx16BImode
	      || mode == VNx8BImode
	      || mode == VNx4BImode
	      || mode == VNx2BImode));
}

/* Three mutually-exclusive flags describing a vector or predicate type.  */
const unsigned int VEC_ADVSIMD  = 1;
const unsigned int VEC_SVE_DATA = 2;
const unsigned int VEC_SVE_PRED = 4;
/* Indicates a structure of 2, 3 or 4 vectors or predicates.  */
const unsigned int VEC_STRUCT   = 8;
/* Can be used in combination with VEC_SVE_DATA to indicate that the
   vector has fewer significant bytes than a full SVE vector.  */
const unsigned int VEC_PARTIAL  = 16;
/* Useful combinations of the above.  */
const unsigned int VEC_ANY_SVE  = VEC_SVE_DATA | VEC_SVE_PRED;
const unsigned int VEC_ANY_DATA = VEC_ADVSIMD | VEC_SVE_DATA;

/* Return a set of flags describing the vector properties of mode MODE.
   If ANY_TARGET_P is false (the default), ignore modes that are not supported
   by the current target.  Otherwise categorize the modes that can be used
   with the set of all targets supported by the port.  */

static unsigned int
aarch64_classify_vector_mode (machine_mode mode, bool any_target_p = false)
{
  if (aarch64_sve_pred_mode_p (mode))
    return VEC_SVE_PRED;

  /* Make the decision based on the mode's enum value rather than its
     properties, so that we keep the correct classification regardless
     of -msve-vector-bits.  */
  switch (mode)
    {
    /* Partial SVE QI vectors.  */
    case E_VNx2QImode:
    case E_VNx4QImode:
    case E_VNx8QImode:
    /* Partial SVE HI vectors.  */
    case E_VNx2HImode:
    case E_VNx4HImode:
    /* Partial SVE SI vector.  */
    case E_VNx2SImode:
    /* Partial SVE HF vectors.  */
    case E_VNx2HFmode:
    case E_VNx4HFmode:
    /* Partial SVE BF vectors.  */
    case E_VNx2BFmode:
    case E_VNx4BFmode:
    /* Partial SVE SF vector.  */
    case E_VNx2SFmode:
      return (TARGET_SVE || any_target_p) ? VEC_SVE_DATA | VEC_PARTIAL : 0;

    case E_VNx16QImode:
    case E_VNx8HImode:
    case E_VNx4SImode:
    case E_VNx2DImode:
    case E_VNx8BFmode:
    case E_VNx8HFmode:
    case E_VNx4SFmode:
    case E_VNx2DFmode:
      return (TARGET_SVE || any_target_p) ? VEC_SVE_DATA : 0;

    /* x2 SVE vectors.  */
    case E_VNx32QImode:
    case E_VNx16HImode:
    case E_VNx8SImode:
    case E_VNx4DImode:
    case E_VNx16BFmode:
    case E_VNx16HFmode:
    case E_VNx8SFmode:
    case E_VNx4DFmode:
    /* x3 SVE vectors.  */
    case E_VNx48QImode:
    case E_VNx24HImode:
    case E_VNx12SImode:
    case E_VNx6DImode:
    case E_VNx24BFmode:
    case E_VNx24HFmode:
    case E_VNx12SFmode:
    case E_VNx6DFmode:
    /* x4 SVE vectors.  */
    case E_VNx64QImode:
    case E_VNx32HImode:
    case E_VNx16SImode:
    case E_VNx8DImode:
    case E_VNx32BFmode:
    case E_VNx32HFmode:
    case E_VNx16SFmode:
    case E_VNx8DFmode:
      return (TARGET_SVE || any_target_p) ? VEC_SVE_DATA | VEC_STRUCT : 0;

    case E_OImode:
    case E_CImode:
    case E_XImode:
      return (TARGET_FLOAT || any_target_p) ? VEC_ADVSIMD | VEC_STRUCT : 0;

    /* Structures of 64-bit Advanced SIMD vectors.  */
    case E_V2x8QImode:
    case E_V2x4HImode:
    case E_V2x2SImode:
    case E_V2x1DImode:
    case E_V2x4BFmode:
    case E_V2x4HFmode:
    case E_V2x2SFmode:
    case E_V2x1DFmode:
    case E_V3x8QImode:
    case E_V3x4HImode:
    case E_V3x2SImode:
    case E_V3x1DImode:
    case E_V3x4BFmode:
    case E_V3x4HFmode:
    case E_V3x2SFmode:
    case E_V3x1DFmode:
    case E_V4x8QImode:
    case E_V4x4HImode:
    case E_V4x2SImode:
    case E_V4x1DImode:
    case E_V4x4BFmode:
    case E_V4x4HFmode:
    case E_V4x2SFmode:
    case E_V4x1DFmode:
      return (TARGET_FLOAT || any_target_p)
	      ? VEC_ADVSIMD | VEC_STRUCT | VEC_PARTIAL : 0;

    /* Structures of 128-bit Advanced SIMD vectors.  */
    case E_V2x16QImode:
    case E_V2x8HImode:
    case E_V2x4SImode:
    case E_V2x2DImode:
    case E_V2x8BFmode:
    case E_V2x8HFmode:
    case E_V2x4SFmode:
    case E_V2x2DFmode:
    case E_V3x16QImode:
    case E_V3x8HImode:
    case E_V3x4SImode:
    case E_V3x2DImode:
    case E_V3x8BFmode:
    case E_V3x8HFmode:
    case E_V3x4SFmode:
    case E_V3x2DFmode:
    case E_V4x16QImode:
    case E_V4x8HImode:
    case E_V4x4SImode:
    case E_V4x2DImode:
    case E_V4x8BFmode:
    case E_V4x8HFmode:
    case E_V4x4SFmode:
    case E_V4x2DFmode:
      return (TARGET_FLOAT || any_target_p) ? VEC_ADVSIMD | VEC_STRUCT : 0;

    /* 64-bit Advanced SIMD vectors.  */
    case E_V8QImode:
    case E_V4HImode:
    case E_V2SImode:
    case E_V1DImode:
    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2SFmode:
    case E_V1DFmode:
    /* 128-bit Advanced SIMD vectors.  */
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V4SFmode:
    case E_V2DFmode:
      return (TARGET_FLOAT || any_target_p) ? VEC_ADVSIMD : 0;

    case E_VNx32BImode:
    case E_VNx64BImode:
      return TARGET_SVE ? VEC_SVE_PRED | VEC_STRUCT : 0;

    default:
      return 0;
    }
}

/* Like aarch64_classify_vector_mode, but also include modes that are used
   for memory operands but not register operands.  Such modes do not count
   as real vector modes; they are just an internal construct to make things
   easier to describe.  */
static unsigned int
aarch64_classify_vector_memory_mode (machine_mode mode)
{
  switch (mode)
    {
    case VNx1SImode:
    case VNx1DImode:
      return TARGET_SVE ? VEC_SVE_DATA | VEC_PARTIAL : 0;

    case VNx1TImode:
      return TARGET_SVE ? VEC_SVE_DATA : 0;

    case VNx2TImode:
    case VNx3TImode:
    case VNx4TImode:
      return TARGET_SVE ? VEC_SVE_DATA | VEC_STRUCT : 0;

    default:
      return aarch64_classify_vector_mode (mode);
    }
}

/* Return true if MODE is any of the Advanced SIMD structure modes.  */
bool
aarch64_advsimd_struct_mode_p (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  return (vec_flags & VEC_ADVSIMD) && (vec_flags & VEC_STRUCT);
}

/* Return true if MODE is an Advanced SIMD D-register structure mode.  */
static bool
aarch64_advsimd_partial_struct_mode_p (machine_mode mode)
{
  return (aarch64_classify_vector_mode (mode)
	  == (VEC_ADVSIMD | VEC_STRUCT | VEC_PARTIAL));
}

/* Return true if MODE is an Advanced SIMD Q-register structure mode.  */
static bool
aarch64_advsimd_full_struct_mode_p (machine_mode mode)
{
  return (aarch64_classify_vector_mode (mode) == (VEC_ADVSIMD | VEC_STRUCT));
}

/* Return true if MODE is any of the data vector modes, including
   structure modes.  */
static bool
aarch64_vector_data_mode_p (machine_mode mode)
{
  return aarch64_classify_vector_mode (mode) & VEC_ANY_DATA;
}

/* Return true if MODE is any form of SVE mode, including predicates,
   vectors and structures.  */
bool
aarch64_sve_mode_p (machine_mode mode)
{
  return aarch64_classify_vector_mode (mode) & VEC_ANY_SVE;
}

/* Return true if MODE is an SVE data vector mode; either a single vector
   or a structure of vectors.  */
static bool
aarch64_sve_data_mode_p (machine_mode mode)
{
  return aarch64_classify_vector_mode (mode) & VEC_SVE_DATA;
}

/* Return the number of defined bytes in one constituent vector of
   SVE mode MODE, which has vector flags VEC_FLAGS.  */
static poly_int64
aarch64_vl_bytes (machine_mode mode, unsigned int vec_flags)
{
  if (vec_flags & VEC_PARTIAL)
    /* A single partial vector.  */
    return GET_MODE_SIZE (mode);

  if (vec_flags & VEC_SVE_DATA)
    /* A single vector or a tuple.  */
    return BYTES_PER_SVE_VECTOR;

  /* A single predicate.  */
  gcc_assert (vec_flags & VEC_SVE_PRED);
  return BYTES_PER_SVE_PRED;
}

/* If MODE holds an array of vectors, return the number of vectors
   in the array, otherwise return 1.  */

static unsigned int
aarch64_ldn_stn_vectors (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags == (VEC_ADVSIMD | VEC_PARTIAL | VEC_STRUCT))
    return exact_div (GET_MODE_SIZE (mode), 8).to_constant ();
  if (vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
    return exact_div (GET_MODE_SIZE (mode), 16).to_constant ();
  if (vec_flags == (VEC_SVE_DATA | VEC_STRUCT))
    return exact_div (GET_MODE_SIZE (mode),
		      BYTES_PER_SVE_VECTOR).to_constant ();
  return 1;
}

/* Given an Advanced SIMD vector mode MODE and a tuple size NELEMS, return the
   corresponding vector structure mode.  */
static opt_machine_mode
aarch64_advsimd_vector_array_mode (machine_mode mode,
				   unsigned HOST_WIDE_INT nelems)
{
  unsigned int flags = VEC_ADVSIMD | VEC_STRUCT;
  if (known_eq (GET_MODE_SIZE (mode), 8))
    flags |= VEC_PARTIAL;

  machine_mode struct_mode;
  FOR_EACH_MODE_IN_CLASS (struct_mode, GET_MODE_CLASS (mode))
    if (aarch64_classify_vector_mode (struct_mode) == flags
	&& GET_MODE_INNER (struct_mode) == GET_MODE_INNER (mode)
	&& known_eq (GET_MODE_NUNITS (struct_mode),
	     GET_MODE_NUNITS (mode) * nelems))
      return struct_mode;
  return opt_machine_mode ();
}

/* Return the SVE vector mode that has NUNITS elements of mode INNER_MODE.  */

opt_machine_mode
aarch64_sve_data_mode (scalar_mode inner_mode, poly_uint64 nunits)
{
  enum mode_class mclass = (is_a <scalar_float_mode> (inner_mode)
			    ? MODE_VECTOR_FLOAT : MODE_VECTOR_INT);
  machine_mode mode;
  FOR_EACH_MODE_IN_CLASS (mode, mclass)
    if (inner_mode == GET_MODE_INNER (mode)
	&& known_eq (nunits, GET_MODE_NUNITS (mode))
	&& aarch64_sve_data_mode_p (mode))
      return mode;
  return opt_machine_mode ();
}

/* Implement target hook TARGET_ARRAY_MODE.  */
static opt_machine_mode
aarch64_array_mode (machine_mode mode, unsigned HOST_WIDE_INT nelems)
{
  if (TARGET_SVE && GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
    {
      /* Use VNx32BI and VNx64BI for tuples of predicates, but explicitly
	 reject giving a mode to other array sizes.  Using integer modes
	 requires a round trip through memory and generates terrible code.  */
      if (nelems == 1)
	return mode;
      if (mode == VNx16BImode && nelems == 2)
	return VNx32BImode;
      if (mode == VNx16BImode && nelems == 4)
	return VNx64BImode;
      return BLKmode;
    }

  auto flags = aarch64_classify_vector_mode (mode);
  if (flags == VEC_SVE_DATA && IN_RANGE (nelems, 2, 4))
    return aarch64_sve_data_mode (GET_MODE_INNER (mode),
				  GET_MODE_NUNITS (mode) * nelems);

  if (flags == VEC_ADVSIMD && IN_RANGE (nelems, 2, 4))
    return aarch64_advsimd_vector_array_mode (mode, nelems);

  return opt_machine_mode ();
}

/* Implement target hook TARGET_ARRAY_MODE_SUPPORTED_P.  */
static bool
aarch64_array_mode_supported_p (machine_mode mode,
				unsigned HOST_WIDE_INT nelems)
{
  if (TARGET_BASE_SIMD
      && (AARCH64_VALID_SIMD_QREG_MODE (mode)
	  || AARCH64_VALID_SIMD_DREG_MODE (mode))
      && (nelems >= 2 && nelems <= 4))
    return true;

  return false;
}

/* MODE is some form of SVE vector mode.  For data modes, return the number
   of vector register bits that each element of MODE occupies, such as 64
   for both VNx2DImode and VNx2SImode (where each 32-bit value is stored
   in a 64-bit container).  For predicate modes, return the number of
   data bits controlled by each significant predicate bit.  */

static unsigned int
aarch64_sve_container_bits (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  poly_uint64 vector_bits = (vec_flags & (VEC_PARTIAL | VEC_SVE_PRED)
			     ? BITS_PER_SVE_VECTOR
			     : GET_MODE_BITSIZE (mode));
  return vector_element_size (vector_bits, GET_MODE_NUNITS (mode));
}

/* Return the SVE predicate mode to use for elements that have
   ELEM_NBYTES bytes, if such a mode exists.  */

opt_machine_mode
aarch64_sve_pred_mode (unsigned int elem_nbytes)
{
  if (TARGET_SVE)
    {
      if (elem_nbytes == 1)
	return VNx16BImode;
      if (elem_nbytes == 2)
	return VNx8BImode;
      if (elem_nbytes == 4)
	return VNx4BImode;
      if (elem_nbytes == 8)
	return VNx2BImode;
    }
  return opt_machine_mode ();
}

/* Return the SVE predicate mode that should be used to control
   SVE mode MODE.  */

machine_mode
aarch64_sve_pred_mode (machine_mode mode)
{
  unsigned int bits = aarch64_sve_container_bits (mode);
  return aarch64_sve_pred_mode (bits / BITS_PER_UNIT).require ();
}

/* Implement TARGET_VECTORIZE_GET_MASK_MODE.  */

static opt_machine_mode
aarch64_get_mask_mode (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags & VEC_SVE_DATA)
    return aarch64_sve_pred_mode (mode);

  return default_get_mask_mode (mode);
}

/* Return the integer element mode associated with SVE mode MODE.  */

static scalar_int_mode
aarch64_sve_element_int_mode (machine_mode mode)
{
  poly_uint64 vector_bits = (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
			     ? BITS_PER_SVE_VECTOR
			     : GET_MODE_BITSIZE (mode));
  unsigned int elt_bits = vector_element_size (vector_bits,
					       GET_MODE_NUNITS (mode));
  return int_mode_for_size (elt_bits, 0).require ();
}

/* Return an integer element mode that contains exactly
   aarch64_sve_container_bits (MODE) bits.  This is wider than
   aarch64_sve_element_int_mode if MODE is a partial vector,
   otherwise it's the same.  */

static scalar_int_mode
aarch64_sve_container_int_mode (machine_mode mode)
{
  return int_mode_for_size (aarch64_sve_container_bits (mode), 0).require ();
}

/* Return the integer vector mode associated with SVE mode MODE.
   Unlike related_int_vector_mode, this can handle the case in which
   MODE is a predicate (and thus has a different total size).  */

machine_mode
aarch64_sve_int_mode (machine_mode mode)
{
  scalar_int_mode int_mode = aarch64_sve_element_int_mode (mode);
  return aarch64_sve_data_mode (int_mode, GET_MODE_NUNITS (mode)).require ();
}

/* Look for a vector mode with the same classification as VEC_MODE,
   but with each group of FACTOR elements coalesced into a single element.
   In other words, look for a mode in which the elements are FACTOR times
   larger and in which the number of elements is FACTOR times smaller.

   Return the mode found, if one exists.  */

static opt_machine_mode
aarch64_coalesce_units (machine_mode vec_mode, unsigned int factor)
{
  auto elt_bits = vector_element_size (GET_MODE_BITSIZE (vec_mode),
				       GET_MODE_NUNITS (vec_mode));
  auto vec_flags = aarch64_classify_vector_mode (vec_mode);
  if (vec_flags & VEC_SVE_PRED)
    {
      if (known_eq (GET_MODE_SIZE (vec_mode), BYTES_PER_SVE_PRED))
	return aarch64_sve_pred_mode (elt_bits * factor);
      return {};
    }

  scalar_mode new_elt_mode;
  if (!int_mode_for_size (elt_bits * factor, false).exists (&new_elt_mode))
    return {};

  if (vec_flags == VEC_ADVSIMD)
    {
      auto mode = aarch64_simd_container_mode (new_elt_mode,
					       GET_MODE_BITSIZE (vec_mode));
      if (mode != word_mode)
	return mode;
    }
  else if (vec_flags & VEC_SVE_DATA)
    {
      poly_uint64 new_nunits;
      if (multiple_p (GET_MODE_NUNITS (vec_mode), factor, &new_nunits))
	return aarch64_sve_data_mode (new_elt_mode, new_nunits);
    }
  return {};
}

/* Implement TARGET_VECTORIZE_RELATED_MODE.  */

static opt_machine_mode
aarch64_vectorize_related_mode (machine_mode vector_mode,
				scalar_mode element_mode,
				poly_uint64 nunits)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (vector_mode);

  /* If we're operating on SVE vectors, try to return an SVE mode.  */
  poly_uint64 sve_nunits;
  if ((vec_flags & VEC_SVE_DATA)
      && multiple_p (BYTES_PER_SVE_VECTOR,
		     GET_MODE_SIZE (element_mode), &sve_nunits))
    {
      machine_mode sve_mode;
      if (maybe_ne (nunits, 0U))
	{
	  /* Try to find a full or partial SVE mode with exactly
	     NUNITS units.  */
	  if (multiple_p (sve_nunits, nunits)
	      && aarch64_sve_data_mode (element_mode,
					nunits).exists (&sve_mode))
	    return sve_mode;
	}
      else
	{
	  /* Take the preferred number of units from the number of bytes
	     that fit in VECTOR_MODE.  We always start by "autodetecting"
	     a full vector mode with preferred_simd_mode, so vectors
	     chosen here will also be full vector modes.  Then
	     autovectorize_vector_modes tries smaller starting modes
	     and thus smaller preferred numbers of units.  */
	  sve_nunits = ordered_min (sve_nunits, GET_MODE_SIZE (vector_mode));
	  if (aarch64_sve_data_mode (element_mode,
				     sve_nunits).exists (&sve_mode))
	    return sve_mode;
	}
    }

  /* Prefer to use 1 128-bit vector instead of 2 64-bit vectors.  */
  if (TARGET_SIMD
      && (vec_flags & VEC_ADVSIMD)
      && known_eq (nunits, 0U)
      && known_eq (GET_MODE_BITSIZE (vector_mode), 64U)
      && maybe_ge (GET_MODE_BITSIZE (element_mode)
		   * GET_MODE_NUNITS (vector_mode), 128U))
    {
      machine_mode res = aarch64_simd_container_mode (element_mode, 128);
      if (VECTOR_MODE_P (res))
	return res;
    }

  return default_vectorize_related_mode (vector_mode, element_mode, nunits);
}

/* Implement TARGET_VECTORIZE_PREFERRED_DIV_AS_SHIFTS_OVER_MULT.  */

static bool
aarch64_vectorize_preferred_div_as_shifts_over_mult (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  bool sve_p = (vec_flags & VEC_ANY_SVE);
  bool simd_p = (vec_flags & VEC_ADVSIMD);

  return (sve_p && TARGET_SVE2) || (simd_p && TARGET_SIMD);
}

/* Implement TARGET_PREFERRED_ELSE_VALUE.  For binary operations,
   prefer to use the first arithmetic operand as the else value if
   the else value doesn't matter, since that exactly matches the SVE
   destructive merging form.  For ternary operations we could either
   pick the first operand and use FMAD-like instructions or the last
   operand and use FMLA-like instructions; the latter seems more
   natural.  */

static tree
aarch64_preferred_else_value (unsigned, tree, unsigned int nops, tree *ops)
{
  return nops == 3 ? ops[2] : ops[0];
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
aarch64_hard_regno_nregs (unsigned regno, machine_mode mode)
{
  /* ??? Logically we should only need to provide a value when
     HARD_REGNO_MODE_OK says that the combination is valid,
     but at the moment we need to handle all modes.  Just ignore
     any runtime parts for registers that can't store them.  */
  HOST_WIDE_INT lowest_size = constant_lower_bound (GET_MODE_SIZE (mode));
  switch (aarch64_regno_regclass (regno))
    {
    case FP_REGS:
    case FP_LO_REGS:
    case FP_LO8_REGS:
      {
	unsigned int vec_flags = aarch64_classify_vector_mode (mode);
	if (vec_flags & VEC_SVE_DATA)
	  return exact_div (GET_MODE_SIZE (mode),
			    aarch64_vl_bytes (mode, vec_flags)).to_constant ();
	if (vec_flags == (VEC_ADVSIMD | VEC_STRUCT | VEC_PARTIAL))
	  return GET_MODE_SIZE (mode).to_constant () / 8;
	return CEIL (lowest_size, UNITS_PER_VREG);
      }

    case PR_REGS:
    case PR_LO_REGS:
    case PR_HI_REGS:
      return mode == VNx64BImode ? 4 : mode == VNx32BImode ? 2 : 1;

    case MOVEABLE_SYSREGS:
    case FFR_REGS:
    case PR_AND_FFR_REGS:
    case FAKE_REGS:
      return 1;

    default:
      return CEIL (lowest_size, UNITS_PER_WORD);
    }
  gcc_unreachable ();
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
aarch64_hard_regno_mode_ok (unsigned regno, machine_mode mode)
{
  if (mode == V8DImode)
    return IN_RANGE (regno, R0_REGNUM, R23_REGNUM)
           && multiple_p (regno - R0_REGNUM, 2);

  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;

  if (regno == VG_REGNUM)
    /* This must have the same size as _Unwind_Word.  */
    return mode == DImode;

  if (regno == FPM_REGNUM)
    return mode == QImode || mode == HImode || mode == SImode || mode == DImode;

  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags == VEC_SVE_PRED)
    return pr_or_ffr_regnum_p (regno);

  if (vec_flags == (VEC_SVE_PRED | VEC_STRUCT))
    return PR_REGNUM_P (regno);

  if (pr_or_ffr_regnum_p (regno))
    return false;

  /* These registers are abstract; their modes don't matter.  */
  if (FAKE_REGNUM_P (regno))
    return true;

  if (regno == SP_REGNUM)
    /* The purpose of comparing with ptr_mode is to support the
       global register variable associated with the stack pointer
       register via the syntax of asm ("wsp") in ILP32.  */
    return mode == Pmode || mode == ptr_mode;

  if (regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    return mode == Pmode;

  if (GP_REGNUM_P (regno))
    {
      if (vec_flags & (VEC_ANY_SVE | VEC_STRUCT))
	return false;
      if (known_le (GET_MODE_SIZE (mode), 8))
	return true;
      if (known_le (GET_MODE_SIZE (mode), 16))
	return (regno & 1) == 0;
    }
  else if (FP_REGNUM_P (regno))
    {
      if (vec_flags & VEC_STRUCT)
	return end_hard_regno (mode, regno) - 1 <= V31_REGNUM;
      else
	return !VECTOR_MODE_P (mode) || vec_flags != 0;
    }

  return false;
}

/* Return true if a function with type FNTYPE returns its value in
   SVE vector or predicate registers.  */

static bool
aarch64_returns_value_in_sve_regs_p (const_tree fntype)
{
  tree return_type = TREE_TYPE (fntype);

  pure_scalable_type_info pst_info;
  switch (pst_info.analyze (return_type))
    {
    case pure_scalable_type_info::IS_PST:
      return (pst_info.num_zr () <= NUM_FP_ARG_REGS
	      && pst_info.num_pr () <= NUM_PR_ARG_REGS);

    case pure_scalable_type_info::DOESNT_MATTER:
      gcc_assert (aarch64_return_in_memory_1 (return_type));
      return false;

    case pure_scalable_type_info::NO_ABI_IDENTITY:
    case pure_scalable_type_info::ISNT_PST:
      return false;
    }
  gcc_unreachable ();
}

/* Return true if a function with type FNTYPE takes arguments in
   SVE vector or predicate registers.  */

static bool
aarch64_takes_arguments_in_sve_regs_p (const_tree fntype)
{
  CUMULATIVE_ARGS args_so_far_v;
  aarch64_init_cumulative_args (&args_so_far_v, NULL_TREE, NULL_RTX,
				NULL_TREE, 0, true);
  cumulative_args_t args_so_far = pack_cumulative_args (&args_so_far_v);

  for (tree chain = TYPE_ARG_TYPES (fntype);
       chain && chain != void_list_node;
       chain = TREE_CHAIN (chain))
    {
      tree arg_type = TREE_VALUE (chain);
      if (arg_type == error_mark_node)
	return false;

      function_arg_info arg (arg_type, /*named=*/true);
      apply_pass_by_reference_rules (&args_so_far_v, arg);
      pure_scalable_type_info pst_info;
      if (pst_info.analyze_registers (arg.type))
	{
	  unsigned int end_zr = args_so_far_v.aapcs_nvrn + pst_info.num_zr ();
	  unsigned int end_pr = args_so_far_v.aapcs_nprn + pst_info.num_pr ();
	  gcc_assert (end_zr <= NUM_FP_ARG_REGS && end_pr <= NUM_PR_ARG_REGS);
	  return true;
	}

      targetm.calls.function_arg_advance (args_so_far, arg);
    }
  return false;
}

/* Implement TARGET_FNTYPE_ABI.  */

static const predefined_function_abi &
aarch64_fntype_abi (const_tree fntype)
{
  if (lookup_attribute ("aarch64_vector_pcs", TYPE_ATTRIBUTES (fntype)))
    return aarch64_simd_abi ();

  if (aarch64_returns_value_in_sve_regs_p (fntype)
      || aarch64_takes_arguments_in_sve_regs_p (fntype))
    return aarch64_sve_abi ();

  return default_function_abi;
}

/* Return the state of PSTATE.SM on entry to functions of type FNTYPE.  */

static aarch64_isa_mode
aarch64_fntype_pstate_sm (const_tree fntype)
{
  if (lookup_attribute ("arm", "streaming", TYPE_ATTRIBUTES (fntype)))
    return AARCH64_ISA_MODE_SM_ON;

  if (lookup_attribute ("arm", "streaming_compatible",
			TYPE_ATTRIBUTES (fntype)))
    return 0;

  return AARCH64_ISA_MODE_SM_OFF;
}

/* Return state flags that describe whether and how functions of type
   FNTYPE share state STATE_NAME with their callers.  */

static unsigned int
aarch64_fntype_shared_flags (const_tree fntype, const char *state_name)
{
  return aarch64_lookup_shared_state_flags (TYPE_ATTRIBUTES (fntype),
					    state_name);
}

/* Return the state of PSTATE.ZA on entry to functions of type FNTYPE.  */

static aarch64_isa_mode
aarch64_fntype_pstate_za (const_tree fntype)
{
  if (aarch64_fntype_shared_flags (fntype, "za")
      || aarch64_fntype_shared_flags (fntype, "zt0"))
    return AARCH64_ISA_MODE_ZA_ON;

  return 0;
}

/* Return the ISA mode on entry to functions of type FNTYPE.  */

static aarch64_isa_mode
aarch64_fntype_isa_mode (const_tree fntype)
{
  return (aarch64_fntype_pstate_sm (fntype)
	  | aarch64_fntype_pstate_za (fntype));
}

/* Return true if FNDECL uses streaming mode internally, as an
   implementation choice.  */

static bool
aarch64_fndecl_is_locally_streaming (const_tree fndecl)
{
  return lookup_attribute ("arm", "locally_streaming",
			   DECL_ATTRIBUTES (fndecl));
}

/* Return the state of PSTATE.SM when compiling the body of
   function FNDECL.  This might be different from the state of
   PSTATE.SM on entry.  */

static aarch64_isa_mode
aarch64_fndecl_pstate_sm (const_tree fndecl)
{
  if (aarch64_fndecl_is_locally_streaming (fndecl))
    return AARCH64_ISA_MODE_SM_ON;

  return aarch64_fntype_pstate_sm (TREE_TYPE (fndecl));
}

/* Return true if function FNDECL has state STATE_NAME, either by creating
   new state itself or by sharing state with callers.  */

static bool
aarch64_fndecl_has_state (tree fndecl, const char *state_name)
{
  return (aarch64_fndecl_has_new_state (fndecl, state_name)
	  || aarch64_fntype_shared_flags (TREE_TYPE (fndecl),
					  state_name) != 0);
}

/* Return the state of PSTATE.ZA when compiling the body of function FNDECL.
   This might be different from the state of PSTATE.ZA on entry.  */

static aarch64_isa_mode
aarch64_fndecl_pstate_za (const_tree fndecl)
{
  if (aarch64_fndecl_has_new_state (fndecl, "za")
      || aarch64_fndecl_has_new_state (fndecl, "zt0"))
    return AARCH64_ISA_MODE_ZA_ON;

  return aarch64_fntype_pstate_za (TREE_TYPE (fndecl));
}

/* Return the ISA mode that should be used to compile the body of
   function FNDECL.  */

static aarch64_isa_mode
aarch64_fndecl_isa_mode (const_tree fndecl)
{
  return (aarch64_fndecl_pstate_sm (fndecl)
	  | aarch64_fndecl_pstate_za (fndecl));
}

/* Return the state of PSTATE.SM on entry to the current function.
   This might be different from the state of PSTATE.SM in the function
   body.  */

static aarch64_isa_mode
aarch64_cfun_incoming_pstate_sm ()
{
  return aarch64_fntype_pstate_sm (TREE_TYPE (cfun->decl));
}

/* Return the state of PSTATE.ZA on entry to the current function.
   This might be different from the state of PSTATE.ZA in the function
   body.  */

static aarch64_isa_mode
aarch64_cfun_incoming_pstate_za ()
{
  return aarch64_fntype_pstate_za (TREE_TYPE (cfun->decl));
}

/* Return state flags that describe whether and how the current function shares
   state STATE_NAME with callers.  */

static unsigned int
aarch64_cfun_shared_flags (const char *state_name)
{
  return aarch64_fntype_shared_flags (TREE_TYPE (cfun->decl), state_name);
}

/* Return true if the current function creates new state of type STATE_NAME
   (as opposed to sharing the state with its callers or ignoring the state
   altogether).  */

static bool
aarch64_cfun_has_new_state (const char *state_name)
{
  return aarch64_fndecl_has_new_state (cfun->decl, state_name);
}

/* Return true if PSTATE.SM is 1 in the body of the current function,
   but is not guaranteed to be 1 on entry.  */

static bool
aarch64_cfun_enables_pstate_sm ()
{
  return (aarch64_fndecl_is_locally_streaming (cfun->decl)
	  && aarch64_cfun_incoming_pstate_sm () != AARCH64_ISA_MODE_SM_ON);
}

/* Return true if the current function has state STATE_NAME, either by
   creating new state itself or by sharing state with callers.  */

static bool
aarch64_cfun_has_state (const char *state_name)
{
  return aarch64_fndecl_has_state (cfun->decl, state_name);
}

/* Return true if a call from the current function to a function with
   ISA mode CALLEE_MODE would involve a change to PSTATE.SM around
   the BL instruction.  */

static bool
aarch64_call_switches_pstate_sm (aarch64_isa_mode callee_mode)
{
  return (bool) (callee_mode & ~AARCH64_ISA_MODE & AARCH64_ISA_MODE_SM_STATE);
}

/* Implement TARGET_COMPATIBLE_VECTOR_TYPES_P.  */

static bool
aarch64_compatible_vector_types_p (const_tree type1, const_tree type2)
{
  return (aarch64_sve::builtin_type_p (type1)
	  == aarch64_sve::builtin_type_p (type2));
}

/* Return true if we should emit CFI for register REGNO.  */

static bool
aarch64_emit_cfi_for_reg_p (unsigned int regno)
{
  return (GP_REGNUM_P (regno)
	  || !default_function_abi.clobbers_full_reg_p (regno));
}

/* Return the mode we should use to save and restore register REGNO.  */

static machine_mode
aarch64_reg_save_mode (unsigned int regno)
{
  if (GP_REGNUM_P (regno) || regno == VG_REGNUM)
    return DImode;

  if (FP_REGNUM_P (regno))
    switch (crtl->abi->id ())
      {
      case ARM_PCS_AAPCS64:
	/* Only the low 64 bits are saved by the base PCS.  */
	return DFmode;

      case ARM_PCS_SIMD:
	/* The vector PCS saves the low 128 bits (which is the full
	   register on non-SVE targets).  */
	return V16QImode;

      case ARM_PCS_SVE:
	/* Use vectors of DImode for registers that need frame
	   information, so that the first 64 bytes of the save slot
	   are always the equivalent of what storing D<n> would give.  */
	if (aarch64_emit_cfi_for_reg_p (regno))
	  return VNx2DImode;

	/* Use vectors of bytes otherwise, so that the layout is
	   endian-agnostic, and so that we can use LDR and STR for
	   big-endian targets.  */
	return VNx16QImode;

      case ARM_PCS_TLSDESC:
      case ARM_PCS_UNKNOWN:
	break;
      }

  if (PR_REGNUM_P (regno))
    /* Save the full predicate register.  */
    return VNx16BImode;

  gcc_unreachable ();
}

/* Return the CONST_INT that should be placed in an UNSPEC_CALLEE_ABI rtx.
   This value encodes the following information:
    - the ISA mode on entry to a callee (ISA_MODE)
    - the ABI of the callee (PCS_VARIANT)
    - whether the callee has an indirect_return
      attribute (INDIRECT_RETURN).  */

rtx
aarch64_gen_callee_cookie (aarch64_isa_mode isa_mode, arm_pcs pcs_variant,
			   bool indirect_return)
{
  unsigned int im = (unsigned int) isa_mode;
  unsigned int ir = (indirect_return ? 1 : 0) << AARCH64_NUM_ISA_MODES;
  unsigned int pv = (unsigned int) pcs_variant
		     << (AARCH64_NUM_ABI_ATTRIBUTES + AARCH64_NUM_ISA_MODES);
  return gen_int_mode (im | ir | pv, DImode);
}

/* COOKIE is a CONST_INT from an UNSPEC_CALLEE_ABI rtx.  Return the
   callee's ABI.  */

static const predefined_function_abi &
aarch64_callee_abi (rtx cookie)
{
  return function_abis[UINTVAL (cookie)
	 >> (AARCH64_NUM_ABI_ATTRIBUTES + AARCH64_NUM_ISA_MODES)];
}

/* COOKIE is a CONST_INT from an UNSPEC_CALLEE_ABI rtx.  Return the
   required ISA mode on entry to the callee, which is also the ISA
   mode on return from the callee.  */

static aarch64_isa_mode
aarch64_callee_isa_mode (rtx cookie)
{
  return UINTVAL (cookie) & ((1 << AARCH64_NUM_ISA_MODES) - 1);
}

/* COOKIE is a CONST_INT from an UNSPEC_CALLEE_ABI rtx.  Return
   whether function was marked with an indirect_return attribute.  */

static bool
aarch64_callee_indirect_return (rtx cookie)
{
  return ((UINTVAL (cookie) >> AARCH64_NUM_ISA_MODES) & 1) == 1;
}

/* INSN is a call instruction.  Return the CONST_INT stored in its
   UNSPEC_CALLEE_ABI rtx.  */

static rtx
aarch64_insn_callee_cookie (const rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  gcc_assert (GET_CODE (pat) == PARALLEL);
  rtx unspec = XVECEXP (pat, 0, 1);
  gcc_assert (GET_CODE (unspec) == UNSPEC
	      && XINT (unspec, 1) == UNSPEC_CALLEE_ABI);
  return XVECEXP (unspec, 0, 0);
}

/* INSN is a call instruction.  Return true if the callee has an
   indirect_return attribute.  */

bool
aarch_fun_is_indirect_return (rtx_insn *insn)
{
  rtx cookie = aarch64_insn_callee_cookie (insn);
  return aarch64_callee_indirect_return (cookie);
}

/* Implement TARGET_INSN_CALLEE_ABI.  */

const predefined_function_abi &
aarch64_insn_callee_abi (const rtx_insn *insn)
{
  return aarch64_callee_abi (aarch64_insn_callee_cookie (insn));
}

/* INSN is a call instruction.  Return the required ISA mode on entry to
   the callee, which is also the ISA mode on return from the callee.  */

static aarch64_isa_mode
aarch64_insn_callee_isa_mode (const rtx_insn *insn)
{
  return aarch64_callee_isa_mode (aarch64_insn_callee_cookie (insn));
}

/* Implement TARGET_HARD_REGNO_CALL_PART_CLOBBERED.  The callee only saves
   the lower 64 bits of a 128-bit register.  Tell the compiler the callee
   clobbers the top 64 bits when restoring the bottom 64 bits.  */

static bool
aarch64_hard_regno_call_part_clobbered (unsigned int abi_id,
					unsigned int regno,
					machine_mode mode)
{
  if (FP_REGNUM_P (regno) && abi_id != ARM_PCS_SVE)
    {
      poly_int64 per_register_size = GET_MODE_SIZE (mode);
      unsigned int nregs = hard_regno_nregs (regno, mode);
      if (nregs > 1)
	per_register_size = exact_div (per_register_size, nregs);
      if (abi_id == ARM_PCS_SIMD || abi_id == ARM_PCS_TLSDESC)
	return maybe_gt (per_register_size, 16);
      return maybe_gt (per_register_size, 8);
    }
  return false;
}

/* Implement REGMODE_NATURAL_SIZE.  */
poly_uint64
aarch64_regmode_natural_size (machine_mode mode)
{
  /* The natural size for SVE data modes is one SVE data vector,
     and similarly for predicates.  We can't independently modify
     anything smaller than that.  */
  /* ??? For now, only do this for variable-width SVE registers.
     Doing it for constant-sized registers breaks lower-subreg.cc.  */
  /* ??? And once that's fixed, we should probably have similar
     code for Advanced SIMD.  */
  if (!aarch64_sve_vg.is_constant ())
    {
      /* REGMODE_NATURAL_SIZE influences general subreg validity rules,
	 so we need to handle memory-only modes as well.  */
      unsigned int vec_flags = aarch64_classify_vector_memory_mode (mode);
      if (vec_flags & VEC_SVE_PRED)
	return BYTES_PER_SVE_PRED;
      if (vec_flags & VEC_SVE_DATA)
	return BYTES_PER_SVE_VECTOR;
    }
  return UNITS_PER_WORD;
}

/* Implement HARD_REGNO_CALLER_SAVE_MODE.  */
machine_mode
aarch64_hard_regno_caller_save_mode (unsigned regno, unsigned,
				     machine_mode mode)
{
  /* The predicate mode determines which bits are significant and
     which are "don't care".  Decreasing the number of lanes would
     lose data while increasing the number of lanes would make bits
     unnecessarily significant.  */
  if (PR_REGNUM_P (regno))
    return mode;
  if (known_lt (GET_MODE_SIZE (mode), 4)
      && REG_CAN_CHANGE_MODE_P (regno, mode, SImode)
      && REG_CAN_CHANGE_MODE_P (regno, SImode, mode))
    return SImode;
  return mode;
}

/* Return true if I's bits are consecutive ones from the MSB.  */
bool
aarch64_high_bits_all_ones_p (HOST_WIDE_INT i)
{
  return exact_log2 (-i) != HOST_WIDE_INT_M1;
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  Make strings word-aligned so
   that strcpy from constants will be faster.  */

static HOST_WIDE_INT
aarch64_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST && !optimize_size)
    return MAX (align, BITS_PER_WORD);
  return align;
}

/* Return true if calls to DECL should be treated as
   long-calls (ie called via a register).  */
static bool
aarch64_decl_is_long_call_p (const_tree decl ATTRIBUTE_UNUSED)
{
  return false;
}

/* Return true if calls to symbol-ref SYM should be treated as
   long-calls (ie called via a register).  */
bool
aarch64_is_long_call_p (rtx sym)
{
  return aarch64_decl_is_long_call_p (SYMBOL_REF_DECL (sym));
}

/* Return true if calls to symbol-ref SYM should not go through
   plt stubs.  */

bool
aarch64_is_noplt_call_p (rtx sym)
{
  const_tree decl = SYMBOL_REF_DECL (sym);

  if (flag_pic
      && decl
      && (!flag_plt
	  || lookup_attribute ("noplt", DECL_ATTRIBUTES (decl)))
      && !targetm.binds_local_p (decl))
    return true;

  return false;
}

/* Emit an insn that's a simple single-set.  Both the operands must be
   known to be valid.  */
inline static rtx_insn *
emit_set_insn (rtx x, rtx y)
{
  return emit_insn (gen_rtx_SET (x, y));
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  */
rtx
aarch64_gen_compare_reg (RTX_CODE code, rtx x, rtx y)
{
  machine_mode cmp_mode = GET_MODE (x);
  machine_mode cc_mode;
  rtx cc_reg;

  if (cmp_mode == TImode)
    {
      gcc_assert (code == NE);

      cc_mode = CCmode;
      cc_reg = gen_rtx_REG (cc_mode, CC_REGNUM);

      rtx x_lo = operand_subword (x, 0, 0, TImode);
      rtx y_lo = operand_subword (y, 0, 0, TImode);
      emit_set_insn (cc_reg, gen_rtx_COMPARE (cc_mode, x_lo, y_lo));

      rtx x_hi = operand_subword (x, 1, 0, TImode);
      rtx y_hi = operand_subword (y, 1, 0, TImode);
      emit_insn (gen_ccmpccdi (cc_reg, cc_reg, x_hi, y_hi,
			       gen_rtx_EQ (cc_mode, cc_reg, const0_rtx),
			       GEN_INT (AARCH64_EQ)));
    }
  else
    {
      cc_mode = SELECT_CC_MODE (code, x, y);
      cc_reg = gen_rtx_REG (cc_mode, CC_REGNUM);
      emit_set_insn (cc_reg, gen_rtx_COMPARE (cc_mode, x, y));
    }
  return cc_reg;
}

/* Similarly, but maybe zero-extend Y if Y_MODE < SImode.  */

static rtx
aarch64_gen_compare_reg_maybe_ze (RTX_CODE code, rtx x, rtx y,
                                  machine_mode y_mode)
{
  if (y_mode == E_QImode || y_mode == E_HImode)
    {
      if (CONST_INT_P (y))
	{
	  y = GEN_INT (INTVAL (y) & GET_MODE_MASK (y_mode));
	  y_mode = SImode;
	}
      else
	{
	  rtx t, cc_reg;
	  machine_mode cc_mode;

	  t = gen_rtx_ZERO_EXTEND (SImode, y);
	  t = gen_rtx_COMPARE (CC_SWPmode, t, x);
	  cc_mode = CC_SWPmode;
	  cc_reg = gen_rtx_REG (cc_mode, CC_REGNUM);
	  emit_set_insn (cc_reg, t);
	  return cc_reg;
	}
    }

  if (!aarch64_plus_operand (y, y_mode))
    y = force_reg (y_mode, y);

  return aarch64_gen_compare_reg (code, x, y);
}

/* Generate conditional branch to LABEL, comparing X to 0 using CODE.
   Return the jump instruction.  */

static rtx
aarch64_gen_compare_zero_and_branch (rtx_code code, rtx x,
				     rtx_code_label *label)
{
  if (aarch64_track_speculation)
    {
      /* Emit an explicit compare instruction, so that we can correctly
	 track the condition codes.  */
      rtx cc_reg = aarch64_gen_compare_reg (code, x, const0_rtx);
      x = gen_rtx_fmt_ee (code, GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
  else
    x = gen_rtx_fmt_ee (code, VOIDmode, x, const0_rtx);

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label), pc_rtx);
  return gen_rtx_SET (pc_rtx, x);
}

/* Return an rtx that branches to LABEL based on the value of bit BITNUM of X.
   If CODE is NE, it branches to LABEL when the bit is set; if CODE is EQ,
   it branches to LABEL when the bit is clear.  */

static rtx
aarch64_gen_test_and_branch (rtx_code code, rtx x, int bitnum,
			     rtx_code_label *label)
{
  auto mode = GET_MODE (x);
  if (aarch64_track_speculation)
    {
      auto mask = gen_int_mode (HOST_WIDE_INT_1U << bitnum, mode);
      emit_insn (gen_aarch64_and3nr_compare0 (mode, x, mask));
      rtx cc_reg = gen_rtx_REG (CC_NZVmode, CC_REGNUM);
      rtx x = gen_rtx_fmt_ee (code, CC_NZVmode, cc_reg, const0_rtx);
      return gen_condjump (x, cc_reg, label);
    }
  return gen_aarch64_tb (code, mode, mode,
			 x, gen_int_mode (bitnum, mode), label);
}

/* Consider the operation:

     OPERANDS[0] = CODE (OPERANDS[1], OPERANDS[2]) + OPERANDS[3]

   where:

   - CODE is [SU]MAX or [SU]MIN
   - OPERANDS[2] and OPERANDS[3] are constant integers
   - OPERANDS[3] is a positive or negative shifted 12-bit immediate
   - all operands have mode MODE

   Decide whether it is possible to implement the operation using:

     SUBS <tmp>, OPERANDS[1], -OPERANDS[3]
     or
     ADDS <tmp>, OPERANDS[1], OPERANDS[3]

   followed by:

     <insn> OPERANDS[0], <tmp>, [wx]zr, <cond>

   where <insn> is one of CSEL, CSINV or CSINC.  Return true if so.
   If GENERATE_P is true, also update OPERANDS as follows:

     OPERANDS[4] = -OPERANDS[3]
     OPERANDS[5] = the rtl condition representing <cond>
     OPERANDS[6] = <tmp>
     OPERANDS[7] = 0 for CSEL, -1 for CSINV or 1 for CSINC.  */
bool
aarch64_maxmin_plus_const (rtx_code code, rtx *operands, bool generate_p)
{
  signop sgn = (code == UMAX || code == UMIN ? UNSIGNED : SIGNED);
  rtx dst = operands[0];
  rtx maxmin_op = operands[2];
  rtx add_op = operands[3];
  machine_mode mode = GET_MODE (dst);

  /* max (x, y) - z == (x >= y + 1 ? x : y) - z
		    == (x >= y ? x : y) - z
		    == (x > y ? x : y) - z
		    == (x > y - 1 ? x : y) - z

     min (x, y) - z == (x <= y - 1 ? x : y) - z
		    == (x <= y ? x : y) - z
		    == (x < y ? x : y) - z
		    == (x < y + 1 ? x : y) - z

     Check whether z is in { y - 1, y, y + 1 } and pick the form(s) for
     which x is compared with z.  Set DIFF to y - z.  Thus the supported
     combinations are as follows, with DIFF being the value after the ":":

     max (x, y) - z == x >= y + 1 ? x - (y + 1) : -1   [z == y + 1]
		    == x >= y ? x - y : 0              [z == y]
		    == x > y ? x - y : 0               [z == y]
		    == x > y - 1 ? x - (y - 1) : 1     [z == y - 1]

     min (x, y) - z == x <= y - 1 ? x - (y - 1) : 1    [z == y - 1]
		    == x <= y ? x - y : 0              [z == y]
		    == x < y ? x - y : 0               [z == y]
		    == x < y + 1 ? x - (y + 1) : -1    [z == y + 1].  */
  auto maxmin_val = rtx_mode_t (maxmin_op, mode);
  auto add_val = rtx_mode_t (add_op, mode);
  auto sub_val = wi::neg (add_val);
  auto diff = wi::sub (maxmin_val, sub_val);
  if (!(diff == 0
	|| (diff == 1 && wi::gt_p (maxmin_val, sub_val, sgn))
	|| (diff == -1 && wi::lt_p (maxmin_val, sub_val, sgn))))
    return false;

  if (!generate_p)
    return true;

  rtx_code cmp;
  switch (code)
    {
    case SMAX:
      cmp = diff == 1 ? GT : GE;
      break;
    case UMAX:
      cmp = diff == 1 ? GTU : GEU;
      break;
    case SMIN:
      cmp = diff == -1 ? LT : LE;
      break;
    case UMIN:
      cmp = diff == -1 ? LTU : LEU;
      break;
    default:
      gcc_unreachable ();
    }
  rtx cc = gen_rtx_REG (CCmode, CC_REGNUM);

  operands[4] = immed_wide_int_const (sub_val, mode);
  operands[5] = gen_rtx_fmt_ee (cmp, VOIDmode, cc, const0_rtx);
  if (can_create_pseudo_p ())
    operands[6] = gen_reg_rtx (mode);
  else
    operands[6] = dst;
  operands[7] = immed_wide_int_const (diff, mode);

  return true;
}


/* Build the SYMBOL_REF for __tls_get_addr.  */

static GTY(()) rtx tls_get_addr_libfunc;

rtx
aarch64_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}

/* Return the TLS model to use for ADDR.  */

static enum tls_model
tls_symbolic_operand_type (rtx addr)
{
  enum tls_model tls_kind = TLS_MODEL_NONE;
  poly_int64 offset;
  addr = strip_offset_and_salt (addr, &offset);
  if (SYMBOL_REF_P (addr))
    tls_kind = SYMBOL_REF_TLS_MODEL (addr);

  return tls_kind;
}

/* We'll allow lo_sum's in addresses in our legitimate addresses
   so that combine would take care of combining addresses where
   necessary, but for generation purposes, we'll generate the address
   as :
   RTL                               Absolute
   tmp = hi (symbol_ref);            adrp  x1, foo
   dest = lo_sum (tmp, symbol_ref);  add dest, x1, :lo_12:foo
                                     nop

   PIC                               TLS
   adrp x1, :got:foo                 adrp tmp, :tlsgd:foo
   ldr  x1, [:got_lo12:foo]          add  dest, tmp, :tlsgd_lo12:foo
                                     bl   __tls_get_addr
                                     nop

   Load TLS symbol, depending on TLS mechanism and TLS access model.

   Global Dynamic - Traditional TLS:
   adrp tmp, :tlsgd:imm
   add  dest, tmp, #:tlsgd_lo12:imm
   bl   __tls_get_addr

   Global Dynamic - TLS Descriptors:
   adrp dest, :tlsdesc:imm
   ldr  tmp, [dest, #:tlsdesc_lo12:imm]
   add  dest, dest, #:tlsdesc_lo12:imm
   blr  tmp
   mrs  tp, tpidr_el0
   add  dest, dest, tp

   Initial Exec:
   mrs  tp, tpidr_el0
   adrp tmp, :gottprel:imm
   ldr  dest, [tmp, #:gottprel_lo12:imm]
   add  dest, dest, tp

   Local Exec:
   mrs  tp, tpidr_el0
   add  t0, tp, #:tprel_hi12:imm, lsl #12
   add  t0, t0, #:tprel_lo12_nc:imm
*/

static void
aarch64_load_symref_appropriately (rtx dest, rtx imm,
				   enum aarch64_symbol_type type)
{
#if TARGET_PECOFF
  rtx tmp = legitimize_pe_coff_symbol (imm, true);
  if (tmp)
    {
      emit_insn (gen_rtx_SET (dest, tmp));
      return;
    }
#endif

  switch (type)
    {
    case SYMBOL_SMALL_ABSOLUTE:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode.  */
	rtx tmp_reg = dest;
	machine_mode mode = GET_MODE (dest);

	gcc_assert (mode == Pmode || mode == ptr_mode);

	if (can_create_pseudo_p ())
	  tmp_reg = gen_reg_rtx (mode);

	HOST_WIDE_INT mid_const = 0;
	if (TARGET_PECOFF)
	  {
	    poly_int64 offset;
	    strip_offset (imm, &offset);

	    HOST_WIDE_INT const_offset;
	    if (offset.is_constant (&const_offset))
	      /* Written this way for the sake of negative offsets.  */
	      mid_const = const_offset / (1 << 20) * (1 << 20);
	  }
	imm = plus_constant (mode, imm, -mid_const);

	emit_move_insn (tmp_reg, gen_rtx_HIGH (mode, copy_rtx (imm)));
	if (mid_const)
	  emit_set_insn (tmp_reg, plus_constant (mode, tmp_reg, mid_const));
	emit_insn (gen_add_losym (dest, tmp_reg, imm));
	return;
      }

    case SYMBOL_TINY_ABSOLUTE:
      emit_insn (gen_rtx_SET (dest, imm));
      return;

    case SYMBOL_SMALL_GOT_28K:
      {
	machine_mode mode = GET_MODE (dest);
	rtx gp_rtx = pic_offset_table_rtx;
	rtx insn;
	rtx mem;

	/* NOTE: pic_offset_table_rtx can be NULL_RTX, because we can reach
	   here before rtl expand.  Tree IVOPT will generate rtl pattern to
	   decide rtx costs, in which case pic_offset_table_rtx is not
	   initialized.  For that case no need to generate the first adrp
	   instruction as the final cost for global variable access is
	   one instruction.  */
	if (gp_rtx != NULL)
	  {
	    /* -fpic for -mcmodel=small allow 32K GOT table size (but we are
	       using the page base as GOT base, the first page may be wasted,
	       in the worst scenario, there is only 28K space for GOT).

	       The generate instruction sequence for accessing global variable
	       is:

		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym]

	       Only one instruction needed. But we must initialize
	       pic_offset_table_rtx properly.  We generate initialize insn for
	       every global access, and allow CSE to remove all redundant.

	       The final instruction sequences will look like the following
	       for multiply global variables access.

		 adrp pic_offset_table_rtx, _GLOBAL_OFFSET_TABLE_

		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym1]
		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym2]
		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym3]
		 ...  */

	    rtx s = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
	    crtl->uses_pic_offset_table = 1;
	    emit_move_insn (gp_rtx, gen_rtx_HIGH (Pmode, s));

	    if (mode != GET_MODE (gp_rtx))
             gp_rtx = gen_lowpart (mode, gp_rtx);

	  }

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      insn = gen_ldr_got_small_28k_di (dest, gp_rtx, imm);
	    else
	      insn = gen_ldr_got_small_28k_si (dest, gp_rtx, imm);

	    mem = XVECEXP (SET_SRC (insn), 0, 0);
	  }
	else
	  {
	    gcc_assert (mode == Pmode);

	    insn = gen_ldr_got_small_28k_sidi (dest, gp_rtx, imm);
	    mem = XVECEXP (XEXP (SET_SRC (insn), 0), 0, 0);
	  }

	/* The operand is expected to be MEM.  Whenever the related insn
	   pattern changed, above code which calculate mem should be
	   updated.  */
	gcc_assert (MEM_P (mem));
	MEM_READONLY_P (mem) = 1;
	MEM_NOTRAP_P (mem) = 1;
	emit_insn (insn);
	return;
      }

    case SYMBOL_SMALL_GOT_4G:
      emit_insn (gen_rtx_SET (dest, imm));
      return;

    case SYMBOL_SMALL_TLSGD:
      {
	rtx_insn *insns;
	/* The return type of __tls_get_addr is the C pointer type
	   so use ptr_mode.  */
	rtx result = gen_rtx_REG (ptr_mode, R0_REGNUM);
	rtx tmp_reg = dest;

	if (GET_MODE (dest) != ptr_mode)
	  tmp_reg = can_create_pseudo_p () ? gen_reg_rtx (ptr_mode) : result;

	start_sequence ();
	if (ptr_mode == SImode)
	  aarch64_emit_call_insn (gen_tlsgd_small_si (result, imm));
	else
	  aarch64_emit_call_insn (gen_tlsgd_small_di (result, imm));
	insns = get_insns ();
	end_sequence ();

	RTL_CONST_CALL_P (insns) = 1;
	emit_libcall_block (insns, tmp_reg, result, imm);
	/* Convert back to the mode of the dest adding a zero_extend
	   from SImode (ptr_mode) to DImode (Pmode). */
	if (dest != tmp_reg)
	  convert_move (dest, tmp_reg, true);
	return;
      }

    case SYMBOL_SMALL_TLSDESC:
      {
	machine_mode mode = GET_MODE (dest);
	rtx x0 = gen_rtx_REG (mode, R0_REGNUM);
	rtx tp;

	gcc_assert (mode == Pmode || mode == ptr_mode);

	/* In ILP32, the got entry is always of SImode size.  Unlike
	   small GOT, the dest is fixed at reg 0.  */
	if (TARGET_ILP32)
	  emit_insn (gen_tlsdesc_small_si (imm));
	else
	  emit_insn (gen_tlsdesc_small_di (imm));
	tp = aarch64_load_tp (NULL);

	if (mode != Pmode)
	  tp = gen_lowpart (mode, tp);

	emit_insn (gen_rtx_SET (dest, gen_rtx_PLUS (mode, tp, x0)));
	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_SMALL_TLSIE:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode,
	   while the got entry is always of SImode size.  The mode of
	   dest depends on how dest is used: if dest is assigned to a
	   pointer (e.g. in the memory), it has SImode; it may have
	   DImode if dest is dereferenced to access the memeory.
	   This is why we have to handle three different tlsie_small
	   patterns here (two patterns for ILP32).  */
	machine_mode mode = GET_MODE (dest);
	rtx tmp_reg = gen_reg_rtx (mode);
	rtx tp = aarch64_load_tp (NULL);

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      emit_insn (gen_tlsie_small_di (tmp_reg, imm));
	    else
	      {
		emit_insn (gen_tlsie_small_si (tmp_reg, imm));
		tp = gen_lowpart (mode, tp);
	      }
	  }
	else
	  {
	    gcc_assert (mode == Pmode);
	    emit_insn (gen_tlsie_small_sidi (tmp_reg, imm));
	  }

	emit_insn (gen_rtx_SET (dest, gen_rtx_PLUS (mode, tp, tmp_reg)));
	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_TLSLE12:
    case SYMBOL_TLSLE24:
    case SYMBOL_TLSLE32:
    case SYMBOL_TLSLE48:
      {
	machine_mode mode = GET_MODE (dest);
	rtx tp = aarch64_load_tp (NULL);

	if (mode != Pmode)
	  tp = gen_lowpart (mode, tp);

	switch (type)
	  {
	  case SYMBOL_TLSLE12:
	    emit_insn ((mode == DImode ? gen_tlsle12_di : gen_tlsle12_si)
			(dest, tp, imm));
	    break;
	  case SYMBOL_TLSLE24:
	    emit_insn ((mode == DImode ? gen_tlsle24_di : gen_tlsle24_si)
			(dest, tp, imm));
	  break;
	  case SYMBOL_TLSLE32:
	    emit_insn ((mode == DImode ? gen_tlsle32_di : gen_tlsle32_si)
			(dest, imm));
	    emit_insn ((mode == DImode ? gen_adddi3 : gen_addsi3)
			(dest, dest, tp));
	  break;
	  case SYMBOL_TLSLE48:
	    emit_insn ((mode == DImode ? gen_tlsle48_di : gen_tlsle48_si)
			(dest, imm));
	    emit_insn ((mode == DImode ? gen_adddi3 : gen_addsi3)
			(dest, dest, tp));
	    break;
	  default:
	    gcc_unreachable ();
	  }

	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_TINY_GOT:
      {
	rtx insn;
	machine_mode mode = GET_MODE (dest);

	if (mode == ptr_mode)
	  insn = gen_ldr_got_tiny (mode, dest, imm);
	else
	  {
	    gcc_assert (mode == Pmode);
	    insn = gen_ldr_got_tiny_sidi (dest, imm);
	  }

	emit_insn (insn);
	return;
      }

    case SYMBOL_TINY_TLSIE:
      {
	machine_mode mode = GET_MODE (dest);
	rtx tp = aarch64_load_tp (NULL);

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      emit_insn (gen_tlsie_tiny_di (dest, imm, tp));
	    else
	      {
		tp = gen_lowpart (mode, tp);
		emit_insn (gen_tlsie_tiny_si (dest, imm, tp));
	      }
	  }
	else
	  {
	    gcc_assert (mode == Pmode);
	    emit_insn (gen_tlsie_tiny_sidi (dest, imm, tp));
	  }

	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    default:
      gcc_unreachable ();
    }
}

/* Emit a move from SRC to DEST.  Assume that the move expanders can
   handle all moves if !can_create_pseudo_p ().  The distinction is
   important because, unlike emit_move_insn, the move expanders know
   how to force Pmode objects into the constant pool even when the
   constant pool address is not itself legitimate.  */
static rtx
aarch64_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p ()
	  ? emit_move_insn (dest, src)
	  : emit_move_insn_1 (dest, src));
}

/* Apply UNOPTAB to OP and store the result in DEST.  */

static void
aarch64_emit_unop (rtx dest, optab unoptab, rtx op)
{
  rtx tmp = expand_unop (GET_MODE (dest), unoptab, op, dest, 0);
  if (dest != tmp)
    emit_move_insn (dest, tmp);
}

/* Apply BINOPTAB to OP0 and OP1 and store the result in DEST.  */

static void
aarch64_emit_binop (rtx dest, optab binoptab, rtx op0, rtx op1)
{
  rtx tmp = expand_binop (GET_MODE (dest), binoptab, op0, op1, dest, 0,
			  OPTAB_DIRECT);
  if (dest != tmp)
    emit_move_insn (dest, tmp);
}

/* Split a move from SRC to DST into multiple moves of mode SINGLE_MODE.  */

void
aarch64_split_move (rtx dst, rtx src, machine_mode single_mode)
{
  machine_mode mode = GET_MODE (dst);
  auto npieces = exact_div (GET_MODE_SIZE (mode),
			    GET_MODE_SIZE (single_mode)).to_constant ();
  auto_vec<rtx, 4> dst_pieces, src_pieces;

  for (unsigned int i = 0; i < npieces; ++i)
    {
      auto off = i * GET_MODE_SIZE (single_mode);
      dst_pieces.safe_push (simplify_gen_subreg (single_mode, dst, mode, off));
      src_pieces.safe_push (simplify_gen_subreg (single_mode, src, mode, off));
    }

  /* At most one pairing may overlap.  */
  if (reg_overlap_mentioned_p (dst_pieces[0], src))
    for (unsigned int i = npieces; i-- > 0;)
      aarch64_emit_move (dst_pieces[i], src_pieces[i]);
  else
    for (unsigned int i = 0; i < npieces; ++i)
      aarch64_emit_move (dst_pieces[i], src_pieces[i]);
}

/* Split a 128-bit move operation into two 64-bit move operations,
   taking care to handle partial overlap of register to register
   copies.  Special cases are needed when moving between GP regs and
   FP regs.  SRC can be a register, constant or memory; DST a register
   or memory.  If either operand is memory it must not have any side
   effects.  */
void
aarch64_split_128bit_move (rtx dst, rtx src)
{
  machine_mode mode = GET_MODE (dst);

  gcc_assert (mode == TImode || mode == TFmode || mode == TDmode);
  gcc_assert (!(side_effects_p (src) || side_effects_p (dst)));
  gcc_assert (mode == GET_MODE (src) || GET_MODE (src) == VOIDmode);

  if (REG_P (dst) && REG_P (src))
    {
      int src_regno = REGNO (src);
      int dst_regno = REGNO (dst);

      /* Handle FP <-> GP regs.  */
      if (FP_REGNUM_P (dst_regno) && GP_REGNUM_P (src_regno))
	{
	  rtx src_lo = gen_lowpart (word_mode, src);
	  rtx src_hi = gen_highpart (word_mode, src);

	  emit_insn (gen_aarch64_movlow_di (mode, dst, src_lo));
	  emit_insn (gen_aarch64_movhigh_di (mode, dst, src_hi));
	  return;
	}
      else if (GP_REGNUM_P (dst_regno) && FP_REGNUM_P (src_regno))
	{
	  rtx dst_lo = gen_lowpart (word_mode, dst);
	  rtx dst_hi = gen_highpart (word_mode, dst);

	  emit_insn (gen_aarch64_movdi_low (mode, dst_lo, src));
	  emit_insn (gen_aarch64_movdi_high (mode, dst_hi, src));
	  return;
	}
    }

  aarch64_split_move (dst, src, word_mode);
}

/* Return true if we should split a move from 128-bit value SRC
   to 128-bit register DEST.  */

bool
aarch64_split_128bit_move_p (rtx dst, rtx src)
{
  if (FP_REGNUM_P (REGNO (dst)))
    return REG_P (src) && !FP_REGNUM_P (REGNO (src));
  /* All moves to GPRs need to be split.  */
  return true;
}

/* Split a complex SIMD move.  */

void
aarch64_split_simd_move (rtx dst, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  machine_mode dst_mode = GET_MODE (dst);

  gcc_assert (VECTOR_MODE_P (dst_mode));

  if (REG_P (dst) && REG_P (src))
    {
      gcc_assert (VECTOR_MODE_P (src_mode));
      emit_insn (gen_aarch64_split_simd_mov (src_mode, dst, src));
    }
}

/* Return a register that contains SVE value X reinterpreted as SVE mode MODE.
   The semantics of those of svreinterpret rather than those of subregs;
   see the comment at the head of aarch64-sve.md for details about the
   difference.  */

rtx
aarch64_sve_reinterpret (machine_mode mode, rtx x)
{
  if (GET_MODE (x) == mode)
    return x;

  /* can_change_mode_class must only return true if subregs and svreinterprets
     have the same semantics.  */
  if (targetm.can_change_mode_class (GET_MODE (x), mode, FP_REGS))
    return force_lowpart_subreg (mode, x, GET_MODE (x));

  rtx res = gen_reg_rtx (mode);
  x = force_reg (GET_MODE (x), x);
  emit_insn (gen_aarch64_sve_reinterpret (mode, res, x));
  return res;
}

bool
aarch64_zero_extend_const_eq (machine_mode xmode, rtx x,
			      machine_mode ymode, rtx y)
{
  rtx r = simplify_const_unary_operation (ZERO_EXTEND, xmode, y, ymode);
  gcc_assert (r != NULL);
  return rtx_equal_p (x, r);
}

/* Return TARGET if it is nonnull and a register of mode MODE.
   Otherwise, return a fresh register of mode MODE if we can,
   or TARGET reinterpreted as MODE if we can't.  */

static rtx
aarch64_target_reg (rtx target, machine_mode mode)
{
  if (target && REG_P (target) && GET_MODE (target) == mode)
    return target;
  if (!can_create_pseudo_p ())
    {
      gcc_assert (target);
      return gen_lowpart (mode, target);
    }
  return gen_reg_rtx (mode);
}

/* Return a register that contains the constant in BUILDER, given that
   the constant is a legitimate move operand.  Use TARGET as the register
   if it is nonnull and convenient.  */

static rtx
aarch64_emit_set_immediate (rtx target, rtx_vector_builder &builder)
{
  rtx src = builder.build ();
  target = aarch64_target_reg (target, GET_MODE (src));
  emit_insn (gen_rtx_SET (target, src));
  return target;
}

static rtx
aarch64_force_temporary (machine_mode mode, rtx x, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (mode, value);
  else
    {
      gcc_assert (x);
      aarch64_emit_move (x, value);
      return x;
    }
}

/* Return true if predicate value X is a constant in which every element
   is a CONST_INT.  When returning true, describe X in BUILDER as a VNx16BI
   value, i.e. as a predicate in which all bits are significant.  */

static bool
aarch64_get_sve_pred_bits (rtx_vector_builder &builder, rtx x)
{
  if (!CONST_VECTOR_P (x))
    return false;

  unsigned int factor = vector_element_size (GET_MODE_NUNITS (VNx16BImode),
					     GET_MODE_NUNITS (GET_MODE (x)));
  unsigned int npatterns = CONST_VECTOR_NPATTERNS (x) * factor;
  unsigned int nelts_per_pattern = CONST_VECTOR_NELTS_PER_PATTERN (x);
  builder.new_vector (VNx16BImode, npatterns, nelts_per_pattern);

  unsigned int nelts = const_vector_encoded_nelts (x);
  for (unsigned int i = 0; i < nelts; ++i)
    {
      rtx elt = CONST_VECTOR_ENCODED_ELT (x, i);
      if (!CONST_INT_P (elt))
	return false;

      builder.quick_push (elt);
      for (unsigned int j = 1; j < factor; ++j)
	builder.quick_push (const0_rtx);
    }
  builder.finalize ();
  return true;
}

/* BUILDER contains a predicate constant of mode VNx16BI.  Return the
   widest predicate element size it can have (that is, the largest size
   for which each element would still be 0 or 1).  */

unsigned int
aarch64_widest_sve_pred_elt_size (rtx_vector_builder &builder)
{
  /* Start with the most optimistic assumption: that we only need
     one bit per pattern.  This is what we will use if only the first
     bit in each pattern is ever set.  */
  unsigned int mask = GET_MODE_SIZE (DImode);
  mask |= builder.npatterns ();

  /* Look for set bits.  */
  unsigned int nelts = builder.encoded_nelts ();
  for (unsigned int i = 1; i < nelts; ++i)
    if (INTVAL (builder.elt (i)) != 0)
      {
	if (i & 1)
	  return 1;
	mask |= i;
      }
  return mask & -mask;
}

/* If VNx16BImode rtx X is a canonical PTRUE for a predicate mode,
   return that predicate mode, otherwise return opt_machine_mode ().  */

opt_machine_mode
aarch64_ptrue_all_mode (rtx x)
{
  gcc_assert (GET_MODE (x) == VNx16BImode);
  if (!CONST_VECTOR_P (x)
      || !CONST_VECTOR_DUPLICATE_P (x)
      || !CONST_INT_P (CONST_VECTOR_ENCODED_ELT (x, 0))
      || INTVAL (CONST_VECTOR_ENCODED_ELT (x, 0)) == 0)
    return opt_machine_mode ();

  unsigned int nelts = const_vector_encoded_nelts (x);
  for (unsigned int i = 1; i < nelts; ++i)
    if (CONST_VECTOR_ENCODED_ELT (x, i) != const0_rtx)
      return opt_machine_mode ();

  return aarch64_sve_pred_mode (nelts);
}

/* BUILDER is a predicate constant of mode VNx16BI.  Consider the value
   that the constant would have with predicate element size ELT_SIZE
   (ignoring the upper bits in each element) and return:

   * -1 if all bits are set
   * N if the predicate has N leading set bits followed by all clear bits
   * 0 if the predicate does not have any of these forms.  */

int
aarch64_partial_ptrue_length (rtx_vector_builder &builder,
			      unsigned int elt_size)
{
  /* If nelts_per_pattern is 3, we have set bits followed by clear bits
     followed by set bits.  */
  if (builder.nelts_per_pattern () == 3)
    return 0;

  /* Skip over leading set bits.  */
  unsigned int nelts = builder.encoded_nelts ();
  unsigned int i = 0;
  for (; i < nelts; i += elt_size)
    if (INTVAL (builder.elt (i)) == 0)
      break;
  unsigned int vl = i / elt_size;

  /* Check for the all-true case.  */
  if (i == nelts)
    return -1;

  /* If nelts_per_pattern is 1, then either VL is zero, or we have a
     repeating pattern of set bits followed by clear bits.  */
  if (builder.nelts_per_pattern () != 2)
    return 0;

  /* We have a "foreground" value and a duplicated "background" value.
     If the background might repeat and the last set bit belongs to it,
     we might have set bits followed by clear bits followed by set bits.  */
  if (i > builder.npatterns () && maybe_ne (nelts, builder.full_nelts ()))
    return 0;

  /* Make sure that the rest are all clear.  */
  for (; i < nelts; i += elt_size)
    if (INTVAL (builder.elt (i)) != 0)
      return 0;

  return vl;
}

/* See if there is an svpattern that encodes an SVE predicate of mode
   PRED_MODE in which the first VL bits are set and the rest are clear.
   Return the pattern if so, otherwise return AARCH64_NUM_SVPATTERNS.
   A VL of -1 indicates an all-true vector.  */

aarch64_svpattern
aarch64_svpattern_for_vl (machine_mode pred_mode, int vl)
{
  if (vl < 0)
    return AARCH64_SV_ALL;

  if (maybe_gt (vl, GET_MODE_NUNITS (pred_mode)))
    return AARCH64_NUM_SVPATTERNS;

  if (vl >= 1 && vl <= 8)
    return aarch64_svpattern (AARCH64_SV_VL1 + (vl - 1));

  if (vl >= 16 && vl <= 256 && pow2p_hwi (vl))
    return aarch64_svpattern (AARCH64_SV_VL16 + (exact_log2 (vl) - 4));

  int max_vl;
  if (GET_MODE_NUNITS (pred_mode).is_constant (&max_vl))
    {
      if (vl == (max_vl / 3) * 3)
	return AARCH64_SV_MUL3;
      /* These would only trigger for non-power-of-2 lengths.  */
      if (vl == (max_vl & -4))
	return AARCH64_SV_MUL4;
      if (vl == (1 << floor_log2 (max_vl)))
	return AARCH64_SV_POW2;
      if (vl == max_vl)
	return AARCH64_SV_ALL;
    }
  return AARCH64_NUM_SVPATTERNS;
}

/* Return a VNx16BImode constant in which every sequence of ELT_SIZE
   bits has the lowest bit set and the upper bits clear.  This is the
   VNx16BImode equivalent of a PTRUE for controlling elements of
   ELT_SIZE bytes.  However, because the constant is VNx16BImode,
   all bits are significant, even the upper zeros.  */

rtx
aarch64_ptrue_all (unsigned int elt_size)
{
  rtx_vector_builder builder (VNx16BImode, elt_size, 1);
  builder.quick_push (const1_rtx);
  for (unsigned int i = 1; i < elt_size; ++i)
    builder.quick_push (const0_rtx);
  return builder.build ();
}

/* Return an all-true predicate register of mode MODE.  */

rtx
aarch64_ptrue_reg (machine_mode mode)
{
  gcc_assert (aarch64_sve_pred_mode_p (mode));
  rtx reg = force_reg (VNx16BImode, CONSTM1_RTX (VNx16BImode));
  return gen_lowpart (mode, reg);
}

/* Return an all-true (restricted to the leading VL bits) predicate register of
   mode MODE.  */

rtx
aarch64_ptrue_reg (machine_mode mode, unsigned int vl)
{
  gcc_assert (aarch64_sve_pred_mode_p (mode));

  rtx_vector_builder builder (VNx16BImode, vl, 2);

  for (unsigned i = 0; i < vl; i++)
    builder.quick_push (CONST1_RTX (BImode));

  for (unsigned i = 0; i < vl; i++)
    builder.quick_push (CONST0_RTX (BImode));

  rtx const_vec = builder.build ();
  rtx reg = force_reg (VNx16BImode, const_vec);
  return gen_lowpart (mode, reg);
}

/* Return an all-false predicate register of mode MODE.  */

rtx
aarch64_pfalse_reg (machine_mode mode)
{
  gcc_assert (aarch64_sve_pred_mode_p (mode));
  rtx reg = force_reg (VNx16BImode, CONST0_RTX (VNx16BImode));
  return gen_lowpart (mode, reg);
}

/* PRED1[0] is a PTEST predicate and PRED1[1] is an aarch64_sve_ptrue_flag
   for it.  PRED2[0] is the predicate for the instruction whose result
   is tested by the PTEST and PRED2[1] is again an aarch64_sve_ptrue_flag
   for it.  Return true if we can prove that the two predicates are
   equivalent for PTEST purposes; that is, if we can replace PRED2[0]
   with PRED1[0] without changing behavior.  */

bool
aarch64_sve_same_pred_for_ptest_p (rtx *pred1, rtx *pred2)
{
  machine_mode mode = GET_MODE (pred1[0]);
  gcc_assert (aarch64_sve_pred_mode_p (mode)
	      && mode == GET_MODE (pred2[0])
	      && aarch64_sve_ptrue_flag (pred1[1], SImode)
	      && aarch64_sve_ptrue_flag (pred2[1], SImode));

  bool ptrue1_p = (pred1[0] == CONSTM1_RTX (mode)
		   || INTVAL (pred1[1]) == SVE_KNOWN_PTRUE);
  bool ptrue2_p = (pred2[0] == CONSTM1_RTX (mode)
		   || INTVAL (pred2[1]) == SVE_KNOWN_PTRUE);
  return (ptrue1_p && ptrue2_p) || rtx_equal_p (pred1[0], pred2[0]);
}

/* Emit a comparison CMP between OP0 and OP1, both of which have mode
   DATA_MODE, and return the result in a predicate of mode PRED_MODE.
   Use TARGET as the target register if nonnull and convenient.  */

static rtx
aarch64_sve_emit_int_cmp (rtx target, machine_mode pred_mode, rtx_code cmp,
			  machine_mode data_mode, rtx op1, rtx op2)
{
  insn_code icode = code_for_aarch64_pred_cmp (cmp, data_mode);
  expand_operand ops[5];
  create_output_operand (&ops[0], target, pred_mode);
  create_input_operand (&ops[1], CONSTM1_RTX (pred_mode), pred_mode);
  create_integer_operand (&ops[2], SVE_KNOWN_PTRUE);
  create_input_operand (&ops[3], op1, data_mode);
  create_input_operand (&ops[4], op2, data_mode);
  expand_insn (icode, 5, ops);
  return ops[0].value;
}

/* Use a comparison to convert integer vector SRC into MODE, which is
   the corresponding SVE predicate mode.  Use TARGET for the result
   if it's nonnull and convenient.  */

rtx
aarch64_convert_sve_data_to_pred (rtx target, machine_mode mode, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  return aarch64_sve_emit_int_cmp (target, mode, NE, src_mode,
				   src, CONST0_RTX (src_mode));
}

/* Return the assembly token for svprfop value PRFOP.  */

static const char *
svprfop_token (enum aarch64_svprfop prfop)
{
  switch (prfop)
    {
#define CASE(UPPER, LOWER, VALUE) case AARCH64_SV_##UPPER: return #LOWER;
    AARCH64_FOR_SVPRFOP (CASE)
#undef CASE
    case AARCH64_NUM_SVPRFOPS:
      break;
    }
  gcc_unreachable ();
}

/* Return the assembly string for an SVE prefetch operation with
   mnemonic MNEMONIC, given that PRFOP_RTX is the prefetch operation
   and that SUFFIX is the format for the remaining operands.  */

char *
aarch64_output_sve_prefetch (const char *mnemonic, rtx prfop_rtx,
			     const char *suffix)
{
  static char buffer[128];
  aarch64_svprfop prfop = (aarch64_svprfop) INTVAL (prfop_rtx);
  unsigned int written = snprintf (buffer, sizeof (buffer), "%s\t%s, %s",
				   mnemonic, svprfop_token (prfop), suffix);
  gcc_assert (written < sizeof (buffer));
  return buffer;
}

/* Check whether we can calculate the number of elements in PATTERN
   at compile time, given that there are NELTS_PER_VQ elements per
   128-bit block.  Return the value if so, otherwise return -1.  */

HOST_WIDE_INT
aarch64_fold_sve_cnt_pat (aarch64_svpattern pattern, unsigned int nelts_per_vq)
{
  unsigned int vl, const_vg;
  if (pattern >= AARCH64_SV_VL1 && pattern <= AARCH64_SV_VL8)
    vl = 1 + (pattern - AARCH64_SV_VL1);
  else if (pattern >= AARCH64_SV_VL16 && pattern <= AARCH64_SV_VL256)
    vl = 16 << (pattern - AARCH64_SV_VL16);
  else if (aarch64_sve_vg.is_constant (&const_vg))
    {
      /* There are two vector granules per quadword.  */
      unsigned int nelts = (const_vg / 2) * nelts_per_vq;
      switch (pattern)
	{
	case AARCH64_SV_POW2: return 1 << floor_log2 (nelts);
	case AARCH64_SV_MUL4: return nelts & -4;
	case AARCH64_SV_MUL3: return (nelts / 3) * 3;
	case AARCH64_SV_ALL: return nelts;
	default: gcc_unreachable ();
	}
    }
  else
    return -1;

  /* There are two vector granules per quadword.  */
  poly_uint64 nelts_all = exact_div (aarch64_sve_vg, 2) * nelts_per_vq;
  if (known_le (vl, nelts_all))
    return vl;

  /* Requesting more elements than are available results in a PFALSE.  */
  if (known_gt (vl, nelts_all))
    return 0;

  return -1;
}

/* Return true if a single CNT[BHWD] instruction can multiply FACTOR
   by the number of 128-bit quadwords in an SVE vector.  */

static bool
aarch64_sve_cnt_factor_p (HOST_WIDE_INT factor)
{
  /* The coefficient must be [1, 16] * {2, 4, 8, 16}.  */
  return (IN_RANGE (factor, 2, 16 * 16)
	  && (factor & 1) == 0
	  && factor <= 16 * (factor & -factor));
}

/* Return true if we can move VALUE into a register using a single
   CNT[BHWD] instruction.  */

static bool
aarch64_sve_cnt_immediate_p (poly_int64 value)
{
  HOST_WIDE_INT factor = value.coeffs[0];
  return value.coeffs[1] == factor && aarch64_sve_cnt_factor_p (factor);
}

/* Likewise for rtx X.  */

bool
aarch64_sve_cnt_immediate_p (rtx x)
{
  poly_int64 value;
  return poly_int_rtx_p (x, &value) && aarch64_sve_cnt_immediate_p (value);
}

/* Return the asm string for an instruction with a CNT-like vector size
   operand (a vector pattern followed by a multiplier in the range [1, 16]).
   PREFIX is the mnemonic without the size suffix and OPERANDS is the
   first part of the operands template (the part that comes before the
   vector size itself).  PATTERN is the pattern to use.  FACTOR is the
   number of quadwords.  NELTS_PER_VQ, if nonzero, is the number of elements
   in each quadword.  If it is zero, we can use any element size.  */

static char *
aarch64_output_sve_cnt_immediate (const char *prefix, const char *operands,
				  aarch64_svpattern pattern,
				  unsigned int factor,
				  unsigned int nelts_per_vq)
{
  static char buffer[sizeof ("sqincd\t%x0, %w0, vl256, mul #16")];

  if (nelts_per_vq == 0)
    /* There is some overlap in the ranges of the four CNT instructions.
       Here we always use the smallest possible element size, so that the
       multiplier is 1 whereever possible.  */
    nelts_per_vq = factor & -factor;
  int shift = std::min (exact_log2 (nelts_per_vq), 4);
  gcc_assert (IN_RANGE (shift, 1, 4));
  char suffix = "dwhb"[shift - 1];

  factor >>= shift;
  unsigned int written;
  if (pattern == AARCH64_SV_ALL && factor == 1)
    written = snprintf (buffer, sizeof (buffer), "%s%c\t%s",
			prefix, suffix, operands);
  else if (factor == 1)
    written = snprintf (buffer, sizeof (buffer), "%s%c\t%s, %s",
			prefix, suffix, operands, svpattern_token (pattern));
  else
    written = snprintf (buffer, sizeof (buffer), "%s%c\t%s, %s, mul #%d",
			prefix, suffix, operands, svpattern_token (pattern),
			factor);
  gcc_assert (written < sizeof (buffer));
  return buffer;
}

/* Return the asm string for an instruction with a CNT-like vector size
   operand (a vector pattern followed by a multiplier in the range [1, 16]).
   PREFIX is the mnemonic without the size suffix and OPERANDS is the
   first part of the operands template (the part that comes before the
   vector size itself).  X is the value of the vector size operand,
   as a polynomial integer rtx; we need to convert this into an "all"
   pattern with a multiplier.  */

char *
aarch64_output_sve_cnt_immediate (const char *prefix, const char *operands,
				  rtx x)
{
  poly_int64 value = rtx_to_poly_int64 (x);
  gcc_assert (aarch64_sve_cnt_immediate_p (value));
  return aarch64_output_sve_cnt_immediate (prefix, operands, AARCH64_SV_ALL,
					   value.coeffs[1], 0);
}

/* Return the asm string for an instruction with a CNT-like vector size
   operand (a vector pattern followed by a multiplier in the range [1, 16]).
   PREFIX is the mnemonic without the size suffix and OPERANDS is the
   first part of the operands template (the part that comes before the
   vector size itself).  CNT_PAT[0..2] are the operands of the
   UNSPEC_SVE_CNT_PAT; see aarch64_sve_cnt_pat for details.  */

char *
aarch64_output_sve_cnt_pat_immediate (const char *prefix,
				      const char *operands, rtx *cnt_pat)
{
  aarch64_svpattern pattern = (aarch64_svpattern) INTVAL (cnt_pat[0]);
  unsigned int nelts_per_vq = INTVAL (cnt_pat[1]);
  unsigned int factor = INTVAL (cnt_pat[2]) * nelts_per_vq;
  return aarch64_output_sve_cnt_immediate (prefix, operands, pattern,
					   factor, nelts_per_vq);
}

/* Return true if we can add X using a single SVE INC or DEC instruction.  */

bool
aarch64_sve_scalar_inc_dec_immediate_p (rtx x)
{
  poly_int64 value;
  return (poly_int_rtx_p (x, &value)
	  && (aarch64_sve_cnt_immediate_p (value)
	      || aarch64_sve_cnt_immediate_p (-value)));
}

/* Return the asm string for adding SVE INC/DEC immediate OFFSET to
   operand 0.  */

char *
aarch64_output_sve_scalar_inc_dec (rtx offset)
{
  poly_int64 offset_value = rtx_to_poly_int64 (offset);
  gcc_assert (offset_value.coeffs[0] == offset_value.coeffs[1]);
  if (offset_value.coeffs[1] > 0)
    return aarch64_output_sve_cnt_immediate ("inc", "%x0", AARCH64_SV_ALL,
					     offset_value.coeffs[1], 0);
  else
    return aarch64_output_sve_cnt_immediate ("dec", "%x0", AARCH64_SV_ALL,
					     -offset_value.coeffs[1], 0);
}

/* Return true if a single RDVL instruction can multiply FACTOR by the
   number of 128-bit quadwords in an SVE vector.  This is also the
   range of ADDVL.  */

static bool
aarch64_sve_rdvl_addvl_factor_p (HOST_WIDE_INT factor)
{
  return (multiple_p (factor, 16)
	  && IN_RANGE (factor, -32 * 16, 31 * 16));
}

/* Return true if ADDPL can be used to add FACTOR multiplied by the number
   of quadwords in an SVE vector.  */

static bool
aarch64_sve_addpl_factor_p (HOST_WIDE_INT factor)
{
  return (multiple_p (factor, 2)
	  && IN_RANGE (factor, -32 * 2, 31 * 2));
}

/* Return true if we can move VALUE into a register using a single
   RDVL instruction.  */

static bool
aarch64_sve_rdvl_immediate_p (poly_int64 value)
{
  HOST_WIDE_INT factor = value.coeffs[0];
  return value.coeffs[1] == factor && aarch64_sve_rdvl_addvl_factor_p (factor);
}

/* Likewise for rtx X.  */

bool
aarch64_sve_rdvl_immediate_p (rtx x)
{
  poly_int64 value;
  return poly_int_rtx_p (x, &value) && aarch64_sve_rdvl_immediate_p (value);
}

/* Return the asm string for moving RDVL immediate OFFSET into register
   operand 0.  */

char *
aarch64_output_sve_rdvl (rtx offset)
{
  static char buffer[sizeof ("rdvl\t%x0, #-") + 3 * sizeof (int)];
  poly_int64 offset_value = rtx_to_poly_int64 (offset);
  gcc_assert (aarch64_sve_rdvl_immediate_p (offset_value));

  int factor = offset_value.coeffs[1];
  snprintf (buffer, sizeof (buffer), "rdvl\t%%x0, #%d", factor / 16);
  return buffer;
}

/* Return true if we can add VALUE to a register using a single ADDVL
   or ADDPL instruction.  */

static bool
aarch64_sve_addvl_addpl_immediate_p (poly_int64 value)
{
  HOST_WIDE_INT factor = value.coeffs[0];
  if (factor == 0 || value.coeffs[1] != factor)
    return false;
  return (aarch64_sve_rdvl_addvl_factor_p (factor)
	  || aarch64_sve_addpl_factor_p (factor));
}

/* Likewise for rtx X.  */

bool
aarch64_sve_addvl_addpl_immediate_p (rtx x)
{
  poly_int64 value;
  return (poly_int_rtx_p (x, &value)
	  && aarch64_sve_addvl_addpl_immediate_p (value));
}

/* Return the asm string for adding ADDVL or ADDPL immediate OFFSET
   to operand 1 and storing the result in operand 0.  */

char *
aarch64_output_sve_addvl_addpl (rtx offset)
{
  static char buffer[sizeof ("addpl\t%x0, %x1, #-") + 3 * sizeof (int)];
  poly_int64 offset_value = rtx_to_poly_int64 (offset);
  gcc_assert (aarch64_sve_addvl_addpl_immediate_p (offset_value));

  int factor = offset_value.coeffs[1];
  if ((factor & 15) == 0)
    snprintf (buffer, sizeof (buffer), "addvl\t%%x0, %%x1, #%d", factor / 16);
  else
    snprintf (buffer, sizeof (buffer), "addpl\t%%x0, %%x1, #%d", factor / 2);
  return buffer;
}

/* Return true if X is a valid immediate for an SVE vector INC or DEC
   instruction.  If it is, store the number of elements in each vector
   quadword in *NELTS_PER_VQ_OUT (if nonnull) and store the multiplication
   factor in *FACTOR_OUT (if nonnull).  */

bool
aarch64_sve_vector_inc_dec_immediate_p (rtx x, int *factor_out,
					unsigned int *nelts_per_vq_out)
{
  rtx elt;
  poly_int64 value;

  if (!const_vec_duplicate_p (x, &elt)
      || !poly_int_rtx_p (elt, &value))
    return false;

  unsigned int nelts_per_vq = 128 / GET_MODE_UNIT_BITSIZE (GET_MODE (x));
  if (nelts_per_vq != 8 && nelts_per_vq != 4 && nelts_per_vq != 2)
    /* There's no vector INCB.  */
    return false;

  HOST_WIDE_INT factor = value.coeffs[0];
  if (value.coeffs[1] != factor)
    return false;

  /* The coefficient must be [1, 16] * NELTS_PER_VQ.  */
  if ((factor % nelts_per_vq) != 0
      || !IN_RANGE (abs (factor), nelts_per_vq, 16 * nelts_per_vq))
    return false;

  if (factor_out)
    *factor_out = factor;
  if (nelts_per_vq_out)
    *nelts_per_vq_out = nelts_per_vq;
  return true;
}

/* Return true if X is a valid immediate for an SVE vector INC or DEC
   instruction.  */

bool
aarch64_sve_vector_inc_dec_immediate_p (rtx x)
{
  return aarch64_sve_vector_inc_dec_immediate_p (x, NULL, NULL);
}

/* Return the asm template for an SVE vector INC or DEC instruction.
   OPERANDS gives the operands before the vector count and X is the
   value of the vector count operand itself.  */

char *
aarch64_output_sve_vector_inc_dec (const char *operands, rtx x)
{
  int factor;
  unsigned int nelts_per_vq;
  if (!aarch64_sve_vector_inc_dec_immediate_p (x, &factor, &nelts_per_vq))
    gcc_unreachable ();
  if (factor < 0)
    return aarch64_output_sve_cnt_immediate ("dec", operands, AARCH64_SV_ALL,
					     -factor, nelts_per_vq);
  else
    return aarch64_output_sve_cnt_immediate ("inc", operands, AARCH64_SV_ALL,
					     factor, nelts_per_vq);
}

/* Return a constant that represents FACTOR multiplied by the
   number of 128-bit quadwords in an SME vector.  ISA_MODE is the
   ISA mode in which the calculation is being performed.  */

rtx
aarch64_sme_vq_immediate (machine_mode mode, HOST_WIDE_INT factor,
			  aarch64_isa_mode isa_mode)
{
  gcc_assert (aarch64_sve_rdvl_addvl_factor_p (factor));
  if (isa_mode & AARCH64_ISA_MODE_SM_ON)
    /* We're in streaming mode, so we can use normal poly-int values.  */
    return gen_int_mode ({ factor, factor }, mode);

  rtvec vec = gen_rtvec (1, gen_int_mode (factor, SImode));
  rtx unspec = gen_rtx_UNSPEC (mode, vec, UNSPEC_SME_VQ);
  return gen_rtx_CONST (mode, unspec);
}

/* Return true if X is a constant that represents some number X
   multiplied by the number of quadwords in an SME vector.  Store this X
   in *FACTOR if so.  */

static bool
aarch64_sme_vq_unspec_p (const_rtx x, HOST_WIDE_INT *factor)
{
  if (!TARGET_SME || GET_CODE (x) != CONST)
    return false;

  x = XEXP (x, 0);
  if (GET_CODE (x) != UNSPEC
      || XINT (x, 1) != UNSPEC_SME_VQ
      || XVECLEN (x, 0) != 1)
    return false;

  x = XVECEXP (x, 0, 0);
  if (!CONST_INT_P (x))
    return false;

  *factor = INTVAL (x);
  return true;
}

/* Return true if X is a constant that represents some number Y
   multiplied by the number of quadwords in an SME vector, and if
   that Y is in the range of RDSVL.  */

bool
aarch64_rdsvl_immediate_p (const_rtx x)
{
  HOST_WIDE_INT factor;
  return (aarch64_sme_vq_unspec_p (x, &factor)
	  && aarch64_sve_rdvl_addvl_factor_p (factor));
}

/* Return the asm string for an RDSVL instruction that calculates X,
   which is a constant that satisfies aarch64_rdsvl_immediate_p.  */

char *
aarch64_output_rdsvl (const_rtx x)
{
  gcc_assert (aarch64_rdsvl_immediate_p (x));
  static char buffer[sizeof ("rdsvl\t%x0, #-") + 3 * sizeof (int)];
  x = XVECEXP (XEXP (x, 0), 0, 0);
  snprintf (buffer, sizeof (buffer), "rdsvl\t%%x0, #%d",
	    (int) INTVAL (x) / 16);
  return buffer;
}

/* Return true if X is a constant that can be added using ADDSVL or ADDSPL.  */

bool
aarch64_addsvl_addspl_immediate_p (const_rtx x)
{
  HOST_WIDE_INT factor;
  return (aarch64_sme_vq_unspec_p (x, &factor)
	  && (aarch64_sve_rdvl_addvl_factor_p (factor)
	      || aarch64_sve_addpl_factor_p (factor)));
}

/* X is a constant that satisfies aarch64_addsvl_addspl_immediate_p.
   Return the asm string for the associated instruction.  */

char *
aarch64_output_addsvl_addspl (rtx x)
{
  static char buffer[sizeof ("addspl\t%x0, %x1, #-") + 3 * sizeof (int)];
  HOST_WIDE_INT factor;
  if (!aarch64_sme_vq_unspec_p (x, &factor))
    gcc_unreachable ();
  if (aarch64_sve_rdvl_addvl_factor_p (factor))
    snprintf (buffer, sizeof (buffer), "addsvl\t%%x0, %%x1, #%d",
	      (int) factor / 16);
  else if (aarch64_sve_addpl_factor_p (factor))
    snprintf (buffer, sizeof (buffer), "addspl\t%%x0, %%x1, #%d",
	      (int) factor / 2);
  else
    gcc_unreachable ();
  return buffer;
}

/* Multipliers for repeating bitmasks of width 32, 16, 8, 4, and 2.  */

static const unsigned HOST_WIDE_INT bitmask_imm_mul[] =
  {
    0x0000000100000001ull,
    0x0001000100010001ull,
    0x0101010101010101ull,
    0x1111111111111111ull,
    0x5555555555555555ull,
  };



/* Return true if 64-bit VAL is a valid bitmask immediate.  */
static bool
aarch64_bitmask_imm (unsigned HOST_WIDE_INT val)
{
  unsigned HOST_WIDE_INT tmp, mask, first_one, next_one;
  int bits;

  /* Check for a single sequence of one bits and return quickly if so.
     The special cases of all ones and all zeroes returns false.  */
  tmp = val + (val & -val);

  if (tmp == (tmp & -tmp))
    return (val + 1) > 1;

  /* Invert if the immediate doesn't start with a zero bit - this means we
     only need to search for sequences of one bits.  */
  if (val & 1)
    val = ~val;

  /* Find the first set bit and set tmp to val with the first sequence of one
     bits removed.  Return success if there is a single sequence of ones.  */
  first_one = val & -val;
  tmp = val & (val + first_one);

  if (tmp == 0)
    return true;

  /* Find the next set bit and compute the difference in bit position.  */
  next_one = tmp & -tmp;
  bits = clz_hwi (first_one) - clz_hwi (next_one);
  mask = val ^ tmp;

  /* Check the bit position difference is a power of 2, and that the first
     sequence of one bits fits within 'bits' bits.  */
  if ((mask >> bits) != 0 || bits != (bits & -bits))
    return false;

  /* Check the sequence of one bits is repeated 64/bits times.  */
  return val == mask * bitmask_imm_mul[__builtin_clz (bits) - 26];
}


/* Return true if VAL is a valid bitmask immediate for MODE.  */
bool
aarch64_bitmask_imm (unsigned HOST_WIDE_INT val, machine_mode mode)
{
  if (mode == DImode)
    return aarch64_bitmask_imm (val);

  if (mode == SImode)
    return aarch64_bitmask_imm ((val & 0xffffffff) | (val << 32));

  /* Replicate small immediates to fit 64 bits.  */
  int size = GET_MODE_UNIT_PRECISION (mode);
  val &= (HOST_WIDE_INT_1U << size) - 1;
  val *= bitmask_imm_mul[__builtin_clz (size) - 26];

  return aarch64_bitmask_imm (val);
}


/* Return true if the immediate VAL can be a bitfield immediate
   by changing the given MASK bits in VAL to zeroes, ones or bits
   from the other half of VAL.  Return the new immediate in VAL2.  */
static inline bool
aarch64_check_bitmask (unsigned HOST_WIDE_INT val,
		       unsigned HOST_WIDE_INT &val2,
		       unsigned HOST_WIDE_INT mask)
{
  val2 = val & ~mask;
  if (val2 != val && aarch64_bitmask_imm (val2))
    return true;
  val2 = val | mask;
  if (val2 != val && aarch64_bitmask_imm (val2))
    return true;
  val = val & ~mask;
  val2 = val | (((val >> 32) | (val << 32)) & mask);
  if (val2 != val && aarch64_bitmask_imm (val2))
    return true;
  val2 = val | (((val >> 16) | (val << 48)) & mask);
  if (val2 != val && aarch64_bitmask_imm (val2))
    return true;
  return false;
}


/* Return true if VAL is a valid MOVZ immediate.  */
static inline bool
aarch64_is_movz (unsigned HOST_WIDE_INT val)
{
  return (val >> (ctz_hwi (val) & 48)) < 65536;
}


/* Return true if immediate VAL can be created by a 64-bit MOVI/MOVN/MOVZ.  */
bool
aarch64_is_mov_xn_imm (unsigned HOST_WIDE_INT val)
{
  return aarch64_is_movz (val) || aarch64_is_movz (~val)
    || aarch64_bitmask_imm (val);
}


/* Return true if VAL is an immediate that can be created by a single
   MOV instruction.  */
bool
aarch64_move_imm (unsigned HOST_WIDE_INT val, machine_mode mode)
{
  gcc_assert (mode == SImode || mode == DImode);

  if (val < 65536)
    return true;

  unsigned HOST_WIDE_INT mask =
    (val >> 32) == 0 || mode == SImode ? 0xffffffff : HOST_WIDE_INT_M1U;

  if (aarch64_is_movz (val & mask) || aarch64_is_movz (~val & mask))
    return true;

  val = (val & mask) | ((val << 32) & ~mask);
  return aarch64_bitmask_imm (val);
}


static int
aarch64_internal_mov_immediate (rtx dest, rtx imm, bool generate,
				machine_mode mode)
{
  int i;
  unsigned HOST_WIDE_INT val, val2, val3, mask;
  int one_match, zero_match;
  int num_insns;

  gcc_assert (mode == SImode || mode == DImode);

  val = INTVAL (imm);

  if (aarch64_move_imm (val, mode))
    {
      if (generate)
	emit_insn (gen_rtx_SET (dest, imm));
      return 1;
    }

  if ((val >> 32) == 0 || mode == SImode)
    {
      if (generate)
	{
	  emit_insn (gen_rtx_SET (dest, GEN_INT (val & 0xffff)));
	  if (mode == SImode)
	    emit_insn (gen_insv_immsi (dest, GEN_INT (16),
				       GEN_INT ((val >> 16) & 0xffff)));
	  else
	    emit_insn (gen_insv_immdi (dest, GEN_INT (16),
				       GEN_INT ((val >> 16) & 0xffff)));
	}
      return 2;
    }

  /* Remaining cases are all for DImode.  */

  mask = 0xffff;
  zero_match = ((val & mask) == 0) + ((val & (mask << 16)) == 0) +
    ((val & (mask << 32)) == 0) + ((val & (mask << 48)) == 0);
  one_match = ((~val & mask) == 0) + ((~val & (mask << 16)) == 0) +
    ((~val & (mask << 32)) == 0) + ((~val & (mask << 48)) == 0);

  /* Try a bitmask immediate and a movk to generate the immediate
     in 2 instructions.  */

  if (zero_match < 2 && one_match < 2)
    {
      for (i = 0; i < 64; i += 16)
	{
	  if (aarch64_check_bitmask (val, val2, mask << i))
	    break;

	  val2 = val & ~(mask << i);
	  if ((val2 >> 32) == 0 && aarch64_move_imm (val2, DImode))
	    break;
	}

      if (i != 64)
	{
	  if (generate)
	    {
	      emit_insn (gen_rtx_SET (dest, GEN_INT (val2)));
	      emit_insn (gen_insv_immdi (dest, GEN_INT (i),
					 GEN_INT ((val >> i) & 0xffff)));
	    }
	  return 2;
	}

      /* Try 2 bitmask immediates which are xor'd together. */
      for (i = 0; i < 64; i += 16)
	{
	  val2 = (val >> i) & mask;
	  val2 |= val2 << 16;
	  val2 |= val2 << 32;
	  if (aarch64_bitmask_imm (val2) && aarch64_bitmask_imm (val ^ val2))
	    break;
	}

      if (i != 64)
	{
	  if (generate)
	    {
	      emit_insn (gen_rtx_SET (dest, GEN_INT (val2)));
	      emit_insn (gen_xordi3 (dest, dest, GEN_INT (val ^ val2)));
	    }
	  return 2;
	}
    }

  /* Try a bitmask plus 2 movk to generate the immediate in 3 instructions.  */
  if (zero_match + one_match == 0)
    {
      for (i = 0; i < 48; i += 16)
	for (int j = i + 16; j < 64; j += 16)
	  if (aarch64_check_bitmask (val, val2, (mask << i) | (mask << j)))
	    {
	      if (generate)
		{
		  emit_insn (gen_rtx_SET (dest, GEN_INT (val2)));
		  emit_insn (gen_insv_immdi (dest, GEN_INT (i),
					     GEN_INT ((val >> i) & 0xffff)));
		  emit_insn (gen_insv_immdi (dest, GEN_INT (j),
					       GEN_INT ((val >> j) & 0xffff)));
		}
	      return 3;
	    }

      /* Try shifting and inserting the bottom 32-bits into the top bits.  */
      val2 = val & 0xffffffff;
      val3 = 0xffffffff;
      val3 = val2 | (val3 << 32);
      for (i = 17; i < 48; i++)
	if ((val2 | (val2 << i)) == val)
	  {
	    if (generate)
	      {
		emit_insn (gen_rtx_SET (dest, GEN_INT (val2 & 0xffff)));
		emit_insn (gen_insv_immdi (dest, GEN_INT (16),
					   GEN_INT (val2 >> 16)));
		emit_insn (gen_ior_ashldi3 (dest, dest, GEN_INT (i), dest));
	      }
	    return 3;
	  }
	else if ((val3 & ~(val3 << i)) == val)
	  {
	    if (generate)
	      {
		emit_insn (gen_rtx_SET (dest, GEN_INT (val3 | 0xffff0000)));
		emit_insn (gen_insv_immdi (dest, GEN_INT (16),
					   GEN_INT (val2 >> 16)));
		emit_insn (gen_and_one_cmpl_ashldi3 (dest, dest, GEN_INT (i),
						      dest));
	      }
	    return 3;
	  }
    }

  /* Generate 2-4 instructions, skipping 16 bits of all zeroes or ones which
     are emitted by the initial mov.  If one_match > zero_match, skip set bits,
     otherwise skip zero bits.  */

  num_insns = 1;
  mask = 0xffff;
  val2 = one_match > zero_match ? ~val : val;
  i = (val2 & mask) != 0 ? 0 : (val2 & (mask << 16)) != 0 ? 16 : 32;

  if (generate)
    emit_insn (gen_rtx_SET (dest, GEN_INT (one_match > zero_match
					   ? (val | ~(mask << i))
					   : (val & (mask << i)))));
  for (i += 16; i < 64; i += 16)
    {
      if ((val2 & (mask << i)) == 0)
	continue;
      if (generate)
	emit_insn (gen_insv_immdi (dest, GEN_INT (i),
				   GEN_INT ((val >> i) & 0xffff)));
      num_insns ++;
    }

  return num_insns;
}

/* Return whether imm is a 128-bit immediate which is simple enough to
   expand inline.  */
bool
aarch64_mov128_immediate (rtx imm)
{
  if (CONST_INT_P (imm))
    return true;

  gcc_assert (CONST_WIDE_INT_NUNITS (imm) == 2);

  rtx lo = GEN_INT (CONST_WIDE_INT_ELT (imm, 0));
  rtx hi = GEN_INT (CONST_WIDE_INT_ELT (imm, 1));

  return aarch64_internal_mov_immediate (NULL_RTX, lo, false, DImode)
	 + aarch64_internal_mov_immediate (NULL_RTX, hi, false, DImode) <= 4;
}


/* Return true if val can be encoded as a 12-bit unsigned immediate with
   a left shift of 0 or 12 bits.  */
bool
aarch64_uimm12_shift (unsigned HOST_WIDE_INT val)
{
  return val < 4096 || (val & 0xfff000) == val;
}

/* Returns the nearest value to VAL that will fit as a 12-bit unsigned immediate
   that can be created with a left shift of 0 or 12.  */
static HOST_WIDE_INT
aarch64_clamp_to_uimm12_shift (unsigned HOST_WIDE_INT val)
{
  /* Check to see if the value fits in 24 bits, as that is the maximum we can
     handle correctly.  */
  gcc_assert (val < 0x1000000);

  if (val < 4096)
    return val;

  return val & 0xfff000;
}


/* Test whether:

     X = (X & AND_VAL) | IOR_VAL;

   can be implemented using:

     MOVK X, #(IOR_VAL >> shift), LSL #shift

   Return the shift if so, otherwise return -1.  */
int
aarch64_movk_shift (const wide_int_ref &and_val,
		    const wide_int_ref &ior_val)
{
  unsigned int precision = and_val.get_precision ();
  unsigned HOST_WIDE_INT mask = 0xffff;
  for (unsigned int shift = 0; shift < precision; shift += 16)
    {
      if (and_val == ~mask && (ior_val & mask) == ior_val)
	return shift;
      mask <<= 16;
    }
  return -1;
}

/* Create mask of ones, covering the lowest to highest bits set in VAL_IN.
   Assumed precondition: VAL_IN Is not zero.  */

unsigned HOST_WIDE_INT
aarch64_and_split_imm1 (HOST_WIDE_INT val_in)
{
  int lowest_bit_set = ctz_hwi (val_in);
  int highest_bit_set = floor_log2 (val_in);
  gcc_assert (val_in != 0);

  return ((HOST_WIDE_INT_UC (2) << highest_bit_set) -
	  (HOST_WIDE_INT_1U << lowest_bit_set));
}

/* Create constant where bits outside of lowest bit set to highest bit set
   are set to 1.  */

unsigned HOST_WIDE_INT
aarch64_and_split_imm2 (HOST_WIDE_INT val_in)
{
  return val_in | ~aarch64_and_split_imm1 (val_in);
}

/* Return true if VAL_IN is a valid 'and' bitmask immediate.  */

bool
aarch64_and_bitmask_imm (unsigned HOST_WIDE_INT val_in, machine_mode mode)
{
  scalar_int_mode int_mode;
  if (!is_a <scalar_int_mode> (mode, &int_mode))
    return false;

  if (aarch64_bitmask_imm (val_in, int_mode))
    return false;

  if (aarch64_move_imm (val_in, int_mode))
    return false;

  unsigned HOST_WIDE_INT imm2 = aarch64_and_split_imm2 (val_in);

  return aarch64_bitmask_imm (imm2, int_mode);
}

/* Return the number of temporary registers that aarch64_add_offset_1
   would need to add OFFSET to a register.  */

static unsigned int
aarch64_add_offset_1_temporaries (HOST_WIDE_INT offset)
{
  return absu_hwi (offset) < 0x1000000 ? 0 : 1;
}

/* A subroutine of aarch64_add_offset.  Set DEST to SRC + OFFSET for
   a non-polynomial OFFSET.  MODE is the mode of the addition.
   FRAME_RELATED_P is true if the RTX_FRAME_RELATED flag should
   be set and CFA adjustments added to the generated instructions.

   TEMP1, if nonnull, is a register of mode MODE that can be used as a
   temporary if register allocation is already complete.  This temporary
   register may overlap DEST but must not overlap SRC.  If TEMP1 is known
   to hold abs (OFFSET), EMIT_MOVE_IMM can be set to false to avoid emitting
   the immediate again.

   Since this function may be used to adjust the stack pointer, we must
   ensure that it cannot cause transient stack deallocation (for example
   by first incrementing SP and then decrementing when adjusting by a
   large immediate).  */

static void
aarch64_add_offset_1 (scalar_int_mode mode, rtx dest,
		      rtx src, HOST_WIDE_INT offset, rtx temp1,
		      bool frame_related_p, bool emit_move_imm)
{
  gcc_assert (emit_move_imm || temp1 != NULL_RTX);
  gcc_assert (temp1 == NULL_RTX || !reg_overlap_mentioned_p (temp1, src));

  unsigned HOST_WIDE_INT moffset = absu_hwi (offset);
  rtx_insn *insn;

  if (!moffset)
    {
      if (!rtx_equal_p (dest, src))
	{
	  insn = emit_insn (gen_rtx_SET (dest, src));
	  RTX_FRAME_RELATED_P (insn) = frame_related_p;
	}
      return;
    }

  /* Single instruction adjustment.  */
  if (aarch64_uimm12_shift (moffset))
    {
      insn = emit_insn (gen_add3_insn (dest, src, GEN_INT (offset)));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      return;
    }

  /* Emit 2 additions/subtractions if the adjustment is less than 24 bits
     and either:

     a) the offset cannot be loaded by a 16-bit move or
     b) there is no spare register into which we can move it.  */
  if (moffset < 0x1000000
      && ((!temp1 && !can_create_pseudo_p ())
	  || !aarch64_move_imm (moffset, mode)))
    {
      HOST_WIDE_INT low_off = moffset & 0xfff;

      low_off = offset < 0 ? -low_off : low_off;
      insn = emit_insn (gen_add3_insn (dest, src, GEN_INT (low_off)));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      insn = emit_insn (gen_add2_insn (dest, GEN_INT (offset - low_off)));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      return;
    }

  /* Emit a move immediate if required and an addition/subtraction.  */
  if (emit_move_imm)
    {
      gcc_assert (temp1 != NULL_RTX || can_create_pseudo_p ());
      temp1 = aarch64_force_temporary (mode, temp1,
				       gen_int_mode (moffset, mode));
    }
  insn = emit_insn (offset < 0
		    ? gen_sub3_insn (dest, src, temp1)
		    : gen_add3_insn (dest, src, temp1));
  if (frame_related_p)
    {
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      rtx adj = plus_constant (mode, src, offset);
      add_reg_note (insn, REG_CFA_ADJUST_CFA, gen_rtx_SET (dest, adj));
    }
}

/* Return the number of temporary registers that aarch64_add_offset
   would need to move OFFSET into a register or add OFFSET to a register;
   ADD_P is true if we want the latter rather than the former.  */

static unsigned int
aarch64_offset_temporaries (bool add_p, poly_int64 offset)
{
  /* This follows the same structure as aarch64_add_offset.  */
  if (add_p && aarch64_sve_addvl_addpl_immediate_p (offset))
    return 0;

  unsigned int count = 0;
  HOST_WIDE_INT factor = offset.coeffs[1];
  HOST_WIDE_INT constant = offset.coeffs[0] - factor;
  poly_int64 poly_offset (factor, factor);
  if (add_p && aarch64_sve_addvl_addpl_immediate_p (poly_offset))
    /* Need one register for the ADDVL/ADDPL result.  */
    count += 1;
  else if (factor != 0)
    {
      factor /= (HOST_WIDE_INT) least_bit_hwi (factor);
      if (!IN_RANGE (factor, -32, 31))
	/* Need one register for the CNT or RDVL result and one for the
	   multiplication factor.  If necessary, the second temporary
	   can be reused for the constant part of the offset.  */
	return 2;
      /* Need one register for the CNT or RDVL result (which might then
	 be shifted).  */
      count += 1;
    }
  return count + aarch64_add_offset_1_temporaries (constant);
}

/* If X can be represented as a poly_int64, return the number
   of temporaries that are required to add it to a register.
   Return -1 otherwise.  */

int
aarch64_add_offset_temporaries (rtx x)
{
  poly_int64 offset;
  if (!poly_int_rtx_p (x, &offset))
    return -1;
  return aarch64_offset_temporaries (true, offset);
}

/* Set DEST to SRC + OFFSET.  MODE is the mode of the addition.
   FRAME_RELATED_P is true if the RTX_FRAME_RELATED flag should
   be set and CFA adjustments added to the generated instructions.

   TEMP1, if nonnull, is a register of mode MODE that can be used as a
   temporary if register allocation is already complete.  This temporary
   register may overlap DEST if !FRAME_RELATED_P but must not overlap SRC.
   If TEMP1 is known to hold abs (OFFSET), EMIT_MOVE_IMM can be set to
   false to avoid emitting the immediate again.

   TEMP2, if nonnull, is a second temporary register that doesn't
   overlap either DEST or REG.

   FORCE_ISA_MODE is AARCH64_ISA_MODE_SM_ON if any variable component of OFFSET
   is measured relative to the SME vector length instead of the current
   prevailing vector length.  It is 0 otherwise.

   Since this function may be used to adjust the stack pointer, we must
   ensure that it cannot cause transient stack deallocation (for example
   by first incrementing SP and then decrementing when adjusting by a
   large immediate).  */

static void
aarch64_add_offset (scalar_int_mode mode, rtx dest, rtx src,
		    poly_int64 offset, rtx temp1, rtx temp2,
		    aarch64_isa_mode force_isa_mode,
		    bool frame_related_p, bool emit_move_imm = true)
{
  gcc_assert (emit_move_imm || temp1 != NULL_RTX);
  gcc_assert (temp1 == NULL_RTX || !reg_overlap_mentioned_p (temp1, src));
  gcc_assert (temp1 == NULL_RTX
	      || !frame_related_p
	      || !reg_overlap_mentioned_p (temp1, dest));
  gcc_assert (temp2 == NULL_RTX || !reg_overlap_mentioned_p (dest, temp2));

  /* Try using ADDVL or ADDPL to add the whole value.  */
  if (src != const0_rtx && aarch64_sve_addvl_addpl_immediate_p (offset))
    {
      gcc_assert (offset.coeffs[0] == offset.coeffs[1]);
      rtx offset_rtx;
      if (force_isa_mode == 0)
	offset_rtx = gen_int_mode (offset, mode);
      else
	offset_rtx = aarch64_sme_vq_immediate (mode, offset.coeffs[0], 0);
      rtx_insn *insn = emit_insn (gen_add3_insn (dest, src, offset_rtx));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      if (frame_related_p && (force_isa_mode & AARCH64_ISA_MODE_SM_ON))
	add_reg_note (insn, REG_CFA_ADJUST_CFA,
		      gen_rtx_SET (dest, plus_constant (Pmode, src,
							offset)));
      return;
    }

  /* Coefficient 1 is multiplied by the number of 128-bit blocks in an
     SVE vector register, over and above the minimum size of 128 bits.
     This is equivalent to half the value returned by CNTD with a
     vector shape of ALL.  */
  HOST_WIDE_INT factor = offset.coeffs[1];
  HOST_WIDE_INT constant = offset.coeffs[0] - factor;

  /* Try using ADDVL or ADDPL to add the VG-based part.  */
  poly_int64 poly_offset (factor, factor);
  if (src != const0_rtx
      && aarch64_sve_addvl_addpl_immediate_p (poly_offset))
    {
      rtx offset_rtx;
      if (force_isa_mode == 0)
	offset_rtx = gen_int_mode (poly_offset, mode);
      else
	offset_rtx = aarch64_sme_vq_immediate (mode, factor, 0);
      if (frame_related_p)
	{
	  rtx_insn *insn = emit_insn (gen_add3_insn (dest, src, offset_rtx));
	  RTX_FRAME_RELATED_P (insn) = true;
	  if (force_isa_mode & AARCH64_ISA_MODE_SM_ON)
	    add_reg_note (insn, REG_CFA_ADJUST_CFA,
			  gen_rtx_SET (dest, plus_constant (Pmode, src,
							    poly_offset)));
	  src = dest;
	}
      else
	{
	  rtx addr = gen_rtx_PLUS (mode, src, offset_rtx);
	  src = aarch64_force_temporary (mode, temp1, addr);
	  temp1 = temp2;
	  temp2 = NULL_RTX;
	}
    }
  /* Otherwise use a CNT-based sequence.  */
  else if (factor != 0)
    {
      /* Calculate CNTB * FACTOR / 16 as CNTB * REL_FACTOR * 2**SHIFT,
	 with negative shifts indicating a shift right.  */
      HOST_WIDE_INT low_bit = least_bit_hwi (factor);
      HOST_WIDE_INT rel_factor = factor / low_bit;
      int shift = exact_log2 (low_bit) - 4;
      gcc_assert (shift >= -4 && (rel_factor & 1) != 0);

      /* Set CODE, VAL and SHIFT so that [+-] VAL * 2**SHIFT is
	 equal to CNTB * FACTOR / 16, with CODE being the [+-].

	 We can avoid a multiplication if REL_FACTOR is in the range
	 of RDVL, although there are then various optimizations that
	 we can try on top.  */
      rtx_code code = PLUS;
      rtx val;
      if (IN_RANGE (rel_factor, -32, 31))
	{
	  if (force_isa_mode & AARCH64_ISA_MODE_SM_ON)
	    {
	      /* Try to use an unshifted RDSVL, otherwise fall back on
		 a shifted RDSVL #1.  */
	      if (aarch64_sve_rdvl_addvl_factor_p (factor))
		shift = 0;
	      else
		factor = rel_factor * 16;
	      val = aarch64_sme_vq_immediate (mode, factor, 0);
	    }
	  /* Try to use an unshifted CNT[BHWD] or RDVL.  */
	  else if (aarch64_sve_cnt_factor_p (factor)
		   || aarch64_sve_rdvl_addvl_factor_p (factor))
	    {
	      val = gen_int_mode (poly_int64 (factor, factor), mode);
	      shift = 0;
	    }
	  /* Try to subtract an unshifted CNT[BHWD].  */
	  else if (aarch64_sve_cnt_factor_p (-factor))
	    {
	      code = MINUS;
	      val = gen_int_mode (poly_int64 (-factor, -factor), mode);
	      shift = 0;
	    }
	  /* If subtraction is free, prefer to load a positive constant.
	     In the best case this will fit a shifted CNTB.  */
	  else if (src != const0_rtx && rel_factor < 0)
	    {
	      code = MINUS;
	      val = gen_int_mode (-rel_factor * BYTES_PER_SVE_VECTOR, mode);
	    }
	  /* Otherwise use a shifted RDVL or CNT[BHWD].  */
	  else
	    val = gen_int_mode (rel_factor * BYTES_PER_SVE_VECTOR, mode);
	}
      else
	{
	  /* If we can calculate CNTB << SHIFT directly, prefer to do that,
	     since it should increase the chances of being able to use
	     a shift and add sequence for the multiplication.
	     If CNTB << SHIFT is out of range, stick with the current
	     shift factor.  */
	  if (force_isa_mode == 0
	      && IN_RANGE (low_bit, 2, 16 * 16))
	    {
	      val = gen_int_mode (poly_int64 (low_bit, low_bit), mode);
	      shift = 0;
	    }
	  else if ((force_isa_mode & AARCH64_ISA_MODE_SM_ON)
		   && aarch64_sve_rdvl_addvl_factor_p (low_bit))
	    {
	      val = aarch64_sme_vq_immediate (mode, low_bit, 0);
	      shift = 0;
	    }
	  else
	    val = gen_int_mode (BYTES_PER_SVE_VECTOR, mode);

	  val = aarch64_force_temporary (mode, temp1, val);

	  /* Prefer to multiply by a positive factor and subtract rather
	     than multiply by a negative factor and add, since positive
	     values are usually easier to move.  */
	  if (rel_factor < 0 && src != const0_rtx)
	    {
	      rel_factor = -rel_factor;
	      code = MINUS;
	    }

	  if (can_create_pseudo_p ())
	    {
	      rtx coeff1 = gen_int_mode (rel_factor, mode);
	      val = expand_mult (mode, val, coeff1, NULL_RTX, true, true);
	    }
	  else
	    {
	      rtx coeff1 = gen_int_mode (rel_factor, mode);
	      coeff1 = aarch64_force_temporary (mode, temp2, coeff1);
	      val = gen_rtx_MULT (mode, val, coeff1);
	    }
	}

      /* Multiply by 2 ** SHIFT.  */
      if (shift > 0)
	{
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_ASHIFT (mode, val, GEN_INT (shift));
	}
      else if (shift < 0)
	{
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_ASHIFTRT (mode, val, GEN_INT (-shift));
	}

      /* Add the result to SRC or subtract the result from SRC.  */
      if (src != const0_rtx)
	{
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_fmt_ee (code, mode, src, val);
	}
      else if (code == MINUS)
	{
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_NEG (mode, val);
	}

      if (constant == 0 || frame_related_p)
	{
	  rtx_insn *insn = emit_insn (gen_rtx_SET (dest, val));
	  if (frame_related_p)
	    {
	      RTX_FRAME_RELATED_P (insn) = true;
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (dest, plus_constant (Pmode, src,
							      poly_offset)));
	    }
	  src = dest;
	  if (constant == 0)
	    return;
	}
      else
	{
	  src = aarch64_force_temporary (mode, temp1, val);
	  temp1 = temp2;
	  temp2 = NULL_RTX;
	}

      emit_move_imm = true;
    }

  aarch64_add_offset_1 (mode, dest, src, constant, temp1,
			frame_related_p, emit_move_imm);
}

/* Like aarch64_add_offset, but the offset is given as an rtx rather
   than a poly_int64.  */

void
aarch64_split_add_offset (scalar_int_mode mode, rtx dest, rtx src,
			  rtx offset_rtx, rtx temp1, rtx temp2)
{
  aarch64_add_offset (mode, dest, src, rtx_to_poly_int64 (offset_rtx),
		      temp1, temp2, 0, false);
}

/* Add DELTA to the stack pointer, marking the instructions frame-related.
   TEMP1 is available as a temporary if nonnull.  FORCE_ISA_MODE is as
   for aarch64_add_offset.  EMIT_MOVE_IMM is false if TEMP1 already
   contains abs (DELTA).  */

static inline void
aarch64_add_sp (rtx temp1, rtx temp2, poly_int64 delta,
		aarch64_isa_mode force_isa_mode, bool emit_move_imm)
{
  aarch64_add_offset (Pmode, stack_pointer_rtx, stack_pointer_rtx, delta,
		      temp1, temp2, force_isa_mode, true, emit_move_imm);
}

/* Subtract DELTA from the stack pointer, marking the instructions
   frame-related if FRAME_RELATED_P.  FORCE_ISA_MODE is as for
   aarch64_add_offset.  TEMP1 is available as a temporary if nonnull.  */

static inline void
aarch64_sub_sp (rtx temp1, rtx temp2, poly_int64 delta,
		aarch64_isa_mode force_isa_mode,
		bool frame_related_p, bool emit_move_imm = true)
{
  aarch64_add_offset (Pmode, stack_pointer_rtx, stack_pointer_rtx, -delta,
		      temp1, temp2, force_isa_mode, frame_related_p,
		      emit_move_imm);
}

/* A streaming-compatible function needs to switch temporarily to the known
   PSTATE.SM mode described by LOCAL_MODE.  The low bit of OLD_SVCR contains
   the runtime state of PSTATE.SM in the streaming-compatible code, before
   the start of the switch to LOCAL_MODE.

   Emit instructions to branch around the mode switch if PSTATE.SM already
   matches LOCAL_MODE.  Return the label that the branch jumps to.  */

static rtx_insn *
aarch64_guard_switch_pstate_sm (rtx old_svcr, aarch64_isa_mode local_mode)
{
  local_mode &= AARCH64_ISA_MODE_SM_STATE;
  gcc_assert (local_mode != 0);
  auto already_ok_cond = (local_mode & AARCH64_ISA_MODE_SM_ON ? NE : EQ);
  auto *label = gen_label_rtx ();
  auto branch = aarch64_gen_test_and_branch (already_ok_cond, old_svcr, 0,
					     label);
  auto *jump = emit_jump_insn (branch);
  JUMP_LABEL (jump) = label;
  return label;
}

/* Emit code to switch from the PSTATE.SM state in OLD_MODE to the PSTATE.SM
   state in NEW_MODE.  This is known to involve either an SMSTART SM or
   an SMSTOP SM.  */

static void
aarch64_switch_pstate_sm (aarch64_isa_mode old_mode, aarch64_isa_mode new_mode)
{
  old_mode &= AARCH64_ISA_MODE_SM_STATE;
  new_mode &= AARCH64_ISA_MODE_SM_STATE;
  gcc_assert (old_mode != new_mode);

  if ((new_mode & AARCH64_ISA_MODE_SM_ON)
      || (!new_mode && (old_mode & AARCH64_ISA_MODE_SM_OFF)))
    emit_insn (gen_aarch64_smstart_sm ());
  else
    emit_insn (gen_aarch64_smstop_sm ());
}

/* As a side-effect, SMSTART SM and SMSTOP SM clobber the contents of all
   FP and predicate registers.  This class emits code to preserve any
   necessary registers around the mode switch.

   The class uses four approaches to saving and restoring contents, enumerated
   by group_type:

   - GPR: save and restore the contents of FP registers using GPRs.
     This is used if the FP register contains no more than 64 significant
     bits.  The registers used are FIRST_GPR onwards.

   - MEM_128: save and restore 128-bit SIMD registers using memory.

   - MEM_SVE_PRED: save and restore full SVE predicate registers using memory.

   - MEM_SVE_DATA: save and restore full SVE vector registers using memory.

   The save slots within each memory group are consecutive, with the
   MEM_SVE_PRED slots occupying a region below the MEM_SVE_DATA slots.

   There will only be two mode switches for each use of SME, so they should
   not be particularly performance-sensitive.  It's also rare for SIMD, SVE
   or predicate registers to be live across mode switches.  We therefore
   don't preallocate the save slots but instead allocate them locally on
   demand.  This makes the code emitted by the class self-contained.  */

class aarch64_sme_mode_switch_regs
{
public:
  static const unsigned int FIRST_GPR = R10_REGNUM;

  void add_reg (machine_mode, unsigned int);
  void add_call_args (rtx_call_insn *);
  void add_call_result (rtx_call_insn *);
  void add_call_preserved_reg (unsigned int);
  void add_call_preserved_regs (bitmap);

  void emit_prologue ();
  void emit_epilogue ();

  /* The number of GPRs needed to save FP registers, starting from
     FIRST_GPR.  */
  unsigned int num_gprs () { return m_group_count[GPR]; }

private:
  enum sequence { PROLOGUE, EPILOGUE };
  enum group_type { GPR, MEM_128, MEM_SVE_PRED, MEM_SVE_DATA, NUM_GROUPS };

  /* Information about the save location for one FP, SIMD, SVE data, or
     SVE predicate register.  */
  struct save_location {
    /* The register to be saved.  */
    rtx reg;

    /* Which group the save location belongs to.  */
    group_type group;

    /* A zero-based index of the register within the group.  */
    unsigned int index;
  };

  unsigned int sve_data_headroom ();
  rtx get_slot_mem (machine_mode, poly_int64);
  void emit_stack_adjust (sequence, poly_int64);
  void emit_mem_move (sequence, const save_location &, poly_int64);

  void emit_gpr_moves (sequence);
  void emit_mem_128_moves (sequence);
  void emit_sve_sp_adjust (sequence);
  void emit_sve_pred_moves (sequence);
  void emit_sve_data_moves (sequence);

  /* All save locations, in no particular order.  */
  auto_vec<save_location, 12> m_save_locations;

  /* The number of registers in each group.  */
  unsigned int m_group_count[NUM_GROUPS] = {};
};

/* Record that (reg:MODE REGNO) needs to be preserved around the mode
   switch.  */

void
aarch64_sme_mode_switch_regs::add_reg (machine_mode mode, unsigned int regno)
{
  if (!FP_REGNUM_P (regno) && !PR_REGNUM_P (regno))
    return;

  unsigned int end_regno = end_hard_regno (mode, regno);
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  gcc_assert ((vec_flags & VEC_STRUCT) || end_regno == regno + 1);
  for (; regno < end_regno; regno++)
    {
      /* Force the mode of SVE saves and restores even for single registers.
	 This is necessary because big-endian targets only allow LDR Z and
	 STR Z to be used with byte modes.  */
      machine_mode submode = mode;
      if (vec_flags & VEC_SVE_PRED)
	submode = VNx16BImode;
      else if (vec_flags & VEC_SVE_DATA)
	submode = SVE_BYTE_MODE;
      else if (vec_flags & VEC_STRUCT)
	{
	  if (vec_flags & VEC_PARTIAL)
	    submode = V8QImode;
	  else
	    submode = V16QImode;
	}
      save_location loc;
      loc.reg = gen_rtx_REG (submode, regno);
      if (vec_flags & VEC_SVE_PRED)
	{
	  gcc_assert (PR_REGNUM_P (regno));
	  loc.group = MEM_SVE_PRED;
	}
      else
	{
	  gcc_assert (FP_REGNUM_P (regno));
	  if (known_le (GET_MODE_SIZE (submode), 8))
	    loc.group = GPR;
	  else if (known_eq (GET_MODE_SIZE (submode), 16))
	    loc.group = MEM_128;
	  else
	    loc.group = MEM_SVE_DATA;
	}
      loc.index = m_group_count[loc.group]++;
      m_save_locations.quick_push (loc);
    }
}

/* Record that the arguments to CALL_INSN need to be preserved around
   the mode switch.  */

void
aarch64_sme_mode_switch_regs::add_call_args (rtx_call_insn *call_insn)
{
  for (rtx node = CALL_INSN_FUNCTION_USAGE (call_insn);
       node; node = XEXP (node, 1))
    {
      rtx item = XEXP (node, 0);
      if (GET_CODE (item) != USE)
	continue;
      item = XEXP (item, 0);
      if (!REG_P (item))
	continue;
      add_reg (GET_MODE (item), REGNO (item));
    }
}

/* Record that the return value from CALL_INSN (if any) needs to be
   preserved around the mode switch.  */

void
aarch64_sme_mode_switch_regs::add_call_result (rtx_call_insn *call_insn)
{
  rtx pat = PATTERN (call_insn);
  gcc_assert (GET_CODE (pat) == PARALLEL);
  pat = XVECEXP (pat, 0, 0);
  if (GET_CODE (pat) == CALL)
    return;
  rtx dest = SET_DEST (pat);
  if (GET_CODE (dest) == PARALLEL)
    for (int i = 0; i < XVECLEN (dest, 0); ++i)
      {
	rtx x = XVECEXP (dest, 0, i);
	gcc_assert (GET_CODE (x) == EXPR_LIST);
	rtx reg = XEXP (x, 0);
	add_reg (GET_MODE (reg), REGNO (reg));
      }
  else
    add_reg (GET_MODE (dest), REGNO (dest));
}

/* REGNO is a register that is call-preserved under the current function's ABI.
   Record that it must be preserved around the mode switch.  */

void
aarch64_sme_mode_switch_regs::add_call_preserved_reg (unsigned int regno)
{
  if (FP_REGNUM_P (regno))
    switch (crtl->abi->id ())
      {
      case ARM_PCS_SVE:
	add_reg (VNx16QImode, regno);
	break;
      case ARM_PCS_SIMD:
	add_reg (V16QImode, regno);
	break;
      case ARM_PCS_AAPCS64:
	add_reg (DImode, regno);
	break;
      default:
	gcc_unreachable ();
      }
  else if (PR_REGNUM_P (regno))
    add_reg (VNx16BImode, regno);
}

/* The hard registers in REGS are call-preserved under the current function's
   ABI.  Record that they must be preserved around the mode switch.  */

void
aarch64_sme_mode_switch_regs::add_call_preserved_regs (bitmap regs)
{
  bitmap_iterator bi;
  unsigned int regno;
  EXECUTE_IF_SET_IN_BITMAP (regs, 0, regno, bi)
    if (HARD_REGISTER_NUM_P (regno))
      add_call_preserved_reg (regno);
    else
      break;
}

/* Emit code to save registers before the mode switch.  */

void
aarch64_sme_mode_switch_regs::emit_prologue ()
{
  emit_sve_sp_adjust (PROLOGUE);
  emit_sve_pred_moves (PROLOGUE);
  emit_sve_data_moves (PROLOGUE);
  emit_mem_128_moves (PROLOGUE);
  emit_gpr_moves (PROLOGUE);
}

/* Emit code to restore registers after the mode switch.  */

void
aarch64_sme_mode_switch_regs::emit_epilogue ()
{
  emit_gpr_moves (EPILOGUE);
  emit_mem_128_moves (EPILOGUE);
  emit_sve_pred_moves (EPILOGUE);
  emit_sve_data_moves (EPILOGUE);
  emit_sve_sp_adjust (EPILOGUE);
}

/* The SVE predicate registers are stored below the SVE data registers,
   with the predicate save area being padded to a data-register-sized
   boundary.  Return the size of this padded area as a whole number
   of data register slots.  */

unsigned int
aarch64_sme_mode_switch_regs::sve_data_headroom ()
{
  return CEIL (m_group_count[MEM_SVE_PRED], 8);
}

/* Return a memory reference of mode MODE to OFFSET bytes from the
   stack pointer.  */

rtx
aarch64_sme_mode_switch_regs::get_slot_mem (machine_mode mode,
					    poly_int64 offset)
{
  rtx addr = plus_constant (Pmode, stack_pointer_rtx, offset);
  return gen_rtx_MEM (mode, addr);
}

/* Allocate or deallocate SIZE bytes of stack space: SEQ decides which.  */

void
aarch64_sme_mode_switch_regs::emit_stack_adjust (sequence seq,
						 poly_int64 size)
{
  if (seq == PROLOGUE)
    size = -size;
  emit_insn (gen_rtx_SET (stack_pointer_rtx,
			  plus_constant (Pmode, stack_pointer_rtx, size)));
}

/* Save or restore the register in LOC, whose slot is OFFSET bytes from
   the stack pointer.  SEQ chooses between saving and restoring.  */

void
aarch64_sme_mode_switch_regs::emit_mem_move (sequence seq,
					     const save_location &loc,
					     poly_int64 offset)
{
  rtx mem = get_slot_mem (GET_MODE (loc.reg), offset);
  if (seq == PROLOGUE)
    emit_move_insn (mem, loc.reg);
  else
    emit_move_insn (loc.reg, mem);
}

/* Emit instructions to save or restore the GPR group.  SEQ chooses between
   saving and restoring.  */

void
aarch64_sme_mode_switch_regs::emit_gpr_moves (sequence seq)
{
  for (auto &loc : m_save_locations)
    if (loc.group == GPR)
      {
	gcc_assert (loc.index < 8);
	rtx gpr = gen_rtx_REG (GET_MODE (loc.reg), FIRST_GPR + loc.index);
	if (seq == PROLOGUE)
	  emit_move_insn (gpr, loc.reg);
	else
	  emit_move_insn (loc.reg, gpr);
      }
}

/* Emit instructions to save or restore the MEM_128 group.  SEQ chooses
   between saving and restoring.  */

void
aarch64_sme_mode_switch_regs::emit_mem_128_moves (sequence seq)
{
  HOST_WIDE_INT count = m_group_count[MEM_128];
  if (count == 0)
    return;

  auto sp = stack_pointer_rtx;
  auto sp_adjust = (seq == PROLOGUE ? -count : count) * 16;

  /* Pick a common mode that supports LDR & STR with pre/post-modification
     and LDP & STP with pre/post-modification.  */
  auto mode = TFmode;

  /* An instruction pattern that should be emitted at the end.  */
  rtx last_pat = NULL_RTX;

  /* A previous MEM_128 location that hasn't been handled yet.  */
  save_location *prev_loc = nullptr;

  /* Look for LDP/STPs and record any leftover LDR/STR in PREV_LOC.  */
  for (auto &loc : m_save_locations)
    if (loc.group == MEM_128)
      {
	if (!prev_loc)
	  {
	    prev_loc = &loc;
	    continue;
	  }
	gcc_assert (loc.index == prev_loc->index + 1);

	/* The offset of the base of the save area from the current
	   stack pointer.  */
	HOST_WIDE_INT bias = 0;
	if (prev_loc->index == 0 && seq == PROLOGUE)
	  bias = sp_adjust;

	/* Get the two sets in the LDP/STP.  */
	rtx ops[] = {
	  gen_rtx_REG (mode, REGNO (prev_loc->reg)),
	  get_slot_mem (mode, prev_loc->index * 16 + bias),
	  gen_rtx_REG (mode, REGNO (loc.reg)),
	  get_slot_mem (mode, loc.index * 16 + bias)
	};
	unsigned int lhs = (seq == PROLOGUE);
	rtx set1 = gen_rtx_SET (ops[lhs], ops[1 - lhs]);
	rtx set2 = gen_rtx_SET (ops[lhs + 2], ops[3 - lhs]);

	/* Combine the sets with any stack allocation/deallocation.  */
	rtx pat;
	if (prev_loc->index == 0)
	  {
	    rtx plus_sp = plus_constant (Pmode, sp, sp_adjust);
	    rtvec vec = gen_rtvec (3, gen_rtx_SET (sp, plus_sp), set1, set2);
	    pat = gen_rtx_PARALLEL (VOIDmode, vec);
	  }
	else if (seq == PROLOGUE)
	  pat = aarch64_gen_store_pair (ops[1], ops[0], ops[2]);
	else
	  pat = aarch64_gen_load_pair (ops[0], ops[2], ops[1]);

	/* Queue a deallocation to the end, otherwise emit the
	   instruction now.  */
	if (seq == EPILOGUE && prev_loc->index == 0)
	  last_pat = pat;
	else
	  emit_insn (pat);
	prev_loc = nullptr;
      }

  /* Handle any leftover LDR/STR.  */
  if (prev_loc)
    {
      rtx reg = gen_rtx_REG (mode, REGNO (prev_loc->reg));
      rtx addr;
      if (prev_loc->index != 0)
	addr = plus_constant (Pmode, sp, prev_loc->index * 16);
      else if (seq == PROLOGUE)
	{
	  rtx allocate = plus_constant (Pmode, sp, -count * 16);
	  addr = gen_rtx_PRE_MODIFY (Pmode, sp, allocate);
	}
      else
	{
	  rtx deallocate = plus_constant (Pmode, sp, count * 16);
	  addr = gen_rtx_POST_MODIFY (Pmode, sp, deallocate);
	}
      rtx mem = gen_rtx_MEM (mode, addr);
      if (seq == PROLOGUE)
	emit_move_insn (mem, reg);
      else
	emit_move_insn (reg, mem);
    }

  if (last_pat)
    emit_insn (last_pat);
}

/* Allocate or deallocate the stack space needed by the SVE groups.
   SEQ chooses between allocating and deallocating.  */

void
aarch64_sme_mode_switch_regs::emit_sve_sp_adjust (sequence seq)
{
  if (unsigned int count = m_group_count[MEM_SVE_DATA] + sve_data_headroom ())
    emit_stack_adjust (seq, count * BYTES_PER_SVE_VECTOR);
}

/* Save or restore the MEM_SVE_DATA group.  SEQ chooses between saving
   and restoring.  */

void
aarch64_sme_mode_switch_regs::emit_sve_data_moves (sequence seq)
{
  for (auto &loc : m_save_locations)
    if (loc.group == MEM_SVE_DATA)
      {
	auto index = loc.index + sve_data_headroom ();
	emit_mem_move (seq, loc, index * BYTES_PER_SVE_VECTOR);
      }
}

/* Save or restore the MEM_SVE_PRED group.  SEQ chooses between saving
   and restoring.  */

void
aarch64_sme_mode_switch_regs::emit_sve_pred_moves (sequence seq)
{
  for (auto &loc : m_save_locations)
    if (loc.group == MEM_SVE_PRED)
      emit_mem_move (seq, loc, loc.index * BYTES_PER_SVE_PRED);
}

/* Set DEST to (vec_series BASE STEP).  */

static void
aarch64_expand_vec_series (rtx dest, rtx base, rtx step)
{
  machine_mode mode = GET_MODE (dest);
  scalar_mode inner = GET_MODE_INNER (mode);

  /* Each operand can be a register or an immediate in the range [-16, 15].  */
  if (!aarch64_sve_index_immediate_p (base))
    base = force_reg (inner, base);
  if (!aarch64_sve_index_immediate_p (step))
    step = force_reg (inner, step);

  emit_set_insn (dest, gen_rtx_VEC_SERIES (mode, base, step));
}

/* Duplicate 128-bit Advanced SIMD vector SRC so that it fills an SVE
   register of mode MODE.  Use TARGET for the result if it's nonnull
   and convenient.

   The two vector modes must have the same element mode.  The behavior
   is to duplicate architectural lane N of SRC into architectural lanes
   N + I * STEP of the result.  On big-endian targets, architectural
   lane 0 of an Advanced SIMD vector is the last element of the vector
   in memory layout, so for big-endian targets this operation has the
   effect of reversing SRC before duplicating it.  Callers need to
   account for this.  */

rtx
aarch64_expand_sve_dupq (rtx target, machine_mode mode, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  gcc_assert (GET_MODE_INNER (mode) == GET_MODE_INNER (src_mode));
  insn_code icode = (BYTES_BIG_ENDIAN
		     ? code_for_aarch64_vec_duplicate_vq_be (mode)
		     : code_for_aarch64_vec_duplicate_vq_le (mode));

  unsigned int i = 0;
  expand_operand ops[3];
  create_output_operand (&ops[i++], target, mode);
  create_output_operand (&ops[i++], src, src_mode);
  if (BYTES_BIG_ENDIAN)
    {
      /* Create a PARALLEL describing the reversal of SRC.  */
      unsigned int nelts_per_vq = 128 / GET_MODE_UNIT_BITSIZE (mode);
      rtx sel = aarch64_gen_stepped_int_parallel (nelts_per_vq,
						  nelts_per_vq - 1, -1);
      create_fixed_operand (&ops[i++], sel);
    }
  expand_insn (icode, i, ops);
  return ops[0].value;
}

/* Try to force 128-bit vector value SRC into memory and use LD1RQ to fetch
   the memory image into DEST.  Return true on success.  */

static bool
aarch64_expand_sve_ld1rq (rtx dest, rtx src)
{
  src = force_const_mem (GET_MODE (src), src);
  if (!src)
    return false;

  /* Make sure that the address is legitimate.  */
  if (!aarch64_sve_ld1rq_operand_p (src))
    {
      rtx addr = force_reg (Pmode, XEXP (src, 0));
      src = replace_equiv_address (src, addr);
    }

  machine_mode mode = GET_MODE (dest);
  machine_mode pred_mode = aarch64_sve_pred_mode (mode);
  rtx ptrue = aarch64_ptrue_reg (pred_mode);
  emit_insn (gen_aarch64_sve_ld1rq (mode, dest, src, ptrue));
  return true;
}

/* SRC is an SVE CONST_VECTOR that contains N "foreground" values followed
   by N "background" values.  Try to move it into TARGET using:

      PTRUE PRED.<T>, VL<N>
      MOV TRUE.<T>, #<foreground>
      MOV FALSE.<T>, #<background>
      SEL TARGET.<T>, PRED.<T>, TRUE.<T>, FALSE.<T>

   The PTRUE is always a single instruction but the MOVs might need a
   longer sequence.  If the background value is zero (as it often is),
   the sequence can sometimes collapse to a PTRUE followed by a
   zero-predicated move.

   Return the target on success, otherwise return null.  */

static rtx
aarch64_expand_sve_const_vector_sel (rtx target, rtx src)
{
  gcc_assert (CONST_VECTOR_NELTS_PER_PATTERN (src) == 2);

  /* Make sure that the PTRUE is valid.  */
  machine_mode mode = GET_MODE (src);
  machine_mode pred_mode = aarch64_sve_pred_mode (mode);
  unsigned int npatterns = CONST_VECTOR_NPATTERNS (src);
  if (aarch64_svpattern_for_vl (pred_mode, npatterns)
      == AARCH64_NUM_SVPATTERNS)
    return NULL_RTX;

  rtx_vector_builder pred_builder (pred_mode, npatterns, 2);
  rtx_vector_builder true_builder (mode, npatterns, 1);
  rtx_vector_builder false_builder (mode, npatterns, 1);
  for (unsigned int i = 0; i < npatterns; ++i)
    {
      true_builder.quick_push (CONST_VECTOR_ENCODED_ELT (src, i));
      pred_builder.quick_push (CONST1_RTX (BImode));
    }
  for (unsigned int i = 0; i < npatterns; ++i)
    {
      false_builder.quick_push (CONST_VECTOR_ENCODED_ELT (src, i + npatterns));
      pred_builder.quick_push (CONST0_RTX (BImode));
    }
  expand_operand ops[4];
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], true_builder.build (), mode);
  create_input_operand (&ops[2], false_builder.build (), mode);
  create_input_operand (&ops[3], pred_builder.build (), pred_mode);
  expand_insn (code_for_vcond_mask (mode, mode), 4, ops);
  return target;
}

/* Return a register containing CONST_VECTOR SRC, given that SRC has an
   SVE data mode and isn't a legitimate constant.  Use TARGET for the
   result if convenient.

   The returned register can have whatever mode seems most natural
   given the contents of SRC.  */

static rtx
aarch64_expand_sve_const_vector (rtx target, rtx src)
{
  machine_mode mode = GET_MODE (src);
  unsigned int npatterns = CONST_VECTOR_NPATTERNS (src);
  unsigned int nelts_per_pattern = CONST_VECTOR_NELTS_PER_PATTERN (src);
  scalar_mode elt_mode = GET_MODE_INNER (mode);
  unsigned int elt_bits = GET_MODE_BITSIZE (elt_mode);
  unsigned int container_bits = aarch64_sve_container_bits (mode);
  unsigned int encoded_bits = npatterns * nelts_per_pattern * container_bits;

  if (nelts_per_pattern == 1
      && encoded_bits <= 128
      && container_bits != elt_bits)
    {
      /* We have a partial vector mode and a constant whose full-vector
	 equivalent would occupy a repeating 128-bit sequence.  Build that
	 full-vector equivalent instead, so that we have the option of
	 using LD1RQ and Advanced SIMD operations.  */
      unsigned int repeat = container_bits / elt_bits;
      machine_mode full_mode = aarch64_full_sve_mode (elt_mode).require ();
      rtx_vector_builder builder (full_mode, npatterns * repeat, 1);
      for (unsigned int i = 0; i < npatterns; ++i)
	for (unsigned int j = 0; j < repeat; ++j)
	  builder.quick_push (CONST_VECTOR_ENCODED_ELT (src, i));
      target = aarch64_target_reg (target, full_mode);
      return aarch64_expand_sve_const_vector (target, builder.build ());
    }

  if (nelts_per_pattern == 1 && encoded_bits == 128)
    {
      /* The constant is a duplicated quadword but can't be narrowed
	 beyond a quadword.  Get the memory image of the first quadword
	 as a 128-bit vector and try using LD1RQ to load it from memory.

	 The effect for both endiannesses is to load memory lane N into
	 architectural lanes N + I * STEP of the result.  On big-endian
	 targets, the layout of the 128-bit vector in an Advanced SIMD
	 register would be different from its layout in an SVE register,
	 but this 128-bit vector is a memory value only.  */
      machine_mode vq_mode = aarch64_vq_mode (elt_mode).require ();
      rtx vq_value = simplify_gen_subreg (vq_mode, src, mode, 0);
      if (vq_value && aarch64_expand_sve_ld1rq (target, vq_value))
	return target;
    }

  if (nelts_per_pattern == 1 && encoded_bits < 128)
    {
      /* The vector is a repeating sequence of 64 bits or fewer.
	 See if we can load them using an Advanced SIMD move and then
	 duplicate it to fill a vector.  This is better than using a GPR
	 move because it keeps everything in the same register file.  */
      machine_mode vq_mode = aarch64_vq_mode (elt_mode).require ();
      rtx_vector_builder builder (vq_mode, npatterns, 1);
      for (unsigned int i = 0; i < npatterns; ++i)
	{
	  /* We want memory lane N to go into architectural lane N,
	     so reverse for big-endian targets.  The DUP .Q pattern
	     has a compensating reverse built-in.  */
	  unsigned int srci = BYTES_BIG_ENDIAN ? npatterns - i - 1 : i;
	  builder.quick_push (CONST_VECTOR_ENCODED_ELT (src, srci));
	}
      rtx vq_src = builder.build ();
      if (aarch64_simd_valid_mov_imm (vq_src))
	{
	  vq_src = force_reg (vq_mode, vq_src);
	  return aarch64_expand_sve_dupq (target, mode, vq_src);
	}

      /* Get an integer representation of the repeating part of Advanced
	 SIMD vector VQ_SRC.  This preserves the endianness of VQ_SRC,
	 which for big-endian targets is lane-swapped wrt a normal
	 Advanced SIMD vector.  This means that for both endiannesses,
	 memory lane N of SVE vector SRC corresponds to architectural
	 lane N of a register holding VQ_SRC.  This in turn means that
	 memory lane 0 of SVE vector SRC is in the lsb of VQ_SRC (viewed
	 as a single 128-bit value) and thus that memory lane 0 of SRC is
	 in the lsb of the integer.  Duplicating the integer therefore
	 ensures that memory lane N of SRC goes into architectural lane
	 N + I * INDEX of the SVE register.  */
      scalar_mode int_mode = int_mode_for_size (encoded_bits, 0).require ();
      rtx elt_value = simplify_gen_subreg (int_mode, vq_src, vq_mode, 0);
      if (elt_value)
	{
	  /* Pretend that we had a vector of INT_MODE to start with.  */
	  elt_mode = int_mode;
	  mode = aarch64_full_sve_mode (int_mode).require ();

	  /* If the integer can be moved into a general register by a
	     single instruction, do that and duplicate the result.  */
	  if (CONST_INT_P (elt_value)
	      && aarch64_move_imm (INTVAL (elt_value),
				   encoded_bits <= 32 ? SImode : DImode))
	    {
	      elt_value = force_reg (elt_mode, elt_value);
	      return expand_vector_broadcast (mode, elt_value);
	    }
	}
      else if (npatterns == 1)
	/* We're duplicating a single value, but can't do better than
	   force it to memory and load from there.  This handles things
	   like symbolic constants.  */
	elt_value = CONST_VECTOR_ENCODED_ELT (src, 0);

      if (elt_value)
	{
	  /* Load the element from memory if we can, otherwise move it into
	     a register and use a DUP.  */
	  rtx op = force_const_mem (elt_mode, elt_value);
	  if (!op)
	    op = force_reg (elt_mode, elt_value);
	  return expand_vector_broadcast (mode, op);
	}
    }

  /* Try using INDEX.  */
  rtx base, step;
  if (const_vec_series_p (src, &base, &step))
    {
      aarch64_expand_vec_series (target, base, step);
      return target;
    }

  /* From here on, it's better to force the whole constant to memory
     if we can.  */
  if (GET_MODE_NUNITS (mode).is_constant ())
    return NULL_RTX;

  if (nelts_per_pattern == 2)
    if (rtx res = aarch64_expand_sve_const_vector_sel (target, src))
      return res;

  /* Expand each pattern individually.  */
  gcc_assert (npatterns > 1);
  rtx_vector_builder builder;
  auto_vec<rtx, 16> vectors (npatterns);
  for (unsigned int i = 0; i < npatterns; ++i)
    {
      builder.new_vector (mode, 1, nelts_per_pattern);
      for (unsigned int j = 0; j < nelts_per_pattern; ++j)
	builder.quick_push (CONST_VECTOR_ELT (src, i + j * npatterns));
      vectors.quick_push (force_reg (mode, builder.build ()));
    }

  /* Use permutes to interleave the separate vectors.  */
  while (npatterns > 1)
    {
      npatterns /= 2;
      for (unsigned int i = 0; i < npatterns; ++i)
	{
	  rtx tmp = (npatterns == 1 ? target : gen_reg_rtx (mode));
	  rtvec v = gen_rtvec (2, vectors[i], vectors[i + npatterns]);
	  emit_set_insn (tmp, gen_rtx_UNSPEC (mode, v, UNSPEC_ZIP1));
	  vectors[i] = tmp;
	}
    }
  gcc_assert (vectors[0] == target);
  return target;
}

/* Use WHILE to set a predicate register of mode MODE in which the first
   VL bits are set and the rest are clear.  Use TARGET for the register
   if it's nonnull and convenient.  */

static rtx
aarch64_sve_move_pred_via_while (rtx target, machine_mode mode,
				 unsigned int vl)
{
  rtx limit = force_reg (DImode, gen_int_mode (vl, DImode));
  target = aarch64_target_reg (target, mode);
  emit_insn (gen_while (UNSPEC_WHILELO, DImode, mode,
			target, const0_rtx, limit));
  return target;
}

static rtx
aarch64_expand_sve_const_pred_1 (rtx, rtx_vector_builder &, bool);

/* BUILDER is a constant predicate in which the index of every set bit
   is a multiple of ELT_SIZE (which is <= 8).  Try to load the constant
   by inverting every element at a multiple of ELT_SIZE and EORing the
   result with an ELT_SIZE PTRUE.

   Return a register that contains the constant on success, otherwise
   return null.  Use TARGET as the register if it is nonnull and
   convenient.  */

static rtx
aarch64_expand_sve_const_pred_eor (rtx target, rtx_vector_builder &builder,
				   unsigned int elt_size)
{
  /* Invert every element at a multiple of ELT_SIZE, keeping the
     other bits zero.  */
  rtx_vector_builder inv_builder (VNx16BImode, builder.npatterns (),
				  builder.nelts_per_pattern ());
  for (unsigned int i = 0; i < builder.encoded_nelts (); ++i)
    if ((i & (elt_size - 1)) == 0 && INTVAL (builder.elt (i)) == 0)
      inv_builder.quick_push (const1_rtx);
    else
      inv_builder.quick_push (const0_rtx);
  inv_builder.finalize ();

  /* See if we can load the constant cheaply.  */
  rtx inv = aarch64_expand_sve_const_pred_1 (NULL_RTX, inv_builder, false);
  if (!inv)
    return NULL_RTX;

  /* EOR the result with an ELT_SIZE PTRUE.  */
  rtx mask = aarch64_ptrue_all (elt_size);
  mask = force_reg (VNx16BImode, mask);
  inv = gen_lowpart (VNx16BImode, inv);
  target = aarch64_target_reg (target, VNx16BImode);
  emit_insn (gen_aarch64_pred_z (XOR, VNx16BImode, target, mask, inv, mask));
  return target;
}

/* BUILDER is a constant predicate in which the index of every set bit
   is a multiple of ELT_SIZE (which is <= 8).  Try to load the constant
   using a TRN1 of size PERMUTE_SIZE, which is >= ELT_SIZE.  Return the
   register on success, otherwise return null.  Use TARGET as the register
   if nonnull and convenient.  */

static rtx
aarch64_expand_sve_const_pred_trn (rtx target, rtx_vector_builder &builder,
				   unsigned int elt_size,
				   unsigned int permute_size)
{
  /* We're going to split the constant into two new constants A and B,
     with element I of BUILDER going into A if (I & PERMUTE_SIZE) == 0
     and into B otherwise.  E.g. for PERMUTE_SIZE == 4 && ELT_SIZE == 1:

     A: { 0, 1, 2, 3, _, _, _, _, 8, 9, 10, 11, _, _, _, _ }
     B: { 4, 5, 6, 7, _, _, _, _, 12, 13, 14, 15, _, _, _, _ }

     where _ indicates elements that will be discarded by the permute.

     First calculate the ELT_SIZEs for A and B.  */
  unsigned int a_elt_size = GET_MODE_SIZE (DImode);
  unsigned int b_elt_size = GET_MODE_SIZE (DImode);
  for (unsigned int i = 0; i < builder.encoded_nelts (); i += elt_size)
    if (INTVAL (builder.elt (i)) != 0)
      {
	if (i & permute_size)
	  b_elt_size |= i - permute_size;
	else
	  a_elt_size |= i;
      }
  a_elt_size &= -a_elt_size;
  b_elt_size &= -b_elt_size;

  /* Now construct the vectors themselves.  */
  rtx_vector_builder a_builder (VNx16BImode, builder.npatterns (),
				builder.nelts_per_pattern ());
  rtx_vector_builder b_builder (VNx16BImode, builder.npatterns (),
				builder.nelts_per_pattern ());
  unsigned int nelts = builder.encoded_nelts ();
  for (unsigned int i = 0; i < nelts; ++i)
    if (i & (elt_size - 1))
      {
	a_builder.quick_push (const0_rtx);
	b_builder.quick_push (const0_rtx);
      }
    else if ((i & permute_size) == 0)
      {
	/* The A and B elements are significant.  */
	a_builder.quick_push (builder.elt (i));
	b_builder.quick_push (builder.elt (i + permute_size));
      }
    else
      {
	/* The A and B elements are going to be discarded, so pick whatever
	   is likely to give a nice constant.  We are targeting element
	   sizes A_ELT_SIZE and B_ELT_SIZE for A and B respectively,
	   with the aim of each being a sequence of ones followed by
	   a sequence of zeros.  So:

	   * if X_ELT_SIZE <= PERMUTE_SIZE, the best approach is to
	     duplicate the last X_ELT_SIZE element, to extend the
	     current sequence of ones or zeros.

	   * if X_ELT_SIZE > PERMUTE_SIZE, the best approach is to add a
	     zero, so that the constant really does have X_ELT_SIZE and
	     not a smaller size.  */
	if (a_elt_size > permute_size)
	  a_builder.quick_push (const0_rtx);
	else
	  a_builder.quick_push (a_builder.elt (i - a_elt_size));
	if (b_elt_size > permute_size)
	  b_builder.quick_push (const0_rtx);
	else
	  b_builder.quick_push (b_builder.elt (i - b_elt_size));
      }
  a_builder.finalize ();
  b_builder.finalize ();

  /* Try loading A into a register.  */
  rtx_insn *last = get_last_insn ();
  rtx a = aarch64_expand_sve_const_pred_1 (NULL_RTX, a_builder, false);
  if (!a)
    return NULL_RTX;

  /* Try loading B into a register.  */
  rtx b = a;
  if (a_builder != b_builder)
    {
      b = aarch64_expand_sve_const_pred_1 (NULL_RTX, b_builder, false);
      if (!b)
	{
	  delete_insns_since (last);
	  return NULL_RTX;
	}
    }

  /* Emit the TRN1 itself.  We emit a TRN that operates on VNx16BI
     operands but permutes them as though they had mode MODE.  */
  machine_mode mode = aarch64_sve_pred_mode (permute_size).require ();
  target = aarch64_target_reg (target, GET_MODE (a));
  rtx type_reg = CONST0_RTX (mode);
  emit_insn (gen_aarch64_sve_trn1_conv (mode, target, a, b, type_reg));
  return target;
}

/* Subroutine of aarch64_expand_sve_const_pred.  Try to load the VNx16BI
   constant in BUILDER into an SVE predicate register.  Return the register
   on success, otherwise return null.  Use TARGET for the register if
   nonnull and convenient.

   ALLOW_RECURSE_P is true if we can use methods that would call this
   function recursively.  */

static rtx
aarch64_expand_sve_const_pred_1 (rtx target, rtx_vector_builder &builder,
				 bool allow_recurse_p)
{
  if (builder.encoded_nelts () == 1)
    /* A PFALSE or a PTRUE .B ALL.  */
    return aarch64_emit_set_immediate (target, builder);

  unsigned int elt_size = aarch64_widest_sve_pred_elt_size (builder);
  if (int vl = aarch64_partial_ptrue_length (builder, elt_size))
    {
      /* If we can load the constant using PTRUE, use it as-is.  */
      machine_mode mode = aarch64_sve_pred_mode (elt_size).require ();
      if (aarch64_svpattern_for_vl (mode, vl) != AARCH64_NUM_SVPATTERNS)
	return aarch64_emit_set_immediate (target, builder);

      /* Otherwise use WHILE to set the first VL bits.  */
      return aarch64_sve_move_pred_via_while (target, mode, vl);
    }

  if (!allow_recurse_p)
    return NULL_RTX;

  /* Try inverting the vector in element size ELT_SIZE and then EORing
     the result with an ELT_SIZE PTRUE.  */
  if (INTVAL (builder.elt (0)) == 0)
    if (rtx res = aarch64_expand_sve_const_pred_eor (target, builder,
						     elt_size))
      return res;

  /* Try using TRN1 to permute two simpler constants.  */
  for (unsigned int i = elt_size; i <= 8; i *= 2)
    if (rtx res = aarch64_expand_sve_const_pred_trn (target, builder,
						     elt_size, i))
      return res;

  return NULL_RTX;
}

/* Return an SVE predicate register that contains the VNx16BImode
   constant in BUILDER, without going through the move expanders.

   The returned register can have whatever mode seems most natural
   given the contents of BUILDER.  Use TARGET for the result if
   convenient.  */

static rtx
aarch64_expand_sve_const_pred (rtx target, rtx_vector_builder &builder)
{
  /* Try loading the constant using pure predicate operations.  */
  if (rtx res = aarch64_expand_sve_const_pred_1 (target, builder, true))
    return res;

  /* Try forcing the constant to memory.  */
  if (builder.full_nelts ().is_constant ())
    if (rtx mem = force_const_mem (VNx16BImode, builder.build ()))
      {
	target = aarch64_target_reg (target, VNx16BImode);
	emit_move_insn (target, mem);
	return target;
      }

  /* The last resort is to load the constant as an integer and then
     compare it against zero.  Use -1 for set bits in order to increase
     the changes of using SVE DUPM or an Advanced SIMD byte mask.  */
  rtx_vector_builder int_builder (VNx16QImode, builder.npatterns (),
				  builder.nelts_per_pattern ());
  for (unsigned int i = 0; i < builder.encoded_nelts (); ++i)
    int_builder.quick_push (INTVAL (builder.elt (i))
			    ? constm1_rtx : const0_rtx);
  return aarch64_convert_sve_data_to_pred (target, VNx16BImode,
					   int_builder.build ());
}

/* Set DEST to immediate IMM.  */

void
aarch64_expand_mov_immediate (rtx dest, rtx imm)
{
  machine_mode mode = GET_MODE (dest);

  /* Check on what type of symbol it is.  */
  scalar_int_mode int_mode;
  if ((SYMBOL_REF_P (imm)
       || LABEL_REF_P (imm)
       || GET_CODE (imm) == CONST
       || GET_CODE (imm) == CONST_POLY_INT)
      && is_a <scalar_int_mode> (mode, &int_mode))
    {
      rtx mem;
      poly_int64 offset;
      HOST_WIDE_INT const_offset;
      enum aarch64_symbol_type sty;

      /* If we have (const (plus symbol offset)), separate out the offset
	 before we start classifying the symbol.  */
      rtx base = strip_offset (imm, &offset);

      /* We must always add an offset involving VL separately, rather than
	 folding it into the relocation.  */
      if (!offset.is_constant (&const_offset))
	{
	  if (!TARGET_SVE)
	    {
	      aarch64_report_sve_required ();
	      return;
	    }
	  if (base == const0_rtx
	      && (aarch64_sve_cnt_immediate_p (offset)
		  || aarch64_sve_rdvl_immediate_p (offset)))
	    emit_insn (gen_rtx_SET (dest, imm));
	  else
	    {
	      /* Do arithmetic on 32-bit values if the result is smaller
		 than that.  */
	      if (partial_subreg_p (int_mode, SImode))
		{
		  /* It is invalid to do symbol calculations in modes
		     narrower than SImode.  */
		  gcc_assert (base == const0_rtx);
		  dest = gen_lowpart (SImode, dest);
		  int_mode = SImode;
		}
	      if (base != const0_rtx)
		{
		  base = aarch64_force_temporary (int_mode, dest, base);
		  aarch64_add_offset (int_mode, dest, base, offset,
				      NULL_RTX, NULL_RTX, 0, false);
		}
	      else
		aarch64_add_offset (int_mode, dest, base, offset,
				    dest, NULL_RTX, 0, false);
	    }
	  return;
	}

      if (aarch64_rdsvl_immediate_p (base))
	{
	  /* We could handle non-constant offsets if they are ever
	     generated.  */
	  gcc_assert (const_offset == 0);
	  emit_insn (gen_rtx_SET (dest, imm));
	  return;
	}

      sty = aarch64_classify_symbol (base, const_offset);
      switch (sty)
	{
	case SYMBOL_FORCE_TO_MEM:
	  if (int_mode != ptr_mode)
	    imm = convert_memory_address (ptr_mode, imm);

	  if (const_offset != 0
	      && targetm.cannot_force_const_mem (ptr_mode, imm))
	    {
	      gcc_assert (can_create_pseudo_p ());
	      base = aarch64_force_temporary (int_mode, dest, base);
	      aarch64_add_offset (int_mode, dest, base, const_offset,
				  NULL_RTX, NULL_RTX, 0, false);
	      return;
	    }

	  mem = force_const_mem (ptr_mode, imm);
	  gcc_assert (mem);

	  /* If we aren't generating PC relative literals, then
	     we need to expand the literal pool access carefully.
	     This is something that needs to be done in a number
	     of places, so could well live as a separate function.  */
	  if (!aarch64_pcrelative_literal_loads)
	    {
	      gcc_assert (can_create_pseudo_p ());
	      base = gen_reg_rtx (ptr_mode);
	      aarch64_expand_mov_immediate (base, XEXP (mem, 0));
	      if (ptr_mode != Pmode)
		base = convert_memory_address (Pmode, base);
	      mem = gen_rtx_MEM (ptr_mode, base);
	    }

	  if (int_mode != ptr_mode)
	    mem = gen_rtx_ZERO_EXTEND (int_mode, mem);

	  emit_insn (gen_rtx_SET (dest, mem));

	  return;

        case SYMBOL_SMALL_TLSGD:
        case SYMBOL_SMALL_TLSDESC:
	case SYMBOL_SMALL_TLSIE:
	case SYMBOL_SMALL_GOT_28K:
	case SYMBOL_SMALL_GOT_4G:
	case SYMBOL_TINY_GOT:
	case SYMBOL_TINY_TLSIE:
	  if (const_offset != 0)
	    {
	      gcc_assert(can_create_pseudo_p ());
	      base = aarch64_force_temporary (int_mode, dest, base);
	      aarch64_add_offset (int_mode, dest, base, const_offset,
				  NULL_RTX, NULL_RTX, 0, false);
	      return;
	    }
	  /* FALLTHRU */

	case SYMBOL_SMALL_ABSOLUTE:
	case SYMBOL_TINY_ABSOLUTE:
	case SYMBOL_TLSLE12:
	case SYMBOL_TLSLE24:
	case SYMBOL_TLSLE32:
	case SYMBOL_TLSLE48:
	  aarch64_load_symref_appropriately (dest, imm, sty);
	  return;

	default:
	  gcc_unreachable ();
	}
    }

  if (!CONST_INT_P (imm))
    {
      if (aarch64_sve_pred_mode_p (mode))
	{
	  /* Only the low bit of each .H, .S and .D element is defined,
	     so we can set the upper bits to whatever we like.  If the
	     predicate is all-true in MODE, prefer to set all the undefined
	     bits as well, so that we can share a single .B predicate for
	     all modes.  */
	  if (imm == CONSTM1_RTX (mode))
	    imm = CONSTM1_RTX (VNx16BImode);

	  /* All methods for constructing predicate modes wider than VNx16BI
	     will set the upper bits of each element to zero.  Expose this
	     by moving such constants as a VNx16BI, so that all bits are
	     significant and so that constants for different modes can be
	     shared.  The wider constant will still be available as a
	     REG_EQUAL note.  */
	  rtx_vector_builder builder;
	  if (aarch64_get_sve_pred_bits (builder, imm))
	    {
	      rtx res = aarch64_expand_sve_const_pred (dest, builder);
	      if (dest != res)
		emit_move_insn (dest, gen_lowpart (mode, res));
	      return;
	    }
	}

      if (GET_CODE (imm) == HIGH || aarch64_simd_valid_mov_imm (imm))
	{
	  emit_insn (gen_rtx_SET (dest, imm));
	  return;
	}

      if (CONST_VECTOR_P (imm) && aarch64_sve_data_mode_p (mode))
	if (rtx res = aarch64_expand_sve_const_vector (dest, imm))
	  {
	    if (dest != res)
	      emit_insn (gen_aarch64_sve_reinterpret (mode, dest, res));
	    return;
	  }

      rtx mem = force_const_mem (mode, imm);
      gcc_assert (mem);
      emit_move_insn (dest, mem);
      return;
    }

  aarch64_internal_mov_immediate (dest, imm, true, mode);
}

/* Return the MEM rtx that provides the canary value that should be used
   for stack-smashing protection.  MODE is the mode of the memory.
   For SSP_GLOBAL, DECL_RTL is the MEM rtx for the canary variable
   (__stack_chk_guard), otherwise it has no useful value.  SALT_TYPE
   indicates whether the caller is performing a SET or a TEST operation.  */

rtx
aarch64_stack_protect_canary_mem (machine_mode mode, rtx decl_rtl,
				  aarch64_salt_type salt_type)
{
  rtx addr;
  if (aarch64_stack_protector_guard == SSP_GLOBAL)
    {
      gcc_assert (MEM_P (decl_rtl));
      addr = XEXP (decl_rtl, 0);
      poly_int64 offset;
      rtx base = strip_offset_and_salt (addr, &offset);
      if (!SYMBOL_REF_P (base))
	return decl_rtl;

      rtvec v = gen_rtvec (2, base, GEN_INT (salt_type));
      addr = gen_rtx_UNSPEC (Pmode, v, UNSPEC_SALT_ADDR);
      addr = gen_rtx_CONST (Pmode, addr);
      addr = plus_constant (Pmode, addr, offset);
    }
  else
    {
      /* Calculate the address from the system register.  */
      rtx salt = GEN_INT (salt_type);
      addr = gen_reg_rtx (mode);
      if (mode == DImode)
	emit_insn (gen_reg_stack_protect_address_di (addr, salt));
      else
	{
	  emit_insn (gen_reg_stack_protect_address_si (addr, salt));
	  addr = convert_memory_address (Pmode, addr);
	}
      addr = plus_constant (Pmode, addr, aarch64_stack_protector_guard_offset);
    }
  return gen_rtx_MEM (mode, force_reg (Pmode, addr));
}

/* Emit an SVE predicated move from SRC to DEST.  PRED is a predicate
   that is known to contain PTRUE.  */

void
aarch64_emit_sve_pred_move (rtx dest, rtx pred, rtx src)
{
  expand_operand ops[3];
  machine_mode mode = GET_MODE (dest);
  create_output_operand (&ops[0], dest, mode);
  create_input_operand (&ops[1], pred, GET_MODE(pred));
  create_input_operand (&ops[2], src, mode);
  temporary_volatile_ok v (true);
  expand_insn (code_for_aarch64_pred_mov (mode), 3, ops);
}

/* Expand a pre-RA SVE data move from SRC to DEST in which at least one
   operand is in memory.  In this case we need to use the predicated LD1
   and ST1 instead of LDR and STR, both for correctness on big-endian
   targets and because LD1 and ST1 support a wider range of addressing modes.
   PRED_MODE is the mode of the predicate.

   See the comment at the head of aarch64-sve.md for details about the
   big-endian handling.  */

void
aarch64_expand_sve_mem_move (rtx dest, rtx src, machine_mode pred_mode)
{
  machine_mode mode = GET_MODE (dest);
  rtx ptrue = aarch64_ptrue_reg (pred_mode);
  if (!register_operand (src, mode)
      && !register_operand (dest, mode))
    {
      rtx tmp = gen_reg_rtx (mode);
      if (MEM_P (src))
	aarch64_emit_sve_pred_move (tmp, ptrue, src);
      else
	emit_move_insn (tmp, src);
      src = tmp;
    }
  aarch64_emit_sve_pred_move (dest, ptrue, src);
}

/* Called only on big-endian targets.  See whether an SVE vector move
   from SRC to DEST is effectively a REV[BHW] instruction, because at
   least one operand is a subreg of an SVE vector that has wider or
   narrower elements.  Return true and emit the instruction if so.

   For example:

     (set (reg:VNx8HI R1) (subreg:VNx8HI (reg:VNx16QI R2) 0))

   represents a VIEW_CONVERT between the following vectors, viewed
   in memory order:

     R2: { [0].high, [0].low,  [1].high, [1].low, ... }
     R1: { [0],      [1],      [2],      [3],     ... }

   The high part of lane X in R2 should therefore correspond to lane X*2
   of R1, but the register representations are:

         msb                                      lsb
     R2: ...... [1].high  [1].low   [0].high  [0].low
     R1: ...... [3]       [2]       [1]       [0]

   where the low part of lane X in R2 corresponds to lane X*2 in R1.
   We therefore need a reverse operation to swap the high and low values
   around.

   This is purely an optimization.  Without it we would spill the
   subreg operand to the stack in one mode and reload it in the
   other mode, which has the same effect as the REV.  */

bool
aarch64_maybe_expand_sve_subreg_move (rtx dest, rtx src)
{
  gcc_assert (BYTES_BIG_ENDIAN);

  /* Do not try to optimize subregs that LRA has created for matched
     reloads.  These subregs only exist as a temporary measure to make
     the RTL well-formed, but they are exempt from the usual
     TARGET_CAN_CHANGE_MODE_CLASS rules.

     For example, if we have:

       (set (reg:VNx8HI R1) (foo:VNx8HI (reg:VNx4SI R2)))

     and the constraints require R1 and R2 to be in the same register,
     LRA may need to create RTL such as:

       (set (subreg:VNx4SI (reg:VNx8HI TMP) 0) (reg:VNx4SI R2))
       (set (reg:VNx8HI TMP) (foo:VNx8HI (subreg:VNx4SI (reg:VNx8HI TMP) 0)))
       (set (reg:VNx8HI R1) (reg:VNx8HI TMP))

     which forces both the input and output of the original instruction
     to use the same hard register.  But for this to work, the normal
     rules have to be suppressed on the subreg input, otherwise LRA
     would need to reload that input too, meaning that the process
     would never terminate.  To compensate for this, the normal rules
     are also suppressed for the subreg output of the first move.
     Ignoring the special case and handling the first move normally
     would therefore generate wrong code: we would reverse the elements
     for the first subreg but not reverse them back for the second subreg.  */
  if (SUBREG_P (dest) && !LRA_SUBREG_P (dest))
    dest = SUBREG_REG (dest);
  if (SUBREG_P (src) && !LRA_SUBREG_P (src))
    src = SUBREG_REG (src);

  /* The optimization handles two single SVE REGs with different element
     sizes.  */
  if (!REG_P (dest)
      || !REG_P (src)
      || aarch64_classify_vector_mode (GET_MODE (dest)) != VEC_SVE_DATA
      || aarch64_classify_vector_mode (GET_MODE (src)) != VEC_SVE_DATA
      || (GET_MODE_UNIT_SIZE (GET_MODE (dest))
	  == GET_MODE_UNIT_SIZE (GET_MODE (src))))
    return false;

  /* Generate *aarch64_sve_mov<mode>_subreg_be.  */
  rtx ptrue = aarch64_ptrue_reg (VNx16BImode);
  rtx unspec = gen_rtx_UNSPEC (GET_MODE (dest), gen_rtvec (2, ptrue, src),
			       UNSPEC_REV_SUBREG);
  emit_insn (gen_rtx_SET (dest, unspec));
  return true;
}

/* Return a copy of X with mode MODE, without changing its other
   attributes.  Unlike gen_lowpart, this doesn't care whether the
   mode change is valid.  */

rtx
aarch64_replace_reg_mode (rtx x, machine_mode mode)
{
  if (GET_MODE (x) == mode)
    return x;

  x = shallow_copy_rtx (x);
  set_mode_and_regno (x, mode, REGNO (x));
  return x;
}

/* Return the SVE REV[BHW] unspec for reversing quantites of mode MODE
   stored in wider integer containers.  */

static unsigned int
aarch64_sve_rev_unspec (machine_mode mode)
{
  switch (GET_MODE_UNIT_SIZE (mode))
    {
    case 1: return UNSPEC_REVB;
    case 2: return UNSPEC_REVH;
    case 4: return UNSPEC_REVW;
    }
  gcc_unreachable ();
}

/* Split a *aarch64_sve_mov<mode>_subreg_be pattern with the given
   operands.  */

void
aarch64_split_sve_subreg_move (rtx dest, rtx ptrue, rtx src)
{
  /* Decide which REV operation we need.  The mode with wider elements
     determines the mode of the operands and the mode with the narrower
     elements determines the reverse width.  */
  machine_mode mode_with_wider_elts = aarch64_sve_int_mode (GET_MODE (dest));
  machine_mode mode_with_narrower_elts = aarch64_sve_int_mode (GET_MODE (src));
  if (GET_MODE_UNIT_SIZE (mode_with_wider_elts)
      < GET_MODE_UNIT_SIZE (mode_with_narrower_elts))
    std::swap (mode_with_wider_elts, mode_with_narrower_elts);

  unsigned int unspec = aarch64_sve_rev_unspec (mode_with_narrower_elts);
  machine_mode pred_mode = aarch64_sve_pred_mode (mode_with_wider_elts);

  /* Get the operands in the appropriate modes and emit the instruction.  */
  ptrue = gen_lowpart (pred_mode, ptrue);
  dest = aarch64_replace_reg_mode (dest, mode_with_wider_elts);
  src = aarch64_replace_reg_mode (src, mode_with_wider_elts);
  emit_insn (gen_aarch64_pred (unspec, mode_with_wider_elts,
			       dest, ptrue, src));
}

static bool
aarch64_function_ok_for_sibcall (tree, tree exp)
{
  if (crtl->abi->id () != expr_callee_abi (exp).id ())
    return false;

  tree fntype = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (exp)));
  if (aarch64_fntype_pstate_sm (fntype) & ~aarch64_cfun_incoming_pstate_sm ())
    return false;
  for (auto state : { "za", "zt0" })
    if (bool (aarch64_cfun_shared_flags (state))
	!= bool (aarch64_fntype_shared_flags (fntype, state)))
      return false;

  /* BTI J is needed where indirect_return functions may return
     if bti is enabled there.  */
  if (lookup_attribute ("indirect_return", TYPE_ATTRIBUTES (fntype))
      && !lookup_attribute ("indirect_return",
			    TYPE_ATTRIBUTES (TREE_TYPE (cfun->decl))))
    return false;

  return true;
}

/* Subroutine of aarch64_pass_by_reference for arguments that are not
   passed in SVE registers.  */

static bool
aarch64_pass_by_reference_1 (CUMULATIVE_ARGS *pcum,
			     const function_arg_info &arg)
{
  HOST_WIDE_INT size;
  machine_mode dummymode;
  int nregs;

  /* GET_MODE_SIZE (BLKmode) is useless since it is 0.  */
  if (arg.mode == BLKmode && arg.type)
    size = int_size_in_bytes (arg.type);
  else
    /* No frontends can create types with variable-sized modes, so we
       shouldn't be asked to pass or return them.  */
    size = GET_MODE_SIZE (arg.mode).to_constant ();

  /* Aggregates are passed by reference based on their size.  */
  if (arg.aggregate_type_p ())
    size = int_size_in_bytes (arg.type);

  /* Variable sized arguments are always returned by reference.  */
  if (size < 0)
    return true;

  /* Can this be a candidate to be passed in fp/simd register(s)?  */
  if (aarch64_vfp_is_call_or_return_candidate (arg.mode, arg.type,
					       &dummymode, &nregs, NULL,
					       !pcum || pcum->silent_p))
    return false;

  /* Arguments which are variable sized or larger than 2 registers are
     passed by reference unless they are a homogenous floating point
     aggregate.  */
  return size > 2 * UNITS_PER_WORD;
}

/* Implement TARGET_PASS_BY_REFERENCE.  */

static bool
aarch64_pass_by_reference (cumulative_args_t pcum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);

  if (!arg.type)
    return aarch64_pass_by_reference_1 (pcum, arg);

  pure_scalable_type_info pst_info;
  switch (pst_info.analyze (arg.type))
    {
    case pure_scalable_type_info::IS_PST:
      if (pcum && !pcum->silent_p && !TARGET_SVE)
	/* We can't gracefully recover at this point, so make this a
	   fatal error.  */
	fatal_error (input_location, "arguments of type %qT require"
		     " the SVE ISA extension", arg.type);

      /* Variadic SVE types are passed by reference.  Normal non-variadic
	 arguments are too if we've run out of registers.  */
      return (!arg.named
	      || pcum->aapcs_nvrn + pst_info.num_zr () > NUM_FP_ARG_REGS
	      || pcum->aapcs_nprn + pst_info.num_pr () > NUM_PR_ARG_REGS);

    case pure_scalable_type_info::DOESNT_MATTER:
      gcc_assert (aarch64_pass_by_reference_1 (pcum, arg));
      return true;

    case pure_scalable_type_info::NO_ABI_IDENTITY:
    case pure_scalable_type_info::ISNT_PST:
      return aarch64_pass_by_reference_1 (pcum, arg);
    }
  gcc_unreachable ();
}

/* Return TRUE if VALTYPE is padded to its least significant bits.  */
static bool
aarch64_return_in_msb (const_tree valtype)
{
  machine_mode dummy_mode;
  int dummy_int;

  /* Never happens in little-endian mode.  */
  if (!BYTES_BIG_ENDIAN)
    return false;

  /* Only composite types smaller than or equal to 16 bytes can
     be potentially returned in registers.  */
  if (!aarch64_composite_type_p (valtype, TYPE_MODE (valtype))
      || int_size_in_bytes (valtype) <= 0
      || int_size_in_bytes (valtype) > 16)
    return false;

  /* But not a composite that is an HFA (Homogeneous Floating-point Aggregate)
     or an HVA (Homogeneous Short-Vector Aggregate); such a special composite
     is always passed/returned in the least significant bits of fp/simd
     register(s).  */
  if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (valtype), valtype,
					       &dummy_mode, &dummy_int, NULL,
					       false))
    return false;

  /* Likewise pure scalable types for SVE vector and predicate registers.  */
  pure_scalable_type_info pst_info;
  if (pst_info.analyze_registers (valtype))
    return false;

  return true;
}

/* Implement TARGET_FUNCTION_VALUE.
   Define how to find the value returned by a function.  */

static rtx
aarch64_function_value (const_tree type, const_tree func,
			bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsignedp;

  mode = TYPE_MODE (type);
  if (INTEGRAL_TYPE_P (type))
    mode = promote_function_mode (type, mode, &unsignedp, func, 1);

  pure_scalable_type_info pst_info;
  if (type && pst_info.analyze_registers (type))
    return pst_info.get_rtx (mode, V0_REGNUM, P0_REGNUM);

  /* Generic vectors that map to full SVE modes with -msve-vector-bits=N
     are returned in memory, not by value.  */
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  bool sve_p = (vec_flags & VEC_ANY_SVE);

  if (aarch64_return_in_msb (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);

      if (size % UNITS_PER_WORD != 0)
	{
	  size += UNITS_PER_WORD - size % UNITS_PER_WORD;
	  mode = int_mode_for_size (size * BITS_PER_UNIT, 0).require ();
	}
    }

  int count;
  machine_mode ag_mode;
  if (aarch64_vfp_is_call_or_return_candidate (mode, type, &ag_mode, &count,
					       NULL, false))
    {
      gcc_assert (!sve_p);
      if (!aarch64_composite_type_p (type, mode))
	{
	  gcc_assert (count == 1 && mode == ag_mode);
	  return gen_rtx_REG (mode, V0_REGNUM);
	}
      else if (aarch64_advsimd_full_struct_mode_p (mode)
	       && known_eq (GET_MODE_SIZE (ag_mode), 16))
	return gen_rtx_REG (mode, V0_REGNUM);
      else if (aarch64_advsimd_partial_struct_mode_p (mode)
	       && known_eq (GET_MODE_SIZE (ag_mode), 8))
	return gen_rtx_REG (mode, V0_REGNUM);
      else
	{
	  int i;
	  rtx par;

	  par = gen_rtx_PARALLEL (mode, rtvec_alloc (count));
	  for (i = 0; i < count; i++)
	    {
	      rtx tmp = gen_rtx_REG (ag_mode, V0_REGNUM + i);
	      rtx offset = gen_int_mode (i * GET_MODE_SIZE (ag_mode), Pmode);
	      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp, offset);
	      XVECEXP (par, 0, i) = tmp;
	    }
	  return par;
	}
    }
  else
    {
      if (sve_p)
	{
	  /* Vector types can acquire a partial SVE mode using things like
	     __attribute__((vector_size(N))), and this is potentially useful.
	     However, the choice of mode doesn't affect the type's ABI
	     identity, so we should treat the types as though they had
	     the associated integer mode, just like they did before SVE
	     was introduced.

	     We know that the vector must be 128 bits or smaller,
	     otherwise we'd have returned it in memory instead.  */
	  gcc_assert (type
		      && (aarch64_some_values_include_pst_objects_p (type)
			  || (vec_flags & VEC_PARTIAL)));

	  scalar_int_mode int_mode = int_mode_for_mode (mode).require ();
	  rtx reg = gen_rtx_REG (int_mode, R0_REGNUM);
	  rtx pair = gen_rtx_EXPR_LIST (VOIDmode, reg, const0_rtx);
	  return gen_rtx_PARALLEL (mode, gen_rtvec (1, pair));
	}
      return gen_rtx_REG (mode, R0_REGNUM);
    }
}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.
   Return true if REGNO is the number of a hard register in which the values
   of called function may come back.  */

static bool
aarch64_function_value_regno_p (const unsigned int regno)
{
  /* Maximum of 16 bytes can be returned in the general registers.  Examples
     of 16-byte return values are: 128-bit integers and 16-byte small
     structures (excluding homogeneous floating-point aggregates).  */
  if (regno == R0_REGNUM || regno == R1_REGNUM)
    return true;

  /* Up to four fp/simd registers can return a function value, e.g. a
     homogeneous floating-point aggregate having four members.  */
  if (regno >= V0_REGNUM && regno < V0_REGNUM + HA_MAX_NUM_FLDS)
    return TARGET_FLOAT;

  if (regno >= P0_REGNUM && regno < P0_REGNUM + HA_MAX_NUM_FLDS)
    return TARGET_SVE;

  return false;
}

/* Subroutine for aarch64_return_in_memory for types that are not returned
   in SVE registers.  */

static bool
aarch64_return_in_memory_1 (const_tree type)
{
  HOST_WIDE_INT size;
  machine_mode ag_mode;
  int count;

  if (!AGGREGATE_TYPE_P (type)
      && TREE_CODE (type) != BITINT_TYPE
      && TREE_CODE (type) != COMPLEX_TYPE
      && TREE_CODE (type) != VECTOR_TYPE)
    /* Simple scalar types always returned in registers.  */
    return false;

  if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (type), type,
					       &ag_mode, &count, NULL, false))
    return false;

  /* Types larger than 2 registers returned in memory.  */
  size = int_size_in_bytes (type);
  return (size < 0 || size > 2 * UNITS_PER_WORD);
}

/* Implement TARGET_RETURN_IN_MEMORY.

   If the type T of the result of a function is such that
     void func (T arg)
   would require that arg be passed as a value in a register (or set of
   registers) according to the parameter passing rules, then the result
   is returned in the same registers as would be used for such an
   argument.  */

static bool
aarch64_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  pure_scalable_type_info pst_info;
  switch (pst_info.analyze (type))
    {
    case pure_scalable_type_info::IS_PST:
      return (pst_info.num_zr () > NUM_FP_ARG_REGS
	      || pst_info.num_pr () > NUM_PR_ARG_REGS);

    case pure_scalable_type_info::DOESNT_MATTER:
      gcc_assert (aarch64_return_in_memory_1 (type));
      return true;

    case pure_scalable_type_info::NO_ABI_IDENTITY:
    case pure_scalable_type_info::ISNT_PST:
      return aarch64_return_in_memory_1 (type);
    }
  gcc_unreachable ();
}

static bool
aarch64_vfp_is_call_candidate (cumulative_args_t pcum_v, machine_mode mode,
			       const_tree type, int *nregs)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  return aarch64_vfp_is_call_or_return_candidate (mode, type,
						  &pcum->aapcs_vfp_rmode,
						  nregs, NULL, pcum->silent_p);
}

/* Given MODE and TYPE of a function argument, return the alignment in
   bits.  The idea is to suppress any stronger alignment requested by
   the user and opt for the natural alignment (specified in AAPCS64 \S
   4.1).  ABI_BREAK_GCC_9 is set to the old alignment if the alignment
   was incorrectly calculated in versions of GCC prior to GCC 9.
   ABI_BREAK_GCC_13 is set to the old alignment if it was incorrectly
   calculated in versions between GCC 9 and GCC 13.  If the alignment
   might have changed between GCC 13 and GCC 14, ABI_BREAK_GCC_14
   is the old GCC 13 alignment, otherwise it is zero.

   This is a helper function for local use only.  */

static unsigned int
aarch64_function_arg_alignment (machine_mode mode, const_tree type,
				unsigned int *abi_break_gcc_9,
				unsigned int *abi_break_gcc_13,
				unsigned int *abi_break_gcc_14)
{
  *abi_break_gcc_9 = 0;
  *abi_break_gcc_13 = 0;
  *abi_break_gcc_14 = 0;
  if (!type)
    return GET_MODE_ALIGNMENT (mode);

  if (integer_zerop (TYPE_SIZE (type)))
    return 0;

  gcc_assert (TYPE_MODE (type) == mode);

  if (!AGGREGATE_TYPE_P (type))
    {
      /* The ABI alignment is the natural alignment of the type, without
	 any attributes applied.  Normally this is the alignment of the
	 TYPE_MAIN_VARIANT, but not always; see PR108910 for a counterexample.
	 For now we just handle the known exceptions explicitly.  */
      type = TYPE_MAIN_VARIANT (type);
      if (POINTER_TYPE_P (type))
	{
	  gcc_assert (known_eq (POINTER_SIZE, GET_MODE_BITSIZE (mode)));
	  return POINTER_SIZE;
	}
      if (TREE_CODE (type) == ENUMERAL_TYPE && TREE_TYPE (type))
	{
	  *abi_break_gcc_14 = TYPE_ALIGN (type);
	  type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
	}
      gcc_assert (!TYPE_USER_ALIGN (type));
      return TYPE_ALIGN (type);
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    return TYPE_ALIGN (TREE_TYPE (type));

  unsigned int alignment = 0;
  unsigned int bitfield_alignment_with_packed = 0;
  unsigned int bitfield_alignment = 0;
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      {
	/* Note that we explicitly consider zero-sized fields here,
	   even though they don't map to AAPCS64 machine types.
	   For example, in:

	       struct __attribute__((aligned(8))) empty {};

	       struct s {
		 [[no_unique_address]] empty e;
		 int x;
	       };

	   "s" contains only one Fundamental Data Type (the int field)
	   but gains 8-byte alignment and size thanks to "e".  */
	alignment = std::max (alignment, DECL_ALIGN (field));
	if (DECL_BIT_FIELD_TYPE (field))
	  {
	    /* Take the bit-field type's alignment into account only
	       if the user didn't reduce this field's alignment with
	       the packed attribute.  */
	    if (!DECL_PACKED (field))
	      bitfield_alignment
		= std::max (bitfield_alignment,
			    TYPE_ALIGN (DECL_BIT_FIELD_TYPE (field)));

	    /* Compute the alignment even if the bit-field is
	       packed, so that we can emit a warning in case the
	       alignment changed between GCC versions.  */
	    bitfield_alignment_with_packed
	      = std::max (bitfield_alignment_with_packed,
			  TYPE_ALIGN (DECL_BIT_FIELD_TYPE (field)));
	  }
      }

  /* Emit a warning if the alignment is different when taking the
     'packed' attribute into account.  */
  if (bitfield_alignment != bitfield_alignment_with_packed
      && bitfield_alignment_with_packed > alignment)
    *abi_break_gcc_13 = bitfield_alignment_with_packed;

  if (bitfield_alignment > alignment)
    {
      *abi_break_gcc_9 = alignment;
      return bitfield_alignment;
    }

  return alignment;
}

/* Return true if TYPE describes a _BitInt(N) or an angreggate that uses the
   _BitInt(N) type.  These include ARRAY_TYPE's with an element that is a
   _BitInt(N) or an aggregate that uses it, and a RECORD_TYPE or a UNION_TYPE
   with a field member that is a _BitInt(N) or an aggregate that uses it.
   Return false otherwise.  */

static bool
bitint_or_aggr_of_bitint_p (tree type)
{
  if (!type)
    return false;

  if (TREE_CODE (type) == BITINT_TYPE)
    return true;

  /* If ARRAY_TYPE, check it's element type.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    return bitint_or_aggr_of_bitint_p (TREE_TYPE (type));

  /* If RECORD_TYPE or UNION_TYPE, check the fields' types.  */
  if (RECORD_OR_UNION_TYPE_P (type))
    for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
      {
	if (TREE_CODE (field) != FIELD_DECL)
	  continue;
	if (bitint_or_aggr_of_bitint_p (TREE_TYPE (field)))
	  return true;
      }
  return false;
}

/* Layout a function argument according to the AAPCS64 rules.  The rule
   numbers refer to the rule numbers in the AAPCS64.  ORIG_MODE is the
   mode that was originally given to us by the target hook, whereas the
   mode in ARG might be the result of replacing partial SVE modes with
   the equivalent integer mode.  */

static void
aarch64_layout_arg (cumulative_args_t pcum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  tree type = arg.type;
  machine_mode mode = arg.mode;
  int ncrn, nvrn, nregs;
  bool allocate_ncrn, allocate_nvrn;
  HOST_WIDE_INT size;
  unsigned int abi_break_gcc_9;
  unsigned int abi_break_gcc_13;
  unsigned int abi_break_gcc_14;

  /* We need to do this once per argument.  */
  if (pcum->aapcs_arg_processed)
    return;

  bool warn_pcs_change
    = (warn_psabi
       && !pcum->silent_p
       && (currently_expanding_function_start
	   || currently_expanding_gimple_stmt));

  /* HFAs and HVAs can have an alignment greater than 16 bytes.  For example:

       typedef struct foo {
         __Int8x16_t foo[2] __attribute__((aligned(32)));
       } foo;

     is still a HVA despite its larger-than-normal alignment.
     However, such over-aligned HFAs and HVAs are guaranteed to have
     no padding.

     If we exclude HFAs and HVAs from the discussion below, then there
     are several things to note:

     - Both the C and AAPCS64 interpretations of a type's alignment should
       give a value that is no greater than the type's size.

     - Types bigger than 16 bytes are passed indirectly.

     - If an argument of type T is passed indirectly, TYPE and MODE describe
       a pointer to T rather than T iself.

     It follows that the AAPCS64 alignment of TYPE must be no greater
     than 16 bytes.

     Versions prior to GCC 9.1 ignored a bitfield's underlying type
     and so could calculate an alignment that was too small.  If this
     happened for TYPE then ABI_BREAK_GCC_9 is this older, too-small alignment.

     Although GCC 9.1 fixed that bug, it introduced a different one:
     it would consider the alignment of a bitfield's underlying type even
     if the field was packed (which should have the effect of overriding
     the alignment of the underlying type).  This was fixed in GCC 13.1.

     As a result of this bug, GCC 9 to GCC 12 could calculate an alignment
     that was too big.  If this happened for TYPE, ABI_BREAK_GCC_13 is
     this older, too-big alignment.

     Also, the fact that GCC 9 to GCC 12 considered irrelevant
     alignments meant they could calculate type alignments that were
     bigger than the type's size, contrary to the assumption above.
     The handling of register arguments was nevertheless (and justifiably)
     written to follow the assumption that the alignment can never be
     greater than the size.  The same was not true for stack arguments;
     their alignment was instead handled by MIN bounds in
     aarch64_function_arg_boundary.

     The net effect is that, if GCC 9 to GCC 12 incorrectly calculated
     an alignment of more than 16 bytes for TYPE then:

     - If the argument was passed in registers, these GCC versions
       would treat the alignment as though it was *less than* 16 bytes.

     - If the argument was passed on the stack, these GCC versions
       would treat the alignment as though it was *equal to* 16 bytes.

     Both behaviors were wrong, but in different cases.  */

  pcum->aapcs_arg_processed = true;

  pure_scalable_type_info pst_info;
  if (type && pst_info.analyze_registers (type))
    {
      /* aarch64_function_arg_alignment has never had an effect on
	 this case.  */

      /* The PCS says that it is invalid to pass an SVE value to an
	 unprototyped function.  There is no ABI-defined location we
	 can return in this case, so we have no real choice but to raise
	 an error immediately, even though this is only a query function.  */
      if (arg.named && pcum->pcs_variant != ARM_PCS_SVE)
	{
	  gcc_assert (!pcum->silent_p);
	  error ("SVE type %qT cannot be passed to an unprototyped function",
		 arg.type);
	  /* Avoid repeating the message, and avoid tripping the assert
	     below.  */
	  pcum->pcs_variant = ARM_PCS_SVE;
	}

      /* We would have converted the argument into pass-by-reference
	 form if it didn't fit in registers.  */
      pcum->aapcs_nextnvrn = pcum->aapcs_nvrn + pst_info.num_zr ();
      pcum->aapcs_nextnprn = pcum->aapcs_nprn + pst_info.num_pr ();
      gcc_assert (arg.named
		  && pcum->pcs_variant == ARM_PCS_SVE
		  && pcum->aapcs_nextnvrn <= NUM_FP_ARG_REGS
		  && pcum->aapcs_nextnprn <= NUM_PR_ARG_REGS);
      pcum->aapcs_reg = pst_info.get_rtx (mode, V0_REGNUM + pcum->aapcs_nvrn,
					  P0_REGNUM + pcum->aapcs_nprn);
      return;
    }

  /* Generic vectors that map to full SVE modes with -msve-vector-bits=N
     are passed by reference, not by value.  */
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  bool sve_p = (vec_flags & VEC_ANY_SVE);
  if (sve_p)
    /* Vector types can acquire a partial SVE mode using things like
       __attribute__((vector_size(N))), and this is potentially useful.
       However, the choice of mode doesn't affect the type's ABI
       identity, so we should treat the types as though they had
       the associated integer mode, just like they did before SVE
       was introduced.

       We know that the vector must be 128 bits or smaller,
       otherwise we'd have passed it in memory instead.  */
    gcc_assert (type
		&& (aarch64_some_values_include_pst_objects_p (type)
		    || (vec_flags & VEC_PARTIAL)));

  /* Size in bytes, rounded to the nearest multiple of 8 bytes.  */
  if (type)
    size = int_size_in_bytes (type);
  else
    /* No frontends can create types with variable-sized modes, so we
       shouldn't be asked to pass or return them.  */
    size = GET_MODE_SIZE (mode).to_constant ();
  size = ROUND_UP (size, UNITS_PER_WORD);

  allocate_ncrn = (type) ? !(FLOAT_TYPE_P (type)) : !FLOAT_MODE_P (mode);
  allocate_nvrn = aarch64_vfp_is_call_candidate (pcum_v,
						 mode,
						 type,
						 &nregs);
  gcc_assert (!sve_p || !allocate_nvrn);

  unsigned int alignment
    = aarch64_function_arg_alignment (mode, type, &abi_break_gcc_9,
				      &abi_break_gcc_13, &abi_break_gcc_14);

  gcc_assert ((allocate_nvrn || alignment <= 16 * BITS_PER_UNIT)
	      && (!alignment || abi_break_gcc_9 < alignment)
	      && (!abi_break_gcc_13 || alignment < abi_break_gcc_13));

  /* _BitInt(N) was only added in GCC 14.  */
  bool warn_pcs_change_le_gcc14
    = warn_pcs_change && !bitint_or_aggr_of_bitint_p (type);

  /* allocate_ncrn may be false-positive, but allocate_nvrn is quite reliable.
     The following code thus handles passing by SIMD/FP registers first.  */

  nvrn = pcum->aapcs_nvrn;

  /* C1 - C5 for floating point, homogenous floating point aggregates (HFA)
     and homogenous short-vector aggregates (HVA).  */
  if (allocate_nvrn)
    {
      /* aarch64_function_arg_alignment has never had an effect on
	 this case.  */
      if (!pcum->silent_p && !TARGET_FLOAT)
	aarch64_err_no_fpadvsimd (mode);

      if (nvrn + nregs <= NUM_FP_ARG_REGS)
	{
	  pcum->aapcs_nextnvrn = nvrn + nregs;
	  if (!aarch64_composite_type_p (type, mode))
	    {
	      gcc_assert (nregs == 1);
	      pcum->aapcs_reg = gen_rtx_REG (mode, V0_REGNUM + nvrn);
	    }
	  else if (aarch64_advsimd_full_struct_mode_p (mode)
		   && known_eq (GET_MODE_SIZE (pcum->aapcs_vfp_rmode), 16))
	    pcum->aapcs_reg = gen_rtx_REG (mode, V0_REGNUM + nvrn);
	  else if (aarch64_advsimd_partial_struct_mode_p (mode)
		   && known_eq (GET_MODE_SIZE (pcum->aapcs_vfp_rmode), 8))
	    pcum->aapcs_reg = gen_rtx_REG (mode, V0_REGNUM + nvrn);
	  else
	    {
	      rtx par;
	      int i;
	      par = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));
	      for (i = 0; i < nregs; i++)
		{
		  rtx tmp = gen_rtx_REG (pcum->aapcs_vfp_rmode,
					 V0_REGNUM + nvrn + i);
		  rtx offset = gen_int_mode
		    (i * GET_MODE_SIZE (pcum->aapcs_vfp_rmode), Pmode);
		  tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp, offset);
		  XVECEXP (par, 0, i) = tmp;
		}
	      pcum->aapcs_reg = par;
	    }
	  return;
	}
      else
	{
	  /* C.3 NSRN is set to 8.  */
	  pcum->aapcs_nextnvrn = NUM_FP_ARG_REGS;
	  goto on_stack;
	}
    }

  ncrn = pcum->aapcs_ncrn;
  nregs = size / UNITS_PER_WORD;

  /* C6 - C9.  though the sign and zero extension semantics are
     handled elsewhere.  This is the case where the argument fits
     entirely general registers.  */
  if (allocate_ncrn && (ncrn + nregs <= NUM_ARG_REGS))
    {
      gcc_assert (nregs == 0 || nregs == 1 || nregs == 2);

      /* C.8 if the argument has an alignment of 16 then the NGRN is
	 rounded up to the next even number.  */
      if (nregs == 2
	  && ncrn % 2)
	{
	  /* Emit a warning if the alignment changed when taking the
	     'packed' attribute into account.  */
	  if (warn_pcs_change_le_gcc14
	      && abi_break_gcc_13
	      && ((abi_break_gcc_13 == 16 * BITS_PER_UNIT)
		  != (alignment == 16 * BITS_PER_UNIT)))
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 13.1", type);

	  if (warn_pcs_change_le_gcc14
	      && abi_break_gcc_14
	      && ((abi_break_gcc_14 == 16 * BITS_PER_UNIT)
		  != (alignment == 16 * BITS_PER_UNIT)))
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 14.1", type);

	  /* The == 16 * BITS_PER_UNIT instead of >= 16 * BITS_PER_UNIT
	     comparison is there because for > 16 * BITS_PER_UNIT
	     alignment nregs should be > 2 and therefore it should be
	     passed by reference rather than value.  */
	  if (alignment == 16 * BITS_PER_UNIT)
	    {
	      if (warn_pcs_change_le_gcc14
		  && abi_break_gcc_9)
		inform (input_location, "parameter passing for argument of type "
			"%qT changed in GCC 9.1", type);
	      ++ncrn;
	      gcc_assert (ncrn + nregs <= NUM_ARG_REGS);
	    }
	}

      /* If an argument with an SVE mode needs to be shifted up to the
	 high part of the register, treat it as though it had an integer mode.
	 Using the normal (parallel [...]) would suppress the shifting.  */
      if (sve_p
	  && BYTES_BIG_ENDIAN
	  && maybe_ne (GET_MODE_SIZE (mode), nregs * UNITS_PER_WORD)
	  && aarch64_pad_reg_upward (mode, type, false))
	{
	  mode = int_mode_for_mode (mode).require ();
	  sve_p = false;
	}

      /* NREGS can be 0 when e.g. an empty structure is to be passed.
	 A reg is still generated for it, but the caller should be smart
	 enough not to use it.  */
      if (nregs == 0
	  || (nregs == 1 && !sve_p)
	  || GET_MODE_CLASS (mode) == MODE_INT)
	pcum->aapcs_reg = gen_rtx_REG (mode, R0_REGNUM + ncrn);
      else
	{
	  rtx par;
	  int i;

	  par = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));
	  for (i = 0; i < nregs; i++)
	    {
	      scalar_int_mode reg_mode = word_mode;
	      if (nregs == 1)
		reg_mode = int_mode_for_mode (mode).require ();
	      rtx tmp = gen_rtx_REG (reg_mode, R0_REGNUM + ncrn + i);
	      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp,
				       GEN_INT (i * UNITS_PER_WORD));
	      XVECEXP (par, 0, i) = tmp;
	    }
	  pcum->aapcs_reg = par;
	}

      pcum->aapcs_nextncrn = ncrn + nregs;
      return;
    }

  /* C.11  */
  pcum->aapcs_nextncrn = NUM_ARG_REGS;

  /* The argument is passed on stack; record the needed number of words for
     this argument and align the total size if necessary.  */
on_stack:
  pcum->aapcs_stack_words = size / UNITS_PER_WORD;

  if (warn_pcs_change_le_gcc14
      && abi_break_gcc_13
      && ((abi_break_gcc_13 >= 16 * BITS_PER_UNIT)
	  != (alignment >= 16 * BITS_PER_UNIT)))
    inform (input_location, "parameter passing for argument of type "
	    "%qT changed in GCC 13.1", type);

  if (warn_pcs_change_le_gcc14
      && abi_break_gcc_14
      && ((abi_break_gcc_14 >= 16 * BITS_PER_UNIT)
	  != (alignment >= 16 * BITS_PER_UNIT)))
    inform (input_location, "parameter passing for argument of type "
	    "%qT changed in GCC 14.1", type);

  if (alignment == 16 * BITS_PER_UNIT)
    {
      int new_size = ROUND_UP (pcum->aapcs_stack_size, 16 / UNITS_PER_WORD);
      if (pcum->aapcs_stack_size != new_size)
	{
	  if (warn_pcs_change_le_gcc14
	      && abi_break_gcc_9)
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 9.1", type);
	  pcum->aapcs_stack_size = new_size;
	}
    }
  return;
}

/* Add the current argument register to the set of those that need
   to be saved and restored around a change to PSTATE.SM.  */

static void
aarch64_record_sme_mode_switch_args (CUMULATIVE_ARGS *pcum)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, pcum->aapcs_reg, NONCONST)
    {
      rtx x = *iter;
      if (REG_P (x) && (FP_REGNUM_P (REGNO (x)) || PR_REGNUM_P (REGNO (x))))
	{
	  unsigned int i = pcum->num_sme_mode_switch_args++;
	  gcc_assert (i < ARRAY_SIZE (pcum->sme_mode_switch_args));
	  pcum->sme_mode_switch_args[i] = x;
	}
    }
}

/* Return a parallel that contains all the registers that need to be
   saved around a change to PSTATE.SM.  Return const0_rtx if there is
   no such mode switch, or if no registers need to be saved.  */

static rtx
aarch64_finish_sme_mode_switch_args (CUMULATIVE_ARGS *pcum)
{
  if (!pcum->num_sme_mode_switch_args)
    return const0_rtx;

  auto argvec = gen_rtvec_v (pcum->num_sme_mode_switch_args,
			     pcum->sme_mode_switch_args);
  return gen_rtx_PARALLEL (VOIDmode, argvec);
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
aarch64_function_arg (cumulative_args_t pcum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  gcc_assert (pcum->pcs_variant == ARM_PCS_AAPCS64
	      || pcum->pcs_variant == ARM_PCS_SIMD
	      || pcum->pcs_variant == ARM_PCS_SVE);

  if (arg.end_marker_p ())
    {
      rtx abi_cookie = aarch64_gen_callee_cookie (pcum->isa_mode,
						  pcum->pcs_variant,
						  pcum->indirect_return);
      rtx sme_mode_switch_args = aarch64_finish_sme_mode_switch_args (pcum);
      rtx shared_za_flags = gen_int_mode (pcum->shared_za_flags, SImode);
      rtx shared_zt0_flags = gen_int_mode (pcum->shared_zt0_flags, SImode);
      return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4, abi_cookie,
						    sme_mode_switch_args,
						    shared_za_flags,
						    shared_zt0_flags));
    }

  aarch64_layout_arg (pcum_v, arg);
  return pcum->aapcs_reg;
}

void
aarch64_init_cumulative_args (CUMULATIVE_ARGS *pcum,
			      const_tree fntype,
			      rtx libname ATTRIBUTE_UNUSED,
			      const_tree fndecl,
			      unsigned n_named ATTRIBUTE_UNUSED,
			      bool silent_p)
{
  pcum->aapcs_ncrn = 0;
  pcum->aapcs_nvrn = 0;
  pcum->aapcs_nprn = 0;
  pcum->aapcs_nextncrn = 0;
  pcum->aapcs_nextnvrn = 0;
  pcum->aapcs_nextnprn = 0;
  if (fntype)
    {
      pcum->pcs_variant = (arm_pcs) fntype_abi (fntype).id ();
      pcum->isa_mode = aarch64_fntype_isa_mode (fntype);
      pcum->indirect_return = lookup_attribute ("indirect_return",
						TYPE_ATTRIBUTES (fntype));
    }
  else
    {
      pcum->pcs_variant = ARM_PCS_AAPCS64;
      pcum->isa_mode = AARCH64_DEFAULT_ISA_MODE;
      pcum->indirect_return = false;
    }
  pcum->aapcs_reg = NULL_RTX;
  pcum->aapcs_arg_processed = false;
  pcum->aapcs_stack_words = 0;
  pcum->aapcs_stack_size = 0;
  pcum->silent_p = silent_p;
  pcum->shared_za_flags
    = (fntype ? aarch64_fntype_shared_flags (fntype, "za") : 0U);
  pcum->shared_zt0_flags
    = (fntype ? aarch64_fntype_shared_flags (fntype, "zt0") : 0U);
  pcum->num_sme_mode_switch_args = 0;

  if (!silent_p
      && !TARGET_FLOAT
      && fntype && fntype != error_mark_node)
    {
      const_tree type = TREE_TYPE (fntype);
      machine_mode mode ATTRIBUTE_UNUSED; /* To pass pointer as argument.  */
      int nregs ATTRIBUTE_UNUSED; /* Likewise.  */
      if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (type), type,
						   &mode, &nregs, NULL, false))
	aarch64_err_no_fpadvsimd (TYPE_MODE (type));
    }

  if (!silent_p
      && !TARGET_SVE
      && pcum->pcs_variant == ARM_PCS_SVE)
    {
      /* We can't gracefully recover at this point, so make this a
	 fatal error.  */
      if (fndecl)
	fatal_error (input_location, "%qE requires the SVE ISA extension",
		     fndecl);
      else
	fatal_error (input_location, "calls to functions of type %qT require"
		     " the SVE ISA extension", fntype);
    }
}

static void
aarch64_function_arg_advance (cumulative_args_t pcum_v,
			      const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  if (pcum->pcs_variant == ARM_PCS_AAPCS64
      || pcum->pcs_variant == ARM_PCS_SIMD
      || pcum->pcs_variant == ARM_PCS_SVE)
    {
      aarch64_layout_arg (pcum_v, arg);
      gcc_assert ((pcum->aapcs_reg != NULL_RTX)
		  != (pcum->aapcs_stack_words != 0));
      if (pcum->aapcs_reg
	  && aarch64_call_switches_pstate_sm (pcum->isa_mode))
	aarch64_record_sme_mode_switch_args (pcum);

      pcum->aapcs_arg_processed = false;
      pcum->aapcs_ncrn = pcum->aapcs_nextncrn;
      pcum->aapcs_nvrn = pcum->aapcs_nextnvrn;
      pcum->aapcs_nprn = pcum->aapcs_nextnprn;
      pcum->aapcs_stack_size += pcum->aapcs_stack_words;
      pcum->aapcs_stack_words = 0;
      pcum->aapcs_reg = NULL_RTX;
    }
}

bool
aarch64_function_arg_regno_p (unsigned regno)
{
  return ((GP_REGNUM_P (regno) && regno < R0_REGNUM + NUM_ARG_REGS)
	  || (FP_REGNUM_P (regno) && regno < V0_REGNUM + NUM_FP_ARG_REGS)
	  || (PR_REGNUM_P (regno) && regno < P0_REGNUM + NUM_PR_ARG_REGS));
}

/* Implement FUNCTION_ARG_BOUNDARY.  Every parameter gets at least
   PARM_BOUNDARY bits of alignment, but will be given anything up
   to STACK_BOUNDARY bits if the type requires it.  This makes sure
   that both before and after the layout of each argument, the Next
   Stacked Argument Address (NSAA) will have a minimum alignment of
   8 bytes.  */

static unsigned int
aarch64_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int abi_break_gcc_9;
  unsigned int abi_break_gcc_13;
  unsigned int abi_break_gcc_14;
  unsigned int alignment = aarch64_function_arg_alignment (mode, type,
							   &abi_break_gcc_9,
							   &abi_break_gcc_13,
							   &abi_break_gcc_14);
  /* We rely on aarch64_layout_arg and aarch64_gimplify_va_arg_expr
     to emit warnings about ABI incompatibility.  */
  alignment = MIN (MAX (alignment, PARM_BOUNDARY), STACK_BOUNDARY);
  return alignment;
}

/* Implement TARGET_GET_RAW_RESULT_MODE and TARGET_GET_RAW_ARG_MODE.  */

static fixed_size_mode
aarch64_get_reg_raw_mode (int regno)
{
  /* Don't use any non GP registers for __builtin_apply and
     __builtin_return if general registers only mode is requested. */
  if (TARGET_GENERAL_REGS_ONLY && !GP_REGNUM_P (regno))
    return as_a <fixed_size_mode> (VOIDmode);
  if (TARGET_SVE && FP_REGNUM_P (regno))
    /* Don't use the SVE part of the register for __builtin_apply and
       __builtin_return.  The SVE registers aren't used by the normal PCS,
       so using them there would be a waste of time.  The PCS extensions
       for SVE types are fundamentally incompatible with the
       __builtin_return/__builtin_apply interface.  */
    return as_a <fixed_size_mode> (V16QImode);
  if (PR_REGNUM_P (regno))
    /* For SVE PR regs, indicate that they should be ignored for
       __builtin_apply/__builtin_return.  */
    return as_a <fixed_size_mode> (VOIDmode);
  return default_get_reg_raw_mode (regno);
}

/* Implement TARGET_FUNCTION_ARG_PADDING.

   Small aggregate types are placed in the lowest memory address.

   The related parameter passing rules are B.4, C.3, C.5 and C.14.  */

static pad_direction
aarch64_function_arg_padding (machine_mode mode, const_tree type)
{
  /* On little-endian targets, the least significant byte of every stack
     argument is passed at the lowest byte address of the stack slot.  */
  if (!BYTES_BIG_ENDIAN)
    return PAD_UPWARD;

  /* Otherwise, integral, floating-point and pointer types are padded downward:
     the least significant byte of a stack argument is passed at the highest
     byte address of the stack slot.  */
  if (type
      ? (INTEGRAL_TYPE_P (type) || SCALAR_FLOAT_TYPE_P (type)
	 || POINTER_TYPE_P (type))
      : (SCALAR_INT_MODE_P (mode) || SCALAR_FLOAT_MODE_P (mode)))
    return PAD_DOWNWARD;

  /* Everything else padded upward, i.e. data in first byte of stack slot.  */
  return PAD_UPWARD;
}

/* Similarly, for use by BLOCK_REG_PADDING (MODE, TYPE, FIRST).

   It specifies padding for the last (may also be the only)
   element of a block move between registers and memory.  If
   assuming the block is in the memory, padding upward means that
   the last element is padded after its highest significant byte,
   while in downward padding, the last element is padded at the
   its least significant byte side.

   Small aggregates and small complex types are always padded
   upwards.

   We don't need to worry about homogeneous floating-point or
   short-vector aggregates; their move is not affected by the
   padding direction determined here.  Regardless of endianness,
   each element of such an aggregate is put in the least
   significant bits of a fp/simd register.

   Return !BYTES_BIG_ENDIAN if the least significant byte of the
   register has useful data, and return the opposite if the most
   significant byte does.  */

bool
aarch64_pad_reg_upward (machine_mode mode, const_tree type,
		     bool first ATTRIBUTE_UNUSED)
{

  /* Aside from pure scalable types, small composite types are always
     padded upward.  */
  if (BYTES_BIG_ENDIAN && aarch64_composite_type_p (type, mode))
    {
      HOST_WIDE_INT size;
      if (type)
	size = int_size_in_bytes (type);
      else
	/* No frontends can create types with variable-sized modes, so we
	   shouldn't be asked to pass or return them.  */
	size = GET_MODE_SIZE (mode).to_constant ();
      if (size < 2 * UNITS_PER_WORD)
	{
	  pure_scalable_type_info pst_info;
	  if (pst_info.analyze_registers (type))
	    return false;
	  return true;
	}
    }

  /* Otherwise, use the default padding.  */
  return !BYTES_BIG_ENDIAN;
}

static scalar_int_mode
aarch64_libgcc_cmp_return_mode (void)
{
  return SImode;
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

/* We use the 12-bit shifted immediate arithmetic instructions so values
   must be multiple of (1 << 12), i.e. 4096.  */
#define ARITH_FACTOR 4096

#if (PROBE_INTERVAL % ARITH_FACTOR) != 0
#error Cannot use simple address calculation for stack probing
#endif

/* Emit code to probe a range of stack addresses from FIRST to FIRST+POLY_SIZE,
   inclusive.  These are offsets from the current stack pointer.  */

static void
aarch64_emit_probe_stack_range (HOST_WIDE_INT first, poly_int64 poly_size)
{
  HOST_WIDE_INT size;
  if (!poly_size.is_constant (&size))
    {
      sorry ("stack probes for SVE frames");
      return;
    }

  rtx reg1 = gen_rtx_REG (Pmode, PROBE_STACK_FIRST_REGNUM);

  /* See the same assertion on PROBE_INTERVAL above.  */
  gcc_assert ((first % ARITH_FACTOR) == 0);

  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  */
  if (size <= PROBE_INTERVAL)
    {
      const HOST_WIDE_INT base = ROUND_UP (size, ARITH_FACTOR);

      emit_set_insn (reg1,
		     plus_constant (Pmode,
				    stack_pointer_rtx, -(first + base)));
      emit_stack_probe (plus_constant (Pmode, reg1, base - size));
    }

  /* The run-time loop is made up of 8 insns in the generic case while the
     compile-time loop is made up of 4+2*(n-2) insns for n # of intervals.  */
  else if (size <= 4 * PROBE_INTERVAL)
    {
      HOST_WIDE_INT i, rem;

      emit_set_insn (reg1,
		     plus_constant (Pmode,
				    stack_pointer_rtx,
				    -(first + PROBE_INTERVAL)));
      emit_stack_probe (reg1);

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 2 until
	 it exceeds SIZE.  If only two probes are needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = 2 * PROBE_INTERVAL; i < size; i += PROBE_INTERVAL)
	{
	  emit_set_insn (reg1,
			 plus_constant (Pmode, reg1, -PROBE_INTERVAL));
	  emit_stack_probe (reg1);
	}

      rem = size - (i - PROBE_INTERVAL);
      if (rem > 256)
	{
	  const HOST_WIDE_INT base = ROUND_UP (rem, ARITH_FACTOR);

	  emit_set_insn (reg1, plus_constant (Pmode, reg1, -base));
	  emit_stack_probe (plus_constant (Pmode, reg1, base - rem));
	}
      else
	emit_stack_probe (plus_constant (Pmode, reg1, -rem));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      rtx reg2 = gen_rtx_REG (Pmode, PROBE_STACK_SECOND_REGNUM);

      /* Step 1: round SIZE to the previous multiple of the interval.  */

      HOST_WIDE_INT rounded_size = size & -PROBE_INTERVAL;


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      emit_set_insn (reg1,
		     plus_constant (Pmode, stack_pointer_rtx, -first));

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      HOST_WIDE_INT adjustment = - (first + rounded_size);
      if (! aarch64_uimm12_shift (adjustment))
	{
	  aarch64_internal_mov_immediate (reg2, GEN_INT (adjustment),
					  true, Pmode);
	  emit_set_insn (reg2, gen_rtx_PLUS (Pmode, stack_pointer_rtx, reg2));
	}
      else
	emit_set_insn (reg2,
		       plus_constant (Pmode, stack_pointer_rtx, adjustment));

      /* Step 3: the loop

	 do
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }
	 while (TEST_ADDR != LAST_ADDR)

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      emit_insn (gen_probe_stack_range (reg1, reg1, reg2));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	{
	  HOST_WIDE_INT rem = size - rounded_size;

	  if (rem > 256)
	    {
	      const HOST_WIDE_INT base = ROUND_UP (rem, ARITH_FACTOR);

	      emit_set_insn (reg2, plus_constant (Pmode, reg2, -base));
	      emit_stack_probe (plus_constant (Pmode, reg2, base - rem));
	    }
	  else
	    emit_stack_probe (plus_constant (Pmode, reg2, -rem));
	}
    }

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   absolute addresses.  */

const char *
aarch64_output_probe_stack_range (rtx reg1, rtx reg2)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  HOST_WIDE_INT stack_clash_probe_interval
    = 1 << param_stack_clash_protection_guard_size;

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg1;
  HOST_WIDE_INT interval;
  if (flag_stack_clash_protection)
    interval = stack_clash_probe_interval;
  else
    interval = PROBE_INTERVAL;

  gcc_assert (aarch64_uimm12_shift (interval));
  xops[1] = GEN_INT (interval);

  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* If doing stack clash protection then we probe up by the ABI specified
     amount.  We do this because we're dropping full pages at a time in the
     loop.  But if we're doing non-stack clash probing, probe at SP 0.  */
  if (flag_stack_clash_protection)
    xops[1] = GEN_INT (STACK_CLASH_CALLER_GUARD);
  else
    xops[1] = CONST0_RTX (GET_MODE (xops[1]));

  /* Probe at TEST_ADDR.  If we're inside the loop it is always safe to probe
     by this amount for each iteration.  */
  output_asm_insn ("str\txzr, [%0, %1]", xops);

  /* Test if TEST_ADDR == LAST_ADDR.  */
  xops[1] = reg2;
  output_asm_insn ("cmp\t%0, %1", xops);

  /* Branch.  */
  fputs ("\tb.ne\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* Emit the probe loop for doing stack clash probes and stack adjustments for
   SVE.  This emits probes from BASE to BASE - ADJUSTMENT based on a guard size
   of GUARD_SIZE.  When a probe is emitted it is done at most
   MIN_PROBE_THRESHOLD bytes from the current BASE at an interval of
   at most MIN_PROBE_THRESHOLD.  By the end of this function
   BASE = BASE - ADJUSTMENT.  */

const char *
aarch64_output_probe_sve_stack_clash (rtx base, rtx adjustment,
				      rtx min_probe_threshold, rtx guard_size)
{
  /* This function is not allowed to use any instruction generation function
     like gen_ and friends.  If you do you'll likely ICE during CFG validation,
     so instead emit the code you want using output_asm_insn.  */
  gcc_assert (flag_stack_clash_protection);
  gcc_assert (CONST_INT_P (min_probe_threshold) && CONST_INT_P (guard_size));
  gcc_assert (INTVAL (guard_size) > INTVAL (min_probe_threshold));

  /* The minimum required allocation before the residual requires probing.  */
  HOST_WIDE_INT residual_probe_guard = INTVAL (min_probe_threshold);

  /* Clamp the value down to the nearest value that can be used with a cmp.  */
  residual_probe_guard = aarch64_clamp_to_uimm12_shift (residual_probe_guard);
  rtx probe_offset_value_rtx = gen_int_mode (residual_probe_guard, Pmode);

  gcc_assert (INTVAL (min_probe_threshold) >= residual_probe_guard);
  gcc_assert (aarch64_uimm12_shift (residual_probe_guard));

  static int labelno = 0;
  char loop_start_lab[32];
  char loop_end_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_start_lab, "SVLPSPL", labelno);
  ASM_GENERATE_INTERNAL_LABEL (loop_end_lab, "SVLPEND", labelno++);

  /* Emit loop start label.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_start_lab);

  /* ADJUSTMENT < RESIDUAL_PROBE_GUARD.  */
  xops[0] = adjustment;
  xops[1] = probe_offset_value_rtx;
  output_asm_insn ("cmp\t%0, %1", xops);

  /* Branch to end if not enough adjustment to probe.  */
  fputs ("\tb.lt\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_end_lab);
  fputc ('\n', asm_out_file);

  /* BASE = BASE - RESIDUAL_PROBE_GUARD.  */
  xops[0] = base;
  xops[1] = probe_offset_value_rtx;
  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* Probe at BASE.  */
  xops[1] = const0_rtx;
  output_asm_insn ("str\txzr, [%0, %1]", xops);

  /* ADJUSTMENT = ADJUSTMENT - RESIDUAL_PROBE_GUARD.  */
  xops[0] = adjustment;
  xops[1] = probe_offset_value_rtx;
  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* Branch to start if still more bytes to allocate.  */
  fputs ("\tb\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_start_lab);
  fputc ('\n', asm_out_file);

  /* No probe leave.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_end_lab);

  /* BASE = BASE - ADJUSTMENT.  */
  xops[0] = base;
  xops[1] = adjustment;
  output_asm_insn ("sub\t%0, %0, %1", xops);
  return "";
}

/* Determine whether a frame chain needs to be generated.  */
static bool
aarch64_needs_frame_chain (void)
{
  if (frame_pointer_needed)
    return true;

  /* A leaf function cannot have calls or write LR.  */
  bool is_leaf = crtl->is_leaf && !df_regs_ever_live_p (LR_REGNUM);

  /* Don't use a frame chain in leaf functions if leaf frame pointers
     are disabled.  */
  if (flag_omit_leaf_frame_pointer && is_leaf)
    return false;

  return aarch64_use_frame_pointer;
}

/* Return true if the current function should save registers above
   the locals area, rather than below it.  */

static bool
aarch64_save_regs_above_locals_p ()
{
  /* When using stack smash protection, make sure that the canary slot
     comes between the locals and the saved registers.  Otherwise,
     it would be possible for a carefully sized smash attack to change
     the saved registers (particularly LR and FP) without reaching the
     canary.  */
  return crtl->stack_protect_guard;
}

/* Return true if the current function needs to record the incoming
   value of PSTATE.SM.  */
static bool
aarch64_need_old_pstate_sm ()
{
  /* Exit early if the incoming value of PSTATE.SM is known at
     compile time.  */
  if (aarch64_cfun_incoming_pstate_sm () != 0)
    return false;

  if (aarch64_cfun_enables_pstate_sm ())
    return true;

  /* Non-local goto receivers are entered with PSTATE.SM equal to 0,
     but the function needs to return with PSTATE.SM unchanged.  */
  if (nonlocal_goto_handler_labels)
    return true;

  /* Likewise for exception handlers.  */
  eh_landing_pad lp;
  for (unsigned int i = 1; vec_safe_iterate (cfun->eh->lp_array, i, &lp); ++i)
    if (lp && lp->post_landing_pad)
      return true;

  /* Non-local gotos need to set PSTATE.SM to zero.  It's possible to call
     streaming-compatible functions without SME being available, so PSTATE.SM
     should only be changed if it is currently set to one.  */
  if (crtl->has_nonlocal_goto)
    return true;

  if (cfun->machine->call_switches_pstate_sm)
    for (auto insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (auto *call = dyn_cast<rtx_call_insn *> (insn))
	if (!SIBLING_CALL_P (call))
	  {
	    /* Return true if there is a call to a non-streaming-compatible
	       function.  */
	    auto callee_isa_mode = aarch64_insn_callee_isa_mode (call);
	    if (aarch64_call_switches_pstate_sm (callee_isa_mode))
	      return true;
	  }
  return false;
}

/* Mark the registers that need to be saved by the callee and calculate
   the size of the callee-saved registers area and frame record (both FP
   and LR may be omitted).  */
static void
aarch64_layout_frame (void)
{
  unsigned regno, last_fp_reg = INVALID_REGNUM;
  machine_mode vector_save_mode = aarch64_reg_save_mode (V8_REGNUM);
  poly_int64 vector_save_size = GET_MODE_SIZE (vector_save_mode);
  bool frame_related_fp_reg_p = false;
  aarch64_frame &frame = cfun->machine->frame;
  poly_int64 top_of_locals = -1;
  bool enables_pstate_sm = aarch64_cfun_enables_pstate_sm ();

  vec_safe_truncate (frame.saved_gprs, 0);
  vec_safe_truncate (frame.saved_fprs, 0);
  vec_safe_truncate (frame.saved_prs, 0);

  frame.emit_frame_chain = aarch64_needs_frame_chain ();

  /* Adjust the outgoing arguments size if required.  Keep it in sync with what
     the mid-end is doing.  */
  crtl->outgoing_args_size = STACK_DYNAMIC_OFFSET (cfun);

#define SLOT_NOT_REQUIRED (-2)
#define SLOT_REQUIRED     (-1)

  frame.wb_push_candidate1 = INVALID_REGNUM;
  frame.wb_push_candidate2 = INVALID_REGNUM;
  frame.spare_pred_reg = INVALID_REGNUM;

  /* First mark all the registers that really need to be saved...  */
  for (regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    frame.reg_offset[regno] = SLOT_NOT_REQUIRED;
  frame.old_svcr_offset = SLOT_NOT_REQUIRED;

  /* ... that includes the eh data registers (if needed)...  */
  if (crtl->calls_eh_return)
    for (regno = 0; EH_RETURN_DATA_REGNO (regno) != INVALID_REGNUM; regno++)
      frame.reg_offset[EH_RETURN_DATA_REGNO (regno)] = SLOT_REQUIRED;

  /* ... and any callee saved register that dataflow says is live.  */
  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    if (df_regs_ever_live_p (regno)
	&& !fixed_regs[regno]
	&& (regno == R30_REGNUM
	    || !crtl->abi->clobbers_full_reg_p (regno)))
      frame.reg_offset[regno] = SLOT_REQUIRED;

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    if ((enables_pstate_sm || df_regs_ever_live_p (regno))
	&& !fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno))
      {
	frame.reg_offset[regno] = SLOT_REQUIRED;
	last_fp_reg = regno;
	if (aarch64_emit_cfi_for_reg_p (regno))
	  frame_related_fp_reg_p = true;
      }

  /* Big-endian SVE frames need a spare predicate register in order
     to save Z8-Z15.  Decide which register they should use.  Prefer
     an unused argument register if possible, so that we don't force P4
     to be saved unnecessarily.  */
  if (frame_related_fp_reg_p
      && crtl->abi->id () == ARM_PCS_SVE
      && BYTES_BIG_ENDIAN)
    {
      bitmap live1 = df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun));
      bitmap live2 = df_get_live_in (EXIT_BLOCK_PTR_FOR_FN (cfun));
      for (regno = P0_REGNUM; regno <= P7_REGNUM; regno++)
	if (!bitmap_bit_p (live1, regno) && !bitmap_bit_p (live2, regno))
	  break;
      gcc_assert (regno <= P7_REGNUM);
      frame.spare_pred_reg = regno;
      df_set_regs_ever_live (regno, true);
    }

  for (regno = P0_REGNUM; regno <= P15_REGNUM; regno++)
    if ((enables_pstate_sm || df_regs_ever_live_p (regno))
	&& !fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno))
      frame.reg_offset[regno] = SLOT_REQUIRED;

  bool regs_at_top_p = aarch64_save_regs_above_locals_p ();

  poly_int64 offset = crtl->outgoing_args_size;
  gcc_assert (multiple_p (offset, STACK_BOUNDARY / BITS_PER_UNIT));
  if (regs_at_top_p)
    {
      offset += get_frame_size ();
      offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);
      top_of_locals = offset;
    }
  frame.bytes_below_saved_regs = offset;
  frame.sve_save_and_probe = INVALID_REGNUM;

  /* Now assign stack slots for the registers.  Start with the predicate
     registers, since predicate LDR and STR have a relatively small
     offset range.  These saves happen below the hard frame pointer.  */
  for (regno = P0_REGNUM; regno <= P15_REGNUM; regno++)
    if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
      {
	vec_safe_push (frame.saved_prs, regno);
	if (frame.sve_save_and_probe == INVALID_REGNUM)
	  frame.sve_save_and_probe = regno;
	frame.reg_offset[regno] = offset;
	offset += BYTES_PER_SVE_PRED;
      }

  poly_int64 saved_prs_size = offset - frame.bytes_below_saved_regs;
  if (maybe_ne (saved_prs_size, 0))
    {
      /* If we have any vector registers to save above the predicate registers,
	 the offset of the vector register save slots need to be a multiple
	 of the vector size.  This lets us use the immediate forms of LDR/STR
	 (or LD1/ST1 for big-endian).

	 A vector register is 8 times the size of a predicate register,
	 and we need to save a maximum of 12 predicate registers, so the
	 first vector register will be at either #1, MUL VL or #2, MUL VL.

	 If we don't have any vector registers to save, and we know how
	 big the predicate save area is, we can just round it up to the
	 next 16-byte boundary.  */
      if (last_fp_reg == INVALID_REGNUM && offset.is_constant ())
	offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);
      else
	{
	  if (known_le (saved_prs_size, vector_save_size))
	    offset = frame.bytes_below_saved_regs + vector_save_size;
	  else if (known_le (saved_prs_size, vector_save_size * 2))
	    offset = frame.bytes_below_saved_regs + vector_save_size * 2;
	  else
	    gcc_unreachable ();
	}
    }

  /* If we need to save any SVE vector registers, add them next.  */
  if (last_fp_reg != INVALID_REGNUM && crtl->abi->id () == ARM_PCS_SVE)
    for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
      if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
	{
	  vec_safe_push (frame.saved_fprs, regno);
	  if (frame.sve_save_and_probe == INVALID_REGNUM)
	    frame.sve_save_and_probe = regno;
	  frame.reg_offset[regno] = offset;
	  offset += vector_save_size;
	}

  /* OFFSET is now the offset of the hard frame pointer from the bottom
     of the callee save area.  */
  auto below_hard_fp_saved_regs_size = offset - frame.bytes_below_saved_regs;
  bool saves_below_hard_fp_p = maybe_ne (below_hard_fp_saved_regs_size, 0);
  gcc_assert (!saves_below_hard_fp_p
	      || (frame.sve_save_and_probe != INVALID_REGNUM
		  && known_eq (frame.reg_offset[frame.sve_save_and_probe],
			       frame.bytes_below_saved_regs)));

  frame.bytes_below_hard_fp = offset;
  frame.hard_fp_save_and_probe = INVALID_REGNUM;

  auto allocate_gpr_slot = [&](unsigned int regno)
    {
      vec_safe_push (frame.saved_gprs, regno);
      frame.reg_offset[regno] = offset;
      offset += UNITS_PER_WORD;
    };

  if (frame.emit_frame_chain)
    {
      /* FP and LR are placed in the linkage record.  */
      allocate_gpr_slot (R29_REGNUM);
      allocate_gpr_slot (R30_REGNUM);
    }
  else if ((flag_stack_clash_protection || !frame.is_scs_enabled)
	   && known_eq (frame.reg_offset[R30_REGNUM], SLOT_REQUIRED))
    /* Put the LR save slot first, since it makes a good choice of probe
       for stack clash purposes.  The idea is that the link register usually
       has to be saved before a call anyway, and so we lose little by
       stopping it from being individually shrink-wrapped.  */
    allocate_gpr_slot (R30_REGNUM);

  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
      allocate_gpr_slot (regno);

  if (aarch64_need_old_pstate_sm ())
    {
      frame.old_svcr_offset = offset;
      offset += UNITS_PER_WORD;
    }

  /* If the current function changes the SVE vector length, ensure that the
     old value of the DWARF VG register is saved and available in the CFI,
     so that outer frames with VL-sized offsets can be processed correctly.  */
  if (cfun->machine->call_switches_pstate_sm
      || aarch64_cfun_enables_pstate_sm ())
    {
      frame.reg_offset[VG_REGNUM] = offset;
      offset += UNITS_PER_WORD;
    }

  poly_int64 max_int_offset = offset;
  offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);
  bool has_align_gap = maybe_ne (offset, max_int_offset);

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
      {
	vec_safe_push (frame.saved_fprs, regno);
	/* If there is an alignment gap between integer and fp callee-saves,
	   allocate the last fp register to it if possible.  */
	if (regno == last_fp_reg
	    && has_align_gap
	    && known_eq (vector_save_size, 8)
	    && multiple_p (offset, 16))
	  {
	    frame.reg_offset[regno] = max_int_offset;
	    break;
	  }

	frame.reg_offset[regno] = offset;
	offset += vector_save_size;
      }

  offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);
  auto saved_regs_size = offset - frame.bytes_below_saved_regs;

  array_slice<unsigned int> push_regs = (!vec_safe_is_empty (frame.saved_gprs)
					 ? frame.saved_gprs
					 : frame.saved_fprs);
  if (!push_regs.empty ()
      && known_eq (frame.reg_offset[push_regs[0]], frame.bytes_below_hard_fp))
    {
      frame.hard_fp_save_and_probe = push_regs[0];
      frame.wb_push_candidate1 = push_regs[0];
      if (push_regs.size () > 1)
	frame.wb_push_candidate2 = push_regs[1];
    }

  /* With stack-clash, a register must be saved in non-leaf functions.
     The saving of the bottommost register counts as an implicit probe,
     which allows us to maintain the invariant described in the comment
     at expand_prologue.  */
  gcc_assert (crtl->is_leaf || maybe_ne (saved_regs_size, 0));

  if (!regs_at_top_p)
    {
      offset += get_frame_size ();
      offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);
      top_of_locals = offset;
    }
  offset += frame.saved_varargs_size;
  gcc_assert (multiple_p (offset, STACK_BOUNDARY / BITS_PER_UNIT));
  frame.frame_size = offset;

  frame.bytes_above_hard_fp = frame.frame_size - frame.bytes_below_hard_fp;
  gcc_assert (known_ge (top_of_locals, 0));
  frame.bytes_above_locals = frame.frame_size - top_of_locals;

  frame.initial_adjust = 0;
  frame.final_adjust = 0;
  frame.callee_adjust = 0;
  frame.sve_callee_adjust = 0;

  frame.wb_pop_candidate1 = frame.wb_push_candidate1;
  frame.wb_pop_candidate2 = frame.wb_push_candidate2;

  /* Shadow call stack only deals with functions where the LR is pushed
     onto the stack and without specifying the "no_sanitize" attribute
     with the argument "shadow-call-stack".  */
  frame.is_scs_enabled
    = (!crtl->calls_eh_return
       && sanitize_flags_p (SANITIZE_SHADOW_CALL_STACK)
       && known_ge (frame.reg_offset[LR_REGNUM], 0));

  /* When shadow call stack is enabled, the scs_pop in the epilogue will
     restore x30, and we don't need to pop x30 again in the traditional
     way.  Pop candidates record the registers that need to be popped
     eventually.  */
  if (frame.is_scs_enabled)
    {
      if (frame.wb_pop_candidate2 == R30_REGNUM)
	frame.wb_pop_candidate2 = INVALID_REGNUM;
      else if (frame.wb_pop_candidate1 == R30_REGNUM)
	frame.wb_pop_candidate1 = INVALID_REGNUM;
    }

  /* If candidate2 is INVALID_REGNUM, we need to adjust max_push_offset to
     256 to ensure that the offset meets the requirements of emit_move_insn.
     Similarly, if candidate1 is INVALID_REGNUM, we need to set
     max_push_offset to 0, because no registers are popped at this time,
     so callee_adjust cannot be adjusted.  */
  HOST_WIDE_INT max_push_offset = 0;
  if (frame.wb_pop_candidate1 != INVALID_REGNUM)
    {
      if (frame.wb_pop_candidate2 != INVALID_REGNUM)
	max_push_offset = 512;
      else
	max_push_offset = 256;
    }

  HOST_WIDE_INT const_size, const_below_saved_regs, const_above_fp;
  HOST_WIDE_INT const_saved_regs_size;
  if (known_eq (saved_regs_size, 0))
    frame.initial_adjust = frame.frame_size;
  else if (frame.frame_size.is_constant (&const_size)
	   && const_size < max_push_offset
	   && known_eq (frame.bytes_above_hard_fp, const_size))
    {
      /* Simple, small frame with no data below the saved registers.

	 stp reg1, reg2, [sp, -frame_size]!
	 stp reg3, reg4, [sp, 16]  */
      frame.callee_adjust = const_size;
    }
  else if (frame.bytes_below_saved_regs.is_constant (&const_below_saved_regs)
	   && saved_regs_size.is_constant (&const_saved_regs_size)
	   && const_below_saved_regs + const_saved_regs_size < 512
	   /* We could handle this case even with data below the saved
	      registers, provided that that data left us with valid offsets
	      for all predicate and vector save slots.  It's such a rare
	      case that it hardly seems worth the effort though.  */
	   && (!saves_below_hard_fp_p || const_below_saved_regs == 0)
	   && !(cfun->calls_alloca
		&& frame.bytes_above_hard_fp.is_constant (&const_above_fp)
		&& const_above_fp < max_push_offset))
    {
      /* Frame with small area below the saved registers:

	 sub sp, sp, frame_size
	 stp reg1, reg2, [sp, bytes_below_saved_regs]
	 stp reg3, reg4, [sp, bytes_below_saved_regs + 16]  */
      frame.initial_adjust = frame.frame_size;
    }
  else if (saves_below_hard_fp_p
	   && known_eq (saved_regs_size, below_hard_fp_saved_regs_size))
    {
      /* Frame in which all saves are SVE saves:

	 sub sp, sp, frame_size - bytes_below_saved_regs
	 save SVE registers relative to SP
	 sub sp, sp, bytes_below_saved_regs  */
      frame.initial_adjust = frame.frame_size - frame.bytes_below_saved_regs;
      frame.final_adjust = frame.bytes_below_saved_regs;
    }
  else if (frame.wb_push_candidate1 != INVALID_REGNUM
	   && frame.bytes_above_hard_fp.is_constant (&const_above_fp)
	   && const_above_fp < max_push_offset)
    {
      /* Frame with large area below the saved registers, or with SVE saves,
	 but with a small area above:

	 stp reg1, reg2, [sp, -hard_fp_offset]!
	 stp reg3, reg4, [sp, 16]
	 [sub sp, sp, below_hard_fp_saved_regs_size]
	 [save SVE registers relative to SP]
	 sub sp, sp, bytes_below_saved_regs  */
      frame.callee_adjust = const_above_fp;
      frame.sve_callee_adjust = below_hard_fp_saved_regs_size;
      frame.final_adjust = frame.bytes_below_saved_regs;
    }
  else
    {
      /* General case:

	 sub sp, sp, hard_fp_offset
	 stp x29, x30, [sp, 0]
	 add x29, sp, 0
	 stp reg3, reg4, [sp, 16]
	 [sub sp, sp, below_hard_fp_saved_regs_size]
	 [save SVE registers relative to SP]
	 sub sp, sp, bytes_below_saved_regs  */
      frame.initial_adjust = frame.bytes_above_hard_fp;
      frame.sve_callee_adjust = below_hard_fp_saved_regs_size;
      frame.final_adjust = frame.bytes_below_saved_regs;
    }

  /* The frame is allocated in pieces, with each non-final piece
     including a register save at offset 0 that acts as a probe for
     the following piece.  In addition, the save of the bottommost register
     acts as a probe for callees and allocas.  Roll back any probes that
     aren't needed.

     A probe isn't needed if it is associated with the final allocation
     (including callees and allocas) that happens before the epilogue is
     executed.  */
  if (crtl->is_leaf
      && !cfun->calls_alloca
      && known_eq (frame.final_adjust, 0))
    {
      if (maybe_ne (frame.sve_callee_adjust, 0))
	frame.sve_save_and_probe = INVALID_REGNUM;
      else
	frame.hard_fp_save_and_probe = INVALID_REGNUM;
    }

  /* Make sure the individual adjustments add up to the full frame size.  */
  gcc_assert (known_eq (frame.initial_adjust
			+ frame.callee_adjust
			+ frame.sve_callee_adjust
			+ frame.final_adjust, frame.frame_size));

  if (frame.callee_adjust == 0)
    {
      /* We've decided not to do a "real" push and pop.  However,
	 setting up the frame chain is treated as being essentially
	 a multi-instruction push.  */
      frame.wb_pop_candidate1 = frame.wb_pop_candidate2 = INVALID_REGNUM;
      if (!frame.emit_frame_chain)
	frame.wb_push_candidate1 = frame.wb_push_candidate2 = INVALID_REGNUM;
    }

  frame.laid_out = true;
}

/* Return true if the register REGNO is saved on entry to
   the current function.  */

static bool
aarch64_register_saved_on_entry (int regno)
{
  return known_ge (cfun->machine->frame.reg_offset[regno], 0);
}

/* Push the register number REGNO of mode MODE to the stack with write-back
   adjusting the stack by ADJUSTMENT.  */

static void
aarch64_pushwb_single_reg (machine_mode mode, unsigned regno,
			   HOST_WIDE_INT adjustment)
 {
  rtx base_rtx = stack_pointer_rtx;
  rtx insn, reg, mem;

  reg = gen_rtx_REG (mode, regno);
  mem = gen_rtx_PRE_MODIFY (Pmode, base_rtx,
			    plus_constant (Pmode, base_rtx, -adjustment));
  mem = gen_frame_mem (mode, mem);

  insn = emit_move_insn (mem, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Generate and return an instruction to store the pair of registers
   REG and REG2 of mode MODE to location BASE with write-back adjusting
   the stack location BASE by ADJUSTMENT.  */

static rtx
aarch64_gen_storewb_pair (machine_mode mode, rtx base, rtx reg, rtx reg2,
			  HOST_WIDE_INT adjustment)
{
  rtx new_base = plus_constant (Pmode, base, -adjustment);
  rtx mem = gen_frame_mem (mode, new_base);
  rtx mem2 = adjust_address_nv (mem, mode, GET_MODE_SIZE (mode));

  return gen_rtx_PARALLEL (VOIDmode,
			   gen_rtvec (3,
				      gen_rtx_SET (base, new_base),
				      gen_rtx_SET (mem, reg),
				      gen_rtx_SET (mem2, reg2)));
}

/* Push registers numbered REGNO1 and REGNO2 to the stack, adjusting the
   stack pointer by ADJUSTMENT.  */

static void
aarch64_push_regs (unsigned regno1, unsigned regno2, HOST_WIDE_INT adjustment)
{
  rtx_insn *insn;
  machine_mode mode = aarch64_reg_save_mode (regno1);

  if (regno2 == INVALID_REGNUM)
    return aarch64_pushwb_single_reg (mode, regno1, adjustment);

  rtx reg1 = gen_rtx_REG (mode, regno1);
  rtx reg2 = gen_rtx_REG (mode, regno2);

  insn = emit_insn (aarch64_gen_storewb_pair (mode, stack_pointer_rtx, reg1,
					      reg2, adjustment));
  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 2)) = 1;
  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 1)) = 1;
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Load the pair of register REG, REG2 of mode MODE from stack location BASE,
   adjusting it by ADJUSTMENT afterwards.  */

static rtx
aarch64_gen_loadwb_pair (machine_mode mode, rtx base, rtx reg, rtx reg2,
			 HOST_WIDE_INT adjustment)
{
  rtx mem = gen_frame_mem (mode, base);
  rtx mem2 = adjust_address_nv (mem, mode, GET_MODE_SIZE (mode));
  rtx new_base = plus_constant (Pmode, base, adjustment);

  return gen_rtx_PARALLEL (VOIDmode,
			   gen_rtvec (3,
				      gen_rtx_SET (base, new_base),
				      gen_rtx_SET (reg, mem),
				      gen_rtx_SET (reg2, mem2)));
}

/* Pop the two registers numbered REGNO1, REGNO2 from the stack, adjusting it
   afterwards by ADJUSTMENT and writing the appropriate REG_CFA_RESTORE notes
   into CFI_OPS.  */

static void
aarch64_pop_regs (unsigned regno1, unsigned regno2, HOST_WIDE_INT adjustment,
		  rtx *cfi_ops)
{
  machine_mode mode = aarch64_reg_save_mode (regno1);
  rtx reg1 = gen_rtx_REG (mode, regno1);

  *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg1, *cfi_ops);

  if (regno2 == INVALID_REGNUM)
    {
      rtx mem = plus_constant (Pmode, stack_pointer_rtx, adjustment);
      mem = gen_rtx_POST_MODIFY (Pmode, stack_pointer_rtx, mem);
      emit_move_insn (reg1, gen_frame_mem (mode, mem));
    }
  else
    {
      rtx reg2 = gen_rtx_REG (mode, regno2);
      *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg2, *cfi_ops);
      emit_insn (aarch64_gen_loadwb_pair (mode, stack_pointer_rtx, reg1,
					  reg2, adjustment));
    }
}

/* Given an ldp/stp register operand mode MODE, return a suitable mode to use
   for a mem rtx representing the entire pair.  */

static machine_mode
aarch64_pair_mode_for_mode (machine_mode mode)
{
  if (known_eq (GET_MODE_SIZE (mode), 4))
    return V2x4QImode;
  else if (known_eq (GET_MODE_SIZE (mode), 8))
    return V2x8QImode;
  else if (known_eq (GET_MODE_SIZE (mode), 16))
    return V2x16QImode;
  else
    gcc_unreachable ();
}

/* Given a base mem MEM with mode and address suitable for a single ldp/stp
   operand, return an rtx like MEM which instead represents the entire pair.  */

static rtx
aarch64_pair_mem_from_base (rtx mem)
{
  auto pair_mode = aarch64_pair_mode_for_mode (GET_MODE (mem));
  mem = adjust_bitfield_address_nv (mem, pair_mode, 0);
  gcc_assert (aarch64_mem_pair_lanes_operand (mem, pair_mode));
  return mem;
}

/* Generate and return a store pair instruction to store REG1 and REG2
   into memory starting at BASE_MEM.  All three rtxes should have modes of the
   same size.  */

rtx
aarch64_gen_store_pair (rtx base_mem, rtx reg1, rtx reg2)
{
  rtx pair_mem = aarch64_pair_mem_from_base (base_mem);

  return gen_rtx_SET (pair_mem,
		      gen_rtx_UNSPEC (GET_MODE (pair_mem),
				      gen_rtvec (2, reg1, reg2),
				      UNSPEC_STP));
}

/* Generate and return a load pair instruction to load a pair of
   registers starting at BASE_MEM into REG1 and REG2.  If CODE is
   UNKNOWN, all three rtxes should have modes of the same size.
   Otherwise, CODE is {SIGN,ZERO}_EXTEND, base_mem should be in SImode,
   and REG{1,2} should be in DImode.  */

rtx
aarch64_gen_load_pair (rtx reg1, rtx reg2, rtx base_mem, enum rtx_code code)
{
  rtx pair_mem = aarch64_pair_mem_from_base (base_mem);

  const bool any_extend_p = (code == ZERO_EXTEND || code == SIGN_EXTEND);
  if (any_extend_p)
    gcc_checking_assert (GET_MODE (base_mem) == SImode
			 && GET_MODE (reg1) == DImode
			 && GET_MODE (reg2) == DImode);
  else
    gcc_assert (code == UNKNOWN);

  rtx unspecs[2] = {
    gen_rtx_UNSPEC (any_extend_p ? SImode : GET_MODE (reg1),
		    gen_rtvec (1, pair_mem),
		    UNSPEC_LDP_FST),
    gen_rtx_UNSPEC (any_extend_p ? SImode : GET_MODE (reg2),
		    gen_rtvec (1, copy_rtx (pair_mem)),
		    UNSPEC_LDP_SND)
  };

  if (any_extend_p)
    for (int i = 0; i < 2; i++)
      unspecs[i] = gen_rtx_fmt_e (code, DImode, unspecs[i]);

  return gen_rtx_PARALLEL (VOIDmode,
			   gen_rtvec (2,
				      gen_rtx_SET (reg1, unspecs[0]),
				      gen_rtx_SET (reg2, unspecs[1])));
}

/* Return TRUE if return address signing should be enabled for the current
   function, otherwise return FALSE.  */

bool
aarch64_return_address_signing_enabled (void)
{
  /* This function should only be called after frame laid out.   */
  gcc_assert (cfun->machine->frame.laid_out);

  /* If signing scope is AARCH_FUNCTION_NON_LEAF, we only sign a leaf function
     if its LR is pushed onto stack.  */
  return (aarch_ra_sign_scope == AARCH_FUNCTION_ALL
	  || (aarch_ra_sign_scope == AARCH_FUNCTION_NON_LEAF
	      && known_ge (cfun->machine->frame.reg_offset[LR_REGNUM], 0)));
}

/* Only used by the arm backend.  */
void aarch_bti_arch_check (void)
{}

/* Return TRUE if Branch Target Identification Mechanism is enabled.  */
bool
aarch_bti_enabled (void)
{
  return (aarch_enable_bti == 1);
}

/* Check if INSN is a BTI J insn.  */
bool
aarch_bti_j_insn_p (rtx_insn *insn)
{
  if (!insn || !INSN_P (insn))
    return false;

  rtx pat = PATTERN (insn);
  return GET_CODE (pat) == UNSPEC_VOLATILE && XINT (pat, 1) == UNSPECV_BTI_J;
}

/* Return TRUE if Guarded Control Stack is enabled.  */
bool
aarch64_gcs_enabled (void)
{
  return (aarch64_enable_gcs == 1);
}

/* Check if X (or any sub-rtx of X) is a PACIASP/PACIBSP instruction.  */
bool
aarch_pac_insn_p (rtx x)
{
  if (!INSN_P (x))
    return false;

  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, PATTERN (x), ALL)
    {
      rtx sub = *iter;
      if (sub && GET_CODE (sub) == UNSPEC)
	{
	  int unspec_val = XINT (sub, 1);
	  switch (unspec_val)
	    {
	    case UNSPEC_PACIASP:
            case UNSPEC_PACIBSP:
	      return true;

	    default:
	      return false;
	    }
	  iter.skip_subrtxes ();
	}
    }
  return false;
}

rtx aarch_gen_bti_c (void)
{
  return gen_bti_c ();
}

rtx aarch_gen_bti_j (void)
{
  return gen_bti_j ();
}

/* The caller is going to use ST1D or LD1D to save or restore an SVE
   register in mode MODE at BASE_RTX + OFFSET, where OFFSET is in
   the range [1, 16] * GET_MODE_SIZE (MODE).  Prepare for this by:

     (1) updating BASE_RTX + OFFSET so that it is a legitimate ST1D
	 or LD1D address

     (2) setting PRED to a valid predicate register for the ST1D or LD1D,
	 if the variable isn't already nonnull

   (1) is needed when OFFSET is in the range [8, 16] * GET_MODE_SIZE (MODE).
   Handle this case using a temporary base register that is suitable for
   all offsets in that range.  Use ANCHOR_REG as this base register if it
   is nonnull, otherwise create a new register and store it in ANCHOR_REG.  */

static inline void
aarch64_adjust_sve_callee_save_base (machine_mode mode, rtx &base_rtx,
				     rtx &anchor_reg, poly_int64 &offset,
				     rtx &ptrue)
{
  if (maybe_ge (offset, 8 * GET_MODE_SIZE (mode)))
    {
      /* This is the maximum valid offset of the anchor from the base.
	 Lower values would be valid too.  */
      poly_int64 anchor_offset = 16 * GET_MODE_SIZE (mode);
      if (!anchor_reg)
	{
	  anchor_reg = gen_rtx_REG (Pmode, STACK_CLASH_SVE_CFA_REGNUM);
	  emit_insn (gen_add3_insn (anchor_reg, base_rtx,
				    gen_int_mode (anchor_offset, Pmode)));
	}
      base_rtx = anchor_reg;
      offset -= anchor_offset;
    }
  if (!ptrue)
    {
      int pred_reg = cfun->machine->frame.spare_pred_reg;
      emit_move_insn (gen_rtx_REG (VNx16BImode, pred_reg),
		      CONSTM1_RTX (VNx16BImode));
      ptrue = gen_rtx_REG (VNx2BImode, pred_reg);
    }
}

/* Add a REG_CFA_EXPRESSION note to INSN to say that register REG
   is saved at BASE + OFFSET.  */

static void
aarch64_add_cfa_expression (rtx_insn *insn, rtx reg,
			    rtx base, poly_int64 offset)
{
  rtx mem = gen_frame_mem (GET_MODE (reg),
			   plus_constant (Pmode, base, offset));
  add_reg_note (insn, REG_CFA_EXPRESSION, gen_rtx_SET (mem, reg));
}

/* Emit code to save the callee-saved registers in REGS.  Skip any
   write-back candidates if SKIP_WB is true, otherwise consider only
   write-back candidates.

   The stack pointer is currently BYTES_BELOW_SP bytes above the bottom
   of the static frame.  HARD_FP_VALID_P is true if the hard frame pointer
   has been set up.  */

static void
aarch64_save_callee_saves (poly_int64 bytes_below_sp,
			   array_slice<unsigned int> regs, bool skip_wb,
			   bool hard_fp_valid_p)
{
  aarch64_frame &frame = cfun->machine->frame;
  rtx_insn *insn;
  rtx anchor_reg = NULL_RTX, ptrue = NULL_RTX;

  auto skip_save_p = [&](unsigned int regno)
    {
      if (cfun->machine->reg_is_wrapped_separately[regno])
	return true;

      if (skip_wb == (regno == frame.wb_push_candidate1
		      || regno == frame.wb_push_candidate2))
	return true;

      return false;
    };

  for (unsigned int i = 0; i < regs.size (); ++i)
    {
      unsigned int regno = regs[i];
      poly_int64 offset;
      bool frame_related_p = aarch64_emit_cfi_for_reg_p (regno);

      if (skip_save_p (regno))
	continue;

      machine_mode mode = aarch64_reg_save_mode (regno);
      rtx reg = gen_rtx_REG (mode, regno);
      rtx move_src = reg;
      offset = frame.reg_offset[regno] - bytes_below_sp;
      if (regno == VG_REGNUM)
	{
	  move_src = gen_rtx_REG (DImode, IP0_REGNUM);
	  emit_move_insn (move_src, gen_int_mode (aarch64_sve_vg, DImode));
	}
      rtx base_rtx = stack_pointer_rtx;
      poly_int64 sp_offset = offset;

      HOST_WIDE_INT const_offset;
      if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	aarch64_adjust_sve_callee_save_base (mode, base_rtx, anchor_reg,
					     offset, ptrue);
      else if (GP_REGNUM_P (REGNO (reg))
	       && (!offset.is_constant (&const_offset) || const_offset >= 512))
	{
	  poly_int64 fp_offset = frame.bytes_below_hard_fp - bytes_below_sp;
	  if (hard_fp_valid_p)
	    base_rtx = hard_frame_pointer_rtx;
	  else
	    {
	      if (!anchor_reg)
		{
		  anchor_reg = gen_rtx_REG (Pmode, STACK_CLASH_SVE_CFA_REGNUM);
		  emit_insn (gen_add3_insn (anchor_reg, base_rtx,
					    gen_int_mode (fp_offset, Pmode)));
		}
	      base_rtx = anchor_reg;
	    }
	  offset -= fp_offset;
	}
      rtx mem = gen_frame_mem (mode, plus_constant (Pmode, base_rtx, offset));
      rtx cfi_mem = gen_frame_mem (mode, plus_constant (Pmode,
							stack_pointer_rtx,
							sp_offset));
      rtx cfi_set = gen_rtx_SET (cfi_mem, reg);
      bool need_cfi_note_p = (base_rtx != stack_pointer_rtx);

      unsigned int regno2;
      if (!aarch64_sve_mode_p (mode)
	  && reg == move_src
	  && i + 1 < regs.size ()
	  && (regno2 = regs[i + 1], !skip_save_p (regno2))
	  && known_eq (GET_MODE_SIZE (mode),
		       frame.reg_offset[regno2] - frame.reg_offset[regno]))
	{
	  rtx reg2 = gen_rtx_REG (mode, regno2);

	  offset += GET_MODE_SIZE (mode);
	  insn = emit_insn (aarch64_gen_store_pair (mem, reg, reg2));

	  rtx cfi_mem2
	    = gen_frame_mem (mode,
			     plus_constant (Pmode,
					    stack_pointer_rtx,
					    sp_offset + GET_MODE_SIZE (mode)));
	  rtx cfi_set2 = gen_rtx_SET (cfi_mem2, reg2);

	  /* The first part of a frame-related parallel insn is always
	     assumed to be relevant to the frame calculations;
	     subsequent parts, are only frame-related if
	     explicitly marked.  */
	  if (aarch64_emit_cfi_for_reg_p (regno2))
	    RTX_FRAME_RELATED_P (cfi_set2) = 1;

	  /* Add a REG_FRAME_RELATED_EXPR note since the unspec
	     representation of stp cannot be understood directly by
	     dwarf2cfi.  */
	  rtx par = gen_rtx_PARALLEL (VOIDmode,
				      gen_rtvec (2, cfi_set, cfi_set2));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, par);

	  regno = regno2;
	  ++i;
	}
      else
	{
	  if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	    {
	      insn = emit_insn (gen_aarch64_pred_mov (mode, mem,
						      ptrue, move_src));
	      need_cfi_note_p = true;
	    }
	  else if (aarch64_sve_mode_p (mode))
	    insn = emit_insn (gen_rtx_SET (mem, move_src));
	  else
	    insn = emit_move_insn (mem, move_src);

	  if (frame_related_p && (need_cfi_note_p || move_src != reg))
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR, cfi_set);
	}

      RTX_FRAME_RELATED_P (insn) = frame_related_p;

      /* Emit a fake instruction to indicate that the VG save slot has
	 been initialized.  */
      if (regno == VG_REGNUM)
	emit_insn (gen_aarch64_old_vg_saved (move_src, mem));
    }
}

/* Emit code to restore the callee registers in REGS, ignoring pop candidates
   and any other registers that are handled separately.  Write the appropriate
   REG_CFA_RESTORE notes into CFI_OPS.

   The stack pointer is currently BYTES_BELOW_SP bytes above the bottom
   of the static frame.  */

static void
aarch64_restore_callee_saves (poly_int64 bytes_below_sp,
			      array_slice<unsigned int> regs, rtx *cfi_ops)
{
  aarch64_frame &frame = cfun->machine->frame;
  poly_int64 offset;
  rtx anchor_reg = NULL_RTX, ptrue = NULL_RTX;

  auto skip_restore_p = [&](unsigned int regno)
    {
      if (cfun->machine->reg_is_wrapped_separately[regno])
	return true;

      if (regno == frame.wb_pop_candidate1
	  || regno == frame.wb_pop_candidate2)
	return true;

      /* The shadow call stack code restores LR separately.  */
      if (frame.is_scs_enabled && regno == LR_REGNUM)
	return true;

      return false;
    };

  for (unsigned int i = 0; i < regs.size (); ++i)
    {
      unsigned int regno = regs[i];
      bool frame_related_p = aarch64_emit_cfi_for_reg_p (regno);
      if (skip_restore_p (regno))
	continue;

      machine_mode mode = aarch64_reg_save_mode (regno);
      rtx reg = gen_rtx_REG (mode, regno);
      offset = frame.reg_offset[regno] - bytes_below_sp;
      rtx base_rtx = stack_pointer_rtx;
      if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	aarch64_adjust_sve_callee_save_base (mode, base_rtx, anchor_reg,
					     offset, ptrue);
      rtx mem = gen_frame_mem (mode, plus_constant (Pmode, base_rtx, offset));

      unsigned int regno2;
      if (!aarch64_sve_mode_p (mode)
	  && i + 1 < regs.size ()
	  && (regno2 = regs[i + 1], !skip_restore_p (regno2))
	  && known_eq (GET_MODE_SIZE (mode),
		       frame.reg_offset[regno2] - frame.reg_offset[regno]))
	{
	  rtx reg2 = gen_rtx_REG (mode, regno2);

	  offset += GET_MODE_SIZE (mode);
	  emit_insn (aarch64_gen_load_pair (reg, reg2, mem));

	  *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg2, *cfi_ops);
	  regno = regno2;
	  ++i;
	}
      else if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	emit_insn (gen_aarch64_pred_mov (mode, reg, ptrue, mem));
      else if (aarch64_sve_mode_p (mode))
	emit_insn (gen_rtx_SET (reg, mem));
      else
	emit_move_insn (reg, mem);
      if (frame_related_p)
	*cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg, *cfi_ops);
    }
}

/* Return true if OFFSET is a signed 4-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_4bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -8, 7));
}

/* Return true if OFFSET is a signed 6-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_6bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -32, 31));
}

/* Return true if OFFSET is an unsigned 6-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_6bit_unsigned_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, 0, 63));
}

/* Return true if OFFSET is a signed 7-bit value multiplied by the size
   of MODE.  */

bool
aarch64_offset_7bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -64, 63));
}

/* Return true if OFFSET is a signed 9-bit value.  */

bool
aarch64_offset_9bit_signed_unscaled_p (machine_mode mode ATTRIBUTE_UNUSED,
				       poly_int64 offset)
{
  HOST_WIDE_INT const_offset;
  return (offset.is_constant (&const_offset)
	  && IN_RANGE (const_offset, -256, 255));
}

/* Return true if OFFSET is a signed 9-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_9bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -256, 255));
}

/* Return true if OFFSET is an unsigned 12-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_12bit_unsigned_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, 0, 4095));
}

/* Implement TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS.  */

static sbitmap
aarch64_get_separate_components (void)
{
  aarch64_frame &frame = cfun->machine->frame;
  sbitmap components = sbitmap_alloc (LAST_SAVED_REGNUM + 1);
  bitmap_clear (components);

  /* The registers we need saved to the frame.  */
  bool enables_pstate_sm = aarch64_cfun_enables_pstate_sm ();
  for (unsigned regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    if (aarch64_register_saved_on_entry (regno))
      {
	/* Disallow shrink wrapping for registers that will be clobbered
	   by an SMSTART SM in the prologue.  */
	if (enables_pstate_sm
	    && (FP_REGNUM_P (regno) || PR_REGNUM_P (regno)))
	  continue;

	/* Punt on saves and restores that use ST1D and LD1D.  We could
	   try to be smarter, but it would involve making sure that the
	   spare predicate register itself is safe to use at the save
	   and restore points.  Also, when a frame pointer is being used,
	   the slots are often out of reach of ST1D and LD1D anyway.  */
	machine_mode mode = aarch64_reg_save_mode (regno);
	if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	  continue;

	poly_int64 offset = frame.reg_offset[regno];

	/* Get the offset relative to the register we'll use.  */
	if (frame_pointer_needed)
	  offset -= frame.bytes_below_hard_fp;

	/* Check that we can access the stack slot of the register with one
	   direct load with no adjustments needed.  */
	if (aarch64_sve_mode_p (mode)
	    ? offset_9bit_signed_scaled_p (mode, offset)
	    : offset_12bit_unsigned_scaled_p (mode, offset))
	  bitmap_set_bit (components, regno);
      }

  /* Don't mess with the hard frame pointer.  */
  if (frame_pointer_needed)
    bitmap_clear_bit (components, HARD_FRAME_POINTER_REGNUM);

  /* If the spare predicate register used by big-endian SVE code
     is call-preserved, it must be saved in the main prologue
     before any saves that use it.  */
  if (frame.spare_pred_reg != INVALID_REGNUM)
    bitmap_clear_bit (components, frame.spare_pred_reg);

  unsigned reg1 = frame.wb_push_candidate1;
  unsigned reg2 = frame.wb_push_candidate2;
  /* If registers have been chosen to be stored/restored with
     writeback don't interfere with them to avoid having to output explicit
     stack adjustment instructions.  */
  if (reg2 != INVALID_REGNUM)
    bitmap_clear_bit (components, reg2);
  if (reg1 != INVALID_REGNUM)
    bitmap_clear_bit (components, reg1);

  bitmap_clear_bit (components, LR_REGNUM);
  bitmap_clear_bit (components, SP_REGNUM);
  if (flag_stack_clash_protection)
    {
      if (frame.sve_save_and_probe != INVALID_REGNUM)
	bitmap_clear_bit (components, frame.sve_save_and_probe);
      if (frame.hard_fp_save_and_probe != INVALID_REGNUM)
	bitmap_clear_bit (components, frame.hard_fp_save_and_probe);
    }

  /* The VG save sequence needs a temporary GPR.  Punt for now on trying
     to find one.  */
  bitmap_clear_bit (components, VG_REGNUM);

  return components;
}

/* Implement TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB.  */

static sbitmap
aarch64_components_for_bb (basic_block bb)
{
  bitmap in = DF_LIVE_IN (bb);
  bitmap gen = &DF_LIVE_BB_INFO (bb)->gen;
  bitmap kill = &DF_LIVE_BB_INFO (bb)->kill;

  sbitmap components = sbitmap_alloc (LAST_SAVED_REGNUM + 1);
  bitmap_clear (components);

  /* Clobbered registers don't generate values in any meaningful sense,
     since nothing after the clobber can rely on their value.  And we can't
     say that partially-clobbered registers are unconditionally killed,
     because whether they're killed or not depends on the mode of the
     value they're holding.  Thus partially call-clobbered registers
     appear in neither the kill set nor the gen set.

     Check manually for any calls that clobber more of a register than the
     current function can.  */
  function_abi_aggregator callee_abis;
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (CALL_P (insn))
      callee_abis.note_callee_abi (insn_callee_abi (insn));
  HARD_REG_SET extra_caller_saves = callee_abis.caller_save_regs (*crtl->abi);

  /* GPRs are used in a bb if they are in the IN, GEN, or KILL sets.  */
  for (unsigned regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    if (!fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno)
	&& (TEST_HARD_REG_BIT (extra_caller_saves, regno)
	    || bitmap_bit_p (in, regno)
	    || bitmap_bit_p (gen, regno)
	    || bitmap_bit_p (kill, regno)))
      {
	bitmap_set_bit (components, regno);

	/* If there is a callee-save at an adjacent offset, add it too
	   to increase the use of LDP/STP.  */
	poly_int64 offset = cfun->machine->frame.reg_offset[regno];
	unsigned regno2 = multiple_p (offset, 16) ? regno + 1 : regno - 1;

	if (regno2 <= LAST_SAVED_REGNUM)
	  {
	    poly_int64 offset2 = cfun->machine->frame.reg_offset[regno2];
	    if (regno < regno2
		? known_eq (offset + 8, offset2)
		: multiple_p (offset2, 16) && known_eq (offset2 + 8, offset))
	      bitmap_set_bit (components, regno2);
	  }
      }

  return components;
}

/* Implement TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS.
   Nothing to do for aarch64.  */

static void
aarch64_disqualify_components (sbitmap, edge, sbitmap, bool)
{
}

/* Return the next set bit in BMP from START onwards.  Return the total number
   of bits in BMP if no set bit is found at or after START.  */

static unsigned int
aarch64_get_next_set_bit (sbitmap bmp, unsigned int start)
{
  unsigned int nbits = SBITMAP_SIZE (bmp);
  if (start == nbits)
    return start;

  gcc_assert (start < nbits);
  for (unsigned int i = start; i < nbits; i++)
    if (bitmap_bit_p (bmp, i))
      return i;

  return nbits;
}

/* Do the work for aarch64_emit_prologue_components and
   aarch64_emit_epilogue_components.  COMPONENTS is the bitmap of registers
   to save/restore, PROLOGUE_P indicates whether to emit the prologue sequence
   for these components or the epilogue sequence.  That is, it determines
   whether we should emit stores or loads and what kind of CFA notes to attach
   to the insns.  Otherwise the logic for the two sequences is very
   similar.  */

static void
aarch64_process_components (sbitmap components, bool prologue_p)
{
  aarch64_frame &frame = cfun->machine->frame;
  rtx ptr_reg = gen_rtx_REG (Pmode, frame_pointer_needed
			     ? HARD_FRAME_POINTER_REGNUM
			     : STACK_POINTER_REGNUM);

  unsigned last_regno = SBITMAP_SIZE (components);
  unsigned regno = aarch64_get_next_set_bit (components, R0_REGNUM);
  rtx_insn *insn = NULL;

  while (regno != last_regno)
    {
      bool frame_related_p = aarch64_emit_cfi_for_reg_p (regno);
      machine_mode mode = aarch64_reg_save_mode (regno);

      rtx reg = gen_rtx_REG (mode, regno);
      poly_int64 offset = frame.reg_offset[regno];
      if (frame_pointer_needed)
	offset -= frame.bytes_below_hard_fp;

      rtx addr = plus_constant (Pmode, ptr_reg, offset);
      rtx mem = gen_frame_mem (mode, addr);

      rtx set = prologue_p ? gen_rtx_SET (mem, reg) : gen_rtx_SET (reg, mem);
      unsigned regno2 = aarch64_get_next_set_bit (components, regno + 1);
      /* No more registers to handle after REGNO.
	 Emit a single save/restore and exit.  */
      if (regno2 == last_regno)
	{
	  insn = emit_insn (set);
	  if (frame_related_p)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      if (prologue_p)
		add_reg_note (insn, REG_CFA_OFFSET, copy_rtx (set));
	      else
		add_reg_note (insn, REG_CFA_RESTORE, reg);
	    }
	  break;
	}

      poly_int64 offset2 = frame.reg_offset[regno2];
      /* The next register is not of the same class or its offset is not
	 mergeable with the current one into a pair.  */
      if (aarch64_sve_mode_p (mode)
	  || !satisfies_constraint_Ump (mem)
	  || GP_REGNUM_P (regno) != GP_REGNUM_P (regno2)
	  || (crtl->abi->id () == ARM_PCS_SIMD && FP_REGNUM_P (regno))
	  || maybe_ne ((offset2 - frame.reg_offset[regno]),
		       GET_MODE_SIZE (mode)))
	{
	  insn = emit_insn (set);
	  if (frame_related_p)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      if (prologue_p)
		add_reg_note (insn, REG_CFA_OFFSET, copy_rtx (set));
	      else
		add_reg_note (insn, REG_CFA_RESTORE, reg);
	    }

	  regno = regno2;
	  continue;
	}

      bool frame_related2_p = aarch64_emit_cfi_for_reg_p (regno2);

      /* REGNO2 can be saved/restored in a pair with REGNO.  */
      rtx reg2 = gen_rtx_REG (mode, regno2);
      if (frame_pointer_needed)
	offset2 -= frame.bytes_below_hard_fp;
      rtx addr2 = plus_constant (Pmode, ptr_reg, offset2);
      rtx mem2 = gen_frame_mem (mode, addr2);
      rtx set2 = prologue_p ? gen_rtx_SET (mem2, reg2)
			     : gen_rtx_SET (reg2, mem2);

      if (prologue_p)
	insn = emit_insn (aarch64_gen_store_pair (mem, reg, reg2));
      else
	insn = emit_insn (aarch64_gen_load_pair (reg, reg2, mem));

      if (frame_related_p || frame_related2_p)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  if (prologue_p)
	    {
	      if (frame_related_p)
		add_reg_note (insn, REG_CFA_OFFSET, set);
	      if (frame_related2_p)
		add_reg_note (insn, REG_CFA_OFFSET, set2);
	    }
	  else
	    {
	      if (frame_related_p)
		add_reg_note (insn, REG_CFA_RESTORE, reg);
	      if (frame_related2_p)
		add_reg_note (insn, REG_CFA_RESTORE, reg2);
	    }
	}

      regno = aarch64_get_next_set_bit (components, regno2 + 1);
    }
}

/* Implement TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS.  */

static void
aarch64_emit_prologue_components (sbitmap components)
{
  aarch64_process_components (components, true);
}

/* Implement TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS.  */

static void
aarch64_emit_epilogue_components (sbitmap components)
{
  aarch64_process_components (components, false);
}

/* Implement TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS.  */

static void
aarch64_set_handled_components (sbitmap components)
{
  for (unsigned regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    if (bitmap_bit_p (components, regno))
      cfun->machine->reg_is_wrapped_separately[regno] = true;
}

/* On AArch64 we have an ABI defined safe buffer.  This constant is used to
   determining the probe offset for alloca.  */

static HOST_WIDE_INT
aarch64_stack_clash_protection_alloca_probe_range (void)
{
  return STACK_CLASH_CALLER_GUARD;
}

/* Emit a stack tie that acts as a scheduling barrier for all previous and
   subsequent memory accesses and that requires the stack pointer and REG
   to have their current values.  REG can be stack_pointer_rtx if no
   other register's value needs to be fixed.  */

static void
aarch64_emit_stack_tie (rtx reg)
{
  emit_insn (gen_stack_tie (reg, gen_int_mode (REGNO (reg), DImode)));
}

/* Allocate POLY_SIZE bytes of stack space using TEMP1 and TEMP2 as scratch
   registers.  If POLY_SIZE is not large enough to require a probe this function
   will only adjust the stack.  When allocating the stack space
   FRAME_RELATED_P is then used to indicate if the allocation is frame related.
   FINAL_ADJUSTMENT_P indicates whether we are allocating the area below
   the saved registers.  If we are then we ensure that any allocation
   larger than the ABI defined buffer needs a probe so that the
   invariant of having a 1KB buffer is maintained.

   We emit barriers after each stack adjustment to prevent optimizations from
   breaking the invariant that we never drop the stack more than a page.  This
   invariant is needed to make it easier to correctly handle asynchronous
   events, e.g. if we were to allow the stack to be dropped by more than a page
   and then have multiple probes up and we take a signal somewhere in between
   then the signal handler doesn't know the state of the stack and can make no
   assumptions about which pages have been probed.

   FORCE_ISA_MODE is AARCH64_ISA_MODE_SM_ON if any variable component of
   POLY_SIZE is measured relative to the SME vector length instead of the
   current prevailing vector length.  It is 0 otherwise.  */

static void
aarch64_allocate_and_probe_stack_space (rtx temp1, rtx temp2,
					poly_int64 poly_size,
					aarch64_isa_mode force_isa_mode,
					bool frame_related_p,
					bool final_adjustment_p)
{
  aarch64_frame &frame = cfun->machine->frame;
  HOST_WIDE_INT guard_size
    = 1 << param_stack_clash_protection_guard_size;
  HOST_WIDE_INT guard_used_by_caller = STACK_CLASH_CALLER_GUARD;
  HOST_WIDE_INT byte_sp_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  gcc_assert (multiple_p (poly_size, byte_sp_alignment));
  HOST_WIDE_INT min_probe_threshold
    = (final_adjustment_p
       ? guard_used_by_caller + byte_sp_alignment
       : guard_size - guard_used_by_caller);
  poly_int64 frame_size = frame.frame_size;

  /* We should always have a positive probe threshold.  */
  gcc_assert (min_probe_threshold > 0);

  if (flag_stack_clash_protection && !final_adjustment_p)
    {
      poly_int64 initial_adjust = frame.initial_adjust;
      poly_int64 sve_callee_adjust = frame.sve_callee_adjust;
      poly_int64 final_adjust = frame.final_adjust;

      if (known_eq (frame_size, 0))
	{
	  dump_stack_clash_frame_info (NO_PROBE_NO_FRAME, false);
	}
      else if (known_lt (initial_adjust + sve_callee_adjust,
			 guard_size - guard_used_by_caller)
	       && known_lt (final_adjust, guard_used_by_caller))
	{
	  dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);
	}
    }

  /* If SIZE is not large enough to require probing, just adjust the stack and
     exit.  */
  if (known_lt (poly_size, min_probe_threshold)
      || !flag_stack_clash_protection)
    {
      aarch64_sub_sp (temp1, temp2, poly_size, force_isa_mode,
		      frame_related_p);
      return;
    }

  HOST_WIDE_INT size;
  /* Handle the SVE non-constant case first.  */
  if (!poly_size.is_constant (&size))
    {
     if (dump_file)
      {
	fprintf (dump_file, "Stack clash SVE prologue: ");
	print_dec (poly_size, dump_file);
	fprintf (dump_file, " bytes, dynamic probing will be required.\n");
      }

      /* First calculate the amount of bytes we're actually spilling.  */
      aarch64_add_offset (Pmode, temp1, CONST0_RTX (Pmode),
			  poly_size, temp1, temp2, force_isa_mode,
			  false, true);

      rtx_insn *insn = get_last_insn ();

      if (frame_related_p)
	{
	  /* This is done to provide unwinding information for the stack
	     adjustments we're about to do, however to prevent the optimizers
	     from removing the R11 move and leaving the CFA note (which would be
	     very wrong) we tie the old and new stack pointer together.
	     The tie will expand to nothing but the optimizers will not touch
	     the instruction.  */
	  rtx stack_ptr_copy = gen_rtx_REG (Pmode, STACK_CLASH_SVE_CFA_REGNUM);
	  emit_move_insn (stack_ptr_copy, stack_pointer_rtx);
	  aarch64_emit_stack_tie (stack_ptr_copy);

	  /* We want the CFA independent of the stack pointer for the
	     duration of the loop.  */
	  add_reg_note (insn, REG_CFA_DEF_CFA, stack_ptr_copy);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      rtx probe_const = gen_int_mode (min_probe_threshold, Pmode);
      rtx guard_const = gen_int_mode (guard_size, Pmode);

      insn = emit_insn (gen_probe_sve_stack_clash (Pmode, stack_pointer_rtx,
						   stack_pointer_rtx, temp1,
						   probe_const, guard_const));

      /* Now reset the CFA register if needed.  */
      if (frame_related_p)
	{
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				      gen_int_mode (poly_size, Pmode)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      return;
    }

  if (dump_file)
    fprintf (dump_file,
	     "Stack clash AArch64 prologue: " HOST_WIDE_INT_PRINT_DEC
	     " bytes, probing will be required.\n", size);

  /* Round size to the nearest multiple of guard_size, and calculate the
     residual as the difference between the original size and the rounded
     size.  */
  HOST_WIDE_INT rounded_size = ROUND_DOWN (size, guard_size);
  HOST_WIDE_INT residual = size - rounded_size;

  /* We can handle a small number of allocations/probes inline.  Otherwise
     punt to a loop.  */
  if (rounded_size <= STACK_CLASH_MAX_UNROLL_PAGES * guard_size)
    {
      for (HOST_WIDE_INT i = 0; i < rounded_size; i += guard_size)
	{
	  aarch64_sub_sp (NULL, temp2, guard_size, force_isa_mode, true);
	  emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					   guard_used_by_caller));
	  emit_insn (gen_blockage ());
	}
      dump_stack_clash_frame_info (PROBE_INLINE, size != rounded_size);
    }
  else
    {
      /* Compute the ending address.  */
      aarch64_add_offset (Pmode, temp1, stack_pointer_rtx, -rounded_size,
			  temp1, NULL, force_isa_mode, false, true);
      rtx_insn *insn = get_last_insn ();

      /* For the initial allocation, we don't have a frame pointer
	 set up, so we always need CFI notes.  If we're doing the
	 final allocation, then we may have a frame pointer, in which
	 case it is the CFA, otherwise we need CFI notes.

	 We can determine which allocation we are doing by looking at
	 the value of FRAME_RELATED_P since the final allocations are not
	 frame related.  */
      if (frame_related_p)
	{
	  /* We want the CFA independent of the stack pointer for the
	     duration of the loop.  */
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, temp1, rounded_size));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* This allocates and probes the stack.  Note that this re-uses some of
	 the existing Ada stack protection code.  However we are guaranteed not
	 to enter the non loop or residual branches of that code.

	 The non-loop part won't be entered because if our allocation amount
	 doesn't require a loop, the case above would handle it.

	 The residual amount won't be entered because TEMP1 is a mutliple of
	 the allocation size.  The residual will always be 0.  As such, the only
	 part we are actually using from that code is the loop setup.  The
	 actual probing is done in aarch64_output_probe_stack_range.  */
      insn = emit_insn (gen_probe_stack_range (stack_pointer_rtx,
					       stack_pointer_rtx, temp1));

      /* Now reset the CFA register if needed.  */
      if (frame_related_p)
	{
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx, rounded_size));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      emit_insn (gen_blockage ());
      dump_stack_clash_frame_info (PROBE_LOOP, size != rounded_size);
    }

  /* Handle any residuals.  Residuals of at least MIN_PROBE_THRESHOLD have to
     be probed.  This maintains the requirement that each page is probed at
     least once.  For initial probing we probe only if the allocation is
     more than GUARD_SIZE - buffer, and below the saved registers we probe
     if the amount is larger than buffer.  GUARD_SIZE - buffer + buffer ==
     GUARD_SIZE.  This works that for any allocation that is large enough to
     trigger a probe here, we'll have at least one, and if they're not large
     enough for this code to emit anything for them, The page would have been
     probed by the saving of FP/LR either by this function or any callees.  If
     we don't have any callees then we won't have more stack adjustments and so
     are still safe.  */
  if (residual)
    {
      gcc_assert (guard_used_by_caller + byte_sp_alignment <= size);

      /* If we're doing final adjustments, and we've done any full page
	 allocations then any residual needs to be probed.  */
      if (final_adjustment_p && rounded_size != 0)
	min_probe_threshold = 0;

      aarch64_sub_sp (temp1, temp2, residual, force_isa_mode, frame_related_p);
      if (residual >= min_probe_threshold)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Stack clash AArch64 prologue residuals: "
		     HOST_WIDE_INT_PRINT_DEC " bytes, probing will be required."
		     "\n", residual);

	  emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					   guard_used_by_caller));
	  emit_insn (gen_blockage ());
	}
    }
}

/* Implement TARGET_EXTRA_LIVE_ON_ENTRY.  */

void
aarch64_extra_live_on_entry (bitmap regs)
{
  if (TARGET_ZA)
    {
      bitmap_set_bit (regs, LOWERING_REGNUM);
      bitmap_set_bit (regs, SME_STATE_REGNUM);
      bitmap_set_bit (regs, TPIDR2_SETUP_REGNUM);
      bitmap_set_bit (regs, ZA_FREE_REGNUM);
      bitmap_set_bit (regs, ZA_SAVED_REGNUM);

      /* The only time ZA can't have live contents on entry is when
	 the function explicitly treats it as a pure output.  */
      auto za_flags = aarch64_cfun_shared_flags ("za");
      if (za_flags != (AARCH64_STATE_SHARED | AARCH64_STATE_OUT))
	bitmap_set_bit (regs, ZA_REGNUM);

      /* Since ZT0 is call-clobbered, it is only live on input if
	 it is explicitly shared, and is not a pure output.  */
      auto zt0_flags = aarch64_cfun_shared_flags ("zt0");
      if (zt0_flags != 0
	  && zt0_flags != (AARCH64_STATE_SHARED | AARCH64_STATE_OUT))
	bitmap_set_bit (regs, ZT0_REGNUM);
    }
}

/* Return 1 if the register is used by the epilogue.  We need to say the
   return register is used, but only after epilogue generation is complete.
   Note that in the case of sibcalls, the values "used by the epilogue" are
   considered live at the start of the called function.  */

int
aarch64_epilogue_uses (int regno)
{
  if (epilogue_completed)
    {
      if (regno == LR_REGNUM)
	return 1;
    }
  if (regno == LOWERING_REGNUM && TARGET_ZA)
    return 1;
  if (regno == SME_STATE_REGNUM && TARGET_ZA)
    return 1;
  if (regno == TPIDR2_SETUP_REGNUM && TARGET_ZA)
    return 1;
  /* If the function shares SME state with its caller, ensure that that
     data is not in the lazy save buffer on exit.  */
  if (regno == ZA_SAVED_REGNUM && aarch64_cfun_incoming_pstate_za () != 0)
    return 1;
  if (regno == ZA_REGNUM && aarch64_cfun_shared_flags ("za") != 0)
    return 1;
  if (regno == ZT0_REGNUM && aarch64_cfun_shared_flags ("zt0") != 0)
    return 1;
  return 0;
}

/* Implement TARGET_USE_LATE_PROLOGUE_EPILOGUE.  */

static bool
aarch64_use_late_prologue_epilogue ()
{
  return aarch64_cfun_enables_pstate_sm ();
}

/* The current function's frame has a save slot for the incoming state
   of SVCR.  Return a legitimate memory for the slot, based on the hard
   frame pointer.  */

static rtx
aarch64_old_svcr_mem ()
{
  gcc_assert (frame_pointer_needed
	      && known_ge (cfun->machine->frame.old_svcr_offset, 0));
  rtx base = hard_frame_pointer_rtx;
  poly_int64 offset = (0
		       /* hard fp -> bottom of frame.  */
		       - cfun->machine->frame.bytes_below_hard_fp
		       /* bottom of frame -> save slot.  */
		       + cfun->machine->frame.old_svcr_offset);
  return gen_frame_mem (DImode, plus_constant (Pmode, base, offset));
}

/* The current function's frame has a save slot for the incoming state
   of SVCR.  Load the slot into register REGNO and return the register.  */

static rtx
aarch64_read_old_svcr (unsigned int regno)
{
  rtx svcr = gen_rtx_REG (DImode, regno);
  emit_move_insn (svcr, aarch64_old_svcr_mem ());
  return svcr;
}

/* Like the rtx version of aarch64_guard_switch_pstate_sm, but first
   load the incoming value of SVCR from its save slot into temporary
   register REGNO.  */

static rtx_insn *
aarch64_guard_switch_pstate_sm (unsigned int regno,
				aarch64_isa_mode local_mode)
{
  rtx old_svcr = aarch64_read_old_svcr (regno);
  return aarch64_guard_switch_pstate_sm (old_svcr, local_mode);
}

/* AArch64 stack frames generated by this compiler look like:

	+-------------------------------+
	|                               |
	|  incoming stack arguments     |
	|                               |
	+-------------------------------+
	|                               | <-- incoming stack pointer (aligned)
	|  callee-allocated save area   |
	|  for register varargs         |
	|                               |
	+-------------------------------+
	|  local variables (1)          | <-- frame_pointer_rtx
	|                               |
	+-------------------------------+
	|  padding (1)                  |
	+-------------------------------+
	|  callee-saved registers       |
	+-------------------------------+
	|  LR'                          |
	+-------------------------------+
	|  FP'                          |
	+-------------------------------+ <-- hard_frame_pointer_rtx (aligned)
	|  SVE vector registers         |
	+-------------------------------+
	|  SVE predicate registers      |
	+-------------------------------+
	|  local variables (2)          |
	+-------------------------------+
	|  padding (2)                  |
	+-------------------------------+
	|  dynamic allocation           |
	+-------------------------------+
	|  padding                      |
	+-------------------------------+
	|  outgoing stack arguments     | <-- arg_pointer
        |                               |
	+-------------------------------+
	|                               | <-- stack_pointer_rtx (aligned)

   The regions marked (1) and (2) are mutually exclusive.  (2) is used
   when aarch64_save_regs_above_locals_p is true.

   Dynamic stack allocations via alloca() decrease stack_pointer_rtx
   but leave frame_pointer_rtx and hard_frame_pointer_rtx
   unchanged.

   By default for stack-clash we assume the guard is at least 64KB, but this
   value is configurable to either 4KB or 64KB.  We also force the guard size to
   be the same as the probing interval and both values are kept in sync.

   With those assumptions the callee can allocate up to 63KB (or 3KB depending
   on the guard size) of stack space without probing.

   When probing is needed, we emit a probe at the start of the prologue
   and every PARAM_STACK_CLASH_PROTECTION_GUARD_SIZE bytes thereafter.

   We can also use register saves as probes.  These are stored in
   sve_save_and_probe and hard_fp_save_and_probe.

   For outgoing arguments we probe if the size is larger than 1KB, such that
   the ABI specified buffer is maintained for the next callee.

   The following registers are reserved during frame layout and should not be
   used for any other purpose:

   - r11: Used by stack clash protection when SVE is enabled, and also
	  as an anchor register when saving and restoring registers
   - r12(EP0) and r13(EP1): Used as temporaries for stack adjustment.
   - r14 and r15: Used for speculation tracking.
   - r16(IP0), r17(IP1): Used by indirect tailcalls.
   - r30(LR), r29(FP): Used by standard frame layout.

   These registers must be avoided in frame layout related code unless the
   explicit intention is to interact with one of the features listed above.  */

/* Generate the prologue instructions for entry into a function.
   Establish the stack frame by decreasing the stack pointer with a
   properly calculated size and, if necessary, create a frame record
   filled with the values of LR and previous frame pointer.  The
   current FP is also set up if it is in use.  */

void
aarch64_expand_prologue (void)
{
  aarch64_frame &frame = cfun->machine->frame;
  poly_int64 frame_size = frame.frame_size;
  poly_int64 initial_adjust = frame.initial_adjust;
  HOST_WIDE_INT callee_adjust = frame.callee_adjust;
  poly_int64 final_adjust = frame.final_adjust;
  poly_int64 sve_callee_adjust = frame.sve_callee_adjust;
  unsigned reg1 = frame.wb_push_candidate1;
  unsigned reg2 = frame.wb_push_candidate2;
  bool emit_frame_chain = frame.emit_frame_chain;
  rtx_insn *insn;
  aarch64_isa_mode force_isa_mode = 0;
  if (aarch64_cfun_enables_pstate_sm ())
    force_isa_mode = AARCH64_ISA_MODE_SM_ON;

  if (flag_stack_clash_protection
      && known_eq (callee_adjust, 0)
      && known_lt (frame.reg_offset[VG_REGNUM], 0))
    {
      /* Fold the SVE allocation into the initial allocation.
	 We don't do this in aarch64_layout_arg to avoid pessimizing
	 the epilogue code.  */
      initial_adjust += sve_callee_adjust;
      sve_callee_adjust = 0;
    }

  /* Sign return address for functions.  */
  if (aarch64_return_address_signing_enabled ())
    {
      switch (aarch64_ra_sign_key)
	{
	  case AARCH64_KEY_A:
	    insn = emit_insn (gen_paciasp ());
	    break;
	  case AARCH64_KEY_B:
	    insn = emit_insn (gen_pacibsp ());
	    break;
	  default:
	    gcc_unreachable ();
	}
      add_reg_note (insn, REG_CFA_NEGATE_RA_STATE, const0_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Push return address to shadow call stack.  */
  if (frame.is_scs_enabled)
    emit_insn (gen_scs_push ());

  if (flag_stack_usage_info)
    current_function_static_stack_size = constant_lower_bound (frame_size);

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (maybe_gt (frame_size, PROBE_INTERVAL)
	      && maybe_gt (frame_size, get_stack_check_protect ()))
	    aarch64_emit_probe_stack_range (get_stack_check_protect (),
					    (frame_size
					     - get_stack_check_protect ()));
	}
      else if (maybe_gt (frame_size, 0))
	aarch64_emit_probe_stack_range (get_stack_check_protect (), frame_size);
    }

  rtx tmp0_rtx = gen_rtx_REG (Pmode, EP0_REGNUM);
  rtx tmp1_rtx = gen_rtx_REG (Pmode, EP1_REGNUM);

  /* In theory we should never have both an initial adjustment
     and a callee save adjustment.  Verify that is the case since the
     code below does not handle it for -fstack-clash-protection.  */
  gcc_assert (known_eq (initial_adjust, 0) || callee_adjust == 0);

  /* Will only probe if the initial adjustment is larger than the guard
     less the amount of the guard reserved for use by the caller's
     outgoing args.  */
  aarch64_allocate_and_probe_stack_space (tmp0_rtx, tmp1_rtx, initial_adjust,
					  force_isa_mode, true, false);

  if (callee_adjust != 0)
    aarch64_push_regs (reg1, reg2, callee_adjust);

  /* The offset of the current SP from the bottom of the static frame.  */
  poly_int64 bytes_below_sp = frame_size - initial_adjust - callee_adjust;

  if (emit_frame_chain)
    {
      /* The offset of the frame chain record (if any) from the current SP.  */
      poly_int64 chain_offset = (initial_adjust + callee_adjust
				 - frame.bytes_above_hard_fp);
      gcc_assert (known_ge (chain_offset, 0));

      gcc_assert (reg1 == R29_REGNUM && reg2 == R30_REGNUM);
      if (callee_adjust == 0)
	aarch64_save_callee_saves (bytes_below_sp, frame.saved_gprs,
				   false, false);
      else
	gcc_assert (known_eq (chain_offset, 0));
      aarch64_add_offset (Pmode, hard_frame_pointer_rtx,
			  stack_pointer_rtx, chain_offset,
			  tmp1_rtx, tmp0_rtx, force_isa_mode,
			  frame_pointer_needed);
      if (frame_pointer_needed && !frame_size.is_constant ())
	{
	  /* Variable-sized frames need to describe the save slot
	     address using DW_CFA_expression rather than DW_CFA_offset.
	     This means that, without taking further action, the
	     locations of the registers that we've already saved would
	     remain based on the stack pointer even after we redefine
	     the CFA based on the frame pointer.  We therefore need new
	     DW_CFA_expressions to re-express the save slots with addresses
	     based on the frame pointer.  */
	  rtx_insn *insn = get_last_insn ();
	  gcc_assert (RTX_FRAME_RELATED_P (insn));

	  /* Add an explicit CFA definition if this was previously
	     implicit.  */
	  if (!find_reg_note (insn, REG_CFA_ADJUST_CFA, NULL_RTX))
	    {
	      rtx src = plus_constant (Pmode, stack_pointer_rtx, chain_offset);
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (hard_frame_pointer_rtx, src));
	    }

	  /* Change the save slot expressions for the registers that
	     we've already saved.  */
	  aarch64_add_cfa_expression (insn, regno_reg_rtx[reg2],
				      hard_frame_pointer_rtx, UNITS_PER_WORD);
	  aarch64_add_cfa_expression (insn, regno_reg_rtx[reg1],
				      hard_frame_pointer_rtx, 0);
	}
      aarch64_emit_stack_tie (hard_frame_pointer_rtx);
    }

  aarch64_save_callee_saves (bytes_below_sp, frame.saved_gprs, true,
			     emit_frame_chain);
  if (maybe_ge (frame.reg_offset[VG_REGNUM], 0))
    {
      unsigned int saved_regs[] = { VG_REGNUM };
      aarch64_save_callee_saves (bytes_below_sp, saved_regs, true,
				 emit_frame_chain);
    }
  if (maybe_ne (sve_callee_adjust, 0))
    {
      gcc_assert (!flag_stack_clash_protection
		  || known_eq (initial_adjust, 0)
		  /* The VG save isn't shrink-wrapped and so serves as
		     a probe of the initial allocation.  */
		  || known_eq (frame.reg_offset[VG_REGNUM], bytes_below_sp));
      aarch64_allocate_and_probe_stack_space (tmp1_rtx, tmp0_rtx,
					      sve_callee_adjust,
					      force_isa_mode,
					      !frame_pointer_needed, false);
      bytes_below_sp -= sve_callee_adjust;
    }
  aarch64_save_callee_saves (bytes_below_sp, frame.saved_prs, true,
			     emit_frame_chain);
  aarch64_save_callee_saves (bytes_below_sp, frame.saved_fprs, true,
			     emit_frame_chain);

  /* We may need to probe the final adjustment if it is larger than the guard
     that is assumed by the called.  */
  gcc_assert (known_eq (bytes_below_sp, final_adjust));
  aarch64_allocate_and_probe_stack_space (tmp1_rtx, tmp0_rtx, final_adjust,
					  force_isa_mode,
					  !frame_pointer_needed, true);
  if (emit_frame_chain && maybe_ne (final_adjust, 0))
    aarch64_emit_stack_tie (hard_frame_pointer_rtx);

  /* Save the incoming value of PSTATE.SM, if required.  Code further
     down does this for locally-streaming functions.  */
  if (known_ge (frame.old_svcr_offset, 0)
      && !aarch64_cfun_enables_pstate_sm ())
    {
      rtx mem = aarch64_old_svcr_mem ();
      MEM_VOLATILE_P (mem) = 1;
      if (TARGET_SME)
	{
	  rtx reg = gen_rtx_REG (DImode, IP0_REGNUM);
	  emit_insn (gen_aarch64_read_svcr (reg));
	  emit_move_insn (mem, reg);
	}
      else
	{
	  rtx old_r0 = NULL_RTX, old_r1 = NULL_RTX;
	  auto &args = crtl->args.info;
	  if (args.aapcs_ncrn > 0)
	    {
	      old_r0 = gen_rtx_REG (DImode, PROBE_STACK_FIRST_REGNUM);
	      emit_move_insn (old_r0, gen_rtx_REG (DImode, R0_REGNUM));
	    }
	  if (args.aapcs_ncrn > 1)
	    {
	      old_r1 = gen_rtx_REG (DImode, PROBE_STACK_SECOND_REGNUM);
	      emit_move_insn (old_r1, gen_rtx_REG (DImode, R1_REGNUM));
	    }
	  emit_insn (gen_aarch64_get_sme_state ());
	  emit_move_insn (mem, gen_rtx_REG (DImode, R0_REGNUM));
	  if (old_r0)
	    emit_move_insn (gen_rtx_REG (DImode, R0_REGNUM), old_r0);
	  if (old_r1)
	    emit_move_insn (gen_rtx_REG (DImode, R1_REGNUM), old_r1);
	}
    }

  /* Enable PSTATE.SM, if required.  */
  if (aarch64_cfun_enables_pstate_sm ())
    {
      rtx_insn *guard_label = nullptr;
      if (known_ge (cfun->machine->frame.old_svcr_offset, 0))
	{
	  /* The current function is streaming-compatible.  Save the
	     original state of PSTATE.SM.  */
	  rtx svcr = gen_rtx_REG (DImode, IP0_REGNUM);
	  emit_insn (gen_aarch64_read_svcr (svcr));
	  emit_move_insn (aarch64_old_svcr_mem (), svcr);
	  guard_label = aarch64_guard_switch_pstate_sm (svcr,
							AARCH64_ISA_MODE);
	}
      aarch64_sme_mode_switch_regs args_switch;
      auto &args = crtl->args.info;
      for (unsigned int i = 0; i < args.num_sme_mode_switch_args; ++i)
	{
	  rtx x = args.sme_mode_switch_args[i];
	  args_switch.add_reg (GET_MODE (x), REGNO (x));
	}
      args_switch.emit_prologue ();
      emit_insn (gen_aarch64_smstart_sm ());
      args_switch.emit_epilogue ();
      if (guard_label)
	emit_label (guard_label);
    }
}

/* Return TRUE if we can use a simple_return insn.

   This function checks whether the callee saved stack is empty, which
   means no restore actions are need. The pro_and_epilogue will use
   this to check whether shrink-wrapping opt is feasible.  */

bool
aarch64_use_return_insn_p (void)
{
  if (!reload_completed)
    return false;

  if (crtl->profile)
    return false;

  return known_eq (cfun->machine->frame.frame_size, 0);
}

/* Generate the epilogue instructions for returning from a function.
   This is almost exactly the reverse of the prolog sequence, except
   that we need to insert barriers to avoid scheduling loads that read
   from a deallocated stack, and we optimize the unwind records by
   emitting them all together if possible.  */
void
aarch64_expand_epilogue (rtx_call_insn *sibcall)
{
  aarch64_frame &frame = cfun->machine->frame;
  poly_int64 initial_adjust = frame.initial_adjust;
  HOST_WIDE_INT callee_adjust = frame.callee_adjust;
  poly_int64 final_adjust = frame.final_adjust;
  poly_int64 sve_callee_adjust = frame.sve_callee_adjust;
  poly_int64 bytes_below_hard_fp = frame.bytes_below_hard_fp;
  unsigned reg1 = frame.wb_pop_candidate1;
  unsigned reg2 = frame.wb_pop_candidate2;
  rtx cfi_ops = NULL;
  rtx_insn *insn;
  /* A stack clash protection prologue may not have left EP0_REGNUM or
     EP1_REGNUM in a usable state.  The same is true for allocations
     with an SVE component, since we then need both temporary registers
     for each allocation.  For stack clash we are in a usable state if
     the adjustment is less than GUARD_SIZE - GUARD_USED_BY_CALLER.  */
  HOST_WIDE_INT guard_size
    = 1 << param_stack_clash_protection_guard_size;
  HOST_WIDE_INT guard_used_by_caller = STACK_CLASH_CALLER_GUARD;
  aarch64_isa_mode force_isa_mode = 0;
  if (aarch64_cfun_enables_pstate_sm ())
    force_isa_mode = AARCH64_ISA_MODE_SM_ON;

  /* We can re-use the registers when:

     (a) the deallocation amount is the same as the corresponding
	 allocation amount (which is false if we combine the initial
	 and SVE callee save allocations in the prologue); and

     (b) the allocation amount doesn't need a probe (which is false
	 if the amount is guard_size - guard_used_by_caller or greater).

     In such situations the register should remain live with the correct
     value.  */
  bool can_inherit_p = (initial_adjust.is_constant ()
			&& final_adjust.is_constant ()
			&& (!flag_stack_clash_protection
			    || (known_lt (initial_adjust,
					  guard_size - guard_used_by_caller)
				&& known_eq (sve_callee_adjust, 0))));

  /* We need to add memory barrier to prevent read from deallocated stack.  */
  bool need_barrier_p
    = maybe_ne (get_frame_size ()
		+ frame.saved_varargs_size, 0);

  /* Reset PSTATE.SM, if required.  */
  if (aarch64_cfun_enables_pstate_sm ())
    {
      rtx_insn *guard_label = nullptr;
      if (known_ge (cfun->machine->frame.old_svcr_offset, 0))
	guard_label = aarch64_guard_switch_pstate_sm (IP0_REGNUM,
						      AARCH64_ISA_MODE);
      aarch64_sme_mode_switch_regs return_switch;
      if (sibcall)
	return_switch.add_call_args (sibcall);
      else if (crtl->return_rtx && REG_P (crtl->return_rtx))
	return_switch.add_reg (GET_MODE (crtl->return_rtx),
			       REGNO (crtl->return_rtx));
      return_switch.emit_prologue ();
      emit_insn (gen_aarch64_smstop_sm ());
      return_switch.emit_epilogue ();
      if (guard_label)
	emit_label (guard_label);
    }

  /* Emit a barrier to prevent loads from a deallocated stack.  */
  if (maybe_gt (final_adjust, crtl->outgoing_args_size)
      || cfun->calls_alloca
      || crtl->calls_eh_return)
    {
      aarch64_emit_stack_tie (stack_pointer_rtx);
      need_barrier_p = false;
    }

  /* Restore the stack pointer from the frame pointer if it may not
     be the same as the stack pointer.  */
  rtx tmp0_rtx = gen_rtx_REG (Pmode, EP0_REGNUM);
  rtx tmp1_rtx = gen_rtx_REG (Pmode, EP1_REGNUM);
  if (frame_pointer_needed
      && (maybe_ne (final_adjust, 0) || cfun->calls_alloca))
    /* If writeback is used when restoring callee-saves, the CFA
       is restored on the instruction doing the writeback.  */
    aarch64_add_offset (Pmode, stack_pointer_rtx,
			hard_frame_pointer_rtx,
			-bytes_below_hard_fp + final_adjust,
			tmp1_rtx, tmp0_rtx, force_isa_mode,
			callee_adjust == 0);
  else
     /* The case where we need to re-use the register here is very rare, so
	avoid the complicated condition and just always emit a move if the
	immediate doesn't fit.  */
     aarch64_add_sp (tmp1_rtx, tmp0_rtx, final_adjust, force_isa_mode, true);

  /* Restore the vector registers before the predicate registers,
     so that we can use P4 as a temporary for big-endian SVE frames.  */
  aarch64_restore_callee_saves (final_adjust, frame.saved_fprs, &cfi_ops);
  aarch64_restore_callee_saves (final_adjust, frame.saved_prs, &cfi_ops);
  if (maybe_ne (sve_callee_adjust, 0))
    aarch64_add_sp (NULL_RTX, NULL_RTX, sve_callee_adjust,
		    force_isa_mode, true);

  /* When shadow call stack is enabled, the scs_pop in the epilogue will
     restore x30, we don't need to restore x30 again in the traditional
     way.  */
  aarch64_restore_callee_saves (final_adjust + sve_callee_adjust,
				frame.saved_gprs, &cfi_ops);

  if (need_barrier_p)
    aarch64_emit_stack_tie (stack_pointer_rtx);

  if (callee_adjust != 0)
    aarch64_pop_regs (reg1, reg2, callee_adjust, &cfi_ops);

  /* If we have no register restore information, the CFA must have been
     defined in terms of the stack pointer since the end of the prologue.  */
  gcc_assert (cfi_ops || !frame_pointer_needed);

  if (cfi_ops && (callee_adjust != 0 || maybe_gt (initial_adjust, 65536)))
    {
      /* Emit delayed restores and set the CFA to be SP + initial_adjust.  */
      insn = get_last_insn ();
      rtx new_cfa = plus_constant (Pmode, stack_pointer_rtx, initial_adjust);
      REG_NOTES (insn) = alloc_reg_note (REG_CFA_DEF_CFA, new_cfa, cfi_ops);
      RTX_FRAME_RELATED_P (insn) = 1;
      cfi_ops = NULL;
    }

  /* Liveness of EP0_REGNUM can not be trusted across function calls either, so
     add restriction on emit_move optimization to leaf functions.  */
  aarch64_add_sp (tmp0_rtx, tmp1_rtx, initial_adjust, force_isa_mode,
		  (!can_inherit_p || !crtl->is_leaf
		   || df_regs_ever_live_p (EP0_REGNUM)));

  if (cfi_ops)
    {
      /* Emit delayed restores and reset the CFA to be SP.  */
      insn = get_last_insn ();
      cfi_ops = alloc_reg_note (REG_CFA_DEF_CFA, stack_pointer_rtx, cfi_ops);
      REG_NOTES (insn) = cfi_ops;
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Pop return address from shadow call stack.  */
  if (frame.is_scs_enabled)
    {
      machine_mode mode = aarch64_reg_save_mode (R30_REGNUM);
      rtx reg = gen_rtx_REG (mode, R30_REGNUM);

      insn = emit_insn (gen_scs_pop ());
      add_reg_note (insn, REG_CFA_RESTORE, reg);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Stack adjustment for exception handler.  */
  if (crtl->calls_eh_return && !sibcall)
    {
      /* If the EH_RETURN_TAKEN_RTX flag is set then we need
	 to unwind the stack and jump to the handler, otherwise
	 skip this eh_return logic and continue with normal
	 return after the label.  We have already reset the CFA
	 to be SP; letting the CFA move during this adjustment
	 is just as correct as retaining the CFA from the body
	 of the function.  Therefore, do nothing special.  */
      rtx_code_label *label = gen_label_rtx ();
      rtx x = aarch64_gen_compare_zero_and_branch (EQ, EH_RETURN_TAKEN_RTX,
						   label);
      rtx jump = emit_jump_insn (x);
      JUMP_LABEL (jump) = label;
      LABEL_NUSES (label)++;
      emit_insn (gen_add2_insn (stack_pointer_rtx,
				EH_RETURN_STACKADJ_RTX));
      emit_jump_insn (gen_indirect_jump (EH_RETURN_HANDLER_RTX));
      emit_barrier ();
      emit_label (label);
    }

  /* We prefer to emit the combined return/authenticate instruction RETAA,
     however there are three cases in which we must instead emit an explicit
     authentication instruction.

	1) Sibcalls don't return in a normal way, so if we're about to call one
	   we must authenticate.

	2) The RETAA instruction is not available before ARMv8.3-A, so if we are
	   generating code for !TARGET_ARMV8_3 we can't use it and must
	   explicitly authenticate.
    */
  if (aarch64_return_address_signing_enabled ()
      && (sibcall || !TARGET_ARMV8_3))
    {
      switch (aarch64_ra_sign_key)
	{
	  case AARCH64_KEY_A:
	    insn = emit_insn (gen_autiasp ());
	    break;
	  case AARCH64_KEY_B:
	    insn = emit_insn (gen_autibsp ());
	    break;
	  default:
	    gcc_unreachable ();
	}
      add_reg_note (insn, REG_CFA_NEGATE_RA_STATE, const0_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  emit_use (gen_rtx_REG (DImode, LR_REGNUM));
  if (!sibcall)
    emit_jump_insn (ret_rtx);
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */
static void
aarch64_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT delta,
			 HOST_WIDE_INT vcall_offset,
			 tree function)
{
  /* The this pointer is always in x0.  Note that this differs from
     Arm where the this pointer maybe bumped to r1 if r0 is required
     to return a pointer to an aggregate.  On AArch64 a result value
     pointer will be in x8.  */
  int this_regno = R0_REGNUM;
  rtx this_rtx, temp0, temp1, addr, funexp;
  rtx_insn *insn;
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk));

  if (aarch_bti_enabled ())
    emit_insn (gen_bti_c());

  reload_completed = 1;
  emit_note (NOTE_INSN_PROLOGUE_END);

  this_rtx = gen_rtx_REG (Pmode, this_regno);
  temp0 = gen_rtx_REG (Pmode, EP0_REGNUM);
  temp1 = gen_rtx_REG (Pmode, EP1_REGNUM);

  if (vcall_offset == 0)
    aarch64_add_offset (Pmode, this_rtx, this_rtx, delta, temp1, temp0,
			0, false);
  else
    {
      gcc_assert ((vcall_offset & (POINTER_BYTES - 1)) == 0);

      addr = this_rtx;
      if (delta != 0)
	{
	  if (delta >= -256 && delta < 256)
	    addr = gen_rtx_PRE_MODIFY (Pmode, this_rtx,
				       plus_constant (Pmode, this_rtx, delta));
	  else
	    aarch64_add_offset (Pmode, this_rtx, this_rtx, delta,
				temp1, temp0, 0, false);
	}

      if (Pmode == ptr_mode)
	aarch64_emit_move (temp0, gen_rtx_MEM (ptr_mode, addr));
      else
	aarch64_emit_move (temp0,
			   gen_rtx_ZERO_EXTEND (Pmode,
						gen_rtx_MEM (ptr_mode, addr)));

      if (vcall_offset >= -256 && vcall_offset < 4096 * POINTER_BYTES)
	  addr = plus_constant (Pmode, temp0, vcall_offset);
      else
	{
	  aarch64_internal_mov_immediate (temp1, GEN_INT (vcall_offset), true,
					  Pmode);
	  addr = gen_rtx_PLUS (Pmode, temp0, temp1);
	}

      if (Pmode == ptr_mode)
	aarch64_emit_move (temp1, gen_rtx_MEM (ptr_mode,addr));
      else
	aarch64_emit_move (temp1,
			   gen_rtx_SIGN_EXTEND (Pmode,
						gen_rtx_MEM (ptr_mode, addr)));

      emit_insn (gen_add2_insn (this_rtx, temp1));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  auto isa_mode = aarch64_fntype_isa_mode (TREE_TYPE (function));
  auto pcs_variant = arm_pcs (fndecl_abi (function).id ());
  bool ir = lookup_attribute ("indirect_return",
			      TYPE_ATTRIBUTES (TREE_TYPE (function)));
  rtx callee_abi = aarch64_gen_callee_cookie (isa_mode, pcs_variant, ir);
  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx, callee_abi));
  SIBLING_CALL_P (insn) = 1;

  insn = get_insns ();
  shorten_branches (insn);

  assemble_start_function (thunk, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk, fnname);

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}

static bool
aarch64_tls_referenced_p (rtx x)
{
  if (!TARGET_HAVE_TLS)
    return false;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    {
      const_rtx x = *iter;
      if (SYMBOL_REF_P (x) && SYMBOL_REF_TLS_MODEL (x) != 0)
	return true;
      /* Don't recurse into UNSPEC_TLS looking for TLS symbols; these are
	 TLS offsets, not real symbol references.  */
      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
	iter.skip_subrtxes ();
    }
  return false;
}


static bool
aarch64_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  if (GET_CODE (x) == HIGH)
    return true;

  /* There's no way to calculate VL-based values using relocations.  */
  subrtx_iterator::array_type array;
  HOST_WIDE_INT factor;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (GET_CODE (*iter) == CONST_POLY_INT
	|| aarch64_sme_vq_unspec_p (x, &factor))
      return true;

  poly_int64 offset;
  rtx base = strip_offset_and_salt (x, &offset);
  if (SYMBOL_REF_P (base) || LABEL_REF_P (base))
    {
      /* We checked for POLY_INT_CST offsets above.  */
      if (aarch64_classify_symbol (base, offset.to_constant ())
	  != SYMBOL_FORCE_TO_MEM)
	return true;
      else
	/* Avoid generating a 64-bit relocation in ILP32; leave
	   to aarch64_expand_mov_immediate to handle it properly.  */
	return mode != ptr_mode;
    }

  return aarch64_tls_referenced_p (x);
}

/* Implement TARGET_CASE_VALUES_THRESHOLD.
   The expansion for a table switch is quite expensive due to the number
   of instructions, the table lookup and hard to predict indirect jump.
   When optimizing for speed, and -O3 enabled, use the per-core tuning if
   set, otherwise use tables for >= 11 cases as a tradeoff between size and
   performance.  When optimizing for size, use 8 for smallest codesize.  */

static unsigned int
aarch64_case_values_threshold (void)
{
  /* Use the specified limit for the number of cases before using jump
     tables at higher optimization levels.  */
  if (optimize > 2
      && aarch64_tune_params.max_case_values != 0)
    return aarch64_tune_params.max_case_values;
  else
    return optimize_size ? 8 : 11;
}

/* Return true if register REGNO is a valid index register.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
aarch64_regno_ok_for_index_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }
  return GP_REGNUM_P (regno);
}

/* Return true if register REGNO is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
aarch64_regno_ok_for_base_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  /* The fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  return (GP_REGNUM_P (regno)
	  || regno == SP_REGNUM
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if X is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
aarch64_base_register_rtx_p (rtx x, bool strict_p)
{
  if (!strict_p
      && SUBREG_P (x)
      && contains_reg_of_mode[GENERAL_REGS][GET_MODE (SUBREG_REG (x))])
    x = SUBREG_REG (x);

  return (REG_P (x) && aarch64_regno_ok_for_base_p (REGNO (x), strict_p));
}

/* Return true if address offset is a valid index.  If it is, fill in INFO
   appropriately.  STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
aarch64_classify_index (struct aarch64_address_info *info, rtx x,
			machine_mode mode, bool strict_p)
{
  enum aarch64_address_type type;
  rtx index;
  int shift;

  /* (reg:P) */
  if ((REG_P (x) || SUBREG_P (x))
      && GET_MODE (x) == Pmode)
    {
      type = ADDRESS_REG_REG;
      index = x;
      shift = 0;
    }
  /* (sign_extend:DI (reg:SI)) */
  else if ((GET_CODE (x) == SIGN_EXTEND
	    || GET_CODE (x) == ZERO_EXTEND)
	   && GET_MODE (x) == DImode
	   && GET_MODE (XEXP (x, 0)) == SImode)
    {
      type = (GET_CODE (x) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (x, 0);
      shift = 0;
    }
  /* (mult:DI (sign_extend:DI (reg:SI)) (const_int scale)) */
  else if (GET_CODE (x) == MULT
	   && (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	       || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	   && GET_MODE (XEXP (x, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (x, 1)));
    }
  /* (ashift:DI (sign_extend:DI (reg:SI)) (const_int shift)) */
  else if (GET_CODE (x) == ASHIFT
	   && (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	       || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	   && GET_MODE (XEXP (x, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (x, 1));
    }
  /* (and:DI (mult:DI (reg:DI) (const_int scale))
     (const_int 0xffffffff<<shift)) */
  else if (GET_CODE (x) == AND
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == MULT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)));
      /* Avoid undefined code dealing with shift being -1. */
      if (shift != -1
	  && INTVAL (XEXP (x, 1)) != (HOST_WIDE_INT)0xffffffff << shift)
	shift = -1;
    }
  /* (and:DI (ashift:DI (reg:DI) (const_int shift))
     (const_int 0xffffffff<<shift)) */
  else if (GET_CODE (x) == AND
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == ASHIFT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (XEXP (x, 0), 1));
      if (INTVAL (XEXP (x, 1)) != (HOST_WIDE_INT)0xffffffff << shift)
	shift = -1;
    }
  /* (mult:P (reg:P) (const_int scale)) */
  else if (GET_CODE (x) == MULT
	   && GET_MODE (x) == Pmode
	   && GET_MODE (XEXP (x, 0)) == Pmode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_REG;
      index = XEXP (x, 0);
      shift = exact_log2 (INTVAL (XEXP (x, 1)));
    }
  /* (ashift:P (reg:P) (const_int shift)) */
  else if (GET_CODE (x) == ASHIFT
	   && GET_MODE (x) == Pmode
	   && GET_MODE (XEXP (x, 0)) == Pmode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_REG;
      index = XEXP (x, 0);
      shift = INTVAL (XEXP (x, 1));
    }
  else
    return false;

  if (!strict_p
      && SUBREG_P (index)
      && contains_reg_of_mode[GENERAL_REGS][GET_MODE (SUBREG_REG (index))])
    index = SUBREG_REG (index);

  auto vec_flags = aarch64_classify_vector_memory_mode (mode);
  if (vec_flags & VEC_SVE_DATA)
    {
      if (type != ADDRESS_REG_REG
	  || (1 << shift) != GET_MODE_UNIT_SIZE (mode))
	return false;
    }
  else
    {
      if (shift != 0
	  && !(IN_RANGE (shift, 1, 3)
	       && known_eq (1 << shift, GET_MODE_SIZE (mode))))
	return false;
    }

  if (REG_P (index)
      && aarch64_regno_ok_for_index_p (REGNO (index), strict_p))
    {
      info->type = type;
      info->offset = index;
      info->shift = shift;
      return true;
    }

  return false;
}

/* Return true if MODE is one of the modes for which we
   support LDP/STP operations.  */

static bool
aarch64_mode_valid_for_sched_fusion_p (machine_mode mode)
{
  return mode == SImode || mode == DImode
	 || mode == SFmode || mode == DFmode
	 || mode == SDmode || mode == DDmode
	 || (aarch64_vector_mode_supported_p (mode)
	     && (known_eq (GET_MODE_SIZE (mode), 8)
		 || known_eq (GET_MODE_SIZE (mode), 16)));
}

/* Return true if REGNO is a virtual pointer register, or an eliminable
   "soft" frame register.  Like REGNO_PTR_FRAME_P except that we don't
   include stack_pointer or hard_frame_pointer.  */
static bool
virt_or_elim_regno_p (unsigned regno)
{
  return ((regno >= FIRST_VIRTUAL_REGISTER
	   && regno <= LAST_VIRTUAL_POINTER_REGISTER)
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if X is a valid address of type TYPE for machine mode MODE.
   If it is, fill in INFO appropriately.  STRICT_P is true if
   REG_OK_STRICT is in effect.  */

bool
aarch64_classify_address (struct aarch64_address_info *info,
			  rtx x, machine_mode mode, bool strict_p,
			  aarch64_addr_query_type type)
{
  enum rtx_code code = GET_CODE (x);
  rtx op0, op1;
  poly_int64 offset;

  HOST_WIDE_INT const_size;

  /* Whether a vector mode is partial doesn't affect address legitimacy.
     Partial vectors like VNx8QImode allow the same indexed addressing
     mode and MUL VL addressing mode as full vectors like VNx16QImode;
     in both cases, MUL VL counts multiples of GET_MODE_SIZE.  */
  unsigned int vec_flags = aarch64_classify_vector_memory_mode (mode);
  vec_flags &= ~VEC_PARTIAL;

  /* On BE, we use load/store pair for all large int mode load/stores.
     TI/TF/TDmode may also use a load/store pair.  */
  bool advsimd_struct_p = (vec_flags == (VEC_ADVSIMD | VEC_STRUCT));
  bool load_store_pair_p = (type == ADDR_QUERY_LDP_STP
			    || type == ADDR_QUERY_LDP_STP_N
			    || mode == TImode
			    || mode == TFmode
			    || mode == TDmode
			    || ((!TARGET_SIMD || BYTES_BIG_ENDIAN)
				&& advsimd_struct_p));
  /* If we are dealing with ADDR_QUERY_LDP_STP_N that means the incoming mode
     corresponds to the actual size of the memory being loaded/stored and the
     mode of the corresponding addressing mode is half of that.  */
  if (type == ADDR_QUERY_LDP_STP_N)
    {
      if (known_eq (GET_MODE_SIZE (mode), 32))
	mode = V16QImode;
      else if (known_eq (GET_MODE_SIZE (mode), 16))
	mode = DFmode;
      else if (known_eq (GET_MODE_SIZE (mode), 8))
	mode = SFmode;
      else
	return false;

      /* This isn't really an Advanced SIMD struct mode, but a mode
	 used to represent the complete mem in a load/store pair.  */
      advsimd_struct_p = false;
    }

  bool allow_reg_index_p = (!load_store_pair_p
			    && ((vec_flags == 0
				 && known_lt (GET_MODE_SIZE (mode), 16))
				|| vec_flags == VEC_ADVSIMD
				|| vec_flags & VEC_SVE_DATA));

  /* For SVE, only accept [Rn], [Rn, #offset, MUL VL] and [Rn, Rm, LSL #shift].
     The latter is not valid for SVE predicates, and that's rejected through
     allow_reg_index_p above.  */
  if ((vec_flags & (VEC_SVE_DATA | VEC_SVE_PRED)) != 0
      && (code != REG && code != PLUS))
    return false;

  /* On LE, for AdvSIMD, don't support anything other than POST_INC or
     REG addressing.  */
  if (advsimd_struct_p
      && TARGET_SIMD
      && !BYTES_BIG_ENDIAN
      && (code != POST_INC && code != REG))
    return false;

  gcc_checking_assert (GET_MODE (x) == VOIDmode
		       || SCALAR_INT_MODE_P (GET_MODE (x)));

  switch (code)
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG_IMM;
      info->base = x;
      info->offset = const0_rtx;
      info->const_offset = 0;
      return aarch64_base_register_rtx_p (x, strict_p);

    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (! strict_p
	  && REG_P (op0)
	  && virt_or_elim_regno_p (REGNO (op0))
	  && poly_int_rtx_p (op1, &offset))
	{
	  info->type = ADDRESS_REG_IMM;
	  info->base = op0;
	  info->offset = op1;
	  info->const_offset = offset;

	  return true;
	}

      if (maybe_ne (GET_MODE_SIZE (mode), 0)
	  && aarch64_base_register_rtx_p (op0, strict_p)
	  && poly_int_rtx_p (op1, &offset))
	{
	  info->type = ADDRESS_REG_IMM;
	  info->base = op0;
	  info->offset = op1;
	  info->const_offset = offset;

	  /* TImode, TFmode and TDmode values are allowed in both pairs of X
	     registers and individual Q registers.  The available
	     address modes are:
	     X,X: 7-bit signed scaled offset
	     Q:   9-bit signed offset
	     We conservatively require an offset representable in either mode.
	     When performing the check for pairs of X registers i.e.  LDP/STP
	     pass down DImode since that is the natural size of the LDP/STP
	     instruction memory accesses.  */
	  if (mode == TImode || mode == TFmode || mode == TDmode)
	    return (aarch64_offset_7bit_signed_scaled_p (DImode, offset)
		    && (aarch64_offset_9bit_signed_unscaled_p (mode, offset)
			|| offset_12bit_unsigned_scaled_p (mode, offset)));

	  if (mode == V8DImode)
	    return (aarch64_offset_7bit_signed_scaled_p (DImode, offset)
	            && aarch64_offset_7bit_signed_scaled_p (DImode, offset + 48));

	  /* A 7bit offset check because OImode will emit a ldp/stp
	     instruction (only !TARGET_SIMD or big endian will get here).
	     For ldp/stp instructions, the offset is scaled for the size of a
	     single element of the pair.  */
	  if (aarch64_advsimd_partial_struct_mode_p (mode)
	      && known_eq (GET_MODE_SIZE (mode), 16))
	    return aarch64_offset_7bit_signed_scaled_p (DImode, offset);
	  if (aarch64_advsimd_full_struct_mode_p (mode)
	      && known_eq (GET_MODE_SIZE (mode), 32))
	    return aarch64_offset_7bit_signed_scaled_p (TImode, offset);

	  /* Three 9/12 bit offsets checks because CImode will emit three
	     ldr/str instructions (only !TARGET_SIMD or big endian will
	     get here).  */
	  if (aarch64_advsimd_partial_struct_mode_p (mode)
	      && known_eq (GET_MODE_SIZE (mode), 24))
	    return (aarch64_offset_7bit_signed_scaled_p (DImode, offset)
		    && (aarch64_offset_9bit_signed_unscaled_p (DImode,
							       offset + 16)
			|| offset_12bit_unsigned_scaled_p (DImode,
							   offset + 16)));
	  if (aarch64_advsimd_full_struct_mode_p (mode)
	      && known_eq (GET_MODE_SIZE (mode), 48))
	    return (aarch64_offset_7bit_signed_scaled_p (TImode, offset)
		    && (aarch64_offset_9bit_signed_unscaled_p (TImode,
							       offset + 32)
			|| offset_12bit_unsigned_scaled_p (TImode,
							   offset + 32)));

	  /* Two 7bit offsets checks because XImode will emit two ldp/stp
	     instructions (only big endian will get here).  */
	  if (aarch64_advsimd_partial_struct_mode_p (mode)
	      && known_eq (GET_MODE_SIZE (mode), 32))
	    return (aarch64_offset_7bit_signed_scaled_p (DImode, offset)
		    && aarch64_offset_7bit_signed_scaled_p (DImode,
							    offset + 16));
	  if (aarch64_advsimd_full_struct_mode_p (mode)
	      && known_eq (GET_MODE_SIZE (mode), 64))
	    return (aarch64_offset_7bit_signed_scaled_p (TImode, offset)
		    && aarch64_offset_7bit_signed_scaled_p (TImode,
							    offset + 32));

	  /* Make "m" use the LD1 offset range for SVE data modes, so
	     that pre-RTL optimizers like ivopts will work to that
	     instead of the wider LDR/STR range.  */
	  if (vec_flags == VEC_SVE_DATA)
	    return (type == ADDR_QUERY_M
		    ? offset_4bit_signed_scaled_p (mode, offset)
		    : offset_9bit_signed_scaled_p (mode, offset));

	  if (vec_flags == (VEC_SVE_DATA | VEC_STRUCT))
	    {
	      poly_int64 end_offset = (offset
				       + GET_MODE_SIZE (mode)
				       - BYTES_PER_SVE_VECTOR);
	      return (type == ADDR_QUERY_M
		      ? offset_4bit_signed_scaled_p (mode, offset)
		      : (offset_9bit_signed_scaled_p (SVE_BYTE_MODE, offset)
			 && offset_9bit_signed_scaled_p (SVE_BYTE_MODE,
							 end_offset)));
	    }

	  if (vec_flags == VEC_SVE_PRED)
	    return offset_9bit_signed_scaled_p (mode, offset);

	  if (vec_flags == (VEC_SVE_PRED | VEC_STRUCT))
	    {
	      poly_int64 end_offset = (offset
				       + GET_MODE_SIZE (mode)
				       - BYTES_PER_SVE_PRED);
	      return (offset_9bit_signed_scaled_p (VNx16BImode, end_offset)
		      && offset_9bit_signed_scaled_p (VNx16BImode, offset));
	    }

	  if (load_store_pair_p)
	    return ((known_eq (GET_MODE_SIZE (mode), 4)
		     || known_eq (GET_MODE_SIZE (mode), 8)
		     || known_eq (GET_MODE_SIZE (mode), 16))
		    && aarch64_offset_7bit_signed_scaled_p (mode, offset));
	  else
	    return (aarch64_offset_9bit_signed_unscaled_p (mode, offset)
		    || offset_12bit_unsigned_scaled_p (mode, offset));
	}

      if (allow_reg_index_p)
	{
	  /* Look for base + (scaled/extended) index register.  */
	  if (aarch64_base_register_rtx_p (op0, strict_p)
	      && aarch64_classify_index (info, op1, mode, strict_p))
	    {
	      info->base = op0;
	      return true;
	    }
	  if (aarch64_base_register_rtx_p (op1, strict_p)
	      && aarch64_classify_index (info, op0, mode, strict_p))
	    {
	      info->base = op1;
	      return true;
	    }
	}

      return false;

    case POST_INC:
    case POST_DEC:
    case PRE_INC:
    case PRE_DEC:
      info->type = ADDRESS_REG_WB;
      info->base = XEXP (x, 0);
      info->offset = NULL_RTX;
      return aarch64_base_register_rtx_p (info->base, strict_p);

    case POST_MODIFY:
    case PRE_MODIFY:
      info->type = ADDRESS_REG_WB;
      info->base = XEXP (x, 0);
      if (GET_CODE (XEXP (x, 1)) == PLUS
	  && poly_int_rtx_p (XEXP (XEXP (x, 1), 1), &offset)
	  && rtx_equal_p (XEXP (XEXP (x, 1), 0), info->base)
	  && aarch64_base_register_rtx_p (info->base, strict_p))
	{
	  info->offset = XEXP (XEXP (x, 1), 1);
	  info->const_offset = offset;

	  /* TImode, TFmode and TDmode values are allowed in both pairs of X
	     registers and individual Q registers.  The available
	     address modes are:
	     X,X: 7-bit signed scaled offset
	     Q:   9-bit signed offset
	     We conservatively require an offset representable in either mode.
	   */
	  if (mode == TImode || mode == TFmode || mode == TDmode)
	    return (aarch64_offset_7bit_signed_scaled_p (mode, offset)
		    && aarch64_offset_9bit_signed_unscaled_p (mode, offset));

	  if (load_store_pair_p)
	    return ((known_eq (GET_MODE_SIZE (mode), 4)
		     || known_eq (GET_MODE_SIZE (mode), 8)
		     || known_eq (GET_MODE_SIZE (mode), 16))
		    && aarch64_offset_7bit_signed_scaled_p (mode, offset));
	  else
	    return aarch64_offset_9bit_signed_unscaled_p (mode, offset);
	}
      return false;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* load literal: pc-relative constant pool entry.  Only supported
         for SI mode or larger.  */
      info->type = ADDRESS_SYMBOLIC;

      if (!load_store_pair_p
	  && GET_MODE_SIZE (mode).is_constant (&const_size)
	  && const_size >= 4)
	{
	  poly_int64 offset;
	  rtx sym = strip_offset_and_salt (x, &offset);
	  return ((LABEL_REF_P (sym)
		   || (SYMBOL_REF_P (sym)
		       && CONSTANT_POOL_ADDRESS_P (sym)
		       && aarch64_pcrelative_literal_loads)));
	}
      return false;

    case LO_SUM:
      info->type = ADDRESS_LO_SUM;
      info->base = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      if (allow_reg_index_p
	  && aarch64_base_register_rtx_p (info->base, strict_p))
	{
	  poly_int64 offset;
	  HOST_WIDE_INT const_offset;
	  rtx sym = strip_offset_and_salt (info->offset, &offset);
	  if (SYMBOL_REF_P (sym)
	      && offset.is_constant (&const_offset)
	      && (aarch64_classify_symbol (sym, const_offset)
		  == SYMBOL_SMALL_ABSOLUTE))
	    {
	      /* The symbol and offset must be aligned to the access size.  */
	      unsigned int align;

	      if (CONSTANT_POOL_ADDRESS_P (sym))
		align = GET_MODE_ALIGNMENT (get_pool_mode (sym));
	      else if (TREE_CONSTANT_POOL_ADDRESS_P (sym))
		{
		  tree exp = SYMBOL_REF_DECL (sym);
		  align = TYPE_ALIGN (TREE_TYPE (exp));
		  align = aarch64_constant_alignment (exp, align);
		}
	      else if (SYMBOL_REF_DECL (sym))
		align = DECL_ALIGN (SYMBOL_REF_DECL (sym));
	      else if (SYMBOL_REF_HAS_BLOCK_INFO_P (sym)
		       && SYMBOL_REF_BLOCK (sym) != NULL)
		align = SYMBOL_REF_BLOCK (sym)->alignment;
	      else
		align = BITS_PER_UNIT;

	      poly_int64 ref_size = GET_MODE_SIZE (mode);
	      if (known_eq (ref_size, 0))
		ref_size = GET_MODE_SIZE (DImode);

	      return (multiple_p (const_offset, ref_size)
		      && multiple_p (align / BITS_PER_UNIT, ref_size));
	    }
	}
      return false;

    default:
      return false;
    }
}

/* Return true if the address X is valid for a PRFM instruction.
   STRICT_P is true if we should do strict checking with
   aarch64_classify_address.  */

bool
aarch64_address_valid_for_prefetch_p (rtx x, bool strict_p)
{
  struct aarch64_address_info addr;

  /* PRFM accepts the same addresses as DImode...  */
  bool res = aarch64_classify_address (&addr, x, DImode, strict_p);
  if (!res)
    return false;

  /* ... except writeback forms.  */
  return addr.type != ADDRESS_REG_WB;
}

bool
aarch64_symbolic_address_p (rtx x)
{
  poly_int64 offset;
  x = strip_offset_and_salt (x, &offset);
  return SYMBOL_REF_P (x) || LABEL_REF_P (x);
}

/* Classify the base of symbolic expression X.  */

enum aarch64_symbol_type
aarch64_classify_symbolic_expression (rtx x)
{
  rtx offset;

  split_const (x, &x, &offset);
  return aarch64_classify_symbol (x, INTVAL (offset));
}


/* Return TRUE if X is a legitimate address for accessing memory in
   mode MODE.  */
static bool
aarch64_legitimate_address_hook_p (machine_mode mode, rtx x, bool strict_p,
				   code_helper = ERROR_MARK)
{
  struct aarch64_address_info addr;

  return aarch64_classify_address (&addr, x, mode, strict_p);
}

/* Return TRUE if X is a legitimate address of type TYPE for accessing
   memory in mode MODE.  STRICT_P is true if REG_OK_STRICT is in effect.  */
bool
aarch64_legitimate_address_p (machine_mode mode, rtx x, bool strict_p,
			      aarch64_addr_query_type type)
{
  struct aarch64_address_info addr;

  return aarch64_classify_address (&addr, x, mode, strict_p, type);
}

/* Implement TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT.  */

static bool
aarch64_legitimize_address_displacement (rtx *offset1, rtx *offset2,
					 poly_int64 orig_offset,
					 machine_mode mode)
{
  HOST_WIDE_INT size;
  if (GET_MODE_SIZE (mode).is_constant (&size))
    {
      HOST_WIDE_INT const_offset, second_offset;

      /* A general SVE offset is A * VQ + B.  Remove the A component from
	 coefficient 0 in order to get the constant B.  */
      const_offset = orig_offset.coeffs[0] - orig_offset.coeffs[1];

      /* Split an out-of-range address displacement into a base and
	 offset.  Use 4KB range for 1- and 2-byte accesses and a 16KB
	 range otherwise to increase opportunities for sharing the base
	 address of different sizes.  Unaligned accesses use the signed
	 9-bit range, TImode/TFmode/TDmode use the intersection of signed
	 scaled 7-bit and signed 9-bit offset.  */
      if (mode == TImode || mode == TFmode || mode == TDmode)
	second_offset = ((const_offset + 0x100) & 0x1f8) - 0x100;
      else if ((const_offset & (size - 1)) != 0)
	second_offset = ((const_offset + 0x100) & 0x1ff) - 0x100;
      else
	second_offset = const_offset & (size < 4 ? 0xfff : 0x3ffc);

      if (second_offset == 0 || known_eq (orig_offset, second_offset))
	return false;

      /* Split the offset into second_offset and the rest.  */
      *offset1 = gen_int_mode (orig_offset - second_offset, Pmode);
      *offset2 = gen_int_mode (second_offset, Pmode);
      return true;
    }
  else
    {
      /* Get the mode we should use as the basis of the range.  For structure
	 modes this is the mode of one vector.  */
      unsigned int vec_flags = aarch64_classify_vector_mode (mode);
      machine_mode step_mode
	= (vec_flags & VEC_STRUCT) != 0 ? SVE_BYTE_MODE : mode;

      /* Get the "mul vl" multiplier we'd like to use.  */
      HOST_WIDE_INT factor = GET_MODE_SIZE (step_mode).coeffs[1];
      HOST_WIDE_INT vnum = orig_offset.coeffs[1] / factor;
      if (vec_flags & VEC_SVE_DATA)
	/* LDR supports a 9-bit range, but the move patterns for
	   structure modes require all vectors to be in range of the
	   same base.  The simplest way of accomodating that while still
	   promoting reuse of anchor points between different modes is
	   to use an 8-bit range unconditionally.  */
	vnum = ((vnum + 128) & 255) - 128;
      else
	/* Predicates are only handled singly, so we might as well use
	   the full range.  */
	vnum = ((vnum + 256) & 511) - 256;
      if (vnum == 0)
	return false;

      /* Convert the "mul vl" multiplier into a byte offset.  */
      poly_int64 second_offset = GET_MODE_SIZE (step_mode) * vnum;
      if (known_eq (second_offset, orig_offset))
	return false;

      /* Split the offset into second_offset and the rest.  */
      *offset1 = gen_int_mode (orig_offset - second_offset, Pmode);
      *offset2 = gen_int_mode (second_offset, Pmode);
      return true;
    }
}

/* Return the binary representation of floating point constant VALUE in INTVAL.
   If the value cannot be converted, return false without setting INTVAL.
   The conversion is done in the given MODE.  */
bool
aarch64_reinterpret_float_as_int (rtx value, unsigned HOST_WIDE_INT *intval)
{

  /* We make a general exception for 0.  */
  if (aarch64_float_const_zero_rtx_p (value))
    {
      *intval = 0;
      return true;
    }

  scalar_float_mode mode;
  if (!CONST_DOUBLE_P (value)
      || !is_a <scalar_float_mode> (GET_MODE (value), &mode)
      || GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT
      /* Only support up to DF mode.  */
      || GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (DFmode))
    return false;

  unsigned HOST_WIDE_INT ival = 0;

  long res[2];
  real_to_target (res,
		  CONST_DOUBLE_REAL_VALUE (value),
		  REAL_MODE_FORMAT (mode));

  if (mode == DFmode || mode == DDmode)
    {
      int order = BYTES_BIG_ENDIAN ? 1 : 0;
      ival = zext_hwi (res[order], 32);
      ival |= (zext_hwi (res[1 - order], 32) << 32);
    }
  else
      ival = zext_hwi (res[0], 32);

  *intval = ival;
  return true;
}

/* Return TRUE if rtx X is an immediate constant that can be moved using a
   single MOV(+MOVK) followed by an FMOV.  */
bool
aarch64_float_const_rtx_p (rtx x)
{
  machine_mode mode = GET_MODE (x);
  if (mode == VOIDmode)
    return false;

  /* Determine whether it's cheaper to write float constants as
     mov/movk pairs over ldr/adrp pairs.  */
  unsigned HOST_WIDE_INT ival;

  if (CONST_DOUBLE_P (x)
      && SCALAR_FLOAT_MODE_P (mode)
      && aarch64_reinterpret_float_as_int (x, &ival))
    {
      machine_mode imode = known_eq (GET_MODE_SIZE (mode), 8) ? DImode : SImode;
      int num_instr = aarch64_internal_mov_immediate
			(NULL_RTX, gen_int_mode (ival, imode), false, imode);
      return num_instr < 3;
    }

  return false;
}

/* Return TRUE if rtx X is immediate constant 0.0 (but not in Decimal
   Floating Point).  */
bool
aarch64_float_const_zero_rtx_p (rtx x)
{
  /* 0.0 in Decimal Floating Point cannot be represented by #0 or
     zr as our callers expect, so no need to check the actual
     value if X is of Decimal Floating Point type.  */
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_DECIMAL_FLOAT)
    return false;

  if (REAL_VALUE_MINUS_ZERO (*CONST_DOUBLE_REAL_VALUE (x)))
    return !HONOR_SIGNED_ZEROS (GET_MODE (x));
  return real_equal (CONST_DOUBLE_REAL_VALUE (x), &dconst0);
}

/* Return true if X is any kind of constant zero rtx.  */

bool
aarch64_const_zero_rtx_p (rtx x)
{
  return (x == CONST0_RTX (GET_MODE (x))
	  || (CONST_DOUBLE_P (x) && aarch64_float_const_zero_rtx_p (x)));
}

/* Return TRUE if rtx X is immediate constant that fits in a single
   MOVI immediate operation.  */
bool
aarch64_can_const_movi_rtx_p (rtx x, machine_mode mode)
{
  if (!TARGET_SIMD)
     return false;

  machine_mode vmode;
  scalar_int_mode imode;
  unsigned HOST_WIDE_INT ival;

  if (CONST_DOUBLE_P (x)
      && SCALAR_FLOAT_MODE_P (mode))
    {
      if (!aarch64_reinterpret_float_as_int (x, &ival))
	return false;

      /* We make a general exception for 0.  */
      if (aarch64_float_const_zero_rtx_p (x))
	return true;

      imode = int_mode_for_mode (mode).require ();
    }
  else if (CONST_INT_P (x)
	   && is_a <scalar_int_mode> (mode, &imode))
    ival = INTVAL (x);
  else
    return false;

   /* use a 64 bit mode for everything except for DI/DF/DD mode, where we use
     a 128 bit vector mode.  */
  int width = GET_MODE_BITSIZE (imode) == 64 ? 128 : 64;

  vmode = aarch64_simd_container_mode (imode, width);
  rtx v_op = aarch64_simd_gen_const_vector_dup (vmode, ival);

  return aarch64_simd_valid_mov_imm (v_op);
}


/* Return the fixed registers used for condition codes.  */

static bool
aarch64_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REGNUM;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Return a fresh memory reference to the current function's TPIDR2 block,
   creating a block if necessary.  */

static rtx
aarch64_get_tpidr2_block ()
{
  if (!cfun->machine->tpidr2_block)
    /* The TPIDR2 block is 16 bytes in size and must be aligned to a 128-bit
       boundary.  */
    cfun->machine->tpidr2_block = assign_stack_local (V16QImode, 16, 128);
  return copy_rtx (cfun->machine->tpidr2_block);
}

/* Return a fresh register that points to the current function's
   TPIDR2 block, creating a block if necessary.  */

static rtx
aarch64_get_tpidr2_ptr ()
{
  rtx block = aarch64_get_tpidr2_block ();
  return force_reg (Pmode, XEXP (block, 0));
}

/* Emit instructions to allocate a ZA lazy save buffer and initialize the
   current function's TPIDR2 block.  */

static void
aarch64_init_tpidr2_block ()
{
  rtx block = aarch64_get_tpidr2_block ();

  /* The ZA save buffer is SVL.B*SVL.B bytes in size.  */
  rtx svl_bytes = aarch64_sme_vq_immediate (Pmode, 16, AARCH64_ISA_MODE);
  rtx svl_bytes_reg = force_reg (DImode, svl_bytes);
  rtx za_size = expand_simple_binop (Pmode, MULT, svl_bytes_reg,
				     svl_bytes_reg, NULL, 0, OPTAB_LIB_WIDEN);
  rtx za_save_buffer = allocate_dynamic_stack_space (za_size, 128,
						     BITS_PER_UNIT, -1, true);
  za_save_buffer = force_reg (Pmode, za_save_buffer);
  cfun->machine->za_save_buffer = za_save_buffer;

  /* The first word of the block points to the save buffer and the second
     word is the number of ZA slices to save.  */
  rtx block_0 = adjust_address (block, DImode, 0);
  emit_insn (aarch64_gen_store_pair (block_0, za_save_buffer, svl_bytes_reg));

  if (!memory_operand (block, V16QImode))
    block = replace_equiv_address (block, force_reg (Pmode, XEXP (block, 0)));
  emit_insn (gen_aarch64_setup_local_tpidr2 (block));
}

/* Restore the contents of ZA from the lazy save buffer, given that
   register TPIDR2_BLOCK points to the current function's TPIDR2 block.
   PSTATE.ZA is known to be 0 and TPIDR2_EL0 is known to be null.  */

void
aarch64_restore_za (rtx tpidr2_block)
{
  emit_insn (gen_aarch64_smstart_za ());
  if (REGNO (tpidr2_block) != R0_REGNUM)
    emit_move_insn (gen_rtx_REG (Pmode, R0_REGNUM), tpidr2_block);
  emit_insn (gen_aarch64_tpidr2_restore ());
}

/* Return the ZT0 save buffer, creating one if necessary.  */

static rtx
aarch64_get_zt0_save_buffer ()
{
  if (!cfun->machine->zt0_save_buffer)
    cfun->machine->zt0_save_buffer = assign_stack_local (V8DImode, 64, 128);
  return cfun->machine->zt0_save_buffer;
}

/* Save ZT0 to the current function's save buffer.  */

static void
aarch64_save_zt0 ()
{
  rtx mem = aarch64_get_zt0_save_buffer ();
  mem = replace_equiv_address (mem, force_reg (Pmode, XEXP (mem, 0)));
  emit_insn (gen_aarch64_sme_str_zt0 (mem));
}

/* Restore ZT0 from the current function's save buffer.  FROM_LAZY_SAVE_P
   is true if the load is happening after a call to a private-ZA function,
   false if it can be treated as a normal load.  */

static void
aarch64_restore_zt0 (bool from_lazy_save_p)
{
  rtx mem = aarch64_get_zt0_save_buffer ();
  mem = replace_equiv_address (mem, force_reg (Pmode, XEXP (mem, 0)));
  emit_insn (from_lazy_save_p
	     ? gen_aarch64_restore_zt0 (mem)
	     : gen_aarch64_sme_ldr_zt0 (mem));
}

/* Implement TARGET_START_CALL_ARGS.  */

static void
aarch64_start_call_args (cumulative_args_t ca_v)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  if (!TARGET_SME && (ca->isa_mode & AARCH64_ISA_MODE_SM_ON))
    {
      error ("calling a streaming function requires the ISA extension %qs",
	     "sme");
      inform (input_location, "you can enable %qs using the command-line"
	      " option %<-march%>, or by using the %<target%>"
	      " attribute or pragma", "sme");
    }

  if ((ca->shared_za_flags & (AARCH64_STATE_IN | AARCH64_STATE_OUT))
      && !aarch64_cfun_has_state ("za"))
    error ("call to a function that shares %qs state from a function"
	   " that has no %qs state", "za", "za");
  else if ((ca->shared_zt0_flags & (AARCH64_STATE_IN | AARCH64_STATE_OUT))
	   && !aarch64_cfun_has_state ("zt0"))
    error ("call to a function that shares %qs state from a function"
	   " that has no %qs state", "zt0", "zt0");
  else if (!TARGET_ZA && (ca->isa_mode & AARCH64_ISA_MODE_ZA_ON))
    error ("call to a function that shares SME state from a function"
	   " that has no SME state");

  /* If this is a call to a private ZA function, emit a marker to
     indicate where any necessary set-up code could be inserted.
     The code itself is inserted by the mode-switching pass.  */
  if (TARGET_ZA && !(ca->isa_mode & AARCH64_ISA_MODE_ZA_ON))
    emit_insn (gen_aarch64_start_private_za_call ());

  /* If this is a call to a shared-ZA function that doesn't share ZT0,
     save and restore ZT0 around the call.  */
  if (aarch64_cfun_has_state ("zt0")
      && (ca->isa_mode & AARCH64_ISA_MODE_ZA_ON)
      && ca->shared_zt0_flags == 0)
    aarch64_save_zt0 ();
}

/* This function is used by the call expanders of the machine description.
   RESULT is the register in which the result is returned.  It's NULL for
   "call" and "sibcall".
   MEM is the location of the function call.
   COOKIE is either:
     - a const_int that gives the argument to the call's UNSPEC_CALLEE_ABI.
     - a PARALLEL that contains such a const_int as its first element.
       The second element is a PARALLEL that lists all the argument
       registers that need to be saved and restored around a change
       in PSTATE.SM, or const0_rtx if no such switch is needed.
       The third and fourth elements are const_ints that contain the
       sharing flags for ZA and ZT0 respectively.
   SIBCALL indicates whether this function call is normal call or sibling call.
   It will generate different pattern accordingly.  */

void
aarch64_expand_call (rtx result, rtx mem, rtx cookie, bool sibcall)
{
  rtx call, callee, tmp;
  rtvec vec;
  machine_mode mode;

  rtx callee_abi = cookie;
  rtx sme_mode_switch_args = const0_rtx;
  unsigned int shared_za_flags = 0;
  unsigned int shared_zt0_flags = 0;
  if (GET_CODE (cookie) == PARALLEL)
    {
      callee_abi = XVECEXP (cookie, 0, 0);
      sme_mode_switch_args = XVECEXP (cookie, 0, 1);
      shared_za_flags = INTVAL (XVECEXP (cookie, 0, 2));
      shared_zt0_flags = INTVAL (XVECEXP (cookie, 0, 3));
    }

  gcc_assert (CONST_INT_P (callee_abi));
  auto callee_isa_mode = aarch64_callee_isa_mode (callee_abi);

  if (aarch64_cfun_has_state ("za")
      && (callee_isa_mode & AARCH64_ISA_MODE_ZA_ON)
      && !shared_za_flags)
    {
      sorry ("call to a function that shares state other than %qs"
	     " from a function that has %qs state", "za", "za");
      inform (input_location, "use %<__arm_preserves(\"za\")%> if the"
	      " callee preserves ZA");
    }

  gcc_assert (MEM_P (mem));
  callee = XEXP (mem, 0);

#if TARGET_PECOFF
  tmp = legitimize_pe_coff_symbol (callee, false);
  if (tmp)
    callee = tmp;
#endif

  mode = GET_MODE (callee);
  gcc_assert (mode == Pmode);

  /* Decide if we should generate indirect calls by loading the
     address of the callee into a register before performing
     the branch-and-link.  */
  if (SYMBOL_REF_P (callee)
      ? (aarch64_is_long_call_p (callee)
	 || aarch64_is_noplt_call_p (callee))
      : !REG_P (callee))
    XEXP (mem, 0) = force_reg (mode, callee);

  /* Accumulate the return values, including state that is shared via
     attributes.  */
  auto_vec<rtx, 8> return_values;
  if (result)
    {
      if (GET_CODE (result) == PARALLEL)
	for (int i = 0; i < XVECLEN (result, 0); ++i)
	  return_values.safe_push (XVECEXP (result, 0, i));
      else
	return_values.safe_push (result);
    }
  unsigned int orig_num_return_values = return_values.length ();
  if (shared_za_flags & AARCH64_STATE_OUT)
    return_values.safe_push (gen_rtx_REG (VNx16BImode, ZA_REGNUM));
  /* When calling private-ZA functions from functions with ZA state,
     we want to know whether the call committed a lazy save.  */
  if (TARGET_ZA && !shared_za_flags)
    return_values.safe_push (gen_rtx_REG (VNx16BImode, ZA_SAVED_REGNUM));
  if (shared_zt0_flags & AARCH64_STATE_OUT)
    return_values.safe_push (gen_rtx_REG (V8DImode, ZT0_REGNUM));

  /* Create the new return value, if necessary.  */
  if (orig_num_return_values != return_values.length ())
    {
      if (return_values.length () == 1)
	result = return_values[0];
      else
	{
	  for (rtx &x : return_values)
	    if (GET_CODE (x) != EXPR_LIST)
	      x = gen_rtx_EXPR_LIST (VOIDmode, x, const0_rtx);
	  rtvec v = gen_rtvec_v (return_values.length (),
				 return_values.address ());
	  result = gen_rtx_PARALLEL (VOIDmode, v);
	}
    }

  call = gen_rtx_CALL (VOIDmode, mem, const0_rtx);

  if (result != NULL_RTX)
    call = gen_rtx_SET (result, call);

  if (sibcall)
    tmp = ret_rtx;
  else
    tmp = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, LR_REGNUM));

  callee_abi = gen_rtx_UNSPEC (DImode, gen_rtvec (1, callee_abi),
			       UNSPEC_CALLEE_ABI);

  vec = gen_rtvec (3, call, callee_abi, tmp);
  call = gen_rtx_PARALLEL (VOIDmode, vec);

  auto call_insn = aarch64_emit_call_insn (call);

  /* Check whether the call requires a change to PSTATE.SM.  We can't
     emit the instructions to change PSTATE.SM yet, since they involve
     a change in vector length and a change in instruction set, which
     cannot be represented in RTL.

     For now, just record which registers will be clobbered and used
     by the changes to PSTATE.SM.  */
  if (!sibcall && aarch64_call_switches_pstate_sm (callee_isa_mode))
    {
      aarch64_sme_mode_switch_regs args_switch;
      if (sme_mode_switch_args != const0_rtx)
	{
	  unsigned int num_args = XVECLEN (sme_mode_switch_args, 0);
	  for (unsigned int i = 0; i < num_args; ++i)
	    {
	      rtx x = XVECEXP (sme_mode_switch_args, 0, i);
	      args_switch.add_reg (GET_MODE (x), REGNO (x));
	    }
	}

      aarch64_sme_mode_switch_regs result_switch;
      if (result)
	result_switch.add_call_result (call_insn);

      unsigned int num_gprs = MAX (args_switch.num_gprs (),
				   result_switch.num_gprs ());
      for (unsigned int i = 0; i < num_gprs; ++i)
	clobber_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		     gen_rtx_REG (DImode, args_switch.FIRST_GPR + i));

      for (int regno = V0_REGNUM; regno < V0_REGNUM + 32; regno += 4)
	clobber_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		     gen_rtx_REG (V4x16QImode, regno));

      for (int regno = P0_REGNUM; regno < P0_REGNUM + 16; regno += 1)
	clobber_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		     gen_rtx_REG (VNx16BImode, regno));

      /* Ensure that the VG save slot has been initialized.  Also emit
	 an instruction to model the effect of the temporary clobber
	 of VG, so that the prologue/epilogue pass sees the need to
	 save the old value.  */
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
	       gen_rtx_REG (DImode, VG_REGNUM));
      emit_insn_before (gen_aarch64_update_vg (), call_insn);

      cfun->machine->call_switches_pstate_sm = true;
    }

  /* Add any ZA-related information.

     ZA_REGNUM represents the current function's ZA state, rather than
     the contents of the ZA register itself.  We ensure that the function's
     ZA state is preserved by private-ZA call sequences, so the call itself
     does not use or clobber ZA_REGNUM.  The same thing applies to
     ZT0_REGNUM.  */
  if (TARGET_ZA)
    {
      /* The callee requires ZA to be active if the callee is shared-ZA,
	 otherwise it requires ZA to be dormant or off.  The state of ZA is
	 captured by a combination of SME_STATE_REGNUM, TPIDR2_SETUP_REGNUM,
	 and ZA_SAVED_REGNUM.  */
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
	       gen_rtx_REG (DImode, SME_STATE_REGNUM));
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
	       gen_rtx_REG (DImode, TPIDR2_SETUP_REGNUM));
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
	       gen_rtx_REG (VNx16BImode, ZA_SAVED_REGNUM));

      /* Keep the aarch64_start/end_private_za_call markers live.  */
      if (!(callee_isa_mode & AARCH64_ISA_MODE_ZA_ON))
	use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		 gen_rtx_REG (VNx16BImode, LOWERING_REGNUM));

      /* If the callee is a shared-ZA function, record whether it uses the
	 current value of ZA and ZT0.  */
      if (shared_za_flags & AARCH64_STATE_IN)
	use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		 gen_rtx_REG (VNx16BImode, ZA_REGNUM));

      if (shared_zt0_flags & AARCH64_STATE_IN)
	use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		 gen_rtx_REG (V8DImode, ZT0_REGNUM));
    }
}

/* Implement TARGET_END_CALL_ARGS.  */

static void
aarch64_end_call_args (cumulative_args_t ca_v)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  /* If this is a call to a private ZA function, emit a marker to
     indicate where any necessary restoration code could be inserted.
     The code itself is inserted by the mode-switching pass.  */
  if (TARGET_ZA && !(ca->isa_mode & AARCH64_ISA_MODE_ZA_ON))
    emit_insn (gen_aarch64_end_private_za_call ());

  /* If this is a call to a shared-ZA function that doesn't share ZT0,
     save and restore ZT0 around the call.  */
  if (aarch64_cfun_has_state ("zt0")
      && (ca->isa_mode & AARCH64_ISA_MODE_ZA_ON)
      && ca->shared_zt0_flags == 0)
    aarch64_restore_zt0 (false);
}

/* Emit call insn with PAT and do aarch64-specific handling.  */

rtx_call_insn *
aarch64_emit_call_insn (rtx pat)
{
  auto insn = emit_call_insn (pat);

  rtx *fusage = &CALL_INSN_FUNCTION_USAGE (insn);
  clobber_reg (fusage, gen_rtx_REG (word_mode, IP0_REGNUM));
  clobber_reg (fusage, gen_rtx_REG (word_mode, IP1_REGNUM));
  return as_a<rtx_call_insn *> (insn);
}

machine_mode
aarch64_select_cc_mode (RTX_CODE code, rtx x, rtx y)
{
  machine_mode mode_x = GET_MODE (x);
  rtx_code code_x = GET_CODE (x);

  /* All floating point compares return CCFP if it is an equality
     comparison, and CCFPE otherwise.  */
  if (GET_MODE_CLASS (mode_x) == MODE_FLOAT)
    {
      switch (code)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	case LTGT:
	  return CCFPEmode;

	default:
	  gcc_unreachable ();
	}
    }

  /* Equality comparisons of short modes against zero can be performed
     using the TST instruction with the appropriate bitmask.  */
  if (y == const0_rtx && (REG_P (x) || SUBREG_P (x))
      && (code == EQ || code == NE)
      && (mode_x == HImode || mode_x == QImode))
    return CC_Zmode;

  /* Similarly, comparisons of zero_extends from shorter modes can
     be performed using an ANDS with an immediate mask.  */
  if (y == const0_rtx && code_x == ZERO_EXTEND
      && (mode_x == SImode || mode_x == DImode)
      && (GET_MODE (XEXP (x, 0)) == HImode || GET_MODE (XEXP (x, 0)) == QImode)
      && (code == EQ || code == NE))
    return CC_Zmode;

  /* Zero extracts support equality comparisons.  */
  if ((mode_x == SImode || mode_x == DImode)
      && y == const0_rtx
      && (code_x == ZERO_EXTRACT && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (x, 2)))
      && (code == EQ || code == NE))
    return CC_Zmode;

  /* ANDS/BICS/TST support equality and all signed comparisons.  */
  if ((mode_x == SImode || mode_x == DImode)
      && y == const0_rtx
      && (code_x == AND)
      && (code == EQ || code == NE || code == LT || code == GE
	  || code == GT || code == LE))
    return CC_NZVmode;

  /* ADDS/SUBS correctly set N and Z flags.  */
  if ((mode_x == SImode || mode_x == DImode)
      && y == const0_rtx
      && (code == EQ || code == NE || code == LT || code == GE)
      && (code_x == PLUS || code_x == MINUS || code_x == NEG))
    return CC_NZmode;

  /* A compare with a shifted operand.  Because of canonicalization,
     the comparison will have to be swapped when we emit the assembly
     code.  */
  if ((mode_x == SImode || mode_x == DImode)
      && (REG_P (y) || SUBREG_P (y) || y == const0_rtx)
      && (code_x == ASHIFT || code_x == ASHIFTRT
	  || code_x == LSHIFTRT
	  || code_x == ZERO_EXTEND || code_x == SIGN_EXTEND))
    return CC_SWPmode;

  /* Similarly for a negated operand, but we can only do this for
     equalities.  */
  if ((mode_x == SImode || mode_x == DImode)
      && (REG_P (y) || SUBREG_P (y))
      && (code == EQ || code == NE)
      && code_x == NEG)
    return CC_Zmode;

  /* A test for unsigned overflow from an addition.  */
  if ((mode_x == DImode || mode_x == TImode)
      && (code == LTU || code == GEU)
      && code_x == PLUS
      && rtx_equal_p (XEXP (x, 0), y))
    return CC_Cmode;

  /* A test for unsigned overflow from an add with carry.  */
  if ((mode_x == DImode || mode_x == TImode)
      && (code == LTU || code == GEU)
      && code_x == PLUS
      && CONST_SCALAR_INT_P (y)
      && (rtx_mode_t (y, mode_x)
	  == (wi::shwi (1, mode_x)
	      << (GET_MODE_BITSIZE (mode_x).to_constant () / 2))))
    return CC_ADCmode;

  /* A test for signed overflow.  */
  if ((mode_x == DImode || mode_x == TImode)
      && code == NE
      && code_x == PLUS
      && GET_CODE (y) == SIGN_EXTEND)
    return CC_Vmode;

  /* For everything else, return CCmode.  */
  return CCmode;
}

static int
aarch64_get_condition_code_1 (machine_mode, enum rtx_code);

int
aarch64_get_condition_code (rtx x)
{
  machine_mode mode = GET_MODE (XEXP (x, 0));
  enum rtx_code comp_code = GET_CODE (x);

  if (GET_MODE_CLASS (mode) != MODE_CC)
    mode = SELECT_CC_MODE (comp_code, XEXP (x, 0), XEXP (x, 1));
  return aarch64_get_condition_code_1 (mode, comp_code);
}

static int
aarch64_get_condition_code_1 (machine_mode mode, enum rtx_code comp_code)
{
  switch (mode)
    {
    case E_CCFPmode:
    case E_CCFPEmode:
      switch (comp_code)
	{
	case GE: return AARCH64_GE;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LS;
	case LT: return AARCH64_MI;
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case ORDERED: return AARCH64_VC;
	case UNORDERED: return AARCH64_VS;
	case UNLT: return AARCH64_LT;
	case UNLE: return AARCH64_LE;
	case UNGT: return AARCH64_HI;
	case UNGE: return AARCH64_PL;
	default: return -1;
	}
      break;

    case E_CCmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_GE;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LE;
	case LT: return AARCH64_LT;
	case GEU: return AARCH64_CS;
	case GTU: return AARCH64_HI;
	case LEU: return AARCH64_LS;
	case LTU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case E_CC_SWPmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_LE;
	case GT: return AARCH64_LT;
	case LE: return AARCH64_GE;
	case LT: return AARCH64_GT;
	case GEU: return AARCH64_LS;
	case GTU: return AARCH64_CC;
	case LEU: return AARCH64_CS;
	case LTU: return AARCH64_HI;
	default: return -1;
	}
      break;

    case E_CC_NZCmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE; /* = any */
	case EQ: return AARCH64_EQ; /* = none */
	case GE: return AARCH64_PL; /* = nfrst */
	case LT: return AARCH64_MI; /* = first */
	case GEU: return AARCH64_CS; /* = nlast */
	case GTU: return AARCH64_HI; /* = pmore */
	case LEU: return AARCH64_LS; /* = plast */
	case LTU: return AARCH64_CC; /* = last */
	default: return -1;
	}
      break;

    case E_CC_NZVmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_PL;
	case LT: return AARCH64_MI;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LE;
	default: return -1;
	}
      break;

    case E_CC_NZmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_PL;
	case LT: return AARCH64_MI;
	default: return -1;
	}
      break;

    case E_CC_Zmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	default: return -1;
	}
      break;

    case E_CC_Cmode:
      switch (comp_code)
	{
	case LTU: return AARCH64_CS;
	case GEU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case E_CC_ADCmode:
      switch (comp_code)
	{
	case GEU: return AARCH64_CS;
	case LTU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case E_CC_Vmode:
      switch (comp_code)
	{
	case NE: return AARCH64_VS;
	case EQ: return AARCH64_VC;
	default: return -1;
	}
      break;

    default:
      return -1;
    }

  return -1;
}

/* Return true if X is a CONST_INT, CONST_WIDE_INT or a constant vector
   duplicate of such constants.  If so, store in RET_WI the wide_int
   representation of the constant paired with the inner mode of the vector mode
   or MODE for scalar X constants.  If MODE is not provided then TImode is
   used.  */

static bool
aarch64_extract_vec_duplicate_wide_int (rtx x, wide_int *ret_wi,
					scalar_mode mode = TImode)
{
  rtx elt = unwrap_const_vec_duplicate (x);
  if (!CONST_SCALAR_INT_P (elt))
    return false;
  scalar_mode smode
    = CONST_SCALAR_INT_P (x) ? mode : GET_MODE_INNER (GET_MODE (x));
  *ret_wi = rtx_mode_t (elt, smode);
  return true;
}

/* Return true if X is a scalar or a constant vector of integer
   immediates that represent the rounding constant used in the fixed-point
   arithmetic instructions.
   The accepted form of the constant is (1 << (C - 1)) where C is in the range
   [1, MODE_WIDTH/2].  */

bool
aarch64_rnd_imm_p (rtx x)
{
  wide_int rnd_cst;
  if (!aarch64_extract_vec_duplicate_wide_int (x, &rnd_cst))
    return false;
  int log2 = wi::exact_log2 (rnd_cst);
  if (log2 < 0)
    return false;
  return IN_RANGE (log2, 0, rnd_cst.get_precision () / 2 - 1);
}

/* Return true if RND is a constant vector of integer rounding constants
   corresponding to a constant vector of shifts, SHIFT.
   The relationship should be RND == (1 << (SHIFT - 1)).  */

bool
aarch64_const_vec_rnd_cst_p (rtx rnd, rtx shift)
{
  wide_int rnd_cst, shft_cst;
  if (!aarch64_extract_vec_duplicate_wide_int (rnd, &rnd_cst)
      || !aarch64_extract_vec_duplicate_wide_int (shift, &shft_cst))
    return false;

  return rnd_cst == (wi::shwi (1, rnd_cst.get_precision ()) << (shft_cst - 1));
}

bool
aarch64_const_vec_all_same_in_range_p (rtx x,
				       HOST_WIDE_INT minval,
				       HOST_WIDE_INT maxval)
{
  rtx elt;
  return (const_vec_duplicate_p (x, &elt)
	  && CONST_INT_P (elt)
	  && IN_RANGE (INTVAL (elt), minval, maxval));
}

/* Some constants can't be made using normal mov instructions in Advanced SIMD
   but we can still create them in various ways.  If the constant in VAL can be
   created using alternate methods then if possible then return true and
   additionally set TARGET to the rtx for the sequence if TARGET is not NULL.
   Otherwise return false if sequence is not possible.  */

bool
aarch64_maybe_generate_simd_constant (rtx target, rtx val, machine_mode mode)
{
  wide_int wval;
  auto smode = GET_MODE_INNER (mode);
  if (!aarch64_extract_vec_duplicate_wide_int (val, &wval, smode))
    return false;

  /* For Advanced SIMD we can create an integer with only the top bit set
     using fneg (0.0f).  */
  if (TARGET_SIMD
      && !TARGET_SVE
      && smode == DImode
      && wi::only_sign_bit_p (wval))
    {
      if (!target)
	return true;

      /* Use the same base type as aarch64_gen_shareable_zero.  */
      rtx zero = CONST0_RTX (V4SImode);
      emit_move_insn (lowpart_subreg (V4SImode, target, mode), zero);
      rtx neg = lowpart_subreg (V2DImode, target, mode);
      emit_insn (gen_aarch64_fnegv2di2 (neg, copy_rtx (neg)));
      return true;
    }

  return false;
}

/* Check if the value in VAL with mode MODE can be created using special
   instruction sequences.  */

bool aarch64_simd_special_constant_p (rtx val, machine_mode mode)
{
  return aarch64_maybe_generate_simd_constant (NULL_RTX, val, mode);
}

bool
aarch64_const_vec_all_same_int_p (rtx x, HOST_WIDE_INT val)
{
  return aarch64_const_vec_all_same_in_range_p (x, val, val);
}

/* Return true if VEC is a constant in which every element is in the range
   [MINVAL, MAXVAL].  The elements do not need to have the same value.  */

static bool
aarch64_const_vec_all_in_range_p (rtx vec,
				  HOST_WIDE_INT minval,
				  HOST_WIDE_INT maxval)
{
  if (!CONST_VECTOR_P (vec)
      || GET_MODE_CLASS (GET_MODE (vec)) != MODE_VECTOR_INT)
    return false;

  int nunits;
  if (!CONST_VECTOR_STEPPED_P (vec))
    nunits = const_vector_encoded_nelts (vec);
  else if (!CONST_VECTOR_NUNITS (vec).is_constant (&nunits))
    return false;

  for (int i = 0; i < nunits; i++)
    {
      rtx vec_elem = CONST_VECTOR_ELT (vec, i);
      if (!CONST_INT_P (vec_elem)
	  || !IN_RANGE (INTVAL (vec_elem), minval, maxval))
	return false;
    }
  return true;
}

/* N Z C V.  */
#define AARCH64_CC_V 1
#define AARCH64_CC_C (1 << 1)
#define AARCH64_CC_Z (1 << 2)
#define AARCH64_CC_N (1 << 3)

/* N Z C V flags for ccmp.  Indexed by AARCH64_COND_CODE.  */
static const int aarch64_nzcv_codes[] =
{
  0,		/* EQ, Z == 1.  */
  AARCH64_CC_Z,	/* NE, Z == 0.  */
  0,		/* CS, C == 1.  */
  AARCH64_CC_C,	/* CC, C == 0.  */
  0,		/* MI, N == 1.  */
  AARCH64_CC_N, /* PL, N == 0.  */
  0,		/* VS, V == 1.  */
  AARCH64_CC_V, /* VC, V == 0.  */
  0,		/* HI, C ==1 && Z == 0.  */
  AARCH64_CC_C,	/* LS, !(C == 1 && Z == 0).  */
  AARCH64_CC_V,	/* GE, N == V.  */
  0,		/* LT, N != V.  */
  AARCH64_CC_Z, /* GT, Z == 0 && N == V.  */
  0,		/* LE, !(Z == 0 && N == V).  */
  0,		/* AL, Any.  */
  0		/* NV, Any.  */
};

/* Print floating-point vector immediate operand X to F, negating it
   first if NEGATE is true.  Return true on success, false if it isn't
   a constant we can handle.  */

static bool
aarch64_print_vector_float_operand (FILE *f, rtx x, bool negate)
{
  rtx elt;

  if (!const_vec_duplicate_p (x, &elt))
    return false;

  REAL_VALUE_TYPE r = *CONST_DOUBLE_REAL_VALUE (elt);
  if (negate)
    r = real_value_negate (&r);

  /* Handle the SVE single-bit immediates specially, since they have a
     fixed form in the assembly syntax.  */
  if (real_equal (&r, &dconst0))
    asm_fprintf (f, "0.0");
  else if (real_equal (&r, &dconst2))
    asm_fprintf (f, "2.0");
  else if (real_equal (&r, &dconst1))
    asm_fprintf (f, "1.0");
  else if (real_equal (&r, &dconsthalf))
    asm_fprintf (f, "0.5");
  else
    {
      const int buf_size = 20;
      char float_buf[buf_size] = {'\0'};
      real_to_decimal_for_mode (float_buf, &r, buf_size, buf_size,
				1, GET_MODE (elt));
      asm_fprintf (f, "%s", float_buf);
    }

  return true;
}

/* Return the equivalent letter for size.  */
static char
sizetochar (int size)
{
  switch (size)
    {
    case 64: return 'd';
    case 32: return 's';
    case 16: return 'h';
    case 8:  return 'b';
    default: gcc_unreachable ();
    }
}

/* Print operand X to file F in a target specific manner according to CODE.
   The acceptable formatting commands given by CODE are:
     'c':		An integer or symbol address without a preceding #
			sign.
     'C':		Take the duplicated element in a vector constant
			and print it in hex.
     'D':		Take the duplicated element in a vector constant
			and print it as an unsigned integer, in decimal.
     'e':		Print the sign/zero-extend size as a character 8->b,
			16->h, 32->w.  Can also be used for masks:
			0xff->b, 0xffff->h, 0xffffffff->w.
     'I':		If the operand is a duplicated vector constant,
			replace it with the duplicated scalar.  If the
			operand is then a floating-point constant, replace
			it with the integer bit representation.  Print the
			transformed constant as a signed decimal number.
     'p':		Prints N such that 2^N == X (X must be power of 2 and
			const int).
     'P':		Print the number of non-zero bits in X (a const_int).
     'H':		Print the higher numbered register of a pair (TImode)
			of regs.
     'm':		Print a condition (eq, ne, etc).
     'M':		Same as 'm', but invert condition.
     'N':		Take the duplicated element in a vector constant
			and print the negative of it in decimal.
     'b/h/s/d/q':	Print a scalar FP/SIMD register name.
     'Z':		Same for SVE registers.  ('z' was already taken.)
			Note that it is not necessary to use %Z for operands
			that have SVE modes.  The convention is to use %Z
			only for non-SVE (or potentially non-SVE) modes.
     'S/T/U/V':		Print a FP/SIMD register name for a register list.
			The register printed is the FP/SIMD register name
			of X + 0/1/2/3 for S/T/U/V.
     'R':		Print a scalar Integer/FP/SIMD register name + 1.
     'X':		Print bottom 16 bits of integer constant in hex.
     'w/x':		Print a general register name or the zero register
			(32-bit or 64-bit).
     '0':		Print a normal operand, if it's a general register,
			then we assume DImode.
     'k':		Print NZCV for conditional compare instructions.
     'K':		Print a predicate register as pn<N> rather than p<N>
     'A':		Output address constant representing the first
			argument of X, specifying a relocation offset
			if appropriate.
     'L':		Output constant address specified by X
			with a relocation offset if appropriate.
     'G':		Prints address of X, specifying a PC relative
			relocation mode if appropriate.
     'y':		Output address of LDP or STP - this is used for
			some LDP/STPs which don't use a PARALLEL in their
			pattern (so the mode needs to be adjusted).
     'z':		Output address of a typical LDP or STP.  */

static void
aarch64_print_operand (FILE *f, rtx x, int code)
{
  rtx elt;
  switch (code)
    {
    case 'c':
      if (CONST_INT_P (x))
	fprintf (f, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      else
	{
	  poly_int64 offset;
	  rtx base = strip_offset_and_salt (x, &offset);
	  if (SYMBOL_REF_P (base))
	    output_addr_const (f, x);
	  else
	    output_operand_lossage ("unsupported operand for code '%c'", code);
	}
      break;

    case 'e':
      {
	x = unwrap_const_vec_duplicate (x);
	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	HOST_WIDE_INT val = INTVAL (x);
	if ((val & ~7) == 8 || val == 0xff)
	  fputc ('b', f);
	else if ((val & ~7) == 16 || val == 0xffff)
	  fputc ('h', f);
	else if ((val & ~7) == 32 || val == 0xffffffff)
	  fputc ('w', f);
	else
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
      }
      break;

    case 'p':
      {
	int n;

	if (!CONST_INT_P (x) || (n = exact_log2 (INTVAL (x))) < 0)
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	asm_fprintf (f, "%d", n);
      }
      break;

    case 'P':
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}

      asm_fprintf (f, "%u", popcount_hwi (INTVAL (x)));
      break;

    case 'H':
      if (x == const0_rtx)
	{
	  asm_fprintf (f, "xzr");
	  break;
	}

      if (!REG_P (x) || !GP_REGNUM_P (REGNO (x) + 1))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}

      asm_fprintf (f, "%s", reg_names [REGNO (x) + 1]);
      break;

    case 'I':
      {
	x = aarch64_bit_representation (unwrap_const_vec_duplicate (x));
	if (CONST_INT_P (x))
	  asm_fprintf (f, "%wd", INTVAL (x));
	else
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
	break;
      }

    case 'M':
    case 'm':
      {
        int cond_code;
	/* CONST_TRUE_RTX means al/nv (al is the default, don't print it).  */
	if (x == const_true_rtx)
	  {
	    if (code == 'M')
	      fputs ("nv", f);
	    return;
	  }

        if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

        cond_code = aarch64_get_condition_code (x);
        gcc_assert (cond_code >= 0);
	if (code == 'M')
	  cond_code = AARCH64_INVERSE_CONDITION_CODE (cond_code);
	if (GET_MODE (XEXP (x, 0)) == CC_NZCmode)
	  fputs (aarch64_sve_condition_codes[cond_code], f);
	else
	  fputs (aarch64_condition_codes[cond_code], f);
      }
      break;

    case 'N':
      if (!const_vec_duplicate_p (x, &elt))
	{
	  output_operand_lossage ("invalid vector constant");
	  return;
	}

      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_INT)
	asm_fprintf (f, "%wd", (HOST_WIDE_INT) -UINTVAL (elt));
      else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_FLOAT
	       && aarch64_print_vector_float_operand (f, x, true))
	;
      else
	{
	  output_operand_lossage ("invalid vector constant");
	  return;
	}
      break;

    case 'b':
    case 'h':
    case 's':
    case 'd':
    case 'q':
    case 'Z':
      code = TOLOWER (code);
      if (!REG_P (x) || !FP_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("incompatible floating point / vector register operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "%c%d", code, REGNO (x) - V0_REGNUM);
      break;

    case 'S':
    case 'T':
    case 'U':
    case 'V':
      if (!REG_P (x) || (!FP_REGNUM_P (REGNO (x)) && !PR_REGNUM_P (REGNO (x))))
	{
	  output_operand_lossage ("incompatible operand for '%%%c'", code);
	  return;
	}
      if (PR_REGNUM_P (REGNO (x)))
	asm_fprintf (f, "p%d", REGNO (x) - P0_REGNUM + (code - 'S'));
      else
	asm_fprintf (f, "%c%d",
		     aarch64_sve_data_mode_p (GET_MODE (x)) ? 'z' : 'v',
		     REGNO (x) - V0_REGNUM + (code - 'S'));
      break;

    case 'R':
      if (REG_P (x) && FP_REGNUM_P (REGNO (x))
	  && (aarch64_advsimd_partial_struct_mode_p (GET_MODE (x))))
	asm_fprintf (f, "d%d", REGNO (x) - V0_REGNUM + 1);
      else if (REG_P (x) && FP_REGNUM_P (REGNO (x)))
	asm_fprintf (f, "q%d", REGNO (x) - V0_REGNUM + 1);
      else if (REG_P (x) && GP_REGNUM_P (REGNO (x)))
	asm_fprintf (f, "x%d", REGNO (x) - R0_REGNUM + 1);
      else
	output_operand_lossage ("incompatible register operand for '%%%c'",
				code);
      break;

    case 'X':
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "0x%wx", UINTVAL (x) & 0xffff);
      break;

    case 'C':
      {
	/* Print a replicated constant in hex.  */
	if (!const_vec_duplicate_p (x, &elt) || !CONST_INT_P (elt))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
	scalar_mode inner_mode = GET_MODE_INNER (GET_MODE (x));
	asm_fprintf (f, "0x%wx", UINTVAL (elt) & GET_MODE_MASK (inner_mode));
      }
      break;

    case 'D':
      {
	/* Print a replicated constant in decimal, treating it as
	   unsigned.  */
	if (!const_vec_duplicate_p (x, &elt) || !CONST_INT_P (elt))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
	scalar_mode inner_mode = GET_MODE_INNER (GET_MODE (x));
	asm_fprintf (f, "%wd", UINTVAL (elt) & GET_MODE_MASK (inner_mode));
      }
      break;

    case 'w':
    case 'x':
      if (aarch64_const_zero_rtx_p (x))
	{
	  asm_fprintf (f, "%czr", code);
	  break;
	}

      if (REG_P (x) && GP_REGNUM_P (REGNO (x)))
	{
	  asm_fprintf (f, "%c%d", code, REGNO (x) - R0_REGNUM);
	  break;
	}

      if (REG_P (x) && REGNO (x) == SP_REGNUM)
	{
	  asm_fprintf (f, "%ssp", code == 'w' ? "w" : "");
	  break;
	}

      /* Fall through */

    case 0:
      if (x == NULL)
	{
	  output_operand_lossage ("missing operand");
	  return;
	}

      switch (GET_CODE (x))
	{
	case CONST_STRING:
	  {
	    asm_fprintf (f, "%s", XSTR (x, 0));
	    break;
	  }
	case REG:
	  if (aarch64_sve_data_mode_p (GET_MODE (x)))
	    {
	      if (REG_NREGS (x) == 1)
		asm_fprintf (f, "z%d", REGNO (x) - V0_REGNUM);
	      else
		{
		  char suffix
		    = sizetochar (GET_MODE_UNIT_BITSIZE (GET_MODE (x)));
		  asm_fprintf (f, "{z%d.%c - z%d.%c}",
			       REGNO (x) - V0_REGNUM, suffix,
			       END_REGNO (x) - V0_REGNUM - 1, suffix);
		}
	    }
	  else
	    asm_fprintf (f, "%s", reg_names [REGNO (x)]);
	  break;

	case MEM:
	  output_address (GET_MODE (x), XEXP (x, 0));
	  break;

	case LABEL_REF:
	case SYMBOL_REF:
	  output_addr_const (asm_out_file, x);
	  break;

	case CONST_INT:
	  asm_fprintf (f, "%wd", INTVAL (x));
	  break;

	case CONST:
	  if (!VECTOR_MODE_P (GET_MODE (x)))
	    {
	      output_addr_const (asm_out_file, x);
	      break;
	    }
	  /* fall through */

	case CONST_VECTOR:
	  if (!const_vec_duplicate_p (x, &elt))
	    {
	      output_operand_lossage ("invalid vector constant");
	      return;
	    }

	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_INT)
	    asm_fprintf (f, "%wd", INTVAL (elt));
	  else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_FLOAT
		   && aarch64_print_vector_float_operand (f, x, false))
	    ;
	  else
	    {
	      output_operand_lossage ("invalid vector constant");
	      return;
	    }
	  break;

	case CONST_DOUBLE:
	  /* Since we define TARGET_SUPPORTS_WIDE_INT we shouldn't ever
	     be getting CONST_DOUBLEs holding integers.  */
	  gcc_assert (GET_MODE (x) != VOIDmode);
	  if (aarch64_float_const_zero_rtx_p (x))
	    {
	      fputc ('0', f);
	      break;
	    }
	  else if (aarch64_float_const_representable_p (x))
	    {
#define buf_size 20
	      char float_buf[buf_size] = {'\0'};
	      real_to_decimal_for_mode (float_buf,
					CONST_DOUBLE_REAL_VALUE (x),
					buf_size, buf_size,
					1, GET_MODE (x));
	      asm_fprintf (asm_out_file, "%s", float_buf);
	      break;
#undef buf_size
	    }
	  output_operand_lossage ("invalid constant");
	  return;
	default:
	  output_operand_lossage ("invalid operand");
	  return;
	}
      break;

    case 'A':
      if (GET_CODE (x) == HIGH)
	x = XEXP (x, 0);

      switch (aarch64_classify_symbolic_expression (x))
	{
	case SYMBOL_SMALL_GOT_4G:
	  asm_fprintf (asm_out_file, ":got:");
	  break;

	case SYMBOL_SMALL_TLSGD:
	  asm_fprintf (asm_out_file, ":tlsgd:");
	  break;

	case SYMBOL_SMALL_TLSDESC:
	  asm_fprintf (asm_out_file, ":tlsdesc:");
	  break;

	case SYMBOL_SMALL_TLSIE:
	  asm_fprintf (asm_out_file, ":gottprel:");
	  break;

	case SYMBOL_TLSLE24:
	  asm_fprintf (asm_out_file, ":tprel:");
	  break;

	case SYMBOL_TINY_GOT:
	  gcc_unreachable ();
	  break;

	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'L':
      switch (aarch64_classify_symbolic_expression (x))
	{
	case SYMBOL_SMALL_GOT_4G:
	  asm_fprintf (asm_out_file, ":got_lo12:");
	  break;

	case SYMBOL_SMALL_TLSGD:
	  asm_fprintf (asm_out_file, ":tlsgd_lo12:");
	  break;

	case SYMBOL_SMALL_TLSDESC:
	  asm_fprintf (asm_out_file, ":tlsdesc_lo12:");
	  break;

	case SYMBOL_SMALL_TLSIE:
	  asm_fprintf (asm_out_file, ":gottprel_lo12:");
	  break;

	case SYMBOL_TLSLE12:
	  asm_fprintf (asm_out_file, ":tprel_lo12:");
	  break;

	case SYMBOL_TLSLE24:
	  asm_fprintf (asm_out_file, ":tprel_lo12_nc:");
	  break;

	case SYMBOL_TINY_GOT:
	  asm_fprintf (asm_out_file, ":got:");
	  break;

	case SYMBOL_TINY_TLSIE:
	  asm_fprintf (asm_out_file, ":gottprel:");
	  break;

	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'G':
      switch (aarch64_classify_symbolic_expression (x))
	{
	case SYMBOL_TLSLE24:
	  asm_fprintf (asm_out_file, ":tprel_hi12:");
	  break;
	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'k':
      {
	HOST_WIDE_INT cond_code;

	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	cond_code = INTVAL (x);
	gcc_assert (cond_code >= 0 && cond_code <= AARCH64_NV);
	asm_fprintf (f, "%d", aarch64_nzcv_codes[cond_code]);
      }
      break;

    case 'K':
      if (!REG_P (x) || !PR_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "pn%d", REGNO (x) - P0_REGNUM);
      break;

    case 'y':
    case 'z':
      {
	machine_mode mode = GET_MODE (x);

	if (!MEM_P (x)
	    || (code == 'y'
		&& maybe_ne (GET_MODE_SIZE (mode), 8)
		&& maybe_ne (GET_MODE_SIZE (mode), 16)
		&& maybe_ne (GET_MODE_SIZE (mode), 32)))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	if (!aarch64_print_address_internal (f, mode, XEXP (x, 0),
					    code == 'y'
					    ? ADDR_QUERY_LDP_STP_N
					    : ADDR_QUERY_LDP_STP))
	  output_operand_lossage ("invalid operand prefix '%%%c'", code);
      }
      break;

    default:
      output_operand_lossage ("invalid operand prefix '%%%c'", code);
      return;
    }
}

/* Print address 'x' of a memory access with mode 'mode'.
   'op' is the context required by aarch64_classify_address.  It can either be
   MEM for a normal memory access or PARALLEL for LDP/STP.  */
static bool
aarch64_print_address_internal (FILE *f, machine_mode mode, rtx x,
				aarch64_addr_query_type type)
{
  struct aarch64_address_info addr;
  unsigned int size, vec_flags;

  /* Check all addresses are Pmode - including ILP32.  */
  if (GET_MODE (x) != Pmode
      && (!CONST_INT_P (x)
	  || trunc_int_for_mode (INTVAL (x), Pmode) != INTVAL (x)))
    {
      output_operand_lossage ("invalid address mode");
      return false;
    }

  const bool load_store_pair_p = (type == ADDR_QUERY_LDP_STP
				  || type == ADDR_QUERY_LDP_STP_N);

  if (aarch64_classify_address (&addr, x, mode, true, type))
    switch (addr.type)
      {
      case ADDRESS_REG_IMM:
	if (known_eq (addr.const_offset, 0))
	  {
	    asm_fprintf (f, "[%s]", reg_names[REGNO (addr.base)]);
	    return true;
	  }

	vec_flags = aarch64_classify_vector_memory_mode (mode);
	if ((vec_flags & VEC_ANY_SVE) && !load_store_pair_p)
	  {
	    HOST_WIDE_INT vnum
	      = exact_div (addr.const_offset,
			   aarch64_vl_bytes (mode, vec_flags)).to_constant ();
	    asm_fprintf (f, "[%s, #%wd, mul vl]",
			 reg_names[REGNO (addr.base)], vnum);
	    return true;
	  }

	if (!CONST_INT_P (addr.offset))
	  return false;

	asm_fprintf (f, "[%s, %wd]", reg_names[REGNO (addr.base)],
		     INTVAL (addr.offset));
	return true;

      case ADDRESS_REG_REG:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, %s]", reg_names [REGNO (addr.base)],
		       reg_names [REGNO (addr.offset)]);
	else
	  asm_fprintf (f, "[%s, %s, lsl %u]", reg_names [REGNO (addr.base)],
		       reg_names [REGNO (addr.offset)], addr.shift);
	return true;

      case ADDRESS_REG_UXTW:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, w%d, uxtw]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM);
	else
	  asm_fprintf (f, "[%s, w%d, uxtw %u]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM, addr.shift);
	return true;

      case ADDRESS_REG_SXTW:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, w%d, sxtw]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM);
	else
	  asm_fprintf (f, "[%s, w%d, sxtw %u]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM, addr.shift);
	return true;

      case ADDRESS_REG_WB:
	/* Writeback is only supported for fixed-width modes.  */
	size = GET_MODE_SIZE (mode).to_constant ();
	switch (GET_CODE (x))
	  {
	  case PRE_INC:
	    asm_fprintf (f, "[%s, %d]!", reg_names [REGNO (addr.base)], size);
	    return true;
	  case POST_INC:
	    asm_fprintf (f, "[%s], %d", reg_names [REGNO (addr.base)], size);
	    return true;
	  case PRE_DEC:
	    asm_fprintf (f, "[%s, -%d]!", reg_names [REGNO (addr.base)], size);
	    return true;
	  case POST_DEC:
	    asm_fprintf (f, "[%s], -%d", reg_names [REGNO (addr.base)], size);
	    return true;
	  case PRE_MODIFY:
	    asm_fprintf (f, "[%s, %wd]!", reg_names[REGNO (addr.base)],
			 INTVAL (addr.offset));
	    return true;
	  case POST_MODIFY:
	    asm_fprintf (f, "[%s], %wd", reg_names[REGNO (addr.base)],
			 INTVAL (addr.offset));
	    return true;
	  default:
	    break;
	  }
	break;

      case ADDRESS_LO_SUM:
	asm_fprintf (f, "[%s, #:lo12:", reg_names [REGNO (addr.base)]);
	output_addr_const (f, addr.offset);
	asm_fprintf (f, "]");
	return true;

      case ADDRESS_SYMBOLIC:
	output_addr_const (f, x);
	return true;
      }

  return false;
}

/* Print address 'x' of a memory access with mode 'mode'.  */
static void
aarch64_print_operand_address (FILE *f, machine_mode mode, rtx x)
{
  if (!aarch64_print_address_internal (f, mode, x, ADDR_QUERY_ANY))
    output_addr_const (f, x);
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
aarch64_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_SALT_ADDR)
    {
      output_addr_const (file, XVECEXP (x, 0, 0));
      return true;
   }
  return false;
}

bool
aarch64_label_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (LABEL_REF_P (x))
    return true;

  /* UNSPEC_TLS entries for a symbol include a LABEL_REF for the
     referencing instruction, but they are constant offsets, not
     symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return false;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (aarch64_label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && aarch64_label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Implement REGNO_REG_CLASS.  */

enum reg_class
aarch64_regno_regclass (unsigned regno)
{
  if (W8_W11_REGNUM_P (regno))
    return W8_W11_REGS;

  if (W12_W15_REGNUM_P (regno))
    return W12_W15_REGS;

  if (STUB_REGNUM_P (regno))
    return STUB_REGS;

  if (GP_REGNUM_P (regno))
    return GENERAL_REGS;

  if (regno == SP_REGNUM)
    return STACK_REG;

  if (regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return POINTER_REGS;

  if (FP_REGNUM_P (regno))
    return (FP_LO8_REGNUM_P (regno) ? FP_LO8_REGS
	    : FP_LO_REGNUM_P (regno) ? FP_LO_REGS : FP_REGS);

  if (PR_REGNUM_P (regno))
    return PR_LO_REGNUM_P (regno) ? PR_LO_REGS : PR_HI_REGS;

  if (regno == FPM_REGNUM)
    return MOVEABLE_SYSREGS;

  if (regno == FFR_REGNUM || regno == FFRT_REGNUM)
    return FFR_REGS;

  if (FAKE_REGNUM_P (regno))
    return FAKE_REGS;

  return NO_REGS;
}

/* OFFSET is an address offset for mode MODE, which has SIZE bytes.
   If OFFSET is out of range, return an offset of an anchor point
   that is in range.  Return 0 otherwise.  */

static HOST_WIDE_INT
aarch64_anchor_offset (HOST_WIDE_INT offset, HOST_WIDE_INT size,
		       machine_mode mode)
{
  /* Does it look like we'll need a 16-byte load/store-pair operation?  */
  if (size > 16)
    return (offset + 0x400) & ~0x7f0;

  /* For offsets that aren't a multiple of the access size, the limit is
     -256...255.  */
  if (offset & (size - 1))
    {
      /* BLKmode typically uses LDP of X-registers.  */
      if (mode == BLKmode)
	return (offset + 512) & ~0x3ff;
      return (offset + 0x100) & ~0x1ff;
    }

  /* Small negative offsets are supported.  */
  if (IN_RANGE (offset, -256, 0))
    return 0;

  if (mode == TImode || mode == TFmode || mode == TDmode)
    return (offset + 0x100) & ~0x1ff;

  /* Use 12-bit offset by access size.  */
  return offset & (~0xfff * size);
}

static rtx
aarch64_legitimize_address (rtx x, rtx /* orig_x  */, machine_mode mode)
{
#if TARGET_PECOFF
  rtx tmp = legitimize_pe_coff_symbol (x, true);
  if (tmp)
    return tmp;
#endif

  /* Try to split X+CONST into Y=X+(CONST & ~mask), Y+(CONST&mask),
     where mask is selected by alignment and size of the offset.
     We try to pick as large a range for the offset as possible to
     maximize the chance of a CSE.  However, for aligned addresses
     we limit the range to 4k so that structures with different sized
     elements are likely to use the same base.  We need to be careful
     not to split a CONST for some forms of address expression, otherwise
     it will generate sub-optimal code.  */

  /* First split X + CONST (base, offset) into (base + X) + offset.  */
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST)
    {
      poly_int64 offset;
      rtx base = strip_offset (XEXP (x, 1), &offset);

      base = expand_binop (Pmode, add_optab, base, XEXP (x, 0),
			   NULL_RTX, true, OPTAB_DIRECT);
      x = plus_constant (Pmode, base, offset);
    }

  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    {
      rtx base = XEXP (x, 0);
      rtx offset_rtx = XEXP (x, 1);
      HOST_WIDE_INT offset = INTVAL (offset_rtx);

      if (GET_CODE (base) == PLUS)
	{
	  rtx op0 = XEXP (base, 0);
	  rtx op1 = XEXP (base, 1);

	  /* Force any scaling into a temp for CSE.  */
	  op0 = force_reg (Pmode, op0);
	  op1 = force_reg (Pmode, op1);

	  /* Let the pointer register be in op0.  */
	  if (REG_POINTER (op1))
	    std::swap (op0, op1);

	  /* If the pointer is virtual or frame related, then we know that
	     virtual register instantiation or register elimination is going
	     to apply a second constant.  We want the two constants folded
	     together easily.  Therefore, emit as (OP0 + CONST) + OP1.  */
	  if (virt_or_elim_regno_p (REGNO (op0)))
	    {
	      base = expand_binop (Pmode, add_optab, op0, offset_rtx,
				   NULL_RTX, true, OPTAB_DIRECT);
	      return gen_rtx_PLUS (Pmode, base, op1);
	    }

	  /* Otherwise, in order to encourage CSE (and thence loop strength
	     reduce) scaled addresses, emit as (OP0 + OP1) + CONST.  */
	  base = expand_binop (Pmode, add_optab, op0, op1,
			       NULL_RTX, true, OPTAB_DIRECT);
	  x = gen_rtx_PLUS (Pmode, base, offset_rtx);
	}

      HOST_WIDE_INT size;
      if (GET_MODE_SIZE (mode).is_constant (&size))
	{
	  HOST_WIDE_INT base_offset = aarch64_anchor_offset (offset, size,
							     mode);
	  if (base_offset != 0)
	    {
	      base = plus_constant (Pmode, base, base_offset);
	      base = force_operand (base, NULL_RTX);
	      return plus_constant (Pmode, base, offset - base_offset);
	    }
	}
    }

  return x;
}

static reg_class_t
aarch64_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x,
			  reg_class_t rclass,
			  machine_mode mode,
			  secondary_reload_info *sri)
{
  /* Use aarch64_sve_reload_mem for SVE memory reloads that cannot use
     LDR and STR.  See the comment at the head of aarch64-sve.md for
     more details about the big-endian handling.  */
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (reg_class_subset_p (rclass, FP_REGS)
      && !((REG_P (x) && HARD_REGISTER_P (x))
	   || aarch64_simd_valid_mov_imm (x))
      && mode != VNx16QImode
      && (vec_flags & VEC_SVE_DATA)
      && ((vec_flags & VEC_PARTIAL) || BYTES_BIG_ENDIAN))
    {
      sri->icode = CODE_FOR_aarch64_sve_reload_mem;
      return NO_REGS;
    }

  /* If we have to disable direct literal pool loads and stores because the
     function is too big, then we need a scratch register.  */
  if (MEM_P (x) && SYMBOL_REF_P (x) && CONSTANT_POOL_ADDRESS_P (x)
      && (SCALAR_FLOAT_MODE_P (GET_MODE (x))
	  || targetm.vector_mode_supported_p (GET_MODE (x)))
      && !aarch64_pcrelative_literal_loads)
    {
      sri->icode = code_for_aarch64_reload_movcp (mode, DImode);
      return NO_REGS;
    }

  /* Without the TARGET_SIMD or TARGET_SVE instructions we cannot move a
     Q register to a Q register directly.  We need a scratch.  */
  if (REG_P (x)
      && (mode == TFmode
	  || mode == TImode
	  || mode == TDmode
	  || (vec_flags == VEC_ADVSIMD && known_eq (GET_MODE_SIZE (mode), 16)))
      && mode == GET_MODE (x)
      && !TARGET_SIMD
      && FP_REGNUM_P (REGNO (x))
      && reg_class_subset_p (rclass, FP_REGS))
    {
      sri->icode = code_for_aarch64_reload_mov (mode);
      return NO_REGS;
    }

  /* A TFmode, TImode or TDmode memory access should be handled via an FP_REGS
     because AArch64 has richer addressing modes for LDR/STR instructions
     than LDP/STP instructions.  */
  if (TARGET_FLOAT && rclass == GENERAL_REGS
      && known_eq (GET_MODE_SIZE (mode), 16) && MEM_P (x))
    return FP_REGS;

  if (rclass == FP_REGS
      && (mode == TImode || mode == TFmode || mode == TDmode)
      && CONSTANT_P(x))
      return GENERAL_REGS;

  return NO_REGS;
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED.  */

static bool
aarch64_secondary_memory_needed (machine_mode mode, reg_class_t class1,
				 reg_class_t class2)
{
  if (!TARGET_SIMD
      && reg_classes_intersect_p (class1, FP_REGS)
      && reg_classes_intersect_p (class2, FP_REGS))
    {
      /* We can't do a 128-bit FPR-to-FPR move without TARGET_SIMD,
	 so we can't easily split a move involving tuples of 128-bit
	 vectors.  Force the copy through memory instead.

	 (Tuples of 64-bit vectors are fine.)  */
      unsigned int vec_flags = aarch64_classify_vector_mode (mode);
      if (vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
	return true;
    }
  return false;
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

static bool
aarch64_frame_pointer_required ()
{
  /* If the function needs to record the incoming value of PSTATE.SM,
     make sure that the slot is accessible from the frame pointer.  */
  return aarch64_need_old_pstate_sm ();
}

static bool
aarch64_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  gcc_assert (from == ARG_POINTER_REGNUM || from == FRAME_POINTER_REGNUM);

  /* If we need a frame pointer, ARG_POINTER_REGNUM and FRAME_POINTER_REGNUM
     can only eliminate to HARD_FRAME_POINTER_REGNUM.  */
  if (frame_pointer_needed)
    return to == HARD_FRAME_POINTER_REGNUM;
  return true;
}

poly_int64
aarch64_initial_elimination_offset (unsigned from, unsigned to)
{
  aarch64_frame &frame = cfun->machine->frame;

  if (to == HARD_FRAME_POINTER_REGNUM)
    {
      if (from == ARG_POINTER_REGNUM)
	return frame.bytes_above_hard_fp;

      if (from == FRAME_POINTER_REGNUM)
	return frame.bytes_above_hard_fp - frame.bytes_above_locals;
    }

  if (to == STACK_POINTER_REGNUM)
    {
      if (from == FRAME_POINTER_REGNUM)
	return frame.frame_size - frame.bytes_above_locals;
    }

  return frame.frame_size;
}


/* Get return address without mangling.  */

rtx
aarch64_return_addr_rtx (void)
{
  rtx val = get_hard_reg_initial_val (Pmode, LR_REGNUM);
  /* Note: aarch64_return_address_signing_enabled only
     works after cfun->machine->frame.laid_out is set,
     so here we don't know if the return address will
     be signed or not.  */
  rtx lr = gen_rtx_REG (Pmode, LR_REGNUM);
  emit_move_insn (lr, val);
  emit_insn (GEN_FCN (CODE_FOR_xpaclri) ());
  return lr;
}


/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
aarch64_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;
  return aarch64_return_addr_rtx ();
}

static void
aarch64_asm_trampoline_template (FILE *f)
{
  /* Even if the current function doesn't have branch protection, some
     later function might, so since this template is only generated once
     we have to add a BTI just in case. */
  asm_fprintf (f, "\thint\t34 // bti c\n");

  if (TARGET_ILP32)
    {
      asm_fprintf (f, "\tldr\tw%d, .+20\n", IP1_REGNUM - R0_REGNUM);
      asm_fprintf (f, "\tldr\tw%d, .+20\n", STATIC_CHAIN_REGNUM - R0_REGNUM);
    }
  else
    {
      asm_fprintf (f, "\tldr\t%s, .+20\n", reg_names [IP1_REGNUM]);
      asm_fprintf (f, "\tldr\t%s, .+24\n", reg_names [STATIC_CHAIN_REGNUM]);
    }
  asm_fprintf (f, "\tbr\t%s\n", reg_names [IP1_REGNUM]);

  /* We always emit a speculation barrier.
     This is because the same trampoline template is used for every nested
     function.  Since nested functions are not particularly common or
     performant we don't worry too much about the extra instructions to copy
     around.
     This is not yet a problem, since we have not yet implemented function
     specific attributes to choose between hardening against straight line
     speculation or not, but such function specific attributes are likely to
     happen in the future.  */
  asm_fprintf (f, "\tdsb\tsy\n\tisb\n");

  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
}

static void
aarch64_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr, mem, a_tramp;
  const int tramp_code_sz = 24;

  /* Don't need to copy the trailing D-words, we fill those in below.  */
  /* We create our own memory address in Pmode so that `emit_block_move` can
     use parts of the backend which expect Pmode addresses.  */
  rtx temp = convert_memory_address (Pmode, XEXP (m_tramp, 0));
  emit_block_move (gen_rtx_MEM (BLKmode, temp),
		   assemble_trampoline_template (),
		   GEN_INT (tramp_code_sz), BLOCK_OP_NORMAL);
  mem = adjust_address (m_tramp, ptr_mode, tramp_code_sz);
  fnaddr = XEXP (DECL_RTL (fndecl), 0);
  if (GET_MODE (fnaddr) != ptr_mode)
    fnaddr = convert_memory_address (ptr_mode, fnaddr);
  emit_move_insn (mem, fnaddr);

  mem = adjust_address (m_tramp, ptr_mode, tramp_code_sz + POINTER_BYTES);
  emit_move_insn (mem, chain_value);

  /* XXX We should really define a "clear_cache" pattern and use
     gen_clear_cache().  */
  a_tramp = XEXP (m_tramp, 0);
  maybe_emit_call_builtin___clear_cache (a_tramp,
					 plus_constant (ptr_mode,
							a_tramp,
							TRAMPOLINE_SIZE));
}

static unsigned char
aarch64_class_max_nregs (reg_class_t regclass, machine_mode mode)
{
  /* ??? Logically we should only need to provide a value when
     HARD_REGNO_MODE_OK says that at least one register in REGCLASS
     can hold MODE, but at the moment we need to handle all modes.
     Just ignore any runtime parts for registers that can't store them.  */
  HOST_WIDE_INT lowest_size = constant_lower_bound (GET_MODE_SIZE (mode));
  unsigned int nregs, vec_flags;
  switch (regclass)
    {
    case W8_W11_REGS:
    case W12_W15_REGS:
    case STUB_REGS:
    case TAILCALL_ADDR_REGS:
    case POINTER_REGS:
    case GENERAL_REGS:
    case ALL_REGS:
    case POINTER_AND_FP_REGS:
    case FP_REGS:
    case FP_LO_REGS:
    case FP_LO8_REGS:
      vec_flags = aarch64_classify_vector_mode (mode);
      if ((vec_flags & VEC_SVE_DATA)
	  && constant_multiple_p (GET_MODE_SIZE (mode),
				  aarch64_vl_bytes (mode, vec_flags), &nregs))
	return nregs;
      if (vec_flags == (VEC_ADVSIMD | VEC_STRUCT | VEC_PARTIAL))
	return GET_MODE_SIZE (mode).to_constant () / 8;
      return (vec_flags & VEC_ADVSIMD
	      ? CEIL (lowest_size, UNITS_PER_VREG)
	      : CEIL (lowest_size, UNITS_PER_WORD));

    case PR_REGS:
    case PR_LO_REGS:
    case PR_HI_REGS:
      return mode == VNx64BImode ? 4 : mode == VNx32BImode ? 2 : 1;

    case MOVEABLE_SYSREGS:
    case STACK_REG:
    case FFR_REGS:
    case PR_AND_FFR_REGS:
    case FAKE_REGS:
      return 1;

    case NO_REGS:
      return 0;

    default:
      break;
    }
  gcc_unreachable ();
}

static reg_class_t
aarch64_preferred_reload_class (rtx x, reg_class_t regclass)
{
  if (regclass == POINTER_REGS)
    return GENERAL_REGS;

  if (regclass == STACK_REG)
    {
      if (REG_P(x)
	  && reg_class_subset_p (REGNO_REG_CLASS (REGNO (x)), POINTER_REGS))
	  return regclass;

      return NO_REGS;
    }

  /* Register eliminiation can result in a request for
     SP+constant->FP_REGS.  We cannot support such operations which
     use SP as source and an FP_REG as destination, so reject out
     right now.  */
  if (! reg_class_subset_p (regclass, GENERAL_REGS) && GET_CODE (x) == PLUS)
    {
      rtx lhs = XEXP (x, 0);

      /* Look through a possible SUBREG introduced by ILP32.  */
      if (SUBREG_P (lhs))
	lhs = SUBREG_REG (lhs);

      gcc_assert (REG_P (lhs));
      gcc_assert (reg_class_subset_p (REGNO_REG_CLASS (REGNO (lhs)),
				      POINTER_REGS));
      return NO_REGS;
    }

  return regclass;
}

void
aarch64_asm_output_labelref (FILE* f, const char *name)
{
  asm_fprintf (f, "%U%s", name);
}

static void
aarch64_elf_asm_constructor (rtx symbol, int priority)
{
  if (priority == DEFAULT_INIT_PRIORITY)
    default_ctor_section_asm_out_constructor (symbol, priority);
  else
    {
      section *s;
      /* While priority is known to be in range [0, 65535], so 18 bytes
         would be enough, the compiler might not know that.  To avoid
         -Wformat-truncation false positive, use a larger size.  */
      char buf[23];
      snprintf (buf, sizeof (buf), ".init_array.%.5u", priority);
      s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL);
      switch_to_section (s);
      assemble_align (POINTER_SIZE);
      assemble_aligned_integer (POINTER_BYTES, symbol);
    }
}

static void
aarch64_elf_asm_destructor (rtx symbol, int priority)
{
  if (priority == DEFAULT_INIT_PRIORITY)
    default_dtor_section_asm_out_destructor (symbol, priority);
  else
    {
      section *s;
      /* While priority is known to be in range [0, 65535], so 18 bytes
         would be enough, the compiler might not know that.  To avoid
         -Wformat-truncation false positive, use a larger size.  */
      char buf[23];
      snprintf (buf, sizeof (buf), ".fini_array.%.5u", priority);
      s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL);
      switch_to_section (s);
      assemble_align (POINTER_SIZE);
      assemble_aligned_integer (POINTER_BYTES, symbol);
    }
}

const char*
aarch64_output_casesi (rtx *operands)
{
  char buf[100];
  char label[100];
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[2])));
  int index;
  static const char *const patterns[4][2] =
  {
    {
      "ldrb\t%w3, [%0,%w1,uxtw]",
      "add\t%3, %4, %w3, sxtb #2"
    },
    {
      "ldrh\t%w3, [%0,%w1,uxtw #1]",
      "add\t%3, %4, %w3, sxth #2"
    },
    {
      "ldr\t%w3, [%0,%w1,uxtw #2]",
      "add\t%3, %4, %w3, sxtw #2"
    },
    /* We assume that DImode is only generated when not optimizing and
       that we don't really need 64-bit address offsets.  That would
       imply an object file with 8GB of code in a single function!  */
    {
      "ldr\t%w3, [%0,%w1,uxtw #2]",
      "add\t%3, %4, %w3, sxtw #2"
    }
  };

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  scalar_int_mode mode = as_a <scalar_int_mode> (GET_MODE (diff_vec));
  index = exact_log2 (GET_MODE_SIZE (mode));

  gcc_assert (index >= 0 && index <= 3);

  /* Need to implement table size reduction, by chaning the code below.  */
  output_asm_insn (patterns[index][0], operands);
  ASM_GENERATE_INTERNAL_LABEL (label, "Lrtx", CODE_LABEL_NUMBER (operands[2]));
  snprintf (buf, sizeof (buf),
	    "adr\t%%4, %s", targetm.strip_name_encoding (label));
  output_asm_insn (buf, operands);
  output_asm_insn (patterns[index][1], operands);
  output_asm_insn ("br\t%3", operands);
  output_asm_insn (aarch64_sls_barrier (aarch64_harden_sls_retbr_p ()),
		   operands);
  assemble_label (asm_out_file, label);
  return "";
}

/* Return the asm string for an SME ZERO instruction whose 8-bit mask
   operand is MASK.  */
const char *
aarch64_output_sme_zero_za (rtx mask)
{
  auto mask_val = UINTVAL (mask);
  if (mask_val == 0)
    return "zero\t{}";

  if (mask_val == 0xff)
    return "zero\t{ za }";

  static constexpr struct { unsigned char mask; char letter; } tiles[] = {
    { 0xff, 'b' },
    { 0x55, 'h' },
    { 0x11, 's' },
    { 0x01, 'd' }
  };
  /* The last entry in the list has the form "za7.d }", but that's the
     same length as "za7.d, ".  */
  static char buffer[sizeof("zero\t{ ") + sizeof ("za7.d, ") * 8 + 1];
  for (auto &tile : tiles)
    {
      unsigned int tile_mask = tile.mask;
      unsigned int tile_index = 0;
      unsigned int i = snprintf (buffer, sizeof (buffer), "zero\t");
      const char *prefix = "{ ";
      auto remaining_mask = mask_val;
      while (tile_mask < 0x100)
	{
	  if ((remaining_mask & tile_mask) == tile_mask)
	    {
	      i += snprintf (buffer + i, sizeof (buffer) - i, "%sza%d.%c",
			     prefix, tile_index, tile.letter);
	      prefix = ", ";
	      remaining_mask &= ~tile_mask;
	    }
	  tile_mask <<= 1;
	  tile_index += 1;
	}
      if (remaining_mask == 0)
	{
	  gcc_assert (i + 3 <= sizeof (buffer));
	  snprintf (buffer + i, sizeof (buffer) - i, " }");
	  return buffer;
	}
    }
  gcc_unreachable ();
}

/* Return size in bits of an arithmetic operand which is shifted/scaled and
   masked such that it is suitable for a UXTB, UXTH, or UXTW extend
   operator.  */

int
aarch64_uxt_size (int shift, HOST_WIDE_INT mask)
{
  if (shift >= 0 && shift <= 4)
    {
      int size;
      for (size = 8; size <= 32; size *= 2)
	{
	  HOST_WIDE_INT bits = ((HOST_WIDE_INT)1U << size) - 1;
	  if (mask == bits << shift)
	    return size;
	}
    }
  return 0;
}

/* Constant pools are per function only when PC relative
   literal loads are true or we are in the large memory
   model.  */

static inline bool
aarch64_can_use_per_function_literal_pools_p (void)
{
  return (aarch64_pcrelative_literal_loads
	  || aarch64_cmodel == AARCH64_CMODEL_LARGE);
}

static bool
aarch64_use_blocks_for_constant_p (machine_mode, const_rtx)
{
  /* We can't use blocks for constants when we're using a per-function
     constant pool.  */
  return !aarch64_can_use_per_function_literal_pools_p ();
}

/* Select appropriate section for constants depending
   on where we place literal pools.  */

static section *
aarch64_select_rtx_section (machine_mode mode,
			    rtx x,
			    unsigned HOST_WIDE_INT align)
{
  if (aarch64_can_use_per_function_literal_pools_p ())
    return function_section (current_function_decl);

  return default_elf_select_rtx_section (mode, x, align);
}

/* Implement ASM_OUTPUT_POOL_EPILOGUE.  */
void
aarch64_asm_output_pool_epilogue (FILE *f, const char *, tree,
				  HOST_WIDE_INT offset)
{
  /* When using per-function literal pools, we must ensure that any code
     section is aligned to the minimal instruction length, lest we get
     errors from the assembler re "unaligned instructions".  */
  if ((offset & 3) && aarch64_can_use_per_function_literal_pools_p ())
    ASM_OUTPUT_ALIGN (f, 2);
}

/* Costs.  */

/* Helper function for rtx cost calculation.  Strip a shift expression
   from X.  Returns the inner operand if successful, or the original
   expression on failure.  */
static rtx
aarch64_strip_shift (rtx x)
{
  rtx op = x;

  /* We accept both ROTATERT and ROTATE: since the RHS must be a constant
     we can convert both to ROR during final output.  */
  if ((GET_CODE (op) == ASHIFT
       || GET_CODE (op) == ASHIFTRT
       || GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ROTATERT
       || GET_CODE (op) == ROTATE)
      && CONST_INT_P (XEXP (op, 1)))
    return XEXP (op, 0);

  if (GET_CODE (op) == MULT
      && CONST_INT_P (XEXP (op, 1))
      && ((unsigned) exact_log2 (INTVAL (XEXP (op, 1)))) < 64)
    return XEXP (op, 0);

  return x;
}

/* Helper function for rtx cost calculation.  Strip an extend
   expression from X.  Returns the inner operand if successful, or the
   original expression on failure.  We deal with a number of possible
   canonicalization variations here. If STRIP_SHIFT is true, then
   we can strip off a shift also.  */
static rtx
aarch64_strip_extend (rtx x, bool strip_shift)
{
  scalar_int_mode mode;
  rtx op = x;

  if (!is_a <scalar_int_mode> (GET_MODE (op), &mode))
    return op;

  if (GET_CODE (op) == AND
      && GET_CODE (XEXP (op, 0)) == MULT
      && CONST_INT_P (XEXP (XEXP (op, 0), 1))
      && CONST_INT_P (XEXP (op, 1))
      && aarch64_uxt_size (exact_log2 (INTVAL (XEXP (XEXP (op, 0), 1))),
			   INTVAL (XEXP (op, 1))) != 0)
    return XEXP (XEXP (op, 0), 0);

  /* Now handle extended register, as this may also have an optional
     left shift by 1..4.  */
  if (strip_shift
      && GET_CODE (op) == ASHIFT
      && CONST_INT_P (XEXP (op, 1))
      && ((unsigned HOST_WIDE_INT) INTVAL (XEXP (op, 1))) <= 4)
    op = XEXP (op, 0);

  if (GET_CODE (op) == ZERO_EXTEND
      || GET_CODE (op) == SIGN_EXTEND)
    op = XEXP (op, 0);

  if (op != x)
    return op;

  return x;
}

/* Helper function for rtx cost calculation. Strip extension as well as any
   inner VEC_SELECT high-half from X. Returns the inner vector operand if
   successful, or the original expression on failure.  */
static rtx
aarch64_strip_extend_vec_half (rtx x)
{
  if (GET_CODE (x) == ZERO_EXTEND || GET_CODE (x) == SIGN_EXTEND)
    {
      x = XEXP (x, 0);
      if (GET_CODE (x) == VEC_SELECT
	  && vec_series_highpart_p (GET_MODE (x), GET_MODE (XEXP (x, 0)),
				    XEXP (x, 1)))
	x = XEXP (x, 0);
    }
  return x;
}

/* Helper function for rtx cost calculation. Strip VEC_DUPLICATE as well as
   any subsequent extend and VEC_SELECT from X. Returns the inner scalar
   operand if successful, or the original expression on failure.  */
static rtx
aarch64_strip_duplicate_vec_elt (rtx x)
{
  if (GET_CODE (x) == VEC_DUPLICATE
      && is_a<scalar_mode> (GET_MODE (XEXP (x, 0))))
    {
      x = XEXP (x, 0);
      if (GET_CODE (x) == VEC_SELECT)
	x = XEXP (x, 0);
      else if ((GET_CODE (x) == ZERO_EXTEND || GET_CODE (x) == SIGN_EXTEND)
	       && GET_CODE (XEXP (x, 0)) == VEC_SELECT)
	x = XEXP (XEXP (x, 0), 0);
    }
  return x;
}

/* Return true iff CODE is a shift supported in combination
   with arithmetic instructions.  */

static bool
aarch64_shift_p (enum rtx_code code)
{
  return code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT;
}


/* Return true iff X is a cheap shift without a sign extend. */

static bool
aarch64_cheap_mult_shift_p (rtx x)
{
  rtx op0, op1;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if (!(aarch64_tune_params.extra_tuning_flags
                      & AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND))
    return false;

  if (GET_CODE (op0) == SIGN_EXTEND)
    return false;

  if (GET_CODE (x) == ASHIFT && CONST_INT_P (op1)
      && UINTVAL (op1) <= 4)
    return true;

  if (GET_CODE (x) != MULT || !CONST_INT_P (op1))
    return false;

  HOST_WIDE_INT l2 = exact_log2 (INTVAL (op1));

  if (l2 > 0 && l2 <= 4)
    return true;

  return false;
}

/* Helper function for rtx cost calculation.  Calculate the cost of
   a MULT or ASHIFT, which may be part of a compound PLUS/MINUS rtx.
   Return the calculated cost of the expression, recursing manually in to
   operands where needed.  */

static int
aarch64_rtx_mult_cost (rtx x, enum rtx_code code, int outer, bool speed)
{
  rtx op0, op1;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params.insn_extra_cost;
  int cost = 0;
  bool compound_p = (outer == PLUS || outer == MINUS);
  machine_mode mode = GET_MODE (x);

  gcc_checking_assert (code == MULT);

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if (VECTOR_MODE_P (mode))
    {
      unsigned int vec_flags = aarch64_classify_vector_mode (mode);
      if (TARGET_SIMD && (vec_flags & VEC_ADVSIMD))
	{
	  /* The select-operand-high-half versions of the instruction have the
	     same cost as the three vector version - don't add the costs of the
	     extension or selection into the costs of the multiply.  */
	  op0 = aarch64_strip_extend_vec_half (op0);
	  op1 = aarch64_strip_extend_vec_half (op1);
	  /* The by-element versions of the instruction have the same costs as
	     the normal 3-vector version.  We make an assumption that the input
	     to the VEC_DUPLICATE is already on the FP & SIMD side.  This means
	     costing of a MUL by element pre RA is a bit optimistic.  */
	  op0 = aarch64_strip_duplicate_vec_elt (op0);
	  op1 = aarch64_strip_duplicate_vec_elt (op1);
	}
      cost += rtx_cost (op0, mode, MULT, 0, speed);
      cost += rtx_cost (op1, mode, MULT, 1, speed);
      if (speed)
	{
	  if (GET_CODE (x) == MULT)
	    cost += extra_cost->vect.mult;
	  /* This is to catch the SSRA costing currently flowing here.  */
	  else
	    cost += extra_cost->vect.alu;
	}
      return cost;
    }

  /* Integer multiply/fma.  */
  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      /* The multiply will be canonicalized as a shift, cost it as such.  */
      if (aarch64_shift_p (GET_CODE (x))
	  || (CONST_INT_P (op1)
	      && exact_log2 (INTVAL (op1)) > 0))
	{
	  bool is_extend = GET_CODE (op0) == ZERO_EXTEND
	                   || GET_CODE (op0) == SIGN_EXTEND;
	  if (speed)
	    {
	      if (compound_p)
	        {
		  /* If the shift is considered cheap,
		     then don't add any cost. */
		  if (aarch64_cheap_mult_shift_p (x))
		    ;
	          else if (REG_P (op1))
		    /* ARITH + shift-by-register.  */
		    cost += extra_cost->alu.arith_shift_reg;
		  else if (is_extend)
		    /* ARITH + extended register.  We don't have a cost field
		       for ARITH+EXTEND+SHIFT, so use extend_arith here.  */
		    cost += extra_cost->alu.extend_arith;
		  else
		    /* ARITH + shift-by-immediate.  */
		    cost += extra_cost->alu.arith_shift;
		}
	      else
		/* LSL (immediate).  */
	        cost += extra_cost->alu.shift;

	    }
	  /* Strip extends as we will have costed them in the case above.  */
	  if (is_extend)
	    op0 = aarch64_strip_extend (op0, true);

	  cost += rtx_cost (op0, VOIDmode, code, 0, speed);

	  return cost;
	}

      /* MNEG or [US]MNEGL.  Extract the NEG operand and indicate that it's a
	 compound and let the below cases handle it.  After all, MNEG is a
	 special-case alias of MSUB.  */
      if (GET_CODE (op0) == NEG)
	{
	  op0 = XEXP (op0, 0);
	  compound_p = true;
	}

      /* Integer multiplies or FMAs have zero/sign extending variants.  */
      if ((GET_CODE (op0) == ZERO_EXTEND
	   && GET_CODE (op1) == ZERO_EXTEND)
	  || (GET_CODE (op0) == SIGN_EXTEND
	      && GET_CODE (op1) == SIGN_EXTEND))
	{
	  cost += rtx_cost (XEXP (op0, 0), VOIDmode, MULT, 0, speed);
	  cost += rtx_cost (XEXP (op1, 0), VOIDmode, MULT, 1, speed);

	  if (speed)
	    {
	      if (compound_p)
		/* SMADDL/UMADDL/UMSUBL/SMSUBL.  */
		cost += extra_cost->mult[0].extend_add;
	      else
		/* MUL/SMULL/UMULL.  */
		cost += extra_cost->mult[0].extend;
	    }

	  return cost;
	}

      /* This is either an integer multiply or a MADD.  In both cases
	 we want to recurse and cost the operands.  */
      cost += rtx_cost (op0, mode, MULT, 0, speed);
      cost += rtx_cost (op1, mode, MULT, 1, speed);

      if (speed)
	{
	  if (compound_p)
	    /* MADD/MSUB.  */
	    cost += extra_cost->mult[mode == DImode].add;
	  else
	    /* MUL.  */
	    cost += extra_cost->mult[mode == DImode].simple;
	}

      return cost;
    }
  else
    {
      if (speed)
	{
	  /* Floating-point FMA/FMUL can also support negations of the
	     operands, unless the rounding mode is upward or downward in
	     which case FNMUL is different than FMUL with operand negation.  */
	  bool neg0 = GET_CODE (op0) == NEG;
	  bool neg1 = GET_CODE (op1) == NEG;
	  if (compound_p || !flag_rounding_math || (neg0 && neg1))
	    {
	      if (neg0)
		op0 = XEXP (op0, 0);
	      if (neg1)
		op1 = XEXP (op1, 0);
	    }

	  if (compound_p)
	    /* FMADD/FNMADD/FNMSUB/FMSUB.  */
	    cost += extra_cost->fp[mode == DFmode].fma;
	  else
	    /* FMUL/FNMUL.  */
	    cost += extra_cost->fp[mode == DFmode].mult;
	}

      cost += rtx_cost (op0, mode, MULT, 0, speed);
      cost += rtx_cost (op1, mode, MULT, 1, speed);
      return cost;
    }
}

static int
aarch64_address_cost (rtx x,
		      machine_mode mode,
		      addr_space_t as ATTRIBUTE_UNUSED,
		      bool speed)
{
  enum rtx_code c = GET_CODE (x);
  const struct cpu_addrcost_table *addr_cost = aarch64_tune_params.addr_cost;
  struct aarch64_address_info info;
  int cost = 0;
  info.shift = 0;

  if (!aarch64_classify_address (&info, x, mode, false))
    {
      if (GET_CODE (x) == CONST || SYMBOL_REF_P (x))
	{
	  /* This is a CONST or SYMBOL ref which will be split
	     in a different way depending on the code model in use.
	     Cost it through the generic infrastructure.  */
	  int cost_symbol_ref = rtx_cost (x, Pmode, MEM, 1, speed);
	  /* Divide through by the cost of one instruction to
	     bring it to the same units as the address costs.  */
	  cost_symbol_ref /= COSTS_N_INSNS (1);
	  /* The cost is then the cost of preparing the address,
	     followed by an immediate (possibly 0) offset.  */
	  return cost_symbol_ref + addr_cost->imm_offset;
	}
      else
	{
	  /* This is most likely a jump table from a case
	     statement.  */
	  return addr_cost->register_offset;
	}
    }

  switch (info.type)
    {
      case ADDRESS_LO_SUM:
      case ADDRESS_SYMBOLIC:
      case ADDRESS_REG_IMM:
	cost += addr_cost->imm_offset;
	break;

      case ADDRESS_REG_WB:
	if (c == PRE_INC || c == PRE_DEC || c == PRE_MODIFY)
	  cost += addr_cost->pre_modify;
	else if (c == POST_INC || c == POST_DEC || c == POST_MODIFY)
	  {
	    unsigned int nvectors = aarch64_ldn_stn_vectors (mode);
	    if (nvectors == 3)
	      cost += addr_cost->post_modify_ld3_st3;
	    else if (nvectors == 4)
	      cost += addr_cost->post_modify_ld4_st4;
	    else
	      cost += addr_cost->post_modify;
	  }
	else
	  gcc_unreachable ();

	break;

      case ADDRESS_REG_REG:
	cost += addr_cost->register_offset;
	break;

      case ADDRESS_REG_SXTW:
	cost += addr_cost->register_sextend;
	break;

      case ADDRESS_REG_UXTW:
	cost += addr_cost->register_zextend;
	break;

      default:
	gcc_unreachable ();
    }


  if (info.shift > 0)
    {
      /* For the sake of calculating the cost of the shifted register
	 component, we can treat same sized modes in the same way.  */
      if (known_eq (GET_MODE_BITSIZE (mode), 16))
	cost += addr_cost->addr_scale_costs.hi;
      else if (known_eq (GET_MODE_BITSIZE (mode), 32))
	cost += addr_cost->addr_scale_costs.si;
      else if (known_eq (GET_MODE_BITSIZE (mode), 64))
	cost += addr_cost->addr_scale_costs.di;
      else
	/* We can't tell, or this is a 128-bit vector.  */
	cost += addr_cost->addr_scale_costs.ti;
    }

  return cost;
}

/* Return the cost of a branch.  If SPEED_P is true then the compiler is
   optimizing for speed.  If PREDICTABLE_P is true then the branch is predicted
   to be taken.  */

int
aarch64_branch_cost (bool speed_p, bool predictable_p)
{
  /* When optimizing for speed, use the cost of unpredictable branches.  */
  const struct cpu_branch_cost *branch_costs =
    aarch64_tune_params.branch_costs;

  if (!speed_p || predictable_p)
    return branch_costs->predictable;
  else
    return branch_costs->unpredictable;
}

/* Return true if X is a zero or sign extract
   usable in an ADD or SUB (extended register) instruction.  */
static bool
aarch64_rtx_arith_op_extract_p (rtx x)
{
  /* The simple case <ARITH>, XD, XN, XM, [us]xt.
     No shift.  */
  if (GET_CODE (x) == SIGN_EXTEND
      || GET_CODE (x) == ZERO_EXTEND)
    return REG_P (XEXP (x, 0));

  return false;
}

static bool
aarch64_frint_unspec_p (unsigned int u)
{
  switch (u)
    {
      case UNSPEC_FRINTZ:
      case UNSPEC_FRINTP:
      case UNSPEC_FRINTM:
      case UNSPEC_FRINTA:
      case UNSPEC_FRINTN:
      case UNSPEC_FRINTX:
      case UNSPEC_FRINTI:
        return true;

      default:
        return false;
    }
}

/* Return true iff X is an rtx that will match an extr instruction
   i.e. as described in the *extr<mode>5_insn family of patterns.
   OP0 and OP1 will be set to the operands of the shifts involved
   on success and will be NULL_RTX otherwise.  */

static bool
aarch64_extr_rtx_p (rtx x, rtx *res_op0, rtx *res_op1)
{
  rtx op0, op1;
  scalar_int_mode mode;
  if (!is_a <scalar_int_mode> (GET_MODE (x), &mode))
    return false;

  *res_op0 = NULL_RTX;
  *res_op1 = NULL_RTX;

  if (GET_CODE (x) != IOR)
    return false;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if ((GET_CODE (op0) == ASHIFT && GET_CODE (op1) == LSHIFTRT)
      || (GET_CODE (op1) == ASHIFT && GET_CODE (op0) == LSHIFTRT))
    {
     /* Canonicalise locally to ashift in op0, lshiftrt in op1.  */
      if (GET_CODE (op1) == ASHIFT)
        std::swap (op0, op1);

      if (!CONST_INT_P (XEXP (op0, 1)) || !CONST_INT_P (XEXP (op1, 1)))
        return false;

      unsigned HOST_WIDE_INT shft_amnt_0 = UINTVAL (XEXP (op0, 1));
      unsigned HOST_WIDE_INT shft_amnt_1 = UINTVAL (XEXP (op1, 1));

      if (shft_amnt_0 < GET_MODE_BITSIZE (mode)
          && shft_amnt_0 + shft_amnt_1 == GET_MODE_BITSIZE (mode))
        {
          *res_op0 = XEXP (op0, 0);
          *res_op1 = XEXP (op1, 0);
          return true;
        }
    }

  return false;
}

/* Calculate the cost of calculating (if_then_else (OP0) (OP1) (OP2)),
   storing it in *COST.  Result is true if the total cost of the operation
   has now been calculated.  */
static bool
aarch64_if_then_else_costs (rtx op0, rtx op1, rtx op2, int *cost, bool speed)
{
  rtx inner;
  rtx comparator;
  enum rtx_code cmpcode;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params.insn_extra_cost;

  if (COMPARISON_P (op0))
    {
      inner = XEXP (op0, 0);
      comparator = XEXP (op0, 1);
      cmpcode = GET_CODE (op0);
    }
  else
    {
      inner = op0;
      comparator = const0_rtx;
      cmpcode = NE;
    }

  if (GET_CODE (op1) == PC || GET_CODE (op2) == PC)
    {
      /* Conditional branch.  */
      if (GET_MODE_CLASS (GET_MODE (inner)) == MODE_CC)
	return true;
      else
	{
	  if (cmpcode == NE || cmpcode == EQ)
	    {
	      if (comparator == const0_rtx)
		{
		  /* TBZ/TBNZ/CBZ/CBNZ.  */
		  if (GET_CODE (inner) == ZERO_EXTRACT)
		    /* TBZ/TBNZ.  */
		    *cost += rtx_cost (XEXP (inner, 0), VOIDmode,
				       ZERO_EXTRACT, 0, speed);
		  else
		    /* CBZ/CBNZ.  */
		    *cost += rtx_cost (inner, VOIDmode, cmpcode, 0, speed);

		  return true;
		}
	      if (register_operand (inner, VOIDmode)
		  && aarch64_imm24 (comparator, VOIDmode))
		{
		  /* SUB and SUBS.  */
		  *cost += COSTS_N_INSNS (2);
		  if (speed)
		    *cost += extra_cost->alu.arith * 2;
		  return true;
		}
	    }
	  else if (cmpcode == LT || cmpcode == GE)
	    {
	      /* TBZ/TBNZ.  */
	      if (comparator == const0_rtx)
		return true;
	    }
	}
    }
  else if (GET_MODE_CLASS (GET_MODE (inner)) == MODE_CC)
    {
      /* CCMP.  */
      if (GET_CODE (op1) == COMPARE)
	{
	  /* Increase cost of CCMP reg, 0, imm, CC to prefer CMP reg, 0.  */
	  if (XEXP (op1, 1) == const0_rtx)
	    *cost += 1;
	  if (speed)
	    {
	      machine_mode mode = GET_MODE (XEXP (op1, 0));

	      if (GET_MODE_CLASS (mode) == MODE_INT)
		*cost += extra_cost->alu.arith;
	      else
		*cost += extra_cost->fp[mode == DFmode].compare;
	    }
	  return true;
	}

      /* It's a conditional operation based on the status flags,
	 so it must be some flavor of CSEL.  */

      /* CSNEG, CSINV, and CSINC are handled for free as part of CSEL.  */
      if (GET_CODE (op1) == NEG
          || GET_CODE (op1) == NOT
          || (GET_CODE (op1) == PLUS && XEXP (op1, 1) == const1_rtx))
	op1 = XEXP (op1, 0);
      else if (GET_CODE (op1) == ZERO_EXTEND && GET_CODE (op2) == ZERO_EXTEND)
	{
	  /* CSEL with zero-extension (*cmovdi_insn_uxtw).  */
	  op1 = XEXP (op1, 0);
	  op2 = XEXP (op2, 0);
	}
      else if (GET_CODE (op1) == ZERO_EXTEND && op2 == const0_rtx)
	{
	  inner = XEXP (op1, 0);
	  if (GET_CODE (inner) == NEG || GET_CODE (inner) == NOT)
	    /* CSINV/NEG with zero extend + const 0 (*csinv3_uxtw_insn3).  */
	    op1 = XEXP (inner, 0);
	}
      else if (op1 == constm1_rtx || op1 == const1_rtx)
	{
	  /* Use CSINV or CSINC.  */
	  *cost += rtx_cost (op2, VOIDmode, IF_THEN_ELSE, 2, speed);
	  return true;
	}
      else if (op2 == constm1_rtx || op2 == const1_rtx)
	{
	  /* Use CSINV or CSINC.  */
	  *cost += rtx_cost (op1, VOIDmode, IF_THEN_ELSE, 1, speed);
	  return true;
	}

      *cost += rtx_cost (op1, VOIDmode, IF_THEN_ELSE, 1, speed);
      *cost += rtx_cost (op2, VOIDmode, IF_THEN_ELSE, 2, speed);
      return true;
    }

  /* We don't know what this is, cost all operands.  */
  return false;
}

/* Check whether X is a bitfield operation of the form shift + extend that
   maps down to a UBFIZ/SBFIZ/UBFX/SBFX instruction.  If so, return the
   operand to which the bitfield operation is applied.  Otherwise return
   NULL_RTX.  */

static rtx
aarch64_extend_bitfield_pattern_p (rtx x)
{
  rtx_code outer_code = GET_CODE (x);
  machine_mode outer_mode = GET_MODE (x);

  if (outer_code != ZERO_EXTEND && outer_code != SIGN_EXTEND
      && outer_mode != SImode && outer_mode != DImode)
    return NULL_RTX;

  rtx inner = XEXP (x, 0);
  rtx_code inner_code = GET_CODE (inner);
  machine_mode inner_mode = GET_MODE (inner);
  rtx op = NULL_RTX;

  switch (inner_code)
    {
      case ASHIFT:
	if (CONST_INT_P (XEXP (inner, 1))
	    && (inner_mode == QImode || inner_mode == HImode))
	  op = XEXP (inner, 0);
	break;
      case LSHIFTRT:
	if (outer_code == ZERO_EXTEND && CONST_INT_P (XEXP (inner, 1))
	    && (inner_mode == QImode || inner_mode == HImode))
	  op = XEXP (inner, 0);
	break;
      case ASHIFTRT:
	if (outer_code == SIGN_EXTEND && CONST_INT_P (XEXP (inner, 1))
	    && (inner_mode == QImode || inner_mode == HImode))
	  op = XEXP (inner, 0);
	break;
      default:
	break;
    }

  return op;
}

/* Return true if the mask and a shift amount from an RTX of the form
   (x << SHFT_AMNT) & MASK are valid to combine into a UBFIZ instruction of
   mode MODE.  See the *andim_ashift<mode>_bfiz pattern.  */

bool
aarch64_mask_and_shift_for_ubfiz_p (scalar_int_mode mode, rtx mask,
				    rtx shft_amnt)
{
  return CONST_INT_P (mask) && CONST_INT_P (shft_amnt)
	 && INTVAL (mask) > 0
	 && UINTVAL (shft_amnt) < GET_MODE_BITSIZE (mode)
	 && exact_log2 ((UINTVAL (mask) >> UINTVAL (shft_amnt)) + 1) >= 0
	 && (UINTVAL (mask)
	     & ((HOST_WIDE_INT_1U << UINTVAL (shft_amnt)) - 1)) == 0;
}

/* Return true if the masks and a shift amount from an RTX of the form
   ((x & MASK1) | ((y << SHIFT_AMNT) & MASK2)) are valid to combine into
   a BFI instruction of mode MODE.  See *arch64_bfi patterns.  */

bool
aarch64_masks_and_shift_for_bfi_p (scalar_int_mode mode,
				   unsigned HOST_WIDE_INT mask1,
				   unsigned HOST_WIDE_INT shft_amnt,
				   unsigned HOST_WIDE_INT mask2)
{
  unsigned HOST_WIDE_INT t;

  /* Verify that there is no overlap in what bits are set in the two masks.  */
  if (mask1 != ~mask2)
    return false;

  /* Verify that mask2 is not all zeros or ones.  */
  if (mask2 == 0 || mask2 == HOST_WIDE_INT_M1U)
    return false;

  /* The shift amount should always be less than the mode size.  */
  gcc_assert (shft_amnt < GET_MODE_BITSIZE (mode));

  /* Verify that the mask being shifted is contiguous and would be in the
     least significant bits after shifting by shft_amnt.  */
  t = mask2 + (HOST_WIDE_INT_1U << shft_amnt);
  return (t == (t & -t));
}

/* Return true if X is an RTX representing an operation in the ABD family
   of instructions.  */

static bool
aarch64_abd_rtx_p (rtx x)
{
  if (GET_CODE (x) != MINUS)
    return false;
  rtx max_arm = XEXP (x, 0);
  rtx min_arm = XEXP (x, 1);
  if (GET_CODE (max_arm) != SMAX && GET_CODE (max_arm) != UMAX)
    return false;
  bool signed_p = GET_CODE (max_arm) == SMAX;
  if (signed_p && GET_CODE (min_arm) != SMIN)
    return false;
  else if (!signed_p && GET_CODE (min_arm) != UMIN)
    return false;

  rtx maxop0 = XEXP (max_arm, 0);
  rtx maxop1 = XEXP (max_arm, 1);
  rtx minop0 = XEXP (min_arm, 0);
  rtx minop1 = XEXP (min_arm, 1);
  return rtx_equal_p (maxop0, minop0) && rtx_equal_p (maxop1, minop1);
}

/* Calculate the cost of calculating X, storing it in *COST.  Result
   is true if the total cost of the operation has now been calculated.  */
static bool
aarch64_rtx_costs (rtx x, machine_mode mode, int outer ATTRIBUTE_UNUSED,
		   int param ATTRIBUTE_UNUSED, int *cost, bool speed)
{
  rtx op0, op1, op2;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params.insn_extra_cost;
  rtx_code code = GET_CODE (x);
  scalar_int_mode int_mode;

  /* By default, assume that everything has equivalent cost to the
     cheapest instruction.  Any additional costs are applied as a delta
     above this default.  */
  *cost = COSTS_N_INSNS (1);

  switch (code)
    {
    case SET:
      /* The cost depends entirely on the operands to SET.  */
      *cost = 0;
      op0 = SET_DEST (x);
      op1 = SET_SRC (x);

      switch (GET_CODE (op0))
	{
	case MEM:
	  if (speed)
	    {
	      rtx address = XEXP (op0, 0);
	      if (VECTOR_MODE_P (mode))
		*cost += extra_cost->ldst.storev;
	      else if (GET_MODE_CLASS (mode) == MODE_INT)
		*cost += extra_cost->ldst.store;
	      else if (mode == SFmode || mode == SDmode)
		*cost += extra_cost->ldst.storef;
	      else if (mode == DFmode || mode == DDmode)
		*cost += extra_cost->ldst.stored;

	      *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	    }

	  *cost += rtx_cost (op1, mode, SET, 1, speed);
	  return true;

	case SUBREG:
	  if (! REG_P (SUBREG_REG (op0)))
	    *cost += rtx_cost (SUBREG_REG (op0), VOIDmode, SET, 0, speed);

	  /* Fall through.  */
	case REG:
	  /* The cost is one per vector-register copied.  */
	  if (VECTOR_MODE_P (GET_MODE (op0)) && REG_P (op1))
	    {
	      int nregs = aarch64_hard_regno_nregs (V0_REGNUM, GET_MODE (op0));
	      *cost = COSTS_N_INSNS (nregs);
	    }
	  /* const0_rtx is in general free, but we will use an
	     instruction to set a register to 0.  */
	  else if (REG_P (op1) || op1 == const0_rtx)
	    {
	      /* The cost is 1 per register copied.  */
	      int nregs = aarch64_hard_regno_nregs (R0_REGNUM, GET_MODE (op0));
	      *cost = COSTS_N_INSNS (nregs);
	    }
          else
	    /* Cost is just the cost of the RHS of the set.  */
	    *cost += rtx_cost (op1, mode, SET, 1, speed);
	  return true;

	case ZERO_EXTRACT:
	case SIGN_EXTRACT:
	  /* Bit-field insertion.  Strip any redundant widening of
	     the RHS to meet the width of the target.  */
	  if (SUBREG_P (op1))
	    op1 = SUBREG_REG (op1);
	  if ((GET_CODE (op1) == ZERO_EXTEND
	       || GET_CODE (op1) == SIGN_EXTEND)
	      && CONST_INT_P (XEXP (op0, 1))
	      && is_a <scalar_int_mode> (GET_MODE (XEXP (op1, 0)), &int_mode)
	      && GET_MODE_BITSIZE (int_mode) >= INTVAL (XEXP (op0, 1)))
	    op1 = XEXP (op1, 0);

          if (CONST_INT_P (op1))
            {
              /* MOV immediate is assumed to always be cheap.  */
              *cost = COSTS_N_INSNS (1);
            }
          else
            {
              /* BFM.  */
	      if (speed)
		*cost += extra_cost->alu.bfi;
	      *cost += rtx_cost (op1, VOIDmode, code, 1, speed);
            }

	  return true;

	default:
	  /* We can't make sense of this, assume default cost.  */
          *cost = COSTS_N_INSNS (1);
	  return false;
	}
      return false;

    case CONST_INT:
      /* If an instruction can incorporate a constant within the
	 instruction, the instruction's expression avoids calling
	 rtx_cost() on the constant.  If rtx_cost() is called on a
	 constant, then it is usually because the constant must be
	 moved into a register by one or more instructions.

	 The exception is constant 0, which can be expressed
	 as XZR/WZR and is therefore free.  The exception to this is
	 if we have (set (reg) (const0_rtx)) in which case we must cost
	 the move.  However, we can catch that when we cost the SET, so
	 we don't need to consider that here.  */
      if (x == const0_rtx)
	*cost = 0;
      else
	{
	  /* To an approximation, building any other constant is
	     proportionally expensive to the number of instructions
	     required to build that constant.  This is true whether we
	     are compiling for SPEED or otherwise.  */
	  machine_mode imode = known_le (GET_MODE_SIZE (mode), 4)
				? SImode : DImode;
	  *cost = COSTS_N_INSNS (aarch64_internal_mov_immediate
				 (NULL_RTX, x, false, imode));
	}
      return true;

    case CONST_DOUBLE:

      /* First determine number of instructions to do the move
	  as an integer constant.  */
      if (!aarch64_float_const_representable_p (x)
	   && !aarch64_can_const_movi_rtx_p (x, mode)
	   && aarch64_float_const_rtx_p (x))
	{
	  unsigned HOST_WIDE_INT ival;
	  bool succeed = aarch64_reinterpret_float_as_int (x, &ival);
	  gcc_assert (succeed);

	  machine_mode imode = known_eq (GET_MODE_SIZE (mode), 8)
				? DImode : SImode;
	  int ncost = aarch64_internal_mov_immediate
		(NULL_RTX, gen_int_mode (ival, imode), false, imode);
	  *cost += COSTS_N_INSNS (ncost);
	  return true;
	}

      if (speed)
	{
	  /* mov[df,sf]_aarch64.  */
	  if (aarch64_float_const_representable_p (x))
	    /* FMOV (scalar immediate).  */
	    *cost += extra_cost->fp[mode == DFmode || mode == DDmode].fpconst;
	  else if (!aarch64_float_const_zero_rtx_p (x))
	    {
	      /* This will be a load from memory.  */
	      if (mode == DFmode || mode == DDmode)
		*cost += extra_cost->ldst.loadd;
	      else
		*cost += extra_cost->ldst.loadf;
	    }
	  else
	    /* Otherwise this is +0.0.  We get this using MOVI d0, #0
	       or MOV v0.s[0], wzr - neither of which are modeled by the
	       cost tables.  Just use the default cost.  */
	    {
	    }
	}

      return true;

    case MEM:
      if (speed)
	{
	  /* For loads we want the base cost of a load, plus an
	     approximation for the additional cost of the addressing
	     mode.  */
	  rtx address = XEXP (x, 0);
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->ldst.loadv;
	  else if (GET_MODE_CLASS (mode) == MODE_INT)
	    *cost += extra_cost->ldst.load;
	  else if (mode == SFmode || mode == SDmode)
	    *cost += extra_cost->ldst.loadf;
	  else if (mode == DFmode || mode == DDmode)
	    *cost += extra_cost->ldst.loadd;

	  *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	}

      return true;

    case NEG:
      op0 = XEXP (x, 0);

      if (VECTOR_MODE_P (mode))
	{
	  /* Many vector comparison operations are represented as NEG
	     of a comparison.  */
	  if (COMPARISON_P (op0))
	    {
	      rtx op00 = XEXP (op0, 0);
	      rtx op01 = XEXP (op0, 1);
	      machine_mode inner_mode = GET_MODE (op00);
	      /* FACGE/FACGT.  */
	      if (GET_MODE_CLASS (inner_mode) == MODE_VECTOR_FLOAT
		  && GET_CODE (op00) == ABS
		  && GET_CODE (op01) == ABS)
		{
		  op00 = XEXP (op00, 0);
		  op01 = XEXP (op01, 0);
		}
	      *cost += rtx_cost (op00, inner_mode, GET_CODE (op0), 0, speed);
	      *cost += rtx_cost (op01, inner_mode, GET_CODE (op0), 1, speed);
	      if (speed)
		*cost += extra_cost->vect.alu;
	      return true;
	    }
	  if (speed)
	    {
	      /* FNEG.  */
	      *cost += extra_cost->vect.alu;
	    }
	  return false;
	}

      if (GET_MODE_CLASS (mode) == MODE_INT)
	{
          if (GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMPARE
              || GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMM_COMPARE)
            {
              /* CSETM.  */
	      *cost += rtx_cost (XEXP (op0, 0), VOIDmode, NEG, 0, speed);
              return true;
            }

	  /* Cost this as SUB wzr, X.  */
          op0 = CONST0_RTX (mode);
          op1 = XEXP (x, 0);
          goto cost_minus;
        }

      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
        {
          /* Support (neg(fma...)) as a single instruction only if
             sign of zeros is unimportant.  This matches the decision
             making in aarch64.md.  */
          if (GET_CODE (op0) == FMA && !HONOR_SIGNED_ZEROS (GET_MODE (op0)))
            {
	      /* FNMADD.  */
	      *cost = rtx_cost (op0, mode, NEG, 0, speed);
              return true;
            }
	  if (GET_CODE (op0) == MULT)
	    {
	      /* FNMUL.  */
	      *cost = rtx_cost (op0, mode, NEG, 0, speed);
	      return true;
	    }
	  if (speed)
	    /* FNEG.  */
	    *cost += extra_cost->fp[mode == DFmode].neg;
          return false;
        }

      return false;

    case CLRSB:
    case CLZ:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.clz;
	}

      return false;

    case CTZ:
      if (VECTOR_MODE_P (mode))
	{
	  *cost = COSTS_N_INSNS (3);
	  if (speed)
	    *cost += extra_cost->vect.alu * 3;
	}
      else if (TARGET_CSSC)
	{
	  *cost = COSTS_N_INSNS (1);
	  if (speed)
	    *cost += extra_cost->alu.clz;
	}
      else
	{
	  *cost = COSTS_N_INSNS (2);
	  if (speed)
	    *cost += extra_cost->alu.clz + extra_cost->alu.rev;
	}
      return false;

    case COMPARE:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (op1 == const0_rtx
	  && GET_CODE (op0) == AND)
	{
	  x = op0;
	  mode = GET_MODE (op0);
	  goto cost_logic;
	}

      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
        {
          /* TODO: A write to the CC flags possibly costs extra, this
	     needs encoding in the cost tables.  */

	  mode = GET_MODE (op0);
          /* ANDS.  */
          if (GET_CODE (op0) == AND)
            {
              x = op0;
              goto cost_logic;
            }

          if (GET_CODE (op0) == PLUS)
            {
	      /* ADDS (and CMN alias).  */
              x = op0;
              goto cost_plus;
            }

          if (GET_CODE (op0) == MINUS)
            {
	      /* SUBS.  */
              x = op0;
              goto cost_minus;
            }

	  if (GET_CODE (op0) == ZERO_EXTRACT && op1 == const0_rtx
	      && GET_MODE (x) == CC_NZmode && CONST_INT_P (XEXP (op0, 1))
	      && CONST_INT_P (XEXP (op0, 2)))
	    {
	      /* COMPARE of ZERO_EXTRACT form of TST-immediate.
		 Handle it here directly rather than going to cost_logic
		 since we know the immediate generated for the TST is valid
		 so we can avoid creating an intermediate rtx for it only
		 for costing purposes.  */
	      if (speed)
		*cost += extra_cost->alu.logical;

	      *cost += rtx_cost (XEXP (op0, 0), GET_MODE (op0),
				 ZERO_EXTRACT, 0, speed);
	      return true;
	    }

          if (GET_CODE (op1) == NEG)
            {
	      /* CMN.  */
	      if (speed)
		*cost += extra_cost->alu.arith;

	      *cost += rtx_cost (op0, mode, COMPARE, 0, speed);
	      *cost += rtx_cost (XEXP (op1, 0), mode, NEG, 1, speed);
              return true;
            }

          /* CMP.

	     Compare can freely swap the order of operands, and
             canonicalization puts the more complex operation first.
             But the integer MINUS logic expects the shift/extend
             operation in op1.  */
          if (! (REG_P (op0)
		 || (SUBREG_P (op0) && REG_P (SUBREG_REG (op0)))))
          {
            op0 = XEXP (x, 1);
            op1 = XEXP (x, 0);
          }
          goto cost_minus;
        }

      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
        {
	  /* FCMP.  */
	  if (speed)
	    *cost += extra_cost->fp[mode == DFmode].compare;

          if (CONST_DOUBLE_P (op1) && aarch64_float_const_zero_rtx_p (op1))
            {
	      *cost += rtx_cost (op0, VOIDmode, COMPARE, 0, speed);
              /* FCMP supports constant 0.0 for no extra cost. */
              return true;
            }
          return false;
        }

      if (VECTOR_MODE_P (mode))
	{
	  /* Vector compare.  */
	  if (speed)
	    *cost += extra_cost->vect.alu;

	  if (aarch64_float_const_zero_rtx_p (op1))
	    {
	      /* Vector cm (eq|ge|gt|lt|le) supports constant 0.0 for no extra
		 cost.  */
	      return true;
	    }
	  return false;
	}
      return false;

    case MINUS:
      {
	op0 = XEXP (x, 0);
	op1 = XEXP (x, 1);

cost_minus:
	if (VECTOR_MODE_P (mode))
	  {
	    unsigned int vec_flags = aarch64_classify_vector_mode (mode);
	    if (TARGET_SIMD && (vec_flags & VEC_ADVSIMD))
	      {
		/* Recognise the SABD and UABD operation here.
		   Recursion from the PLUS case will catch the accumulating
		   forms.  */
		if (aarch64_abd_rtx_p (x))
		  {
		    if (speed)
		      *cost += extra_cost->vect.alu;
		    return true;
		  }
		  /* SUBL2 and SUBW2.
		   The select-operand-high-half versions of the sub instruction
		   have the same cost as the regular three vector version -
		   don't add the costs of the select into the costs of the sub.
		   */
		op0 = aarch64_strip_extend_vec_half (op0);
		op1 = aarch64_strip_extend_vec_half (op1);
	      }
	  }

	*cost += rtx_cost (op0, mode, MINUS, 0, speed);

	/* Detect valid immediates.  */
	if ((GET_MODE_CLASS (mode) == MODE_INT
	     || (GET_MODE_CLASS (mode) == MODE_CC
		 && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT))
	    && CONST_INT_P (op1)
	    && aarch64_uimm12_shift (INTVAL (op1)))
	  {
	    if (speed)
	      /* SUB(S) (immediate).  */
	      *cost += extra_cost->alu.arith;
	    return true;
	  }

	/* Look for SUB (extended register).  */
	if (is_a <scalar_int_mode> (mode)
	    && aarch64_rtx_arith_op_extract_p (op1))
	  {
	    if (speed)
	      *cost += extra_cost->alu.extend_arith;

	    op1 = aarch64_strip_extend (op1, true);
	    *cost += rtx_cost (op1, VOIDmode, GET_CODE (op1), 0, speed);
	    return true;
	  }

	rtx new_op1 = aarch64_strip_extend (op1, false);

	/* Cost this as an FMA-alike operation.  */
	if ((GET_CODE (new_op1) == MULT
	     || aarch64_shift_p (GET_CODE (new_op1)))
	    && code != COMPARE)
	  {
	    *cost += aarch64_rtx_mult_cost (new_op1, MULT, code, speed);
	    return true;
	  }

	*cost += rtx_cost (new_op1, VOIDmode, MINUS, 1, speed);

	if (speed)
	  {
	    if (VECTOR_MODE_P (mode))
	      {
		/* Vector SUB.  */
		*cost += extra_cost->vect.alu;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_INT)
	      {
		/* SUB(S).  */
		*cost += extra_cost->alu.arith;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	      {
		/* FSUB.  */
		*cost += extra_cost->fp[mode == DFmode].addsub;
	      }
	  }
	return true;
      }

    case PLUS:
      {
	rtx new_op0;

	op0 = XEXP (x, 0);
	op1 = XEXP (x, 1);

cost_plus:
	if (VECTOR_MODE_P (mode))
	  {
	    /* ADDL2 and ADDW2.  */
	    unsigned int vec_flags = aarch64_classify_vector_mode (mode);
	    if (TARGET_SIMD && (vec_flags & VEC_ADVSIMD))
	      {
		/* The select-operand-high-half versions of the add instruction
		   have the same cost as the regular three vector version -
		   don't add the costs of the select into the costs of the add.
		   */
		op0 = aarch64_strip_extend_vec_half (op0);
		op1 = aarch64_strip_extend_vec_half (op1);
	      }
	  }

	if (GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMPARE
	    || GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMM_COMPARE)
	  {
	    /* CSINC.  */
	    *cost += rtx_cost (XEXP (op0, 0), mode, PLUS, 0, speed);
	    *cost += rtx_cost (op1, mode, PLUS, 1, speed);
	    return true;
	  }

	if (GET_MODE_CLASS (mode) == MODE_INT
	    && (aarch64_plus_immediate (op1, mode)
		|| aarch64_sve_addvl_addpl_immediate (op1, mode)))
	  {
	    *cost += rtx_cost (op0, mode, PLUS, 0, speed);

	    if (speed)
	      {
		/* ADD (immediate).  */
		*cost += extra_cost->alu.arith;

		/* Some tunings prefer to not use the VL-based scalar ops.
		   Increase the cost of the poly immediate to prevent their
		   formation.  */
		if (GET_CODE (op1) == CONST_POLY_INT
		    && (aarch64_tune_params.extra_tuning_flags
			& AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS))
		  *cost += COSTS_N_INSNS (1);
	      }
	    return true;
	  }

	if (aarch64_pluslong_immediate (op1, mode))
	  {
	    /* 24-bit add in 2 instructions or 12-bit shifted add.  */
	    if ((INTVAL (op1) & 0xfff) != 0)
	      *cost += COSTS_N_INSNS (1);

	    *cost += rtx_cost (op0, mode, PLUS, 0, speed);
	    return true;
	  }

	*cost += rtx_cost (op1, mode, PLUS, 1, speed);

	/* Look for ADD (extended register).  */
	if (is_a <scalar_int_mode> (mode)
	    && aarch64_rtx_arith_op_extract_p (op0))
	  {
	    if (speed)
	      *cost += extra_cost->alu.extend_arith;

	    op0 = aarch64_strip_extend (op0, true);
	    *cost += rtx_cost (op0, VOIDmode, GET_CODE (op0), 0, speed);
	    return true;
	  }

	/* Strip any extend, leave shifts behind as we will
	   cost them through mult_cost.  */
	new_op0 = aarch64_strip_extend (op0, false);

	if (GET_CODE (new_op0) == MULT
	    || aarch64_shift_p (GET_CODE (new_op0)))
	  {
	    *cost += aarch64_rtx_mult_cost (new_op0, MULT, PLUS,
					    speed);
	    return true;
	  }

	*cost += rtx_cost (new_op0, VOIDmode, PLUS, 0, speed);

	if (speed)
	  {
	    if (VECTOR_MODE_P (mode))
	      {
		/* Vector ADD.  */
		*cost += extra_cost->vect.alu;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_INT)
	      {
		/* ADD.  */
		*cost += extra_cost->alu.arith;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	      {
		/* FADD.  */
		*cost += extra_cost->fp[mode == DFmode].addsub;
	      }
	  }
	return true;
      }

    case BITREVERSE:
    case BSWAP:
      *cost = COSTS_N_INSNS (1);

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.rev;
	}
      return false;

    case IOR:
      if (aarch_rev16_p (x))
        {
          *cost = COSTS_N_INSNS (1);

	  if (speed)
	    {
	      if (VECTOR_MODE_P (mode))
		*cost += extra_cost->vect.alu;
	      else
		*cost += extra_cost->alu.rev;
	    }
	  return true;
        }

      if (aarch64_extr_rtx_p (x, &op0, &op1))
        {
	  *cost += rtx_cost (op0, mode, IOR, 0, speed);
	  *cost += rtx_cost (op1, mode, IOR, 1, speed);
          if (speed)
            *cost += extra_cost->alu.shift;

          return true;
        }
    /* Fall through.  */
    case XOR:
    case AND:
    cost_logic:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (VECTOR_MODE_P (mode))
	{
	  if (speed)
	    *cost += extra_cost->vect.alu;
	  return true;
	}

      if (code == AND
          && GET_CODE (op0) == MULT
          && CONST_INT_P (XEXP (op0, 1))
          && CONST_INT_P (op1)
          && aarch64_uxt_size (exact_log2 (INTVAL (XEXP (op0, 1))),
                               INTVAL (op1)) != 0)
        {
          /* This is a UBFM/SBFM.  */
	  *cost += rtx_cost (XEXP (op0, 0), mode, ZERO_EXTRACT, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
          return true;
        }

      if (is_int_mode (mode, &int_mode))
	{
	  if (CONST_INT_P (op1))
	    {
	      /* We have a mask + shift version of a UBFIZ
		 i.e. the *andim_ashift<mode>_bfiz pattern.  */
	      if (GET_CODE (op0) == ASHIFT
		  && aarch64_mask_and_shift_for_ubfiz_p (int_mode, op1,
							 XEXP (op0, 1)))
		{
		  *cost += rtx_cost (XEXP (op0, 0), int_mode, code, 0, speed);
		  if (speed)
		    *cost += extra_cost->alu.bfx;

		  return true;
		}
	      else if (aarch64_bitmask_imm (INTVAL (op1), int_mode))
		{
		/* We possibly get the immediate for free, this is not
		   modelled.  */
		  *cost += rtx_cost (op0, int_mode, code, 0, speed);
		  if (speed)
		    *cost += extra_cost->alu.logical;

		  return true;
		}
	    }
	  else
	    {
	      rtx new_op0 = op0;

	      /* Handle ORN, EON, or BIC.  */
	      if (GET_CODE (op0) == NOT)
		op0 = XEXP (op0, 0);

	      new_op0 = aarch64_strip_shift (op0);

	      /* If we had a shift on op0 then this is a logical-shift-
		 by-register/immediate operation.  Otherwise, this is just
		 a logical operation.  */
	      if (speed)
		{
		  if (new_op0 != op0)
		    {
		      /* Shift by immediate.  */
		      if (CONST_INT_P (XEXP (op0, 1)))
			*cost += extra_cost->alu.log_shift;
		      else
			*cost += extra_cost->alu.log_shift_reg;
		    }
		  else
		    *cost += extra_cost->alu.logical;
		}

	      /* In both cases we want to cost both operands.  */
	      *cost += rtx_cost (new_op0, int_mode, code, 0, speed);
	      *cost += rtx_cost (op1, int_mode, code, 1, speed);

	      return true;
	    }
	}
      return false;

    case NOT:
      x = XEXP (x, 0);
      op0 = aarch64_strip_shift (x);

      if (VECTOR_MODE_P (mode))
	{
	  /* Vector NOT.  */
	  *cost += extra_cost->vect.alu;
	  return false;
	}

      /* MVN-shifted-reg.  */
      if (op0 != x)
        {
	  *cost += rtx_cost (op0, mode, code, 0, speed);

          if (speed)
            *cost += extra_cost->alu.log_shift;

          return true;
        }
      /* EON can have two forms: (xor (not a) b) but also (not (xor a b)).
         Handle the second form here taking care that 'a' in the above can
         be a shift.  */
      else if (GET_CODE (op0) == XOR)
        {
          rtx newop0 = XEXP (op0, 0);
          rtx newop1 = XEXP (op0, 1);
          rtx op0_stripped = aarch64_strip_shift (newop0);

	  *cost += rtx_cost (newop1, mode, code, 1, speed);
	  *cost += rtx_cost (op0_stripped, mode, XOR, 0, speed);

          if (speed)
            {
              if (op0_stripped != newop0)
                *cost += extra_cost->alu.log_shift;
              else
                *cost += extra_cost->alu.logical;
            }

          return true;
        }
      /* MVN.  */
      if (speed)
	*cost += extra_cost->alu.logical;

      return false;

    case ZERO_EXTEND:

      op0 = XEXP (x, 0);
      /* If a value is written in SI mode, then zero extended to DI
	 mode, the operation will in general be free as a write to
	 a 'w' register implicitly zeroes the upper bits of an 'x'
	 register.  However, if this is

	   (set (reg) (zero_extend (reg)))

	 we must cost the explicit register move.  */
      if (mode == DImode
	  && GET_MODE (op0) == SImode)
	{
	  int op_cost = rtx_cost (op0, VOIDmode, ZERO_EXTEND, 0, speed);

	/* If OP_COST is non-zero, then the cost of the zero extend
	   is effectively the cost of the inner operation.  Otherwise
	   we have a MOV instruction and we take the cost from the MOV
	   itself.  This is true independently of whether we are
	   optimizing for space or time.  */
	  if (op_cost)
	    *cost = op_cost;

	  return true;
	}
      else if (MEM_P (op0))
	{
	  /* All loads can zero extend to any size for free.  */
	  *cost = rtx_cost (op0, VOIDmode, ZERO_EXTEND, param, speed);
	  return true;
	}

      op0 = aarch64_extend_bitfield_pattern_p (x);
      if (op0)
	{
	  *cost += rtx_cost (op0, mode, ZERO_EXTEND, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
	  return true;
	}

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      /* UMOV.  */
	      *cost += extra_cost->vect.alu;
	    }
	  else
	    {
	      /* We generate an AND instead of UXTB/UXTH.  */
	      *cost += extra_cost->alu.logical;
	    }
	}
      return false;

    case SIGN_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	{
	  /* LDRSH.  */
	  if (speed)
	    {
	      rtx address = XEXP (XEXP (x, 0), 0);
	      *cost += extra_cost->ldst.load_sign_extend;

	      *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	    }
	  return true;
	}

      op0 = aarch64_extend_bitfield_pattern_p (x);
      if (op0)
	{
	  *cost += rtx_cost (op0, mode, SIGN_EXTEND, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
	  return true;
	}

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.extend;
	}
      return false;

    case ROTATE:
    case ROTATERT:
    case LSHIFTRT:
    case ASHIFTRT:
    case ASHIFT:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (CONST_INT_P (op1))
        {
	  if (speed)
	    {
	      if (VECTOR_MODE_P (mode))
		{
		  /* Vector shift (immediate).  */
		  *cost += extra_cost->vect.alu;
		}
	      else
		{
		  /* LSL (immediate), ASR (immediate), UBMF, UBFIZ and friends.
		     These are all aliases.  */
		  *cost += extra_cost->alu.shift;
		}
	    }

          /* We can incorporate zero/sign extend for free.  */
          if (GET_CODE (op0) == ZERO_EXTEND
              || GET_CODE (op0) == SIGN_EXTEND)
            op0 = XEXP (op0, 0);

	  *cost += rtx_cost (op0, VOIDmode, ASHIFT, 0, speed);
          return true;
        }
      else
        {
	  if (VECTOR_MODE_P (mode))
	    {
	      if (speed)
		/* Vector shift (register).  */
		*cost += extra_cost->vect.alu;
	    }
	  else
	    {
	      if (speed)
		/* LSLV, ASRV.  */
		*cost += extra_cost->alu.shift_reg;

	       /* The register shift amount may be in a shorter mode expressed
		  as a lowpart SUBREG.  For costing purposes just look inside.  */
	      if (SUBREG_P (op1) && subreg_lowpart_p (op1))
		op1 = SUBREG_REG (op1);
	      if (GET_CODE (op1) == AND && REG_P (XEXP (op1, 0))
		  && CONST_INT_P (XEXP (op1, 1))
		  && known_eq (INTVAL (XEXP (op1, 1)),
			       GET_MODE_BITSIZE (mode) - 1))
		{
		  *cost += rtx_cost (op0, mode, code, 0, speed);
		  /* We already demanded XEXP (op1, 0) to be REG_P, so
		     don't recurse into it.  */
		  return true;
		}
	    }
	  return false;  /* All arguments need to be in registers.  */
        }

    case SYMBOL_REF:

      if (aarch64_cmodel == AARCH64_CMODEL_LARGE
	  || aarch64_cmodel == AARCH64_CMODEL_SMALL_SPIC)
	{
	  /* LDR.  */
	  if (speed)
	    *cost += extra_cost->ldst.load;
	}
      else if (aarch64_cmodel == AARCH64_CMODEL_SMALL
	       || aarch64_cmodel == AARCH64_CMODEL_SMALL_PIC)
	{
	  /* ADRP, followed by ADD.  */
	  *cost += COSTS_N_INSNS (1);
	  if (speed)
	    *cost += 2 * extra_cost->alu.arith;
	}
      else if (aarch64_cmodel == AARCH64_CMODEL_TINY
	       || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)
	{
	  /* ADR.  */
	  if (speed)
	    *cost += extra_cost->alu.arith;
	}

      if (flag_pic)
	{
	  /* One extra load instruction, after accessing the GOT.  */
	  *cost += COSTS_N_INSNS (1);
	  if (speed)
	    *cost += extra_cost->ldst.load;
	}
      return true;

    case HIGH:
    case LO_SUM:
      /* ADRP/ADD (immediate).  */
      if (speed)
	*cost += extra_cost->alu.arith;
      return true;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      /* UBFX/SBFX.  */
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.bfx;
	}

      /* We can trust that the immediates used will be correct (there
	 are no by-register forms), so we need only cost op0.  */
      *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed);
      return true;

    case MULT:
      *cost += aarch64_rtx_mult_cost (x, MULT, 0, speed);
      /* aarch64_rtx_mult_cost always handles recursion to its
	 operands.  */
      return true;

    case MOD:
    /* We can expand signed mod by power of 2 using a NEGS, two parallel
       ANDs and a CSNEG.  Assume here that CSNEG is the same as the cost of
       an unconditional negate.  This case should only ever be reached through
       the set_smod_pow2_cheap check in expmed.cc.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && exact_log2 (INTVAL (XEXP (x, 1))) > 0
	  && (mode == SImode || mode == DImode))
	{
	  /* We expand to 4 instructions.  Reset the baseline.  */
	  *cost = COSTS_N_INSNS (4);

	  if (speed)
	    *cost += 2 * extra_cost->alu.logical
		     + 2 * extra_cost->alu.arith;

	  return true;
	}

    /* Fall-through.  */
    case UMOD:
      if (speed)
	{
	  /* Slighly prefer UMOD over SMOD.  */
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else if (GET_MODE_CLASS (mode) == MODE_INT)
	    *cost += (extra_cost->mult[mode == DImode].add
		      + extra_cost->mult[mode == DImode].idiv
		      + (code == MOD ? 1 : 0));
	}
      return false;  /* All arguments need to be in registers.  */

    case DIV:
    case UDIV:
    case SQRT:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else if (GET_MODE_CLASS (mode) == MODE_INT)
	    /* There is no integer SQRT, so only DIV and UDIV can get
	       here.  */
	    *cost += (extra_cost->mult[mode == DImode].idiv
		     /* Slighly prefer UDIV over SDIV.  */
		     + (code == DIV ? 1 : 0));
	  else
	    *cost += extra_cost->fp[mode == DFmode].div;
	}
      return false;  /* All arguments need to be in registers.  */

    case IF_THEN_ELSE:
      return aarch64_if_then_else_costs (XEXP (x, 0), XEXP (x, 1),
					 XEXP (x, 2), cost, speed);

    case EQ:
    case NE:
    case GT:
    case GTU:
    case LT:
    case LTU:
    case GE:
    case GEU:
    case LE:
    case LEU:

      return false; /* All arguments must be in registers.  */

    case FMA:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      op2 = XEXP (x, 2);

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->fp[mode == DFmode].fma;
	}

      /* FMSUB, FNMADD, and FNMSUB are free.  */
      if (GET_CODE (op0) == NEG)
        op0 = XEXP (op0, 0);

      if (GET_CODE (op2) == NEG)
        op2 = XEXP (op2, 0);

      /* aarch64_fnma4_elt_to_64v2df has the NEG as operand 1,
	 and the by-element operand as operand 0.  */
      if (GET_CODE (op1) == NEG)
        op1 = XEXP (op1, 0);

      /* Catch vector-by-element operations.  The by-element operand can
	 either be (vec_duplicate (vec_select (x))) or just
	 (vec_select (x)), depending on whether we are multiplying by
	 a vector or a scalar.

	 Canonicalization is not very good in these cases, FMA4 will put the
	 by-element operand as operand 0, FNMA4 will have it as operand 1.  */
      if (GET_CODE (op0) == VEC_DUPLICATE)
	op0 = XEXP (op0, 0);
      else if (GET_CODE (op1) == VEC_DUPLICATE)
	op1 = XEXP (op1, 0);

      if (GET_CODE (op0) == VEC_SELECT)
	op0 = XEXP (op0, 0);
      else if (GET_CODE (op1) == VEC_SELECT)
	op1 = XEXP (op1, 0);

      /* If the remaining parameters are not registers,
         get the cost to put them into registers.  */
      *cost += rtx_cost (op0, mode, FMA, 0, speed);
      *cost += rtx_cost (op1, mode, FMA, 1, speed);
      *cost += rtx_cost (op2, mode, FMA, 2, speed);
      return true;

    case FLOAT:
    case UNSIGNED_FLOAT:
      if (speed)
	*cost += extra_cost->fp[mode == DFmode].fromint;
      return false;

    case FLOAT_EXTEND:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      /*Vector truncate.  */
	      *cost += extra_cost->vect.alu;
	    }
	  else
	    *cost += extra_cost->fp[mode == DFmode].widen;
	}
      return false;

    case FLOAT_TRUNCATE:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      /*Vector conversion.  */
	      *cost += extra_cost->vect.alu;
	    }
	  else
	    *cost += extra_cost->fp[mode == DFmode].narrow;
	}
      return false;

    case FIX:
    case UNSIGNED_FIX:
      x = XEXP (x, 0);
      /* Strip the rounding part.  They will all be implemented
         by the fcvt* family of instructions anyway.  */
      if (GET_CODE (x) == UNSPEC)
        {
          unsigned int uns_code = XINT (x, 1);

          if (uns_code == UNSPEC_FRINTA
              || uns_code == UNSPEC_FRINTM
              || uns_code == UNSPEC_FRINTN
              || uns_code == UNSPEC_FRINTP
              || uns_code == UNSPEC_FRINTZ)
            x = XVECEXP (x, 0, 0);
        }

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->fp[GET_MODE (x) == DFmode].toint;
	}

      /* We can combine fmul by a power of 2 followed by a fcvt into a single
	 fixed-point fcvt.  */
      if (GET_CODE (x) == MULT
	  && ((VECTOR_MODE_P (mode)
	       && aarch64_vec_fpconst_pow_of_2 (XEXP (x, 1)) > 0)
	      || aarch64_fpconst_pow_of_2 (XEXP (x, 1)) > 0))
	{
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed);
	  return true;
	}

      *cost += rtx_cost (x, VOIDmode, code, 0, speed);
      return true;

    case ABS:
      if (VECTOR_MODE_P (mode))
	{
	  /* ABS (vector).  */
	  if (speed)
	    *cost += extra_cost->vect.alu;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  op0 = XEXP (x, 0);

	  /* FABD, which is analogous to FADD.  */
	  if (GET_CODE (op0) == MINUS)
	    {
	      *cost += rtx_cost (XEXP (op0, 0), mode, MINUS, 0, speed);
	      *cost += rtx_cost (XEXP (op0, 1), mode, MINUS, 1, speed);
	      if (speed)
		*cost += extra_cost->fp[mode == DFmode].addsub;

	      return true;
	    }
	  /* Simple FABS is analogous to FNEG.  */
	  if (speed)
	    *cost += extra_cost->fp[mode == DFmode].neg;
	}
      else
	{
	  /* Integer ABS will either be split to
	     two arithmetic instructions, or will be an ABS
	     (scalar), which we don't model.  */
	  *cost = COSTS_N_INSNS (2);
	  if (speed)
	    *cost += 2 * extra_cost->alu.arith;
	}
      return false;

    case SMAX:
    case SMIN:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    {
	      /* FMAXNM/FMINNM/FMAX/FMIN.
	         TODO: This may not be accurate for all implementations, but
	         we do not model this in the cost tables.  */
	      *cost += extra_cost->fp[mode == DFmode].addsub;
	    }
	}
      return false;

    case UNSPEC:
      /* The floating point round to integer frint* instructions.  */
      if (aarch64_frint_unspec_p (XINT (x, 1)))
        {
          if (speed)
            *cost += extra_cost->fp[mode == DFmode].roundint;

          return false;
        }
      break;

    case TRUNCATE:

      /* Decompose <su>muldi3_highpart.  */
      if (/* (truncate:DI  */
	  mode == DImode
	  /*   (lshiftrt:TI  */
          && GET_MODE (XEXP (x, 0)) == TImode
          && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  /*      (mult:TI  */
          && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  /*        (ANY_EXTEND:TI (reg:DI))
	            (ANY_EXTEND:TI (reg:DI)))  */
          && ((GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
               && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == ZERO_EXTEND)
              || (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND
                  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == SIGN_EXTEND))
          && GET_MODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0)) == DImode
          && GET_MODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 1), 0)) == DImode
	  /*     (const_int 64)  */
          && CONST_INT_P (XEXP (XEXP (x, 0), 1))
          && UINTVAL (XEXP (XEXP (x, 0), 1)) == 64)
        {
          /* UMULH/SMULH.  */
	  if (speed)
	    *cost += extra_cost->mult[mode == DImode].extend;
	  *cost += rtx_cost (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0),
			     mode, MULT, 0, speed);
	  *cost += rtx_cost (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 1), 0),
			     mode, MULT, 1, speed);
          return true;
        }
	break;
    case CONST_VECTOR:
	{
	  /* Load using MOVI/MVNI.  */
	  if (aarch64_simd_valid_mov_imm (x))
	    *cost = extra_cost->vect.movi;
	  else /* Load using constant pool.  */
	    *cost = extra_cost->ldst.load;
	  break;
	}
    case VEC_CONCAT:
	/* depending on the operation, either DUP or INS.
	   For now, keep default costing.  */
	break;
    case VEC_DUPLICATE:
	/* Load using a DUP.  */
	*cost = extra_cost->vect.dup;
	return false;
    case VEC_SELECT:
	{
	  rtx op0 = XEXP (x, 0);
	  *cost = rtx_cost (op0, GET_MODE (op0), VEC_SELECT, 0, speed);

	  /* cost subreg of 0 as free, otherwise as DUP */
	  rtx op1 = XEXP (x, 1);
	  if (vec_series_lowpart_p (mode, GET_MODE (op1), op1))
	    ;
	  else if (vec_series_highpart_p (mode, GET_MODE (op1), op1))
	    *cost = extra_cost->vect.dup;
	  else
	    *cost = extra_cost->vect.extract;
	  return true;
	}
    default:
      break;
    }

  if (dump_file
      && flag_aarch64_verbose_cost)
    fprintf (dump_file,
      "\nFailed to cost RTX.  Assuming default cost.\n");

  return true;
}

/* Wrapper around aarch64_rtx_costs, dumps the partial, or total cost
   calculated for X.  This cost is stored in *COST.  Returns true
   if the total cost of X was calculated.  */
static bool
aarch64_rtx_costs_wrapper (rtx x, machine_mode mode, int outer,
		   int param, int *cost, bool speed)
{
  bool result = aarch64_rtx_costs (x, mode, outer, param, cost, speed);

  if (dump_file
      && flag_aarch64_verbose_cost)
    {
      print_rtl_single (dump_file, x);
      fprintf (dump_file, "\n%s cost: %d (%s)\n",
	       speed ? "Hot" : "Cold",
	       *cost, result ? "final" : "partial");
    }

  return result;
}

static int
aarch64_register_move_cost (machine_mode mode,
			    reg_class_t from_i, reg_class_t to_i)
{
  enum reg_class from = (enum reg_class) from_i;
  enum reg_class to = (enum reg_class) to_i;
  const struct cpu_regmove_cost *regmove_cost
    = aarch64_tune_params.regmove_cost;

  /* Trest any subset of POINTER_REGS as though it were GENERAL_REGS.  */
  if (reg_class_subset_p (to, POINTER_REGS))
    to = GENERAL_REGS;

  if (reg_class_subset_p (from, POINTER_REGS))
    from = GENERAL_REGS;

  /* Make RDFFR very expensive.  In particular, if we know that the FFR
     contains a PTRUE (e.g. after a SETFFR), we must never use RDFFR
     as a way of obtaining a PTRUE.  */
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
      && hard_reg_set_subset_p (reg_class_contents[from_i],
				reg_class_contents[FFR_REGS]))
    return 80;

  /* Moves to/from sysregs are expensive, and must go via GPR.  */
  if (from == MOVEABLE_SYSREGS)
    return 80 + aarch64_register_move_cost (mode, GENERAL_REGS, to);
  if (to == MOVEABLE_SYSREGS)
    return 80 + aarch64_register_move_cost (mode, from, GENERAL_REGS);

  /* Moving between GPR and stack cost is the same as GP2GP.  */
  if ((from == GENERAL_REGS && to == STACK_REG)
      || (to == GENERAL_REGS && from == STACK_REG))
    return regmove_cost->GP2GP;

  /* To/From the stack register, we move via the gprs.  */
  if (to == STACK_REG || from == STACK_REG)
    return aarch64_register_move_cost (mode, from, GENERAL_REGS)
            + aarch64_register_move_cost (mode, GENERAL_REGS, to);

  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags != (VEC_ADVSIMD | VEC_STRUCT | VEC_PARTIAL)
      && known_eq (GET_MODE_SIZE (mode), 16))
    {
      /* 128-bit operations on general registers require 2 instructions.  */
      if (from == GENERAL_REGS && to == GENERAL_REGS)
	return regmove_cost->GP2GP * 2;
      else if (from == GENERAL_REGS)
	return regmove_cost->GP2FP * 2;
      else if (to == GENERAL_REGS)
	return regmove_cost->FP2GP * 2;

      /* When AdvSIMD instructions are disabled it is not possible to move
	 a 128-bit value directly between Q registers.  This is handled in
	 secondary reload.  A general register is used as a scratch to move
	 the upper DI value and the lower DI value is moved directly,
	 hence the cost is the sum of three moves. */
      if (!TARGET_SIMD && !TARGET_SVE)
	return regmove_cost->GP2FP + regmove_cost->FP2GP + regmove_cost->FP2FP;

      return regmove_cost->FP2FP;
    }

  if (from == GENERAL_REGS && to == GENERAL_REGS)
    return regmove_cost->GP2GP;
  else if (from == GENERAL_REGS)
    return regmove_cost->GP2FP;
  else if (to == GENERAL_REGS)
    return regmove_cost->FP2GP;

  if (!TARGET_SIMD && vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
    {
      /* Needs a round-trip through memory, which can use LDP/STP for pairs.
	 The cost must be greater than 2 units to indicate that direct
	 moves aren't possible.  */
      auto per_vector = (aarch64_tune_params.memmov_cost.load_fp
			 + aarch64_tune_params.memmov_cost.store_fp);
      return MIN (CEIL (per_vector, 2), 4);
    }

  return regmove_cost->FP2FP;
}

/* Implements TARGET_MEMORY_MOVE_COST.  */
static int
aarch64_memory_move_cost (machine_mode mode, reg_class_t rclass_i, bool in)
{
  enum reg_class rclass = (enum reg_class) rclass_i;
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
      ? reg_classes_intersect_p (rclass, PR_REGS)
      : reg_class_subset_p (rclass, PR_REGS))
    return (in
	    ? aarch64_tune_params.memmov_cost.load_pred
	    : aarch64_tune_params.memmov_cost.store_pred);

  if (VECTOR_MODE_P (mode) || FLOAT_MODE_P (mode)
      ? reg_classes_intersect_p (rclass, FP_REGS)
      : reg_class_subset_p (rclass, FP_REGS))
    return (in
	    ? aarch64_tune_params.memmov_cost.load_fp
	    : aarch64_tune_params.memmov_cost.store_fp);

  return (in
	  ? aarch64_tune_params.memmov_cost.load_int
	  : aarch64_tune_params.memmov_cost.store_int);
}

/* Implement TARGET_INSN_COST.  We have the opportunity to do something
   much more productive here, such as using insn attributes to cost things.
   But we don't, not yet.

   The main point of this current definition is to make calling insn_cost
   on one instruction equivalent to calling seq_cost on a sequence that
   contains only that instruction.  The default definition would instead
   only look at SET_SRCs, ignoring SET_DESTs.

   This ensures that, for example, storing a 128-bit zero vector is more
   expensive than storing a 128-bit vector register.  A move of zero
   into a 128-bit vector register followed by multiple stores of that
   register is then cheaper than multiple stores of zero (which would
   use STP of XZR).  This in turn allows STP Qs to be formed.  */
static int
aarch64_insn_cost (rtx_insn *insn, bool speed)
{
  if (rtx set = single_set (insn))
    return set_rtx_cost (set, speed);
  return pattern_cost (PATTERN (insn), speed);
}

/* Implement TARGET_INIT_BUILTINS.  */
static void
aarch64_init_builtins ()
{
  aarch64_general_init_builtins ();
  aarch64_sve::init_builtins ();
#ifdef SUBTARGET_INIT_BUILTINS
  SUBTARGET_INIT_BUILTINS;
#endif
}

/* Implement TARGET_FOLD_BUILTIN.  */
static tree
aarch64_fold_builtin (tree fndecl, int nargs, tree *args, bool)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_fold_builtin (subcode, type, nargs, args);

    case AARCH64_BUILTIN_SVE:
      return NULL_TREE;
    }
  gcc_unreachable ();
}

/* Implement TARGET_GIMPLE_FOLD_BUILTIN.  */
static bool
aarch64_gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree fndecl = gimple_call_fndecl (stmt);
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  gimple *new_stmt = NULL;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      new_stmt = aarch64_general_gimple_fold_builtin (subcode, stmt, gsi);
      break;

    case AARCH64_BUILTIN_SVE:
      new_stmt = aarch64_sve::gimple_fold_builtin (subcode, gsi, stmt);
      break;
    }

  if (!new_stmt)
    return false;

  gsi_replace (gsi, new_stmt, false);
  return true;
}

/* Implement TARGET_EXPAND_BUILTIN.  */
static rtx
aarch64_expand_builtin (tree exp, rtx target, rtx, machine_mode, int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_expand_builtin (subcode, exp, target, ignore);

    case AARCH64_BUILTIN_SVE:
      return aarch64_sve::expand_builtin (subcode, exp, target);
    }
  gcc_unreachable ();
}

/* Implement TARGET_BUILTIN_DECL.  */
static tree
aarch64_builtin_decl (unsigned int code, bool initialize_p)
{
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_builtin_decl (subcode, initialize_p);

    case AARCH64_BUILTIN_SVE:
      return aarch64_sve::builtin_decl (subcode, initialize_p);
    }
  gcc_unreachable ();
}

/* Return true if it is safe and beneficial to use the approximate rsqrt optabs
   to optimize 1.0/sqrt.  */

static bool
use_rsqrt_p (machine_mode mode)
{
  return (!flag_trapping_math
	  && flag_unsafe_math_optimizations
	  && ((aarch64_tune_params.approx_modes->recip_sqrt
	       & AARCH64_APPROX_MODE (mode))
	      || flag_mrecip_low_precision_sqrt));
}

/* Function to decide when to use the approximate reciprocal square root
   builtin.  */

static tree
aarch64_builtin_reciprocal (tree fndecl)
{
  machine_mode mode = TYPE_MODE (TREE_TYPE (fndecl));

  if (!use_rsqrt_p (mode))
    return NULL_TREE;
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_builtin_rsqrt (subcode);

    case AARCH64_BUILTIN_SVE:
      return NULL_TREE;
    }
  gcc_unreachable ();
}

/* Emit code to perform the floating-point operation:

     DST = SRC1 * SRC2

   where all three operands are already known to be registers.
   If the operation is an SVE one, PTRUE is a suitable all-true
   predicate.  */

static void
aarch64_emit_mult (rtx dst, rtx ptrue, rtx src1, rtx src2)
{
  if (ptrue)
    emit_insn (gen_aarch64_pred (UNSPEC_COND_FMUL, GET_MODE (dst),
				 dst, ptrue, src1, src2,
				 gen_int_mode (SVE_RELAXED_GP, SImode)));
  else
    emit_set_insn (dst, gen_rtx_MULT (GET_MODE (dst), src1, src2));
}

/* Emit instruction sequence to compute either the approximate square root
   or its approximate reciprocal, depending on the flag RECP, and return
   whether the sequence was emitted or not.  */

bool
aarch64_emit_approx_sqrt (rtx dst, rtx src, bool recp)
{
  machine_mode mode = GET_MODE (dst);

  if (GET_MODE_INNER (mode) == HFmode)
    {
      gcc_assert (!recp);
      return false;
    }

  if (!recp)
    {
      if (!(flag_mlow_precision_sqrt
	    || (aarch64_tune_params.approx_modes->sqrt
		& AARCH64_APPROX_MODE (mode))))
	return false;

      if (!flag_finite_math_only
	  || flag_trapping_math
	  || !flag_unsafe_math_optimizations
	  || optimize_function_for_size_p (cfun))
	return false;
    }
  else
    /* Caller assumes we cannot fail.  */
    gcc_assert (use_rsqrt_p (mode));

  rtx pg = NULL_RTX;
  if (aarch64_sve_mode_p (mode))
    pg = aarch64_ptrue_reg (aarch64_sve_pred_mode (mode));
  machine_mode mmsk = (VECTOR_MODE_P (mode)
		       ? related_int_vector_mode (mode).require ()
		       : int_mode_for_mode (mode).require ());
  rtx xmsk = NULL_RTX;
  if (!recp)
    {
      /* When calculating the approximate square root, compare the
	 argument with 0.0 and create a mask.  */
      rtx zero = CONST0_RTX (mode);
      if (pg)
	{
	  xmsk = gen_reg_rtx (GET_MODE (pg));
	  rtx hint = gen_int_mode (SVE_KNOWN_PTRUE, SImode);
	  emit_insn (gen_aarch64_pred_fcm (UNSPEC_COND_FCMNE, mode,
					   xmsk, pg, hint, src, zero));
	}
      else
	{
	  xmsk = gen_reg_rtx (mmsk);
	  emit_insn (gen_rtx_SET (xmsk,
				  gen_rtx_NEG (mmsk,
					       gen_rtx_EQ (mmsk, src, zero))));
	}
    }

  /* Estimate the approximate reciprocal square root.  */
  rtx xdst = gen_reg_rtx (mode);
  emit_insn (gen_aarch64_rsqrte (mode, xdst, src));

  /* Iterate over the series twice for SF and thrice for DF.  */
  int iterations = (GET_MODE_INNER (mode) == DFmode) ? 3 : 2;

  /* Optionally iterate over the series once less for faster performance
     while sacrificing the accuracy.  */
  if ((recp && flag_mrecip_low_precision_sqrt)
      || (!recp && flag_mlow_precision_sqrt))
    iterations--;

  /* Iterate over the series to calculate the approximate reciprocal square
     root.  */
  rtx x1 = gen_reg_rtx (mode);
  while (iterations--)
    {
      rtx x2 = gen_reg_rtx (mode);
      aarch64_emit_mult (x2, pg, xdst, xdst);

      emit_insn (gen_aarch64_rsqrts (mode, x1, src, x2));

      if (iterations > 0)
	aarch64_emit_mult (xdst, pg, xdst, x1);
    }

  if (!recp)
    {
      if (pg)
	/* Multiply nonzero source values by the corresponding intermediate
	   result elements, so that the final calculation is the approximate
	   square root rather than its reciprocal.  Select a zero result for
	   zero source values, to avoid the Inf * 0 -> NaN that we'd get
	   otherwise.  */
	emit_insn (gen_cond (UNSPEC_COND_FMUL, mode,
			     xdst, xmsk, xdst, src, CONST0_RTX (mode)));
      else
	{
	  /* Qualify the approximate reciprocal square root when the
	     argument is 0.0 by squashing the intermediary result to 0.0.  */
	  rtx xtmp = gen_reg_rtx (mmsk);
	  emit_set_insn (xtmp, gen_rtx_AND (mmsk, gen_rtx_NOT (mmsk, xmsk),
					    gen_rtx_SUBREG (mmsk, xdst, 0)));
	  emit_move_insn (xdst, gen_rtx_SUBREG (mode, xtmp, 0));

	  /* Calculate the approximate square root.  */
	  aarch64_emit_mult (xdst, pg, xdst, src);
	}
    }

  /* Finalize the approximation.  */
  aarch64_emit_mult (dst, pg, xdst, x1);

  return true;
}

/* Emit the instruction sequence to compute the approximation for the division
   of NUM by DEN in QUO and return whether the sequence was emitted or not.  */

bool
aarch64_emit_approx_div (rtx quo, rtx num, rtx den)
{
  machine_mode mode = GET_MODE (quo);

  if (GET_MODE_INNER (mode) == HFmode)
    return false;

  bool use_approx_division_p = (flag_mlow_precision_div
			        || (aarch64_tune_params.approx_modes->division
				    & AARCH64_APPROX_MODE (mode)));

  if (!flag_finite_math_only
      || flag_trapping_math
      || !flag_unsafe_math_optimizations
      || optimize_function_for_size_p (cfun)
      || !use_approx_division_p)
    return false;

  if (!TARGET_SIMD && VECTOR_MODE_P (mode))
    return false;

  rtx pg = NULL_RTX;
  if (aarch64_sve_mode_p (mode))
    pg = aarch64_ptrue_reg (aarch64_sve_pred_mode (mode));

  /* Estimate the approximate reciprocal.  */
  rtx xrcp = gen_reg_rtx (mode);
  emit_insn (gen_aarch64_frecpe (mode, xrcp, den));

  /* Iterate over the series twice for SF and thrice for DF.  */
  int iterations = (GET_MODE_INNER (mode) == DFmode) ? 3 : 2;

  /* Optionally iterate over the series less for faster performance,
     while sacrificing the accuracy.  The default is 2 for DF and 1 for SF.  */
  if (flag_mlow_precision_div)
    iterations = (GET_MODE_INNER (mode) == DFmode
		  ? aarch64_double_recp_precision
		  : aarch64_float_recp_precision);

  /* Iterate over the series to calculate the approximate reciprocal.  */
  rtx xtmp = gen_reg_rtx (mode);
  while (iterations--)
    {
      emit_insn (gen_aarch64_frecps (mode, xtmp, xrcp, den));

      if (iterations > 0)
	aarch64_emit_mult (xrcp, pg, xrcp, xtmp);
    }

  if (num != CONST1_RTX (mode))
    {
      /* As the approximate reciprocal of DEN is already calculated, only
	 calculate the approximate division when NUM is not 1.0.  */
      rtx xnum = force_reg (mode, num);
      aarch64_emit_mult (xrcp, pg, xrcp, xnum);
    }

  /* Finalize the approximation.  */
  aarch64_emit_mult (quo, pg, xrcp, xtmp);
  return true;
}

/* Emit an optimized sequence to perform a vector rotate
   of REG by the vector constant amount AMNT_VEC and place the result
   in DST.  Return true iff successful.  */

bool
aarch64_emit_opt_vec_rotate (rtx dst, rtx reg, rtx amnt_vec)
{
  rtx amnt = unwrap_const_vec_duplicate (amnt_vec);
  gcc_assert (CONST_INT_P (amnt));
  HOST_WIDE_INT rotamnt = UINTVAL (amnt);
  machine_mode mode = GET_MODE (reg);
  /* Don't end up here after reload.  */
  gcc_assert (can_create_pseudo_p ());
  /* Rotates by half the element width map down to REV* instructions and should
     always be preferred when possible.  */
  if (rotamnt == GET_MODE_UNIT_BITSIZE (mode) / 2
      && expand_rotate_as_vec_perm (mode, dst, reg, amnt))
    return true;
  /* 64 and 128-bit vector modes can use the XAR instruction
     when available.  */
  else if ((TARGET_SHA3 && mode == V2DImode)
	   || (TARGET_SVE2
	       && (known_eq (GET_MODE_SIZE (mode), 8)
		   || known_eq (GET_MODE_SIZE (mode), 16))))
    {
      rtx zeroes = aarch64_gen_shareable_zero (mode);
      rtx xar_op
	= gen_rtx_ROTATE (mode, gen_rtx_XOR (mode, reg, zeroes),
						amnt_vec);
      emit_set_insn (dst, xar_op);
      return true;
    }
  /* If none of the above, try to expand rotates by any byte amount as
     permutes.  */
  else if (expand_rotate_as_vec_perm (mode, dst, reg, amnt))
    return true;
  return false;
}

/* Return the number of instructions that can be issued per cycle.  */
static int
aarch64_sched_issue_rate (void)
{
  return aarch64_tune_params.issue_rate;
}

/* Implement TARGET_SCHED_VARIABLE_ISSUE.  */
static int
aarch64_sched_variable_issue (FILE *, int, rtx_insn *insn, int more)
{
  if (DEBUG_INSN_P (insn))
    return more;

  rtx_code code = GET_CODE (PATTERN (insn));
  if (code == USE || code == CLOBBER)
    return more;

  if (get_attr_type (insn) == TYPE_NO_INSN)
    return more;

  return more - 1;
}

static int
aarch64_sched_first_cycle_multipass_dfa_lookahead (void)
{
  int issue_rate = aarch64_sched_issue_rate ();

  return issue_rate > 1 && !sched_fusion ? issue_rate : 0;
}


/* Implement TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD as
   autopref_multipass_dfa_lookahead_guard from haifa-sched.cc.  It only
   has an effect if PARAM_SCHED_AUTOPREF_QUEUE_DEPTH > 0.  */

static int
aarch64_first_cycle_multipass_dfa_lookahead_guard (rtx_insn *insn,
						    int ready_index)
{
  return autopref_multipass_dfa_lookahead_guard (insn, ready_index);
}


/* Vectorizer cost model target hooks.  */

/* If a vld1 from address ADDR should be recorded in vector_load_decls,
   return the decl that should be recorded.  Return null otherwise.  */
tree
aarch64_vector_load_decl (tree addr)
{
  if (TREE_CODE (addr) != ADDR_EXPR)
    return NULL_TREE;
  tree base = get_base_address (TREE_OPERAND (addr, 0));
  if (TREE_CODE (base) != VAR_DECL)
    return NULL_TREE;
  return base;
}

/* Return true if STMT_INFO accesses a decl that is known to be the
   argument to a vld1 in the same function.  */
static bool
aarch64_accesses_vector_load_decl_p (stmt_vec_info stmt_info)
{
  if (!cfun->machine->vector_load_decls)
    return false;
  auto dr = STMT_VINFO_DATA_REF (stmt_info);
  if (!dr)
    return false;
  tree decl = aarch64_vector_load_decl (DR_BASE_ADDRESS (dr));
  return decl && cfun->machine->vector_load_decls->contains (decl);
}

/* Information about how the CPU would issue the scalar, Advanced SIMD
   or SVE version of a vector loop, using the scheme defined by the
   aarch64_base_vec_issue_info hierarchy of structures.  */
class aarch64_vec_op_count
{
public:
  aarch64_vec_op_count () = default;
  aarch64_vec_op_count (const aarch64_vec_issue_info *, unsigned int,
			unsigned int = 1);

  unsigned int vec_flags () const { return m_vec_flags; }
  unsigned int vf_factor () const { return m_vf_factor; }

  const aarch64_base_vec_issue_info *base_issue_info () const;
  const aarch64_simd_vec_issue_info *simd_issue_info () const;
  const aarch64_sve_vec_issue_info *sve_issue_info () const;

  fractional_cost rename_cycles_per_iter () const;
  fractional_cost min_nonpred_cycles_per_iter () const;
  fractional_cost min_pred_cycles_per_iter () const;
  fractional_cost min_cycles_per_iter () const;

  void dump () const;

  /* The number of individual "general" operations.  See the comments
     in aarch64_base_vec_issue_info for details.  */
  unsigned int general_ops = 0;

  /* The number of load and store operations, under the same scheme
     as above.  */
  unsigned int loads = 0;
  unsigned int stores = 0;

  /* The minimum number of cycles needed to execute all loop-carried
     operations, which in the vector code become associated with
     reductions.  */
  unsigned int reduction_latency = 0;

  /* The number of individual predicate operations.  See the comments
     in aarch64_sve_vec_issue_info for details.  */
  unsigned int pred_ops = 0;

private:
  /* The issue information for the core.  */
  const aarch64_vec_issue_info *m_issue_info = nullptr;

  /* - If M_VEC_FLAGS is zero then this structure describes scalar code
     - If M_VEC_FLAGS & VEC_ADVSIMD is nonzero then this structure describes
       Advanced SIMD code.
     - If M_VEC_FLAGS & VEC_ANY_SVE is nonzero then this structure describes
       SVE code.  */
  unsigned int m_vec_flags = 0;

  /* Assume that, when the code is executing on the core described
     by M_ISSUE_INFO, one iteration of the loop will handle M_VF_FACTOR
     times more data than the vectorizer anticipates.

     This is only ever different from 1 for SVE.  It allows us to consider
     what would happen on a 256-bit SVE target even when the -mtune
     parameters say that the “likely” SVE length is 128 bits.  */
  unsigned int m_vf_factor = 1;
};

aarch64_vec_op_count::
aarch64_vec_op_count (const aarch64_vec_issue_info *issue_info,
		      unsigned int vec_flags, unsigned int vf_factor)
  : m_issue_info (issue_info),
    m_vec_flags (vec_flags),
    m_vf_factor (vf_factor)
{
}

/* Return the base issue information (i.e. the parts that make sense
   for both scalar and vector code).  Return null if we have no issue
   information.  */
const aarch64_base_vec_issue_info *
aarch64_vec_op_count::base_issue_info () const
{
  if (auto *ret = simd_issue_info ())
    return ret;
  return m_issue_info->scalar;
}

/* If the structure describes vector code and we have associated issue
   information, return that issue information, otherwise return null.  */
const aarch64_simd_vec_issue_info *
aarch64_vec_op_count::simd_issue_info () const
{
  if (auto *ret = sve_issue_info ())
    return ret;
  if (m_vec_flags)
    return m_issue_info->advsimd;
  return nullptr;
}

/* If the structure describes SVE code and we have associated issue
   information, return that issue information, otherwise return null.  */
const aarch64_sve_vec_issue_info *
aarch64_vec_op_count::sve_issue_info () const
{
  if (m_vec_flags & VEC_ANY_SVE)
    return m_issue_info->sve;
  return nullptr;
}

/* Estimate the minimum number of cycles per iteration needed to rename
   the instructions.

   ??? For now this is done inline rather than via cost tables, since it
   isn't clear how it should be parameterized for the general case.  */
fractional_cost
aarch64_vec_op_count::rename_cycles_per_iter () const
{
  if (sve_issue_info () == &neoverse512tvb_sve_issue_info
      || sve_issue_info () == &neoversen2_sve_issue_info
      || sve_issue_info () == &neoversev2_sve_issue_info)
    /* + 1 for an addition.  We've already counted a general op for each
       store, so we don't need to account for stores separately.  The branch
       reads no registers and so does not need to be counted either.

       ??? This value is very much on the pessimistic side, but seems to work
       pretty well in practice.  */
    return { general_ops + loads + pred_ops + 1, 5 };

  return 0;
}

/* Like min_cycles_per_iter, but excluding predicate operations.  */
fractional_cost
aarch64_vec_op_count::min_nonpred_cycles_per_iter () const
{
  auto *issue_info = base_issue_info ();

  fractional_cost cycles = MAX (reduction_latency, 1);
  cycles = std::max (cycles, { stores, issue_info->stores_per_cycle });
  cycles = std::max (cycles, { loads + stores,
			       issue_info->loads_stores_per_cycle });
  cycles = std::max (cycles, { general_ops,
			       issue_info->general_ops_per_cycle });
  cycles = std::max (cycles, rename_cycles_per_iter ());
  return cycles;
}

/* Like min_cycles_per_iter, but including only the predicate operations.  */
fractional_cost
aarch64_vec_op_count::min_pred_cycles_per_iter () const
{
  if (auto *issue_info = sve_issue_info ())
    return { pred_ops, issue_info->pred_ops_per_cycle };
  return 0;
}

/* Estimate the minimum number of cycles needed to issue the operations.
   This is a very simplistic model!  */
fractional_cost
aarch64_vec_op_count::min_cycles_per_iter () const
{
  return std::max (min_nonpred_cycles_per_iter (),
		   min_pred_cycles_per_iter ());
}

/* Dump information about the structure.  */
void
aarch64_vec_op_count::dump () const
{
  dump_printf_loc (MSG_NOTE, vect_location,
		   "  load operations = %d\n", loads);
  dump_printf_loc (MSG_NOTE, vect_location,
		   "  store operations = %d\n", stores);
  dump_printf_loc (MSG_NOTE, vect_location,
		   "  general operations = %d\n", general_ops);
  if (sve_issue_info ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "  predicate operations = %d\n", pred_ops);
  dump_printf_loc (MSG_NOTE, vect_location,
		   "  reduction latency = %d\n", reduction_latency);
  if (auto rcpi = rename_cycles_per_iter ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "  estimated cycles per iteration to rename = %f\n",
		     rcpi.as_double ());
  if (auto pred_cpi = min_pred_cycles_per_iter ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "  estimated min cycles per iteration"
		       " without predication = %f\n",
		       min_nonpred_cycles_per_iter ().as_double ());
      dump_printf_loc (MSG_NOTE, vect_location,
		       "  estimated min cycles per iteration"
		       " for predication = %f\n", pred_cpi.as_double ());
    }
  if (auto cpi = min_cycles_per_iter ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "  estimated min cycles per iteration = %f\n",
		     cpi.as_double ());
}

/* Information about vector code that we're in the process of costing.  */
class aarch64_vector_costs : public vector_costs
{
public:
  aarch64_vector_costs (vec_info *, bool);

  unsigned int add_stmt_cost (int count, vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info, slp_tree, tree vectype,
			      int misalign,
			      vect_cost_model_location where) override;
  void finish_cost (const vector_costs *) override;
  bool better_main_loop_than_p (const vector_costs *other) const override;

private:
  void record_potential_advsimd_unrolling (loop_vec_info);
  void analyze_loop_vinfo (loop_vec_info);
  void count_ops (unsigned int, vect_cost_for_stmt, stmt_vec_info, slp_tree,
		  aarch64_vec_op_count *);
  fractional_cost adjust_body_cost_sve (const aarch64_vec_op_count *,
					fractional_cost, unsigned int,
					unsigned int *, bool *);
  unsigned int adjust_body_cost (loop_vec_info, const aarch64_vector_costs *,
				 unsigned int);
  bool prefer_unrolled_loop () const;
  unsigned int determine_suggested_unroll_factor ();

  /* True if we have performed one-time initialization based on the
     vec_info.  */
  bool m_analyzed_vinfo = false;

  /* This loop uses an average operation that is not supported by SVE, but is
     supported by Advanced SIMD and SVE2.  */
  bool m_has_avg = false;

  /* Additional initialization costs for using gather or scatter operation in
     the current loop.  */
  unsigned int m_sve_gather_scatter_init_cost = 0;

  /* True if the vector body contains a store to a decl and if the
     function is known to have a vld1 from the same decl.

     In the Advanced SIMD ACLE, the recommended endian-agnostic way of
     initializing a vector is:

       float f[4] = { elts };
       float32x4_t x = vld1q_f32(f);

     We should strongly prefer vectorization of the initialization of f,
     so that the store to f and the load back can be optimized away,
     leaving a vectorization of { elts }.  */
  bool m_stores_to_vector_load_decl = false;

  /* Non-zero if the last operation we costed is a vector promotion or demotion.
     In this case the value is the number of insns in the last operation.

     On AArch64 vector promotion and demotions require us to first widen or
     narrow the input and only after that emit conversion instructions.  For
     costing this means we need to emit the cost of the final conversions as
     well.  */
  unsigned int m_num_last_promote_demote = 0;

  /* - If M_VEC_FLAGS is zero then we're costing the original scalar code.
     - If M_VEC_FLAGS & VEC_ADVSIMD is nonzero then we're costing Advanced
       SIMD code.
     - If M_VEC_FLAGS & VEC_ANY_SVE is nonzero then we're costing SVE code.  */
  unsigned int m_vec_flags = 0;

  /* At the moment, we do not model LDP and STP in the vector and scalar costs.
     This means that code such as:

	a[0] = x;
	a[1] = x;

     will be costed as two scalar instructions and two vector instructions
     (a scalar_to_vec and an unaligned_store).  For SLP, the vector form
     wins if the costs are equal, because of the fact that the vector costs
     include constant initializations whereas the scalar costs don't.
     We would therefore tend to vectorize the code above, even though
     the scalar version can use a single STP.

     We should eventually fix this and model LDP and STP in the main costs;
     see the comment in aarch64_sve_adjust_stmt_cost for some of the problems.
     Until then, we look specifically for code that does nothing more than
     STP-like operations.  We cost them on that basis in addition to the
     normal latency-based costs.

     If the scalar or vector code could be a sequence of STPs +
     initialization, this variable counts the cost of the sequence,
     with 2 units per instruction.  The variable is ~0U for other
     kinds of code.  */
  unsigned int m_stp_sequence_cost = 0;

  /* On some CPUs, SVE and Advanced SIMD provide the same theoretical vector
     throughput, such as 4x128 Advanced SIMD vs. 2x256 SVE.  In those
     situations, we try to predict whether an Advanced SIMD implementation
     of the loop could be completely unrolled and become straight-line code.
     If so, it is generally better to use the Advanced SIMD version rather
     than length-agnostic SVE, since the SVE loop would execute an unknown
     number of times and so could not be completely unrolled in the same way.

     If we're applying this heuristic, M_UNROLLED_ADVSIMD_NITERS is the
     number of Advanced SIMD loop iterations that would be unrolled and
     M_UNROLLED_ADVSIMD_STMTS estimates the total number of statements
     in the unrolled loop.  Both values are zero if we're not applying
     the heuristic.  */
  unsigned HOST_WIDE_INT m_unrolled_advsimd_niters = 0;
  unsigned HOST_WIDE_INT m_unrolled_advsimd_stmts = 0;

  /* If we're vectorizing a loop that executes a constant number of times,
     this variable gives the number of times that the vector loop would
     iterate, otherwise it is zero.  */
  uint64_t m_num_vector_iterations = 0;

  /* Used only when vectorizing loops.  Estimates the number and kind of
     operations that would be needed by one iteration of the scalar
     or vector loop.  There is one entry for each tuning option of
     interest.  */
  auto_vec<aarch64_vec_op_count, 2> m_ops;
};

aarch64_vector_costs::aarch64_vector_costs (vec_info *vinfo,
					    bool costing_for_scalar)
  : vector_costs (vinfo, costing_for_scalar),
    m_vec_flags (costing_for_scalar ? 0
		 : aarch64_classify_vector_mode (vinfo->vector_mode))
{
  if (auto *issue_info = aarch64_tune_params.vec_costs->issue_info)
    {
      m_ops.quick_push ({ issue_info, m_vec_flags });
      if (aarch64_tune_params.vec_costs == &neoverse512tvb_vector_cost)
	{
	  unsigned int vf_factor = (m_vec_flags & VEC_ANY_SVE) ? 2 : 1;
	  m_ops.quick_push ({ &neoversev1_vec_issue_info, m_vec_flags,
			      vf_factor });
	}
    }
}

/* Implement TARGET_VECTORIZE_CREATE_COSTS.  */
vector_costs *
aarch64_vectorize_create_costs (vec_info *vinfo, bool costing_for_scalar)
{
  return new aarch64_vector_costs (vinfo, costing_for_scalar);
}

/* Return true if the current CPU should use the new costs defined
   in GCC 11.  This should be removed for GCC 12 and above, with the
   costs applying to all CPUs instead.  */
static bool
aarch64_use_new_vector_costs_p ()
{
  return (aarch64_tune_params.extra_tuning_flags
	  & AARCH64_EXTRA_TUNE_USE_NEW_VECTOR_COSTS);
}

/* Return the appropriate SIMD costs for vectors of type VECTYPE.  */
static const simd_vec_cost *
aarch64_simd_vec_costs (tree vectype)
{
  const cpu_vector_cost *costs = aarch64_tune_params.vec_costs;
  if (vectype != NULL
      && aarch64_sve_mode_p (TYPE_MODE (vectype))
      && costs->sve != NULL)
    return costs->sve;
  return costs->advsimd;
}

/* Return the appropriate SIMD costs for vectors with VEC_* flags FLAGS.  */
static const simd_vec_cost *
aarch64_simd_vec_costs_for_flags (unsigned int flags)
{
  const cpu_vector_cost *costs = aarch64_tune_params.vec_costs;
  if ((flags & VEC_ANY_SVE) && costs->sve)
    return costs->sve;
  return costs->advsimd;
}

/* If STMT_INFO is a memory reference, return the scalar memory type,
   otherwise return null.  */
static tree
aarch64_dr_type (stmt_vec_info stmt_info)
{
  if (auto dr = STMT_VINFO_DATA_REF (stmt_info))
    return TREE_TYPE (DR_REF (dr));
  return NULL_TREE;
}

/* Decide whether to use the unrolling heuristic described above
   m_unrolled_advsimd_niters, updating that field if so.  LOOP_VINFO
   describes the loop that we're vectorizing.  */
void
aarch64_vector_costs::
record_potential_advsimd_unrolling (loop_vec_info loop_vinfo)
{
  /* The heuristic only makes sense on targets that have the same
     vector throughput for SVE and Advanced SIMD.  */
  if (!(aarch64_tune_params.extra_tuning_flags
	& AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT))
    return;

  /* We only want to apply the heuristic if LOOP_VINFO is being
     vectorized for SVE.  */
  if (!(m_vec_flags & VEC_ANY_SVE))
    return;

  /* Check whether it is possible in principle to use Advanced SIMD
     instead.  */
  if (aarch64_autovec_preference == AARCH64_AUTOVEC_SVE_ONLY)
    return;

  /* We don't want to apply the heuristic to outer loops, since it's
     harder to track two levels of unrolling.  */
  if (LOOP_VINFO_LOOP (loop_vinfo)->inner)
    return;

  /* Only handle cases in which the number of Advanced SIMD iterations
     would be known at compile time but the number of SVE iterations
     would not.  */
  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || aarch64_sve_vg.is_constant ())
    return;

  /* Guess how many times the Advanced SIMD loop would iterate and make
     sure that it is within the complete unrolling limit.  Even if the
     number of iterations is small enough, the number of statements might
     not be, which is why we need to estimate the number of statements too.  */
  unsigned int estimated_vq = aarch64_estimated_sve_vq ();
  unsigned int advsimd_vf = CEIL (vect_vf_for_cost (loop_vinfo), estimated_vq);
  unsigned HOST_WIDE_INT unrolled_advsimd_niters
    = LOOP_VINFO_INT_NITERS (loop_vinfo) / advsimd_vf;
  if (unrolled_advsimd_niters > (unsigned int) param_max_completely_peel_times)
    return;

  /* Record that we're applying the heuristic and should try to estimate
     the number of statements in the Advanced SIMD loop.  */
  m_unrolled_advsimd_niters = unrolled_advsimd_niters;
}

/* Do one-time initialization of the aarch64_vector_costs given that we're
   costing the loop vectorization described by LOOP_VINFO.  */
void
aarch64_vector_costs::analyze_loop_vinfo (loop_vec_info loop_vinfo)
{
  /* Record the number of times that the vector loop would execute,
     if known.  */
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  auto scalar_niters = max_stmt_executions_int (loop);
  if (scalar_niters >= 0)
    {
      unsigned int vf = vect_vf_for_cost (loop_vinfo);
      if (LOOP_VINFO_MASKS (loop_vinfo).is_empty ())
	m_num_vector_iterations = scalar_niters / vf;
      else
	m_num_vector_iterations = CEIL (scalar_niters, vf);
    }

  /* Detect whether we're vectorizing for SVE and should apply the unrolling
     heuristic described above m_unrolled_advsimd_niters.  */
  record_potential_advsimd_unrolling (loop_vinfo);
}

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
aarch64_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				    tree vectype,
				    int misalign ATTRIBUTE_UNUSED)
{
  unsigned elements;
  const cpu_vector_cost *costs = aarch64_tune_params.vec_costs;
  bool fp = false;

  if (vectype != NULL)
    fp = FLOAT_TYPE_P (vectype);

  const simd_vec_cost *simd_costs = aarch64_simd_vec_costs (vectype);

  switch (type_of_cost)
    {
      case scalar_stmt:
	return fp ? costs->scalar_fp_stmt_cost : costs->scalar_int_stmt_cost;

      case scalar_load:
	return costs->scalar_load_cost;

      case scalar_store:
	return costs->scalar_store_cost;

      case vector_stmt:
	return fp ? simd_costs->fp_stmt_cost
		  : simd_costs->int_stmt_cost;

      case vector_load:
	return simd_costs->align_load_cost;

      case vector_store:
	return simd_costs->store_cost;

      case vec_to_scalar:
	return simd_costs->vec_to_scalar_cost;

      case scalar_to_vec:
	return simd_costs->scalar_to_vec_cost;

      case unaligned_load:
      case vector_gather_load:
	return simd_costs->unalign_load_cost;

      case unaligned_store:
      case vector_scatter_store:
	return simd_costs->unalign_store_cost;

      case cond_branch_taken:
	return costs->cond_taken_branch_cost;

      case cond_branch_not_taken:
	return costs->cond_not_taken_branch_cost;

      case vec_perm:
	return simd_costs->permute_cost;

      case vec_promote_demote:
	return fp ? simd_costs->fp_stmt_cost
		  : simd_costs->int_stmt_cost;

      case vec_construct:
	elements = estimated_poly_value (TYPE_VECTOR_SUBPARTS (vectype));
	return elements / 2 + 1;

      default:
	gcc_unreachable ();
    }
}

/* Return true if an access of kind KIND for STMT_INFO (or NODE if SLP)
   represents one vector of an LD[234] or ST[234] operation.  Return the total
   number of vectors (2, 3 or 4) if so, otherwise return a value outside that
   range.  */
static int
aarch64_ld234_st234_vectors (vect_cost_for_stmt kind, stmt_vec_info stmt_info,
			     slp_tree node)
{
  if ((kind == vector_load
       || kind == unaligned_load
       || kind == vector_store
       || kind == unaligned_store)
      && STMT_VINFO_DATA_REF (stmt_info))
    {
      stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      if (stmt_info
	  && vect_mem_access_type (stmt_info, node) == VMAT_LOAD_STORE_LANES)
	return DR_GROUP_SIZE (stmt_info);
    }
  return 0;
}

/* Return true if creating multiple copies of STMT_INFO for Advanced SIMD
   vectors would produce a series of LDP or STP operations.  KIND is the
   kind of statement that STMT_INFO represents.  */
static bool
aarch64_advsimd_ldp_stp_p (enum vect_cost_for_stmt kind,
			   stmt_vec_info stmt_info)
{
  switch (kind)
    {
    case vector_load:
    case vector_store:
    case unaligned_load:
    case unaligned_store:
      break;

    default:
      return false;
    }

  return is_gimple_assign (stmt_info->stmt);
}

/* Return true if STMT_INFO is the second part of a two-statement multiply-add
   or multiply-subtract sequence that might be suitable for fusing into a
   single instruction.  If VEC_FLAGS is zero, analyze the operation as
   a scalar one, otherwise analyze it as an operation on vectors with those
   VEC_* flags.  */
static bool
aarch64_multiply_add_p (vec_info *vinfo, stmt_vec_info stmt_info,
			unsigned int vec_flags)
{
  gassign *assign = dyn_cast<gassign *> (stmt_info->stmt);
  if (!assign)
    return false;
  tree_code code = gimple_assign_rhs_code (assign);
  if (code != PLUS_EXPR && code != MINUS_EXPR)
    return false;

  auto is_mul_result = [&](int i)
    {
      tree rhs = gimple_op (assign, i);
      /* ??? Should we try to check for a single use as well?  */
      if (TREE_CODE (rhs) != SSA_NAME)
	return false;

      stmt_vec_info def_stmt_info = vinfo->lookup_def (rhs);
      if (!def_stmt_info
	  || STMT_VINFO_DEF_TYPE (def_stmt_info) != vect_internal_def)
	return false;
      gassign *rhs_assign = dyn_cast<gassign *> (def_stmt_info->stmt);
      if (!rhs_assign || gimple_assign_rhs_code (rhs_assign) != MULT_EXPR)
	return false;

      if (vec_flags & VEC_ADVSIMD)
	{
	  /* Scalar and SVE code can tie the result to any FMLA input (or none,
	     although that requires a MOVPRFX for SVE).  However, Advanced SIMD
	     only supports MLA forms, so will require a move if the result
	     cannot be tied to the accumulator.  The most important case in
	     which this is true is when the accumulator input is invariant.  */
	  rhs = gimple_op (assign, 3 - i);
	  if (TREE_CODE (rhs) != SSA_NAME)
	    return false;
	  def_stmt_info = vinfo->lookup_def (rhs);
	  if (!def_stmt_info
	      || STMT_VINFO_DEF_TYPE (def_stmt_info) == vect_external_def
	      || STMT_VINFO_DEF_TYPE (def_stmt_info) == vect_constant_def)
	    return false;
	}

      return true;
    };

  if (code == MINUS_EXPR && (vec_flags & VEC_ADVSIMD))
    /* Advanced SIMD doesn't have FNMADD/FNMSUB/FNMLA/FNMLS, so the
       multiplication must be on the second operand (to form an FMLS).
       But if both operands are multiplications and the second operand
       is used more than once, we'll instead negate the second operand
       and use it as an accumulator for the first operand.  */
    return (is_mul_result (2)
	    && (has_single_use (gimple_assign_rhs2 (assign))
		|| !is_mul_result (1)));

  return is_mul_result (1) || is_mul_result (2);
}

/* Return true if STMT_INFO is the second part of a two-statement boolean AND
   expression sequence that might be suitable for fusing into a
   single instruction.  If VEC_FLAGS is zero, analyze the operation as
   a scalar one, otherwise analyze it as an operation on vectors with those
   VEC_* flags.  */

static bool
aarch64_bool_compound_p (vec_info *vinfo, stmt_vec_info stmt_info,
			 unsigned int vec_flags)
{
  gassign *assign = dyn_cast<gassign *> (stmt_info->stmt);
  if (!assign
      || gimple_assign_rhs_code (assign) != BIT_AND_EXPR
      || !STMT_VINFO_VECTYPE (stmt_info)
      || !VECTOR_BOOLEAN_TYPE_P (STMT_VINFO_VECTYPE (stmt_info)))
    return false;

  for (int i = 1; i < 3; ++i)
    {
      tree rhs = gimple_op (assign, i);

      if (TREE_CODE (rhs) != SSA_NAME)
	continue;

      stmt_vec_info def_stmt_info = vinfo->lookup_def (rhs);
      if (!def_stmt_info
	  || STMT_VINFO_DEF_TYPE (def_stmt_info) != vect_internal_def)
	continue;

      gassign *rhs_assign = dyn_cast<gassign *> (def_stmt_info->stmt);
      if (!rhs_assign
	  || TREE_CODE_CLASS (gimple_assign_rhs_code (rhs_assign))
		!= tcc_comparison)
	continue;

      if (vec_flags & VEC_ADVSIMD)
	return false;

      return true;
    }
  return false;
}

/* We are considering implementing STMT_INFO using SVE.  If STMT_INFO is an
   in-loop reduction that SVE supports directly, return its latency in cycles,
   otherwise return zero.  SVE_COSTS specifies the latencies of the relevant
   instructions.  */
static unsigned int
aarch64_sve_in_loop_reduction_latency (vec_info *vinfo,
				       stmt_vec_info stmt_info,
				       const sve_vec_cost *sve_costs)
{
  switch (vect_reduc_type (vinfo, stmt_info))
    {
    case EXTRACT_LAST_REDUCTION:
      return sve_costs->clast_cost;

    case FOLD_LEFT_REDUCTION:
      switch (TYPE_MODE (TREE_TYPE (gimple_get_lhs (stmt_info->stmt))))
	{
	case E_HFmode:
	case E_BFmode:
	  return sve_costs->fadda_f16_cost;

	case E_SFmode:
	  return sve_costs->fadda_f32_cost;

	case E_DFmode:
	  return sve_costs->fadda_f64_cost;

	default:
	  break;
	}
      break;
    }

  return 0;
}

/* STMT_INFO describes a loop-carried operation in the original scalar code
   that we are considering implementing as a reduction.  Return one of the
   following values, depending on VEC_FLAGS:

   - If VEC_FLAGS is zero, return the loop carry latency of the original
     scalar operation.

   - If VEC_FLAGS & VEC_ADVSIMD, return the loop carry latency of the
     Advanced SIMD implementation.

   - If VEC_FLAGS & VEC_ANY_SVE, return the loop carry latency of the
     SVE implementation.  */
static unsigned int
aarch64_in_loop_reduction_latency (vec_info *vinfo, stmt_vec_info stmt_info,
				   unsigned int vec_flags)
{
  const cpu_vector_cost *vec_costs = aarch64_tune_params.vec_costs;
  const sve_vec_cost *sve_costs = nullptr;
  if (vec_flags & VEC_ANY_SVE)
    sve_costs = aarch64_tune_params.vec_costs->sve;

  /* If the caller is asking for the SVE latency, check for forms of reduction
     that only SVE can handle directly.  */
  if (sve_costs)
    {
      unsigned int latency
	= aarch64_sve_in_loop_reduction_latency (vinfo, stmt_info, sve_costs);
      if (latency)
	return latency;
    }

  /* Handle scalar costs.  */
  bool is_float = FLOAT_TYPE_P (TREE_TYPE (gimple_get_lhs (stmt_info->stmt)));
  if (vec_flags == 0)
    {
      if (is_float)
	return vec_costs->scalar_fp_stmt_cost;
      return vec_costs->scalar_int_stmt_cost;
    }

  /* Otherwise, the loop body just contains normal integer or FP operations,
     with a vector reduction outside the loop.  */
  const simd_vec_cost *simd_costs
    = aarch64_simd_vec_costs_for_flags (vec_flags);
  if (is_float)
    return simd_costs->fp_stmt_cost;
  return simd_costs->int_stmt_cost;
}

/* STMT_COST is the cost calculated by aarch64_builtin_vectorization_cost
   for STMT_INFO, which has cost kind KIND.  If this is a scalar operation,
   try to subdivide the target-independent categorization provided by KIND
   to get a more accurate cost.  */
static fractional_cost
aarch64_detect_scalar_stmt_subtype (vec_info *vinfo, vect_cost_for_stmt kind,
				    stmt_vec_info stmt_info,
				    fractional_cost stmt_cost)
{
  /* Detect an extension of a loaded value.  In general, we'll be able to fuse
     the extension with the load.  */
  if (kind == scalar_stmt && vect_is_extending_load (vinfo, stmt_info))
    return 0;

  return stmt_cost;
}

/* STMT_COST is the cost calculated by aarch64_builtin_vectorization_cost
   for the vectorized form of STMT_INFO possibly using SLP node NODE, which has
   cost kind KIND and which when vectorized would operate on vector type
   VECTYPE.  Try to subdivide the target-independent categorization provided by
   KIND to get a more accurate cost.  WHERE specifies where the cost associated
   with KIND occurs.  */
static fractional_cost
aarch64_detect_vector_stmt_subtype (vec_info *vinfo, vect_cost_for_stmt kind,
				    stmt_vec_info stmt_info, slp_tree node,
				    tree vectype,
				    enum vect_cost_model_location where,
				    fractional_cost stmt_cost)
{
  const simd_vec_cost *simd_costs = aarch64_simd_vec_costs (vectype);
  const sve_vec_cost *sve_costs = nullptr;
  if (aarch64_sve_mode_p (TYPE_MODE (vectype)))
    sve_costs = aarch64_tune_params.vec_costs->sve;

  /* It's generally better to avoid costing inductions, since the induction
     will usually be hidden by other operations.  This is particularly true
     for things like COND_REDUCTIONS.  */
  if (is_a<gphi *> (stmt_info->stmt))
    return 0;

  /* Detect cases in which vec_to_scalar is describing the extraction of a
     vector element in preparation for a scalar store.  The store itself is
     costed separately.  */
  if (vect_is_store_elt_extraction (kind, stmt_info))
    return simd_costs->store_elt_extra_cost;

  /* Detect SVE gather loads, which are costed as a single scalar_load
     for each element.  We therefore need to divide the full-instruction
     cost by the number of elements in the vector.  */
  if (kind == scalar_load
      && sve_costs
      && vect_mem_access_type (stmt_info, node) == VMAT_GATHER_SCATTER)
    {
      unsigned int nunits = vect_nunits_for_cost (vectype);
      /* Test for VNx2 modes, which have 64-bit containers.  */
      if (known_eq (GET_MODE_NUNITS (TYPE_MODE (vectype)), aarch64_sve_vg))
	return { sve_costs->gather_load_x64_cost, nunits };
      return { sve_costs->gather_load_x32_cost, nunits };
    }

  /* Detect cases in which a scalar_store is really storing one element
     in a scatter operation.  */
  if (kind == scalar_store
      && sve_costs
      && vect_mem_access_type (stmt_info, node) == VMAT_GATHER_SCATTER)
    return sve_costs->scatter_store_elt_cost;

  /* Detect cases in which vec_to_scalar represents an in-loop reduction.  */
  if (kind == vec_to_scalar
      && where == vect_body
      && sve_costs)
    {
      unsigned int latency
	= aarch64_sve_in_loop_reduction_latency (vinfo, stmt_info, sve_costs);
      if (latency)
	return latency;
    }

  /* Detect cases in which vec_to_scalar represents a single reduction
     instruction like FADDP or MAXV.  */
  if (kind == vec_to_scalar
      && where == vect_epilogue
      && vect_is_reduction (stmt_info))
    switch (GET_MODE_INNER (TYPE_MODE (vectype)))
      {
      case E_QImode:
	return simd_costs->reduc_i8_cost;

      case E_HImode:
	return simd_costs->reduc_i16_cost;

      case E_SImode:
	return simd_costs->reduc_i32_cost;

      case E_DImode:
	return simd_costs->reduc_i64_cost;

      case E_HFmode:
      case E_BFmode:
	return simd_costs->reduc_f16_cost;

      case E_SFmode:
	return simd_costs->reduc_f32_cost;

      case E_DFmode:
	return simd_costs->reduc_f64_cost;

      default:
	break;
      }

  /* Otherwise stick with the original categorization.  */
  return stmt_cost;
}

/* STMT_COST is the cost calculated by aarch64_builtin_vectorization_cost
   for STMT_INFO, which has cost kind KIND and which when vectorized would
   operate on vector type VECTYPE.  Adjust the cost as necessary for SVE
   targets.  */
static fractional_cost
aarch64_sve_adjust_stmt_cost (class vec_info *vinfo, vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info, tree vectype,
			      fractional_cost stmt_cost)
{
  /* Unlike vec_promote_demote, vector_stmt conversions do not change the
     vector register size or number of units.  Integer promotions of this
     type therefore map to SXT[BHW] or UXT[BHW].

     Most loads have extending forms that can do the sign or zero extension
     on the fly.  Optimistically assume that a load followed by an extension
     will fold to this form during combine, and that the extension therefore
     comes for free.  */
  if (kind == vector_stmt && vect_is_extending_load (vinfo, stmt_info))
    stmt_cost = 0;

  /* For similar reasons, vector_stmt integer truncations are a no-op,
     because we can just ignore the unused upper bits of the source.  */
  if (kind == vector_stmt && vect_is_integer_truncation (stmt_info))
    stmt_cost = 0;

  /* Advanced SIMD can load and store pairs of registers using LDP and STP,
     but there are no equivalent instructions for SVE.  This means that
     (all other things being equal) 128-bit SVE needs twice as many load
     and store instructions as Advanced SIMD in order to process vector pairs.

     Also, scalar code can often use LDP and STP to access pairs of values,
     so it is too simplistic to say that one SVE load or store replaces
     VF scalar loads and stores.

     Ideally we would account for this in the scalar and Advanced SIMD
     costs by making suitable load/store pairs as cheap as a single
     load/store.  However, that would be a very invasive change and in
     practice it tends to stress other parts of the cost model too much.
     E.g. stores of scalar constants currently count just a store,
     whereas stores of vector constants count a store and a vec_init.
     This is an artificial distinction for AArch64, where stores of
     nonzero scalar constants need the same kind of register invariant
     as vector stores.

     An alternative would be to double the cost of any SVE loads and stores
     that could be paired in Advanced SIMD (and possibly also paired in
     scalar code).  But this tends to stress other parts of the cost model
     in the same way.  It also means that we can fall back to Advanced SIMD
     even if full-loop predication would have been useful.

     Here we go for a more conservative version: double the costs of SVE
     loads and stores if one iteration of the scalar loop processes enough
     elements for it to use a whole number of Advanced SIMD LDP or STP
     instructions.  This makes it very likely that the VF would be 1 for
     Advanced SIMD, and so no epilogue should be needed.  */
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      stmt_vec_info first = DR_GROUP_FIRST_ELEMENT (stmt_info);
      unsigned int count = DR_GROUP_SIZE (first) - DR_GROUP_GAP (first);
      unsigned int elt_bits = GET_MODE_UNIT_BITSIZE (TYPE_MODE (vectype));
      if (multiple_p (count * elt_bits, 256)
	  && aarch64_advsimd_ldp_stp_p (kind, stmt_info))
	stmt_cost *= 2;
    }

  return stmt_cost;
}

/* STMT_COST is the cost calculated for STMT_INFO, which has cost kind KIND
   and which when vectorized would operate on vector type VECTYPE.  Add the
   cost of any embedded operations.  */
static fractional_cost
aarch64_adjust_stmt_cost (vec_info *vinfo, vect_cost_for_stmt kind,
			  stmt_vec_info stmt_info, slp_tree node, tree vectype,
			  unsigned vec_flags, fractional_cost stmt_cost)
{
  if (vectype)
    {
      const simd_vec_cost *simd_costs = aarch64_simd_vec_costs (vectype);

      /* Detect cases in which a vector load or store represents an
	 LD[234] or ST[234] instruction.  */
      switch (aarch64_ld234_st234_vectors (kind, stmt_info, node))
	{
	case 2:
	  stmt_cost += simd_costs->ld2_st2_permute_cost;
	  break;

	case 3:
	  stmt_cost += simd_costs->ld3_st3_permute_cost;
	  break;

	case 4:
	  stmt_cost += simd_costs->ld4_st4_permute_cost;
	  break;
	}

      gassign *assign = dyn_cast<gassign *> (STMT_VINFO_STMT (stmt_info));
      if ((kind == scalar_stmt || kind == vector_stmt) && assign)
	{
	  /* For MLA we need to reduce the cost since MLA is 1 instruction.  */
	  if (!vect_is_reduction (stmt_info)
	      && aarch64_multiply_add_p (vinfo, stmt_info, vec_flags))
	    return 0;

	  /* For vector boolean ANDs with a compare operand we just need
	     one insn.  */
	  if (aarch64_bool_compound_p (vinfo, stmt_info, vec_flags))
	    return 0;
	}

      if (kind == vector_stmt || kind == vec_to_scalar)
	if (tree cmp_type = vect_embedded_comparison_type (stmt_info))
	  {
	    if (FLOAT_TYPE_P (cmp_type))
	      stmt_cost += simd_costs->fp_stmt_cost;
	    else
	      stmt_cost += simd_costs->int_stmt_cost;
	  }
    }

  if (kind == scalar_stmt)
    if (tree cmp_type = vect_embedded_comparison_type (stmt_info))
      {
	if (FLOAT_TYPE_P (cmp_type))
	  stmt_cost += aarch64_tune_params.vec_costs->scalar_fp_stmt_cost;
	else
	  stmt_cost += aarch64_tune_params.vec_costs->scalar_int_stmt_cost;
      }

  return stmt_cost;
}

/* Return true if STMT_INFO is part of a reduction that has the form:

      r = r op ...;
      r = r op ...;

   with the single accumulator being read and written multiple times.  */
static bool
aarch64_force_single_cycle (vec_info *vinfo, stmt_vec_info stmt_info)
{
  if (!STMT_VINFO_REDUC_DEF (stmt_info))
    return false;

  auto reduc_info = info_for_reduction (vinfo, stmt_info);
  return STMT_VINFO_FORCE_SINGLE_CYCLE (reduc_info);
}

/* COUNT, KIND and STMT_INFO are the same as for vector_costs::add_stmt_cost
   and they describe an operation in the body of a vector loop.  Record issue
   information relating to the vector operation in OPS.  */
void
aarch64_vector_costs::count_ops (unsigned int count, vect_cost_for_stmt kind,
				 stmt_vec_info stmt_info, slp_tree node,
				 aarch64_vec_op_count *ops)
{
  const aarch64_base_vec_issue_info *base_issue = ops->base_issue_info ();
  if (!base_issue)
    return;
  const aarch64_simd_vec_issue_info *simd_issue = ops->simd_issue_info ();
  const aarch64_sve_vec_issue_info *sve_issue = ops->sve_issue_info ();

  /* Calculate the minimum cycles per iteration imposed by a reduction
     operation.  */
  if ((kind == scalar_stmt || kind == vector_stmt || kind == vec_to_scalar)
      && vect_is_reduction (stmt_info))
    {
      unsigned int base
	= aarch64_in_loop_reduction_latency (m_vinfo, stmt_info, m_vec_flags);
      if (aarch64_force_single_cycle (m_vinfo, stmt_info))
	/* ??? Ideally we'd use a tree to reduce the copies down to 1 vector,
	   and then accumulate that, but at the moment the loop-carried
	   dependency includes all copies.  */
	ops->reduction_latency = MAX (ops->reduction_latency, base * count);
      else
	ops->reduction_latency = MAX (ops->reduction_latency, base);
    }

  if (stmt_info && (kind == scalar_stmt || kind == vector_stmt))
    {
      /* Assume that multiply-adds will become a single operation.  */
      if (aarch64_multiply_add_p (m_vinfo, stmt_info, m_vec_flags))
	return;

      /* Assume that bool AND with compare operands will become a single
	 operation.  */
      if (aarch64_bool_compound_p (m_vinfo, stmt_info, m_vec_flags))
	return;
    }


  /* Count the basic operation cost associated with KIND.  */
  switch (kind)
    {
    case cond_branch_taken:
    case cond_branch_not_taken:
    case vector_gather_load:
    case vector_scatter_store:
      /* We currently don't expect these to be used in a loop body.  */
      break;

    case vec_perm:
    case vec_promote_demote:
    case vec_construct:
    case vec_to_scalar:
    case scalar_to_vec:
    case vector_stmt:
    case scalar_stmt:
      ops->general_ops += count;
      break;

    case scalar_load:
    case vector_load:
    case unaligned_load:
      ops->loads += count;
      if (m_vec_flags || FLOAT_TYPE_P (aarch64_dr_type (stmt_info)))
	ops->general_ops += base_issue->fp_simd_load_general_ops * count;
      break;

    case vector_store:
    case unaligned_store:
    case scalar_store:
      ops->stores += count;
      if (m_vec_flags || FLOAT_TYPE_P (aarch64_dr_type (stmt_info)))
	ops->general_ops += base_issue->fp_simd_store_general_ops * count;
      break;
    }

  /* Add any embedded comparison operations.  */
  if ((kind == scalar_stmt || kind == vector_stmt || kind == vec_to_scalar)
      && vect_embedded_comparison_type (stmt_info))
    ops->general_ops += count;

  /* COND_REDUCTIONS need two sets of VEC_COND_EXPRs, whereas so far we
     have only accounted for one.  */
  if ((kind == vector_stmt || kind == vec_to_scalar)
      && vect_reduc_type (m_vinfo, stmt_info) == COND_REDUCTION)
    ops->general_ops += count;

  /* Count the predicate operations needed by an SVE comparison.  */
  if (sve_issue && (kind == vector_stmt || kind == vec_to_scalar))
    if (tree type = vect_comparison_type (stmt_info))
      {
	unsigned int base = (FLOAT_TYPE_P (type)
			     ? sve_issue->fp_cmp_pred_ops
			     : sve_issue->int_cmp_pred_ops);
	ops->pred_ops += base * count;
      }

  /* Add any extra overhead associated with LD[234] and ST[234] operations.  */
  if (simd_issue)
    switch (aarch64_ld234_st234_vectors (kind, stmt_info, node))
      {
      case 2:
	ops->general_ops += simd_issue->ld2_st2_general_ops * count;
	break;

      case 3:
	ops->general_ops += simd_issue->ld3_st3_general_ops * count;
	break;

      case 4:
	ops->general_ops += simd_issue->ld4_st4_general_ops * count;
	break;
      }

  /* Add any overhead associated with gather loads and scatter stores.  */
  if (sve_issue
      && (kind == scalar_load || kind == scalar_store)
      && vect_mem_access_type (stmt_info, node) == VMAT_GATHER_SCATTER)
    {
      unsigned int pairs = CEIL (count, 2);
      ops->pred_ops += sve_issue->gather_scatter_pair_pred_ops * pairs;
      ops->general_ops += sve_issue->gather_scatter_pair_general_ops * pairs;
    }
}

/* Return true if STMT_INFO contains a memory access and if the constant
   component of the memory address is aligned to SIZE bytes.  */
static bool
aarch64_aligned_constant_offset_p (stmt_vec_info stmt_info,
				   poly_uint64 size)
{
  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  if (auto first_stmt = DR_GROUP_FIRST_ELEMENT (stmt_info))
    stmt_info = first_stmt;
  tree constant_offset = DR_INIT (STMT_VINFO_DATA_REF (stmt_info));
  /* Needed for gathers & scatters, for example.  */
  if (!constant_offset)
    return false;

  return multiple_p (wi::to_poly_offset (constant_offset), size);
}

/* Check if a scalar or vector stmt could be part of a region of code
   that does nothing more than store values to memory, in the scalar
   case using STP.  Return the cost of the stmt if so, counting 2 for
   one instruction.  Return ~0U otherwise.

   The arguments are a subset of those passed to add_stmt_cost.  */
unsigned int
aarch64_stp_sequence_cost (unsigned int count, vect_cost_for_stmt kind,
			   stmt_vec_info stmt_info, tree vectype)
{
  /* Code that stores vector constants uses a vector_load to create
     the constant.  We don't apply the heuristic to that case for two
     main reasons:

     - At the moment, STPs are only formed via peephole2, and the
       constant scalar moves would often come between STRs and so
       prevent STP formation.

     - The scalar code also has to load the constant somehow, and that
       isn't costed.  */
  switch (kind)
    {
    case scalar_to_vec:
      /* Count 2 insns for a GPR->SIMD dup and 1 insn for a FPR->SIMD dup.  */
      return (FLOAT_TYPE_P (vectype) ? 2 : 4) * count;

    case vec_construct:
      if (FLOAT_TYPE_P (vectype))
	/* Count 1 insn for the maximum number of FP->SIMD INS
	   instructions.  */
	return (vect_nunits_for_cost (vectype) - 1) * 2 * count;

      /* Count 2 insns for a GPR->SIMD move and 2 insns for the
	 maximum number of GPR->SIMD INS instructions.  */
      return vect_nunits_for_cost (vectype) * 4 * count;

    case vector_store:
    case unaligned_store:
      /* Count 1 insn per vector if we can't form STP Q pairs.  */
      if (aarch64_sve_mode_p (TYPE_MODE (vectype)))
	return count * 2;

      if (stmt_info)
	{
	  /* Assume we won't be able to use STP if the constant offset
	     component of the address is misaligned.  ??? This could be
	     removed if we formed STP pairs earlier, rather than relying
	     on peephole2.  */
	  auto size = GET_MODE_SIZE (TYPE_MODE (vectype));
	  if (!aarch64_aligned_constant_offset_p (stmt_info, size))
	    return count * 2;
	}
      return CEIL (count, 2) * 2;

    case scalar_store:
      if (stmt_info && STMT_VINFO_DATA_REF (stmt_info))
	{
	  /* Check for a mode in which STP pairs can be formed.  */
	  auto size = GET_MODE_SIZE (TYPE_MODE (aarch64_dr_type (stmt_info)));
	  if (maybe_ne (size, 4) && maybe_ne (size, 8))
	    return ~0U;

	  /* Assume we won't be able to use STP if the constant offset
	     component of the address is misaligned.  ??? This could be
	     removed if we formed STP pairs earlier, rather than relying
	     on peephole2.  */
	  if (!aarch64_aligned_constant_offset_p (stmt_info, size))
	    return ~0U;
	}
      return count;

    default:
      return ~0U;
    }
}

unsigned
aarch64_vector_costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
				     stmt_vec_info stmt_info, slp_tree node,
				     tree vectype, int misalign,
				     vect_cost_model_location where)
{
  fractional_cost stmt_cost
    = aarch64_builtin_vectorization_cost (kind, vectype, misalign);

  bool in_inner_loop_p = (where == vect_body
			  && stmt_info
			  && stmt_in_inner_loop_p (m_vinfo, stmt_info));

  /* Do one-time initialization based on the vinfo.  */
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo);
  if (!m_analyzed_vinfo && aarch64_use_new_vector_costs_p ())
    {
      if (loop_vinfo)
	analyze_loop_vinfo (loop_vinfo);

      m_analyzed_vinfo = true;
    }

  /* Apply the heuristic described above m_stp_sequence_cost.  */
  if (m_stp_sequence_cost != ~0U)
    {
      uint64_t cost = aarch64_stp_sequence_cost (count, kind,
						 stmt_info, vectype);
      m_stp_sequence_cost = MIN (m_stp_sequence_cost + cost, ~0U);
    }

  /* Try to get a more accurate cost by looking at STMT_INFO instead
     of just looking at KIND.  */
  if (stmt_info && aarch64_use_new_vector_costs_p ())
    {
      /* If we scalarize a strided store, the vectorizer costs one
	 vec_to_scalar for each element.  However, we can store the first
	 element using an FP store without a separate extract step.  */
      if (vect_is_store_elt_extraction (kind, stmt_info))
	count -= 1;

      stmt_cost = aarch64_detect_scalar_stmt_subtype (m_vinfo, kind,
						      stmt_info, stmt_cost);

      if (vectype && m_vec_flags)
	stmt_cost = aarch64_detect_vector_stmt_subtype (m_vinfo, kind,
							stmt_info, node,
							vectype, where,
							stmt_cost);

      /* Check if we've seen an SVE gather/scatter operation and which size.  */
      if (kind == scalar_load
	  && aarch64_sve_mode_p (TYPE_MODE (vectype))
	  && vect_mem_access_type (stmt_info, node) == VMAT_GATHER_SCATTER)
	{
	  const sve_vec_cost *sve_costs = aarch64_tune_params.vec_costs->sve;
	  if (sve_costs)
	    {
	      /* Test for VNx2 modes, which have 64-bit containers.  */
	      if (known_eq (GET_MODE_NUNITS (TYPE_MODE (vectype)),
			    aarch64_sve_vg))
		m_sve_gather_scatter_init_cost
		  += sve_costs->gather_load_x64_init_cost;
	      else
		m_sve_gather_scatter_init_cost
		  += sve_costs->gather_load_x32_init_cost;
	    }
	}
    }

  /* Do any SVE-specific adjustments to the cost.  */
  if (stmt_info && vectype && aarch64_sve_mode_p (TYPE_MODE (vectype)))
    stmt_cost = aarch64_sve_adjust_stmt_cost (m_vinfo, kind, stmt_info,
					      vectype, stmt_cost);

  /*  Vector promotion and demotion requires us to widen the operation first
      and only after that perform the conversion.  Unfortunately the mid-end
      expects this to be doable as a single operation and doesn't pass on
      enough context here for us to tell which operation is happening.  To
      account for this we count every promote-demote operation twice and if
      the previously costed operation was also a promote-demote we reduce
      the cost of the currently being costed operation to simulate the final
      conversion cost.  Note that for SVE we can do better here if the converted
      value comes from a load since the widening load would consume the widening
      operations.  However since we're in stage 3 we can't change the helper
      vect_is_extending_load and duplicating the code seems not useful.  */
  gassign *assign = NULL;
  if (kind == vec_promote_demote
      && (assign = dyn_cast <gassign *> (STMT_VINFO_STMT (stmt_info)))
      && gimple_assign_rhs_code (assign) == FLOAT_EXPR)
    {
      auto new_count = count * 2 - m_num_last_promote_demote;
      m_num_last_promote_demote = count;
      count = new_count;
    }
  else
    m_num_last_promote_demote = 0;

  if (stmt_info && aarch64_use_new_vector_costs_p ())
    {
      /* Account for any extra "embedded" costs that apply additively
	 to the base cost calculated above.  */
      stmt_cost = aarch64_adjust_stmt_cost (m_vinfo, kind, stmt_info, node,
					    vectype, m_vec_flags, stmt_cost);

      /* If we're recording a nonzero vector loop body cost for the
	 innermost loop, also estimate the operations that would need
	 to be issued by all relevant implementations of the loop.  */
      if (loop_vinfo
	  && (m_costing_for_scalar || where == vect_body)
	  && (!LOOP_VINFO_LOOP (loop_vinfo)->inner || in_inner_loop_p)
	  && stmt_cost != 0)
	for (auto &ops : m_ops)
	  count_ops (count, kind, stmt_info, node, &ops);

      /* If we're applying the SVE vs. Advanced SIMD unrolling heuristic,
	 estimate the number of statements in the unrolled Advanced SIMD
	 loop.  For simplicitly, we assume that one iteration of the
	 Advanced SIMD loop would need the same number of statements
	 as one iteration of the SVE loop.  */
      if (where == vect_body && m_unrolled_advsimd_niters)
	m_unrolled_advsimd_stmts += count * m_unrolled_advsimd_niters;

      /* Detect the use of an averaging operation.  */
      gimple *stmt = stmt_info->stmt;
      if (is_gimple_call (stmt)
	  && gimple_call_internal_p (stmt))
	{
	  switch (gimple_call_internal_fn (stmt))
	    {
	    case IFN_AVG_FLOOR:
	    case IFN_AVG_CEIL:
	      m_has_avg = true;
	    default:
	      break;
	    }
	}
    }

  /* If the statement stores to a decl that is known to be the argument
     to a vld1 in the same function, ignore the store for costing purposes.
     See the comment above m_stores_to_vector_load_decl for more details.  */
  if (stmt_info
      && (kind == vector_store || kind == unaligned_store)
      && aarch64_accesses_vector_load_decl_p (stmt_info))
    {
      stmt_cost = 0;
      m_stores_to_vector_load_decl = true;
    }

  return record_stmt_cost (stmt_info, where, (count * stmt_cost).ceil ());
}

/* Return true if (a) we're applying the Advanced SIMD vs. SVE unrolling
   heuristic described above m_unrolled_advsimd_niters and (b) the heuristic
   says that we should prefer the Advanced SIMD loop.  */
bool
aarch64_vector_costs::prefer_unrolled_loop () const
{
  if (!m_unrolled_advsimd_stmts)
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Number of insns in"
		     " unrolled Advanced SIMD loop = "
		     HOST_WIDE_INT_PRINT_UNSIGNED "\n",
		     m_unrolled_advsimd_stmts);

  /* The balance here is tricky.  On the one hand, we can't be sure whether
     the code is vectorizable with Advanced SIMD or not.  However, even if
     it isn't vectorizable with Advanced SIMD, there's a possibility that
     the scalar code could also be unrolled.  Some of the code might then
     benefit from SLP, or from using LDP and STP.  We therefore apply
     the heuristic regardless of can_use_advsimd_p.  */
  return (m_unrolled_advsimd_stmts
	  && (m_unrolled_advsimd_stmts
	      <= (unsigned int) param_max_completely_peeled_insns));
}

/* Subroutine of adjust_body_cost for handling SVE.  Use ISSUE_INFO to work out
   how fast the SVE code can be issued and compare it to the equivalent value
   for scalar code (SCALAR_CYCLES_PER_ITER).  If COULD_USE_ADVSIMD is true,
   also compare it to the issue rate of Advanced SIMD code
   (ADVSIMD_CYCLES_PER_ITER).

   ORIG_BODY_COST is the cost originally passed to adjust_body_cost and
   *BODY_COST is the current value of the adjusted cost.  *SHOULD_DISPARAGE
   is true if we think the loop body is too expensive.  */

fractional_cost
aarch64_vector_costs::
adjust_body_cost_sve (const aarch64_vec_op_count *ops,
		      fractional_cost scalar_cycles_per_iter,
		      unsigned int orig_body_cost, unsigned int *body_cost,
		      bool *should_disparage)
{
  if (dump_enabled_p ())
    ops->dump ();

  fractional_cost sve_pred_cycles_per_iter = ops->min_pred_cycles_per_iter ();
  fractional_cost sve_cycles_per_iter = ops->min_cycles_per_iter ();

  /* If the scalar version of the loop could issue at least as
     quickly as the predicate parts of the SVE loop, make the SVE loop
     prohibitively expensive.  In this case vectorization is adding an
     overhead that the original scalar code didn't have.

     This is mostly intended to detect cases in which WHILELOs dominate
     for very tight loops, which is something that normal latency-based
     costs would not model.  Adding this kind of cliffedge would be
     too drastic for scalar_cycles_per_iter vs. sve_cycles_per_iter;
     code in the caller handles that case in a more conservative way.  */
  fractional_cost sve_estimate = sve_pred_cycles_per_iter + 1;
  if (scalar_cycles_per_iter < sve_estimate)
    {
      unsigned int min_cost
	= orig_body_cost * estimated_poly_value (BYTES_PER_SVE_VECTOR);
      if (*body_cost < min_cost)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Increasing body cost to %d because the"
			     " scalar code could issue within the limit"
			     " imposed by predicate operations\n",
			     min_cost);
	  *body_cost = min_cost;
	  *should_disparage = true;
	}
    }

  return sve_cycles_per_iter;
}

unsigned int
aarch64_vector_costs::determine_suggested_unroll_factor ()
{
  bool sve = m_vec_flags & VEC_ANY_SVE;
  /* If we are trying to unroll an Advanced SIMD main loop that contains
     an averaging operation that we do not support with SVE and we might use a
     predicated epilogue, we need to be conservative and block unrolling as
     this might lead to a less optimal loop for the first and only epilogue
     using the original loop's vectorization factor.
     TODO: Remove this constraint when we add support for multiple epilogue
     vectorization.  */
  if (!sve && !TARGET_SVE2 && m_has_avg)
    return 1;

  unsigned int max_unroll_factor = 1;
  for (auto vec_ops : m_ops)
    {
      aarch64_simd_vec_issue_info const *vec_issue
	= vec_ops.simd_issue_info ();
      if (!vec_issue)
	return 1;
      /* Limit unroll factor to a value adjustable by the user, the default
	 value is 4. */
      unsigned int unroll_factor = aarch64_vect_unroll_limit;
      unsigned int factor
       = vec_ops.reduction_latency > 1 ? vec_ops.reduction_latency : 1;
      unsigned int temp;

      /* Sanity check, this should never happen.  */
      if ((vec_ops.stores + vec_ops.loads + vec_ops.general_ops) == 0)
	return 1;

      /* Check stores.  */
      if (vec_ops.stores > 0)
	{
	  temp = CEIL (factor * vec_issue->stores_per_cycle,
		       vec_ops.stores);
	  unroll_factor = MIN (unroll_factor, temp);
	}

      /* Check loads + stores.  */
      if (vec_ops.loads > 0)
	{
	  temp = CEIL (factor * vec_issue->loads_stores_per_cycle,
		       vec_ops.loads + vec_ops.stores);
	  unroll_factor = MIN (unroll_factor, temp);
	}

      /* Check general ops.  */
      if (vec_ops.general_ops > 0)
	{
	  temp = CEIL (factor * vec_issue->general_ops_per_cycle,
		       vec_ops.general_ops);
	  unroll_factor = MIN (unroll_factor, temp);
	 }
      max_unroll_factor = MAX (max_unroll_factor, unroll_factor);
    }

  /* Make sure unroll factor is power of 2.  */
  return 1 << ceil_log2 (max_unroll_factor);
}

/* BODY_COST is the cost of a vector loop body.  Adjust the cost as necessary
   and return the new cost.  */
unsigned int
aarch64_vector_costs::
adjust_body_cost (loop_vec_info loop_vinfo,
		  const aarch64_vector_costs *scalar_costs,
		  unsigned int body_cost)
{
  if (scalar_costs->m_ops.is_empty () || m_ops.is_empty ())
    return body_cost;

  const auto &scalar_ops = scalar_costs->m_ops[0];
  const auto &vector_ops = m_ops[0];
  unsigned int estimated_vf = vect_vf_for_cost (loop_vinfo);
  unsigned int orig_body_cost = body_cost;
  bool should_disparage = false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Original vector body cost = %d\n", body_cost);

  /* If we know we have a single partial vector iteration, cap the VF
     to the number of scalar iterations for costing purposes.  */
  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      auto niters = LOOP_VINFO_INT_NITERS (loop_vinfo);
      if (niters < estimated_vf && dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Scalar loop iterates at most %wd times.  Capping VF "
			 " from %d to %wd\n", niters, estimated_vf, niters);

      estimated_vf = MIN (estimated_vf, niters);
    }

  fractional_cost scalar_cycles_per_iter
    = scalar_ops.min_cycles_per_iter () * estimated_vf;

  fractional_cost vector_cycles_per_iter = vector_ops.min_cycles_per_iter ();

  if (dump_enabled_p ())
    {
      if (IN_RANGE (m_num_vector_iterations, 0, 65536))
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Vector loop iterates at most %wd times\n",
			 m_num_vector_iterations);
      dump_printf_loc (MSG_NOTE, vect_location, "Scalar issue estimate:\n");
      scalar_ops.dump ();
      dump_printf_loc (MSG_NOTE, vect_location,
		       "  estimated cycles per vector iteration"
		       " (for VF %d) = %f\n",
		       estimated_vf, scalar_cycles_per_iter.as_double ());
    }

  if (vector_ops.sve_issue_info ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "SVE issue estimate:\n");
      vector_cycles_per_iter
	= adjust_body_cost_sve (&vector_ops, scalar_cycles_per_iter,
				orig_body_cost, &body_cost, &should_disparage);

      if (aarch64_tune_params.vec_costs == &neoverse512tvb_vector_cost)
	{
	  /* Also take Neoverse V1 tuning into account, doubling the
	     scalar and Advanced SIMD estimates to account for the
	     doubling in SVE vector length.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Neoverse V1 estimate:\n");
	  auto vf_factor = m_ops[1].vf_factor ();
	  adjust_body_cost_sve (&m_ops[1], scalar_cycles_per_iter * vf_factor,
				orig_body_cost, &body_cost, &should_disparage);
	}
    }
  else
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Vector issue estimate:\n");
	  vector_ops.dump ();
	}
    }

  /* Decide whether to stick to latency-based costs or whether to try to
     take issue rates into account.  */
  unsigned int threshold = aarch64_loop_vect_issue_rate_niters;
  if (m_vec_flags & VEC_ANY_SVE)
    threshold = CEIL (threshold, aarch64_estimated_sve_vq ());

  if (m_num_vector_iterations >= 1
      && m_num_vector_iterations < threshold)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Low iteration count, so using pure latency"
			 " costs\n");
    }
  /* Increase the cost of the vector code if it looks like the scalar code
     could issue more quickly.  These values are only rough estimates,
     so minor differences should only result in minor changes.  */
  else if (scalar_cycles_per_iter < vector_cycles_per_iter)
    {
      body_cost = fractional_cost::scale (body_cost, vector_cycles_per_iter,
					  scalar_cycles_per_iter);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Increasing body cost to %d because scalar code"
			 " would issue more quickly\n", body_cost);
    }
  /* In general, it's expected that the proposed vector code would be able
     to issue more quickly than the original scalar code.  This should
     already be reflected to some extent in the latency-based costs.

     However, the latency-based costs effectively assume that the scalar
     code and the vector code execute serially, which tends to underplay
     one important case: if the real (non-serialized) execution time of
     a scalar iteration is dominated by loop-carried dependencies,
     and if the vector code is able to reduce both the length of
     the loop-carried dependencies *and* the number of cycles needed
     to issue the code in general, we can be more confident that the
     vector code is an improvement, even if adding the other (non-loop-carried)
     latencies tends to hide this saving.  We therefore reduce the cost of the
     vector loop body in proportion to the saving.  */
  else if (scalar_ops.reduction_latency > vector_ops.reduction_latency
	   && scalar_ops.reduction_latency == scalar_cycles_per_iter
	   && scalar_cycles_per_iter > vector_cycles_per_iter
	   && !should_disparage)
    {
      body_cost = fractional_cost::scale (body_cost, vector_cycles_per_iter,
					  scalar_cycles_per_iter);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Decreasing body cost to %d account for smaller"
			 " reduction latency\n", body_cost);
    }

  return body_cost;
}

void
aarch64_vector_costs::finish_cost (const vector_costs *uncast_scalar_costs)
{
  /* Record the issue information for any SVE WHILE instructions that the
     loop needs.  */
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo);
  if (!m_ops.is_empty ()
      && loop_vinfo
      && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      unsigned int num_masks = 0;
      rgroup_controls *rgm;
      unsigned int num_vectors_m1;
      FOR_EACH_VEC_ELT (LOOP_VINFO_MASKS (loop_vinfo).rgc_vec,
			num_vectors_m1, rgm)
	if (rgm->type)
	  num_masks += num_vectors_m1 + 1;
      for (auto &ops : m_ops)
	if (auto *issue = ops.sve_issue_info ())
	  ops.pred_ops += num_masks * issue->while_pred_ops;
    }

  auto *scalar_costs
    = static_cast<const aarch64_vector_costs *> (uncast_scalar_costs);
  if (loop_vinfo
      && m_vec_flags
      && aarch64_use_new_vector_costs_p ())
    {
      m_costs[vect_body] = adjust_body_cost (loop_vinfo, scalar_costs,
					     m_costs[vect_body]);
      m_suggested_unroll_factor = determine_suggested_unroll_factor ();

      /* For gather and scatters there's an additional overhead for the first
	 iteration.  For low count loops they're not beneficial so model the
	 overhead as loop prologue costs.  */
      m_costs[vect_prologue] += m_sve_gather_scatter_init_cost;
    }

  /* Apply the heuristic described above m_stp_sequence_cost.  Prefer
     the scalar code in the event of a tie, since there is more chance
     of scalar code being optimized with surrounding operations.

     In addition, if the vector body is a simple store to a decl that
     is elsewhere loaded using vld1, strongly prefer the vector form,
     to the extent of giving the prologue a zero cost.  See the comment
     above m_stores_to_vector_load_decl for details.  */
  if (!loop_vinfo
      && scalar_costs
      && m_stp_sequence_cost != ~0U)
    {
      if (m_stores_to_vector_load_decl)
	m_costs[vect_prologue] = 0;
      else if (m_stp_sequence_cost >= scalar_costs->m_stp_sequence_cost)
	m_costs[vect_body] = 2 * scalar_costs->total_cost ();
    }

  vector_costs::finish_cost (scalar_costs);
}

bool
aarch64_vector_costs::
better_main_loop_than_p (const vector_costs *uncast_other) const
{
  auto other = static_cast<const aarch64_vector_costs *> (uncast_other);

  auto this_loop_vinfo = as_a<loop_vec_info> (this->m_vinfo);
  auto other_loop_vinfo = as_a<loop_vec_info> (other->m_vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Comparing two main loops (%s at VF %d vs %s at VF %d)\n",
		     GET_MODE_NAME (this_loop_vinfo->vector_mode),
		     vect_vf_for_cost (this_loop_vinfo),
		     GET_MODE_NAME (other_loop_vinfo->vector_mode),
		     vect_vf_for_cost (other_loop_vinfo));

  /* Apply the unrolling heuristic described above
     m_unrolled_advsimd_niters.  */
  if (bool (m_unrolled_advsimd_stmts)
      != bool (other->m_unrolled_advsimd_stmts))
    {
      bool this_prefer_unrolled = this->prefer_unrolled_loop ();
      bool other_prefer_unrolled = other->prefer_unrolled_loop ();
      if (this_prefer_unrolled != other_prefer_unrolled)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Preferring Advanced SIMD loop because"
			     " it can be unrolled\n");
	  return other_prefer_unrolled;
	}
    }

  for (unsigned int i = 0; i < m_ops.length (); ++i)
    {
      if (dump_enabled_p ())
	{
	  if (i)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Reconsidering with subtuning %d\n", i);
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Issue info for %s loop:\n",
			   GET_MODE_NAME (this_loop_vinfo->vector_mode));
	  this->m_ops[i].dump ();
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Issue info for %s loop:\n",
			   GET_MODE_NAME (other_loop_vinfo->vector_mode));
	  other->m_ops[i].dump ();
	}

      auto this_estimated_vf = (vect_vf_for_cost (this_loop_vinfo)
				* this->m_ops[i].vf_factor ());
      auto other_estimated_vf = (vect_vf_for_cost (other_loop_vinfo)
				 * other->m_ops[i].vf_factor ());

      /* If it appears that one loop could process the same amount of data
	 in fewer cycles, prefer that loop over the other one.  */
      fractional_cost this_cost
	= this->m_ops[i].min_cycles_per_iter () * other_estimated_vf;
      fractional_cost other_cost
	= other->m_ops[i].min_cycles_per_iter () * this_estimated_vf;
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Weighted cycles per iteration of %s loop ~= %f\n",
			   GET_MODE_NAME (this_loop_vinfo->vector_mode),
			   this_cost.as_double ());
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Weighted cycles per iteration of %s loop ~= %f\n",
			   GET_MODE_NAME (other_loop_vinfo->vector_mode),
			   other_cost.as_double ());
	}
      if (this_cost != other_cost)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Preferring loop with lower cycles"
			     " per iteration\n");
	  return this_cost < other_cost;
	}

      /* If the issue rate of SVE code is limited by predicate operations
	 (i.e. if sve_pred_cycles_per_iter > sve_nonpred_cycles_per_iter),
	 and if Advanced SIMD code could issue within the limit imposed
	 by the predicate operations, the predicate operations are adding an
	 overhead that the original code didn't have and so we should prefer
	 the Advanced SIMD version.  */
      auto better_pred_limit_p = [](const aarch64_vec_op_count &a,
				    const aarch64_vec_op_count &b) -> bool
	{
	  if (a.pred_ops == 0
	      && (b.min_pred_cycles_per_iter ()
		  > b.min_nonpred_cycles_per_iter ()))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Preferring Advanced SIMD loop since"
				 " SVE loop is predicate-limited\n");
	      return true;
	    }
	  return false;
	};
      if (better_pred_limit_p (this->m_ops[i], other->m_ops[i]))
	return true;
      if (better_pred_limit_p (other->m_ops[i], this->m_ops[i]))
	return false;
    }

  return vector_costs::better_main_loop_than_p (other);
}

static void initialize_aarch64_code_model (struct gcc_options *);

/* Parse the TO_PARSE string and put the architecture struct that it
   selects into RES and the architectural features into ISA_FLAGS.
   Return an aarch_parse_opt_result describing the parse result.
   If there is an error parsing, RES and ISA_FLAGS are left unchanged.
   When the TO_PARSE string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

static enum aarch_parse_opt_result
aarch64_parse_arch (const char *to_parse, const struct processor **res,
		    aarch64_feature_flags *isa_flags,
		    std::string *invalid_extension)
{
  const char *ext;
  const struct processor *arch;
  size_t len;

  ext = strchr (to_parse, '+');

  if (ext != NULL)
    len = ext - to_parse;
  else
    len = strlen (to_parse);

  if (len == 0)
    return AARCH_PARSE_MISSING_ARG;


  /* Loop through the list of supported ARCHes to find a match.  */
  for (arch = all_architectures; arch->name != NULL; arch++)
    {
      if (strlen (arch->name) == len
	  && strncmp (arch->name, to_parse, len) == 0)
	{
	  auto isa_temp = arch->flags;

	  if (ext != NULL)
	    {
	      /* TO_PARSE string contains at least one extension.  */
	      enum aarch_parse_opt_result ext_res
		= aarch64_parse_extension (ext, &isa_temp, invalid_extension);

	      if (ext_res != AARCH_PARSE_OK)
		return ext_res;
	    }
	  /* Extension parsing was successful.  Confirm the result
	     arch and ISA flags.  */
	  *res = arch;
	  *isa_flags = isa_temp;
	  return AARCH_PARSE_OK;
	}
    }

  /* ARCH name not found in list.  */
  return AARCH_PARSE_INVALID_ARG;
}

/* Parse the TO_PARSE string and put the result tuning in RES and the
   architecture flags in ISA_FLAGS.  Return an aarch_parse_opt_result
   describing the parse result.  If there is an error parsing, RES and
   ISA_FLAGS are left unchanged.
   When the TO_PARSE string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

static enum aarch_parse_opt_result
aarch64_parse_cpu (const char *to_parse, const struct processor **res,
		   aarch64_feature_flags *isa_flags,
		   std::string *invalid_extension)
{
  const char *ext;
  const struct processor *cpu;
  size_t len;

  ext = strchr (to_parse, '+');

  if (ext != NULL)
    len = ext - to_parse;
  else
    len = strlen (to_parse);

  if (len == 0)
    return AARCH_PARSE_MISSING_ARG;


  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strlen (cpu->name) == len && strncmp (cpu->name, to_parse, len) == 0)
	{
	  auto isa_temp = cpu->flags;

	  if (ext != NULL)
	    {
	      /* TO_PARSE string contains at least one extension.  */
	      enum aarch_parse_opt_result ext_res
		= aarch64_parse_extension (ext, &isa_temp, invalid_extension);

	      if (ext_res != AARCH_PARSE_OK)
		return ext_res;
	    }
	  /* Extension parsing was successfull.  Confirm the result
	     cpu and ISA flags.  */
	  *res = cpu;
	  *isa_flags = isa_temp;
	  return AARCH_PARSE_OK;
	}
    }

  /* CPU name not found in list.  */
  return AARCH_PARSE_INVALID_ARG;
}

/* Parse the TO_PARSE string and put the cpu it selects into RES.
   Return an aarch_parse_opt_result describing the parse result.
   If the parsing fails the RES does not change.  */

static enum aarch_parse_opt_result
aarch64_parse_tune (const char *to_parse, const struct processor **res)
{
  const struct processor *cpu;

  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strcmp (cpu->name, to_parse) == 0)
	{
	  *res = cpu;
	  return AARCH_PARSE_OK;
	}
    }

  /* CPU name not found in list.  */
  return AARCH_PARSE_INVALID_ARG;
}

/* Parse TOKEN, which has length LENGTH to see if it is an option
   described in FLAG.  If it is, return the index bit for that fusion type.
   If not, error (printing OPTION_NAME) and return zero.  */

static unsigned int
aarch64_parse_one_option_token (const char *token,
				size_t length,
				const struct aarch64_flag_desc *flag,
				const char *option_name)
{
  for (; flag->name != NULL; flag++)
    {
      if (length == strlen (flag->name)
	  && !strncmp (flag->name, token, length))
	return flag->flag;
    }

  error ("unknown flag passed in %<-moverride=%s%> (%s)", option_name, token);
  return 0;
}

/* Parse OPTION which is a comma-separated list of flags to enable.
   FLAGS gives the list of flags we understand, INITIAL_STATE gives any
   default state we inherit from the CPU tuning structures.  OPTION_NAME
   gives the top-level option we are parsing in the -moverride string,
   for use in error messages.  */

static unsigned int
aarch64_parse_boolean_options (const char *option,
			       const struct aarch64_flag_desc *flags,
			       unsigned int initial_state,
			       const char *option_name)
{
  const char separator = '.';
  const char* specs = option;
  const char* ntoken = option;
  unsigned int found_flags = initial_state;

  while ((ntoken = strchr (specs, separator)))
    {
      size_t token_length = ntoken - specs;
      unsigned token_ops = aarch64_parse_one_option_token (specs,
							   token_length,
							   flags,
							   option_name);
      /* If we find "none" (or, for simplicity's sake, an error) anywhere
	 in the token stream, reset the supported operations.  So:

	   adrp+add.cmp+branch.none.adrp+add

	   would have the result of turning on only adrp+add fusion.  */
      if (!token_ops)
	found_flags = 0;

      found_flags |= token_ops;
      specs = ++ntoken;
    }

  /* We ended with a comma, print something.  */
  if (!(*specs))
    {
      error ("%qs string ill-formed", option_name);
      return 0;
    }

  /* We still have one more token to parse.  */
  size_t token_length = strlen (specs);
  unsigned token_ops = aarch64_parse_one_option_token (specs,
						       token_length,
						       flags,
						       option_name);
   if (!token_ops)
     found_flags = 0;

  found_flags |= token_ops;
  return found_flags;
}

/* Support for overriding instruction fusion.  */

static void
aarch64_parse_fuse_string (const char *fuse_string,
			    struct tune_params *tune)
{
  tune->fusible_ops = aarch64_parse_boolean_options (fuse_string,
						     aarch64_fusible_pairs,
						     tune->fusible_ops,
						     "fuse=");
}

/* Support for overriding other tuning flags.  */

static void
aarch64_parse_tune_string (const char *tune_string,
			    struct tune_params *tune)
{
  tune->extra_tuning_flags
    = aarch64_parse_boolean_options (tune_string,
				     aarch64_tuning_flags,
				     tune->extra_tuning_flags,
				     "tune=");
}

/* Parse the sve_width tuning moverride string in TUNE_STRING.
   Accept the valid SVE vector widths allowed by
   aarch64_sve_vector_bits_enum and use it to override sve_width
   in TUNE.  */

static void
aarch64_parse_sve_width_string (const char *tune_string,
				struct tune_params *tune)
{
  int width = -1;

  int n = sscanf (tune_string, "%d", &width);
  if (n == EOF)
    {
      error ("invalid format for %<sve_width%>");
      return;
    }
  switch (width)
    {
    case SVE_128:
    case SVE_256:
    case SVE_512:
    case SVE_1024:
    case SVE_2048:
      break;
    default:
      error ("invalid %<sve_width%> value: %d", width);
    }
  tune->sve_width = (enum aarch64_sve_vector_bits_enum) width;
}

/* Parse TOKEN, which has length LENGTH to see if it is a tuning option
   we understand.  If it is, extract the option string and handoff to
   the appropriate function.  */

void
aarch64_parse_one_override_token (const char* token,
				  size_t length,
				  struct tune_params *tune)
{
  const struct aarch64_tuning_override_function *fn
    = aarch64_tuning_override_functions;

  const char *option_part = strchr (token, '=');
  if (!option_part)
    {
      error ("tuning string missing in option (%s)", token);
      return;
    }

  /* Get the length of the option name.  */
  length = option_part - token;
  /* Skip the '=' to get to the option string.  */
  option_part++;

  for (; fn->name != NULL; fn++)
    {
      if (!strncmp (fn->name, token, length))
	{
	  fn->parse_override (option_part, tune);
	  return;
	}
    }

  error ("unknown tuning option (%s)",token);
  return;
}

/* A checking mechanism for the implementation of the tls size.  */

static void
initialize_aarch64_tls_size (struct gcc_options *opts)
{
  if (aarch64_tls_size == 0)
    aarch64_tls_size = 24;

  switch (opts->x_aarch64_cmodel_var)
    {
    case AARCH64_CMODEL_TINY:
      /* Both the default and maximum TLS size allowed under tiny is 1M which
	 needs two instructions to address, so we clamp the size to 24.  */
      if (aarch64_tls_size > 24)
	aarch64_tls_size = 24;
      break;
    case AARCH64_CMODEL_SMALL:
      /* The maximum TLS size allowed under small is 4G.  */
      if (aarch64_tls_size > 32)
	aarch64_tls_size = 32;
      break;
    case AARCH64_CMODEL_LARGE:
      /* The maximum TLS size allowed under large is 16E.
	 FIXME: 16E should be 64bit, we only support 48bit offset now.  */
      if (aarch64_tls_size > 48)
	aarch64_tls_size = 48;
      break;
    default:
      gcc_unreachable ();
    }

  return;
}

/* Return the CPU corresponding to the enum CPU.  */

static const struct processor *
aarch64_get_tune_cpu (enum aarch64_processor cpu)
{
  gcc_assert (cpu != aarch64_none);

  return &all_cores[cpu];
}

/* Return the architecture corresponding to the enum ARCH.  */

static const struct processor *
aarch64_get_arch (enum aarch64_arch arch)
{
  gcc_assert (arch != aarch64_no_arch);

  return &all_architectures[arch];
}

/* Parse STRING looking for options in the format:
     string	:: option:string
     option	:: name=substring
     name	:: {a-z}
     substring	:: defined by option.  */

static void
aarch64_parse_override_string (const char* input_string,
			       struct tune_params* tune)
{
  const char separator = ':';
  size_t string_length = strlen (input_string) + 1;
  char *string_root = (char *) xmalloc (sizeof (*string_root) * string_length);
  char *string = string_root;
  strncpy (string, input_string, string_length);
  string[string_length - 1] = '\0';

  char* ntoken = string;

  while ((ntoken = strchr (string, separator)))
    {
      size_t token_length = ntoken - string;
      /* Make this substring look like a string.  */
      *ntoken = '\0';
      aarch64_parse_one_override_token (string, token_length, tune);
      string = ++ntoken;
    }

  /* One last option to parse.  */
  aarch64_parse_one_override_token (string, strlen (string), tune);
  free (string_root);
}

/* Adjust CURRENT_TUNE (a generic tuning struct) with settings that
   are best for a generic target with the currently-enabled architecture
   extensions.  */
static void
aarch64_adjust_generic_arch_tuning (struct tune_params &current_tune)
{
  /* Neoverse V1 is the only core that is known to benefit from
     AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS.  There is therefore no
     point enabling it for SVE2 and above.  */
  if (TARGET_SVE2)
    current_tune.extra_tuning_flags
      &= ~AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS;
}

static void
aarch64_override_options_after_change_1 (struct gcc_options *opts)
{
  /* PR 70044: We have to be careful about being called multiple times for the
     same function.  This means all changes should be repeatable.  */

  /* Set aarch64_use_frame_pointer based on -fno-omit-frame-pointer.
     Disable the frame pointer flag so the mid-end will not use a frame
     pointer in leaf functions in order to support -fomit-leaf-frame-pointer.
     Set x_flag_omit_frame_pointer to the special value 2 to differentiate
     between -fomit-frame-pointer (1) and -fno-omit-frame-pointer (2).  */
  aarch64_use_frame_pointer = opts->x_flag_omit_frame_pointer != 1;
  if (opts->x_flag_omit_frame_pointer == 0)
    opts->x_flag_omit_frame_pointer = 2;

  /* If not optimizing for size, set the default
     alignment to what the target wants.  */
  if (!opts->x_optimize_size)
    {
      if (opts->x_flag_align_loops && !opts->x_str_align_loops)
	opts->x_str_align_loops = aarch64_tune_params.loop_align;
      if (opts->x_flag_align_jumps && !opts->x_str_align_jumps)
	opts->x_str_align_jumps = aarch64_tune_params.jump_align;
      if (opts->x_flag_align_functions && !opts->x_str_align_functions)
	opts->x_str_align_functions = aarch64_tune_params.function_align;
    }

  /* We default to no pc-relative literal loads.  */

  aarch64_pcrelative_literal_loads = false;

  /* If -mpc-relative-literal-loads is set on the command line, this
     implies that the user asked for PC relative literal loads.  */
  if (opts->x_pcrelative_literal_loads == 1)
    aarch64_pcrelative_literal_loads = true;

  /* In the tiny memory model it makes no sense to disallow PC relative
     literal pool loads.  */
  if (aarch64_cmodel == AARCH64_CMODEL_TINY
      || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)
    aarch64_pcrelative_literal_loads = true;

  /* When enabling the lower precision Newton series for the square root, also
     enable it for the reciprocal square root, since the latter is an
     intermediary step for the former.  */
  if (flag_mlow_precision_sqrt)
    flag_mrecip_low_precision_sqrt = true;
}

/* 'Unpack' up the internal tuning structs and update the options
    in OPTS.  The caller must have set up selected_tune and selected_arch
    as all the other target-specific codegen decisions are
    derived from them.  */

void
aarch64_override_options_internal (struct gcc_options *opts)
{
  const struct processor *tune = aarch64_get_tune_cpu (opts->x_selected_tune);
  aarch64_tune = tune->sched_core;
  /* Make a copy of the tuning parameters attached to the core, which
     we may later overwrite.  */
  aarch64_tune_params = *(tune->tune);
  if (tune->tune == &generic_tunings)
    aarch64_adjust_generic_arch_tuning (aarch64_tune_params);

  if (opts->x_aarch64_override_tune_string)
    aarch64_parse_override_string (opts->x_aarch64_override_tune_string,
				   &aarch64_tune_params);

  if (opts->x_aarch64_ldp_policy_param)
    aarch64_tune_params.ldp_policy_model = opts->x_aarch64_ldp_policy_param;

  if (opts->x_aarch64_stp_policy_param)
    aarch64_tune_params.stp_policy_model = opts->x_aarch64_stp_policy_param;

  /* This target defaults to strict volatile bitfields.  */
  if (opts->x_flag_strict_volatile_bitfields < 0 && abi_version_at_least (2))
    opts->x_flag_strict_volatile_bitfields = 1;

  if (aarch64_stack_protector_guard == SSP_GLOBAL
      && opts->x_aarch64_stack_protector_guard_offset_str)
    {
      error ("incompatible options %<-mstack-protector-guard=global%> and "
	     "%<-mstack-protector-guard-offset=%s%>",
	     aarch64_stack_protector_guard_offset_str);
    }

  if (aarch64_stack_protector_guard == SSP_SYSREG
      && !(opts->x_aarch64_stack_protector_guard_offset_str
	   && opts->x_aarch64_stack_protector_guard_reg_str))
    {
      error ("both %<-mstack-protector-guard-offset%> and "
	     "%<-mstack-protector-guard-reg%> must be used "
	     "with %<-mstack-protector-guard=sysreg%>");
    }

  if (opts->x_aarch64_stack_protector_guard_reg_str)
    {
      if (strlen (opts->x_aarch64_stack_protector_guard_reg_str) > 100)
	  error ("specify a system register with a small string length");
    }

  if (opts->x_aarch64_stack_protector_guard_offset_str)
    {
      char *end;
      const char *str = aarch64_stack_protector_guard_offset_str;
      errno = 0;
      long offs = strtol (aarch64_stack_protector_guard_offset_str, &end, 0);
      if (!*str || *end || errno)
	error ("%qs is not a valid offset in %qs", str,
	       "-mstack-protector-guard-offset=");
      aarch64_stack_protector_guard_offset = offs;
    }

  if ((flag_sanitize & SANITIZE_SHADOW_CALL_STACK)
      && !fixed_regs[R18_REGNUM])
    error ("%<-fsanitize=shadow-call-stack%> requires %<-ffixed-x18%>");

  aarch64_feature_flags isa_flags = aarch64_get_isa_flags (opts);
  if ((isa_flags & (AARCH64_FL_SM_ON | AARCH64_FL_ZA_ON))
      && !(isa_flags & AARCH64_FL_SME))
    {
      if (isa_flags & AARCH64_FL_SM_ON)
	error ("streaming functions require the ISA extension %qs", "sme");
      else
	error ("functions with SME state require the ISA extension %qs",
	       "sme");
      inform (input_location, "you can enable %qs using the command-line"
	      " option %<-march%>, or by using the %<target%>"
	      " attribute or pragma", "sme");
      opts->x_target_flags &= ~MASK_GENERAL_REGS_ONLY;
      auto new_flags = isa_flags | feature_deps::SME ().enable;
      aarch64_set_asm_isa_flags (opts, new_flags);
    }

  initialize_aarch64_code_model (opts);
  initialize_aarch64_tls_size (opts);
  aarch64_tpidr_register = opts->x_aarch64_tpidr_reg;

  int queue_depth = 0;
  switch (aarch64_tune_params.autoprefetcher_model)
    {
      case tune_params::AUTOPREFETCHER_OFF:
	queue_depth = -1;
	break;
      case tune_params::AUTOPREFETCHER_WEAK:
	queue_depth = 0;
	break;
      case tune_params::AUTOPREFETCHER_STRONG:
	queue_depth = max_insn_queue_index + 1;
	break;
      default:
	gcc_unreachable ();
    }

  /* We don't mind passing in global_options_set here as we don't use
     the *options_set structs anyway.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_sched_autopref_queue_depth, queue_depth);

  /* Set up parameters to be used in prefetching algorithm.  Do not
     override the defaults unless we are tuning for a core we have
     researched values for.  */
  if (aarch64_tune_params.prefetch->num_slots > 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_simultaneous_prefetches,
			 aarch64_tune_params.prefetch->num_slots);
  if (aarch64_tune_params.prefetch->l1_cache_size >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_l1_cache_size,
			 aarch64_tune_params.prefetch->l1_cache_size);
  if (aarch64_tune_params.prefetch->l1_cache_line_size >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_l1_cache_line_size,
			 aarch64_tune_params.prefetch->l1_cache_line_size);

  if (aarch64_tune_params.prefetch->l1_cache_line_size >= 0)
    {
      SET_OPTION_IF_UNSET (opts, &global_options_set,
			   param_destruct_interfere_size,
			   aarch64_tune_params.prefetch->l1_cache_line_size);
      SET_OPTION_IF_UNSET (opts, &global_options_set,
			   param_construct_interfere_size,
			   aarch64_tune_params.prefetch->l1_cache_line_size);
    }
  else
    {
      /* For a generic AArch64 target, cover the current range of cache line
	 sizes.  */
      SET_OPTION_IF_UNSET (opts, &global_options_set,
			   param_destruct_interfere_size,
			   256);
      SET_OPTION_IF_UNSET (opts, &global_options_set,
			   param_construct_interfere_size,
			   64);
    }

  if (aarch64_tune_params.prefetch->l2_cache_size >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_l2_cache_size,
			 aarch64_tune_params.prefetch->l2_cache_size);
  if (!aarch64_tune_params.prefetch->prefetch_dynamic_strides)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_prefetch_dynamic_strides, 0);
  if (aarch64_tune_params.prefetch->minimum_stride >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_prefetch_minimum_stride,
			 aarch64_tune_params.prefetch->minimum_stride);

  /* Use the alternative scheduling-pressure algorithm by default.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_sched_pressure_algorithm,
		       SCHED_PRESSURE_MODEL);

  /* Validate the guard size.  */
  int guard_size = param_stack_clash_protection_guard_size;

  if (guard_size != 12 && guard_size != 16)
    error ("only values 12 (4 KB) and 16 (64 KB) are supported for guard "
	   "size.  Given value %d (%llu KB) is out of range",
	   guard_size, (1ULL << guard_size) / 1024ULL);

  /* Enforce that interval is the same size as size so the mid-end does the
     right thing.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_stack_clash_protection_probe_interval,
		       guard_size);

  /* The maybe_set calls won't update the value if the user has explicitly set
     one.  Which means we need to validate that probing interval and guard size
     are equal.  */
  int probe_interval
    = param_stack_clash_protection_probe_interval;
  if (guard_size != probe_interval)
    error ("stack clash guard size %<%d%> must be equal to probing interval "
	   "%<%d%>", guard_size, probe_interval);

  /* Enable sw prefetching at specified optimization level for
     CPUS that have prefetch.  Lower optimization level threshold by 1
     when profiling is enabled.  */
  if (opts->x_flag_prefetch_loop_arrays < 0
      && !opts->x_optimize_size
      && aarch64_tune_params.prefetch->default_opt_level >= 0
      && opts->x_optimize >= aarch64_tune_params.prefetch->default_opt_level)
    opts->x_flag_prefetch_loop_arrays = 1;

  /* Avoid loop-dependant FMA chains.  */
  if (aarch64_tune_params.extra_tuning_flags
      & AARCH64_EXTRA_TUNE_AVOID_CROSS_LOOP_FMA)
    SET_OPTION_IF_UNSET (opts, &global_options_set, param_avoid_fma_max_bits,
			 512);

  /* Consider fully pipelined FMA in reassociation.  */
  if (aarch64_tune_params.extra_tuning_flags
      & AARCH64_EXTRA_TUNE_FULLY_PIPELINED_FMA)
    SET_OPTION_IF_UNSET (opts, &global_options_set, param_fully_pipelined_fma,
			 1);

  aarch64_override_options_after_change_1 (opts);
}

/* Print a hint with a suggestion for a core or architecture name that
   most closely resembles what the user passed in STR.  ARCH is true if
   the user is asking for an architecture name.  ARCH is false if the user
   is asking for a core name.  */

static void
aarch64_print_hint_for_core_or_arch (const char *str, bool arch)
{
  auto_vec<const char *> candidates;
  const struct processor *entry = arch ? all_architectures : all_cores;
  for (; entry->name != NULL; entry++)
    candidates.safe_push (entry->name);

#ifdef HAVE_LOCAL_CPU_DETECT
  /* Add also "native" as possible value.  */
  if (arch)
    candidates.safe_push ("native");
#endif

  char *s;
  const char *hint = candidates_list_and_hint (str, s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s;"
			     " did you mean %qs?", s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

/* Print a hint with a suggestion for a core name that most closely resembles
   what the user passed in STR.  */

inline static void
aarch64_print_hint_for_core (const char *str)
{
  aarch64_print_hint_for_core_or_arch (str, false);
}

/* Print a hint with a suggestion for an architecture name that most closely
   resembles what the user passed in STR.  */

inline static void
aarch64_print_hint_for_arch (const char *str)
{
  aarch64_print_hint_for_core_or_arch (str, true);
}


/* Print a hint with a suggestion for an extension name
   that most closely resembles what the user passed in STR.  */

void
aarch64_print_hint_for_extensions (const std::string &str)
{
  auto_vec<const char *> candidates;
  aarch64_get_all_extension_candidates (&candidates);
  char *s;
  const char *hint = candidates_list_and_hint (str.c_str (), s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s;"
			     " did you mean %qs?", s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

/* Validate a command-line -mcpu option.  Parse the cpu and extensions (if any)
   specified in STR and throw errors if appropriate.  Put the results if
   they are valid in RES and ISA_FLAGS.  Return whether the option is
   valid.  */

static bool
aarch64_validate_mcpu (const char *str, const struct processor **res,
		       aarch64_feature_flags *isa_flags)
{
  std::string invalid_extension;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_cpu (str, res, isa_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing cpu name in %<-mcpu=%s%>", str);
	break;
      case AARCH_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-mcpu%>", str);
	aarch64_print_hint_for_core (str);
	/* A common user error is confusing -march and -mcpu.
	   If the -mcpu string matches a known architecture then suggest
	   -march=.  */
	parse_res = aarch64_parse_arch (str, res, isa_flags, &invalid_extension);
	if (parse_res == AARCH_PARSE_OK)
	  inform (input_location, "did you mean %<-march=%s%>?", str);
	break;
      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs in %<-mcpu=%s%>",
	       invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Straight line speculation indicators.  */
enum aarch64_sls_hardening_type
{
  SLS_NONE = 0,
  SLS_RETBR = 1,
  SLS_BLR = 2,
  SLS_ALL = 3,
};
static enum aarch64_sls_hardening_type aarch64_sls_hardening;

/* Return whether we should mitigatate Straight Line Speculation for the RET
   and BR instructions.  */
bool
aarch64_harden_sls_retbr_p (void)
{
  return aarch64_sls_hardening & SLS_RETBR;
}

/* Return whether we should mitigatate Straight Line Speculation for the BLR
   instruction.  */
bool
aarch64_harden_sls_blr_p (void)
{
  return aarch64_sls_hardening & SLS_BLR;
}

/* As of yet we only allow setting these options globally, in the future we may
   allow setting them per function.  */
static void
aarch64_validate_sls_mitigation (const char *const_str)
{
  char *token_save = NULL;
  char *str = NULL;

  if (strcmp (const_str, "none") == 0)
    {
      aarch64_sls_hardening = SLS_NONE;
      return;
    }
  if (strcmp (const_str, "all") == 0)
    {
      aarch64_sls_hardening = SLS_ALL;
      return;
    }

  char *str_root = xstrdup (const_str);
  str = strtok_r (str_root, ",", &token_save);
  if (!str)
    error ("invalid argument given to %<-mharden-sls=%>");

  int temp = SLS_NONE;
  while (str)
    {
      if (strcmp (str, "blr") == 0)
	temp |= SLS_BLR;
      else if (strcmp (str, "retbr") == 0)
	temp |= SLS_RETBR;
      else if (strcmp (str, "none") == 0 || strcmp (str, "all") == 0)
	{
	  error ("%qs must be by itself for %<-mharden-sls=%>", str);
	  break;
	}
      else
	{
	  error ("invalid argument %<%s%> for %<-mharden-sls=%>", str);
	  break;
	}
      str = strtok_r (NULL, ",", &token_save);
    }
  aarch64_sls_hardening = (aarch64_sls_hardening_type) temp;
  free (str_root);
}

/* Validate a command-line -march option.  Parse the arch and extensions
   (if any) specified in STR and throw errors if appropriate.  Put the
   results, if they are valid, in RES and ISA_FLAGS.  Return whether the
   option is valid.  */

static bool
aarch64_validate_march (const char *str, const struct processor **res,
			aarch64_feature_flags *isa_flags)
{
  std::string invalid_extension;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_arch (str, res, isa_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing arch name in %<-march=%s%>", str);
	break;
      case AARCH_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-march%>", str);
	aarch64_print_hint_for_arch (str);
	/* A common user error is confusing -march and -mcpu.
	   If the -march string matches a known CPU suggest -mcpu.  */
	parse_res = aarch64_parse_cpu (str, res, isa_flags, &invalid_extension);
	if (parse_res == AARCH_PARSE_OK)
	  inform (input_location, "did you mean %<-mcpu=%s%>?", str);
	break;
      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs in %<-march=%s%>",
	       invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Validate a command-line -mtune option.  Parse the cpu
   specified in STR and throw errors if appropriate.  Put the
   result, if it is valid, in RES.  Return whether the option is
   valid.  */

static bool
aarch64_validate_mtune (const char *str, const struct processor **res)
{
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_tune (str, res);

  if (parse_res == AARCH_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing cpu name in %<-mtune=%s%>", str);
	break;
      case AARCH_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-mtune%>", str);
	aarch64_print_hint_for_core (str);
	break;
      default:
	gcc_unreachable ();
    }
  return false;
}

/* Return the VG value associated with -msve-vector-bits= value VALUE.  */

static poly_uint16
aarch64_convert_sve_vector_bits (aarch64_sve_vector_bits_enum value)
{
  /* 128-bit SVE and Advanced SIMD modes use different register layouts
     on big-endian targets, so we would need to forbid subregs that convert
     from one to the other.  By default a reinterpret sequence would then
     involve a store to memory in one mode and a load back in the other.
     Even if we optimize that sequence using reverse instructions,
     it would still be a significant potential overhead.

     For now, it seems better to generate length-agnostic code for that
     case instead.  */
  if (value == SVE_SCALABLE
      || (value == SVE_128 && BYTES_BIG_ENDIAN))
    return poly_uint16 (2, 2);
  else
    return (int) value / 64;
}

/* Set the global aarch64_asm_isa_flags to FLAGS and update
   aarch64_isa_flags accordingly.  */

void
aarch64_set_asm_isa_flags (aarch64_feature_flags flags)
{
  aarch64_set_asm_isa_flags (&global_options, flags);
}

static void
aarch64_handle_no_branch_protection (void)
{
  aarch_ra_sign_scope = AARCH_FUNCTION_NONE;
  aarch_enable_bti = 0;
  aarch64_enable_gcs = 0;
}

static void
aarch64_handle_standard_branch_protection (void)
{
  aarch_ra_sign_scope = AARCH_FUNCTION_NON_LEAF;
  aarch64_ra_sign_key = AARCH64_KEY_A;
  aarch_enable_bti = 1;
  aarch64_enable_gcs = 1;
}

static void
aarch64_handle_pac_ret_protection (void)
{
  aarch_ra_sign_scope = AARCH_FUNCTION_NON_LEAF;
  aarch64_ra_sign_key = AARCH64_KEY_A;
}

static void
aarch64_handle_pac_ret_leaf (void)
{
  aarch_ra_sign_scope = AARCH_FUNCTION_ALL;
}

static void
aarch64_handle_pac_ret_b_key (void)
{
  aarch64_ra_sign_key = AARCH64_KEY_B;
}

static void
aarch64_handle_bti_protection (void)
{
  aarch_enable_bti = 1;
}
static void
aarch64_handle_gcs_protection (void)
{
  aarch64_enable_gcs = 1;
}

static const struct aarch_branch_protect_type aarch64_pac_ret_subtypes[] = {
  { "leaf", false, aarch64_handle_pac_ret_leaf, NULL, 0 },
  { "b-key", false, aarch64_handle_pac_ret_b_key, NULL, 0 },
  { NULL, false, NULL, NULL, 0 }
};

static const struct aarch_branch_protect_type aarch64_branch_protect_types[] =
{
  { "none", true, aarch64_handle_no_branch_protection, NULL, 0 },
  { "standard", true, aarch64_handle_standard_branch_protection, NULL, 0 },
  { "pac-ret", false, aarch64_handle_pac_ret_protection,
    aarch64_pac_ret_subtypes, ARRAY_SIZE (aarch64_pac_ret_subtypes) },
  { "bti", false, aarch64_handle_bti_protection, NULL, 0 },
  { "gcs", false, aarch64_handle_gcs_protection, NULL, 0 },
  { NULL, false, NULL, NULL, 0 }
};

/* Implement TARGET_OPTION_OVERRIDE.  This is called once in the beginning
   and is used to parse the -m{cpu,tune,arch} strings and setup the initial
   tuning structs.  In particular it must set selected_tune and
   aarch64_asm_isa_flags that define the available ISA features and tuning
   decisions.  It must also set selected_arch as this will be used to
   output the .arch asm tags for each function.  */

static void
aarch64_override_options (void)
{
  aarch64_feature_flags cpu_isa = 0;
  aarch64_feature_flags arch_isa = 0;
  aarch64_set_asm_isa_flags (0);

  const struct processor *cpu = NULL;
  const struct processor *arch = NULL;
  const struct processor *tune = NULL;

  if (aarch64_harden_sls_string)
    aarch64_validate_sls_mitigation (aarch64_harden_sls_string);

  if (aarch64_branch_protection_string)
    aarch_validate_mbranch_protection (aarch64_branch_protect_types,
				       aarch64_branch_protection_string,
				       "-mbranch-protection=");

  /* -mcpu=CPU is shorthand for -march=ARCH_FOR_CPU, -mtune=CPU.
     If either of -march or -mtune is given, they override their
     respective component of -mcpu.  */
  if (aarch64_cpu_string)
    aarch64_validate_mcpu (aarch64_cpu_string, &cpu, &cpu_isa);

  if (aarch64_arch_string)
    aarch64_validate_march (aarch64_arch_string, &arch, &arch_isa);

  if (aarch64_tune_string)
    aarch64_validate_mtune (aarch64_tune_string, &tune);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  if (cpu && arch)
    {
      /* If both -mcpu and -march are specified, warn if they are not
	 feature compatible.  feature compatible means that the inclusion of the
	 cpu features would end up disabling an achitecture feature.  In
	 otherwords the cpu features need to be a strict superset of the arch
	 features and if so prefer the -march ISA flags.  */
      auto full_arch_flags = arch->flags | arch_isa;
      auto full_cpu_flags = cpu->flags | cpu_isa;
      if (~full_cpu_flags & full_arch_flags)
	{
	  std::string ext_diff
	    = aarch64_get_extension_string_for_isa_flags (full_arch_flags,
							  full_cpu_flags);
	  warning (0, "switch %<-mcpu=%s%> conflicts with %<-march=%s%> switch "
		      "and resulted in options %<%s%> being added",
		       aarch64_cpu_string,
		       aarch64_arch_string,
		       ext_diff.c_str ());
	}

      selected_arch = arch->arch;
      aarch64_set_asm_isa_flags (arch_isa | AARCH64_FL_DEFAULT_ISA_MODE);
    }
  else if (cpu)
    {
      selected_arch = cpu->arch;
      aarch64_set_asm_isa_flags (cpu_isa | AARCH64_FL_DEFAULT_ISA_MODE);
    }
  else if (arch)
    {
      cpu = &all_cores[arch->ident];
      selected_arch = arch->arch;
      aarch64_set_asm_isa_flags (arch_isa | AARCH64_FL_DEFAULT_ISA_MODE);
    }
  else
    {
      /* No -mcpu or -march specified, so use the default CPU.  */
      cpu = &all_cores[TARGET_CPU_DEFAULT];
      selected_arch = cpu->arch;
      aarch64_set_asm_isa_flags (cpu->flags | AARCH64_FL_DEFAULT_ISA_MODE);
    }

  selected_tune = tune ? tune->ident : cpu->ident;

  if (aarch_enable_bti == 2)
    {
#ifdef TARGET_ENABLE_BTI
      aarch_enable_bti = 1;
#else
      aarch_enable_bti = 0;
#endif
    }

  if (aarch64_enable_gcs == 2)
    {
#ifdef TARGET_ENABLE_GCS
      aarch64_enable_gcs = 1;
#else
      aarch64_enable_gcs = 0;
#endif
    }

  /* Return address signing is currently not supported for ILP32 targets.  For
     LP64 targets use the configured option in the absence of a command-line
     option for -mbranch-protection.  */
  if (!TARGET_ILP32 && aarch64_branch_protection_string == NULL)
    {
#ifdef TARGET_ENABLE_PAC_RET
      aarch_ra_sign_scope = AARCH_FUNCTION_NON_LEAF;
#else
      aarch_ra_sign_scope = AARCH_FUNCTION_NONE;
#endif
    }

#ifndef HAVE_AS_MABI_OPTION
  /* The compiler may have been configured with 2.23.* binutils, which does
     not have support for ILP32.  */
  if (TARGET_ILP32)
    error ("assembler does not support %<-mabi=ilp32%>");
#endif

  /* Convert -msve-vector-bits to a VG count.  */
  aarch64_sve_vg = aarch64_convert_sve_vector_bits (aarch64_sve_vector_bits);

  if (aarch_ra_sign_scope != AARCH_FUNCTION_NONE && TARGET_ILP32)
    sorry ("return address signing is only supported for %<-mabi=lp64%>");

  /* The pass to insert speculation tracking runs before
     shrink-wrapping and the latter does not know how to update the
     tracking status.  So disable it in this case.  */
  if (aarch64_track_speculation)
    flag_shrink_wrap = 0;

  aarch64_override_options_internal (&global_options);

  /* Save these options as the default ones in case we push and pop them later
     while processing functions with potential target attributes.  */
  target_option_default_node = target_option_current_node
    = build_target_option_node (&global_options, &global_options_set);
}

/* Implement targetm.override_options_after_change.  */

static void
aarch64_override_options_after_change (void)
{
  aarch64_override_options_after_change_1 (&global_options);
}

/* Implement the TARGET_OFFLOAD_OPTIONS hook.  */
static char *
aarch64_offload_options (void)
{
  if (TARGET_ILP32)
    return xstrdup ("-foffload-abi=ilp32 -foffload-abi-host-opts=-mabi=ilp32");
  else
    return xstrdup ("-foffload-abi=lp64 -foffload-abi-host-opts=-mabi=lp64");
}

static struct machine_function *
aarch64_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();
  return machine;
}

void
aarch64_init_expanders (void)
{
  init_machine_status = aarch64_init_machine_status;
}

/* A checking mechanism for the implementation of the various code models.  */
static void
initialize_aarch64_code_model (struct gcc_options *opts)
{
  aarch64_cmodel = opts->x_aarch64_cmodel_var;
  switch (opts->x_aarch64_cmodel_var)
    {
    case AARCH64_CMODEL_TINY:
      if (opts->x_flag_pic)
	aarch64_cmodel = AARCH64_CMODEL_TINY_PIC;
      break;
    case AARCH64_CMODEL_SMALL:
      if (opts->x_flag_pic)
	{
#ifdef HAVE_AS_SMALL_PIC_RELOCS
	  aarch64_cmodel = (flag_pic == 2
			    ? AARCH64_CMODEL_SMALL_PIC
			    : AARCH64_CMODEL_SMALL_SPIC);
#else
	  aarch64_cmodel = AARCH64_CMODEL_SMALL_PIC;
#endif
	}
      break;
    case AARCH64_CMODEL_LARGE:
      if (opts->x_flag_pic)
	sorry ("code model %qs with %<-f%s%>", "large",
	       opts->x_flag_pic > 1 ? "PIC" : "pic");
      if (opts->x_aarch64_abi == AARCH64_ABI_ILP32)
	sorry ("code model %qs not supported in ilp32 mode", "large");
      break;
    case AARCH64_CMODEL_TINY_PIC:
    case AARCH64_CMODEL_SMALL_PIC:
    case AARCH64_CMODEL_SMALL_SPIC:
      gcc_unreachable ();
    }
}

/* Implements TARGET_OPTION_RESTORE.  Restore the backend codegen decisions
   using the information saved in PTR.  */

static void
aarch64_option_restore (struct gcc_options *opts,
			struct gcc_options * /* opts_set */,
			struct cl_target_option * /* ptr */)
{
  aarch64_override_options_internal (opts);
}

/* Implement TARGET_OPTION_PRINT.  */

static void
aarch64_option_print (FILE *file, int indent, struct cl_target_option *ptr)
{
  const struct processor *cpu
    = aarch64_get_tune_cpu (ptr->x_selected_tune);
  const struct processor *arch = aarch64_get_arch (ptr->x_selected_arch);
  aarch64_feature_flags isa_flags = aarch64_get_asm_isa_flags(ptr);
  std::string extension
    = aarch64_get_extension_string_for_isa_flags (isa_flags, arch->flags);

  fprintf (file, "%*sselected tune = %s\n", indent, "", cpu->name);
  fprintf (file, "%*sselected arch = %s%s\n", indent, "",
	   arch->name, extension.c_str ());
}

static GTY(()) tree aarch64_previous_fndecl;

void
aarch64_reset_previous_fndecl (void)
{
  aarch64_previous_fndecl = NULL;
}

/* Restore or save the TREE_TARGET_GLOBALS from or to NEW_TREE.
   Used by aarch64_set_current_function and aarch64_pragma_target_parse to
   make sure optab availability predicates are recomputed when necessary.  */

void
aarch64_save_restore_target_globals (tree new_tree)
{
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
}

/* Return the target_option_node for FNDECL, or the current options
   if FNDECL is null.  */

static tree
aarch64_fndecl_options (tree fndecl)
{
  if (!fndecl)
    return target_option_current_node;

  if (tree options = DECL_FUNCTION_SPECIFIC_TARGET (fndecl))
    return options;

  return target_option_default_node;
}

/* Implement TARGET_SET_CURRENT_FUNCTION.  Unpack the codegen decisions
   like tuning and ISA features from the DECL_FUNCTION_SPECIFIC_TARGET
   of the function, if such exists.  This function may be called multiple
   times on a single function so use aarch64_previous_fndecl to avoid
   setting up identical state.  */

static void
aarch64_set_current_function (tree fndecl)
{
  tree old_tree = aarch64_fndecl_options (aarch64_previous_fndecl);
  tree new_tree = aarch64_fndecl_options (fndecl);

  auto new_isa_mode = (fndecl
		       ? aarch64_fndecl_isa_mode (fndecl)
		       : AARCH64_DEFAULT_ISA_MODE);
  auto isa_flags = aarch64_get_isa_flags (TREE_TARGET_OPTION (new_tree));

  static bool reported_zt0_p;
  if (!reported_zt0_p
      && !(isa_flags & AARCH64_FL_SME2)
      && fndecl
      && aarch64_fndecl_has_state (fndecl, "zt0"))
    {
      error ("functions with %qs state require the ISA extension %qs",
	     "zt0", "sme2");
      inform (input_location, "you can enable %qs using the command-line"
	      " option %<-march%>, or by using the %<target%>"
	      " attribute or pragma", "sme2");
      reported_zt0_p = true;
    }

  /* If nothing to do, return.  #pragma GCC reset or #pragma GCC pop to
     the default have been handled by aarch64_save_restore_target_globals from
     aarch64_pragma_target_parse.  */
  if (old_tree == new_tree
      && (!fndecl || aarch64_previous_fndecl)
      && (isa_flags & AARCH64_FL_ISA_MODES).val[0] == new_isa_mode)
    {
      gcc_assert (AARCH64_ISA_MODE == new_isa_mode);
      return;
    }

  aarch64_previous_fndecl = fndecl;

  /* First set the target options.  */
  cl_target_option_restore (&global_options, &global_options_set,
			    TREE_TARGET_OPTION (new_tree));

  /* The ISA mode can vary based on function type attributes and
     function declaration attributes.  Make sure that the target
     options correctly reflect these attributes.  */
  if ((isa_flags & AARCH64_FL_ISA_MODES).val[0] != new_isa_mode)
    {
      auto base_flags = (aarch64_asm_isa_flags & ~AARCH64_FL_ISA_MODES);
      aarch64_set_asm_isa_flags (base_flags
				 | aarch64_feature_flags (new_isa_mode));

      aarch64_override_options_internal (&global_options);
      new_tree = build_target_option_node (&global_options,
					   &global_options_set);
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_tree;

      tree new_optimize = build_optimization_node (&global_options,
						   &global_options_set);
      if (new_optimize != optimization_default_node)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  aarch64_save_restore_target_globals (new_tree);

  gcc_assert (AARCH64_ISA_MODE == new_isa_mode);
}

/* Enum describing the various ways we can handle attributes.
   In many cases we can reuse the generic option handling machinery.  */

enum aarch64_attr_opt_type
{
  aarch64_attr_mask,	/* Attribute should set a bit in target_flags.  */
  aarch64_attr_bool,	/* Attribute sets or unsets a boolean variable.  */
  aarch64_attr_enum,	/* Attribute sets an enum variable.  */
  aarch64_attr_custom	/* Attribute requires a custom handling function.  */
};

/* All the information needed to handle a target attribute.
   NAME is the name of the attribute.
   ATTR_TYPE specifies the type of behavior of the attribute as described
   in the definition of enum aarch64_attr_opt_type.
   ALLOW_NEG is true if the attribute supports a "no-" form.
   HANDLER is the function that takes the attribute string as an argument
   It is needed only when the ATTR_TYPE is aarch64_attr_custom.
   OPT_NUM is the enum specifying the option that the attribute modifies.
   This is needed for attributes that mirror the behavior of a command-line
   option, that is it has ATTR_TYPE aarch64_attr_mask, aarch64_attr_bool or
   aarch64_attr_enum.  */

struct aarch64_attribute_info
{
  const char *name;
  enum aarch64_attr_opt_type attr_type;
  bool allow_neg;
  bool (*handler) (const char *);
  enum opt_code opt_num;
};

/* Handle the ARCH_STR argument to the arch= target attribute.  */

static bool
aarch64_handle_attr_arch (const char *str)
{
  const struct processor *tmp_arch = NULL;
  std::string invalid_extension;
  aarch64_feature_flags tmp_flags;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_arch (str, &tmp_arch, &tmp_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    {
      gcc_assert (tmp_arch);
      selected_arch = tmp_arch->arch;
      aarch64_set_asm_isa_flags (tmp_flags | (aarch64_asm_isa_flags
					      & AARCH64_FL_ISA_MODES));
      return true;
    }

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing name in %<target(\"arch=\")%> pragma or attribute");
	break;
      case AARCH_PARSE_INVALID_ARG:
	error ("invalid name %qs in %<target(\"arch=\")%> pragma or attribute", str);
	aarch64_print_hint_for_arch (str);
	break;
      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %s of value %qs in "
	       "%<target()%> pragma or attribute", invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Handle the argument CPU_STR to the cpu= target attribute.  */

static bool
aarch64_handle_attr_cpu (const char *str)
{
  const struct processor *tmp_cpu = NULL;
  std::string invalid_extension;
  aarch64_feature_flags tmp_flags;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_cpu (str, &tmp_cpu, &tmp_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    {
      gcc_assert (tmp_cpu);
      selected_tune = tmp_cpu->ident;
      selected_arch = tmp_cpu->arch;
      aarch64_set_asm_isa_flags (tmp_flags | (aarch64_asm_isa_flags
					      & AARCH64_FL_ISA_MODES));
      return true;
    }

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing name in %<target(\"cpu=\")%> pragma or attribute");
	break;
      case AARCH_PARSE_INVALID_ARG:
	error ("invalid name %qs in %<target(\"cpu=\")%> pragma or attribute", str);
	aarch64_print_hint_for_core (str);
	break;
      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs of value %qs in "
	       "%<target()%> pragma or attribute", invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Handle the argument STR to the branch-protection= attribute.  */

static bool
aarch64_handle_attr_branch_protection (const char* str)
{
  return aarch_validate_mbranch_protection (aarch64_branch_protect_types, str,
					    "target(\"branch-protection=\")");
}

/* Handle the argument STR to the tune= target attribute.  */

static bool
aarch64_handle_attr_tune (const char *str)
{
  const struct processor *tmp_tune = NULL;
  enum aarch_parse_opt_result parse_res
    = aarch64_parse_tune (str, &tmp_tune);

  if (parse_res == AARCH_PARSE_OK)
    {
      gcc_assert (tmp_tune);
      selected_tune = tmp_tune->ident;
      return true;
    }

  switch (parse_res)
    {
      case AARCH_PARSE_INVALID_ARG:
	error ("invalid name %qs in %<target(\"tune=\")%> pragma or attribute", str);
	aarch64_print_hint_for_core (str);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Parse an architecture extensions target attribute string specified in STR.
   For example "+fp+nosimd".  Show any errors if needed.  Return TRUE
   if successful.  Update aarch64_isa_flags to reflect the ISA features
   modified.  */

static bool
aarch64_handle_attr_isa_flags (char *str)
{
  enum aarch_parse_opt_result parse_res;
  auto isa_flags = aarch64_asm_isa_flags;

  /* We allow "+nothing" in the beginning to clear out all architectural
     features if the user wants to handpick specific features.  */
  if (strncmp ("+nothing", str, 8) == 0)
    {
      isa_flags &= AARCH64_FL_ISA_MODES;
      str += 8;
    }

  std::string invalid_extension;
  parse_res = aarch64_parse_extension (str, &isa_flags, &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    {
      aarch64_set_asm_isa_flags (isa_flags);
      return true;
    }

  switch (parse_res)
    {
      case AARCH_PARSE_MISSING_ARG:
	error ("missing value in %<target()%> pragma or attribute");
	break;

      case AARCH_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs of value %qs in "
	       "%<target()%> pragma or attribute", invalid_extension.c_str (), str);
	break;

      default:
	gcc_unreachable ();
    }

 return false;
}

/* The target attributes that we support.  On top of these we also support just
   ISA extensions, like  __attribute__ ((target ("+crc"))), but that case is
   handled explicitly in aarch64_process_one_target_attr.  */

static const struct aarch64_attribute_info aarch64_attributes[] =
{
  { "general-regs-only", aarch64_attr_mask, false, NULL,
     OPT_mgeneral_regs_only },
  { "fix-cortex-a53-835769", aarch64_attr_bool, true, NULL,
     OPT_mfix_cortex_a53_835769 },
  { "fix-cortex-a53-843419", aarch64_attr_bool, true, NULL,
     OPT_mfix_cortex_a53_843419 },
  { "cmodel", aarch64_attr_enum, false, NULL, OPT_mcmodel_ },
  { "strict-align", aarch64_attr_mask, true, NULL, OPT_mstrict_align },
  { "omit-leaf-frame-pointer", aarch64_attr_bool, true, NULL,
     OPT_momit_leaf_frame_pointer },
  { "tls-dialect", aarch64_attr_enum, false, NULL, OPT_mtls_dialect_ },
  { "arch", aarch64_attr_custom, false, aarch64_handle_attr_arch,
     OPT_march_ },
  { "cpu", aarch64_attr_custom, false, aarch64_handle_attr_cpu, OPT_mcpu_ },
  { "tune", aarch64_attr_custom, false, aarch64_handle_attr_tune,
     OPT_mtune_ },
  { "branch-protection", aarch64_attr_custom, false,
     aarch64_handle_attr_branch_protection, OPT_mbranch_protection_ },
  { "sign-return-address", aarch64_attr_enum, false, NULL,
     OPT_msign_return_address_ },
  { "outline-atomics", aarch64_attr_bool, true, NULL,
     OPT_moutline_atomics},
  { NULL, aarch64_attr_custom, false, NULL, OPT____ }
};

/* Parse ARG_STR which contains the definition of one target attribute.
   Show appropriate errors if any or return true if the attribute is valid.  */

static bool
aarch64_process_one_target_attr (char *arg_str)
{
  bool invert = false;

  size_t len = strlen (arg_str);

  if (len == 0)
    {
      error ("malformed %<target()%> pragma or attribute");
      return false;
    }

  auto_vec<char, 32> buffer;
  buffer.safe_grow (len + 1);
  char *str_to_check = buffer.address ();
  memcpy (str_to_check, arg_str, len + 1);

  /* We have something like __attribute__ ((target ("+fp+nosimd"))).
     It is easier to detect and handle it explicitly here rather than going
     through the machinery for the rest of the target attributes in this
     function.  */
  if (*str_to_check == '+')
    return aarch64_handle_attr_isa_flags (str_to_check);

  if (len > 3 && startswith (str_to_check, "no-"))
    {
      invert = true;
      str_to_check += 3;
    }
  char *arg = strchr (str_to_check, '=');

  /* If we found opt=foo then terminate STR_TO_CHECK at the '='
     and point ARG to "foo".  */
  if (arg)
    {
      *arg = '\0';
      arg++;
    }
  const struct aarch64_attribute_info *p_attr;
  bool found = false;
  for (p_attr = aarch64_attributes; p_attr->name; p_attr++)
    {
      /* If the names don't match up, or the user has given an argument
	 to an attribute that doesn't accept one, or didn't give an argument
	 to an attribute that expects one, fail to match.  */
      if (strcmp (str_to_check, p_attr->name) != 0)
	continue;

      found = true;
      bool attr_need_arg_p = p_attr->attr_type == aarch64_attr_custom
			      || p_attr->attr_type == aarch64_attr_enum;

      if (attr_need_arg_p ^ (arg != NULL))
	{
	  error ("pragma or attribute %<target(\"%s\")%> does not accept an argument", str_to_check);
	  return false;
	}

      /* If the name matches but the attribute does not allow "no-" versions
	 then we can't match.  */
      if (invert && !p_attr->allow_neg)
	{
	  error ("pragma or attribute %<target(\"%s\")%> does not allow a negated form", str_to_check);
	  return false;
	}

      switch (p_attr->attr_type)
	{
	/* Has a custom handler registered.
	   For example, cpu=, arch=, tune=.  */
	  case aarch64_attr_custom:
	    gcc_assert (p_attr->handler);
	    if (!p_attr->handler (arg))
	      return false;
	    break;

	  /* Either set or unset a boolean option.  */
	  case aarch64_attr_bool:
	    {
	      struct cl_decoded_option decoded;

	      generate_option (p_attr->opt_num, NULL, !invert,
			       CL_TARGET, &decoded);
	      aarch64_handle_option (&global_options, &global_options_set,
				      &decoded, input_location);
	      break;
	    }
	  /* Set or unset a bit in the target_flags.  aarch64_handle_option
	     should know what mask to apply given the option number.  */
	  case aarch64_attr_mask:
	    {
	      struct cl_decoded_option decoded;
	      /* We only need to specify the option number.
		 aarch64_handle_option will know which mask to apply.  */
	      decoded.opt_index = p_attr->opt_num;
	      decoded.value = !invert;
	      aarch64_handle_option (&global_options, &global_options_set,
				      &decoded, input_location);
	      break;
	    }
	  /* Use the option setting machinery to set an option to an enum.  */
	  case aarch64_attr_enum:
	    {
	      gcc_assert (arg);
	      bool valid;
	      int value;
	      valid = opt_enum_arg_to_value (p_attr->opt_num, arg,
					      &value, CL_TARGET);
	      if (valid)
		{
		  set_option (&global_options, NULL, p_attr->opt_num, value,
			      NULL, DK_UNSPECIFIED, input_location,
			      global_dc);
		}
	      else
		{
		  error ("pragma or attribute %<target(\"%s=%s\")%> is not valid", str_to_check, arg);
		}
	      break;
	    }
	  default:
	    gcc_unreachable ();
	}
    }

  /* If we reached here we either have found an attribute and validated
     it or didn't match any.  If we matched an attribute but its arguments
     were malformed we will have returned false already.  */
  return found;
}

/* Count how many times the character C appears in
   NULL-terminated string STR.  */

static unsigned int
num_occurences_in_str (char c, char *str)
{
  unsigned int res = 0;
  while (*str != '\0')
    {
      if (*str == c)
	res++;

      str++;
    }

  return res;
}

/* Parse the tree in ARGS that contains the target attribute information
   and update the global target options space.  */

bool
aarch64_process_target_attr (tree args)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree head = TREE_VALUE (args);
	  if (head)
	    {
	      if (!aarch64_process_target_attr (head))
		return false;
	    }
	  args = TREE_CHAIN (args);
	} while (args);

      return true;
    }

  if (TREE_CODE (args) != STRING_CST)
    {
      error ("attribute %<target%> argument not a string");
      return false;
    }

  size_t len = strlen (TREE_STRING_POINTER (args));
  auto_vec<char, 32> buffer;
  buffer.safe_grow (len + 1);
  char *str_to_check = buffer.address ();
  memcpy (str_to_check, TREE_STRING_POINTER (args), len + 1);

  if (len == 0)
    {
      error ("malformed %<target()%> pragma or attribute");
      return false;
    }

  /* Used to catch empty spaces between commas i.e.
     attribute ((target ("attr1,,attr2"))).  */
  unsigned int num_commas = num_occurences_in_str (',', str_to_check);

  /* Handle multiple target attributes separated by ','.  */
  char *token = strtok_r (str_to_check, ",", &str_to_check);

  unsigned int num_attrs = 0;
  while (token)
    {
      num_attrs++;
      if (!aarch64_process_one_target_attr (token))
	{
	  /* Check if token is possibly an arch extension without
	     leading '+'.  */
	  aarch64_feature_flags isa_temp = 0;
	  auto with_plus = std::string ("+") + token;
	  enum aarch_parse_opt_result ext_res
	    = aarch64_parse_extension (with_plus.c_str (), &isa_temp, nullptr);

	  if (ext_res == AARCH_PARSE_OK)
	    error ("arch extension %<%s%> should be prefixed by %<+%>",
		   token);
	  else
	    error ("pragma or attribute %<target(\"%s\")%> is not valid", token);
	  return false;
	}

      token = strtok_r (NULL, ",", &str_to_check);
    }

  if (num_attrs != num_commas + 1)
    {
      error ("malformed %<target(\"%s\")%> pragma or attribute", TREE_STRING_POINTER (args));
      return false;
    }

  return true;
}

static bool aarch64_process_target_version_attr (tree args);

/* Implement TARGET_OPTION_VALID_ATTRIBUTE_P.  This is used to
   process attribute ((target ("..."))).  */

static bool
aarch64_option_valid_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree old_optimize;
  tree new_target, new_optimize;
  tree existing_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  /* If what we're processing is the current pragma string then the
     target option node is already stored in target_option_current_node
     by aarch64_pragma_target_parse in aarch64-c.cc.  Use that to avoid
     having to re-parse the string.  This is especially useful to keep
     arm_neon.h compile times down since that header contains a lot
     of intrinsics enclosed in pragmas.  */
  if (!existing_target && args == current_target_pragma)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = target_option_current_node;
      return true;
    }
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  old_optimize
    = build_optimization_node (&global_options, &global_options_set);
  func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  /* If the function changed the optimization levels as well as setting
     target options, start with the optimizations specified.  */
  if (func_optimize && func_optimize != old_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
			     TREE_OPTIMIZATION (func_optimize));

  /* Save the current target options to restore at the end.  */
  cl_target_option_save (&cur_target, &global_options, &global_options_set);

  /* If fndecl already has some target attributes applied to it, unpack
     them so that we add this attribute on top of them, rather than
     overwriting them.  */
  if (existing_target)
    {
      struct cl_target_option *existing_options
	= TREE_TARGET_OPTION (existing_target);

      if (existing_options)
	cl_target_option_restore (&global_options, &global_options_set,
				  existing_options);
    }
  else
    cl_target_option_restore (&global_options, &global_options_set,
			      TREE_TARGET_OPTION (target_option_current_node));

  ret = aarch64_process_target_attr (args);
  if (ret)
    {
      tree version_attr = lookup_attribute ("target_version",
					    DECL_ATTRIBUTES (fndecl));
      if (version_attr != NULL_TREE)
	{
	  /* Reapply any target_version attribute after target attribute.
	     This should be equivalent to applying the target_version once
	     after processing all target attributes.  */
	  tree version_args = TREE_VALUE (version_attr);
	  ret = aarch64_process_target_version_attr (version_args);
	}
    }

  /* Set up any additional state.  */
  if (ret)
    {
      aarch64_override_options_internal (&global_options);
      new_target = build_target_option_node (&global_options,
					     &global_options_set);
    }
  else
    new_target = NULL;

  new_optimize = build_optimization_node (&global_options,
					  &global_options_set);

  if (fndecl && ret)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  cl_target_option_restore (&global_options, &global_options_set, &cur_target);

  if (old_optimize != new_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
			     TREE_OPTIMIZATION (old_optimize));
  return ret;
}

typedef unsigned long long aarch64_fmv_feature_mask;

typedef struct
{
  const char *name;
  aarch64_fmv_feature_mask feature_mask;
  aarch64_feature_flags opt_flags;
} aarch64_fmv_feature_datum;

#define AARCH64_FMV_FEATURE(NAME, FEAT_NAME, C) \
  {NAME, 1ULL << FEAT_##FEAT_NAME, ::feature_deps::fmv_deps_##FEAT_NAME},

/* The "rdma" alias uses a different FEAT_NAME to avoid a duplicate
   feature_deps name.  */
#define FEAT_RDMA FEAT_RDM

/* FMV features are listed in priority order, to make it easier to sort target
   strings.  */
static aarch64_fmv_feature_datum aarch64_fmv_feature_data[] = {
#include "config/aarch64/aarch64-option-extensions.def"
};

/* Parse a function multiversioning feature string STR, as found in a
   target_version or target_clones attribute.

   If ISA_FLAGS is nonnull, then update it with the specified architecture
   features turned on.  If FEATURE_MASK is nonnull, then assign to it a bitmask
   representing the set of features explicitly specified in the feature string.
   Return an aarch_parse_opt_result describing the result.

   When the STR string contains an invalid or duplicate extension, a copy of
   the extension string is created and stored to INVALID_EXTENSION.  */

static enum aarch_parse_opt_result
aarch64_parse_fmv_features (const char *str, aarch64_feature_flags *isa_flags,
			    aarch64_fmv_feature_mask *feature_mask,
			    std::string *invalid_extension)
{
  if (feature_mask)
    *feature_mask = 0ULL;

  if (strcmp (str, "default") == 0)
    return AARCH_PARSE_OK;

  while (str != NULL && *str != 0)
    {
      const char *ext;
      size_t len;

      ext = strchr (str, '+');

      if (ext != NULL)
	len = ext - str;
      else
	len = strlen (str);

      if (len == 0)
	return AARCH_PARSE_MISSING_ARG;

      int num_features = ARRAY_SIZE (aarch64_fmv_feature_data);
      int i;
      for (i = 0; i < num_features; i++)
	{
	  if (strlen (aarch64_fmv_feature_data[i].name) == len
	      && strncmp (aarch64_fmv_feature_data[i].name, str, len) == 0)
	    {
	      if (isa_flags)
		*isa_flags |= aarch64_fmv_feature_data[i].opt_flags;
	      if (feature_mask)
		{
		  auto old_feature_mask = *feature_mask;
		  *feature_mask |= aarch64_fmv_feature_data[i].feature_mask;
		  if (*feature_mask == old_feature_mask)
		    {
		      /* Duplicate feature.  */
		      if (invalid_extension)
			*invalid_extension = std::string (str, len);
		      return AARCH_PARSE_DUPLICATE_FEATURE;
		    }
		}
	      break;
	    }
	}

      if (i == num_features)
	{
	  /* Feature not found in list.  */
	  if (invalid_extension)
	    *invalid_extension = std::string (str, len);
	  return AARCH_PARSE_INVALID_FEATURE;
	}

      str = ext;
      if (str)
	/* Skip over the next '+'.  */
	str++;
    }

  return AARCH_PARSE_OK;
}

/* Parse the tree in ARGS that contains the target_version attribute
   information and update the global target options space.  */

static bool
aarch64_process_target_version_attr (tree args)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      if (TREE_CHAIN (args))
	{
	  error ("attribute %<target_version%> has multiple values");
	  return false;
	}
      args = TREE_VALUE (args);
    }

  if (!args || TREE_CODE (args) != STRING_CST)
    {
      error ("attribute %<target_version%> argument not a string");
      return false;
    }

  const char *str = TREE_STRING_POINTER (args);

  enum aarch_parse_opt_result parse_res;
  auto isa_flags = aarch64_asm_isa_flags;

  std::string invalid_extension;
  parse_res = aarch64_parse_fmv_features (str, &isa_flags, NULL,
					  &invalid_extension);

  if (parse_res == AARCH_PARSE_OK)
    {
      aarch64_set_asm_isa_flags (isa_flags);
      return true;
    }

  switch (parse_res)
    {
    case AARCH_PARSE_MISSING_ARG:
      error ("missing value in %<target_version%> attribute");
      break;

    case AARCH_PARSE_INVALID_FEATURE:
      error ("invalid feature modifier %qs of value %qs in "
	     "%<target_version%> attribute", invalid_extension.c_str (),
	     str);
      break;

    case AARCH_PARSE_DUPLICATE_FEATURE:
      error ("duplicate feature modifier %qs of value %qs in "
	     "%<target_version%> attribute", invalid_extension.c_str (),
	     str);
      break;

    default:
      gcc_unreachable ();
    }

  return false;
}

/* Implement TARGET_OPTION_VALID_VERSION_ATTRIBUTE_P.  This is used to
   process attribute ((target_version ("..."))).  */

static bool
aarch64_option_valid_version_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree new_target;
  tree existing_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  /* Save the current target options to restore at the end.  */
  cl_target_option_save (&cur_target, &global_options, &global_options_set);

  /* If fndecl already has some target attributes applied to it, unpack
     them so that we add this attribute on top of them, rather than
     overwriting them.  */
  if (existing_target)
    {
      struct cl_target_option *existing_options
	= TREE_TARGET_OPTION (existing_target);

      if (existing_options)
	cl_target_option_restore (&global_options, &global_options_set,
				  existing_options);
    }
  else
    cl_target_option_restore (&global_options, &global_options_set,
			      TREE_TARGET_OPTION (target_option_current_node));

  ret = aarch64_process_target_version_attr (args);

  /* Set up any additional state.  */
  if (ret)
    {
      aarch64_override_options_internal (&global_options);
      new_target = build_target_option_node (&global_options,
					     &global_options_set);
    }
  else
    new_target = NULL;

  if (fndecl && ret)
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

  cl_target_option_restore (&global_options, &global_options_set, &cur_target);

  return ret;
}

/* This parses the attribute arguments to target_version in DECL and the
   feature mask required to select those targets.  No adjustments are made to
   add or remove redundant feature requirements.  */

static aarch64_fmv_feature_mask
get_feature_mask_for_version (tree decl)
{
  tree version_attr = lookup_attribute ("target_version",
					DECL_ATTRIBUTES (decl));
  if (version_attr == NULL)
    return 0;

  const char *version_string = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE
						    (version_attr)));
  enum aarch_parse_opt_result parse_res;
  aarch64_fmv_feature_mask feature_mask;

  parse_res = aarch64_parse_fmv_features (version_string, NULL, &feature_mask,
					  NULL);

  /* We should have detected any errors before getting here.  */
  gcc_assert (parse_res == AARCH_PARSE_OK);

  return feature_mask;
}

/* Compare priorities of two feature masks. Return:
     1: mask1 is higher priority
    -1: mask2 is higher priority
     0: masks are equal.  */

static int
compare_feature_masks (aarch64_fmv_feature_mask mask1,
		       aarch64_fmv_feature_mask mask2)
{
  int pop1 = popcount_hwi (mask1);
  int pop2 = popcount_hwi (mask2);
  if (pop1 > pop2)
    return 1;
  if (pop2 > pop1)
    return -1;

  auto diff_mask = mask1 ^ mask2;
  if (diff_mask == 0ULL)
    return 0;
  int num_features = ARRAY_SIZE (aarch64_fmv_feature_data);
  for (int i = num_features - 1; i >= 0; i--)
    {
      auto bit_mask = aarch64_fmv_feature_data[i].feature_mask;
      if (diff_mask & bit_mask)
	return (mask1 & bit_mask) ? 1 : -1;
    }
  gcc_unreachable();
}

/* Compare priorities of two version decls.  */

int
aarch64_compare_version_priority (tree decl1, tree decl2)
{
  auto mask1 = get_feature_mask_for_version (decl1);
  auto mask2 = get_feature_mask_for_version (decl2);

  return compare_feature_masks (mask1, mask2);
}

/* Build the struct __ifunc_arg_t type:

   struct __ifunc_arg_t
   {
     unsigned long _size; // Size of the struct, so it can grow.
     unsigned long _hwcap;
     unsigned long _hwcap2;
   }
 */

static tree
build_ifunc_arg_type ()
{
  tree ifunc_arg_type = lang_hooks.types.make_type (RECORD_TYPE);
  tree field1 = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			    get_identifier ("_size"),
			    long_unsigned_type_node);
  tree field2 = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			    get_identifier ("_hwcap"),
			    long_unsigned_type_node);
  tree field3 = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			    get_identifier ("_hwcap2"),
			    long_unsigned_type_node);

  DECL_FIELD_CONTEXT (field1) = ifunc_arg_type;
  DECL_FIELD_CONTEXT (field2) = ifunc_arg_type;
  DECL_FIELD_CONTEXT (field3) = ifunc_arg_type;

  TYPE_FIELDS (ifunc_arg_type) = field1;
  DECL_CHAIN (field1) = field2;
  DECL_CHAIN (field2) = field3;

  layout_type (ifunc_arg_type);

  tree const_type = build_qualified_type (ifunc_arg_type, TYPE_QUAL_CONST);
  tree pointer_type = build_pointer_type (const_type);

  return pointer_type;
}

/* Implement TARGET_MANGLE_DECL_ASSEMBLER_NAME, to add function multiversioning
   suffixes.  */

tree
aarch64_mangle_decl_assembler_name (tree decl, tree id)
{
  /* For function version, add the target suffix to the assembler name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_FUNCTION_VERSIONED (decl))
    {
      aarch64_fmv_feature_mask feature_mask = get_feature_mask_for_version (decl);

      std::string name = IDENTIFIER_POINTER (id);

      /* For the default version, append ".default".  */
      if (feature_mask == 0ULL)
	{
	  name += ".default";
	  return get_identifier (name.c_str());
	}

      name += "._";

      int num_features = ARRAY_SIZE (aarch64_fmv_feature_data);
      for (int i = 0; i < num_features; i++)
	{
	  if (feature_mask & aarch64_fmv_feature_data[i].feature_mask)
	    {
	      name += "M";
	      name += aarch64_fmv_feature_data[i].name;
	    }
	}

      if (DECL_ASSEMBLER_NAME_SET_P (decl))
	SET_DECL_RTL (decl, NULL);

      id = get_identifier (name.c_str());
    }
  return id;
}

/* Return an identifier for the base assembler name of a versioned function.
   This is computed by taking the default version's assembler name, and
   stripping off the ".default" suffix if it's already been appended.  */

static tree
get_suffixed_assembler_name (tree default_decl, const char *suffix)
{
  std::string name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (default_decl));

  auto size = name.size ();
  if (size >= 8 && name.compare (size - 8, 8, ".default") == 0)
    name.resize (size - 8);
  name += suffix;
  return get_identifier (name.c_str());
}

/* Make the resolver function decl to dispatch the versions of
   a multi-versioned function,  DEFAULT_DECL.  IFUNC_ALIAS_DECL is
   ifunc alias that will point to the created resolver.  Create an
   empty basic block in the resolver and store the pointer in
   EMPTY_BB.  Return the decl of the resolver function.  */

static tree
make_resolver_func (const tree default_decl,
		    const tree ifunc_alias_decl,
		    basic_block *empty_bb)
{
  tree decl, type, t;

  /* Create resolver function name based on default_decl.  We need to remove an
     existing ".default" suffix if this has already been appended.  */
  tree decl_name = get_suffixed_assembler_name (default_decl, ".resolver");
  const char *resolver_name = IDENTIFIER_POINTER (decl_name);

  /* The resolver function should have signature
     (void *) resolver (uint64_t, const __ifunc_arg_t *) */
  type = build_function_type_list (ptr_type_node,
				   uint64_type_node,
				   build_ifunc_arg_type (),
				   NULL_TREE);

  decl = build_fn_decl (resolver_name, type);
  SET_DECL_ASSEMBLER_NAME (decl, decl_name);

  DECL_NAME (decl) = decl_name;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;

  /* Resolver is not external, body is generated.  */
  DECL_EXTERNAL (decl) = 0;
  DECL_EXTERNAL (ifunc_alias_decl) = 0;

  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  DECL_STATIC_CONSTRUCTOR (decl) = 0;

  if (DECL_COMDAT_GROUP (default_decl)
      || TREE_PUBLIC (default_decl))
    {
      /* In this case, each translation unit with a call to this
	 versioned function will put out a resolver.  Ensure it
	 is comdat to keep just one copy.  */
      DECL_COMDAT (decl) = 1;
      make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
    }
  else
    TREE_PUBLIC (ifunc_alias_decl) = 0;

  /* Build result decl and add to function_decl. */
  t = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, ptr_type_node);
  DECL_CONTEXT (t) = decl;
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  /* Build parameter decls and add to function_decl. */
  tree arg1 = build_decl (UNKNOWN_LOCATION, PARM_DECL,
			  get_identifier ("hwcap"),
			  uint64_type_node);
  tree arg2 = build_decl (UNKNOWN_LOCATION, PARM_DECL,
			  get_identifier ("arg"),
			  build_ifunc_arg_type());
  DECL_CONTEXT (arg1) = decl;
  DECL_CONTEXT (arg2) = decl;
  DECL_ARTIFICIAL (arg1) = 1;
  DECL_ARTIFICIAL (arg2) = 1;
  DECL_IGNORED_P (arg1) = 1;
  DECL_IGNORED_P (arg2) = 1;
  DECL_ARG_TYPE (arg1) = uint64_type_node;
  DECL_ARG_TYPE (arg2) = build_ifunc_arg_type ();
  DECL_ARGUMENTS (decl) = arg1;
  TREE_CHAIN (arg1) = arg2;

  gimplify_function_tree (decl);
  push_cfun (DECL_STRUCT_FUNCTION (decl));
  *empty_bb = init_lowered_empty_function (decl, false,
					   profile_count::uninitialized ());

  cgraph_node::add_new_function (decl, true);
  symtab->call_cgraph_insertion_hooks (cgraph_node::get_create (decl));

  pop_cfun ();

  gcc_assert (ifunc_alias_decl != NULL);
  /* Mark ifunc_alias_decl as "ifunc" with resolver as resolver_name.  */
  DECL_ATTRIBUTES (ifunc_alias_decl)
    = make_attribute ("ifunc", resolver_name,
		      DECL_ATTRIBUTES (ifunc_alias_decl));

  /* Create the alias for dispatch to resolver here.  */
  cgraph_node::create_same_body_alias (ifunc_alias_decl, decl);
  return decl;
}

/* This adds a condition to the basic_block NEW_BB in function FUNCTION_DECL
   to return a pointer to VERSION_DECL if all feature bits specified in
   FEATURE_MASK are not set in MASK_VAR.  This function will be called during
   version dispatch to decide which function version to execute.  It returns
   the basic block at the end, to which more conditions can be added.  */
static basic_block
add_condition_to_bb (tree function_decl, tree version_decl,
		     aarch64_fmv_feature_mask feature_mask,
		     tree mask_var, basic_block new_bb)
{
  gimple *return_stmt;
  tree convert_expr, result_var;
  gimple *convert_stmt;
  gimple *if_else_stmt;

  basic_block bb1, bb2, bb3;
  edge e12, e23;

  gimple_seq gseq;

  push_cfun (DECL_STRUCT_FUNCTION (function_decl));

  gcc_assert (new_bb != NULL);
  gseq = bb_seq (new_bb);

  convert_expr = build1 (CONVERT_EXPR, ptr_type_node,
			 build_fold_addr_expr (version_decl));
  result_var = create_tmp_var (ptr_type_node);
  convert_stmt = gimple_build_assign (result_var, convert_expr);
  return_stmt = gimple_build_return (result_var);

  if (feature_mask == 0ULL)
    {
      /* Default version.  */
      gimple_seq_add_stmt (&gseq, convert_stmt);
      gimple_seq_add_stmt (&gseq, return_stmt);
      set_bb_seq (new_bb, gseq);
      gimple_set_bb (convert_stmt, new_bb);
      gimple_set_bb (return_stmt, new_bb);
      pop_cfun ();
      return new_bb;
    }

  tree and_expr_var = create_tmp_var (long_long_unsigned_type_node);
  tree and_expr = build2 (BIT_AND_EXPR,
			  long_long_unsigned_type_node,
			  mask_var,
			  build_int_cst (long_long_unsigned_type_node,
					 feature_mask));
  gimple *and_stmt = gimple_build_assign (and_expr_var, and_expr);
  gimple_set_block (and_stmt, DECL_INITIAL (function_decl));
  gimple_set_bb (and_stmt, new_bb);
  gimple_seq_add_stmt (&gseq, and_stmt);

  tree zero_llu = build_int_cst (long_long_unsigned_type_node, 0);
  if_else_stmt = gimple_build_cond (EQ_EXPR, and_expr_var, zero_llu,
				    NULL_TREE, NULL_TREE);
  gimple_set_block (if_else_stmt, DECL_INITIAL (function_decl));
  gimple_set_bb (if_else_stmt, new_bb);
  gimple_seq_add_stmt (&gseq, if_else_stmt);

  gimple_seq_add_stmt (&gseq, convert_stmt);
  gimple_seq_add_stmt (&gseq, return_stmt);
  set_bb_seq (new_bb, gseq);

  bb1 = new_bb;
  e12 = split_block (bb1, if_else_stmt);
  bb2 = e12->dest;
  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_TRUE_VALUE;

  e23 = split_block (bb2, return_stmt);

  gimple_set_bb (convert_stmt, bb2);
  gimple_set_bb (return_stmt, bb2);

  bb3 = e23->dest;
  make_edge (bb1, bb3, EDGE_FALSE_VALUE);

  remove_edge (e23);
  make_edge (bb2, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);

  pop_cfun ();

  return bb3;
}

/* This function generates the dispatch function for
   multi-versioned functions.  DISPATCH_DECL is the function which will
   contain the dispatch logic.  FNDECLS are the function choices for
   dispatch, and is a tree chain.  EMPTY_BB is the basic block pointer
   in DISPATCH_DECL in which the dispatch code is generated.  */

static int
dispatch_function_versions (tree dispatch_decl,
			    void *fndecls_p,
			    basic_block *empty_bb)
{
  gimple *ifunc_cpu_init_stmt;
  gimple_seq gseq;
  vec<tree> *fndecls;

  gcc_assert (dispatch_decl != NULL
	      && fndecls_p != NULL
	      && empty_bb != NULL);

  push_cfun (DECL_STRUCT_FUNCTION (dispatch_decl));

  gseq = bb_seq (*empty_bb);
  /* Function version dispatch is via IFUNC.  IFUNC resolvers fire before
     constructors, so explicity call __init_cpu_features_resolver here.  */
  tree init_fn_type = build_function_type_list (void_type_node,
						long_unsigned_type_node,
						build_ifunc_arg_type(),
						NULL);
  tree init_fn_id = get_identifier ("__init_cpu_features_resolver");
  tree init_fn_decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
				  init_fn_id, init_fn_type);
  DECL_EXTERNAL (init_fn_decl) = 1;
  TREE_PUBLIC (init_fn_decl) = 1;
  DECL_VISIBILITY (init_fn_decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (init_fn_decl) = 1;
  tree arg1 = DECL_ARGUMENTS (dispatch_decl);
  tree arg2 = TREE_CHAIN (arg1);
  ifunc_cpu_init_stmt = gimple_build_call (init_fn_decl, 2, arg1, arg2);
  gimple_seq_add_stmt (&gseq, ifunc_cpu_init_stmt);
  gimple_set_bb (ifunc_cpu_init_stmt, *empty_bb);

  /* Build the struct type for __aarch64_cpu_features.  */
  tree global_type = lang_hooks.types.make_type (RECORD_TYPE);
  tree field1 = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			    get_identifier ("features"),
			    long_long_unsigned_type_node);
  DECL_FIELD_CONTEXT (field1) = global_type;
  TYPE_FIELDS (global_type) = field1;
  layout_type (global_type);

  tree global_var = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				get_identifier ("__aarch64_cpu_features"),
				global_type);
  DECL_EXTERNAL (global_var) = 1;
  TREE_PUBLIC (global_var) = 1;
  DECL_VISIBILITY (global_var) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (global_var) = 1;
  tree mask_var = create_tmp_var (long_long_unsigned_type_node);

  tree component_expr = build3 (COMPONENT_REF, long_long_unsigned_type_node,
				global_var, field1, NULL_TREE);
  gimple *component_stmt = gimple_build_assign (mask_var, component_expr);
  gimple_set_block (component_stmt, DECL_INITIAL (dispatch_decl));
  gimple_set_bb (component_stmt, *empty_bb);
  gimple_seq_add_stmt (&gseq, component_stmt);

  tree not_expr = build1 (BIT_NOT_EXPR, long_long_unsigned_type_node, mask_var);
  gimple *not_stmt = gimple_build_assign (mask_var, not_expr);
  gimple_set_block (not_stmt, DECL_INITIAL (dispatch_decl));
  gimple_set_bb (not_stmt, *empty_bb);
  gimple_seq_add_stmt (&gseq, not_stmt);

  set_bb_seq (*empty_bb, gseq);

  pop_cfun ();

  /* fndecls_p is actually a vector.  */
  fndecls = static_cast<vec<tree> *> (fndecls_p);

  /* At least one more version other than the default.  */
  unsigned int num_versions = fndecls->length ();
  gcc_assert (num_versions >= 2);

  struct function_version_info
    {
      tree version_decl;
      aarch64_fmv_feature_mask feature_mask;
    } *function_versions;

  function_versions = (struct function_version_info *)
    XNEWVEC (struct function_version_info, (num_versions));

  unsigned int actual_versions = 0;

  for (tree version_decl : *fndecls)
    {
      aarch64_fmv_feature_mask feature_mask;
      /* Get attribute string, parse it and find the right features.  */
      feature_mask = get_feature_mask_for_version (version_decl);
      function_versions [actual_versions].version_decl = version_decl;
      function_versions [actual_versions].feature_mask = feature_mask;
      actual_versions++;
    }

  auto compare_feature_version_info = [](const void *p1, const void *p2) {
    const function_version_info v1 = *(const function_version_info *)p1;
    const function_version_info v2 = *(const function_version_info *)p2;
    return - compare_feature_masks (v1.feature_mask, v2.feature_mask);
  };

  /* Sort the versions according to descending order of dispatch priority.  */
  qsort (function_versions, actual_versions,
	 sizeof (struct function_version_info), compare_feature_version_info);

  for (unsigned int i = 0; i < actual_versions; ++i)
    *empty_bb = add_condition_to_bb (dispatch_decl,
				     function_versions[i].version_decl,
				     function_versions[i].feature_mask,
				     mask_var,
				     *empty_bb);

  free (function_versions);
  return 0;
}

/* Implement TARGET_GENERATE_VERSION_DISPATCHER_BODY.  */

tree
aarch64_generate_version_dispatcher_body (void *node_p)
{
  tree resolver_decl;
  basic_block empty_bb;
  tree default_ver_decl;
  struct cgraph_node *versn;
  struct cgraph_node *node;

  struct cgraph_function_version_info *node_version_info = NULL;
  struct cgraph_function_version_info *versn_info = NULL;

  node = (cgraph_node *)node_p;

  node_version_info = node->function_version ();
  gcc_assert (node->dispatcher_function
	      && node_version_info != NULL);

  if (node_version_info->dispatcher_resolver)
    return node_version_info->dispatcher_resolver;

  /* The first version in the chain corresponds to the default version.  */
  default_ver_decl = node_version_info->next->this_node->decl;

  /* node is going to be an alias, so remove the finalized bit.  */
  node->definition = false;

  resolver_decl = make_resolver_func (default_ver_decl,
				      node->decl, &empty_bb);

  node_version_info->dispatcher_resolver = resolver_decl;

  push_cfun (DECL_STRUCT_FUNCTION (resolver_decl));

  auto_vec<tree, 2> fn_ver_vec;

  for (versn_info = node_version_info->next; versn_info;
       versn_info = versn_info->next)
    {
      versn = versn_info->this_node;
      /* Check for virtual functions here again, as by this time it should
	 have been determined if this function needs a vtable index or
	 not.  This happens for methods in derived classes that override
	 virtual methods in base classes but are not explicitly marked as
	 virtual.  */
      if (DECL_VINDEX (versn->decl))
	sorry ("virtual function multiversioning not supported");

      fn_ver_vec.safe_push (versn->decl);
    }

  dispatch_function_versions (resolver_decl, &fn_ver_vec, &empty_bb);
  cgraph_edge::rebuild_edges ();
  pop_cfun ();

  /* Fix up symbol names.  First we need to obtain the base name, which may
     have already been mangled.  */
  tree base_name = get_suffixed_assembler_name (default_ver_decl, "");

  /* We need to redo the version mangling on the non-default versions for the
     target_clones case.  Redoing the mangling for the target_version case is
     redundant but does no harm.  We need to skip the default version, because
     expand_clones will append ".default" later; fortunately that suffix is the
     one we want anyway.  */
  for (versn_info = node_version_info->next->next; versn_info;
       versn_info = versn_info->next)
    {
      tree version_decl = versn_info->this_node->decl;
      tree name = aarch64_mangle_decl_assembler_name (version_decl,
						      base_name);
      symtab->change_decl_assembler_name (version_decl, name);
    }

  /* We also need to use the base name for the ifunc declaration.  */
  symtab->change_decl_assembler_name (node->decl, base_name);

  return resolver_decl;
}

/* Make a dispatcher declaration for the multi-versioned function DECL.
   Calls to DECL function will be replaced with calls to the dispatcher
   by the front-end.  Returns the decl of the dispatcher function.  */

tree
aarch64_get_function_versions_dispatcher (void *decl)
{
  tree fn = (tree) decl;
  struct cgraph_node *node = NULL;
  struct cgraph_node *default_node = NULL;
  struct cgraph_function_version_info *node_v = NULL;
  struct cgraph_function_version_info *first_v = NULL;

  tree dispatch_decl = NULL;

  struct cgraph_function_version_info *default_version_info = NULL;

  gcc_assert (fn != NULL && DECL_FUNCTION_VERSIONED (fn));

  node = cgraph_node::get (fn);
  gcc_assert (node != NULL);

  node_v = node->function_version ();
  gcc_assert (node_v != NULL);

  if (node_v->dispatcher_resolver != NULL)
    return node_v->dispatcher_resolver;

  /* Find the default version and make it the first node.  */
  first_v = node_v;
  /* Go to the beginning of the chain.  */
  while (first_v->prev != NULL)
    first_v = first_v->prev;
  default_version_info = first_v;
  while (default_version_info != NULL)
    {
      if (get_feature_mask_for_version
	    (default_version_info->this_node->decl) == 0ULL)
	break;
      default_version_info = default_version_info->next;
    }

  /* If there is no default node, just return NULL.  */
  if (default_version_info == NULL)
    return NULL;

  /* Make default info the first node.  */
  if (first_v != default_version_info)
    {
      default_version_info->prev->next = default_version_info->next;
      if (default_version_info->next)
	default_version_info->next->prev = default_version_info->prev;
      first_v->prev = default_version_info;
      default_version_info->next = first_v;
      default_version_info->prev = NULL;
    }

  default_node = default_version_info->this_node;

  if (targetm.has_ifunc_p ())
    {
      struct cgraph_function_version_info *it_v = NULL;
      struct cgraph_node *dispatcher_node = NULL;
      struct cgraph_function_version_info *dispatcher_version_info = NULL;

      /* Right now, the dispatching is done via ifunc.  */
      dispatch_decl = make_dispatcher_decl (default_node->decl);
      TREE_NOTHROW (dispatch_decl) = TREE_NOTHROW (fn);

      dispatcher_node = cgraph_node::get_create (dispatch_decl);
      gcc_assert (dispatcher_node != NULL);
      dispatcher_node->dispatcher_function = 1;
      dispatcher_version_info
	= dispatcher_node->insert_new_function_version ();
      dispatcher_version_info->next = default_version_info;
      dispatcher_node->definition = 1;

      /* Set the dispatcher for all the versions.  */
      it_v = default_version_info;
      while (it_v != NULL)
	{
	  it_v->dispatcher_resolver = dispatch_decl;
	  it_v = it_v->next;
	}
    }
  else
    {
      error_at (DECL_SOURCE_LOCATION (default_node->decl),
		"multiversioning needs %<ifunc%> which is not supported "
		"on this target");
    }

  return dispatch_decl;
}

/* This function returns true if FN1 and FN2 are versions of the same function,
   that is, the target_version attributes of the function decls are different.
   This assumes that FN1 and FN2 have the same signature.  */

bool
aarch64_common_function_versions (tree fn1, tree fn2)
{
  if (TREE_CODE (fn1) != FUNCTION_DECL
      || TREE_CODE (fn2) != FUNCTION_DECL)
    return false;

  return (aarch64_compare_version_priority (fn1, fn2) != 0);
}

/* Implement TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P.  Use an opt-out
   rather than an opt-in list.  */

static bool
aarch64_function_attribute_inlinable_p (const_tree fndecl)
{
  /* A function that has local SME state cannot be inlined into its caller,
     since we only support managing PSTATE.ZA switches at function scope.  */
  return (!aarch64_fndecl_has_new_state (fndecl, "za")
	  && !aarch64_fndecl_has_new_state (fndecl, "zt0"));
}

/* Helper for aarch64_can_inline_p.  In the case where CALLER and CALLEE are
   tri-bool options (yes, no, don't care) and the default value is
   DEF, determine whether to reject inlining.  */

static bool
aarch64_tribools_ok_for_inlining_p (int caller, int callee,
				     int dont_care, int def)
{
  /* If the callee doesn't care, always allow inlining.  */
  if (callee == dont_care)
    return true;

  /* If the caller doesn't care, always allow inlining.  */
  if (caller == dont_care)
    return true;

  /* Otherwise, allow inlining if either the callee and caller values
     agree, or if the callee is using the default value.  */
  return (callee == caller || callee == def);
}

/* Bit allocations for ipa_fn_summary::target_info.  */

/* Set if the function contains a stmt that relies on the function's
   choice of PSTATE.SM setting (0 for non-streaming, 1 for streaming).
   Not meaningful for streaming-compatible functions.  */
constexpr auto AARCH64_IPA_SM_FIXED = 1U << 0;

/* Set if the function clobbers ZA and ZT0.  Not meaningful for functions that
   have ZA state.  */
constexpr auto AARCH64_IPA_CLOBBERS_ZA = 1U << 1;
constexpr auto AARCH64_IPA_CLOBBERS_ZT0 = 1U << 2;

/* Implement TARGET_NEED_IPA_FN_TARGET_INFO.  */

static bool
aarch64_need_ipa_fn_target_info (const_tree, unsigned int &)
{
  /* We could in principle skip this for streaming-compatible functions
     that have ZA state, but that's a rare combination.  */
  return true;
}

/* Implement TARGET_UPDATE_IPA_FN_TARGET_INFO.  */

static bool
aarch64_update_ipa_fn_target_info (unsigned int &info, const gimple *stmt)
{
  if (auto *ga = dyn_cast<const gasm *> (stmt))
    {
      /* We don't know what the asm does, so conservatively assume that
	 it requires the function's current SM mode.  */
      info |= AARCH64_IPA_SM_FIXED;
      for (unsigned int i = 0; i < gimple_asm_nclobbers (ga); ++i)
	{
	  tree op = gimple_asm_clobber_op (ga, i);
	  const char *clobber = TREE_STRING_POINTER (TREE_VALUE (op));
	  if (strcmp (clobber, "za") == 0)
	    info |= AARCH64_IPA_CLOBBERS_ZA;
	  if (strcmp (clobber, "zt0") == 0)
	    info |= AARCH64_IPA_CLOBBERS_ZT0;
	}
    }
  if (auto *call = dyn_cast<const gcall *> (stmt))
    {
      if (gimple_call_builtin_p (call, BUILT_IN_MD))
	{
	  /* The attributes on AArch64 builtins are supposed to be accurate.
	     If the function isn't marked streaming-compatible then it
	     needs whichever SM mode it selects.  */
	  tree decl = gimple_call_fndecl (call);
	  if (aarch64_fndecl_pstate_sm (decl) != 0)
	    info |= AARCH64_IPA_SM_FIXED;
	}
    }
  return true;
}

/* Implement TARGET_CAN_INLINE_P.  Decide whether it is valid
   to inline CALLEE into CALLER based on target-specific info.
   Make sure that the caller and callee have compatible architectural
   features.  Then go through the other possible target attributes
   and see if they can block inlining.  Try not to reject always_inline
   callees unless they are incompatible architecturally.  */

static bool
aarch64_can_inline_p (tree caller, tree callee)
{
  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);

  struct cl_target_option *caller_opts
	= TREE_TARGET_OPTION (caller_tree ? caller_tree
					   : target_option_default_node);

  struct cl_target_option *callee_opts
	= TREE_TARGET_OPTION (callee_tree ? callee_tree
					   : target_option_default_node);

  /* Callee's ISA flags should be a subset of the caller's.  */
  auto caller_asm_isa = (aarch64_get_asm_isa_flags (caller_opts)
			 & ~AARCH64_FL_ISA_MODES);
  auto callee_asm_isa = (aarch64_get_asm_isa_flags (callee_opts)
			 & ~AARCH64_FL_ISA_MODES);
  if (callee_asm_isa & ~caller_asm_isa)
    return false;

  auto caller_isa = (aarch64_get_isa_flags (caller_opts)
		     & ~AARCH64_FL_ISA_MODES);
  auto callee_isa = (aarch64_get_isa_flags (callee_opts)
		     & ~AARCH64_FL_ISA_MODES);
  if (callee_isa & ~caller_isa)
    return false;

  /* Return true if the callee might have target_info property PROPERTY.
     The answer must be true unless we have positive proof to the contrary.  */
  auto callee_has_property = [&](unsigned int property)
    {
      if (ipa_fn_summaries)
	if (auto *summary = ipa_fn_summaries->get (cgraph_node::get (callee)))
	  if (!(summary->target_info & property))
	    return false;
      return true;
    };

  /* Streaming-compatible code can be inlined into functions with any
     PSTATE.SM mode.  Otherwise the caller and callee must agree on
     PSTATE.SM mode, unless we can prove that the callee is naturally
     streaming-compatible.  */
  auto caller_sm = (aarch64_get_isa_flags (caller_opts) & AARCH64_FL_SM_STATE);
  auto callee_sm = (aarch64_get_isa_flags (callee_opts) & AARCH64_FL_SM_STATE);
  if (callee_sm
      && caller_sm != callee_sm
      && callee_has_property (AARCH64_IPA_SM_FIXED))
    return false;

  /* aarch64_function_attribute_inlinable_p prevents new-ZA and new-ZT0
     functions from being inlined into others.  We also need to prevent
     inlining of shared-ZA functions into functions without ZA state,
     since this is an error condition.

     The only other problematic case for ZA is inlining a function that
     directly clobbers ZA or ZT0 into a function that has ZA or ZT0 state.  */
  auto caller_za = (aarch64_get_isa_flags (caller_opts) & AARCH64_FL_ZA_ON);
  auto callee_za = (aarch64_get_isa_flags (callee_opts) & AARCH64_FL_ZA_ON);
  if (!caller_za && callee_za)
    return false;
  if (!callee_za
      && aarch64_fndecl_has_state (caller, "za")
      && callee_has_property (AARCH64_IPA_CLOBBERS_ZA))
    return false;
  if (!callee_za
      && aarch64_fndecl_has_state (caller, "zt0")
      && callee_has_property (AARCH64_IPA_CLOBBERS_ZT0))
    return false;

  /* Allow non-strict aligned functions inlining into strict
     aligned ones.  */
  if ((TARGET_STRICT_ALIGN_P (caller_opts->x_target_flags)
       != TARGET_STRICT_ALIGN_P (callee_opts->x_target_flags))
      && !(!TARGET_STRICT_ALIGN_P (callee_opts->x_target_flags)
	   && TARGET_STRICT_ALIGN_P (caller_opts->x_target_flags)))
    return false;

  bool always_inline = lookup_attribute ("always_inline",
					  DECL_ATTRIBUTES (callee));

  /* If the architectural features match up and the callee is always_inline
     then the other attributes don't matter.  */
  if (always_inline)
    return true;

  if (caller_opts->x_aarch64_cmodel_var
      != callee_opts->x_aarch64_cmodel_var)
    return false;

  if (caller_opts->x_aarch64_tls_dialect
      != callee_opts->x_aarch64_tls_dialect)
    return false;

  /* Honour explicit requests to workaround errata.  */
  if (!aarch64_tribools_ok_for_inlining_p (
	  caller_opts->x_aarch64_fix_a53_err835769,
	  callee_opts->x_aarch64_fix_a53_err835769,
	  2, TARGET_FIX_ERR_A53_835769_DEFAULT))
    return false;

  if (!aarch64_tribools_ok_for_inlining_p (
	  caller_opts->x_aarch64_fix_a53_err843419,
	  callee_opts->x_aarch64_fix_a53_err843419,
	  2, TARGET_FIX_ERR_A53_843419))
    return false;

  /* If the user explicitly specified -momit-leaf-frame-pointer for the
     caller and calle and they don't match up, reject inlining.  */
  if (!aarch64_tribools_ok_for_inlining_p (
	  caller_opts->x_flag_omit_leaf_frame_pointer,
	  callee_opts->x_flag_omit_leaf_frame_pointer,
	  2, 1))
    return false;

  /* If the callee has specific tuning overrides, respect them.  */
  if (callee_opts->x_aarch64_override_tune_string != NULL
      && caller_opts->x_aarch64_override_tune_string == NULL)
    return false;

  /* If the user specified tuning override strings for the
     caller and callee and they don't match up, reject inlining.
     We just do a string compare here, we don't analyze the meaning
     of the string, as it would be too costly for little gain.  */
  if (callee_opts->x_aarch64_override_tune_string
      && caller_opts->x_aarch64_override_tune_string
      && (strcmp (callee_opts->x_aarch64_override_tune_string,
		  caller_opts->x_aarch64_override_tune_string) != 0))
    return false;

  return true;
}

/* Return the ID of the TLDESC ABI, initializing the descriptor if hasn't
   been already.  */

arm_pcs
aarch64_tlsdesc_abi_id ()
{
  predefined_function_abi &tlsdesc_abi = function_abis[ARM_PCS_TLSDESC];
  if (!tlsdesc_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers;
      CLEAR_HARD_REG_SET (full_reg_clobbers);
      SET_HARD_REG_BIT (full_reg_clobbers, R0_REGNUM);
      SET_HARD_REG_BIT (full_reg_clobbers, CC_REGNUM);
      for (int regno = P0_REGNUM; regno <= P15_REGNUM; ++regno)
	SET_HARD_REG_BIT (full_reg_clobbers, regno);
      tlsdesc_abi.initialize (ARM_PCS_TLSDESC, full_reg_clobbers);
    }
  return ARM_PCS_TLSDESC;
}

/* Return true if SYMBOL_REF X binds locally.  */

static bool
aarch64_symbol_binds_local_p (const_rtx x)
{
  return (SYMBOL_REF_DECL (x)
	  ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	  : SYMBOL_REF_LOCAL_P (x));
}

/* Return true if SYMBOL_REF X is thread local */
static bool
aarch64_tls_symbol_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  x = strip_salt (x);
  if (!SYMBOL_REF_P (x))
    return false;

  return SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Classify a TLS symbol into one of the TLS kinds.  */
enum aarch64_symbol_type
aarch64_classify_tls_symbol (rtx x)
{
  enum tls_model tls_kind = tls_symbolic_operand_type (x);

  switch (tls_kind)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
    case TLS_MODEL_LOCAL_DYNAMIC:
      return TARGET_TLS_DESC ? SYMBOL_SMALL_TLSDESC : SYMBOL_SMALL_TLSGD;

    case TLS_MODEL_INITIAL_EXEC:
      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_TINY:
	case AARCH64_CMODEL_TINY_PIC:
	  return SYMBOL_TINY_TLSIE;
	default:
	  return SYMBOL_SMALL_TLSIE;
	}

    case TLS_MODEL_LOCAL_EXEC:
      if (aarch64_tls_size == 12)
	return SYMBOL_TLSLE12;
      else if (aarch64_tls_size == 24)
	return SYMBOL_TLSLE24;
      else if (aarch64_tls_size == 32)
	return SYMBOL_TLSLE32;
      else if (aarch64_tls_size == 48)
	return SYMBOL_TLSLE48;
      else
	gcc_unreachable ();

    case TLS_MODEL_EMULATED:
    case TLS_MODEL_NONE:
      return SYMBOL_FORCE_TO_MEM;

    default:
      gcc_unreachable ();
    }
}

/* Return the correct method for accessing X + OFFSET, where X is either
   a SYMBOL_REF or LABEL_REF.  */

enum aarch64_symbol_type
aarch64_classify_symbol (rtx x, HOST_WIDE_INT offset)
{
  x = strip_salt (x);

  if (LABEL_REF_P (x))
    {
      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_LARGE:
	  return SYMBOL_FORCE_TO_MEM;

	case AARCH64_CMODEL_TINY_PIC:
	case AARCH64_CMODEL_TINY:
	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL_SPIC:
	case AARCH64_CMODEL_SMALL_PIC:
	case AARCH64_CMODEL_SMALL:
	  return SYMBOL_SMALL_ABSOLUTE;

	default:
	  gcc_unreachable ();
	}
    }

  if (SYMBOL_REF_P (x))
    {
      if (aarch64_tls_symbol_p (x))
	return aarch64_classify_tls_symbol (x);

      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_TINY_PIC:
	case AARCH64_CMODEL_TINY:
	  /* With -fPIC non-local symbols use the GOT.  For orthogonality
	     always use the GOT for extern weak symbols.  */
	  if (!TARGET_PECOFF
	      && (flag_pic || SYMBOL_REF_WEAK (x))
	      && !aarch64_symbol_binds_local_p (x))
	    return SYMBOL_TINY_GOT;

	  /* When we retrieve symbol + offset address, we have to make sure
	     the offset does not cause overflow of the final address.  But
	     we have no way of knowing the address of symbol at compile time
	     so we can't accurately say if the distance between the PC and
	     symbol + offset is outside the addressible range of +/-1MB in the
	     TINY code model.  So we limit the maximum offset to +/-64KB and
	     assume the offset to the symbol is not larger than +/-(1MB - 64KB).
	     If offset_within_block_p is true we allow larger offsets.  */
	  if (!(IN_RANGE (offset, -0x10000, 0x10000)
		|| offset_within_block_p (x, offset)))
	    return SYMBOL_FORCE_TO_MEM;

	  return SYMBOL_TINY_ABSOLUTE;


	case AARCH64_CMODEL_SMALL_SPIC:
	case AARCH64_CMODEL_SMALL_PIC:
	case AARCH64_CMODEL_SMALL:
	  if (!TARGET_PECOFF
	      && (flag_pic || SYMBOL_REF_WEAK (x))
	      && !aarch64_symbol_binds_local_p (x))
	    return aarch64_cmodel == AARCH64_CMODEL_SMALL_SPIC
		    ? SYMBOL_SMALL_GOT_28K : SYMBOL_SMALL_GOT_4G;

	  /* Same reasoning as the tiny code model, but the offset cap here is
	     1MB, allowing +/-3.9GB for the offset to the symbol.  */
	  if (!(IN_RANGE (offset, -0x100000, 0x100000)
		|| offset_within_block_p (x, offset)))
	    return SYMBOL_FORCE_TO_MEM;

	  return SYMBOL_SMALL_ABSOLUTE;

	case AARCH64_CMODEL_LARGE:
	  /* This is alright even in PIC code as the constant
	     pool reference is always PC relative and within
	     the same translation unit.  */
	  if (!aarch64_pcrelative_literal_loads && CONSTANT_POOL_ADDRESS_P (x))
	    return SYMBOL_SMALL_ABSOLUTE;
	  else
	    return SYMBOL_FORCE_TO_MEM;

	default:
	  gcc_unreachable ();
	}
    }

  /* By default push everything into the constant pool.  */
  return SYMBOL_FORCE_TO_MEM;
}

bool
aarch64_constant_address_p (rtx x)
{
  return (CONSTANT_P (x) && memory_address_p (DImode, x));
}

bool
aarch64_legitimate_pic_operand_p (rtx x)
{
  poly_int64 offset;
  x = strip_offset_and_salt (x, &offset);
  if (SYMBOL_REF_P (x))
    return false;

  return true;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P hook.  Return true for constants
   that should be rematerialized rather than spilled.  */

static bool
aarch64_legitimate_constant_p (machine_mode mode, rtx x)
{
  /* Support CSE and rematerialization of common constants.  */
  if (CONST_INT_P (x)
      || CONST_DOUBLE_P (x))
    return true;

  /* Only accept variable-length vector constants if they can be
     handled directly.

     ??? It would be possible (but complex) to handle rematerialization
     of other constants via secondary reloads.  */
  if (!GET_MODE_SIZE (mode).is_constant ())
    return aarch64_simd_valid_mov_imm (x);

  /* Otherwise, accept any CONST_VECTOR that, if all else fails, can at
     least be forced to memory and loaded from there.  */
  if (CONST_VECTOR_P (x))
    return !targetm.cannot_force_const_mem (mode, x);

  /* Do not allow vector struct mode constants for Advanced SIMD.
     We could support 0 and -1 easily, but they need support in
     aarch64-simd.md.  */
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
    return false;

  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  /* Accept polynomial constants that can be calculated by using the
     destination of a move as the sole temporary.  Constants that
     require a second temporary cannot be rematerialized (they can't be
     forced to memory and also aren't legitimate constants).  */
  poly_int64 offset;
  if (poly_int_rtx_p (x, &offset))
    return aarch64_offset_temporaries (false, offset) <= 1;

  /* If an offset is being added to something else, we need to allow the
     base to be moved into the destination register, meaning that there
     are no free temporaries for the offset.  */
  x = strip_offset_and_salt (x, &offset);
  if (!offset.is_constant () && aarch64_offset_temporaries (true, offset) > 0)
    return false;

  /* Do not allow const (plus (anchor_symbol, const_int)).  */
  if (maybe_ne (offset, 0) && SYMBOL_REF_P (x) && SYMBOL_REF_ANCHOR_P (x))
    return false;

  /* Treat symbols as constants.  Avoid TLS symbols as they are complex,
     so spilling them is better than rematerialization.  */
  if (SYMBOL_REF_P (x) && !SYMBOL_REF_TLS_MODEL (x))
    return true;

  /* Label references are always constant.  */
  if (LABEL_REF_P (x))
    return true;

  return false;
}

rtx
aarch64_load_tp (rtx target)
{
  if (!target
      || GET_MODE (target) != Pmode
      || !register_operand (target, Pmode))
    target = gen_reg_rtx (Pmode);

  /* Can return in any reg.  */
  emit_insn (gen_aarch64_load_tp_hard (target));
  return target;
}

/* On AAPCS systems, this is the "struct __va_list".  */
static GTY(()) tree va_list_type;

/* Implement TARGET_BUILD_BUILTIN_VA_LIST.
   Return the type to use as __builtin_va_list.

   AAPCS64 \S 7.1.4 requires that va_list be a typedef for a type defined as:

   struct __va_list
   {
     void *__stack;
     void *__gr_top;
     void *__vr_top;
     int   __gr_offs;
     int   __vr_offs;
   };  */

static tree
aarch64_build_builtin_va_list (void)
{
  tree va_list_name;
  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;

  /* Create the type.  */
  va_list_type = lang_hooks.types.make_type (RECORD_TYPE);
  /* Give it the required name.  */
  va_list_name = build_decl (BUILTINS_LOCATION,
			     TYPE_DECL,
			     get_identifier ("__va_list"),
			     va_list_type);
  DECL_ARTIFICIAL (va_list_name) = 1;
  TYPE_NAME (va_list_type) = va_list_name;
  TYPE_STUB_DECL (va_list_type) = va_list_name;

  /* Create the fields.  */
  f_stack = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__stack"),
			ptr_type_node);
  f_grtop = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__gr_top"),
			ptr_type_node);
  f_vrtop = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__vr_top"),
			ptr_type_node);
  f_groff = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__gr_offs"),
			integer_type_node);
  f_vroff = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__vr_offs"),
			integer_type_node);

  /* Tell tree-stdarg pass about our internal offset fields.
     NOTE: va_list_gpr/fpr_counter_field are only used for tree comparision
     purpose to identify whether the code is updating va_list internal
     offset fields through irregular way.  */
  va_list_gpr_counter_field = f_groff;
  va_list_fpr_counter_field = f_vroff;

  DECL_ARTIFICIAL (f_stack) = 1;
  DECL_ARTIFICIAL (f_grtop) = 1;
  DECL_ARTIFICIAL (f_vrtop) = 1;
  DECL_ARTIFICIAL (f_groff) = 1;
  DECL_ARTIFICIAL (f_vroff) = 1;

  DECL_FIELD_CONTEXT (f_stack) = va_list_type;
  DECL_FIELD_CONTEXT (f_grtop) = va_list_type;
  DECL_FIELD_CONTEXT (f_vrtop) = va_list_type;
  DECL_FIELD_CONTEXT (f_groff) = va_list_type;
  DECL_FIELD_CONTEXT (f_vroff) = va_list_type;

  TYPE_FIELDS (va_list_type) = f_stack;
  DECL_CHAIN (f_stack) = f_grtop;
  DECL_CHAIN (f_grtop) = f_vrtop;
  DECL_CHAIN (f_vrtop) = f_groff;
  DECL_CHAIN (f_groff) = f_vroff;

  /* Compute its layout.  */
  layout_type (va_list_type);

  return va_list_type;
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */
static void
aarch64_expand_builtin_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  const CUMULATIVE_ARGS *cum;
  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;
  tree stack, grtop, vrtop, groff, vroff;
  tree t;
  int gr_save_area_size = cfun->va_list_gpr_size;
  int vr_save_area_size = cfun->va_list_fpr_size;
  int vr_offset;

  cum = &crtl->args.info;
  if (cfun->va_list_gpr_size)
    gr_save_area_size = MIN ((NUM_ARG_REGS - cum->aapcs_ncrn) * UNITS_PER_WORD,
			     cfun->va_list_gpr_size);
  if (cfun->va_list_fpr_size)
    vr_save_area_size = MIN ((NUM_FP_ARG_REGS - cum->aapcs_nvrn)
			     * UNITS_PER_VREG, cfun->va_list_fpr_size);

  if (!TARGET_FLOAT)
    {
      gcc_assert (cum->aapcs_nvrn == 0);
      vr_save_area_size = 0;
    }

  f_stack = TYPE_FIELDS (va_list_type_node);
  f_grtop = DECL_CHAIN (f_stack);
  f_vrtop = DECL_CHAIN (f_grtop);
  f_groff = DECL_CHAIN (f_vrtop);
  f_vroff = DECL_CHAIN (f_groff);

  stack = build3 (COMPONENT_REF, TREE_TYPE (f_stack), valist, f_stack,
		  NULL_TREE);
  grtop = build3 (COMPONENT_REF, TREE_TYPE (f_grtop), valist, f_grtop,
		  NULL_TREE);
  vrtop = build3 (COMPONENT_REF, TREE_TYPE (f_vrtop), valist, f_vrtop,
		  NULL_TREE);
  groff = build3 (COMPONENT_REF, TREE_TYPE (f_groff), valist, f_groff,
		  NULL_TREE);
  vroff = build3 (COMPONENT_REF, TREE_TYPE (f_vroff), valist, f_vroff,
		  NULL_TREE);

  /* Emit code to initialize STACK, which points to the next varargs stack
     argument.  CUM->AAPCS_STACK_SIZE gives the number of stack words used
     by named arguments.  STACK is 8-byte aligned.  */
  t = make_tree (TREE_TYPE (stack), virtual_incoming_args_rtx);
  if (cum->aapcs_stack_size > 0)
    t = fold_build_pointer_plus_hwi (t, cum->aapcs_stack_size * UNITS_PER_WORD);
  t = build2 (MODIFY_EXPR, TREE_TYPE (stack), stack, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize GRTOP, the top of the GR save area.
     virtual_incoming_args_rtx should have been 16 byte aligned.  */
  t = make_tree (TREE_TYPE (grtop), virtual_incoming_args_rtx);
  t = build2 (MODIFY_EXPR, TREE_TYPE (grtop), grtop, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize VRTOP, the top of the VR save area.
     This address is gr_save_area_bytes below GRTOP, rounded
     down to the next 16-byte boundary.  */
  t = make_tree (TREE_TYPE (vrtop), virtual_incoming_args_rtx);
  vr_offset = ROUND_UP (gr_save_area_size,
			STACK_BOUNDARY / BITS_PER_UNIT);

  if (vr_offset)
    t = fold_build_pointer_plus_hwi (t, -vr_offset);
  t = build2 (MODIFY_EXPR, TREE_TYPE (vrtop), vrtop, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize GROFF, the offset from GRTOP of the
     next GPR argument.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (groff), groff,
	      build_int_cst (TREE_TYPE (groff), -gr_save_area_size));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Likewise emit code to initialize VROFF, the offset from FTOP
     of the next VR argument.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (vroff), vroff,
	      build_int_cst (TREE_TYPE (vroff), -vr_save_area_size));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  */

static tree
aarch64_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			      gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree addr;
  bool indirect_p;
  bool is_ha;		/* is HFA or HVA.  */
  bool dw_align;	/* double-word align.  */
  machine_mode ag_mode = VOIDmode;
  int nregs;
  machine_mode mode;

  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;
  tree stack, f_top, f_off, off, arg, roundup, on_stack;
  HOST_WIDE_INT size, rsize, adjust, align;
  tree t, u, cond1, cond2;

  indirect_p = pass_va_arg_by_reference (type);
  if (indirect_p)
    type = build_pointer_type (type);

  mode = TYPE_MODE (type);

  f_stack = TYPE_FIELDS (va_list_type_node);
  f_grtop = DECL_CHAIN (f_stack);
  f_vrtop = DECL_CHAIN (f_grtop);
  f_groff = DECL_CHAIN (f_vrtop);
  f_vroff = DECL_CHAIN (f_groff);

  stack = build3 (COMPONENT_REF, TREE_TYPE (f_stack), unshare_expr (valist),
		  f_stack, NULL_TREE);
  size = int_size_in_bytes (type);

  unsigned int abi_break_gcc_9;
  unsigned int abi_break_gcc_13;
  unsigned int abi_break_gcc_14;
  align
    = aarch64_function_arg_alignment (mode, type, &abi_break_gcc_9,
				      &abi_break_gcc_13, &abi_break_gcc_14)
    / BITS_PER_UNIT;

  dw_align = false;
  adjust = 0;
  if (aarch64_vfp_is_call_or_return_candidate (mode, type, &ag_mode, &nregs,
					       &is_ha, false))
    {
      /* No frontends can create types with variable-sized modes, so we
	 shouldn't be asked to pass or return them.  */
      unsigned int ag_size = GET_MODE_SIZE (ag_mode).to_constant ();

      /* TYPE passed in fp/simd registers.  */
      if (!TARGET_FLOAT)
	aarch64_err_no_fpadvsimd (mode);

      f_top = build3 (COMPONENT_REF, TREE_TYPE (f_vrtop),
		      unshare_expr (valist), f_vrtop, NULL_TREE);
      f_off = build3 (COMPONENT_REF, TREE_TYPE (f_vroff),
		      unshare_expr (valist), f_vroff, NULL_TREE);

      rsize = nregs * UNITS_PER_VREG;

      if (is_ha)
	{
	  if (BYTES_BIG_ENDIAN && ag_size < UNITS_PER_VREG)
	    adjust = UNITS_PER_VREG - ag_size;
	}
      else if (BLOCK_REG_PADDING (mode, type, 1) == PAD_DOWNWARD
	       && size < UNITS_PER_VREG)
	{
	  adjust = UNITS_PER_VREG - size;
	}
    }
  else
    {
      /* TYPE passed in general registers.  */
      f_top = build3 (COMPONENT_REF, TREE_TYPE (f_grtop),
		      unshare_expr (valist), f_grtop, NULL_TREE);
      f_off = build3 (COMPONENT_REF, TREE_TYPE (f_groff),
		      unshare_expr (valist), f_groff, NULL_TREE);
      rsize = ROUND_UP (size, UNITS_PER_WORD);
      nregs = rsize / UNITS_PER_WORD;

      if (align <= 8
	  && abi_break_gcc_13
	  && warn_psabi
	  && !bitint_or_aggr_of_bitint_p (type))
	inform (input_location, "parameter passing for argument of type "
		"%qT changed in GCC 13.1", type);

      if (warn_psabi
	  && abi_break_gcc_14
	  && (abi_break_gcc_14 > 8 * BITS_PER_UNIT) != (align > 8)
	  && !bitint_or_aggr_of_bitint_p (type))
	inform (input_location, "parameter passing for argument of type "
		"%qT changed in GCC 14.1", type);

      if (align > 8)
	{
	  if (abi_break_gcc_9
	      && warn_psabi
	      && !bitint_or_aggr_of_bitint_p (type))
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 9.1", type);
	  dw_align = true;
	}

      if (BLOCK_REG_PADDING (mode, type, 1) == PAD_DOWNWARD
	  && size < UNITS_PER_WORD)
	{
	  adjust = UNITS_PER_WORD  - size;
	}
    }

  /* Get a local temporary for the field value.  */
  off = get_initialized_tmp_var (f_off, pre_p, NULL);

  /* Emit code to branch if off >= 0.  */
  t = build2 (GE_EXPR, boolean_type_node, off,
	      build_int_cst (TREE_TYPE (off), 0));
  cond1 = build3 (COND_EXPR, ptr_type_node, t, NULL_TREE, NULL_TREE);

  if (dw_align)
    {
      /* Emit: offs = (offs + 15) & -16.  */
      t = build2 (PLUS_EXPR, TREE_TYPE (off), off,
		  build_int_cst (TREE_TYPE (off), 15));
      t = build2 (BIT_AND_EXPR, TREE_TYPE (off), t,
		  build_int_cst (TREE_TYPE (off), -16));
      roundup = build2 (MODIFY_EXPR, TREE_TYPE (off), off, t);
    }
  else
    roundup = NULL;

  /* Update ap.__[g|v]r_offs  */
  t = build2 (PLUS_EXPR, TREE_TYPE (off), off,
	      build_int_cst (TREE_TYPE (off), rsize));
  t = build2 (MODIFY_EXPR, TREE_TYPE (f_off), unshare_expr (f_off), t);

  /* String up.  */
  if (roundup)
    t = build2 (COMPOUND_EXPR, TREE_TYPE (t), roundup, t);

  /* [cond2] if (ap.__[g|v]r_offs > 0)  */
  u = build2 (GT_EXPR, boolean_type_node, unshare_expr (f_off),
	      build_int_cst (TREE_TYPE (f_off), 0));
  cond2 = build3 (COND_EXPR, ptr_type_node, u, NULL_TREE, NULL_TREE);

  /* String up: make sure the assignment happens before the use.  */
  t = build2 (COMPOUND_EXPR, TREE_TYPE (cond2), t, cond2);
  COND_EXPR_ELSE (cond1) = t;

  /* Prepare the trees handling the argument that is passed on the stack;
     the top level node will store in ON_STACK.  */
  arg = get_initialized_tmp_var (stack, pre_p, NULL);
  if (align > 8)
    {
      /* if (alignof(type) > 8) (arg = arg + 15) & -16;  */
      t = fold_build_pointer_plus_hwi (arg, 15);
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), -16));
      roundup = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg, t);
    }
  else
    roundup = NULL;
  /* Advance ap.__stack  */
  t = fold_build_pointer_plus_hwi (arg, size + 7);
  t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
	      build_int_cst (TREE_TYPE (t), -8));
  t = build2 (MODIFY_EXPR, TREE_TYPE (stack), unshare_expr (stack), t);
  /* String up roundup and advance.  */
  if (roundup)
    t = build2 (COMPOUND_EXPR, TREE_TYPE (t), roundup, t);
  /* String up with arg */
  on_stack = build2 (COMPOUND_EXPR, TREE_TYPE (arg), t, arg);
  /* Big-endianness related address adjustment.  */
  if (BLOCK_REG_PADDING (mode, type, 1) == PAD_DOWNWARD
      && size < UNITS_PER_WORD)
  {
    t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (arg), arg,
		size_int (UNITS_PER_WORD - size));
    on_stack = build2 (COMPOUND_EXPR, TREE_TYPE (arg), on_stack, t);
  }

  COND_EXPR_THEN (cond1) = unshare_expr (on_stack);
  COND_EXPR_THEN (cond2) = unshare_expr (on_stack);

  /* Adjustment to OFFSET in the case of BIG_ENDIAN.  */
  t = off;
  if (adjust)
    t = build2 (PREINCREMENT_EXPR, TREE_TYPE (off), off,
		build_int_cst (TREE_TYPE (off), adjust));

  t = fold_convert (sizetype, t);
  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (f_top), f_top, t);

  if (is_ha)
    {
      /* type ha; // treat as "struct {ftype field[n];}"
         ... [computing offs]
         for (i = 0; i <nregs; ++i, offs += 16)
	   ha.field[i] = *((ftype *)(ap.__vr_top + offs));
	 return ha;  */
      int i;
      tree tmp_ha, field_t, field_ptr_t;

      /* Declare a local variable.  */
      tmp_ha = create_tmp_var_raw (type, "ha");
      gimple_add_tmp_var (tmp_ha);

      /* Establish the base type.  */
      switch (ag_mode)
	{
	case E_SFmode:
	  field_t = float_type_node;
	  field_ptr_t = float_ptr_type_node;
	  break;
	case E_DFmode:
	  field_t = double_type_node;
	  field_ptr_t = double_ptr_type_node;
	  break;
	case E_TFmode:
	  field_t = long_double_type_node;
	  field_ptr_t = long_double_ptr_type_node;
	  break;
	case E_SDmode:
	  field_t = dfloat32_type_node;
	  field_ptr_t = build_pointer_type (dfloat32_type_node);
	  break;
	case E_DDmode:
	  field_t = dfloat64_type_node;
	  field_ptr_t = build_pointer_type (dfloat64_type_node);
	  break;
	case E_TDmode:
	  field_t = dfloat128_type_node;
	  field_ptr_t = build_pointer_type (dfloat128_type_node);
	  break;
	case E_HFmode:
	  field_t = aarch64_fp16_type_node;
	  field_ptr_t = aarch64_fp16_ptr_type_node;
	  break;
	case E_BFmode:
	  field_t = bfloat16_type_node;
	  field_ptr_t = aarch64_bf16_ptr_type_node;
	  break;
	case E_V2SImode:
	case E_V4SImode:
	    {
	      tree innertype = make_signed_type (GET_MODE_PRECISION (SImode));
	      field_t = build_vector_type_for_mode (innertype, ag_mode);
	      field_ptr_t = build_pointer_type (field_t);
	    }
	  break;
	default:
	  gcc_assert (0);
	}

      /* *(field_ptr_t)&ha = *((field_ptr_t)vr_saved_area  */
      TREE_ADDRESSABLE (tmp_ha) = 1;
      tmp_ha = build1 (ADDR_EXPR, field_ptr_t, tmp_ha);
      addr = t;
      t = fold_convert (field_ptr_t, addr);
      t = build2 (MODIFY_EXPR, field_t,
		  build1 (INDIRECT_REF, field_t, tmp_ha),
		  build1 (INDIRECT_REF, field_t, t));

      /* ha.field[i] = *((field_ptr_t)vr_saved_area + i)  */
      for (i = 1; i < nregs; ++i)
	{
	  addr = fold_build_pointer_plus_hwi (addr, UNITS_PER_VREG);
	  u = fold_convert (field_ptr_t, addr);
	  u = build2 (MODIFY_EXPR, field_t,
		      build2 (MEM_REF, field_t, tmp_ha,
			      build_int_cst (field_ptr_t,
					     (i *
					      int_size_in_bytes (field_t)))),
		      build1 (INDIRECT_REF, field_t, u));
	  t = build2 (COMPOUND_EXPR, TREE_TYPE (t), t, u);
	}

      u = fold_convert (TREE_TYPE (f_top), tmp_ha);
      t = build2 (COMPOUND_EXPR, TREE_TYPE (f_top), t, u);
    }

  COND_EXPR_ELSE (cond2) = t;
  addr = fold_convert (build_pointer_type (type), cond1);
  addr = build_va_arg_indirect_ref (addr);

  if (indirect_p)
    addr = build_va_arg_indirect_ref (addr);

  return addr;
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
aarch64_setup_incoming_varargs (cumulative_args_t cum_v,
				const function_arg_info &arg,
				int *pretend_size ATTRIBUTE_UNUSED, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  CUMULATIVE_ARGS local_cum;
  int gr_saved = cfun->va_list_gpr_size;
  int vr_saved = cfun->va_list_fpr_size;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  local_cum = *cum;
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl)))
    aarch64_function_arg_advance (pack_cumulative_args(&local_cum), arg);

  /* Found out how many registers we need to save.
     Honor tree-stdvar analysis results.  */
  if (cfun->va_list_gpr_size)
    gr_saved = MIN (NUM_ARG_REGS - local_cum.aapcs_ncrn,
		    cfun->va_list_gpr_size / UNITS_PER_WORD);
  if (cfun->va_list_fpr_size)
    vr_saved = MIN (NUM_FP_ARG_REGS - local_cum.aapcs_nvrn,
		    cfun->va_list_fpr_size / UNITS_PER_VREG);

  if (!TARGET_FLOAT)
    {
      gcc_assert (local_cum.aapcs_nvrn == 0);
      vr_saved = 0;
    }

  if (!no_rtl)
    {
      if (gr_saved > 0)
	{
	  rtx ptr, mem;

	  /* virtual_incoming_args_rtx should have been 16-byte aligned.  */
	  ptr = plus_constant (Pmode, virtual_incoming_args_rtx,
			       - gr_saved * UNITS_PER_WORD);
	  mem = gen_frame_mem (BLKmode, ptr);
	  set_mem_alias_set (mem, get_varargs_alias_set ());

	  move_block_from_reg (local_cum.aapcs_ncrn + R0_REGNUM,
			       mem, gr_saved);
	}
      if (vr_saved > 0)
	{
	  /* We can't use move_block_from_reg, because it will use
	     the wrong mode, storing D regs only.  */
	  machine_mode mode = TImode;
	  int off, i, vr_start;

	  /* Set OFF to the offset from virtual_incoming_args_rtx of
	     the first vector register.  The VR save area lies below
	     the GR one, and is aligned to 16 bytes.  */
	  off = -ROUND_UP (gr_saved * UNITS_PER_WORD,
			   STACK_BOUNDARY / BITS_PER_UNIT);
	  off -= vr_saved * UNITS_PER_VREG;

	  vr_start = V0_REGNUM + local_cum.aapcs_nvrn;
	  for (i = 0; i < vr_saved; ++i)
	    {
	      rtx ptr, mem;

	      ptr = plus_constant (Pmode, virtual_incoming_args_rtx, off);
	      mem = gen_frame_mem (mode, ptr);
	      set_mem_alias_set (mem, get_varargs_alias_set ());
	      aarch64_emit_move (mem, gen_rtx_REG (mode, vr_start + i));
	      off += UNITS_PER_VREG;
	    }
	}
    }

  /* We don't save the size into *PRETEND_SIZE because we want to avoid
     any complication of having crtl->args.pretend_args_size changed.  */
  cfun->machine->frame.saved_varargs_size
    = (ROUND_UP (gr_saved * UNITS_PER_WORD,
		 STACK_BOUNDARY / BITS_PER_UNIT)
       + vr_saved * UNITS_PER_VREG);
}

static void
aarch64_conditional_register_usage (void)
{
  int i;
  if (!TARGET_FLOAT)
    {
      for (i = V0_REGNUM; i <= V31_REGNUM; i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	  CLEAR_HARD_REG_BIT (operand_reg_set, i);
	}
    }
  if (!TARGET_SVE)
    for (i = P0_REGNUM; i <= P15_REGNUM; i++)
      {
	fixed_regs[i] = 1;
	call_used_regs[i] = 1;
      }

  /* Only allow these registers to be accessed via special patterns.  */
  CLEAR_HARD_REG_BIT (operand_reg_set, VG_REGNUM);
  CLEAR_HARD_REG_BIT (operand_reg_set, FFR_REGNUM);
  CLEAR_HARD_REG_BIT (operand_reg_set, FFRT_REGNUM);
  for (int i = FIRST_FAKE_REGNUM; i <= LAST_FAKE_REGNUM; ++i)
    CLEAR_HARD_REG_BIT (operand_reg_set, i);

  /* When tracking speculation, we need a couple of call-clobbered registers
     to track the speculation state.  It would be nice to just use
     IP0 and IP1, but currently there are numerous places that just
     assume these registers are free for other uses (eg pointer
     authentication).  */
  if (aarch64_track_speculation)
    {
      fixed_regs[SPECULATION_TRACKER_REGNUM] = 1;
      call_used_regs[SPECULATION_TRACKER_REGNUM] = 1;
      fixed_regs[SPECULATION_SCRATCH_REGNUM] = 1;
      call_used_regs[SPECULATION_SCRATCH_REGNUM] = 1;
    }
}

/* Implement TARGET_MEMBER_TYPE_FORCES_BLK.  */

bool
aarch64_member_type_forces_blk (const_tree field_or_array, machine_mode mode)
{
  /* For records we're passed a FIELD_DECL, for arrays we're passed
     an ARRAY_TYPE.  In both cases we're interested in the TREE_TYPE.  */
  const_tree type = TREE_TYPE (field_or_array);

  /* Assign BLKmode to anything that contains more than 2 SVE predicates.
     For structures, the "multiple" case is indicated by MODE being
     VOIDmode.  */
  unsigned int num_zr, num_pr;
  if (aarch64_sve::builtin_type_p (type, &num_zr, &num_pr) && num_pr > 2)
    {
      if (TREE_CODE (field_or_array) == ARRAY_TYPE)
	return !simple_cst_equal (TYPE_SIZE (field_or_array),
				  TYPE_SIZE (type));
      return mode == VOIDmode;
    }

  return default_member_type_forces_blk (field_or_array, mode);
}

/* Bitmasks that indicate whether earlier versions of GCC would have
   taken a different path through the ABI logic.  This should result in
   a -Wpsabi warning if the earlier path led to a different ABI decision.

   WARN_PSABI_EMPTY_CXX17_BASE
      Indicates that the type includes an artificial empty C++17 base field
      that, prior to GCC 10.1, would prevent the type from being treated as
      a HFA or HVA.  See PR94383 for details.

   WARN_PSABI_NO_UNIQUE_ADDRESS
      Indicates that the type includes an empty [[no_unique_address]] field
      that, prior to GCC 10.1, would prevent the type from being treated as
      a HFA or HVA.  */
const unsigned int WARN_PSABI_EMPTY_CXX17_BASE = 1U << 0;
const unsigned int WARN_PSABI_NO_UNIQUE_ADDRESS = 1U << 1;
const unsigned int WARN_PSABI_ZERO_WIDTH_BITFIELD = 1U << 2;

/* Walk down the type tree of TYPE counting consecutive base elements.
   If *MODEP is VOIDmode, then set it to the first valid floating point
   type.  If a non-floating point type is found, or if a floating point
   type that doesn't match a non-VOIDmode *MODEP is found, then return -1,
   otherwise return the count in the sub-tree.

   The WARN_PSABI_FLAGS argument allows the caller to check whether this
   function has changed its behavior relative to earlier versions of GCC.
   Normally the argument should be nonnull and point to a zero-initialized
   variable.  The function then records whether the ABI decision might
   be affected by a known fix to the ABI logic, setting the associated
   WARN_PSABI_* bits if so.

   When the argument is instead a null pointer, the function tries to
   simulate the behavior of GCC before all such ABI fixes were made.
   This is useful to check whether the function returns something
   different after the ABI fixes.  */
static int
aapcs_vfp_sub_candidate (const_tree type, machine_mode *modep,
			 unsigned int *warn_psabi_flags)
{
  machine_mode mode;
  HOST_WIDE_INT size;

  if (aarch64_sve::builtin_type_p (type))
    return -1;

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      mode = TYPE_MODE (type);
      if (mode != DFmode && mode != SFmode
	  && mode != TFmode && mode != HFmode
	  && mode != SDmode && mode != DDmode && mode != TDmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 1;

      break;

    case COMPLEX_TYPE:
      mode = TYPE_MODE (TREE_TYPE (type));
      if (mode != DFmode && mode != SFmode
	  && mode != TFmode && mode != HFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 2;

      break;

    case VECTOR_TYPE:
      /* Use V2SImode and V4SImode as representatives of all 64-bit
	 and 128-bit vector types.  */
      size = int_size_in_bytes (type);
      switch (size)
	{
	case 8:
	  mode = V2SImode;
	  break;
	case 16:
	  mode = V4SImode;
	  break;
	default:
	  return -1;
	}

      if (*modep == VOIDmode)
	*modep = mode;

      /* Vector modes are considered to be opaque: two vectors are
	 equivalent for the purposes of being homogeneous aggregates
	 if they are the same size.  */
      if (*modep == mode)
	return 1;

      break;

    case ARRAY_TYPE:
      {
	int count;
	tree index = TYPE_DOMAIN (type);

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	count = aapcs_vfp_sub_candidate (TREE_TYPE (type), modep,
					 warn_psabi_flags);
	if (count == -1
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || count < 0)
	  return -1;

	count *= (1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		      - tree_to_uhwi (TYPE_MIN_VALUE (index)));

	/* There must be no padding.  */
	if (maybe_ne (wi::to_poly_wide (TYPE_SIZE (type)),
		      count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case RECORD_TYPE:
      {
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    if (DECL_FIELD_ABI_IGNORED (field))
	      {
		/* See whether this is something that earlier versions of
		   GCC failed to ignore.  */
		unsigned int flag;
		if (lookup_attribute ("no_unique_address",
				      DECL_ATTRIBUTES (field)))
		  flag = WARN_PSABI_NO_UNIQUE_ADDRESS;
		else if (cxx17_empty_base_field_p (field))
		  flag = WARN_PSABI_EMPTY_CXX17_BASE;
		else
		  /* No compatibility problem.  */
		  continue;

		/* Simulate the old behavior when WARN_PSABI_FLAGS is null.  */
		if (warn_psabi_flags)
		  {
		    *warn_psabi_flags |= flag;
		    continue;
		  }
	      }
	    /* A zero-width bitfield may affect layout in some
	       circumstances, but adds no members.  The determination
	       of whether or not a type is an HFA is performed after
	       layout is complete, so if the type still looks like an
	       HFA afterwards, it is still classed as one.  This is
	       potentially an ABI break for the hard-float ABI.  */
	    else if (DECL_BIT_FIELD (field)
		     && integer_zerop (DECL_SIZE (field)))
	      {
		/* Prior to GCC-12 these fields were striped early,
		   hiding them from the back-end entirely and
		   resulting in the correct behaviour for argument
		   passing.  Simulate that old behaviour without
		   generating a warning.  */
		if (DECL_FIELD_CXX_ZERO_WIDTH_BIT_FIELD (field))
		  continue;
		if (warn_psabi_flags)
		  {
		    *warn_psabi_flags |= WARN_PSABI_ZERO_WIDTH_BITFIELD;
		    continue;
		  }
	      }

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep,
						 warn_psabi_flags);
	    if (sub_count < 0)
	      return -1;
	    count += sub_count;
	  }

	/* There must be no padding.  */
	if (maybe_ne (wi::to_poly_wide (TYPE_SIZE (type)),
		      count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* These aren't very interesting except in a degenerate case.  */
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep,
						 warn_psabi_flags);
	    if (sub_count < 0)
	      return -1;
	    count = count > sub_count ? count : sub_count;
	  }

	/* There must be no padding.  */
	if (maybe_ne (wi::to_poly_wide (TYPE_SIZE (type)),
		      count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    default:
      break;
    }

  return -1;
}

/* Return TRUE if the type, as described by TYPE and MODE, is a short vector
   type as described in AAPCS64 \S 4.1.2.

   See the comment above aarch64_composite_type_p for the notes on MODE.  */

static bool
aarch64_short_vector_p (const_tree type,
			machine_mode mode)
{
  poly_int64 size = -1;

  if (type && VECTOR_TYPE_P (type))
    {
      if (aarch64_sve::builtin_type_p (type))
	return false;
      size = int_size_in_bytes (type);
    }
  else if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	   || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
    {
      /* The containing "else if" is too loose: it means that we look at TYPE
	 if the type is a vector type (good), but that we otherwise ignore TYPE
	 and look only at the mode.  This is wrong because the type describes
	 the language-level information whereas the mode is purely an internal
	 GCC concept.  We can therefore reach here for types that are not
	 vectors in the AAPCS64 sense.

	 We can't "fix" that for the traditional Advanced SIMD vector modes
	 without breaking backwards compatibility.  However, there's no such
	 baggage for the structure modes, which were introduced in GCC 12.  */
      if (aarch64_advsimd_struct_mode_p (mode))
	return false;

      /* For similar reasons, rely only on the type, not the mode, when
	 processing SVE types.  */
      if (type && aarch64_some_values_include_pst_objects_p (type))
	/* Leave later code to report an error if SVE is disabled.  */
	gcc_assert (!TARGET_SVE || aarch64_sve_mode_p (mode));
      else
	size = GET_MODE_SIZE (mode);
    }
  if (known_eq (size, 8) || known_eq (size, 16))
    {
      /* 64-bit and 128-bit vectors should only acquire an SVE mode if
	 they are being treated as scalable AAPCS64 types.  */
      gcc_assert (!aarch64_sve_mode_p (mode)
		  && !aarch64_advsimd_struct_mode_p (mode));
      return true;
    }
  return false;
}

/* Return TRUE if the type, as described by TYPE and MODE, is a composite
   type as described in AAPCS64 \S 4.3.  This includes aggregate, union and
   array types.  The C99 floating-point complex types are also considered
   as composite types, according to AAPCS64 \S 7.1.1.  The complex integer
   types, which are GCC extensions and out of the scope of AAPCS64, are
   treated as composite types here as well.

   Note that MODE itself is not sufficient in determining whether a type
   is such a composite type or not.  This is because
   stor-layout.cc:compute_record_mode may have already changed the MODE
   (BLKmode) of a RECORD_TYPE TYPE to some other mode.  For example, a
   structure with only one field may have its MODE set to the mode of the
   field.  Also an integer mode whose size matches the size of the
   RECORD_TYPE type may be used to substitute the original mode
   (i.e. BLKmode) in certain circumstances.  In other words, MODE cannot be
   solely relied on.  */

static bool
aarch64_composite_type_p (const_tree type,
			  machine_mode mode)
{
  if (aarch64_short_vector_p (type, mode))
    return false;

  if (type && (AGGREGATE_TYPE_P (type) || TREE_CODE (type) == COMPLEX_TYPE))
    return true;

  if (type
      && TREE_CODE (type) == BITINT_TYPE
      && int_size_in_bytes (type) > 16)
    return true;

  if (mode == BLKmode
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    return true;

  return false;
}

/* Return TRUE if an argument, whose type is described by TYPE and MODE,
   shall be passed or returned in simd/fp register(s) (providing these
   parameter passing registers are available).

   Upon successful return, *COUNT returns the number of needed registers,
   *BASE_MODE returns the mode of the individual register and when IS_HA
   is not NULL, *IS_HA indicates whether or not the argument is a homogeneous
   floating-point aggregate or a homogeneous short-vector aggregate.

   SILENT_P is true if the function should refrain from reporting any
   diagnostics.  This should only be used if the caller is certain that
   any ABI decisions would eventually come through this function with
   SILENT_P set to false.  */

static bool
aarch64_vfp_is_call_or_return_candidate (machine_mode mode,
					 const_tree type,
					 machine_mode *base_mode,
					 int *count,
					 bool *is_ha,
					 bool silent_p)
{
  if (is_ha != NULL) *is_ha = false;

  machine_mode new_mode = VOIDmode;
  bool composite_p = aarch64_composite_type_p (type, mode);

  if ((!composite_p
       && (GET_MODE_CLASS (mode) == MODE_FLOAT
	   || GET_MODE_CLASS (mode) == MODE_DECIMAL_FLOAT
	   || (type && TYPE_MAIN_VARIANT (type) == aarch64_mfp8_type_node)))
      || aarch64_short_vector_p (type, mode))
    {
      *count = 1;
      new_mode = mode;
    }
  else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    {
      if (is_ha != NULL) *is_ha = true;
      *count = 2;
      new_mode = GET_MODE_INNER (mode);
    }
  else if (type && composite_p)
    {
      unsigned int warn_psabi_flags = 0;
      int ag_count = aapcs_vfp_sub_candidate (type, &new_mode,
					      &warn_psabi_flags);
      if (ag_count > 0 && ag_count <= HA_MAX_NUM_FLDS)
	{
	  static unsigned last_reported_type_uid;
	  unsigned uid = TYPE_UID (TYPE_MAIN_VARIANT (type));
	  int alt;
	  if (!silent_p
	      && warn_psabi
	      && warn_psabi_flags
	      && uid != last_reported_type_uid
	      && ((alt = aapcs_vfp_sub_candidate (type, &new_mode, NULL))
		  != ag_count))
	    {
	      const char *url10
		= CHANGES_ROOT_URL "gcc-10/changes.html#empty_base";
	      const char *url12
		= CHANGES_ROOT_URL "gcc-12/changes.html#zero_width_bitfields";
	      gcc_assert (alt == -1);
	      last_reported_type_uid = uid;
	      /* Use TYPE_MAIN_VARIANT to strip any redundant const
		 qualification.  */
	      if (warn_psabi_flags & WARN_PSABI_NO_UNIQUE_ADDRESS)
		inform (input_location, "parameter passing for argument of "
			"type %qT with %<[[no_unique_address]]%> members "
			"changed %{in GCC 10.1%}",
			TYPE_MAIN_VARIANT (type), url10);
	      else if (warn_psabi_flags & WARN_PSABI_EMPTY_CXX17_BASE)
		inform (input_location, "parameter passing for argument of "
			"type %qT when C++17 is enabled changed to match "
			"C++14 %{in GCC 10.1%}",
			TYPE_MAIN_VARIANT (type), url10);
	      else if (warn_psabi_flags & WARN_PSABI_ZERO_WIDTH_BITFIELD)
		inform (input_location, "parameter passing for argument of "
			"type %qT changed %{in GCC 12.1%}",
			TYPE_MAIN_VARIANT (type), url12);
	    }

	  if (is_ha != NULL) *is_ha = true;
	  *count = ag_count;
	}
      else
	return false;
    }
  else
    return false;

  gcc_assert (!aarch64_sve_mode_p (new_mode));
  *base_mode = new_mode;
  return true;
}

/* Implement TARGET_STRUCT_VALUE_RTX.  */

static rtx
aarch64_struct_value_rtx (tree fndecl ATTRIBUTE_UNUSED,
			  int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, AARCH64_STRUCT_VALUE_REGNUM);
}

/* Implements target hook vector_mode_supported_p.  */
static bool
aarch64_vector_mode_supported_p (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  return vec_flags != 0 && (vec_flags & VEC_STRUCT) == 0;
}

/* Implements target hook vector_mode_supported_any_target_p.  */
static bool
aarch64_vector_mode_supported_any_target_p (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode, true);
  return vec_flags != 0 && (vec_flags & VEC_STRUCT) == 0;
}

/* Return the full-width SVE vector mode for element mode MODE, if one
   exists.  */
opt_machine_mode
aarch64_full_sve_mode (scalar_mode mode)
{
  switch (mode)
    {
    case E_DFmode:
      return VNx2DFmode;
    case E_SFmode:
      return VNx4SFmode;
    case E_HFmode:
      return VNx8HFmode;
    case E_BFmode:
      return VNx8BFmode;
    case E_DImode:
      return VNx2DImode;
    case E_SImode:
      return VNx4SImode;
    case E_HImode:
      return VNx8HImode;
    case E_QImode:
      return VNx16QImode;
    default:
      return opt_machine_mode ();
    }
}

/* Return the 128-bit Advanced SIMD vector mode for element mode MODE,
   if it exists.  */
opt_machine_mode
aarch64_vq_mode (scalar_mode mode)
{
  switch (mode)
    {
    case E_DFmode:
      return V2DFmode;
    case E_SFmode:
      return V4SFmode;
    case E_HFmode:
      return V8HFmode;
    case E_BFmode:
      return V8BFmode;
    case E_SImode:
      return V4SImode;
    case E_HImode:
      return V8HImode;
    case E_QImode:
      return V16QImode;
    case E_DImode:
      return V2DImode;
    default:
      return opt_machine_mode ();
    }
}

/* Return appropriate SIMD container
   for MODE within a vector of WIDTH bits.  */
static machine_mode
aarch64_simd_container_mode (scalar_mode mode, poly_int64 width)
{
  if (TARGET_SVE
      && maybe_ne (width, 128)
      && known_eq (width, BITS_PER_SVE_VECTOR))
    return aarch64_full_sve_mode (mode).else_mode (word_mode);

  gcc_assert (known_eq (width, 64) || known_eq (width, 128));
  if (TARGET_BASE_SIMD)
    {
      if (known_eq (width, 128))
	return aarch64_vq_mode (mode).else_mode (word_mode);
      else
	switch (mode)
	  {
	  case E_SFmode:
	    return V2SFmode;
	  case E_HFmode:
	    return V4HFmode;
	  case E_BFmode:
	    return V4BFmode;
	  case E_SImode:
	    return V2SImode;
	  case E_HImode:
	    return V4HImode;
	  case E_QImode:
	    return V8QImode;
	  default:
	    break;
	  }
    }
  return word_mode;
}

/* Compare an SVE mode SVE_M and an Advanced SIMD mode ASIMD_M
   and return whether the SVE mode should be preferred over the
   Advanced SIMD one in aarch64_autovectorize_vector_modes.  */
static bool
aarch64_cmp_autovec_modes (machine_mode sve_m, machine_mode asimd_m)
{
  /* Take into account the aarch64-autovec-preference param if non-zero.  */
  bool only_asimd_p = aarch64_autovec_preference == AARCH64_AUTOVEC_ASIMD_ONLY;
  bool only_sve_p = aarch64_autovec_preference == AARCH64_AUTOVEC_SVE_ONLY;

  if (only_asimd_p)
    return false;
  if (only_sve_p)
    return true;

  /* The preference in case of a tie in costs.  */
  bool prefer_asimd = aarch64_autovec_preference == AARCH64_AUTOVEC_PREFER_ASIMD;
  bool prefer_sve = aarch64_autovec_preference == AARCH64_AUTOVEC_PREFER_SVE;

  poly_int64 nunits_sve = GET_MODE_NUNITS (sve_m);
  poly_int64 nunits_asimd = GET_MODE_NUNITS (asimd_m);
  /* If the CPU information does not have an SVE width registered use the
     generic poly_int comparison that prefers SVE.  If a preference is
     explicitly requested avoid this path.  */
  if (aarch64_tune_params.sve_width == SVE_SCALABLE
      && !prefer_asimd
      && !prefer_sve)
    return maybe_gt (nunits_sve, nunits_asimd);

  /* Otherwise estimate the runtime width of the modes involved.  */
  HOST_WIDE_INT est_sve = estimated_poly_value (nunits_sve);
  HOST_WIDE_INT est_asimd = estimated_poly_value (nunits_asimd);

  /* Preferring SVE means picking it first unless the Advanced SIMD mode
     is clearly wider.  */
  if (prefer_sve)
    return est_sve >= est_asimd;
  /* Conversely, preferring Advanced SIMD means picking SVE only if SVE
     is clearly wider.  */
  if (prefer_asimd)
    return est_sve > est_asimd;

  /* In the default case prefer Advanced SIMD over SVE in case of a tie.  */
  return est_sve > est_asimd;
}

/* Return 128-bit container as the preferred SIMD mode for MODE.  */
static machine_mode
aarch64_preferred_simd_mode (scalar_mode mode)
{
  /* Take into account explicit auto-vectorization ISA preferences through
     aarch64_cmp_autovec_modes.  */
  if (TARGET_SVE && aarch64_cmp_autovec_modes (VNx16QImode, V16QImode))
    return aarch64_full_sve_mode (mode).else_mode (word_mode);
  if (TARGET_SIMD)
    return aarch64_vq_mode (mode).else_mode (word_mode);
  return word_mode;
}

/* Return a list of possible vector sizes for the vectorizer
   to iterate over.  */
static unsigned int
aarch64_autovectorize_vector_modes (vector_modes *modes, bool)
{
  static const machine_mode sve_modes[] = {
    /* Try using full vectors for all element types.  */
    VNx16QImode,

    /* Try using 16-bit containers for 8-bit elements and full vectors
       for wider elements.  */
    VNx8QImode,

    /* Try using 32-bit containers for 8-bit and 16-bit elements and
       full vectors for wider elements.  */
    VNx4QImode,

    /* Try using 64-bit containers for all element types.  */
    VNx2QImode
  };

  static const machine_mode advsimd_modes[] = {
    /* Try using 128-bit vectors for all element types.  */
    V16QImode,

    /* Try using 64-bit vectors for 8-bit elements and 128-bit vectors
       for wider elements.  */
    V8QImode,

    /* Try using 64-bit vectors for 16-bit elements and 128-bit vectors
       for wider elements.

       TODO: We could support a limited form of V4QImode too, so that
       we use 32-bit vectors for 8-bit elements.  */
    V4HImode,

    /* Try using 64-bit vectors for 32-bit elements and 128-bit vectors
       for 64-bit elements.

       TODO: We could similarly support limited forms of V2QImode and V2HImode
       for this case.  */
    V2SImode
  };

  /* Try using N-byte SVE modes only after trying N-byte Advanced SIMD mode.
     This is because:

     - If we can't use N-byte Advanced SIMD vectors then the placement
       doesn't matter; we'll just continue as though the Advanced SIMD
       entry didn't exist.

     - If an SVE main loop with N bytes ends up being cheaper than an
       Advanced SIMD main loop with N bytes then by default we'll replace
       the Advanced SIMD version with the SVE one.

     - If an Advanced SIMD main loop with N bytes ends up being cheaper
       than an SVE main loop with N bytes then by default we'll try to
       use the SVE loop to vectorize the epilogue instead.  */

  bool only_asimd_p = aarch64_autovec_preference == AARCH64_AUTOVEC_ASIMD_ONLY;
  bool only_sve_p = aarch64_autovec_preference == AARCH64_AUTOVEC_SVE_ONLY;

  unsigned int sve_i = (TARGET_SVE && !only_asimd_p) ? 0 : ARRAY_SIZE (sve_modes);
  unsigned int advsimd_i = 0;

  while (!only_sve_p && advsimd_i < ARRAY_SIZE (advsimd_modes))
    {
      if (sve_i < ARRAY_SIZE (sve_modes)
	  && aarch64_cmp_autovec_modes (sve_modes[sve_i],
					advsimd_modes[advsimd_i]))
	modes->safe_push (sve_modes[sve_i++]);
      else
	modes->safe_push (advsimd_modes[advsimd_i++]);
    }
  while (sve_i < ARRAY_SIZE (sve_modes))
   modes->safe_push (sve_modes[sve_i++]);

  unsigned int flags = 0;
  if (aarch64_vect_compare_costs)
    flags |= VECT_COMPARE_COSTS;
  return flags;
}

/* Implement TARGET_MANGLE_TYPE.  */

static const char *
aarch64_mangle_type (const_tree type)
{
  /* The AArch64 ABI documents say that "__va_list" has to be
     mangled as if it is in the "std" namespace.  */
  if (lang_hooks.types_compatible_p (CONST_CAST_TREE (type), va_list_type))
    return "St9__va_list";

  /* Half-precision floating point types.  */
  if (SCALAR_FLOAT_TYPE_P (type) && TYPE_PRECISION (type) == 16)
    {
      if (TYPE_MAIN_VARIANT (type) == float16_type_node)
	return NULL;
      if (TYPE_MODE (type) == BFmode)
	return "u6__bf16";
      else
	return "Dh";
    }

  /* Modal 8 bit floating point types.  */
  if (TYPE_MAIN_VARIANT (type) == aarch64_mfp8_type_node)
    return "u6__mfp8";

  /* Mangle AArch64-specific internal types.  TYPE_NAME is non-NULL_TREE for
     builtin types.  */
  if (TYPE_NAME (type) != NULL)
    {
      const char *res;
      if ((res = aarch64_general_mangle_builtin_type (type))
	  || (res = aarch64_sve::mangle_builtin_type (type)))
	return res;
    }

  /* Use the default mangling.  */
  return NULL;
}

/* Implement TARGET_INVALID_CONVERSION.  */

static const char *
aarch64_invalid_conversion (const_tree fromtype, const_tree totype)
{
  /* Do not allow conversions to/from FP8. But do allow conversions between
     volatile and const variants of __mfp8. */
  bool fromtype_is_fp8
      = (TYPE_MAIN_VARIANT (fromtype) == aarch64_mfp8_type_node);
  bool totype_is_fp8 = (TYPE_MAIN_VARIANT (totype) == aarch64_mfp8_type_node);

  if (fromtype_is_fp8 && totype_is_fp8)
    return NULL;

  if (fromtype_is_fp8)
    return N_ ("invalid conversion from type %<mfloat8_t%>");
  if (totype_is_fp8)
    return N_ ("invalid conversion to type %<mfloat8_t%>");

  /* Conversion allowed.  */
  return NULL;
}

/* Implement TARGET_VERIFY_TYPE_CONTEXT.  */

static bool
aarch64_verify_type_context (location_t loc, type_context_kind context,
			     const_tree type, bool silent_p)
{
  return aarch64_sve::verify_type_context (loc, context, type, silent_p);
}

/* Find the first rtx_insn before insn that will generate an assembly
   instruction.  */

static rtx_insn *
aarch64_prev_real_insn (rtx_insn *insn)
{
  if (!insn)
    return NULL;

  do
    {
      insn = prev_real_insn (insn);
    }
  while (insn && recog_memoized (insn) < 0);

  return insn;
}

static bool
is_madd_op (enum attr_type t1)
{
  unsigned int i;
  /* A number of these may be AArch32 only.  */
  enum attr_type mlatypes[] = {
    TYPE_MLA, TYPE_MLAS, TYPE_SMLAD, TYPE_SMLADX, TYPE_SMLAL, TYPE_SMLALD,
    TYPE_SMLALS, TYPE_SMLALXY, TYPE_SMLAWX, TYPE_SMLAWY, TYPE_SMLAXY,
    TYPE_SMMLA, TYPE_UMLAL, TYPE_UMLALS,TYPE_SMLSD, TYPE_SMLSDX, TYPE_SMLSLD
  };

  for (i = 0; i < ARRAY_SIZE (mlatypes); i++)
    {
      if (t1 == mlatypes[i])
	return true;
    }

  return false;
}

/* Check if there is a register dependency between a load and the insn
   for which we hold recog_data.  */

static bool
dep_between_memop_and_curr (rtx memop)
{
  rtx load_reg;
  int opno;

  gcc_assert (GET_CODE (memop) == SET);

  if (!REG_P (SET_DEST (memop)))
    return false;

  load_reg = SET_DEST (memop);
  for (opno = 1; opno < recog_data.n_operands; opno++)
    {
      rtx operand = recog_data.operand[opno];
      if (REG_P (operand)
          && reg_overlap_mentioned_p (load_reg, operand))
        return true;

    }
  return false;
}


/* When working around the Cortex-A53 erratum 835769,
   given rtx_insn INSN, return true if it is a 64-bit multiply-accumulate
   instruction and has a preceding memory instruction such that a NOP
   should be inserted between them.  */

bool
aarch64_madd_needs_nop (rtx_insn* insn)
{
  enum attr_type attr_type;
  rtx_insn *prev;
  rtx body;

  if (!TARGET_FIX_ERR_A53_835769)
    return false;

  if (!INSN_P (insn) || recog_memoized (insn) < 0)
    return false;

  attr_type = get_attr_type (insn);
  if (!is_madd_op (attr_type))
    return false;

  prev = aarch64_prev_real_insn (insn);
  /* aarch64_prev_real_insn can call recog_memoized on insns other than INSN.
     Restore recog state to INSN to avoid state corruption.  */
  extract_constrain_insn_cached (insn);

  if (!prev || !contains_mem_rtx_p (PATTERN (prev)))
    return false;

  body = single_set (prev);

  /* If the previous insn is a memory op and there is no dependency between
     it and the DImode madd, emit a NOP between them.  If body is NULL then we
     have a complex memory operation, probably a load/store pair.
     Be conservative for now and emit a NOP.  */
  if (GET_MODE (recog_data.operand[0]) == DImode
      && (!body || !dep_between_memop_and_curr (body)))
    return true;

  return false;

}


/* Implement FINAL_PRESCAN_INSN.  */

void
aarch64_final_prescan_insn (rtx_insn *insn)
{
  if (aarch64_madd_needs_nop (insn))
    fprintf (asm_out_file, "\tnop // between mem op and mult-accumulate\n");
}


/* Return true if BASE_OR_STEP is a valid immediate operand for an SVE INDEX
   instruction.  */

bool
aarch64_sve_index_immediate_p (rtx base_or_step)
{
  return (CONST_INT_P (base_or_step)
	  && IN_RANGE (INTVAL (base_or_step), -16, 15));
}

/* Return true if X is a valid immediate for the SVE ADD and SUB instructions
   when applied to mode MODE.  Negate X first if NEGATE_P is true.  */

bool
aarch64_sve_arith_immediate_p (machine_mode mode, rtx x, bool negate_p)
{
  rtx elt = unwrap_const_vec_duplicate (x);
  if (!CONST_INT_P (elt))
    return false;

  HOST_WIDE_INT val = INTVAL (elt);
  if (negate_p)
    val = -val;
  val &= GET_MODE_MASK (GET_MODE_INNER (mode));

  if (val & 0xff)
    return IN_RANGE (val, 0, 0xff);
  return IN_RANGE (val, 0, 0xff00);
}

/* Return true if X is a valid immediate for the SVE SQADD and SQSUB
   instructions when applied to mode MODE.  Negate X first if NEGATE_P
   is true.  */

bool
aarch64_sve_sqadd_sqsub_immediate_p (machine_mode mode, rtx x, bool negate_p)
{
  if (!aarch64_sve_arith_immediate_p (mode, x, negate_p))
    return false;

  /* After the optional negation, the immediate must be nonnegative.
     E.g. a saturating add of -127 must be done via SQSUB Zn.B, Zn.B, #127
     instead of SQADD Zn.B, Zn.B, #129.  */
  rtx elt = unwrap_const_vec_duplicate (x);
  return negate_p == (INTVAL (elt) < 0);
}

/* Return true if X is a valid immediate operand for an SVE logical
   instruction such as AND.  */

bool
aarch64_sve_bitmask_immediate_p (rtx x)
{
  rtx elt;

  return (const_vec_duplicate_p (x, &elt)
	  && CONST_INT_P (elt)
	  && aarch64_bitmask_imm (INTVAL (elt),
				  GET_MODE_INNER (GET_MODE (x))));
}

/* Return true if X is a valid immediate for the SVE DUP and CPY
   instructions.  */

bool
aarch64_sve_dup_immediate_p (rtx x)
{
  x = aarch64_bit_representation (unwrap_const_vec_duplicate (x));
  if (!CONST_INT_P (x))
    return false;

  HOST_WIDE_INT val = INTVAL (x);
  if (val & 0xff)
    return IN_RANGE (val, -0x80, 0x7f);
  return IN_RANGE (val, -0x8000, 0x7f00);
}

/* Return true if X is a valid immediate operand for an SVE CMP instruction.
   SIGNED_P says whether the operand is signed rather than unsigned.  */

bool
aarch64_sve_cmp_immediate_p (rtx x, bool signed_p)
{
  x = unwrap_const_vec_duplicate (x);
  return (CONST_INT_P (x)
	  && (signed_p
	      ? IN_RANGE (INTVAL (x), -16, 15)
	      : IN_RANGE (INTVAL (x), 0, 127)));
}

/* Return true if X is a valid immediate operand for an SVE FADD or FSUB
   instruction.  Negate X first if NEGATE_P is true.  */

bool
aarch64_sve_float_arith_immediate_p (rtx x, bool negate_p)
{
  rtx elt;
  REAL_VALUE_TYPE r;

  if (GET_MODE_INNER (GET_MODE (x)) == BFmode
      || !const_vec_duplicate_p (x, &elt)
      || !CONST_DOUBLE_P (elt))
    return false;

  r = *CONST_DOUBLE_REAL_VALUE (elt);

  if (negate_p)
    r = real_value_negate (&r);

  if (real_equal (&r, &dconst1))
    return true;
  if (real_equal (&r, &dconsthalf))
    return true;
  return false;
}

/* Return true if X is a valid immediate operand for an SVE FMUL
   instruction.  */

bool
aarch64_sve_float_mul_immediate_p (rtx x)
{
  rtx elt;

  return (GET_MODE_INNER (GET_MODE (x)) != BFmode
	  && const_vec_duplicate_p (x, &elt)
	  && CONST_DOUBLE_P (elt)
	  && (real_equal (CONST_DOUBLE_REAL_VALUE (elt), &dconsthalf)
	      || real_equal (CONST_DOUBLE_REAL_VALUE (elt), &dconst2)));
}

/* Return true if replicating VAL32 is a valid 2-byte or 4-byte immediate
   for the Advanced SIMD operation described by WHICH and INSN.  If INFO
   is nonnull, use it to describe valid immediates.  */
static bool
aarch64_advsimd_valid_immediate_hs (unsigned int val32,
				    simd_immediate_info *info,
				    enum simd_immediate_check which,
				    simd_immediate_info::insn_type insn)
{
  /* Try a 4-byte immediate with LSL.  */
  for (unsigned int shift = 0; shift < 32; shift += 8)
    if ((val32 & (0xff << shift)) == val32)
      {
	if (info)
	  *info = simd_immediate_info (SImode, val32 >> shift, insn,
				       simd_immediate_info::LSL, shift);
	return true;
      }

  /* Try a 2-byte immediate with LSL.  */
  unsigned int imm16 = val32 & 0xffff;
  if (imm16 == (val32 >> 16))
    for (unsigned int shift = 0; shift < 16; shift += 8)
      if ((imm16 & (0xff << shift)) == imm16)
	{
	  if (info)
	    *info = simd_immediate_info (HImode, imm16 >> shift, insn,
					 simd_immediate_info::LSL, shift);
	  return true;
	}

  /* Try a 4-byte immediate with MSL, except for cases that MVN
     can handle.  */
  if (which == AARCH64_CHECK_MOV)
    for (unsigned int shift = 8; shift < 24; shift += 8)
      {
	unsigned int low = (1 << shift) - 1;
	if (((val32 & (0xff << shift)) | low) == val32)
	  {
	    if (info)
	      *info = simd_immediate_info (SImode, val32 >> shift, insn,
					   simd_immediate_info::MSL, shift);
	    return true;
	  }
      }

  return false;
}

/* Return true if replicating VAL64 with mode MODE is a valid immediate for the
   Advanced SIMD operation described by WHICH.  If INFO is nonnull,
   use it to describe valid immediates.  */
static bool
aarch64_advsimd_valid_immediate (unsigned HOST_WIDE_INT val64,
				 scalar_int_mode mode,
				 simd_immediate_info *info,
				 enum simd_immediate_check which)
{
  unsigned int val32 = val64 & 0xffffffff;
  unsigned int val8 = val64 & 0xff;

  if (mode != DImode)
    {
      if ((which == AARCH64_CHECK_MOV || which == AARCH64_CHECK_ORR)
	  && aarch64_advsimd_valid_immediate_hs (val32, info, which,
						 simd_immediate_info::MOV))
	return true;

      if ((which == AARCH64_CHECK_MOV || which == AARCH64_CHECK_AND)
	  && aarch64_advsimd_valid_immediate_hs (~val32, info, which,
						 simd_immediate_info::MVN))
	return true;

      /* Try using a replicated byte.  */
      if (which == AARCH64_CHECK_MOV && mode == QImode)
	{
	  if (info)
	    *info = simd_immediate_info (QImode, val8);
	  return true;
	}
    }

  /* Try using a bit-to-bytemask.  */
  if (which == AARCH64_CHECK_MOV)
    {
      unsigned int i;
      for (i = 0; i < 64; i += 8)
	{
	  unsigned char byte = (val64 >> i) & 0xff;
	  if (byte != 0 && byte != 0xff)
	    break;
	}
      if (i == 64)
	{
	  if (info)
	    *info = simd_immediate_info (DImode, val64);
	  return true;
	}
    }
  return false;
}

/* Return true if replicating IVAL with MODE gives a valid immediate for an SVE
   MOV instruction.  If INFO is nonnull, use it to describe valid
   immediates.  */

static bool
aarch64_sve_valid_immediate (unsigned HOST_WIDE_INT ival, scalar_int_mode mode,
			     simd_immediate_info *info,
			     enum simd_immediate_check which)
{
  HOST_WIDE_INT val = trunc_int_for_mode (ival, mode);

  if (which == AARCH64_CHECK_MOV)
    {
      if (IN_RANGE (val, -0x80, 0x7f))
	{
	  /* DUP with no shift.  */
	  if (info)
	    *info = simd_immediate_info (mode, val,
					 simd_immediate_info::SVE_MOV);
	  return true;
	}
      if ((val & 0xff) == 0 && IN_RANGE (val, -0x8000, 0x7f00))
	{
	  /* DUP with LSL #8.  */
	  if (info)
	    *info = simd_immediate_info (mode, val,
					 simd_immediate_info::SVE_MOV);
	  return true;
	}
    }
  if (aarch64_bitmask_imm (ival, mode))
    {
      /* DUPM.  */
      if (info)
	*info = simd_immediate_info (mode, val, simd_immediate_info::SVE_MOV);
      return true;
    }
  return false;
}

/* Return true if X is an UNSPEC_PTRUE constant of the form:

       (const (unspec [PATTERN ZERO] UNSPEC_PTRUE))

   where PATTERN is the svpattern as a CONST_INT and where ZERO
   is a zero constant of the required PTRUE mode (which can have
   fewer elements than X's mode, if zero bits are significant).

   If so, and if INFO is nonnull, describe the immediate in INFO.  */
bool
aarch64_sve_ptrue_svpattern_p (rtx x, struct simd_immediate_info *info)
{
  if (GET_CODE (x) != CONST)
    return false;

  x = XEXP (x, 0);
  if (GET_CODE (x) != UNSPEC || XINT (x, 1) != UNSPEC_PTRUE)
    return false;

  if (info)
    {
      aarch64_svpattern pattern
	= (aarch64_svpattern) INTVAL (XVECEXP (x, 0, 0));
      machine_mode pred_mode = GET_MODE (XVECEXP (x, 0, 1));
      scalar_int_mode int_mode = aarch64_sve_element_int_mode (pred_mode);
      *info = simd_immediate_info (int_mode, pattern);
    }
  return true;
}

/* Return true if X is a valid SVE predicate.  If INFO is nonnull, use
   it to describe valid immediates.  */

static bool
aarch64_sve_pred_valid_immediate (rtx x, simd_immediate_info *info)
{
  if (aarch64_sve_ptrue_svpattern_p (x, info))
    return true;

  if (x == CONST0_RTX (GET_MODE (x)))
    {
      if (info)
	*info = simd_immediate_info (DImode, 0);
      return true;
    }

  /* Analyze the value as a VNx16BImode.  This should be relatively
     efficient, since rtx_vector_builder has enough built-in capacity
     to store all VLA predicate constants without needing the heap.  */
  rtx_vector_builder builder;
  if (!aarch64_get_sve_pred_bits (builder, x))
    return false;

  unsigned int elt_size = aarch64_widest_sve_pred_elt_size (builder);
  if (int vl = aarch64_partial_ptrue_length (builder, elt_size))
    {
      machine_mode mode = aarch64_sve_pred_mode (elt_size).require ();
      aarch64_svpattern pattern = aarch64_svpattern_for_vl (mode, vl);
      if (pattern != AARCH64_NUM_SVPATTERNS)
	{
	  if (info)
	    {
	      scalar_int_mode int_mode = aarch64_sve_element_int_mode (mode);
	      *info = simd_immediate_info (int_mode, pattern);
	    }
	  return true;
	}
    }
  return false;
}

/* We can only represent floating point constants which will fit in
   "quarter-precision" values.  These values are characterised by
   a sign bit, a 4-bit mantissa and a 3-bit exponent.  And are given
   by:

   (-1)^s * (n/16) * 2^r

   Where:
     's' is the sign bit.
     'n' is an integer in the range 16 <= n <= 31.
     'r' is an integer in the range -3 <= r <= 4.

   Return true iff R represents a vale encodable into an AArch64 floating point
   move instruction as an immediate.  Othewise false.  */

static bool
aarch64_real_float_const_representable_p (REAL_VALUE_TYPE r)
{
  /* This represents our current view of how many bits
     make up the mantissa.  */
  int point_pos = 2 * HOST_BITS_PER_WIDE_INT - 1;
  int exponent;
  unsigned HOST_WIDE_INT mantissa, mask;
  REAL_VALUE_TYPE m;
  bool fail = false;

  /* We cannot represent infinities, NaNs or +/-zero.  We won't
     know if we have +zero until we analyse the mantissa, but we
     can reject the other invalid values.  */
  if (REAL_VALUE_ISINF (r) || REAL_VALUE_ISNAN (r)
      || REAL_VALUE_MINUS_ZERO (r))
    return false;

  /* Extract exponent.  */
  r = real_value_abs (&r);
  exponent = REAL_EXP (&r);

  /* For the mantissa, we expand into two HOST_WIDE_INTS, apart from the
     highest (sign) bit, with a fixed binary point at bit point_pos.
     m1 holds the low part of the mantissa, m2 the high part.
     WARNING: If we ever have a representation using more than 2 * H_W_I - 1
     bits for the mantissa, this can fail (low bits will be lost).  */
  real_ldexp (&m, &r, point_pos - exponent);
  wide_int w = real_to_integer (&m, &fail, HOST_BITS_PER_WIDE_INT * 2);

  /* If the low part of the mantissa has bits set we cannot represent
     the value.  */
  if (fail || w.ulow () != 0)
    return false;

  /* We have rejected the lower HOST_WIDE_INT, so update our
     understanding of how many bits lie in the mantissa and
     look only at the high HOST_WIDE_INT.  */
  mantissa = w.elt (1);
  point_pos -= HOST_BITS_PER_WIDE_INT;

  /* We can only represent values with a mantissa of the form 1.xxxx.  */
  mask = ((unsigned HOST_WIDE_INT)1 << (point_pos - 5)) - 1;
  if ((mantissa & mask) != 0)
    return false;

  /* Having filtered unrepresentable values, we may now remove all
     but the highest 5 bits.  */
  mantissa >>= point_pos - 5;

  /* We cannot represent the value 0.0, so reject it.  This is handled
     elsewhere.  */
  if (mantissa == 0)
    return false;

  /* Then, as bit 4 is always set, we can mask it off, leaving
     the mantissa in the range [0, 15].  */
  mantissa &= ~(1 << 4);
  gcc_assert (mantissa <= 15);

  /* GCC internally does not use IEEE754-like encoding (where normalized
     significands are in the range [1, 2).  GCC uses [0.5, 1) (see real.cc).
     Our mantissa values are shifted 4 places to the left relative to
     normalized IEEE754 so we must modify the exponent returned by REAL_EXP
     by 5 places to correct for GCC's representation.  */
  exponent = 5 - exponent;

  return (exponent >= 0 && exponent <= 7);
}

/* Return true if OP is a valid SIMD immediate for the operation
   described by WHICH.  If INFO is nonnull, use it to describe valid
   immediates.  */
static bool
aarch64_simd_valid_imm (rtx op, simd_immediate_info *info,
			enum simd_immediate_check which)
{
  machine_mode mode = GET_MODE (op);
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags == 0 || vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
    return false;

  if ((vec_flags & VEC_ADVSIMD) && !TARGET_SIMD)
    return false;

  if (vec_flags == (VEC_SVE_PRED | VEC_STRUCT))
    return op == CONST0_RTX (mode) || op == CONSTM1_RTX (mode);

  if (vec_flags & VEC_SVE_PRED)
    return aarch64_sve_pred_valid_immediate (op, info);

  scalar_mode elt_mode = GET_MODE_INNER (mode);
  rtx base, step;
  unsigned int n_elts;
  if (CONST_VECTOR_P (op)
      && CONST_VECTOR_DUPLICATE_P (op))
    n_elts = CONST_VECTOR_NPATTERNS (op);
  else if (which == AARCH64_CHECK_MOV
	   && TARGET_SVE
	   && const_vec_series_p (op, &base, &step))
    {
      gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
      if (!aarch64_sve_index_immediate_p (base)
	  || !aarch64_sve_index_immediate_p (step))
	return false;

      if (info)
	{
	  /* Get the corresponding container mode.  E.g. an INDEX on V2SI
	     should yield two integer values per 128-bit block, meaning
	     that we need to treat it in the same way as V2DI and then
	     ignore the upper 32 bits of each element.  */
	  elt_mode = aarch64_sve_container_int_mode (mode);
	  *info = simd_immediate_info (elt_mode, base, step);
	}
      return true;
    }
  else if (CONST_VECTOR_P (op)
	   && CONST_VECTOR_NUNITS (op).is_constant (&n_elts))
    /* N_ELTS set above.  */;
  else
    return false;

  /* If all elements in an SVE vector have the same value, we have a free
     choice between using the element mode and using the container mode.
     Using the element mode means that unused parts of the vector are
     duplicates of the used elements, while using the container mode means
     that the unused parts are an extension of the used elements.  Using the
     element mode is better for (say) VNx4HI 0x101, since 0x01010101 is valid
     for its container mode VNx4SI while 0x00000101 isn't.

     If not all elements in an SVE vector have the same value, we need the
     transition from one element to the next to occur at container boundaries.
     E.g. a fixed-length VNx4HI containing { 1, 2, 3, 4 } should be treated
     in the same way as a VNx4SI containing { 1, 2, 3, 4 }.  */
  scalar_int_mode elt_int_mode;
  if ((vec_flags & VEC_SVE_DATA) && n_elts > 1)
    elt_int_mode = aarch64_sve_container_int_mode (mode);
  else
    elt_int_mode = int_mode_for_mode (elt_mode).require ();

  unsigned int elt_size = GET_MODE_SIZE (elt_int_mode);
  if (elt_size > 8)
    return false;

  /* Expand the vector constant out into a byte vector, with the least
     significant byte of the register first.  */
  auto_vec<unsigned char, 16> bytes;
  bytes.reserve (n_elts * elt_size);
  for (unsigned int i = 0; i < n_elts; i++)
    {
      /* The vector is provided in gcc endian-neutral fashion.
	 For aarch64_be Advanced SIMD, it must be laid out in the vector
	 register in reverse order.  */
      bool swap_p = ((vec_flags & VEC_ADVSIMD) != 0 && BYTES_BIG_ENDIAN);
      rtx elt = CONST_VECTOR_ELT (op, swap_p ? (n_elts - 1 - i) : i);

      if (elt_mode != elt_int_mode)
	elt = gen_lowpart (elt_int_mode, elt);

      if (!CONST_INT_P (elt))
	return false;

      unsigned HOST_WIDE_INT elt_val = INTVAL (elt);
      for (unsigned int byte = 0; byte < elt_size; byte++)
	{
	  bytes.quick_push (elt_val & 0xff);
	  elt_val >>= BITS_PER_UNIT;
	}
    }

  /* The immediate must repeat every eight bytes.  */
  unsigned int nbytes = bytes.length ();
  for (unsigned i = 8; i < nbytes; ++i)
    if (bytes[i] != bytes[i - 8])
      return false;

  /* Get the repeating 8-byte value as an integer.  No endian correction
     is needed here because bytes is already in lsb-first order.  */
  unsigned HOST_WIDE_INT val64 = 0;
  for (unsigned int i = 0; i < 8; i++)
    val64 |= ((unsigned HOST_WIDE_INT) bytes[i % nbytes]
	      << (i * BITS_PER_UNIT));

  /* Try encoding the integer immediate as a floating point value if it's an
     exact value.  */
  scalar_float_mode fmode = DFmode;
  scalar_int_mode imode = DImode;
  unsigned HOST_WIDE_INT ival = val64;
  unsigned int val32 = val64 & 0xffffffff;
  if (val32 == (val64 >> 32))
    {
      fmode = SFmode;
      imode = SImode;
      ival = val32;
      unsigned int val16 = val32 & 0xffff;
      if (val16 == (val32 >> 16))
	{
	  fmode = HFmode;
	  imode = HImode;
	  ival = val16;
	  unsigned int val8 = val16 & 0xff;
	  if (val8 == (val16 >> 8))
	    {
	      imode = QImode;
	      ival = val8;
	    }
	}
    }

  if (which == AARCH64_CHECK_MOV
      && imode != QImode
      && (imode != HImode || TARGET_FP_F16INST))
    {
      long int as_long_ints[2];
      as_long_ints[0] = ival & 0xFFFFFFFF;
      as_long_ints[1] = (ival >> 32) & 0xFFFFFFFF;

      REAL_VALUE_TYPE r;
      real_from_target (&r, as_long_ints, fmode);
      if (aarch64_real_float_const_representable_p (r))
	{
	  if (info)
	    {
	      rtx float_val = const_double_from_real_value (r, fmode);
	      *info = simd_immediate_info (fmode, float_val);
	    }
	  return true;
	}
    }

  if (vec_flags & VEC_SVE_DATA)
    return aarch64_sve_valid_immediate (ival, imode, info, which);

  if (aarch64_advsimd_valid_immediate (val64, imode, info, which))
    return true;

  if (TARGET_SVE)
    return aarch64_sve_valid_immediate (ival, imode, info, which);
  return false;
}

/* Return true if OP is a valid SIMD move immediate for SVE or AdvSIMD.  */
bool
aarch64_simd_valid_mov_imm (rtx op)
{
  return aarch64_simd_valid_imm (op, NULL, AARCH64_CHECK_MOV);
}

/* Return true if OP is a valid SIMD orr immediate for SVE or AdvSIMD.  */
bool
aarch64_simd_valid_orr_imm (rtx op)
{
  return aarch64_simd_valid_imm (op, NULL, AARCH64_CHECK_ORR);
}

/* Return true if OP is a valid SIMD and immediate for SVE or AdvSIMD.  */
bool
aarch64_simd_valid_and_imm (rtx op)
{
  return aarch64_simd_valid_imm (op, NULL, AARCH64_CHECK_AND);
}

/* Return true if OP is a valid SIMD xor immediate for SVE.  */
bool
aarch64_simd_valid_xor_imm (rtx op)
{
  return aarch64_simd_valid_imm (op, NULL, AARCH64_CHECK_XOR);
}

/* Check whether X is a VEC_SERIES-like constant that starts at 0 and
   has a step in the range of INDEX.  Return the index expression if so,
   otherwise return null.  */
rtx
aarch64_check_zero_based_sve_index_immediate (rtx x)
{
  rtx base, step;
  if (const_vec_series_p (x, &base, &step)
      && base == const0_rtx
      && aarch64_sve_index_immediate_p (step))
    return step;
  return NULL_RTX;
}

/* Check of immediate shift constants are within range.  */
bool
aarch64_simd_shift_imm_p (rtx x, machine_mode mode, bool left)
{
  x = unwrap_const_vec_duplicate (x);
  if (!CONST_INT_P (x))
    return false;
  int bit_width = GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT;
  if (left)
    return IN_RANGE (INTVAL (x), 0, bit_width - 1);
  else
    return IN_RANGE (INTVAL (x), 1, bit_width);
}

/* Return the bitmask CONST_INT to select the bits required by a zero extract
   operation of width WIDTH at bit position POS.  */

rtx
aarch64_mask_from_zextract_ops (rtx width, rtx pos)
{
  gcc_assert (CONST_INT_P (width));
  gcc_assert (CONST_INT_P (pos));

  unsigned HOST_WIDE_INT mask
    = ((unsigned HOST_WIDE_INT) 1 << UINTVAL (width)) - 1;
  return GEN_INT (mask << UINTVAL (pos));
}

bool
aarch64_mov_operand_p (rtx x, machine_mode mode)
{
  if (GET_CODE (x) == HIGH
      && aarch64_valid_symref (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
    return true;

  if (CONST_INT_P (x))
    return true;

  if (VECTOR_MODE_P (GET_MODE (x)))
    {
      /* Require predicate constants to be VNx16BI before RA, so that we
	 force everything to have a canonical form.  */
      if (!lra_in_progress
	  && !reload_completed
	  && aarch64_sve_pred_mode_p (GET_MODE (x))
	  && known_eq (GET_MODE_SIZE (GET_MODE (x)), BYTES_PER_SVE_PRED)
	  && GET_MODE (x) != VNx16BImode)
	return false;

      return aarch64_simd_valid_mov_imm (x);
    }

  /* Remove UNSPEC_SALT_ADDR before checking symbol reference.  */
  x = strip_salt (x);

  /* GOT accesses are valid moves.  */
  if (SYMBOL_REF_P (x)
      && aarch64_classify_symbolic_expression (x) == SYMBOL_SMALL_GOT_4G)
    return true;

  if (SYMBOL_REF_P (x) && mode == DImode && CONSTANT_ADDRESS_P (x))
    return true;

  if (TARGET_SVE
      && (aarch64_sve_cnt_immediate_p (x)
	  || aarch64_sve_rdvl_immediate_p (x)))
    return true;

  if (aarch64_rdsvl_immediate_p (x))
    return true;

  return aarch64_classify_symbolic_expression (x)
    == SYMBOL_TINY_ABSOLUTE;
}

/* Return a function-invariant register that contains VALUE.  *CACHED_INSN
   caches instructions that set up such registers, so that they can be
   reused by future calls.  */

static rtx
aarch64_get_shareable_reg (rtx_insn **cached_insn, rtx value)
{
  rtx_insn *insn = *cached_insn;
  if (insn && INSN_P (insn) && !insn->deleted ())
    {
      rtx pat = PATTERN (insn);
      if (GET_CODE (pat) == SET)
	{
	  rtx dest = SET_DEST (pat);
	  if (REG_P (dest)
	      && !HARD_REGISTER_P (dest)
	      && rtx_equal_p (SET_SRC (pat), value))
	    return dest;
	}
    }
  rtx reg = gen_reg_rtx (GET_MODE (value));
  *cached_insn = emit_insn_before (gen_rtx_SET (reg, value),
				   function_beg_insn);
  return reg;
}

/* Create a 0 constant that is based on V4SI to allow CSE to optimally share
   the constant creation.  */

rtx
aarch64_gen_shareable_zero (machine_mode mode)
{
  rtx reg = aarch64_get_shareable_reg (&cfun->machine->advsimd_zero_insn,
				       CONST0_RTX (V4SImode));
  return lowpart_subreg (mode, reg, GET_MODE (reg));
}

/* INSN is some form of extension or shift that can be split into a
   permutation involving a shared zero.  Return true if we should
   perform such a split.

   ??? For now, make sure that the split instruction executes more
   frequently than the zero that feeds it.  In future it would be good
   to split without that restriction and instead recombine shared zeros
   if they turn out not to be worthwhile.  This would allow splits in
   single-block functions and would also cope more naturally with
   rematerialization.  The downside of not doing this is that we lose the
   optimizations for vector epilogues as well.  */

bool
aarch64_split_simd_shift_p (rtx_insn *insn)
{
  return (can_create_pseudo_p ()
	  && optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn))
	  && (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
	      < BLOCK_FOR_INSN (insn)->count));
}

/* Return a const_int vector of VAL.  */
rtx
aarch64_simd_gen_const_vector_dup (machine_mode mode, HOST_WIDE_INT val)
{
  rtx c = gen_int_mode (val, GET_MODE_INNER (mode));
  return gen_const_vec_duplicate (mode, c);
}

/* Check OP is a legal scalar immediate for the MOVI instruction.  */

bool
aarch64_simd_scalar_immediate_valid_for_move (rtx op, scalar_int_mode mode)
{
  machine_mode vmode;

  vmode = aarch64_simd_container_mode (mode, 64);
  rtx op_v = aarch64_simd_gen_const_vector_dup (vmode, INTVAL (op));
  return aarch64_simd_valid_mov_imm (op_v);
}

/* Construct and return a PARALLEL RTX vector with elements numbering the
   lanes of either the high (HIGH == TRUE) or low (HIGH == FALSE) half of
   the vector - from the perspective of the architecture.  This does not
   line up with GCC's perspective on lane numbers, so we end up with
   different masks depending on our target endian-ness.  The diagram
   below may help.  We must draw the distinction when building masks
   which select one half of the vector.  An instruction selecting
   architectural low-lanes for a big-endian target, must be described using
   a mask selecting GCC high-lanes.

                 Big-Endian             Little-Endian

GCC             0   1   2   3           3   2   1   0
              | x | x | x | x |       | x | x | x | x |
Architecture    3   2   1   0           3   2   1   0

Low Mask:         { 2, 3 }                { 0, 1 }
High Mask:        { 0, 1 }                { 2, 3 }

   MODE Is the mode of the vector and NUNITS is the number of units in it.  */

rtx
aarch64_simd_vect_par_cnst_half (machine_mode mode, int nunits, bool high)
{
  rtvec v = rtvec_alloc (nunits / 2);
  int high_base = nunits / 2;
  int low_base = 0;
  int base;
  rtx t1;
  int i;

  if (BYTES_BIG_ENDIAN)
    base = high ? low_base : high_base;
  else
    base = high ? high_base : low_base;

  for (i = 0; i < nunits / 2; i++)
    RTVEC_ELT (v, i) = GEN_INT (base + i);

  t1 = gen_rtx_PARALLEL (mode, v);
  return t1;
}

/* Check OP for validity as a PARALLEL RTX vector with elements
   numbering the lanes of either the high (HIGH == TRUE) or low lanes,
   from the perspective of the architecture.  See the diagram above
   aarch64_simd_vect_par_cnst_half for more details.  */

bool
aarch64_simd_check_vect_par_cnst_half (rtx op, machine_mode mode,
				       bool high)
{
  int nelts;
  if (!VECTOR_MODE_P (mode) || !GET_MODE_NUNITS (mode).is_constant (&nelts))
    return false;

  rtx ideal = aarch64_simd_vect_par_cnst_half (mode, nelts, high);
  HOST_WIDE_INT count_op = XVECLEN (op, 0);
  HOST_WIDE_INT count_ideal = XVECLEN (ideal, 0);
  int i = 0;

  if (count_op != count_ideal)
    return false;

  for (i = 0; i < count_ideal; i++)
    {
      rtx elt_op = XVECEXP (op, 0, i);
      rtx elt_ideal = XVECEXP (ideal, 0, i);

      if (!CONST_INT_P (elt_op)
	  || INTVAL (elt_ideal) != INTVAL (elt_op))
	return false;
    }
  return true;
}

/* Return a PARALLEL containing NELTS elements, with element I equal
   to BASE + I * STEP.  */

rtx
aarch64_gen_stepped_int_parallel (unsigned int nelts, int base, int step)
{
  rtvec vec = rtvec_alloc (nelts);
  for (unsigned int i = 0; i < nelts; ++i)
    RTVEC_ELT (vec, i) = gen_int_mode (base + i * step, DImode);
  return gen_rtx_PARALLEL (VOIDmode, vec);
}

/* Return true if OP is a PARALLEL of CONST_INTs that form a linear
   series with step STEP.  */

bool
aarch64_stepped_int_parallel_p (rtx op, int step)
{
  if (GET_CODE (op) != PARALLEL || !CONST_INT_P (XVECEXP (op, 0, 0)))
    return false;

  unsigned HOST_WIDE_INT base = UINTVAL (XVECEXP (op, 0, 0));
  for (int i = 1; i < XVECLEN (op, 0); ++i)
    if (!CONST_INT_P (XVECEXP (op, 0, i))
	|| UINTVAL (XVECEXP (op, 0, i)) != base + i * step)
      return false;

  return true;
}

/* Return true if OPERANDS[0] to OPERANDS[NUM_OPERANDS - 1] form a
   sequence of strided registers, with the stride being equal STRIDE.
   The operands are already known to be FPRs.  */
bool
aarch64_strided_registers_p (rtx *operands, unsigned int num_operands,
			     unsigned int stride)
{
  for (unsigned int i = 1; i < num_operands; ++i)
    if (REGNO (operands[i]) != REGNO (operands[0]) + i * stride)
      return false;
  return true;
}

/* Bounds-check lanes.  Ensure OPERAND lies between LOW (inclusive) and
   HIGH (exclusive).  */
void
aarch64_simd_lane_bounds (rtx operand, HOST_WIDE_INT low, HOST_WIDE_INT high,
			  const_tree exp)
{
  HOST_WIDE_INT lane;
  gcc_assert (CONST_INT_P (operand));
  lane = INTVAL (operand);

  if (lane < low || lane >= high)
  {
    if (exp)
      error_at (EXPR_LOCATION (exp), "lane %wd out of range %wd - %wd",
		lane, low, high - 1);
    else
      error ("lane %wd out of range %wd - %wd", lane, low, high - 1);
  }
}

/* Peform endian correction on lane number N, which indexes a vector
   of mode MODE, and return the result as an SImode rtx.  */

rtx
aarch64_endian_lane_rtx (machine_mode mode, unsigned int n)
{
  return gen_int_mode (ENDIAN_LANE_N (GET_MODE_NUNITS (mode), n), SImode);
}

/* Return TRUE if OP is a valid vector addressing mode.  */

bool
aarch64_simd_mem_operand_p (rtx op)
{
  return (MEM_P (op)
	  && (GET_CODE (XEXP (op, 0)) == POST_INC || REG_P (XEXP (op, 0)))
	  && memory_operand (op, VOIDmode));
}

/* Return true if OP is a valid MEM operand for an SVE LD1R instruction.  */

bool
aarch64_sve_ld1r_operand_p (rtx op)
{
  struct aarch64_address_info addr;
  scalar_mode mode;

  return (MEM_P (op)
	  && is_a <scalar_mode> (GET_MODE (op), &mode)
	  && aarch64_classify_address (&addr, XEXP (op, 0), mode, false)
	  && addr.type == ADDRESS_REG_IMM
	  && offset_6bit_unsigned_scaled_p (mode, addr.const_offset));
}

/* Return true if OP is a valid MEM operand for an SVE LD1R{Q,O} instruction
   where the size of the read data is specified by `mode` and the size of the
   vector elements are specified by `elem_mode`.   */
bool
aarch64_sve_ld1rq_ld1ro_operand_p (rtx op, machine_mode mode,
				   scalar_mode elem_mode)
{
  struct aarch64_address_info addr;
  if (!MEM_P (op)
      || !aarch64_classify_address (&addr, XEXP (op, 0), elem_mode, false))
    return false;

  if (addr.type == ADDRESS_REG_IMM)
    return offset_4bit_signed_scaled_p (mode, addr.const_offset);

  if (addr.type == ADDRESS_REG_REG)
    return (1U << addr.shift) == GET_MODE_SIZE (elem_mode);

  return false;
}

/* Return true if OP is a valid MEM operand for an SVE LD1RQ instruction.  */
bool
aarch64_sve_ld1rq_operand_p (rtx op)
{
  return aarch64_sve_ld1rq_ld1ro_operand_p (op, TImode,
					    GET_MODE_INNER (GET_MODE (op)));
}

/* Return true if OP is a valid MEM operand for an SVE LD1RO instruction for
   accessing a vector where the element size is specified by `elem_mode`.  */
bool
aarch64_sve_ld1ro_operand_p (rtx op, scalar_mode elem_mode)
{
  return aarch64_sve_ld1rq_ld1ro_operand_p (op, OImode, elem_mode);
}

/* Return true if OP is a valid MEM operand for an SVE LDFF1 instruction.  */
bool
aarch64_sve_ldff1_operand_p (rtx op)
{
  if (!MEM_P (op))
    return false;

  struct aarch64_address_info addr;
  if (!aarch64_classify_address (&addr, XEXP (op, 0), GET_MODE (op), false))
    return false;

  if (addr.type == ADDRESS_REG_IMM)
    return known_eq (addr.const_offset, 0);

  return addr.type == ADDRESS_REG_REG;
}

/* Return true if OP is a valid MEM operand for an SVE LDNF1 instruction.  */
bool
aarch64_sve_ldnf1_operand_p (rtx op)
{
  struct aarch64_address_info addr;

  return (MEM_P (op)
	  && aarch64_classify_address (&addr, XEXP (op, 0),
				       GET_MODE (op), false)
	  && addr.type == ADDRESS_REG_IMM);
}

/* Return true if OP is a valid MEM operand for an SVE LDR instruction.
   The conditions for STR are the same.  */
bool
aarch64_sve_ldr_operand_p (rtx op)
{
  struct aarch64_address_info addr;

  return (MEM_P (op)
	  && aarch64_classify_address (&addr, XEXP (op, 0), GET_MODE (op),
				       false, ADDR_QUERY_ANY)
	  && addr.type == ADDRESS_REG_IMM);
}

/* Return true if OP is a valid address for an SVE PRF[BHWD] instruction,
   addressing memory of mode MODE.  */
bool
aarch64_sve_prefetch_operand_p (rtx op, machine_mode mode)
{
  struct aarch64_address_info addr;
  if (!aarch64_classify_address (&addr, op, mode, false, ADDR_QUERY_ANY))
    return false;

  if (addr.type == ADDRESS_REG_IMM)
    return offset_6bit_signed_scaled_p (mode, addr.const_offset);

  return addr.type == ADDRESS_REG_REG;
}

/* Return true if OP is a valid MEM operand for an SVE_STRUCT mode.
   We need to be able to access the individual pieces, so the range
   is different from LD[234] and ST[234].  */
bool
aarch64_sve_struct_memory_operand_p (rtx op)
{
  if (!MEM_P (op))
    return false;

  machine_mode mode = GET_MODE (op);
  struct aarch64_address_info addr;
  if (!aarch64_classify_address (&addr, XEXP (op, 0), SVE_BYTE_MODE, false,
				 ADDR_QUERY_ANY)
      || addr.type != ADDRESS_REG_IMM)
    return false;

  poly_int64 first = addr.const_offset;
  poly_int64 last = first + GET_MODE_SIZE (mode) - BYTES_PER_SVE_VECTOR;
  return (offset_4bit_signed_scaled_p (SVE_BYTE_MODE, first)
	  && offset_4bit_signed_scaled_p (SVE_BYTE_MODE, last));
}

/* Return true if OFFSET is a constant integer and if VNUM is
   OFFSET * the number of bytes in an SVE vector.  This is the requirement
   that exists in SME LDR and STR instructions, where the VL offset must
   equal the ZA slice offset.  */
bool
aarch64_sme_ldr_vnum_offset_p (rtx offset, rtx vnum)
{
  if (!CONST_INT_P (offset) || !IN_RANGE (INTVAL (offset), 0, 15))
    return false;

  if (TARGET_STREAMING)
    {
      poly_int64 const_vnum;
      return (poly_int_rtx_p (vnum, &const_vnum)
	      && known_eq (const_vnum,
			   INTVAL (offset) * BYTES_PER_SVE_VECTOR));
    }
  else
    {
      HOST_WIDE_INT factor;
      return (aarch64_sme_vq_unspec_p (vnum, &factor)
	      && factor == INTVAL (offset) * 16);
    }
}

/* Emit a register copy from operand to operand, taking care not to
   early-clobber source registers in the process.

   COUNT is the number of components into which the copy needs to be
   decomposed.  */
void
aarch64_simd_emit_reg_reg_move (rtx *operands, machine_mode mode,
				unsigned int count)
{
  unsigned int i;
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);

  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      || rdest < rsrc)
    for (i = 0; i < count; i++)
      emit_move_insn (gen_rtx_REG (mode, rdest + i),
		      gen_rtx_REG (mode, rsrc + i));
  else
    for (i = 0; i < count; i++)
      emit_move_insn (gen_rtx_REG (mode, rdest + count - i - 1),
		      gen_rtx_REG (mode, rsrc + count - i - 1));
}

/* Compute and return the length of aarch64_simd_reglist<mode>, where <mode> is
   one of VSTRUCT modes: OI, CI, or XI.  */
int
aarch64_simd_attr_length_rglist (machine_mode mode)
{
  /* This is only used (and only meaningful) for Advanced SIMD, not SVE.  */
  return (GET_MODE_SIZE (mode).to_constant () / UNITS_PER_VREG) * 4;
}

/* Implement target hook TARGET_VECTOR_ALIGNMENT.  The AAPCS64 sets the maximum
   alignment of a vector to 128 bits.  SVE predicates have an alignment of
   16 bits.  */
static HOST_WIDE_INT
aarch64_simd_vector_alignment (const_tree type)
{
  /* ??? Checking the mode isn't ideal, but VECTOR_BOOLEAN_TYPE_P can
     be set for non-predicate vectors of booleans.  Modes are the most
     direct way we have of identifying real SVE predicate types.  */
  if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_VECTOR_BOOL)
    return 16;
  widest_int min_size
    = constant_lower_bound (wi::to_poly_widest (TYPE_SIZE (type)));
  return wi::umin (min_size, 128).to_uhwi ();
}

/* Implement target hook TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT.  */
static poly_uint64
aarch64_vectorize_preferred_vector_alignment (const_tree type)
{
  if (aarch64_sve_data_mode_p (TYPE_MODE (type)))
    {
      /* If the length of the vector is a fixed power of 2, try to align
	 to that length, otherwise don't try to align at all.  */
      HOST_WIDE_INT result;
      if (!GET_MODE_BITSIZE (TYPE_MODE (type)).is_constant (&result)
	  || !pow2p_hwi (result))
	result = TYPE_ALIGN (TREE_TYPE (type));
      return result;
    }
  return TYPE_ALIGN (type);
}

/* Implement target hook TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE.  */
static bool
aarch64_simd_vector_alignment_reachable (const_tree type, bool is_packed)
{
  if (is_packed)
    return false;

  /* For fixed-length vectors, check that the vectorizer will aim for
     full-vector alignment.  This isn't true for generic GCC vectors
     that are wider than the ABI maximum of 128 bits.  */
  poly_uint64 preferred_alignment =
    aarch64_vectorize_preferred_vector_alignment (type);
  if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
      && maybe_ne (wi::to_widest (TYPE_SIZE (type)),
		   preferred_alignment))
    return false;

  /* Vectors whose size is <= BIGGEST_ALIGNMENT are naturally aligned.  */
  return true;
}

/* Return true if the vector misalignment factor is supported by the
   target.  */
static bool
aarch64_builtin_support_vector_misalignment (machine_mode mode,
					     const_tree type, int misalignment,
					     bool is_packed)
{
  if (TARGET_SIMD && STRICT_ALIGNMENT)
    {
      /* Return if movmisalign pattern is not supported for this mode.  */
      if (optab_handler (movmisalign_optab, mode) == CODE_FOR_nothing)
        return false;

      /* Misalignment factor is unknown at compile time.  */
      if (misalignment == -1)
	return false;
    }
  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

/* If VALS is a vector constant that can be loaded into a register
   using DUP, generate instructions to do so and return an RTX to
   assign to the register.  Otherwise return NULL_RTX.  */
static rtx
aarch64_simd_dup_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx x;

  if (!const_vec_duplicate_p (vals, &x))
    return NULL_RTX;

  /* We can load this constant by using DUP and a constant in a
     single ARM register.  This will be cheaper than a vector
     load.  */
  x = force_reg (inner_mode, x);
  return gen_vec_duplicate (mode, x);
}


/* Generate code to load VALS, which is a PARALLEL containing only
   constants (for vec_init) or CONST_VECTOR, efficiently into a
   register.  Returns an RTX to copy into the register, or NULL_RTX
   for a PARALLEL that cannot be converted into a CONST_VECTOR.  */
static rtx
aarch64_simd_make_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  rtx const_dup;
  rtx const_vec = NULL_RTX;
  int n_const = 0;
  int i;

  if (CONST_VECTOR_P (vals))
    const_vec = vals;
  else if (GET_CODE (vals) == PARALLEL)
    {
      /* A CONST_VECTOR must contain only CONST_INTs and
	 CONST_DOUBLEs, but CONSTANT_P allows more (e.g. SYMBOL_REF).
	 Only store valid constants in a CONST_VECTOR.  */
      int n_elts = XVECLEN (vals, 0);
      for (i = 0; i < n_elts; ++i)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	    n_const++;
	}
      if (n_const == n_elts)
	const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
    }
  else
    gcc_unreachable ();

  if (const_vec != NULL_RTX
      && aarch64_simd_valid_mov_imm (const_vec))
    /* Load using MOVI/MVNI.  */
    return const_vec;
  else if ((const_dup = aarch64_simd_dup_constant (vals)) != NULL_RTX)
    /* Loaded using DUP.  */
    return const_dup;
  else if (const_vec != NULL_RTX)
    /* Load from constant pool. We cannot take advantage of single-cycle
       LD1 because we need a PC-relative addressing mode.  */
    return const_vec;
  else
    /* A PARALLEL containing something not valid inside CONST_VECTOR.
       We cannot construct an initializer.  */
    return NULL_RTX;
}

/* A subroutine of aarch64_expand_vector_init, with the same interface.
   The caller has already tried a divide-and-conquer approach, so do
   not consider that case here.  */

void
aarch64_expand_vector_init_fallback (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode inner_mode = GET_MODE_INNER (mode);
  /* The number of vector elements.  */
  int n_elts = XVECLEN (vals, 0);
  /* The number of vector elements which are not constant.  */
  int n_var = 0;
  rtx any_const = NULL_RTX;
  /* The first element of vals.  */
  rtx v0 = XVECEXP (vals, 0, 0);
  bool all_same = true;

  /* This is a special vec_init<M><N> where N is not an element mode but a
     vector mode with half the elements of M.  We expect to find two entries
     of mode N in VALS and we must put their concatentation into TARGET.  */
  if (XVECLEN (vals, 0) == 2 && VECTOR_MODE_P (GET_MODE (XVECEXP (vals, 0, 0))))
    {
      machine_mode narrow_mode = GET_MODE (XVECEXP (vals, 0, 0));
      gcc_assert (GET_MODE_INNER (narrow_mode) == inner_mode
		  && known_eq (GET_MODE_SIZE (mode),
			       2 * GET_MODE_SIZE (narrow_mode)));
      emit_insn (gen_aarch64_vec_concat (narrow_mode, target,
					 XVECEXP (vals, 0, 0),
					 XVECEXP (vals, 0, 1)));
     return;
   }

  /* Count the number of variable elements to initialise.  */
  for (int i = 0; i < n_elts; ++i)
    {
      rtx x = XVECEXP (vals, 0, i);
      if (!(CONST_INT_P (x) || CONST_DOUBLE_P (x)))
	++n_var;
      else
	any_const = x;

      all_same &= rtx_equal_p (x, v0);
    }

  /* No variable elements, hand off to aarch64_simd_make_constant which knows
     how best to handle this.  */
  if (n_var == 0)
    {
      rtx constant = aarch64_simd_make_constant (vals);
      if (constant != NULL_RTX)
	{
	  emit_move_insn (target, constant);
	  return;
	}
    }

  /* Splat a single non-constant element if we can.  */
  if (all_same)
    {
      rtx x = force_reg (inner_mode, v0);
      aarch64_emit_move (target, gen_vec_duplicate (mode, x));
      return;
    }

  enum insn_code icode = optab_handler (vec_set_optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);

  /* If there are only variable elements, try to optimize
     the insertion using dup for the most common element
     followed by insertions.  */

  /* The algorithm will fill matches[*][0] with the earliest matching element,
     and matches[X][1] with the count of duplicate elements (if X is the
     earliest element which has duplicates).  */

  if (n_var >= n_elts - 1 && n_elts <= 16)
    {
      int matches[16][2] = {0};
      for (int i = 0; i < n_elts; i++)
	{
	  for (int j = 0; j <= i; j++)
	    {
	      if (rtx_equal_p (XVECEXP (vals, 0, i), XVECEXP (vals, 0, j)))
		{
		  matches[i][0] = j;
		  matches[j][1]++;
		  break;
		}
	    }
	}
      int maxelement = 0;
      int maxv = 0;
      rtx const_elem = NULL_RTX;
      int const_elem_pos = 0;

      for (int i = 0; i < n_elts; i++)
	{
	  if (matches[i][1] > maxv)
	    {
	      maxelement = i;
	      maxv = matches[i][1];
	    }
	  if (CONST_INT_P (XVECEXP (vals, 0, i))
	      || CONST_DOUBLE_P (XVECEXP (vals, 0, i)))
	    {
	      const_elem_pos = i;
	      const_elem = XVECEXP (vals, 0, i);
	    }
	}

      /* Create a duplicate of the most common element, unless all elements
	 are equally useless to us, in which case just immediately set the
	 vector register using the first element.  */

      if (maxv == 1)
	{
	  /* For vectors of two 64-bit elements, we can do even better.  */
	  if (n_elts == 2
	      && (inner_mode == E_DImode
		  || inner_mode == E_DFmode))

	    {
	      rtx x0 = XVECEXP (vals, 0, 0);
	      rtx x1 = XVECEXP (vals, 0, 1);
	      /* Combine can pick up this case, but handling it directly
		 here leaves clearer RTL.

		 This is load_pair_lanes<mode>, and also gives us a clean-up
		 for store_pair_lanes<mode>.  */
	      if (memory_operand (x0, inner_mode)
		  && memory_operand (x1, inner_mode)
		  && aarch64_mergeable_load_pair_p (mode, x0, x1))
		{
		  rtx t;
		  if (inner_mode == DFmode)
		    t = gen_load_pair_lanesdf (target, x0, x1);
		  else
		    t = gen_load_pair_lanesdi (target, x0, x1);
		  emit_insn (t);
		  return;
		}
	    }
	  /* The subreg-move sequence below will move into lane zero of the
	     vector register.  For big-endian we want that position to hold
	     the last element of VALS.  */
	  maxelement = BYTES_BIG_ENDIAN ? n_elts - 1 : 0;

	  /* If we have a single constant element, use that for duplicating
	     instead.  */
	  if (const_elem)
	    {
	      maxelement = const_elem_pos;
	      aarch64_emit_move (target, gen_vec_duplicate (mode, const_elem));
	    }
	  else
	    {
	      rtx x = force_reg (inner_mode, XVECEXP (vals, 0, maxelement));
	      aarch64_emit_move (target, lowpart_subreg (mode, x, inner_mode));
	    }
	}
      else
	{
	  rtx x = force_reg (inner_mode, XVECEXP (vals, 0, maxelement));
	  aarch64_emit_move (target, gen_vec_duplicate (mode, x));
	}

      /* Insert the rest.  */
      for (int i = 0; i < n_elts; i++)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (matches[i][0] == maxelement)
	    continue;
	  x = force_reg (inner_mode, x);
	  emit_insn (GEN_FCN (icode) (target, x, GEN_INT (i)));
	}
      return;
    }

  /* Initialise a vector which is part-variable.  We want to first try
     to build those lanes which are constant in the most efficient way we
     can.  */
  if (n_var != n_elts)
    {
      rtx copy = copy_rtx (vals);

      /* Load constant part of vector.  We really don't care what goes into the
	 parts we will overwrite, but we're more likely to be able to load the
	 constant efficiently if it has fewer, larger, repeating parts
	 (see aarch64_simd_valid_imm).  */
      for (int i = 0; i < n_elts; i++)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	    continue;
	  rtx subst = any_const;
	  for (int bit = n_elts / 2; bit > 0; bit /= 2)
	    {
	      /* Look in the copied vector, as more elements are const.  */
	      rtx test = XVECEXP (copy, 0, i ^ bit);
	      if (CONST_INT_P (test) || CONST_DOUBLE_P (test))
		{
		  subst = test;
		  break;
		}
	    }
	  XVECEXP (copy, 0, i) = subst;
	}
      aarch64_expand_vector_init_fallback (target, copy);
    }

  /* Insert the variable lanes directly.  */
  for (int i = 0; i < n_elts; i++)
    {
      rtx x = XVECEXP (vals, 0, i);
      if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	continue;
      x = force_reg (inner_mode, x);
      emit_insn (GEN_FCN (icode) (target, x, GEN_INT (i)));
    }
}

/* Return even or odd half of VALS depending on EVEN_P.  */

static rtx
aarch64_unzip_vector_init (machine_mode mode, rtx vals, bool even_p)
{
  int n = XVECLEN (vals, 0);
  machine_mode new_mode
    = aarch64_simd_container_mode (GET_MODE_INNER (mode),
				   GET_MODE_BITSIZE (mode).to_constant () / 2);
  rtvec vec = rtvec_alloc (n / 2);
  for (int i = 0; i < n / 2; i++)
    RTVEC_ELT (vec, i) = (even_p) ? XVECEXP (vals, 0, 2 * i)
				  : XVECEXP (vals, 0, 2 * i + 1);
  return gen_rtx_PARALLEL (new_mode, vec);
}

/* Return true if SET is a scalar move.  */

static bool
scalar_move_insn_p (rtx set)
{
  rtx src = SET_SRC (set);
  rtx dest = SET_DEST (set);
  return (is_a<scalar_mode> (GET_MODE (dest))
	  && aarch64_mov_operand (src, GET_MODE (dest)));
}

/* Similar to seq_cost, but ignore cost for scalar moves.  */

static unsigned
seq_cost_ignoring_scalar_moves (const rtx_insn *seq, bool speed)
{
  unsigned cost = 0;

  for (; seq; seq = NEXT_INSN (seq))
    if (NONDEBUG_INSN_P (seq))
      {
	if (rtx set = single_set (seq))
	  {
	    if (!scalar_move_insn_p (set))
	      cost += set_rtx_cost (set, speed);
	  }
	else
	  {
	    int this_cost = insn_cost (CONST_CAST_RTX_INSN (seq), speed);
	    if (this_cost > 0)
	      cost += this_cost;
	    else
	      cost++;
	  }
      }

  return cost;
}

/* Expand a vector initialization sequence, such that TARGET is
   initialized to contain VALS.  */

void
aarch64_expand_vector_init (rtx target, rtx vals)
{
  /* Try decomposing the initializer into even and odd halves and
     then ZIP them together.  Use the resulting sequence if it is
     strictly cheaper than loading VALS directly.

     Prefer the fallback sequence in the event of a tie, since it
     will tend to use fewer registers.  */

  machine_mode mode = GET_MODE (target);
  int n_elts = XVECLEN (vals, 0);

  if (n_elts < 4
      || maybe_ne (GET_MODE_BITSIZE (mode), 128))
    {
      aarch64_expand_vector_init_fallback (target, vals);
      return;
    }

  start_sequence ();
  rtx halves[2];
  unsigned costs[2];
  for (int i = 0; i < 2; i++)
    {
      start_sequence ();
      rtx new_vals = aarch64_unzip_vector_init (mode, vals, i == 0);
      rtx tmp_reg = gen_reg_rtx (GET_MODE (new_vals));
      aarch64_expand_vector_init (tmp_reg, new_vals);
      halves[i] = gen_rtx_SUBREG (mode, tmp_reg, 0);
      rtx_insn *rec_seq = get_insns ();
      end_sequence ();
      costs[i] = seq_cost_ignoring_scalar_moves (rec_seq, !optimize_size);
      emit_insn (rec_seq);
    }

  rtvec v = gen_rtvec (2, halves[0], halves[1]);
  rtx_insn *zip1_insn
    = emit_set_insn (target, gen_rtx_UNSPEC (mode, v, UNSPEC_ZIP1));
  unsigned seq_total_cost
    = (!optimize_size) ? std::max (costs[0], costs[1]) : costs[0] + costs[1];
  seq_total_cost += insn_cost (zip1_insn, !optimize_size);

  rtx_insn *seq = get_insns ();
  end_sequence ();

  start_sequence ();
  aarch64_expand_vector_init_fallback (target, vals);
  rtx_insn *fallback_seq = get_insns ();
  unsigned fallback_seq_cost
    = seq_cost_ignoring_scalar_moves (fallback_seq, !optimize_size);
  end_sequence ();

  emit_insn (seq_total_cost < fallback_seq_cost ? seq : fallback_seq);
}

/* Emit RTL corresponding to:
   insr TARGET, ELEM.  */

static void
emit_insr (rtx target, rtx elem)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);
  elem = force_reg (elem_mode, elem);

  insn_code icode = optab_handler (vec_shl_insert_optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);
  emit_insn (GEN_FCN (icode) (target, target, elem));
}

/* Subroutine of aarch64_sve_expand_vector_init for handling
   trailing constants.
   This function works as follows:
   (a) Create a new vector consisting of trailing constants.
   (b) Initialize TARGET with the constant vector using emit_move_insn.
   (c) Insert remaining elements in TARGET using insr.
   NELTS is the total number of elements in original vector while
   while NELTS_REQD is the number of elements that are actually
   significant.

   ??? The heuristic used is to do above only if number of constants
   is at least half the total number of elements.  May need fine tuning.  */

static bool
aarch64_sve_expand_vector_init_handle_trailing_constants
 (rtx target, const rtx_vector_builder &builder, int nelts, int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);
  int n_trailing_constants = 0;

  for (int i = nelts_reqd - 1;
       i >= 0 && valid_for_const_vector_p (elem_mode, builder.elt (i));
       i--)
    n_trailing_constants++;

  if (n_trailing_constants >= nelts_reqd / 2)
    {
      /* Try to use the natural pattern of BUILDER to extend the trailing
	 constant elements to a full vector.  Replace any variables in the
	 extra elements with zeros.

	 ??? It would be better if the builders supported "don't care"
	     elements, with the builder filling in whichever elements
	     give the most compact encoding.  */
      rtx_vector_builder v (mode, nelts, 1);
      for (int i = 0; i < nelts; i++)
	{
	  rtx x = builder.elt (i + nelts_reqd - n_trailing_constants);
	  if (!valid_for_const_vector_p (elem_mode, x))
	    x = CONST0_RTX (elem_mode);
	  v.quick_push (x);
	}
      rtx const_vec = v.build ();
      emit_move_insn (target, const_vec);

      for (int i = nelts_reqd - n_trailing_constants - 1; i >= 0; i--)
	emit_insr (target, builder.elt (i));

      return true;
    }

  return false;
}

/* Subroutine of aarch64_sve_expand_vector_init.
   Works as follows:
   (a) Initialize TARGET by broadcasting element NELTS_REQD - 1 of BUILDER.
   (b) Skip trailing elements from BUILDER, which are the same as
       element NELTS_REQD - 1.
   (c) Insert earlier elements in reverse order in TARGET using insr.  */

static void
aarch64_sve_expand_vector_init_insert_elems (rtx target,
					     const rtx_vector_builder &builder,
					     int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);

  struct expand_operand ops[2];
  enum insn_code icode = optab_handler (vec_duplicate_optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);

  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], builder.elt (nelts_reqd - 1), elem_mode);
  expand_insn (icode, 2, ops);

  int ndups = builder.count_dups (nelts_reqd - 1, -1, -1);
  for (int i = nelts_reqd - ndups - 1; i >= 0; i--)
    emit_insr (target, builder.elt (i));
}

/* Subroutine of aarch64_sve_expand_vector_init to handle case
   when all trailing elements of builder are same.
   This works as follows:
   (a) Use expand_insn interface to broadcast last vector element in TARGET.
   (b) Insert remaining elements in TARGET using insr.

   ??? The heuristic used is to do above if number of same trailing elements
   is at least 3/4 of total number of elements, loosely based on
   heuristic from mostly_zeros_p.  May need fine-tuning.  */

static bool
aarch64_sve_expand_vector_init_handle_trailing_same_elem
 (rtx target, const rtx_vector_builder &builder, int nelts_reqd)
{
  int ndups = builder.count_dups (nelts_reqd - 1, -1, -1);
  if (ndups >= (3 * nelts_reqd) / 4)
    {
      aarch64_sve_expand_vector_init_insert_elems (target, builder,
						   nelts_reqd - ndups + 1);
      return true;
    }

  return false;
}

/* Initialize register TARGET from BUILDER. NELTS is the constant number
   of elements in BUILDER.

   The function tries to initialize TARGET from BUILDER if it fits one
   of the special cases outlined below.

   Failing that, the function divides BUILDER into two sub-vectors:
   v_even = even elements of BUILDER;
   v_odd = odd elements of BUILDER;

   and recursively calls itself with v_even and v_odd.

   if (recursive call succeeded for v_even or v_odd)
     TARGET = zip (v_even, v_odd)

   The function returns true if it managed to build TARGET from BUILDER
   with one of the special cases, false otherwise.

   Example: {a, 1, b, 2, c, 3, d, 4}

   The vector gets divided into:
   v_even = {a, b, c, d}
   v_odd = {1, 2, 3, 4}

   aarch64_sve_expand_vector_init(v_odd) hits case 1 and
   initialize tmp2 from constant vector v_odd using emit_move_insn.

   aarch64_sve_expand_vector_init(v_even) fails since v_even contains
   4 elements, so we construct tmp1 from v_even using insr:
   tmp1 = dup(d)
   insr tmp1, c
   insr tmp1, b
   insr tmp1, a

   And finally:
   TARGET = zip (tmp1, tmp2)
   which sets TARGET to {a, 1, b, 2, c, 3, d, 4}.  */

static bool
aarch64_sve_expand_vector_init (rtx target, const rtx_vector_builder &builder,
				int nelts, int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);

  /* Case 1: Vector contains trailing constants.  */

  if (aarch64_sve_expand_vector_init_handle_trailing_constants
       (target, builder, nelts, nelts_reqd))
    return true;

  /* Case 2: Vector contains leading constants.  */

  rtx_vector_builder rev_builder (mode, nelts_reqd, 1);
  for (int i = 0; i < nelts_reqd; i++)
    rev_builder.quick_push (builder.elt (nelts_reqd - i - 1));
  rev_builder.finalize ();

  if (aarch64_sve_expand_vector_init_handle_trailing_constants
       (target, rev_builder, nelts, nelts_reqd))
    {
      emit_insn (gen_aarch64_sve_rev (mode, target, target));
      return true;
    }

  /* Case 3: Vector contains trailing same element.  */

  if (aarch64_sve_expand_vector_init_handle_trailing_same_elem
       (target, builder, nelts_reqd))
    return true;

  /* Case 4: Vector contains leading same element.  */

  if (aarch64_sve_expand_vector_init_handle_trailing_same_elem
       (target, rev_builder, nelts_reqd) && nelts_reqd == nelts)
    {
      emit_insn (gen_aarch64_sve_rev (mode, target, target));
      return true;
    }

  /* Avoid recursing below 4-elements.
     ??? The threshold 4 may need fine-tuning.  */

  if (nelts_reqd <= 4)
    return false;

  rtx_vector_builder v_even (mode, nelts, 1);
  rtx_vector_builder v_odd (mode, nelts, 1);

  for (int i = 0; i < nelts * 2; i += 2)
    {
      v_even.quick_push (builder.elt (i));
      v_odd.quick_push (builder.elt (i + 1));
    }

  v_even.finalize ();
  v_odd.finalize ();

  rtx tmp1 = gen_reg_rtx (mode);
  bool did_even_p = aarch64_sve_expand_vector_init (tmp1, v_even,
						    nelts, nelts_reqd / 2);

  rtx tmp2 = gen_reg_rtx (mode);
  bool did_odd_p = aarch64_sve_expand_vector_init (tmp2, v_odd,
						   nelts, nelts_reqd / 2);

  if (!did_even_p && !did_odd_p)
    return false;

  /* Initialize v_even and v_odd using INSR if it didn't match any of the
     special cases and zip v_even, v_odd.  */

  if (!did_even_p)
    aarch64_sve_expand_vector_init_insert_elems (tmp1, v_even, nelts_reqd / 2);

  if (!did_odd_p)
    aarch64_sve_expand_vector_init_insert_elems (tmp2, v_odd, nelts_reqd / 2);

  rtvec v = gen_rtvec (2, tmp1, tmp2);
  emit_set_insn (target, gen_rtx_UNSPEC (mode, v, UNSPEC_ZIP1));
  return true;
}

/* Initialize register TARGET from the elements in PARALLEL rtx VALS.  */

void
aarch64_sve_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  int nelts = XVECLEN (vals, 0);

  rtx_vector_builder v (mode, nelts, 1);
  for (int i = 0; i < nelts; i++)
    v.quick_push (XVECEXP (vals, 0, i));
  v.finalize ();

  /* If neither sub-vectors of v could be initialized specially,
     then use INSR to insert all elements from v into TARGET.
     ??? This might not be optimal for vectors with large
     initializers like 16-element or above.
     For nelts < 4, it probably isn't useful to handle specially.  */

  if (nelts < 4
      || !aarch64_sve_expand_vector_init (target, v, nelts, nelts))
    aarch64_sve_expand_vector_init_insert_elems (target, v, nelts);
}

/* Check whether VALUE is a vector constant in which every element
   is either a power of 2 or a negated power of 2.  If so, return
   a constant vector of log2s, and flip CODE between PLUS and MINUS
   if VALUE contains negated powers of 2.  Return NULL_RTX otherwise.  */

static rtx
aarch64_convert_mult_to_shift (rtx value, rtx_code &code)
{
  if (!CONST_VECTOR_P (value))
    return NULL_RTX;

  rtx_vector_builder builder;
  if (!builder.new_unary_operation (GET_MODE (value), value, false))
    return NULL_RTX;

  scalar_mode int_mode = GET_MODE_INNER (GET_MODE (value));
  /* 1 if the result of the multiplication must be negated,
     0 if it mustn't, or -1 if we don't yet care.  */
  int negate = -1;
  unsigned int encoded_nelts = const_vector_encoded_nelts (value);
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    {
      rtx elt = CONST_VECTOR_ENCODED_ELT (value, i);
      if (!CONST_SCALAR_INT_P (elt))
	return NULL_RTX;
      rtx_mode_t val (elt, int_mode);
      wide_int pow2 = wi::neg (val);
      if (val != pow2)
	{
	  /* It matters whether we negate or not.  Make that choice,
	     and make sure that it's consistent with previous elements.  */
	  if (negate == !wi::neg_p (val))
	    return NULL_RTX;
	  negate = wi::neg_p (val);
	  if (!negate)
	    pow2 = val;
	}
      /* POW2 is now the value that we want to be a power of 2.  */
      int shift = wi::exact_log2 (pow2);
      if (shift < 0)
	return NULL_RTX;
      builder.quick_push (gen_int_mode (shift, int_mode));
    }
  if (negate == -1)
    /* PLUS and MINUS are equivalent; canonicalize on PLUS.  */
    code = PLUS;
  else if (negate == 1)
    code = code == PLUS ? MINUS : PLUS;
  return builder.build ();
}

/* Prepare for an integer SVE multiply-add or multiply-subtract pattern;
   CODE is PLUS for the former and MINUS for the latter.  OPERANDS is the
   operands array, in the same order as for fma_optab.  Return true if
   the function emitted all the necessary instructions, false if the caller
   should generate the pattern normally with the new OPERANDS array.  */

bool
aarch64_prepare_sve_int_fma (rtx *operands, rtx_code code)
{
  machine_mode mode = GET_MODE (operands[0]);
  if (rtx shifts = aarch64_convert_mult_to_shift (operands[2], code))
    {
      rtx product = expand_binop (mode, vashl_optab, operands[1], shifts,
				  NULL_RTX, true, OPTAB_DIRECT);
      force_expand_binop (mode, code == PLUS ? add_optab : sub_optab,
			  operands[3], product, operands[0], true,
			  OPTAB_DIRECT);
      return true;
    }
  operands[2] = force_reg (mode, operands[2]);
  return false;
}

/* Likewise, but for a conditional pattern.  */

bool
aarch64_prepare_sve_cond_int_fma (rtx *operands, rtx_code code)
{
  machine_mode mode = GET_MODE (operands[0]);
  if (rtx shifts = aarch64_convert_mult_to_shift (operands[3], code))
    {
      rtx product = expand_binop (mode, vashl_optab, operands[2], shifts,
				  NULL_RTX, true, OPTAB_DIRECT);
      emit_insn (gen_cond (code, mode, operands[0], operands[1],
			   operands[4], product, operands[5]));
      return true;
    }
  operands[3] = force_reg (mode, operands[3]);
  return false;
}

static unsigned HOST_WIDE_INT
aarch64_shift_truncation_mask (machine_mode mode)
{
  if (!SHIFT_COUNT_TRUNCATED || aarch64_vector_data_mode_p (mode))
    return 0;
  return GET_MODE_UNIT_BITSIZE (mode) - 1;
}

/* Select a format to encode pointers in exception handling data.  */
int
aarch64_asm_preferred_eh_data_format (int code ATTRIBUTE_UNUSED, int global)
{
   int type;
   switch (aarch64_cmodel)
     {
     case AARCH64_CMODEL_TINY:
     case AARCH64_CMODEL_TINY_PIC:
     case AARCH64_CMODEL_SMALL:
     case AARCH64_CMODEL_SMALL_PIC:
     case AARCH64_CMODEL_SMALL_SPIC:
       /* text+got+data < 4Gb.  4-byte signed relocs are sufficient
	  for everything.  */
       type = DW_EH_PE_sdata4;
       break;
     default:
       /* No assumptions here.  8-byte relocs required.  */
       type = DW_EH_PE_sdata8;
       break;
     }
   return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
}

/* Output .variant_pcs for aarch64_vector_pcs function symbols.  */

static void
aarch64_asm_output_variant_pcs (FILE *stream, const tree decl, const char* name)
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      arm_pcs pcs = (arm_pcs) fndecl_abi (decl).id ();
      if (pcs == ARM_PCS_SIMD || pcs == ARM_PCS_SVE)
	{
	  fprintf (stream, "\t.variant_pcs\t");
	  assemble_name (stream, name);
	  fprintf (stream, "\n");
	}
    }
}

/* The last .arch and .tune assembly strings that we printed.  */
static std::string aarch64_last_printed_arch_string;
static std::string aarch64_last_printed_tune_string;

/* Implement ASM_DECLARE_FUNCTION_NAME.  Output the ISA features used
   by the function fndecl.  */

void
aarch64_declare_function_name (FILE *stream, const char* name,
				tree fndecl)
{
  tree target_parts = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  struct cl_target_option *targ_options;
  if (target_parts)
    targ_options = TREE_TARGET_OPTION (target_parts);
  else
    targ_options = TREE_TARGET_OPTION (target_option_current_node);
  gcc_assert (targ_options);

  const struct processor *this_arch
    = aarch64_get_arch (targ_options->x_selected_arch);

  auto isa_flags = aarch64_get_asm_isa_flags (targ_options);
  std::string extension
    = aarch64_get_extension_string_for_isa_flags (isa_flags,
						  this_arch->flags);
  /* Only update the assembler .arch string if it is distinct from the last
     such string we printed.  */
  std::string to_print = this_arch->name + extension;
  if (to_print != aarch64_last_printed_arch_string)
    {
      asm_fprintf (asm_out_file, "\t.arch %s\n", to_print.c_str ());
      aarch64_last_printed_arch_string = to_print;
    }

  /* Print the cpu name we're tuning for in the comments, might be
     useful to readers of the generated asm.  Do it only when it changes
     from function to function and verbose assembly is requested.  */
  const struct processor *this_tune
    = aarch64_get_tune_cpu (targ_options->x_selected_tune);

  if (flag_debug_asm && aarch64_last_printed_tune_string != this_tune->name)
    {
      asm_fprintf (asm_out_file, "\t" ASM_COMMENT_START ".tune %s\n",
		   this_tune->name);
      aarch64_last_printed_tune_string = this_tune->name;
    }

  aarch64_asm_output_variant_pcs (stream, fndecl, name);

  /* Don't forget the type directive for ELF.  */
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "function");
  ASM_OUTPUT_FUNCTION_LABEL (stream, name, fndecl);

  cfun->machine->label_is_assembled = true;
}

/* Implement PRINT_PATCHABLE_FUNCTION_ENTRY.  */

void
aarch64_print_patchable_function_entry (FILE *file,
					unsigned HOST_WIDE_INT patch_area_size,
					bool record_p)
{
  if (!cfun->machine->label_is_assembled)
    {
      /* Emit the patching area before the entry label, if any.  */
      default_print_patchable_function_entry (file, patch_area_size,
					      record_p);
      return;
    }

  rtx pa = gen_patchable_area (GEN_INT (patch_area_size),
			       GEN_INT (record_p));
  basic_block bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;

  if (!aarch_bti_enabled ()
      || cgraph_node::get (cfun->decl)->only_called_directly_p ())
    {
      /* Emit the patchable_area at the beginning of the function.  */
      rtx_insn *insn = emit_insn_before (pa, BB_HEAD (bb));
      INSN_ADDRESSES_NEW (insn, -1);
      return;
    }

  rtx_insn *insn = next_real_nondebug_insn (get_insns ());
  if (!insn
      || !INSN_P (insn)
      || GET_CODE (PATTERN (insn)) != UNSPEC_VOLATILE
      || XINT (PATTERN (insn), 1) != UNSPECV_BTI_C)
    {
      /* Emit a BTI_C.  */
      insn = emit_insn_before (gen_bti_c (), BB_HEAD (bb));
    }

  /* Emit the patchable_area after BTI_C.  */
  insn = emit_insn_after (pa, insn);
  INSN_ADDRESSES_NEW (insn, -1);
}

/* Output patchable area.  */

void
aarch64_output_patchable_area (unsigned int patch_area_size, bool record_p)
{
  default_print_patchable_function_entry (asm_out_file, patch_area_size,
					  record_p);
}

/* Implement ASM_OUTPUT_DEF_FROM_DECLS.  Output .variant_pcs for aliases.  */

void
aarch64_asm_output_alias (FILE *stream, const tree decl, const tree target)
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  const char *value = IDENTIFIER_POINTER (target);
  aarch64_asm_output_variant_pcs (stream, decl, name);
  ASM_OUTPUT_DEF (stream, name, value);
}

/* Implement ASM_OUTPUT_EXTERNAL.  Output .variant_pcs for undefined
   function symbol references.  */

void
aarch64_asm_output_external (FILE *stream, tree decl, const char* name)
{
  default_elf_asm_output_external (stream, decl, name);
  aarch64_asm_output_variant_pcs (stream, decl, name);
}

/* Triggered after a .cfi_startproc directive is emitted into the assembly file.
   Used to output the .cfi_b_key_frame directive when signing the current
   function with the B key.  */

void
aarch64_post_cfi_startproc (FILE *f, tree ignored ATTRIBUTE_UNUSED)
{
  if (cfun->machine->frame.laid_out && aarch64_return_address_signing_enabled ()
      && aarch64_ra_sign_key == AARCH64_KEY_B)
	asm_fprintf (f, "\t.cfi_b_key_frame\n");
}

/* Implements TARGET_ASM_FILE_START.  Output the assembly header.  */

static void
aarch64_start_file (void)
{
  struct cl_target_option *default_options
    = TREE_TARGET_OPTION (target_option_default_node);

  const struct processor *default_arch
    = aarch64_get_arch (default_options->x_selected_arch);
  auto default_isa_flags = aarch64_get_asm_isa_flags (default_options);
  std::string extension
    = aarch64_get_extension_string_for_isa_flags (default_isa_flags,
						  default_arch->flags);

   aarch64_last_printed_arch_string = default_arch->name + extension;
   aarch64_last_printed_tune_string = "";
   asm_fprintf (asm_out_file, "\t.arch %s\n",
		aarch64_last_printed_arch_string.c_str ());

   default_file_start ();
}

/* Emit load exclusive.  */

static void
aarch64_emit_load_exclusive (machine_mode mode, rtx rval,
			     rtx mem, rtx model_rtx)
{
  if (mode == TImode)
    emit_insn (gen_aarch64_load_exclusive_pair (gen_lowpart (DImode, rval),
						gen_highpart (DImode, rval),
						mem, model_rtx));
  else
    emit_insn (gen_aarch64_load_exclusive (mode, rval, mem, model_rtx));
}

/* Emit store exclusive.  */

static void
aarch64_emit_store_exclusive (machine_mode mode, rtx bval,
			      rtx mem, rtx rval, rtx model_rtx)
{
  if (mode == TImode)
    emit_insn (gen_aarch64_store_exclusive_pair
	       (bval, mem, operand_subword (rval, 0, 0, TImode),
		operand_subword (rval, 1, 0, TImode), model_rtx));
  else
    emit_insn (gen_aarch64_store_exclusive (mode, bval, mem, rval, model_rtx));
}

/* Mark the previous jump instruction as unlikely.  */

static void
aarch64_emit_unlikely_jump (rtx insn)
{
  rtx_insn *jump = emit_jump_insn (insn);
  add_reg_br_prob_note (jump, profile_probability::very_unlikely ());
}

/* We store the names of the various atomic helpers in a 5x5 array.
   Return the libcall function given MODE, MODEL and NAMES.  */

rtx
aarch64_atomic_ool_func(machine_mode mode, rtx model_rtx,
			const atomic_ool_names *names)
{
  memmodel model = memmodel_from_int (INTVAL (model_rtx));
  int mode_idx, model_idx;

  switch (mode)
    {
    case E_QImode:
      mode_idx = 0;
      break;
    case E_HImode:
      mode_idx = 1;
      break;
    case E_SImode:
      mode_idx = 2;
      break;
    case E_DImode:
      mode_idx = 3;
      break;
    case E_TImode:
      mode_idx = 4;
      break;
    default:
      gcc_unreachable ();
    }

  switch (model)
    {
    case MEMMODEL_RELAXED:
      model_idx = 0;
      break;
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
      model_idx = 1;
      break;
    case MEMMODEL_RELEASE:
      model_idx = 2;
      break;
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      model_idx = 3;
      break;
    case MEMMODEL_SYNC_ACQUIRE:
    case MEMMODEL_SYNC_RELEASE:
    case MEMMODEL_SYNC_SEQ_CST:
      model_idx = 4;
      break;
    default:
      gcc_unreachable ();
    }

  return init_one_libfunc_visibility (names->str[mode_idx][model_idx],
				      VISIBILITY_HIDDEN);
}

#define DEF0(B, N) \
  { "__aarch64_" #B #N "_relax", \
    "__aarch64_" #B #N "_acq", \
    "__aarch64_" #B #N "_rel", \
    "__aarch64_" #B #N "_acq_rel", \
    "__aarch64_" #B #N "_sync" }

#define DEF4(B)  DEF0(B, 1), DEF0(B, 2), DEF0(B, 4), DEF0(B, 8), \
		 { NULL, NULL, NULL, NULL }
#define DEF5(B)  DEF0(B, 1), DEF0(B, 2), DEF0(B, 4), DEF0(B, 8), DEF0(B, 16)

static const atomic_ool_names aarch64_ool_cas_names = { { DEF5(cas) } };
const atomic_ool_names aarch64_ool_swp_names = { { DEF4(swp) } };
const atomic_ool_names aarch64_ool_ldadd_names = { { DEF4(ldadd) } };
const atomic_ool_names aarch64_ool_ldset_names = { { DEF4(ldset) } };
const atomic_ool_names aarch64_ool_ldclr_names = { { DEF4(ldclr) } };
const atomic_ool_names aarch64_ool_ldeor_names = { { DEF4(ldeor) } };

#undef DEF0
#undef DEF4
#undef DEF5

/* Expand a compare and swap pattern.  */

void
aarch64_expand_compare_and_swap (rtx operands[])
{
  rtx bval, rval, mem, oldval, newval, is_weak, mod_s, mod_f, x, cc_reg;
  machine_mode mode, r_mode;

  bval = operands[0];
  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);

  /* Normally the succ memory model must be stronger than fail, but in the
     unlikely event of fail being ACQUIRE and succ being RELEASE we need to
     promote succ to ACQ_REL so that we don't lose the acquire semantics.  */
  if (is_mm_acquire (memmodel_from_int (INTVAL (mod_f)))
      && is_mm_release (memmodel_from_int (INTVAL (mod_s))))
    mod_s = GEN_INT (MEMMODEL_ACQ_REL);

  r_mode = mode;
  if (mode == QImode || mode == HImode)
    {
      r_mode = SImode;
      rval = gen_reg_rtx (r_mode);
    }

  if (TARGET_LSE)
    {
      /* The CAS insn requires oldval and rval overlap, but we need to
	 have a copy of oldval saved across the operation to tell if
	 the operation is successful.  */
      if (reg_overlap_mentioned_p (rval, oldval))
        rval = copy_to_mode_reg (r_mode, oldval);
      else
	emit_move_insn (rval, gen_lowpart (r_mode, oldval));
      if (mode == TImode)
	newval = force_reg (mode, newval);

      emit_insn (gen_aarch64_compare_and_swap_lse (mode, rval, mem,
						   newval, mod_s));
      cc_reg = aarch64_gen_compare_reg_maybe_ze (NE, rval, oldval, mode);
    }
  else if (TARGET_OUTLINE_ATOMICS)
    {
      /* Oldval must satisfy compare afterward.  */
      if (!aarch64_plus_operand (oldval, mode))
	oldval = force_reg (mode, oldval);
      rtx func = aarch64_atomic_ool_func (mode, mod_s, &aarch64_ool_cas_names);
      rval = emit_library_call_value (func, NULL_RTX, LCT_NORMAL, r_mode,
				      oldval, mode, newval, mode,
				      XEXP (mem, 0), Pmode);
      cc_reg = aarch64_gen_compare_reg_maybe_ze (NE, rval, oldval, mode);
    }
  else
    {
      /* The oldval predicate varies by mode.  Test it and force to reg.  */
      insn_code code = code_for_aarch64_compare_and_swap (mode);
      if (!insn_data[code].operand[2].predicate (oldval, mode))
	oldval = force_reg (mode, oldval);

      emit_insn (GEN_FCN (code) (rval, mem, oldval, newval,
				 is_weak, mod_s, mod_f));
      cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);
    }

  if (r_mode != mode)
    rval = gen_lowpart (mode, rval);
  emit_move_insn (operands[1], rval);

  x = gen_rtx_EQ (SImode, cc_reg, const0_rtx);
  emit_insn (gen_rtx_SET (bval, x));
}

/* Emit a barrier, that is appropriate for memory model MODEL, at the end of a
   sequence implementing an atomic operation.  */

static void
aarch64_emit_post_barrier (enum memmodel model)
{
  const enum memmodel base_model = memmodel_base (model);

  if (is_mm_sync (model)
      && (base_model == MEMMODEL_ACQUIRE
	  || base_model == MEMMODEL_ACQ_REL
	  || base_model == MEMMODEL_SEQ_CST))
    {
      emit_insn (gen_mem_thread_fence (GEN_INT (MEMMODEL_SEQ_CST)));
    }
}

/* Split a compare and swap pattern.  */

void
aarch64_split_compare_and_swap (rtx operands[])
{
  /* Split after prolog/epilog to avoid interactions with shrinkwrapping.  */
  gcc_assert (epilogue_completed);

  rtx rval, mem, oldval, newval, scratch, x, model_rtx;
  machine_mode mode;
  bool is_weak;
  rtx_code_label *label1, *label2;
  enum memmodel model;

  rval = operands[0];
  mem = operands[1];
  oldval = operands[2];
  newval = operands[3];
  model_rtx = operands[5];
  scratch = operands[7];
  mode = GET_MODE (mem);
  model = memmodel_from_int (INTVAL (model_rtx));
  is_weak = operands[4] != const0_rtx && mode != TImode;

  /* When OLDVAL is zero and we want the strong version we can emit a tighter
    loop:
    .label1:
	LD[A]XR	rval, [mem]
	CBNZ	rval, .label2
	ST[L]XR	scratch, newval, [mem]
	CBNZ	scratch, .label1
    .label2:
	CMP	rval, 0.  */
  bool strong_zero_p = (!is_weak && !aarch64_track_speculation &&
			oldval == const0_rtx && mode != TImode);

  label1 = NULL;
  if (!is_weak)
    {
      label1 = gen_label_rtx ();
      emit_label (label1);
    }
  label2 = gen_label_rtx ();

  /* The initial load can be relaxed for a __sync operation since a final
     barrier will be emitted to stop code hoisting.  */
  if (is_mm_sync (model))
    aarch64_emit_load_exclusive (mode, rval, mem, GEN_INT (MEMMODEL_RELAXED));
  else
    aarch64_emit_load_exclusive (mode, rval, mem, model_rtx);

  if (strong_zero_p)
    x = gen_rtx_NE (VOIDmode, rval, const0_rtx);
  else
    {
      rtx cc_reg = aarch64_gen_compare_reg_maybe_ze (NE, rval, oldval, mode);
      x = gen_rtx_NE (VOIDmode, cc_reg, const0_rtx);
    }
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label2), pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  aarch64_emit_store_exclusive (mode, scratch, mem, newval, model_rtx);

  if (!is_weak)
    {
      x = aarch64_gen_compare_zero_and_branch (NE, scratch, label1);
      aarch64_emit_unlikely_jump (x);
    }
  else
    aarch64_gen_compare_reg (NE, scratch, const0_rtx);

  /* 128-bit LDAXP is not atomic unless STLXP succeeds.  So for a mismatch,
     store the returned value and loop if the STLXP fails.  */
  if (mode == TImode)
    {
      rtx_code_label *label3 = gen_label_rtx ();
      emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (Pmode, label3)));
      emit_barrier ();

      emit_label (label2);
      aarch64_emit_store_exclusive (mode, scratch, mem, rval, model_rtx);

      x = aarch64_gen_compare_zero_and_branch (NE, scratch, label1);
      aarch64_emit_unlikely_jump (x);

      label2 = label3;
    }

  emit_label (label2);

  /* If we used a CBNZ in the exchange loop emit an explicit compare with RVAL
     to set the condition flags.  If this is not used it will be removed by
     later passes.  */
  if (strong_zero_p)
    aarch64_gen_compare_reg (NE, rval, const0_rtx);

  /* Emit any final barrier needed for a __sync operation.  */
  if (is_mm_sync (model))
    aarch64_emit_post_barrier (model);
}

/* Split an atomic operation.  */

void
aarch64_split_atomic_op (enum rtx_code code, rtx old_out, rtx new_out, rtx mem,
			 rtx value, rtx model_rtx, rtx cond)
{
  /* Split after prolog/epilog to avoid interactions with shrinkwrapping.  */
  gcc_assert (epilogue_completed);

  machine_mode mode = GET_MODE (mem);
  machine_mode wmode = (mode == DImode ? DImode : SImode);
  const enum memmodel model = memmodel_from_int (INTVAL (model_rtx));
  const bool is_sync = is_mm_sync (model);
  rtx_code_label *label;
  rtx x;

  /* Split the atomic operation into a sequence.  */
  label = gen_label_rtx ();
  emit_label (label);

  if (new_out)
    new_out = gen_lowpart (wmode, new_out);
  if (old_out)
    old_out = gen_lowpart (wmode, old_out);
  else
    old_out = new_out;
  value = simplify_gen_subreg (wmode, value, mode, 0);

  /* The initial load can be relaxed for a __sync operation since a final
     barrier will be emitted to stop code hoisting.  */
 if (is_sync)
    aarch64_emit_load_exclusive (mode, old_out, mem,
				 GEN_INT (MEMMODEL_RELAXED));
  else
    aarch64_emit_load_exclusive (mode, old_out, mem, model_rtx);

  switch (code)
    {
    case SET:
      new_out = value;
      break;

    case NOT:
      x = gen_rtx_AND (wmode, old_out, value);
      emit_insn (gen_rtx_SET (new_out, x));
      x = gen_rtx_NOT (wmode, new_out);
      emit_insn (gen_rtx_SET (new_out, x));
      break;

    case MINUS:
      if (CONST_INT_P (value))
	{
	  value = GEN_INT (-UINTVAL (value));
	  code = PLUS;
	}
      /* Fall through.  */

    default:
      x = gen_rtx_fmt_ee (code, wmode, old_out, value);
      emit_insn (gen_rtx_SET (new_out, x));
      break;
    }

  aarch64_emit_store_exclusive (mode, cond, mem,
				gen_lowpart (mode, new_out), model_rtx);

  x = aarch64_gen_compare_zero_and_branch (NE, cond, label);
  aarch64_emit_unlikely_jump (x);

  /* Emit any final barrier needed for a __sync operation.  */
  if (is_sync)
    aarch64_emit_post_barrier (model);
}

static void
aarch64_init_libfuncs (void)
{
   /* Half-precision float operations.  The compiler handles all operations
     with NULL libfuncs by converting to SFmode.  */

  /* Conversions.  */
  set_conv_libfunc (trunc_optab, HFmode, SFmode, "__gnu_f2h_ieee");
  set_conv_libfunc (sext_optab, SFmode, HFmode, "__gnu_h2f_ieee");

  /* Arithmetic.  */
  set_optab_libfunc (add_optab, HFmode, NULL);
  set_optab_libfunc (sdiv_optab, HFmode, NULL);
  set_optab_libfunc (smul_optab, HFmode, NULL);
  set_optab_libfunc (neg_optab, HFmode, NULL);
  set_optab_libfunc (sub_optab, HFmode, NULL);

  /* Comparisons.  */
  set_optab_libfunc (eq_optab, HFmode, NULL);
  set_optab_libfunc (ne_optab, HFmode, NULL);
  set_optab_libfunc (lt_optab, HFmode, NULL);
  set_optab_libfunc (le_optab, HFmode, NULL);
  set_optab_libfunc (ge_optab, HFmode, NULL);
  set_optab_libfunc (gt_optab, HFmode, NULL);
  set_optab_libfunc (unord_optab, HFmode, NULL);
}

/* Target hook for c_mode_for_suffix.  */
static machine_mode
aarch64_c_mode_for_suffix (char suffix)
{
  if (suffix == 'q')
    return TFmode;

  return VOIDmode;
}

/* Return true iff X with mode MODE can be represented by a quarter-precision
   floating point immediate operand X.  Note, we cannot represent 0.0.  */

bool
aarch64_float_const_representable_p (rtx x)
{
  x = unwrap_const_vec_duplicate (x);
  machine_mode mode = GET_MODE (x);
  if (!CONST_DOUBLE_P (x))
    return false;

  if ((mode == HFmode && !TARGET_FP_F16INST)
      || mode == BFmode)
    return false;

  REAL_VALUE_TYPE r = *CONST_DOUBLE_REAL_VALUE (x);

  return aarch64_real_float_const_representable_p (r);
}

/* Returns the string with the instruction for the SIMD immediate
 * CONST_VECTOR of MODE and WIDTH.  WHICH selects a move, and(bic) or orr.  */
char*
aarch64_output_simd_imm (rtx const_vector, unsigned width,
			 enum simd_immediate_check which)
{
  bool is_valid;
  static char templ[40];
  const char *mnemonic;
  const char *shift_op;
  unsigned int lane_count = 0;
  char element_char;

  struct simd_immediate_info info;

  is_valid = aarch64_simd_valid_imm (const_vector, &info, which);
  gcc_assert (is_valid);

  element_char = sizetochar (GET_MODE_BITSIZE (info.elt_mode));
  lane_count = width / GET_MODE_BITSIZE (info.elt_mode);

  if (GET_MODE_CLASS (info.elt_mode) == MODE_FLOAT)
    {
      gcc_assert (info.insn == simd_immediate_info::MOV
		  && info.u.mov.shift == 0);
      /* For FP zero change it to a CONST_INT 0 and use the integer SIMD
	 move immediate path.  */
      if (aarch64_float_const_zero_rtx_p (info.u.mov.value))
        info.u.mov.value = GEN_INT (0);
      else
	{
	  const unsigned int buf_size = 20;
	  char float_buf[buf_size] = {'\0'};
	  real_to_decimal_for_mode (float_buf,
				    CONST_DOUBLE_REAL_VALUE (info.u.mov.value),
				    buf_size, buf_size, 1, info.elt_mode);

	  if (lane_count == 1)
	    snprintf (templ, sizeof (templ), "fmov\t%%d0, %s", float_buf);
	  else
	    snprintf (templ, sizeof (templ), "fmov\t%%0.%d%c, %s",
		      lane_count, element_char, float_buf);
	  return templ;
	}
    }

  gcc_assert (CONST_INT_P (info.u.mov.value));

  if (which == AARCH64_CHECK_MOV)
    {
      if (info.insn == simd_immediate_info::INDEX)
	{
	  gcc_assert (TARGET_SVE);
	  snprintf (templ, sizeof (templ), "index\t%%Z0.%c, #"
		    HOST_WIDE_INT_PRINT_DEC ", #" HOST_WIDE_INT_PRINT_DEC,
		    element_char, INTVAL (info.u.index.base),
		    INTVAL (info.u.index.step));
	  return templ;
	}

      if (info.insn == simd_immediate_info::SVE_MOV)
	{
	  gcc_assert (TARGET_SVE);
	  snprintf (templ, sizeof (templ), "mov\t%%Z0.%c, #" HOST_WIDE_INT_PRINT_DEC,
		    element_char, INTVAL (info.u.mov.value));
	  return templ;
	}

      mnemonic = info.insn == simd_immediate_info::MVN ? "mvni" : "movi";
      shift_op = (info.u.mov.modifier == simd_immediate_info::MSL
		  ? "msl" : "lsl");
      if (lane_count == 1)
	snprintf (templ, sizeof (templ), "%s\t%%d0, " HOST_WIDE_INT_PRINT_HEX,
		  mnemonic, UINTVAL (info.u.mov.value));
      else if (info.u.mov.shift)
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, "
		  HOST_WIDE_INT_PRINT_HEX ", %s %d", mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value), shift_op,
		  info.u.mov.shift);
      else
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, "
		  HOST_WIDE_INT_PRINT_HEX, mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value));
    }
  else
    {
      /* AARCH64_CHECK_ORR, AARCH64_CHECK_AND or AARCH64_CHECK_XOR.  */
      mnemonic = "orr";
      if (which == AARCH64_CHECK_AND)
	mnemonic = info.insn == simd_immediate_info::MVN ? "bic" : "and";
      else if (which == AARCH64_CHECK_XOR)
	mnemonic = "eor";

      if (info.insn == simd_immediate_info::SVE_MOV)
	{
	  gcc_assert (TARGET_SVE);
	  snprintf (templ, sizeof (templ), "%s\t%%Z0.%c, %%Z0.%c, "
		    HOST_WIDE_INT_PRINT_DEC, mnemonic, element_char,
		    element_char, INTVAL (info.u.mov.value));
	}
      else if (info.u.mov.shift)
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, #"
		  HOST_WIDE_INT_PRINT_DEC ", %s #%d", mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value), "lsl",
		  info.u.mov.shift);
      else
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, #"
		  HOST_WIDE_INT_PRINT_DEC, mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value));
    }
  return templ;
}

/* Returns the string with the ORR instruction for the SIMD immediate
   CONST_VECTOR of WIDTH bits.  */
char*
aarch64_output_simd_orr_imm (rtx const_vector, unsigned width)
{
  return aarch64_output_simd_imm (const_vector, width, AARCH64_CHECK_ORR);
}

/* Returns the string with the AND/BIC instruction for the SIMD immediate
   CONST_VECTOR of WIDTH bits.  */
char*
aarch64_output_simd_and_imm (rtx const_vector, unsigned width)
{
  return aarch64_output_simd_imm (const_vector, width, AARCH64_CHECK_AND);
}

/* Returns the string with the EOR instruction for the SIMD immediate
   CONST_VECTOR of WIDTH bits.  */
char*
aarch64_output_simd_xor_imm (rtx const_vector, unsigned width)
{
  return aarch64_output_simd_imm (const_vector, width, AARCH64_CHECK_XOR);
}

/* Returns the string with the MOV instruction for the SIMD immediate
   CONST_VECTOR of WIDTH bits.  */
char*
aarch64_output_simd_mov_imm (rtx const_vector, unsigned width)
{
  return aarch64_output_simd_imm (const_vector, width, AARCH64_CHECK_MOV);
}

char*
aarch64_output_scalar_simd_mov_immediate (rtx immediate, scalar_int_mode mode)
{

  /* If a floating point number was passed and we desire to use it in an
     integer mode do the conversion to integer.  */
  if (CONST_DOUBLE_P (immediate) && GET_MODE_CLASS (mode) == MODE_INT)
    {
      unsigned HOST_WIDE_INT ival;
      if (!aarch64_reinterpret_float_as_int (immediate, &ival))
	  gcc_unreachable ();
      immediate = gen_int_mode (ival, mode);
    }

  machine_mode vmode;
  /* use a 64 bit mode for everything except for DI/DF/DD mode, where we use
     a 128 bit vector mode.  */
  int width = GET_MODE_BITSIZE (mode) == 64 ? 128 : 64;

  vmode = aarch64_simd_container_mode (mode, width);
  rtx v_op = aarch64_simd_gen_const_vector_dup (vmode, INTVAL (immediate));
  return aarch64_output_simd_mov_imm (v_op, width);
}

/* Return the output string to use for moving immediate CONST_VECTOR
   into an SVE register.  */

char *
aarch64_output_sve_mov_immediate (rtx const_vector)
{
  static char templ[40];
  struct simd_immediate_info info;
  char element_char;
  bool is_valid;

  is_valid = aarch64_simd_valid_imm (const_vector, &info, AARCH64_CHECK_MOV);
  gcc_assert (is_valid);

  element_char = sizetochar (GET_MODE_BITSIZE (info.elt_mode));

  machine_mode vec_mode = GET_MODE (const_vector);
  if (aarch64_sve_pred_mode_p (vec_mode))
    {
      static char buf[sizeof ("ptrue\t%0.N, vlNNNNN")];
      if (info.insn == simd_immediate_info::MOV)
	{
	  gcc_assert (info.u.mov.value == const0_rtx);
	  snprintf (buf, sizeof (buf), "pfalse\t%%0.b");
	}
      else
	{
	  gcc_assert (info.insn == simd_immediate_info::PTRUE);
	  unsigned int total_bytes;
	  if (info.u.pattern == AARCH64_SV_ALL
	      && BYTES_PER_SVE_VECTOR.is_constant (&total_bytes))
	    snprintf (buf, sizeof (buf), "ptrue\t%%0.%c, vl%d", element_char,
		      total_bytes / GET_MODE_SIZE (info.elt_mode));
	  else
	    snprintf (buf, sizeof (buf), "ptrue\t%%0.%c, %s", element_char,
		      svpattern_token (info.u.pattern));
	}
      return buf;
    }

  if (info.insn == simd_immediate_info::INDEX)
    {
      snprintf (templ, sizeof (templ), "index\t%%0.%c, #"
		HOST_WIDE_INT_PRINT_DEC ", #" HOST_WIDE_INT_PRINT_DEC,
		element_char, INTVAL (info.u.index.base),
		INTVAL (info.u.index.step));
      return templ;
    }

  if (GET_MODE_CLASS (info.elt_mode) == MODE_FLOAT)
    {
      if (aarch64_float_const_zero_rtx_p (info.u.mov.value))
	info.u.mov.value = GEN_INT (0);
      else
	{
	  const int buf_size = 20;
	  char float_buf[buf_size] = {};
	  real_to_decimal_for_mode (float_buf,
				    CONST_DOUBLE_REAL_VALUE (info.u.mov.value),
				    buf_size, buf_size, 1, info.elt_mode);

	  snprintf (templ, sizeof (templ), "fmov\t%%0.%c, #%s",
		    element_char, float_buf);
	  return templ;
	}
    }

  if (info.u.mov.value == const0_rtx && TARGET_NON_STREAMING)
    snprintf (templ, sizeof (templ), "movi\t%%d0, #0");
  else
    snprintf (templ, sizeof (templ), "mov\t%%0.%c, #" HOST_WIDE_INT_PRINT_DEC,
	      element_char, INTVAL (info.u.mov.value));
  return templ;
}

/* Return the asm template for a PTRUES.  CONST_UNSPEC is the
   aarch64_sve_ptrue_svpattern_immediate that describes the predicate
   pattern.  */

char *
aarch64_output_sve_ptrues (rtx const_unspec)
{
  static char templ[40];
  struct simd_immediate_info info;
  bool is_valid;

  is_valid = aarch64_simd_valid_imm (const_unspec, &info, AARCH64_CHECK_MOV);
  gcc_assert (is_valid && info.insn == simd_immediate_info::PTRUE);

  char element_char = sizetochar (GET_MODE_BITSIZE (info.elt_mode));
  snprintf (templ, sizeof (templ), "ptrues\t%%0.%c, %s", element_char,
	    svpattern_token (info.u.pattern));
  return templ;
}

/* Split operands into moves from op[1] + op[2] into op[0].  */

void
aarch64_split_combinev16qi (rtx operands[3])
{
  machine_mode halfmode = GET_MODE (operands[1]);

  gcc_assert (halfmode == V16QImode);

  rtx destlo = simplify_gen_subreg (halfmode, operands[0],
				    GET_MODE (operands[0]), 0);
  rtx desthi = simplify_gen_subreg (halfmode, operands[0],
				    GET_MODE (operands[0]),
				    GET_MODE_SIZE (halfmode));

  bool skiplo = rtx_equal_p (destlo, operands[1]);
  bool skiphi = rtx_equal_p (desthi, operands[2]);

  if (skiplo && skiphi)
    {
      /* No-op move.  Can't split to nothing; emit something.  */
      emit_note (NOTE_INSN_DELETED);
      return;
    }

  /* Special case of reversed high/low parts.  */
  if (reg_overlap_mentioned_p (operands[2], destlo)
      && reg_overlap_mentioned_p (operands[1], desthi))
    {
      emit_insn (gen_xorv16qi3 (operands[1], operands[1], operands[2]));
      emit_insn (gen_xorv16qi3 (operands[2], operands[1], operands[2]));
      emit_insn (gen_xorv16qi3 (operands[1], operands[1], operands[2]));
    }
  else if (!reg_overlap_mentioned_p (operands[2], destlo))
    {
      /* Try to avoid unnecessary moves if part of the result
	 is in the right place already.  */
      if (!skiplo)
	emit_move_insn (destlo, operands[1]);
      if (!skiphi)
	emit_move_insn (desthi, operands[2]);
    }
  else
    {
      if (!skiphi)
	emit_move_insn (desthi, operands[2]);
      if (!skiplo)
	emit_move_insn (destlo, operands[1]);
    }
}

/* vec_perm support.  */

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  vec_perm_indices perm;
  machine_mode vmode;
  machine_mode op_mode;
  unsigned int vec_flags;
  unsigned int op_vec_flags;
  bool one_vector_p;
  bool zero_op0_p, zero_op1_p;
  bool testing_p;
};

static bool aarch64_expand_vec_perm_const_1 (struct expand_vec_perm_d *d);

/* Generate a variable permutation.  */

static void
aarch64_expand_vec_perm_1 (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  bool one_vector_p = rtx_equal_p (op0, op1);

  gcc_checking_assert (vmode == V8QImode || vmode == V16QImode);
  gcc_checking_assert (GET_MODE (op0) == vmode);
  gcc_checking_assert (GET_MODE (op1) == vmode);
  gcc_checking_assert (GET_MODE (sel) == vmode);
  gcc_checking_assert (TARGET_SIMD);

  if (one_vector_p)
    {
      if (vmode == V8QImode)
	{
	  /* Expand the argument to a V16QI mode by duplicating it.  */
	  rtx pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_aarch64_combinev8qi (pair, op0, op0));
	  emit_insn (gen_aarch64_qtbl1v8qi (target, pair, sel));
	}
      else
	{
	  emit_insn (gen_aarch64_qtbl1v16qi (target, op0, sel));
	}
    }
  else
    {
      rtx pair;

      if (vmode == V8QImode)
	{
	  pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_aarch64_combinev8qi (pair, op0, op1));
	  emit_insn (gen_aarch64_qtbl1v8qi (target, pair, sel));
	}
      else
	{
	  pair = gen_reg_rtx (V2x16QImode);
	  emit_insn (gen_aarch64_combinev16qi (pair, op0, op1));
	  emit_insn (gen_aarch64_qtbl2v16qi (target, pair, sel));
	}
    }
}

/* Expand a vec_perm with the operands given by TARGET, OP0, OP1 and SEL.
   NELT is the number of elements in the vector.  */

void
aarch64_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel,
			 unsigned int nelt)
{
  machine_mode vmode = GET_MODE (target);
  bool one_vector_p = rtx_equal_p (op0, op1);
  rtx mask;

  /* The TBL instruction does not use a modulo index, so we must take care
     of that ourselves.  */
  mask = aarch64_simd_gen_const_vector_dup (vmode,
      one_vector_p ? nelt - 1 : 2 * nelt - 1);
  sel = expand_simple_binop (vmode, AND, sel, mask, NULL, 0, OPTAB_LIB_WIDEN);

  /* For big-endian, we also need to reverse the index within the vector
     (but not which vector).  */
  if (BYTES_BIG_ENDIAN)
    {
      /* If one_vector_p, mask is a vector of (nelt - 1)'s already.  */
      if (!one_vector_p)
        mask = aarch64_simd_gen_const_vector_dup (vmode, nelt - 1);
      sel = expand_simple_binop (vmode, XOR, sel, mask,
				 NULL, 0, OPTAB_LIB_WIDEN);
    }
  aarch64_expand_vec_perm_1 (target, op0, op1, sel);
}

/* Generate (set TARGET (unspec [OP0 OP1] CODE)).  */

static void
emit_unspec2 (rtx target, int code, rtx op0, rtx op1)
{
  emit_insn (gen_rtx_SET (target,
			  gen_rtx_UNSPEC (GET_MODE (target),
					  gen_rtvec (2, op0, op1), code)));
}

/* Expand an SVE vec_perm with the given operands.  */

void
aarch64_expand_sve_vec_perm (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = GET_MODE (sel);
  /* Enforced by the pattern condition.  */
  int nunits = GET_MODE_NUNITS (sel_mode).to_constant ();

  /* Note: vec_perm indices are supposed to wrap when they go beyond the
     size of the two value vectors, i.e. the upper bits of the indices
     are effectively ignored.  SVE TBL instead produces 0 for any
     out-of-range indices, so we need to modulo all the vec_perm indices
     to ensure they are all in range.  */
  rtx sel_reg = force_reg (sel_mode, sel);

  /* Check if the sel only references the first values vector.  */
  if (CONST_VECTOR_P (sel)
      && aarch64_const_vec_all_in_range_p (sel, 0, nunits - 1))
    {
      emit_unspec2 (target, UNSPEC_TBL, op0, sel_reg);
      return;
    }

  /* Check if the two values vectors are the same.  */
  if (rtx_equal_p (op0, op1))
    {
      rtx max_sel = aarch64_simd_gen_const_vector_dup (sel_mode, nunits - 1);
      rtx sel_mod = expand_simple_binop (sel_mode, AND, sel_reg, max_sel,
					 NULL, 0, OPTAB_DIRECT);
      emit_unspec2 (target, UNSPEC_TBL, op0, sel_mod);
      return;
    }

  /* Run TBL on for each value vector and combine the results.  */

  rtx res0 = gen_reg_rtx (data_mode);
  rtx res1 = gen_reg_rtx (data_mode);
  rtx neg_num_elems = aarch64_simd_gen_const_vector_dup (sel_mode, -nunits);
  if (!CONST_VECTOR_P (sel)
      || !aarch64_const_vec_all_in_range_p (sel, 0, 2 * nunits - 1))
    {
      rtx max_sel = aarch64_simd_gen_const_vector_dup (sel_mode,
						       2 * nunits - 1);
      sel_reg = expand_simple_binop (sel_mode, AND, sel_reg, max_sel,
				     NULL, 0, OPTAB_DIRECT);
    }
  emit_unspec2 (res0, UNSPEC_TBL, op0, sel_reg);
  rtx sel_sub = expand_simple_binop (sel_mode, PLUS, sel_reg, neg_num_elems,
				     NULL, 0, OPTAB_DIRECT);
  emit_unspec2 (res1, UNSPEC_TBL, op1, sel_sub);
  if (GET_MODE_CLASS (data_mode) == MODE_VECTOR_INT)
    emit_insn (gen_rtx_SET (target, gen_rtx_IOR (data_mode, res0, res1)));
  else
    emit_unspec2 (target, UNSPEC_IORF, res0, res1);
}

/* Recognize patterns suitable for the TRN instructions.  */
static bool
aarch64_evpc_trn (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  poly_uint64 nelt = d->perm.length ();
  rtx out, in0, in1;
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  if (!d->perm[0].is_constant (&odd)
      || (odd != 0 && odd != 1)
      || !d->perm.series_p (0, 2, odd, 2)
      || !d->perm.series_p (1, 2, nelt + odd, 2))
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  /* We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && d->vec_flags == VEC_ADVSIMD)
    {
      std::swap (in0, in1);
      odd = !odd;
    }
  out = d->target;

  emit_set_insn (out, gen_rtx_UNSPEC (vmode, gen_rtvec (2, in0, in1),
				      odd ? UNSPEC_TRN2 : UNSPEC_TRN1));
  return true;
}

/* Try to re-encode the PERM constant so it combines odd and even elements.
   This rewrites constants such as {0, 1, 4, 5}/V4SF to {0, 2}/V2DI.
   We retry with this new constant with the full suite of patterns.  */
static bool
aarch64_evpc_reencode (struct expand_vec_perm_d *d)
{
  expand_vec_perm_d newd;

  /* The subregs that we'd create are not supported for big-endian SVE;
     see aarch64_modes_compatible_p for details.  */
  if (BYTES_BIG_ENDIAN && (d->vec_flags & VEC_ANY_SVE))
    return false;

  /* Get the new mode.  Always twice the size of the inner
     and half the elements.  */
  machine_mode new_mode;
  if (!aarch64_coalesce_units (d->vmode, 2).exists (&new_mode))
    return false;

  vec_perm_indices newpermindices;
  if (!newpermindices.new_shrunk_vector (d->perm, 2))
    return false;

  newd.vmode = new_mode;
  newd.vec_flags = d->vec_flags;
  newd.op_mode = newd.vmode;
  newd.op_vec_flags = newd.vec_flags;
  newd.target = d->target ? gen_lowpart (new_mode, d->target) : NULL;
  newd.op0 = d->op0 ? gen_lowpart (new_mode, d->op0) : NULL;
  newd.op1 = d->op1 ? gen_lowpart (new_mode, d->op1) : NULL;
  newd.testing_p = d->testing_p;
  newd.one_vector_p = d->one_vector_p;

  newd.perm.new_vector (newpermindices.encoding (), newd.one_vector_p ? 1 : 2,
			newpermindices.nelts_per_input ());
  return aarch64_expand_vec_perm_const_1 (&newd);
}

/* Recognize patterns suitable for the UZP instructions.  */
static bool
aarch64_evpc_uzp (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  rtx out, in0, in1;
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  if (!d->perm[0].is_constant (&odd)
      || (odd != 0 && odd != 1)
      || !d->perm.series_p (0, 1, odd, 2))
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  /* We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && d->vec_flags == VEC_ADVSIMD)
    {
      std::swap (in0, in1);
      odd = !odd;
    }
  out = d->target;

  emit_set_insn (out, gen_rtx_UNSPEC (vmode, gen_rtvec (2, in0, in1),
				      odd ? UNSPEC_UZP2 : UNSPEC_UZP1));
  return true;
}

/* Recognize patterns suitable for the ZIP instructions.  */
static bool
aarch64_evpc_zip (struct expand_vec_perm_d *d)
{
  unsigned int high;
  poly_uint64 nelt = d->perm.length ();
  rtx out, in0, in1;
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  poly_uint64 first = d->perm[0];
  if ((maybe_ne (first, 0U) && maybe_ne (first * 2, nelt))
      || !d->perm.series_p (0, 2, first, 1)
      || !d->perm.series_p (1, 2, first + nelt, 1))
    return false;
  high = maybe_ne (first, 0U);

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  /* We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && d->vec_flags == VEC_ADVSIMD)
    {
      std::swap (in0, in1);
      high = !high;
    }
  out = d->target;

  emit_set_insn (out, gen_rtx_UNSPEC (vmode, gen_rtvec (2, in0, in1),
				      high ? UNSPEC_ZIP2 : UNSPEC_ZIP1));
  return true;
}

/* Recognize patterns for the EXT insn.  */

static bool
aarch64_evpc_ext (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT location;
  rtx offset;

  /* The first element always refers to the first vector.
     Check if the extracted indices are increasing by one.  */
  if ((d->vec_flags & VEC_SVE_PRED)
      || !d->perm[0].is_constant (&location)
      || !d->perm.series_p (0, 1, location, 1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  /* The case where (location == 0) is a no-op for both big- and little-endian,
     and is removed by the mid-end at optimization levels -O1 and higher.

     We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && location != 0 && d->vec_flags == VEC_ADVSIMD)
    {
      /* After setup, we want the high elements of the first vector (stored
         at the LSB end of the register), and the low elements of the second
         vector (stored at the MSB end of the register). So swap.  */
      std::swap (d->op0, d->op1);
      /* location != 0 (above), so safe to assume (nelt - location) < nelt.
	 to_constant () is safe since this is restricted to Advanced SIMD
	 vectors.  */
      location = d->perm.length ().to_constant () - location;
    }

  offset = GEN_INT (location);
  emit_set_insn (d->target,
		 gen_rtx_UNSPEC (d->vmode,
				 gen_rtvec (3, d->op0, d->op1, offset),
				 UNSPEC_EXT));
  return true;
}

/* Recognize patterns for the REV{64,32,16} insns, which reverse elements
   within each 64-bit, 32-bit or 16-bit granule.  */

static bool
aarch64_evpc_rev_local (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT diff;
  unsigned int i, size, unspec;
  machine_mode pred_mode;

  if ((d->vec_flags & VEC_SVE_PRED)
      || !d->one_vector_p
      || !d->perm[0].is_constant (&diff)
      || !diff)
    return false;

  if (d->vec_flags & VEC_SVE_DATA)
    size = (diff + 1) * aarch64_sve_container_bits (d->vmode);
  else
    size = (diff + 1) * GET_MODE_UNIT_BITSIZE (d->vmode);
  if (size == 64)
    {
      unspec = UNSPEC_REV64;
      pred_mode = VNx2BImode;
    }
  else if (size == 32)
    {
      unspec = UNSPEC_REV32;
      pred_mode = VNx4BImode;
    }
  else if (size == 16)
    {
      unspec = UNSPEC_REV16;
      pred_mode = VNx8BImode;
    }
  else
    return false;

  unsigned int step = diff + 1;
  for (i = 0; i < step; ++i)
    if (!d->perm.series_p (i, step, diff - i, step))
      return false;

  /* Success! */
  if (d->testing_p)
    return true;

  if (d->vec_flags & VEC_SVE_DATA)
    {
      rtx pred = aarch64_ptrue_reg (pred_mode);
      emit_insn (gen_aarch64_sve_revbhw (d->vmode, pred_mode,
					 d->target, pred, d->op0));
      return true;
    }
  rtx src = gen_rtx_UNSPEC (d->vmode, gen_rtvec (1, d->op0), unspec);
  emit_set_insn (d->target, src);
  return true;
}

/* Recognize patterns for the REV insn, which reverses elements within
   a full vector.  */

static bool
aarch64_evpc_rev_global (struct expand_vec_perm_d *d)
{
  poly_uint64 nelt = d->perm.length ();

  if (!d->one_vector_p || d->vec_flags == VEC_ADVSIMD)
    return false;

  if (!d->perm.series_p (0, 1, nelt - 1, -1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  rtx src = gen_rtx_UNSPEC (d->vmode, gen_rtvec (1, d->op0), UNSPEC_REV);
  emit_set_insn (d->target, src);
  return true;
}

static bool
aarch64_evpc_dup (struct expand_vec_perm_d *d)
{
  rtx out = d->target;
  rtx in0;
  HOST_WIDE_INT elt;
  machine_mode vmode = d->vmode;
  rtx lane;

  if ((d->vec_flags & VEC_SVE_PRED)
      || d->perm.encoding ().encoded_nelts () != 1
      || !d->perm[0].is_constant (&elt))
    return false;

  if ((d->vec_flags & VEC_SVE_DATA)
      && elt * (aarch64_sve_container_bits (vmode) / 8) >= 64)
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  /* The generic preparation in aarch64_expand_vec_perm_const_1
     swaps the operand order and the permute indices if it finds
     d->perm[0] to be in the second operand.  Thus, we can always
     use d->op0 and need not do any extra arithmetic to get the
     correct lane number.  */
  in0 = d->op0;
  lane = GEN_INT (elt); /* The pattern corrects for big-endian.  */

  rtx parallel = gen_rtx_PARALLEL (vmode, gen_rtvec (1, lane));
  rtx select = gen_rtx_VEC_SELECT (GET_MODE_INNER (vmode), in0, parallel);
  emit_set_insn (out, gen_rtx_VEC_DUPLICATE (vmode, select));
  return true;
}

/* Recognize things that can be done using the SVE2p1 Hybrid-VLA
   permutations, which apply Advanced-SIMD-style permutations to each
   individual 128-bit block.  */

static bool
aarch64_evpc_hvla (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  if (!TARGET_SVE2p1
      || !TARGET_NON_STREAMING
      || BYTES_BIG_ENDIAN
      || d->vec_flags != VEC_SVE_DATA
      || GET_MODE_UNIT_BITSIZE (vmode) > 64)
    return false;

  /* Set SUBELTS to the number of elements in an Advanced SIMD vector
     and make sure that adding SUBELTS to each block of SUBELTS indices
     gives the next block of SUBELTS indices.  That is, it must be possible
     to interpret the index vector as SUBELTS interleaved linear series in
     which each series has step SUBELTS.  */
  unsigned int subelts = 128U / GET_MODE_UNIT_BITSIZE (vmode);
  unsigned int pairs = subelts / 2;
  for (unsigned int i = 0; i < subelts; ++i)
    if (!d->perm.series_p (i, subelts, d->perm[i], subelts))
      return false;

  /* Used once we have verified that we can use UNSPEC to do the operation.  */
  auto use_binary = [&](int unspec) -> bool
    {
      if (!d->testing_p)
	{
	  rtvec vec = gen_rtvec (2, d->op0, d->op1);
	  emit_set_insn (d->target, gen_rtx_UNSPEC (vmode, vec, unspec));
	}
      return true;
    };

  /* Now check whether the first SUBELTS elements match a supported
     Advanced-SIMD-style operation.  */
  poly_int64 first = d->perm[0];
  poly_int64 nelt = d->perm.length ();
  auto try_zip = [&]() -> bool
    {
      if (maybe_ne (first, 0) && maybe_ne (first, pairs))
	return false;
      for (unsigned int i = 0; i < pairs; ++i)
	if (maybe_ne (d->perm[i * 2], first + i)
	    || maybe_ne (d->perm[i * 2 + 1], first + nelt + i))
	  return false;
      return use_binary (maybe_ne (first, 0) ? UNSPEC_ZIPQ2 : UNSPEC_ZIPQ1);
    };
  auto try_uzp = [&]() -> bool
    {
      if (maybe_ne (first, 0) && maybe_ne (first, 1))
	return false;
      for (unsigned int i = 0; i < pairs; ++i)
	if (maybe_ne (d->perm[i], first + i * 2)
	    || maybe_ne (d->perm[i + pairs], first + nelt + i * 2))
	  return false;
      return use_binary (maybe_ne (first, 0) ? UNSPEC_UZPQ2 : UNSPEC_UZPQ1);
    };
  auto try_extq = [&]() -> bool
    {
      HOST_WIDE_INT start;
      if (!first.is_constant (&start) || !IN_RANGE (start, 0, subelts - 1))
	return false;
      for (unsigned int i = 0; i < subelts; ++i)
	{
	  poly_int64 next = (start + i >= subelts
			     ? start + i - subelts + nelt
			     : start + i);
	  if (maybe_ne (d->perm[i], next))
	    return false;
	}
      if (!d->testing_p)
	{
	  rtx op2 = gen_int_mode (start, SImode);
	  emit_insn (gen_aarch64_sve_extq (vmode, d->target,
					   d->op0, d->op1, op2));
	}
      return true;
    };
  auto try_dupq = [&]() -> bool
    {
      HOST_WIDE_INT start;
      if (!first.is_constant (&start) || !IN_RANGE (start, 0, subelts - 1))
	return false;
      for (unsigned int i = 0; i < subelts; ++i)
	if (maybe_ne (d->perm[i], start))
	  return false;
      if (!d->testing_p)
	{
	  rtx op1 = gen_int_mode (start, SImode);
	  emit_insn (gen_aarch64_sve_dupq (vmode, d->target, d->op0, op1));
	}
      return true;
    };

  return try_zip () || try_uzp () || try_extq () || try_dupq ();
}

static bool
aarch64_evpc_tbl (struct expand_vec_perm_d *d)
{
  rtx rperm[MAX_COMPILE_TIME_VEC_BYTES], sel;
  machine_mode vmode = d->vmode;

  /* Make sure that the indices are constant.  */
  unsigned int encoded_nelts = d->perm.encoding ().encoded_nelts ();
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    if (!d->perm[i].is_constant ())
      return false;

  if (d->testing_p)
    return true;

  /* Generic code will try constant permutation twice.  Once with the
     original mode and again with the elements lowered to QImode.
     So wait and don't do the selector expansion ourselves.  */
  if (vmode != V8QImode && vmode != V16QImode)
    return false;

  /* to_constant is safe since this routine is specific to Advanced SIMD
     vectors.  */
  unsigned int nelt = d->perm.length ().to_constant ();

  /* If one register is the constant vector of 0 then we only need
     a one reg TBL and we map any accesses to the vector of 0 to -1.  We can't
     do this earlier since vec_perm_indices clamps elements to within range so
     we can only do it during codegen.  */
  if (d->zero_op0_p)
    d->op0 = d->op1;
  else if (d->zero_op1_p)
    d->op1 = d->op0;

  for (unsigned int i = 0; i < nelt; ++i)
    {
      auto val = d->perm[i].to_constant ();

      /* If we're selecting from a 0 vector, we can just use an out of range
	 index instead.  */
      if ((d->zero_op0_p && val < nelt) || (d->zero_op1_p && val >= nelt))
	rperm[i] = constm1_rtx;
      else
	{
	  /* If we are remapping a zero register as the first parameter we need
	     to adjust the indices of the non-zero register.  */
	  if (d->zero_op0_p)
	    val = val % nelt;

	  /* If big-endian and two vectors we end up with a weird mixed-endian
	     mode on NEON.  Reverse the index within each word but not the word
	     itself.  to_constant is safe because we checked is_constant
	     above.  */
	  rperm[i] = GEN_INT (BYTES_BIG_ENDIAN ? val ^ (nelt - 1) : val);
	}
    }

  sel = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (nelt, rperm));
  sel = force_reg (vmode, sel);

  aarch64_expand_vec_perm_1 (d->target, d->op0, d->op1, sel);
  return true;
}

/* Try to implement D using an SVE TBL instruction.  */

static bool
aarch64_evpc_sve_tbl (struct expand_vec_perm_d *d)
{
  unsigned HOST_WIDE_INT nelt;

  /* Permuting two variable-length vectors could overflow the
     index range.  */
  if (!d->one_vector_p && !d->perm.length ().is_constant (&nelt))
    return false;

  if (d->testing_p)
    return true;

  machine_mode sel_mode = related_int_vector_mode (d->vmode).require ();
  rtx sel = vec_perm_indices_to_rtx (sel_mode, d->perm);
  if (d->one_vector_p)
    emit_unspec2 (d->target, UNSPEC_TBL, d->op0, force_reg (sel_mode, sel));
  else
    aarch64_expand_sve_vec_perm (d->target, d->op0, d->op1, sel);
  return true;
}

/* Try to implement D using SVE dup instruction.  */

static bool
aarch64_evpc_sve_dup (struct expand_vec_perm_d *d)
{
  if (BYTES_BIG_ENDIAN
      || !d->one_vector_p
      || d->vec_flags != VEC_SVE_DATA
      || d->op_vec_flags != VEC_ADVSIMD
      || d->perm.encoding ().nelts_per_pattern () != 1
      || !known_eq (d->perm.encoding ().npatterns (),
		    GET_MODE_NUNITS (d->op_mode))
      || !known_eq (GET_MODE_BITSIZE (d->op_mode), 128))
    return false;

  int npatterns = d->perm.encoding ().npatterns ();
  for (int i = 0; i < npatterns; i++)
    if (!known_eq (d->perm[i], i))
      return false;

  if (d->testing_p)
    return true;

  aarch64_expand_sve_dupq (d->target, GET_MODE (d->target), d->op0);
  return true;
}

/* Try to implement D using SVE SEL instruction.  */

static bool
aarch64_evpc_sel (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  int unit_size = GET_MODE_UNIT_SIZE (vmode);

  if (d->vec_flags != VEC_SVE_DATA
      || unit_size > 8)
    return false;

  int n_patterns = d->perm.encoding ().npatterns ();
  poly_int64 vec_len = d->perm.length ();

  for (int i = 0; i < n_patterns; ++i)
    if (!known_eq (d->perm[i], i)
	&& !known_eq (d->perm[i], vec_len + i))
      return false;

  for (int i = n_patterns; i < n_patterns * 2; i++)
    if (!d->perm.series_p (i, n_patterns, i, n_patterns)
	&& !d->perm.series_p (i, n_patterns, vec_len + i, n_patterns))
      return false;

  if (d->testing_p)
    return true;

  machine_mode pred_mode = aarch64_sve_pred_mode (vmode);

  /* Build a predicate that is true when op0 elements should be used.  */
  rtx_vector_builder builder (pred_mode, n_patterns, 2);
  for (int i = 0; i < n_patterns * 2; i++)
    {
      rtx elem = known_eq (d->perm[i], i) ? CONST1_RTX (BImode)
					  : CONST0_RTX (BImode);
      builder.quick_push (elem);
    }

  rtx const_vec = builder.build ();
  rtx pred = force_reg (pred_mode, const_vec);
  /* TARGET = PRED ? OP0 : OP1.  */
  emit_insn (gen_vcond_mask (vmode, vmode, d->target, d->op0, d->op1, pred));
  return true;
}

/* Recognize patterns suitable for the INS instructions.  */
static bool
aarch64_evpc_ins (struct expand_vec_perm_d *d)
{
  machine_mode mode = d->vmode;
  unsigned HOST_WIDE_INT nelt;

  if (d->vec_flags != VEC_ADVSIMD)
    return false;

  /* to_constant is safe since this routine is specific to Advanced SIMD
     vectors.  */
  nelt = d->perm.length ().to_constant ();
  rtx insv = d->op0;

  HOST_WIDE_INT idx = -1;

  for (unsigned HOST_WIDE_INT i = 0; i < nelt; i++)
    {
      HOST_WIDE_INT elt;
      if (!d->perm[i].is_constant (&elt))
	return false;
      if (elt == (HOST_WIDE_INT) i)
	continue;
      if (idx != -1)
	{
	  idx = -1;
	  break;
	}
      idx = i;
    }

  if (idx == -1)
    {
      insv = d->op1;
      for (unsigned HOST_WIDE_INT i = 0; i < nelt; i++)
	{
	  if (d->perm[i].to_constant () == (HOST_WIDE_INT) (i + nelt))
	    continue;
	  if (idx != -1)
	    return false;
	  idx = i;
	}

      if (idx == -1)
	return false;
    }

  if (d->testing_p)
    return true;

  gcc_assert (idx != -1);

  unsigned extractindex = d->perm[idx].to_constant ();
  rtx extractv = d->op0;
  if (extractindex >= nelt)
    {
      extractv = d->op1;
      extractindex -= nelt;
    }
  gcc_assert (extractindex < nelt);

  insn_code icode = code_for_aarch64_simd_vec_copy_lane (mode);
  expand_operand ops[5];
  create_output_operand (&ops[0], d->target, mode);
  create_input_operand (&ops[1], insv, mode);
  create_integer_operand (&ops[2], 1 << idx);
  create_input_operand (&ops[3], extractv, mode);
  create_integer_operand (&ops[4], extractindex);
  expand_insn (icode, 5, ops);

  return true;
}

static bool
aarch64_expand_vec_perm_const_1 (struct expand_vec_perm_d *d)
{
  gcc_assert (d->op_mode != E_VOIDmode);

  /* The pattern matching functions above are written to look for a small
     number to begin the sequence (0, 1, N/2).  If we begin with an index
     from the second operand, we can swap the operands.  */
  poly_int64 nelt = d->perm.length ();
  if (known_ge (d->perm[0], nelt))
    {
      d->perm.rotate_inputs (1);
      std::swap (d->op0, d->op1);
    }

  if (((d->vec_flags == VEC_ADVSIMD && TARGET_SIMD)
       || d->vec_flags == VEC_SVE_DATA
       || d->vec_flags == (VEC_SVE_DATA | VEC_PARTIAL)
       || d->vec_flags == VEC_SVE_PRED)
      && known_gt (nelt, 1))
    {
      if (d->vmode == d->op_mode)
	{
	  if (aarch64_evpc_rev_local (d))
	    return true;
	  else if (aarch64_evpc_rev_global (d))
	    return true;
	  else if (aarch64_evpc_ext (d))
	    return true;
	  else if (aarch64_evpc_dup (d))
	    return true;
	  else if (aarch64_evpc_zip (d))
	    return true;
	  else if (aarch64_evpc_uzp (d))
	    return true;
	  else if (aarch64_evpc_trn (d))
	    return true;
	  else if (aarch64_evpc_sel (d))
	    return true;
	  else if (aarch64_evpc_ins (d))
	    return true;
	  else if (aarch64_evpc_hvla (d))
	    return true;
	  else if (aarch64_evpc_reencode (d))
	    return true;

	  if (d->vec_flags == VEC_SVE_DATA)
	    return aarch64_evpc_sve_tbl (d);
	  else if (d->vec_flags == VEC_ADVSIMD)
	    return aarch64_evpc_tbl (d);
	}
      else
	{
	  if (aarch64_evpc_sve_dup (d))
	    return true;
	}
    }
  return false;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
aarch64_vectorize_vec_perm_const (machine_mode vmode, machine_mode op_mode,
				  rtx target, rtx op0, rtx op1,
				  const vec_perm_indices &sel)
{
  struct expand_vec_perm_d d;

  /* Check whether the mask can be applied to a single vector.  */
  if (sel.ninputs () == 1
      || (op0 && rtx_equal_p (op0, op1)))
    d.one_vector_p = true;
  else if (sel.all_from_input_p (0))
    {
      d.one_vector_p = true;
      op1 = op0;
    }
  else if (sel.all_from_input_p (1))
    {
      d.one_vector_p = true;
      op0 = op1;
    }
  else
    d.one_vector_p = false;

  d.zero_op0_p = op0 == CONST0_RTX (op_mode);
  d.zero_op1_p = op1 == CONST0_RTX (op_mode);
  d.perm.new_vector (sel.encoding (), d.one_vector_p ? 1 : 2,
		     sel.nelts_per_input ());
  d.vmode = vmode;
  d.vec_flags = aarch64_classify_vector_mode (d.vmode);
  d.op_mode = op_mode;
  d.op_vec_flags = aarch64_classify_vector_mode (d.op_mode);
  d.target = target;
  d.op0 = op0 ? force_reg (op_mode, op0) : NULL_RTX;
  if (op0 == op1)
    d.op1 = d.op0;
  else
    d.op1 = op1 ? force_reg (op_mode, op1) : NULL_RTX;
  d.testing_p = !target;

  if (!d.testing_p)
    return aarch64_expand_vec_perm_const_1 (&d);

  rtx_insn *last = get_last_insn ();
  bool ret = aarch64_expand_vec_perm_const_1 (&d);
  gcc_assert (last == get_last_insn ());

  return ret;
}
/* Generate a byte permute mask for a register of mode MODE,
   which has NUNITS units.  */

rtx
aarch64_reverse_mask (machine_mode mode, unsigned int nunits)
{
  /* We have to reverse each vector because we dont have
     a permuted load that can reverse-load according to ABI rules.  */
  rtx mask;
  rtvec v = rtvec_alloc (16);
  unsigned int i, j;
  unsigned int usize = GET_MODE_UNIT_SIZE (mode);

  gcc_assert (BYTES_BIG_ENDIAN);
  gcc_assert (AARCH64_VALID_SIMD_QREG_MODE (mode));

  for (i = 0; i < nunits; i++)
    for (j = 0; j < usize; j++)
      RTVEC_ELT (v, i * usize + j) = GEN_INT ((i + 1) * usize - 1 - j);
  mask = gen_rtx_CONST_VECTOR (V16QImode, v);
  return force_reg (V16QImode, mask);
}

/* Expand an SVE integer comparison using the SVE equivalent of:

     (set TARGET (CODE OP0 OP1)).  */

void
aarch64_expand_sve_vec_cmp_int (rtx target, rtx_code code, rtx op0, rtx op1)
{
  machine_mode pred_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);
  rtx res = aarch64_sve_emit_int_cmp (target, pred_mode, code, data_mode,
				      op0, op1);
  if (!rtx_equal_p (target, res))
    emit_move_insn (target, res);
}

/* Return the UNSPEC_COND_* code for comparison CODE.  */

static unsigned int
aarch64_unspec_cond_code (rtx_code code)
{
  switch (code)
    {
    case NE:
      return UNSPEC_COND_FCMNE;
    case EQ:
      return UNSPEC_COND_FCMEQ;
    case LT:
      return UNSPEC_COND_FCMLT;
    case GT:
      return UNSPEC_COND_FCMGT;
    case LE:
      return UNSPEC_COND_FCMLE;
    case GE:
      return UNSPEC_COND_FCMGE;
    case UNORDERED:
      return UNSPEC_COND_FCMUO;
    default:
      gcc_unreachable ();
    }
}

/* Emit:

      (set TARGET (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X>))

   where <X> is the operation associated with comparison CODE.
   KNOWN_PTRUE_P is true if PRED is known to be a PTRUE.  */

static void
aarch64_emit_sve_fp_cond (rtx target, rtx_code code, rtx pred,
			  bool known_ptrue_p, rtx op0, rtx op1)
{
  rtx flag = gen_int_mode (known_ptrue_p, SImode);
  rtx unspec = gen_rtx_UNSPEC (GET_MODE (pred),
			       gen_rtvec (4, pred, flag, op0, op1),
			       aarch64_unspec_cond_code (code));
  emit_set_insn (target, unspec);
}

/* Emit the SVE equivalent of:

      (set TMP1 (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X1>))
      (set TMP2 (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X2>))
      (set TARGET (ior:PRED_MODE TMP1 TMP2))

   where <Xi> is the operation associated with comparison CODEi.
   KNOWN_PTRUE_P is true if PRED is known to be a PTRUE.  */

static void
aarch64_emit_sve_or_fp_conds (rtx target, rtx_code code1, rtx_code code2,
			      rtx pred, bool known_ptrue_p, rtx op0, rtx op1)
{
  machine_mode pred_mode = GET_MODE (pred);
  rtx tmp1 = gen_reg_rtx (pred_mode);
  aarch64_emit_sve_fp_cond (tmp1, code1, pred, known_ptrue_p, op0, op1);
  rtx tmp2 = gen_reg_rtx (pred_mode);
  aarch64_emit_sve_fp_cond (tmp2, code2, pred, known_ptrue_p, op0, op1);
  aarch64_emit_binop (target, ior_optab, tmp1, tmp2);
}

/* Emit the SVE equivalent of:

      (set TMP (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X>))
      (set TARGET (not TMP))

   where <X> is the operation associated with comparison CODE.
   KNOWN_PTRUE_P is true if PRED is known to be a PTRUE.  */

static void
aarch64_emit_sve_invert_fp_cond (rtx target, rtx_code code, rtx pred,
				 bool known_ptrue_p, rtx op0, rtx op1)
{
  machine_mode pred_mode = GET_MODE (pred);
  rtx tmp = gen_reg_rtx (pred_mode);
  aarch64_emit_sve_fp_cond (tmp, code, pred, known_ptrue_p, op0, op1);
  aarch64_emit_unop (target, one_cmpl_optab, tmp);
}

/* Expand an SVE floating-point comparison using the SVE equivalent of:

     (set TARGET (CODE OP0 OP1))

   If CAN_INVERT_P is true, the caller can also handle inverted results;
   return true if the result is in fact inverted.  */

bool
aarch64_expand_sve_vec_cmp_float (rtx target, rtx_code code,
				  rtx op0, rtx op1, bool can_invert_p)
{
  machine_mode pred_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);

  rtx ptrue = aarch64_ptrue_reg (pred_mode);
  switch (code)
    {
    case UNORDERED:
      /* UNORDERED has no immediate form.  */
      op1 = force_reg (data_mode, op1);
      /* fall through */
    case LT:
    case LE:
    case GT:
    case GE:
    case EQ:
    case NE:
      {
	/* There is native support for the comparison.  */
	aarch64_emit_sve_fp_cond (target, code, ptrue, true, op0, op1);
	return false;
      }

    case LTGT:
      /* This is a trapping operation (LT or GT).  */
      aarch64_emit_sve_or_fp_conds (target, LT, GT, ptrue, true, op0, op1);
      return false;

    case UNEQ:
      if (!flag_trapping_math)
	{
	  /* This would trap for signaling NaNs.  */
	  op1 = force_reg (data_mode, op1);
	  aarch64_emit_sve_or_fp_conds (target, UNORDERED, EQ,
					ptrue, true, op0, op1);
	  return false;
	}
      /* fall through */
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
      if (flag_trapping_math)
	{
	  /* Work out which elements are ordered.  */
	  rtx ordered = gen_reg_rtx (pred_mode);
	  op1 = force_reg (data_mode, op1);
	  aarch64_emit_sve_invert_fp_cond (ordered, UNORDERED,
					   ptrue, true, op0, op1);

	  /* Test the opposite condition for the ordered elements,
	     then invert the result.  */
	  if (code == UNEQ)
	    code = NE;
	  else
	    code = reverse_condition_maybe_unordered (code);
	  if (can_invert_p)
	    {
	      aarch64_emit_sve_fp_cond (target, code,
					ordered, false, op0, op1);
	      return true;
	    }
	  aarch64_emit_sve_invert_fp_cond (target, code,
					   ordered, false, op0, op1);
	  return false;
	}
      break;

    case ORDERED:
      /* ORDERED has no immediate form.  */
      op1 = force_reg (data_mode, op1);
      break;

    default:
      gcc_unreachable ();
    }

  /* There is native support for the inverse comparison.  */
  code = reverse_condition_maybe_unordered (code);
  if (can_invert_p)
    {
      aarch64_emit_sve_fp_cond (target, code, ptrue, true, op0, op1);
      return true;
    }
  aarch64_emit_sve_invert_fp_cond (target, code, ptrue, true, op0, op1);
  return false;
}

/* Expand an SVE vcond pattern with operands OPS.  DATA_MODE is the mode
   of the data being selected and CMP_MODE is the mode of the values being
   compared.  */

void
aarch64_expand_sve_vcond (machine_mode data_mode, machine_mode cmp_mode,
			  rtx *ops)
{
  machine_mode pred_mode = aarch64_get_mask_mode (cmp_mode).require ();
  rtx pred = gen_reg_rtx (pred_mode);
  if (FLOAT_MODE_P (cmp_mode))
    {
      if (aarch64_expand_sve_vec_cmp_float (pred, GET_CODE (ops[3]),
					    ops[4], ops[5], true))
	std::swap (ops[1], ops[2]);
    }
  else
    aarch64_expand_sve_vec_cmp_int (pred, GET_CODE (ops[3]), ops[4], ops[5]);

  if (!aarch64_sve_reg_or_dup_imm (ops[1], data_mode))
    ops[1] = force_reg (data_mode, ops[1]);
  /* The "false" value can only be zero if the "true" value is a constant.  */
  if (register_operand (ops[1], data_mode)
      || !aarch64_simd_reg_or_zero (ops[2], data_mode))
    ops[2] = force_reg (data_mode, ops[2]);

  rtvec vec = gen_rtvec (3, pred, ops[1], ops[2]);
  emit_set_insn (ops[0], gen_rtx_UNSPEC (data_mode, vec, UNSPEC_SEL));
}

/* Return true if:

   (a) MODE1 and MODE2 use the same layout for bytes that are common
       to both modes;

   (b) subregs involving the two modes behave as the target-independent
       subreg rules require; and

   (c) there is at least one register that can hold both modes.

   Return false otherwise.  */

static bool
aarch64_modes_compatible_p (machine_mode mode1, machine_mode mode2)
{
  unsigned int flags1 = aarch64_classify_vector_mode (mode1);
  unsigned int flags2 = aarch64_classify_vector_mode (mode2);

  bool sve1_p = (flags1 & VEC_ANY_SVE);
  bool sve2_p = (flags2 & VEC_ANY_SVE);

  bool partial_sve1_p = sve1_p && (flags1 & VEC_PARTIAL);
  bool partial_sve2_p = sve2_p && (flags2 & VEC_PARTIAL);

  bool pred1_p = (flags1 & VEC_SVE_PRED);
  bool pred2_p = (flags2 & VEC_SVE_PRED);

  bool partial_advsimd_struct1_p = (flags1 == (VEC_ADVSIMD | VEC_STRUCT
					       | VEC_PARTIAL));
  bool partial_advsimd_struct2_p = (flags2 == (VEC_ADVSIMD | VEC_STRUCT
					       | VEC_PARTIAL));

  /* Don't allow changes between predicate modes and other modes.
     Only predicate registers can hold predicate modes and only
     non-predicate registers can hold non-predicate modes, so any
     attempt to mix them would require a round trip through memory.  */
  if (pred1_p != pred2_p)
    return false;

  /* The contents of partial SVE modes are distributed evenly across
     the register, whereas GCC expects them to be clustered together.
     We therefore need to be careful about mode changes involving them.  */
  if (partial_sve1_p && partial_sve2_p)
    {
      /* Reject changes between partial SVE modes that have different
	 patterns of significant and insignificant bits.  */
      if ((aarch64_sve_container_bits (mode1)
	   != aarch64_sve_container_bits (mode2))
	  || GET_MODE_UNIT_SIZE (mode1) != GET_MODE_UNIT_SIZE (mode2))
	return false;
    }
  else if (partial_sve1_p)
    {
      /* The first lane of MODE1 is where GCC expects it, but anything
	 bigger than that is not.  */
      if (maybe_gt (GET_MODE_SIZE (mode2), GET_MODE_UNIT_SIZE (mode1)))
	return false;
    }
  else if (partial_sve2_p)
    {
      /* Similarly in reverse.  */
      if (maybe_gt (GET_MODE_SIZE (mode1), GET_MODE_UNIT_SIZE (mode2)))
	return false;
    }

  /* Don't allow changes between partial Advanced SIMD structure modes
     and other modes that are bigger than 8 bytes.  E.g. V16QI and V2x8QI
     are the same size, but the former occupies one Q register while the
     latter occupies two D registers.  */
  if (partial_advsimd_struct1_p != partial_advsimd_struct2_p
      && maybe_gt (GET_MODE_SIZE (mode1), 8)
      && maybe_gt (GET_MODE_SIZE (mode2), 8))
    return false;

  if (maybe_ne (BITS_PER_SVE_VECTOR, 128u))
    {
      /* Don't allow changes between SVE modes and other modes that might
	 be bigger than 128 bits.  In particular, OImode, CImode and XImode
	 divide into 128-bit quantities while SVE modes divide into
	 BITS_PER_SVE_VECTOR quantities.  */
      if (sve1_p && !sve2_p && maybe_gt (GET_MODE_BITSIZE (mode2), 128))
	return false;
      if (sve2_p && !sve1_p && maybe_gt (GET_MODE_BITSIZE (mode1), 128))
	return false;
    }

  if (BYTES_BIG_ENDIAN)
    {
      /* Don't allow changes between SVE data modes and non-SVE modes.
	 See the comment at the head of aarch64-sve.md for details.  */
      if (sve1_p != sve2_p)
	return false;

      /* Don't allow changes in element size: lane 0 of the new vector
	 would not then be lane 0 of the old vector.  See the comment
	 above aarch64_maybe_expand_sve_subreg_move for a more detailed
	 description.

	 In the worst case, this forces a register to be spilled in
	 one mode and reloaded in the other, which handles the
	 endianness correctly.  */
      if (sve1_p && GET_MODE_UNIT_SIZE (mode1) != GET_MODE_UNIT_SIZE (mode2))
	return false;
    }
  return true;
}

/* Implement TARGET_MODES_TIEABLE_P.  In principle we should always defer
   to aarch64_modes_compatible_p.  However due to issues with register
   allocation it is preferable to avoid tieing integer scalar and FP
   scalar modes.  Executing integer operations in general registers is
   better than treating them as scalar vector operations.  This reduces
   latency and avoids redundant int<->FP moves.  So tie modes if they
   are either the same class, or one of them is a vector mode.  */

static bool
aarch64_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (aarch64_modes_compatible_p (mode1, mode2))
    {
      if (GET_MODE_CLASS (mode1) == GET_MODE_CLASS (mode2))
	return true;
      if (VECTOR_MODE_P (mode1) || VECTOR_MODE_P (mode2))
	return true;
    }
  return false;
}

/* Return a new RTX holding the result of moving POINTER forward by
   AMOUNT bytes.  */

static rtx
aarch64_move_pointer (rtx pointer, poly_int64 amount)
{
  rtx next = plus_constant (Pmode, XEXP (pointer, 0), amount);

  return adjust_automodify_address (pointer, GET_MODE (pointer),
				    next, amount);
}

/* Expand a cpymem/movmem using the MOPS extension.  OPERANDS are taken
   from the cpymem/movmem pattern.  IS_MEMMOVE is true if this is a memmove
   rather than memcpy.  Return true iff we succeeded.  */
bool
aarch64_expand_cpymem_mops (rtx *operands, bool is_memmove)
{
  if (!TARGET_MOPS)
    return false;

  /* All three registers are changed by the instruction, so each one
     must be a fresh pseudo.  */
  rtx dst_addr = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  rtx src_addr = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
  rtx dst_mem = replace_equiv_address (operands[0], dst_addr);
  rtx src_mem = replace_equiv_address (operands[1], src_addr);
  rtx sz_reg = copy_to_mode_reg (DImode, operands[2]);
  if (is_memmove)
    emit_insn (gen_aarch64_movmemdi (dst_mem, src_mem, sz_reg));
  else
    emit_insn (gen_aarch64_cpymemdi (dst_mem, src_mem, sz_reg));
  return true;
}

/* Expand cpymem/movmem, as if from a __builtin_memcpy/memmove.
   OPERANDS are taken from the cpymem/movmem pattern.  IS_MEMMOVE is true
   if this is a memmove rather than memcpy.  Return true if we succeed,
   otherwise return false, indicating that a libcall should be emitted.  */
bool
aarch64_expand_cpymem (rtx *operands, bool is_memmove)
{
  int mode_bytes;
  rtx dst = operands[0];
  rtx src = operands[1];
  unsigned align = UINTVAL (operands[3]);
  rtx base;
  machine_mode mode = BLKmode, next_mode;

  /* Variable-sized or strict-align copies may use the MOPS expansion.  */
  if (!CONST_INT_P (operands[2]) || (STRICT_ALIGNMENT && align < 16))
    return aarch64_expand_cpymem_mops (operands, is_memmove);

  unsigned HOST_WIDE_INT size = UINTVAL (operands[2]);

  /* Set inline limits for memmove/memcpy.  MOPS has a separate threshold.  */
  unsigned max_copy_size = TARGET_SIMD ? 256 : 128;
  unsigned mops_threshold = is_memmove ? aarch64_mops_memmove_size_threshold
				       : aarch64_mops_memcpy_size_threshold;

  /* Reduce the maximum size with -Os.  */
  if (optimize_function_for_size_p (cfun))
    max_copy_size /= 4;

  /* Large copies use MOPS when available or a library call.  */
  if (size > max_copy_size || (TARGET_MOPS && size > mops_threshold))
    return aarch64_expand_cpymem_mops (operands, is_memmove);

  /* Default to 32-byte LDP/STP on large copies, however small copies or
     no SIMD support fall back to 16-byte chunks.
     ??? Although it would be possible to use LDP/STP Qn in streaming mode
     (so using TARGET_BASE_SIMD instead of TARGET_SIMD), it isn't clear
     whether that would improve performance.  */
  bool use_qregs = size > 24 && TARGET_SIMD;

  base = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  dst = adjust_automodify_address (dst, VOIDmode, base, 0);

  base = copy_to_mode_reg (Pmode, XEXP (src, 0));
  src = adjust_automodify_address (src, VOIDmode, base, 0);

  auto_vec<std::pair<rtx, rtx>, 16> ops;
  int offset = 0;

  while (size > 0)
    {
      /* Find the largest mode in which to do the copy in without over reading
	 or writing.  */
      opt_scalar_int_mode mode_iter;
      FOR_EACH_MODE_IN_CLASS (mode_iter, MODE_INT)
	if (GET_MODE_SIZE (mode_iter.require ()) <= MIN (size, 16))
	  mode = mode_iter.require ();

      gcc_assert (mode != BLKmode);

      mode_bytes = GET_MODE_SIZE (mode).to_constant ();

      /* Prefer Q-register accesses.  */
      if (mode_bytes == 16 && use_qregs)
	mode = V4SImode;

      rtx reg = gen_reg_rtx (mode);
      rtx load = gen_move_insn (reg, adjust_address (src, mode, offset));
      rtx store = gen_move_insn (adjust_address (dst, mode, offset), reg);
      ops.safe_push ({ load, store });
      size -= mode_bytes;
      offset += mode_bytes;

      /* Emit trailing copies using overlapping unaligned accesses
	 (when !STRICT_ALIGNMENT) - this is smaller and faster.  */
      if (size > 0 && size < 16 && !STRICT_ALIGNMENT)
	{
	  next_mode = smallest_mode_for_size
	    (size * BITS_PER_UNIT, MODE_INT).require ();
	  int n_bytes = GET_MODE_SIZE (next_mode).to_constant ();
	  gcc_assert (n_bytes <= mode_bytes);
	  offset -= n_bytes - size;
	  size = n_bytes;
	}
    }

  /* Memcpy interleaves loads with stores, memmove emits all loads first.  */
  int nops = ops.length();
  int inc = is_memmove || nops <= 8 ? nops : 6;

  for (int i = 0; i < nops; i += inc)
    {
      int m = MIN (nops, i + inc);
      /* Emit loads.  */
      for (int j = i; j < m; j++)
	emit_insn (ops[j].first);
      /* Emit stores.  */
      for (int j = i; j < m; j++)
	emit_insn (ops[j].second);
    }
  return true;
}

/* Expand a setmem using the MOPS instructions.  OPERANDS are the same
   as for the setmem pattern.  Return true iff we succeed.  */
static bool
aarch64_expand_setmem_mops (rtx *operands)
{
  if (!TARGET_MOPS)
    return false;

  /* The first two registers are changed by the instruction, so both
     of them must be a fresh pseudo.  */
  rtx dst_addr = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  rtx dst_mem = replace_equiv_address (operands[0], dst_addr);
  rtx sz_reg = copy_to_mode_reg (DImode, operands[1]);
  rtx val = operands[2];
  if (val != CONST0_RTX (QImode))
    val = force_reg (QImode, val);
  emit_insn (gen_aarch64_setmemdi (dst_mem, val, sz_reg));
  return true;
}

/* Expand setmem, as if from a __builtin_memset.  Return true if
   we succeed, otherwise return false.  */

bool
aarch64_expand_setmem (rtx *operands)
{
  int mode_bytes;
  unsigned HOST_WIDE_INT len;
  rtx dst = operands[0];
  rtx val = operands[2], src;
  unsigned align = UINTVAL (operands[3]);
  rtx base;
  machine_mode mode = BLKmode, next_mode;

  /* Variable-sized or strict-align memset may use the MOPS expansion.  */
  if (!CONST_INT_P (operands[1]) || !TARGET_SIMD
      || (STRICT_ALIGNMENT && align < 16))
    return aarch64_expand_setmem_mops (operands);

  /* Set inline limits for memset.  MOPS has a separate threshold.  */
  unsigned max_set_size = MAX_SET_SIZE (optimize_function_for_speed_p (cfun));
  unsigned mops_threshold = aarch64_mops_memset_size_threshold;

  len = UINTVAL (operands[1]);

  /* Large memset uses MOPS when available or a library call.  */
  if (len > max_set_size || (TARGET_MOPS && len > mops_threshold))
    return aarch64_expand_setmem_mops (operands);

  base = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  dst = adjust_automodify_address (dst, VOIDmode, base, 0);

  /* Prepare the val using a DUP/MOVI v0.16B, val.  */
  val = expand_vector_broadcast (V16QImode, val);
  val = force_reg (V16QImode, val);

  int offset = 0;
  while (len > 0)
    {
      /* Find the largest mode in which to do the copy without
	 over writing.  */
      opt_scalar_int_mode mode_iter;
      FOR_EACH_MODE_IN_CLASS (mode_iter, MODE_INT)
	if (GET_MODE_SIZE (mode_iter.require ()) <= MIN (len, 16))
	  mode = mode_iter.require ();

      gcc_assert (mode != BLKmode);

      mode_bytes = GET_MODE_SIZE (mode).to_constant ();

      src = val;

      /* Prefer Q-register accesses.  */
      if (mode_bytes == 16)
	mode = V16QImode;
      else
	src = lowpart_subreg (mode, src, GET_MODE (val));

      emit_move_insn (adjust_address (dst, mode, offset), src);
      len -= mode_bytes;
      offset += mode_bytes;

      /* Emit trailing writes using overlapping unaligned accesses
	 (when !STRICT_ALIGNMENT) - this is smaller and faster.  */
      if (len > 0 && len < 16 && !STRICT_ALIGNMENT)
	{
	  next_mode = smallest_mode_for_size
	    (len * BITS_PER_UNIT, MODE_INT).require ();
	  int n_bytes = GET_MODE_SIZE (next_mode).to_constant ();
	  gcc_assert (n_bytes <= mode_bytes);
	  offset -= n_bytes - len;
	  len = n_bytes;
	}
    }

  return true;
}


/* Split a DImode store of a CONST_INT SRC to MEM DST as two
   SImode stores.  Handle the case when the constant has identical
   bottom and top halves.  This is beneficial when the two stores can be
   merged into an STP and we avoid synthesising potentially expensive
   immediates twice.  Return true if such a split is possible.  */

bool
aarch64_split_dimode_const_store (rtx dst, rtx src)
{
  rtx lo = gen_lowpart (SImode, src);
  rtx hi = gen_highpart_mode (SImode, DImode, src);

  if (!rtx_equal_p (lo, hi))
    return false;

  unsigned int orig_cost
    = aarch64_internal_mov_immediate (NULL_RTX, src, false, DImode);
  unsigned int lo_cost
    = aarch64_internal_mov_immediate (NULL_RTX, lo, false, SImode);

  /* We want to transform:
     MOV	x1, 49370
     MOVK	x1, 0x140, lsl 16
     MOVK	x1, 0xc0da, lsl 32
     MOVK	x1, 0x140, lsl 48
     STR	x1, [x0]
   into:
     MOV	w1, 49370
     MOVK	w1, 0x140, lsl 16
     STP	w1, w1, [x0]
   So we want to perform this when we save at least one instruction.  */
  if (orig_cost <= lo_cost)
    return false;

  rtx mem_lo = adjust_address (dst, SImode, 0);
  if (!aarch64_mem_pair_operand (mem_lo, SImode))
    return false;

  rtx tmp_reg = gen_reg_rtx (SImode);
  aarch64_expand_mov_immediate (tmp_reg, lo);
  rtx mem_hi = aarch64_move_pointer (mem_lo, GET_MODE_SIZE (SImode));
  /* Don't emit an explicit store pair as this may not be always profitable.
     Let the sched-fusion logic decide whether to merge them.  */
  emit_move_insn (mem_lo, tmp_reg);
  emit_move_insn (mem_hi, tmp_reg);

  return true;
}

/* Generate RTL for a conditional branch with rtx comparison CODE in
   mode CC_MODE.  The destination of the unlikely conditional branch
   is LABEL_REF.  */

void
aarch64_gen_unlikely_cbranch (enum rtx_code code, machine_mode cc_mode,
			      rtx label_ref)
{
  rtx x;
  x = gen_rtx_fmt_ee (code, VOIDmode,
		      gen_rtx_REG (cc_mode, CC_REGNUM),
		      const0_rtx);

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (VOIDmode, label_ref),
			    pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
}

/* Generate DImode scratch registers for 128-bit (TImode) addition.

   OP1 represents the TImode destination operand 1
   OP2 represents the TImode destination operand 2
   LOW_DEST represents the low half (DImode) of TImode operand 0
   LOW_IN1 represents the low half (DImode) of TImode operand 1
   LOW_IN2 represents the low half (DImode) of TImode operand 2
   HIGH_DEST represents the high half (DImode) of TImode operand 0
   HIGH_IN1 represents the high half (DImode) of TImode operand 1
   HIGH_IN2 represents the high half (DImode) of TImode operand 2.  */

void
aarch64_addti_scratch_regs (rtx op1, rtx op2, rtx *low_dest,
			    rtx *low_in1, rtx *low_in2,
			    rtx *high_dest, rtx *high_in1,
			    rtx *high_in2)
{
  *low_dest = gen_reg_rtx (DImode);
  *low_in1 = force_lowpart_subreg (DImode, op1, TImode);
  *low_in2 = force_lowpart_subreg (DImode, op2, TImode);
  *high_dest = gen_reg_rtx (DImode);
  *high_in1 = force_highpart_subreg (DImode, op1, TImode);
  *high_in2 = force_highpart_subreg (DImode, op2, TImode);
}

/* Generate DImode scratch registers for 128-bit (TImode) subtraction.

   OP1 represents the TImode destination operand 1
   OP2 represents the TImode destination operand 2
   LOW_DEST represents the low half (DImode) of TImode operand 0
   LOW_IN1 represents the low half (DImode) of TImode operand 1
   LOW_IN2 represents the low half (DImode) of TImode operand 2
   HIGH_DEST represents the high half (DImode) of TImode operand 0
   HIGH_IN1 represents the high half (DImode) of TImode operand 1
   HIGH_IN2 represents the high half (DImode) of TImode operand 2.  */


void
aarch64_subvti_scratch_regs (rtx op1, rtx op2, rtx *low_dest,
			     rtx *low_in1, rtx *low_in2,
			     rtx *high_dest, rtx *high_in1,
			     rtx *high_in2)
{
  *low_dest = gen_reg_rtx (DImode);
  *low_in1 = force_lowpart_subreg (DImode, op1, TImode);
  *low_in2 = force_lowpart_subreg (DImode, op2, TImode);
  *high_dest = gen_reg_rtx (DImode);

  *high_in1 = force_highpart_subreg (DImode, op1, TImode);
  *high_in2 = force_highpart_subreg (DImode, op2, TImode);
}

/* Generate RTL for 128-bit (TImode) subtraction with overflow.

   OP0 represents the TImode destination operand 0
   LOW_DEST represents the low half (DImode) of TImode operand 0
   LOW_IN1 represents the low half (DImode) of TImode operand 1
   LOW_IN2 represents the low half (DImode) of TImode operand 2
   HIGH_DEST represents the high half (DImode) of TImode operand 0
   HIGH_IN1 represents the high half (DImode) of TImode operand 1
   HIGH_IN2 represents the high half (DImode) of TImode operand 2
   UNSIGNED_P is true if the operation is being performed on unsigned
   values.  */
void
aarch64_expand_subvti (rtx op0, rtx low_dest, rtx low_in1,
		       rtx low_in2, rtx high_dest, rtx high_in1,
		       rtx high_in2, bool unsigned_p)
{
  if (low_in2 == const0_rtx)
    {
      low_dest = low_in1;
      high_in2 = force_reg (DImode, high_in2);
      if (unsigned_p)
	emit_insn (gen_subdi3_compare1 (high_dest, high_in1, high_in2));
      else
	emit_insn (gen_subvdi_insn (high_dest, high_in1, high_in2));
    }
  else
    {
      if (aarch64_plus_immediate (low_in2, DImode))
	emit_insn (gen_subdi3_compare1_imm (low_dest, low_in1, low_in2,
					    GEN_INT (-UINTVAL (low_in2))));
      else
	{
	  low_in2 = force_reg (DImode, low_in2);
	  emit_insn (gen_subdi3_compare1 (low_dest, low_in1, low_in2));
	}
      high_in2 = force_reg (DImode, high_in2);

      if (unsigned_p)
	emit_insn (gen_usubdi3_carryinC (high_dest, high_in1, high_in2));
      else
	emit_insn (gen_subdi3_carryinV (high_dest, high_in1, high_in2));
    }

  emit_move_insn (gen_lowpart (DImode, op0), low_dest);
  emit_move_insn (gen_highpart (DImode, op0), high_dest);

}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
aarch64_asan_shadow_offset (void)
{
  if (TARGET_ILP32)
    return (HOST_WIDE_INT_1 << 29);
  else
    return (HOST_WIDE_INT_1 << 36);
}

static rtx
aarch64_gen_ccmp_first (rtx_insn **prep_seq, rtx_insn **gen_seq,
			rtx_code code, tree treeop0, tree treeop1)
{
  machine_mode op_mode, cmp_mode, cc_mode = CCmode;
  rtx op0, op1;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));
  insn_code icode;
  struct expand_operand ops[4];

  start_sequence ();
  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);

  op_mode = GET_MODE (op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (op1);

  if (CONST_SCALAR_INT_P (op1))
    canonicalize_comparison (op_mode, &code, &op1);

  switch (op_mode)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
      cmp_mode = SImode;
      icode = CODE_FOR_cmpsi;
      break;

    case E_DImode:
      cmp_mode = DImode;
      icode = CODE_FOR_cmpdi;
      break;

    case E_SFmode:
      cmp_mode = SFmode;
      cc_mode = aarch64_select_cc_mode (code, op0, op1);
      icode = cc_mode == CCFPEmode ? CODE_FOR_fcmpesf : CODE_FOR_fcmpsf;
      break;

    case E_DFmode:
      cmp_mode = DFmode;
      cc_mode = aarch64_select_cc_mode (code, op0, op1);
      icode = cc_mode == CCFPEmode ? CODE_FOR_fcmpedf : CODE_FOR_fcmpdf;
      break;

    default:
      end_sequence ();
      return NULL_RTX;
    }

  op0 = prepare_operand (icode, op0, 0, op_mode, cmp_mode, unsignedp);
  op1 = prepare_operand (icode, op1, 1, op_mode, cmp_mode, unsignedp);
  if (!op0 || !op1)
    {
      end_sequence ();
      return NULL_RTX;
    }
  *prep_seq = get_insns ();
  end_sequence ();

  create_fixed_operand (&ops[0], op0);
  create_fixed_operand (&ops[1], op1);

  start_sequence ();
  if (!maybe_expand_insn (icode, 2, ops))
    {
      end_sequence ();
      return NULL_RTX;
    }
  *gen_seq = get_insns ();
  end_sequence ();

  return gen_rtx_fmt_ee (code, cc_mode,
			 gen_rtx_REG (cc_mode, CC_REGNUM), const0_rtx);
}

static rtx
aarch64_gen_ccmp_next (rtx_insn **prep_seq, rtx_insn **gen_seq, rtx prev,
		       rtx_code cmp_code, tree treeop0, tree treeop1,
		       rtx_code bit_code)
{
  rtx op0, op1, target;
  machine_mode op_mode, cmp_mode, cc_mode = CCmode;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));
  insn_code icode;
  struct expand_operand ops[6];
  int aarch64_cond;

  push_to_sequence (*prep_seq);
  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);

  op_mode = GET_MODE (op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (op1);

  if (CONST_SCALAR_INT_P (op1))
    canonicalize_comparison (op_mode, &cmp_code, &op1);

  switch (op_mode)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
      cmp_mode = SImode;
      break;

    case E_DImode:
      cmp_mode = DImode;
      break;

    case E_SFmode:
      cmp_mode = SFmode;
      cc_mode = aarch64_select_cc_mode (cmp_code, op0, op1);
      break;

    case E_DFmode:
      cmp_mode = DFmode;
      cc_mode = aarch64_select_cc_mode (cmp_code, op0, op1);
      break;

    default:
      end_sequence ();
      return NULL_RTX;
    }

  icode = code_for_ccmp (cc_mode, cmp_mode);

  op0 = prepare_operand (icode, op0, 2, op_mode, cmp_mode, unsignedp);
  op1 = prepare_operand (icode, op1, 3, op_mode, cmp_mode, unsignedp);
  if (!op0 || !op1)
    {
      end_sequence ();
      return NULL_RTX;
    }
  *prep_seq = get_insns ();
  end_sequence ();

  target = gen_rtx_REG (cc_mode, CC_REGNUM);
  aarch64_cond = aarch64_get_condition_code_1 (cc_mode, cmp_code);

  if (bit_code != AND)
    {
      /* Treat the ccmp patterns as canonical and use them where possible,
	 but fall back to ccmp_rev patterns if there's no other option.  */
      rtx_code prev_code = GET_CODE (prev);
      machine_mode prev_mode = GET_MODE (XEXP (prev, 0));
      if ((prev_mode == CCFPmode || prev_mode == CCFPEmode)
	  && !(prev_code == EQ
	       || prev_code == NE
	       || prev_code == ORDERED
	       || prev_code == UNORDERED))
	icode = code_for_ccmp_rev (cc_mode, cmp_mode);
      else
	{
	  rtx_code code = reverse_condition (prev_code);
	  prev = gen_rtx_fmt_ee (code, VOIDmode, XEXP (prev, 0), const0_rtx);
	}
      aarch64_cond = AARCH64_INVERSE_CONDITION_CODE (aarch64_cond);
    }

  create_fixed_operand (&ops[0], XEXP (prev, 0));
  create_fixed_operand (&ops[1], target);
  create_fixed_operand (&ops[2], op0);
  create_fixed_operand (&ops[3], op1);
  create_fixed_operand (&ops[4], prev);
  create_fixed_operand (&ops[5], GEN_INT (aarch64_cond));

  push_to_sequence (*gen_seq);
  if (!maybe_expand_insn (icode, 6, ops))
    {
      end_sequence ();
      return NULL_RTX;
    }

  *gen_seq = get_insns ();
  end_sequence ();

  return gen_rtx_fmt_ee (cmp_code, VOIDmode, target, const0_rtx);
}

#undef TARGET_GEN_CCMP_FIRST
#define TARGET_GEN_CCMP_FIRST aarch64_gen_ccmp_first

#undef TARGET_GEN_CCMP_NEXT
#define TARGET_GEN_CCMP_NEXT aarch64_gen_ccmp_next

/* Implement TARGET_SCHED_MACRO_FUSION_P.  Return true if target supports
   instruction fusion of some sort.  */

static bool
aarch64_macro_fusion_p (void)
{
  return aarch64_tune_params.fusible_ops != AARCH64_FUSE_NOTHING;
}


/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

static bool
aarch_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  rtx set_dest;
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  /* prev and curr are simple SET insns i.e. no flag setting or branching.  */
  bool simple_sets_p = prev_set && curr_set && !any_condjump_p (curr);

  if (!aarch64_macro_fusion_p ())
    return false;

  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_MOV_MOVK))
    {
      /* We are trying to match:
         prev (mov)  == (set (reg r0) (const_int imm16))
         curr (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 16))
                             (const_int imm16_1))  */

      set_dest = SET_DEST (curr_set);

      if (GET_CODE (set_dest) == ZERO_EXTRACT
          && CONST_INT_P (SET_SRC (curr_set))
          && CONST_INT_P (SET_SRC (prev_set))
          && CONST_INT_P (XEXP (set_dest, 2))
          && INTVAL (XEXP (set_dest, 2)) == 16
          && REG_P (XEXP (set_dest, 0))
          && REG_P (SET_DEST (prev_set))
          && REGNO (XEXP (set_dest, 0)) == REGNO (SET_DEST (prev_set)))
        {
          return true;
        }
    }

  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_ADRP_ADD))
    {

      /*  We're trying to match:
          prev (adrp) == (set (reg r1)
                              (high (symbol_ref ("SYM"))))
          curr (add) == (set (reg r0)
                             (lo_sum (reg r1)
                                     (symbol_ref ("SYM"))))
          Note that r0 need not necessarily be the same as r1, especially
          during pre-regalloc scheduling.  */

      if (satisfies_constraint_Ush (SET_SRC (prev_set))
          && REG_P (SET_DEST (prev_set)) && REG_P (SET_DEST (curr_set)))
        {
          if (GET_CODE (SET_SRC (curr_set)) == LO_SUM
              && REG_P (XEXP (SET_SRC (curr_set), 0))
              && REGNO (XEXP (SET_SRC (curr_set), 0))
                 == REGNO (SET_DEST (prev_set))
              && rtx_equal_p (XEXP (SET_SRC (prev_set), 0),
                              XEXP (SET_SRC (curr_set), 1)))
            return true;
        }
    }

  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_MOVK_MOVK))
    {

      /* We're trying to match:
         prev (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 32))
                             (const_int imm16_1))
         curr (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 48))
                             (const_int imm16_2))  */

      if (GET_CODE (SET_DEST (prev_set)) == ZERO_EXTRACT
          && GET_CODE (SET_DEST (curr_set)) == ZERO_EXTRACT
          && REG_P (XEXP (SET_DEST (prev_set), 0))
          && REG_P (XEXP (SET_DEST (curr_set), 0))
          && REGNO (XEXP (SET_DEST (prev_set), 0))
             == REGNO (XEXP (SET_DEST (curr_set), 0))
          && CONST_INT_P (XEXP (SET_DEST (prev_set), 2))
          && CONST_INT_P (XEXP (SET_DEST (curr_set), 2))
          && INTVAL (XEXP (SET_DEST (prev_set), 2)) == 32
          && INTVAL (XEXP (SET_DEST (curr_set), 2)) == 48
          && CONST_INT_P (SET_SRC (prev_set))
          && CONST_INT_P (SET_SRC (curr_set)))
        return true;

    }
  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_ADRP_LDR))
    {
      /* We're trying to match:
          prev (adrp) == (set (reg r0)
                              (high (symbol_ref ("SYM"))))
          curr (ldr) == (set (reg r1)
                             (mem (lo_sum (reg r0)
                                             (symbol_ref ("SYM")))))
                 or
          curr (ldr) == (set (reg r1)
                             (zero_extend (mem
                                           (lo_sum (reg r0)
                                                   (symbol_ref ("SYM"))))))  */
      if (satisfies_constraint_Ush (SET_SRC (prev_set))
          && REG_P (SET_DEST (prev_set)) && REG_P (SET_DEST (curr_set)))
        {
          rtx curr_src = SET_SRC (curr_set);

          if (GET_CODE (curr_src) == ZERO_EXTEND)
            curr_src = XEXP (curr_src, 0);

          if (MEM_P (curr_src) && GET_CODE (XEXP (curr_src, 0)) == LO_SUM
              && REG_P (XEXP (XEXP (curr_src, 0), 0))
              && REGNO (XEXP (XEXP (curr_src, 0), 0))
                 == REGNO (SET_DEST (prev_set))
              && rtx_equal_p (XEXP (XEXP (curr_src, 0), 1),
                              XEXP (SET_SRC (prev_set), 0)))
              return true;
        }
    }

  /* Fuse compare (CMP/CMN/TST/BICS) and conditional branch.  */
  if (aarch64_fusion_enabled_p (AARCH64_FUSE_CMP_BRANCH)
      && prev_set && curr_set && any_condjump_p (curr)
      && GET_CODE (SET_SRC (prev_set)) == COMPARE
      && SCALAR_INT_MODE_P (GET_MODE (XEXP (SET_SRC (prev_set), 0)))
      && reg_referenced_p (SET_DEST (prev_set), PATTERN (curr)))
    return true;

  /* Fuse CMP and CSEL/CSET.  */
  if (prev_set && curr_set
      && GET_CODE (SET_SRC (prev_set)) == COMPARE
      && SCALAR_INT_MODE_P (GET_MODE (XEXP (SET_SRC (prev_set), 0)))
      && reg_referenced_p (SET_DEST (prev_set), PATTERN (curr)))
    {
      enum attr_type prev_type = get_attr_type (prev);
      if ((prev_type == TYPE_ALUS_SREG || prev_type == TYPE_ALUS_IMM)
	  && ((aarch64_fusion_enabled_p (AARCH64_FUSE_CMP_CSEL)
	       && GET_CODE (SET_SRC (curr_set)) == IF_THEN_ELSE
	       && aarch64_reg_or_zero (XEXP (SET_SRC (curr_set), 1), VOIDmode)
	       && aarch64_reg_or_zero (XEXP (SET_SRC (curr_set), 2), VOIDmode)
	       && SCALAR_INT_MODE_P (GET_MODE (XEXP (SET_SRC (curr_set), 1))))
	      || (aarch64_fusion_enabled_p (AARCH64_FUSE_CMP_CSET)
		  && GET_RTX_CLASS (GET_CODE (SET_SRC (curr_set)))
		     == RTX_COMPARE
		  && REG_P (SET_DEST (curr_set)))))
	return true;
    }

  /* Fuse flag-setting ALU instructions and conditional branch.  */
  if (aarch64_fusion_enabled_p (AARCH64_FUSE_ALU_BRANCH)
      && any_condjump_p (curr))
    {
      unsigned int condreg1, condreg2;
      rtx cc_reg_1;
      aarch64_fixed_condition_code_regs (&condreg1, &condreg2);
      cc_reg_1 = gen_rtx_REG (CCmode, condreg1);

      if (reg_referenced_p (cc_reg_1, PATTERN (curr))
	  && prev
	  && modified_in_p (cc_reg_1, prev))
	{
	  enum attr_type prev_type = get_attr_type (prev);

	  /* FIXME: this misses some which is considered simple arthematic
	     instructions for ThunderX.  Simple shifts are missed here.  */
	  if (prev_type == TYPE_ALUS_SREG
	      || prev_type == TYPE_ALUS_IMM
	      || prev_type == TYPE_LOGICS_REG
	      || prev_type == TYPE_LOGICS_IMM)
	    return true;
	}
    }

  /* Fuse ALU instructions and CBZ/CBNZ.  */
  if (prev_set
      && curr_set
      && aarch64_fusion_enabled_p (AARCH64_FUSE_ALU_CBZ)
      && any_condjump_p (curr))
    {
      /* We're trying to match:
	  prev (alu_insn) == (set (r0) plus ((r0) (r1/imm)))
	  curr (cbz) ==  (set (pc) (if_then_else (eq/ne) (r0)
							 (const_int 0))
						 (label_ref ("SYM"))
						 (pc))  */
      if (SET_DEST (curr_set) == (pc_rtx)
	  && GET_CODE (SET_SRC (curr_set)) == IF_THEN_ELSE
	  && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  && REG_P (SET_DEST (prev_set))
	  && REGNO (SET_DEST (prev_set))
	     == REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0)))
	{
	  /* Fuse ALU operations followed by conditional branch instruction.  */
	  switch (get_attr_type (prev))
	    {
	    case TYPE_ALU_IMM:
	    case TYPE_ALU_SREG:
	    case TYPE_ADC_REG:
	    case TYPE_ADC_IMM:
	    case TYPE_ADCS_REG:
	    case TYPE_ADCS_IMM:
	    case TYPE_LOGIC_REG:
	    case TYPE_LOGIC_IMM:
	    case TYPE_CSEL:
	    case TYPE_ADR:
	    case TYPE_MOV_IMM:
	    case TYPE_SHIFT_REG:
	    case TYPE_SHIFT_IMM:
	    case TYPE_BFM:
	    case TYPE_RBIT:
	    case TYPE_REV:
	    case TYPE_EXTEND:
	      return true;

	    default:;
	    }
	}
    }

  /* Fuse A+B+1 and A-B-1 */
  if (simple_sets_p
      && aarch64_fusion_enabled_p (AARCH64_FUSE_ADDSUB_2REG_CONST1))
    {
      /* We're trying to match:
	  prev == (set (r0) (plus (r0) (r1)))
	  curr == (set (r0) (plus (r0) (const_int 1)))
	or:
	  prev == (set (r0) (minus (r0) (r1)))
	  curr == (set (r0) (plus (r0) (const_int -1))) */

      rtx prev_src = SET_SRC (prev_set);
      rtx curr_src = SET_SRC (curr_set);

      int polarity = 1;
      if (GET_CODE (prev_src) == MINUS)
	polarity = -1;

      if (GET_CODE (curr_src) == PLUS
	  && (GET_CODE (prev_src) == PLUS || GET_CODE (prev_src) == MINUS)
	  && CONST_INT_P (XEXP (curr_src, 1))
	  && INTVAL (XEXP (curr_src, 1)) == polarity
	  && REG_P (XEXP (curr_src, 0))
	  && REG_P (SET_DEST (prev_set))
	  && REGNO (SET_DEST (prev_set)) == REGNO (XEXP (curr_src, 0)))
	return true;
    }

  return false;
}

/* Return true iff the instruction fusion described by OP is enabled.  */

bool
aarch64_fusion_enabled_p (enum aarch64_fusion_pairs op)
{
  return (aarch64_tune_params.fusible_ops & op) != 0;
}

/* If MEM is in the form of [base+offset], extract the two parts
   of address and set to BASE and OFFSET, otherwise return false
   after clearing BASE and OFFSET.  */

bool
extract_base_offset_in_addr (rtx mem, rtx *base, rtx *offset)
{
  rtx addr;

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  if (REG_P (addr))
    {
      *base = addr;
      *offset = const0_rtx;
      return true;
    }

  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && CONST_INT_P (XEXP (addr, 1)))
    {
      *base = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return true;
    }

  *base = NULL_RTX;
  *offset = NULL_RTX;

  return false;
}

/* Types for scheduling fusion.  */
enum sched_fusion_type
{
  SCHED_FUSION_NONE = 0,
  SCHED_FUSION_LD_SIGN_EXTEND,
  SCHED_FUSION_LD_ZERO_EXTEND,
  SCHED_FUSION_LD,
  SCHED_FUSION_ST,
  SCHED_FUSION_NUM
};

/* If INSN is a load or store of address in the form of [base+offset],
   extract the two parts and set to BASE and OFFSET.  Return scheduling
   fusion type this INSN is.  */

static enum sched_fusion_type
fusion_load_store (rtx_insn *insn, rtx *base, rtx *offset)
{
  rtx x, dest, src;
  enum sched_fusion_type fusion = SCHED_FUSION_LD;

  gcc_assert (INSN_P (insn));
  x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return SCHED_FUSION_NONE;

  src = SET_SRC (x);
  dest = SET_DEST (x);

  machine_mode dest_mode = GET_MODE (dest);

  if (!aarch64_mode_valid_for_sched_fusion_p (dest_mode))
    return SCHED_FUSION_NONE;

  if (GET_CODE (src) == SIGN_EXTEND)
    {
      fusion = SCHED_FUSION_LD_SIGN_EXTEND;
      src = XEXP (src, 0);
      if (!MEM_P (src) || GET_MODE (src) != SImode)
	return SCHED_FUSION_NONE;
    }
  else if (GET_CODE (src) == ZERO_EXTEND)
    {
      fusion = SCHED_FUSION_LD_ZERO_EXTEND;
      src = XEXP (src, 0);
      if (!MEM_P (src) || GET_MODE (src) != SImode)
	return SCHED_FUSION_NONE;
    }

  if (MEM_P (src) && REG_P (dest))
    extract_base_offset_in_addr (src, base, offset);
  else if (MEM_P (dest) && (REG_P (src) || src == const0_rtx))
    {
      fusion = SCHED_FUSION_ST;
      extract_base_offset_in_addr (dest, base, offset);
    }
  else
    return SCHED_FUSION_NONE;

  if (*base == NULL_RTX || *offset == NULL_RTX)
    fusion = SCHED_FUSION_NONE;

  return fusion;
}

/* Implement the TARGET_SCHED_FUSION_PRIORITY hook.

   Currently we only support to fuse ldr or str instructions, so FUSION_PRI
   and PRI are only calculated for these instructions.  For other instruction,
   FUSION_PRI and PRI are simply set to MAX_PRI - 1.  In the future, other
   type instruction fusion can be added by returning different priorities.

   It's important that irrelevant instructions get the largest FUSION_PRI.  */

static void
aarch64_sched_fusion_priority (rtx_insn *insn, int max_pri,
			       int *fusion_pri, int *pri)
{
  int tmp, off_val;
  rtx base, offset;
  enum sched_fusion_type fusion;

  gcc_assert (INSN_P (insn));

  tmp = max_pri - 1;
  fusion = fusion_load_store (insn, &base, &offset);
  if (fusion == SCHED_FUSION_NONE)
    {
      *pri = tmp;
      *fusion_pri = tmp;
      return;
    }

  /* Set FUSION_PRI according to fusion type and base register.  */
  *fusion_pri = tmp - fusion * FIRST_PSEUDO_REGISTER - REGNO (base);

  /* Calculate PRI.  */
  tmp /= 2;

  /* INSN with smaller offset goes first.  */
  off_val = (int)(INTVAL (offset));
  if (off_val >= 0)
    tmp -= (off_val & 0xfffff);
  else
    tmp += ((- off_val) & 0xfffff);

  *pri = tmp;
  return;
}

/* Implement the TARGET_SCHED_ADJUST_PRIORITY hook.
   Adjust priority of sha1h instructions so they are scheduled before
   other SHA1 instructions.  */

static int
aarch64_sched_adjust_priority (rtx_insn *insn, int priority)
{
  rtx x = PATTERN (insn);

  if (GET_CODE (x) == SET)
    {
      x = SET_SRC (x);

      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_SHA1H)
	return priority + 10;
    }

  return priority;
}

/* If REVERSED is null, return true if memory reference *MEM2 comes
   immediately after memory reference *MEM1.  Do not change the references
   in this case.

   Otherwise, check if *MEM1 and *MEM2 are consecutive memory references and,
   if they are, try to make them use constant offsets from the same base
   register.  Return true on success.  When returning true, set *REVERSED
   to true if *MEM1 comes after *MEM2, false if *MEM1 comes before *MEM2.  */
static bool
aarch64_check_consecutive_mems (rtx *mem1, rtx *mem2, bool *reversed)
{
  if (reversed)
    *reversed = false;

  if (GET_RTX_CLASS (GET_CODE (XEXP (*mem1, 0))) == RTX_AUTOINC
      || GET_RTX_CLASS (GET_CODE (XEXP (*mem2, 0))) == RTX_AUTOINC)
    return false;

  if (!MEM_SIZE_KNOWN_P (*mem1) || !MEM_SIZE_KNOWN_P (*mem2))
    return false;

  auto size1 = MEM_SIZE (*mem1);
  auto size2 = MEM_SIZE (*mem2);

  rtx base1, base2, offset1, offset2;
  extract_base_offset_in_addr (*mem1, &base1, &offset1);
  extract_base_offset_in_addr (*mem2, &base2, &offset2);

  /* Make sure at least one memory is in base+offset form.  */
  if (!(base1 && offset1) && !(base2 && offset2))
    return false;

  /* If both mems already use the same base register, just check the
     offsets.  */
  if (base1 && base2 && rtx_equal_p (base1, base2))
    {
      if (!offset1 || !offset2)
	return false;

      if (known_eq (UINTVAL (offset1) + size1, UINTVAL (offset2)))
	return true;

      if (known_eq (UINTVAL (offset2) + size2, UINTVAL (offset1)) && reversed)
	{
	  *reversed = true;
	  return true;
	}

      return false;
    }

  /* Otherwise, check whether the MEM_EXPRs and MEM_OFFSETs together
     guarantee that the values are consecutive.  */
  if (MEM_EXPR (*mem1)
      && MEM_EXPR (*mem2)
      && MEM_OFFSET_KNOWN_P (*mem1)
      && MEM_OFFSET_KNOWN_P (*mem2))
    {
      poly_int64 expr_offset1;
      poly_int64 expr_offset2;
      tree expr_base1 = get_addr_base_and_unit_offset (MEM_EXPR (*mem1),
						       &expr_offset1);
      tree expr_base2 = get_addr_base_and_unit_offset (MEM_EXPR (*mem2),
						       &expr_offset2);
      if (!expr_base1
	  || !expr_base2
	  || !DECL_P (expr_base1)
	  || !operand_equal_p (expr_base1, expr_base2, OEP_ADDRESS_OF))
	return false;

      expr_offset1 += MEM_OFFSET (*mem1);
      expr_offset2 += MEM_OFFSET (*mem2);

      if (known_eq (expr_offset1 + size1, expr_offset2))
	;
      else if (known_eq (expr_offset2 + size2, expr_offset1) && reversed)
	*reversed = true;
      else
	return false;

      if (reversed)
	{
	  if (base2)
	    {
	      rtx addr1 = plus_constant (Pmode, XEXP (*mem2, 0),
					 expr_offset1 - expr_offset2);
	      *mem1 = replace_equiv_address_nv (*mem1, addr1);
	    }
	  else
	    {
	      rtx addr2 = plus_constant (Pmode, XEXP (*mem1, 0),
					 expr_offset2 - expr_offset1);
	      *mem2 = replace_equiv_address_nv (*mem2, addr2);
	    }
	}
      return true;
    }

  return false;
}

/* Test if MODE is suitable for a single transfer register in an ldp or stp
   instruction.  */

bool
aarch64_ldpstp_operand_mode_p (machine_mode mode)
{
  if (!targetm.hard_regno_mode_ok (V0_REGNUM, mode)
      || hard_regno_nregs (V0_REGNUM, mode) > 1)
    return false;

  const auto size = GET_MODE_SIZE (mode);
  return known_eq (size, 4) || known_eq (size, 8) || known_eq (size, 16);
}

/* Return true if MEM1 and MEM2 can be combined into a single access
   of mode MODE, with the combined access having the same address as MEM1.  */

bool
aarch64_mergeable_load_pair_p (machine_mode mode, rtx mem1, rtx mem2)
{
  if (STRICT_ALIGNMENT && MEM_ALIGN (mem1) < GET_MODE_ALIGNMENT (mode))
    return false;
  return aarch64_check_consecutive_mems (&mem1, &mem2, nullptr);
}

/* Return true if MEM agrees with the ldp-stp policy model.
   Otherwise, false.  */

bool
aarch64_mem_ok_with_ldpstp_policy_model (rtx mem, bool load, machine_mode mode)
{
  auto policy = (load
		 ? aarch64_tune_params.ldp_policy_model
		 : aarch64_tune_params.stp_policy_model);

  /* If we have AARCH64_LDP_STP_POLICY_NEVER, reject the load pair.  */
  if (policy == AARCH64_LDP_STP_POLICY_NEVER)
    return false;

  /* If we have AARCH64_LDP_STP_POLICY_ALIGNED,
     do not emit the load pair unless the alignment is checked to be
     at least double the alignment of the type.  */
  if (policy == AARCH64_LDP_STP_POLICY_ALIGNED
      && !optimize_function_for_size_p (cfun)
      && MEM_ALIGN (mem) < 2 * GET_MODE_ALIGNMENT (mode))
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into ldp/stp.  LOAD is true if they are load instructions.  */

bool
aarch64_operands_ok_for_ldpstp (rtx *operands, bool load)
{
  enum reg_class rclass_1, rclass_2;
  rtx mem_1, mem_2, reg_1, reg_2;

  if (load)
    {
      mem_1 = operands[1];
      mem_2 = operands[3];
      reg_1 = operands[0];
      reg_2 = operands[2];
      gcc_assert (REG_P (reg_1) && REG_P (reg_2));
      if (REGNO (reg_1) == REGNO (reg_2))
	return false;
      if (reg_overlap_mentioned_p (reg_1, mem_2))
	return false;
    }
  else
    {
      mem_1 = operands[0];
      mem_2 = operands[2];
      reg_1 = operands[1];
      reg_2 = operands[3];
    }

  /* The mems cannot be volatile.  */
  if (MEM_VOLATILE_P (mem_1) || MEM_VOLATILE_P (mem_2))
    return false;

  /* Check if the addresses are in the form of [base+offset].  */
  bool reversed = false;
  if (!aarch64_check_consecutive_mems (&mem_1, &mem_2, &reversed))
    return false;

  /* The operands must be of the same size.  */
  gcc_assert (known_eq (GET_MODE_SIZE (GET_MODE (mem_1)),
			GET_MODE_SIZE (GET_MODE (mem_2))));

  /* The lower memory access must be a mem-pair operand.  */
  rtx lower_mem = reversed ? mem_2 : mem_1;
  machine_mode lower_mem_mode = GET_MODE (lower_mem);
  if (!aarch64_mem_pair_operand (lower_mem, lower_mem_mode))
    return false;

  /* Check if lower_mem is ok with the ldp-stp policy model.  */
  if (!aarch64_mem_ok_with_ldpstp_policy_model (lower_mem, load,
						lower_mem_mode))
    return false;

  if (REG_P (reg_1) && FP_REGNUM_P (REGNO (reg_1)))
    rclass_1 = FP_REGS;
  else
    rclass_1 = GENERAL_REGS;

  if (REG_P (reg_2) && FP_REGNUM_P (REGNO (reg_2)))
    rclass_2 = FP_REGS;
  else
    rclass_2 = GENERAL_REGS;

  /* Check if the registers are of same class.  */
  if (rclass_1 != rclass_2)
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store that can be merged,
   swap them if they are not in ascending order.  */
void
aarch64_swap_ldrstr_operands (rtx* operands, bool load)
{
  int mem_op = load ? 1 : 0;
  bool reversed = false;
  if (!aarch64_check_consecutive_mems (operands + mem_op,
				       operands + mem_op + 2, &reversed))
    gcc_unreachable ();

  if (reversed)
    {
      /* Irrespective of whether this is a load or a store,
	 we do the same swap.  */
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
}

/* Helper function used for generation of load/store pair instructions, called
   from peepholes in aarch64-ldpstp.md.  OPERANDS is an array of
   operands as matched by the peepholes in that file.  LOAD_P is true if we're
   generating a load pair, otherwise we're generating a store pair.  CODE is
   either {ZERO,SIGN}_EXTEND for extending loads or UNKNOWN if we're generating a
   standard load/store pair.  */

void
aarch64_finish_ldpstp_peephole (rtx *operands, bool load_p, enum rtx_code code)
{
  aarch64_swap_ldrstr_operands (operands, load_p);

  if (load_p)
    emit_insn (aarch64_gen_load_pair (operands[0], operands[2],
				      operands[1], code));
  else
    {
      gcc_assert (code == UNKNOWN);
      emit_insn (aarch64_gen_store_pair (operands[0], operands[1],
					 operands[3]));
    }
}

/* Taking X and Y to be HOST_WIDE_INT pointers, return the result of a
   comparison between the two.  */
int
aarch64_host_wide_int_compare (const void *x, const void *y)
{
  return wi::cmps (* ((const HOST_WIDE_INT *) x),
		   * ((const HOST_WIDE_INT *) y));
}

/* Taking X and Y to be pairs of RTX, one pointing to a MEM rtx and the
   other pointing to a REG rtx containing an offset, compare the offsets
   of the two pairs.

   Return:

	1 iff offset (X) > offset (Y)
	0 iff offset (X) == offset (Y)
	-1 iff offset (X) < offset (Y)  */
int
aarch64_ldrstr_offset_compare (const void *x, const void *y)
{
  const rtx * operands_1 = (const rtx *) x;
  const rtx * operands_2 = (const rtx *) y;
  rtx mem_1, mem_2, base, offset_1, offset_2;

  if (MEM_P (operands_1[0]))
    mem_1 = operands_1[0];
  else
    mem_1 = operands_1[1];

  if (MEM_P (operands_2[0]))
    mem_2 = operands_2[0];
  else
    mem_2 = operands_2[1];

  /* Extract the offsets.  */
  extract_base_offset_in_addr (mem_1, &base, &offset_1);
  extract_base_offset_in_addr (mem_2, &base, &offset_2);

  gcc_assert (offset_1 != NULL_RTX && offset_2 != NULL_RTX);

  return wi::cmps (INTVAL (offset_1), INTVAL (offset_2));
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into ldp/stp by adjusting the offset.  LOAD is true if they
   are load instructions.  MODE is the mode of memory operands.

   Given below consecutive stores:

     str  w1, [xb, 0x100]
     str  w1, [xb, 0x104]
     str  w1, [xb, 0x108]
     str  w1, [xb, 0x10c]

   Though the offsets are out of the range supported by stp, we can
   still pair them after adjusting the offset, like:

     add  scratch, xb, 0x100
     stp  w1, w1, [scratch]
     stp  w1, w1, [scratch, 0x8]

   The peephole patterns detecting this opportunity should guarantee
   the scratch register is avaliable.  */

bool
aarch64_operands_adjust_ok_for_ldpstp (rtx *operands, bool load,
				       machine_mode mode)
{
  const int num_insns = 4;
  enum reg_class rclass;
  HOST_WIDE_INT offvals[num_insns], msize;
  rtx mem[num_insns], reg[num_insns], base[num_insns], offset[num_insns];

  if (load)
    {
      for (int i = 0; i < num_insns; i++)
	{
	  reg[i] = operands[2 * i];
	  mem[i] = operands[2 * i + 1];

	  gcc_assert (REG_P (reg[i]));
	}

      /* Do not attempt to merge the loads if the loads clobber each other.  */
      for (int i = 0; i < 8; i += 2)
	for (int j = i + 2; j < 8; j += 2)
	  if (reg_overlap_mentioned_p (operands[i], operands[j]))
	    return false;
    }
  else
    for (int i = 0; i < num_insns; i++)
      {
	mem[i] = operands[2 * i];
	reg[i] = operands[2 * i + 1];
      }

  /* Skip if memory operand is by itself valid for ldp/stp.  */
  if (!MEM_P (mem[0]) || aarch64_mem_pair_operand (mem[0], mode))
    return false;

  for (int i = 0; i < num_insns; i++)
    {
      /* The mems cannot be volatile.  */
      if (MEM_VOLATILE_P (mem[i]))
	return false;

      /* Check if the addresses are in the form of [base+offset].  */
      extract_base_offset_in_addr (mem[i], base + i, offset + i);
      if (base[i] == NULL_RTX || offset[i] == NULL_RTX)
	return false;
    }

  /* Check if the registers are of same class.  */
  rclass = REG_P (reg[0]) && FP_REGNUM_P (REGNO (reg[0]))
    ? FP_REGS : GENERAL_REGS;

  for (int i = 1; i < num_insns; i++)
    if (REG_P (reg[i]) && FP_REGNUM_P (REGNO (reg[i])))
      {
	if (rclass != FP_REGS)
	  return false;
      }
    else
      {
	if (rclass != GENERAL_REGS)
	  return false;
      }

  /* Only the last register in the order in which they occur
     may be clobbered by the load.  */
  if (rclass == GENERAL_REGS && load)
    for (int i = 0; i < num_insns - 1; i++)
      if (reg_mentioned_p (reg[i], mem[i]))
	return false;

  /* Check if the bases are same.  */
  for (int i = 0; i < num_insns - 1; i++)
    if (!rtx_equal_p (base[i], base[i + 1]))
      return false;

  for (int i = 0; i < num_insns; i++)
    offvals[i] = INTVAL (offset[i]);

  msize = GET_MODE_SIZE (mode).to_constant ();

  /* Check if the offsets can be put in the right order to do a ldp/stp.  */
  qsort (offvals, num_insns, sizeof (HOST_WIDE_INT),
	 aarch64_host_wide_int_compare);

  if (!(offvals[1] == offvals[0] + msize
	&& offvals[3] == offvals[2] + msize))
    return false;

  /* Check that offsets are within range of each other.  The ldp/stp
     instructions have 7 bit immediate offsets, so use 0x80.  */
  if (offvals[2] - offvals[0] >= msize * 0x80)
    return false;

  /* The offsets must be aligned with respect to each other.  */
  if (offvals[0] % msize != offvals[2] % msize)
    return false;

   /* Check if mem[0] is ok with the ldp-stp policy model.  */
  if (!aarch64_mem_ok_with_ldpstp_policy_model (mem[0], load, mode))
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store, this function pairs them
   into LDP/STP after adjusting the offset.  It depends on the fact
   that the operands can be sorted so the offsets are correct for STP.
   MODE is the mode of memory operands.  CODE is the rtl operator
   which should be applied to all memory operands, it's SIGN_EXTEND,
   ZERO_EXTEND or UNKNOWN.  */

bool
aarch64_gen_adjusted_ldpstp (rtx *operands, bool load,
			     machine_mode mode, RTX_CODE code)
{
  rtx base, offset_1, offset_2;
  rtx mem_1, mem_2;
  rtx temp_operands[8];
  HOST_WIDE_INT off_val_1, off_val_2, base_off, new_off_1, new_off_2,
		stp_off_upper_limit, stp_off_lower_limit, msize;

  /* We make changes on a copy as we may still bail out.  */
  for (int i = 0; i < 8; i ++)
    temp_operands[i] = operands[i];

  /* Sort the operands.  Note for cases as below:
       [base + 0x310] = A
       [base + 0x320] = B
       [base + 0x330] = C
       [base + 0x320] = D
     We need stable sorting otherwise wrong data may be store to offset 0x320.
     Also note the dead store in above case should be optimized away, but no
     guarantees here.  */
  gcc_stablesort(temp_operands, 4, 2 * sizeof (rtx *),
		 aarch64_ldrstr_offset_compare);

  /* Copy the memory operands so that if we have to bail for some
     reason the original addresses are unchanged.  */
  if (load)
    {
      mem_1 = copy_rtx (temp_operands[1]);
      mem_2 = copy_rtx (temp_operands[5]);
    }
  else
    {
      mem_1 = copy_rtx (temp_operands[0]);
      mem_2 = copy_rtx (temp_operands[4]);
      gcc_assert (code == UNKNOWN);
    }

  extract_base_offset_in_addr (mem_1, &base, &offset_1);
  extract_base_offset_in_addr (mem_2, &base, &offset_2);
  gcc_assert (base != NULL_RTX && offset_1 != NULL_RTX
	      && offset_2 != NULL_RTX);

  /* Adjust offset so it can fit in LDP/STP instruction.  */
  msize = GET_MODE_SIZE (mode).to_constant();
  stp_off_upper_limit = msize * (0x40 - 1);
  stp_off_lower_limit = - msize * 0x40;

  off_val_1 = INTVAL (offset_1);
  off_val_2 = INTVAL (offset_2);

  /* The base offset is optimally half way between the two STP/LDP offsets.  */
  if (msize <= 4)
    base_off = (off_val_1 + off_val_2) / 2;
  else
    /* However, due to issues with negative LDP/STP offset generation for
       larger modes, for DF, DD, DI and vector modes. we must not use negative
       addresses smaller than 9 signed unadjusted bits can store.  This
       provides the most range in this case.  */
    base_off = off_val_1;

  /* Adjust the base so that it is aligned with the addresses but still
     optimal.  */
  if (base_off % msize != off_val_1 % msize)
    /* Fix the offset, bearing in mind we want to make it bigger not
       smaller.  */
    base_off += (((base_off % msize) - (off_val_1 % msize)) + msize) % msize;
  else if (msize <= 4)
    /* The negative range of LDP/STP is one larger than the positive range.  */
    base_off += msize;

  /* Check if base offset is too big or too small.  We can attempt to resolve
     this issue by setting it to the maximum value and seeing if the offsets
     still fit.  */
  if (base_off >= 0x1000)
    {
      base_off = 0x1000 - 1;
      /* We must still make sure that the base offset is aligned with respect
	 to the address.  But it may not be made any bigger.  */
      base_off -= (((base_off % msize) - (off_val_1 % msize)) + msize) % msize;
    }

  /* Likewise for the case where the base is too small.  */
  if (base_off <= -0x1000)
    {
      base_off = -0x1000 + 1;
      base_off += (((base_off % msize) - (off_val_1 % msize)) + msize) % msize;
    }

  /* Offset of the first STP/LDP.  */
  new_off_1 = off_val_1 - base_off;

  /* Offset of the second STP/LDP.  */
  new_off_2 = off_val_2 - base_off;

  /* The offsets must be within the range of the LDP/STP instructions.  */
  if (new_off_1 > stp_off_upper_limit || new_off_1 < stp_off_lower_limit
      || new_off_2 > stp_off_upper_limit || new_off_2 < stp_off_lower_limit)
    return false;

  replace_equiv_address_nv (mem_1, plus_constant (Pmode, operands[8],
						  new_off_1), true);
  replace_equiv_address_nv (mem_2, plus_constant (Pmode, operands[8],
						  new_off_2), true);

  if (!aarch64_mem_pair_operand (mem_1, mode)
      || !aarch64_mem_pair_operand (mem_2, mode))
    return false;

  if (load)
    {
      operands[0] = temp_operands[0];
      operands[1] = mem_1;
      operands[2] = temp_operands[2];
      operands[4] = temp_operands[4];
      operands[5] = mem_2;
      operands[6] = temp_operands[6];
    }
  else
    {
      operands[0] = mem_1;
      operands[1] = temp_operands[1];
      operands[3] = temp_operands[3];
      operands[4] = mem_2;
      operands[5] = temp_operands[5];
      operands[7] = temp_operands[7];
    }

  /* Emit adjusting instruction.  */
  emit_insn (gen_rtx_SET (operands[8], plus_constant (DImode, base, base_off)));
  /* Emit ldp/stp instructions.  */
  if (load)
    {
      emit_insn (aarch64_gen_load_pair (operands[0], operands[2],
					operands[1], code));
      emit_insn (aarch64_gen_load_pair (operands[4], operands[6],
					operands[5], code));
    }
  else
    {
      emit_insn (aarch64_gen_store_pair (operands[0], operands[1],
					 operands[3]));
      emit_insn (aarch64_gen_store_pair (operands[4], operands[5],
					 operands[7]));
    }
  return true;
}

/* Implement TARGET_VECTORIZE_CONDITIONAL_OPERATION_IS_EXPENSIVE.  Assume that
   predicated operations when available are beneficial.  */

static bool
aarch64_conditional_operation_is_expensive (unsigned)
{
  return false;
}

/* Implement TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE.  Assume for now that
   it isn't worth branching around empty masked ops (including masked
   stores).  */

static bool
aarch64_empty_mask_is_expensive (unsigned)
{
  return false;
}

/* Return 1 if pseudo register should be created and used to hold
   GOT address for PIC code.  */

bool
aarch64_use_pseudo_pic_reg (void)
{
  return aarch64_cmodel == AARCH64_CMODEL_SMALL_SPIC;
}

/* Implement TARGET_UNSPEC_MAY_TRAP_P.  */

static int
aarch64_unspec_may_trap_p (const_rtx x, unsigned flags)
{
  switch (XINT (x, 1))
    {
    case UNSPEC_GOTSMALLPIC:
    case UNSPEC_GOTSMALLPIC28K:
    case UNSPEC_GOTTINYPIC:
      return 0;
    default:
      break;
    }

  return default_unspec_may_trap_p (x, flags);
}


/* If X is a positive CONST_DOUBLE with a value that is a power of 2
   return the log2 of that value.  Otherwise return -1.  */

int
aarch64_fpconst_pow_of_2 (rtx x)
{
  const REAL_VALUE_TYPE *r;

  if (!CONST_DOUBLE_P (x))
    return -1;

  r = CONST_DOUBLE_REAL_VALUE (x);

  if (REAL_VALUE_NEGATIVE (*r)
      || REAL_VALUE_ISNAN (*r)
      || REAL_VALUE_ISINF (*r)
      || !real_isinteger (r, DFmode))
    return -1;

  return exact_log2 (real_to_integer (r));
}

/* If X is a positive CONST_DOUBLE with a value that is the reciprocal of a
   power of 2 (i.e 1/2^n) return the number of float bits. e.g. for x==(1/2^n)
   return n. Otherwise return -1.  */

int
aarch64_fpconst_pow2_recip (rtx x)
{
  REAL_VALUE_TYPE r0;

  if (!CONST_DOUBLE_P (x))
    return -1;

  r0 = *CONST_DOUBLE_REAL_VALUE (x);
  if (exact_real_inverse (DFmode, &r0)
      && !REAL_VALUE_NEGATIVE (r0))
    {
	int ret = exact_log2 (real_to_integer (&r0));
	if (ret >= 1 && ret <= 32)
	    return ret;
    }
  return -1;
}

/* If X is a vector of equal CONST_DOUBLE values and that value is
   Y, return the aarch64_fpconst_pow_of_2 of Y.  Otherwise return -1.  */

int
aarch64_vec_fpconst_pow_of_2 (rtx x)
{
  int nelts;
  if (!CONST_VECTOR_P (x)
      || !CONST_VECTOR_NUNITS (x).is_constant (&nelts))
    return -1;

  if (GET_MODE_CLASS (GET_MODE (x)) != MODE_VECTOR_FLOAT)
    return -1;

  int firstval = aarch64_fpconst_pow_of_2 (CONST_VECTOR_ELT (x, 0));
  if (firstval <= 0)
    return -1;

  for (int i = 1; i < nelts; i++)
    if (aarch64_fpconst_pow_of_2 (CONST_VECTOR_ELT (x, i)) != firstval)
      return -1;

  return firstval;
}

/* Implement TARGET_PROMOTED_TYPE to promote 16-bit floating point types
   to float.

   __fp16 always promotes through this hook.
   _Float16 may promote if TARGET_FLT_EVAL_METHOD is 16, but we do that
   through the generic excess precision logic rather than here.  */

static tree
aarch64_promoted_type (const_tree t)
{
  if (SCALAR_FLOAT_TYPE_P (t)
      && TYPE_MAIN_VARIANT (t) == aarch64_fp16_type_node)
    return float_type_node;

  return NULL_TREE;
}

/* Implement the TARGET_OPTAB_SUPPORTED_P hook.  */

static bool
aarch64_optab_supported_p (int op, machine_mode mode1, machine_mode,
			   optimization_type opt_type)
{
  switch (op)
    {
    case rsqrt_optab:
      return opt_type == OPTIMIZE_FOR_SPEED && use_rsqrt_p (mode1);

    default:
      return true;
    }
}

/* Implement the TARGET_DWARF_POLY_INDETERMINATE_VALUE hook.  */

static unsigned int
aarch64_dwarf_poly_indeterminate_value (unsigned int i, unsigned int *factor,
					int *offset)
{
  /* Polynomial invariant 1 == (VG / 2) - 1.  */
  gcc_assert (i == 1);
  *factor = 2;
  *offset = 1;
  return AARCH64_DWARF_VG;
}

/* Implement TARGET_LIBGCC_FLOATING_POINT_MODE_SUPPORTED_P - return TRUE
   if MODE is [BH]Fmode, and punt to the generic implementation otherwise.  */

static bool
aarch64_libgcc_floating_mode_supported_p (scalar_float_mode mode)
{
  return ((mode == HFmode || mode == BFmode)
	  ? true
	  : default_libgcc_floating_mode_supported_p (mode));
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P - return TRUE
   if MODE is [BH]Fmode, and punt to the generic implementation otherwise.  */

static bool
aarch64_scalar_mode_supported_p (scalar_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode))
    return default_decimal_float_supported_p ();

  return ((mode == HFmode || mode == BFmode)
	  ? true
	  : default_scalar_mode_supported_p (mode));
}

/* Set the value of FLT_EVAL_METHOD.
   ISO/IEC TS 18661-3 defines two values that we'd like to make use of:

    0: evaluate all operations and constants, whose semantic type has at
       most the range and precision of type float, to the range and
       precision of float; evaluate all other operations and constants to
       the range and precision of the semantic type;

    N, where _FloatN is a supported interchange floating type
       evaluate all operations and constants, whose semantic type has at
       most the range and precision of _FloatN type, to the range and
       precision of the _FloatN type; evaluate all other operations and
       constants to the range and precision of the semantic type;

   If we have the ARMv8.2-A extensions then we support _Float16 in native
   precision, so we should set this to 16.  Otherwise, we support the type,
   but want to evaluate expressions in float precision, so set this to
   0.  */

static enum flt_eval_method
aarch64_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
      case EXCESS_PRECISION_TYPE_FAST:
      case EXCESS_PRECISION_TYPE_STANDARD:
	/* We can calculate either in 16-bit range and precision or
	   32-bit range and precision.  Make that decision based on whether
	   we have native support for the ARMv8.2-A 16-bit floating-point
	   instructions or not.  */
	return (TARGET_FP_F16INST
		? FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16
		: FLT_EVAL_METHOD_PROMOTE_TO_FLOAT);
      case EXCESS_PRECISION_TYPE_IMPLICIT:
      case EXCESS_PRECISION_TYPE_FLOAT16:
	return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16;
      default:
	gcc_unreachable ();
    }
  return FLT_EVAL_METHOD_UNPREDICTABLE;
}

/* Implement TARGET_C_BITINT_TYPE_INFO.
   Return true if _BitInt(N) is supported and fill its details into *INFO.  */
bool
aarch64_bitint_type_info (int n, struct bitint_info *info)
{
  if (TARGET_BIG_END)
    return false;

  if (n <= 8)
    info->limb_mode = QImode;
  else if (n <= 16)
    info->limb_mode = HImode;
  else if (n <= 32)
    info->limb_mode = SImode;
  else if (n <= 64)
    info->limb_mode = DImode;
  else if (n <= 128)
    info->limb_mode = TImode;
  else
    /* The AAPCS for AArch64 defines _BitInt(N > 128) as an array with
       type {signed,unsigned} __int128[M] where M*128 >= N.  However, to be
       able to use libgcc's implementation to support large _BitInt's we need
       to use a LIMB_MODE that is no larger than 'long long'.  This is why we
       use DImode for our internal LIMB_MODE and we define the ABI_LIMB_MODE to
       be TImode to ensure we are ABI compliant.  */
    info->limb_mode = DImode;

  if (n > 128)
    info->abi_limb_mode = TImode;
  else
    info->abi_limb_mode = info->limb_mode;
  info->big_endian = TARGET_BIG_END;
  info->extended = false;
  return true;
}

/* Implement TARGET_C_MODE_FOR_FLOATING_TYPE.  Return TFmode for
   TI_LONG_DOUBLE_TYPE which is for long double type, go with the default
   one for the others.  */

static machine_mode
aarch64_c_mode_for_floating_type (enum tree_index ti)
{
  if (ti == TI_LONG_DOUBLE_TYPE)
    return TFmode;
  return default_mode_for_floating_type (ti);
}

/* Implement TARGET_SCHED_CAN_SPECULATE_INSN.  Return true if INSN can be
   scheduled for speculative execution.  Reject the long-running division
   and square-root instructions.  */

static bool
aarch64_sched_can_speculate_insn (rtx_insn *insn)
{
  switch (get_attr_type (insn))
    {
      case TYPE_SDIV:
      case TYPE_UDIV:
      case TYPE_FDIVS:
      case TYPE_FDIVD:
      case TYPE_FSQRTS:
      case TYPE_FSQRTD:
      case TYPE_NEON_FP_SQRT_S:
      case TYPE_NEON_FP_SQRT_D:
      case TYPE_NEON_FP_SQRT_S_Q:
      case TYPE_NEON_FP_SQRT_D_Q:
      case TYPE_NEON_FP_DIV_S:
      case TYPE_NEON_FP_DIV_D:
      case TYPE_NEON_FP_DIV_S_Q:
      case TYPE_NEON_FP_DIV_D_Q:
	return false;
      default:
	return true;
    }
}

/* Implement TARGET_COMPUTE_PRESSURE_CLASSES.  */

static int
aarch64_compute_pressure_classes (reg_class *classes)
{
  int i = 0;
  classes[i++] = GENERAL_REGS;
  classes[i++] = FP_REGS;
  /* PR_REGS isn't a useful pressure class because many predicate pseudo
     registers need to go in PR_LO_REGS at some point during their
     lifetime.  Splitting it into two halves has the effect of making
     all predicates count against PR_LO_REGS, so that we try whenever
     possible to restrict the number of live predicates to 8.  This
     greatly reduces the amount of spilling in certain loops.  */
  classes[i++] = PR_LO_REGS;
  classes[i++] = PR_HI_REGS;
  return i;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
aarch64_can_change_mode_class (machine_mode from,
			       machine_mode to, reg_class_t)
{
  return aarch64_modes_compatible_p (from, to);
}

/* Implement TARGET_EARLY_REMAT_MODES.  */

static void
aarch64_select_early_remat_modes (sbitmap modes)
{
  /* SVE values are not normally live across a call, so it should be
     worth doing early rematerialization even in VL-specific mode.  */
  for (int i = 0; i < NUM_MACHINE_MODES; ++i)
    if (aarch64_sve_mode_p ((machine_mode) i))
      bitmap_set_bit (modes, i);
}

/* Override the default target speculation_safe_value.  */
static rtx
aarch64_speculation_safe_value (machine_mode mode,
				rtx result, rtx val, rtx failval)
{
  /* Maybe we should warn if falling back to hard barriers.  They are
     likely to be noticably more expensive than the alternative below.  */
  if (!aarch64_track_speculation)
    return default_speculation_safe_value (mode, result, val, failval);

  if (!REG_P (val))
    val = copy_to_mode_reg (mode, val);

  if (!aarch64_reg_or_zero (failval, mode))
    failval = copy_to_mode_reg (mode, failval);

  emit_insn (gen_despeculate_copy (mode, result, val, failval));
  return result;
}

/* Implement TARGET_ESTIMATED_POLY_VALUE.
   Look into the tuning structure for an estimate.
   KIND specifies the type of requested estimate: min, max or likely.
   For cores with a known SVE width all three estimates are the same.
   For generic SVE tuning we want to distinguish the maximum estimate from
   the minimum and likely ones.
   The likely estimate is the same as the minimum in that case to give a
   conservative behavior of auto-vectorizing with SVE when it is a win
   even for 128-bit SVE.
   When SVE width information is available VAL.coeffs[1] is multiplied by
   the number of VQ chunks over the initial Advanced SIMD 128 bits.  */

static HOST_WIDE_INT
aarch64_estimated_poly_value (poly_int64 val,
			      poly_value_estimate_kind kind
				= POLY_VALUE_LIKELY)
{
  unsigned int width_source = aarch64_tune_params.sve_width;

  /* If there is no core-specific information then the minimum and likely
     values are based on 128-bit vectors and the maximum is based on
     the architectural maximum of 2048 bits.  */
  if (width_source == SVE_SCALABLE)
    switch (kind)
      {
      case POLY_VALUE_MIN:
      case POLY_VALUE_LIKELY:
	return val.coeffs[0];
      case POLY_VALUE_MAX:
	  return val.coeffs[0] + val.coeffs[1] * 15;
      }

  /* Allow sve_width to be a bitmask of different VL, treating the lowest
     as likely.  This could be made more general if future -mtune options
     need it to be.  */
  if (kind == POLY_VALUE_MAX)
    width_source = 1 << floor_log2 (width_source);
  else
    width_source = least_bit_hwi (width_source);

  /* If the core provides width information, use that.  */
  HOST_WIDE_INT over_128 = width_source - 128;
  return val.coeffs[0] + val.coeffs[1] * over_128 / 128;
}


/* Return true for types that could be supported as SIMD return or
   argument types.  */

static bool
supported_simd_type (tree t)
{
  if (SCALAR_FLOAT_TYPE_P (t) || INTEGRAL_TYPE_P (t) || POINTER_TYPE_P (t))
    {
      HOST_WIDE_INT s = tree_to_shwi (TYPE_SIZE_UNIT (t));
      return s == 1 || s == 2 || s == 4 || s == 8;
    }
  return false;
}

/* Determine the lane size for the clone argument/return type.  This follows
   the LS(P) rule in the VFABIA64.  */

static unsigned
lane_size (cgraph_simd_clone_arg_type clone_arg_type, tree type)
{
  gcc_assert (clone_arg_type != SIMD_CLONE_ARG_TYPE_MASK);

  /* For non map-to-vector types that are pointers we use the element type it
     points to.  */
  if (POINTER_TYPE_P (type))
    switch (clone_arg_type)
      {
      default:
	break;
      case SIMD_CLONE_ARG_TYPE_UNIFORM:
      case SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP:
      case SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP:
	type = TREE_TYPE (type);
	break;
      }

  /* For types (or pointers of non map-to-vector types point to) that are
     integers or floating point, we use their size if they are 1, 2, 4 or 8.
   */
  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type))
    switch (TYPE_PRECISION (type) / BITS_PER_UNIT)
      {
      default:
	break;
      case 1:
      case 2:
      case 4:
      case 8:
	return TYPE_PRECISION (type);
      }
  /* For any other we use the size of uintptr_t.  For map-to-vector types that
     are pointers, using the size of uintptr_t is the same as using the size of
     their type, seeing all pointers are the same size as uintptr_t.  */
  return POINTER_SIZE;
}


/* Implement TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN.  */

static int
aarch64_simd_clone_compute_vecsize_and_simdlen (struct cgraph_node *node,
					struct cgraph_simd_clone *clonei,
					tree base_type ATTRIBUTE_UNUSED,
					int num, bool explicit_p)
{
  tree t, ret_type;
  unsigned int nds_elt_bits;
  unsigned HOST_WIDE_INT const_simdlen;

  if (!TARGET_SIMD)
    return 0;

  /* For now, SVE simdclones won't produce illegal simdlen, So only check
     const simdlens here.  */
  if (maybe_ne (clonei->simdlen, 0U)
      && clonei->simdlen.is_constant (&const_simdlen)
      && (const_simdlen < 2
	  || const_simdlen > 1024
	  || (const_simdlen & (const_simdlen - 1)) != 0))
    {
      if (explicit_p)
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "unsupported simdlen %wd", const_simdlen);
      return 0;
    }

  ret_type = TREE_TYPE (TREE_TYPE (node->decl));
  /* According to AArch64's Vector ABI the type that determines the simdlen is
     the narrowest of types, so we ignore base_type for AArch64.  */
  if (TREE_CODE (ret_type) != VOID_TYPE
      && !supported_simd_type (ret_type))
    {
      if (!explicit_p)
	;
      else if (COMPLEX_FLOAT_TYPE_P (ret_type))
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "GCC does not currently support return type %qT "
		    "for simd", ret_type);
      else
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "unsupported return type %qT for simd",
		    ret_type);
      return 0;
    }

  auto_vec<std::pair <tree, unsigned int>> vec_elts (clonei->nargs + 1);

  /* We are looking for the NDS type here according to the VFABIA64.  */
  if (TREE_CODE (ret_type) != VOID_TYPE)
    {
      nds_elt_bits = lane_size (SIMD_CLONE_ARG_TYPE_VECTOR, ret_type);
      vec_elts.safe_push (std::make_pair (ret_type, nds_elt_bits));
    }
  else
    nds_elt_bits = POINTER_SIZE;

  int i;
  tree type_arg_types = TYPE_ARG_TYPES (TREE_TYPE (node->decl));
  bool decl_arg_p = (node->definition || type_arg_types == NULL_TREE);
  for (t = (decl_arg_p ? DECL_ARGUMENTS (node->decl) : type_arg_types), i = 0;
       t && t != void_list_node; t = TREE_CHAIN (t), i++)
    {
      tree arg_type = decl_arg_p ? TREE_TYPE (t) : TREE_VALUE (t);
      if (clonei->args[i].arg_type != SIMD_CLONE_ARG_TYPE_UNIFORM
	  && !supported_simd_type (arg_type))
	{
	  if (!explicit_p)
	    ;
	  else if (COMPLEX_FLOAT_TYPE_P (ret_type))
	    warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			"GCC does not currently support argument type %qT "
			"for simd", arg_type);
	  else
	    warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			"unsupported argument type %qT for simd",
			arg_type);
	  return 0;
	}
      unsigned lane_bits = lane_size (clonei->args[i].arg_type, arg_type);
      if (clonei->args[i].arg_type == SIMD_CLONE_ARG_TYPE_VECTOR)
	vec_elts.safe_push (std::make_pair (arg_type, lane_bits));
      if (nds_elt_bits > lane_bits)
	nds_elt_bits = lane_bits;
    }

  clonei->vecsize_mangle = 'n';
  clonei->mask_mode = VOIDmode;
  poly_uint64 simdlen;
  auto_vec<poly_uint64> simdlens (2);
  /* Keep track of the possible simdlens the clones of this function can have,
     and check them later to see if we support them.  */
  if (known_eq (clonei->simdlen, 0U))
    {
      simdlen = exact_div (poly_uint64 (64), nds_elt_bits);
      if (maybe_ne (simdlen, 1U))
	simdlens.safe_push (simdlen);
      simdlens.safe_push (simdlen * 2);
    }
  else
    simdlens.safe_push (clonei->simdlen);

  clonei->vecsize_int = 0;
  clonei->vecsize_float = 0;

  /* We currently do not support generating simdclones where vector arguments
     do not fit into a single vector register, i.e. vector types that are more
     than 128-bits large.  This is because of how we currently represent such
     types in ACLE, where we use a struct to allow us to pass them as arguments
     and return.
     Hence why we have to check whether the simdlens available for this
     simdclone would cause a vector type to be larger than 128-bits, and reject
     such a clone.  */
  unsigned j = 0;
  while (j < simdlens.length ())
    {
      bool remove_simdlen = false;
      for (auto elt : vec_elts)
	if (known_gt (simdlens[j] * elt.second, 128U))
	  {
	    /* Don't issue a warning for every simdclone when there is no
	       specific simdlen clause.  */
	    if (explicit_p && maybe_ne (clonei->simdlen, 0U))
	      warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			  "GCC does not currently support simdlen %wd for "
			  "type %qT",
			  constant_lower_bound (simdlens[j]), elt.first);
	    remove_simdlen = true;
	    break;
	  }
      if (remove_simdlen)
	simdlens.ordered_remove (j);
      else
	j++;
    }


  int count = simdlens.length ();
  if (count == 0)
    {
      if (explicit_p && known_eq (clonei->simdlen, 0U))
	{
	  /* Warn the user if we can't generate any simdclone.  */
	  simdlen = exact_div (poly_uint64 (64), nds_elt_bits);
	  warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		      "GCC does not currently support a simdclone with simdlens"
		      " %wd and %wd for these types.",
		      constant_lower_bound (simdlen),
		      constant_lower_bound (simdlen*2));
	}
      return 0;
    }

  gcc_assert (num < count);
  clonei->simdlen = simdlens[num];
  return count;
}

/* Implement TARGET_SIMD_CLONE_ADJUST.  */

static void
aarch64_simd_clone_adjust (struct cgraph_node *node)
{
  /* Add aarch64_vector_pcs target attribute to SIMD clones so they
     use the correct ABI.  */

  tree t = TREE_TYPE (node->decl);
  TYPE_ATTRIBUTES (t) = make_attribute ("aarch64_vector_pcs", "default",
					TYPE_ATTRIBUTES (t));
}

/* Implement TARGET_SIMD_CLONE_USABLE.  */

static int
aarch64_simd_clone_usable (struct cgraph_node *node)
{
  switch (node->simdclone->vecsize_mangle)
    {
    case 'n':
      if (!TARGET_SIMD)
	return -1;
      return 0;
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_COMP_TYPE_ATTRIBUTES */

static int
aarch64_comp_type_attributes (const_tree type1, const_tree type2)
{
  auto check_attr = [&](const char *ns, const char *name) {
    tree attr1 = lookup_attribute (ns, name, TYPE_ATTRIBUTES (type1));
    tree attr2 = lookup_attribute (ns, name, TYPE_ATTRIBUTES (type2));
    if (!attr1 && !attr2)
      return true;

    return attr1 && attr2 && attribute_value_equal (attr1, attr2);
  };

  if (!check_attr ("gnu", "aarch64_vector_pcs"))
    return 0;
  if (!check_attr ("gnu", "indirect_return"))
    return 0;
  if (!check_attr ("gnu", "Advanced SIMD type"))
    return 0;
  if (!check_attr ("gnu", "SVE type"))
    return 0;
  if (!check_attr ("gnu", "SVE sizeless type"))
    return 0;
  if (!check_attr ("arm", "streaming"))
    return 0;
  if (!check_attr ("arm", "streaming_compatible"))
    return 0;
  if (aarch64_lookup_shared_state_flags (TYPE_ATTRIBUTES (type1), "za")
      != aarch64_lookup_shared_state_flags (TYPE_ATTRIBUTES (type2), "za"))
    return 0;
  if (aarch64_lookup_shared_state_flags (TYPE_ATTRIBUTES (type1), "zt0")
      != aarch64_lookup_shared_state_flags (TYPE_ATTRIBUTES (type2), "zt0"))
    return 0;
  return 1;
}

/* Implement TARGET_MERGE_DECL_ATTRIBUTES.  */

static tree
aarch64_merge_decl_attributes (tree olddecl, tree newdecl)
{
  tree old_attrs = DECL_ATTRIBUTES (olddecl);
  tree old_new = lookup_attribute ("arm", "new", old_attrs);

  tree new_attrs = DECL_ATTRIBUTES (newdecl);
  tree new_new = lookup_attribute ("arm", "new", new_attrs);

  if (DECL_INITIAL (olddecl) && new_new)
    {
      error ("cannot apply attribute %qs to %q+D after the function"
	     " has been defined", "new", newdecl);
      inform (DECL_SOURCE_LOCATION (olddecl), "%q+D defined here",
	      newdecl);
    }
  else
    {
      if (old_new && new_new)
	{
	  old_attrs = remove_attribute ("arm", "new", old_attrs);
	  TREE_VALUE (new_new) = chainon (TREE_VALUE (new_new),
					  TREE_VALUE (old_new));
	}
      if (new_new)
	aarch64_check_arm_new_against_type (TREE_VALUE (new_new), newdecl);
    }

  return merge_attributes (old_attrs, new_attrs);
}

/* Implement TARGET_GET_MULTILIB_ABI_NAME */

static const char *
aarch64_get_multilib_abi_name (void)
{
  if (TARGET_BIG_END)
    return TARGET_ILP32 ? "aarch64_be_ilp32" : "aarch64_be";
  return TARGET_ILP32 ? "aarch64_ilp32" : "aarch64";
}

/* Implement TARGET_STACK_PROTECT_GUARD. In case of a
   global variable based guard use the default else
   return a null tree.  */
static tree
aarch64_stack_protect_guard (void)
{
  if (aarch64_stack_protector_guard == SSP_GLOBAL)
    return default_stack_protect_guard ();

  return NULL_TREE;
}

/* Implement TARGET_INVALID_UNARY_OP.  */

static const char *
aarch64_invalid_unary_op (int op, const_tree type)
{
  /* Reject all single-operand operations on __mfp8 except for &.  */
  if (TYPE_MAIN_VARIANT (type) == aarch64_mfp8_type_node && op != ADDR_EXPR)
    return N_ ("operation not permitted on type %<mfloat8_t%>");

  /* Operation allowed.  */
  return NULL;
}

/* Implement TARGET_INVALID_BINARY_OP.  */

static const char *
aarch64_invalid_binary_op (int op ATTRIBUTE_UNUSED, const_tree type1,
			   const_tree type2)
{
  if (VECTOR_TYPE_P (type1)
      && VECTOR_TYPE_P (type2)
      && !TYPE_INDIVISIBLE_P (type1)
      && !TYPE_INDIVISIBLE_P (type2)
      && (aarch64_sve::builtin_type_p (type1)
	  != aarch64_sve::builtin_type_p (type2)))
    return N_("cannot combine GNU and SVE vectors in a binary operation");

  /* Reject all 2-operand operations on __mfp8.  */
  if (TYPE_MAIN_VARIANT (type1) == aarch64_mfp8_type_node
      || TYPE_MAIN_VARIANT (type2) == aarch64_mfp8_type_node)
    return N_ ("operation not permitted on type %<mfloat8_t%>");

  /* Operation allowed.  */
  return NULL;
}

/* Implement TARGET_MEMTAG_CAN_TAG_ADDRESSES.  Here we tell the rest of the
   compiler that we automatically ignore the top byte of our pointers, which
   allows using -fsanitize=hwaddress.  */
bool
aarch64_can_tag_addresses ()
{
  return !TARGET_ILP32;
}

/* Implement TARGET_ASM_FILE_END for AArch64.  This adds the AArch64 GNU NOTE
   section at the end if needed.  */
#define GNU_PROPERTY_AARCH64_FEATURE_1_AND	0xc0000000
#define GNU_PROPERTY_AARCH64_FEATURE_1_BTI	(1U << 0)
#define GNU_PROPERTY_AARCH64_FEATURE_1_PAC	(1U << 1)
#define GNU_PROPERTY_AARCH64_FEATURE_1_GCS	(1U << 2)
void
aarch64_file_end_indicate_exec_stack ()
{
  file_end_indicate_exec_stack ();

  unsigned feature_1_and = 0;
  if (aarch_bti_enabled ())
    feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_BTI;

  if (aarch_ra_sign_scope != AARCH_FUNCTION_NONE)
    feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_PAC;

  if (aarch64_gcs_enabled ())
    feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_GCS;

  if (feature_1_and)
    {
      /* Generate .note.gnu.property section.  */
      switch_to_section (get_section (".note.gnu.property",
				      SECTION_NOTYPE, NULL));

      /* PT_NOTE header: namesz, descsz, type.
	 namesz = 4 ("GNU\0")
	 descsz = 16 (Size of the program property array)
		  [(12 + padding) * Number of array elements]
	 type   = 5 (NT_GNU_PROPERTY_TYPE_0).  */
      assemble_align (POINTER_SIZE);
      assemble_integer (GEN_INT (4), 4, 32, 1);
      assemble_integer (GEN_INT (ROUND_UP (12, POINTER_BYTES)), 4, 32, 1);
      assemble_integer (GEN_INT (5), 4, 32, 1);

      /* PT_NOTE name.  */
      assemble_string ("GNU", 4);

      /* PT_NOTE contents for NT_GNU_PROPERTY_TYPE_0:
	 type   = GNU_PROPERTY_AARCH64_FEATURE_1_AND
	 datasz = 4
	 data   = feature_1_and.  */
      assemble_integer (GEN_INT (GNU_PROPERTY_AARCH64_FEATURE_1_AND), 4, 32, 1);
      assemble_integer (GEN_INT (4), 4, 32, 1);
      assemble_integer (GEN_INT (feature_1_and), 4, 32, 1);

      /* Pad the size of the note to the required alignment.  */
      assemble_align (POINTER_SIZE);
    }
}
#undef GNU_PROPERTY_AARCH64_FEATURE_1_GCS
#undef GNU_PROPERTY_AARCH64_FEATURE_1_PAC
#undef GNU_PROPERTY_AARCH64_FEATURE_1_BTI
#undef GNU_PROPERTY_AARCH64_FEATURE_1_AND

/* Helper function for straight line speculation.
   Return what barrier should be emitted for straight line speculation
   mitigation.
   When not mitigating against straight line speculation this function returns
   an empty string.
   When mitigating against straight line speculation, use:
   * SB when the v8.5-A SB extension is enabled.
   * DSB+ISB otherwise.  */
const char *
aarch64_sls_barrier (int mitigation_required)
{
  return mitigation_required
    ? (TARGET_SB ? "sb" : "dsb\tsy\n\tisb")
    : "";
}

static GTY (()) tree aarch64_sls_shared_thunks[30];
static GTY (()) bool aarch64_sls_shared_thunks_needed = false;
const char *indirect_symbol_names[30] = {
    "__call_indirect_x0",
    "__call_indirect_x1",
    "__call_indirect_x2",
    "__call_indirect_x3",
    "__call_indirect_x4",
    "__call_indirect_x5",
    "__call_indirect_x6",
    "__call_indirect_x7",
    "__call_indirect_x8",
    "__call_indirect_x9",
    "__call_indirect_x10",
    "__call_indirect_x11",
    "__call_indirect_x12",
    "__call_indirect_x13",
    "__call_indirect_x14",
    "__call_indirect_x15",
    "", /* "__call_indirect_x16",  */
    "", /* "__call_indirect_x17",  */
    "__call_indirect_x18",
    "__call_indirect_x19",
    "__call_indirect_x20",
    "__call_indirect_x21",
    "__call_indirect_x22",
    "__call_indirect_x23",
    "__call_indirect_x24",
    "__call_indirect_x25",
    "__call_indirect_x26",
    "__call_indirect_x27",
    "__call_indirect_x28",
    "__call_indirect_x29",
};

/* Function to create a BLR thunk.  This thunk is used to mitigate straight
   line speculation.  Instead of a simple BLR that can be speculated past,
   we emit a BL to this thunk, and this thunk contains a BR to the relevant
   register.  These thunks have the relevant speculation barries put after
   their indirect branch so that speculation is blocked.

   We use such a thunk so the speculation barriers are kept off the
   architecturally executed path in order to reduce the performance overhead.

   When optimizing for size we use stubs shared by the linked object.
   When optimizing for performance we emit stubs for each function in the hope
   that the branch predictor can better train on jumps specific for a given
   function.  */
rtx
aarch64_sls_create_blr_label (int regnum)
{
  gcc_assert (STUB_REGNUM_P (regnum));
  if (optimize_function_for_size_p (cfun))
    {
      /* For the thunks shared between different functions in this compilation
	 unit we use a named symbol -- this is just for users to more easily
	 understand the generated assembly.  */
      aarch64_sls_shared_thunks_needed = true;
      const char *thunk_name = indirect_symbol_names[regnum];
      if (aarch64_sls_shared_thunks[regnum] == NULL)
	{
	  /* Build a decl representing this function stub and record it for
	     later.  We build a decl here so we can use the GCC machinery for
	     handling sections automatically (through `get_named_section` and
	     `make_decl_one_only`).  That saves us a lot of trouble handling
	     the specifics of different output file formats.  */
	  tree decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
				  get_identifier (thunk_name),
				  build_function_type_list (void_type_node,
							    NULL_TREE));
	  DECL_RESULT (decl) = build_decl (BUILTINS_LOCATION, RESULT_DECL,
					   NULL_TREE, void_type_node);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_IGNORED_P (decl) = 1;
	  DECL_ARTIFICIAL (decl) = 1;
	  make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
	  resolve_unique_section (decl, 0, false);
	  aarch64_sls_shared_thunks[regnum] = decl;
	}

      return gen_rtx_SYMBOL_REF (Pmode, thunk_name);
    }

  if (cfun->machine->call_via[regnum] == NULL)
    cfun->machine->call_via[regnum]
      = gen_rtx_LABEL_REF (Pmode, gen_label_rtx ());
  return cfun->machine->call_via[regnum];
}

/* Helper function for aarch64_sls_emit_blr_function_thunks and
   aarch64_sls_emit_shared_blr_thunks below.  */
static void
aarch64_sls_emit_function_stub (FILE *out_file, int regnum)
{
  /* Save in x16 and branch to that function so this transformation does
     not prevent jumping to `BTI c` instructions.  */
  asm_fprintf (out_file, "\tmov\tx16, x%d\n", regnum);
  asm_fprintf (out_file, "\tbr\tx16\n");
}

/* Emit all BLR stubs for this particular function.
   Here we emit all the BLR stubs needed for the current function.  Since we
   emit these stubs in a consecutive block we know there will be no speculation
   gadgets between each stub, and hence we only emit a speculation barrier at
   the end of the stub sequences.

   This is called in the TARGET_ASM_FUNCTION_EPILOGUE hook.  */
void
aarch64_sls_emit_blr_function_thunks (FILE *out_file)
{
  if (! aarch64_harden_sls_blr_p ())
    return;

  bool any_functions_emitted = false;
  /* We must save and restore the current function section since this assembly
     is emitted at the end of the function.  This means it can be emitted *just
     after* the cold section of a function.  That cold part would be emitted in
     a different section.  That switch would trigger a `.cfi_endproc` directive
     to be emitted in the original section and a `.cfi_startproc` directive to
     be emitted in the new section.  Switching to the original section without
     restoring would mean that the `.cfi_endproc` emitted as a function ends
     would happen in a different section -- leaving an unmatched
     `.cfi_startproc` in the cold text section and an unmatched `.cfi_endproc`
     in the standard text section.  */
  section *save_text_section = in_section;
  switch_to_section (function_section (current_function_decl));
  for (int regnum = 0; regnum < 30; ++regnum)
    {
      rtx specu_label = cfun->machine->call_via[regnum];
      if (specu_label == NULL)
	continue;

      targetm.asm_out.print_operand (out_file, specu_label, 0);
      asm_fprintf (out_file, ":\n");
      aarch64_sls_emit_function_stub (out_file, regnum);
      any_functions_emitted = true;
    }
  if (any_functions_emitted)
    /* Can use the SB if needs be here, since this stub will only be used
      by the current function, and hence for the current target.  */
    asm_fprintf (out_file, "\t%s\n", aarch64_sls_barrier (true));
  switch_to_section (save_text_section);
}

/* Emit shared BLR stubs for the current compilation unit.
   Over the course of compiling this unit we may have converted some BLR
   instructions to a BL to a shared stub function.  This is where we emit those
   stub functions.
   This function is for the stubs shared between different functions in this
   compilation unit.  We share when optimizing for size instead of speed.

   This function is called through the TARGET_ASM_FILE_END hook.  */
void
aarch64_sls_emit_shared_blr_thunks (FILE *out_file)
{
  if (! aarch64_sls_shared_thunks_needed)
    return;

  for (int regnum = 0; regnum < 30; ++regnum)
    {
      tree decl = aarch64_sls_shared_thunks[regnum];
      if (!decl)
	continue;

      const char *name = indirect_symbol_names[regnum];
      switch_to_section (get_named_section (decl, NULL, 0));
      ASM_OUTPUT_ALIGN (out_file, 2);
      targetm.asm_out.globalize_label (out_file, name);
      /* Only emits if the compiler is configured for an assembler that can
	 handle visibility directives.  */
      targetm.asm_out.assemble_visibility (decl, VISIBILITY_HIDDEN);
      ASM_OUTPUT_TYPE_DIRECTIVE (out_file, name, "function");
      ASM_OUTPUT_LABEL (out_file, name);
      aarch64_sls_emit_function_stub (out_file, regnum);
      /* Use the most conservative target to ensure it can always be used by any
	 function in the translation unit.  */
      asm_fprintf (out_file, "\tdsb\tsy\n\tisb\n");
      ASM_DECLARE_FUNCTION_SIZE (out_file, name, decl);
    }
}

/* Implement TARGET_ASM_FILE_END.  */
void
aarch64_asm_file_end ()
{
  aarch64_sls_emit_shared_blr_thunks (asm_out_file);
  /* Since this function will be called for the ASM_FILE_END hook, we ensure
     that what would be called otherwise (e.g. `file_end_indicate_exec_stack`
     for FreeBSD) still gets called.  */
#ifdef TARGET_ASM_FILE_END
  TARGET_ASM_FILE_END ();
#endif
}

const char *
aarch64_indirect_call_asm (rtx addr)
{
  gcc_assert (REG_P (addr));
  if (aarch64_harden_sls_blr_p ())
    {
      rtx stub_label = aarch64_sls_create_blr_label (REGNO (addr));
      output_asm_insn ("bl\t%0", &stub_label);
    }
  else
   output_asm_insn ("blr\t%0", &addr);
  return "";
}

/* Emit the assembly instruction to load the thread pointer into DEST.
   Select between different tpidr_elN registers depending on -mtp= setting.  */

const char *
aarch64_output_load_tp (rtx dest)
{
  const char *tpidrs[] = {"tpidr_el0", "tpidr_el1", "tpidr_el2",
			  "tpidr_el3", "tpidrro_el0"};
  char buffer[64];
  snprintf (buffer, sizeof (buffer), "mrs\t%%0, %s",
	    tpidrs[aarch64_tpidr_register]);
  output_asm_insn (buffer, &dest);
  return "";
}

/* Set up the value of REG_ALLOC_ORDER from scratch.

   It was previously good practice to put call-clobbered registers ahead
   of call-preserved registers, but that isn't necessary these days.
   IRA's model of register save/restore costs is much more sophisticated
   than the model that a simple ordering could provide.  We leave
   HONOR_REG_ALLOC_ORDER undefined so that we can get the full benefit
   of IRA's model.

   However, it is still useful to list registers that are members of
   multiple classes after registers that are members of fewer classes.
   For example, we have:

   - FP_LO8_REGS: v0-v7
   - FP_LO_REGS: v0-v15
   - FP_REGS: v0-v31

   If, as a tie-breaker, we allocate FP_REGS in the order v0-v31,
   we run the risk of starving other (lower-priority) pseudos that
   require FP_LO8_REGS or FP_LO_REGS.  Allocating FP_LO_REGS in the
   order v0-v15 could similarly starve pseudos that require FP_LO8_REGS.
   Allocating downwards rather than upwards avoids this problem, at least
   in code that has reasonable register pressure.

   The situation for predicate registers is similar.  */

void
aarch64_adjust_reg_alloc_order ()
{
  for (int i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    if (IN_RANGE (i, V0_REGNUM, V31_REGNUM))
      reg_alloc_order[i] = V31_REGNUM - (i - V0_REGNUM);
    else if (IN_RANGE (i, P0_REGNUM, P15_REGNUM))
      reg_alloc_order[i] = P15_REGNUM - (i - P0_REGNUM);
    else
      reg_alloc_order[i] = i;
}

/* Return true if the PARALLEL PAR can be used in a VEC_SELECT expression
   of vector mode MODE to select half the elements of that vector.
   Allow any combination of indices except duplicates (or out of range of
   the mode units).  */

bool
aarch64_parallel_select_half_p (machine_mode mode, rtx par)
{
  int nunits = XVECLEN (par, 0);
  if (!known_eq (GET_MODE_NUNITS (mode), nunits * 2))
    return false;
  int mode_nunits = nunits * 2;
  /* Put all the elements of PAR into a hash_set and use its
     uniqueness guarantees to check that we don't try to insert the same
     element twice.  */
  hash_set<rtx> parset;
  for (int i = 0; i < nunits; ++i)
    {
      rtx elt = XVECEXP (par, 0, i);
      if (!CONST_INT_P (elt)
	  || !IN_RANGE (INTVAL (elt), 0, mode_nunits - 1)
	  || parset.add (elt))
	return false;
    }
  return true;
}

/* Return true if PAR1 and PAR2, two PARALLEL rtxes of CONST_INT values,
   contain any common elements.  */

bool
aarch64_pars_overlap_p (rtx par1, rtx par2)
{
  int len1 = XVECLEN (par1, 0);
  int len2 = XVECLEN (par2, 0);
  hash_set<rtx> parset;
  for (int i = 0; i < len1; ++i)
    parset.add (XVECEXP (par1, 0, i));
  for (int i = 0; i < len2; ++i)
    if (parset.contains (XVECEXP (par2, 0, i)))
      return true;
  return false;
}

/* Implement OPTIMIZE_MODE_SWITCHING.  */

bool
aarch64_optimize_mode_switching (aarch64_mode_entity entity)
{
  bool have_sme_state = (aarch64_cfun_incoming_pstate_za () != 0
			 || (aarch64_cfun_has_new_state ("za")
			     && df_regs_ever_live_p (ZA_REGNUM))
			 || (aarch64_cfun_has_new_state ("zt0")
			     && df_regs_ever_live_p (ZT0_REGNUM)));

  if (have_sme_state && nonlocal_goto_handler_labels)
    {
      static bool reported;
      if (!reported)
	{
	  sorry ("non-local gotos in functions with SME state");
	  reported = true;
	}
    }

  switch (entity)
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
    case aarch64_mode_entity::LOCAL_SME_STATE:
      return have_sme_state && !nonlocal_goto_handler_labels;
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_EMIT for ZA_SAVE_BUFFER.  */

static void
aarch64_mode_emit_za_save_buffer (aarch64_tristate_mode mode,
				  aarch64_tristate_mode prev_mode)
{
  if (mode == aarch64_tristate_mode::YES)
    {
      gcc_assert (prev_mode == aarch64_tristate_mode::NO);
      aarch64_init_tpidr2_block ();
    }
  else
    gcc_unreachable ();
}

/* Implement TARGET_MODE_EMIT for LOCAL_SME_STATE.  */

static void
aarch64_mode_emit_local_sme_state (aarch64_local_sme_state mode,
				   aarch64_local_sme_state prev_mode)
{
  /* Back-propagation should ensure that we're always starting from
     a known mode.  */
  gcc_assert (prev_mode != aarch64_local_sme_state::ANY);

  if (prev_mode == aarch64_local_sme_state::INACTIVE_CALLER)
    {
      /* Commit any uncommitted lazy save.  This leaves ZA either active
	 and zero (lazy save case) or off (normal case).

	 The sequence is:

	     mrs <temp>, tpidr2_el0
	     cbz <temp>, no_save
	     bl __arm_tpidr2_save
	     msr tpidr2_el0, xzr
	     zero { za }       // Only if ZA is live
	     zero { zt0 }      // Only if ZT0 is live
	 no_save:  */
      auto tmp_reg = gen_reg_rtx (DImode);
      emit_insn (gen_aarch64_read_tpidr2 (tmp_reg));
      auto label = gen_label_rtx ();
      rtx branch = aarch64_gen_compare_zero_and_branch (EQ, tmp_reg, label);
      auto jump = emit_jump_insn (branch);
      JUMP_LABEL (jump) = label;
      emit_insn (gen_aarch64_tpidr2_save ());
      emit_insn (gen_aarch64_clear_tpidr2 ());
      if (mode == aarch64_local_sme_state::ACTIVE_LIVE
	  || mode == aarch64_local_sme_state::ACTIVE_DEAD)
	{
	  if (aarch64_cfun_has_state ("za"))
	    emit_insn (gen_aarch64_initial_zero_za ());
	  if (aarch64_cfun_has_state ("zt0"))
	    emit_insn (gen_aarch64_sme_zero_zt0 ());
	}
      emit_label (label);
    }

  if (mode == aarch64_local_sme_state::ACTIVE_LIVE
      || mode == aarch64_local_sme_state::ACTIVE_DEAD)
    {
      if (prev_mode == aarch64_local_sme_state::INACTIVE_LOCAL)
	{
	  /* Make ZA active after being inactive.

	     First handle the case in which the lazy save we set up was
	     committed by a callee.  If the function's source-level ZA state
	     is live then we must conditionally restore it from the lazy
	     save buffer.  Otherwise we can just force PSTATE.ZA to 1.  */
	  if (mode == aarch64_local_sme_state::ACTIVE_LIVE)
	    emit_insn (gen_aarch64_restore_za (aarch64_get_tpidr2_ptr ()));
	  else
	    emit_insn (gen_aarch64_smstart_za ());

	  /* Now handle the case in which the lazy save was not committed.
	     In that case, ZA still contains the current function's ZA state,
	     and we just need to cancel the lazy save.  */
	  emit_insn (gen_aarch64_clear_tpidr2 ());

	  /* Restore the ZT0 state, if we have some.  */
	  if (aarch64_cfun_has_state ("zt0"))
	    aarch64_restore_zt0 (true);

	  return;
	}

      if (prev_mode == aarch64_local_sme_state::SAVED_LOCAL)
	{
	  /* Retrieve the current function's ZA state from the lazy save
	     buffer.  */
	  aarch64_restore_za (aarch64_get_tpidr2_ptr ());

	  /* Restore the ZT0 state, if we have some.  */
	  if (aarch64_cfun_has_state ("zt0"))
	    aarch64_restore_zt0 (true);
	  return;
	}

      if (prev_mode == aarch64_local_sme_state::INACTIVE_CALLER
	  || prev_mode == aarch64_local_sme_state::OFF)
	{
	  /* INACTIVE_CALLER means that we are enabling ZA for the first
	     time in this function.  The code above means that ZA is either
	     active and zero (if we committed a lazy save) or off.  Handle
	     the latter case by forcing ZA on.

	     OFF means that PSTATE.ZA is guaranteed to be 0.  We just need
	     to force it to 1.

	     Both cases leave ZA zeroed.  */
	  emit_insn (gen_aarch64_smstart_za ());

	  /* Restore the ZT0 state, if we have some.  */
	  if (prev_mode == aarch64_local_sme_state::OFF
	      && aarch64_cfun_has_state ("zt0"))
	    aarch64_restore_zt0 (true);
	  return;
	}

      if (prev_mode == aarch64_local_sme_state::ACTIVE_DEAD
	  || prev_mode == aarch64_local_sme_state::ACTIVE_LIVE)
	/* A simple change in liveness, such as in a CFG structure where
	   ZA is only conditionally defined.  No code is needed.  */
	return;

      gcc_unreachable ();
    }

  if (mode == aarch64_local_sme_state::INACTIVE_LOCAL)
    {
      if (prev_mode == aarch64_local_sme_state::ACTIVE_LIVE
	  || prev_mode == aarch64_local_sme_state::ACTIVE_DEAD
	  || prev_mode == aarch64_local_sme_state::INACTIVE_CALLER)
	{
	  /* Save the ZT0 state, if we have some.  */
	  if (aarch64_cfun_has_state ("zt0"))
	    aarch64_save_zt0 ();

	  /* A transition from ACTIVE_LIVE to INACTIVE_LOCAL is the usual
	     case of setting up a lazy save buffer before a call.
	     A transition from INACTIVE_CALLER is similar, except that
	     the contents of ZA are known to be zero.

	     A transition from ACTIVE_DEAD means that ZA is live at the
	     point of the transition, but is dead on at least one incoming
	     edge.  (That is, ZA is only conditionally initialized.)
	     For efficiency, we want to set up a lazy save even for
	     dead contents, since forcing ZA off would make later code
	     restore ZA from the lazy save buffer.  */
	  emit_insn (gen_aarch64_write_tpidr2 (aarch64_get_tpidr2_ptr ()));
	  return;
	}

      if (prev_mode == aarch64_local_sme_state::SAVED_LOCAL
	  || prev_mode == aarch64_local_sme_state::OFF)
	/* We're simply discarding the information about which inactive
	   state applies.  */
	return;

      gcc_unreachable ();
    }

  if (mode == aarch64_local_sme_state::INACTIVE_CALLER
      || mode == aarch64_local_sme_state::OFF)
    {
      /* Save the ZT0 state, if we have some.  */
      if ((prev_mode == aarch64_local_sme_state::ACTIVE_LIVE
	   || prev_mode == aarch64_local_sme_state::ACTIVE_DEAD)
	  && mode == aarch64_local_sme_state::OFF
	  && aarch64_cfun_has_state ("zt0"))
	aarch64_save_zt0 ();

      /* The transition to INACTIVE_CALLER is used before returning from
	 new("za") functions.  Any state in ZA belongs to the current
	 function rather than a caller, but that state is no longer
	 needed.  Clear any pending lazy save and turn ZA off.

	 The transition to OFF is used before calling a private-ZA function.
	 We committed any incoming lazy save above, so at this point any
	 contents in ZA belong to the current function.  */
      if (prev_mode == aarch64_local_sme_state::INACTIVE_LOCAL)
	emit_insn (gen_aarch64_clear_tpidr2 ());

      if (prev_mode != aarch64_local_sme_state::OFF
	  && prev_mode != aarch64_local_sme_state::SAVED_LOCAL)
	emit_insn (gen_aarch64_smstop_za ());

      return;
    }

  if (mode == aarch64_local_sme_state::SAVED_LOCAL)
    {
      /* This is a transition to an exception handler.  */
      gcc_assert (prev_mode == aarch64_local_sme_state::OFF
		  || prev_mode == aarch64_local_sme_state::INACTIVE_LOCAL);
      return;
    }

  gcc_unreachable ();
}

/* Implement TARGET_MODE_EMIT.  */

static void
aarch64_mode_emit (int entity, int mode, int prev_mode, HARD_REG_SET live)
{
  if (mode == prev_mode)
    return;

  start_sequence ();
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      aarch64_mode_emit_za_save_buffer (aarch64_tristate_mode (mode),
					aarch64_tristate_mode (prev_mode));
      break;

    case aarch64_mode_entity::LOCAL_SME_STATE:
      aarch64_mode_emit_local_sme_state (aarch64_local_sme_state (mode),
					 aarch64_local_sme_state (prev_mode));
      break;
    }
  rtx_insn *seq = get_insns ();
  end_sequence ();

  /* Get the set of clobbered registers that are currently live.  */
  HARD_REG_SET clobbers = {};
  for (rtx_insn *insn = seq; insn; insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;
      vec_rtx_properties properties;
      properties.add_insn (insn, false);
      for (rtx_obj_reference ref : properties.refs ())
	if (ref.is_write () && HARD_REGISTER_NUM_P (ref.regno))
	  SET_HARD_REG_BIT (clobbers, ref.regno);
    }
  clobbers &= live;

  /* Emit instructions to save clobbered registers to pseudos.  Queue
     instructions to restore the registers afterwards.

     This should only needed in rare situations.  */
  auto_vec<rtx, 33> after;
  for (unsigned int regno = R0_REGNUM; regno < R30_REGNUM; ++regno)
    if (TEST_HARD_REG_BIT (clobbers, regno))
      {
	rtx hard_reg = gen_rtx_REG (DImode, regno);
	rtx pseudo_reg = gen_reg_rtx (DImode);
	emit_move_insn (pseudo_reg, hard_reg);
	after.quick_push (gen_move_insn (hard_reg, pseudo_reg));
      }
  if (TEST_HARD_REG_BIT (clobbers, CC_REGNUM))
    {
      rtx pseudo_reg = gen_reg_rtx (DImode);
      emit_insn (gen_aarch64_save_nzcv (pseudo_reg));
      after.quick_push (gen_aarch64_restore_nzcv (pseudo_reg));
    }

  /* Emit the transition instructions themselves.  */
  emit_insn (seq);

  /* Restore the clobbered registers.  */
  for (auto *insn : after)
    emit_insn (insn);
}

/* Return true if INSN references the SME state represented by hard register
   REGNO.  */

static bool
aarch64_insn_references_sme_state_p (rtx_insn *insn, unsigned int regno)
{
  df_ref ref;
  FOR_EACH_INSN_DEF (ref, insn)
    if (!DF_REF_FLAGS_IS_SET (ref, DF_REF_MUST_CLOBBER)
	&& DF_REF_REGNO (ref) == regno)
      return true;
  FOR_EACH_INSN_USE (ref, insn)
    if (DF_REF_REGNO (ref) == regno)
      return true;
  return false;
}

/* Implement TARGET_MODE_NEEDED for LOCAL_SME_STATE.  */

static aarch64_local_sme_state
aarch64_mode_needed_local_sme_state (rtx_insn *insn, HARD_REG_SET live)
{
  if (!CALL_P (insn)
      && find_reg_note (insn, REG_EH_REGION, NULL_RTX))
    {
      static bool reported;
      if (!reported)
	{
	  sorry ("catching non-call exceptions in functions with SME state");
	  reported = true;
	}
      /* Aim for graceful error recovery by picking the value that is
	 least likely to generate an ICE.  */
      return aarch64_local_sme_state::INACTIVE_LOCAL;
    }

  /* A non-local goto is equivalent to a return.  We disallow non-local
     receivers in functions with SME state, so we know that the target
     expects ZA to be dormant or off.  */
  if (JUMP_P (insn)
      && find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
    return aarch64_local_sme_state::INACTIVE_CALLER;

  /* start_private_za_call and end_private_za_call bracket a sequence
     that calls a private-ZA function.  Force ZA to be turned off if the
     function doesn't have any live ZA state, otherwise require ZA to be
     inactive.  */
  auto icode = recog_memoized (insn);
  if (icode == CODE_FOR_aarch64_start_private_za_call
      || icode == CODE_FOR_aarch64_end_private_za_call)
    return (TEST_HARD_REG_BIT (live, ZA_REGNUM)
	    ? aarch64_local_sme_state::INACTIVE_LOCAL
	    : aarch64_local_sme_state::OFF);

  /* Force ZA to contain the current function's ZA state if INSN wants
     to access it.  Do the same for accesses to ZT0, since ZA and ZT0
     are both controlled by PSTATE.ZA.  */
  if (aarch64_insn_references_sme_state_p (insn, ZA_REGNUM)
      || aarch64_insn_references_sme_state_p (insn, ZT0_REGNUM))
    return (TEST_HARD_REG_BIT (live, ZA_REGNUM)
	    ? aarch64_local_sme_state::ACTIVE_LIVE
	    : aarch64_local_sme_state::ACTIVE_DEAD);

  return aarch64_local_sme_state::ANY;
}

/* Implement TARGET_MODE_NEEDED for ZA_SAVE_BUFFER.  */

static aarch64_tristate_mode
aarch64_mode_needed_za_save_buffer (rtx_insn *insn, HARD_REG_SET live)
{
  /* We need to set up a lazy save buffer no later than the first
     transition to INACTIVE_LOCAL (which involves setting up a lazy save).  */
  if (aarch64_mode_needed_local_sme_state (insn, live)
      == aarch64_local_sme_state::INACTIVE_LOCAL)
    return aarch64_tristate_mode::YES;

  /* Also make sure that the lazy save buffer is set up before the first
     insn that throws internally.  The exception handler will sometimes
     load from it.  */
  if (find_reg_note (insn, REG_EH_REGION, NULL_RTX))
    return aarch64_tristate_mode::YES;

  return aarch64_tristate_mode::MAYBE;
}

/* Implement TARGET_MODE_NEEDED.  */

static int
aarch64_mode_needed (int entity, rtx_insn *insn, HARD_REG_SET live)
{
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      return int (aarch64_mode_needed_za_save_buffer (insn, live));

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_mode_needed_local_sme_state (insn, live));
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_AFTER for LOCAL_SME_STATE.  */

static aarch64_local_sme_state
aarch64_mode_after_local_sme_state (aarch64_local_sme_state mode,
				    HARD_REG_SET live)
{
  /* Note places where ZA dies, so that we can try to avoid saving and
     restoring state that isn't needed.  */
  if (mode == aarch64_local_sme_state::ACTIVE_LIVE
      && !TEST_HARD_REG_BIT (live, ZA_REGNUM))
    return aarch64_local_sme_state::ACTIVE_DEAD;

  /* Note where ZA is born, e.g. when moving past an __arm_out("za")
     function.  */
  if (mode == aarch64_local_sme_state::ACTIVE_DEAD
      && TEST_HARD_REG_BIT (live, ZA_REGNUM))
    return aarch64_local_sme_state::ACTIVE_LIVE;

  return mode;
}

/* Implement TARGET_MODE_AFTER.  */

static int
aarch64_mode_after (int entity, int mode, rtx_insn *, HARD_REG_SET live)
{
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      return mode;

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_mode_after_local_sme_state
		  (aarch64_local_sme_state (mode), live));
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_CONFLUENCE for LOCAL_SME_STATE.  */

static aarch64_local_sme_state
aarch64_local_sme_confluence (aarch64_local_sme_state mode1,
			      aarch64_local_sme_state mode2)
{
  /* Perform a symmetrical check for two values.  */
  auto is_pair = [&](aarch64_local_sme_state val1,
		     aarch64_local_sme_state val2)
    {
      return ((mode1 == val1 && mode2 == val2)
	      || (mode1 == val2 && mode2 == val1));
    };

  /* INACTIVE_CALLER means ZA is off or it has dormant contents belonging
     to a caller.  OFF is one of the options.  */
  if (is_pair (aarch64_local_sme_state::INACTIVE_CALLER,
	       aarch64_local_sme_state::OFF))
    return aarch64_local_sme_state::INACTIVE_CALLER;

  /* Similarly for dormant contents belonging to the current function.  */
  if (is_pair (aarch64_local_sme_state::INACTIVE_LOCAL,
	       aarch64_local_sme_state::OFF))
    return aarch64_local_sme_state::INACTIVE_LOCAL;

  /* Treat a conditionally-initialized value as a fully-initialized value.  */
  if (is_pair (aarch64_local_sme_state::ACTIVE_LIVE,
	       aarch64_local_sme_state::ACTIVE_DEAD))
    return aarch64_local_sme_state::ACTIVE_LIVE;

  return aarch64_local_sme_state::ANY;
}

/* Implement TARGET_MODE_CONFLUENCE.  */

static int
aarch64_mode_confluence (int entity, int mode1, int mode2)
{
  gcc_assert (mode1 != mode2);
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      return int (aarch64_tristate_mode::MAYBE);

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_local_sme_confluence
		  (aarch64_local_sme_state (mode1),
		   aarch64_local_sme_state (mode2)));
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_BACKPROP for an entity that either stays
   NO throughput, or makes one transition from NO to YES.  */

static aarch64_tristate_mode
aarch64_one_shot_backprop (aarch64_tristate_mode mode1,
			   aarch64_tristate_mode mode2)
{
  /* Keep bringing the transition forward until it starts from NO.  */
  if (mode1 == aarch64_tristate_mode::MAYBE
      && mode2 == aarch64_tristate_mode::YES)
    return mode2;

  return aarch64_tristate_mode::MAYBE;
}

/* Implement TARGET_MODE_BACKPROP for LOCAL_SME_STATE.  */

static aarch64_local_sme_state
aarch64_local_sme_backprop (aarch64_local_sme_state mode1,
			    aarch64_local_sme_state mode2)
{
  /* We always need to know what the current state is when transitioning
     to a new state.  Force any location with indeterminate starting state
     to be active.  */
  if (mode1 == aarch64_local_sme_state::ANY)
    switch (mode2)
      {
      case aarch64_local_sme_state::INACTIVE_CALLER:
      case aarch64_local_sme_state::OFF:
      case aarch64_local_sme_state::ACTIVE_DEAD:
	/* The current function's ZA state is not live.  */
	return aarch64_local_sme_state::ACTIVE_DEAD;

      case aarch64_local_sme_state::INACTIVE_LOCAL:
      case aarch64_local_sme_state::ACTIVE_LIVE:
	/* The current function's ZA state is live.  */
	return aarch64_local_sme_state::ACTIVE_LIVE;

      case aarch64_local_sme_state::SAVED_LOCAL:
	/* This is a transition to an exception handler.  Since we don't
	   support non-call exceptions for SME functions, the source of
	   the transition must be known.  We'll assert later if that's
	   not the case.  */
	return aarch64_local_sme_state::ANY;

      case aarch64_local_sme_state::ANY:
	return aarch64_local_sme_state::ANY;
      }

  return aarch64_local_sme_state::ANY;
}

/* Implement TARGET_MODE_BACKPROP.  */

static int
aarch64_mode_backprop (int entity, int mode1, int mode2)
{
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      return int (aarch64_one_shot_backprop (aarch64_tristate_mode (mode1),
					     aarch64_tristate_mode (mode2)));

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_local_sme_backprop
		  (aarch64_local_sme_state (mode1),
		   aarch64_local_sme_state (mode2)));
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_ENTRY.  */

static int
aarch64_mode_entry (int entity)
{
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      return int (aarch64_tristate_mode::NO);

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_cfun_shared_flags ("za") != 0
		  ? aarch64_local_sme_state::ACTIVE_LIVE
		  : aarch64_cfun_incoming_pstate_za () != 0
		  ? aarch64_local_sme_state::ACTIVE_DEAD
		  : aarch64_local_sme_state::INACTIVE_CALLER);
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_EXIT.  */

static int
aarch64_mode_exit (int entity)
{
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      return int (aarch64_tristate_mode::MAYBE);

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_cfun_shared_flags ("za") != 0
		  ? aarch64_local_sme_state::ACTIVE_LIVE
		  : aarch64_cfun_incoming_pstate_za () != 0
		  ? aarch64_local_sme_state::ACTIVE_DEAD
		  : aarch64_local_sme_state::INACTIVE_CALLER);
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_EH_HANDLER.  */

static int
aarch64_mode_eh_handler (int entity)
{
  switch (aarch64_mode_entity (entity))
    {
    case aarch64_mode_entity::HAVE_ZA_SAVE_BUFFER:
      /* Require a lazy save buffer to be allocated before the first
	 insn that can throw.  */
      return int (aarch64_tristate_mode::YES);

    case aarch64_mode_entity::LOCAL_SME_STATE:
      return int (aarch64_local_sme_state::SAVED_LOCAL);
    }
  gcc_unreachable ();
}

/* Implement TARGET_MODE_PRIORITY.  */

static int
aarch64_mode_priority (int, int n)
{
  return n;
}

/* Implement TARGET_MD_ASM_ADJUST.  */

static rtx_insn *
aarch64_md_asm_adjust (vec<rtx> &outputs, vec<rtx> &inputs,
		       vec<machine_mode> &input_modes,
		       vec<const char *> &constraints,
		       vec<rtx> &uses, vec<rtx> &clobbers,
		       HARD_REG_SET &clobbered_regs, location_t loc)
{
  rtx_insn *seq = arm_md_asm_adjust (outputs, inputs, input_modes, constraints,
				     uses, clobbers, clobbered_regs, loc);

  /* "za" in the clobber list of a function with ZA state is defined to
     mean that the asm can read from and write to ZA.  We can model the
     read using a USE, but unfortunately, it's not possible to model the
     write directly.   Use a separate insn to model the effect.

     We must ensure that ZA is active on entry, which is enforced by using
     SME_STATE_REGNUM.  The asm must ensure that ZA is active on return.

     The same thing applies to ZT0.  */
  if (TARGET_ZA)
    for (unsigned int i = clobbers.length (); i-- > 0; )
      {
	rtx x = clobbers[i];
	if (REG_P (x)
	    && (REGNO (x) == ZA_REGNUM || REGNO (x) == ZT0_REGNUM))
	  {
	    auto id = cfun->machine->next_asm_update_za_id++;

	    start_sequence ();
	    if (seq)
	      emit_insn (seq);
	    rtx id_rtx = gen_int_mode (id, SImode);
	    emit_insn (REGNO (x) == ZA_REGNUM
		       ? gen_aarch64_asm_update_za (id_rtx)
		       : gen_aarch64_asm_update_zt0 (id_rtx));
	    seq = get_insns ();
	    end_sequence ();

	    auto mode = REGNO (x) == ZA_REGNUM ? VNx16QImode : V8DImode;
	    uses.safe_push (gen_rtx_REG (mode, REGNO (x)));
	    uses.safe_push (gen_rtx_REG (DImode, SME_STATE_REGNUM));

	    clobbers.ordered_remove (i);
	    CLEAR_HARD_REG_BIT (clobbered_regs, REGNO (x));
	  }
      }
  return seq;
}

/* BB is the target of an exception or nonlocal goto edge, which means
   that PSTATE.SM is known to be 0 on entry.  Put it into the state that
   the current function requires.  */

static bool
aarch64_switch_pstate_sm_for_landing_pad (basic_block bb)
{
  if (TARGET_NON_STREAMING)
    return false;

  start_sequence ();
  rtx_insn *guard_label = nullptr;
  if (TARGET_STREAMING_COMPATIBLE)
    guard_label = aarch64_guard_switch_pstate_sm (IP0_REGNUM,
						  AARCH64_ISA_MODE_SM_OFF);
  aarch64_sme_mode_switch_regs args_switch;
  args_switch.add_call_preserved_regs (df_get_live_in (bb));
  args_switch.emit_prologue ();
  aarch64_switch_pstate_sm (AARCH64_ISA_MODE_SM_OFF, AARCH64_ISA_MODE_SM_ON);
  args_switch.emit_epilogue ();
  if (guard_label)
    emit_label (guard_label);
  auto seq = get_insns ();
  end_sequence ();

  emit_insn_after (seq, bb_note (bb));
  return true;
}

/* JUMP is a nonlocal goto.  Its target requires PSTATE.SM to be 0 on entry,
   so arrange to make it so.  */

static bool
aarch64_switch_pstate_sm_for_jump (rtx_insn *jump)
{
  if (TARGET_NON_STREAMING)
    return false;

  start_sequence ();
  rtx_insn *guard_label = nullptr;
  if (TARGET_STREAMING_COMPATIBLE)
    guard_label = aarch64_guard_switch_pstate_sm (IP0_REGNUM,
						  AARCH64_ISA_MODE_SM_OFF);
  aarch64_switch_pstate_sm (AARCH64_ISA_MODE_SM_ON, AARCH64_ISA_MODE_SM_OFF);
  if (guard_label)
    emit_label (guard_label);
  auto seq = get_insns ();
  end_sequence ();

  emit_insn_before (seq, jump);
  return true;
}

/* If CALL involves a change in PSTATE.SM, emit the instructions needed
   to switch to the new mode and the instructions needed to restore the
   original mode.  Return true if something changed.  */
static bool
aarch64_switch_pstate_sm_for_call (rtx_call_insn *call)
{
  /* Mode switches for sibling calls are handled via the epilogue.  */
  if (SIBLING_CALL_P (call))
    return false;

  auto callee_isa_mode = aarch64_insn_callee_isa_mode (call);
  if (!aarch64_call_switches_pstate_sm (callee_isa_mode))
    return false;

  /* Switch mode before the call, preserving any argument registers
     across the switch.  */
  start_sequence ();
  rtx_insn *args_guard_label = nullptr;
  if (TARGET_STREAMING_COMPATIBLE)
    args_guard_label = aarch64_guard_switch_pstate_sm (IP0_REGNUM,
						       callee_isa_mode);
  aarch64_sme_mode_switch_regs args_switch;
  args_switch.add_call_args (call);
  args_switch.emit_prologue ();
  aarch64_switch_pstate_sm (AARCH64_ISA_MODE, callee_isa_mode);
  args_switch.emit_epilogue ();
  if (args_guard_label)
    emit_label (args_guard_label);
  auto args_seq = get_insns ();
  end_sequence ();
  emit_insn_before (args_seq, call);

  if (find_reg_note (call, REG_NORETURN, NULL_RTX))
    return true;

  /* Switch mode after the call, preserving any return registers across
     the switch.  */
  start_sequence ();
  rtx_insn *return_guard_label = nullptr;
  if (TARGET_STREAMING_COMPATIBLE)
    return_guard_label = aarch64_guard_switch_pstate_sm (IP0_REGNUM,
							 callee_isa_mode);
  aarch64_sme_mode_switch_regs return_switch;
  return_switch.add_call_result (call);
  return_switch.emit_prologue ();
  aarch64_switch_pstate_sm (callee_isa_mode, AARCH64_ISA_MODE);
  return_switch.emit_epilogue ();
  if (return_guard_label)
    emit_label (return_guard_label);
  auto result_seq = get_insns ();
  end_sequence ();
  emit_insn_after (result_seq, call);
  return true;
}

namespace {

const pass_data pass_data_switch_pstate_sm =
{
  RTL_PASS, // type
  "smstarts", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_NONE, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_finish, // todo_flags_finish
};

class pass_switch_pstate_sm : public rtl_opt_pass
{
public:
  pass_switch_pstate_sm (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_switch_pstate_sm, ctxt)
  {}

  // opt_pass methods:
  bool gate (function *) override final;
  unsigned int execute (function *) override final;
};

bool
pass_switch_pstate_sm::gate (function *fn)
{
  return (aarch64_fndecl_pstate_sm (fn->decl) != AARCH64_ISA_MODE_SM_OFF
	  || cfun->machine->call_switches_pstate_sm);
}

/* Emit any instructions needed to switch PSTATE.SM.  */
unsigned int
pass_switch_pstate_sm::execute (function *fn)
{
  basic_block bb;

  auto_sbitmap blocks (last_basic_block_for_fn (cfun));
  bitmap_clear (blocks);
  FOR_EACH_BB_FN (bb, fn)
    {
      if (has_abnormal_call_or_eh_pred_edge_p (bb)
	  && aarch64_switch_pstate_sm_for_landing_pad (bb))
	bitmap_set_bit (blocks, bb->index);

      if (cfun->machine->call_switches_pstate_sm)
	{
	  rtx_insn *insn;
	  FOR_BB_INSNS (bb, insn)
	    if (auto *call = dyn_cast<rtx_call_insn *> (insn))
	      if (aarch64_switch_pstate_sm_for_call (call))
		bitmap_set_bit (blocks, bb->index);
	}

      auto end = BB_END (bb);
      if (JUMP_P (end)
	  && find_reg_note (end, REG_NON_LOCAL_GOTO, NULL_RTX)
	  && aarch64_switch_pstate_sm_for_jump (end))
	bitmap_set_bit (blocks, bb->index);
    }
  find_many_sub_basic_blocks (blocks);
  clear_aux_for_blocks ();
  return 0;
}

}

rtl_opt_pass *
make_pass_switch_pstate_sm (gcc::context *ctxt)
{
  return new pass_switch_pstate_sm (ctxt);
}

/* Parse an implementation-defined system register name of
   the form S[0-3]_[0-7]_C[0-15]_C[0-15]_[0-7].
   Return true if name matched against above pattern, false
   otherwise.  */
bool
aarch64_is_implem_def_reg (const char *regname)
{
  unsigned pos = 0;
  unsigned name_len = strlen (regname);
  if (name_len < 12 || name_len > 14)
    return false;

  auto cterm_valid_p = [&]()
  {
    bool leading_zero_p = false;
    unsigned i = 0;
    char n[3] = {0};

    if (regname[pos] != 'c')
      return false;
    pos++;
    while (regname[pos] != '_')
      {
	if (leading_zero_p)
	  return false;
	if (i == 0 && regname[pos] == '0')
	  leading_zero_p = true;
	if (i > 2)
	  return false;
	if (!ISDIGIT (regname[pos]))
	  return false;
	n[i++] = regname[pos++];
      }
    if (atoi (n) > 15)
      return false;
    return true;
  };

  if (regname[pos] != 's')
    return false;
  pos++;
  if (regname[pos] < '0' || regname[pos] > '3')
    return false;
  pos++;
  if (regname[pos++] != '_')
    return false;
  if (regname[pos] < '0' || regname[pos] > '7')
    return false;
  pos++;
  if (regname[pos++] != '_')
    return false;
  if (!cterm_valid_p ())
    return false;
  if (regname[pos++] != '_')
    return false;
  if (!cterm_valid_p ())
    return false;
  if (regname[pos++] != '_')
    return false;
  if (regname[pos] < '0' || regname[pos] > '7')
    return false;
  return true;
}

/* Return true if REGNAME matches either a known permitted system
   register name, or a generic sysreg specification.  For use in
   back-end predicate `aarch64_sysreg_string'.  */
bool
aarch64_valid_sysreg_name_p (const char *regname)
{
  const sysreg_t *sysreg = aarch64_lookup_sysreg_map (regname);
  if (sysreg == NULL)
    return aarch64_is_implem_def_reg (regname);
  if (sysreg->arch_reqs)
    return bool (aarch64_isa_flags & sysreg->arch_reqs);
  return true;
}

/* Return the generic sysreg specification for a valid system register
   name, otherwise NULL.  WRITE_P is true iff the register is being
   written to.  IS128OP indicates the requested system register should
   be checked for a 128-bit implementation.  */
const char *
aarch64_retrieve_sysreg (const char *regname, bool write_p, bool is128op)
{
  const sysreg_t *sysreg = aarch64_lookup_sysreg_map (regname);
  if (sysreg == NULL)
    {
      if (aarch64_is_implem_def_reg (regname))
	return regname;
      else
	return NULL;
    }
  if (is128op && !(sysreg->properties & F_REG_128))
    return NULL;
  if ((write_p && (sysreg->properties & F_REG_READ))
      || (!write_p && (sysreg->properties & F_REG_WRITE)))
    return NULL;
  if ((~aarch64_isa_flags & sysreg->arch_reqs) != 0)
    return NULL;
  return sysreg->encoding;
}

/* Target-specific selftests.  */

#if CHECKING_P

namespace selftest {

/* Selftest for the RTL loader.
   Verify that the RTL loader copes with a dump from
   print_rtx_function.  This is essentially just a test that class
   function_reader can handle a real dump, but it also verifies
   that lookup_reg_by_dump_name correctly handles hard regs.
   The presence of hard reg names in the dump means that the test is
   target-specific, hence it is in this file.  */

static void
aarch64_test_loading_full_dump ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("aarch64/times-two.rtl"));

  ASSERT_STREQ ("times_two", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));

  rtx_insn *insn_1 = get_insn_by_uid (1);
  ASSERT_EQ (NOTE, GET_CODE (insn_1));

  rtx_insn *insn_15 = get_insn_by_uid (15);
  ASSERT_EQ (INSN, GET_CODE (insn_15));
  ASSERT_EQ (USE, GET_CODE (PATTERN (insn_15)));

  /* Verify crtl->return_rtx.  */
  ASSERT_EQ (REG, GET_CODE (crtl->return_rtx));
  ASSERT_EQ (0, REGNO (crtl->return_rtx));
  ASSERT_EQ (SImode, GET_MODE (crtl->return_rtx));
}

/* Test the fractional_cost class.  */

static void
aarch64_test_fractional_cost ()
{
  using cf = fractional_cost;

  ASSERT_EQ (cf (0, 20), 0);

  ASSERT_EQ (cf (4, 2), 2);
  ASSERT_EQ (3, cf (9, 3));

  ASSERT_NE (cf (5, 2), 2);
  ASSERT_NE (3, cf (8, 3));

  ASSERT_EQ (cf (7, 11) + cf (15, 11), 2);
  ASSERT_EQ (cf (2, 3) + cf (3, 5), cf (19, 15));
  ASSERT_EQ (cf (2, 3) + cf (1, 6) + cf (1, 6), 1);

  ASSERT_EQ (cf (14, 15) - cf (4, 15), cf (2, 3));
  ASSERT_EQ (cf (1, 4) - cf (1, 2), 0);
  ASSERT_EQ (cf (3, 5) - cf (1, 10), cf (1, 2));
  ASSERT_EQ (cf (11, 3) - 3, cf (2, 3));
  ASSERT_EQ (3 - cf (7, 3), cf (2, 3));
  ASSERT_EQ (3 - cf (10, 3), 0);

  ASSERT_EQ (cf (2, 3) * 5, cf (10, 3));
  ASSERT_EQ (14 * cf (11, 21), cf (22, 3));

  ASSERT_TRUE (cf (4, 15) <= cf (5, 15));
  ASSERT_TRUE (cf (5, 15) <= cf (5, 15));
  ASSERT_FALSE (cf (6, 15) <= cf (5, 15));
  ASSERT_TRUE (cf (1, 3) <= cf (2, 5));
  ASSERT_TRUE (cf (1, 12) <= cf (1, 6));
  ASSERT_TRUE (cf (5, 3) <= cf (5, 3));
  ASSERT_TRUE (cf (239, 240) <= 1);
  ASSERT_TRUE (cf (240, 240) <= 1);
  ASSERT_FALSE (cf (241, 240) <= 1);
  ASSERT_FALSE (2 <= cf (207, 104));
  ASSERT_TRUE (2 <= cf (208, 104));
  ASSERT_TRUE (2 <= cf (209, 104));

  ASSERT_TRUE (cf (4, 15) < cf (5, 15));
  ASSERT_FALSE (cf (5, 15) < cf (5, 15));
  ASSERT_FALSE (cf (6, 15) < cf (5, 15));
  ASSERT_TRUE (cf (1, 3) < cf (2, 5));
  ASSERT_TRUE (cf (1, 12) < cf (1, 6));
  ASSERT_FALSE (cf (5, 3) < cf (5, 3));
  ASSERT_TRUE (cf (239, 240) < 1);
  ASSERT_FALSE (cf (240, 240) < 1);
  ASSERT_FALSE (cf (241, 240) < 1);
  ASSERT_FALSE (2 < cf (207, 104));
  ASSERT_FALSE (2 < cf (208, 104));
  ASSERT_TRUE (2 < cf (209, 104));

  ASSERT_FALSE (cf (4, 15) >= cf (5, 15));
  ASSERT_TRUE (cf (5, 15) >= cf (5, 15));
  ASSERT_TRUE (cf (6, 15) >= cf (5, 15));
  ASSERT_FALSE (cf (1, 3) >= cf (2, 5));
  ASSERT_FALSE (cf (1, 12) >= cf (1, 6));
  ASSERT_TRUE (cf (5, 3) >= cf (5, 3));
  ASSERT_FALSE (cf (239, 240) >= 1);
  ASSERT_TRUE (cf (240, 240) >= 1);
  ASSERT_TRUE (cf (241, 240) >= 1);
  ASSERT_TRUE (2 >= cf (207, 104));
  ASSERT_TRUE (2 >= cf (208, 104));
  ASSERT_FALSE (2 >= cf (209, 104));

  ASSERT_FALSE (cf (4, 15) > cf (5, 15));
  ASSERT_FALSE (cf (5, 15) > cf (5, 15));
  ASSERT_TRUE (cf (6, 15) > cf (5, 15));
  ASSERT_FALSE (cf (1, 3) > cf (2, 5));
  ASSERT_FALSE (cf (1, 12) > cf (1, 6));
  ASSERT_FALSE (cf (5, 3) > cf (5, 3));
  ASSERT_FALSE (cf (239, 240) > 1);
  ASSERT_FALSE (cf (240, 240) > 1);
  ASSERT_TRUE (cf (241, 240) > 1);
  ASSERT_TRUE (2 > cf (207, 104));
  ASSERT_FALSE (2 > cf (208, 104));
  ASSERT_FALSE (2 > cf (209, 104));

  ASSERT_EQ (cf (1, 2).ceil (), 1);
  ASSERT_EQ (cf (11, 7).ceil (), 2);
  ASSERT_EQ (cf (20, 1).ceil (), 20);
  ASSERT_EQ ((cf (0xfffffffd) + 1).ceil (), 0xfffffffe);
  ASSERT_EQ ((cf (0xfffffffd) + 2).ceil (), 0xffffffff);
  ASSERT_EQ ((cf (0xfffffffd) + 3).ceil (), 0xffffffff);
  ASSERT_EQ ((cf (0x7fffffff) * 2).ceil (), 0xfffffffe);
  ASSERT_EQ ((cf (0x80000000) * 2).ceil (), 0xffffffff);

  ASSERT_EQ (cf (1, 2).as_double (), 0.5);
}

/* Calculate whether our system register data, as imported from
   `aarch64-sys-reg.def' has any duplicate entries.  */
static void
aarch64_test_sysreg_encoding_clashes (void)
{
  using dup_instances_t = hash_map<nofree_string_hash,
				   std::vector<const sysreg_t*>>;

  dup_instances_t duplicate_instances;

  /* Every time an encoding is established to come up more than once
     we add it to a "clash-analysis queue", which is then used to extract
     necessary information from our hash map when establishing whether
     repeated encodings are valid.  */

  /* 1) Collect recurrence information.  */
  for (unsigned i = 0; i < ARRAY_SIZE (aarch64_sysregs); i++)
    {
      const sysreg_t *reg = aarch64_sysregs + i;

      std::vector<const sysreg_t*> *tmp
	= &duplicate_instances.get_or_insert (reg->encoding);

      tmp->push_back (reg);
    }

  /* 2) Carry out analysis on collected data.  */
  for (auto instance : duplicate_instances)
    {
      unsigned nrep = instance.second.size ();
      if (nrep > 1)
	for (unsigned i = 0; i < nrep; i++)
	  for (unsigned j = i + 1; j < nrep; j++)
	    {
	      const sysreg_t *a = instance.second[i];
	      const sysreg_t *b = instance.second[j];
	      ASSERT_TRUE ((a->properties != b->properties)
			   || (a->arch_reqs != b->arch_reqs));
	    }
    }
}

/* Run all target-specific selftests.  */

static void
aarch64_run_selftests (void)
{
  aarch64_test_loading_full_dump ();
  aarch64_test_fractional_cost ();
  aarch64_test_sysreg_encoding_clashes ();
}

} // namespace selftest

#endif /* #if CHECKING_P */

#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD aarch64_stack_protect_guard

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST aarch64_address_cost

/* This hook will determines whether unnamed bitfields affect the alignment
   of the containing structure.  The hook returns true if the structure
   should inherit the alignment requirements of an unnamed bitfield's
   type.  */
#undef TARGET_ALIGN_ANON_BITFIELD
#define TARGET_ALIGN_ANON_BITFIELD hook_bool_void_true

#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.xword\t"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#if TARGET_PECOFF
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP
#endif

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START aarch64_start_file

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK aarch64_output_mi_thunk

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION aarch64_select_rtx_section

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE aarch64_asm_trampoline_template

#undef TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY
#define TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY aarch64_print_patchable_function_entry

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST aarch64_build_builtin_va_list

#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_arg_info_false

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED aarch64_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE aarch64_can_eliminate

#undef TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P
#define TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P \
  aarch64_function_attribute_inlinable_p

#undef TARGET_NEED_IPA_FN_TARGET_INFO
#define TARGET_NEED_IPA_FN_TARGET_INFO aarch64_need_ipa_fn_target_info

#undef TARGET_UPDATE_IPA_FN_TARGET_INFO
#define TARGET_UPDATE_IPA_FN_TARGET_INFO aarch64_update_ipa_fn_target_info

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P aarch64_can_inline_p

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM aarch64_cannot_force_const_mem

#undef TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD aarch64_case_values_threshold

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE aarch64_conditional_register_usage

#undef TARGET_MEMBER_TYPE_FORCES_BLK
#define TARGET_MEMBER_TYPE_FORCES_BLK aarch64_member_type_forces_blk

/* Only the least significant bit is used for initialization guard
   variables.  */
#undef TARGET_CXX_GUARD_MASK_BIT
#define TARGET_CXX_GUARD_MASK_BIT hook_bool_void_true

#undef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX aarch64_c_mode_for_suffix

#ifdef TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_END)
#endif

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS aarch64_class_max_nregs

#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL aarch64_builtin_decl

#undef TARGET_BUILTIN_RECIPROCAL
#define TARGET_BUILTIN_RECIPROCAL aarch64_builtin_reciprocal

#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION aarch64_excess_precision

#undef TARGET_C_BITINT_TYPE_INFO
#define TARGET_C_BITINT_TYPE_INFO aarch64_bitint_type_info

#undef TARGET_C_MODE_FOR_FLOATING_TYPE
#define TARGET_C_MODE_FOR_FLOATING_TYPE aarch64_c_mode_for_floating_type

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN aarch64_expand_builtin

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START aarch64_expand_builtin_va_start

#undef TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN aarch64_fold_builtin

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG aarch64_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE aarch64_function_arg_advance

#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY aarch64_function_arg_boundary

#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING aarch64_function_arg_padding

#undef TARGET_GET_RAW_RESULT_MODE
#define TARGET_GET_RAW_RESULT_MODE aarch64_get_reg_raw_mode
#undef TARGET_GET_RAW_ARG_MODE
#define TARGET_GET_RAW_ARG_MODE aarch64_get_reg_raw_mode

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL aarch64_function_ok_for_sibcall

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE aarch64_function_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P aarch64_function_value_regno_p

#undef TARGET_START_CALL_ARGS
#define TARGET_START_CALL_ARGS aarch64_start_call_args

#undef TARGET_END_CALL_ARGS
#define TARGET_END_CALL_ARGS aarch64_end_call_args

#undef TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN aarch64_gimple_fold_builtin

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR aarch64_gimplify_va_arg_expr

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  aarch64_init_builtins

#undef TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS
#define TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS \
  aarch64_ira_change_pseudo_allocno_class

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P aarch64_legitimate_address_hook_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P aarch64_legitimate_constant_p

#undef TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT
#define TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT \
  aarch64_legitimize_address_displacement

#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE aarch64_libgcc_cmp_return_mode

#undef TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P
#define TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P \
aarch64_libgcc_floating_mode_supported_p

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE aarch64_mangle_type

#undef TARGET_INVALID_CONVERSION
#define TARGET_INVALID_CONVERSION aarch64_invalid_conversion

#undef TARGET_INVALID_UNARY_OP
#define TARGET_INVALID_UNARY_OP aarch64_invalid_unary_op

#undef TARGET_INVALID_BINARY_OP
#define TARGET_INVALID_BINARY_OP aarch64_invalid_binary_op

#undef TARGET_VERIFY_TYPE_CONTEXT
#define TARGET_VERIFY_TYPE_CONTEXT aarch64_verify_type_context

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST aarch64_memory_move_cost

#undef TARGET_MIN_DIVISIONS_FOR_RECIP_MUL
#define TARGET_MIN_DIVISIONS_FOR_RECIP_MUL aarch64_min_divisions_for_recip_mul

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

/* This target hook should return true if accesses to volatile bitfields
   should use the narrowest mode possible.  It should return false if these
   accesses should use the bitfield container type.  */
#undef TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE aarch64_override_options

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE \
  aarch64_override_options_after_change

#undef TARGET_OFFLOAD_OPTIONS
#define TARGET_OFFLOAD_OPTIONS aarch64_offload_options

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE aarch64_option_restore

#undef TARGET_OPTION_PRINT
#define TARGET_OPTION_PRINT aarch64_option_print

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P aarch64_option_valid_attribute_p

#undef TARGET_OPTION_VALID_VERSION_ATTRIBUTE_P
#define TARGET_OPTION_VALID_VERSION_ATTRIBUTE_P \
  aarch64_option_valid_version_attribute_p

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION aarch64_set_current_function

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE aarch64_pass_by_reference

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS aarch64_preferred_reload_class

#undef TARGET_SCHED_REASSOCIATION_WIDTH
#define TARGET_SCHED_REASSOCIATION_WIDTH aarch64_reassociation_width

#undef TARGET_DWARF_FRAME_REG_MODE
#define TARGET_DWARF_FRAME_REG_MODE aarch64_dwarf_frame_reg_mode

#undef TARGET_OUTPUT_CFI_DIRECTIVE
#define TARGET_OUTPUT_CFI_DIRECTIVE aarch64_output_cfi_directive

#undef TARGET_DW_CFI_OPRND1_DESC
#define TARGET_DW_CFI_OPRND1_DESC aarch64_dw_cfi_oprnd1_desc

#undef TARGET_PROMOTED_TYPE
#define TARGET_PROMOTED_TYPE aarch64_promoted_type

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD aarch64_secondary_reload

#undef TARGET_SECONDARY_MEMORY_NEEDED
#define TARGET_SECONDARY_MEMORY_NEEDED aarch64_secondary_memory_needed

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK aarch64_shift_truncation_mask

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS aarch64_setup_incoming_varargs

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX   aarch64_struct_value_rtx

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST aarch64_register_move_cost

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY aarch64_return_in_memory

#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB aarch64_return_in_msb

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS aarch64_rtx_costs_wrapper

#undef TARGET_INSN_COST
#define TARGET_INSN_COST aarch64_insn_cost

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P aarch64_scalar_mode_supported_p

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE aarch64_sched_issue_rate

#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE aarch64_sched_variable_issue

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  aarch64_sched_first_cycle_multipass_dfa_lookahead

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD \
  aarch64_first_cycle_multipass_dfa_lookahead_guard

#undef TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS
#define TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS \
  aarch64_get_separate_components

#undef TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB
#define TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB \
  aarch64_components_for_bb

#undef TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS
#define TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS \
  aarch64_disqualify_components

#undef TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS \
  aarch64_emit_prologue_components

#undef TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS \
  aarch64_emit_epilogue_components

#undef TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS
#define TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS \
  aarch64_set_handled_components

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT aarch64_trampoline_init

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P aarch64_use_blocks_for_constant_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P aarch64_vector_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_ANY_TARGET_P
#define TARGET_VECTOR_MODE_SUPPORTED_ANY_TARGET_P aarch64_vector_mode_supported_any_target_p

#undef TARGET_COMPATIBLE_VECTOR_TYPES_P
#define TARGET_COMPATIBLE_VECTOR_TYPES_P aarch64_compatible_vector_types_p

#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT \
  aarch64_builtin_support_vector_misalignment

#undef TARGET_ARRAY_MODE
#define TARGET_ARRAY_MODE aarch64_array_mode

#undef TARGET_ARRAY_MODE_SUPPORTED_P
#define TARGET_ARRAY_MODE_SUPPORTED_P aarch64_array_mode_supported_p

#undef TARGET_VECTORIZE_CREATE_COSTS
#define TARGET_VECTORIZE_CREATE_COSTS aarch64_vectorize_create_costs

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  aarch64_builtin_vectorization_cost

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE aarch64_preferred_simd_mode

#undef TARGET_VECTORIZE_BUILTINS
#define TARGET_VECTORIZE_BUILTINS

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES \
  aarch64_autovectorize_vector_modes

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV \
  aarch64_atomic_assign_expand_fenv

/* Section anchor support.  */

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -256

/* Limit the maximum anchor offset to 4k-1, since that's the limit for a
   byte offset; we can do much more for larger data types, but have no way
   to determine the size of the access.  We assume accesses are aligned.  */
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 4095

#undef TARGET_VECTORIZE_PREFERRED_DIV_AS_SHIFTS_OVER_MULT
#define TARGET_VECTORIZE_PREFERRED_DIV_AS_SHIFTS_OVER_MULT \
  aarch64_vectorize_preferred_div_as_shifts_over_mult

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT aarch64_simd_vector_alignment

#undef TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT
#define TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT \
  aarch64_vectorize_preferred_vector_alignment
#undef TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE \
  aarch64_simd_vector_alignment_reachable

/* vec_perm support.  */

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST \
  aarch64_vectorize_vec_perm_const

#undef TARGET_VECTORIZE_RELATED_MODE
#define TARGET_VECTORIZE_RELATED_MODE aarch64_vectorize_related_mode
#undef TARGET_VECTORIZE_GET_MASK_MODE
#define TARGET_VECTORIZE_GET_MASK_MODE aarch64_get_mask_mode
#undef TARGET_VECTORIZE_CONDITIONAL_OPERATION_IS_EXPENSIVE
#define TARGET_VECTORIZE_CONDITIONAL_OPERATION_IS_EXPENSIVE \
  aarch64_conditional_operation_is_expensive
#undef TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE
#define TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE \
  aarch64_empty_mask_is_expensive
#undef TARGET_PREFERRED_ELSE_VALUE
#define TARGET_PREFERRED_ELSE_VALUE \
  aarch64_preferred_else_value

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS aarch64_init_libfuncs

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS aarch64_fixed_condition_code_regs

#undef TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM CC_REGNUM

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET aarch64_asan_shadow_offset

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS aarch64_legitimize_address

#undef TARGET_SCHED_CAN_SPECULATE_INSN
#define TARGET_SCHED_CAN_SPECULATE_INSN aarch64_sched_can_speculate_insn

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost

#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY aarch64_sched_adjust_priority

#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P aarch64_macro_fusion_p

#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P aarch_macro_fusion_pair_p

#undef TARGET_SCHED_FUSION_PRIORITY
#define TARGET_SCHED_FUSION_PRIORITY aarch64_sched_fusion_priority

#undef TARGET_UNSPEC_MAY_TRAP_P
#define TARGET_UNSPEC_MAY_TRAP_P aarch64_unspec_may_trap_p

#undef TARGET_USE_PSEUDO_PIC_REG
#define TARGET_USE_PSEUDO_PIC_REG aarch64_use_pseudo_pic_reg

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND aarch64_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS aarch64_print_operand_address

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA aarch64_output_addr_const_extra

#undef TARGET_OPTAB_SUPPORTED_P
#define TARGET_OPTAB_SUPPORTED_P aarch64_optab_supported_p

#undef TARGET_OMIT_STRUCT_RETURN_REG
#define TARGET_OMIT_STRUCT_RETURN_REG true

#undef TARGET_DWARF_POLY_INDETERMINATE_VALUE
#define TARGET_DWARF_POLY_INDETERMINATE_VALUE \
  aarch64_dwarf_poly_indeterminate_value

/* The architecture reserves bits 0 and 1 so use bit 2 for descriptors.  */
#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 4

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS aarch64_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK aarch64_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P aarch64_modes_tieable_p

#undef TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  aarch64_hard_regno_call_part_clobbered

#undef TARGET_INSN_CALLEE_ABI
#define TARGET_INSN_CALLEE_ABI aarch64_insn_callee_abi

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT aarch64_constant_alignment

#undef TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE
#define TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE \
  aarch64_stack_clash_protection_alloca_probe_range

#undef TARGET_COMPUTE_PRESSURE_CLASSES
#define TARGET_COMPUTE_PRESSURE_CLASSES aarch64_compute_pressure_classes

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS aarch64_can_change_mode_class

#undef TARGET_SELECT_EARLY_REMAT_MODES
#define TARGET_SELECT_EARLY_REMAT_MODES aarch64_select_early_remat_modes

#undef TARGET_SPECULATION_SAFE_VALUE
#define TARGET_SPECULATION_SAFE_VALUE aarch64_speculation_safe_value

#undef TARGET_ESTIMATED_POLY_VALUE
#define TARGET_ESTIMATED_POLY_VALUE aarch64_estimated_poly_value

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE aarch64_attribute_table

#undef TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN
#define TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN \
  aarch64_simd_clone_compute_vecsize_and_simdlen

#undef TARGET_SIMD_CLONE_ADJUST
#define TARGET_SIMD_CLONE_ADJUST aarch64_simd_clone_adjust

#undef TARGET_SIMD_CLONE_USABLE
#define TARGET_SIMD_CLONE_USABLE aarch64_simd_clone_usable

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES aarch64_comp_type_attributes

#undef TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES aarch64_merge_decl_attributes

#undef TARGET_GET_MULTILIB_ABI_NAME
#define TARGET_GET_MULTILIB_ABI_NAME aarch64_get_multilib_abi_name

#undef TARGET_FNTYPE_ABI
#define TARGET_FNTYPE_ABI aarch64_fntype_abi

#undef TARGET_MEMTAG_CAN_TAG_ADDRESSES
#define TARGET_MEMTAG_CAN_TAG_ADDRESSES aarch64_can_tag_addresses

#if CHECKING_P
#undef TARGET_RUN_TARGET_SELFTESTS
#define TARGET_RUN_TARGET_SELFTESTS selftest::aarch64_run_selftests
#endif /* #if CHECKING_P */

#undef TARGET_ASM_POST_CFI_STARTPROC
#define TARGET_ASM_POST_CFI_STARTPROC aarch64_post_cfi_startproc

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_MODE_EMIT
#define TARGET_MODE_EMIT aarch64_mode_emit

#undef TARGET_MODE_NEEDED
#define TARGET_MODE_NEEDED aarch64_mode_needed

#undef TARGET_MODE_AFTER
#define TARGET_MODE_AFTER aarch64_mode_after

#undef TARGET_MODE_CONFLUENCE
#define TARGET_MODE_CONFLUENCE aarch64_mode_confluence

#undef TARGET_MODE_BACKPROP
#define TARGET_MODE_BACKPROP aarch64_mode_backprop

#undef TARGET_MODE_ENTRY
#define TARGET_MODE_ENTRY aarch64_mode_entry

#undef TARGET_MODE_EXIT
#define TARGET_MODE_EXIT aarch64_mode_exit

#undef TARGET_MODE_EH_HANDLER
#define TARGET_MODE_EH_HANDLER aarch64_mode_eh_handler

#undef TARGET_MODE_PRIORITY
#define TARGET_MODE_PRIORITY aarch64_mode_priority

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST aarch64_md_asm_adjust

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END aarch64_asm_file_end

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE aarch64_sls_emit_blr_function_thunks

#undef TARGET_HAVE_SHADOW_CALL_STACK
#define TARGET_HAVE_SHADOW_CALL_STACK true

#undef TARGET_CONST_ANCHOR
#define TARGET_CONST_ANCHOR 0x1000000

#undef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY aarch64_extra_live_on_entry

#undef TARGET_USE_LATE_PROLOGUE_EPILOGUE
#define TARGET_USE_LATE_PROLOGUE_EPILOGUE aarch64_use_late_prologue_epilogue

#undef TARGET_EMIT_EPILOGUE_FOR_SIBCALL
#define TARGET_EMIT_EPILOGUE_FOR_SIBCALL aarch64_expand_epilogue

#undef TARGET_OPTION_FUNCTION_VERSIONS
#define TARGET_OPTION_FUNCTION_VERSIONS aarch64_common_function_versions

#undef TARGET_COMPARE_VERSION_PRIORITY
#define TARGET_COMPARE_VERSION_PRIORITY aarch64_compare_version_priority

#undef TARGET_GENERATE_VERSION_DISPATCHER_BODY
#define TARGET_GENERATE_VERSION_DISPATCHER_BODY \
  aarch64_generate_version_dispatcher_body

#undef TARGET_GET_FUNCTION_VERSIONS_DISPATCHER
#define TARGET_GET_FUNCTION_VERSIONS_DISPATCHER \
  aarch64_get_function_versions_dispatcher

#undef TARGET_MANGLE_DECL_ASSEMBLER_NAME
#define TARGET_MANGLE_DECL_ASSEMBLER_NAME aarch64_mangle_decl_assembler_name

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-aarch64.h"
