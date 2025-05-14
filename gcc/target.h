/* Data structure definitions for a generic GCC target.
   Copyright (C) 2001-2025 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   In other words, you are welcome to use, share and improve this program.
   You are forbidden to forbid anyone else to use, share and improve
   what you give them.   Help stamp out software-hoarding!  */


/* This file contains a data structure that describes a GCC target.
   At present it is incomplete, but in future it should grow to
   contain most or all target machine and target O/S specific
   information.

   This structure has its initializer declared in target-def.h in the
   form of large macro TARGET_INITIALIZER that expands to many smaller
   macros.

   The smaller macros each initialize one component of the structure,
   and each has a default.  Each target should have a file that
   includes target.h and target-def.h, and overrides any inappropriate
   defaults by undefining the relevant macro and defining a suitable
   replacement.  That file should then contain the definition of
   "targetm" like so:

   struct gcc_target targetm = TARGET_INITIALIZER;

   Doing things this way allows us to bring together everything that
   defines a GCC target.  By supplying a default that is appropriate
   to most targets, we can easily add new items without needing to
   edit dozens of target configuration files.  It should also allow us
   to gradually reduce the amount of conditional compilation that is
   scattered throughout GCC.  */

#ifndef GCC_TARGET_H
#define GCC_TARGET_H

#include "insn-codes.h"
#include "tm.h"
#include "hard-reg-set.h"

#if CHECKING_P

struct cumulative_args_t { void *magic; void *p; };

#else /* !CHECKING_P */

/* When using a GCC build compiler, we could use
   __attribute__((transparent_union)) to get cumulative_args_t function
   arguments passed like scalars where the ABI would mandate a less
   efficient way of argument passing otherwise.  However, that would come
   at the cost of less type-safe !CHECKING_P compilation.  */

union cumulative_args_t { void *p; };

#endif /* !CHECKING_P */

/* Target properties of _BitInt(N) type.  _BitInt(N) is to be represented
   as series of abi_limb_mode CEIL (N, GET_MODE_PRECISION (abi_limb_mode))
   limbs, ordered from least significant to most significant if !big_endian,
   otherwise from most significant to least significant.  If extended is
   false, the bits above or equal to N are undefined when stored in a register
   or memory, otherwise they are zero or sign extended depending on if
   it is unsigned _BitInt(N) or _BitInt(N) / signed _BitInt(N).
   limb_mode is either the same as abi_limb_mode, or some narrower mode
   in which _BitInt lowering should actually perform operations in and
   what libgcc _BitInt helpers should use.
   E.g. abi_limb_mode could be TImode which is something some processor
   specific ABI would specify to use, but it would be desirable to handle
   it as an array of DImode instead for efficiency.
   Note, abi_limb_mode can be different from limb_mode only if big_endian
   matches WORDS_BIG_ENDIAN.  */

struct bitint_info {
  machine_mode abi_limb_mode, limb_mode;
  bool big_endian;
  bool extended;
};

/* Types of memory operation understood by the "by_pieces" infrastructure.
   Used by the TARGET_USE_BY_PIECES_INFRASTRUCTURE_P target hook and
   internally by the functions in expr.cc.  */

enum by_pieces_operation
{
  CLEAR_BY_PIECES,
  MOVE_BY_PIECES,
  SET_BY_PIECES,
  STORE_BY_PIECES,
  COMPARE_BY_PIECES
};

extern unsigned HOST_WIDE_INT by_pieces_ninsns (unsigned HOST_WIDE_INT,
						unsigned int,
						unsigned int,
						by_pieces_operation);

/* An example implementation for ELF targets.  Defined in varasm.cc  */
extern void elf_record_gcc_switches (const char *);

/* Some places still assume that all pointer or address modes are the
   standard Pmode and ptr_mode.  These optimizations become invalid if
   the target actually supports multiple different modes.  For now,
   we disable such optimizations on such targets, using this function.  */
extern bool target_default_pointer_address_modes_p (void);

/* For hooks which use the MOVE_RATIO macro, this gives the legacy default
   behavior.  */
extern unsigned int get_move_ratio (bool);

struct stdarg_info;
struct spec_info_def;
struct hard_reg_set_container;
struct cgraph_node;
struct cgraph_simd_clone;

/* The struct used by the secondary_reload target hook.  */
struct secondary_reload_info
{
  /* icode is actually an enum insn_code, but we don't want to force every
     file that includes target.h to include optabs.h .  */
  int icode;
  int extra_cost; /* Cost for using (a) scratch register(s) to be taken
		     into account by copy_cost.  */
  /* The next two members are for the use of the backward
     compatibility hook.  */
  struct secondary_reload_info *prev_sri;
  int t_icode; /* Actually an enum insn_code - see above.  */
};

/* This is defined in sched-int.h .  */
struct _dep;

/* This is defined in ddg.h .  */
struct ddg;

/* This is defined in cfgloop.h .  */
class loop;

/* This is defined in ifcvt.h.  */
struct noce_if_info;

/* This is defined in tree-ssa-alias.h.  */
class ao_ref;

/* This is defined in tree-vectorizer.h.  */
class _stmt_vec_info;

/* This is defined in calls.h.  */
class function_arg_info;

/* This is defined in function-abi.h.  */
class predefined_function_abi;

/* This is defined in avoid-store-forwarding.h.  */
struct store_fwd_info;

/* These are defined in tree-vect-stmts.cc.  */
extern tree stmt_vectype (class _stmt_vec_info *);
extern bool stmt_in_inner_loop_p (class vec_info *, class _stmt_vec_info *);

/* Assembler instructions for creating various kinds of integer object.  */

struct asm_int_op
{
  const char *hi;
  const char *psi;
  const char *si;
  const char *pdi;
  const char *di;
  const char *pti;
  const char *ti;
};

/* Types of costs for vectorizer cost model.  */
enum vect_cost_for_stmt
{
  scalar_stmt,
  scalar_load,
  scalar_store,
  vector_stmt,
  vector_load,
  vector_gather_load,
  unaligned_load,
  unaligned_store,
  vector_store,
  vector_scatter_store,
  vec_to_scalar,
  scalar_to_vec,
  cond_branch_not_taken,
  cond_branch_taken,
  vec_perm,
  vec_promote_demote,
  vec_construct
};

/* Separate locations for which the vectorizer cost model should
   track costs.  */
enum vect_cost_model_location {
  vect_prologue = 0,
  vect_body = 1,
  vect_epilogue = 2
};

class vec_perm_indices;

/* The type to use for lists of vector sizes.  */
typedef vec<machine_mode> vector_modes;

/* Same, but can be used to construct local lists that are
   automatically freed.  */
typedef auto_vec<machine_mode, 8> auto_vector_modes;

/* First argument of targetm.omp.device_kind_arch_isa.  */
enum omp_device_kind_arch_isa {
  omp_device_kind,
  omp_device_arch,
  omp_device_isa
};

/* Flags returned by TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES:

   VECT_COMPARE_COSTS
       Tells the loop vectorizer to try all the provided modes and
       pick the one with the lowest cost.  By default the vectorizer
       will choose the first mode that works.  */
const unsigned int VECT_COMPARE_COSTS = 1U << 0;

/* The contexts in which the use of a type T can be checked by
   TARGET_VERIFY_TYPE_CONTEXT.  */
enum type_context_kind {
  /* Directly measuring the size of T.  */
  TCTX_SIZEOF,

  /* Directly measuring the alignment of T.  */
  TCTX_ALIGNOF,

  /* Creating objects of type T with static storage duration.  */
  TCTX_STATIC_STORAGE,

  /* Creating objects of type T with thread-local storage duration.  */
  TCTX_THREAD_STORAGE,

  /* Creating a field of type T.  */
  TCTX_FIELD,

  /* Creating an array with elements of type T.  */
  TCTX_ARRAY_ELEMENT,

  /* Adding to or subtracting from a pointer to T, or computing the
     difference between two pointers when one of them is a pointer to T.  */
  TCTX_POINTER_ARITH,

  /* Dynamically allocating objects of type T.  */
  TCTX_ALLOCATION,

  /* Dynamically deallocating objects of type T.  */
  TCTX_DEALLOCATION,

  /* Throwing or catching an object of type T.  */
  TCTX_EXCEPTIONS,

  /* Capturing objects of type T by value in a closure.  */
  TCTX_CAPTURE_BY_COPY,

  /* Objects of type T appearing in OpenMP map clause.  */
  TCTX_OMP_MAP,

  /* Objects of type T appearing in OpenMP target region
     without explicit map.  */
  TCTX_OMP_MAP_IMP_REF,

  /* Objects of type T appearing in OpenMP private clause.  */
  TCTX_OMP_PRIVATE,

  /* Objects of type T appearing in OpenMP firstprivate clause.  */
  TCTX_OMP_FIRSTPRIVATE,

  /* Objects of type T appearing in OpenMP device clauses.  */
  TCTX_OMP_DEVICE_ADDR

};

enum poly_value_estimate_kind
{
  POLY_VALUE_MIN,
  POLY_VALUE_MAX,
  POLY_VALUE_LIKELY
};

enum class spill_cost_type
{
  SAVE,
  RESTORE
};

enum class frame_cost_type
{
  ALLOCATION,
  DEALLOCATION
};

typedef void (*emit_support_tinfos_callback) (tree);

extern bool verify_type_context (location_t, type_context_kind, const_tree,
				 bool = false);

/* The target structure.  This holds all the backend hooks.  */
#define DEFHOOKPOD(NAME, DOC, TYPE, INIT) TYPE NAME;
#define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) TYPE (* NAME) PARAMS;
#define DEFHOOK_UNDOC DEFHOOK
#define HOOKSTRUCT(FRAGMENT) FRAGMENT

#include "target.def"

extern struct gcc_target targetm;

/* Return an estimate of the runtime value of X, for use in things
   like cost calculations or profiling frequencies.  Note that this
   function should never be used in situations where the actual
   runtime value is needed for correctness, since the function only
   provides a rough guess.  */

inline HOST_WIDE_INT
estimated_poly_value (poly_int64 x,
		      poly_value_estimate_kind kind = POLY_VALUE_LIKELY)
{
  if (NUM_POLY_INT_COEFFS == 1)
    return x.coeffs[0];
  else
    return targetm.estimated_poly_value (x, kind);
}

/* Return true when MODE can be used to copy GET_MODE_BITSIZE bits
   unchanged.  */

inline bool
mode_can_transfer_bits (machine_mode mode)
{
  if (mode == BLKmode)
    return true;
  if (maybe_ne (GET_MODE_BITSIZE (mode),
		GET_MODE_UNIT_PRECISION (mode) * GET_MODE_NUNITS (mode)))
    return false;
  if (targetm.mode_can_transfer_bits)
    return targetm.mode_can_transfer_bits (mode);
  return true;
}

/* Return true if OpenMP context types.  */

inline bool
omp_type_context (type_context_kind context)
{
  switch (context)
    {
    case TCTX_OMP_MAP:
    case TCTX_OMP_MAP_IMP_REF:
    case TCTX_OMP_PRIVATE:
    case TCTX_OMP_FIRSTPRIVATE:
    case TCTX_OMP_DEVICE_ADDR:
      return true;
    default:
      return false;
    }
}

#ifdef GCC_TM_H

#ifndef CUMULATIVE_ARGS_MAGIC
#define CUMULATIVE_ARGS_MAGIC ((void *) &targetm.calls)
#endif

inline CUMULATIVE_ARGS *
get_cumulative_args (cumulative_args_t arg)
{
#if CHECKING_P
  gcc_assert (arg.magic == CUMULATIVE_ARGS_MAGIC);
#endif /* CHECKING_P */
  return (CUMULATIVE_ARGS *) arg.p;
}

inline cumulative_args_t
pack_cumulative_args (CUMULATIVE_ARGS *arg)
{
  cumulative_args_t ret;

#if CHECKING_P
  ret.magic = CUMULATIVE_ARGS_MAGIC;
#endif /* CHECKING_P */
  ret.p = (void *) arg;
  return ret;
}
#endif /* GCC_TM_H */

#endif /* GCC_TARGET_H */
