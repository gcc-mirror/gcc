/* Default target hook functions.
   Copyright (C) 2003-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* The migration of target macros to target hooks works as follows:

   1. Create a target hook that uses the existing target macros to
      implement the same functionality.

   2. Convert all the MI files to use the hook instead of the macro.

   3. Repeat for a majority of the remaining target macros.  This will
      take some time.

   4. Tell target maintainers to start migrating.

   5. Eventually convert the backends to override the hook instead of
      defining the macros.  This will take some time too.

   6. TBD when, poison the macros.  Unmigrated targets will break at
      this point.

   Note that we expect steps 1-3 to be done by the people that
   understand what the MI does with each macro, and step 5 to be done
   by the target maintainers for their respective targets.

   Note that steps 1 and 2 don't have to be done together, but no
   target can override the new hook until step 2 is complete for it.

   Once the macros are poisoned, we will revert to the old migration
   rules - migrate the macro, callers, and targets all at once.  This
   comment can thus be removed at that point.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "rtl.h"
#include "tree.h"
#include "tree-ssa-alias.h"
#include "gimple-expr.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "profile-count.h"
#include "optabs.h"
#include "regs.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "varasm.h"
#include "flags.h"
#include "explow.h"
#include "calls.h"
#include "expr.h"
#include "output.h"
#include "common/common-target.h"
#include "reload.h"
#include "intl.h"
#include "opts.h"
#include "gimplify.h"
#include "predict.h"
#include "params.h"
#include "real.h"


bool
default_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			      rtx addr ATTRIBUTE_UNUSED,
			      bool strict ATTRIBUTE_UNUSED)
{
#ifdef GO_IF_LEGITIMATE_ADDRESS
  /* Defer to the old implementation using a goto.  */
  if (strict)
    return strict_memory_address_p (mode, addr);
  else
    return memory_address_p (mode, addr);
#else
  gcc_unreachable ();
#endif
}

void
default_external_libcall (rtx fun ATTRIBUTE_UNUSED)
{
#ifdef ASM_OUTPUT_EXTERNAL_LIBCALL
  ASM_OUTPUT_EXTERNAL_LIBCALL (asm_out_file, fun);
#endif
}

int
default_unspec_may_trap_p (const_rtx x, unsigned flags)
{
  int i;

  /* Any floating arithmetic may trap.  */
  if ((SCALAR_FLOAT_MODE_P (GET_MODE (x)) && flag_trapping_math))
    return 1;

  for (i = 0; i < XVECLEN (x, 0); ++i)
    {
      if (may_trap_p_1 (XVECEXP (x, 0, i), flags))
	return 1;
    }

  return 0;
}

machine_mode
default_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
			       machine_mode mode,
			       int *punsignedp ATTRIBUTE_UNUSED,
			       const_tree funtype ATTRIBUTE_UNUSED,
			       int for_return ATTRIBUTE_UNUSED)
{
  if (type != NULL_TREE && for_return == 2)
    return promote_mode (type, mode, punsignedp);
  return mode;
}

machine_mode
default_promote_function_mode_always_promote (const_tree type,
					      machine_mode mode,
					      int *punsignedp,
					      const_tree funtype ATTRIBUTE_UNUSED,
					      int for_return ATTRIBUTE_UNUSED)
{
  return promote_mode (type, mode, punsignedp);
}

machine_mode
default_cc_modes_compatible (machine_mode m1, machine_mode m2)
{
  if (m1 == m2)
    return m1;
  return VOIDmode;
}

bool
default_return_in_memory (const_tree type,
			  const_tree fntype ATTRIBUTE_UNUSED)
{
  return (TYPE_MODE (type) == BLKmode);
}

rtx
default_legitimize_address (rtx x, rtx orig_x ATTRIBUTE_UNUSED,
			    machine_mode mode ATTRIBUTE_UNUSED)
{
  return x;
}

bool
default_legitimize_address_displacement (rtx *disp ATTRIBUTE_UNUSED,
					 rtx *offset ATTRIBUTE_UNUSED,
					 machine_mode mode ATTRIBUTE_UNUSED)
{
  return false;
}

bool
default_const_not_ok_for_debug_p (rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    return true;
  return false;
}

rtx
default_expand_builtin_saveregs (void)
{
  error ("__builtin_saveregs not supported by this target");
  return const0_rtx;
}

void
default_setup_incoming_varargs (cumulative_args_t ca ATTRIBUTE_UNUSED,
				machine_mode mode ATTRIBUTE_UNUSED,
				tree type ATTRIBUTE_UNUSED,
				int *pretend_arg_size ATTRIBUTE_UNUSED,
				int second_time ATTRIBUTE_UNUSED)
{
}

/* The default implementation of TARGET_BUILTIN_SETJMP_FRAME_VALUE.  */

rtx
default_builtin_setjmp_frame_value (void)
{
  return virtual_stack_vars_rtx;
}

/* Generic hook that takes a CUMULATIVE_ARGS pointer and returns false.  */

bool
hook_bool_CUMULATIVE_ARGS_false (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  return false;
}

bool
default_pretend_outgoing_varargs_named (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  return (targetm.calls.setup_incoming_varargs
	  != default_setup_incoming_varargs);
}

scalar_int_mode
default_eh_return_filter_mode (void)
{
  return targetm.unwind_word_mode ();
}

scalar_int_mode
default_libgcc_cmp_return_mode (void)
{
  return word_mode;
}

scalar_int_mode
default_libgcc_shift_count_mode (void)
{
  return word_mode;
}

scalar_int_mode
default_unwind_word_mode (void)
{
  return word_mode;
}

/* The default implementation of TARGET_SHIFT_TRUNCATION_MASK.  */

unsigned HOST_WIDE_INT
default_shift_truncation_mask (machine_mode mode)
{
  return SHIFT_COUNT_TRUNCATED ? GET_MODE_UNIT_BITSIZE (mode) - 1 : 0;
}

/* The default implementation of TARGET_MIN_DIVISIONS_FOR_RECIP_MUL.  */

unsigned int
default_min_divisions_for_recip_mul (machine_mode mode ATTRIBUTE_UNUSED)
{
  return have_insn_for (DIV, mode) ? 3 : 2;
}

/* The default implementation of TARGET_MODE_REP_EXTENDED.  */

int
default_mode_rep_extended (scalar_int_mode, scalar_int_mode)
{
  return UNKNOWN;
}

/* Generic hook that takes a CUMULATIVE_ARGS pointer and returns true.  */

bool
hook_bool_CUMULATIVE_ARGS_true (cumulative_args_t a ATTRIBUTE_UNUSED)
{
  return true;
}

/* Return machine mode for non-standard suffix
   or VOIDmode if non-standard suffixes are unsupported.  */
machine_mode
default_mode_for_suffix (char suffix ATTRIBUTE_UNUSED)
{
  return VOIDmode;
}

/* The generic C++ ABI specifies this is a 64-bit value.  */
tree
default_cxx_guard_type (void)
{
  return long_long_integer_type_node;
}

/* Returns the size of the cookie to use when allocating an array
   whose elements have the indicated TYPE.  Assumes that it is already
   known that a cookie is needed.  */

tree
default_cxx_get_cookie_size (tree type)
{
  tree cookie_size;

  /* We need to allocate an additional max (sizeof (size_t), alignof
     (true_type)) bytes.  */
  tree sizetype_size;
  tree type_align;

  sizetype_size = size_in_bytes (sizetype);
  type_align = size_int (TYPE_ALIGN_UNIT (type));
  if (tree_int_cst_lt (type_align, sizetype_size))
    cookie_size = sizetype_size;
  else
    cookie_size = type_align;

  return cookie_size;
}

/* Return true if a parameter must be passed by reference.  This version
   of the TARGET_PASS_BY_REFERENCE hook uses just MUST_PASS_IN_STACK.  */

bool
hook_pass_by_reference_must_pass_in_stack (cumulative_args_t c ATTRIBUTE_UNUSED,
	machine_mode mode ATTRIBUTE_UNUSED, const_tree type ATTRIBUTE_UNUSED,
	bool named_arg ATTRIBUTE_UNUSED)
{
  return targetm.calls.must_pass_in_stack (mode, type);
}

/* Return true if a parameter follows callee copies conventions.  This
   version of the hook is true for all named arguments.  */

bool
hook_callee_copies_named (cumulative_args_t ca ATTRIBUTE_UNUSED,
			  machine_mode mode ATTRIBUTE_UNUSED,
			  const_tree type ATTRIBUTE_UNUSED, bool named)
{
  return named;
}

/* Emit to STREAM the assembler syntax for insn operand X.  */

void
default_print_operand (FILE *stream ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED,
		       int code ATTRIBUTE_UNUSED)
{
#ifdef PRINT_OPERAND
  PRINT_OPERAND (stream, x, code);
#else
  gcc_unreachable ();
#endif
}

/* Emit to STREAM the assembler syntax for an insn operand whose memory
   address is X.  */

void
default_print_operand_address (FILE *stream ATTRIBUTE_UNUSED,
			       machine_mode /*mode*/,
			       rtx x ATTRIBUTE_UNUSED)
{
#ifdef PRINT_OPERAND_ADDRESS
  PRINT_OPERAND_ADDRESS (stream, x);
#else
  gcc_unreachable ();
#endif
}

/* Return true if CODE is a valid punctuation character for the
   `print_operand' hook.  */

bool
default_print_operand_punct_valid_p (unsigned char code ATTRIBUTE_UNUSED)
{
#ifdef PRINT_OPERAND_PUNCT_VALID_P
  return PRINT_OPERAND_PUNCT_VALID_P (code);
#else
  return false;
#endif
}

/* The default implementation of TARGET_MANGLE_ASSEMBLER_NAME.  */
tree
default_mangle_assembler_name (const char *name ATTRIBUTE_UNUSED)
{
  const char *skipped = name + (*name == '*' ? 1 : 0);
  const char *stripped = targetm.strip_name_encoding (skipped);
  if (*name != '*' && user_label_prefix[0])
    stripped = ACONCAT ((user_label_prefix, stripped, NULL));
  return get_identifier (stripped);
}

/* True if MODE is valid for the target.  By "valid", we mean able to
   be manipulated in non-trivial ways.  In particular, this means all
   the arithmetic is supported.

   By default we guess this means that any C type is supported.  If
   we can't map the mode back to a type that would be available in C,
   then reject it.  Special case, here, is the double-word arithmetic
   supported by optabs.c.  */

bool
default_scalar_mode_supported_p (scalar_mode mode)
{
  int precision = GET_MODE_PRECISION (mode);

  switch (GET_MODE_CLASS (mode))
    {
    case MODE_PARTIAL_INT:
    case MODE_INT:
      if (precision == CHAR_TYPE_SIZE)
	return true;
      if (precision == SHORT_TYPE_SIZE)
	return true;
      if (precision == INT_TYPE_SIZE)
	return true;
      if (precision == LONG_TYPE_SIZE)
	return true;
      if (precision == LONG_LONG_TYPE_SIZE)
	return true;
      if (precision == 2 * BITS_PER_WORD)
	return true;
      return false;

    case MODE_FLOAT:
      if (precision == FLOAT_TYPE_SIZE)
	return true;
      if (precision == DOUBLE_TYPE_SIZE)
	return true;
      if (precision == LONG_DOUBLE_TYPE_SIZE)
	return true;
      return false;

    case MODE_DECIMAL_FLOAT:
    case MODE_FRACT:
    case MODE_UFRACT:
    case MODE_ACCUM:
    case MODE_UACCUM:
      return false;

    default:
      gcc_unreachable ();
    }
}

/* Return true if libgcc supports floating-point mode MODE (known to
   be supported as a scalar mode).  */

bool
default_libgcc_floating_mode_supported_p (scalar_float_mode mode)
{
  switch (mode)
    {
#ifdef HAVE_SFmode
    case E_SFmode:
#endif
#ifdef HAVE_DFmode
    case E_DFmode:
#endif
#ifdef HAVE_XFmode
    case E_XFmode:
#endif
#ifdef HAVE_TFmode
    case E_TFmode:
#endif
      return true;

    default:
      return false;
    }
}

/* Return the machine mode to use for the type _FloatN, if EXTENDED is
   false, or _FloatNx, if EXTENDED is true, or VOIDmode if not
   supported.  */
opt_scalar_float_mode
default_floatn_mode (int n, bool extended)
{
  if (extended)
    {
      opt_scalar_float_mode cand1, cand2;
      scalar_float_mode mode;
      switch (n)
	{
	case 32:
#ifdef HAVE_DFmode
	  cand1 = DFmode;
#endif
	  break;

	case 64:
#ifdef HAVE_XFmode
	  cand1 = XFmode;
#endif
#ifdef HAVE_TFmode
	  cand2 = TFmode;
#endif
	  break;

	case 128:
	  break;

	default:
	  /* Those are the only valid _FloatNx types.  */
	  gcc_unreachable ();
	}
      if (cand1.exists (&mode)
	  && REAL_MODE_FORMAT (mode)->ieee_bits > n
	  && targetm.scalar_mode_supported_p (mode)
	  && targetm.libgcc_floating_mode_supported_p (mode))
	return cand1;
      if (cand2.exists (&mode)
	  && REAL_MODE_FORMAT (mode)->ieee_bits > n
	  && targetm.scalar_mode_supported_p (mode)
	  && targetm.libgcc_floating_mode_supported_p (mode))
	return cand2;
    }
  else
    {
      opt_scalar_float_mode cand;
      scalar_float_mode mode;
      switch (n)
	{
	case 16:
	  /* Always enable _Float16 if we have basic support for the mode.
	     Targets can control the range and precision of operations on
	     the _Float16 type using TARGET_C_EXCESS_PRECISION.  */
#ifdef HAVE_HFmode
	  cand = HFmode;
#endif
	  break;

	case 32:
#ifdef HAVE_SFmode
	  cand = SFmode;
#endif
	  break;

	case 64:
#ifdef HAVE_DFmode
	  cand = DFmode;
#endif
	  break;

	case 128:
#ifdef HAVE_TFmode
	  cand = TFmode;
#endif
	  break;

	default:
	  break;
	}
      if (cand.exists (&mode)
	  && REAL_MODE_FORMAT (mode)->ieee_bits == n
	  && targetm.scalar_mode_supported_p (mode)
	  && targetm.libgcc_floating_mode_supported_p (mode))
	return cand;
    }
  return opt_scalar_float_mode ();
}

/* Make some target macros useable by target-independent code.  */
bool
targhook_words_big_endian (void)
{
  return !!WORDS_BIG_ENDIAN;
}

bool
targhook_float_words_big_endian (void)
{
  return !!FLOAT_WORDS_BIG_ENDIAN;
}

/* True if the target supports floating-point exceptions and rounding
   modes.  */

bool
default_float_exceptions_rounding_supported_p (void)
{
#ifdef HAVE_adddf3
  return HAVE_adddf3;
#else
  return false;
#endif
}

/* True if the target supports decimal floating point.  */

bool
default_decimal_float_supported_p (void)
{
  return ENABLE_DECIMAL_FLOAT;
}

/* True if the target supports fixed-point arithmetic.  */

bool
default_fixed_point_supported_p (void)
{
  return ENABLE_FIXED_POINT;
}

/* True if the target supports GNU indirect functions.  */

bool
default_has_ifunc_p (void)
{
  return HAVE_GNU_INDIRECT_FUNCTION;
}

/* NULL if INSN insn is valid within a low-overhead loop, otherwise returns
   an error message.

   This function checks whether a given INSN is valid within a low-overhead
   loop.  If INSN is invalid it returns the reason for that, otherwise it
   returns NULL. A called function may clobber any special registers required
   for low-overhead looping. Additionally, some targets (eg, PPC) use the count
   register for branch on table instructions. We reject the doloop pattern in
   these cases.  */

const char *
default_invalid_within_doloop (const rtx_insn *insn)
{
  if (CALL_P (insn))
    return "Function call in loop.";

  if (tablejump_p (insn, NULL, NULL) || computed_jump_p (insn))
    return "Computed branch in the loop.";

  return NULL;
}

/* Mapping of builtin functions to vectorized variants.  */

tree
default_builtin_vectorized_function (unsigned int, tree, tree)
{
  return NULL_TREE;
}

/* Mapping of target builtin functions to vectorized variants.  */

tree
default_builtin_md_vectorized_function (tree, tree, tree)
{
  return NULL_TREE;
}

/* Vectorized conversion.  */

tree
default_builtin_vectorized_conversion (unsigned int code ATTRIBUTE_UNUSED,
				       tree dest_type ATTRIBUTE_UNUSED,
				       tree src_type ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* Default vectorizer cost model values.  */

int
default_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
                                    tree vectype,
                                    int misalign ATTRIBUTE_UNUSED)
{
  switch (type_of_cost)
    {
      case scalar_stmt:
      case scalar_load:
      case scalar_store:
      case vector_stmt:
      case vector_load:
      case vector_store:
      case vec_to_scalar:
      case scalar_to_vec:
      case cond_branch_not_taken:
      case vec_perm:
      case vec_promote_demote:
        return 1;

      case unaligned_load:
      case unaligned_store:
        return 2;

      case cond_branch_taken:
        return 3;

      case vec_construct:
	return TYPE_VECTOR_SUBPARTS (vectype) - 1;

      default:
        gcc_unreachable ();
    }
}

/* Reciprocal.  */

tree
default_builtin_reciprocal (tree)
{
  return NULL_TREE;
}

bool
hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false (
	cumulative_args_t ca ATTRIBUTE_UNUSED,
	machine_mode mode ATTRIBUTE_UNUSED,
	const_tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
{
  return false;
}

bool
hook_bool_CUMULATIVE_ARGS_mode_tree_bool_true (
	cumulative_args_t ca ATTRIBUTE_UNUSED,
	machine_mode mode ATTRIBUTE_UNUSED,
	const_tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
{
  return true;
}

int
hook_int_CUMULATIVE_ARGS_mode_tree_bool_0 (
	cumulative_args_t ca ATTRIBUTE_UNUSED,
	machine_mode mode ATTRIBUTE_UNUSED,
	tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
{
  return 0;
}

void
default_function_arg_advance (cumulative_args_t ca ATTRIBUTE_UNUSED,
			      machine_mode mode ATTRIBUTE_UNUSED,
			      const_tree type ATTRIBUTE_UNUSED,
			      bool named ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Default implementation of TARGET_FUNCTION_ARG_OFFSET.  */

HOST_WIDE_INT
default_function_arg_offset (machine_mode, const_tree)
{
  return 0;
}

/* Default implementation of TARGET_FUNCTION_ARG_PADDING: usually pad
   upward, but pad short args downward on big-endian machines.  */

pad_direction
default_function_arg_padding (machine_mode mode, const_tree type)
{
  if (!BYTES_BIG_ENDIAN)
    return PAD_UPWARD;

  unsigned HOST_WIDE_INT size;
  if (mode == BLKmode)
    {
      if (!type || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	return PAD_UPWARD;
      size = int_size_in_bytes (type);
    }
  else
    size = GET_MODE_SIZE (mode);

  if (size < (PARM_BOUNDARY / BITS_PER_UNIT))
    return PAD_DOWNWARD;

  return PAD_UPWARD;
}

rtx
default_function_arg (cumulative_args_t ca ATTRIBUTE_UNUSED,
		      machine_mode mode ATTRIBUTE_UNUSED,
		      const_tree type ATTRIBUTE_UNUSED,
		      bool named ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

rtx
default_function_incoming_arg (cumulative_args_t ca ATTRIBUTE_UNUSED,
			       machine_mode mode ATTRIBUTE_UNUSED,
			       const_tree type ATTRIBUTE_UNUSED,
			       bool named ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

unsigned int
default_function_arg_boundary (machine_mode mode ATTRIBUTE_UNUSED,
			       const_tree type ATTRIBUTE_UNUSED)
{
  return PARM_BOUNDARY;
}

unsigned int
default_function_arg_round_boundary (machine_mode mode ATTRIBUTE_UNUSED,
				     const_tree type ATTRIBUTE_UNUSED)
{
  return PARM_BOUNDARY;
}

void
hook_void_bitmap (bitmap regs ATTRIBUTE_UNUSED)
{
}

const char *
hook_invalid_arg_for_unprototyped_fn (
	const_tree typelist ATTRIBUTE_UNUSED,
	const_tree funcdecl ATTRIBUTE_UNUSED,
	const_tree val ATTRIBUTE_UNUSED)
{
  return NULL;
}

/* Initialize the stack protection decls.  */

/* Stack protection related decls living in libgcc.  */
static GTY(()) tree stack_chk_guard_decl;

tree
default_stack_protect_guard (void)
{
  tree t = stack_chk_guard_decl;

  if (t == NULL)
    {
      rtx x;

      t = build_decl (UNKNOWN_LOCATION,
		      VAR_DECL, get_identifier ("__stack_chk_guard"),
		      ptr_type_node);
      TREE_STATIC (t) = 1;
      TREE_PUBLIC (t) = 1;
      DECL_EXTERNAL (t) = 1;
      TREE_USED (t) = 1;
      TREE_THIS_VOLATILE (t) = 1;
      DECL_ARTIFICIAL (t) = 1;
      DECL_IGNORED_P (t) = 1;

      /* Do not share RTL as the declaration is visible outside of
	 current function.  */
      x = DECL_RTL (t);
      RTX_FLAG (x, used) = 1;

      stack_chk_guard_decl = t;
    }

  return t;
}

static GTY(()) tree stack_chk_fail_decl;

tree
default_external_stack_protect_fail (void)
{
  tree t = stack_chk_fail_decl;

  if (t == NULL_TREE)
    {
      t = build_function_type_list (void_type_node, NULL_TREE);
      t = build_decl (UNKNOWN_LOCATION,
		      FUNCTION_DECL, get_identifier ("__stack_chk_fail"), t);
      TREE_STATIC (t) = 1;
      TREE_PUBLIC (t) = 1;
      DECL_EXTERNAL (t) = 1;
      TREE_USED (t) = 1;
      TREE_THIS_VOLATILE (t) = 1;
      TREE_NOTHROW (t) = 1;
      DECL_ARTIFICIAL (t) = 1;
      DECL_IGNORED_P (t) = 1;
      DECL_VISIBILITY (t) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (t) = 1;

      stack_chk_fail_decl = t;
    }

  return build_call_expr (t, 0);
}

tree
default_hidden_stack_protect_fail (void)
{
#ifndef HAVE_GAS_HIDDEN
  return default_external_stack_protect_fail ();
#else
  tree t = stack_chk_fail_decl;

  if (!flag_pic)
    return default_external_stack_protect_fail ();

  if (t == NULL_TREE)
    {
      t = build_function_type_list (void_type_node, NULL_TREE);
      t = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
		      get_identifier ("__stack_chk_fail_local"), t);
      TREE_STATIC (t) = 1;
      TREE_PUBLIC (t) = 1;
      DECL_EXTERNAL (t) = 1;
      TREE_USED (t) = 1;
      TREE_THIS_VOLATILE (t) = 1;
      TREE_NOTHROW (t) = 1;
      DECL_ARTIFICIAL (t) = 1;
      DECL_IGNORED_P (t) = 1;
      DECL_VISIBILITY_SPECIFIED (t) = 1;
      DECL_VISIBILITY (t) = VISIBILITY_HIDDEN;

      stack_chk_fail_decl = t;
    }

  return build_call_expr (t, 0);
#endif
}

bool
hook_bool_const_rtx_commutative_p (const_rtx x,
				   int outer_code ATTRIBUTE_UNUSED)
{
  return COMMUTATIVE_P (x);
}

rtx
default_function_value (const_tree ret_type ATTRIBUTE_UNUSED,
			const_tree fn_decl_or_type,
			bool outgoing ATTRIBUTE_UNUSED)
{
  /* The old interface doesn't handle receiving the function type.  */
  if (fn_decl_or_type
      && !DECL_P (fn_decl_or_type))
    fn_decl_or_type = NULL;

#ifdef FUNCTION_VALUE
  return FUNCTION_VALUE (ret_type, fn_decl_or_type);
#else
  gcc_unreachable ();
#endif
}

rtx
default_libcall_value (machine_mode mode ATTRIBUTE_UNUSED,
		       const_rtx fun ATTRIBUTE_UNUSED)
{
#ifdef LIBCALL_VALUE
  return LIBCALL_VALUE (mode);
#else
  gcc_unreachable ();
#endif
}

/* The default hook for TARGET_FUNCTION_VALUE_REGNO_P.  */

bool
default_function_value_regno_p (const unsigned int regno ATTRIBUTE_UNUSED)
{
#ifdef FUNCTION_VALUE_REGNO_P
  return FUNCTION_VALUE_REGNO_P (regno);
#else
  gcc_unreachable ();
#endif
}

rtx
default_internal_arg_pointer (void)
{
  /* If the reg that the virtual arg pointer will be translated into is
     not a fixed reg or is the stack pointer, make a copy of the virtual
     arg pointer, and address parms via the copy.  The frame pointer is
     considered fixed even though it is not marked as such.  */
  if ((ARG_POINTER_REGNUM == STACK_POINTER_REGNUM
       || ! (fixed_regs[ARG_POINTER_REGNUM]
	     || ARG_POINTER_REGNUM == FRAME_POINTER_REGNUM)))
    return copy_to_reg (virtual_incoming_args_rtx);
  else
    return virtual_incoming_args_rtx;
}

rtx
default_static_chain (const_tree ARG_UNUSED (fndecl_or_type), bool incoming_p)
{
  if (incoming_p)
    {
#ifdef STATIC_CHAIN_INCOMING_REGNUM
      return gen_rtx_REG (Pmode, STATIC_CHAIN_INCOMING_REGNUM);
#endif
    }

#ifdef STATIC_CHAIN_REGNUM
  return gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);
#endif

  {
    static bool issued_error;
    if (!issued_error)
      {
	issued_error = true;
	sorry ("nested functions not supported on this target");
      }

    /* It really doesn't matter what we return here, so long at it
       doesn't cause the rest of the compiler to crash.  */
    return gen_rtx_MEM (Pmode, stack_pointer_rtx);
  }
}

void
default_trampoline_init (rtx ARG_UNUSED (m_tramp), tree ARG_UNUSED (t_func),
			 rtx ARG_UNUSED (r_chain))
{
  sorry ("nested function trampolines not supported on this target");
}

int
default_return_pops_args (tree fundecl ATTRIBUTE_UNUSED,
			  tree funtype ATTRIBUTE_UNUSED,
			  int size ATTRIBUTE_UNUSED)
{
  return 0;
}

reg_class_t
default_branch_target_register_class (void)
{
  return NO_REGS;
}

reg_class_t
default_ira_change_pseudo_allocno_class (int regno ATTRIBUTE_UNUSED,
					 reg_class_t cl,
					 reg_class_t best_cl ATTRIBUTE_UNUSED)
{
  return cl;
}

extern bool
default_lra_p (void)
{
  return true;
}

int
default_register_priority (int hard_regno ATTRIBUTE_UNUSED)
{
  return 0;
}

extern bool
default_register_usage_leveling_p (void)
{
  return false;
}

extern bool
default_different_addr_displacement_p (void)
{
  return false;
}

reg_class_t
default_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED,
			  reg_class_t reload_class_i ATTRIBUTE_UNUSED,
			  machine_mode reload_mode ATTRIBUTE_UNUSED,
			  secondary_reload_info *sri)
{
  enum reg_class rclass = NO_REGS;
  enum reg_class reload_class = (enum reg_class) reload_class_i;

  if (sri->prev_sri && sri->prev_sri->t_icode != CODE_FOR_nothing)
    {
      sri->icode = sri->prev_sri->t_icode;
      return NO_REGS;
    }
#ifdef SECONDARY_INPUT_RELOAD_CLASS
  if (in_p)
    rclass = SECONDARY_INPUT_RELOAD_CLASS (reload_class, reload_mode, x);
#endif
#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
  if (! in_p)
    rclass = SECONDARY_OUTPUT_RELOAD_CLASS (reload_class, reload_mode, x);
#endif
  if (rclass != NO_REGS)
    {
      enum insn_code icode
	= direct_optab_handler (in_p ? reload_in_optab : reload_out_optab,
				reload_mode);

      if (icode != CODE_FOR_nothing
	  && !insn_operand_matches (icode, in_p, x))
	icode = CODE_FOR_nothing;
      else if (icode != CODE_FOR_nothing)
	{
	  const char *insn_constraint, *scratch_constraint;
	  enum reg_class insn_class, scratch_class;

	  gcc_assert (insn_data[(int) icode].n_operands == 3);
	  insn_constraint = insn_data[(int) icode].operand[!in_p].constraint;
	  if (!*insn_constraint)
	    insn_class = ALL_REGS;
	  else
	    {
	      if (in_p)
		{
		  gcc_assert (*insn_constraint == '=');
		  insn_constraint++;
		}
	      insn_class = (reg_class_for_constraint
			    (lookup_constraint (insn_constraint)));
	      gcc_assert (insn_class != NO_REGS);
	    }

	  scratch_constraint = insn_data[(int) icode].operand[2].constraint;
	  /* The scratch register's constraint must start with "=&",
	     except for an input reload, where only "=" is necessary,
	     and where it might be beneficial to re-use registers from
	     the input.  */
	  gcc_assert (scratch_constraint[0] == '='
		      && (in_p || scratch_constraint[1] == '&'));
	  scratch_constraint++;
	  if (*scratch_constraint == '&')
	    scratch_constraint++;
	  scratch_class = (reg_class_for_constraint
			   (lookup_constraint (scratch_constraint)));

	  if (reg_class_subset_p (reload_class, insn_class))
	    {
	      gcc_assert (scratch_class == rclass);
	      rclass = NO_REGS;
	    }
	  else
	    rclass = insn_class;

        }
      if (rclass == NO_REGS)
	sri->icode = icode;
      else
	sri->t_icode = icode;
    }
  return rclass;
}

/* The default implementation of TARGET_SECONDARY_MEMORY_NEEDED_MODE.  */

machine_mode
default_secondary_memory_needed_mode (machine_mode mode)
{
  if (!targetm.lra_p ()
      && GET_MODE_BITSIZE (mode) < BITS_PER_WORD
      && INTEGRAL_MODE_P (mode))
    return mode_for_size (BITS_PER_WORD, GET_MODE_CLASS (mode), 0).require ();
  return mode;
}

/* By default, if flag_pic is true, then neither local nor global relocs
   should be placed in readonly memory.  */

int
default_reloc_rw_mask (void)
{
  return flag_pic ? 3 : 0;
}

/* By default, do no modification. */
tree default_mangle_decl_assembler_name (tree decl ATTRIBUTE_UNUSED,
					 tree id)
{
   return id;
}

/* The default implementation of TARGET_STATIC_RTX_ALIGNMENT.  */

HOST_WIDE_INT
default_static_rtx_alignment (machine_mode mode)
{
  return GET_MODE_ALIGNMENT (mode);
}

/* The default implementation of TARGET_CONSTANT_ALIGNMENT.  */

HOST_WIDE_INT
default_constant_alignment (const_tree, HOST_WIDE_INT align)
{
  return align;
}

/* An implementation of TARGET_CONSTANT_ALIGNMENT that aligns strings
   to at least BITS_PER_WORD but otherwise makes no changes.  */

HOST_WIDE_INT
constant_alignment_word_strings (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST)
    return MAX (align, BITS_PER_WORD);
  return align;
}

/* Default to natural alignment for vector types.  */
HOST_WIDE_INT
default_vector_alignment (const_tree type)
{
  HOST_WIDE_INT align = tree_to_shwi (TYPE_SIZE (type));
  if (align > MAX_OFILE_ALIGNMENT)
    align = MAX_OFILE_ALIGNMENT;
  return align;
}

/* The default implementation of
   TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT.  */

HOST_WIDE_INT
default_preferred_vector_alignment (const_tree type)
{
  return TYPE_ALIGN (type);
}

/* By default assume vectors of element TYPE require a multiple of the natural
   alignment of TYPE.  TYPE is naturally aligned if IS_PACKED is false.  */
bool
default_builtin_vector_alignment_reachable (const_tree /*type*/, bool is_packed)
{
  return ! is_packed;
}

/* By default, assume that a target supports any factor of misalignment
   memory access if it supports movmisalign patten.
   is_packed is true if the memory access is defined in a packed struct.  */
bool
default_builtin_support_vector_misalignment (machine_mode mode,
					     const_tree type
					     ATTRIBUTE_UNUSED,
					     int misalignment
					     ATTRIBUTE_UNUSED,
					     bool is_packed
					     ATTRIBUTE_UNUSED)
{
  if (optab_handler (movmisalign_optab, mode) != CODE_FOR_nothing)
    return true;
  return false;
}

/* By default, only attempt to parallelize bitwise operations, and
   possibly adds/subtracts using bit-twiddling.  */

machine_mode
default_preferred_simd_mode (scalar_mode)
{
  return word_mode;
}

/* By default only the size derived from the preferred vector mode
   is tried.  */

unsigned int
default_autovectorize_vector_sizes (void)
{
  return 0;
}

/* By defaults a vector of integers is used as a mask.  */

opt_machine_mode
default_get_mask_mode (unsigned nunits, unsigned vector_size)
{
  unsigned elem_size = vector_size / nunits;
  scalar_int_mode elem_mode
    = smallest_int_mode_for_size (elem_size * BITS_PER_UNIT);
  machine_mode vector_mode;

  gcc_assert (elem_size * nunits == vector_size);

  if (mode_for_vector (elem_mode, nunits).exists (&vector_mode)
      && VECTOR_MODE_P (vector_mode)
      && targetm.vector_mode_supported_p (vector_mode))
    return vector_mode;

  return opt_machine_mode ();
}

/* By default, the cost model accumulates three separate costs (prologue,
   loop body, and epilogue) for a vectorized loop or block.  So allocate an
   array of three unsigned ints, set it to zero, and return its address.  */

void *
default_init_cost (struct loop *loop_info ATTRIBUTE_UNUSED)
{
  unsigned *cost = XNEWVEC (unsigned, 3);
  cost[vect_prologue] = cost[vect_body] = cost[vect_epilogue] = 0;
  return cost;
}

/* By default, the cost model looks up the cost of the given statement
   kind and mode, multiplies it by the occurrence count, accumulates
   it into the cost specified by WHERE, and returns the cost added.  */

unsigned
default_add_stmt_cost (void *data, int count, enum vect_cost_for_stmt kind,
		       struct _stmt_vec_info *stmt_info, int misalign,
		       enum vect_cost_model_location where)
{
  unsigned *cost = (unsigned *) data;
  unsigned retval = 0;

  tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
  int stmt_cost = targetm.vectorize.builtin_vectorization_cost (kind, vectype,
								misalign);
   /* Statements in an inner loop relative to the loop being
      vectorized are weighted more heavily.  The value here is
      arbitrary and could potentially be improved with analysis.  */
  if (where == vect_body && stmt_info && stmt_in_inner_loop_p (stmt_info))
    count *= 50;  /* FIXME.  */

  retval = (unsigned) (count * stmt_cost);
  cost[where] += retval;

  return retval;
}

/* By default, the cost model just returns the accumulated costs.  */

void
default_finish_cost (void *data, unsigned *prologue_cost,
		     unsigned *body_cost, unsigned *epilogue_cost)
{
  unsigned *cost = (unsigned *) data;
  *prologue_cost = cost[vect_prologue];
  *body_cost     = cost[vect_body];
  *epilogue_cost = cost[vect_epilogue];
}

/* Free the cost data.  */

void
default_destroy_cost_data (void *data)
{
  free (data);
}

/* Determine whether or not a pointer mode is valid. Assume defaults
   of ptr_mode or Pmode - can be overridden.  */
bool
default_valid_pointer_mode (scalar_int_mode mode)
{
  return (mode == ptr_mode || mode == Pmode);
}

/* Determine whether the memory reference specified by REF may alias
   the C libraries errno location.  */
bool
default_ref_may_alias_errno (ao_ref *ref)
{
  tree base = ao_ref_base (ref);
  /* The default implementation assumes the errno location is
     a declaration of type int or is always accessed via a
     pointer to int.  We assume that accesses to errno are
     not deliberately obfuscated (even in conforming ways).  */
  if (TYPE_UNSIGNED (TREE_TYPE (base))
      || TYPE_MODE (TREE_TYPE (base)) != TYPE_MODE (integer_type_node))
    return false;
  /* The default implementation assumes an errno location
     declaration is never defined in the current compilation unit.  */
  if (DECL_P (base)
      && !TREE_STATIC (base))
    return true;
  else if (TREE_CODE (base) == MEM_REF
	   && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
    {
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (TREE_OPERAND (base, 0));
      return !pi || pi->pt.anything || pi->pt.nonlocal;
    }
  return false;
}

/* Return the mode for a pointer to a given ADDRSPACE,
   defaulting to ptr_mode for all address spaces.  */

scalar_int_mode
default_addr_space_pointer_mode (addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  return ptr_mode;
}

/* Return the mode for an address in a given ADDRSPACE,
   defaulting to Pmode for all address spaces.  */

scalar_int_mode
default_addr_space_address_mode (addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  return Pmode;
}

/* Named address space version of valid_pointer_mode.
   To match the above, the same modes apply to all address spaces.  */

bool
default_addr_space_valid_pointer_mode (scalar_int_mode mode,
				       addr_space_t as ATTRIBUTE_UNUSED)
{
  return targetm.valid_pointer_mode (mode);
}

/* Some places still assume that all pointer or address modes are the
   standard Pmode and ptr_mode.  These optimizations become invalid if
   the target actually supports multiple different modes.  For now,
   we disable such optimizations on such targets, using this function.  */

bool
target_default_pointer_address_modes_p (void)
{
  if (targetm.addr_space.address_mode != default_addr_space_address_mode)
    return false;
  if (targetm.addr_space.pointer_mode != default_addr_space_pointer_mode)
    return false;

  return true;
}

/* Named address space version of legitimate_address_p.
   By default, all address spaces have the same form.  */

bool
default_addr_space_legitimate_address_p (machine_mode mode, rtx mem,
					 bool strict,
					 addr_space_t as ATTRIBUTE_UNUSED)
{
  return targetm.legitimate_address_p (mode, mem, strict);
}

/* Named address space version of LEGITIMIZE_ADDRESS.
   By default, all address spaces have the same form.  */

rtx
default_addr_space_legitimize_address (rtx x, rtx oldx, machine_mode mode,
				       addr_space_t as ATTRIBUTE_UNUSED)
{
  return targetm.legitimize_address (x, oldx, mode);
}

/* The default hook for determining if one named address space is a subset of
   another and to return which address space to use as the common address
   space.  */

bool
default_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  return (subset == superset);
}

/* The default hook for determining if 0 within a named address
   space is a valid address.  */

bool
default_addr_space_zero_address_valid (addr_space_t as ATTRIBUTE_UNUSED)
{
  return false;
}

/* The default hook for debugging the address space is to return the
   address space number to indicate DW_AT_address_class.  */
int
default_addr_space_debug (addr_space_t as)
{
  return as;
}

/* The default hook implementation for TARGET_ADDR_SPACE_DIAGNOSE_USAGE.
   Don't complain about any address space.  */

void
default_addr_space_diagnose_usage (addr_space_t, location_t)
{
}
	 

/* The default hook for TARGET_ADDR_SPACE_CONVERT. This hook should never be
   called for targets with only a generic address space.  */

rtx
default_addr_space_convert (rtx op ATTRIBUTE_UNUSED,
			    tree from_type ATTRIBUTE_UNUSED,
			    tree to_type ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* The defualt implementation of TARGET_HARD_REGNO_NREGS.  */

unsigned int
default_hard_regno_nregs (unsigned int, machine_mode mode)
{
  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}

bool
default_hard_regno_scratch_ok (unsigned int regno ATTRIBUTE_UNUSED)
{
  return true;
}

/* The default implementation of TARGET_MODE_DEPENDENT_ADDRESS_P.  */

bool
default_mode_dependent_address_p (const_rtx addr ATTRIBUTE_UNUSED,
				  addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  return false;
}

bool
default_target_option_valid_attribute_p (tree ARG_UNUSED (fndecl),
					 tree ARG_UNUSED (name),
					 tree ARG_UNUSED (args),
					 int ARG_UNUSED (flags))
{
  warning (OPT_Wattributes,
	   "target attribute is not supported on this machine");

  return false;
}

bool
default_target_option_pragma_parse (tree ARG_UNUSED (args),
				    tree ARG_UNUSED (pop_target))
{
  /* If args is NULL the caller is handle_pragma_pop_options ().  In that case,
     emit no warning because "#pragma GCC pop_target" is valid on targets that
     do not have the "target" pragma.  */
  if (args)
    warning (OPT_Wpragmas,
	     "#pragma GCC target is not supported for this machine");

  return false;
}

bool
default_target_can_inline_p (tree caller, tree callee)
{
  tree callee_opts = DECL_FUNCTION_SPECIFIC_TARGET (callee);
  tree caller_opts = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  if (! callee_opts)
    callee_opts = target_option_default_node;
  if (! caller_opts)
    caller_opts = target_option_default_node;

  /* If both caller and callee have attributes, assume that if the
     pointer is different, the two functions have different target
     options since build_target_option_node uses a hash table for the
     options.  */
  return callee_opts == caller_opts;
}

/* If the machine does not have a case insn that compares the bounds,
   this means extra overhead for dispatch tables, which raises the
   threshold for using them.  */

unsigned int
default_case_values_threshold (void)
{
  return (targetm.have_casesi () ? 4 : 5);
}

bool
default_have_conditional_execution (void)
{
  return HAVE_conditional_execution;
}

/* By default we assume that c99 functions are present at the runtime,
   but sincos is not.  */
bool
default_libc_has_function (enum function_class fn_class)
{
  if (fn_class == function_c94
      || fn_class == function_c99_misc
      || fn_class == function_c99_math_complex)
    return true;

  return false;
}

bool
gnu_libc_has_function (enum function_class fn_class ATTRIBUTE_UNUSED)
{
  return true;
}

bool
no_c99_libc_has_function (enum function_class fn_class ATTRIBUTE_UNUSED)
{
  return false;
}

tree
default_builtin_tm_load_store (tree ARG_UNUSED (type))
{
  return NULL_TREE;
}

/* Compute cost of moving registers to/from memory.  */

int
default_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			  reg_class_t rclass ATTRIBUTE_UNUSED,
			  bool in ATTRIBUTE_UNUSED)
{
#ifndef MEMORY_MOVE_COST
    return (4 + memory_move_secondary_cost (mode, (enum reg_class) rclass, in));
#else
    return MEMORY_MOVE_COST (mode, (enum reg_class) rclass, in);
#endif
}

/* Compute cost of moving data from a register of class FROM to one of
   TO, using MODE.  */

int
default_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
                            reg_class_t from ATTRIBUTE_UNUSED,
                            reg_class_t to ATTRIBUTE_UNUSED)
{
#ifndef REGISTER_MOVE_COST
  return 2;
#else
  return REGISTER_MOVE_COST (mode, (enum reg_class) from, (enum reg_class) to);
#endif
}

/* The default implementation of TARGET_SLOW_UNALIGNED_ACCESS.  */

bool
default_slow_unaligned_access (machine_mode, unsigned int)
{
  return STRICT_ALIGNMENT;
}

/* For hooks which use the MOVE_RATIO macro, this gives the legacy default
   behavior.  SPEED_P is true if we are compiling for speed.  */

unsigned int
get_move_ratio (bool speed_p ATTRIBUTE_UNUSED)
{
  unsigned int move_ratio;
#ifdef MOVE_RATIO
  move_ratio = (unsigned int) MOVE_RATIO (speed_p);
#else
#if defined (HAVE_movmemqi) || defined (HAVE_movmemhi) || defined (HAVE_movmemsi) || defined (HAVE_movmemdi) || defined (HAVE_movmemti)
  move_ratio = 2;
#else /* No movmem patterns, pick a default.  */
  move_ratio = ((speed_p) ? 15 : 3);
#endif
#endif
  return move_ratio;
}

/* Return TRUE if the move_by_pieces/set_by_pieces infrastructure should be
   used; return FALSE if the movmem/setmem optab should be expanded, or
   a call to memcpy emitted.  */

bool
default_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
					unsigned int alignment,
					enum by_pieces_operation op,
					bool speed_p)
{
  unsigned int max_size = 0;
  unsigned int ratio = 0;

  switch (op)
    {
    case CLEAR_BY_PIECES:
      max_size = STORE_MAX_PIECES;
      ratio = CLEAR_RATIO (speed_p);
      break;
    case MOVE_BY_PIECES:
      max_size = MOVE_MAX_PIECES;
      ratio = get_move_ratio (speed_p);
      break;
    case SET_BY_PIECES:
      max_size = STORE_MAX_PIECES;
      ratio = SET_RATIO (speed_p);
      break;
    case STORE_BY_PIECES:
      max_size = STORE_MAX_PIECES;
      ratio = get_move_ratio (speed_p);
      break;
    case COMPARE_BY_PIECES:
      max_size = COMPARE_MAX_PIECES;
      /* Pick a likely default, just as in get_move_ratio.  */
      ratio = speed_p ? 15 : 3;
      break;
    }

  return by_pieces_ninsns (size, alignment, max_size + 1, op) < ratio;
}

/* This hook controls code generation for expanding a memcmp operation by
   pieces.  Return 1 for the normal pattern of compare/jump after each pair
   of loads, or a higher number to reduce the number of branches.  */

int
default_compare_by_pieces_branch_ratio (machine_mode)
{
  return 1;
}

/* Write PATCH_AREA_SIZE NOPs into the asm outfile FILE around a function
   entry.  If RECORD_P is true and the target supports named sections,
   the location of the NOPs will be recorded in a special object section
   called "__patchable_function_entries".  This routine may be called
   twice per function to put NOPs before and after the function
   entry.  */

void
default_print_patchable_function_entry (FILE *file,
					unsigned HOST_WIDE_INT patch_area_size,
					bool record_p)
{
  const char *nop_templ = 0;
  int code_num;
  rtx_insn *my_nop = make_insn_raw (gen_nop ());

  /* We use the template alone, relying on the (currently sane) assumption
     that the NOP template does not have variable operands.  */
  code_num = recog_memoized (my_nop);
  nop_templ = get_insn_template (code_num, my_nop);

  if (record_p && targetm_common.have_named_sections)
    {
      char buf[256];
      static int patch_area_number;
      section *previous_section = in_section;

      patch_area_number++;
      ASM_GENERATE_INTERNAL_LABEL (buf, "LPFE", patch_area_number);

      switch_to_section (get_section ("__patchable_function_entries",
				      0, NULL));
      fputs (integer_asm_op (POINTER_SIZE_UNITS, false), file);
      assemble_name_raw (file, buf);
      fputc ('\n', file);

      switch_to_section (previous_section);
      ASM_OUTPUT_LABEL (file, buf);
    }

  unsigned i;
  for (i = 0; i < patch_area_size; ++i)
    fprintf (file, "\t%s\n", nop_templ);
}

bool
default_profile_before_prologue (void)
{
#ifdef PROFILE_BEFORE_PROLOGUE
  return true;
#else
  return false;
#endif
}

/* The default implementation of TARGET_PREFERRED_RELOAD_CLASS.  */

reg_class_t
default_preferred_reload_class (rtx x ATTRIBUTE_UNUSED,
			        reg_class_t rclass)
{
#ifdef PREFERRED_RELOAD_CLASS 
  return (reg_class_t) PREFERRED_RELOAD_CLASS (x, (enum reg_class) rclass);
#else
  return rclass;
#endif
}

/* The default implementation of TARGET_OUTPUT_PREFERRED_RELOAD_CLASS.  */

reg_class_t
default_preferred_output_reload_class (rtx x ATTRIBUTE_UNUSED,
				       reg_class_t rclass)
{
  return rclass;
}

/* The default implementation of TARGET_PREFERRED_RENAME_CLASS.  */
reg_class_t
default_preferred_rename_class (reg_class_t rclass ATTRIBUTE_UNUSED)
{
  return NO_REGS;
}

/* The default implementation of TARGET_CLASS_LIKELY_SPILLED_P.  */

bool
default_class_likely_spilled_p (reg_class_t rclass)
{
  return (reg_class_size[(int) rclass] == 1);
}

/* The default implementation of TARGET_CLASS_MAX_NREGS.  */

unsigned char
default_class_max_nregs (reg_class_t rclass ATTRIBUTE_UNUSED,
			 machine_mode mode ATTRIBUTE_UNUSED)
{
#ifdef CLASS_MAX_NREGS
  return (unsigned char) CLASS_MAX_NREGS ((enum reg_class) rclass, mode);
#else
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
#endif
}

/* Determine the debugging unwind mechanism for the target.  */

enum unwind_info_type
default_debug_unwind_info (void)
{
  /* If the target wants to force the use of dwarf2 unwind info, let it.  */
  /* ??? Change all users to the hook, then poison this.  */
#ifdef DWARF2_FRAME_INFO
  if (DWARF2_FRAME_INFO)
    return UI_DWARF2;
#endif

  /* Otherwise, only turn it on if dwarf2 debugging is enabled.  */
#ifdef DWARF2_DEBUGGING_INFO
  if (write_symbols == DWARF2_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
    return UI_DWARF2;
#endif

  return UI_NONE;
}

/* Determine the correct mode for a Dwarf frame register that represents
   register REGNO.  */

machine_mode
default_dwarf_frame_reg_mode (int regno)
{
  machine_mode save_mode = reg_raw_mode[regno];

  if (targetm.hard_regno_call_part_clobbered (regno, save_mode))
    save_mode = choose_hard_reg_mode (regno, 1, true);
  return save_mode;
}

/* To be used by targets where reg_raw_mode doesn't return the right
   mode for registers used in apply_builtin_return and apply_builtin_arg.  */

machine_mode
default_get_reg_raw_mode (int regno)
{
  return reg_raw_mode[regno];
}

/* Return true if a leaf function should stay leaf even with profiling
   enabled.  */

bool
default_keep_leaf_when_profiled ()
{
  return false;
}

/* Return true if the state of option OPTION should be stored in PCH files
   and checked by default_pch_valid_p.  Store the option's current state
   in STATE if so.  */

static inline bool
option_affects_pch_p (int option, struct cl_option_state *state)
{
  if ((cl_options[option].flags & CL_TARGET) == 0)
    return false;
  if ((cl_options[option].flags & CL_PCH_IGNORE) != 0)
    return false;
  if (option_flag_var (option, &global_options) == &target_flags)
    if (targetm.check_pch_target_flags)
      return false;
  return get_option_state (&global_options, option, state);
}

/* Default version of get_pch_validity.
   By default, every flag difference is fatal; that will be mostly right for
   most targets, but completely right for very few.  */

void *
default_get_pch_validity (size_t *sz)
{
  struct cl_option_state state;
  size_t i;
  char *result, *r;

  *sz = 2;
  if (targetm.check_pch_target_flags)
    *sz += sizeof (target_flags);
  for (i = 0; i < cl_options_count; i++)
    if (option_affects_pch_p (i, &state))
      *sz += state.size;

  result = r = XNEWVEC (char, *sz);
  r[0] = flag_pic;
  r[1] = flag_pie;
  r += 2;
  if (targetm.check_pch_target_flags)
    {
      memcpy (r, &target_flags, sizeof (target_flags));
      r += sizeof (target_flags);
    }

  for (i = 0; i < cl_options_count; i++)
    if (option_affects_pch_p (i, &state))
      {
	memcpy (r, state.data, state.size);
	r += state.size;
      }

  return result;
}

/* Return a message which says that a PCH file was created with a different
   setting of OPTION.  */

static const char *
pch_option_mismatch (const char *option)
{
  return xasprintf (_("created and used with differing settings of '%s'"),
		    option);
}

/* Default version of pch_valid_p.  */

const char *
default_pch_valid_p (const void *data_p, size_t len)
{
  struct cl_option_state state;
  const char *data = (const char *)data_p;
  size_t i;

  /* -fpic and -fpie also usually make a PCH invalid.  */
  if (data[0] != flag_pic)
    return _("created and used with different settings of -fpic");
  if (data[1] != flag_pie)
    return _("created and used with different settings of -fpie");
  data += 2;

  /* Check target_flags.  */
  if (targetm.check_pch_target_flags)
    {
      int tf;
      const char *r;

      memcpy (&tf, data, sizeof (target_flags));
      data += sizeof (target_flags);
      len -= sizeof (target_flags);
      r = targetm.check_pch_target_flags (tf);
      if (r != NULL)
	return r;
    }

  for (i = 0; i < cl_options_count; i++)
    if (option_affects_pch_p (i, &state))
      {
	if (memcmp (data, state.data, state.size) != 0)
	  return pch_option_mismatch (cl_options[i].opt_text);
	data += state.size;
	len -= state.size;
      }

  return NULL;
}

/* Default version of cstore_mode.  */

scalar_int_mode
default_cstore_mode (enum insn_code icode)
{
  return as_a <scalar_int_mode> (insn_data[(int) icode].operand[0].mode);
}

/* Default version of member_type_forces_blk.  */

bool
default_member_type_forces_blk (const_tree, machine_mode)
{
  return false;
}

rtx
default_load_bounds_for_arg (rtx addr ATTRIBUTE_UNUSED,
			     rtx ptr ATTRIBUTE_UNUSED,
			     rtx bnd ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void
default_store_bounds_for_arg (rtx val ATTRIBUTE_UNUSED,
			      rtx addr ATTRIBUTE_UNUSED,
			      rtx bounds ATTRIBUTE_UNUSED,
			      rtx to ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

rtx
default_load_returned_bounds (rtx slot ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

void
default_store_returned_bounds (rtx slot ATTRIBUTE_UNUSED,
			       rtx bounds ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Default version of canonicalize_comparison.  */

void
default_canonicalize_comparison (int *, rtx *, rtx *, bool)
{
}

/* Default implementation of TARGET_ATOMIC_ASSIGN_EXPAND_FENV.  */

void
default_atomic_assign_expand_fenv (tree *, tree *, tree *)
{
}

#ifndef PAD_VARARGS_DOWN
#define PAD_VARARGS_DOWN BYTES_BIG_ENDIAN
#endif

/* Build an indirect-ref expression over the given TREE, which represents a
   piece of a va_arg() expansion.  */
tree
build_va_arg_indirect_ref (tree addr)
{
  addr = build_simple_mem_ref_loc (EXPR_LOCATION (addr), addr);
  return addr;
}

/* The "standard" implementation of va_arg: read the value from the
   current (padded) address and increment by the (padded) size.  */

tree
std_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			  gimple_seq *post_p)
{
  tree addr, t, type_size, rounded_size, valist_tmp;
  unsigned HOST_WIDE_INT align, boundary;
  bool indirect;

  /* All of the alignment and movement below is for args-grow-up machines.
     As of 2004, there are only 3 ARGS_GROW_DOWNWARD targets, and they all
     implement their own specialized gimplify_va_arg_expr routines.  */
  if (ARGS_GROW_DOWNWARD)
    gcc_unreachable ();

  indirect = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect)
    type = build_pointer_type (type);

  align = PARM_BOUNDARY / BITS_PER_UNIT;
  boundary = targetm.calls.function_arg_boundary (TYPE_MODE (type), type);

  /* When we align parameter on stack for caller, if the parameter
     alignment is beyond MAX_SUPPORTED_STACK_ALIGNMENT, it will be
     aligned at MAX_SUPPORTED_STACK_ALIGNMENT.  We will match callee
     here with caller.  */
  if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
    boundary = MAX_SUPPORTED_STACK_ALIGNMENT;

  boundary /= BITS_PER_UNIT;

  /* Hoist the valist value into a temporary for the moment.  */
  valist_tmp = get_initialized_tmp_var (valist, pre_p, NULL);

  /* va_list pointer is aligned to PARM_BOUNDARY.  If argument actually
     requires greater alignment, we must perform dynamic alignment.  */
  if (boundary > align
      && !integer_zerop (TYPE_SIZE (type)))
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build_pointer_plus_hwi (valist_tmp, boundary - 1));
      gimplify_and_add (t, pre_p);

      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build2 (BIT_AND_EXPR, TREE_TYPE (valist),
			       valist_tmp,
			       build_int_cst (TREE_TYPE (valist), -boundary)));
      gimplify_and_add (t, pre_p);
    }
  else
    boundary = align;

  /* If the actual alignment is less than the alignment of the type,
     adjust the type accordingly so that we don't assume strict alignment
     when dereferencing the pointer.  */
  boundary *= BITS_PER_UNIT;
  if (boundary < TYPE_ALIGN (type))
    {
      type = build_variant_type_copy (type);
      SET_TYPE_ALIGN (type, boundary);
    }

  /* Compute the rounded size of the type.  */
  type_size = size_in_bytes (type);
  rounded_size = round_up (type_size, align);

  /* Reduce rounded_size so it's sharable with the postqueue.  */
  gimplify_expr (&rounded_size, pre_p, post_p, is_gimple_val, fb_rvalue);

  /* Get AP.  */
  addr = valist_tmp;
  if (PAD_VARARGS_DOWN && !integer_zerop (rounded_size))
    {
      /* Small args are padded downward.  */
      t = fold_build2_loc (input_location, GT_EXPR, sizetype,
		       rounded_size, size_int (align));
      t = fold_build3 (COND_EXPR, sizetype, t, size_zero_node,
		       size_binop (MINUS_EXPR, rounded_size, type_size));
      addr = fold_build_pointer_plus (addr, t);
    }

  /* Compute new value for AP.  */
  t = fold_build_pointer_plus (valist_tmp, rounded_size);
  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
  gimplify_and_add (t, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (indirect)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}

tree
default_chkp_bound_type (void)
{
  tree res = make_node (POINTER_BOUNDS_TYPE);
  TYPE_PRECISION (res) = TYPE_PRECISION (size_type_node) * 2;
  TYPE_NAME (res) = get_identifier ("__bounds_type");
  SET_TYPE_MODE (res, targetm.chkp_bound_mode ());
  layout_type (res);
  return res;
}

machine_mode
default_chkp_bound_mode (void)
{
  return VOIDmode;
}

tree
default_builtin_chkp_function (unsigned int fcode ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

rtx
default_chkp_function_value_bounds (const_tree ret_type ATTRIBUTE_UNUSED,
				    const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
				    bool outgoing ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

tree
default_chkp_make_bounds_constant (HOST_WIDE_INT lb ATTRIBUTE_UNUSED,
				   HOST_WIDE_INT ub ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

int
default_chkp_initialize_bounds (tree var ATTRIBUTE_UNUSED,
				tree lb ATTRIBUTE_UNUSED,
				tree ub ATTRIBUTE_UNUSED,
				tree *stmts ATTRIBUTE_UNUSED)
{
  return 0;
}

void
default_setup_incoming_vararg_bounds (cumulative_args_t ca ATTRIBUTE_UNUSED,
				      machine_mode mode ATTRIBUTE_UNUSED,
				      tree type ATTRIBUTE_UNUSED,
				      int *pretend_arg_size ATTRIBUTE_UNUSED,
				      int second_time ATTRIBUTE_UNUSED)
{
}

/* An implementation of TARGET_CAN_USE_DOLOOP_P for targets that do
   not support nested low-overhead loops.  */

bool
can_use_doloop_if_innermost (const widest_int &, const widest_int &,
			     unsigned int loop_depth, bool)
{
  return loop_depth == 1;
}

/* Default implementation of TARGET_OPTAB_SUPPORTED_P.  */

bool
default_optab_supported_p (int, machine_mode, machine_mode, optimization_type)
{
  return true;
}

/* Default implementation of TARGET_MAX_NOCE_IFCVT_SEQ_COST.  */

unsigned int
default_max_noce_ifcvt_seq_cost (edge e)
{
  bool predictable_p = predictable_edge_p (e);

  enum compiler_param param
    = (predictable_p
       ? PARAM_MAX_RTL_IF_CONVERSION_PREDICTABLE_COST
       : PARAM_MAX_RTL_IF_CONVERSION_UNPREDICTABLE_COST);

  /* If we have a parameter set, use that, otherwise take a guess using
     BRANCH_COST.  */
  if (global_options_set.x_param_values[param])
    return PARAM_VALUE (param);
  else
    return BRANCH_COST (true, predictable_p) * COSTS_N_INSNS (3);
}

/* Default implementation of TARGET_MIN_ARITHMETIC_PRECISION.  */

unsigned int
default_min_arithmetic_precision (void)
{
  return WORD_REGISTER_OPERATIONS ? BITS_PER_WORD : BITS_PER_UNIT;
}

/* Default implementation of TARGET_C_EXCESS_PRECISION.  */

enum flt_eval_method
default_excess_precision (enum excess_precision_type ATTRIBUTE_UNUSED)
{
  return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;
}

bool
default_stack_clash_protection_final_dynamic_probe (rtx residual ATTRIBUTE_UNUSED)
{
  return 0;
}

#include "gt-targhooks.h"
