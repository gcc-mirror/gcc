/* Default target hook functions.
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "expr.h"
#include "output.h"
#include "diagnostic-core.h"
#include "function.h"
#include "target.h"
#include "tm_p.h"
#include "target-def.h"
#include "ggc.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "reload.h"
#include "optabs.h"
#include "recog.h"
#include "intl.h"
#include "opts.h"
#include "tree-flow.h"
#include "tree-ssa-alias.h"


bool
default_legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
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
  ASM_OUTPUT_EXTERNAL_LIBCALL(asm_out_file, fun);
#endif
}

int
default_unspec_may_trap_p (const_rtx x, unsigned flags)
{
  int i;

  if (GET_CODE (x) == UNSPEC_VOLATILE
      /* Any floating arithmetic may trap.  */
      || (SCALAR_FLOAT_MODE_P (GET_MODE (x))
	  && flag_trapping_math))
    return 1;

  for (i = 0; i < XVECLEN (x, 0); ++i)
    {
      if (may_trap_p_1 (XVECEXP (x, 0, i), flags))
	return 1;
    }

  return 0;
}

enum machine_mode
default_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
			       enum machine_mode mode,
			       int *punsignedp ATTRIBUTE_UNUSED,
			       const_tree funtype ATTRIBUTE_UNUSED,
			       int for_return ATTRIBUTE_UNUSED)
{
  if (for_return == 2)
    return promote_mode (type, mode, punsignedp);
  return mode;
}

enum machine_mode
default_promote_function_mode_always_promote (const_tree type,
					      enum machine_mode mode,
					      int *punsignedp,
					      const_tree funtype ATTRIBUTE_UNUSED,
					      int for_return ATTRIBUTE_UNUSED)
{
  return promote_mode (type, mode, punsignedp);
}


enum machine_mode
default_cc_modes_compatible (enum machine_mode m1, enum machine_mode m2)
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
			    enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return x;
}

rtx
default_expand_builtin_saveregs (void)
{
  error ("__builtin_saveregs not supported by this target");
  return const0_rtx;
}

void
default_setup_incoming_varargs (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
				enum machine_mode mode ATTRIBUTE_UNUSED,
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
hook_bool_CUMULATIVE_ARGS_false (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED)
{
  return false;
}

bool
default_pretend_outgoing_varargs_named (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED)
{
  return (targetm.calls.setup_incoming_varargs
	  != default_setup_incoming_varargs);
}

enum machine_mode
default_eh_return_filter_mode (void)
{
  return targetm.unwind_word_mode ();
}

enum machine_mode
default_libgcc_cmp_return_mode (void)
{
  return word_mode;
}

enum machine_mode
default_libgcc_shift_count_mode (void)
{
  return word_mode;
}

enum machine_mode
default_unwind_word_mode (void)
{
  return word_mode;
}

/* The default implementation of TARGET_SHIFT_TRUNCATION_MASK.  */

unsigned HOST_WIDE_INT
default_shift_truncation_mask (enum machine_mode mode)
{
  return SHIFT_COUNT_TRUNCATED ? GET_MODE_BITSIZE (mode) - 1 : 0;
}

/* The default implementation of TARGET_MIN_DIVISIONS_FOR_RECIP_MUL.  */

unsigned int
default_min_divisions_for_recip_mul (enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return have_insn_for (DIV, mode) ? 3 : 2;
}

/* The default implementation of TARGET_MODE_REP_EXTENDED.  */

int
default_mode_rep_extended (enum machine_mode mode ATTRIBUTE_UNUSED,
			   enum machine_mode mode_rep ATTRIBUTE_UNUSED)
{
  return UNKNOWN;
}

/* Generic hook that takes a CUMULATIVE_ARGS pointer and returns true.  */

bool
hook_bool_CUMULATIVE_ARGS_true (CUMULATIVE_ARGS * a ATTRIBUTE_UNUSED)
{
  return true;
}

/* Return machine mode for non-standard suffix
   or VOIDmode if non-standard suffixes are unsupported.  */
enum machine_mode
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
  if (INT_CST_LT_UNSIGNED (type_align, sizetype_size))
    cookie_size = sizetype_size;
  else
    cookie_size = type_align;

  return cookie_size;
}

/* Return true if a parameter must be passed by reference.  This version
   of the TARGET_PASS_BY_REFERENCE hook uses just MUST_PASS_IN_STACK.  */

bool
hook_pass_by_reference_must_pass_in_stack (CUMULATIVE_ARGS *c ATTRIBUTE_UNUSED,
	enum machine_mode mode ATTRIBUTE_UNUSED, const_tree type ATTRIBUTE_UNUSED,
	bool named_arg ATTRIBUTE_UNUSED)
{
  return targetm.calls.must_pass_in_stack (mode, type);
}

/* Return true if a parameter follows callee copies conventions.  This
   version of the hook is true for all named arguments.  */

bool
hook_callee_copies_named (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
			  enum machine_mode mode ATTRIBUTE_UNUSED,
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

/* The default implementation of TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

bool
default_asm_output_addr_const_extra (FILE *file ATTRIBUTE_UNUSED,
				     rtx x ATTRIBUTE_UNUSED)
{
#ifdef OUTPUT_ADDR_CONST_EXTRA
  OUTPUT_ADDR_CONST_EXTRA (file, x, fail);
  return true;

fail:
#endif
  return false;
}

/* True if MODE is valid for the target.  By "valid", we mean able to
   be manipulated in non-trivial ways.  In particular, this means all
   the arithmetic is supported.

   By default we guess this means that any C type is supported.  If
   we can't map the mode back to a type that would be available in C,
   then reject it.  Special case, here, is the double-word arithmetic
   supported by optabs.c.  */

bool
default_scalar_mode_supported_p (enum machine_mode mode)
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

/* NULL if INSN insn is valid within a low-overhead loop, otherwise returns
   an error message.

   This function checks whether a given INSN is valid within a low-overhead
   loop.  If INSN is invalid it returns the reason for that, otherwise it
   returns NULL. A called function may clobber any special registers required
   for low-overhead looping. Additionally, some targets (eg, PPC) use the count
   register for branch on table instructions. We reject the doloop pattern in
   these cases.  */

const char *
default_invalid_within_doloop (const_rtx insn)
{
  if (CALL_P (insn))
    return "Function call in loop.";

  if (JUMP_TABLE_DATA_P (insn))
    return "Computed branch in the loop.";

  return NULL;
}

/* Mapping of builtin functions to vectorized variants.  */

tree
default_builtin_vectorized_function (tree fndecl ATTRIBUTE_UNUSED,
				     tree type_out ATTRIBUTE_UNUSED,
				     tree type_in ATTRIBUTE_UNUSED)
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
                                    tree vectype ATTRIBUTE_UNUSED,
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
        return 1;

      case unaligned_load:
      case unaligned_store:
        return 2;

      case cond_branch_taken:
        return 3;

      default:
        gcc_unreachable ();
    }
}

/* Reciprocal.  */

tree
default_builtin_reciprocal (unsigned int fn ATTRIBUTE_UNUSED,
			    bool md_fn ATTRIBUTE_UNUSED,
			    bool sqrt ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

bool
hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false (
	CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
	enum machine_mode mode ATTRIBUTE_UNUSED,
	const_tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
{
  return false;
}

bool
hook_bool_CUMULATIVE_ARGS_mode_tree_bool_true (
	CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
	enum machine_mode mode ATTRIBUTE_UNUSED,
	const_tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
{
  return true;
}

int
hook_int_CUMULATIVE_ARGS_mode_tree_bool_0 (
	CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
	enum machine_mode mode ATTRIBUTE_UNUSED,
	tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
{
  return 0;
}

void
default_function_arg_advance (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
			      enum machine_mode mode ATTRIBUTE_UNUSED,
			      const_tree type ATTRIBUTE_UNUSED,
			      bool named ATTRIBUTE_UNUSED)
{
#ifdef FUNCTION_ARG_ADVANCE
  CUMULATIVE_ARGS args = *ca;
  FUNCTION_ARG_ADVANCE (args, mode, CONST_CAST_TREE (type), named);
  *ca = args;
#else
  gcc_unreachable ();
#endif
}

rtx
default_function_arg (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
		      enum machine_mode mode ATTRIBUTE_UNUSED,
		      const_tree type ATTRIBUTE_UNUSED,
		      bool named ATTRIBUTE_UNUSED)
{
#ifdef FUNCTION_ARG
  return FUNCTION_ARG (*ca, mode, CONST_CAST_TREE (type), named);
#else
  gcc_unreachable ();
#endif
}

rtx
default_function_incoming_arg (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
			       enum machine_mode mode ATTRIBUTE_UNUSED,
			       const_tree type ATTRIBUTE_UNUSED,
			       bool named ATTRIBUTE_UNUSED)
{
#ifdef FUNCTION_INCOMING_ARG
  return FUNCTION_INCOMING_ARG (*ca, mode, CONST_CAST_TREE (type), named);
#else
  gcc_unreachable ();
#endif
}

unsigned int
default_function_arg_boundary (enum machine_mode mode ATTRIBUTE_UNUSED,
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
default_libcall_value (enum machine_mode mode ATTRIBUTE_UNUSED,
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
default_static_chain (const_tree fndecl, bool incoming_p)
{
  if (!DECL_STATIC_CHAIN (fndecl))
    return NULL;

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

#ifdef IRA_COVER_CLASSES
const reg_class_t *
default_ira_cover_classes (void)
{
  static reg_class_t classes[] = IRA_COVER_CLASSES;
  return classes;
}
#endif

reg_class_t
default_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED,
			  reg_class_t reload_class_i ATTRIBUTE_UNUSED,
			  enum machine_mode reload_mode ATTRIBUTE_UNUSED,
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
	  && insn_data[(int) icode].operand[in_p].predicate
	  && ! insn_data[(int) icode].operand[in_p].predicate (x, reload_mode))
	icode = CODE_FOR_nothing;
      else if (icode != CODE_FOR_nothing)
	{
	  const char *insn_constraint, *scratch_constraint;
	  char insn_letter, scratch_letter;
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
	      insn_letter = *insn_constraint;
	      insn_class
		= (insn_letter == 'r' ? GENERAL_REGS
		   : REG_CLASS_FROM_CONSTRAINT ((unsigned char) insn_letter,
						insn_constraint));
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
	  scratch_letter = *scratch_constraint;
	  scratch_class
	    = (scratch_letter == 'r' ? GENERAL_REGS
	       : REG_CLASS_FROM_CONSTRAINT ((unsigned char) scratch_letter,
					    scratch_constraint));

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

bool
default_handle_c_option (size_t code ATTRIBUTE_UNUSED,
			 const char *arg ATTRIBUTE_UNUSED,
			 int value ATTRIBUTE_UNUSED)
{
  return false;
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

bool
default_builtin_vector_alignment_reachable (const_tree type, bool is_packed)
{
  if (is_packed)
    return false;

  /* Assuming that types whose size is > pointer-size are not guaranteed to be
     naturally aligned.  */
  if (tree_int_cst_compare (TYPE_SIZE (type), bitsize_int (POINTER_SIZE)) > 0)
    return false;

  /* Assuming that types whose size is <= pointer-size
     are naturally aligned.  */
  return true;
}

/* By default, assume that a target supports any factor of misalignment
   memory access if it supports movmisalign patten.
   is_packed is true if the memory access is defined in a packed struct.  */
bool
default_builtin_support_vector_misalignment (enum machine_mode mode,
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

enum machine_mode
default_preferred_simd_mode (enum machine_mode mode ATTRIBUTE_UNUSED)
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

/* Determine whether or not a pointer mode is valid. Assume defaults
   of ptr_mode or Pmode - can be overridden.  */
bool
default_valid_pointer_mode (enum machine_mode mode)
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

/* Return the mode for a pointer to a given ADDRSPACE, defaulting to ptr_mode
   for the generic address space only.  */

enum machine_mode
default_addr_space_pointer_mode (addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  gcc_assert (ADDR_SPACE_GENERIC_P (addrspace));
  return ptr_mode;
}

/* Return the mode for an address in a given ADDRSPACE, defaulting to Pmode
   for the generic address space only.  */

enum machine_mode
default_addr_space_address_mode (addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  gcc_assert (ADDR_SPACE_GENERIC_P (addrspace));
  return Pmode;
}

/* Named address space version of valid_pointer_mode.  */

bool
default_addr_space_valid_pointer_mode (enum machine_mode mode, addr_space_t as)
{
  if (!ADDR_SPACE_GENERIC_P (as))
    return (mode == targetm.addr_space.pointer_mode (as)
	    || mode == targetm.addr_space.address_mode (as));

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

/* Named address space version of legitimate_address_p.  */

bool
default_addr_space_legitimate_address_p (enum machine_mode mode, rtx mem,
					 bool strict, addr_space_t as)
{
  if (!ADDR_SPACE_GENERIC_P (as))
    gcc_unreachable ();

  return targetm.legitimate_address_p (mode, mem, strict);
}

/* Named address space version of LEGITIMIZE_ADDRESS.  */

rtx
default_addr_space_legitimize_address (rtx x, rtx oldx,
				       enum machine_mode mode, addr_space_t as)
{
  if (!ADDR_SPACE_GENERIC_P (as))
    return x;

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

/* The default hook for TARGET_ADDR_SPACE_CONVERT. This hook should never be
   called for targets with only a generic address space.  */

rtx
default_addr_space_convert (rtx op ATTRIBUTE_UNUSED,
			    tree from_type ATTRIBUTE_UNUSED,
			    tree to_type ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

bool
default_hard_regno_scratch_ok (unsigned int regno ATTRIBUTE_UNUSED)
{
  return true;
}

/* The default implementation of TARGET_MODE_DEPENDENT_ADDRESS_P.  */

bool
default_mode_dependent_address_p (const_rtx addr ATTRIBUTE_UNUSED)
{
#ifdef GO_IF_MODE_DEPENDENT_ADDRESS

  GO_IF_MODE_DEPENDENT_ADDRESS (CONST_CAST_RTX (addr), win);
  return false;
  /* Label `win' might (not) be used via GO_IF_MODE_DEPENDENT_ADDRESS.  */
 win: ATTRIBUTE_UNUSED_LABEL
  return true;

#else

  return false;

#endif
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
  warning (OPT_Wpragmas,
	   "#pragma GCC target is not supported for this machine");

  return false;
}

bool
default_target_can_inline_p (tree caller, tree callee)
{
  bool ret = false;
  tree callee_opts = DECL_FUNCTION_SPECIFIC_TARGET (callee);
  tree caller_opts = DECL_FUNCTION_SPECIFIC_TARGET (caller);

  /* If callee has no option attributes, then it is ok to inline */
  if (!callee_opts)
    ret = true;

  /* If caller has no option attributes, but callee does then it is not ok to
     inline */
  else if (!caller_opts)
    ret = false;

  /* If both caller and callee have attributes, assume that if the pointer is
     different, the the two functions have different target options since
     build_target_option_node uses a hash table for the options.  */
  else
    ret = (callee_opts == caller_opts);

  return ret;
}

#ifndef HAVE_casesi
# define HAVE_casesi 0
#endif

/* If the machine does not have a case insn that compares the bounds,
   this means extra overhead for dispatch tables, which raises the
   threshold for using them.  */

unsigned int default_case_values_threshold (void)
{
  return (HAVE_casesi ? 4 : 5);
}

bool
default_have_conditional_execution (void)
{
#ifdef HAVE_conditional_execution
  return HAVE_conditional_execution;
#else
  return false;
#endif
}

/* Compute cost of moving registers to/from memory.  */

int
default_memory_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
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
default_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                            reg_class_t from ATTRIBUTE_UNUSED,
                            reg_class_t to ATTRIBUTE_UNUSED)
{
#ifndef REGISTER_MOVE_COST
  return 2;
#else
  return REGISTER_MOVE_COST (mode, (enum reg_class) from, (enum reg_class) to);
#endif
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
#ifdef PREFERRED_OUTPUT_RELOAD_CLASS
  return PREFERRED_OUTPUT_RELOAD_CLASS (x, (enum reg_class) rclass);
#else
  return rclass;
#endif
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

/* Determine the exception handling mechanism for the target.  */

enum unwind_info_type
default_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  /* Obey the configure switch to turn on sjlj exceptions.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  /* ??? Change all users to the hook, then poison this.  */
#ifdef DWARF2_UNWIND_INFO
  if (DWARF2_UNWIND_INFO)
    return UI_DWARF2;
#endif

  return UI_SJLJ;
}

/* To be used by targets that force dwarf2 unwind enabled.  */

enum unwind_info_type
dwarf2_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  /* Obey the configure switch to turn on sjlj exceptions.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  return UI_DWARF2;
}

/* To be used by targets that force sjlj unwind enabled.  */

enum unwind_info_type
sjlj_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  return UI_SJLJ;
}

/* To be used by targets where reg_raw_mode doesn't return the right
   mode for registers used in apply_builtin_return and apply_builtin_arg.  */

enum machine_mode
default_get_reg_raw_mode(int regno)
{
  return reg_raw_mode[regno];
}

/* Return true if the state of option OPTION should be stored in PCH files
   and checked by default_pch_valid_p.  Store the option's current state
   in STATE if so.  */

static inline bool
option_affects_pch_p (int option, struct cl_option_state *state)
{
  if ((cl_options[option].flags & CL_TARGET) == 0)
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
  char *r;

  asprintf (&r, _("created and used with differing settings of '%s'"), option);
  if (r == NULL)
    return _("out of memory");
  return r;
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

const struct default_options empty_optimization_table[] =
  {
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#include "gt-targhooks.h"
