/* Default target hook functions.
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009
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
#include "toplev.h"
#include "function.h"
#include "target.h"
#include "tm_p.h"
#include "target-def.h"
#include "ggc.h"
#include "hard-reg-set.h"
#include "reload.h"
#include "optabs.h"
#include "recog.h"


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

/* Emit any directives required to unwind this instruction.  */

void
default_unwind_emit (FILE * stream ATTRIBUTE_UNUSED,
		     rtx insn ATTRIBUTE_UNUSED)
{
  /* Should never happen.  */
  gcc_unreachable ();
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
default_builtin_vectorized_function (unsigned int fn ATTRIBUTE_UNUSED,
				     tree type_out ATTRIBUTE_UNUSED,
				     tree type_in ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* Vectorized conversion.  */

tree
default_builtin_vectorized_conversion (unsigned int code ATTRIBUTE_UNUSED,
				       tree type ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
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

#ifdef FUNCTION_OUTGOING_VALUE
  if (outgoing)
    return FUNCTION_OUTGOING_VALUE (ret_type, fn_decl_or_type);
#endif

#ifdef FUNCTION_VALUE
  return FUNCTION_VALUE (ret_type, fn_decl_or_type);
#else
  return NULL_RTX;
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

enum reg_class
default_branch_target_register_class (void)
{
  return NO_REGS;
}

#ifdef IRA_COVER_CLASSES
const enum reg_class *
default_ira_cover_classes (void)
{
  static enum reg_class classes[] = IRA_COVER_CLASSES;
  return classes;
}
#endif

enum reg_class
default_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED,
			  enum reg_class reload_class ATTRIBUTE_UNUSED,
			  enum machine_mode reload_mode ATTRIBUTE_UNUSED,
			  secondary_reload_info *sri)
{
  enum reg_class rclass = NO_REGS;

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
      enum insn_code icode = (in_p ? reload_in_optab[(int) reload_mode]
			      : reload_out_optab[(int) reload_mode]);

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

bool
default_hard_regno_scratch_ok (unsigned int regno ATTRIBUTE_UNUSED)
{
  return true;
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

#include "gt-targhooks.h"
