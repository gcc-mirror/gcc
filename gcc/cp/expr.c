/* Convert language-specific tree expression to rtl instructions,
   for GNU compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include <stdio.h>
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "expr.h"
#include "cp-tree.h"

static tree extract_aggr_init PROTO((tree, tree));
static tree extract_scalar_init PROTO((tree, tree));
static rtx cplus_expand_expr PROTO((tree, rtx, enum machine_mode,
				    enum expand_modifier));

/* Hook used by expand_expr to expand language-specific tree codes.  */

static rtx
cplus_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  int ignore = target == const0_rtx;

  if (ignore)
    target = 0;

  /* No sense saving up arithmetic to be done
     if it's all in the wrong mode to form part of an address.
     And force_operand won't know whether to sign-extend or zero-extend.  */

  if (mode != Pmode && modifier == EXPAND_SUM)
    modifier = EXPAND_NORMAL;

  switch (code)
    {
    case NEW_EXPR:
      {
	/* Something needs to be initialized, but we didn't know
	   where that thing was when building the tree.  For example,
	   it could be the return value of a function, or a parameter
	   to a function which lays down in the stack, or a temporary
	   variable which must be passed by reference.

	   Cleanups are handled in a language-specific way: they
	   might be run by the called function (true in GNU C++
	   for parameters with cleanups), or they might be
	   run by the caller, after the call (true in GNU C++
	   for other cleanup needs).  */

	tree func = TREE_OPERAND (exp, 0);
	tree args = TREE_OPERAND (exp, 1);
	tree type = TREE_TYPE (exp), slot;
	tree fn_type = TREE_TYPE (TREE_TYPE (func));
	tree return_type = TREE_TYPE (fn_type);
	tree call_exp;
	rtx call_target, return_target;
	int pcc_struct_return = 0;

	/* The expression `init' wants to initialize what
	   `target' represents.  SLOT holds the slot for TARGET.  */
	slot = TREE_OPERAND (exp, 2);

	if (target == 0)
	  {
	    /* Should always be called with a target in BLKmode case.  */
	    my_friendly_assert (mode != BLKmode, 205);
	    my_friendly_assert (DECL_RTL (slot) != 0, 206);

	    target = gen_reg_rtx (mode);
	  }

	/* The target the initializer will initialize (CALL_TARGET)
	   must now be directed to initialize the target we are
	   supposed to initialize (TARGET).  The semantics for
	   choosing what CALL_TARGET is is language-specific,
	   as is building the call which will perform the
	   initialization.  It is left here to show the choices that
	   exist for C++.  */
	   
	if (TREE_CODE (func) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (func, 0)) == FUNCTION_DECL
	    && DECL_CONSTRUCTOR_P (TREE_OPERAND (func, 0)))
	  {
	    type = build_pointer_type (type);
	    /* Don't clobber a value that might be part of a default
	       parameter value.  */
	    mark_addressable (slot);
	    if (TREE_PERMANENT (args))
	      args = expr_tree_cons (0, build1 (ADDR_EXPR, type, slot),
				TREE_CHAIN (args));
	    else
	      TREE_VALUE (args) = build1 (ADDR_EXPR, type, slot);
	    call_target = 0;
	  }
	else if (TREE_CODE (return_type) == REFERENCE_TYPE)
	  {
	    type = return_type;
	    call_target = 0;
	  }
	else
	  {
#ifdef PCC_STATIC_STRUCT_RETURN
	    pcc_struct_return = 1;
	    call_target = 0;
#else
	    call_target = target;
#endif
	  }
	if (call_target)
	  {
	    /* Make this a valid memory address now.  The code below assumes
	       that it can compare rtx and make assumptions based on the
	       result.  The assumptions are true only if the address was
	       valid to begin with.  */
	    call_target = validize_mem (call_target);

	    /* If this is a reference to a symbol, expand_inline_function
	       will do this transformation and return a different target
	       than the one we gave it, though functionally equivalent.  Do
	       the transformation here to avoid confusion.  */
	    if (! cse_not_expected && GET_CODE (call_target) == MEM
		&& GET_CODE (XEXP (call_target, 0)) == SYMBOL_REF)
	      {
		call_target = gen_rtx
		  (MEM, mode, memory_address (mode, XEXP (call_target, 0)));
		MEM_IN_STRUCT_P (call_target) = 1;
	      }
	  }

	call_exp = build (CALL_EXPR, type, func, args, NULL_TREE);
	TREE_SIDE_EFFECTS (call_exp) = 1;
	return_target = expand_call (call_exp, call_target, ignore);
	if (call_target == 0)
	  {
	    if (pcc_struct_return)
	      {
		extern int flag_access_control;
		int old_ac = flag_access_control;

		tree init = build_decl (VAR_DECL, 0, type);
		TREE_ADDRESSABLE (init) = 1;
		DECL_RTL (init) = return_target;

		flag_access_control = 0;
		expand_aggr_init (slot, init, 0, LOOKUP_ONLYCONVERTING);
		flag_access_control = old_ac;

		if (TYPE_NEEDS_DESTRUCTOR (type))
		  {
		    init = build_decl (VAR_DECL, 0,
				       build_reference_type (type));
		    DECL_RTL (init) = XEXP (return_target, 0);

		    init = maybe_build_cleanup (convert_from_reference (init));
		    if (init != NULL_TREE)
		      expand_expr (init, const0_rtx, VOIDmode, 0);
		  }
		call_target = return_target = DECL_RTL (slot);
	      }
	    else
	      call_target = return_target;
	  }

	if (call_target != return_target)
	  {
	    my_friendly_assert (TYPE_HAS_TRIVIAL_INIT_REF (type), 317);
	    if (GET_MODE (return_target) == BLKmode)
	      emit_block_move (call_target, return_target, expr_size (exp),
			       TYPE_ALIGN (type) / BITS_PER_UNIT);
	    else
	      emit_move_insn (call_target, return_target);
	  }

	if (TREE_CODE (return_type) == REFERENCE_TYPE)
	  {
	    tree init;

	    if (GET_CODE (call_target) == REG
		&& REGNO (call_target) < FIRST_PSEUDO_REGISTER)
	      my_friendly_abort (39);

	    type = TREE_TYPE (exp);

	    init = build (RTL_EXPR, return_type, 0, call_target);
	    /* We got back a reference to the type we want.  Now initialize
	       target with that.  */
	    expand_aggr_init (slot, init, 0, LOOKUP_ONLYCONVERTING);
	  }

	if (DECL_RTL (slot) != target)
	  emit_move_insn (DECL_RTL (slot), target);
	return DECL_RTL (slot);
      }

    case OFFSET_REF:
      {
#if 1
	return expand_expr (default_conversion (resolve_offset_ref (exp)),
			    target, tmode, EXPAND_NORMAL);
#else
	/* This is old crusty code, and does not handle all that the
	   resolve_offset_ref function does.  (mrs) */
	tree base = build_unary_op (ADDR_EXPR, TREE_OPERAND (exp, 0), 0);
	tree offset = build_unary_op (ADDR_EXPR, TREE_OPERAND (exp, 1), 0);
	return expand_expr (build (PLUS_EXPR, TREE_TYPE (exp), base, offset),
			    target, tmode, EXPAND_NORMAL);
#endif
      }

    case THUNK_DECL:
      return DECL_RTL (exp);

    case THROW_EXPR:
      expand_throw (TREE_OPERAND (exp, 0));
      return NULL;

    case VEC_INIT_EXPR:
      return expand_expr
	(expand_vec_init
	 (NULL_TREE, TREE_OPERAND (exp, 0),
	  build_binary_op (MINUS_EXPR, TREE_OPERAND (exp, 2),
			   integer_one_node, 1),
	  TREE_OPERAND (exp, 1), 0), target, tmode, modifier);

    default:
      break;
    }
  my_friendly_abort (40);
  /* NOTREACHED */
  return NULL;
}

void
init_cplus_expand ()
{
  lang_expand_expr = cplus_expand_expr;
}

/* If DECL had its rtl moved from where callers expect it
   to be, fix it up.  RESULT is the nominal rtl for the RESULT_DECL,
   which may be a pseudo instead of a hard register.  */

void
fixup_result_decl (decl, result)
     tree decl;
     rtx result;
{
  if (REG_P (result))
    {
      if (REGNO (result) >= FIRST_PSEUDO_REGISTER)
	{
	  rtx real_decl_result;

#ifdef FUNCTION_OUTGOING_VALUE
	  real_decl_result
	    = FUNCTION_OUTGOING_VALUE (TREE_TYPE (decl), current_function_decl);
#else
	  real_decl_result
	    = FUNCTION_VALUE (TREE_TYPE (decl), current_function_decl);
#endif
	  REG_FUNCTION_VALUE_P (real_decl_result) = 1;
	  result = real_decl_result;
	}
      store_expr (decl, result, 0);
      emit_insn (gen_rtx (USE, VOIDmode, result));
    }
}

/* Expand this initialization inline and see if it's simple enough that
   it can be done at compile-time.  */

static tree
extract_aggr_init (decl, init)
     tree decl, init;
{
  return 0;
}

static tree
extract_scalar_init (decl, init)
     tree decl, init;
{
  rtx value, insns, insn;
  extern struct obstack temporary_obstack;
  tree t = NULL_TREE;

  push_obstacks (&temporary_obstack, &temporary_obstack);
  start_sequence ();
  value = expand_expr (init, NULL_RTX, VOIDmode, 0);
  insns = get_insns ();
  end_sequence ();
  reg_scan (insns, max_reg_num (), 0);
  jump_optimize (insns, 0, 0, 1);
  pop_obstacks ();

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      rtx r, to;

      if (GET_CODE (insn) == NOTE)
	continue;
      else if (GET_CODE (insn) != INSN)
	return 0;

      r = PATTERN (insn);
      if (GET_CODE (r) != SET)
	return 0;

      to = XEXP (r, 0);

      if (! (to == value
	     || (GET_CODE (to) == SUBREG && XEXP (to, 0) == value)))
	return 0;

      r = XEXP (r, 1);

      switch (GET_CODE (r))
	{
	case CONST_INT:
	  t = build_int_2 (XEXP (r, 0), 0);
	  break;
	default:
	  return 0;
	}
    }

  return t; 
}

int
extract_init (decl, init)
     tree decl, init;
{
  return 0;

#if 0
  if (IS_AGGR_TYPE (TREE_TYPE (decl))
      || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    init = extract_aggr_init (decl, init);
  else
    init = extract_scalar_init (decl, init);

  if (init == NULL_TREE)
    return 0;

  DECL_INITIAL (decl) = init;
  return 1;
#endif
}

void
do_case (start, end)
     tree start, end;
{
  tree value1 = NULL_TREE, value2 = NULL_TREE, label;

  if (start != NULL_TREE && TREE_TYPE (start) != NULL_TREE 
      && POINTER_TYPE_P (TREE_TYPE (start)))
    error ("pointers are not permitted as case values");

  if (end && pedantic)
    pedwarn ("ANSI C++ forbids range expressions in switch statement");

  if (processing_template_decl)
    {
      add_tree (build_min_nt (CASE_LABEL, start, end));
      return;
    }

  if (start)
    value1 = check_cp_case_value (start);
  if (end)
    value2 = check_cp_case_value (end);
  
  label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

  if (value1 != error_mark_node
      && value2 != error_mark_node)
    {
      tree duplicate;
      int success;

      if (end)
	success = pushcase_range (value1, value2, convert_and_check,
				  label, &duplicate);
      else if (start)
	success = pushcase (value1, convert_and_check, label, &duplicate);
      else
	success = pushcase (NULL_TREE, 0, label, &duplicate);

      if (success == 1)
	{
	  if (end)
	    error ("case label not within a switch statement");
	  else if (start)
	    cp_error ("case label `%E' not within a switch statement", start);
	  else
	    error ("default label not within a switch statement");
	}
      else if (success == 2)
	{
	  if (end)
	    {
	      error ("duplicate (or overlapping) case value");
	      cp_error_at ("this is the first entry overlapping that value",
			   duplicate);
	    }
	  else if (start)
	    {
	      cp_error ("duplicate case value `%E'", start);
	      cp_error_at ("previously used here", duplicate);
	    }
	  else
	    {
	      error ("multiple default labels in one switch");
	      cp_error_at ("this is the first default label", duplicate);
	    }
	}
      else if (success == 3)
	warning ("case value out of range");
      else if (success == 4)
	warning ("empty range specified");
      else if (success == 5)
	{
	  if (end)
	    error ("case label within scope of cleanup or variable array");
	  else if (! start)
	    error ("`default' label within scope of cleanup or variable array");
	  else
	    cp_error ("case label `%E' within scope of cleanup or variable array", start);
	}
    }
  if (start)
    define_case_label (label);
  else
    define_case_label (NULL_TREE);
}
