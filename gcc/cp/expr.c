/* Convert language-specific tree expression to rtl instructions,
   for GNU compiler.
   Copyright (C) 1988, 1992, 1993 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "expr.h"
#include "cp-tree.h"

#undef NULL
#define NULL 0

/* Hook used by expand_expr to expand language-specific tree codes.  */

rtx
cplus_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  rtx original_target = target;
  int ignore = target == const0_rtx;

  if (ignore)
    target = 0, original_target = 0;

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
	    type = TYPE_POINTER_TO (type);
	    /* Don't clobber a value that might be part of a default
	       parameter value.  */
	    if (TREE_PERMANENT (args))
	      args = tree_cons (0, build1 (ADDR_EXPR, type, slot),
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
	    preserve_temp_slots (call_target);

	    /* Make this a valid memory address now.  The code below assumes
	       that it can compare rtx and make assumptions based on the
	       result.  The assumptions are true only if the address was
	       valid to begin with.  */
	    call_target = validize_mem (call_target);
	  }

	preserve_temp_slots (DECL_RTL (slot));
	call_exp = build (CALL_EXPR, type, func, args, 0);
	TREE_SIDE_EFFECTS (call_exp) = 1;
	return_target = expand_expr (call_exp, call_target, mode, 0);
	free_temp_slots ();
	if (call_target == 0)
	  {
	    if (pcc_struct_return)
	      {
		extern int flag_access_control;
		int old_ac = flag_access_control;

		tree init = build (RTL_EXPR, type, 0, return_target);
		TREE_ADDRESSABLE (init) = 1;

		flag_access_control = 0;
		expand_aggr_init (slot, init, 0);
		flag_access_control = old_ac;

		if (TYPE_NEEDS_DESTRUCTOR (type))
		  {
		    init = build (RTL_EXPR, build_reference_type (type), 0,
				  XEXP (return_target, 0));
		    init = maybe_build_cleanup (convert_from_reference (init));
		    if (init != NULL_TREE)
		      expand_expr (init, 0, 0, 0);
		  }
		call_target = return_target = DECL_RTL (slot);
	      }
	    else
	      call_target = return_target;
	  }

	if (call_target != return_target)
	  {
	    my_friendly_assert (! TYPE_NEEDS_CONSTRUCTING (type), 317);
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
	    expand_aggr_init (slot, init, 0);
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
      emit_move_insn (result, DECL_RTL (decl));
      emit_insn (gen_rtx (USE, VOIDmode, result));
    }
}

/* Return nonzero iff DECL is memory-based.  The DECL_RTL of
   certain const variables might be a CONST_INT, or a REG
   in some cases.  We cannot use `memory_operand' as a test
   here because on most RISC machines, a variable's address
   is not, by itself, a legitimate address.  */
int
decl_in_memory_p (decl)
     tree decl;
{
  return DECL_RTL (decl) != 0 && GET_CODE (DECL_RTL (decl)) == MEM;
}
