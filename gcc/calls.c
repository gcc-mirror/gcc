/* Convert function calls to rtl insns, for GNU C compiler.
   Copyright (C) 1989, 1992 Free Software Foundation, Inc.

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
#include "gvarargs.h"
#include "insn-flags.h"

/* Decide whether a function's arguments should be processed
   from first to last or from last to first.

   They should if the stack and args grow in opposite directions, but
   only if we have push insns.  */

#ifdef PUSH_ROUNDING

#if defined (STACK_GROWS_DOWNWARD) != defined (ARGS_GROW_DOWNARD)
#define PUSH_ARGS_REVERSED	/* If it's last to first */
#endif

#endif

/* Like STACK_BOUNDARY but in units of bytes, not bits.  */
#define STACK_BYTES (STACK_BOUNDARY / BITS_PER_UNIT)

/* Data structure and subroutines used within expand_call.  */

struct arg_data
{
  /* Tree node for this argument.  */
  tree tree_value;
  /* Mode for value; TYPE_MODE unless promoted.  */
  enum machine_mode mode;
  /* Current RTL value for argument, or 0 if it isn't precomputed.  */
  rtx value;
  /* Initially-compute RTL value for argument; only for const functions.  */
  rtx initial_value;
  /* Register to pass this argument in, 0 if passed on stack, or an
     EXPR_LIST if the arg is to be copied into multiple different
     registers.  */
  rtx reg;
  /* If REG was promoted from the actual mode of the argument expression,
     indicates whether the promotion is sign- or zero-extended.  */
  int unsignedp;
  /* Number of registers to use.  0 means put the whole arg in registers.
     Also 0 if not passed in registers.  */
  int partial;
  /* Non-zero if argument must be passed on stack.
     Note that some arguments may be passed on the stack
     even though pass_on_stack is zero, just because FUNCTION_ARG says so.
     pass_on_stack identifies arguments that *cannot* go in registers.  */
  int pass_on_stack;
  /* Offset of this argument from beginning of stack-args.  */
  struct args_size offset;
  /* Similar, but offset to the start of the stack slot.  Different from
     OFFSET if this arg pads downward.  */
  struct args_size slot_offset;
  /* Size of this argument on the stack, rounded up for any padding it gets,
     parts of the argument passed in registers do not count.
     If REG_PARM_STACK_SPACE is defined, then register parms
     are counted here as well.  */
  struct args_size size;
  /* Location on the stack at which parameter should be stored.  The store
     has already been done if STACK == VALUE.  */
  rtx stack;
  /* Location on the stack of the start of this argument slot.  This can
     differ from STACK if this arg pads downward.  This location is known
     to be aligned to FUNCTION_ARG_BOUNDARY.  */
  rtx stack_slot;
#ifdef ACCUMULATE_OUTGOING_ARGS
  /* Place that this stack area has been saved, if needed.  */
  rtx save_area;
#endif
#ifdef STRICT_ALIGNMENT
  /* If an argument's alignment does not permit direct copying into registers,
     copy in smaller-sized pieces into pseudos.  These are stored in a
     block pointed to by this field.  The next field says how many
     word-sized pseudos we made.  */
  rtx *aligned_regs;
  int n_aligned_regs;
#endif
};

#ifdef ACCUMULATE_OUTGOING_ARGS
/* A vector of one char per byte of stack space.  A byte if non-zero if
   the corresponding stack location has been used.
   This vector is used to prevent a function call within an argument from
   clobbering any stack already set up.  */
static char *stack_usage_map;

/* Size of STACK_USAGE_MAP.  */
static int highest_outgoing_arg_in_use;

/* stack_arg_under_construction is nonzero when an argument may be
   initialized with a constructor call (including a C function that
   returns a BLKmode struct) and expand_call must take special action
   to make sure the object being constructed does not overlap the
   argument list for the constructor call.  */
int stack_arg_under_construction;
#endif

static int calls_function	PROTO((tree, int));
static void emit_call_1		PROTO((rtx, tree, int, int, rtx, rtx, int,
				       rtx, int));
static void store_one_arg	PROTO ((struct arg_data *, rtx, int, int,
					tree, int));

/* If WHICH is 1, return 1 if EXP contains a call to the built-in function
   `alloca'.

   If WHICH is 0, return 1 if EXP contains a call to any function.
   Actually, we only need return 1 if evaluating EXP would require pushing
   arguments on the stack, but that is too difficult to compute, so we just
   assume any function call might require the stack.  */

static int
calls_function (exp, which)
     tree exp;
     int which;
{
  register int i;
  int type = TREE_CODE_CLASS (TREE_CODE (exp));
  int length = tree_code_length[(int) TREE_CODE (exp)];

  /* Only expressions and references can contain calls.  */

  if (type != 'e' && type != '<' && type != '1' && type != '2' && type != 'r'
      && type != 'b')
    return 0;

  switch (TREE_CODE (exp))
    {
    case CALL_EXPR:
      if (which == 0)
	return 1;
      else if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	       && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		   == FUNCTION_DECL)
	       && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	       && (DECL_FUNCTION_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		   == BUILT_IN_ALLOCA))
	return 1;

      /* Third operand is RTL.  */
      length = 2;
      break;

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) != 0)
	return 0;
      break;

    case BLOCK:
      {
	register tree local;

	for (local = BLOCK_VARS (exp); local; local = TREE_CHAIN (local))
	  if (DECL_INITIAL (local) != 0
	      && calls_function (DECL_INITIAL (local), which))
	    return 1;
      }
      {
	register tree subblock;

	for (subblock = BLOCK_SUBBLOCKS (exp);
	     subblock;
	     subblock = TREE_CHAIN (subblock))
	  if (calls_function (subblock, which))
	    return 1;
      }
      return 0;

    case METHOD_CALL_EXPR:
      length = 3;
      break;

    case WITH_CLEANUP_EXPR:
      length = 1;
      break;

    case RTL_EXPR:
      return 0;
    }

  for (i = 0; i < length; i++)
    if (TREE_OPERAND (exp, i) != 0
	&& calls_function (TREE_OPERAND (exp, i), which))
      return 1;

  return 0;
}

/* Force FUNEXP into a form suitable for the address of a CALL,
   and return that as an rtx.  Also load the static chain register
   if FNDECL is a nested function.

   USE_INSNS points to a variable holding a chain of USE insns
   to which a USE of the static chain
   register should be added, if required.  */

rtx
prepare_call_address (funexp, fndecl, use_insns)
     rtx funexp;
     tree fndecl;
     rtx *use_insns;
{
  rtx static_chain_value = 0;

  funexp = protect_from_queue (funexp, 0);

  if (fndecl != 0)
    /* Get possible static chain value for nested function in C. */
    static_chain_value = lookup_static_chain (fndecl);

  /* Make a valid memory address and copy constants thru pseudo-regs,
     but not for a constant address if -fno-function-cse.  */
  if (GET_CODE (funexp) != SYMBOL_REF)
    funexp = memory_address (FUNCTION_MODE, funexp);
  else
    {
#ifndef NO_FUNCTION_CSE
      if (optimize && ! flag_no_function_cse)
#ifdef NO_RECURSIVE_FUNCTION_CSE
	if (fndecl != current_function_decl)
#endif
	  funexp = force_reg (Pmode, funexp);
#endif
    }

  if (static_chain_value != 0)
    {
      emit_move_insn (static_chain_rtx, static_chain_value);

      /* Put the USE insn in the chain we were passed.  It will later be
	 output immediately in front of the CALL insn.  */
      push_to_sequence (*use_insns);
      emit_insn (gen_rtx (USE, VOIDmode, static_chain_rtx));
      *use_insns = get_insns ();
      end_sequence ();
    }

  return funexp;
}

/* Generate instructions to call function FUNEXP,
   and optionally pop the results.
   The CALL_INSN is the first insn generated.

   FUNTYPE is the data type of the function, or, for a library call,
   the identifier for the name of the call.  This is given to the
   macro RETURN_POPS_ARGS to determine whether this function pops its own args.

   STACK_SIZE is the number of bytes of arguments on the stack,
   rounded up to STACK_BOUNDARY; zero if the size is variable.
   This is both to put into the call insn and
   to generate explicit popping code if necessary.

   STRUCT_VALUE_SIZE is the number of bytes wanted in a structure value.
   It is zero if this call doesn't want a structure value.

   NEXT_ARG_REG is the rtx that results from executing
     FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1)
   just after all the args have had their registers assigned.
   This could be whatever you like, but normally it is the first
   arg-register beyond those used for args in this call,
   or 0 if all the arg-registers are used in this call.
   It is passed on to `gen_call' so you can put this info in the call insn.

   VALREG is a hard register in which a value is returned,
   or 0 if the call does not return a value.

   OLD_INHIBIT_DEFER_POP is the value that `inhibit_defer_pop' had before
   the args to this call were processed.
   We restore `inhibit_defer_pop' to that value.

   USE_INSNS is a chain of USE insns to be emitted immediately before
   the actual CALL insn.

   IS_CONST is true if this is a `const' call.  */

static void
emit_call_1 (funexp, funtype, stack_size, struct_value_size, next_arg_reg,
	     valreg, old_inhibit_defer_pop, use_insns, is_const)
     rtx funexp;
     tree funtype;
     int stack_size;
     int struct_value_size;
     rtx next_arg_reg;
     rtx valreg;
     int old_inhibit_defer_pop;
     rtx use_insns;
     int is_const;
{
  rtx stack_size_rtx = GEN_INT (stack_size);
  rtx struct_value_size_rtx = GEN_INT (struct_value_size);
  rtx call_insn;
  int already_popped = 0;

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (funexp) != SYMBOL_REF)
    funexp = memory_address (FUNCTION_MODE, funexp);

#ifndef ACCUMULATE_OUTGOING_ARGS
#if defined (HAVE_call_pop) && defined (HAVE_call_value_pop)
  if (HAVE_call_pop && HAVE_call_value_pop
      && (RETURN_POPS_ARGS (funtype, stack_size) > 0 || stack_size == 0))
    {
      rtx n_pop = GEN_INT (RETURN_POPS_ARGS (funtype, stack_size));
      rtx pat;

      /* If this subroutine pops its own args, record that in the call insn
	 if possible, for the sake of frame pointer elimination.  */
      if (valreg)
	pat = gen_call_value_pop (valreg,
				  gen_rtx (MEM, FUNCTION_MODE, funexp),
				  stack_size_rtx, next_arg_reg, n_pop);
      else
	pat = gen_call_pop (gen_rtx (MEM, FUNCTION_MODE, funexp),
			    stack_size_rtx, next_arg_reg, n_pop);

      emit_call_insn (pat);
      already_popped = 1;
    }
  else
#endif
#endif

#if defined (HAVE_call) && defined (HAVE_call_value)
  if (HAVE_call && HAVE_call_value)
    {
      if (valreg)
	emit_call_insn (gen_call_value (valreg,
					gen_rtx (MEM, FUNCTION_MODE, funexp),
					stack_size_rtx, next_arg_reg,
					NULL_RTX));
      else
	emit_call_insn (gen_call (gen_rtx (MEM, FUNCTION_MODE, funexp),
				  stack_size_rtx, next_arg_reg,
				  struct_value_size_rtx));
    }
  else
#endif
    abort ();

  /* Find the CALL insn we just emitted and write the USE insns before it.  */
  for (call_insn = get_last_insn ();
       call_insn && GET_CODE (call_insn) != CALL_INSN;
       call_insn = PREV_INSN (call_insn))
    ;

  if (! call_insn)
    abort ();

  /* Put the USE insns before the CALL.  */
  emit_insns_before (use_insns, call_insn);

  /* If this is a const call, then set the insn's unchanging bit.  */
  if (is_const)
    CONST_CALL_P (call_insn) = 1;

  /* Restore this now, so that we do defer pops for this call's args
     if the context of the call as a whole permits.  */
  inhibit_defer_pop = old_inhibit_defer_pop;

#ifndef ACCUMULATE_OUTGOING_ARGS
  /* If returning from the subroutine does not automatically pop the args,
     we need an instruction to pop them sooner or later.
     Perhaps do it now; perhaps just record how much space to pop later.

     If returning from the subroutine does pop the args, indicate that the
     stack pointer will be changed.  */

  if (stack_size != 0 && RETURN_POPS_ARGS (funtype, stack_size) > 0)
    {
      if (!already_popped)
	emit_insn (gen_rtx (CLOBBER, VOIDmode, stack_pointer_rtx));
      stack_size -= RETURN_POPS_ARGS (funtype, stack_size);
      stack_size_rtx = GEN_INT (stack_size);
    }

  if (stack_size != 0)
    {
      if (flag_defer_pop && inhibit_defer_pop == 0 && !is_const)
	pending_stack_adjust += stack_size;
      else
	adjust_stack (stack_size_rtx);
    }
#endif
}

/* Generate all the code for a function call
   and return an rtx for its value.
   Store the value in TARGET (specified as an rtx) if convenient.
   If the value is stored in TARGET then TARGET is returned.
   If IGNORE is nonzero, then we ignore the value of the function call.  */

rtx
expand_call (exp, target, ignore)
     tree exp;
     rtx target;
     int ignore;
{
  /* List of actual parameters.  */
  tree actparms = TREE_OPERAND (exp, 1);
  /* RTX for the function to be called.  */
  rtx funexp;
  /* Tree node for the function to be called (not the address!).  */
  tree funtree;
  /* Data type of the function.  */
  tree funtype;
  /* Declaration of the function being called,
     or 0 if the function is computed (not known by name).  */
  tree fndecl = 0;
  char *name = 0;

  /* Register in which non-BLKmode value will be returned,
     or 0 if no value or if value is BLKmode.  */
  rtx valreg;
  /* Address where we should return a BLKmode value;
     0 if value not BLKmode.  */
  rtx structure_value_addr = 0;
  /* Nonzero if that address is being passed by treating it as
     an extra, implicit first parameter.  Otherwise,
     it is passed by being copied directly into struct_value_rtx.  */
  int structure_value_addr_parm = 0;
  /* Size of aggregate value wanted, or zero if none wanted
     or if we are using the non-reentrant PCC calling convention
     or expecting the value in registers.  */
  int struct_value_size = 0;
  /* Nonzero if called function returns an aggregate in memory PCC style,
     by returning the address of where to find it.  */
  int pcc_struct_value = 0;

  /* Number of actual parameters in this call, including struct value addr.  */
  int num_actuals;
  /* Number of named args.  Args after this are anonymous ones
     and they must all go on the stack.  */
  int n_named_args;
  /* Count arg position in order args appear.  */
  int argpos;

  /* Vector of information about each argument.
     Arguments are numbered in the order they will be pushed,
     not the order they are written.  */
  struct arg_data *args;

  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  struct args_size original_args_size;
  /* Data on reg parms scanned so far.  */
  CUMULATIVE_ARGS args_so_far;
  /* Nonzero if a reg parm has been scanned.  */
  int reg_parm_seen;
  /* Nonzero if this is an indirect function call.  */
  int current_call_is_indirect = 0;

  /* Nonzero if we must avoid push-insns in the args for this call. 
     If stack space is allocated for register parameters, but not by the
     caller, then it is preallocated in the fixed part of the stack frame.
     So the entire argument block must then be preallocated (i.e., we
     ignore PUSH_ROUNDING in that case).  */

#if defined(REG_PARM_STACK_SPACE) && ! defined(OUTGOING_REG_PARM_STACK_SPACE)
  int must_preallocate = 1;
#else
#ifdef PUSH_ROUNDING
  int must_preallocate = 0;
#else
  int must_preallocate = 1;
#endif
#endif

  /* Size of the stack reserved for parameter registers.  */
  int reg_parm_stack_space = 0;

  /* 1 if scanning parms front to back, -1 if scanning back to front.  */
  int inc;
  /* Address of space preallocated for stack parms
     (on machines that lack push insns), or 0 if space not preallocated.  */
  rtx argblock = 0;

  /* Nonzero if it is plausible that this is a call to alloca.  */
  int may_be_alloca;
  /* Nonzero if this is a call to setjmp or a related function.  */
  int returns_twice;
  /* Nonzero if this is a call to `longjmp'.  */
  int is_longjmp;
  /* Nonzero if this is a call to an inline function.  */
  int is_integrable = 0;
  /* Nonzero if this is a call to a `const' function.
     Note that only explicitly named functions are handled as `const' here.  */
  int is_const = 0;
  /* Nonzero if this is a call to a `volatile' function.  */
  int is_volatile = 0;
#if defined(ACCUMULATE_OUTGOING_ARGS) && defined(REG_PARM_STACK_SPACE)
  /* Define the boundary of the register parm stack space that needs to be
     save, if any.  */
  int low_to_save = -1, high_to_save;
  rtx save_area = 0;		/* Place that it is saved */
#endif

#ifdef ACCUMULATE_OUTGOING_ARGS
  int initial_highest_arg_in_use = highest_outgoing_arg_in_use;
  char *initial_stack_usage_map = stack_usage_map;
#endif

  rtx old_stack_level = 0;
  int old_pending_adj;
  int old_stack_arg_under_construction;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  tree old_cleanups = cleanups_this_call;

  rtx use_insns = 0;

  register tree p;
  register int i, j;

  /* See if we can find a DECL-node for the actual function.
     As a result, decide whether this is a call to an integrable function.  */

  p = TREE_OPERAND (exp, 0);
  if (TREE_CODE (p) == ADDR_EXPR)
    {
      fndecl = TREE_OPERAND (p, 0);
      if (TREE_CODE (fndecl) != FUNCTION_DECL)
	{
	  /* May still be a `const' function if it is
	     a call through a pointer-to-const.
	     But we don't handle that.  */
	  fndecl = 0;
	}
      else
	{
	  if (!flag_no_inline
	      && fndecl != current_function_decl
	      && DECL_SAVED_INSNS (fndecl))
	    is_integrable = 1;
	  else if (! TREE_ADDRESSABLE (fndecl))
	    {
	      /* In case this function later becomes inlinable,
		 record that there was already a non-inline call to it.

		 Use abstraction instead of setting TREE_ADDRESSABLE
		 directly.  */
	      if (DECL_INLINE (fndecl) && extra_warnings && warn_inline
		  && !flag_no_inline)
		warning_with_decl (fndecl, "can't inline call to `%s' which was declared inline");
	      mark_addressable (fndecl);
	    }

	  if (TREE_READONLY (fndecl) && ! TREE_THIS_VOLATILE (fndecl)
	      && TYPE_MODE (TREE_TYPE (exp)) != VOIDmode)
	    is_const = 1;
	}
    }

  is_volatile = TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (p)));

#ifdef REG_PARM_STACK_SPACE
#ifdef MAYBE_REG_PARM_STACK_SPACE
  reg_parm_stack_space = MAYBE_REG_PARM_STACK_SPACE;
#else
  reg_parm_stack_space = REG_PARM_STACK_SPACE (fndecl);
#endif
#endif

  /* Warn if this value is an aggregate type,
     regardless of which calling convention we are using for it.  */
  if (warn_aggregate_return
      && (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == QUAL_UNION_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE))
    warning ("function call has aggregate value");

  /* Set up a place to return a structure.  */

  /* Cater to broken compilers.  */
  if (aggregate_value_p (exp))
    {
      /* This call returns a big structure.  */
      is_const = 0;

#ifdef PCC_STATIC_STRUCT_RETURN
      {
	pcc_struct_value = 1;
	is_integrable = 0;  /* Easier than making that case work right.  */
      }
#else /* not PCC_STATIC_STRUCT_RETURN */
      {
	struct_value_size = int_size_in_bytes (TREE_TYPE (exp));

	if (struct_value_size < 0)
	  abort ();

	if (target && GET_CODE (target) == MEM)
	  structure_value_addr = XEXP (target, 0);
	else
	  {
	    /* Assign a temporary on the stack to hold the value.  */

	    /* For variable-sized objects, we must be called with a target
	       specified.  If we were to allocate space on the stack here,
	       we would have no way of knowing when to free it.  */

	    structure_value_addr
	      = XEXP (assign_stack_temp (BLKmode, struct_value_size, 1), 0);
	    target = 0;
	  }
      }
#endif /* not PCC_STATIC_STRUCT_RETURN */
    }

  /* If called function is inline, try to integrate it.  */

  if (is_integrable)
    {
      rtx temp;
      rtx before_call = get_last_insn ();

      temp = expand_inline_function (fndecl, actparms, target,
				     ignore, TREE_TYPE (exp),
				     structure_value_addr);

      /* If inlining succeeded, return.  */
      if ((HOST_WIDE_INT) temp != -1)
	{
	  /* Perform all cleanups needed for the arguments of this call
	     (i.e. destructors in C++).  It is ok if these destructors
	     clobber RETURN_VALUE_REG, because the only time we care about
	     this is when TARGET is that register.  But in C++, we take
	     care to never return that register directly.  */
	  expand_cleanups_to (old_cleanups);

#ifdef ACCUMULATE_OUTGOING_ARGS
	  /* If the outgoing argument list must be preserved, push
	     the stack before executing the inlined function if it
	     makes any calls.  */

	  for (i = reg_parm_stack_space - 1; i >= 0; i--)
	    if (i < highest_outgoing_arg_in_use && stack_usage_map[i] != 0)
	      break;

	  if (stack_arg_under_construction || i >= 0)
	    {
	      rtx insn = NEXT_INSN (before_call), seq;

	      /* Look for a call in the inline function code.
		 If OUTGOING_ARGS_SIZE (DECL_SAVED_INSNS (fndecl)) is
		 nonzero then there is a call and it is not necessary
		 to scan the insns.  */

	      if (OUTGOING_ARGS_SIZE (DECL_SAVED_INSNS (fndecl)) == 0)
		for (; insn; insn = NEXT_INSN (insn))
		  if (GET_CODE (insn) == CALL_INSN)
		    break;

	      if (insn)
		{
		  /* Reserve enough stack space so that the largest
		     argument list of any function call in the inline
		     function does not overlap the argument list being
		     evaluated.  This is usually an overestimate because
		     allocate_dynamic_stack_space reserves space for an
		     outgoing argument list in addition to the requested
		     space, but there is no way to ask for stack space such
		     that an argument list of a certain length can be
		     safely constructed.  */

		  int adjust = OUTGOING_ARGS_SIZE (DECL_SAVED_INSNS (fndecl));
#ifdef REG_PARM_STACK_SPACE
		  /* Add the stack space reserved for register arguments
		     in the inline function.  What is really needed is the
		     largest value of reg_parm_stack_space in the inline
		     function, but that is not available.  Using the current
		     value of reg_parm_stack_space is wrong, but gives
		     correct results on all supported machines.  */
		  adjust += reg_parm_stack_space;
#endif
		  start_sequence ();
		  emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);
		  allocate_dynamic_stack_space (GEN_INT (adjust),
						NULL_RTX, BITS_PER_UNIT);
		  seq = get_insns ();
		  end_sequence ();
		  emit_insns_before (seq, NEXT_INSN (before_call));
		  emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);
		}
	    }
#endif

	  /* If the result is equivalent to TARGET, return TARGET to simplify
	     checks in store_expr.  They can be equivalent but not equal in the
	     case of a function that returns BLKmode.  */
	  if (temp != target && rtx_equal_p (temp, target))
	    return target;
	  return temp;
	}

      /* If inlining failed, mark FNDECL as needing to be compiled
	 separately after all.  */
      mark_addressable (fndecl);
    }

  /* When calling a const function, we must pop the stack args right away,
     so that the pop is deleted or moved with the call.  */
  if (is_const)
    NO_DEFER_POP;

  function_call_count++;

  if (fndecl && DECL_NAME (fndecl))
    name = IDENTIFIER_POINTER (DECL_NAME (fndecl));

  /* On some machines (such as the PA) indirect calls have a different
     calling convention than normal calls.  FUNCTION_ARG in the target
     description can look at current_call_is_indirect to determine which
     calling convention to use.  */
  current_call_is_indirect = (fndecl == 0);
#if 0
    = TREE_CODE (TREE_OPERAND (exp, 0)) == NON_LVALUE_EXPR ? 1 : 0;
#endif

#if 0
  /* Unless it's a call to a specific function that isn't alloca,
     if it has one argument, we must assume it might be alloca.  */

  may_be_alloca =
    (!(fndecl != 0 && strcmp (name, "alloca"))
     && actparms != 0
     && TREE_CHAIN (actparms) == 0);
#else
  /* We assume that alloca will always be called by name.  It
     makes no sense to pass it as a pointer-to-function to
     anything that does not understand its behavior.  */
  may_be_alloca =
    (name && ((IDENTIFIER_LENGTH (DECL_NAME (fndecl)) == 6
		 && name[0] == 'a'
		 && ! strcmp (name, "alloca"))
		|| (IDENTIFIER_LENGTH (DECL_NAME (fndecl)) == 16
		    && name[0] == '_'
		    && ! strcmp (name, "__builtin_alloca"))));
#endif

  /* See if this is a call to a function that can return more than once
     or a call to longjmp.  */

  returns_twice = 0;
  is_longjmp = 0;

  if (name != 0 && IDENTIFIER_LENGTH (DECL_NAME (fndecl)) <= 15)
    {
      char *tname = name;

      if (name[0] == '_')
	tname += ((name[1] == '_' && name[2] == 'x') ? 3 : 1);

      if (tname[0] == 's')
	{
	  returns_twice
	    = ((tname[1] == 'e'
		&& (! strcmp (tname, "setjmp")
		    || ! strcmp (tname, "setjmp_syscall")))
	       || (tname[1] == 'i'
		   && ! strcmp (tname, "sigsetjmp"))
	       || (tname[1] == 'a'
		   && ! strcmp (tname, "savectx")));
	  if (tname[1] == 'i'
	      && ! strcmp (tname, "siglongjmp"))
	    is_longjmp = 1;
	}
      else if ((tname[0] == 'q' && tname[1] == 's'
		&& ! strcmp (tname, "qsetjmp"))
	       || (tname[0] == 'v' && tname[1] == 'f'
		   && ! strcmp (tname, "vfork")))
	returns_twice = 1;

      else if (tname[0] == 'l' && tname[1] == 'o'
	       && ! strcmp (tname, "longjmp"))
	is_longjmp = 1;
    }

  if (may_be_alloca)
    current_function_calls_alloca = 1;

  /* Don't let pending stack adjusts add up to too much.
     Also, do all pending adjustments now
     if there is any chance this might be a call to alloca.  */

  if (pending_stack_adjust >= 32
      || (pending_stack_adjust > 0 && may_be_alloca))
    do_pending_stack_adjust ();

  /* Operand 0 is a pointer-to-function; get the type of the function.  */
  funtype = TREE_TYPE (TREE_OPERAND (exp, 0));
  if (TREE_CODE (funtype) != POINTER_TYPE)
    abort ();
  funtype = TREE_TYPE (funtype);

  /* Push the temporary stack slot level so that we can free temporaries used
     by each of the arguments separately.  */
  push_temp_slots ();

  /* Start updating where the next arg would go.  */
  INIT_CUMULATIVE_ARGS (args_so_far, funtype, NULL_RTX);

  /* If struct_value_rtx is 0, it means pass the address
     as if it were an extra parameter.  */
  if (structure_value_addr && struct_value_rtx == 0)
    {
#ifdef ACCUMULATE_OUTGOING_ARGS
      /* If the stack will be adjusted, make sure the structure address
	 does not refer to virtual_outgoing_args_rtx.  */
      rtx temp = (stack_arg_under_construction
		  ? copy_addr_to_reg (structure_value_addr)
		  : force_reg (Pmode, structure_value_addr));
#else
      rtx temp = force_reg (Pmode, structure_value_addr);
#endif

      actparms
	= tree_cons (error_mark_node,
		     make_tree (build_pointer_type (TREE_TYPE (funtype)),
				temp),
		     actparms);
      structure_value_addr_parm = 1;
    }

  /* Count the arguments and set NUM_ACTUALS.  */
  for (p = actparms, i = 0; p; p = TREE_CHAIN (p)) i++;
  num_actuals = i;

  /* Compute number of named args.
     Normally, don't include the last named arg if anonymous args follow.
     (If no anonymous args follow, the result of list_length
     is actually one too large.)

     If SETUP_INCOMING_VARARGS is defined, this machine will be able to
     place unnamed args that were passed in registers into the stack.  So
     treat all args as named.  This allows the insns emitting for a specific
     argument list to be independent of the function declaration.

     If SETUP_INCOMING_VARARGS is not defined, we do not have any reliable
     way to pass unnamed args in registers, so we must force them into
     memory.  */
#ifndef SETUP_INCOMING_VARARGS
  if (TYPE_ARG_TYPES (funtype) != 0)
    n_named_args
      = list_length (TYPE_ARG_TYPES (funtype)) - 1
	/* Count the struct value address, if it is passed as a parm.  */
	+ structure_value_addr_parm;
  else
#endif
    /* If we know nothing, treat all args as named.  */
    n_named_args = num_actuals;

  /* Make a vector to hold all the information about each arg.  */
  args = (struct arg_data *) alloca (num_actuals * sizeof (struct arg_data));
  bzero (args, num_actuals * sizeof (struct arg_data));

  args_size.constant = 0;
  args_size.var = 0;

  /* In this loop, we consider args in the order they are written.
     We fill up ARGS from the front of from the back if necessary
     so that in any case the first arg to be pushed ends up at the front.  */

#ifdef PUSH_ARGS_REVERSED
  i = num_actuals - 1, inc = -1;
  /* In this case, must reverse order of args
     so that we compute and push the last arg first.  */
#else
  i = 0, inc = 1;
#endif

  /* I counts args in order (to be) pushed; ARGPOS counts in order written.  */
  for (p = actparms, argpos = 0; p; p = TREE_CHAIN (p), i += inc, argpos++)
    {
      tree type = TREE_TYPE (TREE_VALUE (p));
      enum machine_mode mode;

      args[i].tree_value = TREE_VALUE (p);

      /* Replace erroneous argument with constant zero.  */
      if (type == error_mark_node || TYPE_SIZE (type) == 0)
	args[i].tree_value = integer_zero_node, type = integer_type_node;

      /* Decide where to pass this arg.

	 args[i].reg is nonzero if all or part is passed in registers.

	 args[i].partial is nonzero if part but not all is passed in registers,
	 and the exact value says how many words are passed in registers.

	 args[i].pass_on_stack is nonzero if the argument must at least be
	 computed on the stack.  It may then be loaded back into registers
	 if args[i].reg is nonzero.

	 These decisions are driven by the FUNCTION_... macros and must agree
	 with those made by function.c.  */

#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
      /* See if this argument should be passed by invisible reference.  */
      if (FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, TYPE_MODE (type), type,
					  argpos < n_named_args))
	{
#ifdef FUNCTION_ARG_CALLEE_COPIES
	  if (FUNCTION_ARG_CALLEE_COPIES (args_so_far, TYPE_MODE (type), type,
					  argpos < n_named_args)
	      /* If it's in a register, we must make a copy of it too.  */
	      /* ??? Is this a sufficient test?  Is there a better one? */
	      && !(TREE_CODE (args[i].tree_value) == VAR_DECL
		   && REG_P (DECL_RTL (args[i].tree_value))))
	    {
	      args[i].tree_value = build1 (ADDR_EXPR,
					   build_pointer_type (type),
					   args[i].tree_value);
	      type = build_pointer_type (type);
	    }
	  else
#endif
	    {
	      /* We make a copy of the object and pass the address to the
		 function being called.  */
	      rtx copy;

	      if (TYPE_SIZE (type) == 0
		  || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
		{
		  /* This is a variable-sized object.  Make space on the stack
		     for it.  */
		  rtx size_rtx = expr_size (TREE_VALUE (p));

		  if (old_stack_level == 0)
		    {
		      emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);
		      old_pending_adj = pending_stack_adjust;
		      pending_stack_adjust = 0;
		    }

		  copy = gen_rtx (MEM, BLKmode,
				  allocate_dynamic_stack_space (size_rtx,
								NULL_RTX,
								TYPE_ALIGN (type)));
		}
	      else
		{
		  int size = int_size_in_bytes (type);
		  copy = assign_stack_temp (TYPE_MODE (type), size, 1);
		}

	      store_expr (args[i].tree_value, copy, 0);

	      args[i].tree_value = build1 (ADDR_EXPR,
					   build_pointer_type (type),
					   make_tree (type, copy));
	      type = build_pointer_type (type);
	    }
	}
#endif /* FUNCTION_ARG_PASS_BY_REFERENCE */

      mode = TYPE_MODE (type);

#ifdef PROMOTE_FUNCTION_ARGS
      /* Compute the mode in which the arg is actually to be extended to.  */
      if (TREE_CODE (type) == INTEGER_TYPE || TREE_CODE (type) == ENUMERAL_TYPE
	  || TREE_CODE (type) == BOOLEAN_TYPE || TREE_CODE (type) == CHAR_TYPE
	  || TREE_CODE (type) == REAL_TYPE || TREE_CODE (type) == POINTER_TYPE
	  || TREE_CODE (type) == OFFSET_TYPE)
	{
	  int unsignedp = TREE_UNSIGNED (type);
	  PROMOTE_MODE (mode, unsignedp, type);
	  args[i].unsignedp = unsignedp;
	}
#endif

      args[i].mode = mode;
      args[i].reg = FUNCTION_ARG (args_so_far, mode, type,
				  argpos < n_named_args);
#ifdef FUNCTION_ARG_PARTIAL_NREGS
      if (args[i].reg)
	args[i].partial
	  = FUNCTION_ARG_PARTIAL_NREGS (args_so_far, mode, type,
					argpos < n_named_args);
#endif

      args[i].pass_on_stack = MUST_PASS_IN_STACK (mode, type);

      /* If FUNCTION_ARG returned an (expr_list (nil) FOO), it means that
	 we are to pass this arg in the register(s) designated by FOO, but
	 also to pass it in the stack.  */
      if (args[i].reg && GET_CODE (args[i].reg) == EXPR_LIST
	  && XEXP (args[i].reg, 0) == 0)
	args[i].pass_on_stack = 1, args[i].reg = XEXP (args[i].reg, 1);

      /* If this is an addressable type, we must preallocate the stack
	 since we must evaluate the object into its final location.

	 If this is to be passed in both registers and the stack, it is simpler
	 to preallocate.  */
      if (TREE_ADDRESSABLE (type)
	  || (args[i].pass_on_stack && args[i].reg != 0))
	must_preallocate = 1;

      /* If this is an addressable type, we cannot pre-evaluate it.  Thus,
	 we cannot consider this function call constant.  */
      if (TREE_ADDRESSABLE (type))
	is_const = 0;

      /* Compute the stack-size of this argument.  */
      if (args[i].reg == 0 || args[i].partial != 0
#ifdef REG_PARM_STACK_SPACE
	  || reg_parm_stack_space > 0
#endif
	  || args[i].pass_on_stack)
	locate_and_pad_parm (mode, type,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			     1,
#else
			     args[i].reg != 0,
#endif
			     fndecl, &args_size, &args[i].offset,
			     &args[i].size);

#ifndef ARGS_GROW_DOWNWARD
      args[i].slot_offset = args_size;
#endif

#ifndef REG_PARM_STACK_SPACE
      /* If a part of the arg was put into registers,
	 don't include that part in the amount pushed.  */
      if (! args[i].pass_on_stack)
	args[i].size.constant -= ((args[i].partial * UNITS_PER_WORD)
				  / (PARM_BOUNDARY / BITS_PER_UNIT)
				  * (PARM_BOUNDARY / BITS_PER_UNIT));
#endif
      
      /* Update ARGS_SIZE, the total stack space for args so far.  */

      args_size.constant += args[i].size.constant;
      if (args[i].size.var)
	{
	  ADD_PARM_SIZE (args_size, args[i].size.var);
	}

      /* Since the slot offset points to the bottom of the slot,
	 we must record it after incrementing if the args grow down.  */
#ifdef ARGS_GROW_DOWNWARD
      args[i].slot_offset = args_size;

      args[i].slot_offset.constant = -args_size.constant;
      if (args_size.var)
	{
	  SUB_PARM_SIZE (args[i].slot_offset, args_size.var);
	}
#endif

      /* Increment ARGS_SO_FAR, which has info about which arg-registers
	 have been used, etc.  */

      FUNCTION_ARG_ADVANCE (args_so_far, TYPE_MODE (type), type,
			    argpos < n_named_args);
    }

#ifdef FINAL_REG_PARM_STACK_SPACE
  reg_parm_stack_space = FINAL_REG_PARM_STACK_SPACE (args_size.constant,
						     args_size.var);
#endif
      
  /* Compute the actual size of the argument block required.  The variable
     and constant sizes must be combined, the size may have to be rounded,
     and there may be a minimum required size.  */

  original_args_size = args_size;
  if (args_size.var)
    {
      /* If this function requires a variable-sized argument list, don't try to
	 make a cse'able block for this call.  We may be able to do this
	 eventually, but it is too complicated to keep track of what insns go
	 in the cse'able block and which don't.  */

      is_const = 0;
      must_preallocate = 1;

      args_size.var = ARGS_SIZE_TREE (args_size);
      args_size.constant = 0;

#ifdef STACK_BOUNDARY
      if (STACK_BOUNDARY != BITS_PER_UNIT)
	args_size.var = round_up (args_size.var, STACK_BYTES);
#endif

#ifdef REG_PARM_STACK_SPACE
      if (reg_parm_stack_space > 0)
	{
	  args_size.var
	    = size_binop (MAX_EXPR, args_size.var,
			  size_int (REG_PARM_STACK_SPACE (fndecl)));

#ifndef OUTGOING_REG_PARM_STACK_SPACE
	  /* The area corresponding to register parameters is not to count in
	     the size of the block we need.  So make the adjustment.  */
	  args_size.var
	    = size_binop (MINUS_EXPR, args_size.var,
			  size_int (reg_parm_stack_space));
#endif
	}
#endif
    }
  else
    {
#ifdef STACK_BOUNDARY
      args_size.constant = (((args_size.constant + (STACK_BYTES - 1))
			     / STACK_BYTES) * STACK_BYTES);
#endif

#ifdef REG_PARM_STACK_SPACE
      args_size.constant = MAX (args_size.constant,
				reg_parm_stack_space);
#ifdef MAYBE_REG_PARM_STACK_SPACE
      if (reg_parm_stack_space == 0)
	args_size.constant = 0;
#endif
#ifndef OUTGOING_REG_PARM_STACK_SPACE
      args_size.constant -= reg_parm_stack_space;
#endif
#endif
    }

  /* See if we have or want to preallocate stack space.

     If we would have to push a partially-in-regs parm
     before other stack parms, preallocate stack space instead.

     If the size of some parm is not a multiple of the required stack
     alignment, we must preallocate.

     If the total size of arguments that would otherwise create a copy in
     a temporary (such as a CALL) is more than half the total argument list
     size, preallocation is faster.

     Another reason to preallocate is if we have a machine (like the m88k)
     where stack alignment is required to be maintained between every
     pair of insns, not just when the call is made.  However, we assume here
     that such machines either do not have push insns (and hence preallocation
     would occur anyway) or the problem is taken care of with
     PUSH_ROUNDING.  */

  if (! must_preallocate)
    {
      int partial_seen = 0;
      int copy_to_evaluate_size = 0;

      for (i = 0; i < num_actuals && ! must_preallocate; i++)
	{
	  if (args[i].partial > 0 && ! args[i].pass_on_stack)
	    partial_seen = 1;
	  else if (partial_seen && args[i].reg == 0)
	    must_preallocate = 1;

	  if (TYPE_MODE (TREE_TYPE (args[i].tree_value)) == BLKmode
	      && (TREE_CODE (args[i].tree_value) == CALL_EXPR
		  || TREE_CODE (args[i].tree_value) == TARGET_EXPR
		  || TREE_CODE (args[i].tree_value) == COND_EXPR
		  || TREE_ADDRESSABLE (TREE_TYPE (args[i].tree_value))))
	    copy_to_evaluate_size
	      += int_size_in_bytes (TREE_TYPE (args[i].tree_value));
	}

      if (copy_to_evaluate_size * 2 >= args_size.constant
	  && args_size.constant > 0)
	must_preallocate = 1;
    }

  /* If the structure value address will reference the stack pointer, we must
     stabilize it.  We don't need to do this if we know that we are not going
     to adjust the stack pointer in processing this call.  */

  if (structure_value_addr
      && (reg_mentioned_p (virtual_stack_dynamic_rtx, structure_value_addr)
       || reg_mentioned_p (virtual_outgoing_args_rtx, structure_value_addr))
      && (args_size.var
#ifndef ACCUMULATE_OUTGOING_ARGS
	  || args_size.constant
#endif
	  ))
    structure_value_addr = copy_to_reg (structure_value_addr);

  /* If this function call is cse'able, precompute all the parameters.
     Note that if the parameter is constructed into a temporary, this will
     cause an additional copy because the parameter will be constructed
     into a temporary location and then copied into the outgoing arguments.
     If a parameter contains a call to alloca and this function uses the
     stack, precompute the parameter.  */

  /* If we preallocated the stack space, and some arguments must be passed
     on the stack, then we must precompute any parameter which contains a
     function call which will store arguments on the stack.
     Otherwise, evaluating the parameter may clobber previous parameters
     which have already been stored into the stack.  */

  for (i = 0; i < num_actuals; i++)
    if (is_const
	|| ((args_size.var != 0 || args_size.constant != 0)
	    && calls_function (args[i].tree_value, 1))
	|| (must_preallocate && (args_size.var != 0 || args_size.constant != 0)
	    && calls_function (args[i].tree_value, 0)))
      {
	args[i].initial_value = args[i].value
	  = expand_expr (args[i].tree_value, NULL_RTX, VOIDmode, 0);

	if (GET_MODE (args[i].value ) != VOIDmode
	    && GET_MODE (args[i].value) != args[i].mode)
	  args[i].value = convert_to_mode (args[i].mode, args[i].value,
					   args[i].unsignedp);
	preserve_temp_slots (args[i].value);

	free_temp_slots ();

	/* ANSI doesn't require a sequence point here,
	   but PCC has one, so this will avoid some problems.  */
	emit_queue ();
      }

  /* Now we are about to start emitting insns that can be deleted
     if a libcall is deleted.  */
  if (is_const)
    start_sequence ();

  /* If we have no actual push instructions, or shouldn't use them,
     make space for all args right now.  */

  if (args_size.var != 0)
    {
      if (old_stack_level == 0)
	{
	  emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);
	  old_pending_adj = pending_stack_adjust;
	  pending_stack_adjust = 0;
#ifdef ACCUMULATE_OUTGOING_ARGS
	  /* stack_arg_under_construction says whether a stack arg is
	     being constructed at the old stack level.  Pushing the stack
	     gets a clean outgoing argument block.  */
	  old_stack_arg_under_construction = stack_arg_under_construction;
	  stack_arg_under_construction = 0;
#endif
	}
      argblock = push_block (ARGS_SIZE_RTX (args_size), 0, 0);
    }
  else if (must_preallocate)
    {
      /* Note that we must go through the motions of allocating an argument
	 block even if the size is zero because we may be storing args
	 in the area reserved for register arguments, which may be part of
	 the stack frame.  */
      int needed = args_size.constant;

#ifdef ACCUMULATE_OUTGOING_ARGS
      /* Store the maximum argument space used.  It will be pushed by the
	 prologue.

	 Since the stack pointer will never be pushed, it is possible for
	 the evaluation of a parm to clobber something we have already
	 written to the stack.  Since most function calls on RISC machines
	 do not use the stack, this is uncommon, but must work correctly.
	 
	 Therefore, we save any area of the stack that was already written
	 and that we are using.  Here we set up to do this by making a new
	 stack usage map from the old one.  The actual save will be done
	 by store_one_arg. 

	 Another approach might be to try to reorder the argument
	 evaluations to avoid this conflicting stack usage.  */

      if (needed > current_function_outgoing_args_size)
	current_function_outgoing_args_size = needed;

#if defined(REG_PARM_STACK_SPACE) && ! defined(OUTGOING_REG_PARM_STACK_SPACE)
      /* Since we will be writing into the entire argument area, the
	 map must be allocated for its entire size, not just the part that
	 is the responsibility of the caller.  */
      needed += reg_parm_stack_space;
#endif

#ifdef ARGS_GROW_DOWNWARD
      highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use,
					 needed + 1);
#else
      highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use, needed);
#endif
      stack_usage_map = (char *) alloca (highest_outgoing_arg_in_use);

      if (initial_highest_arg_in_use)
	bcopy (initial_stack_usage_map, stack_usage_map,
	       initial_highest_arg_in_use);

      if (initial_highest_arg_in_use != highest_outgoing_arg_in_use)
	bzero (&stack_usage_map[initial_highest_arg_in_use],
	       highest_outgoing_arg_in_use - initial_highest_arg_in_use);
      needed = 0;

      /* The address of the outgoing argument list must not be copied to a
	 register here, because argblock would be left pointing to the
	 wrong place after the call to allocate_dynamic_stack_space below. */

      argblock = virtual_outgoing_args_rtx;

#else /* not ACCUMULATE_OUTGOING_ARGS */
      if (inhibit_defer_pop == 0)
	{
	  /* Try to reuse some or all of the pending_stack_adjust
	     to get this space.  Maybe we can avoid any pushing.  */
	  if (needed > pending_stack_adjust)
	    {
	      needed -= pending_stack_adjust;
	      pending_stack_adjust = 0;
	    }
	  else
	    {
	      pending_stack_adjust -= needed;
	      needed = 0;
	    }
	}
      /* Special case this because overhead of `push_block' in this
	 case is non-trivial.  */
      if (needed == 0)
	argblock = virtual_outgoing_args_rtx;
      else
	argblock = push_block (GEN_INT (needed), 0, 0);

      /* We only really need to call `copy_to_reg' in the case where push
	 insns are going to be used to pass ARGBLOCK to a function
	 call in ARGS.  In that case, the stack pointer changes value
	 from the allocation point to the call point, and hence
	 the value of VIRTUAL_OUTGOING_ARGS_RTX changes as well.
	 But might as well always do it.  */
      argblock = copy_to_reg (argblock);
#endif /* not ACCUMULATE_OUTGOING_ARGS */
    }


#ifdef ACCUMULATE_OUTGOING_ARGS
  /* The save/restore code in store_one_arg handles all cases except one:
     a constructor call (including a C function returning a BLKmode struct)
     to initialize an argument.  */
  if (stack_arg_under_construction)
    {
#if defined(REG_PARM_STACK_SPACE) && ! defined(OUTGOING_REG_PARM_STACK_SPACE)
      rtx push_size = GEN_INT (reg_parm_stack_space + args_size.constant);
#else
      rtx push_size = GEN_INT (args_size.constant);
#endif
      if (old_stack_level == 0)
	{
	  emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);
	  old_pending_adj = pending_stack_adjust;
	  pending_stack_adjust = 0;
	  /* stack_arg_under_construction says whether a stack arg is
	     being constructed at the old stack level.  Pushing the stack
	     gets a clean outgoing argument block.  */
	  old_stack_arg_under_construction = stack_arg_under_construction;
	  stack_arg_under_construction = 0;
	  /* Make a new map for the new argument list.  */
	  stack_usage_map = (char *)alloca (highest_outgoing_arg_in_use);
	  bzero (stack_usage_map, highest_outgoing_arg_in_use);
	  highest_outgoing_arg_in_use = 0;
	}
      allocate_dynamic_stack_space (push_size, NULL_RTX, BITS_PER_UNIT);
    }
  /* If argument evaluation might modify the stack pointer, copy the
     address of the argument list to a register.  */
  for (i = 0; i < num_actuals; i++)
    if (args[i].pass_on_stack)
      {
	argblock = copy_addr_to_reg (argblock);
	break;
      }
#endif


  /* If we preallocated stack space, compute the address of each argument.
     We need not ensure it is a valid memory address here; it will be 
     validized when it is used.  */
  if (argblock)
    {
      rtx arg_reg = argblock;
      int arg_offset = 0;

      if (GET_CODE (argblock) == PLUS)
	arg_reg = XEXP (argblock, 0), arg_offset = INTVAL (XEXP (argblock, 1));

      for (i = 0; i < num_actuals; i++)
	{
	  rtx offset = ARGS_SIZE_RTX (args[i].offset);
	  rtx slot_offset = ARGS_SIZE_RTX (args[i].slot_offset);
	  rtx addr;

	  /* Skip this parm if it will not be passed on the stack.  */
	  if (! args[i].pass_on_stack && args[i].reg != 0)
	    continue;

	  if (GET_CODE (offset) == CONST_INT)
	    addr = plus_constant (arg_reg, INTVAL (offset));
	  else
	    addr = gen_rtx (PLUS, Pmode, arg_reg, offset);

	  addr = plus_constant (addr, arg_offset);
	  args[i].stack = gen_rtx (MEM, args[i].mode, addr);

	  if (GET_CODE (slot_offset) == CONST_INT)
	    addr = plus_constant (arg_reg, INTVAL (slot_offset));
	  else
	    addr = gen_rtx (PLUS, Pmode, arg_reg, slot_offset);

	  addr = plus_constant (addr, arg_offset);
	  args[i].stack_slot = gen_rtx (MEM, args[i].mode, addr);
	}
    }
					       
#ifdef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we push args individually in reverse order, perform stack alignment
     before the first push (the last arg).  */
  if (argblock == 0)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));
#endif
#endif

  /* Don't try to defer pops if preallocating, not even from the first arg,
     since ARGBLOCK probably refers to the SP.  */
  if (argblock)
    NO_DEFER_POP;

  /* Get the function to call, in the form of RTL.  */
  if (fndecl)
    /* Get a SYMBOL_REF rtx for the function address.  */
    funexp = XEXP (DECL_RTL (fndecl), 0);
  else
    /* Generate an rtx (probably a pseudo-register) for the address.  */
    {
      funexp = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
      free_temp_slots ();	/* FUNEXP can't be BLKmode */
      emit_queue ();
    }

  /* Figure out the register where the value, if any, will come back.  */
  valreg = 0;
  if (TYPE_MODE (TREE_TYPE (exp)) != VOIDmode
      && ! structure_value_addr)
    {
      if (pcc_struct_value)
	valreg = hard_function_value (build_pointer_type (TREE_TYPE (exp)),
				      fndecl);
      else
	valreg = hard_function_value (TREE_TYPE (exp), fndecl);
    }

  /* Precompute all register parameters.  It isn't safe to compute anything
     once we have started filling any specific hard regs. */
  reg_parm_seen = 0;
  for (i = 0; i < num_actuals; i++)
    if (args[i].reg != 0 && ! args[i].pass_on_stack)
      {
	reg_parm_seen = 1;

	if (args[i].value == 0)
	  {
	    args[i].value = expand_expr (args[i].tree_value, NULL_RTX,
					 VOIDmode, 0);
	    preserve_temp_slots (args[i].value);
	    free_temp_slots ();

	    /* ANSI doesn't require a sequence point here,
	       but PCC has one, so this will avoid some problems.  */
	    emit_queue ();
	  }

	/* If we are to promote the function arg to a wider mode,
	   do it now.  */

	if (GET_MODE (args[i].value) != VOIDmode
	    && GET_MODE (args[i].value) != args[i].mode)
	  args[i].value = convert_to_mode (args[i].mode, args[i].value,
					   args[i].unsignedp);
      }

#if defined(ACCUMULATE_OUTGOING_ARGS) && defined(REG_PARM_STACK_SPACE)
  /* The argument list is the property of the called routine and it
     may clobber it.  If the fixed area has been used for previous
     parameters, we must save and restore it.

     Here we compute the boundary of the that needs to be saved, if any.  */

#ifdef ARGS_GROW_DOWNWARD
  for (i = 0; i < reg_parm_stack_space + 1; i++)
#else
  for (i = 0; i < reg_parm_stack_space; i++)
#endif
    {
      if (i >=  highest_outgoing_arg_in_use
	  || stack_usage_map[i] == 0)
	continue;

      if (low_to_save == -1)
	low_to_save = i;

      high_to_save = i;
    }

  if (low_to_save >= 0)
    {
      int num_to_save = high_to_save - low_to_save + 1;
      enum machine_mode save_mode
	= mode_for_size (num_to_save * BITS_PER_UNIT, MODE_INT, 1);
      rtx stack_area;

      /* If we don't have the required alignment, must do this in BLKmode.  */
      if ((low_to_save & (MIN (GET_MODE_SIZE (save_mode),
			       BIGGEST_ALIGNMENT / UNITS_PER_WORD) - 1)))
	save_mode = BLKmode;

      stack_area = gen_rtx (MEM, save_mode,
			    memory_address (save_mode,
					    
#ifdef ARGS_GROW_DOWNWARD
					    plus_constant (argblock,
							   - high_to_save)
#else
					    plus_constant (argblock,
							   low_to_save)
#endif
					    ));
      if (save_mode == BLKmode)
	{
	  save_area = assign_stack_temp (BLKmode, num_to_save, 1);
	  emit_block_move (validize_mem (save_area), stack_area,
			   GEN_INT (num_to_save),
			   PARM_BOUNDARY / BITS_PER_UNIT);
	}
      else
	{
	  save_area = gen_reg_rtx (save_mode);
	  emit_move_insn (save_area, stack_area);
	}
    }
#endif
	  

  /* Now store (and compute if necessary) all non-register parms.
     These come before register parms, since they can require block-moves,
     which could clobber the registers used for register parms.
     Parms which have partial registers are not stored here,
     but we do preallocate space here if they want that.  */

  for (i = 0; i < num_actuals; i++)
    if (args[i].reg == 0 || args[i].pass_on_stack)
      store_one_arg (&args[i], argblock, may_be_alloca,
		     args_size.var != 0, fndecl, reg_parm_stack_space);

#ifdef STRICT_ALIGNMENT
  /* If we have a parm that is passed in registers but not in memory
     and whose alignment does not permit a direct copy into registers,
     make a group of pseudos that correspond to each register that we
     will later fill.  */

  for (i = 0; i < num_actuals; i++)
    if (args[i].reg != 0 && ! args[i].pass_on_stack
	&& args[i].mode == BLKmode
	&& (TYPE_ALIGN (TREE_TYPE (args[i].tree_value))
	    < MIN (BIGGEST_ALIGNMENT, BITS_PER_WORD)))
      {
	int bytes = int_size_in_bytes (TREE_TYPE (args[i].tree_value));

	args[i].n_aligned_regs
	  = args[i].partial ? args[i].partial
	    : (bytes + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;

	args[i].aligned_regs = (rtx *) alloca (sizeof (rtx)
					       * args[i].n_aligned_regs);

	for (j = 0; j < args[i].n_aligned_regs; j++)
	  {
	    rtx reg = gen_reg_rtx (word_mode);
	    rtx word = operand_subword_force (args[i].value, j, BLKmode);
	    int bitsize = TYPE_ALIGN (TREE_TYPE (args[i].tree_value));
	    int bitpos;

	    args[i].aligned_regs[j] = reg;

	    /* Clobber REG and move each partword into it.  Ensure we don't
	       go past the end of the structure.  Note that the loop below
	       works because we've already verified that padding
	       and endianness are compatible.  */

	    emit_insn (gen_rtx (CLOBBER, VOIDmode, reg));

	    for (bitpos = 0;
		 bitpos < BITS_PER_WORD && bytes > 0;
		 bitpos += bitsize, bytes -= bitsize / BITS_PER_UNIT)
	      {
		int xbitpos = (BYTES_BIG_ENDIAN
			       ? BITS_PER_WORD - bitpos - bitsize
			       : bitpos);

		store_bit_field (reg, bitsize, xbitpos, word_mode,
				 extract_bit_field (word, bitsize, xbitpos, 1,
						    NULL_RTX, word_mode,
						    word_mode,
						    bitsize / BITS_PER_UNIT,
						    BITS_PER_WORD),
				 bitsize / BITS_PER_UNIT, BITS_PER_WORD);
	      }
	  }
      }
#endif

  /* Now store any partially-in-registers parm.
     This is the last place a block-move can happen.  */
  if (reg_parm_seen)
    for (i = 0; i < num_actuals; i++)
      if (args[i].partial != 0 && ! args[i].pass_on_stack)
	store_one_arg (&args[i], argblock, may_be_alloca,
		       args_size.var != 0, fndecl, reg_parm_stack_space);

#ifndef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we pushed args in forward order, perform stack alignment
     after pushing the last arg.  */
  if (argblock == 0)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));
#endif
#endif

  /* If register arguments require space on the stack and stack space
     was not preallocated, allocate stack space here for arguments
     passed in registers.  */
#if ! defined(ALLOCATE_OUTGOING_ARGS) && defined(OUTGOING_REG_PARM_STACK_SPACE)
  if (must_preallocate == 0 && reg_parm_stack_space > 0)
    anti_adjust_stack (GEN_INT (reg_parm_stack_space));
#endif

  /* Pass the function the address in which to return a structure value.  */
  if (structure_value_addr && ! structure_value_addr_parm)
    {
      emit_move_insn (struct_value_rtx,
		      force_reg (Pmode,
				 force_operand (structure_value_addr,
						NULL_RTX)));
      if (GET_CODE (struct_value_rtx) == REG)
	{
	  push_to_sequence (use_insns);
	  emit_insn (gen_rtx (USE, VOIDmode, struct_value_rtx));
	  use_insns = get_insns ();
	  end_sequence ();
	}
    }

  /* Now do the register loads required for any wholly-register parms or any
     parms which are passed both on the stack and in a register.  Their
     expressions were already evaluated. 

     Mark all register-parms as living through the call, putting these USE
     insns in a list headed by USE_INSNS.  */

  for (i = 0; i < num_actuals; i++)
    {
      rtx list = args[i].reg;
      int partial = args[i].partial;

      while (list)
	{
	  rtx reg;
	  int nregs;

	  /* Process each register that needs to get this arg.  */
	  if (GET_CODE (list) == EXPR_LIST)
	    reg = XEXP (list, 0), list = XEXP (list, 1);
	  else
	    reg = list, list = 0;

	  /* Set to non-zero if must move a word at a time, even if just one
	     word (e.g, partial == 1 && mode == DFmode).  Set to zero if
	     we just use a normal move insn.  */
	  nregs = (partial ? partial
		   : (TYPE_MODE (TREE_TYPE (args[i].tree_value)) == BLKmode
		      ? ((int_size_in_bytes (TREE_TYPE (args[i].tree_value))
			  + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)
		      : 0));

	  /* If simple case, just do move.  If normal partial, store_one_arg
	     has already loaded the register for us.  In all other cases,
	     load the register(s) from memory.  */

	  if (nregs == 0)
	    emit_move_insn (reg, args[i].value);

#ifdef STRICT_ALIGNMENT
	  /* If we have pre-computed the values to put in the registers in
	     the case of non-aligned structures, copy them in now.  */

	  else if (args[i].n_aligned_regs != 0)
	    for (j = 0; j < args[i].n_aligned_regs; j++)
	      emit_move_insn (gen_rtx (REG, word_mode, REGNO (reg) + j),
			      args[i].aligned_regs[j]);
#endif

	  else if (args[i].partial == 0 || args[i].pass_on_stack)
	    move_block_to_reg (REGNO (reg),
			       validize_mem (args[i].value), nregs,
			       args[i].mode);
	
	  push_to_sequence (use_insns);
	  if (nregs == 0)
	    emit_insn (gen_rtx (USE, VOIDmode, reg));
	  else
	    use_regs (REGNO (reg), nregs);
	  use_insns = get_insns ();
	  end_sequence ();

	  /* PARTIAL referred only to the first register, so clear it for the
	     next time.  */
	  partial = 0;
	}
    }

  /* Perform postincrements before actually calling the function.  */
  emit_queue ();

  /* All arguments and registers used for the call must be set up by now!  */

  funexp = prepare_call_address (funexp, fndecl, &use_insns);

  /* Generate the actual call instruction.  */
  emit_call_1 (funexp, funtype, args_size.constant, struct_value_size,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       valreg, old_inhibit_defer_pop, use_insns, is_const);

  /* If call is cse'able, make appropriate pair of reg-notes around it.
     Test valreg so we don't crash; may safely ignore `const'
     if return type is void.  */
  if (is_const && valreg != 0)
    {
      rtx note = 0;
      rtx temp = gen_reg_rtx (GET_MODE (valreg));
      rtx insns;

      /* Construct an "equal form" for the value which mentions all the
	 arguments in order as well as the function name.  */
#ifdef PUSH_ARGS_REVERSED
      for (i = 0; i < num_actuals; i++)
	note = gen_rtx (EXPR_LIST, VOIDmode, args[i].initial_value, note);
#else
      for (i = num_actuals - 1; i >= 0; i--)
	note = gen_rtx (EXPR_LIST, VOIDmode, args[i].initial_value, note);
#endif
      note = gen_rtx (EXPR_LIST, VOIDmode, funexp, note);

      insns = get_insns ();
      end_sequence ();

      emit_libcall_block (insns, temp, valreg, note);

      valreg = temp;
    }

  /* For calls to `setjmp', etc., inform flow.c it should complain
     if nonvolatile values are live.  */

  if (returns_twice)
    {
      emit_note (name, NOTE_INSN_SETJMP);
      current_function_calls_setjmp = 1;
    }

  if (is_longjmp)
    current_function_calls_longjmp = 1;

  /* Notice functions that cannot return.
     If optimizing, insns emitted below will be dead.
     If not optimizing, they will exist, which is useful
     if the user uses the `return' command in the debugger.  */

  if (is_volatile || is_longjmp)
    emit_barrier ();

  /* If value type not void, return an rtx for the value.  */

  /* If there are cleanups to be called, don't use a hard reg as target.  */
  if (cleanups_this_call != old_cleanups
      && target && REG_P (target)
      && REGNO (target) < FIRST_PSEUDO_REGISTER)
    target = 0;

  if (TYPE_MODE (TREE_TYPE (exp)) == VOIDmode
      || ignore)
    {
      target = const0_rtx;
    }
  else if (structure_value_addr)
    {
      if (target == 0 || GET_CODE (target) != MEM)
	{
	  target = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)),
			    memory_address (TYPE_MODE (TREE_TYPE (exp)),
					    structure_value_addr));
	  MEM_IN_STRUCT_P (target)
	    = (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE
	       || TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE
	       || TREE_CODE (TREE_TYPE (exp)) == QUAL_UNION_TYPE);
	}
    }
  else if (pcc_struct_value)
    {
      if (target == 0)
	{
	  /* We used leave the value in the location that it is
	     returned in, but that causes problems if it is used more
	     than once in one expression.  Rather than trying to track
	     when a copy is required, we always copy when TARGET is
	     not specified.  This calling sequence is only used on
	     a few machines and TARGET is usually nonzero.  */
	  if (TYPE_MODE (TREE_TYPE (exp)) == BLKmode)
	    target = assign_stack_temp (BLKmode,
					int_size_in_bytes (TREE_TYPE (exp)),
					1);
	  else
	    target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));
	}

      if (TYPE_MODE (TREE_TYPE (exp)) != BLKmode)
	emit_move_insn (target, gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)),
					 copy_to_reg (valreg)));
      else
	emit_block_move (target, gen_rtx (MEM, BLKmode, copy_to_reg (valreg)),
			 expr_size (exp),
			 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
    }
  else if (target && GET_MODE (target) == TYPE_MODE (TREE_TYPE (exp))
	   && GET_MODE (target) == GET_MODE (valreg))
    /* TARGET and VALREG cannot be equal at this point because the latter
       would not have REG_FUNCTION_VALUE_P true, while the former would if
       it were referring to the same register.

       If they refer to the same register, this move will be a no-op, except
       when function inlining is being done.  */
    emit_move_insn (target, valreg);
  else
    target = copy_to_reg (valreg);

#ifdef PROMOTE_FUNCTION_RETURN
  /* If we promoted this return value, make the proper SUBREG.  TARGET
     might be const0_rtx here, so be careful.  */
  if (GET_CODE (target) == REG
      && GET_MODE (target) != TYPE_MODE (TREE_TYPE (exp)))
    {
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
      int unsignedp = TREE_UNSIGNED (TREE_TYPE (exp));

      if (TREE_CODE (TREE_TYPE (exp)) == INTEGER_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == ENUMERAL_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == BOOLEAN_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == CHAR_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == REAL_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == OFFSET_TYPE)
	{
	  PROMOTE_MODE (mode, unsignedp, TREE_TYPE (exp));
	}

      /* If we didn't promote as expected, something is wrong.  */
      if (mode != GET_MODE (target))
	abort ();

      target = gen_rtx (SUBREG, TYPE_MODE (TREE_TYPE (exp)), target, 0);
      SUBREG_PROMOTED_VAR_P (target) = 1;
      SUBREG_PROMOTED_UNSIGNED_P (target) = unsignedp;
    }
#endif

  /* Perform all cleanups needed for the arguments of this call
     (i.e. destructors in C++).  */
  expand_cleanups_to (old_cleanups);

  /* If size of args is variable or this was a constructor call for a stack
     argument, restore saved stack-pointer value.  */

  if (old_stack_level)
    {
      emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);
      pending_stack_adjust = old_pending_adj;
#ifdef ACCUMULATE_OUTGOING_ARGS
      stack_arg_under_construction = old_stack_arg_under_construction;
      highest_outgoing_arg_in_use = initial_highest_arg_in_use;
      stack_usage_map = initial_stack_usage_map;
#endif
    }
#ifdef ACCUMULATE_OUTGOING_ARGS
  else
    {
#ifdef REG_PARM_STACK_SPACE
      if (save_area)
	{
	  enum machine_mode save_mode = GET_MODE (save_area);
	  rtx stack_area
	    = gen_rtx (MEM, save_mode,
		       memory_address (save_mode,
#ifdef ARGS_GROW_DOWNWARD
				       plus_constant (argblock, - high_to_save)
#else
				       plus_constant (argblock, low_to_save)
#endif
				       ));

	  if (save_mode != BLKmode)
	    emit_move_insn (stack_area, save_area);
	  else
	    emit_block_move (stack_area, validize_mem (save_area),
			     GEN_INT (high_to_save - low_to_save + 1),
			     PARM_BOUNDARY / BITS_PER_UNIT);
	}
#endif
	  
      /* If we saved any argument areas, restore them.  */
      for (i = 0; i < num_actuals; i++)
	if (args[i].save_area)
	  {
	    enum machine_mode save_mode = GET_MODE (args[i].save_area);
	    rtx stack_area
	      = gen_rtx (MEM, save_mode,
			 memory_address (save_mode,
					 XEXP (args[i].stack_slot, 0)));

	    if (save_mode != BLKmode)
	      emit_move_insn (stack_area, args[i].save_area);
	    else
	      emit_block_move (stack_area, validize_mem (args[i].save_area),
			       GEN_INT (args[i].size.constant),
			       PARM_BOUNDARY / BITS_PER_UNIT);
	  }

      highest_outgoing_arg_in_use = initial_highest_arg_in_use;
      stack_usage_map = initial_stack_usage_map;
    }
#endif

  /* If this was alloca, record the new stack level for nonlocal gotos.  
     Check for the handler slots since we might not have a save area
     for non-local gotos. */

  if (may_be_alloca && nonlocal_goto_handler_slot != 0)
    emit_stack_save (SAVE_NONLOCAL, &nonlocal_goto_stack_level, NULL_RTX);

  pop_temp_slots ();

  return target;
}

/* Output a library call to function FUN (a SYMBOL_REF rtx)
   (emitting the queue unless NO_QUEUE is nonzero),
   for a value of mode OUTMODE,
   with NARGS different arguments, passed as alternating rtx values
   and machine_modes to convert them to.
   The rtx values should have been passed through protect_from_queue already.

   NO_QUEUE will be true if and only if the library call is a `const' call
   which will be enclosed in REG_LIBCALL/REG_RETVAL notes; it is equivalent
   to the variable is_const in expand_call.

   NO_QUEUE must be true for const calls, because if it isn't, then
   any pending increment will be emitted between REG_LIBCALL/REG_RETVAL notes,
   and will be lost if the libcall sequence is optimized away.

   NO_QUEUE must be false for non-const calls, because if it isn't, the
   call insn will have its CONST_CALL_P bit set, and it will be incorrectly
   optimized.  For instance, the instruction scheduler may incorrectly
   move memory references across the non-const call.  */

void
emit_library_call (va_alist)
     va_dcl
{
  va_list p;
  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  struct args_size original_args_size;
  register int argnum;
  enum machine_mode outmode;
  int nargs;
  rtx fun;
  rtx orgfun;
  int inc;
  int count;
  rtx argblock = 0;
  CUMULATIVE_ARGS args_so_far;
  struct arg { rtx value; enum machine_mode mode; rtx reg; int partial;
	       struct args_size offset; struct args_size size; };
  struct arg *argvec;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  int no_queue = 0;
  rtx use_insns;
  /* library calls are never indirect calls.  */
  int current_call_is_indirect = 0;

  va_start (p);
  orgfun = fun = va_arg (p, rtx);
  no_queue = va_arg (p, int);
  outmode = va_arg (p, enum machine_mode);
  nargs = va_arg (p, int);

  /* Copy all the libcall-arguments out of the varargs data
     and into a vector ARGVEC.

     Compute how to pass each argument.  We only support a very small subset
     of the full argument passing conventions to limit complexity here since
     library functions shouldn't have many args.  */

  argvec = (struct arg *) alloca (nargs * sizeof (struct arg));

  INIT_CUMULATIVE_ARGS (args_so_far, NULL_TREE, fun);

  args_size.constant = 0;
  args_size.var = 0;

  for (count = 0; count < nargs; count++)
    {
      rtx val = va_arg (p, rtx);
      enum machine_mode mode = va_arg (p, enum machine_mode);

      /* We cannot convert the arg value to the mode the library wants here;
	 must do it earlier where we know the signedness of the arg.  */
      if (mode == BLKmode
	  || (GET_MODE (val) != mode && GET_MODE (val) != VOIDmode))
	abort ();

      /* On some machines, there's no way to pass a float to a library fcn.
	 Pass it as a double instead.  */
#ifdef LIBGCC_NEEDS_DOUBLE
      if (LIBGCC_NEEDS_DOUBLE && mode == SFmode)
	val = convert_to_mode (DFmode, val, 0), mode = DFmode;
#endif

      /* There's no need to call protect_from_queue, because
	 either emit_move_insn or emit_push_insn will do that.  */

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (GET_CODE (val) != REG && GET_CODE (val) != MEM
	  && ! (CONSTANT_P (val) && LEGITIMATE_CONSTANT_P (val)))
	val = force_operand (val, NULL_RTX);

      argvec[count].value = val;
      argvec[count].mode = mode;

#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
      if (FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, mode, NULL_TREE, 1))
	abort ();
#endif

      argvec[count].reg = FUNCTION_ARG (args_so_far, mode, NULL_TREE, 1);
      if (argvec[count].reg && GET_CODE (argvec[count].reg) == EXPR_LIST)
	abort ();
#ifdef FUNCTION_ARG_PARTIAL_NREGS
      argvec[count].partial
	= FUNCTION_ARG_PARTIAL_NREGS (args_so_far, mode, NULL_TREE, 1);
#else
      argvec[count].partial = 0;
#endif

      locate_and_pad_parm (mode, NULL_TREE,
			   argvec[count].reg && argvec[count].partial == 0,
			   NULL_TREE, &args_size, &argvec[count].offset,
			   &argvec[count].size);

      if (argvec[count].size.var)
	abort ();

#ifndef REG_PARM_STACK_SPACE
      if (argvec[count].partial)
	argvec[count].size.constant -= argvec[count].partial * UNITS_PER_WORD;
#endif

      if (argvec[count].reg == 0 || argvec[count].partial != 0
#ifdef REG_PARM_STACK_SPACE
	  || 1
#endif
	  )
	args_size.constant += argvec[count].size.constant;

#ifdef ACCUMULATE_OUTGOING_ARGS
      /* If this arg is actually passed on the stack, it might be
	 clobbering something we already put there (this library call might
	 be inside the evaluation of an argument to a function whose call
	 requires the stack).  This will only occur when the library call
	 has sufficient args to run out of argument registers.  Abort in
	 this case; if this ever occurs, code must be added to save and
	 restore the arg slot.  */

      if (argvec[count].reg == 0 || argvec[count].partial != 0)
	abort ();
#endif

      FUNCTION_ARG_ADVANCE (args_so_far, mode, (tree)0, 1);
    }
  va_end (p);

  /* If this machine requires an external definition for library
     functions, write one out.  */
  assemble_external_libcall (fun);

  original_args_size = args_size;
#ifdef STACK_BOUNDARY
  args_size.constant = (((args_size.constant + (STACK_BYTES - 1))
			 / STACK_BYTES) * STACK_BYTES);
#endif

#ifdef REG_PARM_STACK_SPACE
  args_size.constant = MAX (args_size.constant,
			    REG_PARM_STACK_SPACE (NULL_TREE));
#ifndef OUTGOING_REG_PARM_STACK_SPACE
  args_size.constant -= REG_PARM_STACK_SPACE (NULL_TREE);
#endif
#endif

#ifdef ACCUMULATE_OUTGOING_ARGS
  if (args_size.constant > current_function_outgoing_args_size)
    current_function_outgoing_args_size = args_size.constant;
  args_size.constant = 0;
#endif

#ifndef PUSH_ROUNDING
  argblock = push_block (GEN_INT (args_size.constant), 0, 0);
#endif

#ifdef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we push args individually in reverse order, perform stack alignment
     before the first push (the last arg).  */
  if (argblock == 0)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));
#endif
#endif

#ifdef PUSH_ARGS_REVERSED
  inc = -1;
  argnum = nargs - 1;
#else
  inc = 1;
  argnum = 0;
#endif

  /* Push the args that need to be pushed.  */

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      if (! (reg != 0 && partial == 0))
	emit_push_insn (val, mode, NULL_TREE, NULL_RTX, 0, partial, reg, 0,
			argblock, GEN_INT (argvec[count].offset.constant));
      NO_DEFER_POP;
    }

#ifndef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we pushed args in forward order, perform stack alignment
     after pushing the last arg.  */
  if (argblock == 0)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));
#endif
#endif

#ifdef PUSH_ARGS_REVERSED
  argnum = nargs - 1;
#else
  argnum = 0;
#endif

  /* Now load any reg parms into their regs.  */

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      if (reg != 0 && partial == 0)
	emit_move_insn (reg, val);
      NO_DEFER_POP;
    }

  /* For version 1.37, try deleting this entirely.  */
  if (! no_queue)
    emit_queue ();

  /* Any regs containing parms remain in use through the call.  */
  start_sequence ();
  for (count = 0; count < nargs; count++)
    if (argvec[count].reg != 0)
      emit_insn (gen_rtx (USE, VOIDmode, argvec[count].reg));

  use_insns = get_insns ();
  end_sequence ();

  fun = prepare_call_address (fun, NULL_TREE, &use_insns);

  /* Don't allow popping to be deferred, since then
     cse'ing of library calls could delete a call and leave the pop.  */
  NO_DEFER_POP;

  /* We pass the old value of inhibit_defer_pop + 1 to emit_call_1, which
     will set inhibit_defer_pop to that value.  */

  emit_call_1 (fun, get_identifier (XSTR (orgfun, 0)), args_size.constant, 0,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       outmode != VOIDmode ? hard_libcall_value (outmode) : NULL_RTX,
	       old_inhibit_defer_pop + 1, use_insns, no_queue);

  /* Now restore inhibit_defer_pop to its actual original value.  */
  OK_DEFER_POP;
}

/* Like emit_library_call except that an extra argument, VALUE,
   comes second and says where to store the result.
   (If VALUE is zero, the result comes in the function value register.)  */

void
emit_library_call_value (va_alist)
     va_dcl
{
  va_list p;
  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  struct args_size original_args_size;
  register int argnum;
  enum machine_mode outmode;
  int nargs;
  rtx fun;
  rtx orgfun;
  int inc;
  int count;
  rtx argblock = 0;
  CUMULATIVE_ARGS args_so_far;
  struct arg { rtx value; enum machine_mode mode; rtx reg; int partial;
	       struct args_size offset; struct args_size size; };
  struct arg *argvec;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  int no_queue = 0;
  rtx use_insns;
  rtx value;
  rtx mem_value = 0;
  /* library calls are never indirect calls.  */
  int current_call_is_indirect = 0;

  va_start (p);
  orgfun = fun = va_arg (p, rtx);
  value = va_arg (p, rtx);
  no_queue = va_arg (p, int);
  outmode = va_arg (p, enum machine_mode);
  nargs = va_arg (p, int);

  /* If this kind of value comes back in memory,
     decide where in memory it should come back.  */
  if (RETURN_IN_MEMORY (type_for_mode (outmode, 0)))
    {
      if (GET_CODE (value) == MEM)
	mem_value = value;
      else
	mem_value = assign_stack_temp (outmode, GET_MODE_SIZE (outmode), 0);
    }

  /* ??? Unfinished: must pass the memory address as an argument.  */

  /* Copy all the libcall-arguments out of the varargs data
     and into a vector ARGVEC.

     Compute how to pass each argument.  We only support a very small subset
     of the full argument passing conventions to limit complexity here since
     library functions shouldn't have many args.  */

  argvec = (struct arg *) alloca ((nargs + 1) * sizeof (struct arg));

  INIT_CUMULATIVE_ARGS (args_so_far, NULL_TREE, fun);

  args_size.constant = 0;
  args_size.var = 0;

  count = 0;

  /* If there's a structure value address to be passed,
     either pass it in the special place, or pass it as an extra argument.  */
  if (mem_value)
    {
      rtx addr = XEXP (mem_value, 0);

      if (! struct_value_rtx)
	{
	  nargs++;

	  /* Make sure it is a reasonable operand for a move or push insn.  */
	  if (GET_CODE (addr) != REG && GET_CODE (addr) != MEM
	      && ! (CONSTANT_P (addr) && LEGITIMATE_CONSTANT_P (addr)))
	    addr = force_operand (addr, NULL_RTX);

	  argvec[count].value = addr;
	  argvec[count].mode = outmode;
	  argvec[count].partial = 0;

	  argvec[count].reg = FUNCTION_ARG (args_so_far, outmode, NULL_TREE, 1);
#ifdef FUNCTION_ARG_PARTIAL_NREGS
	  if (FUNCTION_ARG_PARTIAL_NREGS (args_so_far, outmode, NULL_TREE, 1))
	    abort ();
#endif

	  locate_and_pad_parm (outmode, NULL_TREE,
			       argvec[count].reg && argvec[count].partial == 0,
			       NULL_TREE, &args_size, &argvec[count].offset,
			       &argvec[count].size);


	  if (argvec[count].reg == 0 || argvec[count].partial != 0
#ifdef REG_PARM_STACK_SPACE
	      || 1
#endif
	      )
	    args_size.constant += argvec[count].size.constant;

	  FUNCTION_ARG_ADVANCE (args_so_far, outmode, (tree)0, 1);
	}
    }

  for (; count < nargs; count++)
    {
      rtx val = va_arg (p, rtx);
      enum machine_mode mode = va_arg (p, enum machine_mode);

      /* We cannot convert the arg value to the mode the library wants here;
	 must do it earlier where we know the signedness of the arg.  */
      if (mode == BLKmode
	  || (GET_MODE (val) != mode && GET_MODE (val) != VOIDmode))
	abort ();

      /* On some machines, there's no way to pass a float to a library fcn.
	 Pass it as a double instead.  */
#ifdef LIBGCC_NEEDS_DOUBLE
      if (LIBGCC_NEEDS_DOUBLE && mode == SFmode)
	val = convert_to_mode (DFmode, val, 0), mode = DFmode;
#endif

      /* There's no need to call protect_from_queue, because
	 either emit_move_insn or emit_push_insn will do that.  */

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (GET_CODE (val) != REG && GET_CODE (val) != MEM
	  && ! (CONSTANT_P (val) && LEGITIMATE_CONSTANT_P (val)))
	val = force_operand (val, NULL_RTX);

      argvec[count].value = val;
      argvec[count].mode = mode;

#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
      if (FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, mode, NULL_TREE, 1))
	abort ();
#endif

      argvec[count].reg = FUNCTION_ARG (args_so_far, mode, NULL_TREE, 1);
      if (argvec[count].reg && GET_CODE (argvec[count].reg) == EXPR_LIST)
	abort ();
#ifdef FUNCTION_ARG_PARTIAL_NREGS
      argvec[count].partial
	= FUNCTION_ARG_PARTIAL_NREGS (args_so_far, mode, NULL_TREE, 1);
#else
      argvec[count].partial = 0;
#endif

      locate_and_pad_parm (mode, NULL_TREE,
			   argvec[count].reg && argvec[count].partial == 0,
			   NULL_TREE, &args_size, &argvec[count].offset,
			   &argvec[count].size);

      if (argvec[count].size.var)
	abort ();

#ifndef REG_PARM_STACK_SPACE
      if (argvec[count].partial)
	argvec[count].size.constant -= argvec[count].partial * UNITS_PER_WORD;
#endif

      if (argvec[count].reg == 0 || argvec[count].partial != 0
#ifdef REG_PARM_STACK_SPACE
	  || 1
#endif
	  )
	args_size.constant += argvec[count].size.constant;

#ifdef ACCUMULATE_OUTGOING_ARGS
      /* If this arg is actually passed on the stack, it might be
	 clobbering something we already put there (this library call might
	 be inside the evaluation of an argument to a function whose call
	 requires the stack).  This will only occur when the library call
	 has sufficient args to run out of argument registers.  Abort in
	 this case; if this ever occurs, code must be added to save and
	 restore the arg slot.  */

      if (argvec[count].reg == 0 || argvec[count].partial != 0)
	abort ();
#endif

      FUNCTION_ARG_ADVANCE (args_so_far, mode, (tree)0, 1);
    }
  va_end (p);

  /* If this machine requires an external definition for library
     functions, write one out.  */
  assemble_external_libcall (fun);

  original_args_size = args_size;
#ifdef STACK_BOUNDARY
  args_size.constant = (((args_size.constant + (STACK_BYTES - 1))
			 / STACK_BYTES) * STACK_BYTES);
#endif

#ifdef REG_PARM_STACK_SPACE
  args_size.constant = MAX (args_size.constant,
			    REG_PARM_STACK_SPACE (NULL_TREE));
#ifndef OUTGOING_REG_PARM_STACK_SPACE
  args_size.constant -= REG_PARM_STACK_SPACE (NULL_TREE);
#endif
#endif

#ifdef ACCUMULATE_OUTGOING_ARGS
  if (args_size.constant > current_function_outgoing_args_size)
    current_function_outgoing_args_size = args_size.constant;
  args_size.constant = 0;
#endif

#ifndef PUSH_ROUNDING
  argblock = push_block (GEN_INT (args_size.constant), 0, 0);
#endif

#ifdef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we push args individually in reverse order, perform stack alignment
     before the first push (the last arg).  */
  if (argblock == 0)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));
#endif
#endif

#ifdef PUSH_ARGS_REVERSED
  inc = -1;
  argnum = nargs - 1;
#else
  inc = 1;
  argnum = 0;
#endif

  /* Push the args that need to be pushed.  */

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      if (! (reg != 0 && partial == 0))
	emit_push_insn (val, mode, NULL_TREE, NULL_RTX, 0, partial, reg, 0,
			argblock, GEN_INT (argvec[count].offset.constant));
      NO_DEFER_POP;
    }

#ifndef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we pushed args in forward order, perform stack alignment
     after pushing the last arg.  */
  if (argblock == 0)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));
#endif
#endif

#ifdef PUSH_ARGS_REVERSED
  argnum = nargs - 1;
#else
  argnum = 0;
#endif

  /* Now load any reg parms into their regs.  */

  if (mem_value != 0 && struct_value_rtx != 0)
    emit_move_insn (struct_value_rtx, XEXP (mem_value, 0));

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      if (reg != 0 && partial == 0)
	emit_move_insn (reg, val);
      NO_DEFER_POP;
    }

#if 0
  /* For version 1.37, try deleting this entirely.  */
  if (! no_queue)
    emit_queue ();
#endif

  /* Any regs containing parms remain in use through the call.  */
  start_sequence ();
  for (count = 0; count < nargs; count++)
    if (argvec[count].reg != 0)
      emit_insn (gen_rtx (USE, VOIDmode, argvec[count].reg));

  use_insns = get_insns ();
  end_sequence ();

  fun = prepare_call_address (fun, NULL_TREE, &use_insns);

  /* Don't allow popping to be deferred, since then
     cse'ing of library calls could delete a call and leave the pop.  */
  NO_DEFER_POP;

  /* We pass the old value of inhibit_defer_pop + 1 to emit_call_1, which
     will set inhibit_defer_pop to that value.  */

  emit_call_1 (fun, get_identifier (XSTR (orgfun, 0)), args_size.constant, 0,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       outmode != VOIDmode ? hard_libcall_value (outmode) : NULL_RTX,
	       old_inhibit_defer_pop + 1, use_insns, no_queue);

  /* Now restore inhibit_defer_pop to its actual original value.  */
  OK_DEFER_POP;

  /* Copy the value to the right place.  */
  if (outmode != VOIDmode)
    {
      if (mem_value)
	{
	  if (value == 0)
	    value = hard_libcall_value (outmode);
	  if (value != mem_value)
	    emit_move_insn (value, mem_value);
	}
      else if (value != 0)
	emit_move_insn (value, hard_libcall_value (outmode));
    }
}

#if 0
/* Return an rtx which represents a suitable home on the stack
   given TYPE, the type of the argument looking for a home.
   This is called only for BLKmode arguments.

   SIZE is the size needed for this target.
   ARGS_ADDR is the address of the bottom of the argument block for this call.
   OFFSET describes this parameter's offset into ARGS_ADDR.  It is meaningless
   if this machine uses push insns.  */

static rtx
target_for_arg (type, size, args_addr, offset)
     tree type;
     rtx size;
     rtx args_addr;
     struct args_size offset;
{
  rtx target;
  rtx offset_rtx = ARGS_SIZE_RTX (offset);

  /* We do not call memory_address if possible,
     because we want to address as close to the stack
     as possible.  For non-variable sized arguments,
     this will be stack-pointer relative addressing.  */
  if (GET_CODE (offset_rtx) == CONST_INT)
    target = plus_constant (args_addr, INTVAL (offset_rtx));
  else
    {
      /* I have no idea how to guarantee that this
	 will work in the presence of register parameters.  */
      target = gen_rtx (PLUS, Pmode, args_addr, offset_rtx);
      target = memory_address (QImode, target);
    }

  return gen_rtx (MEM, BLKmode, target);
}
#endif

/* Store a single argument for a function call
   into the register or memory area where it must be passed.
   *ARG describes the argument value and where to pass it.

   ARGBLOCK is the address of the stack-block for all the arguments,
   or 0 on a machine where arguments are pushed individually.

   MAY_BE_ALLOCA nonzero says this could be a call to `alloca'
   so must be careful about how the stack is used. 

   VARIABLE_SIZE nonzero says that this was a variable-sized outgoing
   argument stack.  This is used if ACCUMULATE_OUTGOING_ARGS to indicate
   that we need not worry about saving and restoring the stack.

   FNDECL is the declaration of the function we are calling.  */

static void
store_one_arg (arg, argblock, may_be_alloca, variable_size, fndecl,
	       reg_parm_stack_space)
     struct arg_data *arg;
     rtx argblock;
     int may_be_alloca;
     int variable_size;
     tree fndecl;
     int reg_parm_stack_space;
{
  register tree pval = arg->tree_value;
  rtx reg = 0;
  int partial = 0;
  int used = 0;
  int i, lower_bound, upper_bound;

  if (TREE_CODE (pval) == ERROR_MARK)
    return;

#ifdef ACCUMULATE_OUTGOING_ARGS
  /* If this is being stored into a pre-allocated, fixed-size, stack area,
     save any previous data at that location.  */
  if (argblock && ! variable_size && arg->stack)
    {
#ifdef ARGS_GROW_DOWNWARD
      /* stack_slot is negative, but we want to index stack_usage_map */
      /* with positive values. */
      if (GET_CODE (XEXP (arg->stack_slot, 0)) == PLUS)
	upper_bound = -INTVAL (XEXP (XEXP (arg->stack_slot, 0), 1)) + 1;
      else
	abort ();

      lower_bound = upper_bound - arg->size.constant;
#else
      if (GET_CODE (XEXP (arg->stack_slot, 0)) == PLUS)
	lower_bound = INTVAL (XEXP (XEXP (arg->stack_slot, 0), 1));
      else
	lower_bound = 0;

      upper_bound = lower_bound + arg->size.constant;
#endif

      for (i = lower_bound; i < upper_bound; i++)
	if (stack_usage_map[i]
#ifdef REG_PARM_STACK_SPACE
	    /* Don't store things in the fixed argument area at this point;
	       it has already been saved.  */
	    && i > reg_parm_stack_space
#endif
	    )
	  break;

      if (i != upper_bound)
	{
	  /* We need to make a save area.  See what mode we can make it.  */
	  enum machine_mode save_mode
	    = mode_for_size (arg->size.constant * BITS_PER_UNIT, MODE_INT, 1);
	  rtx stack_area
	    = gen_rtx (MEM, save_mode,
		       memory_address (save_mode, XEXP (arg->stack_slot, 0)));

	  if (save_mode == BLKmode)
	    {
	      arg->save_area = assign_stack_temp (BLKmode,
						  arg->size.constant, 1);
	      emit_block_move (validize_mem (arg->save_area), stack_area,
			       GEN_INT (arg->size.constant),
			       PARM_BOUNDARY / BITS_PER_UNIT);
	    }
	  else
	    {
	      arg->save_area = gen_reg_rtx (save_mode);
	      emit_move_insn (arg->save_area, stack_area);
	    }
	}
    }
#endif

  /* If this isn't going to be placed on both the stack and in registers,
     set up the register and number of words.  */
  if (! arg->pass_on_stack)
    reg = arg->reg, partial = arg->partial;

  if (reg != 0 && partial == 0)
    /* Being passed entirely in a register.  We shouldn't be called in
       this case.   */
    abort ();

#ifdef STRICT_ALIGNMENT
  /* If this arg needs special alignment, don't load the registers
     here.  */
  if (arg->n_aligned_regs != 0)
    reg = 0;
#endif
  
  /* If this is being partially passed in a register, but multiple locations
     are specified, we assume that the one partially used is the one that is
     listed first.  */
  if (reg && GET_CODE (reg) == EXPR_LIST)
    reg = XEXP (reg, 0);

  /* If this is being passed partially in a register, we can't evaluate
     it directly into its stack slot.  Otherwise, we can.  */
  if (arg->value == 0)
    {
#ifdef ACCUMULATE_OUTGOING_ARGS
      /* stack_arg_under_construction is nonzero if a function argument is
	 being evaluated directly into the outgoing argument list and
	 expand_call must take special action to preserve the argument list
	 if it is called recursively.

	 For scalar function arguments stack_usage_map is sufficient to
	 determine which stack slots must be saved and restored.  Scalar
	 arguments in general have pass_on_stack == 0.

	 If this argument is initialized by a function which takes the
	 address of the argument (a C++ constructor or a C function
	 returning a BLKmode structure), then stack_usage_map is
	 insufficient and expand_call must push the stack around the
	 function call.  Such arguments have pass_on_stack == 1.

	 Note that it is always safe to set stack_arg_under_construction,
	 but this generates suboptimal code if set when not needed.  */

      if (arg->pass_on_stack)
	stack_arg_under_construction++;
#endif
      arg->value = expand_expr (pval, partial ? NULL_RTX : arg->stack,
				VOIDmode, 0);

      /* If we are promoting object (or for any other reason) the mode
	 doesn't agree, convert the mode.  */

      if (GET_MODE (arg->value) != VOIDmode
	  && GET_MODE (arg->value) != arg->mode)
	arg->value = convert_to_mode (arg->mode, arg->value, arg->unsignedp);

#ifdef ACCUMULATE_OUTGOING_ARGS
      if (arg->pass_on_stack)
	stack_arg_under_construction--;
#endif
    }

  /* Don't allow anything left on stack from computation
     of argument to alloca.  */
  if (may_be_alloca)
    do_pending_stack_adjust ();

  if (arg->value == arg->stack)
    /* If the value is already in the stack slot, we are done.  */
    ;
  else if (arg->mode != BLKmode)
    {
      register int size;

      /* Argument is a scalar, not entirely passed in registers.
	 (If part is passed in registers, arg->partial says how much
	 and emit_push_insn will take care of putting it there.)
	 
	 Push it, and if its size is less than the
	 amount of space allocated to it,
	 also bump stack pointer by the additional space.
	 Note that in C the default argument promotions
	 will prevent such mismatches.  */

      size = GET_MODE_SIZE (arg->mode);
      /* Compute how much space the push instruction will push.
	 On many machines, pushing a byte will advance the stack
	 pointer by a halfword.  */
#ifdef PUSH_ROUNDING
      size = PUSH_ROUNDING (size);
#endif
      used = size;

      /* Compute how much space the argument should get:
	 round up to a multiple of the alignment for arguments.  */
      if (none != FUNCTION_ARG_PADDING (arg->mode, TREE_TYPE (pval)))
	used = (((size + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		 / (PARM_BOUNDARY / BITS_PER_UNIT))
		* (PARM_BOUNDARY / BITS_PER_UNIT));

      /* This isn't already where we want it on the stack, so put it there.
	 This can either be done with push or copy insns.  */
      emit_push_insn (arg->value, arg->mode, TREE_TYPE (pval), NULL_RTX,
		      0, partial, reg, used - size,
		      argblock, ARGS_SIZE_RTX (arg->offset));
    }
  else
    {
      /* BLKmode, at least partly to be pushed.  */

      register int excess;
      rtx size_rtx;

      /* Pushing a nonscalar.
	 If part is passed in registers, PARTIAL says how much
	 and emit_push_insn will take care of putting it there.  */

      /* Round its size up to a multiple
	 of the allocation unit for arguments.  */

      if (arg->size.var != 0)
	{
	  excess = 0;
	  size_rtx = ARGS_SIZE_RTX (arg->size);
	}
      else
	{
	  /* PUSH_ROUNDING has no effect on us, because
	     emit_push_insn for BLKmode is careful to avoid it.  */
	  excess = (arg->size.constant - int_size_in_bytes (TREE_TYPE (pval))
		    + partial * UNITS_PER_WORD);
	  size_rtx = expr_size (pval);
	}

      emit_push_insn (arg->value, arg->mode, TREE_TYPE (pval), size_rtx,
		      TYPE_ALIGN (TREE_TYPE (pval)) / BITS_PER_UNIT, partial,
		      reg, excess, argblock, ARGS_SIZE_RTX (arg->offset));
    }


  /* Unless this is a partially-in-register argument, the argument is now
     in the stack. 

     ??? Note that this can change arg->value from arg->stack to
     arg->stack_slot and it matters when they are not the same.
     It isn't totally clear that this is correct in all cases.  */
  if (partial == 0)
    arg->value = arg->stack_slot;

  /* Once we have pushed something, pops can't safely
     be deferred during the rest of the arguments.  */
  NO_DEFER_POP;

  /* ANSI doesn't require a sequence point here,
     but PCC has one, so this will avoid some problems.  */
  emit_queue ();

  /* Free any temporary slots made in processing this argument.  */
  free_temp_slots ();

#ifdef ACCUMULATE_OUTGOING_ARGS
  /* Now mark the segment we just used.  */
  if (argblock && ! variable_size && arg->stack)
    for (i = lower_bound; i < upper_bound; i++)
      stack_usage_map[i] = 1;
#endif
}
