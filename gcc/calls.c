/* Convert function calls to rtl insns, for GNU C compiler.
   Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "function.h"
#include "regs.h"
#include "toplev.h"
#include "output.h"
#include "tm_p.h"
#include "timevar.h"
#include "sbitmap.h"
#include "langhooks.h"
#include "target.h"
#include "cgraph.h"
#include "except.h"

/* Like PREFERRED_STACK_BOUNDARY but in units of bytes, not bits.  */
#define STACK_BYTES (PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT)

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
     PARALLEL if the arg is to be copied into multiple non-contiguous
     registers.  */
  rtx reg;
  /* Register to pass this argument in when generating tail call sequence.
     This is not the same register as for normal calls on machines with
     register windows.  */
  rtx tail_call_reg;
  /* If REG was promoted from the actual mode of the argument expression,
     indicates whether the promotion is sign- or zero-extended.  */
  int unsignedp;
  /* Number of registers to use.  0 means put the whole arg in registers.
     Also 0 if not passed in registers.  */
  int partial;
  /* Nonzero if argument must be passed on stack.
     Note that some arguments may be passed on the stack
     even though pass_on_stack is zero, just because FUNCTION_ARG says so.
     pass_on_stack identifies arguments that *cannot* go in registers.  */
  int pass_on_stack;
  /* Some fields packaged up for locate_and_pad_parm.  */
  struct locate_and_pad_arg_data locate;
  /* Location on the stack at which parameter should be stored.  The store
     has already been done if STACK == VALUE.  */
  rtx stack;
  /* Location on the stack of the start of this argument slot.  This can
     differ from STACK if this arg pads downward.  This location is known
     to be aligned to FUNCTION_ARG_BOUNDARY.  */
  rtx stack_slot;
  /* Place that this stack area has been saved, if needed.  */
  rtx save_area;
  /* If an argument's alignment does not permit direct copying into registers,
     copy in smaller-sized pieces into pseudos.  These are stored in a
     block pointed to by this field.  The next field says how many
     word-sized pseudos we made.  */
  rtx *aligned_regs;
  int n_aligned_regs;
};

/* A vector of one char per byte of stack space.  A byte if nonzero if
   the corresponding stack location has been used.
   This vector is used to prevent a function call within an argument from
   clobbering any stack already set up.  */
static char *stack_usage_map;

/* Size of STACK_USAGE_MAP.  */
static int highest_outgoing_arg_in_use;

/* A bitmap of virtual-incoming stack space.  Bit is set if the corresponding
   stack location's tail call argument has been already stored into the stack.
   This bitmap is used to prevent sibling call optimization if function tries
   to use parent's incoming argument slots when they have been already
   overwritten with tail call arguments.  */
static sbitmap stored_args_map;

/* stack_arg_under_construction is nonzero when an argument may be
   initialized with a constructor call (including a C function that
   returns a BLKmode struct) and expand_call must take special action
   to make sure the object being constructed does not overlap the
   argument list for the constructor call.  */
int stack_arg_under_construction;

static int calls_function (tree, int);
static int calls_function_1 (tree, int);

static void emit_call_1 (rtx, tree, tree, HOST_WIDE_INT, HOST_WIDE_INT,
			 HOST_WIDE_INT, rtx, rtx, int, rtx, int,
			 CUMULATIVE_ARGS *);
static void precompute_register_parameters (int, struct arg_data *, int *);
static int store_one_arg (struct arg_data *, rtx, int, int, int);
static void store_unaligned_arguments_into_pseudos (struct arg_data *, int);
static int finalize_must_preallocate (int, int, struct arg_data *,
				      struct args_size *);
static void precompute_arguments (int, int, struct arg_data *);
static int compute_argument_block_size (int, struct args_size *, int);
static void initialize_argument_information (int, struct arg_data *,
					     struct args_size *, int, tree,
					     tree, CUMULATIVE_ARGS *, int,
					     rtx *, int *, int *, int *,
					     bool);
static void compute_argument_addresses (struct arg_data *, rtx, int);
static rtx rtx_for_function_call (tree, tree);
static void load_register_parameters (struct arg_data *, int, rtx *, int,
				      int, int *);
static rtx emit_library_call_value_1 (int, rtx, rtx, enum libcall_type,
				      enum machine_mode, int, va_list);
static int special_function_p (tree, int);
static rtx try_to_integrate (tree, tree, rtx, int, tree, rtx);
static int check_sibcall_argument_overlap_1 (rtx);
static int check_sibcall_argument_overlap (rtx, struct arg_data *, int);

static int combine_pending_stack_adjustment_and_call (int, struct args_size *,
						      int);
static tree fix_unsafe_tree (tree);
static bool shift_returned_value (tree, rtx *);

#ifdef REG_PARM_STACK_SPACE
static rtx save_fixed_argument_area (int, rtx, int *, int *);
static void restore_fixed_argument_area (rtx, rtx, int, int);
#endif

/* If WHICH is 1, return 1 if EXP contains a call to the built-in function
   `alloca'.

   If WHICH is 0, return 1 if EXP contains a call to any function.
   Actually, we only need return 1 if evaluating EXP would require pushing
   arguments on the stack, but that is too difficult to compute, so we just
   assume any function call might require the stack.  */

static tree calls_function_save_exprs;

static int
calls_function (tree exp, int which)
{
  int val;

  calls_function_save_exprs = 0;
  val = calls_function_1 (exp, which);
  calls_function_save_exprs = 0;
  return val;
}

/* Recursive function to do the work of above function.  */

static int
calls_function_1 (tree exp, int which)
{
  int i;
  enum tree_code code = TREE_CODE (exp);
  int class = TREE_CODE_CLASS (code);
  int length = first_rtl_op (code);

  /* If this code is language-specific, we don't know what it will do.  */
  if ((int) code >= NUM_TREE_CODES)
    return 1;

  switch (code)
    {
    case CALL_EXPR:
      if (which == 0)
	return 1;
      else if ((TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))
		== FUNCTION_TYPE)
	       && (TYPE_RETURNS_STACK_DEPRESSED
		   (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))))
	return 1;
      else if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	       && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		   == FUNCTION_DECL)
	       && (special_function_p (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				       0)
		   & ECF_MAY_BE_ALLOCA))
	return 1;

      break;

    case CONSTRUCTOR:
      {
	tree tem;

	for (tem = CONSTRUCTOR_ELTS (exp); tem != 0; tem = TREE_CHAIN (tem))
	  if (calls_function_1 (TREE_VALUE (tem), which))
	    return 1;
      }

      return 0;

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) != 0)
	return 0;
      if (value_member (exp, calls_function_save_exprs))
	return 0;
      calls_function_save_exprs = tree_cons (NULL_TREE, exp,
					     calls_function_save_exprs);
      return (TREE_OPERAND (exp, 0) != 0
	      && calls_function_1 (TREE_OPERAND (exp, 0), which));

    case BLOCK:
      {
	tree local;
	tree subblock;

	for (local = BLOCK_VARS (exp); local; local = TREE_CHAIN (local))
	  if (DECL_INITIAL (local) != 0
	      && calls_function_1 (DECL_INITIAL (local), which))
	    return 1;

	for (subblock = BLOCK_SUBBLOCKS (exp);
	     subblock;
	     subblock = TREE_CHAIN (subblock))
	  if (calls_function_1 (subblock, which))
	    return 1;
      }
      return 0;

    case TREE_LIST:
      for (; exp != 0; exp = TREE_CHAIN (exp))
	if (calls_function_1 (TREE_VALUE (exp), which))
	  return 1;
      return 0;

    default:
      break;
    }

  /* Only expressions and blocks can contain calls.  */
  if (! IS_EXPR_CODE_CLASS (class) && class != 'b')
    return 0;

  for (i = 0; i < length; i++)
    if (TREE_OPERAND (exp, i) != 0
	&& calls_function_1 (TREE_OPERAND (exp, i), which))
      return 1;

  return 0;
}

/* Force FUNEXP into a form suitable for the address of a CALL,
   and return that as an rtx.  Also load the static chain register
   if FNDECL is a nested function.

   CALL_FUSAGE points to a variable holding the prospective
   CALL_INSN_FUNCTION_USAGE information.  */

rtx
prepare_call_address (rtx funexp, tree fndecl, rtx *call_fusage,
		      int reg_parm_seen, int sibcallp)
{
  rtx static_chain_value = 0;

  funexp = protect_from_queue (funexp, 0);

  if (fndecl != 0)
    /* Get possible static chain value for nested function in C.  */
    static_chain_value = lookup_static_chain (fndecl);

  /* Make a valid memory address and copy constants through pseudo-regs,
     but not for a constant address if -fno-function-cse.  */
  if (GET_CODE (funexp) != SYMBOL_REF)
    /* If we are using registers for parameters, force the
       function address into a register now.  */
    funexp = ((SMALL_REGISTER_CLASSES && reg_parm_seen)
	      ? force_not_mem (memory_address (FUNCTION_MODE, funexp))
	      : memory_address (FUNCTION_MODE, funexp));
  else if (! sibcallp)
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

      if (GET_CODE (static_chain_rtx) == REG)
	use_reg (call_fusage, static_chain_rtx);
    }

  return funexp;
}

/* Generate instructions to call function FUNEXP,
   and optionally pop the results.
   The CALL_INSN is the first insn generated.

   FNDECL is the declaration node of the function.  This is given to the
   macro RETURN_POPS_ARGS to determine whether this function pops its own args.

   FUNTYPE is the data type of the function.  This is given to the macro
   RETURN_POPS_ARGS to determine whether this function pops its own args.
   We used to allow an identifier for library functions, but that doesn't
   work when the return type is an aggregate type and the calling convention
   says that the pointer to this aggregate is to be popped by the callee.

   STACK_SIZE is the number of bytes of arguments on the stack,
   ROUNDED_STACK_SIZE is that number rounded up to
   PREFERRED_STACK_BOUNDARY; zero if the size is variable.  This is
   both to put into the call insn and to generate explicit popping
   code if necessary.

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

   CALL_FUSAGE is either empty or an EXPR_LIST of USE expressions that
   denote registers used by the called function.  */

static void
emit_call_1 (rtx funexp, tree fndecl ATTRIBUTE_UNUSED, tree funtype ATTRIBUTE_UNUSED,
	     HOST_WIDE_INT stack_size ATTRIBUTE_UNUSED,
	     HOST_WIDE_INT rounded_stack_size,
	     HOST_WIDE_INT struct_value_size ATTRIBUTE_UNUSED,
	     rtx next_arg_reg ATTRIBUTE_UNUSED, rtx valreg,
	     int old_inhibit_defer_pop, rtx call_fusage, int ecf_flags,
	     CUMULATIVE_ARGS *args_so_far ATTRIBUTE_UNUSED)
{
  rtx rounded_stack_size_rtx = GEN_INT (rounded_stack_size);
  rtx call_insn;
  int already_popped = 0;
  HOST_WIDE_INT n_popped = RETURN_POPS_ARGS (fndecl, funtype, stack_size);
#if defined (HAVE_call) && defined (HAVE_call_value)
  rtx struct_value_size_rtx;
  struct_value_size_rtx = GEN_INT (struct_value_size);
#endif

#ifdef CALL_POPS_ARGS
  n_popped += CALL_POPS_ARGS (* args_so_far);
#endif

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (funexp) != SYMBOL_REF)
    funexp = memory_address (FUNCTION_MODE, funexp);

#if defined (HAVE_sibcall_pop) && defined (HAVE_sibcall_value_pop)
  if ((ecf_flags & ECF_SIBCALL)
      && HAVE_sibcall_pop && HAVE_sibcall_value_pop
      && (n_popped > 0 || stack_size == 0))
    {
      rtx n_pop = GEN_INT (n_popped);
      rtx pat;

      /* If this subroutine pops its own args, record that in the call insn
	 if possible, for the sake of frame pointer elimination.  */

      if (valreg)
	pat = GEN_SIBCALL_VALUE_POP (valreg,
				     gen_rtx_MEM (FUNCTION_MODE, funexp),
				     rounded_stack_size_rtx, next_arg_reg,
				     n_pop);
      else
	pat = GEN_SIBCALL_POP (gen_rtx_MEM (FUNCTION_MODE, funexp),
			       rounded_stack_size_rtx, next_arg_reg, n_pop);

      emit_call_insn (pat);
      already_popped = 1;
    }
  else
#endif

#if defined (HAVE_call_pop) && defined (HAVE_call_value_pop)
  /* If the target has "call" or "call_value" insns, then prefer them
     if no arguments are actually popped.  If the target does not have
     "call" or "call_value" insns, then we must use the popping versions
     even if the call has no arguments to pop.  */
#if defined (HAVE_call) && defined (HAVE_call_value)
  if (HAVE_call && HAVE_call_value && HAVE_call_pop && HAVE_call_value_pop
      && n_popped > 0 && ! (ecf_flags & ECF_SP_DEPRESSED))
#else
  if (HAVE_call_pop && HAVE_call_value_pop)
#endif
    {
      rtx n_pop = GEN_INT (n_popped);
      rtx pat;

      /* If this subroutine pops its own args, record that in the call insn
	 if possible, for the sake of frame pointer elimination.  */

      if (valreg)
	pat = GEN_CALL_VALUE_POP (valreg,
				  gen_rtx_MEM (FUNCTION_MODE, funexp),
				  rounded_stack_size_rtx, next_arg_reg, n_pop);
      else
	pat = GEN_CALL_POP (gen_rtx_MEM (FUNCTION_MODE, funexp),
			    rounded_stack_size_rtx, next_arg_reg, n_pop);

      emit_call_insn (pat);
      already_popped = 1;
    }
  else
#endif

#if defined (HAVE_sibcall) && defined (HAVE_sibcall_value)
  if ((ecf_flags & ECF_SIBCALL)
      && HAVE_sibcall && HAVE_sibcall_value)
    {
      if (valreg)
	emit_call_insn (GEN_SIBCALL_VALUE (valreg,
					   gen_rtx_MEM (FUNCTION_MODE, funexp),
					   rounded_stack_size_rtx,
					   next_arg_reg, NULL_RTX));
      else
	emit_call_insn (GEN_SIBCALL (gen_rtx_MEM (FUNCTION_MODE, funexp),
				     rounded_stack_size_rtx, next_arg_reg,
				     struct_value_size_rtx));
    }
  else
#endif

#if defined (HAVE_call) && defined (HAVE_call_value)
  if (HAVE_call && HAVE_call_value)
    {
      if (valreg)
	emit_call_insn (GEN_CALL_VALUE (valreg,
					gen_rtx_MEM (FUNCTION_MODE, funexp),
					rounded_stack_size_rtx, next_arg_reg,
					NULL_RTX));
      else
	emit_call_insn (GEN_CALL (gen_rtx_MEM (FUNCTION_MODE, funexp),
				  rounded_stack_size_rtx, next_arg_reg,
				  struct_value_size_rtx));
    }
  else
#endif
    abort ();

  /* Find the call we just emitted.  */
  call_insn = last_call_insn ();

  /* Mark memory as used for "pure" function call.  */
  if (ecf_flags & ECF_PURE)
    call_fusage
      = gen_rtx_EXPR_LIST
	(VOIDmode,
	 gen_rtx_USE (VOIDmode,
		      gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode))),
	 call_fusage);

  /* Put the register usage information there.  */
  add_function_usage_to (call_insn, call_fusage);

  /* If this is a const call, then set the insn's unchanging bit.  */
  if (ecf_flags & (ECF_CONST | ECF_PURE))
    CONST_OR_PURE_CALL_P (call_insn) = 1;

  /* If this call can't throw, attach a REG_EH_REGION reg note to that
     effect.  */
  if (ecf_flags & ECF_NOTHROW)
    REG_NOTES (call_insn) = gen_rtx_EXPR_LIST (REG_EH_REGION, const0_rtx,
					       REG_NOTES (call_insn));
  else
    note_eh_region_may_contain_throw ();

  if (ecf_flags & ECF_NORETURN)
    REG_NOTES (call_insn) = gen_rtx_EXPR_LIST (REG_NORETURN, const0_rtx,
					       REG_NOTES (call_insn));
  if (ecf_flags & ECF_ALWAYS_RETURN)
    REG_NOTES (call_insn) = gen_rtx_EXPR_LIST (REG_ALWAYS_RETURN, const0_rtx,
					       REG_NOTES (call_insn));

  if (ecf_flags & ECF_RETURNS_TWICE)
    {
      REG_NOTES (call_insn) = gen_rtx_EXPR_LIST (REG_SETJMP, const0_rtx,
					         REG_NOTES (call_insn));
      current_function_calls_setjmp = 1;
    }

  SIBLING_CALL_P (call_insn) = ((ecf_flags & ECF_SIBCALL) != 0);

  /* Restore this now, so that we do defer pops for this call's args
     if the context of the call as a whole permits.  */
  inhibit_defer_pop = old_inhibit_defer_pop;

  if (n_popped > 0)
    {
      if (!already_popped)
	CALL_INSN_FUNCTION_USAGE (call_insn)
	  = gen_rtx_EXPR_LIST (VOIDmode,
			       gen_rtx_CLOBBER (VOIDmode, stack_pointer_rtx),
			       CALL_INSN_FUNCTION_USAGE (call_insn));
      rounded_stack_size -= n_popped;
      rounded_stack_size_rtx = GEN_INT (rounded_stack_size);
      stack_pointer_delta -= n_popped;
    }

  if (!ACCUMULATE_OUTGOING_ARGS)
    {
      /* If returning from the subroutine does not automatically pop the args,
	 we need an instruction to pop them sooner or later.
	 Perhaps do it now; perhaps just record how much space to pop later.

	 If returning from the subroutine does pop the args, indicate that the
	 stack pointer will be changed.  */

      if (rounded_stack_size != 0)
	{
	  if (ecf_flags & (ECF_SP_DEPRESSED | ECF_NORETURN | ECF_LONGJMP))
	    /* Just pretend we did the pop.  */
	    stack_pointer_delta -= rounded_stack_size;
	  else if (flag_defer_pop && inhibit_defer_pop == 0
	      && ! (ecf_flags & (ECF_CONST | ECF_PURE)))
	    pending_stack_adjust += rounded_stack_size;
	  else
	    adjust_stack (rounded_stack_size_rtx);
	}
    }
  /* When we accumulate outgoing args, we must avoid any stack manipulations.
     Restore the stack pointer to its original value now.  Usually
     ACCUMULATE_OUTGOING_ARGS targets don't get here, but there are exceptions.
     On  i386 ACCUMULATE_OUTGOING_ARGS can be enabled on demand, and
     popping variants of functions exist as well.

     ??? We may optimize similar to defer_pop above, but it is
     probably not worthwhile.

     ??? It will be worthwhile to enable combine_stack_adjustments even for
     such machines.  */
  else if (n_popped)
    anti_adjust_stack (GEN_INT (n_popped));
}

/* Determine if the function identified by NAME and FNDECL is one with
   special properties we wish to know about.

   For example, if the function might return more than one time (setjmp), then
   set RETURNS_TWICE to a nonzero value.

   Similarly set LONGJMP for if the function is in the longjmp family.

   Set MAY_BE_ALLOCA for any memory allocation function that might allocate
   space from the stack such as alloca.  */

static int
special_function_p (tree fndecl, int flags)
{
  if (! (flags & ECF_MALLOC)
      && fndecl && DECL_NAME (fndecl)
      && IDENTIFIER_LENGTH (DECL_NAME (fndecl)) <= 17
      /* Exclude functions not at the file scope, or not `extern',
	 since they are not the magic functions we would otherwise
	 think they are.
         FIXME: this should be handled with attributes, not with this
         hacky imitation of DECL_ASSEMBLER_NAME.  It's (also) wrong
         because you can declare fork() inside a function if you
         wish.  */
      && (DECL_CONTEXT (fndecl) == NULL_TREE 
	  || TREE_CODE (DECL_CONTEXT (fndecl)) == TRANSLATION_UNIT_DECL)
      && TREE_PUBLIC (fndecl))
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
      const char *tname = name;

      /* We assume that alloca will always be called by name.  It
	 makes no sense to pass it as a pointer-to-function to
	 anything that does not understand its behavior.  */
      if (((IDENTIFIER_LENGTH (DECL_NAME (fndecl)) == 6
	    && name[0] == 'a'
	    && ! strcmp (name, "alloca"))
	   || (IDENTIFIER_LENGTH (DECL_NAME (fndecl)) == 16
	       && name[0] == '_'
	       && ! strcmp (name, "__builtin_alloca"))))
	flags |= ECF_MAY_BE_ALLOCA;

      /* Disregard prefix _, __ or __x.  */
      if (name[0] == '_')
	{
	  if (name[1] == '_' && name[2] == 'x')
	    tname += 3;
	  else if (name[1] == '_')
	    tname += 2;
	  else
	    tname += 1;
	}

      if (tname[0] == 's')
	{
	  if ((tname[1] == 'e'
	       && (! strcmp (tname, "setjmp")
		   || ! strcmp (tname, "setjmp_syscall")))
	      || (tname[1] == 'i'
		  && ! strcmp (tname, "sigsetjmp"))
	      || (tname[1] == 'a'
		  && ! strcmp (tname, "savectx")))
	    flags |= ECF_RETURNS_TWICE;

	  if (tname[1] == 'i'
	      && ! strcmp (tname, "siglongjmp"))
	    flags |= ECF_LONGJMP;
	}
      else if ((tname[0] == 'q' && tname[1] == 's'
		&& ! strcmp (tname, "qsetjmp"))
	       || (tname[0] == 'v' && tname[1] == 'f'
		   && ! strcmp (tname, "vfork")))
	flags |= ECF_RETURNS_TWICE;

      else if (tname[0] == 'l' && tname[1] == 'o'
	       && ! strcmp (tname, "longjmp"))
	flags |= ECF_LONGJMP;

      else if ((tname[0] == 'f' && tname[1] == 'o'
		&& ! strcmp (tname, "fork"))
	       /* Linux specific: __clone.  check NAME to insist on the
		  leading underscores, to avoid polluting the ISO / POSIX
		  namespace.  */
	       || (name[0] == '_' && name[1] == '_'
		   && ! strcmp (tname, "clone"))
	       || (tname[0] == 'e' && tname[1] == 'x' && tname[2] == 'e'
		   && tname[3] == 'c' && (tname[4] == 'l' || tname[4] == 'v')
		   && (tname[5] == '\0'
		       || ((tname[5] == 'p' || tname[5] == 'e')
			   && tname[6] == '\0'))))
	flags |= ECF_FORK_OR_EXEC;
    }
  return flags;
}

/* Return nonzero when tree represent call to longjmp.  */

int
setjmp_call_p (tree fndecl)
{
  return special_function_p (fndecl, 0) & ECF_RETURNS_TWICE;
}

/* Return true when exp contains alloca call.  */
bool
alloca_call_p (tree exp)
{
  if (TREE_CODE (exp) == CALL_EXPR
      && TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	  == FUNCTION_DECL)
      && (special_function_p (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
			      0) & ECF_MAY_BE_ALLOCA))
    return true;
  return false;
}

/* Detect flags (function attributes) from the function decl or type node.  */

int
flags_from_decl_or_type (tree exp)
{
  int flags = 0;
  tree type = exp;

  if (DECL_P (exp))
    {
      struct cgraph_rtl_info *i = cgraph_rtl_info (exp);
      type = TREE_TYPE (exp);

      if (i)
	{
	  if (i->pure_function)
	    flags |= ECF_PURE | ECF_LIBCALL_BLOCK;
	  if (i->const_function)
	    flags |= ECF_CONST | ECF_LIBCALL_BLOCK;
	}

      /* The function exp may have the `malloc' attribute.  */
      if (DECL_IS_MALLOC (exp))
	flags |= ECF_MALLOC;

      /* The function exp may have the `pure' attribute.  */
      if (DECL_IS_PURE (exp))
	flags |= ECF_PURE | ECF_LIBCALL_BLOCK;

      if (TREE_NOTHROW (exp))
	flags |= ECF_NOTHROW;

      if (TREE_READONLY (exp) && ! TREE_THIS_VOLATILE (exp))
	flags |= ECF_LIBCALL_BLOCK;
    }

  if (TREE_READONLY (exp) && ! TREE_THIS_VOLATILE (exp))
    flags |= ECF_CONST;

  if (TREE_THIS_VOLATILE (exp))
    flags |= ECF_NORETURN;

  /* Mark if the function returns with the stack pointer depressed.   We
     cannot consider it pure or constant in that case.  */
  if (TREE_CODE (type) == FUNCTION_TYPE && TYPE_RETURNS_STACK_DEPRESSED (type))
    {
      flags |= ECF_SP_DEPRESSED;
      flags &= ~(ECF_PURE | ECF_CONST | ECF_LIBCALL_BLOCK);
    }

  return flags;
}

/* Detect flags from a CALL_EXPR.  */

int
call_expr_flags (tree t)
{
  int flags;
  tree decl = get_callee_fndecl (t);

  if (decl)
    flags = flags_from_decl_or_type (decl);
  else
    {
      t = TREE_TYPE (TREE_OPERAND (t, 0));
      if (t && TREE_CODE (t) == POINTER_TYPE)
	flags = flags_from_decl_or_type (TREE_TYPE (t));
      else
	flags = 0;
    }

  return flags;
}

/* Precompute all register parameters as described by ARGS, storing values
   into fields within the ARGS array.

   NUM_ACTUALS indicates the total number elements in the ARGS array.

   Set REG_PARM_SEEN if we encounter a register parameter.  */

static void
precompute_register_parameters (int num_actuals, struct arg_data *args, int *reg_parm_seen)
{
  int i;

  *reg_parm_seen = 0;

  for (i = 0; i < num_actuals; i++)
    if (args[i].reg != 0 && ! args[i].pass_on_stack)
      {
	*reg_parm_seen = 1;

	if (args[i].value == 0)
	  {
	    push_temp_slots ();
	    args[i].value = expand_expr (args[i].tree_value, NULL_RTX,
					 VOIDmode, 0);
	    preserve_temp_slots (args[i].value);
	    pop_temp_slots ();

	    /* ANSI doesn't require a sequence point here,
	       but PCC has one, so this will avoid some problems.  */
	    emit_queue ();
	  }

	/* If the value is a non-legitimate constant, force it into a
	   pseudo now.  TLS symbols sometimes need a call to resolve.  */
	if (CONSTANT_P (args[i].value)
	    && !LEGITIMATE_CONSTANT_P (args[i].value))
	  args[i].value = force_reg (args[i].mode, args[i].value);

	/* If we are to promote the function arg to a wider mode,
	   do it now.  */

	if (args[i].mode != TYPE_MODE (TREE_TYPE (args[i].tree_value)))
	  args[i].value
	    = convert_modes (args[i].mode,
			     TYPE_MODE (TREE_TYPE (args[i].tree_value)),
			     args[i].value, args[i].unsignedp);

	/* If the value is expensive, and we are inside an appropriately
	   short loop, put the value into a pseudo and then put the pseudo
	   into the hard reg.

	   For small register classes, also do this if this call uses
	   register parameters.  This is to avoid reload conflicts while
	   loading the parameters registers.  */

	if ((! (GET_CODE (args[i].value) == REG
		|| (GET_CODE (args[i].value) == SUBREG
		    && GET_CODE (SUBREG_REG (args[i].value)) == REG)))
	    && args[i].mode != BLKmode
	    && rtx_cost (args[i].value, SET) > COSTS_N_INSNS (1)
	    && ((SMALL_REGISTER_CLASSES && *reg_parm_seen)
		|| preserve_subexpressions_p ()))
	  args[i].value = copy_to_mode_reg (args[i].mode, args[i].value);
      }
}

#ifdef REG_PARM_STACK_SPACE

  /* The argument list is the property of the called routine and it
     may clobber it.  If the fixed area has been used for previous
     parameters, we must save and restore it.  */

static rtx
save_fixed_argument_area (int reg_parm_stack_space, rtx argblock, int *low_to_save, int *high_to_save)
{
  int low;
  int high;

  /* Compute the boundary of the area that needs to be saved, if any.  */
  high = reg_parm_stack_space;
#ifdef ARGS_GROW_DOWNWARD
  high += 1;
#endif
  if (high > highest_outgoing_arg_in_use)
    high = highest_outgoing_arg_in_use;

  for (low = 0; low < high; low++)
    if (stack_usage_map[low] != 0)
      {
	int num_to_save;
	enum machine_mode save_mode;
	int delta;
	rtx stack_area;
	rtx save_area;

	while (stack_usage_map[--high] == 0)
	  ;

	*low_to_save = low;
	*high_to_save = high;

	num_to_save = high - low + 1;
	save_mode = mode_for_size (num_to_save * BITS_PER_UNIT, MODE_INT, 1);

	/* If we don't have the required alignment, must do this
	   in BLKmode.  */
	if ((low & (MIN (GET_MODE_SIZE (save_mode),
			 BIGGEST_ALIGNMENT / UNITS_PER_WORD) - 1)))
	  save_mode = BLKmode;

#ifdef ARGS_GROW_DOWNWARD
	delta = -high;
#else
	delta = low;
#endif
	stack_area = gen_rtx_MEM (save_mode,
				  memory_address (save_mode,
						  plus_constant (argblock,
								 delta)));

	set_mem_align (stack_area, PARM_BOUNDARY);
	if (save_mode == BLKmode)
	  {
	    save_area = assign_stack_temp (BLKmode, num_to_save, 0);
	    emit_block_move (validize_mem (save_area), stack_area,
			     GEN_INT (num_to_save), BLOCK_OP_CALL_PARM);
	  }
	else
	  {
	    save_area = gen_reg_rtx (save_mode);
	    emit_move_insn (save_area, stack_area);
	  }

	return save_area;
      }

  return NULL_RTX;
}

static void
restore_fixed_argument_area (rtx save_area, rtx argblock, int high_to_save, int low_to_save)
{
  enum machine_mode save_mode = GET_MODE (save_area);
  int delta;
  rtx stack_area;

#ifdef ARGS_GROW_DOWNWARD
  delta = -high_to_save;
#else
  delta = low_to_save;
#endif
  stack_area = gen_rtx_MEM (save_mode,
			    memory_address (save_mode,
					    plus_constant (argblock, delta)));
  set_mem_align (stack_area, PARM_BOUNDARY);

  if (save_mode != BLKmode)
    emit_move_insn (stack_area, save_area);
  else
    emit_block_move (stack_area, validize_mem (save_area),
		     GEN_INT (high_to_save - low_to_save + 1),
		     BLOCK_OP_CALL_PARM);
}
#endif /* REG_PARM_STACK_SPACE */

/* If any elements in ARGS refer to parameters that are to be passed in
   registers, but not in memory, and whose alignment does not permit a
   direct copy into registers.  Copy the values into a group of pseudos
   which we will later copy into the appropriate hard registers.

   Pseudos for each unaligned argument will be stored into the array
   args[argnum].aligned_regs.  The caller is responsible for deallocating
   the aligned_regs array if it is nonzero.  */

static void
store_unaligned_arguments_into_pseudos (struct arg_data *args, int num_actuals)
{
  int i, j;

  for (i = 0; i < num_actuals; i++)
    if (args[i].reg != 0 && ! args[i].pass_on_stack
	&& args[i].mode == BLKmode
	&& (TYPE_ALIGN (TREE_TYPE (args[i].tree_value))
	    < (unsigned int) MIN (BIGGEST_ALIGNMENT, BITS_PER_WORD)))
      {
	int bytes = int_size_in_bytes (TREE_TYPE (args[i].tree_value));
	int nregs = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
	int endian_correction = 0;

	args[i].n_aligned_regs = args[i].partial ? args[i].partial : nregs;
	args[i].aligned_regs = xmalloc (sizeof (rtx) * args[i].n_aligned_regs);

	/* Structures smaller than a word are normally aligned to the
	   least significant byte.  On a BYTES_BIG_ENDIAN machine,
	   this means we must skip the empty high order bytes when
	   calculating the bit offset.  */
	if (bytes < UNITS_PER_WORD
#ifdef BLOCK_REG_PADDING
	    && (BLOCK_REG_PADDING (args[i].mode,
				   TREE_TYPE (args[i].tree_value), 1)
		== downward)
#else
	    && BYTES_BIG_ENDIAN
#endif
	    )
	  endian_correction = BITS_PER_WORD - bytes * BITS_PER_UNIT;

	for (j = 0; j < args[i].n_aligned_regs; j++)
	  {
	    rtx reg = gen_reg_rtx (word_mode);
	    rtx word = operand_subword_force (args[i].value, j, BLKmode);
	    int bitsize = MIN (bytes * BITS_PER_UNIT, BITS_PER_WORD);

	    args[i].aligned_regs[j] = reg;
	    word = extract_bit_field (word, bitsize, 0, 1, NULL_RTX,
				      word_mode, word_mode, BITS_PER_WORD);

	    /* There is no need to restrict this code to loading items
	       in TYPE_ALIGN sized hunks.  The bitfield instructions can
	       load up entire word sized registers efficiently.

	       ??? This may not be needed anymore.
	       We use to emit a clobber here but that doesn't let later
	       passes optimize the instructions we emit.  By storing 0 into
	       the register later passes know the first AND to zero out the
	       bitfield being set in the register is unnecessary.  The store
	       of 0 will be deleted as will at least the first AND.  */

	    emit_move_insn (reg, const0_rtx);

	    bytes -= bitsize / BITS_PER_UNIT;
	    store_bit_field (reg, bitsize, endian_correction, word_mode,
			     word, BITS_PER_WORD);
	  }
      }
}

/* Fill in ARGS_SIZE and ARGS array based on the parameters found in
   ACTPARMS.

   NUM_ACTUALS is the total number of parameters.

   N_NAMED_ARGS is the total number of named arguments.

   FNDECL is the tree code for the target of this call (if known)

   ARGS_SO_FAR holds state needed by the target to know where to place
   the next argument.

   REG_PARM_STACK_SPACE is the number of bytes of stack space reserved
   for arguments which are passed in registers.

   OLD_STACK_LEVEL is a pointer to an rtx which olds the old stack level
   and may be modified by this routine.

   OLD_PENDING_ADJ, MUST_PREALLOCATE and FLAGS are pointers to integer
   flags which may may be modified by this routine. 

   CALL_FROM_THUNK_P is true if this call is the jump from a thunk to
   the thunked-to function.  */

static void
initialize_argument_information (int num_actuals ATTRIBUTE_UNUSED,
				 struct arg_data *args,
				 struct args_size *args_size,
				 int n_named_args ATTRIBUTE_UNUSED,
				 tree actparms, tree fndecl,
				 CUMULATIVE_ARGS *args_so_far,
				 int reg_parm_stack_space,
				 rtx *old_stack_level, int *old_pending_adj,
				 int *must_preallocate, int *ecf_flags,
				 bool call_from_thunk_p)
{
  /* 1 if scanning parms front to back, -1 if scanning back to front.  */
  int inc;

  /* Count arg position in order args appear.  */
  int argpos;

  int i;
  tree p;

  args_size->constant = 0;
  args_size->var = 0;

  /* In this loop, we consider args in the order they are written.
     We fill up ARGS from the front or from the back if necessary
     so that in any case the first arg to be pushed ends up at the front.  */

  if (PUSH_ARGS_REVERSED)
    {
      i = num_actuals - 1, inc = -1;
      /* In this case, must reverse order of args
	 so that we compute and push the last arg first.  */
    }
  else
    {
      i = 0, inc = 1;
    }

  /* I counts args in order (to be) pushed; ARGPOS counts in order written.  */
  for (p = actparms, argpos = 0; p; p = TREE_CHAIN (p), i += inc, argpos++)
    {
      tree type = TREE_TYPE (TREE_VALUE (p));
      int unsignedp;
      enum machine_mode mode;

      args[i].tree_value = TREE_VALUE (p);

      /* Replace erroneous argument with constant zero.  */
      if (type == error_mark_node || !COMPLETE_TYPE_P (type))
	args[i].tree_value = integer_zero_node, type = integer_type_node;

      /* If TYPE is a transparent union, pass things the way we would
	 pass the first field of the union.  We have already verified that
	 the modes are the same.  */
      if (TREE_CODE (type) == UNION_TYPE && TYPE_TRANSPARENT_UNION (type))
	type = TREE_TYPE (TYPE_FIELDS (type));

      /* Decide where to pass this arg.

	 args[i].reg is nonzero if all or part is passed in registers.

	 args[i].partial is nonzero if part but not all is passed in registers,
	 and the exact value says how many words are passed in registers.

	 args[i].pass_on_stack is nonzero if the argument must at least be
	 computed on the stack.  It may then be loaded back into registers
	 if args[i].reg is nonzero.

	 These decisions are driven by the FUNCTION_... macros and must agree
	 with those made by function.c.  */

      /* See if this argument should be passed by invisible reference.  */
      if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (type))
	  || TREE_ADDRESSABLE (type)
#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
	  || FUNCTION_ARG_PASS_BY_REFERENCE (*args_so_far, TYPE_MODE (type),
					     type, argpos < n_named_args)
#endif
	  )
	{
	  /* If we're compiling a thunk, pass through invisible
             references instead of making a copy.  */
	  if (call_from_thunk_p
#ifdef FUNCTION_ARG_CALLEE_COPIES
	      || (FUNCTION_ARG_CALLEE_COPIES (*args_so_far, TYPE_MODE (type),
					     type, argpos < n_named_args)
		  /* If it's in a register, we must make a copy of it too.  */
		  /* ??? Is this a sufficient test?  Is there a better one? */
		  && !(TREE_CODE (args[i].tree_value) == VAR_DECL
		       && REG_P (DECL_RTL (args[i].tree_value)))
		  && ! TREE_ADDRESSABLE (type))
#endif
	      )
	    {
	      /* C++ uses a TARGET_EXPR to indicate that we want to make a
	         new object from the argument.  If we are passing by
	         invisible reference, the callee will do that for us, so we
	         can strip off the TARGET_EXPR.  This is not always safe,
	         but it is safe in the only case where this is a useful
	         optimization; namely, when the argument is a plain object.
	         In that case, the frontend is just asking the backend to
	         make a bitwise copy of the argument.  */

	      if (TREE_CODE (args[i].tree_value) == TARGET_EXPR
		  && (DECL_P (TREE_OPERAND (args[i].tree_value, 1)))
		  && ! REG_P (DECL_RTL (TREE_OPERAND (args[i].tree_value, 1))))
		args[i].tree_value = TREE_OPERAND (args[i].tree_value, 1);

	      args[i].tree_value = build1 (ADDR_EXPR,
					   build_pointer_type (type),
					   args[i].tree_value);
	      type = build_pointer_type (type);
	    }
	  else if (TREE_CODE (args[i].tree_value) == TARGET_EXPR)
	    {
	      /* In the V3 C++ ABI, parameters are destroyed in the caller.
		 We implement this by passing the address of the temporary
	         rather than expanding it into another allocated slot.  */
	      args[i].tree_value = build1 (ADDR_EXPR,
					   build_pointer_type (type),
					   args[i].tree_value);
	      type = build_pointer_type (type);
	    }
	  else
	    {
	      /* We make a copy of the object and pass the address to the
		 function being called.  */
	      rtx copy;

	      if (!COMPLETE_TYPE_P (type)
		  || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
		  || (flag_stack_check && ! STACK_CHECK_BUILTIN
		      && (0 < compare_tree_int (TYPE_SIZE_UNIT (type),
						STACK_CHECK_MAX_VAR_SIZE))))
		{
		  /* This is a variable-sized object.  Make space on the stack
		     for it.  */
		  rtx size_rtx = expr_size (TREE_VALUE (p));

		  if (*old_stack_level == 0)
		    {
		      emit_stack_save (SAVE_BLOCK, old_stack_level, NULL_RTX);
		      *old_pending_adj = pending_stack_adjust;
		      pending_stack_adjust = 0;
		    }

		  copy = gen_rtx_MEM (BLKmode,
				      allocate_dynamic_stack_space
				      (size_rtx, NULL_RTX, TYPE_ALIGN (type)));
		  set_mem_attributes (copy, type, 1);
		}
	      else
		copy = assign_temp (type, 0, 1, 0);

	      store_expr (args[i].tree_value, copy, 0);
	      *ecf_flags &= ~(ECF_CONST | ECF_PURE | ECF_LIBCALL_BLOCK);

	      args[i].tree_value = build1 (ADDR_EXPR,
					   build_pointer_type (type),
					   make_tree (type, copy));
	      type = build_pointer_type (type);
	    }
	}

      mode = TYPE_MODE (type);
      unsignedp = TREE_UNSIGNED (type);

      if (targetm.calls.promote_function_args (fndecl ? TREE_TYPE (fndecl) : 0))
	mode = promote_mode (type, mode, &unsignedp, 1);

      args[i].unsignedp = unsignedp;
      args[i].mode = mode;

      args[i].reg = FUNCTION_ARG (*args_so_far, mode, type,
				  argpos < n_named_args);
#ifdef FUNCTION_INCOMING_ARG
      /* If this is a sibling call and the machine has register windows, the
	 register window has to be unwinded before calling the routine, so
	 arguments have to go into the incoming registers.  */
      args[i].tail_call_reg = FUNCTION_INCOMING_ARG (*args_so_far, mode, type,
						     argpos < n_named_args);
#else
      args[i].tail_call_reg = args[i].reg;
#endif

#ifdef FUNCTION_ARG_PARTIAL_NREGS
      if (args[i].reg)
	args[i].partial
	  = FUNCTION_ARG_PARTIAL_NREGS (*args_so_far, mode, type,
					argpos < n_named_args);
#endif

      args[i].pass_on_stack = MUST_PASS_IN_STACK (mode, type);

      /* If FUNCTION_ARG returned a (parallel [(expr_list (nil) ...) ...]),
	 it means that we are to pass this arg in the register(s) designated
	 by the PARALLEL, but also to pass it in the stack.  */
      if (args[i].reg && GET_CODE (args[i].reg) == PARALLEL
	  && XEXP (XVECEXP (args[i].reg, 0, 0), 0) == 0)
	args[i].pass_on_stack = 1;

      /* If this is an addressable type, we must preallocate the stack
	 since we must evaluate the object into its final location.

	 If this is to be passed in both registers and the stack, it is simpler
	 to preallocate.  */
      if (TREE_ADDRESSABLE (type)
	  || (args[i].pass_on_stack && args[i].reg != 0))
	*must_preallocate = 1;

      /* If this is an addressable type, we cannot pre-evaluate it.  Thus,
	 we cannot consider this function call constant.  */
      if (TREE_ADDRESSABLE (type))
	*ecf_flags &= ~ECF_LIBCALL_BLOCK;

      /* Compute the stack-size of this argument.  */
      if (args[i].reg == 0 || args[i].partial != 0
	  || reg_parm_stack_space > 0
	  || args[i].pass_on_stack)
	locate_and_pad_parm (mode, type,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			     1,
#else
			     args[i].reg != 0,
#endif
			     args[i].pass_on_stack ? 0 : args[i].partial,
			     fndecl, args_size, &args[i].locate);
#ifdef BLOCK_REG_PADDING
      else
	/* The argument is passed entirely in registers.  See at which
	   end it should be padded.  */
	args[i].locate.where_pad =
	  BLOCK_REG_PADDING (mode, type,
			     int_size_in_bytes (type) <= UNITS_PER_WORD);
#endif

      /* Update ARGS_SIZE, the total stack space for args so far.  */

      args_size->constant += args[i].locate.size.constant;
      if (args[i].locate.size.var)
	ADD_PARM_SIZE (*args_size, args[i].locate.size.var);

      /* Increment ARGS_SO_FAR, which has info about which arg-registers
	 have been used, etc.  */

      FUNCTION_ARG_ADVANCE (*args_so_far, TYPE_MODE (type), type,
			    argpos < n_named_args);
    }
}

/* Update ARGS_SIZE to contain the total size for the argument block.
   Return the original constant component of the argument block's size.

   REG_PARM_STACK_SPACE holds the number of bytes of stack space reserved
   for arguments passed in registers.  */

static int
compute_argument_block_size (int reg_parm_stack_space,
			     struct args_size *args_size,
			     int preferred_stack_boundary ATTRIBUTE_UNUSED)
{
  int unadjusted_args_size = args_size->constant;

  /* For accumulate outgoing args mode we don't need to align, since the frame
     will be already aligned.  Align to STACK_BOUNDARY in order to prevent
     backends from generating misaligned frame sizes.  */
  if (ACCUMULATE_OUTGOING_ARGS && preferred_stack_boundary > STACK_BOUNDARY)
    preferred_stack_boundary = STACK_BOUNDARY;

  /* Compute the actual size of the argument block required.  The variable
     and constant sizes must be combined, the size may have to be rounded,
     and there may be a minimum required size.  */

  if (args_size->var)
    {
      args_size->var = ARGS_SIZE_TREE (*args_size);
      args_size->constant = 0;

      preferred_stack_boundary /= BITS_PER_UNIT;
      if (preferred_stack_boundary > 1)
	{
	  /* We don't handle this case yet.  To handle it correctly we have
	     to add the delta, round and subtract the delta.
	     Currently no machine description requires this support.  */
	  if (stack_pointer_delta & (preferred_stack_boundary - 1))
	    abort ();
	  args_size->var = round_up (args_size->var, preferred_stack_boundary);
	}

      if (reg_parm_stack_space > 0)
	{
	  args_size->var
	    = size_binop (MAX_EXPR, args_size->var,
			  ssize_int (reg_parm_stack_space));

#ifndef OUTGOING_REG_PARM_STACK_SPACE
	  /* The area corresponding to register parameters is not to count in
	     the size of the block we need.  So make the adjustment.  */
	  args_size->var
	    = size_binop (MINUS_EXPR, args_size->var,
			  ssize_int (reg_parm_stack_space));
#endif
	}
    }
  else
    {
      preferred_stack_boundary /= BITS_PER_UNIT;
      if (preferred_stack_boundary < 1)
	preferred_stack_boundary = 1;
      args_size->constant = (((args_size->constant
			       + stack_pointer_delta
			       + preferred_stack_boundary - 1)
			      / preferred_stack_boundary
			      * preferred_stack_boundary)
			     - stack_pointer_delta);

      args_size->constant = MAX (args_size->constant,
				 reg_parm_stack_space);

#ifdef MAYBE_REG_PARM_STACK_SPACE
      if (reg_parm_stack_space == 0)
	args_size->constant = 0;
#endif

#ifndef OUTGOING_REG_PARM_STACK_SPACE
      args_size->constant -= reg_parm_stack_space;
#endif
    }
  return unadjusted_args_size;
}

/* Precompute parameters as needed for a function call.

   FLAGS is mask of ECF_* constants.

   NUM_ACTUALS is the number of arguments.

   ARGS is an array containing information for each argument; this
   routine fills in the INITIAL_VALUE and VALUE fields for each
   precomputed argument.  */

static void
precompute_arguments (int flags, int num_actuals, struct arg_data *args)
{
  int i;

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
     which have already been stored into the stack.  (we have code to avoid
     such case by saving the outgoing stack arguments, but it results in
     worse code)  */

  for (i = 0; i < num_actuals; i++)
    if ((flags & ECF_LIBCALL_BLOCK)
	|| calls_function (args[i].tree_value, !ACCUMULATE_OUTGOING_ARGS))
      {
	enum machine_mode mode;

	/* If this is an addressable type, we cannot pre-evaluate it.  */
	if (TREE_ADDRESSABLE (TREE_TYPE (args[i].tree_value)))
	  abort ();

	args[i].value
	  = expand_expr (args[i].tree_value, NULL_RTX, VOIDmode, 0);

	/* ANSI doesn't require a sequence point here,
	   but PCC has one, so this will avoid some problems.  */
	emit_queue ();

	args[i].initial_value = args[i].value
	  = protect_from_queue (args[i].value, 0);

	mode = TYPE_MODE (TREE_TYPE (args[i].tree_value));
	if (mode != args[i].mode)
	  {
	    args[i].value
	      = convert_modes (args[i].mode, mode,
			       args[i].value, args[i].unsignedp);
#ifdef PROMOTE_FOR_CALL_ONLY
	    /* CSE will replace this only if it contains args[i].value
	       pseudo, so convert it down to the declared mode using
	       a SUBREG.  */
	    if (GET_CODE (args[i].value) == REG
		&& GET_MODE_CLASS (args[i].mode) == MODE_INT)
	      {
		args[i].initial_value
		  = gen_lowpart_SUBREG (mode, args[i].value);
		SUBREG_PROMOTED_VAR_P (args[i].initial_value) = 1;
		SUBREG_PROMOTED_UNSIGNED_SET (args[i].initial_value,
		  args[i].unsignedp);
	      }
#endif
	  }
      }
}

/* Given the current state of MUST_PREALLOCATE and information about
   arguments to a function call in NUM_ACTUALS, ARGS and ARGS_SIZE,
   compute and return the final value for MUST_PREALLOCATE.  */

static int
finalize_must_preallocate (int must_preallocate, int num_actuals, struct arg_data *args, struct args_size *args_size)
{
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
      int i;

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

      if (copy_to_evaluate_size * 2 >= args_size->constant
	  && args_size->constant > 0)
	must_preallocate = 1;
    }
  return must_preallocate;
}

/* If we preallocated stack space, compute the address of each argument
   and store it into the ARGS array.

   We need not ensure it is a valid memory address here; it will be
   validized when it is used.

   ARGBLOCK is an rtx for the address of the outgoing arguments.  */

static void
compute_argument_addresses (struct arg_data *args, rtx argblock, int num_actuals)
{
  if (argblock)
    {
      rtx arg_reg = argblock;
      int i, arg_offset = 0;

      if (GET_CODE (argblock) == PLUS)
	arg_reg = XEXP (argblock, 0), arg_offset = INTVAL (XEXP (argblock, 1));

      for (i = 0; i < num_actuals; i++)
	{
	  rtx offset = ARGS_SIZE_RTX (args[i].locate.offset);
	  rtx slot_offset = ARGS_SIZE_RTX (args[i].locate.slot_offset);
	  rtx addr;

	  /* Skip this parm if it will not be passed on the stack.  */
	  if (! args[i].pass_on_stack && args[i].reg != 0)
	    continue;

	  if (GET_CODE (offset) == CONST_INT)
	    addr = plus_constant (arg_reg, INTVAL (offset));
	  else
	    addr = gen_rtx_PLUS (Pmode, arg_reg, offset);

	  addr = plus_constant (addr, arg_offset);
	  args[i].stack = gen_rtx_MEM (args[i].mode, addr);
	  set_mem_align (args[i].stack, PARM_BOUNDARY);
	  set_mem_attributes (args[i].stack,
			      TREE_TYPE (args[i].tree_value), 1);

	  if (GET_CODE (slot_offset) == CONST_INT)
	    addr = plus_constant (arg_reg, INTVAL (slot_offset));
	  else
	    addr = gen_rtx_PLUS (Pmode, arg_reg, slot_offset);

	  addr = plus_constant (addr, arg_offset);
	  args[i].stack_slot = gen_rtx_MEM (args[i].mode, addr);
	  set_mem_align (args[i].stack_slot, PARM_BOUNDARY);
	  set_mem_attributes (args[i].stack_slot,
			      TREE_TYPE (args[i].tree_value), 1);

	  /* Function incoming arguments may overlap with sibling call
	     outgoing arguments and we cannot allow reordering of reads
	     from function arguments with stores to outgoing arguments
	     of sibling calls.  */
	  set_mem_alias_set (args[i].stack, 0);
	  set_mem_alias_set (args[i].stack_slot, 0);
	}
    }
}

/* Given a FNDECL and EXP, return an rtx suitable for use as a target address
   in a call instruction.

   FNDECL is the tree node for the target function.  For an indirect call
   FNDECL will be NULL_TREE.

   ADDR is the operand 0 of CALL_EXPR for this call.  */

static rtx
rtx_for_function_call (tree fndecl, tree addr)
{
  rtx funexp;

  /* Get the function to call, in the form of RTL.  */
  if (fndecl)
    {
      /* If this is the first use of the function, see if we need to
	 make an external definition for it.  */
      if (! TREE_USED (fndecl))
	{
	  assemble_external (fndecl);
	  TREE_USED (fndecl) = 1;
	}

      /* Get a SYMBOL_REF rtx for the function address.  */
      funexp = XEXP (DECL_RTL (fndecl), 0);
    }
  else
    /* Generate an rtx (probably a pseudo-register) for the address.  */
    {
      push_temp_slots ();
      funexp = expand_expr (addr, NULL_RTX, VOIDmode, 0);
      pop_temp_slots ();	/* FUNEXP can't be BLKmode.  */
      emit_queue ();
    }
  return funexp;
}

/* Do the register loads required for any wholly-register parms or any
   parms which are passed both on the stack and in a register.  Their
   expressions were already evaluated.

   Mark all register-parms as living through the call, putting these USE
   insns in the CALL_INSN_FUNCTION_USAGE field.

   When IS_SIBCALL, perform the check_sibcall_overlap_argument_overlap
   checking, setting *SIBCALL_FAILURE if appropriate.  */

static void
load_register_parameters (struct arg_data *args, int num_actuals,
			  rtx *call_fusage, int flags, int is_sibcall,
			  int *sibcall_failure)
{
  int i, j;

  for (i = 0; i < num_actuals; i++)
    {
      rtx reg = ((flags & ECF_SIBCALL)
		 ? args[i].tail_call_reg : args[i].reg);
      if (reg)
	{
	  int partial = args[i].partial;
	  int nregs;
	  int size = 0;
	  rtx before_arg = get_last_insn ();
	  /* Set to non-negative if must move a word at a time, even if just
	     one word (e.g, partial == 1 && mode == DFmode).  Set to -1 if
	     we just use a normal move insn.  This value can be zero if the
	     argument is a zero size structure with no fields.  */
	  nregs = -1;
	  if (partial)
	    nregs = partial;
	  else if (TYPE_MODE (TREE_TYPE (args[i].tree_value)) == BLKmode)
	    {
	      size = int_size_in_bytes (TREE_TYPE (args[i].tree_value));
	      nregs = (size + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
	    }
	  else
	    size = GET_MODE_SIZE (args[i].mode);

	  /* Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */

	  if (GET_CODE (reg) == PARALLEL)
	    {
	      tree type = TREE_TYPE (args[i].tree_value);
	      emit_group_load (reg, args[i].value, type,
			       int_size_in_bytes (type));
	    }

	  /* If simple case, just do move.  If normal partial, store_one_arg
	     has already loaded the register for us.  In all other cases,
	     load the register(s) from memory.  */

	  else if (nregs == -1)
	    {
	      emit_move_insn (reg, args[i].value);
#ifdef BLOCK_REG_PADDING
	      /* Handle case where we have a value that needs shifting
		 up to the msb.  eg. a QImode value and we're padding
		 upward on a BYTES_BIG_ENDIAN machine.  */
	      if (size < UNITS_PER_WORD
		  && (args[i].locate.where_pad
		      == (BYTES_BIG_ENDIAN ? upward : downward)))
		{
		  rtx x;
		  int shift = (UNITS_PER_WORD - size) * BITS_PER_UNIT;

		  /* Assigning REG here rather than a temp makes CALL_FUSAGE
		     report the whole reg as used.  Strictly speaking, the
		     call only uses SIZE bytes at the msb end, but it doesn't
		     seem worth generating rtl to say that.  */
		  reg = gen_rtx_REG (word_mode, REGNO (reg));
		  x = expand_binop (word_mode, ashl_optab, reg,
				    GEN_INT (shift), reg, 1, OPTAB_WIDEN);
		  if (x != reg)
		    emit_move_insn (reg, x);
		}
#endif
	    }

	  /* If we have pre-computed the values to put in the registers in
	     the case of non-aligned structures, copy them in now.  */

	  else if (args[i].n_aligned_regs != 0)
	    for (j = 0; j < args[i].n_aligned_regs; j++)
	      emit_move_insn (gen_rtx_REG (word_mode, REGNO (reg) + j),
			      args[i].aligned_regs[j]);

	  else if (partial == 0 || args[i].pass_on_stack)
	    {
	      rtx mem = validize_mem (args[i].value);

#ifdef BLOCK_REG_PADDING
	      /* Handle a BLKmode that needs shifting.  */
	      if (nregs == 1 && size < UNITS_PER_WORD
		  && args[i].locate.where_pad == downward)
		{
		  rtx tem = operand_subword_force (mem, 0, args[i].mode);
		  rtx ri = gen_rtx_REG (word_mode, REGNO (reg));
		  rtx x = gen_reg_rtx (word_mode);
		  int shift = (UNITS_PER_WORD - size) * BITS_PER_UNIT;
		  optab dir = BYTES_BIG_ENDIAN ? lshr_optab : ashl_optab;

		  emit_move_insn (x, tem);
		  x = expand_binop (word_mode, dir, x, GEN_INT (shift),
				    ri, 1, OPTAB_WIDEN);
		  if (x != ri)
		    emit_move_insn (ri, x);
		}
	      else
#endif
		move_block_to_reg (REGNO (reg), mem, nregs, args[i].mode);
	    }

	  /* When a parameter is a block, and perhaps in other cases, it is
	     possible that it did a load from an argument slot that was
	     already clobbered.  */
	  if (is_sibcall
	      && check_sibcall_argument_overlap (before_arg, &args[i], 0))
	    *sibcall_failure = 1;

	  /* Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */
	  if (GET_CODE (reg) == PARALLEL)
	    use_group_regs (call_fusage, reg);
	  else if (nregs == -1)
	    use_reg (call_fusage, reg);
	  else
	    use_regs (call_fusage, REGNO (reg), nregs == 0 ? 1 : nregs);
	}
    }
}

/* Try to integrate function.  See expand_inline_function for documentation
   about the parameters.  */

static rtx
try_to_integrate (tree fndecl, tree actparms, rtx target, int ignore,
		  tree type, rtx structure_value_addr)
{
  rtx temp;
  rtx before_call;
  int i;
  rtx old_stack_level = 0;
  int reg_parm_stack_space = 0;

#ifdef REG_PARM_STACK_SPACE
#ifdef MAYBE_REG_PARM_STACK_SPACE
  reg_parm_stack_space = MAYBE_REG_PARM_STACK_SPACE;
#else
  reg_parm_stack_space = REG_PARM_STACK_SPACE (fndecl);
#endif
#endif

  before_call = get_last_insn ();

  timevar_push (TV_INTEGRATION);

  temp = expand_inline_function (fndecl, actparms, target,
				 ignore, type,
				 structure_value_addr);

  timevar_pop (TV_INTEGRATION);

  /* If inlining succeeded, return.  */
  if (temp != (rtx) (size_t) - 1)
    {
      if (ACCUMULATE_OUTGOING_ARGS)
	{
	  /* If the outgoing argument list must be preserved, push
	     the stack before executing the inlined function if it
	     makes any calls.  */

	  i = reg_parm_stack_space;
	  if (i > highest_outgoing_arg_in_use)
	    i = highest_outgoing_arg_in_use;
	  while (--i >= 0 && stack_usage_map[i] == 0)
	    ;

	  if (stack_arg_under_construction || i >= 0)
	    {
	      rtx first_insn
		= before_call ? NEXT_INSN (before_call) : get_insns ();
	      rtx insn = NULL_RTX, seq;

	      /* Look for a call in the inline function code.
	         If DECL_SAVED_INSNS (fndecl)->outgoing_args_size is
	         nonzero then there is a call and it is not necessary
	         to scan the insns.  */

	      if (DECL_SAVED_INSNS (fndecl)->outgoing_args_size == 0)
		for (insn = first_insn; insn; insn = NEXT_INSN (insn))
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
		     safely constructed.

		     Add the stack space reserved for register arguments, if
		     any, in the inline function.  What is really needed is the
		     largest value of reg_parm_stack_space in the inline
		     function, but that is not available.  Using the current
		     value of reg_parm_stack_space is wrong, but gives
		     correct results on all supported machines.  */

		  int adjust = (DECL_SAVED_INSNS (fndecl)->outgoing_args_size
				+ reg_parm_stack_space);

		  start_sequence ();
		  emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);
		  allocate_dynamic_stack_space (GEN_INT (adjust),
						NULL_RTX, BITS_PER_UNIT);
		  seq = get_insns ();
		  end_sequence ();
		  emit_insn_before (seq, first_insn);
		  emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);
		}
	    }
	}

      /* If the result is equivalent to TARGET, return TARGET to simplify
         checks in store_expr.  They can be equivalent but not equal in the
         case of a function that returns BLKmode.  */
      if (temp != target && rtx_equal_p (temp, target))
	return target;
      return temp;
    }

  /* If inlining failed, mark FNDECL as needing to be compiled
     separately after all.  If function was declared inline,
     give a warning.  */
  if (DECL_INLINE (fndecl) && warn_inline && !flag_no_inline
      && optimize > 0 && !TREE_ADDRESSABLE (fndecl))
    {
      warning ("%Jinlining failed in call to '%F'", fndecl, fndecl);
      warning ("called from here");
    }
  (*lang_hooks.mark_addressable) (fndecl);
  return (rtx) (size_t) - 1;
}

/* We need to pop PENDING_STACK_ADJUST bytes.  But, if the arguments
   wouldn't fill up an even multiple of PREFERRED_UNIT_STACK_BOUNDARY
   bytes, then we would need to push some additional bytes to pad the
   arguments.  So, we compute an adjust to the stack pointer for an
   amount that will leave the stack under-aligned by UNADJUSTED_ARGS_SIZE
   bytes.  Then, when the arguments are pushed the stack will be perfectly
   aligned.  ARGS_SIZE->CONSTANT is set to the number of bytes that should
   be popped after the call.  Returns the adjustment.  */

static int
combine_pending_stack_adjustment_and_call (int unadjusted_args_size,
					   struct args_size *args_size,
					   int preferred_unit_stack_boundary)
{
  /* The number of bytes to pop so that the stack will be
     under-aligned by UNADJUSTED_ARGS_SIZE bytes.  */
  HOST_WIDE_INT adjustment;
  /* The alignment of the stack after the arguments are pushed, if we
     just pushed the arguments without adjust the stack here.  */
  HOST_WIDE_INT unadjusted_alignment;

  unadjusted_alignment
    = ((stack_pointer_delta + unadjusted_args_size)
       % preferred_unit_stack_boundary);

  /* We want to get rid of as many of the PENDING_STACK_ADJUST bytes
     as possible -- leaving just enough left to cancel out the
     UNADJUSTED_ALIGNMENT.  In other words, we want to ensure that the
     PENDING_STACK_ADJUST is non-negative, and congruent to
     -UNADJUSTED_ALIGNMENT modulo the PREFERRED_UNIT_STACK_BOUNDARY.  */

  /* Begin by trying to pop all the bytes.  */
  unadjusted_alignment
    = (unadjusted_alignment
       - (pending_stack_adjust % preferred_unit_stack_boundary));
  adjustment = pending_stack_adjust;
  /* Push enough additional bytes that the stack will be aligned
     after the arguments are pushed.  */
  if (preferred_unit_stack_boundary > 1)
    {
      if (unadjusted_alignment > 0)
	adjustment -= preferred_unit_stack_boundary - unadjusted_alignment;
      else
	adjustment += unadjusted_alignment;
    }

  /* Now, sets ARGS_SIZE->CONSTANT so that we pop the right number of
     bytes after the call.  The right number is the entire
     PENDING_STACK_ADJUST less our ADJUSTMENT plus the amount required
     by the arguments in the first place.  */
  args_size->constant
    = pending_stack_adjust - adjustment + unadjusted_args_size;

  return adjustment;
}

/* Scan X expression if it does not dereference any argument slots
   we already clobbered by tail call arguments (as noted in stored_args_map
   bitmap).
   Return nonzero if X expression dereferences such argument slots,
   zero otherwise.  */

static int
check_sibcall_argument_overlap_1 (rtx x)
{
  RTX_CODE code;
  int i, j;
  unsigned int k;
  const char *fmt;

  if (x == NULL_RTX)
    return 0;

  code = GET_CODE (x);

  if (code == MEM)
    {
      if (XEXP (x, 0) == current_function_internal_arg_pointer)
	i = 0;
      else if (GET_CODE (XEXP (x, 0)) == PLUS
	       && XEXP (XEXP (x, 0), 0) ==
		  current_function_internal_arg_pointer
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	i = INTVAL (XEXP (XEXP (x, 0), 1));
      else
	return 0;

#ifdef ARGS_GROW_DOWNWARD
      i = -i - GET_MODE_SIZE (GET_MODE (x));
#endif

      for (k = 0; k < GET_MODE_SIZE (GET_MODE (x)); k++)
	if (i + k < stored_args_map->n_bits
	    && TEST_BIT (stored_args_map, i + k))
	  return 1;

      return 0;
    }

  /* Scan all subexpressions.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	{
	  if (check_sibcall_argument_overlap_1 (XEXP (x, i)))
	    return 1;
	}
      else if (*fmt == 'E')
	{
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (check_sibcall_argument_overlap_1 (XVECEXP (x, i, j)))
	      return 1;
	}
    }
  return 0;
}

/* Scan sequence after INSN if it does not dereference any argument slots
   we already clobbered by tail call arguments (as noted in stored_args_map
   bitmap).  If MARK_STORED_ARGS_MAP, add stack slots for ARG to
   stored_args_map bitmap afterwards (when ARG is a register MARK_STORED_ARGS_MAP
   should be 0).  Return nonzero if sequence after INSN dereferences such argument
   slots, zero otherwise.  */

static int
check_sibcall_argument_overlap (rtx insn, struct arg_data *arg, int mark_stored_args_map)
{
  int low, high;

  if (insn == NULL_RTX)
    insn = get_insns ();
  else
    insn = NEXT_INSN (insn);

  for (; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& check_sibcall_argument_overlap_1 (PATTERN (insn)))
      break;

  if (mark_stored_args_map)
    {
#ifdef ARGS_GROW_DOWNWARD
      low = -arg->locate.slot_offset.constant - arg->locate.size.constant;
#else
      low = arg->locate.slot_offset.constant;
#endif

      for (high = low + arg->locate.size.constant; low < high; low++)
	SET_BIT (stored_args_map, low);
    }
  return insn != NULL_RTX;
}

static tree
fix_unsafe_tree (tree t)
{
  switch (unsafe_for_reeval (t))
    {
    case 0: /* Safe.  */
      break;

    case 1: /* Mildly unsafe.  */
      t = unsave_expr (t);
      break;

    case 2: /* Wildly unsafe.  */
      {
	tree var = build_decl (VAR_DECL, NULL_TREE,
			       TREE_TYPE (t));
	SET_DECL_RTL (var,
		      expand_expr (t, NULL_RTX, VOIDmode, EXPAND_NORMAL));
	t = var;
      }
      break;

    default:
      abort ();
    }
  return t;
}


/* If function value *VALUE was returned at the most significant end of a
   register, shift it towards the least significant end and convert it to
   TYPE's mode.  Return true and update *VALUE if some action was needed.

   TYPE is the type of the function's return value, which is known not
   to have mode BLKmode.  */

static bool
shift_returned_value (tree type, rtx *value)
{
  if (targetm.calls.return_in_msb (type))
    {
      HOST_WIDE_INT shift;

      shift = (GET_MODE_BITSIZE (GET_MODE (*value))
	       - BITS_PER_UNIT * int_size_in_bytes (type));
      if (shift > 0)
	{
	  *value = expand_binop (GET_MODE (*value), lshr_optab, *value,
				 GEN_INT (shift), 0, 1, OPTAB_WIDEN);
	  *value = convert_to_mode (TYPE_MODE (type), *value, 0);
	  return true;
	}
    }
  return false;
}

/* Generate all the code for a function call
   and return an rtx for its value.
   Store the value in TARGET (specified as an rtx) if convenient.
   If the value is stored in TARGET then TARGET is returned.
   If IGNORE is nonzero, then we ignore the value of the function call.  */

rtx
expand_call (tree exp, rtx target, int ignore)
{
  /* Nonzero if we are currently expanding a call.  */
  static int currently_expanding_call = 0;

  /* List of actual parameters.  */
  tree actparms = TREE_OPERAND (exp, 1);
  /* RTX for the function to be called.  */
  rtx funexp;
  /* Sequence of insns to perform a tail recursive "call".  */
  rtx tail_recursion_insns = NULL_RTX;
  /* Sequence of insns to perform a normal "call".  */
  rtx normal_call_insns = NULL_RTX;
  /* Sequence of insns to perform a tail recursive "call".  */
  rtx tail_call_insns = NULL_RTX;
  /* Data type of the function.  */
  tree funtype;
  tree type_arg_types;
  /* Declaration of the function being called,
     or 0 if the function is computed (not known by name).  */
  tree fndecl = 0;
  /* The type of the function being called.  */
  tree fntype;
  rtx insn;
  int try_tail_call = 1;
  int try_tail_recursion = 1;
  int pass;

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
  HOST_WIDE_INT struct_value_size = 0;
  /* Nonzero if called function returns an aggregate in memory PCC style,
     by returning the address of where to find it.  */
  int pcc_struct_value = 0;
  rtx struct_value = 0;

  /* Number of actual parameters in this call, including struct value addr.  */
  int num_actuals;
  /* Number of named args.  Args after this are anonymous ones
     and they must all go on the stack.  */
  int n_named_args;

  /* Vector of information about each argument.
     Arguments are numbered in the order they will be pushed,
     not the order they are written.  */
  struct arg_data *args;

  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  struct args_size adjusted_args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  int unadjusted_args_size;
  /* Data on reg parms scanned so far.  */
  CUMULATIVE_ARGS args_so_far;
  /* Nonzero if a reg parm has been scanned.  */
  int reg_parm_seen;
  /* Nonzero if this is an indirect function call.  */

  /* Nonzero if we must avoid push-insns in the args for this call.
     If stack space is allocated for register parameters, but not by the
     caller, then it is preallocated in the fixed part of the stack frame.
     So the entire argument block must then be preallocated (i.e., we
     ignore PUSH_ROUNDING in that case).  */

  int must_preallocate = !PUSH_ARGS;

  /* Size of the stack reserved for parameter registers.  */
  int reg_parm_stack_space = 0;

  /* Address of space preallocated for stack parms
     (on machines that lack push insns), or 0 if space not preallocated.  */
  rtx argblock = 0;

  /* Mask of ECF_ flags.  */
  int flags = 0;
  /* Nonzero if this is a call to an inline function.  */
  int is_integrable = 0;
#ifdef REG_PARM_STACK_SPACE
  /* Define the boundary of the register parm stack space that needs to be
     saved, if any.  */
  int low_to_save, high_to_save;
  rtx save_area = 0;		/* Place that it is saved */
#endif

  int initial_highest_arg_in_use = highest_outgoing_arg_in_use;
  rtx temp_target = 0;
  char *initial_stack_usage_map = stack_usage_map;

  int old_stack_allocated;

  /* State variables to track stack modifications.  */
  rtx old_stack_level = 0;
  int old_stack_arg_under_construction = 0;
  int old_pending_adj = 0;
  int old_inhibit_defer_pop = inhibit_defer_pop;

  /* Some stack pointer alterations we make are performed via
     allocate_dynamic_stack_space. This modifies the stack_pointer_delta,
     which we then also need to save/restore along the way.  */
  int old_stack_pointer_delta = 0;

  rtx call_fusage;
  tree p = TREE_OPERAND (exp, 0);
  tree addr = TREE_OPERAND (exp, 0);
  int i;
  /* The alignment of the stack, in bits.  */
  HOST_WIDE_INT preferred_stack_boundary;
  /* The alignment of the stack, in bytes.  */
  HOST_WIDE_INT preferred_unit_stack_boundary;

  /* See if this is "nothrow" function call.  */
  if (TREE_NOTHROW (exp))
    flags |= ECF_NOTHROW;

  /* See if we can find a DECL-node for the actual function.
     As a result, decide whether this is a call to an integrable function.  */

  fndecl = get_callee_fndecl (exp);
  if (fndecl)
    {
      fntype = TREE_TYPE (fndecl);
      if (!flag_no_inline
	  && fndecl != current_function_decl
	  && DECL_INLINE (fndecl)
	  && DECL_SAVED_INSNS (fndecl)
	  && DECL_SAVED_INSNS (fndecl)->inlinable)
	is_integrable = 1;
      else if (! TREE_ADDRESSABLE (fndecl))
	{
	  /* In case this function later becomes inlinable,
	     record that there was already a non-inline call to it.

	     Use abstraction instead of setting TREE_ADDRESSABLE
	     directly.  */
	  if (DECL_INLINE (fndecl) && warn_inline && !flag_no_inline
	      && optimize > 0)
	    {
	      warning ("%Jcan't inline call to '%F'", fndecl, fndecl);
	      warning ("called from here");
	    }
	  (*lang_hooks.mark_addressable) (fndecl);
	}

      if (ignore
	  && lookup_attribute ("warn_unused_result",
			       TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	warning ("ignoring return value of `%D', "
		 "declared with attribute warn_unused_result", fndecl);

      flags |= flags_from_decl_or_type (fndecl);
    }

  /* If we don't have specific function to call, see if we have a
     attributes set in the type.  */
  else
    {
      fntype = TREE_TYPE (TREE_TYPE (p));
      if (ignore
	  && lookup_attribute ("warn_unused_result", TYPE_ATTRIBUTES (fntype)))
	warning ("ignoring return value of function "
		 "declared with attribute warn_unused_result");
      flags |= flags_from_decl_or_type (fntype);
    }

  struct_value = targetm.calls.struct_value_rtx (fntype, 0);

  /* Warn if this value is an aggregate type,
     regardless of which calling convention we are using for it.  */
  if (warn_aggregate_return && AGGREGATE_TYPE_P (TREE_TYPE (exp)))
    warning ("function call has aggregate value");

  /* If the result of a pure or const function call is ignored (or void),
     and none of its arguments are volatile, we can avoid expanding the
     call and just evaluate the arguments for side-effects.  */
  if ((flags & (ECF_CONST | ECF_PURE))
      && (ignore || target == const0_rtx
	  || TYPE_MODE (TREE_TYPE (exp)) == VOIDmode))
    {
      bool volatilep = false;
      tree arg;

      for (arg = actparms; arg; arg = TREE_CHAIN (arg))
	if (TREE_THIS_VOLATILE (TREE_VALUE (arg)))
	  {
	    volatilep = true;
	    break;
	  }

      if (! volatilep)
	{
	  for (arg = actparms; arg; arg = TREE_CHAIN (arg))
	    expand_expr (TREE_VALUE (arg), const0_rtx,
			 VOIDmode, EXPAND_NORMAL);
	  return const0_rtx;
	}
    }

#ifdef REG_PARM_STACK_SPACE
#ifdef MAYBE_REG_PARM_STACK_SPACE
  reg_parm_stack_space = MAYBE_REG_PARM_STACK_SPACE;
#else
  reg_parm_stack_space = REG_PARM_STACK_SPACE (fndecl);
#endif
#endif

#ifndef OUTGOING_REG_PARM_STACK_SPACE
  if (reg_parm_stack_space > 0 && PUSH_ARGS)
    must_preallocate = 1;
#endif

  /* Set up a place to return a structure.  */

  /* Cater to broken compilers.  */
  if (aggregate_value_p (exp, fndecl))
    {
      /* This call returns a big structure.  */
      flags &= ~(ECF_CONST | ECF_PURE | ECF_LIBCALL_BLOCK);

#ifdef PCC_STATIC_STRUCT_RETURN
      {
	pcc_struct_value = 1;
	/* Easier than making that case work right.  */
	if (is_integrable)
	  {
	    /* In case this is a static function, note that it has been
	       used.  */
	    if (! TREE_ADDRESSABLE (fndecl))
	      (*lang_hooks.mark_addressable) (fndecl);
	    is_integrable = 0;
	  }
      }
#else /* not PCC_STATIC_STRUCT_RETURN */
      {
	struct_value_size = int_size_in_bytes (TREE_TYPE (exp));

	if (CALL_EXPR_HAS_RETURN_SLOT_ADDR (exp))
	  {
	    /* The structure value address arg is already in actparms.
	       Pull it out.  It might be nice to just leave it there, but
	       we need to set structure_value_addr.  */
	    tree return_arg = TREE_VALUE (actparms);
	    actparms = TREE_CHAIN (actparms);
	    structure_value_addr = expand_expr (return_arg, NULL_RTX,
						VOIDmode, EXPAND_NORMAL);
	  }
	else if (target && GET_CODE (target) == MEM)
	  structure_value_addr = XEXP (target, 0);
	else
	  {
	    /* For variable-sized objects, we must be called with a target
	       specified.  If we were to allocate space on the stack here,
	       we would have no way of knowing when to free it.  */
	    rtx d = assign_temp (TREE_TYPE (exp), 1, 1, 1);

	    mark_temp_addr_taken (d);
	    structure_value_addr = XEXP (d, 0);
	    target = 0;
	  }
      }
#endif /* not PCC_STATIC_STRUCT_RETURN */
    }

  /* If called function is inline, try to integrate it.  */

  if (is_integrable)
    {
      rtx temp = try_to_integrate (fndecl, actparms, target,
				   ignore, TREE_TYPE (exp),
				   structure_value_addr);
      if (temp != (rtx) (size_t) - 1)
	return temp;
    }

  /* Figure out the amount to which the stack should be aligned.  */
  preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;
  if (fndecl)
    {
      struct cgraph_rtl_info *i = cgraph_rtl_info (fndecl);
      if (i && i->preferred_incoming_stack_boundary)
	preferred_stack_boundary = i->preferred_incoming_stack_boundary;
    }

  /* Operand 0 is a pointer-to-function; get the type of the function.  */
  funtype = TREE_TYPE (addr);
  if (! POINTER_TYPE_P (funtype))
    abort ();
  funtype = TREE_TYPE (funtype);

  /* Munge the tree to split complex arguments into their imaginary
     and real parts.  */
  if (targetm.calls.split_complex_arg)
    {
      type_arg_types = split_complex_types (TYPE_ARG_TYPES (funtype));
      actparms = split_complex_values (actparms);
    }
  else
    type_arg_types = TYPE_ARG_TYPES (funtype);

  /* See if this is a call to a function that can return more than once
     or a call to longjmp or malloc.  */
  flags |= special_function_p (fndecl, flags);

  if (flags & ECF_MAY_BE_ALLOCA)
    current_function_calls_alloca = 1;

  /* If struct_value_rtx is 0, it means pass the address
     as if it were an extra parameter.  */
  if (structure_value_addr && struct_value == 0)
    {
      /* If structure_value_addr is a REG other than
	 virtual_outgoing_args_rtx, we can use always use it.  If it
	 is not a REG, we must always copy it into a register.
	 If it is virtual_outgoing_args_rtx, we must copy it to another
	 register in some cases.  */
      rtx temp = (GET_CODE (structure_value_addr) != REG
		  || (ACCUMULATE_OUTGOING_ARGS
		      && stack_arg_under_construction
		      && structure_value_addr == virtual_outgoing_args_rtx)
		  ? copy_addr_to_reg (convert_memory_address 
				      (Pmode, structure_value_addr))
		  : structure_value_addr);

      actparms
	= tree_cons (error_mark_node,
		     make_tree (build_pointer_type (TREE_TYPE (funtype)),
				temp),
		     actparms);
      structure_value_addr_parm = 1;
    }

  /* Count the arguments and set NUM_ACTUALS.  */
  for (p = actparms, num_actuals = 0; p; p = TREE_CHAIN (p))
    num_actuals++;

  /* Compute number of named args.
     First, do a raw count of the args for INIT_CUMULATIVE_ARGS.  */

  if (type_arg_types != 0)
    n_named_args
      = (list_length (type_arg_types)
	 /* Count the struct value address, if it is passed as a parm.  */
	 + structure_value_addr_parm);
  else
    /* If we know nothing, treat all args as named.  */
    n_named_args = num_actuals;

  /* Start updating where the next arg would go.

     On some machines (such as the PA) indirect calls have a different
     calling convention than normal calls.  The fourth argument in
     INIT_CUMULATIVE_ARGS tells the backend if this is an indirect call
     or not.  */
  INIT_CUMULATIVE_ARGS (args_so_far, funtype, NULL_RTX, fndecl, n_named_args);

  /* Now possibly adjust the number of named args.
     Normally, don't include the last named arg if anonymous args follow.
     We do include the last named arg if
     targetm.calls.strict_argument_naming() returns nonzero.
     (If no anonymous args follow, the result of list_length is actually
     one too large.  This is harmless.)

     If targetm.calls.pretend_outgoing_varargs_named() returns
     nonzero, and targetm.calls.strict_argument_naming() returns zero,
     this machine will be able to place unnamed args that were passed
     in registers into the stack.  So treat all args as named.  This
     allows the insns emitting for a specific argument list to be
     independent of the function declaration.

     If targetm.calls.pretend_outgoing_varargs_named() returns zero,
     we do not have any reliable way to pass unnamed args in
     registers, so we must force them into memory.  */

  if (type_arg_types != 0
      && targetm.calls.strict_argument_naming (&args_so_far))
    ;
  else if (type_arg_types != 0
	   && ! targetm.calls.pretend_outgoing_varargs_named (&args_so_far))
    /* Don't include the last named arg.  */
    --n_named_args;
  else
    /* Treat all args as named.  */
    n_named_args = num_actuals;

  /* Make a vector to hold all the information about each arg.  */
  args = alloca (num_actuals * sizeof (struct arg_data));
  memset (args, 0, num_actuals * sizeof (struct arg_data));

  /* Build up entries in the ARGS array, compute the size of the
     arguments into ARGS_SIZE, etc.  */
  initialize_argument_information (num_actuals, args, &args_size,
				   n_named_args, actparms, fndecl,
				   &args_so_far, reg_parm_stack_space,
				   &old_stack_level, &old_pending_adj,
				   &must_preallocate, &flags,
				   CALL_FROM_THUNK_P (exp));

  if (args_size.var)
    {
      /* If this function requires a variable-sized argument list, don't
	 try to make a cse'able block for this call.  We may be able to
	 do this eventually, but it is too complicated to keep track of
	 what insns go in the cse'able block and which don't.  */

      flags &= ~ECF_LIBCALL_BLOCK;
      must_preallocate = 1;
    }

  /* Now make final decision about preallocating stack space.  */
  must_preallocate = finalize_must_preallocate (must_preallocate,
						num_actuals, args,
						&args_size);

  /* If the structure value address will reference the stack pointer, we
     must stabilize it.  We don't need to do this if we know that we are
     not going to adjust the stack pointer in processing this call.  */

  if (structure_value_addr
      && (reg_mentioned_p (virtual_stack_dynamic_rtx, structure_value_addr)
	  || reg_mentioned_p (virtual_outgoing_args_rtx,
			      structure_value_addr))
      && (args_size.var
	  || (!ACCUMULATE_OUTGOING_ARGS && args_size.constant)))
    structure_value_addr = copy_to_reg (structure_value_addr);

  /* Tail calls can make things harder to debug, and we're traditionally
     pushed these optimizations into -O2.  Don't try if we're already
     expanding a call, as that means we're an argument.  Don't try if
     there's cleanups, as we know there's code to follow the call.

     If rtx_equal_function_value_matters is false, that means we've
     finished with regular parsing.  Which means that some of the
     machinery we use to generate tail-calls is no longer in place.
     This is most often true of sjlj-exceptions, which we couldn't
     tail-call to anyway.

     If current_nesting_level () == 0, we're being called after
     the function body has been expanded.  This can happen when
     setting up trampolines in expand_function_end.  */
  if (currently_expanding_call++ != 0
      || !flag_optimize_sibling_calls
      || !rtx_equal_function_value_matters
      || current_nesting_level () == 0
      || any_pending_cleanups ()
      || args_size.var)
    try_tail_call = try_tail_recursion = 0;

  /* Tail recursion fails, when we are not dealing with recursive calls.  */
  if (!try_tail_recursion
      || TREE_CODE (addr) != ADDR_EXPR
      || TREE_OPERAND (addr, 0) != current_function_decl)
    try_tail_recursion = 0;

  /*  Rest of purposes for tail call optimizations to fail.  */
  if (
#ifdef HAVE_sibcall_epilogue
      !HAVE_sibcall_epilogue
#else
      1
#endif
      || !try_tail_call
      /* Doing sibling call optimization needs some work, since
	 structure_value_addr can be allocated on the stack.
	 It does not seem worth the effort since few optimizable
	 sibling calls will return a structure.  */
      || structure_value_addr != NULL_RTX
      /* Check whether the target is able to optimize the call
	 into a sibcall.  */
      || !(*targetm.function_ok_for_sibcall) (fndecl, exp)
      /* Functions that do not return exactly once may not be sibcall
         optimized.  */
      || (flags & (ECF_RETURNS_TWICE | ECF_LONGJMP | ECF_NORETURN))
      || TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (addr)))
      /* If the called function is nested in the current one, it might access
         some of the caller's arguments, but could clobber them beforehand if
         the argument areas are shared.  */
      || (fndecl && decl_function_context (fndecl) == current_function_decl)
      /* If this function requires more stack slots than the current
	 function, we cannot change it into a sibling call.  */
      || args_size.constant > current_function_args_size
      /* If the callee pops its own arguments, then it must pop exactly
	 the same number of arguments as the current function.  */
      || (RETURN_POPS_ARGS (fndecl, funtype, args_size.constant)
	  != RETURN_POPS_ARGS (current_function_decl,
			       TREE_TYPE (current_function_decl),
			       current_function_args_size))
      || !(*lang_hooks.decls.ok_for_sibcall) (fndecl))
    try_tail_call = 0;

  if (try_tail_call || try_tail_recursion)
    {
      int end, inc;
      actparms = NULL_TREE;
      /* Ok, we're going to give the tail call the old college try.
	 This means we're going to evaluate the function arguments
	 up to three times.  There are two degrees of badness we can
	 encounter, those that can be unsaved and those that can't.
	 (See unsafe_for_reeval commentary for details.)

	 Generate a new argument list.  Pass safe arguments through
	 unchanged.  For the easy badness wrap them in UNSAVE_EXPRs.
	 For hard badness, evaluate them now and put their resulting
	 rtx in a temporary VAR_DECL.

	 initialize_argument_information has ordered the array for the
	 order to be pushed, and we must remember this when reconstructing
	 the original argument order.  */

      if (PUSH_ARGS_REVERSED)
	{
	  inc = 1;
	  i = 0;
	  end = num_actuals;
	}
      else
	{
	  inc = -1;
	  i = num_actuals - 1;
	  end = -1;
	}

      for (; i != end; i += inc)
	{
          args[i].tree_value = fix_unsafe_tree (args[i].tree_value);
	  /* We need to build actparms for optimize_tail_recursion.  We can
	     safely trash away TREE_PURPOSE, since it is unused by this
	     function.  */
	  if (try_tail_recursion)
	    actparms = tree_cons (NULL_TREE, args[i].tree_value, actparms);
	}
      /* Do the same for the function address if it is an expression.  */
      if (!fndecl)
        addr = fix_unsafe_tree (addr);
      /* Expanding one of those dangerous arguments could have added
	 cleanups, but otherwise give it a whirl.  */
      if (any_pending_cleanups ())
	try_tail_call = try_tail_recursion = 0;
    }

  /* Generate a tail recursion sequence when calling ourselves.  */

  if (try_tail_recursion)
    {
      /* We want to emit any pending stack adjustments before the tail
	 recursion "call".  That way we know any adjustment after the tail
	 recursion call can be ignored if we indeed use the tail recursion
	 call expansion.  */
      int save_pending_stack_adjust = pending_stack_adjust;
      int save_stack_pointer_delta = stack_pointer_delta;

      /* Emit any queued insns now; otherwise they would end up in
	 only one of the alternates.  */
      emit_queue ();

      /* Use a new sequence to hold any RTL we generate.  We do not even
	 know if we will use this RTL yet.  The final decision can not be
	 made until after RTL generation for the entire function is
	 complete.  */
      start_sequence ();
      /* If expanding any of the arguments creates cleanups, we can't
	 do a tailcall.  So, we'll need to pop the pending cleanups
	 list.  If, however, all goes well, and there are no cleanups
	 then the call to expand_start_target_temps will have no
	 effect.  */
      expand_start_target_temps ();
      if (optimize_tail_recursion (actparms, get_last_insn ()))
	{
	  if (any_pending_cleanups ())
	    try_tail_call = try_tail_recursion = 0;
	  else
	    tail_recursion_insns = get_insns ();
	}
      expand_end_target_temps ();
      end_sequence ();

      /* Restore the original pending stack adjustment for the sibling and
	 normal call cases below.  */
      pending_stack_adjust = save_pending_stack_adjust;
      stack_pointer_delta = save_stack_pointer_delta;
    }

  if (profile_arc_flag && (flags & ECF_FORK_OR_EXEC))
    {
      /* A fork duplicates the profile information, and an exec discards
	 it.  We can't rely on fork/exec to be paired.  So write out the
	 profile information we have gathered so far, and clear it.  */
      /* ??? When Linux's __clone is called with CLONE_VM set, profiling
	 is subject to race conditions, just as with multithreaded
	 programs.  */

      emit_library_call (gcov_flush_libfunc, LCT_ALWAYS_RETURN, VOIDmode, 0);
    }

  /* Ensure current function's preferred stack boundary is at least
     what we need.  We don't have to increase alignment for recursive
     functions.  */
  if (cfun->preferred_stack_boundary < preferred_stack_boundary
      && fndecl != current_function_decl)
    cfun->preferred_stack_boundary = preferred_stack_boundary;
  if (fndecl == current_function_decl)
    cfun->recursive_call_emit = true;

  preferred_unit_stack_boundary = preferred_stack_boundary / BITS_PER_UNIT;

  function_call_count++;

  /* We want to make two insn chains; one for a sibling call, the other
     for a normal call.  We will select one of the two chains after
     initial RTL generation is complete.  */
  for (pass = try_tail_call ? 0 : 1; pass < 2; pass++)
    {
      int sibcall_failure = 0;
      /* We want to emit any pending stack adjustments before the tail
	 recursion "call".  That way we know any adjustment after the tail
	 recursion call can be ignored if we indeed use the tail recursion
	 call expansion.  */
      int save_pending_stack_adjust = 0;
      int save_stack_pointer_delta = 0;
      rtx insns;
      rtx before_call, next_arg_reg;

      if (pass == 0)
	{
	  /* Emit any queued insns now; otherwise they would end up in
             only one of the alternates.  */
	  emit_queue ();

	  /* State variables we need to save and restore between
	     iterations.  */
	  save_pending_stack_adjust = pending_stack_adjust;
	  save_stack_pointer_delta = stack_pointer_delta;
	}
      if (pass)
	flags &= ~ECF_SIBCALL;
      else
	flags |= ECF_SIBCALL;

      /* Other state variables that we must reinitialize each time
	 through the loop (that are not initialized by the loop itself).  */
      argblock = 0;
      call_fusage = 0;

      /* Start a new sequence for the normal call case.

	 From this point on, if the sibling call fails, we want to set
	 sibcall_failure instead of continuing the loop.  */
      start_sequence ();

      if (pass == 0)
	{
	  /* We know at this point that there are not currently any
	     pending cleanups.  If, however, in the process of evaluating
	     the arguments we were to create some, we'll need to be
	     able to get rid of them.  */
	  expand_start_target_temps ();
	}

      /* Don't let pending stack adjusts add up to too much.
	 Also, do all pending adjustments now if there is any chance
	 this might be a call to alloca or if we are expanding a sibling
	 call sequence or if we are calling a function that is to return
	 with stack pointer depressed.  */
      if (pending_stack_adjust >= 32
	  || (pending_stack_adjust > 0
	      && (flags & (ECF_MAY_BE_ALLOCA | ECF_SP_DEPRESSED)))
	  || pass == 0)
	do_pending_stack_adjust ();

      /* When calling a const function, we must pop the stack args right away,
	 so that the pop is deleted or moved with the call.  */
      if (pass && (flags & ECF_LIBCALL_BLOCK))
	NO_DEFER_POP;

#ifdef FINAL_REG_PARM_STACK_SPACE
      reg_parm_stack_space = FINAL_REG_PARM_STACK_SPACE (args_size.constant,
							 args_size.var);
#endif
      /* Precompute any arguments as needed.  */
      if (pass)
	precompute_arguments (flags, num_actuals, args);

      /* Now we are about to start emitting insns that can be deleted
	 if a libcall is deleted.  */
      if (pass && (flags & (ECF_LIBCALL_BLOCK | ECF_MALLOC)))
	start_sequence ();

      adjusted_args_size = args_size;
      /* Compute the actual size of the argument block required.  The variable
	 and constant sizes must be combined, the size may have to be rounded,
	 and there may be a minimum required size.  When generating a sibcall
	 pattern, do not round up, since we'll be re-using whatever space our
	 caller provided.  */
      unadjusted_args_size
	= compute_argument_block_size (reg_parm_stack_space,
				       &adjusted_args_size,
				       (pass == 0 ? 0
					: preferred_stack_boundary));

      old_stack_allocated = stack_pointer_delta - pending_stack_adjust;

      /* The argument block when performing a sibling call is the
         incoming argument block.  */
      if (pass == 0)
	{
	  argblock = virtual_incoming_args_rtx;
	  argblock
#ifdef STACK_GROWS_DOWNWARD
	    = plus_constant (argblock, current_function_pretend_args_size);
#else
	    = plus_constant (argblock, -current_function_pretend_args_size);
#endif
	  stored_args_map = sbitmap_alloc (args_size.constant);
	  sbitmap_zero (stored_args_map);
	}

      /* If we have no actual push instructions, or shouldn't use them,
	 make space for all args right now.  */
      else if (adjusted_args_size.var != 0)
	{
	  if (old_stack_level == 0)
	    {
	      emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);
	      old_stack_pointer_delta = stack_pointer_delta;
	      old_pending_adj = pending_stack_adjust;
	      pending_stack_adjust = 0;
	      /* stack_arg_under_construction says whether a stack arg is
		 being constructed at the old stack level.  Pushing the stack
		 gets a clean outgoing argument block.  */
	      old_stack_arg_under_construction = stack_arg_under_construction;
	      stack_arg_under_construction = 0;
	    }
	  argblock = push_block (ARGS_SIZE_RTX (adjusted_args_size), 0, 0);
	}
      else
	{
	  /* Note that we must go through the motions of allocating an argument
	     block even if the size is zero because we may be storing args
	     in the area reserved for register arguments, which may be part of
	     the stack frame.  */

	  int needed = adjusted_args_size.constant;

	  /* Store the maximum argument space used.  It will be pushed by
	     the prologue (if ACCUMULATE_OUTGOING_ARGS, or stack overflow
	     checking).  */

	  if (needed > current_function_outgoing_args_size)
	    current_function_outgoing_args_size = needed;

	  if (must_preallocate)
	    {
	      if (ACCUMULATE_OUTGOING_ARGS)
		{
		  /* Since the stack pointer will never be pushed, it is
		     possible for the evaluation of a parm to clobber
		     something we have already written to the stack.
		     Since most function calls on RISC machines do not use
		     the stack, this is uncommon, but must work correctly.

		     Therefore, we save any area of the stack that was already
		     written and that we are using.  Here we set up to do this
		     by making a new stack usage map from the old one.  The
		     actual save will be done by store_one_arg.

		     Another approach might be to try to reorder the argument
		     evaluations to avoid this conflicting stack usage.  */

#ifndef OUTGOING_REG_PARM_STACK_SPACE
		  /* Since we will be writing into the entire argument area,
		     the map must be allocated for its entire size, not just
		     the part that is the responsibility of the caller.  */
		  needed += reg_parm_stack_space;
#endif

#ifdef ARGS_GROW_DOWNWARD
		  highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use,
						     needed + 1);
#else
		  highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use,
						     needed);
#endif
		  stack_usage_map = alloca (highest_outgoing_arg_in_use);

		  if (initial_highest_arg_in_use)
		    memcpy (stack_usage_map, initial_stack_usage_map,
			    initial_highest_arg_in_use);

		  if (initial_highest_arg_in_use != highest_outgoing_arg_in_use)
		    memset (&stack_usage_map[initial_highest_arg_in_use], 0,
			   (highest_outgoing_arg_in_use
			    - initial_highest_arg_in_use));
		  needed = 0;

		  /* The address of the outgoing argument list must not be
		     copied to a register here, because argblock would be left
		     pointing to the wrong place after the call to
		     allocate_dynamic_stack_space below.  */

		  argblock = virtual_outgoing_args_rtx;
		}
	      else
		{
		  if (inhibit_defer_pop == 0)
		    {
		      /* Try to reuse some or all of the pending_stack_adjust
			 to get this space.  */
		      needed
			= (combine_pending_stack_adjustment_and_call
			   (unadjusted_args_size,
			    &adjusted_args_size,
			    preferred_unit_stack_boundary));

		      /* combine_pending_stack_adjustment_and_call computes
			 an adjustment before the arguments are allocated.
			 Account for them and see whether or not the stack
			 needs to go up or down.  */
		      needed = unadjusted_args_size - needed;

		      if (needed < 0)
			{
			  /* We're releasing stack space.  */
			  /* ??? We can avoid any adjustment at all if we're
			     already aligned.  FIXME.  */
			  pending_stack_adjust = -needed;
			  do_pending_stack_adjust ();
			  needed = 0;
			}
		      else
			/* We need to allocate space.  We'll do that in
			   push_block below.  */
			pending_stack_adjust = 0;
		    }

		  /* Special case this because overhead of `push_block' in
		     this case is non-trivial.  */
		  if (needed == 0)
		    argblock = virtual_outgoing_args_rtx;
		  else
		    {
		      argblock = push_block (GEN_INT (needed), 0, 0);
#ifdef ARGS_GROW_DOWNWARD
		      argblock = plus_constant (argblock, needed);
#endif
		    }

		  /* We only really need to call `copy_to_reg' in the case
		     where push insns are going to be used to pass ARGBLOCK
		     to a function call in ARGS.  In that case, the stack
		     pointer changes value from the allocation point to the
		     call point, and hence the value of
		     VIRTUAL_OUTGOING_ARGS_RTX changes as well.  But might
		     as well always do it.  */
		  argblock = copy_to_reg (argblock);
		}
	    }
	}

      if (ACCUMULATE_OUTGOING_ARGS)
	{
	  /* The save/restore code in store_one_arg handles all
	     cases except one: a constructor call (including a C
	     function returning a BLKmode struct) to initialize
	     an argument.  */
	  if (stack_arg_under_construction)
	    {
#ifndef OUTGOING_REG_PARM_STACK_SPACE
	      rtx push_size = GEN_INT (reg_parm_stack_space
				       + adjusted_args_size.constant);
#else
	      rtx push_size = GEN_INT (adjusted_args_size.constant);
#endif
	      if (old_stack_level == 0)
		{
		  emit_stack_save (SAVE_BLOCK, &old_stack_level,
				   NULL_RTX);
		  old_stack_pointer_delta = stack_pointer_delta;
		  old_pending_adj = pending_stack_adjust;
		  pending_stack_adjust = 0;
		  /* stack_arg_under_construction says whether a stack
		     arg is being constructed at the old stack level.
		     Pushing the stack gets a clean outgoing argument
		     block.  */
		  old_stack_arg_under_construction
		    = stack_arg_under_construction;
		  stack_arg_under_construction = 0;
		  /* Make a new map for the new argument list.  */
		  stack_usage_map = alloca (highest_outgoing_arg_in_use);
		  memset (stack_usage_map, 0, highest_outgoing_arg_in_use);
		  highest_outgoing_arg_in_use = 0;
		}
	      allocate_dynamic_stack_space (push_size, NULL_RTX,
					    BITS_PER_UNIT);
	    }

	  /* If argument evaluation might modify the stack pointer,
	     copy the address of the argument list to a register.  */
	  for (i = 0; i < num_actuals; i++)
	    if (args[i].pass_on_stack)
	      {
		argblock = copy_addr_to_reg (argblock);
		break;
	      }
	}

      compute_argument_addresses (args, argblock, num_actuals);

      /* If we push args individually in reverse order, perform stack alignment
	 before the first push (the last arg).  */
      if (PUSH_ARGS_REVERSED && argblock == 0
	  && adjusted_args_size.constant != unadjusted_args_size)
	{
	  /* When the stack adjustment is pending, we get better code
	     by combining the adjustments.  */
	  if (pending_stack_adjust
	      && ! (flags & ECF_LIBCALL_BLOCK)
	      && ! inhibit_defer_pop)
	    {
	      pending_stack_adjust
		= (combine_pending_stack_adjustment_and_call
		   (unadjusted_args_size,
		    &adjusted_args_size,
		    preferred_unit_stack_boundary));
	      do_pending_stack_adjust ();
	    }
	  else if (argblock == 0)
	    anti_adjust_stack (GEN_INT (adjusted_args_size.constant
					- unadjusted_args_size));
	}
      /* Now that the stack is properly aligned, pops can't safely
	 be deferred during the evaluation of the arguments.  */
      NO_DEFER_POP;

      funexp = rtx_for_function_call (fndecl, addr);

      /* Figure out the register where the value, if any, will come back.  */
      valreg = 0;
      if (TYPE_MODE (TREE_TYPE (exp)) != VOIDmode
	  && ! structure_value_addr)
	{
	  if (pcc_struct_value)
	    valreg = hard_function_value (build_pointer_type (TREE_TYPE (exp)),
					  fndecl, (pass == 0));
	  else
	    valreg = hard_function_value (TREE_TYPE (exp), fndecl, (pass == 0));
	}

      /* Precompute all register parameters.  It isn't safe to compute anything
	 once we have started filling any specific hard regs.  */
      precompute_register_parameters (num_actuals, args, &reg_parm_seen);

#ifdef REG_PARM_STACK_SPACE
      /* Save the fixed argument area if it's part of the caller's frame and
	 is clobbered by argument setup for this call.  */
      if (ACCUMULATE_OUTGOING_ARGS && pass)
	save_area = save_fixed_argument_area (reg_parm_stack_space, argblock,
					      &low_to_save, &high_to_save);
#endif

      /* Now store (and compute if necessary) all non-register parms.
	 These come before register parms, since they can require block-moves,
	 which could clobber the registers used for register parms.
	 Parms which have partial registers are not stored here,
	 but we do preallocate space here if they want that.  */

      for (i = 0; i < num_actuals; i++)
	if (args[i].reg == 0 || args[i].pass_on_stack)
	  {
	    rtx before_arg = get_last_insn ();

	    if (store_one_arg (&args[i], argblock, flags,
			       adjusted_args_size.var != 0,
			       reg_parm_stack_space)
		|| (pass == 0
		    && check_sibcall_argument_overlap (before_arg,
						       &args[i], 1)))
	      sibcall_failure = 1;

	    if (flags & ECF_CONST
		&& args[i].stack
		&& args[i].value == args[i].stack)
	      call_fusage = gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_USE (VOIDmode,
							    args[i].value),
					       call_fusage);
	  }

      /* If we have a parm that is passed in registers but not in memory
	 and whose alignment does not permit a direct copy into registers,
	 make a group of pseudos that correspond to each register that we
	 will later fill.  */
      if (STRICT_ALIGNMENT)
	store_unaligned_arguments_into_pseudos (args, num_actuals);

      /* Now store any partially-in-registers parm.
	 This is the last place a block-move can happen.  */
      if (reg_parm_seen)
	for (i = 0; i < num_actuals; i++)
	  if (args[i].partial != 0 && ! args[i].pass_on_stack)
	    {
	      rtx before_arg = get_last_insn ();

	      if (store_one_arg (&args[i], argblock, flags,
				 adjusted_args_size.var != 0,
				 reg_parm_stack_space)
		  || (pass == 0
		      && check_sibcall_argument_overlap (before_arg,
							 &args[i], 1)))
		sibcall_failure = 1;
	    }

      /* If we pushed args in forward order, perform stack alignment
	 after pushing the last arg.  */
      if (!PUSH_ARGS_REVERSED && argblock == 0)
	anti_adjust_stack (GEN_INT (adjusted_args_size.constant
				    - unadjusted_args_size));

      /* If register arguments require space on the stack and stack space
	 was not preallocated, allocate stack space here for arguments
	 passed in registers.  */
#ifdef OUTGOING_REG_PARM_STACK_SPACE
      if (!ACCUMULATE_OUTGOING_ARGS
	  && must_preallocate == 0 && reg_parm_stack_space > 0)
	anti_adjust_stack (GEN_INT (reg_parm_stack_space));
#endif

      /* Pass the function the address in which to return a
	 structure value.  */
      if (pass != 0 && structure_value_addr && ! structure_value_addr_parm)
	{
	  structure_value_addr 
	    = convert_memory_address (Pmode, structure_value_addr);
	  emit_move_insn (struct_value,
			  force_reg (Pmode,
				     force_operand (structure_value_addr,
						    NULL_RTX)));

	  if (GET_CODE (struct_value) == REG)
	    use_reg (&call_fusage, struct_value);
	}

      funexp = prepare_call_address (funexp, fndecl, &call_fusage,
				     reg_parm_seen, pass == 0);

      load_register_parameters (args, num_actuals, &call_fusage, flags,
				pass == 0, &sibcall_failure);

      /* Perform postincrements before actually calling the function.  */
      emit_queue ();

      /* Save a pointer to the last insn before the call, so that we can
	 later safely search backwards to find the CALL_INSN.  */
      before_call = get_last_insn ();

      /* Set up next argument register.  For sibling calls on machines
	 with register windows this should be the incoming register.  */
#ifdef FUNCTION_INCOMING_ARG
      if (pass == 0)
	next_arg_reg = FUNCTION_INCOMING_ARG (args_so_far, VOIDmode,
					      void_type_node, 1);
      else
#endif
	next_arg_reg = FUNCTION_ARG (args_so_far, VOIDmode,
				     void_type_node, 1);

      /* All arguments and registers used for the call must be set up by
	 now!  */

      /* Stack must be properly aligned now.  */
      if (pass && stack_pointer_delta % preferred_unit_stack_boundary)
	abort ();

      /* Generate the actual call instruction.  */
      emit_call_1 (funexp, fndecl, funtype, unadjusted_args_size,
		   adjusted_args_size.constant, struct_value_size,
		   next_arg_reg, valreg, old_inhibit_defer_pop, call_fusage,
		   flags, & args_so_far);

      /* If call is cse'able, make appropriate pair of reg-notes around it.
	 Test valreg so we don't crash; may safely ignore `const'
	 if return type is void.  Disable for PARALLEL return values, because
	 we have no way to move such values into a pseudo register.  */
      if (pass && (flags & ECF_LIBCALL_BLOCK))
	{
	  rtx insns;
	  rtx insn;
	  bool failed = valreg == 0 || GET_CODE (valreg) == PARALLEL;

          insns = get_insns ();

	  /* Expansion of block moves possibly introduced a loop that may
	     not appear inside libcall block.  */
	  for (insn = insns; insn; insn = NEXT_INSN (insn))
	    if (GET_CODE (insn) == JUMP_INSN)
	      failed = true;

	  if (failed)
	    {
	      end_sequence ();
	      emit_insn (insns);
	    }
	  else
	    {
	      rtx note = 0;
	      rtx temp = gen_reg_rtx (GET_MODE (valreg));

	      /* Mark the return value as a pointer if needed.  */
	      if (TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE)
		mark_reg_pointer (temp,
				  TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp))));

	      end_sequence ();
	      if (flag_unsafe_math_optimizations
		  && fndecl
		  && DECL_BUILT_IN (fndecl)
		  && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_SQRT
		      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_SQRTF
		      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_SQRTL))
		note = gen_rtx_fmt_e (SQRT, 
				      GET_MODE (temp), 
				      args[0].initial_value);
	      else
		{
		  /* Construct an "equal form" for the value which
		     mentions all the arguments in order as well as
		     the function name.  */
		  for (i = 0; i < num_actuals; i++)
		    note = gen_rtx_EXPR_LIST (VOIDmode,
					      args[i].initial_value, note);
		  note = gen_rtx_EXPR_LIST (VOIDmode, funexp, note);
		  
		  if (flags & ECF_PURE)
		    note = gen_rtx_EXPR_LIST (VOIDmode,
			gen_rtx_USE (VOIDmode,
				     gen_rtx_MEM (BLKmode,
						  gen_rtx_SCRATCH (VOIDmode))),
			note);
		}
	      emit_libcall_block (insns, temp, valreg, note);

	      valreg = temp;
	    }
	}
      else if (pass && (flags & ECF_MALLOC))
	{
	  rtx temp = gen_reg_rtx (GET_MODE (valreg));
	  rtx last, insns;

	  /* The return value from a malloc-like function is a pointer.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE)
	    mark_reg_pointer (temp, BIGGEST_ALIGNMENT);

	  emit_move_insn (temp, valreg);

	  /* The return value from a malloc-like function can not alias
	     anything else.  */
	  last = get_last_insn ();
	  REG_NOTES (last) =
	    gen_rtx_EXPR_LIST (REG_NOALIAS, temp, REG_NOTES (last));

	  /* Write out the sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	  valreg = temp;
	}

      /* For calls to `setjmp', etc., inform flow.c it should complain
	 if nonvolatile values are live.  For functions that cannot return,
	 inform flow that control does not fall through.  */

      if ((flags & (ECF_NORETURN | ECF_LONGJMP)) || pass == 0)
	{
	  /* The barrier must be emitted
	     immediately after the CALL_INSN.  Some ports emit more
	     than just a CALL_INSN above, so we must search for it here.  */

	  rtx last = get_last_insn ();
	  while (GET_CODE (last) != CALL_INSN)
	    {
	      last = PREV_INSN (last);
	      /* There was no CALL_INSN?  */
	      if (last == before_call)
		abort ();
	    }

	  emit_barrier_after (last);

	  /* Stack adjustments after a noreturn call are dead code.
	     However when NO_DEFER_POP is in effect, we must preserve
	     stack_pointer_delta.  */
	  if (inhibit_defer_pop == 0)
	    {
	      stack_pointer_delta = old_stack_allocated;
	      pending_stack_adjust = 0;
	    }
	}

      if (flags & ECF_LONGJMP)
	current_function_calls_longjmp = 1;

      /* If value type not void, return an rtx for the value.  */

      /* If there are cleanups to be called, don't use a hard reg as target.
	 We need to double check this and see if it matters anymore.  */
      if (any_pending_cleanups ())
	{
	  if (target && REG_P (target)
	      && REGNO (target) < FIRST_PSEUDO_REGISTER)
	    target = 0;
	  sibcall_failure = 1;
	}

      if (TYPE_MODE (TREE_TYPE (exp)) == VOIDmode
	  || ignore)
	target = const0_rtx;
      else if (structure_value_addr)
	{
	  if (target == 0 || GET_CODE (target) != MEM)
	    {
	      target
		= gen_rtx_MEM (TYPE_MODE (TREE_TYPE (exp)),
			       memory_address (TYPE_MODE (TREE_TYPE (exp)),
					       structure_value_addr));
	      set_mem_attributes (target, exp, 1);
	    }
	}
      else if (pcc_struct_value)
	{
	  /* This is the special C++ case where we need to
	     know what the true target was.  We take care to
	     never use this value more than once in one expression.  */
	  target = gen_rtx_MEM (TYPE_MODE (TREE_TYPE (exp)),
				copy_to_reg (valreg));
	  set_mem_attributes (target, exp, 1);
	}
      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      else if (GET_CODE (valreg) == PARALLEL)
	{
	  /* Second condition is added because "target" is freed at the
	     the end of "pass0" for -O2 when call is made to
	     expand_end_target_temps ().  Its "in_use" flag has been set
	     to false, so allocate a new temp.  */
	  if (target == 0 || (pass == 1 && target == temp_target))
	    {
	      /* This will only be assigned once, so it can be readonly.  */
	      tree nt = build_qualified_type (TREE_TYPE (exp),
					      (TYPE_QUALS (TREE_TYPE (exp))
					       | TYPE_QUAL_CONST));

	      target = assign_temp (nt, 0, 1, 1);
	      temp_target = target;
	      preserve_temp_slots (target);
	    }

	  if (! rtx_equal_p (target, valreg))
	    emit_group_store (target, valreg, TREE_TYPE (exp),
			      int_size_in_bytes (TREE_TYPE (exp)));

	  /* We can not support sibling calls for this case.  */
	  sibcall_failure = 1;
	}
      else if (target
	       && GET_MODE (target) == TYPE_MODE (TREE_TYPE (exp))
	       && GET_MODE (target) == GET_MODE (valreg))
	{
	  /* TARGET and VALREG cannot be equal at this point because the
	     latter would not have REG_FUNCTION_VALUE_P true, while the
	     former would if it were referring to the same register.

	     If they refer to the same register, this move will be a no-op,
	     except when function inlining is being done.  */
	  emit_move_insn (target, valreg);

	  /* If we are setting a MEM, this code must be executed.  Since it is
	     emitted after the call insn, sibcall optimization cannot be
	     performed in that case.  */
	  if (GET_CODE (target) == MEM)
	    sibcall_failure = 1;
	}
      else if (TYPE_MODE (TREE_TYPE (exp)) == BLKmode)
	{
	  target = copy_blkmode_from_reg (target, valreg, TREE_TYPE (exp));

	  /* We can not support sibling calls for this case.  */
	  sibcall_failure = 1;
	}
      else
	{
	  if (shift_returned_value (TREE_TYPE (exp), &valreg))
	    sibcall_failure = 1;

	  target = copy_to_reg (valreg);
	}

      if (targetm.calls.promote_function_return(funtype))
	{
      /* If we promoted this return value, make the proper SUBREG.  TARGET
	 might be const0_rtx here, so be careful.  */
      if (GET_CODE (target) == REG
	  && TYPE_MODE (TREE_TYPE (exp)) != BLKmode
	  && GET_MODE (target) != TYPE_MODE (TREE_TYPE (exp)))
	{
	  tree type = TREE_TYPE (exp);
	  int unsignedp = TREE_UNSIGNED (type);
	  int offset = 0;

	  /* If we don't promote as expected, something is wrong.  */
	  if (GET_MODE (target)
	      != promote_mode (type, TYPE_MODE (type), &unsignedp, 1))
	    abort ();

	if ((WORDS_BIG_ENDIAN || BYTES_BIG_ENDIAN)
	    && GET_MODE_SIZE (GET_MODE (target))
	       > GET_MODE_SIZE (TYPE_MODE (type)))
	  {
	    offset = GET_MODE_SIZE (GET_MODE (target))
		     - GET_MODE_SIZE (TYPE_MODE (type));
	    if (! BYTES_BIG_ENDIAN)
	      offset = (offset / UNITS_PER_WORD) * UNITS_PER_WORD;
	    else if (! WORDS_BIG_ENDIAN)
	      offset %= UNITS_PER_WORD;
	  }
	  target = gen_rtx_SUBREG (TYPE_MODE (type), target, offset);
	  SUBREG_PROMOTED_VAR_P (target) = 1;
	  SUBREG_PROMOTED_UNSIGNED_SET (target, unsignedp);
	}
	}

      /* If size of args is variable or this was a constructor call for a stack
	 argument, restore saved stack-pointer value.  */

      if (old_stack_level && ! (flags & ECF_SP_DEPRESSED))
	{
	  emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);
	  stack_pointer_delta = old_stack_pointer_delta;
	  pending_stack_adjust = old_pending_adj;
	  stack_arg_under_construction = old_stack_arg_under_construction;
	  highest_outgoing_arg_in_use = initial_highest_arg_in_use;
	  stack_usage_map = initial_stack_usage_map;
	  sibcall_failure = 1;
	}
      else if (ACCUMULATE_OUTGOING_ARGS && pass)
	{
#ifdef REG_PARM_STACK_SPACE
	  if (save_area)
	    restore_fixed_argument_area (save_area, argblock,
					 high_to_save, low_to_save);
#endif

	  /* If we saved any argument areas, restore them.  */
	  for (i = 0; i < num_actuals; i++)
	    if (args[i].save_area)
	      {
		enum machine_mode save_mode = GET_MODE (args[i].save_area);
		rtx stack_area
		  = gen_rtx_MEM (save_mode,
				 memory_address (save_mode,
						 XEXP (args[i].stack_slot, 0)));

		if (save_mode != BLKmode)
		  emit_move_insn (stack_area, args[i].save_area);
		else
		  emit_block_move (stack_area, args[i].save_area,
				   GEN_INT (args[i].locate.size.constant),
				   BLOCK_OP_CALL_PARM);
	      }

	  highest_outgoing_arg_in_use = initial_highest_arg_in_use;
	  stack_usage_map = initial_stack_usage_map;
	}

      /* If this was alloca, record the new stack level for nonlocal gotos.
	 Check for the handler slots since we might not have a save area
	 for non-local gotos.  */

      if ((flags & ECF_MAY_BE_ALLOCA) && nonlocal_goto_handler_slots != 0)
	emit_stack_save (SAVE_NONLOCAL, &nonlocal_goto_stack_level, NULL_RTX);

      /* Free up storage we no longer need.  */
      for (i = 0; i < num_actuals; ++i)
	if (args[i].aligned_regs)
	  free (args[i].aligned_regs);

      if (pass == 0)
	{
	  /* Undo the fake expand_start_target_temps we did earlier.  If
	     there had been any cleanups created, we've already set
	     sibcall_failure.  */
	  expand_end_target_temps ();
	}

      /* If this function is returning into a memory location marked as
	 readonly, it means it is initializing that location. We normally treat
	 functions as not clobbering such locations, so we need to specify that
	 this one does. We do this by adding the appropriate CLOBBER to the
	 CALL_INSN function usage list.  This cannot be done by emitting a
	 standalone CLOBBER after the call because the latter would be ignored
	 by at least the delay slot scheduling pass. We do this now instead of
	 adding to call_fusage before the call to emit_call_1 because TARGET
	 may be modified in the meantime.  */
      if (structure_value_addr != 0 && target != 0
	  && GET_CODE (target) == MEM && RTX_UNCHANGING_P (target))
	add_function_usage_to
	  (last_call_insn (),
	   gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_CLOBBER (VOIDmode, target),
			      NULL_RTX));

      insns = get_insns ();
      end_sequence ();

      if (pass == 0)
	{
	  tail_call_insns = insns;

	  /* Restore the pending stack adjustment now that we have
	     finished generating the sibling call sequence.  */

	  pending_stack_adjust = save_pending_stack_adjust;
	  stack_pointer_delta = save_stack_pointer_delta;

	  /* Prepare arg structure for next iteration.  */
	  for (i = 0; i < num_actuals; i++)
	    {
	      args[i].value = 0;
	      args[i].aligned_regs = 0;
	      args[i].stack = 0;
	    }

	  sbitmap_free (stored_args_map);
	}
      else
	{
	  normal_call_insns = insns;

	  /* Verify that we've deallocated all the stack we used.  */
	  if (! (flags & (ECF_NORETURN | ECF_LONGJMP))
	      && old_stack_allocated != stack_pointer_delta
					- pending_stack_adjust)
	    abort ();
	}

      /* If something prevents making this a sibling call,
	 zero out the sequence.  */
      if (sibcall_failure)
	tail_call_insns = NULL_RTX;
    }

  /* The function optimize_sibling_and_tail_recursive_calls doesn't
     handle CALL_PLACEHOLDERs inside other CALL_PLACEHOLDERs.  This
     can happen if the arguments to this function call an inline
     function who's expansion contains another CALL_PLACEHOLDER.

     If there are any C_Ps in any of these sequences, replace them
     with their normal call.  */

  for (insn = normal_call_insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == CALL_INSN
	&& GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
      replace_call_placeholder (insn, sibcall_use_normal);

  for (insn = tail_call_insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == CALL_INSN
	&& GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
      replace_call_placeholder (insn, sibcall_use_normal);

  for (insn = tail_recursion_insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == CALL_INSN
	&& GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
      replace_call_placeholder (insn, sibcall_use_normal);

  /* If this was a potential tail recursion site, then emit a
     CALL_PLACEHOLDER with the normal and the tail recursion streams.
     One of them will be selected later.  */
  if (tail_recursion_insns || tail_call_insns)
    {
      /* The tail recursion label must be kept around.  We could expose
	 its use in the CALL_PLACEHOLDER, but that creates unwanted edges
	 and makes determining true tail recursion sites difficult.

	 So we set LABEL_PRESERVE_P here, then clear it when we select
	 one of the call sequences after rtl generation is complete.  */
      if (tail_recursion_insns)
	LABEL_PRESERVE_P (tail_recursion_label) = 1;
      emit_call_insn (gen_rtx_CALL_PLACEHOLDER (VOIDmode, normal_call_insns,
						tail_call_insns,
						tail_recursion_insns,
						tail_recursion_label));
    }
  else
    emit_insn (normal_call_insns);

  currently_expanding_call--;

  /* If this function returns with the stack pointer depressed, ensure
     this block saves and restores the stack pointer, show it was
     changed, and adjust for any outgoing arg space.  */
  if (flags & ECF_SP_DEPRESSED)
    {
      clear_pending_stack_adjust ();
      emit_insn (gen_rtx (CLOBBER, VOIDmode, stack_pointer_rtx));
      emit_move_insn (virtual_stack_dynamic_rtx, stack_pointer_rtx);
      save_stack_pointer ();
    }

  return target;
}

/* Traverse an argument list in VALUES and expand all complex
   arguments into their components.  */
tree
split_complex_values (tree values)
{
  tree p;

  /* Before allocating memory, check for the common case of no complex.  */
  for (p = values; p; p = TREE_CHAIN (p))
    {
      tree type = TREE_TYPE (TREE_VALUE (p));
      if (type && TREE_CODE (type) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (type))
        goto found;
    }
  return values;

 found:
  values = copy_list (values);

  for (p = values; p; p = TREE_CHAIN (p))
    {
      tree complex_value = TREE_VALUE (p);
      tree complex_type;

      complex_type = TREE_TYPE (complex_value);
      if (!complex_type)
	continue;

      if (TREE_CODE (complex_type) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (complex_type))
	{
	  tree subtype;
	  tree real, imag, next;

	  subtype = TREE_TYPE (complex_type);
	  complex_value = save_expr (complex_value);
	  real = build1 (REALPART_EXPR, subtype, complex_value);
	  imag = build1 (IMAGPART_EXPR, subtype, complex_value);

	  TREE_VALUE (p) = real;
	  next = TREE_CHAIN (p);
	  imag = build_tree_list (NULL_TREE, imag);
	  TREE_CHAIN (p) = imag;
	  TREE_CHAIN (imag) = next;

	  /* Skip the newly created node.  */
	  p = TREE_CHAIN (p);
	}
    }

  return values;
}

/* Traverse a list of TYPES and expand all complex types into their
   components.  */
tree
split_complex_types (tree types)
{
  tree p;

  /* Before allocating memory, check for the common case of no complex.  */
  for (p = types; p; p = TREE_CHAIN (p))
    {
      tree type = TREE_VALUE (p);
      if (TREE_CODE (type) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (type))
        goto found;
    }
  return types;

 found:
  types = copy_list (types);

  for (p = types; p; p = TREE_CHAIN (p))
    {
      tree complex_type = TREE_VALUE (p);

      if (TREE_CODE (complex_type) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (complex_type))
	{
	  tree next, imag;

	  /* Rewrite complex type with component type.  */
	  TREE_VALUE (p) = TREE_TYPE (complex_type);
	  next = TREE_CHAIN (p);

	  /* Add another component type for the imaginary part.  */
	  imag = build_tree_list (NULL_TREE, TREE_VALUE (p));
	  TREE_CHAIN (p) = imag;
	  TREE_CHAIN (imag) = next;

	  /* Skip the newly created node.  */
	  p = TREE_CHAIN (p);
	}
    }

  return types;
}

/* Output a library call to function FUN (a SYMBOL_REF rtx).
   The RETVAL parameter specifies whether return value needs to be saved, other
   parameters are documented in the emit_library_call function below.  */

static rtx
emit_library_call_value_1 (int retval, rtx orgfun, rtx value,
			   enum libcall_type fn_type,
			   enum machine_mode outmode, int nargs, va_list p)
{
  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  struct args_size original_args_size;
  int argnum;
  rtx fun;
  int inc;
  int count;
  rtx argblock = 0;
  CUMULATIVE_ARGS args_so_far;
  struct arg
  {
    rtx value;
    enum machine_mode mode;
    rtx reg;
    int partial;
    struct locate_and_pad_arg_data locate;
    rtx save_area;
  };
  struct arg *argvec;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  rtx call_fusage = 0;
  rtx mem_value = 0;
  rtx valreg;
  int pcc_struct_value = 0;
  int struct_value_size = 0;
  int flags;
  int reg_parm_stack_space = 0;
  int needed;
  rtx before_call;
  tree tfom;			/* type_for_mode (outmode, 0) */

#ifdef REG_PARM_STACK_SPACE
  /* Define the boundary of the register parm stack space that needs to be
     save, if any.  */
  int low_to_save, high_to_save;
  rtx save_area = 0;            /* Place that it is saved.  */
#endif

  /* Size of the stack reserved for parameter registers.  */
  int initial_highest_arg_in_use = highest_outgoing_arg_in_use;
  char *initial_stack_usage_map = stack_usage_map;

  rtx struct_value = targetm.calls.struct_value_rtx (0, 0);

#ifdef REG_PARM_STACK_SPACE
#ifdef MAYBE_REG_PARM_STACK_SPACE
  reg_parm_stack_space = MAYBE_REG_PARM_STACK_SPACE;
#else
  reg_parm_stack_space = REG_PARM_STACK_SPACE ((tree) 0);
#endif
#endif

  /* By default, library functions can not throw.  */
  flags = ECF_NOTHROW;

  switch (fn_type)
    {
    case LCT_NORMAL:
      break;
    case LCT_CONST:
      flags |= ECF_CONST;
      break;
    case LCT_PURE:
      flags |= ECF_PURE;
      break;
    case LCT_CONST_MAKE_BLOCK:
      flags |= ECF_CONST | ECF_LIBCALL_BLOCK;
      break;
    case LCT_PURE_MAKE_BLOCK:
      flags |= ECF_PURE | ECF_LIBCALL_BLOCK;
      break;
    case LCT_NORETURN:
      flags |= ECF_NORETURN;
      break;
    case LCT_THROW:
      flags = ECF_NORETURN;
      break;
    case LCT_ALWAYS_RETURN:
      flags = ECF_ALWAYS_RETURN;
      break;
    case LCT_RETURNS_TWICE:
      flags = ECF_RETURNS_TWICE;
      break;
    }
  fun = orgfun;

  /* Ensure current function's preferred stack boundary is at least
     what we need.  */
  if (cfun->preferred_stack_boundary < PREFERRED_STACK_BOUNDARY)
    cfun->preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;

  /* If this kind of value comes back in memory,
     decide where in memory it should come back.  */
  if (outmode != VOIDmode)
    {
      tfom = (*lang_hooks.types.type_for_mode) (outmode, 0);
      if (aggregate_value_p (tfom, 0))
	{
#ifdef PCC_STATIC_STRUCT_RETURN
	  rtx pointer_reg
	    = hard_function_value (build_pointer_type (tfom), 0, 0);
	  mem_value = gen_rtx_MEM (outmode, pointer_reg);
	  pcc_struct_value = 1;
	  if (value == 0)
	    value = gen_reg_rtx (outmode);
#else /* not PCC_STATIC_STRUCT_RETURN */
	  struct_value_size = GET_MODE_SIZE (outmode);
	  if (value != 0 && GET_CODE (value) == MEM)
	    mem_value = value;
	  else
	    mem_value = assign_temp (tfom, 0, 1, 1);
#endif
	  /* This call returns a big structure.  */
	  flags &= ~(ECF_CONST | ECF_PURE | ECF_LIBCALL_BLOCK);
	}
    }
  else
    tfom = void_type_node;

  /* ??? Unfinished: must pass the memory address as an argument.  */

  /* Copy all the libcall-arguments out of the varargs data
     and into a vector ARGVEC.

     Compute how to pass each argument.  We only support a very small subset
     of the full argument passing conventions to limit complexity here since
     library functions shouldn't have many args.  */

  argvec = alloca ((nargs + 1) * sizeof (struct arg));
  memset (argvec, 0, (nargs + 1) * sizeof (struct arg));

#ifdef INIT_CUMULATIVE_LIBCALL_ARGS
  INIT_CUMULATIVE_LIBCALL_ARGS (args_so_far, outmode, fun);
#else
  INIT_CUMULATIVE_ARGS (args_so_far, NULL_TREE, fun, 0, nargs);
#endif

  args_size.constant = 0;
  args_size.var = 0;

  count = 0;

  /* Now we are about to start emitting insns that can be deleted
     if a libcall is deleted.  */
  if (flags & ECF_LIBCALL_BLOCK)
    start_sequence ();

  push_temp_slots ();

  /* If there's a structure value address to be passed,
     either pass it in the special place, or pass it as an extra argument.  */
  if (mem_value && struct_value == 0 && ! pcc_struct_value)
    {
      rtx addr = XEXP (mem_value, 0);
      nargs++;

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (GET_CODE (addr) != REG && GET_CODE (addr) != MEM
	  && ! (CONSTANT_P (addr) && LEGITIMATE_CONSTANT_P (addr)))
	addr = force_operand (addr, NULL_RTX);

      argvec[count].value = addr;
      argvec[count].mode = Pmode;
      argvec[count].partial = 0;

      argvec[count].reg = FUNCTION_ARG (args_so_far, Pmode, NULL_TREE, 1);
#ifdef FUNCTION_ARG_PARTIAL_NREGS
      if (FUNCTION_ARG_PARTIAL_NREGS (args_so_far, Pmode, NULL_TREE, 1))
	abort ();
#endif

      locate_and_pad_parm (Pmode, NULL_TREE,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
                           1,
#else
			   argvec[count].reg != 0,
#endif
			   0, NULL_TREE, &args_size, &argvec[count].locate);

      if (argvec[count].reg == 0 || argvec[count].partial != 0
	  || reg_parm_stack_space > 0)
	args_size.constant += argvec[count].locate.size.constant;

      FUNCTION_ARG_ADVANCE (args_so_far, Pmode, (tree) 0, 1);

      count++;
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

      /* There's no need to call protect_from_queue, because
	 either emit_move_insn or emit_push_insn will do that.  */

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (GET_CODE (val) != REG && GET_CODE (val) != MEM
	  && ! (CONSTANT_P (val) && LEGITIMATE_CONSTANT_P (val)))
	val = force_operand (val, NULL_RTX);

#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
      if (FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, mode, NULL_TREE, 1))
	{
	  rtx slot;
	  int must_copy = 1
#ifdef FUNCTION_ARG_CALLEE_COPIES
	    && ! FUNCTION_ARG_CALLEE_COPIES (args_so_far, mode,
					     NULL_TREE, 1)
#endif
	    ;

	  /* loop.c won't look at CALL_INSN_FUNCTION_USAGE of const/pure
	     functions, so we have to pretend this isn't such a function.  */
	  if (flags & ECF_LIBCALL_BLOCK)
	    {
	      rtx insns = get_insns ();
	      end_sequence ();
	      emit_insn (insns);
	    }
	  flags &= ~(ECF_CONST | ECF_PURE | ECF_LIBCALL_BLOCK);

	  /* If this was a CONST function, it is now PURE since
	     it now reads memory.  */
	  if (flags & ECF_CONST)
	    {
	      flags &= ~ECF_CONST;
	      flags |= ECF_PURE;
	    }

	  if (GET_MODE (val) == MEM && ! must_copy)
	    slot = val;
	  else if (must_copy)
	    {
	      slot = assign_temp ((*lang_hooks.types.type_for_mode) (mode, 0),
				  0, 1, 1);
	      emit_move_insn (slot, val);
	    }
	  else
	    {
	      tree type = (*lang_hooks.types.type_for_mode) (mode, 0);

	      slot
		= gen_rtx_MEM (mode,
			       expand_expr (build1 (ADDR_EXPR,
						    build_pointer_type (type),
						    make_tree (type, val)),
					    NULL_RTX, VOIDmode, 0));
	    }

	  call_fusage = gen_rtx_EXPR_LIST (VOIDmode,
					   gen_rtx_USE (VOIDmode, slot),
					   call_fusage);
	  if (must_copy)
	    call_fusage = gen_rtx_EXPR_LIST (VOIDmode,
					     gen_rtx_CLOBBER (VOIDmode,
							      slot),
					     call_fusage);

	  mode = Pmode;
	  val = force_operand (XEXP (slot, 0), NULL_RTX);
	}
#endif

      argvec[count].value = val;
      argvec[count].mode = mode;

      argvec[count].reg = FUNCTION_ARG (args_so_far, mode, NULL_TREE, 1);

#ifdef FUNCTION_ARG_PARTIAL_NREGS
      argvec[count].partial
	= FUNCTION_ARG_PARTIAL_NREGS (args_so_far, mode, NULL_TREE, 1);
#else
      argvec[count].partial = 0;
#endif

      locate_and_pad_parm (mode, NULL_TREE,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			   1,
#else
			   argvec[count].reg != 0,
#endif
			   argvec[count].partial,
			   NULL_TREE, &args_size, &argvec[count].locate);

      if (argvec[count].locate.size.var)
	abort ();

      if (argvec[count].reg == 0 || argvec[count].partial != 0
	  || reg_parm_stack_space > 0)
	args_size.constant += argvec[count].locate.size.constant;

      FUNCTION_ARG_ADVANCE (args_so_far, mode, (tree) 0, 1);
    }

#ifdef FINAL_REG_PARM_STACK_SPACE
  reg_parm_stack_space = FINAL_REG_PARM_STACK_SPACE (args_size.constant,
						     args_size.var);
#endif
  /* If this machine requires an external definition for library
     functions, write one out.  */
  assemble_external_libcall (fun);

  original_args_size = args_size;
  args_size.constant = (((args_size.constant
			  + stack_pointer_delta
			  + STACK_BYTES - 1)
			  / STACK_BYTES
			  * STACK_BYTES)
			 - stack_pointer_delta);

  args_size.constant = MAX (args_size.constant,
			    reg_parm_stack_space);

#ifndef OUTGOING_REG_PARM_STACK_SPACE
  args_size.constant -= reg_parm_stack_space;
#endif

  if (args_size.constant > current_function_outgoing_args_size)
    current_function_outgoing_args_size = args_size.constant;

  if (ACCUMULATE_OUTGOING_ARGS)
    {
      /* Since the stack pointer will never be pushed, it is possible for
	 the evaluation of a parm to clobber something we have already
	 written to the stack.  Since most function calls on RISC machines
	 do not use the stack, this is uncommon, but must work correctly.

	 Therefore, we save any area of the stack that was already written
	 and that we are using.  Here we set up to do this by making a new
	 stack usage map from the old one.

	 Another approach might be to try to reorder the argument
	 evaluations to avoid this conflicting stack usage.  */

      needed = args_size.constant;

#ifndef OUTGOING_REG_PARM_STACK_SPACE
      /* Since we will be writing into the entire argument area, the
	 map must be allocated for its entire size, not just the part that
	 is the responsibility of the caller.  */
      needed += reg_parm_stack_space;
#endif

#ifdef ARGS_GROW_DOWNWARD
      highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use,
					 needed + 1);
#else
      highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use,
					 needed);
#endif
      stack_usage_map = alloca (highest_outgoing_arg_in_use);

      if (initial_highest_arg_in_use)
	memcpy (stack_usage_map, initial_stack_usage_map,
		initial_highest_arg_in_use);

      if (initial_highest_arg_in_use != highest_outgoing_arg_in_use)
	memset (&stack_usage_map[initial_highest_arg_in_use], 0,
	       highest_outgoing_arg_in_use - initial_highest_arg_in_use);
      needed = 0;

      /* We must be careful to use virtual regs before they're instantiated,
         and real regs afterwards.  Loop optimization, for example, can create
	 new libcalls after we've instantiated the virtual regs, and if we
	 use virtuals anyway, they won't match the rtl patterns.  */

      if (virtuals_instantiated)
	argblock = plus_constant (stack_pointer_rtx, STACK_POINTER_OFFSET);
      else
	argblock = virtual_outgoing_args_rtx;
    }
  else
    {
      if (!PUSH_ARGS)
	argblock = push_block (GEN_INT (args_size.constant), 0, 0);
    }

  /* If we push args individually in reverse order, perform stack alignment
     before the first push (the last arg).  */
  if (argblock == 0 && PUSH_ARGS_REVERSED)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));

  if (PUSH_ARGS_REVERSED)
    {
      inc = -1;
      argnum = nargs - 1;
    }
  else
    {
      inc = 1;
      argnum = 0;
    }

#ifdef REG_PARM_STACK_SPACE
  if (ACCUMULATE_OUTGOING_ARGS)
    {
      /* The argument list is the property of the called routine and it
	 may clobber it.  If the fixed area has been used for previous
	 parameters, we must save and restore it.  */
      save_area = save_fixed_argument_area (reg_parm_stack_space, argblock,
					    &low_to_save, &high_to_save);
    }
#endif

  /* Push the args that need to be pushed.  */

  /* ARGNUM indexes the ARGVEC array in the order in which the arguments
     are to be pushed.  */
  for (count = 0; count < nargs; count++, argnum += inc)
    {
      enum machine_mode mode = argvec[argnum].mode;
      rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;
      int lower_bound = 0, upper_bound = 0, i;

      if (! (reg != 0 && partial == 0))
	{
	  if (ACCUMULATE_OUTGOING_ARGS)
	    {
	      /* If this is being stored into a pre-allocated, fixed-size,
		 stack area, save any previous data at that location.  */

#ifdef ARGS_GROW_DOWNWARD
	      /* stack_slot is negative, but we want to index stack_usage_map
		 with positive values.  */
	      upper_bound = -argvec[argnum].locate.offset.constant + 1;
	      lower_bound = upper_bound - argvec[argnum].locate.size.constant;
#else
	      lower_bound = argvec[argnum].locate.offset.constant;
	      upper_bound = lower_bound + argvec[argnum].locate.size.constant;
#endif

	      i = lower_bound;
	      /* Don't worry about things in the fixed argument area;
		 it has already been saved.  */
	      if (i < reg_parm_stack_space)
		i = reg_parm_stack_space;
	      while (i < upper_bound && stack_usage_map[i] == 0)
		i++;

	      if (i < upper_bound)
		{
		  /* We need to make a save area.  */
		  unsigned int size
		    = argvec[argnum].locate.size.constant * BITS_PER_UNIT;
		  enum machine_mode save_mode
		    = mode_for_size (size, MODE_INT, 1);
		  rtx adr
		    = plus_constant (argblock,
				     argvec[argnum].locate.offset.constant);
		  rtx stack_area
		    = gen_rtx_MEM (save_mode, memory_address (save_mode, adr));

		  if (save_mode == BLKmode)
		    {
		      argvec[argnum].save_area
			= assign_stack_temp (BLKmode,
				             argvec[argnum].locate.size.constant,
					     0);

		      emit_block_move (validize_mem (argvec[argnum].save_area),
			  	       stack_area,
				       GEN_INT (argvec[argnum].locate.size.constant),
				       BLOCK_OP_CALL_PARM);
		    }
		  else
		    {
		      argvec[argnum].save_area = gen_reg_rtx (save_mode);

		      emit_move_insn (argvec[argnum].save_area, stack_area);
		    }
		}
	    }

	  emit_push_insn (val, mode, NULL_TREE, NULL_RTX, PARM_BOUNDARY,
			  partial, reg, 0, argblock,
			  GEN_INT (argvec[argnum].locate.offset.constant),
			  reg_parm_stack_space,
			  ARGS_SIZE_RTX (argvec[argnum].locate.alignment_pad));

	  /* Now mark the segment we just used.  */
	  if (ACCUMULATE_OUTGOING_ARGS)
	    for (i = lower_bound; i < upper_bound; i++)
	      stack_usage_map[i] = 1;

	  NO_DEFER_POP;
	}
    }

  /* If we pushed args in forward order, perform stack alignment
     after pushing the last arg.  */
  if (argblock == 0 && !PUSH_ARGS_REVERSED)
    anti_adjust_stack (GEN_INT (args_size.constant
				- original_args_size.constant));

  if (PUSH_ARGS_REVERSED)
    argnum = nargs - 1;
  else
    argnum = 0;

  fun = prepare_call_address (fun, NULL_TREE, &call_fusage, 0, 0);

  /* Now load any reg parms into their regs.  */

  /* ARGNUM indexes the ARGVEC array in the order in which the arguments
     are to be pushed.  */
  for (count = 0; count < nargs; count++, argnum += inc)
    {
      rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      /* Handle calls that pass values in multiple non-contiguous
	 locations.  The PA64 has examples of this for library calls.  */
      if (reg != 0 && GET_CODE (reg) == PARALLEL)
	emit_group_load (reg, val, NULL_TREE, GET_MODE_SIZE (GET_MODE (val)));
      else if (reg != 0 && partial == 0)
	emit_move_insn (reg, val);

      NO_DEFER_POP;
    }

  /* Any regs containing parms remain in use through the call.  */
  for (count = 0; count < nargs; count++)
    {
      rtx reg = argvec[count].reg;
      if (reg != 0 && GET_CODE (reg) == PARALLEL)
	use_group_regs (&call_fusage, reg);
      else if (reg != 0)
	use_reg (&call_fusage, reg);
    }

  /* Pass the function the address in which to return a structure value.  */
  if (mem_value != 0 && struct_value != 0 && ! pcc_struct_value)
    {
      emit_move_insn (struct_value,
		      force_reg (Pmode,
				 force_operand (XEXP (mem_value, 0),
						NULL_RTX)));
      if (GET_CODE (struct_value) == REG)
	use_reg (&call_fusage, struct_value);
    }

  /* Don't allow popping to be deferred, since then
     cse'ing of library calls could delete a call and leave the pop.  */
  NO_DEFER_POP;
  valreg = (mem_value == 0 && outmode != VOIDmode
	    ? hard_libcall_value (outmode) : NULL_RTX);

  /* Stack must be properly aligned now.  */
  if (stack_pointer_delta & (PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT - 1))
    abort ();

  before_call = get_last_insn ();

  /* We pass the old value of inhibit_defer_pop + 1 to emit_call_1, which
     will set inhibit_defer_pop to that value.  */
  /* The return type is needed to decide how many bytes the function pops.
     Signedness plays no role in that, so for simplicity, we pretend it's
     always signed.  We also assume that the list of arguments passed has
     no impact, so we pretend it is unknown.  */

  emit_call_1 (fun,
	       get_identifier (XSTR (orgfun, 0)),
	       build_function_type (tfom, NULL_TREE),
	       original_args_size.constant, args_size.constant,
	       struct_value_size,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       valreg,
	       old_inhibit_defer_pop + 1, call_fusage, flags, & args_so_far);

  /* For calls to `setjmp', etc., inform flow.c it should complain
     if nonvolatile values are live.  For functions that cannot return,
     inform flow that control does not fall through.  */

  if (flags & (ECF_NORETURN | ECF_LONGJMP))
    {
      /* The barrier note must be emitted
	 immediately after the CALL_INSN.  Some ports emit more than
	 just a CALL_INSN above, so we must search for it here.  */

      rtx last = get_last_insn ();
      while (GET_CODE (last) != CALL_INSN)
	{
	  last = PREV_INSN (last);
	  /* There was no CALL_INSN?  */
	  if (last == before_call)
	    abort ();
	}

      emit_barrier_after (last);
    }

  /* Now restore inhibit_defer_pop to its actual original value.  */
  OK_DEFER_POP;

  /* If call is cse'able, make appropriate pair of reg-notes around it.
     Test valreg so we don't crash; may safely ignore `const'
     if return type is void.  Disable for PARALLEL return values, because
     we have no way to move such values into a pseudo register.  */
  if (flags & ECF_LIBCALL_BLOCK)
    {
      rtx insns;

      if (valreg == 0)
	{
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	}
      else
	{
	  rtx note = 0;
	  rtx temp;
	  int i;

	  if (GET_CODE (valreg) == PARALLEL)
	    {
	      temp = gen_reg_rtx (outmode);
	      emit_group_store (temp, valreg, NULL_TREE, 
				GET_MODE_SIZE (outmode));
	      valreg = temp;
	    }

	  temp = gen_reg_rtx (GET_MODE (valreg));

	  /* Construct an "equal form" for the value which mentions all the
	     arguments in order as well as the function name.  */
	  for (i = 0; i < nargs; i++)
	    note = gen_rtx_EXPR_LIST (VOIDmode, argvec[i].value, note);
	  note = gen_rtx_EXPR_LIST (VOIDmode, fun, note);

	  insns = get_insns ();
	  end_sequence ();

	  if (flags & ECF_PURE)
	    note = gen_rtx_EXPR_LIST (VOIDmode,
			gen_rtx_USE (VOIDmode,
				     gen_rtx_MEM (BLKmode,
						  gen_rtx_SCRATCH (VOIDmode))),
			note);

	  emit_libcall_block (insns, temp, valreg, note);

	  valreg = temp;
	}
    }
  pop_temp_slots ();

  /* Copy the value to the right place.  */
  if (outmode != VOIDmode && retval)
    {
      if (mem_value)
	{
	  if (value == 0)
	    value = mem_value;
	  if (value != mem_value)
	    emit_move_insn (value, mem_value);
	}
      else if (GET_CODE (valreg) == PARALLEL)
	{
	  if (value == 0)
	    value = gen_reg_rtx (outmode);
	  emit_group_store (value, valreg, NULL_TREE, GET_MODE_SIZE (outmode));
	}
      else if (value != 0)
	emit_move_insn (value, valreg);
      else
	value = valreg;
    }

  if (ACCUMULATE_OUTGOING_ARGS)
    {
#ifdef REG_PARM_STACK_SPACE
      if (save_area)
	restore_fixed_argument_area (save_area, argblock,
				     high_to_save, low_to_save);
#endif

      /* If we saved any argument areas, restore them.  */
      for (count = 0; count < nargs; count++)
	if (argvec[count].save_area)
	  {
	    enum machine_mode save_mode = GET_MODE (argvec[count].save_area);
	    rtx adr = plus_constant (argblock,
				     argvec[count].locate.offset.constant);
	    rtx stack_area = gen_rtx_MEM (save_mode,
					  memory_address (save_mode, adr));

	    if (save_mode == BLKmode)
	      emit_block_move (stack_area,
		  	       validize_mem (argvec[count].save_area),
			       GEN_INT (argvec[count].locate.size.constant),
			       BLOCK_OP_CALL_PARM);
	    else
	      emit_move_insn (stack_area, argvec[count].save_area);
	  }

      highest_outgoing_arg_in_use = initial_highest_arg_in_use;
      stack_usage_map = initial_stack_usage_map;
    }

  return value;

}

/* Output a library call to function FUN (a SYMBOL_REF rtx)
   (emitting the queue unless NO_QUEUE is nonzero),
   for a value of mode OUTMODE,
   with NARGS different arguments, passed as alternating rtx values
   and machine_modes to convert them to.
   The rtx values should have been passed through protect_from_queue already.

   FN_TYPE should be LCT_NORMAL for `normal' calls, LCT_CONST for `const'
   calls, LCT_PURE for `pure' calls, LCT_CONST_MAKE_BLOCK for `const' calls
   which should be enclosed in REG_LIBCALL/REG_RETVAL notes,
   LCT_PURE_MAKE_BLOCK for `purep' calls which should be enclosed in
   REG_LIBCALL/REG_RETVAL notes with extra (use (memory (scratch)),
   or other LCT_ value for other types of library calls.  */

void
emit_library_call (rtx orgfun, enum libcall_type fn_type,
		   enum machine_mode outmode, int nargs, ...)
{
  va_list p;

  va_start (p, nargs);
  emit_library_call_value_1 (0, orgfun, NULL_RTX, fn_type, outmode, nargs, p);
  va_end (p);
}

/* Like emit_library_call except that an extra argument, VALUE,
   comes second and says where to store the result.
   (If VALUE is zero, this function chooses a convenient way
   to return the value.

   This function returns an rtx for where the value is to be found.
   If VALUE is nonzero, VALUE is returned.  */

rtx
emit_library_call_value (rtx orgfun, rtx value,
			 enum libcall_type fn_type,
			 enum machine_mode outmode, int nargs, ...)
{
  rtx result;
  va_list p;

  va_start (p, nargs);
  result = emit_library_call_value_1 (1, orgfun, value, fn_type, outmode,
				      nargs, p);
  va_end (p);

  return result;
}

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

   FNDECL is the declaration of the function we are calling.

   Return nonzero if this arg should cause sibcall failure,
   zero otherwise.  */

static int
store_one_arg (struct arg_data *arg, rtx argblock, int flags,
	       int variable_size ATTRIBUTE_UNUSED, int reg_parm_stack_space)
{
  tree pval = arg->tree_value;
  rtx reg = 0;
  int partial = 0;
  int used = 0;
  int i, lower_bound = 0, upper_bound = 0;
  int sibcall_failure = 0;

  if (TREE_CODE (pval) == ERROR_MARK)
    return 1;

  /* Push a new temporary level for any temporaries we make for
     this argument.  */
  push_temp_slots ();

  if (ACCUMULATE_OUTGOING_ARGS && !(flags & ECF_SIBCALL))
    {
      /* If this is being stored into a pre-allocated, fixed-size, stack area,
	 save any previous data at that location.  */
      if (argblock && ! variable_size && arg->stack)
	{
#ifdef ARGS_GROW_DOWNWARD
	  /* stack_slot is negative, but we want to index stack_usage_map
	     with positive values.  */
	  if (GET_CODE (XEXP (arg->stack_slot, 0)) == PLUS)
	    upper_bound = -INTVAL (XEXP (XEXP (arg->stack_slot, 0), 1)) + 1;
	  else
	    upper_bound = 0;

	  lower_bound = upper_bound - arg->locate.size.constant;
#else
	  if (GET_CODE (XEXP (arg->stack_slot, 0)) == PLUS)
	    lower_bound = INTVAL (XEXP (XEXP (arg->stack_slot, 0), 1));
	  else
	    lower_bound = 0;

	  upper_bound = lower_bound + arg->locate.size.constant;
#endif

	  i = lower_bound;
	  /* Don't worry about things in the fixed argument area;
	     it has already been saved.  */
	  if (i < reg_parm_stack_space)
	    i = reg_parm_stack_space;
	  while (i < upper_bound && stack_usage_map[i] == 0)
	    i++;

	  if (i < upper_bound)
	    {
	      /* We need to make a save area.  */
	      unsigned int size = arg->locate.size.constant * BITS_PER_UNIT;
	      enum machine_mode save_mode = mode_for_size (size, MODE_INT, 1);
	      rtx adr = memory_address (save_mode, XEXP (arg->stack_slot, 0));
	      rtx stack_area = gen_rtx_MEM (save_mode, adr);

	      if (save_mode == BLKmode)
		{
		  tree ot = TREE_TYPE (arg->tree_value);
		  tree nt = build_qualified_type (ot, (TYPE_QUALS (ot)
						       | TYPE_QUAL_CONST));

		  arg->save_area = assign_temp (nt, 0, 1, 1);
		  preserve_temp_slots (arg->save_area);
		  emit_block_move (validize_mem (arg->save_area), stack_area,
				   expr_size (arg->tree_value),
				   BLOCK_OP_CALL_PARM);
		}
	      else
		{
		  arg->save_area = gen_reg_rtx (save_mode);
		  emit_move_insn (arg->save_area, stack_area);
		}
	    }
	}
    }

  /* If this isn't going to be placed on both the stack and in registers,
     set up the register and number of words.  */
  if (! arg->pass_on_stack)
    {
      if (flags & ECF_SIBCALL)
	reg = arg->tail_call_reg;
      else
	reg = arg->reg;
      partial = arg->partial;
    }

  if (reg != 0 && partial == 0)
    /* Being passed entirely in a register.  We shouldn't be called in
       this case.  */
    abort ();

  /* If this arg needs special alignment, don't load the registers
     here.  */
  if (arg->n_aligned_regs != 0)
    reg = 0;

  /* If this is being passed partially in a register, we can't evaluate
     it directly into its stack slot.  Otherwise, we can.  */
  if (arg->value == 0)
    {
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

      arg->value = expand_expr (pval,
				(partial
				 || TYPE_MODE (TREE_TYPE (pval)) != arg->mode)
				? NULL_RTX : arg->stack,
				VOIDmode, EXPAND_STACK_PARM);

      /* If we are promoting object (or for any other reason) the mode
	 doesn't agree, convert the mode.  */

      if (arg->mode != TYPE_MODE (TREE_TYPE (pval)))
	arg->value = convert_modes (arg->mode, TYPE_MODE (TREE_TYPE (pval)),
				    arg->value, arg->unsignedp);

      if (arg->pass_on_stack)
	stack_arg_under_construction--;
    }

  /* Don't allow anything left on stack from computation
     of argument to alloca.  */
  if (flags & ECF_MAY_BE_ALLOCA)
    do_pending_stack_adjust ();

  if (arg->value == arg->stack)
    /* If the value is already in the stack slot, we are done.  */
    ;
  else if (arg->mode != BLKmode)
    {
      int size;

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
		      PARM_BOUNDARY, partial, reg, used - size, argblock,
		      ARGS_SIZE_RTX (arg->locate.offset), reg_parm_stack_space,
		      ARGS_SIZE_RTX (arg->locate.alignment_pad));

      /* Unless this is a partially-in-register argument, the argument is now
	 in the stack.  */
      if (partial == 0)
	arg->value = arg->stack;
    }
  else
    {
      /* BLKmode, at least partly to be pushed.  */

      unsigned int parm_align;
      int excess;
      rtx size_rtx;

      /* Pushing a nonscalar.
	 If part is passed in registers, PARTIAL says how much
	 and emit_push_insn will take care of putting it there.  */

      /* Round its size up to a multiple
	 of the allocation unit for arguments.  */

      if (arg->locate.size.var != 0)
	{
	  excess = 0;
	  size_rtx = ARGS_SIZE_RTX (arg->locate.size);
	}
      else
	{
	  /* PUSH_ROUNDING has no effect on us, because
	     emit_push_insn for BLKmode is careful to avoid it.  */
	  if (reg && GET_CODE (reg) == PARALLEL)
	  {
	    /* Use the size of the elt to compute excess.  */
	    rtx elt = XEXP (XVECEXP (reg, 0, 0), 0);
	    excess = (arg->locate.size.constant
		      - int_size_in_bytes (TREE_TYPE (pval))
		      + partial * GET_MODE_SIZE (GET_MODE (elt)));
	  } 
	  else
	    excess = (arg->locate.size.constant
		      - int_size_in_bytes (TREE_TYPE (pval))
		      + partial * UNITS_PER_WORD);
	  size_rtx = expand_expr (size_in_bytes (TREE_TYPE (pval)),
				  NULL_RTX, TYPE_MODE (sizetype), 0);
	}

      /* Some types will require stricter alignment, which will be
	 provided for elsewhere in argument layout.  */
      parm_align = MAX (PARM_BOUNDARY, TYPE_ALIGN (TREE_TYPE (pval)));

      /* When an argument is padded down, the block is aligned to
	 PARM_BOUNDARY, but the actual argument isn't.  */
      if (FUNCTION_ARG_PADDING (arg->mode, TREE_TYPE (pval)) == downward)
	{
	  if (arg->locate.size.var)
	    parm_align = BITS_PER_UNIT;
	  else if (excess)
	    {
	      unsigned int excess_align = (excess & -excess) * BITS_PER_UNIT;
	      parm_align = MIN (parm_align, excess_align);
	    }
	}

      if ((flags & ECF_SIBCALL) && GET_CODE (arg->value) == MEM)
	{
	  /* emit_push_insn might not work properly if arg->value and
	     argblock + arg->locate.offset areas overlap.  */
	  rtx x = arg->value;
	  int i = 0;

	  if (XEXP (x, 0) == current_function_internal_arg_pointer
	      || (GET_CODE (XEXP (x, 0)) == PLUS
		  && XEXP (XEXP (x, 0), 0) ==
		     current_function_internal_arg_pointer
		  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT))
	    {
	      if (XEXP (x, 0) != current_function_internal_arg_pointer)
		i = INTVAL (XEXP (XEXP (x, 0), 1));

	      /* expand_call should ensure this.  */
	      if (arg->locate.offset.var || GET_CODE (size_rtx) != CONST_INT)
		abort ();

	      if (arg->locate.offset.constant > i)
		{
		  if (arg->locate.offset.constant < i + INTVAL (size_rtx))
		    sibcall_failure = 1;
		}
	      else if (arg->locate.offset.constant < i)
		{
		  if (i < arg->locate.offset.constant + INTVAL (size_rtx))
		    sibcall_failure = 1;
		}
	    }
	}

      emit_push_insn (arg->value, arg->mode, TREE_TYPE (pval), size_rtx,
		      parm_align, partial, reg, excess, argblock,
		      ARGS_SIZE_RTX (arg->locate.offset), reg_parm_stack_space,
		      ARGS_SIZE_RTX (arg->locate.alignment_pad));

      /* Unless this is a partially-in-register argument, the argument is now
	 in the stack.

	 ??? Unlike the case above, in which we want the actual
	 address of the data, so that we can load it directly into a
	 register, here we want the address of the stack slot, so that
	 it's properly aligned for word-by-word copying or something
	 like that.  It's not clear that this is always correct.  */
      if (partial == 0)
	arg->value = arg->stack_slot;
    }

  /* Mark all slots this store used.  */
  if (ACCUMULATE_OUTGOING_ARGS && !(flags & ECF_SIBCALL)
      && argblock && ! variable_size && arg->stack)
    for (i = lower_bound; i < upper_bound; i++)
      stack_usage_map[i] = 1;

  /* Once we have pushed something, pops can't safely
     be deferred during the rest of the arguments.  */
  NO_DEFER_POP;

  /* ANSI doesn't require a sequence point here,
     but PCC has one, so this will avoid some problems.  */
  emit_queue ();

  /* Free any temporary slots made in processing this argument.  Show
     that we might have taken the address of something and pushed that
     as an operand.  */
  preserve_temp_slots (NULL_RTX);
  free_temp_slots ();
  pop_temp_slots ();

  return sibcall_failure;
}

/* Nonzero if we do not know how to pass TYPE solely in registers.
   We cannot do so in the following cases:

   - if the type has variable size
   - if the type is marked as addressable (it is required to be constructed
     into the stack)
   - if the padding and mode of the type is such that a copy into a register
     would put it into the wrong part of the register.

   Which padding can't be supported depends on the byte endianness.

   A value in a register is implicitly padded at the most significant end.
   On a big-endian machine, that is the lower end in memory.
   So a value padded in memory at the upper end can't go in a register.
   For a little-endian machine, the reverse is true.  */

bool
default_must_pass_in_stack (enum machine_mode mode, tree type)
{
  if (!type)
    return false;

  /* If the type has variable size...  */
  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return true;

  /* If the type is marked as addressable (it is required
     to be constructed into the stack)...  */
  if (TREE_ADDRESSABLE (type))
    return true;

  /* If the padding and mode of the type is such that a copy into
     a register would put it into the wrong part of the register.  */
  if (mode == BLKmode
      && int_size_in_bytes (type) % (PARM_BOUNDARY / BITS_PER_UNIT)
      && (FUNCTION_ARG_PADDING (mode, type)
	  == (BYTES_BIG_ENDIAN ? upward : downward)))
    return true;

  return false;
}
