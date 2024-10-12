/* Convert function calls to rtl insns, for GNU C compiler.
   Copyright (C) 1989-2024 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "varasm.h"
#include "internal-fn.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "expr.h"
#include "output.h"
#include "langhooks.h"
#include "except.h"
#include "dbgcnt.h"
#include "rtl-iter.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "intl.h"
#include "stringpool.h"
#include "hash-map.h"
#include "hash-traits.h"
#include "attribs.h"
#include "builtins.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "attr-fnspec.h"
#include "value-query.h"
#include "tree-pretty-print.h"
#include "tree-eh.h"

/* Like PREFERRED_STACK_BOUNDARY but in units of bytes, not bits.  */
#define STACK_BYTES (PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT)

/* Data structure and subroutines used within expand_call.  */

struct arg_data
{
  /* Tree node for this argument.  */
  tree tree_value;
  /* Mode for value; TYPE_MODE unless promoted.  */
  machine_mode mode;
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
  /* If REG is a PARALLEL, this is a copy of VALUE pulled into the correct
     form for emit_group_move.  */
  rtx parallel_value;
  /* If REG was promoted from the actual mode of the argument expression,
     indicates whether the promotion is sign- or zero-extended.  */
  int unsignedp;
  /* Number of bytes to put in registers.  0 means put the whole arg
     in registers.  Also 0 if not passed in registers.  */
  int partial;
  /* True if argument must be passed on stack.
     Note that some arguments may be passed on the stack
     even though pass_on_stack is false, just because FUNCTION_ARG says so.
     pass_on_stack identifies arguments that *cannot* go in registers.  */
  bool pass_on_stack;
  /* Some fields packaged up for locate_and_pad_parm.  */
  struct locate_and_pad_arg_data locate;
  /* Location on the stack at which parameter should be stored.  The store
     has already been done if STACK == VALUE.  */
  rtx stack;
  /* Location on the stack of the start of this argument slot.  This can
     differ from STACK if this arg pads downward.  This location is known
     to be aligned to TARGET_FUNCTION_ARG_BOUNDARY.  */
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
static unsigned int highest_outgoing_arg_in_use;

/* Assume that any stack location at this byte index is used,
   without checking the contents of stack_usage_map.  */
static unsigned HOST_WIDE_INT stack_usage_watermark = HOST_WIDE_INT_M1U;

/* A bitmap of virtual-incoming stack space.  Bit is set if the corresponding
   stack location's tail call argument has been already stored into the stack.
   This bitmap is used to prevent sibling call optimization if function tries
   to use parent's incoming argument slots when they have been already
   overwritten with tail call arguments.  */
static sbitmap stored_args_map;

/* Assume that any virtual-incoming location at this byte index has been
   stored, without checking the contents of stored_args_map.  */
static unsigned HOST_WIDE_INT stored_args_watermark;

/* stack_arg_under_construction is nonzero when an argument may be
   initialized with a constructor call (including a C function that
   returns a BLKmode struct) and expand_call must take special action
   to make sure the object being constructed does not overlap the
   argument list for the constructor call.  */
static int stack_arg_under_construction;

static void precompute_register_parameters (int, struct arg_data *, int *);
static bool store_one_arg (struct arg_data *, rtx, int, int, int);
static void store_unaligned_arguments_into_pseudos (struct arg_data *, int);
static bool finalize_must_preallocate (bool, int, struct arg_data *,
				       struct args_size *);
static void precompute_arguments (int, struct arg_data *);
static void compute_argument_addresses (struct arg_data *, rtx, int);
static rtx rtx_for_function_call (tree, tree);
static void load_register_parameters (struct arg_data *, int, rtx *, int,
				      int, bool *);
static int special_function_p (const_tree, int);
static bool check_sibcall_argument_overlap_1 (rtx);
static bool check_sibcall_argument_overlap (rtx_insn *, struct arg_data *,
					    bool);
static tree split_complex_types (tree);

#ifdef REG_PARM_STACK_SPACE
static rtx save_fixed_argument_area (int, rtx, int *, int *);
static void restore_fixed_argument_area (rtx, rtx, int, int);
#endif

/* Return true if bytes [LOWER_BOUND, UPPER_BOUND) of the outgoing
   stack region might already be in use.  */

static bool
stack_region_maybe_used_p (poly_uint64 lower_bound, poly_uint64 upper_bound,
			   unsigned int reg_parm_stack_space)
{
  unsigned HOST_WIDE_INT const_lower, const_upper;
  const_lower = constant_lower_bound (lower_bound);
  if (!upper_bound.is_constant (&const_upper))
    const_upper = HOST_WIDE_INT_M1U;

  if (const_upper > stack_usage_watermark)
    return true;

  /* Don't worry about things in the fixed argument area;
     it has already been saved.  */
  const_lower = MAX (const_lower, reg_parm_stack_space);
  const_upper = MIN (const_upper, highest_outgoing_arg_in_use);
  for (unsigned HOST_WIDE_INT i = const_lower; i < const_upper; ++i)
    if (stack_usage_map[i])
      return true;
  return false;
}

/* Record that bytes [LOWER_BOUND, UPPER_BOUND) of the outgoing
   stack region are now in use.  */

static void
mark_stack_region_used (poly_uint64 lower_bound, poly_uint64 upper_bound)
{
  unsigned HOST_WIDE_INT const_lower, const_upper;
  const_lower = constant_lower_bound (lower_bound);
  if (upper_bound.is_constant (&const_upper)
      && const_upper <= highest_outgoing_arg_in_use)
    for (unsigned HOST_WIDE_INT i = const_lower; i < const_upper; ++i)
      stack_usage_map[i] = 1;
  else
    stack_usage_watermark = MIN (stack_usage_watermark, const_lower);
}

/* Force FUNEXP into a form suitable for the address of a CALL,
   and return that as an rtx.  Also load the static chain register
   if FNDECL is a nested function.

   CALL_FUSAGE points to a variable holding the prospective
   CALL_INSN_FUNCTION_USAGE information.  */

rtx
prepare_call_address (tree fndecl_or_type, rtx funexp, rtx static_chain_value,
		      rtx *call_fusage, int reg_parm_seen, int flags)
{
  /* Make a valid memory address and copy constants through pseudo-regs,
     but not for a constant address if -fno-function-cse.  */
  if (GET_CODE (funexp) != SYMBOL_REF)
    {
      /* If it's an indirect call by descriptor, generate code to perform
	 runtime identification of the pointer and load the descriptor.  */
      if ((flags & ECF_BY_DESCRIPTOR) && !flag_trampolines)
	{
	  const int bit_val = targetm.calls.custom_function_descriptors;
	  rtx call_lab = gen_label_rtx ();

	  gcc_assert (fndecl_or_type && TYPE_P (fndecl_or_type));
	  fndecl_or_type
	    = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, NULL_TREE,
			  fndecl_or_type);
	  DECL_STATIC_CHAIN (fndecl_or_type) = 1;
	  rtx chain = targetm.calls.static_chain (fndecl_or_type, false);

	  if (GET_MODE (funexp) != Pmode)
	    funexp = convert_memory_address (Pmode, funexp);

	  /* Avoid long live ranges around function calls.  */
	  funexp = copy_to_mode_reg (Pmode, funexp);

	  if (REG_P (chain))
	    emit_insn (gen_rtx_CLOBBER (VOIDmode, chain));

	  /* Emit the runtime identification pattern.  */
	  rtx mask = gen_rtx_AND (Pmode, funexp, GEN_INT (bit_val));
	  emit_cmp_and_jump_insns (mask, const0_rtx, EQ, NULL_RTX, Pmode, 1,
				   call_lab);

	  /* Statically predict the branch to very likely taken.  */
	  rtx_insn *insn = get_last_insn ();
	  if (JUMP_P (insn))
	    predict_insn_def (insn, PRED_BUILTIN_EXPECT, TAKEN);

	  /* Load the descriptor.  */
	  rtx mem = gen_rtx_MEM (ptr_mode,
				 plus_constant (Pmode, funexp, - bit_val));
	  MEM_NOTRAP_P (mem) = 1;
	  mem = convert_memory_address (Pmode, mem);
	  emit_move_insn (chain, mem);

	  mem = gen_rtx_MEM (ptr_mode,
			     plus_constant (Pmode, funexp,
					    POINTER_SIZE / BITS_PER_UNIT
					      - bit_val));
	  MEM_NOTRAP_P (mem) = 1;
	  mem = convert_memory_address (Pmode, mem);
	  emit_move_insn (funexp, mem);

	  emit_label (call_lab);

	  if (REG_P (chain))
	    {
	      use_reg (call_fusage, chain);
	      STATIC_CHAIN_REG_P (chain) = 1;
	    }

	  /* Make sure we're not going to be overwritten below.  */
	  gcc_assert (!static_chain_value);
	}

      /* If we are using registers for parameters, force the
	 function address into a register now.  */
      funexp = ((reg_parm_seen
		 && targetm.small_register_classes_for_mode_p (FUNCTION_MODE))
		 ? force_not_mem (memory_address (FUNCTION_MODE, funexp))
		 : memory_address (FUNCTION_MODE, funexp));
    }
  else
    {
      /* funexp could be a SYMBOL_REF represents a function pointer which is
	 of ptr_mode.  In this case, it should be converted into address mode
	 to be a valid address for memory rtx pattern.  See PR 64971.  */
      if (GET_MODE (funexp) != Pmode)
	funexp = convert_memory_address (Pmode, funexp);

      if (!(flags & ECF_SIBCALL))
	{
	  if (!NO_FUNCTION_CSE && optimize && ! flag_no_function_cse)
	    funexp = force_reg (Pmode, funexp);
	}
    }

  if (static_chain_value != 0
      && (TREE_CODE (fndecl_or_type) != FUNCTION_DECL
	  || DECL_STATIC_CHAIN (fndecl_or_type)))
    {
      rtx chain;

      chain = targetm.calls.static_chain (fndecl_or_type, false);
      static_chain_value = convert_memory_address (Pmode, static_chain_value);

      emit_move_insn (chain, static_chain_value);
      if (REG_P (chain))
	{
	  use_reg (call_fusage, chain);
	  STATIC_CHAIN_REG_P (chain) = 1;
	}
    }

  return funexp;
}

/* Generate instructions to call function FUNEXP,
   and optionally pop the results.
   The CALL_INSN is the first insn generated.

   FNDECL is the declaration node of the function.  This is given to the
   hook TARGET_RETURN_POPS_ARGS to determine whether this function pops
   its own args.

   FUNTYPE is the data type of the function.  This is given to the hook
   TARGET_RETURN_POPS_ARGS to determine whether this function pops its
   own args.  We used to allow an identifier for library functions, but
   that doesn't work when the return type is an aggregate type and the
   calling convention says that the pointer to this aggregate is to be
   popped by the callee.

   STACK_SIZE is the number of bytes of arguments on the stack,
   ROUNDED_STACK_SIZE is that number rounded up to
   PREFERRED_STACK_BOUNDARY; zero if the size is variable.  This is
   both to put into the call insn and to generate explicit popping
   code if necessary.

   STRUCT_VALUE_SIZE is the number of bytes wanted in a structure value.
   It is zero if this call doesn't want a structure value.

   NEXT_ARG_REG is the rtx that results from executing
     targetm.calls.function_arg (&args_so_far,
				 function_arg_info::end_marker ());
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
emit_call_1 (rtx funexp, tree fntree ATTRIBUTE_UNUSED, tree fndecl ATTRIBUTE_UNUSED,
	     tree funtype ATTRIBUTE_UNUSED,
	     poly_int64 stack_size ATTRIBUTE_UNUSED,
	     poly_int64 rounded_stack_size,
	     poly_int64 struct_value_size ATTRIBUTE_UNUSED,
	     rtx next_arg_reg ATTRIBUTE_UNUSED, rtx valreg,
	     int old_inhibit_defer_pop, rtx call_fusage, int ecf_flags,
	     cumulative_args_t args_so_far ATTRIBUTE_UNUSED)
{
  rtx rounded_stack_size_rtx = gen_int_mode (rounded_stack_size, Pmode);
  rtx call, funmem, pat;
  bool already_popped = false;
  poly_int64 n_popped = 0;

  /* Sibling call patterns never pop arguments (no sibcall(_value)_pop
     patterns exist).  Any popping that the callee does on return will
     be from our caller's frame rather than ours.  */
  if (!(ecf_flags & ECF_SIBCALL))
    {
      n_popped += targetm.calls.return_pops_args (fndecl, funtype, stack_size);

#ifdef CALL_POPS_ARGS
      n_popped += CALL_POPS_ARGS (*get_cumulative_args (args_so_far));
#endif
    }

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (funexp) != SYMBOL_REF)
    funexp = memory_address (FUNCTION_MODE, funexp);

  funmem = gen_rtx_MEM (FUNCTION_MODE, funexp);
  if (fndecl && TREE_CODE (fndecl) == FUNCTION_DECL)
    {
      tree t = fndecl;

      /* Although a built-in FUNCTION_DECL and its non-__builtin
	 counterpart compare equal and get a shared mem_attrs, they
	 produce different dump output in compare-debug compilations,
	 if an entry gets garbage collected in one compilation, then
	 adds a different (but equivalent) entry, while the other
	 doesn't run the garbage collector at the same spot and then
	 shares the mem_attr with the equivalent entry. */
      if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL)
	{
	  tree t2 = builtin_decl_explicit (DECL_FUNCTION_CODE (t));
	  if (t2)
	    t = t2;
	}

	set_mem_expr (funmem, t);
    }
  else if (fntree)
    set_mem_expr (funmem, build_simple_mem_ref (CALL_EXPR_FN (fntree)));

  if (ecf_flags & ECF_SIBCALL)
    {
      if (valreg)
	pat = targetm.gen_sibcall_value (valreg, funmem,
					 rounded_stack_size_rtx,
					 next_arg_reg, NULL_RTX);
      else
	pat = targetm.gen_sibcall (funmem, rounded_stack_size_rtx,
				   next_arg_reg,
				   gen_int_mode (struct_value_size, Pmode));
    }
  /* If the target has "call" or "call_value" insns, then prefer them
     if no arguments are actually popped.  If the target does not have
     "call" or "call_value" insns, then we must use the popping versions
     even if the call has no arguments to pop.  */
  else if (maybe_ne (n_popped, 0)
	   || !(valreg
		? targetm.have_call_value ()
		: targetm.have_call ()))
    {
      rtx n_pop = gen_int_mode (n_popped, Pmode);

      /* If this subroutine pops its own args, record that in the call insn
	 if possible, for the sake of frame pointer elimination.  */

      if (valreg)
	pat = targetm.gen_call_value_pop (valreg, funmem,
					  rounded_stack_size_rtx,
					  next_arg_reg, n_pop);
      else
	pat = targetm.gen_call_pop (funmem, rounded_stack_size_rtx,
				    next_arg_reg, n_pop);

      already_popped = true;
    }
  else
    {
      if (valreg)
	pat = targetm.gen_call_value (valreg, funmem, rounded_stack_size_rtx,
				      next_arg_reg, NULL_RTX);
      else
	pat = targetm.gen_call (funmem, rounded_stack_size_rtx, next_arg_reg,
				gen_int_mode (struct_value_size, Pmode));
    }
  emit_insn (pat);

  /* Find the call we just emitted.  */
  rtx_call_insn *call_insn = last_call_insn ();

  /* Some target create a fresh MEM instead of reusing the one provided
     above.  Set its MEM_EXPR.  */
  call = get_call_rtx_from (call_insn);
  if (call
      && MEM_EXPR (XEXP (call, 0)) == NULL_TREE
      && MEM_EXPR (funmem) != NULL_TREE)
    set_mem_expr (XEXP (call, 0), MEM_EXPR (funmem));

  /* Put the register usage information there.  */
  add_function_usage_to (call_insn, call_fusage);

  /* If this is a const call, then set the insn's unchanging bit.  */
  if (ecf_flags & ECF_CONST)
    RTL_CONST_CALL_P (call_insn) = 1;

  /* If this is a pure call, then set the insn's unchanging bit.  */
  if (ecf_flags & ECF_PURE)
    RTL_PURE_CALL_P (call_insn) = 1;

  /* If this is a const call, then set the insn's unchanging bit.  */
  if (ecf_flags & ECF_LOOPING_CONST_OR_PURE)
    RTL_LOOPING_CONST_OR_PURE_CALL_P (call_insn) = 1;

  /* Create a nothrow REG_EH_REGION note, if needed.  */
  make_reg_eh_region_note (call_insn, ecf_flags, 0);

  if (ecf_flags & ECF_NORETURN)
    add_reg_note (call_insn, REG_NORETURN, const0_rtx);

  if (ecf_flags & ECF_RETURNS_TWICE)
    {
      add_reg_note (call_insn, REG_SETJMP, const0_rtx);
      cfun->calls_setjmp = 1;
    }

  SIBLING_CALL_P (call_insn) = ((ecf_flags & ECF_SIBCALL) != 0);

  /* Restore this now, so that we do defer pops for this call's args
     if the context of the call as a whole permits.  */
  inhibit_defer_pop = old_inhibit_defer_pop;

  if (maybe_ne (n_popped, 0))
    {
      if (!already_popped)
	CALL_INSN_FUNCTION_USAGE (call_insn)
	  = gen_rtx_EXPR_LIST (VOIDmode,
			       gen_rtx_CLOBBER (VOIDmode, stack_pointer_rtx),
			       CALL_INSN_FUNCTION_USAGE (call_insn));
      rounded_stack_size -= n_popped;
      rounded_stack_size_rtx = gen_int_mode (rounded_stack_size, Pmode);
      stack_pointer_delta -= n_popped;

      add_args_size_note (call_insn, stack_pointer_delta);

      /* If popup is needed, stack realign must use DRAP  */
      if (SUPPORTS_STACK_ALIGNMENT)
        crtl->need_drap = true;
    }
  /* For noreturn calls when not accumulating outgoing args force
     REG_ARGS_SIZE note to prevent crossjumping of calls with different
     args sizes.  */
  else if (!ACCUMULATE_OUTGOING_ARGS && (ecf_flags & ECF_NORETURN) != 0)
    add_args_size_note (call_insn, stack_pointer_delta);

  if (!ACCUMULATE_OUTGOING_ARGS)
    {
      /* If returning from the subroutine does not automatically pop the args,
	 we need an instruction to pop them sooner or later.
	 Perhaps do it now; perhaps just record how much space to pop later.

	 If returning from the subroutine does pop the args, indicate that the
	 stack pointer will be changed.  */

      if (maybe_ne (rounded_stack_size, 0))
	{
	  if (ecf_flags & ECF_NORETURN)
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
  else if (maybe_ne (n_popped, 0))
    anti_adjust_stack (gen_int_mode (n_popped, Pmode));
}

/* Determine if the function identified by FNDECL is one with
   special properties we wish to know about.  Modify FLAGS accordingly.

   For example, if the function might return more than one time (setjmp), then
   set ECF_RETURNS_TWICE.

   Set ECF_MAY_BE_ALLOCA for any memory allocation function that might allocate
   space from the stack such as alloca.  */

static int
special_function_p (const_tree fndecl, int flags)
{
  tree name_decl = DECL_NAME (fndecl);

  if (maybe_special_function_p (fndecl)
      && IDENTIFIER_LENGTH (name_decl) <= 11)
    {
      const char *name = IDENTIFIER_POINTER (name_decl);
      const char *tname = name;

      /* We assume that alloca will always be called by name.  It
	 makes no sense to pass it as a pointer-to-function to
	 anything that does not understand its behavior.  */
      if (IDENTIFIER_LENGTH (name_decl) == 6
	  && name[0] == 'a'
	  && ! strcmp (name, "alloca"))
	flags |= ECF_MAY_BE_ALLOCA;

      /* Disregard prefix _ or __.  */
      if (name[0] == '_')
	{
	  if (name[1] == '_')
	    tname += 2;
	  else
	    tname += 1;
	}

      /* ECF_RETURNS_TWICE is safe even for -ffreestanding.  */
      if (! strcmp (tname, "setjmp")
	  || ! strcmp (tname, "sigsetjmp")
	  || ! strcmp (name, "savectx")
	  || ! strcmp (name, "vfork")
	  || ! strcmp (name, "getcontext"))
	flags |= ECF_RETURNS_TWICE;
    }

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && ALLOCA_FUNCTION_CODE_P (DECL_FUNCTION_CODE (fndecl)))
    flags |= ECF_MAY_BE_ALLOCA;

  return flags;
}

/* Return fnspec for DECL.  */

static attr_fnspec
decl_fnspec (tree fndecl)
{
  tree attr;
  tree type = TREE_TYPE (fndecl);
  if (type)
    {
      attr = lookup_attribute ("fn spec", TYPE_ATTRIBUTES (type));
      if (attr)
	{
	  return TREE_VALUE (TREE_VALUE (attr));
	}
    }
  if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    return builtin_fnspec (fndecl);
  return "";
}

/* Similar to special_function_p; return a set of ERF_ flags for the
   function FNDECL.  */
static int
decl_return_flags (tree fndecl)
{
  attr_fnspec fnspec = decl_fnspec (fndecl);

  unsigned int arg;
  if (fnspec.returns_arg (&arg))
    return ERF_RETURNS_ARG | arg;

  if (fnspec.returns_noalias_p ())
    return ERF_NOALIAS;
  return 0;
}

/* Return true when FNDECL represents a call to setjmp.  */

bool
setjmp_call_p (const_tree fndecl)
{
  if (DECL_IS_RETURNS_TWICE (fndecl))
    return true;
  if (special_function_p (fndecl, 0) & ECF_RETURNS_TWICE)
    return true;

  return false;
}


/* Return true if STMT may be an alloca call.  */

bool
gimple_maybe_alloca_call_p (const gimple *stmt)
{
  tree fndecl;

  if (!is_gimple_call (stmt))
    return false;

  fndecl = gimple_call_fndecl (stmt);
  if (fndecl && (special_function_p (fndecl, 0) & ECF_MAY_BE_ALLOCA))
    return true;

  return false;
}

/* Return true if STMT is a builtin alloca call.  */

bool
gimple_alloca_call_p (const gimple *stmt)
{
  tree fndecl;

  if (!is_gimple_call (stmt))
    return false;

  fndecl = gimple_call_fndecl (stmt);
  if (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      CASE_BUILT_IN_ALLOCA:
	return gimple_call_num_args (stmt) > 0;
      default:
	break;
      }

  return false;
}

/* Return true when exp contains a builtin alloca call.  */

bool
alloca_call_p (const_tree exp)
{
  tree fndecl;
  if (TREE_CODE (exp) == CALL_EXPR
      && (fndecl = get_callee_fndecl (exp))
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      CASE_BUILT_IN_ALLOCA:
        return true;
      default:
	break;
      }

  return false;
}

/* Return TRUE if FNDECL is either a TM builtin or a TM cloned
   function.  Return FALSE otherwise.  */

static bool
is_tm_builtin (const_tree fndecl)
{
  if (fndecl == NULL)
    return false;

  if (decl_is_tm_clone (fndecl))
    return true;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_TM_COMMIT:
	case BUILT_IN_TM_COMMIT_EH:
	case BUILT_IN_TM_ABORT:
	case BUILT_IN_TM_IRREVOCABLE:
	case BUILT_IN_TM_GETTMCLONE_IRR:
	case BUILT_IN_TM_MEMCPY:
	case BUILT_IN_TM_MEMMOVE:
	case BUILT_IN_TM_MEMSET:
	CASE_BUILT_IN_TM_STORE (1):
	CASE_BUILT_IN_TM_STORE (2):
	CASE_BUILT_IN_TM_STORE (4):
	CASE_BUILT_IN_TM_STORE (8):
	CASE_BUILT_IN_TM_STORE (FLOAT):
	CASE_BUILT_IN_TM_STORE (DOUBLE):
	CASE_BUILT_IN_TM_STORE (LDOUBLE):
	CASE_BUILT_IN_TM_STORE (M64):
	CASE_BUILT_IN_TM_STORE (M128):
	CASE_BUILT_IN_TM_STORE (M256):
	CASE_BUILT_IN_TM_LOAD (1):
	CASE_BUILT_IN_TM_LOAD (2):
	CASE_BUILT_IN_TM_LOAD (4):
	CASE_BUILT_IN_TM_LOAD (8):
	CASE_BUILT_IN_TM_LOAD (FLOAT):
	CASE_BUILT_IN_TM_LOAD (DOUBLE):
	CASE_BUILT_IN_TM_LOAD (LDOUBLE):
	CASE_BUILT_IN_TM_LOAD (M64):
	CASE_BUILT_IN_TM_LOAD (M128):
	CASE_BUILT_IN_TM_LOAD (M256):
	case BUILT_IN_TM_LOG:
	case BUILT_IN_TM_LOG_1:
	case BUILT_IN_TM_LOG_2:
	case BUILT_IN_TM_LOG_4:
	case BUILT_IN_TM_LOG_8:
	case BUILT_IN_TM_LOG_FLOAT:
	case BUILT_IN_TM_LOG_DOUBLE:
	case BUILT_IN_TM_LOG_LDOUBLE:
	case BUILT_IN_TM_LOG_M64:
	case BUILT_IN_TM_LOG_M128:
	case BUILT_IN_TM_LOG_M256:
	  return true;
	default:
	  break;
	}
    }
  return false;
}

/* Detect flags (function attributes) from the function decl or type node.  */

int
flags_from_decl_or_type (const_tree exp)
{
  int flags = 0;

  if (DECL_P (exp))
    {
      /* The function exp may have the `malloc' attribute.  */
      if (DECL_IS_MALLOC (exp))
	flags |= ECF_MALLOC;

      /* The function exp may have the `returns_twice' attribute.  */
      if (DECL_IS_RETURNS_TWICE (exp))
	flags |= ECF_RETURNS_TWICE;

      /* Process the pure and const attributes.  */
      if (TREE_READONLY (exp))
	flags |= ECF_CONST;
      if (DECL_PURE_P (exp))
	flags |= ECF_PURE;
      if (DECL_LOOPING_CONST_OR_PURE_P (exp))
	flags |= ECF_LOOPING_CONST_OR_PURE;

      if (DECL_IS_NOVOPS (exp))
	flags |= ECF_NOVOPS;
      if (lookup_attribute ("leaf", DECL_ATTRIBUTES (exp)))
	flags |= ECF_LEAF;
      if (lookup_attribute ("cold", DECL_ATTRIBUTES (exp)))
	flags |= ECF_COLD;

      if (TREE_NOTHROW (exp))
	flags |= ECF_NOTHROW;

      if (flag_tm)
	{
	  if (is_tm_builtin (exp))
	    flags |= ECF_TM_BUILTIN;
	  else if ((flags & (ECF_CONST|ECF_NOVOPS)) != 0
		   || lookup_attribute ("transaction_pure",
					TYPE_ATTRIBUTES (TREE_TYPE (exp))))
	    flags |= ECF_TM_PURE;
	}

      if (lookup_attribute ("expected_throw", DECL_ATTRIBUTES (exp)))
	flags |= ECF_XTHROW;

      flags = special_function_p (exp, flags);

      if ((flags & ECF_CONST) == 0
	  && lookup_attribute ("unsequenced noptr",
			       TYPE_ATTRIBUTES (TREE_TYPE (exp))))
	{
	  /* [[unsequenced]] with no pointers in arguments is like
	     [[gnu::const]] without finite guarantee.  */
	  flags |= ECF_CONST;
	  if ((flags & ECF_PURE) == 0)
	    flags |= ECF_LOOPING_CONST_OR_PURE;
	}
      if ((flags & (ECF_CONST | ECF_PURE)) == 0
	  && lookup_attribute ("reproducible noptr",
			       TYPE_ATTRIBUTES (TREE_TYPE (exp))))
	/* [[reproducible]] with no pointers in arguments is like
	   [[gnu::pure]] without finite guarantee.  */
	flags |= ECF_PURE | ECF_LOOPING_CONST_OR_PURE;
    }
  else if (TYPE_P (exp))
    {
      if (TYPE_READONLY (exp))
	flags |= ECF_CONST;

      if (flag_tm
	  && ((flags & ECF_CONST) != 0
	      || lookup_attribute ("transaction_pure", TYPE_ATTRIBUTES (exp))))
	flags |= ECF_TM_PURE;

      if ((flags & ECF_CONST) == 0
	  && lookup_attribute ("unsequenced noptr", TYPE_ATTRIBUTES (exp)))
	/* [[unsequenced]] with no pointers in arguments is like
	   [[gnu::const]] without finite guarantee.  */
	flags |= ECF_CONST | ECF_LOOPING_CONST_OR_PURE;
      if ((flags & ECF_CONST) == 0
	  && lookup_attribute ("reproducible noptr", TYPE_ATTRIBUTES (exp)))
	/* [[reproducible]] with no pointers in arguments is like
	   [[gnu::pure]] without finite guarantee.  */
	flags |= ECF_PURE | ECF_LOOPING_CONST_OR_PURE;
    }
  else
    gcc_unreachable ();

  if (TREE_THIS_VOLATILE (exp))
    {
      flags |= ECF_NORETURN;
      if (flags & (ECF_CONST|ECF_PURE))
	flags |= ECF_LOOPING_CONST_OR_PURE;
    }

  return flags;
}

/* Detect flags from a CALL_EXPR.  */

int
call_expr_flags (const_tree t)
{
  int flags;
  tree decl = get_callee_fndecl (t);

  if (decl)
    flags = flags_from_decl_or_type (decl);
  else if (CALL_EXPR_FN (t) == NULL_TREE)
    flags = internal_fn_flags (CALL_EXPR_IFN (t));
  else
    {
      tree type = TREE_TYPE (CALL_EXPR_FN (t));
      if (type && TREE_CODE (type) == POINTER_TYPE)
	flags = flags_from_decl_or_type (TREE_TYPE (type));
      else
	flags = 0;
      if (CALL_EXPR_BY_DESCRIPTOR (t))
	flags |= ECF_BY_DESCRIPTOR;
    }

  return flags;
}

/* Return true if ARG should be passed by invisible reference.  */

bool
pass_by_reference (CUMULATIVE_ARGS *ca, function_arg_info arg)
{
  if (tree type = arg.type)
    {
      /* If this type contains non-trivial constructors, then it is
	 forbidden for the middle-end to create any new copies.  */
      if (TREE_ADDRESSABLE (type))
	return true;

      /* GCC post 3.4 passes *all* variable sized types by reference.  */
      if (!TYPE_SIZE (type) || !poly_int_tree_p (TYPE_SIZE (type)))
	return true;

      /* If a record type should be passed the same as its first (and only)
	 member, use the type and mode of that member.  */
      if (TREE_CODE (type) == RECORD_TYPE && TYPE_TRANSPARENT_AGGR (type))
	{
	  arg.type = TREE_TYPE (first_field (type));
	  arg.mode = TYPE_MODE (arg.type);
	}
    }

  return targetm.calls.pass_by_reference (pack_cumulative_args (ca), arg);
}

/* Return true if TYPE should be passed by reference when passed to
   the "..." arguments of a function.  */

bool
pass_va_arg_by_reference (tree type)
{
  return pass_by_reference (NULL, function_arg_info (type, /*named=*/false));
}

/* Decide whether ARG, which occurs in the state described by CA,
   should be passed by reference.  Return true if so and update
   ARG accordingly.  */

bool
apply_pass_by_reference_rules (CUMULATIVE_ARGS *ca, function_arg_info &arg)
{
  if (pass_by_reference (ca, arg))
    {
      arg.type = build_pointer_type (arg.type);
      arg.mode = TYPE_MODE (arg.type);
      arg.pass_by_reference = true;
      return true;
    }
  return false;
}

/* Return true if ARG, which is passed by reference, should be callee
   copied instead of caller copied.  */

bool
reference_callee_copied (CUMULATIVE_ARGS *ca, const function_arg_info &arg)
{
  if (arg.type && TREE_ADDRESSABLE (arg.type))
    return false;
  return targetm.calls.callee_copies (pack_cumulative_args (ca), arg);
}


/* Precompute all register parameters as described by ARGS, storing values
   into fields within the ARGS array.

   NUM_ACTUALS indicates the total number elements in the ARGS array.

   Set REG_PARM_SEEN if we encounter a register parameter.  */

static void
precompute_register_parameters (int num_actuals, struct arg_data *args,
				int *reg_parm_seen)
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
	    args[i].value = expand_normal (args[i].tree_value);
	    preserve_temp_slots (args[i].value);
	    pop_temp_slots ();
	  }

	/* If we are to promote the function arg to a wider mode,
	   do it now.  */

	machine_mode old_mode = TYPE_MODE (TREE_TYPE (args[i].tree_value));

	/* Some ABIs require scalar floating point modes to be returned
	   in a wider scalar integer mode.  We need to explicitly
	   reinterpret to an integer mode of the correct precision
	   before extending to the desired result.  */
	if (SCALAR_INT_MODE_P (args[i].mode)
	    && SCALAR_FLOAT_MODE_P (old_mode)
	    && known_gt (GET_MODE_SIZE (args[i].mode),
			 GET_MODE_SIZE (old_mode)))
	  args[i].value = convert_float_to_wider_int (args[i].mode, old_mode,
						      args[i].value);
	else if (args[i].mode != old_mode)
	  args[i].value = convert_modes (args[i].mode, old_mode,
					 args[i].value, args[i].unsignedp);

	/* If the value is a non-legitimate constant, force it into a
	   pseudo now.  TLS symbols sometimes need a call to resolve.  */
	if (CONSTANT_P (args[i].value)
	    && (!targetm.legitimate_constant_p (args[i].mode, args[i].value)
		|| targetm.precompute_tls_p (args[i].mode, args[i].value)))
	  args[i].value = force_reg (args[i].mode, args[i].value);

	/* If we're going to have to load the value by parts, pull the
	   parts into pseudos.  The part extraction process can involve
	   non-trivial computation.  */
	if (GET_CODE (args[i].reg) == PARALLEL)
	  {
	    tree type = TREE_TYPE (args[i].tree_value);
	    args[i].parallel_value
	      = emit_group_load_into_temps (args[i].reg, args[i].value,
					    type, int_size_in_bytes (type));
	  }

	/* If the value is expensive, and we are inside an appropriately
	   short loop, put the value into a pseudo and then put the pseudo
	   into the hard reg.

	   For small register classes, also do this if this call uses
	   register parameters.  This is to avoid reload conflicts while
	   loading the parameters registers.  */

	else if ((! (REG_P (args[i].value)
		     || (GET_CODE (args[i].value) == SUBREG
			 && REG_P (SUBREG_REG (args[i].value)))))
		 && args[i].mode != BLKmode
		 && (set_src_cost (args[i].value, args[i].mode,
				   optimize_insn_for_speed_p ())
		     > COSTS_N_INSNS (1))
		 && ((*reg_parm_seen
		      && targetm.small_register_classes_for_mode_p (args[i].mode))
		     || optimize))
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
  unsigned int low;
  unsigned int high;

  /* Compute the boundary of the area that needs to be saved, if any.  */
  high = reg_parm_stack_space;
  if (ARGS_GROW_DOWNWARD)
    high += 1;

  if (high > highest_outgoing_arg_in_use)
    high = highest_outgoing_arg_in_use;

  for (low = 0; low < high; low++)
    if (stack_usage_map[low] != 0 || low >= stack_usage_watermark)
      {
	int num_to_save;
	machine_mode save_mode;
	int delta;
	rtx addr;
	rtx stack_area;
	rtx save_area;

	while (stack_usage_map[--high] == 0)
	  ;

	*low_to_save = low;
	*high_to_save = high;

	num_to_save = high - low + 1;

	/* If we don't have the required alignment, must do this
	   in BLKmode.  */
	scalar_int_mode imode;
	if (int_mode_for_size (num_to_save * BITS_PER_UNIT, 1).exists (&imode)
	    && (low & (MIN (GET_MODE_SIZE (imode),
			    BIGGEST_ALIGNMENT / UNITS_PER_WORD) - 1)) == 0)
	  save_mode = imode;
	else
	  save_mode = BLKmode;

	if (ARGS_GROW_DOWNWARD)
	  delta = -high;
	else
	  delta = low;

	addr = plus_constant (Pmode, argblock, delta);
	stack_area = gen_rtx_MEM (save_mode, memory_address (save_mode, addr));

	set_mem_align (stack_area, PARM_BOUNDARY);
	if (save_mode == BLKmode)
	  {
	    save_area = assign_stack_temp (BLKmode, num_to_save);
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
  machine_mode save_mode = GET_MODE (save_area);
  int delta;
  rtx addr, stack_area;

  if (ARGS_GROW_DOWNWARD)
    delta = -high_to_save;
  else
    delta = low_to_save;

  addr = plus_constant (Pmode, argblock, delta);
  stack_area = gen_rtx_MEM (save_mode, memory_address (save_mode, addr));
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
	&& GET_CODE (args[i].reg) != PARALLEL
	&& args[i].mode == BLKmode
	&& MEM_P (args[i].value)
	&& (MEM_ALIGN (args[i].value)
	    < (unsigned int) MIN (BIGGEST_ALIGNMENT, BITS_PER_WORD)))
      {
	int bytes = int_size_in_bytes (TREE_TYPE (args[i].tree_value));
	int endian_correction = 0;

	if (args[i].partial)
	  {
	    gcc_assert (args[i].partial % UNITS_PER_WORD == 0);
	    args[i].n_aligned_regs = args[i].partial / UNITS_PER_WORD;
	  }
	else
	  {
	    args[i].n_aligned_regs
	      = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
	  }

	args[i].aligned_regs = XNEWVEC (rtx, args[i].n_aligned_regs);

	/* Structures smaller than a word are normally aligned to the
	   least significant byte.  On a BYTES_BIG_ENDIAN machine,
	   this means we must skip the empty high order bytes when
	   calculating the bit offset.  */
	if (bytes < UNITS_PER_WORD
#ifdef BLOCK_REG_PADDING
	    && (BLOCK_REG_PADDING (args[i].mode,
				   TREE_TYPE (args[i].tree_value), 1)
		== PAD_DOWNWARD)
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
				      word_mode, word_mode, false, NULL);

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
	    store_bit_field (reg, bitsize, endian_correction, 0, 0,
			     word_mode, word, false, false);
	  }
      }
}

/* Issue an error if CALL_EXPR was flagged as requiring
   tall-call optimization.  */

void
maybe_complain_about_tail_call (tree call_expr, const char *reason)
{
  gcc_assert (TREE_CODE (call_expr) == CALL_EXPR);
  if (!CALL_EXPR_MUST_TAIL_CALL (call_expr))
    return;

  error_at (EXPR_LOCATION (call_expr), "cannot tail-call: %s", reason);
  CALL_EXPR_MUST_TAIL_CALL (call_expr) = 0;
}

/* Fill in ARGS_SIZE and ARGS array based on the parameters found in
   CALL_EXPR EXP.

   NUM_ACTUALS is the total number of parameters.

   N_NAMED_ARGS is the total number of named arguments.

   STRUCT_VALUE_ADDR_VALUE is the implicit argument for a struct return
   value, or null.

   FNDECL is the tree code for the target of this call (if known)

   ARGS_SO_FAR holds state needed by the target to know where to place
   the next argument.

   REG_PARM_STACK_SPACE is the number of bytes of stack space reserved
   for arguments which are passed in registers.

   OLD_STACK_LEVEL is a pointer to an rtx which olds the old stack level
   and may be modified by this routine.

   OLD_PENDING_ADJ and FLAGS are pointers to integer flags which
   may be modified by this routine.

   MUST_PREALLOCATE is a pointer to bool which may be
   modified by this routine.

   MAY_TAILCALL is cleared if we encounter an invisible pass-by-reference
   that requires allocation of stack space.

   CALL_FROM_THUNK_P is true if this call is the jump from a thunk to
   the thunked-to function.  */

static void
initialize_argument_information (int num_actuals ATTRIBUTE_UNUSED,
				 struct arg_data *args,
				 struct args_size *args_size,
				 int n_named_args ATTRIBUTE_UNUSED,
				 tree exp, tree struct_value_addr_value,
				 tree fndecl, tree fntype,
				 cumulative_args_t args_so_far,
				 int reg_parm_stack_space,
				 rtx *old_stack_level,
				 poly_int64 *old_pending_adj,
				 bool *must_preallocate, int *ecf_flags,
				 bool *may_tailcall, bool call_from_thunk_p)
{
  CUMULATIVE_ARGS *args_so_far_pnt = get_cumulative_args (args_so_far);
  location_t loc = EXPR_LOCATION (exp);

  /* Count arg position in order args appear.  */
  int argpos;

  int i;

  args_size->constant = 0;
  args_size->var = 0;

  /* In this loop, we consider args in the order they are written.
     We fill up ARGS from the back.  */

  i = num_actuals - 1;
  {
    int j = i;
    call_expr_arg_iterator iter;
    tree arg;

    if (struct_value_addr_value)
      {
	args[j].tree_value = struct_value_addr_value;
	j--;
      }
    argpos = 0;
    FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
      {
	tree argtype = TREE_TYPE (arg);

	if (targetm.calls.split_complex_arg
	    && argtype
	    && TREE_CODE (argtype) == COMPLEX_TYPE
	    && targetm.calls.split_complex_arg (argtype))
	  {
	    tree subtype = TREE_TYPE (argtype);
	    args[j].tree_value = build1 (REALPART_EXPR, subtype, arg);
	    j--;
	    args[j].tree_value = build1 (IMAGPART_EXPR, subtype, arg);
	  }
	else
	  args[j].tree_value = arg;
	j--;
	argpos++;
      }
  }

  /* I counts args in order (to be) pushed; ARGPOS counts in order written.  */
  for (argpos = 0; argpos < num_actuals; i--, argpos++)
    {
      tree type = TREE_TYPE (args[i].tree_value);
      int unsignedp;

      /* Replace erroneous argument with constant zero.  */
      if (type == error_mark_node || !COMPLETE_TYPE_P (type))
	args[i].tree_value = integer_zero_node, type = integer_type_node;

      /* If TYPE is a transparent union or record, pass things the way
	 we would pass the first field of the union or record.  We have
	 already verified that the modes are the same.  */
      if (RECORD_OR_UNION_TYPE_P (type) && TYPE_TRANSPARENT_AGGR (type))
	type = TREE_TYPE (first_field (type));

      /* Decide where to pass this arg.

	 args[i].reg is nonzero if all or part is passed in registers.

	 args[i].partial is nonzero if part but not all is passed in registers,
	 and the exact value says how many bytes are passed in registers.

	 args[i].pass_on_stack is true if the argument must at least be
	 computed on the stack.  It may then be loaded back into registers
	 if args[i].reg is nonzero.

	 These decisions are driven by the FUNCTION_... macros and must agree
	 with those made by function.cc.  */

      /* See if this argument should be passed by invisible reference.  */
      function_arg_info arg (type, argpos < n_named_args);
      if (pass_by_reference (args_so_far_pnt, arg))
	{
	  const bool callee_copies
	    = reference_callee_copied (args_so_far_pnt, arg);
	  tree base;

	  /* If we're compiling a thunk, pass directly the address of an object
	     already in memory, instead of making a copy.  Likewise if we want
	     to make the copy in the callee instead of the caller.  */
	  if ((call_from_thunk_p || callee_copies)
	      && TREE_CODE (args[i].tree_value) != WITH_SIZE_EXPR
	      && ((base = get_base_address (args[i].tree_value)), true)
	      && TREE_CODE (base) != SSA_NAME
	      && (!DECL_P (base) || MEM_P (DECL_RTL (base))))
	    {
	      /* We may have turned the parameter value into an SSA name.
		 Go back to the original parameter so we can take the
		 address.  */
	      if (TREE_CODE (args[i].tree_value) == SSA_NAME)
		{
		  gcc_assert (SSA_NAME_IS_DEFAULT_DEF (args[i].tree_value));
		  args[i].tree_value = SSA_NAME_VAR (args[i].tree_value);
		  gcc_assert (TREE_CODE (args[i].tree_value) == PARM_DECL);
		}
	      /* Argument setup code may have copied the value to register.  We
		 revert that optimization now because the tail call code must
		 use the original location.  */
	      if (TREE_CODE (args[i].tree_value) == PARM_DECL
		  && !MEM_P (DECL_RTL (args[i].tree_value))
		  && DECL_INCOMING_RTL (args[i].tree_value)
		  && MEM_P (DECL_INCOMING_RTL (args[i].tree_value)))
		set_decl_rtl (args[i].tree_value,
			      DECL_INCOMING_RTL (args[i].tree_value));

	      mark_addressable (args[i].tree_value);

	      /* We can't use sibcalls if a callee-copied argument is
		 stored in the current function's frame.  */
	      if (!call_from_thunk_p && DECL_P (base) && !TREE_STATIC (base))
		{
		  *may_tailcall = false;
		  maybe_complain_about_tail_call (exp,
						  _("a callee-copied argument is"
						    " stored in the current"
						    " function's frame"));
		}

	      args[i].tree_value = build_fold_addr_expr_loc (loc,
							 args[i].tree_value);
	      type = TREE_TYPE (args[i].tree_value);

	      if (*ecf_flags & ECF_CONST)
		*ecf_flags &= ~(ECF_CONST | ECF_LOOPING_CONST_OR_PURE);
	    }
	  else
	    {
	      /* We make a copy of the object and pass the address to the
		 function being called.  */
	      rtx copy;

	      if (!COMPLETE_TYPE_P (type)
		  || TREE_CODE (TYPE_SIZE_UNIT (type)) != INTEGER_CST
		  || (flag_stack_check == GENERIC_STACK_CHECK
		      && compare_tree_int (TYPE_SIZE_UNIT (type),
					   STACK_CHECK_MAX_VAR_SIZE) > 0))
		{
		  /* This is a variable-sized object.  Make space on the stack
		     for it.  */
		  rtx size_rtx = expr_size (args[i].tree_value);

		  if (*old_stack_level == 0)
		    {
		      emit_stack_save (SAVE_BLOCK, old_stack_level);
		      *old_pending_adj = pending_stack_adjust;
		      pending_stack_adjust = 0;
		    }

		  /* We can pass TRUE as the 4th argument because we just
		     saved the stack pointer and will restore it right after
		     the call.  */
		  copy = allocate_dynamic_stack_space (size_rtx,
						       TYPE_ALIGN (type),
						       TYPE_ALIGN (type),
						       max_int_size_in_bytes
						       (type),
						       true);
		  copy = gen_rtx_MEM (BLKmode, copy);
		  set_mem_attributes (copy, type, 1);
		}
	      else
		copy = assign_temp (type, 1, 0);

	      store_expr (args[i].tree_value, copy, 0, false, false);

	      /* Just change the const function to pure and then let
		 the next test clear the pure based on
		 callee_copies.  */
	      if (*ecf_flags & ECF_CONST)
		{
		  *ecf_flags &= ~ECF_CONST;
		  *ecf_flags |= ECF_PURE;
		}

	      if (!callee_copies && *ecf_flags & ECF_PURE)
		*ecf_flags &= ~(ECF_PURE | ECF_LOOPING_CONST_OR_PURE);

	      args[i].tree_value
		= build_fold_addr_expr_loc (loc, make_tree (type, copy));
	      type = TREE_TYPE (args[i].tree_value);
	      *may_tailcall = false;
	      maybe_complain_about_tail_call (exp,
					      _("argument must be passed"
						" by copying"));
	    }
	  arg.pass_by_reference = true;
	}

      unsignedp = TYPE_UNSIGNED (type);
      arg.type = type;
      arg.mode
	= promote_function_mode (type, TYPE_MODE (type), &unsignedp,
				 fndecl ? TREE_TYPE (fndecl) : fntype, 0);

      args[i].unsignedp = unsignedp;
      args[i].mode = arg.mode;

      targetm.calls.warn_parameter_passing_abi (args_so_far, type);

      args[i].reg = targetm.calls.function_arg (args_so_far, arg);

      /* If this is a sibling call and the machine has register windows, the
	 register window has to be unwinded before calling the routine, so
	 arguments have to go into the incoming registers.  */
      if (targetm.calls.function_incoming_arg != targetm.calls.function_arg)
	args[i].tail_call_reg
	  = targetm.calls.function_incoming_arg (args_so_far, arg);
      else
	args[i].tail_call_reg = args[i].reg;

      if (args[i].reg)
	args[i].partial = targetm.calls.arg_partial_bytes (args_so_far, arg);

      args[i].pass_on_stack = targetm.calls.must_pass_in_stack (arg);

      /* If FUNCTION_ARG returned a (parallel [(expr_list (nil) ...) ...]),
	 it means that we are to pass this arg in the register(s) designated
	 by the PARALLEL, but also to pass it in the stack.  */
      if (args[i].reg && GET_CODE (args[i].reg) == PARALLEL
	  && XEXP (XVECEXP (args[i].reg, 0, 0), 0) == 0)
	args[i].pass_on_stack = true;

      /* If this is an addressable type, we must preallocate the stack
	 since we must evaluate the object into its final location.

	 If this is to be passed in both registers and the stack, it is simpler
	 to preallocate.  */
      if (TREE_ADDRESSABLE (type)
	  || (args[i].pass_on_stack && args[i].reg != 0))
	*must_preallocate = true;

      /* Compute the stack-size of this argument.  */
      if (args[i].reg == 0 || args[i].partial != 0
	       || reg_parm_stack_space > 0
	       || args[i].pass_on_stack)
	locate_and_pad_parm (arg.mode, type,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			     1,
#else
			     args[i].reg != 0,
#endif
			     reg_parm_stack_space,
			     args[i].pass_on_stack ? 0 : args[i].partial,
			     fndecl, args_size, &args[i].locate);
#ifdef BLOCK_REG_PADDING
      else
	/* The argument is passed entirely in registers.  See at which
	   end it should be padded.  */
	args[i].locate.where_pad =
	  BLOCK_REG_PADDING (arg.mode, type,
			     int_size_in_bytes (type) <= UNITS_PER_WORD);
#endif

      /* Update ARGS_SIZE, the total stack space for args so far.  */

      args_size->constant += args[i].locate.size.constant;
      if (args[i].locate.size.var)
	ADD_PARM_SIZE (*args_size, args[i].locate.size.var);

      /* Increment ARGS_SO_FAR, which has info about which arg-registers
	 have been used, etc.  */

      /* ??? Traditionally we've passed TYPE_MODE here, instead of the
	 promoted_mode used for function_arg above.  However, the
	 corresponding handling of incoming arguments in function.cc
	 does pass the promoted mode.  */
      arg.mode = TYPE_MODE (type);
      targetm.calls.function_arg_advance (args_so_far, arg);
    }
}

/* Update ARGS_SIZE to contain the total size for the argument block.
   Return the original constant component of the argument block's size.

   REG_PARM_STACK_SPACE holds the number of bytes of stack space reserved
   for arguments passed in registers.  */

static poly_int64
compute_argument_block_size (int reg_parm_stack_space,
			     struct args_size *args_size,
			     tree fndecl ATTRIBUTE_UNUSED,
			     tree fntype ATTRIBUTE_UNUSED,
			     int preferred_stack_boundary ATTRIBUTE_UNUSED)
{
  poly_int64 unadjusted_args_size = args_size->constant;

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
	  gcc_assert (multiple_p (stack_pointer_delta,
				  preferred_stack_boundary));
	  args_size->var = round_up (args_size->var, preferred_stack_boundary);
	}

      if (reg_parm_stack_space > 0)
	{
	  args_size->var
	    = size_binop (MAX_EXPR, args_size->var,
			  ssize_int (reg_parm_stack_space));

	  /* The area corresponding to register parameters is not to count in
	     the size of the block we need.  So make the adjustment.  */
	  if (! OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl))))
	    args_size->var
	      = size_binop (MINUS_EXPR, args_size->var,
			    ssize_int (reg_parm_stack_space));
	}
    }
  else
    {
      preferred_stack_boundary /= BITS_PER_UNIT;
      if (preferred_stack_boundary < 1)
	preferred_stack_boundary = 1;
      args_size->constant = (aligned_upper_bound (args_size->constant
						  + stack_pointer_delta,
						  preferred_stack_boundary)
			     - stack_pointer_delta);

      args_size->constant = upper_bound (args_size->constant,
					 reg_parm_stack_space);

      if (! OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl))))
	args_size->constant -= reg_parm_stack_space;
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
precompute_arguments (int num_actuals, struct arg_data *args)
{
  int i;

  /* If this is a libcall, then precompute all arguments so that we do not
     get extraneous instructions emitted as part of the libcall sequence.  */

  /* If we preallocated the stack space, and some arguments must be passed
     on the stack, then we must precompute any parameter which contains a
     function call which will store arguments on the stack.
     Otherwise, evaluating the parameter may clobber previous parameters
     which have already been stored into the stack.  (we have code to avoid
     such case by saving the outgoing stack arguments, but it results in
     worse code)  */
  if (!ACCUMULATE_OUTGOING_ARGS)
    return;

  for (i = 0; i < num_actuals; i++)
    {
      tree type;
      machine_mode mode;

      if (TREE_CODE (args[i].tree_value) != CALL_EXPR)
	continue;

      /* If this is an addressable type, we cannot pre-evaluate it.  */
      type = TREE_TYPE (args[i].tree_value);
      gcc_assert (!TREE_ADDRESSABLE (type));

      args[i].initial_value = args[i].value
	= expand_normal (args[i].tree_value);

      mode = TYPE_MODE (type);
      if (mode != args[i].mode)
	{
	  int unsignedp = args[i].unsignedp;
	  args[i].value
	    = convert_modes (args[i].mode, mode,
			     args[i].value, args[i].unsignedp);

	  /* CSE will replace this only if it contains args[i].value
	     pseudo, so convert it down to the declared mode using
	     a SUBREG.  */
	  if (REG_P (args[i].value)
	      && GET_MODE_CLASS (args[i].mode) == MODE_INT
	      && promote_mode (type, mode, &unsignedp) != args[i].mode)
	    {
	      args[i].initial_value
		= gen_lowpart_SUBREG (mode, args[i].value);
	      SUBREG_PROMOTED_VAR_P (args[i].initial_value) = 1;
	      SUBREG_PROMOTED_SET (args[i].initial_value, args[i].unsignedp);
	    }
	}
    }
}

/* Given the current state of MUST_PREALLOCATE and information about
   arguments to a function call in NUM_ACTUALS, ARGS and ARGS_SIZE,
   compute and return the final value for MUST_PREALLOCATE.  */

static bool
finalize_must_preallocate (bool must_preallocate, int num_actuals,
			   struct arg_data *args, struct args_size *args_size)
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
      bool partial_seen = false;
      poly_int64 copy_to_evaluate_size = 0;
      int i;

      for (i = 0; i < num_actuals && ! must_preallocate; i++)
	{
	  if (args[i].partial > 0 && ! args[i].pass_on_stack)
	    partial_seen = true;
	  else if (partial_seen && args[i].reg == 0)
	    must_preallocate = true;

	  if (TYPE_MODE (TREE_TYPE (args[i].tree_value)) == BLKmode
	      && (TREE_CODE (args[i].tree_value) == CALL_EXPR
		  || TREE_CODE (args[i].tree_value) == TARGET_EXPR
		  || TREE_CODE (args[i].tree_value) == COND_EXPR
		  || TREE_ADDRESSABLE (TREE_TYPE (args[i].tree_value))))
	    copy_to_evaluate_size
	      += int_size_in_bytes (TREE_TYPE (args[i].tree_value));
	}

      if (maybe_ne (args_size->constant, 0)
	  && maybe_ge (copy_to_evaluate_size * 2, args_size->constant))
	must_preallocate = true;
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
      int i;
      poly_int64 arg_offset = 0;

      if (GET_CODE (argblock) == PLUS)
	{
	  arg_reg = XEXP (argblock, 0);
	  arg_offset = rtx_to_poly_int64 (XEXP (argblock, 1));
	}

      for (i = 0; i < num_actuals; i++)
	{
	  rtx offset = ARGS_SIZE_RTX (args[i].locate.offset);
	  rtx slot_offset = ARGS_SIZE_RTX (args[i].locate.slot_offset);
	  rtx addr;
	  unsigned int align, boundary;
	  poly_uint64 units_on_stack = 0;
	  machine_mode partial_mode = VOIDmode;

	  /* Skip this parm if it will not be passed on the stack.  */
	  if (! args[i].pass_on_stack
	      && args[i].reg != 0
	      && args[i].partial == 0)
	    continue;

	  if (TYPE_EMPTY_P (TREE_TYPE (args[i].tree_value)))
	    continue;

	  addr = simplify_gen_binary (PLUS, Pmode, arg_reg, offset);
	  addr = plus_constant (Pmode, addr, arg_offset);

	  if (args[i].partial != 0)
	    {
	      /* Only part of the parameter is being passed on the stack.
		 Generate a simple memory reference of the correct size.  */
	      units_on_stack = args[i].locate.size.constant;
	      poly_uint64 bits_on_stack = units_on_stack * BITS_PER_UNIT;
	      partial_mode = int_mode_for_size (bits_on_stack, 1).else_blk ();
	      args[i].stack = gen_rtx_MEM (partial_mode, addr);
	      set_mem_size (args[i].stack, units_on_stack);
	    }
	  else
	    {
	      args[i].stack = gen_rtx_MEM (args[i].mode, addr);
	      set_mem_attributes (args[i].stack,
				  TREE_TYPE (args[i].tree_value), 1);
	    }
	  align = BITS_PER_UNIT;
	  boundary = args[i].locate.boundary;
	  poly_int64 offset_val;
	  if (args[i].locate.where_pad != PAD_DOWNWARD)
	    align = boundary;
	  else if (poly_int_rtx_p (offset, &offset_val))
	    {
	      align = least_bit_hwi (boundary);
	      unsigned int offset_align
		= known_alignment (offset_val) * BITS_PER_UNIT;
	      if (offset_align != 0)
		align = MIN (align, offset_align);
	    }
	  set_mem_align (args[i].stack, align);

	  addr = simplify_gen_binary (PLUS, Pmode, arg_reg, slot_offset);
	  addr = plus_constant (Pmode, addr, arg_offset);

	  if (args[i].partial != 0)
	    {
	      /* Only part of the parameter is being passed on the stack.
		 Generate a simple memory reference of the correct size.
	       */
	      args[i].stack_slot = gen_rtx_MEM (partial_mode, addr);
	      set_mem_size (args[i].stack_slot, units_on_stack);
	    }
	  else
	    {
	      args[i].stack_slot = gen_rtx_MEM (args[i].mode, addr);
	      set_mem_attributes (args[i].stack_slot,
				  TREE_TYPE (args[i].tree_value), 1);
	    }
	  set_mem_align (args[i].stack_slot, args[i].locate.boundary);

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
      if (!TREE_USED (fndecl) && fndecl != current_function_decl)
	TREE_USED (fndecl) = 1;

      /* Get a SYMBOL_REF rtx for the function address.  */
      funexp = XEXP (DECL_RTL (fndecl), 0);
    }
  else
    /* Generate an rtx (probably a pseudo-register) for the address.  */
    {
      push_temp_slots ();
      funexp = expand_normal (addr);
      pop_temp_slots ();	/* FUNEXP can't be BLKmode.  */
    }
  return funexp;
}

/* Return the static chain for this function, if any.  */

rtx
rtx_for_static_chain (const_tree fndecl_or_type, bool incoming_p)
{
  if (DECL_P (fndecl_or_type) && !DECL_STATIC_CHAIN (fndecl_or_type))
    return NULL;

  return targetm.calls.static_chain (fndecl_or_type, incoming_p);
}

/* Internal state for internal_arg_pointer_based_exp and its helpers.  */
static struct
{
  /* Last insn that has been scanned by internal_arg_pointer_based_exp_scan,
     or NULL_RTX if none has been scanned yet.  */
  rtx_insn *scan_start;
  /* Vector indexed by REGNO - FIRST_PSEUDO_REGISTER, recording if a pseudo is
     based on crtl->args.internal_arg_pointer.  The element is NULL_RTX if the
     pseudo isn't based on it, a CONST_INT offset if the pseudo is based on it
     with fixed offset, or PC if this is with variable or unknown offset.  */
  vec<rtx> cache;
} internal_arg_pointer_exp_state;

static rtx internal_arg_pointer_based_exp (const_rtx, bool);

/* Helper function for internal_arg_pointer_based_exp.  Scan insns in
   the tail call sequence, starting with first insn that hasn't been
   scanned yet, and note for each pseudo on the LHS whether it is based
   on crtl->args.internal_arg_pointer or not, and what offset from that
   that pointer it has.  */

static void
internal_arg_pointer_based_exp_scan (void)
{
  rtx_insn *insn, *scan_start = internal_arg_pointer_exp_state.scan_start;

  if (scan_start == NULL_RTX)
    insn = get_insns ();
  else
    insn = NEXT_INSN (scan_start);

  while (insn)
    {
      rtx set = single_set (insn);
      if (set && REG_P (SET_DEST (set)) && !HARD_REGISTER_P (SET_DEST (set)))
	{
	  rtx val = NULL_RTX;
	  unsigned int idx = REGNO (SET_DEST (set)) - FIRST_PSEUDO_REGISTER;
	  /* Punt on pseudos set multiple times.  */
	  if (idx < internal_arg_pointer_exp_state.cache.length ()
	      && (internal_arg_pointer_exp_state.cache[idx]
		  != NULL_RTX))
	    val = pc_rtx;
	  else
	    val = internal_arg_pointer_based_exp (SET_SRC (set), false);
	  if (val != NULL_RTX)
	    {
	      if (idx >= internal_arg_pointer_exp_state.cache.length ())
		internal_arg_pointer_exp_state.cache
		  .safe_grow_cleared (idx + 1, true);
	      internal_arg_pointer_exp_state.cache[idx] = val;
	    }
	}
      if (NEXT_INSN (insn) == NULL_RTX)
	scan_start = insn;
      insn = NEXT_INSN (insn);
    }

  internal_arg_pointer_exp_state.scan_start = scan_start;
}

/* Compute whether RTL is based on crtl->args.internal_arg_pointer.  Return
   NULL_RTX if RTL isn't based on it, a CONST_INT offset if RTL is based on
   it with fixed offset, or PC if this is with variable or unknown offset.
   TOPLEVEL is true if the function is invoked at the topmost level.  */

static rtx
internal_arg_pointer_based_exp (const_rtx rtl, bool toplevel)
{
  if (CONSTANT_P (rtl))
    return NULL_RTX;

  if (rtl == crtl->args.internal_arg_pointer)
    return const0_rtx;

  if (REG_P (rtl) && HARD_REGISTER_P (rtl))
    return NULL_RTX;

  poly_int64 offset;
  if (GET_CODE (rtl) == PLUS && poly_int_rtx_p (XEXP (rtl, 1), &offset))
    {
      rtx val = internal_arg_pointer_based_exp (XEXP (rtl, 0), toplevel);
      if (val == NULL_RTX || val == pc_rtx)
	return val;
      return plus_constant (Pmode, val, offset);
    }

  /* When called at the topmost level, scan pseudo assignments in between the
     last scanned instruction in the tail call sequence and the latest insn
     in that sequence.  */
  if (toplevel)
    internal_arg_pointer_based_exp_scan ();

  if (REG_P (rtl))
    {
      unsigned int idx = REGNO (rtl) - FIRST_PSEUDO_REGISTER;
      if (idx < internal_arg_pointer_exp_state.cache.length ())
	return internal_arg_pointer_exp_state.cache[idx];

      return NULL_RTX;
    }

  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, rtl, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x) && internal_arg_pointer_based_exp (x, false) != NULL_RTX)
	return pc_rtx;
      if (MEM_P (x))
	iter.skip_subrtxes ();
    }

  return NULL_RTX;
}

/* Return true if SIZE bytes starting from address ADDR might overlap an
   already-clobbered argument area.  This function is used to determine
   if we should give up a sibcall.  */

static bool
mem_might_overlap_already_clobbered_arg_p (rtx addr, poly_uint64 size)
{
  poly_int64 i;
  unsigned HOST_WIDE_INT start, end;
  rtx val;

  if (bitmap_empty_p (stored_args_map)
      && stored_args_watermark == HOST_WIDE_INT_M1U)
    return false;
  val = internal_arg_pointer_based_exp (addr, true);
  if (val == NULL_RTX)
    return false;
  else if (!poly_int_rtx_p (val, &i))
    return true;

  if (known_eq (size, 0U))
    return false;

  if (STACK_GROWS_DOWNWARD)
    i -= crtl->args.pretend_args_size;
  else
    i += crtl->args.pretend_args_size;

  if (ARGS_GROW_DOWNWARD)
    i = -i - size;

  /* We can ignore any references to the function's pretend args,
     which at this point would manifest as negative values of I.  */
  if (known_le (i, 0) && known_le (size, poly_uint64 (-i)))
    return false;

  start = maybe_lt (i, 0) ? 0 : constant_lower_bound (i);
  if (!(i + size).is_constant (&end))
    end = HOST_WIDE_INT_M1U;

  if (end > stored_args_watermark)
    return true;

  end = MIN (end, SBITMAP_SIZE (stored_args_map));
  for (unsigned HOST_WIDE_INT k = start; k < end; ++k)
    if (bitmap_bit_p (stored_args_map, k))
      return true;

  return false;
}

/* Do the register loads required for any wholly-register parms or any
   parms which are passed both on the stack and in a register.  Their
   expressions were already evaluated.

   Mark all register-parms as living through the call, putting these USE
   insns in the CALL_INSN_FUNCTION_USAGE field.

   When IS_SIBCALL, perform the check_sibcall_argument_overlap
   checking, setting *SIBCALL_FAILURE if appropriate.  */

static void
load_register_parameters (struct arg_data *args, int num_actuals,
			  rtx *call_fusage, int flags, int is_sibcall,
			  bool *sibcall_failure)
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
	  poly_int64 size = 0;
	  HOST_WIDE_INT const_size = 0;
	  rtx_insn *before_arg = get_last_insn ();
	  tree tree_value = args[i].tree_value;
	  tree type = TREE_TYPE (tree_value);
	  if (RECORD_OR_UNION_TYPE_P (type) && TYPE_TRANSPARENT_AGGR (type))
	    type = TREE_TYPE (first_field (type));
	  /* Set non-negative if we must move a word at a time, even if
	     just one word (e.g, partial == 4 && mode == DFmode).  Set
	     to -1 if we just use a normal move insn.  This value can be
	     zero if the argument is a zero size structure.  */
	  nregs = -1;
	  if (GET_CODE (reg) == PARALLEL)
	    ;
	  else if (partial)
	    {
	      gcc_assert (partial % UNITS_PER_WORD == 0);
	      nregs = partial / UNITS_PER_WORD;
	    }
	  else if (TYPE_MODE (type) == BLKmode)
	    {
	      /* Variable-sized parameters should be described by a
		 PARALLEL instead.  */
	      const_size = int_size_in_bytes (type);
	      gcc_assert (const_size >= 0);
	      nregs = (const_size + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
	      size = const_size;
	    }
	  else
	    size = GET_MODE_SIZE (args[i].mode);

	  /* Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */

	  if (GET_CODE (reg) == PARALLEL)
	    emit_group_move (reg, args[i].parallel_value);

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
	      if (args[i].locate.where_pad
		  == (BYTES_BIG_ENDIAN ? PAD_UPWARD : PAD_DOWNWARD))
		{
		  gcc_checking_assert (ordered_p (size, UNITS_PER_WORD));
		  if (maybe_lt (size, UNITS_PER_WORD))
		    {
		      rtx x;
		      poly_int64 shift
			= (UNITS_PER_WORD - size) * BITS_PER_UNIT;

		      /* Assigning REG here rather than a temp makes
			 CALL_FUSAGE report the whole reg as used.
			 Strictly speaking, the call only uses SIZE
			 bytes at the msb end, but it doesn't seem worth
			 generating rtl to say that.  */
		      reg = gen_rtx_REG (word_mode, REGNO (reg));
		      x = expand_shift (LSHIFT_EXPR, word_mode,
					reg, shift, reg, 1);
		      if (x != reg)
			emit_move_insn (reg, x);
		    }
		}
#endif
	    }

	  /* If we have pre-computed the values to put in the registers in
	     the case of non-aligned structures, copy them in now.  */

	  else if (args[i].n_aligned_regs != 0)
	    for (j = 0; j < args[i].n_aligned_regs; j++)
	      emit_move_insn (gen_rtx_REG (word_mode, REGNO (reg) + j),
			      args[i].aligned_regs[j]);

	  /* If we need a single register and the source is a constant
	     VAR_DECL with a simple constructor, expand that constructor
	     via a pseudo rather than read from (possibly misaligned)
	     memory.  PR middle-end/95126.  */
	  else if (nregs == 1
		   && partial == 0
		   && !args[i].pass_on_stack
		   && VAR_P (tree_value)
		   && TREE_READONLY (tree_value)
		   && !TREE_SIDE_EFFECTS (tree_value)
		   && immediate_const_ctor_p (DECL_INITIAL (tree_value)))
	    {
	      rtx target = gen_reg_rtx (word_mode);
	      store_constructor (DECL_INITIAL (tree_value), target, 0,
				 int_expr_size (DECL_INITIAL (tree_value)),
				 false);
	      reg = gen_rtx_REG (word_mode, REGNO (reg));
	      emit_move_insn (reg, target);
	    }
	  else if (partial == 0 || args[i].pass_on_stack)
	    {
	      /* SIZE and CONST_SIZE are 0 for partial arguments and
		 the size of a BLKmode type otherwise.  */
	      gcc_checking_assert (known_eq (size, const_size));
	      rtx mem = validize_mem (copy_rtx (args[i].value));

	      /* Check for overlap with already clobbered argument area,
	         providing that this has non-zero size.  */
	      if (is_sibcall
		  && const_size != 0
		  && (mem_might_overlap_already_clobbered_arg_p
		      (XEXP (args[i].value, 0), const_size)))
		*sibcall_failure = true;

	      if (const_size % UNITS_PER_WORD == 0
		  || MEM_ALIGN (mem) % BITS_PER_WORD == 0)
		move_block_to_reg (REGNO (reg), mem, nregs, args[i].mode);
	      else
		{
		  if (nregs > 1)
		    move_block_to_reg (REGNO (reg), mem, nregs - 1,
				       args[i].mode);
		  rtx dest = gen_rtx_REG (word_mode, REGNO (reg) + nregs - 1);
		  unsigned int bitoff = (nregs - 1) * BITS_PER_WORD;
		  unsigned int bitsize = const_size * BITS_PER_UNIT - bitoff;
		  rtx x = extract_bit_field (mem, bitsize, bitoff, 1, dest,
					     word_mode, word_mode, false,
					     NULL);
		  if (BYTES_BIG_ENDIAN)
		    x = expand_shift (LSHIFT_EXPR, word_mode, x,
				      BITS_PER_WORD - bitsize, dest, 1);
		  if (x != dest)
		    emit_move_insn (dest, x);
		}

	      /* Handle a BLKmode that needs shifting.  */
	      if (nregs == 1 && const_size < UNITS_PER_WORD
#ifdef BLOCK_REG_PADDING
		  && args[i].locate.where_pad == PAD_DOWNWARD
#else
		  && BYTES_BIG_ENDIAN
#endif
		  )
		{
		  rtx dest = gen_rtx_REG (word_mode, REGNO (reg));
		  int shift = (UNITS_PER_WORD - const_size) * BITS_PER_UNIT;
		  enum tree_code dir = (BYTES_BIG_ENDIAN
					? RSHIFT_EXPR : LSHIFT_EXPR);
		  rtx x;

		  x = expand_shift (dir, word_mode, dest, shift, dest, 1);
		  if (x != dest)
		    emit_move_insn (dest, x);
		}
	    }

	  /* When a parameter is a block, and perhaps in other cases, it is
	     possible that it did a load from an argument slot that was
	     already clobbered.  */
	  if (is_sibcall
	      && check_sibcall_argument_overlap (before_arg, &args[i], false))
	    *sibcall_failure = true;

	  /* Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */
	  if (GET_CODE (reg) == PARALLEL)
	    use_group_regs (call_fusage, reg);
	  else if (nregs == -1)
	    use_reg_mode (call_fusage, reg, TYPE_MODE (type));
	  else if (nregs > 0)
	    use_regs (call_fusage, REGNO (reg), nregs);
	}
    }
}

/* We need to pop PENDING_STACK_ADJUST bytes.  But, if the arguments
   wouldn't fill up an even multiple of PREFERRED_UNIT_STACK_BOUNDARY
   bytes, then we would need to push some additional bytes to pad the
   arguments.  So, we try to compute an adjust to the stack pointer for an
   amount that will leave the stack under-aligned by UNADJUSTED_ARGS_SIZE
   bytes.  Then, when the arguments are pushed the stack will be perfectly
   aligned.

   Return true if this optimization is possible, storing the adjustment
   in ADJUSTMENT_OUT and setting ARGS_SIZE->CONSTANT to the number of
   bytes that should be popped after the call.  */

static bool
combine_pending_stack_adjustment_and_call (poly_int64 *adjustment_out,
					   poly_int64 unadjusted_args_size,
					   struct args_size *args_size,
					   unsigned int preferred_unit_stack_boundary)
{
  /* The number of bytes to pop so that the stack will be
     under-aligned by UNADJUSTED_ARGS_SIZE bytes.  */
  poly_int64 adjustment;
  /* The alignment of the stack after the arguments are pushed, if we
     just pushed the arguments without adjust the stack here.  */
  unsigned HOST_WIDE_INT unadjusted_alignment;

  if (!known_misalignment (stack_pointer_delta + unadjusted_args_size,
			   preferred_unit_stack_boundary,
			   &unadjusted_alignment))
    return false;

  /* We want to get rid of as many of the PENDING_STACK_ADJUST bytes
     as possible -- leaving just enough left to cancel out the
     UNADJUSTED_ALIGNMENT.  In other words, we want to ensure that the
     PENDING_STACK_ADJUST is non-negative, and congruent to
     -UNADJUSTED_ALIGNMENT modulo the PREFERRED_UNIT_STACK_BOUNDARY.  */

  /* Begin by trying to pop all the bytes.  */
  unsigned HOST_WIDE_INT tmp_misalignment;
  if (!known_misalignment (pending_stack_adjust,
			   preferred_unit_stack_boundary,
			   &tmp_misalignment))
    return false;
  unadjusted_alignment -= tmp_misalignment;
  adjustment = pending_stack_adjust;
  /* Push enough additional bytes that the stack will be aligned
     after the arguments are pushed.  */
  if (preferred_unit_stack_boundary > 1 && unadjusted_alignment)
    adjustment -= preferred_unit_stack_boundary - unadjusted_alignment;

  /* We need to know whether the adjusted argument size
     (UNADJUSTED_ARGS_SIZE - ADJUSTMENT) constitutes an allocation
     or a deallocation.  */
  if (!ordered_p (adjustment, unadjusted_args_size))
    return false;

  /* Now, sets ARGS_SIZE->CONSTANT so that we pop the right number of
     bytes after the call.  The right number is the entire
     PENDING_STACK_ADJUST less our ADJUSTMENT plus the amount required
     by the arguments in the first place.  */
  args_size->constant
    = pending_stack_adjust - adjustment + unadjusted_args_size;

  *adjustment_out = adjustment;
  return true;
}

/* Scan X expression if it does not dereference any argument slots
   we already clobbered by tail call arguments (as noted in stored_args_map
   bitmap).
   Return true if X expression dereferences such argument slots,
   false otherwise.  */

static bool
check_sibcall_argument_overlap_1 (rtx x)
{
  RTX_CODE code;
  int i, j;
  const char *fmt;

  if (x == NULL_RTX)
    return false;

  code = GET_CODE (x);

  /* We need not check the operands of the CALL expression itself.  */
  if (code == CALL)
    return false;

  if (code == MEM)
    return (mem_might_overlap_already_clobbered_arg_p
	    (XEXP (x, 0), GET_MODE_SIZE (GET_MODE (x))));

  /* Scan all subexpressions.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	{
	  if (check_sibcall_argument_overlap_1 (XEXP (x, i)))
	    return true;
	}
      else if (*fmt == 'E')
	{
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (check_sibcall_argument_overlap_1 (XVECEXP (x, i, j)))
	      return true;
	}
    }
  return false;
}

/* Scan sequence after INSN if it does not dereference any argument slots
   we already clobbered by tail call arguments (as noted in stored_args_map
   bitmap).  If MARK_STORED_ARGS_MAP, add stack slots for ARG to
   stored_args_map bitmap afterwards (when ARG is a register
   MARK_STORED_ARGS_MAP should be false).  Return true if sequence after
   INSN dereferences such argument slots, false otherwise.  */

static bool
check_sibcall_argument_overlap (rtx_insn *insn, struct arg_data *arg,
				bool mark_stored_args_map)
{
  poly_uint64 low, high;
  unsigned HOST_WIDE_INT const_low, const_high;

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
      if (ARGS_GROW_DOWNWARD)
	low = -arg->locate.slot_offset.constant - arg->locate.size.constant;
      else
	low = arg->locate.slot_offset.constant;
      high = low + arg->locate.size.constant;

      const_low = constant_lower_bound (low);
      if (high.is_constant (&const_high))
	for (unsigned HOST_WIDE_INT i = const_low; i < const_high; ++i)
	  bitmap_set_bit (stored_args_map, i);
      else
	stored_args_watermark = MIN (stored_args_watermark, const_low);
    }
  return insn != NULL_RTX;
}

/* Given that a function returns a value of mode MODE at the most
   significant end of hard register VALUE, shift VALUE left or right
   as specified by LEFT_P.  Return true if some action was needed.  */

bool
shift_return_value (machine_mode mode, bool left_p, rtx value)
{
  gcc_assert (REG_P (value) && HARD_REGISTER_P (value));
  machine_mode value_mode = GET_MODE (value);
  poly_int64 shift = GET_MODE_BITSIZE (value_mode) - GET_MODE_BITSIZE (mode);

  if (known_eq (shift, 0))
    return false;

  /* Use ashr rather than lshr for right shifts.  This is for the benefit
     of the MIPS port, which requires SImode values to be sign-extended
     when stored in 64-bit registers.  */
  if (!force_expand_binop (value_mode, left_p ? ashl_optab : ashr_optab,
			   value, gen_int_shift_amount (value_mode, shift),
			   value, 1, OPTAB_WIDEN))
    gcc_unreachable ();
  return true;
}

/* If X is a likely-spilled register value, copy it to a pseudo
   register and return that register.  Return X otherwise.  */

static rtx
avoid_likely_spilled_reg (rtx x)
{
  rtx new_rtx;

  if (REG_P (x)
      && HARD_REGISTER_P (x)
      && targetm.class_likely_spilled_p (REGNO_REG_CLASS (REGNO (x))))
    {
      /* Make sure that we generate a REG rather than a CONCAT.
	 Moves into CONCATs can need nontrivial instructions,
	 and the whole point of this function is to avoid
	 using the hard register directly in such a situation.  */
      generating_concat_p = 0;
      new_rtx = gen_reg_rtx (GET_MODE (x));
      generating_concat_p = 1;
      emit_move_insn (new_rtx, x);
      return new_rtx;
    }
  return x;
}

/* Helper function for expand_call.
   Return false is EXP is not implementable as a sibling call.  */

static bool
can_implement_as_sibling_call_p (tree exp,
				 rtx structure_value_addr,
				 tree funtype,
				 tree fndecl,
				 int flags,
				 tree addr,
				 const args_size &args_size)
{
  if (!targetm.have_sibcall_epilogue ()
      && !targetm.emit_epilogue_for_sibcall)
    {
      maybe_complain_about_tail_call
	(exp,
	 _("machine description does not have"
	   " a sibcall_epilogue instruction pattern"));
      return false;
    }

  /* Doing sibling call optimization needs some work, since
     structure_value_addr can be allocated on the stack.
     It does not seem worth the effort since few optimizable
     sibling calls will return a structure.  */
  if (structure_value_addr != NULL_RTX)
    {
      maybe_complain_about_tail_call (exp, _("callee returns a structure"));
      return false;
    }

  /* Check whether the target is able to optimize the call
     into a sibcall.  */
  if (!targetm.function_ok_for_sibcall (fndecl, exp))
    {
      maybe_complain_about_tail_call (exp,
				      _("target is not able to optimize the"
					" call into a sibling call"));
      return false;
    }

  /* Functions that do not return exactly once may not be sibcall
     optimized.  */
  if (flags & ECF_RETURNS_TWICE)
    {
      maybe_complain_about_tail_call (exp, _("callee returns twice"));
      return false;
    }
  if (flags & ECF_NORETURN)
    {
      maybe_complain_about_tail_call (exp, _("callee does not return"));
      return false;
    }

  if (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (addr))))
    {
      maybe_complain_about_tail_call (exp, _("volatile function type"));
      return false;
    }

  /* __sanitizer_cov_trace_pc is supposed to inspect its return address
     to identify the caller, and therefore should not be tailcalled.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_SANITIZER_COV_TRACE_PC)
    {
      /* No need for maybe_complain_about_tail_call here:
	 the call is synthesized by the compiler.  */
      return false;
    }

  /* If the called function is nested in the current one, it might access
     some of the caller's arguments, but could clobber them beforehand if
     the argument areas are shared.  */
  if (fndecl && decl_function_context (fndecl) == current_function_decl)
    {
      maybe_complain_about_tail_call (exp, _("nested function"));
      return false;
    }

  /* If this function requires more stack slots than the current
     function, we cannot change it into a sibling call.
     crtl->args.pretend_args_size is not part of the
     stack allocated by our caller.  */
  if (maybe_gt (args_size.constant,
		crtl->args.size - crtl->args.pretend_args_size))
    {
      maybe_complain_about_tail_call (exp,
				      _("callee required more stack slots"
					" than the caller"));
      return false;
    }

  /* If the callee pops its own arguments, then it must pop exactly
     the same number of arguments as the current function.  */
  if (maybe_ne (targetm.calls.return_pops_args (fndecl, funtype,
						args_size.constant),
		targetm.calls.return_pops_args (current_function_decl,
						TREE_TYPE
						(current_function_decl),
						crtl->args.size)))
    {
      maybe_complain_about_tail_call (exp,
				      _("inconsistent number of"
					" popped arguments"));
      return false;
    }

  if (!lang_hooks.decls.ok_for_sibcall (fndecl))
    {
      maybe_complain_about_tail_call (exp, _("frontend does not support"
					     " sibling call"));
      return false;
    }

  /* All checks passed.  */
  return true;
}

/* Update stack alignment when the parameter is passed in the stack
   since the outgoing parameter requires extra alignment on the calling
   function side. */

static void
update_stack_alignment_for_call (struct locate_and_pad_arg_data *locate)
{
  if (crtl->stack_alignment_needed < locate->boundary)
    crtl->stack_alignment_needed = locate->boundary;
  if (crtl->preferred_stack_boundary < locate->boundary)
    crtl->preferred_stack_boundary = locate->boundary;
}

/* Generate all the code for a CALL_EXPR exp
   and return an rtx for its value.
   Store the value in TARGET (specified as an rtx) if convenient.
   If the value is stored in TARGET then TARGET is returned.
   If IGNORE is nonzero, then we ignore the value of the function call.  */

rtx
expand_call (tree exp, rtx target, int ignore)
{
  /* Nonzero if we are currently expanding a call.  */
  static int currently_expanding_call = 0;

  /* RTX for the function to be called.  */
  rtx funexp;
  /* Sequence of insns to perform a normal "call".  */
  rtx_insn *normal_call_insns = NULL;
  /* Sequence of insns to perform a tail "call".  */
  rtx_insn *tail_call_insns = NULL;
  /* Data type of the function.  */
  tree funtype;
  tree type_arg_types;
  tree rettype;
  /* Declaration of the function being called,
     or 0 if the function is computed (not known by name).  */
  tree fndecl = 0;
  /* The type of the function being called.  */
  tree fntype;
  bool try_tail_call = CALL_EXPR_TAILCALL (exp);
  /* tree-tailcall decided not to do tail calls. Error for the musttail case,
     unfortunately we don't know the reason so it's fairly vague.
     When tree-tailcall reported an error it already cleared the flag,
     so this shouldn't really happen unless the
     the musttail pass gave up walking before finding the call.  */
  if (!try_tail_call)
      maybe_complain_about_tail_call (exp, _("other reasons"));
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
  /* Holds the value of implicit argument for the struct value.  */
  tree structure_value_addr_value = NULL_TREE;
  /* Size of aggregate value wanted, or zero if none wanted
     or if we are using the non-reentrant PCC calling convention
     or expecting the value in registers.  */
  poly_int64 struct_value_size = 0;
  /* True if called function returns an aggregate in memory PCC style,
     by returning the address of where to find it.  */
  bool pcc_struct_value = false;
  rtx struct_value = 0;

  /* Number of actual parameters in this call, including struct value addr.  */
  int num_actuals;
  /* Number of named args.  Args after this are anonymous ones
     and they must all go on the stack.  */
  int n_named_args;
  /* Number of complex actual arguments that need to be split.  */
  int num_complex_actuals = 0;

  /* Vector of information about each argument.
     Arguments are numbered in the order they will be pushed,
     not the order they are written.  */
  struct arg_data *args;

  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  struct args_size adjusted_args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  poly_int64 unadjusted_args_size;
  /* Data on reg parms scanned so far.  */
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;
  /* Nonzero if a reg parm has been scanned.  */
  int reg_parm_seen;

  /* True if we must avoid push-insns in the args for this call.
     If stack space is allocated for register parameters, but not by the
     caller, then it is preallocated in the fixed part of the stack frame.
     So the entire argument block must then be preallocated (i.e., we
     ignore PUSH_ROUNDING in that case).  */
  bool must_preallocate = !targetm.calls.push_argument (0);

  /* Size of the stack reserved for parameter registers.  */
  int reg_parm_stack_space = 0;

  /* Address of space preallocated for stack parms
     (on machines that lack push insns), or 0 if space not preallocated.  */
  rtx argblock = 0;

  /* Mask of ECF_ and ERF_ flags.  */
  int flags = 0;
  int return_flags = 0;
#ifdef REG_PARM_STACK_SPACE
  /* Define the boundary of the register parm stack space that needs to be
     saved, if any.  */
  int low_to_save, high_to_save;
  rtx save_area = 0;		/* Place that it is saved */
#endif

  unsigned int initial_highest_arg_in_use = highest_outgoing_arg_in_use;
  char *initial_stack_usage_map = stack_usage_map;
  unsigned HOST_WIDE_INT initial_stack_usage_watermark = stack_usage_watermark;
  char *stack_usage_map_buf = NULL;

  poly_int64 old_stack_allocated;

  /* State variables to track stack modifications.  */
  rtx old_stack_level = 0;
  int old_stack_arg_under_construction = 0;
  poly_int64 old_pending_adj = 0;
  int old_inhibit_defer_pop = inhibit_defer_pop;

  /* Some stack pointer alterations we make are performed via
     allocate_dynamic_stack_space. This modifies the stack_pointer_delta,
     which we then also need to save/restore along the way.  */
  poly_int64 old_stack_pointer_delta = 0;

  rtx call_fusage;
  tree addr = CALL_EXPR_FN (exp);
  int i;
  /* The alignment of the stack, in bits.  */
  unsigned HOST_WIDE_INT preferred_stack_boundary;
  /* The alignment of the stack, in bytes.  */
  unsigned HOST_WIDE_INT preferred_unit_stack_boundary;
  /* The static chain value to use for this call.  */
  rtx static_chain_value;
  /* See if this is "nothrow" function call.  */
  if (TREE_NOTHROW (exp))
    flags |= ECF_NOTHROW;

  /* See if we can find a DECL-node for the actual function, and get the
     function attributes (flags) from the function decl or type node.  */
  fndecl = get_callee_fndecl (exp);
  if (fndecl)
    {
      fntype = TREE_TYPE (fndecl);
      flags |= flags_from_decl_or_type (fndecl);
      return_flags |= decl_return_flags (fndecl);
    }
  else
    {
      fntype = TREE_TYPE (TREE_TYPE (addr));
      flags |= flags_from_decl_or_type (fntype);
      if (CALL_EXPR_BY_DESCRIPTOR (exp))
	flags |= ECF_BY_DESCRIPTOR;
    }
  rettype = TREE_TYPE (exp);

  struct_value = targetm.calls.struct_value_rtx (fntype, 0);

  /* Warn if this value is an aggregate type,
     regardless of which calling convention we are using for it.  */
  if (AGGREGATE_TYPE_P (rettype))
    warning (OPT_Waggregate_return, "function call has aggregate value");

  /* If the result of a non looping pure or const function call is
     ignored (or void), and none of its arguments are volatile, we can
     avoid expanding the call and just evaluate the arguments for
     side-effects.  */
  if ((flags & (ECF_CONST | ECF_PURE))
      && (!(flags & ECF_LOOPING_CONST_OR_PURE))
      && (flags & ECF_NOTHROW)
      && (ignore || target == const0_rtx
	  || TYPE_MODE (rettype) == VOIDmode))
    {
      bool volatilep = false;
      tree arg;
      call_expr_arg_iterator iter;

      FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	if (TREE_THIS_VOLATILE (arg))
	  {
	    volatilep = true;
	    break;
	  }

      if (! volatilep)
	{
	  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	    expand_expr (arg, const0_rtx, VOIDmode, EXPAND_NORMAL);
	  return const0_rtx;
	}
    }

#ifdef REG_PARM_STACK_SPACE
  reg_parm_stack_space = REG_PARM_STACK_SPACE (!fndecl ? fntype : fndecl);
#endif

  if (! OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl)))
      && reg_parm_stack_space > 0 && targetm.calls.push_argument (0))
    must_preallocate = true;

  /* Set up a place to return a structure.  */

  /* Cater to broken compilers.  */
  if (aggregate_value_p (exp, fntype))
    {
      /* This call returns a big structure.  */
      flags &= ~(ECF_CONST | ECF_PURE | ECF_LOOPING_CONST_OR_PURE);

#ifdef PCC_STATIC_STRUCT_RETURN
      {
	pcc_struct_value = true;
      }
#else /* not PCC_STATIC_STRUCT_RETURN */
      {
	if (!poly_int_tree_p (TYPE_SIZE_UNIT (rettype), &struct_value_size))
	  struct_value_size = -1;

	/* Even if it is semantically safe to use the target as the return
	   slot, it may be not sufficiently aligned for the return type.  */
	if (CALL_EXPR_RETURN_SLOT_OPT (exp)
	    && target
	    && MEM_P (target)
	    /* If rettype is addressable, we may not create a temporary.
	       If target is properly aligned at runtime and the compiler
	       just doesn't know about it, it will work fine, otherwise it
	       will be UB.  */
	    && (TREE_ADDRESSABLE (rettype)
		|| !(MEM_ALIGN (target) < TYPE_ALIGN (rettype)
		     && targetm.slow_unaligned_access (TYPE_MODE (rettype),
						       MEM_ALIGN (target)))))
	  structure_value_addr = XEXP (target, 0);
	else
	  {
	    /* For variable-sized objects, we must be called with a target
	       specified.  If we were to allocate space on the stack here,
	       we would have no way of knowing when to free it.  */
	    rtx d = assign_temp (rettype, 1, 1);
	    structure_value_addr = XEXP (d, 0);
	    target = 0;
	  }
      }
#endif /* not PCC_STATIC_STRUCT_RETURN */
    }

  /* Figure out the amount to which the stack should be aligned.  */
  preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;
  if (fndecl)
    {
      struct cgraph_rtl_info *i = cgraph_node::rtl_info (fndecl);
      /* Without automatic stack alignment, we can't increase preferred
	 stack boundary.  With automatic stack alignment, it is
	 unnecessary since unless we can guarantee that all callers will
	 align the outgoing stack properly, callee has to align its
	 stack anyway.  */
      if (i
	  && i->preferred_incoming_stack_boundary
	  && i->preferred_incoming_stack_boundary < preferred_stack_boundary)
	preferred_stack_boundary = i->preferred_incoming_stack_boundary;
    }

  /* Operand 0 is a pointer-to-function; get the type of the function.  */
  funtype = TREE_TYPE (addr);
  gcc_assert (POINTER_TYPE_P (funtype));
  funtype = TREE_TYPE (funtype);

  /* Count whether there are actual complex arguments that need to be split
     into their real and imaginary parts.  Munge the type_arg_types
     appropriately here as well.  */
  if (targetm.calls.split_complex_arg)
    {
      call_expr_arg_iterator iter;
      tree arg;
      FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	{
	  tree type = TREE_TYPE (arg);
	  if (type && TREE_CODE (type) == COMPLEX_TYPE
	      && targetm.calls.split_complex_arg (type))
	    num_complex_actuals++;
	}
      type_arg_types = split_complex_types (TYPE_ARG_TYPES (funtype));
    }
  else
    type_arg_types = TYPE_ARG_TYPES (funtype);

  if (flags & ECF_MAY_BE_ALLOCA)
    cfun->calls_alloca = 1;

  /* If struct_value_rtx is 0, it means pass the address
     as if it were an extra parameter.  Put the argument expression
     in structure_value_addr_value.  */
  if (structure_value_addr && struct_value == 0)
    {
      /* If structure_value_addr is a REG other than
	 virtual_outgoing_args_rtx, we can use always use it.  If it
	 is not a REG, we must always copy it into a register.
	 If it is virtual_outgoing_args_rtx, we must copy it to another
	 register in some cases.  */
      rtx temp = (!REG_P (structure_value_addr)
		  || (ACCUMULATE_OUTGOING_ARGS
		      && stack_arg_under_construction
		      && structure_value_addr == virtual_outgoing_args_rtx)
		  ? copy_addr_to_reg (convert_memory_address
				      (Pmode, structure_value_addr))
		  : structure_value_addr);

      structure_value_addr_value =
	make_tree (build_pointer_type (TREE_TYPE (funtype)), temp);
      structure_value_addr_parm = 1;
    }

  /* Count the arguments and set NUM_ACTUALS.  */
  num_actuals
    = call_expr_nargs (exp) + num_complex_actuals + structure_value_addr_parm;

  /* Compute number of named args.
     First, do a raw count of the args for INIT_CUMULATIVE_ARGS.  */

  if (type_arg_types != 0)
    n_named_args
      = (list_length (type_arg_types)
	 /* Count the struct value address, if it is passed as a parm.  */
	 + structure_value_addr_parm);
  else if (TYPE_NO_NAMED_ARGS_STDARG_P (funtype))
    n_named_args = structure_value_addr_parm;
  else
    /* If we know nothing, treat all args as named.  */
    n_named_args = num_actuals;

  /* Start updating where the next arg would go.

     On some machines (such as the PA) indirect calls have a different
     calling convention than normal calls.  The fourth argument in
     INIT_CUMULATIVE_ARGS tells the backend if this is an indirect call
     or not.  */
  INIT_CUMULATIVE_ARGS (args_so_far_v, funtype, NULL_RTX, fndecl, n_named_args);
  args_so_far = pack_cumulative_args (&args_so_far_v);

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

  if ((type_arg_types != 0 || TYPE_NO_NAMED_ARGS_STDARG_P (funtype))
      && targetm.calls.strict_argument_naming (args_so_far))
    ;
  else if (type_arg_types != 0
	   && ! targetm.calls.pretend_outgoing_varargs_named (args_so_far))
    /* Don't include the last named arg.  */
    --n_named_args;
  else if (TYPE_NO_NAMED_ARGS_STDARG_P (funtype)
	   && ! targetm.calls.pretend_outgoing_varargs_named (args_so_far))
    n_named_args = 0;
  else
    /* Treat all args as named.  */
    n_named_args = num_actuals;

  /* Make a vector to hold all the information about each arg.  */
  args = XCNEWVEC (struct arg_data, num_actuals);

  /* Build up entries in the ARGS array, compute the size of the
     arguments into ARGS_SIZE, etc.  */
  initialize_argument_information (num_actuals, args, &args_size,
				   n_named_args, exp,
				   structure_value_addr_value, fndecl, fntype,
				   args_so_far, reg_parm_stack_space,
				   &old_stack_level, &old_pending_adj,
				   &must_preallocate, &flags,
				   &try_tail_call, CALL_FROM_THUNK_P (exp));

  if (args_size.var)
    must_preallocate = true;

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
	  || (!ACCUMULATE_OUTGOING_ARGS
	      && maybe_ne (args_size.constant, 0))))
    structure_value_addr = copy_to_reg (structure_value_addr);

  /* Tail calls can make things harder to debug, and we've traditionally
     pushed these optimizations into -O2.  Don't try if we're already
     expanding a call, as that means we're an argument.  Don't try if
     there's cleanups, as we know there's code to follow the call.  */
  if (currently_expanding_call++ != 0)
    {
      maybe_complain_about_tail_call (exp, _("inside another call"));
      try_tail_call = 0;
    }
  if (!flag_optimize_sibling_calls
	&& !CALL_FROM_THUNK_P (exp)
	&& !CALL_EXPR_MUST_TAIL_CALL (exp))
    try_tail_call = 0;
  if (args_size.var)
    {
      maybe_complain_about_tail_call (exp, _("variable size arguments"));
      try_tail_call = 0;
    }
  if (dbg_cnt (tail_call) == false)
    try_tail_call = 0;

  /* Workaround buggy C/C++ wrappers around Fortran routines with
     character(len=constant) arguments if the hidden string length arguments
     are passed on the stack; if the callers forget to pass those arguments,
     attempting to tail call in such routines leads to stack corruption.
     Avoid tail calls in functions where at least one such hidden string
     length argument is passed (partially or fully) on the stack in the
     caller and the callee needs to pass any arguments on the stack.
     See PR90329.  */
  if (try_tail_call && maybe_ne (args_size.constant, 0))
    for (tree arg = DECL_ARGUMENTS (current_function_decl);
	 arg; arg = DECL_CHAIN (arg))
      if (DECL_HIDDEN_STRING_LENGTH (arg) && DECL_INCOMING_RTL (arg))
	{
	  subrtx_iterator::array_type array;
	  FOR_EACH_SUBRTX (iter, array, DECL_INCOMING_RTL (arg), NONCONST)
	    if (MEM_P (*iter))
	      {
		try_tail_call = 0;
		maybe_complain_about_tail_call (exp,
				_("hidden string length argument passed on stack"));
		break;
	      }
	}

  /* If the user has marked the function as requiring tail-call
     optimization, attempt it.  */
  if (CALL_EXPR_MUST_TAIL_CALL (exp))
    try_tail_call = 1;

  /*  Rest of purposes for tail call optimizations to fail.  */
  if (try_tail_call)
    try_tail_call = can_implement_as_sibling_call_p (exp,
						     structure_value_addr,
						     funtype,
						     fndecl,
						     flags, addr, args_size);

  /* Check if caller and callee disagree in promotion of function
     return value.  */
  if (try_tail_call)
    {
      machine_mode caller_mode, caller_promoted_mode;
      machine_mode callee_mode, callee_promoted_mode;
      int caller_unsignedp, callee_unsignedp;
      tree caller_res = DECL_RESULT (current_function_decl);

      caller_unsignedp = TYPE_UNSIGNED (TREE_TYPE (caller_res));
      caller_mode = DECL_MODE (caller_res);
      callee_unsignedp = TYPE_UNSIGNED (TREE_TYPE (funtype));
      callee_mode = TYPE_MODE (TREE_TYPE (funtype));
      caller_promoted_mode
	= promote_function_mode (TREE_TYPE (caller_res), caller_mode,
				 &caller_unsignedp,
				 TREE_TYPE (current_function_decl), 1);
      callee_promoted_mode
	= promote_function_mode (TREE_TYPE (funtype), callee_mode,
				 &callee_unsignedp,
				 funtype, 1);
      if (caller_mode != VOIDmode
	  && (caller_promoted_mode != callee_promoted_mode
	      || ((caller_mode != caller_promoted_mode
		   || callee_mode != callee_promoted_mode)
		  && (caller_unsignedp != callee_unsignedp
		      || partial_subreg_p (caller_mode, callee_mode)))))
	{
	  try_tail_call = 0;
	  maybe_complain_about_tail_call (exp,
					  _("caller and callee disagree in"
					    " promotion of function"
					    " return value"));
	}
    }

  /* Ensure current function's preferred stack boundary is at least
     what we need.  Stack alignment may also increase preferred stack
     boundary.  */
  for (i = 0; i < num_actuals; i++)
    if (reg_parm_stack_space > 0
	|| args[i].reg == 0
	|| args[i].partial != 0
	|| args[i].pass_on_stack)
      update_stack_alignment_for_call (&args[i].locate);
  if (crtl->preferred_stack_boundary < preferred_stack_boundary)
    crtl->preferred_stack_boundary = preferred_stack_boundary;
  else
    preferred_stack_boundary = crtl->preferred_stack_boundary;

  preferred_unit_stack_boundary = preferred_stack_boundary / BITS_PER_UNIT;

  if (flag_callgraph_info)
    record_final_call (fndecl, EXPR_LOCATION (exp));

  /* We want to make two insn chains; one for a sibling call, the other
     for a normal call.  We will select one of the two chains after
     initial RTL generation is complete.  */
  for (pass = try_tail_call ? 0 : 1; pass < 2; pass++)
    {
      bool sibcall_failure = false;
      bool normal_failure = false;
      /* We want to emit any pending stack adjustments before the tail
	 recursion "call".  That way we know any adjustment after the tail
	 recursion call can be ignored if we indeed use the tail
	 call expansion.  */
      saved_pending_stack_adjust save;
      rtx_insn *insns, *before_call, *after_args;
      rtx next_arg_reg;

      if (pass == 0)
	{
	  /* State variables we need to save and restore between
	     iterations.  */
	  save_pending_stack_adjust (&save);
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

      /* Don't let pending stack adjusts add up to too much.
	 Also, do all pending adjustments now if there is any chance
	 this might be a call to alloca or if we are expanding a sibling
	 call sequence.
	 Also do the adjustments before a throwing call, otherwise
	 exception handling can fail; PR 19225. */
      if (maybe_ge (pending_stack_adjust, 32)
	  || (maybe_ne (pending_stack_adjust, 0)
	      && (flags & ECF_MAY_BE_ALLOCA))
	  || (maybe_ne (pending_stack_adjust, 0)
	      && flag_exceptions && !(flags & ECF_NOTHROW))
	  || pass == 0)
	do_pending_stack_adjust ();

      /* Precompute any arguments as needed.  */
      if (pass)
	precompute_arguments (num_actuals, args);

      /* Now we are about to start emitting insns that can be deleted
	 if a libcall is deleted.  */
      if (pass && (flags & ECF_MALLOC))
	start_sequence ();

      /* Check the canary value for sibcall or function which doesn't
	 return and could throw.  */
      if ((pass == 0
	   || ((flags & ECF_NORETURN) != 0 && tree_could_throw_p (exp)))
	  && crtl->stack_protect_guard
	  && targetm.stack_protect_runtime_enabled_p ())
	stack_protect_epilogue ();

      adjusted_args_size = args_size;
      /* Compute the actual size of the argument block required.  The variable
	 and constant sizes must be combined, the size may have to be rounded,
	 and there may be a minimum required size.  When generating a sibcall
	 pattern, do not round up, since we'll be re-using whatever space our
	 caller provided.  */
      unadjusted_args_size
	= compute_argument_block_size (reg_parm_stack_space,
				       &adjusted_args_size,
				       fndecl, fntype,
				       (pass == 0 ? 0
					: preferred_stack_boundary));

      old_stack_allocated = stack_pointer_delta - pending_stack_adjust;

      /* The argument block when performing a sibling call is the
	 incoming argument block.  */
      if (pass == 0)
	{
	  argblock = crtl->args.internal_arg_pointer;
	  if (STACK_GROWS_DOWNWARD)
	    argblock
	      = plus_constant (Pmode, argblock, crtl->args.pretend_args_size);
	  else
	    argblock
	      = plus_constant (Pmode, argblock, -crtl->args.pretend_args_size);

	  HOST_WIDE_INT map_size = constant_lower_bound (args_size.constant);
	  stored_args_map = sbitmap_alloc (map_size);
	  bitmap_clear (stored_args_map);
	  stored_args_watermark = HOST_WIDE_INT_M1U;
	}

      /* If we have no actual push instructions, or shouldn't use them,
	 make space for all args right now.  */
      else if (adjusted_args_size.var != 0)
	{
	  if (old_stack_level == 0)
	    {
	      emit_stack_save (SAVE_BLOCK, &old_stack_level);
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
	  if (flag_stack_usage_info)
	    current_function_has_unbounded_dynamic_stack_size = 1;
	}
      else
	{
	  /* Note that we must go through the motions of allocating an argument
	     block even if the size is zero because we may be storing args
	     in the area reserved for register arguments, which may be part of
	     the stack frame.  */

	  poly_int64 needed = adjusted_args_size.constant;

	  /* Store the maximum argument space used.  It will be pushed by
	     the prologue (if ACCUMULATE_OUTGOING_ARGS, or stack overflow
	     checking).  */

	  crtl->outgoing_args_size = upper_bound (crtl->outgoing_args_size,
						  needed);

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

		  /* Since we will be writing into the entire argument area,
		     the map must be allocated for its entire size, not just
		     the part that is the responsibility of the caller.  */
		  if (! OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl))))
		    needed += reg_parm_stack_space;

		  poly_int64 limit = needed;
		  if (ARGS_GROW_DOWNWARD)
		    limit += 1;

		  /* For polynomial sizes, this is the maximum possible
		     size needed for arguments with a constant size
		     and offset.  */
		  HOST_WIDE_INT const_limit = constant_lower_bound (limit);
		  highest_outgoing_arg_in_use
		    = MAX (initial_highest_arg_in_use, const_limit);

		  free (stack_usage_map_buf);
		  stack_usage_map_buf = XNEWVEC (char, highest_outgoing_arg_in_use);
		  stack_usage_map = stack_usage_map_buf;

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
		  /* Try to reuse some or all of the pending_stack_adjust
		     to get this space.  */
		  if (inhibit_defer_pop == 0
		      && (combine_pending_stack_adjustment_and_call
			  (&needed,
			   unadjusted_args_size,
			   &adjusted_args_size,
			   preferred_unit_stack_boundary)))
		    {
		      /* combine_pending_stack_adjustment_and_call computes
			 an adjustment before the arguments are allocated.
			 Account for them and see whether or not the stack
			 needs to go up or down.  */
		      needed = unadjusted_args_size - needed;

		      /* Checked by
			 combine_pending_stack_adjustment_and_call.  */
		      gcc_checking_assert (ordered_p (needed, 0));
		      if (maybe_lt (needed, 0))
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
		  if (known_eq (needed, 0))
		    argblock = virtual_outgoing_args_rtx;
		  else
		    {
		      rtx needed_rtx = gen_int_mode (needed, Pmode);
		      argblock = push_block (needed_rtx, 0, 0);
		      if (ARGS_GROW_DOWNWARD)
			argblock = plus_constant (Pmode, argblock, needed);
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
	      rtx push_size
		= (gen_int_mode
		   (adjusted_args_size.constant
		    + (OUTGOING_REG_PARM_STACK_SPACE (!fndecl ? fntype
						      : TREE_TYPE (fndecl))
		       ? 0 : reg_parm_stack_space), Pmode));
	      if (old_stack_level == 0)
		{
		  emit_stack_save (SAVE_BLOCK, &old_stack_level);
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
		  free (stack_usage_map_buf);
		  stack_usage_map_buf = XCNEWVEC (char, highest_outgoing_arg_in_use);
		  stack_usage_map = stack_usage_map_buf;
		  highest_outgoing_arg_in_use = 0;
		  stack_usage_watermark = HOST_WIDE_INT_M1U;
		}
	      /* We can pass TRUE as the 4th argument because we just
		 saved the stack pointer and will restore it right after
		 the call.  */
	      allocate_dynamic_stack_space (push_size, 0, BIGGEST_ALIGNMENT,
					    -1, true);
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

      /* Stack is properly aligned, pops can't safely be deferred during
	 the evaluation of the arguments.  */
      NO_DEFER_POP;

      /* Precompute all register parameters.  It isn't safe to compute
	 anything once we have started filling any specific hard regs.
	 TLS symbols sometimes need a call to resolve.  Precompute
	 register parameters before any stack pointer manipulation
	 to avoid unaligned stack in the called function.  */
      precompute_register_parameters (num_actuals, args, &reg_parm_seen);

      OK_DEFER_POP;

      /* Perform stack alignment before the first push (the last arg).  */
      if (argblock == 0
	  && maybe_gt (adjusted_args_size.constant, reg_parm_stack_space)
	  && maybe_ne (adjusted_args_size.constant, unadjusted_args_size))
	{
	  /* When the stack adjustment is pending, we get better code
	     by combining the adjustments.  */
	  if (maybe_ne (pending_stack_adjust, 0)
	      && ! inhibit_defer_pop
	      && (combine_pending_stack_adjustment_and_call
		  (&pending_stack_adjust,
		   unadjusted_args_size,
		   &adjusted_args_size,
		   preferred_unit_stack_boundary)))
	    do_pending_stack_adjust ();
	  else if (argblock == 0)
	    anti_adjust_stack (gen_int_mode (adjusted_args_size.constant
					     - unadjusted_args_size,
					     Pmode));
	}
      /* Now that the stack is properly aligned, pops can't safely
	 be deferred during the evaluation of the arguments.  */
      NO_DEFER_POP;

      /* Record the maximum pushed stack space size.  We need to delay
	 doing it this far to take into account the optimization done
	 by combine_pending_stack_adjustment_and_call.  */
      if (flag_stack_usage_info
	  && !ACCUMULATE_OUTGOING_ARGS
	  && pass
	  && adjusted_args_size.var == 0)
	{
	  poly_int64 pushed = (adjusted_args_size.constant
			       + pending_stack_adjust);
	  current_function_pushed_stack_size
	    = upper_bound (current_function_pushed_stack_size, pushed);
	}

      funexp = rtx_for_function_call (fndecl, addr);

      if (CALL_EXPR_STATIC_CHAIN (exp))
	static_chain_value = expand_normal (CALL_EXPR_STATIC_CHAIN (exp));
      else
	static_chain_value = 0;

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
	{
	  if (args[i].reg == 0 || args[i].pass_on_stack)
	    {
	      rtx_insn *before_arg = get_last_insn ();

	      /* We don't allow passing huge (> 2^30 B) arguments
	         by value.  It would cause an overflow later on.  */
	      if (constant_lower_bound (adjusted_args_size.constant)
		  >= (1 << (HOST_BITS_PER_INT - 2)))
	        {
	          sorry ("passing too large argument on stack");
		  /* Don't worry about stack clean-up.  */
		  if (pass == 0)
		    sibcall_failure = true;
		  else
		    normal_failure = true;
		  continue;
		}

	      if (store_one_arg (&args[i], argblock, flags,
				 adjusted_args_size.var != 0,
				 reg_parm_stack_space)
		  || (pass == 0
		      && check_sibcall_argument_overlap (before_arg,
							 &args[i], true)))
		sibcall_failure = true;
	      }

	  if (args[i].stack)
	    call_fusage
	      = gen_rtx_EXPR_LIST (TYPE_MODE (TREE_TYPE (args[i].tree_value)),
				   gen_rtx_USE (VOIDmode, args[i].stack),
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
	      rtx_insn *before_arg = get_last_insn ();

	     /* On targets with weird calling conventions (e.g. PA) it's
		hard to ensure that all cases of argument overlap between
		stack and registers work.  Play it safe and bail out.  */
	      if (ARGS_GROW_DOWNWARD && !STACK_GROWS_DOWNWARD)
		{
		  sibcall_failure = true;
		  break;
		}

	      if (store_one_arg (&args[i], argblock, flags,
				 adjusted_args_size.var != 0,
				 reg_parm_stack_space)
		  || (pass == 0
		      && check_sibcall_argument_overlap (before_arg,
							 &args[i], true)))
		sibcall_failure = true;
	    }

      /* Set up the next argument register.  For sibling calls on machines
	 with register windows this should be the incoming register.  */
      if (pass == 0)
	next_arg_reg = targetm.calls.function_incoming_arg
	  (args_so_far, function_arg_info::end_marker ());
      else
	next_arg_reg = targetm.calls.function_arg
	  (args_so_far, function_arg_info::end_marker ());

      targetm.calls.start_call_args (args_so_far);

      bool any_regs = false;
      for (i = 0; i < num_actuals; i++)
	if (args[i].reg != NULL_RTX)
	  {
	    any_regs = true;
	    targetm.calls.call_args (args_so_far, args[i].reg, funtype);
	  }
      if (!any_regs)
	targetm.calls.call_args (args_so_far, pc_rtx, funtype);

      /* Figure out the register where the value, if any, will come back.  */
      valreg = 0;
      if (TYPE_MODE (rettype) != VOIDmode
	  && ! structure_value_addr)
	{
	  if (pcc_struct_value)
	    valreg = hard_function_value (build_pointer_type (rettype),
					  fndecl, NULL, (pass == 0));
	  else
	    valreg = hard_function_value (rettype, fndecl, fntype,
					  (pass == 0));

	  /* If VALREG is a PARALLEL whose first member has a zero
	     offset, use that.  This is for targets such as m68k that
	     return the same value in multiple places.  */
	  if (GET_CODE (valreg) == PARALLEL)
	    {
	      rtx elem = XVECEXP (valreg, 0, 0);
	      rtx where = XEXP (elem, 0);
	      rtx offset = XEXP (elem, 1);
	      if (offset == const0_rtx
		  && GET_MODE (where) == GET_MODE (valreg))
		valreg = where;
	    }
	}

      /* If register arguments require space on the stack and stack space
	 was not preallocated, allocate stack space here for arguments
	 passed in registers.  */
      if (OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl)))
          && !ACCUMULATE_OUTGOING_ARGS
	  && !must_preallocate && reg_parm_stack_space > 0)
	anti_adjust_stack (GEN_INT (reg_parm_stack_space));

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

	  if (REG_P (struct_value))
	    use_reg (&call_fusage, struct_value);
	}

      after_args = get_last_insn ();
      funexp = prepare_call_address (fndecl ? fndecl : fntype, funexp,
				     static_chain_value, &call_fusage,
				     reg_parm_seen, flags);

      load_register_parameters (args, num_actuals, &call_fusage, flags,
				pass == 0, &sibcall_failure);

      /* Save a pointer to the last insn before the call, so that we can
	 later safely search backwards to find the CALL_INSN.  */
      before_call = get_last_insn ();

      if (pass == 1 && (return_flags & ERF_RETURNS_ARG))
	{
	  int arg_nr = return_flags & ERF_RETURN_ARG_MASK;
	  arg_nr = num_actuals - arg_nr - 1;
	  if (arg_nr >= 0
	      && arg_nr < num_actuals
	      && args[arg_nr].reg
	      && valreg
	      && REG_P (valreg)
	      && GET_MODE (args[arg_nr].reg) == GET_MODE (valreg))
	  call_fusage
	    = gen_rtx_EXPR_LIST (TYPE_MODE (TREE_TYPE (args[arg_nr].tree_value)),
				 gen_rtx_SET (valreg, args[arg_nr].reg),
				 call_fusage);
	}
      /* All arguments and registers used for the call must be set up by
	 now!  */

      /* Stack must be properly aligned now.  */
      gcc_assert (!pass
		  || multiple_p (stack_pointer_delta,
				 preferred_unit_stack_boundary));

      /* Generate the actual call instruction.  */
      emit_call_1 (funexp, exp, fndecl, funtype, unadjusted_args_size,
		   adjusted_args_size.constant, struct_value_size,
		   next_arg_reg, valreg, old_inhibit_defer_pop, call_fusage,
		   flags, args_so_far);

      if (flag_ipa_ra)
	{
	  rtx_call_insn *last;
	  rtx datum = NULL_RTX;
	  if (fndecl != NULL_TREE)
	    {
	      datum = XEXP (DECL_RTL (fndecl), 0);
	      gcc_assert (datum != NULL_RTX
			  && GET_CODE (datum) == SYMBOL_REF);
	    }
	  last = last_call_insn ();
	  add_reg_note (last, REG_CALL_DECL, datum);
	}

      /* If the call setup or the call itself overlaps with anything
	 of the argument setup we probably clobbered our call address.
	 In that case we can't do sibcalls.  */
      if (pass == 0
	  && check_sibcall_argument_overlap (after_args, 0, false))
	sibcall_failure = true;

      /* If a non-BLKmode value is returned at the most significant end
	 of a register, shift the register right by the appropriate amount
	 and update VALREG accordingly.  BLKmode values are handled by the
	 group load/store machinery below.  */
      if (!structure_value_addr
	  && !pcc_struct_value
	  && TYPE_MODE (rettype) != VOIDmode
	  && TYPE_MODE (rettype) != BLKmode
	  && REG_P (valreg)
	  && targetm.calls.return_in_msb (rettype))
	{
	  if (shift_return_value (TYPE_MODE (rettype), false, valreg))
	    sibcall_failure = true;
	  valreg = gen_rtx_REG (TYPE_MODE (rettype), REGNO (valreg));
	}

      if (pass && (flags & ECF_MALLOC))
	{
	  rtx temp = gen_reg_rtx (GET_MODE (valreg));
	  rtx_insn *last, *insns;

	  /* The return value from a malloc-like function is a pointer.  */
	  if (TREE_CODE (rettype) == POINTER_TYPE)
	    mark_reg_pointer (temp, MALLOC_ABI_ALIGNMENT);

	  emit_move_insn (temp, valreg);

	  /* The return value from a malloc-like function cannot alias
	     anything else.  */
	  last = get_last_insn ();
	  add_reg_note (last, REG_NOALIAS, temp);

	  /* Write out the sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	  valreg = temp;
	}

      /* For calls to `setjmp', etc., inform
	 function.cc:setjmp_warnings that it should complain if
	 nonvolatile values are live.  For functions that cannot
	 return, inform flow that control does not fall through.  */

      if ((flags & ECF_NORETURN) || pass == 0)
	{
	  /* The barrier must be emitted
	     immediately after the CALL_INSN.  Some ports emit more
	     than just a CALL_INSN above, so we must search for it here.  */

	  rtx_insn *last = get_last_insn ();
	  while (!CALL_P (last))
	    {
	      last = PREV_INSN (last);
	      /* There was no CALL_INSN?  */
	      gcc_assert (last != before_call);
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

      /* If value type not void, return an rtx for the value.  */

      if (TYPE_MODE (rettype) == VOIDmode
	  || ignore)
	target = const0_rtx;
      else if (structure_value_addr)
	{
	  if (target == 0 || !MEM_P (target))
	    {
	      target
		= gen_rtx_MEM (TYPE_MODE (rettype),
			       memory_address (TYPE_MODE (rettype),
					       structure_value_addr));
	      set_mem_attributes (target, rettype, 1);
	    }
	}
      else if (pcc_struct_value)
	{
	  /* This is the special C++ case where we need to
	     know what the true target was.  We take care to
	     never use this value more than once in one expression.  */
	  target = gen_rtx_MEM (TYPE_MODE (rettype),
				copy_to_reg (valreg));
	  set_mem_attributes (target, rettype, 1);
	}
      /* Handle calls that return values in multiple non-contiguous locations.
	 The Irix 6 ABI has examples of this.  */
      else if (GET_CODE (valreg) == PARALLEL)
	{
	  if (target == 0)
	    target = emit_group_move_into_temps (valreg);
	  else if (rtx_equal_p (target, valreg))
	    ;
	  else if (GET_CODE (target) == PARALLEL)
	    /* Handle the result of a emit_group_move_into_temps
	       call in the previous pass.  */
	    emit_group_move (target, valreg);
	  else
	    emit_group_store (target, valreg, rettype,
			      int_size_in_bytes (rettype));
	}
      else if (target
	       && GET_MODE (target) == TYPE_MODE (rettype)
	       && GET_MODE (target) == GET_MODE (valreg))
	{
	  bool may_overlap = false;

	  /* We have to copy a return value in a CLASS_LIKELY_SPILLED hard
	     reg to a plain register.  */
	  if (!REG_P (target) || HARD_REGISTER_P (target))
	    valreg = avoid_likely_spilled_reg (valreg);

	  /* If TARGET is a MEM in the argument area, and we have
	     saved part of the argument area, then we can't store
	     directly into TARGET as it may get overwritten when we
	     restore the argument save area below.  Don't work too
	     hard though and simply force TARGET to a register if it
	     is a MEM; the optimizer is quite likely to sort it out.  */
	  if (ACCUMULATE_OUTGOING_ARGS && pass && MEM_P (target))
	    for (i = 0; i < num_actuals; i++)
	      if (args[i].save_area)
		{
		  may_overlap = true;
		  break;
		}

	  if (may_overlap)
	    target = copy_to_reg (valreg);
	  else
	    {
	      /* TARGET and VALREG cannot be equal at this point
		 because the latter would not have
		 REG_FUNCTION_VALUE_P true, while the former would if
		 it were referring to the same register.

		 If they refer to the same register, this move will be
		 a no-op, except when function inlining is being
		 done.  */
	      emit_move_insn (target, valreg);

	      /* If we are setting a MEM, this code must be executed.
		 Since it is emitted after the call insn, sibcall
		 optimization cannot be performed in that case.  */
	      if (MEM_P (target))
		sibcall_failure = true;
	    }
	}
      else
	target = copy_to_reg (avoid_likely_spilled_reg (valreg));

      /* If we promoted this return value, make the proper SUBREG.
         TARGET might be const0_rtx here, so be careful.  */
      if (REG_P (target)
	  && TYPE_MODE (rettype) != BLKmode
	  && GET_MODE (target) != TYPE_MODE (rettype))
	{
	  tree type = rettype;
	  int unsignedp = TYPE_UNSIGNED (type);
	  machine_mode ret_mode = TYPE_MODE (type);
	  machine_mode pmode;

	  /* Ensure we promote as expected, and get the new unsignedness.  */
	  pmode = promote_function_mode (type, ret_mode, &unsignedp,
					 funtype, 1);
	  gcc_assert (GET_MODE (target) == pmode);

	  if (SCALAR_INT_MODE_P (pmode)
	      && SCALAR_FLOAT_MODE_P (ret_mode)
	      && known_gt (GET_MODE_SIZE (pmode), GET_MODE_SIZE (ret_mode)))
	    target = convert_wider_int_to_float (ret_mode, pmode, target);
	  else
	    {
	      target = gen_lowpart_SUBREG (ret_mode, target);
	      SUBREG_PROMOTED_VAR_P (target) = 1;
	      SUBREG_PROMOTED_SET (target, unsignedp);
	    }
	}

      /* If size of args is variable or this was a constructor call for a stack
	 argument, restore saved stack-pointer value.  */

      if (old_stack_level)
	{
	  rtx_insn *prev = get_last_insn ();

	  emit_stack_restore (SAVE_BLOCK, old_stack_level);
	  stack_pointer_delta = old_stack_pointer_delta;

	  fixup_args_size_notes (prev, get_last_insn (), stack_pointer_delta);

	  pending_stack_adjust = old_pending_adj;
	  old_stack_allocated = stack_pointer_delta - pending_stack_adjust;
	  stack_arg_under_construction = old_stack_arg_under_construction;
	  highest_outgoing_arg_in_use = initial_highest_arg_in_use;
	  stack_usage_map = initial_stack_usage_map;
	  stack_usage_watermark = initial_stack_usage_watermark;
	  sibcall_failure = true;
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
		machine_mode save_mode = GET_MODE (args[i].save_area);
		rtx stack_area
		  = gen_rtx_MEM (save_mode,
				 memory_address (save_mode,
						 XEXP (args[i].stack_slot, 0)));

		if (save_mode != BLKmode)
		  emit_move_insn (stack_area, args[i].save_area);
		else
		  emit_block_move (stack_area, args[i].save_area,
				   (gen_int_mode
				    (args[i].locate.size.constant, Pmode)),
				   BLOCK_OP_CALL_PARM);
	      }

	  highest_outgoing_arg_in_use = initial_highest_arg_in_use;
	  stack_usage_map = initial_stack_usage_map;
	  stack_usage_watermark = initial_stack_usage_watermark;
	}

      /* If this was alloca, record the new stack level.  */
      if (flags & ECF_MAY_BE_ALLOCA)
	record_new_stack_level ();

      /* Free up storage we no longer need.  */
      for (i = 0; i < num_actuals; ++i)
	free (args[i].aligned_regs);

      targetm.calls.end_call_args (args_so_far);

      insns = get_insns ();
      end_sequence ();

      if (pass == 0)
	{
	  tail_call_insns = insns;

	  /* Restore the pending stack adjustment now that we have
	     finished generating the sibling call sequence.  */

	  restore_pending_stack_adjust (&save);

	  /* Prepare arg structure for next iteration.  */
	  for (i = 0; i < num_actuals; i++)
	    {
	      args[i].value = 0;
	      args[i].aligned_regs = 0;
	      args[i].stack = 0;
	    }

	  sbitmap_free (stored_args_map);
	  internal_arg_pointer_exp_state.scan_start = NULL;
	  internal_arg_pointer_exp_state.cache.release ();
	}
      else
	{
	  normal_call_insns = insns;

	  /* Verify that we've deallocated all the stack we used.  */
	  gcc_assert ((flags & ECF_NORETURN)
		      || normal_failure
		      || known_eq (old_stack_allocated,
				   stack_pointer_delta
				   - pending_stack_adjust));
	  if (normal_failure)
	    normal_call_insns = NULL;
	}

      /* If something prevents making this a sibling call,
	 zero out the sequence.  */
      if (sibcall_failure)
	tail_call_insns = NULL;
      else
	break;
    }

  /* If tail call production succeeded, we need to remove REG_EQUIV notes on
     arguments too, as argument area is now clobbered by the call.  */
  if (tail_call_insns)
    {
      emit_insn (tail_call_insns);
      crtl->tail_call_emit = true;
    }
  else
    {
      emit_insn (normal_call_insns);
      if (try_tail_call)
	/* Ideally we'd emit a message for all of the ways that it could
	   have failed.  */
	maybe_complain_about_tail_call (exp, _("tail call production failed"));
    }

  currently_expanding_call--;

  free (stack_usage_map_buf);
  free (args);
  return target;
}

/* A sibling call sequence invalidates any REG_EQUIV notes made for
   this function's incoming arguments.

   At the start of RTL generation we know the only REG_EQUIV notes
   in the rtl chain are those for incoming arguments, so we can look
   for REG_EQUIV notes between the start of the function and the
   NOTE_INSN_FUNCTION_BEG.

   This is (slight) overkill.  We could keep track of the highest
   argument we clobber and be more selective in removing notes, but it
   does not seem to be worth the effort.  */

void
fixup_tail_calls (void)
{
  rtx_insn *insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx note;

      /* There are never REG_EQUIV notes for the incoming arguments
	 after the NOTE_INSN_FUNCTION_BEG note, so stop if we see it.  */
      if (NOTE_P (insn)
	  && NOTE_KIND (insn) == NOTE_INSN_FUNCTION_BEG)
	break;

      note = find_reg_note (insn, REG_EQUIV, 0);
      if (note)
	remove_note (insn, note);
      note = find_reg_note (insn, REG_EQUIV, 0);
      gcc_assert (!note);
    }
}

/* Traverse a list of TYPES and expand all complex types into their
   components.  */
static tree
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

/* Output a library call to function ORGFUN (a SYMBOL_REF rtx)
   for a value of mode OUTMODE,
   with NARGS different arguments, passed as ARGS.
   Store the return value if RETVAL is nonzero: store it in VALUE if
   VALUE is nonnull, otherwise pick a convenient location.  In either
   case return the location of the stored value.

   FN_TYPE should be LCT_NORMAL for `normal' calls, LCT_CONST for
   `const' calls, LCT_PURE for `pure' calls, or another LCT_ value for
   other types of library calls.  */

rtx
emit_library_call_value_1 (int retval, rtx orgfun, rtx value,
			   enum libcall_type fn_type,
			   machine_mode outmode, int nargs, rtx_mode_t *args)
{
  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  /* Size of arguments before any adjustments (such as rounding).  */
  struct args_size original_args_size;
  int argnum;
  rtx fun;
  /* Todo, choose the correct decl type of orgfun. Sadly this information
     isn't present here, so we default to native calling abi here.  */
  tree fndecl ATTRIBUTE_UNUSED = NULL_TREE; /* library calls default to host calling abi ? */
  tree fntype ATTRIBUTE_UNUSED = NULL_TREE; /* library calls default to host calling abi ? */
  int count;
  rtx argblock = 0;
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;
  struct arg
  {
    rtx value;
    machine_mode mode;
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
  bool pcc_struct_value = false;
  poly_int64 struct_value_size = 0;
  int flags;
  int reg_parm_stack_space = 0;
  poly_int64 needed;
  rtx_insn *before_call;
  bool have_push_fusage;
  tree tfom;			/* type_for_mode (outmode, 0) */

#ifdef REG_PARM_STACK_SPACE
  /* Define the boundary of the register parm stack space that needs to be
     save, if any.  */
  int low_to_save = 0, high_to_save = 0;
  rtx save_area = 0;            /* Place that it is saved.  */
#endif

  /* Size of the stack reserved for parameter registers.  */
  unsigned int initial_highest_arg_in_use = highest_outgoing_arg_in_use;
  char *initial_stack_usage_map = stack_usage_map;
  unsigned HOST_WIDE_INT initial_stack_usage_watermark = stack_usage_watermark;
  char *stack_usage_map_buf = NULL;

  rtx struct_value = targetm.calls.struct_value_rtx (0, 0);

#ifdef REG_PARM_STACK_SPACE
  reg_parm_stack_space = REG_PARM_STACK_SPACE ((tree) 0);
#endif

  /* By default, library functions cannot throw.  */
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
    case LCT_NORETURN:
      flags |= ECF_NORETURN;
      break;
    case LCT_THROW:
      flags &= ~ECF_NOTHROW;
      break;
    case LCT_RETURNS_TWICE:
      flags = ECF_RETURNS_TWICE;
      break;
    }
  fun = orgfun;

  /* Ensure current function's preferred stack boundary is at least
     what we need.  */
  if (crtl->preferred_stack_boundary < PREFERRED_STACK_BOUNDARY)
    crtl->preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;

  /* If this kind of value comes back in memory,
     decide where in memory it should come back.  */
  if (outmode != VOIDmode)
    {
      tfom = lang_hooks.types.type_for_mode (outmode, 0);
      if (aggregate_value_p (tfom, 0))
	{
#ifdef PCC_STATIC_STRUCT_RETURN
	  rtx pointer_reg
	    = hard_function_value (build_pointer_type (tfom), 0, 0, 0);
	  mem_value = gen_rtx_MEM (outmode, pointer_reg);
	  pcc_struct_value = true;
	  if (value == 0)
	    value = gen_reg_rtx (outmode);
#else /* not PCC_STATIC_STRUCT_RETURN */
	  struct_value_size = GET_MODE_SIZE (outmode);
	  if (value != 0 && MEM_P (value))
	    mem_value = value;
	  else
	    mem_value = assign_temp (tfom, 1, 1);
#endif
	  /* This call returns a big structure.  */
	  flags &= ~(ECF_CONST | ECF_PURE | ECF_LOOPING_CONST_OR_PURE);
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

  argvec = XALLOCAVEC (struct arg, nargs + 1);
  memset (argvec, 0, (nargs + 1) * sizeof (struct arg));

#ifdef INIT_CUMULATIVE_LIBCALL_ARGS
  INIT_CUMULATIVE_LIBCALL_ARGS (args_so_far_v, outmode, fun);
#else
  INIT_CUMULATIVE_ARGS (args_so_far_v, NULL_TREE, fun, 0, nargs);
#endif
  args_so_far = pack_cumulative_args (&args_so_far_v);

  args_size.constant = 0;
  args_size.var = 0;

  count = 0;

  push_temp_slots ();

  /* If there's a structure value address to be passed,
     either pass it in the special place, or pass it as an extra argument.  */
  if (mem_value && struct_value == 0 && ! pcc_struct_value)
    {
      rtx addr = XEXP (mem_value, 0);

      nargs++;

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (!REG_P (addr) && !MEM_P (addr)
	  && !(CONSTANT_P (addr)
	       && targetm.legitimate_constant_p (Pmode, addr)))
	addr = force_operand (addr, NULL_RTX);

      argvec[count].value = addr;
      argvec[count].mode = Pmode;
      argvec[count].partial = 0;

      function_arg_info ptr_arg (Pmode, /*named=*/true);
      argvec[count].reg = targetm.calls.function_arg (args_so_far, ptr_arg);
      gcc_assert (targetm.calls.arg_partial_bytes (args_so_far, ptr_arg) == 0);

      locate_and_pad_parm (Pmode, NULL_TREE,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			   1,
#else
			   argvec[count].reg != 0,
#endif
			   reg_parm_stack_space, 0,
			   NULL_TREE, &args_size, &argvec[count].locate);

      if (argvec[count].reg == 0 || argvec[count].partial != 0
	  || reg_parm_stack_space > 0)
	args_size.constant += argvec[count].locate.size.constant;

      targetm.calls.function_arg_advance (args_so_far, ptr_arg);

      count++;
    }

  for (unsigned int i = 0; count < nargs; i++, count++)
    {
      rtx val = args[i].first;
      function_arg_info arg (args[i].second, /*named=*/true);
      int unsigned_p = 0;

      /* We cannot convert the arg value to the mode the library wants here;
	 must do it earlier where we know the signedness of the arg.  */
      gcc_assert (arg.mode != BLKmode
		  && (GET_MODE (val) == arg.mode
		      || GET_MODE (val) == VOIDmode));

      /* Make sure it is a reasonable operand for a move or push insn.  */
      if (!REG_P (val) && !MEM_P (val)
	  && !(CONSTANT_P (val)
	       && targetm.legitimate_constant_p (arg.mode, val)))
	val = force_operand (val, NULL_RTX);

      if (pass_by_reference (&args_so_far_v, arg))
	{
	  rtx slot;
	  int must_copy = !reference_callee_copied (&args_so_far_v, arg);

	  /* If this was a CONST function, it is now PURE since it now
	     reads memory.  */
	  if (flags & ECF_CONST)
	    {
	      flags &= ~ECF_CONST;
	      flags |= ECF_PURE;
	    }

	  if (MEM_P (val) && !must_copy)
	    {
	      tree val_expr = MEM_EXPR (val);
	      if (val_expr)
		mark_addressable (val_expr);
	      slot = val;
	    }
	  else
	    {
	      slot = assign_temp (lang_hooks.types.type_for_mode (arg.mode, 0),
				  1, 1);
	      emit_move_insn (slot, val);
	    }

	  call_fusage = gen_rtx_EXPR_LIST (VOIDmode,
					   gen_rtx_USE (VOIDmode, slot),
					   call_fusage);
	  if (must_copy)
	    call_fusage = gen_rtx_EXPR_LIST (VOIDmode,
					     gen_rtx_CLOBBER (VOIDmode,
							      slot),
					     call_fusage);

	  arg.mode = Pmode;
	  arg.pass_by_reference = true;
	  val = force_operand (XEXP (slot, 0), NULL_RTX);
	}

      arg.mode = promote_function_mode (NULL_TREE, arg.mode, &unsigned_p,
					NULL_TREE, 0);
      argvec[count].mode = arg.mode;
      argvec[count].value = convert_modes (arg.mode, GET_MODE (val), val,
					   unsigned_p);
      argvec[count].reg = targetm.calls.function_arg (args_so_far, arg);

      argvec[count].partial
	= targetm.calls.arg_partial_bytes (args_so_far, arg);

      if (argvec[count].reg == 0
	  || argvec[count].partial != 0
	  || reg_parm_stack_space > 0)
	{
	  locate_and_pad_parm (arg.mode, NULL_TREE,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			       1,
#else
			       argvec[count].reg != 0,
#endif
			       reg_parm_stack_space, argvec[count].partial,
			       NULL_TREE, &args_size, &argvec[count].locate);
	  args_size.constant += argvec[count].locate.size.constant;
	  gcc_assert (!argvec[count].locate.size.var);
	}
#ifdef BLOCK_REG_PADDING
      else
	/* The argument is passed entirely in registers.  See at which
	   end it should be padded.  */
	argvec[count].locate.where_pad =
	  BLOCK_REG_PADDING (arg.mode, NULL_TREE,
			     known_le (GET_MODE_SIZE (arg.mode),
				       UNITS_PER_WORD));
#endif

      targetm.calls.function_arg_advance (args_so_far, arg);
    }

  for (int i = 0; i < nargs; i++)
    if (reg_parm_stack_space > 0
	|| argvec[i].reg == 0
	|| argvec[i].partial != 0)
      update_stack_alignment_for_call (&argvec[i].locate);

  /* If this machine requires an external definition for library
     functions, write one out.  */
  assemble_external_libcall (fun);

  original_args_size = args_size;
  args_size.constant = (aligned_upper_bound (args_size.constant
					     + stack_pointer_delta,
					     STACK_BYTES)
			- stack_pointer_delta);

  args_size.constant = upper_bound (args_size.constant,
				    reg_parm_stack_space);

  if (! OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl))))
    args_size.constant -= reg_parm_stack_space;

  crtl->outgoing_args_size = upper_bound (crtl->outgoing_args_size,
					  args_size.constant);

  if (flag_stack_usage_info && !ACCUMULATE_OUTGOING_ARGS)
    {
      poly_int64 pushed = args_size.constant + pending_stack_adjust;
      current_function_pushed_stack_size
	= upper_bound (current_function_pushed_stack_size, pushed);
    }

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

      /* Since we will be writing into the entire argument area, the
	 map must be allocated for its entire size, not just the part that
	 is the responsibility of the caller.  */
      if (! OUTGOING_REG_PARM_STACK_SPACE ((!fndecl ? fntype : TREE_TYPE (fndecl))))
	needed += reg_parm_stack_space;

      poly_int64 limit = needed;
      if (ARGS_GROW_DOWNWARD)
	limit += 1;

      /* For polynomial sizes, this is the maximum possible size needed
	 for arguments with a constant size and offset.  */
      HOST_WIDE_INT const_limit = constant_lower_bound (limit);
      highest_outgoing_arg_in_use = MAX (initial_highest_arg_in_use,
					 const_limit);

      stack_usage_map_buf = XNEWVEC (char, highest_outgoing_arg_in_use);
      stack_usage_map = stack_usage_map_buf;

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
	argblock = plus_constant (Pmode, stack_pointer_rtx,
				  STACK_POINTER_OFFSET);
      else
	argblock = virtual_outgoing_args_rtx;
    }
  else
    {
      if (!targetm.calls.push_argument (0))
	argblock = push_block (gen_int_mode (args_size.constant, Pmode), 0, 0);
    }

  /* We push args individually in reverse order, perform stack alignment
     before the first push (the last arg).  */
  if (argblock == 0)
    anti_adjust_stack (gen_int_mode (args_size.constant
				     - original_args_size.constant,
				     Pmode));

  argnum = nargs - 1;

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

  rtx call_cookie
    = targetm.calls.function_arg (args_so_far,
				  function_arg_info::end_marker ());

  /* Push the args that need to be pushed.  */

  have_push_fusage = false;

  /* ARGNUM indexes the ARGVEC array in the order in which the arguments
     are to be pushed.  */
  for (count = 0; count < nargs; count++, argnum--)
    {
      machine_mode mode = argvec[argnum].mode;
      rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;
      unsigned int parm_align = argvec[argnum].locate.boundary;
      poly_int64 lower_bound = 0, upper_bound = 0;

      if (! (reg != 0 && partial == 0))
	{
	  rtx use;

	  if (ACCUMULATE_OUTGOING_ARGS)
	    {
	      /* If this is being stored into a pre-allocated, fixed-size,
		 stack area, save any previous data at that location.  */

	      if (ARGS_GROW_DOWNWARD)
		{
		  /* stack_slot is negative, but we want to index stack_usage_map
		     with positive values.  */
		  upper_bound = -argvec[argnum].locate.slot_offset.constant + 1;
		  lower_bound = upper_bound - argvec[argnum].locate.size.constant;
		}
	      else
		{
		  lower_bound = argvec[argnum].locate.slot_offset.constant;
		  upper_bound = lower_bound + argvec[argnum].locate.size.constant;
		}

	      if (stack_region_maybe_used_p (lower_bound, upper_bound,
					     reg_parm_stack_space))
		{
		  /* We need to make a save area.  */
		  poly_uint64 size
		    = argvec[argnum].locate.size.constant * BITS_PER_UNIT;
		  machine_mode save_mode
		    = int_mode_for_size (size, 1).else_blk ();
		  rtx adr
		    = plus_constant (Pmode, argblock,
				     argvec[argnum].locate.offset.constant);
		  rtx stack_area
		    = gen_rtx_MEM (save_mode, memory_address (save_mode, adr));

		  if (save_mode == BLKmode)
		    {
		      argvec[argnum].save_area
			= assign_stack_temp (BLKmode,
					     argvec[argnum].locate.size.constant
					     );

		      emit_block_move (validize_mem
				         (copy_rtx (argvec[argnum].save_area)),
				       stack_area,
				       (gen_int_mode
					(argvec[argnum].locate.size.constant,
					 Pmode)),
				       BLOCK_OP_CALL_PARM);
		    }
		  else
		    {
		      argvec[argnum].save_area = gen_reg_rtx (save_mode);

		      emit_move_insn (argvec[argnum].save_area, stack_area);
		    }
		}
	    }

	  emit_push_insn (val, mode, lang_hooks.types.type_for_mode (mode, 0),
			  NULL_RTX, parm_align, partial, reg, 0, argblock,
			  (gen_int_mode
			   (argvec[argnum].locate.offset.constant, Pmode)),
			  reg_parm_stack_space,
			  ARGS_SIZE_RTX (argvec[argnum].locate.alignment_pad), false);

	  /* Now mark the segment we just used.  */
	  if (ACCUMULATE_OUTGOING_ARGS)
	    mark_stack_region_used (lower_bound, upper_bound);

	  NO_DEFER_POP;

	  /* Indicate argument access so that alias.cc knows that these
	     values are live.  */
	  if (argblock)
	    use = plus_constant (Pmode, argblock,
				 argvec[argnum].locate.offset.constant);
	  else if (have_push_fusage)
	    continue;
	  else
	    {
	      /* When arguments are pushed, trying to tell alias.cc where
		 exactly this argument is won't work, because the
		 auto-increment causes confusion.  So we merely indicate
		 that we access something with a known mode somewhere on
		 the stack.  */
	      use = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				  gen_rtx_SCRATCH (Pmode));
	      have_push_fusage = true;
	    }
	  use = gen_rtx_MEM (argvec[argnum].mode, use);
	  use = gen_rtx_USE (VOIDmode, use);
	  call_fusage = gen_rtx_EXPR_LIST (VOIDmode, use, call_fusage);
	}
    }

  argnum = nargs - 1;

  fun = prepare_call_address (NULL, fun, NULL, &call_fusage, 0, 0);

  targetm.calls.start_call_args (args_so_far);

  /* When expanding a normal call, args are stored in push order,
     which is the reverse of what we have here.  */
  bool any_regs = false;
  for (int i = nargs; i-- > 0; )
    if (argvec[i].reg != NULL_RTX)
      {
	targetm.calls.call_args (args_so_far, argvec[i].reg, NULL_TREE);
	any_regs = true;
      }
  if (!any_regs)
    targetm.calls.call_args (args_so_far, pc_rtx, NULL_TREE);

  /* Now load any reg parms into their regs.  */

  /* ARGNUM indexes the ARGVEC array in the order in which the arguments
     are to be pushed.  */
  for (count = 0; count < nargs; count++, argnum--)
    {
      machine_mode mode = argvec[argnum].mode;
      rtx val = argvec[argnum].value;
      rtx reg = argvec[argnum].reg;
      int partial = argvec[argnum].partial;

      /* Handle calls that pass values in multiple non-contiguous
	 locations.  The PA64 has examples of this for library calls.  */
      if (reg != 0 && GET_CODE (reg) == PARALLEL)
	emit_group_load (reg, val, NULL_TREE, GET_MODE_SIZE (mode));
      else if (reg != 0 && partial == 0)
        {
	  emit_move_insn (reg, val);
#ifdef BLOCK_REG_PADDING
	  poly_int64 size = GET_MODE_SIZE (argvec[argnum].mode);

	  /* Copied from load_register_parameters.  */

	  /* Handle case where we have a value that needs shifting
	     up to the msb.  eg. a QImode value and we're padding
	     upward on a BYTES_BIG_ENDIAN machine.  */
	  if (known_lt (size, UNITS_PER_WORD)
	      && (argvec[argnum].locate.where_pad
		  == (BYTES_BIG_ENDIAN ? PAD_UPWARD : PAD_DOWNWARD)))
	    {
	      rtx x;
	      poly_int64 shift = (UNITS_PER_WORD - size) * BITS_PER_UNIT;

	      /* Assigning REG here rather than a temp makes CALL_FUSAGE
		 report the whole reg as used.  Strictly speaking, the
		 call only uses SIZE bytes at the msb end, but it doesn't
		 seem worth generating rtl to say that.  */
	      reg = gen_rtx_REG (word_mode, REGNO (reg));
	      x = expand_shift (LSHIFT_EXPR, word_mode, reg, shift, reg, 1);
	      if (x != reg)
		emit_move_insn (reg, x);
	    }
#endif
	}

      NO_DEFER_POP;
    }

  /* Any regs containing parms remain in use through the call.  */
  for (count = 0; count < nargs; count++)
    {
      rtx reg = argvec[count].reg;
      if (reg != 0 && GET_CODE (reg) == PARALLEL)
	use_group_regs (&call_fusage, reg);
      else if (reg != 0)
        {
	  int partial = argvec[count].partial;
	  if (partial)
	    {
	      int nregs;
              gcc_assert (partial % UNITS_PER_WORD == 0);
	      nregs = partial / UNITS_PER_WORD;
	      use_regs (&call_fusage, REGNO (reg), nregs);
	    }
	  else
	    use_reg (&call_fusage, reg);
	}
    }

  /* Pass the function the address in which to return a structure value.  */
  if (mem_value != 0 && struct_value != 0 && ! pcc_struct_value)
    {
      emit_move_insn (struct_value,
		      force_reg (Pmode,
				 force_operand (XEXP (mem_value, 0),
						NULL_RTX)));
      if (REG_P (struct_value))
	use_reg (&call_fusage, struct_value);
    }

  /* Don't allow popping to be deferred, since then
     cse'ing of library calls could delete a call and leave the pop.  */
  NO_DEFER_POP;
  valreg = (mem_value == 0 && outmode != VOIDmode
	    ? hard_libcall_value (outmode, orgfun) : NULL_RTX);

  /* Stack must be properly aligned now.  */
  gcc_assert (multiple_p (stack_pointer_delta,
			  PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT));

  before_call = get_last_insn ();

  if (flag_callgraph_info)
    record_final_call (SYMBOL_REF_DECL (orgfun), UNKNOWN_LOCATION);

  /* We pass the old value of inhibit_defer_pop + 1 to emit_call_1, which
     will set inhibit_defer_pop to that value.  */
  /* The return type is needed to decide how many bytes the function pops.
     Signedness plays no role in that, so for simplicity, we pretend it's
     always signed.  We also assume that the list of arguments passed has
     no impact, so we pretend it is unknown.  */

  emit_call_1 (fun, NULL,
	       get_identifier (XSTR (orgfun, 0)),
	       build_function_type (tfom, NULL_TREE),
	       original_args_size.constant, args_size.constant,
	       struct_value_size, call_cookie, valreg,
	       old_inhibit_defer_pop + 1, call_fusage, flags, args_so_far);

  if (flag_ipa_ra)
    {
      rtx datum = orgfun;
      gcc_assert (GET_CODE (datum) == SYMBOL_REF);
      rtx_call_insn *last = last_call_insn ();
      add_reg_note (last, REG_CALL_DECL, datum);
    }

  /* Right-shift returned value if necessary.  */
  if (!pcc_struct_value
      && TYPE_MODE (tfom) != BLKmode
      && targetm.calls.return_in_msb (tfom))
    {
      shift_return_value (TYPE_MODE (tfom), false, valreg);
      valreg = gen_rtx_REG (TYPE_MODE (tfom), REGNO (valreg));
    }

  targetm.calls.end_call_args (args_so_far);

  /* For calls to `setjmp', etc., inform function.cc:setjmp_warnings
     that it should complain if nonvolatile values are live.  For
     functions that cannot return, inform flow that control does not
     fall through.  */
  if (flags & ECF_NORETURN)
    {
      /* The barrier note must be emitted
	 immediately after the CALL_INSN.  Some ports emit more than
	 just a CALL_INSN above, so we must search for it here.  */
      rtx_insn *last = get_last_insn ();
      while (!CALL_P (last))
	{
	  last = PREV_INSN (last);
	  /* There was no CALL_INSN?  */
	  gcc_assert (last != before_call);
	}

      emit_barrier_after (last);
    }

  /* Consider that "regular" libcalls, i.e. all of them except for LCT_THROW
     and LCT_RETURNS_TWICE, cannot perform non-local gotos.  */
  if (flags & ECF_NOTHROW)
    {
      rtx_insn *last = get_last_insn ();
      while (!CALL_P (last))
	{
	  last = PREV_INSN (last);
	  /* There was no CALL_INSN?  */
	  gcc_assert (last != before_call);
	}

      make_reg_eh_region_note_nothrow_nononlocal (last);
    }

  /* Now restore inhibit_defer_pop to its actual original value.  */
  OK_DEFER_POP;

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
      else
	{
	  /* Convert to the proper mode if a promotion has been active.  */
	  if (GET_MODE (valreg) != outmode)
	    {
	      int unsignedp = TYPE_UNSIGNED (tfom);

	      gcc_assert (promote_function_mode (tfom, outmode, &unsignedp,
						 fndecl ? TREE_TYPE (fndecl) : fntype, 1)
			  == GET_MODE (valreg));
	      valreg = convert_modes (outmode, GET_MODE (valreg), valreg, 0);
	    }

	  if (value != 0)
	    emit_move_insn (value, valreg);
	  else
	    value = valreg;
	}
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
	    machine_mode save_mode = GET_MODE (argvec[count].save_area);
	    rtx adr = plus_constant (Pmode, argblock,
				     argvec[count].locate.offset.constant);
	    rtx stack_area = gen_rtx_MEM (save_mode,
					  memory_address (save_mode, adr));

	    if (save_mode == BLKmode)
	      emit_block_move (stack_area,
			       validize_mem
			         (copy_rtx (argvec[count].save_area)),
			       (gen_int_mode
				(argvec[count].locate.size.constant, Pmode)),
			       BLOCK_OP_CALL_PARM);
	    else
	      emit_move_insn (stack_area, argvec[count].save_area);
	  }

      highest_outgoing_arg_in_use = initial_highest_arg_in_use;
      stack_usage_map = initial_stack_usage_map;
      stack_usage_watermark = initial_stack_usage_watermark;
    }

  free (stack_usage_map_buf);

  return value;

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

   Return true if this arg should cause sibcall failure,
   false otherwise.  */

static bool
store_one_arg (struct arg_data *arg, rtx argblock, int flags,
	       int variable_size ATTRIBUTE_UNUSED, int reg_parm_stack_space)
{
  tree pval = arg->tree_value;
  rtx reg = 0;
  int partial = 0;
  poly_int64 used = 0;
  poly_int64 lower_bound = 0, upper_bound = 0;
  bool sibcall_failure = false;

  if (TREE_CODE (pval) == ERROR_MARK)
    return true;

  /* Push a new temporary level for any temporaries we make for
     this argument.  */
  push_temp_slots ();

  if (ACCUMULATE_OUTGOING_ARGS && !(flags & ECF_SIBCALL))
    {
      /* If this is being stored into a pre-allocated, fixed-size, stack area,
	 save any previous data at that location.  */
      if (argblock && ! variable_size && arg->stack)
	{
	  if (ARGS_GROW_DOWNWARD)
	    {
	      /* stack_slot is negative, but we want to index stack_usage_map
		 with positive values.  */
	      if (GET_CODE (XEXP (arg->stack_slot, 0)) == PLUS)
		{
		  rtx offset = XEXP (XEXP (arg->stack_slot, 0), 1);
		  upper_bound = -rtx_to_poly_int64 (offset) + 1;
		}
	      else
		upper_bound = 0;

	      lower_bound = upper_bound - arg->locate.size.constant;
	    }
	  else
	    {
	      if (GET_CODE (XEXP (arg->stack_slot, 0)) == PLUS)
		{
		  rtx offset = XEXP (XEXP (arg->stack_slot, 0), 1);
		  lower_bound = rtx_to_poly_int64 (offset);
		}
	      else
		lower_bound = 0;

	      upper_bound = lower_bound + arg->locate.size.constant;
	    }

	  if (stack_region_maybe_used_p (lower_bound, upper_bound,
					 reg_parm_stack_space))
	    {
	      /* We need to make a save area.  */
	      poly_uint64 size = arg->locate.size.constant * BITS_PER_UNIT;
	      machine_mode save_mode
		= int_mode_for_size (size, 1).else_blk ();
	      rtx adr = memory_address (save_mode, XEXP (arg->stack_slot, 0));
	      rtx stack_area = gen_rtx_MEM (save_mode, adr);

	      if (save_mode == BLKmode)
		{
		  arg->save_area
		    = assign_temp (TREE_TYPE (arg->tree_value), 1, 1);
		  preserve_temp_slots (arg->save_area);
		  emit_block_move (validize_mem (copy_rtx (arg->save_area)),
				   stack_area,
				   (gen_int_mode
				    (arg->locate.size.constant, Pmode)),
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

  /* Being passed entirely in a register.  We shouldn't be called in
     this case.  */
  gcc_assert (reg == 0 || partial != 0);

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
	 arguments in general have pass_on_stack == false.

	 If this argument is initialized by a function which takes the
	 address of the argument (a C++ constructor or a C function
	 returning a BLKmode structure), then stack_usage_map is
	 insufficient and expand_call must push the stack around the
	 function call.  Such arguments have pass_on_stack == true.

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

  /* Check for overlap with already clobbered argument area.  */
  if ((flags & ECF_SIBCALL)
      && MEM_P (arg->value)
      && mem_might_overlap_already_clobbered_arg_p (XEXP (arg->value, 0),
						    arg->locate.size.constant))
    sibcall_failure = true;

  /* Don't allow anything left on stack from computation
     of argument to alloca.  */
  if (flags & ECF_MAY_BE_ALLOCA)
    do_pending_stack_adjust ();

  if (arg->value == arg->stack)
    /* If the value is already in the stack slot, we are done.  */
    ;
  else if (arg->mode != BLKmode)
    {
      unsigned int parm_align;

      /* Argument is a scalar, not entirely passed in registers.
	 (If part is passed in registers, arg->partial says how much
	 and emit_push_insn will take care of putting it there.)

	 Push it, and if its size is less than the
	 amount of space allocated to it,
	 also bump stack pointer by the additional space.
	 Note that in C the default argument promotions
	 will prevent such mismatches.  */

      poly_int64 size = (TYPE_EMPTY_P (TREE_TYPE (pval))
			 ? 0 : GET_MODE_SIZE (arg->mode));

      /* Compute how much space the push instruction will push.
	 On many machines, pushing a byte will advance the stack
	 pointer by a halfword.  */
#ifdef PUSH_ROUNDING
      size = PUSH_ROUNDING (size);
#endif
      used = size;

      /* Compute how much space the argument should get:
	 round up to a multiple of the alignment for arguments.  */
      if (targetm.calls.function_arg_padding (arg->mode, TREE_TYPE (pval))
	  != PAD_NONE)
	/* At the moment we don't (need to) support ABIs for which the
	   padding isn't known at compile time.  In principle it should
	   be easy to add though.  */
	used = force_align_up (size, PARM_BOUNDARY / BITS_PER_UNIT);

      /* Compute the alignment of the pushed argument.  */
      parm_align = arg->locate.boundary;
      if (targetm.calls.function_arg_padding (arg->mode, TREE_TYPE (pval))
	  == PAD_DOWNWARD)
	{
	  poly_int64 pad = used - size;
	  unsigned int pad_align = known_alignment (pad) * BITS_PER_UNIT;
	  if (pad_align != 0)
	    parm_align = MIN (parm_align, pad_align);
	}

      /* This isn't already where we want it on the stack, so put it there.
	 This can either be done with push or copy insns.  */
      if (maybe_ne (used, 0)
	  && !emit_push_insn (arg->value, arg->mode, TREE_TYPE (pval),
			      NULL_RTX, parm_align, partial, reg, used - size,
			      argblock, ARGS_SIZE_RTX (arg->locate.offset),
			      reg_parm_stack_space,
			      ARGS_SIZE_RTX (arg->locate.alignment_pad), true))
	sibcall_failure = true;

      /* Unless this is a partially-in-register argument, the argument is now
	 in the stack.  */
      if (partial == 0)
	arg->value = arg->stack;
    }
  else
    {
      /* BLKmode, at least partly to be pushed.  */

      unsigned int parm_align;
      poly_int64 excess;
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
	  /* PUSH_ROUNDING has no effect on us, because emit_push_insn
	     for BLKmode is careful to avoid it.  */
	  excess = (arg->locate.size.constant
		    - arg_int_size_in_bytes (TREE_TYPE (pval))
		    + partial);
	  size_rtx = expand_expr (arg_size_in_bytes (TREE_TYPE (pval)),
				  NULL_RTX, TYPE_MODE (sizetype),
				  EXPAND_NORMAL);
	}

      parm_align = arg->locate.boundary;

      /* When an argument is padded down, the block is aligned to
	 PARM_BOUNDARY, but the actual argument isn't.  */
      if (targetm.calls.function_arg_padding (arg->mode, TREE_TYPE (pval))
	  == PAD_DOWNWARD)
	{
	  if (arg->locate.size.var)
	    parm_align = BITS_PER_UNIT;
	  else
	    {
	      unsigned int excess_align
		= known_alignment (excess) * BITS_PER_UNIT;
	      if (excess_align != 0)
		parm_align = MIN (parm_align, excess_align);
	    }
	}

      if ((flags & ECF_SIBCALL) && MEM_P (arg->value))
	{
	  /* emit_push_insn might not work properly if arg->value and
	     argblock + arg->locate.offset areas overlap.  */
	  rtx x = arg->value;
	  poly_int64 i = 0;

	  if (strip_offset (XEXP (x, 0), &i)
	      == crtl->args.internal_arg_pointer)
	    {
	      /* arg.locate doesn't contain the pretend_args_size offset,
		 it's part of argblock.  Ensure we don't count it in I.  */
	      if (STACK_GROWS_DOWNWARD)
		i -= crtl->args.pretend_args_size;
	      else
		i += crtl->args.pretend_args_size;

	      /* expand_call should ensure this.  */
	      gcc_assert (!arg->locate.offset.var
			  && arg->locate.size.var == 0);
	      poly_int64 size_val = rtx_to_poly_int64 (size_rtx);

	      if (known_eq (arg->locate.offset.constant, i))
		{
		  /* Even though they appear to be at the same location,
		     if part of the outgoing argument is in registers,
		     they aren't really at the same location.  Check for
		     this by making sure that the incoming size is the
		     same as the outgoing size.  */
		  if (partial != 0)
		    sibcall_failure = true;
		}
	      else if (maybe_in_range_p (arg->locate.offset.constant,
					 i, size_val))
		sibcall_failure = true;
	      /* Use arg->locate.size.constant instead of size_rtx
		 because we only care about the part of the argument
		 on the stack.  */
	      else if (maybe_in_range_p (i, arg->locate.offset.constant,
					 arg->locate.size.constant))
		sibcall_failure = true;
	    }
	}

      if (!CONST_INT_P (size_rtx) || INTVAL (size_rtx) != 0)
	emit_push_insn (arg->value, arg->mode, TREE_TYPE (pval), size_rtx,
			parm_align, partial, reg, excess, argblock,
			ARGS_SIZE_RTX (arg->locate.offset),
			reg_parm_stack_space,
			ARGS_SIZE_RTX (arg->locate.alignment_pad), false);
      /* If we bypass emit_push_insn because it is a zero sized argument,
	 we still might need to adjust stack if such argument requires
	 extra alignment.  See PR104558.  */
      else if ((arg->locate.alignment_pad.var
		|| maybe_ne (arg->locate.alignment_pad.constant, 0))
	       && !argblock)
	anti_adjust_stack (ARGS_SIZE_RTX (arg->locate.alignment_pad));

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

  if (arg->reg && GET_CODE (arg->reg) == PARALLEL)
    {
      tree type = TREE_TYPE (arg->tree_value);
      arg->parallel_value
	= emit_group_load_into_temps (arg->reg, arg->value, type,
				      int_size_in_bytes (type));
    }

  /* Mark all slots this store used.  */
  if (ACCUMULATE_OUTGOING_ARGS && !(flags & ECF_SIBCALL)
      && argblock && ! variable_size && arg->stack)
    mark_stack_region_used (lower_bound, upper_bound);

  /* Once we have pushed something, pops can't safely
     be deferred during the rest of the arguments.  */
  NO_DEFER_POP;

  /* Free any temporary slots made in processing this argument.  */
  pop_temp_slots ();

  return sibcall_failure;
}

/* Nonzero if we do not know how to pass ARG solely in registers.  */

bool
must_pass_in_stack_var_size (const function_arg_info &arg)
{
  if (!arg.type)
    return false;

  /* If the type has variable size...  */
  if (!poly_int_tree_p (TYPE_SIZE (arg.type)))
    return true;

  /* If the type is marked as addressable (it is required
     to be constructed into the stack)...  */
  if (TREE_ADDRESSABLE (arg.type))
    return true;

  return false;
}

/* Another version of the TARGET_MUST_PASS_IN_STACK hook.  This one
   takes trailing padding of a structure into account.  */
/* ??? Should be able to merge these two by examining BLOCK_REG_PADDING.  */

bool
must_pass_in_stack_var_size_or_pad (const function_arg_info &arg)
{
  if (!arg.type)
    return false;

  /* If the type has variable size...  */
  if (TREE_CODE (TYPE_SIZE (arg.type)) != INTEGER_CST)
    return true;

  /* If the type is marked as addressable (it is required
     to be constructed into the stack)...  */
  if (TREE_ADDRESSABLE (arg.type))
    return true;

  if (TYPE_EMPTY_P (arg.type))
    return false;

  /* If the padding and mode of the type is such that a copy into
     a register would put it into the wrong part of the register.  */
  if (arg.mode == BLKmode
      && int_size_in_bytes (arg.type) % (PARM_BOUNDARY / BITS_PER_UNIT)
      && (targetm.calls.function_arg_padding (arg.mode, arg.type)
	  == (BYTES_BIG_ENDIAN ? PAD_UPWARD : PAD_DOWNWARD)))
    return true;

  return false;
}

/* Return true if TYPE must be passed on the stack when passed to
   the "..." arguments of a function.  */

bool
must_pass_va_arg_in_stack (tree type)
{
  function_arg_info arg (type, /*named=*/false);
  return targetm.calls.must_pass_in_stack (arg);
}

/* Return true if FIELD is the C++17 empty base field that should
   be ignored for ABI calling convention decisions in order to
   maintain ABI compatibility between C++14 and earlier, which doesn't
   add this FIELD to classes with empty bases, and C++17 and later
   which does.  */

bool
cxx17_empty_base_field_p (const_tree field)
{
  return (DECL_FIELD_ABI_IGNORED (field)
	  && DECL_ARTIFICIAL (field)
	  && RECORD_OR_UNION_TYPE_P (TREE_TYPE (field))
	  && !lookup_attribute ("no_unique_address", DECL_ATTRIBUTES (field)));
}
