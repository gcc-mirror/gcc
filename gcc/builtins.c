/* Expand builtin functions.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.

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
#include "system.h"
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "obstack.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "except.h"
#include "function.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "insn-config.h"
#include "expr.h"
#include "recog.h"
#include "output.h"
#include "typeclass.h"
#include "defaults.h"
#include "toplev.h"
#include "tm_p.h"

#define CALLED_AS_BUILT_IN(NODE) \
   (!strncmp (IDENTIFIER_POINTER (DECL_NAME (NODE)), "__builtin_", 10))

/* Register mappings for target machines without register windows.  */
#ifndef INCOMING_REGNO
#define INCOMING_REGNO(OUT) (OUT)
#endif
#ifndef OUTGOING_REGNO
#define OUTGOING_REGNO(IN) (IN)
#endif

#ifndef PAD_VARARGS_DOWN
#define PAD_VARARGS_DOWN BYTES_BIG_ENDIAN
#endif

/* Define the names of the builtin function types and codes.  */
const char *const built_in_class_names[4]
  = {"NOT_BUILT_IN", "BUILT_IN_FRONTEND", "BUILT_IN_MD", "BUILT_IN_NORMAL"};

#define DEF_BUILTIN(x) STRINGIFY(x),
const char *const built_in_names[(int) END_BUILTINS] =
{
#include "builtins.def"
};
#undef DEF_BUILTIN

tree (*lang_type_promotes_to) PARAMS ((tree));

static int get_pointer_alignment	PARAMS ((tree, unsigned));
static tree c_strlen			PARAMS ((tree));
static rtx get_memory_rtx		PARAMS ((tree));
static int apply_args_size		PARAMS ((void));
static int apply_result_size		PARAMS ((void));
#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
static rtx result_vector		PARAMS ((int, rtx));
#endif
static rtx expand_builtin_apply_args	PARAMS ((void));
static rtx expand_builtin_apply_args_1	PARAMS ((void));
static rtx expand_builtin_apply		PARAMS ((rtx, rtx, rtx));
static void expand_builtin_return	PARAMS ((rtx));
static rtx expand_builtin_classify_type	PARAMS ((tree));
static rtx expand_builtin_mathfn	PARAMS ((tree, rtx, rtx));
static rtx expand_builtin_constant_p	PARAMS ((tree));
static rtx expand_builtin_args_info	PARAMS ((tree));
static rtx expand_builtin_next_arg	PARAMS ((tree));
static rtx expand_builtin_va_start	PARAMS ((int, tree));
static rtx expand_builtin_va_end	PARAMS ((tree));
static rtx expand_builtin_va_copy	PARAMS ((tree));
#ifdef HAVE_cmpstrsi
static rtx expand_builtin_memcmp	PARAMS ((tree, tree, rtx));
static rtx expand_builtin_strcmp	PARAMS ((tree, rtx));
#endif
static rtx expand_builtin_memcpy	PARAMS ((tree));
static rtx expand_builtin_strcpy	PARAMS ((tree));
static rtx expand_builtin_memset	PARAMS ((tree));
static rtx expand_builtin_bzero		PARAMS ((tree));
static rtx expand_builtin_strlen	PARAMS ((tree, rtx,
						 enum machine_mode));
static rtx expand_builtin_alloca	PARAMS ((tree, rtx));
static rtx expand_builtin_ffs		PARAMS ((tree, rtx, rtx));
static rtx expand_builtin_frame_address	PARAMS ((tree));
static tree stabilize_va_list		PARAMS ((tree, int));

/* Return the alignment in bits of EXP, a pointer valued expression.
   But don't return more than MAX_ALIGN no matter what.
   The alignment returned is, by default, the alignment of the thing that
   EXP points to (if it is not a POINTER_TYPE, 0 is returned).

   Otherwise, look at the expression to see if we can do better, i.e., if the
   expression is actually pointing at an object whose alignment is tighter.  */

static int
get_pointer_alignment (exp, max_align)
     tree exp;
     unsigned max_align;
{
  unsigned align, inner;

  if (TREE_CODE (TREE_TYPE (exp)) != POINTER_TYPE)
    return 0;

  align = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
  align = MIN (align, max_align);

  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case NON_LVALUE_EXPR:
	  exp = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (TREE_TYPE (exp)) != POINTER_TYPE)
	    return align;
	  inner = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
	  align = MIN (inner, max_align);
	  break;

	case PLUS_EXPR:
	  /* If sum of pointer + int, restrict our maximum alignment to that
	     imposed by the integer.  If not, we can't do any better than
	     ALIGN.  */
	  if (TREE_CODE (TREE_OPERAND (exp, 1)) != INTEGER_CST)
	    return align;

	  while (((TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)) * BITS_PER_UNIT)
		  & (max_align - 1))
		 != 0)
	    max_align >>= 1;

	  exp = TREE_OPERAND (exp, 0);
	  break;

	case ADDR_EXPR:
	  /* See what we are pointing at and look at its alignment.  */
	  exp = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (exp) == FUNCTION_DECL)
	    align = FUNCTION_BOUNDARY;
	  else if (DECL_P (exp))
	    align = DECL_ALIGN (exp);
#ifdef CONSTANT_ALIGNMENT
	  else if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'c')
	    align = CONSTANT_ALIGNMENT (exp, align);
#endif
	  return MIN (align, max_align);

	default:
	  return align;
	}
    }
}

/* Compute the length of a C string.  TREE_STRING_LENGTH is not the right
   way, because it could contain a zero byte in the middle.
   TREE_STRING_LENGTH is the size of the character array, not the string.

   The value returned is of type `ssizetype'.

   Unfortunately, string_constant can't access the values of const char
   arrays with initializers, so neither can we do so here.  */

static tree
c_strlen (src)
     tree src;
{
  tree offset_node;
  int offset, max;
  char *ptr;

  src = string_constant (src, &offset_node);
  if (src == 0)
    return 0;

  max = TREE_STRING_LENGTH (src);
  ptr = TREE_STRING_POINTER (src);

  if (offset_node && TREE_CODE (offset_node) != INTEGER_CST)
    {
      /* If the string has an internal zero byte (e.g., "foo\0bar"), we can't
	 compute the offset to the following null if we don't know where to
	 start searching for it.  */
      int i;

      for (i = 0; i < max; i++)
	if (ptr[i] == 0)
	  return 0;

      /* We don't know the starting offset, but we do know that the string
	 has no internal zero bytes.  We can assume that the offset falls
	 within the bounds of the string; otherwise, the programmer deserves
	 what he gets.  Subtract the offset from the length of the string,
	 and return that.  This would perhaps not be valid if we were dealing
	 with named arrays in addition to literal string constants.  */

      return size_diffop (size_int (max), offset_node);
    }

  /* We have a known offset into the string.  Start searching there for
     a null character.  */
  if (offset_node == 0)
    offset = 0;
  else
    {
      /* Did we get a long long offset?  If so, punt.  */
      if (TREE_INT_CST_HIGH (offset_node) != 0)
	return 0;
      offset = TREE_INT_CST_LOW (offset_node);
    }

  /* If the offset is known to be out of bounds, warn, and call strlen at
     runtime.  */
  if (offset < 0 || offset > max)
    {
      warning ("offset outside bounds of constant string");
      return 0;
    }

  /* Use strlen to search for the first zero byte.  Since any strings
     constructed with build_string will have nulls appended, we win even
     if we get handed something like (char[4])"abcd".

     Since OFFSET is our starting index into the string, no further
     calculation is needed.  */
  return ssize_int (strlen (ptr + offset));
}

/* Given TEM, a pointer to a stack frame, follow the dynamic chain COUNT
   times to get the address of either a higher stack frame, or a return
   address located within it (depending on FNDECL_CODE).  */

rtx
expand_builtin_return_addr (fndecl_code, count, tem)
     enum built_in_function fndecl_code;
     int count;
     rtx tem;
{
  int i;

  /* Some machines need special handling before we can access
     arbitrary frames.  For example, on the sparc, we must first flush
     all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
  if (count > 0)
    SETUP_FRAME_ADDRESSES ();
#endif

  /* On the sparc, the return address is not in the frame, it is in a
     register.  There is no way to access it off of the current frame
     pointer, but it can be accessed off the previous frame pointer by
     reading the value from the register window save area.  */
#ifdef RETURN_ADDR_IN_PREVIOUS_FRAME
  if (fndecl_code == BUILT_IN_RETURN_ADDRESS)
    count--;
#endif

  /* Scan back COUNT frames to the specified frame.  */
  for (i = 0; i < count; i++)
    {
      /* Assume the dynamic chain pointer is in the word that the
	 frame address points to, unless otherwise specified.  */
#ifdef DYNAMIC_CHAIN_ADDRESS
      tem = DYNAMIC_CHAIN_ADDRESS (tem);
#endif
      tem = memory_address (Pmode, tem);
      tem = copy_to_reg (gen_rtx_MEM (Pmode, tem));
    }

  /* For __builtin_frame_address, return what we've got.  */
  if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
    return tem;

  /* For __builtin_return_address, Get the return address from that
     frame.  */
#ifdef RETURN_ADDR_RTX
  tem = RETURN_ADDR_RTX (count, tem);
#else
  tem = memory_address (Pmode,
			plus_constant (tem, GET_MODE_SIZE (Pmode)));
  tem = gen_rtx_MEM (Pmode, tem);
#endif
  return tem;
}

/* __builtin_setjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.  Much of
   the code below (and for longjmp) is copied from the handling of
   non-local gotos.

   NOTE: This is intended for use by GNAT and the exception handling
   scheme in the compiler and will only work in the method used by
   them.  */

rtx
expand_builtin_setjmp (buf_addr, target, first_label, next_label)
     rtx buf_addr;
     rtx target;
     rtx first_label, next_label;
{
  rtx lab1 = gen_label_rtx ();
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
  enum machine_mode value_mode;
  rtx stack_save;

  value_mode = TYPE_MODE (integer_type_node);

#ifdef POINTERS_EXTEND_UNSIGNED
  buf_addr = convert_memory_address (Pmode, buf_addr);
#endif

  buf_addr = force_reg (Pmode, force_operand (buf_addr, NULL_RTX));

  if (target == 0 || GET_CODE (target) != REG
      || REGNO (target) < FIRST_PSEUDO_REGISTER)
    target = gen_reg_rtx (value_mode);

  emit_queue ();

  /* We store the frame pointer and the address of lab1 in the buffer
     and use the rest of it for the stack save area, which is
     machine-dependent.  */

#ifndef BUILTIN_SETJMP_FRAME_VALUE
#define BUILTIN_SETJMP_FRAME_VALUE virtual_stack_vars_rtx
#endif

  emit_move_insn (gen_rtx_MEM (Pmode, buf_addr),
		  BUILTIN_SETJMP_FRAME_VALUE);
  emit_move_insn (validize_mem
		  (gen_rtx_MEM (Pmode,
				plus_constant (buf_addr,
					       GET_MODE_SIZE (Pmode)))),
		  force_reg (Pmode, gen_rtx_LABEL_REF (Pmode, lab1)));

  stack_save = gen_rtx_MEM (sa_mode,
			    plus_constant (buf_addr,
					   2 * GET_MODE_SIZE (Pmode)));
  emit_stack_save (SAVE_NONLOCAL, &stack_save, NULL_RTX);

  /* If there is further processing to do, do it.  */
#ifdef HAVE_builtin_setjmp_setup
  if (HAVE_builtin_setjmp_setup)
    emit_insn (gen_builtin_setjmp_setup (buf_addr));
#endif

  /* Set TARGET to zero and branch to the first-time-through label.  */
  emit_move_insn (target, const0_rtx);
  emit_jump_insn (gen_jump (first_label));
  emit_barrier ();
  emit_label (lab1);

  /* Tell flow about the strange goings on.  Putting `lab1' on
     `nonlocal_goto_handler_labels' to indicates that function
     calls may traverse the arc back to this label.  */

  current_function_has_nonlocal_label = 1;
  nonlocal_goto_handler_labels =
    gen_rtx_EXPR_LIST (VOIDmode, lab1, nonlocal_goto_handler_labels);

  /* Clobber the FP when we get here, so we have to make sure it's
     marked as used by this function.  */
  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));

  /* Mark the static chain as clobbered here so life information
     doesn't get messed up for it.  */
  emit_insn (gen_rtx_CLOBBER (VOIDmode, static_chain_rtx));

  /* Now put in the code to restore the frame pointer, and argument
     pointer, if needed.  The code below is from expand_end_bindings
     in stmt.c; see detailed documentation there.  */
#ifdef HAVE_nonlocal_goto
  if (! HAVE_nonlocal_goto)
#endif
    emit_move_insn (virtual_stack_vars_rtx, hard_frame_pointer_rtx);

#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
  if (fixed_regs[ARG_POINTER_REGNUM])
    {
#ifdef ELIMINABLE_REGS
      size_t i;
      static struct elims {int from, to;} elim_regs[] = ELIMINABLE_REGS;

      for (i = 0; i < sizeof elim_regs / sizeof elim_regs[0]; i++)
	if (elim_regs[i].from == ARG_POINTER_REGNUM
	    && elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	  break;

      if (i == sizeof elim_regs / sizeof elim_regs [0])
#endif
	{
	  /* Now restore our arg pointer from the address at which it
	     was saved in our stack frame.
	     If there hasn't be space allocated for it yet, make
	     some now.  */
	  if (arg_pointer_save_area == 0)
	    arg_pointer_save_area
	      = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);
	  emit_move_insn (virtual_incoming_args_rtx,
			  copy_to_reg (arg_pointer_save_area));
	}
    }
#endif

#ifdef HAVE_builtin_setjmp_receiver
  if (HAVE_builtin_setjmp_receiver)
    emit_insn (gen_builtin_setjmp_receiver (lab1));
  else
#endif
#ifdef HAVE_nonlocal_goto_receiver
    if (HAVE_nonlocal_goto_receiver)
      emit_insn (gen_nonlocal_goto_receiver ());
    else
#endif
      {
	; /* Nothing */
      }

  /* Set TARGET, and branch to the next-time-through label.  */
  emit_move_insn (target, const1_rtx);
  emit_jump_insn (gen_jump (next_label));
  emit_barrier ();

  return target;
}

/* __builtin_longjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.  Much of
   the code below is copied from the handling of non-local gotos.

   NOTE: This is intended for use by GNAT and the exception handling
   scheme in the compiler and will only work in the method used by
   them.  */

void
expand_builtin_longjmp (buf_addr, value)
     rtx buf_addr, value;
{
  rtx fp, lab, stack;
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);

#ifdef POINTERS_EXTEND_UNSIGNED
  buf_addr = convert_memory_address (Pmode, buf_addr);
#endif
  buf_addr = force_reg (Pmode, buf_addr);

  /* We used to store value in static_chain_rtx, but that fails if pointers
     are smaller than integers.  We instead require that the user must pass
     a second argument of 1, because that is what builtin_setjmp will
     return.  This also makes EH slightly more efficient, since we are no
     longer copying around a value that we don't care about.  */
  if (value != const1_rtx)
    abort ();

#ifdef HAVE_builtin_longjmp
  if (HAVE_builtin_longjmp)
    emit_insn (gen_builtin_longjmp (buf_addr));
  else
#endif
    {
      fp = gen_rtx_MEM (Pmode, buf_addr);
      lab = gen_rtx_MEM (Pmode, plus_constant (buf_addr,
					       GET_MODE_SIZE (Pmode)));

      stack = gen_rtx_MEM (sa_mode, plus_constant (buf_addr,
						   2 * GET_MODE_SIZE (Pmode)));

      /* Pick up FP, label, and SP from the block and jump.  This code is
	 from expand_goto in stmt.c; see there for detailed comments.  */
#if HAVE_nonlocal_goto
      if (HAVE_nonlocal_goto)
	/* We have to pass a value to the nonlocal_goto pattern that will
	   get copied into the static_chain pointer, but it does not matter
	   what that value is, because builtin_setjmp does not use it.  */
	emit_insn (gen_nonlocal_goto (value, fp, stack, lab));
      else
#endif
	{
	  lab = copy_to_reg (lab);

	  emit_move_insn (hard_frame_pointer_rtx, fp);
	  emit_stack_restore (SAVE_NONLOCAL, stack, NULL_RTX);

	  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
	  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
	  emit_indirect_jump (lab);
	}
    }
}

/* Get a MEM rtx for expression EXP which can be used in a string instruction
   (cmpstrsi, movstrsi, ..).  */
static rtx
get_memory_rtx (exp)
     tree exp;
{
  rtx mem;
  int is_aggregate;

  mem = gen_rtx_MEM (BLKmode,
		     memory_address (BLKmode,
				     expand_expr (exp, NULL_RTX,
						  ptr_mode, EXPAND_SUM)));

  RTX_UNCHANGING_P (mem) = TREE_READONLY (exp);

  /* Figure out the type of the object pointed to.  Set MEM_IN_STRUCT_P
     if the value is the address of a structure or if the expression is
     cast to a pointer to structure type.  */
  is_aggregate = 0;

  while (TREE_CODE (exp) == NOP_EXPR)
    {
      tree cast_type = TREE_TYPE (exp);
      if (TREE_CODE (cast_type) == POINTER_TYPE
	  && AGGREGATE_TYPE_P (TREE_TYPE (cast_type)))
	{
	  is_aggregate = 1;
	  break;
	}
      exp = TREE_OPERAND (exp, 0);
    }

  if (is_aggregate == 0)
    {
      tree type;

      if (TREE_CODE (exp) == ADDR_EXPR)
	/* If this is the address of an object, check whether the
	   object is an array.  */
	type = TREE_TYPE (TREE_OPERAND (exp, 0));
      else
	type = TREE_TYPE (TREE_TYPE (exp));
      is_aggregate = AGGREGATE_TYPE_P (type);
    }

  MEM_SET_IN_STRUCT_P (mem, is_aggregate);
  return mem;
}

/* Built-in functions to perform an untyped call and return.  */

/* For each register that may be used for calling a function, this
   gives a mode used to copy the register's value.  VOIDmode indicates
   the register is not used for calling a function.  If the machine
   has register windows, this gives only the outbound registers.
   INCOMING_REGNO gives the corresponding inbound register.  */
static enum machine_mode apply_args_mode[FIRST_PSEUDO_REGISTER];

/* For each register that may be used for returning values, this gives
   a mode used to copy the register's value.  VOIDmode indicates the
   register is not used for returning values.  If the machine has
   register windows, this gives only the outbound registers.
   INCOMING_REGNO gives the corresponding inbound register.  */
static enum machine_mode apply_result_mode[FIRST_PSEUDO_REGISTER];

/* For each register that may be used for calling a function, this
   gives the offset of that register into the block returned by
   __builtin_apply_args.  0 indicates that the register is not
   used for calling a function.  */
static int apply_args_reg_offset[FIRST_PSEUDO_REGISTER];

/* Return the offset of register REGNO into the block returned by 
   __builtin_apply_args.  This is not declared static, since it is
   needed in objc-act.c.  */

int 
apply_args_register_offset (regno)
     int regno;
{
  apply_args_size ();

  /* Arguments are always put in outgoing registers (in the argument
     block) if such make sense.  */
#ifdef OUTGOING_REGNO
  regno = OUTGOING_REGNO(regno);
#endif
  return apply_args_reg_offset[regno];
}

/* Return the size required for the block returned by __builtin_apply_args,
   and initialize apply_args_mode.  */

static int
apply_args_size ()
{
  static int size = -1;
  int align, regno;
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      /* The first value is the incoming arg-pointer.  */
      size = GET_MODE_SIZE (Pmode);

      /* The second value is the structure value address unless this is
	 passed as an "invisible" first argument.  */
      if (struct_value_rtx)
	size += GET_MODE_SIZE (Pmode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_ARG_REGNO_P (regno))
	  {
	    /* Search for the proper mode for copying this register's
	       value.  I'm not sure this is right, but it works so far.  */
	    enum machine_mode best_mode = VOIDmode;

	    for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
		 mode != VOIDmode;
		 mode = GET_MODE_WIDER_MODE (mode))
	      if (HARD_REGNO_MODE_OK (regno, mode)
		  && HARD_REGNO_NREGS (regno, mode) == 1)
		best_mode = mode;

	    if (best_mode == VOIDmode)
	      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
		   mode != VOIDmode;
		   mode = GET_MODE_WIDER_MODE (mode))
		if (HARD_REGNO_MODE_OK (regno, mode)
		    && (mov_optab->handlers[(int) mode].insn_code
			!= CODE_FOR_nothing))
		  best_mode = mode;

	    mode = best_mode;
	    if (mode == VOIDmode)
	      abort ();

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    apply_args_reg_offset[regno] = size;
	    size += GET_MODE_SIZE (mode);
	    apply_args_mode[regno] = mode;
	  }
	else
	  {
	    apply_args_mode[regno] = VOIDmode;
	    apply_args_reg_offset[regno] = 0;
	  }
    }
  return size;
}

/* Return the size required for the block returned by __builtin_apply,
   and initialize apply_result_mode.  */

static int
apply_result_size ()
{
  static int size = -1;
  int align, regno;
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      size = 0;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_VALUE_REGNO_P (regno))
	  {
	    /* Search for the proper mode for copying this register's
	       value.  I'm not sure this is right, but it works so far.  */
	    enum machine_mode best_mode = VOIDmode;

	    for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
		 mode != TImode;
		 mode = GET_MODE_WIDER_MODE (mode))
	      if (HARD_REGNO_MODE_OK (regno, mode))
		best_mode = mode;

	    if (best_mode == VOIDmode)
	      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
		   mode != VOIDmode;
		   mode = GET_MODE_WIDER_MODE (mode))
		if (HARD_REGNO_MODE_OK (regno, mode)
		    && (mov_optab->handlers[(int) mode].insn_code
			!= CODE_FOR_nothing))
		  best_mode = mode;

	    mode = best_mode;
	    if (mode == VOIDmode)
	      abort ();

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    size += GET_MODE_SIZE (mode);
	    apply_result_mode[regno] = mode;
	  }
	else
	  apply_result_mode[regno] = VOIDmode;

      /* Allow targets that use untyped_call and untyped_return to override
	 the size so that machine-specific information can be stored here.  */
#ifdef APPLY_RESULT_SIZE
      size = APPLY_RESULT_SIZE;
#endif
    }
  return size;
}

#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
/* Create a vector describing the result block RESULT.  If SAVEP is true,
   the result block is used to save the values; otherwise it is used to
   restore the values.  */

static rtx
result_vector (savep, result)
     int savep;
     rtx result;
{
  int regno, size, align, nelts;
  enum machine_mode mode;
  rtx reg, mem;
  rtx *savevec = (rtx *) alloca (FIRST_PSEUDO_REGISTER * sizeof (rtx));
  
  size = nelts = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, savep ? regno : INCOMING_REGNO (regno));
	mem = change_address (result, mode,
			      plus_constant (XEXP (result, 0), size));
	savevec[nelts++] = (savep
			    ? gen_rtx_SET (VOIDmode, mem, reg)
			    : gen_rtx_SET (VOIDmode, reg, mem));
	size += GET_MODE_SIZE (mode);
      }
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelts, savevec));
}
#endif /* HAVE_untyped_call or HAVE_untyped_return */

/* Save the state required to perform an untyped call with the same
   arguments as were passed to the current function.  */

static rtx
expand_builtin_apply_args_1 ()
{
  rtx registers;
  int size, align, regno;
  enum machine_mode mode;

  /* Create a block where the arg-pointer, structure value address,
     and argument registers can be saved.  */
  registers = assign_stack_local (BLKmode, apply_args_size (), -1);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value_rtx)
    size += GET_MODE_SIZE (Pmode);

  /* Save each register used in calling a function to the block.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	rtx tem;

	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;

	tem = gen_rtx_REG (mode, INCOMING_REGNO (regno));

	emit_move_insn (change_address (registers, mode,
					plus_constant (XEXP (registers, 0),
						       size)),
			tem);
	size += GET_MODE_SIZE (mode);
      }

  /* Save the arg pointer to the block.  */
  emit_move_insn (change_address (registers, Pmode, XEXP (registers, 0)),
		  copy_to_reg (virtual_incoming_args_rtx));
  size = GET_MODE_SIZE (Pmode);

  /* Save the structure value address unless this is passed as an
     "invisible" first argument.  */
  if (struct_value_incoming_rtx)
    {
      emit_move_insn (change_address (registers, Pmode,
				      plus_constant (XEXP (registers, 0),
						     size)),
		      copy_to_reg (struct_value_incoming_rtx));
      size += GET_MODE_SIZE (Pmode);
    }

  /* Return the address of the block.  */
  return copy_addr_to_reg (XEXP (registers, 0));
}

/* __builtin_apply_args returns block of memory allocated on
   the stack into which is stored the arg pointer, structure
   value address, static chain, and all the registers that might
   possibly be used in performing a function call.  The code is
   moved to the start of the function so the incoming values are
   saved.  */
static rtx
expand_builtin_apply_args ()
{
  /* Don't do __builtin_apply_args more than once in a function.
     Save the result of the first call and reuse it.  */
  if (apply_args_value != 0)
    return apply_args_value;
  {
    /* When this function is called, it means that registers must be
       saved on entry to this function.  So we migrate the
       call to the first insn of this function.  */
    rtx temp;
    rtx seq;

    start_sequence ();
    temp = expand_builtin_apply_args_1 ();
    seq = get_insns ();
    end_sequence ();

    apply_args_value = temp;

    /* Put the sequence after the NOTE that starts the function.
       If this is inside a SEQUENCE, make the outer-level insn
       chain current, so the code is placed at the start of the
       function.  */
    push_topmost_sequence ();
    emit_insns_before (seq, NEXT_INSN (get_insns ()));
    pop_topmost_sequence ();
    return temp;
  }
}

/* Perform an untyped call and save the state required to perform an
   untyped return of whatever value was returned by the given function.  */

static rtx
expand_builtin_apply (function, arguments, argsize)
     rtx function, arguments, argsize;
{
  int size, align, regno;
  enum machine_mode mode;
  rtx incoming_args, result, reg, dest, call_insn;
  rtx old_stack_level = 0;
  rtx call_fusage = 0;

  /* Create a block where the return registers can be saved.  */
  result = assign_stack_local (BLKmode, apply_result_size (), -1);

  /* ??? The argsize value should be adjusted here.  */

  /* Fetch the arg pointer from the ARGUMENTS block.  */
  incoming_args = gen_reg_rtx (Pmode);
  emit_move_insn (incoming_args,
		  gen_rtx_MEM (Pmode, arguments));
#ifndef STACK_GROWS_DOWNWARD
  incoming_args = expand_binop (Pmode, sub_optab, incoming_args, argsize,
				incoming_args, 0, OPTAB_LIB_WIDEN);
#endif

  /* Perform postincrements before actually calling the function.  */
  emit_queue ();

  /* Push a new argument block and copy the arguments.  */
  do_pending_stack_adjust ();

  /* Save the stack with nonlocal if available */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_save (SAVE_NONLOCAL, &old_stack_level, NULL_RTX);
  else
#endif
    emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);

  /* Push a block of memory onto the stack to store the memory arguments.
     Save the address in a register, and copy the memory arguments.  ??? I
     haven't figured out how the calling convention macros effect this,
     but it's likely that the source and/or destination addresses in
     the block copy will need updating in machine specific ways.  */
  dest = allocate_dynamic_stack_space (argsize, 0, 0);
  emit_block_move (gen_rtx_MEM (BLKmode, dest),
		   gen_rtx_MEM (BLKmode, incoming_args),
		   argsize,
		   PARM_BOUNDARY / BITS_PER_UNIT);

  /* Refer to the argument block.  */
  apply_args_size ();
  arguments = gen_rtx_MEM (BLKmode, arguments);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value_rtx)
    size += GET_MODE_SIZE (Pmode);

  /* Restore each of the registers previously saved.  Make USE insns
     for each of these registers for use in making the call.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, regno);
	emit_move_insn (reg,
			change_address (arguments, mode,
					plus_constant (XEXP (arguments, 0),
						       size)));

	use_reg (&call_fusage, reg);
	size += GET_MODE_SIZE (mode);
      }

  /* Restore the structure value address unless this is passed as an
     "invisible" first argument.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value_rtx)
    {
      rtx value = gen_reg_rtx (Pmode);
      emit_move_insn (value,
		      change_address (arguments, Pmode,
				      plus_constant (XEXP (arguments, 0),
						     size)));
      emit_move_insn (struct_value_rtx, value);
      if (GET_CODE (struct_value_rtx) == REG)
	  use_reg (&call_fusage, struct_value_rtx);
      size += GET_MODE_SIZE (Pmode);
    }

  /* All arguments and registers used for the call are set up by now!  */
  function = prepare_call_address (function, NULL_TREE, &call_fusage, 0);

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (function) != SYMBOL_REF)
    function = memory_address (FUNCTION_MODE, function);

  /* Generate the actual call instruction and save the return value.  */
#ifdef HAVE_untyped_call
  if (HAVE_untyped_call)
    emit_call_insn (gen_untyped_call (gen_rtx_MEM (FUNCTION_MODE, function),
				      result, result_vector (1, result)));
  else
#endif
#ifdef HAVE_call_value
  if (HAVE_call_value)
    {
      rtx valreg = 0;

      /* Locate the unique return register.  It is not possible to
	 express a call that sets more than one return register using
	 call_value; use untyped_call for that.  In fact, untyped_call
	 only needs to save the return registers in the given block.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((mode = apply_result_mode[regno]) != VOIDmode)
	  {
	    if (valreg)
	      abort (); /* HAVE_untyped_call required.  */
	    valreg = gen_rtx_REG (mode, regno);
	  }

      emit_call_insn (gen_call_value (valreg,
				      gen_rtx_MEM (FUNCTION_MODE, function),
				      const0_rtx, NULL_RTX, const0_rtx));

      emit_move_insn (change_address (result, GET_MODE (valreg),
				      XEXP (result, 0)),
		      valreg);
    }
  else
#endif
    abort ();

  /* Find the CALL insn we just emitted.  */
  for (call_insn = get_last_insn ();
       call_insn && GET_CODE (call_insn) != CALL_INSN;
       call_insn = PREV_INSN (call_insn))
    ;

  if (! call_insn)
    abort ();

  /* Put the register usage information on the CALL.  If there is already
     some usage information, put ours at the end.  */
  if (CALL_INSN_FUNCTION_USAGE (call_insn))
    {
      rtx link;

      for (link = CALL_INSN_FUNCTION_USAGE (call_insn); XEXP (link, 1) != 0;
	   link = XEXP (link, 1))
	;

      XEXP (link, 1) = call_fusage;
    }
  else
    CALL_INSN_FUNCTION_USAGE (call_insn) = call_fusage;

  /* Restore the stack.  */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_restore (SAVE_NONLOCAL, old_stack_level, NULL_RTX);
  else
#endif
    emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);

  /* Return the address of the result block.  */
  return copy_addr_to_reg (XEXP (result, 0));
}

/* Perform an untyped return.  */

static void
expand_builtin_return (result)
     rtx result;
{
  int size, align, regno;
  enum machine_mode mode;
  rtx reg;
  rtx call_fusage = 0;

  apply_result_size ();
  result = gen_rtx_MEM (BLKmode, result);

#ifdef HAVE_untyped_return
  if (HAVE_untyped_return)
    {
      emit_jump_insn (gen_untyped_return (result, result_vector (0, result)));
      emit_barrier ();
      return;
    }
#endif

  /* Restore the return value and note that each value is used.  */
  size = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, INCOMING_REGNO (regno));
	emit_move_insn (reg,
			change_address (result, mode,
					plus_constant (XEXP (result, 0),
						       size)));

	push_to_sequence (call_fusage);
	emit_insn (gen_rtx_USE (VOIDmode, reg));
	call_fusage = get_insns ();
	end_sequence ();
	size += GET_MODE_SIZE (mode);
      }

  /* Put the USE insns before the return.  */
  emit_insns (call_fusage);

  /* Return whatever values was restored by jumping directly to the end
     of the function.  */
  expand_null_return ();
}

/* Expand a call to __builtin_classify_type with arguments found in
   ARGLIST.  */
static rtx
expand_builtin_classify_type (arglist)
     tree arglist;
{
  if (arglist != 0)
    {
      tree type = TREE_TYPE (TREE_VALUE (arglist));
      enum tree_code code = TREE_CODE (type);
      if (code == VOID_TYPE)
	return GEN_INT (void_type_class);
      if (code == INTEGER_TYPE)
	return GEN_INT (integer_type_class);
      if (code == CHAR_TYPE)
	return GEN_INT (char_type_class);
      if (code == ENUMERAL_TYPE)
	return GEN_INT (enumeral_type_class);
      if (code == BOOLEAN_TYPE)
	return GEN_INT (boolean_type_class);
      if (code == POINTER_TYPE)
	return GEN_INT (pointer_type_class);
      if (code == REFERENCE_TYPE)
	return GEN_INT (reference_type_class);
      if (code == OFFSET_TYPE)
	return GEN_INT (offset_type_class);
      if (code == REAL_TYPE)
	return GEN_INT (real_type_class);
      if (code == COMPLEX_TYPE)
	return GEN_INT (complex_type_class);
      if (code == FUNCTION_TYPE)
	return GEN_INT (function_type_class);
      if (code == METHOD_TYPE)
	return GEN_INT (method_type_class);
      if (code == RECORD_TYPE)
	return GEN_INT (record_type_class);
      if (code == UNION_TYPE || code == QUAL_UNION_TYPE)
	return GEN_INT (union_type_class);
      if (code == ARRAY_TYPE)
	{
	  if (TYPE_STRING_FLAG (type))
	    return GEN_INT (string_type_class);
	  else
	    return GEN_INT (array_type_class);
	}
      if (code == SET_TYPE)
	return GEN_INT (set_type_class);
      if (code == FILE_TYPE)
	return GEN_INT (file_type_class);
      if (code == LANG_TYPE)
	return GEN_INT (lang_type_class);
    }
  return GEN_INT (no_type_class);
}

/* Expand expression EXP, which is a call to __builtin_constant_p.  */
static rtx
expand_builtin_constant_p (exp)
     tree exp;
{
  tree arglist = TREE_OPERAND (exp, 1);
  enum machine_mode value_mode = TYPE_MODE (TREE_TYPE (exp));

  if (arglist == 0)
    return const0_rtx;
  else
    {
      tree arg = TREE_VALUE (arglist);
      rtx tmp;

      /* We return 1 for a numeric type that's known to be a constant
	 value at compile-time or for an aggregate type that's a
	 literal constant.  */
      STRIP_NOPS (arg);

      /* If we know this is a constant, emit the constant of one.  */
      if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'c'
	  || (TREE_CODE (arg) == CONSTRUCTOR
	      && TREE_CONSTANT (arg))
	  || (TREE_CODE (arg) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST))
	return const1_rtx;

      /* If we aren't going to be running CSE or this expression
	 has side effects, show we don't know it to be a constant.
	 Likewise if it's a pointer or aggregate type since in those
	 case we only want literals, since those are only optimized
	 when generating RTL, not later.  */
      if (TREE_SIDE_EFFECTS (arg) || cse_not_expected
	  || AGGREGATE_TYPE_P (TREE_TYPE (arg))
	  || POINTER_TYPE_P (TREE_TYPE (arg)))
	return const0_rtx;

      /* Otherwise, emit (constant_p_rtx (ARG)) and let CSE get a
	 chance to see if it can deduce whether ARG is constant.  */

      tmp = expand_expr (arg, NULL_RTX, VOIDmode, 0);
      tmp = gen_rtx_CONSTANT_P_RTX (value_mode, tmp);
      return tmp;
    }
}

/* Expand a call to one of the builtin math functions (sin, cos, or sqrt).
   Return 0 if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */
static rtx
expand_builtin_mathfn (exp, target, subtarget)
     tree exp;
     rtx target, subtarget;
{
  optab builtin_optab;  
  rtx op0, insns;
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);

  if (arglist == 0
      /* Arg could be wrong type if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != REAL_TYPE)
    return 0;

  /* Stabilize and compute the argument.  */
  if (TREE_CODE (TREE_VALUE (arglist)) != VAR_DECL
      && TREE_CODE (TREE_VALUE (arglist)) != PARM_DECL)
    {
      exp = copy_node (exp);
      TREE_OPERAND (exp, 1) = arglist;
      /* Wrap the computation of the argument in a SAVE_EXPR.  That
	 way, if we need to expand the argument again (as in the
	 flag_errno_math case below where we cannot directly set
	 errno), we will not perform side-effects more than once.
	 Note that here we're mutating the original EXP as well as the
	 copy; that's the right thing to do in case the original EXP
	 is expanded later.  */
      TREE_VALUE (arglist) = save_expr (TREE_VALUE (arglist));
      arglist = copy_node (arglist);
    }
  op0 = expand_expr (TREE_VALUE (arglist), subtarget, VOIDmode, 0);

  /* Make a suitable register to place result in.  */
  target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));

  emit_queue ();
  start_sequence ();

  switch (DECL_FUNCTION_CODE (fndecl))
    {
     case BUILT_IN_SIN:
      builtin_optab = sin_optab; break;
     case BUILT_IN_COS:
      builtin_optab = cos_optab; break;
     case BUILT_IN_FSQRT:
      builtin_optab = sqrt_optab; break;
     default:
      abort ();
    }

  /* Compute into TARGET.
     Set TARGET to wherever the result comes back.  */
  target = expand_unop (TYPE_MODE (TREE_TYPE (TREE_VALUE (arglist))),
			builtin_optab, op0, target, 0);

  /* If we were unable to expand via the builtin, stop the
     sequence (without outputting the insns) and return 0, causing
     a call to the library function.  */
  if (target == 0)
    {
      end_sequence ();
      return 0;
    }

  /* Check the results by default.  But if flag_fast_math is turned on,
     then assume sqrt will always be called with valid arguments.  */

  if (flag_errno_math && ! flag_fast_math)
    {
      rtx lab1;

      /* Don't define the builtin FP instructions
	 if your machine is not IEEE.  */
      if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT)
	abort ();

      lab1 = gen_label_rtx ();

      /* Test the result; if it is NaN, set errno=EDOM because
	 the argument was not in the domain.  */
      emit_cmp_and_jump_insns (target, target, EQ, 0, GET_MODE (target),
			       0, 0, lab1);

#ifdef TARGET_EDOM
	{
#ifdef GEN_ERRNO_RTX
	  rtx errno_rtx = GEN_ERRNO_RTX;
#else
	  rtx errno_rtx
	    = gen_rtx_MEM (word_mode, gen_rtx_SYMBOL_REF (Pmode, "errno"));
#endif

	  emit_move_insn (errno_rtx, GEN_INT (TARGET_EDOM));
	}
#else
      /* We can't set errno=EDOM directly; let the library call do it.
	 Pop the arguments right away in case the call gets deleted.  */
      NO_DEFER_POP;
      expand_call (exp, target, 0);
      OK_DEFER_POP;
#endif

      emit_label (lab1);
    }

  /* Output the entire sequence.  */
  insns = get_insns ();
  end_sequence ();
  emit_insns (insns);
 
  return target;
}

/* Expand expression EXP which is a call to the strlen builtin.  Return 0
   if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient (and in mode MODE if that's
   convenient).  */
static rtx
expand_builtin_strlen (exp, target, mode)
     tree exp;
     rtx target;
     enum machine_mode mode;
{
  tree arglist = TREE_OPERAND (exp, 1);
  enum machine_mode value_mode = TYPE_MODE (TREE_TYPE (exp));

  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
    return 0;
  else
    {
      rtx pat;
      tree src = TREE_VALUE (arglist);
      tree len = c_strlen (src);

      int align
	= get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;

      rtx result, src_reg, char_rtx, before_strlen;
      enum machine_mode insn_mode = value_mode, char_mode;
      enum insn_code icode = CODE_FOR_nothing;

      /* If the length is known, just return it.  */
      if (len != 0)
	return expand_expr (len, target, mode, EXPAND_MEMORY_USE_BAD);

      /* If SRC is not a pointer type, don't do this operation inline.  */
      if (align == 0)
	return 0;

      /* Bail out if we can't compute strlen in the right mode.  */
      while (insn_mode != VOIDmode)
	{
	  icode = strlen_optab->handlers[(int) insn_mode].insn_code;
	  if (icode != CODE_FOR_nothing)
	    break;

	  insn_mode = GET_MODE_WIDER_MODE (insn_mode);
	}
      if (insn_mode == VOIDmode)
	return 0;

      /* Make a place to write the result of the instruction.  */
      result = target;
      if (! (result != 0
	     && GET_CODE (result) == REG
	     && GET_MODE (result) == insn_mode
	     && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	result = gen_reg_rtx (insn_mode);

      /* Make a place to hold the source address.  We will not expand
	 the actual source until we are sure that the expansion will
	 not fail -- there are trees that cannot be expanded twice.  */
      src_reg = gen_reg_rtx (Pmode);

      /* Mark the beginning of the strlen sequence so we can emit the
	 source operand later.  */
      before_strlen = get_last_insn();

      /* Check the string is readable and has an end.  */
      if (current_function_check_memory_usage)
	emit_library_call (chkr_check_str_libfunc, 1, VOIDmode, 2,
			   src_reg, Pmode,
			   GEN_INT (MEMORY_USE_RO),
			   TYPE_MODE (integer_type_node));

      char_rtx = const0_rtx;
      char_mode = insn_data[(int)icode].operand[2].mode;
      if (! (*insn_data[(int)icode].operand[2].predicate) (char_rtx, char_mode))
	char_rtx = copy_to_mode_reg (char_mode, char_rtx);

      pat = GEN_FCN (icode) (result, gen_rtx_MEM (BLKmode, src_reg),
			     char_rtx, GEN_INT (align));
      if (! pat)
	return 0;
      emit_insn (pat);

      /* Now that we are assured of success, expand the source.  */
      start_sequence ();
      pat = expand_expr (src, src_reg, ptr_mode, EXPAND_SUM);
      if (pat != src_reg)
	emit_move_insn (src_reg, pat);
      pat = gen_sequence ();
      end_sequence ();

      if (before_strlen)
	emit_insn_after (pat, before_strlen);
      else
	emit_insn_before (pat, get_insns ());

      /* Return the value in the proper mode for this function.  */
      if (GET_MODE (result) == value_mode)
	target = result;
      else if (target != 0)
	convert_move (target, result, 0);
      else
	target = convert_to_mode (value_mode, result, 0);

      return target;
    }
}

/* Expand a call to the memcpy builtin, with arguments in ARGLIST.  */
static rtx
expand_builtin_memcpy (arglist)
     tree arglist;
{
  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
      || TREE_CHAIN (arglist) == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  != POINTER_TYPE)
      || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE
				(TREE_CHAIN (TREE_CHAIN (arglist)))))
	  != INTEGER_TYPE))
    return 0;
  else
    {
      tree dest = TREE_VALUE (arglist);
      tree src = TREE_VALUE (TREE_CHAIN (arglist));
      tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

      int src_align
	= get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
      int dest_align
	= get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
      rtx dest_mem, src_mem, dest_addr, len_rtx;

      /* If either SRC or DEST is not a pointer type, don't do
	 this operation in-line.  */
      if (src_align == 0 || dest_align == 0)
	return 0;

      dest_mem = get_memory_rtx (dest);
      src_mem = get_memory_rtx (src);
      len_rtx = expand_expr (len, NULL_RTX, VOIDmode, 0);

      /* Just copy the rights of SRC to the rights of DEST.  */
      if (current_function_check_memory_usage)
	emit_library_call (chkr_copy_bitmap_libfunc, 1, VOIDmode, 3,
			   XEXP (dest_mem, 0), Pmode,
			   XEXP (src_mem, 0), Pmode,
			   len_rtx, TYPE_MODE (sizetype));

      /* Copy word part most expediently.  */
      dest_addr
	= emit_block_move (dest_mem, src_mem, len_rtx,
			   MIN (src_align, dest_align));

      if (dest_addr == 0)
	dest_addr = force_operand (XEXP (dest_mem, 0), NULL_RTX);

      return dest_addr;
    }
}

/* Expand expression EXP, which is a call to the strcpy builtin.  Return 0
   if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_strcpy (exp)
     tree exp;
{
  tree arglist = TREE_OPERAND (exp, 1);
  rtx result;

  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
      || TREE_CHAIN (arglist) == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  != POINTER_TYPE))
    return 0;
  else
    {
      tree len = c_strlen (TREE_VALUE (TREE_CHAIN (arglist)));

      if (len == 0)
	return 0;

      len = size_binop (PLUS_EXPR, len, ssize_int (1));
      chainon (arglist, build_tree_list (NULL_TREE, len));
    }

  result = expand_builtin_memcpy (arglist);

  if (! result)
    TREE_CHAIN (TREE_CHAIN (arglist)) = 0;
  return result;
}

/* Expand expression EXP, which is a call to the memset builtin.  Return 0
   if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_memset (exp)
     tree exp;
{
  tree arglist = TREE_OPERAND (exp, 1);

  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
      || TREE_CHAIN (arglist) == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  != INTEGER_TYPE)
      || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
      || (INTEGER_TYPE
	  != (TREE_CODE (TREE_TYPE
			 (TREE_VALUE
			  (TREE_CHAIN (TREE_CHAIN (arglist))))))))
    return 0;
  else
    {
      tree dest = TREE_VALUE (arglist);
      tree val = TREE_VALUE (TREE_CHAIN (arglist));
      tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

      int dest_align
	= get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
      rtx dest_mem, dest_addr, len_rtx;

      /* If DEST is not a pointer type, don't do this 
	 operation in-line.  */
      if (dest_align == 0)
	return 0;

      /* If the arguments have side-effects, then we can only evaluate
	 them at most once.  The following code evaluates them twice if
	 they are not constants because we break out to expand_call
	 in that case.  They can't be constants if they have side-effects
	 so we can check for that first.  Alternatively, we could call
	 save_expr to make multiple evaluation safe.  */
      if (TREE_SIDE_EFFECTS (val) || TREE_SIDE_EFFECTS (len))
	return 0;

      /* If VAL is not 0, don't do this operation in-line. */
      if (expand_expr (val, NULL_RTX, VOIDmode, 0) != const0_rtx)
	return 0;

      len_rtx = expand_expr (len, NULL_RTX, VOIDmode, 0);

      dest_mem = get_memory_rtx (dest);
	   
      /* Just check DST is writable and mark it as readable.  */
      if (current_function_check_memory_usage)
	emit_library_call (chkr_check_addr_libfunc, 1, VOIDmode, 3,
			   XEXP (dest_mem, 0), Pmode,
			   len_rtx, TYPE_MODE (sizetype),
			   GEN_INT (MEMORY_USE_WO),
			   TYPE_MODE (integer_type_node));


      dest_addr = clear_storage (dest_mem, len_rtx, dest_align);

      if (dest_addr == 0)
	dest_addr = force_operand (XEXP (dest_mem, 0), NULL_RTX);

      return dest_addr;
    }
}

/* Expand expression EXP, which is a call to the bzero builtin.  Return 0
   if we failed the caller should emit a normal call.  */
static rtx
expand_builtin_bzero (exp)
     tree exp;
{
  tree arglist = TREE_OPERAND (exp, 1);
  tree dest, size, newarglist;
  rtx result;

  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (dest = TREE_VALUE (arglist))) != POINTER_TYPE
      || TREE_CHAIN (arglist) == 0
      || (TREE_CODE (TREE_TYPE (size = TREE_VALUE (TREE_CHAIN (arglist))))
	  != INTEGER_TYPE))
    return NULL_RTX;

  /* New argument list transforming bzero(ptr x, int y) to
     memset(ptr x, int 0, size_t y).  */
  
  newarglist = build_tree_list (NULL_TREE, convert (sizetype, size));
  newarglist = tree_cons (NULL_TREE, integer_zero_node, newarglist);
  newarglist = tree_cons (NULL_TREE, dest, newarglist);

  TREE_OPERAND (exp, 1) = newarglist;
  result = expand_builtin_memset(exp);
      
  /* Always restore the original arguments.  */
  TREE_OPERAND (exp, 1) = arglist;

  return result;
}

#ifdef HAVE_cmpstrsi
/* Expand expression EXP, which is a call to the memcmp or the strcmp builtin.
   ARGLIST is the argument list for this call.  Return 0 if we failed and the
   caller should emit a normal call, otherwise try to get the result in
   TARGET, if convenient.  */
static rtx
expand_builtin_memcmp (exp, arglist, target)
     tree exp;
     tree arglist;
     rtx target;
{
  /* If we need to check memory accesses, call the library function.  */
  if (current_function_check_memory_usage)
    return 0;

  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
      || TREE_CHAIN (arglist) == 0
      || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE
      || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
      || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))))) != INTEGER_TYPE)
    return 0;
  else if (!HAVE_cmpstrsi)
    return 0;

  {
    enum machine_mode mode;
    tree arg1 = TREE_VALUE (arglist);
    tree arg2 = TREE_VALUE (TREE_CHAIN (arglist));
    tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
    rtx result;

    int arg1_align
      = get_pointer_alignment (arg1, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
    int arg2_align
      = get_pointer_alignment (arg2, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
    enum machine_mode insn_mode
      = insn_data[(int) CODE_FOR_cmpstrsi].operand[0].mode;

    /* If we don't have POINTER_TYPE, call the function.  */
    if (arg1_align == 0 || arg2_align == 0)
      return 0;

    /* Make a place to write the result of the instruction.  */
    result = target;
    if (! (result != 0
	   && GET_CODE (result) == REG && GET_MODE (result) == insn_mode
	   && REGNO (result) >= FIRST_PSEUDO_REGISTER))
      result = gen_reg_rtx (insn_mode);

    emit_insn (gen_cmpstrsi (result, get_memory_rtx (arg1),
			     get_memory_rtx (arg2),
			     expand_expr (len, NULL_RTX, VOIDmode, 0),
			     GEN_INT (MIN (arg1_align, arg2_align))));

    /* Return the value in the proper mode for this function.  */
    mode = TYPE_MODE (TREE_TYPE (exp));
    if (GET_MODE (result) == mode)
      return result;
    else if (target != 0)
      {
	convert_move (target, result, 0);
	return target;
      }
    else
      return convert_to_mode (mode, result, 0);
  }
}

/* Expand expression EXP, which is a call to the strcmp builtin.  Return 0
   if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strcmp (exp, target)
     tree exp;
     rtx target;
{
  tree arglist = TREE_OPERAND (exp, 1);

  /* If we need to check memory accesses, call the library function.  */
  if (current_function_check_memory_usage)
    return 0;

  if (arglist == 0
      /* Arg could be non-pointer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
      || TREE_CHAIN (arglist) == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist))))
	  != POINTER_TYPE))
    return 0;

  else if (! HAVE_cmpstrsi)
    return 0;
  {
    tree arg1 = TREE_VALUE (arglist);
    tree arg2 = TREE_VALUE (TREE_CHAIN (arglist));
    tree len = c_strlen (arg1);
    tree len2 = c_strlen (arg2);
    rtx result;

    if (len)
      len = size_binop (PLUS_EXPR, ssize_int (1), len);

    if (len2)
      len2 = size_binop (PLUS_EXPR, ssize_int (1), len2);

    /* If we don't have a constant length for the first, use the length
       of the second, if we know it.  We don't require a constant for
       this case; some cost analysis could be done if both are available
       but neither is constant.  For now, assume they're equally cheap.

       If both strings have constant lengths, use the smaller.  This
       could arise if optimization results in strcpy being called with
       two fixed strings, or if the code was machine-generated.  We should
       add some code to the `memcmp' handler below to deal with such
       situations, someday.  */

    if (!len || TREE_CODE (len) != INTEGER_CST)
      {
	if (len2)
	  len = len2;
	else if (len == 0)
	  return 0;
      }
    else if (len2 && TREE_CODE (len2) == INTEGER_CST
	     && tree_int_cst_lt (len2, len))
      len = len2;

    chainon (arglist, build_tree_list (NULL_TREE, len));
    result = expand_builtin_memcmp (exp, arglist, target);
    if (! result)
      TREE_CHAIN (TREE_CHAIN (arglist)) = 0;

    return result;
  }
}
#endif

/* Expand a call to __builtin_saveregs, generating the result in TARGET,
   if that's convenient.  */

rtx
expand_builtin_saveregs ()
{
  rtx val, seq;

  /* Don't do __builtin_saveregs more than once in a function.
     Save the result of the first call and reuse it.  */
  if (saveregs_value != 0)
    return saveregs_value;

  /* When this function is called, it means that registers must be
     saved on entry to this function.  So we migrate the call to the
     first insn of this function.  */

  start_sequence ();

#ifdef EXPAND_BUILTIN_SAVEREGS
  /* Do whatever the machine needs done in this case.  */
  val = EXPAND_BUILTIN_SAVEREGS ();
#else
  /* ??? We used to try and build up a call to the out of line function,
     guessing about what registers needed saving etc.  This became much
     harder with __builtin_va_start, since we don't have a tree for a
     call to __builtin_saveregs to fall back on.  There was exactly one
     port (i860) that used this code, and I'm unconvinced it could actually
     handle the general case.  So we no longer try to handle anything
     weird and make the backend absorb the evil.  */

  error ("__builtin_saveregs not supported by this target");
  val = const0_rtx;
#endif

  seq = get_insns ();
  end_sequence ();

  saveregs_value = val;

  /* Put the sequence after the NOTE that starts the function.  If this
     is inside a SEQUENCE, make the outer-level insn chain current, so
     the code is placed at the start of the function.  */
  push_topmost_sequence ();
  emit_insns_after (seq, get_insns ());
  pop_topmost_sequence ();

  return val;
}

/* __builtin_args_info (N) returns word N of the arg space info
   for the current function.  The number and meanings of words
   is controlled by the definition of CUMULATIVE_ARGS.  */
static rtx
expand_builtin_args_info (exp)
     tree exp;
{
  tree arglist = TREE_OPERAND (exp, 1);
  int nwords = sizeof (CUMULATIVE_ARGS) / sizeof (int);
  int *word_ptr = (int *) &current_function_args_info;
#if 0	
  /* These are used by the code below that is if 0'ed away */
  int i;
  tree type, elts, result;
#endif

  if (sizeof (CUMULATIVE_ARGS) % sizeof (int) != 0)
    abort ();

  if (arglist != 0)
    {
      tree arg = TREE_VALUE (arglist);
      if (TREE_CODE (arg) != INTEGER_CST)
	error ("argument of `__builtin_args_info' must be constant");
      else
	{
	  int wordnum = TREE_INT_CST_LOW (arg);

	  if (wordnum < 0 || wordnum >= nwords || TREE_INT_CST_HIGH (arg))
	    error ("argument of `__builtin_args_info' out of range");
	  else
	    return GEN_INT (word_ptr[wordnum]);
	}
    }
  else
    error ("missing argument in `__builtin_args_info'");

  return const0_rtx;

#if 0
  for (i = 0; i < nwords; i++)
    elts = tree_cons (NULL_TREE, build_int_2 (word_ptr[i], 0));

  type = build_array_type (integer_type_node,
			   build_index_type (build_int_2 (nwords, 0)));
  result = build (CONSTRUCTOR, type, NULL_TREE, nreverse (elts));
  TREE_CONSTANT (result) = 1;
  TREE_STATIC (result) = 1;
  result = build1 (INDIRECT_REF, build_pointer_type (type), result);
  TREE_CONSTANT (result) = 1;
  return expand_expr (result, NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_BAD);
#endif
}

/* Expand ARGLIST, from a call to __builtin_next_arg.  */
static rtx
expand_builtin_next_arg (arglist)
     tree arglist;
{
  tree fntype = TREE_TYPE (current_function_decl);

  if ((TYPE_ARG_TYPES (fntype) == 0
       || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	   == void_type_node))
      && ! current_function_varargs)
    {
      error ("`va_start' used in function with fixed args");
      return const0_rtx;
    }

  if (arglist)
    {
      tree last_parm = tree_last (DECL_ARGUMENTS (current_function_decl));
      tree arg = TREE_VALUE (arglist);

      /* Strip off all nops for the sake of the comparison.  This
	 is not quite the same as STRIP_NOPS.  It does more.  
	 We must also strip off INDIRECT_EXPR for C++ reference
	 parameters.  */
      while (TREE_CODE (arg) == NOP_EXPR
	     || TREE_CODE (arg) == CONVERT_EXPR
	     || TREE_CODE (arg) == NON_LVALUE_EXPR
	     || TREE_CODE (arg) == INDIRECT_REF)
	arg = TREE_OPERAND (arg, 0);
      if (arg != last_parm)
	warning ("second parameter of `va_start' not last named argument");
    }
  else if (! current_function_varargs)
    /* Evidently an out of date version of <stdarg.h>; can't validate
       va_start's second argument, but can still work as intended.  */
    warning ("`__builtin_next_arg' called without an argument");

  return expand_binop (Pmode, add_optab,
		       current_function_internal_arg_pointer,
		       current_function_arg_offset_rtx,
		       NULL_RTX, 0, OPTAB_LIB_WIDEN);
}

/* Make it easier for the backends by protecting the valist argument
   from multiple evaluations.  */

static tree
stabilize_va_list (valist, was_ptr)
     tree valist;
     int was_ptr;
{
  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      /* If stdarg.h took the address of an array-type valist that was passed
         as a parameter, we'll have taken the address of the parameter itself
         rather than the array as we'd intended.  Undo this mistake.  */

      if (was_ptr)
	{
	  STRIP_NOPS (valist);

	  /* Two cases: either &array, which decomposed to 
	        <ptr <array <record> valist>>
	     or &ptr, which turned into
		<ptr <ptr <record>>>
	     In the first case we'll need to put the ADDR_EXPR back
	     after frobbing the types as if &array[0].  */

	  if (TREE_CODE (valist) != ADDR_EXPR)
	    abort ();
	  valist = TREE_OPERAND (valist, 0);
	}

      if (TYPE_MAIN_VARIANT (TREE_TYPE (valist))
	  == TYPE_MAIN_VARIANT (va_list_type_node))
	{
	  tree pt = build_pointer_type (TREE_TYPE (va_list_type_node));
	  valist = build1 (ADDR_EXPR, pt, valist);
	  TREE_SIDE_EFFECTS (valist)
	    = TREE_SIDE_EFFECTS (TREE_OPERAND (valist, 0));
	}
      else
	{
	  if (! POINTER_TYPE_P (TREE_TYPE (valist))
	      || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (valist)))
		  != TYPE_MAIN_VARIANT (TREE_TYPE (va_list_type_node))))
	    abort ();
	}

      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);
    }
  else
    {
      if (! was_ptr)
	{
	  tree pt;

	  if (! TREE_SIDE_EFFECTS (valist))
	    return valist;

	  pt = build_pointer_type (va_list_type_node);
          valist = fold (build1 (ADDR_EXPR, pt, valist));
	  TREE_SIDE_EFFECTS (valist) = 1;
	}
      if (TREE_SIDE_EFFECTS (valist))
        valist = save_expr (valist);
      valist = fold (build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)),
			     valist));
    }

  return valist;
}

/* The "standard" implementation of va_start: just assign `nextarg' to
   the variable.  */
void
std_expand_builtin_va_start (stdarg_p, valist, nextarg)
     int stdarg_p ATTRIBUTE_UNUSED;
     tree valist;
     rtx nextarg;
{
  tree t;

  if (!stdarg_p)
    nextarg = plus_constant (nextarg, -UNITS_PER_WORD);

  t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
	     make_tree (ptr_type_node, nextarg));
  TREE_SIDE_EFFECTS (t) = 1;

  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Expand ARGLIST, which from a call to __builtin_stdarg_va_start or
   __builtin_varargs_va_start, depending on STDARG_P.  */
static rtx
expand_builtin_va_start (stdarg_p, arglist)
     int stdarg_p;
     tree arglist;
{
  rtx nextarg;
  tree chain = arglist, valist;

  if (stdarg_p)
    nextarg = expand_builtin_next_arg (chain = TREE_CHAIN (arglist));
  else
    nextarg = expand_builtin_next_arg (NULL_TREE);

  if (TREE_CHAIN (chain))
    error ("too many arguments to function `va_start'");

  valist = stabilize_va_list (TREE_VALUE (arglist), 1);

#ifdef EXPAND_BUILTIN_VA_START
  EXPAND_BUILTIN_VA_START (stdarg_p, valist, nextarg);
#else
  std_expand_builtin_va_start (stdarg_p, valist, nextarg);
#endif

  return const0_rtx;
}

/* Allocate an alias set for use in storing and reading from the varargs
   spill area.  */
int
get_varargs_alias_set ()
{
  static int set = -1;
  if (set == -1)
    set = new_alias_set ();
  return set;
}

/* The "standard" implementation of va_arg: read the value from the
   current (padded) address and increment by the (padded) size.  */
rtx
std_expand_builtin_va_arg (valist, type)
     tree valist, type;
{
  tree addr_tree, t;
  HOST_WIDE_INT align;
  HOST_WIDE_INT rounded_size;
  rtx addr;

  /* Compute the rounded size of the type.  */
  align = PARM_BOUNDARY / BITS_PER_UNIT;
  rounded_size = (((int_size_in_bytes (type) + align - 1) / align) * align);

  /* Get AP.  */
  addr_tree = valist;
  if (PAD_VARARGS_DOWN)
    {
      /* Small args are padded downward.  */

      HOST_WIDE_INT adj;
      adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
      if (rounded_size > align)
	adj = rounded_size;

      addr_tree = build (PLUS_EXPR, TREE_TYPE (addr_tree), addr_tree,
			 build_int_2 (rounded_size - adj, 0));
    }

  addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
  addr = copy_to_reg (addr);

  /* Compute new value for AP.  */
  t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
	     build (PLUS_EXPR, TREE_TYPE (valist), valist,
		    build_int_2 (rounded_size, 0)));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return addr;
}

/* Expand __builtin_va_arg, which is not really a builtin function, but
   a very special sort of operator.  */
rtx
expand_builtin_va_arg (valist, type)
     tree valist, type;
{
  rtx addr, result;
  tree promoted_type, want_va_type, have_va_type;

  /* Verify that valist is of the proper type.  */

  want_va_type = va_list_type_node;
  have_va_type = TREE_TYPE (valist);
  if (TREE_CODE (want_va_type) == ARRAY_TYPE)
    {
      /* If va_list is an array type, the argument may have decayed 
	 to a pointer type, e.g. by being passed to another function.
         In that case, unwrap both types so that we can compare the
	 underlying records.  */
      if (TREE_CODE (have_va_type) == ARRAY_TYPE
	  || TREE_CODE (have_va_type) == POINTER_TYPE)
	{
	  want_va_type = TREE_TYPE (want_va_type);
	  have_va_type = TREE_TYPE (have_va_type);
	}
    }
  if (TYPE_MAIN_VARIANT (want_va_type) != TYPE_MAIN_VARIANT (have_va_type))
    {
      error ("first argument to `va_arg' not of type `va_list'");
      addr = const0_rtx;
    }

  /* Generate a diagnostic for requesting data of a type that cannot
     be passed through `...' due to type promotion at the call site.  */
  else if ((promoted_type = (*lang_type_promotes_to) (type)) != NULL_TREE)
    {
      const char *name = "<anonymous type>", *pname = 0;
      static int gave_help;

      if (TYPE_NAME (type))
	{
	  if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	    name = IDENTIFIER_POINTER (TYPE_NAME (type));
	  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (type)))
	    name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	}
      if (TYPE_NAME (promoted_type))
	{
	  if (TREE_CODE (TYPE_NAME (promoted_type)) == IDENTIFIER_NODE)
	    pname = IDENTIFIER_POINTER (TYPE_NAME (promoted_type));
	  else if (TREE_CODE (TYPE_NAME (promoted_type)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (promoted_type)))
	    pname = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (promoted_type)));
	}

      error ("`%s' is promoted to `%s' when passed through `...'", name, pname);
      if (! gave_help)
	{
	  gave_help = 1;
	  error ("(so you should pass `%s' not `%s' to `va_arg')", pname, name);
	}

      addr = const0_rtx;
    }
  else
    {
      /* Make it easier for the backends by protecting the valist argument
         from multiple evaluations.  */
      valist = stabilize_va_list (valist, 0);

#ifdef EXPAND_BUILTIN_VA_ARG
      addr = EXPAND_BUILTIN_VA_ARG (valist, type);
#else
      addr = std_expand_builtin_va_arg (valist, type);
#endif
    }

  result = gen_rtx_MEM (TYPE_MODE (type), addr);
  MEM_ALIAS_SET (result) = get_varargs_alias_set ();

  return result;
}

/* Expand ARGLIST, from a call to __builtin_va_end.  */
static rtx
expand_builtin_va_end (arglist)
     tree arglist;
{
  tree valist = TREE_VALUE (arglist);

#ifdef EXPAND_BUILTIN_VA_END
  valist = stabilize_va_list (valist, 0);
  EXPAND_BUILTIN_VA_END(arglist);
#else
  /* Evaluate for side effects, if needed.  I hate macros that don't
     do that.  */
  if (TREE_SIDE_EFFECTS (valist))
    expand_expr (valist, const0_rtx, VOIDmode, EXPAND_NORMAL);
#endif

  return const0_rtx;
}

/* Expand ARGLIST, from a call to __builtin_va_copy.  We do this as a 
   builtin rather than just as an assignment in stdarg.h because of the
   nastiness of array-type va_list types.  */
static rtx
expand_builtin_va_copy (arglist)
     tree arglist;
{
  tree dst, src, t;

  dst = TREE_VALUE (arglist);
  src = TREE_VALUE (TREE_CHAIN (arglist));

  dst = stabilize_va_list (dst, 1);
  src = stabilize_va_list (src, 0);

  if (TREE_CODE (va_list_type_node) != ARRAY_TYPE)
    {
      t = build (MODIFY_EXPR, va_list_type_node, dst, src);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
  else
    {
      rtx dstb, srcb, size;

      /* Evaluate to pointers.  */
      dstb = expand_expr (dst, NULL_RTX, Pmode, EXPAND_NORMAL);
      srcb = expand_expr (src, NULL_RTX, Pmode, EXPAND_NORMAL);
      size = expand_expr (TYPE_SIZE_UNIT (va_list_type_node), NULL_RTX,
			  VOIDmode, EXPAND_NORMAL);

      /* "Dereference" to BLKmode memories.  */
      dstb = gen_rtx_MEM (BLKmode, dstb);
      MEM_ALIAS_SET (dstb) = get_alias_set (TREE_TYPE (TREE_TYPE (dst)));
      srcb = gen_rtx_MEM (BLKmode, srcb);
      MEM_ALIAS_SET (srcb) = get_alias_set (TREE_TYPE (TREE_TYPE (src)));

      /* Copy.  */
      emit_block_move (dstb, srcb, size, 
		       TYPE_ALIGN (va_list_type_node) / BITS_PER_UNIT);
    }

  return const0_rtx;
}

/* Expand a call to one of the builtin functions __builtin_frame_address or
   __builtin_return_address.  */
static rtx
expand_builtin_frame_address (exp)
     tree exp;
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);

  /* The argument must be a nonnegative integer constant.
     It counts the number of frames to scan up the stack.
     The value is the return address saved in that frame.  */
  if (arglist == 0)
    /* Warning about missing arg was already issued.  */
    return const0_rtx;
  else if (TREE_CODE (TREE_VALUE (arglist)) != INTEGER_CST
	   || tree_int_cst_sgn (TREE_VALUE (arglist)) < 0)
    {
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	error ("invalid arg to `__builtin_frame_address'");
      else
	error ("invalid arg to `__builtin_return_address'");
      return const0_rtx;
    }
  else
    {
      rtx tem = expand_builtin_return_addr (DECL_FUNCTION_CODE (fndecl),
					    TREE_INT_CST_LOW (TREE_VALUE (arglist)),
					    hard_frame_pointer_rtx);

      /* Some ports cannot access arbitrary stack frames.  */
      if (tem == NULL)
	{
	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	    warning ("unsupported arg to `__builtin_frame_address'");
	  else
	    warning ("unsupported arg to `__builtin_return_address'");
	  return const0_rtx;
	}

      /* For __builtin_frame_address, return what we've got.  */
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	return tem;

      if (GET_CODE (tem) != REG
	  && ! CONSTANT_P (tem))
	tem = copy_to_mode_reg (Pmode, tem);
      return tem;
    }
}

/* Expand a call to the alloca builtin, with arguments ARGLIST.  Return 0 if
   we failed and the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */
static rtx
expand_builtin_alloca (arglist, target)
     tree arglist;
     rtx target;
{
  rtx op0;

  if (arglist == 0
      /* Arg could be non-integer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
    return 0;

  /* Compute the argument.  */
  op0 = expand_expr (TREE_VALUE (arglist), NULL_RTX, VOIDmode, 0);

  /* Allocate the desired space.  */
  return allocate_dynamic_stack_space (op0, target, BITS_PER_UNIT);
}

/* Expand a call to the ffs builtin.  The arguments are in ARGLIST.
   Return 0 if a normal call should be emitted rather than expanding the
   function in-line.  If convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */
static rtx
expand_builtin_ffs (arglist, target, subtarget)
     tree arglist;
     rtx target, subtarget;
{
  rtx op0;
  if (arglist == 0
      /* Arg could be non-integer if user redeclared this fcn wrong.  */
      || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
    return 0;

  /* Compute the argument.  */
  op0 = expand_expr (TREE_VALUE (arglist), subtarget, VOIDmode, 0);
  /* Compute ffs, into TARGET if possible.
     Set TARGET to wherever the result comes back.  */
  target = expand_unop (TYPE_MODE (TREE_TYPE (TREE_VALUE (arglist))),
			ffs_optab, op0, target, 1);
  if (target == 0)
    abort ();
  return target;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget;
     enum machine_mode mode;
     int ignore;
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

#ifdef MD_EXPAND_BUILTIN
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return MD_EXPAND_BUILTIN (exp, target, subtarget, mode, ignore);
#endif
  
  /* When not optimizing, generate calls to library functions for a certain
     set of builtins.  */
  if (! optimize && ! CALLED_AS_BUILT_IN (fndecl)
      && (fcode == BUILT_IN_SIN || fcode == BUILT_IN_COS
	  || fcode == BUILT_IN_FSQRT || fcode == BUILT_IN_MEMSET
	  || fcode == BUILT_IN_MEMCPY || fcode == BUILT_IN_MEMCMP
	  || fcode == BUILT_IN_BCMP || fcode == BUILT_IN_BZERO
	  || fcode == BUILT_IN_STRLEN || fcode == BUILT_IN_STRCPY
	  || fcode == BUILT_IN_STRCMP || fcode == BUILT_IN_FFS))
    return expand_call (exp, target, ignore);

  switch (fcode)
    {
    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_FABS:
      /* build_function_call changes these into ABS_EXPR.  */
      abort ();

    case BUILT_IN_SIN:
    case BUILT_IN_COS:
      /* Treat these like sqrt, but only if the user asks for them.  */
      if (! flag_fast_math)
	break;
    case BUILT_IN_FSQRT:
      target = expand_builtin_mathfn (exp, target, subtarget);
      if (target)
	return target;
      break;

    case BUILT_IN_FMOD:
      break;

    case BUILT_IN_APPLY_ARGS:
      return expand_builtin_apply_args ();

      /* __builtin_apply (FUNCTION, ARGUMENTS, ARGSIZE) invokes
	 FUNCTION with a copy of the parameters described by
	 ARGUMENTS, and ARGSIZE.  It returns a block of memory
	 allocated on the stack into which is stored all the registers
	 that might possibly be used for returning the result of a
	 function.  ARGUMENTS is the value returned by
	 __builtin_apply_args.  ARGSIZE is the number of bytes of
	 arguments that must be copied.  ??? How should this value be
	 computed?  We'll also need a safe worst case value for varargs
	 functions.  */
    case BUILT_IN_APPLY:
      if (arglist == 0
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  || ! POINTER_TYPE_P (TREE_TYPE (TREE_VALUE (arglist)))
	  || TREE_CHAIN (arglist) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE
	  || TREE_CHAIN (TREE_CHAIN (arglist)) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))))) != INTEGER_TYPE)
	return const0_rtx;
      else
	{
	  int i;
	  tree t;
	  rtx ops[3];

	  for (t = arglist, i = 0; t; t = TREE_CHAIN (t), i++)
	    ops[i] = expand_expr (TREE_VALUE (t), NULL_RTX, VOIDmode, 0);

	  return expand_builtin_apply (ops[0], ops[1], ops[2]);
	}

      /* __builtin_return (RESULT) causes the function to return the
	 value described by RESULT.  RESULT is address of the block of
	 memory returned by __builtin_apply.  */
    case BUILT_IN_RETURN:
      if (arglist
	  /* Arg could be non-pointer if user redeclared this fcn wrong.  */
	  && TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) == POINTER_TYPE)
	expand_builtin_return (expand_expr (TREE_VALUE (arglist),
					    NULL_RTX, VOIDmode, 0));
      return const0_rtx;

    case BUILT_IN_SAVEREGS:
      return expand_builtin_saveregs ();

    case BUILT_IN_ARGS_INFO:
      return expand_builtin_args_info (exp);

      /* Return the address of the first anonymous stack arg.  */
    case BUILT_IN_NEXT_ARG:
      return expand_builtin_next_arg (arglist);

    case BUILT_IN_CLASSIFY_TYPE:
      return expand_builtin_classify_type (arglist);

    case BUILT_IN_CONSTANT_P:
      return expand_builtin_constant_p (exp);

    case BUILT_IN_FRAME_ADDRESS:
    case BUILT_IN_RETURN_ADDRESS:
      return expand_builtin_frame_address (exp);

    /* Returns the address of the area where the structure is returned.
       0 otherwise.  */
    case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
      if (arglist != 0
          || ! AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl)))
          || GET_CODE (DECL_RTL (DECL_RESULT (current_function_decl))) != MEM)
        return const0_rtx;
      else
        return XEXP (DECL_RTL (DECL_RESULT (current_function_decl)), 0);

    case BUILT_IN_ALLOCA:
      target = expand_builtin_alloca (arglist, target);
      if (target)
	return target;
      break;

    case BUILT_IN_FFS:
      target = expand_builtin_ffs (arglist, target, subtarget);
      if (target)
	return target;
      break;

    case BUILT_IN_STRLEN:
      target = expand_builtin_strlen (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCPY:
      target = expand_builtin_strcpy (exp);
      if (target)
	return target;
      break;
      
    case BUILT_IN_MEMCPY:
      target = expand_builtin_memcpy (arglist);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMSET:
      target = expand_builtin_memset (exp);
      if (target)
	return target;
      break;

    case BUILT_IN_BZERO:
      target = expand_builtin_bzero (exp);
      if (target)
	return target;
      break;

/* These comparison functions need an instruction that returns an actual
   index.  An ordinary compare that just sets the condition codes
   is not enough.  */
#ifdef HAVE_cmpstrsi
    case BUILT_IN_STRCMP:
      target = expand_builtin_strcmp (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      target = expand_builtin_memcmp (exp, arglist, target);
      if (target)
	return target;
      break;
#else
    case BUILT_IN_STRCMP:
    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      break;
#endif

    case BUILT_IN_SETJMP:
      if (arglist == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
	break;
      else
	{
	  rtx buf_addr = expand_expr (TREE_VALUE (arglist), subtarget,
				      VOIDmode, 0);
	  rtx lab = gen_label_rtx ();
	  rtx ret = expand_builtin_setjmp (buf_addr, target, lab, lab);
	  emit_label (lab);
	  return ret;
	}

      /* __builtin_longjmp is passed a pointer to an array of five words.
	 It's similar to the C library longjmp function but works with
	 __builtin_setjmp above.  */
    case BUILT_IN_LONGJMP:
      if (arglist == 0 || TREE_CHAIN (arglist) == 0
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
	break;
      else
	{
	  rtx buf_addr = expand_expr (TREE_VALUE (arglist), subtarget,
				      VOIDmode, 0);
	  rtx value = expand_expr (TREE_VALUE (TREE_CHAIN (arglist)),
				   NULL_RTX, VOIDmode, 0);

	  if (value != const1_rtx)
	    {
	      error ("__builtin_longjmp second argument must be 1");
	      return const0_rtx;
	    }

	  expand_builtin_longjmp (buf_addr, value);
	  return const0_rtx;
	}

    case BUILT_IN_TRAP:
#ifdef HAVE_trap
      if (HAVE_trap)
	emit_insn (gen_trap ());
      else
#endif
	error ("__builtin_trap not supported by this target");
      emit_barrier ();
      return const0_rtx;

      /* Various hooks for the DWARF 2 __throw routine.  */
    case BUILT_IN_UNWIND_INIT:
      expand_builtin_unwind_init ();
      return const0_rtx;
    case BUILT_IN_DWARF_CFA:
      return virtual_cfa_rtx;
#ifdef DWARF2_UNWIND_INFO
    case BUILT_IN_DWARF_FP_REGNUM:
      return expand_builtin_dwarf_fp_regnum ();
    case BUILT_IN_INIT_DWARF_REG_SIZES:
      expand_builtin_init_dwarf_reg_sizes (TREE_VALUE (arglist));
      return const0_rtx;
#endif
    case BUILT_IN_FROB_RETURN_ADDR:
      return expand_builtin_frob_return_addr (TREE_VALUE (arglist));
    case BUILT_IN_EXTRACT_RETURN_ADDR:
      return expand_builtin_extract_return_addr (TREE_VALUE (arglist));
    case BUILT_IN_EH_RETURN:
      expand_builtin_eh_return (TREE_VALUE (arglist),
				TREE_VALUE (TREE_CHAIN (arglist)),
				TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))));
      return const0_rtx;
    case BUILT_IN_VARARGS_START:
      return expand_builtin_va_start (0, arglist);
    case BUILT_IN_STDARG_START:
      return expand_builtin_va_start (1, arglist);
    case BUILT_IN_VA_END:
      return expand_builtin_va_end (arglist);
    case BUILT_IN_VA_COPY:
      return expand_builtin_va_copy (arglist);

    default:			/* just do library call, if unknown builtin */
      error ("built-in function `%s' not currently supported",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */
  return expand_call (exp, target, ignore);
}
