/* Expands front end tree to back end RTL for GNU C-Compiler
   Copyright (C) 1987, 1988, 1989, 1991, 1992 Free Software Foundation, Inc.

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


/* This file handles the generation of rtl code from tree structure
   at the level of the function as a whole.
   It creates the rtl expressions for parameters and auto variables
   and has full responsibility for allocating stack slots.

   `expand_function_start' is called at the beginning of a function,
   before the function body is parsed, and `expand_function_end' is
   called after parsing the body.

   Call `assign_stack_local' to allocate a stack slot for a local variable.
   This is usually done during the RTL generation for the function body,
   but it can also be done in the reload pass when a pseudo-register does
   not get a hard register.

   Call `put_var_into_stack' when you learn, belatedly, that a variable
   previously given a pseudo-register must in fact go in the stack.
   This function changes the DECL_RTL to be a stack slot instead of a reg
   then scans all the RTL instructions so far generated to correct them.  */

#include "config.h"

#include <stdio.h>

#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "insn-flags.h"
#include "expr.h"
#include "insn-codes.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "basic-block.h"

/* Round a value to the lowest integer less than it that is a multiple of
   the required alignment.  Avoid using division in case the value is
   negative.  Assume the alignment is a power of two.  */
#define FLOOR_ROUND(VALUE,ALIGN) ((VALUE) & ~((ALIGN) - 1))

/* Similar, but round to the next highest integer that meets the
   alignment.  */
#define CEIL_ROUND(VALUE,ALIGN)	(((VALUE) + (ALIGN) - 1) & ~((ALIGN)- 1))

/* NEED_SEPARATE_AP means that we cannot derive ap from the value of fp
   during rtl generation.  If they are different register numbers, this is
   always true.  It may also be true if
   FIRST_PARM_OFFSET - STARTING_FRAME_OFFSET is not a constant during rtl
   generation.  See fix_lexical_addr for details.  */

#if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
#define NEED_SEPARATE_AP
#endif

/* Number of bytes of args popped by function being compiled on its return.
   Zero if no bytes are to be popped.
   May affect compilation of return insn or of function epilogue.  */

int current_function_pops_args;

/* Nonzero if function being compiled needs to be given an address
   where the value should be stored.  */

int current_function_returns_struct;

/* Nonzero if function being compiled needs to
   return the address of where it has put a structure value.  */

int current_function_returns_pcc_struct;

/* Nonzero if function being compiled needs to be passed a static chain.  */

int current_function_needs_context;

/* Nonzero if function being compiled can call setjmp.  */

int current_function_calls_setjmp;

/* Nonzero if function being compiled can call longjmp.  */

int current_function_calls_longjmp;

/* Nonzero if function being compiled receives nonlocal gotos
   from nested functions.  */

int current_function_has_nonlocal_label;

/* Nonzero if function being compiled contains nested functions.  */

int current_function_contains_functions;

/* Nonzero if function being compiled can call alloca,
   either as a subroutine or builtin.  */

int current_function_calls_alloca;

/* Nonzero if the current function returns a pointer type */

int current_function_returns_pointer;

/* If some insns can be deferred to the delay slots of the epilogue, the
   delay list for them is recorded here.  */

rtx current_function_epilogue_delay_list;

/* If function's args have a fixed size, this is that size, in bytes.
   Otherwise, it is -1.
   May affect compilation of return insn or of function epilogue.  */

int current_function_args_size;

/* # bytes the prologue should push and pretend that the caller pushed them.
   The prologue must do this, but only if parms can be passed in registers.  */

int current_function_pretend_args_size;

/* # of bytes of outgoing arguments required to be pushed by the prologue.
   If this is non-zero, it means that ACCUMULATE_OUTGOING_ARGS was defined
   and no stack adjusts will be done on function calls.  */

int current_function_outgoing_args_size;

/* This is the offset from the arg pointer to the place where the first
   anonymous arg can be found, if there is one.  */

rtx current_function_arg_offset_rtx;

/* Nonzero if current function uses varargs.h or equivalent.
   Zero for functions that use stdarg.h.  */

int current_function_varargs;

/* Quantities of various kinds of registers
   used for the current function's args.  */

CUMULATIVE_ARGS current_function_args_info;

/* Name of function now being compiled.  */

char *current_function_name;

/* If non-zero, an RTL expression for that location at which the current
   function returns its result.  Always equal to
   DECL_RTL (DECL_RESULT (current_function_decl)), but provided
   independently of the tree structures.  */

rtx current_function_return_rtx;

/* Nonzero if the current function uses the constant pool.  */

int current_function_uses_const_pool;

/* Nonzero if the current function uses pic_offset_table_rtx.  */
int current_function_uses_pic_offset_table;

/* The arg pointer hard register, or the pseudo into which it was copied.  */
rtx current_function_internal_arg_pointer;

/* The FUNCTION_DECL for an inline function currently being expanded.  */
tree inline_function_decl;

/* Number of function calls seen so far in current function.  */

int function_call_count;

/* List (chain of TREE_LIST) of LABEL_DECLs for all nonlocal labels
   (labels to which there can be nonlocal gotos from nested functions)
   in this function.  */

tree nonlocal_labels;

/* RTX for stack slot that holds the current handler for nonlocal gotos.
   Zero when function does not have nonlocal labels.  */

rtx nonlocal_goto_handler_slot;

/* RTX for stack slot that holds the stack pointer value to restore
   for a nonlocal goto.
   Zero when function does not have nonlocal labels.  */

rtx nonlocal_goto_stack_level;

/* Label that will go on parm cleanup code, if any.
   Jumping to this label runs cleanup code for parameters, if
   such code must be run.  Following this code is the logical return label.  */

rtx cleanup_label;

/* Label that will go on function epilogue.
   Jumping to this label serves as a "return" instruction
   on machines which require execution of the epilogue on all returns.  */

rtx return_label;

/* List (chain of EXPR_LISTs) of pseudo-regs of SAVE_EXPRs.
   So we can mark them all live at the end of the function, if nonopt.  */
rtx save_expr_regs;

/* List (chain of EXPR_LISTs) of all stack slots in this function.
   Made for the sake of unshare_all_rtl.  */
rtx stack_slot_list;

/* Chain of all RTL_EXPRs that have insns in them.  */
tree rtl_expr_chain;

/* Label to jump back to for tail recursion, or 0 if we have
   not yet needed one for this function.  */
rtx tail_recursion_label;

/* Place after which to insert the tail_recursion_label if we need one.  */
rtx tail_recursion_reentry;

/* Location at which to save the argument pointer if it will need to be
   referenced.  There are two cases where this is done: if nonlocal gotos
   exist, or if vars stored at an offset from the argument pointer will be
   needed by inner routines.  */

rtx arg_pointer_save_area;

/* Offset to end of allocated area of stack frame.
   If stack grows down, this is the address of the last stack slot allocated.
   If stack grows up, this is the address for the next slot.  */
int frame_offset;

/* List (chain of TREE_LISTs) of static chains for containing functions.
   Each link has a FUNCTION_DECL in the TREE_PURPOSE and a reg rtx
   in an RTL_EXPR in the TREE_VALUE.  */
static tree context_display;

/* List (chain of TREE_LISTs) of trampolines for nested functions.
   The trampoline sets up the static chain and jumps to the function.
   We supply the trampoline's address when the function's address is requested.

   Each link has a FUNCTION_DECL in the TREE_PURPOSE and a reg rtx
   in an RTL_EXPR in the TREE_VALUE.  */
static tree trampoline_list;

/* Insn after which register parms and SAVE_EXPRs are born, if nonopt.  */
static rtx parm_birth_insn;

#if 0
/* Nonzero if a stack slot has been generated whose address is not
   actually valid.  It means that the generated rtl must all be scanned
   to detect and correct the invalid addresses where they occur.  */
static int invalid_stack_slot;
#endif

/* Last insn of those whose job was to put parms into their nominal homes.  */
static rtx last_parm_insn;

/* 1 + last pseudo register number used for loading a copy
   of a parameter of this function.  */
static int max_parm_reg;

/* Vector indexed by REGNO, containing location on stack in which
   to put the parm which is nominally in pseudo register REGNO,
   if we discover that that parm must go in the stack.  */
static rtx *parm_reg_stack_loc;

#if 0  /* Turned off because 0 seems to work just as well.  */
/* Cleanup lists are required for binding levels regardless of whether
   that binding level has cleanups or not.  This node serves as the
   cleanup list whenever an empty list is required.  */
static tree empty_cleanup_list;
#endif

/* Nonzero once virtual register instantiation has been done.
   assign_stack_local uses frame_pointer_rtx when this is nonzero.  */
static int virtuals_instantiated;

/* Nonzero if we need to distinguish between the return value of this function
   and the return value of a function called by this function.  This helps
   integrate.c  */

extern int rtx_equal_function_value_matters;

void fixup_gotos ();

static tree round_down ();
static rtx round_trampoline_addr ();
static rtx fixup_stack_1 ();
static void fixup_var_refs ();
static void fixup_var_refs_insns ();
static void fixup_var_refs_1 ();
static void optimize_bit_field ();
static void instantiate_decls ();
static void instantiate_decls_1 ();
static void instantiate_decl ();
static int instantiate_virtual_regs_1 ();
static rtx fixup_memory_subreg ();
static rtx walk_fixup_memory_subreg ();

/* In order to evaluate some expressions, such as function calls returning
   structures in memory, we need to temporarily allocate stack locations.
   We record each allocated temporary in the following structure.

   Associated with each temporary slot is a nesting level.  When we pop up
   one level, all temporaries associated with the previous level are freed.
   Normally, all temporaries are freed after the execution of the statement
   in which they were created.  However, if we are inside a ({...}) grouping,
   the result may be in a temporary and hence must be preserved.  If the
   result could be in a temporary, we preserve it if we can determine which
   one it is in.  If we cannot determine which temporary may contain the
   result, all temporaries are preserved.  A temporary is preserved by
   pretending it was allocated at the previous nesting level.

   Automatic variables are also assigned temporary slots, at the nesting
   level where they are defined.  They are marked a "kept" so that
   free_temp_slots will not free them.  */

struct temp_slot
{
  /* Points to next temporary slot.  */
  struct temp_slot *next;
  /* The rtx to used to reference the slot. */
  rtx slot;
  /* The size, in units, of the slot.  */
  int size;
  /* Non-zero if this temporary is currently in use.  */
  char in_use;
  /* Nesting level at which this slot is being used.  */
  int level;
  /* Non-zero if this should survive a call to free_temp_slots.  */
  int keep;
};

/* List of all temporaries allocated, both available and in use.  */

struct temp_slot *temp_slots;

/* Current nesting level for temporaries.  */

int temp_slot_level;

/* Pointer to chain of `struct function' for containing functions.  */
struct function *outer_function_chain;

/* Given a function decl for a containing function,
   return the `struct function' for it.  */

struct function *
find_function_data (decl)
     tree decl;
{
  struct function *p;
  for (p = outer_function_chain; p; p = p->next)
    if (p->decl == decl)
      return p;
  abort ();
}

/* Save the current context for compilation of a nested function.
   This is called from language-specific code.
   The caller is responsible for saving any language-specific status,
   since this function knows only about language-independent variables.  */

void
push_function_context ()
{
  struct function *p = (struct function *) xmalloc (sizeof (struct function));

  p->next = outer_function_chain;
  outer_function_chain = p;

  p->name = current_function_name;
  p->decl = current_function_decl;
  p->pops_args = current_function_pops_args;
  p->returns_struct = current_function_returns_struct;
  p->returns_pcc_struct = current_function_returns_pcc_struct;
  p->needs_context = current_function_needs_context;
  p->calls_setjmp = current_function_calls_setjmp;
  p->calls_longjmp = current_function_calls_longjmp;
  p->calls_alloca = current_function_calls_alloca;
  p->has_nonlocal_label = current_function_has_nonlocal_label;
  p->args_size = current_function_args_size;
  p->pretend_args_size = current_function_pretend_args_size;
  p->arg_offset_rtx = current_function_arg_offset_rtx;
  p->uses_const_pool = current_function_uses_const_pool;
  p->uses_pic_offset_table = current_function_uses_pic_offset_table;
  p->internal_arg_pointer = current_function_internal_arg_pointer;
  p->max_parm_reg = max_parm_reg;
  p->parm_reg_stack_loc = parm_reg_stack_loc;
  p->outgoing_args_size = current_function_outgoing_args_size;
  p->return_rtx = current_function_return_rtx;
  p->nonlocal_goto_handler_slot = nonlocal_goto_handler_slot;
  p->nonlocal_goto_stack_level = nonlocal_goto_stack_level;
  p->nonlocal_labels = nonlocal_labels;
  p->cleanup_label = cleanup_label;
  p->return_label = return_label;
  p->save_expr_regs = save_expr_regs;
  p->stack_slot_list = stack_slot_list;
  p->parm_birth_insn = parm_birth_insn;
  p->frame_offset = frame_offset;
  p->tail_recursion_label = tail_recursion_label;
  p->tail_recursion_reentry = tail_recursion_reentry;
  p->arg_pointer_save_area = arg_pointer_save_area;
  p->rtl_expr_chain = rtl_expr_chain;
  p->last_parm_insn = last_parm_insn;
  p->context_display = context_display;
  p->trampoline_list = trampoline_list;
  p->function_call_count = function_call_count;
  p->temp_slots = temp_slots;
  p->temp_slot_level = temp_slot_level;
  p->fixup_var_refs_queue = 0;
  p->epilogue_delay_list = current_function_epilogue_delay_list;

  save_tree_status (p);
  save_storage_status (p);
  save_emit_status (p);
  init_emit ();
  save_expr_status (p);
  save_stmt_status (p);
  save_varasm_status (p);
}

/* Restore the last saved context, at the end of a nested function.
   This function is called from language-specific code.  */

void
pop_function_context ()
{
  struct function *p = outer_function_chain;

  outer_function_chain = p->next;

  current_function_name = p->name;
  current_function_decl = p->decl;
  current_function_pops_args = p->pops_args;
  current_function_returns_struct = p->returns_struct;
  current_function_returns_pcc_struct = p->returns_pcc_struct;
  current_function_needs_context = p->needs_context;
  current_function_calls_setjmp = p->calls_setjmp;
  current_function_calls_longjmp = p->calls_longjmp;
  current_function_calls_alloca = p->calls_alloca;
  current_function_has_nonlocal_label = p->has_nonlocal_label;
  current_function_contains_functions = 1;
  current_function_args_size = p->args_size;
  current_function_pretend_args_size = p->pretend_args_size;
  current_function_arg_offset_rtx = p->arg_offset_rtx;
  current_function_uses_const_pool = p->uses_const_pool;
  current_function_uses_pic_offset_table = p->uses_pic_offset_table;
  current_function_internal_arg_pointer = p->internal_arg_pointer;
  max_parm_reg = p->max_parm_reg;
  parm_reg_stack_loc = p->parm_reg_stack_loc;
  current_function_outgoing_args_size = p->outgoing_args_size;
  current_function_return_rtx = p->return_rtx;
  nonlocal_goto_handler_slot = p->nonlocal_goto_handler_slot;
  nonlocal_goto_stack_level = p->nonlocal_goto_stack_level;
  nonlocal_labels = p->nonlocal_labels;
  cleanup_label = p->cleanup_label;
  return_label = p->return_label;
  save_expr_regs = p->save_expr_regs;
  stack_slot_list = p->stack_slot_list;
  parm_birth_insn = p->parm_birth_insn;
  frame_offset = p->frame_offset;
  tail_recursion_label = p->tail_recursion_label;
  tail_recursion_reentry = p->tail_recursion_reentry;
  arg_pointer_save_area = p->arg_pointer_save_area;
  rtl_expr_chain = p->rtl_expr_chain;
  last_parm_insn = p->last_parm_insn;
  context_display = p->context_display;
  trampoline_list = p->trampoline_list;
  function_call_count = p->function_call_count;
  temp_slots = p->temp_slots;
  temp_slot_level = p->temp_slot_level;
  current_function_epilogue_delay_list = p->epilogue_delay_list;

  restore_tree_status (p);
  restore_storage_status (p);
  restore_expr_status (p);
  restore_emit_status (p);
  restore_stmt_status (p);
  restore_varasm_status (p);

  /* Finish doing put_var_into_stack for any of our variables
     which became addressable during the nested function.  */
  {
    struct var_refs_queue *queue = p->fixup_var_refs_queue;
    for (; queue; queue = queue->next)
      fixup_var_refs (queue->modified, queue->promoted_mode, queue->unsignedp);
  }

  free (p);

  /* Reset variables that have known state during rtx generation.  */
  rtx_equal_function_value_matters = 1;
  virtuals_instantiated = 0;
}

/* Allocate fixed slots in the stack frame of the current function.  */

/* Return size needed for stack frame based on slots so far allocated.
   This size counts from zero.  It is not rounded to STACK_BOUNDARY;
   the caller may have to do that.  */

int
get_frame_size ()
{
#ifdef FRAME_GROWS_DOWNWARD
  return -frame_offset;
#else
  return frame_offset;
#endif
}

/* Allocate a stack slot of SIZE bytes and return a MEM rtx for it
   with machine mode MODE.
   
   ALIGN controls the amount of alignment for the address of the slot:
   0 means according to MODE,
   -1 means use BIGGEST_ALIGNMENT and round size to multiple of that,
   positive specifies alignment boundary in bits.

   We do not round to stack_boundary here.  */

rtx
assign_stack_local (mode, size, align)
     enum machine_mode mode;
     int size;
     int align;
{
  register rtx x, addr;
  int bigend_correction = 0;
  int alignment;

  if (align == 0)
    {
      alignment = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
      if (mode == BLKmode)
	alignment = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
    }
  else if (align == -1)
    {
      alignment = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
      size = CEIL_ROUND (size, alignment);
    }
  else
    alignment = align / BITS_PER_UNIT;

  /* Round frame offset to that alignment.
     We must be careful here, since FRAME_OFFSET might be negative and
     division with a negative dividend isn't as well defined as we might
     like.  So we instead assume that ALIGNMENT is a power of two and
     use logical operations which are unambiguous.  */
#ifdef FRAME_GROWS_DOWNWARD
  frame_offset = FLOOR_ROUND (frame_offset, alignment);
#else
  frame_offset = CEIL_ROUND (frame_offset, alignment);
#endif

  /* On a big-endian machine, if we are allocating more space than we will use,
     use the least significant bytes of those that are allocated.  */
#if BYTES_BIG_ENDIAN
  if (mode != BLKmode)
    bigend_correction = size - GET_MODE_SIZE (mode);
#endif

#ifdef FRAME_GROWS_DOWNWARD
  frame_offset -= size;
#endif

  /* If we have already instantiated virtual registers, return the actual
     address relative to the frame pointer.  */
  if (virtuals_instantiated)
    addr = plus_constant (frame_pointer_rtx,
			  (frame_offset + bigend_correction
			   + STARTING_FRAME_OFFSET));
  else
    addr = plus_constant (virtual_stack_vars_rtx,
			  frame_offset + bigend_correction);

#ifndef FRAME_GROWS_DOWNWARD
  frame_offset += size;
#endif

  x = gen_rtx (MEM, mode, addr);

  stack_slot_list = gen_rtx (EXPR_LIST, VOIDmode, x, stack_slot_list);

  return x;
}

/* Assign a stack slot in a containing function.
   First three arguments are same as in preceding function.
   The last argument specifies the function to allocate in.  */

rtx
assign_outer_stack_local (mode, size, align, function)
     enum machine_mode mode;
     int size;
     int align;
     struct function *function;
{
  register rtx x, addr;
  int bigend_correction = 0;
  int alignment;

  /* Allocate in the memory associated with the function in whose frame
     we are assigning.  */
  push_obstacks (function->function_obstack,
		 function->function_maybepermanent_obstack);

  if (align == 0)
    {
      alignment = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
      if (mode == BLKmode)
	alignment = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
    }
  else if (align == -1)
    {
      alignment = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
      size = CEIL_ROUND (size, alignment);
    }
  else
    alignment = align / BITS_PER_UNIT;

  /* Round frame offset to that alignment.  */
#ifdef FRAME_GROWS_DOWNWARD
  function->frame_offset = FLOOR_ROUND (function->frame_offset, alignment);
#else
  function->frame_offset = CEIL_ROUND (function->frame_offset, alignment);
#endif

  /* On a big-endian machine, if we are allocating more space than we will use,
     use the least significant bytes of those that are allocated.  */
#if BYTES_BIG_ENDIAN
  if (mode != BLKmode)
    bigend_correction = size - GET_MODE_SIZE (mode);
#endif

#ifdef FRAME_GROWS_DOWNWARD
  function->frame_offset -= size;
#endif
  addr = plus_constant (virtual_stack_vars_rtx,
			function->frame_offset + bigend_correction);
#ifndef FRAME_GROWS_DOWNWARD
  function->frame_offset += size;
#endif

  x = gen_rtx (MEM, mode, addr);

  function->stack_slot_list
    = gen_rtx (EXPR_LIST, VOIDmode, x, function->stack_slot_list);

  pop_obstacks ();

  return x;
}

/* Allocate a temporary stack slot and record it for possible later
   reuse.

   MODE is the machine mode to be given to the returned rtx.

   SIZE is the size in units of the space required.  We do no rounding here
   since assign_stack_local will do any required rounding.

   KEEP is non-zero if this slot is to be retained after a call to
   free_temp_slots.  Automatic variables for a block are allocated with this
   flag.  */

rtx
assign_stack_temp (mode, size, keep)
     enum machine_mode mode;
     int size;
     int keep;
{
  struct temp_slot *p, *best_p = 0;

  /* First try to find an available, already-allocated temporary that is the
     exact size we require.  */
  for (p = temp_slots; p; p = p->next)
    if (p->size == size && GET_MODE (p->slot) == mode && ! p->in_use)
      break;

  /* If we didn't find, one, try one that is larger than what we want.  We
     find the smallest such.  */
  if (p == 0)
    for (p = temp_slots; p; p = p->next)
      if (p->size > size && GET_MODE (p->slot) == mode && ! p->in_use
	  && (best_p == 0 || best_p->size > p->size))
	best_p = p;

  /* Make our best, if any, the one to use.  */
  if (best_p)
    p = best_p;

  /* If we still didn't find one, make a new temporary.  */
  if (p == 0)
    {
      p = (struct temp_slot *) oballoc (sizeof (struct temp_slot));
      p->size = size;
      /* If the temp slot mode doesn't indicate the alignment,
	 use the largest possible, so no one will be disappointed.  */
      p->slot = assign_stack_local (mode, size, mode == BLKmode ? -1 : 0); 
      p->next = temp_slots;
      temp_slots = p;
    }

  p->in_use = 1;
  p->level = temp_slot_level;
  p->keep = keep;
  return p->slot;
}

/* If X could be a reference to a temporary slot, mark that slot as belonging
   to the to one level higher.  If X matched one of our slots, just mark that
   one.  Otherwise, we can't easily predict which it is, so upgrade all of
   them.  Kept slots need not be touched.

   This is called when an ({...}) construct occurs and a statement
   returns a value in memory.  */

void
preserve_temp_slots (x)
     rtx x;
{
  struct temp_slot *p;

  /* If X is not in memory or is at a constant address, it cannot be in
     a temporary slot.  */
  if (x == 0 || GET_CODE (x) != MEM || CONSTANT_P (XEXP (x, 0)))
    return;

  /* First see if we can find a match.  */
  for (p = temp_slots; p; p = p->next)
    if (p->in_use && x == p->slot)
      {
	p->level--;
	return;
      }

  /* Otherwise, preserve all non-kept slots at this level.  */
  for (p = temp_slots; p; p = p->next)
    if (p->in_use && p->level == temp_slot_level && ! p->keep)
      p->level--;
}

/* Free all temporaries used so far.  This is normally called at the end
   of generating code for a statement.  */

void
free_temp_slots ()
{
  struct temp_slot *p;

  for (p = temp_slots; p; p = p->next)
    if (p->in_use && p->level == temp_slot_level && ! p->keep)
      p->in_use = 0;
}

/* Push deeper into the nesting level for stack temporaries.  */

void
push_temp_slots ()
{
  /* For GNU C++, we must allow a sequence to be emitted anywhere in
     the level where the sequence was started.  By not changing levels
     when the compiler is inside a sequence, the temporaries for the
     sequence and the temporaries will not unwittingly conflict with
     the temporaries for other sequences and/or code at that level.  */
  if (in_sequence_p ())
    return;

  temp_slot_level++;
}

/* Pop a temporary nesting level.  All slots in use in the current level
   are freed.  */

void
pop_temp_slots ()
{
  struct temp_slot *p;

  /* See comment in push_temp_slots about why we don't change levels
     in sequences.  */
  if (in_sequence_p ())
    return;

  for (p = temp_slots; p; p = p->next)
    if (p->in_use && p->level == temp_slot_level)
      p->in_use = 0;

  temp_slot_level--;
}

/* Retroactively move an auto variable from a register to a stack slot.
   This is done when an address-reference to the variable is seen.  */

void
put_var_into_stack (decl)
     tree decl;
{
  register rtx reg;
  register rtx new = 0;
  enum machine_mode promoted_mode, decl_mode;
  struct function *function = 0;
  tree context = decl_function_context (decl);

  /* Get the current rtl used for this object and it's original mode.  */
  reg = TREE_CODE (decl) == SAVE_EXPR ? SAVE_EXPR_RTL (decl) : DECL_RTL (decl);

  /* No need to do anything if decl has no rtx yet
     since in that case caller is setting TREE_ADDRESSABLE
     and a stack slot will be assigned when the rtl is made.  */
  if (reg == 0)
    return;

  /* Get the declared mode for this object.  */
  decl_mode = (TREE_CODE (decl) == SAVE_EXPR ? TYPE_MODE (TREE_TYPE (decl))
	       : DECL_MODE (decl));
  /* Get the mode it's actually stored in.  */
  promoted_mode = GET_MODE (reg);

  /* If this variable comes from an outer function,
     find that function's saved context.  */
  if (context != current_function_decl)
    for (function = outer_function_chain; function; function = function->next)
      if (function->decl == context)
	break;

  /* If this is a variable-size object with a pseudo to address it,
     put that pseudo into the stack, if the var is nonlocal.  */
  if (DECL_NONLOCAL (decl)
      && GET_CODE (reg) == MEM
      && GET_CODE (XEXP (reg, 0)) == REG
      && REGNO (XEXP (reg, 0)) > LAST_VIRTUAL_REGISTER)
    {
      reg = XEXP (reg, 0);
      decl_mode = promoted_mode = GET_MODE (reg);
    }
  if (GET_CODE (reg) != REG)
    return;

  if (function)
    {
      if (REGNO (reg) < function->max_parm_reg)
	new = function->parm_reg_stack_loc[REGNO (reg)];
      if (new == 0)
	new = assign_outer_stack_local (GET_MODE (reg),
					GET_MODE_SIZE (decl_mode),
					0, function);
    }
  else
    {
      if (REGNO (reg) < max_parm_reg)
	new = parm_reg_stack_loc[REGNO (reg)];
      if (new == 0)
	new = assign_stack_local (GET_MODE (reg),
				  GET_MODE_SIZE (decl_mode), 0);
    }

  XEXP (reg, 0) = XEXP (new, 0);
  /* `volatil' bit means one thing for MEMs, another entirely for REGs.  */
  REG_USERVAR_P (reg) = 0;
  PUT_CODE (reg, MEM);
  PUT_MODE (reg, decl_mode);

  /* If this is a memory ref that contains aggregate components,
     mark it as such for cse and loop optimize.  */
  MEM_IN_STRUCT_P (reg)
    = (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
       || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
       || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE);

  /* Now make sure that all refs to the variable, previously made
     when it was a register, are fixed up to be valid again.  */
  if (function)
    {
      struct var_refs_queue *temp;

      /* Variable is inherited; fix it up when we get back to its function.  */
      push_obstacks (function->function_obstack,
		     function->function_maybepermanent_obstack);
      temp
	= (struct var_refs_queue *) oballoc (sizeof (struct var_refs_queue));
      temp->modified = reg;
      temp->promoted_mode = promoted_mode;
      temp->unsignedp = TREE_UNSIGNED (TREE_TYPE (decl));
      temp->next = function->fixup_var_refs_queue;
      function->fixup_var_refs_queue = temp;
      pop_obstacks ();
    }
  else
    /* Variable is local; fix it up now.  */
    fixup_var_refs (reg, promoted_mode, TREE_UNSIGNED (TREE_TYPE (decl)));
}

static void
fixup_var_refs (var, promoted_mode, unsignedp)
     rtx var;
     enum machine_mode promoted_mode;
     int unsignedp;
{
  tree pending;
  rtx first_insn = get_insns ();
  struct sequence_stack *stack = sequence_stack;
  tree rtl_exps = rtl_expr_chain;

  /* Must scan all insns for stack-refs that exceed the limit.  */
  fixup_var_refs_insns (var, promoted_mode, unsignedp, first_insn, stack == 0);

  /* Scan all pending sequences too.  */
  for (; stack; stack = stack->next)
    {
      push_to_sequence (stack->first);
      fixup_var_refs_insns (var, promoted_mode, unsignedp,
			    stack->first, stack->next != 0);
      /* Update remembered end of sequence
	 in case we added an insn at the end.  */
      stack->last = get_last_insn ();
      end_sequence ();
    }

  /* Scan all waiting RTL_EXPRs too.  */
  for (pending = rtl_exps; pending; pending = TREE_CHAIN (pending))
    {
      rtx seq = RTL_EXPR_SEQUENCE (TREE_VALUE (pending));
      if (seq != const0_rtx && seq != 0)
	{
	  push_to_sequence (seq);
	  fixup_var_refs_insns (var, promoted_mode, unsignedp, seq, 0);
	  end_sequence ();
	}
    }
}

/* This structure is used by the following two functions to record MEMs or
   pseudos used to replace VAR, any SUBREGs of VAR, and any MEMs containing
   VAR as an address.  We need to maintain this list in case two operands of
   an insn were required to match; in that case we must ensure we use the
   same replacement.  */

struct fixup_replacement
{
  rtx old;
  rtx new;
  struct fixup_replacement *next;
};
   
/* REPLACEMENTS is a pointer to a list of the above structures and X is
   some part of an insn.  Return a struct fixup_replacement whose OLD
   value is equal to X.  Allocate a new structure if no such entry exists. */

static struct fixup_replacement *
find_fixup_replacement (replacements, x)
     struct fixup_replacement **replacements;
     rtx x;
{
  struct fixup_replacement *p;

  /* See if we have already replaced this.  */
  for (p = *replacements; p && p->old != x; p = p->next)
    ;

  if (p == 0)
    {
      p = (struct fixup_replacement *) oballoc (sizeof (struct fixup_replacement));
      p->old = x;
      p->new = 0;
      p->next = *replacements;
      *replacements = p;
    }

  return p;
}

/* Scan the insn-chain starting with INSN for refs to VAR
   and fix them up.  TOPLEVEL is nonzero if this chain is the
   main chain of insns for the current function.  */

static void
fixup_var_refs_insns (var, promoted_mode, unsignedp, insn, toplevel)
     rtx var;
     enum machine_mode promoted_mode;
     int unsignedp;
     rtx insn;
     int toplevel;
{
  while (insn)
    {
      rtx next = NEXT_INSN (insn);
      rtx note;
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	  || GET_CODE (insn) == JUMP_INSN)
	{
	  /* The insn to load VAR from a home in the arglist
	     is now a no-op.  When we see it, just delete it.  */
	  if (toplevel
	      && GET_CODE (PATTERN (insn)) == SET
	      && SET_DEST (PATTERN (insn)) == var
	      && rtx_equal_p (SET_SRC (PATTERN (insn)), var))
	    {
	      /* In unoptimized compilation, we shouldn't call delete_insn
		 except in jump.c doing warnings.  */
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;
	      if (insn == last_parm_insn)
		last_parm_insn = PREV_INSN (next);
	    }
	  else
	    {
	      /* See if we have to do anything to INSN now that VAR is in
		 memory.  If it needs to be loaded into a pseudo, use a single
		 pseudo for the entire insn in case there is a MATCH_DUP
		 between two operands.  We pass a pointer to the head of
		 a list of struct fixup_replacements.  If fixup_var_refs_1
		 needs to allocate pseudos or replacement MEMs (for SUBREGs),
		 it will record them in this list.
		 
		 If it allocated a pseudo for any replacement, we copy into
		 it here.  */

	      struct fixup_replacement *replacements = 0;

	      fixup_var_refs_1 (var, promoted_mode, &PATTERN (insn), insn,
				&replacements);

	      while (replacements)
		{
		  if (GET_CODE (replacements->new) == REG)
		    {
		      rtx insert_before;
		      rtx seq;

		      /* OLD might be a (subreg (mem)).  */
		      if (GET_CODE (replacements->old) == SUBREG)
			replacements->old
			  = fixup_memory_subreg (replacements->old, insn, 0);
		      else
			replacements->old
			  = fixup_stack_1 (replacements->old, insn);

		      /* We can not separate USE insns from the CALL_INSN
			 that they belong to.  If this is a CALL_INSN, insert
			 the move insn before the USE insns preceding it
			 instead of immediately before the insn.  */
		      if (GET_CODE (insn) == CALL_INSN)
			{
			  insert_before = insn;
			  while (GET_CODE (PREV_INSN (insert_before)) == INSN
				 && GET_CODE (PATTERN (PREV_INSN (insert_before))) == USE)
			    insert_before = PREV_INSN (insert_before);
			}
		      else
			insert_before = insn;

		      /* If we are changing the mode, do a conversion.
			 This might be wasteful, but combine.c will
			 eliminate much of the waste.  */

		      if (GET_MODE (replacements->new)
			  != GET_MODE (replacements->old))
			{
			  start_sequence ();
			  convert_move (replacements->new,
					replacements->old, unsignedp);
			  seq = gen_sequence ();
			  end_sequence ();
			}
		      else
			seq = gen_move_insn (replacements->new,
					     replacements->old);

		      emit_insn_before (seq, insert_before);
		    }

		  replacements = replacements->next;
		}
	    }

	  /* Also fix up any invalid exprs in the REG_NOTES of this insn.
	     But don't touch other insns referred to by reg-notes;
	     we will get them elsewhere.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (GET_CODE (note) != INSN_LIST)
	      XEXP (note, 0) = walk_fixup_memory_subreg (XEXP (note, 0), insn);
	}
      insn = next;
    }
}

/* VAR is a MEM that used to be a pseudo register with mode PROMOTED_MODE.
   See if the rtx expression at *LOC in INSN needs to be changed.  

   REPLACEMENTS is a pointer to a list head that starts out zero, but may
   contain a list of original rtx's and replacements. If we find that we need
   to modify this insn by replacing a memory reference with a pseudo or by
   making a new MEM to implement a SUBREG, we consult that list to see if
   we have already chosen a replacement. If none has already been allocated,
   we allocate it and update the list.  fixup_var_refs_insns will copy VAR
   or the SUBREG, as appropriate, to the pseudo.  */

static void
fixup_var_refs_1 (var, promoted_mode, loc, insn, replacements)
     register rtx var;
     enum machine_mode promoted_mode;
     register rtx *loc;
     rtx insn;
     struct fixup_replacement **replacements;
{
  register int i;
  register rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  register char *fmt;
  register rtx tem, tem1;
  struct fixup_replacement *replacement;

  switch (code)
    {
    case MEM:
      if (var == x)
	{
	  /* If we already have a replacement, use it.  Otherwise, 
	     try to fix up this address in case it is invalid.  */

	  replacement = find_fixup_replacement (replacements, var);
	  if (replacement->new)
	    {
	      *loc = replacement->new;
	      return;
	    }

	  *loc = replacement->new = x = fixup_stack_1 (x, insn);

	  /* Unless we are forcing memory to register or we changed the mode,
	     we can leave things the way they are if the insn is valid.  */
	     
	  INSN_CODE (insn) = -1;
	  if (! flag_force_mem && GET_MODE (x) == promoted_mode
	      && recog_memoized (insn) >= 0)
	    return;

	  *loc = replacement->new = gen_reg_rtx (promoted_mode);
	  return;
	}

      /* If X contains VAR, we need to unshare it here so that we update
	 each occurrence separately.  But all identical MEMs in one insn
	 must be replaced with the same rtx because of the possibility of
	 MATCH_DUPs.  */

      if (reg_mentioned_p (var, x))
	{
	  replacement = find_fixup_replacement (replacements, x);
	  if (replacement->new == 0)
	    replacement->new = copy_most_rtx (x, var);

	  *loc = x = replacement->new;
	}
      break;

    case REG:
    case CC0:
    case PC:
    case CONST_INT:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
      return;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      /* Note that in some cases those types of expressions are altered
	 by optimize_bit_field, and do not survive to get here.  */
      if (XEXP (x, 0) == var
	  || (GET_CODE (XEXP (x, 0)) == SUBREG
	      && SUBREG_REG (XEXP (x, 0)) == var))
	{
	  /* Get TEM as a valid MEM in the mode presently in the insn.

	     We don't worry about the possibility of MATCH_DUP here; it
	     is highly unlikely and would be tricky to handle.  */

	  tem = XEXP (x, 0);
	  if (GET_CODE (tem) == SUBREG)
	    tem = fixup_memory_subreg (tem, insn, 1);
	  tem = fixup_stack_1 (tem, insn);

	  /* Unless we want to load from memory, get TEM into the proper mode
	     for an extract from memory.  This can only be done if the
	     extract is at a constant position and length.  */

	  if (! flag_force_mem && GET_CODE (XEXP (x, 1)) == CONST_INT
	      && GET_CODE (XEXP (x, 2)) == CONST_INT
	      && ! mode_dependent_address_p (XEXP (tem, 0))
	      && ! MEM_VOLATILE_P (tem))
	    {
	      enum machine_mode wanted_mode = VOIDmode;
	      enum machine_mode is_mode = GET_MODE (tem);
	      int width = INTVAL (XEXP (x, 1));
	      int pos = INTVAL (XEXP (x, 2));

#ifdef HAVE_extzv
	      if (GET_CODE (x) == ZERO_EXTRACT)
		wanted_mode = insn_operand_mode[(int) CODE_FOR_extzv][1];
#endif
#ifdef HAVE_extv
	      if (GET_CODE (x) == SIGN_EXTRACT)
		wanted_mode = insn_operand_mode[(int) CODE_FOR_extv][1];
#endif
	      /* If we have a narrower mode, we can do something.  */
	      if (wanted_mode != VOIDmode
		  && GET_MODE_SIZE (wanted_mode) < GET_MODE_SIZE (is_mode))
		{
		  int offset = pos / BITS_PER_UNIT;
		  rtx old_pos = XEXP (x, 2);
		  rtx newmem;

		  /* If the bytes and bits are counted differently, we
		     must adjust the offset.  */
#if BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN
		  offset = (GET_MODE_SIZE (is_mode)
			    - GET_MODE_SIZE (wanted_mode) - offset);
#endif

		  pos %= GET_MODE_BITSIZE (wanted_mode);

		  newmem = gen_rtx (MEM, wanted_mode,
				    plus_constant (XEXP (tem, 0), offset));
		  RTX_UNCHANGING_P (newmem) = RTX_UNCHANGING_P (tem);
		  MEM_VOLATILE_P (newmem) = MEM_VOLATILE_P (tem);
		  MEM_IN_STRUCT_P (newmem) = MEM_IN_STRUCT_P (tem);

		  /* Make the change and see if the insn remains valid.  */
		  INSN_CODE (insn) = -1;
		  XEXP (x, 0) = newmem;
		  XEXP (x, 2) = GEN_INT (pos);

		  if (recog_memoized (insn) >= 0)
		    return;

		  /* Otherwise, restore old position.  XEXP (x, 0) will be
		     restored later.  */
		  XEXP (x, 2) = old_pos;
		}
	    }

	  /* If we get here, the bitfield extract insn can't accept a memory
	     reference.  Copy the input into a register.  */

	  tem1 = gen_reg_rtx (GET_MODE (tem));
	  emit_insn_before (gen_move_insn (tem1, tem), insn);
	  XEXP (x, 0) = tem1;
	  return;
	}
      break;
	      
    case SUBREG:
      if (SUBREG_REG (x) == var)
	{
	  /* If this is a special SUBREG made because VAR was promoted
	     from a wider mode, replace it with VAR and call ourself
	     recursively, this time saying that the object previously
	     had its current mode (by virtue of the SUBREG).  */

	  if (SUBREG_PROMOTED_VAR_P (x))
	    {
	      *loc = var;
	      fixup_var_refs_1 (var, GET_MODE (var), loc, insn, replacements);
	      return;
	    }

	  /* If this SUBREG makes VAR wider, it has become a paradoxical
	     SUBREG with VAR in memory, but these aren't allowed at this 
	     stage of the compilation.  So load VAR into a pseudo and take
	     a SUBREG of that pseudo.  */
	  if (GET_MODE_SIZE (GET_MODE (x)) > GET_MODE_SIZE (GET_MODE (var)))
	    {
	      replacement = find_fixup_replacement (replacements, var);
	      if (replacement->new == 0)
		replacement->new = gen_reg_rtx (GET_MODE (var));
	      SUBREG_REG (x) = replacement->new;
	      return;
	    }

	  /* See if we have already found a replacement for this SUBREG.
	     If so, use it.  Otherwise, make a MEM and see if the insn
	     is recognized.  If not, or if we should force MEM into a register,
	     make a pseudo for this SUBREG.  */
	  replacement = find_fixup_replacement (replacements, x);
	  if (replacement->new)
	    {
	      *loc = replacement->new;
	      return;
	    }
	  
	  replacement->new = *loc = fixup_memory_subreg (x, insn, 0);

	  if (! flag_force_mem && recog_memoized (insn) >= 0)
	    return;

	  *loc = replacement->new = gen_reg_rtx (GET_MODE (x));
	  return;
	}
      break;

    case SET:
      /* First do special simplification of bit-field references.  */
      if (GET_CODE (SET_DEST (x)) == SIGN_EXTRACT
	  || GET_CODE (SET_DEST (x)) == ZERO_EXTRACT)
	optimize_bit_field (x, insn, 0);
      if (GET_CODE (SET_SRC (x)) == SIGN_EXTRACT
	  || GET_CODE (SET_SRC (x)) == ZERO_EXTRACT)
	optimize_bit_field (x, insn, NULL_PTR);

      /* If SET_DEST is now a paradoxical SUBREG, put the result of this
	 insn into a pseudo and store the low part of the pseudo into VAR. */
      if (GET_CODE (SET_DEST (x)) == SUBREG
	  && SUBREG_REG (SET_DEST (x)) == var
	  && (GET_MODE_SIZE (GET_MODE (SET_DEST (x)))
	      > GET_MODE_SIZE (GET_MODE (var))))
	{
	  SET_DEST (x) = tem = gen_reg_rtx (GET_MODE (SET_DEST (x)));
	  emit_insn_after (gen_move_insn (var, gen_lowpart (GET_MODE (var),
							    tem)),
			   insn);
	  break;
	}
	  
      {
	rtx dest = SET_DEST (x);
	rtx src = SET_SRC (x);
	rtx outerdest = dest;

	while (GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART
	       || GET_CODE (dest) == SIGN_EXTRACT
	       || GET_CODE (dest) == ZERO_EXTRACT)
	  dest = XEXP (dest, 0);

	if (GET_CODE (src) == SUBREG)
	  src = XEXP (src, 0);

	/* If VAR does not appear at the top level of the SET
	   just scan the lower levels of the tree.  */

        if (src != var && dest != var)
	  break;

	/* We will need to rerecognize this insn.  */
	INSN_CODE (insn) = -1;

#ifdef HAVE_insv
	if (GET_CODE (outerdest) == ZERO_EXTRACT && dest == var)
	  {
	    /* Since this case will return, ensure we fixup all the
	       operands here.  */
	    fixup_var_refs_1 (var, promoted_mode, &XEXP (outerdest, 1),
			      insn, replacements);
	    fixup_var_refs_1 (var, promoted_mode, &XEXP (outerdest, 2),
			      insn, replacements);
	    fixup_var_refs_1 (var, promoted_mode, &SET_SRC (x),
			      insn, replacements);

	    tem = XEXP (outerdest, 0);

	    /* Clean up (SUBREG:SI (MEM:mode ...) 0)
	       that may appear inside a ZERO_EXTRACT.
	       This was legitimate when the MEM was a REG.  */
	    if (GET_CODE (tem) == SUBREG
		&& SUBREG_REG (tem) == var)
	      tem = fixup_memory_subreg (tem, insn, 1);
	    else
	      tem = fixup_stack_1 (tem, insn);

	    if (GET_CODE (XEXP (outerdest, 1)) == CONST_INT
		&& GET_CODE (XEXP (outerdest, 2)) == CONST_INT
		&& ! mode_dependent_address_p (XEXP (tem, 0))
		&& ! MEM_VOLATILE_P (tem))
	      {
		enum machine_mode wanted_mode
		  = insn_operand_mode[(int) CODE_FOR_insv][0];
		enum machine_mode is_mode = GET_MODE (tem);
		int width = INTVAL (XEXP (outerdest, 1));
		int pos = INTVAL (XEXP (outerdest, 2));

		/* If we have a narrower mode, we can do something.  */
		if (GET_MODE_SIZE (wanted_mode) < GET_MODE_SIZE (is_mode))
		  {
		    int offset = pos / BITS_PER_UNIT;
		    rtx old_pos = XEXP (outerdest, 2);
		    rtx newmem;

#if BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN
		    offset = (GET_MODE_SIZE (is_mode)
			      - GET_MODE_SIZE (wanted_mode) - offset);
#endif

		    pos %= GET_MODE_BITSIZE (wanted_mode);

		    newmem = gen_rtx (MEM, wanted_mode,
				      plus_constant (XEXP (tem, 0), offset));
		    RTX_UNCHANGING_P (newmem) = RTX_UNCHANGING_P (tem);
		    MEM_VOLATILE_P (newmem) = MEM_VOLATILE_P (tem);
		    MEM_IN_STRUCT_P (newmem) = MEM_IN_STRUCT_P (tem);

		    /* Make the change and see if the insn remains valid.  */
		    INSN_CODE (insn) = -1;
		    XEXP (outerdest, 0) = newmem;
		    XEXP (outerdest, 2) = GEN_INT (pos);
		    
		    if (recog_memoized (insn) >= 0)
		      return;
		    
		    /* Otherwise, restore old position.  XEXP (x, 0) will be
		       restored later.  */
		    XEXP (outerdest, 2) = old_pos;
		  }
	      }

	    /* If we get here, the bit-field store doesn't allow memory
	       or isn't located at a constant position.  Load the value into
	       a register, do the store, and put it back into memory.  */

	    tem1 = gen_reg_rtx (GET_MODE (tem));
	    emit_insn_before (gen_move_insn (tem1, tem), insn);
	    emit_insn_after (gen_move_insn (tem, tem1), insn);
	    XEXP (outerdest, 0) = tem1;
	    return;
	  }
#endif

	/* STRICT_LOW_PART is a no-op on memory references
	   and it can cause combinations to be unrecognizable,
	   so eliminate it.  */

	if (dest == var && GET_CODE (SET_DEST (x)) == STRICT_LOW_PART)
	  SET_DEST (x) = XEXP (SET_DEST (x), 0);

	/* A valid insn to copy VAR into or out of a register
	   must be left alone, to avoid an infinite loop here.
	   If the reference to VAR is by a subreg, fix that up,
	   since SUBREG is not valid for a memref.
	   Also fix up the address of the stack slot.  */

	if ((SET_SRC (x) == var
	     || (GET_CODE (SET_SRC (x)) == SUBREG
		 && SUBREG_REG (SET_SRC (x)) == var))
	    && (GET_CODE (SET_DEST (x)) == REG
		|| (GET_CODE (SET_DEST (x)) == SUBREG
		    && GET_CODE (SUBREG_REG (SET_DEST (x))) == REG))
	    && recog_memoized (insn) >= 0)
	  {
	    replacement = find_fixup_replacement (replacements, SET_SRC (x));
	    if (replacement->new)
	      {
	      SET_SRC (x) = replacement->new;
	      return;
	    }
	    else if (GET_CODE (SET_SRC (x)) == SUBREG)
	      SET_SRC (x) = replacement->new
		= fixup_memory_subreg (SET_SRC (x), insn, 0);
	    else
	      SET_SRC (x) = replacement->new
		= fixup_stack_1 (SET_SRC (x), insn);
	    return;
	  }

	if ((SET_DEST (x) == var
	     || (GET_CODE (SET_DEST (x)) == SUBREG
		 && SUBREG_REG (SET_DEST (x)) == var))
	    && (GET_CODE (SET_SRC (x)) == REG
		|| (GET_CODE (SET_SRC (x)) == SUBREG
		    && GET_CODE (SUBREG_REG (SET_SRC (x))) == REG))
	    && recog_memoized (insn) >= 0)
	  {
	    if (GET_CODE (SET_DEST (x)) == SUBREG)
	      SET_DEST (x) = fixup_memory_subreg (SET_DEST (x), insn, 0);
	    else
	      SET_DEST (x) = fixup_stack_1 (SET_DEST (x), insn);
	    return;
	  }

	/* Otherwise, storing into VAR must be handled specially
	   by storing into a temporary and copying that into VAR
	   with a new insn after this one.  Note that this case
	   will be used when storing into a promoted scalar since
	   the insn will now have different modes on the input
	   and output and hence will be invalid (except for the case
	   of setting it to a constant, which does not need any
	   change if it is valid).  We generate extra code in that case,
	   but combine.c will eliminate it.  */

	if (dest == var)
	  {
	    rtx temp;
	    rtx fixeddest = SET_DEST (x);

	    /* STRICT_LOW_PART can be discarded, around a MEM.  */
	    if (GET_CODE (fixeddest) == STRICT_LOW_PART)
	      fixeddest = XEXP (fixeddest, 0);
	    /* Convert (SUBREG (MEM)) to a MEM in a changed mode.  */
	    if (GET_CODE (fixeddest) == SUBREG)
	      fixeddest = fixup_memory_subreg (fixeddest, insn, 0);
	    else
	      fixeddest = fixup_stack_1 (fixeddest, insn);

	    temp = gen_reg_rtx (GET_MODE (SET_SRC (x)) == VOIDmode
				? GET_MODE (fixeddest)
				: GET_MODE (SET_SRC (x)));

	    emit_insn_after (gen_move_insn (fixeddest,
					    gen_lowpart (GET_MODE (fixeddest),
							 temp)),
			     insn);

	    SET_DEST (x) = temp;
	  }
      }
    }

  /* Nothing special about this RTX; fix its operands.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	fixup_var_refs_1 (var, promoted_mode, &XEXP (x, i), insn, replacements);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    fixup_var_refs_1 (var, promoted_mode, &XVECEXP (x, i, j),
			      insn, replacements);
	}
    }
}

/* Given X, an rtx of the form (SUBREG:m1 (MEM:m2 addr)),
   return an rtx (MEM:m1 newaddr) which is equivalent.
   If any insns must be emitted to compute NEWADDR, put them before INSN.

   UNCRITICAL nonzero means accept paradoxical subregs.
   This is used for subregs found inside of ZERO_EXTRACTs.  */

static rtx
fixup_memory_subreg (x, insn, uncritical)
     rtx x;
     rtx insn;
     int uncritical;
{
  int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
  rtx addr = XEXP (SUBREG_REG (x), 0);
  enum machine_mode mode = GET_MODE (x);
  rtx saved, result;

  /* Paradoxical SUBREGs are usually invalid during RTL generation.  */
  if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))
      && ! uncritical)
    abort ();

#if BYTES_BIG_ENDIAN
  offset += (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	     - MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode)));
#endif
  addr = plus_constant (addr, offset);
  if (!flag_force_addr && memory_address_p (mode, addr))
    /* Shortcut if no insns need be emitted.  */
    return change_address (SUBREG_REG (x), mode, addr);
  start_sequence ();
  result = change_address (SUBREG_REG (x), mode, addr);
  emit_insn_before (gen_sequence (), insn);
  end_sequence ();
  return result;
}

/* Do fixup_memory_subreg on all (SUBREG (MEM ...) ...) contained in X.
   Replace subexpressions of X in place.
   If X itself is a (SUBREG (MEM ...) ...), return the replacement expression.
   Otherwise return X, with its contents possibly altered.

   If any insns must be emitted to compute NEWADDR, put them before INSN.  */

static rtx
walk_fixup_memory_subreg (x, insn)
     register rtx x;
     rtx insn;
{
  register enum rtx_code code;
  register char *fmt;
  register int i;

  if (x == 0)
    return 0;

  code = GET_CODE (x);

  if (code == SUBREG && GET_CODE (SUBREG_REG (x)) == MEM)
    return fixup_memory_subreg (x, insn, 0);

  /* Nothing special about this RTX; fix its operands.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = walk_fixup_memory_subreg (XEXP (x, i), insn);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j)
	      = walk_fixup_memory_subreg (XVECEXP (x, i, j), insn);
	}
    }
  return x;
}

#if 0
/* Fix up any references to stack slots that are invalid memory addresses
   because they exceed the maximum range of a displacement.  */

void
fixup_stack_slots ()
{
  register rtx insn;

  /* Did we generate a stack slot that is out of range
     or otherwise has an invalid address?  */
  if (invalid_stack_slot)
    {
      /* Yes.  Must scan all insns for stack-refs that exceed the limit.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	    || GET_CODE (insn) == JUMP_INSN)
	  fixup_stack_1 (PATTERN (insn), insn);
    }
}
#endif

/* For each memory ref within X, if it refers to a stack slot
   with an out of range displacement, put the address in a temp register
   (emitting new insns before INSN to load these registers)
   and alter the memory ref to use that register.
   Replace each such MEM rtx with a copy, to avoid clobberage.  */

static rtx
fixup_stack_1 (x, insn)
     rtx x;
     rtx insn;
{
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register char *fmt;

  if (code == MEM)
    {
      register rtx ad = XEXP (x, 0);
      /* If we have address of a stack slot but it's not valid
	 (displacement is too large), compute the sum in a register.  */
      if (GET_CODE (ad) == PLUS
	  && GET_CODE (XEXP (ad, 0)) == REG
	  && REGNO (XEXP (ad, 0)) >= FIRST_VIRTUAL_REGISTER
	  && REGNO (XEXP (ad, 0)) <= LAST_VIRTUAL_REGISTER
	  && GET_CODE (XEXP (ad, 1)) == CONST_INT)
	{
	  rtx temp, seq;
	  if (memory_address_p (GET_MODE (x), ad))
	    return x;

	  start_sequence ();
	  temp = copy_to_reg (ad);
	  seq = gen_sequence ();
	  end_sequence ();
	  emit_insn_before (seq, insn);
	  return change_address (x, VOIDmode, temp);
	}
      return x;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = fixup_stack_1 (XEXP (x, i), insn);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j) = fixup_stack_1 (XVECEXP (x, i, j), insn);
	}
    }
  return x;
}

/* Optimization: a bit-field instruction whose field
   happens to be a byte or halfword in memory
   can be changed to a move instruction.

   We call here when INSN is an insn to examine or store into a bit-field.
   BODY is the SET-rtx to be altered.

   EQUIV_MEM is the table `reg_equiv_mem' if that is available; else 0.
   (Currently this is called only from function.c, and EQUIV_MEM
   is always 0.)  */

static void
optimize_bit_field (body, insn, equiv_mem)
     rtx body;
     rtx insn;
     rtx *equiv_mem;
{
  register rtx bitfield;
  int destflag;
  rtx seq = 0;
  enum machine_mode mode;

  if (GET_CODE (SET_DEST (body)) == SIGN_EXTRACT
      || GET_CODE (SET_DEST (body)) == ZERO_EXTRACT)
    bitfield = SET_DEST (body), destflag = 1;
  else
    bitfield = SET_SRC (body), destflag = 0;

  /* First check that the field being stored has constant size and position
     and is in fact a byte or halfword suitably aligned.  */

  if (GET_CODE (XEXP (bitfield, 1)) == CONST_INT
      && GET_CODE (XEXP (bitfield, 2)) == CONST_INT
      && ((mode = mode_for_size (INTVAL (XEXP (bitfield, 1)), MODE_INT, 1))
	  != BLKmode)
      && INTVAL (XEXP (bitfield, 2)) % INTVAL (XEXP (bitfield, 1)) == 0)
    {
      register rtx memref = 0;

      /* Now check that the containing word is memory, not a register,
	 and that it is safe to change the machine mode.  */

      if (GET_CODE (XEXP (bitfield, 0)) == MEM)
	memref = XEXP (bitfield, 0);
      else if (GET_CODE (XEXP (bitfield, 0)) == REG
	       && equiv_mem != 0)
	memref = equiv_mem[REGNO (XEXP (bitfield, 0))];
      else if (GET_CODE (XEXP (bitfield, 0)) == SUBREG
	       && GET_CODE (SUBREG_REG (XEXP (bitfield, 0))) == MEM)
	memref = SUBREG_REG (XEXP (bitfield, 0));
      else if (GET_CODE (XEXP (bitfield, 0)) == SUBREG
	       && equiv_mem != 0
	       && GET_CODE (SUBREG_REG (XEXP (bitfield, 0))) == REG)
	memref = equiv_mem[REGNO (SUBREG_REG (XEXP (bitfield, 0)))];

      if (memref
	  && ! mode_dependent_address_p (XEXP (memref, 0))
	  && ! MEM_VOLATILE_P (memref))
	{
	  /* Now adjust the address, first for any subreg'ing
	     that we are now getting rid of,
	     and then for which byte of the word is wanted.  */

	  register int offset = INTVAL (XEXP (bitfield, 2));
	  /* Adjust OFFSET to count bits from low-address byte.  */
#if BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN
	  offset = (GET_MODE_BITSIZE (GET_MODE (XEXP (bitfield, 0)))
		    - offset - INTVAL (XEXP (bitfield, 1)));
#endif
	  /* Adjust OFFSET to count bytes from low-address byte.  */
	  offset /= BITS_PER_UNIT;
	  if (GET_CODE (XEXP (bitfield, 0)) == SUBREG)
	    {
	      offset += SUBREG_WORD (XEXP (bitfield, 0)) * UNITS_PER_WORD;
#if BYTES_BIG_ENDIAN
	      offset -= (MIN (UNITS_PER_WORD,
			      GET_MODE_SIZE (GET_MODE (XEXP (bitfield, 0))))
			 - MIN (UNITS_PER_WORD,
				GET_MODE_SIZE (GET_MODE (memref))));
#endif
	    }

	  memref = change_address (memref, mode, 
				   plus_constant (XEXP (memref, 0), offset));

	  /* Store this memory reference where
	     we found the bit field reference.  */

	  if (destflag)
	    {
	      validate_change (insn, &SET_DEST (body), memref, 1);
	      if (! CONSTANT_ADDRESS_P (SET_SRC (body)))
		{
		  rtx src = SET_SRC (body);
		  while (GET_CODE (src) == SUBREG
			 && SUBREG_WORD (src) == 0)
		    src = SUBREG_REG (src);
		  if (GET_MODE (src) != GET_MODE (memref))
		    src = gen_lowpart (GET_MODE (memref), SET_SRC (body));
		  validate_change (insn, &SET_SRC (body), src, 1);
		}
	      else if (GET_MODE (SET_SRC (body)) != VOIDmode
		       && GET_MODE (SET_SRC (body)) != GET_MODE (memref))
		/* This shouldn't happen because anything that didn't have
		   one of these modes should have got converted explicitly
		   and then referenced through a subreg.
		   This is so because the original bit-field was
		   handled by agg_mode and so its tree structure had
		   the same mode that memref now has.  */
		abort ();
	    }
	  else
	    {
	      rtx dest = SET_DEST (body);

	      while (GET_CODE (dest) == SUBREG
		     && SUBREG_WORD (dest) == 0)
		dest = SUBREG_REG (dest);

	      validate_change (insn, &SET_DEST (body), dest, 1);

	      if (GET_MODE (dest) == GET_MODE (memref))
		validate_change (insn, &SET_SRC (body), memref, 1);
	      else
		{
		  /* Convert the mem ref to the destination mode.  */
		  rtx newreg = gen_reg_rtx (GET_MODE (dest));

		  start_sequence ();
		  convert_move (newreg, memref,
				GET_CODE (SET_SRC (body)) == ZERO_EXTRACT);
		  seq = get_insns ();
		  end_sequence ();

		  validate_change (insn, &SET_SRC (body), newreg, 1);
		}
	    }

	  /* See if we can convert this extraction or insertion into
	     a simple move insn.  We might not be able to do so if this
	     was, for example, part of a PARALLEL.

	     If we succeed, write out any needed conversions.  If we fail,
	     it is hard to guess why we failed, so don't do anything
	     special; just let the optimization be suppressed.  */

	  if (apply_change_group () && seq)
	    emit_insns_before (seq, insn);
	}
    }
}

/* These routines are responsible for converting virtual register references
   to the actual hard register references once RTL generation is complete.

   The following four variables are used for communication between the
   routines.  They contain the offsets of the virtual registers from their
   respective hard registers.  */

static int in_arg_offset;
static int var_offset;
static int dynamic_offset;
static int out_arg_offset;

/* In most machines, the stack pointer register is equivalent to the bottom
   of the stack.  */

#ifndef STACK_POINTER_OFFSET
#define STACK_POINTER_OFFSET	0
#endif

/* If not defined, pick an appropriate default for the offset of dynamically
   allocated memory depending on the value of ACCUMULATE_OUTGOING_ARGS,
   REG_PARM_STACK_SPACE, and OUTGOING_REG_PARM_STACK_SPACE.  */

#ifndef STACK_DYNAMIC_OFFSET

#ifdef ACCUMULATE_OUTGOING_ARGS
/* The bottom of the stack points to the actual arguments.  If
   REG_PARM_STACK_SPACE is defined, this includes the space for the register
   parameters.  However, if OUTGOING_REG_PARM_STACK space is not defined,
   stack space for register parameters is not pushed by the caller, but 
   rather part of the fixed stack areas and hence not included in
   `current_function_outgoing_args_size'.  Nevertheless, we must allow
   for it when allocating stack dynamic objects.  */

#if defined(REG_PARM_STACK_SPACE) && ! defined(OUTGOING_REG_PARM_STACK_SPACE)
#define STACK_DYNAMIC_OFFSET(FNDECL)	\
(current_function_outgoing_args_size	\
 + REG_PARM_STACK_SPACE (FNDECL) + (STACK_POINTER_OFFSET))

#else
#define STACK_DYNAMIC_OFFSET(FNDECL)	\
(current_function_outgoing_args_size + (STACK_POINTER_OFFSET))
#endif

#else
#define STACK_DYNAMIC_OFFSET(FNDECL) STACK_POINTER_OFFSET
#endif
#endif

/* Pass through the INSNS of function FNDECL and convert virtual register
   references to hard register references.  */

void
instantiate_virtual_regs (fndecl, insns)
     tree fndecl;
     rtx insns;
{
  rtx insn;

  /* Compute the offsets to use for this function.  */
  in_arg_offset = FIRST_PARM_OFFSET (fndecl);
  var_offset = STARTING_FRAME_OFFSET;
  dynamic_offset = STACK_DYNAMIC_OFFSET (fndecl);
  out_arg_offset = STACK_POINTER_OFFSET;

  /* Scan all variables and parameters of this function.  For each that is
     in memory, instantiate all virtual registers if the result is a valid
     address.  If not, we do it later.  That will handle most uses of virtual
     regs on many machines.  */
  instantiate_decls (fndecl, 1);

  /* Initialize recognition, indicating that volatile is OK.  */
  init_recog ();

  /* Scan through all the insns, instantiating every virtual register still
     present.  */
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	|| GET_CODE (insn) == CALL_INSN)
      {
	instantiate_virtual_regs_1 (&PATTERN (insn), insn, 1);
	instantiate_virtual_regs_1 (&REG_NOTES (insn), NULL_RTX, 0);
      }

  /* Now instantiate the remaining register equivalences for debugging info.
     These will not be valid addresses.  */
  instantiate_decls (fndecl, 0);

  /* Indicate that, from now on, assign_stack_local should use
     frame_pointer_rtx.  */
  virtuals_instantiated = 1;
}

/* Scan all decls in FNDECL (both variables and parameters) and instantiate
   all virtual registers in their DECL_RTL's.

   If VALID_ONLY, do this only if the resulting address is still valid.
   Otherwise, always do it.  */

static void
instantiate_decls (fndecl, valid_only)
     tree fndecl;
     int valid_only;
{
  tree decl;

  if (DECL_INLINE (fndecl))
    /* When compiling an inline function, the obstack used for
       rtl allocation is the maybepermanent_obstack.  Calling
       `resume_temporary_allocation' switches us back to that
       obstack while we process this function's parameters.  */
    resume_temporary_allocation ();

  /* Process all parameters of the function.  */
  for (decl = DECL_ARGUMENTS (fndecl); decl; decl = TREE_CHAIN (decl))
    {
      instantiate_decl (DECL_RTL (decl), int_size_in_bytes (TREE_TYPE (decl)),
			valid_only);	
      instantiate_decl (DECL_INCOMING_RTL (decl),
			int_size_in_bytes (TREE_TYPE (decl)), valid_only);
    }

  /* Now process all variables defined in the function or its subblocks. */
  instantiate_decls_1 (DECL_INITIAL (fndecl), valid_only);

  if (DECL_INLINE (fndecl))
    {
      /* Save all rtl allocated for this function by raising the
	 high-water mark on the maybepermanent_obstack.  */
      preserve_data ();
      /* All further rtl allocation is now done in the current_obstack.  */
      rtl_in_current_obstack ();
    }
}

/* Subroutine of instantiate_decls: Process all decls in the given
   BLOCK node and all its subblocks.  */

static void
instantiate_decls_1 (let, valid_only)
     tree let;
     int valid_only;
{
  tree t;

  for (t = BLOCK_VARS (let); t; t = TREE_CHAIN (t))
    instantiate_decl (DECL_RTL (t), int_size_in_bytes (TREE_TYPE (t)),
		      valid_only);

  /* Process all subblocks.  */
  for (t = BLOCK_SUBBLOCKS (let); t; t = TREE_CHAIN (t))
    instantiate_decls_1 (t, valid_only);
}

/* Subroutine of the preceeding procedures: Given RTL representing a
   decl and the size of the object, do any instantiation required.

   If VALID_ONLY is non-zero, it means that the RTL should only be
   changed if the new address is valid.  */

static void
instantiate_decl (x, size, valid_only)
     rtx x;
     int size;
     int valid_only;
{
  enum machine_mode mode;
  rtx addr;

  /* If this is not a MEM, no need to do anything.  Similarly if the
     address is a constant or a register that is not a virtual register.  */

  if (x == 0 || GET_CODE (x) != MEM)
    return;

  addr = XEXP (x, 0);
  if (CONSTANT_P (addr)
      || (GET_CODE (addr) == REG
	  && (REGNO (addr) < FIRST_VIRTUAL_REGISTER
	      || REGNO (addr) > LAST_VIRTUAL_REGISTER)))
    return;

  /* If we should only do this if the address is valid, copy the address.
     We need to do this so we can undo any changes that might make the
     address invalid.  This copy is unfortunate, but probably can't be
     avoided.  */

  if (valid_only)
    addr = copy_rtx (addr);

  instantiate_virtual_regs_1 (&addr, NULL_RTX, 0);

  if (! valid_only)
    return;

  /* Now verify that the resulting address is valid for every integer or
     floating-point mode up to and including SIZE bytes long.  We do this
     since the object might be accessed in any mode and frame addresses
     are shared.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       mode != VOIDmode && GET_MODE_SIZE (mode) <= size;
       mode = GET_MODE_WIDER_MODE (mode))
    if (! memory_address_p (mode, addr))
      return;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
       mode != VOIDmode && GET_MODE_SIZE (mode) <= size;
       mode = GET_MODE_WIDER_MODE (mode))
    if (! memory_address_p (mode, addr))
      return;

  /* Otherwise, put back the address, now that we have updated it and we
     know it is valid.  */

  XEXP (x, 0) = addr;
}

/* Given a pointer to a piece of rtx and an optional pointer to the
   containing object, instantiate any virtual registers present in it.

   If EXTRA_INSNS, we always do the replacement and generate
   any extra insns before OBJECT.  If it zero, we do nothing if replacement
   is not valid.

   Return 1 if we either had nothing to do or if we were able to do the
   needed replacement.  Return 0 otherwise; we only return zero if 
   EXTRA_INSNS is zero.

   We first try some simple transformations to avoid the creation of extra
   pseudos.  */

static int
instantiate_virtual_regs_1 (loc, object, extra_insns)
     rtx *loc;
     rtx object;
     int extra_insns;
{
  rtx x;
  RTX_CODE code;
  rtx new = 0;
  int offset;
  rtx temp;
  rtx seq;
  int i, j;
  char *fmt;

  /* Re-start here to avoid recursion in common cases.  */
 restart:

  x = *loc;
  if (x == 0)
    return 1;

  code = GET_CODE (x);

  /* Check for some special cases.  */
  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case RETURN:
      return 1;

    case SET:
      /* We are allowed to set the virtual registers.  This means that
	 that the actual register should receive the source minus the
	 appropriate offset.  This is used, for example, in the handling
	 of non-local gotos.  */
      if (SET_DEST (x) == virtual_incoming_args_rtx)
	new = arg_pointer_rtx, offset = - in_arg_offset;
      else if (SET_DEST (x) == virtual_stack_vars_rtx)
	new = frame_pointer_rtx, offset = - var_offset;
      else if (SET_DEST (x) == virtual_stack_dynamic_rtx)
	new = stack_pointer_rtx, offset = - dynamic_offset;
      else if (SET_DEST (x) == virtual_outgoing_args_rtx)
	new = stack_pointer_rtx, offset = - out_arg_offset;

      if (new)
	{
	  /* The only valid sources here are PLUS or REG.  Just do
	     the simplest possible thing to handle them.  */
	  if (GET_CODE (SET_SRC (x)) != REG
	      && GET_CODE (SET_SRC (x)) != PLUS)
	    abort ();

	  start_sequence ();
	  if (GET_CODE (SET_SRC (x)) != REG)
	    temp = force_operand (SET_SRC (x), NULL_RTX);
	  else
	    temp = SET_SRC (x);
	  temp = force_operand (plus_constant (temp, offset), NULL_RTX);
	  seq = get_insns ();
	  end_sequence ();

	  emit_insns_before (seq, object);
	  SET_DEST (x) = new;

	  if (!validate_change (object, &SET_SRC (x), temp, 0)
	      || ! extra_insns)
	    abort ();

	  return 1;
	}

      instantiate_virtual_regs_1 (&SET_DEST (x), object, extra_insns);
      loc = &SET_SRC (x);
      goto restart;

    case PLUS:
      /* Handle special case of virtual register plus constant.  */
      if (CONSTANT_P (XEXP (x, 1)))
	{
	  rtx old;

	  /* Check for (plus (plus VIRT foo) (const_int)) first.  */
	  if (GET_CODE (XEXP (x, 0)) == PLUS)
	    {
	      rtx inner = XEXP (XEXP (x, 0), 0);

	      if (inner == virtual_incoming_args_rtx)
		new = arg_pointer_rtx, offset = in_arg_offset;
	      else if (inner == virtual_stack_vars_rtx)
		new = frame_pointer_rtx, offset = var_offset;
	      else if (inner == virtual_stack_dynamic_rtx)
		new = stack_pointer_rtx, offset = dynamic_offset;
	      else if (inner == virtual_outgoing_args_rtx)
		new = stack_pointer_rtx, offset = out_arg_offset;
	      else
		{
		  loc = &XEXP (x, 0);
		  goto restart;
		}

	      instantiate_virtual_regs_1 (&XEXP (XEXP (x, 0), 1), object,
					  extra_insns);
	      new = gen_rtx (PLUS, Pmode, new, XEXP (XEXP (x, 0), 1));
	    }

	  else if (XEXP (x, 0) == virtual_incoming_args_rtx)
	    new = arg_pointer_rtx, offset = in_arg_offset;
	  else if (XEXP (x, 0) == virtual_stack_vars_rtx)
	    new = frame_pointer_rtx, offset = var_offset;
	  else if (XEXP (x, 0) == virtual_stack_dynamic_rtx)
	    new = stack_pointer_rtx, offset = dynamic_offset;
	  else if (XEXP (x, 0) == virtual_outgoing_args_rtx)
	    new = stack_pointer_rtx, offset = out_arg_offset;
	  else
	    {
	      /* We know the second operand is a constant.  Unless the
		 first operand is a REG (which has been already checked),
		 it needs to be checked.  */
	      if (GET_CODE (XEXP (x, 0)) != REG)
		{
		  loc = &XEXP (x, 0);
		  goto restart;
		}
	      return 1;
	    }

	  old = XEXP (x, 0);
	  XEXP (x, 0) = new;
	  new = plus_constant (XEXP (x, 1), offset);

	  /* If the new constant is zero, try to replace the sum with its
	     first operand.  */
	  if (new == const0_rtx
	      && validate_change (object, loc, XEXP (x, 0), 0))
	    return 1;

	  /* Next try to replace constant with new one.  */
	  if (!validate_change (object, &XEXP (x, 1), new, 0))
	    {
	      if (! extra_insns)
		{
		  XEXP (x, 0) = old;
		  return 0;
		}

	      /* Otherwise copy the new constant into a register and replace
		 constant with that register.  */
	      temp = gen_reg_rtx (Pmode);
	      if (validate_change (object, &XEXP (x, 1), temp, 0))
		emit_insn_before (gen_move_insn (temp, new), object);
	      else
		{
		  /* If that didn't work, replace this expression with a
		     register containing the sum.  */

		  new = gen_rtx (PLUS, Pmode, XEXP (x, 0), new);
		  XEXP (x, 0) = old;

		  start_sequence ();
		  temp = force_operand (new, NULL_RTX);
		  seq = get_insns ();
		  end_sequence ();

		  emit_insns_before (seq, object);
		  if (! validate_change (object, loc, temp, 0)
		      && ! validate_replace_rtx (x, temp, object))
		    abort ();
		}
	    }

	  return 1;
	}

      /* Fall through to generic two-operand expression case.  */
    case EXPR_LIST:
    case CALL:
    case COMPARE:
    case MINUS:
    case MULT:
    case DIV:      case UDIV:
    case MOD:      case UMOD:
    case AND:      case IOR:      case XOR:
    case LSHIFT:   case ASHIFT:   case ROTATE:
    case ASHIFTRT: case LSHIFTRT: case ROTATERT:
    case NE:       case EQ:
    case GE:       case GT:       case GEU:    case GTU:
    case LE:       case LT:       case LEU:    case LTU:
      if (XEXP (x, 1) && ! CONSTANT_P (XEXP (x, 1)))
	instantiate_virtual_regs_1 (&XEXP (x, 1), object, extra_insns);
      loc = &XEXP (x, 0);
      goto restart;

    case MEM:
      /* Most cases of MEM that convert to valid addresses have already been
	 handled by our scan of regno_reg_rtx.  The only special handling we
	 need here is to make a copy of the rtx to ensure it isn't being
	 shared if we have to change it to a pseudo. 

	 If the rtx is a simple reference to an address via a virtual register,
	 it can potentially be shared.  In such cases, first try to make it
	 a valid address, which can also be shared.  Otherwise, copy it and
	 proceed normally. 

	 First check for common cases that need no processing.  These are
	 usually due to instantiation already being done on a previous instance
	 of a shared rtx.  */

      temp = XEXP (x, 0);
      if (CONSTANT_ADDRESS_P (temp)
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  || temp == arg_pointer_rtx
#endif
	  || temp == frame_pointer_rtx)
	return 1;

      if (GET_CODE (temp) == PLUS
	  && CONSTANT_ADDRESS_P (XEXP (temp, 1))
	  && (XEXP (temp, 0) == frame_pointer_rtx
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      || XEXP (temp, 0) == arg_pointer_rtx
#endif
	      ))
	return 1;

      if (temp == virtual_stack_vars_rtx
	  || temp == virtual_incoming_args_rtx
	  || (GET_CODE (temp) == PLUS
	      && CONSTANT_ADDRESS_P (XEXP (temp, 1))
	      && (XEXP (temp, 0) == virtual_stack_vars_rtx
		  || XEXP (temp, 0) == virtual_incoming_args_rtx)))
	{
	  /* This MEM may be shared.  If the substitution can be done without
	     the need to generate new pseudos, we want to do it in place
	     so all copies of the shared rtx benefit.  The call below will
	     only make substitutions if the resulting address is still
	     valid.

	     Note that we cannot pass X as the object in the recursive call
	     since the insn being processed may not allow all valid
	     addresses.  However, if we were not passed on object, we can
	     only modify X without copying it if X will have a valid
	     address.

	     ??? Also note that this can still lose if OBJECT is an insn that
	     has less restrictions on an address that some other insn.
	     In that case, we will modify the shared address.  This case
	     doesn't seem very likely, though.  */

	  if (instantiate_virtual_regs_1 (&XEXP (x, 0),
					  object ? object : x, 0))
	    return 1;

	  /* Otherwise make a copy and process that copy.  We copy the entire
	     RTL expression since it might be a PLUS which could also be
	     shared.  */
	  *loc = x = copy_rtx (x);
	}

      /* Fall through to generic unary operation case.  */
    case USE:
    case CLOBBER:
    case SUBREG:
    case STRICT_LOW_PART:
    case NEG:          case NOT:
    case PRE_DEC:      case PRE_INC:      case POST_DEC:    case POST_INC:
    case SIGN_EXTEND:  case ZERO_EXTEND:
    case TRUNCATE:     case FLOAT_EXTEND: case FLOAT_TRUNCATE:
    case FLOAT:        case FIX:
    case UNSIGNED_FIX: case UNSIGNED_FLOAT:
    case ABS:
    case SQRT:
    case FFS:
      /* These case either have just one operand or we know that we need not
	 check the rest of the operands.  */
      loc = &XEXP (x, 0);
      goto restart;

    case REG:
      /* Try to replace with a PLUS.  If that doesn't work, compute the sum
	 in front of this insn and substitute the temporary.  */
      if (x == virtual_incoming_args_rtx)
	new = arg_pointer_rtx, offset = in_arg_offset;
      else if (x == virtual_stack_vars_rtx)
	new = frame_pointer_rtx, offset = var_offset;
      else if (x == virtual_stack_dynamic_rtx)
	new = stack_pointer_rtx, offset = dynamic_offset;
      else if (x == virtual_outgoing_args_rtx)
	new = stack_pointer_rtx, offset = out_arg_offset;

      if (new)
	{
	  temp = plus_constant (new, offset);
	  if (!validate_change (object, loc, temp, 0))
	    {
	      if (! extra_insns)
		return 0;

	      start_sequence ();
	      temp = force_operand (temp, NULL_RTX);
	      seq = get_insns ();
	      end_sequence ();

	      emit_insns_before (seq, object);
	      if (! validate_change (object, loc, temp, 0)
		  && ! validate_replace_rtx (x, temp, object))
		abort ();
	    }
	}

      return 1;
    }

  /* Scan all subexpressions.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    if (*fmt == 'e')
      {
	if (!instantiate_virtual_regs_1 (&XEXP (x, i), object, extra_insns))
	  return 0;
      }
    else if (*fmt == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	if (! instantiate_virtual_regs_1 (&XVECEXP (x, i, j), object,
					  extra_insns))
	  return 0;

  return 1;
}

/* Optimization: assuming this function does not receive nonlocal gotos,
   delete the handlers for such, as well as the insns to establish
   and disestablish them.  */

static void
delete_handlers ()
{
  rtx insn;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      /* Delete the handler by turning off the flag that would
	 prevent jump_optimize from deleting it.
	 Also permit deletion of the nonlocal labels themselves
	 if nothing local refers to them.  */
      if (GET_CODE (insn) == CODE_LABEL)
	LABEL_PRESERVE_P (insn) = 0;
      if (GET_CODE (insn) == INSN
	  && ((nonlocal_goto_handler_slot != 0
	       && reg_mentioned_p (nonlocal_goto_handler_slot, PATTERN (insn)))
	      || (nonlocal_goto_stack_level != 0
		  && reg_mentioned_p (nonlocal_goto_stack_level,
				      PATTERN (insn)))))
	delete_insn (insn);
    }
}

/* Return a list (chain of EXPR_LIST nodes) for the nonlocal labels
   of the current function.  */

rtx
nonlocal_label_rtx_list ()
{
  tree t;
  rtx x = 0;

  for (t = nonlocal_labels; t; t = TREE_CHAIN (t))
    x = gen_rtx (EXPR_LIST, VOIDmode, label_rtx (TREE_VALUE (t)), x);

  return x;
}

/* Output a USE for any register use in RTL.
   This is used with -noreg to mark the extent of lifespan
   of any registers used in a user-visible variable's DECL_RTL.  */

void
use_variable (rtl)
     rtx rtl;
{
  if (GET_CODE (rtl) == REG)
    /* This is a register variable.  */
    emit_insn (gen_rtx (USE, VOIDmode, rtl));
  else if (GET_CODE (rtl) == MEM
	   && GET_CODE (XEXP (rtl, 0)) == REG
	   && (REGNO (XEXP (rtl, 0)) < FIRST_VIRTUAL_REGISTER
	       || REGNO (XEXP (rtl, 0)) > LAST_VIRTUAL_REGISTER)
	   && XEXP (rtl, 0) != current_function_internal_arg_pointer)
    /* This is a variable-sized structure.  */
    emit_insn (gen_rtx (USE, VOIDmode, XEXP (rtl, 0)));
}

/* Like use_variable except that it outputs the USEs after INSN
   instead of at the end of the insn-chain.  */

void
use_variable_after (rtl, insn)
     rtx rtl, insn;
{
  if (GET_CODE (rtl) == REG)
    /* This is a register variable.  */
    emit_insn_after (gen_rtx (USE, VOIDmode, rtl), insn);
  else if (GET_CODE (rtl) == MEM
	   && GET_CODE (XEXP (rtl, 0)) == REG
	   && (REGNO (XEXP (rtl, 0)) < FIRST_VIRTUAL_REGISTER
	       || REGNO (XEXP (rtl, 0)) > LAST_VIRTUAL_REGISTER)
	   && XEXP (rtl, 0) != current_function_internal_arg_pointer)
    /* This is a variable-sized structure.  */
    emit_insn_after (gen_rtx (USE, VOIDmode, XEXP (rtl, 0)), insn);
}

int
max_parm_reg_num ()
{
  return max_parm_reg;
}

/* Return the first insn following those generated by `assign_parms'.  */

rtx
get_first_nonparm_insn ()
{
  if (last_parm_insn)
    return NEXT_INSN (last_parm_insn);
  return get_insns ();
}

/* Return the first NOTE_INSN_BLOCK_BEG note in the function.
   Crash if there is none.  */

rtx
get_first_block_beg ()
{
  register rtx searcher;
  register rtx insn = get_first_nonparm_insn ();

  for (searcher = insn; searcher; searcher = NEXT_INSN (searcher))
    if (GET_CODE (searcher) == NOTE
	&& NOTE_LINE_NUMBER (searcher) == NOTE_INSN_BLOCK_BEG)
      return searcher;

  abort ();	/* Invalid call to this function.  (See comments above.)  */
  return NULL_RTX;
}

/* Return 1 if EXP returns an aggregate value, for which an address
   must be passed to the function or returned by the function.  */

int
aggregate_value_p (exp)
     tree exp;
{
  int i, regno, nregs;
  rtx reg;
  if (TYPE_MODE (TREE_TYPE (exp)) == BLKmode)
    return 1;
  if (RETURN_IN_MEMORY (TREE_TYPE (exp)))
    return 1;
  if (flag_pcc_struct_return
      && (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE))
    return 1;
  /* Make sure we have suitable call-clobbered regs to return
     the value in; if not, we must return it in memory.  */
  reg = hard_function_value (TREE_TYPE (exp), 0);
  regno = REGNO (reg);
  nregs = HARD_REGNO_NREGS (regno, TYPE_MODE (TREE_TYPE (exp)));
  for (i = 0; i < nregs; i++)
    if (! call_used_regs[regno + i])
      return 1;
  return 0;
}

/* Assign RTL expressions to the function's parameters.
   This may involve copying them into registers and using
   those registers as the RTL for them.

   If SECOND_TIME is non-zero it means that this function is being
   called a second time.  This is done by integrate.c when a function's
   compilation is deferred.  We need to come back here in case the
   FUNCTION_ARG macro computes items needed for the rest of the compilation
   (such as changing which registers are fixed or caller-saved).  But suppress
   writing any insns or setting DECL_RTL of anything in this case.  */

void
assign_parms (fndecl, second_time)
     tree fndecl;
     int second_time;
{
  register tree parm;
  register rtx entry_parm = 0;
  register rtx stack_parm = 0;
  CUMULATIVE_ARGS args_so_far;
  enum machine_mode promoted_mode, passed_mode, nominal_mode;
  int unsignedp;
  /* Total space needed so far for args on the stack,
     given as a constant and a tree-expression.  */
  struct args_size stack_args_size;
  tree fntype = TREE_TYPE (fndecl);
  tree fnargs = DECL_ARGUMENTS (fndecl);
  /* This is used for the arg pointer when referring to stack args.  */
  rtx internal_arg_pointer;
  /* This is a dummy PARM_DECL that we used for the function result if 
     the function returns a structure.  */
  tree function_result_decl = 0;
  int nparmregs = list_length (fnargs) + LAST_VIRTUAL_REGISTER + 1;
  int varargs_setup = 0;

  /* Nonzero if the last arg is named `__builtin_va_alist',
     which is used on some machines for old-fashioned non-ANSI varargs.h;
     this should be stuck onto the stack as if it had arrived there.  */
  int vararg
    = (fnargs
       && (parm = tree_last (fnargs)) != 0
       && DECL_NAME (parm)
       && (! strcmp (IDENTIFIER_POINTER (DECL_NAME (parm)),
		     "__builtin_va_alist")));

  /* Nonzero if function takes extra anonymous args.
     This means the last named arg must be on the stack
     right before the anonymous ones. */
  int stdarg
    = (TYPE_ARG_TYPES (fntype) != 0
       && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	   != void_type_node));

  /* If the reg that the virtual arg pointer will be translated into is
     not a fixed reg or is the stack pointer, make a copy of the virtual
     arg pointer, and address parms via the copy.  The frame pointer is
     considered fixed even though it is not marked as such.

     The second time through, simply use ap to avoid generating rtx.  */

  if ((ARG_POINTER_REGNUM == STACK_POINTER_REGNUM
       || ! (fixed_regs[ARG_POINTER_REGNUM]
	     || ARG_POINTER_REGNUM == FRAME_POINTER_REGNUM))
      && ! second_time)
    internal_arg_pointer = copy_to_reg (virtual_incoming_args_rtx);
  else
    internal_arg_pointer = virtual_incoming_args_rtx;
  current_function_internal_arg_pointer = internal_arg_pointer;

  stack_args_size.constant = 0;
  stack_args_size.var = 0;

  /* If struct value address is treated as the first argument, make it so.  */
  if (aggregate_value_p (DECL_RESULT (fndecl))
      && ! current_function_returns_pcc_struct
      && struct_value_incoming_rtx == 0)
    {
      tree type = build_pointer_type (fntype);

      function_result_decl = build_decl (PARM_DECL, NULL_TREE, type);

      DECL_ARG_TYPE (function_result_decl) = type;
      TREE_CHAIN (function_result_decl) = fnargs;
      fnargs = function_result_decl;
    }
			       
  parm_reg_stack_loc = (rtx *) oballoc (nparmregs * sizeof (rtx));
  bzero (parm_reg_stack_loc, nparmregs * sizeof (rtx));

#ifdef INIT_CUMULATIVE_INCOMING_ARGS
  INIT_CUMULATIVE_INCOMING_ARGS (args_so_far, fntype, NULL_PTR);
#else
  INIT_CUMULATIVE_ARGS (args_so_far, fntype, NULL_PTR);
#endif

  /* We haven't yet found an argument that we must push and pretend the
     caller did.  */
  current_function_pretend_args_size = 0;

  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    {
      int aggregate
	= (TREE_CODE (TREE_TYPE (parm)) == ARRAY_TYPE
	   || TREE_CODE (TREE_TYPE (parm)) == RECORD_TYPE
	   || TREE_CODE (TREE_TYPE (parm)) == UNION_TYPE);
      struct args_size stack_offset;
      struct args_size arg_size;
      int passed_pointer = 0;
      tree passed_type = DECL_ARG_TYPE (parm);

      /* Set LAST_NAMED if this is last named arg before some
	 anonymous args.  We treat it as if it were anonymous too.  */
      int last_named = ((TREE_CHAIN (parm) == 0
			 || DECL_NAME (TREE_CHAIN (parm)) == 0)
			&& (vararg || stdarg));

      if (TREE_TYPE (parm) == error_mark_node
	  /* This can happen after weird syntax errors
	     or if an enum type is defined among the parms.  */
	  || TREE_CODE (parm) != PARM_DECL
	  || passed_type == NULL)
	{
	  DECL_INCOMING_RTL (parm) = DECL_RTL (parm) = gen_rtx (MEM, BLKmode,
								const0_rtx);
	  TREE_USED (parm) = 1;
	  continue;
	}

      /* For varargs.h function, save info about regs and stack space
	 used by the individual args, not including the va_alist arg.  */
      if (vararg && last_named)
	current_function_args_info = args_so_far;

      /* Find mode of arg as it is passed, and mode of arg
	 as it should be during execution of this function.  */
      passed_mode = TYPE_MODE (passed_type);
      nominal_mode = TYPE_MODE (TREE_TYPE (parm));

      /* If the parm's mode is VOID, its value doesn't matter,
	 and avoid the usual things like emit_move_insn that could crash.  */
      if (nominal_mode == VOIDmode)
	{
	  DECL_INCOMING_RTL (parm) = DECL_RTL (parm) = const0_rtx;
	  continue;
	}

#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
      /* See if this arg was passed by invisible reference.  */
      if (FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, passed_mode,
					  passed_type, ! last_named))
	{
	  passed_type = build_pointer_type (passed_type);
	  passed_pointer = 1;
	  passed_mode = nominal_mode = Pmode;
	}
#endif

      promoted_mode = passed_mode;

#ifdef PROMOTE_FUNCTION_ARGS
      /* Compute the mode in which the arg is actually extended to.  */
      if (TREE_CODE (passed_type) == INTEGER_TYPE
	  || TREE_CODE (passed_type) == ENUMERAL_TYPE
	  || TREE_CODE (passed_type) == BOOLEAN_TYPE
	  || TREE_CODE (passed_type) == CHAR_TYPE
	  || TREE_CODE (passed_type) == REAL_TYPE
	  || TREE_CODE (passed_type) == POINTER_TYPE
	  || TREE_CODE (passed_type) == OFFSET_TYPE)
	{
	  unsignedp = TREE_UNSIGNED (passed_type);
	  PROMOTE_MODE (promoted_mode, unsignedp, passed_type);
	}
#endif

      /* Let machine desc say which reg (if any) the parm arrives in.
	 0 means it arrives on the stack.  */
#ifdef FUNCTION_INCOMING_ARG
      entry_parm = FUNCTION_INCOMING_ARG (args_so_far, promoted_mode,
					  passed_type, ! last_named);
#else
      entry_parm = FUNCTION_ARG (args_so_far, promoted_mode,
				 passed_type, ! last_named);
#endif

      if (entry_parm)
	passed_mode = promoted_mode;

#ifdef SETUP_INCOMING_VARARGS
      /* If this is the last named parameter, do any required setup for
	 varargs or stdargs.  We need to know about the case of this being an
	 addressable type, in which case we skip the registers it
	 would have arrived in.

	 For stdargs, LAST_NAMED will be set for two parameters, the one that
	 is actually the last named, and the dummy parameter.  We only
	 want to do this action once.

	 Also, indicate when RTL generation is to be suppressed.  */
      if (last_named && !varargs_setup)
	{
	  SETUP_INCOMING_VARARGS (args_so_far, passed_mode, passed_type,
				  current_function_pretend_args_size,
				  second_time);
	  varargs_setup = 1;
	}
#endif

      /* Determine parm's home in the stack,
	 in case it arrives in the stack or we should pretend it did.

	 Compute the stack position and rtx where the argument arrives
	 and its size.

	 There is one complexity here:  If this was a parameter that would
	 have been passed in registers, but wasn't only because it is
	 __builtin_va_alist, we want locate_and_pad_parm to treat it as if
	 it came in a register so that REG_PARM_STACK_SPACE isn't skipped.
	 In this case, we call FUNCTION_ARG with NAMED set to 1 instead of
	 0 as it was the previous time.  */

      locate_and_pad_parm (passed_mode, passed_type,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			   1,
#else
#ifdef FUNCTION_INCOMING_ARG
			   FUNCTION_INCOMING_ARG (args_so_far, passed_mode,
						  passed_type,
						  (! last_named
						   || varargs_setup)) != 0,
#else
			   FUNCTION_ARG (args_so_far, passed_mode,
					 passed_type,
					 ! last_named || varargs_setup) != 0,
#endif
#endif
			   fndecl, &stack_args_size, &stack_offset, &arg_size);

      if (! second_time)
	{
	  rtx offset_rtx = ARGS_SIZE_RTX (stack_offset);

	  if (offset_rtx == const0_rtx)
	    stack_parm = gen_rtx (MEM, passed_mode, internal_arg_pointer);
	  else
	    stack_parm = gen_rtx (MEM, passed_mode,
				  gen_rtx (PLUS, Pmode,
					   internal_arg_pointer, offset_rtx));

	  /* If this is a memory ref that contains aggregate components,
	     mark it as such for cse and loop optimize.  */
	  MEM_IN_STRUCT_P (stack_parm) = aggregate;
	}

      /* If this parameter was passed both in registers and in the stack,
	 use the copy on the stack.  */
      if (MUST_PASS_IN_STACK (passed_mode, passed_type))
	entry_parm = 0;

      /* If this parm was passed part in regs and part in memory,
	 pretend it arrived entirely in memory
	 by pushing the register-part onto the stack.

	 In the special case of a DImode or DFmode that is split,
	 we could put it together in a pseudoreg directly,
	 but for now that's not worth bothering with.  */

      if (entry_parm)
	{
	  int nregs = 0;
#ifdef FUNCTION_ARG_PARTIAL_NREGS
	  nregs = FUNCTION_ARG_PARTIAL_NREGS (args_so_far, passed_mode,
					      passed_type, ! last_named);
#endif

	  if (nregs > 0)
	    {
	      current_function_pretend_args_size
		= (((nregs * UNITS_PER_WORD) + (PARM_BOUNDARY / BITS_PER_UNIT) - 1)
		   / (PARM_BOUNDARY / BITS_PER_UNIT)
		   * (PARM_BOUNDARY / BITS_PER_UNIT));

	      if (! second_time)
		move_block_from_reg (REGNO (entry_parm),
				     validize_mem (stack_parm), nregs);
	      entry_parm = stack_parm;
	    }
	}

      /* If we didn't decide this parm came in a register,
	 by default it came on the stack.  */
      if (entry_parm == 0)
	entry_parm = stack_parm;

      /* Record permanently how this parm was passed.  */
      if (! second_time)
	DECL_INCOMING_RTL (parm) = entry_parm;

      /* If there is actually space on the stack for this parm,
	 count it in stack_args_size; otherwise set stack_parm to 0
	 to indicate there is no preallocated stack slot for the parm.  */

      if (entry_parm == stack_parm
#if defined (REG_PARM_STACK_SPACE) && ! defined (MAYBE_REG_PARM_STACK_SPACE)
	  /* On some machines, even if a parm value arrives in a register
	     there is still an (uninitialized) stack slot allocated for it.

	     ??? When MAYBE_REG_PARM_STACK_SPACE is defined, we can't tell
	     whether this parameter already has a stack slot allocated,
	     because an arg block exists only if current_function_args_size
	     is larger than some threshhold, and we haven't calculated that
	     yet.  So, for now, we just assume that stack slots never exist
	     in this case.  */
	  || REG_PARM_STACK_SPACE (fndecl) > 0
#endif
	  )
	{
	  stack_args_size.constant += arg_size.constant;
	  if (arg_size.var)
	    ADD_PARM_SIZE (stack_args_size, arg_size.var);
	}
      else
	/* No stack slot was pushed for this parm.  */
	stack_parm = 0;

      /* Update info on where next arg arrives in registers.  */

      FUNCTION_ARG_ADVANCE (args_so_far, passed_mode,
			    passed_type, ! last_named);

      /* If this is our second time through, we are done with this parm. */
      if (second_time)
	continue;

      /* If we can't trust the parm stack slot to be aligned enough
	 for its ultimate type, don't use that slot after entry.
	 We'll make another stack slot, if we need one.  */
      {
#ifdef FUNCTION_ARG_BOUNDARY
	int thisparm_boundary
	  = FUNCTION_ARG_BOUNDARY (passed_mode, passed_type);
#else
	int thisparm_boundary = PARM_BOUNDARY;
#endif

	if (GET_MODE_ALIGNMENT (nominal_mode) > thisparm_boundary)
	  stack_parm = 0;
      }

      /* Now adjust STACK_PARM to the mode and precise location
	 where this parameter should live during execution,
	 if we discover that it must live in the stack during execution.
	 To make debuggers happier on big-endian machines, we store
	 the value in the last bytes of the space available.  */

      if (nominal_mode != BLKmode && nominal_mode != passed_mode
	  && stack_parm != 0)
	{
	  rtx offset_rtx;

#if BYTES_BIG_ENDIAN
	  if (GET_MODE_SIZE (nominal_mode) < UNITS_PER_WORD)
	    stack_offset.constant += (GET_MODE_SIZE (passed_mode)
				      - GET_MODE_SIZE (nominal_mode));
#endif

	  offset_rtx = ARGS_SIZE_RTX (stack_offset);
	  if (offset_rtx == const0_rtx)
	    stack_parm = gen_rtx (MEM, nominal_mode, internal_arg_pointer);
	  else
	    stack_parm = gen_rtx (MEM, nominal_mode,
				  gen_rtx (PLUS, Pmode,
					   internal_arg_pointer, offset_rtx));

	  /* If this is a memory ref that contains aggregate components,
	     mark it as such for cse and loop optimize.  */
	  MEM_IN_STRUCT_P (stack_parm) = aggregate;
	}

      /* ENTRY_PARM is an RTX for the parameter as it arrives,
	 in the mode in which it arrives.
	 STACK_PARM is an RTX for a stack slot where the parameter can live
	 during the function (in case we want to put it there).
	 STACK_PARM is 0 if no stack slot was pushed for it.

	 Now output code if necessary to convert ENTRY_PARM to
	 the type in which this function declares it,
	 and store that result in an appropriate place,
	 which may be a pseudo reg, may be STACK_PARM,
	 or may be a local stack slot if STACK_PARM is 0.

	 Set DECL_RTL to that place.  */

      if (nominal_mode == BLKmode)
	{
	  /* If a BLKmode arrives in registers, copy it to a stack slot.  */
	  if (GET_CODE (entry_parm) == REG)
	    {
	      int size_stored = CEIL_ROUND (int_size_in_bytes (TREE_TYPE (parm)),
					    UNITS_PER_WORD);

	      /* Note that we will be storing an integral number of words.
		 So we have to be careful to ensure that we allocate an
		 integral number of words.  We do this below in the
		 assign_stack_local if space was not allocated in the argument
		 list.  If it was, this will not work if PARM_BOUNDARY is not
		 a multiple of BITS_PER_WORD.  It isn't clear how to fix this
		 if it becomes a problem.  */

	      if (stack_parm == 0)
		{
		  stack_parm
		    = assign_stack_local (GET_MODE (entry_parm), size_stored, 0);
		  /* If this is a memory ref that contains aggregate components,
		     mark it as such for cse and loop optimize.  */
		  MEM_IN_STRUCT_P (stack_parm) = aggregate;
		}

	      else if (PARM_BOUNDARY % BITS_PER_WORD != 0)
		abort ();

	      move_block_from_reg (REGNO (entry_parm),
				   validize_mem (stack_parm),
				   size_stored / UNITS_PER_WORD);
	    }
	  DECL_RTL (parm) = stack_parm;
	}
      else if (! ((obey_regdecls && ! DECL_REGISTER (parm)
		   && ! DECL_INLINE (fndecl))
		  /* layout_decl may set this.  */
		  || TREE_ADDRESSABLE (parm)
		  || TREE_SIDE_EFFECTS (parm)
		  /* If -ffloat-store specified, don't put explicit
		     float variables into registers.  */
		  || (flag_float_store
		      && TREE_CODE (TREE_TYPE (parm)) == REAL_TYPE))
	       /* Always assign pseudo to structure return or item passed
		  by invisible reference.  */
	       || passed_pointer || parm == function_result_decl)
	{
	  /* Store the parm in a pseudoregister during the function, but we
	     may need to do it in a wider mode.  */

	  register rtx parmreg;

	  unsignedp = TREE_UNSIGNED (TREE_TYPE (parm));
	  if (TREE_CODE (TREE_TYPE (parm)) == INTEGER_TYPE
	      || TREE_CODE (TREE_TYPE (parm)) == ENUMERAL_TYPE
	      || TREE_CODE (TREE_TYPE (parm)) == BOOLEAN_TYPE
	      || TREE_CODE (TREE_TYPE (parm)) == CHAR_TYPE
	      || TREE_CODE (TREE_TYPE (parm)) == REAL_TYPE
	      || TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE
	      || TREE_CODE (TREE_TYPE (parm)) == OFFSET_TYPE)
	    {
	      PROMOTE_MODE (nominal_mode, unsignedp, TREE_TYPE (parm));
	    }

	  parmreg = gen_reg_rtx (nominal_mode);
	  REG_USERVAR_P (parmreg) = 1;

	  /* If this was an item that we received a pointer to, set DECL_RTL
	     appropriately.  */
	  if (passed_pointer)
	    {
	      DECL_RTL (parm) = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (passed_type)), parmreg);
	      MEM_IN_STRUCT_P (DECL_RTL (parm)) = aggregate;
	    }
	  else
	    DECL_RTL (parm) = parmreg;

	  /* Copy the value into the register.  */
	  if (GET_MODE (parmreg) != GET_MODE (entry_parm))
	    {
	      /* If ENTRY_PARM is a hard register, it might be in a register
		 not valid for operating in its mode (e.g., an odd-numbered
		 register for a DFmode).  In that case, moves are the only
		 thing valid, so we can't do a convert from there.  This
		 occurs when the calling sequence allow such misaligned
		 usages.  */
	      if (GET_CODE (entry_parm) == REG
		  && REGNO (entry_parm) < FIRST_PSEUDO_REGISTER
		  && ! HARD_REGNO_MODE_OK (REGNO (entry_parm),
					   GET_MODE (entry_parm)))
		convert_move (parmreg, copy_to_reg (entry_parm), unsignedp);
	      else
		convert_move (parmreg, validize_mem (entry_parm), unsignedp);
	    }
	  else
	    emit_move_insn (parmreg, validize_mem (entry_parm));

	  /* If we were passed a pointer but the actual value
	     can safely live in a register, put it in one.  */
	  if (passed_pointer && TYPE_MODE (TREE_TYPE (parm)) != BLKmode
	      && ! ((obey_regdecls && ! DECL_REGISTER (parm)
		     && ! DECL_INLINE (fndecl))
		    /* layout_decl may set this.  */
		    || TREE_ADDRESSABLE (parm)
		    || TREE_SIDE_EFFECTS (parm)
		    /* If -ffloat-store specified, don't put explicit
		       float variables into registers.  */
		    || (flag_float_store
			&& TREE_CODE (TREE_TYPE (parm)) == REAL_TYPE)))
	    {
	      /* We can't use nominal_mode, because it will have been set to
		 Pmode above.  We must use the actual mode of the parm.  */
	      parmreg = gen_reg_rtx (TYPE_MODE (TREE_TYPE (parm)));
	      emit_move_insn (parmreg, DECL_RTL (parm));
	      DECL_RTL (parm) = parmreg;
	    }

	  /* In any case, record the parm's desired stack location
	     in case we later discover it must live in the stack.  */
	  if (REGNO (parmreg) >= nparmregs)
	    {
	      rtx *new;
	      nparmregs = REGNO (parmreg) + 5;
	      new = (rtx *) oballoc (nparmregs * sizeof (rtx));
	      bcopy (parm_reg_stack_loc, new, nparmregs * sizeof (rtx));
	      parm_reg_stack_loc = new;
	    }
	  parm_reg_stack_loc[REGNO (parmreg)] = stack_parm;

	  /* Mark the register as eliminable if we did no conversion
	     and it was copied from memory at a fixed offset,
	     and the arg pointer was not copied to a pseudo-reg.
	     If the arg pointer is a pseudo reg or the offset formed
	     an invalid address, such memory-equivalences
	     as we make here would screw up life analysis for it.  */
	  if (nominal_mode == passed_mode
	      && GET_CODE (entry_parm) == MEM
	      && entry_parm == stack_parm
	      && stack_offset.var == 0
	      && reg_mentioned_p (virtual_incoming_args_rtx,
				  XEXP (entry_parm, 0)))
	    REG_NOTES (get_last_insn ())
	      = gen_rtx (EXPR_LIST, REG_EQUIV,
			 entry_parm, REG_NOTES (get_last_insn ()));

	  /* For pointer data type, suggest pointer register.  */
	  if (TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE)
	    mark_reg_pointer (parmreg);
	}
      else
	{
	  /* Value must be stored in the stack slot STACK_PARM
	     during function execution.  */

	  if (passed_mode != nominal_mode)
	    {
	      /* Conversion is required.   */
	      if (GET_CODE (entry_parm) == REG
		  && REGNO (entry_parm) < FIRST_PSEUDO_REGISTER
		  && ! HARD_REGNO_MODE_OK (REGNO (entry_parm), passed_mode))
		entry_parm = copy_to_reg (entry_parm);

	      entry_parm = convert_to_mode (nominal_mode, entry_parm,
					    TREE_UNSIGNED (TREE_TYPE (parm)));
	    }

	  if (entry_parm != stack_parm)
	    {
	      if (stack_parm == 0)
		{
		  stack_parm
		    = assign_stack_local (GET_MODE (entry_parm),
					  GET_MODE_SIZE (GET_MODE (entry_parm)), 0);
		  /* If this is a memory ref that contains aggregate components,
		     mark it as such for cse and loop optimize.  */
		  MEM_IN_STRUCT_P (stack_parm) = aggregate;
		}

	      emit_move_insn (validize_mem (stack_parm),
			      validize_mem (entry_parm));
	    }

	  DECL_RTL (parm) = stack_parm;
	}
      
      /* If this "parameter" was the place where we are receiving the
	 function's incoming structure pointer, set up the result.  */
      if (parm == function_result_decl)
	DECL_RTL (DECL_RESULT (fndecl))
	  = gen_rtx (MEM, DECL_MODE (DECL_RESULT (fndecl)), DECL_RTL (parm));

      if (TREE_THIS_VOLATILE (parm))
	MEM_VOLATILE_P (DECL_RTL (parm)) = 1;
      if (TREE_READONLY (parm))
	RTX_UNCHANGING_P (DECL_RTL (parm)) = 1;
    }

  max_parm_reg = max_reg_num ();
  last_parm_insn = get_last_insn ();

  current_function_args_size = stack_args_size.constant;

  /* Adjust function incoming argument size for alignment and
     minimum length.  */

#ifdef REG_PARM_STACK_SPACE
#ifndef MAYBE_REG_PARM_STACK_SPACE
  current_function_args_size = MAX (current_function_args_size,
				    REG_PARM_STACK_SPACE (fndecl));
#endif
#endif

#ifdef STACK_BOUNDARY
#define STACK_BYTES (STACK_BOUNDARY / BITS_PER_UNIT)

  current_function_args_size
    = ((current_function_args_size + STACK_BYTES - 1)
       / STACK_BYTES) * STACK_BYTES;
#endif  

#ifdef ARGS_GROW_DOWNWARD
  current_function_arg_offset_rtx
    = (stack_args_size.var == 0 ? GEN_INT (-stack_args_size.constant)
       : expand_expr (size_binop (MINUS_EXPR, stack_args_size.var,	
				  size_int (-stack_args_size.constant)),   
		      NULL_RTX, VOIDmode, 0));
#else
  current_function_arg_offset_rtx = ARGS_SIZE_RTX (stack_args_size);
#endif

  /* See how many bytes, if any, of its args a function should try to pop
     on return.  */

  current_function_pops_args = RETURN_POPS_ARGS (TREE_TYPE (fndecl),
						 current_function_args_size);

  /* For stdarg.h function, save info about regs and stack space
     used by the named args.  */

  if (stdarg)
    current_function_args_info = args_so_far;

  /* Set the rtx used for the function return value.  Put this in its
     own variable so any optimizers that need this information don't have
     to include tree.h.  Do this here so it gets done when an inlined
     function gets output.  */

  current_function_return_rtx = DECL_RTL (DECL_RESULT (fndecl));
}

/* Compute the size and offset from the start of the stacked arguments for a
   parm passed in mode PASSED_MODE and with type TYPE.

   INITIAL_OFFSET_PTR points to the current offset into the stacked
   arguments.

   The starting offset and size for this parm are returned in *OFFSET_PTR
   and *ARG_SIZE_PTR, respectively.

   IN_REGS is non-zero if the argument will be passed in registers.  It will
   never be set if REG_PARM_STACK_SPACE is not defined.

   FNDECL is the function in which the argument was defined.

   There are two types of rounding that are done.  The first, controlled by
   FUNCTION_ARG_BOUNDARY, forces the offset from the start of the argument
   list to be aligned to the specific boundary (in bits).  This rounding
   affects the initial and starting offsets, but not the argument size.

   The second, controlled by FUNCTION_ARG_PADDING and PARM_BOUNDARY,
   optionally rounds the size of the parm to PARM_BOUNDARY.  The
   initial offset is not affected by this rounding, while the size always
   is and the starting offset may be.  */

/*  offset_ptr will be negative for ARGS_GROW_DOWNWARD case; 
    initial_offset_ptr is positive because locate_and_pad_parm's
    callers pass in the total size of args so far as
    initial_offset_ptr. arg_size_ptr is always positive.*/

static void pad_to_arg_alignment (), pad_below ();

void
locate_and_pad_parm (passed_mode, type, in_regs, fndecl,
		     initial_offset_ptr, offset_ptr, arg_size_ptr)
     enum machine_mode passed_mode;
     tree type;
     int in_regs;
     tree fndecl;
     struct args_size *initial_offset_ptr;
     struct args_size *offset_ptr;
     struct args_size *arg_size_ptr;
{
  tree sizetree
    = type ? size_in_bytes (type) : size_int (GET_MODE_SIZE (passed_mode));
  enum direction where_pad = FUNCTION_ARG_PADDING (passed_mode, type);
  int boundary = FUNCTION_ARG_BOUNDARY (passed_mode, type);
  int boundary_in_bytes = boundary / BITS_PER_UNIT;
  int reg_parm_stack_space = 0;

#ifdef REG_PARM_STACK_SPACE
  /* If we have found a stack parm before we reach the end of the
     area reserved for registers, skip that area.  */
  if (! in_regs)
    {
#ifdef MAYBE_REG_PARM_STACK_SPACE
      reg_parm_stack_space = MAYBE_REG_PARM_STACK_SPACE;
#else
      reg_parm_stack_space = REG_PARM_STACK_SPACE (fndecl);
#endif
      if (reg_parm_stack_space > 0)
	{
	  if (initial_offset_ptr->var)
	    {
	      initial_offset_ptr->var
		= size_binop (MAX_EXPR, ARGS_SIZE_TREE (*initial_offset_ptr),
			      size_int (reg_parm_stack_space));
	      initial_offset_ptr->constant = 0;
	    }
	  else if (initial_offset_ptr->constant < reg_parm_stack_space)
	    initial_offset_ptr->constant = reg_parm_stack_space;
	}
    }
#endif /* REG_PARM_STACK_SPACE */

  arg_size_ptr->var = 0;
  arg_size_ptr->constant = 0;

#ifdef ARGS_GROW_DOWNWARD
  if (initial_offset_ptr->var)
    {
      offset_ptr->constant = 0;
      offset_ptr->var = size_binop (MINUS_EXPR, integer_zero_node,
				    initial_offset_ptr->var);
    }
  else
    {
      offset_ptr->constant = - initial_offset_ptr->constant;
      offset_ptr->var = 0;
    }
  if (where_pad == upward
      && (TREE_CODE (sizetree) != INTEGER_CST
	  || ((TREE_INT_CST_LOW (sizetree) * BITS_PER_UNIT) % PARM_BOUNDARY)))
    sizetree = round_up (sizetree, PARM_BOUNDARY / BITS_PER_UNIT);
  SUB_PARM_SIZE (*offset_ptr, sizetree);
  if (where_pad != downward)
    pad_to_arg_alignment (offset_ptr, boundary);
  if (initial_offset_ptr->var)
    {
      arg_size_ptr->var = size_binop (MINUS_EXPR,
				      size_binop (MINUS_EXPR,
						  integer_zero_node,
						  initial_offset_ptr->var),
				      offset_ptr->var);
    }
  else
    {
      arg_size_ptr->constant = (- initial_offset_ptr->constant -
				offset_ptr->constant); 
    }
/*  ADD_PARM_SIZE (*arg_size_ptr, sizetree); */
  if (where_pad == downward)
    pad_below (arg_size_ptr, passed_mode, sizetree);
#else /* !ARGS_GROW_DOWNWARD */
  pad_to_arg_alignment (initial_offset_ptr, boundary);
  *offset_ptr = *initial_offset_ptr;
  if (where_pad == downward)
    pad_below (offset_ptr, passed_mode, sizetree);

#ifdef PUSH_ROUNDING
  if (passed_mode != BLKmode)
    sizetree = size_int (PUSH_ROUNDING (TREE_INT_CST_LOW (sizetree)));
#endif

  if (where_pad != none
      && (TREE_CODE (sizetree) != INTEGER_CST
	  || ((TREE_INT_CST_LOW (sizetree) * BITS_PER_UNIT) % PARM_BOUNDARY)))
    sizetree = round_up (sizetree, PARM_BOUNDARY / BITS_PER_UNIT);

  ADD_PARM_SIZE (*arg_size_ptr, sizetree);
#endif /* ARGS_GROW_DOWNWARD */
}

/* Round the stack offset in *OFFSET_PTR up to a multiple of BOUNDARY.
   BOUNDARY is measured in bits, but must be a multiple of a storage unit.  */

static void
pad_to_arg_alignment (offset_ptr, boundary)
     struct args_size *offset_ptr;
     int boundary;
{
  int boundary_in_bytes = boundary / BITS_PER_UNIT;
  
  if (boundary > BITS_PER_UNIT)
    {
      if (offset_ptr->var)
	{
	  offset_ptr->var  =
#ifdef ARGS_GROW_DOWNWARD
	    round_down 
#else
	    round_up
#endif
	      (ARGS_SIZE_TREE (*offset_ptr),
	       boundary / BITS_PER_UNIT);
	  offset_ptr->constant = 0; /*?*/
	}
      else
	offset_ptr->constant =
#ifdef ARGS_GROW_DOWNWARD
	  FLOOR_ROUND (offset_ptr->constant, boundary_in_bytes);
#else
	  CEIL_ROUND (offset_ptr->constant, boundary_in_bytes);
#endif
    }
}

static void
pad_below (offset_ptr, passed_mode, sizetree)
     struct args_size *offset_ptr;
     enum machine_mode passed_mode;
     tree sizetree;
{
  if (passed_mode != BLKmode)
    {
      if (GET_MODE_BITSIZE (passed_mode) % PARM_BOUNDARY)
	offset_ptr->constant
	  += (((GET_MODE_BITSIZE (passed_mode) + PARM_BOUNDARY - 1)
	       / PARM_BOUNDARY * PARM_BOUNDARY / BITS_PER_UNIT)
	      - GET_MODE_SIZE (passed_mode));
    }
  else
    {
      if (TREE_CODE (sizetree) != INTEGER_CST
	  || (TREE_INT_CST_LOW (sizetree) * BITS_PER_UNIT) % PARM_BOUNDARY)
	{
	  /* Round the size up to multiple of PARM_BOUNDARY bits.  */
	  tree s2 = round_up (sizetree, PARM_BOUNDARY / BITS_PER_UNIT);
	  /* Add it in.  */
	  ADD_PARM_SIZE (*offset_ptr, s2);
	  SUB_PARM_SIZE (*offset_ptr, sizetree);
	}
    }
}

static tree
round_down (value, divisor)
     tree value;
     int divisor;
{
  return size_binop (MULT_EXPR,
		     size_binop (FLOOR_DIV_EXPR, value, size_int (divisor)),
		     size_int (divisor));
}

/* Walk the tree of blocks describing the binding levels within a function
   and warn about uninitialized variables.
   This is done after calling flow_analysis and before global_alloc
   clobbers the pseudo-regs to hard regs.  */

void
uninitialized_vars_warning (block)
     tree block;
{
  register tree decl, sub;
  for (decl = BLOCK_VARS (block); decl; decl = TREE_CHAIN (decl))
    {
      if (TREE_CODE (decl) == VAR_DECL
	  /* These warnings are unreliable for and aggregates
	     because assigning the fields one by one can fail to convince
	     flow.c that the entire aggregate was initialized.
	     Unions are troublesome because members may be shorter.  */
	  && TREE_CODE (TREE_TYPE (decl)) != RECORD_TYPE
	  && TREE_CODE (TREE_TYPE (decl)) != UNION_TYPE
	  && TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE
	  && DECL_RTL (decl) != 0
	  && GET_CODE (DECL_RTL (decl)) == REG
	  && regno_uninitialized (REGNO (DECL_RTL (decl))))
	warning_with_decl (decl,
			   "`%s' may be used uninitialized in this function");
      if (TREE_CODE (decl) == VAR_DECL
	  && DECL_RTL (decl) != 0
	  && GET_CODE (DECL_RTL (decl)) == REG
	  && regno_clobbered_at_setjmp (REGNO (DECL_RTL (decl))))
	warning_with_decl (decl,
			   "variable `%s' may be clobbered by `longjmp'");
    }
  for (sub = BLOCK_SUBBLOCKS (block); sub; sub = TREE_CHAIN (sub))
    uninitialized_vars_warning (sub);
}

/* Do the appropriate part of uninitialized_vars_warning
   but for arguments instead of local variables.  */

void
setjmp_args_warning (block)
     tree block;
{
  register tree decl;
  for (decl = DECL_ARGUMENTS (current_function_decl);
       decl; decl = TREE_CHAIN (decl))
    if (DECL_RTL (decl) != 0
	&& GET_CODE (DECL_RTL (decl)) == REG
	&& regno_clobbered_at_setjmp (REGNO (DECL_RTL (decl))))
      warning_with_decl (decl, "argument `%s' may be clobbered by `longjmp'");
}

/* If this function call setjmp, put all vars into the stack
   unless they were declared `register'.  */

void
setjmp_protect (block)
     tree block;
{
  register tree decl, sub;
  for (decl = BLOCK_VARS (block); decl; decl = TREE_CHAIN (decl))
    if ((TREE_CODE (decl) == VAR_DECL
	 || TREE_CODE (decl) == PARM_DECL)
	&& DECL_RTL (decl) != 0
	&& GET_CODE (DECL_RTL (decl)) == REG
	/* If this variable came from an inline function, it must be
	   that it's life doesn't overlap the setjmp.  If there was a
	   setjmp in the function, it would already be in memory.  We
	   must exclude such variable because their DECL_RTL might be
	   set to strange things such as virtual_stack_vars_rtx.  */
	&& ! DECL_FROM_INLINE (decl)
	&& (
#ifdef NON_SAVING_SETJMP
	    /* If longjmp doesn't restore the registers,
	       don't put anything in them.  */
	    NON_SAVING_SETJMP
	    ||
#endif
	    ! DECL_REGISTER (decl)))
      put_var_into_stack (decl);
  for (sub = BLOCK_SUBBLOCKS (block); sub; sub = TREE_CHAIN (sub))
    setjmp_protect (sub);
}

/* Like the previous function, but for args instead of local variables.  */

void
setjmp_protect_args ()
{
  register tree decl, sub;
  for (decl = DECL_ARGUMENTS (current_function_decl);
       decl; decl = TREE_CHAIN (decl))
    if ((TREE_CODE (decl) == VAR_DECL
	 || TREE_CODE (decl) == PARM_DECL)
	&& DECL_RTL (decl) != 0
	&& GET_CODE (DECL_RTL (decl)) == REG
	&& (
	    /* If longjmp doesn't restore the registers,
	       don't put anything in them.  */
#ifdef NON_SAVING_SETJMP
	    NON_SAVING_SETJMP
	    ||
#endif
	    ! DECL_REGISTER (decl)))
      put_var_into_stack (decl);
}

/* Return the context-pointer register corresponding to DECL,
   or 0 if it does not need one.  */

rtx
lookup_static_chain (decl)
     tree decl;
{
  tree context = decl_function_context (decl);
  tree link;

  if (context == 0)
    return 0;
  
  /* We treat inline_function_decl as an alias for the current function
     because that is the inline function whose vars, types, etc.
     are being merged into the current function.
     See expand_inline_function.  */
  if (context == current_function_decl || context == inline_function_decl)
    return virtual_stack_vars_rtx;

  for (link = context_display; link; link = TREE_CHAIN (link))
    if (TREE_PURPOSE (link) == context)
      return RTL_EXPR_RTL (TREE_VALUE (link));

  abort ();
}

/* Convert a stack slot address ADDR for variable VAR
   (from a containing function)
   into an address valid in this function (using a static chain).  */

rtx
fix_lexical_addr (addr, var)
     rtx addr;
     tree var;
{
  rtx basereg;
  int displacement;
  tree context = decl_function_context (var);
  struct function *fp;
  rtx base = 0;

  /* If this is the present function, we need not do anything.  */
  if (context == current_function_decl || context == inline_function_decl)
    return addr;

  for (fp = outer_function_chain; fp; fp = fp->next)
    if (fp->decl == context)
      break;

  if (fp == 0)
    abort ();

  /* Decode given address as base reg plus displacement.  */
  if (GET_CODE (addr) == REG)
    basereg = addr, displacement = 0;
  else if (GET_CODE (addr) == PLUS && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    basereg = XEXP (addr, 0), displacement = INTVAL (XEXP (addr, 1));
  else
    abort ();

  /* We accept vars reached via the containing function's
     incoming arg pointer and via its stack variables pointer.  */
  if (basereg == fp->internal_arg_pointer)
    {
      /* If reached via arg pointer, get the arg pointer value
	 out of that function's stack frame.

	 There are two cases:  If a separate ap is needed, allocate a
	 slot in the outer function for it and dereference it that way.
	 This is correct even if the real ap is actually a pseudo.
	 Otherwise, just adjust the offset from the frame pointer to
	 compensate.  */

#ifdef NEED_SEPARATE_AP
      rtx addr;

      if (fp->arg_pointer_save_area == 0)
	fp->arg_pointer_save_area
	  = assign_outer_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0, fp);

      addr = fix_lexical_addr (XEXP (fp->arg_pointer_save_area, 0), var);
      addr = memory_address (Pmode, addr);

      base = copy_to_reg (gen_rtx (MEM, Pmode, addr));
#else
      displacement += (FIRST_PARM_OFFSET (context) - STARTING_FRAME_OFFSET);
      base = lookup_static_chain (var);
#endif
    }

  else if (basereg == virtual_stack_vars_rtx)
    {
      /* This is the same code as lookup_static_chain, duplicated here to
	 avoid an extra call to decl_function_context.  */
      tree link;

      for (link = context_display; link; link = TREE_CHAIN (link))
	if (TREE_PURPOSE (link) == context)
	  {
	    base = RTL_EXPR_RTL (TREE_VALUE (link));
	    break;
	  }
    }

  if (base == 0)
    abort ();

  /* Use same offset, relative to appropriate static chain or argument
     pointer.  */
  return plus_constant (base, displacement);
}

/* Return the address of the trampoline for entering nested fn FUNCTION.
   If necessary, allocate a trampoline (in the stack frame)
   and emit rtl to initialize its contents (at entry to this function).  */

rtx
trampoline_address (function)
     tree function;
{
  tree link;
  tree rtlexp;
  rtx tramp;
  struct function *fp;
  tree fn_context;

  /* Find an existing trampoline and return it.  */
  for (link = trampoline_list; link; link = TREE_CHAIN (link))
    if (TREE_PURPOSE (link) == function)
      return XEXP (RTL_EXPR_RTL (TREE_VALUE (link)), 0);
  for (fp = outer_function_chain; fp; fp = fp->next)
    for (link = fp->trampoline_list; link; link = TREE_CHAIN (link))
      if (TREE_PURPOSE (link) == function)
	{
	  tramp = fix_lexical_addr (XEXP (RTL_EXPR_RTL (TREE_VALUE (link)), 0),
				    function);
	  return round_trampoline_addr (tramp);
	}

  /* None exists; we must make one.  */

  /* Find the `struct function' for the function containing FUNCTION.  */
  fp = 0;
  fn_context = decl_function_context (function);
  if (fn_context != current_function_decl)
    for (fp = outer_function_chain; fp; fp = fp->next)
      if (fp->decl == fn_context)
	break;

  /* Allocate run-time space for this trampoline
     (usually in the defining function's stack frame).  */
#ifdef ALLOCATE_TRAMPOLINE
  tramp = ALLOCATE_TRAMPOLINE (fp);
#else
  /* If rounding needed, allocate extra space
     to ensure we have TRAMPOLINE_SIZE bytes left after rounding up.  */
#ifdef TRAMPOLINE_ALIGNMENT
#define TRAMPOLINE_REAL_SIZE (TRAMPOLINE_SIZE + TRAMPOLINE_ALIGNMENT - 1)
#else
#define TRAMPOLINE_REAL_SIZE (TRAMPOLINE_SIZE)
#endif
  if (fp != 0)
    tramp = assign_outer_stack_local (BLKmode, TRAMPOLINE_REAL_SIZE, 0, fp);
  else
    tramp = assign_stack_local (BLKmode, TRAMPOLINE_REAL_SIZE, 0);
#endif

  /* Record the trampoline for reuse and note it for later initialization
     by expand_function_end.  */
  if (fp != 0)
    {
      push_obstacks (fp->current_obstack, fp->function_maybepermanent_obstack);
      rtlexp = make_node (RTL_EXPR);
      RTL_EXPR_RTL (rtlexp) = tramp;
      fp->trampoline_list = tree_cons (function, rtlexp, fp->trampoline_list);
      pop_obstacks ();
    }
  else
    {
      /* Make the RTL_EXPR node temporary, not momentary, so that the
	 trampoline_list doesn't become garbage.  */
      int momentary = suspend_momentary ();
      rtlexp = make_node (RTL_EXPR);
      resume_momentary (momentary);

      RTL_EXPR_RTL (rtlexp) = tramp;
      trampoline_list = tree_cons (function, rtlexp, trampoline_list);
    }

  tramp = fix_lexical_addr (XEXP (tramp, 0), function);
  return round_trampoline_addr (tramp);
}

/* Given a trampoline address,
   round it to multiple of TRAMPOLINE_ALIGNMENT.  */

static rtx
round_trampoline_addr (tramp)
     rtx tramp;
{
#ifdef TRAMPOLINE_ALIGNMENT
  /* Round address up to desired boundary.  */
  rtx temp = gen_reg_rtx (Pmode);
  temp = expand_binop (Pmode, add_optab, tramp,
		       GEN_INT (TRAMPOLINE_ALIGNMENT - 1),
		       temp, 0, OPTAB_LIB_WIDEN);
  tramp = expand_binop (Pmode, and_optab, temp,
			GEN_INT (- TRAMPOLINE_ALIGNMENT),
			temp, 0, OPTAB_LIB_WIDEN);
#endif
  return tramp;
}

/* The functions identify_blocks and reorder_blocks provide a way to
   reorder the tree of BLOCK nodes, for optimizers that reshuffle or
   duplicate portions of the RTL code.  Call identify_blocks before
   changing the RTL, and call reorder_blocks after.  */

static int all_blocks ();
static tree blocks_nreverse ();

/* Put all this function's BLOCK nodes into a vector, and return it.
   Also store in each NOTE for the beginning or end of a block
   the index of that block in the vector.
   The arguments are TOP_BLOCK, the top-level block of the function,
   and INSNS, the insn chain of the function.  */

tree *
identify_blocks (top_block, insns)
     tree top_block;
     rtx insns;
{
  int n_blocks;
  tree *block_vector;
  int *block_stack;
  int depth = 0;
  int next_block_number = 0;
  int current_block_number = 0;
  rtx insn;

  if (top_block == 0)
    return 0;

  n_blocks = all_blocks (top_block, 0);
  block_vector = (tree *) xmalloc (n_blocks * sizeof (tree));
  block_stack = (int *) alloca (n_blocks * sizeof (int));

  all_blocks (top_block, block_vector);

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG)
	  {
	    block_stack[depth++] = current_block_number;
	    current_block_number = next_block_number;
	    NOTE_BLOCK_NUMBER (insn) =  next_block_number++;
	  }
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	  {
	    current_block_number = block_stack[--depth];
	    NOTE_BLOCK_NUMBER (insn) = current_block_number;
	  }
      }

  return block_vector;
}

/* Given BLOCK_VECTOR which was returned by identify_blocks,
   and a revised instruction chain, rebuild the tree structure
   of BLOCK nodes to correspond to the new order of RTL.
   The new block tree is inserted below TOP_BLOCK.
   Returns the current top-level block.  */

tree
reorder_blocks (block_vector, top_block, insns)
     tree *block_vector;
     tree top_block;
     rtx insns;
{
  tree current_block = top_block;
  rtx insn;

  if (block_vector == 0)
    return top_block;

  /* Prune the old tree away, so that it doesn't get in the way.  */
  BLOCK_SUBBLOCKS (current_block) = 0;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG)
	  {
	    tree block = block_vector[NOTE_BLOCK_NUMBER (insn)];
	    /* If we have seen this block before, copy it.  */
	    if (TREE_ASM_WRITTEN (block))
	      block = copy_node (block);
	    BLOCK_SUBBLOCKS (block) = 0;
	    TREE_ASM_WRITTEN (block) = 1;
	    BLOCK_SUPERCONTEXT (block) = current_block; 
	    BLOCK_CHAIN (block) = BLOCK_SUBBLOCKS (current_block);
	    BLOCK_SUBBLOCKS (current_block) = block;
	    current_block = block;
	    NOTE_SOURCE_FILE (insn) = 0;
	  }
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	  {
	    BLOCK_SUBBLOCKS (current_block)
	      = blocks_nreverse (BLOCK_SUBBLOCKS (current_block));
	    current_block = BLOCK_SUPERCONTEXT (current_block);
	    NOTE_SOURCE_FILE (insn) = 0;
	  }
      }

  return current_block;
}

/* Reverse the order of elements in the chain T of blocks,
   and return the new head of the chain (old last element).  */

static tree
blocks_nreverse (t)
     tree t;
{
  register tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      next = BLOCK_CHAIN (decl);
      BLOCK_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Count the subblocks of BLOCK, and list them all into the vector VECTOR.
   Also clear TREE_ASM_WRITTEN in all blocks.  */

static int
all_blocks (block, vector)
     tree block;
     tree *vector;
{
  int n_blocks = 1;
  tree subblocks; 

  TREE_ASM_WRITTEN (block) = 0;
  /* Record this block.  */
  if (vector)
    vector[0] = block;

  /* Record the subblocks, and their subblocks.  */
  for (subblocks = BLOCK_SUBBLOCKS (block);
       subblocks; subblocks = BLOCK_CHAIN (subblocks))
    n_blocks += all_blocks (subblocks, vector ? vector + n_blocks : 0);

  return n_blocks;
}

/* Generate RTL for the start of the function SUBR (a FUNCTION_DECL tree node)
   and initialize static variables for generating RTL for the statements
   of the function.  */

void
init_function_start (subr, filename, line)
     tree subr;
     char *filename;
     int line;
{
  char *junk;

  init_stmt_for_function ();

  cse_not_expected = ! optimize;

  /* Caller save not needed yet.  */
  caller_save_needed = 0;

  /* No stack slots have been made yet.  */
  stack_slot_list = 0;

  /* There is no stack slot for handling nonlocal gotos.  */
  nonlocal_goto_handler_slot = 0;
  nonlocal_goto_stack_level = 0;

  /* No labels have been declared for nonlocal use.  */
  nonlocal_labels = 0;

  /* No function calls so far in this function.  */
  function_call_count = 0;

  /* No parm regs have been allocated.
     (This is important for output_inline_function.)  */
  max_parm_reg = LAST_VIRTUAL_REGISTER + 1;

  /* Initialize the RTL mechanism.  */
  init_emit ();

  /* Initialize the queue of pending postincrement and postdecrements,
     and some other info in expr.c.  */
  init_expr ();

  /* We haven't done register allocation yet.  */
  reg_renumber = 0;

  init_const_rtx_hash_table ();

  current_function_name = (*decl_printable_name) (subr, &junk);

  /* Nonzero if this is a nested function that uses a static chain.  */

  current_function_needs_context
    = (decl_function_context (current_function_decl) != 0);

  /* Set if a call to setjmp is seen.  */
  current_function_calls_setjmp = 0;

  /* Set if a call to longjmp is seen.  */
  current_function_calls_longjmp = 0;

  current_function_calls_alloca = 0;
  current_function_has_nonlocal_label = 0;
  current_function_contains_functions = 0;

  current_function_returns_pcc_struct = 0;
  current_function_returns_struct = 0;
  current_function_epilogue_delay_list = 0;
  current_function_uses_const_pool = 0;
  current_function_uses_pic_offset_table = 0;

  /* We have not yet needed to make a label to jump to for tail-recursion.  */
  tail_recursion_label = 0;

  /* We haven't had a need to make a save area for ap yet.  */

  arg_pointer_save_area = 0;

  /* No stack slots allocated yet.  */
  frame_offset = 0;

  /* No SAVE_EXPRs in this function yet.  */
  save_expr_regs = 0;

  /* No RTL_EXPRs in this function yet.  */
  rtl_expr_chain = 0;

  /* We have not allocated any temporaries yet.  */
  temp_slots = 0;
  temp_slot_level = 0;

  /* Within function body, compute a type's size as soon it is laid out.  */
  immediate_size_expand++;

  init_pending_stack_adjust ();
  inhibit_defer_pop = 0;

  current_function_outgoing_args_size = 0;

  /* Initialize the insn lengths.  */
  init_insn_lengths ();

  /* Prevent ever trying to delete the first instruction of a function.
     Also tell final how to output a linenum before the function prologue.  */
  emit_line_note (filename, line);

  /* Make sure first insn is a note even if we don't want linenums.
     This makes sure the first insn will never be deleted.
     Also, final expects a note to appear there.  */
  emit_note (NULL_PTR, NOTE_INSN_DELETED);

  /* Set flags used by final.c.  */
  if (aggregate_value_p (DECL_RESULT (subr)))
    {
#ifdef PCC_STATIC_STRUCT_RETURN
      if (flag_pcc_struct_return)
	current_function_returns_pcc_struct = 1;
      else
#endif
	current_function_returns_struct = 1;
    }

  /* Warn if this value is an aggregate type,
     regardless of which calling convention we are using for it.  */
  if (warn_aggregate_return
      && (TREE_CODE (TREE_TYPE (DECL_RESULT (subr))) == RECORD_TYPE
	  || TREE_CODE (TREE_TYPE (DECL_RESULT (subr))) == UNION_TYPE
	  || TREE_CODE (TREE_TYPE (DECL_RESULT (subr))) == ARRAY_TYPE))
    warning ("function returns an aggregate");

  current_function_returns_pointer
    = (TREE_CODE (TREE_TYPE (DECL_RESULT (subr))) == POINTER_TYPE);

  /* Indicate that we need to distinguish between the return value of the
     present function and the return value of a function being called.  */
  rtx_equal_function_value_matters = 1;

  /* Indicate that we have not instantiated virtual registers yet.  */
  virtuals_instantiated = 0;

  /* Indicate we have no need of a frame pointer yet.  */
  frame_pointer_needed = 0;

  /* By default assume not varargs.  */
  current_function_varargs = 0;
}

/* Indicate that the current function uses extra args
   not explicitly mentioned in the argument list in any fashion.  */

void
mark_varargs ()
{
  current_function_varargs = 1;
}

/* Expand a call to __main at the beginning of a possible main function.  */

void
expand_main_function ()
{
#if !defined (INIT_SECTION_ASM_OP) || defined (INVOKE__main)
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__main"), 0,
		     VOIDmode, 0);
#endif /* not INIT_SECTION_ASM_OP or INVOKE__main */
}

/* Start the RTL for a new function, and set variables used for
   emitting RTL.
   SUBR is the FUNCTION_DECL node.
   PARMS_HAVE_CLEANUPS is nonzero if there are cleanups associated with
   the function's parameters, which must be run at any return statement.  */

void
expand_function_start (subr, parms_have_cleanups)
     tree subr;
     int parms_have_cleanups;
{
  register int i;
  tree tem;
  rtx last_ptr;

  /* Make sure volatile mem refs aren't considered
     valid operands of arithmetic insns.  */
  init_recog_no_volatile ();

  /* If function gets a static chain arg, store it in the stack frame.
     Do this first, so it gets the first stack slot offset.  */
  if (current_function_needs_context)
    {
      last_ptr = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);
      emit_move_insn (last_ptr, static_chain_incoming_rtx);
    }

  /* If the parameters of this function need cleaning up, get a label
     for the beginning of the code which executes those cleanups.  This must
     be done before doing anything with return_label.  */
  if (parms_have_cleanups)
    cleanup_label = gen_label_rtx ();
  else
    cleanup_label = 0;

  /* Make the label for return statements to jump to, if this machine
     does not have a one-instruction return and uses an epilogue,
     or if it returns a structure, or if it has parm cleanups.  */
#ifdef HAVE_return
  if (cleanup_label == 0 && HAVE_return
      && ! current_function_returns_pcc_struct
      && ! (current_function_returns_struct && ! optimize))
    return_label = 0;
  else
    return_label = gen_label_rtx ();
#else
  return_label = gen_label_rtx ();
#endif

  /* Initialize rtx used to return the value.  */
  /* Do this before assign_parms so that we copy the struct value address
     before any library calls that assign parms might generate.  */

  /* Decide whether to return the value in memory or in a register.  */
  if (aggregate_value_p (DECL_RESULT (subr)))
    {
      /* Returning something that won't go in a register.  */
      register rtx value_address;

#ifdef PCC_STATIC_STRUCT_RETURN
      if (current_function_returns_pcc_struct)
	{
	  int size = int_size_in_bytes (TREE_TYPE (DECL_RESULT (subr)));
	  value_address = assemble_static_space (size);
	}
      else
#endif
	{
	  /* Expect to be passed the address of a place to store the value.
	     If it is passed as an argument, assign_parms will take care of
	     it.  */
	  if (struct_value_incoming_rtx)
	    {
	      value_address = gen_reg_rtx (Pmode);
	      emit_move_insn (value_address, struct_value_incoming_rtx);
	    }
	}
      if (value_address)
	DECL_RTL (DECL_RESULT (subr))
	  = gen_rtx (MEM, DECL_MODE (DECL_RESULT (subr)),
		     value_address);
    }
  else if (DECL_MODE (DECL_RESULT (subr)) == VOIDmode)
    /* If return mode is void, this decl rtl should not be used.  */
    DECL_RTL (DECL_RESULT (subr)) = 0;
  else if (parms_have_cleanups)
    {
      /* If function will end with cleanup code for parms,
	 compute the return values into a pseudo reg,
	 which we will copy into the true return register
	 after the cleanups are done.  */

      enum machine_mode mode = DECL_MODE (DECL_RESULT (subr));
#ifdef PROMOTE_FUNCTION_RETURN
      tree type = TREE_TYPE (DECL_RESULT (subr));
      int unsignedp = TREE_UNSIGNED (type);

      if (TREE_CODE (type) == INTEGER_TYPE || TREE_CODE (type) == ENUMERAL_TYPE
	  || TREE_CODE (type) == BOOLEAN_TYPE || TREE_CODE (type) == CHAR_TYPE
	  || TREE_CODE (type) == REAL_TYPE || TREE_CODE (type) == POINTER_TYPE
	  || TREE_CODE (type) == OFFSET_TYPE)
	{
	  PROMOTE_MODE (mode, unsignedp, type);
	}
#endif

      DECL_RTL (DECL_RESULT (subr)) = gen_reg_rtx (mode);
    }
  else
    /* Scalar, returned in a register.  */
    {
#ifdef FUNCTION_OUTGOING_VALUE
      DECL_RTL (DECL_RESULT (subr))
	= FUNCTION_OUTGOING_VALUE (TREE_TYPE (DECL_RESULT (subr)), subr);
#else
      DECL_RTL (DECL_RESULT (subr))
	= FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (subr)), subr);
#endif

      /* Mark this reg as the function's return value.  */
      if (GET_CODE (DECL_RTL (DECL_RESULT (subr))) == REG)
	{
	  REG_FUNCTION_VALUE_P (DECL_RTL (DECL_RESULT (subr))) = 1;
	  /* Needed because we may need to move this to memory
	     in case it's a named return value whose address is taken.  */
	  DECL_REGISTER (DECL_RESULT (subr)) = 1;
	}
    }

  /* Initialize rtx for parameters and local variables.
     In some cases this requires emitting insns.  */

  assign_parms (subr, 0);

  /* The following was moved from init_function_start.
     The move is supposed to make sdb output more accurate.  */
  /* Indicate the beginning of the function body,
     as opposed to parm setup.  */
  emit_note (NULL_PTR, NOTE_INSN_FUNCTION_BEG);

  /* If doing stupid allocation, mark parms as born here.  */

  if (GET_CODE (get_last_insn ()) != NOTE)
    emit_note (NULL_PTR, NOTE_INSN_DELETED);
  parm_birth_insn = get_last_insn ();

  if (obey_regdecls)
    {
      for (i = LAST_VIRTUAL_REGISTER + 1; i < max_parm_reg; i++)
	use_variable (regno_reg_rtx[i]);

      if (current_function_internal_arg_pointer != virtual_incoming_args_rtx)
	use_variable (current_function_internal_arg_pointer);
    }

  /* Fetch static chain values for containing functions.  */
  tem = decl_function_context (current_function_decl);
  /* If not doing stupid register allocation, then start off with the static
     chain pointer in a pseudo register.  Otherwise, we use the stack
     address that was generated above.  */
  if (tem && ! obey_regdecls)
    last_ptr = copy_to_reg (static_chain_incoming_rtx);
  context_display = 0;
  while (tem)
    {
      tree rtlexp = make_node (RTL_EXPR);

      RTL_EXPR_RTL (rtlexp) = last_ptr;
      context_display = tree_cons (tem, rtlexp, context_display);
      tem = decl_function_context (tem);
      if (tem == 0)
	break;
      /* Chain thru stack frames, assuming pointer to next lexical frame
	 is found at the place we always store it.  */
#ifdef FRAME_GROWS_DOWNWARD
      last_ptr = plus_constant (last_ptr, - GET_MODE_SIZE (Pmode));
#endif
      last_ptr = copy_to_reg (gen_rtx (MEM, Pmode,
				       memory_address (Pmode, last_ptr)));
    }

  /* After the display initializations is where the tail-recursion label
     should go, if we end up needing one.   Ensure we have a NOTE here
     since some things (like trampolines) get placed before this.  */
  tail_recursion_reentry = emit_note (NULL_PTR, NOTE_INSN_DELETED);

  /* Evaluate now the sizes of any types declared among the arguments.  */
  for (tem = nreverse (get_pending_sizes ()); tem; tem = TREE_CHAIN (tem))
    expand_expr (TREE_VALUE (tem), NULL_RTX, VOIDmode, 0);

  /* Make sure there is a line number after the function entry setup code.  */
  force_next_line_note ();
}

/* Generate RTL for the end of the current function.
   FILENAME and LINE are the current position in the source file.  */

/* It is up to language-specific callers to do cleanups for parameters.  */

void
expand_function_end (filename, line)
     char *filename;
     int line;
{
  register int i;
  tree link;

  static rtx initial_trampoline;

#ifdef NON_SAVING_SETJMP
  /* Don't put any variables in registers if we call setjmp
     on a machine that fails to restore the registers.  */
  if (NON_SAVING_SETJMP && current_function_calls_setjmp)
    {
      setjmp_protect (DECL_INITIAL (current_function_decl));
      setjmp_protect_args ();
    }
#endif

  /* Save the argument pointer if a save area was made for it.  */
  if (arg_pointer_save_area)
    {
      rtx x = gen_move_insn (arg_pointer_save_area, virtual_incoming_args_rtx);
      emit_insn_before (x, tail_recursion_reentry);
    }

  /* Initialize any trampolines required by this function.  */
  for (link = trampoline_list; link; link = TREE_CHAIN (link))
    {
      tree function = TREE_PURPOSE (link);
      rtx context = lookup_static_chain (function);
      rtx tramp = RTL_EXPR_RTL (TREE_VALUE (link));
      rtx seq;

      /* First make sure this compilation has a template for
	 initializing trampolines.  */
      if (initial_trampoline == 0)
	{
	  end_temporary_allocation ();
	  initial_trampoline
	    = gen_rtx (MEM, BLKmode, assemble_trampoline_template ());
	  resume_temporary_allocation ();
	}

      /* Generate insns to initialize the trampoline.  */
      start_sequence ();
      tramp = change_address (initial_trampoline, BLKmode,
			      round_trampoline_addr (XEXP (tramp, 0)));
      emit_block_move (tramp, initial_trampoline, GEN_INT (TRAMPOLINE_SIZE),
		       FUNCTION_BOUNDARY / BITS_PER_UNIT);
      INITIALIZE_TRAMPOLINE (XEXP (tramp, 0),
			     XEXP (DECL_RTL (function), 0), context);
      seq = get_insns ();
      end_sequence ();

      /* Put those insns at entry to the containing function (this one).  */
      emit_insns_before (seq, tail_recursion_reentry);
    }
  /* Clear the trampoline_list for the next function.  */
  trampoline_list = 0;

#if 0  /* I think unused parms are legitimate enough.  */
  /* Warn about unused parms.  */
  if (warn_unused)
    {
      rtx decl;

      for (decl = DECL_ARGUMENTS (current_function_decl);
	   decl; decl = TREE_CHAIN (decl))
	if (! TREE_USED (decl) && TREE_CODE (decl) == VAR_DECL)
	  warning_with_decl (decl, "unused parameter `%s'");
    }
#endif

  /* Delete handlers for nonlocal gotos if nothing uses them.  */
  if (nonlocal_goto_handler_slot != 0 && !current_function_has_nonlocal_label)
    delete_handlers ();

  /* End any sequences that failed to be closed due to syntax errors.  */
  while (in_sequence_p ())
    end_sequence ();

  /* Outside function body, can't compute type's actual size
     until next function's body starts.  */
  immediate_size_expand--;

  /* If doing stupid register allocation,
     mark register parms as dying here.  */

  if (obey_regdecls)
    {
      rtx tem;
      for (i = LAST_VIRTUAL_REGISTER + 1; i < max_parm_reg; i++)
	use_variable (regno_reg_rtx[i]);

      /* Likewise for the regs of all the SAVE_EXPRs in the function.  */

      for (tem = save_expr_regs; tem; tem = XEXP (tem, 1))
	{
	  use_variable (XEXP (tem, 0));
	  use_variable_after (XEXP (tem, 0), parm_birth_insn);
	}

      if (current_function_internal_arg_pointer != virtual_incoming_args_rtx)
	use_variable (current_function_internal_arg_pointer);
    }

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();

  /* Mark the end of the function body.
     If control reaches this insn, the function can drop through
     without returning a value.  */
  emit_note (NULL_PTR, NOTE_INSN_FUNCTION_END);

  /* Output a linenumber for the end of the function.
     SDB depends on this.  */
  emit_line_note_force (filename, line);

  /* Output the label for the actual return from the function,
     if one is expected.  This happens either because a function epilogue
     is used instead of a return instruction, or because a return was done
     with a goto in order to run local cleanups, or because of pcc-style
     structure returning.  */

  if (return_label)
    emit_label (return_label);

  /* If we had calls to alloca, and this machine needs
     an accurate stack pointer to exit the function,
     insert some code to save and restore the stack pointer.  */
#ifdef EXIT_IGNORE_STACK
  if (! EXIT_IGNORE_STACK)
#endif
    if (current_function_calls_alloca)
      {
	rtx tem = 0;

	emit_stack_save (SAVE_FUNCTION, &tem, parm_birth_insn);
	emit_stack_restore (SAVE_FUNCTION, tem, NULL_RTX);
      }

  /* If scalar return value was computed in a pseudo-reg,
     copy that to the hard return register.  */
  if (DECL_RTL (DECL_RESULT (current_function_decl)) != 0
      && GET_CODE (DECL_RTL (DECL_RESULT (current_function_decl))) == REG
      && (REGNO (DECL_RTL (DECL_RESULT (current_function_decl)))
	  >= FIRST_PSEUDO_REGISTER))
    {
      rtx real_decl_result;

#ifdef FUNCTION_OUTGOING_VALUE
      real_decl_result
	= FUNCTION_OUTGOING_VALUE (TREE_TYPE (DECL_RESULT (current_function_decl)),
				   current_function_decl);
#else
      real_decl_result
	= FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (current_function_decl)),
			  current_function_decl);
#endif
      REG_FUNCTION_VALUE_P (real_decl_result) = 1;
      emit_move_insn (real_decl_result,
		      DECL_RTL (DECL_RESULT (current_function_decl)));
      emit_insn (gen_rtx (USE, VOIDmode, real_decl_result));
    }

  /* If returning a structure, arrange to return the address of the value
     in a place where debuggers expect to find it.

     If returning a structure PCC style,
     the caller also depends on this value.
     And current_function_returns_pcc_struct is not necessarily set.  */
  if (current_function_returns_struct
      || current_function_returns_pcc_struct)
    {
      rtx value_address = XEXP (DECL_RTL (DECL_RESULT (current_function_decl)), 0);
      tree type = TREE_TYPE (DECL_RESULT (current_function_decl));
#ifdef FUNCTION_OUTGOING_VALUE
      rtx outgoing
	= FUNCTION_OUTGOING_VALUE (build_pointer_type (type),
				   current_function_decl);
#else
      rtx outgoing
	= FUNCTION_VALUE (build_pointer_type (type),
			  current_function_decl);
#endif

      /* Mark this as a function return value so integrate will delete the
	 assignment and USE below when inlining this function.  */
      REG_FUNCTION_VALUE_P (outgoing) = 1;

      emit_move_insn (outgoing, value_address);
      use_variable (outgoing);
    }

  /* Output a return insn if we are using one.
     Otherwise, let the rtl chain end here, to drop through
     into the epilogue.  */

#ifdef HAVE_return
  if (HAVE_return)
    {
      emit_jump_insn (gen_return ());
      emit_barrier ();
    }
#endif

  /* Fix up any gotos that jumped out to the outermost
     binding level of the function.
     Must follow emitting RETURN_LABEL.  */

  /* If you have any cleanups to do at this point,
     and they need to create temporary variables,
     then you will lose.  */
  fixup_gotos (NULL_PTR, NULL_RTX, NULL_TREE, get_insns (), 0);
}

/* These arrays record the INSN_UIDs of the prologue and epilogue insns.  */

static int *prologue;
static int *epilogue;

/* Create an array that records the INSN_UIDs of INSNS (either a sequence
   or a single insn).  */

static int *
record_insns (insns)
     rtx insns;
{
  int *vec;

  if (GET_CODE (insns) == SEQUENCE)
    {
      int len = XVECLEN (insns, 0);
      vec = (int *) oballoc ((len + 1) * sizeof (int));
      vec[len] = 0;
      while (--len >= 0)
	vec[len] = INSN_UID (XVECEXP (insns, 0, len));
    }
  else
    {
      vec = (int *) oballoc (2 * sizeof (int));
      vec[0] = INSN_UID (insns);
      vec[1] = 0;
    }
  return vec;
}

/* Determine how many INSN_UIDs in VEC are part of INSN.  */

static int
contains (insn, vec)
     rtx insn;
     int *vec;
{
  register int i, j;

  if (GET_CODE (insn) == INSN
      && GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      int count = 0;
      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	for (j = 0; vec[j]; j++)
	  if (INSN_UID (XVECEXP (PATTERN (insn), 0, i)) == vec[j])
	    count++;
      return count;
    }
  else
    {
      for (j = 0; vec[j]; j++)
	if (INSN_UID (insn) == vec[j])
	  return 1;
    }
  return 0;
}

/* Generate the prologe and epilogue RTL if the machine supports it.  Thread
   this into place with notes indicating where the prologue ends and where
   the epilogue begins.  Update the basic block information when possible.  */

void
thread_prologue_and_epilogue_insns (f)
     rtx f;
{
#ifdef HAVE_prologue
  if (HAVE_prologue)
    {
      rtx head, seq, insn;

      /* The first insn (a NOTE_INSN_DELETED) is followed by zero or more
	 prologue insns and a NOTE_INSN_PROLOGUE_END.  */
      emit_note_after (NOTE_INSN_PROLOGUE_END, f);
      seq = gen_prologue ();
      head = emit_insn_after (seq, f);

      /* Include the new prologue insns in the first block.  Ignore them
	 if they form a basic block unto themselves.  */
      if (basic_block_head && n_basic_blocks
	  && GET_CODE (basic_block_head[0]) != CODE_LABEL)
	basic_block_head[0] = NEXT_INSN (f);

      /* Retain a map of the prologue insns.  */
      prologue = record_insns (GET_CODE (seq) == SEQUENCE ? seq : head);
    }
  else
#endif
    prologue = 0;

#ifdef HAVE_epilogue
  if (HAVE_epilogue)
    {
      rtx insn = get_last_insn ();
      rtx prev = prev_nonnote_insn (insn);

      /* If we end with a BARRIER, we don't need an epilogue.  */
      if (! (prev && GET_CODE (prev) == BARRIER))
	{
	  rtx tail, seq;

	  /* The last basic block ends with a NOTE_INSN_EPILOGUE_BEG,
	     the epilogue insns (this must include the jump insn that
	     returns), USE insns ad the end of a function, and a BARRIER.  */

	  emit_barrier_after (insn);

	  /* Place the epilogue before the USE insns at the end of a
	     function.  */
	  while (prev
		 && GET_CODE (prev) == INSN
		 && GET_CODE (PATTERN (prev)) == USE)
	    {
	      insn = PREV_INSN (prev);
	      prev = prev_nonnote_insn (prev);
	    }

	  seq = gen_epilogue ();
	  tail = emit_jump_insn_after (seq, insn);
	  emit_note_after (NOTE_INSN_EPILOGUE_BEG, insn);

	  /* Include the new epilogue insns in the last block.  Ignore
	     them if they form a basic block unto themselves.  */
	  if (basic_block_end && n_basic_blocks
	      && GET_CODE (basic_block_end[n_basic_blocks - 1]) != JUMP_INSN)
	    basic_block_end[n_basic_blocks - 1] = tail;

	  /* Retain a map of the epilogue insns.  */
	  epilogue = record_insns (GET_CODE (seq) == SEQUENCE ? seq : tail);
	  return;
	}
    }
#endif
  epilogue = 0;
}

/* Reposition the prologue-end and epilogue-begin notes after instruction
   scheduling and delayed branch scheduling.  */

void
reposition_prologue_and_epilogue_notes (f)
     rtx f;
{
#if defined (HAVE_prologue) || defined (HAVE_epilogue)
  /* Reposition the prologue and epilogue notes.  */
  if (n_basic_blocks)
    {
      rtx next, prev;
      int len;

      if (prologue)
	{
	  register rtx insn, note = 0;

	  /* Scan from the beginning until we reach the last prologue insn.
	     We apparently can't depend on basic_block_{head,end} after
	     reorg has run.  */
	  for (len = 0; prologue[len]; len++)
	    ;
	  for (insn = f; insn; insn = NEXT_INSN (insn))
	    if (GET_CODE (insn) == NOTE)
	      {
		if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_PROLOGUE_END)
		  note = insn;
	      }
	    else if ((len -= contains (insn, prologue)) == 0)
	      {
		/* Find the prologue-end note if we haven't already, and
		   move it to just after the last prologue insn.  */
		if (note == 0)
		  for (note = insn; note = NEXT_INSN (note);)
		    if (GET_CODE (note) == NOTE
			&& NOTE_LINE_NUMBER (note) == NOTE_INSN_PROLOGUE_END)
		      break;
		next = NEXT_INSN (note);
		prev = PREV_INSN (note);
		if (prev)
		  NEXT_INSN (prev) = next;
		if (next)
		  PREV_INSN (next) = prev;
		add_insn_after (note, insn);
		break;
	      }
	}

      if (epilogue)
	{
	  register rtx insn, note = 0;

	  /* Scan from the end until we reach the first epilogue insn.
	     We apparently can't depend on basic_block_{head,end} after
	     reorg has run.  */
	  for (len = 0; epilogue[len]; len++)
	    ;
	  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
	    if (GET_CODE (insn) == NOTE)
	      {
		if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EPILOGUE_BEG)
		  note = insn;
	      }
	    else if ((len -= contains (insn, epilogue)) == 0)
	      {
		/* Find the epilogue-begin note if we haven't already, and
		   move it to just before the first epilogue insn.  */
		if (note == 0)
		  for (note = insn; note = PREV_INSN (note);)
		    if (GET_CODE (note) == NOTE
			&& NOTE_LINE_NUMBER (note) == NOTE_INSN_EPILOGUE_BEG)
		      break;
		next = NEXT_INSN (note);
		prev = PREV_INSN (note);
		if (prev)
		  NEXT_INSN (prev) = next;
		if (next)
		  PREV_INSN (next) = prev;
		add_insn_after (note, PREV_INSN (insn));
		break;
	      }
	}
    }
#endif /* HAVE_prologue or HAVE_epilogue */
}
