/* Expands front end tree to back end RTL for GNU C-Compiler
   Copyright (C) 1987, 88, 89, 91-99, 2000 Free Software Foundation, Inc.

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
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "except.h"
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
#include "obstack.h"
#include "toplev.h"
#include "hash.h"
#include "ggc.h"
#include "tm_p.h"

#ifndef TRAMPOLINE_ALIGNMENT
#define TRAMPOLINE_ALIGNMENT FUNCTION_BOUNDARY
#endif

#ifndef LOCAL_ALIGNMENT
#define LOCAL_ALIGNMENT(TYPE, ALIGNMENT) ALIGNMENT
#endif

#if !defined (PREFERRED_STACK_BOUNDARY) && defined (STACK_BOUNDARY)
#define PREFERRED_STACK_BOUNDARY STACK_BOUNDARY
#endif

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#define SYMBOL__MAIN __main
#endif

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

/* Nonzero if function being compiled doesn't contain any calls
   (ignoring the prologue and epilogue).  This is set prior to
   local register allocation and is valid for the remaining
   compiler passes. */
int current_function_is_leaf;

/* Nonzero if function being compiled doesn't modify the stack pointer
   (ignoring the prologue and epilogue).  This is only valid after
   life_analysis has run. */
int current_function_sp_is_unchanging;

/* Nonzero if the function being compiled is a leaf function which only
   uses leaf registers.  This is valid after reload (specifically after
   sched2) and is useful only if the port defines LEAF_REGISTERS.  */
int current_function_uses_only_leaf_regs;

/* Nonzero once virtual register instantiation has been done.
   assign_stack_local uses frame_pointer_rtx when this is nonzero.  */
static int virtuals_instantiated;

/* These variables hold pointers to functions to
   save and restore machine-specific data,
   in push_function_context and pop_function_context.  */
void (*init_machine_status) PARAMS ((struct function *));
void (*save_machine_status) PARAMS ((struct function *));
void (*restore_machine_status) PARAMS ((struct function *));
void (*mark_machine_status) PARAMS ((struct function *));
void (*free_machine_status) PARAMS ((struct function *));

/* Likewise, but for language-specific data.  */
void (*init_lang_status) PARAMS ((struct function *));
void (*save_lang_status) PARAMS ((struct function *));
void (*restore_lang_status) PARAMS ((struct function *));
void (*mark_lang_status) PARAMS ((struct function *));
void (*free_lang_status) PARAMS ((struct function *));

/* The FUNCTION_DECL for an inline function currently being expanded.  */
tree inline_function_decl;

/* The currently compiled function.  */
struct function *cfun = 0;

/* Global list of all compiled functions.  */
struct function *all_functions = 0;

/* These arrays record the INSN_UIDs of the prologue and epilogue insns.  */
static int *prologue;
static int *epilogue;

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
  /* The rtx to used to reference the slot.  */
  rtx slot;
  /* The rtx used to represent the address if not the address of the
     slot above.  May be an EXPR_LIST if multiple addresses exist.  */
  rtx address;
  /* The alignment (in bits) of the slot. */
  int align;
  /* The size, in units, of the slot.  */
  HOST_WIDE_INT size;
  /* The alias set for the slot.  If the alias set is zero, we don't
     know anything about the alias set of the slot.  We must only
     reuse a slot if it is assigned an object of the same alias set.
     Otherwise, the rest of the compiler may assume that the new use
     of the slot cannot alias the old use of the slot, which is
     false.  If the slot has alias set zero, then we can't reuse the
     slot at all, since we have no idea what alias set may have been
     imposed on the memory.  For example, if the stack slot is the
     call frame for an inline functioned, we have no idea what alias
     sets will be assigned to various pieces of the call frame.  */
  int alias_set;
  /* The value of `sequence_rtl_expr' when this temporary is allocated.  */
  tree rtl_expr;
  /* Non-zero if this temporary is currently in use.  */
  char in_use;
  /* Non-zero if this temporary has its address taken.  */
  char addr_taken;
  /* Nesting level at which this slot is being used.  */
  int level;
  /* Non-zero if this should survive a call to free_temp_slots.  */
  int keep;
  /* The offset of the slot from the frame_pointer, including extra space
     for alignment.  This info is for combine_temp_slots.  */
  HOST_WIDE_INT base_offset;
  /* The size of the slot, including extra space for alignment.  This
     info is for combine_temp_slots.  */
  HOST_WIDE_INT full_size;
};

/* This structure is used to record MEMs or pseudos used to replace VAR, any
   SUBREGs of VAR, and any MEMs containing VAR as an address.  We need to
   maintain this list in case two operands of an insn were required to match;
   in that case we must ensure we use the same replacement.  */

struct fixup_replacement
{
  rtx old;
  rtx new;
  struct fixup_replacement *next;
};
   
struct insns_for_mem_entry {
  /* The KEY in HE will be a MEM.  */
  struct hash_entry he;
  /* These are the INSNS which reference the MEM.  */
  rtx insns;
};

/* Forward declarations.  */

static rtx assign_stack_local_1 PARAMS ((enum machine_mode, HOST_WIDE_INT,
					 int, struct function *));
static rtx assign_stack_temp_for_type PARAMS ((enum machine_mode,
					       HOST_WIDE_INT, int, tree));
static struct temp_slot *find_temp_slot_from_address  PARAMS ((rtx));
static void put_reg_into_stack	PARAMS ((struct function *, rtx, tree,
					 enum machine_mode, enum machine_mode,
					 int, int, int, struct hash_table *));
static void fixup_var_refs	PARAMS ((rtx, enum machine_mode, int, 
					 struct hash_table *));
static struct fixup_replacement
  *find_fixup_replacement	PARAMS ((struct fixup_replacement **, rtx));
static void fixup_var_refs_insns PARAMS ((rtx, enum machine_mode, int,
					  rtx, int, struct hash_table *));
static void fixup_var_refs_1	PARAMS ((rtx, enum machine_mode, rtx *, rtx,
					 struct fixup_replacement **));
static rtx fixup_memory_subreg	PARAMS ((rtx, rtx, int));
static rtx walk_fixup_memory_subreg  PARAMS ((rtx, rtx, int));
static rtx fixup_stack_1	PARAMS ((rtx, rtx));
static void optimize_bit_field	PARAMS ((rtx, rtx, rtx *));
static void instantiate_decls	PARAMS ((tree, int));
static void instantiate_decls_1	PARAMS ((tree, int));
static void instantiate_decl	PARAMS ((rtx, int, int));
static int instantiate_virtual_regs_1 PARAMS ((rtx *, rtx, int));
static void delete_handlers	PARAMS ((void));
static void pad_to_arg_alignment PARAMS ((struct args_size *, int,
					  struct args_size *));
#ifndef ARGS_GROW_DOWNWARD
static void pad_below		PARAMS ((struct args_size *, enum machine_mode,
					 tree));
#endif
#ifdef ARGS_GROW_DOWNWARD
static tree round_down		PARAMS ((tree, int));
#endif
static rtx round_trampoline_addr PARAMS ((rtx));
static tree blocks_nreverse	PARAMS ((tree));
static int all_blocks		PARAMS ((tree, tree *));
/* We always define `record_insns' even if its not used so that we
   can always export `prologue_epilogue_contains'.  */
static int *record_insns	PARAMS ((rtx)) ATTRIBUTE_UNUSED;
static int contains		PARAMS ((rtx, int *));
static void put_addressof_into_stack PARAMS ((rtx, struct hash_table *));
static boolean purge_addressof_1 PARAMS ((rtx *, rtx, int, int, 
					  struct hash_table *));
static int is_addressof		PARAMS ((rtx *, void *));
static struct hash_entry *insns_for_mem_newfunc PARAMS ((struct hash_entry *,
							 struct hash_table *,
							 hash_table_key));
static unsigned long insns_for_mem_hash PARAMS ((hash_table_key));
static boolean insns_for_mem_comp PARAMS ((hash_table_key, hash_table_key));
static int insns_for_mem_walk   PARAMS ((rtx *, void *));
static void compute_insns_for_mem PARAMS ((rtx, rtx, struct hash_table *));
static void mark_temp_slot PARAMS ((struct temp_slot *));
static void mark_function_status PARAMS ((struct function *));
static void mark_function_chain PARAMS ((void *));
static void prepare_function_start PARAMS ((void));


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
   This is called from language-specific code.  The caller should use
   the save_lang_status callback to save any language-specific state,
   since this function knows only about language-independent
   variables.  */

void
push_function_context_to (context)
     tree context;
{
  struct function *p, *context_data;

  if (context)
    {
      context_data = (context == current_function_decl
		      ? cfun
		      : find_function_data (context));
      context_data->contains_functions = 1;
    }

  if (cfun == 0)
    init_dummy_function_start ();
  p = cfun;

  p->next = outer_function_chain;
  outer_function_chain = p;
  p->fixup_var_refs_queue = 0;

  save_tree_status (p);
  if (save_lang_status)
    (*save_lang_status) (p);
  if (save_machine_status)
    (*save_machine_status) (p);

  cfun = 0;
}

void
push_function_context ()
{
  push_function_context_to (current_function_decl);
}

/* Restore the last saved context, at the end of a nested function.
   This function is called from language-specific code.  */

void
pop_function_context_from (context)
     tree context ATTRIBUTE_UNUSED;
{
  struct function *p = outer_function_chain;
  struct var_refs_queue *queue;
  struct var_refs_queue *next;

  cfun = p;
  outer_function_chain = p->next;

  current_function_decl = p->decl;
  reg_renumber = 0;

  restore_tree_status (p);
  restore_emit_status (p);

  if (restore_machine_status)
    (*restore_machine_status) (p);
  if (restore_lang_status)
    (*restore_lang_status) (p);

  /* Finish doing put_var_into_stack for any of our variables
     which became addressable during the nested function.  */
  for (queue = p->fixup_var_refs_queue; queue; queue = next)
    {
      next = queue->next;
      fixup_var_refs (queue->modified, queue->promoted_mode,
		      queue->unsignedp, 0);
      free (queue);
    }
  p->fixup_var_refs_queue = 0;

  /* Reset variables that have known state during rtx generation.  */
  rtx_equal_function_value_matters = 1;
  virtuals_instantiated = 0;
}

void
pop_function_context ()
{
  pop_function_context_from (current_function_decl);
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been parsed, but not compiled, to let
   garbage collection reclaim the memory.  */

void
free_after_parsing (f)
     struct function *f;
{
  /* f->expr->forced_labels is used by code generation.  */
  /* f->emit->regno_reg_rtx is used by code generation.  */
  /* f->varasm is used by code generation.  */
  /* f->eh->eh_return_stub_label is used by code generation.  */

  if (free_lang_status)
    (*free_lang_status) (f);
  free_stmt_status (f);
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been compiled, to let garbage collection
   reclaim the memory.  */

void
free_after_compilation (f)
     struct function *f;
{
  free_eh_status (f);
  free_expr_status (f);
  free_emit_status (f);
  free_varasm_status (f);

  if (free_machine_status)
    (*free_machine_status) (f);

  if (f->x_parm_reg_stack_loc)
    free (f->x_parm_reg_stack_loc);

  f->arg_offset_rtx = NULL;
  f->return_rtx = NULL;
  f->internal_arg_pointer = NULL;
  f->x_nonlocal_labels = NULL;
  f->x_nonlocal_goto_handler_slots = NULL;
  f->x_nonlocal_goto_handler_labels = NULL;
  f->x_nonlocal_goto_stack_level = NULL;
  f->x_cleanup_label = NULL;
  f->x_return_label = NULL;
  f->x_save_expr_regs = NULL;
  f->x_stack_slot_list = NULL;
  f->x_rtl_expr_chain = NULL;
  f->x_tail_recursion_label = NULL;
  f->x_tail_recursion_reentry = NULL;
  f->x_arg_pointer_save_area = NULL;
  f->x_context_display = NULL;
  f->x_trampoline_list = NULL;
  f->x_parm_birth_insn = NULL;
  f->x_last_parm_insn = NULL;
  f->x_parm_reg_stack_loc = NULL;
  f->x_temp_slots = NULL;
  f->fixup_var_refs_queue = NULL;
  f->original_arg_vector = NULL;
  f->original_decl_initial = NULL;
  f->inl_last_parm_insn = NULL;
  f->epilogue_delay_list = NULL;
}


/* Allocate fixed slots in the stack frame of the current function.  */

/* Return size needed for stack frame based on slots so far allocated in
   function F.
   This size counts from zero.  It is not rounded to PREFERRED_STACK_BOUNDARY;
   the caller may have to do that.  */

HOST_WIDE_INT
get_func_frame_size (f)
     struct function *f;
{
#ifdef FRAME_GROWS_DOWNWARD
  return -f->x_frame_offset;
#else
  return f->x_frame_offset;
#endif
}

/* Return size needed for stack frame based on slots so far allocated.
   This size counts from zero.  It is not rounded to PREFERRED_STACK_BOUNDARY;
   the caller may have to do that.  */
HOST_WIDE_INT
get_frame_size ()
{
  return get_func_frame_size (cfun);
}

/* Allocate a stack slot of SIZE bytes and return a MEM rtx for it
   with machine mode MODE.
   
   ALIGN controls the amount of alignment for the address of the slot:
   0 means according to MODE,
   -1 means use BIGGEST_ALIGNMENT and round size to multiple of that,
   positive specifies alignment boundary in bits.

   We do not round to stack_boundary here.

   FUNCTION specifies the function to allocate in.  */

static rtx
assign_stack_local_1 (mode, size, align, function)
     enum machine_mode mode;
     HOST_WIDE_INT size;
     int align;
     struct function *function;
{
  register rtx x, addr;
  int bigend_correction = 0;
  int alignment;

  /* Allocate in the memory associated with the function in whose frame
     we are assigning.  */
  if (function != cfun)
    push_obstacks (function->function_obstack,
		   function->function_maybepermanent_obstack);

  if (align == 0)
    {
      tree type;

      alignment = GET_MODE_ALIGNMENT (mode);
      if (mode == BLKmode)
	alignment = BIGGEST_ALIGNMENT;

      /* Allow the target to (possibly) increase the alignment of this
	 stack slot.  */
      type = type_for_mode (mode, 0);
      if (type)
	alignment = LOCAL_ALIGNMENT (type, alignment);

      alignment /= BITS_PER_UNIT;
    }
  else if (align == -1)
    {
      alignment = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
      size = CEIL_ROUND (size, alignment);
    }
  else
    alignment = align / BITS_PER_UNIT;

#ifdef FRAME_GROWS_DOWNWARD
  function->x_frame_offset -= size;
#endif

  /* Ignore alignment we can't do with expected alignment of the boundary.  */
  if (alignment * BITS_PER_UNIT > PREFERRED_STACK_BOUNDARY)
    alignment = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;

  if (function->stack_alignment_needed < alignment * BITS_PER_UNIT)
    function->stack_alignment_needed = alignment * BITS_PER_UNIT;

  /* Round frame offset to that alignment.
     We must be careful here, since FRAME_OFFSET might be negative and
     division with a negative dividend isn't as well defined as we might
     like.  So we instead assume that ALIGNMENT is a power of two and
     use logical operations which are unambiguous.  */
#ifdef FRAME_GROWS_DOWNWARD
  function->x_frame_offset = FLOOR_ROUND (function->x_frame_offset, alignment);
#else
  function->x_frame_offset = CEIL_ROUND (function->x_frame_offset, alignment);
#endif

  /* On a big-endian machine, if we are allocating more space than we will use,
     use the least significant bytes of those that are allocated.  */
  if (BYTES_BIG_ENDIAN && mode != BLKmode)
    bigend_correction = size - GET_MODE_SIZE (mode);

  /* If we have already instantiated virtual registers, return the actual
     address relative to the frame pointer.  */
  if (function == cfun && virtuals_instantiated)
    addr = plus_constant (frame_pointer_rtx,
			  (frame_offset + bigend_correction
			   + STARTING_FRAME_OFFSET));
  else
    addr = plus_constant (virtual_stack_vars_rtx,
			  function->x_frame_offset + bigend_correction);

#ifndef FRAME_GROWS_DOWNWARD
  function->x_frame_offset += size;
#endif

  x = gen_rtx_MEM (mode, addr);

  function->x_stack_slot_list
    = gen_rtx_EXPR_LIST (VOIDmode, x, function->x_stack_slot_list);

  if (function != cfun)
    pop_obstacks ();

  return x;
}

/* Wrapper around assign_stack_local_1;  assign a local stack slot for the
   current function.  */
rtx
assign_stack_local (mode, size, align)
     enum machine_mode mode;
     HOST_WIDE_INT size;
     int align;
{
  return assign_stack_local_1 (mode, size, align, cfun);
}

/* Allocate a temporary stack slot and record it for possible later
   reuse.

   MODE is the machine mode to be given to the returned rtx.

   SIZE is the size in units of the space required.  We do no rounding here
   since assign_stack_local will do any required rounding.

   KEEP is 1 if this slot is to be retained after a call to
   free_temp_slots.  Automatic variables for a block are allocated
   with this flag.  KEEP is 2 if we allocate a longer term temporary,
   whose lifetime is controlled by CLEANUP_POINT_EXPRs.  KEEP is 3
   if we are to allocate something at an inner level to be treated as
   a variable in the block (e.g., a SAVE_EXPR).  

   TYPE is the type that will be used for the stack slot.  */

static rtx
assign_stack_temp_for_type (mode, size, keep, type)
     enum machine_mode mode;
     HOST_WIDE_INT size;
     int keep;
     tree type;
{
  int align;
  int alias_set;
  struct temp_slot *p, *best_p = 0;

  /* If SIZE is -1 it means that somebody tried to allocate a temporary
     of a variable size.  */
  if (size == -1)
    abort ();

  /* If we know the alias set for the memory that will be used, use
     it.  If there's no TYPE, then we don't know anything about the
     alias set for the memory.  */
  if (type)
    alias_set = get_alias_set (type);
  else 
    alias_set = 0;

  align = GET_MODE_ALIGNMENT (mode);
  if (mode == BLKmode)
    align = BIGGEST_ALIGNMENT;

  if (! type)
    type = type_for_mode (mode, 0);
  if (type)
    align = LOCAL_ALIGNMENT (type, align);

  /* Try to find an available, already-allocated temporary of the proper
     mode which meets the size and alignment requirements.  Choose the
     smallest one with the closest alignment.  */
  for (p = temp_slots; p; p = p->next)
    if (p->align >= align && p->size >= size && GET_MODE (p->slot) == mode
	&& ! p->in_use
	&& (!flag_strict_aliasing
	    || (alias_set && p->alias_set == alias_set))
	&& (best_p == 0 || best_p->size > p->size
	    || (best_p->size == p->size && best_p->align > p->align)))
      {
	if (p->align == align && p->size == size)
	  {
	    best_p = 0;
	    break;
	  }
	best_p = p;
      }

  /* Make our best, if any, the one to use.  */
  if (best_p)
    {
      /* If there are enough aligned bytes left over, make them into a new
	 temp_slot so that the extra bytes don't get wasted.  Do this only
	 for BLKmode slots, so that we can be sure of the alignment.  */
      if (GET_MODE (best_p->slot) == BLKmode
	  /* We can't split slots if -fstrict-aliasing because the
	     information about the alias set for the new slot will be
	     lost.  */
	  && !flag_strict_aliasing)
	{
	  int alignment = best_p->align / BITS_PER_UNIT;
	  HOST_WIDE_INT rounded_size = CEIL_ROUND (size, alignment);

	  if (best_p->size - rounded_size >= alignment)
	    {
	      p = (struct temp_slot *) oballoc (sizeof (struct temp_slot));
	      p->in_use = p->addr_taken = 0;
	      p->size = best_p->size - rounded_size;
	      p->base_offset = best_p->base_offset + rounded_size;
	      p->full_size = best_p->full_size - rounded_size;
	      p->slot = gen_rtx_MEM (BLKmode,
				     plus_constant (XEXP (best_p->slot, 0),
						    rounded_size));
	      p->align = best_p->align;
	      p->address = 0;
	      p->rtl_expr = 0;
	      p->next = temp_slots;
	      temp_slots = p;

	      stack_slot_list = gen_rtx_EXPR_LIST (VOIDmode, p->slot,
						   stack_slot_list);

	      best_p->size = rounded_size;
	      best_p->full_size = rounded_size;
	    }
	}

      p = best_p;
    }
	      
  /* If we still didn't find one, make a new temporary.  */
  if (p == 0)
    {
      HOST_WIDE_INT frame_offset_old = frame_offset;

      p = (struct temp_slot *) oballoc (sizeof (struct temp_slot));

      /* We are passing an explicit alignment request to assign_stack_local.
	 One side effect of that is assign_stack_local will not round SIZE
	 to ensure the frame offset remains suitably aligned.

	 So for requests which depended on the rounding of SIZE, we go ahead
	 and round it now.  We also make sure ALIGNMENT is at least
	 BIGGEST_ALIGNMENT.  */
      if (mode == BLKmode && align < BIGGEST_ALIGNMENT)
	abort();
      p->slot = assign_stack_local (mode,
				    (mode == BLKmode
				     ? CEIL_ROUND (size, align / BITS_PER_UNIT)
				     : size),
				    align);

      p->align = align;
      p->alias_set = alias_set;

      /* The following slot size computation is necessary because we don't
	 know the actual size of the temporary slot until assign_stack_local
	 has performed all the frame alignment and size rounding for the
	 requested temporary.  Note that extra space added for alignment
	 can be either above or below this stack slot depending on which
	 way the frame grows.  We include the extra space if and only if it
	 is above this slot.  */
#ifdef FRAME_GROWS_DOWNWARD
      p->size = frame_offset_old - frame_offset;
#else
      p->size = size;
#endif

      /* Now define the fields used by combine_temp_slots.  */
#ifdef FRAME_GROWS_DOWNWARD
      p->base_offset = frame_offset;
      p->full_size = frame_offset_old - frame_offset;
#else
      p->base_offset = frame_offset_old;
      p->full_size = frame_offset - frame_offset_old;
#endif
      p->address = 0;
      p->next = temp_slots;
      temp_slots = p;
    }

  p->in_use = 1;
  p->addr_taken = 0;
  p->rtl_expr = seq_rtl_expr;

  if (keep == 2)
    {
      p->level = target_temp_slot_level;
      p->keep = 0;
    }
  else if (keep == 3)
    {
      p->level = var_temp_slot_level;
      p->keep = 0;
    }
  else
    {
      p->level = temp_slot_level;
      p->keep = keep;
    }

  /* We may be reusing an old slot, so clear any MEM flags that may have been
     set from before.  */
  RTX_UNCHANGING_P (p->slot) = 0;
  MEM_IN_STRUCT_P (p->slot) = 0;
  MEM_SCALAR_P (p->slot) = 0;
  MEM_ALIAS_SET (p->slot) = 0;
  return p->slot;
}

/* Allocate a temporary stack slot and record it for possible later
   reuse.  First three arguments are same as in preceding function.  */

rtx
assign_stack_temp (mode, size, keep)
     enum machine_mode mode;
     HOST_WIDE_INT size;
     int keep;
{
  return assign_stack_temp_for_type (mode, size, keep, NULL_TREE);
}

/* Assign a temporary of given TYPE.
   KEEP is as for assign_stack_temp.
   MEMORY_REQUIRED is 1 if the result must be addressable stack memory;
   it is 0 if a register is OK.
   DONT_PROMOTE is 1 if we should not promote values in register
   to wider modes.  */

rtx
assign_temp (type, keep, memory_required, dont_promote)
     tree type;
     int keep;
     int memory_required;
     int dont_promote ATTRIBUTE_UNUSED;
{
  enum machine_mode mode = TYPE_MODE (type);
#ifndef PROMOTE_FOR_CALL_ONLY
  int unsignedp = TREE_UNSIGNED (type);
#endif

  if (mode == BLKmode || memory_required)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      rtx tmp;

      /* Unfortunately, we don't yet know how to allocate variable-sized
	 temporaries.  However, sometimes we have a fixed upper limit on
	 the size (which is stored in TYPE_ARRAY_MAX_SIZE) and can use that
	 instead.  This is the case for Chill variable-sized strings.  */
      if (size == -1 && TREE_CODE (type) == ARRAY_TYPE
	  && TYPE_ARRAY_MAX_SIZE (type) != NULL_TREE
	  && TREE_CODE (TYPE_ARRAY_MAX_SIZE (type)) == INTEGER_CST)
	size = TREE_INT_CST_LOW (TYPE_ARRAY_MAX_SIZE (type));

      tmp = assign_stack_temp_for_type (mode, size, keep, type);
      MEM_SET_IN_STRUCT_P (tmp, AGGREGATE_TYPE_P (type));
      return tmp;
    }

#ifndef PROMOTE_FOR_CALL_ONLY
  if (! dont_promote)
    mode = promote_mode (type, mode, &unsignedp, 0);
#endif

  return gen_reg_rtx (mode);
}

/* Combine temporary stack slots which are adjacent on the stack.

   This allows for better use of already allocated stack space.  This is only
   done for BLKmode slots because we can be sure that we won't have alignment
   problems in this case.  */

void
combine_temp_slots ()
{
  struct temp_slot *p, *q;
  struct temp_slot *prev_p, *prev_q;
  int num_slots;

  /* We can't combine slots, because the information about which slot
     is in which alias set will be lost.  */
  if (flag_strict_aliasing)
    return;

  /* If there are a lot of temp slots, don't do anything unless 
     high levels of optimizaton.  */
  if (! flag_expensive_optimizations)
    for (p = temp_slots, num_slots = 0; p; p = p->next, num_slots++)
      if (num_slots > 100 || (num_slots > 10 && optimize == 0))
	return;

  for (p = temp_slots, prev_p = 0; p; p = prev_p ? prev_p->next : temp_slots)
    {
      int delete_p = 0;

      if (! p->in_use && GET_MODE (p->slot) == BLKmode)
	for (q = p->next, prev_q = p; q; q = prev_q->next)
	  {
	    int delete_q = 0;
	    if (! q->in_use && GET_MODE (q->slot) == BLKmode)
	      {
		if (p->base_offset + p->full_size == q->base_offset)
		  {
		    /* Q comes after P; combine Q into P.  */
		    p->size += q->size;
		    p->full_size += q->full_size;
		    delete_q = 1;
		  }
		else if (q->base_offset + q->full_size == p->base_offset)
		  {
		    /* P comes after Q; combine P into Q.  */
		    q->size += p->size;
		    q->full_size += p->full_size;
		    delete_p = 1;
		    break;
		  }
	      }
	    /* Either delete Q or advance past it.  */
	    if (delete_q)
	      prev_q->next = q->next;
	    else
	      prev_q = q;
	  }
      /* Either delete P or advance past it.  */
      if (delete_p)
	{
	  if (prev_p)
	    prev_p->next = p->next;
	  else
	    temp_slots = p->next;
	}
      else
	prev_p = p;
    }
}

/* Find the temp slot corresponding to the object at address X.  */

static struct temp_slot *
find_temp_slot_from_address (x)
     rtx x;
{
  struct temp_slot *p;
  rtx next;

  for (p = temp_slots; p; p = p->next)
    {
      if (! p->in_use)
	continue;

      else if (XEXP (p->slot, 0) == x
	       || p->address == x
	       || (GET_CODE (x) == PLUS
		   && XEXP (x, 0) == virtual_stack_vars_rtx
		   && GET_CODE (XEXP (x, 1)) == CONST_INT
		   && INTVAL (XEXP (x, 1)) >= p->base_offset
		   && INTVAL (XEXP (x, 1)) < p->base_offset + p->full_size))
	return p;

      else if (p->address != 0 && GET_CODE (p->address) == EXPR_LIST)
	for (next = p->address; next; next = XEXP (next, 1))
	  if (XEXP (next, 0) == x)
	    return p;
    }

  /* If we have a sum involving a register, see if it points to a temp
     slot.  */
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == REG
      && (p = find_temp_slot_from_address (XEXP (x, 0))) != 0)
    return p;
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == REG
	   && (p = find_temp_slot_from_address (XEXP (x, 1))) != 0)
    return p;

  return 0;
}
      
/* Indicate that NEW is an alternate way of referring to the temp slot
   that previously was known by OLD.  */

void
update_temp_slot_address (old, new)
     rtx old, new;
{
  struct temp_slot *p;

  if (rtx_equal_p (old, new))
    return;

  p = find_temp_slot_from_address (old);

  /* If we didn't find one, see if both OLD is a PLUS.  If so, and NEW
     is a register, see if one operand of the PLUS is a temporary
     location.  If so, NEW points into it.  Otherwise, if both OLD and
     NEW are a PLUS and if there is a register in common between them.
     If so, try a recursive call on those values.  */
  if (p == 0)
    {
      if (GET_CODE (old) != PLUS)
	return;

      if (GET_CODE (new) == REG)
	{
	  update_temp_slot_address (XEXP (old, 0), new);
	  update_temp_slot_address (XEXP (old, 1), new);
	  return;
	}
      else if (GET_CODE (new) != PLUS)
	return;

      if (rtx_equal_p (XEXP (old, 0), XEXP (new, 0)))
	update_temp_slot_address (XEXP (old, 1), XEXP (new, 1));
      else if (rtx_equal_p (XEXP (old, 1), XEXP (new, 0)))
	update_temp_slot_address (XEXP (old, 0), XEXP (new, 1));
      else if (rtx_equal_p (XEXP (old, 0), XEXP (new, 1)))
	update_temp_slot_address (XEXP (old, 1), XEXP (new, 0));
      else if (rtx_equal_p (XEXP (old, 1), XEXP (new, 1)))
	update_temp_slot_address (XEXP (old, 0), XEXP (new, 0));

      return;
    }

  /* Otherwise add an alias for the temp's address. */
  else if (p->address == 0)
    p->address = new;
  else
    {
      if (GET_CODE (p->address) != EXPR_LIST)
	p->address = gen_rtx_EXPR_LIST (VOIDmode, p->address, NULL_RTX);

      p->address = gen_rtx_EXPR_LIST (VOIDmode, new, p->address);
    }
}

/* If X could be a reference to a temporary slot, mark the fact that its
   address was taken.  */

void
mark_temp_addr_taken (x)
     rtx x;
{
  struct temp_slot *p;

  if (x == 0)
    return;

  /* If X is not in memory or is at a constant address, it cannot be in
     a temporary slot.  */
  if (GET_CODE (x) != MEM || CONSTANT_P (XEXP (x, 0)))
    return;

  p = find_temp_slot_from_address (XEXP (x, 0));
  if (p != 0)
    p->addr_taken = 1;
}

/* If X could be a reference to a temporary slot, mark that slot as
   belonging to the to one level higher than the current level.  If X
   matched one of our slots, just mark that one.  Otherwise, we can't
   easily predict which it is, so upgrade all of them.  Kept slots
   need not be touched.

   This is called when an ({...}) construct occurs and a statement
   returns a value in memory.  */

void
preserve_temp_slots (x)
     rtx x;
{
  struct temp_slot *p = 0;

  /* If there is no result, we still might have some objects whose address
     were taken, so we need to make sure they stay around.  */
  if (x == 0)
    {
      for (p = temp_slots; p; p = p->next)
	if (p->in_use && p->level == temp_slot_level && p->addr_taken)
	  p->level--;

      return;
    }

  /* If X is a register that is being used as a pointer, see if we have
     a temporary slot we know it points to.  To be consistent with
     the code below, we really should preserve all non-kept slots
     if we can't find a match, but that seems to be much too costly.  */
  if (GET_CODE (x) == REG && REGNO_POINTER_FLAG (REGNO (x)))
    p = find_temp_slot_from_address (x);

  /* If X is not in memory or is at a constant address, it cannot be in
     a temporary slot, but it can contain something whose address was
     taken.  */
  if (p == 0 && (GET_CODE (x) != MEM || CONSTANT_P (XEXP (x, 0))))
    {
      for (p = temp_slots; p; p = p->next)
	if (p->in_use && p->level == temp_slot_level && p->addr_taken)
	  p->level--;

      return;
    }

  /* First see if we can find a match.  */
  if (p == 0)
    p = find_temp_slot_from_address (XEXP (x, 0));

  if (p != 0)
    {
      /* Move everything at our level whose address was taken to our new
	 level in case we used its address.  */
      struct temp_slot *q;

      if (p->level == temp_slot_level)
	{
	  for (q = temp_slots; q; q = q->next)
	    if (q != p && q->addr_taken && q->level == p->level)
	      q->level--;

	  p->level--;
	  p->addr_taken = 0;
	}
      return;
    }

  /* Otherwise, preserve all non-kept slots at this level.  */
  for (p = temp_slots; p; p = p->next)
    if (p->in_use && p->level == temp_slot_level && ! p->keep)
      p->level--;
}

/* X is the result of an RTL_EXPR.  If it is a temporary slot associated
   with that RTL_EXPR, promote it into a temporary slot at the present
   level so it will not be freed when we free slots made in the
   RTL_EXPR.  */

void
preserve_rtl_expr_result (x)
     rtx x;
{
  struct temp_slot *p;

  /* If X is not in memory or is at a constant address, it cannot be in
     a temporary slot.  */
  if (x == 0 || GET_CODE (x) != MEM || CONSTANT_P (XEXP (x, 0)))
    return;

  /* If we can find a match, move it to our level unless it is already at
     an upper level.  */
  p = find_temp_slot_from_address (XEXP (x, 0));
  if (p != 0)
    {
      p->level = MIN (p->level, temp_slot_level);
      p->rtl_expr = 0;
    }

  return;
}

/* Free all temporaries used so far.  This is normally called at the end
   of generating code for a statement.  Don't free any temporaries
   currently in use for an RTL_EXPR that hasn't yet been emitted.
   We could eventually do better than this since it can be reused while
   generating the same RTL_EXPR, but this is complex and probably not
   worthwhile.  */

void
free_temp_slots ()
{
  struct temp_slot *p;

  for (p = temp_slots; p; p = p->next)
    if (p->in_use && p->level == temp_slot_level && ! p->keep
	&& p->rtl_expr == 0)
      p->in_use = 0;

  combine_temp_slots ();
}

/* Free all temporary slots used in T, an RTL_EXPR node.  */

void
free_temps_for_rtl_expr (t)
     tree t;
{
  struct temp_slot *p;

  for (p = temp_slots; p; p = p->next)
    if (p->rtl_expr == t)
      p->in_use = 0;

  combine_temp_slots ();
}

/* Mark all temporaries ever allocated in this function as not suitable
   for reuse until the current level is exited.  */

void
mark_all_temps_used ()
{
  struct temp_slot *p;

  for (p = temp_slots; p; p = p->next)
    {
      p->in_use = p->keep = 1;
      p->level = MIN (p->level, temp_slot_level);
    }
}

/* Push deeper into the nesting level for stack temporaries.  */

void
push_temp_slots ()
{
  temp_slot_level++;
}

/* Likewise, but save the new level as the place to allocate variables
   for blocks.  */

#if 0
void
push_temp_slots_for_block ()
{
  push_temp_slots ();

  var_temp_slot_level = temp_slot_level;
}

/* Likewise, but save the new level as the place to allocate temporaries
   for TARGET_EXPRs.  */

void
push_temp_slots_for_target ()
{
  push_temp_slots ();

  target_temp_slot_level = temp_slot_level;
}

/* Set and get the value of target_temp_slot_level.  The only
   permitted use of these functions is to save and restore this value.  */

int
get_target_temp_slot_level ()
{
  return target_temp_slot_level;
}

void
set_target_temp_slot_level (level)
     int level;
{
  target_temp_slot_level = level;
}
#endif

/* Pop a temporary nesting level.  All slots in use in the current level
   are freed.  */

void
pop_temp_slots ()
{
  struct temp_slot *p;

  for (p = temp_slots; p; p = p->next)
    if (p->in_use && p->level == temp_slot_level && p->rtl_expr == 0)
      p->in_use = 0;

  combine_temp_slots ();

  temp_slot_level--;
}

/* Initialize temporary slots.  */

void
init_temp_slots ()
{
  /* We have not allocated any temporaries yet.  */
  temp_slots = 0;
  temp_slot_level = 0;
  var_temp_slot_level = 0;
  target_temp_slot_level = 0;
}

/* Retroactively move an auto variable from a register to a stack slot.
   This is done when an address-reference to the variable is seen.  */

void
put_var_into_stack (decl)
     tree decl;
{
  register rtx reg;
  enum machine_mode promoted_mode, decl_mode;
  struct function *function = 0;
  tree context;
  int can_use_addressof;

  context = decl_function_context (decl);

  /* Get the current rtl used for this object and its original mode.  */
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
  if (context != current_function_decl && context != inline_function_decl)
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

  can_use_addressof
    = (function == 0
       && optimize > 0
       /* FIXME make it work for promoted modes too */
       && decl_mode == promoted_mode
#ifdef NON_SAVING_SETJMP
       && ! (NON_SAVING_SETJMP && current_function_calls_setjmp)
#endif
       );

  /* If we can't use ADDRESSOF, make sure we see through one we already
     generated.  */
  if (! can_use_addressof && GET_CODE (reg) == MEM
      && GET_CODE (XEXP (reg, 0)) == ADDRESSOF)
    reg = XEXP (XEXP (reg, 0), 0);

  /* Now we should have a value that resides in one or more pseudo regs.  */

  if (GET_CODE (reg) == REG)
    {
      /* If this variable lives in the current function and we don't need
	 to put things in the stack for the sake of setjmp, try to keep it
	 in a register until we know we actually need the address.  */
      if (can_use_addressof)
	gen_mem_addressof (reg, decl);
      else
	put_reg_into_stack (function, reg, TREE_TYPE (decl),
			    promoted_mode, decl_mode,
			    TREE_SIDE_EFFECTS (decl), 0,
			    TREE_USED (decl) || DECL_INITIAL (decl) != 0,
			    0);
    }
  else if (GET_CODE (reg) == CONCAT)
    {
      /* A CONCAT contains two pseudos; put them both in the stack.
	 We do it so they end up consecutive.  */
      enum machine_mode part_mode = GET_MODE (XEXP (reg, 0));
      tree part_type = TREE_TYPE (TREE_TYPE (decl));
#ifdef FRAME_GROWS_DOWNWARD
      /* Since part 0 should have a lower address, do it second.  */
      put_reg_into_stack (function, XEXP (reg, 1), part_type, part_mode,
			  part_mode, TREE_SIDE_EFFECTS (decl), 0,
			  TREE_USED (decl) || DECL_INITIAL (decl) != 0,
			  0);
      put_reg_into_stack (function, XEXP (reg, 0), part_type, part_mode,
			  part_mode, TREE_SIDE_EFFECTS (decl), 0,
			  TREE_USED (decl) || DECL_INITIAL (decl) != 0,
			  0);
#else
      put_reg_into_stack (function, XEXP (reg, 0), part_type, part_mode,
			  part_mode, TREE_SIDE_EFFECTS (decl), 0,
			  TREE_USED (decl) || DECL_INITIAL (decl) != 0,
			  0);
      put_reg_into_stack (function, XEXP (reg, 1), part_type, part_mode,
			  part_mode, TREE_SIDE_EFFECTS (decl), 0,
			  TREE_USED (decl) || DECL_INITIAL (decl) != 0,
			  0);
#endif

      /* Change the CONCAT into a combined MEM for both parts.  */
      PUT_CODE (reg, MEM);
      MEM_VOLATILE_P (reg) = MEM_VOLATILE_P (XEXP (reg, 0));
      MEM_ALIAS_SET (reg) = get_alias_set (decl);

      /* The two parts are in memory order already.
	 Use the lower parts address as ours.  */
      XEXP (reg, 0) = XEXP (XEXP (reg, 0), 0);
      /* Prevent sharing of rtl that might lose.  */
      if (GET_CODE (XEXP (reg, 0)) == PLUS)
	XEXP (reg, 0) = copy_rtx (XEXP (reg, 0));
    }
  else
    return;
  
  if (current_function_check_memory_usage)
    emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
		       XEXP (reg, 0), Pmode,
		       GEN_INT (GET_MODE_SIZE (GET_MODE (reg))),
		       TYPE_MODE (sizetype),
		       GEN_INT (MEMORY_USE_RW),
		       TYPE_MODE (integer_type_node));
}

/* Subroutine of put_var_into_stack.  This puts a single pseudo reg REG
   into the stack frame of FUNCTION (0 means the current function).
   DECL_MODE is the machine mode of the user-level data type.
   PROMOTED_MODE is the machine mode of the register.
   VOLATILE_P is nonzero if this is for a "volatile" decl.
   USED_P is nonzero if this reg might have already been used in an insn.  */

static void
put_reg_into_stack (function, reg, type, promoted_mode, decl_mode, volatile_p,
		    original_regno, used_p, ht)
     struct function *function;
     rtx reg;
     tree type;
     enum machine_mode promoted_mode, decl_mode;
     int volatile_p;
     int original_regno;
     int used_p;
     struct hash_table *ht;
{
  struct function *func = function ? function : cfun;
  rtx new = 0;
  int regno = original_regno;

  if (regno == 0)
    regno = REGNO (reg);

  if (regno < func->x_max_parm_reg)
    new = func->x_parm_reg_stack_loc[regno];
  if (new == 0)
    new = assign_stack_local_1 (decl_mode, GET_MODE_SIZE (decl_mode), 0, func);

  PUT_CODE (reg, MEM);
  PUT_MODE (reg, decl_mode);
  XEXP (reg, 0) = XEXP (new, 0);
  /* `volatil' bit means one thing for MEMs, another entirely for REGs.  */
  MEM_VOLATILE_P (reg) = volatile_p;

  /* If this is a memory ref that contains aggregate components,
     mark it as such for cse and loop optimize.  If we are reusing a
     previously generated stack slot, then we need to copy the bit in
     case it was set for other reasons.  For instance, it is set for
     __builtin_va_alist.  */
  MEM_SET_IN_STRUCT_P (reg,
		       AGGREGATE_TYPE_P (type) || MEM_IN_STRUCT_P (new));
  MEM_ALIAS_SET (reg) = get_alias_set (type);

  /* Now make sure that all refs to the variable, previously made
     when it was a register, are fixed up to be valid again.  */

  if (used_p && function != 0)
    {
      struct var_refs_queue *temp;

      temp
	= (struct var_refs_queue *) xmalloc (sizeof (struct var_refs_queue));
      temp->modified = reg;
      temp->promoted_mode = promoted_mode;
      temp->unsignedp = TREE_UNSIGNED (type);
      temp->next = function->fixup_var_refs_queue;
      function->fixup_var_refs_queue = temp;
    }
  else if (used_p)
    /* Variable is local; fix it up now.  */
    fixup_var_refs (reg, promoted_mode, TREE_UNSIGNED (type), ht);
}

static void
fixup_var_refs (var, promoted_mode, unsignedp, ht)
     rtx var;
     enum machine_mode promoted_mode;
     int unsignedp;
     struct hash_table *ht;
{
  tree pending;
  rtx first_insn = get_insns ();
  struct sequence_stack *stack = seq_stack;
  tree rtl_exps = rtl_expr_chain;

  /* Must scan all insns for stack-refs that exceed the limit.  */
  fixup_var_refs_insns (var, promoted_mode, unsignedp, first_insn, 
			stack == 0, ht);
  /* If there's a hash table, it must record all uses of VAR.  */
  if (ht)
    return;

  /* Scan all pending sequences too.  */
  for (; stack; stack = stack->next)
    {
      push_to_sequence (stack->first);
      fixup_var_refs_insns (var, promoted_mode, unsignedp,
			    stack->first, stack->next != 0, 0);
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
	  fixup_var_refs_insns (var, promoted_mode, unsignedp, seq, 0,
				0);
	  end_sequence ();
	}
    }

  /* Scan the catch clauses for exception handling too.  */
  push_to_sequence (catch_clauses);
  fixup_var_refs_insns (var, promoted_mode, unsignedp, catch_clauses,
			0, 0);
  end_sequence ();
}

/* REPLACEMENTS is a pointer to a list of the struct fixup_replacement and X is
   some part of an insn.  Return a struct fixup_replacement whose OLD
   value is equal to X.  Allocate a new structure if no such entry exists.  */

static struct fixup_replacement *
find_fixup_replacement (replacements, x)
     struct fixup_replacement **replacements;
     rtx x;
{
  struct fixup_replacement *p;

  /* See if we have already replaced this.  */
  for (p = *replacements; p != 0 && ! rtx_equal_p (p->old, x); p = p->next)
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
fixup_var_refs_insns (var, promoted_mode, unsignedp, insn, toplevel, ht)
     rtx var;
     enum machine_mode promoted_mode;
     int unsignedp;
     rtx insn;
     int toplevel;
     struct hash_table *ht;
{
  rtx call_dest = 0;
  rtx insn_list = NULL_RTX;

  /* If we already know which INSNs reference VAR there's no need
     to walk the entire instruction chain.  */
  if (ht)
    {
      insn_list = ((struct insns_for_mem_entry *) 
		   hash_lookup (ht, var, /*create=*/0, /*copy=*/0))->insns;
      insn = insn_list ? XEXP (insn_list, 0) : NULL_RTX;
      insn_list = XEXP (insn_list, 1);
    }

  while (insn)
    {
      rtx next = NEXT_INSN (insn);
      rtx set, prev, prev_set;
      rtx note;

      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  /* Remember the notes in case we delete the insn.  */
	  note = REG_NOTES (insn);

	  /* If this is a CLOBBER of VAR, delete it.

	     If it has a REG_LIBCALL note, delete the REG_LIBCALL
	     and REG_RETVAL notes too.  */
 	  if (GET_CODE (PATTERN (insn)) == CLOBBER
	      && (XEXP (PATTERN (insn), 0) == var
		  || (GET_CODE (XEXP (PATTERN (insn), 0)) == CONCAT
		      && (XEXP (XEXP (PATTERN (insn), 0), 0) == var
			  || XEXP (XEXP (PATTERN (insn), 0), 1) == var))))
	    {
	      if ((note = find_reg_note (insn, REG_LIBCALL, NULL_RTX)) != 0)
		/* The REG_LIBCALL note will go away since we are going to
		   turn INSN into a NOTE, so just delete the
		   corresponding REG_RETVAL note.  */
		remove_note (XEXP (note, 0),
			     find_reg_note (XEXP (note, 0), REG_RETVAL,
					    NULL_RTX));

	      /* In unoptimized compilation, we shouldn't call delete_insn
		 except in jump.c doing warnings.  */
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;
	    }

	  /* The insn to load VAR from a home in the arglist
	     is now a no-op.  When we see it, just delete it.
	     Similarly if this is storing VAR from a register from which
	     it was loaded in the previous insn.  This will occur
	     when an ADDRESSOF was made for an arglist slot.  */
	  else if (toplevel
		   && (set = single_set (insn)) != 0
		   && SET_DEST (set) == var
		   /* If this represents the result of an insn group,
		      don't delete the insn.  */
		   && find_reg_note (insn, REG_RETVAL, NULL_RTX) == 0
		   && (rtx_equal_p (SET_SRC (set), var)
		       || (GET_CODE (SET_SRC (set)) == REG
			   && (prev = prev_nonnote_insn (insn)) != 0
			   && (prev_set = single_set (prev)) != 0
			   && SET_DEST (prev_set) == SET_SRC (set)
			   && rtx_equal_p (SET_SRC (prev_set), var))))
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
	      struct fixup_replacement *replacements = 0;
	      rtx next_insn = NEXT_INSN (insn);

	      if (SMALL_REGISTER_CLASSES)
		{
		  /* If the insn that copies the results of a CALL_INSN
		     into a pseudo now references VAR, we have to use an
		     intermediate pseudo since we want the life of the
		     return value register to be only a single insn.

		     If we don't use an intermediate pseudo, such things as
		     address computations to make the address of VAR valid
		     if it is not can be placed between the CALL_INSN and INSN.

		     To make sure this doesn't happen, we record the destination
		     of the CALL_INSN and see if the next insn uses both that
		     and VAR.  */

		  if (call_dest != 0 && GET_CODE (insn) == INSN
		      && reg_mentioned_p (var, PATTERN (insn))
		      && reg_mentioned_p (call_dest, PATTERN (insn)))
		    {
		      rtx temp = gen_reg_rtx (GET_MODE (call_dest));

		      emit_insn_before (gen_move_insn (temp, call_dest), insn);

		      PATTERN (insn) = replace_rtx (PATTERN (insn),
						    call_dest, temp);
		    }
	      
		  if (GET_CODE (insn) == CALL_INSN
		      && GET_CODE (PATTERN (insn)) == SET)
		    call_dest = SET_DEST (PATTERN (insn));
		  else if (GET_CODE (insn) == CALL_INSN
			   && GET_CODE (PATTERN (insn)) == PARALLEL
			   && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
		    call_dest = SET_DEST (XVECEXP (PATTERN (insn), 0, 0));
		  else
		    call_dest = 0;
		}

	      /* See if we have to do anything to INSN now that VAR is in
		 memory.  If it needs to be loaded into a pseudo, use a single
		 pseudo for the entire insn in case there is a MATCH_DUP
		 between two operands.  We pass a pointer to the head of
		 a list of struct fixup_replacements.  If fixup_var_refs_1
		 needs to allocate pseudos or replacement MEMs (for SUBREGs),
		 it will record them in this list.
		 
		 If it allocated a pseudo for any replacement, we copy into
		 it here.  */

	      fixup_var_refs_1 (var, promoted_mode, &PATTERN (insn), insn,
				&replacements);

	      /* If this is last_parm_insn, and any instructions were output
		 after it to fix it up, then we must set last_parm_insn to
		 the last such instruction emitted.  */
	      if (insn == last_parm_insn)
		last_parm_insn = PREV_INSN (next_insn);

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
	  while (note)
	    {
	      if (GET_CODE (note) != INSN_LIST)
		XEXP (note, 0)
		  = walk_fixup_memory_subreg (XEXP (note, 0), insn, 1);
	       note = XEXP (note, 1);
	    }
	}

      if (!ht)
	insn = next;
      else if (insn_list)
	{
	  insn = XEXP (insn_list, 0);
	  insn_list = XEXP (insn_list, 1);
	}
      else
	insn = NULL_RTX;
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
  register const char *fmt;
  register rtx tem, tem1;
  struct fixup_replacement *replacement;

  switch (code)
    {
    case ADDRESSOF:
      if (XEXP (x, 0) == var)
	{
	  /* Prevent sharing of rtl that might lose.  */
	  rtx sub = copy_rtx (XEXP (var, 0));

	  if (! validate_change (insn, loc, sub, 0))
	    {
	      rtx y = gen_reg_rtx (GET_MODE (sub));
	      rtx seq, new_insn;

	      /* We should be able to replace with a register or all is lost.
		 Note that we can't use validate_change to verify this, since
		 we're not caring for replacing all dups simultaneously.  */
	      if (! validate_replace_rtx (*loc, y, insn))
		abort ();

	      /* Careful!  First try to recognize a direct move of the
		 value, mimicking how things are done in gen_reload wrt
		 PLUS.  Consider what happens when insn is a conditional
		 move instruction and addsi3 clobbers flags.  */

	      start_sequence ();
	      new_insn = emit_insn (gen_rtx_SET (VOIDmode, y, sub));
	      seq = gen_sequence ();
	      end_sequence ();

	      if (recog_memoized (new_insn) < 0)
		{
		  /* That failed.  Fall back on force_operand and hope.  */

		  start_sequence ();
		  force_operand (sub, y);
		  seq = gen_sequence ();
		  end_sequence ();
		}

#ifdef HAVE_cc0
	      /* Don't separate setter from user.  */
	      if (PREV_INSN (insn) && sets_cc0_p (PREV_INSN (insn)))
		insn = PREV_INSN (insn);
#endif

	      emit_insn_before (seq, insn);
	    }
	}
      return;

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
	    {
	      if (GET_MODE_BITSIZE (GET_MODE (tem))
		  > GET_MODE_BITSIZE (GET_MODE (var)))
		{
		  replacement = find_fixup_replacement (replacements, var);
		  if (replacement->new == 0)
		    replacement->new = gen_reg_rtx (GET_MODE (var));
		  SUBREG_REG (tem) = replacement->new;
		}
	      else
		tem = fixup_memory_subreg (tem, insn, 0);
	    }
	  else
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
	      HOST_WIDE_INT pos = INTVAL (XEXP (x, 2));

#ifdef HAVE_extzv
	      if (GET_CODE (x) == ZERO_EXTRACT)
		{
		  wanted_mode
		    = insn_data[(int) CODE_FOR_extzv].operand[1].mode;
		  if (wanted_mode == VOIDmode)
		    wanted_mode = word_mode;
		}
#endif
#ifdef HAVE_extv
	      if (GET_CODE (x) == SIGN_EXTRACT)
		{
		  wanted_mode = insn_data[(int) CODE_FOR_extv].operand[1].mode;
		  if (wanted_mode == VOIDmode)
		    wanted_mode = word_mode;
		}
#endif
	      /* If we have a narrower mode, we can do something.  */
	      if (wanted_mode != VOIDmode
		  && GET_MODE_SIZE (wanted_mode) < GET_MODE_SIZE (is_mode))
		{
		  HOST_WIDE_INT offset = pos / BITS_PER_UNIT;
		  rtx old_pos = XEXP (x, 2);
		  rtx newmem;

		  /* If the bytes and bits are counted differently, we
		     must adjust the offset.  */
		  if (BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN)
		    offset = (GET_MODE_SIZE (is_mode)
			      - GET_MODE_SIZE (wanted_mode) - offset);

		  pos %= GET_MODE_BITSIZE (wanted_mode);

		  newmem = gen_rtx_MEM (wanted_mode,
					plus_constant (XEXP (tem, 0), offset));
		  RTX_UNCHANGING_P (newmem) = RTX_UNCHANGING_P (tem);
		  MEM_COPY_ATTRIBUTES (newmem, tem);

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

	  INSN_CODE (insn) = -1;
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

      /* For a paradoxical SUBREG inside a ZERO_EXTRACT, load the object
	 into a register and then store it back out.  */
      if (GET_CODE (SET_DEST (x)) == ZERO_EXTRACT
	  && GET_CODE (XEXP (SET_DEST (x), 0)) == SUBREG
	  && SUBREG_REG (XEXP (SET_DEST (x), 0)) == var
	  && (GET_MODE_SIZE (GET_MODE (XEXP (SET_DEST (x), 0)))
	      > GET_MODE_SIZE (GET_MODE (var))))
	{
	  replacement = find_fixup_replacement (replacements, var);
	  if (replacement->new == 0)
	    replacement->new = gen_reg_rtx (GET_MODE (var));

	  SUBREG_REG (XEXP (SET_DEST (x), 0)) = replacement->new;
	  emit_insn_after (gen_move_insn (var, replacement->new), insn);
	}

      /* If SET_DEST is now a paradoxical SUBREG, put the result of this
	 insn into a pseudo and store the low part of the pseudo into VAR.  */
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
#ifdef HAVE_insv
	rtx outerdest = dest;
#endif

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
	      tem = fixup_memory_subreg (tem, insn, 0);
	    else
	      tem = fixup_stack_1 (tem, insn);

	    if (GET_CODE (XEXP (outerdest, 1)) == CONST_INT
		&& GET_CODE (XEXP (outerdest, 2)) == CONST_INT
		&& ! mode_dependent_address_p (XEXP (tem, 0))
		&& ! MEM_VOLATILE_P (tem))
	      {
		enum machine_mode wanted_mode;
		enum machine_mode is_mode = GET_MODE (tem);
		HOST_WIDE_INT pos = INTVAL (XEXP (outerdest, 2));

		wanted_mode = insn_data[(int) CODE_FOR_insv].operand[0].mode;
		if (wanted_mode == VOIDmode)
		  wanted_mode = word_mode;

		/* If we have a narrower mode, we can do something.  */
		if (GET_MODE_SIZE (wanted_mode) < GET_MODE_SIZE (is_mode))
		  {
		    HOST_WIDE_INT offset = pos / BITS_PER_UNIT;
		    rtx old_pos = XEXP (outerdest, 2);
		    rtx newmem;

		    if (BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN)
		      offset = (GET_MODE_SIZE (is_mode)
				- GET_MODE_SIZE (wanted_mode) - offset);

		    pos %= GET_MODE_BITSIZE (wanted_mode);

		    newmem = gen_rtx_MEM (wanted_mode,
					  plus_constant (XEXP (tem, 0),
							 offset));
		    RTX_UNCHANGING_P (newmem) = RTX_UNCHANGING_P (tem);
		    MEM_COPY_ATTRIBUTES (newmem, tem);

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
	   Also fix up the address of the stack slot.

	   Note that we must not try to recognize the insn until
	   after we know that we have valid addresses and no
	   (subreg (mem ...) ...) constructs, since these interfere
	   with determining the validity of the insn.  */

	if ((SET_SRC (x) == var
	     || (GET_CODE (SET_SRC (x)) == SUBREG
		 && SUBREG_REG (SET_SRC (x)) == var))
	    && (GET_CODE (SET_DEST (x)) == REG
		|| (GET_CODE (SET_DEST (x)) == SUBREG
		    && GET_CODE (SUBREG_REG (SET_DEST (x))) == REG))
	    && GET_MODE (var) == promoted_mode
	    && x == single_set (insn))
	  {
	    rtx pat;

	    replacement = find_fixup_replacement (replacements, SET_SRC (x));
	    if (replacement->new)
	      SET_SRC (x) = replacement->new;
	    else if (GET_CODE (SET_SRC (x)) == SUBREG)
	      SET_SRC (x) = replacement->new
		= fixup_memory_subreg (SET_SRC (x), insn, 0);
	    else
	      SET_SRC (x) = replacement->new
		= fixup_stack_1 (SET_SRC (x), insn);

	    if (recog_memoized (insn) >= 0)
	      return;

	    /* INSN is not valid, but we know that we want to
	       copy SET_SRC (x) to SET_DEST (x) in some way.  So
	       we generate the move and see whether it requires more
	       than one insn.  If it does, we emit those insns and
	       delete INSN.  Otherwise, we an just replace the pattern 
	       of INSN; we have already verified above that INSN has
	       no other function that to do X.  */

	    pat = gen_move_insn (SET_DEST (x), SET_SRC (x));
	    if (GET_CODE (pat) == SEQUENCE)
	      {
		emit_insn_after (pat, insn);
		PUT_CODE (insn, NOTE);
		NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (insn) = 0;
	      }
	    else
	      PATTERN (insn) = pat;

	    return;
	  }

	if ((SET_DEST (x) == var
	     || (GET_CODE (SET_DEST (x)) == SUBREG
		 && SUBREG_REG (SET_DEST (x)) == var))
	    && (GET_CODE (SET_SRC (x)) == REG
		|| (GET_CODE (SET_SRC (x)) == SUBREG
		    && GET_CODE (SUBREG_REG (SET_SRC (x))) == REG))
	    && GET_MODE (var) == promoted_mode
	    && x == single_set (insn))
	  {
	    rtx pat;

	    if (GET_CODE (SET_DEST (x)) == SUBREG)
	      SET_DEST (x) = fixup_memory_subreg (SET_DEST (x), insn, 0);
	    else
	      SET_DEST (x) = fixup_stack_1 (SET_DEST (x), insn);

	    if (recog_memoized (insn) >= 0)
	      return;

	    pat = gen_move_insn (SET_DEST (x), SET_SRC (x));
	    if (GET_CODE (pat) == SEQUENCE)
	      {
		emit_insn_after (pat, insn);
		PUT_CODE (insn, NOTE);
		NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (insn) = 0;
	      }
	    else
	      PATTERN (insn) = pat;

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
	      {
		fixeddest = fixup_memory_subreg (fixeddest, insn, 0);
		promoted_mode = GET_MODE (fixeddest);
	      }
	    else
	      fixeddest = fixup_stack_1 (fixeddest, insn);

	    temp = gen_reg_rtx (promoted_mode);

	    emit_insn_after (gen_move_insn (fixeddest,
					    gen_lowpart (GET_MODE (fixeddest),
							 temp)),
			     insn);

	    SET_DEST (x) = temp;
	  }
      }

    default:
      break;
    }

  /* Nothing special about this RTX; fix its operands.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	fixup_var_refs_1 (var, promoted_mode, &XEXP (x, i), insn, replacements);
      else if (fmt[i] == 'E')
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
   This is used for subregs found inside REG_NOTES.  */

static rtx
fixup_memory_subreg (x, insn, uncritical)
     rtx x;
     rtx insn;
     int uncritical;
{
  int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
  rtx addr = XEXP (SUBREG_REG (x), 0);
  enum machine_mode mode = GET_MODE (x);
  rtx result;

  /* Paradoxical SUBREGs are usually invalid during RTL generation.  */
  if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))
      && ! uncritical)
    abort ();

  if (BYTES_BIG_ENDIAN)
    offset += (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	       - MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode)));
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

   If any insns must be emitted to compute NEWADDR, put them before INSN. 

   UNCRITICAL is as in fixup_memory_subreg.  */

static rtx
walk_fixup_memory_subreg (x, insn, uncritical)
     register rtx x;
     rtx insn;
     int uncritical;
{
  register enum rtx_code code;
  register const char *fmt;
  register int i;

  if (x == 0)
    return 0;

  code = GET_CODE (x);

  if (code == SUBREG && GET_CODE (SUBREG_REG (x)) == MEM)
    return fixup_memory_subreg (x, insn, uncritical);

  /* Nothing special about this RTX; fix its operands.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = walk_fixup_memory_subreg (XEXP (x, i), insn, uncritical);
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j)
	      = walk_fixup_memory_subreg (XVECEXP (x, i, j), insn, uncritical);
	}
    }
  return x;
}

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
  register const char *fmt;

  if (code == MEM)
    {
      register rtx ad = XEXP (x, 0);
      /* If we have address of a stack slot but it's not valid
	 (displacement is too large), compute the sum in a register.  */
      if (GET_CODE (ad) == PLUS
	  && GET_CODE (XEXP (ad, 0)) == REG
	  && ((REGNO (XEXP (ad, 0)) >= FIRST_VIRTUAL_REGISTER
	       && REGNO (XEXP (ad, 0)) <= LAST_VIRTUAL_REGISTER)
	      || REGNO (XEXP (ad, 0)) == FRAME_POINTER_REGNUM
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	      || REGNO (XEXP (ad, 0)) == HARD_FRAME_POINTER_REGNUM
#endif
	      || REGNO (XEXP (ad, 0)) == STACK_POINTER_REGNUM
	      || REGNO (XEXP (ad, 0)) == ARG_POINTER_REGNUM
	      || XEXP (ad, 0) == current_function_internal_arg_pointer)
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
      else if (fmt[i] == 'E')
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

	  HOST_WIDE_INT offset = INTVAL (XEXP (bitfield, 2));
	  rtx insns;

	  /* Adjust OFFSET to count bits from low-address byte.  */
	  if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
	    offset = (GET_MODE_BITSIZE (GET_MODE (XEXP (bitfield, 0)))
		      - offset - INTVAL (XEXP (bitfield, 1)));

	  /* Adjust OFFSET to count bytes from low-address byte.  */
	  offset /= BITS_PER_UNIT;
	  if (GET_CODE (XEXP (bitfield, 0)) == SUBREG)
	    {
	      offset += SUBREG_WORD (XEXP (bitfield, 0)) * UNITS_PER_WORD;
	      if (BYTES_BIG_ENDIAN)
		offset -= (MIN (UNITS_PER_WORD,
				GET_MODE_SIZE (GET_MODE (XEXP (bitfield, 0))))
			   - MIN (UNITS_PER_WORD,
				  GET_MODE_SIZE (GET_MODE (memref))));
	    }

	  start_sequence ();
	  memref = change_address (memref, mode,
				   plus_constant (XEXP (memref, 0), offset));
	  insns = get_insns ();
	  end_sequence ();
	  emit_insns_before (insns, insn);

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
		     && SUBREG_WORD (dest) == 0
		     && (GET_MODE_CLASS (GET_MODE (dest))
			 == GET_MODE_CLASS (GET_MODE (SUBREG_REG (dest))))
		     && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))
			 <= UNITS_PER_WORD))
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
static int cfa_offset;

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

/* On a few machines, the CFA coincides with the arg pointer.  */

#ifndef ARG_POINTER_CFA_OFFSET
#define ARG_POINTER_CFA_OFFSET 0
#endif


/* Build up a (MEM (ADDRESSOF (REG))) rtx for a register REG that just had
   its address taken.  DECL is the decl for the object stored in the
   register, for later use if we do need to force REG into the stack.
   REG is overwritten by the MEM like in put_reg_into_stack.  */

rtx
gen_mem_addressof (reg, decl)
     rtx reg;
     tree decl;
{
  tree type = TREE_TYPE (decl);
  rtx r = gen_rtx_ADDRESSOF (Pmode, gen_reg_rtx (GET_MODE (reg)),
			     REGNO (reg), decl);

  /* If the original REG was a user-variable, then so is the REG whose
     address is being taken.  Likewise for unchanging.  */
  REG_USERVAR_P (XEXP (r, 0)) = REG_USERVAR_P (reg);
  RTX_UNCHANGING_P (XEXP (r, 0)) = RTX_UNCHANGING_P (reg);

  PUT_CODE (reg, MEM);
  PUT_MODE (reg, DECL_MODE (decl));
  XEXP (reg, 0) = r;
  MEM_VOLATILE_P (reg) = TREE_SIDE_EFFECTS (decl);
  MEM_SET_IN_STRUCT_P (reg, AGGREGATE_TYPE_P (type));
  MEM_ALIAS_SET (reg) = get_alias_set (decl);

  if (TREE_USED (decl) || DECL_INITIAL (decl) != 0)
    fixup_var_refs (reg, GET_MODE (reg), TREE_UNSIGNED (type), 0);

  return reg;
}

/* If DECL has an RTL that is an ADDRESSOF rtx, put it into the stack.  */

#if 0
void
flush_addressof (decl)
     tree decl;
{
  if ((TREE_CODE (decl) == PARM_DECL || TREE_CODE (decl) == VAR_DECL)
      && DECL_RTL (decl) != 0
      && GET_CODE (DECL_RTL (decl)) == MEM
      && GET_CODE (XEXP (DECL_RTL (decl), 0)) == ADDRESSOF
      && GET_CODE (XEXP (XEXP (DECL_RTL (decl), 0), 0)) == REG)
    put_addressof_into_stack (XEXP (DECL_RTL (decl), 0), 0);
}
#endif

/* Force the register pointed to by R, an ADDRESSOF rtx, into the stack.  */

static void
put_addressof_into_stack (r, ht)
     rtx r;
     struct hash_table *ht;
{
  tree decl = ADDRESSOF_DECL (r);
  rtx reg = XEXP (r, 0);

  if (GET_CODE (reg) != REG)
    abort ();

  put_reg_into_stack (0, reg, TREE_TYPE (decl), GET_MODE (reg),
		      DECL_MODE (decl), TREE_SIDE_EFFECTS (decl),
		      ADDRESSOF_REGNO (r),
		      TREE_USED (decl) || DECL_INITIAL (decl) != 0, ht);
}

/* List of replacements made below in purge_addressof_1 when creating
   bitfield insertions.  */
static rtx purge_bitfield_addressof_replacements;

/* List of replacements made below in purge_addressof_1 for patterns
   (MEM (ADDRESSOF (REG ...))).  The key of the list entry is the
   corresponding (ADDRESSOF (REG ...)) and value is a substitution for
   the all pattern.  List PURGE_BITFIELD_ADDRESSOF_REPLACEMENTS is not
   enough in complex cases, e.g. when some field values can be
   extracted by usage MEM with narrower mode. */
static rtx purge_addressof_replacements;

/* Helper function for purge_addressof.  See if the rtx expression at *LOC
   in INSN needs to be changed.  If FORCE, always put any ADDRESSOFs into
   the stack.  If the function returns FALSE then the replacement could not
   be made.  */

static boolean
purge_addressof_1 (loc, insn, force, store, ht)
     rtx *loc;
     rtx insn;
     int force, store;
     struct hash_table *ht;
{
  rtx x;
  RTX_CODE code;
  int i, j;
  const char *fmt;
  boolean result = true;

  /* Re-start here to avoid recursion in common cases.  */
 restart:

  x = *loc;
  if (x == 0)
    return true;

  code = GET_CODE (x);

  /* If we don't return in any of the cases below, we will recurse inside
     the RTX, which will normally result in any ADDRESSOF being forced into
     memory.  */
  if (code == SET)
    {
      result = purge_addressof_1 (&SET_DEST (x), insn, force, 1, ht);
      result &= purge_addressof_1 (&SET_SRC (x), insn, force, 0, ht);
      return result;
    }

  else if (code == ADDRESSOF && GET_CODE (XEXP (x, 0)) == MEM)
    {
      /* We must create a copy of the rtx because it was created by
	 overwriting a REG rtx which is always shared.  */
      rtx sub = copy_rtx (XEXP (XEXP (x, 0), 0));
      rtx insns;

      if (validate_change (insn, loc, sub, 0)
	  || validate_replace_rtx (x, sub, insn))
	return true;
  
      start_sequence ();
      sub = force_operand (sub, NULL_RTX);
      if (! validate_change (insn, loc, sub, 0)
	  && ! validate_replace_rtx (x, sub, insn))
	abort ();

      insns = gen_sequence ();
      end_sequence ();
      emit_insn_before (insns, insn);
      return true;
    }

  else if (code == MEM && GET_CODE (XEXP (x, 0)) == ADDRESSOF && ! force)
    {
      rtx sub = XEXP (XEXP (x, 0), 0);
      rtx sub2;

      if (GET_CODE (sub) == MEM)
	{
	  sub2 = gen_rtx_MEM (GET_MODE (x), copy_rtx (XEXP (sub, 0)));
	  MEM_COPY_ATTRIBUTES (sub2, sub);
	  RTX_UNCHANGING_P (sub2) = RTX_UNCHANGING_P (sub);
	  sub = sub2;
	}
      else if (GET_CODE (sub) == REG
	       && (MEM_VOLATILE_P (x) || GET_MODE (x) == BLKmode))
	;
      else if (GET_CODE (sub) == REG && GET_MODE (x) != GET_MODE (sub))
	{
	  int size_x, size_sub;

	  if (!insn)
	    {
	      /* When processing REG_NOTES look at the list of
		 replacements done on the insn to find the register that X
		 was replaced by.  */
	      rtx tem;

	      for (tem = purge_bitfield_addressof_replacements;
		   tem != NULL_RTX;
		   tem = XEXP (XEXP (tem, 1), 1))
		if (rtx_equal_p (x, XEXP (tem, 0)))
		  {
		    *loc = XEXP (XEXP (tem, 1), 0);
		    return true;
		  }

	      /* See comment for purge_addressof_replacements. */
	      for (tem = purge_addressof_replacements;
		   tem != NULL_RTX;
		   tem = XEXP (XEXP (tem, 1), 1))
		if (rtx_equal_p (XEXP (x, 0), XEXP (tem, 0)))
		  {
		    rtx z = XEXP (XEXP (tem, 1), 0);

		    if (GET_MODE (x) == GET_MODE (z)
			|| (GET_CODE (XEXP (XEXP (tem, 1), 0)) != REG
			    && GET_CODE (XEXP (XEXP (tem, 1), 0)) != SUBREG))
		      abort ();

		    /* It can happen that the note may speak of things
		       in a wider (or just different) mode than the
		       code did.  This is especially true of
		       REG_RETVAL. */

		    if (GET_CODE (z) == SUBREG && SUBREG_WORD (z) == 0)
		      z = SUBREG_REG (z);
		    
		    if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD
			&& (GET_MODE_SIZE (GET_MODE (x))
			    > GET_MODE_SIZE (GET_MODE (z))))
		      {
			/* This can occur as a result in invalid
			   pointer casts, e.g. float f; ... 
			   *(long long int *)&f.
			   ??? We could emit a warning here, but
			   without a line number that wouldn't be
			   very helpful.  */
			z = gen_rtx_SUBREG (GET_MODE (x), z, 0);
		      }
		    else
		      z = gen_lowpart (GET_MODE (x), z);

		    *loc = z;
		    return true;
		  }

	      /* Sometimes we may not be able to find the replacement.  For
		 example when the original insn was a MEM in a wider mode,
		 and the note is part of a sign extension of a narrowed
		 version of that MEM.  Gcc testcase compile/990829-1.c can
		 generate an example of this siutation.  Rather than complain
		 we return false, which will prompt our caller to remove the
		 offending note.  */
	      return false;
	    }

	  size_x = GET_MODE_BITSIZE (GET_MODE (x));
	  size_sub = GET_MODE_BITSIZE (GET_MODE (sub));

	  /* Don't even consider working with paradoxical subregs,
	     or the moral equivalent seen here.  */
	  if (size_x <= size_sub
	      && int_mode_for_mode (GET_MODE (sub)) != BLKmode)
	    {
	      /* Do a bitfield insertion to mirror what would happen
		 in memory.  */

	      rtx val, seq;

	      if (store)
		{
		  rtx p = PREV_INSN (insn);

		  start_sequence ();
		  val = gen_reg_rtx (GET_MODE (x));
		  if (! validate_change (insn, loc, val, 0))
		    {
		      /* Discard the current sequence and put the
			 ADDRESSOF on stack.  */
		      end_sequence ();
		      goto give_up;
		    }
		  seq = gen_sequence ();
		  end_sequence ();
		  emit_insn_before (seq, insn);
		  compute_insns_for_mem (p ? NEXT_INSN (p) : get_insns (), 
					 insn, ht);
	      
		  start_sequence ();
		  store_bit_field (sub, size_x, 0, GET_MODE (x),
				   val, GET_MODE_SIZE (GET_MODE (sub)),
				   GET_MODE_SIZE (GET_MODE (sub)));

		  /* Make sure to unshare any shared rtl that store_bit_field
		     might have created.  */
		  unshare_all_rtl_again (get_insns ());

		  seq = gen_sequence ();
		  end_sequence ();
		  p = emit_insn_after (seq, insn);
		  if (NEXT_INSN (insn))
		    compute_insns_for_mem (NEXT_INSN (insn), 
					   p ? NEXT_INSN (p) : NULL_RTX,
					   ht);
		}
	      else
		{
		  rtx p = PREV_INSN (insn);

		  start_sequence ();
		  val = extract_bit_field (sub, size_x, 0, 1, NULL_RTX,
					   GET_MODE (x), GET_MODE (x),
					   GET_MODE_SIZE (GET_MODE (sub)),
					   GET_MODE_SIZE (GET_MODE (sub)));

		  if (! validate_change (insn, loc, val, 0))
		    {
		      /* Discard the current sequence and put the
			 ADDRESSOF on stack.  */
		      end_sequence ();
		      goto give_up;
		    }

		  seq = gen_sequence ();
		  end_sequence ();
		  emit_insn_before (seq, insn);
		  compute_insns_for_mem (p ? NEXT_INSN (p) : get_insns (),
					 insn, ht);
		}

	      /* Remember the replacement so that the same one can be done
		 on the REG_NOTES.  */
	      purge_bitfield_addressof_replacements
		= gen_rtx_EXPR_LIST (VOIDmode, x,
				     gen_rtx_EXPR_LIST
				     (VOIDmode, val,
				      purge_bitfield_addressof_replacements));

	      /* We replaced with a reg -- all done.  */
	      return true;
	    }
	}

      else if (validate_change (insn, loc, sub, 0))
	{
	  /* Remember the replacement so that the same one can be done
	     on the REG_NOTES.  */
	  if (GET_CODE (sub) == REG || GET_CODE (sub) == SUBREG)
	    {
	      rtx tem;

	      for (tem = purge_addressof_replacements;
		   tem != NULL_RTX;
		   tem = XEXP (XEXP (tem, 1), 1))
		if (rtx_equal_p (XEXP (x, 0), XEXP (tem, 0)))
		  {
		    XEXP (XEXP (tem, 1), 0) = sub;
		    return true;
		  }
	      purge_addressof_replacements
		= gen_rtx (EXPR_LIST, VOIDmode, XEXP (x, 0),
			   gen_rtx_EXPR_LIST (VOIDmode, sub,
					      purge_addressof_replacements));
	      return true;
	    }
	  goto restart;
	}
    give_up:;
      /* else give up and put it into the stack */
    }

  else if (code == ADDRESSOF)
    {
      put_addressof_into_stack (x, ht);
      return true;
    }
  else if (code == SET)
    {
      result = purge_addressof_1 (&SET_DEST (x), insn, force, 1, ht);
      result &= purge_addressof_1 (&SET_SRC (x), insn, force, 0, ht);
      return result;
    }

  /* Scan all subexpressions. */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	result &= purge_addressof_1 (&XEXP (x, i), insn, force, 0, ht);
      else if (*fmt == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  result &= purge_addressof_1 (&XVECEXP (x, i, j), insn, force, 0, ht);
    }

  return result;
}

/* Return a new hash table entry in HT.  */

static struct hash_entry *
insns_for_mem_newfunc (he, ht, k)
     struct hash_entry *he;
     struct hash_table *ht;
     hash_table_key k ATTRIBUTE_UNUSED;
{
  struct insns_for_mem_entry *ifmhe;
  if (he)
    return he;

  ifmhe = ((struct insns_for_mem_entry *)
	   hash_allocate (ht, sizeof (struct insns_for_mem_entry)));
  ifmhe->insns = NULL_RTX;

  return &ifmhe->he;
}

/* Return a hash value for K, a REG.  */

static unsigned long
insns_for_mem_hash (k)
     hash_table_key k;
{
  /* K is really a RTX.  Just use the address as the hash value.  */
  return (unsigned long) k;
}

/* Return non-zero if K1 and K2 (two REGs) are the same.  */

static boolean
insns_for_mem_comp (k1, k2)
     hash_table_key k1;
     hash_table_key k2;
{
  return k1 == k2;
}

struct insns_for_mem_walk_info {
  /* The hash table that we are using to record which INSNs use which
     MEMs.  */
  struct hash_table *ht;

  /* The INSN we are currently proessing.  */
  rtx insn;

  /* Zero if we are walking to find ADDRESSOFs, one if we are walking
     to find the insns that use the REGs in the ADDRESSOFs.  */
  int pass;
};

/* Called from compute_insns_for_mem via for_each_rtx.  If R is a REG
   that might be used in an ADDRESSOF expression, record this INSN in
   the hash table given by DATA (which is really a pointer to an
   insns_for_mem_walk_info structure).  */

static int
insns_for_mem_walk (r, data)
     rtx *r;
     void *data;
{
  struct insns_for_mem_walk_info *ifmwi 
    = (struct insns_for_mem_walk_info *) data;

  if (ifmwi->pass == 0 && *r && GET_CODE (*r) == ADDRESSOF
      && GET_CODE (XEXP (*r, 0)) == REG)
    hash_lookup (ifmwi->ht, XEXP (*r, 0), /*create=*/1, /*copy=*/0);
  else if (ifmwi->pass == 1 && *r && GET_CODE (*r) == REG)
    {
      /* Lookup this MEM in the hashtable, creating it if necessary.  */
      struct insns_for_mem_entry *ifme 
	= (struct insns_for_mem_entry *) hash_lookup (ifmwi->ht,
						      *r,
						      /*create=*/0,
						      /*copy=*/0);

      /* If we have not already recorded this INSN, do so now.  Since
	 we process the INSNs in order, we know that if we have
	 recorded it it must be at the front of the list.  */
      if (ifme && (!ifme->insns || XEXP (ifme->insns, 0) != ifmwi->insn))
	{
	  /* We do the allocation on the same obstack as is used for
	     the hash table since this memory will not be used once
	     the hash table is deallocated.  */
	  push_obstacks (&ifmwi->ht->memory, &ifmwi->ht->memory);
	  ifme->insns = gen_rtx_EXPR_LIST (VOIDmode, ifmwi->insn, 
					   ifme->insns);
	  pop_obstacks ();
	}
    }

  return 0;
}

/* Walk the INSNS, until we reach LAST_INSN, recording which INSNs use
   which REGs in HT.  */

static void
compute_insns_for_mem (insns, last_insn, ht)
     rtx insns;
     rtx last_insn;
     struct hash_table *ht;
{
  rtx insn;
  struct insns_for_mem_walk_info ifmwi;
  ifmwi.ht = ht;

  for (ifmwi.pass = 0; ifmwi.pass < 2; ++ifmwi.pass)
    for (insn = insns; insn != last_insn; insn = NEXT_INSN (insn))
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  ifmwi.insn = insn;
	  for_each_rtx (&insn, insns_for_mem_walk, &ifmwi);
	}
}

/* Helper function for purge_addressof called through for_each_rtx.
   Returns true iff the rtl is an ADDRESSOF.  */
static int
is_addressof (rtl, data)
     rtx * rtl;
     void * data ATTRIBUTE_UNUSED;
{
  return GET_CODE (* rtl) == ADDRESSOF;
}

/* Eliminate all occurrences of ADDRESSOF from INSNS.  Elide any remaining
   (MEM (ADDRESSOF)) patterns, and force any needed registers into the
   stack.  */

void
purge_addressof (insns)
     rtx insns;
{
  rtx insn;
  struct hash_table ht;
  
  /* When we actually purge ADDRESSOFs, we turn REGs into MEMs.  That
     requires a fixup pass over the instruction stream to correct
     INSNs that depended on the REG being a REG, and not a MEM.  But,
     these fixup passes are slow.  Furthermore, more MEMs are not
     mentioned in very many instructions.  So, we speed up the process
     by pre-calculating which REGs occur in which INSNs; that allows
     us to perform the fixup passes much more quickly.  */
  hash_table_init (&ht, 
		   insns_for_mem_newfunc,
		   insns_for_mem_hash,
		   insns_for_mem_comp);
  compute_insns_for_mem (insns, NULL_RTX, &ht);

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	|| GET_CODE (insn) == CALL_INSN)
      {
	if (! purge_addressof_1 (&PATTERN (insn), insn,
				 asm_noperands (PATTERN (insn)) > 0, 0, &ht))
	  /* If we could not replace the ADDRESSOFs in the insn,
	     something is wrong.  */
	  abort ();
	
	if (! purge_addressof_1 (&REG_NOTES (insn), NULL_RTX, 0, 0, &ht))
	  {
	    /* If we could not replace the ADDRESSOFs in the insn's notes,
	       we can just remove the offending notes instead.  */
	    rtx note;

	    for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	      {
		/* If we find a REG_RETVAL note then the insn is a libcall.
		   Such insns must have REG_EQUAL notes as well, in order
		   for later passes of the compiler to work.  So it is not
		   safe to delete the notes here, and instead we abort.  */
		if (REG_NOTE_KIND (note) == REG_RETVAL)
		  abort ();
		if (for_each_rtx (& note, is_addressof, NULL))
		  remove_note (insn, note);
	      }
	  }
      }

  /* Clean up.  */
  hash_table_free (&ht);
  purge_bitfield_addressof_replacements = 0;
  purge_addressof_replacements = 0;
}

/* Pass through the INSNS of function FNDECL and convert virtual register
   references to hard register references.  */

void
instantiate_virtual_regs (fndecl, insns)
     tree fndecl;
     rtx insns;
{
  rtx insn;
  int i;

  /* Compute the offsets to use for this function.  */
  in_arg_offset = FIRST_PARM_OFFSET (fndecl);
  var_offset = STARTING_FRAME_OFFSET;
  dynamic_offset = STACK_DYNAMIC_OFFSET (fndecl);
  out_arg_offset = STACK_POINTER_OFFSET;
  cfa_offset = ARG_POINTER_CFA_OFFSET;

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

  /* Instantiate the stack slots for the parm registers, for later use in
     addressof elimination.  */
  for (i = 0; i < max_parm_reg; ++i)
    if (parm_reg_stack_loc[i])
      instantiate_virtual_regs_1 (&parm_reg_stack_loc[i], NULL_RTX, 0);

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

  if (DECL_SAVED_INSNS (fndecl))
    /* When compiling an inline function, the obstack used for
       rtl allocation is the maybepermanent_obstack.  Calling
       `resume_temporary_allocation' switches us back to that
       obstack while we process this function's parameters.  */
    resume_temporary_allocation ();

  /* Process all parameters of the function.  */
  for (decl = DECL_ARGUMENTS (fndecl); decl; decl = TREE_CHAIN (decl))
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (decl));

      instantiate_decl (DECL_RTL (decl), size, valid_only);	

      /* If the parameter was promoted, then the incoming RTL mode may be
	 larger than the declared type size.  We must use the larger of
	 the two sizes.  */
      size = MAX (GET_MODE_SIZE (GET_MODE (DECL_INCOMING_RTL (decl))), size);
      instantiate_decl (DECL_INCOMING_RTL (decl), size, valid_only);
    }

  /* Now process all variables defined in the function or its subblocks.  */
  instantiate_decls_1 (DECL_INITIAL (fndecl), valid_only);

  if (DECL_INLINE (fndecl) || DECL_DEFER_OUTPUT (fndecl))
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

/* Subroutine of the preceding procedures: Given RTL representing a
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
      || (GET_CODE (addr) == ADDRESSOF && GET_CODE (XEXP (addr, 0)) == REG)
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

  if (valid_only)
    {
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
    }

  /* Put back the address now that we have updated it and we either know
     it is valid or we don't care whether it is valid.  */

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
  HOST_WIDE_INT offset = 0;
  rtx temp;
  rtx seq;
  int i, j;
  const char *fmt;

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
	 the actual register should receive the source minus the
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
      else if (SET_DEST (x) == virtual_cfa_rtx)
	new = arg_pointer_rtx, offset = - cfa_offset;

      if (new)
	{
	  rtx src = SET_SRC (x);

	  instantiate_virtual_regs_1 (&src, NULL_RTX, 0);

	  /* The only valid sources here are PLUS or REG.  Just do
	     the simplest possible thing to handle them.  */
	  if (GET_CODE (src) != REG && GET_CODE (src) != PLUS)
	    abort ();

	  start_sequence ();
	  if (GET_CODE (src) != REG)
	    temp = force_operand (src, NULL_RTX);
	  else
	    temp = src;
	  temp = force_operand (plus_constant (temp, offset), NULL_RTX);
	  seq = get_insns ();
	  end_sequence ();

	  emit_insns_before (seq, object);
	  SET_DEST (x) = new;

	  if (! validate_change (object, &SET_SRC (x), temp, 0)
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
	  rtx old, new_offset;

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
	      else if (inner == virtual_cfa_rtx)
	        new = arg_pointer_rtx, offset = cfa_offset;
	      else
		{
		  loc = &XEXP (x, 0);
		  goto restart;
		}

	      instantiate_virtual_regs_1 (&XEXP (XEXP (x, 0), 1), object,
					  extra_insns);
	      new = gen_rtx_PLUS (Pmode, new, XEXP (XEXP (x, 0), 1));
	    }

	  else if (XEXP (x, 0) == virtual_incoming_args_rtx)
	    new = arg_pointer_rtx, offset = in_arg_offset;
	  else if (XEXP (x, 0) == virtual_stack_vars_rtx)
	    new = frame_pointer_rtx, offset = var_offset;
	  else if (XEXP (x, 0) == virtual_stack_dynamic_rtx)
	    new = stack_pointer_rtx, offset = dynamic_offset;
	  else if (XEXP (x, 0) == virtual_outgoing_args_rtx)
	    new = stack_pointer_rtx, offset = out_arg_offset;
          else if (XEXP (x, 0) == virtual_cfa_rtx)
            new = arg_pointer_rtx, offset = cfa_offset;
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

	  new_offset = plus_constant (XEXP (x, 1), offset);

	  /* If the new constant is zero, try to replace the sum with just
	     the register.  */
	  if (new_offset == const0_rtx
	      && validate_change (object, loc, new, 0))
	    return 1;

	  /* Next try to replace the register and new offset.
	     There are two changes to validate here and we can't assume that
	     in the case of old offset equals new just changing the register
	     will yield a valid insn.  In the interests of a little efficiency,
	     however, we only call validate change once (we don't queue up the
	     changes and then call apply_change_group).  */

	  old = XEXP (x, 0);
	  if (offset == 0
	      ? ! validate_change (object, &XEXP (x, 0), new, 0)
	      : (XEXP (x, 0) = new,
		 ! validate_change (object, &XEXP (x, 1), new_offset, 0)))
	    {
	      if (! extra_insns)
		{
		  XEXP (x, 0) = old;
		  return 0;
		}

	      /* Otherwise copy the new constant into a register and replace
		 constant with that register.  */
	      temp = gen_reg_rtx (Pmode);
	      XEXP (x, 0) = new;
	      if (validate_change (object, &XEXP (x, 1), temp, 0))
		emit_insn_before (gen_move_insn (temp, new_offset), object);
	      else
		{
		  /* If that didn't work, replace this expression with a
		     register containing the sum.  */

		  XEXP (x, 0) = old;
		  new = gen_rtx_PLUS (Pmode, new, new_offset);

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
    case ROTATERT: case ROTATE:
    case ASHIFTRT: case LSHIFTRT: case ASHIFT:
    case NE:       case EQ:
    case GE:       case GT:       case GEU:    case GTU:
    case LE:       case LT:       case LEU:    case LTU:
      if (XEXP (x, 1) && ! CONSTANT_P (XEXP (x, 1)))
	instantiate_virtual_regs_1 (&XEXP (x, 1), object, extra_insns);
      loc = &XEXP (x, 0);
      goto restart;

    case MEM:
      /* Most cases of MEM that convert to valid addresses have already been
	 handled by our scan of decls.  The only special handling we
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
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	  || temp == hard_frame_pointer_rtx
#endif
	  || temp == frame_pointer_rtx)
	return 1;

      if (GET_CODE (temp) == PLUS
	  && CONSTANT_ADDRESS_P (XEXP (temp, 1))
	  && (XEXP (temp, 0) == frame_pointer_rtx
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	      || XEXP (temp, 0) == hard_frame_pointer_rtx
#endif
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
	     doesn't seem very likely, though.  One case where this could
	     happen is in the case of a USE or CLOBBER reference, but we
	     take care of that below.  */

	  if (instantiate_virtual_regs_1 (&XEXP (x, 0),
					  object ? object : x, 0))
	    return 1;

	  /* Otherwise make a copy and process that copy.  We copy the entire
	     RTL expression since it might be a PLUS which could also be
	     shared.  */
	  *loc = x = copy_rtx (x);
	}

      /* Fall through to generic unary operation case.  */
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

    case USE:
    case CLOBBER:
      /* If the operand is a MEM, see if the change is a valid MEM.  If not,
	 go ahead and make the invalid one, but do it to a copy.  For a REG,
	 just make the recursive call, since there's no chance of a problem. */

      if ((GET_CODE (XEXP (x, 0)) == MEM
	   && instantiate_virtual_regs_1 (&XEXP (XEXP (x, 0), 0), XEXP (x, 0),
					  0))
	  || (GET_CODE (XEXP (x, 0)) == REG
	      && instantiate_virtual_regs_1 (&XEXP (x, 0), object, 0)))
	return 1;

      XEXP (x, 0) = copy_rtx (XEXP (x, 0));
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
      else if (x == virtual_cfa_rtx)
        new = arg_pointer_rtx, offset = cfa_offset;

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

    case ADDRESSOF:
      if (GET_CODE (XEXP (x, 0)) == REG)
	return 1;

      else if (GET_CODE (XEXP (x, 0)) == MEM)
	{
	  /* If we have a (addressof (mem ..)), do any instantiation inside
	     since we know we'll be making the inside valid when we finally
	     remove the ADDRESSOF.  */
	  instantiate_virtual_regs_1 (&XEXP (XEXP (x, 0), 0), NULL_RTX, 0);
	  return 1;
	}
      break;
      
    default:
      break;
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
	{
	  tree t, last_t;

	  LABEL_PRESERVE_P (insn) = 0;

	  /* Remove it from the nonlocal_label list, to avoid confusing
	     flow.  */
	  for (t = nonlocal_labels, last_t = 0; t;
	       last_t = t, t = TREE_CHAIN (t))
	    if (DECL_RTL (TREE_VALUE (t)) == insn)
	      break;
	  if (t)
	    {
	      if (! last_t)
		nonlocal_labels = TREE_CHAIN (nonlocal_labels);
	      else
		TREE_CHAIN (last_t) = TREE_CHAIN (t);
	    }
	}
      if (GET_CODE (insn) == INSN)
	{
	  int can_delete = 0;
	  rtx t;
	  for (t = nonlocal_goto_handler_slots; t != 0; t = XEXP (t, 1))
	    if (reg_mentioned_p (t, PATTERN (insn)))
	      {
		can_delete = 1;
		break;
	      }
	  if (can_delete
	      || (nonlocal_goto_stack_level != 0
		  && reg_mentioned_p (nonlocal_goto_stack_level,
				      PATTERN (insn))))
	    delete_insn (insn);
	}
    }
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

/* Return 1 if EXP is an aggregate type (or a value with aggregate type).
   This means a type for which function calls must pass an address to the
   function or get an address back from the function.
   EXP may be a type node or an expression (whose type is tested).  */

int
aggregate_value_p (exp)
     tree exp;
{
  int i, regno, nregs;
  rtx reg;
  tree type;
  if (TREE_CODE_CLASS (TREE_CODE (exp)) == 't')
    type = exp;
  else
    type = TREE_TYPE (exp);

  if (RETURN_IN_MEMORY (type))
    return 1;
  /* Types that are TREE_ADDRESSABLE must be constructed in memory,
     and thus can't be returned in registers.  */
  if (TREE_ADDRESSABLE (type))
    return 1;
  if (flag_pcc_struct_return && AGGREGATE_TYPE_P (type))
    return 1;
  /* Make sure we have suitable call-clobbered regs to return
     the value in; if not, we must return it in memory.  */
  reg = hard_function_value (type, 0, 0);

  /* If we have something other than a REG (e.g. a PARALLEL), then assume
     it is OK.  */
  if (GET_CODE (reg) != REG)
    return 0;

  regno = REGNO (reg);
  nregs = HARD_REGNO_NREGS (regno, TYPE_MODE (type));
  for (i = 0; i < nregs; i++)
    if (! call_used_regs[regno + i])
      return 1;
  return 0;
}

/* Assign RTL expressions to the function's parameters.
   This may involve copying them into registers and using
   those registers as the RTL for them.  */

void
assign_parms (fndecl)
     tree fndecl;
{
  register tree parm;
  register rtx entry_parm = 0;
  register rtx stack_parm = 0;
  CUMULATIVE_ARGS args_so_far;
  enum machine_mode promoted_mode, passed_mode;
  enum machine_mode nominal_mode, promoted_nominal_mode;
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
#ifdef SETUP_INCOMING_VARARGS
  int varargs_setup = 0;
#endif
  rtx conversion_insns = 0;
  struct args_size alignment_pad;

  /* Nonzero if the last arg is named `__builtin_va_alist',
     which is used on some machines for old-fashioned non-ANSI varargs.h;
     this should be stuck onto the stack as if it had arrived there.  */
  int hide_last_arg
    = (current_function_varargs
       && fnargs
       && (parm = tree_last (fnargs)) != 0
       && DECL_NAME (parm)
       && (! strcmp (IDENTIFIER_POINTER (DECL_NAME (parm)),
		     "__builtin_va_alist")));

  /* Nonzero if function takes extra anonymous args.
     This means the last named arg must be on the stack
     right before the anonymous ones.  */
  int stdarg
    = (TYPE_ARG_TYPES (fntype) != 0
       && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	   != void_type_node));

  current_function_stdarg = stdarg;

  /* If the reg that the virtual arg pointer will be translated into is
     not a fixed reg or is the stack pointer, make a copy of the virtual
     arg pointer, and address parms via the copy.  The frame pointer is
     considered fixed even though it is not marked as such.

     The second time through, simply use ap to avoid generating rtx.  */

  if ((ARG_POINTER_REGNUM == STACK_POINTER_REGNUM
       || ! (fixed_regs[ARG_POINTER_REGNUM]
	     || ARG_POINTER_REGNUM == FRAME_POINTER_REGNUM)))
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
      tree type = build_pointer_type (TREE_TYPE (fntype));

      function_result_decl = build_decl (PARM_DECL, NULL_TREE, type);

      DECL_ARG_TYPE (function_result_decl) = type;
      TREE_CHAIN (function_result_decl) = fnargs;
      fnargs = function_result_decl;
    }
			       
  max_parm_reg = LAST_VIRTUAL_REGISTER + 1;
  parm_reg_stack_loc = (rtx *) xcalloc (max_parm_reg, sizeof (rtx));

#ifdef INIT_CUMULATIVE_INCOMING_ARGS
  INIT_CUMULATIVE_INCOMING_ARGS (args_so_far, fntype, NULL_RTX);
#else
  INIT_CUMULATIVE_ARGS (args_so_far, fntype, NULL_RTX, 0);
#endif

  /* We haven't yet found an argument that we must push and pretend the
     caller did.  */
  current_function_pretend_args_size = 0;

  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    {
      int aggregate = AGGREGATE_TYPE_P (TREE_TYPE (parm));
      struct args_size stack_offset;
      struct args_size arg_size;
      int passed_pointer = 0;
      int did_conversion = 0;
      tree passed_type = DECL_ARG_TYPE (parm);
      tree nominal_type = TREE_TYPE (parm);
      int pretend_named;

      /* Set LAST_NAMED if this is last named arg before some
	 anonymous args.  */
      int last_named = ((TREE_CHAIN (parm) == 0
			 || DECL_NAME (TREE_CHAIN (parm)) == 0)
			&& (stdarg || current_function_varargs));
      /* Set NAMED_ARG if this arg should be treated as a named arg.  For
	 most machines, if this is a varargs/stdarg function, then we treat
	 the last named arg as if it were anonymous too.  */
      int named_arg = STRICT_ARGUMENT_NAMING ? 1 : ! last_named;

      if (TREE_TYPE (parm) == error_mark_node
	  /* This can happen after weird syntax errors
	     or if an enum type is defined among the parms.  */
	  || TREE_CODE (parm) != PARM_DECL
	  || passed_type == NULL)
	{
	  DECL_INCOMING_RTL (parm) = DECL_RTL (parm)
	    = gen_rtx_MEM (BLKmode, const0_rtx);
	  TREE_USED (parm) = 1;
	  continue;
	}

      /* For varargs.h function, save info about regs and stack space
	 used by the individual args, not including the va_alist arg.  */
      if (hide_last_arg && last_named)
	current_function_args_info = args_so_far;

      /* Find mode of arg as it is passed, and mode of arg
	 as it should be during execution of this function.  */
      passed_mode = TYPE_MODE (passed_type);
      nominal_mode = TYPE_MODE (nominal_type);

      /* If the parm's mode is VOID, its value doesn't matter,
	 and avoid the usual things like emit_move_insn that could crash.  */
      if (nominal_mode == VOIDmode)
	{
	  DECL_INCOMING_RTL (parm) = DECL_RTL (parm) = const0_rtx;
	  continue;
	}

      /* If the parm is to be passed as a transparent union, use the
	 type of the first field for the tests below.  We have already
	 verified that the modes are the same.  */
      if (DECL_TRANSPARENT_UNION (parm)
	  || TYPE_TRANSPARENT_UNION (passed_type))
	passed_type = TREE_TYPE (TYPE_FIELDS (passed_type));

      /* See if this arg was passed by invisible reference.  It is if
	 it is an object whose size depends on the contents of the
	 object itself or if the machine requires these objects be passed
	 that way.  */

      if ((TREE_CODE (TYPE_SIZE (passed_type)) != INTEGER_CST
	   && contains_placeholder_p (TYPE_SIZE (passed_type)))
	  || TREE_ADDRESSABLE (passed_type)
#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
	  || FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far, passed_mode,
					      passed_type, named_arg)
#endif
	  )
	{
	  passed_type = nominal_type = build_pointer_type (passed_type);
	  passed_pointer = 1;
	  passed_mode = nominal_mode = Pmode;
	}

      promoted_mode = passed_mode;

#ifdef PROMOTE_FUNCTION_ARGS
      /* Compute the mode in which the arg is actually extended to.  */
      unsignedp = TREE_UNSIGNED (passed_type);
      promoted_mode = promote_mode (passed_type, promoted_mode, &unsignedp, 1);
#endif

      /* Let machine desc say which reg (if any) the parm arrives in.
	 0 means it arrives on the stack.  */
#ifdef FUNCTION_INCOMING_ARG
      entry_parm = FUNCTION_INCOMING_ARG (args_so_far, promoted_mode,
					  passed_type, named_arg);
#else
      entry_parm = FUNCTION_ARG (args_so_far, promoted_mode,
				 passed_type, named_arg);
#endif

      if (entry_parm == 0)
	promoted_mode = passed_mode;

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
	  SETUP_INCOMING_VARARGS (args_so_far, promoted_mode, passed_type,
				  current_function_pretend_args_size, 0);
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

      pretend_named = named_arg || PRETEND_OUTGOING_VARARGS_NAMED;
      locate_and_pad_parm (promoted_mode, passed_type,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			   1,
#else
#ifdef FUNCTION_INCOMING_ARG
			   FUNCTION_INCOMING_ARG (args_so_far, promoted_mode,
						  passed_type,
						  pretend_named) != 0,
#else
			   FUNCTION_ARG (args_so_far, promoted_mode,
					 passed_type,
					 pretend_named) != 0,
#endif
#endif
			   fndecl, &stack_args_size, &stack_offset, &arg_size,
                           &alignment_pad);

      {
	rtx offset_rtx = ARGS_SIZE_RTX (stack_offset);

	if (offset_rtx == const0_rtx)
	  stack_parm = gen_rtx_MEM (promoted_mode, internal_arg_pointer);
	else
	  stack_parm = gen_rtx_MEM (promoted_mode,
				    gen_rtx_PLUS (Pmode,
						  internal_arg_pointer,
						  offset_rtx));

	/* If this is a memory ref that contains aggregate components,
	   mark it as such for cse and loop optimize.  Likewise if it
	   is readonly.  */
	MEM_SET_IN_STRUCT_P (stack_parm, aggregate);
	RTX_UNCHANGING_P (stack_parm) = TREE_READONLY (parm);
	MEM_ALIAS_SET (stack_parm) = get_alias_set (parm);
      }

      /* If this parameter was passed both in registers and in the stack,
	 use the copy on the stack.  */
      if (MUST_PASS_IN_STACK (promoted_mode, passed_type))
	entry_parm = 0;

#ifdef FUNCTION_ARG_PARTIAL_NREGS
      /* If this parm was passed part in regs and part in memory,
	 pretend it arrived entirely in memory
	 by pushing the register-part onto the stack.

	 In the special case of a DImode or DFmode that is split,
	 we could put it together in a pseudoreg directly,
	 but for now that's not worth bothering with.  */

      if (entry_parm)
	{
	  int nregs = FUNCTION_ARG_PARTIAL_NREGS (args_so_far, promoted_mode,
						  passed_type, named_arg);

	  if (nregs > 0)
	    {
	      current_function_pretend_args_size
		= (((nregs * UNITS_PER_WORD) + (PARM_BOUNDARY / BITS_PER_UNIT) - 1)
		   / (PARM_BOUNDARY / BITS_PER_UNIT)
		   * (PARM_BOUNDARY / BITS_PER_UNIT));

	      /* Handle calls that pass values in multiple non-contiguous
		 locations.  The Irix 6 ABI has examples of this.  */
	      if (GET_CODE (entry_parm) == PARALLEL)
		emit_group_store (validize_mem (stack_parm), entry_parm,
				  int_size_in_bytes (TREE_TYPE (parm)),
				  (TYPE_ALIGN (TREE_TYPE (parm))
				   / BITS_PER_UNIT));
	      else
		move_block_from_reg (REGNO (entry_parm),
				     validize_mem (stack_parm), nregs,
				     int_size_in_bytes (TREE_TYPE (parm)));

	      entry_parm = stack_parm;
	    }
	}
#endif

      /* If we didn't decide this parm came in a register,
	 by default it came on the stack.  */
      if (entry_parm == 0)
	entry_parm = stack_parm;

      /* Record permanently how this parm was passed.  */
      DECL_INCOMING_RTL (parm) = entry_parm;

      /* If there is actually space on the stack for this parm,
	 count it in stack_args_size; otherwise set stack_parm to 0
	 to indicate there is no preallocated stack slot for the parm.  */

      if (entry_parm == stack_parm
          || (GET_CODE (entry_parm) == PARALLEL
              && XEXP (XVECEXP (entry_parm, 0, 0), 0) == NULL_RTX)
#if defined (REG_PARM_STACK_SPACE) && ! defined (MAYBE_REG_PARM_STACK_SPACE)
	  /* On some machines, even if a parm value arrives in a register
	     there is still an (uninitialized) stack slot allocated for it.

	     ??? When MAYBE_REG_PARM_STACK_SPACE is defined, we can't tell
	     whether this parameter already has a stack slot allocated,
	     because an arg block exists only if current_function_args_size
	     is larger than some threshold, and we haven't calculated that
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

      FUNCTION_ARG_ADVANCE (args_so_far, promoted_mode,
			    passed_type, named_arg);

      /* If we can't trust the parm stack slot to be aligned enough
	 for its ultimate type, don't use that slot after entry.
	 We'll make another stack slot, if we need one.  */
      {
	int thisparm_boundary
	  = FUNCTION_ARG_BOUNDARY (promoted_mode, passed_type);

	if (GET_MODE_ALIGNMENT (nominal_mode) > thisparm_boundary)
	  stack_parm = 0;
      }

      /* If parm was passed in memory, and we need to convert it on entry,
	 don't store it back in that same slot.  */
      if (entry_parm != 0
	  && nominal_mode != BLKmode && nominal_mode != passed_mode)
	stack_parm = 0;

#if 0
      /* Now adjust STACK_PARM to the mode and precise location
	 where this parameter should live during execution,
	 if we discover that it must live in the stack during execution.
	 To make debuggers happier on big-endian machines, we store
	 the value in the last bytes of the space available.  */

      if (nominal_mode != BLKmode && nominal_mode != passed_mode
	  && stack_parm != 0)
	{
	  rtx offset_rtx;

	  if (BYTES_BIG_ENDIAN
	      && GET_MODE_SIZE (nominal_mode) < UNITS_PER_WORD)
	    stack_offset.constant += (GET_MODE_SIZE (passed_mode)
				      - GET_MODE_SIZE (nominal_mode));

	  offset_rtx = ARGS_SIZE_RTX (stack_offset);
	  if (offset_rtx == const0_rtx)
	    stack_parm = gen_rtx_MEM (nominal_mode, internal_arg_pointer);
	  else
	    stack_parm = gen_rtx_MEM (nominal_mode,
				      gen_rtx_PLUS (Pmode,
						    internal_arg_pointer,
						    offset_rtx));

	  /* If this is a memory ref that contains aggregate components,
	     mark it as such for cse and loop optimize.  */
	  MEM_SET_IN_STRUCT_P (stack_parm, aggregate);
	}
#endif /* 0 */

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

      if (nominal_mode == BLKmode || GET_CODE (entry_parm) == PARALLEL)
	{
	  /* If a BLKmode arrives in registers, copy it to a stack slot.
	     Handle calls that pass values in multiple non-contiguous
	     locations.  The Irix 6 ABI has examples of this.  */
	  if (GET_CODE (entry_parm) == REG
	      || GET_CODE (entry_parm) == PARALLEL)
	    {
	      int size_stored
		= CEIL_ROUND (int_size_in_bytes (TREE_TYPE (parm)),
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
		    = assign_stack_local (GET_MODE (entry_parm),
					  size_stored, 0);

		  /* If this is a memory ref that contains aggregate
		     components, mark it as such for cse and loop optimize.  */
		  MEM_SET_IN_STRUCT_P (stack_parm, aggregate);
		}

	      else if (PARM_BOUNDARY % BITS_PER_WORD != 0)
		abort ();

	      if (TREE_READONLY (parm))
		RTX_UNCHANGING_P (stack_parm) = 1;

	      /* Handle calls that pass values in multiple non-contiguous
		 locations.  The Irix 6 ABI has examples of this.  */
	      if (GET_CODE (entry_parm) == PARALLEL)
		emit_group_store (validize_mem (stack_parm), entry_parm,
				  int_size_in_bytes (TREE_TYPE (parm)),
				  (TYPE_ALIGN (TREE_TYPE (parm))
				   / BITS_PER_UNIT));
	      else
		move_block_from_reg (REGNO (entry_parm),
				     validize_mem (stack_parm),
				     size_stored / UNITS_PER_WORD,
				     int_size_in_bytes (TREE_TYPE (parm)));
	    }
	  DECL_RTL (parm) = stack_parm;
	}
      else if (! ((! optimize
		   && ! DECL_REGISTER (parm)
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
	  int regno, regnoi = 0, regnor = 0;

	  unsignedp = TREE_UNSIGNED (TREE_TYPE (parm));

	  promoted_nominal_mode
	    = promote_mode (TREE_TYPE (parm), nominal_mode, &unsignedp, 0);

	  parmreg = gen_reg_rtx (promoted_nominal_mode);
	  mark_user_reg (parmreg);

	  /* If this was an item that we received a pointer to, set DECL_RTL
	     appropriately.  */
	  if (passed_pointer)
	    {
	      DECL_RTL (parm)
		= gen_rtx_MEM (TYPE_MODE (TREE_TYPE (passed_type)), parmreg);
	      MEM_SET_IN_STRUCT_P (DECL_RTL (parm), aggregate);
	    }
	  else
	    DECL_RTL (parm) = parmreg;

	  /* Copy the value into the register.  */
	  if (nominal_mode != passed_mode
	      || promoted_nominal_mode != promoted_mode)
	    {
	      int save_tree_used;
	      /* ENTRY_PARM has been converted to PROMOTED_MODE, its
		 mode, by the caller.  We now have to convert it to 
		 NOMINAL_MODE, if different.  However, PARMREG may be in
		 a different mode than NOMINAL_MODE if it is being stored
		 promoted.

		 If ENTRY_PARM is a hard register, it might be in a register
		 not valid for operating in its mode (e.g., an odd-numbered
		 register for a DFmode).  In that case, moves are the only
		 thing valid, so we can't do a convert from there.  This
		 occurs when the calling sequence allow such misaligned
		 usages.

		 In addition, the conversion may involve a call, which could
		 clobber parameters which haven't been copied to pseudo
		 registers yet.  Therefore, we must first copy the parm to
		 a pseudo reg here, and save the conversion until after all
		 parameters have been moved.  */

	      rtx tempreg = gen_reg_rtx (GET_MODE (entry_parm));

	      emit_move_insn (tempreg, validize_mem (entry_parm));

	      push_to_sequence (conversion_insns);
	      tempreg = convert_to_mode (nominal_mode, tempreg, unsignedp);

	      /* TREE_USED gets set erroneously during expand_assignment.  */
	      save_tree_used = TREE_USED (parm);
	      expand_assignment (parm,
				 make_tree (nominal_type, tempreg), 0, 0);
	      TREE_USED (parm) = save_tree_used;
	      conversion_insns = get_insns ();
	      did_conversion = 1;
	      end_sequence ();
	    }
	  else
	    emit_move_insn (parmreg, validize_mem (entry_parm));

	  /* If we were passed a pointer but the actual value
	     can safely live in a register, put it in one.  */
	  if (passed_pointer && TYPE_MODE (TREE_TYPE (parm)) != BLKmode
	      && ! ((! optimize
		     && ! DECL_REGISTER (parm)
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
	      mark_user_reg (parmreg);
	      emit_move_insn (parmreg, DECL_RTL (parm));
	      DECL_RTL (parm) = parmreg;
	      /* STACK_PARM is the pointer, not the parm, and PARMREG is
		 now the parm.  */
	      stack_parm = 0;
	    }
#ifdef FUNCTION_ARG_CALLEE_COPIES
	  /* If we are passed an arg by reference and it is our responsibility
	     to make a copy, do it now.
	     PASSED_TYPE and PASSED mode now refer to the pointer, not the
	     original argument, so we must recreate them in the call to
	     FUNCTION_ARG_CALLEE_COPIES.  */
	  /* ??? Later add code to handle the case that if the argument isn't
	     modified, don't do the copy.  */

	  else if (passed_pointer
		   && FUNCTION_ARG_CALLEE_COPIES (args_so_far,
						  TYPE_MODE (DECL_ARG_TYPE (parm)),
						  DECL_ARG_TYPE (parm),
						  named_arg)
		   && ! TREE_ADDRESSABLE (DECL_ARG_TYPE (parm)))
	    {
	      rtx copy;
	      tree type = DECL_ARG_TYPE (parm);

	      /* This sequence may involve a library call perhaps clobbering
		 registers that haven't been copied to pseudos yet.  */

	      push_to_sequence (conversion_insns);

	      if (TYPE_SIZE (type) == 0
		  || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
		/* This is a variable sized object.  */
		copy = gen_rtx_MEM (BLKmode,
				    allocate_dynamic_stack_space
				    (expr_size (parm), NULL_RTX,
				     TYPE_ALIGN (type)));
	      else
		copy = assign_stack_temp (TYPE_MODE (type),
					  int_size_in_bytes (type), 1);
	      MEM_SET_IN_STRUCT_P (copy, AGGREGATE_TYPE_P (type));
	      RTX_UNCHANGING_P (copy) = TREE_READONLY (parm);

	      store_expr (parm, copy, 0);
	      emit_move_insn (parmreg, XEXP (copy, 0));
	      if (current_function_check_memory_usage)
		emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
				   XEXP (copy, 0), Pmode,
				   GEN_INT (int_size_in_bytes (type)),
				   TYPE_MODE (sizetype),
				   GEN_INT (MEMORY_USE_RW),
				   TYPE_MODE (integer_type_node));
	      conversion_insns = get_insns ();
	      did_conversion = 1;
	      end_sequence ();
	    }
#endif /* FUNCTION_ARG_CALLEE_COPIES */

	  /* In any case, record the parm's desired stack location
	     in case we later discover it must live in the stack. 

	     If it is a COMPLEX value, store the stack location for both
	     halves.  */

	  if (GET_CODE (parmreg) == CONCAT)
	    regno = MAX (REGNO (XEXP (parmreg, 0)), REGNO (XEXP (parmreg, 1)));
	  else
	    regno = REGNO (parmreg);

	  if (regno >= max_parm_reg)
	    {
	      rtx *new;
	      int old_max_parm_reg = max_parm_reg;

	      /* It's slow to expand this one register at a time,
		 but it's also rare and we need max_parm_reg to be
		 precisely correct.  */
	      max_parm_reg = regno + 1;
	      new = (rtx *) xrealloc (parm_reg_stack_loc,
				      max_parm_reg * sizeof (rtx));
	      bzero ((char *) (new + old_max_parm_reg),
		     (max_parm_reg - old_max_parm_reg) * sizeof (rtx));
	      parm_reg_stack_loc = new;
	    }

	  if (GET_CODE (parmreg) == CONCAT)
	    {
	      enum machine_mode submode = GET_MODE (XEXP (parmreg, 0));

	      regnor = REGNO (gen_realpart (submode, parmreg));
	      regnoi = REGNO (gen_imagpart (submode, parmreg));

	      if (stack_parm != 0)
		{
		  parm_reg_stack_loc[regnor]
		    = gen_realpart (submode, stack_parm);
		  parm_reg_stack_loc[regnoi]
		    = gen_imagpart (submode, stack_parm);
		}
	      else
		{
		  parm_reg_stack_loc[regnor] = 0;
		  parm_reg_stack_loc[regnoi] = 0;
		}
	    }
	  else
	    parm_reg_stack_loc[REGNO (parmreg)] = stack_parm;

	  /* Mark the register as eliminable if we did no conversion
	     and it was copied from memory at a fixed offset,
	     and the arg pointer was not copied to a pseudo-reg.
	     If the arg pointer is a pseudo reg or the offset formed
	     an invalid address, such memory-equivalences
	     as we make here would screw up life analysis for it.  */
	  if (nominal_mode == passed_mode
	      && ! did_conversion
	      && stack_parm != 0
	      && GET_CODE (stack_parm) == MEM
	      && stack_offset.var == 0
	      && reg_mentioned_p (virtual_incoming_args_rtx,
				  XEXP (stack_parm, 0)))
	    {
	      rtx linsn = get_last_insn ();
	      rtx sinsn, set;

	      /* Mark complex types separately.  */
	      if (GET_CODE (parmreg) == CONCAT)
		/* Scan backwards for the set of the real and
		   imaginary parts.  */
		for (sinsn = linsn; sinsn != 0;
		     sinsn = prev_nonnote_insn (sinsn))
		  {
		    set = single_set (sinsn);
		    if (set != 0
			&& SET_DEST (set) == regno_reg_rtx [regnoi])
		      REG_NOTES (sinsn)
			= gen_rtx_EXPR_LIST (REG_EQUIV,
					     parm_reg_stack_loc[regnoi],
					     REG_NOTES (sinsn));
		    else if (set != 0
			     && SET_DEST (set) == regno_reg_rtx [regnor])
		      REG_NOTES (sinsn)
			= gen_rtx_EXPR_LIST (REG_EQUIV,
					     parm_reg_stack_loc[regnor],
					     REG_NOTES (sinsn));
		  }
	      else if ((set = single_set (linsn)) != 0
		       && SET_DEST (set) == parmreg)
	        REG_NOTES (linsn)
		  = gen_rtx_EXPR_LIST (REG_EQUIV,
				       stack_parm, REG_NOTES (linsn));
	    }

	  /* For pointer data type, suggest pointer register.  */
	  if (POINTER_TYPE_P (TREE_TYPE (parm)))
	    mark_reg_pointer (parmreg,
			      (TYPE_ALIGN (TREE_TYPE (TREE_TYPE (parm)))
			       / BITS_PER_UNIT));
	}
      else
	{
	  /* Value must be stored in the stack slot STACK_PARM
	     during function execution.  */

	  if (promoted_mode != nominal_mode)
	    {
	      /* Conversion is required.   */
	      rtx tempreg = gen_reg_rtx (GET_MODE (entry_parm));

	      emit_move_insn (tempreg, validize_mem (entry_parm));

	      push_to_sequence (conversion_insns);
	      entry_parm = convert_to_mode (nominal_mode, tempreg,
					    TREE_UNSIGNED (TREE_TYPE (parm)));
	      if (stack_parm)
		{
		  /* ??? This may need a big-endian conversion on sparc64.  */
		  stack_parm = change_address (stack_parm, nominal_mode,
					       NULL_RTX);
		}
	      conversion_insns = get_insns ();
	      did_conversion = 1;
	      end_sequence ();
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
		  MEM_SET_IN_STRUCT_P (stack_parm, aggregate);
		}

	      if (promoted_mode != nominal_mode)
		{
		  push_to_sequence (conversion_insns);
		  emit_move_insn (validize_mem (stack_parm),
				  validize_mem (entry_parm));
		  conversion_insns = get_insns ();
		  end_sequence ();
		}
	      else
		emit_move_insn (validize_mem (stack_parm),
				validize_mem (entry_parm));
	    }
	  if (current_function_check_memory_usage)
	    {
	      push_to_sequence (conversion_insns);
	      emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
				 XEXP (stack_parm, 0), Pmode,
				 GEN_INT (GET_MODE_SIZE (GET_MODE 
							 (entry_parm))),
				 TYPE_MODE (sizetype),
				 GEN_INT (MEMORY_USE_RW),
				 TYPE_MODE (integer_type_node));

	      conversion_insns = get_insns ();
	      end_sequence ();
	    }
	  DECL_RTL (parm) = stack_parm;
	}
      
      /* If this "parameter" was the place where we are receiving the
	 function's incoming structure pointer, set up the result.  */
      if (parm == function_result_decl)
	{
	  tree result = DECL_RESULT (fndecl);
	  tree restype = TREE_TYPE (result);

	  DECL_RTL (result)
	    = gen_rtx_MEM (DECL_MODE (result), DECL_RTL (parm));

	  MEM_SET_IN_STRUCT_P (DECL_RTL (result), 
			       AGGREGATE_TYPE_P (restype));
	}

      if (TREE_THIS_VOLATILE (parm))
	MEM_VOLATILE_P (DECL_RTL (parm)) = 1;
      if (TREE_READONLY (parm))
	RTX_UNCHANGING_P (DECL_RTL (parm)) = 1;
    }

  /* Output all parameter conversion instructions (possibly including calls)
     now that all parameters have been copied out of hard registers.  */
  emit_insns (conversion_insns);

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
		      NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_BAD));
#else
  current_function_arg_offset_rtx = ARGS_SIZE_RTX (stack_args_size);
#endif

  /* See how many bytes, if any, of its args a function should try to pop
     on return.  */

  current_function_pops_args = RETURN_POPS_ARGS (fndecl, TREE_TYPE (fndecl),
						 current_function_args_size);

  /* For stdarg.h function, save info about
     regs and stack space used by the named args.  */

  if (!hide_last_arg)
    current_function_args_info = args_so_far;

  /* Set the rtx used for the function return value.  Put this in its
     own variable so any optimizers that need this information don't have
     to include tree.h.  Do this here so it gets done when an inlined
     function gets output.  */

  current_function_return_rtx = DECL_RTL (DECL_RESULT (fndecl));
}

/* Indicate whether REGNO is an incoming argument to the current function
   that was promoted to a wider mode.  If so, return the RTX for the
   register (to get its mode).  PMODE and PUNSIGNEDP are set to the mode
   that REGNO is promoted from and whether the promotion was signed or
   unsigned.  */

#ifdef PROMOTE_FUNCTION_ARGS

rtx
promoted_input_arg (regno, pmode, punsignedp)
     int regno;
     enum machine_mode *pmode;
     int *punsignedp;
{
  tree arg;

  for (arg = DECL_ARGUMENTS (current_function_decl); arg;
       arg = TREE_CHAIN (arg))
    if (GET_CODE (DECL_INCOMING_RTL (arg)) == REG
	&& REGNO (DECL_INCOMING_RTL (arg)) == regno
	&& TYPE_MODE (DECL_ARG_TYPE (arg)) == TYPE_MODE (TREE_TYPE (arg)))
      {
	enum machine_mode mode = TYPE_MODE (TREE_TYPE (arg));
	int unsignedp = TREE_UNSIGNED (TREE_TYPE (arg));

	mode = promote_mode (TREE_TYPE (arg), mode, &unsignedp, 1);
	if (mode == GET_MODE (DECL_INCOMING_RTL (arg))
	    && mode != DECL_MODE (arg))
	  {
	    *pmode = DECL_MODE (arg);
	    *punsignedp = unsignedp;
	    return DECL_INCOMING_RTL (arg);
	  }
      }

  return 0;
}

#endif

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

void
locate_and_pad_parm (passed_mode, type, in_regs, fndecl,
		     initial_offset_ptr, offset_ptr, arg_size_ptr,
                     alignment_pad)
     enum machine_mode passed_mode;
     tree type;
     int in_regs ATTRIBUTE_UNUSED;
     tree fndecl ATTRIBUTE_UNUSED;
     struct args_size *initial_offset_ptr;
     struct args_size *offset_ptr;
     struct args_size *arg_size_ptr;
     struct args_size *alignment_pad;

{
  tree sizetree
    = type ? size_in_bytes (type) : size_int (GET_MODE_SIZE (passed_mode));
  enum direction where_pad = FUNCTION_ARG_PADDING (passed_mode, type);
  int boundary = FUNCTION_ARG_BOUNDARY (passed_mode, type);

#ifdef REG_PARM_STACK_SPACE
  /* If we have found a stack parm before we reach the end of the
     area reserved for registers, skip that area.  */
  if (! in_regs)
    {
      int reg_parm_stack_space = 0;

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
  if (where_pad != none
      && (TREE_CODE (sizetree) != INTEGER_CST
	  || ((TREE_INT_CST_LOW (sizetree) * BITS_PER_UNIT) % PARM_BOUNDARY)))
    sizetree = round_up (sizetree, PARM_BOUNDARY / BITS_PER_UNIT);
  SUB_PARM_SIZE (*offset_ptr, sizetree);
  if (where_pad != downward)
    pad_to_arg_alignment (offset_ptr, boundary, alignment_pad);
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
      arg_size_ptr->constant = (- initial_offset_ptr->constant
				- offset_ptr->constant); 
    }
#else /* !ARGS_GROW_DOWNWARD */
  pad_to_arg_alignment (initial_offset_ptr, boundary, alignment_pad);
  *offset_ptr = *initial_offset_ptr;

#ifdef PUSH_ROUNDING
  if (passed_mode != BLKmode)
    sizetree = size_int (PUSH_ROUNDING (TREE_INT_CST_LOW (sizetree)));
#endif

  /* Pad_below needs the pre-rounded size to know how much to pad below
     so this must be done before rounding up.  */
  if (where_pad == downward
    /* However, BLKmode args passed in regs have their padding done elsewhere.
       The stack slot must be able to hold the entire register.  */
      && !(in_regs && passed_mode == BLKmode))
    pad_below (offset_ptr, passed_mode, sizetree);

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
pad_to_arg_alignment (offset_ptr, boundary, alignment_pad)
     struct args_size *offset_ptr;
     int boundary;
     struct args_size *alignment_pad;
{
  tree save_var = NULL_TREE;
  HOST_WIDE_INT save_constant = 0;

  int boundary_in_bytes = boundary / BITS_PER_UNIT;
  
  if (boundary > PARM_BOUNDARY && boundary > STACK_BOUNDARY)
    {
      save_var = offset_ptr->var;
      save_constant = offset_ptr->constant;
    }

  alignment_pad->var = NULL_TREE;
  alignment_pad->constant = 0;

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
          if (boundary > PARM_BOUNDARY && boundary > STACK_BOUNDARY)
            alignment_pad->var = size_binop (MINUS_EXPR, offset_ptr->var, save_var);
	}
      else
        {
	  offset_ptr->constant =
#ifdef ARGS_GROW_DOWNWARD
	    FLOOR_ROUND (offset_ptr->constant, boundary_in_bytes);
#else
	    CEIL_ROUND (offset_ptr->constant, boundary_in_bytes);
#endif
          if (boundary > PARM_BOUNDARY && boundary > STACK_BOUNDARY)
            alignment_pad->constant = offset_ptr->constant - save_constant;
        }
    }
}

#ifndef ARGS_GROW_DOWNWARD
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
#endif

#ifdef ARGS_GROW_DOWNWARD
static tree
round_down (value, divisor)
     tree value;
     int divisor;
{
  return size_binop (MULT_EXPR,
		     size_binop (FLOOR_DIV_EXPR, value, size_int (divisor)),
		     size_int (divisor));
}
#endif

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
      if (warn_uninitialized
          && TREE_CODE (decl) == VAR_DECL
	  /* These warnings are unreliable for and aggregates
	     because assigning the fields one by one can fail to convince
	     flow.c that the entire aggregate was initialized.
	     Unions are troublesome because members may be shorter.  */
	  && ! AGGREGATE_TYPE_P (TREE_TYPE (decl))
	  && DECL_RTL (decl) != 0
	  && GET_CODE (DECL_RTL (decl)) == REG
	  /* Global optimizations can make it difficult to determine if a
	     particular variable has been initialized.  However, a VAR_DECL
	     with a nonzero DECL_INITIAL had an initializer, so do not
	     claim it is potentially uninitialized.

	     We do not care about the actual value in DECL_INITIAL, so we do
	     not worry that it may be a dangling pointer.  */
	  && DECL_INITIAL (decl) == NULL_TREE
	  && regno_uninitialized (REGNO (DECL_RTL (decl))))
	warning_with_decl (decl,
			   "`%s' might be used uninitialized in this function");
      if (extra_warnings
          && TREE_CODE (decl) == VAR_DECL
	  && DECL_RTL (decl) != 0
	  && GET_CODE (DECL_RTL (decl)) == REG
	  && regno_clobbered_at_setjmp (REGNO (DECL_RTL (decl))))
	warning_with_decl (decl,
			   "variable `%s' might be clobbered by `longjmp' or `vfork'");
    }
  for (sub = BLOCK_SUBBLOCKS (block); sub; sub = TREE_CHAIN (sub))
    uninitialized_vars_warning (sub);
}

/* Do the appropriate part of uninitialized_vars_warning
   but for arguments instead of local variables.  */

void
setjmp_args_warning ()
{
  register tree decl;
  for (decl = DECL_ARGUMENTS (current_function_decl);
       decl; decl = TREE_CHAIN (decl))
    if (DECL_RTL (decl) != 0
	&& GET_CODE (DECL_RTL (decl)) == REG
	&& regno_clobbered_at_setjmp (REGNO (DECL_RTL (decl))))
      warning_with_decl (decl, "argument `%s' might be clobbered by `longjmp' or `vfork'");
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
	&& (GET_CODE (DECL_RTL (decl)) == REG
	    || (GET_CODE (DECL_RTL (decl)) == MEM
		&& GET_CODE (XEXP (DECL_RTL (decl), 0)) == ADDRESSOF))
	/* If this variable came from an inline function, it must be
	   that its life doesn't overlap the setjmp.  If there was a
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
  register tree decl;
  for (decl = DECL_ARGUMENTS (current_function_decl);
       decl; decl = TREE_CHAIN (decl))
    if ((TREE_CODE (decl) == VAR_DECL
	 || TREE_CODE (decl) == PARM_DECL)
	&& DECL_RTL (decl) != 0
	&& (GET_CODE (DECL_RTL (decl)) == REG
	    || (GET_CODE (DECL_RTL (decl)) == MEM
		&& GET_CODE (XEXP (DECL_RTL (decl), 0)) == ADDRESSOF))
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

  if (context == 0
      || (TREE_CODE (decl) == FUNCTION_DECL && DECL_NO_STATIC_CHAIN (decl)))
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
  HOST_WIDE_INT displacement;
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

  if (GET_CODE (addr) == ADDRESSOF && GET_CODE (XEXP (addr, 0)) == MEM)
    addr = XEXP (XEXP (addr, 0), 0);

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

      if (fp->x_arg_pointer_save_area == 0)
	fp->x_arg_pointer_save_area
	  = assign_stack_local_1 (Pmode, GET_MODE_SIZE (Pmode), 0, fp);

      addr = fix_lexical_addr (XEXP (fp->x_arg_pointer_save_area, 0), var);
      addr = memory_address (Pmode, addr);

      base = copy_to_reg (gen_rtx_MEM (Pmode, addr));
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
      return
	round_trampoline_addr (XEXP (RTL_EXPR_RTL (TREE_VALUE (link)), 0));

  for (fp = outer_function_chain; fp; fp = fp->next)
    for (link = fp->x_trampoline_list; link; link = TREE_CHAIN (link))
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
  if (fn_context != current_function_decl
      && fn_context != inline_function_decl)
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
#define TRAMPOLINE_REAL_SIZE \
  (TRAMPOLINE_SIZE + (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT) - 1)
#else
#define TRAMPOLINE_REAL_SIZE (TRAMPOLINE_SIZE)
#endif
  tramp = assign_stack_local_1 (BLKmode, TRAMPOLINE_REAL_SIZE, 0,
				fp ? fp : cfun);
#endif

  /* Record the trampoline for reuse and note it for later initialization
     by expand_function_end.  */
  if (fp != 0)
    {
      push_obstacks (fp->function_maybepermanent_obstack,
		     fp->function_maybepermanent_obstack);
      rtlexp = make_node (RTL_EXPR);
      RTL_EXPR_RTL (rtlexp) = tramp;
      fp->x_trampoline_list = tree_cons (function, rtlexp,
					 fp->x_trampoline_list);
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
		       GEN_INT (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT - 1),
		       temp, 0, OPTAB_LIB_WIDEN);
  tramp = expand_binop (Pmode, and_optab, temp,
			GEN_INT (- TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT),
			temp, 0, OPTAB_LIB_WIDEN);
#endif
  return tramp;
}

/* The functions identify_blocks and reorder_blocks provide a way to
   reorder the tree of BLOCK nodes, for optimizers that reshuffle or
   duplicate portions of the RTL code.  Call identify_blocks before
   changing the RTL, and call reorder_blocks after.  */

/* Put all this function's BLOCK nodes including those that are chained
   onto the first block into a vector, and return it.
   Also store in each NOTE for the beginning or end of a block
   the index of that block in the vector.
   The arguments are BLOCK, the chain of top-level blocks of the function,
   and INSNS, the insn chain of the function.  */

void
identify_blocks (block, insns)
     tree block;
     rtx insns;
{
  int n_blocks;
  tree *block_vector;
  tree *block_stack;
  int depth = 0;
  int current_block_number = 1;
  rtx insn;

  if (block == 0)
    return;

  /* Fill the BLOCK_VECTOR with all of the BLOCKs in this function, in
     depth-first order.  */
  n_blocks = all_blocks (block, 0);
  block_vector = (tree *) xmalloc (n_blocks * sizeof (tree));
  all_blocks (block, block_vector);

  block_stack = (tree *) xmalloc (n_blocks * sizeof (tree));

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG)
	  {
	    tree b;

	    /* If there are more block notes than BLOCKs, something
	       is badly wrong.  */
	    if (current_block_number == n_blocks)
	      abort ();

	    b = block_vector[current_block_number++];
	    NOTE_BLOCK (insn) = b;
	    block_stack[depth++] = b;
	  }
	else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	  {
	    if (depth == 0)
	      /* There are more NOTE_INSN_BLOCK_ENDs that
		 NOTE_INSN_BLOCK_BEGs.  Something is badly wrong.  */
	      abort ();

	    NOTE_BLOCK (insn) = block_stack[--depth];
	  }
      }

  /* In whole-function mode, we might not have seen the whole function
     yet, so we might not use up all the blocks.  */
  if (n_blocks != current_block_number 
      && !cfun->x_whole_function_mode_p)
    abort ();

  free (block_vector);
  free (block_stack);
}

/* Given a revised instruction chain, rebuild the tree structure of
   BLOCK nodes to correspond to the new order of RTL.  The new block
   tree is inserted below TOP_BLOCK.  Returns the current top-level
   block.  */

tree
reorder_blocks (block, insns)
     tree block;
     rtx insns;
{
  tree current_block = block;
  rtx insn;

  if (block == NULL_TREE)
    return NULL_TREE;

  /* Prune the old trees away, so that it doesn't get in the way.  */
  BLOCK_SUBBLOCKS (current_block) = 0;
  BLOCK_CHAIN (current_block) = 0;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG)
	  {
	    tree block = NOTE_BLOCK (insn);
	    /* If we have seen this block before, copy it.  */
	    if (TREE_ASM_WRITTEN (block))
	      block = copy_node (block);
	    BLOCK_SUBBLOCKS (block) = 0;
	    TREE_ASM_WRITTEN (block) = 1;
	    BLOCK_SUPERCONTEXT (block) = current_block; 
	    BLOCK_CHAIN (block) = BLOCK_SUBBLOCKS (current_block);
	    BLOCK_SUBBLOCKS (current_block) = block;
	    current_block = block;
	  }
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	  {
	    BLOCK_SUBBLOCKS (current_block)
	      = blocks_nreverse (BLOCK_SUBBLOCKS (current_block));
	    current_block = BLOCK_SUPERCONTEXT (current_block);
	  }
      }

  BLOCK_SUBBLOCKS (current_block)
    = blocks_nreverse (BLOCK_SUBBLOCKS (current_block));
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

/* Count the subblocks of the list starting with BLOCK, and list them
   all into the vector VECTOR.  Also clear TREE_ASM_WRITTEN in all
   blocks.  */

static int
all_blocks (block, vector)
     tree block;
     tree *vector;
{
  int n_blocks = 0;

  while (block)
    {
      TREE_ASM_WRITTEN (block) = 0;

      /* Record this block.  */
      if (vector)
	vector[n_blocks] = block;

      ++n_blocks;
      
      /* Record the subblocks, and their subblocks...  */
      n_blocks += all_blocks (BLOCK_SUBBLOCKS (block),
			      vector ? vector + n_blocks : 0);
      block = BLOCK_CHAIN (block);
    }

  return n_blocks;
}

/* Allocate a function structure and reset its contents to the defaults.  */
static void
prepare_function_start ()
{
  cfun = (struct function *) xcalloc (1, sizeof (struct function));

  init_stmt_for_function ();
  init_eh_for_function ();

  cse_not_expected = ! optimize;

  /* Caller save not needed yet.  */
  caller_save_needed = 0;

  /* No stack slots have been made yet.  */
  stack_slot_list = 0;

  current_function_has_nonlocal_label = 0;
  current_function_has_nonlocal_goto = 0;

  /* There is no stack slot for handling nonlocal gotos.  */
  nonlocal_goto_handler_slots = 0;
  nonlocal_goto_stack_level = 0;

  /* No labels have been declared for nonlocal use.  */
  nonlocal_labels = 0;
  nonlocal_goto_handler_labels = 0;

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

  init_varasm_status (cfun);

  /* Clear out data used for inlining.  */
  cfun->inlinable = 0;
  cfun->original_decl_initial = 0;
  cfun->original_arg_vector = 0;  

  cfun->stack_alignment_needed = 0;

  /* Set if a call to setjmp is seen.  */
  current_function_calls_setjmp = 0;

  /* Set if a call to longjmp is seen.  */
  current_function_calls_longjmp = 0;

  current_function_calls_alloca = 0;
  current_function_contains_functions = 0;
  current_function_is_leaf = 0;
  current_function_sp_is_unchanging = 0;
  current_function_uses_only_leaf_regs = 0;
  current_function_has_computed_jump = 0;
  current_function_is_thunk = 0;

  current_function_returns_pcc_struct = 0;
  current_function_returns_struct = 0;
  current_function_epilogue_delay_list = 0;
  current_function_uses_const_pool = 0;
  current_function_uses_pic_offset_table = 0;
  current_function_cannot_inline = 0;

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

  /* Set up to allocate temporaries.  */
  init_temp_slots ();

  /* Indicate that we need to distinguish between the return value of the
     present function and the return value of a function being called.  */
  rtx_equal_function_value_matters = 1;

  /* Indicate that we have not instantiated virtual registers yet.  */
  virtuals_instantiated = 0;

  /* Indicate we have no need of a frame pointer yet.  */
  frame_pointer_needed = 0;

  /* By default assume not varargs or stdarg.  */
  current_function_varargs = 0;
  current_function_stdarg = 0;

  /* We haven't made any trampolines for this function yet.  */
  trampoline_list = 0;

  init_pending_stack_adjust ();
  inhibit_defer_pop = 0;

  current_function_outgoing_args_size = 0;

  if (init_lang_status)
    (*init_lang_status) (cfun);
  if (init_machine_status)
    (*init_machine_status) (cfun);
}

/* Initialize the rtl expansion mechanism so that we can do simple things
   like generate sequences.  This is used to provide a context during global
   initialization of some passes.  */
void
init_dummy_function_start ()
{
  prepare_function_start ();
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
  prepare_function_start ();

  /* Remember this function for later.  */
  cfun->next_global = all_functions;
  all_functions = cfun;
  
  current_function_name = (*decl_printable_name) (subr, 2);
  cfun->decl = subr;

  /* Nonzero if this is a nested function that uses a static chain.  */

  current_function_needs_context
    = (decl_function_context (current_function_decl) != 0
       && ! DECL_NO_STATIC_CHAIN (current_function_decl));

  /* Within function body, compute a type's size as soon it is laid out.  */
  immediate_size_expand++;

  /* Prevent ever trying to delete the first instruction of a function.
     Also tell final how to output a linenum before the function prologue.
     Note linenums could be missing, e.g. when compiling a Java .class file. */
  if (line > 0)
    emit_line_note (filename, line);

  /* Make sure first insn is a note even if we don't want linenums.
     This makes sure the first insn will never be deleted.
     Also, final expects a note to appear there.  */
  emit_note (NULL_PTR, NOTE_INSN_DELETED);

  /* Set flags used by final.c.  */
  if (aggregate_value_p (DECL_RESULT (subr)))
    {
#ifdef PCC_STATIC_STRUCT_RETURN
      current_function_returns_pcc_struct = 1;
#endif
      current_function_returns_struct = 1;
    }

  /* Warn if this value is an aggregate type,
     regardless of which calling convention we are using for it.  */
  if (warn_aggregate_return
      && AGGREGATE_TYPE_P (TREE_TYPE (DECL_RESULT (subr))))
    warning ("function returns an aggregate");

  current_function_returns_pointer
    = POINTER_TYPE_P (TREE_TYPE (DECL_RESULT (subr)));
}

/* Make sure all values used by the optimization passes have sane
   defaults.  */
void
init_function_for_compilation ()
{
  reg_renumber = 0;
  /* No prologue/epilogue insns yet.  */
  prologue = epilogue = 0;
}

/* Indicate that the current function uses extra args
   not explicitly mentioned in the argument list in any fashion.  */

void
mark_varargs ()
{
  current_function_varargs = 1;
}

/* Expand a call to __main at the beginning of a possible main function.  */

#if defined(INIT_SECTION_ASM_OP) && !defined(INVOKE__main)
#undef HAS_INIT_SECTION
#define HAS_INIT_SECTION
#endif

void
expand_main_function ()
{
#if !defined (HAS_INIT_SECTION)
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, NAME__MAIN), 0,
		     VOIDmode, 0);
#endif /* not HAS_INIT_SECTION */
}

extern struct obstack permanent_obstack;

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
  tree tem;
  rtx last_ptr = NULL_RTX;

  /* Make sure volatile mem refs aren't considered
     valid operands of arithmetic insns.  */
  init_recog_no_volatile ();

  /* Set this before generating any memory accesses.  */
  current_function_check_memory_usage
    = (flag_check_memory_usage
       && ! DECL_NO_CHECK_MEMORY_USAGE (current_function_decl));

  current_function_instrument_entry_exit
    = (flag_instrument_function_entry_exit
       && ! DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (subr));

  current_function_limit_stack
    = (stack_limit_rtx != NULL_RTX && ! DECL_NO_LIMIT_STACK (subr));

  /* If function gets a static chain arg, store it in the stack frame.
     Do this first, so it gets the first stack slot offset.  */
  if (current_function_needs_context)
    {
      last_ptr = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);

      /* Delay copying static chain if it is not a register to avoid
	 conflicts with regs used for parameters.  */
      if (! SMALL_REGISTER_CLASSES
	  || GET_CODE (static_chain_incoming_rtx) == REG)
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
      && ! current_function_instrument_entry_exit
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
      register rtx value_address = 0;

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
	{
	  DECL_RTL (DECL_RESULT (subr))
	    = gen_rtx_MEM (DECL_MODE (DECL_RESULT (subr)), value_address);
	  MEM_SET_IN_STRUCT_P (DECL_RTL (DECL_RESULT (subr)),
			       AGGREGATE_TYPE_P (TREE_TYPE
						 (DECL_RESULT
						  (subr))));
	}
    }
  else if (DECL_MODE (DECL_RESULT (subr)) == VOIDmode)
    /* If return mode is void, this decl rtl should not be used.  */
    DECL_RTL (DECL_RESULT (subr)) = 0;
  else if (parms_have_cleanups || current_function_instrument_entry_exit)
    {
      /* If function will end with cleanup code for parms,
	 compute the return values into a pseudo reg,
	 which we will copy into the true return register
	 after the cleanups are done.  */

      enum machine_mode mode = DECL_MODE (DECL_RESULT (subr));

#ifdef PROMOTE_FUNCTION_RETURN
      tree type = TREE_TYPE (DECL_RESULT (subr));
      int unsignedp = TREE_UNSIGNED (type);

      mode = promote_mode (type, mode, &unsignedp, 1);
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

  assign_parms (subr);

  /* Copy the static chain now if it wasn't a register.  The delay is to
     avoid conflicts with the parameter passing registers.  */

  if (SMALL_REGISTER_CLASSES && current_function_needs_context)
      if (GET_CODE (static_chain_incoming_rtx) != REG)
        emit_move_insn (last_ptr, static_chain_incoming_rtx);

  /* The following was moved from init_function_start.
     The move is supposed to make sdb output more accurate.  */
  /* Indicate the beginning of the function body,
     as opposed to parm setup.  */
  emit_note (NULL_PTR, NOTE_INSN_FUNCTION_BEG);

  if (GET_CODE (get_last_insn ()) != NOTE)
    emit_note (NULL_PTR, NOTE_INSN_DELETED);
  parm_birth_insn = get_last_insn ();

  context_display = 0;
  if (current_function_needs_context)
    {
      /* Fetch static chain values for containing functions.  */
      tem = decl_function_context (current_function_decl);
      /* Copy the static chain pointer into a pseudo.  If we have
	 small register classes, copy the value from memory if
	 static_chain_incoming_rtx is a REG.  */
      if (tem)
	{
	  /* If the static chain originally came in a register, put it back
	     there, then move it out in the next insn.  The reason for
	     this peculiar code is to satisfy function integration.  */
	  if (SMALL_REGISTER_CLASSES
	      && GET_CODE (static_chain_incoming_rtx) == REG)
	    emit_move_insn (static_chain_incoming_rtx, last_ptr);
	  last_ptr = copy_to_reg (static_chain_incoming_rtx);
	}

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
	  last_ptr = copy_to_reg (gen_rtx_MEM (Pmode,
					       memory_address (Pmode,
							       last_ptr)));

	  /* If we are not optimizing, ensure that we know that this
	     piece of context is live over the entire function.  */
	  if (! optimize)
	    save_expr_regs = gen_rtx_EXPR_LIST (VOIDmode, last_ptr,
						save_expr_regs);
	}
    }

  if (current_function_instrument_entry_exit)
    {
      rtx fun = DECL_RTL (current_function_decl);
      if (GET_CODE (fun) == MEM)
	fun = XEXP (fun, 0);
      else
	abort ();
      emit_library_call (profile_function_entry_libfunc, 0, VOIDmode, 2,
			 fun, Pmode,
			 expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
						     0,
						     hard_frame_pointer_rtx),
			 Pmode);
    }

  /* After the display initializations is where the tail-recursion label
     should go, if we end up needing one.   Ensure we have a NOTE here
     since some things (like trampolines) get placed before this.  */
  tail_recursion_reentry = emit_note (NULL_PTR, NOTE_INSN_DELETED);

  /* Evaluate now the sizes of any types declared among the arguments.  */
  for (tem = nreverse (get_pending_sizes ()); tem; tem = TREE_CHAIN (tem))
    {
      expand_expr (TREE_VALUE (tem), const0_rtx, VOIDmode,
		   EXPAND_MEMORY_USE_BAD);
      /* Flush the queue in case this parameter declaration has
	 side-effects.  */
      emit_queue ();
    }

  /* Make sure there is a line number after the function entry setup code.  */
  force_next_line_note ();
}

/* Undo the effects of init_dummy_function_start.  */
void
expand_dummy_function_end ()
{
  /* End any sequences that failed to be closed due to syntax errors.  */
  while (in_sequence_p ())
    end_sequence ();

  /* Outside function body, can't compute type's actual size
     until next function's body starts.  */

  free_after_parsing (cfun);
  free_after_compilation (cfun);
  free (cfun);
  cfun = 0;
}

/* Emit CODE for each register of the return value.  Useful values for
   code are USE and CLOBBER.  */

void
diddle_return_value (code)
     enum rtx_code code;
{
  tree decl_result = DECL_RESULT (current_function_decl);
  rtx return_reg = DECL_RTL (decl_result);

  if (return_reg)
    {
      if (GET_CODE (return_reg) == REG
	  && REGNO (return_reg) < FIRST_PSEUDO_REGISTER)
	{
	  /* Use hard_function_value to avoid creating a reference to a BLKmode 
	     register in the USE/CLOBBER insn.  */
	  return_reg = hard_function_value (TREE_TYPE (decl_result),
					    current_function_decl, 1);
	  REG_FUNCTION_VALUE_P (return_reg) = 1;
	  emit_insn (gen_rtx_fmt_e (code, VOIDmode, return_reg));
	}
      else if (GET_CODE (return_reg) == PARALLEL)
	{
	  int i;

	  for (i = 0; i < XVECLEN (return_reg, 0); i++)
	    {
	      rtx x = XEXP (XVECEXP (return_reg, 0, i), 0);

	      if (GET_CODE (x) == REG
		  && REGNO (x) < FIRST_PSEUDO_REGISTER)
		emit_insn (gen_rtx_fmt_e (code, VOIDmode, x));
	    }
	}
    }
}

/* Generate RTL for the end of the current function.
   FILENAME and LINE are the current position in the source file. 

   It is up to language-specific callers to do cleanups for parameters--
   or else, supply 1 for END_BINDINGS and we will call expand_end_bindings.  */

void
expand_function_end (filename, line, end_bindings)
     char *filename;
     int line;
     int end_bindings;
{
  tree link;

#ifdef TRAMPOLINE_TEMPLATE
  static rtx initial_trampoline;
#endif

  finish_expr_for_function ();

#ifdef NON_SAVING_SETJMP
  /* Don't put any variables in registers if we call setjmp
     on a machine that fails to restore the registers.  */
  if (NON_SAVING_SETJMP && current_function_calls_setjmp)
    {
      if (DECL_INITIAL (current_function_decl) != error_mark_node)
	setjmp_protect (DECL_INITIAL (current_function_decl));

      setjmp_protect_args ();
    }
#endif

  /* Save the argument pointer if a save area was made for it.  */
  if (arg_pointer_save_area)
    {
      /* arg_pointer_save_area may not be a valid memory address, so we
	 have to check it and fix it if necessary.  */
      rtx seq;
      start_sequence ();
      emit_move_insn (validize_mem (arg_pointer_save_area),
		      virtual_incoming_args_rtx);
      seq = gen_sequence ();
      end_sequence ();
      emit_insn_before (seq, tail_recursion_reentry);
    }

  /* Initialize any trampolines required by this function.  */
  for (link = trampoline_list; link; link = TREE_CHAIN (link))
    {
      tree function = TREE_PURPOSE (link);
      rtx context ATTRIBUTE_UNUSED = lookup_static_chain (function);
      rtx tramp = RTL_EXPR_RTL (TREE_VALUE (link));
#ifdef TRAMPOLINE_TEMPLATE
      rtx blktramp;
#endif
      rtx seq;

#ifdef TRAMPOLINE_TEMPLATE
      /* First make sure this compilation has a template for
	 initializing trampolines.  */
      if (initial_trampoline == 0)
	{
	  end_temporary_allocation ();
	  initial_trampoline
	    = gen_rtx_MEM (BLKmode, assemble_trampoline_template ());
	  resume_temporary_allocation ();

	  ggc_add_rtx_root (&initial_trampoline, 1);
	}
#endif

      /* Generate insns to initialize the trampoline.  */
      start_sequence ();
      tramp = round_trampoline_addr (XEXP (tramp, 0));
#ifdef TRAMPOLINE_TEMPLATE
      blktramp = change_address (initial_trampoline, BLKmode, tramp);
      emit_block_move (blktramp, initial_trampoline,
		       GEN_INT (TRAMPOLINE_SIZE),
		       TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT);
#endif
      INITIALIZE_TRAMPOLINE (tramp, XEXP (DECL_RTL (function), 0), context);
      seq = get_insns ();
      end_sequence ();

      /* Put those insns at entry to the containing function (this one).  */
      emit_insns_before (seq, tail_recursion_reentry);
    }

  /* If we are doing stack checking and this function makes calls,
     do a stack probe at the start of the function to ensure we have enough
     space for another stack frame.  */
  if (flag_stack_check && ! STACK_CHECK_BUILTIN)
    {
      rtx insn, seq;

      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == CALL_INSN)
	  {
	    start_sequence ();
	    probe_stack_range (STACK_CHECK_PROTECT,
			       GEN_INT (STACK_CHECK_MAX_FRAME_SIZE));
	    seq = get_insns ();
	    end_sequence ();
	    emit_insns_before (seq, tail_recursion_reentry);
	    break;
	  }
    }

  /* Warn about unused parms if extra warnings were specified.  */
  if (warn_unused && extra_warnings)
    {
      tree decl;

      for (decl = DECL_ARGUMENTS (current_function_decl);
	   decl; decl = TREE_CHAIN (decl))
	if (! TREE_USED (decl) && TREE_CODE (decl) == PARM_DECL
	    && DECL_NAME (decl) && ! DECL_ARTIFICIAL (decl))
	  warning_with_decl (decl, "unused parameter `%s'");
    }

  /* Delete handlers for nonlocal gotos if nothing uses them.  */
  if (nonlocal_goto_handler_slots != 0
      && ! current_function_has_nonlocal_label)
    delete_handlers ();

  /* End any sequences that failed to be closed due to syntax errors.  */
  while (in_sequence_p ())
    end_sequence ();

  /* Outside function body, can't compute type's actual size
     until next function's body starts.  */
  immediate_size_expand--;

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();

  /* Mark the end of the function body.
     If control reaches this insn, the function can drop through
     without returning a value.  */
  emit_note (NULL_PTR, NOTE_INSN_FUNCTION_END);

  /* Must mark the last line number note in the function, so that the test
     coverage code can avoid counting the last line twice.  This just tells
     the code to ignore the immediately following line note, since there
     already exists a copy of this note somewhere above.  This line number
     note is still needed for debugging though, so we can't delete it.  */
  if (flag_test_coverage)
    emit_note (NULL_PTR, NOTE_REPEATED_LINE_NUMBER);

  /* Output a linenumber for the end of the function.
     SDB depends on this.  */
  emit_line_note_force (filename, line);

  /* Output the label for the actual return from the function,
     if one is expected.  This happens either because a function epilogue
     is used instead of a return instruction, or because a return was done
     with a goto in order to run local cleanups, or because of pcc-style
     structure returning.  */

  if (return_label)
    {
      /* Before the return label, clobber the return registers so that
         they are not propogated live to the rest of the function.  This
	 can only happen with functions that drop through; if there had
	 been a return statement, there would have either been a return
	 rtx, or a jump to the return label.  */
      diddle_return_value (CLOBBER);

      emit_label (return_label);
    }

  /* C++ uses this.  */
  if (end_bindings)
    expand_end_bindings (0, 0, 0);

  /* Now handle any leftover exception regions that may have been
     created for the parameters.  */
  {
    rtx last = get_last_insn ();
    rtx label;

    expand_leftover_cleanups ();

    /* If there are any catch_clauses remaining, output them now.  */
    emit_insns (catch_clauses);
    catch_clauses = NULL_RTX;
    /* If the above emitted any code, may sure we jump around it.  */
    if (last != get_last_insn ())
      {
	label = gen_label_rtx ();
	last = emit_jump_insn_after (gen_jump (label), last);
	last = emit_barrier_after (last);
	emit_label (label);
      }
  }

  if (current_function_instrument_entry_exit)
    {
      rtx fun = DECL_RTL (current_function_decl);
      if (GET_CODE (fun) == MEM)
	fun = XEXP (fun, 0);
      else
	abort ();
      emit_library_call (profile_function_exit_libfunc, 0, VOIDmode, 2,
			 fun, Pmode,
			 expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
						     0,
						     hard_frame_pointer_rtx),
			 Pmode);
    }

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
      /* If this is a BLKmode structure being returned in registers, then use
	 the mode computed in expand_return.  */
      if (GET_MODE (real_decl_result) == BLKmode)
	PUT_MODE (real_decl_result,
		  GET_MODE (DECL_RTL (DECL_RESULT (current_function_decl))));
      emit_move_insn (real_decl_result,
		      DECL_RTL (DECL_RESULT (current_function_decl)));

      /* The delay slot scheduler assumes that current_function_return_rtx
	 holds the hard register containing the return value, not a temporary
	 pseudo.  */
      current_function_return_rtx = real_decl_result;
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
    }

  /* If this is an implementation of __throw, do what's necessary to 
     communicate between __builtin_eh_return and the epilogue.  */
  expand_eh_return ();

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
  expand_fixups (get_insns ());
}

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

int
prologue_epilogue_contains (insn)
     rtx insn;
{
  if (prologue && contains (insn, prologue))
    return 1;
  if (epilogue && contains (insn, epilogue))
    return 1;
  return 0;
}

/* Generate the prologue and epilogue RTL if the machine supports it.  Thread
   this into place with notes indicating where the prologue ends and where
   the epilogue begins.  Update the basic block information when possible.  */

void
thread_prologue_and_epilogue_insns (f)
     rtx f ATTRIBUTE_UNUSED;
{
  int insertted = 0;
  edge e;
  rtx seq;

#ifdef HAVE_prologue
  if (HAVE_prologue)
    {
      start_sequence ();
      seq = gen_prologue();
      emit_insn (seq);

      /* Retain a map of the prologue insns.  */
      if (GET_CODE (seq) != SEQUENCE)
	seq = get_insns ();
      prologue = record_insns (seq);

      emit_note (NULL, NOTE_INSN_PROLOGUE_END);
      seq = gen_sequence ();
      end_sequence ();

      /* If optimization is off, and perhaps in an empty function,
	 the entry block will have no successors.  */
      if (ENTRY_BLOCK_PTR->succ)
	{
	  /* Can't deal with multiple successsors of the entry block.  */
	  if (ENTRY_BLOCK_PTR->succ->succ_next)
	    abort ();

	  insert_insn_on_edge (seq, ENTRY_BLOCK_PTR->succ);
	  insertted = 1;
	}
      else
	emit_insn_after (seq, f);
    }
#endif

  /* If the exit block has no non-fake predecessors, we don't need
     an epilogue.  */
  for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
    if ((e->flags & EDGE_FAKE) == 0)
      break;
  if (e == NULL)
    goto epilogue_done;

#ifdef HAVE_epilogue
  if (HAVE_epilogue)
    {
      /* Find the edge that falls through to EXIT.  Other edges may exist
	 due to RETURN instructions, but those don't need epilogues.
	 There really shouldn't be a mixture -- either all should have
	 been converted or none, however...  */

      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	if (e->flags & EDGE_FALLTHRU)
	  break;
      if (e == NULL)
	goto epilogue_done;

      start_sequence ();
      emit_note (NULL, NOTE_INSN_EPILOGUE_BEG);

      seq = gen_epilogue ();
      emit_jump_insn (seq);

      /* Retain a map of the epilogue insns.  */
      if (GET_CODE (seq) != SEQUENCE)
	seq = get_insns ();
      epilogue = record_insns (seq);

      seq = gen_sequence ();
      end_sequence();

      insert_insn_on_edge (seq, e);
      insertted = 1;
    }
#endif
epilogue_done:

  if (insertted)
    commit_edge_insertions ();
}

/* Reposition the prologue-end and epilogue-begin notes after instruction
   scheduling and delayed branch scheduling.  */

void
reposition_prologue_and_epilogue_notes (f)
     rtx f ATTRIBUTE_UNUSED;
{
#if defined (HAVE_prologue) || defined (HAVE_epilogue)
  /* Reposition the prologue and epilogue notes.  */
  if (n_basic_blocks)
    {
      int len;

      if (prologue)
	{
	  register rtx insn, note = 0;

	  /* Scan from the beginning until we reach the last prologue insn.
	     We apparently can't depend on basic_block_{head,end} after
	     reorg has run.  */
	  for (len = 0; prologue[len]; len++)
	    ;
	  for (insn = f; len && insn; insn = NEXT_INSN (insn))
	    {
	      if (GET_CODE (insn) == NOTE)
		{
		  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_PROLOGUE_END)
		    note = insn;
		}
	      else if ((len -= contains (insn, prologue)) == 0)
		{
		  rtx next;
		  /* Find the prologue-end note if we haven't already, and
		     move it to just after the last prologue insn.  */
		  if (note == 0)
		    {
		      for (note = insn; (note = NEXT_INSN (note));)
			if (GET_CODE (note) == NOTE
			    && NOTE_LINE_NUMBER (note) == NOTE_INSN_PROLOGUE_END)
			  break;
		    }

		  next = NEXT_INSN (note);

		  /* Whether or not we can depend on BLOCK_HEAD, 
		     attempt to keep it up-to-date.  */
		  if (BLOCK_HEAD (0) == note)
		    BLOCK_HEAD (0) = next;

		  remove_insn (note);
		  add_insn_after (note, insn);
		}
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
	  for (insn = get_last_insn (); len && insn; insn = PREV_INSN (insn))
	    {
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
		    {
		      for (note = insn; (note = PREV_INSN (note));)
			if (GET_CODE (note) == NOTE
			    && NOTE_LINE_NUMBER (note) == NOTE_INSN_EPILOGUE_BEG)
			  break;
		    }

		  /* Whether or not we can depend on BLOCK_HEAD, 
		     attempt to keep it up-to-date.  */
		  if (n_basic_blocks
		      && BLOCK_HEAD (n_basic_blocks-1) == insn)
		    BLOCK_HEAD (n_basic_blocks-1) = note;

		  remove_insn (note);
		  add_insn_before (note, insn);
		}
	    }
	}
    }
#endif /* HAVE_prologue or HAVE_epilogue */
}

/* Mark T for GC.  */

static void
mark_temp_slot (t)
  struct temp_slot *t;
{
  while (t)
    {
      ggc_mark_rtx (t->slot);
      ggc_mark_rtx (t->address);
      ggc_mark_tree (t->rtl_expr);

      t = t->next;
    }
}

/* Mark P for GC.  */

static void
mark_function_status (p)
     struct function *p;
{
  int i;
  rtx *r;

  if (p == 0)
    return;

  ggc_mark_rtx (p->arg_offset_rtx);

  if (p->x_parm_reg_stack_loc)
    for (i = p->x_max_parm_reg, r = p->x_parm_reg_stack_loc;
	 i > 0; --i, ++r)
      ggc_mark_rtx (*r);

  ggc_mark_rtx (p->return_rtx);
  ggc_mark_rtx (p->x_cleanup_label);
  ggc_mark_rtx (p->x_return_label);
  ggc_mark_rtx (p->x_save_expr_regs);
  ggc_mark_rtx (p->x_stack_slot_list);
  ggc_mark_rtx (p->x_parm_birth_insn);
  ggc_mark_rtx (p->x_tail_recursion_label);
  ggc_mark_rtx (p->x_tail_recursion_reentry);
  ggc_mark_rtx (p->internal_arg_pointer);
  ggc_mark_rtx (p->x_arg_pointer_save_area);
  ggc_mark_tree (p->x_rtl_expr_chain);
  ggc_mark_rtx (p->x_last_parm_insn);
  ggc_mark_tree (p->x_context_display);
  ggc_mark_tree (p->x_trampoline_list);
  ggc_mark_rtx (p->epilogue_delay_list);

  mark_temp_slot (p->x_temp_slots);

  {
    struct var_refs_queue *q = p->fixup_var_refs_queue;
    while (q)
      {
	ggc_mark_rtx (q->modified);
	q = q->next;
      }
  }

  ggc_mark_rtx (p->x_nonlocal_goto_handler_slots);
  ggc_mark_rtx (p->x_nonlocal_goto_handler_labels);
  ggc_mark_rtx (p->x_nonlocal_goto_stack_level);
  ggc_mark_tree (p->x_nonlocal_labels);
}

/* Mark the function chain ARG (which is really a struct function **)
   for GC.  */

static void
mark_function_chain (arg)
     void *arg;
{
  struct function *f = *(struct function **) arg;

  for (; f; f = f->next_global)
    {
      ggc_mark_tree (f->decl);

      mark_function_status (f);
      mark_eh_status (f->eh);
      mark_stmt_status (f->stmt);
      mark_expr_status (f->expr);
      mark_emit_status (f->emit);
      mark_varasm_status (f->varasm);

      if (mark_machine_status)
	(*mark_machine_status) (f);
      if (mark_lang_status)
	(*mark_lang_status) (f);

      if (f->original_arg_vector)
	ggc_mark_rtvec ((rtvec) f->original_arg_vector);
      if (f->original_decl_initial)
	ggc_mark_tree (f->original_decl_initial);
    }
}

/* Called once, at initialization, to initialize function.c.  */

void
init_function_once ()
{
  ggc_add_root (&all_functions, 1, sizeof all_functions,
		mark_function_chain);
}
