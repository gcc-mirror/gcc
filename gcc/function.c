/* Expands front end tree to back end RTL for GCC.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
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
   not get a hard register.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "basic-block.h"
#include "toplev.h"
#include "hashtab.h"
#include "ggc.h"
#include "tm_p.h"
#include "integrate.h"
#include "langhooks.h"
#include "target.h"
#include "cfglayout.h"
#include "gimple.h"
#include "tree-pass.h"
#include "predict.h"
#include "df.h"
#include "timevar.h"
#include "vecprim.h"

/* So we can assign to cfun in this file.  */
#undef cfun

#ifndef STACK_ALIGNMENT_NEEDED
#define STACK_ALIGNMENT_NEEDED 1
#endif

#define STACK_BYTES (STACK_BOUNDARY / BITS_PER_UNIT)

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#endif

/* Round a value to the lowest integer less than it that is a multiple of
   the required alignment.  Avoid using division in case the value is
   negative.  Assume the alignment is a power of two.  */
#define FLOOR_ROUND(VALUE,ALIGN) ((VALUE) & ~((ALIGN) - 1))

/* Similar, but round to the next highest integer that meets the
   alignment.  */
#define CEIL_ROUND(VALUE,ALIGN)	(((VALUE) + (ALIGN) - 1) & ~((ALIGN)- 1))

/* Nonzero if function being compiled doesn't contain any calls
   (ignoring the prologue and epilogue).  This is set prior to
   local register allocation and is valid for the remaining
   compiler passes.  */
int current_function_is_leaf;

/* Nonzero if function being compiled doesn't modify the stack pointer
   (ignoring the prologue and epilogue).  This is only valid after
   pass_stack_ptr_mod has run.  */
int current_function_sp_is_unchanging;

/* Nonzero if the function being compiled is a leaf function which only
   uses leaf registers.  This is valid after reload (specifically after
   sched2) and is useful only if the port defines LEAF_REGISTERS.  */
int current_function_uses_only_leaf_regs;

/* Nonzero once virtual register instantiation has been done.
   assign_stack_local uses frame_pointer_rtx when this is nonzero.
   calls.c:emit_library_call_value_1 uses it to set up
   post-instantiation libcalls.  */
int virtuals_instantiated;

/* Assign unique numbers to labels generated for profiling, debugging, etc.  */
static GTY(()) int funcdef_no;

/* These variables hold pointers to functions to create and destroy
   target specific, per-function data structures.  */
struct machine_function * (*init_machine_status) (void);

/* The currently compiled function.  */
struct function *cfun = 0;

/* These hashes record the prologue and epilogue insns.  */
static GTY((if_marked ("ggc_marked_p"), param_is (struct rtx_def)))
  htab_t prologue_insn_hash;
static GTY((if_marked ("ggc_marked_p"), param_is (struct rtx_def)))
  htab_t epilogue_insn_hash;


htab_t types_used_by_vars_hash = NULL;
tree types_used_by_cur_var_decl = NULL;

/* Forward declarations.  */

static struct temp_slot *find_temp_slot_from_address (rtx);
static void pad_to_arg_alignment (struct args_size *, int, struct args_size *);
static void pad_below (struct args_size *, enum machine_mode, tree);
static void reorder_blocks_1 (rtx, tree, VEC(tree,heap) **);
static int all_blocks (tree, tree *);
static tree *get_block_vector (tree, int *);
extern tree debug_find_var_in_block_tree (tree, tree);
/* We always define `record_insns' even if it's not used so that we
   can always export `prologue_epilogue_contains'.  */
static void record_insns (rtx, rtx, htab_t *) ATTRIBUTE_UNUSED;
static bool contains (const_rtx, htab_t);
#ifdef HAVE_return
static void emit_return_into_block (basic_block);
#endif
static void prepare_function_start (void);
static void do_clobber_return_reg (rtx, void *);
static void do_use_return_reg (rtx, void *);
static void set_insn_locators (rtx, int) ATTRIBUTE_UNUSED;

/* Stack of nested functions.  */
/* Keep track of the cfun stack.  */

typedef struct function *function_p;

DEF_VEC_P(function_p);
DEF_VEC_ALLOC_P(function_p,heap);
static VEC(function_p,heap) *function_context_stack;

/* Save the current context for compilation of a nested function.
   This is called from language-specific code.  */

void
push_function_context (void)
{
  if (cfun == 0)
    allocate_struct_function (NULL, false);

  VEC_safe_push (function_p, heap, function_context_stack, cfun);
  set_cfun (NULL);
}

/* Restore the last saved context, at the end of a nested function.
   This function is called from language-specific code.  */

void
pop_function_context (void)
{
  struct function *p = VEC_pop (function_p, function_context_stack);
  set_cfun (p);
  current_function_decl = p->decl;

  /* Reset variables that have known state during rtx generation.  */
  virtuals_instantiated = 0;
  generating_concat_p = 1;
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been parsed, but not compiled, to let
   garbage collection reclaim the memory.  */

void
free_after_parsing (struct function *f)
{
  f->language = 0;
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been compiled, to let garbage collection
   reclaim the memory.  */

void
free_after_compilation (struct function *f)
{
  prologue_insn_hash = NULL;
  epilogue_insn_hash = NULL;

  if (crtl->emit.regno_pointer_align)
    free (crtl->emit.regno_pointer_align);

  memset (crtl, 0, sizeof (struct rtl_data));
  f->eh = NULL;
  f->machine = NULL;
  f->cfg = NULL;

  regno_reg_rtx = NULL;
  insn_locators_free ();
}

/* Return size needed for stack frame based on slots so far allocated.
   This size counts from zero.  It is not rounded to PREFERRED_STACK_BOUNDARY;
   the caller may have to do that.  */

HOST_WIDE_INT
get_frame_size (void)
{
  if (FRAME_GROWS_DOWNWARD)
    return -frame_offset;
  else
    return frame_offset;
}

/* Issue an error message and return TRUE if frame OFFSET overflows in
   the signed target pointer arithmetics for function FUNC.  Otherwise
   return FALSE.  */

bool
frame_offset_overflow (HOST_WIDE_INT offset, tree func)
{
  unsigned HOST_WIDE_INT size = FRAME_GROWS_DOWNWARD ? -offset : offset;

  if (size > ((unsigned HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (Pmode) - 1))
	       /* Leave room for the fixed part of the frame.  */
	       - 64 * UNITS_PER_WORD)
    {
      error_at (DECL_SOURCE_LOCATION (func),
		"total size of local objects too large");
      return TRUE;
    }

  return FALSE;
}

/* Return stack slot alignment in bits for TYPE and MODE.  */

static unsigned int
get_stack_local_alignment (tree type, enum machine_mode mode)
{
  unsigned int alignment;

  if (mode == BLKmode)
    alignment = BIGGEST_ALIGNMENT;
  else
    alignment = GET_MODE_ALIGNMENT (mode);

  /* Allow the frond-end to (possibly) increase the alignment of this
     stack slot.  */
  if (! type)
    type = lang_hooks.types.type_for_mode (mode, 0);

  return STACK_SLOT_ALIGNMENT (type, mode, alignment);
}

/* Allocate a stack slot of SIZE bytes and return a MEM rtx for it
   with machine mode MODE.

   ALIGN controls the amount of alignment for the address of the slot:
   0 means according to MODE,
   -1 means use BIGGEST_ALIGNMENT and round size to multiple of that,
   -2 means use BITS_PER_UNIT,
   positive specifies alignment boundary in bits.

   If REDUCE_ALIGNMENT_OK is true, it is OK to reduce alignment.

   We do not round to stack_boundary here.  */

rtx
assign_stack_local_1 (enum machine_mode mode, HOST_WIDE_INT size,
		      int align,
		      bool reduce_alignment_ok ATTRIBUTE_UNUSED)
{
  rtx x, addr;
  int bigend_correction = 0;
  unsigned int alignment, alignment_in_bits;
  int frame_off, frame_alignment, frame_phase;

  if (align == 0)
    {
      alignment = get_stack_local_alignment (NULL, mode);
      alignment /= BITS_PER_UNIT;
    }
  else if (align == -1)
    {
      alignment = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
      size = CEIL_ROUND (size, alignment);
    }
  else if (align == -2)
    alignment = 1; /* BITS_PER_UNIT / BITS_PER_UNIT */
  else
    alignment = align / BITS_PER_UNIT;

  alignment_in_bits = alignment * BITS_PER_UNIT;

  if (FRAME_GROWS_DOWNWARD)
    frame_offset -= size;

  /* Ignore alignment if it exceeds MAX_SUPPORTED_STACK_ALIGNMENT.  */
  if (alignment_in_bits > MAX_SUPPORTED_STACK_ALIGNMENT)
    {
      alignment_in_bits = MAX_SUPPORTED_STACK_ALIGNMENT;
      alignment = alignment_in_bits / BITS_PER_UNIT;
    }

  if (SUPPORTS_STACK_ALIGNMENT)
    {
      if (crtl->stack_alignment_estimated < alignment_in_bits)
	{
          if (!crtl->stack_realign_processed)
	    crtl->stack_alignment_estimated = alignment_in_bits;
          else
	    {
	      /* If stack is realigned and stack alignment value
		 hasn't been finalized, it is OK not to increase
		 stack_alignment_estimated.  The bigger alignment
		 requirement is recorded in stack_alignment_needed
		 below.  */
	      gcc_assert (!crtl->stack_realign_finalized);
	      if (!crtl->stack_realign_needed)
		{
		  /* It is OK to reduce the alignment as long as the
		     requested size is 0 or the estimated stack
		     alignment >= mode alignment.  */
		  gcc_assert (reduce_alignment_ok
		              || size == 0
			      || (crtl->stack_alignment_estimated
				  >= GET_MODE_ALIGNMENT (mode)));
		  alignment_in_bits = crtl->stack_alignment_estimated;
		  alignment = alignment_in_bits / BITS_PER_UNIT;
		}
	    }
	}
    }

  if (crtl->stack_alignment_needed < alignment_in_bits)
    crtl->stack_alignment_needed = alignment_in_bits;
  if (crtl->max_used_stack_slot_alignment < alignment_in_bits)
    crtl->max_used_stack_slot_alignment = alignment_in_bits;

  /* Calculate how many bytes the start of local variables is off from
     stack alignment.  */
  frame_alignment = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;
  frame_off = STARTING_FRAME_OFFSET % frame_alignment;
  frame_phase = frame_off ? frame_alignment - frame_off : 0;

  /* Round the frame offset to the specified alignment.  The default is
     to always honor requests to align the stack but a port may choose to
     do its own stack alignment by defining STACK_ALIGNMENT_NEEDED.  */
  if (STACK_ALIGNMENT_NEEDED
      || mode != BLKmode
      || size != 0)
    {
      /*  We must be careful here, since FRAME_OFFSET might be negative and
	  division with a negative dividend isn't as well defined as we might
	  like.  So we instead assume that ALIGNMENT is a power of two and
	  use logical operations which are unambiguous.  */
      if (FRAME_GROWS_DOWNWARD)
	frame_offset
	  = (FLOOR_ROUND (frame_offset - frame_phase,
			  (unsigned HOST_WIDE_INT) alignment)
	     + frame_phase);
      else
	frame_offset
	  = (CEIL_ROUND (frame_offset - frame_phase,
			 (unsigned HOST_WIDE_INT) alignment)
	     + frame_phase);
    }

  /* On a big-endian machine, if we are allocating more space than we will use,
     use the least significant bytes of those that are allocated.  */
  if (BYTES_BIG_ENDIAN && mode != BLKmode && GET_MODE_SIZE (mode) < size)
    bigend_correction = size - GET_MODE_SIZE (mode);

  /* If we have already instantiated virtual registers, return the actual
     address relative to the frame pointer.  */
  if (virtuals_instantiated)
    addr = plus_constant (frame_pointer_rtx,
			  trunc_int_for_mode
			  (frame_offset + bigend_correction
			   + STARTING_FRAME_OFFSET, Pmode));
  else
    addr = plus_constant (virtual_stack_vars_rtx,
			  trunc_int_for_mode
			  (frame_offset + bigend_correction,
			   Pmode));

  if (!FRAME_GROWS_DOWNWARD)
    frame_offset += size;

  x = gen_rtx_MEM (mode, addr);
  set_mem_align (x, alignment_in_bits);
  MEM_NOTRAP_P (x) = 1;

  stack_slot_list
    = gen_rtx_EXPR_LIST (VOIDmode, x, stack_slot_list);

  if (frame_offset_overflow (frame_offset, current_function_decl))
    frame_offset = 0;

  return x;
}

/* Wrap up assign_stack_local_1 with last parameter as false.  */

rtx
assign_stack_local (enum machine_mode mode, HOST_WIDE_INT size, int align)
{
  return assign_stack_local_1 (mode, size, align, false);
}


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

struct GTY(()) temp_slot {
  /* Points to next temporary slot.  */
  struct temp_slot *next;
  /* Points to previous temporary slot.  */
  struct temp_slot *prev;
  /* The rtx to used to reference the slot.  */
  rtx slot;
  /* The size, in units, of the slot.  */
  HOST_WIDE_INT size;
  /* The type of the object in the slot, or zero if it doesn't correspond
     to a type.  We use this to determine whether a slot can be reused.
     It can be reused if objects of the type of the new slot will always
     conflict with objects of the type of the old slot.  */
  tree type;
  /* The alignment (in bits) of the slot.  */
  unsigned int align;
  /* Nonzero if this temporary is currently in use.  */
  char in_use;
  /* Nonzero if this temporary has its address taken.  */
  char addr_taken;
  /* Nesting level at which this slot is being used.  */
  int level;
  /* Nonzero if this should survive a call to free_temp_slots.  */
  int keep;
  /* The offset of the slot from the frame_pointer, including extra space
     for alignment.  This info is for combine_temp_slots.  */
  HOST_WIDE_INT base_offset;
  /* The size of the slot, including extra space for alignment.  This
     info is for combine_temp_slots.  */
  HOST_WIDE_INT full_size;
};

/* A table of addresses that represent a stack slot.  The table is a mapping
   from address RTXen to a temp slot.  */
static GTY((param_is(struct temp_slot_address_entry))) htab_t temp_slot_address_table;

/* Entry for the above hash table.  */
struct GTY(()) temp_slot_address_entry {
  hashval_t hash;
  rtx address;
  struct temp_slot *temp_slot;
};

/* Removes temporary slot TEMP from LIST.  */

static void
cut_slot_from_list (struct temp_slot *temp, struct temp_slot **list)
{
  if (temp->next)
    temp->next->prev = temp->prev;
  if (temp->prev)
    temp->prev->next = temp->next;
  else
    *list = temp->next;

  temp->prev = temp->next = NULL;
}

/* Inserts temporary slot TEMP to LIST.  */

static void
insert_slot_to_list (struct temp_slot *temp, struct temp_slot **list)
{
  temp->next = *list;
  if (*list)
    (*list)->prev = temp;
  temp->prev = NULL;
  *list = temp;
}

/* Returns the list of used temp slots at LEVEL.  */

static struct temp_slot **
temp_slots_at_level (int level)
{
  if (level >= (int) VEC_length (temp_slot_p, used_temp_slots))
    VEC_safe_grow_cleared (temp_slot_p, gc, used_temp_slots, level + 1);

  return &(VEC_address (temp_slot_p, used_temp_slots)[level]);
}

/* Returns the maximal temporary slot level.  */

static int
max_slot_level (void)
{
  if (!used_temp_slots)
    return -1;

  return VEC_length (temp_slot_p, used_temp_slots) - 1;
}

/* Moves temporary slot TEMP to LEVEL.  */

static void
move_slot_to_level (struct temp_slot *temp, int level)
{
  cut_slot_from_list (temp, temp_slots_at_level (temp->level));
  insert_slot_to_list (temp, temp_slots_at_level (level));
  temp->level = level;
}

/* Make temporary slot TEMP available.  */

static void
make_slot_available (struct temp_slot *temp)
{
  cut_slot_from_list (temp, temp_slots_at_level (temp->level));
  insert_slot_to_list (temp, &avail_temp_slots);
  temp->in_use = 0;
  temp->level = -1;
}

/* Compute the hash value for an address -> temp slot mapping.
   The value is cached on the mapping entry.  */
static hashval_t
temp_slot_address_compute_hash (struct temp_slot_address_entry *t)
{
  int do_not_record = 0;
  return hash_rtx (t->address, GET_MODE (t->address),
		   &do_not_record, NULL, false);
}

/* Return the hash value for an address -> temp slot mapping.  */
static hashval_t
temp_slot_address_hash (const void *p)
{
  const struct temp_slot_address_entry *t;
  t = (const struct temp_slot_address_entry *) p;
  return t->hash;
}

/* Compare two address -> temp slot mapping entries.  */
static int
temp_slot_address_eq (const void *p1, const void *p2)
{
  const struct temp_slot_address_entry *t1, *t2;
  t1 = (const struct temp_slot_address_entry *) p1;
  t2 = (const struct temp_slot_address_entry *) p2;
  return exp_equiv_p (t1->address, t2->address, 0, true);
}

/* Add ADDRESS as an alias of TEMP_SLOT to the addess -> temp slot mapping.  */
static void
insert_temp_slot_address (rtx address, struct temp_slot *temp_slot)
{
  void **slot;
  struct temp_slot_address_entry *t = GGC_NEW (struct temp_slot_address_entry);
  t->address = address;
  t->temp_slot = temp_slot;
  t->hash = temp_slot_address_compute_hash (t);
  slot = htab_find_slot_with_hash (temp_slot_address_table, t, t->hash, INSERT);
  *slot = t;
}

/* Remove an address -> temp slot mapping entry if the temp slot is
   not in use anymore.  Callback for remove_unused_temp_slot_addresses.  */
static int
remove_unused_temp_slot_addresses_1 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  const struct temp_slot_address_entry *t;
  t = (const struct temp_slot_address_entry *) *slot;
  if (! t->temp_slot->in_use)
    *slot = NULL;
  return 1;
}

/* Remove all mappings of addresses to unused temp slots.  */
static void
remove_unused_temp_slot_addresses (void)
{
  htab_traverse (temp_slot_address_table,
		 remove_unused_temp_slot_addresses_1,
		 NULL);
}

/* Find the temp slot corresponding to the object at address X.  */

static struct temp_slot *
find_temp_slot_from_address (rtx x)
{
  struct temp_slot *p;
  struct temp_slot_address_entry tmp, *t;

  /* First try the easy way:
     See if X exists in the address -> temp slot mapping.  */
  tmp.address = x;
  tmp.temp_slot = NULL;
  tmp.hash = temp_slot_address_compute_hash (&tmp);
  t = (struct temp_slot_address_entry *)
    htab_find_with_hash (temp_slot_address_table, &tmp, tmp.hash);
  if (t)
    return t->temp_slot;

  /* If we have a sum involving a register, see if it points to a temp
     slot.  */
  if (GET_CODE (x) == PLUS && REG_P (XEXP (x, 0))
      && (p = find_temp_slot_from_address (XEXP (x, 0))) != 0)
    return p;
  else if (GET_CODE (x) == PLUS && REG_P (XEXP (x, 1))
	   && (p = find_temp_slot_from_address (XEXP (x, 1))) != 0)
    return p;

  /* Last resort: Address is a virtual stack var address.  */
  if (GET_CODE (x) == PLUS
      && XEXP (x, 0) == virtual_stack_vars_rtx
      && CONST_INT_P (XEXP (x, 1)))
    {
      int i;
      for (i = max_slot_level (); i >= 0; i--)
	for (p = *temp_slots_at_level (i); p; p = p->next)
	  {
	    if (INTVAL (XEXP (x, 1)) >= p->base_offset
		&& INTVAL (XEXP (x, 1)) < p->base_offset + p->full_size)
	      return p;
	  }
    }

  return NULL;
}

/* Allocate a temporary stack slot and record it for possible later
   reuse.

   MODE is the machine mode to be given to the returned rtx.

   SIZE is the size in units of the space required.  We do no rounding here
   since assign_stack_local will do any required rounding.

   KEEP is 1 if this slot is to be retained after a call to
   free_temp_slots.  Automatic variables for a block are allocated
   with this flag.  KEEP values of 2 or 3 were needed respectively
   for variables whose lifetime is controlled by CLEANUP_POINT_EXPRs
   or for SAVE_EXPRs, but they are now unused.

   TYPE is the type that will be used for the stack slot.  */

rtx
assign_stack_temp_for_type (enum machine_mode mode, HOST_WIDE_INT size,
			    int keep, tree type)
{
  unsigned int align;
  struct temp_slot *p, *best_p = 0, *selected = NULL, **pp;
  rtx slot;

  /* If SIZE is -1 it means that somebody tried to allocate a temporary
     of a variable size.  */
  gcc_assert (size != -1);

  /* These are now unused.  */
  gcc_assert (keep <= 1);

  align = get_stack_local_alignment (type, mode);

  /* Try to find an available, already-allocated temporary of the proper
     mode which meets the size and alignment requirements.  Choose the
     smallest one with the closest alignment.

     If assign_stack_temp is called outside of the tree->rtl expansion,
     we cannot reuse the stack slots (that may still refer to
     VIRTUAL_STACK_VARS_REGNUM).  */
  if (!virtuals_instantiated)
    {
      for (p = avail_temp_slots; p; p = p->next)
	{
	  if (p->align >= align && p->size >= size
	      && GET_MODE (p->slot) == mode
	      && objects_must_conflict_p (p->type, type)
	      && (best_p == 0 || best_p->size > p->size
		  || (best_p->size == p->size && best_p->align > p->align)))
	    {
	      if (p->align == align && p->size == size)
		{
		  selected = p;
		  cut_slot_from_list (selected, &avail_temp_slots);
		  best_p = 0;
		  break;
		}
	      best_p = p;
	    }
	}
    }

  /* Make our best, if any, the one to use.  */
  if (best_p)
    {
      selected = best_p;
      cut_slot_from_list (selected, &avail_temp_slots);

      /* If there are enough aligned bytes left over, make them into a new
	 temp_slot so that the extra bytes don't get wasted.  Do this only
	 for BLKmode slots, so that we can be sure of the alignment.  */
      if (GET_MODE (best_p->slot) == BLKmode)
	{
	  int alignment = best_p->align / BITS_PER_UNIT;
	  HOST_WIDE_INT rounded_size = CEIL_ROUND (size, alignment);

	  if (best_p->size - rounded_size >= alignment)
	    {
	      p = GGC_NEW (struct temp_slot);
	      p->in_use = p->addr_taken = 0;
	      p->size = best_p->size - rounded_size;
	      p->base_offset = best_p->base_offset + rounded_size;
	      p->full_size = best_p->full_size - rounded_size;
	      p->slot = adjust_address_nv (best_p->slot, BLKmode, rounded_size);
	      p->align = best_p->align;
	      p->type = best_p->type;
	      insert_slot_to_list (p, &avail_temp_slots);

	      stack_slot_list = gen_rtx_EXPR_LIST (VOIDmode, p->slot,
						   stack_slot_list);

	      best_p->size = rounded_size;
	      best_p->full_size = rounded_size;
	    }
	}
    }

  /* If we still didn't find one, make a new temporary.  */
  if (selected == 0)
    {
      HOST_WIDE_INT frame_offset_old = frame_offset;

      p = GGC_NEW (struct temp_slot);

      /* We are passing an explicit alignment request to assign_stack_local.
	 One side effect of that is assign_stack_local will not round SIZE
	 to ensure the frame offset remains suitably aligned.

	 So for requests which depended on the rounding of SIZE, we go ahead
	 and round it now.  We also make sure ALIGNMENT is at least
	 BIGGEST_ALIGNMENT.  */
      gcc_assert (mode != BLKmode || align == BIGGEST_ALIGNMENT);
      p->slot = assign_stack_local (mode,
				    (mode == BLKmode
				     ? CEIL_ROUND (size, (int) align / BITS_PER_UNIT)
				     : size),
				    align);

      p->align = align;

      /* The following slot size computation is necessary because we don't
	 know the actual size of the temporary slot until assign_stack_local
	 has performed all the frame alignment and size rounding for the
	 requested temporary.  Note that extra space added for alignment
	 can be either above or below this stack slot depending on which
	 way the frame grows.  We include the extra space if and only if it
	 is above this slot.  */
      if (FRAME_GROWS_DOWNWARD)
	p->size = frame_offset_old - frame_offset;
      else
	p->size = size;

      /* Now define the fields used by combine_temp_slots.  */
      if (FRAME_GROWS_DOWNWARD)
	{
	  p->base_offset = frame_offset;
	  p->full_size = frame_offset_old - frame_offset;
	}
      else
	{
	  p->base_offset = frame_offset_old;
	  p->full_size = frame_offset - frame_offset_old;
	}

      selected = p;
    }

  p = selected;
  p->in_use = 1;
  p->addr_taken = 0;
  p->type = type;
  p->level = temp_slot_level;
  p->keep = keep;

  pp = temp_slots_at_level (p->level);
  insert_slot_to_list (p, pp);
  insert_temp_slot_address (XEXP (p->slot, 0), p);

  /* Create a new MEM rtx to avoid clobbering MEM flags of old slots.  */
  slot = gen_rtx_MEM (mode, XEXP (p->slot, 0));
  stack_slot_list = gen_rtx_EXPR_LIST (VOIDmode, slot, stack_slot_list);

  /* If we know the alias set for the memory that will be used, use
     it.  If there's no TYPE, then we don't know anything about the
     alias set for the memory.  */
  set_mem_alias_set (slot, type ? get_alias_set (type) : 0);
  set_mem_align (slot, align);

  /* If a type is specified, set the relevant flags.  */
  if (type != 0)
    {
      MEM_VOLATILE_P (slot) = TYPE_VOLATILE (type);
      MEM_SET_IN_STRUCT_P (slot, (AGGREGATE_TYPE_P (type)
				  || TREE_CODE (type) == COMPLEX_TYPE));
    }
  MEM_NOTRAP_P (slot) = 1;

  return slot;
}

/* Allocate a temporary stack slot and record it for possible later
   reuse.  First three arguments are same as in preceding function.  */

rtx
assign_stack_temp (enum machine_mode mode, HOST_WIDE_INT size, int keep)
{
  return assign_stack_temp_for_type (mode, size, keep, NULL_TREE);
}

/* Assign a temporary.
   If TYPE_OR_DECL is a decl, then we are doing it on behalf of the decl
   and so that should be used in error messages.  In either case, we
   allocate of the given type.
   KEEP is as for assign_stack_temp.
   MEMORY_REQUIRED is 1 if the result must be addressable stack memory;
   it is 0 if a register is OK.
   DONT_PROMOTE is 1 if we should not promote values in register
   to wider modes.  */

rtx
assign_temp (tree type_or_decl, int keep, int memory_required,
	     int dont_promote ATTRIBUTE_UNUSED)
{
  tree type, decl;
  enum machine_mode mode;
#ifdef PROMOTE_MODE
  int unsignedp;
#endif

  if (DECL_P (type_or_decl))
    decl = type_or_decl, type = TREE_TYPE (decl);
  else
    decl = NULL, type = type_or_decl;

  mode = TYPE_MODE (type);
#ifdef PROMOTE_MODE
  unsignedp = TYPE_UNSIGNED (type);
#endif

  if (mode == BLKmode || memory_required)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      rtx tmp;

      /* Zero sized arrays are GNU C extension.  Set size to 1 to avoid
	 problems with allocating the stack space.  */
      if (size == 0)
	size = 1;

      /* Unfortunately, we don't yet know how to allocate variable-sized
	 temporaries.  However, sometimes we can find a fixed upper limit on
	 the size, so try that instead.  */
      else if (size == -1)
	size = max_int_size_in_bytes (type);

      /* The size of the temporary may be too large to fit into an integer.  */
      /* ??? Not sure this should happen except for user silliness, so limit
	 this to things that aren't compiler-generated temporaries.  The
	 rest of the time we'll die in assign_stack_temp_for_type.  */
      if (decl && size == -1
	  && TREE_CODE (TYPE_SIZE_UNIT (type)) == INTEGER_CST)
	{
	  error ("size of variable %q+D is too large", decl);
	  size = 1;
	}

      tmp = assign_stack_temp_for_type (mode, size, keep, type);
      return tmp;
    }

#ifdef PROMOTE_MODE
  if (! dont_promote)
    mode = promote_mode (type, mode, &unsignedp);
#endif

  return gen_reg_rtx (mode);
}

/* Combine temporary stack slots which are adjacent on the stack.

   This allows for better use of already allocated stack space.  This is only
   done for BLKmode slots because we can be sure that we won't have alignment
   problems in this case.  */

static void
combine_temp_slots (void)
{
  struct temp_slot *p, *q, *next, *next_q;
  int num_slots;

  /* We can't combine slots, because the information about which slot
     is in which alias set will be lost.  */
  if (flag_strict_aliasing)
    return;

  /* If there are a lot of temp slots, don't do anything unless
     high levels of optimization.  */
  if (! flag_expensive_optimizations)
    for (p = avail_temp_slots, num_slots = 0; p; p = p->next, num_slots++)
      if (num_slots > 100 || (num_slots > 10 && optimize == 0))
	return;

  for (p = avail_temp_slots; p; p = next)
    {
      int delete_p = 0;

      next = p->next;

      if (GET_MODE (p->slot) != BLKmode)
	continue;

      for (q = p->next; q; q = next_q)
	{
       	  int delete_q = 0;

	  next_q = q->next;

	  if (GET_MODE (q->slot) != BLKmode)
	    continue;

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
	  if (delete_q)
	    cut_slot_from_list (q, &avail_temp_slots);
	}

      /* Either delete P or advance past it.  */
      if (delete_p)
	cut_slot_from_list (p, &avail_temp_slots);
    }
}

/* Indicate that NEW_RTX is an alternate way of referring to the temp
   slot that previously was known by OLD_RTX.  */

void
update_temp_slot_address (rtx old_rtx, rtx new_rtx)
{
  struct temp_slot *p;

  if (rtx_equal_p (old_rtx, new_rtx))
    return;

  p = find_temp_slot_from_address (old_rtx);

  /* If we didn't find one, see if both OLD_RTX is a PLUS.  If so, and
     NEW_RTX is a register, see if one operand of the PLUS is a
     temporary location.  If so, NEW_RTX points into it.  Otherwise,
     if both OLD_RTX and NEW_RTX are a PLUS and if there is a register
     in common between them.  If so, try a recursive call on those
     values.  */
  if (p == 0)
    {
      if (GET_CODE (old_rtx) != PLUS)
	return;

      if (REG_P (new_rtx))
	{
	  update_temp_slot_address (XEXP (old_rtx, 0), new_rtx);
	  update_temp_slot_address (XEXP (old_rtx, 1), new_rtx);
	  return;
	}
      else if (GET_CODE (new_rtx) != PLUS)
	return;

      if (rtx_equal_p (XEXP (old_rtx, 0), XEXP (new_rtx, 0)))
	update_temp_slot_address (XEXP (old_rtx, 1), XEXP (new_rtx, 1));
      else if (rtx_equal_p (XEXP (old_rtx, 1), XEXP (new_rtx, 0)))
	update_temp_slot_address (XEXP (old_rtx, 0), XEXP (new_rtx, 1));
      else if (rtx_equal_p (XEXP (old_rtx, 0), XEXP (new_rtx, 1)))
	update_temp_slot_address (XEXP (old_rtx, 1), XEXP (new_rtx, 0));
      else if (rtx_equal_p (XEXP (old_rtx, 1), XEXP (new_rtx, 1)))
	update_temp_slot_address (XEXP (old_rtx, 0), XEXP (new_rtx, 0));

      return;
    }

  /* Otherwise add an alias for the temp's address.  */
  insert_temp_slot_address (new_rtx, p);
}

/* If X could be a reference to a temporary slot, mark the fact that its
   address was taken.  */

void
mark_temp_addr_taken (rtx x)
{
  struct temp_slot *p;

  if (x == 0)
    return;

  /* If X is not in memory or is at a constant address, it cannot be in
     a temporary slot.  */
  if (!MEM_P (x) || CONSTANT_P (XEXP (x, 0)))
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
preserve_temp_slots (rtx x)
{
  struct temp_slot *p = 0, *next;

  /* If there is no result, we still might have some objects whose address
     were taken, so we need to make sure they stay around.  */
  if (x == 0)
    {
      for (p = *temp_slots_at_level (temp_slot_level); p; p = next)
	{
	  next = p->next;

	  if (p->addr_taken)
	    move_slot_to_level (p, temp_slot_level - 1);
	}

      return;
    }

  /* If X is a register that is being used as a pointer, see if we have
     a temporary slot we know it points to.  To be consistent with
     the code below, we really should preserve all non-kept slots
     if we can't find a match, but that seems to be much too costly.  */
  if (REG_P (x) && REG_POINTER (x))
    p = find_temp_slot_from_address (x);

  /* If X is not in memory or is at a constant address, it cannot be in
     a temporary slot, but it can contain something whose address was
     taken.  */
  if (p == 0 && (!MEM_P (x) || CONSTANT_P (XEXP (x, 0))))
    {
      for (p = *temp_slots_at_level (temp_slot_level); p; p = next)
	{
	  next = p->next;

	  if (p->addr_taken)
	    move_slot_to_level (p, temp_slot_level - 1);
	}

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
	  for (q = *temp_slots_at_level (temp_slot_level); q; q = next)
	    {
	      next = q->next;

	      if (p != q && q->addr_taken)
		move_slot_to_level (q, temp_slot_level - 1);
	    }

	  move_slot_to_level (p, temp_slot_level - 1);
	  p->addr_taken = 0;
	}
      return;
    }

  /* Otherwise, preserve all non-kept slots at this level.  */
  for (p = *temp_slots_at_level (temp_slot_level); p; p = next)
    {
      next = p->next;

      if (!p->keep)
	move_slot_to_level (p, temp_slot_level - 1);
    }
}

/* Free all temporaries used so far.  This is normally called at the
   end of generating code for a statement.  */

void
free_temp_slots (void)
{
  struct temp_slot *p, *next;

  for (p = *temp_slots_at_level (temp_slot_level); p; p = next)
    {
      next = p->next;

      if (!p->keep)
	make_slot_available (p);
    }

  remove_unused_temp_slot_addresses ();
  combine_temp_slots ();
}

/* Push deeper into the nesting level for stack temporaries.  */

void
push_temp_slots (void)
{
  temp_slot_level++;
}

/* Pop a temporary nesting level.  All slots in use in the current level
   are freed.  */

void
pop_temp_slots (void)
{
  struct temp_slot *p, *next;

  for (p = *temp_slots_at_level (temp_slot_level); p; p = next)
    {
      next = p->next;
      make_slot_available (p);
    }

  remove_unused_temp_slot_addresses ();
  combine_temp_slots ();

  temp_slot_level--;
}

/* Initialize temporary slots.  */

void
init_temp_slots (void)
{
  /* We have not allocated any temporaries yet.  */
  avail_temp_slots = 0;
  used_temp_slots = 0;
  temp_slot_level = 0;

  /* Set up the table to map addresses to temp slots.  */
  if (! temp_slot_address_table)
    temp_slot_address_table = htab_create_ggc (32,
					       temp_slot_address_hash,
					       temp_slot_address_eq,
					       NULL);
  else
    htab_empty (temp_slot_address_table);
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

/* The bottom of the stack points to the actual arguments.  If
   REG_PARM_STACK_SPACE is defined, this includes the space for the register
   parameters.  However, if OUTGOING_REG_PARM_STACK space is not defined,
   stack space for register parameters is not pushed by the caller, but
   rather part of the fixed stack areas and hence not included in
   `crtl->outgoing_args_size'.  Nevertheless, we must allow
   for it when allocating stack dynamic objects.  */

#if defined(REG_PARM_STACK_SPACE)
#define STACK_DYNAMIC_OFFSET(FNDECL)	\
((ACCUMULATE_OUTGOING_ARGS						      \
  ? (crtl->outgoing_args_size				      \
     + (OUTGOING_REG_PARM_STACK_SPACE ((!(FNDECL) ? NULL_TREE : TREE_TYPE (FNDECL))) ? 0 \
					       : REG_PARM_STACK_SPACE (FNDECL))) \
  : 0) + (STACK_POINTER_OFFSET))
#else
#define STACK_DYNAMIC_OFFSET(FNDECL)	\
((ACCUMULATE_OUTGOING_ARGS ? crtl->outgoing_args_size : 0)	      \
 + (STACK_POINTER_OFFSET))
#endif
#endif


/* Given a piece of RTX and a pointer to a HOST_WIDE_INT, if the RTX
   is a virtual register, return the equivalent hard register and set the
   offset indirectly through the pointer.  Otherwise, return 0.  */

static rtx
instantiate_new_reg (rtx x, HOST_WIDE_INT *poffset)
{
  rtx new_rtx;
  HOST_WIDE_INT offset;

  if (x == virtual_incoming_args_rtx)
    {
      if (stack_realign_drap)
        {
	  /* Replace virtual_incoming_args_rtx with internal arg
	     pointer if DRAP is used to realign stack.  */
          new_rtx = crtl->args.internal_arg_pointer;
          offset = 0;
        }
      else
        new_rtx = arg_pointer_rtx, offset = in_arg_offset;
    }
  else if (x == virtual_stack_vars_rtx)
    new_rtx = frame_pointer_rtx, offset = var_offset;
  else if (x == virtual_stack_dynamic_rtx)
    new_rtx = stack_pointer_rtx, offset = dynamic_offset;
  else if (x == virtual_outgoing_args_rtx)
    new_rtx = stack_pointer_rtx, offset = out_arg_offset;
  else if (x == virtual_cfa_rtx)
    {
#ifdef FRAME_POINTER_CFA_OFFSET
      new_rtx = frame_pointer_rtx;
#else
      new_rtx = arg_pointer_rtx;
#endif
      offset = cfa_offset;
    }
  else
    return NULL_RTX;

  *poffset = offset;
  return new_rtx;
}

/* A subroutine of instantiate_virtual_regs, called via for_each_rtx.
   Instantiate any virtual registers present inside of *LOC.  The expression
   is simplified, as much as possible, but is not to be considered "valid"
   in any sense implied by the target.  If any change is made, set CHANGED
   to true.  */

static int
instantiate_virtual_regs_in_rtx (rtx *loc, void *data)
{
  HOST_WIDE_INT offset;
  bool *changed = (bool *) data;
  rtx x, new_rtx;

  x = *loc;
  if (x == 0)
    return 0;

  switch (GET_CODE (x))
    {
    case REG:
      new_rtx = instantiate_new_reg (x, &offset);
      if (new_rtx)
	{
	  *loc = plus_constant (new_rtx, offset);
	  if (changed)
	    *changed = true;
	}
      return -1;

    case PLUS:
      new_rtx = instantiate_new_reg (XEXP (x, 0), &offset);
      if (new_rtx)
	{
	  new_rtx = plus_constant (new_rtx, offset);
	  *loc = simplify_gen_binary (PLUS, GET_MODE (x), new_rtx, XEXP (x, 1));
	  if (changed)
	    *changed = true;
	  return -1;
	}

      /* FIXME -- from old code */
	  /* If we have (plus (subreg (virtual-reg)) (const_int)), we know
	     we can commute the PLUS and SUBREG because pointers into the
	     frame are well-behaved.  */
      break;

    default:
      break;
    }

  return 0;
}

/* A subroutine of instantiate_virtual_regs_in_insn.  Return true if X
   matches the predicate for insn CODE operand OPERAND.  */

static int
safe_insn_predicate (int code, int operand, rtx x)
{
  const struct insn_operand_data *op_data;

  if (code < 0)
    return true;

  op_data = &insn_data[code].operand[operand];
  if (op_data->predicate == NULL)
    return true;

  return op_data->predicate (x, op_data->mode);
}

/* A subroutine of instantiate_virtual_regs.  Instantiate any virtual
   registers present inside of insn.  The result will be a valid insn.  */

static void
instantiate_virtual_regs_in_insn (rtx insn)
{
  HOST_WIDE_INT offset;
  int insn_code, i;
  bool any_change = false;
  rtx set, new_rtx, x, seq;

  /* There are some special cases to be handled first.  */
  set = single_set (insn);
  if (set)
    {
      /* We're allowed to assign to a virtual register.  This is interpreted
	 to mean that the underlying register gets assigned the inverse
	 transformation.  This is used, for example, in the handling of
	 non-local gotos.  */
      new_rtx = instantiate_new_reg (SET_DEST (set), &offset);
      if (new_rtx)
	{
	  start_sequence ();

	  for_each_rtx (&SET_SRC (set), instantiate_virtual_regs_in_rtx, NULL);
	  x = simplify_gen_binary (PLUS, GET_MODE (new_rtx), SET_SRC (set),
				   GEN_INT (-offset));
	  x = force_operand (x, new_rtx);
	  if (x != new_rtx)
	    emit_move_insn (new_rtx, x);

	  seq = get_insns ();
	  end_sequence ();

	  emit_insn_before (seq, insn);
	  delete_insn (insn);
	  return;
	}

      /* Handle a straight copy from a virtual register by generating a
	 new add insn.  The difference between this and falling through
	 to the generic case is avoiding a new pseudo and eliminating a
	 move insn in the initial rtl stream.  */
      new_rtx = instantiate_new_reg (SET_SRC (set), &offset);
      if (new_rtx && offset != 0
	  && REG_P (SET_DEST (set))
	  && REGNO (SET_DEST (set)) > LAST_VIRTUAL_REGISTER)
	{
	  start_sequence ();

	  x = expand_simple_binop (GET_MODE (SET_DEST (set)), PLUS,
				   new_rtx, GEN_INT (offset), SET_DEST (set),
				   1, OPTAB_LIB_WIDEN);
	  if (x != SET_DEST (set))
	    emit_move_insn (SET_DEST (set), x);

	  seq = get_insns ();
	  end_sequence ();

	  emit_insn_before (seq, insn);
	  delete_insn (insn);
	  return;
	}

      extract_insn (insn);
      insn_code = INSN_CODE (insn);

      /* Handle a plus involving a virtual register by determining if the
	 operands remain valid if they're modified in place.  */
      if (GET_CODE (SET_SRC (set)) == PLUS
	  && recog_data.n_operands >= 3
	  && recog_data.operand_loc[1] == &XEXP (SET_SRC (set), 0)
	  && recog_data.operand_loc[2] == &XEXP (SET_SRC (set), 1)
	  && CONST_INT_P (recog_data.operand[2])
	  && (new_rtx = instantiate_new_reg (recog_data.operand[1], &offset)))
	{
	  offset += INTVAL (recog_data.operand[2]);

	  /* If the sum is zero, then replace with a plain move.  */
	  if (offset == 0
	      && REG_P (SET_DEST (set))
	      && REGNO (SET_DEST (set)) > LAST_VIRTUAL_REGISTER)
	    {
	      start_sequence ();
	      emit_move_insn (SET_DEST (set), new_rtx);
	      seq = get_insns ();
	      end_sequence ();

	      emit_insn_before (seq, insn);
	      delete_insn (insn);
	      return;
	    }

	  x = gen_int_mode (offset, recog_data.operand_mode[2]);

	  /* Using validate_change and apply_change_group here leaves
	     recog_data in an invalid state.  Since we know exactly what
	     we want to check, do those two by hand.  */
	  if (safe_insn_predicate (insn_code, 1, new_rtx)
	      && safe_insn_predicate (insn_code, 2, x))
	    {
	      *recog_data.operand_loc[1] = recog_data.operand[1] = new_rtx;
	      *recog_data.operand_loc[2] = recog_data.operand[2] = x;
	      any_change = true;

	      /* Fall through into the regular operand fixup loop in
		 order to take care of operands other than 1 and 2.  */
	    }
	}
    }
  else
    {
      extract_insn (insn);
      insn_code = INSN_CODE (insn);
    }

  /* In the general case, we expect virtual registers to appear only in
     operands, and then only as either bare registers or inside memories.  */
  for (i = 0; i < recog_data.n_operands; ++i)
    {
      x = recog_data.operand[i];
      switch (GET_CODE (x))
	{
	case MEM:
	  {
	    rtx addr = XEXP (x, 0);
	    bool changed = false;

	    for_each_rtx (&addr, instantiate_virtual_regs_in_rtx, &changed);
	    if (!changed)
	      continue;

	    start_sequence ();
	    x = replace_equiv_address (x, addr);
	    /* It may happen that the address with the virtual reg
	       was valid (e.g. based on the virtual stack reg, which might
	       be acceptable to the predicates with all offsets), whereas
	       the address now isn't anymore, for instance when the address
	       is still offsetted, but the base reg isn't virtual-stack-reg
	       anymore.  Below we would do a force_reg on the whole operand,
	       but this insn might actually only accept memory.  Hence,
	       before doing that last resort, try to reload the address into
	       a register, so this operand stays a MEM.  */
	    if (!safe_insn_predicate (insn_code, i, x))
	      {
		addr = force_reg (GET_MODE (addr), addr);
		x = replace_equiv_address (x, addr);
	      }
	    seq = get_insns ();
	    end_sequence ();
	    if (seq)
	      emit_insn_before (seq, insn);
	  }
	  break;

	case REG:
	  new_rtx = instantiate_new_reg (x, &offset);
	  if (new_rtx == NULL)
	    continue;
	  if (offset == 0)
	    x = new_rtx;
	  else
	    {
	      start_sequence ();

	      /* Careful, special mode predicates may have stuff in
		 insn_data[insn_code].operand[i].mode that isn't useful
		 to us for computing a new value.  */
	      /* ??? Recognize address_operand and/or "p" constraints
		 to see if (plus new offset) is a valid before we put
		 this through expand_simple_binop.  */
	      x = expand_simple_binop (GET_MODE (x), PLUS, new_rtx,
				       GEN_INT (offset), NULL_RTX,
				       1, OPTAB_LIB_WIDEN);
	      seq = get_insns ();
	      end_sequence ();
	      emit_insn_before (seq, insn);
	    }
	  break;

	case SUBREG:
	  new_rtx = instantiate_new_reg (SUBREG_REG (x), &offset);
	  if (new_rtx == NULL)
	    continue;
	  if (offset != 0)
	    {
	      start_sequence ();
	      new_rtx = expand_simple_binop (GET_MODE (new_rtx), PLUS, new_rtx,
					 GEN_INT (offset), NULL_RTX,
					 1, OPTAB_LIB_WIDEN);
	      seq = get_insns ();
	      end_sequence ();
	      emit_insn_before (seq, insn);
	    }
	  x = simplify_gen_subreg (recog_data.operand_mode[i], new_rtx,
				   GET_MODE (new_rtx), SUBREG_BYTE (x));
	  gcc_assert (x);
	  break;

	default:
	  continue;
	}

      /* At this point, X contains the new value for the operand.
	 Validate the new value vs the insn predicate.  Note that
	 asm insns will have insn_code -1 here.  */
      if (!safe_insn_predicate (insn_code, i, x))
	{
	  start_sequence ();
	  if (REG_P (x))
	    {
	      gcc_assert (REGNO (x) <= LAST_VIRTUAL_REGISTER);
	      x = copy_to_reg (x);
	    }
	  else
	    x = force_reg (insn_data[insn_code].operand[i].mode, x);
	  seq = get_insns ();
	  end_sequence ();
	  if (seq)
	    emit_insn_before (seq, insn);
	}

      *recog_data.operand_loc[i] = recog_data.operand[i] = x;
      any_change = true;
    }

  if (any_change)
    {
      /* Propagate operand changes into the duplicates.  */
      for (i = 0; i < recog_data.n_dups; ++i)
	*recog_data.dup_loc[i]
	  = copy_rtx (recog_data.operand[(unsigned)recog_data.dup_num[i]]);

      /* Force re-recognition of the instruction for validation.  */
      INSN_CODE (insn) = -1;
    }

  if (asm_noperands (PATTERN (insn)) >= 0)
    {
      if (!check_asm_operands (PATTERN (insn)))
	{
	  error_for_asm (insn, "impossible constraint in %<asm%>");
	  delete_insn (insn);
	}
    }
  else
    {
      if (recog_memoized (insn) < 0)
	fatal_insn_not_found (insn);
    }
}

/* Subroutine of instantiate_decls.  Given RTL representing a decl,
   do any instantiation required.  */

void
instantiate_decl_rtl (rtx x)
{
  rtx addr;

  if (x == 0)
    return;

  /* If this is a CONCAT, recurse for the pieces.  */
  if (GET_CODE (x) == CONCAT)
    {
      instantiate_decl_rtl (XEXP (x, 0));
      instantiate_decl_rtl (XEXP (x, 1));
      return;
    }

  /* If this is not a MEM, no need to do anything.  Similarly if the
     address is a constant or a register that is not a virtual register.  */
  if (!MEM_P (x))
    return;

  addr = XEXP (x, 0);
  if (CONSTANT_P (addr)
      || (REG_P (addr)
	  && (REGNO (addr) < FIRST_VIRTUAL_REGISTER
	      || REGNO (addr) > LAST_VIRTUAL_REGISTER)))
    return;

  for_each_rtx (&XEXP (x, 0), instantiate_virtual_regs_in_rtx, NULL);
}

/* Helper for instantiate_decls called via walk_tree: Process all decls
   in the given DECL_VALUE_EXPR.  */

static tree
instantiate_expr (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;
  if (! EXPR_P (t))
    {
      *walk_subtrees = 0;
      if (DECL_P (t) && DECL_RTL_SET_P (t))
	instantiate_decl_rtl (DECL_RTL (t));
    }
  return NULL;
}

/* Subroutine of instantiate_decls: Process all decls in the given
   BLOCK node and all its subblocks.  */

static void
instantiate_decls_1 (tree let)
{
  tree t;

  for (t = BLOCK_VARS (let); t; t = TREE_CHAIN (t))
    {
      if (DECL_RTL_SET_P (t))
	instantiate_decl_rtl (DECL_RTL (t));
      if (TREE_CODE (t) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (t))
	{
	  tree v = DECL_VALUE_EXPR (t);
	  walk_tree (&v, instantiate_expr, NULL, NULL);
	}
    }

  /* Process all subblocks.  */
  for (t = BLOCK_SUBBLOCKS (let); t; t = BLOCK_CHAIN (t))
    instantiate_decls_1 (t);
}

/* Scan all decls in FNDECL (both variables and parameters) and instantiate
   all virtual registers in their DECL_RTL's.  */

static void
instantiate_decls (tree fndecl)
{
  tree decl, t, next;

  /* Process all parameters of the function.  */
  for (decl = DECL_ARGUMENTS (fndecl); decl; decl = TREE_CHAIN (decl))
    {
      instantiate_decl_rtl (DECL_RTL (decl));
      instantiate_decl_rtl (DECL_INCOMING_RTL (decl));
      if (DECL_HAS_VALUE_EXPR_P (decl))
	{
	  tree v = DECL_VALUE_EXPR (decl);
	  walk_tree (&v, instantiate_expr, NULL, NULL);
	}
    }

  /* Now process all variables defined in the function or its subblocks.  */
  instantiate_decls_1 (DECL_INITIAL (fndecl));

  t = cfun->local_decls;
  cfun->local_decls = NULL_TREE;
  for (; t; t = next)
    {
      next = TREE_CHAIN (t);
      decl = TREE_VALUE (t);
      if (DECL_RTL_SET_P (decl))
	instantiate_decl_rtl (DECL_RTL (decl));
      ggc_free (t);
    }
}

/* Pass through the INSNS of function FNDECL and convert virtual register
   references to hard register references.  */

static unsigned int
instantiate_virtual_regs (void)
{
  rtx insn;

  /* Compute the offsets to use for this function.  */
  in_arg_offset = FIRST_PARM_OFFSET (current_function_decl);
  var_offset = STARTING_FRAME_OFFSET;
  dynamic_offset = STACK_DYNAMIC_OFFSET (current_function_decl);
  out_arg_offset = STACK_POINTER_OFFSET;
#ifdef FRAME_POINTER_CFA_OFFSET
  cfa_offset = FRAME_POINTER_CFA_OFFSET (current_function_decl);
#else
  cfa_offset = ARG_POINTER_CFA_OFFSET (current_function_decl);
#endif

  /* Initialize recognition, indicating that volatile is OK.  */
  init_recog ();

  /* Scan through all the insns, instantiating every virtual register still
     present.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	/* These patterns in the instruction stream can never be recognized.
	   Fortunately, they shouldn't contain virtual registers either.  */
	if (GET_CODE (PATTERN (insn)) == USE
	    || GET_CODE (PATTERN (insn)) == CLOBBER
	    || GET_CODE (PATTERN (insn)) == ADDR_VEC
	    || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC
	    || GET_CODE (PATTERN (insn)) == ASM_INPUT)
	  continue;
	else if (DEBUG_INSN_P (insn))
	  for_each_rtx (&INSN_VAR_LOCATION (insn),
			instantiate_virtual_regs_in_rtx, NULL);
	else
	  instantiate_virtual_regs_in_insn (insn);

	if (INSN_DELETED_P (insn))
	  continue;

	for_each_rtx (&REG_NOTES (insn), instantiate_virtual_regs_in_rtx, NULL);

	/* Instantiate any virtual registers in CALL_INSN_FUNCTION_USAGE.  */
	if (CALL_P (insn))
	  for_each_rtx (&CALL_INSN_FUNCTION_USAGE (insn),
			instantiate_virtual_regs_in_rtx, NULL);
      }

  /* Instantiate the virtual registers in the DECLs for debugging purposes.  */
  instantiate_decls (current_function_decl);

  targetm.instantiate_decls ();

  /* Indicate that, from now on, assign_stack_local should use
     frame_pointer_rtx.  */
  virtuals_instantiated = 1;
  return 0;
}

struct rtl_opt_pass pass_instantiate_virtual_regs =
{
 {
  RTL_PASS,
  "vregs",                              /* name */
  NULL,                                 /* gate */
  instantiate_virtual_regs,             /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func                        /* todo_flags_finish */
 }
};


/* Return 1 if EXP is an aggregate type (or a value with aggregate type).
   This means a type for which function calls must pass an address to the
   function or get an address back from the function.
   EXP may be a type node or an expression (whose type is tested).  */

int
aggregate_value_p (const_tree exp, const_tree fntype)
{
  int i, regno, nregs;
  rtx reg;

  const_tree type = (TYPE_P (exp)) ? exp : TREE_TYPE (exp);

  /* DECL node associated with FNTYPE when relevant, which we might need to
     check for by-invisible-reference returns, typically for CALL_EXPR input
     EXPressions.  */
  const_tree fndecl = NULL_TREE;

  if (fntype)
    switch (TREE_CODE (fntype))
      {
      case CALL_EXPR:
	fndecl = get_callee_fndecl (fntype);
	fntype = (fndecl
		  ? TREE_TYPE (fndecl)
		  : TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (fntype))));
	break;
      case FUNCTION_DECL:
	fndecl = fntype;
	fntype = TREE_TYPE (fndecl);
	break;
      case FUNCTION_TYPE:
      case METHOD_TYPE:
        break;
      case IDENTIFIER_NODE:
	fntype = 0;
	break;
      default:
	/* We don't expect other rtl types here.  */
	gcc_unreachable ();
      }

  if (TREE_CODE (type) == VOID_TYPE)
    return 0;

  /* If the front end has decided that this needs to be passed by
     reference, do so.  */
  if ((TREE_CODE (exp) == PARM_DECL || TREE_CODE (exp) == RESULT_DECL)
      && DECL_BY_REFERENCE (exp))
    return 1;

  /* If the EXPression is a CALL_EXPR, honor DECL_BY_REFERENCE set on the
     called function RESULT_DECL, meaning the function returns in memory by
     invisible reference.  This check lets front-ends not set TREE_ADDRESSABLE
     on the function type, which used to be the way to request such a return
     mechanism but might now be causing troubles at gimplification time if
     temporaries with the function type need to be created.  */
  if (TREE_CODE (exp) == CALL_EXPR && fndecl && DECL_RESULT (fndecl)
      && DECL_BY_REFERENCE (DECL_RESULT (fndecl)))
    return 1;

  if (targetm.calls.return_in_memory (type, fntype))
    return 1;
  /* Types that are TREE_ADDRESSABLE must be constructed in memory,
     and thus can't be returned in registers.  */
  if (TREE_ADDRESSABLE (type))
    return 1;
  if (flag_pcc_struct_return && AGGREGATE_TYPE_P (type))
    return 1;
  /* Make sure we have suitable call-clobbered regs to return
     the value in; if not, we must return it in memory.  */
  reg = hard_function_value (type, 0, fntype, 0);

  /* If we have something other than a REG (e.g. a PARALLEL), then assume
     it is OK.  */
  if (!REG_P (reg))
    return 0;

  regno = REGNO (reg);
  nregs = hard_regno_nregs[regno][TYPE_MODE (type)];
  for (i = 0; i < nregs; i++)
    if (! call_used_regs[regno + i])
      return 1;
  return 0;
}

/* Return true if we should assign DECL a pseudo register; false if it
   should live on the local stack.  */

bool
use_register_for_decl (const_tree decl)
{
  if (!targetm.calls.allocate_stack_slots_for_args())
    return true;

  /* Honor volatile.  */
  if (TREE_SIDE_EFFECTS (decl))
    return false;

  /* Honor addressability.  */
  if (TREE_ADDRESSABLE (decl))
    return false;

  /* Only register-like things go in registers.  */
  if (DECL_MODE (decl) == BLKmode)
    return false;

  /* If -ffloat-store specified, don't put explicit float variables
     into registers.  */
  /* ??? This should be checked after DECL_ARTIFICIAL, but tree-ssa
     propagates values across these stores, and it probably shouldn't.  */
  if (flag_float_store && FLOAT_TYPE_P (TREE_TYPE (decl)))
    return false;

  /* If we're not interested in tracking debugging information for
     this decl, then we can certainly put it in a register.  */
  if (DECL_IGNORED_P (decl))
    return true;

  if (optimize)
    return true;

  if (!DECL_REGISTER (decl))
    return false;

  switch (TREE_CODE (TREE_TYPE (decl)))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* When not optimizing, disregard register keyword for variables with
	 types containing methods, otherwise the methods won't be callable
	 from the debugger.  */
      if (TYPE_METHODS (TREE_TYPE (decl)))
	return false;
      break;
    default:
      break;
    }

  return true;
}

/* Return true if TYPE should be passed by invisible reference.  */

bool
pass_by_reference (CUMULATIVE_ARGS *ca, enum machine_mode mode,
		   tree type, bool named_arg)
{
  if (type)
    {
      /* If this type contains non-trivial constructors, then it is
	 forbidden for the middle-end to create any new copies.  */
      if (TREE_ADDRESSABLE (type))
	return true;

      /* GCC post 3.4 passes *all* variable sized types by reference.  */
      if (!TYPE_SIZE (type) || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	return true;
    }

  return targetm.calls.pass_by_reference (ca, mode, type, named_arg);
}

/* Return true if TYPE, which is passed by reference, should be callee
   copied instead of caller copied.  */

bool
reference_callee_copied (CUMULATIVE_ARGS *ca, enum machine_mode mode,
			 tree type, bool named_arg)
{
  if (type && TREE_ADDRESSABLE (type))
    return false;
  return targetm.calls.callee_copies (ca, mode, type, named_arg);
}

/* Structures to communicate between the subroutines of assign_parms.
   The first holds data persistent across all parameters, the second
   is cleared out for each parameter.  */

struct assign_parm_data_all
{
  CUMULATIVE_ARGS args_so_far;
  struct args_size stack_args_size;
  tree function_result_decl;
  tree orig_fnargs;
  rtx first_conversion_insn;
  rtx last_conversion_insn;
  HOST_WIDE_INT pretend_args_size;
  HOST_WIDE_INT extra_pretend_bytes;
  int reg_parm_stack_space;
};

struct assign_parm_data_one
{
  tree nominal_type;
  tree passed_type;
  rtx entry_parm;
  rtx stack_parm;
  enum machine_mode nominal_mode;
  enum machine_mode passed_mode;
  enum machine_mode promoted_mode;
  struct locate_and_pad_arg_data locate;
  int partial;
  BOOL_BITFIELD named_arg : 1;
  BOOL_BITFIELD passed_pointer : 1;
  BOOL_BITFIELD on_stack : 1;
  BOOL_BITFIELD loaded_in_reg : 1;
};

/* A subroutine of assign_parms.  Initialize ALL.  */

static void
assign_parms_initialize_all (struct assign_parm_data_all *all)
{
  tree fntype;

  memset (all, 0, sizeof (*all));

  fntype = TREE_TYPE (current_function_decl);

#ifdef INIT_CUMULATIVE_INCOMING_ARGS
  INIT_CUMULATIVE_INCOMING_ARGS (all->args_so_far, fntype, NULL_RTX);
#else
  INIT_CUMULATIVE_ARGS (all->args_so_far, fntype, NULL_RTX,
			current_function_decl, -1);
#endif

#ifdef REG_PARM_STACK_SPACE
  all->reg_parm_stack_space = REG_PARM_STACK_SPACE (current_function_decl);
#endif
}

/* If ARGS contains entries with complex types, split the entry into two
   entries of the component type.  Return a new list of substitutions are
   needed, else the old list.  */

static tree
split_complex_args (tree args)
{
  tree p;

  /* Before allocating memory, check for the common case of no complex.  */
  for (p = args; p; p = TREE_CHAIN (p))
    {
      tree type = TREE_TYPE (p);
      if (TREE_CODE (type) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (type))
        goto found;
    }
  return args;

 found:
  args = copy_list (args);

  for (p = args; p; p = TREE_CHAIN (p))
    {
      tree type = TREE_TYPE (p);
      if (TREE_CODE (type) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (type))
	{
	  tree decl;
	  tree subtype = TREE_TYPE (type);
	  bool addressable = TREE_ADDRESSABLE (p);

	  /* Rewrite the PARM_DECL's type with its component.  */
	  TREE_TYPE (p) = subtype;
	  DECL_ARG_TYPE (p) = TREE_TYPE (DECL_ARG_TYPE (p));
	  DECL_MODE (p) = VOIDmode;
	  DECL_SIZE (p) = NULL;
	  DECL_SIZE_UNIT (p) = NULL;
	  /* If this arg must go in memory, put it in a pseudo here.
	     We can't allow it to go in memory as per normal parms,
	     because the usual place might not have the imag part
	     adjacent to the real part.  */
	  DECL_ARTIFICIAL (p) = addressable;
	  DECL_IGNORED_P (p) = addressable;
	  TREE_ADDRESSABLE (p) = 0;
	  layout_decl (p, 0);

	  /* Build a second synthetic decl.  */
	  decl = build_decl (EXPR_LOCATION (p),
			     PARM_DECL, NULL_TREE, subtype);
	  DECL_ARG_TYPE (decl) = DECL_ARG_TYPE (p);
	  DECL_ARTIFICIAL (decl) = addressable;
	  DECL_IGNORED_P (decl) = addressable;
	  layout_decl (decl, 0);

	  /* Splice it in; skip the new decl.  */
	  TREE_CHAIN (decl) = TREE_CHAIN (p);
	  TREE_CHAIN (p) = decl;
	  p = decl;
	}
    }

  return args;
}

/* A subroutine of assign_parms.  Adjust the parameter list to incorporate
   the hidden struct return argument, and (abi willing) complex args.
   Return the new parameter list.  */

static tree
assign_parms_augmented_arg_list (struct assign_parm_data_all *all)
{
  tree fndecl = current_function_decl;
  tree fntype = TREE_TYPE (fndecl);
  tree fnargs = DECL_ARGUMENTS (fndecl);

  /* If struct value address is treated as the first argument, make it so.  */
  if (aggregate_value_p (DECL_RESULT (fndecl), fndecl)
      && ! cfun->returns_pcc_struct
      && targetm.calls.struct_value_rtx (TREE_TYPE (fndecl), 1) == 0)
    {
      tree type = build_pointer_type (TREE_TYPE (fntype));
      tree decl;

      decl = build_decl (DECL_SOURCE_LOCATION (fndecl),
			 PARM_DECL, NULL_TREE, type);
      DECL_ARG_TYPE (decl) = type;
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;

      TREE_CHAIN (decl) = fnargs;
      fnargs = decl;
      all->function_result_decl = decl;
    }

  all->orig_fnargs = fnargs;

  /* If the target wants to split complex arguments into scalars, do so.  */
  if (targetm.calls.split_complex_arg)
    fnargs = split_complex_args (fnargs);

  return fnargs;
}

/* A subroutine of assign_parms.  Examine PARM and pull out type and mode
   data for the parameter.  Incorporate ABI specifics such as pass-by-
   reference and type promotion.  */

static void
assign_parm_find_data_types (struct assign_parm_data_all *all, tree parm,
			     struct assign_parm_data_one *data)
{
  tree nominal_type, passed_type;
  enum machine_mode nominal_mode, passed_mode, promoted_mode;
  int unsignedp;

  memset (data, 0, sizeof (*data));

  /* NAMED_ARG is a misnomer.  We really mean 'non-variadic'. */
  if (!cfun->stdarg)
    data->named_arg = 1;  /* No variadic parms.  */
  else if (TREE_CHAIN (parm))
    data->named_arg = 1;  /* Not the last non-variadic parm. */
  else if (targetm.calls.strict_argument_naming (&all->args_so_far))
    data->named_arg = 1;  /* Only variadic ones are unnamed.  */
  else
    data->named_arg = 0;  /* Treat as variadic.  */

  nominal_type = TREE_TYPE (parm);
  passed_type = DECL_ARG_TYPE (parm);

  /* Look out for errors propagating this far.  Also, if the parameter's
     type is void then its value doesn't matter.  */
  if (TREE_TYPE (parm) == error_mark_node
      /* This can happen after weird syntax errors
	 or if an enum type is defined among the parms.  */
      || TREE_CODE (parm) != PARM_DECL
      || passed_type == NULL
      || VOID_TYPE_P (nominal_type))
    {
      nominal_type = passed_type = void_type_node;
      nominal_mode = passed_mode = promoted_mode = VOIDmode;
      goto egress;
    }

  /* Find mode of arg as it is passed, and mode of arg as it should be
     during execution of this function.  */
  passed_mode = TYPE_MODE (passed_type);
  nominal_mode = TYPE_MODE (nominal_type);

  /* If the parm is to be passed as a transparent union, use the type of
     the first field for the tests below.  We have already verified that
     the modes are the same.  */
  if (TREE_CODE (passed_type) == UNION_TYPE
      && TYPE_TRANSPARENT_UNION (passed_type))
    passed_type = TREE_TYPE (TYPE_FIELDS (passed_type));

  /* See if this arg was passed by invisible reference.  */
  if (pass_by_reference (&all->args_so_far, passed_mode,
			 passed_type, data->named_arg))
    {
      passed_type = nominal_type = build_pointer_type (passed_type);
      data->passed_pointer = true;
      passed_mode = nominal_mode = Pmode;
    }

  /* Find mode as it is passed by the ABI.  */
  unsignedp = TYPE_UNSIGNED (passed_type);
  promoted_mode = promote_function_mode (passed_type, passed_mode, &unsignedp,
				         TREE_TYPE (current_function_decl), 0);

 egress:
  data->nominal_type = nominal_type;
  data->passed_type = passed_type;
  data->nominal_mode = nominal_mode;
  data->passed_mode = passed_mode;
  data->promoted_mode = promoted_mode;
}

/* A subroutine of assign_parms.  Invoke setup_incoming_varargs.  */

static void
assign_parms_setup_varargs (struct assign_parm_data_all *all,
			    struct assign_parm_data_one *data, bool no_rtl)
{
  int varargs_pretend_bytes = 0;

  targetm.calls.setup_incoming_varargs (&all->args_so_far,
					data->promoted_mode,
					data->passed_type,
					&varargs_pretend_bytes, no_rtl);

  /* If the back-end has requested extra stack space, record how much is
     needed.  Do not change pretend_args_size otherwise since it may be
     nonzero from an earlier partial argument.  */
  if (varargs_pretend_bytes > 0)
    all->pretend_args_size = varargs_pretend_bytes;
}

/* A subroutine of assign_parms.  Set DATA->ENTRY_PARM corresponding to
   the incoming location of the current parameter.  */

static void
assign_parm_find_entry_rtl (struct assign_parm_data_all *all,
			    struct assign_parm_data_one *data)
{
  HOST_WIDE_INT pretend_bytes = 0;
  rtx entry_parm;
  bool in_regs;

  if (data->promoted_mode == VOIDmode)
    {
      data->entry_parm = data->stack_parm = const0_rtx;
      return;
    }

#ifdef FUNCTION_INCOMING_ARG
  entry_parm = FUNCTION_INCOMING_ARG (all->args_so_far, data->promoted_mode,
				      data->passed_type, data->named_arg);
#else
  entry_parm = FUNCTION_ARG (all->args_so_far, data->promoted_mode,
			     data->passed_type, data->named_arg);
#endif

  if (entry_parm == 0)
    data->promoted_mode = data->passed_mode;

  /* Determine parm's home in the stack, in case it arrives in the stack
     or we should pretend it did.  Compute the stack position and rtx where
     the argument arrives and its size.

     There is one complexity here:  If this was a parameter that would
     have been passed in registers, but wasn't only because it is
     __builtin_va_alist, we want locate_and_pad_parm to treat it as if
     it came in a register so that REG_PARM_STACK_SPACE isn't skipped.
     In this case, we call FUNCTION_ARG with NAMED set to 1 instead of 0
     as it was the previous time.  */
  in_regs = entry_parm != 0;
#ifdef STACK_PARMS_IN_REG_PARM_AREA
  in_regs = true;
#endif
  if (!in_regs && !data->named_arg)
    {
      if (targetm.calls.pretend_outgoing_varargs_named (&all->args_so_far))
	{
	  rtx tem;
#ifdef FUNCTION_INCOMING_ARG
	  tem = FUNCTION_INCOMING_ARG (all->args_so_far, data->promoted_mode,
				       data->passed_type, true);
#else
	  tem = FUNCTION_ARG (all->args_so_far, data->promoted_mode,
			      data->passed_type, true);
#endif
	  in_regs = tem != NULL;
	}
    }

  /* If this parameter was passed both in registers and in the stack, use
     the copy on the stack.  */
  if (targetm.calls.must_pass_in_stack (data->promoted_mode,
					data->passed_type))
    entry_parm = 0;

  if (entry_parm)
    {
      int partial;

      partial = targetm.calls.arg_partial_bytes (&all->args_so_far,
						 data->promoted_mode,
						 data->passed_type,
						 data->named_arg);
      data->partial = partial;

      /* The caller might already have allocated stack space for the
	 register parameters.  */
      if (partial != 0 && all->reg_parm_stack_space == 0)
	{
	  /* Part of this argument is passed in registers and part
	     is passed on the stack.  Ask the prologue code to extend
	     the stack part so that we can recreate the full value.

	     PRETEND_BYTES is the size of the registers we need to store.
	     CURRENT_FUNCTION_PRETEND_ARGS_SIZE is the amount of extra
	     stack space that the prologue should allocate.

	     Internally, gcc assumes that the argument pointer is aligned
	     to STACK_BOUNDARY bits.  This is used both for alignment
	     optimizations (see init_emit) and to locate arguments that are
	     aligned to more than PARM_BOUNDARY bits.  We must preserve this
	     invariant by rounding CURRENT_FUNCTION_PRETEND_ARGS_SIZE up to
	     a stack boundary.  */

	  /* We assume at most one partial arg, and it must be the first
	     argument on the stack.  */
	  gcc_assert (!all->extra_pretend_bytes && !all->pretend_args_size);

	  pretend_bytes = partial;
	  all->pretend_args_size = CEIL_ROUND (pretend_bytes, STACK_BYTES);

	  /* We want to align relative to the actual stack pointer, so
	     don't include this in the stack size until later.  */
	  all->extra_pretend_bytes = all->pretend_args_size;
	}
    }

  locate_and_pad_parm (data->promoted_mode, data->passed_type, in_regs,
		       entry_parm ? data->partial : 0, current_function_decl,
		       &all->stack_args_size, &data->locate);

  /* Update parm_stack_boundary if this parameter is passed in the
     stack.  */
  if (!in_regs && crtl->parm_stack_boundary < data->locate.boundary)
    crtl->parm_stack_boundary = data->locate.boundary;

  /* Adjust offsets to include the pretend args.  */
  pretend_bytes = all->extra_pretend_bytes - pretend_bytes;
  data->locate.slot_offset.constant += pretend_bytes;
  data->locate.offset.constant += pretend_bytes;

  data->entry_parm = entry_parm;
}

/* A subroutine of assign_parms.  If there is actually space on the stack
   for this parm, count it in stack_args_size and return true.  */

static bool
assign_parm_is_stack_parm (struct assign_parm_data_all *all,
			   struct assign_parm_data_one *data)
{
  /* Trivially true if we've no incoming register.  */
  if (data->entry_parm == NULL)
    ;
  /* Also true if we're partially in registers and partially not,
     since we've arranged to drop the entire argument on the stack.  */
  else if (data->partial != 0)
    ;
  /* Also true if the target says that it's passed in both registers
     and on the stack.  */
  else if (GET_CODE (data->entry_parm) == PARALLEL
	   && XEXP (XVECEXP (data->entry_parm, 0, 0), 0) == NULL_RTX)
    ;
  /* Also true if the target says that there's stack allocated for
     all register parameters.  */
  else if (all->reg_parm_stack_space > 0)
    ;
  /* Otherwise, no, this parameter has no ABI defined stack slot.  */
  else
    return false;

  all->stack_args_size.constant += data->locate.size.constant;
  if (data->locate.size.var)
    ADD_PARM_SIZE (all->stack_args_size, data->locate.size.var);

  return true;
}

/* A subroutine of assign_parms.  Given that this parameter is allocated
   stack space by the ABI, find it.  */

static void
assign_parm_find_stack_rtl (tree parm, struct assign_parm_data_one *data)
{
  rtx offset_rtx, stack_parm;
  unsigned int align, boundary;

  /* If we're passing this arg using a reg, make its stack home the
     aligned stack slot.  */
  if (data->entry_parm)
    offset_rtx = ARGS_SIZE_RTX (data->locate.slot_offset);
  else
    offset_rtx = ARGS_SIZE_RTX (data->locate.offset);

  stack_parm = crtl->args.internal_arg_pointer;
  if (offset_rtx != const0_rtx)
    stack_parm = gen_rtx_PLUS (Pmode, stack_parm, offset_rtx);
  stack_parm = gen_rtx_MEM (data->promoted_mode, stack_parm);

  if (!data->passed_pointer)
    {
      set_mem_attributes (stack_parm, parm, 1);
      /* set_mem_attributes could set MEM_SIZE to the passed mode's size,
	 while promoted mode's size is needed.  */
      if (data->promoted_mode != BLKmode
	  && data->promoted_mode != DECL_MODE (parm))
	{
	  set_mem_size (stack_parm,
			GEN_INT (GET_MODE_SIZE (data->promoted_mode)));
	  if (MEM_EXPR (stack_parm) && MEM_OFFSET (stack_parm))
	    {
	      int offset = subreg_lowpart_offset (DECL_MODE (parm),
						  data->promoted_mode);
	      if (offset)
		set_mem_offset (stack_parm,
				plus_constant (MEM_OFFSET (stack_parm),
					       -offset));
	    }
	}
    }

  boundary = data->locate.boundary;
  align = BITS_PER_UNIT;

  /* If we're padding upward, we know that the alignment of the slot
     is FUNCTION_ARG_BOUNDARY.  If we're using slot_offset, we're
     intentionally forcing upward padding.  Otherwise we have to come
     up with a guess at the alignment based on OFFSET_RTX.  */
  if (data->locate.where_pad != downward || data->entry_parm)
    align = boundary;
  else if (CONST_INT_P (offset_rtx))
    {
      align = INTVAL (offset_rtx) * BITS_PER_UNIT | boundary;
      align = align & -align;
    }
  set_mem_align (stack_parm, align);

  if (data->entry_parm)
    set_reg_attrs_for_parm (data->entry_parm, stack_parm);

  data->stack_parm = stack_parm;
}

/* A subroutine of assign_parms.  Adjust DATA->ENTRY_RTL such that it's
   always valid and contiguous.  */

static void
assign_parm_adjust_entry_rtl (struct assign_parm_data_one *data)
{
  rtx entry_parm = data->entry_parm;
  rtx stack_parm = data->stack_parm;

  /* If this parm was passed part in regs and part in memory, pretend it
     arrived entirely in memory by pushing the register-part onto the stack.
     In the special case of a DImode or DFmode that is split, we could put
     it together in a pseudoreg directly, but for now that's not worth
     bothering with.  */
  if (data->partial != 0)
    {
      /* Handle calls that pass values in multiple non-contiguous
	 locations.  The Irix 6 ABI has examples of this.  */
      if (GET_CODE (entry_parm) == PARALLEL)
	emit_group_store (validize_mem (stack_parm), entry_parm,
			  data->passed_type,
			  int_size_in_bytes (data->passed_type));
      else
	{
	  gcc_assert (data->partial % UNITS_PER_WORD == 0);
	  move_block_from_reg (REGNO (entry_parm), validize_mem (stack_parm),
			       data->partial / UNITS_PER_WORD);
	}

      entry_parm = stack_parm;
    }

  /* If we didn't decide this parm came in a register, by default it came
     on the stack.  */
  else if (entry_parm == NULL)
    entry_parm = stack_parm;

  /* When an argument is passed in multiple locations, we can't make use
     of this information, but we can save some copying if the whole argument
     is passed in a single register.  */
  else if (GET_CODE (entry_parm) == PARALLEL
	   && data->nominal_mode != BLKmode
	   && data->passed_mode != BLKmode)
    {
      size_t i, len = XVECLEN (entry_parm, 0);

      for (i = 0; i < len; i++)
	if (XEXP (XVECEXP (entry_parm, 0, i), 0) != NULL_RTX
	    && REG_P (XEXP (XVECEXP (entry_parm, 0, i), 0))
	    && (GET_MODE (XEXP (XVECEXP (entry_parm, 0, i), 0))
		== data->passed_mode)
	    && INTVAL (XEXP (XVECEXP (entry_parm, 0, i), 1)) == 0)
	  {
	    entry_parm = XEXP (XVECEXP (entry_parm, 0, i), 0);
	    break;
	  }
    }

  data->entry_parm = entry_parm;
}

/* A subroutine of assign_parms.  Reconstitute any values which were
   passed in multiple registers and would fit in a single register.  */

static void
assign_parm_remove_parallels (struct assign_parm_data_one *data)
{
  rtx entry_parm = data->entry_parm;

  /* Convert the PARALLEL to a REG of the same mode as the parallel.
     This can be done with register operations rather than on the
     stack, even if we will store the reconstituted parameter on the
     stack later.  */
  if (GET_CODE (entry_parm) == PARALLEL && GET_MODE (entry_parm) != BLKmode)
    {
      rtx parmreg = gen_reg_rtx (GET_MODE (entry_parm));
      emit_group_store (parmreg, entry_parm, data->passed_type,
			GET_MODE_SIZE (GET_MODE (entry_parm)));
      entry_parm = parmreg;
    }

  data->entry_parm = entry_parm;
}

/* A subroutine of assign_parms.  Adjust DATA->STACK_RTL such that it's
   always valid and properly aligned.  */

static void
assign_parm_adjust_stack_rtl (struct assign_parm_data_one *data)
{
  rtx stack_parm = data->stack_parm;

  /* If we can't trust the parm stack slot to be aligned enough for its
     ultimate type, don't use that slot after entry.  We'll make another
     stack slot, if we need one.  */
  if (stack_parm
      && ((STRICT_ALIGNMENT
	   && GET_MODE_ALIGNMENT (data->nominal_mode) > MEM_ALIGN (stack_parm))
	  || (data->nominal_type
	      && TYPE_ALIGN (data->nominal_type) > MEM_ALIGN (stack_parm)
	      && MEM_ALIGN (stack_parm) < PREFERRED_STACK_BOUNDARY)))
    stack_parm = NULL;

  /* If parm was passed in memory, and we need to convert it on entry,
     don't store it back in that same slot.  */
  else if (data->entry_parm == stack_parm
	   && data->nominal_mode != BLKmode
	   && data->nominal_mode != data->passed_mode)
    stack_parm = NULL;

  /* If stack protection is in effect for this function, don't leave any
     pointers in their passed stack slots.  */
  else if (crtl->stack_protect_guard
	   && (flag_stack_protect == 2
	       || data->passed_pointer
	       || POINTER_TYPE_P (data->nominal_type)))
    stack_parm = NULL;

  data->stack_parm = stack_parm;
}

/* A subroutine of assign_parms.  Return true if the current parameter
   should be stored as a BLKmode in the current frame.  */

static bool
assign_parm_setup_block_p (struct assign_parm_data_one *data)
{
  if (data->nominal_mode == BLKmode)
    return true;
  if (GET_MODE (data->entry_parm) == BLKmode)
    return true;

#ifdef BLOCK_REG_PADDING
  /* Only assign_parm_setup_block knows how to deal with register arguments
     that are padded at the least significant end.  */
  if (REG_P (data->entry_parm)
      && GET_MODE_SIZE (data->promoted_mode) < UNITS_PER_WORD
      && (BLOCK_REG_PADDING (data->passed_mode, data->passed_type, 1)
	  == (BYTES_BIG_ENDIAN ? upward : downward)))
    return true;
#endif

  return false;
}

/* A subroutine of assign_parms.  Arrange for the parameter to be
   present and valid in DATA->STACK_RTL.  */

static void
assign_parm_setup_block (struct assign_parm_data_all *all,
			 tree parm, struct assign_parm_data_one *data)
{
  rtx entry_parm = data->entry_parm;
  rtx stack_parm = data->stack_parm;
  HOST_WIDE_INT size;
  HOST_WIDE_INT size_stored;

  if (GET_CODE (entry_parm) == PARALLEL)
    entry_parm = emit_group_move_into_temps (entry_parm);

  size = int_size_in_bytes (data->passed_type);
  size_stored = CEIL_ROUND (size, UNITS_PER_WORD);
  if (stack_parm == 0)
    {
      DECL_ALIGN (parm) = MAX (DECL_ALIGN (parm), BITS_PER_WORD);
      stack_parm = assign_stack_local (BLKmode, size_stored,
				       DECL_ALIGN (parm));
      if (GET_MODE_SIZE (GET_MODE (entry_parm)) == size)
	PUT_MODE (stack_parm, GET_MODE (entry_parm));
      set_mem_attributes (stack_parm, parm, 1);
    }

  /* If a BLKmode arrives in registers, copy it to a stack slot.  Handle
     calls that pass values in multiple non-contiguous locations.  */
  if (REG_P (entry_parm) || GET_CODE (entry_parm) == PARALLEL)
    {
      rtx mem;

      /* Note that we will be storing an integral number of words.
	 So we have to be careful to ensure that we allocate an
	 integral number of words.  We do this above when we call
	 assign_stack_local if space was not allocated in the argument
	 list.  If it was, this will not work if PARM_BOUNDARY is not
	 a multiple of BITS_PER_WORD.  It isn't clear how to fix this
	 if it becomes a problem.  Exception is when BLKmode arrives
	 with arguments not conforming to word_mode.  */

      if (data->stack_parm == 0)
	;
      else if (GET_CODE (entry_parm) == PARALLEL)
	;
      else
	gcc_assert (!size || !(PARM_BOUNDARY % BITS_PER_WORD));

      mem = validize_mem (stack_parm);

      /* Handle values in multiple non-contiguous locations.  */
      if (GET_CODE (entry_parm) == PARALLEL)
	{
	  push_to_sequence2 (all->first_conversion_insn,
			     all->last_conversion_insn);
	  emit_group_store (mem, entry_parm, data->passed_type, size);
	  all->first_conversion_insn = get_insns ();
	  all->last_conversion_insn = get_last_insn ();
	  end_sequence ();
	}

      else if (size == 0)
	;

      /* If SIZE is that of a mode no bigger than a word, just use
	 that mode's store operation.  */
      else if (size <= UNITS_PER_WORD)
	{
	  enum machine_mode mode
	    = mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0);

	  if (mode != BLKmode
#ifdef BLOCK_REG_PADDING
	      && (size == UNITS_PER_WORD
		  || (BLOCK_REG_PADDING (mode, data->passed_type, 1)
		      != (BYTES_BIG_ENDIAN ? upward : downward)))
#endif
	      )
	    {
	      rtx reg;

	      /* We are really truncating a word_mode value containing
		 SIZE bytes into a value of mode MODE.  If such an
		 operation requires no actual instructions, we can refer
		 to the value directly in mode MODE, otherwise we must
		 start with the register in word_mode and explicitly
		 convert it.  */
	      if (TRULY_NOOP_TRUNCATION (size * BITS_PER_UNIT, BITS_PER_WORD))
		reg = gen_rtx_REG (mode, REGNO (entry_parm));
	      else
		{
		  reg = gen_rtx_REG (word_mode, REGNO (entry_parm));
		  reg = convert_to_mode (mode, copy_to_reg (reg), 1);
		}
	      emit_move_insn (change_address (mem, mode, 0), reg);
	    }

	  /* Blocks smaller than a word on a BYTES_BIG_ENDIAN
	     machine must be aligned to the left before storing
	     to memory.  Note that the previous test doesn't
	     handle all cases (e.g. SIZE == 3).  */
	  else if (size != UNITS_PER_WORD
#ifdef BLOCK_REG_PADDING
		   && (BLOCK_REG_PADDING (mode, data->passed_type, 1)
		       == downward)
#else
		   && BYTES_BIG_ENDIAN
#endif
		   )
	    {
	      rtx tem, x;
	      int by = (UNITS_PER_WORD - size) * BITS_PER_UNIT;
	      rtx reg = gen_rtx_REG (word_mode, REGNO (entry_parm));

	      x = expand_shift (LSHIFT_EXPR, word_mode, reg,
				build_int_cst (NULL_TREE, by),
				NULL_RTX, 1);
	      tem = change_address (mem, word_mode, 0);
	      emit_move_insn (tem, x);
	    }
	  else
	    move_block_from_reg (REGNO (entry_parm), mem,
				 size_stored / UNITS_PER_WORD);
	}
      else
	move_block_from_reg (REGNO (entry_parm), mem,
			     size_stored / UNITS_PER_WORD);
    }
  else if (data->stack_parm == 0)
    {
      push_to_sequence2 (all->first_conversion_insn, all->last_conversion_insn);
      emit_block_move (stack_parm, data->entry_parm, GEN_INT (size),
		       BLOCK_OP_NORMAL);
      all->first_conversion_insn = get_insns ();
      all->last_conversion_insn = get_last_insn ();
      end_sequence ();
    }

  data->stack_parm = stack_parm;
  SET_DECL_RTL (parm, stack_parm);
}

/* A subroutine of assign_parms.  Allocate a pseudo to hold the current
   parameter.  Get it there.  Perform all ABI specified conversions.  */

static void
assign_parm_setup_reg (struct assign_parm_data_all *all, tree parm,
		       struct assign_parm_data_one *data)
{
  rtx parmreg;
  enum machine_mode promoted_nominal_mode;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (parm));
  bool did_conversion = false;

  /* Store the parm in a pseudoregister during the function, but we may
     need to do it in a wider mode.  Using 2 here makes the result
     consistent with promote_decl_mode and thus expand_expr_real_1.  */
  promoted_nominal_mode
    = promote_function_mode (data->nominal_type, data->nominal_mode, &unsignedp,
			     TREE_TYPE (current_function_decl), 2);

  parmreg = gen_reg_rtx (promoted_nominal_mode);

  if (!DECL_ARTIFICIAL (parm))
    mark_user_reg (parmreg);

  /* If this was an item that we received a pointer to,
     set DECL_RTL appropriately.  */
  if (data->passed_pointer)
    {
      rtx x = gen_rtx_MEM (TYPE_MODE (TREE_TYPE (data->passed_type)), parmreg);
      set_mem_attributes (x, parm, 1);
      SET_DECL_RTL (parm, x);
    }
  else
    SET_DECL_RTL (parm, parmreg);

  assign_parm_remove_parallels (data);

  /* Copy the value into the register, thus bridging between
     assign_parm_find_data_types and expand_expr_real_1.  */
  if (data->nominal_mode != data->passed_mode
      || promoted_nominal_mode != data->promoted_mode)
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

      rtx tempreg = gen_reg_rtx (GET_MODE (data->entry_parm));

      emit_move_insn (tempreg, validize_mem (data->entry_parm));

      push_to_sequence2 (all->first_conversion_insn, all->last_conversion_insn);
      tempreg = convert_to_mode (data->nominal_mode, tempreg, unsignedp);

      if (GET_CODE (tempreg) == SUBREG
	  && GET_MODE (tempreg) == data->nominal_mode
	  && REG_P (SUBREG_REG (tempreg))
	  && data->nominal_mode == data->passed_mode
	  && GET_MODE (SUBREG_REG (tempreg)) == GET_MODE (data->entry_parm)
	  && GET_MODE_SIZE (GET_MODE (tempreg))
	     < GET_MODE_SIZE (GET_MODE (data->entry_parm)))
	{
	  /* The argument is already sign/zero extended, so note it
	     into the subreg.  */
	  SUBREG_PROMOTED_VAR_P (tempreg) = 1;
	  SUBREG_PROMOTED_UNSIGNED_SET (tempreg, unsignedp);
	}

      /* TREE_USED gets set erroneously during expand_assignment.  */
      save_tree_used = TREE_USED (parm);
      expand_assignment (parm, make_tree (data->nominal_type, tempreg), false);
      TREE_USED (parm) = save_tree_used;
      all->first_conversion_insn = get_insns ();
      all->last_conversion_insn = get_last_insn ();
      end_sequence ();

      did_conversion = true;
    }
  else
    emit_move_insn (parmreg, validize_mem (data->entry_parm));

  /* If we were passed a pointer but the actual value can safely live
     in a register, put it in one.  */
  if (data->passed_pointer
      && TYPE_MODE (TREE_TYPE (parm)) != BLKmode
      /* If by-reference argument was promoted, demote it.  */
      && (TYPE_MODE (TREE_TYPE (parm)) != GET_MODE (DECL_RTL (parm))
	  || use_register_for_decl (parm)))
    {
      /* We can't use nominal_mode, because it will have been set to
	 Pmode above.  We must use the actual mode of the parm.  */
      parmreg = gen_reg_rtx (TYPE_MODE (TREE_TYPE (parm)));
      mark_user_reg (parmreg);

      if (GET_MODE (parmreg) != GET_MODE (DECL_RTL (parm)))
	{
	  rtx tempreg = gen_reg_rtx (GET_MODE (DECL_RTL (parm)));
	  int unsigned_p = TYPE_UNSIGNED (TREE_TYPE (parm));

	  push_to_sequence2 (all->first_conversion_insn,
			     all->last_conversion_insn);
	  emit_move_insn (tempreg, DECL_RTL (parm));
	  tempreg = convert_to_mode (GET_MODE (parmreg), tempreg, unsigned_p);
	  emit_move_insn (parmreg, tempreg);
	  all->first_conversion_insn = get_insns ();
	  all->last_conversion_insn = get_last_insn ();
	  end_sequence ();

	  did_conversion = true;
	}
      else
	emit_move_insn (parmreg, DECL_RTL (parm));

      SET_DECL_RTL (parm, parmreg);

      /* STACK_PARM is the pointer, not the parm, and PARMREG is
	 now the parm.  */
      data->stack_parm = NULL;
    }

  /* Mark the register as eliminable if we did no conversion and it was
     copied from memory at a fixed offset, and the arg pointer was not
     copied to a pseudo-reg.  If the arg pointer is a pseudo reg or the
     offset formed an invalid address, such memory-equivalences as we
     make here would screw up life analysis for it.  */
  if (data->nominal_mode == data->passed_mode
      && !did_conversion
      && data->stack_parm != 0
      && MEM_P (data->stack_parm)
      && data->locate.offset.var == 0
      && reg_mentioned_p (virtual_incoming_args_rtx,
			  XEXP (data->stack_parm, 0)))
    {
      rtx linsn = get_last_insn ();
      rtx sinsn, set;

      /* Mark complex types separately.  */
      if (GET_CODE (parmreg) == CONCAT)
	{
	  enum machine_mode submode
	    = GET_MODE_INNER (GET_MODE (parmreg));
	  int regnor = REGNO (XEXP (parmreg, 0));
	  int regnoi = REGNO (XEXP (parmreg, 1));
	  rtx stackr = adjust_address_nv (data->stack_parm, submode, 0);
	  rtx stacki = adjust_address_nv (data->stack_parm, submode,
					  GET_MODE_SIZE (submode));

	  /* Scan backwards for the set of the real and
	     imaginary parts.  */
	  for (sinsn = linsn; sinsn != 0;
	       sinsn = prev_nonnote_insn (sinsn))
	    {
	      set = single_set (sinsn);
	      if (set == 0)
		continue;

	      if (SET_DEST (set) == regno_reg_rtx [regnoi])
		set_unique_reg_note (sinsn, REG_EQUIV, stacki);
	      else if (SET_DEST (set) == regno_reg_rtx [regnor])
		set_unique_reg_note (sinsn, REG_EQUIV, stackr);
	    }
	}
      else if ((set = single_set (linsn)) != 0
	       && SET_DEST (set) == parmreg)
	set_unique_reg_note (linsn, REG_EQUIV, data->stack_parm);
    }

  /* For pointer data type, suggest pointer register.  */
  if (POINTER_TYPE_P (TREE_TYPE (parm)))
    mark_reg_pointer (parmreg,
		      TYPE_ALIGN (TREE_TYPE (TREE_TYPE (parm))));
}

/* A subroutine of assign_parms.  Allocate stack space to hold the current
   parameter.  Get it there.  Perform all ABI specified conversions.  */

static void
assign_parm_setup_stack (struct assign_parm_data_all *all, tree parm,
		         struct assign_parm_data_one *data)
{
  /* Value must be stored in the stack slot STACK_PARM during function
     execution.  */
  bool to_conversion = false;

  assign_parm_remove_parallels (data);

  if (data->promoted_mode != data->nominal_mode)
    {
      /* Conversion is required.  */
      rtx tempreg = gen_reg_rtx (GET_MODE (data->entry_parm));

      emit_move_insn (tempreg, validize_mem (data->entry_parm));

      push_to_sequence2 (all->first_conversion_insn, all->last_conversion_insn);
      to_conversion = true;

      data->entry_parm = convert_to_mode (data->nominal_mode, tempreg,
					  TYPE_UNSIGNED (TREE_TYPE (parm)));

      if (data->stack_parm)
	{
	  int offset = subreg_lowpart_offset (data->nominal_mode,
					      GET_MODE (data->stack_parm));
	  /* ??? This may need a big-endian conversion on sparc64.  */
	  data->stack_parm
	    = adjust_address (data->stack_parm, data->nominal_mode, 0);
	  if (offset && MEM_OFFSET (data->stack_parm))
	    set_mem_offset (data->stack_parm,
			    plus_constant (MEM_OFFSET (data->stack_parm),
					   offset));
	}
    }

  if (data->entry_parm != data->stack_parm)
    {
      rtx src, dest;

      if (data->stack_parm == 0)
	{
	  int align = STACK_SLOT_ALIGNMENT (data->passed_type,
					    GET_MODE (data->entry_parm),
					    TYPE_ALIGN (data->passed_type));
	  data->stack_parm
	    = assign_stack_local (GET_MODE (data->entry_parm),
				  GET_MODE_SIZE (GET_MODE (data->entry_parm)),
				  align);
	  set_mem_attributes (data->stack_parm, parm, 1);
	}

      dest = validize_mem (data->stack_parm);
      src = validize_mem (data->entry_parm);

      if (MEM_P (src))
	{
	  /* Use a block move to handle potentially misaligned entry_parm.  */
	  if (!to_conversion)
	    push_to_sequence2 (all->first_conversion_insn,
			       all->last_conversion_insn);
	  to_conversion = true;

	  emit_block_move (dest, src,
			   GEN_INT (int_size_in_bytes (data->passed_type)),
			   BLOCK_OP_NORMAL);
	}
      else
	emit_move_insn (dest, src);
    }

  if (to_conversion)
    {
      all->first_conversion_insn = get_insns ();
      all->last_conversion_insn = get_last_insn ();
      end_sequence ();
    }

  SET_DECL_RTL (parm, data->stack_parm);
}

/* A subroutine of assign_parms.  If the ABI splits complex arguments, then
   undo the frobbing that we did in assign_parms_augmented_arg_list.  */

static void
assign_parms_unsplit_complex (struct assign_parm_data_all *all, tree fnargs)
{
  tree parm;
  tree orig_fnargs = all->orig_fnargs;

  for (parm = orig_fnargs; parm; parm = TREE_CHAIN (parm))
    {
      if (TREE_CODE (TREE_TYPE (parm)) == COMPLEX_TYPE
	  && targetm.calls.split_complex_arg (TREE_TYPE (parm)))
	{
	  rtx tmp, real, imag;
	  enum machine_mode inner = GET_MODE_INNER (DECL_MODE (parm));

	  real = DECL_RTL (fnargs);
	  imag = DECL_RTL (TREE_CHAIN (fnargs));
	  if (inner != GET_MODE (real))
	    {
	      real = gen_lowpart_SUBREG (inner, real);
	      imag = gen_lowpart_SUBREG (inner, imag);
	    }

	  if (TREE_ADDRESSABLE (parm))
	    {
	      rtx rmem, imem;
	      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (parm));
	      int align = STACK_SLOT_ALIGNMENT (TREE_TYPE (parm),
						DECL_MODE (parm),
						TYPE_ALIGN (TREE_TYPE (parm)));

	      /* split_complex_arg put the real and imag parts in
		 pseudos.  Move them to memory.  */
	      tmp = assign_stack_local (DECL_MODE (parm), size, align);
	      set_mem_attributes (tmp, parm, 1);
	      rmem = adjust_address_nv (tmp, inner, 0);
	      imem = adjust_address_nv (tmp, inner, GET_MODE_SIZE (inner));
	      push_to_sequence2 (all->first_conversion_insn,
				 all->last_conversion_insn);
	      emit_move_insn (rmem, real);
	      emit_move_insn (imem, imag);
	      all->first_conversion_insn = get_insns ();
	      all->last_conversion_insn = get_last_insn ();
	      end_sequence ();
	    }
	  else
	    tmp = gen_rtx_CONCAT (DECL_MODE (parm), real, imag);
	  SET_DECL_RTL (parm, tmp);

	  real = DECL_INCOMING_RTL (fnargs);
	  imag = DECL_INCOMING_RTL (TREE_CHAIN (fnargs));
	  if (inner != GET_MODE (real))
	    {
	      real = gen_lowpart_SUBREG (inner, real);
	      imag = gen_lowpart_SUBREG (inner, imag);
	    }
	  tmp = gen_rtx_CONCAT (DECL_MODE (parm), real, imag);
	  set_decl_incoming_rtl (parm, tmp, false);
	  fnargs = TREE_CHAIN (fnargs);
	}
      else
	{
	  SET_DECL_RTL (parm, DECL_RTL (fnargs));
	  set_decl_incoming_rtl (parm, DECL_INCOMING_RTL (fnargs), false);

	  /* Set MEM_EXPR to the original decl, i.e. to PARM,
	     instead of the copy of decl, i.e. FNARGS.  */
	  if (DECL_INCOMING_RTL (parm) && MEM_P (DECL_INCOMING_RTL (parm)))
	    set_mem_expr (DECL_INCOMING_RTL (parm), parm);
	}

      fnargs = TREE_CHAIN (fnargs);
    }
}

/* Assign RTL expressions to the function's parameters.  This may involve
   copying them into registers and using those registers as the DECL_RTL.  */

static void
assign_parms (tree fndecl)
{
  struct assign_parm_data_all all;
  tree fnargs, parm;

  crtl->args.internal_arg_pointer
    = targetm.calls.internal_arg_pointer ();

  assign_parms_initialize_all (&all);
  fnargs = assign_parms_augmented_arg_list (&all);

  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    {
      struct assign_parm_data_one data;

      /* Extract the type of PARM; adjust it according to ABI.  */
      assign_parm_find_data_types (&all, parm, &data);

      /* Early out for errors and void parameters.  */
      if (data.passed_mode == VOIDmode)
	{
	  SET_DECL_RTL (parm, const0_rtx);
	  DECL_INCOMING_RTL (parm) = DECL_RTL (parm);
	  continue;
	}

      /* Estimate stack alignment from parameter alignment.  */
      if (SUPPORTS_STACK_ALIGNMENT)
        {
          unsigned int align = FUNCTION_ARG_BOUNDARY (data.promoted_mode,
						      data.passed_type);
	  align = MINIMUM_ALIGNMENT (data.passed_type, data.promoted_mode,
				     align);
	  if (TYPE_ALIGN (data.nominal_type) > align)
	    align = MINIMUM_ALIGNMENT (data.nominal_type,
				       TYPE_MODE (data.nominal_type),
				       TYPE_ALIGN (data.nominal_type));
	  if (crtl->stack_alignment_estimated < align)
	    {
	      gcc_assert (!crtl->stack_realign_processed);
	      crtl->stack_alignment_estimated = align;
	    }
	}

      if (cfun->stdarg && !TREE_CHAIN (parm))
	assign_parms_setup_varargs (&all, &data, false);

      /* Find out where the parameter arrives in this function.  */
      assign_parm_find_entry_rtl (&all, &data);

      /* Find out where stack space for this parameter might be.  */
      if (assign_parm_is_stack_parm (&all, &data))
	{
	  assign_parm_find_stack_rtl (parm, &data);
	  assign_parm_adjust_entry_rtl (&data);
	}

      /* Record permanently how this parm was passed.  */
      set_decl_incoming_rtl (parm, data.entry_parm, data.passed_pointer);

      /* Update info on where next arg arrives in registers.  */
      FUNCTION_ARG_ADVANCE (all.args_so_far, data.promoted_mode,
			    data.passed_type, data.named_arg);

      assign_parm_adjust_stack_rtl (&data);

      if (assign_parm_setup_block_p (&data))
	assign_parm_setup_block (&all, parm, &data);
      else if (data.passed_pointer || use_register_for_decl (parm))
	assign_parm_setup_reg (&all, parm, &data);
      else
	assign_parm_setup_stack (&all, parm, &data);
    }

  if (targetm.calls.split_complex_arg && fnargs != all.orig_fnargs)
    assign_parms_unsplit_complex (&all, fnargs);

  /* Output all parameter conversion instructions (possibly including calls)
     now that all parameters have been copied out of hard registers.  */
  emit_insn (all.first_conversion_insn);

  /* Estimate reload stack alignment from scalar return mode.  */
  if (SUPPORTS_STACK_ALIGNMENT)
    {
      if (DECL_RESULT (fndecl))
	{
	  tree type = TREE_TYPE (DECL_RESULT (fndecl));
	  enum machine_mode mode = TYPE_MODE (type);

	  if (mode != BLKmode
	      && mode != VOIDmode
	      && !AGGREGATE_TYPE_P (type))
	    {
	      unsigned int align = GET_MODE_ALIGNMENT (mode);
	      if (crtl->stack_alignment_estimated < align)
		{
		  gcc_assert (!crtl->stack_realign_processed);
		  crtl->stack_alignment_estimated = align;
		}
	    }
	}
    }

  /* If we are receiving a struct value address as the first argument, set up
     the RTL for the function result. As this might require code to convert
     the transmitted address to Pmode, we do this here to ensure that possible
     preliminary conversions of the address have been emitted already.  */
  if (all.function_result_decl)
    {
      tree result = DECL_RESULT (current_function_decl);
      rtx addr = DECL_RTL (all.function_result_decl);
      rtx x;

      if (DECL_BY_REFERENCE (result))
	x = addr;
      else
	{
	  addr = convert_memory_address (Pmode, addr);
	  x = gen_rtx_MEM (DECL_MODE (result), addr);
	  set_mem_attributes (x, result, 1);
	}
      SET_DECL_RTL (result, x);
    }

  /* We have aligned all the args, so add space for the pretend args.  */
  crtl->args.pretend_args_size = all.pretend_args_size;
  all.stack_args_size.constant += all.extra_pretend_bytes;
  crtl->args.size = all.stack_args_size.constant;

  /* Adjust function incoming argument size for alignment and
     minimum length.  */

#ifdef REG_PARM_STACK_SPACE
  crtl->args.size = MAX (crtl->args.size,
				    REG_PARM_STACK_SPACE (fndecl));
#endif

  crtl->args.size = CEIL_ROUND (crtl->args.size,
					   PARM_BOUNDARY / BITS_PER_UNIT);

#ifdef ARGS_GROW_DOWNWARD
  crtl->args.arg_offset_rtx
    = (all.stack_args_size.var == 0 ? GEN_INT (-all.stack_args_size.constant)
       : expand_expr (size_diffop (all.stack_args_size.var,
				   size_int (-all.stack_args_size.constant)),
		      NULL_RTX, VOIDmode, EXPAND_NORMAL));
#else
  crtl->args.arg_offset_rtx = ARGS_SIZE_RTX (all.stack_args_size);
#endif

  /* See how many bytes, if any, of its args a function should try to pop
     on return.  */

  crtl->args.pops_args = RETURN_POPS_ARGS (fndecl, TREE_TYPE (fndecl),
						 crtl->args.size);

  /* For stdarg.h function, save info about
     regs and stack space used by the named args.  */

  crtl->args.info = all.args_so_far;

  /* Set the rtx used for the function return value.  Put this in its
     own variable so any optimizers that need this information don't have
     to include tree.h.  Do this here so it gets done when an inlined
     function gets output.  */

  crtl->return_rtx
    = (DECL_RTL_SET_P (DECL_RESULT (fndecl))
       ? DECL_RTL (DECL_RESULT (fndecl)) : NULL_RTX);

  /* If scalar return value was computed in a pseudo-reg, or was a named
     return value that got dumped to the stack, copy that to the hard
     return register.  */
  if (DECL_RTL_SET_P (DECL_RESULT (fndecl)))
    {
      tree decl_result = DECL_RESULT (fndecl);
      rtx decl_rtl = DECL_RTL (decl_result);

      if (REG_P (decl_rtl)
	  ? REGNO (decl_rtl) >= FIRST_PSEUDO_REGISTER
	  : DECL_REGISTER (decl_result))
	{
	  rtx real_decl_rtl;

	  real_decl_rtl = targetm.calls.function_value (TREE_TYPE (decl_result),
							fndecl, true);
	  REG_FUNCTION_VALUE_P (real_decl_rtl) = 1;
	  /* The delay slot scheduler assumes that crtl->return_rtx
	     holds the hard register containing the return value, not a
	     temporary pseudo.  */
	  crtl->return_rtx = real_decl_rtl;
	}
    }
}

/* A subroutine of gimplify_parameters, invoked via walk_tree.
   For all seen types, gimplify their sizes.  */

static tree
gimplify_parm_type (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;

  *walk_subtrees = 0;
  if (TYPE_P (t))
    {
      if (POINTER_TYPE_P (t))
	*walk_subtrees = 1;
      else if (TYPE_SIZE (t) && !TREE_CONSTANT (TYPE_SIZE (t))
	       && !TYPE_SIZES_GIMPLIFIED (t))
	{
	  gimplify_type_sizes (t, (gimple_seq *) data);
	  *walk_subtrees = 1;
	}
    }

  return NULL;
}

/* Gimplify the parameter list for current_function_decl.  This involves
   evaluating SAVE_EXPRs of variable sized parameters and generating code
   to implement callee-copies reference parameters.  Returns a sequence of
   statements to add to the beginning of the function.  */

gimple_seq
gimplify_parameters (void)
{
  struct assign_parm_data_all all;
  tree fnargs, parm;
  gimple_seq stmts = NULL;

  assign_parms_initialize_all (&all);
  fnargs = assign_parms_augmented_arg_list (&all);

  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    {
      struct assign_parm_data_one data;

      /* Extract the type of PARM; adjust it according to ABI.  */
      assign_parm_find_data_types (&all, parm, &data);

      /* Early out for errors and void parameters.  */
      if (data.passed_mode == VOIDmode || DECL_SIZE (parm) == NULL)
	continue;

      /* Update info on where next arg arrives in registers.  */
      FUNCTION_ARG_ADVANCE (all.args_so_far, data.promoted_mode,
			    data.passed_type, data.named_arg);

      /* ??? Once upon a time variable_size stuffed parameter list
	 SAVE_EXPRs (amongst others) onto a pending sizes list.  This
	 turned out to be less than manageable in the gimple world.
	 Now we have to hunt them down ourselves.  */
      walk_tree_without_duplicates (&data.passed_type,
				    gimplify_parm_type, &stmts);

      if (TREE_CODE (DECL_SIZE_UNIT (parm)) != INTEGER_CST)
	{
	  gimplify_one_sizepos (&DECL_SIZE (parm), &stmts);
	  gimplify_one_sizepos (&DECL_SIZE_UNIT (parm), &stmts);
	}

      if (data.passed_pointer)
	{
          tree type = TREE_TYPE (data.passed_type);
	  if (reference_callee_copied (&all.args_so_far, TYPE_MODE (type),
				       type, data.named_arg))
	    {
	      tree local, t;

	      /* For constant-sized objects, this is trivial; for
		 variable-sized objects, we have to play games.  */
	      if (TREE_CODE (DECL_SIZE_UNIT (parm)) == INTEGER_CST
		  && !(flag_stack_check == GENERIC_STACK_CHECK
		       && compare_tree_int (DECL_SIZE_UNIT (parm),
					    STACK_CHECK_MAX_VAR_SIZE) > 0))
		{
		  local = create_tmp_var (type, get_name (parm));
		  DECL_IGNORED_P (local) = 0;
		  /* If PARM was addressable, move that flag over
		     to the local copy, as its address will be taken,
		     not the PARMs.  */
		  if (TREE_ADDRESSABLE (parm))
		    {
		      TREE_ADDRESSABLE (parm) = 0;
		      TREE_ADDRESSABLE (local) = 1;
		    }
		}
	      else
		{
		  tree ptr_type, addr;

		  ptr_type = build_pointer_type (type);
		  addr = create_tmp_var (ptr_type, get_name (parm));
		  DECL_IGNORED_P (addr) = 0;
		  local = build_fold_indirect_ref (addr);

		  t = built_in_decls[BUILT_IN_ALLOCA];
		  t = build_call_expr (t, 1, DECL_SIZE_UNIT (parm));
		  t = fold_convert (ptr_type, t);
		  t = build2 (MODIFY_EXPR, TREE_TYPE (addr), addr, t);
		  gimplify_and_add (t, &stmts);
		}

	      gimplify_assign (local, parm, &stmts);

	      SET_DECL_VALUE_EXPR (parm, local);
	      DECL_HAS_VALUE_EXPR_P (parm) = 1;
	    }
	}
    }

  return stmts;
}

/* Compute the size and offset from the start of the stacked arguments for a
   parm passed in mode PASSED_MODE and with type TYPE.

   INITIAL_OFFSET_PTR points to the current offset into the stacked
   arguments.

   The starting offset and size for this parm are returned in
   LOCATE->OFFSET and LOCATE->SIZE, respectively.  When IN_REGS is
   nonzero, the offset is that of stack slot, which is returned in
   LOCATE->SLOT_OFFSET.  LOCATE->ALIGNMENT_PAD is the amount of
   padding required from the initial offset ptr to the stack slot.

   IN_REGS is nonzero if the argument will be passed in registers.  It will
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

/*  LOCATE->OFFSET will be negative for ARGS_GROW_DOWNWARD case;
    INITIAL_OFFSET_PTR is positive because locate_and_pad_parm's
    callers pass in the total size of args so far as
    INITIAL_OFFSET_PTR.  LOCATE->SIZE is always positive.  */

void
locate_and_pad_parm (enum machine_mode passed_mode, tree type, int in_regs,
		     int partial, tree fndecl ATTRIBUTE_UNUSED,
		     struct args_size *initial_offset_ptr,
		     struct locate_and_pad_arg_data *locate)
{
  tree sizetree;
  enum direction where_pad;
  unsigned int boundary;
  int reg_parm_stack_space = 0;
  int part_size_in_regs;

#ifdef REG_PARM_STACK_SPACE
  reg_parm_stack_space = REG_PARM_STACK_SPACE (fndecl);

  /* If we have found a stack parm before we reach the end of the
     area reserved for registers, skip that area.  */
  if (! in_regs)
    {
      if (reg_parm_stack_space > 0)
	{
	  if (initial_offset_ptr->var)
	    {
	      initial_offset_ptr->var
		= size_binop (MAX_EXPR, ARGS_SIZE_TREE (*initial_offset_ptr),
			      ssize_int (reg_parm_stack_space));
	      initial_offset_ptr->constant = 0;
	    }
	  else if (initial_offset_ptr->constant < reg_parm_stack_space)
	    initial_offset_ptr->constant = reg_parm_stack_space;
	}
    }
#endif /* REG_PARM_STACK_SPACE */

  part_size_in_regs = (reg_parm_stack_space == 0 ? partial : 0);

  sizetree
    = type ? size_in_bytes (type) : size_int (GET_MODE_SIZE (passed_mode));
  where_pad = FUNCTION_ARG_PADDING (passed_mode, type);
  boundary = FUNCTION_ARG_BOUNDARY (passed_mode, type);
  locate->where_pad = where_pad;

  /* Alignment can't exceed MAX_SUPPORTED_STACK_ALIGNMENT.  */
  if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
    boundary = MAX_SUPPORTED_STACK_ALIGNMENT;

  locate->boundary = boundary;

  if (SUPPORTS_STACK_ALIGNMENT)
    {
      /* stack_alignment_estimated can't change after stack has been
	 realigned.  */
      if (crtl->stack_alignment_estimated < boundary)
        {
          if (!crtl->stack_realign_processed)
	    crtl->stack_alignment_estimated = boundary;
	  else
	    {
	      /* If stack is realigned and stack alignment value
		 hasn't been finalized, it is OK not to increase
		 stack_alignment_estimated.  The bigger alignment
		 requirement is recorded in stack_alignment_needed
		 below.  */
	      gcc_assert (!crtl->stack_realign_finalized
			  && crtl->stack_realign_needed);
	    }
	}
    }

  /* Remember if the outgoing parameter requires extra alignment on the
     calling function side.  */
  if (crtl->stack_alignment_needed < boundary)
    crtl->stack_alignment_needed = boundary;
  if (crtl->preferred_stack_boundary < boundary)
    crtl->preferred_stack_boundary = boundary;

#ifdef ARGS_GROW_DOWNWARD
  locate->slot_offset.constant = -initial_offset_ptr->constant;
  if (initial_offset_ptr->var)
    locate->slot_offset.var = size_binop (MINUS_EXPR, ssize_int (0),
					  initial_offset_ptr->var);

  {
    tree s2 = sizetree;
    if (where_pad != none
	&& (!host_integerp (sizetree, 1)
	    || (tree_low_cst (sizetree, 1) * BITS_PER_UNIT) % PARM_BOUNDARY))
      s2 = round_up (s2, PARM_BOUNDARY / BITS_PER_UNIT);
    SUB_PARM_SIZE (locate->slot_offset, s2);
  }

  locate->slot_offset.constant += part_size_in_regs;

  if (!in_regs
#ifdef REG_PARM_STACK_SPACE
      || REG_PARM_STACK_SPACE (fndecl) > 0
#endif
     )
    pad_to_arg_alignment (&locate->slot_offset, boundary,
			  &locate->alignment_pad);

  locate->size.constant = (-initial_offset_ptr->constant
			   - locate->slot_offset.constant);
  if (initial_offset_ptr->var)
    locate->size.var = size_binop (MINUS_EXPR,
				   size_binop (MINUS_EXPR,
					       ssize_int (0),
					       initial_offset_ptr->var),
				   locate->slot_offset.var);

  /* Pad_below needs the pre-rounded size to know how much to pad
     below.  */
  locate->offset = locate->slot_offset;
  if (where_pad == downward)
    pad_below (&locate->offset, passed_mode, sizetree);

#else /* !ARGS_GROW_DOWNWARD */
  if (!in_regs
#ifdef REG_PARM_STACK_SPACE
      || REG_PARM_STACK_SPACE (fndecl) > 0
#endif
      )
    pad_to_arg_alignment (initial_offset_ptr, boundary,
			  &locate->alignment_pad);
  locate->slot_offset = *initial_offset_ptr;

#ifdef PUSH_ROUNDING
  if (passed_mode != BLKmode)
    sizetree = size_int (PUSH_ROUNDING (TREE_INT_CST_LOW (sizetree)));
#endif

  /* Pad_below needs the pre-rounded size to know how much to pad below
     so this must be done before rounding up.  */
  locate->offset = locate->slot_offset;
  if (where_pad == downward)
    pad_below (&locate->offset, passed_mode, sizetree);

  if (where_pad != none
      && (!host_integerp (sizetree, 1)
	  || (tree_low_cst (sizetree, 1) * BITS_PER_UNIT) % PARM_BOUNDARY))
    sizetree = round_up (sizetree, PARM_BOUNDARY / BITS_PER_UNIT);

  ADD_PARM_SIZE (locate->size, sizetree);

  locate->size.constant -= part_size_in_regs;
#endif /* ARGS_GROW_DOWNWARD */

#ifdef FUNCTION_ARG_OFFSET
  locate->offset.constant += FUNCTION_ARG_OFFSET (passed_mode, type);
#endif
}

/* Round the stack offset in *OFFSET_PTR up to a multiple of BOUNDARY.
   BOUNDARY is measured in bits, but must be a multiple of a storage unit.  */

static void
pad_to_arg_alignment (struct args_size *offset_ptr, int boundary,
		      struct args_size *alignment_pad)
{
  tree save_var = NULL_TREE;
  HOST_WIDE_INT save_constant = 0;
  int boundary_in_bytes = boundary / BITS_PER_UNIT;
  HOST_WIDE_INT sp_offset = STACK_POINTER_OFFSET;

#ifdef SPARC_STACK_BOUNDARY_HACK
  /* ??? The SPARC port may claim a STACK_BOUNDARY higher than
     the real alignment of %sp.  However, when it does this, the
     alignment of %sp+STACK_POINTER_OFFSET is STACK_BOUNDARY.  */
  if (SPARC_STACK_BOUNDARY_HACK)
    sp_offset = 0;
#endif

  if (boundary > PARM_BOUNDARY)
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
	  tree sp_offset_tree = ssize_int (sp_offset);
	  tree offset = size_binop (PLUS_EXPR,
				    ARGS_SIZE_TREE (*offset_ptr),
				    sp_offset_tree);
#ifdef ARGS_GROW_DOWNWARD
	  tree rounded = round_down (offset, boundary / BITS_PER_UNIT);
#else
	  tree rounded = round_up   (offset, boundary / BITS_PER_UNIT);
#endif

	  offset_ptr->var = size_binop (MINUS_EXPR, rounded, sp_offset_tree);
	  /* ARGS_SIZE_TREE includes constant term.  */
	  offset_ptr->constant = 0;
	  if (boundary > PARM_BOUNDARY)
	    alignment_pad->var = size_binop (MINUS_EXPR, offset_ptr->var,
					     save_var);
	}
      else
	{
	  offset_ptr->constant = -sp_offset +
#ifdef ARGS_GROW_DOWNWARD
	    FLOOR_ROUND (offset_ptr->constant + sp_offset, boundary_in_bytes);
#else
	    CEIL_ROUND (offset_ptr->constant + sp_offset, boundary_in_bytes);
#endif
	    if (boundary > PARM_BOUNDARY)
	      alignment_pad->constant = offset_ptr->constant - save_constant;
	}
    }
}

static void
pad_below (struct args_size *offset_ptr, enum machine_mode passed_mode, tree sizetree)
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


/* True if register REGNO was alive at a place where `setjmp' was
   called and was set more than once or is an argument.  Such regs may
   be clobbered by `longjmp'.  */

static bool
regno_clobbered_at_setjmp (bitmap setjmp_crosses, int regno)
{
  /* There appear to be cases where some local vars never reach the
     backend but have bogus regnos.  */
  if (regno >= max_reg_num ())
    return false;

  return ((REG_N_SETS (regno) > 1
	   || REGNO_REG_SET_P (df_get_live_out (ENTRY_BLOCK_PTR), regno))
	  && REGNO_REG_SET_P (setjmp_crosses, regno));
}

/* Walk the tree of blocks describing the binding levels within a
   function and warn about variables the might be killed by setjmp or
   vfork.  This is done after calling flow_analysis before register
   allocation since that will clobber the pseudo-regs to hard
   regs.  */

static void
setjmp_vars_warning (bitmap setjmp_crosses, tree block)
{
  tree decl, sub;

  for (decl = BLOCK_VARS (block); decl; decl = TREE_CHAIN (decl))
    {
      if (TREE_CODE (decl) == VAR_DECL
	  && DECL_RTL_SET_P (decl)
	  && REG_P (DECL_RTL (decl))
	  && regno_clobbered_at_setjmp (setjmp_crosses, REGNO (DECL_RTL (decl))))
	warning (OPT_Wclobbered, "variable %q+D might be clobbered by"
                 " %<longjmp%> or %<vfork%>", decl);
    }

  for (sub = BLOCK_SUBBLOCKS (block); sub; sub = BLOCK_CHAIN (sub))
    setjmp_vars_warning (setjmp_crosses, sub);
}

/* Do the appropriate part of setjmp_vars_warning
   but for arguments instead of local variables.  */

static void
setjmp_args_warning (bitmap setjmp_crosses)
{
  tree decl;
  for (decl = DECL_ARGUMENTS (current_function_decl);
       decl; decl = TREE_CHAIN (decl))
    if (DECL_RTL (decl) != 0
	&& REG_P (DECL_RTL (decl))
	&& regno_clobbered_at_setjmp (setjmp_crosses, REGNO (DECL_RTL (decl))))
      warning (OPT_Wclobbered,
               "argument %q+D might be clobbered by %<longjmp%> or %<vfork%>",
	       decl);
}

/* Generate warning messages for variables live across setjmp.  */

void
generate_setjmp_warnings (void)
{
  bitmap setjmp_crosses = regstat_get_setjmp_crosses ();

  if (n_basic_blocks == NUM_FIXED_BLOCKS
      || bitmap_empty_p (setjmp_crosses))
    return;

  setjmp_vars_warning (setjmp_crosses, DECL_INITIAL (current_function_decl));
  setjmp_args_warning (setjmp_crosses);
}


/* Identify BLOCKs referenced by more than one NOTE_INSN_BLOCK_{BEG,END},
   and create duplicate blocks.  */
/* ??? Need an option to either create block fragments or to create
   abstract origin duplicates of a source block.  It really depends
   on what optimization has been performed.  */

void
reorder_blocks (void)
{
  tree block = DECL_INITIAL (current_function_decl);
  VEC(tree,heap) *block_stack;

  if (block == NULL_TREE)
    return;

  block_stack = VEC_alloc (tree, heap, 10);

  /* Reset the TREE_ASM_WRITTEN bit for all blocks.  */
  clear_block_marks (block);

  /* Prune the old trees away, so that they don't get in the way.  */
  BLOCK_SUBBLOCKS (block) = NULL_TREE;
  BLOCK_CHAIN (block) = NULL_TREE;

  /* Recreate the block tree from the note nesting.  */
  reorder_blocks_1 (get_insns (), block, &block_stack);
  BLOCK_SUBBLOCKS (block) = blocks_nreverse (BLOCK_SUBBLOCKS (block));

  VEC_free (tree, heap, block_stack);
}

/* Helper function for reorder_blocks.  Reset TREE_ASM_WRITTEN.  */

void
clear_block_marks (tree block)
{
  while (block)
    {
      TREE_ASM_WRITTEN (block) = 0;
      clear_block_marks (BLOCK_SUBBLOCKS (block));
      block = BLOCK_CHAIN (block);
    }
}

static void
reorder_blocks_1 (rtx insns, tree current_block, VEC(tree,heap) **p_block_stack)
{
  rtx insn;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      if (NOTE_P (insn))
	{
	  if (NOTE_KIND (insn) == NOTE_INSN_BLOCK_BEG)
	    {
	      tree block = NOTE_BLOCK (insn);
	      tree origin;

	      origin = (BLOCK_FRAGMENT_ORIGIN (block)
			? BLOCK_FRAGMENT_ORIGIN (block)
			: block);

	      /* If we have seen this block before, that means it now
		 spans multiple address regions.  Create a new fragment.  */
	      if (TREE_ASM_WRITTEN (block))
		{
		  tree new_block = copy_node (block);

		  BLOCK_FRAGMENT_ORIGIN (new_block) = origin;
		  BLOCK_FRAGMENT_CHAIN (new_block)
		    = BLOCK_FRAGMENT_CHAIN (origin);
		  BLOCK_FRAGMENT_CHAIN (origin) = new_block;

		  NOTE_BLOCK (insn) = new_block;
		  block = new_block;
		}

	      BLOCK_SUBBLOCKS (block) = 0;
	      TREE_ASM_WRITTEN (block) = 1;
	      /* When there's only one block for the entire function,
		 current_block == block and we mustn't do this, it
		 will cause infinite recursion.  */
	      if (block != current_block)
		{
		  if (block != origin)
		    gcc_assert (BLOCK_SUPERCONTEXT (origin) == current_block);

		  BLOCK_SUPERCONTEXT (block) = current_block;
		  BLOCK_CHAIN (block) = BLOCK_SUBBLOCKS (current_block);
		  BLOCK_SUBBLOCKS (current_block) = block;
		  current_block = origin;
		}
	      VEC_safe_push (tree, heap, *p_block_stack, block);
	    }
	  else if (NOTE_KIND (insn) == NOTE_INSN_BLOCK_END)
	    {
	      NOTE_BLOCK (insn) = VEC_pop (tree, *p_block_stack);
	      BLOCK_SUBBLOCKS (current_block)
		= blocks_nreverse (BLOCK_SUBBLOCKS (current_block));
	      current_block = BLOCK_SUPERCONTEXT (current_block);
	    }
	}
    }
}

/* Reverse the order of elements in the chain T of blocks,
   and return the new head of the chain (old last element).  */

tree
blocks_nreverse (tree t)
{
  tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      next = BLOCK_CHAIN (decl);
      BLOCK_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Count the subblocks of the list starting with BLOCK.  If VECTOR is
   non-NULL, list them all into VECTOR, in a depth-first preorder
   traversal of the block tree.  Also clear TREE_ASM_WRITTEN in all
   blocks.  */

static int
all_blocks (tree block, tree *vector)
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

/* Return a vector containing all the blocks rooted at BLOCK.  The
   number of elements in the vector is stored in N_BLOCKS_P.  The
   vector is dynamically allocated; it is the caller's responsibility
   to call `free' on the pointer returned.  */

static tree *
get_block_vector (tree block, int *n_blocks_p)
{
  tree *block_vector;

  *n_blocks_p = all_blocks (block, NULL);
  block_vector = XNEWVEC (tree, *n_blocks_p);
  all_blocks (block, block_vector);

  return block_vector;
}

static GTY(()) int next_block_index = 2;

/* Set BLOCK_NUMBER for all the blocks in FN.  */

void
number_blocks (tree fn)
{
  int i;
  int n_blocks;
  tree *block_vector;

  /* For SDB and XCOFF debugging output, we start numbering the blocks
     from 1 within each function, rather than keeping a running
     count.  */
#if defined (SDB_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  if (write_symbols == SDB_DEBUG || write_symbols == XCOFF_DEBUG)
    next_block_index = 1;
#endif

  block_vector = get_block_vector (DECL_INITIAL (fn), &n_blocks);

  /* The top-level BLOCK isn't numbered at all.  */
  for (i = 1; i < n_blocks; ++i)
    /* We number the blocks from two.  */
    BLOCK_NUMBER (block_vector[i]) = next_block_index++;

  free (block_vector);

  return;
}

/* If VAR is present in a subblock of BLOCK, return the subblock.  */

tree
debug_find_var_in_block_tree (tree var, tree block)
{
  tree t;

  for (t = BLOCK_VARS (block); t; t = TREE_CHAIN (t))
    if (t == var)
      return block;

  for (t = BLOCK_SUBBLOCKS (block); t; t = TREE_CHAIN (t))
    {
      tree ret = debug_find_var_in_block_tree (var, t);
      if (ret)
	return ret;
    }

  return NULL_TREE;
}

/* Keep track of whether we're in a dummy function context.  If we are,
   we don't want to invoke the set_current_function hook, because we'll
   get into trouble if the hook calls target_reinit () recursively or
   when the initial initialization is not yet complete.  */

static bool in_dummy_function;

/* Invoke the target hook when setting cfun.  Update the optimization options
   if the function uses different options than the default.  */

static void
invoke_set_current_function_hook (tree fndecl)
{
  if (!in_dummy_function)
    {
      tree opts = ((fndecl)
		   ? DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl)
		   : optimization_default_node);

      if (!opts)
	opts = optimization_default_node;

      /* Change optimization options if needed.  */
      if (optimization_current_node != opts)
	{
	  optimization_current_node = opts;
	  cl_optimization_restore (TREE_OPTIMIZATION (opts));
	}

      targetm.set_current_function (fndecl);
    }
}

/* cfun should never be set directly; use this function.  */

void
set_cfun (struct function *new_cfun)
{
  if (cfun != new_cfun)
    {
      cfun = new_cfun;
      invoke_set_current_function_hook (new_cfun ? new_cfun->decl : NULL_TREE);
    }
}

/* Initialized with NOGC, making this poisonous to the garbage collector.  */

static VEC(function_p,heap) *cfun_stack;

/* Push the current cfun onto the stack, and set cfun to new_cfun.  */

void
push_cfun (struct function *new_cfun)
{
  VEC_safe_push (function_p, heap, cfun_stack, cfun);
  set_cfun (new_cfun);
}

/* Pop cfun from the stack.  */

void
pop_cfun (void)
{
  struct function *new_cfun = VEC_pop (function_p, cfun_stack);
  set_cfun (new_cfun);
}

/* Return value of funcdef and increase it.  */
int
get_next_funcdef_no (void)
{
  return funcdef_no++;
}

/* Allocate a function structure for FNDECL and set its contents
   to the defaults.  Set cfun to the newly-allocated object.
   Some of the helper functions invoked during initialization assume
   that cfun has already been set.  Therefore, assign the new object
   directly into cfun and invoke the back end hook explicitly at the
   very end, rather than initializing a temporary and calling set_cfun
   on it.

   ABSTRACT_P is true if this is a function that will never be seen by
   the middle-end.  Such functions are front-end concepts (like C++
   function templates) that do not correspond directly to functions
   placed in object files.  */

void
allocate_struct_function (tree fndecl, bool abstract_p)
{
  tree result;
  tree fntype = fndecl ? TREE_TYPE (fndecl) : NULL_TREE;

  cfun = GGC_CNEW (struct function);

  cfun->function_frequency = FUNCTION_FREQUENCY_NORMAL;

  init_eh_for_function ();

  if (init_machine_status)
    cfun->machine = (*init_machine_status) ();

#ifdef OVERRIDE_ABI_FORMAT
  OVERRIDE_ABI_FORMAT (fndecl);
#endif

  invoke_set_current_function_hook (fndecl);

  if (fndecl != NULL_TREE)
    {
      DECL_STRUCT_FUNCTION (fndecl) = cfun;
      cfun->decl = fndecl;
      current_function_funcdef_no = get_next_funcdef_no ();

      result = DECL_RESULT (fndecl);
      if (!abstract_p && aggregate_value_p (result, fndecl))
	{
#ifdef PCC_STATIC_STRUCT_RETURN
	  cfun->returns_pcc_struct = 1;
#endif
	  cfun->returns_struct = 1;
	}

      cfun->stdarg
	= (fntype
	   && TYPE_ARG_TYPES (fntype) != 0
	   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	       != void_type_node));

      /* Assume all registers in stdarg functions need to be saved.  */
      cfun->va_list_gpr_size = VA_LIST_MAX_GPR_SIZE;
      cfun->va_list_fpr_size = VA_LIST_MAX_FPR_SIZE;
    }
}

/* This is like allocate_struct_function, but pushes a new cfun for FNDECL
   instead of just setting it.  */

void
push_struct_function (tree fndecl)
{
  VEC_safe_push (function_p, heap, cfun_stack, cfun);
  allocate_struct_function (fndecl, false);
}

/* Reset cfun, and other non-struct-function variables to defaults as
   appropriate for emitting rtl at the start of a function.  */

static void
prepare_function_start (void)
{
  gcc_assert (!crtl->emit.x_last_insn);
  init_temp_slots ();
  init_emit ();
  init_varasm_status ();
  init_expr ();
  default_rtl_profile ();

  cse_not_expected = ! optimize;

  /* Caller save not needed yet.  */
  caller_save_needed = 0;

  /* We haven't done register allocation yet.  */
  reg_renumber = 0;

  /* Indicate that we have not instantiated virtual registers yet.  */
  virtuals_instantiated = 0;

  /* Indicate that we want CONCATs now.  */
  generating_concat_p = 1;

  /* Indicate we have no need of a frame pointer yet.  */
  frame_pointer_needed = 0;
}

/* Initialize the rtl expansion mechanism so that we can do simple things
   like generate sequences.  This is used to provide a context during global
   initialization of some passes.  You must call expand_dummy_function_end
   to exit this context.  */

void
init_dummy_function_start (void)
{
  gcc_assert (!in_dummy_function);
  in_dummy_function = true;
  push_struct_function (NULL_TREE);
  prepare_function_start ();
}

/* Generate RTL for the start of the function SUBR (a FUNCTION_DECL tree node)
   and initialize static variables for generating RTL for the statements
   of the function.  */

void
init_function_start (tree subr)
{
  if (subr && DECL_STRUCT_FUNCTION (subr))
    set_cfun (DECL_STRUCT_FUNCTION (subr));
  else
    allocate_struct_function (subr, false);
  prepare_function_start ();

  /* Warn if this value is an aggregate type,
     regardless of which calling convention we are using for it.  */
  if (AGGREGATE_TYPE_P (TREE_TYPE (DECL_RESULT (subr))))
    warning (OPT_Waggregate_return, "function returns an aggregate");
}

/* Make sure all values used by the optimization passes have sane defaults.  */
unsigned int
init_function_for_compilation (void)
{
  reg_renumber = 0;
  return 0;
}

struct rtl_opt_pass pass_init_function =
{
 {
  RTL_PASS,
  "*init_function",                     /* name */
  NULL,                                 /* gate */
  init_function_for_compilation,        /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};


void
expand_main_function (void)
{
#if (defined(INVOKE__main)				\
     || (!defined(HAS_INIT_SECTION)			\
	 && !defined(INIT_SECTION_ASM_OP)		\
	 && !defined(INIT_ARRAY_SECTION_ASM_OP)))
  emit_library_call (init_one_libfunc (NAME__MAIN), LCT_NORMAL, VOIDmode, 0);
#endif
}

/* Expand code to initialize the stack_protect_guard.  This is invoked at
   the beginning of a function to be protected.  */

#ifndef HAVE_stack_protect_set
# define HAVE_stack_protect_set		0
# define gen_stack_protect_set(x,y)	(gcc_unreachable (), NULL_RTX)
#endif

void
stack_protect_prologue (void)
{
  tree guard_decl = targetm.stack_protect_guard ();
  rtx x, y;

  x = expand_normal (crtl->stack_protect_guard);
  y = expand_normal (guard_decl);

  /* Allow the target to copy from Y to X without leaking Y into a
     register.  */
  if (HAVE_stack_protect_set)
    {
      rtx insn = gen_stack_protect_set (x, y);
      if (insn)
	{
	  emit_insn (insn);
	  return;
	}
    }

  /* Otherwise do a straight move.  */
  emit_move_insn (x, y);
}

/* Expand code to verify the stack_protect_guard.  This is invoked at
   the end of a function to be protected.  */

#ifndef HAVE_stack_protect_test
# define HAVE_stack_protect_test		0
# define gen_stack_protect_test(x, y, z)	(gcc_unreachable (), NULL_RTX)
#endif

void
stack_protect_epilogue (void)
{
  tree guard_decl = targetm.stack_protect_guard ();
  rtx label = gen_label_rtx ();
  rtx x, y, tmp;

  x = expand_normal (crtl->stack_protect_guard);
  y = expand_normal (guard_decl);

  /* Allow the target to compare Y with X without leaking either into
     a register.  */
  switch (HAVE_stack_protect_test != 0)
    {
    case 1:
      tmp = gen_stack_protect_test (x, y, label);
      if (tmp)
	{
	  emit_insn (tmp);
	  break;
	}
      /* FALLTHRU */

    default:
      emit_cmp_and_jump_insns (x, y, EQ, NULL_RTX, ptr_mode, 1, label);
      break;
    }

  /* The noreturn predictor has been moved to the tree level.  The rtl-level
     predictors estimate this branch about 20%, which isn't enough to get
     things moved out of line.  Since this is the only extant case of adding
     a noreturn function at the rtl level, it doesn't seem worth doing ought
     except adding the prediction by hand.  */
  tmp = get_last_insn ();
  if (JUMP_P (tmp))
    predict_insn_def (tmp, PRED_NORETURN, TAKEN);

  expand_expr_stmt (targetm.stack_protect_fail ());
  emit_label (label);
}

/* Start the RTL for a new function, and set variables used for
   emitting RTL.
   SUBR is the FUNCTION_DECL node.
   PARMS_HAVE_CLEANUPS is nonzero if there are cleanups associated with
   the function's parameters, which must be run at any return statement.  */

void
expand_function_start (tree subr)
{
  /* Make sure volatile mem refs aren't considered
     valid operands of arithmetic insns.  */
  init_recog_no_volatile ();

  crtl->profile
    = (profile_flag
       && ! DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (subr));

  crtl->limit_stack
    = (stack_limit_rtx != NULL_RTX && ! DECL_NO_LIMIT_STACK (subr));

  /* Make the label for return statements to jump to.  Do not special
     case machines with special return instructions -- they will be
     handled later during jump, ifcvt, or epilogue creation.  */
  return_label = gen_label_rtx ();

  /* Initialize rtx used to return the value.  */
  /* Do this before assign_parms so that we copy the struct value address
     before any library calls that assign parms might generate.  */

  /* Decide whether to return the value in memory or in a register.  */
  if (aggregate_value_p (DECL_RESULT (subr), subr))
    {
      /* Returning something that won't go in a register.  */
      rtx value_address = 0;

#ifdef PCC_STATIC_STRUCT_RETURN
      if (cfun->returns_pcc_struct)
	{
	  int size = int_size_in_bytes (TREE_TYPE (DECL_RESULT (subr)));
	  value_address = assemble_static_space (size);
	}
      else
#endif
	{
	  rtx sv = targetm.calls.struct_value_rtx (TREE_TYPE (subr), 2);
	  /* Expect to be passed the address of a place to store the value.
	     If it is passed as an argument, assign_parms will take care of
	     it.  */
	  if (sv)
	    {
	      value_address = gen_reg_rtx (Pmode);
	      emit_move_insn (value_address, sv);
	    }
	}
      if (value_address)
	{
	  rtx x = value_address;
	  if (!DECL_BY_REFERENCE (DECL_RESULT (subr)))
	    {
	      x = gen_rtx_MEM (DECL_MODE (DECL_RESULT (subr)), x);
	      set_mem_attributes (x, DECL_RESULT (subr), 1);
	    }
	  SET_DECL_RTL (DECL_RESULT (subr), x);
	}
    }
  else if (DECL_MODE (DECL_RESULT (subr)) == VOIDmode)
    /* If return mode is void, this decl rtl should not be used.  */
    SET_DECL_RTL (DECL_RESULT (subr), NULL_RTX);
  else
    {
      /* Compute the return values into a pseudo reg, which we will copy
	 into the true return register after the cleanups are done.  */
      tree return_type = TREE_TYPE (DECL_RESULT (subr));
      if (TYPE_MODE (return_type) != BLKmode
	  && targetm.calls.return_in_msb (return_type))
	/* expand_function_end will insert the appropriate padding in
	   this case.  Use the return value's natural (unpadded) mode
	   within the function proper.  */
	SET_DECL_RTL (DECL_RESULT (subr),
		      gen_reg_rtx (TYPE_MODE (return_type)));
      else
	{
	  /* In order to figure out what mode to use for the pseudo, we
	     figure out what the mode of the eventual return register will
	     actually be, and use that.  */
	  rtx hard_reg = hard_function_value (return_type, subr, 0, 1);

	  /* Structures that are returned in registers are not
	     aggregate_value_p, so we may see a PARALLEL or a REG.  */
	  if (REG_P (hard_reg))
	    SET_DECL_RTL (DECL_RESULT (subr),
			  gen_reg_rtx (GET_MODE (hard_reg)));
	  else
	    {
	      gcc_assert (GET_CODE (hard_reg) == PARALLEL);
	      SET_DECL_RTL (DECL_RESULT (subr), gen_group_rtx (hard_reg));
	    }
	}

      /* Set DECL_REGISTER flag so that expand_function_end will copy the
	 result to the real return register(s).  */
      DECL_REGISTER (DECL_RESULT (subr)) = 1;
    }

  /* Initialize rtx for parameters and local variables.
     In some cases this requires emitting insns.  */
  assign_parms (subr);

  /* If function gets a static chain arg, store it.  */
  if (cfun->static_chain_decl)
    {
      tree parm = cfun->static_chain_decl;
      rtx local, chain, insn;

      local = gen_reg_rtx (Pmode);
      chain = targetm.calls.static_chain (current_function_decl, true);

      set_decl_incoming_rtl (parm, chain, false);
      SET_DECL_RTL (parm, local);
      mark_reg_pointer (local, TYPE_ALIGN (TREE_TYPE (TREE_TYPE (parm))));

      insn = emit_move_insn (local, chain);

      /* Mark the register as eliminable, similar to parameters.  */
      if (MEM_P (chain)
	  && reg_mentioned_p (arg_pointer_rtx, XEXP (chain, 0)))
	set_unique_reg_note (insn, REG_EQUIV, chain);
    }

  /* If the function receives a non-local goto, then store the
     bits we need to restore the frame pointer.  */
  if (cfun->nonlocal_goto_save_area)
    {
      tree t_save;
      rtx r_save;

      /* ??? We need to do this save early.  Unfortunately here is
	 before the frame variable gets declared.  Help out...  */
      tree var = TREE_OPERAND (cfun->nonlocal_goto_save_area, 0);
      if (!DECL_RTL_SET_P (var))
	expand_decl (var);

      t_save = build4 (ARRAY_REF, ptr_type_node,
		       cfun->nonlocal_goto_save_area,
		       integer_zero_node, NULL_TREE, NULL_TREE);
      r_save = expand_expr (t_save, NULL_RTX, VOIDmode, EXPAND_WRITE);
      r_save = convert_memory_address (Pmode, r_save);

      emit_move_insn (r_save, targetm.builtin_setjmp_frame_value ());
      update_nonlocal_goto_save_area ();
    }

  /* The following was moved from init_function_start.
     The move is supposed to make sdb output more accurate.  */
  /* Indicate the beginning of the function body,
     as opposed to parm setup.  */
  emit_note (NOTE_INSN_FUNCTION_BEG);

  gcc_assert (NOTE_P (get_last_insn ()));

  parm_birth_insn = get_last_insn ();

  if (crtl->profile)
    {
#ifdef PROFILE_HOOK
      PROFILE_HOOK (current_function_funcdef_no);
#endif
    }

  /* After the display initializations is where the stack checking
     probe should go.  */
  if(flag_stack_check)
    stack_check_probe_note = emit_note (NOTE_INSN_DELETED);

  /* Make sure there is a line number after the function entry setup code.  */
  force_next_line_note ();
}

/* Undo the effects of init_dummy_function_start.  */
void
expand_dummy_function_end (void)
{
  gcc_assert (in_dummy_function);

  /* End any sequences that failed to be closed due to syntax errors.  */
  while (in_sequence_p ())
    end_sequence ();

  /* Outside function body, can't compute type's actual size
     until next function's body starts.  */

  free_after_parsing (cfun);
  free_after_compilation (cfun);
  pop_cfun ();
  in_dummy_function = false;
}

/* Call DOIT for each hard register used as a return value from
   the current function.  */

void
diddle_return_value (void (*doit) (rtx, void *), void *arg)
{
  rtx outgoing = crtl->return_rtx;

  if (! outgoing)
    return;

  if (REG_P (outgoing))
    (*doit) (outgoing, arg);
  else if (GET_CODE (outgoing) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (outgoing, 0); i++)
	{
	  rtx x = XEXP (XVECEXP (outgoing, 0, i), 0);

	  if (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER)
	    (*doit) (x, arg);
	}
    }
}

static void
do_clobber_return_reg (rtx reg, void *arg ATTRIBUTE_UNUSED)
{
  emit_clobber (reg);
}

void
clobber_return_register (void)
{
  diddle_return_value (do_clobber_return_reg, NULL);

  /* In case we do use pseudo to return value, clobber it too.  */
  if (DECL_RTL_SET_P (DECL_RESULT (current_function_decl)))
    {
      tree decl_result = DECL_RESULT (current_function_decl);
      rtx decl_rtl = DECL_RTL (decl_result);
      if (REG_P (decl_rtl) && REGNO (decl_rtl) >= FIRST_PSEUDO_REGISTER)
	{
	  do_clobber_return_reg (decl_rtl, NULL);
	}
    }
}

static void
do_use_return_reg (rtx reg, void *arg ATTRIBUTE_UNUSED)
{
  emit_use (reg);
}

static void
use_return_register (void)
{
  diddle_return_value (do_use_return_reg, NULL);
}

/* Possibly warn about unused parameters.  */
void
do_warn_unused_parameter (tree fn)
{
  tree decl;

  for (decl = DECL_ARGUMENTS (fn);
       decl; decl = TREE_CHAIN (decl))
    if (!TREE_USED (decl) && TREE_CODE (decl) == PARM_DECL
	&& DECL_NAME (decl) && !DECL_ARTIFICIAL (decl)
	&& !TREE_NO_WARNING (decl))
      warning (OPT_Wunused_parameter, "unused parameter %q+D", decl);
}

static GTY(()) rtx initial_trampoline;

/* Generate RTL for the end of the current function.  */

void
expand_function_end (void)
{
  rtx clobber_after;

  /* If arg_pointer_save_area was referenced only from a nested
     function, we will not have initialized it yet.  Do that now.  */
  if (arg_pointer_save_area && ! crtl->arg_pointer_save_area_init)
    get_arg_pointer_save_area ();

  /* If we are doing generic stack checking and this function makes calls,
     do a stack probe at the start of the function to ensure we have enough
     space for another stack frame.  */
  if (flag_stack_check == GENERIC_STACK_CHECK)
    {
      rtx insn, seq;

      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (CALL_P (insn))
	  {
	    rtx max_frame_size = GEN_INT (STACK_CHECK_MAX_FRAME_SIZE);
	    start_sequence ();
	    if (STACK_CHECK_MOVING_SP)
	      anti_adjust_stack_and_probe (max_frame_size, true);
	    else
	      probe_stack_range (STACK_OLD_CHECK_PROTECT, max_frame_size);
	    seq = get_insns ();
	    end_sequence ();
	    emit_insn_before (seq, stack_check_probe_note);
	    break;
	  }
    }

  /* End any sequences that failed to be closed due to syntax errors.  */
  while (in_sequence_p ())
    end_sequence ();

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();

  /* Output a linenumber for the end of the function.
     SDB depends on this.  */
  force_next_line_note ();
  set_curr_insn_source_location (input_location);

  /* Before the return label (if any), clobber the return
     registers so that they are not propagated live to the rest of
     the function.  This can only happen with functions that drop
     through; if there had been a return statement, there would
     have either been a return rtx, or a jump to the return label.

     We delay actual code generation after the current_function_value_rtx
     is computed.  */
  clobber_after = get_last_insn ();

  /* Output the label for the actual return from the function.  */
  emit_label (return_label);

  if (USING_SJLJ_EXCEPTIONS)
    {
      /* Let except.c know where it should emit the call to unregister
	 the function context for sjlj exceptions.  */
      if (flag_exceptions)
	sjlj_emit_function_exit_after (get_last_insn ());
    }
  else
    {
      /* We want to ensure that instructions that may trap are not
	 moved into the epilogue by scheduling, because we don't
	 always emit unwind information for the epilogue.  */
      if (flag_non_call_exceptions)
	emit_insn (gen_blockage ());
    }

  /* If this is an implementation of throw, do what's necessary to
     communicate between __builtin_eh_return and the epilogue.  */
  expand_eh_return ();

  /* If scalar return value was computed in a pseudo-reg, or was a named
     return value that got dumped to the stack, copy that to the hard
     return register.  */
  if (DECL_RTL_SET_P (DECL_RESULT (current_function_decl)))
    {
      tree decl_result = DECL_RESULT (current_function_decl);
      rtx decl_rtl = DECL_RTL (decl_result);

      if (REG_P (decl_rtl)
	  ? REGNO (decl_rtl) >= FIRST_PSEUDO_REGISTER
	  : DECL_REGISTER (decl_result))
	{
	  rtx real_decl_rtl = crtl->return_rtx;

	  /* This should be set in assign_parms.  */
	  gcc_assert (REG_FUNCTION_VALUE_P (real_decl_rtl));

	  /* If this is a BLKmode structure being returned in registers,
	     then use the mode computed in expand_return.  Note that if
	     decl_rtl is memory, then its mode may have been changed,
	     but that crtl->return_rtx has not.  */
	  if (GET_MODE (real_decl_rtl) == BLKmode)
	    PUT_MODE (real_decl_rtl, GET_MODE (decl_rtl));

	  /* If a non-BLKmode return value should be padded at the least
	     significant end of the register, shift it left by the appropriate
	     amount.  BLKmode results are handled using the group load/store
	     machinery.  */
	  if (TYPE_MODE (TREE_TYPE (decl_result)) != BLKmode
	      && targetm.calls.return_in_msb (TREE_TYPE (decl_result)))
	    {
	      emit_move_insn (gen_rtx_REG (GET_MODE (decl_rtl),
					   REGNO (real_decl_rtl)),
			      decl_rtl);
	      shift_return_value (GET_MODE (decl_rtl), true, real_decl_rtl);
	    }
	  /* If a named return value dumped decl_return to memory, then
	     we may need to re-do the PROMOTE_MODE signed/unsigned
	     extension.  */
	  else if (GET_MODE (real_decl_rtl) != GET_MODE (decl_rtl))
	    {
	      int unsignedp = TYPE_UNSIGNED (TREE_TYPE (decl_result));
	      promote_function_mode (TREE_TYPE (decl_result),
				     GET_MODE (decl_rtl), &unsignedp,
				     TREE_TYPE (current_function_decl), 1);

	      convert_move (real_decl_rtl, decl_rtl, unsignedp);
	    }
	  else if (GET_CODE (real_decl_rtl) == PARALLEL)
	    {
	      /* If expand_function_start has created a PARALLEL for decl_rtl,
		 move the result to the real return registers.  Otherwise, do
		 a group load from decl_rtl for a named return.  */
	      if (GET_CODE (decl_rtl) == PARALLEL)
		emit_group_move (real_decl_rtl, decl_rtl);
	      else
		emit_group_load (real_decl_rtl, decl_rtl,
				 TREE_TYPE (decl_result),
				 int_size_in_bytes (TREE_TYPE (decl_result)));
	    }
	  /* In the case of complex integer modes smaller than a word, we'll
	     need to generate some non-trivial bitfield insertions.  Do that
	     on a pseudo and not the hard register.  */
	  else if (GET_CODE (decl_rtl) == CONCAT
		   && GET_MODE_CLASS (GET_MODE (decl_rtl)) == MODE_COMPLEX_INT
		   && GET_MODE_BITSIZE (GET_MODE (decl_rtl)) <= BITS_PER_WORD)
	    {
	      int old_generating_concat_p;
	      rtx tmp;

	      old_generating_concat_p = generating_concat_p;
	      generating_concat_p = 0;
	      tmp = gen_reg_rtx (GET_MODE (decl_rtl));
	      generating_concat_p = old_generating_concat_p;

	      emit_move_insn (tmp, decl_rtl);
	      emit_move_insn (real_decl_rtl, tmp);
	    }
	  else
	    emit_move_insn (real_decl_rtl, decl_rtl);
	}
    }

  /* If returning a structure, arrange to return the address of the value
     in a place where debuggers expect to find it.

     If returning a structure PCC style,
     the caller also depends on this value.
     And cfun->returns_pcc_struct is not necessarily set.  */
  if (cfun->returns_struct
      || cfun->returns_pcc_struct)
    {
      rtx value_address = DECL_RTL (DECL_RESULT (current_function_decl));
      tree type = TREE_TYPE (DECL_RESULT (current_function_decl));
      rtx outgoing;

      if (DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
	type = TREE_TYPE (type);
      else
	value_address = XEXP (value_address, 0);

      outgoing = targetm.calls.function_value (build_pointer_type (type),
					       current_function_decl, true);

      /* Mark this as a function return value so integrate will delete the
	 assignment and USE below when inlining this function.  */
      REG_FUNCTION_VALUE_P (outgoing) = 1;

      /* The address may be ptr_mode and OUTGOING may be Pmode.  */
      value_address = convert_memory_address (GET_MODE (outgoing),
					      value_address);

      emit_move_insn (outgoing, value_address);

      /* Show return register used to hold result (in this case the address
	 of the result.  */
      crtl->return_rtx = outgoing;
    }

  /* Emit the actual code to clobber return register.  */
  {
    rtx seq;

    start_sequence ();
    clobber_return_register ();
    seq = get_insns ();
    end_sequence ();

    emit_insn_after (seq, clobber_after);
  }

  /* Output the label for the naked return from the function.  */
  if (naked_return_label)
    emit_label (naked_return_label);

  /* @@@ This is a kludge.  We want to ensure that instructions that
     may trap are not moved into the epilogue by scheduling, because
     we don't always emit unwind information for the epilogue.  */
  if (! USING_SJLJ_EXCEPTIONS && flag_non_call_exceptions)
    emit_insn (gen_blockage ());

  /* If stack protection is enabled for this function, check the guard.  */
  if (crtl->stack_protect_guard)
    stack_protect_epilogue ();

  /* If we had calls to alloca, and this machine needs
     an accurate stack pointer to exit the function,
     insert some code to save and restore the stack pointer.  */
  if (! EXIT_IGNORE_STACK
      && cfun->calls_alloca)
    {
      rtx tem = 0;

      emit_stack_save (SAVE_FUNCTION, &tem, parm_birth_insn);
      emit_stack_restore (SAVE_FUNCTION, tem, NULL_RTX);
    }

  /* ??? This should no longer be necessary since stupid is no longer with
     us, but there are some parts of the compiler (eg reload_combine, and
     sh mach_dep_reorg) that still try and compute their own lifetime info
     instead of using the general framework.  */
  use_return_register ();
}

rtx
get_arg_pointer_save_area (void)
{
  rtx ret = arg_pointer_save_area;

  if (! ret)
    {
      ret = assign_stack_local (Pmode, GET_MODE_SIZE (Pmode), 0);
      arg_pointer_save_area = ret;
    }

  if (! crtl->arg_pointer_save_area_init)
    {
      rtx seq;

      /* Save the arg pointer at the beginning of the function.  The
	 generated stack slot may not be a valid memory address, so we
	 have to check it and fix it if necessary.  */
      start_sequence ();
      emit_move_insn (validize_mem (ret),
                      crtl->args.internal_arg_pointer);
      seq = get_insns ();
      end_sequence ();

      push_topmost_sequence ();
      emit_insn_after (seq, entry_of_function ());
      pop_topmost_sequence ();
    }

  return ret;
}

/* Add a list of INSNS to the hash HASHP, possibly allocating HASHP
   for the first time.  */

static void
record_insns (rtx insns, rtx end, htab_t *hashp)
{
  rtx tmp;
  htab_t hash = *hashp;

  if (hash == NULL)
    *hashp = hash
      = htab_create_ggc (17, htab_hash_pointer, htab_eq_pointer, NULL);

  for (tmp = insns; tmp != end; tmp = NEXT_INSN (tmp))
    {
      void **slot = htab_find_slot (hash, tmp, INSERT);
      gcc_assert (*slot == NULL);
      *slot = tmp;
    }
}

/* INSN has been duplicated as COPY, as part of duping a basic block.
   If INSN is an epilogue insn, then record COPY as epilogue as well.  */

void
maybe_copy_epilogue_insn (rtx insn, rtx copy)
{
  void **slot;

  if (epilogue_insn_hash == NULL
      || htab_find (epilogue_insn_hash, insn) == NULL)
    return;

  slot = htab_find_slot (epilogue_insn_hash, copy, INSERT);
  gcc_assert (*slot == NULL);
  *slot = copy;
}

/* Set the locator of the insn chain starting at INSN to LOC.  */
static void
set_insn_locators (rtx insn, int loc)
{
  while (insn != NULL_RTX)
    {
      if (INSN_P (insn))
	INSN_LOCATOR (insn) = loc;
      insn = NEXT_INSN (insn);
    }
}

/* Determine if any INSNs in HASH are, or are part of, INSN.  Because
   we can be running after reorg, SEQUENCE rtl is possible.  */

static bool
contains (const_rtx insn, htab_t hash)
{
  if (hash == NULL)
    return false;

  if (NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      int i;
      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	if (htab_find (hash, XVECEXP (PATTERN (insn), 0, i)))
	  return true;
      return false;
    }

  return htab_find (hash, insn) != NULL;
}

int
prologue_epilogue_contains (const_rtx insn)
{
  if (contains (insn, prologue_insn_hash))
    return 1;
  if (contains (insn, epilogue_insn_hash))
    return 1;
  return 0;
}

#ifdef HAVE_return
/* Insert gen_return at the end of block BB.  This also means updating
   block_for_insn appropriately.  */

static void
emit_return_into_block (basic_block bb)
{
  emit_jump_insn_after (gen_return (), BB_END (bb));
}
#endif /* HAVE_return */

/* Generate the prologue and epilogue RTL if the machine supports it.  Thread
   this into place with notes indicating where the prologue ends and where
   the epilogue begins.  Update the basic block information when possible.  */

static void
thread_prologue_and_epilogue_insns (void)
{
  int inserted = 0;
  edge e;
#if defined (HAVE_sibcall_epilogue) || defined (HAVE_epilogue) || defined (HAVE_return) || defined (HAVE_prologue)
  rtx seq;
#endif
#if defined (HAVE_epilogue) || defined(HAVE_return)
  rtx epilogue_end = NULL_RTX;
#endif
  edge_iterator ei;

  rtl_profile_for_bb (ENTRY_BLOCK_PTR);
#ifdef HAVE_prologue
  if (HAVE_prologue)
    {
      start_sequence ();
      seq = gen_prologue ();
      emit_insn (seq);

      /* Insert an explicit USE for the frame pointer
         if the profiling is on and the frame pointer is required.  */
      if (crtl->profile && frame_pointer_needed)
	emit_use (hard_frame_pointer_rtx);

      /* Retain a map of the prologue insns.  */
      record_insns (seq, NULL, &prologue_insn_hash);
      emit_note (NOTE_INSN_PROLOGUE_END);

#ifndef PROFILE_BEFORE_PROLOGUE
      /* Ensure that instructions are not moved into the prologue when
	 profiling is on.  The call to the profiling routine can be
	 emitted within the live range of a call-clobbered register.  */
      if (crtl->profile)
        emit_insn (gen_blockage ());
#endif

      seq = get_insns ();
      end_sequence ();
      set_insn_locators (seq, prologue_locator);

      /* Can't deal with multiple successors of the entry block
         at the moment.  Function should always have at least one
         entry point.  */
      gcc_assert (single_succ_p (ENTRY_BLOCK_PTR));

      insert_insn_on_edge (seq, single_succ_edge (ENTRY_BLOCK_PTR));
      inserted = 1;
    }
#endif

  /* If the exit block has no non-fake predecessors, we don't need
     an epilogue.  */
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    if ((e->flags & EDGE_FAKE) == 0)
      break;
  if (e == NULL)
    goto epilogue_done;

  rtl_profile_for_bb (EXIT_BLOCK_PTR);
#ifdef HAVE_return
  if (optimize && HAVE_return)
    {
      /* If we're allowed to generate a simple return instruction,
	 then by definition we don't need a full epilogue.  Examine
	 the block that falls through to EXIT.   If it does not
	 contain any code, examine its predecessors and try to
	 emit (conditional) return instructions.  */

      basic_block last;
      rtx label;

      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
	if (e->flags & EDGE_FALLTHRU)
	  break;
      if (e == NULL)
	goto epilogue_done;
      last = e->src;

      /* Verify that there are no active instructions in the last block.  */
      label = BB_END (last);
      while (label && !LABEL_P (label))
	{
	  if (active_insn_p (label))
	    break;
	  label = PREV_INSN (label);
	}

      if (BB_HEAD (last) == label && LABEL_P (label))
	{
	  edge_iterator ei2;

	  for (ei2 = ei_start (last->preds); (e = ei_safe_edge (ei2)); )
	    {
	      basic_block bb = e->src;
	      rtx jump;

	      if (bb == ENTRY_BLOCK_PTR)
		{
		  ei_next (&ei2);
		  continue;
		}

	      jump = BB_END (bb);
	      if (!JUMP_P (jump) || JUMP_LABEL (jump) != label)
		{
		  ei_next (&ei2);
		  continue;
		}

	      /* If we have an unconditional jump, we can replace that
		 with a simple return instruction.  */
	      if (simplejump_p (jump))
		{
		  emit_return_into_block (bb);
		  delete_insn (jump);
		}

	      /* If we have a conditional jump, we can try to replace
		 that with a conditional return instruction.  */
	      else if (condjump_p (jump))
		{
		  if (! redirect_jump (jump, 0, 0))
		    {
		      ei_next (&ei2);
		      continue;
		    }

		  /* If this block has only one successor, it both jumps
		     and falls through to the fallthru block, so we can't
		     delete the edge.  */
		  if (single_succ_p (bb))
		    {
		      ei_next (&ei2);
		      continue;
		    }
		}
	      else
		{
		  ei_next (&ei2);
		  continue;
		}

	      /* Fix up the CFG for the successful change we just made.  */
	      redirect_edge_succ (e, EXIT_BLOCK_PTR);
	    }

	  /* Emit a return insn for the exit fallthru block.  Whether
	     this is still reachable will be determined later.  */

	  emit_barrier_after (BB_END (last));
	  emit_return_into_block (last);
	  epilogue_end = BB_END (last);
	  single_succ_edge (last)->flags &= ~EDGE_FALLTHRU;
	  goto epilogue_done;
	}
    }
#endif

  /* A small fib -- epilogue is not yet completed, but we wish to re-use
     this marker for the splits of EH_RETURN patterns, and nothing else
     uses the flag in the meantime.  */
  epilogue_completed = 1;

#ifdef HAVE_eh_return
  /* Find non-fallthru edges that end with EH_RETURN instructions.  On
     some targets, these get split to a special version of the epilogue
     code.  In order to be able to properly annotate these with unwind
     info, try to split them now.  If we get a valid split, drop an
     EPILOGUE_BEG note and mark the insns as epilogue insns.  */
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    {
      rtx prev, last, trial;

      if (e->flags & EDGE_FALLTHRU)
	continue;
      last = BB_END (e->src);
      if (!eh_returnjump_p (last))
	continue;

      prev = PREV_INSN (last);
      trial = try_split (PATTERN (last), last, 1);
      if (trial == last)
	continue;

      record_insns (NEXT_INSN (prev), NEXT_INSN (trial), &epilogue_insn_hash);
      emit_note_after (NOTE_INSN_EPILOGUE_BEG, prev);
    }
#endif

  /* Find the edge that falls through to EXIT.  Other edges may exist
     due to RETURN instructions, but those don't need epilogues.
     There really shouldn't be a mixture -- either all should have
     been converted or none, however...  */

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    if (e->flags & EDGE_FALLTHRU)
      break;
  if (e == NULL)
    goto epilogue_done;

#ifdef HAVE_epilogue
  if (HAVE_epilogue)
    {
      start_sequence ();
      epilogue_end = emit_note (NOTE_INSN_EPILOGUE_BEG);
      seq = gen_epilogue ();
      emit_jump_insn (seq);

      /* Retain a map of the epilogue insns.  */
      record_insns (seq, NULL, &epilogue_insn_hash);
      set_insn_locators (seq, epilogue_locator);

      seq = get_insns ();
      end_sequence ();

      insert_insn_on_edge (seq, e);
      inserted = 1;
    }
  else
#endif
    {
      basic_block cur_bb;

      if (! next_active_insn (BB_END (e->src)))
	goto epilogue_done;
      /* We have a fall-through edge to the exit block, the source is not
         at the end of the function, and there will be an assembler epilogue
         at the end of the function.
         We can't use force_nonfallthru here, because that would try to
         use return.  Inserting a jump 'by hand' is extremely messy, so
	 we take advantage of cfg_layout_finalize using
	fixup_fallthru_exit_predecessor.  */
      cfg_layout_initialize (0);
      FOR_EACH_BB (cur_bb)
	if (cur_bb->index >= NUM_FIXED_BLOCKS
	    && cur_bb->next_bb->index >= NUM_FIXED_BLOCKS)
	  cur_bb->aux = cur_bb->next_bb;
      cfg_layout_finalize ();
    }
epilogue_done:
  default_rtl_profile ();

  if (inserted)
    {
      commit_edge_insertions ();

      /* The epilogue insns we inserted may cause the exit edge to no longer
	 be fallthru.  */
      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
	{
	  if (((e->flags & EDGE_FALLTHRU) != 0)
	      && returnjump_p (BB_END (e->src)))
	    e->flags &= ~EDGE_FALLTHRU;
	}
    }

#ifdef HAVE_sibcall_epilogue
  /* Emit sibling epilogues before any sibling call sites.  */
  for (ei = ei_start (EXIT_BLOCK_PTR->preds); (e = ei_safe_edge (ei)); )
    {
      basic_block bb = e->src;
      rtx insn = BB_END (bb);

      if (!CALL_P (insn)
	  || ! SIBLING_CALL_P (insn))
	{
	  ei_next (&ei);
	  continue;
	}

      start_sequence ();
      emit_note (NOTE_INSN_EPILOGUE_BEG);
      emit_insn (gen_sibcall_epilogue ());
      seq = get_insns ();
      end_sequence ();

      /* Retain a map of the epilogue insns.  Used in life analysis to
	 avoid getting rid of sibcall epilogue insns.  Do this before we
	 actually emit the sequence.  */
      record_insns (seq, NULL, &epilogue_insn_hash);
      set_insn_locators (seq, epilogue_locator);

      emit_insn_before (seq, insn);
      ei_next (&ei);
    }
#endif

#ifdef HAVE_epilogue
  if (epilogue_end)
    {
      rtx insn, next;

      /* Similarly, move any line notes that appear after the epilogue.
         There is no need, however, to be quite so anal about the existence
	 of such a note.  Also possibly move
	 NOTE_INSN_FUNCTION_BEG notes, as those can be relevant for debug
	 info generation.  */
      for (insn = epilogue_end; insn; insn = next)
	{
	  next = NEXT_INSN (insn);
	  if (NOTE_P (insn)
	      && (NOTE_KIND (insn) == NOTE_INSN_FUNCTION_BEG))
	    reorder_insns (insn, insn, PREV_INSN (epilogue_end));
	}
    }
#endif

  /* Threading the prologue and epilogue changes the artificial refs
     in the entry and exit blocks.  */
  epilogue_completed = 1;
  df_update_entry_exit_and_calls ();
}

/* Reposition the prologue-end and epilogue-begin notes after
   instruction scheduling.  */

void
reposition_prologue_and_epilogue_notes (void)
{
#if defined (HAVE_prologue) || defined (HAVE_epilogue) \
    || defined (HAVE_sibcall_epilogue)
  /* Since the hash table is created on demand, the fact that it is
     non-null is a signal that it is non-empty.  */
  if (prologue_insn_hash != NULL)
    {
      size_t len = htab_elements (prologue_insn_hash);
      rtx insn, last = NULL, note = NULL;

      /* Scan from the beginning until we reach the last prologue insn.  */
      /* ??? While we do have the CFG intact, there are two problems:
	 (1) The prologue can contain loops (typically probing the stack),
	     which means that the end of the prologue isn't in the first bb.
	 (2) Sometimes the PROLOGUE_END note gets pushed into the next bb.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  if (NOTE_P (insn))
	    {
	      if (NOTE_KIND (insn) == NOTE_INSN_PROLOGUE_END)
		note = insn;
	    }
	  else if (contains (insn, prologue_insn_hash))
	    {
	      last = insn;
	      if (--len == 0)
		break;
	    }
	}

      if (last)
	{
	  if (note == NULL)
	    {
	      /* Scan forward looking for the PROLOGUE_END note.  It should
		 be right at the beginning of the block, possibly with other
		 insn notes that got moved there.  */
	      for (note = NEXT_INSN (last); ; note = NEXT_INSN (note))
		{
		  if (NOTE_P (note)
		      && NOTE_KIND (note) == NOTE_INSN_PROLOGUE_END)
		    break;
		}
	    }

	  /* Avoid placing note between CODE_LABEL and BASIC_BLOCK note.  */
	  if (LABEL_P (last))
	    last = NEXT_INSN (last);
	  reorder_insns (note, note, last);
	}
    }

  if (epilogue_insn_hash != NULL)
    {
      edge_iterator ei;
      edge e;

      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
	{
	  rtx insn, first = NULL, note = NULL;
	  basic_block bb = e->src;

	  /* Scan from the beginning until we reach the first epilogue insn. */
	  FOR_BB_INSNS (bb, insn)
	    {
	      if (NOTE_P (insn))
		{
		  if (NOTE_KIND (insn) == NOTE_INSN_EPILOGUE_BEG)
		    {
		      note = insn;
		      if (first != NULL)
			break;
		    }
		}
	      else if (first == NULL && contains (insn, epilogue_insn_hash))
		{
		  first = insn;
		  if (note != NULL)
		    break;
		}
	    }

	  if (note)
	    {
	      /* If the function has a single basic block, and no real
		 epilogue insns (e.g. sibcall with no cleanup), the
		 epilogue note can get scheduled before the prologue
		 note.  If we have frame related prologue insns, having
		 them scanned during the epilogue will result in a crash.
		 In this case re-order the epilogue note to just before
		 the last insn in the block.  */
	      if (first == NULL)
		first = BB_END (bb);

	      if (PREV_INSN (first) != note)
		reorder_insns (note, note, PREV_INSN (first));
	    }
	}
    }
#endif /* HAVE_prologue or HAVE_epilogue */
}

/* Returns the name of the current function.  */
const char *
current_function_name (void)
{
  if (cfun == NULL)
    return "<none>";
  return lang_hooks.decl_printable_name (cfun->decl, 2);
}


static unsigned int
rest_of_handle_check_leaf_regs (void)
{
#ifdef LEAF_REGISTERS
  current_function_uses_only_leaf_regs
    = optimize > 0 && only_leaf_regs_used () && leaf_function_p ();
#endif
  return 0;
}

/* Insert a TYPE into the used types hash table of CFUN.  */

static void
used_types_insert_helper (tree type, struct function *func)
{
  if (type != NULL && func != NULL)
    {
      void **slot;

      if (func->used_types_hash == NULL)
	func->used_types_hash = htab_create_ggc (37, htab_hash_pointer,
						 htab_eq_pointer, NULL);
      slot = htab_find_slot (func->used_types_hash, type, INSERT);
      if (*slot == NULL)
	*slot = type;
    }
}

/* Given a type, insert it into the used hash table in cfun.  */
void
used_types_insert (tree t)
{
  while (POINTER_TYPE_P (t) || TREE_CODE (t) == ARRAY_TYPE)
    t = TREE_TYPE (t);
  t = TYPE_MAIN_VARIANT (t);
  if (debug_info_level > DINFO_LEVEL_NONE)
    {
      if (cfun)
	used_types_insert_helper (t, cfun);
      else
	/* So this might be a type referenced by a global variable.
	   Record that type so that we can later decide to emit its debug
	   information.  */
	types_used_by_cur_var_decl =
	  tree_cons (t, NULL, types_used_by_cur_var_decl);

    }
}

/* Helper to Hash a struct types_used_by_vars_entry.  */

static hashval_t
hash_types_used_by_vars_entry (const struct types_used_by_vars_entry *entry)
{
  gcc_assert (entry && entry->var_decl && entry->type);

  return iterative_hash_object (entry->type,
				iterative_hash_object (entry->var_decl, 0));
}

/* Hash function of the types_used_by_vars_entry hash table.  */

hashval_t
types_used_by_vars_do_hash (const void *x)
{
  const struct types_used_by_vars_entry *entry =
    (const struct types_used_by_vars_entry *) x;

  return hash_types_used_by_vars_entry (entry);
}

/*Equality function of the types_used_by_vars_entry hash table.  */

int
types_used_by_vars_eq (const void *x1, const void *x2)
{
  const struct types_used_by_vars_entry *e1 =
    (const struct types_used_by_vars_entry *) x1;
  const struct types_used_by_vars_entry *e2 =
    (const struct types_used_by_vars_entry *)x2;

  return (e1->var_decl == e2->var_decl && e1->type == e2->type);
}

/* Inserts an entry into the types_used_by_vars_hash hash table. */

void
types_used_by_var_decl_insert (tree type, tree var_decl)
{
  if (type != NULL && var_decl != NULL)
    {
      void **slot;
      struct types_used_by_vars_entry e;
      e.var_decl = var_decl;
      e.type = type;
      if (types_used_by_vars_hash == NULL)
	types_used_by_vars_hash =
	  htab_create_ggc (37, types_used_by_vars_do_hash,
			   types_used_by_vars_eq, NULL);
      slot = htab_find_slot_with_hash (types_used_by_vars_hash, &e,
				       hash_types_used_by_vars_entry (&e), INSERT);
      if (*slot == NULL)
	{
	  struct types_used_by_vars_entry *entry;
	  entry = (struct types_used_by_vars_entry*) ggc_alloc
		    (sizeof (struct types_used_by_vars_entry));
	  entry->type = type;
	  entry->var_decl = var_decl;
	  *slot = entry;
	}
    }
}

struct rtl_opt_pass pass_leaf_regs =
{
 {
  RTL_PASS,
  "*leaf_regs",                         /* name */
  NULL,                                 /* gate */
  rest_of_handle_check_leaf_regs,       /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

static unsigned int
rest_of_handle_thread_prologue_and_epilogue (void)
{
  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE);
  /* On some machines, the prologue and epilogue code, or parts thereof,
     can be represented as RTL.  Doing so lets us schedule insns between
     it and the rest of the code and also allows delayed branch
     scheduling to operate in the epilogue.  */

  thread_prologue_and_epilogue_insns ();
  return 0;
}

struct rtl_opt_pass pass_thread_prologue_and_epilogue =
{
 {
  RTL_PASS,
  "pro_and_epilogue",                   /* name */
  NULL,                                 /* gate */
  rest_of_handle_thread_prologue_and_epilogue, /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_THREAD_PROLOGUE_AND_EPILOGUE,      /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  TODO_verify_flow,                     /* todo_flags_start */
  TODO_dump_func |
  TODO_df_verify |
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect                      /* todo_flags_finish */
 }
};


/* This mini-pass fixes fall-out from SSA in asm statements that have
   in-out constraints.  Say you start with

     orig = inout;
     asm ("": "+mr" (inout));
     use (orig);

   which is transformed very early to use explicit output and match operands:

     orig = inout;
     asm ("": "=mr" (inout) : "0" (inout));
     use (orig);

   Or, after SSA and copyprop,

     asm ("": "=mr" (inout_2) : "0" (inout_1));
     use (inout_1);

   Clearly inout_2 and inout_1 can't be coalesced easily anymore, as
   they represent two separate values, so they will get different pseudo
   registers during expansion.  Then, since the two operands need to match
   per the constraints, but use different pseudo registers, reload can
   only register a reload for these operands.  But reloads can only be
   satisfied by hardregs, not by memory, so we need a register for this
   reload, just because we are presented with non-matching operands.
   So, even though we allow memory for this operand, no memory can be
   used for it, just because the two operands don't match.  This can
   cause reload failures on register-starved targets.

   So it's a symptom of reload not being able to use memory for reloads
   or, alternatively it's also a symptom of both operands not coming into
   reload as matching (in which case the pseudo could go to memory just
   fine, as the alternative allows it, and no reload would be necessary).
   We fix the latter problem here, by transforming

     asm ("": "=mr" (inout_2) : "0" (inout_1));

   back to

     inout_2 = inout_1;
     asm ("": "=mr" (inout_2) : "0" (inout_2));  */

static void
match_asm_constraints_1 (rtx insn, rtx *p_sets, int noutputs)
{
  int i;
  bool changed = false;
  rtx op = SET_SRC (p_sets[0]);
  int ninputs = ASM_OPERANDS_INPUT_LENGTH (op);
  rtvec inputs = ASM_OPERANDS_INPUT_VEC (op);
  bool *output_matched = XALLOCAVEC (bool, noutputs);

  memset (output_matched, 0, noutputs * sizeof (bool));
  for (i = 0; i < ninputs; i++)
    {
      rtx input, output, insns;
      const char *constraint = ASM_OPERANDS_INPUT_CONSTRAINT (op, i);
      char *end;
      int match, j;

      if (*constraint == '%')
	constraint++;

      match = strtoul (constraint, &end, 10);
      if (end == constraint)
	continue;

      gcc_assert (match < noutputs);
      output = SET_DEST (p_sets[match]);
      input = RTVEC_ELT (inputs, i);
      /* Only do the transformation for pseudos.  */
      if (! REG_P (output)
	  || rtx_equal_p (output, input)
	  || (GET_MODE (input) != VOIDmode
	      && GET_MODE (input) != GET_MODE (output)))
	continue;

      /* We can't do anything if the output is also used as input,
	 as we're going to overwrite it.  */
      for (j = 0; j < ninputs; j++)
        if (reg_overlap_mentioned_p (output, RTVEC_ELT (inputs, j)))
	  break;
      if (j != ninputs)
	continue;

      /* Avoid changing the same input several times.  For
	 asm ("" : "=mr" (out1), "=mr" (out2) : "0" (in), "1" (in));
	 only change in once (to out1), rather than changing it
	 first to out1 and afterwards to out2.  */
      if (i > 0)
	{
	  for (j = 0; j < noutputs; j++)
	    if (output_matched[j] && input == SET_DEST (p_sets[j]))
	      break;
	  if (j != noutputs)
	    continue;
	}
      output_matched[match] = true;

      start_sequence ();
      emit_move_insn (output, input);
      insns = get_insns ();
      end_sequence ();
      emit_insn_before (insns, insn);

      /* Now replace all mentions of the input with output.  We can't
	 just replace the occurrence in inputs[i], as the register might
	 also be used in some other input (or even in an address of an
	 output), which would mean possibly increasing the number of
	 inputs by one (namely 'output' in addition), which might pose
	 a too complicated problem for reload to solve.  E.g. this situation:

	   asm ("" : "=r" (output), "=m" (input) : "0" (input))

	 Here 'input' is used in two occurrences as input (once for the
	 input operand, once for the address in the second output operand).
	 If we would replace only the occurrence of the input operand (to
	 make the matching) we would be left with this:

	   output = input
	   asm ("" : "=r" (output), "=m" (input) : "0" (output))

	 Now we suddenly have two different input values (containing the same
	 value, but different pseudos) where we formerly had only one.
	 With more complicated asms this might lead to reload failures
	 which wouldn't have happen without this pass.  So, iterate over
	 all operands and replace all occurrences of the register used.  */
      for (j = 0; j < noutputs; j++)
	if (!rtx_equal_p (SET_DEST (p_sets[j]), input)
	    && reg_overlap_mentioned_p (input, SET_DEST (p_sets[j])))
	  SET_DEST (p_sets[j]) = replace_rtx (SET_DEST (p_sets[j]),
					      input, output);
      for (j = 0; j < ninputs; j++)
	if (reg_overlap_mentioned_p (input, RTVEC_ELT (inputs, j)))
	  RTVEC_ELT (inputs, j) = replace_rtx (RTVEC_ELT (inputs, j),
					       input, output);

      changed = true;
    }

  if (changed)
    df_insn_rescan (insn);
}

static unsigned
rest_of_match_asm_constraints (void)
{
  basic_block bb;
  rtx insn, pat, *p_sets;
  int noutputs;

  if (!crtl->has_asm_statement)
    return 0;

  df_set_flags (DF_DEFER_INSN_RESCAN);
  FOR_EACH_BB (bb)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  if (!INSN_P (insn))
	    continue;

	  pat = PATTERN (insn);
	  if (GET_CODE (pat) == PARALLEL)
	    p_sets = &XVECEXP (pat, 0, 0), noutputs = XVECLEN (pat, 0);
	  else if (GET_CODE (pat) == SET)
	    p_sets = &PATTERN (insn), noutputs = 1;
	  else
	    continue;

	  if (GET_CODE (*p_sets) == SET
	      && GET_CODE (SET_SRC (*p_sets)) == ASM_OPERANDS)
	    match_asm_constraints_1 (insn, p_sets, noutputs);
	 }
    }

  return TODO_df_finish;
}

struct rtl_opt_pass pass_match_asm_constraints =
{
 {
  RTL_PASS,
  "asmcons",				/* name */
  NULL,					/* gate */
  rest_of_match_asm_constraints,	/* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,				/* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func                       /* todo_flags_finish */
 }
};


#include "gt-function.h"
