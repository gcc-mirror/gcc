/* Save and restore call-clobbered registers which are live across a call.
   Copyright (C) 1989-2017 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "reload.h"
#include "alias.h"
#include "addresses.h"
#include "dumpfile.h"
#include "rtl-iter.h"
#include "target.h"

#define MOVE_MAX_WORDS (MOVE_MAX / UNITS_PER_WORD)

#define regno_save_mode \
  (this_target_reload->x_regno_save_mode)
#define cached_reg_save_code \
  (this_target_reload->x_cached_reg_save_code)
#define cached_reg_restore_code \
  (this_target_reload->x_cached_reg_restore_code)

/* For each hard register, a place on the stack where it can be saved,
   if needed.  */

static rtx
  regno_save_mem[FIRST_PSEUDO_REGISTER][MAX_MOVE_MAX / MIN_UNITS_PER_WORD + 1];

/* The number of elements in the subsequent array.  */
static int save_slots_num;

/* Allocated slots so far.  */
static rtx save_slots[FIRST_PSEUDO_REGISTER];

/* Set of hard regs currently residing in save area (during insn scan).  */

static HARD_REG_SET hard_regs_saved;

/* Number of registers currently in hard_regs_saved.  */

static int n_regs_saved;

/* Computed by mark_referenced_regs, all regs referenced in a given
   insn.  */
static HARD_REG_SET referenced_regs;


typedef void refmarker_fn (rtx *loc, machine_mode mode, int hardregno,
			   void *mark_arg);

static int reg_save_code (int, machine_mode);
static int reg_restore_code (int, machine_mode);

struct saved_hard_reg;
static void initiate_saved_hard_regs (void);
static void new_saved_hard_reg (int, int);
static void finish_saved_hard_regs (void);
static int saved_hard_reg_compare_func (const void *, const void *);

static void mark_set_regs (rtx, const_rtx, void *);
static void mark_referenced_regs (rtx *, refmarker_fn *mark, void *mark_arg);
static refmarker_fn mark_reg_as_referenced;
static refmarker_fn replace_reg_with_saved_mem;
static int insert_save (struct insn_chain *, int, int, HARD_REG_SET *,
			machine_mode *);
static int insert_restore (struct insn_chain *, int, int, int,
			   machine_mode *);
static struct insn_chain *insert_one_insn (struct insn_chain *, int, int,
					   rtx);
static void add_stored_regs (rtx, const_rtx, void *);



static GTY(()) rtx savepat;
static GTY(()) rtx restpat;
static GTY(()) rtx test_reg;
static GTY(()) rtx test_mem;
static GTY(()) rtx_insn *saveinsn;
static GTY(()) rtx_insn *restinsn;

/* Return the INSN_CODE used to save register REG in mode MODE.  */
static int
reg_save_code (int reg, machine_mode mode)
{
  bool ok;
  if (cached_reg_save_code[reg][mode])
     return cached_reg_save_code[reg][mode];
  if (!targetm.hard_regno_mode_ok (reg, mode))
    {
      /* Depending on how targetm.hard_regno_mode_ok is defined, range
	 propagation might deduce here that reg >= FIRST_PSEUDO_REGISTER.
	 So the assert below silences a warning.  */
      gcc_assert (reg < FIRST_PSEUDO_REGISTER);
      cached_reg_save_code[reg][mode] = -1;
      cached_reg_restore_code[reg][mode] = -1;
      return -1;
    }

  /* Update the register number and modes of the register
     and memory operand.  */
  set_mode_and_regno (test_reg, mode, reg);
  PUT_MODE (test_mem, mode);

  /* Force re-recognition of the modified insns.  */
  INSN_CODE (saveinsn) = -1;
  INSN_CODE (restinsn) = -1;

  cached_reg_save_code[reg][mode] = recog_memoized (saveinsn);
  cached_reg_restore_code[reg][mode] = recog_memoized (restinsn);

  /* Now extract both insns and see if we can meet their
     constraints.  We don't know here whether the save and restore will
     be in size- or speed-tuned code, so just use the set of enabled
     alternatives.  */
  ok = (cached_reg_save_code[reg][mode] != -1
	&& cached_reg_restore_code[reg][mode] != -1);
  if (ok)
    {
      extract_insn (saveinsn);
      ok = constrain_operands (1, get_enabled_alternatives (saveinsn));
      extract_insn (restinsn);
      ok &= constrain_operands (1, get_enabled_alternatives (restinsn));
    }

  if (! ok)
    {
      cached_reg_save_code[reg][mode] = -1;
      cached_reg_restore_code[reg][mode] = -1;
    }
  gcc_assert (cached_reg_save_code[reg][mode]);
  return cached_reg_save_code[reg][mode];
}

/* Return the INSN_CODE used to restore register REG in mode MODE.  */
static int
reg_restore_code (int reg, machine_mode mode)
{
  if (cached_reg_restore_code[reg][mode])
     return cached_reg_restore_code[reg][mode];
  /* Populate our cache.  */
  reg_save_code (reg, mode);
  return cached_reg_restore_code[reg][mode];
}

/* Initialize for caller-save.

   Look at all the hard registers that are used by a call and for which
   reginfo.c has not already excluded from being used across a call.

   Ensure that we can find a mode to save the register and that there is a
   simple insn to save and restore the register.  This latter check avoids
   problems that would occur if we tried to save the MQ register of some
   machines directly into memory.  */

void
init_caller_save (void)
{
  rtx addr_reg;
  int offset;
  rtx address;
  int i, j;

  if (caller_save_initialized_p)
    return;

  caller_save_initialized_p = true;

  CLEAR_HARD_REG_SET (no_caller_save_reg_set);
  /* First find all the registers that we need to deal with and all
     the modes that they can have.  If we can't find a mode to use,
     we can't have the register live over calls.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (call_used_regs[i]
          && !TEST_HARD_REG_BIT (call_fixed_reg_set, i))
	{
	  for (j = 1; j <= MOVE_MAX_WORDS; j++)
	    {
	      regno_save_mode[i][j] = HARD_REGNO_CALLER_SAVE_MODE (i, j,
								   VOIDmode);
	      if (regno_save_mode[i][j] == VOIDmode && j == 1)
		{
		  SET_HARD_REG_BIT (call_fixed_reg_set, i);
		}
	    }
	}
      else
	regno_save_mode[i][1] = VOIDmode;
    }

  /* The following code tries to approximate the conditions under which
     we can easily save and restore a register without scratch registers or
     other complexities.  It will usually work, except under conditions where
     the validity of an insn operand is dependent on the address offset.
     No such cases are currently known.

     We first find a typical offset from some BASE_REG_CLASS register.
     This address is chosen by finding the first register in the class
     and by finding the smallest power of two that is a valid offset from
     that register in every mode we will use to save registers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT
	(reg_class_contents
	 [(int) base_reg_class (regno_save_mode[i][1], ADDR_SPACE_GENERIC,
				PLUS, CONST_INT)], i))
      break;

  gcc_assert (i < FIRST_PSEUDO_REGISTER);

  addr_reg = gen_rtx_REG (Pmode, i);

  for (offset = 1 << (HOST_BITS_PER_INT / 2); offset; offset >>= 1)
    {
      address = gen_rtx_PLUS (Pmode, addr_reg, gen_int_mode (offset, Pmode));

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (regno_save_mode[i][1] != VOIDmode
	  && ! strict_memory_address_p (regno_save_mode[i][1], address))
	  break;

      if (i == FIRST_PSEUDO_REGISTER)
	break;
    }

  /* If we didn't find a valid address, we must use register indirect.  */
  if (offset == 0)
    address = addr_reg;

  /* Next we try to form an insn to save and restore the register.  We
     see if such an insn is recognized and meets its constraints.

     To avoid lots of unnecessary RTL allocation, we construct all the RTL
     once, then modify the memory and register operands in-place.  */

  test_reg = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);
  test_mem = gen_rtx_MEM (word_mode, address);
  savepat = gen_rtx_SET (test_mem, test_reg);
  restpat = gen_rtx_SET (test_reg, test_mem);

  saveinsn = gen_rtx_INSN (VOIDmode, 0, 0, 0, savepat, 0, -1, 0);
  restinsn = gen_rtx_INSN (VOIDmode, 0, 0, 0, restpat, 0, -1, 0);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    for (j = 1; j <= MOVE_MAX_WORDS; j++)
      if (reg_save_code (i,regno_save_mode[i][j]) == -1)
	{
	  regno_save_mode[i][j] = VOIDmode;
	  if (j == 1)
	    {
	      SET_HARD_REG_BIT (call_fixed_reg_set, i);
	      if (call_used_regs[i])
		SET_HARD_REG_BIT (no_caller_save_reg_set, i);
	    }
	}
}



/* Initialize save areas by showing that we haven't allocated any yet.  */

void
init_save_areas (void)
{
  int i, j;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    for (j = 1; j <= MOVE_MAX_WORDS; j++)
      regno_save_mem[i][j] = 0;
  save_slots_num = 0;

}

/* The structure represents a hard register which should be saved
   through the call.  It is used when the integrated register
   allocator (IRA) is used and sharing save slots is on.  */
struct saved_hard_reg
{
  /* Order number starting with 0.  */
  int num;
  /* The hard regno.  */
  int hard_regno;
  /* Execution frequency of all calls through which given hard
     register should be saved.  */
  int call_freq;
  /* Stack slot reserved to save the hard register through calls.  */
  rtx slot;
  /* True if it is first hard register in the chain of hard registers
     sharing the same stack slot.  */
  int first_p;
  /* Order number of the next hard register structure with the same
     slot in the chain.  -1 represents end of the chain.  */
  int next;
};

/* Map: hard register number to the corresponding structure.  */
static struct saved_hard_reg *hard_reg_map[FIRST_PSEUDO_REGISTER];

/* The number of all structures representing hard registers should be
   saved, in order words, the number of used elements in the following
   array.  */
static int saved_regs_num;

/* Pointers to all the structures.  Index is the order number of the
   corresponding structure.  */
static struct saved_hard_reg *all_saved_regs[FIRST_PSEUDO_REGISTER];

/* First called function for work with saved hard registers.  */
static void
initiate_saved_hard_regs (void)
{
  int i;

  saved_regs_num = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    hard_reg_map[i] = NULL;
}

/* Allocate and return new saved hard register with given REGNO and
   CALL_FREQ.  */
static void
new_saved_hard_reg (int regno, int call_freq)
{
  struct saved_hard_reg *saved_reg;

  saved_reg
    = (struct saved_hard_reg *) xmalloc (sizeof (struct saved_hard_reg));
  hard_reg_map[regno] = all_saved_regs[saved_regs_num] = saved_reg;
  saved_reg->num = saved_regs_num++;
  saved_reg->hard_regno = regno;
  saved_reg->call_freq = call_freq;
  saved_reg->first_p = FALSE;
  saved_reg->next = -1;
}

/* Free memory allocated for the saved hard registers.  */
static void
finish_saved_hard_regs (void)
{
  int i;

  for (i = 0; i < saved_regs_num; i++)
    free (all_saved_regs[i]);
}

/* The function is used to sort the saved hard register structures
   according their frequency.  */
static int
saved_hard_reg_compare_func (const void *v1p, const void *v2p)
{
  const struct saved_hard_reg *p1 = *(struct saved_hard_reg * const *) v1p;
  const struct saved_hard_reg *p2 = *(struct saved_hard_reg * const *) v2p;

  if (flag_omit_frame_pointer)
    {
      if (p1->call_freq - p2->call_freq != 0)
	return p1->call_freq - p2->call_freq;
    }
  else if (p2->call_freq - p1->call_freq != 0)
    return p2->call_freq - p1->call_freq;

  return p1->num - p2->num;
}

/* Allocate save areas for any hard registers that might need saving.
   We take a conservative approach here and look for call-clobbered hard
   registers that are assigned to pseudos that cross calls.  This may
   overestimate slightly (especially if some of these registers are later
   used as spill registers), but it should not be significant.

   For IRA we use priority coloring to decrease stack slots needed for
   saving hard registers through calls.  We build conflicts for them
   to do coloring.

   Future work:

     In the fallback case we should iterate backwards across all possible
     modes for the save, choosing the largest available one instead of
     falling back to the smallest mode immediately.  (eg TF -> DF -> SF).

     We do not try to use "move multiple" instructions that exist
     on some machines (such as the 68k moveml).  It could be a win to try
     and use them when possible.  The hard part is doing it in a way that is
     machine independent since they might be saving non-consecutive
     registers. (imagine caller-saving d0,d1,a0,a1 on the 68k) */

void
setup_save_areas (void)
{
  int i, j, k, freq;
  HARD_REG_SET hard_regs_used;
  struct saved_hard_reg *saved_reg;
  rtx_insn *insn;
  struct insn_chain *chain, *next;
  unsigned int regno;
  HARD_REG_SET hard_regs_to_save, used_regs, this_insn_sets;
  reg_set_iterator rsi;

  CLEAR_HARD_REG_SET (hard_regs_used);

  /* Find every CALL_INSN and record which hard regs are live across the
     call into HARD_REG_MAP and HARD_REGS_USED.  */
  initiate_saved_hard_regs ();
  /* Create hard reg saved regs.  */
  for (chain = reload_insn_chain; chain != 0; chain = next)
    {
      rtx cheap;

      insn = chain->insn;
      next = chain->next;
      if (!CALL_P (insn)
	  || find_reg_note (insn, REG_NORETURN, NULL))
	continue;
      freq = REG_FREQ_FROM_BB (BLOCK_FOR_INSN (insn));
      REG_SET_TO_HARD_REG_SET (hard_regs_to_save,
			       &chain->live_throughout);
      get_call_reg_set_usage (insn, &used_regs, call_used_reg_set);

      /* Record all registers set in this call insn.  These don't
	 need to be saved.  N.B. the call insn might set a subreg
	 of a multi-hard-reg pseudo; then the pseudo is considered
	 live during the call, but the subreg that is set
	 isn't.  */
      CLEAR_HARD_REG_SET (this_insn_sets);
      note_stores (PATTERN (insn), mark_set_regs, &this_insn_sets);
      /* Sibcalls are considered to set the return value.  */
      if (SIBLING_CALL_P (insn) && crtl->return_rtx)
	mark_set_regs (crtl->return_rtx, NULL_RTX, &this_insn_sets);

      AND_COMPL_HARD_REG_SET (used_regs, call_fixed_reg_set);
      AND_COMPL_HARD_REG_SET (used_regs, this_insn_sets);
      AND_HARD_REG_SET (hard_regs_to_save, used_regs);
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (TEST_HARD_REG_BIT (hard_regs_to_save, regno))
	  {
	    if (hard_reg_map[regno] != NULL)
	      hard_reg_map[regno]->call_freq += freq;
	    else
	      new_saved_hard_reg (regno, freq);
	    SET_HARD_REG_BIT (hard_regs_used, regno);
	  }
      cheap = find_reg_note (insn, REG_RETURNED, NULL);
      if (cheap)
	cheap = XEXP (cheap, 0);
      /* Look through all live pseudos, mark their hard registers.  */
      EXECUTE_IF_SET_IN_REG_SET
	(&chain->live_throughout, FIRST_PSEUDO_REGISTER, regno, rsi)
	{
	  int r = reg_renumber[regno];
	  int bound;

	  if (r < 0 || regno_reg_rtx[regno] == cheap)
	    continue;

	  bound = r + hard_regno_nregs (r, PSEUDO_REGNO_MODE (regno));
	  for (; r < bound; r++)
	    if (TEST_HARD_REG_BIT (used_regs, r))
	      {
		if (hard_reg_map[r] != NULL)
		  hard_reg_map[r]->call_freq += freq;
		else
		  new_saved_hard_reg (r, freq);
		 SET_HARD_REG_BIT (hard_regs_to_save, r);
		 SET_HARD_REG_BIT (hard_regs_used, r);
	      }
	}
    }

  /* If requested, figure out which hard regs can share save slots.  */
  if (optimize && flag_ira_share_save_slots)
    {
      rtx slot;
      char *saved_reg_conflicts;
      int next_k;
      struct saved_hard_reg *saved_reg2, *saved_reg3;
      int call_saved_regs_num;
      struct saved_hard_reg *call_saved_regs[FIRST_PSEUDO_REGISTER];
      int best_slot_num;
      int prev_save_slots_num;
      rtx prev_save_slots[FIRST_PSEUDO_REGISTER];

      /* Find saved hard register conflicts.  */
      saved_reg_conflicts = (char *) xmalloc (saved_regs_num * saved_regs_num);
      memset (saved_reg_conflicts, 0, saved_regs_num * saved_regs_num);
      for (chain = reload_insn_chain; chain != 0; chain = next)
	{
	  rtx cheap;
	  call_saved_regs_num = 0;
	  insn = chain->insn;
	  next = chain->next;
	  if (!CALL_P (insn)
	      || find_reg_note (insn, REG_NORETURN, NULL))
	    continue;

	  cheap = find_reg_note (insn, REG_RETURNED, NULL);
	  if (cheap)
	    cheap = XEXP (cheap, 0);

	  REG_SET_TO_HARD_REG_SET (hard_regs_to_save,
				   &chain->live_throughout);
	  get_call_reg_set_usage (insn, &used_regs, call_used_reg_set);

	  /* Record all registers set in this call insn.  These don't
	     need to be saved.  N.B. the call insn might set a subreg
	     of a multi-hard-reg pseudo; then the pseudo is considered
	     live during the call, but the subreg that is set
	     isn't.  */
	  CLEAR_HARD_REG_SET (this_insn_sets);
	  note_stores (PATTERN (insn), mark_set_regs, &this_insn_sets);
	  /* Sibcalls are considered to set the return value,
	     compare df-scan.c:df_get_call_refs.  */
	  if (SIBLING_CALL_P (insn) && crtl->return_rtx)
	    mark_set_regs (crtl->return_rtx, NULL_RTX, &this_insn_sets);

	  AND_COMPL_HARD_REG_SET (used_regs, call_fixed_reg_set);
	  AND_COMPL_HARD_REG_SET (used_regs, this_insn_sets);
	  AND_HARD_REG_SET (hard_regs_to_save, used_regs);
	  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	    if (TEST_HARD_REG_BIT (hard_regs_to_save, regno))
	      {
		gcc_assert (hard_reg_map[regno] != NULL);
		call_saved_regs[call_saved_regs_num++] = hard_reg_map[regno];
	      }
	  /* Look through all live pseudos, mark their hard registers.  */
	  EXECUTE_IF_SET_IN_REG_SET
	    (&chain->live_throughout, FIRST_PSEUDO_REGISTER, regno, rsi)
	    {
	      int r = reg_renumber[regno];
	      int bound;

	      if (r < 0 || regno_reg_rtx[regno] == cheap)
		continue;

	      bound = r + hard_regno_nregs (r, PSEUDO_REGNO_MODE (regno));
	      for (; r < bound; r++)
		if (TEST_HARD_REG_BIT (used_regs, r))
		  call_saved_regs[call_saved_regs_num++] = hard_reg_map[r];
	    }
	  for (i = 0; i < call_saved_regs_num; i++)
	    {
	      saved_reg = call_saved_regs[i];
	      for (j = 0; j < call_saved_regs_num; j++)
		if (i != j)
		  {
		    saved_reg2 = call_saved_regs[j];
		    saved_reg_conflicts[saved_reg->num * saved_regs_num
					+ saved_reg2->num]
		      = saved_reg_conflicts[saved_reg2->num * saved_regs_num
					    + saved_reg->num]
		      = TRUE;
		  }
	    }
	}
      /* Sort saved hard regs.  */
      qsort (all_saved_regs, saved_regs_num, sizeof (struct saved_hard_reg *),
	     saved_hard_reg_compare_func);
      /* Initiate slots available from the previous reload
	 iteration.  */
      prev_save_slots_num = save_slots_num;
      memcpy (prev_save_slots, save_slots, save_slots_num * sizeof (rtx));
      save_slots_num = 0;
      /* Allocate stack slots for the saved hard registers.  */
      for (i = 0; i < saved_regs_num; i++)
	{
	  saved_reg = all_saved_regs[i];
	  regno = saved_reg->hard_regno;
	  for (j = 0; j < i; j++)
	    {
	      saved_reg2 = all_saved_regs[j];
	      if (! saved_reg2->first_p)
		continue;
	      slot = saved_reg2->slot;
	      for (k = j; k >= 0; k = next_k)
		{
		  saved_reg3 = all_saved_regs[k];
		  next_k = saved_reg3->next;
		  if (saved_reg_conflicts[saved_reg->num * saved_regs_num
					  + saved_reg3->num])
		    break;
		}
	      if (k < 0
		  && (GET_MODE_SIZE (regno_save_mode[regno][1])
		      <= GET_MODE_SIZE (regno_save_mode
					[saved_reg2->hard_regno][1])))
		{
		  saved_reg->slot
		    = adjust_address_nv
		      (slot, regno_save_mode[saved_reg->hard_regno][1], 0);
		  regno_save_mem[regno][1] = saved_reg->slot;
		  saved_reg->next = saved_reg2->next;
		  saved_reg2->next = i;
		  if (dump_file != NULL)
		    fprintf (dump_file, "%d uses slot of %d\n",
			     regno, saved_reg2->hard_regno);
		  break;
		}
	    }
	  if (j == i)
	    {
	      saved_reg->first_p = TRUE;
	      for (best_slot_num = -1, j = 0; j < prev_save_slots_num; j++)
		{
		  slot = prev_save_slots[j];
		  if (slot == NULL_RTX)
		    continue;
		  if (GET_MODE_SIZE (regno_save_mode[regno][1])
		      <= GET_MODE_SIZE (GET_MODE (slot))
		      && best_slot_num < 0)
		    best_slot_num = j;
		  if (GET_MODE (slot) == regno_save_mode[regno][1])
		    break;
		}
	      if (best_slot_num >= 0)
		{
		  saved_reg->slot = prev_save_slots[best_slot_num];
		  saved_reg->slot
		    = adjust_address_nv
		      (saved_reg->slot,
		       regno_save_mode[saved_reg->hard_regno][1], 0);
		  if (dump_file != NULL)
		    fprintf (dump_file,
			     "%d uses a slot from prev iteration\n", regno);
		  prev_save_slots[best_slot_num] = NULL_RTX;
		  if (best_slot_num + 1 == prev_save_slots_num)
		    prev_save_slots_num--;
		}
	      else
		{
		  saved_reg->slot
		    = assign_stack_local_1
		      (regno_save_mode[regno][1],
		       GET_MODE_SIZE (regno_save_mode[regno][1]), 0,
		       ASLK_REDUCE_ALIGN);
		  if (dump_file != NULL)
		    fprintf (dump_file, "%d uses a new slot\n", regno);
		}
	      regno_save_mem[regno][1] = saved_reg->slot;
	      save_slots[save_slots_num++] = saved_reg->slot;
	    }
	}
      free (saved_reg_conflicts);
      finish_saved_hard_regs ();
    }
  else
    {
      /* We are not sharing slots. 

	 Run through all the call-used hard-registers and allocate
	 space for each in the caller-save area.  Try to allocate space
	 in a manner which allows multi-register saves/restores to be done.  */

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	for (j = MOVE_MAX_WORDS; j > 0; j--)
	  {
	    int do_save = 1;

	    /* If no mode exists for this size, try another.  Also break out
	       if we have already saved this hard register.  */
	    if (regno_save_mode[i][j] == VOIDmode || regno_save_mem[i][1] != 0)
	      continue;

	    /* See if any register in this group has been saved.  */
	    for (k = 0; k < j; k++)
	      if (regno_save_mem[i + k][1])
		{
		  do_save = 0;
		  break;
		}
	    if (! do_save)
	      continue;

	    for (k = 0; k < j; k++)
	      if (! TEST_HARD_REG_BIT (hard_regs_used, i + k))
		{
		  do_save = 0;
		  break;
		}
	    if (! do_save)
	      continue;

	    /* We have found an acceptable mode to store in.  Since
	       hard register is always saved in the widest mode
	       available, the mode may be wider than necessary, it is
	       OK to reduce the alignment of spill space.  We will
	       verify that it is equal to or greater than required
	       when we restore and save the hard register in
	       insert_restore and insert_save.  */
	    regno_save_mem[i][j]
	      = assign_stack_local_1 (regno_save_mode[i][j],
				      GET_MODE_SIZE (regno_save_mode[i][j]),
				      0, ASLK_REDUCE_ALIGN);

	    /* Setup single word save area just in case...  */
	    for (k = 0; k < j; k++)
	      /* This should not depend on WORDS_BIG_ENDIAN.
		 The order of words in regs is the same as in memory.  */
	      regno_save_mem[i + k][1]
		= adjust_address_nv (regno_save_mem[i][j],
				     regno_save_mode[i + k][1],
				     k * UNITS_PER_WORD);
	  }
    }

  /* Now loop again and set the alias set of any save areas we made to
     the alias set used to represent frame objects.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    for (j = MOVE_MAX_WORDS; j > 0; j--)
      if (regno_save_mem[i][j] != 0)
	set_mem_alias_set (regno_save_mem[i][j], get_frame_alias_set ());
}



/* Find the places where hard regs are live across calls and save them.  */

void
save_call_clobbered_regs (void)
{
  struct insn_chain *chain, *next, *last = NULL;
  machine_mode save_mode [FIRST_PSEUDO_REGISTER];

  /* Computed in mark_set_regs, holds all registers set by the current
     instruction.  */
  HARD_REG_SET this_insn_sets;

  CLEAR_HARD_REG_SET (hard_regs_saved);
  n_regs_saved = 0;

  for (chain = reload_insn_chain; chain != 0; chain = next)
    {
      rtx_insn *insn = chain->insn;
      enum rtx_code code = GET_CODE (insn);

      next = chain->next;

      gcc_assert (!chain->is_caller_save_insn);

      if (NONDEBUG_INSN_P (insn))
	{
	  /* If some registers have been saved, see if INSN references
	     any of them.  We must restore them before the insn if so.  */

	  if (n_regs_saved)
	    {
	      int regno;
	      HARD_REG_SET this_insn_sets;

	      if (code == JUMP_INSN)
		/* Restore all registers if this is a JUMP_INSN.  */
		COPY_HARD_REG_SET (referenced_regs, hard_regs_saved);
	      else
		{
		  CLEAR_HARD_REG_SET (referenced_regs);
		  mark_referenced_regs (&PATTERN (insn),
					mark_reg_as_referenced, NULL);
		  AND_HARD_REG_SET (referenced_regs, hard_regs_saved);
		}

	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (referenced_regs, regno))
		  regno += insert_restore (chain, 1, regno, MOVE_MAX_WORDS,
					   save_mode);
	      /* If a saved register is set after the call, this means we no
		 longer should restore it.  This can happen when parts of a
		 multi-word pseudo do not conflict with other pseudos, so
		 IRA may allocate the same hard register for both.  One may
		 be live across the call, while the other is set
		 afterwards.  */
	      CLEAR_HARD_REG_SET (this_insn_sets);
	      note_stores (PATTERN (insn), mark_set_regs, &this_insn_sets);
	      AND_COMPL_HARD_REG_SET (hard_regs_saved, this_insn_sets);
	    }

	  if (code == CALL_INSN
	      && ! SIBLING_CALL_P (insn)
	      && ! find_reg_note (insn, REG_NORETURN, NULL))
	    {
	      unsigned regno;
	      HARD_REG_SET hard_regs_to_save;
	      HARD_REG_SET call_def_reg_set;
	      reg_set_iterator rsi;
	      rtx cheap;

	      cheap = find_reg_note (insn, REG_RETURNED, NULL);
	      if (cheap)
		cheap = XEXP (cheap, 0);

	      /* Use the register life information in CHAIN to compute which
		 regs are live during the call.  */
	      REG_SET_TO_HARD_REG_SET (hard_regs_to_save,
				       &chain->live_throughout);
	      /* Save hard registers always in the widest mode available.  */
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (hard_regs_to_save, regno))
		  save_mode [regno] = regno_save_mode [regno][1];
		else
		  save_mode [regno] = VOIDmode;

	      /* Look through all live pseudos, mark their hard registers
		 and choose proper mode for saving.  */
	      EXECUTE_IF_SET_IN_REG_SET
		(&chain->live_throughout, FIRST_PSEUDO_REGISTER, regno, rsi)
		{
		  int r = reg_renumber[regno];
		  int nregs;
		  machine_mode mode;

		  if (r < 0 || regno_reg_rtx[regno] == cheap)
		    continue;
		  nregs = hard_regno_nregs (r, PSEUDO_REGNO_MODE (regno));
		  mode = HARD_REGNO_CALLER_SAVE_MODE
		    (r, nregs, PSEUDO_REGNO_MODE (regno));
		  if (partial_subreg_p (save_mode[r], mode))
		    save_mode[r] = mode;
		  while (nregs-- > 0)
		    SET_HARD_REG_BIT (hard_regs_to_save, r + nregs);
		}

	      /* Record all registers set in this call insn.  These don't need
		 to be saved.  N.B. the call insn might set a subreg of a
		 multi-hard-reg pseudo; then the pseudo is considered live
		 during the call, but the subreg that is set isn't.  */
	      CLEAR_HARD_REG_SET (this_insn_sets);
	      note_stores (PATTERN (insn), mark_set_regs, &this_insn_sets);

	      /* Compute which hard regs must be saved before this call.  */
	      AND_COMPL_HARD_REG_SET (hard_regs_to_save, call_fixed_reg_set);
	      AND_COMPL_HARD_REG_SET (hard_regs_to_save, this_insn_sets);
	      AND_COMPL_HARD_REG_SET (hard_regs_to_save, hard_regs_saved);
	      get_call_reg_set_usage (insn, &call_def_reg_set,
				      call_used_reg_set);
	      AND_HARD_REG_SET (hard_regs_to_save, call_def_reg_set);

	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (hard_regs_to_save, regno))
		  regno += insert_save (chain, 1, regno, &hard_regs_to_save, save_mode);

	      /* Must recompute n_regs_saved.  */
	      n_regs_saved = 0;
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (hard_regs_saved, regno))
		  n_regs_saved++;
	      
	      if (cheap
		  && HARD_REGISTER_P (cheap)
		  && TEST_HARD_REG_BIT (call_used_reg_set, REGNO (cheap)))
		{
		  rtx dest, newpat;
		  rtx pat = PATTERN (insn);
		  if (GET_CODE (pat) == PARALLEL)
		    pat = XVECEXP (pat, 0, 0);
		  dest = SET_DEST (pat);
		  /* For multiple return values dest is PARALLEL.
		     Currently we handle only single return value case.  */
		  if (REG_P (dest))
		    {
		      newpat = gen_rtx_SET (cheap, copy_rtx (dest));
		      chain = insert_one_insn (chain, 0, -1, newpat);
		    }
		}
	    }
          last = chain;
	}
      else if (DEBUG_INSN_P (insn) && n_regs_saved)
	mark_referenced_regs (&PATTERN (insn),
			      replace_reg_with_saved_mem,
			      save_mode);

      if (chain->next == 0 || chain->next->block != chain->block)
	{
	  int regno;
	  /* At the end of the basic block, we must restore any registers that
	     remain saved.  If the last insn in the block is a JUMP_INSN, put
	     the restore before the insn, otherwise, put it after the insn.  */

	  if (n_regs_saved
	      && DEBUG_INSN_P (insn)
	      && last
	      && last->block == chain->block)
	    {
	      rtx_insn *ins, *prev;
	      basic_block bb = BLOCK_FOR_INSN (insn);

	      /* When adding hard reg restores after a DEBUG_INSN, move
		 all notes between last real insn and this DEBUG_INSN after
		 the DEBUG_INSN, otherwise we could get code
		 -g/-g0 differences.  */
	      for (ins = PREV_INSN (insn); ins != last->insn; ins = prev)
		{
		  prev = PREV_INSN (ins);
		  if (NOTE_P (ins))
		    {
		      SET_NEXT_INSN (prev) = NEXT_INSN (ins);
		      SET_PREV_INSN (NEXT_INSN (ins)) = prev;
		      SET_PREV_INSN (ins) = insn;
		      SET_NEXT_INSN (ins) = NEXT_INSN (insn);
		      SET_NEXT_INSN (insn) = ins;
		      if (NEXT_INSN (ins))
			SET_PREV_INSN (NEXT_INSN (ins)) = ins;
                      if (BB_END (bb) == insn)
			BB_END (bb) = ins;
		    }
		  else
		    gcc_assert (DEBUG_INSN_P (ins));
		}
	    }
	  last = NULL;

	  if (n_regs_saved)
	    for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	      if (TEST_HARD_REG_BIT (hard_regs_saved, regno))
		regno += insert_restore (chain, JUMP_P (insn),
					 regno, MOVE_MAX_WORDS, save_mode);
	}
    }
}

/* Here from note_stores, or directly from save_call_clobbered_regs, when
   an insn stores a value in a register.
   Set the proper bit or bits in this_insn_sets.  All pseudos that have
   been assigned hard regs have had their register number changed already,
   so we can ignore pseudos.  */
static void
mark_set_regs (rtx reg, const_rtx setter ATTRIBUTE_UNUSED, void *data)
{
  int regno, endregno, i;
  HARD_REG_SET *this_insn_sets = (HARD_REG_SET *) data;

  if (GET_CODE (reg) == SUBREG)
    {
      rtx inner = SUBREG_REG (reg);
      if (!REG_P (inner) || REGNO (inner) >= FIRST_PSEUDO_REGISTER)
	return;
      regno = subreg_regno (reg);
      endregno = regno + subreg_nregs (reg);
    }
  else if (REG_P (reg)
	   && REGNO (reg) < FIRST_PSEUDO_REGISTER)
    {
      regno = REGNO (reg);
      endregno = END_REGNO (reg);
    }
  else
    return;

  for (i = regno; i < endregno; i++)
    SET_HARD_REG_BIT (*this_insn_sets, i);
}

/* Here from note_stores when an insn stores a value in a register.
   Set the proper bit or bits in the passed regset.  All pseudos that have
   been assigned hard regs have had their register number changed already,
   so we can ignore pseudos.  */
static void
add_stored_regs (rtx reg, const_rtx setter, void *data)
{
  int regno, endregno, i;
  machine_mode mode = GET_MODE (reg);
  int offset = 0;

  if (GET_CODE (setter) == CLOBBER)
    return;

  if (GET_CODE (reg) == SUBREG
      && REG_P (SUBREG_REG (reg))
      && REGNO (SUBREG_REG (reg)) < FIRST_PSEUDO_REGISTER)
    {
      offset = subreg_regno_offset (REGNO (SUBREG_REG (reg)),
				    GET_MODE (SUBREG_REG (reg)),
				    SUBREG_BYTE (reg),
				    GET_MODE (reg));
      regno = REGNO (SUBREG_REG (reg)) + offset;
      endregno = regno + subreg_nregs (reg);
    }
  else
    {
      if (!REG_P (reg) || REGNO (reg) >= FIRST_PSEUDO_REGISTER)
	return;

      regno = REGNO (reg) + offset;
      endregno = end_hard_regno (mode, regno);
    }

  for (i = regno; i < endregno; i++)
    SET_REGNO_REG_SET ((regset) data, i);
}

/* Walk X and record all referenced registers in REFERENCED_REGS.  */
static void
mark_referenced_regs (rtx *loc, refmarker_fn *mark, void *arg)
{
  enum rtx_code code = GET_CODE (*loc);
  const char *fmt;
  int i, j;

  if (code == SET)
    mark_referenced_regs (&SET_SRC (*loc), mark, arg);
  if (code == SET || code == CLOBBER)
    {
      loc = &SET_DEST (*loc);
      code = GET_CODE (*loc);
      if ((code == REG && REGNO (*loc) < FIRST_PSEUDO_REGISTER)
	  || code == PC || code == CC0
	  || (code == SUBREG && REG_P (SUBREG_REG (*loc))
	      && REGNO (SUBREG_REG (*loc)) < FIRST_PSEUDO_REGISTER
	      /* If we're setting only part of a multi-word register,
		 we shall mark it as referenced, because the words
		 that are not being set should be restored.  */
	      && ((GET_MODE_SIZE (GET_MODE (*loc))
		   >= GET_MODE_SIZE (GET_MODE (SUBREG_REG (*loc))))
		  || (GET_MODE_SIZE (GET_MODE (SUBREG_REG (*loc)))
		      <= UNITS_PER_WORD))))
	return;
    }
  if (code == MEM || code == SUBREG)
    {
      loc = &XEXP (*loc, 0);
      code = GET_CODE (*loc);
    }

  if (code == REG)
    {
      int regno = REGNO (*loc);
      int hardregno = (regno < FIRST_PSEUDO_REGISTER ? regno
		       : reg_renumber[regno]);

      if (hardregno >= 0)
	mark (loc, GET_MODE (*loc), hardregno, arg);
      else if (arg)
	/* ??? Will we ever end up with an equiv expression in a debug
	   insn, that would have required restoring a reg, or will
	   reload take care of it for us?  */
	return;
      /* If this is a pseudo that did not get a hard register, scan its
	 memory location, since it might involve the use of another
	 register, which might be saved.  */
      else if (reg_equiv_mem (regno) != 0)
	mark_referenced_regs (&XEXP (reg_equiv_mem (regno), 0), mark, arg);
      else if (reg_equiv_address (regno) != 0)
	mark_referenced_regs (&reg_equiv_address (regno), mark, arg);
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_referenced_regs (&XEXP (*loc, i), mark, arg);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (*loc, i) - 1; j >= 0; j--)
	  mark_referenced_regs (&XVECEXP (*loc, i, j), mark, arg);
    }
}

/* Parameter function for mark_referenced_regs() that adds registers
   present in the insn and in equivalent mems and addresses to
   referenced_regs.  */

static void
mark_reg_as_referenced (rtx *loc ATTRIBUTE_UNUSED,
			machine_mode mode,
			int hardregno,
			void *arg ATTRIBUTE_UNUSED)
{
  add_to_hard_reg_set (&referenced_regs, mode, hardregno);
}

/* Parameter function for mark_referenced_regs() that replaces
   registers referenced in a debug_insn that would have been restored,
   should it be a non-debug_insn, with their save locations.  */

static void
replace_reg_with_saved_mem (rtx *loc,
			    machine_mode mode,
			    int regno,
			    void *arg)
{
  unsigned int i, nregs = hard_regno_nregs (regno, mode);
  rtx mem;
  machine_mode *save_mode = (machine_mode *)arg;

  for (i = 0; i < nregs; i++)
    if (TEST_HARD_REG_BIT (hard_regs_saved, regno + i))
      break;

  /* If none of the registers in the range would need restoring, we're
     all set.  */
  if (i == nregs)
    return;

  while (++i < nregs)
    if (!TEST_HARD_REG_BIT (hard_regs_saved, regno + i))
      break;

  if (i == nregs
      && regno_save_mem[regno][nregs])
    {
      mem = copy_rtx (regno_save_mem[regno][nregs]);

      if (nregs == hard_regno_nregs (regno, save_mode[regno]))
	mem = adjust_address_nv (mem, save_mode[regno], 0);

      if (GET_MODE (mem) != mode)
	{
	  /* This is gen_lowpart_if_possible(), but without validating
	     the newly-formed address.  */
	  int offset = 0;

	  if (WORDS_BIG_ENDIAN)
	    offset = (MAX (GET_MODE_SIZE (GET_MODE (mem)), UNITS_PER_WORD)
		      - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD));
	  if (BYTES_BIG_ENDIAN)
	    /* Adjust the address so that the address-after-the-data is
	       unchanged.  */
	    offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode))
		       - MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (mem))));

	  mem = adjust_address_nv (mem, mode, offset);
	}
    }
  else
    {
      mem = gen_rtx_CONCATN (mode, rtvec_alloc (nregs));
      for (i = 0; i < nregs; i++)
	if (TEST_HARD_REG_BIT (hard_regs_saved, regno + i))
	  {
	    gcc_assert (regno_save_mem[regno + i][1]);
	    XVECEXP (mem, 0, i) = copy_rtx (regno_save_mem[regno + i][1]);
	  }
	else
	  {
	    machine_mode smode = save_mode[regno];
	    gcc_assert (smode != VOIDmode);
	    if (hard_regno_nregs (regno, smode) > 1)
	      smode = mode_for_size (GET_MODE_SIZE (mode) / nregs,
				     GET_MODE_CLASS (mode), 0).require ();
	    XVECEXP (mem, 0, i) = gen_rtx_REG (smode, regno + i);
	  }
    }

  gcc_assert (GET_MODE (mem) == mode);
  *loc = mem;
}


/* Insert a sequence of insns to restore.  Place these insns in front of
   CHAIN if BEFORE_P is nonzero, behind the insn otherwise.  MAXRESTORE is
   the maximum number of registers which should be restored during this call.
   It should never be less than 1 since we only work with entire registers.

   Note that we have verified in init_caller_save that we can do this
   with a simple SET, so use it.  Set INSN_CODE to what we save there
   since the address might not be valid so the insn might not be recognized.
   These insns will be reloaded and have register elimination done by
   find_reload, so we need not worry about that here.

   Return the extra number of registers saved.  */

static int
insert_restore (struct insn_chain *chain, int before_p, int regno,
		int maxrestore, machine_mode *save_mode)
{
  int i, k;
  rtx pat = NULL_RTX;
  int code;
  unsigned int numregs = 0;
  struct insn_chain *new_chain;
  rtx mem;

  /* A common failure mode if register status is not correct in the
     RTL is for this routine to be called with a REGNO we didn't
     expect to save.  That will cause us to write an insn with a (nil)
     SET_DEST or SET_SRC.  Instead of doing so and causing a crash
     later, check for this common case here instead.  This will remove
     one step in debugging such problems.  */
  gcc_assert (regno_save_mem[regno][1]);

  /* Get the pattern to emit and update our status.

     See if we can restore `maxrestore' registers at once.  Work
     backwards to the single register case.  */
  for (i = maxrestore; i > 0; i--)
    {
      int j;
      int ok = 1;

      if (regno_save_mem[regno][i] == 0)
	continue;

      for (j = 0; j < i; j++)
	if (! TEST_HARD_REG_BIT (hard_regs_saved, regno + j))
	  {
	    ok = 0;
	    break;
	  }
      /* Must do this one restore at a time.  */
      if (! ok)
	continue;

      numregs = i;
      break;
    }

  mem = regno_save_mem [regno][numregs];
  if (save_mode [regno] != VOIDmode
      && save_mode [regno] != GET_MODE (mem)
      && numregs == hard_regno_nregs (regno, save_mode [regno])
      /* Check that insn to restore REGNO in save_mode[regno] is
	 correct.  */
      && reg_save_code (regno, save_mode[regno]) >= 0)
    mem = adjust_address_nv (mem, save_mode[regno], 0);
  else
    mem = copy_rtx (mem);

  /* Verify that the alignment of spill space is equal to or greater
     than required.  */
  gcc_assert (MIN (MAX_SUPPORTED_STACK_ALIGNMENT,
		   GET_MODE_ALIGNMENT (GET_MODE (mem))) <= MEM_ALIGN (mem));

  pat = gen_rtx_SET (gen_rtx_REG (GET_MODE (mem), regno), mem);
  code = reg_restore_code (regno, GET_MODE (mem));
  new_chain = insert_one_insn (chain, before_p, code, pat);

  /* Clear status for all registers we restored.  */
  for (k = 0; k < i; k++)
    {
      CLEAR_HARD_REG_BIT (hard_regs_saved, regno + k);
      SET_REGNO_REG_SET (&new_chain->dead_or_set, regno + k);
      n_regs_saved--;
    }

  /* Tell our callers how many extra registers we saved/restored.  */
  return numregs - 1;
}

/* Like insert_restore above, but save registers instead.  */

static int
insert_save (struct insn_chain *chain, int before_p, int regno,
	     HARD_REG_SET (*to_save), machine_mode *save_mode)
{
  int i;
  unsigned int k;
  rtx pat = NULL_RTX;
  int code;
  unsigned int numregs = 0;
  struct insn_chain *new_chain;
  rtx mem;

  /* A common failure mode if register status is not correct in the
     RTL is for this routine to be called with a REGNO we didn't
     expect to save.  That will cause us to write an insn with a (nil)
     SET_DEST or SET_SRC.  Instead of doing so and causing a crash
     later, check for this common case here.  This will remove one
     step in debugging such problems.  */
  gcc_assert (regno_save_mem[regno][1]);

  /* Get the pattern to emit and update our status.

     See if we can save several registers with a single instruction.
     Work backwards to the single register case.  */
  for (i = MOVE_MAX_WORDS; i > 0; i--)
    {
      int j;
      int ok = 1;
      if (regno_save_mem[regno][i] == 0)
	continue;

      for (j = 0; j < i; j++)
	if (! TEST_HARD_REG_BIT (*to_save, regno + j))
	  {
	    ok = 0;
	    break;
	  }
      /* Must do this one save at a time.  */
      if (! ok)
	continue;

      numregs = i;
      break;
    }

  mem = regno_save_mem [regno][numregs];
  if (save_mode [regno] != VOIDmode
      && save_mode [regno] != GET_MODE (mem)
      && numregs == hard_regno_nregs (regno, save_mode [regno])
      /* Check that insn to save REGNO in save_mode[regno] is
	 correct.  */
      && reg_save_code (regno, save_mode[regno]) >= 0)
    mem = adjust_address_nv (mem, save_mode[regno], 0);
  else
    mem = copy_rtx (mem);

  /* Verify that the alignment of spill space is equal to or greater
     than required.  */
  gcc_assert (MIN (MAX_SUPPORTED_STACK_ALIGNMENT,
		   GET_MODE_ALIGNMENT (GET_MODE (mem))) <= MEM_ALIGN (mem));

  pat = gen_rtx_SET (mem, gen_rtx_REG (GET_MODE (mem), regno));
  code = reg_save_code (regno, GET_MODE (mem));
  new_chain = insert_one_insn (chain, before_p, code, pat);

  /* Set hard_regs_saved and dead_or_set for all the registers we saved.  */
  for (k = 0; k < numregs; k++)
    {
      SET_HARD_REG_BIT (hard_regs_saved, regno + k);
      SET_REGNO_REG_SET (&new_chain->dead_or_set, regno + k);
      n_regs_saved++;
    }

  /* Tell our callers how many extra registers we saved/restored.  */
  return numregs - 1;
}

/* A note_uses callback used by insert_one_insn.  Add the hard-register
   equivalent of each REG to regset DATA.  */

static void
add_used_regs (rtx *loc, void *data)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, *loc, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x))
	{
	  unsigned int regno = REGNO (x);
	  if (HARD_REGISTER_NUM_P (regno))
	    bitmap_set_range ((regset) data, regno, REG_NREGS (x));
	  else
	    gcc_checking_assert (reg_renumber[regno] < 0);
	}
    }
}

/* Emit a new caller-save insn and set the code.  */
static struct insn_chain *
insert_one_insn (struct insn_chain *chain, int before_p, int code, rtx pat)
{
  rtx_insn *insn = chain->insn;
  struct insn_chain *new_chain;

  /* If INSN references CC0, put our insns in front of the insn that sets
     CC0.  This is always safe, since the only way we could be passed an
     insn that references CC0 is for a restore, and doing a restore earlier
     isn't a problem.  We do, however, assume here that CALL_INSNs don't
     reference CC0.  Guard against non-INSN's like CODE_LABEL.  */

  if (HAVE_cc0 && (NONJUMP_INSN_P (insn) || JUMP_P (insn))
      && before_p
      && reg_referenced_p (cc0_rtx, PATTERN (insn)))
    chain = chain->prev, insn = chain->insn;

  new_chain = new_insn_chain ();
  if (before_p)
    {
      rtx link;

      new_chain->prev = chain->prev;
      if (new_chain->prev != 0)
	new_chain->prev->next = new_chain;
      else
	reload_insn_chain = new_chain;

      chain->prev = new_chain;
      new_chain->next = chain;
      new_chain->insn = emit_insn_before (pat, insn);
      /* ??? It would be nice if we could exclude the already / still saved
	 registers from the live sets.  */
      COPY_REG_SET (&new_chain->live_throughout, &chain->live_throughout);
      note_uses (&PATTERN (chain->insn), add_used_regs,
		 &new_chain->live_throughout);
      /* If CHAIN->INSN is a call, then the registers which contain
	 the arguments to the function are live in the new insn.  */
      if (CALL_P (chain->insn))
	for (link = CALL_INSN_FUNCTION_USAGE (chain->insn);
	     link != NULL_RTX;
	     link = XEXP (link, 1))
	  note_uses (&XEXP (link, 0), add_used_regs,
		     &new_chain->live_throughout);

      CLEAR_REG_SET (&new_chain->dead_or_set);
      if (chain->insn == BB_HEAD (BASIC_BLOCK_FOR_FN (cfun, chain->block)))
	BB_HEAD (BASIC_BLOCK_FOR_FN (cfun, chain->block)) = new_chain->insn;
    }
  else
    {
      new_chain->next = chain->next;
      if (new_chain->next != 0)
	new_chain->next->prev = new_chain;
      chain->next = new_chain;
      new_chain->prev = chain;
      new_chain->insn = emit_insn_after (pat, insn);
      /* ??? It would be nice if we could exclude the already / still saved
	 registers from the live sets, and observe REG_UNUSED notes.  */
      COPY_REG_SET (&new_chain->live_throughout, &chain->live_throughout);
      /* Registers that are set in CHAIN->INSN live in the new insn.
	 (Unless there is a REG_UNUSED note for them, but we don't
	  look for them here.) */
      note_stores (PATTERN (chain->insn), add_stored_regs,
		   &new_chain->live_throughout);
      CLEAR_REG_SET (&new_chain->dead_or_set);
      if (chain->insn == BB_END (BASIC_BLOCK_FOR_FN (cfun, chain->block)))
	BB_END (BASIC_BLOCK_FOR_FN (cfun, chain->block)) = new_chain->insn;
    }
  new_chain->block = chain->block;
  new_chain->is_caller_save_insn = 1;

  INSN_CODE (new_chain->insn) = code;
  return new_chain;
}
#include "gt-caller-save.h"
