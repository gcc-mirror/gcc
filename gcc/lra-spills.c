/* Change pseudos by memory.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

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
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	 */


/* This file contains code for a pass to change spilled pseudos into
   memory.

   The pass creates necessary stack slots and assigns spilled pseudos
   to the stack slots in following way:

   for all spilled pseudos P most frequently used first do
     for all stack slots S do
       if P doesn't conflict with pseudos assigned to S then
	 assign S to P and goto to the next pseudo process
       end
     end
     create new stack slot S and assign P to S
   end

   The actual algorithm is bit more complicated because of different
   pseudo sizes.

   After that the code changes spilled pseudos (except ones created
   from scratches) by corresponding stack slot memory in RTL.

   If at least one stack slot was created, we need to run more passes
   because we have new addresses which should be checked and because
   the old address displacements might change and address constraints
   (or insn memory constraints) might not be satisfied any more.

   For some targets, the pass can spill some pseudos into hard
   registers of different class (usually into vector registers)
   instead of spilling them into memory if it is possible and
   profitable.  Spilling GENERAL_REGS pseudo into SSE registers for
   Intel Corei7 is an example of such optimization.  And this is
   actually recommended by Intel optimization guide.

   The file also contains code for final change of pseudos on hard
   regs correspondingly assigned to them.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "except.h"
#include "timevar.h"
#include "target.h"
#include "lra-int.h"
#include "ira.h"
#include "df.h"


/* Max regno at the start of the pass.	*/
static int regs_num;

/* Map spilled regno -> hard regno used instead of memory for
   spilling.  */
static rtx *spill_hard_reg;

/* The structure describes stack slot of a spilled pseudo.  */
struct pseudo_slot
{
  /* Number (0, 1, ...) of the stack slot to which given pseudo
     belongs.  */
  int slot_num;
  /* First or next slot with the same slot number.  */
  struct pseudo_slot *next, *first;
  /* Memory representing the spilled pseudo.  */
  rtx mem;
};

/* The stack slots for each spilled pseudo.  Indexed by regnos.	 */
static struct pseudo_slot *pseudo_slots;

/* The structure describes a register or a stack slot which can be
   used for several spilled pseudos.  */
struct slot
{
  /* First pseudo with given stack slot.  */
  int regno;
  /* Hard reg into which the slot pseudos are spilled.	The value is
     negative for pseudos spilled into memory.	*/
  int hard_regno;
  /* Memory representing the all stack slot.  It can be different from
     memory representing a pseudo belonging to give stack slot because
     pseudo can be placed in a part of the corresponding stack slot.
     The value is NULL for pseudos spilled into a hard reg.  */
  rtx mem;
  /* Combined live ranges of all pseudos belonging to given slot.  It
     is used to figure out that a new spilled pseudo can use given
     stack slot.  */
  lra_live_range_t live_ranges;
};

/* Array containing info about the stack slots.	 The array element is
   indexed by the stack slot number in the range [0..slots_num).  */
static struct slot *slots;
/* The number of the stack slots currently existing.  */
static int slots_num;

/* Set up memory of the spilled pseudo I.  The function can allocate
   the corresponding stack slot if it is not done yet.	*/
static void
assign_mem_slot (int i)
{
  rtx x = NULL_RTX;
  enum machine_mode mode = GET_MODE (regno_reg_rtx[i]);
  unsigned int inherent_size = PSEUDO_REGNO_BYTES (i);
  unsigned int inherent_align = GET_MODE_ALIGNMENT (mode);
  unsigned int max_ref_width = GET_MODE_SIZE (lra_reg_info[i].biggest_mode);
  unsigned int total_size = MAX (inherent_size, max_ref_width);
  unsigned int min_align = max_ref_width * BITS_PER_UNIT;
  int adjust = 0;

  lra_assert (regno_reg_rtx[i] != NULL_RTX && REG_P (regno_reg_rtx[i])
	      && lra_reg_info[i].nrefs != 0 && reg_renumber[i] < 0);

  x = slots[pseudo_slots[i].slot_num].mem;

  /* We can use a slot already allocated because it is guaranteed the
     slot provides both enough inherent space and enough total
     space.  */
  if (x)
    ;
  /* Each pseudo has an inherent size which comes from its own mode,
     and a total size which provides room for paradoxical subregs
     which refer to the pseudo reg in wider modes.  We allocate a new
     slot, making sure that it has enough inherent space and total
     space.  */
  else
    {
      rtx stack_slot;

      /* No known place to spill from => no slot to reuse.  */
      x = assign_stack_local (mode, total_size,
			      min_align > inherent_align
			      || total_size > inherent_size ? -1 : 0);
      stack_slot = x;
      /* Cancel the big-endian correction done in assign_stack_local.
	 Get the address of the beginning of the slot.	This is so we
	 can do a big-endian correction unconditionally below.	*/
      if (BYTES_BIG_ENDIAN)
	{
	  adjust = inherent_size - total_size;
	  if (adjust)
	    stack_slot
	      = adjust_address_nv (x,
				   mode_for_size (total_size * BITS_PER_UNIT,
						  MODE_INT, 1),
				   adjust);
	}
      slots[pseudo_slots[i].slot_num].mem = stack_slot;
    }

  /* On a big endian machine, the "address" of the slot is the address
     of the low part that fits its inherent mode.  */
  if (BYTES_BIG_ENDIAN && inherent_size < total_size)
    adjust += (total_size - inherent_size);

  x = adjust_address_nv (x, GET_MODE (regno_reg_rtx[i]), adjust);

  /* Set all of the memory attributes as appropriate for a spill.  */
  set_mem_attrs_for_spill (x);
  pseudo_slots[i].mem = x;
}

/* Sort pseudos according their usage frequencies.  */
static int
regno_freq_compare (const void *v1p, const void *v2p)
{
  const int regno1 = *(const int *) v1p;
  const int regno2 = *(const int *) v2p;
  int diff;

  if ((diff = lra_reg_info[regno2].freq - lra_reg_info[regno1].freq) != 0)
    return diff;
  return regno1 - regno2;
}

/* Redefine STACK_GROWS_DOWNWARD in terms of 0 or 1.  */
#ifdef STACK_GROWS_DOWNWARD
# undef STACK_GROWS_DOWNWARD
# define STACK_GROWS_DOWNWARD 1
#else
# define STACK_GROWS_DOWNWARD 0
#endif

/* Sort pseudos according to their slots, putting the slots in the order
   that they should be allocated.  Slots with lower numbers have the highest
   priority and should get the smallest displacement from the stack or
   frame pointer (whichever is being used).

   The first allocated slot is always closest to the frame pointer,
   so prefer lower slot numbers when frame_pointer_needed.  If the stack
   and frame grow in the same direction, then the first allocated slot is
   always closest to the initial stack pointer and furthest away from the
   final stack pointer, so allocate higher numbers first when using the
   stack pointer in that case.  The reverse is true if the stack and
   frame grow in opposite directions.  */
static int
pseudo_reg_slot_compare (const void *v1p, const void *v2p)
{
  const int regno1 = *(const int *) v1p;
  const int regno2 = *(const int *) v2p;
  int diff, slot_num1, slot_num2;
  int total_size1, total_size2;

  slot_num1 = pseudo_slots[regno1].slot_num;
  slot_num2 = pseudo_slots[regno2].slot_num;
  if ((diff = slot_num1 - slot_num2) != 0)
    return (frame_pointer_needed
	    || !FRAME_GROWS_DOWNWARD == STACK_GROWS_DOWNWARD ? diff : -diff);
  total_size1 = GET_MODE_SIZE (lra_reg_info[regno1].biggest_mode);
  total_size2 = GET_MODE_SIZE (lra_reg_info[regno2].biggest_mode);
  if ((diff = total_size2 - total_size1) != 0)
    return diff;
  return regno1 - regno2;
}

/* Assign spill hard registers to N pseudos in PSEUDO_REGNOS which is
   sorted in order of highest frequency first.  Put the pseudos which
   did not get a spill hard register at the beginning of array
   PSEUDO_REGNOS.  Return the number of such pseudos.  */
static int
assign_spill_hard_regs (int *pseudo_regnos, int n)
{
  int i, k, p, regno, res, spill_class_size, hard_regno, nr;
  enum reg_class rclass, spill_class;
  enum machine_mode mode;
  lra_live_range_t r;
  rtx insn, set;
  basic_block bb;
  HARD_REG_SET conflict_hard_regs;
  bitmap_head ok_insn_bitmap;
  bitmap setjump_crosses = regstat_get_setjmp_crosses ();
  /* Hard registers which can not be used for any purpose at given
     program point because they are unallocatable or already allocated
     for other pseudos.	 */
  HARD_REG_SET *reserved_hard_regs;

  if (! lra_reg_spill_p)
    return n;
  /* Set up reserved hard regs for every program point.	 */
  reserved_hard_regs = XNEWVEC (HARD_REG_SET, lra_live_max_point);
  for (p = 0; p < lra_live_max_point; p++)
    COPY_HARD_REG_SET (reserved_hard_regs[p], lra_no_alloc_regs);
  for (i = FIRST_PSEUDO_REGISTER; i < regs_num; i++)
    if (lra_reg_info[i].nrefs != 0
	&& (hard_regno = lra_get_regno_hard_regno (i)) >= 0)
      for (r = lra_reg_info[i].live_ranges; r != NULL; r = r->next)
	for (p = r->start; p <= r->finish; p++)
	  add_to_hard_reg_set (&reserved_hard_regs[p],
			       lra_reg_info[i].biggest_mode, hard_regno);
  bitmap_initialize (&ok_insn_bitmap, &reg_obstack);
  FOR_EACH_BB (bb)
    FOR_BB_INSNS (bb, insn)
      if (DEBUG_INSN_P (insn)
	  || ((set = single_set (insn)) != NULL_RTX
	      && REG_P (SET_SRC (set)) && REG_P (SET_DEST (set))))
	bitmap_set_bit (&ok_insn_bitmap, INSN_UID (insn));
  for (res = i = 0; i < n; i++)
    {
      regno = pseudo_regnos[i];
      rclass = lra_get_allocno_class (regno);
      if (bitmap_bit_p (setjump_crosses, regno)
	  || (spill_class
	      = ((enum reg_class)
		 targetm.spill_class ((reg_class_t) rclass,
				      PSEUDO_REGNO_MODE (regno)))) == NO_REGS
	  || bitmap_intersect_compl_p (&lra_reg_info[regno].insn_bitmap,
				       &ok_insn_bitmap))
	{
	  pseudo_regnos[res++] = regno;
	  continue;
	}
      lra_assert (spill_class != NO_REGS);
      COPY_HARD_REG_SET (conflict_hard_regs,
			 lra_reg_info[regno].conflict_hard_regs);
      for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
	for (p = r->start; p <= r->finish; p++)
	  IOR_HARD_REG_SET (conflict_hard_regs, reserved_hard_regs[p]);
      spill_class_size = ira_class_hard_regs_num[spill_class];
      mode = lra_reg_info[regno].biggest_mode;
      for (k = 0; k < spill_class_size; k++)
	{
	  hard_regno = ira_class_hard_regs[spill_class][k];
	  if (! overlaps_hard_reg_set_p (conflict_hard_regs, mode, hard_regno))
	    break;
	}
      if (k >= spill_class_size)
	{
	   /* There is no available regs -- assign memory later.  */
	  pseudo_regnos[res++] = regno;
	  continue;
	}
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "  Spill r%d into hr%d\n", regno, hard_regno);
      /* Update reserved_hard_regs.  */
      for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
	for (p = r->start; p <= r->finish; p++)
	  add_to_hard_reg_set (&reserved_hard_regs[p],
			       lra_reg_info[regno].biggest_mode, hard_regno);
      spill_hard_reg[regno]
	= gen_raw_REG (PSEUDO_REGNO_MODE (regno), hard_regno);
      for (nr = 0;
	   nr < hard_regno_nregs[hard_regno][lra_reg_info[regno].biggest_mode];
	   nr++)
	/* Just loop.  */
	df_set_regs_ever_live (hard_regno + nr, true);
    }
  bitmap_clear (&ok_insn_bitmap);
  free (reserved_hard_regs);
  return res;
}

/* Add pseudo REGNO to slot SLOT_NUM.  */
static void
add_pseudo_to_slot (int regno, int slot_num)
{
  struct pseudo_slot *first;

  if (slots[slot_num].regno < 0)
    {
      /* It is the first pseudo in the slot.  */
      slots[slot_num].regno = regno;
      pseudo_slots[regno].first = &pseudo_slots[regno];
      pseudo_slots[regno].next = NULL;
    }
  else
    {
      first = pseudo_slots[regno].first = &pseudo_slots[slots[slot_num].regno];
      pseudo_slots[regno].next = first->next;
      first->next = &pseudo_slots[regno];
    }
  pseudo_slots[regno].mem = NULL_RTX;
  pseudo_slots[regno].slot_num = slot_num;
  slots[slot_num].live_ranges
    = lra_merge_live_ranges (slots[slot_num].live_ranges,
			     lra_copy_live_range_list
			     (lra_reg_info[regno].live_ranges));
}

/* Assign stack slot numbers to pseudos in array PSEUDO_REGNOS of
   length N.  Sort pseudos in PSEUDO_REGNOS for subsequent assigning
   memory stack slots.	*/
static void
assign_stack_slot_num_and_sort_pseudos (int *pseudo_regnos, int n)
{
  int i, j, regno;

  slots_num = 0;
  /* Assign stack slot numbers to spilled pseudos, use smaller numbers
     for most frequently used pseudos.	*/
  for (i = 0; i < n; i++)
    {
      regno = pseudo_regnos[i];
      if (! flag_ira_share_spill_slots)
	j = slots_num;
      else
	{
	  for (j = 0; j < slots_num; j++)
	    if (slots[j].hard_regno < 0
		&& ! (lra_intersected_live_ranges_p
		      (slots[j].live_ranges,
		       lra_reg_info[regno].live_ranges)))
	      break;
	}
      if (j >= slots_num)
	{
	  /* New slot.	*/
	  slots[j].live_ranges = NULL;
	  slots[j].regno = slots[j].hard_regno = -1;
	  slots[j].mem = NULL_RTX;
	  slots_num++;
	}
      add_pseudo_to_slot (regno, j);
    }
  /* Sort regnos according to their slot numbers.  */
  qsort (pseudo_regnos, n, sizeof (int), pseudo_reg_slot_compare);
}

/* Recursively process LOC in INSN and change spilled pseudos to the
   corresponding memory or spilled hard reg.  Ignore spilled pseudos
   created from the scratches.	*/
static void
remove_pseudos (rtx *loc, rtx insn)
{
  int i;
  rtx hard_reg;
  const char *fmt;
  enum rtx_code code;

  if (*loc == NULL_RTX)
    return;
  code = GET_CODE (*loc);
  if (code == REG && (i = REGNO (*loc)) >= FIRST_PSEUDO_REGISTER
      && lra_get_regno_hard_regno (i) < 0
      /* We do not want to assign memory for former scratches because
	 it might result in an address reload for some targets.	 In
	 any case we transform such pseudos not getting hard registers
	 into scratches back.  */
      && ! lra_former_scratch_p (i))
    {
      if ((hard_reg = spill_hard_reg[i]) != NULL_RTX)
	*loc = copy_rtx (hard_reg);
      else
	{
	  rtx x = lra_eliminate_regs_1 (insn, pseudo_slots[i].mem,
					GET_MODE (pseudo_slots[i].mem),
					false, false, true);
	  *loc = x != pseudo_slots[i].mem ? x : copy_rtx (x);
	}
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	remove_pseudos (&XEXP (*loc, i), insn);
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (*loc, i) - 1; j >= 0; j--)
	    remove_pseudos (&XVECEXP (*loc, i, j), insn);
	}
    }
}

/* Convert spilled pseudos into their stack slots or spill hard regs,
   put insns to process on the constraint stack (that is all insns in
   which pseudos were changed to memory or spill hard regs).   */
static void
spill_pseudos (void)
{
  basic_block bb;
  rtx insn;
  int i;
  bitmap_head spilled_pseudos, changed_insns;

  bitmap_initialize (&spilled_pseudos, &reg_obstack);
  bitmap_initialize (&changed_insns, &reg_obstack);
  for (i = FIRST_PSEUDO_REGISTER; i < regs_num; i++)
    {
      if (lra_reg_info[i].nrefs != 0 && lra_get_regno_hard_regno (i) < 0
	  && ! lra_former_scratch_p (i))
	{
	  bitmap_set_bit (&spilled_pseudos, i);
	  bitmap_ior_into (&changed_insns, &lra_reg_info[i].insn_bitmap);
	}
    }
  FOR_EACH_BB (bb)
    {
      FOR_BB_INSNS (bb, insn)
	if (bitmap_bit_p (&changed_insns, INSN_UID (insn)))
	  {
	    rtx *link_loc, link;
	    remove_pseudos (&PATTERN (insn), insn);
	    if (CALL_P (insn))
	      remove_pseudos (&CALL_INSN_FUNCTION_USAGE (insn), insn);
	    for (link_loc = &REG_NOTES (insn);
		 (link = *link_loc) != NULL_RTX;
		 link_loc = &XEXP (link, 1))
	      {
		switch (REG_NOTE_KIND (link))
		  {
		  case REG_FRAME_RELATED_EXPR:
		  case REG_CFA_DEF_CFA:
		  case REG_CFA_ADJUST_CFA:
		  case REG_CFA_OFFSET:
		  case REG_CFA_REGISTER:
		  case REG_CFA_EXPRESSION:
		  case REG_CFA_RESTORE:
		  case REG_CFA_SET_VDRAP:
		    remove_pseudos (&XEXP (link, 0), insn);
		    break;
		  default:
		    break;
		  }
	      }
	    if (lra_dump_file != NULL)
	      fprintf (lra_dump_file,
		       "Changing spilled pseudos to memory in insn #%u\n",
		       INSN_UID (insn));
	    lra_push_insn (insn);
	    if (lra_reg_spill_p || targetm.different_addr_displacement_p ())
	      lra_set_used_insn_alternative (insn, -1);
	  }
	else if (CALL_P (insn))
	  /* Presence of any pseudo in CALL_INSN_FUNCTION_USAGE does
	     not affect value of insn_bitmap of the corresponding
	     lra_reg_info.  That is because we don't need to reload
	     pseudos in CALL_INSN_FUNCTION_USAGEs.  So if we process
	     only insns in the insn_bitmap of given pseudo here, we
	     can miss the pseudo in some
	     CALL_INSN_FUNCTION_USAGEs.  */
	  remove_pseudos (&CALL_INSN_FUNCTION_USAGE (insn), insn);
      bitmap_and_compl_into (df_get_live_in (bb), &spilled_pseudos);
      bitmap_and_compl_into (df_get_live_out (bb), &spilled_pseudos);
    }
  bitmap_clear (&spilled_pseudos);
  bitmap_clear (&changed_insns);
}

/* Return true if we need to change some pseudos into memory.  */
bool
lra_need_for_spills_p (void)
{
  int i; max_regno = max_reg_num ();

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (lra_reg_info[i].nrefs != 0 && lra_get_regno_hard_regno (i) < 0
	&& ! lra_former_scratch_p (i))
      return true;
  return false;
}

/* Change spilled pseudos into memory or spill hard regs.  Put changed
   insns on the constraint stack (these insns will be considered on
   the next constraint pass).  The changed insns are all insns in
   which pseudos were changed.  */
void
lra_spill (void)
{
  int i, n, curr_regno;
  int *pseudo_regnos;

  regs_num = max_reg_num ();
  spill_hard_reg = XNEWVEC (rtx, regs_num);
  pseudo_regnos = XNEWVEC (int, regs_num);
  for (n = 0, i = FIRST_PSEUDO_REGISTER; i < regs_num; i++)
    if (lra_reg_info[i].nrefs != 0 && lra_get_regno_hard_regno (i) < 0
	/* We do not want to assign memory for former scratches.  */
	&& ! lra_former_scratch_p (i))
      {
	spill_hard_reg[i] = NULL_RTX;
	pseudo_regnos[n++] = i;
      }
  lra_assert (n > 0);
  pseudo_slots = XNEWVEC (struct pseudo_slot, regs_num);
  slots = XNEWVEC (struct slot, regs_num);
  /* Sort regnos according their usage frequencies.  */
  qsort (pseudo_regnos, n, sizeof (int), regno_freq_compare);
  n = assign_spill_hard_regs (pseudo_regnos, n);
  assign_stack_slot_num_and_sort_pseudos (pseudo_regnos, n);
  for (i = 0; i < n; i++)
    if (pseudo_slots[pseudo_regnos[i]].mem == NULL_RTX)
      assign_mem_slot (pseudo_regnos[i]);
  if (n > 0 && crtl->stack_alignment_needed)
    /* If we have a stack frame, we must align it now.  The stack size
       may be a part of the offset computation for register
       elimination.  */
    assign_stack_local (BLKmode, 0, crtl->stack_alignment_needed);
  if (lra_dump_file != NULL)
    {
      for (i = 0; i < slots_num; i++)
	{
	  fprintf (lra_dump_file, "  Slot %d regnos (width = %d):", i,
		   GET_MODE_SIZE (GET_MODE (slots[i].mem)));
	  for (curr_regno = slots[i].regno;;
	       curr_regno = pseudo_slots[curr_regno].next - pseudo_slots)
	    {
	      fprintf (lra_dump_file, "	 %d", curr_regno);
	      if (pseudo_slots[curr_regno].next == NULL)
		break;
	    }
	  fprintf (lra_dump_file, "\n");
	}
    }
  spill_pseudos ();
  free (slots);
  free (pseudo_slots);
  free (pseudo_regnos);
  free (spill_hard_reg);
}

/* Apply alter_subreg for subregs of regs in *LOC.  Use FINAL_P for
   alter_subreg calls. Return true if any subreg of reg is
   processed.  */
static bool
alter_subregs (rtx *loc, bool final_p)
{
  int i;
  rtx x = *loc;
  bool res;
  const char *fmt;
  enum rtx_code code;

  if (x == NULL_RTX)
    return false;
  code = GET_CODE (x);
  if (code == SUBREG && REG_P (SUBREG_REG (x)))
    {
      lra_assert (REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER);
      alter_subreg (loc, final_p);
      return true;
    }
  fmt = GET_RTX_FORMAT (code);
  res = false;
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (alter_subregs (&XEXP (x, i), final_p))
	    res = true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (alter_subregs (&XVECEXP (x, i, j), final_p))
	      res = true;
	}
    }
  return res;
}

/* Return true if REGNO is used for return in the current
   function.  */
static bool
return_regno_p (unsigned int regno)
{
  rtx outgoing = crtl->return_rtx;

  if (! outgoing)
    return false;

  if (REG_P (outgoing))
    return REGNO (outgoing) == regno;
  else if (GET_CODE (outgoing) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (outgoing, 0); i++)
	{
	  rtx x = XEXP (XVECEXP (outgoing, 0, i), 0);

	  if (REG_P (x) && REGNO (x) == regno)
	    return true;
	}
    }
  return false;
}

/* Final change of pseudos got hard registers into the corresponding
   hard registers and removing temporary clobbers.  */
void
lra_final_code_change (void)
{
  int i, hard_regno;
  basic_block bb;
  rtx insn, curr;
  int max_regno = max_reg_num ();

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (lra_reg_info[i].nrefs != 0
	&& (hard_regno = lra_get_regno_hard_regno (i)) >= 0)
      SET_REGNO (regno_reg_rtx[i], hard_regno);
  FOR_EACH_BB (bb)
    FOR_BB_INSNS_SAFE (bb, insn, curr)
      if (INSN_P (insn))
	{
	  rtx pat = PATTERN (insn);

	  if (GET_CODE (pat) == CLOBBER && LRA_TEMP_CLOBBER_P (pat))
	    {
	      /* Remove clobbers temporarily created in LRA.  We don't
		 need them anymore and don't want to waste compiler
		 time processing them in a few subsequent passes.  */
	      lra_invalidate_insn_data (insn);
	      delete_insn (insn);
	      continue;
	    }

	  /* IRA can generate move insns involving pseudos.  It is
	     better remove them earlier to speed up compiler a bit.
	     It is also better to do it here as they might not pass
	     final RTL check in LRA, (e.g. insn moving a control
	     register into itself).  So remove an useless move insn
	     unless next insn is USE marking the return reg (we should
	     save this as some subsequent optimizations assume that
	     such original insns are saved).  */
	  if (NONJUMP_INSN_P (insn) && GET_CODE (pat) == SET
	      && REG_P (SET_SRC (pat)) && REG_P (SET_DEST (pat))
	      && REGNO (SET_SRC (pat)) == REGNO (SET_DEST (pat))
	      && ! return_regno_p (REGNO (SET_SRC (pat))))
	    {
	      lra_invalidate_insn_data (insn);
	      delete_insn (insn);
	      continue;
	    }
	
	  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
	  struct lra_static_insn_data *static_id = id->insn_static_data;
	  bool insn_change_p = false;

	  for (i = id->insn_static_data->n_operands - 1; i >= 0; i--)
	    if ((DEBUG_INSN_P (insn) || ! static_id->operand[i].is_operator)
		&& alter_subregs (id->operand_loc[i], ! DEBUG_INSN_P (insn)))
	      {
		lra_update_dup (id, i);
		insn_change_p = true;
	      }
	  if (insn_change_p)
	    lra_update_operator_dups (id);
	}
}
