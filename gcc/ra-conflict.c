/* Allocate registers for pseudo-registers that span basic blocks.
   Copyright (C) 2007 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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
#include "tm.h"
#include "machmode.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "regs.h"
#include "function.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "output.h"
#include "toplev.h"
#include "tree-pass.h"
#include "timevar.h"
#include "df.h"
#include "vecprim.h"
#include "ra.h"
#include "sbitmap.h"
#include "sparseset.h"

/* Externs defined in regs.h.  */

int max_allocno;
struct allocno *allocno;
HOST_WIDEST_FAST_INT *conflicts;
int *reg_allocno;
HOST_WIDE_INT *partial_bitnum;
HOST_WIDE_INT max_bitnum;
alloc_pool adjacency_pool;
adjacency_t **adjacency;

typedef struct df_ref * df_ref_t;
DEF_VEC_P(df_ref_t);
DEF_VEC_ALLOC_P(df_ref_t,heap);

/* Macros to determine the bit number within the triangular bit matrix for
   the two allocnos Low and HIGH, with LOW strictly less than HIGH.  */

#define CONFLICT_BITNUM(I, J) \
  (((I) < (J)) ? (partial_bitnum[I] + (J)) : (partial_bitnum[J] + (I)))

#define CONFLICT_BITNUM_FAST(I, I_PARTIAL_BITNUM, J) \
  (((I) < (J)) ? ((I_PARTIAL_BITNUM) + (J)) : (partial_bitnum[J] + (I)))

bool
conflict_p (int allocno1, int allocno2)
{
  HOST_WIDE_INT bitnum;
  HOST_WIDEST_FAST_INT word, mask;

#ifdef ENABLE_CHECKING
  int blk1, blk2;

  gcc_assert (allocno1 >= 0 && allocno1 < max_allocno);
  gcc_assert (allocno2 >= 0 && allocno2 < max_allocno);

  blk1 = regno_basic_block (allocno[allocno1].reg);
  blk2 = regno_basic_block (allocno[allocno2].reg);
  gcc_assert (blk1 == 0 || blk2 == 0 || blk1 == blk2);
#endif

  if (allocno1 == allocno2)
    /* By definition, an allocno does not conflict with itself.  */
    return 0;

  bitnum = CONFLICT_BITNUM (allocno1, allocno2);

#ifdef ENABLE_CHECKING
  gcc_assert (bitnum >= 0 && bitnum < max_bitnum);
#endif

  word = conflicts[bitnum / HOST_BITS_PER_WIDEST_FAST_INT];
  mask = (HOST_WIDEST_FAST_INT) 1 << (bitnum % HOST_BITS_PER_WIDEST_FAST_INT);
  return (word & mask) != 0;
}

/* Add conflict edges between ALLOCNO1 and ALLOCNO2.  */

static void
set_conflict (int allocno1, int allocno2)
{
  HOST_WIDE_INT bitnum, index;
  HOST_WIDEST_FAST_INT word, mask;

#ifdef ENABLE_CHECKING
  int blk1, blk2;

  gcc_assert (allocno1 >= 0 && allocno1 < max_allocno);
  gcc_assert (allocno2 >= 0 && allocno2 < max_allocno);

  blk1 = regno_basic_block (allocno[allocno1].reg);
  blk2 = regno_basic_block (allocno[allocno2].reg);
  gcc_assert (blk1 == 0 || blk2 == 0 || blk1 == blk2);
#endif

  /* By definition, an allocno does not conflict with itself.  */
  if (allocno1 == allocno2)
    return;

  bitnum = CONFLICT_BITNUM (allocno1, allocno2);

#ifdef ENABLE_CHECKING
  gcc_assert (bitnum >= 0 && bitnum < max_bitnum);
#endif

  index = bitnum / HOST_BITS_PER_WIDEST_FAST_INT;
  word = conflicts[index];
  mask = (HOST_WIDEST_FAST_INT) 1 << (bitnum % HOST_BITS_PER_WIDEST_FAST_INT);

  if ((word & mask) == 0)
    {
      conflicts[index] = word | mask;
      add_neighbor (allocno1, allocno2);
      add_neighbor (allocno2, allocno1);
    }
}

/* Add conflict edges between ALLOCNO1 and all allocnos currently live.  */

static void
set_conflicts (int allocno1, sparseset live)
{
  int i;
  HOST_WIDE_INT bitnum, index;
  HOST_WIDEST_FAST_INT word, mask;
  HOST_WIDE_INT partial_bitnum_allocno1;

#ifdef ENABLE_CHECKING
  gcc_assert (allocno1 >= 0 && allocno1 < max_allocno);
#endif

  partial_bitnum_allocno1 = partial_bitnum[allocno1];

  EXECUTE_IF_SET_IN_SPARSESET (live, i)
  {
    /* By definition, an allocno does not conflict with itself.  */
    if (allocno1 == i)
      continue;

#ifdef ENABLE_CHECKING
    gcc_assert (i >= 0 && i < max_allocno);
#endif

    bitnum = CONFLICT_BITNUM_FAST (allocno1, partial_bitnum_allocno1, i);

#ifdef ENABLE_CHECKING
    gcc_assert (bitnum >= 0 && bitnum < max_bitnum);
#endif

    index = bitnum / HOST_BITS_PER_WIDEST_FAST_INT;
    word = conflicts[index];
    mask = (HOST_WIDEST_FAST_INT) 1 << (bitnum % HOST_BITS_PER_WIDEST_FAST_INT);

    if ((word & mask) == 0)
      {
	conflicts[index] = word | mask;
	add_neighbor (allocno1, i);
	add_neighbor (i, allocno1);
      }
  }
}


/* Add a conflict between R1 and R2.  */

static void
record_one_conflict_between_regnos (enum machine_mode mode1, int r1, 
				    enum machine_mode mode2, int r2)
{
  int allocno1 = reg_allocno[r1];
  int allocno2 = reg_allocno[r2];

  if (dump_file)
    fprintf (dump_file, "    rocbr adding %d<=>%d\n", r1, r2);

  if (allocno1 >= 0 && allocno2 >= 0)
    set_conflict (allocno1, allocno2);
  else if (allocno1 >= 0)
    {
      if (r2 < FIRST_PSEUDO_REGISTER)
	add_to_hard_reg_set (&allocno[allocno1].hard_reg_conflicts, mode2, r2);
    }
  else if (allocno2 >= 0)
    {
      if (r1 < FIRST_PSEUDO_REGISTER)
        add_to_hard_reg_set (&allocno[allocno2].hard_reg_conflicts, mode1, r1);
    }

  /* Now, recursively handle the reg_renumber cases.  */
  if (reg_renumber[r1] >= 0)
    record_one_conflict_between_regnos (mode1, reg_renumber[r1], mode2, r2);

  if (reg_renumber[r2] >= 0)
    record_one_conflict_between_regnos (mode1, r1, mode2, reg_renumber[r2]);
}


/* Record a conflict between register REGNO and everything currently
   live.  REGNO must not be a pseudo reg that was allocated by
   local_alloc; such numbers must be translated through reg_renumber
   before calling here.  */

static void
record_one_conflict (sparseset allocnos_live, 
		     HARD_REG_SET *hard_regs_live, int regno)
{
  int i;

  if (regno < FIRST_PSEUDO_REGISTER)
    /* When a hard register becomes live, record conflicts with live
       pseudo regs.  */
    EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
      {
	SET_HARD_REG_BIT (allocno[i].hard_reg_conflicts, regno);
	if (dump_file)
	  fprintf (dump_file, "  roc adding %d<=>%d\n", allocno[i].reg, regno);
      }
  else
    /* When a pseudo-register becomes live, record conflicts first
       with hard regs, then with other pseudo regs.  */
    {
      int ialloc = reg_allocno[regno];

      if (dump_file)
	{
	  fprintf (dump_file, "  roc adding %d<=>(", regno);
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (TEST_HARD_REG_BIT (*hard_regs_live, i) 
		&& !TEST_HARD_REG_BIT (allocno[ialloc].hard_reg_conflicts, i))
	      fprintf (dump_file, "%d ", i);
	  
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	    {
	      if (!conflict_p (ialloc, i))
		fprintf (dump_file, "%d ", allocno[i].reg);
	    }
	  fprintf (dump_file, ")\n");
	}

      IOR_HARD_REG_SET (allocno[ialloc].hard_reg_conflicts, *hard_regs_live);
      set_conflicts (ialloc, allocnos_live);
    }
}


/* Handle the case where REG is set by the insn being scanned, during
   the backward scan to accumulate conflicts.  Record a conflict with
   all other registers already live.

   REG might actually be something other than a register; if so, we do
   nothing.  */

static void
mark_reg_store (sparseset allocnos_live, 
		HARD_REG_SET *hard_regs_live, 
		struct df_ref *ref)
{
  rtx reg = DF_REF_REG (ref);
  unsigned int regno = DF_REF_REGNO (ref);
  enum machine_mode mode = GET_MODE (reg);

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is or has a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER && reg_allocno[regno] >= 0)
    record_one_conflict (allocnos_live, hard_regs_live, regno);

  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (regno < FIRST_PSEUDO_REGISTER && ! fixed_regs[regno])
    {
      unsigned int start = regno;
      unsigned int last = end_hard_regno (mode, regno);
      if ((GET_CODE (reg) == SUBREG) && !DF_REF_FLAGS_IS_SET (ref, DF_REF_EXTRACT))
	{
	  start += subreg_regno_offset (regno, GET_MODE (SUBREG_REG (reg)),
					SUBREG_BYTE (reg), GET_MODE (reg));
	  last = start + subreg_nregs_with_regno (regno, reg);
	}

      regno = start;
      while (regno < last)
	record_one_conflict (allocnos_live, hard_regs_live, regno++);
    }
}


/* Return true if REGNO with MODE can be assigned to a register in
   CL.  */

static bool
may_overlap_class_p (enum machine_mode mode, unsigned int regno, 
		     enum reg_class rc)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      enum reg_class pref_class = reg_preferred_class (regno);
      enum reg_class alt_class = reg_alternate_class (regno);
      return (reg_classes_intersect_p (rc, pref_class)
	      || reg_classes_intersect_p (rc, alt_class));
    }
  else
    return in_hard_reg_set_p (reg_class_contents[rc], mode, regno);
}


/* SRC is an input operand to an instruction in which register DEST is
   an output operand.  SRC may be bound to a member of class SRC_CLASS
   and DEST may be bound to an earlyclobbered register that overlaps
   SRC_CLASS.  If SRC is a register that might be allocated a member
   of SRC_CLASS, add a conflict between it and DEST.  */

static void
add_conflicts_for_earlyclobber (rtx dest, enum reg_class src_class, rtx src)
{
  if (GET_CODE (src) == SUBREG)
    src = SUBREG_REG (src);
  if (REG_P (src)
      && may_overlap_class_p (GET_MODE (src), REGNO (src), src_class))
    record_one_conflict_between_regnos (GET_MODE (src), REGNO (src),
                                        GET_MODE (dest), REGNO (dest));
}


/* Look at the defs in INSN and determine if any of them are marked as
   early clobber.  If they are marked as early clobber, add a conflict
   between any input operand that could be allocated to the same
   register.  */

static void
set_conflicts_for_earlyclobber (rtx insn)
{
  int alt;
  int def;
  int use;

  extract_insn (insn);
  preprocess_constraints ();

  if (dump_file) 
    fprintf (dump_file, "  starting early clobber conflicts.\n");

  for (alt = 0; alt < recog_data.n_alternatives; alt++)
    for (def = 0; def < recog_data.n_operands; def++)
      if ((recog_op_alt[def][alt].earlyclobber)
	  && (recog_op_alt[def][alt].cl != NO_REGS))
	{
	  rtx dreg = recog_data.operand[def];
	  enum machine_mode dmode = recog_data.operand_mode[def];
	  if (GET_CODE (dreg) == SUBREG)
	    dreg = SUBREG_REG (dreg);
	  if (REG_P (dreg)
	      &&  may_overlap_class_p (dmode, REGNO (dreg), recog_op_alt[def][alt].cl))

	    for (use = 0; use < recog_data.n_operands; use++)
	      if (use != def
		  && recog_data.operand_type[use] != OP_OUT
		  && reg_classes_intersect_p (recog_op_alt[def][alt].cl,
					      recog_op_alt[use][alt].cl))
		{
		  add_conflicts_for_earlyclobber (dreg,
						  recog_op_alt[use][alt].cl,
						  recog_data.operand[use]);
		  /*  Reload may end up swapping commutative operands,
		      so you have to take both orderings into account.
		      The constraints for the two operands can be
		      completely different.  (Indeed, if the
		      constraints for the two operands are the same
		      for all alternatives, there's no point marking
		      them as commutative.)  */
		  if (use < recog_data.n_operands + 1
		      && recog_data.constraints[use][0] == '%')
		    add_conflicts_for_earlyclobber (dreg,
						    recog_op_alt[use][alt].cl,
						    recog_data.operand[use + 1]);
		}
	}
}


/* Init LIVE_SUBREGS[ALLOCNUM] and LIVE_SUBREGS_USED[ALLOCNUM] using
   REG to the the number of nregs, and INIT_VALUE to get the
   initialization.  ALLOCNUM need not be the regno of REG.  */

void
ra_init_live_subregs (bool init_value, 
		      sbitmap *live_subregs, 
		      int *live_subregs_used,
		      int allocnum, 
		      rtx reg)
{
  unsigned int regno = REGNO (SUBREG_REG (reg));
  int size = GET_MODE_SIZE (GET_MODE (regno_reg_rtx[regno]));

  gcc_assert (size > 0);

  /* Been there, done that.  */
  if (live_subregs_used[allocnum])
    return;

  /* Create a new one with zeros.  */
  if (live_subregs[allocnum] == NULL)
    live_subregs[allocnum] = sbitmap_alloc (size);

  /* If the entire reg was live before blasting into subregs, we need
     to init all of the subregs to ones else init to 0.  */
  if (init_value)
    sbitmap_ones (live_subregs[allocnum]);
  else 
    sbitmap_zero (live_subregs[allocnum]);

  /* Set the number of bits that we really want.  */
  live_subregs_used[allocnum] = size;
}


/* Set REG to be not live in the sets ALLOCNOS_LIVE, LIVE_SUBREGS,
   HARD_REGS_LIVE.  DEF is the definition of the register.  */

inline static void
clear_reg_in_live (sparseset allocnos_live,
		   sbitmap *live_subregs, 
		   int *live_subregs_used,
		   HARD_REG_SET *hard_regs_live, 
		   rtx reg, struct df_ref *def)
{
  unsigned int regno = (GET_CODE (reg) == SUBREG) 
    ? REGNO (SUBREG_REG (reg)): REGNO (reg);
  int allocnum = reg_allocno[regno];

  if (allocnum >= 0)
    {
      if (GET_CODE (reg) == SUBREG
	  && !DF_REF_FLAGS_IS_SET (def, DF_REF_EXTRACT))
	{
	  unsigned int start = SUBREG_BYTE (reg);
	  unsigned int last = start + GET_MODE_SIZE (GET_MODE (reg));

	  ra_init_live_subregs (sparseset_bit_p (allocnos_live, allocnum), 
				live_subregs, live_subregs_used, allocnum, reg);

	  if (!DF_REF_FLAGS_IS_SET (def, DF_REF_STRICT_LOWER_PART))
	    {
	      /* Expand the range to cover entire words.
		 Bytes added here are "don't care".  */
	      start = start / UNITS_PER_WORD * UNITS_PER_WORD;
	      last = ((last + UNITS_PER_WORD - 1)
		      / UNITS_PER_WORD * UNITS_PER_WORD);
	    }

	  /* Ignore the paradoxical bits.  */
	  if ((int)last > live_subregs_used[allocnum])
	    last = live_subregs_used[allocnum];

	  while (start < last)
	    {
	      RESET_BIT (live_subregs[allocnum], start);
	      start++;
	    }

	  if (sbitmap_empty_p (live_subregs[allocnum]))
	    {
	      live_subregs_used[allocnum] = 0;
	      sparseset_clear_bit (allocnos_live, allocnum);
	    }
	  else
	    /* Set the allocnos live here because that bit has to be
	       true to get us to look at the live_subregs fields.  */
	    sparseset_set_bit (allocnos_live, allocnum);
	}
      else
	{
	  /* Resetting the live_subregs_used is effectively saying do not use the 
	     subregs because we are writing the whole pseudo.  */
	  live_subregs_used[allocnum] = 0;
	  sparseset_clear_bit (allocnos_live, allocnum);
	}
    }

  if (regno >= FIRST_PSEUDO_REGISTER)
    return;

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (! fixed_regs[regno])
    {
      unsigned int start = regno;
      if (GET_CODE (reg) == SUBREG
	  && !DF_REF_FLAGS_IS_SET (def, DF_REF_EXTRACT))
	{
	  unsigned int last;
	  start += SUBREG_BYTE (reg);
	  last = start + subreg_nregs_with_regno (regno, reg);
	  regno = start;

	  while (regno < last)
	    {
	      CLEAR_HARD_REG_BIT (*hard_regs_live, regno);
	      regno++;
	    }
	}
      else
	remove_from_hard_reg_set (hard_regs_live, GET_MODE (reg), regno);
    }
}



/* Set REG to be live in the sets ALLOCNOS_LIVE, LIVE_SUBREGS,
   HARD_REGS_LIVE.  If EXTRACT is false, assume that the entire reg is
   set live even if REG is a subreg.  */

inline static void
set_reg_in_live (sparseset allocnos_live, 
		 sbitmap *live_subregs, 
		 int *live_subregs_used,
		 HARD_REG_SET *hard_regs_live, 
		 rtx reg,
		 bool extract)
{
  unsigned int regno = (GET_CODE (reg) == SUBREG) 
    ? REGNO (SUBREG_REG (reg)): REGNO (reg);
  int allocnum = reg_allocno[regno];

  if (allocnum >= 0)
    {
      if ((GET_CODE (reg) == SUBREG) && !extract)
	{
	  unsigned int start = SUBREG_BYTE (reg);
	  unsigned int last = start + GET_MODE_SIZE (GET_MODE (reg));

	  ra_init_live_subregs (sparseset_bit_p (allocnos_live, allocnum), 
				live_subregs, live_subregs_used, allocnum, reg);
	  
	  /* Ignore the paradoxical bits.  */
	  if ((int)last > live_subregs_used[allocnum])
	    last = live_subregs_used[allocnum];

	  while (start < last)
	    {
	      SET_BIT (live_subregs[allocnum], start);
	      start++;
	    }
	}
      else
	/* Resetting the live_subregs_used is effectively saying do not use the 
	   subregs because we are writing the whole pseudo.  */
	  live_subregs_used[allocnum] = 0;
     
      sparseset_set_bit (allocnos_live, allocnum);
    }
      
  if (regno >= FIRST_PSEUDO_REGISTER)
    return;

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (! fixed_regs[regno])
    {
      if ((GET_CODE (reg) == SUBREG) && !extract)
	{
	  unsigned int start = regno;
	  unsigned int last;

	  start += subreg_regno_offset (regno, GET_MODE (SUBREG_REG (reg)),
					SUBREG_BYTE (reg), GET_MODE (reg));
	  last = start + subreg_nregs_with_regno (regno, reg);
	  regno = start;

	  while (regno < last)
	    {
	      SET_HARD_REG_BIT (*hard_regs_live, regno);
	      regno++;
	    }
	}
      else
	add_to_hard_reg_set (hard_regs_live, GET_MODE (reg), regno);
    }
}


/* Add hard reg conflicts to RENUMBERS_LIVE assuming that pseudo in
   allocno[ALLOCNUM] is allocated to a set of hard regs starting at
   RENUMBER.

   We are smart about the case where only subregs of REG have been
   set, as indicated by LIVE_SUBREGS[ALLOCNUM] and
   LIVE_SUBREGS_USED[ALLOCNUM].  See global_conflicts for description
   of LIVE_SUBREGS and LIVE_SUBREGS_USED.  */

inline static void
set_renumbers_live (HARD_REG_SET *renumbers_live,   
		    sbitmap *live_subregs, 
		    int *live_subregs_used,
		    int allocnum, int renumber)
{
  /* The width of the pseudo.  */
  int nbytes = live_subregs_used[allocnum];
  int regno = allocno[allocnum].reg;
  enum machine_mode mode = GET_MODE (regno_reg_rtx[regno]);

  if (dump_file)
    fprintf (dump_file, "  set_renumbers_live %d->%d ", 
	     regno, renumber);

  if (nbytes > 0)
    {
      int i;
      sbitmap live_subs = live_subregs[allocnum];

      /* First figure out how many hard regs we are considering using.  */
      int target_nregs = hard_regno_nregs[renumber][mode];

      /* Now figure out the number of bytes per hard reg.  Note that
	 this may be different that what would be obtained by looking
	 at the mode in the pseudo.  For instance, a complex number
	 made up of 2 32-bit parts gets mapped to 2 hard regs, even if
	 the hardregs are 64-bit floating point values.  */
      int target_width = nbytes / target_nregs;
      
      if (dump_file)
	fprintf (dump_file, "target_nregs=%d target_width=%d nbytes=%d", 
		 target_nregs, target_width, nbytes);

      for (i = 0; i < target_nregs; i++)
	{
	  int j;
	  bool set = false;
	  for (j = 0; j < target_width; j++)
	    {
	      int reg_start = i * target_width;
	      if (reg_start + j >= nbytes)
		break;
	      set |= TEST_BIT (live_subs, reg_start + j);
	    }

	  if (set)
	    SET_HARD_REG_BIT (*renumbers_live, renumber + i);
	}
    }
  else
    add_to_hard_reg_set (renumbers_live, mode, renumber);

  if (dump_file)
    fprintf (dump_file, "\n");
}

/* Dump out a REF with its reg_renumber range to FILE using
   PREFIX.  */

static void
dump_ref (FILE *file, 
	  const char * prefix, 
	  const char * suffix, 
	  rtx reg,
	  unsigned int regno,
	  sbitmap *live_subregs, 
	  int *live_subregs_used
)
{
  int allocnum = reg_allocno[regno];

  fprintf (file, "%s %d", prefix, regno);
  if (allocnum >= 0 
      && live_subregs_used[allocnum] > 0)
    {
      int j;
      char s = '[';
      
      for (j = 0; j < live_subregs_used[allocnum]; j++)
	if (TEST_BIT (live_subregs[allocnum], j))
	  {
	    fprintf (dump_file, "%c%d", s, j);
	    s = ',';
	  }
      fprintf (dump_file, "]");
    }

  if (reg_renumber[regno] >= 0)
    {
      enum machine_mode mode = GET_MODE (reg);
      unsigned int start;
      unsigned int last;

      regno = reg_renumber[regno];

      start = regno;
      last = end_hard_regno (mode, regno);
      if (GET_CODE (reg) == SUBREG)
	{
	  start += subreg_regno_offset (regno, GET_MODE (SUBREG_REG (reg)),
					SUBREG_BYTE (reg), GET_MODE (reg));
	  last = start + subreg_nregs_with_regno (regno, reg);
	}

      if (start == last - 1)
	fprintf (file, "(%d)", start);
      else 
	fprintf (file, "(%d:%d..%d)", regno, start, last-1);
    }
  fprintf (file, suffix);
}


/* Scan the rtl code and record all conflicts and register preferences in the
   conflict matrices and preference tables.  */

void
global_conflicts (void)
{
  unsigned int i;
  basic_block bb;
  rtx insn;

  /* Regs that have allocnos can be in either 
     hard_regs_live (if regno < FIRST_PSEUDO_REGISTER) or 
     allocnos_live (if regno >= FIRST_PSEUDO_REGISTER) or 
     both if local_alloc has preallocated it and reg_renumber >= 0.  */

  HARD_REG_SET hard_regs_live;
  HARD_REG_SET renumbers_live;
  sparseset allocnos_live;
  bitmap live = BITMAP_ALLOC (NULL);
  VEC (df_ref_t, heap) *clobbers = NULL;
  VEC (df_ref_t, heap) *dying_regs = NULL;

  /* live_subregs is a vector used to keep accurate information about
     which hardregs are live in multiword pseudos.  live_subregs and
     live_subregs_used are indexed by reg_allocno.  The live_subreg
     entry for a particular pseudo is a bitmap with one bit per byte
     of the register.  It is only used if the corresponding element is
     non zero in live_subregs_used.  The value in live_subregs_used is
     number of bytes that the pseudo can occupy.  */
  sbitmap *live_subregs = XCNEWVEC (sbitmap, max_allocno);
  int *live_subregs_used = XNEWVEC (int, max_allocno);

  if (dump_file)
    {
      fprintf (dump_file, "fixed registers : "); 
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (fixed_regs[i])
	  fprintf (dump_file, "%d ", i);
      fprintf (dump_file, "\n");
    }

  allocnos_live = sparseset_alloc (max_allocno);

  FOR_EACH_BB (bb)
    {
      bitmap_iterator bi;

      bitmap_copy (live, DF_LIVE_OUT (bb));
      df_simulate_artificial_refs_at_end (bb, live);

      sparseset_clear (allocnos_live);
      memset (live_subregs_used, 0, max_allocno * sizeof (int));
      CLEAR_HARD_REG_SET (hard_regs_live);
      CLEAR_HARD_REG_SET (renumbers_live);

      /* Initialize allocnos_live and hard_regs_live for bottom of block.  */
      EXECUTE_IF_SET_IN_BITMAP (live, 0, i, bi)
	{
	  if (i >= FIRST_PSEUDO_REGISTER)
	    break;
	  if (! fixed_regs[i])
	    SET_HARD_REG_BIT (hard_regs_live, i);
	}
    
      EXECUTE_IF_SET_IN_BITMAP (live, FIRST_PSEUDO_REGISTER, i, bi)
	{
	  int allocnum = reg_allocno[i];

	  if (allocnum >= 0)
	    {
	      int renumber = reg_renumber[i];
	      rtx reg = regno_reg_rtx[i];

	      set_reg_in_live (allocnos_live, live_subregs, live_subregs_used, 
			       &hard_regs_live, reg, false);
	      if (renumber >= 0 && renumber < FIRST_PSEUDO_REGISTER)
		set_renumbers_live (&renumbers_live, live_subregs, live_subregs_used, 
				    allocnum, renumber);
	    }
	} 

      if (dump_file)
	fprintf (dump_file, "\nstarting basic block %d\n\n", bb->index);

      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  unsigned int uid = INSN_UID (insn);
	  struct df_ref **def_rec;
	  struct df_ref **use_rec;

	  if (!INSN_P (insn))
	    continue;	

	  if (dump_file)
	    {
	      fprintf (dump_file, "insn = %d live = hardregs [", uid);
	      
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (TEST_HARD_REG_BIT (hard_regs_live, i))
		  fprintf (dump_file, "%d ", i);

	      fprintf (dump_file, "] renumbered [");
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (TEST_HARD_REG_BIT (renumbers_live, i))
		  fprintf (dump_file, "%d ", i);

	      fprintf (dump_file, "] pseudos [");
	      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
		{
		  dump_ref (dump_file, " ", "", regno_reg_rtx[allocno[i].reg],
			    allocno[i].reg, live_subregs, live_subregs_used);
		}
	      fprintf (dump_file, "]\n");
	    }

	  /* Add the defs into live.  Most of them will already be
	     there, the ones that are missing are the unused ones and
	     the clobbers.  We do this in order to make sure that
	     interferences are added between every def and everything
	     that is live across the insn.  These defs will be removed
	     later.  */
	  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	    {
	      struct df_ref *def = *def_rec;

	      /* FIXME: Ignoring may clobbers is technically the wrong
		 thing to do.  However the old version of the this
		 code ignores may clobbers (and instead has many
		 places in the register allocator to handle these
		 constraints).  It is quite likely that with a new
		 allocator, the correct thing to do is to not ignore
		 the constraints and then do not put in the large
		 number of special checks.  */
	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER))
		{
		  rtx reg = DF_REF_REG (def);
		  set_reg_in_live (allocnos_live, live_subregs, live_subregs_used, 
				   &hard_regs_live, reg, 
				   DF_REF_FLAGS_IS_SET (def, DF_REF_EXTRACT));
		  if (dump_file)
		    dump_ref (dump_file, "  adding def", "\n",
			      reg, DF_REF_REGNO (def), live_subregs, live_subregs_used);
		}
	    }
	  
	  /* Add the hardregs into renumbers_live to build the
	     interferences.  Renumbers_live will be rebuilt in the
	     next step from scratch, so corrupting it here is no
	     problem.  */
	  IOR_HARD_REG_SET (renumbers_live, hard_regs_live);

	  /* Add the interferences for the defs.  */
	  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	    {
	      struct df_ref *def = *def_rec;
	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER))
		mark_reg_store (allocnos_live, &renumbers_live, def);
	    }
	  
	  /* Remove the defs from the live sets.  Leave the partial
	     and conditional defs in the set because they do not
	     kill.  */
	  VEC_truncate (df_ref_t, clobbers, 0);
	  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	    {
	      struct df_ref *def = *def_rec;

	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_CONDITIONAL))
		{
		  rtx reg = DF_REF_REG (def);

		  clear_reg_in_live (allocnos_live, live_subregs, live_subregs_used,
				     &hard_regs_live, reg, def);
		  if (dump_file)
		    dump_ref (dump_file, "  clearing def", "\n", 
			      reg, DF_REF_REGNO (def), live_subregs, live_subregs_used);
		}
	      
	      if (DF_REF_FLAGS_IS_SET (def, DF_REF_MUST_CLOBBER))
		VEC_safe_push (df_ref_t, heap, clobbers, def);
	    }
	  
	  /* Go thru all of the live pseudos and reset renumbers_live.
	     We must start from scratch here because there could have
	     been several pseudos alive that have the same
	     reg_renumber and if we see a clobber for one of them, we
	     cannot not want to kill the renumbers from the other
	     pseudos.  */
	  CLEAR_HARD_REG_SET (renumbers_live);
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	    {
	      unsigned int regno = allocno[i].reg;
	      int renumber = reg_renumber[regno];

	      if (renumber >= 0 && renumber < FIRST_PSEUDO_REGISTER)
		set_renumbers_live (&renumbers_live, live_subregs, live_subregs_used, 
				    i, renumber);
	    }
					 
	  /* Add the uses to the live sets.  Keep track of the regs
	     that are dying inside the insn, this set will be useful
	     later.  */
	  VEC_truncate (df_ref_t, dying_regs, 0);
	  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      unsigned int regno = DF_REF_REGNO (use);
	      bool added = false;
	      int renumber = reg_renumber[regno];
	      int allocnum = reg_allocno[regno];
	      bool renumbering = false;
	      rtx reg = DF_REF_REG (use);

	      /* DF_REF_READ_WRITE on a use means that this use is
		 fabricated from a def that is a partial set to a
		 multiword reg.  Here, we only model the subreg case
		 precisely so we do not need to look at the fabricated
		 use unless that set also happens to wrapped in a
		 ZERO_EXTRACT. */
	      if (DF_REF_FLAGS_IS_SET (use, DF_REF_READ_WRITE) 
		  && (!DF_REF_FLAGS_IS_SET (use, DF_REF_EXTRACT)) 
		  && DF_REF_FLAGS_IS_SET (use, DF_REF_SUBREG))
		continue;
	      
	      if (dump_file)
		dump_ref (dump_file, "  seeing use", "\n",
			  reg, regno, live_subregs, live_subregs_used);

	      if (allocnum >= 0)
		{
		  if (GET_CODE (reg) == SUBREG
		      && !DF_REF_FLAGS_IS_SET (use, DF_REF_EXTRACT)) 
		    {
		      unsigned int start = SUBREG_BYTE (reg);
		      unsigned int last = start + GET_MODE_SIZE (GET_MODE (reg));

		      ra_init_live_subregs (sparseset_bit_p (allocnos_live, allocnum), 
					    live_subregs, live_subregs_used, allocnum, reg);
		      
		      /* Ignore the paradoxical bits.  */
		      if ((int)last > live_subregs_used[allocnum])
			last = live_subregs_used[allocnum];
		      
		      while (start < last)
			{
			  if (!TEST_BIT (live_subregs[allocnum], start)) 
			    {
			      if (dump_file)
				fprintf (dump_file, "    dying pseudo subreg %d[%d]\n", regno, start);
			      SET_BIT (live_subregs[allocnum], start);
			      
			      added = true;
			    }
			  start++;
			}
		      
		      sparseset_set_bit (allocnos_live, allocnum);
		      if (renumber >= 0 && renumber < FIRST_PSEUDO_REGISTER)
			set_renumbers_live (&renumbers_live, live_subregs, live_subregs_used, 
					    allocnum, renumber);
		    }
		  else if (live_subregs_used[allocnum] > 0
			   || !sparseset_bit_p (allocnos_live, allocnum))
		    {
		      if (dump_file)
			fprintf (dump_file, "    %sdying pseudo\n", 
				 (live_subregs_used[allocnum] > 0) ? "partially ": "");
		      /* Resetting the live_subregs_used is
			 effectively saying do not use the subregs
			 because we are reading the whole pseudo.  */
		      live_subregs_used[allocnum] = 0;
		      sparseset_set_bit (allocnos_live, allocnum);
		      if (renumber >= 0 && renumber < FIRST_PSEUDO_REGISTER)
			set_renumbers_live (&renumbers_live, live_subregs, live_subregs_used, 
					    allocnum, renumber);
		      added = true;
		    }
		}

	      if (renumber >= 0 && renumber < FIRST_PSEUDO_REGISTER)
		{
		  regno = renumber;
		  renumbering = true;
		}
	      
	      if (regno < FIRST_PSEUDO_REGISTER)
		{
		  unsigned int start = regno;
		  unsigned int last;
		  if (GET_CODE (reg) == SUBREG)
		    {
		      start += subreg_regno_offset (regno, GET_MODE (SUBREG_REG (reg)),
						    SUBREG_BYTE (reg), GET_MODE (reg));
		      last = start + subreg_nregs_with_regno (regno, reg);
		    }
		  else
		    last = end_hard_regno (GET_MODE (reg), regno);
		  
		  regno = start;
		  while (regno < last)
		    {
		      if ((!TEST_HARD_REG_BIT (hard_regs_live, regno)) 
			  && (!TEST_HARD_REG_BIT (renumbers_live, regno)) 
			  && ! fixed_regs[regno])
			{
			  if (dump_file)
			    fprintf (dump_file, "    dying hard reg %d\n", regno);
			  if (renumbering)
			    SET_HARD_REG_BIT (renumbers_live, regno);
			  else
			    SET_HARD_REG_BIT (hard_regs_live, regno);
			  
			  added = true;
			}
		      regno++;
		    }
		}
	      if (added)
		VEC_safe_push (df_ref_t, heap, dying_regs, use);
	    }
	  
	  /* These three cases are all closely related, they all deal
             with some set of outputs of the insn need to conflict
             with some of the registers that are used by the insn but
             die within the insn. If no registers die within the insn,
             the tests can be skipped. */

	  if (VEC_length (df_ref_t, dying_regs) > 0)
	    {
	      int k;
	      /* There appears to be an ambiguity as to what a clobber
		 means in an insn.  In some cases, the clobber happens
		 within the processing of the insn and in some cases
		 it happens at the end of processing the insn.  There
		 is currently no way to distinguish these two cases so
		 this code causes real clobbers to interfere with
		 registers that die within an insn.

		 This is consistent with the prior version of
		 interference graph builder but is was discovered
		 while developing this version of the code, that on
		 some architectures such as the x86-64, the clobbers
		 only appear to happen at the end of the insn.
		 However, the ppc-32 contains clobbers for which these
		 interferences are necessary.

		 FIXME: We should consider either adding a new kind of
		 clobber, or adding a flag to the clobber distinguish
		 these two cases.  */
	      if (dump_file && VEC_length (df_ref_t, clobbers))
		fprintf (dump_file, "  clobber conflicts\n");
	      for (k = VEC_length (df_ref_t, clobbers) - 1; k >= 0; k--)
		{
		  struct df_ref *def = VEC_index (df_ref_t, clobbers, k);
		  int j;

		  for (j = VEC_length (df_ref_t, dying_regs) - 1; j >= 0; j--)
		    {
		      struct df_ref *use = VEC_index (df_ref_t, dying_regs, j);
		      record_one_conflict_between_regnos (GET_MODE (DF_REF_REG (def)),
							  DF_REF_REGNO (def),
							  GET_MODE (DF_REF_REG (use)),
							  DF_REF_REGNO (use));
		    }
		}

	      /* Early clobbers, by definition, need to not only
		 clobber the registers that are live across the insn
		 but need to clobber the registers that die within the
		 insn.  The clobbering for registers live across the
		 insn is handled above.  */ 
	      set_conflicts_for_earlyclobber (insn);

	      /* If INSN is a store with multiple outputs, then any
		 reg that dies here and is used inside of the address
		 of the output must conflict with the other outputs.

		 FIXME: There has been some discussion as to whether
		 this is right place to handle this issue.  This is a
		 hold over from an early version global conflicts.

		 1) There is some evidence that code only deals with a
		 bug that is only on the m68k.  The conditions of this
		 test are such that this case only triggers for a very
		 peculiar insn, one that is a parallel where one of
		 the sets is a store and the other sets a reg that is
		 used in the address of the store.  See
		 http://gcc.gnu.org/ml/gcc-patches/1998-12/msg00259.html

		 2) The situation that this is addressing is a bug in
		 the part of reload that handles stores, adding this
		 conflict only hides the problem.  (Of course no one
		 really wants to fix reload so it is understandable
		 why a bandaid was just added here.)

		 Just because an output is unused does not mean the
		 compiler can assume the side effect will not occur.
		 Consider if REG appears in the address of an output
		 and we reload the output.  If we allocate REG to the
		 same hard register as an unused output we could set
		 the hard register before the output reload insn.

		 3) This could actually be handled by making the other
		 (non store) operand of the insn be an early clobber.
		 This would insert the same conflict, even if it is
		 not technically an early clobber.  */

	      /* It is unsafe to use !single_set here since it will ignore an
		 unused output.  */
	      if (GET_CODE (PATTERN (insn)) == PARALLEL && multiple_sets (insn))
		{ 
		  int j;
		  if (dump_file)
		    fprintf (dump_file, "  multiple sets\n");
		  for (j = VEC_length (df_ref_t, dying_regs) - 1; j >= 0; j--)
		    {
		      int used_in_output = 0;
		      struct df_ref *use = VEC_index (df_ref_t, dying_regs, j);
		      rtx reg = DF_REF_REG (use);
		      int uregno = DF_REF_REGNO (use);
		      enum machine_mode umode = GET_MODE (DF_REF_REG (use));
		      int k;

		      for (k = XVECLEN (PATTERN (insn), 0) - 1; k >= 0; k--)
			{
			  rtx set = XVECEXP (PATTERN (insn), 0, k);
			  if (GET_CODE (set) == SET
			      && !REG_P (SET_DEST (set))
			      && !rtx_equal_p (reg, SET_DEST (set))
			      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
			    used_in_output = 1;
			}
		      if (used_in_output)
			for (k = XVECLEN (PATTERN (insn), 0) - 1; k >= 0; k--)
			  {
			    rtx set = XVECEXP (PATTERN (insn), 0, k);
			    if (GET_CODE (set) == SET
				&& REG_P (SET_DEST (set))
				&& !rtx_equal_p (reg, SET_DEST (set)))
			      record_one_conflict_between_regnos (GET_MODE (SET_DEST (set)),
								  REGNO (SET_DEST (set)), 
								  umode, uregno);
			  }
		    }
		}
	    }
	}

      /* Add the renumbers live to the hard_regs_live for the next few
	 calls.  All of this gets recomputed at the top of the loop so
	 there is no harm.  */
      IOR_HARD_REG_SET (hard_regs_live, renumbers_live);
	  
#ifdef EH_RETURN_DATA_REGNO
      if (bb_has_eh_pred (bb))
	{
	  unsigned int i;
    
	  for (i = 0; ; ++i)
	    {
	      unsigned int regno = EH_RETURN_DATA_REGNO (i);
	      if (regno == INVALID_REGNUM)
		break;
	      record_one_conflict (allocnos_live, &hard_regs_live, regno);
	    }

	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	    {
	      allocno[i].no_eh_reg = 1;
	    }
	}
#endif

      if (bb_has_abnormal_pred (bb))
	{
	  unsigned int i;
#ifdef STACK_REGS
	  /* Pseudos can't go in stack regs at the start of a basic block that
	     is reached by an abnormal edge. Likewise for call clobbered regs,
	     because caller-save, fixup_abnormal_edges and possibly the table
	     driven EH machinery are not quite ready to handle such regs live
	     across such edges.  */
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	    {
	      allocno[i].no_stack_reg = 1;
	    }

	  for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
	    record_one_conflict (allocnos_live, &hard_regs_live, i);
#endif
	  
	  /* No need to record conflicts for call clobbered regs if we have
	     nonlocal labels around, as we don't ever try to allocate such
	     regs in this case.  */
	  if (! current_function_has_nonlocal_label)
	    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	      if (call_used_regs [i])
		record_one_conflict (allocnos_live, &hard_regs_live, i);
	}
    }
  
  for (i = 0; i < (unsigned int)max_allocno; i++)
    if (live_subregs[i])
      free (live_subregs[i]);

  /* Clean up.  */
  free (allocnos_live);
  free (live_subregs);
  free (live_subregs_used);
  VEC_free (df_ref_t, heap, dying_regs);
  VEC_free (df_ref_t, heap, clobbers);
  BITMAP_FREE (live);
}
